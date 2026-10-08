"""Check downstream freshness after a separate library build.

Usage: python3 run.py /absolute/path/to/fpm /new/output/root
"""
from pathlib import Path
import hashlib
import json
import os
import subprocess
import sys

fpm = Path(sys.argv[1]).resolve()
root = Path(sys.argv[2]).resolve()
root.mkdir(parents=True, exist_ok=False)
results = []
environment = dict(os.environ, OMP_NUM_THREADS="1")

for case in ["body", "module_interface"]:
    project = root / case
    (project / "src").mkdir(parents=True)
    (project / "test").mkdir()
    (project / "fpm.toml").write_text("name = 'dependency_freshness'\n")
    source = project / "src" / "provider.f90"
    if case == "body":
        def library(value, width):
            return ("module provider\nimplicit none\ncontains\n"
                    "integer function value()\nvalue=" + str(value) +
                    "\nend function\nend module\n")
        check = ("program check_value\nuse provider, only: value\n"
                 "implicit none\ncharacter(32) :: argument\ninteger :: expected\n"
                 "call get_command_argument(1, argument)\nread(argument,*) expected\n"
                 "print *, 'value:', value(), 'expected:', expected\n"
                 "if (value() /= expected) error stop 'stale linked library value'\n"
                 "end program\n")
    else:
        def library(value, width):
            return ("module provider\nimplicit none\ntype :: packet\n"
                    "integer :: reserved(" + str(width) + ")\ninteger :: value\n"
                    "end type\ncontains\nfunction make_packet() result(data)\n"
                    "type(packet) :: data\ndata%reserved=17\ndata%value=" + str(value) +
                    "\nend function\nend module\n")
        check = ("program check_value\nuse provider, only: packet, make_packet\n"
                 "implicit none\ntype(packet) :: data\ncharacter(32) :: argument\n"
                 "integer :: expected, width\ncall get_command_argument(1, argument)\n"
                 "read(argument,*) expected\ncall get_command_argument(2, argument)\n"
                 "read(argument,*) width\n"
                 "if (size(data%reserved) /= width) error stop 'stale module interface'\n"
                 "data=make_packet()\nprint *, 'value:', data%value, 'expected:', expected\n"
                 "if (data%value /= expected) error stop 'stale module payload'\n"
                 "end program\n")
    source.write_text(library(7, 3))
    test_source = project / "test" / "check_value.f90"
    test_source.write_text(check)
    test_bytes = test_source.read_bytes()
    phases = [("cold", "test", 7, 3), ("warm", "test", 7, 3),
              ("edit_build", "build", 11, 1), ("test_after_build", "test", 11, 1),
              ("warm_changed", "test", 11, 1), ("restore_build", "build", 7, 3),
              ("test_after_restore", "test", 7, 3), ("warm_restored", "test", 7, 3)]
    for phase, operation, value, width in phases:
        if operation == "build":
            before = source.stat()
            source.write_text(library(value, width))
            os.utime(source, ns=(before.st_atime_ns, before.st_mtime_ns))
        command = [str(fpm), operation, "--profile", "debug"]
        if operation == "test":
            command += ["--", str(value), str(width)]
        outcome = subprocess.run(command, cwd=project, env=environment,
                                 capture_output=True, text=True)
        output = outcome.stdout + outcome.stderr
        (project / (phase + ".log")).write_text(output)
        ok = outcome.returncode == 0 and test_source.read_bytes() == test_bytes
        if phase.startswith("warm"):
            ok = ok and "Project is up to date" in output
        results.append({"case": case, "phase": phase, "command": command,
                        "expected_value": value, "expected_width": width,
                        "exit": outcome.returncode, "pass": ok})
        (root / "result.json").write_text(json.dumps({
            "driver": str(fpm), "driver_sha256": hashlib.sha256(fpm.read_bytes()).hexdigest(),
            "results": results}, indent=2) + "\n")

print(json.dumps({"passed": sum(r["pass"] for r in results), "total": len(results)}))
sys.exit(0 if all(r["pass"] for r in results) else 1)
