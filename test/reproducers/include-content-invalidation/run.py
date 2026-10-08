"""Check executable values after direct/nested include edits, without source edits.

Usage: python run.py /absolute/path/to/fpm /fresh/output/root
"""
from pathlib import Path
import hashlib
import json
import os
import re
import subprocess
import sys

fpm = Path(sys.argv[1]).resolve()
root = Path(sys.argv[2]).resolve()
root.mkdir(parents=True, exist_ok=False)
results = []
failed = False

for case in ["direct", "nested", "include_directory", "cpp"]:
    project = root / case
    (project / "src").mkdir(parents=True)
    (project / "app").mkdir()
    (project / "generated").mkdir()
    manifest = "name = 'include_wake'\n"
    if case == "include_directory":
        manifest += "[library]\nsource-dir = 'src'\ninclude-dir = ['generated']\n"
    if case == "cpp":
        manifest += "[preprocess.cpp]\nsuffixes = ['f90']\n"
    (project / "fpm.toml").write_text(manifest)
    leaf = project / ("generated" if case == "include_directory" else "src") / "value.inc"
    leaf.write_text("value = 7\n")
    include = "include 'value.inc'"
    if case == "nested":
        (project / "src" / "body.inc").write_text(include + "\n")
        include = "include 'body.inc'"
    if case == "cpp":
        include = '#include "value.inc"'
    source = project / "src" / "value.f90"
    source.write_text("module included_value\nimplicit none\ncontains\n"
                      "integer function value()\n" + include + "\n"
                      "end function\nend module\n")
    (project / "app" / "main.f90").write_text(
        "program main\nuse included_value, only: value\n"
        "implicit none\nprint *, value()\nend program\n")
    original_source = source.read_bytes()

    for phase, value in [("cold", 7), ("warm", 7), ("edit", 11),
                         ("restore", 7), ("missing", None), ("recover", 11)]:
        if phase in ["edit", "restore", "recover"]:
            stat = leaf.stat() if leaf.exists() else None
            leaf.write_text("value = " + str(value) + "\n")
            if stat is not None:
                os.utime(leaf, ns=(stat.st_atime_ns, stat.st_mtime_ns))
        if phase == "missing":
            leaf.unlink()
        command = [str(fpm), "run", "--profile", "release"]
        result = subprocess.run(command, cwd=project, capture_output=True, text=True)
        text = result.stdout + result.stderr
        (project / (phase + ".log")).write_text(text)
        values = re.findall(r"^\s*(-?\d+)\s*$", result.stdout, re.MULTILINE)
        actual = int(values[-1]) if values else None
        ok = result.returncode != 0 if value is None else (
            result.returncode == 0 and actual == value)
        ok = ok and source.read_bytes() == original_source
        if phase == "warm":
            ok = ok and "Project is up to date" in text
        results.append({"case": case, "phase": phase, "expected": value,
                        "actual": actual, "exit": result.returncode, "pass": ok})
        failed = failed or not ok
        (root / "result.json").write_text(json.dumps({
            "driver": str(fpm), "driver_sha256": hashlib.sha256(fpm.read_bytes()).hexdigest(),
            "results": results}, indent=2) + "\n")

print(json.dumps({"passed": sum(r["pass"] for r in results), "total": len(results)}))
sys.exit(1 if failed else 0)
