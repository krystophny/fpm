#!/bin/sh
# Run with an explicit fpm image: ./run.sh /absolute/path/to/fpm
set -eu
fpm_driver=${1:?supply the fpm executable to test}
case "$fpm_driver" in /*) ;; *) echo 'supply an absolute executable path' >&2; exit 2;; esac
fixture=$(mktemp -d "${TMPDIR:-/tmp}/fpm-absolute-dependency.XXXXXX")
trap 'rm -rf "$fixture"' EXIT HUP INT TERM
mkdir -p "$fixture/dependency/src" "$fixture/consumer/app"
cat > "$fixture/dependency/fpm.toml" <<'MANIFEST'
name = "constant-dependency"
MANIFEST
cat > "$fixture/dependency/src/value.f90" <<'SOURCE'
module value_module
  implicit none
  integer, parameter :: value = 7
end module value_module
SOURCE
cat > "$fixture/consumer/fpm.toml" <<MANIFEST
name = "absolute-dependency-consumer"
[dependencies]
constant-dependency = { path = "$fixture/dependency" }
MANIFEST
cat > "$fixture/consumer/app/main.f90" <<'SOURCE'
program main
  use value_module, only: value
  implicit none
  if (value /= 7) error stop 'incorrect dependency payload'
  print '(a)', 'absolute dependency payload verified'
end program main
SOURCE
cd "$fixture/consumer"
"$fpm_driver" run --profile release
