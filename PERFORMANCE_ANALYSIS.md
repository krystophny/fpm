# FPM Source Cache Performance Analysis

## Problem
FPM was parsing all 125 source files on every `fpm run`, even with zero rebuilds.
This added ~160ms overhead compared to ~1ms for direct executable execution.

## Solution
Implemented source file caching with mtime-based invalidation using binary unformatted I/O.

## Performance Results

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| **Zero-rebuild time** | 162ms | 53ms | **3.0x faster** |
| **Overhead vs direct exec** | 161ms | 52ms | **67% reduction** |
| **Instructions executed** | 147.5M | 115M | **22% fewer** |

## Implementation

### Architecture
- **Cache location**: `build/cache/sources/*.cache`
- **Format**: Binary unformatted Fortran I/O (stream access)
- **Invalidation**: mtime (nanosecond precision)
- **Files cached**: 125 source files (512KB total)

### Why Binary Instead of TOML?
Initial implementation used TOML serialization via `dump_to_toml`/`load_from_toml`.
Callgrind profiling revealed:
- TOML deserialization: **35.9M instructions (24.3% of runtime!)**
- Loading 125 TOML files = 125x lexer init + 125x parser runs
- **TOML overhead > Fortran parsing overhead!**

Binary unformatted I/O eliminated this bottleneck entirely:
- Direct memory→disk mapping
- No lexer, no parser, no string conversion
- **10-20x faster than TOML**

### Callgrind Impact
```
Component                Before (TOML)    After (Binary)   Improvement
─────────────────────────────────────────────────────────────────────
Total instructions       147.5M           115M             -22%
load_srcfile_from_cache  35.9M (24%)      <1M (negligible) -97%
add_sources_from_dir     47.6M (32%)      15.2M (13%)      -68%
```

## Remaining Performance Budget

The 53ms overhead includes necessary work that cannot be easily cached:
- TOML manifest parsing: 20M instr (18%) - *some duplicate reads*
- Dependency resolution: 30M instr (26%)
- Feature/profile processing: 32M instr (28%)
- Build graph generation: 26M instr (23%)

### Future Optimizations
1. **Manifest cache** (low-hanging fruit): Each fpm.toml read 4x - save ~3-5ms
2. **Dependency graph cache**: Complex due to config dependencies - save ~5-7ms
3. **Build target cache**: Depends on sources + config - save ~5ms

## Comparison to Other Build Tools

| Tool | Zero-rebuild overhead |
|------|----------------------|
| fpm (before) | ~160ms |
| **fpm (after)** | **~50ms** |
| Cargo (Rust) | 50-100ms |
| Make | 20-50ms |
| Ninja | 5-10ms (minimal functionality) |

**FPM is now competitive with Cargo!**

## Technical Details

### Cache File Format (Binary)
Each `.cache` file contains:
- file_name (length-prefixed string)
- exe_name (optional, length-prefixed)
- unit_scope, unit_type (integers)
- digest (int64 - from fnv_1a hash)
- mtime_sec, mtime_nsec (int64 - for validation)
- modules_provided (array of strings)
- parent_modules (array of strings)
- modules_used (array of strings)  
- include_dependencies (array of strings)
- link_libraries (array of strings)

### Cache Validation
```fortran
! Check if cached file is still valid
call get_file_mtime(source_file, current_mtime_sec, current_mtime_nsec, error)
if (current_mtime_sec == cached%mtime_sec .and. &
    current_mtime_nsec == cached%mtime_nsec) then
    ! Cache hit - use cached data
    from_cache = .true.
end if
```

### Key Design Decisions
1. **Use existing digest**: Never recompute hashes, use parser's `fnv_1a(file_lines)`
2. **Per-file caching**: Simple, atomic, independent invalidation
3. **Binary format**: 10-20x faster than TOML
4. **Fail-safe**: Cache errors fallback to parsing
5. **mtime validation**: Fast check before expensive parsing

## Lessons Learned
1. **Profile before optimizing**: TOML overhead was unexpected
2. **Use native formats**: Binary I/O >> text serialization for performance
3. **Cache the right thing**: Source digest from parser, not recomputed
4. **Measure everything**: Callgrind revealed the real bottleneck
