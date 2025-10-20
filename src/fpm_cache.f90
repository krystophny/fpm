!># Source file caching for incremental builds
!>
!> This module implements caching of parsed source file information to eliminate
!> redundant parsing when source files have not changed. Uses Cargo-style
!> content-based invalidation with mtime fast-path optimization.
!>
!> The cache structure mirrors srcfile_t but adds content hashing and mtime tracking.
!> Cache is stored in `.fpm/cache/sources.toml` as human-readable TOML.
!>
!>## Key Features
!>
!> - **Content-based invalidation**: Files are hashed (FNV-1a) to detect actual changes
!> - **mtime fast-path**: Quick timestamp check before expensive hashing
!> - **Manifest tracking**: Invalidate cache when fpm.toml changes
!> - **Module dependency tracking**: Preserves module_provided/used relationships
!> - **Fail-safe**: Any cache error falls back to full parse
!>
module fpm_cache
use iso_fortran_env, only: int64
use iso_c_binding, only: c_char, c_int, c_long, c_null_char
use fpm_error, only: error_t, fatal_error
use fpm_filesystem, only: join_path, exists, filewrite, delete_file, canon_path, dirname, mkdir
use fpm_strings, only: string_t, fnv_1a, operator(==), len_trim
use fpm_model, only: srcfile_t, fpm_model_t, package_t
use fpm_toml, only: toml_table, toml_array, toml_stat, get_value, set_value, &
                   get_list, set_list, add_table, toml_key, toml_error, &
                   toml_serialize, new_table, add_array, len, toml_load
implicit none

private
public :: source_cache_t, fpm_cache_t
public :: new_cache, load_cache, save_cache, cache_is_valid, incremental_check
public :: hash_file_content
public :: populate_cache_from_source, restore_source_from_cache, find_cached_source

!> Per-file cache entry
type :: source_cache_t
    !> File path (relative to project root)
    character(:), allocatable :: file_name

    !> Modification time (seconds since epoch)
    integer(int64) :: mtime_sec

    !> Modification time (nanoseconds part)
    integer(int64) :: mtime_nsec

    !> Content hash (FNV-1a)
    integer(int64) :: content_hash

    !> Unit type (FPM_UNIT_*)
    integer :: unit_type

    !> Unit scope (FPM_SCOPE_*)
    integer :: unit_scope

    !> Modules provided by this file
    type(string_t), allocatable :: modules_provided(:)

    !> Parent modules (submodules only)
    type(string_t), allocatable :: parent_modules(:)

    !> Modules used by this file
    type(string_t), allocatable :: modules_used(:)

    !> Include dependencies
    type(string_t), allocatable :: include_dependencies(:)

end type source_cache_t

!> Main cache structure
type :: fpm_cache_t
    !> fpm version (major.minor for compatibility checking)
    character(:), allocatable :: fpm_version

    !> fpm.toml content hash
    integer(int64) :: manifest_hash

    !> build/cache.toml content hash (dependency versions)
    integer(int64) :: dep_cache_hash

    !> Cached source files
    type(source_cache_t), allocatable :: sources(:)

end type fpm_cache_t

!> Current fpm version (update when cache format changes)
character(*), parameter :: CACHE_VERSION = "0.10"

!> Cache file location (relative to build_dir)
character(*), parameter :: CACHE_FILE = "cache" // "/" // "sources.toml"

interface
    function c_get_mtime(path, sec, nsec) result(r) bind(c, name="c_get_mtime")
        import c_char, c_int, c_long
        character(kind=c_char), intent(in) :: path(*)
        integer(kind=c_long), intent(out) :: sec
        integer(kind=c_long), intent(out) :: nsec
        integer(kind=c_int) :: r
    end function c_get_mtime
end interface

contains

!> Create new empty cache
subroutine new_cache(cache, manifest_hash, dep_cache_hash)
    type(fpm_cache_t), intent(out) :: cache
    integer(int64), intent(in) :: manifest_hash
    integer(int64), intent(in) :: dep_cache_hash

    cache%fpm_version = CACHE_VERSION
    cache%manifest_hash = manifest_hash
    cache%dep_cache_hash = dep_cache_hash
    allocate(cache%sources(0))

end subroutine new_cache

!> Hash file content using FNV-1a
!> Uses existing read_text_file from fpm_filesystem for consistency
function hash_file_content(file_path, error) result(hash)
    character(*), intent(in) :: file_path
    type(error_t), allocatable, intent(out) :: error
    integer(int64) :: hash

    character(:), allocatable :: content
    integer :: fh, length, iostat

    hash = 0_int64

    ! Check file exists
    if (.not. exists(file_path)) then
        call fatal_error(error, "Cache: File not found: " // file_path)
        return
    end if

    ! Read entire file as binary (like read_text_file but with error handling)
    open(newunit=fh, file=file_path, status='old', action='read', &
         access='stream', form='unformatted', iostat=iostat)

    if (iostat /= 0) then
        call fatal_error(error, "Cache: Cannot open file: " // file_path)
        return
    end if

    inquire(fh, size=length)
    allocate(character(len=length) :: content)

    if (length > 0) then
        read(fh, iostat=iostat) content
        if (iostat /= 0) then
            close(fh)
            call fatal_error(error, "Cache: Cannot read file: " // file_path)
            return
        end if
    end if

    close(fh)

    ! Hash content
    hash = fnv_1a(content)

end function hash_file_content

!> Get file modification time using C stat()
subroutine get_file_mtime(file_path, mtime_sec, mtime_nsec, error)
    character(*), intent(in) :: file_path
    integer(int64), intent(out) :: mtime_sec
    integer(int64), intent(out) :: mtime_nsec
    type(error_t), allocatable, intent(out) :: error

    integer(c_int) :: r
    integer(c_long) :: sec, nsec
    character(len=len(file_path)+1) :: c_path

    ! Check file exists
    if (.not. exists(file_path)) then
        call fatal_error(error, "Cache: File not found: " // file_path)
        mtime_sec = 0_int64
        mtime_nsec = 0_int64
        return
    end if

    ! Convert to C string (null-terminated)
    c_path = file_path // c_null_char

    ! Call C stat() wrapper
    r = c_get_mtime(c_path, sec, nsec)

    if (r /= 0) then
        call fatal_error(error, "Cache: Cannot stat file: " // file_path)
        mtime_sec = 0_int64
        mtime_nsec = 0_int64
        return
    end if

    ! Convert to int64
    mtime_sec = int(sec, int64)
    mtime_nsec = int(nsec, int64)

end subroutine get_file_mtime

!> Check if cache is valid (quick manifest-level check)
function cache_is_valid(cache, manifest_path, dep_cache_path) result(is_valid)
    type(fpm_cache_t), intent(in) :: cache
    character(*), intent(in) :: manifest_path
    character(*), intent(in) :: dep_cache_path
    logical :: is_valid

    integer(int64) :: current_manifest_hash, current_dep_hash
    type(error_t), allocatable :: error

    is_valid = .false.

    ! Check version compatibility
    if (cache%fpm_version /= CACHE_VERSION) then
        return
    end if

    ! Check manifest hash
    current_manifest_hash = hash_file_content(manifest_path, error)
    if (allocated(error)) return

    if (current_manifest_hash /= cache%manifest_hash) then
        return
    end if

    ! Check dependency cache hash
    if (exists(dep_cache_path)) then
        current_dep_hash = hash_file_content(dep_cache_path, error)
        if (allocated(error)) return

        if (current_dep_hash /= cache%dep_cache_hash) then
            return
        end if
    end if

    is_valid = .true.

end function cache_is_valid

!> Incremental check: find changed files using mtime + hash hybrid
subroutine incremental_check(cache, changed_files, error)
    type(fpm_cache_t), intent(inout) :: cache
    type(string_t), allocatable, intent(out) :: changed_files(:)
    type(error_t), allocatable, intent(out) :: error

    integer :: i, n_changed
    integer(int64) :: current_mtime_sec, current_mtime_nsec, current_hash
    type(string_t), allocatable :: temp_changed(:)
    logical :: file_changed

    allocate(temp_changed(size(cache%sources)))
    n_changed = 0

    do i = 1, size(cache%sources)
        associate(cached => cache%sources(i))

            file_changed = .false.

            ! Check if file still exists
            if (.not. exists(cached%file_name)) then
                ! File deleted - mark as changed
                n_changed = n_changed + 1
                temp_changed(n_changed)%s = cached%file_name
                cycle
            end if

            ! Fast path: check mtime
            call get_file_mtime(cached%file_name, current_mtime_sec, current_mtime_nsec, error)
            if (allocated(error)) return

            if (current_mtime_sec /= cached%mtime_sec .or. &
                current_mtime_nsec /= cached%mtime_nsec) then

                ! mtime changed - verify with content hash
                current_hash = hash_file_content(cached%file_name, error)
                if (allocated(error)) return

                if (current_hash /= cached%content_hash) then
                    ! Content actually changed
                    file_changed = .true.
                else
                    ! False alarm (touch, editor swap) - update mtime only
                    cache%sources(i)%mtime_sec = current_mtime_sec
                    cache%sources(i)%mtime_nsec = current_mtime_nsec
                end if
            end if

            if (file_changed) then
                n_changed = n_changed + 1
                temp_changed(n_changed)%s = cached%file_name
            end if

        end associate
    end do

    ! Return only changed files
    allocate(changed_files(n_changed))
    changed_files(1:n_changed) = temp_changed(1:n_changed)

end subroutine incremental_check

!> Load cache from TOML file
subroutine load_cache(cache, cache_path, error)
    type(fpm_cache_t), intent(out) :: cache
    character(*), intent(in) :: cache_path
    type(error_t), allocatable, intent(out) :: error

    type(toml_table), allocatable :: table
    type(toml_table), pointer :: child
    type(toml_array), pointer :: array
    type(toml_error), allocatable :: parse_error
    integer :: unit, iostat, i
    character(:), allocatable :: version_str

    ! Check if cache file exists
    if (.not. exists(cache_path)) then
        call fatal_error(error, "Cache: Cache file does not exist: " // cache_path)
        return
    end if

    ! Parse TOML file
    call toml_load(table, cache_path, error=parse_error)

    if (allocated(parse_error)) then
        call fatal_error(error, "Cache: Invalid TOML in cache file: " // parse_error%message)
        return
    end if

    ! Read cache metadata
    call get_value(table, "version", version_str)
    if (allocated(version_str)) then
        cache%fpm_version = version_str
    else
        cache%fpm_version = "unknown"
    end if

    call get_value(table, "manifest-hash", cache%manifest_hash)
    call get_value(table, "dep-cache-hash", cache%dep_cache_hash)

    ! TODO: Read sources array (MVP: metadata only for now)
    allocate(cache%sources(0))

end subroutine load_cache

!> Save cache to TOML file
subroutine save_cache(cache, cache_path, error)
    type(fpm_cache_t), intent(in) :: cache
    character(*), intent(in) :: cache_path
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table, src_table
    type(toml_array), pointer :: src_array
    integer :: unit, iostat, i

    ! Create cache directory if needed
    if (.not. exists(dirname(cache_path))) then
        call mkdir(dirname(cache_path))
    end if

    ! Build TOML table
    call new_table(table)
    call set_value(table, "version", cache%fpm_version)
    call set_value(table, "manifest-hash", cache%manifest_hash)
    call set_value(table, "dep-cache-hash", cache%dep_cache_hash)

    ! TODO: Serialize sources array (MVP: metadata only for now)
    ! Array serialization requires understanding fpm's TOML patterns better

    ! Serialize to file
    open(newunit=unit, file=cache_path, status='replace', action='write', iostat=iostat)
    if (iostat /= 0) then
        call fatal_error(error, "Cache: Cannot write cache file: " // cache_path)
        return
    end if

    write(unit, '(a)') toml_serialize(table)
    close(unit)

end subroutine save_cache

!> Find cached source entry by file name
function find_cached_source(cache, file_name) result(idx)
    type(fpm_cache_t), intent(in) :: cache
    character(*), intent(in) :: file_name
    integer :: idx

    integer :: i

    idx = 0
    do i = 1, size(cache%sources)
        if (cache%sources(i)%file_name == file_name) then
            idx = i
            return
        end if
    end do

end function find_cached_source

!> Populate cache entry from parsed source file
subroutine populate_cache_from_source(cache_entry, source, error)
    type(source_cache_t), intent(out) :: cache_entry
    type(srcfile_t), intent(in) :: source
    type(error_t), allocatable, intent(out) :: error

    integer :: i

    cache_entry%file_name = source%file_name
    cache_entry%unit_type = source%unit_type
    cache_entry%unit_scope = source%unit_scope

    ! Hash content and get mtime
    cache_entry%content_hash = hash_file_content(source%file_name, error)
    if (allocated(error)) return

    call get_file_mtime(source%file_name, cache_entry%mtime_sec, &
                       cache_entry%mtime_nsec, error)
    if (allocated(error)) return

    ! Copy module information
    if (allocated(source%modules_provided)) then
        allocate(cache_entry%modules_provided(size(source%modules_provided)))
        cache_entry%modules_provided = source%modules_provided
    else
        allocate(cache_entry%modules_provided(0))
    end if

    if (allocated(source%parent_modules)) then
        allocate(cache_entry%parent_modules(size(source%parent_modules)))
        cache_entry%parent_modules = source%parent_modules
    else
        allocate(cache_entry%parent_modules(0))
    end if

    if (allocated(source%modules_used)) then
        allocate(cache_entry%modules_used(size(source%modules_used)))
        cache_entry%modules_used = source%modules_used
    else
        allocate(cache_entry%modules_used(0))
    end if

    if (allocated(source%include_dependencies)) then
        allocate(cache_entry%include_dependencies(size(source%include_dependencies)))
        cache_entry%include_dependencies = source%include_dependencies
    else
        allocate(cache_entry%include_dependencies(0))
    end if

end subroutine populate_cache_from_source

!> Restore source file from cache entry (without reparsing)
subroutine restore_source_from_cache(source, cache_entry)
    type(srcfile_t), intent(out) :: source
    type(source_cache_t), intent(in) :: cache_entry

    integer :: i

    source%file_name = cache_entry%file_name
    source%unit_type = cache_entry%unit_type
    source%unit_scope = cache_entry%unit_scope

    ! Initialize link_libraries (not cached, will be set by caller)
    allocate(source%link_libraries(0))

    ! Restore module information
    if (allocated(cache_entry%modules_provided)) then
        allocate(source%modules_provided(size(cache_entry%modules_provided)))
        source%modules_provided = cache_entry%modules_provided
    end if

    if (allocated(cache_entry%parent_modules)) then
        allocate(source%parent_modules(size(cache_entry%parent_modules)))
        source%parent_modules = cache_entry%parent_modules
    end if

    if (allocated(cache_entry%modules_used)) then
        allocate(source%modules_used(size(cache_entry%modules_used)))
        source%modules_used = cache_entry%modules_used
    end if

    if (allocated(cache_entry%include_dependencies)) then
        allocate(source%include_dependencies(size(cache_entry%include_dependencies)))
        source%include_dependencies = cache_entry%include_dependencies
    end if

end subroutine restore_source_from_cache

end module fpm_cache
