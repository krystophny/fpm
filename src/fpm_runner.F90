module fpm_runner
use fpm_command_line, only: fpm_run_settings
use fpm_environment, only: get_env, get_os_type, os_is_unix, OS_MACOS
use fpm_filesystem, only: delete_file, exists, get_temp_filename, getline, run, run_argv
use fpm_strings, only: add_strings, string_cat, string_t
use shlex_module, only: ms_split, sh_split => split
implicit none
private

public :: run_executables, runner_parallel_enabled

type :: run_command_t
    type(string_t), allocatable :: argv(:)
    type(string_t) :: command
    type(string_t) :: log_file
    logical :: argv_ok = .true.
end type run_command_t

contains

subroutine run_executables(executables, settings, parallel, stat)
    type(string_t), intent(in) :: executables(:)
    class(fpm_run_settings), intent(in) :: settings
    logical, intent(in) :: parallel
    integer, allocatable, intent(out) :: stat(:)

    type(run_command_t), allocatable :: commands(:)
    integer :: i

    allocate(stat(size(executables)), source=0)
    allocate(commands(size(executables)))

    do i = 1, size(executables)
        call make_run_command(executables(i)%s, settings, commands(i))
    end do

    if (any(.not.commands%argv_ok)) then
        call run_shell_fallback(commands, settings%verbose, stat)
        return
    end if

    if (settings%verbose) then
        do i = 1, size(commands)
            write(*, '(a,a)') '+ ', commands(i)%command%s
        end do
    end if

    if (parallel) then
        !$omp parallel do default(shared) schedule(dynamic,1)
        do i = 1, size(commands)
            call run_argv(commands(i)%argv, echo=.false., verbose=.false., &
                          redirect=commands(i)%log_file%s, exitstat=stat(i))
        end do
        !$omp end parallel do
    else
        do i = 1, size(commands)
            call run_argv(commands(i)%argv, echo=.false., verbose=.false., &
                          redirect=commands(i)%log_file%s, exitstat=stat(i))
        end do
    end if

    call replay_logs(commands, stat, settings%verbose)
end subroutine run_executables

logical function runner_parallel_enabled()
#ifdef _OPENMP
    runner_parallel_enabled = .true.
#else
    runner_parallel_enabled = .false.
#endif
end function runner_parallel_enabled

subroutine make_run_command(executable, settings, command)
    character(len=*), intent(in) :: executable
    class(fpm_run_settings), intent(in) :: settings
    type(run_command_t), intent(out) :: command

    character(len=:), allocatable :: runner_cmd, shell_cmd
    logical :: ok

    command%log_file%s = get_temp_filename()
    ok = .true.

    if (get_os_type() == OS_MACOS) then
        call add_strings(command%argv, string_t('env'))
        call add_strings(command%argv, &
                         string_t('DYLD_LIBRARY_PATH='//get_env('DYLD_LIBRARY_PATH', '')))
    end if

    runner_cmd = runner_command_text(settings)
    if (len_trim(runner_cmd) > 0) call append_words(command%argv, runner_cmd, ok)
    if (ok) call add_strings(command%argv, string_t(executable))
    if (ok .and. allocated(settings%args)) then
        if (len_trim(settings%args) > 0) call append_words(command%argv, settings%args, ok)
    end if

    shell_cmd = executable
    if (len_trim(runner_cmd) > 0) shell_cmd = trim(runner_cmd)//' '//shell_cmd
    if (allocated(settings%args)) then
        if (len_trim(settings%args) > 0) shell_cmd = shell_cmd//' '//settings%args
    end if
    if (get_os_type() == OS_MACOS) then
        shell_cmd = 'env DYLD_LIBRARY_PATH='//get_env('DYLD_LIBRARY_PATH', '')//' '//shell_cmd
    end if

    command%command%s = shell_cmd
    command%argv_ok = ok
end subroutine make_run_command

function runner_command_text(settings) result(text)
    class(fpm_run_settings), intent(in) :: settings
    character(len=:), allocatable :: text

    text = ''
    if (allocated(settings%runner)) then
        if (len_trim(settings%runner) > 0 .and. trim(settings%runner) /= ' ') then
            text = trim(settings%runner)
        end if
    end if
    if (allocated(settings%runner_args)) then
        if (len_trim(settings%runner_args) > 0) then
            if (len_trim(text) > 0) then
                text = text//' '//trim(settings%runner_args)
            else
                text = trim(settings%runner_args)
            end if
        end if
    end if
end function runner_command_text

subroutine append_words(argv, text, ok)
    type(string_t), allocatable, intent(inout) :: argv(:)
    character(len=*), intent(in) :: text
    logical, intent(inout) :: ok

    character(len=:), allocatable :: parts(:)
    integer :: i

    if (.not.ok) return

    !$omp critical(fpm_shlex_split)
    if (os_is_unix()) then
        parts = sh_split(text, join_spaced=.false., keep_quotes=.false., success=ok)
    else
        parts = ms_split(text, success=ok)
    end if
    if (ok) then
        do i = 1, size(parts)
            if (len_trim(parts(i)) == 0) cycle
            if (os_is_unix()) then
                call add_strings(argv, string_t(clean_shlex_token(parts(i))))
            else
                call add_strings(argv, string_t(trim(parts(i))))
            end if
        end do
    end if
    !$omp end critical(fpm_shlex_split)
end subroutine append_words

function clean_shlex_token(token) result(clean)
    character(len=*), intent(in) :: token
    character(len=:), allocatable :: clean

    integer :: i

    clean = ''
    do i = 1, len_trim(token)
        select case (token(i:i))
        case ("'", '"')
            cycle
        case default
            clean = clean//token(i:i)
        end select
    end do
    clean = trim(adjustl(clean))
end function clean_shlex_token

subroutine run_shell_fallback(commands, verbose, stat)
    type(run_command_t), intent(in) :: commands(:)
    logical, intent(in) :: verbose
    integer, intent(inout) :: stat(:)

    integer :: i

    do i = 1, size(commands)
        call run(commands(i)%command%s, echo=verbose, exitstat=stat(i))
    end do
end subroutine run_shell_fallback

subroutine replay_logs(commands, stat, verbose)
    type(run_command_t), intent(in) :: commands(:)
    integer, intent(in) :: stat(:)
    logical, intent(in) :: verbose

    integer :: i

    do i = 1, size(commands)
        call replay_log(commands(i)%log_file%s)
        if (exists(commands(i)%log_file%s)) call delete_file(commands(i)%log_file%s)
    end do
end subroutine replay_logs

subroutine replay_log(path)
    character(len=*), intent(in) :: path

    character(len=:), allocatable :: line
    integer :: fh, iostat

    open(newunit=fh, file=path, status='old', iostat=iostat)
    if (iostat /= 0) return
    do
        call getline(fh, line, iostat)
        if (iostat /= 0) exit
        write(*, '(a)') trim(line)
    end do
    close(fh)
end subroutine replay_log

end module fpm_runner
