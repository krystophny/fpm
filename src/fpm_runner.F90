module fpm_runner
    use fpm_command_line, only: fpm_run_settings
    use fpm_environment, only: get_env, get_os_type, OS_MACOS
    use fpm_filesystem, only: delete_file, exists, get_temp_filename, getline, run, run_argv
    use fpm_compiler, only: split_append
    use fpm_strings, only: add_strings, string_t
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

        runner_cmd = trim(adjustl(settings%runner_command()))
        if (len_trim(runner_cmd) > 0) call split_append(command%argv, runner_cmd, .false., ok)
        if (ok) call add_strings(command%argv, string_t(executable))
        if (ok .and. allocated(settings%args)) then
            if (len_trim(settings%args) > 0) call split_append(command%argv, settings%args, .false., ok)
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
