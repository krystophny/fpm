module test_runner
    use testsuite, only: new_unittest, unittest_t, error_t, test_failed
    use fpm_command_line, only: fpm_test_settings
    use fpm_environment, only: os_is_unix
    use fpm_filesystem, only: get_temp_filename, mkdir, os_delete_dir, run
    use fpm_runner, only: run_executables, runner_parallel_enabled
    use fpm_strings, only: string_t
    implicit none
    private

    public :: collect_runner

contains

    subroutine collect_runner(testsuite)
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            & new_unittest("run-executables-status", run_executables_status), &
            & new_unittest("run-executables-parallel", run_executables_parallel) &
            ]
    end subroutine collect_runner

    subroutine run_executables_status(error)
        type(error_t), allocatable, intent(out) :: error

        type(fpm_test_settings) :: settings
        type(string_t), allocatable :: executables(:)
        integer, allocatable :: stat(:)
        character(len=:), allocatable :: dir

        if (.not.os_is_unix()) return

        call init_settings(settings)
        call make_script_dir(dir)
        call make_script(dir, "pass.sh", "exit 0")
        call make_script(dir, "fail.sh", "exit 7")

        executables = [string_t(dir//"/pass.sh"), string_t(dir//"/fail.sh")]
        call run_executables(executables, settings, parallel=.true., stat=stat)

        if (size(stat) /= 2) then
            call test_failed(error, "wrong status count")
        elseif (stat(1) /= 0 .or. stat(2) /= 7) then
            call test_failed(error, "wrong run status values")
        end if

        call os_delete_dir(os_is_unix(), dir)
    end subroutine run_executables_status

    subroutine run_executables_parallel(error)
        type(error_t), allocatable, intent(out) :: error

        type(fpm_test_settings) :: settings
        type(string_t), allocatable :: executables(:)
        integer, allocatable :: stat(:)
        integer :: count0, count1, rate
        real :: elapsed
        character(len=:), allocatable :: dir

        if (.not.os_is_unix()) return

        call init_settings(settings)
        call make_script_dir(dir)
        call make_script(dir, "one.sh", "sleep 1")
        call make_script(dir, "two.sh", "sleep 1")
        call make_script(dir, "three.sh", "sleep 1")
        call make_script(dir, "four.sh", "sleep 1")

        executables = [ &
            string_t(dir//"/one.sh"), string_t(dir//"/two.sh"), &
            string_t(dir//"/three.sh"), string_t(dir//"/four.sh") &
            ]

        call system_clock(count0, rate)
        call run_executables(executables, settings, parallel=.true., stat=stat)
        call system_clock(count1)

        elapsed = real(count1 - count0)/real(rate)
        if (any(stat /= 0)) then
            call test_failed(error, "parallel scripts failed")
        elseif (runner_parallel_enabled() .and. elapsed > 3.2) then
            call test_failed(error, "parallel test run took too long")
        end if

        call os_delete_dir(os_is_unix(), dir)
    end subroutine run_executables_parallel

    subroutine init_settings(settings)
        type(fpm_test_settings), intent(out) :: settings

        settings%verbose = .false.
        settings%runner = ''
        settings%runner_args = ''
    end subroutine init_settings

    subroutine make_script_dir(dir)
        character(len=:), allocatable, intent(out) :: dir

        dir = get_temp_filename()
        call mkdir(dir, echo=.false.)
    end subroutine make_script_dir

    subroutine make_script(dir, name, body)
        character(len=*), intent(in) :: dir, name, body

        character(len=:), allocatable :: path
        integer :: unit

        path = dir//"/"//name
        open(newunit=unit, file=path, status="replace")
        write(unit, '(a)') "#!/bin/sh"
        write(unit, '(a)') body
        write(unit, '(a)') "exit $?"
        close(unit)
        call run("chmod +x "//path, echo=.false., verbose=.false.)
    end subroutine make_script

end module test_runner
