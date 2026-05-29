#ifdef PROF2
submodule (say_mod) say_prof2
    implicit none
contains
    module subroutine say
        print *, 'prof2'
    end subroutine say
end submodule say_prof2
#endif
