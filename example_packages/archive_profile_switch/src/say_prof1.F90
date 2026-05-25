#ifdef PROF1
submodule (say_mod) say_prof1
    implicit none
contains
    module subroutine say
        print *, 'prof1'
    end subroutine say
end submodule say_prof1
#endif
