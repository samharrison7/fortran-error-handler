module TestClassModule
    implicit none
    private

    type, public :: TestClass
        real :: stuff
        contains
            procedure, public :: setStuff
            procedure, public :: getStuff
    end type

    contains

        subroutine setStuff(this, newStuff)
            class(TestClass) :: this
            real :: newStuff
            this%stuff = newStuff
        end subroutine

        function getStuff(this) result(stuff)
            class(TestClass) :: this
            real :: stuff
            stuff = this%stuff
        end function

end module