module TestClassModule
    use ErrorCriteriaModule
    use ErrorInstanceModule
    use ResultModule
    implicit none
    private

    type, public :: TestClass
        real :: stuff
        type(ErrorCriteria) :: EH
        contains
            procedure, public :: init
            procedure, public :: setStuff
            procedure, public :: getStuff
            procedure, public :: squareRoot
    end type

    contains

        subroutine init(this)
            class(TestClass) :: this
            call this%EH%init()
        end subroutine

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

        function squareRoot(this, input) result(r)
            class(TestClass) :: this
            real :: input
            type(ErrorInstance) :: error
            type(Result0D) :: r

            error = this%EH%positive(input)
            r = Result( &
                data = sqrt(input), &
                error = error &
            )
        end function

end module