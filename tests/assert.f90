!> Testing utilities for assertion
module assert
    use util
    implicit none

    interface assertEqual
        module procedure :: assertEqualInteger
        module procedure :: assertEqualReal
        module procedure :: assertEqualDp
        module procedure :: assertEqualQp
        module procedure :: assertEqualLogical
        module procedure :: assertEqualString
        module procedure :: assertEqualComplex
    end interface

  contains

    subroutine exitOut(msg)
        character(len=*) :: msg
        write(*, '(a)') trim(msg)
        error stop 1
    end subroutine

    subroutine assertEqualInteger(a, b, msg)
        integer, intent(in) :: a, b
        character(len=*), optional :: msg
        if (a /= b) then
            if (present(msg)) write(*, '(a)') msg
            call exitOut("Assertion error: Values " // trim(str(a)) // " and " // trim(str(b)) // " are not equal.")
        end if
    end subroutine

    subroutine assertEqualReal(a, b, msg)
        real, intent(in) :: a, b
        character(len=*), optional :: msg
        if (a /= b) then
            if (present(msg)) write(*, '(a)') msg
            call exitOut("Assertion error: Values " // trim(str(a)) // " and " // trim(str(b)) // " are not equal.")
        end if
    end subroutine

    subroutine assertEqualDp(a, b, msg)
        real(dp), intent(in) :: a, b
        character(len=*), optional :: msg
        if (a /= b) then
            if (present(msg)) write(*, '(a)') msg
            call exitOut("Assertion error: Values " // trim(str(a)) // " and " // trim(str(b)) // " are not equal.")
        end if
    end subroutine

    subroutine assertEqualQp(a, b, msg)
        real(qp), intent(in) :: a, b
        character(len=*), optional :: msg
        if (a /= b) then
            if (present(msg)) write(*, '(a)') msg
            call exitOut("Assertion error: Values " // trim(str(a)) // " and " // trim(str(b)) // " are not equal.")
        end if
    end subroutine

    subroutine assertEqualLogical(a, b, msg)
        logical, intent(in) :: a, b
        character(len=*), optional :: msg
        if (a .neqv. b) then
            if (present(msg)) write(*, '(a)') msg
            call exitOut("Assertion error: Values " // trim(str(a)) // " and " // trim(str(b)) // " are not equal.")
        end if
    end subroutine

    subroutine assertEqualString(a, b, msg)
        character(len=*), intent(in) :: a, b
        character(len=*), optional :: msg
        if (trim(a) /= trim(b)) then
            if (present(msg)) write(*, '(a)') msg
            call exitOut("Assertion error: Values '" // trim(a) // "' and '" // trim(b) // "' are not equal.")
        end if
    end subroutine

    subroutine assertEqualComplex(a, b, msg)
        complex, intent(in) :: a, b
        character(len=*), optional :: msg
        if (a /= b) then
            if (present(msg)) write(*, '(a)') msg
            call exitOut("Assertion error: Values " // trim(str(a)) // " and " // trim(str(b)) // " are not equal.")
        end if
    end subroutine

end module