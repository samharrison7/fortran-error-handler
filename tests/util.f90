module util
    implicit none

    ! Set kind parameters for use with real variables, as per the convention
    ! recommended in, e.g., http://fortranwiki.org/fortran/show/Real+precision
    ! and N. S. Clerman and W. Spector, Modern Fortran (2012).
    integer, parameter :: dp = selected_real_kind(15,307)       !! Double precision, 15 digits, 1e307
    integer, parameter :: qp = selected_real_kind(33,4931)      !! Quadruple precision, 33 digits, 1e4931

    interface str
        module procedure :: strFromInteger, strFromReal, strFromDp, strFromQp, strFromLogical, strFromComplex
    end interface

  contains

    !> Convert an integer to a string
    function strFromInteger(i) result(str)
        integer, intent(in) :: i        !! The integer to convert to a string
        character(len=256) :: str       !! The string to return
        write(str, *)i
        str = trim(adjustl(str))
    end function

    !> Convert a real to a string
    function strFromReal(r) result(str)
        real, intent(in) :: r           !! The integer to convert to a string
        character(len=256) :: str       !! The string to return
        write(str, *)r
        str = trim(adjustl(str))
    end function

    !> Convert a double-precision real to a string
    function strFromDp(r) result(str)
        real(dp), intent(in) :: r           !! The integer to convert to a string
        character(len=256) :: str           !! The string to return
        write(str, *)r
        str = trim(adjustl(str))
    end function

    !> Convert a quadruple-precision real to a string
    function strFromQp(r) result(str)
        real(qp), intent(in) :: r           !! The integer to convert to a string
        character(len=256) :: str           !! The string to return
        write(str, *)r
        str = trim(adjustl(str))
    end function

    !> Convert a logical to a string
    function strFromLogical(l) result(str)
        logical, intent(in) :: l            !! The logical to convert to a string
        character(len=5) :: str             !! The string to return
        if (l) then
            str = "true"
        else
            str = "false"
        end if
    end function
    
    !> Convert a complex number to a string
    function strFromComplex(r) result(str)
        complex, intent(in) :: r            !! The complex number to convert to a string
        character(len=256) :: str           !! The string to return
        write(str, *)r
        str = trim(adjustl(str))
    end function

end module