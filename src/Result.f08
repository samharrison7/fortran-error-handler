module ResultModule
    use ErrorHandlerModule
    use ErrorInstanceModule
    implicit none
    private

    integer, parameter :: sp = selected_real_kind(6,37)         !! Single precision, 6 digits, 1e37
    integer, parameter :: dp = selected_real_kind(15,307)       !! Double precision, 15 digits, 1e307
    integer, parameter :: qp = selected_real_kind(33,4931)      !! Quadruple precision, 33 digits, 1e4931

    !> Result type used for returning data and errors from functions.
    !! TODO:
    !!  - initWithPoly for initialising with class(*) or class(App) data.
    type, public :: Result
        class(*), allocatable                           :: data     !! The data returned from the function
        type(ErrorInstance), dimension(:), allocatable  :: errors   !! The errors (if any) returned from the function

        contains
            procedure :: getData
            procedure :: getError
            procedure :: getErrors
            procedure :: getErrorCode
    end type

    interface Result
        ! procedure initWithInteger
        ! procedure initWithReal
        ! procedure initWithCharacter
        procedure initWithPoly
    end interface

    contains
        ! !> Initialise the result object with integer data.
        ! !! Either a scalar ErrorInstance or array of ErrorInstances
        ! !! can be provided (the latter taking precedent if both
        ! ! provided). If none provided, default "no error" returned.
        ! function initWithInteger(data, error, errors) result(this)
        !     type(Result)                            :: this
        !     integer, intent(in)                     :: data
        !     type(ErrorInstance), intent(in), optional  :: error
        !     type(ErrorInstance), intent(in), optional  :: errors(:)

        !     ! Store the given data in this%data
        !     allocate(this%data, source=data)

        !     ! Allocate array of errors, if it isn't already
        !     if (.not. allocated(this%errors)) allocate(this%errors(0))
            
        !     ! If errors array given as param, use that, otherwise
        !     ! use error param, and if that's not there, use the
        !     ! default "no error" error.
        !     if (present(errors)) then
        !         this%errors = errors
        !     else if (present(error)) then
        !         this%errors = [error]
        !     else
        !         this%errors = [ErrorInstance(0, "No error.", .false.)]
        !     end if
        ! end function

        ! !> Initialise the result object with real(dp) data.
        ! function initWithReal(data, error, errors) result(this)
        !     type(Result)                            :: this
        !     real(dp), intent(in)                    :: data
        !     type(ErrorInstance), intent(in), optional  :: error
        !     type(ErrorInstance), intent(in), optional  :: errors(:)

        !     ! Store the given data in this%data
        !     allocate(this%data, source=data)

        !     ! Allocate array of errors, if it isn't already
        !     if (.not. allocated(this%errors)) allocate(this%errors(0))
            
        !     ! If errors array given as param, use that, otherwise
        !     ! use error param, and if that's not there, use the
        !     ! default "no error" error.
        !     if (present(errors)) then
        !         this%errors = errors
        !     else if (present(error)) then
        !         this%errors = [error]
        !     else
        !         this%errors = [ErrorInstance(0, "No error.", .false.)]
        !     end if
        ! end function

        ! !> Initialise the result object with character data.
        ! function initWithCharacter(data, error, errors) result(this)
        !     type(Result)                        :: this
        !     character(len=*), intent(in)        :: data
        !     type(ErrorInstance), intent(in), optional  :: error
        !     type(ErrorInstance), intent(in), optional  :: errors(:)

        !     ! Store the given data in this%data
        !     allocate(this%data, source=data)

        !     ! Allocate array of errors, if it isn't already
        !     if (.not. allocated(this%errors)) allocate(this%errors(0))
            
        !     ! If errors array given as param, use that, otherwise
        !     ! use error param, and if that's not there, use the
        !     ! default "no error" error.
        !     if (present(errors)) then
        !         this%errors = errors
        !     else if (present(error)) then
        !         this%errors = [error]
        !     else
        !         this%errors = [ErrorInstance(0, "No error.", .false.)]
        !     end if
        ! end function

        !> Initialise the result object with polymorphic class(*) data.
        function initWithPoly(data, error, errors) result(this)
            type(Result)                            :: this
            class(*), intent(in)                    :: data
            type(ErrorInstance), intent(in), optional  :: error
            type(ErrorInstance), intent(in), optional  :: errors(:)

            ! Store the given data in this%data
            allocate(this%data, source=data)

            ! Allocate array of errors, if it isn't already
            if (.not. allocated(this%errors)) allocate(this%errors(0))
            
            ! If errors array given as param, use that, otherwise
            ! use error param, and if that's not there, use the
            ! default "no error" error.
            if (present(errors)) then
                this%errors = errors
            else if (present(error)) then
                this%errors = [error]
            else
                this%errors = [ErrorInstance(0, "No error.", .false.)]
            end if
        end function

        !> Return the data from the Result object.
        pure function getData(this) result(data)
            class(Result), intent(in)   :: this
            class(*), allocatable       :: data
            allocate(data, source=this%data)
        end function

        !> Returns the error code from the first error in the errors array.
        pure function getErrorCode(this) result(errorCode)
            class(Result), intent(in)   :: this
            integer                     :: errorCode
            errorCode = this%errors(1)%getCode()
        end function

        !> Returns the first error from the errors array.
        pure function getError(this) result(error)
            class(Result), intent(in) :: this
            type(ErrorInstance) :: error
            if (size(this%errors)>0) error = this%errors(1)
        end function

        !> Return the errors array.
        pure function getErrors(this) result(errors)
            class(Result), intent(in) :: this
            type(ErrorInstance) :: errors(size(this%errors))
            if (size(this%errors)>0) errors = this%errors
        end function

end module