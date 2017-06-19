module ResultModule
    use ErrorHandlerModule
    use ErrorInstanceModule
    implicit none
    private

    !> Result type used for returning data and errors from functions.
    type, abstract, public :: Result
        private
        class(*), allocatable                           :: data     !! The data returned from the function
        type(ErrorInstance), dimension(:), allocatable  :: errors   !! The errors (if any) returned from the function

        contains
            procedure, public :: getData
            procedure, public :: getDataAsReal
            procedure, public :: getDataAsInteger
            procedure, public :: getDataAsCharacter
            procedure, public :: getError
            procedure, public :: getErrors
            procedure, public :: getErrorCode

            generic, public :: operator(.real.) => getDataAsReal
            generic, public :: operator(.integer.) => getDataAsInteger
            generic, public :: operator(.character.) => getDataAsCharacter
    end type

    interface Result
        procedure initWithScalar
        procedure initWithArray
    end interface

    contains

        !> Initialise the result object with polymorphic class(*) data.
        function initWithScalar(data, error, errors) result(this)
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

        !> Initialise the result object with array of polymorphic class(*) data.
        function initWithArray(data, error, errors) result(this)
            type(Result)                            :: this
            class(*), intent(in)                    :: data(:)
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

        !> Attempt to return the data as a real
        pure function getDataAsReal(this) result(data)
            class(Result), intent(in) :: this
            real :: data
            select type (d => this%data)
                type is (real)
                    data = d
                type is (integer)
                    data = real(d)
                class default
                    data = transfer(source=d, mold=data)
            end select
        end function

        !> Attempt to return the data as a real
        pure function getDataAsInteger(this) result(data)
            class(Result), intent(in) :: this
            integer :: data
            select type (d => this%data)
                type is (integer)
                    data = d
                type is (real)
                    data = nint(d)
                class default
                    data = transfer(source=d, mold=data)
            end select
        end function

        !> Attempt to return the data as a real
        pure function getDataAsCharacter(this) result(data)
            class(Result), intent(in) :: this
            character(:), allocatable :: data
            select type (d => this%data)
                type is (character(len=*))
                    data = d
                class default
                    data = transfer(source=d, mold=data)
            end select
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