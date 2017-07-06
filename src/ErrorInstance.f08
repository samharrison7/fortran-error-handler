module ErrorInstanceModule
    implicit none
    private

    !> Data type for error codes
    !! TODO:
    !!  - Could create an ErrorInstance class, as currently ErrorInstance-related procedures
    !!    are defined in ErrorHandler class (i.e. this file), which doesn't make complete sense.
    !!  - Ultimately it would be nice if ErrorInstance could be passed `this` to automatically
    !!    figure out what the text for the stack point should be.
    type, public :: ErrorInstance
        integer                                         :: code                     !> Numeric error code
        character(len=256)                              :: message = ""             !> Message to accompany the error
        logical                                         :: isCritical = .true.      !> Shoud program execution be stopped?
        character(len=256), dimension(:), allocatable   :: trace                    !> Custom backtrace for the error

        contains
            procedure, public :: addToTrace
            procedure, public :: getCode
            procedure, public :: isError
            procedure, public :: notError
    end type

    interface ErrorInstance
        procedure init
    end interface

    contains
        !> Create a new ErrorInstance.
        function init(code, message, isCritical, trace) result(this)
            type(ErrorInstance)                     :: this             !> The ErrorInstance class
            integer, intent(in)                     :: code             !> Code for the error
            character(len=*), intent(in), optional  :: message          !> Custom error message
            logical, intent(in), optional           :: isCritical       !> Is the error message critical?
            character(len=*), intent(in), optional  :: trace(:)         !> User-defined trace for the error

            this%code = code
            if (present(message)) this%message = message
            if (present(isCritical)) this%isCritical = isCritical
            if (present(trace)) then
                allocate(this%trace, source=trace)
            else
                allocate(this%trace(0))
            end if
        end function

        !> Add a node to the trace.
        !! WARNING: GFortran bug means this must be compiled with
        !! flag -O1 at least. See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=70231
        !! and https://stackoverflow.com/questions/44385909/adding-to-an-array-of-characters-in-fortran
        pure subroutine addToTrace(this, point)
            class(ErrorInstance), intent(inout) :: this
            character(len=*), intent(in) :: point
            character(len=256) :: tempPoint

            tempPoint = point       ! Make character length 256
            ! Check if this is the first time something has been added to
            ! the trace or not. If it is, allocate as null array.
            if (.not. allocated(this%trace)) allocate(this%trace(0))
            ! Add the new node to the trace array.
            this%trace = [this%trace, tempPoint]
        end subroutine

        !> Return the error code.
        pure function getCode(this) result(code)
            class(ErrorInstance), intent(in)    :: this
            integer                             :: code
            code = this%code
        end function

        pure function isError(this)
            class(ErrorInstance), intent(in)    :: this
            logical                             :: isError
            isError = .true.
            if (this%code == 0) isError = .false.
        end function

        pure function notError(this)
            class(ErrorInstance), intent(in)    :: this
            logical                             :: notError
            notError = .true.
            if (this%code /= 0) notError = .false.
        end function

end module