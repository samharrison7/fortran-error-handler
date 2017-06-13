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
        integer :: code                     !> Numeric error code
        character(len=256) :: message       !> Message to accompany the error
        logical :: isCritical = .true.      !> Shoud program execution be stopped?
        character(len=256), dimension(:), allocatable :: trace   !> Custom backtrace for the error

        contains
            procedure, public :: addPointToTrace

            ! Getters
            procedure, public :: getCode
    end type

    interface ErrorInstance
        procedure init
    end interface

    contains

        function init(code, message, isCritical, trace) result(this)
            type(ErrorInstance) :: this
            integer, intent(in) :: code
            character(len=*), intent(in), optional :: message
            logical, intent(in), optional :: isCritical
            character(len=*), intent(in), optional :: trace(:)

            this%code = code
            if (present(message)) this%message = message
            if (present(isCritical)) this%isCritical = isCritical
            if (present(trace)) this%trace = trace
        end function

        !> Add a point to the stack trace.
        !! WARNING: GFortran bug means this must be compiled with
        !! flag -O1 at least. See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=70231
        !! and https://stackoverflow.com/questions/44385909/adding-to-an-array-of-characters-in-fortran
        pure subroutine addPointToTrace(this, point)
            class(ErrorInstance), intent(inout) :: this
            character(len=*), intent(in) :: point
            character(len=256) :: tempPoint

            tempPoint = point       ! Make character length 256
            ! Check if this is the first time something has been added to
            ! the stack trace or not. If it is, allocate as null array.
            if (.not. allocated(this%trace)) allocate(this%trace(0))
            ! Add the new trace point to the trace array.
            this%trace = [this%trace, tempPoint]
        end subroutine

        !> Return the error code
        pure function getCode(this) result(code)
            class(ErrorInstance), intent(in)   :: this
            integer                         :: code
            code = this%code
        end function

end module