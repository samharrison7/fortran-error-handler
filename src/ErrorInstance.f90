module ErrorInstanceModule
    implicit none
    private

    !> An ErrorInstance represents an error with an associated error code,
    !! message, trace and whether or not the error is critical (i.e., stops
    !! the program executing).
    type, public :: ErrorInstance
        integer                         :: code = 1                 !! Numeric error code
        character(len=256)              :: message = ""             !! Message to accompany the error
        logical                         :: isCritical = .true.      !! Shoud program execution be stopped?
        character(len=256), allocatable :: trace(:)                 !! Custom backtrace for the error

        contains
            procedure, public :: addToTrace
            procedure, public :: getCode
            procedure, public :: isError
            procedure, public :: isCriticalError
            procedure, public :: notError
    end type

    !> Interface for creating new ErrorInstances using ErrorInstance(...) syntax
    interface ErrorInstance
        procedure init
    end interface

    contains
        !> Create a new ErrorInstance.
        function init(code, message, isCritical, trace) result(this)
            type(ErrorInstance)                     :: this             !! The ErrorInstance class
            integer, intent(in), optional           :: code             !! Code for the error
            character(len=*), intent(in), optional  :: message          !! Custom error message
            logical, intent(in), optional           :: isCritical       !! Is the error message critical?
            character(len=*), intent(in), optional  :: trace(:)         !! User-defined trace for the error
            integer                                 :: i                ! Loop iterator
            character(len=256), allocatable         :: tempTrace(:)     ! Temporary fixed-length character string array for trace node

            if (present(code)) this%code = code                     ! Defaults to 1
            if (present(message)) this%message = message            ! Defaults to ""
            if (present(isCritical)) this%isCritical = isCritical   ! Defaults to .true.
            ! If trace present, loop through and convert the nodes to fixed-length
            ! character strings before storing in this%trace.
            if (present(trace)) then
                allocate(tempTrace(size(trace)))       ! Make tempTrace the correct size
                do i=1, size(trace)
                    tempTrace(i) = trace(i)
                end do
                allocate(this%trace, source=tempTrace)
            end if
        end function

        !> Add a node to the trace.
        !! WARNING: GFortran bug means this must be compiled with
        !! flag -O1 at least. See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=70231
        !! and https://stackoverflow.com/questions/44385909/adding-to-an-array-of-characters-in-fortran
        subroutine addToTrace(this, message)
            class(ErrorInstance), intent(inout) :: this
            character(len=*), intent(in) :: message
            character(len=256) :: tempMessage

            tempMessage = message       ! Make character length 256
            ! Check if this is the first time something has been added to
            ! the trace or not. If it is, allocate as null array.
            if (.not. allocated(this%trace)) allocate(this%trace(0))
            ! Add the new node to the trace array.
            this%trace = [this%trace, tempMessage]
        end subroutine

        !> Return the error code.
        function getCode(this) result(code)
            class(ErrorInstance), intent(in)    :: this
            integer                             :: code
            code = this%code
        end function

        !> Is the error an actual error or code 0 (not an error)?
        function isError(this)
            class(ErrorInstance), intent(in)    :: this
            logical                             :: isError
            isError = .true.
            if (this%code == 0) isError = .false.
        end function

        !> Is the error a critical error?
        function isCriticalError(this)
            class(ErrorInstance), intent(in)    :: this
            logical                             :: isCriticalError
            isCriticalError = .false.
            if (this%code /= 0 .and. this%isCritical) isCriticalError = .true.
        end function

        !> Is the error code 0 (not an error)?
        function notError(this)
            class(ErrorInstance), intent(in)    :: this
            logical                             :: notError
            notError = .true.
            if (this%code /= 0) notError = .false.
        end function

end module