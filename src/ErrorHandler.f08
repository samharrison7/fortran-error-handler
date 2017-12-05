!> Module for dealing with errors.
module ErrorHandlerModule
    use ErrorInstanceModule
    implicit none
    private

    !> The class that is responsible for handling (i.e., adding and triggering)
    !! errors. Errors can be critical (resulting in the program being terminated)
    !! or just warnings.
    type, public :: ErrorHandler
        private
        type(ErrorInstance), allocatable    :: errors(:)                    !! Array of all possible errors.
        type(ErrorInstance), allocatable    :: errorQueue(:)                !! Queue of errors to be triggered.
        character(len=256)                  :: criticalPrefix = "Error:"    !! Prefix to output error message.
        character(len=256)                  :: warningPrefix = "Warning:"   !! Prefix to output error message.
        character(len=256)                  :: messageSuffix = ""           !! Suffix to output error message.
        logical                             :: isInitialised = .false.      !! Has the ErrorHandler been initialised?
        logical                             :: bashColors = .true.          !! Should colors be displayed in bash consoles?
        
        contains
            procedure, public :: init => initErrorHandler
            procedure, public :: queue
            procedure, public :: trigger
            procedure, public :: modify
            procedure, public :: errorExists

            ! Adding ErrorInstaces
            generic, public :: add => addErrorInstance, addMultipleErrorInstancesFromCodes, addMultipleErrorInstancesFromErrors
            procedure :: addErrorInstance, addMultipleErrorInstancesFromCodes, addMultipleErrorInstancesFromErrors

            ! Removing ErrorInstances
            generic, public :: remove => removeErrorInstance, removeMultipleErrorInstances
            procedure :: removeErrorInstance, removeMultipleErrorInstances

            ! Getters
            procedure, public :: getError
            procedure, public :: getErrors
            procedure, public :: getErrorFromCode
            procedure, public :: getNoError

            ! Setters
            procedure, public :: setErrors

            ! Initialised checks
            procedure, public :: stopIfNotInitialised
            procedure, public :: stopIfInitialised

            ! Testing
            procedure, public :: printErrors
    end type

    contains

        subroutine initErrorHandler(this, &
                        errors, &
                        criticalPrefix, &
                        warningPrefix, &
                        messageSuffix, &
                        bashColors)
            class(ErrorHandler), intent(inout) :: this
            type(ErrorInstance), intent(in), optional :: errors(:)
            character(len=*), intent(in), optional :: criticalPrefix
            character(len=*), intent(in), optional :: warningPrefix
            character(len=*), intent(in), optional :: messageSuffix
            logical, intent(in), optional :: bashColors
            integer :: i                                    ! Loop iterator.
            logical, allocatable :: mask(:)                 ! Logical mask to remove default errors from input errors array.
            type(ErrorInstance) :: defaultErrors(2)         ! Temporary array containing default errors.

            ! Stop the program running is ErrorHandler is already initialised
            call this%stopIfInitialised()

            ! Mark as initialised
            this%isInitialised = .true.

            ! Set queue to empty
            allocate(this%errorQueue(0))

            ! Construct the default errors array with the error that will always be present
            defaultErrors(1)%code = 0
            defaultErrors(1)%message = "No error."
            defaultErrors(1)%isCritical = .false.
            defaultErrors(2)%code = 1
            defaultErrors(2)%message = "An error has occurred."
            defaultErrors(2)%isCritical = .true.

            ! If array of ErrorInstances is provided, check if there are any errors
            ! using the default error codes present and override the default message/isCritical
            ! with these. Then, remove these errors from the input array to stop them
            ! being duplicated when the input errors are added to the default errors.
            if (present(errors)) then
                allocate(mask(size(errors)))
                mask = .false.       ! Logical mask to indicate the default error codes to remove
                do i=1, size(errors)
                    ! If a "no error" is provided
                    if (errors(i)%code == 0) then
                        defaultErrors(1)%code = 0
                        defaultErrors(1)%message = errors(i)%message
                        defaultErrors(1)%isCritical = errors(i)%isCritical
                        mask(i) = .true.
                    ! If a general error is provided
                    else if (errors(i)%code == 1) then
                        defaultErrors(2)%code = 1
                        defaultErrors(2)%message = errors(i)%message
                        defaultErrors(2)%isCritical = errors(i)%isCritical
                        mask(i) = .true.
                    end if
                end do

                ! Add the provided errors to the default errors
                ! Pack removes the default errors from input using the
                ! logical mask specified above.
                allocate(this%errors(1:2+size(pack(errors, .not. mask))))
                this%errors = defaultErrors
                call this%addMultipleErrorInstancesFromErrors(pack(errors, .not. mask))
            else
                this%errors = defaultErrors
            end if

            ! Set the error message prefixes
            if (present(criticalPrefix)) this%criticalPrefix = criticalPrefix
            if (present(warningPrefix)) this%warningPrefix = warningPrefix
            if (present(messageSuffix)) this%messageSuffix = messageSuffix

            ! Set whether bash colors wanted or not, and add them to prefixes if so
            if (present(bashColors)) this%bashColors = bashColors
            if (this%bashColors .eqv. .true.) this%criticalPrefix = "\x1B[91m" // adjustl(trim(this%criticalPrefix)) // "\x1B[0m"
            if (this%bashColors .eqv. .true.) this%warningPrefix = "\x1B[94m" // adjustl(trim(this%warningPrefix)) // "\x1B[0m"
        end subroutine

        !> Add an error to the list of possible errors,
        !! by specifying its code and message.
        subroutine addErrorInstance(this, code, message, isCritical, error)
            class(ErrorHandler)             :: this             !! Dummy argument
            integer, optional               :: code             !! Error code
            character(len=*), optional      :: message          !! Error message to be printed when handled
            logical, optional               :: isCritical       !! Should program execution be stopped?   
            type(ErrorInstance), optional   :: error            !! ErrorInstance to add
            type(ErrorInstance)             :: errorOut         ! The new error
            integer                         :: i                ! Loop iterator

            ! Stop the program running is ErrorHandler not initialised
            call this%stopIfNotInitialised()

            ! If an error code has been provided
            if (present(code)) then
                ! Check if the error instance already exists
                do i=1, size(this%errors)
                    if (this%errors(i)%code == code) then
                        write(*,"(a,a,i5,a)") trim(this%criticalPrefix), " Tried adding error code that already exists: ", code, "."
                        write(*,"(a)") "Did you mean to use modify() procedure instead?"
                        error stop 1
                    end if
                end do

                ! Store the parameters for the new error in the ErrorInstance
                errorOut%code = code
                if (present(message)) then
                    errorOut%message = message
                else
                    errorOut%message = ""
                end if
                if (present(isCritical)) then
                    errorOut%isCritical = isCritical
                else
                    errorOut%isCritical = .true.
                end if

                ! Add the new error to the errors array.
                this%errors = [this%errors, errorOut]

            ! Else, if an ErrorInstance has been provided
            else if (present(error)) then

                ! Check if the error instance already exists
                do i=lbound(this%errors,1), ubound(this%errors,1)
                    if (this%errors(i)%code == error%getCode()) then
                        write(*,"(a,a,i5,a)") trim(this%criticalPrefix), &
                            " Tried adding error code that already exists: ", error%getCode(), "."
                        write(*,"(a)") "Did you mean to use modify() procedure instead?"
                        error stop 1
                    end if
                end do

                ! Add the new error to the errors array.
                this%errors = [this%errors, error]

            end if
        end subroutine

        !> Add multiple error instances from an array of codes, messages and
        !! criticalities.
        subroutine addMultipleErrorInstancesFromCodes(this, codes, messages, areCritical)
            class(ErrorHandler)                     :: this             !! The ErrorHandler instance
            integer, intent(in)                     :: codes(:)         !! Error codes to add
            character(len=*), intent(in), optional  :: messages(:)      !! Messages to accompany the error codes
            logical, intent(in), optional           :: areCritical(:)   !! Array of criticality to accompany the error codes
            integer                                 :: i                ! Loop iterator

            ! Check that messages and areCritical array sizes are the same as codes
            if (present(messages)) then
                if (size(messages) /= size(codes)) then
                    write(*,"(a)") trim(this%criticalPrefix), &
                        " Mismatching array sizes for codes and messages parameters to ErrorHandler%add."
                    error stop 1
                end if
            end if
            if (present(areCritical)) then
                if (size(areCritical) /= size(codes)) then
                    write(*,"(a)") trim(this%criticalPrefix), &
                        " Mismatching array sizes for codes and areCritical parameters to ErrorHandler%add."
                    error stop 1
                end if
            end if
            ! If we've got this far, add the errors
            do i=1, size(codes)
                call this%addErrorInstance(codes(i), messages(i), areCritical(i))
            end do
            
        end subroutine

        !> Add multiple error instances from an array of ErrorInstances.
        subroutine addMultipleErrorInstancesFromErrors(this, errors)
            class(ErrorHandler)                 :: this         !! The ErrorHandler instance
            type(ErrorInstance), intent(in)     :: errors(:)    !! Array of errors to add
            integer                             :: i            ! Loop iterator

            ! Add the errors one by one
            do i=1, size(errors)
                call this%addErrorInstance(error=errors(i))
            end do
        end subroutine

        !> Modify the existing error with the given code.
        subroutine modify(this, code, message, isCritical, trace)
            class(ErrorHandler)                     :: this         !! Dummy class variable.
            integer, intent(in)                     :: code         !! Code of the error to modify.
            character(len=*), intent(in), optional  :: message      !! Modified error message.
            logical, intent(in), optional           :: isCritical   !! Modified error criticality.
            character(len=*), intent(in), optional  :: trace(:)     !! Modified error trace.
            integer                                 :: i            ! Loop iterator.
            logical                                 :: errorExists  ! Boolean to check if error exists.
            
            errorExists = .false.
            ! Loop through this%errors to see if the given code matches an existing one.
            ! Set message, criticality and trace if so.
            do i=lbound(this%errors,1), ubound(this%errors,1)
                if (this%errors(i)%code == code) then
                    if (present(message)) this%errors(i)%message = message
                    if (present(isCritical)) this%errors(i)%isCritical = isCritical
                    if (present(trace)) allocate(this%errors(i)%trace, source=trace)
                    errorExists = .true.
                end if
            end do

            ! If the error code wasn't found, stop with an error message saying so.
            if (.not. errorExists) then
                write(*,"(a,a,i5,a)") trim(this%criticalPrefix), &
                    " Trying to modify error with code that doesn't exist: ", code, "."
                error stop 1
            end if
        end subroutine

        !> Remove an ErrorInstance from the list of errors. If the specified
        !! error doesn't exist, then nothing happens. If the error is 0 or 1
        !! (the defaults), an error is thrown.
        subroutine removeErrorInstance(this, code)
            class(ErrorHandler) :: this                     !! This ErrorHandler instance.
            integer, intent(in) :: code                     !! Code of the error to remove.
            integer             :: i                        ! Loop iterator.
            logical             :: mask(size(this%errors))  ! Mask to help removing error.

            if (code == 0 .or. code == 1) then
                write(*,"(a,a,i1,a)") trim(this%criticalPrefix), " Cannot remove default error code ", code, "."
                write(*,"(a)") "Did you mean to use modify()?"
                error stop 1
            end if
            
            mask = .false.
            do i=1, size(this%errors)
                if (this%errors(i)%code == code) then
                    mask(i) = .true.
                end if
            end do
            
            ! Use the logical mask to get remove the error
            this%errors = pack(this%errors, .not. mask)
        end subroutine

        !> Remove multiple error instances from an array of codes. If the specified
        !! error doesn't exist, then nothing happens. If the error is 0 or 1
        !! (the defaults), an error is thrown.
        subroutine removeMultipleErrorInstances(this, codes)
            class(ErrorHandler) :: this         !! This ErrorHandler instance.
            integer, intent(in) :: codes(:)     !! Array of error codes to remove.
            integer             :: i            ! Loop iterator.

            do i=1, size(codes)
                call this%removeErrorInstance(codes(i))
            end do
        end subroutine

        !> Get predefined error with a given array key.
        function getError(this, key) result(error)
            class(ErrorHandler) :: this     !! This ErrorHandler instance.
            integer             :: key      !! Array key of the error.
            type(ErrorInstance) :: error    !! The error to return.
            error = this%errors(key)
        end function

        !> Return all of the predefined errors.
        function getErrors(this) result(errors)
            class(ErrorHandler) :: this                         !! This ErrorHandler instance.
            type(ErrorInstance) :: errors(size(this%errors))    !! The array of errors to return.
            errors = this%errors
        end function

        !> Set the list of errors. Any existing errors will be overriden
        subroutine setErrors(this, errors)
            class(ErrorHandler) :: this         !! The ErrorHandler instance
            type(ErrorInstance) :: errors(:)    !! New array of errors
            this%errors = errors
        end subroutine

        function errorExists(this, code)
            class(ErrorHandler) :: this         !! The ErrorHandler instance
            integer, intent(in) :: code         !! Error code to check existance far
            logical             :: errorExists  ! Whether or not the error exists
            integer             :: i            ! Loop iterator
            errorExists = .false.
            ! Loop through the existing error, check against code
            do i=lbound(this%errors,1), ubound(this%errors,1)
                if (this%errors(i)%code == code) errorExists = .true.
            end do
        end function

        !> Queue an error/errors so that it/they will be triggered on the next trigger() call.
        subroutine queue(this, code, error, errors)
            class(ErrorHandler)                 :: this             !! This ErrorHandler instance.
            integer, optional                   :: code             !! Code of error to queue.
            type(ErrorInstance), optional       :: error            !! ErrorInstance to queue.
            type(ErrorInstance), optional       :: errors(:)        !! Array of ErrorInstances to queue.
            type(ErrorInstance), allocatable    :: errorsToQueue(:) ! The generated ErrorInstance to add.
            integer                             :: k                ! Loop iterator.

            if (present(code)) then
                allocate(errorsToQueue(1))
                ! Returns no error if code isn't valid
                errorsToQueue(1) = this%getErrorFromCode(code)
            else if (present(error)) then
                allocate(errorsToQueue(1))
                ! Check if the error already exists
                if (this%errorExists(error%code)) then
                    ! Set the error to trigger the already existing one
                    errorsToQueue(1) = this%getErrorFromCode(error%code)
                    ! If message given for input error, override the default one
                    if (error%message /= "") errorsToQueue(1)%message = error%message
                    ! If there is a trace, add that
                    if (allocated(error%trace)) errorsToQueue(1)%trace = error%trace
                    ! isCritical defaults to .true. for ErrorInstances, so unless it's specified
                    ! when the error is declared, it will trigger as true, regardless of the
                    ! default criticality stored in the ErrorHandler.
                    errorsToQueue(1)%isCritical = error%isCritical
                ! If the error doesn't exist, queue it anyway. Good for on-the-fly error raising.
                else
                    errorsToQueue(1) = error
                end if
            else if (present(errors)) then
                allocate(errorsToQueue(size(errors)))
                ! Loop through input errors and see if that code already exists
                do k=1, size(errors)
                    if (this%errorExists(errors(k)%code)) then
                        ! Set the error to trigger the already existing one
                        errorsToQueue(k) = this%getErrorFromCode(errors(k)%code)
                        ! If a message given for the input error, override the default one
                        if (errors(k)%message /= "") errorsToQueue(k)%message = errors(k)%message
                        ! If there is a trace, add that
                        if (allocated(errors(k)%trace)) errorsToQueue(k)%trace = errors(k)%trace
                        ! isCritical default to .true. for ErrorInstances, so unless it's specified
                        ! when the error is declared, it will trigger as true, regardless of the
                        ! default criticality stored in the ErrorHandler.
                        errorsToQueue(k)%isCritical = errors(k)%isCritical
                    ! If the error doesn't exist, queue it anyway. Good for on-the-fly error raising.
                    else
                        errorsToQueue(k) = errors(k)
                    end if
                end do
            ! If no code, error or errors provided, make errorsToQueue zero-sized
            else
                allocate(errorsToQueue(0))
            end if

            this%errorQueue = [this%errorQueue, errorsToQueue]
        end subroutine

        !> Trigger an error from a code, ErrorInstance or array
        !! of ErrorInstances. If multiple are specified, then order of precedence
        !! is code, ErrorInstances, ErrorInstance. If there is a critical error,
        !! program execution is stopped, with exit code of first critical error.
        !! If no errors are specified, then nothing happens.
        subroutine trigger(this, code, error, errors)
            class(ErrorHandler)             :: this             !! Dummy argument
            integer, optional               :: code             !! Error code
            type(ErrorInstance), optional   :: error            !! ErrorInstance
            type(ErrorInstance), optional   :: errors(:)        !! Array of ErrorInstances
            
            character(len=256)              :: messagePrefix    ! The message prefix (critical or warning)
            character(len=1000)             :: outputMessage    ! The full message to be output
            character(len=500)              :: traceMessage     ! The strack trace message
            type(ErrorInstance), allocatable :: errorsOut(:)    ! The errors to output
            integer                         :: i, j, k          ! Loop iterators

            ! Stop the program running is ErrorHandler not initialised
            call this%stopIfNotInitialised()

            ! Try find error from code, then singular error, then array of errors, 
            ! then finally, if not present, then set error to generic error. If
            ! error code provided isn't valid, then no error returned. This is intentional.
            if (present(code)) then
                allocate(errorsOut(1))
                ! Returns no error if code isn't valid
                errorsOut(1) = this%getErrorFromCode(code)
                ! Add any queued errors, with queued errors coming first
                errorsOut = [this%errorQueue, errorsOut]
            else if (present(error)) then
                allocate(errorsOut(1))
                ! Check if the error already exists
                if (this%errorExists(error%code)) then
                    ! Set the error to trigger the already existing one
                    errorsOut(1) = this%getErrorFromCode(error%code)
                    ! If message given for input error, override the default one
                    if (error%message /= "") errorsOut(1)%message = error%message
                    ! If there is a trace, add that
                    if (allocated(error%trace)) errorsOut(1)%trace = error%trace
                    ! isCritical default to .true. for ErrorInstances, so unless it's specified
                    ! when the error is declared, it will trigger as true, regardless of the
                    ! default criticality stored in the ErrorHandler.
                    errorsOut(1)%isCritical = error%isCritical
                ! If the error doesn't exist, output it anyway. Good for on-the-fly error raising.
                else
                    errorsOut(1) = error
                end if
                ! Add any queued errors, with queued errors coming first
                errorsOut = [this%errorQueue, errorsOut]
            else if (present(errors)) then
                allocate(errorsOut(size(errors)))
                ! Loop through input errors and see if that code already exists
                do k=1, size(errors)
                    if (this%errorExists(errors(k)%code)) then
                        ! Set the error to trigger the already existing one
                        errorsOut(k) = this%getErrorFromCode(errors(k)%code)
                        ! If a message given for the input error, override the default one
                        if (errors(k)%message /= "") errorsOut(k)%message = errors(k)%message
                        ! If there is a trace, add that
                        if (allocated(errors(k)%trace)) errorsOut(k)%trace = errors(k)%trace
                        ! isCritical default to .true. for ErrorInstances, so unless it's specified
                        ! when the error is declared, it will trigger as true, regardless of the
                        ! default criticality stored in the ErrorHandler.
                        errorsOut(k)%isCritical = errors(k)%isCritical
                    ! If the error doesn't exist, output it anyway. Good for on-the-fly error raising.
                    else
                        errorsOut(k) = errors(k)
                    end if
                end do
                ! Add any queued errors, with queued errors coming first
                errorsOut = [this%errorQueue, errorsOut]
            else
                if (size(this%errorQueue) == 0) then
                    allocate(errorsOut(1))
                    errorsOut(1) = this%getErrorFromCode(1)
                    ! Add any queued errors, with queued errors coming first
                    errorsOut = [this%errorQueue, errorsOut]
                else
                    allocate(errorsOut, source=this%errorQueue)
                end if
            end if

            ! Only do something if there's actually an error (i.e., the
            ! error code isn't 0).
            do i=1, size(errorsOut)
                ! Check if error code > 0 and < 99999 also stops the program trying to
                ! print out errors for elements of arrays that mightn't have
                ! been used (e.g., if the wrong size array was declared). Don't do 
                ! anything if error code is zero.
                if (errorsOut(i)%getCode() > 0 .and. errorsOut(i)%getCode() < 99999) then
                    ! Set message prefix according to whether error is
                    ! critical or warning. Also add colour for Bash consoles.
                    if (errorsOut(i)%isCritical) then
                        messagePrefix = this%criticalPrefix
                    else
                        messagePrefix = this%warningPrefix
                    end if

                    ! Compose the message to output to the console
                    outputMessage = trim(messagePrefix) // " " // &
                                    trim(errorsOut(i)%message) // " " // &
                                    trim(this%messageSuffix)
                    ! Loop through the stack trace and add to the message,
                    ! then print
                    if (size(errorsOut(i)%trace)>0 .and. allocated(errorsOut(i)%trace)) then
                        traceMessage = "Trace: "

                        do j=1, size(errorsOut(i)%trace)
                            traceMessage = " " // trim(traceMessage) // " " // trim(errorsOut(i)%trace(j))
                            if (j<size(errorsOut(i)%trace)) traceMessage = " " // trim(traceMessage) // new_line('A') &
                                                            // "       >"
                        end do

                        write(*,"(a)") new_line('A') // trim(outputMessage)
                        write(*,"(a)") trim(adjustl(traceMessage))
                    else
                        write(*,"(a)") new_line('A') // trim(outputMessage)
                    end if
                    
                    if (i == size(errorsOut)) write(*,"(a)") ! Finish messages with new line
                end if
            end do

            ! Now we have to see if any of the errors were critical,
            ! and stop the program running if so. We'll trigger error
            ! stop on the first critical error found.
            do i=1, size(errorsOut)
                if (errorsOut(i)%isCritical) then
                    write(*,"(a)")                  ! New line
                    error stop errorsOut(i)%code
                end if
            end do

        end subroutine

        !> Check error handler is initialised. If it isn't,
        !! stop the program running.
        pure subroutine stopIfNotInitialised(this)
            class(ErrorHandler), intent(in) :: this     !! This ErrorHandler instance
            if (this%isInitialised .eqv. .false.) then
                error stop "Error handling not initialised. Call init() procedure on ErrorHandler object before using."
            end if
        end subroutine

        !> Check error handler is initialised. If it isn't,
        !! stop the program running.
        pure subroutine stopIfInitialised(this)
            class(ErrorHandler), intent(in) :: this     !! This ErrorHandler instance
            if (this%isInitialised .eqv. .true.) then
                error stop "Error handling already initialised, no need to call again."
            end if
        end subroutine

        !> Get an ErrorInstance object from its unique error code
        pure function getErrorFromCode(this, code) result(error)
            class(ErrorHandler), intent(in) :: this     !! This ErrorHandler instance
            integer, intent(in)             :: code     !! Error code
            type(ErrorInstance)             :: error    !! The returned error
            integer                         :: i        !! Loop iterator

            ! Stop the program running is ErrorHandler not initialised
            call this%stopIfNotInitialised()

            error = this%getNoError()                      ! No error (in case loop doesn't return one)
            do i=lbound(this%errors,1), ubound(this%errors,1)
                if (this%errors(i)%code == code) then
                    error = this%errors(i)
                end if
            end do
        end function

        !> Return the default no error, with code 0.
        pure function getNoError(this) result(noError)
            class(ErrorHandler), intent(in) :: this     !! This ErrorHandler instance
            type(ErrorInstance) :: noError              !! The no error to return

            ! Stop the program running is ErrorHandler not initialised
            call this%stopIfNotInitialised()
            ! Return the default no error
            noError = this%errors(1)
        end function

        !> Print all errors to the console, for debugging purposes
        subroutine printErrors(this)
            class(ErrorHandler), intent(in) :: this
            integer :: i

            do i=lbound(this%errors,1), ubound(this%errors,1)
                write(*,"(i5,a,a,a)") this%errors(i)%code, " ", trim(adjustl(this%errors(i)%message))
            end do
        end subroutine

end module