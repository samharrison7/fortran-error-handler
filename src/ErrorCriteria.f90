!> Module container for `ErrorCriteria` class
module ErrorCriteriaModule
    use ErrorHandlerModule
    use ErrorInstanceModule
    implicit none
    private

    ! Set kind parameters for use with real variables, as per the convention
    ! recommended in, e.g., http://fortranwiki.org/fortran/show/Real+precision
    ! and N. S. Clerman and W. Spector, Modern Fortran (2012).
    integer, parameter :: dp = selected_real_kind(15,307)       !! Double precision, 15 digits, 1e307
    integer, parameter :: qp = selected_real_kind(33,4931)      !! Quadruple precision, 33 digits, 1e4931

    !> ErrorCrtiera extends ErrorHandler and defines a number of common "criteria" used
    !! for error checking, such as checking whether a number falls between given bounds.
    !! Criteria functions expedite the error checking process with intuitive function calls 
    !! returning pre-defined ErrorInstances.
    !! Fortran's lack of out-of-the-box key=>value arrays (e.g., dictionaries, hash
    !! tables, lists) makes this seemingly trivial task a bit complex. To save overcomplicating
    !! things and using, e.g., third-party linked lists, we'll specify that each criterion is
    !! aligned with a specific array index, and the codes/message/criticality arrays below are
    !! filed following this specification:
    !!      Array index     Function name       Default code
    !!      1               nonZero             101
    !!      2               zero                102
    !!      3               lessThan            103
    !!      4               greaterThan         104
    !!      5               limit               105
    !!      6               notEqual            106
    !!      7               equal               107
    !!      8               positive            108
    !!      9               negative            109
    type, public, extends(ErrorHandler) :: ErrorCriteria

        ! Current error criteria codes, messages and criticality will be used to keep track of the
        ! codes/messages/criticality that relate to the error criteria in amongst other error codes.
        integer, allocatable             :: currentErrorCriteriaCodes(:)
        character(len=256), allocatable  :: currentErrorCriteriaNames(:)
        character(len=256), allocatable  :: currentErrorCriteriaMessages(:)
        logical, allocatable             :: currentErrorCriteriaIsCritical(:)

        ! Default error criteria codes and messages that are used if none others specified.
        integer             :: defaultErrorCriteriaCodes(9)
        character(len=256)  :: defaultErrorCriteriaNames(9)
        character(len=256)  :: defaultErrorCriteriaMessages(9)
        logical             :: defaultErrorCriteriaIsCritical(9)

        ! Tolerance to accept when equating numbers (how far away from the criteria
        ! is the value allowed to be to pass).
        real                :: epsilon = 1.0e-5

        contains
            ! Initialising and setters
            procedure, public :: init => initErrorCriteria      ! Overload Errorhandler init procedure
            procedure, public :: setEpsilon

            ! Getters and adding
            procedure, public :: getCodeFromCriterionName
            procedure, public :: getIndexFromCriterionName
            procedure, public :: addErrorCriterion
            procedure, public :: addErrorCriteria

            ! Removing ErrorInstances: removeErrorInstance and removeErrorInstances are
            ! bound to remove generic in parent. Here, we overload then to check that
            ! the errors being removed aren't in the current error criteria.
            procedure, public :: removeErrorInstance => removeErrorInstanceCheckCriteria
            procedure, public :: removeMultipleErrorInstances => removeMultipleErrorInstancesCheckCriteria

            ! Modify error criteria codes
            procedure, public :: modifyErrorCriteriaCodes       
            generic, public :: modifyErrorCriterionCode => modifyErrorCriterionCodeByIndex, modifyErrorCriterionCodeByName
            procedure, public :: modifyErrorCriterionCodeByIndex
            procedure, public :: modifyErrorCriterionCodeByName

            ! The criteria functions
            generic, public :: limit => integerLimit, realLimit, realDPLimit, realQPLimit
            generic, public :: nonZero => integerNonZero, realNonZero, realDPNonZero, realQPNonZero
            generic, public :: zero => integerZero, realZero, realDPZero, realQPZero
            generic, public :: lessThan => integerLessThan, realLessThan, realDPLessThan, realQPLessThan
            generic, public :: greaterThan => integerGreaterThan, realGreaterThan, realDPGreaterThan, realQPGreaterThan
            generic, public :: notEqual => integerNotEqual, realNotEqual, realDPNotEqual, realQPNotEqual
            generic, public :: equal => integerEqual, realEqual, realDPEqual, realQPEqual
            generic, public :: positive => integerPositive, realPositive, realDPPositive, realQPPositive
            generic, public :: negative => integerNegative, realNegative, realDPNegative, realQPNegative
            ! Limit
            procedure, private :: integerLimit
            procedure, private ::  realLimit
            procedure, private :: realDPLimit
            procedure, private :: realQPLimit
            ! Non-zero
            procedure, private :: integerNonZero
            procedure, private :: realNonZero
            procedure, private :: realDPNonZero
            procedure, private :: realQPNonZero
            ! Zero
            procedure, private :: integerZero
            procedure, private :: realZero
            procedure, private :: realDPZero
            procedure, private :: realQPZero
            ! Less than
            procedure, private :: integerLessThan
            procedure, private :: realLessThan
            procedure, private :: realDPLessThan
            procedure, private :: realQPLessThan
            ! Greater than
            procedure, private :: integerGreaterThan
            procedure, private :: realGreaterThan
            procedure, private :: realDPGreaterThan
            procedure, private :: realQPGreaterThan
            ! Not equal
            procedure, private :: integerNotEqual
            procedure, private :: realNotEqual
            procedure, private :: realDPNotEqual
            procedure, private :: realQPNotEqual
            ! Equal
            procedure, private :: integerEqual
            procedure, private :: realEqual
            procedure, private :: realDPEqual
            procedure, private :: realQPEqual
            ! Positive
            procedure, private :: integerPositive
            procedure, private :: realPositive
            procedure, private :: realDPPositive
            procedure, private :: realQPPositive
            ! Negative
            procedure, private :: integerNegative
            procedure, private :: realNegative
            procedure, private :: realDPNegative
            procedure, private :: realQPNegative
    end type

    contains
        !> Initialise the ErrorCriteria and at the same time initialise
        !! the parent ErrorHandler, setting custom errors.
        subroutine initErrorCriteria(this, &
                                     errors, &
                                     criticalPrefix, &
                                     warningPrefix, &
                                     messageSuffix, &
                                     bashColors, &
                                     printErrorCode, &
                                     triggerWarnings, &
                                     on)
            class(ErrorCriteria), intent(inout)         :: this                 !! This ErrorCriteria instance
            type(ErrorInstance), intent(in), optional   :: errors(:)            !! Custom defined errors
            character(len=*), intent(in), optional      :: criticalPrefix       !! Prefix to critical error messages
            character(len=*), intent(in), optional      :: warningPrefix        !! Prefix to warning error messages
            character(len=*), intent(in), optional      :: messageSuffix        !! Suffix to error messages
            logical, intent(in), optional               :: bashColors           !! Should prefixes be colored in bash shells?
            logical, intent(in), optional               :: printErrorCode       !! Should error messages be prefixed with the error code?
            logical, intent(in), optional               :: triggerWarnings      !! Should warnings be printing on trigger?
            logical, intent(in), optional               :: on                   !! Should the ErrorHandler output errors?

            ! Initialise the parent ErrorHandler                                                                    
            call this%ErrorHandler%init( &
                errors=errors, &
                criticalPrefix=criticalPrefix, &
                warningPrefix=warningPrefix, &
                messageSuffix=messageSuffix, &
                bashColors=bashColors, &
                triggerWarnings=triggerWarnings, &
                printErrorCode=printErrorCode, &
                on=on &
            )

            ! Define the default error criteria. Messages will be overidden by specific
            ! criteria functions to provide more detail, but they're present here in case
            ! criteria errors triggered directly by user.
            this%defaultErrorCriteriaCodes = [101,102,103,104,105,106,107,108,109]
            this%defaultErrorCriteriaNames = [character(len=256) :: &
                "nonZero", &
                "zero", &
                "lessThan", &
                "greaterThan", &
                "limit", &
                "notEqual", &
                "equal", &
                "positive", &
                "negative" &
            ]
            this%defaultErrorCriteriaMessages = [character(len=256) :: &
                "Value must be non-zero.", &
                "Value must be zero.", &
                "Value must be less than criteria.", &
                "Value must be greater than criteria.", &
                "Value must be between limit criteria.", &
                "Value must not equal criteria.", &
                "Value must equal criteria.", &
                "Value must be positive.", &
                "Value must be negative." &
            ]
            this%defaultErrorCriteriaIsCritical = .true.

            ! Add the default error criteria codes
            call this%ErrorHandler%add(& 
                codes = this%defaultErrorCriteriaCodes, &
                messages = this%defaultErrorCriteriaMessages, &
                areCritical = this%defaultErrorCriteriaIsCritical &
            )

            ! Set the current error criteria to the current
            allocate(this%currentErrorCriteriaCodes, source=this%defaultErrorCriteriaCodes)
            allocate(this%currentErrorCriteriaNames, source=this%defaultErrorCriteriaNames)
            allocate(this%currentErrorCriteriaMessages, source=this%defaultErrorCriteriaMessages)
            allocate(this%currentErrorCriteriaIsCritical, source=this%defaultErrorCriteriaIsCritical)
        end subroutine

        !> Set epsilon, the tolerance allowed when equating
        !! values/criteria, to account for imprecision in floating
        !! point numbers.
        subroutine setEpsilon(this, epsilon)
            class(ErrorCriteria), intent(inout) :: this     !! This ErrorCriteria instance
            real, intent(in)                    :: epsilon  !! The tolerance allowed
            this%epsilon = epsilon
        end subroutine

        !> Modify the error codes for the error criteria. Defaults given
        !! in the docs for the ErrorCriteria derived type, as well as the
        !! indices required for the codes parameter. Must be the same size
        !! as the number of criteria.
        subroutine modifyErrorCriteriaCodes(this, codes)
            class(ErrorCriteria)            :: this         !! This ErrorCriteria instance
            integer, intent(in)             :: codes(:)     !! The new codes

            if (size(codes) /= size(this%currentErrorCriteriaCodes)) then
                error stop "Error modifying error criteria codes: Array of new codes doesn't match number of criteria functions."
            end if

            ! Stop if we haven't initialised the error handler
            call this%stopIfNotInitialised

            ! Remove the old codes (use the ErrorHandler's method to avoid throwing
            ! error that the error code is an error criteria one)
            call this%ErrorHandler%remove(codes=this%currentErrorCriteriaCodes)
            ! Add the new ones
            call this%ErrorHandler%add( &
                codes=codes, &
                messages=this%currentErrorCriteriaMessages, &
                areCritical=this%currentErrorCriteriaIsCritical &
            )
            ! Update the current error criteria codes array
            this%currentErrorCriteriaCodes = codes
        end subroutine

        !> Modify criterion error at given array index, with the array
        !! index corresponding to the functions according to the docs for
        !! the ErrorCriteria derived type.
        subroutine modifyErrorCriterionCodeByIndex(this, index, newCode)
            class(ErrorCriteria)    :: this         !! This ErrorCriteria instance
            integer, intent(in)     :: index        !! Array index to change code at
            integer, intent(in)     :: newCode      !! New error code

            ! Stop if we haven't initialised the error handler
            call this%stopIfNotInitialised

            if (index > size(this%currentErrorCriteriaCodes)) then
                error stop "Error modifying error criterion code by index: Index doesn't match any criteria functions."
            end if

            ! Remove the old code (use the ErrorHandler's method to avoid throwing
            ! error that the error code is an error criteria one).
            call this%ErrorHandler%remove(code=this%currentErrorCriteriaCodes(index))
            ! Add the new code
            call this%ErrorHandler%add( &
                code=newCode, &
                message=this%currentErrorCriteriaMessages(index), &
                isCritical=this%currentErrorCriteriaIsCritical(index) &
            )
            ! Update the current error criteria codes array
            this%currentErrorCriteriaCodes(index) = newCode
        end subroutine

        !> Modify criterion error code with given criterion name, with the array
        !! index corresponding to the functions according to the docs for
        !! the ErrorCriteria derived type.
        subroutine modifyErrorCriterionCodeByName(this, name, newCode)
            class(ErrorCriteria)            :: this         !! This ErrorCriteria instance
            character(len=*), intent(in)    :: name         !! Function name to change the code for
            integer, intent(in)             :: newCode      !! New error code
            integer                         :: index        ! The array index corresponding to the function name

            ! Stop if we haven't initialised the error handler
            call this%stopIfNotInitialised

            ! Get the index of the named error criterion in the error criteria array,
            ! then use the modifyErrorCriterionCodeByIndex method to modify
            index = this%getIndexFromCriterionName(name)
            if (index == 0) then
                error stop "Error modifying error criterion code by name: Name doesn't match any criteria functions."
            end if
            call this%modifyErrorCriterionCodeByIndex(index, newCode)
        end subroutine

        !> Get the error code from the criterion function name
        function getCodeFromCriterionName(this, name) result(code)
            class(ErrorCriteria), intent(in)    :: this     !! This ErrorCriteria instance
            character(len=*), intent(in)        :: name     !! Function name to get code for
            integer                             :: code     !! The error code
            integer                             :: index    ! The index corersponding to the function name

            ! Stop if we haven't initialised the error handler
            call this%stopIfNotInitialised

            ! Get the index from the name, and use that to get the code
            index = this%getIndexFromCriterionName(name)
            if (lbound(this%currentErrorCriteriaCodes,1) <= index .and. ubound(this%currentErrorCriteriaCodes,1) >= index) then
                code = this%currentErrorCriteriaCodes(index)
            else
                code = 0    ! Not found
            end if
        end function

        !> Get the index of the error criteria array for a given criterion name
        function getIndexFromCriterionName(this, name) result(index)
            class(ErrorCriteria), intent(in)    :: this     !! This ErrorCriteria instance
            character(len=*), intent(in)        :: name     !! Function name to get the index for
            integer                             :: index    !! Index for the given function name
            integer                             :: i        ! Loop iterator

            ! Stop if we haven't initialised the error handler
            call this%stopIfNotInitialised

            index = 0   ! Default in case criterion doesn't exist
            ! Loop through the criteria names
            do i=1, size(this%currentErrorCriteriaNames)
                if (name==this%currentErrorCriteriaNames(i)) index = i
            end do
        end function

        !> Remove an ErrorInstance from the errors array, but check if
        !! it is a code for an error criterion first. If it is, throw an error
        !! message to that effect and suggest using modify() instead.
        subroutine removeErrorInstanceCheckCriteria(this, code)
            class(ErrorCriteria)    :: this     !! This ErrorCriteria instance
            integer, intent(in)     :: code     !! The code to remove
            integer                 :: i        ! Loop iterator

            ! Stop if we haven't initialised the error handler
            call this%stopIfNotInitialised
            
            ! Firstly, check if the code specified is an error criteria
            ! code, and if so, throw an error.
            do i=1, size(this%currentErrorCriteriaCodes)
                if (this%currentErrorCriteriaCodes(i) == code) then
                    write(*,'(a,i5,a)') "ERROR: Trying to remove error code that is used in error criteria: ", code, "."
                    write(*,'(a)') "Use modifyErrorCriterionCode() instead."
                    error stop 1
                end if
            end do

            ! If error isn't an error criteria, then remove using normal method
            call this%ErrorHandler%remove(code)
        end subroutine

        !> Remove multiple ErrorInstances from the errors array, but check if
        !! any of the codes are for an error criterion first. If they are, throw an error
        !! message to that effect and suggest using modify() instead.
        subroutine removeMultipleErrorInstancesCheckCriteria(this, codes)
            class(ErrorCriteria)    :: this         !! This ErrorCriteria instance
            integer, intent(in)     :: codes(:)     !! The array of codes to remove
            integer                 :: i            ! Loop iterator

            ! Loop through the codes and try and remove one-by-one.
            ! removeErrorInstance checks if initialised.
            do i=1, size(codes)
                call this%removeErrorInstance(codes(i))
            end do
        end subroutine

        !> Add a new error criterion to the list of current error criteria and
        !! list of errors. A separate criterion function(s) must accompany this
        !! new criterion.
        subroutine addErrorCriterion(this, code, name, message, isCritical)
            class(ErrorCriteria)    :: this         !! This ErrorCriteria instance
            integer, intent(in)     :: code         !! Error code for the new criterion
            character(len=*)        :: name         !! Name of this criterion
            character(len=*)        :: message      !! Error message for this criterion
            logical                 :: isCritical   !! If triggered, is it critical?

            ! Add the error to the errors array
            call this%ErrorHandler%add(& 
                code = code, &
                message = message, &
                isCritical = isCritical &
            )

            ! Reallocate the current error criteria to include the newly added one
            deallocate(this%currentErrorCriteriaCodes)
            allocate(this%currentErrorCriteriaCodes, source=[this%defaultErrorCriteriaCodes,code])
            deallocate(this%currentErrorCriteriaNames)
            allocate(this%currentErrorCriteriaNames, source=[this%defaultErrorCriteriaNames,name])
            deallocate(this%currentErrorCriteriaMessages)
            allocate(this%currentErrorCriteriaMessages, source=[this%defaultErrorCriteriaMessages,message])
            deallocate(this%currentErrorCriteriaIsCritical)
            allocate(this%currentErrorCriteriaIsCritical, source=[this%defaultErrorCriteriaIsCritical,isCritical])
        end subroutine

        !> Add an array of new error criteria to the list of current error criteria 
        !! and list of errors. Separate criteria functions must accompany these
        !! new criteria.
        subroutine addErrorCriteria(this, codes, names, messages, areCritical)
            class(ErrorCriteria)    :: this             !! This ErrorCriteria instance
            integer, intent(in)     :: codes(:)         !! Error codes for the new criteria
            character(len=*)        :: names(:)         !! Names for the criteria
            character(len=*)        :: messages(:)      !! Error messages
            logical                 :: areCritical(:)   !! Are the errors critical?

            ! Add the errors to the errors array
            call this%ErrorHandler%add( & 
                codes = codes, &
                messages = messages, &
                areCritical = areCritical &
            )

            ! Reallocate the current error criteria to include the newly added ones
            deallocate(this%currentErrorCriteriaCodes)
            allocate(this%currentErrorCriteriaCodes, source=[this%defaultErrorCriteriaCodes,codes])
            deallocate(this%currentErrorCriteriaNames)
            allocate(this%currentErrorCriteriaNames, source=[this%defaultErrorCriteriaNames,names])
            deallocate(this%currentErrorCriteriaMessages)
            allocate(this%currentErrorCriteriaMessages, source=[this%defaultErrorCriteriaMessages,messages])
            deallocate(this%currentErrorCriteriaIsCritical)
            allocate(this%currentErrorCriteriaIsCritical, source=[this%defaultErrorCriteriaIsCritical,areCritical])
        end subroutine

!--------------------!
!-- CRITERIA TESTS --!
!--------------------!

!-- LIMIT --!

        !> Test whether an integer value falls within a limit. If only upper
        !! or lower bounds are specified, the value must be less than
        !! or greater than the limit (respetively).
        function integerLimit(this, value, lbound, ubound, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            integer, intent(in)                     :: value            !! The value to test
            integer, intent(in), optional           :: lbound           !! The lower bound of the limit
            integer, intent(in), optional           :: ubound           !! The upper bound of the limit
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charLbound       ! Character variable to store lbound in
            character(len=100)                      :: charUbound       ! Character variable to store ubound in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .false.
            ! Check if value between limits, or if only one limit provided,
            ! check if value greater than/less than
            if (present(lbound) .and. present(ubound)) then
                ! Stop if lbound greater than upper bound
                if (lbound > ubound) error stop "Error criteria limit specified with lower bound greater than upper bound."
                if (value >= lbound .and. value <= ubound) pass = .true.
            else if (present(lbound)) then
                if (value >= lbound) pass = .true.
            else if (present(ubound)) then
                if (value <= ubound) pass = .true.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('limit'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    if (present(lbound) .and. present(ubound)) then
                        write(charLbound,*) lbound
                        write(charUbound,*) ubound
                        error%message = "Value must be between " &
                                        // trim(adjustl(charLbound)) // " and " // trim(adjustl(charUbound)) // ". " &
                                        // "Given value: " // trim(adjustl(charValue)) // "."
                    ! Message for greater than test
                    else if (present(lbound)) then
                        write(charLbound,*) lbound
                        error%message = "Value must be greater than " &
                                        // trim(adjustl(charLbound)) // ". " &
                                        // "Given value: " // trim(adjustl(charValue)) // "."
                    ! Message for less than test
                    else if (present(ubound)) then
                        write(charUbound,*) ubound
                        error%message = "Value must be less than " &
                                        // trim(adjustl(charUbound)) // ". " &
                                        // "Given value: " // trim(adjustl(charValue)) // "."
                    end if
                end if
                ! Add trace message
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real value falls within a limit. If only upper
        !! or lower bounds are specified, the value must be less than
        !! or greater than the limit (respetively).
        function realLimit(this, value, lbound, ubound, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            real, intent(in)                        :: value            !! The value to test
            real, intent(in), optional              :: lbound           !! The lower bound of the limit
            real, intent(in), optional              :: ubound           !! The upper bound of the limit
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charLbound       ! Character variable to store lbound in
            character(len=100)                      :: charUbound       ! Character variable to store ubound in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .false.
            ! Check if value between limits, or if only one limit provided,
            ! check if value greater than/less than
            if (present(lbound) .and. present(ubound)) then
                ! Stop if lbound greater than upper bound
                if (lbound > ubound) error stop "Error criteria limit specified with lower bound greater than upper bound."
                if (value >= lbound .and. value <= ubound) pass = .true.
            else if (present(lbound)) then
                if (value >= lbound) pass = .true.
            else if (present(ubound)) then
                if (value <= ubound) pass = .true.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('limit'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    if (present(lbound) .and. present(ubound)) then
                        write(charLbound,*) lbound
                        write(charUbound,*) ubound
                        error%message = "Value must be between " &
                                        // trim(adjustl(charLbound)) // " and " // trim(adjustl(charUbound)) // ". " &
                                        // "Given value: " // trim(adjustl(charValue)) // "."
                    ! Message for greater than test
                    else if (present(lbound)) then
                        write(charLbound,*) lbound
                        error%message = "Value must be greater than " &
                                        // trim(adjustl(charLbound)) // ". " &
                                        // "Given value: " // trim(adjustl(charValue)) // "."
                    ! Message for less than test
                    else if (present(ubound)) then
                        write(charUbound,*) ubound
                        error%message = "Value must be less than " &
                                        // trim(adjustl(charUbound)) // ". " &
                                        // "Given value: " // trim(adjustl(charValue)) // "."
                    end if
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(dp) value falls within a limit. If only upper
        !! or lower bounds are specified, the value must be less than
        !! or greater than the limit (respetively).
        function realDPLimit(this, value, lbound, ubound, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            real(dp), intent(in)                    :: value            !! The value to test
            real(dp), intent(in), optional          :: lbound           !! The lower bound of the limit
            real(dp), intent(in), optional          :: ubound           !! The upper bound of the limit
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charLbound       ! Character variable to store lbound in
            character(len=100)                      :: charUbound       ! Character variable to store ubound in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .false.
            ! Check if value between limits, or if only one limit provided,
            ! check if value greater than/less than
            if (present(lbound) .and. present(ubound)) then
                ! Stop if lbound greater than upper bound
                if (lbound > ubound) error stop "Error criteria limit specified with lower bound greater than upper bound."
                if (value >= lbound .and. value <= ubound) pass = .true.
            else if (present(lbound)) then
                if (value >= lbound) pass = .true.
            else if (present(ubound)) then
                if (value <= ubound) pass = .true.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('limit'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    if (present(lbound) .and. present(ubound)) then
                        write(charLbound,*) lbound
                        write(charUbound,*) ubound
                        error%message = "Value must be between " &
                                        // trim(adjustl(charLbound)) // " and " // trim(adjustl(charUbound)) // ". " &
                                        // "Given value: " // trim(adjustl(charValue)) // "."
                    ! Message for greater than test
                    else if (present(lbound)) then
                        write(charLbound,*) lbound
                        error%message = "Value must be greater than " &
                                        // trim(adjustl(charLbound)) // ". " &
                                        // "Given value: " // trim(adjustl(charValue)) // "."
                    ! Message for less than test
                    else if (present(ubound)) then
                        write(charUbound,*) ubound
                        error%message = "Value must be less than " &
                                        // trim(adjustl(charUbound)) // ". " &
                                        // "Given value: " // trim(adjustl(charValue)) // "."
                    end if
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(qp) value falls within a limit. If only upper
        !! or lower bounds are specified, the value must be less than
        !! or greater than the limit (respetively).
        function realQPLimit(this, value, lbound, ubound, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            real(qp), intent(in)                    :: value            !! The value to test
            real(qp), intent(in), optional          :: lbound           !! The lower bound of the limit
            real(qp), intent(in), optional          :: ubound           !! The upper bound of the limit
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charLbound       ! Character variable to store lbound in
            character(len=100)                      :: charUbound       ! Character variable to store ubound in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .false.
            ! Check if value between limits, or if only one limit provided,
            ! check if value greater than/less than
            if (present(lbound) .and. present(ubound)) then
                ! Stop if lbound greater than upper bound
                if (lbound > ubound) error stop "Error criteria limit specified with lower bound greater than upper bound."
                if (value >= lbound .and. value <= ubound) pass = .true.
            else if (present(lbound)) then
                if (value >= lbound) pass = .true.
            else if (present(ubound)) then
                if (value <= ubound) pass = .true.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('limit'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    if (present(lbound) .and. present(ubound)) then
                        write(charLbound,*) lbound
                        write(charUbound,*) ubound
                        error%message = "Value must be between " &
                                        // trim(adjustl(charLbound)) // " and " // trim(adjustl(charUbound)) // ". " &
                                        // "Given value: " // trim(adjustl(charValue)) // "."
                    ! Message for greater than test
                    else if (present(lbound)) then
                        write(charLbound,*) lbound
                        error%message = "Value must be greater than " &
                                        // trim(adjustl(charLbound)) // ". " &
                                        // "Given value: " // trim(adjustl(charValue)) // "."
                    ! Message for less than test
                    else if (present(ubound)) then
                        write(charUbound,*) ubound
                        error%message = "Value must be less than " &
                                        // trim(adjustl(charUbound)) // ". " &
                                        // "Given value: " // trim(adjustl(charValue)) // "."
                    end if
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

!-- NON-ZERO --!

        !> Test whether an integer is non-zero.
        function integerNonZero(this, value, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            integer, intent(in)                     :: value            !! The value to test
            real, optional, intent(in)              :: epsilon          !! How close to zero can the value be?
            character(len=*), optional, intent(in)  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .false.
            ! Check if value not equal to 0
            if (value /= 0) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('nonZero'))    ! Get the non-zero error
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be non-zero. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real number is non-zero, or further than a specified
        !! amount (epsilon) from zero.
        function realNonZero(this, value, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            real, intent(in)                        :: value            !! The value to test
            real, optional, intent(in)              :: epsilon          !! How close to zero can the value be?
            character(len=*), optional, intent(in)  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .true.
            ! Check if the value is outside of 0 +- epsilon.
            if (present(epsilon)) then
                if (abs(value) <= epsilon) pass = .false.
            else
                if (abs(value) <= this%epsilon) pass = .false.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('nonZero'))    ! Get the non-zero error
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be non-zero. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(dp) number is non-zero, or further than a specified
        !! amount (epsilon) from zero.
        function realDPNonZero(this, value, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            real(dp), intent(in)                    :: value            !! The value to test
            real, optional, intent(in)              :: epsilon          !! How close to zero can the value be?
            character(len=*), optional, intent(in)  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .true.
            ! Check if the value is outside of 0 +- epsilon.
            if (present(epsilon)) then
                if (abs(value) <= epsilon) pass = .false.
            else
                if (abs(value) <= this%epsilon) pass = .false.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('nonZero'))    ! Get the non-zero error
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be non-zero. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(qp) number is non-zero, or further than a specified
        !! amount (epsilon) from zero.
        function realQPNonZero(this, value, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            real(qp), intent(in)                    :: value            !! The value to test
            real, optional, intent(in)              :: epsilon          !! How close to zero can the value be?
            character(len=*), optional, intent(in)  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .true.
            ! Check if the value is outside of 0 +- epsilon.
            if (present(epsilon)) then
                if (abs(value) <= epsilon) pass = .false.
            else
                if (abs(value) <= this%epsilon) pass = .false.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('nonZero'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be non-zero. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

!-- ZERO --!

        !> Test whether an integer is zero.
        function integerZero(this, value, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            integer, intent(in)                     :: value            !! The value to test
            real, optional, intent(in)              :: epsilon          !! How far from zero can the value be?
            character(len=*), optional, intent(in)  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .false.
            ! Check if value is 0
            if (value == 0) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('zero'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be zero. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real is within epsilon of zero.
        function realZero(this, value, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            real, intent(in)                        :: value            !! The value to test
            real, optional, intent(in)              :: epsilon          !! How far from zero can the value be?
            character(len=*), optional, intent(in)  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .true.
            ! Check if the value is outside of 0 +- epsilon
            if (present(epsilon)) then
                if (abs(value) >= epsilon) pass = .false.
            else
                if (abs(value) >= this%epsilon) pass = .false.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('zero'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be zero. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(dp) is within epsilon of zero.
        function realDPZero(this, value, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            real(dp), intent(in)                    :: value            !! The value to test
            real, optional, intent(in)              :: epsilon          !! How far from zero can the value be?
            character(len=*), optional, intent(in)  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .true.
            ! Check if the value is outside of 0 +- epsilon
            if (present(epsilon)) then
                if (abs(value) >= epsilon) pass = .false.
            else
                if (abs(value) >= this%epsilon) pass = .false.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('zero'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be zero. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(qp) is within epsilon of zero.
        function realQPZero(this, value, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            real(qp), intent(in)                    :: value            !! The value to test
            real, optional, intent(in)              :: epsilon          !! How far from zero can the value be?
            character(len=*), optional, intent(in)  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .true.
            ! Check if the value is outside of 0 +- epsilon
            if (present(epsilon)) then
                if (abs(value) >= epsilon) pass = .false.
            else
                if (abs(value) >= this%epsilon) pass = .false.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('zero'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be zero. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

!-- LESS THAN --!

        !> Test whether an integer value is less than given criteria.
        function integerLessThan(this, value, ubound, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            integer, intent(in)                     :: value            !! The value to test
            integer, intent(in)                     :: ubound           !! Value must be less than than ubound
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charUbound       ! Character variable to store ubound in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .false.
            ! Check if value is less than criterion
            if (value <= ubound) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('lessThan'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charUbound,*) ubound
                    error%message = "Value must be less than " &
                                    // trim(adjustl(charUbound)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real value is less than given criteria.
        function realLessThan(this, value, ubound, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            real, intent(in)                        :: value            !! The value to test
            real, intent(in)                        :: ubound           !! Value must be less than than ubound
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charUbound       ! Character variable to store ubound in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .false.
            ! Check if value is less than criterion
            if (value <= ubound) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('lessThan'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charUbound,*) ubound
                    error%message = "Value must be less than " &
                                    // trim(adjustl(charUbound)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(dp) value is less than given criteria.
        function realDPLessThan(this, value, ubound, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            real(dp), intent(in)                    :: value            !! The value to test
            real(dp), intent(in)                    :: ubound           !! Value must be less than than ubound
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charUbound       ! Character variable to store ubound in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .false.
            ! Check if value is less than criterion
            if (value <= ubound) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('lessThan'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charUbound,*) ubound
                    error%message = "Value must be less than " &
                                    // trim(adjustl(charUbound)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(qp) value is less than given criteria.
        function realQPLessThan(this, value, ubound, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorHandler class
            real(qp), intent(in)                    :: value            !! The value to test
            real(qp), intent(in)                    :: ubound           !! Value must be less than than ubound
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charUbound       ! Character variable to store ubound in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .false.
            ! Check if value is less than criterion
            if (value <= ubound) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('lessThan'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charUbound,*) ubound
                    error%message = "Value must be less than " &
                                    // trim(adjustl(charUbound)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

!-- GREATER THAN --!

        !> Test whether an integer value is greater than given criteria.
        function integerGreaterThan(this, value, lbound, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            integer, intent(in)                     :: value            !! The value to test
            integer, intent(in)                     :: lbound           !! Value must be higher than lbound
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charLbound       ! Character variable to store lbound in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .false.
            ! Check if value is greater than criterion
            if (value >= lbound) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('greaterThan'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charLbound,*) lbound
                    error%message = "Value must be greater than " &
                                    // trim(adjustl(charLbound)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real value is greater than given criteria.
        function realGreaterThan(this, value, lbound, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            real, intent(in)                        :: value            !! The value to test
            real, intent(in)                        :: lbound           !! Value must be higher than lbound
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charLbound       ! Character variable to store lbound in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .false.
            ! Check if value is greater than criterion
            if (value >= lbound) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('greaterThan'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charLbound,*) lbound
                    error%message = "Value must be greater than " &
                                    // trim(adjustl(charLbound)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(dp) value is greater than given criteria.
        function realDPGreaterThan(this, value, lbound, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            real(dp), intent(in)                    :: value            !! The value to test
            real(dp), intent(in)                    :: lbound           !! Value must be higher than lbound
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charLbound       ! Character variable to store lbound in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .false.
            ! Check if value is greater than criterion
            if (value >= lbound) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('greaterThan'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charLbound,*) lbound
                    error%message = "Value must be greater than " &
                                    // trim(adjustl(charLbound)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(qp) value is greater than given criteria.
        function realQPGreaterThan(this, value, lbound, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            real(qp), intent(in)                    :: value            !! The value to test
            real(qp), intent(in)                    :: lbound           !! Value must be higher than lbound
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charLbound       ! Character variable to store lbound in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .false.
            ! Check if value is greater than criterion
            if (value >= lbound) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('greaterThan'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charLbound,*) lbound
                    error%message = "Value must be greater than " &
                                    // trim(adjustl(charLbound)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

!-- NOT EQUAL --!

        !> Test whether an integer is not equal to criterion.
        function integerNotEqual(this, value, criterion, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            integer, intent(in)                     :: value            !! The value to test
            integer, intent(in)                     :: criterion        !! Value should be equal to this criterion
            real, intent(in), optional              :: epsilon          !! Distance from criterion allowed
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charCriterion    ! Character variable to store criterion in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .false.
            ! Check if the value is not equal to criterion
            if (value /= criterion) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('notEqual'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charCriterion,*) criterion
                    error%message = "Value must not be equal to " // trim(adjustl(charCriterion)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real number is not equal to, or further than a specified
        !! amount (epsilon), from criterion.
        function realNotEqual(this, value, criterion, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            real, intent(in)                        :: value            !! The value to test
            real, intent(in)                        :: criterion        !! Value should be equal to this criterion
            real, intent(in), optional              :: epsilon          !! Distance from criterion allowed
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charCriterion    ! Character variable to store criterion in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .true.
            ! Check if the value is outside of criterion +- epsilon
            if (present(epsilon)) then
                if (value <= criterion + epsilon .and. value >= criterion - epsilon) pass = .false.
            else
                if (value <= criterion + this%epsilon .and. value >= criterion - this%epsilon) pass = .false.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('notEqual'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charCriterion,*) criterion
                    error%message = "Value must not be equal to " // trim(adjustl(charCriterion)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(dp) number is not equal to, or further than a specified
        !! amount (epsilon), from criterion.
        function realDPNotEqual(this, value, criterion, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            real(dp), intent(in)                    :: value            !! The value to test
            real(dp), intent(in)                    :: criterion        !! Value should be equal to this criterion
            real, intent(in), optional              :: epsilon          !! Distance from criterion allowed
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charCriterion    ! Character variable to store criterion in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .true.
            ! Check if the value is outside of criterion +- epsilon
            if (present(epsilon)) then
                if (value <= criterion + epsilon .and. value >= criterion - epsilon) pass = .false.
            else
                if (value <= criterion + this%epsilon .and. value >= criterion - this%epsilon) pass = .false.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('notEqual'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charCriterion,*) criterion
                    error%message = "Value must not be equal to " // trim(adjustl(charCriterion)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(qp) number is not equal to, or further than a specified
        !! amount (epsilon), from criterion.
        function realQPNotEqual(this, value, criterion, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            real(qp), intent(in)                    :: value            !! The value to test
            real(qp), intent(in)                    :: criterion        !! Value should be equal to this criterion
            real, intent(in), optional              :: epsilon          !! Distance from criterion allowed
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charCriterion    ! Character variable to store criterion in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .true.
            ! Check if the value is outside of criterion +- epsilon
            if (present(epsilon)) then
                if (value <= criterion + epsilon .and. value >= criterion - epsilon) pass = .false.
            else
                if (value <= criterion + this%epsilon .and. value >= criterion - this%epsilon) pass = .false.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('notEqual'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charCriterion,*) criterion
                    error%message = "Value must not be equal to " // trim(adjustl(charCriterion)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

!-- EQUAL --!

        !> Test whether an integer is equal to criterion.
        function integerEqual(this, value, criterion, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            integer, intent(in)                     :: value            !! The value to test
            integer, intent(in)                     :: criterion        !! Value should be equal to this criterion
            real, intent(in), optional              :: epsilon          !! Distance from criterion allowed
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charCriterion    ! Character variable to store criterion in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            ! Check if the value is inside of criterion +- epsilon.
            pass = .false.
            if (value == criterion) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('equal'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charCriterion,*) criterion
                    error%message = "Value must be equal to " // trim(adjustl(charCriterion)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real number is equal or closer than a specified amount (epsilon) to criterion.
        function realEqual(this, value, criterion, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            real, intent(in)                        :: value            !! The value to test
            real, intent(in)                        :: criterion        !! Value should be equal to this criterion
            real, intent(in), optional              :: epsilon          !! Distance from criterion allowed
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charCriterion    ! Character variable to store criterion in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .true.
            ! Check if the value is inside of criterion +- epsilon.
            if (present(epsilon)) then
                if (value >= criterion + epsilon .or. value <= criterion - epsilon) pass = .false.
            else
                if (value >= criterion + this%epsilon .or. value <= criterion - this%epsilon) pass = .false.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('equal'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charCriterion,*) criterion
                    error%message = "Value must be equal to " // trim(adjustl(charCriterion)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(dp) number is equal or closer than a specified amount (epsilon) to criterion.
        function realDPEqual(this, value, criterion, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            real(dp), intent(in)                    :: value            !! The value to test
            real(dp), intent(in)                    :: criterion        !! Value should be equal to this criterion
            real, intent(in), optional              :: epsilon          !! Distance from criterion allowed
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charCriterion    ! Character variable to store criterion in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .true.
            ! Check if the value is inside of criterion +- epsilon.
            if (present(epsilon)) then
                if (value >= criterion + epsilon .or. value <= criterion - epsilon) pass = .false.
            else
                if (value >= criterion + this%epsilon .or. value <= criterion - this%epsilon) pass = .false.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('equal'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charCriterion,*) criterion
                    error%message = "Value must be equal to " // trim(adjustl(charCriterion)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(qp) number is equal or closer than a specified amount (epsilon) to criterion.
        function realQPEqual(this, value, criterion, epsilon, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            real(qp), intent(in)                    :: value            !! The value to test
            real(qp), intent(in)                    :: criterion        !! Value should be equal to this criterion
            real, intent(in), optional              :: epsilon          !! Distance from criterion allowed
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in
            character(len=100)                      :: charCriterion    ! Character variable to store criterion in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            pass = .true.
            ! Check if the value is inside of criterion +- epsilon.
            if (present(epsilon)) then
                if (value >= criterion + epsilon .or. value <= criterion - epsilon) pass = .false.
            else
                if (value >= criterion + this%epsilon .or. value <= criterion - this%epsilon) pass = .false.
            end if

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error.
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('equal'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    write(charCriterion,*) criterion
                    error%message = "Value must be equal to " // trim(adjustl(charCriterion)) // ". " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

!-- POSITIVE --!

        !> Test whether an integer value is positive.
        function integerPositive(this, value, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            integer, intent(in)                     :: value            !! The value to test
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            ! Check whether value is positive
            pass = .false.
            if (value > 0) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error.
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('positive'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be positive. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real value is positive.
        function realPositive(this, value, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            real, intent(in)                        :: value            !! The value to test
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            ! Check whether value is positive
            pass = .false.
            if (value > 0) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error.
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('positive'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be positive. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(dp) value is positive.
        function realDPPositive(this, value, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            real(dp), intent(in)                    :: value            !! The value to test
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            ! Check whether value is positive
            pass = .false.
            if (value > 0) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error.
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('positive'))
                 write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be positive. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(qp) value is positive.
        function realQPPositive(this, value, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            real(qp), intent(in)                    :: value            !! The value to test
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            ! Check if value is positive
            pass = .false.
            if (value > 0) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error.
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('positive'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be positive. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

!-- NEGATIVE --!

        !> Test whether an integer value is greater than given criteria.
        function integerNegative(this, value, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            integer, intent(in)                     :: value            !! The value to test
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            ! Check if value is negative
            pass = .false.
            if (value < 0) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error.
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('negative')) ! Get the limit error
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be negative. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real value is negative.
        function realNegative(this, value, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            real, intent(in)                        :: value            !! The value to test
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            ! Check if value is negative
            pass = .false.
            if (value < 0) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error.
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('negative'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be negative. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(dp) value is negative criteria.
        function realDPNegative(this, value, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            real(dp), intent(in)                    :: value            !! The value to test
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            ! Check if value is negative
            pass = .false.
            if (value < 0) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error.
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('negative'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be negative. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

        !> Test whether a real(qp) value is negative.
        function realQPNegative(this, value, message, traceMessage) result(error)
            class(ErrorCriteria), intent(in)        :: this             !! The ErrorCriteria class
            real(qp), intent(in)                    :: value            !! The value to test
            character(len=*), intent(in), optional  :: message          !! Overwrite the standard error message
            character(len=*), intent(in), optional  :: traceMessage     !! Message to display for error trace (if any)
            type(ErrorInstance)                     :: error            !! The error to return
            logical                                 :: pass             ! Does the value pass the test?
            character(len=100)                      :: charValue        ! Character variable to store value in

            ! Stop the program running if ErrorHandler not initialised
            call this%stopIfNotInitialised()

            ! Check if value is negative
            pass = .false.
            if (value < 0) pass = .true.

            ! If value doesn't pass test, get the error to return, compose the message
            ! and add specified point to trace. Else, return no error.
            if (.not. pass) then
                error = this%getErrorFromCode(this%getCodeFromCriterionName('negative'))
                write(charValue,*) value
                ! Customise the error message, based on whether user has provided a message
                if (present(message)) then
                    error%message = message &
                                    // " Given value: " // trim(adjustl(charValue)) // "."
                else
                    error%message = "Value must be negative. " &
                                    // "Given value: " // trim(adjustl(charValue)) // "."
                end if
                if (present(traceMessage)) call error%addToTrace(traceMessage)
            else
                error = this%getNoError()
            end if
        end function

end module