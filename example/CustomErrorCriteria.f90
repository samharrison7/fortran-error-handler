module CustomErrorCriteriaModule
    use ErrorCriteriaModule
    use ErrorInstanceModule
    implicit none
    private

    !> Example type that extends the ErrorCriteria to implement
    !! custom error criteria.
    type, public, extends(ErrorCriteria) :: CustomErrorCriteria

      contains
        procedure, public :: init => initCustomErrorCriteria        ! Override ErrorCriteria init
        procedure, public :: factor => integerFactor                ! New criterion
        procedure, public :: multiple => integerMultiple            ! Another new criterion
    end type

  contains

    !> Initialise the CustomErrorCriteria and at the same time initialise
    !! the parent ErrorCriteria (and thus ErrorHandler), setting custom errors
    !! and default error criteria errors.
    subroutine initCustomErrorCriteria(this, &
                                       errors, &
                                       criticalPrefix, &
                                       warningPrefix, &
                                       messageSuffix, &
                                       bashColors, &
                                       printErrorCode, &
                                       triggerWarnings, &
                                       on)
        class(CustomErrorCriteria), intent(inout)   :: this                 !> This ErrorCriteria instance
        type(ErrorInstance), intent(in), optional   :: errors(:)            !> Custom defined errors
        character(len=*), intent(in), optional      :: criticalPrefix       !> Prefix to critical error messages
        character(len=*), intent(in), optional      :: warningPrefix        !> Prefix to warning error messages
        character(len=*), intent(in), optional      :: messageSuffix        !> Suffix to error messages
        logical, intent(in), optional               :: bashColors           !> Should prefixes be colored in bash shells?
        logical, intent(in), optional               :: printErrorCode       !! Should error messages be prefixed with the error code?
        logical, intent(in), optional               :: triggerWarnings      !! Should warnings be printing on trigger?
        logical, intent(in), optional               :: on                   !! Should the ErrorHandler output errors?

        !> We must initialise the parent ErrorCriteria for the default criteria to be set                                                              
        call this%ErrorCriteria%init(errors, criticalPrefix, warningPrefix, messageSuffix, bashColors, &
                                     printErrorCode, triggerWarnings, on)
        ! Add our new error criterion. Make sure you include a function(s) that corresponds to this!
        call this%addErrorCriteria( &
            codes = [110,111], &
            names = [character(len=100) :: 'factor','multiple'], &
            messages = [character(len=100) :: 'Value must be a factor of criterion.','Value must be multiple of criterion.'], &
            areCritical = [.true.,.true.] &
        )

    end subroutine

    !> Test whether an integer value is a factor of criterion
    function integerFactor(this, value, criterion, message, traceMessage) result(error)
        class(CustomErrorCriteria), intent(in)  :: this             !> The ErrorCriteria class
        integer, intent(in)                     :: value            !> The value to test
        integer, intent(in)                     :: criterion            !> The value to test
        character(len=*), intent(in), optional  :: message          !> Overwrite the standard error message
        character(len=*), intent(in), optional  :: traceMessage     !> Message to display for error trace (if any)
        type(ErrorInstance)                     :: error            !> The error to return
        logical                                 :: pass             !> Does the value pass the test?
        character(len=100)                      :: charValue        !> Character variable to store value in
        character(len=100)                      :: charCriterion    !> Character variable to store criterion in

        ! Stop the program running if ErrorHandler not initialised
        call this%stopIfNotInitialised()

        ! Check if value is a factor of criterion
        pass = .false.
        if (mod(criterion,value) == 0) pass = .true.

        ! If value doesn't pass test, get the error to return, compose the message
        ! and add specified point to trace. Else, return no error.
        if (.not. pass) then
            ! Make sure the criterion name matches the one you defined above,
            ! otherwise, no error will be returned.
            error = this%getErrorFromCode(this%getCodeFromCriterionName('factor'))
            write(charValue,*) value            ! Store value as char to be output in message
            write(charCriterion,*) criterion    ! Likewise for criterion
            ! Customise the error message, based on whether user has provided a message
            if (present(message)) then
                error%message = message &
                                // " Given value: " // trim(adjustl(charValue)) // "."
            else
                error%message = "Value must be a factor of " // trim(adjustl(charCriterion)) // ". " &
                                // "Given value: " // trim(adjustl(charValue)) // "."
            end if
            if (present(traceMessage)) call error%addToTrace(traceMessage)
        else
            error = this%getNoError()
        end if
    end function

    !> Test whether an integer value is a multiple of criterion
    function integerMultiple(this, value, criterion, message, traceMessage) result(error)
        class(CustomErrorCriteria), intent(in)  :: this             !> The ErrorCriteria class
        integer, intent(in)                     :: value            !> The value to test
        integer, intent(in)                     :: criterion            !> The value to test
        character(len=*), intent(in), optional  :: message          !> Overwrite the standard error message
        character(len=*), intent(in), optional  :: traceMessage     !> Message to display for error trace (if any)
        type(ErrorInstance)                     :: error            !> The error to return
        logical                                 :: pass             !> Does the value pass the test?
        character(len=100)                      :: charValue        !> Character variable to store value in
        character(len=100)                      :: charCriterion    !> Character variable to store criterion in

        ! Stop the program running if ErrorHandler not initialised
        call this%stopIfNotInitialised()

        ! Check if value is a multiple of criterion
        pass = .false.
        if (mod(value,criterion) == 0) pass = .true.

        ! If value doesn't pass test, get the error to return, compose the message
        ! and add specified point to trace. Else, return no error.
        if (.not. pass) then
            ! Make sure the criterion name matches the one you defined above,
            ! otherwise, no error will be returned.
            error = this%getErrorFromCode(this%getCodeFromCriterionName('multiple'))
            write(charValue,*) value            ! Store value as char to be output in message
            write(charCriterion,*) criterion    ! Likewise for criterion
            ! Customise the error message, based on whether user has provided a message
            if (present(message)) then
                error%message = message &
                                // " Given value: " // trim(adjustl(charValue)) // "."
            else
                error%message = "Value must be a multiple of " // trim(adjustl(charCriterion)) // ". " &
                                // "Given value: " // trim(adjustl(charValue)) // "."
            end if
            if (present(traceMessage)) call error%addToTrace(traceMessage)
        else
            error = this%getNoError()
        end if
    end function

end module