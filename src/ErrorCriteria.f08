module ErrorCriteriaModule
    use ErrorHandlerModule
    use ErrorInstanceModule
    implicit none
    private

    !> ErrorCrtiera extends ErrorHandler and defines a number of common "criteria" used
    !! for error checking, such as checking whether a number falls between given bounds.
    !! Criteria functions expedite the error checking process with intuitive function calls 
    !! returning pre-defined ErrorInstances.
    type, public, extends(ErrorHandler) :: ErrorCriteria
        private

        ! Current error criteria codes, messages and criticality will be used to keep track of the
        ! codes/messages/criticality that relate to the error criteria in amongst other error codes.
        ! Fortran being Fortran and lacking out-of-the-box key=>value arrays (e.g., dictionaries, hash
        ! tables, lists) makes this seemingly trivial task a bit complex. To save overcomplicating
        ! things and using, e.g., third-party linked lists, we'll specify that each criterion is
        ! aligned with a specific array index, and the codes/message/criticality arrays below are
        ! filed following this specification:
        !      Array index     Function name       Default code
        !      1               nonZero             101
        !      2               lessThan            102
        !      3               greaterThan         103
        !      4               limit               104
        !      5               notEqual            105
        !      6               equal               106
        !      7               positive            107
        !      8               negative            108
        integer :: currentErrorCriteriaCodes(8)
        character(len=256) :: currentErrorCriteriaMessages(8)
        logical :: currentErrorCriteriaIsCritical(8)

        ! Default error criteria codes and messages that are used if none others specified.
        integer :: defaultErrorCriteriaCodes(8)
        character(len=256) :: defaultErrorCriteriaMessages(8)
        logical :: defaultErrorCriteriaIsCritical(8)

        contains
            procedure, public :: init => initErrorCriteria      ! Overload Errorhandler init procedure
            procedure, public :: getCodeFromCriterionName
            procedure, public :: getIndexFromCriterionName

            ! Removing ErrorInstances. removeErrorInstance and removeErrorInstances are
            ! bound to remove generic in parent. Here, we overload then to check that
            ! the errors being removed aren't in the current error criteria.
            procedure, public :: removeErrorInstance => removeErrorInstanceCheckCriteria
            procedure, public :: removeMultipleErrorInstances => removeMultipleErrorInstancesCheckCriteria

            ! Modify error criterion codes
            procedure, public :: modifyErrorCriteriaCodes       
            generic, public :: modifyErrorCriterionCode => modifyErrorCriterionCodeByIndex, modifyErrorCriterionCodeByName
            procedure, public :: modifyErrorCriterionCodeByIndex, modifyErrorCriterionCodeByName

            ! The criteria functions
            generic, public :: limit => integerLimit, realLimit
            procedure, private :: integerLimit, realLimit
    end type

    contains

        subroutine initErrorCriteria(this, &
                        errors, &
                        criticalPrefix, &
                        warningPrefix, &
                        messageSuffix, &
                        bashColors)
            class(ErrorCriteria), intent(inout) :: this
            type(ErrorInstance), intent(in), optional :: errors(:)
            character(len=*), intent(in), optional :: criticalPrefix
            character(len=*), intent(in), optional :: warningPrefix
            character(len=*), intent(in), optional :: messageSuffix
            logical, intent(in), optional :: bashColors

            call this%ErrorHandler%init(errors,criticalPrefix,warningPrefix,messageSuffix,bashColors)

            ! Define the default error criteria
            this%defaultErrorCriteriaCodes = [101,102,103,104,105,106,107,108]
            this%defaultErrorCriteriaMessages = [character(len=256) :: &
                "Value must be non-zero.", &
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
            this%currentErrorCriteriaCodes = this%defaultErrorCriteriaCodes
            this%currentErrorCriteriaMessages = this%defaultErrorCriteriaMessages
            this%currentErrorCriteriaIsCritical = this%defaultErrorCriteriaIsCritical
        end subroutine

        !> Modify the error codes for the error criteria. Fortran being Fortran
        !! and lacking out-of-the-box key=>value arrays (e.g., dictionaries, hash
        !! tables, lists) makes this seemingly trivial task a bit complex. 
        !! To save overcomplicating things and defining our own derived types
        !! (or using third-party linked lists, etc.), we'll hard code each
        !! criterion as being aligned with a specific array index, and the integer value
        !! at that index is the error code for that particular criterion:
        !!      Array index     Function name       Default code
        !!      1               nonZero             101
        !!      2               lessThan            102
        !!      3               greaterThan         103
        !!      4               limit               104
        !!      5               notEqual            105
        !!      6               equal               106
        !!      7               positive            107
        !!      8               negative            108
        subroutine modifyErrorCriteriaCodes(this, codes)
            class(ErrorCriteria)    :: this
            integer, intent(in), optional       :: codes(size(this%currentErrorCriteriaCodes))
            integer                             :: i

            ! Stop if we haven't initialised the error handler
            call this%stopIfNotInitialised

            ! Remove the old codes (use the ErrorHandler's method to avoid throwing
            ! error that the error code is an error criteria one).
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
        !! index corresponding to the functions as such:
        !!      Array index     Function name       Default code
        !!      1               nonZero             101
        !!      2               lessThan            102
        !!      3               greaterThan         103
        !!      4               limit               104
        !!      5               notEqual            105
        !!      6               equal               106
        !!      7               positive            107
        !!      8               negative            108
        !! TODO: Test this
        subroutine modifyErrorCriterionCodeByIndex(this, index, newCode)
            class(ErrorCriteria) :: this
            integer, intent(in) :: index
            integer, intent(in) :: newCode
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
        !! index corresponding to the functions as such:
        !!      Array index     Function name       Default code
        !!      1               nonZero             101
        !!      2               lessThan            102
        !!      3               greaterThan         103
        !!      4               limit               104
        !!      5               notEqual            105
        !!      6               equal               106
        !!      7               positive            107
        !!      8               negative            108
        subroutine modifyErrorCriterionCodeByName(this, name, newCode)
            class(ErrorCriteria) :: this
            character(len=*), intent(in) :: name
            integer, intent(in) :: newCode
            integer :: index
            ! Get the index of the named error criterion in the error criteria array,
            ! then use the modifyErrorCriterionCodeByIndex method to modify
            index = this%getIndexFromCriterionName(name)
            call this%modifyErrorCriterionCodeByIndex(index, newCode)
        end subroutine

        function getCodeFromCriterionName(this, name) result(code)
            class(ErrorCriteria) :: this
            character(len=*) :: name
            integer :: code
            integer :: index
            ! Get the index from the name, and use that to get the code
            index = this%getIndexFromCriterionName(name)
            if (lbound(this%currentErrorCriteriaCodes,1) <= index .and. ubound(this%currentErrorCriteriaCodes,1) >= index) then
                code = this%currentErrorCriteriaCodes(index)
            else
                code = 0
            end if
        end function

        function getIndexFromCriterionName(this, name) result(index)
            class(ErrorCriteria) :: this
            character(len=*) :: name
            integer :: index

            select case (name)
                case ("nonZero")
                    index = 1
                case ("lessThan")
                    index = 2
                case ("greaterThan")
                    index = 3
                case ("limit")
                    index = 4
                case ("notEqual")
                    index = 5
                case ("equal")
                    index = 6
                case ("positive")
                    index = 7
                case ("negative")
                    index = 8
                case default
                    index = 0
            end select
        end function

        subroutine removeErrorInstanceCheckCriteria(this, code)
            class(ErrorCriteria) :: this
            integer, intent(in) :: code
            type(ErrorInstance) :: error
            integer :: i
            logical :: mask(size(this%getErrors()))
            
            ! Firstly, check if the code specified is an error criteria
            ! code, and if so, throw an error.
            do i=1, size(this%currentErrorCriteriaCodes)        ! Loop through current error criteria codes
                if (this%currentErrorCriteriaCodes(i) == code) then
                    write(*,'(a,i5,a)') "ERROR: Trying to remove error code that is used in error criteria: ", code, "."
                    write(*,'(a)') "Use modifyErrorCriterionCode() instead."
                    error stop 1
                end if
            end do

            ! If error isn't an error criteria, then remove using normal method
            call this%ErrorHandler%remove(code)
        end subroutine

        subroutine removeMultipleErrorInstancesCheckCriteria(this, codes)
            class(ErrorCriteria) :: this
            integer, intent(in) :: codes(:)
            integer :: i

            ! Loop through the codes and call removeErrorInstance
            do i=1, size(codes)
                call this%removeErrorInstance(codes(i))
            end do
        end subroutine

        !> Test whether an integer value falls within a limit. If only upper
        !! or lower bounds are specified, the value must be less than
        !! or greater than the limit (respetively).
        pure function integerLimit(this, value, lbound, ubound, message) result(error)
            class(ErrorCriteria), intent(in)         :: this             !> The ErrorHandler class
            integer, intent(in)                     :: value            !> The value to test
            integer, intent(in), optional           :: lbound           !> The lower bound of the limit
            integer, intent(in), optional           :: ubound           !> The upper bound of the limit
            character(len=*), intent(in), optional  :: message          !> Overwrite the standard error message
            type(ErrorInstance)                        :: error            !> The error to return
            logical                                 :: pass             !> Does the value pass the test?

            ! Stop the program running is ErrorHandler not initialised
            call this%ErrorHandler%stopIfNotInitialised()

            if (present(lbound) .and. present(ubound)) then
                if (value >= lbound .and. value <= ubound) pass = .true.
            else if (present(lbound)) then
                if (value >= lbound) pass = .true.
            else if (present(ubound)) then
                if (value <= ubound) pass = .true.
            else
                pass = .false.
            end if

            if (.not. pass) then
                error = this%ErrorHandler%getErrorFromCode(102)              ! Get the limit error
                if (present(message)) error%message = message   ! Customise the message
                call error%addPointToTrace('ErrorHandler%limit')
            else
                error = this%ErrorHandler%getErrorFromCode(0)
            end if
        end function

        !> Test whether a real value falls within a limit. If only upper
        !! or lower bounds are specified, the value must be less than
        !! or greater than the limit (respetively).
        pure function realLimit(this, value, lbound, ubound, message) result(error)
            class(ErrorCriteria), intent(in)         :: this             !> The ErrorHandler class
            real, intent(in)                    :: value            !> The value to test
            real, intent(in), optional          :: lbound           !> The lower bound of the limit
            real, intent(in), optional          :: ubound           !> The upper bound of the limit
            character(len=*), intent(in), optional  :: message          !> Overwrite the standard error message
            type(ErrorInstance)                         :: error            !> The error to return
            logical                                 :: pass             !> Does the value pass the test?

            ! Stop the program running is ErrorHandler not initialised
            call this%ErrorHandler%stopIfNotInitialised()

            if (present(lbound) .and. present(ubound)) then
                if (value >= lbound .and. value <= ubound) pass = .true.
            else if (present(lbound)) then
                if (value >= lbound) pass = .true.
            else if (present(ubound)) then
                if (value <= ubound) pass = .true.
            else
                pass = .false.
            end if

            if (.not. pass) then
                error = this%ErrorHandler%getErrorFromCode(102)              ! Get the limit error
                if (present(message)) error%message = message   ! Customise the message
                call error%addPointToTrace('ErrorHandler%limit')
            else
                error = this%ErrorHandler%getErrorFromCode(0)
            end if
        end function

end module