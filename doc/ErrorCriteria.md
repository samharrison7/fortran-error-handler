# ErrorCriteria

This class extends the ErrorHandler and defines a number of common "criteria" used for error checking, such as checking whether a number falls between given bounds. Criteria functions expedite the error checking process with intuitive function calls returning pre-defined ErrorInstances. It is an *optional extension* and the [ErrorHandler](ErrorHandler.md) can be used without it.

- [Initialising](#initialising)
- [Criteria functions](#criteria)
- [Modifying default criteria errors](#modifying)
- [Getters](#getters)
- [Removing errors](#removing)
- [Extending the error criteria](#extending)

<a name="type-kind"></a>
### Type and kind conventions
**TL;DR:** Only `integer`, `real`, `real(dp)` and `real(qp)` can be tested using the criteria, where `dp` (double precision) and `qp` (quadruple precision) are defined as:

```fortran
integer, parameter :: dp = selected_real_kind(15,307)
integer, parameter :: qp = selected_real_kind(33,4931) 
```

**More detailed explanation:** The lack of dynamic (run-time) polymorphism in Fortran means that the compiler must know the type of a variable before operating on it. This means that generic procedure names, such as the criterion names used by the ErrorCriteria (e.g., `limit`, `postive`; see below) have to be bound to a separate procedure for each variable type *and* kind (an alternative approch would have been to use the `select type` construct in one large procedure for each criterion, but the implication remains the same). Currently, procedures for those types and kind listed above are implemented, and the use of `selected_real_kind` to define `dp` and `qp` was chosen as "best practice" in texts such as N. S. Clerman and W. Spector's book "Modern Fortran" (2012). If there is demand in the future, other types and kinds might be added (e.g., support for 8-byte integers or complex numbers).

<a name="Initialising"></a>
## Initialising

Initialising ErrorCriteria serves the purpose of [initialising its parent](ErrorHandler.md#initialising), ErrorHandler, and then adding further default errors, each one corresponding to a particular criterion test (these can be [modified](#modifying) later). The `init` procedure takes the same parameters as ErrorHandler's `init` procedure and thus can be used in exactly the same way:

#### `ErrorHandler%init(errors, criticalPrefix, warningPrefix, messageSuffix, bashColors)`

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `type(ErrorInstance), optional :: errors(:)` | Custom errors to add to the default errors. | - |
| `character(len=*), optional :: criticalPrefix` | Custom prefix to be prepended to critical error messages. | "Error:" |
| `character(len=*), optional :: warningPrefix` | Custom prefix to non-critical (warning) error messages. | "Warning:" |
| `character(len=*), optional :: messageSuffix` | Custom suffix to be appended to error message. | - |
| `logical, optional :: bashColors` | Should colors be displayed in output to bash consoles? | .true. |

For example:

```fortran
type(ErrorCriteria) :: EH

call EH%init( &
    errors = [ErrorInstance(code=200,message="A custom error message.", isCritical=.false.)], &
    criticalPrefix = "Errortastic:", &
    warningPrefix = "Warningtastic:", &
    messageSuffix = "Have a nice day!", &
    bashColors = .false. &
)
```
Using the convention of naming the instantiated ErrorCriteria as something related to handling errors (e.g., "EH") is encouraged, as the ErrorCriteria deals not just with handling error criteria, but also everything that the ErrorHandler is responsible for too.

<a name="criteria"></a>
## Criteria functions

The following criteria functions are available for use. Each one returns an ErrorInstance, and if the value provided passes the criterion, then the ErrorInstance returned is the default "no error" error (code 0), and thus [triggering](ErrorHandler.md#triggering) it won't cause anything to happen. Each of the criteria has its own error code, the default (applied by the `init` procedure) for which is shown below. As per the above, the numeric parameters `value`, `lbound`, `ubound` and `criterion` can be `integer`, `real`, `real(dp)` or `real(qp)`, whilst `epsilon` *must* be `real`.

**`epsilon`** is a tolerance to account for imprecission in floating point numbers, and is an optional parameter to criteria functions, which defaults to `1.0e-5`. When testing if a value is equal to another number, the criterion function will return true as long as the value falls with the criterion +- epsilon. It has no effect when working with integers.

**`message`** is an optional `character` string that overrides the criterion's default message (which is a generic message such as "Value must be less than [ubound]"). "Given value: [value]." will always be appended to the message in the ErrorInstance returned by the criterion function. A **`traceMessage`** (optional `character` string) can also be applied if the user wishes to add a trace point as this stage (see the [ErrorInstance](ErrorInstance.md#trace) docs).

The array index column below is the internal array index that represents the given criterion, and is used externally when [modifying](#modifying) the default criteria codes.

| Function | Description | Default code | Array index |
| :--- | :--- | :--- | :--- |
| `ErrorCriteria%nonZero(value, epsilon, message, traceMessage)` | Check a value is non-zero, or further than +- epsilon from zero. | 101 | 1 |
| `ErrorCriteria%zero(value, epsilon, message, traceMessage)` | Check a value is zero, or within +- epsilon of zero. | 102 | 2 |
| `ErrorCriteria%lessThan(value, ubound, message, traceMessage)` | Check a value is less than ubound. | 103 | 3 |
| `ErrorCriteria%greaterThan(value, lbound, message, traceMessage)` | Check a value is greater than lbound. | 104 | 4 |
| `ErrorCriteria%limit(value, lbound, ubound, message, traceMessage)` | Check a value is between lbound and ubound. If only lbound or ubound specified, value is testing to be greater than or less than, respectively. | 105 | 5 |
| `ErrorCriteria%notEqual(value, criterion, epsilon, message, traceMessage)` | Check a value is not equal to criterion, or further than +- epsilon from criterion. | 106 | 6 |
| `ErrorCriteria%equal(value, criterion, epsilon, message, traceMessage)` | Check a value is equal to criterion, or within +- epsilon of criterion. | 107 | 7 |
| `ErrorCriteria%positive(value, message, traceMessage)` | Check a value is positive. | 108 | 8 |
| `ErrorCriteria%negative(value, message, traceMessage)` | Check a value is negative. | 109 | 9 |

For example:

```fortran
type(ErrorInstance) :: error

error = EH%limit(3,0,2)
call EH%trigger(error=error)
```

This will result in:

```bash
$ Error: Value must be between 0 and 2. Given value: 3.
$ ERROR STOP 105
```

<a name="modifying"></a>
## Modifying default criteria errors

A number of procedures exist to edit the default error criteria.

#### `ErrorCriteria%modifyErrorCriteriaCodes(codes)`

An array of new error codes can be provided, with the array index that the criterion relates to given in the table above (e.g., the first value in the array is the code for the `nonZero` criterion). The array must match the number of error criteria functions (currently, nine), otherwise an error is thrown.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer :: codes(:)` | Array of new error codes. Must match number of error criteria functions. | - |

#### `ErrorCriteria%modifyErrorCriterionCode(index, newCode)`

Modify an individual error criterion code by its array index (see above). An error is thrown if the index doesn't match a criteria function's index.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer :: index` | Array index that represents the given error criterion to modify. | - |
| `integer :: newCode` | The new error code. | - |

#### `ErrorCriteria%modifyErrorCriterionCode(name, newCode)`

Modify an individual error criterion code by the criterion function name (e.g., notEqual, limit, etc.). An error is thrown if the name doesn't match a criteria function.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `character(len=*) :: name` | Name of the error criterion function to modify. | - |
| `integer :: newCode` | The new error code. | - |

<a name="getters"></a>
## Getters

A couple of procedures exist to get return an error criterion's code or index from the function name.

#### `ErrorCriteria%getCodeFromCriterionName(name)`

Returns an `integer` error code.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `character(len=*) :: name` | Name of the error criterion function to get the code for. | - |

#### `ErrorCriteria%getIndexFromCriterionName(name)`

Returns an `integer` array index.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `character(len=*) :: name` | Name of the error criterion function to get the array index for. | - |

<a name="removing"></a>
## Removing errors

On the surface, removing errors using the `remove` generic is exactly the same as for the ErrorHandler. It is worth noting that internally, the ErrorCriteria checks that the error to be removed isn't an error for a criterion. If it is, an error message is thrown. To remove an error that is used for a criterion, the criterion's error code must first be changed ([see above](#modifying)).

<a name="extending"></a>
## Extending the error criteria

Of course, the ErrorCriteria provided here only includes checks for a few of the most common criteria, and most users will have their own application-specific checks to perform. Whilst one could simply edit the ErrorCriteria.f08 file to add further criteria functions, this is **strongly discouraged**. Doing so would mean pulling in future updates to the Fortran Error Handler would conflict with (i.e., override) your custom criteria.

Instead, you should create your own class that extends the ErrorCriteria and instantiate that class as your error handler. Two self-explanatory procedures, `ErrorCriteria%addErrorCriterion(code, name, message, isCritical)` and `ErrorCriteria%addErrorCriteria(codes, names, messages, areCritical)` can be used in the `init` procedure to add your custom criteria to the current list or criteria and the overall list of errors. This is best demonstrated by an example class, with two new custom criteria, testing whether an integer is a factor of our a multiple of another integer:

```fortran
module CustomErrorCriteriaModule
    use ErrorCriteriaModule
    use ErrorInstanceModule
    implicit none
    private

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
                                        bashColors)
        class(CustomErrorCriteria), intent(inout)   :: this                 !> This ErrorCriteria instance
        type(ErrorInstance), intent(in), optional   :: errors(:)            !> Custom defined errors
        character(len=*), intent(in), optional      :: criticalPrefix       !> Prefix to critical error messages
        character(len=*), intent(in), optional      :: warningPrefix        !> Prefix to warning error messages
        character(len=*), intent(in), optional      :: messageSuffix        !> Suffix to error messages
        logical, intent(in), optional               :: bashColors           !> Should prefixes be colored in bash shells?

        !> We must initialise the parent ErrorCriteria for the default criteria to be set                                                              
        call this%ErrorCriteria%init(errors,criticalPrefix,warningPrefix,messageSuffix,bashColors)
        ! Add our new error criterion. Make sure you include a function(s) that corresponds to this!
        ! If only one criterion is to be added, you can use this%addErrorCriterion(code,name,message,isCritical).
        call this%addErrorCriteria( &
            codes = [110,111], &
            names = [character(len=100) :: 'factor','multiple'], &
            messages = [character(len=100) :: 'Value must be a factor of criterion.','Value must be multiple of criterion.'], &
            areCritical = [.true.,.true.] &
        )

    end subroutine

    !> Test whether an integer value is a factor of criterion
    pure function integerFactor(this, value, criterion, message, traceMessage) result(error)
        class(CustomErrorCriteria), intent(in)  :: this             !> The ErrorCriteria class
        integer, intent(in)                     :: value            !> The value to test
        integer, intent(in)                     :: criterion        !> The value to test
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
    pure function integerMultiple(this, value, criterion, message, traceMessage) result(error)
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
```

These criteria can then be used as such:

```fortran
use CustomErrorCriteriaModule

type(CustomErrorCriteria) :: EH

call EH%init()
! Check whether 3 is a multiple of 2, and whether 3 is a factor of eight
call EH%trigger( &
        errors = [ &
            EH%multiple(3,2), &
            EH%factor(3,8) &
        ] &
    )
```

This results in:

```bash
$ Error: Value must be a multiple of 2. Given value: 3.
$ Error: Value must be a factor of 8. Given value: 3.
$ ERROR STOP 111
```

Check out the [ErrorCriteria.f08](/src/ErrorCriteria.f08) file for further ideas on how to implement custom criteria.