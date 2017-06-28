# ErrorHandler

***Work in progress.***

The ErrorHandler is the class responsible for initialising the error handling environment, triggering error events, and storing a list of possible error codes and their respective error messages.

## Initialising
The ErrorHandler must be initialised before use, by calling the `init` procedure. This sets the default "generic error" with code 1, and the default "no error" with code 0. A number of optional parameters can be specified:

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
type(ErrorHandler) :: EH

call EH%init( &
    errors = [ErrorInstance(code=200,message="A custom error message.", isCritical=.false.)], &
    criticalPrefix = "Errortastic:", &
    warningPrefix = "Warningtastic:", &
    messageSuffix = "Have a nice day!", &
    bashColors = .false. &
)
```

## Adding custom errors
As well as specifying custom errors when initialising, custom errors can be added on the fly using the `add` procedure. This can add a single error from a code, message and isCritical parameter, a single error from an ErrorInstance, multiple errors from an arrays of codes, messages and areCritical parameters, or multiple errors from an array of ErrorInstances. `add` is generic, and one of a number of actual procedures will be called, depending on the parameters specified:

#### `ErrorHandler%add(code, message, isCritical, error)`
For single errors, `add` acts as a generic to the ErrorHandler's `addErrorInstance` procedure. All parameters are optional, and if a code is specified, then the error parameter is ignored. Attempting to add a pre-existing error code cause an error.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer, optional :: code` | Error code to add. | - |
| `character(len=*), optional :: message` | Message for the error. | "" |
| `logical, optional :: isCritical` | Is the error critical? | .true. |
| `type(ErrorInstance), optional :: error | ErrorInstance to add, if a code hasn't been specified. | - |

For example:

```fortran
EH%add(code=300, message="Another custom error message.", isCritical=.false.)
EH%add(error=ErrorInstance(code=400, message="A really important error"))
```

#### `ErrorHandler%add(codes, message, areCritical)`
For multiple errors input as an array of codes and optional messages and areCritical parameters, `add` acts as a generic to the ErrorHandler's `addMultipleErrorInstancesFromCodes` procedure. If specified, `messages` and `areCritical` must be of the same size as `codes`, otherwise an error will be thrown.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer :: codes(:)` | Array of error codes to add. | - |
| `character(len=*), optional :: messages(:)` | Corresponding messages for the errors. | "" |
| `logical, optional :: areCritical(:)` | Are the errors critical? | .true. |

For example:

```fortran
EH%add(codes=[201,202], messages=["A","B"], areCritical=[.false.,.true.])
```

#### `ErrorHandler%add(errors)`
Finally, multiple errors can be added as an array of ErrorInstances and in this case, `add` acts as a generic to the ErrorHandler's `addMultipleErrorInstancesFromErrors` procedure:

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `type(ErrorInstance) :: errors(:)` | Array of ErrorInstances to add. | - |

## Triggering errors

#### `ErrorHandler%trigger(code, error, errors)`

Triggering an error causes the error code to be printed to the console, along with any prefixes or suffixes. If the error is critical, program execution will be stopped with a stop code of the error code. If multiple critical errors are specified, they will all be printed to the console and the first error encountered will be used as the stop code. If a code, error and errors are provided, the code, followed by error, will take precedence.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer, optional :: code` | Code of error to trigger. Must be in ErrorHandler's list of errors. | - |
| `type(ErrorInstance), optional :: error` | ErrorInstance to trigger. | - |
| `type(ErrorInstance) :: errors(:)` | Array of ErrorInstances to trigger. | - |

A number of situations can arise, depending on whether the code/error(s) exist in the ErrorHandler's list of errors (i.e., whether it is one of the default errors, or has been previously added by the user):

- `code` provided and it exists: The corresponding error will be got from the ErrorHandler's list and its message/criticality used.
- `code` provided but it doesn't exist: No error will be triggered - i.e., nothing will happen.
- `error` or `errors` provided and it exists:
    - If no `message` exists (i.e., the message is "") in the input error/errors, then the default message from the ErrorHandler's list of errors will be used.
    - If a `message` exists, then this will override the default message.
    - If an `isCritical` exists, then this will override the default criticality from the ErrorHandler's list of errors.
    - When an ErrorInstance is constructured, not specifying `isCritical` means that it defaults to `.true.`. Thus, inputting an error/errors that, when constructed, didn't explicitly specify `isCritical`, means the error will be triggered as critical, regardless of the criticality of the error in the ErrorHandler's list of errors.
- `error` or `errors` provided and they don't exist: They will be triggered anyway, allowing for one-off errors to be triggered on-the-fly, without having to add them to the ErrorHandler. It is up to the user to ensure this is rational for their application, and that confusion isn't caused by using the same error codes for on-the-fly errors at different points in the same application.
- No parameters provided - `ErrorHandler%trigger()`. The default error (code 1) will be triggered.

For example:

```fortran
! Add an error
call EH%add(code=200, message="Custom error message.", isCritical=.false.)
! Trigger some errors
call EH%trigger(code=200)
call EH%trigger(error=ErrorInstance(code=999, message="On-the-fly error.", isCritical=.false.))
call EH%trigger(errors=[ &
    ErrorInstance(code=200, message="Override default message."), &
    ErrorInstance(code=997, message="Another specific error.") &
    ] &
)
```

This results in:
```bash
$ Warning: A custom error message.
$ Warning: On-the-fly error.
$ Warning: Override default message.
$ Error: Another specific error.
$ ERROR STOP 997
```