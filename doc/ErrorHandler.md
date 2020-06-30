# ErrorHandler

The ErrorHandler is the class responsible for initialising the error handling environment, queuing and triggering error events, and storing a list of possible error codes and their respective error messages.

- [Initialising](#initialising)
- [Adding custom errors](#adding)
- [Queueing errors](#queueing)
- [Triggering errors](#triggering)
- [Modifying errors](#modifying)
- [Removing errors](#removing)
- [Getters, setters and tests](#getters)

<a name="initialising"></a>
## Initialising
The ErrorHandler must be initialised before use, by calling the `init` procedure. This sets the default "generic error" with code 1, and the default "no error" with code 0. A number of optional parameters can be specified:

#### `ErrorHandler%init(errors, criticalPrefix, warningPrefix, messageSuffix, bashColors)`

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `type(ErrorInstance), optional :: errors(:)` | Custom errors to add to the default errors. | - |
| `character(len=*), optional :: criticalPrefix` | Custom prefix to be prepended to critical error messages. | "Error:" |
| `character(len=*), optional :: warningPrefix` | Custom prefix to non-critical (warning) error messages. | "Warning:" |
| `character(len=*), optional :: messageSuffix` | Custom suffix to be appended to error message. | - |
| `logical, optional :: bashColors` | Should colors be displayed in output to bash consoles? | `.true.` |
| `logical, optional :: printErrorCode` | Should the error code be prepended to the prefix? | `.false.` |
| `logical, optional :: triggerWarnings` | Should warnings be printed when the error queue is triggered? | `.true.` |
| `logical, optional :: on` | Should the `trigger` procedure actually trigger errors? | `.true.` |

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

<a name="adding"></a>
## Adding custom errors
As well as specifying custom errors when initialising, custom errors can be added on the fly using the `add` procedure. This can add a single error from a code, message and isCritical parameter, a single error from an ErrorInstance, multiple errors from an arrays of codes, messages and areCritical parameters, or multiple errors from an array of ErrorInstances. `add` is generic, and one of a number of actual procedures will be called, depending on the parameters specified:

#### `ErrorHandler%add(code, message, isCritical, error)`
For single errors, `add` acts as a generic to the ErrorHandler's `addErrorInstance` procedure. All parameters are optional, and if a code is specified, then the error parameter is ignored. Attempting to add a pre-existing error code causes an error.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer, optional :: code` | Error code to add. | - |
| `character(len=*), optional :: message` | Message for the error. | "" |
| `logical, optional :: isCritical` | Is the error critical? | .true. |
| `type(ErrorInstance), optional :: error` | ErrorInstance to add, if a code hasn't been specified. | - |

For example:

```fortran
call EH%add(code=300, message="Another custom error message.", isCritical=.false.)
call EH%add(error=ErrorInstance(code=400, message="A really important error"))
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
call EH%add(codes=[201,202], messages=["A","B"], areCritical=[.false.,.true.])
```

#### `ErrorHandler%add(errors)`
Finally, multiple errors can be added as an array of ErrorInstances and in this case, `add` acts as a generic to the ErrorHandler's `addMultipleErrorInstancesFromErrors` procedure:

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `type(ErrorInstance) :: errors(:)` | Array of ErrorInstances to add. | - |

<a name="queueing"></a>
## Queueing errors

The ErrorHandler instance contains an array of queued errors that are triggered when the `trigger()` procedure is called (see [below](#triggering)). This is useful for large scripts that might contain multiple errors.

#### `ErrorHandler%queue(code, message, isCritical, trace, error, errors)`

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer, optional :: code` | Code of error to trigger. Must be in ErrorHandler's list of errors. | - |
| `type(ErrorInstance), optional :: error` | ErrorInstance to queue. | - |
| `type(ErrorInstance) :: errors(:)` | Array of ErrorInstances to queue. | - |

A number of situations can arise, depending on whether the code/error(s) exist in the ErrorHandler's list of errors (i.e., whether it is one of the default errors, or has been previously added by the user). These follow the same convention as for [triggering errors](#triggering).

For example:

```fortran
! Add an error
call EH%add(code=200, message="Custom error message.", isCritical=.false.)
call EH%add(code=300, message="Another custom error message.", isCritical=.true.)
! Queue some errors and trigger
call EH%queue(200)
call EH%queue(error=ErrorInstance(code=999, message="On-the-fly queued error."))
call EH%trigger(300)
```

This results in:
```bash
Warning: A custom error message.
Error: On-the-fly queued error.
Error: Another custom error message.
ERROR STOP 999
```

<a name="triggering"></a>
## Triggering errors

#### `ErrorHandler%trigger(code, error, errors)`

Triggering an error causes the error code to be printed to the console, along with any prefixes or suffixes. If the error is critical, program execution will be stopped with a stop code of the error code. If multiple critical ErrorInstances are provided in the `errors` array parameter, they will all be printed to the console and the first error encountered will be used as the stop code to stop the execution of the program. If a code, error and errors are provided, the code, followed by error and then errors, will take precedence. Any [queued errors](#queueing) will be output before the triggered error.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer, optional :: code` | Code of error to trigger. Must be in ErrorHandler's list of errors. | - |
| `type(ErrorInstance), optional :: error` | ErrorInstance to trigger. Specify `isCritical` when constructing ErrorInstance to avoid it defaulting to `.true.`; see *note* below. | - |
| `type(ErrorInstance) :: errors(:)` | Array of ErrorInstances to trigger. Same applies as above regarding criticality. | - |

A number of situations can arise, depending on whether the code/error(s) exist in the ErrorHandler's list of errors (i.e., whether it is one of the default errors, or has been previously added by the user):

- `code` provided and it exists: The corresponding error will be taken from the ErrorHandler's list and its message/criticality used.
- `code` provided but it doesn't exist: No error will be triggered - i.e., nothing will happen.
- `error` or `errors` provided and it/they exist:
    - If no `message` exists (i.e., the message is "") in the input error/errors, then the default message from the ErrorHandler's list of errors will be used.
    - If a `message` exists, then this will override the default message.
    - If an `isCritical` exists, then this will override the default criticality from the ErrorHandler's list of errors.
    - *Note*: When an ErrorInstance is constructed, not specifying `isCritical` means that it defaults to `.true.`. Thus, inputting an error/errors that, when constructed, didn't explicitly specify `isCritical`, means the error will be triggered as critical, regardless of the criticality of the error in the ErrorHandler's list of errors. This might seem slightly unexpected, so be careful!
- `error` or `errors` provided and they don't exist: They will be triggered anyway, allowing for one-off errors to be triggered on-the-fly (at arbitrary points in the code), without having to add them to the ErrorHandler. It is up to the user to ensure this is rational for their application, and that confusion isn't caused by using the same error codes for on-the-fly errors at different points in the same application.
- No parameters provided - `ErrorHandler%trigger()`. Only the queue will be triggered, or if the queue is empty, the default error (code 1) will be triggered.

For example:
```fortran
! Add an error
call EH%add(code=200, message="Custom error message.", isCritical=.false.)
! Trigger some errors
call EH%trigger(code=200)
call EH%trigger(error=ErrorInstance(code=999, message="On-the-fly error.", isCritical=.false.))
call EH%trigger(errors=[ &
    ! Note that isCritical=.false. is required to avoid ErrorInstance defaulting to isCritical=.true. (see above)
    ErrorInstance(code=200, message="Override default message.", isCritical=.false.), &      !
    ErrorInstance(code=997, message="Another specific error.") &
    ] &
)
```

This results in:
```bash
$ Warning: Custom error message.
$ Warning: On-the-fly error.
$ Warning: Override default message.
$ Error: Another specific error.
$ ERROR STOP 997
```

### Turning error triggering off
If you wish to turn off error triggering for testing purposes (not recommended in general), then set up the `ErrorHandler` object via its `init` method with the parameter `on=.false.`. E.g., `ErrorHandler%init(... on=.false.)`. `on` is set to `.true.` by default.

<a name="modifying"></a>
## Modifying errors

The message, criticality and [trace](docs/ErrorInstance.md#traces) of errors that have already been added to the ErrorHandler instance can be modified using the `modify` procedure. The error code itself cannot be modified; to achieve this, [remove](#removing) and re-[add](#adding) the error.

#### `ErrorHandler%modify(code, message, isCritical, trace)`

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer :: code` | Code of the error to modify. Error is thrown if code doesn't exist already. | - |
| `character(len=*), optional :: message` | New error message. | - |
| `logical, optional :: isCritical` | New criticality for the error. | - |
| `character(len=*), optional :: trace(:)` | New trace for the error. | - |

For example:

```fortran
call EH%add(code=400, message="Old message.", isCritical=.false.)
call EH%trigger(400)
call EH%modify(code=400, message="New message.", isCritical=.true.)
call EH%trigger(400)
```

This results in:
```bash
$ Warning: Old message.
$ Error: New message.
$ ERROR STOP 400
```



<a name="removing"></a>
## Removing errors

The `remove` binding acts as a generic to two ErrorHandler procedures, `removeErrorInstance` and `removeMultipleErrorInstances`, which take an integer error code or array of integer error codes, respectively.

If the error code specified doesn't exist, then nothing happens. If the error code is 0 and 1, then an error is thrown as these two codes are the reserved codes for the default "no error" (code 0) and the generic error (code 1).

#### `ErrorHandler%remove(code)`

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer :: code` | Code of the error to remove. | - |

#### `ErrorHandler%remove(codes)`

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer :: codes(:)` | Array of error codes to remove. | - |

For example:

```fortran
call EH%remove(400)
```

<a name="getters"></a>
## Getters, setters and tests

A number of procedures exist to get and set errors, as well as testing whether an error exists in the list of errors.

#### `ErrorHandler%getError(key)`

Return an error at a specific array index location. Probably not that useful, *might be depracated* in the future. Currently no check whether the given key is valid (within the array bounds), so use with caution. Returns `type(ErrorInstance)`.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer :: key` | Array index of the error to remove. | - |

#### `ErrorHandler%getErrors()`

Return all of the defined errors (i.e., the `this%errors` property). Returns a `type(ErrorInstance)` array.

#### `ErrorHandler%getNoError()`

Return the default "no error" with code 0. Returns `type(ErrorInstance)`.

#### `ErrorHandler%getErrorFromCode(code)`

Return array with the given error code. If the error isn't found, the default "no error" is returned. Returns `type(ErrorInstance)`.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer :: code` | Code of the error. | - |

#### `ErrorHandler%setErrors(errors)`

Sets the list of errors (`this%errors`) from the given input. Any existing errors will be overridden. Probably not that useful, *might be depracated* in the future. 

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `type(ErrorInstance) :: errors(:)` | Array of new errors. | - |

#### `ErrorHandler%errorExists(code)`

Check if a given error code exists in the current list of errors, and returns a `logical` true or false.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer :: code` | The error code to test. | - |

#### `ErrorHandler%printErrors()`

Print a list of the defined errors and their messages. Useful for testing purposes.