# Fortran Error Handler

Fortran error handling frameworks are few and far between, and those that do exist often implement only parts of the error handling process, or rely on pre-processors. The goal of this error handling framework is to provide a universal and comprehensive solution for applications requiring functional and robust error handling, utilising the power of object-oriented Fortran.

The framework consists of two main classes:

### ErrorHandler
Responsible for initiating the error handling environment, triggering error events, and storing a list of possible error codes and their respetive error messages.

```fortran
type(ErrorHandler) :: EH

call EH%init()
call EH%add( &
    code = 200, &
    message = "A custom error message.", &
    isCritical = .false. &
)
call EH%trigger(200)
```

Which outputs:

```bash
WARNING: A custom error message.
```

### ErrorInstance
An ErrorInstance is an object representing an error, containing an error code, error message, whether the error is critical (should stop the program executing), and a user-defined trace of where the error has come from.

```fortran
type(ErrorHandler) :: EH
type(ErrorInstance) :: errors(2)

errors(1) = ErrorInstance(200, "A custom error message.", .false.)
errors(2) = ErrorInstance(300, "Another custom error message.", .true.)
call EH%init(errors=errors)
call EH%trigger(300)
```
```bash
ERROR: Another custom error message.
```

Two further classes provide added functionality:

### Result
A Result object, though not required to use the framework, is designed as an object to be return from any procedures that may throw an error. It consists of data (i.e., what the function should return if there aren't any errors) and an ErrorInstance.

```fortran
type(ErrorHandler) :: EH
type(ErrorInstance) :: procError
type(Result) :: r

procError = ErrorInstance(100, "Non-critical error that a procedure throws.", .false.)
call EH%init(errors=[procError])

r = Result( &
    data = 'Here is some data', &
    error = procError
)
EH%trigger(errors=r%getErrors())
```
```bash
WARNING: Non-critical error that a procedure throws.
```

### ErrorCriteria
This class extends the ErrorHandler and defines a number of common "criteria" used for error checking, such as checking whether a number falls between given bounds. Criteria functions expedite the error checking process with intuitive function calls returning pre-defined ErrorInstances.

```fortran
type(ErrorCriteria) :: EH
type(ErrorInstance) :: limitError

call EH%init()          ! This sets some default error criteria messages
limitError = EH%limit( &
    value = 2,
    lbound = 0,
    ubound = 1,
    message = "Value must be between 0 and 1."
)
EH%trigger(error=limitError)
```
```bash
ERROR: Value must be between 0 and 1.
```


Caveats:
Error code must be <9999