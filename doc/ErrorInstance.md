# ErrorInstance

***Work in progress.***

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
Error: Another custom error message.
```