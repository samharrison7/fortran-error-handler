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
call EH%trigger(errors=r%getErrors())
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
call EH%trigger(error=limitError)
```
```bash
ERROR: Value must be between 0 and 1.
```

## Result
The limited nature of polymorphism in Fortran makes providing a Result object with a generic data element a non-trivial task. To enable the `Result()` constructor to work with any type of input data, the data is stored as an unlimited polymorphic object, `class(*)`. This means that the `select type` construct must be used when the data is returned from the result object (using the `getData()` function), otherwise your compiler is likely to complain that you're trying to convert a `class(*)` object to whatever type you're trying to store data in.

For example, if we know that the data is an object of type `TestClass`, then the following approach can be used:

```fortran
type(TestClass) :: tc
type(Result) :: r

call tc%setImportantNumber(1.2345)      ! Give tc some data
r = Result(data=tc)                     ! Store in Result object
select type(data => r%getData())        ! Use select type to retrieve the TestClass object
    type is (TestClass)
        write(*,'(f6.4)') data%getImportantNumber()
end select
```

```sh
1.2345
```

Another approach, to be used with caution, would be to use the intrinsic `transfer` function, which casts a bitwise representation from one type to another; e.g., `class(*)` to `type(TestClass)`. For example:

```fortran
type(TestClass) :: tc
type(Result) :: r

call tc%setImportantNumber(1.2345)      ! Give tc some data
r = Result(data=tc)                     ! Store in Result object
tc = transfer(source=r%getData(), mold=tc)      ! Cast class(*) object to TestClass object
write(*,'(f6.4)') tc%getImportantNumber()
```

```sh
1.2345
```

To make this process slightly less painful, specific result types are provided for intrinsic data types: integer, real, complex, logical. These are extensions of `Result` and the `getData()` function returns a variable of the corresponding data type, not an unlimited polymorphic object. For example:

```fortran
type(IntegerResult) :: ir
type(RealResult) :: rr
type(ComplexResult) :: cr
type(LogicalResult) :: lr

ir = Result(data=1)
rr = Result(data=1.2345)
cr = Result(data=)
lr = Result(data=.false.)

write(*,*) ir%getData(), rr%getData(), cr%getData()
if (lr%getData .eqv. .false.) write(*,*) "True!"
```

Caveats:
Error code must be <99999

Write a bit about kinds as well, reference the "best answer" here: https://software.intel.com/en-us/forums/intel-fortran-compiler-for-linux-and-mac-os-x/topic/611490