# Fortran Error Handler

Fortran error handling frameworks are few and far between, and those that do exist often implement only parts of the error handling process, or rely on pre-processors. The goal of this error handling framework is to provide a universal and comprehensive solution for applications requiring functional and robust error handling, utilising the power of modern object-oriented Fortran.

- [Installation](#installation)
- [Usage](#usage)
    - [A quick example](#usage-example)
- [Structure](#structure)
    - [ErrorHandler](#structure-errorhandler)
    - [ErrorInstance](#structure-errorinstance)
    - [Result](#structure-result)
    - [ErrorCriteria](#structure-errorcriteria)
- [Learn more](#more)

## Installation <a name="installation"></a>

Simply download the source and compile. An example Makefile.example is included, which can be altered according to your compiler and preferences. The framework has only been tested using GFortran 6.3.0. Note that a few bugs in GFortran mean that `-O1` or higher and `-fcheck-no-bounds` must be used.

## Usage <a name="usage"></a>

Read the below documentation for example usage, and check out the [example](example/) directory for ideas of how to incorporate into your project.

### A quick example <a name="usage-example"></a>

```fortran
use ErrorCriteriaModule
use ErrorInstanceModule
use ResultModule

type(ErrorCriteria) :: EH
integer :: i
type(Result0D) :: r

! Initialise the ErrorHandler with two custom errors. Default error (code 1) and no error (code 0)
! will also be created, alongside default errors for criteria (see ErrorCriteria docs)
call EH%init( &
    errors = [ &
        ErrorInstance(code=200, message="A custom error message.", isCritical=.false.), &
        ErrorInstance(code=200, message="Another custom error message.", isCritical=.true.) &
    ] &
)

! Get the user to enter an integer
write(*,"(a)") "Enter an integer between 0 and 2."
read(*,*) i

! Construct a Result object, where the error argument is the result of the ErrorCriteria limit
! function, which returns an error if the criterion test is failed, or a "no error" (code 0) if the
! value passes the test.
r = Result( &
    data = i, &
    error = EH%limit(i,0,2) &
)
! Use the ErrorHandler to trigger the error. Nothing will happen if the value passed the test.
! If the value fails the test, the program will be stopped and an error message displayed.
call EH%trigger(error=r%getError())
! Print the integer. We'll only get this far if the value passed the test.
write(*,"(a,i1)") "Input value is: ", .integer. r
```

If we enter 1 when asked:
```bash
$ Enter an integer between 0 and 2.
$ 1
$ Input value is: 2
```
If we enter 3 when asked:
```bash
$ Enter an integer between 0 and 2.
$ 3
$ Error: Value must be between 0 and 2. Given value: 3.
$ ERROR STOP 105
```

The stop code 105 is the default error code for the limit criterion. This can be changed to whatever you like.

## Structure <a name="structure"></a>

The framework consists of two main classes:

### ErrorHandler <a name="structure-errorhandler"></a>
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

### ErrorInstance <a name="structure-errorinstance"></a>
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

A number of further classes provide added functionality:

### Result <a name="structure-result"></a>
A Result object, though not required to use the framework, is designed as an object to be return from any procedures that may throw an error. It consists of data (i.e., what the function should return if there aren't any errors) and an ErrorInstance.

The pitfalls of polymorphism in Fortran - specifically, the lack of array rank polymorphism - means a number of separate classes exist for data of different ranks, each one extending from an abstract Result class. Currently, this is limited to rank 4 (4 dimensional) data, though this could easily be extended.

```fortran
type(ErrorHandler) :: EH
type(ErrorInstance) :: procError
type(Result0D) :: r

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

### ErrorCriteria <a name="structure-errorcriteria"></a>
This class extends the ErrorHandler and defines a number of common "criteria" used for error checking, such as checking whether a number falls between given bounds. Criteria functions expedite the error checking process with intuitive function calls returning pre-defined ErrorInstances.

```fortran
type(ErrorCriteria) :: EH
type(ErrorInstance) :: limitError

call EH%init()          ! This sets some default error criteria messages
limitError = EH%limit( &
    value = 2, &
    lbound = 0, &
    ubound = 1 &
)
call EH%trigger(error=limitError)
```
```bash
ERROR: Value must be between 0 and 1. Given value: 2.
```

## Learn more <a name="more"></a>

Explore the documentation for each class to learn how to best use the framework, and browse the examples to get an idea of how to implement the framework into your project:

- [ErrorHandler](doc/ErrorHandler.md)
- [ErrorInstance](doc/ErrorInstance.md)
- [Result](doc/Result.md)
- [ErrorCriteria](doc/ErrorCriteria.md)
- [Examples](examples/)

## Caveats and limitations <a name="caveats"></a>

- Error code must be less than 99999
- GFortran bugs:
    - `-O1` or higher must be used to avoid "character length mismatch in array constructor" errors with allocatable character variables.
    - `-fcheck=no-bounds` must be used to avoid errors on allocating rank-2 or higher arrays.
- Result objects only support up to rank-4 (4 dimensional) data.
- Limited support for different kinds, due to Fortran's lack of kind polymorphism. In particular, ErrorCriteria only accept 4-byte integers and single precision, double precision and quadruple precision reals, as such:

```fortran
integer, parameter :: dp = selected_real_kind(15,307)
integer, parameter :: qp = selected_real_kind(33,4931)

integer :: i = 1
real :: r = 1.0
real(dp) :: r_dp = 1.0_dp
real(qp) :: r_qp = 1.0_qp
```