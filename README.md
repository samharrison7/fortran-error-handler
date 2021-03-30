# Fortran Error Handler

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4268263.svg)](https://doi.org/10.5281/zenodo.4268263)
[![fair-software.eu](https://img.shields.io/badge/fair--software.eu-%E2%97%8F%20%20%E2%97%8F%20%20%E2%97%8B%20%20%E2%97%8F%20%20%E2%97%8B-orange)](https://fair-software.eu)

Fortran error handling frameworks are few and far between, and those that do exist often implement only parts of the error handling process, or rely on pre-processors. The goal of this error handling framework is to provide a universal and comprehensive solution for applications requiring functional and robust error handling, utilising the power of modern object-oriented Fortran.

- [Getting started](#getting-started)
- [Usage](#usage)
    - [Basic structure](#usage-structure)
    - [Quick start guide](#usage-quickstart)
- [Learn more](#more)
- [Caveats and limitations](#more)

<a name="getting-started"></a>
## Getting started

If you wish to use the Fortran Error Handler in a project, the simplest way to do so is to include the source files (in `src/`) at the start of your compilation setup. Source files should be compiled in this order: `ErrorInstance.f90`, `ErrorHandler.f90`, `ErrorCriteria.f90`, `Result.f90`. An example [Makefile.example](./Makefile.example) is included, which can be altered according to your compiler and preferences. 

The code can also be compiled using `cmake`, which creates an example executable (using `example/example_usage.f90`), an executable of unit tests (using `tests/run_tests.f90`), and a library of the framework:

```bash
$ mkdir build
$ cd build
$ cmake ..
$ make
# To run the unit tests
$ ./test
# To run the example
$ ./example
```

Whether the library is shared or not is specified by the `BUILD_SHARED_LIBS` variable. If you wish to build a shared library, then pass the `BUILD_SHARED_LIBS` option as on:

```bash
$ cmake .. -DBUILD_SHARED_LIBS=ON
```

The framework has been tested using GFortran 7 upwards and Intel Fortran 18.

<a name="usage"></a>
## Usage

Read the below documentation for example usage, and check out the [example](example/) directory for ideas of how to incorporate into your project.

<a name="usage-structure"></a>
### Basic structure

The framework consists of two main classes:

- [ErrorHandler](doc/ErrorHandler.md): Responsible for initiating the error handling environment, queuing and triggering error events, and storing a list of possible error codes and their respective error messages.
- [ErrorInstance](doc/ErrorInstance.md): An ErrorInstance is an object representing an error, containing an error code, error message, whether the error is critical (should stop the program executing), and a user-defined trace of where the error has come from.

A number of further classes provide added functionality:

- [Result](doc/Result.md): A Result object, though not required to use the framework, is designed as an object to be returned from any procedures that (may) throw an error. An object of `type(Result)` contains an `errors` component, which is an array of ErrorInstances and is intended to be returned from procedures that normally wouldn't return anything else (i.e., subroutines). To facilitate the returning of data as well as errors from procedures (i.e., functions), a number of separate types extend `Result`, each one dealing with data of a different rank (dimensionality). In addition to the `errors` component, they contain a `data` component, whose rank is determined by the type name. For example, `type(Result0D)` is used for scalar data (0D), `type(Result1D)` is used for data of rank-1 (1D) and `type(Result2D)` is used for data of rank-2 (2D). The maximum data rank is currently rank-4 (`type(Result4D)`).
- [ErrorCriteria](doc/ErrorCriteria.md): This class extends the ErrorHandler and defines a number of common "criteria" used for error checking, such as checking whether a number falls between given bounds. Criteria functions expedite the error checking process with intuitive function calls returning pre-defined ErrorInstances.

<a name="usage-quickstart"></a>
### Quick start guide

Let's create a quick program that asks for a user's input, checks that it passes a number of criteria and if it doesn't, triggers an error.

Firstly, you need to include the appropriate modules to use:

```fortran
use ErrorInstanceModule
use ErrorCriteriaModule
use ResultModule
```

ErrorCriteria extends ErrorHandler and so if we are using the ErrorCriteria class, we don't need to directly use the ErrorHandler. ErrorCriteria can be thought of as the ErrorHandler (thus responsible for storing and triggering errors), but with criteria-checking capabilities. If we're using it, we need to create and initialise an ErrorCriteria instance so that default ErrorInstances are set for different criteria. If we don't want criteria-checking capabilities, we can just create and initialise the ErrorHandler instead.

Let's also initialise an integer to store the user's input into, and a scalar (0D) Result object to store data and errors in:

```fortran
type(ErrorCriteria) :: EH       ! If criteria-checking not needed, use ErrorHandler instead
integer :: i
type(Result0D) :: r
```

We first need to initialise the ErrorCriteria (and thus ErrorHandler). This sets a default error with code 1, and a "no error" with code 0, as well as a number of default errors for the criteria (see the [ErrorCriteria docs](doc/ErrorCriteria.md)). At the same time, let's specify two custom errors that we might want to trigger at some later point in the code. `isCritical` determines whether triggering of the error stops the program executing or just prints a warning, and it defaults to true (i.e., triggering an error stops the program).

```fortran
call EH%init( &
    errors = [ &
        ErrorInstance(code=200, message="A custom error message.", isCritical=.false.), &
        ErrorInstance(code=300, message="Another custom error message.", isCritical=.true.) &
    ] &
)
```

We can also add errors to the ErrorHandler on the fly by using the `add` procedure: `call EH%add(code=200, message="A custom error message.")`.

Now let's get the user to enter an integer, and specify that it should be between 0 and 10, but not equal to 5:

```fortran
write(*,"(a)") "Enter an integer between 0 and 10, but not equal to 5:"
read(*,*) i
```

We now need to test `i` meets these criteria, and we can do that using the ErrorCriteria's `limit` and `notEqual` methods. We'll do this at the same time as creating the Result object, which stores the data `i` as well as any errors:

```fortran
r = Result( &
    data = i, &
    errors = [ &
        EH%limit(i,0,10), &
        EH%notEqual(i,5) &
    ] &
)
```

Now we can attempt to trigger any errors that might be present, by using the ErrorHandler's `trigger` method, which accepts either an error code, an ErrorInstance or an array of ErrorInstances as its parameters. If the criteria check didn't result in any errors, nothing will happen (in reality, the criteria returns an ErrorInstance with code 0 - the default "no error" - which the `trigger` method ignores).

```fortran
call EH%trigger(errors=r%getErrors())
```

The default ErrorInstances set up by the `init` procedure for ErrorCriteria all have `isCritical` set to true, and so if either of these criteria errors are triggered, program execution will be stopped. The default criteria errors can be altered by using the `modify` procedure - see the [ErrorCriteria docs](doc/ErrorCriteria.md). Finally, let's print the value to the console:

```fortran
write(*,"(a,i1)") "Input value is: ", .integer. r
```

A number of operators, such an `.integer.` and `.real.`, are available to quickly return the data (which is stored polymorphically) from a Result object as a specific type. If you are uncomfortable using custom operators, then the corresponding procedures `r%getDataAsInteger()`, `r%getDataAsReal()`, etc, are also available. See the [Result docs](doc/Result.md) for more details.

Let's test it out with a few different integers:

```bash
$ Enter an integer between 0 and 10, but not equal to 5:
$ 12
$ Error: Value must be between 0 and 10. Given value: 12.
$ ERROR STOP 105

$ Enter an integer between 0 and 10, but not equal to 5:
$ 5
$ Error: Value must not be equal to 5. Given value: 5.
$ ERROR STOP 106

$ Enter an integer between 0 and 10, but not equal to 5:
$ 1
$ Input value is: 1
```

The stop codes 105 and 106 are the default error codes for those particular limit criteria. These codes can be modified using the ErrorCriteria's `modifyErrorCriterionCode` procedure. See the [ErrorCriteria docs](doc/ErrorCriteria.md).

The framework is designed to work seemlessly in large object-oriented projects, where Result objects can be returned from functions with ErrorInstances that contain a user-defined trace of where the error came from. The goal of such a trace is to provide more useful errors to end users of your application, who might not have access to the source code and thus find standard stack traces containing references to files and line numbers useless. More details can be found in the [ErrorInstance docs](doc/ErrorInstance.md), and more thorough examples in the [examples directory](example/).

## Learn more <a name="more"></a>

Explore the documentation for each class to learn how to best use the framework, and browse the examples to get an idea of how to implement the framework into your project:

- [ErrorHandler](doc/ErrorHandler.md)
- [ErrorInstance](doc/ErrorInstance.md)
- [Result](doc/Result.md)
- [ErrorCriteria](doc/ErrorCriteria.md)
- [Examples](example/)

## Caveats and limitations <a name="caveats"></a>

- Error code must be less than 99999.
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
