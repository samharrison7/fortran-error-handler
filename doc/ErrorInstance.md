# ErrorInstance

An ErrorInstance is an object representing an error, containing an error code, error message, whether the error is critical (should it stop the program executing), and a user-specified trace of where the error has come from.

- [Creating ErrorInstances](#creating)
- [Error traces](#traces)
    - [Passing ErrorInstances between procedures](#traces-passing)
- [Tests and getters](#tests)

<a name="creating"></a>
## Creating ErrorInstances

An `ErrorInstance(code, message, isCritical, trace)` interface is provided so that ErrorInstances can easily be created. The interface returns the created error.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer :: code` | The error code. | - |
| `character(len=*), optional :: message` | Default message for the error (which there is plenty of opportunity to override). | "" |
| `logical, optional :: isCritical` | Is the error critical? | .true. |
| `character(len=*), optional :: trace(:)` | Custom trace for the error | - |

For example:

```fortran
type(ErrorInstance) :: error
error = ErrorInstance(100, "Error message.", .false.)
```

<a name="traces"></a>
## Error traces

ErrorInstances contain a "trace" property, which is in essence an array of character strings, the idea behind which is to provide a useful utility to give the ability to store information about the error *trace* - where it was triggered and through what route it has passed before being triggered. The trace is automatically printed when `ErrorHandler%trigger()` is called.

Many debugging options already exist that print out an error backtrace (stack trace), detailing the trace of the error in terms of file names and line numbers. Whilst this is useful to the developer, it is often useless to the end user. The trace property attempts to plug this gap by providing the ability to build a custom trace based on what will be most useful information for the end user.

*Note*: A trace message is limited to a length of 256 characters.

#### `ErrorInstance%addToTrace(message)` and `Result%addToTrace(message)`
This procedure adds the specified message to the array of trace messages for ErrorInstance objects, whilst for [Result](Result.md) objects, it adds the same trace messages to all of the errors contained in the Result object (as Result objects can contain an array of errors).

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `character(len=*) :: message` | Trace message to add. | - |

<a name="traces-passing"></a>
### Passing ErrorInstances between procedures
The error trace becomes most useful when errors are passed from procedure-to-procedure. In each procedure, a new "node" can be added to the error to describe what was happening when the error occured. This works well when used alongside the [Result](Result.md) object. Using the error trace, passing errors from nested procedure calls is more useful than triggering errors directly from those procedures, as it gives the end user much more information about where the error happened.

This arbitrary example contains two functions that are used to calculate the area of a circle, from a diameter and from a radius. The former calls another function to calculate the radius from the diameter, and this function checks that the provided diameter is positive (see the [ErrorCriteria docs](ErrorCriteria.md)). The latter calculates the area directly and performs the positive check itself. The trace is formulated so we can see where the error was triggered from an where is passes through:

```fortran
program trace
    use ErrorInstanceModule
    use ErrorCriteriaModule
    use ResultModule
    implicit none

    type(ErrorCriteria) :: EH
    type(Result0D) :: a1, a2

    call EH%init()
    a1 = areaFromRadius(-1.0)
    a2 = areaFromDiameter(-2.0)
    call EH%trigger(errors=[.error. a1, .error. a2])

  contains
    function radius(diameter)
        real :: diameter
        type(Result0D) :: radius
        radius = Result( &
            data = diameter/2, &
            error = EH%positive(diameter) &         ! Make sure diameter is positive
        )
        call radius%addToTrace('Radius from diameter')
    end function

    function areaFromDiameter(diameter) result(area)
        real :: diameter
        type(Result0D) :: area
        area = Result( &
            data = 3.142*.real.radius(diameter)**2, &
            error = .error.radius(diameter) &       ! Pass the error from the radius() function
        )
        call area%addToTrace('Area from diameter')
    end function

    function areaFromRadius(radius) result(area)
        real :: radius
        type(Result0D) :: area
        area = Result( &
            data = 3.142*radius**2, &
            error = EH%positive(radius) &           ! Make sure the radius is positive
        )
        call area%addToTrace('Area from radius')
    end function

end program
```

This results in:

```bash
$ Error: Value must be positive. Given value: -1.00000000.
$ Trace: Area from radius
$ Error: Value must be positive. Given value: -2.00000000.
$ Trace: Radius from diameter > Area from diameter
```

<a name="tests"></a>
## Tests and getters

The error code can be retrieved from an ErrorInstance object by the `getCode()` procedure, which returns an integer. *Note:* The properties of ErrorInstances aren't currently private, and thus can be accessed directly from outside of the module (e.g., `ErrorInstance%code`, `ErrorInstance%message`). This might change in the future.

An ErrorInstance can be tested to see whether it is an error or the default "no error", using the functions `ErrorInstance%isError()` and `ErrorInstance%notError()`. Both return a logical value. This is particularly useful when returning errors from procedures (such as error criteria) to see whether the procedure resulted in an error.