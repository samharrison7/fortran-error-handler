# Result

The Result type is an addon to the framework and is designed as an object to be returned from any procedures that may throw an error. It consists of data (i.e., what the function should return if there aren't any errors) and an array of ErrorInstances.

- [Creating Result objects](#creating)
- [Data](#data)
- [Errors](#errors)
- [Error trace](#trace)

<a name="rank"></a>
#### Rank (dimensions)
Fortran lacks any real kind of array-rank polymorphism ([assumed rank arrays](https://software.intel.com/en-us/node/692101) currently have very limited implementation and scope, and the `select rank` construct has even less support) and creating a Result object able to store both type- and rank-polymorphic data [proved impossible](https://stackoverflow.com/questions/44564872/using-assumed-rank-fortran-array-as-derived-type-component). As such, a different type exists for each rank, up to rank-4 (4 dimensional) data: `type(Result0D)`, `type(Result1D)`, `type(Result2D)`, `type(Result3D)` and `type(Result4D)`. These all extend from the parent `type(Result)`, which can be instantied itself to provide a Result object with no data (e.g., for returning an error from functions that would normally be subroutines).

Extending to high ranks is trivial - take a look at the [Result.f08](../src/Result.f08) file to see how it's done. If you do this, it is strongly encouraged to place the extension in a separate file so that you can later update the framework without conflicting with or overriding the changes you've made.

<a name="creating"></a>
## Creating Result objects
A generic `Result(data, error, errors)` interface can be used to construct the Result object with a type corresponding to the rank of the data that you provide. If no error is provided, the errors array is set to contain only the default "no error" with code 0.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `class(*) :: data`, either scalar or ranks 1 to 4 | The data to store. | - |
| `type(ErrorInstance), optional :: error` | An error, which is stored as the first and only item in the Result object's errors array | - |
| `type(ErrorInstance), optional :: errors(:)` | Errors to store. | - |

For example:

```fortran
type(ErrorInstance) :: randomError
type(ErrorInstance) :: anotherError
type(Result) :: rNoData
type(Result0D) :: r0D
type(Result1D) :: r1D

randomError = ErrorInstance(42, "A random error.")
rNoData = Result(error=randomError)
r1D = Result(data=1, errors=[randomError,anotherError])
r2D = Result(data=[1,2], error=randomError)
```

<a name="data"></a>
## Data
The data property is an unlimited polymorphic object, `class(*)`, and so any object (or array of the same type objects) can be stored in it. However, this does means that a `select type` construct or `transfer()` function must be used when performing operations with the data that are returned from the Result object (using `Result%getData()`), to avoid type conversion errors.

For your convenience, data for the most common types and kinds have pre-defined functions and corresponding operators that use the `select type` construct to return the data as the specified type. These follow the same kind conventions as described in the [ErrorCriteria docs](ErrorCriteria.md#type-kind) for integers and reals.

The following functions are generics that will work with any rank of Result object that has data (`Result0D`, `Result1D`, etc.), and for non-scalar data, they return an array of the specified type. If they are unsuccessful returning the data as the specified type, an error will be thrown.

| Return type and kind | Function | Operator | Implicit conversion |
| :--- | :--- | :--- | :--- |
| `integer` | `Result%getDataAsInteger()` | `.integer.` | If data is `real`, `real(dp)` or `real(qp)`, the nearest integer will be returned |
| `real` | `Result%getDataAsReal()` | `.real.` | If data is `integer`, `real(dp)` or `real(qp)`, it will be converted to `real` |
| `real(dp)` | `Result%getDataAsRealDP()` | `.dp.` | If data is `integer`, `real` or `real(qp)`, it will be converted to `real(dp)` |
| `real(qp)` | `Result%getDataAsRealQP()` | `.qp.` | If data is `integer`, `real` or `real(dp)`, it will be converted to `real(qp)` |
| `character(:)` | `Result%getDataAsCharacter()` | `.character.` |  |
| `logical` | `Result%getDataAsLogical()` | `.logical.` |  |
| `complex` | `Result%getDataAsComplex()` | `.complex.` |  |

For example:

```fortran
type(Result0D) :: r0D
type(Result1D) :: r1D

r0D = Result(data=1.23_dp)
r1D = Result(data=['Hello','World'])

print *, .integer. r0D, &           ! real(dp) data will be converted
            .real. r0D, &
            .dp. r0D, &
            .qp. r0D
print *, .character. r1D
print *, r1D%getDataAsCharacter()   ! This is equivalent to the previous line
print *, .integer. r1D              ! This will throw an error
```

This results in:

```bash
$            1   1.23000002       1.2300000000000000        1.22999999999999998223643160599749535
$  HelloWorld
$  HelloWorld
$ ERROR STOP Error trying to return 1D data as INTEGER. Are you sure the data is of type INTEGER?
```

<a name="errors"></a>
## Errors

Result objects store an array of ErrorInstances, making it possible to return multiple errors from functions. `Result%getErrors()` returns this array of errors, whilst `Result%getError()` returns the first error in this array. Two operators map directly to these functions: `.errors.` and `.error.`, respectively:

```fortran
type(Result) :: r

r = Result(error=ErrorInstance(100, "A warning.", .false.))
call EH%trigger(error = .error. r)          ! Equivalent to `call EH%trigger(error=r%getError())`

r = Result(errors=[ &
    ErrorInstance(100, "A warning.", .false.), &
    ErrorInstance(200, "An error.", .true.) &
])
call EH%trigger(errors = .errors. r)        ! Equivalent to `call EH%trigger(errors=r%getErrors())`
```

This results in:

```bash
$ Warning: A warning.
$ Warning: A warning.
$ Error: An error.
$ ERROR STOP 200
```

`Result%getErrorCode()` returns the integer code of the first error in the errors array.

### Adding further errors
Once a Result object has been created, you can add further errors to the array of errors stored by it, by using the `Result%addError(error)` and `Result%addErrors(errors)` functions.

#### `Result%addError(error)`

Add a single error to the Result object.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `type(ErrorInstance) :: error` | The error to add to the Result object's array of errors. | - |

#### `Result%addErrors(errors)`

Add an array of errors to the Result object.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `type(ErrorInstance) :: errors(:)` | The errors to add to the Result object's array of errors. | - |

### Checking for errors
You can use the `Result%hasError()` and `Result%hasCriticalError()` procedures to check whether a Result object has an error (and that error isn't the default no error, with code 0), and whether it has a critical error, respetively. Both return a logical value.

<a name="trace"></a>
## Error trace

The procedure `Result%addToTrace(message)` can be used to add the same node to each of the Result object's errors. See the [ErrorInstance docs](ErrorInstance.md#traces).