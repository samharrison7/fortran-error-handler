# Result

***Work in progress***

<!-- ***Only sp (not kind attr), dp and qp supported in .real. (.sp., alias of .real., .dp., .qp) because kinds (real(kind)) can't be set dynamically and select type construct takes into account kind, thus real(dp) won't pass type is (real). You must specify dp = selected_kind_parameter() in wherever you pass data from. Don't currently support integer types.***

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
``` -->