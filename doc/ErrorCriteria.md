# ErrorCriteria

***Work in progress.***

This class extends the ErrorHandler and defines a number of common "criteria" used for error checking, such as checking whether a number falls between given bounds. Criteria functions expedite the error checking process with intuitive function calls returning pre-defined ErrorInstances. It is an *optional extension* and the [ErrorHandler](docs/ErrorHandler.md) can be used without it.

### Type and kind conventions
**TL;DR:** Only `integer`, `real`, `real(dp)` and `real(qp)` can be tested using the criteria, where `dp` (double precision) and `qp` (quadruple precision) are defined as:

```fortran
integer, parameter :: dp = selected_real_kind(15,307)
integer, parameter :: qp = selected_real_kind(33,4931) 
```

**More detailed explanation:** The lack of dynamic (run-time) polymorphism in Fortran means that the compiler must know the type of a variable before operating on it. This means that generic procedure names, such as the criterion names used by the ErrorCriteria (e.g., `limit`, `postive`; see below) have to be bound to a separate procedure for each variable type *and* kind (an alternative approch would have been to use the `select type` construct in one large procedure for each criterion, but the implication remains the same). Currently, procedures for those types and kind listed above are implemented, and the use of `selected_real_kind` to define `dp` and `qp` was chosen as "best practice" in texts such as N. S. Clerman and W. Spector's book "Modern Fortran" (2012). If there is demand in the future, other types and kinds might be added (e.g., support for 8-byte integers or complex numbers).

## Example

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

## Default criteria functions

| Function | Description | Default code |
| :--- | :--- | :--- |
| `ErrorCriteria%nonZero(value, epsilon, message, traceMessage)` | Check a value is non-zero, or further than +- epsilon from zero. | 101 |
| `ErrorCriteria%zero(value, epsilon, message, traceMessage)` | Check a value is zero, or within +- epsilon of zero. | 102 |
| `ErrorCriteria%lessThan(value, ubound, message, traceMessage)` | Check a value is less than ubound. | 103 |
| `ErrorCriteria%greaterThan(value, lbound, message, traceMessage)` | Check a value is greater than lbound. | 104 |
| `ErrorCriteria%limit(value, lbound, ubound, message, traceMessage)` | Check a value is between lbound and ubound. | 105 |
| `ErrorCriteria%notEqual(value, criterion, epsilon, message, traceMessage)` | Check a value is not equal to criterion, or further than +- epsilon from criterion. | 106 |
| `ErrorCriteria%equal(value, criterion, epsilon, message, traceMessage)` | Check a value is equal to criterion, or within +- epsilon of criterion. | 107 |
| `ErrorCriteria%positive(value, message, traceMessage)` | Check a value is positive. | 108 |
| `ErrorCriteria%negative(value, message, traceMessage)` | Check a value is negative. | 109 |


## Extending the error criteria

Create a new class that extends ErrorCriteria to specify your own custom error criteria.