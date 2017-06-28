# ErrorCriteria

***Work in progress.***

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
