program example_usage
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
            ErrorInstance(code=300, message="Another custom error message.", isCritical=.true.) &
        ] &
    )

    ! Trigger the 200 error, which is non-critical and so the program will carry on running
    call EH%trigger(200)

    write(*,"(a)") "Enter an integer between 0 and 10, but not equal to 5:"
    read(*,*) i

    r = Result( &
        data = i, &
        errors = [ &
            EH%limit(i,0,10), &
            EH%notEqual(i,5) &
        ] &
    )
    call r%addToTrace("example_usage")
    call r%addToTrace("another trace point")
    call EH%trigger(errors=.errors. r)
    write(*,"(a,i1)") "Input value is: ", .integer. r
    
end program