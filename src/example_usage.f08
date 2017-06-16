program example_usage
    use TestClassModule
    use ErrorInstanceModule
    use ErrorHandlerModule
    use ErrorCriteriaModule
    use ResultModule
    implicit none

    type(ErrorInstance) :: errors(4)
    type(ErrorInstance) :: newError
    type(ErrorCriteria), target :: crit
    type(ErrorHandler), pointer :: err
    type(Result) :: r
    real :: start, finish
    integer :: i
    integer :: int
    real :: re(2)
    type(TestClass) :: tcin
    type(TestClass) :: tcout
    class(*), allocatable :: tc

    call cpu_time(start)

        errors(1) = ErrorInstance(100, "A big error.")
        errors(2) = ErrorInstance(200, "A little error.", .false.)
        errors(3) = ErrorInstance(1, "This is a default error.")
        errors(4) = ErrorInstance(0, "NO ERROR.", .false.)
        newError = ErrorInstance(700, "An error!", .false.)

        call crit%init(errors=errors)
        err => crit%ErrorHandler

        call crit%add(300, "300 error message", .false.)
        call crit%add(error=newError)
        call crit%modifyErrorCriteriaCodes([201,202,203,204,205,206,207,208])
        call crit%modifyErrorCriterionCode(name='lessThan',newCode=12345)
        ! call crit%printErrors()
        print *,  crit%getCodeFromCriterionName('nonZero')
        call crit%remove([200,300])
        call crit%add([501,502,503],["A","B","C"], [.true.,.true.,.true.])
        call crit%printErrors()
        ! call crit%trigger(code=700)

        ! call tcin%setStuff(1.2345)
        r = Result( &
            data = 'hi', &
            errors = errors &
        )
        ! call crit%trigger(errors=r%getErrors())
        ! allocate(tc, source=r%getData())
        ! re = .real. r  ! Make this work
        ! re = transfer(source=r%getData(), mold=re)

        ! re = tcin%getStuff()
        write(*,*) .character. r

        ! select type(d => r%getData())
        !     type is (TestClass)
        !         write(*,'(f6.4)') d%getStuff()
        ! end select


    call cpu_time(finish)
    print '("Time = ",f10.8," seconds.")',finish-start

    
end program