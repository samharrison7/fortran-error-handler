program example_usage
    use TestClassModule
    use ErrorInstanceModule
    use ErrorHandlerModule
    use ErrorCriteriaModule
    use ResultModule
    implicit none

    type(ErrorInstance) :: errors(4)
    type(ErrorInstance) :: newError
    type(ErrorCriteria) :: EH
    type(Result0D) :: r
    type(Result1D) :: r1d
    type(Result2D) :: r2d
    type(Result3D) :: r3d
    type(Result4D) :: r4d
    real :: start, finish
    integer :: i
    type(TestClass) :: test

    integer, parameter :: dp = selected_real_kind(15,307)       !! Double precision, 15 digits, 1e307
    integer, parameter :: qp = selected_real_kind(33,4931)      !! Quadruple precision, 33 digits, 1e4931

    call cpu_time(start)

        call EH%init()
        call test%init()
        r = test%squareRoot(2.1)
        write(*,*) .real. r
        call EH%trigger(error=r%getError())


        ! errors(1) = ErrorInstance(100, "A big error.")
        ! errors(2) = ErrorInstance(200, "A little error.", .false.)
        ! errors(3) = ErrorInstance(1, "This is a default error.")
        ! errors(4) = ErrorInstance(0, "NO ERROR.", .false.)
        ! newError = ErrorInstance(700, "An error!", .false.)

        ! call crit%init(errors=errors)
        ! err => crit%ErrorHandler

        ! call crit%add(300, "300 error message", .false.)
        ! call crit%add(error=newError)
        ! call crit%modifyErrorCriteriaCodes([201,202,203,204,205,206,207,208,209])
        ! call crit%modifyErrorCriterionCode(name='lessThan',newCode=12345)
        ! call crit%modify(code=205,isCritical=.false.)
        ! call crit%modify(code=206,isCritical=.false.)
        ! ! call crit%printErrors()
        ! print *,  crit%getCodeFromCriterionName('nonZero')
        ! call crit%remove([200,300])
        ! call crit%add([501,502,503],["A","B","C"], [.true.,.true.,.true.])
        ! call crit%printErrors()
        ! ! call crit%trigger(code=700)

        ! ! call tcin%setStuff(1.2345)
        ! r = Result( &
        !     data = (1.0,2.0), &
        !     errors = errors &
        ! )
        ! r1d = Result( &
        !     data = [(1.23,4.56),(7.89,0.12)], &
        !     errors = errors &
        ! )
        ! r2d = Result( &
        !     data = reshape([.true.,.true.,.true.,.false.], [2,2]), &
        !     errors = errors &
        ! )
        ! r3d = Result( &
        !     data = reshape([.true.,.true.,.true.,.false.,.true.,.true.,.true.,.false.], [2,2,2]), &
        !     errors = errors &
        ! )
        ! r4d = Result( &
        !     data = reshape([.true.,.true.,.true.,.false.,.true.,.true.,.true., &
        !         .false.,.true.,.true.,.true.,.false.,.true.,.true.,.true.,.false., &
        !         .true.,.true.,.true.,.false.,.true.,.true.,.true.,.false.,.true., &
        !         .true.,.true.,.false.,.true.,.true.,.true.,.false.], [2,2,2,2]), &
        !     errors = errors &
        ! )

        ! ! in = .integer. r1d
        ! ! in2d = .integer. r2d

        ! write(*,*) .complex. r
        ! write(*,*) .complex. r1d
        ! write(*,*) .logical. r2d
        ! write(*,*) .logical. r3d
        ! write(*,*) .logical. r4d

        ! do i=1, 50000
        !     call crit%trigger(error=crit%negative(value=-1,traceMessage="destination"))
        ! end do


    call cpu_time(finish)
    print '("Time = ",f10.8," seconds.")',finish-start

    
end program