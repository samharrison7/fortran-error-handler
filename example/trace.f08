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