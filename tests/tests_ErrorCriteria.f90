module tests_ErrorCriteria
    use assert
    use ErrorCriteriaModule
    use ErrorInstanceModule
    implicit none

  contains

    subroutine createErrorHandlerWithCustomError()
        type(ErrorCriteria) :: EH
        type(ErrorInstance) :: error
        call EH%init(errors=[ErrorInstance(401, "Nothing here!")])
        error = EH%getErrorFromCode(401)
        call assertEqual(401, error%code, "Failed to create ErrorHandler with custom error.")
    end subroutine

    subroutine checkErrorCriteriaNonZero()
        type(ErrorCriteria) :: EH
        type(ErrorInstance) :: error
        call EH%init()
        error = EH%nonZero(0)
        call assertEqual(.true., error%isError(), "ErrorCriteria nonZero check failed.")
    end subroutine

    subroutine checkErrorCriteriaZero()
        type(ErrorCriteria) :: EH
        type(ErrorInstance) :: error
        call EH%init()
        error = EH%zero(0)
        call assertEqual(.false., error%isError(), "ErrorCriteria zero check failed.")
    end subroutine

    subroutine checkErrorCriteriaLessThan()
        type(ErrorCriteria) :: EH
        type(ErrorInstance) :: error
        call EH%init()
        error = EH%lessThan(1.0, 2.0)
        call assertEqual(.false., error%isError(), "ErrorCriteria lessThan check failed.")
    end subroutine

    subroutine checkErrorCriteriaGreaterThan()
        type(ErrorCriteria) :: EH
        type(ErrorInstance) :: error
        call EH%init()
        error = EH%greaterThan(1.0, 2.0)
        call assertEqual(.true., error%isError(), "ErrorCriteria greaterThan check failed.")
    end subroutine

    subroutine checkErrorCriteriaLimit()
        type(ErrorCriteria) :: EH
        type(ErrorInstance) :: error
        call EH%init()
        error = EH%limit(1.0_dp, 0.0_dp, 1.1_dp)
        call assertEqual(.false., error%isError(), "ErrorCriteria limit check failed.")
    end subroutine

    subroutine checkErrorCriteriaNotEqual()
        type(ErrorCriteria) :: EH
        type(ErrorInstance) :: error
        call EH%init()
        error = EH%notEqual(1.0_qp, 0.0_qp)
        call assertEqual(.false., error%isError(), "ErrorCriteria notEqual check failed.")
    end subroutine

    subroutine checkErrorCriteriaEqual()
        type(ErrorCriteria) :: EH
        type(ErrorInstance) :: error
        call EH%init()
        error = EH%equal(1.0_qp, 0.0_qp)
        call assertEqual(.true., error%isError(), "ErrorCriteria equal check failed.")
    end subroutine

    subroutine checkErrorCriteriaPositive()
        type(ErrorCriteria) :: EH
        type(ErrorInstance) :: error
        call EH%init()
        error = EH%positive(42)
        call assertEqual(.false., error%isError(), "ErrorCriteria positive check failed.")
    end subroutine

    subroutine checkErrorCriteriaNegative()
        type(ErrorCriteria) :: EH
        type(ErrorInstance) :: error
        call EH%init()
        error = EH%negative(-42)
        call assertEqual(.false., error%isError(), "ErrorCriteria negative check failed.")
    end subroutine

end module