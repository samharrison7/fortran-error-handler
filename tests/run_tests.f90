program run_test
    use tests_Result
    use tests_ErrorInstance
    use tests_ErrorCriteria
    implicit none

    write(*, '(a)') "Starting tests for ErrorInstance..."
    call createErrorWithDefaultCode()
    call createErrorWithMessageHello()
    call createErrorWithCode42()
    call createCriticalError()
    call createNonCriticalError()
    call addMessageToTrace()
    call addTwoMessagesToTrace()
    call addTraceMessageSayingHello()

    write(*, '(a)') "Starting tests for Result..."
    call createResultWithNoError()
    call createResultWith0DIntegerData()
    call createResultWith0DRealData()
    call createResultWith0DDPData()
    call createResultWith0DQPData()
    call createResultWith0DLogicalData()
    call createResultWith0DCharacterData()
    call createResultWith0DComplexData()
    call createResultWith1DIntegerData()
    call createResultWith1DRealData()
    call createResultWith1DDPData()
    call createResultWith1DQPData()
    call createResultWith2DIntegerData()
    call createResultWith2DRealData()
    call createResultWith2DDPData()
    call createResultWith2DQPData()
    call createResultWith3DIntegerData()
    call createResultWith3DRealData()
    call createResultWith3DDPData()
    call createResultWith3DQPData()
    call createResultWith4DIntegerData()
    call createResultWith4DRealData()
    call createResultWith4DDPData()
    call createResultWith4DQPData()

    write(*, '(a)') "Starting tests for ErrorHandler and ErrorCriteria..."
    call createErrorHandlerWithCustomError()
    call checkErrorCriteriaNonZero()
    call checkErrorCriteriaLessThan()
    call checkErrorCriteriaGreaterThan()
    call checkErrorCriteriaLimit()
    call checkErrorCriteriaNotEqual()
    call checkErrorCriteriaEqual()
    call checkErrorCriteriaPositive()
    call checkErrorCriteriaNegative()

    write(*, '(a)') "Tests completed successfully!"

end program    