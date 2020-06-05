module tests_ErrorInstance
    use ErrorInstanceModule
    use assert
    implicit none

  contains

    subroutine createErrorWithDefaultCode()
        type(ErrorInstance) :: error
        error = ErrorInstance()
        call assertEqual(1, error%code, "Failed to create ErrorInstance with default error code.")
    end subroutine

    subroutine createErrorWithCode42()
        type(ErrorInstance) :: error
        error = ErrorInstance(code=42)
        call assertEqual(42, error%code, "Failed to create ErrorInstance with error code 42.")
    end subroutine

    subroutine createErrorWithMessageHello()
        type(ErrorInstance) :: error
        error = ErrorInstance(message="Hello")
        call assertEqual("Hello", error%message, "Failed to create ErrorInstance with message 'Hello'.")
    end subroutine

    subroutine createCriticalError()
        type(ErrorInstance) :: error
        error = ErrorInstance(isCritical=.true.)
        call assertEqual(error%isCritical, .true., "Failed to create ErrorInstace with isCritical = .true.")
    end subroutine

    subroutine createNonCriticalError()
        type(ErrorInstance) :: error
        error = ErrorInstance(isCritical=.false.)
        call assertEqual(error%isCritical, .false., "Failed to create ErrorInstace with isCritical = .false.")
    end subroutine

    subroutine addMessageToTrace()
        type(ErrorInstance) :: error
        error = ErrorInstance()
        call error%addToTrace("")
        call assertEqual(1, size(error%trace), "Failed to add a trace message to ErrorInstance.")
    end subroutine

    subroutine addTwoMessagesToTrace()
        type(ErrorInstance) :: error
        error = ErrorInstance()
        call error%addToTrace("")
        call error%addToTrace("")
        call assertEqual(2, size(error%trace), "Failed to add a trace message to ErrorInstance.")
    end subroutine

    subroutine addTraceMessageSayingHello()
        type(ErrorInstance) :: error
        error = ErrorInstance()
        call error%addToTrace("Hello")
        call assertEqual("Hello", error%trace(1), "Failed to add trace message saying 'Hello'.")
    end subroutine
end module