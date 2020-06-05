module tests_Result
    use util
    use assert
    use ResultModule
    use ErrorInstanceModule
    implicit none

  contains

    subroutine createResultWithNoError()
        type(Result0D) :: rslt
        rslt = Result(data=0)
        call assertEqual(.false., rslt%hasError(), "Failed to create Result with no error.")
    end subroutine

    subroutine createResultWith0DIntegerData()
        type(Result0D) :: rslt
        rslt = Result(data=42)
        call assertEqual(42, .integer. rslt, "Failed to create Result with 0D integer data.")
    end subroutine
    
    subroutine createResultWith0DRealData()
        type(Result0D) :: rslt
        rslt = Result(data=42.0)
        call assertEqual(42.0, .real. rslt, "Failed to create Result with 0D real data.")
    end subroutine
    
    subroutine createResultWith0DDPData()
        type(Result0D) :: rslt
        rslt = Result(data=42.0_dp)
        call assertEqual(42.0_dp, .dp. rslt, "Failed to create Result with 0D real DP data.")
    end subroutine
    
    subroutine createResultWith0DQPData()
        type(Result0D) :: rslt
        rslt = Result(data=42.0_qp)
        call assertEqual(42.0_qp, .qp. rslt, "Failed to create Result with 0D real QP data.")
    end subroutine

    subroutine createResultWith0DLogicalData()
        type(Result0D) :: rslt
        rslt = Result(data=.true.)
        call assertEqual(.true., .logical. rslt, "Failed to create Result with 0D logical data.")
    end subroutine

    subroutine createResultWith0DCharacterData()
        type(Result0D) :: rslt
        rslt = Result(data="Hello")
        call assertEqual("Hello", .character. rslt, "Failed to create Result with 0D logical data.")
    end subroutine

    subroutine createResultWith0DComplexData()
        type(Result0D) :: rslt
        rslt = Result(data=(1.0,0.0))
        call assertEqual((1.0,0.0), .complex. rslt, "Failed to create Result with 0D complex data.")
    end subroutine

    subroutine createResultWith1DIntegerData()
        type(Result1D) :: rslt
        rslt = Result(data=[42])
        call assertEqual(1, size(.integer. rslt), "Failed to create Result with 1D integer data.")
    end subroutine
    
    subroutine createResultWith1DRealData()
        type(Result1D) :: rslt
        rslt = Result(data=[42.0])
        call assertEqual(1, size(.real. rslt), "Failed to create Result with 1D real data.")
    end subroutine
    
    subroutine createResultWith1DDPData()
        type(Result1D) :: rslt
        rslt = Result(data=[42.0_dp])
        call assertEqual(1, size(.dp. rslt), "Failed to create Result with 1D real DP data.")
    end subroutine
    
    subroutine createResultWith1DQPData()
        type(Result1D) :: rslt
        rslt = Result(data=[42.0_qp])
        call assertEqual(1, size(.qp. rslt), "Failed to create Result with 1D real QP data.")
    end subroutine
    
    subroutine createResultWith2DIntegerData()
        type(Result2D) :: rslt
        integer :: test_data(1,1) = 42
        rslt = Result(data=test_data)
        call assertEqual(2, size(shape(.integer. rslt)), "Failed to create Result with 2D integer data.")
    end subroutine
    
    subroutine createResultWith2DRealData()
        type(Result2D) :: rslt
        integer :: test_data(1,1) = 42.0
        rslt = Result(data=test_data)
        call assertEqual(2, size(shape(.real. rslt)), "Failed to create Result with 2D real data.")
    end subroutine
    
    subroutine createResultWith2DDPData()
        type(Result2D) :: rslt
        integer :: test_data(1,1) = 42.0_dp
        rslt = Result(data=test_data)
        call assertEqual(2, size(shape(.dp. rslt)), "Failed to create Result with 2D real DP data.")
    end subroutine
    
    subroutine createResultWith2DQPData()
        type(Result2D) :: rslt
        integer :: test_data(1,1) = 42.0_qp
        rslt = Result(data=test_data)
        call assertEqual(2, size(shape(.qp. rslt)), "Failed to create Result with 2D real QP data.")
    end subroutine
    
    subroutine createResultWith3DIntegerData()
        type(Result3D) :: rslt
        integer :: test_data(1,1,1) = 42
        rslt = Result(data=test_data)
        call assertEqual(3, size(shape(.integer. rslt)), "Failed to create Result with 3D integer data.")
    end subroutine
    
    subroutine createResultWith3DRealData()
        type(Result3D) :: rslt
        integer :: test_data(1,1,1) = 42.0
        rslt = Result(data=test_data)
        call assertEqual(3, size(shape(.real. rslt)), "Failed to create Result with 3D real data.")
    end subroutine
    
    subroutine createResultWith3DDPData()
        type(Result3D) :: rslt
        integer :: test_data(1,1,1) = 42.0_dp
        rslt = Result(data=test_data)
        call assertEqual(3, size(shape(.dp. rslt)), "Failed to create Result with 3D real DP data.")
    end subroutine
    
    subroutine createResultWith3DQPData()
        type(Result3D) :: rslt
        integer :: test_data(1,1,1) = 42.0_qp
        rslt = Result(data=test_data)
        call assertEqual(3, size(shape(.qp. rslt)), "Failed to create Result with 3D real QP data.")
    end subroutine

    subroutine createResultWith4DIntegerData()
        type(Result4D) :: rslt
        integer :: test_data(1,1,1,1) = 42
        rslt = Result(data=test_data)
        call assertEqual(4, size(shape(.integer. rslt)), "Failed to create Result with 4D integer data.")
    end subroutine
    
    subroutine createResultWith4DRealData()
        type(Result4D) :: rslt
        integer :: test_data(1,1,1,1) = 42.0
        rslt = Result(data=test_data)
        call assertEqual(4, size(shape(.real. rslt)), "Failed to create Result with 4D real data.")
    end subroutine
    
    subroutine createResultWith4DDPData()
        type(Result4D) :: rslt
        integer :: test_data(1,1,1,1) = 42.0_dp
        rslt = Result(data=test_data)
        call assertEqual(4, size(shape(.dp. rslt)), "Failed to create Result with 4D real DP data.")
    end subroutine
    
    subroutine createResultWith4DQPData()
        type(Result4D) :: rslt
        integer :: test_data(1,1,1,1) = 42.0_qp
        rslt = Result(data=test_data)
        call assertEqual(4, size(shape(.qp. rslt)), "Failed to create Result with 4D real QP data.")
    end subroutine

end module