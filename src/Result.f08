module ResultModule
    use ErrorInstanceModule
    implicit none
    private

    ! Set kind parameters for use with real variables, as per the convention
    ! recommended in, e.g., http://fortranwiki.org/fortran/show/Real+precision
    ! and N. S. Clerman and W. Spector, Modern Fortran (2012).
    integer, parameter :: dp = selected_real_kind(15,307)       !! Double precision, 15 digits, 1e307
    integer, parameter :: qp = selected_real_kind(33,4931)      !! Quadruple precision, 33 digits, 1e4931

    type, public :: Result
        type(ErrorInstance), dimension(:), allocatable :: errors !! The errors (if any) returned from the function
        
        contains
            ! Error procedures
            procedure, public :: getError
            procedure, public :: getErrors
            procedure, public :: getErrorCode
            procedure, public :: setErrors
            procedure, public :: addToTrace

            ! Operators
            generic, public :: operator(.error.) => getError
            generic, public :: operator(.errors.) => getErrors
    end type

    type, public, extends(Result) :: Result0D
        private
        class(*), allocatable :: data     !! The data returned from the function

        contains
            ! Getters for data
            procedure, public :: getData => getData0D
            procedure, public :: getDataAsReal => getDataAsReal0D
            procedure, public :: getDataAsRealDP => getDataAsRealDP0D
            procedure, public :: getDataAsRealQP => getDataAsRealQP0D
            procedure, public :: getDataAsInteger => getDataAsInteger0D
            procedure, public :: getDataAsCharacter => getDataAsCharacter0D
            procedure, public :: getDataAsLogical => getDataAsLogical0D
            procedure, public :: getDataAsComplex => getDataAsComplex0D
            
            ! Operators
            generic, public :: operator(.real.) => getDataAsReal
            generic, public :: operator(.dp.) => getDataAsRealDP
            generic, public :: operator(.qp.) => getDataAsRealQP
            generic, public :: operator(.integer.) => getDataAsInteger
            generic, public :: operator(.character.) => getDataAsCharacter
            generic, public :: operator(.logical.) => getDataAsLogical
            generic, public :: operator(.complex.) => getDataAsComplex
            
    end type

    type, public, extends(Result) :: Result1D
        private
        class(*), allocatable :: data(:)     !! The data returned from the function

        contains
            ! Getters for data
            procedure, public :: getData => getData1D
            procedure, public :: getDataAsReal => getDataAsReal1D
            procedure, public :: getDataAsRealDP => getDataAsRealDP1D
            procedure, public :: getDataAsRealQP => getDataAsRealQP1D
            procedure, public :: getDataAsInteger => getDataAsInteger1D
            procedure, public :: getDataAsCharacter => getDataAsCharacter1D
            procedure, public :: getDataAsLogical => getDataAsLogical1D
            procedure, public :: getDataAsComplex => getDataAsComplex1D

            ! Operators
            generic, public :: operator(.real.) => getDataAsReal
            generic, public :: operator(.dp.) => getDataAsRealDP
            generic, public :: operator(.qp.) => getDataAsRealQP
            generic, public :: operator(.integer.) => getDataAsInteger
            generic, public :: operator(.character.) => getDataAsCharacter
            generic, public :: operator(.logical.) => getDataAsLogical
            generic, public :: operator(.complex.) => getDataAsComplex
    end type

    type, public, extends(Result) :: Result2D
        private
        class(*), allocatable :: data(:,:)     !! The data returned from the function

        contains
            ! Getters for data
            procedure, public :: getData => getData2D
            procedure, public :: getDataAsReal => getDataAsReal2D
            procedure, public :: getDataAsRealDP => getDataAsRealDP2D
            procedure, public :: getDataAsRealQP => getDataAsRealQP2D
            procedure, public :: getDataAsInteger => getDataAsInteger2D
            procedure, public :: getDataAsCharacter => getDataAsCharacter2D
            procedure, public :: getDataAsLogical => getDataAsLogical2D
            procedure, public :: getDataAsComplex => getDataAsComplex2D       

            ! Operators
            generic, public :: operator(.real.) => getDataAsReal
            generic, public :: operator(.dp.) => getDataAsRealDP
            generic, public :: operator(.qp.) => getDataAsRealQP
            generic, public :: operator(.integer.) => getDataAsInteger
            generic, public :: operator(.character.) => getDataAsCharacter
            generic, public :: operator(.logical.) => getDataAsLogical
            generic, public :: operator(.complex.) => getDataAsComplex
    end type

    type, public, extends(Result) :: Result3D
        private
        class(*), allocatable :: data(:,:,:)     !! The data returned from the function

        contains
            ! Getters for data
            procedure, public :: getData => getData3D
            procedure, public :: getDataAsReal => getDataAsReal3D
            procedure, public :: getDataAsRealDP => getDataAsRealDP3D
            procedure, public :: getDataAsRealQP => getDataAsRealQP3D
            procedure, public :: getDataAsInteger => getDataAsInteger3D
            procedure, public :: getDataAsCharacter => getDataAsCharacter3D
            procedure, public :: getDataAsLogical => getDataAsLogical3D
            procedure, public :: getDataAsComplex => getDataAsComplex3D
            
            ! Operators
            generic, public :: operator(.real.) => getDataAsReal
            generic, public :: operator(.dp.) => getDataAsRealDP
            generic, public :: operator(.qp.) => getDataAsRealQP
            generic, public :: operator(.integer.) => getDataAsInteger
            generic, public :: operator(.character.) => getDataAsCharacter
            generic, public :: operator(.logical.) => getDataAsLogical
            generic, public :: operator(.complex.) => getDataAsComplex
    end type

    type, public, extends(Result) :: Result4D
        private
        class(*), allocatable :: data(:,:,:,:)     !! The data returned from the function

        contains
            ! Getters for data
            procedure, public :: getData => getData4D
            procedure, public :: getDataAsReal => getDataAsReal4D
            procedure, public :: getDataAsRealDP => getDataAsRealDP4D
            procedure, public :: getDataAsRealQP => getDataAsRealQP4D
            procedure, public :: getDataAsInteger => getDataAsInteger4D
            procedure, public :: getDataAsCharacter => getDataAsCharacter4D
            procedure, public :: getDataAsLogical => getDataAsLogical4D
            procedure, public :: getDataAsComplex => getDataAsComplex4D

            ! Operators
            generic, public :: operator(.real.) => getDataAsReal
            generic, public :: operator(.dp.) => getDataAsRealDP
            generic, public :: operator(.qp.) => getDataAsRealQP
            generic, public :: operator(.integer.) => getDataAsInteger
            generic, public :: operator(.character.) => getDataAsCharacter
            generic, public :: operator(.logical.) => getDataAsLogical
            generic, public :: operator(.complex.) => getDataAsComplex
    end type

    interface Result
        module procedure initNoData, init0D, init1D, init2D, init3D, init4D
    end interface

    contains

!-------------!
!-- No data --!
!-------------!

        !> Initialise the result object with no data.
        pure function initNoData(error, errors) result(this)
            type(Result) :: this
            type(ErrorInstance), intent(in), optional :: error
            type(ErrorInstance), intent(in), optional :: errors(:)
            ! Set the errors
            call this%setErrors(error, errors)
        end function

!--------!
!-- 0D --!
!--------!

        !> Initialise the result object with 0D polymorphic class(*) data.
        pure function init0D(data, error, errors) result(this)
            type(Result0D)                              :: this
            class(*), intent(in)                        :: data
            type(ErrorInstance), intent(in), optional   :: error
            type(ErrorInstance), intent(in), optional   :: errors(:)

            ! Store the given data in this%data and set the errors
            allocate(this%data, source=data)
            call this%setErrors(error, errors)
        end function

        !> Return the data from the Result object.
        pure function getData0D(this) result(data)
            class(Result0D), intent(in)     :: this
            class(*), allocatable           :: data
            allocate(data, source=this%data)
        end function

        !> Attempt to return the data as a real (single precision).
        !! If dp or qp variables passed as data, they will be converted
        !! to single precision.
        pure function getDataAsReal0D(this) result(data)
            class(Result0D), intent(in)     :: this
            real                            :: data
            select type (d => this%data)
                type is (real)
                    data = d
                type is (real(dp))
                    data = real(d)
                type is (real(qp))
                    data = real(d)
                type is (integer)
                    data = real(d)
                class default
                    error stop "Error trying to return 0D data as REAL. Are you sure the data is of type and kind REAL?"
            end select
        end function

        !> Attempt to return the data as real with double precision (real(dp)).
        !> If sp or qp variables passed, they will be converted implicitally.
        pure function getDataAsRealDP0D(this) result(data)
            class(Result0D), intent(in)     :: this
            real(dp)                        :: data
            select type (d => this%data)
                type is (real(dp))
                    data = d
                type is (real)
                    data = real(d, kind=dp)
                type is (real(qp))
                    data = real(d, kind=dp)
                type is (integer)
                    data = real(d, kind=dp)
                class default
                    error stop "Error trying to return 0D data as REAL(DP). Are you sure the data is of type and kind REAL(DP)?"
            end select
        end function

        !> Attempt to return the data as real with quadruple precision (real(qp)).
        !> If sp or dp variables passed, they will be converted implicitally.
        pure function getDataAsRealQP0D(this) result(data)
            class(Result0D), intent(in)     :: this
            real(qp)                        :: data
            select type (d => this%data)
                type is (real(qp))
                    data = d
                type is (real(dp))
                    data = real(d, kind=qp)
                type is (real)
                    data = real(d, kind=qp)
                type is (integer)
                    data = real(d, kind=qp)
                class default
                    error stop "Error trying to return 0D data as REAL(QP). Are you sure the data is of type and kind REAL(QP)?"
            end select
        end function

        !> Attempt to return the data as an integer. Real variable of kinds sp, dp and qp
        !! will be converted to nearest integer.
        pure function getDataAsInteger0D(this) result(data)
            class(Result0D), intent(in) :: this
            integer                     :: data
            select type (d => this%data)
                type is (integer)
                    data = d
                type is (real)
                    data = nint(d)
                type is (real(dp))
                    data = nint(d)
                type is (real(qp))
                    data = nint(d)
                class default
                    error stop "Error trying to return 0D data as INTEGER. Are you sure the data is of type and kind INTEGER?"
            end select
        end function

        !> Attempt to return the data as a character string
        pure function getDataAsCharacter0D(this) result(data)
            class(Result0D), intent(in) :: this
            character(:), allocatable   :: data
            select type (d => this%data)
                type is (character(len=*))
                    data = d
                class default
                    error stop "Error trying to return 0D data as CHARACTER. Are you sure the data is of type CHARACTER?"
            end select
        end function

        !> Attempt to return the data as logical
        pure function getDataAsLogical0D(this) result(data)
            class(Result0D), intent(in) :: this
            logical                     :: data
            select type (d => this%data)
                type is (logical)
                    data = d
                class default
                    error stop "Error trying to return 0D data as LOGICAL. Are you sure the data is of type LOGICAL?"
            end select
        end function

        !> Attempt to return the data as complex
        pure function getDataAsComplex0D(this) result(data)
            class(Result0D), intent(in) :: this
            complex :: data
            select type (d => this%data)
                type is (complex)
                    data = d
                class default
                    error stop "Error trying to return 0D data as COMPLEX. Are you sure the data is of type and kind COMPLEX?"
            end select
        end function

!--------!
!-- 1D --!
!--------!

        !> Initialise the result object with 1D polymorphic class(*) data.
        pure function init1D(data, error, errors) result(this)
            type(Result1D)                              :: this
            class(*), intent(in)                        :: data(:)
            type(ErrorInstance), intent(in), optional   :: error
            type(ErrorInstance), intent(in), optional   :: errors(:)

            ! Store the given data in this%data and set the errors
            allocate(this%data, source=data)
            call this%setErrors(error, errors)
        end function

        !> Return the data from the Result object.
        pure function getData1D(this) result(data)
            class(Result1D), intent(in) :: this
            class(*), allocatable       :: data(:)
            allocate(data, source=this%data)
        end function

        !> Attempt to return the data as a single precision real 1D array.
        !! If dp or qp data given, they will be converted to single precision.
        pure function getDataAsReal1D(this) result(data)
            class(Result1D), intent(in) :: this
            real                        :: data(size(this%data))
            select type (d => this%data)
                type is (real)
                    data = d
                type is (real(dp))
                    data = real(d)
                type is (real(qp))
                    data = real(d)
                type is (integer)
                    data = real(d)
                class default
                    error stop "Error trying to return 1D data as REAL. Are you sure the data is of type and kind REAL?"
            end select
        end function

        !> Attempt to return the data as 1D real with double precision (real(dp)).
        !> If sp or qp variables passed, they will be converted implicitally.
        pure function getDataAsRealDP1D(this) result(data)
            class(Result1D), intent(in) :: this
            real(dp)                    :: data(size(this%data))
            select type (d => this%data)
                type is (real(dp))
                    data = d
                type is (real)
                    data = real(d, kind=dp)
                type is (real(qp))
                    data = real(d, kind=dp)
                type is (integer)
                    data = real(d, kind=dp)
                class default
                    error stop "Error trying to return 1D data as REAL(DP). Are you sure the data is of type and kind REAL(DP)?"
            end select
        end function

        !> Attempt to return the data as 1D real with quadruple precision (real(dp)).
        !> If sp or dp variables passed, they will be converted implicitally.
        pure function getDataAsRealQP1D(this) result(data)
            class(Result1D), intent(in) :: this
            real(qp)                    :: data(size(this%data))
            select type (d => this%data)
                type is (real(qp))
                    data = d
                type is (real)
                    data = real(d, kind=qp)
                type is (real(dp))
                    data = real(d, kind=qp)
                type is (integer)
                    data = real(d, kind=qp)
                class default
                    error stop "Error trying to return 1D data as REAL(QP). Are you sure the data is of type and kind REAL(QP)?"
            end select
        end function

        !> Attempt to return the data as a 1D integer array. Reals of kind sp,
        !! dp and qp will be converted to nearest integer.
        pure function getDataAsInteger1D(this) result(data)
            class(Result1D), intent(in) :: this
            integer                     :: data(size(this%data))
            select type (d => this%data)
                type is (integer)
                    data = d
                type is (real)
                    data = nint(d)
                type is (real(dp))
                    data = nint(d)
                type is (real(qp))
                    data = nint(d)
                class default
                    error stop "Error trying to return 1D data as INTEGER. Are you sure the data is of type INTEGER?"
            end select
        end function

        !> Attempt to return the data as a 1D character array
        pure function getDataAsCharacter1D(this) result(data)
            class(Result1D), intent(in) :: this
            character(:), allocatable   :: data(:)
            select type (d => this%data)
                type is (character(len=*))
                    data = d
                class default
                    error stop "Error trying to return 1D data as CHARACTER. Are you sure the data is of type CHARACTER?"
            end select
        end function

        !> Attempt to return the data as a 1D logical array
        pure function getDataAsLogical1D(this) result(data)
            class(Result1D), intent(in) :: this
            logical                     :: data(size(this%data))
            select type (d => this%data)
                type is (logical)
                    data = d
                class default
                    error stop "Error trying to return 1D data as LOGICAL. Are you sure the data is of type LOGICAL?"
            end select
        end function

        !> Attempt to return the data as a 1D complex array
        pure function getDataAsComplex1D(this) result(data)
            class(Result1D), intent(in) :: this
            complex                     :: data(size(this%data))
            select type (d => this%data)
                type is (complex)
                    data = d
                class default
                    error stop "Error trying to return 1D data as COMPLEX. Are you sure the data is of type and kind COMPLEX?"
            end select
        end function

!--------!
!-- 2D --!
!--------!

        !> Initialise the result object with 2D polymorphic class(*) data.
        pure function init2D(data, error, errors) result(this)
            type(Result2D)                              :: this
            class(*), intent(in)                        :: data(:,:)
            type(ErrorInstance), intent(in), optional   :: error
            type(ErrorInstance), intent(in), optional   :: errors(:)

            ! Store the given data in this%data and set the errors
            allocate(this%data, source=data)
            call this%setErrors(error, errors)
        end function

        !> Return the data from the Result object.
        pure function getData2D(this) result(data)
            class(Result2D), intent(in) :: this
            class(*), allocatable       :: data(:,:)
            allocate(data, source=this%data)
        end function

        !> Attempt to return the data as a real 2D array with single precision.
        !! dp and qp data will be converted to sp.
        pure function getDataAsReal2D(this) result(data)
            class(Result2D), intent(in) :: this
            real                        :: data(size(this%data,1),size(this%data,2))
            select type (d => this%data)
                type is (real)
                    data = d
                type is (real(dp))
                    data = real(d)
                type is (real(qp))
                    data = real(d)
                type is (integer)
                    data = real(d)
                class default
                    error stop "Error trying to return 2D data as REAL. Are you sure the data is of type and kind REAL?"
            end select
        end function

        !> Attempt to return the data as 2D real with double precision (real(dp)).
        !! If sp or qp variables passed, they will be converted implicitally.
        pure function getDataAsRealDP2D(this) result(data)
            class(Result2D), intent(in) :: this
            real(dp)                    :: data(size(this%data,1),size(this%data,2))
            select type (d => this%data)
                type is (real(dp))
                    data = d
                type is (real)
                    data = real(d, kind=dp)
                type is (real(qp))
                    data = real(d, kind=dp)
                type is (integer)
                    data = real(d, kind=dp)
                class default
                    error stop "Error trying to return 2D data as REAL(DP). Are you sure the data is of type and kind REAL(DP)?"
            end select
        end function

        !> Attempt to return the data as 2D real with quadruple precision (real(qp)).
        !! If sp or dp variables passed, they will be converted implicitally.
        pure function getDataAsRealQP2D(this) result(data)
            class(Result2D), intent(in) :: this
            real(qp)                    :: data(size(this%data,1),size(this%data,2))
            select type (d => this%data)
                type is (real(qp))
                    data = d
                type is (real)
                    data = real(d, kind=qp)
                type is (real(dp))
                    data = real(d, kind=qp)
                type is (integer)
                    data = real(d, kind=qp)
                class default
                    error stop "Error trying to return 2D data as REAL(QP). Are you sure the data is of type and kind REAL(QP)?"
            end select
        end function

        !> Attempt to return the data as a 2D integer array
        pure function getDataAsInteger2D(this) result(data)
            class(Result2D), intent(in) :: this
            integer                     :: data(size(this%data,1),size(this%data,2))
            select type (d => this%data)
                type is (integer)
                    data = d
                type is (real)
                    data = nint(d)
                type is (real(dp))
                    data = nint(d)
                type is (real(qp))
                    data = nint(d)
                class default
                    error stop "Error trying to return 2D data as INTEGER. Are you sure the data is of type INTEGER?"
            end select
        end function

        !> Attempt to return the data as a 2D character array
        pure function getDataAsCharacter2D(this) result(data)
            class(Result2D), intent(in) :: this
            character(:), allocatable   :: data(:,:)
            select type (d => this%data)
                type is (character(len=*))
                    data = d
                class default
                    error stop "Error trying to return 2D data as CHARACTER. Are you sure the data is of type CHARACTER?"
            end select
        end function

        !> Attempt to return the data as a 2D logical array
        pure function getDataAsLogical2D(this) result(data)
            class(Result2D), intent(in) :: this
            logical                     :: data(size(this%data,1),size(this%data,2))
            select type (d => this%data)
                type is (logical)
                    data = d
                class default
                    error stop "Error trying to return 2D data as LOGICAL. Are you sure the data is of type LOGICAL?"
            end select
        end function

        !> Attempt to return the data as a 2D complex array
        pure function getDataAsComplex2D(this) result(data)
            class(Result2D), intent(in) :: this
            complex                     :: data(size(this%data,1),size(this%data,2))
            select type (d => this%data)
                type is (complex)
                    data = d
                class default
                    error stop "Error trying to return 2D data as COMPLEX. Are you sure the data is of type and kind COMPLEX?"
            end select
        end function

!--------!
!-- 3D --!
!--------!

        !> Initialise the result object with 3D polymorphic class(*) data.
        pure function init3D(data, error, errors) result(this)
            type(Result3D)                              :: this
            class(*), intent(in)                        :: data(:,:,:)
            type(ErrorInstance), intent(in), optional   :: error
            type(ErrorInstance), intent(in), optional   :: errors(:)

            ! Store the given data in this%data and set the errors
            allocate(this%data, source=data)
            call this%setErrors(error, errors)
        end function

        !> Return the data from the Result object.
        pure function getData3D(this) result(data)
            class(Result3D), intent(in) :: this
            class(*), allocatable       :: data(:,:,:)
            allocate(data, source=this%data)
        end function

        !> Attempt to return the data as 3D real with single precision (real).
        !! If dp or qp variables passed, they will be converted implicitally.
        pure function getDataAsReal3D(this) result(data)
            class(Result3D), intent(in) :: this
            real                        :: data(size(this%data,1), &
                                                size(this%data,2), &
                                                size(this%data,3))
            select type (d => this%data)
                type is (real)
                    data = d
                type is (real(dp))
                    data = real(d)
                type is (real(qp))
                    data = real(d)
                type is (integer)
                    data = real(d)
                class default
                    error stop "Error trying to return 3D data as REAL. Are you sure the data is of type and kind REAL?"
            end select
        end function

        !> Attempt to return the data as 3D real with double precision (real(dp)).
        !! If sp or qp variables passed, they will be converted implicitally.
        pure function getDataAsRealDP3D(this) result(data)
            class(Result3D), intent(in) :: this
            real(dp)                    :: data(size(this%data,1), &
                                                size(this%data,2), &
                                                size(this%data,3))
            select type (d => this%data)
                type is (real(dp))
                    data = d
                type is (real)
                    data = real(d, kind=dp)
                type is (real(qp))
                    data = real(d, kind=dp)
                type is (integer)
                    data = real(d, kind=dp)
                class default
                    error stop "Error trying to return 3D data as REAL(DP). Are you sure the data is of type and kind REAL(DP)?"
            end select
        end function

        !> Attempt to return the data as 3D real with quadruple precision (real(qp)).
        !! If sp or dp variables passed, they will be converted implicitally.
        pure function getDataAsRealQP3D(this) result(data)
            class(Result3D), intent(in) :: this
            real(qp)                    :: data(size(this%data,1), &
                                                size(this%data,2), &
                                                size(this%data,3))
            select type (d => this%data)
                type is (real(qp))
                    data = d
                type is (real)
                    data = real(d, kind=qp)
                type is (real(dp))
                    data = real(d, kind=qp)
                type is (integer)
                    data = real(d, kind=qp)
                class default
                    error stop "Error trying to return 3D data as REAL(QP). Are you sure the data is of type and kind REAL(QP)?"
            end select
        end function

        !> Attempt to return the data as a 3D integer array
        pure function getDataAsInteger3D(this) result(data)
            class(Result3D), intent(in) :: this
            integer                     :: data(size(this%data,1), &
                                                size(this%data,2), &
                                                size(this%data,3))
            select type (d => this%data)
                type is (integer)
                    data = d
                type is (real)
                    data = nint(d)
                class default
                    error stop "Error trying to return 3D data as INTEGER. Are you sure the data is of type INTEGER?"
            end select
        end function

        !> Attempt to return the data as a 3D character array
        pure function getDataAsCharacter3D(this) result(data)
            class(Result3D), intent(in) :: this
            character(:), allocatable   :: data(:,:,:)
            select type (d => this%data)
                type is (character(len=*))
                    data = d
                class default
                    error stop "Error trying to return 3D data as CHARACTER. Are you sure the data is of type CHARACTER?"
            end select
        end function

        !> Attempt to return the data as a 3D logical array
        pure function getDataAsLogical3D(this) result(data)
            class(Result3D), intent(in) :: this
            logical                     :: data(size(this%data,1), &
                                                size(this%data,2), &
                                                size(this%data,3))
            select type (d => this%data)
                type is (logical)
                    data = d
                class default
                    error stop "Error trying to return 3D data as LOGICAL. Are you sure the data is of type LOGICAL?"
            end select
        end function

        !> Attempt to return the data as 3D complex array
        pure function getDataAsComplex3D(this) result(data)
            class(Result3D), intent(in) :: this
            complex                     :: data(size(this%data,1), &
                                                size(this%data,2), &
                                                size(this%data,3))
            select type (d => this%data)
                type is (complex)
                    data = d
                class default
                    error stop "Error trying to return 3D data as COMPLEX. Are you sure the data is of type and kind COMPLEX?"
            end select
        end function


!--------!
!-- 4D --!
!--------!

        !> Initialise the result object with polymorphic class(*) data.
        pure function init4D(data, error, errors) result(this)
            type(Result4D)                              :: this
            class(*), intent(in)                        :: data(:,:,:,:)
            type(ErrorInstance), intent(in), optional   :: error
            type(ErrorInstance), intent(in), optional   :: errors(:)

            ! Store the given data in this%data and set the errors
            allocate(this%data, source=data)
            call this%setErrors(error, errors)
        end function

        !> Return the data from the Result object.
        pure function getData4D(this) result(data)
            class(Result4D), intent(in) :: this
            class(*), allocatable       :: data(:,:,:,:)
            allocate(data, source=this%data)
        end function

        !> Attempt to return the data as 4D real with single precision (real).
        !! If dp or qp variables passed, they will be converted implicitally.
        pure function getDataAsReal4D(this) result(data)
            class(Result4D), intent(in) :: this
            real                        :: data(size(this%data,1), &
                                                size(this%data,2), &
                                                size(this%data,3), &
                                                size(this%data,4))
            select type (d => this%data)
                type is (real)
                    data = d
                type is (real(dp))
                    data = real(d)
                type is (real(qp))
                    data = real(d)
                type is (integer)
                    data = real(d)
                class default
                    error stop "Error trying to return 4D data as REAL. Are you sure the data is of type and kind REAL?"
            end select
        end function

        !> Attempt to return the data as 4D real with double precision (real(dp)).
        !! If sp or qp variables passed, they will be converted implicitally.
        pure function getDataAsRealDP4D(this) result(data)
            class(Result4D), intent(in) :: this
            real(dp)                    :: data(size(this%data,1), &
                                                size(this%data,2), &
                                                size(this%data,3), &
                                                size(this%data,4))
            select type (d => this%data)
                type is (real(dp))
                    data = d
                type is (real)
                    data = real(d, kind=dp)
                type is (real(qp))
                    data = real(d, kind=dp)
                type is (integer)
                    data = real(d, kind=dp)
                class default
                    error stop "Error trying to return 4D data as REAL(DP). Are you sure the data is of type and kind REAL(DP)?"
            end select
        end function

        !> Attempt to return the data as 4D real with double precision (real(qp)).
        !! If sp or dp variables passed, they will be converted implicitally.
        pure function getDataAsRealQP4D(this) result(data)
            class(Result4D), intent(in) :: this
            real(qp)                    :: data(size(this%data,1), &
                                                size(this%data,2), &
                                                size(this%data,3), &
                                                size(this%data,4))
            select type (d => this%data)
                type is (real(qp))
                    data = d
                type is (real)
                    data = real(d, kind=qp)
                type is (real(dp))
                    data = real(d, kind=qp)
                type is (integer)
                    data = real(d, kind=qp)
                class default
                    error stop "Error trying to return 4D data as REAL(QP). Are you sure the data is of type and kind REAL(QP)?"
            end select
        end function

        !> Attempt to return the data as a 4D real array
        pure function getDataAsInteger4D(this) result(data)
            class(Result4D), intent(in) :: this
            integer                     :: data(size(this%data,1), &
                                                size(this%data,2), &
                                                size(this%data,3), &
                                                size(this%data,4))
            select type (d => this%data)
                type is (integer)
                    data = d
                type is (real)
                    data = nint(d)
                class default
                    error stop "Error trying to return 4D data as INTEGER. Are you sure the data is of type INTEGER?"
            end select
        end function

        !> Attempt to return the data as a 4D character array
        pure function getDataAsCharacter4D(this) result(data)
            class(Result4D), intent(in) :: this
            character(:), allocatable   :: data(:,:,:,:)
            select type (d => this%data)
                type is (character(len=*))
                    data = d
                class default
                    error stop "Error trying to return 4D data as CHARACTER. Are you sure the data is of type CHARACTER?"
            end select
        end function

        !> Attempt to return the data as a 4D logical array
        pure function getDataAsLogical4D(this) result(data)
            class(Result4D), intent(in) :: this
            logical                     :: data(size(this%data,1), &
                                                size(this%data,2), &
                                                size(this%data,3), &
                                                size(this%data,4))
            select type (d => this%data)
                type is (logical)
                    data = d
                class default
                    error stop "Error trying to return 4D data as LOGICAL. Are you sure the data is of type LOGICAL?"
            end select
        end function

        !> Attempt to return the data as a 4D complex array
        pure function getDataAsComplex4D(this) result(data)
            class(Result4D), intent(in) :: this
            complex                     :: data(size(this%data,1), &
                                                size(this%data,2), &
                                                size(this%data,3), &
                                                size(this%data,4))
            select type (d => this%data)
                type is (complex)
                    data = d
                class default
                    error stop "Error trying to return 4D data as COMPLEX. Are you sure the data is of type and kind COMPLEX?"
            end select
        end function

!------------!
!-- ERRORS --!
!------------!

        !> Returns the error code from the first error in the errors array.
        pure function getErrorCode(this) result(errorCode)
            class(Result), intent(in)   :: this
            integer                     :: errorCode
            errorCode = this%errors(1)%getCode()
        end function

        !> Returns the first error from the errors array.
        pure function getError(this) result(error)
            class(Result), intent(in)   :: this
            type(ErrorInstance)         :: error
            if (size(this%errors)>0) error = this%errors(1)
        end function

        !> Return the errors array.
        pure function getErrors(this) result(errors)
            class(Result), intent(in)   :: this
            type(ErrorInstance)         :: errors(size(this%errors))
            if (size(this%errors)>0) errors = this%errors
        end function

        !> Set the array of ErrorInstances from a single error
        !! or array of errors.
        pure subroutine setErrors(this, error, errors)
            class(Result), intent(inout)                :: this
            type(ErrorInstance), optional, intent(in)   :: error
            type(ErrorInstance), optional, intent(in)   :: errors(:)

            ! Allocate array of errors, if it isn't already
            if (.not. allocated(this%errors)) allocate(this%errors(0))
            
            ! If errors array given as param, use that, otherwise
            ! use error param, and if that's not there, use the
            ! default "no error" error.
            if (present(errors)) then
                this%errors = errors
            else if (present(error)) then
                this%errors = [error]
            else
                this%errors = [ErrorInstance(0, "No error.", .false.)]
            end if
        end subroutine

        ! Add the same trace message to all errors in a Result object
        pure subroutine addToTrace(this, traceMessage)
            class(Result), intent(inout)    :: this             !> The Result instance
            character(len=*), intent(in)    :: traceMessage     !> Message to add to trace
            integer                         :: i                !> Loop iterator
            ! Loop through the errors and add the trace message one-by-one
            do i=1, size(this%errors)
                call this%errors(i)%addToTrace(traceMessage)
            end do
        end subroutine

end module
