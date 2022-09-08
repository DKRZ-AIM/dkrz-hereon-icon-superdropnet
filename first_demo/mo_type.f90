MODULE mo_type
  IMPLICIT NONE
  ! provide the necessary derived types

  TYPE :: t_scalar_field_1d
    ! a 1D scalar field phi(x)
    INTEGER :: k = 5                   ! array length
    REAL    :: xmin = 0                ! min grid coordinate
    REAL    :: xmax = 10               ! max grid coordinate
    REAL, ALLOCATABLE :: x(:)          ! coordinate array
    REAL, ALLOCATABLE :: phi(:)        ! scalar field array
  END TYPE

  TYPE :: t_scalar_field_2d
    ! a 2D scalar field phi(x1, x2)
    INTEGER :: k1 = 5                  ! array length
    REAL    :: x1min = 0               ! min grid coordinate
    REAL    :: x1max = 10              ! max grid coordinate
    INTEGER :: k2 = 5                  ! array length
    REAL    :: x2min = 0               ! min grid coordinate
    REAL    :: x2max = 10              ! max grid coordinate
    REAL, ALLOCATABLE :: x1(:,:)       ! coordinate array (x1)
    REAL, ALLOCATABLE :: x2(:,:)       ! coordinate array (x2)
    REAL, ALLOCATABLE :: phi(:,:)      ! scalar field array

  END TYPE

  TYPE :: t_scalar_field_3d
    ! a 3D scalar field phi(x1, x2, x3)
    INTEGER :: k1 = 5                  ! array length
    REAL    :: x1min = 0               ! min grid coordinate
    REAL    :: x1max = 10              ! max grid coordinate
    INTEGER :: k2 = 5                  ! array length
    REAL    :: x2min = 0               ! min grid coordinate
    REAL    :: x2max = 10              ! max grid coordinate
    INTEGER :: k3 = 5                  ! array length
    REAL    :: x3min = 0               ! min grid coordinate
    REAL    :: x3max = 10              ! max grid coordinate
    REAL, ALLOCATABLE :: x1(:,:,:)       ! coordinate array (x1)
    REAL, ALLOCATABLE :: x2(:,:,:)       ! coordinate array (x2)
    REAL, ALLOCATABLE :: x3(:,:,:)       ! coordinate array (x3)
    REAL, ALLOCATABLE :: phi(:,:,:)      ! scalar field array

  END TYPE

END MODULE mo_type
