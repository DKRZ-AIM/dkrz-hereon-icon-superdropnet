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
  END type

END MODULE mo_type
