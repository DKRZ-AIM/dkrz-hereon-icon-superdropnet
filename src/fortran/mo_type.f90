MODULE mo_type
  IMPLICIT NONE
  ! provide the necessary derived types

  TYPE :: t_scalar_field_1d
    ! a 1D scalar field
    ! phi(x) with array size len(x) = k
    INTEGER :: k                     ! array length
    REAL, DIMENSION(100) :: x          ! grid coordinate
    REAL, DIMENSION(100) :: phi        ! scalar field
  END type

END MODULE mo_type
