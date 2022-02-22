MODULE mo_function
  IMPLICIT NONE

CONTAINS

  SUBROUTINE f_linspace(nx, xmin, xmax, x)
    ! like numpy linspace
    ! produces an array x of nx evenly spaced samples
    ! in the interval [xmin, xmax]

    INTEGER, INTENT(in) :: nx         ! grid size
    REAL, INTENT(in)    :: xmin, xmax ! grid limits
    REAL, INTENT(out)   :: x(nx)      ! grid array

    INTEGER :: i
    REAL :: dx

    dx = (xmax - xmin) / (nx - 1.0)

    DO i=1,nx
      x(i) = xmin + (i-1.0)*dx
    END DO

  END SUBROUTINE f_linspace

  SUBROUTINE f_scalar_field_1d(nx, x, phi)
    ! calculate a 1d scalar field
    ! given by the function
    !   phi(x) = 0.5 * x**2

    INTEGER, INTENT(in) :: nx       ! grid size
    REAL, INTENT(in)    :: x(nx)    ! coordinates
    REAL, INTENT(out)   :: phi(nx)  ! scalar field
    
    INTEGER :: i

    DO i=1,nx
      phi(i) = 0.5 * x(i) * x(i)
    END DO

  END SUBROUTINE f_scalar_field_1d



END MODULE mo_function 
