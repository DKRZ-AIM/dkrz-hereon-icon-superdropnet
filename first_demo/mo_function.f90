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

    IF (nx .EQ. 1) THEN
      dx = 0
    ELSE
      dx = (xmax - xmin) / (nx - 1.0)
    ENDIF

    DO i=1,nx
      x(i) = xmin + (i-1.0)*dx
    END DO

  END SUBROUTINE f_linspace

  SUBROUTINE f_meshgrid(nx1, nx2, x1min, x1max, x2min, x2max, x1, x2)
    ! like numpy meshgrid
    ! produces 2D coordinate arrays x1(nx1, nx2), x2(nx1, nx2)

    INTEGER, INTENT(in)  :: nx1, nx2                   ! grid sizes 
    REAL,    INTENT(in)  :: x1min, x1max, x2min, x2max ! grid limits
    REAL,    INTENT(out) :: x1(nx1, nx2), x2(nx1, nx2) ! 2D grid arrays
    
    INTEGER :: i, j
    REAL    :: a_x1(nx1), a_x2(nx2)

    CALL f_linspace(nx1, x1min, x1max, a_x1)
    CALL f_linspace(nx2, x2min, x2max, a_x2)

    DO j=1,nx2
      DO i=1,nx1
        x1(i, j) = a_x1(i)
        x2(i, j) = a_x2(j)
      END DO
    END DO

  END SUBROUTINE f_meshgrid

  SUBROUTINE f_spacegrid(nx1, nx2, nx3, x1min, x1max, x2min, x2max, x3min, x3max, x1, x2, x3)
    ! produces 3D coordinate arrays x1(nx1, nx2, nx3), x2(nx1, nx2, nx3), x3(nx1, nx2, nx3)

    INTEGER, INTENT(in)  :: nx1, nx2, nx3                   ! grid sizes 
    REAL,    INTENT(in)  :: x1min, x1max, x2min, x2max, x3min, x3max ! grid limits
    REAL,    INTENT(out) :: x1(nx1, nx2, nx3), x2(nx1, nx2, nx3), x3(nx1, nx2, nx3) ! 3D grid arrays
    
    INTEGER :: i, j, k
    REAL    :: a_x1(nx1), a_x2(nx2), a_x3(nx3)

    CALL f_linspace(nx1, x1min, x1max, a_x1)
    CALL f_linspace(nx2, x2min, x2max, a_x2)
    CALL f_linspace(nx3, x3min, x3max, a_x3)

    DO k=1,nx3
      DO j=1,nx2
        DO i=1,nx1
          x1(i, j, k) = a_x1(i)
          x2(i, j, k) = a_x2(j)
          x3(i, j, k) = a_x3(k)
        END DO
      END DO
    END DO

  END SUBROUTINE f_spacegrid

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

  SUBROUTINE f_scalar_field_2d(nx1, nx2, x1, x2, phi)
    ! calculate a 2d scalar field
    ! given by the function
    !   phi(x) = 0.5 * (x-1)**2 + y**2 + 2 * y

    INTEGER, INTENT(in) :: nx1, nx2                    ! grid size
    REAL, INTENT(in)    :: x1(nx1, nx2), x2(nx1, nx2)  ! coordinates
    REAL, INTENT(out)   :: phi(nx1, nx2)               ! scalar field
    
    INTEGER :: i, j

    phi(:,:) = 0.0

    DO j=1, nx2
      DO i=1, nx1
        phi(i, j) = 0.5 * (x1(i, j) - 1.0) * (x1(i, j) - 1.0) + &
        &           x2(i, j) * x2(i, j) + 2 * x2(i, j)
      END DO
    END DO

  END SUBROUTINE f_scalar_field_2d

  SUBROUTINE f_scalar_field_3d(nx1, nx2, nx3, x1, x2, x3, phi)
    ! calculate a 3d scalar field
    ! given by the function
    !   phi(x) = 0.5 * (x-1)**2 + y**2 + 2 * y + 7 * z

    INTEGER, INTENT(in) :: nx1, nx2, nx3                    ! grid size
    REAL, INTENT(in)    :: x1(nx1, nx2, nx3), x2(nx1, nx2, nx3), x3(nx1, nx2, nx3)  ! coordinates
    REAL, INTENT(out)   :: phi(nx1, nx2, nx3)               ! scalar field
    
    INTEGER :: i, j, k

    phi(:,:,:) = 0.0

    DO k=1, nx3
      DO j=1, nx2
        DO i=1, nx1
          phi(i,j,k) = 0.5 * (x1(i,j,k) - 1.0) * (x1(i,j,k) - 1.0) + &
          &           x2(i,j,k) * x2(i,j,k) + 2 * x2(i,j,k) + 7 * x3(i,j,k)
        END DO
      END DO
    END DO

  END SUBROUTINE f_scalar_field_3d

  SUBROUTINE f_scalar_field_index_3d(nx1, nx2, nx3, x1, x2, x3, phi)
    ! calculate a 3d scalar field
    ! given by the function
    !   phi(x) = 0.5 * (x-1)**2 + y**2 + 2 * y + 7 * z

    INTEGER, INTENT(in) :: nx1, nx2, nx3                    ! grid size
    REAL, INTENT(in)    :: x1(nx1, nx2, nx3), x2(nx1, nx2, nx3), x3(nx1, nx2, nx3)  ! coordinates
    REAL, INTENT(out)   :: phi(nx1, nx2, nx3)               ! scalar field
    
    INTEGER :: i, j, k

    phi(:,:,:) = 0.0

    DO k=1, nx3
      DO j=1, nx2
        DO i=1, nx1
          phi(i,j,k) = 100 * i + 10 * j + 1 * k
        END DO
      END DO
    END DO

  END SUBROUTINE f_scalar_field_index_3d

END MODULE mo_function 
