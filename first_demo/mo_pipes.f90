MODULE mo_pipes

USE, intrinsic :: iso_c_binding

IMPLICIT NONE


INTERFACE 
  
  SUBROUTINE ip_init_pipes(mpi_rank, mpi_world_size) bind(C, NAME='ip_init_pipes')
    USE iso_c_binding
    INTEGER(c_int) :: mpi_rank
    INTEGER(c_int) :: mpi_world_size
  END SUBROUTINE ip_init_pipes
  
  SUBROUTINE ip_close_pipes() bind(C, NAME='ip_close_pipes')
  END SUBROUTINE ip_close_pipes
  
  SUBROUTINE ip_scalar_field_1d(nx, x, phi) bind(C, NAME='ip_scalar_field_1d')
    USE, INTRINSIC :: iso_c_binding, only: c_int, c_float
    REAL(c_float) :: x(nx), phi(nx) 
    INTEGER(c_int) :: nx
  END SUBROUTINE ip_scalar_field_1d

  SUBROUTINE ip_scalar_field_2d(nx1, nx2, x1, x2, phi) bind(C, NAME='ip_scalar_field_2d')
    USE iso_c_binding
    INTEGER(c_int) :: nx1, nx2
    REAL(c_float) :: x1(nx1, nx2), x2(nx1, nx2), phi(nx1, nx2) 
  END SUBROUTINE ip_scalar_field_2d

END INTERFACE

END MODULE mo_pipes