PROGRAM routine

  USE, INTRINSIC :: iso_c_binding ! for cffi

  USE mo_type, ONLY: t_scalar_field_1d
  USE mo_function, ONLY: f_scalar_field_1d, f_linspace

  IMPLICIT NONE 

  ! -----------------------------------------------------------------
  ! https://www.noahbrenowitz.com/post/calling-fortran-from-python/
  INTERFACE
    SUBROUTINE i_hello_world() bind(c)
    END SUBROUTINE i_hello_world

    SUBROUTINE i_scalar_field_1d(nx, x, phi) bind(c)
      USE iso_c_binding
      INTEGER(c_int) :: nx
      REAL(c_float) :: x(nx), phi(nx) 
    END SUBROUTINE i_scalar_field_1d

  END INTERFACE
  ! -----------------------------------------------------------------

  ! the main program
  ! call subroutines from here 

  ! sketch
  ! - define a field with variables
  ! - define routines to get / set these variables
  ! - define a function that operates on the variables /
  !   calculates something based on the variables
  ! - later replace this function by a python routine

  INTEGER :: i     ! loop idxs
  INTEGER :: nx    ! grid dimension
  REAL    :: xmin, xmax ! grid limits
  
  ! (1) define a 1d scalar field
  TYPE(t_scalar_field_1d) :: sf_1d_fo ! fortran
  TYPE(t_scalar_field_1d) :: sf_1d_py ! python

  ! test for the interface
  CALL i_hello_world()

  ! (2) set default values
  nx = 11
  xmin = -5.0
  xmax = +5.0

  ! (3) initialize the arrays
  sf_1d_fo % k = nx

  ! allocate them here
  ! later we want to have allocated arrays to
  ! share with the python routines
  ALLOCATE(sf_1d_fo % x(nx))
  ALLOCATE(sf_1d_fo % phi(nx))
  ALLOCATE(sf_1d_py % x(nx))
  ALLOCATE(sf_1d_py % phi(nx))

  CALL f_linspace(nx, xmin, xmax, sf_1d_fo % x)
  CALL f_linspace(nx, xmin, xmax, sf_1d_py % x)

  ! (4) calculate the field value by a function
  ! (4A) Fortran implementation
  CALL f_scalar_field_1d(nx, sf_1d_fo % x, sf_1d_fo % phi)

  ! (4B) Python implementation
  CALL i_scalar_field_1d(nx, sf_1d_py % x, sf_1d_py % phi)

  ! (5) print to stdout
  PRINT *, '   1D scalar field'
  PRINT *, '     (X)', '  Fortran (PHI)', '  Python (PHI)'
  DO i=1, nx
    PRINT *, sf_1d_fo % x(i), sf_1d_fo % phi(i), sf_1d_py % phi(i)
  END DO

END PROGRAM routine
