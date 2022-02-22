PROGRAM routine

  USE mo_type, ONLY: t_scalar_field_1d
  USE mo_function, ONLY: f_scalar_field_1d, f_linspace

  IMPLICIT NONE 

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
  TYPE(t_scalar_field_1d) :: scalar_field_1d

  ! (2) set default values
  nx = 11
  xmin = -5.0
  xmax = +5.0

  ! (3) initialize the arrays
  scalar_field_1d % k = nx

  ! allocate them here
  ! later we want to have allocated arrays to
  ! share with the python routines
  ALLOCATE(scalar_field_1d % x(nx))
  ALLOCATE(scalar_field_1d % phi(nx))

  CALL f_linspace(nx, xmin, xmax, scalar_field_1d % x)

  ! (4) calculate the field value by a function
  CALL f_scalar_field_1d(nx, scalar_field_1d % x, scalar_field_1d % phi)

  ! (5) print to stdout
  PRINT *, '   1D scalar field'
  PRINT *, '     (X)', '          (PHI)'
  DO i=1, nx
    PRINT *, scalar_field_1d % x(i), scalar_field_1d % phi(i)
  END DO

END PROGRAM routine
