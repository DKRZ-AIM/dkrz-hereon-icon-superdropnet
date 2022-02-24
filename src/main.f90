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
    
    SUBROUTINE i_print_shape(nx) bind(c)
      USE iso_c_binding
      INTEGER(c_int) :: nx
    END SUBROUTINE i_print_shape

    SUBROUTINE i_print_value(x) bind(c)
      USE iso_c_binding
      REAL(c_float) :: x
    END SUBROUTINE i_print_value

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

  INTEGER        :: i     ! loop idxs
  INTEGER        :: nx    ! grid dimension # TODO check
  REAL           :: xmin, xmax ! grid limits
  ! wall-time measurements
  INTEGER        :: ic1, ic2, cmax ! system clock
  REAL           :: crate
  INTEGER        :: repeats

  
  ! (1) define a 1d scalar field
  TYPE(t_scalar_field_1d) :: sf_1d_fo ! fortran
  TYPE(t_scalar_field_1d) :: sf_1d_py ! python

  ! test for the interface
  CALL i_hello_world()

  ! (2) set default values
  nx = 100
  xmin = -5.0
  xmax = +5.0

  repeats = 1000
 
  ! test for the interface
  PRINT *, 'shape in fortran', nx
  CALL i_print_shape(nx)
  CALL i_print_value(xmin)

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
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL f_scalar_field_1d(nx, sf_1d_fo % x, sf_1d_fo % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Fortran function call (seconds)', (ic2 - ic1) /crate  / repeats

  ! (4B) Python implementation
  sf_1d_py % phi(:) = 0.0 ! initialize to zero
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL i_scalar_field_1d(nx, sf_1d_py % x, sf_1d_py % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Python function call  (seconds)', (ic2 - ic1) /crate  / repeats

  ! (5) print to stdout
  PRINT *, '   1D scalar field (head)'
  PRINT *, '     (X)', '  Fortran (PHI)', '  Python (PHI)'
  DO i=1, 10
    PRINT *, sf_1d_fo % x(i), sf_1d_fo % phi(i), sf_1d_py % phi(i)
  END DO

  ! (6) check the arrays are equal
  IF(ALL(sf_1d_fo % phi .EQ. sf_1d_py % phi)) THEN
    PRINT *, ' Fortran and Python scalar field are equal'
  ELSE
    PRINT *, ' Fortran and Python scalar field are **NOT** equal'
  ENDIF


  ! (7) deallocate
  DEALLOCATE(sf_1d_fo % x)
  DEALLOCATE(sf_1d_fo % phi)
  DEALLOCATE(sf_1d_py % x)
  DEALLOCATE(sf_1d_py % phi)

END PROGRAM routine
