PROGRAM routine

  USE, INTRINSIC :: iso_c_binding ! for cffi
  USE mpi

  USE mo_type, ONLY: t_scalar_field_1d, t_scalar_field_2d
  USE mo_function, ONLY: f_scalar_field_1d, f_linspace, &
  &                      f_scalar_field_2d, f_meshgrid

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

    SUBROUTINE i_scalar_field_2d(nx1, nx2, x1, x2, phi) bind(c)
      USE iso_c_binding
      INTEGER(c_int) :: nx1, nx2
      REAL(c_float) :: x1(nx1, nx2), x2(nx1, nx2), phi(nx1, nx2) 
    END SUBROUTINE i_scalar_field_2d

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

  INTEGER        :: i, j  ! loop idxs
  INTEGER        :: nx1, nx2    ! grid dimension # TODO check
  REAL           :: x1min, x1max ! grid limits
  REAL           :: x2min, x2max
  ! wall-time measurements
  INTEGER        :: ic1, ic2, cmax ! system clock
  REAL           :: crate
  INTEGER        :: repeats

  ! FORTRAN mpi side
  INTEGER(KIND=4) :: error
  INTEGER(KIND=4) :: mpi_rank
  INTEGER(KIND=4) :: mpi_size

  ! Derived types
  TYPE(t_scalar_field_1d) :: sf_1d_fo ! fortran
  TYPE(t_scalar_field_1d) :: sf_1d_py ! python
    
  TYPE(t_scalar_field_2d) :: sf_2d_fo ! fortran
  TYPE(t_scalar_field_2d) :: sf_2d_py ! python

  ! ------------------------------------------------------------
  !
  ! Demo: 1D scalar field
  !
  ! ------------------------------------------------------------
   
  PRINT *, '--------------------------------------------'
  PRINT *, '       DEMO 1D Scalar Field                 '
  PRINT *, '--------------------------------------------'
  
  ! (1) test for the interface
  CALL i_hello_world()

  ! (2) set default values
  nx1 = 10000
  x1min = -5.0
  x1max = +5.0

  repeats = 3
 
  ! test for the interface
  PRINT *, 'shape in fortran', nx1
  CALL i_print_shape(nx1)
  CALL i_print_value(x1min)

  ! (3) initialize the arrays
  sf_1d_fo % k = nx1

  ! allocate them here
  ! later we want to have allocated arrays to
  ! share with the python routines
  ALLOCATE(sf_1d_fo % x(nx1))
  ALLOCATE(sf_1d_fo % phi(nx1))
  ALLOCATE(sf_1d_py % x(nx1))
  ALLOCATE(sf_1d_py % phi(nx1))

  CALL f_linspace(nx1, x1min, x1max, sf_1d_fo % x)
  CALL f_linspace(nx1, x1min, x1max, sf_1d_py % x)

  ! (4) calculate the field value by a function
  ! (4A) Fortran implementation
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL f_scalar_field_1d(nx1, sf_1d_fo % x, sf_1d_fo % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Fortran function call (seconds)', (ic2 - ic1) /crate  / repeats

  ! (4B) Python implementation
  sf_1d_py % phi(:) = 0.0 ! initialize to zero
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL i_scalar_field_1d(nx1, sf_1d_py % x, sf_1d_py % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Python  function call (seconds)', (ic2 - ic1) /crate  / repeats

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

  ! ------------------------------------------------------------
  !
  ! Demo: 1D scalar field, using MPI
  !
  ! ------------------------------------------------------------
   
  PRINT *, '--------------------------------------------'
  PRINT *, '       DEMO 1D Scalar Field  -- MPI         '
  PRINT *, '--------------------------------------------'

  CALL MPI_Init(error)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, mpi_rank, error);
  CALL MPI_Comm_size(MPI_COMM_WORLD, mpi_size, error);
  
  ! (1) test for the interface
  CALL i_hello_world()

  ! (2) set default values
  nx1 = 10000
  x1min = -5.0
  x1max = +5.0

  repeats = 3
 
  ! test for the interface
  PRINT *, 'shape in fortran', nx1
  CALL i_print_shape(nx1)
  CALL i_print_value(x1min)

  ! (3) initialize the arrays
  sf_1d_fo % k = nx1

  ! allocate them here
  ! later we want to have allocated arrays to
  ! share with the python routines
  ALLOCATE(sf_1d_fo % x(nx1))
  ALLOCATE(sf_1d_fo % phi(nx1))
  ALLOCATE(sf_1d_py % x(nx1))
  ALLOCATE(sf_1d_py % phi(nx1))

  CALL f_linspace(nx1, x1min, x1max, sf_1d_fo % x)
  CALL f_linspace(nx1, x1min, x1max, sf_1d_py % x)

  ! (4) calculate the field value by a function
  ! (4A) Fortran implementation
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL f_scalar_field_1d(nx1, sf_1d_fo % x, sf_1d_fo % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Fortran function call (seconds)', (ic2 - ic1) /crate  / repeats

  ! (4B) Python implementation
  sf_1d_py % phi(:) = 0.0 ! initialize to zero
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL i_scalar_field_1d(nx1, sf_1d_py % x, sf_1d_py % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Python  function call (seconds)', (ic2 - ic1) /crate  / repeats

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

  CALL MPI_Finalize(error)

  ! ------------------------------------------------------------
  !
  ! Demo: 2D scalar field
  !
  ! ------------------------------------------------------------
  
  PRINT *, '--------------------------------------------'
  PRINT *, '       DEMO 2D Scalar Field                 '
  PRINT *, '--------------------------------------------'

  ! (2) set default values
  nx2 = 150
  x2min = -7.0
  x2max = +5.0

  ! (3) initialize the arrays
  sf_2d_fo % k1 = nx1
  sf_2d_fo % k2 = nx2
  sf_2d_py % k1 = nx1
  sf_2d_py % k2 = nx2

  ! allocate them here
  ! later we want to have allocated arrays to
  ! share with the python routines
  ALLOCATE(sf_2d_fo % x1(nx1, nx2))
  ALLOCATE(sf_2d_fo % x2(nx1, nx2))
  ALLOCATE(sf_2d_fo % phi(nx1, nx2))
  ALLOCATE(sf_2d_py % x1(nx1, nx2))
  ALLOCATE(sf_2d_py % x2(nx1, nx2))
  ALLOCATE(sf_2d_py % phi(nx1, nx2))

  CALL f_meshgrid(nx1, nx2, x1min, x1max, x2min, x2max, &
  &               sf_2d_fo % x1, sf_2d_fo % x2)
  CALL f_meshgrid(nx1, nx2, x1min, x1max, x2min, x2max, &
  &               sf_2d_py % x1, sf_2d_py % x2)

  ! (4) calculate the field value by a function
  ! (4A) Fortran implementation
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL f_scalar_field_2d(nx1, nx2, sf_2d_fo % x1, sf_2d_fo % x2, sf_2d_fo % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Fortran function call (seconds)', (ic2 - ic1) /crate  / repeats

  ! (4B) Python implementation
  sf_2d_py % phi(:, :) = 0.0 ! initialize to zero
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL i_scalar_field_2d(nx1, nx2, sf_2d_py % x1, sf_2d_py % x2, sf_2d_py % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Python  function call (seconds)', (ic2 - ic1) /crate  / repeats

  ! (5) print to stdout
  PRINT *, '   2D scalar field (head)'
  PRINT *, '     (X1)', '          (X2)', '  Fortran (PHI)', '  Python (PHI)'
  DO i=1, 3
    DO j=1, 3
      PRINT *, sf_2d_fo % x1(i, j), sf_2d_fo % x2(i, j), sf_2d_fo % phi(i, j), sf_2d_py % phi(i, j)
    END DO
  END DO

  ! (6) check the arrays are equal
  IF(ALL(sf_2d_fo % phi .EQ. sf_2d_py % phi)) THEN
    PRINT *, ' Fortran and Python scalar field are equal'
  ELSE
    PRINT *, ' Fortran and Python scalar field are **NOT** equal'
  ENDIF


  ! (7) deallocate
  DEALLOCATE(sf_2d_fo % x1)
  DEALLOCATE(sf_2d_fo % x2)
  DEALLOCATE(sf_2d_fo % phi)
  DEALLOCATE(sf_2d_py % x1)
  DEALLOCATE(sf_2d_py % x2)
  DEALLOCATE(sf_2d_py % phi)

END PROGRAM routine
