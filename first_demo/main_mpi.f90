PROGRAM routine

  USE, INTRINSIC :: iso_c_binding ! for cffi
  USE mpi
  USE IFPORT

  USE mo_type, ONLY: t_scalar_field_1d, t_scalar_field_2d
  USE mo_function, ONLY: f_scalar_field_1d, f_linspace, &
  &                      f_scalar_field_2d, f_meshgrid
  USE mo_pipes, ONLY: ip_init_pipes, ip_close_pipes, ip_scalar_field_1d, ip_scalar_field_2d


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
  ! command line arguments
  CHARACTER(len=32) :: arg

  ! FORTRAN mpi side
  INTEGER(KIND=4) :: error
  INTEGER(KIND=4) :: mpi_rank
  INTEGER(KIND=4) :: mpi_size

  ! useful helpers
  CHARACTER(MAX_HOSTNAM_LENGTH + 1) :: hostname
  INTEGER(4) :: istat

  ! Derived types
  TYPE(t_scalar_field_1d) :: sf_1d_fo    ! fortran
  TYPE(t_scalar_field_1d) :: sf_1d_py    ! python cffi
  TYPE(t_scalar_field_1d) :: sf_1d_pipes ! python pipes
    
  TYPE(t_scalar_field_2d) :: sf_2d_fo    ! fortran
  TYPE(t_scalar_field_2d) :: sf_2d_py    ! python cffi
  TYPE(t_scalar_field_2d) :: sf_2d_pipes ! python pipes

  ! get command line arguments
  PRINT *, '------------------------------'
  PRINT *, ' Processing command line args '
  PRINT *, '  pass them as                '
  PRINT *, '   ./my_demo nx1 nx2          '
  PRINT *, ' WARNING any unintended usage '
  PRINT *, '  may result in unpredictable behaviour'
  PRINT *, '------------------------------'

  ! assign default values

  IF (COMMAND_ARGUMENT_COUNT() .EQ. 0) THEN 
    nx1 = 100 ! default for nx1
    nx2 = 100 ! default for nx2
  ELSEIF (COMMAND_ARGUMENT_COUNT() .EQ. 1) THEN
    nx2 = 100 ! default for nx2
  ELSEIF (COMMAND_ARGUMENT_COUNT() .GT. 2) THEN
    PRINT *, 'Too many command line arguments'
    STOP
  ENDIF

  DO i=1, COMMAND_ARGUMENT_COUNT()
    CALL GET_COMMAND_ARGUMENT(i, arg)
    PRINT *, 'command line argument', i, arg
    
    IF (i.EQ.1) THEN
      READ(arg,*)nx1
    ELSEIF (i.EQ.2) THEN
      READ(arg,*)nx2
    ENDIF

  ENDDO


  PRINT *, nx1, nx2


  ! ------------------------------------------------------------
  !
  ! Demo: 1D scalar field, using MPI
  !
  ! ------------------------------------------------------------
   
  CALL MPI_Init(error)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, mpi_rank, error);
  CALL MPI_Comm_size(MPI_COMM_WORLD, mpi_size, error);
  
  IF (mpi_rank .eq. 0) THEN
      PRINT *, '--------------------------------------------'
      PRINT *, '       DEMO 1D Scalar Field  -- MPI         '
      PRINT *, '--------------------------------------------'
  ENDIF

  ! (1) test for the interface
  IF (mpi_rank .eq. 0) THEN
    PRINT *, '--- hello world statement ---'
  ENDIF
  CALL i_hello_world()

  istat = HOSTNAM (hostname)
  PRINT *, 'rank', mpi_rank, 'running on ', hostname

  CALL ip_init_pipes(mpi_rank)

  CALL MPI_BARRIER(MPI_COMM_WORLD, error);

  ! (2) set default values
  x1min = -5.0
  x1max = +5.0

  repeats = 3
 
  ! test for the interface
  PRINT *, 'shape in fortran', nx1
  CALL i_print_shape(nx1)
  PRINT *, 'current rank', mpi_rank
  CALL i_print_value(FLOAT(mpi_rank))

  CALL MPI_BARRIER(MPI_COMM_WORLD, error);
  ! (3) initialize the arrays
  sf_1d_fo % k = nx1

  ! allocate them here
  ! later we want to have allocated arrays to
  ! share with the python routines
  ALLOCATE(sf_1d_fo % x(nx1))
  ALLOCATE(sf_1d_fo % phi(nx1))
  ALLOCATE(sf_1d_py % x(nx1))
  ALLOCATE(sf_1d_py % phi(nx1))
  ALLOCATE(sf_1d_pipes % x(nx1))
  ALLOCATE(sf_1d_pipes % phi(nx1))

  CALL f_linspace(nx1, x1min, x1max, sf_1d_fo % x)
  CALL f_linspace(nx1, x1min, x1max, sf_1d_py % x)
  CALL f_linspace(nx1, x1min, x1max, sf_1d_pipes % x)

  ! (4) calculate the field value by a function
  ! (4A) Fortran implementation
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL f_scalar_field_1d(nx1, sf_1d_fo % x, sf_1d_fo % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Fortran function call (seconds)', (ic2 - ic1) /crate  / repeats

  CALL MPI_BARRIER(MPI_COMM_WORLD, error);
  ! (4B) Python cffi implementation
  sf_1d_py % phi(:) = 0.0 ! initialize to zero
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL i_scalar_field_1d(nx1, sf_1d_py % x, sf_1d_py % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Py-cffi  function call (seconds)', (ic2 - ic1) /crate  / repeats

  CALL MPI_BARRIER(MPI_COMM_WORLD, error);
  
  ! (4C) Python pipes implementation
  sf_1d_pipes % phi(:) = 0.0 ! initialize to zero
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL ip_scalar_field_1d(nx1, sf_1d_pipes % x, sf_1d_pipes % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Py-pipes function call (seconds)', (ic2 - ic1) /crate  / repeats

  CALL MPI_BARRIER(MPI_COMM_WORLD, error);
  
  ! (5) print to stdout
  ! PRINT *, '   1D scalar field (head)'
  ! PRINT *, '     (X)', '  Fortran (PHI)', '  Python (PHI)'
  ! DO i=1, 10
  !   PRINT *, sf_1d_fo % x(i), sf_1d_fo % phi(i), sf_1d_py % phi(i)
  ! END DO

  ! (6) check the arrays are equal
  IF(ALL(sf_1d_fo % phi .EQ. sf_1d_py % phi)) THEN
    PRINT *, ' Fortran and Py-cffi scalar field are equal in rank', mpi_rank
  ELSE
    PRINT *, ' Fortran and Py-cffi scalar field are **NOT** equal in rank', mpi_rank
  ENDIF
  IF(ALL(sf_1d_fo % phi .EQ. sf_1d_pipes % phi)) THEN
    PRINT *, ' Fortran and Py-pipes scalar field are equal in rank', mpi_rank
  ELSE
    PRINT *, ' Fortran and Py-pipes scalar field are **NOT** equal in rank', mpi_rank
  ENDIF

  ! (7) deallocate
  DEALLOCATE(sf_1d_fo % x)
  DEALLOCATE(sf_1d_fo % phi)
  DEALLOCATE(sf_1d_py % x)
  DEALLOCATE(sf_1d_py % phi)
  DEALLOCATE(sf_1d_pipes % x)
  DEALLOCATE(sf_1d_pipes % phi)

  CALL MPI_BARRIER(MPI_COMM_WORLD, error);

  ! ------------------------------------------------------------
  !
  ! Demo: 2D scalar field
  !
  ! ------------------------------------------------------------
  
  IF (mpi_rank .eq. 0) THEN
      PRINT *, '--------------------------------------------'
      PRINT *, '       DEMO 2D Scalar Field                 '
      PRINT *, '--------------------------------------------'
  ENDIF

  ! (2) set default values
  x2min = -7.0
  x2max = +5.0

  ! (3) initialize the arrays
  sf_2d_fo % k1 = nx1
  sf_2d_fo % k2 = nx2
  sf_2d_py % k1 = nx1
  sf_2d_py % k2 = nx2
  sf_2d_pipes % k1 = nx1
  sf_2d_pipes % k2 = nx2

  ! allocate them here
  ! later we want to have allocated arrays to
  ! share with the python routines
  ALLOCATE(sf_2d_fo % x1(nx1, nx2))
  ALLOCATE(sf_2d_fo % x2(nx1, nx2))
  ALLOCATE(sf_2d_fo % phi(nx1, nx2))
  ALLOCATE(sf_2d_py % x1(nx1, nx2))
  ALLOCATE(sf_2d_py % x2(nx1, nx2))
  ALLOCATE(sf_2d_py % phi(nx1, nx2))
  ALLOCATE(sf_2d_pipes % x1(nx1, nx2))
  ALLOCATE(sf_2d_pipes % x2(nx1, nx2))
  ALLOCATE(sf_2d_pipes % phi(nx1, nx2))

  CALL f_meshgrid(nx1, nx2, x1min, x1max, x2min, x2max, &
  &               sf_2d_fo % x1, sf_2d_fo % x2)
  CALL f_meshgrid(nx1, nx2, x1min, x1max, x2min, x2max, &
  &               sf_2d_py % x1, sf_2d_py % x2)
  CALL f_meshgrid(nx1, nx2, x1min, x1max, x2min, x2max, &
  &               sf_2d_pipes % x1, sf_2d_pipes % x2)

  ! (4) calculate the field value by a function
  ! (4A) Fortran implementation
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL f_scalar_field_2d(nx1, nx2, sf_2d_fo % x1, sf_2d_fo % x2, sf_2d_fo % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Fortran function call (seconds)', (ic2 - ic1) /crate  / repeats

  CALL MPI_BARRIER(MPI_COMM_WORLD, error);
  ! (4B) Python cffi implementation
  sf_2d_py % phi(:, :) = 0.0 ! initialize to zero
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL i_scalar_field_2d(nx1, nx2, sf_2d_py % x1, sf_2d_py % x2, sf_2d_py % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Python  function call (seconds)', (ic2 - ic1) /crate  / repeats

  CALL MPI_BARRIER(MPI_COMM_WORLD, error);
  ! (4C) Python pipes implementation
  sf_2d_pipes % phi(:, :) = 0.0 ! initialize to zero
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL ip_scalar_field_2d(nx1, nx2, sf_2d_pipes % x1, sf_2d_pipes % x2, sf_2d_pipes % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Py-pipes function call (seconds)', (ic2 - ic1) /crate  / repeats

  CALL MPI_BARRIER(MPI_COMM_WORLD, error);

  ! (5) print to stdout
  ! PRINT *, '   2D scalar field (head)'
  ! PRINT *, '     (X1)', '          (X2)', '  Fortran (PHI)', '  Python (PHI)'
  ! DO i=1, 3
  !   DO j=1, 3
  !     PRINT *, sf_2d_fo % x1(i, j), sf_2d_fo % x2(i, j), sf_2d_fo % phi(i, j), sf_2d_py % phi(i, j)
  !   END DO
  ! END DO

  ! (6) check the arrays are equal
  IF(ALL(sf_2d_fo % phi .EQ. sf_2d_py % phi)) THEN
    PRINT *, ' Fortran and Py-cffi 2D scalar field are equal in rank', mpi_rank
  ELSE
    PRINT *, ' Fortran and Py-cffi 2D scalar field are **NOT** equal in rank', mpi_rank
  ENDIF
  IF(ALL(sf_2d_fo % phi .EQ. sf_2d_pipes % phi)) THEN
    PRINT *, ' Fortran and Py-pipes 2D scalar field are equal in rank', mpi_rank
  ELSE
    PRINT *, ' Fortran and Py-pipes 2D scalar field are **NOT** equal in rank', mpi_rank
  ENDIF


  ! (7) deallocate
  DEALLOCATE(sf_2d_fo % x1)
  DEALLOCATE(sf_2d_fo % x2)
  DEALLOCATE(sf_2d_fo % phi)
  DEALLOCATE(sf_2d_py % x1)
  DEALLOCATE(sf_2d_py % x2)
  DEALLOCATE(sf_2d_py % phi)
  DEALLOCATE(sf_2d_pipes % x1)
  DEALLOCATE(sf_2d_pipes % x2)
  DEALLOCATE(sf_2d_pipes % phi)

  CALL ip_close_pipes()
  CALL MPI_Finalize(error)
  
END PROGRAM routine
