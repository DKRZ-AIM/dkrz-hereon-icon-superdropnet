PROGRAM routine

  USE, INTRINSIC :: iso_c_binding ! for cffi

  USE mo_type, ONLY: t_scalar_field_1d, t_scalar_field_2d, t_scalar_field_3d
  USE mo_function, ONLY: f_scalar_field_1d, f_linspace, &
  &                      f_scalar_field_2d, f_meshgrid, &
  &                      f_scalar_field_3d, f_spacegrid, &
  &                      f_scalar_field_index_3d
  
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

    SUBROUTINE i_scalar_field_3d(nx1, nx2, nx3, x1, x2, x3, phi) bind(c)
      USE iso_c_binding
      INTEGER(c_int) :: nx1, nx2, nx3
      REAL(c_float)  :: x1(nx1, nx2, nx3), x2(nx1, nx2, nx3), x3(nx1, nx2, nx3)
      REAL(c_float)  :: phi(nx1, nx2, nx3)
    END SUBROUTINE i_scalar_field_3d
    
    SUBROUTINE i_scalar_field_index_3d(nx1, nx2, nx3, x1, x2, x3, phi) bind(c)
      USE iso_c_binding
      INTEGER(c_int) :: nx1, nx2, nx3
      REAL(c_float)  :: x1(nx1, nx2, nx3), x2(nx1, nx2, nx3), x3(nx1, nx2, nx3)
      REAL(c_float)  :: phi(nx1, nx2, nx3)
    END SUBROUTINE i_scalar_field_index_3d
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

  INTEGER        :: i, j, k  ! loop idxs
  INTEGER        :: nx1, nx2, nx3    ! grid dimension # TODO check
  REAL           :: x1min, x1max ! grid limits
  REAL           :: x2min, x2max
  REAL           :: x3min, x3max
  ! wall-time measurements
  INTEGER        :: ic1, ic2, cmax ! system clock
  REAL           :: crate
  INTEGER        :: repeats
  ! command line arguments
  CHARACTER(len=32) :: arg

  ! Derived types
  TYPE(t_scalar_field_1d) :: sf_1d_fo    ! fortran
  TYPE(t_scalar_field_1d) :: sf_1d_py    ! python cffi
  TYPE(t_scalar_field_1d) :: sf_1d_pipes ! python pipes
    
  TYPE(t_scalar_field_2d) :: sf_2d_fo    ! fortran
  TYPE(t_scalar_field_2d) :: sf_2d_py    ! python cffi
  TYPE(t_scalar_field_2d) :: sf_2d_pipes ! python pipes

  TYPE(t_scalar_field_3d) :: sf_3d_fo    ! fortran
  TYPE(t_scalar_field_3d) :: sf_3d_py    ! python cffi
  TYPE(t_scalar_field_3d) :: sf_i3d_fo    ! fortran
  TYPE(t_scalar_field_3d) :: sf_i3d_py    ! python cffi

  ! get command line arguments
  PRINT *, '------------------------------'
  PRINT *, ' Processing command line args '
  PRINT *, '  pass them as                '
  PRINT *, '   ./my_demo nx1 nx2 nx3      '
  PRINT *, ' WARNING any unintended usage '
  PRINT *, '  may result in unpredictable behaviour'
  PRINT *, '------------------------------'

  ! assign default values

  IF (COMMAND_ARGUMENT_COUNT() .EQ. 0) THEN 
    nx1 = 100 ! default for nx1
    nx2 = 100 ! default for nx2
    nx3 = 100 ! default for nx3
  ELSEIF (COMMAND_ARGUMENT_COUNT() .EQ. 1) THEN
    nx2 = 100 ! default for nx2
    nx3 = 100 ! default for nx3
  ELSEIF (COMMAND_ARGUMENT_COUNT() .EQ. 2) THEN
    nx3 = 100 ! default for nx3
  ELSEIF (COMMAND_ARGUMENT_COUNT() .GT. 3) THEN
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
    ELSEIF (i.EQ.3) THEN
      READ(arg,*)nx3
    ENDIF

  ENDDO


  PRINT *, 'nx1_value', nx1
  PRINT *, 'nx2_value', nx2


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
  
  ! open pipe
  !CALL ip_init_pipes(0, 1)
  
  ! (2) set default values
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

  PRINT *, 'Time per Fortran  function call (seconds)', (ic2 - ic1) /crate  / repeats

  ! (4B) Python cffi implementation
  sf_1d_py % phi(:) = 0.0 ! initialize to zero
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL i_scalar_field_1d(nx1, sf_1d_py % x, sf_1d_py % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Py-cffi  function call (seconds)', (ic2 - ic1) /crate  / repeats

  ! (4C) Python pipes implementation
  !sf_1d_pipes % phi(:) = 0.0 ! initialize to zero
  !CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  !DO i=1, repeats
  !  CALL ip_scalar_field_1d(nx1, sf_1d_pipes % x, sf_1d_pipes % phi)
  !END DO
  !CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  !PRINT *, 'Time per Py-pipes function call (seconds)', (ic2 - ic1) /crate  / repeats

  ! (5) print to stdout
  PRINT *, '   1D scalar field (head)'
  PRINT *, '     (X)', '    Fortran (PHI)', '  Py-cffi (PHI)', ' Py-pipes (PHI)'
  DO i=1, 10
    PRINT *, sf_1d_fo % x(i), sf_1d_fo % phi(i), sf_1d_py % phi(i), sf_1d_pipes % phi(i)
  END DO

  ! (6) check the arrays are equal
  IF(ALL(sf_1d_fo % phi .EQ. sf_1d_py % phi)) THEN
    PRINT *, ' Fortran and Py-cffi scalar field are equal'
  ELSE
    PRINT *, ' Fortran and Py-cffi scalar field are **NOT** equal'
  ENDIF
  !IF(ALL(sf_1d_fo % phi .EQ. sf_1d_pipes % phi)) THEN
  !  PRINT *, ' Fortran and Py-pipes scalar field are equal'
  !ELSE
  !  PRINT *, ' Fortran and Py-pipes scalar field are **NOT** equal'
  !ENDIF


  ! (7) deallocate
  DEALLOCATE(sf_1d_fo % x)
  DEALLOCATE(sf_1d_fo % phi)
  DEALLOCATE(sf_1d_py % x)
  DEALLOCATE(sf_1d_py % phi)
  DEALLOCATE(sf_1d_pipes % x)
  DEALLOCATE(sf_1d_pipes % phi)


  ! ------------------------------------------------------------
  !
  ! Demo: 2D scalar field
  !
  ! ------------------------------------------------------------
  
  PRINT *, '--------------------------------------------'
  PRINT *, '       DEMO 2D Scalar Field                 '
  PRINT *, '--------------------------------------------'

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

  PRINT *, 'Time per Fortran  function call (seconds)', (ic2 - ic1) /crate  / repeats

  ! (4B) Python cffi implementation
  sf_2d_py % phi(:, :) = 0.0 ! initialize to zero
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL i_scalar_field_2d(nx1, nx2, sf_2d_py % x1, sf_2d_py % x2, sf_2d_py % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Py-cffi  function call (seconds)', (ic2 - ic1) /crate  / repeats

  ! (4C) Python pipes implementation
  !sf_2d_pipes % phi(:, :) = 0.0 ! initialize to zero
  !CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  !DO i=1, repeats
  !  CALL ip_scalar_field_2d(nx1, nx2, sf_2d_pipes % x1, sf_2d_pipes % x2, sf_2d_pipes % phi)
  !END DO
  !CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  !PRINT *, 'Time per Py-pipes function call (seconds)', (ic2 - ic1) /crate  / repeats

  ! (5) print to stdout
  PRINT *, '   2D scalar field (head)'
  PRINT *, '     (X1)', '          (X2)', '    Fortran (PHI)', '  Py-cffi (PHI)', ' Py-pipes (PHI)'
  DO i=1, MIN(nx1,3)
    DO j=1, MIN(nx2,3)
      PRINT *, sf_2d_fo % x1(i, j), sf_2d_fo % x2(i, j), sf_2d_fo % phi(i, j), sf_2d_py % phi(i, j), sf_2d_pipes % phi(i, j)
    END DO
  END DO

  ! (6) check the arrays are equal
  IF(ALL(sf_2d_fo % phi .EQ. sf_2d_py % phi)) THEN
    PRINT *, ' Fortran and Py-cffi 2D scalar field are equal'
  ELSE
    PRINT *, ' Fortran and Py-cffi 2D scalar field are **NOT** equal'
  ENDIF
  !IF(ALL(sf_2d_fo % phi .EQ. sf_2d_pipes % phi)) THEN
  !  PRINT *, ' Fortran and Py-pipes 2D scalar field are equal'
  !ELSE
  !  PRINT *, ' Fortran and Py-pipes 2D scalar field are **NOT** equal'
  !ENDIF


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

  ! ------------------------------------------------------------
  !
  ! Demo: 3D scalar field
  !
  ! ------------------------------------------------------------
  
  PRINT *, '--------------------------------------------'
  PRINT *, '       DEMO 3D Scalar Field                 '
  PRINT *, '--------------------------------------------'

  ! (2) set default values
  x3min = -1.5
  x3max = +1.5

  ! (3) initialize the arrays
  sf_3d_fo % k1 = nx1
  sf_3d_fo % k2 = nx2
  sf_3d_fo % k3 = nx3
  sf_3d_py % k1 = nx1
  sf_3d_py % k2 = nx2
  sf_3d_py % k3 = nx3

  ! allocate them here
  ! later we want to have allocated arrays to
  ! share with the python routines
  ALLOCATE(sf_3d_fo % x1(nx1, nx2, nx3))
  ALLOCATE(sf_3d_fo % x2(nx1, nx2, nx3))
  ALLOCATE(sf_3d_fo % x3(nx1, nx2, nx3))
  ALLOCATE(sf_3d_fo % phi(nx1, nx2, nx3))
  ALLOCATE(sf_3d_py % x1(nx1, nx2, nx3))
  ALLOCATE(sf_3d_py % x2(nx1, nx2, nx3))
  ALLOCATE(sf_3d_py % x3(nx1, nx2, nx3))
  ALLOCATE(sf_3d_py % phi(nx1, nx2, nx3))


  CALL f_spacegrid(nx1, nx2, nx3, x1min, x1max, x2min, x2max, x3min, x3max, &
  &               sf_3d_fo % x1, sf_3d_fo % x2, sf_3d_fo % x3)
  CALL f_spacegrid(nx1, nx2, nx3, x1min, x1max, x2min, x2max, x3min, x3max, &
  &               sf_3d_py % x1, sf_3d_py % x2, sf_3d_py % x3)

  ! (4) calculate the field value by a function
  ! (4A) Fortran implementation
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL f_scalar_field_3d(nx1, nx2, nx3, sf_3d_fo % x1, sf_3d_fo % x2, sf_3d_fo % x3, sf_3d_fo % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Fortran  function call (seconds)', (ic2 - ic1) /crate  / repeats

  ! (4B) Python cffi implementation
  sf_3d_py % phi(:, :, :) = 0.0 ! initialize to zero
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL i_scalar_field_3d(nx1, nx2, nx3, sf_3d_py % x1, sf_3d_py % x2, sf_3d_py % x3, sf_3d_py % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Py-cffi  function call (seconds)', (ic2 - ic1) /crate  / repeats

  ! (5) print to stdout
  PRINT *, '   3D scalar field (head)'
  PRINT *, '     (X1)', '          (X2)',  '        (x3)', '    Fortran (PHI)', '  Py-cffi (PHI)'
  DO k=1, MIN(nx3,3)
  DO j=1, MIN(nx2,3)
    DO i=1, MIN(nx1,3)
      PRINT *, sf_3d_fo % x1(i,j,k), sf_3d_fo % x2(i,j,k), sf_3d_fo % x3(i,j,k), sf_3d_fo % phi(i,j,k), sf_3d_py % phi(i,j,k)
    END DO
  END DO
  END DO

  ! (6) check the arrays are equal
  IF(ALL(sf_3d_fo % phi .EQ. sf_3d_py % phi)) THEN
    PRINT *, ' Fortran and Py-cffi 3D scalar field are equal'
  ELSE
    PRINT *, ' Fortran and Py-cffi 3D scalar field are **NOT** equal'
  ENDIF

  DO k=1,nx3
    DO j=1,nx2
      DO i=1,nx1
        IF ((sf_3d_fo % phi(i,j,k) .NE. sf_3d_py % phi(i,j,k) )) THEN
          PRINT *, sf_3d_fo % x1(i,j,k), sf_3d_fo % x2(i,j,k), sf_3d_fo % x3(i,j,k), sf_3d_fo % phi(i,j,k), sf_3d_py % phi(i,j,k)
        ENDIF
      ENDDO
    ENDDO
  ENDDO

  ! (7) deallocate
  DEALLOCATE(sf_3d_fo % x1)
  DEALLOCATE(sf_3d_fo % x2)
  DEALLOCATE(sf_3d_fo % phi)
  DEALLOCATE(sf_3d_py % x1)
  DEALLOCATE(sf_3d_py % x2)
  DEALLOCATE(sf_3d_py % phi)

  ! ------------------------------------------------------------
  !
  ! Demo: 3D scalar field to test array orders
  !
  ! ------------------------------------------------------------
  
  PRINT *, '--------------------------------------------'
  PRINT *, '       DEMO 3D Scalar Field (Index Edition) ' 
  PRINT *, '--------------------------------------------'

  ! (2) set default values (dummy here)
  x3min = -1.5
  x3max = +1.5

  ! (3) initialize the arrays
  sf_i3d_fo % k1 = nx1
  sf_i3d_fo % k2 = nx2
  sf_i3d_fo % k3 = nx3
  sf_i3d_py % k1 = nx1
  sf_i3d_py % k2 = nx2
  sf_i3d_py % k3 = nx3

  ! allocate them here
  ! later we want to have allocated arrays to
  ! share with the python routines
  ALLOCATE(sf_i3d_fo % x1(nx1, nx2, nx3))
  ALLOCATE(sf_i3d_fo % x2(nx1, nx2, nx3))
  ALLOCATE(sf_i3d_fo % x3(nx1, nx2, nx3))
  ALLOCATE(sf_i3d_fo % phi(nx1, nx2, nx3))
  ALLOCATE(sf_i3d_py % x1(nx1, nx2, nx3))
  ALLOCATE(sf_i3d_py % x2(nx1, nx2, nx3))
  ALLOCATE(sf_i3d_py % x3(nx1, nx2, nx3))
  ALLOCATE(sf_i3d_py % phi(nx1, nx2, nx3))


  CALL f_spacegrid(nx1, nx2, nx3, x1min, x1max, x2min, x2max, x3min, x3max, &
  &               sf_i3d_fo % x1, sf_i3d_fo % x2, sf_i3d_fo % x3)
  CALL f_spacegrid(nx1, nx2, nx3, x1min, x1max, x2min, x2max, x3min, x3max, &
  &               sf_i3d_py % x1, sf_i3d_py % x2, sf_i3d_py % x3)

  ! (4) calculate the field value by a function
  ! (4A) Fortran implementation
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL f_scalar_field_index_3d(nx1, nx2, nx3, sf_i3d_fo % x1, sf_i3d_fo % x2, sf_i3d_fo % x3, sf_i3d_fo % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Fortran  function call (seconds)', (ic2 - ic1) /crate  / repeats

  ! (4B) Python cffi implementation
  sf_i3d_py % phi(:, :, :) = 0.0 ! initialize to zero
  CALL system_clock(count=ic1, count_rate=crate, count_max=cmax)
  DO i=1, repeats
    CALL i_scalar_field_index_3d(nx1, nx2, nx3, sf_i3d_py % x1, sf_i3d_py % x2, sf_i3d_py % x3, sf_i3d_py % phi)
  END DO
  CALL system_clock(count=ic2, count_rate=crate, count_max=cmax)

  PRINT *, 'Time per Py-cffi  function call (seconds)', (ic2 - ic1) /crate  / repeats

  ! (5) print to stdout
  PRINT *, '   3D scalar field (head)'
  PRINT *, '     (i)', '          (j)',  '        (k)', '    Fortran (PHI)', '  Py-cffi (PHI)'
  DO k=1, MIN(nx3,3)
  DO j=1, MIN(nx2,3)
    DO i=1, MIN(nx1,3)
      PRINT *, i, j, k, sf_i3d_fo % phi(i,j,k), sf_i3d_py % phi(i,j,k)
    END DO
  END DO
  END DO

  ! (6) check the arrays are equal
  IF(ALL(sf_i3d_fo % phi .EQ. sf_i3d_py % phi)) THEN
    PRINT *, ' Fortran and Py-cffi 3D scalar field are equal'
  ELSE
    PRINT *, ' Fortran and Py-cffi 3D scalar field are **NOT** equal'
  ENDIF

  DO k=1,nx3
    DO j=1,nx2
      DO i=1,nx1
        IF ((sf_i3d_fo % phi(i,j,k) .NE. sf_i3d_py % phi(i,j,k) )) THEN
          PRINT *, i, j, k, sf_i3d_fo % phi(i,j,k), sf_i3d_py % phi(i,j,k)
        ENDIF
      ENDDO
    ENDDO
  ENDDO

  ! (7) deallocate
  DEALLOCATE(sf_i3d_fo % x1)
  DEALLOCATE(sf_i3d_fo % x2)
  DEALLOCATE(sf_i3d_fo % phi)
  DEALLOCATE(sf_i3d_py % x1)
  DEALLOCATE(sf_i3d_py % x2)
  DEALLOCATE(sf_i3d_py % phi)
  
  ! stop worker and close pipe
  !CALL ip_close_pipes()

END PROGRAM routine
