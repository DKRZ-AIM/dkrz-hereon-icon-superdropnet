! Pseudo code for setting up the YAC coupling 
! on the ICON side
!
! Follow existing coupling routines (atmo-ocean etc´.)

MODULE pseudo_superdropnet_coupling_frame

  USE mo_kind                ,ONLY: wp

  USE mo_run_config          ,ONLY: ltimer
  USE mo_timer               ,ONLY: timer_start, timer_stop,                &
       &                            timer_coupling_put, timer_coupling_get, &

  USE mo_impl_constants      ,ONLY: MAX_CHAR_LENGTH 

  USE mo_master_control      ,ONLY: get_my_process_name

  USE mo_mpi                 ,ONLY: my_process_is_mpi_workroot

  USE mo_exception           ,ONLY: finish, message, print_value, warning

  USE mo_yac_finterface      ,ONLY: yac_fget_version, yac_fdef_comp,        &
    &                               yac_fdef_field_mask, yac_fsearch,       &
    &                               yac_fput, yac_fget,                     &

  IMPLICIT NONE

  PRIVATE

  CHARACTER(len=*), PARAMETER :: str_module = 'pseudo_superdropnet_coupling_frame' ! Output of module for debug

  PUBLIC :: pseudo_construct_coupling
  PUBLIC :: field_id

  INTEGER, PARAMETER    :: no_of_fields = 140
  INTEGER               :: field_id(no_of_fields)

CONTAINS

  SUBROUTINE pseudo_construct_coupling (...)

    CHARACTER(LEN=*), PARAMETER :: str_routine = 'pseudo_construct_coupling'

    CHARACTER(LEN=MAX_CHAR_LENGTH)    :: field_name(no_of_fields)

    CHARACTER(LEN=max_char_length) :: grid_name
    CHARACTER(LEN=max_char_length) :: comp_name

    INTEGER :: comp_id

    INTEGER :: jc

    ! for the "hello world" exchange test
    REAL(wp), ALLOCATABLE :: test_buffer(:,:)
    INTEGER               :: info, ierror, number_cells, number_arrays, test_sum
    LOGICAL :: is_workroot

    is_workroot = my_process_is_mpi_workroot()

    CALL print_value('workroot in coupling test', is_workroot)

    !IF ( is_workroot ) THEN
    CALL print_value(' in if workroot in coupling test', is_workroot)


    comp_name = TRIM(get_my_process_name())
    CALL message('got comp name', comp_name)

    ! Inform the coupler about what we are
    CALL yac_fdef_comp ( TRIM(comp_name), comp_id )

    ! Print the YAC version
    CALL message('Running ICON atmosphere and ML-Python in coupled mode with YAC version ', TRIM(yac_fget_version()) )

    ! Announce one grid (patch) to the coupler
    grid_name = "icon_atmos_grid"

    field_name(1) = "example_field_icon_to_python"
    field_name(2) = "example_field_python_to_icon"
    field_name(3) = "moments_ic2py_zlev_1"
    field_name(4) = "moments_py2ic_zlev_1"
    field_name(5) = "moments_ic2py_zlev_2"
    field_name(6) = "moments_py2ic_zlev_2"
    field_name(7) = "moments_ic2py_zlev_3"
    field_name(8) = "moments_py2ic_zlev_3"
    field_name(9) = "moments_ic2py_zlev_4"
    field_name(10) = "moments_py2ic_zlev_4"
    field_name(11) = "moments_ic2py_zlev_5"
    field_name(12) = "moments_py2ic_zlev_5"
    field_name(13) = "moments_ic2py_zlev_6"
    field_name(14) = "moments_py2ic_zlev_6"
    field_name(15) = "moments_ic2py_zlev_7"
    field_name(16) = "moments_py2ic_zlev_7"
    field_name(17) = "moments_ic2py_zlev_8"
    field_name(18) = "moments_py2ic_zlev_8"
    field_name(19) = "moments_ic2py_zlev_9"
    field_name(20) = "moments_py2ic_zlev_9"
    field_name(21) = "moments_ic2py_zlev_10"
    field_name(22) = "moments_py2ic_zlev_10"
    field_name(23) = "moments_ic2py_zlev_11"
    field_name(24) = "moments_py2ic_zlev_11"
    field_name(25) = "moments_ic2py_zlev_12"
    field_name(26) = "moments_py2ic_zlev_12"
    field_name(27) = "moments_ic2py_zlev_13"
    field_name(28) = "moments_py2ic_zlev_13"
    field_name(29) = "moments_ic2py_zlev_14"
    field_name(30) = "moments_py2ic_zlev_14"
    field_name(31) = "moments_ic2py_zlev_15"
    field_name(32) = "moments_py2ic_zlev_15"
    field_name(33) = "moments_ic2py_zlev_16"
    field_name(34) = "moments_py2ic_zlev_16"
    field_name(35) = "moments_ic2py_zlev_17"
    field_name(36) = "moments_py2ic_zlev_17"
    field_name(37) = "moments_ic2py_zlev_18"
    field_name(38) = "moments_py2ic_zlev_18"
    field_name(39) = "moments_ic2py_zlev_19"
    field_name(40) = "moments_py2ic_zlev_19"
    field_name(41) = "moments_ic2py_zlev_20"
    field_name(42) = "moments_py2ic_zlev_20"
    field_name(43) = "moments_ic2py_zlev_21"
    field_name(44) = "moments_py2ic_zlev_21"
    field_name(45) = "moments_ic2py_zlev_22"
    field_name(46) = "moments_py2ic_zlev_22"
    field_name(47) = "moments_ic2py_zlev_23"
    field_name(48) = "moments_py2ic_zlev_23"
    field_name(49) = "moments_ic2py_zlev_24"
    field_name(50) = "moments_py2ic_zlev_24"
    field_name(51) = "moments_ic2py_zlev_25"
    field_name(52) = "moments_py2ic_zlev_25"
    field_name(53) = "moments_ic2py_zlev_26"
    field_name(54) = "moments_py2ic_zlev_26"
    field_name(55) = "moments_ic2py_zlev_27"
    field_name(56) = "moments_py2ic_zlev_27"
    field_name(57) = "moments_ic2py_zlev_28"
    field_name(58) = "moments_py2ic_zlev_28"
    field_name(59) = "moments_ic2py_zlev_29"
    field_name(60) = "moments_py2ic_zlev_29"
    field_name(61) = "moments_ic2py_zlev_30"
    field_name(62) = "moments_py2ic_zlev_30"
    field_name(63) = "moments_ic2py_zlev_31"
    field_name(64) = "moments_py2ic_zlev_31"
    field_name(65) = "moments_ic2py_zlev_32"
    field_name(66) = "moments_py2ic_zlev_32"
    field_name(67) = "moments_ic2py_zlev_33"
    field_name(68) = "moments_py2ic_zlev_33"
    field_name(69) = "moments_ic2py_zlev_34"
    field_name(70) = "moments_py2ic_zlev_34"
    field_name(71) = "moments_ic2py_zlev_35"
    field_name(72) = "moments_py2ic_zlev_35"
    field_name(73) = "moments_ic2py_zlev_36"
    field_name(74) = "moments_py2ic_zlev_36"
    field_name(75) = "moments_ic2py_zlev_37"
    field_name(76) = "moments_py2ic_zlev_37"
    field_name(77) = "moments_ic2py_zlev_38"
    field_name(78) = "moments_py2ic_zlev_38"
    field_name(79) = "moments_ic2py_zlev_39"
    field_name(80) = "moments_py2ic_zlev_39"
    field_name(81) = "moments_ic2py_zlev_40"
    field_name(82) = "moments_py2ic_zlev_40"
    field_name(83) = "moments_ic2py_zlev_41"
    field_name(84) = "moments_py2ic_zlev_41"
    field_name(85) = "moments_ic2py_zlev_42"
    field_name(86) = "moments_py2ic_zlev_42"
    field_name(87) = "moments_ic2py_zlev_43"
    field_name(88) = "moments_py2ic_zlev_43"
    field_name(89) = "moments_ic2py_zlev_44"
    field_name(90) = "moments_py2ic_zlev_44"
    field_name(91) = "moments_ic2py_zlev_45"
    field_name(92) = "moments_py2ic_zlev_45"
    field_name(93) = "moments_ic2py_zlev_46"
    field_name(94) = "moments_py2ic_zlev_46"
    field_name(95) = "moments_ic2py_zlev_47"
    field_name(96) = "moments_py2ic_zlev_47"
    field_name(97) = "moments_ic2py_zlev_48"
    field_name(98) = "moments_py2ic_zlev_48"
    field_name(99) = "moments_ic2py_zlev_49"
    field_name(100) = "moments_py2ic_zlev_49"
    field_name(101) = "moments_ic2py_zlev_50"
    field_name(102) = "moments_py2ic_zlev_50"
    field_name(103) = "moments_ic2py_zlev_51"
    field_name(104) = "moments_py2ic_zlev_51"
    field_name(105) = "moments_ic2py_zlev_52"
    field_name(106) = "moments_py2ic_zlev_52"
    field_name(107) = "moments_ic2py_zlev_53"
    field_name(108) = "moments_py2ic_zlev_53"
    field_name(109) = "moments_ic2py_zlev_54"
    field_name(110) = "moments_py2ic_zlev_54"
    field_name(111) = "moments_ic2py_zlev_55"
    field_name(112) = "moments_py2ic_zlev_55"
    field_name(113) = "moments_ic2py_zlev_56"
    field_name(114) = "moments_py2ic_zlev_56"
    field_name(115) = "moments_ic2py_zlev_57"
    field_name(116) = "moments_py2ic_zlev_57"
    field_name(117) = "moments_ic2py_zlev_58"
    field_name(118) = "moments_py2ic_zlev_58"
    field_name(119) = "moments_ic2py_zlev_59"
    field_name(120) = "moments_py2ic_zlev_59"
    field_name(121) = "moments_ic2py_zlev_60"
    field_name(122) = "moments_py2ic_zlev_60"
    field_name(123) = "moments_ic2py_zlev_61"
    field_name(124) = "moments_py2ic_zlev_61"
    field_name(125) = "moments_ic2py_zlev_62"
    field_name(126) = "moments_py2ic_zlev_62"
    field_name(127) = "moments_ic2py_zlev_63"
    field_name(128) = "moments_py2ic_zlev_63"
    field_name(129) = "moments_ic2py_zlev_64"
    field_name(130) = "moments_py2ic_zlev_64"
    field_name(131) = "moments_ic2py_zlev_65"
    field_name(132) = "moments_py2ic_zlev_65"
    field_name(133) = "moments_ic2py_zlev_66"
    field_name(134) = "moments_py2ic_zlev_66"
    field_name(135) = "moments_ic2py_zlev_67"
    field_name(136) = "moments_py2ic_zlev_67"
    field_name(137) = "moments_ic2py_zlev_68"
    field_name(138) = "moments_py2ic_zlev_68"
    field_name(139) = "moments_ic2py_zlev_69"
    field_name(140) = "moments_py2ic_zlev_69"

    DO jc = 1, no_of_fields
      CALL yac_fdef_field_mask ( &
        & TRIM(field_name(jc)),  &
        & comp_id,               &
        [...] ! other IDs
        & 1,                     &
        & field_id(jc) )
    ENDDO

    ! Print field ID
    DO jc = 1, no_of_fields
      CALL print_value('Field ID for '//TRIM(field_name(jc)), field_id(jc))
    ENDDO

    ! End definition of coupling fields and search

    CALL yac_fsearch ( error_status )
    IF (error_status .NE. 0) CALL finish(str_routine, 'finished with error status')

    ! hello world example
    ALLOCATE(test_buffer(880,1))
    DO jc=1,880
      test_buffer(jc, 1) = jc-1.0_wp
    ENDDO

    test_sum = 0.0_wp
    DO jc=1,880
      test_sum = test_sum + test_buffer(jc, 1)
    ENDDO

    IF (test_sum .NE. 386760) CALL finish(str_routine, 'test sum is not correct')

    number_cells = 880
    number_arrays = 1

    ! ---- PUT ----
    IF (ltimer) CALL timer_start(timer_coupling_put)

    CALL yac_fput ( field_id(1), number_cells, number_arrays, &
     &              test_buffer(1:number_cells,1:number_arrays), info, ierror )

    IF (ltimer) CALL timer_stop(timer_coupling_put)
    ! ---- END PUT ----

    ! ---- Python side ----

    ! ---- GET ----
    IF (ltimer) CALL timer_start(timer_coupling_get)

    CALL yac_fget ( field_id(2), number_cells, number_arrays, &
     &              test_buffer(1:number_cells,1:number_arrays), info, ierror )

    IF (ltimer) CALL timer_stop(timer_coupling_get)
    ! ---- END GET ----

    test_sum = 0.0_wp
    DO jc=1,880
      test_sum = test_sum + test_buffer(jc, 1)
    ENDDO
    IF (test_sum .NE. 387640) CALL finish(str_routine, 'test sum is not correct')

    DEALLOCATE(test_buffer)


    !ENDIF

  END SUBROUTINE pseudo_construct_coupling
