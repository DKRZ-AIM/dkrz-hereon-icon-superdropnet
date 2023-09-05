! Microphysics processes replaced by call to neural network
! via yac

MODULE superdropnet_by_yac

  USE mo_kind,               ONLY: sp, wp
  USE mo_exception,          ONLY: message, finish, warning, print_value
  USE mo_2mom_mcrph_types,   ONLY: particle
  USE mo_impl_constants      ,ONLY: MAX_CHAR_LENGTH
  USE mo_run_config          ,ONLY: ltimer
  USE mo_timer               ,ONLY: timer_start, timer_stop,                &
       &                            timer_coupling_put, timer_coupling_get 
 
  USE mo_atmo_ml_coupling_frame ,ONLY: field_id
  USE mo_parallel_config     ,ONLY: nproma
  USE mo_yac_finterface      ,ONLY: yac_fput, yac_fget
  USE mo_mlbridges_config,   ONLY: mlbridges_config

  IMPLICIT NONE

  PRIVATE

  CHARACTER(len=*), PARAMETER :: routine = 'superdropnet_by_yac'

  PUBLIC :: warm_rain_yac

CONTAINS

  SUBROUTINE warm_rain_yac(ik_slice, cloud, rain)
    INTEGER, INTENT(in) :: ik_slice(4)
    CLASS(particle),  INTENT(inout)       :: cloud, rain

    ! formatted output
    CHARACTER(LEN=90)           :: momstr
    CHARACTER(LEN=*), PARAMETER :: routine = 'warm_rain_yac'

    ! start and end indices for 2D slices
    INTEGER :: istart, iend, kstart, kend
    INTEGER :: k, i, l
    
    ! yac parameters
    INTEGER :: info, ierror
    INTEGER :: fid_ic2py, fid_py2ic

    ! moments in current ikslice
    INTEGER  :: dim_i, dim_k, dim_m ! dimension passed array
    REAL(wp), ALLOCATABLE :: buffer_ic2py(:,:), buffer_py2ic(:,:)

    LOGICAL :: isdebug, ischeck ! mlbridges_config parameters
    LOGICAL :: write_coupler_restart
    CHARACTER(LEN=MAX_CHAR_LENGTH) :: cmodelpath ! for C compatibility
    REAL(wp) :: maxmoment

    istart = ik_slice(1)
    iend   = ik_slice(2)
    kstart = ik_slice(3)
    kend   = ik_slice(4)

    dim_i = iend - istart + 1
    dim_k = kend - kstart + 1
    dim_m = 4 ! moment dimension

    ! from config
    isdebug = mlbridges_config%isdebug
    ischeck = mlbridges_config%ischeck

    IF ( dim_k .NE. 1) CALL finish(routine, 'Invalid for YAC bridge: dim_k /= 1')
    IF ( dim_i .NE. nproma ) CALL finish(routine, 'Invalid for YAC bridge: dim_i /= nproma')

    ! Calculate the field id for the current zlev
    fid_ic2py = INT( 2 * (kstart - 1) + 1 )
    fid_py2ic = INT( 2 * (kstart - 1) + 2 )
    
    ALLOCATE(buffer_ic2py(dim_i, dim_m))
    ALLOCATE(buffer_py2ic(dim_i, dim_m))

    ! loop the ik_slice to create the array containing current moments
    DO k=kstart,kend
      DO i=istart,iend
  
        buffer_ic2py(i-istart+1, 1)  = cloud%q(i,k)
        buffer_ic2py(i-istart+1, 2)  = cloud%n(i,k)
        buffer_ic2py(i-istart+1, 3)  = rain%q(i,k)
        buffer_ic2py(i-istart+1, 4)  = rain%n(i,k)

      ENDDO
    ENDDO

    ! initialize the new moments to 0
    buffer_py2ic(:,:) = 0.0_wp
    
    ! ---- PUT ----
    IF (ltimer) CALL timer_start(timer_coupling_put)

    CALL yac_fput ( field_id(fid_ic2py), dim_i, dim_m, &
     &              buffer_ic2py(1:dim_i, 1:dim_m), info, ierror )
    ! [ ... ] handle out-of-sync similar to existing YAC coupling in ICON

    IF (ierror .NE. 0) CALL finish(routine, TRIM(momstr))

    IF (ltimer) CALL timer_stop(timer_coupling_put)
    ! ---- END PUT ----
    ! ---- GET ----
    IF (ltimer) CALL timer_start(timer_coupling_get)

    CALL yac_fget ( field_id(fid_py2ic), dim_i, dim_m, &
     &              buffer_py2ic(1:dim_i, 1:dim_m), info, ierror )
    ! [ ... ] handle out-of-sync similar to existing YAC coupling in ICON

    IF (ierror .NE. 0) CALL finish(routine, TRIM(momstr))

    IF (ltimer) CALL timer_stop(timer_coupling_get)
    ENDIF
    ! ---- END GET ----


    ! loop the ik_slice to update the moments
    DO k=kstart,kend
      DO i=istart,iend
        cloud%q(i,k) = buffer_py2ic(i-istart+1, 1)
        cloud%n(i,k) = buffer_py2ic(i-istart+1, 2)
        rain%q(i,k)  = buffer_py2ic(i-istart+1, 3)
        rain%n(i,k)  = buffer_py2ic(i-istart+1, 4)
      ENDDO
    ENDDO

!
    DEALLOCATE(buffer_ic2py)
    DEALLOCATE(buffer_py2ic)
  
  END SUBROUTINE warm_rain_yac
END MODULE superdropnet_by_yac
