! Microphysics processes replaced by call to neural network
! via cffi

MODULE superdropnet_by_cffi

  USE mo_kind,               ONLY: sp, wp
  USE mo_exception,          ONLY: message, finish
  USE mo_2mom_mcrph_types,   ONLY: particle
  USE mo_impl_constants       ,ONLY: MAX_CHAR_LENGTH
 
  USE mo_mlbridges_config,   ONLY: mlbridges_config

  USE mo_timer, ONLY: timers_level, timer_start, timer_stop
  IMPLICIT NONE

  INTERFACE

    SUBROUTINE i_warm_rain_nn( dim_i, dim_k, n_moments, &
      &                        current_moments, new_moments, &
      &                        trained_model_path, &
      &                        t0, t1, &
      &                        cffi_state ) bind(c)
      USE iso_c_binding

      INTEGER(c_int)    :: dim_i  ! dimension along i (of ikslice)
      INTEGER(c_int)    :: dim_k  ! dimension along k (of ikslice)
      INTEGER(c_int)    :: n_moments ! number of moments
      REAL(c_double)    :: current_moments(dim_i, dim_k, n_moments)
      REAL(c_double)    :: new_moments(dim_i, dim_k, n_moments)
      CHARACTER(c_char) :: trained_model_path
      REAL(c_double)    :: t0, t1 ! Start and end times for in-situ SuperdropNet timings
      INTEGER(c_int)    :: cffi_state ! state flag
    END SUBROUTINE i_warm_rain_nn

    SUBROUTINE i_checksum( dim_i, dim_k, n_moments, &
  END INTERFACE


  PRIVATE

  CHARACTER(len=*), PARAMETER :: routine = 'superdropnet_by_cffi'

  PUBLIC :: warm_rain_cffi

CONTAINS

  SUBROUTINE warm_rain_cffi(ik_slice, cloud, rain)
    INTEGER, INTENT(in) :: ik_slice(4)
    CLASS(particle),  INTENT(inout)       :: cloud, rain

    ! formatted output
    CHARACTER(LEN=90)           :: momstr
    CHARACTER(LEN=*), PARAMETER :: routine = 'warm_rain_cffi'

    ! start and end indices for 2D slices
    INTEGER :: istart, iend, kstart, kend
    INTEGER :: k, i, l
    
    INTEGER :: cffi_state ! cffi state flag

    ! time stamps
    REAL(wp) :: t0, t1

    ! moments in current ikslice
    INTEGER  :: dim_i, dim_k, dim_m ! dimension passed array
    REAL(wp), ALLOCATABLE :: current_moments(:,:,:), new_moments(:,:,:)

    LOGICAL :: ischeck ! from mlbridges config
    CHARACTER(LEN=MAX_CHAR_LENGTH) :: cmodelpath ! for C compatibility

    istart = ik_slice(1)
    iend   = ik_slice(2)
    kstart = ik_slice(3)
    kend   = ik_slice(4)

    dim_i = iend - istart + 1
    dim_k = kend - kstart + 1
    dim_m = 4 ! moment dimension

    ! initialize state
    cffi_state = 0 ! 1 - moments were zero; 2 - ML inference

    ! from config
    ischeck = mlbridges_config%ischeck
    cmodelpath = TRIM(mlbridges_config%modelpath)//CHAR(0)

    IF (timers_level > 10) CALL timer_start(timer_phys_2mom_cffialloc)
    ALLOCATE(current_moments(dim_i, dim_k, dim_m))
    ALLOCATE(new_moments(dim_i, dim_k, dim_m))

    ! loop the ik_slice to create the array containing current moments
    DO k=kstart,kend
      DO i=istart,iend
  
        current_moments(i-istart+1, k-kstart+1, 1) = cloud%q(i,k)
        current_moments(i-istart+1, k-kstart+1, 2) = cloud%n(i,k)
        current_moments(i-istart+1, k-kstart+1, 3) = rain%q(i,k)
        current_moments(i-istart+1, k-kstart+1, 4) = rain%n(i,k)
  
      ENDDO
    ENDDO
    IF (timers_level > 10) CALL timer_stop(timer_phys_2mom_cffialloc)

    ! initialize the new moments to 0
    new_moments(:,:,:) = 0.0_wp
    
    CALL i_warm_rain_nn(dim_i, dim_k, dim_m, current_moments, new_moments, cmodelpath, t0, t1, cffi_state)
    
    ! check if there were any problems
      IF (cffi_state .EQ. 3) THEN
        WRITE(momstr, '("STOP CFFI routine finished with error code", i2, " - NaN in input moments")') cffi_state
        CALL finish(TRIM(routine), TRIM(momstr))
      ELSEIF (cffi_state .EQ. 4) THEN
        WRITE(momstr, '("STOP CFFI routine finished with error code", i2, " - NaN in output moments")') cffi_state
        CALL finish(TRIM(routine), TRIM(momstr))
      ELSEIF (cffi_state .EQ. 5) THEN
        WRITE(momstr, '("STOP CFFI routine finished with error code", i2, " - output moments > 1e20")') cffi_state
        CALL finish(TRIM(routine), TRIM(momstr))
      ELSEIF ( MAXVAL(ABS(current_moments)) == 0.0_wp .AND. cffi_state /= 1 ) THEN
        WRITE(momstr, '("ASSERT current moments are zero - cffi state must be 1, but is", i2)') cffi_state
        CALL finish(TRIM(routine), TRIM(momstr))
      ELSEIF ( MAXVAL(ABS(current_moments)) > 0.0_wp .AND. cffi_state /= 2 ) THEN
        WRITE(momstr, '("ASSERT current moments are nonzero - cffi state must be 2, but is", i2)') cffi_state
        CALL finish(TRIM(routine), TRIM(momstr))
      ENDIF
    ENDIF

    ! loop the ik_slice to update the moments
    DO k=kstart,kend
      DO i=istart,iend
        cloud%q(i,k) = new_moments(i-istart+1, k-kstart+1, 1)
        cloud%n(i,k) = new_moments(i-istart+1, k-kstart+1, 2)
        rain%q(i,k)  = new_moments(i-istart+1, k-kstart+1, 3)
        rain%n(i,k)  = new_moments(i-istart+1, k-kstart+1, 4)
      ENDDO
    ENDDO
!
    DEALLOCATE(current_moments)
    DEALLOCATE(new_moments)
  
  END SUBROUTINE warm_rain_cffi
END MODULE superdropnet_by_cffi
