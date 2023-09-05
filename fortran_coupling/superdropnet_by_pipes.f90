! Microphysics processes replaced by call to neural network
! via pipes

MODULE superdropnet_by_pipes

  USE mo_kind,               ONLY: sp, wp
  USE mo_exception,          ONLY: message, finish
  USE mo_2mom_mcrph_types,   ONLY: particle
  USE mo_impl_constants       ,ONLY: MAX_CHAR_LENGTH
 
  USE mo_mlbridges_config,   ONLY: mlbridges_config

  IMPLICIT NONE

  INTERFACE

    SUBROUTINE ip_warm_rain_pipes_nn( dim_i, dim_k, n_moments, &
      &                              current_moments, new_moments, &
      &                              trained_model_path, &
      &                              pipes_return_state ) bind(c)
      USE, INTRINSIC :: iso_c_binding

      INTEGER(c_int)    :: dim_i  ! dimension along i (of ikslice)
      INTEGER(c_int)    :: dim_k  ! dimension along k (of ikslice)
      INTEGER(c_int)    :: n_moments ! number of moments
      REAL(c_double)    :: current_moments(dim_i, dim_k, n_moments)
      REAL(c_double)    :: new_moments(dim_i, dim_k, n_moments)
      CHARACTER(c_char) :: trained_model_path
      INTEGER(c_int)    :: pipes_return_state ! state flag
    END SUBROUTINE ip_warm_rain_pipes_nn

    SUBROUTINE ip_checksum_pipes( dim_i, dim_k, n_moments, &
      &                          current_moments, new_moments, &
      &                          pipes_return_state ) bind(c)
      USE, INTRINSIC :: iso_c_binding

      INTEGER(c_int) :: dim_i  ! dimension along i (of ikslice)
      INTEGER(c_int) :: dim_k  ! dimension along k (of ikslice)
      INTEGER(c_int) :: n_moments ! number of moments
      REAL(c_double) :: current_moments(dim_i, dim_k, n_moments)
      REAL(c_double) :: new_moments(dim_i, dim_k, n_moments)
      INTEGER(c_int) :: pipes_return_state ! state flag
    END SUBROUTINE ip_checksum_pipes

    SUBROUTINE ip_init_pipes(shm_rank, shm_size) bind(C)
      USE iso_c_binding
      INTEGER, intent (IN) :: shm_rank
      INTEGER, intent (IN) :: shm_size
    END SUBROUTINE ip_init_pipes  
  
    SUBROUTINE ip_close_pipes() bind(C)
      USE iso_c_binding
    END SUBROUTINE ip_close_pipes
    
  END INTERFACE

  PRIVATE

  CHARACTER(len=*), PARAMETER :: routine = 'superdropnet_by_pipes'

  PUBLIC :: mlbridges_pipes_worker_initialize
  PUBLIC :: mlbridges_pipes_worker_destruct
  PUBLIC :: warm_rain_pipes

CONTAINS

  SUBROUTINE mlbridges_pipes_worker_initialize(shm_rank, shm_size)

    CHARACTER(*), PARAMETER :: routine = "superdropnet_by_pipes:mlbridges_pipes_worker_initialize"
    INTEGER, intent (IN) :: shm_rank
    INTEGER, intent (IN) :: shm_size

    CALL ip_init_pipes(shm_rank, shm_size)

  END SUBROUTINE mlbridges_pipes_worker_initialize

  SUBROUTINE mlbridges_pipes_worker_destruct()

    CHARACTER(*), PARAMETER :: routine = "superdropnet_by_pipes:mlbridges_pipes_worker_destruct"
    
    CALL ip_close_pipes()

  END SUBROUTINE mlbridges_pipes_worker_destruct

  SUBROUTINE warm_rain_pipes(ik_slice, cloud, rain)
    INTEGER, INTENT(in) :: ik_slice(4)
    CLASS(particle),  INTENT(inout)       :: cloud, rain

    ! formatted output
    CHARACTER(LEN=90)           :: momstr
    CHARACTER(LEN=*), PARAMETER :: routine = 'warm_rain_pipes'

    ! start and end indices for 2D slices
    INTEGER :: istart, iend, kstart, kend
    INTEGER :: k, i, l
    
    INTEGER :: pipes_return_state ! pipes return state code

    ! moments in current ikslice
    INTEGER  :: dim_i, dim_k, dim_m ! dimension passed array
    REAL(wp), ALLOCATABLE :: current_moments(:,:,:), new_moments(:,:,:)

    LOGICAL :: isdebug, ischeck ! mlbridges_config parameters
    CHARACTER(LEN=MAX_CHAR_LENGTH) :: cmodelpath ! for C compatibility

    istart = ik_slice(1)
    iend   = ik_slice(2)
    kstart = ik_slice(3)
    kend   = ik_slice(4)

    dim_i = iend - istart + 1
    dim_k = kend - kstart + 1
    dim_m = 4 ! moment dimension

    ! initialize state
    pipes_return_state = 0 ! 1 - moments were zero; 2 - ML inference

    ! from config
    isdebug = mlbridges_config%isdebug
    ischeck = mlbridges_config%ischeck
    cmodelpath = TRIM(mlbridges_config%modelpath)//CHAR(0)

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

    ! initialize the new moments to 0
    new_moments(:,:,:) = 0.0_wp
    
    ! check sum formula via pipes
    ! no moments calculated
    IF (ischeck) THEN
        CALL ip_checksum_pipes(dim_i, dim_k, dim_m, current_moments, new_moments, pipes_return_state)

        DO i=1, dim_i
          DO k=1, dim_k
            DO l=1, 4
              WRITE(momstr, '("New moments M(i,k,l) = ", f6.1, i4, i4, i4)') & 
                     & new_moments(i, k, l), i, k, l
              CALL message(TRIM(routine), TRIM(momstr), all_print=.TRUE.)
            ENDDO
          ENDDO
        ENDDO
    ! update the moments from warm rain neural network via pipes (default case)
    ELSE
        CALL ip_warm_rain_pipes_nn(dim_i, dim_k, dim_m, current_moments, new_moments, cmodelpath, pipes_return_state)
    ENDIF

    ! in benchmark mode, keep the old moments
    IF (mlbridges_config%isbenchmark) THEN
      new_moments = current_moments
    ! assert bridge has been called correctly
    ELSE
      IF (pipes_return_state .EQ. 3) THEN
        WRITE(momstr, '("STOP CFFI routine finished with error code", i2, " - NaN in input moments")') pipes_return_state
        CALL finish(TRIM(routine), TRIM(momstr))
      ELSEIF (pipes_return_state .EQ. 4) THEN
        WRITE(momstr, '("STOP CFFI routine finished with error code", i2, " - NaN in output moments")') pipes_return_state
        CALL finish(TRIM(routine), TRIM(momstr))
      ELSEIF ( MAXVAL(ABS(current_moments)) == 0.0_wp .AND. pipes_return_state /= 1 ) THEN
        WRITE(momstr, '("ASSERT current moments are zero - cffi state must be 1, but is", i2)') pipes_return_state
        CALL finish(TRIM(routine), TRIM(momstr))
      ELSEIF ( MAXVAL(ABS(current_moments)) > 0.0_wp .AND. pipes_return_state /= 2 ) THEN
        WRITE(momstr, '("ASSERT current moments are nonzero - cffi state must be 2, but is", i2)') pipes_return_state
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
  
  END SUBROUTINE warm_rain_pipes
END MODULE superdropnet_by_pipes
