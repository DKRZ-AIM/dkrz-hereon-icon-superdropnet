USE mo_mlbridges_config,           ONLY: mlbridges_config
USE mo_2mom_mcrph_processes_cffi,  ONLY: warm_rain_cffi
USE mo_2mom_mcrph_processes_yac,   ONLY: warm_rain_yac
USE mo_2mom_mcrph_processes_pipes, ONLY: warm_rain_pipes

! necessary code snippets for invoking the 
! coupling of SuperdropNet
! instead of warm rain processes

! [...] cloud microphysics module
SELECT CASE (mlbridges_config%icon_ml_bridge)
  ! -- case 'fortran' is the ICON-AES default behaviour
  CASE ('fortran')
    ! warm rain processes
    ! [...] use standard code from cloud microphysics module
  ! -- case 'cffi' calls embedded python
  CASE ('cffi')
    CALL warm_rain_cffi(ik_slice, cloud, rain)
  CASE('yac')
    CALL warm_rain_yac(ik_slice, cloud, rain)
  ! -- case 'pipes' connects to separate python worker via FIFO pipes
  CASE ('pipes')
    CALL warm_rain_pipes(ik_slice, cloud, rain)
  ! -- add additional bridges here
END SELECT
