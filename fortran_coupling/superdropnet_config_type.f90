  TYPE t_mlbridges_config
     ! For a detailed description, see https://gitlab.dkrz.de/aim/2022-03-hereon-python-fortran-bridges
     CHARACTER(len=MAX_CHAR_LENGTH) :: icon_ml_bridge ! choice of bridge
     LOGICAL                        :: isbenchmark    ! switch for benchmark mode (always call model)
     LOGICAL                        :: isdebug        ! switch for debug mode
     LOGICAL                        :: ischeck        ! switch for check mode, implements checksum formula instead of NN
     CHARACTER(len=MAX_CHAR_LENGTH) :: modelpath      ! path to trained model
  END TYPE t_mlbridges_config
