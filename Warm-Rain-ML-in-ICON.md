# ML in ICON

Third project milestone: Neural network inference with a pretrained model to replace the warm rain processes at runtime in ICON-AES. 

## Developer information

Collection of notes for developers of ICON Python bridges in the context of the voucher. See below for Usage.

### ICON-AES branch

We forked the ICON-AES master and created a new branch called `icon-aes-ml-bridges`.

### New namelist

A separate namelist controls the parameters that may be entered to select the ICON ML bridge. See the new files

```bash
src/namelists/mo_mlbridges_nml.f90            # namelist
src/configure_model/mo_mlbridges_config.f90   # configuration
```

### Example scripts

The example script to run the warm bubble experiment with a bridge is located in 

```bash
run/exp.atm_mlbridges_warm_bubble_fortran # for the fortran case
run/exp.atm_mlbridges_warm_bubble_cffi    # for the cffi case
```

Choose the bridge:

```bash
&mlbridges_nml
  mlbridges_config%icon_ml_bridge = 'fortran' ! select the bridge type (fortran: no bridge!)
/
```

Currently accepted values (an error is thrown if the bridge is not yet implemented):
- `fortran`: no python code is applied, pure ICON run
- `cffi`: call python code via the CFFI embedded python bridge
- `pipes`: create pipes and call python code
- `mpi`: call python code via MPI communicator

### In ICON-AES code

In places where a call to a Python program should replace an existing Fortran subroutine, we use `SELECT` statements to allow for flexibility. The original Fortran subroutine should always be preserved. Follow the pattern in `src/config/mo_mlbridges_config`:

```fortran
    ! valid options for the icon-ml bridge include
    ! fortran - calls the default fortran code
    ! cffi    - calls python code via cffi
    ! pipes   - calls python code via pipes
    ! mpi     - calls python code via mpi
    ! -----
    SELECT CASE (mlbridges_config%icon_ml_bridge)
      CASE ('fortran')
        CALL message('', ' Selected ICON machine learning bridge: fortran')
      CASE ('cffi')
        CALL message('', ' Selected ICON machine learning bridge: cffi')
        ! Test the bridge by a call to CFFI function
        CALL message('', '  testing the cffi bridge ...')
        CALL i_check_interface()
        CALL message('', '  ... finished testing the cffi bridge')
      CASE ('pipes')
        CALL message('', ' Selected ICON machine learning bridge: pipes')
        ! REMOVE lines when bridge is implemented
        WRITE (message_text,*) 'This bridge is not yet implemented'
        CALL finish(routine,message_text)
      CASE ('mpi')
        CALL message('', ' Selected ICON machine learning bridge: mpi')
        ! REMOVE lines when bridge is implemented
        WRITE (message_text,*) 'This bridge is not yet implemented'
        CALL finish(routine,message_text)
      CASE DEFAULT
        WRITE (message_text,*) ' Selected invalid ICON machine learning bridge ', &
            & mlbridges_config%icon_ml_bridge, &
            & ' Valid bridges are: fortran, cffi, pipes, mpi'
        CALL finish(routine,message_text)
    END SELECT

```

### How to add a new bridge

Check before merging:
- Add your bridge to the config in `src/configure_model/mo_mlbridges_config.f90`
- In places where different bridges are selected, add a select case with your bridge TODO add example
- Create an experiment script that uses the bridge
- Run scripts for `fortran`, `cffi` and your bridge to make sure the new bridge does not break existing code
- Document **all** additional changes to paths, runscripts, config files, ... that are necessary to run the bridge below in the Usage information

## Usage information

### CFFI bridge

#### Set up Python parameters

#### Set up Fortran code

#### Before running the program
