# ML in ICON

Third project milestone: Neural network inference with a pretrained model at runtime in ICON-AES. 

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

Currently accepted inputs (an error is thrown if the bridge is not yet implemented):
- `fortran`: no python code is applied, pure ICON run
- `cffi`: call python code via the CFFI embedded python bridge
- `pipes`: create pipes and call python code
- `mpi`: call python code via MPI communicator

### How to add a new bridge

Checklist:
- Add your bridge to the config in `src/configure_model/mo_mlbridges_config.f90`
- In places where different bridges are selected, add a select case with your bridge TODO add example
- Create an experiment script that uses the bridge
- Document **all** additional changes to paths, runscripts, config files, ... that are necessary to run the bridge below in the Usage information

## Usage information

Stub
