# ML in ICON

Third project milestone: Neural network inference with a pretrained model at runtime in ICON-AES. 

## Usage information

### ICON-AES branch

We forked the ICON-AES master and created a new branch called `icon-aes-ml-bridges`. Obtain the code here:

```bash
git clone --recursive git@gitlab.dkrz.de:k202141/icon-aes.git
git checkout icon-aes-ml-bridges
```

Configure ICON to use the ML bridge as an external module:

```bash
./config/dkrz/levante.intel-2021.5.0 --enable-mlbridges
```

Compile ICON executable:

```bash
make -j8
```

### Conda environment

Install a conda environment `iconml`:

```bash
conda env create --file docker/kernel-env-cuda11.yaml
```

[05.09.22] Because of ongoing problems installing the ML libraries using the yml, additionally run

```bash
conda activate iconml
pip install pytorch_lightning
pip install torch==1.10.1+cu113
```

### Use embedded Python (CFFI) bridge

Before job submission, make sure you activate the conda environment and set the python path correctly. 

```bash
cd ~icon-aes # the ICON AES git root dir

source ~/.bashrc
conda activate iconml

export LD_LIBRARY_PATH=${PWD}/externals/mlbridges/lib
export PYTHONPATH=${PWD}/externals/mlbridges/cffi_interface
```

## Developer information

Collection of notes for developers of ICON Python bridges in the context of the voucher.

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
- Document **all** additional changes to paths, runscripts, config files, ... that are necessary to run the bridge
