# ML in ICON

Third project milestone: Neural network inference with a pretrained model at runtime in ICON-AES. 

## Usage information

### ICON-AES branch

We forked the ICON-AES master and created a new branch called `icon-aes-ml-bridges`. Obtain the code here:

```bash
git clone --recursive -b icon-aes-ml-bridges git@gitlab.dkrz.de:k202141/icon-aes.git
```

[2022-10-21] TODO: `autoconf` tools were missing from subrepository. Add them here:

````bash
cd externals/mlbridges
automake --add-missing
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
pip install torch==1.11.0 torchvision==0.12.0 torchaudio==0.11.0
```

Fallback: install current stable version with CUDA toolkit 11.3 from https://pytorch.org/get-started/locally/.

### Use embedded Python (CFFI) bridge

Before job submission, make sure you activate the conda environment and set the python path correctly. 

```bash
cd ~icon-aes # the ICON AES git root dir

source ~/.bashrc
conda activate iconml

export PYTHONPATH=${PWD}/externals/mlbridges/cffi_interface
```

### Test 

To see whether your installation was successful, run the bubble experiments:

```bash
./make_runscripts aes_bubble_2mom_fortran
./make_runscripts aes_bubble_2mom_cffi
```

### Standalone usage

To create the library for usage outside of ICON:

```bash
autoreconf
./configure
make
```

The compiled library can be imported in python like a regular module

```python
from libcffi import ffi, lib
```

For examples, see notebook `DebugCFFI.ipynb`.

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
run/exp.aes_bubble_2mom_fortran # for the fortran case
run/exp.aes_bubble_2mom_cffi    # for the cffi case
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
