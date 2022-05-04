# Calling Python from ICON

Second project milestone. The aim is to run a Python function from within ICON. The test scenario is the emission tracer tendency introduced in the ICON course.

## Prepare the Python code

### Compile CFFI library

All CFFI related code is compiled into a dynamic library `cffi_plugin.so`. This is done in *this* repository here. Check out the repository 

```bash
git clone git@gitlab.dkrz.de:aim/2022-03-hereon-python-fortran-bridges.git
```

where the latest version of the library can be compiled from the main branch. Run 

```
make
```

which will create the dynamic library in `./lib/cffi_plugin.so`.

### Pipes

TODO

### MPI

TODO

## ICON-AES

### Repository

We are currently working in ICON-AES (branch icon_course_2020, the branch we used in the ICON course). 

#### Forking ICON-AES

Forked the icon-aes git repo to a local copy:

https://gitlab.dkrz.de/k202141/icon-aes-fork

Cleaned the history and push the project to personal git - otherwise it is too large and was rejected.

```bash
# settings
(base) k202141:~/rootgit/icon-aes\ git remote -v
origin  git@gitlab.dkrz.de:k202141/icon-aes-fork.git (fetch)
origin  git@gitlab.dkrz.de:k202141/icon-aes-fork.git (push)
upstream        git@gitlab.dkrz.de:icon/icon-aes.git (fetch)
upstream        NO-PUSH-TO-UPSTREAM (push)
```

#### Working with our fork

Clone from gitlab or directly from my repo on Levante.

```bash
# Option 1
git clone git@gitlab.dkrz.de:k202141/icon-aes-fork.git
# Option 2
git clone /work/ka1176/caroline/gitlab/icon-aes/
```

If any of the external components are missing (like `yaxt`) etc, copy them from directly from my repo (`/work/ka1176/caroline/gitlab/icon-aes/externals`)

### Configure ICON-AES

Configure ICON-AES to run on Levante and using the dynamic library

```bash
cd icon-aes

config/dkrz/levante_dev_ml-interface.intel-2021.5.0
```

### Compile ICON

Now, ICON compilation should work immediately

```bash
make -j8
```

### Run the test experiment

Generate the runscript for the emission test experiment:

```bash
./make_runscripts -s atm_amip_emission_test
```

Set the python path to include the library path (TODO: add this to the runscript):

```bash
export PYTHONPATH="$PYTHONPATH:/work/ka1176/caroline/gitlab/2022-03-hereon-python-fortran-bridges/lib/"
```

Submit the script

```bash
cd run
sbatch -A ka1176 --partition=compute --time=00:10:00 exp.atm_amip_emission_test.run
```

## Background information

### Modules on Levante

These are the modules that were loaded:

```bash
module load intel-oneapi-compilers
module load openmpi/4.1.2-intel-2021.5.0
```

### Test tracer tendency from ICON (CFFI)

We replace the call to `add_emi_echam_ttr` with a call to the corresonding Python function via the interface.

Required changes
1. Create a new module in `src/atm_phy_echam/mo_echam_ttr_cffi.f90`
1. In there, create an interface to an external subroutine `i_add_emi_echam_ttr` with the same arguments as `add_emi_echam_ttr` in `mo_echam_ttr.f90`
1. In addition, pass the values of the `echam_ttr_config` type, as nonstandard datatypes currently not supported in CFFI's `transfer_arrays` module
1. Add a function `add_emi_echam_ttr` in the new module that does the call to the interface function
1. Define the appropriate counterparts on the Python side and in the C header
1. In `src/atm_phy_echam/mo_interface_echam_ttr`, use the new module

Demo run for one month with emission turned on:

```bash
sbatch -A ka1176 --nodes=8 --tasks-per-node=24 --partition=compute --time=00:60:00 exp.atm_amip_emission_caroline_month_med.run
```
