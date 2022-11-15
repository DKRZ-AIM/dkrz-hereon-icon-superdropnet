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

In order to include the C module that serves the pipe worker on ICON's end, it must be added manually as dependency to the build process of ICON. This is done by adding it to icon.mk.in, under "Dependency generation rule for Fortran-to-C-bindings":

```
c_binding.d: icon.mk
	$(silent_DEPGEN):;{ \
	  echo '$(src_prefix)src/io/restart/mo_c_restart_util.@OBJEXT@:| support/util_multifile_restart.@OBJEXT@' && \
	  echo '$(src_prefix)src/io/shared/mo_util_file.@OBJEXT@:| support/util_file.@OBJEXT@' && \

(...)

	  echo '$(src_prefix)src/atm_phy_echam/mo_python_pipes_worker.@OBJEXT@:| $(src_prefix)src/atm_phy_echam/mo_pipes_c.@OBJEXT@'; \
	} >$@
```

You need to run configuration and then make again to have it added.

### MPI via YAC

[Started by CA 13.09.22]

#### Standalone YAC with python bindings

Compile external `yaxt` v0.9.3.1:

```bash
./configure \
--prefix="/work/ka1176/caroline/gitlab/yaxt" \
FC=/sw/spack-levante/openmpi-4.1.2-yfwe6t/bin/mpif90 \
CC=/sw/spack-levante/openmpi-4.1.2-yfwe6t/bin/mpicc  \
CFLAGS='-g -O2' \
FCFLAGS='-g -O2' \
MPI_LAUNCH=/sw/spack-levante/openmpi-4.1.2-yfwe6t/bin/mpiexec
```

Compile external `yac`:

```bash
./configure \
--prefix="/work/ka1176/caroline/gitlab/yac" \
--disable-mpi-checks \
--with-yaxt-root="/work/ka1176/caroline/gitlab/yaxt" \
--with-yaxt-include="/work/ka1176/caroline/gitlab/yaxt/inst_headers" \
--with-yaxt-lib="/work/ka1176/caroline/gitlab/yaxt/src/.libs" \
--with-xml2-include=/usr/include/libxml2 \
--enable-python-bindings \
FC=/sw/spack-levante/openmpi-4.1.2-yfwe6t/bin/mpif90 \
CC=/sw/spack-levante/openmpi-4.1.2-yfwe6t/bin/mpicc  \
CFLAGS='-g -O2' \
FCFLAGS='-g -O2' \
MPI_LAUNCH=/sw/spack-levante/openmpi-4.1.2-yfwe6t/bin/mpiexec

make
```

Install python bindings - for me the check suite was completed when I used my own conda environment (including `mpi4py, cython`) and ran

```bash
conda activate iconml
cd ~yac/python
python setup.py install
make check
```

#### Test YAC standalone

Before submitting the runscript:

```bash
module load python
LD_LIBRARY_PATH="/work/ka1176/caroline/gitlab/yaxt/src/.libs:$LD_LIBRARY_PATH"; export LD_LIBRARY_PATH;
PYTHONPATH="/work/ka1176/caroline/gitlab/yac/python:$PYTHONPATH"; export PYTHONPATH;
```

There is a test setup, currently in `~icon-aes/run` that couples two python scripts.

```
--- Dummy-ICON ----- Python
--- PUT ic2py  ----- GET ic2py -> v
---     |      ----- v' = f(v)
--- GET py2ic  ----- PUT py2ic(v')
```

```bash
 /sw/spack-levante/openmpi-4.1.2-yfwe6t/bin/mpiexec -n 1 python3 yac_caroline_dummy.py : -n 1 python3 yac_caroline_test.py
```

#### YAC usage in ICON-AES

Configure ICON the following way:

```bash
conda activate iconml
./config/dkrz/levante.intel-2021.5.0 --enable-mlbridges --enable-coupling --enable-python-bindings
```

## Prepare the Fortran code (ICON-AES)

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
git clone /work/ka1176/caroline/gitlab/icon-aes-fork/
```

If any of the external components are missing (like `yaxt`) etc, copy them from directly from my repo - this may be a bit slow:

```bash
cd icon-aes-fork

rsync -rvtu /work/ka1176/caroline/gitlab/icon-aes-fork/externals/ ./externals
```

### Configure ICON-AES

Configure the ICON-AES fork to run on Levante and using the dynamic library

```bash
config/dkrz/levante_dev_ml-interface.intel-2021.5.0
```

### Compile ICON

Now, ICON compilation should work immediately

```bash
make -j8
```

### Run the test experiments

Test experiment scripts have been prepared for all working versions of the Python Fortran bridge. The switch is done via the configuration option `echam_ttr_config(:)%icon_ml_bridge='cffi'` with permissible values `fortran, cffi, pipes, mpi`. Note that the latter two are not yet implemented.

Generate the runscripts for the emission test experiments:

```bash
./make_runscripts -s atm_amip_iconml_emission_fortran
./make_runscripts -s atm_amip_iconml_emission_cffi
./make_runscripts -s atm_amip_iconml_emission_pipes # not implemented
./make_runscripts -s atm_amip_iconml_emission_mpi   # not implemented

```

Set the python path to include the library path (TODO: add this to the runscript):

```bash
export PYTHONPATH="$PYTHONPATH:/work/ka1176/caroline/gitlab/2022-03-hereon-python-fortran-bridges/lib/"
export PYTHONPATH="$PYTHONPATH:/work/ka1176/caroline/gitlab/2022-03-hereon-python-fortran-bridges/cffi_interface/"
```

Submit the scripts

```bash
cd run
sbatch -A ka1176 --partition=compute --time=00:10:00 exp.atm_amip_iconml_emission_fortran.run
sbatch -A ka1176 --partition=compute --time=00:10:00 exp.atm_amip_iconml_emission_cffi.run
sbatch -A ka1176 --partition=compute --time=00:10:00 exp.atm_amip_iconml_emission_pipes.run
sbatch -A ka1176 --partition=compute --time=00:10:00 exp.atm_amip_iconml_emission_mpi.run
```

This should produce exactly the same output for all bridges. Check out the Jupyter notebook in `notebooks/TestScenario.ipynb` for a comparison.

## Running the pipes bridge with ICON

The pipes worker needs to be run once on every node that has an ICON process. I.e., in the simplest case, ICON runs with just a handful of processes on a single node, and the pipes worker runs on the same node.

ICON assumes that all MPI tasks run its executable, which makes running the pipe_worker python script a bit hard. Current best solution: Modify the experiment .run script directly by adding a non-blocking call to the pipe_worker right before the ICON executable is called:

```
python ${basedir}/src/atm_phy_echam/pipe_worker.py -s ${SLURM_JOB_ID} -n ${mpi_total_procs} --create-pipes --remove-pipes &
```

The drawback of this solution is that the python process will run in parallel to all other processes without having a process of its own, which can affect performance and/or benchmarking reliability.

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
1. Add a function `add_emi_echam_ttr_cffi` in the new module that does the call to the interface function
1. Define the appropriate counterparts on the Python side and in the C header
1. In `src/atm_phy_echam/mo_interface_echam_ttr`, there is a `SELECT` statement for the `icon_ml_bridge`. Place the call to the new function in the appropriate `CASE`.

Demo run for one month with emission turned on - for using one of the implemented Python Fortran bridges, use the switch in experiment config file:

```bash
sbatch -A ka1176 --nodes=8 --tasks-per-node=24 --partition=compute --time=00:60:00 exp.atm_amip_emission_caroline_month_med.run
```
