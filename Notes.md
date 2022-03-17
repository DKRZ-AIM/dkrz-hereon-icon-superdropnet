# Notes

Caroline Arnold, DKRZ, 2022

## Info

ICON version: 2.6.1

## Demo using CFFI

The Makefile compiles the shared object (dynamic library) `libplugin.so` along with the Fortran modules. 

### Mistral

To run the demo:

```bash
cd first_demo

module load intel intelmpi

make clean
make my_demo

# Need to add the current directory to the search path for SOs at runtime
# change this to your git root directory
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"/work/ka1176/caroline/gitlab/202
2-03-hereon-python-fortran-bridges/src/"

# need to update the pythonpath (?)
export PYTHONPATH=$PYTHONPATH:"/work/ka1176/caroline/gitlab/202
2-03-hereon-python-fortran-bridges/src/"

# execute
./my_demo
```

Alternatively using MPI on the Fortran side:

```bash
make my_demo_mpi

# alternatively execute using MPI with 4 processes
mpirun -np 4 ./my_demo_mpi
```

Using MPI with multiple nodes in a job script:

```bash
#!/bin/sh

#SBATCH -A ka1176
#SBATCH --partition=compute
#SBATCH --time=00:10:00
#SBATCH --nodes=2
#SBATCH --tasks-per-node=6

module load intel intelmpi

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"/work/ka1176/caroline/gitlab/2022-03-hereon-python-fortran-bridges/first_demo"

mpirun -np 12 ./my_demo_mpi
```

Note from `https://cffi.readthedocs.io/en/latest/embedding.html`:

> You can avoid the LD_LIBRARY_PATH issue if you compile libmy_plugin.so with the path hard-coded inside in the first place.



### Levante

##### Conda environment

```bash
module load python3

# yes it was not so clever to copy paste now the env on levante is called iconml_mistral
conda create -n iconml_mistral
conda activate iconml_mistral
conda install pytorch cpuonly -c pytorch
conda install cffi
```

#### Run the demo

```bash
# load the required modules
module load python3
module load intel-oneapi-compilers intel-oneapi-mpi

cd first_demo

# activate the conda environment
conda init 
source ~/.bashrc
conda activate iconml_mistral

# make builds everything
make

# need to set both these variables to include the current directory
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"..."
export PYTHONPATH=$PYTHONPATH:"..."

# execute
./my_demo
```

#### MPI

Like in the previous section, but add

```bash
export I_MPI_PMI_LIBRARY=/usr/lib64/libpmi.so

srun --pty -A ka1176 --partition=shared --mem=1024 --cpu_bind=verbose --hint=nomultithread --distribution=block:cyclic --nodes=1 --tasks-per-node=2 ./my_demo_mpi
```

TODO: Fails, figure out.

### Changing functions

Useful blog post: https://www.noahbrenowitz.com/post/calling-fortran-from-python/

Using CFFI: https://cffi.readthedocs.io/en/release-0.6/

> Any python function that we want to expose to fortran must be defined in 3 places. First, its C header declaration must be put in header.h. Second, its implementation must be defined in the module string of builder.py—or in an external module as described above. Finally, the fortran code must contain an interface block defining the subroutine.

## CFFI in ICON-AES

Trying to plug cffi into ICON-AES (branch icon_course_2020, the branch we used in the ICON course). Forked the icon-aes git repo to a local copy.

TODO: Push the project to personal git - it is too large and was rejected

```bash
# settings
(base) k202141:~/rootgit/icon-aes\ git remote -v
origin  git@gitlab.dkrz.de:k202141/icon-aes-fork.git (fetch)
origin  git@gitlab.dkrz.de:k202141/icon-aes-fork.git (push)
upstream        git@gitlab.dkrz.de:icon/icon-aes.git (fetch)
upstream        NO-PUSH-TO-UPSTREAM (push)

# ----------------------------
# configure
# ----------------------------

# these were missing - copied from ICON course directory
rsync -rvtu /work/mh1049/icon_course_2020/b380623/icon-aes/externals/ externals/

config/dkrz/mistral.intel
```

For now, manually compile the shared object

```bash
cd src/ml_interface
module purge gcc # conflicting gcc
python builder.py
```

There seems to be a conflict in gcc, during the compilation of the ICON executable, the following modules are loaded:

```
module load gcc/6.4.0
module load intel/17.0.6
module load openmpi/2.0.2p1_hpcx-intel14
```

Need to figure out how to get this into the config, for now editing the `icon.mk` file manually (`icon.mk.backup` is the original version):

```bash
(base) k202141:~/rootgit/icon-aes\ diff icon.mk icon.mk.backup
55,56c55,56
< LDFLAGS= -L/sw/rhel6-x64/hdf5/hdf5-1.8.18-parallel-openmpi2-intel14/lib -L/sw/rhel6-x64/netcdf/netcdf_c-4.4.0-parallel-openmpi2-intel14/lib -L/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.3-parallel-openmpi2-intel14/lib -L/sw/rhel6-x64/grib_api/grib_api-1.15.0-gcc48/lib -mkl=sequential -Wl,-rpath -Wl,/sw/rhel6-x64/hdf5/hdf5-1.8.18-parallel-openmpi2-intel14/lib -Wl,-rpath -Wl,/sw/rhel6-x64/netcdf/netcdf_c-4.4.0-parallel-openmpi2-intel14/lib -Wl,-rpath -Wl,/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.3-parallel-openmpi2-intel14/lib -Wl,-rpath -Wl,/sw/rhel6-x64/grib_api/grib_api-1.15.0-gcc48/lib -L/work/ka1176/caroline/gitlab/icon-aes/src/ml_interface
< BUNDLED_LIBFILES= externals/self/src/libself.a externals/yac/src/libyac.a externals/mtime/src/.libs/libmtime.a externals/cdi/src/.libs/libcdi_f2003.a externals/cdi/src/.libs/libcdi.a  /work/ka1176/caroline/gitlab/icon-aes/src/ml_interface/libplugin.so
---
> LDFLAGS= -L/sw/rhel6-x64/hdf5/hdf5-1.8.18-parallel-openmpi2-intel14/lib -L/sw/rhel6-x64/netcdf/netcdf_c-4.4.0-parallel-openmpi2-intel14/lib -L/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.3-parallel-openmpi2-intel14/lib -L/sw/rhel6-x64/grib_api/grib_api-1.15.0-gcc48/lib -mkl=sequential -Wl,-rpath -Wl,/sw/rhel6-x64/hdf5/hdf5-1.8.18-parallel-openmpi2-intel14/lib -Wl,-rpath -Wl,/sw/rhel6-x64/netcdf/netcdf_c-4.4.0-parallel-openmpi2-intel14/lib -Wl,-rpath -Wl,/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.3-parallel-openmpi2-intel14/lib -Wl,-rpath -Wl,/sw/rhel6-x64/grib_api/grib_api-1.15.0-gcc48/lib
> BUNDLED_LIBFILES= externals/self/src/libself.a externals/yac/src/libyac.a externals/mtime/src/.libs/libmtime.a externals/cdi/src/.libs/libcdi_f2003.a externals/cdi/src/.libs/libcdi.a


```

Now try to compile ICON including the dynamic library:

```
make -j 8 # on shared node
```
