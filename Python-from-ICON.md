# Calling Python from ICON

Second project milestone. The aim is to run a Python function from within ICON. The test scenario is the emission tracer tendency introduced in the ICON course.

## CFFI in ICON-AES

### Setup

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

### Hello world from ICON

Make a runscript that calls the ECHAM test tracer print function and submit:

```
./make_runscripts -s atm_amip_test_caroline

cd run
sbatch -A ka1176 --partition=compute --time=00:10:00 exp.atm_amip_caroline_test.run
```

In the ECHAM test tracer configuration print routine, there is a call to `i_hello_world` that calls the corresponding Python function. In the log file, you should find the line 

```bash
Hello from the Python world!
```

executed once per MPI thread.

### Test tracer tendency from ICON

TODO 
