## Notes

### Compile

The Makefile compiles the shared object (dynamic library) `libplugin.so` along with the Fortran modules. 

#### Mistral

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
mpirun -np 4 ./my_demo
```

Note from `https://cffi.readthedocs.io/en/latest/embedding.html`:

> You can avoid the LD_LIBRARY_PATH issue if you compile libmy_plugin.so with the path hard-coded inside in the first place.

#### Levante

```bash
module load python3
module load intel-oneapi-compilers

cd first_demo

make

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"..."
export PYTHONPATH=$PYTHONPATH:"..."

./my_demo
```

### Changing functions

Useful blog post: https://www.noahbrenowitz.com/post/calling-fortran-from-python/

Using CFFI: https://cffi.readthedocs.io/en/release-0.6/

> Any python function that we want to expose to fortran must be defined in 3 places. First, its C header declaration must be put in header.h. Second, its implementation must be defined in the module string of builder.py—or in an external module as described above. Finally, the fortran code must contain an interface block defining the subroutine.
