## Notes

### Compile

The Makefile compiles the shared object (dynamic library) `libplugin.so` along with the Fortran modules. To run the demo:

```bash
cd src

module load intel

make my_demo

# Need to add the current directory to the search path for SOs at runtime
# change this to your git rooot directory
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"/work/ka1176/caroline/gitlab/202
2-03-hereon-python-fortran-bridges/src/"

# execute
./my_demo
```

Note from `https://cffi.readthedocs.io/en/latest/embedding.html`:

> You can avoid the LD_LIBRARY_PATH issue if you compile libmy_plugin.so with the path hard-coded inside in the first place.

### Changing functions

Useful blog post: https://www.noahbrenowitz.com/post/calling-fortran-from-python/

Using CFFI: https://cffi.readthedocs.io/en/release-0.6/

> Any python function that we want to expose to fortran must be defined in 3 places. First, its C header declaration must be put in header.h. Second, its implementation must be defined in the module string of builder.py—or in an external module as described above. Finally, the fortran code must contain an interface block defining the subroutine.
