## Notes

### Compile

TODO: All this goes in the Makefile

1. Compile the shared object (dynamic library): `python builder.py`
2. Execute the current Makefile
3. Need to add the current directory to the search path for SOs at runtime:

```bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"/work/ka1176/caroline/gitlab/202
2-03-hereon-python-fortran-bridges/src/"
```

### Changing functions

https://www.noahbrenowitz.com/post/calling-fortran-from-python/

Any python function that we want to expose to fortran must be defined in 3 places. First, its C header declaration must be put in header.h. Second, its implementation must be defined in the module string of builder.py—or in an external module as described above. Finally, the fortran code must contain an interface block defining the subroutine.
