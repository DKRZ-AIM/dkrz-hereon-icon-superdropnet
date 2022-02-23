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
