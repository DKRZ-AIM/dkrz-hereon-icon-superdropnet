How to compile and run first_demo
===================================


    export LD_LIBRARY_PATH=${PWD}
    module load intel_oneapi_compilers
    module load intel_oneapi_mpi

This should enable make to run through correctly and ./my_demo to work.

builder.py
------------

This contains all the code to create the libplugin.so library from my_module.py and a C header (inside builder.py) using CFFI.
To understand more about the details of the calls in builder.py, refer to Noah Brenowitz' blog entry:
https://www.noahbrenowitz.com/post/calling-fortran-from-python/

Calling this directly or via make will generate several files, e.g., my_plugin.c and plugin.h. We do not edit these directly and can ignore them.


