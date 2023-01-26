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


Array ordering
----------------

Fortran uses column-first ordering, C and Python row-first. This is reflected in my_module.py when calling the transfer_arrays.asarray() method. The shape parameter is given in 'backwards' direction, i.e., (nx3, nx2, nx1). This results in the arrays being reshaped to the right order.

