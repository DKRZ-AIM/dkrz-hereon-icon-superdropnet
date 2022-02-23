# module.py
# contents of this file are written in builder.py
# as argument of ffibuilder.embedding_init_code
from my_plugin import ffi
import numpy as np
import transfer_arrays

@ffi.def_extern()
def i_hello_world():
    print("Hello from the Python World!")

@ffi.def_extern()
def i_scalar_field_1d(nx, ptr_x, ptr_phi):
    print("Entered the python function: i_scalar_field")

    x = transfer_arrays.asarray(ffi, ptr_x, shape=(nx,))
    phi = transfer_arrays.asarray(ffi, ptr_phi, shape=(nx,))

    print(x)

    #phi = 0.5 * x**2
    print("Leaving the python function: i_scalar_field")
