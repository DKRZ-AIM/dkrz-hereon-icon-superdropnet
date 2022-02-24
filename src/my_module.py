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
def i_print_shape(nx):
    print(" shape in python", nx, type(nx), nx[0])

@ffi.def_extern()
def i_print_value(x):
    print(" value in python", x, type(x), x[0])

@ffi.def_extern()
def i_scalar_field_1d(ptr_nx, ptr_x, ptr_phi):
    nx = ptr_nx[0] # TODO check this
    x = transfer_arrays.asarray(ffi, ptr_x, shape=(nx,))
    phi = transfer_arrays.asarray(ffi, ptr_phi, shape=(nx,))

    # the brackets [:] are important here
    phi[:] = 0.5 * x**2
