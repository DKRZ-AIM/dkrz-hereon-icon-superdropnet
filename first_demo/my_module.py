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

@ffi.def_extern()
def i_scalar_field_2d(ptr_nx1, ptr_nx2, ptr_x1, ptr_x2, ptr_phi):
    nx1 = ptr_nx1[0]
    nx2 = ptr_nx2[0]
    x1 = transfer_arrays.asarray(ffi, ptr_x1, shape=(nx1, nx2))
    x2 = transfer_arrays.asarray(ffi, ptr_x2, shape=(nx1, nx2))
    phi = transfer_arrays.asarray(ffi, ptr_phi, shape=(nx1, nx2))

    # the brackets [:] are important here
    phi[:,:] = 0.5 * (x1 - 1.0)**2 + x2**2 + 2*x2

@ffi.def_extern()
def i_scalar_field_3d(ptr_nx1, ptr_nx2, ptr_nx3,
                      ptr_x1, ptr_x2, ptr_x3, ptr_phi):
    nx1 = ptr_nx1[0]
    nx2 = ptr_nx2[0]
    nx3 = ptr_nx3[0]

    x1 = transfer_arrays.asarray(ffi, ptr_x1, shape=(nx1, nx2, nx3))
    x2 = transfer_arrays.asarray(ffi, ptr_x2, shape=(nx1, nx2, nx3))
    x3 = transfer_arrays.asarray(ffi, ptr_x3, shape=(nx1, nx2, nx3))
    phi = transfer_arrays.asarray(ffi, ptr_phi, shape=(nx1, nx2, nx3))

    phi[:,:,:] = 0.5 * (x1 - 1.0)**2 + x2**2 + 2*x2 + 7*x3
