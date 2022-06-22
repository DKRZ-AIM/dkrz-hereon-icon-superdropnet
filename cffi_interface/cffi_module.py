# module.py
# contents of this file are written in builder.py
# as argument of ffibuilder.embedding_init_code
from cffi_plugin import ffi
import numpy as np
import transfer_arrays

@ffi.def_extern()
def i_check_interface():
    '''function to check the interface functionality (stub)'''
    print("Check interface")

@ffi.def_extern()
def i_get_emi_number(n):
    n[0] = 17

@ffi.def_extern()
def i_get_emi_float(n):
    n[0] = 18.3

@ffi.def_extern()
def i_hello_world():
    print("Hello from the Python World!")

@ffi.def_extern()
def i_add_one(ptr_nx, ptr_x):
    nx = ptr_nx[0]
    x = transfer_arrays.asarray(ffi, ptr_x, shape=(nx,))
    x[:] += 1.0

@ffi.def_extern()
def i_add_emi_echam_ttr(ptr_jg, ptr_jcs, ptr_jce,
                        ptr_kbdim, ptr_air_mass,
                        ptr_clat, ptr_emi_flux,
                        ptr_emi_lat_n, ptr_emi_lat_s,
                        ptr_dxdt):
    '''
    Function i_add_emi_echam_ttr: Update the ECHAM emission test tracer tendency  

    Parameters:
    ptr_jg : domain index
    ptr_jcs : start at this cell in block of cells
    ptr_jce : stop at this cell in block of cells
    ptr_kbdim : maximum length of block
    ptr_air_mass : air mass per square metre in lowest level
    ptr_clat : latitudes in radiant
    ptr_emi_flux : emission flux
    ptr_emi_lat_n : northern latitude boundary
    ptr_emi_lat_s : southern latitude boundary
    ptr_dxdt : mass mixing ratio tendency in lowest level
    '''

    # print('entered python_interface i_add_emi_echam_ttr')

    jg = ptr_jg[0]
    jcs = ptr_jcs[0] 
    jce = ptr_jce[0] 
    kbdim = ptr_kbdim[0]

    # Correct indices for the slice jcs:jce
    # array index passed from Fortran starts at 1
    # jcs: subtract 1 for Python indexing starting at 0
    # jce: keep unchanged - Fortran includes the last 
    # index in the array slice, Python does not !
    jcs -= 1

    # emission type numbers
    # TODO pass the type?
    emi_flux = ptr_emi_flux[0]
    emi_lat_n = ptr_emi_lat_n[0]
    emi_lat_s = ptr_emi_lat_s[0]

    air_mass = transfer_arrays.asarray(ffi, ptr_air_mass, shape=(kbdim,))
    clat = transfer_arrays.asarray(ffi, ptr_clat, shape=(kbdim,))
    dxdt = transfer_arrays.asarray(ffi, ptr_dxdt, shape=(kbdim,))

    cond = (emi_lat_s <= clat[jcs:jce]) & (clat[jcs:jce] <= emi_lat_n)

    dxdt[jcs:jce][cond] = emi_flux / air_mass[jcs:jce][cond]
    dxdt[jcs:jce][~cond] = 0.0

#@ffi.def_extern()
#def i_print_shape(nx):
#    print(" shape in python", nx, type(nx), nx[0])
#
#@ffi.def_extern()
#def i_print_value(x):
#    print(" value in python", x, type(x), x[0])
#
#@ffi.def_extern()
#def i_print_gpu():
#    if torch.cuda.is_available():
#        print("Has GPU")
#    else:
#        print("Does not have GPU")
#
#@ffi.def_extern()
#def i_scalar_field_1d(ptr_nx, ptr_x, ptr_phi):
#    nx = ptr_nx[0] # TODO check this
#    x = transfer_arrays.asarray(ffi, ptr_x, shape=(nx,))
#    phi = transfer_arrays.asarray(ffi, ptr_phi, shape=(nx,))
#
#    # the brackets [:] are important here
#    phi[:] = 0.5 * x**2
#
#    time.sleep(60)
#
#@ffi.def_extern()
#def i_scalar_field_2d(ptr_nx1, ptr_nx2, ptr_x1, ptr_x2, ptr_phi):
#    nx1 = ptr_nx1[0]
#    nx2 = ptr_nx2[0]
#    x1 = transfer_arrays.asarray(ffi, ptr_x1, shape=(nx1, nx2))
#    x2 = transfer_arrays.asarray(ffi, ptr_x2, shape=(nx1, nx2))
#    phi = transfer_arrays.asarray(ffi, ptr_phi, shape=(nx1, nx2))
#
#    # the brackets [:] are important here
#    phi[:,:] = 0.5 * (x1 - 1.0)**2 + x2**2 + 2*x2
