# module.py
# contents of this file are written in builder.py
# as argument of ffibuilder.embedding_init_code

from cffi_plugin import ffi
import numpy as np
import os
import transfer_arrays
import pytorch_lightning as pl

import sys
sys.path.append('/work/ka1176/caroline/gitlab/2022-03-hereon-python-fortran-bridges/cffi_interface')
from solvers.moment_solver import simulation_forecast
sys.path.append('/work/ka1176/caroline/gitlab/2022-03-hereon-python-fortran-bridges/cffi_interface/models')
sys.path.append('/work/ka1176/caroline/gitlab/2022-03-hereon-python-fortran-bridges/cffi_interface')

import models.plModel as plm


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

    # Use [:] notation to copy data into the buffer
    # https://cffi.readthedocs.io/en/latest/ref.html?highlight=ffi.buffer#ffi-buffer-ffi-from-buffer
    dxdt[jcs:jce][cond] = emi_flux / air_mass[jcs:jce][cond]
    dxdt[jcs:jce][~cond] = 0.0

@ffi.def_extern()
def i_warm_rain_nn(ptr_dim_i, ptr_dim_k, ptr_n_moments, 
                   ptr_current_moments, ptr_new_moments,
                   ptr_istate):
    """
    Call the pretrained warm rain network for inference for a given ikslice
    and time step. Calculates the new moments for cloud and rain.

    Parameters:

    ptr_dim_i           : dimension along i (of ikslice)
    ptr_dim_k           : dimension along k (of ikslice)
    ptr_n_moments       : number of moments (4 for warm rain)
    ptr_current_moments : current moments in the order of 
      cloud%q, cloud%n, rain%q, rain%n
    ptr_new_moments     : new moments

    ptr_istate : flag to indicate state
       0 : nothing happened
       1 : input moments were zero, no update
       2 : ML inference update
       3 : error code (TODO)

    """

    dim_i = ptr_dim_i[0]
    dim_k = ptr_dim_k[0]
    n_moments = ptr_n_moments[0]

    # Shape takes into account that C reads row major
    # while the array is saved in Fortran column major
    # memory layout
    shape = (n_moments, dim_k, dim_i) 

    current_moments = transfer_arrays.asarray(ffi, ptr_current_moments, shape=shape)
    new_moments     = transfer_arrays.asarray(ffi, ptr_new_moments, shape=shape)

    new_moments[:,:,:] = 0.0
    
    inputs_mean = np.asarray([[0.0002621447787797809, 51128093.51524663,
                    0.0003302890736022656, 5194.251154308974,
                    0.5566250557023539, 4.8690682855354596e-12,
                    0.0005924338523807814, 1.0848856769219835e-05,
                    2.0193905073168525]])

    inputs_std = np.asarray([[0.0003865559774857862, 86503916.13808665,
                    0.00041369562655559327, 19127.947970150628,
                    0.46107363560819126, 3.873092422358367e-12,
                    0.00042887039563850967, 1.920461805101116e-06,
                    1.3098055608321857]])

    updates_mean = np.asarray([[-8.527820407019667e-08, -13961.459867976775,
                    8.527678028525988e-08, 0.010221931180955181]])

    updates_std = np.asarray([[3.600841676033818e-07, 55095.904252313965,
                    3.6008419243808887e-07, 68.6678997504877]])

    #NN Model initalization
    pl_model = plm.LightningModel(inputs_mean=inputs_mean, inputs_std=inputs_std,
                            updates_mean=updates_mean, updates_std=updates_std) 

    hard_coded_path = '/work/ka1176/caroline/gitlab/2022-03-hereon-python-fortran-bridges/cffi_interface'
    trained_model = pl_model.load_from_checkpoint(hard_coded_path + "/trained_models/best_model.ckpt")

    # ML inference only if input moments are non zero
    if np.all(current_moments == 0.0):
        new_moments[:,:,:] = 0.0
        ptr_istate[0] = 1

    else:
        # TODO use batch prediction here instead of the nested loop
        # new_moments[:, :, :] = new_forecast.moments_out[0, :, :, :]

        for i in range(dim_i):
            for k in range(dim_k):

                # If only one of the batch elements is all zero
                # still need to assign the new moments to zero
                if np.allclose(current_moments[:,k,i], 0.0):
                    new_moments[:,k,i] = 0.0
                    continue

                #Solver class initalized and new moment calculated
                new_forecast = simulation_forecast(
                  current_moments[:, k, i], trained_model,inputs_mean, inputs_std
                )
                new_forecast.test()

                new_moments[:, k, i] = new_forecast.moments_out[0, :]

        ptr_istate[0] = 2

@ffi.def_extern()
def i_warm_rain_py(ptr_dim_i, ptr_dim_k, ptr_n_moments, 
                   ptr_current_moments, ptr_new_moments,
                   ptr_istate):
    """
    Python code that depends on index orders.
    Use only for debugging the row vs column major issue.

    Parameters:

    ptr_dim_i           : dimension along i (of ikslice)
    ptr_dim_k           : dimension along k (of ikslice)
    ptr_n_moments       : number of moments (4 for warm rain)
    ptr_current_moments : current moments in the order of 
      cloud%q, cloud%n, rain%q, rain%n
    ptr_new_moments     : new moments

    ptr_istate : flag to indicate state
       0 : nothing happened
       4 : successful classic code
       5 : not successful classic code

    """

    ptr_istate[0] = 5 # default

    dim_i = ptr_dim_i[0]
    dim_k = ptr_dim_k[0]
    n_moments = ptr_n_moments[0]

    # Shape takes into account that C reads row major
    # while the array is saved in Fortran column major
    # memory layout
    shape = (n_moments, dim_k, dim_i) 

    current_moments = transfer_arrays.asarray(ffi, ptr_current_moments, shape=shape)
    new_moments     = transfer_arrays.asarray(ffi, ptr_new_moments, shape=shape)

    for i in range(dim_i):
        for k in range(dim_k):
            for l in range(n_moments):
                new_moments[l, k, i] = 100 * i + 10 * k + 1 * l

    ptr_istate[0] = 4
