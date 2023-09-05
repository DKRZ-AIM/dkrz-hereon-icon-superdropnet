# module.py
# contents of this file are written in builder.py
# as argument of ffibuilder.embedding_init_code

import libcffi
from libcffi import ffi
import numpy as np
import os
import transfer_arrays
import time
import torch

import sys
from solvers.moment_solver import simulation_forecast

import models.plModel as plm

# initialization code
# TODO execute this in the load_pretrained_model function
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

    
pl_model = plm.LightningModel(inputs_mean=inputs_mean, inputs_std=inputs_std,
                            updates_mean=updates_mean, updates_std=updates_std) 

model_path = '/work/ka1176/caroline/gitlab/icon-aes/externals/mlbridges/cffi_interface/trained_models/best_model.ckpt'
trained_model = pl_model.load_from_checkpoint(model_path)
# end of initialization code

@ffi.def_extern()
def i_check_interface(ptr_istate):
    '''
    Check if the interface is working

    Pass istate parameter back and forth

    Assign non zero value here, to be
    checked on the Fortran side
    '''
    ptr_istate[0] = 1

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
def i_load_pretrained_model(ptr_handle, ptr_trained_model_path):
    """
    Load a pretrained model and attach it to a handle
    to keep it alive throughout execution

    Parameters:
    ptr_handle : the handle
    ptr_trained_model_path : absolute path to trained model (str)
    """

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

    #Add a check for GPU
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    
    #NN Model initalization
    pl_model = plm.LightningModel(inputs_mean=inputs_mean, inputs_std=inputs_std,
                            updates_mean=updates_mean, updates_std=updates_std, device=device) 

    model_path = ffi.string(ptr_trained_model_path).decode("UTF-8")
    trained_model = pl_model.load_from_checkpoint(model_path)

    handle = ffi.new_handle(trained_model)
    #ptr_handle[:] = ffi.new_handle(trained_model)

@ffi.def_extern()
def i_retrieve_model_from_handle(ptr_handle):
    """
    Test function to retrieve an already loaded pretrained
    model from handle

    Parameters:
    ptr_handle : the handle
    """

    retrieved_model = ffi.from_handle(ptr_handle[:])



@ffi.def_extern()
def i_warm_rain_nn(ptr_dim_i, ptr_dim_k, ptr_n_moments, 
                   ptr_current_moments, ptr_new_moments,
                   ptr_trained_model_path,
                   ptr_t0, ptr_t1, 
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
    ptr_t0              : start time
    ptr_t1              : end time

    ptr_istate : flag to indicate state
       0 : nothing happened
       1 : input moments were zero, no update
       2 : ML inference update
       3 : error code (encountered None value in input moments)
       4 : error code (encountered None value in output moments)
       5 : error code (encountered value > 1e20 in output moments)

    """

    dim_i = ptr_dim_i[0]
    dim_k = ptr_dim_k[0]
    n_moments = ptr_n_moments[0]

    # Shape takes into account that C reads row major
    # while the array is saved in Fortran column major
    # memory layout
    shape = (n_moments, dim_k, dim_i) 

    # add timer
    ptr_t0[0] = time.time()
    current_moments = transfer_arrays.asarray(ffi, ptr_current_moments, shape=shape)
    new_moments     = transfer_arrays.asarray(ffi, ptr_new_moments, shape=shape)
    ptr_t1[0] = time.time()

    new_moments[:,:,:] = 0.0

    # TODO retrieve trained_model from handle
    # currently it is initialized at the top of the module
    # and persists

    # ML inference only if input moments are non zero
    if np.all(current_moments == 0.0):
        new_moments[:,:,:] = 0.0
        ptr_istate[0] = 1

    # Catch NONE moments here and stop evaluation on Fortran side
    elif np.any(np.isnan(current_moments)):
        ptr_istate[0] = 3

    else:
        # current_moments shape: moments x dim_k x dim_i
        # solver expects:        dim_ik  x moments
        # Solver gives (fc_moments): dim_ik x moments
        #We change output to the correct shape and save in new_moments
        
        moments_shape = current_moments.shape
        swapped_moments = np.swapaxes(current_moments,0, 2).reshape(-1, 4)
        
        new_forecast = simulation_forecast(swapped_moments, trained_model,
                                           inputs_mean, inputs_std,
                                           updates_mean, updates_std)
        new_forecast.test()

        fc_moments = np.swapaxes(new_forecast.moments_out, 0, 1)
        fc_moments = fc_moments.reshape(moments_shape)

        new_moments[:, :, :] = fc_moments

        ptr_istate[0] = 2

        if np.any(np.isnan(new_moments)):
            ptr_istate[0] = 4
        elif np.any(new_moments>1e20):
            ptr_istate[0] = 5

@ffi.def_extern()
def i_checksum(ptr_dim_i, ptr_dim_k, ptr_n_moments, 
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
       4 : successful checksum code
       5 : not successful checksum code

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
