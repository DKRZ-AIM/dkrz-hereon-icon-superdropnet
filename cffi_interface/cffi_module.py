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

    dxdt[jcs:jce][cond] = emi_flux / air_mass[jcs:jce][cond]
    dxdt[jcs:jce][~cond] = 0.0

@ffi.def_extern()
def i_warm_rain_nn(ptr_ncells, ptr_nlevels, 
                   ptr_istart, ptr_iend, ptr_kstart, ptr_kend,
                   ptr_n_cloud_t0, ptr_q_cloud_t0,
                   ptr_n_rain_t0,  ptr_q_rain_t0,  
                   ptr_n_cloud_t1, ptr_q_cloud_t1, 
                   ptr_n_rain_t1,  ptr_q_rain_t1
    ):
    """
    Call the pretrained warm rain network for inference for a given ikslice
    and time step. Calculates the new moments for cloud and rain.

    Parameters:

    ptr_ncells : number of cells
    ptr_nlevels : number of levels
    ptr_istart : start of ikslice along cell dimension
    ptr_iend    : end of ikslice along cell dimension
    ptr_kstart  : start of ikslice along level dimension
    ptr_kend    : end of ikslice along level dimension

    ptr_n_cloud_t0 : cloud % n at previous time step
    ptr_q_cloud_t0 : cloud % q at previous time step
    ptr_n_rain_t0  : rain % n at previous time step
    ptr_q_rain_t0  : rain % q at previous time step
    ptr_n_cloud_t1 : cloud % n at next time step
    ptr_q_cloud_t1 : cloud % q at next time step
    ptr_n_rain_t1  : rain % n at next time step
    ptr_q_rain_t1  : rain % q at next time step

    Updates:
    ptr_n_cloud_t1, ptr_q_cloud_t1, ptr_n_rain_t1, ptr_q_rain_t1
    """

    ncells = ptr_ncells[0]
    nlevels = ptr_nlevels[0]
    istart = ptr_istart[0]
    iend = ptr_iend[0]
    kstart = ptr_kstart[0]
    kend = ptr_kend[0] 
    
    # python indexing does not include last element
    istart -= 1
    kstart -= 1

    shape = (ncells, nlevels)
    n_cloud_t0 = transfer_arrays.asarray(ffi, ptr_n_cloud_t0, shape = shape)
    q_cloud_t0 = transfer_arrays.asarray(ffi,ptr_q_cloud_t0, shape = shape)
    n_rain_t0 = transfer_arrays.asarray(ffi,ptr_n_rain_t0, shape = shape)
    q_rain_t0 = transfer_arrays.asarray(ffi,ptr_q_rain_t0, shape = shape)

    n_cloud_t1 = transfer_arrays.asarray(ffi,ptr_n_cloud_t1, shape=shape)
    q_cloud_t1 = transfer_arrays.asarray(ffi,ptr_q_cloud_t1, shape = shape)
    n_rain_t1 = transfer_arrays.asarray(ffi,ptr_n_rain_t1, shape=shape)
    q_rain_t1 = transfer_arrays.asarray(ffi,ptr_q_rain_t1, shape = shape)
    
    n_cloud_t0 = n_cloud_t0[istart:iend, kstart:kend]
    q_cloud_t0 = q_cloud_t0[istart:iend, kstart:kend]
    q_rain_t0  = q_rain_t0[istart:iend, kstart:kend]
    n_rain_t0  = n_rain_t0[istart:iend, kstart:kend]
    
    all_fortran_moments = np.stack((q_cloud_t0,n_cloud_t0,q_rain_t0,n_rain_t0),axis =-1)
    
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

    q_cloud_t1[:,:] = 0.0
    n_cloud_t1[:,:] = 0.0
    q_rain_t1[:,:]  = 0.0
    n_rain_t1[:,:]  = 0.0

    #Loop starts here 
    for i in range (istart, iend):
        for k in range (kstart, kend):
            # indexing issue
            # fortran_moments = all_fortran_moments[i,k - kstart,:]  

            fortran_moments = all_fortran_moments[i, 0, :]

            # ML inference only if input moments are non zero
            if np.all(fortran_moments == 0.0):
                q_cloud_t1[i,k] = 0.0
                n_cloud_t1[i,k] = 0.0
                q_rain_t1[i,k]  = 0.0
                n_rain_t1[i,k]  = 0.0

            else:
                #Solver class initalized and new moment calculated
                new_forecast = simulation_forecast(
            fortran_moments, trained_model,inputs_mean, inputs_std
            )
                new_forecast.test()

                q_cloud_t1[i,k] = new_forecast.moments_out[0, 0]
                n_cloud_t1[i,k] = new_forecast.moments_out[0, 1]
                q_rain_t1[i,k]  = new_forecast.moments_out[0, 2]
                n_rain_t1[i,k]  = new_forecast.moments_out[0, 3]
           
           

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
