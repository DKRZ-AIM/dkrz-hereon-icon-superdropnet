#!/usr/bin/env python3

'''
Main python program for running ICON-Atmo and
the ML component in coupled mode via yac

Fields:
(1) example_field_icon_to_python (here: get)
(2) example_field_python_to_icon (here: put)
(3) moments_ic2py
(4) moments_py2ic
'''

from yac import YAC, Field, UnstructuredGrid, Location
import xarray as xr
import numpy as np
import time
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

from datetime import datetime, timedelta

# ICON logs to stderr
sys.stdout = sys.stderr

import torch

'''
YAC events (info) from yac
'''
yac_action_type = dict(
  NONE               = 0,
  COUPLING           = 1,
  RESTART            = 2,
  GET_FOR_RESTART    = 3,
  PUT_FOR_RESTART    = 4,
  GET_FOR_CHECKPOINT = 5,
  PUT_FOR_CHECKPOINT = 6,
  OUT_OF_BOUND       = 7,
)

def main(args):
    routine = 'yac_interface_main.py'
    print(f'{routine:s} starts execution')

    # Calculation
    # -----------
    # Grid cells are divided in blocks of nproma
    # --> number of horizontal slices is ceil(ngrid / nproma)
    # where the last slice may have < nproma entries
    #
    # Horizontally, the ikslice contains one single level
    # 70 levels, no call in the level 1
    #
    # --> n_ikslice steps in each time step
    nproma = 880
    ngrid  = 880
    nlev   = 70
    n_ikslice = int(np.ceil(ngrid / nproma) * (nlev - 1))

    print(f'{routine:s}: nproma = {nproma}')
    print(f'{routine:s}: ngrid = {ngrid}')
    print(f'{routine:s}: nlev = {nlev}')
    print(f'{routine:s}: n_ikslice = {n_ikslice}')

    
    yac = YAC("coupling.xml", "coupling.xsd")
    comp = yac.def_comp(f"WarmRainML")
    
    f = xr.open_dataset('/pool/data/ICON/grids/public/mpim/Torus_Triangles_20x22_5000m/Torus_Triangles_20x22_5000m.nc')

    print(f'{routine:s} Torch CUDA is available:', torch.cuda.is_available())
    
    vertices_per_cell = f.dims["nv"]
    nbr_cells = f.dims["cell"]
    
    vtx = np.array([f["clon_vertices"].values.ravel(), f["clat_vertices"].values.ravel()])
    vtx, cellidx = np.unique(vtx, return_inverse=True, axis=1)
    
    grid = UnstructuredGrid("warm_rain_grid", np.ones(nbr_cells)*vertices_per_cell,
                            vtx[0,:], vtx[1,:], cellidx)
    
    
    points = grid.def_points(Location.CELL, f["clon"], f["clat"])
    
    print(f'{routine:s}: Created grid points for YAC')
    
    example_field_ic2py = Field.create("example_field_icon_to_python", comp, points)
    example_field_py2ic = Field.create("example_field_python_to_icon", comp, points)

    all_moments_ic2py = []
    all_moments_py2ic = []

    for zlev in range(1, nlev):
        print(f'{routine:s}: Registering fields for zlev {zlev}')
        moments_ic2py = Field.create(f"moments_ic2py_zlev_{zlev}", comp, points)
        moments_py2ic = Field.create(f"moments_py2ic_zlev_{zlev}", comp, points)

        all_moments_ic2py.append(moments_ic2py)
        all_moments_py2ic.append(moments_py2ic)

    for ff in [example_field_ic2py, example_field_py2ic] + all_moments_ic2py + all_moments_py2ic:
        print(f'{routine:s} Field {ff.name} with ID {ff.field_id}, collection size {ff.collection_size} and size {ff.size}')

    print(f'{routine:s} Starting YAC search')
    
    yac.search()
    
    print(f'{routine:s}: Finished YAC search')
    
    info = -1

    test_buffer = np.zeros([1, example_field_ic2py.size])
    test_buffer -= 1
    
    test_buffer, info = example_field_ic2py.get(test_buffer)
    print(f'{routine:s}: max(test_buffer) = {np.max(test_buffer)}')
    
    assert np.sum(test_buffer) == 880 * 879 / 2
    
    test_buffer += 1.0
    
    assert np.sum(test_buffer) == 880 * 881 / 2
    
    example_field_py2ic.put(test_buffer)
    
    print(f'{routine:s}: Done with put/get example field exchange. Info = {info}')

    # TODO pass the numer of time loop steps in coupling config
    # Calculation
    # -----------
    # From tstart, add tstep until > tend

    print(f'{routine:s}: Start date {yac.start_datetime} / End date {yac.end_datetime}')

    st0 = yac.start_datetime
    stn = yac.end_datetime
    
    # numeric values
    nt0 = datetime.fromisoformat(st0)
    ntn = datetime.fromisoformat(stn)

    ndt = timedelta(seconds=20)
    print(f'{routine:s} Hard coded time delta {ndt}')

    nti = nt0 # loop time

    while nti < ntn:
        print(f'{routine:s}: starting ml component time step {nti}')

        for iz, zlev in enumerate(range(1, nlev)):
            mom_buffer, info = all_moments_ic2py[iz].get()
            print(f'{routine:s}: pydebug - get')
            print(f'{routine:s}: iz = {iz}, zlev = {zlev}, mom_buffer.shape = ', mom_buffer.shape, info)
            new_mom_buffer = np.empty(mom_buffer.shape)
            assert info != yac_action_type['OUT_OF_BOUND'], print("out-of-bound-event")

            # ML inference only if input moments are non zero
            if np.allclose(mom_buffer, 0.0):
                print(f'{routine:s}: all moments zero - no network applied. max = {np.max(np.abs(mom_buffer))}')
                new_mom_buffer[:,:] = 0.0

            # Catch NONE moments here and stop evaluation on Fortran side
            elif np.any(np.isnan(mom_buffer)):
                print(f'{routine:s}: nan in moments - all set to -1.0')
                new_mom_buffer[:,:] = -1.0

            else:
                print(f'{routine:s}: moments non zero - network applied. max = {np.max(np.abs(mom_buffer))}')
                # current_moments shape: moments x dim_ik
                # solver expects:        dim_ik  x moments
                # Solver gives (fc_moments): dim_ik x moments
                #We change output to the correct shape and save in new_moments
        
                moments_shape = mom_buffer.shape
                swapped_moments = np.swapaxes(mom_buffer, 0, 1)
        
                new_forecast = simulation_forecast(swapped_moments, trained_model,
                                                   inputs_mean, inputs_std,
                                                   updates_mean, updates_std)
                new_forecast.test()

                fc_moments = np.swapaxes(new_forecast.moments_out, 0, 1)
                fc_moments = fc_moments.reshape(moments_shape)

                new_mom_buffer[:, :] = fc_moments
                if np.any(np.isnan(new_mom_buffer)):
                    np.save(f'before_{nti}_{zlev}', mom_buffer)
                    np.save(f'after_{nti}_{zlev}', new_mom_buffer)
                    raise ValueError("NaN in calculated moments")
                if np.any(new_mom_buffer>1e20):
                    np.save(f'before_{nti}_{zlev}', mom_buffer)
                    np.save(f'after_{nti}_{zlev}', new_mom_buffer)
                    raise ValueError("Calculated moments > 1e20")
            #
            all_moments_py2ic[iz].put(new_mom_buffer)
            print(f'{routine:s}: pydebug - put')
            if not np.allclose(mom_buffer, 0):
                np.save(f'before_{nti}_{zlev}', mom_buffer)
                np.save(f'after_{nti}_{zlev}', new_mom_buffer)

        print(f'{routine:s}: finished ml component time step {nti}')
        nti = nti+ ndt

if __name__=='__main__':
    main(sys.argv[1:])

