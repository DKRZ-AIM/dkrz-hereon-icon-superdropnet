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
    moments_ic2py = Field.create("moments_ic2py", comp, points)
    moments_py2ic = Field.create("moments_py2ic", comp, points)

    for f in [example_field_ic2py, example_field_py2ic, moments_ic2py, moments_py2ic]:
        print(f'{routine:s} Field {f.name} with ID {f.field_id} and size {f.size}')
    
    yac.search()
    
    print(f'{routine:s}: Finished YAC search')
    
    info = -1
    
    test_buffer = np.zeros([1, example_field_ic2py.size])
    test_buffer -= 1
    
    test_buffer, info = example_field_ic2py.get(test_buffer)
    
    assert np.sum(test_buffer) == 880 * 879 / 2
    
    test_buffer += 1.0
    
    assert np.sum(test_buffer) == 880 * 881 / 2
    
    example_field_py2ic.put(test_buffer)
    
    print(f'{routine:s}: Done with put/get example field exchange. Info = {info}')

    # TODO pass number of steps in coupling config
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
    nlev   = 2 # ! 70
    n_ikslice = int(np.ceil(ngrid / nproma) * (nlev - 1))

    print(f'{routine:s}: nproma = {nproma}')
    print(f'{routine:s}: ngrid = {ngrid}')
    print(f'{routine:s}: nlev = {nlev}')
    print(f'{routine:s}: n_ikslice = {n_ikslice}')

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

        for i in range(n_ikslice):
            mom_buffer, info = moments_ic2py.get()
            print(f'{routine:s}: pydebug - get')
            print(f'{routine:s}: i = {i}, mom_buffer.shape = ', mom_buffer.shape, info)
            #assert info != yac_action_type['OUT_OF_BOUND'], print("out-of-bound-event")
            moments_py2ic.put(mom_buffer)
            print(f'{routine:s}: pydebug - put')

        print(f'{routine:s}: finished ml component time step {nti}')
        nti = nti+ ndt

if __name__=='__main__':
    main(sys.argv[1:])

