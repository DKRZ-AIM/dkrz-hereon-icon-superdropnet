#!/usr/bin/env python3

'''
Test setup for coupling a Python routine to ICON 
via YAC

Fields:
(1) example_field_icon_to_python (here: get)
(2) example_field_python_to_icon (here: put)
'''

from yac import YAC, Field, UnstructuredGrid, Location
import xarray as xr
import numpy as np
import time

routine = '*** PYTHON yac_caroline_test'

yac = YAC("coupling.xml", "coupling.xsd")
comp = yac.def_comp(f"WarmRainML")

f = xr.open_dataset('/pool/data/ICON/grids/public/mpim/Torus_Triangles_20x22_5000m/Torus_Triangles_20x22_5000m.nc')

vertices_per_cell = f.dims["nv"]
nbr_cells = f.dims["cell"]

vtx = np.array([f["clon_vertices"].values.ravel(), f["clat_vertices"].values.ravel()])
vtx, cellidx = np.unique(vtx, return_inverse=True, axis=1)

grid = UnstructuredGrid("warm_rain_grid", np.ones(nbr_cells)*vertices_per_cell,
                        vtx[0,:], vtx[1,:], cellidx)


points = grid.def_points(Location.CELL, f["clon"], f["clat"])

print(f'{routine:s}: Created grid points for YAC')

field_ic2py = Field.create("example_field_icon_to_python", comp, points)
field_py2ic = Field.create("example_field_python_to_icon", comp, points)

print(f'{routine:s}: Created fields for YAC')

yac.search()

print(f'{routine:s}: Finished YAC search')

info = -1

received_values = np.zeros([1, field_ic2py.size])
received_values -= 1

received_values, info = field_ic2py.get(received_values)
#received_values, info = field_ic2py.get()
print(f'{routine:s}: ic2py get values done - shape: {received_values.shape}')
time.sleep(10)

print(routine, str(received_values[-10:,-1]))

assert np.sum(received_values) == 880 * 879 / 2

print(f'{routine:s}: mimic python code execution - sleep(3)')

time.sleep(3)

received_values += 1.0

assert np.sum(received_values) == 880 * 881 / 2

field_py2ic.put(received_values)
print(f'{routine:s}: py2ic put values done - shape: {received_values.shape}')

print(f'{routine:s}: Done with info = {info}')
