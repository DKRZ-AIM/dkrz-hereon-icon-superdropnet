#!/usr/bin/env python3

'''
Dummy setup for coupling a Python routine to ICON 
via YAC

This is the dummy ICON side!

Fields:
(1) example_field_icon_to_python (here: put)
(2) example_field_python_to_icon (here: get)
'''

from yac import YAC, Field, UnstructuredGrid, Location
import xarray as xr
import numpy as np

routine = 'DUMMY-ICON yac_caroline_dummy'

yac = YAC("coupling.xml", "coupling.xsd")
comp = yac.def_comp(f"atmo")

f = xr.open_dataset('/pool/data/ICON/grids/public/mpim/Torus_Triangles_20x22_5000m/Torus_Triangles_20x22_5000m.nc')

vertices_per_cell = f.dims["nv"]
nbr_cells = f.dims["cell"]

vtx = np.array([f["clon_vertices"].values.ravel(), f["clat_vertices"].values.ravel()])
vtx, cellidx = np.unique(vtx, return_inverse=True, axis=1)

grid = UnstructuredGrid("icon_atmos_grid", np.ones(nbr_cells)*vertices_per_cell,
                        vtx[0,:], vtx[1,:], cellidx)


points = grid.def_points(Location.CELL, f["clon"], f["clat"])

print(f'{routine:s}: Created grid points for YAC')

field_ic2py = Field.create("example_field_icon_to_python", comp, points)
field_py2ic = Field.create("example_field_python_to_icon", comp, points)

print(f'{routine:s}: Created fields for YAC')

yac.search()

print(f'{routine:s}: Finished YAC search')

print(f'{routine:s} Field size ic2py: {field_ic2py.size}')
print(f'{routine:s} Field size py2ic: {field_py2ic.size}')
print(f'{routine:s} Collection size ic2py: {field_ic2py.collection_size}')
print(f'{routine:s} Collection size py2ic: {field_py2ic.collection_size}')

info = -1

time = yac.start_datetime

'''
FLOW

Dummy-ICON: declare array

PUT ic2py

GET py2ic
'''

values_to_pass = np.arange(field_ic2py.size)
values_to_pass = values_to_pass.reshape(-1, field_ic2py.size)

assert np.sum(values_to_pass) == field_ic2py.size * (field_ic2py.size - 1) / 2

print(f'{routine:s}: Created values in "Dummy-ICON" to pass to Python')

field_ic2py.put(values_to_pass)

print(f'{routine:s}: ic2py put values done - shape: {values_to_pass.shape}')
values_to_receive = np.empty(values_to_pass.shape)
values_to_receive, info = field_py2ic.get(values_to_receive)

assert np.sum(values_to_receive) == 880 * 881 / 2
print(f'{routine:s}: ic2py get values done - shape: {values_to_pass.shape}')

print(f'{routine:s}: Done with info = {info}')
