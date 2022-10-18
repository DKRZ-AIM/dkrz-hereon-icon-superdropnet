#!/usr/bin/env python3
# python side

from yac import *

print("Hello world, I am Python")
#import xarray as xr
#import numpy as np
#
#yac = YAC("coupling.xml", "coupling.xsd")
#comp = yac.def_comp(f"C0")
#
#f = xr.open_dataset("tsurf_unstruct.nc")
#
#vertices_per_cell = f.dims["nv"]
#nbr_cells = f.dims["cell"]
#
#vtx = np.array([f["clon_bnds"].values.ravel(), f["clat_bnds"].values.ravel()])
#vtx, cellidx = np.unique(vtx, return_inverse=True, axis=1)
#
#grid = UnstructuredGrid("grid0", np.ones(nbr_cells)*vertices_per_cell,
#                        vtx[0,:], vtx[1,:], cellidx)
#
#
#points = grid.def_points(Location.CELL, f["clon"], f["clat"])
#
#field = Field.create("example_field", comp, points)
#
#yac.search()
#
#time = f["time"]
#for i, t in enumerate(time.values):
#    print(t)
#    field.put(f["tsurf"][i,:].values.reshape((-1, field.size)))
#
