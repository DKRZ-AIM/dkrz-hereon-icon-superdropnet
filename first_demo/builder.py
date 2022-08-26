import cffi

ffibuilder = cffi.FFI()

header = """
extern void i_hello_world();
extern void i_print_shape(int *);
extern void i_print_value(double *);
extern void i_scalar_field_1d(int *, float *, float *);
extern void i_scalar_field_2d(int *, int * , float *, float *, float *);
extern void i_scalar_field_3d(int *, int *, int *, float *, float *, float *, float *);
"""

with open("plugin.h", "w") as f:
    f.write(header)

ffibuilder.embedding_api(header)
ffibuilder.set_source("my_plugin", r'''
    #include "plugin.h"
''')

module = open('my_module.py', 'r').read()

ffibuilder.embedding_init_code(module)
ffibuilder.compile(target="libplugin.so", verbose=True)
