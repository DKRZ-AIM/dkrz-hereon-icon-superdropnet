# transfer_arrays.py

# following
# https://www.noahbrenowitz.com/post/calling-fortran-from-python/
import numpy as np

# create the dictionary for mapping ctypes to (numpy) dtypes
ctype2dtype = {}

# integer types
for prefix in ['int', 'uint']:
    for log_bytes in range(4):
        ctype = '%s%d_t' % (prefix, 8 * (2**log_bytes))
        dtype = '%s%d' % (prefix[0], 2**log_bytes)
        ctype2dtype[ctype] = np.dtype(dtype)

# floating point types
ctype2dtype['float']  = np.dtype('f4')
ctype2dtype['double'] = np.dtype('f8')

#for key in ctype2dtype:
#    print(key, ctype2dtype[key])

def asarray(ffi, ptr, shape, **kwargs):
    length = np.prod(shape)
    # Get the canonical C type of the elements of ptr as a string.
    T = ffi.getctype(ffi.typeof(ptr).item)

    if T not in ctype2dtype:
        raise RuntimeError("Cannot create an array for element type: %s" % T)

    a = np.frombuffer(ffi.buffer(ptr, length * ffi.sizeof(T)), ctype2dtype[T])
    a = a.reshape(shape, **kwargs)

    print('******************** will print array')
    print(ctype2dtype[T])
    print(a)

    print('******************** printed array')

    return a
