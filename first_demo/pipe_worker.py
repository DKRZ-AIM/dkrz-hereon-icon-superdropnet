import os, sys, time, io
import numpy as np
import argparse
import subprocess
import socket

pipe_in_name_prefix = "/tmp/esmdemopipe_out"
pipe_out_name_prefix = "/tmp/esmdemopipe_in"

class PipeWorker(object):

    def __init__(self, suffix):
        self.suffix = suffix
        self.pipe_in_name = "%s_%s.tmp" % (pipe_in_name_prefix, suffix)
        self.pipe_out_name = "%s_%s.tmp" % (pipe_out_name_prefix, suffix)
        self.pipe_in = None
        self.pipe_out = None
        
    def open_pipes(self):
        self.pipe_in = io.open(self.pipe_in_name, 'rb') #os.O_RDONLY)
        self.pipe_out = io.open(self.pipe_out_name,'wb') # os.O_WRONLY)
    
    def close_pipes(self):
        if self.pipe_in:
            self.pipe_in.close()
        if self.pipe_out:
            self.pipe_out.close()
            
    def create_pipes(self):
        print("PWK: Creating pipes");
        for pn in (self.pipe_in_name, self.pipe_out_name):
            subprocess.run(["mkfifo", pn])

    def work(self):
        while True:
            raw = self.pipe_in.read(4)
            if not raw:
                continue
            case = int.from_bytes(raw, sys.byteorder)
            if case == 1:
                self.exec_scalar_field_1d()
            elif case == 2:
                self.exec_scalar_field_2d()
            elif case == 0:
                print("PWK: --- Worker received stop command ---")
                return # end worker
            else:
                raise Exception("PWK: Bad case indicator: %s" % case)
        
    def exec_scalar_field_1d(self):
        # We should read the whole record in one read attempt.
        # If we only read parts, and pipe it back, the Fortran code will 
        # only read that part and stop reading. I did not find out how
        # to reliably determine the number of bytes actually read from stream
        # in Fortran.
        raw = self.pipe_in.read(4)
        if not raw:
            # When reading *formatted* input - possibly obsolete now:
            # Sleeping between attempts results in broken pipes occasionally.
            # Not sure if this is due to concurrency issues or because
            # of the string formatting used by Fortran which adds empty lines.
            # As we will pass binary data evnetually, this will be solved somehow.
            #time.sleep(0.1)
            raise Exception("No input received!")
        num_elements = int.from_bytes(raw, sys.byteorder)
        raw = self.pipe_in.read(num_elements*4)
        elements = np.frombuffer(raw, dtype="float32")
        elements_out = np.empty_like(elements)
        elements_out[:] = 0.5 * elements**2
        raw_out = elements_out.tobytes()
        self.pipe_out.write(raw_out)
        self.pipe_out.flush() # need to flush because of buffered writer

    def exec_scalar_field_2d(self):
        nx1 = int.from_bytes(self.pipe_in.read(4), sys.byteorder)
        nx2 = int.from_bytes(self.pipe_in.read(4), sys.byteorder)
        x1 = np.frombuffer(self.pipe_in.read(nx1*nx2*4), dtype="float32")
        x1 = x1.reshape(nx1, nx2)
        x2 = np.frombuffer(self.pipe_in.read(nx1*nx2*4), dtype="float32")
        x2 = x2.reshape(nx1, nx2)
        # formula: phi(x) = 0.5 * (x-1)**2 + y**2 + 2 * y
        phi = np.empty_like(x1)
        phi[:,:] = 0.5 * (x1 - 1.0)**2 + x2**2 + 2*x2
        raw_out = phi.tobytes()
        self.pipe_out.write(raw_out)
        self.pipe_out.flush()
        

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Pipe worker")
    parser.add_argument("-s", "--suffix", help="Suffix for pipe files", default="")
    parser.add_argument("--create-pipes", help="Create pipes on startup", action="store_true", default=False)
    args = parser.parse_args()
    print("PWK: Pipe worker running on host %s" % socket.gethostname())
    print("PWK: Creating PipeWorker object")
    pw = PipeWorker(args.suffix)
    print("PWK: Entering main try")
    try:
        if args.create_pipes:
            print("PWK: Creating pipes...")
            pw.create_pipes()
            print("PWK: Pipes created by pipes worker.")
        print("PWK: Opening pipes...")
        pw.open_pipes()
        print("PWK: Pipes opened by pipes worker.")
        pw.work()
    finally:
        pw.close_pipes()
