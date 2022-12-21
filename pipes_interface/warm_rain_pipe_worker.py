import os, sys, time, io
import numpy as np
import argparse
import subprocess
import socket

from warm_rain import i_warm_rain_nn

pipe_in_name_prefix = "/tmp/esmdemopipe_out" 
pipe_out_name_prefix = "/tmp/esmdemopipe_in" 

class PipeContext(object):
    """
    Wraps a set of (in, out) pipes with additional context info.
    A single context represents the ability to communicate from/to a foreign MPI process.
    """
    
    def __init__(self, index, pipe_in_name, pipe_out_name):
        self.pipe_index = index
        self.pipe_in_name = pipe_in_name
        self.pipe_out_name = pipe_out_name
        self.pipe_in = None
        self.pipe_out = None
        self.pipes_created = False
        
    def open_pipes(self):
        print("PWK: opening pipe_in %s: %s" % (self.pipe_index, self.pipe_in_name))
        sys.stdout.flush()
        self.pipe_in = io.open(self.pipe_in_name, 'rb') #os.O_RDONLY)
        print("PWK: pipe_in %s opened" % self.pipe_index)
        print("PWK: opening pipe_out %s: %s" % (self.pipe_index, self.pipe_out_name))
        sys.stdout.flush()
        self.pipe_out = io.open(self.pipe_out_name,'wb') # os.O_WRONLY)
        print("PWK: pipe_out %s opened" % self.pipe_index)
        sys.stdout.flush()
    
    def close_pipes(self):
        if self.pipe_in:
            self.pipe_in.close()
        if self.pipe_out:
            self.pipe_out.close()

    def create_pipes(self):
        print("PWK: Creating pipes %s" % self.pipe_index);
        sys.stdout.flush()
        for pn in (self.pipe_in_name, self.pipe_out_name):
            subprocess.run(["mkfifo", pn])
            print("Pipes %s created: %s" % (self.pipe_index, pn))
        self.pipes_created = True

    def remove_pipes(self):
        if self.pipes_created:
            os.remove(self.pipe_in_name)
            os.remove(self.pipe_out_name)
            print("PWK: Pipes %s removed." % self.pipe_index)
            sys.stdout.flush()

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
        
    """
    def exec_ttr(self):
        # read
        jg = int.from_bytes(self.pipe_in.read(4), sys.byteorder)
        jcs = int.from_bytes(self.pipe_in.read(4), sys.byteorder)
        jce = int.from_bytes(self.pipe_in.read(4), sys.byteorder)
        kbdim = int.from_bytes(self.pipe_in.read(4), sys.byteorder)
        air_mass = np.frombuffer(self.pipe_in.read(kbdim*8), dtype="float64")
        clat = np.frombuffer(self.pipe_in.read(kbdim*8), dtype="float64")
        # calculate
        jcs -= 1
        cond = (self.emi_lat_s <= clat[jcs:jce]) & (clat[jcs:jce] <= self.emi_lat_n)
        dxdt = np.empty_like(air_mass)
        dxdt[jcs:jce][cond] = self.emi_flux / air_mass[jcs:jce][cond]
        dxdt[jcs:jce][~cond] = 0.0
        # write
        raw_out = dxdt.tobytes()
        self.pipe_out.write(raw_out)
        self.pipe_out.flush()
    """

    def exec_warm_rain(self):
        #print_it = False
        # read
        dim_i = int.from_bytes(self.pipe_in.read(4), sys.byteorder)
        dim_k = int.from_bytes(self.pipe_in.read(4), sys.byteorder)
        n_moments = int.from_bytes(self.pipe_in.read(4), sys.byteorder)
        current_moments = np.frombuffer(self.pipe_in.read(dim_i*dim_k*n_moments*8), dtype="float64")
        current_moments = current_moments.reshape(n_moments, dim_k, dim_i)
        # calculate
        #if current_moments.any():
        #    print_it = True
        #if print_it:
        #    print("Current moments, pipe_index %s: %s" % (self.pipe_index, current_moments))
        #moments_shape = current_moments.shape
        #print("PWK: moments_shape: %s" % (moments_shape,))
        new_moments, return_state = i_warm_rain_nn(dim_i, dim_k, n_moments, current_moments)
        #print("PWK: new_moments_shape: %s" % (new_moments.shape,))
        #if print_it:
        #    print("New moments, pipe_index %s: %s" % (self.pipe_index, new_moments))
        # write
        raw_out = new_moments.tobytes()
        self.pipe_out.write(raw_out)
        self.pipe_out.write(return_state.to_bytes(4, byteorder=sys.byteorder))
        self.pipe_out.flush()


    """
    def exec_config_ttr(self):
        # read emi_flux, emi_lat_n, emi_lat_s
        self.emi_flux = np.frombuffer(self.pipe_in.read(8), dtype="float64")
        self.emi_lat_n = np.frombuffer(self.pipe_in.read(8), dtype="float64")
        self.emi_lat_s = np.frombuffer(self.pipe_in.read(8), dtype="float64")
    """    
    
    def exec_ping(self):
        # ping command was already received. Send int 42 back.
        raw_out = int(42).to_bytes(4, byteorder=sys.byteorder)
        self.pipe_out.write(raw_out)
        self.pipe_out.flush()

class PipeWorker(object):

    def __init__(self, suffix, num_pipes):
        self.suffix = suffix
        self.pipes = []
        for i in range(num_pipes):
            pc = PipeContext(i, "%s_%s_%s.tmp" % (pipe_in_name_prefix, suffix, i), "%s_%s_%s.tmp" % (pipe_out_name_prefix, suffix, i))
            self.pipes.append(pc)
    
    def work(self):
        while True:
            for pc in self.pipes:
                raw = pc.pipe_in.read(4)
                if not raw:
                    continue
                case = int.from_bytes(raw, sys.byteorder)
                if case == 1:
                    pc.exec_warm_rain()
                #elif case == 2:
                #    pc.exec_config_ttr()
                elif case == 10:
                    print("PWK: Sending ping back...", flush=True)
                    pc.exec_ping()
                    print("PWK: Ping sent.", flush=True)
                elif case == 0:
                    print("PWK: --- Worker received stop command ---", flush=True)
                    return # end worker
                else:
                    raise Exception("PWK: Bad case indicator on pipe %s: %s" % (pc.pipe_index, case))

    def create_pipes(self):
        for pipe in self.pipes:
            pipe.create_pipes()
    
    def open_pipes(self):
        for pipe in self.pipes:
            pipe.open_pipes()
    
    def close_pipes(self):
        for pipe in self.pipes:
            pipe.close_pipes()

    def remove_pipes(self):
        for pipe in self.pipes:
            pipe.remove_pipes()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Warm Rain Pipe worker")
    parser.add_argument("-s", "--suffix", help="Suffix for pipe files", default="")
    parser.add_argument("-n", "--num-pipes", help="Number of pipes; index starts with 0", type=int, default=1)
    parser.add_argument("--create-pipes", help="Create pipes on startup", action="store_true", default=False)
    parser.add_argument("--dry-run", help="Dry run - do not creat or listen to pipes", action="store_true", default=False)
    parser.add_argument("--remove-pipes", help="Remove pipes on shutdown", action="store_true", default=False)
    args = parser.parse_args()
    print("PWK: Warm rain pipe worker running on host %s" % socket.gethostname())
    if args.dry_run:
        print("PWK: dry run!")
    print("PWK: Using suffix %s" % args.suffix)
    print("PWK: Creating PipeWorker object")
    pw = PipeWorker(args.suffix, args.num_pipes)
    print("PWK: Entering main try", flush=True)
    sys.stdout.flush()
    try:
        if args.create_pipes:
            print("PWK: Creating pipes...", flush=True)
            sys.stdout.flush()
            if not args.dry_run:
                pw.create_pipes()
            print("PWK: Pipes created by pipes worker.", flush=True)
        print("PWK: Opening pipes...", flush=True)
        sys.stdout.flush()
        if not args.dry_run:
            pw.open_pipes()
        print("PWK: Pipes opened by pipes worker.", flush=True)
        sys.stdout.flush()
        if not args.dry_run:
            pw.work()
    finally:
        if not args.dry_run:
            pw.close_pipes()
            if args.remove_pipes:
                pw.remove_pipes()
    print("PWK: Terminated normally.")
    sys.stdout.flush()
