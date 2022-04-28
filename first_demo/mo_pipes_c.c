#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>


static const char PIPE_OUT_PATH_PREFIX[] = "/tmp/esmdemopipe_out";
static const char PIPE_IN_PATH_PREFIX[] = "/tmp/esmdemopipe_in";

int pipe_in, pipe_out;
char pipe_in_fn[200], pipe_out_fn[200];
char cmd[200];

void ip_init_pipes(int* mpi_rank) {
	// create file (fifo) names from MPI rank
	snprintf(pipe_in_fn, 200, "%s_%i.tmp", PIPE_IN_PATH_PREFIX, mpi_rank);
	snprintf(pipe_out_fn, 200, "%s_%i.tmp", PIPE_OUT_PATH_PREFIX, mpi_rank);
	// create fifos
	mkfifo(pipe_in_fn, 0666);
	mkfifo(pipe_out_fn, 0666);
	// fork python worker script
	snprintf(cmd, 200, "python ./pipe_worker.py -s %i &", mpi_rank);
	system(cmd);
	pipe_out = open(pipe_out_fn, O_WRONLY);
	pipe_in = open(pipe_in_fn, O_RDONLY);
	printf("Pipes opened.\n");
}

void ip_close_pipes() {
	int n = 0;
	write(pipe_out, &n, 4); // stop worker command
	close(pipe_out);
	close(pipe_in);
	printf("Pipes closed.\n");
	unlink(pipe_out_fn);
	unlink(pipe_in_fn);
	printf("Pipes removed.\n");
}

	
void ip_scalar_field_1d(int* nx, float* x, float* phi) {
	int n = 1;
	write(pipe_out, &n, 4);  // case indicator
	write(pipe_out, nx, 4); // payload size
	write(pipe_out, x, *nx*4);  // payload
	n = read(pipe_in, phi, *nx*4);
}


void ip_scalar_field_2d(int* nx1, int* nx2, float* x1, float* x2, float* phi) {
	int n = 2;
	write(pipe_out, &n, 4);  // case indicator
	write(pipe_out, nx1, 4); // dimension nx1
	write(pipe_out, nx2, 4); // dimension nx2
	write(pipe_out, x1, *nx1**nx2*4);  // payload x1
	write(pipe_out, x2, *nx1**nx2*4);  // payload x2
	n = read(pipe_in, phi, *nx1**nx2*4);
}

