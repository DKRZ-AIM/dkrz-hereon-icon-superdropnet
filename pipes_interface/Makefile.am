CFLAGS= -fPIC -g
LDFLAGS= -shared -O3
CC=mpiicc

all: ../lib/libpipes.so

../lib/libpipes.so: mo_pipes_c.c
	$(CC) $(CFLAGS) mo_pipes_c.c -o $@ $(LDFLAGS)

clean:
	-rm ../lib/libpipes.so
	-rm libpipes.o
