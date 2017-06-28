COMP=gnu

ifeq ($(COMP),cray)
CC=cc
FC=ftn
FCFLAGS=-efmw -hcpu=sandybridge -h bounds
endif

ifeq ($(COMP),gnu)
CC=gcc
FC=gfortran
FCFLAGS=-Wall #-fbounds-check
endif

ifeq ($(COMP),intel)
CC=icc
FC=ifort
endif

EXE=laplacian
OBJ=laplacian.o


all: $(EXE)

laplacian.o: laplacian.f90

$(EXE): $(OBJ)
	$(FC) $(FCFLAGS) $(OBJ) -o $@ 

%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CCFLAGS) -c $< -o $@

clean:
	-/bin/rm -f $(EXE)  a.out *.o *.mod *.png *~
