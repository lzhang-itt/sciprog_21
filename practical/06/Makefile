FC = gfortran
FCFLAGS = -O3 -Wall
LDFLAGS = -lm

all: matrix

matrix: main.o matmul.o
	$(FC) -o $@ $^ $(LDFLAGS)
##$(FC) -o matrix main.o matmul.o $(LDFLAGS)	 

matmul.o: matmul.f90
	$(FC) -c $(FCFLAGS) $<
##$(FC) -c $(FCFLAGS) matmul.f90

main.o: main.f90
	$(FC) -c $(FCFLAGS) $<
##$(FC) -c $(FCFLAGS) main.f90

clean:
	rm *.o *.mod
