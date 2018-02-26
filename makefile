all:
	ifort  -g -O3 -mavx          is_prime.F90 -o seri
	ifort  -g -O3 -mavx -fopenmp is_prime.F90 -o omp
	mpif90 -g -O3 -mavx -fopenmp is_prime_mpi.F90 -o mpi
clean:
	rm -f seri omp mpi *~ *.mod
