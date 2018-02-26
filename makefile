all:
	ifort -g -O3 -mavx          is_prime.F90 -o seri
	ifort -g -O3 -mavx -fopenmp is_prime.F90 -o omp
clean:
	rm -f seri omp *~ *.mod
