CC=gcc
FC=gfortran

all:run
	

main.o:main.c
	@$(CC) -c main.c -o tmp/main.o

intSwap.o:intSwap.f90
	@$(FC) -c intSwap.f90 -o tmp/intSwap.o

assemble:intSwap.o main.o
	@$(CC) tmp/intSwap.o tmp/main.o -o executable.out -lgfortran -g #here both fortran/c compilers work

run:assemble
	@./executable.out

clean:
	@-rm *.o *.out tmp/*.o* >/dev/null 2>&1 || true
