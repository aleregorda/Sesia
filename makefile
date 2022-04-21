.SUFFIXES: .o .f90

include Makefile.machine

CODE = FALCON_v27_par.o
SUBROUTINE = parametri.o proprieta.o

.f90.o:
	$(F90) $(INCLUDE) $(OPTF) $(FLAGS) $*.f90

code:	$(CODE) $(SUBROUTINE)
	$(F90) $(OPTF) $(CODE) $(SUBROUTINE) $(LIBS) -o codice
	${RM} $*.o
