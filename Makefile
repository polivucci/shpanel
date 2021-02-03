.SUFFIXES: .F90 .o

FORT 	  = gfortran
FORT_OPT  = -O3 -cpp -c
MODFLAG   = -J
IFLAG     = -I

OPT   = -o
RM    = rm -f
ECHO  = echo

OBJ_PATH    = ./objects/
MOD_PATH    = $(OBJ_PATH)
SRC_PATH    = ./source/
BIN_PATH    = ./

vpath %.F90 $(SRC_PATH)
vpath %.o $(OBJ_PATH)
vpath %.mod $(OBJ_PATH)
vpath % $(BIN_PATH)

OBJ    = const.o input.o airfoil.o coeff.o solver.o post.o shpanel.o

%.o:%.F90
	@$(ECHO) " Compiling           $< ... "
	@$(FORT) $(FORT_OPT) $(MODFLAG)$(MOD_PATH) $(IFLAG)$(MOD_PATH) $(OPT)$(OBJ_PATH)$*.o $(SRC_PATH)$*.F90
	@$(ECHO) "                      ...done."

shpanel: dir $(OBJ)
	@$(ECHO) " Building   shpanel ... "
	@cd $(OBJ_PATH); $(FORT) $(OBJ) $(OPT) ../shpanel
	@$(ECHO) "       ... done."

dir:
	-@mkdir -p objects/

#CLEAN
clean:
	-@$(RM) *~ shpanel
	-@cd $(OBJ_PATH); $(RM) *.o *.mod *~
