# This is a makefile for GNU make.

# 1. Source files 

sources = gcontr.f iget.f mark1.f contour_531.f find_contours_irreg_grid.f draw_to_scratch.f polyline.f find_contours_no_coord.f find_contours_reg_grid.f convert_to_ind.f convert_to_irreg_coord.f convert_to_reg_coord.f null_polyline.f

# 2. Objects and libraries

objects := $(sources:.f=.o)
lib_stat = libcontour_531.a

# 3. Compiler-dependent part

FC = gfortran
FFLAGS = -ffree-form -I/home/guez/build/Libraries_gfortran_debug/modules
ARFLAGS = rvU

# 4. Rules

SHELL = bash
.DELETE_ON_ERROR:
.PHONY: all clean clobber depend
all: ${lib_stat}
${lib_stat}: ${lib_stat}(${objects})

depend depend.mk:
	makedepf90 -free -Wmissing -Wconfused -nosrc $(addprefix -u , jumble nr_util) ${sources} >depend.mk

clean:
	rm -f ${lib_stat} ${objects}

clobber: clean
	rm -f *.mod depend.mk

ifneq ($(MAKECMDGOALS), clobber)
include depend.mk
endif
