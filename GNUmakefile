# This is a makefile for GNU make.

# 1. Source files 

VPATH = .

sources = fill0.f gcontr.f iget.f mark1.f contour_531.f find_contours_irreg_grid.f draw_to_scratch.f polyline.f find_contours_no_coord.f find_contours_reg_grid.f convert_to_ind.f convert_to_irreg_coord.f convert_to_reg_coord.f null_polyline.f

# 2. Objects and libraries

objects := $(sources:.f=.o)
lib_stat = libcontour_531.a

# 3. Compiler-dependent part

mode = debug
include ${general_compiler_options_dir}/${FC}_${mode}.mk

# 4. Rules

SHELL = bash
.DELETE_ON_ERROR:
.PHONY: all clean clobber depend
all: ${lib_stat} log
${lib_stat}: ${lib_stat}(${objects})

depend ${VPATH}/depend.mk:
	makedepf90 -free -Wmissing -Wconfused -I${VPATH} -nosrc $(addprefix -u , numer_rec_95 jumble nr_util) ${sources} >${VPATH}/depend.mk

clean:
	rm -f ${lib_stat} ${objects} log

clobber: clean
	rm -f *.mod ${VPATH}/depend.mk

log:
	hostname >$@
	${FC} ${version_flag} >>$@ 2>&1
	echo -e "\nFC = ${FC}\n\nFFLAGS = ${FFLAGS}\n\nLDFLAGS = ${LDFLAGS}" >>$@

ifneq ($(MAKECMDGOALS), clobber)
include ${VPATH}/depend.mk
endif
