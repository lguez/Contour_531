# This is a makefile for GNU make.

# 1. Source files and libraries

makefile_dir = .
VPATH = ${makefile_dir}/Sources
sources = test_gcontr.f draw3.f cur.f define_example.f

lib_list = contour_531 numer_rec_95 jumble geometry nr_util shapelib_03 fortrangis shp fortranc netcdf95 netcdff

# 2. Objects and executable file

objects := $(sources:.f=.o)

execut = test_find_contours_no_coord test_gcontr test_find_contours

# 3. Compiler-dependent part

mode = debug
include ${general_compiler_options_dir}/${FC}_${mode}.mk
lib_dir = ${HOME}/Desktop/lib/Contour_531
FFLAGS += -I${lib_dir}
LDLIBS += -L${lib_dir}

# 4. Rules

SHELL = bash
LINK.o = $(FC) $(LDFLAGS) $(TARGET_ARCH)
.DELETE_ON_ERROR:
.PHONY: all clean clobber depend
all: ${execut} log
test_gcontr: ${objects}
test_find_contours_no_coord: define_example.o

depend ${makefile_dir}/depend.mk:
	makedepf90 -free -Wmissing -Wconfused -I${VPATH} -nosrc $(addprefix -u , ${lib_list} shapelib) ${sources} >${makefile_dir}/depend.mk

clean:
	rm -f ${execut} ${objects} log

clobber: clean
	rm -f *.mod ${makefile_dir}/depend.mk

log:
	hostname >$@
	${FC} ${version_flag} >>$@ 2>&1
	echo -e "\nFC = ${FC}\n\nFFLAGS = ${FFLAGS}\n\nLDLIBS = ${LDLIBS}\n\nLDFLAGS = ${LDFLAGS}" >>$@

ifneq ($(MAKECMDGOALS), clobber)
include ${makefile_dir}/depend.mk
endif
