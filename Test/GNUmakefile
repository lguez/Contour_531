# This is a makefile for GNU make.

# 1. Source files and libraries

sources = test_gcontr.f draw3.f cur.f define_example.f

lib_list = contour_531 numer_rec_95 jumble geometry nr_util shapelib_03 fortrangis shp fortranc netcdf95 netcdff

# 2. Objects and executable file

objects := $(sources:.f=.o)

execut = test_find_contours_no_coord test_gcontr test_find_contours

# 3. Compiler-dependent part

FC = gfortran

FFLAGS = -ffree-form -I/home/guez/build/Libraries_gfortran_debug/modules -I/usr/include -I${HOME}/.local/lib/FortranGIS_gfortran

LDLIBS = -L/home/guez/build/Libraries_gfortran_debug/Jumble -L/home/guez/build/Libraries_gfortran_debug/Contour_531 -L/home/guez/build/Libraries_gfortran_debug/Numer_Rec_95 -L/home/guez/build/Libraries_gfortran_debug/NR_util -L/home/guez/build/Libraries_gfortran_debug/NetCDF95 -L/home/guez/build/Libraries_gfortran_debug/Shapelib_03 -L/home/guez/build/Libraries_gfortran_debug/Geometry -L/home/guez/.local/lib/FortranGIS_gfortran -lshapelib_03 -lfortrangis -lcontour_531 -lnumer_rec_95 -lnetcdf95 -lshp -lfortranc -lgeometry -ljumble -lnetcdff -lnr_util

# 4. Rules

SHELL = bash
LINK.o = $(FC) $(LDFLAGS) $(TARGET_ARCH)
.DELETE_ON_ERROR:
.PHONY: all clean clobber depend
all: ${execut}
test_gcontr: ${objects}
test_find_contours_no_coord: define_example.o

depend depend.mk:
	makedepf90 -free -Wmissing -Wconfused -nosrc ${sources} >depend.mk

clean:
	rm -f ${execut} ${objects}

clobber: clean
	rm -f *.mod depend.mk

ifneq ($(MAKECMDGOALS), clobber)
include depend.mk
endif
