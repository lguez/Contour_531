cmake_minimum_required(VERSION 3.13)
project(Contour_531_project Fortran)

add_library(contour_531 gcontr.f90 iget.f90 mark1.f90 contour_531.f90
  find_contours_irreg_grid.f90 draw_to_scratch.f90 polyline.f90
  find_contours_no_coord.f90 find_contours_reg_grid.f90 convert_to_ind.f90
  convert_to_irreg_coord.f90 convert_to_reg_coord.f90)

target_link_libraries(contour_531 PUBLIC jumble nr_util)
include(ConfigureCompilerFlags)
add_subdirectory(Test EXCLUDE_FROM_ALL)
