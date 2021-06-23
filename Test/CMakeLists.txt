set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR})
find_package(NetCDF REQUIRED)
find_package(NetCDF_Fortran REQUIRED)

if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
  find_package(NetCDF95 CONFIG REQUIRED)
  find_package(Shapelib_03 CONFIG REQUIRED)
  find_package(Geometry CONFIG REQUIRED)
  find_package(Numer_Rec_95 CONFIG REQUIRED)
endif()

# test_find_contours_no_coord
add_executable(test_find_contours_no_coord
  test_find_contours_no_coord.f90 define_example.f90)

target_link_libraries(test_find_contours_no_coord PRIVATE contour_531 jumble
  numer_rec_95 nr_util)

# test_gcontr

add_executable(test_gcontr test_gcontr.f90 define_example.f90 draw3.f90 cur.f90)

target_link_libraries(test_gcontr PRIVATE contour_531 numer_rec_95
  nr_util jumble)

# test_find_contours

add_executable(test_find_contours test_find_contours.f90)

target_link_libraries(test_find_contours PRIVATE jumble
  NetCDF95::netcdf95 shapelib_03 contour_531 geometry nr_util)

add_custom_target(tests_contour_531)

add_dependencies(tests_contour_531 test_find_contours_no_coord
  test_gcontr test_find_contours)
