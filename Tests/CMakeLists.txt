if(NOT TARGET NetCDF95::netcdf95)
  list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)
  find_package(NetCDF95 CONFIG REQUIRED)
endif()

if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
  find_package(Jumble CONFIG REQUIRED)
  find_package(Shapelib_03 CONFIG REQUIRED)
  find_package(Geometry CONFIG REQUIRED)
  find_package(Numer_Rec_95 CONFIG REQUIRED)
endif()

if(NOT TARGET fortrangis::fortranGIS)
  option(FETCH "Download and build FortranGIS")

  if(FETCH)
    include(FetchContent)
    FetchContent_Declare(fortrangis GIT_REPOSITORY
      https://github.com/ARPA-SIMC/fortrangis)
    FetchContent_MakeAvailable(fortrangis)
  else()
    find_package(fortrangis CONFIG REQUIRED)
  endif()
endif()

# test_find_contours_no_coord
add_executable(test_find_contours_no_coord
  test_find_contours_no_coord.f90 define_example.f90)
target_link_libraries(test_find_contours_no_coord PRIVATE contour_531
  Jumble::jumble Numer_Rec_95::numer_rec_95)

# test_gcontr
add_executable(test_gcontr test_gcontr.f90 define_example.f90 draw3.f90 cur.f90)
target_link_libraries(test_gcontr PRIVATE contour_531
  Numer_Rec_95::numer_rec_95 Jumble::jumble)

# test_find_contours
add_executable(test_find_contours test_find_contours.f90)
target_link_libraries(test_find_contours PRIVATE Jumble::jumble
  NetCDF_Fortran::netcdff NetCDF95::netcdf95 Shapelib_03::shapelib_03
  contour_531 Geometry::geometry fortrangis::fortranGIS)

# tests_contour_531
add_custom_target(tests_contour_531)
add_dependencies(tests_contour_531 test_find_contours_no_coord
  test_gcontr test_find_contours)
