# test_find_contours_no_coord
add_executable(test_find_contours_no_coord
  test_find_contours_no_coord.f90 define_example.f90)

target_link_libraries(test_find_contours_no_coord contour_531 jumble
  numer_rec_95)

# test_gcontr
add_executable(test_gcontr test_gcontr.f90 define_example.f90 draw3.f90 cur.f90)
target_link_libraries(test_gcontr contour_531 numer_rec_95)

# test_find_contours

find_library(fortrangis_LIBRARY fortrangis)
find_library(fortranc_LIBRARY fortranc)
find_library(shapelib_LIBRARY shp)
find_library(netcdff_LIBRARY netcdff)
find_package(NetCDF REQUIRED)

add_executable(test_find_contours test_find_contours.f90)

target_include_directories(test_find_contours PRIVATE
  ${netcdff_INCLUDE_DIR} ${fortrangis_INCLUDE_DIR})

target_link_libraries(test_find_contours jumble netcdf95 shapelib_03
  contour_531 geometry ${fortrangis_LIBRARY} ${fortranc_LIBRARY}
  ${shapelib_LIBRARY} ${netcdff_LIBRARY} ${NetCDF_LIBRARY})
