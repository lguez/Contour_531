cmake_minimum_required(VERSION 3.13)
project(Contour_531 Fortran)

add_library(contour_531 gcontr.f90 iget.f90 mark1.f90 contour_531.f90
  find_contours_irreg_grid.f90 draw_to_scratch.f90 polyline.f90
  find_contours_no_coord.f90 find_contours_reg_grid.f90 convert_to_ind.f90
  convert_to_irreg_coord.f90 convert_to_reg_coord.f90)

set_target_properties(contour_531 PROPERTIES Fortran_MODULE_DIRECTORY
  ${PROJECT_BINARY_DIR}/modules)
target_include_directories(contour_531 PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/modules>)

if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
  find_package(NR_util CONFIG REQUIRED)
  find_package(Jumble CONFIG REQUIRED)
endif()

target_link_libraries(contour_531 PRIVATE jumble nr_util)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR})
include(ConfigureCompilerFlags)
add_subdirectory(Test EXCLUDE_FROM_ALL)

include(CMakePackageConfigHelpers)
configure_package_config_file(Config.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
    INSTALL_DESTINATION lib/cmake/${PROJECT_NAME})

if(CMAKE_Fortran_COMPILER_ID MATCHES NAG|Intel)
  set(install_include_dir include/${PROJECT_NAME})
  install(DIRECTORY ${PROJECT_BINARY_DIR}/modules/ DESTINATION
    ${install_include_dir})
else()
  set(install_include_dir include)
  install(FILES ${PROJECT_BINARY_DIR}/modules/contour_531.mod TYPE INCLUDE)
endif()

install(TARGETS contour_531 EXPORT ${PROJECT_NAME}Targets LIBRARY
  INCLUDES DESTINATION ${install_include_dir})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
  DESTINATION lib/cmake/${PROJECT_NAME})
install(EXPORT ${PROJECT_NAME}Targets DESTINATION lib/cmake/${PROJECT_NAME})
export(EXPORT ${PROJECT_NAME}Targets)
