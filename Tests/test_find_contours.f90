program test_find_contours

  use, intrinsic:: iso_fortran_env

  use contour_531, only: find_contours_reg_grid, find_contours_irreg_grid, &
       polyline
  use geometry, only: polygon_area_2d
  use jumble, only: read_column, get_command_arg_dyn
  use netcdf, only: nf90_nowrite
  use netcdf95, only: nf95_open, nf95_get_var, nf95_inq_varid, nf95_close, &
       nf95_gw_var
  use nr_util, only: assert
  use shapelib, only: shpfileobject, shpt_polygon, ftdouble, shpclose, shpt_arc
  use shapelib_03, only: shp_create_03, dbf_add_field_03, &
       shp_append_object_03, dbf_write_attribute_03

  implicit none

  real, allocatable:: z(:, :) ! (nx, ny)
  real, allocatable:: levels(:)
  integer i, j, field_number, shape_number, n_contours
  character(len = :), allocatable:: filename
  integer ncid, varid
  type(polyline), allocatable:: contours(:)
  TYPE(shpfileobject) shphandle_polygon, shphandle_polyline
  integer nx, ny

  real, allocatable:: x(:), y(:)
  ! if reg_grid, we will only use the first two values to get the
  ! corner and step

  real:: zmax = huge(0.)
  logical:: reg_grid = .true.

  namelist /main_nml/ zmax, reg_grid

  !-------------------------------------------------------------

  call get_command_arg_dyn(1, filename, "Required argument: netCDF-file")

  print *, "Reading from ", filename, "..."
  call nf95_open(filename, nf90_nowrite, ncid)

  call nf95_inq_varid(ncid, "x", varid)
  call nf95_gw_var(ncid, varid, x)
  nx = size(x)

  call nf95_inq_varid(ncid, "y", varid)
  call nf95_gw_var(ncid, varid, y)
  ny = size(y)

  allocate(z(nx, ny))

  call nf95_inq_varid(ncid, "z", varid)
  call nf95_get_var(ncid, varid, z)

  call nf95_close(ncid)

  print *, 'Reading from "levels.txt"...'
  call read_column(levels, "levels.txt")

  write(unit = error_unit, nml = main_nml)
  write(unit = error_unit, fmt = *) "Enter namelist main_nml:"
  read(unit = *, nml = main_nml)
  write(unit = *, nml = main_nml)

  call shp_create_03("test_find_contours_polygon", shpt_polygon, &
       shphandle_polygon)
  call shp_create_03("test_find_contours_polyline", shpt_arc, &
       shphandle_polyline)
  call dbf_add_field_03(field_number, shphandle_polygon, 'level', ftdouble, &
       nwidth = 13, ndecimals = 6)
  call dbf_add_field_03(field_number, shphandle_polyline, 'level', ftdouble, &
       nwidth = 13, ndecimals = 6)

  do j = 1, size(levels)
     if (reg_grid) then
        if (zmax == huge(0.)) then
           call find_contours_reg_grid(corner = [x(1), y(1)], &
                step = [x(2) - x(1), y(2) - y(1)], &
                z = z, level = levels(j), contours = contours)
        else
           call find_contours_reg_grid(corner = [x(1), y(1)], &
                step = [x(2) - x(1), y(2) - y(1)], &
                z = z, level = levels(j), contours = contours, zmax = zmax)
        end if
     else
        call find_contours_irreg_grid(x, y, z, levels(j), contours)
     end if

     n_contours = size(contours)
     print *, "level = ", levels(j)
     print *, "Found", n_contours, "contours."

     do i = 1, n_contours
        if (contours(i)%closed) then
           call assert(contours(i)%n_points >= 4, "closed")
           if (polygon_area_2d(contours(i)%points) == 0.) then
              print *, "contour number ", i
              print *, "Zero area"
           else
              call shp_append_object_03(shape_number, shphandle_polygon, &
                   shpt_polygon, contours(i)%points)
              call dbf_write_attribute_03(shphandle_polygon, shape_number, &
                   ifield = 0, fieldvalue = levels(j))
           end if
        else
           call assert(contours(i)%n_points >= 2, "open")
           call shp_append_object_03(shape_number, shphandle_polyline, &
                shpt_arc, contours(i)%points)
           call dbf_write_attribute_03(shphandle_polyline, shape_number, &
                ifield = 0, fieldvalue = levels(j))
        end if
     end do
  end do

  CALL shpclose(shphandle_polygon)
  CALL shpclose(shphandle_polyline)
  print *, 'Created shapefiles "test_find_contours_polygon"'
  print *, 'and "test_find_contours_polyline".'

END program test_find_contours
