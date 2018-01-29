program test_find_contours_no_coord

  use, intrinsic:: iso_fortran_env

  use contour_531, only: find_contours_no_coord, polyline
  use define_example_m, only: define_example_reg
  use jumble, only: new_unit

  implicit none

  real, allocatable:: Z(:, :), CV(:)
  real x_min, y_min, step_x, step_y
  integer i, j, k, unit
  type(polyline), allocatable:: contours(:)
  real:: zmax = huge(0.)

  namelist /main_nml/ zmax

  !-------------------------------------------------------------

  write(unit = error_unit, nml = main_nml)
  write(unit = error_unit, fmt = *) "Enter namelist main_nml:"
  read(unit = *, nml = main_nml)
  write(unit = *, nml = main_nml)

  call define_example_reg(x_min, y_min, step_x, step_y, z, cv)
  call new_unit(unit)
  open(unit, file = "test_find_contours_no_coord.csv", status = "replace", &
       action = "write")

  do k = 1, size(cv)
     if (zmax == huge(0.)) then
        call find_contours_no_coord(z, cv(k), contours)
     else
        call find_contours_no_coord(z, cv(k), contours, zmax)
     end if

     do i = 1, size(contours)
        write(unit, fmt = *) 3, k, contours(i)%points(:, 1)

        do j = 2, contours(i)%n_points - 1
           write(unit, fmt = *) 1, k, contours(i)%points(:, j)
        end do

        write(unit, fmt = *) 5, k, contours(i)%points(:, contours(i)%n_points)
     end do
  end do

  close(unit)

END program test_find_contours_no_coord
