program test_gcontr

  use, intrinsic:: ISO_FORTRAN_ENV

  use contour_531, only: GCONTR
  use define_example_m, only: define_example_reg
  use draw3_m, only: draw3, set_unit, unit
  use cur, only: set_cur

  implicit none

  real, allocatable:: Z(:, :), CV(:)
  real x_min, y_min, step_x, step_y
  real:: zmax = huge(0.)

  namelist /main_nml/ zmax

  !-------------------------------------------------------------

  write(unit = error_unit, nml = main_nml)
  write(unit = error_unit, fmt = *) "Enter namelist main_nml:"
  read(unit = *, nml = main_nml)
  write(unit = *, nml = main_nml)

  call define_example_reg(x_min, y_min, step_x, step_y, z, cv)
  call set_cur(size(z, 1), size(z, 2))
  call set_unit
  open(unit, file = "test_gcontr.csv", status = "replace", action = "write")

  if (zmax == huge(0.)) then
     CALL GCONTR(Z, CV, DRAW3)
  else
     CALL GCONTR(Z, CV, DRAW3, zmax)
  end if

  close(unit)

END program test_gcontr
