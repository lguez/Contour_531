module convert_to_reg_coord_m

  implicit none

  interface convert_to_reg_coord
     module procedure convert_int_to_reg_coord, convert_real_to_reg_coord, &
          convert_polyline_to_reg_coord
     ! The difference between the arguments is the type of argument
     ! ind. If step is absent then it is assumed to be [1., 1.].
  end interface convert_to_reg_coord

contains

  pure function convert_int_to_reg_coord(ind, corner, step)

    integer, intent(in):: ind(:, :) ! (2, :)
    real, intent(in):: corner(:) ! (2)
    real, intent(in), optional:: step(:) ! (2)
    real convert_int_to_reg_coord(2, size(ind, 2))

    ! Local:
    integer j

    !---------------------------------------------------

    if (present(step)) then
       forall (j = 1:size(ind, 2)) convert_int_to_reg_coord(:, j) = corner &
            + (ind(:, j) - 1) * step
    else
       forall (j = 1:size(ind, 2)) convert_int_to_reg_coord(:, j) = corner &
            + ind(:, j) - 1
    end if

  end function convert_int_to_reg_coord

  !*********************************************************************

  pure function convert_real_to_reg_coord(ind, corner, step)

    real, intent(in):: ind(:, :) ! (2, :)
    real, intent(in):: corner(:) ! (2)
    real, intent(in), optional:: step(:) ! (2)
    real convert_real_to_reg_coord(2, size(ind, 2))

    ! Local:
    integer j

    !---------------------------------------------------

    if (present(step)) then
       forall (j = 1:size(ind, 2)) convert_real_to_reg_coord(:, j) = corner &
            + (ind(:, j) - 1.) * step
    else
       forall (j = 1:size(ind, 2)) convert_real_to_reg_coord(:, j) = corner &
            + ind(:, j) - 1.
    end if

  end function convert_real_to_reg_coord

  !*********************************************************************

  pure type(polyline) function convert_polyline_to_reg_coord(ind, corner, step)

    use polyline_m, only: polyline

    type(polyline), intent(in):: ind
    real, intent(in):: corner(:) ! (2)
    real, intent(in), optional:: step(:) ! (2)

    ! Local:
    integer j

    !---------------------------------------------------

    convert_polyline_to_reg_coord%n_points = ind%n_points
    convert_polyline_to_reg_coord%closed = ind%closed
    allocate(convert_polyline_to_reg_coord%points(2, ind%n_points))

    if (present(step)) then
       forall (j = 1:ind%n_points) convert_polyline_to_reg_coord%points(:, j) &
            = corner + (ind%points(:, j) - 1.) * step
    else
       forall (j = 1:ind%n_points) convert_polyline_to_reg_coord%points(:, j) &
            = corner + ind%points(:, j) - 1.
    end if

  end function convert_polyline_to_reg_coord

end module convert_to_reg_coord_m
