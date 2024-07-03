module find_contours_reg_grid_m

  implicit none

contains

  subroutine find_contours_reg_grid(corner, z, level, contours, zmax, step)

    ! Subroutine because there is input/ouput. The grid is cartesian
    ! with uniform spacing. If a contour is closed then the last point
    ! for this contour repeats the first point. If a contour is closed
    ! then the number of points in the contour (component n_points) is
    ! >= 4. For a closed contour, the order of points (clockwise or
    ! counter-clockwise) is not specified.

    use convert_to_reg_coord_m, only: convert_to_reg_coord
    use find_contours_no_coord_m, only: find_contours_no_coord
    use polyline_m, only: polyline

    real, intent(in):: corner(:) ! (2)
    real, intent(in):: z(:, :), level
    type(polyline), allocatable, intent(out):: contours(:)
    real, intent(in), optional:: zmax
    real, intent(in):: step(:) ! (2)

    ! Local:
    integer i

    !-----------------------------------------------------------

    call find_contours_no_coord(z, level, contours, zmax)

    ! (replaced forall by do because of gfortran bug)
    do i = 1, size(contours)
       contours(i)%points = convert_to_reg_coord(contours(i)%points, corner, &
            step)
    end do

  end subroutine find_contours_reg_grid

end module find_contours_reg_grid_m
