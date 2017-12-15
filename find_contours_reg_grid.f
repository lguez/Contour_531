module find_contours_reg_grid_m

  implicit none

contains

  subroutine find_contours_reg_grid(corner, step, z, level, contours, zmax)

    ! Subroutine because there is input/ouput. If a contour is closed
    ! then the last element in the component arrays for this contour
    ! repeats the first element.

    use convert_to_reg_coord_m, only: convert_to_reg_coord
    use find_contours_no_coord_m, only: find_contours_no_coord
    use polyline_m, only: polyline

    real, intent(in):: corner(:), step(:) ! (2)
    real, intent(in):: z(:, :), level
    type(polyline), allocatable, intent(out):: contours(:)
    real, intent(in), optional:: zmax

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
