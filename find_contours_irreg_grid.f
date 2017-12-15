module find_contours_irreg_grid_m

  implicit none

contains

  subroutine find_contours_irreg_grid(x_nodes, y_nodes, z, level, contours, &
       zmax)

    ! Subroutine because there is input/ouput.

    use convert_to_irreg_coord_m, only: convert_to_irreg_coord
    use find_contours_no_coord_m, only: find_contours_no_coord
    use nr_util, only: assert_eq
    use polyline_m, only: polyline

    real, intent(in):: x_nodes(:) ! (nx)
    real, intent(in):: y_nodes(:) ! (ny)
    real, intent(in):: z(:, :) ! (nx, ny)
    real, intent(in):: level
    type(polyline), allocatable, intent(out):: contours(:)
    real, intent(in), optional:: zmax

    ! Local:
    integer i, nx, ny
    real step_x_nodes(size(x_nodes) - 1) ! (nx - 1)
    real step_y_nodes(size(y_nodes) - 1) ! (ny - 1)

    !-----------------------------------------------------------

    nx = assert_eq(size(x_nodes), size(z, 1), &
         "find_contours_irreg_grid: size(x_nodes)") 
    ny = assert_eq(size(y_nodes), size(z, 2), &
         "find_contours_irreg_grid: size(y_nodes)")

    call find_contours_no_coord(z, level, contours, zmax)

    forall (i = 1:nx - 1) step_x_nodes(i) = x_nodes(i + 1) - x_nodes(i)
    forall (i = 1:ny - 1) step_y_nodes(i) = y_nodes(i + 1) - y_nodes(i)

    do i = 1, size(contours)
       contours(i)%points(1, :) &
            = convert_to_irreg_coord(contours(i)%points(1, :), x_nodes, &
            step_x_nodes)
       contours(i)%points(2, :) &
            = convert_to_irreg_coord(contours(i)%points(2, :), y_nodes, &
            step_y_nodes)
    end do

  end subroutine find_contours_irreg_grid

end module find_contours_irreg_grid_m
