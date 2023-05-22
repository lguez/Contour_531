module find_contours_no_coord_m

  implicit none

contains

  subroutine find_contours_no_coord(z, level, contours, zmax)

    ! Subroutine because there is input/ouput. If a contour is closed
    ! then the last point for this contour repeats the first point. If
    ! a contour is closed then the number of points in the contour
    ! (component n_points) is >= 4. For a closed contour, the order of
    ! points (clockwise or counter-clockwise) is not specified.

    use draw_to_scratch_m, only: init_scratch, draw_to_scratch, n_cont, jump, &
         points
    use GCONTR_m, only: gcontr
    use polyline_m, only: polyline

    real, intent(in):: z(:, :), level
    type(polyline), allocatable, intent(out):: contours(:)
    real, intent(in), optional:: zmax

    ! Local:
    integer j
    integer i ! contour index

    !-----------------------------------------------------------

    call init_scratch
    call GCONTR(Z, [level], draw_to_scratch, ZMAX)
    allocate(contours(n_cont))
    contours%n_points = 0
    j = 0

    do i = 1, n_cont
       do
          j = j + 1
          contours(i)%n_points = contours(i)%n_points + 1
          if (jump(j) == 4 .or. jump(j) == 5) then
             ! 4 - FINISH A CONTOUR AT A BOUNDARY,
             ! 5 - FINISH A CLOSED CONTOUR (NOT AT A BOUNDARY).
             contours(i)%closed = jump(j) == 5
             exit
          end if
       end do
    end do
           
    j = 0

    do i = 1, n_cont
       allocate(contours(i)%points(2, contours(i)%n_points))
       contours(i)%points = points(:, j + 1:j + contours(i)%n_points)
       j = j + contours(i)%n_points
    end do

    ! Correct really strange behavior which is sometimes encountered:
    forall (i = 1:n_cont, contours(i)%closed .and. &
         (contours(i)%n_points <= 3 &
         .or. &
         any(contours(i)%points(:, contours(i)%n_points) &
         /= contours(i)%points(:, 1)))) &
         contours(i)%closed = .false.
        
  end subroutine find_contours_no_coord

end module find_contours_no_coord_m
