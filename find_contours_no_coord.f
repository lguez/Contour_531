module find_contours_no_coord_m

  implicit none

contains

  subroutine find_contours_no_coord(z, level, contours, zmax)

    ! Subroutine because there is input/ouput. If a contour is closed
    ! then the last element in the component arrays for this contour
    ! repeats the first element.

    use draw_to_scratch_m, only: init_scratch, draw_to_scratch, n_cont, unit
    use GCONTR_m, only: gcontr
    use nr_util, only: assert
    use polyline_m, only: polyline

    real, intent(in):: z(:, :), level
    type(polyline), allocatable, intent(out):: contours(:)
    real, intent(in), optional:: zmax

    ! Local:
    integer jump, trash, j
    integer i ! contour index

    !-----------------------------------------------------------

    call init_scratch
    call GCONTR(Z, [level], draw_to_scratch, ZMAX)
    allocate(contours(n_cont))
    contours%n_points = 0
    rewind(unit)

    do i = 1, n_cont
       do
          read(unit) jump
          contours(i)%n_points = contours(i)%n_points + 1
          if (jump == 4 .or. jump == 5) then
             contours(i)%closed = jump == 5
             exit
          end if
       end do
    end do
           
    rewind(unit)

    do i = 1, n_cont
       allocate(contours(i)%points(2, contours(i)%n_points))

       do j = 1, contours(i)%n_points
          read(unit) trash, trash, contours(i)%points(:, j)
       end do
    end do

    close(unit)

    ! Correct really strange behavior which is sometimes encountered:
    forall (i = 1:n_cont, contours(i)%closed .and. &
         (contours(i)%n_points <= 3 &
         .or. &
         any(contours(i)%points(:, contours(i)%n_points) &
         /= contours(i)%points(:, 1)))) &
         contours(i)%closed = .false.
        
  end subroutine find_contours_no_coord

end module find_contours_no_coord_m
