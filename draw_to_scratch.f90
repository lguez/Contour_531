module draw_to_scratch_m

  implicit none

  integer, save, protected:: n_cont ! number of contours
  integer, save, private:: n_points
  integer, allocatable, save:: jump(:) ! (max_points)
  integer:: max_points = 100
  real, allocatable, save:: points(:, :) ! (2, max_points)

contains

  SUBROUTINE draw_to_scratch(X, Y, IFLAG)

    ! DO OUTPUT FOR GCONTR.

    real, intent(inout):: x, y
    integer, intent(in):: iflag

    ! Local:
    integer jump_scal, dble_max_points
    integer, allocatable:: tmp_jump(:)
    real, allocatable:: tmp_points(:, :)

    !-----------------------------------------------------------

    JUMP_SCAL = MOD(IFLAG, 10)
    
    if (jump_scal == 6) then
       ! 6 - SET X AND Y TO THE APPROXIMATE 'PEN' POSITION
       X = 0.
       Y = 0.
    else
       if (jump_scal == 2 .or. jump_scal == 3) n_cont = n_cont + 1
       ! 2 - START A CONTOUR AT A BOUNDARY,
       ! 3 - START A CONTOUR NOT AT A BOUNDARY,
       n_points = n_points + 1

       if (n_points > max_points) then
          dble_max_points = 2 * max_points
          allocate(tmp_jump(dble_max_points), tmp_points(2, dble_max_points))
          tmp_jump(:max_points) = jump
          tmp_points(:, :max_points) = points
          call move_alloc(tmp_jump, jump)
          call move_alloc(tmp_points, points)
          max_points = dble_max_points
          print *, "draw_to_scratch: doubled size of jump and points to", &
               max_points
       end if
       
       JUMP(n_points) = jump_scal
       points(:, n_points) = [X, Y]
    end if
    
  END SUBROUTINE draw_to_scratch

  !***************************************************************

  subroutine init_scratch


    !---------------------------------------

    n_cont = 0
    n_points = 0
    if (.not. allocated(jump)) allocate(jump(max_points), points(2, max_points))

  end subroutine init_scratch

end module draw_to_scratch_m
