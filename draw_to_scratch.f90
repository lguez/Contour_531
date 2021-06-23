module draw_to_scratch_m

  implicit none

  integer, protected:: unit
  integer, save, protected:: n_cont ! number of contours

contains

  SUBROUTINE draw_to_scratch(X, Y, IFLAG)

    ! DO OUTPUT FOR GCONTR.

    real, intent(inout):: x, y
    integer, intent(in):: iflag

    ! Local:
    integer jump_scal

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
       write(unit) JUMP_SCAL, IFLAG / 10, X, Y
    end if
    
  END SUBROUTINE draw_to_scratch

  !***************************************************************

  subroutine init_scratch

    use jumble, only: new_unit

    !---------------------------------------

    call new_unit(unit)
    open(unit, form = "unformatted", status = "scratch", action = "readwrite")
    n_cont = 0

  end subroutine init_scratch

end module draw_to_scratch_m
