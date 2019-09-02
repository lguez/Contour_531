module draw_to_scratch_m

  implicit none

  integer, protected:: unit
  integer, protected:: n_cont ! number of contours

contains

  SUBROUTINE draw_to_scratch(X, Y, IFLAG)

    ! DO OUTPUT FOR GCONTR.

    real, intent(inout):: x, y
    integer, intent(in):: iflag

    ! Local:
    integer jump

    !-----------------------------------------------------------

    JUMP = MOD(IFLAG, 10)
    
    if (jump == 6) then
       X = 0.
       Y = 0.
    else
       if (jump == 2 .or. jump == 3) n_cont = n_cont + 1
       write(unit) JUMP, IFLAG / 10, X, Y
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
