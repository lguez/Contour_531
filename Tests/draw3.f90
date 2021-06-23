module draw3_m

  implicit none

  integer, protected:: unit

contains

  SUBROUTINE DRAW3(X, Y, IFLAG)

    ! DO OUTPUT FOR GCONTR.

    use cur, only: xcur, ycur

    real, intent(inout):: x, y
    integer, intent(in):: iflag

    ! Local:
    integer jump

    !-----------------------------------------------------------

    JUMP = MOD(IFLAG, 10)
    
    if (jump == 6) then
       X = XCUR
       Y = YCUR
    else
       write(unit, fmt = *) JUMP, IFLAG / 10, X, Y
    end if
    
  END SUBROUTINE DRAW3

  !***************************************************************

  subroutine set_unit

    use jumble, only: new_unit

    !---------------------------------------

    call new_unit(unit)

  end subroutine set_unit

end module draw3_m
