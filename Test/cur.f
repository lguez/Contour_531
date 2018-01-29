module cur

  implicit none

  real XCUR, YCUR

contains

  subroutine set_cur(nx, ny)

    integer, intent(in):: nx, ny

    !----------------------------------------------

    IF (MOD(NX, 2) == 0) then
       YCUR = 1.
    else
       YCUR = NY
    end IF
    
    IF (MOD(NY, 2) == 0) then
       XCUR = 1.
    else
       XCUR = NX
    end IF

  end subroutine set_cur

end module cur
