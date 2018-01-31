module FILL0_m

  implicit none

contains

  SUBROUTINE FILL0(BITMAP, N)

    ! FILL THE FIRST N BITS OF BITMAP WITH ZEROES.

    INTEGER, intent(inout):: BITMAP(:)
    integer, intent(in):: N ! n / nbpw + 1 must be <= size(bitmap)

    ! Local:

    integer, parameter:: NBPW = bit_size(0) - 1
    ! NBPW IS THE MINIMUM NUMBER OF SIGNIFICANT BITS PER WORD USED
    ! BY INTEGER ARITHMETIC. THIS IS USUALLY ONE LESS THAN THE
    ! ACTUAL NUMBER OF BITS PER WORD, BUT AN IMPORTANT EXCEPTION IS
    ! THE CDC-6000 SERIES OF MACHINES, WHERE NBPW SHOULD BE 48.

    integer i, LOOP, NBLW

    !------------------------------------------------------------------

    LOOP = N / NBPW
    NBLW = MOD(N, NBPW)
    IF (LOOP /= 0) then
       DO I=1, LOOP
          BITMAP(I) = 0
       end DO
    end IF
    IF (NBLW /= 0) BITMAP(LOOP + 1) = MOD(BITMAP(LOOP + 1), 2**(NBPW - NBLW))

  END SUBROUTINE FILL0

end module FILL0_m
