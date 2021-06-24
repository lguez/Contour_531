module MARK1_m

  implicit none

contains

  SUBROUTINE MARK1(BITMAP, N)

    ! PUT A ONE IN THE NTH BIT OF BITMAP.

    INTEGER, intent(inout):: BITMAP(:)
    integer, intent(in):: N

    ! Local:
    
    integer, parameter:: NBPW = bit_size(0) - 1
    ! NBPW IS THE MINIMUM NUMBER OF SIGNIFICANT BITS PER WORD USED
    ! BY INTEGER ARITHMETIC. THIS IS USUALLY ONE LESS THAN THE
    ! ACTUAL NUMBER OF BITS PER WORD, BUT AN IMPORTANT EXCEPTION IS
    ! THE CDC-6000 SERIES OF MACHINES, WHERE NBPW SHOULD BE 48.

    integer NWORD, NBIT, i

    !-----------------------------------------------------------------------
    
    NWORD = (N - 1) / NBPW
    NBIT = MOD(N - 1, NBPW)
    I = 2**(NBPW - NBIT - 1)
    BITMAP(NWORD + 1) = BITMAP(NWORD + 1) &
         + I * (1 - MOD(BITMAP(NWORD + 1) / I, 2))

  END SUBROUTINE MARK1

end module MARK1_m
