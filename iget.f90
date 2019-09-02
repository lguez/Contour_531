module iget_m

  implicit none

contains

  integer FUNCTION IGET(BITMAP, N)
    !
    !     IGET=0 IF THE NTH BIT OF BITMAP IS ZERO, ELSE IGET IS ONE.
    !
    INTEGER BITMAP(:), N
    !
    integer, parameter:: NBPW = bit_size(0) - 1
    !     BY INTEGER ARITHMETIC.  THIS IS USUALLY ONE LESS THAN THE
    !     ACTUAL NUMBER OF BITS PER WORD, BUT AN IMPORTANT EXCEPTION IS
    !     THE CDC-6000 SERIES OF MACHINES, WHERE NBPW SHOULD BE 48.

    integer NWORD, NBIT
    !
    NWORD = (N-1)/NBPW
    NBIT = MOD(N-1,NBPW)
    IGET = MOD(BITMAP(NWORD+1)/2**(NBPW-NBIT-1),2)

  END FUNCTION IGET

end module iget_m
