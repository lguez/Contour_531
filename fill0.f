module FILL0_m

  implicit none

contains

  SUBROUTINE FILL0(BITMAP, N)

    ! FILL THE FIRST N BITS OF BITMAP WITH ZEROES.

    INTEGER, intent(inout):: BITMAP(:)
    integer, intent(in):: N
    ! If mod(n, npbw /= 0) then size(bitmap) must be >= n / nbpw + 1,
    ! else size(bitmap) must be >= n / nbpw.

    ! Local:

    integer, parameter:: NBPW = bit_size(0) - 1
    ! NBPW IS THE MINIMUM NUMBER OF SIGNIFICANT BITS PER WORD USED
    ! BY INTEGER ARITHMETIC. THIS IS USUALLY ONE LESS THAN THE
    ! ACTUAL NUMBER OF BITS PER WORD, BUT AN IMPORTANT EXCEPTION IS
    ! THE CDC-6000 SERIES OF MACHINES, WHERE NBPW SHOULD BE 48.

    integer LOOP, NBLW

    !------------------------------------------------------------------

    LOOP = N / NBPW
    NBLW = MOD(N, NBPW)
    BITMAP(:loop) = 0
    IF (NBLW /= 0) BITMAP(LOOP + 1) = MOD(BITMAP(LOOP + 1), 2**(NBPW - NBLW))

  END SUBROUTINE FILL0

end module FILL0_m
