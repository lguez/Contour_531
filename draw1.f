      SUBROUTINE DRAW1(X, Y, IFLAG)                                      
!     THIS SUBROUTINE USES CALCOMP PLOT ROUTINES TO DRAW LINES FOR THE
!     CONTOUR PLOTTING ROUTINE GCONTR.
      REAL CVAL(10)
      INTEGER CLAB(10)
      COMMON /GCTCOM/ XCUR, YCUR, XL, YL, CVAL, CLAB, NCH
      DATA IBLANK /1H /
      IH = IFLAG/10
      IL = IFLAG - 10*IH
      IF (IL.EQ.6) GO TO 40
      IPEN = 2
      IF (IL.EQ.2) IPEN = 3
      IF (IL.EQ.3) IPEN = 3
      XCUR = X
      YCUR = Y
      XX = (X-1.0)*XL
      YY = (Y-1.0)*YL
      CALL PLOT(XX, YY, IPEN)
      IF (IL.LT.2) GO TO 30
      IF (IL.GT.4) GO TO 30
      IF (NCH.LT.1) GO TO 30
      IF (CLAB(IH).EQ.IBLANK) GO TO 30
      IF (CLAB(IH).NE.0) GO TO 10
      CALL NUMBER(XX, YY-0.03, 0.07, CVAL(IH), 0.0, -1)
      GO TO 20
   10 CALL SYMBOL(XX, YY-0.03, 0.07, CLAB(IH), 0.0, NCH)
   20 CALL PLOT(XX, YY, 3)
   30 RETURN
   40 X = XCUR
      Y = YCUR
      RETURN
!
      END
