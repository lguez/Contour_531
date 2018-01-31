      SUBROUTINE CGRID(NOPT, NX, SX, XS, XF, NY, SY, YS, YF)            
!
! SUBROUTINE WHICH DRAWS A FRAME AROUND THE PLOT AND DRAWS
! EITHER TICK MARKS OR GRID LINES.
!
! PARAMETERS:  NOPT -- =0, DRAW TICKS ONLY
!                      =1, DRAW GRID LINES
!                      =2, DRAW GRID LINES TO EDGE OF FRAME.
!              NX -- NUMBER OF INTERVALS IN X DIRECTION
!              SX -- SPACING IN INCHES BETWEEN TICK MARKS OR GRID LINES
!                    ALONG THE X AXIS
!              XS -- LOCATION OF FIRST TICK OR GRID LINE ON X AXIS
!              XF -- LOCATION OF RIGHT EDGE OF FRAME
!              NY -- NUMBER OF INTERVALS IN Y DIRECTION
!              SY -- SPACING IN INCHES BETWEEN TICK MARKS OR GRID LINES
!                    ALONG THE Y AXIS
!              YS -- LOCATION OF FIRST TICK OR GRID LINE ON Y AXIS
!              YF -- LOCATION OF TOP EDGE OF FRAME
! ASSUMPTIONS: NX, SX, NY, SY ALL POSITIVE.
!              THE LOWER LEFT-HAND CORNER OF THE FRAME IS DRAWN AT (0,0)
!              IF XS<0, USE 0; IF YS<0, USE 0
!              IF XF<=0, USE NX*SX; IF YF<=0, USE NY*SY.
!
      XINC = SX
      YINC = SY
      XLGTH = FLOAT(NX)*SX
      YLGTH = FLOAT(NY)*SY
      XMIN = AMAX1(XS,0.0)
      YMIN = AMAX1(YS,0.0)
      XMAX = AMAX1(XF,XLGTH+XMIN)
      YMAX = AMAX1(YF,YLGTH+YMIN)
!
!     DRAW FRAME.
!
      CALL PLOT(0.0, 0.0, 3)
      CALL PLOT(XMAX, 0.0, 2)
      CALL PLOT(XMAX, YMAX, 2)
      CALL PLOT(0.0, YMAX, 2)
      CALL PLOT(0.0, 0.0, 2)
      IF (NOPT.NE.0) GO TO 130
!
!     DRAW TICK MARKS.
!
      DO 120 J=1,4
        GO TO (10, 50, 20, 40), J
   10   X2 = 0.0
        IF (XMIN.NE.0.0) X2 = XMIN - SX
        Y2 = 0.0
        GO TO 30
   20   XINC = -SX
        X2 = XMIN + XLGTH + SX
        IF (XMAX.EQ.XMIN+XLGTH) X2 = XMAX
        Y2 = YMAX
   30   Y1 = Y2
        Y2 = Y2 + SIGN(0.125,XINC)
        N = NX
        IF (ABS(XMAX-XMIN-XLGTH)+ABS(XMIN)) 70, 80, 70
   40   YINC = -SY
        Y2 = YMIN + YLGTH + SY
        IF (YMAX.EQ.YMIN+YLGTH) Y2 = YMAX
        X2 = 0.0
        GO TO 60
   50   Y2 = 0.0
        IF (YMIN.NE.0.0) Y2 = YMIN - SY
        X2 = XMAX
   60   X1 = X2
        N = NY
        X2 = X2 - SIGN(0.125,YINC)
        IF (ABS(YMAX-YMIN-YLGTH)+ABS(YMIN)) 70, 80, 70
   70   N = N + 1
   80   DO 110 I=1,N
          IF (MOD(J,2).EQ.0) GO TO 90
          X2 = X2 + XINC
          X1 = X2
          GO TO 100
   90     Y2 = Y2 + YINC
          Y1 = Y2
  100     CALL PLOT(X1, Y1, 3)
          CALL PLOT(X2, Y2, 2)
  110   CONTINUE
  120 CONTINUE
      GO TO 240
!
!     DRAW GRID LINES
!
  130 X1 = XMIN
      X2 = XMIN + XLGTH
      IF (NOPT.NE.2) GO TO 140
      X1 = 0.0
      X2 = XMAX
  140 Y1 = YMIN - SY
      N = NY + 1
      IF (YMAX.EQ.YMIN+YLGTH) N = N - 1
      IF (YMIN.NE.0.0) GO TO 150
      Y1 = 0.0
      N = N - 1
  150 IF (N.LE.0) GO TO 170
      J = 1
      DO 160 I=1,N
        J = -J
        Y1 = Y1 + SY
        CALL PLOT(X1, Y1, 3)
        CALL PLOT(X2, Y1, 2)
        XX = X1
        X1 = X2
        X2 = XX
  160 CONTINUE
  170 Y1 = YMIN + YLGTH
      Y2 = YMIN
      IF (NOPT.NE.2) GO TO 180
      Y1 = YMAX
      Y2 = 0.0
  180 N = NX + 1
      IF (J.LT.0) GO TO 200
      X1 = XMIN - SX
      IF (XMAX.EQ.XMIN+XLGTH) N = N - 1
      IF (XMIN.NE.0.0) GO TO 190
      X1 = 0.0
      N = N - 1
  190 IF (N.LE.0) GO TO 240
      XINC = SX
      GO TO 220
  200 X1 = XMIN + XLGTH + SX
      IF (XMIN.EQ.0.0) N = N - 1
      IF (XMAX.NE.XLGTH+XMIN) GO TO 210
      N = N - 1
      X1 = XMAX
  210 XINC = -SX
  220 DO 230 I=1,N
        X1 = X1 + XINC
        CALL PLOT(X1, Y1, 3)
        CALL PLOT(X1, Y2, 2)
        XX = Y1
        Y1 = Y2
        Y2 = XX
  230 CONTINUE
  240 RETURN
!
      END
