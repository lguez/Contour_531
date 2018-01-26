      module gcontr_m

        implicit none

      contains
      SUBROUTINE GCONTR(Z, CV, DRAW, ZMAX)
!
!     THIS SUBROUTINE DRAWS A CONTOUR THROUGH EQUAL VALUES OF AN ARRAY.
!
!     *****     FORMAL ARGUMENTS     ***********************************
!
!     Z IS THE ARRAY FOR WHICH CONTOURS ARE TO BE DRAWN.  THE ELEMENTS
!     OF Z ARE ASSUMED TO LIE UPON THE NODES OF A TOPOLOGICALLY
!     RECTANGULAR COORDINATE SYSTEM - E.G. CARTESIAN, POLAR (EXCEPT
!     THE ORIGIN), ETC.
!
!     NX IS THE LIMIT FOR THE FIRST SUBSCRIPT OF Z.
!
!     NY IS THE LIMIT FOR THE SECOND SUBSCRIPT OF Z.
!
!     CV ARE THE VALUES OF THE CONTOURS TO BE DRAWN.
!
!     NCV IS THE NUMBER OF CONTOUR VALUES IN CV.
!
!     ZMAX IS THE MAXIMUM VALUE OF Z FOR CONSIDERATION.  A VALUE OF
!     Z(I,J) GREATER THAN ZMAX IS A SIGNAL THAT THAT POINT AND THE
!     GRID LINE SEGMENTS RADIATING FROM THAT POINT TO IT'S NEIGHBORS
!     ARE TO BE EXCLUDED FROM CONTOURING.
!
!     BITMAP IS A WORK AREA LARGE ENOUGH TO HOLD 2*NX*NY*NCV BITS.  IT
!     IS ACCESSED BY LOW-LEVEL ROUTINES, WHICH ARE DESCRIBED BELOW.
!     LET J BE THE NUMBER OF USEFUL BITS IN EACH WORD OF BITMAP,
!     AS DETERMINED BY THE USER MACHINE AND IMPLEMENTATION OF
!     THE BITMAP MANIPULATION SUBPROGRAMS DESCRIBED BELOW.  THEN
!     THE NUMBER OF WORDS REQUIRED FOR THE BITMAP IS THE FLOOR OF
!         (2*NX*NY*NCV+J-1)/J.
!
!     DRAW IS A USER-PROVIDED SUBROUTINE USED TO DRAW CONTOURS.
!     THE CALLING SEQUENCE FOR DRAW IS:
!
!         CALL DRAW (X,Y,IFLAG)
!         LET NX = INTEGER PART OF X, FX = FRACTIONAL PART OF X.
!         THEN X SHOULD BE INTERPRETED SUCH THAT INCREASES IN NX
!         CORRESPOND TO INCREASES IN THE FIRST SUBSCRIPT OF Z, AND
!         FX IS THE FRACTIONAL DISTANCE FROM THE ABSCISSA CORRESPONDING
!         TO NX TO THE ABSCISSA CORRESPONDING TO NX+1,
!         AND Y SHOULD BE INTERPRETED SIMILARLY FOR THE SECOND
!         SUBSCRIPT OF Z.
!         THE LOW-ORDER DIGIT OF IFLAG WILL HAVE ONE OF THE VALUES:
!             1 - CONTINUE A CONTOUR,
!             2 - START A CONTOUR AT A BOUNDARY,
!             3 - START A CONTOUR NOT AT A BOUNDARY,
!             4 - FINISH A CONTOUR AT A BOUNDARY,
!             5 - FINISH A CLOSED CONTOUR (NOT AT A BOUNDARY).
!                 NOTE THAT REQUESTS 1, 4 AND 5 ARE FOR PEN-DOWN
!                 MOVES, AND THAT REQUESTS 2 AND 3 ARE FOR PEN-UP
!                 MOVES.
!             6 - SET X AND Y TO THE APPROXIMATE 'PEN' POSITION, USING
!                 THE NOTATION DISCUSSED ABOVE.  THIS CALL MAY BE
!                 IGNORED, THE RESULT BEING THAT THE 'PEN' POSITION
!                 IS TAKEN TO CORRESPOND TO Z(1,1).
!         IFLAG/10 IS THE CONTOUR NUMBER.
!
!     *****     EXTERNAL SUBPROGRAMS     *******************************
!
!     DRAW IS THE USER-SUPPLIED LINE DRAWING SUBPROGRAM DESCRIBED ABOVE.
!     DRAW MAY BE SENSITIVE TO THE HOST COMPUTER AND TO THE PLOT DEVICE.
!     FILL0 IS USED TO FILL A BITMAP WITH ZEROES.  CALL FILL0 (BITMAP,N)
!     FILLS THE FIRST N BITS OF BITMAP WITH ZEROES.
!     MARK1 IS USED TO PLACE A 1 IN A SPECIFIC BIT OF THE BITMAP.
!     CALL MARK1 (BITMAP,N) PUTS A 1 IN THE NTH BIT OF THE BITMAP.
!     IGET IS USED TO DETERMINE THE SETTING OF A PARTICULAR BIT IN THE
!     BITMAP.  I=IGET(BITMAP,N) SETS I TO ZERO IF THE NTH BIT OF THE
!     BITMAP IS ZERO, AND SETS I TO ONE IF THE NTH BIT IS ONE.
!     FILL0, MARK1 AND IGET ARE MACHINE SENSITIVE.
!
!     ******************************************************************

        use FILL0_m, only: fill0
        use iget_m, only: iget
        use MARK1_m, only: MARK1
        
        REAL, intent(in):: Z(:, :) ! (nx, ny)
        real, intent(in):: cv(:) ! (ncv)

      interface
         subroutine draw(x, y, iflag)
         implicit none
         real, intent(inout):: x, y
         integer, intent(in):: iflag
         end subroutine draw
      end interface
      
      real, intent(in), optional:: zmax
      integer bitmap((2 * size(z) * size(cv) - 1) / (bit_size(0) - 1) + 1)
      INTEGER L1(4)
      integer L2(4) ! [IMAX, JMAX, IMIN, JMIN]
      integer IJ(2) ! [i, j]
!
!     L1 AND L2 CONTAIN LIMITS USED DURING THE SPIRAL SEARCH FOR THE
!     BEGINNING OF A CONTOUR.
!     IJ STORES SUBCRIPTS USED DURING THE SPIRAL SEARCH.
!
      INTEGER I1(2), I2(2), I3(6)
!
!     I1, I2 AND I3 ARE USED FOR SUBSCRIPT COMPUTATIONS DURING THE
!     EXAMINATION OF LINES FROM Z(I,J) TO IT'S NEIGHBORS.
!
      REAL XINT(4)
!
!     XINT IS USED TO MARK INTERSECTIONS OF THE CONTOUR UNDER
!     CONSIDERATION WITH THE EDGES OF THE CELL BEING EXAMINED.
!
      REAL XY(2) ! [x, y]
      !     XY IS USED TO COMPUTE COORDINATES FOR THE DRAW SUBROUTINE.

      integer nx, ny, ncv, icur, JCUR, IBKEY, IDIR, NXIDIR, k, l, ii, jj, jump
      integer ix, icv, IEDGE, IFLAG, NI, ks
      real dmax, Z1, z2, CVAL, zz
!
      DATA L1(3) /-1/, L1(4) /-1/
      DATA I1 /1,0/, I2 /1,-1/, I3 /1,0,0,1,1,0/

      !--------------------------------------------------------------------

      nx = size(z, 1)
      ny = size(z, 2)
      ncv = size(cv)
      L1(1) = NX
      L1(2) = NY
      if (present(zmax)) then
         DMAX = ZMAX
      else
         dmax = huge(0.)
      end if
!
!     SET THE CURRENT PEN POSITION.  THE DEFAULT POSITION CORRESPONDS
!     TO Z(1,1).
!
      XY(1) = 1.0
      XY(2) = 1.0
      CALL DRAW(XY(1), XY(2), 6)
      ICUR = MAX0(1,MIN0(INT(XY(1)),NX))
      JCUR = MAX0(1,MIN0(INT(XY(2)),NY))
!
!     CLEAR THE BITMAP
!
      CALL FILL0(BITMAP, 2*NX*NY*NCV)
!
!     SEARCH ALONG A RECTANGULAR SPIRAL PATH FOR A LINE SEGMENT HAVING
!     THE FOLLOWING PROPERTIES:
!          1.  THE END POINTS ARE NOT EXCLUDED,
!          2.  NO MARK HAS BEEN RECORDED FOR THE SEGMENT,
!          3.  THE VALUES OF Z AT THE ENDS OF THE SEGMENT ARE SUCH THAT
!              ONE Z IS LESS THAN THE CURRENT CONTOUR VALUE, AND THE
!              OTHER IS GREATER THAN OR EQUAL TO THE CURRENT CONTOUR
!              VALUE.
!
!     SEARCH ALL BOUNDARIES FIRST, THEN SEARCH INTERIOR LINE SEGMENTS.
!     NOTE THAT THE INTERIOR LINE SEGMENTS NEAR EXCLUDED POINTS MAY BE
!     BOUNDARIES.
!
      IBKEY = 0
   10 IJ(1) = ICUR
      IJ(2) = JCUR
   20 L2(1) = IJ(1)
      L2(3) = -IJ(1)
      L2(2) = IJ(2)
      L2(4) = -IJ(2)
      IDIR = 0
!     DIRECTION ZERO IS +IJ(1), 1 IS +IJ(2), 2 IS -IJ(1), 3 IS -IJ(2).
   30 NXIDIR = IDIR + 1
      K = NXIDIR
      IF (NXIDIR.GT.3) NXIDIR = 0
   40 IJ(1) = IABS(IJ(1))
      IJ(2) = IABS(IJ(2))
      IF (Z(IJ(1),IJ(2)).GT.DMAX) GO TO 140
      L = 1
!     L=1 MEANS HORIZONTAL LINE, L=2 MEANS VERTICAL LINE.
50    continue
      IF (IJ(L).GE.L1(L)) GO TO 130
      II = IJ(1) + I1(L)
      JJ = IJ(2) + I1(3-L)
      IF (Z(II,JJ).GT.DMAX) GO TO 130
      jump = 100
!     THE NEXT 15 STATEMENTS (OR SO) DETECT BOUNDARIES.
   60 IX = 1

      IF (IJ(3-L) /= 1) then
         II = IJ(1) - I1(3-L)
         JJ = IJ(2) - I1(L)
         IF (Z(II,JJ) <= DMAX) then
            II = IJ(1) + I2(L)
            JJ = IJ(2) + I2(3-L)
            IF (Z(II,JJ).LT.DMAX) IX = 0
         end IF
         IF (IJ(3-L).GE.L1(3-L)) GO TO 90
      end IF

      II = IJ(1) + I1(3-L)
      JJ = IJ(2) + I1(L)
      IF (Z(II,JJ).GT.DMAX) GO TO 90
      IF (Z(IJ(1)+1,IJ(2)+1).LT.DMAX) then
         if (jump == 100) then
            GO TO 100
         else if (jump == 280) then
            go to 280
         else
            stop 1
         end if
      end IF
90    continue
      IX = IX + 2
      if (jump == 100) then
         GO TO 100
      else if (jump == 280) then
         go to 280
      else
         stop 1
      end if
100   continue
      IF (IX.EQ.3) GO TO 130
      IF (IX+IBKEY.EQ.0) GO TO 130
!     NOW DETERMINE WHETHER THE LINE SEGMENT IS CROSSED BY THE CONTOUR.
      II = IJ(1) + I1(L)
      JJ = IJ(2) + I1(3-L)
      Z1 = Z(IJ(1),IJ(2))
      Z2 = Z(II,JJ)
      DO 120 ICV=1,NCV
        IF (IGET(BITMAP,2*(NX*(NY*(ICV-1)+IJ(2)-1)+IJ(1)-1)+L).NE.0) GO TO 120
        IF (CV(ICV).LE.AMIN1(Z1,Z2)) GO TO 110
        IF (CV(ICV).LE.AMAX1(Z1,Z2)) GO TO 190
  110   CALL MARK1(BITMAP, 2*(NX*(NY*(ICV-1)+IJ(2)-1)+IJ(1)-1)+L)
  120 CONTINUE
  130 L = L + 1
      IF (L.LE.2) GO TO 50
  140 L = MOD(IDIR,2) + 1
      IJ(L) = ISIGN(IJ(L),L1(K))
!
!     LINES FROM Z(IJ(1),IJ(2)) TO Z(IJ(1)+1,IJ(2)) AND
!     Z(IJ(1),IJ(2)+1) ARE NOT SATISFACTORY.  CONTINUE THE SPIRAL.
!
  150 IF (IJ(L).GE.L1(K)) GO TO 170
      IJ(L) = IJ(L) + 1
      IF (IJ(L).GT.L2(K)) GO TO 160
      GO TO 40
  160 L2(K) = IJ(L)
      IDIR = NXIDIR
      GO TO 30
  170 IF (IDIR.EQ.NXIDIR) GO TO 180
      NXIDIR = NXIDIR + 1
      IJ(L) = L1(K)
      K = NXIDIR
      L = 3 - L
      IJ(L) = L2(K)
      IF (NXIDIR.GT.3) NXIDIR = 0
      GO TO 150
  180 IF (IBKEY.NE.0) RETURN
      IBKEY = 1
      GO TO 10
!
!     AN ACCEPTABLE LINE SEGMENT HAS BEEN FOUND.
!     FOLLOW THE CONTOUR UNTIL IT EITHER HITS A BOUNDARY OR CLOSES.
!
  190 IEDGE = L
      CVAL = CV(ICV)
      IF (IX.NE.1) IEDGE = IEDGE + 2
      IFLAG = 2 + IBKEY
      XINT(IEDGE) = (CVAL-Z1)/(Z2-Z1)
  200 XY(L) = FLOAT(IJ(L)) + XINT(IEDGE)
      XY(3-L) = FLOAT(IJ(3-L))
      CALL MARK1(BITMAP, 2*(NX*(NY*(ICV-1)+IJ(2)-1)+IJ(1)-1)+L)
      CALL DRAW(XY(1), XY(2), IFLAG+10*ICV)
      IF (IFLAG.LT.4) GO TO 210
      ICUR = IJ(1)
      JCUR = IJ(2)
      GO TO 20
!
!     CONTINUE A CONTOUR.  THE EDGES ARE NUMBERED CLOCKWISE WITH
!     THE BOTTOM EDGE BEING EDGE NUMBER ONE.
!
  210 NI = 1
      IF (IEDGE >= 3) then
         IJ(1) = IJ(1) - I3(IEDGE)
         IJ(2) = IJ(2) - I3(IEDGE+2)
      end IF
      DO 250 K=1,4
        IF (K.EQ.IEDGE) GO TO 250
        II = IJ(1) + I3(K)
        JJ = IJ(2) + I3(K+1)
        Z1 = Z(II,JJ)
        II = IJ(1) + I3(K+1)
        JJ = IJ(2) + I3(K+2)
        Z2 = Z(II,JJ)
        IF (CVAL.LE.AMIN1(Z1,Z2)) GO TO 250
        IF (CVAL.GT.AMAX1(Z1,Z2)) GO TO 250
        IF (K.EQ.1) GO TO 230
        IF (K.NE.4) GO TO 240
  230   ZZ = Z1
        Z1 = Z2
        Z2 = ZZ
  240   XINT(K) = (CVAL-Z1)/(Z2-Z1)
        NI = NI + 1
        KS = K
  250 CONTINUE
      IF (NI.EQ.2) GO TO 260
!
!     THE CONTOUR CROSSES ALL FOUR EDGES OF THE CELL BEING EXAMINED.
!     CHOOSE THE LINES TOP-TO-LEFT AND BOTTOM-TO-RIGHT IF THE
!     INTERPOLATION POINT ON THE TOP EDGE IS LESS THAN THE INTERPOLATION
!     POINT ON THE BOTTOM EDGE.  OTHERWISE, CHOOSE THE OTHER PAIR.  THIS
!     METHOD PRODUCES THE SAME RESULTS IF THE AXES ARE REVERSED.  THE
!     CONTOUR MAY CLOSE AT ANY EDGE, BUT MUST NOT CROSS ITSELF INSIDE
!     ANY CELL.
!
      KS = 5 - IEDGE
      IF (XINT(3).LT.XINT(1)) GO TO 260
      KS = 3 - IEDGE
      IF (KS.LE.0) KS = KS + 4
!
!     DETERMINE WHETHER THE CONTOUR WILL CLOSE OR RUN INTO A BOUNDARY
!     AT EDGE KS OF THE CURRENT CELL.
!
  260 L = KS
      IFLAG = 1
      JUMP = 280
      IF (KS >= 3) then
         IJ(1) = IJ(1) + I3(KS)
         IJ(2) = IJ(2) + I3(KS+2)
         L = KS - 2
      end IF
      IF (IGET(BITMAP,2*(NX*(NY*(ICV-1)+IJ(2)-1)+IJ(1)-1)+L).EQ.0) GO TO 60
      IFLAG = 5
      GO TO 290
280   continue
      IF (IX.NE.0) IFLAG = 4
  290 IEDGE = KS + 2
      IF (IEDGE.GT.4) IEDGE = IEDGE - 4
      XINT(IEDGE) = XINT(KS)
      GO TO 200
!
      END SUBROUTINE GCONTR
      end module gcontr_m
      
