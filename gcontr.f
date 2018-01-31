      SUBROUTINE GCONTR(Z, NRZ, NX, NY, CV, NCV, ZMAX, BITMAP, DRAW)    
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
!     NRZ IS THE NUMBER OF ROWS DECLARED FOR Z IN THE CALLING PROGRAM.
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
!
      REAL Z(NRZ,1), CV(1)
      INTEGER BITMAP(1)
      INTEGER L1(4), L2(4), IJ(2)
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
      REAL XY(2)
!
!     XY IS USED TO COMPUTE COORDINATES FOR THE DRAW SUBROUTINE.
!
      EQUIVALENCE (L2(1),IMAX), (L2(2),JMAX), (L2(3),IMIN), &
        (L2(4),JMIN)
      EQUIVALENCE (IJ(1),I), (IJ(2),J)
      EQUIVALENCE (XY(1),X), (XY(2),Y)
!
      DATA L1(3) /-1/, L1(4) /-1/
      DATA I1 /1,0/, I2 /1,-1/, I3 /1,0,0,1,1,0/
!
      L1(1) = NX
      L1(2) = NY
      DMAX = ZMAX
!
!     SET THE CURRENT PEN POSITION.  THE DEFAULT POSITION CORRESPONDS
!     TO Z(1,1).
!
      X = 1.0
      Y = 1.0
      CALL DRAW(X, Y, 6)
      ICUR = MAX0(1,MIN0(INT(X),NX))
      JCUR = MAX0(1,MIN0(INT(Y),NY))
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
   10 I = ICUR
      J = JCUR
   20 IMAX = I
      IMIN = -I
      JMAX = J
      JMIN = -J
      IDIR = 0
!     DIRECTION ZERO IS +I, 1 IS +J, 2 IS -I, 3 IS -J.
   30 NXIDIR = IDIR + 1
      K = NXIDIR
      IF (NXIDIR.GT.3) NXIDIR = 0
   40 I = IABS(I)
      J = IABS(J)
      IF (Z(I,J).GT.DMAX) GO TO 140
      L = 1
!     L=1 MEANS HORIZONTAL LINE, L=2 MEANS VERTICAL LINE.
   50 IF (IJ(L).GE.L1(L)) GO TO 130
      II = I + I1(L)
      JJ = J + I1(3-L)
      IF (Z(II,JJ).GT.DMAX) GO TO 130
      ASSIGN 100 TO JUMP
!     THE NEXT 15 STATEMENTS (OR SO) DETECT BOUNDARIES.
   60 IX = 1
      IF (IJ(3-L).EQ.1) GO TO 80
      II = I - I1(3-L)
      JJ = J - I1(L)
      IF (Z(II,JJ).GT.DMAX) GO TO 70
      II = I + I2(L)
      JJ = J + I2(3-L)
      IF (Z(II,JJ).LT.DMAX) IX = 0
   70 IF (IJ(3-L).GE.L1(3-L)) GO TO 90
   80 II = I + I1(3-L)
      JJ = J + I1(L)
      IF (Z(II,JJ).GT.DMAX) GO TO 90
      IF (Z(I+1,J+1).LT.DMAX) GO TO JUMP, (100, 280)
   90 IX = IX + 2
      GO TO JUMP, (100, 280)
  100 IF (IX.EQ.3) GO TO 130
      IF (IX+IBKEY.EQ.0) GO TO 130
!     NOW DETERMINE WHETHER THE LINE SEGMENT IS CROSSED BY THE CONTOUR.
      II = I + I1(L)
      JJ = J + I1(3-L)
      Z1 = Z(I,J)
      Z2 = Z(II,JJ)
      DO 120 ICV=1,NCV
        IF (IGET(BITMAP,2*(NX*(NY*(ICV-1)+J-1)+I-1)+L).NE.0) GO TO 120
        IF (CV(ICV).LE.AMIN1(Z1,Z2)) GO TO 110
        IF (CV(ICV).LE.AMAX1(Z1,Z2)) GO TO 190
  110   CALL MARK1(BITMAP, 2*(NX*(NY*(ICV-1)+J-1)+I-1)+L)
  120 CONTINUE
  130 L = L + 1
      IF (L.LE.2) GO TO 50
  140 L = MOD(IDIR,2) + 1
      IJ(L) = ISIGN(IJ(L),L1(K))
!
!     LINES FROM Z(I,J) TO Z(I+1,J) AND Z(I,J+1) ARE NOT SATISFACTORY.
!     CONTINUE THE SPIRAL.
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
      CALL MARK1(BITMAP, 2*(NX*(NY*(ICV-1)+J-1)+I-1)+L)
      CALL DRAW(X, Y, IFLAG+10*ICV)
      IF (IFLAG.LT.4) GO TO 210
      ICUR = I
      JCUR = J
      GO TO 20
!
!     CONTINUE A CONTOUR.  THE EDGES ARE NUMBERED CLOCKWISE WITH
!     THE BOTTOM EDGE BEING EDGE NUMBER ONE.
!
  210 NI = 1
      IF (IEDGE.LT.3) GO TO 220
      I = I - I3(IEDGE)
      J = J - I3(IEDGE+2)
  220 DO 250 K=1,4
        IF (K.EQ.IEDGE) GO TO 250
        II = I + I3(K)
        JJ = J + I3(K+1)
        Z1 = Z(II,JJ)
        II = I + I3(K+1)
        JJ = J + I3(K+2)
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
      ASSIGN 280 TO JUMP
      IF (KS.LT.3) GO TO 270
      I = I + I3(KS)
      J = J + I3(KS+2)
      L = KS - 2
  270 IF (IGET(BITMAP,2*(NX*(NY*(ICV-1)+J-1)+I-1)+L).EQ.0) GO TO 60
      IFLAG = 5
      GO TO 290
  280 IF (IX.NE.0) IFLAG = 4
  290 IEDGE = KS + 2
      IF (IEDGE.GT.4) IEDGE = IEDGE - 4
      XINT(IEDGE) = XINT(KS)
      GO TO 200
!
      END
      DIMENSION Z(51,51), C(10), WORK(1680)                             
!     DIMENSION OF WORK IS LARGE ENOUGH TO CONTAIN                      
!     2*(DIMENSION OF C)*(TOTAL DIMENSION OF Z) USEFUL BITS.  SEE THE   
!     BITMAP ROUTINES ACCESSED BY GCONTR.                               
      REAL MU                                                           
      EXTERNAL DRAW                                                     
      COMMON /CUR/ XCUR, YCUR                                           
      DATA C(1), C(2), C(3), C(4), C(5) /3.05,3.2,3.5,3.50135,3.6/      
      DATA C(6), C(7), C(8), C(9), C(10) /3.766413,4.0,4.130149,5.0,     &
        10.0/                                                           
      DATA NX /51/, NY /51/, NF /10/                                    
      DATA XMIN /-2.0/, XMAX /2.0/, YMIN /-2.0/, YMAX /2.0/, MU /0.3/   
      DX = (XMAX-XMIN)/FLOAT(NX-1)                                      
      DY = (YMAX-YMIN)/FLOAT(NY-1)                                      
      XCUR = 1.0                                                        
      YCUR = 1.0                                                        
      IF (MOD(NX,2).NE.0) YCUR = FLOAT(NY)                              
      IF (MOD(NY,2).NE.0) XCUR = FLOAT(NX)                              
      X = XMIN - DX                                                     
      DO 20 I=1,NX                                                      
        Y = YMIN - DY                                                   
        X = X + DX                                                      
        DO 10 J=1,NY                                                    
          Y = Y + DY                                                    
          Z(I,J) = (1.0-MU)*(2.0/SQRT((X-MU)**2+Y**2)+(X-MU)**2+Y**2)    &
            + MU*(2.0/SQRT((X+1.0-MU)**2+Y**2)+(X+1.0-MU)**2+Y**2)      
   10   CONTINUE                                                        
   20 CONTINUE                                                          
      CALL GCONTR(Z, 51, NX, NY, C, NF, 1.E6, WORK, DRAW)               
      STOP                                                              
      END                                                               
      REAL Z(51,51), C(10), CVAL(10), MU                                
      INTEGER WORK(1680), L(10), CLAB(10)                               
!     DIMENSION OF WORK IS LARGE ENOUGH TO CONTAIN                      
!     2*(DIMENSION OF C)*(TOTAL DIMENSION OF Z) USEFUL BITS.  SEE THE   
!     BITMAP ROUTINES ACCESSED BY GCONTR.                               
      EXTERNAL DRAW                                                     
      COMMON /GCTCOM/ XCUR, YCUR, XL, YL, CVAL, CLAB, NCH               
      DATA C(1), C(2), C(3), C(4), C(5) /3.05,3.2,3.5,3.50135,3.6/      
      DATA C(6), C(7), C(8), C(9), C(10) /3.766413,4.0,4.130149,5.0,     &
        10.0/                                                           
      DATA L(1), L(2), L(3), L(4), L(5) /1HA,1HB,1HC,1HD,1HE/           
      DATA L(6), L(7), L(8), L(9), L(10) /1HF,1HG,1HH,1HI,1HJ/          
      DATA NX /51/, NY /51/, NF /10/, NXG /5/, NYG /5/                  
      DATA XMIN /-2.0/, XMAX /2.0/, YMIN /-2.0/, YMAX /2.0/, MU /0.3/   
      DATA XLEN /8.0/, YLEN /8.0/                                       
!     INITIALIZE PLOTTING SUBROUTINES.                                  
      CALL PLOTS                                                        
      DX = (XMAX-XMIN)/FLOAT(NX-1)                                      
      DY = (YMAX-YMIN)/FLOAT(NY-1)                                      
      XL = XLEN/FLOAT(NX)                                               
      YL = YLEN/FLOAT(NY)                                               
      XCUR = 1.0                                                        
      YCUR = 1.0                                                        
      IF (MOD(NX,2).NE.0) YCUR = FLOAT(NY)                              
      IF (MOD(NY,2).NE.0) XCUR = FLOAT(NX)                              
      X = XMIN - DX                                                     
      DO 20 I=1,NX                                                      
        Y = YMIN - DY                                                   
        X = X + DX                                                      
        DO 10 J=1,NY                                                    
          Y = Y + DY                                                    
!     EVALUATE FUNCTION TO BE PLOTTED.                                  
          Z(I,J) = (1.0-MU)*(2.0/SQRT((X-MU)**2+Y**2)+(X-MU)**2+Y**2)    &
            + MU*(2.0/SQRT((X+1.0-MU)**2+Y**2)+(X+1.0-MU)**2+Y**2)      
   10   CONTINUE                                                        
   20 CONTINUE                                                          
      DO 30 I=1,NF                                                      
        CVAL(I) = C(I)                                                  
        CLAB(I) = L(I)                                                  
   30 CONTINUE                                                          
      NCH = 1                                                           
!     PEN UP MOVE TO BELOW LOWER LEFT CORNER OF PAGE.                   
!     THIS CALL WORKS DIFFERENTLY ON DIFFERENT MACHINES.  YOU MAY       
!     NEED TO CHANGE IT.                                                
      CALL PLOT(0.0, -11.0, -3)                                         
!     PEN UP MOVE TO 1 INCH ABOVE LOWER LEFT CORNER OF PAGE.            
      CALL PLOT(0.0, 1.0, -3)                                           
      SX = 8.0/FLOAT(NXG)                                               
      SY = 8.0/FLOAT(NXG)                                               
!     DRAW A GRID.                                                      
      CALL CGRID(1, NXG, SX, 0.0, 0.0, NYG, SY, 0.0, 0.0)               
!     DRAW THE CONTOUR PLOTS.                                           
      CALL GCONTR(Z, 51, NX, NY, CVAL, NF, 1.0E6, WORK, DRAW)           
      XX = 9.0                                                          
      YY = 8.0                                                          
!     WRITE A TABLE OF CONTOUR LABELS AND VALUES.                       
      CALL SYMBOL(XX, YY+0.14, 0.07, 10HCONTOUR ID, 0.0, 10)            
      DO 40 I=1,NF                                                      
        CALL SYMBOL(XX, YY, 0.07, L(I), 0.0, 2)                         
        CALL NUMBER(XX+0.12, YY, 0.07, C(I), 0.0, 5)                    
        YY = YY - 0.14                                                  
   40 CONTINUE                                                          
!     PEN UP MOVE TO BELOW LOWER RIGHT CORNER OF PAGE.                  
!     THIS CALL WORKS DIFFERENTLY ON DIFFERENT MACHINES.  YOU MAY NEED  
!     TO CHANGE IT, OR YOU MAY NOT NEED IT.                             
      CALL PLOT(10.0, -11.0, -3)                                        
!     REDUCE PICTURE SIZE, PLOT END OF FILE INFORMATION.                
!     THE END OF FILE INFORMATION MAY NOT BE AVAILABLE AT ALL SITES.    
!     IF NOT AVAILABLE, CHANGE THE NEXT TWO STATEMENTS TO COMMENTS.     
      CALL FACTOR(0.3)                                                  
      CALL PLOT(0.0, 0.0, 999)                                          
      STOP                                                              
!                                                                       
      END                                                               
