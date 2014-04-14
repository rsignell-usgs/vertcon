
C      SCCSID[]= "@(#) angle.f 1.1 - 99/10/20"

      subroutine angle(degrees,deg,min,sec)
c
      integer*4   deg
      integer*4   min
      real*4      sec
      real*8      degrees
c
      deg=degrees
      min=(degrees-deg)*60.d0
      sec=(degrees-deg-min/60.d0)*3600.d0
      if( (sec+0.000001).ge.60.0 )then
        min=min+1
        sec=sec-60.0
      endif
      if( min.ge.60 )then
        deg=deg+1
        min=min-1
      endif
      if( deg.ge.360 )then
        deg=deg-360
      endif
      return
      end

C      SCCSID[]= "@(#) askpt.f 1.1 - 99/10/20"

C
      SUBROUTINE ASKPT (NCONV, NAME, IDLA, IMLA, SLA,
     +                  IDLO, IMLO, SLO, XPT, YPT, EOF, NOPT)
C
C     Interactively ask for the name and location of a point
C
      LOGICAL          EOF, NOPT, T1, T2, T3, T4, G_UNIT
      CHARACTER*80     CCARD
      CHARACTER*40     ANS, DUM
      CHARACTER*40     NAME
      CHARACTER*40     B40
      CHARACTER*1      CH1, CH2, CH3
      INTEGER          IOS, IERR, IDLA, IMLA, IDLO, IMLO
      INTEGER          IFLAG1, IFLAG2
      INTEGER          N1, N2, N3, N4, NQ, NCONV
      INTEGER          LU, LENG
      REAL             SLA, SLO, RMLA, RMLO, RCARD
      DOUBLE PRECISION XPT, YPT, RDLA, RDLO, DCARD
C
      PARAMETER (B40='                                        ')
C
      INTEGER          NQ_PGM 
      INTEGER          LUIN, LUOUT, NIN, NOUT
      INTEGER          LDUMP,NSPACE(2) 
      COMMON/INOUT/ NQ_PGM,LUIN,LUOUT,NOUT,NIN,LDUMP,NSPACE
C
      CHARACTER*2      CODE, DATUM
      DOUBLE PRECISION HGT
      COMMON/WEBCCC/HGT,CODE,DATUM
C
      DATA IFLAG1 /1/, IFLAG2 /2/
C
      LU = NOUT
C
      IF( NQ_PGM.EQ.0 )THEN
        IF( NCONV.EQ.0 )THEN
          WRITE (LUOUT,101) 
          WRITE (LUOUT,110)
        ENDIF
      ENDIF
C
  101 FORMAT(15x,'INTERACTIVE PROCESSING',//'  Station Name,',
     +   ' Latitude and Longitude are entered -',//
     +   '  [station name entry is not required;',
     +   '   the position entries',/'   (i.e., latitude & longitude)',
     +   ' are identifed with generated'/,'   sequence numbers',
     +   ' when station name is not entered]'/)
  110 FORMAT(' Latitudes and Longitudes may be entered',
     +   ' in three formats:', //
     +   '   (1) degrees, minutes, and decimal seconds, OR', /,
     +   '   (2) degrees, and decimal minutes OR', /,
     +   '   (3) decimal degrees.', /,
     +   '       [decimal points must be entered !]'/
     +   ' Degrees, minutes and seconds can be separated',
     +   ' by either blanks or commas.', //
     +   ' To terminate session :'/'  (1) key in ''n'' or',
     +   '''N'' for ''another computation ?'' prompt  - or -'/
     +   '  (2) press <RETURN> for latitude or longitude',
     +   ' prompt  - or -'/'  (3) enter <ctrl-c>'/)
C
C     NAME       
C
      NAME = '        '
      IF( NQ_PGM.EQ.2 )THEN
C       DO NOTHING
        NAME = 'NO_NAME '
      ELSE
        WRITE (LUOUT,*) ' '
        WRITE (LUOUT,*) 'Station       Enter the NAME :    ',
     +                  'or press <RETURN> to skip NAME -'
        READ (LUIN,'(A40)') NAME
      ENDIF
C
C     LATITUDE
C
      NQ = 1
      IF( NQ_PGM.LE.1 )THEN
        WRITE (LUOUT,*) ' '
        WRITE (LUOUT,*) 'DD MM SS.ss   Enter the LATITUDE :'
      ENDIF
C
  170 FORMAT(A40)
C
      READ (LUIN,170,ERR=9930,IOSTAT=IOS) ANS
C
      IF( ANS.EQ.B40 )THEN
        IF( NQ_PGM.EQ.2 )THEN
          GOTO 9950
        ELSE
          GOTO 9999
        ENDIF
      ENDIF
C
      DUM = ANS
      CALL NBLANK (DUM, IFLAG2, N2)
      LENG = N2
      RDLA = DCARD ( DUM(1:N2), LENG, IERR )
      IF( IERR.EQ.0 )THEN
        IF( LENG.GT.0 )THEN
          RMLA = RCARD ( DUM, LENG, IERR )
          IF( IERR.EQ.0 )THEN
            IF( LENG.GT.0 )THEN
              SLA  = RCARD ( DUM, LENG, IERR )
              IF( IERR.EQ.0 )THEN
C               DO NOTHING ... RDLA, RMLA, SLA ... WERE FOUND
              ELSE
                GOTO 9950
              ENDIF
            ELSE
              SLA = 0.E0
            ENDIF
          ELSE
            GOTO 9950
          ENDIF
        ELSE
          RMLA = 0.E0
          SLA  = 0.E0
        ENDIF
      ELSE
        GOTO 9950
      ENDIF
C
      IF( (RDLA.EQ.0.D0).AND.(RMLA.EQ.0.E0).AND.(SLA.EQ.0.E0) )THEN
        IF( NQ_PGM.EQ.2 )THEN
          GOTO 9950
        ELSE
          GOTO 9999
        ENDIF
      ENDIF
C
C     Check for illogical latitude values
C
      T1 = ( RDLA .LT. 0.D0  )
      T2 = ( RDLA .GT. 90.D0 )
      T3 = ( RMLA .LT. 0.E0 .OR. RMLA .GT. 60.E0 )
      T4 = (  SLA .LT. 0.E0 .OR.  SLA .GT. 60.E0 )
C
C     DISPLAY THE LATITUDE DATA ENTRY ERROR 
C
      IF( T1.OR.T2.OR.T3.OR.T4 )THEN
        IF( T1 )THEN
          GOTO 9940
        ELSE
          GOTO 9950
        ENDIF
      ENDIF
C
C     Calculate decimal degrees
C
      YPT = RDLA + DBLE(RMLA)/60.D0 + DBLE(SLA)/3600.D0
C
      IF( DABS(YPT).GE.90.0D0 )THEN
        GOTO 9950
      ENDIF  
C
C     LONGITUDE
C
      IF( NQ_PGM.LE.1 )THEN
        WRITE (LUOUT,*) ' '
        WRITE (LUOUT,*) 'DDD MM SS.ss  Enter the LONGITUDE :',
     +                  ' (longitude is positive west)'
      ENDIF
C
      READ (LUIN,170,ERR=9930,IOSTAT=IOS) ANS
C
      IF( ANS.EQ.B40 )THEN
        IF( NQ_PGM.EQ.2 )THEN
          GOTO 9960
        ELSE
          GOTO 9999
        ENDIF
      ENDIF
C
      DUM = ANS
      CALL NBLANK (DUM, IFLAG2, N2)
      LENG = N2
      RDLO = DCARD ( DUM(1:N2), LENG, IERR )
      IF( IERR.EQ.0 )THEN
        IF( LENG.GT.0 )THEN
          RMLO = RCARD ( DUM, LENG, IERR )
          IF( IERR.EQ.0 )THEN
            IF( LENG.GT.0 )THEN
              SLO  = RCARD ( DUM, LENG, IERR )
              IF( IERR.EQ.0 )THEN
C               DO NOTHING ... RDLO, RMLO, SLO ... WERE FOUND
              ELSE
                GOTO 9960
              ENDIF
            ELSE
              SLO = 0.E0
            ENDIF
          ELSE
            GOTO 9960
          ENDIF
        ELSE
          RMLO = 0.E0
          SLO  = 0.E0
        ENDIF
      ELSE
        GOTO 9960
      ENDIF
C
      IF( (RDLO.EQ.0.D0).AND.(RMLO.EQ.0.E0).AND.(SLO.EQ.0.E0) )THEN
        IF( NQ_PGM.EQ.2 )THEN
          GOTO 9960
        ELSE
          GOTO 9999
        ENDIF
      ENDIF
C
C     Check for illogical longitude values
C
      T1 = ( RDLO .LT.   0.D0 )
      T2 = ( RDLO .GT. 360.D0 ) 
      T3 = ( RMLO .LT. 0.E0 .OR. RMLO .GT. 60.E0 )
      T4 = (  SLO .LT. 0.E0 .OR.  SLO .GT. 60.E0 ) 
C
C     DISPLAY THE LONGITUDE DATA ENTRY ERROR 
C
      IF( T1.OR.T2.OR.T3.OR.T4 )THEN
        IF( T1 )THEN
          GOTO 9940
        ELSE
          GOTO 9960
        ENDIF
      ENDIF
C
C     Calculate decimal degrees
C
      XPT = RDLO + DBLE(RMLO)/60.D0 + DBLE(SLO)/3600.D0
C
      IF( DABS(XPT).GE.360.0D0 )THEN
        GOTO 9960
      ENDIF  
C
C     Get degrees, minutes, seconds
C
      CALL ANGLE (YPT, IDLA, IMLA, SLA)
      CALL ANGLE (XPT, IDLO, IMLO, SLO)
C
C     DEFAULT VALUES...(HGT,CODE,DATUM)... FOR WEB ENTRY
C
      IF( NQ_PGM.GT.0 )THEN
        NQ    = 2
        HGT   = -9999.0D0
        CODE  = 'MT'
        DATUM = '29'
        G_UNIT=.TRUE.
C
C       ORTHOMETRIC HGT (Elevation)
C
        IF( NQ_PGM.LE.1 )THEN
          WRITE (LUOUT,*) ' '
          WRITE (LUOUT,*) 'DDDD.ddd  MT  ',
     +   'Enter the ORTHOMETRIC HEIGHT : (Default is meters)'
        ENDIF
C
        READ (LUIN,170,ERR=9930,IOSTAT=IOS) ANS
C
        IF( ANS.EQ.B40 )THEN
          GOTO 8000
        ENDIF
C
        DUM = ANS
        CALL NBLANK (DUM, IFLAG2, N2)
C
        N3   = 0
        N4   = 0
        CH3  = ANS(N2:N2)
C
C       LOOK FOR "UNITS" IF ENTERED
C
        IF( CH3.EQ.'T' .OR. CH3.EQ.'t' )THEN
          N4  = N2
          N3  = N4-1
          CH2 = ANS(N3:N3)
          IF( CH2.EQ.'F' .OR. CH2.EQ.'f' )THEN
            CODE = 'FT'
          ELSE
            CODE = 'MT'
          ENDIF
C
          N2  = N3-1
 6998     CH1 = ANS(N2:N2)
          IF( CH1.EQ.' ' )THEN
            N2 = N2-1
            GOTO 6998
          ENDIF
C
          G_UNIT = .FALSE.
          GOTO 7000      
        ENDIF
C
C       LOOK FOR "M,F,m,f" IF ENTERED
C
        T1 = (CH3.EQ.'M' .OR. CH3.EQ.'m')
        T2 = (CH3.EQ.'F' .OR. CH3.EQ.'f')
C
        IF( T1 .OR. T2 )THEN
          N3 = N2
          IF( T1 )THEN
            CODE = 'MT'
          ELSE
            CODE = 'FT'
          ENDIF
C
          N2  = N3-1
 6999     CH1 = ANS(N2:N2)
          IF( CH1.EQ.' ' )THEN
            N2 = N2-1
            GOTO 6999
          ENDIF
C
          G_UNIT = .FALSE.
          GOTO 7000
        ENDIF
C
        IF( CH3.EQ.' ' )THEN
          G_UNIT = .FALSE.
          CODE = 'MT'
          N2   = N2-1
        ENDIF
C
C       RESIZE ARRAY AGAIN FOR EXTRA BLANKS IN THE ANSWER
C
 7000   CONTINUE
        IF( N2.GE.1 )THEN
          DUM = ANS(1:N2)
          CALL NBLANK (DUM(1:N2), IFLAG2, N2)
          LENG = N2
          HGT  = DCARD ( DUM(1:N2), LENG, IERR )
          IF( IERR.EQ.0 )THEN
            IF( -300.0D0.LT.HGT .AND. HGT.LT.20000.0D0 )THEN
C             DO NOTHING ... HGT ... WAS FOUND TO BE GOOD
            ELSE
              NQ = 2
              GOTO 9970
            ENDIF
          ELSE
            GOTO 9970
          ENDIF
        ENDIF
C
C       CODE UNITS FOR THE ORTHOMETRIC HGT
C
        IF( G_UNIT )THEN
          IF( NQ_PGM.LE.1 )THEN
            WRITE (LUOUT,*) ' '
            WRITE (LUOUT,*) 'MT or FT      Enter the UNITS : ',
     +                      '(Default is meters) '
C
            READ (LUIN,170,ERR=9930,IOSTAT=IOS) ANS
C
            IF( ANS.EQ.B40 )THEN
              GOTO 8000
            ENDIF
C
            DUM = ANS
            CALL NBLANK (DUM, IFLAG2, N2)
            LENG = N2
            CODE = CCARD ( DUM(1:N2), LENG, IERR )
            IF( IERR.EQ.0 )THEN
C             DO NOTHING ... CODE ... WAS FOUND
            ELSE
              GOTO 9970
            ENDIF
          ENDIF
        ENDIF
C
C       DATUM CODE ( A,G ) FOR THE ORTHOMETRIC HGT (Elevation)
C
 8000   NQ = 7
        IF( NQ_PGM.LE.1 )THEN
          WRITE (LUOUT,*) ' '
          WRITE (LUOUT,*) 'G or A        Enter the DATUM CODE : ',
     +    'G  for NGVD 29 or '
          WRITE (LUOUT,*) '                                     ',
     +    'A  for NAVD 88 '
        ENDIF
C
        READ (LUIN,170,ERR=9930,IOSTAT=IOS) ANS
C
        IF( ANS.EQ.B40 )THEN
          IF( NQ_PGM.EQ.2 )THEN
            GOTO 9980
          ELSE
            GOTO 8000
          ENDIF
        ENDIF
C
        CH1 = ANS(1:1)
        IF(     CH1.EQ.'A' .OR. CH1.EQ.'a' )THEN
          DATUM = '88'
        ELSEIF( CH1.EQ.'G' .OR. CH1.EQ.'g' )THEN
          DATUM = '29'
        ELSE
          GOTO 9980
        ENDIF
      ENDIF
C
 9000 RETURN
  162 FORMAT('ERROR',I6)
C
C     Error messages
C
 9930 CONTINUE
      IF( NQ_PGM.EQ.2 )THEN
        WRITE(LUOUT,162) NQ
      ELSE
        CALL NBLANK (ANS, IFLAG1, N1)
        CALL NBLANK (ANS, IFLAG2, N2)
        WRITE (LUOUT,9935) ANS(N1:N2)
      ENDIF
 9935 FORMAT(' ERROR - in the answer:', /,
     +    9X, '''', A, '''', /,
     +    '         Must enter number in prescribed format!', /)
      NOPT = .TRUE.
      GOTO 9000
 9940 CONTINUE
      IF( NQ_PGM.EQ.2 )THEN
        WRITE(LUOUT,162) NQ
      ELSE
        CALL NBLANK (ANS, IFLAG1, N1)
        CALL NBLANK (ANS, IFLAG2, N2)
        WRITE (LUOUT,9945) ANS(N1:N2)
      ENDIF
 9945 FORMAT(' ERROR - in the answer:', /,
     +    9X, '''', A, '''', /,
     +   '         Latitude and Longitudes must be positive!', /,
     +   '         Longitude is positive west.', /)
      NOPT = .TRUE.
      GOTO 9000
 9950 CONTINUE
      IF( NQ_PGM.EQ.2 )THEN
        WRITE(LUOUT,162) NQ
      ELSE
        CALL NBLANK (ANS, IFLAG1, N1)
        CALL NBLANK (ANS, IFLAG2, N2)
        WRITE (LUOUT,9955) ANS(N1:N2)
      ENDIF
 9955 FORMAT(' ERROR - Illogical value for latitude in the answer:', /,
     +   '         ''', A, '''', /,
     +   '         Latitude must be between 0 and 90 degrees.', /,
     +   '         Minutes and seconds must be between 0 and 60.'/)
      NOPT = .TRUE.
      GOTO 9000
 9960 CONTINUE
      IF( NQ_PGM.EQ.2 )THEN
        WRITE(LUOUT,162) NQ
      ELSE
        CALL NBLANK (ANS, IFLAG1, N1)
        CALL NBLANK (ANS, IFLAG2, N2)
        WRITE (LUOUT,9965) ANS(N1:N2)
      ENDIF
 9965 FORMAT(' ERROR - Illogical value for longitude in the answer:',/,
     +  '         ''', A, '''', /,
     +  '         Longitude must be between 0 and 360 degrees.',/,
     +  '         Minutes and seconds must be between 0 and 60.'/)
      NOPT = .TRUE.
      GOTO 9000
 9970 CONTINUE
      IF( NQ_PGM.EQ.2 )THEN
        WRITE(LUOUT,162) NQ
      ELSE
        CALL NBLANK (ANS, IFLAG1, N1)
        CALL NBLANK (ANS, IFLAG2, N2)
        WRITE (LUOUT,9975) ANS(N1:N2)
      ENDIF
 9975 FORMAT(' ERROR - Illogical value for Elevation in the answer:',/,
     +    '         ''', A, '''', /,
     +    '         Orthometric height must be between ',/,
     +    '         -300.0 and 20000.0 and ',/,
     +    '         Units of meters (MT).',/)
      NOPT = .TRUE.
      GOTO 9000
 9980 CONTINUE
      IF( NQ_PGM.EQ.2 )THEN
        WRITE(LUOUT,162) NQ
      ELSE
        CALL NBLANK (ANS, IFLAG1, N1)
        CALL NBLANK (ANS, IFLAG2, N2)
        WRITE (LUOUT,9985) ANS(N1:N2)
      ENDIF
 9985 FORMAT(' ERROR - Illogical value for Datum in the answer:',/,
     +       '         ''', A, '''', /,
     +       '         Datum must be either ',/,
     +       '         NGVD = G  or  NAVD = A'/)
      NOPT = .TRUE.
      GOTO 9000
 9999 CONTINUE
      EOF = .TRUE.
      GOTO 9000
      END

C      SCCSID[]= "@(#) ccard.f 1.1 - 99/10/20"

      CHARACTER*(*) FUNCTION CCARD (CHLINE, LENG, IERR)
C
C     Read a character variable from a line of card image.
C     LENG is the length of the card
C     blanks are the delimiters of the character variable
C
      CHARACTER*80      CHLINE
      INTEGER           I, IERR, ILENG
      INTEGER           J
      INTEGER           LENG
C
      IERR = 0
C
C     Find first non-blank character
C     DO WHILE line character is blank, 
C     I is first non-blank character
C
      I = 1
   10 IF( CHLINE(I:I).EQ.' ' .OR. CHLINE(I:I).EQ.',' )THEN
        I = I + 1
C
C       Check for totally blank card (assume length of 2)
C
        IF( I.GE.LENG )THEN
          CCARD = '  '
          RETURN
        ENDIF
        GOTO 10
      ENDIF
C
C     Find first blank character (or end of line)
C     DO WHILE line character is not a blank
C
      J = I + 1
   20 IF( CHLINE(J:J).NE.' ' .AND. CHLINE(J:J).NE.',' )THEN
        J = J + 1
* Check for totally filed card
        IF( J.GT.LENG )THEN
          GOTO 40
        ENDIF
        GOTO 20
      ENDIF
C
C     J is now 1 more than the position 
C     of the last non-blank character
C
   40 J = J - 1
C
C     ILENG is the length of the character string, 
C     it can be any length up to the length of the line
C
      ILENG = J - I + 1
      IF( ILENG.GT.LENG )THEN
        STOP 'CCARD'
      ENDIF
C
C     Read the char variable from the line, 
C     and set the return VAR to it
C
      READ (CHLINE(I:J), 55, ERR=9999) CCARD
   55 FORMAT (A)
C
C     Now reset the values of LENG and 
C     CHLINE to the rest of the card
C
      CHLINE(1:LENG) = CHLINE((J+1):LENG)
      LENG = LENG - J
      RETURN
C
C     Read error
C
 9999 IERR = 1
      RETURN
      END

C      SCCSID[]= "@(#) coeff.f 1.1 - 99/10/20"

c
      subroutine coeff (tee1,tee2,tee3,tee4,ay,bee,cee,dee)
      implicit double precision (a-h,o-z)
      ay=tee1                          
      bee=tee3-tee1
      cee=tee2-tee1
      dee=tee4-tee3-tee2+tee1
      return
      end

C      SCCSID[]= "@(#) dcard.f 1.1 - 99/10/20"

c
      DOUBLE PRECISION FUNCTION DCARD (CHLINE, LENG, IERR)
C
C     Read a double precision number from a line of card image.
C     LENG is the length of the card
C     blanks are the delimiters of the REAL*8 variable
C
      CHARACTER*80     CHLINE
      INTEGER          I, IERR, ILENG
      INTEGER          J
      INTEGER          LENG
      DOUBLE PRECISION VAR
C
      IERR = 0
C
C     Find first non-blank character
C     DO WHILE line character is blank, 
C     I is first non-blank character
C
      I = 1
   10 IF( CHLINE(I:I).EQ.' ' .OR. CHLINE(I:I).EQ.',' )THEN
        I = I + 1
C
C       Check for totally blank card
C
        IF( I.GE.LENG )THEN
          DCARD = 0.0D0
          LENG = 0
          RETURN
        ENDIF
        GOTO 10
      ENDIF
C
C     Find first blank character (or end of line)
C     DO WHILE line character is not a blank
C
      J = I + 1
   20 IF ( CHLINE(J:J) .NE. ' '  .AND.  CHLINE(J:J) .NE. ',' ) THEN
        J = J + 1
C
C       Check for totally filed card
C
        IF( J.GT.LENG )THEN
          GOTO 40
        ENDIF
        GOTO 20
      ENDIF
C
C     J is now 1 more than the position 
C     of the last non-blank character
C
   40 J = J - 1
C
C     ILENG is the length of the real string, it cannot be greater
C     than 15 characters
C
      ILENG = J - I + 1
      IF( ILENG.GT.20 )THEN
        STOP 'DCARD'
      ENDIF
C
C     Read the real variable from the line, and 
C     set the return VAR to it
C
      READ (CHLINE(I:J), 55, ERR=9999) VAR
   55 FORMAT (F20.0)
      DCARD = VAR
C
C     Now reset the values of LENG and 
C     CHLINE to the rest of the card
C
      CHLINE(1:LENG) = CHLINE((J+1):LENG)
      LENG = LENG - J
      RETURN
C
C     Read error
C
 9999 IERR = 1
      RETURN
      END

C      SCCSID[]= "@(#) fgrid.f 1.1 - 99/10/20"

c
      subroutine fgrid(lin,glo,gla,glomn,glamn,dglo,dgla,nlo,
     .   xgrid,ygrid,irow,jcol,tee1,tee2,tee3,tee4,ios)
c
      implicit double precision (a-h,o-z)
      parameter(lgh=461)
c
      integer dummy
      integer i, irow, ios
      integer jcol
      integer lin
      integer nlo  
      real*4  z(lgh)
      real*4  glamn,dgla,glomn,dglo
C
C     calculate the coordinates for the point
C     in terms of grid indices
C
      xgrid=(glo-glomn)/dglo+1.d0
      ygrid=(gla-glamn)/dgla+1.d0
C
C     find the i,j, values for the SW corner of local square
C
      irow=int(ygrid)
      jcol=int(xgrid)
c
      read(lin,rec=irow+1) dummy,(z(i),i=1,nlo)
      if( (z(jcol).eq.9999.0).or.(z(jcol+1).eq.9999.0) )then
        goto 10
      endif
c
      tee1=z(jcol)
      tee3=z(jcol+1)
c
      read(lin,rec=irow+2) dummy,(z(i),i=1,nlo)
      if( (z(jcol).eq.9999.0).or.(z(jcol+1).eq.9999.0) )then
        goto 10
      endif
c
      tee2=z(jcol)
      tee4=z(jcol+1)
      return
c
   10 ios = 999
      return
      end

C      SCCSID[]= "@(#) fhelp.f 1.1 - 99/10/20"

c
      SUBROUTINE FHELP(VRSION)
C
C     Print information about the formats of the input data
C     file types used by VERTCON.
C
      CHARACTER*15     VERSIO
      CHARACTER*6      COR0, COR1, COR2, COR3
      CHARACTER*1      ANS, FF
C
      INTEGER          NQ_PGM 
      INTEGER          LUIN, LUOUT, NIN, NOUT
      INTEGER          LDUMP,NSPACE(2) 
      COMMON/INOUT/ NQ_PGM,LUIN,LUOUT,NOUT,NIN,LDUMP,NSPACE
C
      DATA COR0,COR1,COR2,COR3/'      ',' -0.05','  0.10',' -0.03'/
      DATA VERSIO/'VERTCON Version'/
C
      FF = CHAR(12)
C
C     CHOSE THE INPUT FILE FORMAT
C
 9001 WRITE (LUOUT,*) ' '
      WRITE (LUOUT,*) ' What format do you want information about?'
      WRITE (LUOUT,*) '  1) Free Format Type 1'
      WRITE (LUOUT,*) '  2) Free Format Type 2'
      WRITE (LUOUT,*) '  3) NGS Blue Book Format Type 3'
      WRITE (LUOUT,*) '  4) NGS Internal Bench Mark Format Type 4'
C
      READ (LUIN,'(A1)') ANS
      IF( ANS .EQ. ' ' )THEN
         ITYPE = 5
      ELSE
        READ (ANS,90,ERR=9940,IOSTAT=IOS) ITYPE
   90   FORMAT (I1)
      ENDIF
C
      IF( ITYPE.GT.4 .OR. ITYPE.LT.1 )THEN
        GOTO 9940
      ENDIF
C
C     Print information
C
    2 FORMAT(A1)
    3 FORMAT(' The following is an example of the input.'/)
    4 FORMAT(' The following are TWO EXAMPES of the output.'/)
    9 FORMAT(' The following is an example of the output.'/)
  931 FORMAT(15X,    '     (Hit RETURN to continue.)')
C
      IF (ITYPE .EQ. 1) THEN
C
C       FOR FILE FORMAT ITYPE = 1
C       FREE FORMAT TYPE1 INPUT FILE
C       AND    OUTPUT FORMAT
C
        WRITE (LUOUT,2) FF
        WRITE (LUOUT, 110)
  110   FORMAT (' Free Format Type 1 - ',//,' The first 40 characters',
     +          ' of the input data record may contain the',/,
     +          ' station name or be blank.  The rest of the record',
     +          ' (columns 41-80)',/,' must contain'
     +          ' the latitude and longitude;',/,'  they may be',
     +          ' given either in',//,'  (1) decimal degrees; -OR-',/,
     +          '  (2) integer degrees and decimal minutes; -OR-',/,
     +          '  (3) integer degrees, minutes, and decimal seconds'/
     +          '      [decimal points must be entered !]'/)
C
        WRITE (LUOUT,3)
        WRITE (LUOUT, 120)
  120   FORMAT (' <- - - - - -  Columns 1-40  - - - - - ->',
     +                     '<------------ Columns 41-80----------->',
     +          ' AAA                                     34.',
     +                                     '4444444      98.8888888'/
     +          ' BBB                                     25',
     +                                   ' 55.55555     76 56.66666'/
     +          ' CCC                                     45 45',
     +                                     ' 45.555   111 11 11.111'/)
        WRITE (LUOUT,931)
        READ  (LUIN,'(A1)') ANS
        WRITE (LUOUT,2)
        WRITE (LUOUT,4)
        WRITE (LUOUT, 140)
  140   FORMAT (' Station Name:  AAA')
        WRITE (LUOUT, 142)
        WRITE (LUOUT, 143)
        WRITE (LUOUT, 141)
  141   FORMAT (' Station sequence #:   1')
        WRITE (LUOUT, 142)
  142   FORMAT ('     Latitude         Longitude',
     +          '    NAVD 88 - NGVD 29 (meters)',/
     +          '  34 26 39.99984   98 53',
     +          ' 19.99968        -0.16', /)
  143   FORMAT(20x,'- OR -'/)
        WRITE (LUOUT, 130)
  130   FORMAT (' NOTE -  that with Free Format Type 1 data -'//7x,
     +          ' Station Name is printed if not',
     +          ' blank in the input',/7x,
     +          '  else (generated) Station sequence #',
     +          ' is printed;'/7x,' Output latitude and longitude',
     +          ' are expressed',/7x,'  in degrees, minutes, and', 
     +          ' seconds regardless'/7x,'  of the method of input.'/)
      ELSEIF( ITYPE.EQ.2 )THEN
C
C       FOR FILE FORMAT ITYPE = 2
C       FREE FORMAT TYPE2 INPUT FILE
C       FREE FORMAT OUTPUT
C
        WRITE (LUOUT,2)
        WRITE (LUOUT, 210)
  210   FORMAT (' Free Format Type 2 - '//' The first 32 characters',
     +          ' of the input data record must contain the',/,
     +          ' latitude and longitude;  they may be given either in',
     +       //,'  (1) decimal degrees; -OR-',/,
     +          '  (2) integer degrees and decimal minutes; -OR-',/,
     +          '  (3) integer degrees, minutes, and decimal seconds',/
     +          '      [decimal points must be entered !]'//
     +          ' The rest of the input record ',
     +          ' (col.41-80) may contain the station name',/,
     +          '  or be blank')
        WRITE (LUOUT,931)
        READ  (LUIN,'(A1)') ANS
        WRITE (LUOUT,2)
        WRITE (LUOUT,3)
        WRITE (LUOUT, 220)
  220   FORMAT (' <- - - - - -  Columns 1-40 - - - - - ->',
     +          '<- - - - - -  Columns 41-80 - - - - ->')
        WRITE (LUOUT, 230) COR0,COR0,COR0
        WRITE (LUOUT,9)
        WRITE (LUOUT, 220)
        WRITE (LUOUT, 221) VERSIO,VRSION
  221   FORMAT(1x,A15,F5.2)
        WRITE (LUOUT, 230) COR1,COR2,COR3
  230   FORMAT (' 45 45 45.55555 111 11 11.11111  ',A6,' one', /,
     +          ' 25 55.5555555   76 56.6666666   ',A6,' two', /,
     +          ' 34.444444444    98.888888888    ',A6,' three', /)
        WRITE (LUOUT,5) VRSION
    5   FORMAT(' NOTE : The output record format is the same as the ',
     +    'input format -'/7x,' except : VERTCON Version',F5.2,
     +    ' (i.e., first line) was added, and'/,16x,
     +    ' It contains the NAVD 88 - NGVD 29 values (col. 33-39)'/)
      ELSEIF( ITYPE.EQ.3 )THEN
C
C       FOR INPUT FILE FORMAT ITYPE = 3
C       SAME OUTPUT
C
        WRITE (LUOUT,2)
        WRITE (LUOUT, 310)
  310   FORMAT (' NGS Blue Book format-'//
     +   ' Columns   Type',12x,'Contents'/25(' -')/
     +   ' 1 -   6   999999          sequence no.'/
     +   ' 7 -  10   char(4)         *30* code'/
     +   ' 11 - 14   9999            SPSN no.'/
     +   ' 15 - 39   char(25)        designation'/
     +   ' 40 - 41   char(2)         unit (KM=kilometers)'/
     +   ' 42 - 49   99999999        accumulated distance'/
     +   ' 50 - 51   char(2)         unit (MT=meters or FT=feet)'/
     +   ' 52 - 61   9999.99999      field elevation'/
     +   ' 62 - 67   char(6)         ACRN number'/
     +   ' 68 - 73   999999          latitude   (ddmmss)'/
     +   ' 74 - 80   9999999         longitude (dddmmss)'/)
        WRITE (LUOUT,6)
    6   FORMAT (' NOTE : Blue Book record *15* is required in the',
     +   ' input file !'/7x,' It must specify the input Datum in',
     +   ' columns 11 -16'/7x,' (e.g., NGVD29 or NAVD88) -'/
     +   ' VERTCON - converts NGVD29 field elevations to NAVD88 when',
     +   ' former is input',/35x,'- or -'/10x,' converts NAVD88',
     +   ' field elevations to NGVD29 when former is input;')
        WRITE (LUOUT,931)
        READ  (LUIN,'(A1)') ANS
        WRITE (LUOUT,2)
        WRITE (LUOUT,9)
        WRITE (LUOUT, 320) VERSIO,VRSION
  320   FORMAT (' <',15('- '),'Columns 1 - 80 ',16(' -'),'>'/
     +          1x,A15,F5.2/ 
     +          ' 000010*ZZ*VERTOBS NGS   NATIONAL GEODETIC SURVEY'/ 
     +          ' 000020*10*LXXX         1993020219930202MM4    12MD',
     +          '    EIBNGS           19930202'/
     +          ' 000030*11* TEST OF VERTCON WITH 30 RECORDS'/
     +          ' 000040*15*NAVD88'/
     +          ' 000050*30*0001 BOSSLER RM 1',11x,'KM12.34   MT',
     +          '12.3456   AB12394010200803040'/)
        WRITE (LUOUT,61) VRSION
   61 FORMAT(' The output records are the same as input records - '/
     +   ' except : 1) VERTCON Version',F5.2,' (i.e., first line) was',
     +   ' added'/10x,'2) datum code in *15* record is reset to new',
     +   ' datum'/10x,'3) field elevation is converted to the new',
     +   ' datum'/10x,'  [unit of field elevation (meter/feet) is',
     +   ' retained;'/10x,'   other than *15* and *30* records are',
     +   ' copied unaltered]'/)
      ELSEIF( ITYPE.EQ.4 )THEN
C
C       FOR INPUT FILE FORMAT ITYPE = 4
C       SAME OUTPUT
C
        WRITE (LUOUT,2)
        WRITE (LUOUT, 311)
  311   FORMAT (' NGS Internal Bench Mark format-'//
     +   ' Columns   Type',12x,'Contents'/25(' -')/
     +   '  1 -  5   99999           mark number'/
     +   '  6 - 13   99999999        data base id. (UID)'/
     +   ' 14 - 19   char(6)         archive ref. no. (ACRN)'/
     +   ' 20 - 49   char(30)        designation'/
     +   ' 50 - 55   999999          latitude  (ddmmss)'/
     +   '      56   char(1)         latitude (N=north, S=south)'/
     +   ' 57 - 63   9999999         longitude (dddmmss)'/
     +   '      64   char(1)         longitude (E=east,  W=west)'/
     +   ' 65 - 74   9999.99999      approximate elevation (meter)'/
     +   '      75   char(1)         elev. code (A,B,C,G,F,P,S,X)'/
     +   ' 76 - 83   999.9999        surface gravity (gal.)'/
     +   ' 84 - 88   999.9           sigma gravity (mgal.)'/
     +   '      89   char(1)         status code (D,F,P)'/
     +   ' 90 - 94   99999           original mark no.'/
     +   ' 95 - 96   char(2)         datum code (88 = NAVD88;'/
     +   '                              blank or 29 = NGVD29)'/)
        WRITE (LUOUT,931)
        READ  (LUIN,'(A1)') ANS
        WRITE (LUOUT,2)
        WRITE (LUOUT,7)
    7   FORMAT (' NOTE :  VERTCON prompts the user ',
     +   ' for the the desired datum conversion; e.g.:'//2x,
     +   ' '' Do you want to convert NGVD29 heights ? Enter y/n :''',
     +   //2x,' When the response is ''y'' or ''Y'' VERTCON will :',
     +    /4x,' convert NGVD29 approx. elevations to NAVD88 for',
     +        ' records'/5x,' having blanks or ''29'' in cols 95-96',
     +   //2x,' When the response is ''n'' or ''N'' VERTCON will :',
     +    /4x,' convert NAVD88 approx. elevations to NGVD29 for',
     +        ' records'/5x,' having  ''88'' in cols 95-96'/)
        WRITE (LUOUT,931)
        READ  (LUIN,'(A1)') ANS
        WRITE (LUOUT,2)
        WRITE (LUOUT,9)
        WRITE (LUOUT, 321) VERSIO,VRSION
  321   FORMAT (' <',16('- '),'Columns 1 - 75 ',13(' -'),'>'/
     +       1x,A15,F5.2/ '  1234 1234567EV1234 BOSSLER RM 1',17x,
     +       '334455N1334455W 123.12345F'//55x,'<- - - 76 - 96 - - ->'/
     +       55x,'980.5555  1.0P 123429'/)
        WRITE (LUOUT,71) VRSION
   71 FORMAT(' The output records are the same as input records - '/
     +   ' except : 1) VERTCON Version',F5.2,' (i.e., first line) was',
     +   ' added'/10x,'2) datum code in cols. 95-96 is reset to new',
     +   ' datum'/10x,'3) approximate elevation is converted to new',
     +   ' datum'/)
      ENDIF
C
      WRITE (LUOUT,*) ' Do you want more information (Y/N)?'
      WRITE (LUOUT,*) ' (Default is Y)'
      READ  (LUIN,'(A1)') ANS
      IF( ANS.NE.'N' .AND. ANS.NE.'n' )THEN
        GOTO 9001
      ENDIF
C
      RETURN
C
C     Error message
C
 9940 WRITE (LUOUT,*) ' Gotta pick ''1'', ''2'' , ''3'' or ''4'' -',
     +                ' sorry try again.'
      GOTO 9001
      END

C      SCCSID[]= "@(#) getpt.f 1.1 - 99/10/20"

      SUBROUTINE GETPT (NCONV, VRSION,ITYPE, NAME, IDLA, IMLA, SLA, 
     +      IDLO, IMLO, SLO, XPT, YPT, EOF, NOPT)
C
C     Get the name, latitude, and longitude of a point 
C             either interactively
C          or from an input data file
C
      LOGICAL          EOF, NOPT
      CHARACTER*40     NAME
      CHARACTER*1      ANS
      INTEGER          ITYPE
      INTEGER          IDLA, IMLA, IDLO, IMLO
      INTEGER          NCONV
      REAL             SLA, SLO
      DOUBLE PRECISION XPT, YPT
C
      INTEGER          NQ_PGM 
      INTEGER          LUIN, LUOUT, NIN, NOUT
      INTEGER          LDUMP,NSPACE(2) 
      COMMON/INOUT/ NQ_PGM,LUIN,LUOUT,NOUT,NIN,LDUMP,NSPACE
C
      EOF  = .FALSE.
      NOPT = .FALSE.
C
      IF( ITYPE.EQ.0 )THEN
C
C       FOR INTERACTIVE USE - NO INPUT FILE
C
        IF( NCONV.GE.1 )THEN
          WRITE (LUOUT,*) ' '
          WRITE (LUOUT,*) ' Do you want to do another',
     +                    ' computation (Y/N)?'
          WRITE (LUOUT,*) ' (Default is Y)'
          READ  (LUIN,'(A1)') ANS
          IF( ANS.EQ.'n' .OR. ANS.EQ.'N' )THEN
            GOTO 9999
          ENDIF
        ENDIF
C
C       Get a point (X,Y) to compute
C
        CALL ASKPT (NCONV, NAME, IDLA, IMLA, SLA,
     +              IDLO, IMLO, SLO, XPT, YPT, EOF, NOPT)
C
        IF( NOPT )THEN
          GOTO 9000
        ENDIF
      ELSEIF( ITYPE.EQ.1 )THEN
C
C       Free format type 1
C
        CALL TYPE1 (NAME, IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +              XPT, YPT, EOF, NOPT)
        IF( NOPT )THEN
          STOP
        ENDIF
      ELSEIF( ITYPE.EQ.2 )THEN
C
C       Free format type 2
C
        CALL TYPE2 (NAME, IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +              XPT, YPT, EOF, NOPT)
        IF( NOPT )THEN
          STOP
        ENDIF
      ELSEIF( (ITYPE.EQ.3).OR.(ITYPE.EQ.4) )THEN
C
C       Files Format type 3 or 4
C
        CALL TYPE34 (NCONV,VRSION,ITYPE, NAME, IDLA, IMLA, SLA, 
     +     IDLO, IMLO, SLO, XPT, YPT, EOF, NOPT)
        IF( NOPT )THEN
          STOP
        ENDIF
      ENDIF
C
 9000 RETURN
C
C     End of file
C
 9999 EOF = .TRUE.
      RETURN
      END

C      SCCSID[]= "@(#) headr.f 1.1 - 99/10/20"

      SUBROUTINE HEADR (VRSION, NQ_ARG)
C
C     This subroutine prints the header information and the disclaimer
C
      LOGICAL          T1, T2, EFLAG 
      CHARACTER*3      NQ_ARG
      CHARACTER*1      ANS
C
      INTEGER          NQ_PGM 
      INTEGER          LUIN, LUOUT, NIN, NOUT
      INTEGER          LDUMP,NSPACE(2) 
      COMMON/INOUT/ NQ_PGM,LUIN,LUOUT,NOUT,NIN,LDUMP,NSPACE
C
C     ADDED CODE TO TEST FOR HGT OR WEB PROGRAMMING
C
      NQ_PGM = 0
      ANS    = ' '
C
      T1 = (NQ_ARG.EQ.'WEB' .OR. NQ_ARG.EQ.'web')
      T2 = (NQ_ARG.EQ.'OHT' .OR. NQ_ARG.EQ.'oht')
C
      IF( T1 .OR. T2 )THEN
        IF( T1 )THEN
          NQ_PGM = 2
        ELSE
          NQ_PGM = 1
          WRITE (LUOUT,*) '               =================== '
          WRITE (LUOUT,*) '                  OHT ... Entry    '
          WRITE (LUOUT,*) '               =================== '
          INQUIRE (FILE='vertcon.out', EXIST=EFLAG)
          IF( EFLAG )THEN
            OPEN  (NOUT,FILE='vertcon.out',STATUS='OLD')
            CLOSE (NOUT,STATUS='DELETE')
          ENDIF
        ENDIF
      ENDIF
C
      IF( NQ_PGM.EQ.0 )THEN
        WRITE (LUOUT,1)
        WRITE (LUOUT,2) VRSION
        WRITE (LUOUT,933)
        READ  (LUIN,'(A1)') ANS
        WRITE (LUOUT,930)
        WRITE (LUOUT,931)
        WRITE (LUOUT,932)
        WRITE (LUOUT,933)
        READ (LUIN,'(A1)') ANS
      ENDIF
C
    1 FORMAT( /,15x,
     + ' National Geodetic Survey Program VERTCON'/25x,
     + ' (VERTical CONversion)'//15x,
     + ' For use when needing to convert between:'/10x,
     + ' National Geodetic Vertical Datum of 1929 (NGVD 29)'/30x,
     + ' and'/,12x,' North American Vertical Datum of 1988',
     + ' (NAVD 88)'/)
    2 FORMAT( /24x,'Dennis G. Milbert, Ph.D.'/
     +   27x,'David B. Zilkoski'//
     +   20x,' (Version 2.00)',' September 1994'/
     +   20x,' (Version',f5.2,')',' September 2003'/)
  933 FORMAT(23x,'(Hit RETURN to continue)')
C
  930 FORMAT(//31x'DISCLAIMER', //,
     + ' This program and supporting information is furnished by',
     + ' the government of', /,
     + ' the United States of America, and is accepted/used by the',
     + ' recipient with', /,
     + ' the understanding that the U. S. government makes no',
     + ' warranties, express or', /,
     + ' implied, concerning the accuracy, completeness, reliability,',
     + ' or suitability', /,
     + ' of this program, of its constituent parts, or of any',
     + ' supporting data.')
C
  931 FORMAT(/' The',
     + ' government of the United States of America shall be',
     + ' under no liability', /,
     + ' whatsoever resulting from any use of this program.',
     + '  This program should', /,
     + ' not be relied upon as the sole basis for solving a problem',
     + ' whose incorrect', /,
     + ' solution could result in injury to person or property.')
C
  932 FORMAT( /,
     + ' This program is the property of the government of the',
     + ' United States of', /,
     + ' America. Therefore, the recipient further agrees not to',
     + ' assert proprietary', /,
     + ' rights therein and not to represent this program to anyone as',
     + ' being other', /,
     + ' than a government program.', /)
C
      RETURN
      END

C      SCCSID[]= "@(#) icard.f 1.1 - 99/10/20"

      INTEGER FUNCTION ICARD (CHLINE, LENG, IERR)
C
C     Read an integer from a line of card image.
C     LENG is the length of the card
C     blanks are the delimiters of the integer
C
      CHARACTER*80    CHLINE
      INTEGER         I, IERR, ILENG, IVAR
      INTEGER         J
      INTEGER         LENG
C
      IERR = 0
C
C     Find first non-blank character
C     DO WHILE line character is bland, 
C     I is first non-blank character
C
      I = 1
   10 IF( CHLINE(I:I).EQ.' ' .OR. CHLINE(I:I).EQ.',' )THEN
        I = I + 1
C
C       Check for totally blank card
C
        IF( I.GE.LENG )THEN
          ICARD = 0
          LENG = 0
          RETURN
        ENDIF
        GOTO 10
      ENDIF
C
C     Find first blank character (or end of line)
C     DO WHILE line character is not a blank
C 
      J = I + 1
   20 IF( CHLINE(J:J).NE.' ' .AND. CHLINE(J:J).NE.',' )THEN
        J = J + 1
C
C       Check for totally filed card
C
        IF( J.GT.LENG )THEN
          GOTO 40
        ENDIF
        GOTO 20
      ENDIF
C
C     J is now 1 more than the position of 
C     the last non-blank character
C
   40 J = J - 1
C
C     ILENG is the length of the integer string, 
C     it cannot be greater than 13 characters
C
      ILENG = J - I + 1
      IF( ILENG.GT.13 )THEN
        STOP 'ICARD'
      ENDIF
C
C     Read the integer variable from the line, and 
C     set the return VAR to it
C
      READ (CHLINE(I:J), 55, ERR=9999) IVAR
   55 FORMAT (I13)
      ICARD = IVAR
C
C     Now reset the values for LENG and 
C     CHLINE to the rest of the card
C
      CHLINE(1:LENG) = CHLINE((J+1):LENG)
      LENG = LENG - J
      RETURN
C
C     Read error
C
 9999 IERR = 1
      RETURN
      END

C      SCCSID[]= "@(#) initl.f 1.1 - 99/10/20"

      SUBROUTINE INITL ( SCREEN, PAGE, NAME, IPAGE, ITYPE,
     +                   NQ_RUN, MYGRID )
C 
      LOGICAL          PAGE, SCREEN
      CHARACTER*80     B80
      CHARACTER*80     CARD
      CHARACTER*40     NAME
      CHARACTER*21     D_GRID, MYGRID
      CHARACTER*20     B20, B32
      INTEGER          IPAGE, ITYPE
      INTEGER          NQ_RUN
C
      PARAMETER (B20='                    ',B80=B20//B20//B20//B20)
      PARAMETER (B32='                                ')
C
      CHARACTER*96     B96
      COMMON/CURNT/ B96 
C
      PARAMETER ( MAXAREA=10 )
      CHARACTER*32  FILES(MAXAREA)
      COMMON/GRIDS/ FILES        
C
      INTEGER          NQ_PGM 
      INTEGER          LUIN, LUOUT, NIN, NOUT
      INTEGER          LDUMP,NSPACE(2) 
      COMMON/INOUT/ NQ_PGM,LUIN,LUOUT,NOUT,NIN,LDUMP,NSPACE
C
      PARAMETER ( N1SPACE=6*MAXAREA, N2SPACE=2*MAXAREA )
      REAL*4           SPACE1(N1SPACE), SPACE2(N2SPACE)
      REAL*4           MARGIN(MAXAREA)
      COMMON/GSTUFF/ SPACE1,MARGIN,SPACE2
C
      EQUIVALENCE(CARD,B96)
C
C     Initialize card variable in common CURNT to blank
C
      CARD = B80
C
C     Set the logical units for input/output common INOUT
C
      LUIN  = 5
      LUOUT = 6
      NOUT  = 101
      NIN   = 102
      LDUMP = LUOUT
C
C     INITIALIZE  Defaults:
C
C     SCREEN = .TRUE.  => send results to screen
C     PAGE   = .FALSE. => don't start a new page in the output file
C     NAME   = '     ' => no station name
C     IPAGE  = 0       => current output file page number is 0
C     ITYPE  = 0       => interactive input of points
C
      SCREEN = .TRUE.
      PAGE   = .FALSE.
      NAME   = '        '
      IPAGE  = 0
      ITYPE  = 0
C
      MARGIN(1) = 5.0
      MARGIN(2) = 5.0
      MARGIN(3) = 0.0
      D_GRID  = MYGRID
C
      IF( NQ_RUN.EQ.1 )THEN
        FILES(1)= D_GRID//'vertconw.94'
        FILES(2)= D_GRID//'vertconc.94'
        FILES(3)= D_GRID//'vertcone.94'
      ELSE
        FILES(1)= 'vertconw.94'
        FILES(2)= 'vertconc.94'
        FILES(3)= 'vertcone.94'
      ENDIF
C
      DO 90 N=4,MAXAREA
        FILES(N)  = B32
        MARGIN(N) = 0.0
   90 CONTINUE
      RETURN
      END
C "@(#) interp.f 1.2 - 00/05/09"

      subroutine interp (gla,glo,val,ios)
C
C     bilinear interpolation from a standard grid file
C
      implicit double precision(a-h,o-z)
      logical      nogo
      character*56 ident
      character*32 blank
      character*8  pgm
      integer      ios, irow
      integer      jcol
      integer      lin, lgh
      integer      n, narea
      real*4       skip
C
      parameter(narea=10,lgh=461)
      parameter(blank='                                ')
C
      character*32 files( narea )
      common/grids/ files
C
      integer      nz(    narea ), nla(   narea ), nlo(   narea )
      real*4       glamn( narea ), dgla(  narea ), glamx( narea ),
     .             glomn( narea ), dglo(  narea ), glomx( narea ),
     .             margin(narea )
      common/gstuff/ glamn,dgla,glamx,glomn,dglo,glomx,margin,nla,nlo
C
C     do the interpolation, check for clipped points
C
      ios =  0
      lin = 90
C
      do 10 n=1,narea
        lin=lin+1
c
        if( gla.ge.glamn(n).and.gla.le.glamx(n) .and.
     *      glo.ge.glomn(n).and.glo.le.(glomx(n)-margin(n)) )then
c
          call fgrid(lin,glo,gla,glomn(n),glamn(n),
     .         dglo(n),dgla(n),nlo(n),xgrid,ygrid,irow,jcol,
     .         tee1,tee2,tee3,tee4,ios)
c
          if( ios.eq.0 )then
            call coeff(tee1,tee2,tee3,tee4,ay,bee,cee,dee)
            call surf(xgrid,ygrid,val,ay,bee,cee,dee,irow,jcol)
          endif
c
          return
        endif
   10 continue
c
      ios = 999
      return
C ==================================================
C
      entry loadgrd(nogo)
C
      lin = 90
      nogo=.true.
C
C     open available grid files 
C
      do 20 n=1,narea
        if( files(n).ne.blank )then
          lin =lin+1
          nogo=.false.
          open(lin,file=files(n),status='old',form='unformatted',
     &                  access='direct',recl=1848)
          read(lin,rec=1) ident,pgm,nlo(n),nla(n),nz(n),
     &                  glomn(n),dglo(n),glamn(n),dgla(n),skip
C
C         write(6,'(i4,4x,a32/1x,a56,a8/3i5,5f10.5)')
C    .              n,files(n),ident,pgm,nlo(n),nla(n),nz(n),
C    .              glomn(n),dglo(n),glamn(n),dgla(n),margin(n)
C
          glamx(n)=glamn(n)+dgla(n)*(nla(n)-1)
          glomn(n)=360.0+glomn(n)
          glomx(n)=glomn(n)+dglo(n)*(nlo(n)-1)
          if(nlo(n).gt.lgh)then
            stop 12345
          endif
          if(nla(n).lt.3.or.nlo(n).lt.3)then
            stop 54321
          endif
        else
          margin(n)=0.0
          glamn(n)=0.0
          glomn(n)=0.0
          glamx(n)=0.0
          glomx(n)=0.0
        endif
   20 continue
      return
C ==================================================
      entry closegrd
C
      lin = 90
      do 30 n=1,narea
        if( files(n).ne.blank )then
          lin=lin+1
          close(lin,status='keep')
        endif
   30 continue
      return
      end

C      SCCSID[]= "@(#) iparms.f 1.1 - 99/10/20"

      SUBROUTINE IPARMS (ITYPE, SCREEN, VRSION)
C
C     This subroutine interactively requests for information
C
      LOGICAL          SCREEN, EFLAG
      CHARACTER*80     INFILE, OFILE
      CHARACTER*80     B80
      CHARACTER*20     B20
      CHARACTER*1      ANS
      INTEGER          IOS, IFLAG1, IFLAG2, ITYPE
      INTEGER          N1, N2
C
      PARAMETER (B20='                    ', B80=B20//B20//B20//B20)
C
      INTEGER          NQ_PGM 
      INTEGER          LUIN, LUOUT, NIN, NOUT
      INTEGER          LDUMP 
      LOGICAL          TONGVD, CODE15
      COMMON/INOUT/ NQ_PGM,LUIN,LUOUT,NOUT,NIN,LDUMP,TONGVD,CODE15
C
      DATA IFLAG1 /1/, IFLAG2 /2/
C
C     GET THE NAME FOR THE INPUT FILE
C
   11 FORMAT(/'  VERTCON  processing can be done :',//
     + '  (1) Interactively  - OR -',/
     + '  (2) By entering INPUT FILES in either of 4 formats'/)
  111 FORMAT('  VERTCON provides numerous ''defaults'' ',
     +     'while prompting for information;',/4x,
     +     ' if you accept an indicated default just press',
     +     ' <RETURN> !! '/)
C
      IF( NQ_PGM.EQ.2 )THEN
        ANS = 'N'
      ELSE
        WRITE (LUOUT,11) 
        WRITE (LUOUT,111) 
        WRITE (LUOUT,*) ' '
        WRITE (LUOUT,*) ' Do you have an input data file (Y/N)?'
        WRITE (LUOUT,*) ' (Default is N)'
        READ  (LUIN,'(A1)') ANS
      ENDIF
C
   12 FORMAT(/30(' -')//,15x,'NON-INTERACTIVE PROCESSING'/)
C
      IF( ANS.EQ.'Y' .OR. ANS.EQ.'y' )THEN
        WRITE (LUOUT,12)
C
C       CHOOSE THE INPUT FILE FORMAT
C       CALL NBLANK (INFILE, IFLAG2, N2)
C
 9001   WRITE (LUOUT,*) ' '
        WRITE (LUOUT,*) ' What is your file format?'
        WRITE (LUOUT,*) '  0) Help - File format information'
        WRITE (LUOUT,*) '  1) Free Format Type 1'
        WRITE (LUOUT,*) '  2) Free Format Type 2'
        WRITE (LUOUT,*) '  3) NGS Blue Book format Type 3'
        WRITE (LUOUT,*) '  4) NGS Internal Bench Mark File Type 4'
c
        READ (LUIN,'(A1)') ANS
        IF( ANS.EQ.' ' )THEN
          ITYPE = 5
        ELSE
          READ (ANS,13,ERR=9940,IOSTAT=IOS) ITYPE
   13     FORMAT (I1)
        ENDIF
C
        IF( ITYPE.GT.4 .OR. ITYPE.LT.0 )THEN
          GOTO 9940
        ENDIF
C
C       Get help information
C
        IF( ITYPE.EQ.0 )THEN  
          CALL FHELP(VRSION)
          GOTO 9001
        ENDIF
C
    1   WRITE (LUOUT,*) ' What is the name of the input data file?'
        WRITE (LUOUT,*) ' The default name is ''vertcon.inp''.'
        READ  (LUIN,'(A80)') INFILE
C
        IF( INFILE.EQ.B80 )THEN
          INFILE = 'vertcon.inp'
        ENDIF
C
        OPEN (NIN,FILE=INFILE,FORM='FORMATTED',STATUS='OLD',
     +        ACCESS='SEQUENTIAL',ERR=9920,IOSTAT=IOS)
        WRITE (LUOUT,101) INFILE
C
  101 FORMAT(' Opened - ',A80)
  112 FORMAT(' Do you want to convert NGVD29 heights to NAVD88 ?',
     +     ' Enter y/n :'/
     +    ' [if answer is no -  NAVD88 heights are converted',
     +    ' to NGVD29'/ '  provided code 88 is in col. 95-96]')
C
        TONGVD=.FALSE.
        CODE15=.FALSE.
C
C       GET TRANSFORMATION INFO. FOR TYPE 4 CHOICE
C
        IF( ITYPE.EQ.4 )THEN
          WRITE(LUOUT,112) 
          READ (LUIN,'(A1)') ANS
          IF( (ANS.EQ.'y') .OR. (ANS.EQ.'Y') )THEN
            TONGVD=.TRUE.
          ENDIF
        ENDIF
C
C       CHECK FOR A SCREEN OUTPUT ALSO
C
   14 FORMAT(' Would you want to look at VERTCON predictions',
     +    ' [listed in Type 1 OUTPUT format!]'/,9x,
     +    ' in addition to generating an output file',
     +    ' (Y/N)?'/,' (Default is N)')
C
        WRITE (LUOUT,*) ' '
        WRITE (LUOUT,14) 
        READ  (LUIN,'(A1)') ANS
C
   15 FORMAT(' Do you want VERTCON predictions',
     +     ' sent to your screen (Y/N)?'/ 
     +     '   [if not, they will go to temp.out scratch file]'/
     +     3x,' (Default is N)')
C
        IF( (ANS.EQ.'y') .OR. (ANS.EQ.'Y') )THEN
C
C         PROVIDE SCREEN DUMP TO TEMPORARY FILE FOR ITYPE > 1
C
          IF( ITYPE.EQ.0 )THEN
            GOTO 2
          ELSE
            WRITE (LUOUT,15)
            READ  (LUIN,'(A1)') ANS
            IF( (ANS.NE.'y') .AND. (ANS.NE.'Y') )THEN
              LDUMP=103
              OPEN (LDUMP,FILE='temp.out',FORM='FORMATTED',
     +              STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
              WRITE(LUOUT,*) ' Opened - temp.out'
              GOTO 2
            ELSE
              GOTO 2
            ENDIF
          ENDIF
        ENDIF
C
        SCREEN = .FALSE.
        GOTO 2
C
C       Error message
C
 9920   CALL NBLANK (INFILE, IFLAG1, N1)
        CALL NBLANK (INFILE, IFLAG2, N2)
        WRITE (LUOUT,9915) IOS, INFILE(N1:N2)
        GOTO 1
C
 9940   WRITE (LUOUT,*) ' Gotta pick ''1'' or ''2'' or ''3'' or "4" ',
     +                  ' sorry try again.'
        GOTO 9001
      ENDIF
C
C     GET THE NAME FOR THE OUTPUT FILE
C
    2 CONTINUE
      IF( NQ_PGM.LE.1 )THEN
        WRITE (LUOUT,*) ' '
        WRITE (LUOUT,*) ' Enter the name of file which will',
     +                ' contain VERTCON output data :'
        WRITE (LUOUT,*) ' '
        WRITE (LUOUT,*) ' ( if default file name',
     +                ' ''vertcon.out'' is o.k. press <RETURN> )'
        WRITE (LUOUT,*) ' '
C
        READ (LUIN,'(A80)') OFILE
C
        IF( OFILE.EQ.B80 )THEN
          OFILE = 'vertcon.out'
        ENDIF
C
        INQUIRE (FILE=OFILE, EXIST=EFLAG)
        IF( EFLAG )THEN
          CALL NBLANK (OFILE, IFLAG1, N1)
          CALL NBLANK (OFILE, IFLAG2, N2)
          WRITE (LUOUT,*) ' The file ''', OFILE(N1:N2), ''''
          WRITE (LUOUT,*) ' already exists.  Do you want to overwrite',
     +                                 ' it (Y/N)?'
          WRITE (LUOUT,*) ' (Default is Y)'
          READ (LUIN, '(A1)') ANS
          IF( ANS.EQ.'n' .OR. ANS.EQ.'N' )THEN
            GOTO 2
          ENDIF
        ENDIF
C
        OPEN (NOUT,FILE=OFILE,FORM='FORMATTED',STATUS='UNKNOWN',
     +      ACCESS='SEQUENTIAL',ERR=9910,IOSTAT=IOS)
      ELSE
C
C       DO NOTHING ... SET THE DEFAULT TO BLANK
C
      ENDIF
C
  102 FORMAT(' Opened - ',A60/30(' -')/)
  103 FORMAT(' Opened - Version( OHT )'/)
  105 FORMAT(' Opened - Version Normal'/)
C
      IF(     NQ_PGM.EQ.1 )THEN
        WRITE(LUOUT,103) 
        WRITE(LUOUT,102) OFILE
      ELSEIF( NQ_PGM.EQ.2 )THEN
C
C       DO NOTHING
C       WRITE(LUOUT,104)
C 104   FORMAT(' Opened - Version( WEB )'/)
C
      ELSE
        WRITE(LUOUT,105)
        WRITE(LUOUT,102) OFILE
      ENDIF
C
      RETURN
C
C     Error message
C
 9910 CONTINUE
      CALL NBLANK (OFILE, IFLAG1, N1)
      CALL NBLANK (OFILE, IFLAG2, N2)
      WRITE (LUOUT,9915) IOS, OFILE(N1:N2)
 9915 FORMAT(' ERROR (', I5, ') - The operating system could not',
     +       ' open the file ', /, '''', A, '''', /, ' Try again'
     +       ' -  may enter <ctrl-c> to terminate !'/)
      GOTO 2
      END

C      SCCSID[]= "@(#) main.f 1.1 - 99/10/20"

      program vertcon 
***********************************************************************
*                            Release 2.10                             *
* PURPOSE:    CALCULATE THE DATUM CONVERSION CORRECTION TO NGVD29     *
*             ORTHOMETRIC HEIGHT IN ORDER TO OBTAIN NAVD88 HEIGHT.    *
*             THE INPUT REQUIRES LATITUDE AND LONGITUDE VALUES;       *
*                                                                     *
*             [THE GEODETIC DATUM TO WHICH THE POSITION IS REFERENCED *
*             (I.E., NAD27 OR NAD83) IS GENERALLY IRRELEVANT DUE TO   *
*             THE IMPRECISION OF SCALED NAD27 POSITIONS;  HOWEVER,    *
*             SINCE NUMEROUS BENCH MARKS ARE ALSO HORIZONTAL NETWORK  *
*             STATIONS, ALL POSITIONS WILL BE CONVERTED TO NAD83      *
*             POSITIONS IN THE NEXT RELEASE]                          *
*                                                                     *
*             THE COMPUTATION IS PERFORMED AS AN INTERPOLATION USING  *
*             A BILINEAR INTERPOLATOR AMONG FOUR GRID POINTS.         *
*                                                                     *
*             THE PROGRAM REQUIRES THAT THE USER SPECIFY:             *
*             1)  THE NAME OF AN INPUT FILE (IF AVAILABLE).           *
*             2)  THE NAME OF AN OUTPUT FILE                          *
*                                                                     *
*             THIS PROGRAM ALLOWS FOR :                               *
*               GENERIC FILE FORMATS -  SEE SUBROUTINE TYPE1 OR TYPE2 *
*                                 OR                                  *
*               NGS INTERNAL BENCH MARK FILE FORMAT - SEE TYPE3       *
*                                                                     *
*            [THE CODE CAN BE EASILY MODIFIED TO ACCOMMODATE CUSTOM   *
*             FILE SPECIFICATIONS (TYPE..) BY MODIFYING SUBROUTINES:  *
*             GETPT, IPARMS, WRTPT, AND (OPTIONALLY) FHELP.]          *
*                                                                     *
*                                                                     *
* VERSION CODE:  2.00   1994 August      RUDOLF J. FURY               *
* VERSION CODE:  2.10   2003 September   ROBERT W. SAFFORD            *
*                                                                     *
* INFORMATION CONCERNING THE 'VERTCON' SYSTEM MAY BE OBTAINED FROM:   *
*                  INFORMATION CENTER, NOAA/NOS/NGS                   *
*                  SILVER SPRING, MARYLAND 20910-3282                 *
***********************************************************************
*                                                                     *
*                  DISCLAIMER                                         *
*                                                                     *
*   THIS PROGRAM AND SUPPORTING INFORMATION IS FURNISHED BY THE       *
* GOVERNMENT OF THE UNITED STATES OF AMERICA, AND IS ACCEPTED AND     *
* USED BY THE RECIPIENT WITH THE UNDERSTANDING THAT THE UNITED STATES *
* GOVERNMENT MAKES NO WARRANTIES, EXPRESS OR IMPLIED, CONCERNING THE  *
* ACCURACY, COMPLETENESS, RELIABILITY, OR SUITABILITY OF THIS         *
* PROGRAM, OF ITS CONSTITUENT PARTS, OR OF ANY SUPPORTING DATA.       *
*                                                                     *
*   THE GOVERNMENT OF THE UNITED STATES OF AMERICA SHALL BE UNDER NO  *
* LIABILITY WHATSOEVER RESULTING FROM ANY USE OF THIS PROGRAM.  THIS  *
* PROGRAM SHOULD NOT BE RELIED UPON AS THE SOLE BASIS FOR SOLVING A   *
* PROBLEM WHOSE INCORRECT SOLUTION COULD RESULT IN INJURY TO PERSON   *
* OR PROPERTY.                                                        *
*                                                                     *
*   THIS PROGRAM IS PROPERTY OF THE GOVERNMENT OF THE UNITED STATES   *
* OF AMERICA.  THEREFORE, THE RECIPIENT FURTHER AGREES NOT TO ASSERT  *
* PROPRIETARY RIGHTS THEREIN AND NOT TO REPRESENT THIS PROGRAM TO     *
* ANYONE AS BEING OTHER THAN A GOVERNMENT PROGRAM.                    *
*                                                                     *
***********************************************************************
*
c
      LOGICAL        PAGE, SCREEN, NOGO
      CHARACTER*60   NAME
      CHARACTER*21   MYGRID
      CHARACTER*3    ARGV, NQ_ARG
      INTEGER        I, IARGC
      INTEGER        NQ, NN_ARG, NQ_RUN
      REAL           VRSION
c
      PARAMETER ( VRSION = 2.10E0 )
      PARAMETER ( NQ_RUN = 0 )
      PARAMETER ( MYGRID = '/ngslib/data/Vertcon/' )
c
      SAVE           NCONV, ITYPE, PAGE, SCREEN
c
      INTEGER        NQ_PGM
      INTEGER        LUIN,  LUOUT,  NIN,  NOUT
      INTEGER        LDUMP, NSPACE(2)
      COMMON/INOUT/ NQ_PGM,LUIN,LUOUT,NOUT,NIN,LDUMP,NSPACE
C
C**************************************************************
C
C     TO LOCATE THE "GRID" FILES CORRECTLY YOU MUST 
C     SET ... PARAMETER (NQ_RUN = ??) ... AND 
C     SET ... PARAMETER (MYGRID = ??) ... THEN
C     RECOMPILE THIS CODE FOR THE SYSTEM YOU ARE USING !!!!
C
C     NQ_RUN = 0    "GRIDS" FOUND AND READ FROM YOUR LOCAL DIRECTORY
C     NQ_RUN = 1    "GRIDS" READ ONLY FROM NGSLIB/DATA/VERTCON 
C
C*************************************************************
C
C     READ THE COMMAND_LINE PARAMETER(S)
C
      NN_ARG = IARGC()
      NQ_ARG = '   '
C
      IF( NN_ARG.GE.1 )THEN
        DO 10 I = 1,NN_ARG
          CALL GETARG( I, ARGV )
          IF( I.EQ.1 )THEN
            NQ_ARG = ARGV
          ENDIF
   10   CONTINUE
      ENDIF
C
C     INITIALIZE VARIABLES
C
      CALL INITL ( SCREEN, PAGE, NAME, IPAGE, ITYPE,
     +             NQ_RUN, MYGRID )
C
      CALL HEADR (VRSION,NQ_ARG)
C
C     LOAD 'VERTCON' GRIDS
C
      call loadgrd (nogo)

      IF( .not.nogo )THEN
C
C       REQUEST FOR THE NEEDED VARIABLE VALUES FROM THE USER
C
        CALL IPARMS (ITYPE, SCREEN, VRSION)
        CALL MLOOP (NCONV, IPAGE, ITYPE, VRSION, PAGE, SCREEN, NAME)
C
        IF( ITYPE.GT.0 )THEN 
          CLOSE(NIN, IOSTAT=IOS,ERR=1001,STATUS='KEEP')
          CLOSE(NOUT,IOSTAT=IOS,ERR=1002,STATUS='KEEP')
          call closegrd
          IF( LDUMP.NE.LUOUT )THEN
            CLOSE(LDUMP,IOSTAT=IOS,ERR=1003,STATUS='KEEP')
          ENDIF
        ENDIF
      ELSE
        IF( NQ_PGM.EQ.2 )THEN
          NQ = 6
          WRITE(LUOUT,162) NQ
        ENDIF
      ENDIF

      IF( NQ_PGM.EQ.2 )THEN
C       DO NOTHING 
        GOTO 1006
      ELSE
        write(LUOUT,*) 'Normal End'
        stop
      ENDIF
C
 1001 CONTINUE
 1104 format(' Input  File i/o err. =',i5)
  162 format('ERROR',I6)
C
      IF( NQ_PGM.EQ.2 )THEN
        NQ = 5
        WRITE(LUOUT,162) NQ
        GOTO 1006
      ELSE
        write(LUOUT,1104) ios
        stop
      ENDIF
C
 1002 CONTINUE
 1105 format(' Output File i/o err. =',i5)
      IF( NQ_PGM.EQ.2 )THEN
        NQ = 7
        WRITE(LUOUT,162) NQ
        GOTO 1006
      ELSE
        write(LUOUT,1105) ios
        stop
      ENDIF
C
 1003 CONTINUE
      write(LUOUT,1106) ios
 1106 format(' Dump   File i/o err. =',i5)
      IF( NQ_PGM.EQ.2 )THEN
        NQ = 3
        WRITE(LUOUT,162) NQ
        GOTO 1006
      ELSE
        write(LUOUT,1105) ios
        stop
      ENDIF
C
 1006 CONTINUE
C     STOP
      END                                     

C      SCCSID[]= "@(#) mloop.f 1.1 - 99/10/20"

      SUBROUTINE MLOOP (NCONV, IPAGE, ITYPE, VRSION,
     +                  PAGE, SCREEN, NAME)
C
C     THIS SUBROUTINE LOOPS THROUGH THE INPUT DATA (EITHER AN INPUT 
C     DATA FILE OR INTERACTIVELY), CALCULATES THE CORRECTION VALUES,    
C     PRINTS THE RESULTS TO THE OUTPUT FILE AND/OR THE SCREEN           
C
      LOGICAL           PAGE, SCREEN, NOPT, EOF
      CHARACTER*40      NAME
      INTEGER           NQ, NCONV, NPOINT       
      INTEGER           IDLA, IMLA, IDLO, IMLO, IPAGE, ITYPE
      REAL              GHT
      REAL              SLA, SLO
      DOUBLE PRECISION  dhpred,xeast
      DOUBLE PRECISION  XPT,YPT
      SAVE IOS
C
      INTEGER          NQ_PGM 
      INTEGER          LUIN, LUOUT, NIN, NOUT
      INTEGER          LDUMP,NSPACE(2) 
      COMMON/INOUT/ NQ_PGM,LUIN,LUOUT,NOUT,NIN,LDUMP,NSPACE
C
C     set defaults for those variables not used by every format type
C     BEGIN THE COMPUTATION LOOP FOR EACH INTERPOLATION
C     DO UNTIL END OF FILE OR NO MORE COMPUTATIONS REQUESTED
C
      NCONV  = 0
      NPOINT = 0
C
  160 CONTINUE
      PAGE = .FALSE.
C
C     GET THE NAME AND LOCATION OF ANOTHER POINT
C
      CALL GETPT (NCONV, VRSION, ITYPE, NAME, IDLA, IMLA, SLA, IDLO,
     +              IMLO, SLO, XPT, YPT, EOF, NOPT)
      IF( NOPT )THEN
        IF( NQ_PGM.EQ.2 )THEN
          EOF = .TRUE.
          GOTO 9998
        ELSE
          GOTO 160
        ENDIF
      ENDIF
C
      IF( EOF )THEN
        GOTO 9999
      ENDIF
C 
C     DO THE INTERPOLATION
C     LOOP ONCE FOR EACH COMPUTATION
C
      xeast = 360.0d0-xpt
      call interp (ypt,xeast,dhpred,ios)
      NCONV = NCONV+1
C
  161 format(' Position out of bounds :',2f10.5)
  162 FORMAT('ERROR',I6)
C
      IF( IOS.NE.0 )THEN
        IF( NQ_PGM.EQ.2 )THEN
          NQ = 4
          WRITE(LUOUT,162) NQ
          EOF = .TRUE.
          GOTO 9998
        ELSE
          WRITE(LUOUT,161) ypt,xpt
          GOTO 160
        ENDIF
      ENDIF
C
      GHT    = SNGL( dhpred*0.001d0 )
      GHT    = ROUND(GHT)
      NPOINT = NPOINT+1
C
C     WRITE TO OUTPUT FILE AND SCREEN
C
C     ITYPE
C           SCREEN
C       0   INTERACTIVE and OHT or WEB
C       1   TYPE 1
C       2   TYPE 2
C       3   TYPE 3
C       4   TYPE 4
C
      CALL WRTPT (ITYPE, NCONV, NPOINT, VRSION, NAME,
     +             IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +              GHT, IPAGE, PAGE, SCREEN)
C
C     START THE LOOP AGAIN
C
 9998 CONTINUE
      IF( NQ_PGM.EQ.2 )THEN
        NPOINT = 0
        NCONV  = 0
        GOTO 9999
      ELSE
        GOTO 160
      ENDIF
C
 9999 RETURN
      END

C      SCCSID[]= "@(#) nblank.f 1.1 - 99/10/20"

      SUBROUTINE NBLANK (A, IFLAG, NBLK)
C
C     Return position of last non-blank in string (IFLAG = 2)
C     or position of first non-blank in string (IFLAG = 1)
C
      INTEGER IFLAG, NBLK, LENG, IBLK
      CHARACTER*(*) A
C
      LENG = LEN(A)
      IF( IFLAG.EQ.2 )THEN
        DO 1 IBLK = LENG, 1, -1
          IF( A(IBLK:IBLK).NE.' ' )THEN
            NBLK = IBLK
            RETURN
          ENDIF
    1   CONTINUE
      ELSEIF( IFLAG.EQ.1 )THEN
        DO 2 IBLK = 1, LENG, +1
          IF( A(IBLK:IBLK).NE.' ' )THEN
            NBLK = IBLK
            RETURN
          ENDIF
    2   CONTINUE
      ENDIF
C
C     String contains all blanks
C
      NBLK = 0
      RETURN
      END

C      SCCSID[]= "@(#) print.f 1.1 - 99/10/20"

      SUBROUTINE PRINT (LU, NPOINT, NAME, VRSION, 
     +    IDLA, IMLA, SLA, IDLO, IMLO, SLO, GHT, IPAGE, PAGE)
c
c This subroutine prints out the actual transformation results using
c a pretty format - not the same as the input file format (if there
c is one).  This subroutine is used by type-1 format input and
c interactive input.
c
      LOGICAL          PAGE
      CHARACTER*40     NAME
      CHARACTER*6      UNITS
      CHARACTER*2      NAVD,NGVD
      CHARACTER*1      FF
      INTEGER          IPAGE, IDLA, IMLA, IDLO, IMLO
      INTEGER          LU
      REAL             VRSION
      REAL             SLA, SLO
      REAL             GHT, HT
      REAL*8           GHTX, MT2FT, OHT
C
      PARAMETER ( MT2FT = 39.37D0/12.00D0 )
C
      INTEGER          NQ_PGM 
      INTEGER          LUIN, LUOUT, NIN, NOUT
      INTEGER          LDUMP,NSPACE
      LOGICAL          TONGVD 
      COMMON/INOUT/ NQ_PGM,LUIN,LUOUT,NOUT,NIN,LDUMP,TONGVD,NSPACE
C
      CHARACTER*2      CODE, DATUM
      REAL*8           HGT
      COMMON/WEBCCC/HGT,CODE,DATUM
C
      SAVE NAVD, NGVD
      DATA NAVD/'88'/,NGVD/'29'/
C
      FF = CHAR(12)
c
c     FIRST PAGE HEADING
c
      IF( NQ_PGM.EQ.0 )THEN
        IF( NPOINT.EQ.1 )THEN
          WRITE (LU,10) IPAGE
          WRITE (LU,5)
          WRITE (LU,7) VRSION
          WRITE (LU,8)
        ENDIF
c
        IF( PAGE )THEN
          IF( IPAGE.GT.1 )THEN
            WRITE (LU,'(A1)') FF
            WRITE (LU,10) IPAGE
          ENDIF
        ENDIF
      ENDIF
C
   10 FORMAT (70(' '), 'Page ', I4)
    5 FORMAT (10X, ' VERTical CONversion (VERTCON) Transformation',
     +             ' Program'/23x,'Between NGVD 29 and NAVD 88')
    7 FORMAT (30X, ' Version',F5.2)
    8 FORMAT ( / 1X, 79('=') )
C
  922 FORMAT (/,2X, 'Station Name:  ', A40)
  921 FORMAT (/,2x, 'Station sequence #: ', I4)
  900 FORMAT (5X, 'Latitude', 17X, 'Longitude', 4X, 'NAVD 88 - NGVD 29',
     +             ' (meters)')
  923 FORMAT (2X, I2, 1X, I2.2, F9.5, 10X, I3, 1X, I2.2, F9.5,
     +        5X, F9.3)
C
      IF( NQ_PGM.EQ.0 )THEN
        IF( NAME(1:8).NE.'        ' )THEN
          WRITE (LU,922) NAME
        ELSE
          WRITE (LU,921) NPOINT
        ENDIF
        WRITE (LU,900)
        WRITE (LU,923) IDLA, IMLA, SLA, IDLO, IMLO, SLO, GHT
      ENDIF 
C
      IF( NQ_PGM.NE.0 )THEN
        GHTX = DBLE(GHT)
        OHT  = HGT
C
        IF(     CODE.EQ.'MT'.OR. CODE.EQ.'mt' )THEN
          UNITS = 'meters'
        ELSEIF( CODE.EQ.'M' .OR. CODE.EQ.'m'  )THEN
          UNITS = 'meters'
        ELSEIF( CODE.EQ.'FT'.OR. CODE.EQ.'ft' )THEN
          UNITS = 'feet  '
          GHTX  = GHTX*MT2FT
        ELSEIF( CODE.EQ.' ' )THEN
          UNITS = 'meters'
        ELSE
          UNITS = 'meters'
        ENDIF
C
        IF( DATUM.EQ.NGVD )THEN
          OHT = OHT+GHTX
          HT  = SNGL(OHT)
          HT  = ROUND(HT)
          IF( HT.LT.-3000.0 )THEN
            IF( NQ_PGM.EQ.1 .OR. LU.EQ.LDUMP )THEN
              WRITE(LU,1923) 
              WRITE(LU,1928) GHTX,UNITS
            ELSE
              WRITE (LUOUT,928) GHTX,UNITS
            ENDIF
          ELSE 
            IF( NQ_PGM.EQ.1 .OR. LU.EQ.LDUMP )THEN
              WRITE(LU,1923) 
              WRITE(LU,1924) GHTX,UNITS,HT,UNITS,NAVD 
            ELSE
              WRITE (LUOUT,924) GHTX,UNITS,HT,UNITS,NAVD 
            ENDIF
          ENDIF
        ELSE
          OHT = OHT-GHTX
          HT  = SNGL(OHT)
          HT  = ROUND(HT)
          IF( HT.LT.-3000.0 )THEN
            IF( NQ_PGM.EQ.1 .OR. LU.EQ.LDUMP )THEN
              WRITE(LU,1923) 
              WRITE(LU,1928) GHTX,UNITS
            ELSE
              WRITE (LUOUT,928) GHTX,UNITS
            ENDIF
          ELSE 
            IF( NQ_PGM.EQ.1 .OR. LU.EQ.LDUMP )THEN
              WRITE(LU,1923) 
              WRITE(LU,1925) GHTX,UNITS,HT,UNITS,NGVD 
            ELSE
              WRITE (LUOUT,925) GHTX,UNITS,HT,UNITS,NGVD 
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C 
  924 FORMAT('SHIFT: ',F7.3,1X,A6,' HEIGHT: ',F9.3,1X,A6,' NAVD ',A2)
  925 FORMAT('SHIFT: ',F7.3,1X,A6,' HEIGHT: ',F9.3,1X,A6,' NGVD ',A2)
  928 FORMAT('SHIFT: ',F7.3,1X,A6)
 1923 FORMAT(1X,' ')
 1924 FORMAT(1X,'SHIFT: ',F7.3,1X,A6,
     +        ' HEIGHT: ',F9.3,1X,A6,' NAVD ',A2)
 1925 FORMAT(1X,'SHIFT: ',F7.3,1X,A6,
     +        ' HEIGHT: ',F9.3,1X,A6,' NGVD ',A2)
 1928 FORMAT(1X,'SHIFT: ',F7.3,1X,A6)
C
      RETURN
      END

C      SCCSID[]= "@(#) print2.f 1.1 - 99/10/20"

      SUBROUTINE PRINT2 (LU, NCONV, VRSION, GHT)
C
C     This subroutine prints out the actual transformation results 
C     using a free format - the same as the input file format. 
C     This is used for type 2 format.
C
      CHARACTER*96  B96 
      CHARACTER*80  CARD
      INTEGER       LU
      INTEGER       NCONV
      REAL          GHT
      REAL          VRSION
C
      COMMON/CURNT/ B96
      EQUIVALENCE (CARD,B96)
C
C     Write header record to identify source of correction and value
C
   10 FORMAT('VERTCON  Version', F5.2)
C
      IF( NCONV.EQ.1 )THEN
        WRITE (LU, 10) VRSION
      ENDIF
C
C     In this format, the variable CARD contains the image of the input
C     card.  This is written to the output file instead of using the
C     latitude, longitude, and name variables.  The correction
C     overwrites whatever is in columns 33-40
C     write(6,'(a80)') CARD
C
      WRITE (LU,100) CARD(1:32), GHT, CARD(41:80)
  100 FORMAT(A32, F7.3, 1X, A40)
      RETURN
      END

C      SCCSID[]= "@(#) print3.f 1.1 - 99/10/20"

      SUBROUTINE PRINT3 (LU, NCONV, VRSION, GHT)
C
C     This subroutine prints out the actual transformed results using
C     NGS Blue Book File format  (same as the input file format)
C     This is used for type 3 format.
C
      CHARACTER*80     CARD
      CHARACTER*2      CODE, METER
      INTEGER          LU
      INTEGER          NCONV
      REAL             GHT, GHTX
      REAL             VRSION
C
      CHARACTER*96     B96 
      COMMON/CURNT/ B96
C
      INTEGER          NQ_PGM 
      INTEGER          LUIN, LUOUT, NIN, NOUT
      INTEGER          LDUMP,NSPACE 
      LOGICAL          TONGVD
      COMMON/INOUT/ NQ_PGM,LUIN,LUOUT,NOUT,NIN,LDUMP,TONGVD,NSPACE
C
      EQUIVALENCE (CARD,B96)
      DATA METER/'MT'/
C
C     Write header record to identify source of correction and value
C
   10 FORMAT('VERTCON  Version', F5.2)
C
      IF( NCONV.EQ.1 )THEN
        WRITE (LU, 10) VRSION
      ENDIF
C
C     In this format, the variable CARD contains the image of the input
C     card.  This is written to the output file instead of using the
C     latitude, longitude, and name variables.  The elevation is
C     overwritten by the corrected value
C
      READ(B96,'(49x,A2,F10.5)',IOSTAT=IOS,ERR=101) CODE,HT
      GHTX=GHT
      IF( CODE.NE.METER )THEN
        GHTX=DBLE(GHT)/0.3048D0
      ENDIF
C
      IF( TONGVD )THEN
        HT=HT+GHTX
      ELSE
        HT=HT-GHTX
      ENDIF
C
      HT=ROUND(HT)
C
      WRITE (LU,100,IOSTAT=IOS,ERR=102) B96(1:51), HT, B96(62:80)
  100 FORMAT (A51, F10.5, A19)
      RETURN
C
  101 WRITE(LU,*) 'NGS Blue Book format elevation decode error -'
      GOTO 103
C
  102 WRITE(LU,*) 'NGS Blue Book format elevation encode error -'
  103 WRITE(LU,'(A80)') CARD
      STOP 
      END

C      SCCSID[]= "@(#) print4.f 1.1 - 99/10/20"

      SUBROUTINE PRINT4 (LU, NCONV, VRSION, GHT)
C
C     This subroutine prints out the actual transformed results using
C     NGS Internal Benchmark File format (same as input file format)
C     This is used for type 4 format.
C
      CHARACTER*80     CARD
      CHARACTER*2      NAVD,NGVD,CODE
      INTEGER          LU
      INTEGER          NCONV
      REAL             GHT
      REAL             VRSION
C
      CHARACTER*96     B96 
      COMMON/CURNT/ B96
C
      INTEGER          NQ_PGM 
      INTEGER          LUIN, LUOUT, NIN, NOUT
      INTEGER          LDUMP,NSPACE 
      LOGICAL          TONGVD
      COMMON/INOUT/ NQ_PGM,LUIN,LUOUT,NOUT,NIN,LDUMP,TONGVD,NSPACE
C
      SAVE NAVD,NGVD
      EQUIVALENCE (CARD,B96)
      DATA NAVD/'88'/,NGVD/'29'/
C
C     Write header record to identify source of correction and value
C
   10 FORMAT('VERTCON  Version', F5.2)
C
      IF( NCONV.EQ.1 )THEN
        WRITE (LU, 10) VRSION
      ENDIF
C
C     In this format, the variable CARD contains the image of the input
C     card.  This is written to the output file instead of using the
C     latitude, longitude, and name variables.  The approximate
C     elevation is overwritten by the corrected walue when applicable
C
C     WRITE(NOUT,'(A96,f5.2)') B96,GHT 
C
      READ(B96,'(64x,F10.5,20x,A2)',IOSTAT=IOS,ERR=101) HT,CODE
C
      IF( TONGVD )THEN
        IF( (CODE.EQ.'  ').OR.(CODE.EQ.NGVD) )THEN
          B96(95:96)=NAVD
          HT=HT+GHT
        ENDIF
      ELSE
        IF( CODE.EQ.NAVD )THEN
          B96(95:96)=NGVD
          HT=HT-GHT
        ENDIF
      ENDIF
      HT=ROUND(HT)
C
      WRITE (LU,100,IOSTAT=IOS,ERR=102) B96(1:64), HT, B96(75:96)
  100 FORMAT (A64, F10.5, A22)
      RETURN
C
  101 WRITE(LU,*) 'NGS Internal BM format elevation decode error'
      GOTO 103
C
  102 WRITE(LU,*) 'NGS Internal BM format elevation encode error'
  103 WRITE(LU,'(A80)') CARD
      STOP 
      END

C      SCCSID[]= "@(#) rcard.f 1.1 - 99/10/20"

      REAL FUNCTION RCARD (CHLINE, LENG, IERR)
C
C     Read a real number from a line of card image.
C     LENG is the length of the card
C     blanks are the delimiters of the REAL*4 variable
C
      CHARACTER*80    CHLINE
      INTEGER         I, IERR, ILENG
      INTEGER         J
      INTEGER         LENG
      REAL            VAR
C
      IERR = 0
C
C     Find first non-blank character
C     DO WHILE line character is blank, 
C     I is first non-blank character
C
      I = 1
   10 IF( CHLINE(I:I).EQ.' ' .OR. CHLINE(I:I).EQ.',' )THEN
        I = I + 1
C
C       Check for totally blank card
C
        IF( I.GE.LENG )THEN
          RCARD = 0.0E0
          LENG = 0
          RETURN
        ENDIF
        GOTO 10
      ENDIF
C
C     Find first blank character (or end of line)
C     DO WHILE line character is not a blank
C
      J = I + 1
   20 IF( CHLINE(J:J).NE.' ' .AND. CHLINE(J:J).NE.',' )THEN
        J = J + 1
C
C       Check for totally filed card
C
        IF( J.GT.LENG )THEN
          GOTO 40
        ENDIF
        GOTO 20
      ENDIF
C
C     J is now 1 more than the position of 
C     the last non-blank character
C
   40 J = J - 1
C
C     ILENG is the length of the real string, 
C     it cannot be greater than 15 characters
C
      ILENG = J - I + 1
      IF( ILENG.GT.15 )THEN
        STOP 'RCARD'
      ENDIF
C
C     Read the real variable from the line, and 
C     set the return VAR to it
C
      READ (CHLINE(I:J), 55, ERR=9999) VAR
   55 FORMAT (F15.0)
      RCARD = VAR
C
C     Now reset the values of LENG and 
C     CHLINE to the rest of the card
C
      CHLINE(1:LENG) = CHLINE((J+1):LENG)
      LENG = LENG - J
      RETURN
C
C     Read error
C
 9999 IERR = 1
      RETURN
      END

C      SCCSID[]= "@(#) round.f 1.1 - 99/10/20"

      REAL FUNCTION ROUND(VALUE)
C   
      REAL       VALUE
      INTEGER    INTHT
C
      INTHT=(SIGN(1.0,VALUE)*(ABS(VALUE)+0.0005))*1000.0
      ROUND=FLOAT(INTHT)*0.001
C
      RETURN
      END

C      SCCSID[]= "@(#) surf.f 1.1 - 99/10/20"

c
c*********************************************************************
c*    subroutine surf: interpolates the z value                      *
c*********************************************************************
c
      subroutine surf (xgrid,ygrid,zee,ay,bee,cee,dee,irow,jcol)
c
c     calculates the value of the grid at the point (xpt,ypt). the 
c     interp. is done in the index coordinate system for convenience. 
c
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c
      row  = ygrid - dble(float(irow))
      col  = xgrid - dble(float(jcol))
c 
      zee1 = ay
      zee2 = bee*col
      zee3 = cee*row
      zee4 = dee*col*row
      zee  = zee1+zee2+zee3+zee4 
c
      return
      end 

C      SCCSID[]= "@(#) type1.f 1.1 - 99/10/20"

      SUBROUTINE TYPE1 (NAME, IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +                  XPT, YPT, EOF, NOPT)
C
C Read a record from a file of type 1. In this type there is a station
C name (or blanks) in columns 1-40, and free-format latitude and
C longitude values in columns 41-80.  By free format we mean that the
C numbers making up the degrees, minutes and seconds of latitude,
C degrees, minutes, seconds of longitude must appear in that order in
C columns 41 through 80 but are not restricted to any specific columns.
C The latitude and longitude may be either (1) integer degrees, integer
C minutes, decimal seconds, or (2) integer degrees, decimal minutes, or
C (3) decimal degrees.
C
      LOGICAL          EOF, NOPT
      CHARACTER*80     B80
      CHARACTER*80     CARD
      CHARACTER*40     NAME
      CHARACTER*40     DUMLA, DUMLO
      CHARACTER*20     B20
      CHARACTER*1      DOT, BLK
      INTEGER          IFLAG1, IFLAG2, N1, N2
      INTEGER          IDOT, IBLK, LENG, IERR
      INTEGER          IDLA, IMLA, IDLO, IMLO
      REAL             SLA, SLO, RCARD, RMLA, RMLO
      DOUBLE PRECISION XPT, YPT, RDLA, RDLO, DCARD
C
      PARAMETER (DOT='.', BLK=' ')
      PARAMETER (B20='                    ', B80=B20//B20//B20//B20)
C
      CHARACTER*96     B96
      COMMON/CURNT/ B96
C
      INTEGER          NQ_PGM 
      INTEGER          LUIN, LUOUT, NIN, NOUT
      INTEGER          LDUMP,NSPACE(2) 
      COMMON/INOUT/ NQ_PGM,LUIN,LUOUT,NOUT,NIN,LDUMP,NSPACE
C
      EQUIVALENCE(CARD,B96)
      DATA IFLAG1 /1/, IFLAG2 /2/
C
C     FOR INPUT FILE OF ITYPE = 1
C
    1 READ (NIN,'(A80)',END=9999) CARD
C
C     Check for blank line
C
      IF( CARD.EQ.B80 )THEN
        GOTO 1
      ENDIF
      READ (CARD(1:40), '(A40)') NAME
C
C     Find position of the first decimal point (to indicate the 
C     last number in the latitude)
C
      IDOT = INDEX(CARD(41:80), DOT)
C
C     Error - no decimal point
C
      IF( IDOT.EQ.0 )THEN
        GOTO 9980
      ENDIF
C
C     find position of the first blank after the first decimal point 
C     (to indicate the blank after the last number in the latitude)
C
      IDOT = IDOT + 40
      IBLK = INDEX(CARD(IDOT+1:80), BLK)
      IBLK = IBLK + IDOT
      DUMLA = CARD(41:IBLK)
      LENG = IBLK - 41
      RDLA = DCARD( DUMLA, LENG, IERR )
      IF (IERR .NE. 0) GOTO 9950
      IF (LENG .GT. 0) THEN
        RMLA = RCARD( DUMLA, LENG, IERR )
        IF (IERR .NE. 0) GOTO 9950
        IF (LENG .GT. 0) THEN
          SLA  = RCARD( DUMLA, LENG, IERR )
          IF (IERR .NE. 0) GOTO 9950
        ELSE
          SLA = 0.E0
        ENDIF
      ELSE
        RMLA = 0.E0
        SLA = 0.E0
      ENDIF
* Check for illogical values
      IF (RDLA .LT.   0.D0) GOTO 9940
      IF (RDLA .GT.  90.D0) GOTO 9950
      IF (RMLA .LT. 0.E0  .OR.  RMLA .GT. 60.E0) GOTO 9950
      IF ( SLA .LT. 0.E0  .OR.   SLA .GT. 60.E0) GOTO 9950
* LONGITUDE
      DUMLO = CARD(IBLK+1:80)
      CALL NBLANK (DUMLO, IFLAG2, N2)
      LENG = N2
      RDLO = DCARD( DUMLO, LENG, IERR )
      IF (IERR .NE. 0) GOTO 9960
      IF (LENG .GT. 0) THEN
        RMLO = RCARD( DUMLO, LENG, IERR )
        IF (IERR .NE. 0) GOTO 9960
        IF (LENG .GT. 0) THEN
          SLO  = RCARD( DUMLO, LENG, IERR )
          IF (IERR .NE. 0) GOTO 9960
        ELSE
          SLO = 0.E0
        ENDIF
      ELSE
        RMLO = 0.E0
        SLO = 0.E0
      ENDIF
* Check for illogical values
      IF (RDLO .LT.   0.D0) GOTO 9940
      IF (RDLO .GT. 360.D0) GOTO 9960
      IF (RMLO .LT. 0.E0  .OR.  RMLO .GT. 60.E0) GOTO 9960
      IF ( SLO .LT. 0.E0  .OR.   SLO .GT. 60.E0) GOTO 9960
* Calculate decimal degrees
      YPT = RDLA + DBLE(RMLA)/60.D0 + DBLE(SLA)/3600.D0
      XPT = RDLO + DBLE(RMLO)/60.D0 + DBLE(SLO)/3600.D0
* Get degrees, minutes, seconds
      CALL ANGLE (YPT, IDLA, IMLA, SLA)
      CALL ANGLE (XPT, IDLO, IMLO, SLO)
 9000 RETURN
* Error messages
 9940 CONTINUE
      CALL NBLANK (CARD, IFLAG1, N1)
      CALL NBLANK (CARD, IFLAG2, N2)
      WRITE (LUOUT,9945) CARD(N1:N2)
 9945 FORMAT (' ERROR - in the following record:', /,
     +        9X, '''', A, '''', /,
     +        '         Latitude and Longitudes must be positive!', /,
     +        '         Longitude is positive west.', /)
      NOPT = .TRUE.
      GOTO 9000
 9950 CONTINUE
      CALL NBLANK (CARD, IFLAG1, N1)
      CALL NBLANK (CARD, IFLAG2, N2)
      WRITE (LUOUT,9955) CARD(N1:N2)
 9955 FORMAT (' ERROR - Illogical values for latitude',
     +        ' in the following record:', /,
     +        9X, '''', A, '''', /,
     +        '         Latitude must be between 0 and 90 degrees.', /,
     +        '         Minutes and seconds must be between 0',
     +                                                    ' and 60.', /)
      NOPT = .TRUE.
      GOTO 9000
 9960 CONTINUE
      CALL NBLANK (CARD, IFLAG1, N1)
      CALL NBLANK (CARD, IFLAG2, N2)
      WRITE (LUOUT,9965) CARD(N1:N2)
 9965 FORMAT (' ERROR - Illogical values for longitude',
     +        ' in the following record:', /,
     +        9X, '''', A, '''', /,
     +        '         Longitude must be between 0 and 360 degrees.',/,
     +        '         Minutes and seconds must be between 0',
     +                                                    ' and 60.', /)
      NOPT = .TRUE.
      GOTO 9000
 9980 CONTINUE
      CALL NBLANK (CARD, IFLAG1, N1)
      CALL NBLANK (CARD, IFLAG2, N2)
      WRITE (LUOUT,9985) CARD(N1:N2)
 9985 FORMAT (' ERROR - The following record does not have a decimal',
     +        ' point in the latitude.', /,
     +        9X, '''', A, '''', /,
     +        '         In the free format a decimal point is used',
     +        ' to determine what is', /,
     +        '         the last number in the latitude.  Please',
     +        ' correct this record', /,
     +        '         and check all of the data in this file to',
     +        ' ensure that it follows', /,
     +        '         the correct format.', /)
      NOPT = .TRUE.
      GOTO 9000
 9999 CONTINUE
      EOF = .TRUE.
      GOTO 9000
      END

C      SCCSID[]= "@(#) type2.f 1.1 - 99/10/20"

      SUBROUTINE TYPE2 (NAME,IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +                  XPT, YPT, EOF, NOPT)
c
c Read a record from a file of type 2. In this type there is free-format
c latitude and longitude values in columns 1-32, and a station name
c (or blanks) in columns 41-80.  Columns 33-39 will contain the corr.
c in the output record and are ignored in the input record.
c By free format we mean that the numbers making up the degrees,
c minutes and seconds of latitude, degrees, minutes, seconds of
c longitude must appear in that order in columns 1 through 32 but are
c not restricted to any specific columns.  The latitude and longitude
c may be either (1) integer degrees, integer minutes, decimal seconds,
c or (2) integer degrees, decimal minutes, or (3) decimal degrees.
c
      LOGICAL          EOF, NOPT
      CHARACTER*80     B80
      CHARACTER*80     CARD
      CHARACTER*40     NAME
      CHARACTER*32     DUMLA, DUMLO
      CHARACTER*20     B20
      CHARACTER*1      DOT, BLK
      INTEGER          IFLAG1, IFLAG2
      INTEGER          IDOT, IBLK, IERR
      INTEGER          IDLA, IMLA, IDLO, IMLO
      INTEGER          LENG
      INTEGER          N1, N2
      REAL             RCARD, RMLA, RMLO
      REAL             SLA, SLO
      DOUBLE PRECISION DCARD
      DOUBLE PRECISION RDLA, RDLO
      DOUBLE PRECISION XPT, YPT
C
      PARAMETER (DOT='.', BLK=' ')
      PARAMETER (B20='                    ', B80=B20//B20//B20//B20)
C
      CHARACTER*96     B96
      COMMON/CURNT/ B96
C
      INTEGER          NQ_PGM 
      INTEGER          LUIN, LUOUT, NIN, NOUT
      INTEGER          LDUMP,NSPACE(2) 
      COMMON/INOUT/ NQ_PGM,LUIN,LUOUT,NOUT,NIN,LDUMP,NSPACE
C
      EQUIVALENCE(CARD,B96)
      DATA IFLAG1 /1/, IFLAG2 /2/
* FOR INPUT FILE OF ITYPE = 2
    1 READ (NIN,'(A80)',END=9999) CARD
* Check for blank line
      IF( CARD.EQ.B80 )THEN
        GOTO 1
      ENDIF
C
      READ(CARD(41:80),'(A40)') NAME
C
C     Find position of the first decimal point 
C       (to indicate the last number in the latitude)
C
      IDOT = INDEX(CARD(1:32), DOT)
* Error - no decimal point
      IF( IDOT.EQ.0 )THEN 
        GOTO 9980
      ENDIF
* find position of the first blank after the first decimal point (to
* indicate the blank after the last number in the latitude)
      IBLK = INDEX(CARD(IDOT+1:32), BLK)
      IBLK = IBLK + IDOT
      DUMLA = CARD(1:IBLK)
      LENG = IBLK - 1
      RDLA = DCARD( DUMLA, LENG, IERR )
      IF (IERR .NE. 0) GOTO 9950
      IF (LENG .GT. 0) THEN
        RMLA = RCARD( DUMLA, LENG, IERR )
        IF (IERR .NE. 0) GOTO 9950
        IF (LENG .GT. 0) THEN
          SLA  = RCARD( DUMLA, LENG, IERR )
          IF (IERR .NE. 0) GOTO 9950
        ELSE
          SLA = 0.E0
        ENDIF
      ELSE
        RMLA = 0.E0
        SLA = 0.E0
      ENDIF
* Check for illogical values
      IF (RDLA .LT.   0.D0) GOTO 9940
      IF (RDLA .GT.  90.D0) GOTO 9950
      IF (RMLA .LT. 0.E0  .OR.  RMLA .GT. 60.E0) GOTO 9950
      IF ( SLA .LT. 0.E0  .OR.   SLA .GT. 60.E0) GOTO 9950
* LONGITUDE
      DUMLO = CARD(IBLK+1:32)
      CALL NBLANK (DUMLO, IFLAG2, N2)
      LENG = N2
      RDLO = DCARD( DUMLO, LENG, IERR )
      IF (IERR .NE. 0) GOTO 9960
      IF (LENG .GT. 0) THEN
        RMLO = RCARD( DUMLO, LENG, IERR )
        IF (IERR .NE. 0) GOTO 9960
        IF (LENG .GT. 0) THEN
          SLO  = RCARD( DUMLO, LENG, IERR )
          IF (IERR .NE. 0) GOTO 9960
        ELSE
          SLO = 0.E0
        ENDIF
      ELSE
        RMLO = 0.E0
        SLO = 0.E0
      ENDIF
* Check for illogical values
      IF (RDLO .LT.   0.D0) GOTO 9940
      IF (RDLO .GT. 360.D0) GOTO 9960
      IF (RMLO .LT. 0.E0  .OR.  RMLO .GT. 60.E0) GOTO 9960
      IF ( SLO .LT. 0.E0  .OR.   SLO .GT. 60.E0) GOTO 9960
* Calculate decimal degrees
      YPT = RDLA + DBLE(RMLA)/60.D0 + DBLE(SLA)/3600.D0
      XPT = RDLO + DBLE(RMLO)/60.D0 + DBLE(SLO)/3600.D0
* Get degrees, minutes, seconds
      CALL ANGLE (YPT, IDLA, IMLA, SLA)
      CALL ANGLE (XPT, IDLO, IMLO, SLO)
 9000 RETURN
* Error messages
 9940 CONTINUE
      CALL NBLANK (CARD, IFLAG1, N1)
      CALL NBLANK (CARD, IFLAG2, N2)
      WRITE (LUOUT,9945) CARD(N1:N2)
 9945 FORMAT (' ERROR - in the following record:', /,
     +        9X, '''', A, '''', /,
     +        '         Latitude and Longitudes must be positive!', /,
     +        '         Longitude is positive west.', /)
      NOPT = .TRUE.
      GOTO 9000
 9950 CONTINUE
      CALL NBLANK (CARD, IFLAG1, N1)
      CALL NBLANK (CARD, IFLAG2, N2)
      WRITE (LUOUT,9955) CARD(N1:N2)
 9955 FORMAT (' ERROR - Illogical values for latitude',
     +        ' in the following record:', /,
     +        9X, '''', A, '''', /,
     +        '         Latitude must be between 0 and 90 degrees.', /,
     +        '         Minutes and seconds must be between 0',
     +                                                    ' and 60.', /)
      NOPT = .TRUE.
      GOTO 9000
 9960 CONTINUE
      CALL NBLANK (CARD, IFLAG1, N1)
      CALL NBLANK (CARD, IFLAG2, N2)
      WRITE (LUOUT,9965) CARD(N1:N2)
 9965 FORMAT (' ERROR - Illogical values for longitude',
     +        ' in the following record:', /,
     +        9X, '''', A, '''', /,
     +        '         Longitude must be between 0 and 360 degrees.',/,
     +        '         Minutes and seconds must be between 0',
     +                                                    ' and 60.', /)
      NOPT = .TRUE.
      GOTO 9000
 9980 CONTINUE
      CALL NBLANK (CARD, IFLAG1, N1)
      CALL NBLANK (CARD, IFLAG2, N2)
      WRITE (LUOUT,9985) CARD(N1:N2)
 9985 FORMAT (' ERROR - The following record does not have a decimal',
     +        ' point in the latitude.', /,
     +        9X, '''', A, '''', /,
     +        '         In the free format a decimal point is used',
     +        ' to determine what is', /,
     +        '         the last number in the latitude.  Please',
     +        ' correct this record', /,
     +        '         and check all of the data in this file to',
     +        ' ensure that it follows', /,
     +        '         the correct format.', /)
      NOPT = .TRUE.
      GOTO 9000
 9999 CONTINUE
      EOF = .TRUE.
      GOTO 9000
      END

C      SCCSID[]= "@(#) type34.f 1.1 - 99/10/20"

      SUBROUTINE TYPE34 (NCONV,VRSION,ITYPE,NAME,IDLA, IMLA, SLA, 
     .    IDLO, IMLO, SLO, XPT, YPT, EOF, NOPT)
C
C Read a record from a file of type 3 or 4. In this type only the
C latitude, longitude and orthometric height are extracted;
C the record MUST BE either in NGS Blue Book or in 
C       NGS Internal Bench Mark File format
C  NOTE - blank records or non- *30* blue book records are copied
C
      LOGICAL          EOF, NOPT
      CHARACTER*80     B80
      CHARACTER*80     CARD
      CHARACTER*40     NAME
      CHARACTER*20     B20
      CHARACTER*6      NGVD,NAVD
      INTEGER          IDLA, IMLA, IDLO, IMLO, ITYPE
      INTEGER          NCONV
      REAL             RMLA, RMLO
      REAL             SLA, SLO
      REAL             VRSION
      DOUBLE PRECISION RDLA, RDLO
      DOUBLE PRECISION XPT, YPT
C
      PARAMETER (B20='                    ', B80=B20//B20//B20//B20)
C
      CHARACTER*96     B96 
      COMMON/CURNT/ B96
C
      INTEGER          NQ_PGM 
      INTEGER          LUIN, LUOUT, NIN, NOUT
      INTEGER          LDUMP 
      LOGICAL          TONGVD, CODE15
      COMMON/INOUT/ NQ_PGM,LUIN,LUOUT,NOUT,NIN,LDUMP,TONGVD,CODE15
C
      EQUIVALENCE(CARD,B96)
      DATA NGVD/'NGVD29'/,NAVD/'NAVD88'/
* FOR INPUT FILE OF ITYPE = 3 or 4
   10 IF( ITYPE.EQ.3 )THEN
        READ (NIN,'(A80)',IOSTAT=IOS,ERR=9991,END=9999) CARD
      ELSE
        READ (NIN,'(A96)',IOSTAT=IOS,ERR=9991,END=9999) B96 
      ENDIF
C
C Check for blank line
C
    1 FORMAT('VERTCON  Version', F5.2)
    2 FORMAT(A80)
C
      IF( CARD.EQ.B80 )THEN
        NCONV=NCONV+1
        IF( NCONV.EQ.1 )THEN
          WRITE (NOUT, 1) VRSION
        ENDIF
        WRITE(NOUT,2,IOSTAT=IOS,ERR=9992) CARD
        GOTO 10
       ENDIF
C
C Check for non-*30* blue book records
C
      IF( ITYPE.EQ.3 )THEN
        IF( CARD(7:10).NE.'*30*' )THEN
          NCONV=NCONV+1
          IF( NCONV.EQ.1)THEN
            WRITE (NOUT, 1) VRSION
          ENDIF
          IF( CARD(7:10).EQ.'*15*' )THEN
            IF( CARD(11:16).EQ.NGVD )THEN
              CODE15=.TRUE.
              TONGVD=.TRUE.
              CARD(11:16)=NAVD(1:6)
            ELSEIF( CARD(11:16).EQ.NAVD )THEN
              CODE15=.TRUE.
              TONGVD=.FALSE.
              CARD(11:16)=NGVD(1:6)
            ENDIF
          ENDIF
          WRITE(NOUT,2,IOSTAT=IOS,ERR=9992) CARD
          GOTO 10
        ELSE
          IF(.NOT.CODE15)THEN
         WRITE(LUOUT,*) ' Blue Book record *15* with Datum Code',
     +           ' was not found before *30* record -'
            NOPT=.TRUE.
            RETURN
          ENDIF
        ENDIF
C
        READ(CARD(15:39),'(A25)') NAME
        READ(B96,3,IOSTAT=IOS,ERR=9970) 
     .        RDLA,RMLA,SLA,RDLO,RMLO,SLO
    3 FORMAT(67x,3f2.0,f3.0,2f2.0,16x)
      ELSE
        READ(CARD(20:49),'(A30)') NAME
        READ(B96,4,IOSTAT=IOS,ERR=9970) 
     .        RDLA,RMLA,SLA,RDLO,RMLO,SLO
    4 FORMAT(49x,3f2.0,1x,f3.0,2f2.0,33x)
      ENDIF
* Check for illogical values
      IF (RDLA .LT.   0.D0) GOTO 9950
      IF (RDLA .GT.  90.D0) GOTO 9950
      IF (RMLA .LT. 0.E0  .OR.  RMLA .GT. 60.E0) GOTO 9950
      IF ( SLA .LT. 0.E0  .OR.   SLA .GT. 60.E0) GOTO 9950
* LONGITUDE
* Check for illogical values
      IF (RDLO .LT.   0.D0) GOTO 9960
      IF (RDLO .GT. 360.D0) GOTO 9960
      IF (RMLO .LT. 0.E0  .OR.  RMLO .GT. 60.E0) GOTO 9960
      IF ( SLO .LT. 0.E0  .OR.   SLO .GT. 60.E0) GOTO 9960
* Calculate decimal degrees
      YPT = RDLA + DBLE(RMLA)/60.D0 + DBLE(SLA)/3600.D0
      XPT = RDLO + DBLE(RMLO)/60.D0 + DBLE(SLO)/3600.D0
* Get degrees, minutes, seconds
      CALL ANGLE (YPT, IDLA, IMLA, SLA)
      CALL ANGLE (XPT, IDLO, IMLO, SLO)
      RETURN
* Error messages
 9950 WRITE (LUOUT,*) ' ERROR - Illogical values for latitude'
      GO TO 9980
 9960 WRITE (LUOUT,*) ' ERROR - Illogical values for longitude'
      GO TO 9980
 9970 WRITE (LUOUT,*) ' ERROR - NGS File geod. position format error'
*
 9980 NOPT = .TRUE.  
      WRITE(LUOUT,'(a80)') CARD
      RETURN
 9991 WRITE(LUOUT,5) IOS
    5 FORMAT(' INPUT file i/o error -',i5)
      GO TO 9980
 9992 WRITE(LUOUT,6) IOS
    6 FORMAT(' OUTPUT file i/o error -',i5)
      GO TO 9980
*
 9999 EOF = .TRUE.
      RETURN
      END

C      SCCSID[]= "@(#) wrtpt.f 1.1 - 99/10/20"

      SUBROUTINE WRTPT (ITYPE, NCONV, NPOINT, VRSION, NAME,
     +                  IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +                  GHT, IPAGE, PAGE, SCREEN)
C
C     Write the computations to output file (and screen).
C
      LOGICAL          PAGE, SCREEN
      CHARACTER*80     CARD
      CHARACTER*40     NAME
      INTEGER          ITYPE, IPAGE
      INTEGER          IDLA, IMLA, IDLO, IMLO
      INTEGER          NCONV, NPOINT
      REAL             GHT
      REAL             SLA, SLO
      REAL             VRSION
C
      CHARACTER*96     B96
      COMMON/CURNT/ B96
C
      INTEGER          NQ_PGM 
      INTEGER          LUIN, LUOUT, NIN, NOUT
      INTEGER          LDUMP,NSPACE(2) 
      COMMON/INOUT/ NQ_PGM,LUIN,LUOUT,NOUT,NIN,LDUMP,NSPACE
C
      EQUIVALENCE(CARD,B96)
c
c     PAGE NUMBER COUNTER
c     this is where you change how many print out on a page
c
      IF( (NPOINT.EQ.1).OR.(MOD(NPOINT,13).EQ.0) )THEN
        PAGE  = .TRUE.
        IPAGE = IPAGE + 1
      ENDIF
c
c     WRITE TO OUTPUT FILE
c
      IF(     ITYPE.EQ.0 )THEN
c
c       ONLY INTERACTIVE USE - NO INPUT FILE
c
        CALL PRINT (NOUT, NPOINT, NAME, VRSION, 
     +         IDLA, IMLA, SLA, 
     +         IDLO, IMLO, SLO, 
     +         GHT, IPAGE, PAGE)
      ELSEIF( ITYPE.EQ.1 )THEN
c       FOR FREE FORMAT TYPE 1
        CALL PRINT (NOUT, NPOINT, NAME, VRSION, 
     +         IDLA, IMLA, SLA, 
     +         IDLO, IMLO, SLO, 
     +         GHT, IPAGE, PAGE)
      ELSEIF( ITYPE.EQ.2 )THEN
c       FOR FREE FORMAT TYPE 2
        CALL PRINT2 (NOUT, NCONV, VRSION, GHT)
      ELSEIF( ITYPE.EQ.3 )THEN
c       NGS BLUE BOOK FORMAT TYPE 3
        CALL PRINT3 (NOUT, NCONV, VRSION, GHT)
      ELSEIF( ITYPE.EQ.4 )THEN
c       NGS BM FORMAT TYPE 4
        CALL PRINT4 (NOUT, NCONV, VRSION, GHT)
      ENDIF
c
c     FOR SCREEN OUTPUT
c
      IF( SCREEN )THEN
        IF( NQ_PGM.LE.1 )THEN
          CALL PRINT (LDUMP, NPOINT, NAME, VRSION, 
     +         IDLA, IMLA, SLA, 
     +         IDLO, IMLO, SLO, 
     +         GHT, IPAGE, PAGE)
        ELSE
C
C         DO NOTHING  ...  "WEB" INTERFACE HERE
C
        ENDIF
      ENDIF
c
      RETURN
      END
