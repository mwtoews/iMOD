      SUBROUTINE URWORD(LINE,ICOL,ISTART,ISTOP,NCODE,N,R,IOUT,IN)
C     ******************************************************************
C     ROUTINE TO EXTRACT A WORD FROM A LINE OF TEXT, AND OPTIONALLY
C     CONVERT THE WORD TO A NUMBER.
C        ISTART AND ISTOP WILL BE RETURNED WITH THE STARTING AND
C          ENDING CHARACTER POSITIONS OF THE WORD.
C        THE LAST CHARACTER IN THE LINE IS SET TO BLANK SO THAT IF ANY
C          PROBLEMS OCCUR WITH FINDING A WORD, ISTART AND ISTOP WILL
C          POINT TO THIS BLANK CHARACTER.  THUS, A WORD WILL ALWAYS BE
C          RETURNED UNLESS THERE IS A NUMERIC CONVERSION ERROR.  BE SURE
C          THAT THE LAST CHARACTER IN LINE IS NOT AN IMPORTANT CHARACTER
C          BECAUSE IT WILL ALWAYS BE SET TO BLANK.
C        A WORD STARTS WITH THE FIRST CHARACTER THAT IS NOT A SPACE OR
C          COMMA, AND ENDS WHEN A SUBSEQUENT CHARACTER THAT IS A SPACE
C          OR COMMA.  NOTE THAT THESE PARSING RULES DO NOT TREAT TWO
C          COMMAS SEPARATED BY ONE OR MORE SPACES AS A NULL WORD.
C        FOR A WORD THAT BEGINS WITH "'", THE WORD STARTS WITH THE
C          CHARACTER AFTER THE QUOTE AND ENDS WITH THE CHARACTER
C          PRECEDING A SUBSEQUENT QUOTE.  THUS, A QUOTED WORD CAN
C          INCLUDE SPACES AND COMMAS.  THE QUOTED WORD CANNOT CONTAIN
C          A QUOTE CHARACTER.
C        IF NCODE IS 1, THE WORD IS CONVERTED TO UPPER CASE.
C        IF NCODE IS 2, THE WORD IS CONVERTED TO AN INTEGER.
C        IF NCODE IS 3, THE WORD IS CONVERTED TO A REAL NUMBER.
C        NUMBER CONVERSION ERROR IS WRITTEN TO UNIT IOUT IF IOUT IS
C          POSITIVE; ERROR IS WRITTEN TO DEFAULT OUTPUT IF IOUT IS 0;
C          NO ERROR MESSAGE IS WRITTEN IF IOUT IS NEGATIVE.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*(*) LINE
      CHARACTER*20 STRING
      CHARACTER*30 RW
      CHARACTER*1 TAB
C     ------------------------------------------------------------------
      TAB=CHAR(9)
C
C1------Set last char in LINE to blank and set ISTART and ISTOP to point
C1------to this blank as a default situation when no word is found.  If
C1------starting location in LINE is out of bounds, do not look for a
C1------word.
      LINLEN=LEN(LINE)
      LINE(LINLEN:LINLEN)=' '
      ISTART=LINLEN
      ISTOP=LINLEN
      LINLEN=LINLEN-1
      IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
C
C2------Find start of word, which is indicated by first character that
C2------is not a blank, a comma, or a tab.
      DO 10 I=ICOL,LINLEN
      IF(LINE(I:I).NE.' ' .AND. LINE(I:I).NE.','
     &    .AND. LINE(I:I).NE.TAB) GO TO 20
10    CONTINUE
      ICOL=LINLEN+1
      GO TO 100
C
C3------Found start of word.  Look for end.
C3A-----When word is quoted, only a quote can terminate it.
20    IF(LINE(I:I).EQ.'''') THEN
         I=I+1
         IF(I.LE.LINLEN) THEN
            DO 25 J=I,LINLEN
            IF(LINE(J:J).EQ.'''') GO TO 40
25          CONTINUE
         END IF
C
C3B-----When word is not quoted, space, comma, or tab will terminate.
      ELSE
         DO 30 J=I,LINLEN
         IF(LINE(J:J).EQ.' ' .OR. LINE(J:J).EQ.','
     &    .OR. LINE(J:J).EQ.TAB) GO TO 40
30       CONTINUE
      END IF
C
C3C-----End of line without finding end of word; set end of word to
C3C-----end of line.
      J=LINLEN+1
C
C4------Found end of word; set J to point to last character in WORD and
C-------set ICOL to point to location for scanning for another word.
40    ICOL=J+1
      J=J-1
      IF(J.LT.I) GO TO 100
      ISTART=I
      ISTOP=J
C
C5------Convert word to upper case and RETURN if NCODE is 1.
      IF(NCODE.EQ.1) THEN
         IDIFF=ICHAR('a')-ICHAR('A')
         DO 50 K=ISTART,ISTOP
            IF(LINE(K:K).GE.'a' .AND. LINE(K:K).LE.'z')
     1             LINE(K:K)=CHAR(ICHAR(LINE(K:K))-IDIFF)
50       CONTINUE
         RETURN
      END IF
C
C6------Convert word to a number if requested.
100   IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
         RW=' '
         L=30-ISTOP+ISTART
         IF(L.LT.1) GO TO 200
         RW(L:30)=LINE(ISTART:ISTOP)
         IF(NCODE.EQ.2) READ(RW,'(I30)',ERR=200) N
         IF(NCODE.EQ.3) READ(RW,'(F30.0)',ERR=200) R
      END IF
      RETURN
C
C7------Number conversion error.
200   IF(NCODE.EQ.3) THEN
         STRING= 'A REAL NUMBER'
         L=13
      ELSE
         STRING= 'AN INTEGER'
         L=10
      END IF
C
C7A-----If output unit is negative, set last character of string to 'E'.
      IF(IOUT.LT.0) THEN
         N=0
         R=0.
         LINE(LINLEN+1:LINLEN+1)='E'
         RETURN
C
C7B-----If output unit is positive; write a message to output unit.
      ELSE IF(IOUT.GT.0) THEN
         IF(IN.GT.0) THEN
            WRITE(IOUT,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
         ELSE
            WRITE(IOUT,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
         END IF
201      FORMAT(1X,/1X,'FILE UNIT ',I4,' : ERROR CONVERTING "',A,
     1       '" TO ',A,' IN LINE:',/1X,A)
202      FORMAT(1X,/1X,'KEYBOARD INPUT : ERROR CONVERTING "',A,
     1       '" TO ',A,' IN LINE:',/1X,A)
C
C7C-----If output unit is 0; write a message to default output.
      ELSE
         IF(IN.GT.0) THEN
            WRITE(*,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
         ELSE
            WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
         END IF
      END IF
C
C7D-----STOP after writing message.
      CALL PKSSTOP(' ')
      END
      SUBROUTINE UPCASE(WORD)
C     ******************************************************************
C     CONVERT A CHARACTER STRING TO ALL UPPER CASE
C     ******************************************************************
C       SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER WORD*(*)
C
C1------Compute the difference between lowercase and uppercase.
      L = LEN(WORD)
      IDIFF=ICHAR('a')-ICHAR('A')
C
C2------Loop through the string and convert any lowercase characters.
      DO 10 K=1,L
      IF(WORD(K:K).GE.'a' .AND. WORD(K:K).LE.'z')
     1   WORD(K:K)=CHAR(ICHAR(WORD(K:K))-IDIFF)
10    CONTINUE
C
C3------return.
      RETURN
      END
      SUBROUTINE URDCOM(IN,IOUT,LINE)
C     ******************************************************************
C     READ COMMENTS FROM A FILE AND PRINT THEM.  RETURN THE FIRST LINE
C     THAT IS NOT A COMMENT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*(*) LINE
C     ------------------------------------------------------------------
C
C1------Read a line
   10 READ(IN,'(A)') LINE
C
C2------If the line does not start with "#", return.
      IF(LINE(1:1).NE.'#') RETURN
C
C3------Find the last non-blank character.
      L=LEN(LINE)
      DO 20 I=L,1,-1
      IF(LINE(I:I).NE.' ') GO TO 30
   20 CONTINUE
C
C4------Print the line up to the last non-blank character if IOUT>0.
   30 IF (IOUT.GT.0) WRITE(IOUT,'(1X,A)') LINE(1:I)
      GO TO 10
C
      END
       SUBROUTINE U1DRELDUM(IN,JJ)
C     ******************************************************************
C     ROUTINE TO INPUT 1-D REAL DATA MATRICES
C       A IS ARRAY TO INPUT
C       ANAME IS 24 CHARACTER DESCRIPTION OF A
C       JJ IS NO. OF ELEMENTS
C       IN IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*20 FMTIN
      CHARACTER*200 CNTRL
      CHARACTER*200 FNAME
      DATA NUNOPN/99/
      INCLUDE 'openspec.inc'
      REAL RDUM
C     ------------------------------------------------------------------
C
C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
C
C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,0,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,0,IN)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,0,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
      ELSE
C
C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
      END IF
C
C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,0,IN)
         IF(LOCAT.GT.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,0,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,0,IN)
         END IF
      END IF
C
C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.GT.0) GO TO 90
C
C4A-----LOCAT <0 OR =0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
      DO 80 J=1,JJ
   80 RDUM=CNSTNT
      RETURN
C
C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 CONTINUE
      IF(FMTIN.EQ.'(FREE)') THEN
      READ(LOCAT,*) (RDUM,J=1,JJ)
      ELSE
         READ(LOCAT,FMTIN) (RDUM,J=1,JJ)
      END IF
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
C
C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
      ZERO=0.
      IF(CNSTNT.EQ.ZERO) GO TO 120
      DO 100 J=1,JJ
  100 RDUM=RDUM*CNSTNT
C
C6------IF PRINT CODE (IPRN) =0 OR >0 THEN PRINT ARRAY VALUES.
120   CONTINUE
C
C7------RETURN
      RETURN
C8------CONTROL RECORD ERROR.
500   WRITE(*,502) ANAME
502   FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      WRITE(*,'(1X,A)') CNTRL
      CALL PKSSTOP(' ')
      END

C  SWR UTILITY SUBROUTINES
      SUBROUTINE SSWR_RD_COMM(Iu)
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Iu
C       + + + LOCAL DEFINITIONS + + +
        CHARACTER (LEN=2), PARAMETER :: comment = '//'
        CHARACTER (LEN=200) :: line
        LOGICAL :: iscomment
        INTEGER :: ios
        line = comment
        DO
          READ (Iu,'(A)',IOSTAT=ios) line
          IF (ios /= 0) CALL PKSSTOP('COULD NOT READ FROM UNIT Iu')
          IF (LEN_TRIM(line).LT.1) THEN
            line = comment
            CYCLE
          END IF
          line = TRIM(ADJUSTL(line))
          iscomment = .FALSE.
          SELECT CASE (line(1:1))
            CASE ('#')
              iscomment = .TRUE.
            CASE ('!')
              iscomment = .TRUE.
            CASE DEFAULT
              IF (line(1:2).EQ.comment) iscomment = .TRUE.
          END SELECT
          IF (.NOT.iscomment) THEN
            BACKSPACE(Iu)
            RETURN
          END IF
        END DO
        RETURN
      END SUBROUTINE SSWR_RD_COMM
      
C=======================================================================
      SUBROUTINE PKSSTOP(STOPMESS)
C     ******************************************************************
C     STOP PROGRAM, WITH OPTION TO PRINT MESSAGE BEFORE STOPPING
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER STOPMESS*(*)
C     ------------------------------------------------------------------
   10 FORMAT(1X,A)

      IF (STOPMESS.NE.' ') THEN
        WRITE(*,10) STOPMESS
      ENDIF
      STOP

      END      
      
      SUBROUTINE GETIRCL( N, ICOL, IROW, ILAY, NCOL, NROW, NLAY )
C     ******************************************************************
C     CONVERT NODE NUMBER TO (ICOL,IROW,ILAY).
C     ******************************************************************

      IMPLICIT NONE
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER, INTENT(IN)  :: N, NCOL, NROW, NLAY 
      INTEGER, INTENT(OUT) :: ICOL, IROW, ILAY
C
      INTEGER :: NRC
C     ------------------------------------------------------------------
C
      NRC = NROW*NCOL
C
      ILAY = INT( (N - 1)/NRC ) + 1
      IROW = INT( (N - (ILAY-1)*NRC - 1 )/NCOL )  + 1
C
C     N = ICOL + (IROW-1)*NCOL + (ILAY-1)*NRC
      ICOL = N - (IROW-1)*NCOL - (ILAY-1)*NRC
C
      RETURN
      END