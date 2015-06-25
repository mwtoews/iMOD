!!  Copyright (C) Stichting Deltares, 2005-2014.
!!
!!  This file is part of iMOD.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License as published by
!!  the Free Software Foundation, either version 3 of the License, or
!!  (at your option) any later version.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!!
!!  Contact: imod.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands.

MODULE IMOD_UTL

 INTEGER,PARAMETER :: NOS=3
 INTEGER, SAVE :: OS = 1                      !## operating system 1=dos,2=linux,3=unix
 CHARACTER(LEN=20),DIMENSION(NOS),SAVE :: OSN

 LOGICAL,PARAMETER :: LPWT=.TRUE.    !##   FALSE=USE OLD PWT PACKAGE, TRUE=USE NEW ONE
 INTEGER,PARAMETER,PRIVATE :: MAXFILES=500
 INTEGER,PARAMETER :: ICF = 1 ! Intel compiler: ICF = 1
 INTEGER,SAVE :: IUOUT
 INTEGER,DIMENSION(2),SAVE :: IFLAG
 REAL,PARAMETER :: MSWPMV=10.0       !##   add meter to surface level urban area

CONTAINS

 !###===================================================================
 SUBROUTINE IMOD_UTL_STRING(LINE)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: LINE

 CALL IMOD_UTL_DELNULCHAR(LINE)
 CALL IMOD_UTL_DELCONTROLM(LINE)

 END SUBROUTINE IMOD_UTL_STRING

 !###===================================================================
 SUBROUTINE IMOD_UTL_FILENAME(LINE)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: LINE
 LOGICAL :: LEX

 CALL IMOD_UTL_SWAPSLASH(LINE)
 LINE=ADJUSTL(LINE)

 END SUBROUTINE IMOD_UTL_FILENAME

 !###===================================================================
 SUBROUTINE IMOD_UTL_DELNULCHAR(LINE)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: LINE
 INTEGER :: I

 !## find ^M (null character)
 I=INDEX(LINE,CHAR(0))
 IF(I.EQ.0)RETURN
 !## replace by space
 LINE(I:I)=CHAR(32)

 END SUBROUTINE IMOD_UTL_DELNULCHAR

 !###===================================================================
 SUBROUTINE IMOD_UTL_DELCONTROLM(LINE)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: LINE
 INTEGER :: I

 !#find ^M (carriage return)
 I=INDEX(LINE,CHAR(13))
 IF(I.LE.0)RETURN
 !#replace by space
 LINE(I:I)=CHAR(32)

 END SUBROUTINE IMOD_UTL_DELCONTROLM

 !###===================================================================
 SUBROUTINE IMOD_UTL_SWAPSLASH(LINE)
 !###===================================================================
 IMPLICIT NONE

 CHARACTER(LEN=*),INTENT(INOUT) :: LINE
 INTEGER :: I,IFR,ITO

 IF(OS.EQ.1)THEN
  IFR=47
  ITO=92
 ELSEIF(OS.EQ.2)THEN
  IFR=92
  ITO=47
 ENDIF

 DO
  I=INDEX(LINE,CHAR(IFR))
  IF(I.EQ.0)EXIT
  LINE(I:I)=CHAR(ITO)
 ENDDO

 END SUBROUTINE IMOD_UTL_SWAPSLASH

 !###===================================================================
 SUBROUTINE IMOD_UTL_GETSLASH(SLASH)
 !###===================================================================
 IMPLICIT NONE

 CHARACTER(LEN=*),INTENT(OUT) :: SLASH

 IF(OS.EQ.1)THEN ! DOS
   SLASH='\'   
 ELSEIF(OS.EQ.2)THEN ! UNIX
   SLASH='/'   
 ENDIF

 END SUBROUTINE IMOD_UTL_GETSLASH
 
 !###===================================================================
 SUBROUTINE IMOD_UTL_GETDIR(DIR)
 !###===================================================================
 
 CHARACTER(LEN=*),INTENT(INOUT) :: DIR
 
 INTEGER :: I
 CHARACTER(LEN=1) :: SLASH
 
 CALL IMOD_UTL_GETSLASH(SLASH) 
 I = INDEX(DIR,SLASH,BACK=.TRUE.)
 DIR = DIR(1:I)
 
 END SUBROUTINE IMOD_UTL_GETDIR
 
 !###===================================================================
 INTEGER FUNCTION IMOD_UTL_GETUNIT()
 !###===================================================================
 IMPLICIT NONE
 LOGICAL :: LEX

 DO IMOD_UTL_GETUNIT=20,MAXFILES
  INQUIRE(UNIT=IMOD_UTL_GETUNIT,OPENED=LEX)
  IF(.NOT.LEX)RETURN
 ENDDO

 CALL IMOD_UTL_PRINTTEXT('INCREASE IMOD_UTL_GETUNIT, MAX. FILES TO BE OPENED = '// &
      TRIM(IMOD_UTL_ITOS(MAXFILES))//'!',2)

 END FUNCTION IMOD_UTL_GETUNIT

 !###===================================================================
 SUBROUTINE IMOD_UTL_CHECKPATH(FNAME)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: I

 I=0
 IF(OS.EQ.1)THEN
  I=INDEX(FNAME,':')
 ELSEIF(OS.EQ.2)THEN
  IF(FNAME(1:1).EQ.CHAR(92))I=1
  IF(FNAME(1:1).EQ.CHAR(47))I=1
 ENDIF
 IF(I.EQ.0)CALL IMOD_UTL_PRINTTEXT('You should specify entire path in the first line of the runfile!',2)

 END SUBROUTINE IMOD_UTL_CHECKPATH

 !###======================================================================
 SUBROUTINE IMOD_UTL_S_CAP(STR,TXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN)    :: TXT
 CHARACTER(LEN=*),INTENT(INOUT) :: STR
 INTEGER                        :: I,J,K,B1,B2

 IF(TXT.EQ.'l'.OR.TXT.EQ.'L')THEN
  B1= 65
  B2= 90
  K = 32
 ELSEIF(TXT.EQ.'u'.OR.TXT.EQ.'U')THEN
  B1= 97
  B2= 122
  K =-32
 ENDIF

 DO I=1,LEN_TRIM(STR)
  J=IACHAR(STR(I:I))
  IF(J.GE.B1.AND.J.LE.B2)J=J+K
  STR(I:I)=ACHAR(J)
 END DO

 END SUBROUTINE IMOD_UTL_S_CAP

!###======================================================================
 SUBROUTINE IMOD_UTL_CAP(STR,TXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: TXT
 CHARACTER(LEN=*),INTENT(INOUT) :: STR
 INTEGER :: I,J,K,B1,B2

 IF(TXT.EQ.'l'.OR.TXT.EQ.'L')THEN
  B1= 65
  B2= 90
  K = 32
 ELSEIF(TXT.EQ.'u'.OR.TXT.EQ.'U')THEN
  B1= 97
  B2= 122
  K =-32
 ENDIF

 DO I=1,LEN_TRIM(STR)
  J=IACHAR(STR(I:I))
  IF(J.GE.B1.AND.J.LE.B2)J=J+K
  STR(I:I)=ACHAR(J)
 END DO

 RETURN
 END SUBROUTINE IMOD_UTL_CAP

 !###======================================================================
 FUNCTION IMOD_UTL_CAPF(STR,TXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256) :: IMOD_UTL_CAPF
 CHARACTER(LEN=*),INTENT(IN) :: TXT
 CHARACTER(LEN=*),INTENT(IN) :: STR

 IMOD_UTL_CAPF=STR
 CALL IMOD_UTL_CAP(IMOD_UTL_CAPF,TXT)

 END FUNCTION IMOD_UTL_CAPF

 !###======================================================================
 FUNCTION ITOS(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I
 CHARACTER(LEN=10)  :: TXT,ITOS

 WRITE(TXT,'(I10)') I
 ITOS=ADJUSTL(TXT) !TXT

 END FUNCTION ITOS

 !###======================================================================
 CHARACTER(LEN=10) FUNCTION IMOD_UTL_ITOS(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN)             :: I
 CHARACTER(LEN=10)              :: TXT
 CHARACTER(LEN=1),DIMENSION(10) :: A
 INTEGER                        :: J,K

 WRITE(TXT,'(I10)') I
 READ(TXT,'(10A1)') A(1:10)
 DO J=1,10
  IF(A(J).NE.' ')EXIT
 END DO
 K=10-J+1
 WRITE(IMOD_UTL_ITOS(1:K),'(10A1)') A(J:10)
 IMOD_UTL_ITOS(K+1:)=''

 END FUNCTION

 !###======================================================================
 FUNCTION IMOD_UTL_RTOS(X,F,NDEC)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NDEC
 REAL,INTENT(IN) :: X
 CHARACTER(LEN=1),INTENT(IN) :: F
 CHARACTER(LEN=15) :: TXT,FRM,IMOD_UTL_RTOS,CFN_SPRINTS_R
 INTEGER :: IOS

 IF(F.EQ.'*')THEN
  WRITE(TXT,*,IOSTAT=IOS) X
 ELSE
  WRITE(FRM,'(2A1,I2.2,A1,I2.2,A1)') '(',F,LEN(IMOD_UTL_RTOS),'.',NDEC,')'
  WRITE(TXT,FRM,IOSTAT=IOS) X
 ENDIF

 IF(IOS.NE.0)TXT='error'

 IMOD_UTL_RTOS=ADJUSTL(TXT)

 END FUNCTION IMOD_UTL_RTOS

 !###======================================================================
 SUBROUTINE IMOD_UTL_CLOSEUNITS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 LOGICAL :: LEX

 DO I=10,MAXFILES
  INQUIRE(UNIT=I,OPENED=LEX)
  IF(LEX)CLOSE(I)
 END DO

 END SUBROUTINE IMOD_UTL_CLOSEUNITS

 !###======================================================================
 LOGICAL FUNCTION IMOD_UTL_DIREXIST(DIRNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIRNAME
 INTEGER :: IU, IOS

 IMOD_UTL_DIREXIST=.FALSE.
 !## try to create a file in folder
 IU=IMOD_UTL_GETUNIT()
 OPEN(IU,FILE=TRIM(DIRNAME)//'\tmp.tmp#0#1#2',STATUS='UNKNOWN',IOSTAT=IOS)
 IF(IOS.EQ.0)THEN
  IMOD_UTL_DIREXIST=.TRUE.
  CLOSE(IU,STATUS='DELETE')
 ENDIF

 END FUNCTION IMOD_UTL_DIREXIST

 !###======================================================================
 SUBROUTINE IMOD_UTL_CREATEDIR(DIRNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIRNAME
 CHARACTER(LEN=256) :: STRING

 STRING=DIRNAME
 CALL IMOD_UTL_SWAPSLASH(STRING)

 !#check entire path first ...
 IF(IMOD_UTL_DIREXIST(STRING))RETURN

 CALL IMOD_UTL_IOSDIRMAKE('"'//TRIM(STRING)//'"')

 IF(.NOT.IMOD_UTL_DIREXIST(STRING))CALL IMOD_UTL_PRINTTEXT('Could not create directory: '//TRIM(STRING),2)

 END SUBROUTINE IMOD_UTL_CREATEDIR

 !###====================================================================
 SUBROUTINE IMOD_UTL_IOSDIRMAKE(DIR)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 CHARACTER(LEN=256) :: STRING

 STRING=DIR
 CALL IMOD_UTL_SWAPSLASH(STRING)
 IF(OS.EQ.1)STRING='mkdir '//TRIM(STRING)
 IF(OS.EQ.2)STRING='mkdir -p '//TRIM(STRING)
 CALL SYSTEM(STRING)

 END SUBROUTINE IMOD_UTL_IOSDIRMAKE

 !###====================================================================
 SUBROUTINE IMOD_UTL_IOSDELDIR(DIR)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 CHARACTER(LEN=256) :: STRING

 STRING=DIR
 CALL IMOD_UTL_SWAPSLASH(STRING)
 IF(OS.EQ.1)STRING='del /q '//TRIM(STRING)
 IF(OS.EQ.2)STRING='/bin/rm -rf '//TRIM(STRING)
 CALL SYSTEM(STRING)

 END SUBROUTINE IMOD_UTL_IOSDELDIR

 !###====================================================================
 INTEGER FUNCTION IMOD_UTL_IDATETOJDATE(IDATE,FNAME)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDATE
 CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: FNAME
 INTEGER            :: IY,IM,ID

 IMOD_UTL_IDATETOJDATE=0

 IY = IDATE / 10000
 IM = MOD( IDATE, 10000 ) / 100
 ID = MOD( IDATE, 100 )

 IF(IM.LE.0.OR.IM.GT.12)THEN
  CALL IMOD_UTL_PRINTTEXT('',0)
  CALL IMOD_UTL_PRINTTEXT('Error converting current date ['//TRIM(IMOD_UTL_ITOS(IDATE))//'] into julian data',0)
  IF(PRESENT(FNAME))CALL IMOD_UTL_PRINTTEXT('Occured in file: '//TRIM(FNAME),0)
  CALL IMOD_UTL_PRINTTEXT('',2)
 ENDIF
 IF(ID.LE.0.OR.ID.GT.IMOD_UTL_DATEDAYSINMONTH(IY,IM))THEN
  CALL IMOD_UTL_PRINTTEXT('',0)
  CALL IMOD_UTL_PRINTTEXT('Error converting current date ['//TRIM(IMOD_UTL_ITOS(IDATE))//'] into julian data',0)
  IF(PRESENT(FNAME))CALL IMOD_UTL_PRINTTEXT('Occured in file: '//TRIM(FNAME),0)
  CALL IMOD_UTL_PRINTTEXT('',2)
 ENDIF

 IMOD_UTL_IDATETOJDATE=IMOD_UTL_JD(IY,IM,ID)

 END FUNCTION

 !###====================================================================
 INTEGER FUNCTION IMOD_UTL_JDATETOIDATE(JDATE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: JDATE
 INTEGER            :: IY,IM,ID

 CALL IMOD_UTL_GDATE(JDATE,IY,IM,ID)
 IMOD_UTL_JDATETOIDATE=IY*10000+IM*100+ID

 END FUNCTION IMOD_UTL_JDATETOIDATE

 !###======================================================================
 FUNCTION IMOD_UTL_JDATETOGDATE(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I
 CHARACTER(LEN=10)  :: IMOD_UTL_JDATETOGDATE
 INTEGER :: IY,IM,ID

 CALL IMOD_UTL_GDATE(I,IY,IM,ID)
 IMOD_UTL_JDATETOGDATE=TRIM(IMOD_UTL_ITOS(ID))//'/'//TRIM(IMOD_UTL_ITOS(IM))//'/'//TRIM(IMOD_UTL_ITOS(IY))

 END FUNCTION IMOD_UTL_JDATETOGDATE

 !###======================================================================
 INTEGER FUNCTION IMOD_UTL_GDATETOJDATE(CDATE)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: CDATE
 INTEGER :: IY,IM,ID,I,J,MD
 INTEGER,DIMENSION(3) :: IOS

 IOS=1

 I=INDEX(CDATE,'/',.FALSE.)
 IF(I.GT.0)THEN
  READ(CDATE(1:I-1),*,IOSTAT=IOS(1)) ID
  J=INDEX(CDATE,'/',.TRUE.)
  IF(J.GT.0)THEN
   READ(CDATE(J+1:),*,IOSTAT=IOS(3)) IY
   IF(J-I.GT.0)READ(CDATE(I+1:J-1),*,IOSTAT=IOS(2)) IM
  ENDIF
 ENDIF

 !## initialize default value
 IMOD_UTL_GDATETOJDATE=0

 IM=MAX(1,MIN(12,IM))
 MD=IMOD_UTL_DATEDAYSINMONTH(IY,IM)
 ID=MAX(1,MIN(MD,ID))

 !## error reading dates
 IF(SUM(IOS).NE.0)RETURN

 IMOD_UTL_GDATETOJDATE=IMOD_UTL_JD(IY,IM,ID)

 END FUNCTION IMOD_UTL_GDATETOJDATE

 !###===================================================================
 INTEGER FUNCTION IMOD_UTL_DATEDAYSINMONTH(IY,IM)
 !###===================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IY,IM

 SELECT CASE (IM)
  CASE (2)
   IMOD_UTL_DATEDAYSINMONTH=28
   IF((MOD(IY,100).NE.0.AND.MOD(IY,4).EQ.0) .OR. MOD(IY,400).EQ.0)IMOD_UTL_DATEDAYSINMONTH=29
  CASE (1,3,5,7,8,10,12)
   IMOD_UTL_DATEDAYSINMONTH=31
  CASE DEFAULT
   IMOD_UTL_DATEDAYSINMONTH=30
 END SELECT

 END FUNCTION IMOD_UTL_DATEDAYSINMONTH

 !###===================================================================
 INTEGER FUNCTION IMOD_UTL_JD(YEAR,MONTH,DAY)
 !###===================================================================
 !EXTRACTED FROM THE INTERNET: http://aa.usno.navy.mil/faq/docs/JD_Formula.html
 !COMPUTES THE JULIAN DATE (JD) GIVEN A GREGORIAN CALENDAR DATE (YEAR,MONTH,DAY).
 !EXAMPLE: YEAR=1970,MONTH=1,DAY=1,JD=2440588
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: YEAR,MONTH,DAY
 INTEGER            :: I,J,K

 I =YEAR
 J =MONTH
 K =DAY
 IMOD_UTL_JD=K-32075+1461*(I+4800+(J-14)/12)/4+367*(J-2-(J-14)/12*12) &
             /12-3*((I+4900+(J-14)/12)/100)/4

 END FUNCTION IMOD_UTL_JD

 !###===================================================================
 SUBROUTINE IMOD_UTL_GDATE(JD,YEAR,MONTH,DAY)
 !###===================================================================
 !EXTRACTED FROM THE INTERNET: http://aa.usno.navy.mil/faq/docs/JD_Formula.html
 !COMPUTES THE GREGORIAN CALENDAR DATE (YEAR,MONTH,DAY) GIVEN THE JULIAN DATE (JD).
 IMPLICIT NONE
 INTEGER,INTENT(IN)  :: JD
 INTEGER,INTENT(OUT) :: YEAR,MONTH,DAY
 INTEGER             :: I,J,K,L,N

 L=JD+68569
 N=4*L/146097
 L=L-(146097*N+3)/4
 I=4000*(L+1)/1461001
 L=L-1461*I/4+31
 J=80*L/2447
 K=L-2447*J/80
 L=J/11
 J=J+2-12*L
 I=100*(N-49)+I+L

 YEAR =I
 MONTH=J
 DAY  =K

 END SUBROUTINE IMOD_UTL_GDATE

 !###===================================================================
 SUBROUTINE IMOD_UTL_MODEL1CHECKFNAME(FNAME,LU)
 !###===================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: LU
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 REAL :: X
 INTEGER :: IOS,I,J
 LOGICAL :: LEX

 IF(LEN_TRIM(FNAME).EQ.0)THEN
  IF(LU.EQ.0)CALL IMOD_UTL_PRINTTEXT('No file given',2)
  IF(LU.GT.0)THEN
   WRITE(LU,*) 'Error:'
   WRITE(LU,*) '  No file given'
  ENDIF
 ENDIF

 !get first non character
 I=0
 DO
  I=I+1
  J=ICHAR(FNAME(I:I))
  IF(J.GT.32)EXIT
 ENDDO

 X=IMOD_UTL_GETREAL(FNAME(I:),IOS)
 IF(IOS.NE.0)THEN

  INQUIRE(FILE=FNAME(I:),OPENED=LEX)
  IF(LEX)RETURN

  INQUIRE(FILE=FNAME(I:),EXIST=LEX)
  IF(.NOT.LEX)THEN
   IF(LU.EQ.0)CALL IMOD_UTL_PRINTTEXT('File '//TRIM(FNAME(I:))//' does not exist !',2)
   IF(LU.GT.0)THEN
    WRITE(LU,*) 'Error:'
    WRITE(LU,*)    TRIM(FNAME(I:))//' does not exist!'
   ENDIF
  ENDIF

 ENDIF

 END SUBROUTINE

 !###===================================================================
 CHARACTER(LEN=256) FUNCTION IMOD_UTL_GETFNAME(LINE)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: LINE
 INTEGER :: I,J,K

 K=39
 I=INDEX(LINE,CHAR(K),.FALSE.)  !## '-tje
 IF(I.EQ.0)THEN
  K=34
  I=INDEX(LINE,CHAR(K),.FALSE.) !## "-tje
 ENDIF
 !## quotes found, find other, to be sure it is consistent
 IF(I.GT.0)THEN
  J=INDEX(LINE,CHAR(K),.TRUE.)
  IF(I.EQ.J)THEN
   CALL IMOD_UTL_PRINTTEXT('',0)
   CALL IMOD_UTL_PRINTTEXT('Missing second quote '//CHAR(K)//' in line:',0)
   CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),0)
   CALL IMOD_UTL_PRINTTEXT('',2)
  ENDIF
  IMOD_UTL_GETFNAME=LINE(I+1:J-1)
 ELSE
  !## search for comma's, backward
  I=INDEX(TRIM(LINE),',',.TRUE.)
  J=INDEX(TRIM(LINE),' ',.TRUE.)
  IMOD_UTL_GETFNAME=LINE(MAX(I+1,J+1):) !J-1)
 ENDIF

 END FUNCTION IMOD_UTL_GETFNAME

 !###===================================================================
 REAL FUNCTION IMOD_UTL_GETREAL(LINE,IOS)
 !###===================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IOS
 CHARACTER(LEN=*),INTENT(IN) :: LINE
 INTEGER :: ITYPE,CFN_DETERM_TYPE

 READ(LINE,*,IOSTAT=IOS) IMOD_UTL_GETREAL
 IF(IOS.NE.0)IMOD_UTL_GETREAL=0.0

 END FUNCTION IMOD_UTL_GETREAL

 !###===================================================================
 INTEGER FUNCTION IMOD_UTL_GETIDFTYPE(FNAME)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: I,RDRS_TYPE,IU

 IMOD_UTL_GETIDFTYPE=0

 !CALL OPENIDF(IU,FNAME,'R',4)
 READ(IU,REC=1) I
 IF(I.EQ.1271)IMOD_UTL_GETIDFTYPE=1
 CLOSE(IU)

 END FUNCTION IMOD_UTL_GETIDFTYPE

 !###===================================================================
 SUBROUTINE IMOD_UTL_OPENIDF(IU,FNAME,ACT,NREC)
 !###===================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: IU
 INTEGER,INTENT(IN) :: NREC
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=*),INTENT(IN) :: ACT
 CHARACTER(LEN=75) :: MESSAGE
 INTEGER :: IOS,OSD_OPEN2
 INTEGER :: CFN_GETLUN

 IU=0
 IF(ACT.NE.'R'.AND.ACT.NE.'r'.AND. &   !read
    ACT.NE.'W'.AND.ACT.EQ.'w')RETURN   !write

 IU     = CFN_GETLUN(10,99)
 MESSAGE=''

 IF(ACT.EQ.'W'.OR.ACT.EQ.'w')THEN
  MESSAGE='REPLACE,UNFORMATTED,DIRECT'
 ELSEIF(ACT.EQ.'R'.OR.ACT.EQ.'r')THEN
  CALL IMOD_UTL_MODEL1CHECKFNAME(FNAME,0)
  MESSAGE='READONLY,SHARED,OLD,UNFORMATTED,DIRECT'
 ENDIF

 IOS=OSD_OPEN2(IU,NREC,FNAME,TRIM(MESSAGE))  !unitnumber,recl,fname,string(options)

 !#error occured opening idf-file
 IF(IOS.NE.0)THEN
!  WRITE(*,*) IOS
  CALL IMOD_UTL_PRINTTEXT('Can not open indexed file: '//TRIM(FNAME),2)
 ENDIF

 END SUBROUTINE

 !###===================================================================
 SUBROUTINE IMOD_UTL_OPENASC(IU,FNAME,ACT)
 !###===================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: IU
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=*),INTENT(IN) :: ACT
 CHARACTER(LEN=75) :: MESSAGE
 INTEGER :: IOS,OSD_OPEN2
 INTEGER :: CFN_GETLUN

 IU=0
 IF(ACT.NE.'R'.AND.ACT.NE.'r'.AND. &   !read
    ACT.NE.'W'.AND.ACT.NE.'w'.AND. &   !write
    ACT.NE.'A'.AND.ACT.NE.'a')RETURN   !append

 !read,denywrite -> 'unknown,formatted,readonly,shared,append'  !rest default

 IU     = CFN_GETLUN(10,99)
 MESSAGE=''

 !#dos
 IF(ACT.EQ.'W'.OR.ACT.EQ.'w')THEN
  MESSAGE='WRITE,UNKNOWN,FORMATTED'
 ELSEIF(ACT.EQ.'R'.OR.ACT.EQ.'r')THEN
  CALL IMOD_UTL_MODEL1CHECKFNAME(FNAME,0)
  MESSAGE='READONLY,SHARED,OLD,FORMATTED'
 ELSEIF(ACT.EQ.'A'.OR.ACT.EQ.'a')THEN
  CALL IMOD_UTL_MODEL1CHECKFNAME(FNAME,0)
  MESSAGE='OLD,FORMATTED,APPEND'
 ENDIF

 IOS=OSD_OPEN2(IU,0,FNAME,TRIM(MESSAGE))  !unitnumber,recl,fname,string(options)

 !#error occured opening idf-file
 IF(IOS.NE.0)THEN
!  WRITE(*,*) 'IOS=',IOS
  CALL IMOD_UTL_PRINTTEXT('Can not open sequential file: ['//TRIM(FNAME)//']',2)
 ENDIF

 END SUBROUTINE

 !###====================================================
 SUBROUTINE IMOD_UTL_GETMED(X,NX,NODATA,PERC,NPERC,MX,XMED)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NX,NPERC            !## size array,number of percentiles to be comp.
 INTEGER,INTENT(OUT) :: MX                 !## number of values ne nodata
 REAL,INTENT(IN),DIMENSION(NPERC) :: PERC  !## percentile 0-100%
 REAL,INTENT(OUT),DIMENSION(NPERC) :: XMED !## yielding percentile(s)
 REAL,INTENT(IN) :: NODATA                 !## nodata value !,PERC
 REAL,DIMENSION(NX),INTENT(INOUT) :: X     !## array
 INTEGER :: I,J,IP
 REAL :: FRAC

 XMED=NODATA

 IF(NX.LE.0)RETURN

 !## only one sample
 IF(NX.EQ.1)THEN
  XMED=X(1)
  MX  =1
  RETURN
 ENDIF

 !## do not include nodata values for median-computation
 MX=0
 DO I=1,NX
  IF(X(I).NE.NODATA)THEN
   MX   =MX+1
   X(MX)=X(I)
  ENDIF
 END DO

 IF(MX.LE.0)RETURN

 !## sort data, excl. nodata values
 IF(MX.LE.100)THEN
  CALL IMOD_UTL_SHELLSORT(MX,X)
 ELSE
  CALL IMOD_UTL_QKSORT(MX,MX,X)
 ENDIF

 DO IP=1,NPERC

  IF(PERC(IP).LE.0.0)THEN
   XMED(IP)=X(1)
  ELSEIF(PERC(IP).GE.100.0)THEN
   XMED(IP)=X(MX)
  ELSE
   FRAC=1.0/(PERC(IP)/100.0)

   IF(MOD(REAL(MX),FRAC).EQ.0.0)THEN
    I=INT(REAL(MX)/FRAC)
    XMED(IP)=X(I)
   ELSE
    I=MAX(1,INT(REAL(MX)/FRAC))
    J=MIN(I+1,MX)
    XMED(IP)=(X(I)+X(J))/2.0
   ENDIF
  ENDIF
 ENDDO

 END SUBROUTINE IMOD_UTL_GETMED

!C====================================================
SUBROUTINE IMOD_UTL_QKSORT(NDIM,N,ARR)
!====================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: N,NDIM
REAL,INTENT(INOUT),DIMENSION(NDIM) :: ARR
INTEGER :: M,NSTACK
PARAMETER (M=7,NSTACK=50)
INTEGER :: I,IR,J,JSTACK,K,L,ISTACK(NSTACK)
REAL :: A,TEMP

JSTACK=0
L=1
IR=N
1 IF(IR-L.LT.M)THEN
 DO J=L+1,IR
  A=ARR(J)
  DO I=J-1,1,-1
   IF(ARR(I).LE.A)GOTO 2
   ARR(I+1)=ARR(I)
  ENDDO
  I=0
2   ARR(I+1)=A
 ENDDO
 IF(JSTACK.EQ.0)RETURN
 IR=ISTACK(JSTACK)
 L=ISTACK(JSTACK-1)
 JSTACK=JSTACK-2
ELSE
 K=(L+IR)/2
 TEMP=ARR(K)
 ARR(K)=ARR(L+1)
 ARR(L+1)=TEMP
 IF(ARR(L+1).GT.ARR(IR))THEN
  TEMP=ARR(L+1)
  ARR(L+1)=ARR(IR)
  ARR(IR)=TEMP
 ENDIF
 IF(ARR(L).GT.ARR(IR))THEN
  TEMP=ARR(L)
  ARR(L)=ARR(IR)
  ARR(IR)=TEMP
 ENDIF
 IF(ARR(L+1).GT.ARR(L))THEN
  TEMP=ARR(L+1)
  ARR(L+1)=ARR(L)
  ARR(L)=TEMP
 ENDIF
 I=L+1
 J=IR
 A=ARR(L)
3  CONTINUE
 I=I+1
 IF(ARR(I).LT.A)GOTO 3
4  CONTINUE
 J=J-1
 IF(ARR(J).GT.A)GOTO 4
 IF(J.LT.I)GOTO 5
 TEMP=ARR(I)
 ARR(I)=ARR(J)
 ARR(J)=TEMP
 GOTO 3
5  ARR(L)=ARR(J)
 ARR(J)=A
 JSTACK=JSTACK+2
 IF(JSTACK.GT.NSTACK)PAUSE 'NSTACK TOO SMALL'
 IF(IR-I+1.GT.J-L)THEN
  ISTACK(JSTACK)=IR
  ISTACK(JSTACK-1)=I
  IR=J-1
 ELSE
  ISTACK(JSTACK)=J-1
  ISTACK(JSTACK-1)=L
  L=I
 ENDIF
ENDIF
GOTO 1

END SUBROUTINE IMOD_UTL_QKSORT

 !###======================================================================
 SUBROUTINE IMOD_UTL_QKSORT2(ARR,BRR,NDIM,N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NDIM,N
 REAL,INTENT(INOUT),DIMENSION(NDIM) :: ARR,BRR
 INTEGER   M,NSTACK
 PARAMETER (M=7,NSTACK=50)
 INTEGER   I,IR,J,JSTACK,K,L,ISTACK(NSTACK)
 REAL      A,B,ATEMP,BTEMP

 JSTACK=0
 L=1
 IR=N
1  IF(IR-L.LT.M)THEN
  DO J=L+1,IR
   A=ARR(J)
   B=BRR(J)
   DO I=J-1,1,-1
    IF(ARR(I).LE.A)GOTO 2
    ARR(I+1)=ARR(I)
    BRR(I+1)=BRR(I)
   ENDDO
   I=0
2  ARR(I+1)=A
   BRR(I+1)=B
  ENDDO
  IF(JSTACK.EQ.0)RETURN
  IR=ISTACK(JSTACK)
  L =ISTACK(JSTACK-1)
  JSTACK=JSTACK-2
 ELSE
  K=(L+IR)/2
  ATEMP   =ARR(K)
  BTEMP   =BRR(K)
  ARR(K)  =ARR(L+1)
  BRR(K)  =BRR(L+1)
  ARR(L+1)=ATEMP
  BRR(L+1)=BTEMP
  IF(ARR(L+1).GT.ARR(IR))THEN
   ATEMP   =ARR(L+1)
   BTEMP   =BRR(L+1)
   ARR(L+1)=ARR(IR)
   BRR(L+1)=BRR(IR)
   ARR(IR)=ATEMP
   BRR(IR)=BTEMP
  ENDIF
  IF(ARR(L).GT.ARR(IR))THEN
   ATEMP  =ARR(L)
   BTEMP  =BRR(L)
   ARR(L) =ARR(IR)
   BRR(L) =BRR(IR)
   ARR(IR)=ATEMP
   BRR(IR)=BTEMP
  ENDIF
  IF(ARR(L+1).GT.ARR(L))THEN
   ATEMP   =ARR(L+1)
   BTEMP   =BRR(L+1)
   ARR(L+1)=ARR(L)
   BRR(L+1)=BRR(L)
   ARR(L)  =ATEMP
   BRR(L)  =BTEMP
  ENDIF
  I=L+1
  J=IR
  A=ARR(L)
  B=BRR(L)
3   CONTINUE
  I=I+1
  IF(ARR(I).LT.A)GOTO 3
4   CONTINUE
  J=J-1
  IF(ARR(J).GT.A)GOTO 4
  IF(J.LT.I)GOTO 5
  ATEMP =ARR(I)
  BTEMP =BRR(I)
  ARR(I)=ARR(J)
  BRR(I)=BRR(J)
  ARR(J)=ATEMP
  BRR(J)=BTEMP
  GOTO 3
5   ARR(L)=ARR(J)
  BRR(L)=BRR(J)
  ARR(J)=A
  BRR(J)=B
  JSTACK=JSTACK+2
  IF(JSTACK.GT.NSTACK)PAUSE 'NSTACK TOO SMALL'
  IF(IR-I+1.GT.J-L)THEN
   ISTACK(JSTACK)=IR
   ISTACK(JSTACK-1)=I
   IR=J-1
  ELSE
   ISTACK(JSTACK)=J-1
   ISTACK(JSTACK-1)=L
   L=I
  ENDIF
 ENDIF
 GOTO 1

 END SUBROUTINE IMOD_UTL_QKSORT2

 !###====================================================
 SUBROUTINE IMOD_UTL_SHELLSORT(N,A)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL,DIMENSION(N),INTENT(INOUT) :: A
 INTEGER            :: I,J,INC
 REAL               :: V

 INC=1
 1 INC=3*INC+1
 IF(INC.LE.N)GOTO 1
 2 CONTINUE
  INC=INC/3
  DO I=INC+1,N
   V=A(I)
   J=I
 3 IF(A(J-INC).GT.V)THEN
    A(J)=A(J-INC)
    J=J-INC
    IF(J.LE.INC)GOTO 4
    GOTO 3
   ENDIF
 4  A(J)=V
  END DO
 IF(INC.GT.1)GOTO 2

 END SUBROUTINE

 !###==================================================================
 SUBROUTINE IMOD_UTL_POL1INTMAIN(NCOL,NROW,NPC,NPR,XCRD,YCRD,ZCRD,DELR,DELC,X, &
                        IINT,NODATA)
 !###==================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW,NPC,NPR,IINT
 REAL,INTENT(IN) :: NODATA
 REAL,DIMENSION(NPC),INTENT(IN) :: XCRD       !fixed point x-coordinates
 REAL,DIMENSION(NPR),INTENT(IN) :: YCRD       !fixed point y-coordinates
 REAL,DIMENSION(NPC,NPR),INTENT(IN) :: ZCRD   !fixed point values
 REAL,DIMENSION(0:NCOL),INTENT(IN) :: DELR
 REAL,DIMENSION(0:NROW),INTENT(IN) :: DELC
 REAL,DIMENSION(NCOL,NROW),INTENT(INOUT) :: X
 REAL :: Y,DY,X1,X2
 INTEGER :: NMAX,IROW,ICOL !,ITYPE
 !TYPE(WIN_MESSAGE) :: MESSAGE
 REAL,ALLOCATABLE,DIMENSION(:) :: C,D,YMTMP,YNTMP

 NMAX=MAX(NPR,NPC)
 IF(ALLOCATED(C))DEALLOCATE(C)
 IF(ALLOCATED(D))DEALLOCATE(D)
 IF(ALLOCATED(YMTMP))DEALLOCATE(YMTMP)
 IF(ALLOCATED(YNTMP))DEALLOCATE(YNTMP)
 ALLOCATE(C(NMAX),D(NMAX),YNTMP(NPR),YMTMP(NPC))

 !loop over all points!
 DO IROW=1,NROW
  DO ICOL=1,NCOL
   X1=(DELR(ICOL-1)+DELR(ICOL))/2.0
   X2=(DELC(IROW-1)+DELC(IROW))/2.0
   CALL IMOD_UTL_POL2DINT(XCRD,YCRD,ZCRD,C,D,NMAX,YMTMP,YNTMP,NPC,NPR,X1,X2,Y,DY,IINT,NODATA)
    X(ICOL,IROW)=Y
  ENDDO
 ENDDO

 END SUBROUTINE IMOD_UTL_POL1INTMAIN

 !###==================================================================
 SUBROUTINE IMOD_UTL_POL2DINT(XCRD,YCRD,ZCRD,C,D,NMAX,YMTMP,YNTMP,NPC,NPR,XINT,&
                     YINT,Y,DY,IINT,NODATA)
 !###==================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NPC,NPR,NMAX,IINT
 REAL,INTENT(IN) :: NODATA
 REAL,DIMENSION(NPC),INTENT(IN) :: XCRD
 REAL,DIMENSION(NPR),INTENT(IN) :: YCRD
 REAL,DIMENSION(NPC,NPR) :: ZCRD
 REAL,INTENT(IN) :: XINT,YINT
 REAL,INTENT(OUT) :: DY,Y
 REAL,DIMENSION(NPR),INTENT(INOUT) :: YNTMP
 REAL,DIMENSION(NPC),INTENT(INOUT) :: YMTMP
 REAL,DIMENSION(NMAX),INTENT(INOUT) :: C,D
 INTEGER :: J,K,XPOS,XPOS1,XPOS2,DXPOS,YPOS,YPOS1,YPOS2,DYPOS,DPOS,IDATA,INODATA
 REAL :: YMEAN

 CALL IMOD_UTL_POL1LOCATER(XCRD,NPC,XINT,XPOS)
 CALL IMOD_UTL_POL1LOCATER(YCRD,NPR,YINT,YPOS)

 DPOS =INT(SQRT(REAL(IINT)))   !iint=4,dpos=2; iint=16,dpos=4
 DPOS =DPOS/2

 XPOS1=MAX(1,XPOS-(DPOS-1))
 XPOS2=MIN(XPOS+DPOS,NPC)
 DXPOS=(XPOS2-XPOS1)+1

 YPOS1=MAX(1,YPOS-(DPOS-1))
 YPOS2=MIN(YPOS+DPOS,NPR)
 DYPOS=(YPOS2-YPOS1)+1

 IDATA=0; INODATA=0; YMEAN = 0.
 JL: DO J=XPOS1,XPOS2
  DO K=YPOS1,YPOS2
   YNTMP(K)=ZCRD(J,K)
   IF(YNTMP(K).NE.NODATA)THEN
    YMEAN = YMEAN + YNTMP(K)
    IDATA=IDATA+1
   ENDIF
  END DO
 END DO JL
 IF (IDATA.GT.0) THEN
   YMEAN = YMEAN/IDATA
   JLOOP: DO J=XPOS1,XPOS2
    DO K=YPOS1,YPOS2
     YNTMP(K)=ZCRD(J,K)
     IF(YNTMP(K).EQ.NODATA)THEN
      YNTMP(K)=YMEAN
     ENDIF
    END DO
    CALL IMOD_UTL_POL1DINT(YCRD(YPOS1),YNTMP(YPOS1),C,D,DYPOS,NMAX,YINT,YMTMP(J),DY)
   END DO JLOOP
 ELSE
    INODATA=1
 END IF

 IF(INODATA.EQ.1)THEN
  Y =NODATA
  DY=0.0
 ELSE
  CALL IMOD_UTL_POL1DINT(XCRD(XPOS1),YMTMP(XPOS1),C,D,DXPOS,NMAX,XINT,Y,DY)
 ENDIF

 END SUBROUTINE IMOD_UTL_POL2DINT

 !###==================================================================
 SUBROUTINE IMOD_UTL_POL1DINT(XA,YA,C,D,NPR,NMAX,X,Y,DY)
 !###==================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NPR,NMAX
 REAL,DIMENSION(NPR),INTENT(IN) :: XA,YA
 REAL,DIMENSION(NMAX),INTENT(INOUT) :: C,D
 REAL,INTENT(IN) :: X
 REAL,INTENT(OUT) :: DY,Y
 INTEGER :: I,M,NS
 REAL :: DEN,DIF,DIFT,HO,HP,W

 NS =1
 DIF=ABS(X-XA(1))

 DO I=1,NPR
  DIFT=ABS(X-XA(I))
  IF(DIFT.LT.DIF)THEN
   NS =I
   DIF=DIFT
  ENDIF
  C(I)=YA(I)
  D(I)=YA(I)
 END DO

 Y =YA(NS)
 NS=NS-1

 DO M=1,NPR-1
  DO I=1,NPR-M
   HO =XA(I)-X
   HP =XA(I+M)-X
   W  =C(I+1)-D(I)
   DEN=HO-HP
   IF(DEN.EQ.0.0)PAUSE 'FAILURE IN POLINT' !occurs whenever two xa(i) are almost the same
   DEN=W/DEN
   D(I)=HP*DEN
   C(I)=HO*DEN
  END DO
  IF(2*NS.LT.NPR-M)THEN
   DY=C(NS+1)
  ELSE
   DY=D(NS)
   NS=NS-1
  ENDIF
  Y=Y+DY
 END DO

 END SUBROUTINE IMOD_UTL_POL1DINT

 !###==================================================================
 SUBROUTINE IMOD_UTL_POL1LOCATER(XX,N,X,J)
 !###==================================================================
 !return position such that x is within xx(j) and xx(j+1)
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: J
 INTEGER,INTENT(IN) :: N
 REAL,INTENT(IN) :: X
 REAL,INTENT(IN),DIMENSION(N) :: XX
 INTEGER :: JL,JM,JU

 JL=0
 JU=N+1
 DO
  IF(JU-JL.GT.1)THEN
   JM=(JU+JL)/2
   IF((XX(N).GT.XX(1)).EQV.(X.GT.XX(JM)))THEN
    JL=JM
   ELSE
    JU=JM
   ENDIF
  ELSE
   EXIT
  ENDIF
 ENDDO

 J=JL

 END SUBROUTINE IMOD_UTL_POL1LOCATER

 !###==================================================================
 SUBROUTINE IMOD_UTL_POL1LOCATED(XX,N,X,J)
 !###==================================================================
 !return position such that x is within xx(j) and xx(j+1)
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: J
 INTEGER,INTENT(IN) :: N
 DOUBLE PRECISION,INTENT(IN) :: X
 REAL,INTENT(IN),DIMENSION(N) :: XX
 INTEGER :: JL,JM,JU

 JL=0
 JU=N+1
 DO
  IF(JU-JL.GT.1)THEN
   JM=(JU+JL)/2
   IF((XX(N).GT.XX(1)).EQV.(X.GT.XX(JM)))THEN
    JL=JM
   ELSE
    JU=JM
   ENDIF
  ELSE
   EXIT
  ENDIF
 ENDDO

 J=JL

 END SUBROUTINE IMOD_UTL_POL1LOCATED

 !###===================================================================
 SUBROUTINE IMOD_UTL_OSSYSTEM()
 !###===================================================================
 IMPLICIT NONE
 INTEGER :: VOS,OSD_GET_OS

! !#capsim/metaswap/none active?

!#get operating system
 VOS=OSD_GET_OS()
 OS =0
 IF(VOS.EQ.3)OS=1
 IF(VOS.EQ.2)OS=2
 IF(VOS.EQ.4)OS=2

 OS=1

 SELECT CASE (OS)
  !## dos
  CASE (1)
   OSN(OS)   ='DOS-mode'
  !## linux/unix (beowulf)
  CASE (2)
   OSN(OS)   ='UNIX/LINUX-mode'
  !## something different
  CASE DEFAULT
   CALL IMOD_UTL_PRINTTEXT('No proper operating system!',2)
 END SELECT

 END SUBROUTINE IMOD_UTL_OSSYSTEM

 !###===================================================================
 SUBROUTINE IMOD_UTL_PRINTTEXT(TXT,TXTTYPE,IU)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: TXT
 INTEGER,INTENT(IN) :: TXTTYPE
 INTEGER,INTENT(IN),OPTIONAL :: IU

 LOGICAL :: LEX

  SELECT CASE (TXTTYPE)
  !## file
  CASE (0)
   WRITE(*,'(A)') TRIM(TXT)
  !## information
  CASE (-1,1)
   WRITE(*,'(A)') TRIM(TXT)
   !IF(IFLAG(1).EQ.1)PAUSE
  !## error
  CASE (2)
   WRITE(*,'(A)')
   WRITE(*,'(A)') 'Error occured!'
   WRITE(*,'(A)') TRIM(TXT)
   IF(IFLAG(1).EQ.1)PAUSE
  CASE DEFAULT
   WRITE(*,'(A)') TRIM(TXT)
   !IF(IFLAG(1).EQ.1)PAUSE
  END SELECT

 IF(TXTTYPE.EQ.-1)THEN
  IF (PRESENT(IU)) THEN
   INQUIRE(UNIT=IU,OPENED=LEX)
   IF(LEX)THEN
    WRITE(IU,'(A)') TRIM(TXT)
    CALL FLUSH(IU)
   END IF
  END IF
 END IF

 IF(TXTTYPE.EQ.2)THEN
   CALL EXIT(1)
 ENDIF

 END SUBROUTINE IMOD_UTL_PRINTTEXT

 !###====================================================================
 REAL FUNCTION IMOD_UTL_GETMOSTFREQ(FREQ,MFREQ,NFREQ)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: MFREQ,NFREQ
 REAL,DIMENSION(MFREQ),INTENT(IN) :: FREQ
 INTEGER :: I,MI,NI

 NI=1  !number of unique
 MI=NI !max. number of unique
 IMOD_UTL_GETMOSTFREQ=FREQ(NI)

 DO I=2,NFREQ
  IF(FREQ(I).NE.FREQ(I-1))THEN
   IF(NI.GT.MI)THEN
    IMOD_UTL_GETMOSTFREQ=FREQ(I-1)
    MI=NI
   ENDIF
   NI=1
  ELSE
   NI=NI+1
  ENDIF
 END DO
 !test final
 IF(NI.GT.MI) IMOD_UTL_GETMOSTFREQ=FREQ(NFREQ)

 END FUNCTION IMOD_UTL_GETMOSTFREQ

 !###======================================================================
 INTEGER FUNCTION GETUNIT()
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: LEX

 DO GETUNIT=20,5000
  INQUIRE(UNIT=GETUNIT,OPENED=LEX)
  IF(.NOT.LEX)EXIT
 END DO

 IF(GETUNIT.GT.5000)THEN
  WRITE(*,*) 'Can not open more than 500 files simultaneously!'
  GETUNIT=0
 ENDIF

 END FUNCTION GETUNIT

 !###====================================================================
 INTEGER FUNCTION JD(YEAR,MONTH,DAY)
 !###====================================================================
 !EXTRACTED FROM THE INTERNET: http://aa.usno.navy.mil/faq/docs/JD_Formula.html
 !COMPUTES THE JULIAN DATE (JD) GIVEN A GREGORIAN CALENDAR DATE (YEAR,MONTH,DAY).
 !EXAMPLE: YEAR=1970,MONTH=1,DAY=1,JD=2440588
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: YEAR,MONTH,DAY
 INTEGER            :: I,J,K

 I =YEAR
 J =MONTH
 K =DAY
 JD=K-32075+1461*(I+4800+(J-14)/12)/4+367*(J-2-(J-14)/12*12) &
    /12-3*((I+4900+(J-14)/12)/100)/4

 END FUNCTION JD

 !###======================================================================
 INTEGER FUNCTION UTL_IDFGETDATE(IDFNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 INTEGER :: IOS
 INTEGER :: I,J,N!,IY,IM,ID

 !## initially no data
 UTL_IDFGETDATE=0

 !## find 8 numbers after eachother ...
 N=0
 !## start after last "\"-symbol
 DO I=INDEX(IDFNAME,'\',.TRUE.)+1,LEN_TRIM(IDFNAME)
  SELECT CASE (ICHAR(IDFNAME(I:I)))
   CASE (48:57)
    !## count numbers
    N=N+1
    !## stop if 8
    IF(N.EQ.8)EXIT
    !## mark first position
    IF(N.EQ.1)J=I
   CASE DEFAULT
    N=0
  END SELECT
 END DO

 IF(N.LT.8)RETURN

 READ(IDFNAME(J:),'(I8)',IOSTAT=IOS) UTL_IDFGETDATE
 IF(IOS.NE.0)UTL_IDFGETDATE=0

 END FUNCTION UTL_IDFGETDATE

 !###====================================================================
 INTEGER FUNCTION IDATETOJDATE(IDATE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDATE
 INTEGER :: IY,IM,ID

 CALL IMOD_UTL_IDATETOGDATE(IDATE,IY,IM,ID)
 IDATETOJDATE=JD(IY,IM,ID)

 END FUNCTION IDATETOJDATE

 !###====================================================================
 SUBROUTINE IMOD_UTL_IDATETOGDATE(IDATE,IY,IM,ID)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDATE
 INTEGER,INTENT(OUT) :: IY,IM,ID
 CHARACTER(LEN=8)   :: CDATE

 IY = IDATE / 10000
 IM = MOD( IDATE, 10000 ) / 100
 ID = MOD( IDATE, 100 )

 END SUBROUTINE IMOD_UTL_IDATETOGDATE

 !###======================================================================
 SUBROUTINE IMOD_UTL_FILLARRAY(IP,NP,B)
 !###======================================================================
 !# read binair number (e.g. 256) and returns array (/1,0,0,1,0,0,1/)
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NP
 INTEGER,INTENT(IN) :: B
 INTEGER,INTENT(OUT),DIMENSION(NP) :: IP
 INTEGER :: I,BB

 IP=0
 BB=B
 DO I=1,NP
  IP(I)=MOD(BB,2)
  BB=BB/2
 END DO

 !## make sure results are only 0/1 values
 DO I=1,NP
  IF(IP(I).LT.0.OR.IP(I).GT.1)IP(I)=0
 END DO

 END SUBROUTINE IMOD_UTL_FILLARRAY

 !###======================================================================
 SUBROUTINE IMOD_UTL_READARRAY(IP,NP,B)
 !###======================================================================
 !# write a binair-number given an array (/1,0,0,4,0,0,7/)
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NP
 INTEGER,INTENT(OUT) :: B
 INTEGER,INTENT(IN),DIMENSION(NP) :: IP
 INTEGER :: I,J

 B=0
 DO I=1,NP
  J=MAX(0,MIN(IP(I),1))
  B=B+(J*(2**(I-1)))
 END DO

 END SUBROUTINE IMOD_UTL_READARRAY

!###====================================================================
 FUNCTION IMOD_UTL_SUBST(FNAME,SUB1,SUB2)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: SUB1,SUB2
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: I,J
 CHARACTER(LEN=256) :: IMOD_UTL_SUBST

 IMOD_UTL_SUBST=FNAME

 I=INDEX(FNAME,SUB1)
 IF(I.EQ.0)RETURN
 I=I-1
 J=I+LEN_TRIM(SUB1)+1

 IMOD_UTL_SUBST=FNAME(:I)//TRIM(SUB2)//FNAME(J:)

 END FUNCTION IMOD_UTL_SUBST

 !###====================================================================
 SUBROUTINE IMOD_UTL_READIPF(SDATE,EDATE,TTIME,QSORT,Q,FNAME,ISS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: SDATE,EDATE,TTIME
 REAL,INTENT(INOUT),DIMENSION(TTIME) :: QSORT
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 REAL,DIMENSION(1),INTENT(OUT) :: Q
 INTEGER, INTENT(IN) :: ISS
 INTEGER :: IR,I,I1,I2,IU,NR,NC,IDATE,JDATE,NDATE,NAJ,N,IOS
 REAL :: QQ,FRAC,Q1
 CHARACTER(LEN=8),DIMENSION(2) :: ATTRIB
 REAL,DIMENSION(2) :: NODATA

 Q=0.0

 !## open textfiles with pump information
 IU=IMOD_UTL_GETUNIT()
 CALL IMOD_UTL_OPENASC(IU,FNAME,'R')
 READ(IU,*,IOSTAT=IOS) NR
 IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Can not read number of rows in '//TRIM(FNAME),2)

 IF(NR.GT.0.0)THEN
  READ(IU,*,IOSTAT=IOS) NC
  IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Can not read number of columns in '//TRIM(FNAME),2)
  IF(NC.GT.2)CALL IMOD_UTL_PRINTTEXT('Max. # columns in txt file is 2, pck2rpwel in imod_pck.f90',2)

  DO I=1,NC
   READ(IU,*,IOSTAT=IOS) ATTRIB(I),NODATA(I)
   IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Can not read attribute '//TRIM(IMOD_UTL_ITOS(I))//' in '//TRIM(FNAME),2)
  END DO

  IF(ISS.EQ.1)QSORT=0.0
  IF(ISS.EQ.2)QSORT=NODATA(2)!-999.99
  I1=1

  DO IR=1,NR

   IF(IR.EQ.1.OR.ISS.EQ.1)THEN
    READ(IU,*) IDATE,QQ
   ELSE
    QQ   =Q1
    IDATE=JDATE
   ENDIF

 !#don't bother for steady-state, take the mean!
   IF(ISS.EQ.1)THEN
    QSORT(1)=QSORT(1)+QQ
   ELSE

    !## edate=end date of current simulation period
    NDATE=EDATE
    IF(IR.LT.NR)THEN
     READ(IU,*) NDATE,Q1
     JDATE=NDATE
     NDATE=IMOD_UTL_IDATETOJDATE(NDATE,TRIM(FNAME)) !## fname=optional for error message
    ENDIF
    !## ndate is min of end date in txt file or simulation period
    NDATE=MIN(NDATE,EDATE)

    !## is begin date read from txt file
    IDATE=IMOD_UTL_IDATETOJDATE(IDATE,TRIM(FNAME))  !## fname=optional for error message

    !## stop searchin for data, outside modeling window!
    IF(IDATE.GT.EDATE)EXIT

    !## within modeling window
    IF(NDATE.GT.SDATE)THEN !.AND. &

     !### defintions ($ time window current stressperiod)
     !  $        |---------|         $
     !sdate    idate     ndate     edate

     N=NDATE-SDATE
     !## if startingdate (read from txt file) greater than start date of current stressperiod
     IF(IDATE.GT.SDATE)N=N-(IDATE-SDATE)
     I2=I1+N-1

     QSORT(I1:I2)=QQ

     I1=I2+1

    ENDIF

   ENDIF
  END DO

  IF(ISS.EQ.1)THEN
 !##determine for each period appropriate attribute term
   Q   =QSORT(1)/REAL(NR)
 !#determine for each period appropriate q-median term
  ELSEIF(ISS.EQ.2)THEN
 !  Q   =IMOD_UTL_GETMED_OLD(QSORT,TTIME,NODATA(2),NAJ)
   CALL IMOD_UTL_GETMED(QSORT,TTIME,NODATA(2),(/50.0/),1,NAJ,Q(1))
   FRAC=REAL(NAJ)/REAL(TTIME)
   Q   =Q*FRAC
  ENDIF

 ENDIF

 CLOSE(IU)

 RETURN
 END SUBROUTINE IMOD_UTL_READIPF

 !###======================================================================
 LOGICAL FUNCTION IMOD_UTL_DIRINFO(DIR,WC,LISTNAME,FT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,WC,FT
 CHARACTER(LEN=*),INTENT(OUT),DIMENSION(:),POINTER :: LISTNAME
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: C_LISTNAME
 CHARACTER(LEN=256) :: LINE
 INTEGER :: IU,I,J,N,IOS
 LOGICAL :: LEX

 IMOD_UTL_DIRINFO=.FALSE.

 IF(LEN(C_LISTNAME).LT.LEN(LISTNAME))CALL IMOD_UTL_PRINTTEXT('c_listname()<listname()',2)

 IU=IMOD_UTL_GETUNIT()
 INQUIRE(FILE='.\dir_imod.txt',EXIST=LEX)
 IF(LEX)THEN
  OPEN(IU,FILE='.\dir_imod.txt',ACTION='READ',FORM='FORMATTED',IOSTAT=IOS)
  IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('iMOD does not have priveleges to delete [.\dir_imod.txt]',2)
  CLOSE(IU,STATUS='DELETE',IOSTAT=IOS)
  IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('iMOD does not have priveleges to delete [.\dir_imod.txt]',2)
 ENDIF

 IU=IMOD_UTL_GETUNIT()
 OPEN(IU,FILE='.\dir.bat',ACTION='WRITE',FORM='FORMATTED',IOSTAT=IOS)
 IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('iMOD does not have priveleges to create: [.\dir_imod.bat]',2)

 IF(FT.EQ.'F'.OR.FT.EQ.'f')LINE='dir /b /o "'//TRIM(DIR)//'\'//TRIM(WC)//'" > "'//'.\dir_imod.txt"'
 IF(FT.EQ.'D'.OR.FT.EQ.'d')LINE='dir /ad /b /o "'//TRIM(DIR)//'\'//TRIM(WC)//'" > ".\dir_imod.txt"'

 !## remove \\
 DO
  I=INDEX(LINE,'\\')
  IF(I.EQ.0)EXIT
  LINE(I+1:256-1)=LINE(I+2:)
 ENDDO

 WRITE(IU,'(A)') TRIM(LINE)
 CLOSE(IU)

 LINE='.\dir.bat'
 CALL SYSTEM(TRIM(LINE))

 IU=IMOD_UTL_GETUNIT()
 OPEN(IU,FILE='.\dir_imod.txt',ACTION='READ',FORM='FORMATTED')
 IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('iMOD does not have priveleges to read: [.\dir_imod.txt]',2)

 ALLOCATE(C_LISTNAME(50))

 I=0
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  J=LEN_TRIM(LINE)
  IF(J.EQ.0)EXIT
  LINE(2:J+1)=LINE(1:J)
  LINE(1:1)='"'
  LINE(J+2:J+2)='"'
  I=I+1
  IF(I.GT.SIZE(C_LISTNAME))THEN
   N=SIZE(C_LISTNAME)
   ALLOCATE(LISTNAME(N));  LISTNAME(1:N)=C_LISTNAME(1:N)
   DEALLOCATE(C_LISTNAME); ALLOCATE(C_LISTNAME(N*2))
   C_LISTNAME(1:N)=LISTNAME(1:N); DEALLOCATE(LISTNAME)
  ENDIF
  READ(LINE,*,IOSTAT=IOS) C_LISTNAME(I)
  IF(IOS.NE.0)EXIT
 END DO

 CLOSE(IU)!,STATUS='DELETE')

 N=I

 ALLOCATE(LISTNAME(N))
 LISTNAME(1:N)=C_LISTNAME(1:N)
 DEALLOCATE(C_LISTNAME)

 IMOD_UTL_DIRINFO=.TRUE.

 END FUNCTION IMOD_UTL_DIRINFO


 !###====================================================================
 SUBROUTINE IMOD_UTL_APPLYFCT_R(A,NODATA,NROW,NCOL,FCT,IMP)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL
 REAL,INTENT(IN) :: FCT,IMP,NODATA
 REAL,INTENT(INOUT),DIMENSION(NCOL,NROW) :: A
 INTEGER :: IROW,ICOL

 DO IROW=1,NROW
  DO ICOL=1,NCOL
   IF(A(ICOL,IROW).NE.NODATA)THEN
    A(ICOL,IROW)=A(ICOL,IROW)*FCT
    A(ICOL,IROW)=A(ICOL,IROW)+IMP
   ENDIF
  END DO
 END DO

 RETURN
 END SUBROUTINE IMOD_UTL_APPLYFCT_R

 !###====================================================================
 SUBROUTINE IMOD_UTL_APPLYFCT_I(A,NODATA,NROW,NCOL,FCT,IMP)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL
 REAL,INTENT(IN) :: FCT,IMP,NODATA
 INTEGER,INTENT(INOUT),DIMENSION(NCOL,NROW) :: A
 INTEGER :: IROW,ICOL

 DO IROW=1,NROW
  DO ICOL=1,NCOL
   IF(A(ICOL,IROW).NE.NODATA)THEN
    A(ICOL,IROW)=A(ICOL,IROW)*FCT
    A(ICOL,IROW)=A(ICOL,IROW)+IMP
   ENDIF
  END DO
 END DO

 RETURN
 END SUBROUTINE IMOD_UTL_APPLYFCT_I

!###====================================================
 LOGICAL FUNCTION IMOD_UTL_EQUALS_REAL(A,B)
 !###====================================================
 IMPLICIT NONE
 REAL, INTENT(IN) :: A, B
 REAL :: EPS

 EPS=ABS(A)*EPSILON(A) ! SCALE EPSILON

 IF(EPS.EQ.0.0)THEN
  EPS=TINY (A) ! IF EPS UNDERFLOWED TO 0
 ! USE A VERY SMALL
 ! POSITIVE VALUE FOR EPSILON
 END IF

 IF(ABS(A-B).GT.EPS)THEN
  IMOD_UTL_EQUALS_REAL=.FALSE. ! NOT EQUAL IF DIFFERENCE>EPS
 ELSE
  IMOD_UTL_EQUALS_REAL=.TRUE.  ! EQUAL OTHERWISE
 ENDIF

 END FUNCTION IMOD_UTL_EQUALS_REAL

 !###====================================================================
 SUBROUTINE IMOD_UTL_SCALE1PDELRC(XMIN,YMIN,XMAX,YMAX,DELC,DELR,PDELR,PDELC,NROW,NCOL,CS,NROWIDF,NCOLIDF,IU,IEQ,ITB)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL,NROWIDF,NCOLIDF,IU,IEQ,ITB
 REAL,INTENT(IN) :: CS,XMIN,YMIN,XMAX,YMAX
 REAL,INTENT(IN),DIMENSION(0:NCOL) :: DELR
 REAL,INTENT(IN),DIMENSION(0:NROW) :: DELC
 REAL :: DX,DY,X,Y,D
 INTEGER,INTENT(OUT),DIMENSION(2,NCOL) :: PDELR
 INTEGER,INTENT(OUT),DIMENSION(2,NROW) :: PDELC
 INTEGER :: I,J,IREC
 CHARACTER(LEN=256) :: IDFNAME
 REAL,ALLOCATABLE,DIMENSION(:) :: DELRIDF,DELCIDF
 REAL :: MIND

 IF(XMIN.GT.DELR(0).OR.XMAX.LT.DELR(NCOL).OR.YMIN.GT.DELC(NROW).OR.YMAX.LT.DELC(0))THEN
  INQUIRE(UNIT=IU,NAME=IDFNAME)
  CALL IMOD_UTL_PRINTTEXT('=======================================',0)
  CALL IMOD_UTL_PRINTTEXT('Warning!',0)
  CALL IMOD_UTL_PRINTTEXT('File: '//TRIM(IDFNAME),0)
  CALL IMOD_UTL_PRINTTEXT('Undersizes current model dimensions!',0)
  IF(XMIN.GT.DELR(0))THEN
   CALL IMOD_UTL_PRINTTEXT('XMIN IDF '//TRIM(IMOD_UTL_RTOS(XMIN,'F',2))//' > XMIN MODEL '//TRIM(IMOD_UTL_RTOS(DELR(0),'F',2)),0)
  ENDIF
  IF(XMAX.LT.DELR(NCOL))THEN
   CALL IMOD_UTL_PRINTTEXT('XMAX IDF '//TRIM(IMOD_UTL_RTOS(XMAX,'F',2))//' < XMAX MODEL '//TRIM(IMOD_UTL_RTOS(DELR(NCOL),'F',2)),0)
  ENDIF
  IF(YMIN.GT.DELC(NROW))THEN
   CALL IMOD_UTL_PRINTTEXT('YMIN IDF '//TRIM(IMOD_UTL_RTOS(YMIN,'F',2))//' > YMIN MODEL '//TRIM(IMOD_UTL_RTOS(DELC(NROW),'F',2)),0)
  ENDIF
  IF(YMAX.LT.DELC(0))THEN
   CALL IMOD_UTL_PRINTTEXT('YMAX IDF '//TRIM(IMOD_UTL_RTOS(YMAX,'F',2))//' < YMAX MODEL '//TRIM(IMOD_UTL_RTOS(DELC(0),'F',2)),0)
  ENDIF
 ! CALL PRINTTEXT('---------------------------------------',0)
 ! CALL PRINTTEXT('XMIN MODEL '//TRIM(RTOS(DELR(0),'F',2)),0)
 ! CALL PRINTTEXT('YMIN MODEL '//TRIM(RTOS(DELC(NROW),'F',2)),0)
 ! CALL PRINTTEXT('XMAX MODEL '//TRIM(RTOS(DELR(NCOL),'F',2)),0)
 ! CALL PRINTTEXT('YMAX MODEL '//TRIM(RTOS(DELC(0),'F',2)),0)
  CALL IMOD_UTL_PRINTTEXT('=======================================',0)
  CALL IMOD_UTL_PRINTTEXT('Error',2)
 ENDIF

 IF(ALLOCATED(DELRIDF))DEALLOCATE(DELRIDF)
 IF(ALLOCATED(DELCIDF))DEALLOCATE(DELCIDF)
 ALLOCATE(DELRIDF(0:NCOLIDF),DELCIDF(0:NROWIDF))

 DELRIDF(0)=XMIN
 DELCIDF(0)=YMAX

 IF(IEQ.EQ.0)THEN
  DO I=1,NCOLIDF; DELRIDF(I)=XMIN+REAL(I)*CS; ENDDO
  DO I=1,NROWIDF; DELCIDF(I)=YMAX-REAL(I)*CS; ENDDO
 ELSEIF(IEQ.EQ.1)THEN
  IREC      =10+ITB*2
  DO I=1,NCOLIDF
   IREC=IREC+1
   READ(IU,REC=IREC+ICF) DELRIDF(I)
   DELRIDF(I)=DELRIDF(I-1)+DELRIDF(I)
  END DO
  DO I=1,NROWIDF
   IREC=IREC+1
   READ(IU,REC=IREC+ICF) DELCIDF(I)
   DELCIDF(I)=DELCIDF(I-1)-DELCIDF(I)
  END DO
 ENDIF

 !## start/end column direction
 DO I=1,NCOL
  CALL IMOD_UTL_POL1LOCATER(DELRIDF,NCOLIDF+1,DELR(I-1),PDELR(1,I))
  !## check whether position is exact equally
  J=PDELR(1,I)
  IF(J.LE.NCOLIDF)THEN
   IF(DELRIDF(J).EQ.DELR(I-1))PDELR(1,I)=PDELR(1,I)+1
  ENDIF
  CALL IMOD_UTL_POL1LOCATER(DELRIDF,NCOLIDF+1,DELR(I),PDELR(2,I))
  PDELR(1,I)=MIN(PDELR(1,I),NCOLIDF)
  PDELR(2,I)=MIN(PDELR(2,I),NCOLIDF)
 ENDDO

 DO I=1,NROW
  CALL IMOD_UTL_POL1LOCATER(DELCIDF,NROWIDF+1,DELC(I-1),PDELC(1,I))
  CALL IMOD_UTL_POL1LOCATER(DELCIDF,NROWIDF+1,DELC(I),PDELC(2,I))
  !## check whether position is exact equally
  J=PDELC(2,I)
  IF(J.LE.NROWIDF)THEN
   IF(DELCIDF(J-1).EQ.DELC(I))PDELC(2,I)=PDELC(2,I)-1
  ENDIF
  PDELC(1,I)=MIN(PDELC(1,I),NROWIDF)
  PDELC(2,I)=MIN(PDELC(2,I),NROWIDF)
 ENDDO
 IF(ALLOCATED(DELRIDF))DEALLOCATE(DELRIDF)
 IF(ALLOCATED(DELCIDF))DEALLOCATE(DELCIDF)

 DO I=1,NCOL
  IF(PDELR(2,I).LT.PDELR(1,I))then
   DX        =(DELR(I-1)-XMIN)/CS
   PDELR(1,I)=INT(DX)+1
   WRITE(*,*) DELR(I-1),XMIN,DELR(I-1)-XMIN,DX,CS,PDELR(1,I)
   DX        =(DELR(I)-XMIN)/CS
   PDELR(2,I)=INT(DX)+1
   WRITE(*,*) DELR(I),XMIN,DELR(I)-XMIN,DX,INT(DX)+1,PDELR(2,I)
   DX=DELR(I)-XMIN
   IF(MOD(DX,CS).EQ.0.0)PDELR(2,I)=PDELR(2,I)-1
   WRITE(*,*) DELR(I),XMIN,DELR(I)-XMIN,DX,PDELR(2,I),MOD(DX,CS)
   CALL IMOD_UTL_PRINTTEXT('PDELR(2,I).LT.PDELR(1,I)',2)
  ENDIF
 ENDDO
 DO I=1,NROW
  IF(PDELC(2,I).LT.PDELC(1,I))THEN
   DY=(YMAX-DELC(I-1))/CS
   PDELC(1,I)=INT(DY)+1
   WRITE(*,*) DELC(I-1),YMAX,YMAX-DELC(I-1),DY,CS,PDELC(1,I)
   DY=(YMAX-DELC(I))
   PDELC(2,I)=INT(DY)+1
   WRITE(*,*) DELC(I),YMAX,YMAX-DELC(I),DY,PDELC(2,I)!,MOD(DY,CS)
   DY=YMAX-DELC(I)
   IF(MOD(DY,CS).EQ.0.0)PDELC(2,I)=PDELC(2,I)-1
   WRITE(*,*) DELC(I),YMAX,YMAX-DELC(I),DY,PDELC(2,I),MOD(DY,CS)
   CALL IMOD_UTL_PRINTTEXT('PDELC(2,I).LT.PDELC(1,I)',2)
  ENDIF
 ENDDO

 !##adjust pdelr/pdelc in case reading idf is coarser, then you don't need to read it in again, values will be copied in READCOPYVALUES_R()
 J=1
 DO I=2,NCOL
  IF(PDELR(1,I).EQ.PDELR(1,J).AND. &
     PDELR(2,I).EQ.PDELR(2,J))THEN
   PDELR(1,I)=-J
   PDELR(2,I)=-J
  ELSE
   J=I
  ENDIF
 END DO
 J=1
 DO I=2,NROW
  IF(PDELC(1,I).EQ.PDELC(1,J).AND. &
     PDELC(2,I).EQ.PDELC(2,J))THEN
   PDELC(1,I)=-J
   PDELC(2,I)=-J
  ELSE
   J=I
  ENDIF
 END DO

 RETURN
 END SUBROUTINE IMOD_UTL_SCALE1PDELRC

 !###======================================================================
 SUBROUTINE IMOD_UTL_ST1CREATEIPF_GETTLP(N,TLP,KH,C,TOP,BOT,ZZ1,ZZ2,MAXC,MINKH,ICLAY,CTXTFILE)
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: MINP=0.05
 INTEGER,INTENT(IN) :: N,ICLAY
 CHARACTER(LEN=*),INTENT(IN) :: CTXTFILE
 REAL,INTENT(IN) :: ZZ1,ZZ2
 REAL,INTENT(IN) :: MAXC,MINKH
 REAL,INTENT(IN),DIMENSION(N) :: KH,TOP,BOT
 REAL,INTENT(IN),DIMENSION(N-1) :: C
 REAL,INTENT(INOUT),DIMENSION(N) :: TLP
 INTEGER :: JLAY,ILAY,K,IDIFF
 REAL :: ZM,ZT,ZB,ZC,FC,DZ,Z1,Z2
 REAL,ALLOCATABLE,DIMENSION(:) :: L,TL
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IL

 ALLOCATE(L(N),TL(N),IL(N))

 Z1=ZZ1; Z2=ZZ2

 !## make sure thickness is not exactly zero, minimal thickness is 0.01m
 IDIFF=0; IF(Z1.EQ.Z2)THEN; Z1=Z1+0.005; Z2=Z2-0.005; IDIFF=1; ENDIF

 !## filterlength for each modellayer
 L=0.0
 DO ILAY=1,N
  ZT=MIN(TOP(ILAY),Z1); ZB=MAX(BOT(ILAY),Z2)
  L(ILAY)=MAX(0.0,ZT-ZB)
 ENDDO

 !## correct whenever significant resistant layer is active
 TL=0.0; JLAY=1; IL=0
 DO ILAY=1,N
  IF(L(ILAY).GT.0.0)THEN
   TL(JLAY)=TL(JLAY)+L(ILAY); IL(ILAY)=JLAY
   IF(ILAY.LT.N)THEN; IF(C(ILAY).GT.MAXC)JLAY=JLAY+1; ENDIF
  ENDIF
 ENDDO
 !## filter over more layers, significant clay in between, take longest in aquifer, MAKE OTHER ZERO!
 IF(JLAY.GT.1)THEN
  K=0; ZM=0.0; DO ILAY=1,JLAY; IF(TL(ILAY).GT.ZM)THEN; ZM=TL(ILAY); K=ILAY ; ENDIF; ENDDO
  DO ILAY=1,N; IF(IL(ILAY).NE.K)L(ILAY)=0.0; ENDDO
 ENDIF

 TLP=0.0
 !## well within any aquifer(s)
 IF(SUM(L).GT.0.0)THEN
  !## compute percentage and include sumkd, only if itype.eq.2
  L=L*KH
  !## percentage (0-1) L*KH
  DO ILAY=1,N; IF(L(ILAY).NE.0.0)TLP=(1.0/SUM(L))*L; ENDDO
 ENDIF

 !## correct for dismatch with centre of modelcell
 DO ILAY=1,N
  IF(TLP(ILAY).GT.0.0)THEN
   DZ= TOP(ILAY)-BOT(ILAY)
   ZC=(TOP(ILAY)+BOT(ILAY))/2.0
   ZT= MIN(TOP(ILAY),Z1)
   ZB= MAX(BOT(ILAY),Z2)
   FC=(ZT+ZB)/2.0
   TLP(ILAY)=TLP(ILAY)*(1.0-(ABS(ZC-FC)/(0.5*DZ)))
  ENDIF
 ENDDO

 !## normalize tlp() again
 IF(SUM(TLP).GT.0.0)TLP=(1.0/SUM(TLP))*TLP

 !## remove small percentages
 DO ILAY=1,N; IF(TLP(ILAY).LT.MINP)TLP(ILAY)=0.0; ENDDO

 !## normalize tlp() again
 IF(SUM(TLP).GT.0.0)TLP=(1.0/SUM(TLP))*TLP

 !## remove small PERMEABILITIES
 IF(MINKH.GT.0.0)THEN
  ZT=SUM(TLP)
  DO ILAY=1,N; IF(KH(ILAY).LT.MINKH)TLP(ILAY)=0.0; ENDDO
  IF(SUM(TLP).GT.0.0)THEN
   ZT=ZT/SUM(TLP)
   !## normalize tlp() again
   TLP=ZT*TLP
  ENDIF
 ENDIF

 !## normalize tlp() again
 IF(SUM(TLP).GT.0.0)TLP=(1.0/SUM(TLP))*TLP

 !## if no layers has been used for the assignment, try to allocate it to the nearest if iclay.eq.1
 IF(SUM(TLP).EQ.0.AND.ICLAY.EQ.1)THEN
  ZM=(Z1+Z2)/2.0; DZ=99999.0; JLAY=0
  DO ILAY=1,N
   ZT=TOP(ILAY); ZB=BOT(ILAY)
   IF(ABS(ZT-ZM).LT.DZ.OR.ABS(ZB-ZM).LT.DZ)THEN
    DZ  =MIN(ABS(ZT-ZM),ABS(ZB-ZM))
    JLAY=ILAY
   ENDIF
  ENDDO
  IF(JLAY.EQ.0)STOP 'JLAY.EQ.0, NO ABLE TO ASSIGN PROPER MODELLAYER'
  TLP(JLAY)=-1.0
 ENDIF

 !## make sure only one layer is assigned whenever z1.eq.z2
 IF(IDIFF.EQ.1)THEN
  K=0; ZT=0.0; DO ILAY=1,N
   IF(ABS(TLP(ILAY)).GT.ZT)THEN
    ZT=ABS(TLP(ILAY)); K=ILAY
   ENDIF
  ENDDO
  ZT=TLP(K)
  TLP=0.0; TLP(K)=1.0
  IF(ZT.LT.0.0)TLP(K)=-1.0*TLP(K)
 ENDIF

 DEALLOCATE(L,TL,IL)

 END SUBROUTINE IMOD_UTL_ST1CREATEIPF_GETTLP

 !###======================================================================
 LOGICAL FUNCTION IMOD_UTL_HAS_EXT(FNAME,EXT)
 !###======================================================================
 
 IMPLICIT NONE

 CHARACTER(LEN=*), INTENT(IN) :: FNAME
 CHARACTER(LEN=*), INTENT(IN) :: EXT

 CHARACTER(LEN=256) :: S
 INTEGER :: I, N

 S = TRIM(FNAME)
 CALL IMOD_UTL_S_CAP(S,'l')
 I = INDEX(S,'.',BACK=.TRUE.)
 IF (I.GT.0) THEN
  N = LEN_TRIM(EXT)
  IMOD_UTL_HAS_EXT = (S(I+1:I+N) == TRIM(EXT))
 ELSE
  IMOD_UTL_HAS_EXT = .FALSE.
 END IF

 END FUNCTION IMOD_UTL_HAS_EXT

 !###======================================================================
 SUBROUTINE IMOD_UTL_DIR_LEVEL_UP(FNAME)
 !###======================================================================
 
 IMPLICIT NONE

 CHARACTER(LEN=*), INTENT(INOUT) :: FNAME
 INTEGER :: N

 N = LEN_TRIM(FNAME)

 IF (N==0) RETURN

 IF (FNAME(1:1)=='.')THEN
  WRITE(FNAME,'(2A)') '..\',TRIM(FNAME)
 END IF

 CALL IMOD_UTL_SWAPSLASH(FNAME)

 END SUBROUTINE IMOD_UTL_DIR_LEVEL_UP

 !###====================================================================== 
 SUBROUTINE IMOD_UTL_ABS_PATH(PATH)
 !###======================================================================
 
 IMPLICIT NONE
 CHARACTER(LEN=*), INTENT(INOUT) :: PATH
 INTEGER :: LUN, I
 CHARACTER(LEN=1024) :: ABSPATH

 CALL IMOD_UTL_CREATEDIR(PATH)
 ABSPATH = TRIM(PATH)//'\TEST.TXT'
 CALL IMOD_UTL_SWAPSLASH(ABSPATH)
 LUN = IMOD_UTL_GETUNIT()
 OPEN(UNIT=LUN,FILE=ABSPATH)
 INQUIRE(UNIT=LUN,NAME=ABSPATH)
 I = INDEX(ABSPATH,'\',.TRUE.)
 IF (I.GT.0) ABSPATH = ABSPATH(1:I-1)
 I = INDEX(ABSPATH,'/',.TRUE.)
 IF (I.GT.0) ABSPATH = ABSPATH(1:I-1)
 PATH = TRIM(ABSPATH)
 CLOSE(LUN,STATUS='DELETE')

 END SUBROUTINE IMOD_UTL_ABS_PATH

!###====================================================================
 SUBROUTINE IMOD_UTL_GETRCL(NODE,NROW,NCOL,ILAY,IROW,ICOL)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN)     :: NROW,NCOL,NODE
 INTEGER,INTENT(OUT)    :: ILAY,IROW,ICOL
 INTEGER                :: K,INODE
 DOUBLE PRECISION :: RNODE,RNODES

 RNODE =DBLE(NODE)
 RNODES=DBLE(NCOL*NROW)
 INODE =NODE

 K=1
 IF(MOD(RNODE,RNODES).EQ.0.0)K=0
 ILAY =INT(RNODE)/INT(RNODES)+K
 INODE=INODE-((ILAY-1)*NCOL*NROW)
 K=1
 IF(MOD(INODE,NCOL).EQ.0)K=0
 IROW=INT(INODE/NCOL)+K
 ICOL=INODE-((IROW-1)*NCOL)

 END SUBROUTINE IMOD_UTL_GETRCL

 !###====================================================================
 SUBROUTINE IMOD_UTL_READOPENFILE(IU,NROW,NCOL,FNAME,NODATA,XMIN,YMIN,XMAX,YMAX,CS,IEQ,ITB)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: NROW,NCOL,IU,IEQ,ITB
 REAL,INTENT(OUT) :: NODATA,XMIN,YMIN,CS,XMAX,YMAX
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: IOS
 REAL :: DX,DY
 INTEGER(KIND=1) :: I1,I2,I3,I4

 CALL IMOD_UTL_OPENIDF(IU,FNAME,'R',4)
 IF(IU.LE.0)CALL IMOD_UTL_PRINTTEXT('Can not open '//TRIM(FNAME)//' for reading!',2)
 READ(IU,REC=1+ICF)  NCOL
 READ(IU,REC=2+ICF)  NROW
 READ(IU,REC=3+ICF)  XMIN
 READ(IU,REC=4+ICF)  XMAX
 READ(IU,REC=5+ICF)  YMIN
 READ(IU,REC=6+ICF)  YMAX
 READ(IU,REC=9+ICF)  NODATA
 ITB=0
 READ(IU,REC=10+ICF,IOSTAT=IOS) IEQ
 IF(IOS.NE.0)RETURN
 IF(IEQ.NE.0.AND.IEQ.NE.1)THEN
  READ(IU,REC=10+ICF,IOSTAT=IOS) I1,I2,I3,I4
  IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('iMOD can not read IEQ,ITB properly!',0)
  IEQ=INT(I1)
  ITB=INT(I2)
 ENDIF

 IF(IEQ.EQ.0)THEN
  READ(IU,REC=11+ICF) DX
  READ(IU,REC=12+ICF) DY
  IF(DX.NE.DY)CALL IMOD_UTL_PRINTTEXT(TRIM(FNAME)//': dx ne dy',2)
 ELSE
  DX=0.0
  DY=0.0
  CALL IMOD_UTL_PRINTTEXT('Non-equidistantial IDF is used!',0)
 ENDIF
 CS=DX

 RETURN
 END SUBROUTINE IMOD_UTL_READOPENFILE

 !###====================================================================
 SUBROUTINE IMOD_UTL_GETICIR(XC,YC,ICOL,IROW,NCOL,NROW,DELR,DELC,SIMCSIZE,IEQ)
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: XC,YC
 INTEGER,INTENT(OUT) :: ICOL,IROW
 INTEGER,INTENT(IN) :: NCOL,NROW
 REAL, DIMENSION(0:NCOL), INTENT(IN) :: DELR
 REAL, DIMENSION(0:NROW), INTENT(IN) :: DELC
 REAL, INTENT(IN) :: SIMCSIZE
 INTEGER, INTENT(IN) :: IEQ

 REAL :: DX,DY
 INTEGER :: I

 IF(IEQ.EQ.0)THEN

  DX=XC-DELR(0)
  I =0
  IF(MOD(DX,SIMCSIZE).NE.0.0)I=1
  ICOL=(DX/SIMCSIZE)+I

  DY=DELC(0)-YC
  I =0
  IF(MOD(DY,SIMCSIZE).NE.0.0)I=1
  IROW=(DY/SIMCSIZE)+I

 ELSE

  CALL IMOD_UTL_POL1LOCATER(DELR,NCOL+1,XC,ICOL)
  CALL IMOD_UTL_POL1LOCATER(DELC,NROW+1,YC,IROW)

 ENDIF

 END SUBROUTINE IMOD_UTL_GETICIR

!###======================================================================
LOGICAL FUNCTION IMOD_UTL_INTERSECT_EQUI(XMIN,XMAX,YMIN,YMAX,CS,X1,X2,Y1,Y2,MX,N,XA,YA,FA,LN,LHFB)
!###======================================================================
IMPLICIT NONE
REAL,INTENT(IN) :: XMIN,XMAX,YMIN,YMAX,CS
REAL,INTENT(INOUT) :: X1,X2,Y1,Y2
INTEGER,INTENT(IN) :: MX
INTEGER,INTENT(OUT) :: N
REAL,INTENT(OUT),DIMENSION(MX) :: XA,YA,FA,LN !## XA=col, YA=row, FA=fraction to nearest corner, 1,2,3,4 clockwise
LOGICAL,INTENT(IN) :: LHFB
REAL :: X,Y,XMN,XMX,YMN,YMX,DX,DY,LENG,TD
INTEGER :: I,ICOL,IROW,ID
REAL :: A,B,XBEGIN,YBEGIN

IMOD_UTL_INTERSECT_EQUI=.TRUE.

N=0
IF((MIN(X1,X2).GE.XMAX.OR.MAX(X1,X2).LE.XMIN).OR. &
   (MIN(Y1,Y2).GE.YMAX.OR.MAX(Y1,Y2).LE.YMIN))RETURN

IMOD_UTL_INTERSECT_EQUI=.FALSE.

IF(MX.LT.3)RETURN

XBEGIN=X1
YBEGIN=Y1

!#arrange x1,x2,y1,y2 such that x1<x2
IF(X1.GT.X2)THEN
 X    =X1
 Y    =Y1
 X1   =X2
 Y1   =Y2
 X2   =X
 Y2   =Y
ENDIF

!## find search box - result can be negative, does not matter!
I=1
XMN=XMIN+CS*(INT((X1-XMIN)/CS)+I)
I=0
DX=X2-XMIN
IF(MOD(DX,CS).EQ.0.0)I=-1
XMX=XMIN+CS*(INT((X2-XMIN)/CS)+I)

Y =MIN(Y1,Y2)
I =0
DY=YMAX-Y
IF(MOD(DY,CS).EQ.0.0)I=-1
YMN=YMAX-CS*(INT((YMAX-Y)/CS)+I)
Y =MAX(Y1,Y2)
I =1
YMX=YMAX-CS*(INT((YMAX-Y)/CS)+I)

!## use always mid between point x1,y1 and x2,y2 as first position
!## duplicate first and last point to process them as well
XA(1)= X1
YA(1)= Y1
XA(2)= X1
YA(2)= Y1
XA(3)=(X1+X2)/2.0
YA(3)=(Y1+Y2)/2.0
XA(4)= X2
YA(4)= Y2
XA(5)= X2
YA(5)= Y2
N    = 5

!## find mathematical expression for line: y=ax+b
DX=X2-X1
DY=Y2-Y1
IF(DX.EQ.0.0)DX=10.0E-10
A=DY/DX
B=Y1-A*X1

!!## shift coordinates!
!IF(Y1.NE.A*X1+B.OR. &
!   Y2.NE.A*X2+B)THEN
!ENDIF

!## continue seach rest of intersections
!## try intersections with y-axes firstly
Y=YMN-CS
DO
 Y=Y+CS
 IF(Y.GT.YMX)EXIT

 !## array overwritten
 N=N+1; IF(N.GT.MX)RETURN

 XA(N)=(Y-B)/A
 YA(N)=Y

 !## double intersections, for better estimate for hfb
 IF(LHFB)THEN
  !## array overwritten
  N=N+1; IF(N.GT.MX)RETURN
  XA(N)=(Y-B)/A
  YA(N)=Y
 ENDIF
 
ENDDO
!## try intersections with x-axes secondly
X=XMN-CS
DO
 X=X+CS
 IF(X.GT.XMX)EXIT

 !## array overwritten
 N=N+1; IF(N.GT.MX)RETURN
 XA(N)=X
 YA(N)=A*X+B

 !## double intersections, for better estimate for hfb
 IF(LHFB)THEN
  N=N+1; IF(N.GT.MX)RETURN
  XA(N)=X
  YA(N)=A*X+B
 ENDIF
 
ENDDO

!## sort intersections, determined by the one with the largest difference
IF(ABS(DX).GE.ABS(DY))THEN
 CALL IMOD_UTL_QKSORT2(XA,YA,MX,N)
ELSE
 CALL IMOD_UTL_QKSORT2(YA,XA,MX,N)
ENDIF

!## resort - if neccessary
IF(XA(1).NE.XBEGIN.OR.YA(1).NE.YBEGIN)THEN
 DO I=1,N/2
  X        =XA(I)
  XA(I)    =XA(N-I+1)
  XA(N-I+1)=X
  Y        =YA(I)
  YA(I)    =YA(N-I+1)
  YA(N-I+1)=Y
  LENG     =LN(I)
  LN(I)    =LN(N-I+1)
  LN(N-I+1)=LENG
 END DO
ENDIF

!## sample each of the point to determine irow/icol, overwrite point with this
!## skip first and last, they represented already by the second and one-last point
DO I=2,N

 !## mid point
 X   =(XA(I-1)+XA(I))/2.0
 Y   =(YA(I-1)+YA(I))/2.0
 ICOL=INT((X-XMIN)/CS)+1
 IROW=INT((YMAX-Y)/CS)+1

 TD=CS*2.0; ID=0 !## fraction
 CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,XMIN+(ICOL-1)*CS,YMAX-(IROW-1)*CS,X,Y,1)
 CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,XMIN+ ICOL   *CS,YMAX-(IROW-1)*CS,X,Y,2)
 CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,XMIN+ ICOL   *CS,YMAX- IROW   *CS,X,Y,3)
 CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,XMIN+(ICOL-1)*CS,YMAX- IROW   *CS,X,Y,4)
 FA(I-1)=REAL(ID) !+TD
  
 DX  =XA(I)-XA(I-1)
 DY  =YA(I)-YA(I-1)
 LENG=SQRT(DX**2.0+DY**2.0)
 !## store results
 XA(I-1)=REAL(ICOL)
 YA(I-1)=REAL(IROW)
 LN(I-1)=LENG
END DO
N=N-1

IMOD_UTL_INTERSECT_EQUI=.TRUE.

END FUNCTION IMOD_UTL_INTERSECT_EQUI
 
!###======================================================================
SUBROUTINE IMOD_UTL_INTERSECT_NCORNER(ID,TD,X1,Y1,XC,YC,JD)
!###======================================================================
IMPLICIT NONE
INTEGER,INTENT(INOUT) :: ID
REAL,INTENT(INOUT) :: TD
INTEGER,INTENT(IN) :: JD
REAL,INTENT(IN) :: X1,Y1,XC,YC
REAL :: D
  
!## get nearest corner
D=(XC-X1)**2.0+(YC-Y1)**2.0
IF(D.GT.0.0)THEN
 D=SQRT(D)
 IF(D.LT.TD)THEN
  ID=JD
  TD=D
 ENDIF
ELSE
 ID=JD
 TD=0.0
ENDIF

END SUBROUTINE IMOD_UTL_INTERSECT_NCORNER

!###======================================================================
LOGICAL FUNCTION IMOD_UTL_INTERSECT_NONEQUI(DELR,DELC,NROW,NCOL,X1,X2,Y1,Y2,MX,N,XA,YA,FA,LN,LHFB)
!###======================================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: MX,NROW,NCOL
REAL,INTENT(IN),DIMENSION(0:NCOL) :: DELR
REAL,INTENT(IN),DIMENSION(0:NROW) :: DELC
REAL,INTENT(INOUT) :: X1,X2,Y1,Y2
INTEGER,INTENT(OUT) :: N
REAL,INTENT(OUT),DIMENSION(MX) :: XA,YA,FA,LN
LOGICAL,INTENT(IN) :: LHFB
REAL :: X,Y,XMN,XMX,YMN,YMX,DX,DY,LENG,XMIN,YMIN,XMAX,YMAX
INTEGER :: I,ICOL,IROW,IMN,ID
REAL :: A,B,XBEGIN,YBEGIN,CS,TD

IMOD_UTL_INTERSECT_NONEQUI=.TRUE.

XMIN=DELR(0)
XMAX=DELR(NCOL)
YMIN=DELC(NROW)
YMAX=DELC(0)

N=0
IF((MIN(X1,X2).GE.XMAX.OR.MAX(X1,X2).LE.XMIN).OR. &
   (MIN(Y1,Y2).GE.YMAX.OR.MAX(Y1,Y2).LE.YMIN))RETURN

IMOD_UTL_INTERSECT_NONEQUI=.FALSE.

IF(MX.LT.3)RETURN

XBEGIN=X1
YBEGIN=Y1

!## arrange x1,x2,y1,y2 such that x1<x2
IF(X1.GT.X2)THEN
 X    =X1
 Y    =Y1
 X1   =X2
 Y1   =Y2
 X2   =X
 Y2   =Y
ENDIF

!!## adjust perfect 45/135/215,305 aanpassen
!DX=X2-X1; DY=Y2-Y1; IF(ABS(DX).EQ.ABS(DY))X1=X1+1.0

!## use always mid between point x1,y1 and x2,y2 as first position
XA(1)= X1
YA(1)= Y1
XA(2)= X1
YA(2)= Y1
XA(3)=(X1+X2)/2.0
YA(3)=(Y1+Y2)/2.0
XA(4)= X2
YA(4)= Y2
XA(5)= X2
YA(5)= Y2
N    = 5

!## find mathematical expression for line: y=ax+b
DX=X2-X1
DY=Y2-Y1
IF(DX.EQ.0.0)DX=10.0E-10
A=DY/DX
B=Y1-A*X1

!!## shift coordinates!
!IF(Y1.NE.A*X1+B.OR. &
!   Y2.NE.A*X2+B)THEN
!ENDIF

!continue search rest of intersections
!try intersections with y-axes firstly
IMN=0
DO I=1,NROW 
 IF(DELC(I).GT.MIN(Y1,Y2))THEN
  YMN=DELC(I)
  IMN=I
 ENDIF
END DO
DO I=NROW,1,-1 
 IF(DELC(I).LT.MAX(Y1,Y2))YMX=DELC(I)
END DO

IF(IMN.GT.0)THEN
 CS=DELC(IMN-1)-DELC(IMN)
 Y =YMN-CS
 DO
  Y=Y+CS
  IF(Y.GT.YMX)EXIT
  N    =N+1
  !## array overwritten
  IF(N.GT.MX)RETURN
  XA(N)=(Y-B)/A
  YA(N)=Y

  !## double intersections, for better estimate for hfb
  IF(LHFB)THEN
   !## array overwritten
   N=N+1; IF(N.GT.MX)RETURN
   XA(N)=(Y-B)/A
   YA(N)=Y
  ENDIF
  
  IMN  =IMN-1
  !## model is not bigger than line-segment
  IF(IMN.LE.0)EXIT
  CS=DELC(IMN-1)-DELC(IMN)
 ENDDO
ENDIF

!## try intersections with x-axes secondly
IMN=0
DO I=NCOL,1,-1 !0,-1
 IF(DELR(I).GT.X1)THEN
  XMN=DELR(I)
  IMN=I
 ENDIF
END DO
DO I=0,NCOL
 IF(DELR(I).LT.X2)XMX=DELR(I)
END DO

IF(IMN.GT.0)THEN
 CS=DELC(IMN-1)-DELC(IMN)
 X =XMN-CS
 DO
  X=X+CS
  IF(X.GT.XMX)EXIT
  N    =N+1
  !## array overwritten
  IF(N.GT.MX)RETURN
  XA(N)=X
  YA(N)=A*X+B

  !## double intersections, for better estimate for hfb
  IF(LHFB)THEN
   !## array overwritten
   N=N+1; IF(N.GT.MX)RETURN
   XA(N)=X
   YA(N)=A*X+B
  ENDIF
  
  IMN  =IMN+1
  !## model is not bigger than line-segment
  IF(IMN.GT.NCOL)EXIT
  CS=DELR(IMN)-DELR(IMN-1)
 ENDDO
ENDIF

!## sort intersections, determined by the one with the largest difference
IF(ABS(DX).GE.ABS(DY))THEN
 CALL IMOD_UTL_QKSORT2(XA,YA,MX,N)
ELSE
 CALL IMOD_UTL_QKSORT2(YA,XA,MX,N)
ENDIF

!## resort - if neccessary
IF(XA(1).NE.XBEGIN.OR.YA(1).NE.YBEGIN)THEN
 DO I=1,N/2
  X        =XA(I)
  XA(I)    =XA(N-I+1)
  XA(N-I+1)=X
  Y        =YA(I)
  YA(I)    =YA(N-I+1)
  YA(N-I+1)=Y
  LENG     =LN(I)
  LN(I)    =LN(N-I+1)
  LN(N-I+1)=LENG
 END DO
ENDIF

!## sample each of the point to determine irow/icol, overwrite point with this
!## skip first and last, they represented already by the second and one-last point
DO I=2,N
 !## mid point
 X   =(XA(I-1)+XA(I))/2.0
 Y   =(YA(I-1)+YA(I))/2.0

 CALL IMOD_UTL_POL1LOCATER(DELR,NCOL+1,X,ICOL)
 CALL IMOD_UTL_POL1LOCATER(DELC,NROW+1,Y,IROW)

 TD=CS*2.0; ID=0 !## fraction
 CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,DELR(ICOL-1),DELC(IROW-1),X,Y,1)
 CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,DELR(ICOL)  ,DELC(IROW-1),X,Y,2)
 CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,DELR(ICOL)  ,DELC(IROW)  ,X,Y,3)
 CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,DELR(ICOL-1),DELC(IROW)  ,X,Y,4)
 FA(I-1)=REAL(ID) !+TD

 DX  =XA(I)-XA(I-1)
 DY  =YA(I)-YA(I-1)
 LENG=SQRT(DX**2.0+DY**2.0)
 !## store results
 XA(I-1)=REAL(ICOL)
 YA(I-1)=REAL(IROW)
 LN(I-1)=LENG
END DO
N=N-1

IMOD_UTL_INTERSECT_NONEQUI=.TRUE.

END FUNCTION IMOD_UTL_INTERSECT_NONEQUI

 !###====================================================
 REAL FUNCTION IMOD_UTL_GETMED2(A,NA,NODATA,NAJ)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NA
 INTEGER,INTENT(OUT) :: NAJ
 REAL,INTENT(IN) :: NODATA
 REAL,DIMENSION(NA),INTENT(INOUT) :: A
 INTEGER :: I

 IMOD_UTL_GETMED2=NODATA
 NAJ=0

 IF(NA.GT.0)THEN
  IF(NA.EQ.1)THEN
   IF(A(1).NE.NODATA)THEN
    IMOD_UTL_GETMED2=A(1)
    NAJ   =1
   ENDIF
  ELSE
   CALL IMOD_UTL_SHELLSORT(NA,A)

   !## do not include nodata values for median-computation
   NAJ=0
   DO I=1,NA
    IF(A(I).NE.NODATA)THEN
     NAJ   =NAJ+1
     A(NAJ)=A(I)
    ENDIF
   END DO

   IF(NAJ.GT.0)THEN
    IF(MOD(REAL(NAJ+1.0),2.0).EQ.0.0)THEN
     I=INT(REAL(NAJ+1.0)/2.0)
     IMOD_UTL_GETMED2=A(I)
    ELSE
     I=INT(REAL(NAJ)/2.0)
     IMOD_UTL_GETMED2=(A(I)+A(I+1))/2.0
    ENDIF
   ENDIF

  ENDIF
 ENDIF

 END FUNCTION IMOD_UTL_GETMED2

 !###====================================================================
 SUBROUTINE IMOD_UTL_LUDECOMP(AA,IDX,N,ISING)
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: NMAX
 REAL :: TINY
 PARAMETER( TINY=1.0E-20, NMAX=2000)
 INTEGER :: N
 INTEGER :: ISING
 INTEGER :: IDX(N)
 REAL :: AA(N,N)
 REAL :: VV(NMAX)
 INTEGER :: I,IMAX,J,K
 REAL :: AAMAX,DUM,SUM

 DO I=1,N
  IDX(I)=0
 END DO
 ISING=0

 DO I=1,N
  AAMAX=0.
  DO J=1,N
   IF(ABS(AA(I,J)).GT.AAMAX)AAMAX=ABS(AA(I,J))
  ENDDO
  IF(AAMAX.EQ.0.)THEN
   WRITE(*,*) 'Matrix is singular'
   ISING=1
   RETURN
  ENDIF
  VV(I)=1./AAMAX
 ENDDO
 DO J=1,N
  DO I=1,J-1
   SUM=AA(I,J)
   DO K=1,I-1
    SUM=SUM-AA(I,K)*AA(K,J)
   ENDDO
   AA(I,J)=SUM
  ENDDO
  AAMAX=0.
  DO I=J,N
   SUM=AA(I,J)
   DO K=1,J-1
    SUM=SUM-AA(I,K)*AA(K,J)
   ENDDO
   AA(I,J)=SUM
   DUM=VV(I)*ABS(SUM)
   IF(DUM.GE.AAMAX)THEN
    IMAX=I
    AAMAX=DUM
   ENDIF
  ENDDO
  IF(J.NE.IMAX)THEN
   DO K=1,N
    DUM=AA(IMAX,K)
    AA(IMAX,K)=AA(J,K)
    AA(J,K)=DUM
   ENDDO
   VV(IMAX)=VV(J)
  ENDIF
  IDX(J)=IMAX
  IF(AA(J,J).EQ.0.)AA(J,J)=TINY
  IF(J.NE.N)THEN
   DUM=1./AA(J,J)
   DO I=J+1,N
    AA(I,J)=AA(I,J)*DUM
   ENDDO
  ENDIF
 ENDDO

 RETURN
 END SUBROUTINE IMOD_UTL_LUDECOMP

!###====================================================================
 SUBROUTINE IMOD_UTL_LUBACKSUB(AA,IDX,BB,N)
!###====================================================================
 
 IMPLICIT NONE
 INTEGER :: N
 REAL :: AA(N,N)
 REAL :: BB(N)
 INTEGER :: IDX(N)
 INTEGER :: I,II,J,LL
 REAL :: SUM

 II=0
 DO I=1,N
  LL=IDX(I)
  SUM=BB(LL)
  BB(LL)=BB(I)
  IF(II.NE.0)THEN
   DO J=II,I-1
    SUM=SUM-AA(I,J)*BB(J)
   ENDDO
  ELSE IF(SUM.NE.0.)THEN
   II=I
  ENDIF
  BB(I)=SUM
 ENDDO
 DO I=N,1,-1
  SUM=BB(I)
  DO J=I+1,N
   SUM=SUM-AA(I,J)*BB(J)
  ENDDO
  BB(I)=SUM/AA(I,I)
 ENDDO

 RETURN
 END SUBROUTINE IMOD_UTL_LUBACKSUB

 !###====================================================================
 SUBROUTINE IMOD_UTL_LUDECOMP_DBL(AA,IDX,N,ISING)
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: NMAX
 DOUBLE PRECISION :: TINY
 PARAMETER( TINY=1.0E-20, NMAX=2000)
 INTEGER :: N
 INTEGER :: ISING
 INTEGER :: IDX(N)
 DOUBLE PRECISION :: AA(N,N)
 DOUBLE PRECISION :: VV(NMAX)
 INTEGER :: I,IMAX,J,K
 DOUBLE PRECISION :: AAMAX,DUM,SUM

 DO I=1,N
  IDX(I)=0
 END DO
 ISING=0

 DO I=1,N
  AAMAX=0.
  DO J=1,N
   IF(ABS(AA(I,J)).GT.AAMAX)AAMAX=ABS(AA(I,J))
  ENDDO
  IF(AAMAX.EQ.0.)THEN
   WRITE(*,*) 'Matrix is singular'
   ISING=1
   RETURN
  ENDIF
  VV(I)=1./AAMAX
 ENDDO
 DO J=1,N
  DO I=1,J-1
   SUM=AA(I,J)
   DO K=1,I-1
    SUM=SUM-AA(I,K)*AA(K,J)
   ENDDO
   AA(I,J)=SUM
  ENDDO
  AAMAX=0.
  DO I=J,N
   SUM=AA(I,J)
   DO K=1,J-1
    SUM=SUM-AA(I,K)*AA(K,J)
   ENDDO
   AA(I,J)=SUM
   DUM=VV(I)*ABS(SUM)
   IF(DUM.GE.AAMAX)THEN
    IMAX=I
    AAMAX=DUM
   ENDIF
  ENDDO
  IF(J.NE.IMAX)THEN
   DO K=1,N
    DUM=AA(IMAX,K)
    AA(IMAX,K)=AA(J,K)
    AA(J,K)=DUM
   ENDDO
   VV(IMAX)=VV(J)
  ENDIF
  IDX(J)=IMAX
  IF(AA(J,J).EQ.0.)AA(J,J)=TINY
  IF(J.NE.N)THEN
   DUM=1./AA(J,J)
   DO I=J+1,N
    AA(I,J)=AA(I,J)*DUM
   ENDDO
  ENDIF
 ENDDO

 RETURN
 END SUBROUTINE IMOD_UTL_LUDECOMP_DBL

 !###====================================================================
 SUBROUTINE IMOD_UTL_LUBACKSUB_DBL(AA,IDX,BB,N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: N
 DOUBLE PRECISION :: AA(N,N)
 DOUBLE PRECISION :: BB(N)
 INTEGER :: IDX(N)
 INTEGER :: I,II,J,LL
 DOUBLE PRECISION :: SUM

 II=0
 DO I=1,N
  LL=IDX(I)
  SUM=BB(LL)
  BB(LL)=BB(I)
  IF(II.NE.0)THEN
   DO J=II,I-1
    SUM=SUM-AA(I,J)*BB(J)
   ENDDO
  ELSE IF(SUM.NE.0.)THEN
   II=I
  ENDIF
  BB(I)=SUM
 ENDDO
 DO I=N,1,-1
  SUM=BB(I)
  DO J=I+1,N
   SUM=SUM-AA(I,J)*BB(J)
  ENDDO
  BB(I)=SUM/AA(I,I)
 ENDDO

 RETURN
 END SUBROUTINE IMOD_UTL_LUBACKSUB_DBL
 
END MODULE IMOD_UTL