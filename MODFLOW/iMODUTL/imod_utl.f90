!!  Copyright (C) Stichting Deltares, 2005-2017.
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

 DOUBLE PRECISION,POINTER,DIMENSION(:) :: XA,YA,XA_DUMMY,YA_DUMMY
 REAL,POINTER,DIMENSION(:) :: ZA,FA,LN,ZA_DUMMY,FA_DUMMY,LN_DUMMY
 INTEGER,POINTER,DIMENSION(:) :: CA,RA,CA_DUMMY,RA_DUMMY
 DOUBLE PRECISION :: XBEGIN,YBEGIN,A,B
 INTEGER :: IHOR,IVER
 INTEGER,PARAMETER :: NOS=3
 INTEGER, SAVE :: OS = 1                     !## operating system 1=dos,2=linux,3=unix
 CHARACTER(LEN=20),DIMENSION(NOS),SAVE :: OSN

 LOGICAL,PARAMETER :: LPWT=.TRUE.    !##   FALSE=USE OLD PWT PACKAGE, TRUE=USE NEW ONE
 INTEGER,PARAMETER,PRIVATE :: MAXFILES=9999 !500
 INTEGER,PARAMETER :: ICF = 1 ! Intel compiler: ICF = 1
 INTEGER,SAVE :: IUOUT
 INTEGER,DIMENSION(2),SAVE :: IFLAG
 REAL,PARAMETER :: MSWPMV=0.0       !##   add meter to surface level urban area

character(len=1024), parameter :: licfile = 'I_accepted_v4_2.txt' 
integer, parameter :: nlic = 33
character(len=79), dimension(nlic) :: lic
integer, parameter :: nhdr = 40
character(len=79), dimension(nhdr) :: hdr

!         1234567890123456789012345678901234567890123456789012345678901234567890123456789
data hdr/'===============================================================================',&!01
         'iMODFLOW Version 4_1, October 2017                                         ',&!02
         '                                                                               ',&!03
         'Copyright (C) Stichting Deltares, 2005-2017.                                   ',&!04
         '                                                                               ',&!05
         'This Deltares-software executable is part of iMOD. iMOD is Deltares-software;  ',&!06 
         'the source code of iMOD is also available as free open source software at      ',&!07 
         'oss.deltares.nl. You may use the Deltares-software executables of iMOD without ',&!08 
         'any remuneration to be paid to Deltares if you accepted the iMOD Software      ',&!09 
         'License Agreement (iMOD License) which is offered to you as a PDF-file; you    ',&!10 
         'should have received a copy of this PDF-file with this Deltares-software       ',&!01 
         'executable. If not, see                                                        ',&!02
         'http://oss.deltares.nl/web/iMOD/iMOD_Software_License_Agreement.               ',&!03 
         'Please go to the PDF-file of the iMOD License, read it and decide whether you  ',&!04 
         'want or do not want to accept the iMOD License.                                ',&!05 
         '                                                                               ',&!06
         'According to the file "I_accepted_v4_2.txt" on your computer you accepted the  ',&!07
         'terms and conditions of the iMOD license; WARNING: IF IT WAS NOT YOU OR THE    ',&!08
         'LEGAL ENTITY ON WHOSE BEHALF YOU INTENT TO USE THE IMOD-EXECUTABLE, THAT       ',&!09 
         'ACCEPTED THE TERMS AND CONDITIONS OF THE iMOD LICENSE YOU ARE NOT ENTITLED TO  ',&!10
         'USE THIS DELTARES-EXECUTABLE OF IMOD. In this case your use of this            ',&!01
         'Deltares-executable of the iMOD-software is prohibited and illegal: abort the  ',&!02
         'use of this Deltares-executable immediately and refrain from using the         ',&!03 
         'Deltares-executables of iMOD. To make use of the Deltares-executables of iMOD  ',&!04
         'please make sure to accept the terms and conditions or have it lawfully        ',&!05
         'accepted by the legal entity on whose behalf you intent to use the             ',&!06 
         'iMOD-executable byre-invoking the "I accept"-procedure; to re-invoke the       ',&!07
         '"I accept"-procedure abort the use of this Deltares-executable of iMOD, delete ',&!08
         'the file "I_accepted_v4_2.txt", and invoke this Deltares-executable of iMOD    ',&!09
         'again.                                                                         ',&!10
         '                                                                               ',&!01
         'The iMOD software is distributed in the hope that it will be useful, but       ',&!02
         'WITHOUT ANY GUARANTEE OR (IMPLIED) WARRANTY. Any use of the                    ',&!03
         'Deltares-executables of the iMOD-software is for your own risk. See the iMOD   ',&!04
         'License for more details.                                                      ',&!05 
         '                                                                               ',&!06
         'For more info, please contact: Stichting Deltares, P.O. Box 177, 2600 MH Delft,',&!07
         'The Netherlands. Email: imod.support@deltares.nl.                              ',&!08
         '===============================================================================',&!09
         '                                                                               '/ !10
                                                                                            
!         1234567890123456789012345678901234567890123456789012345678901234567890123456789   
data lic/'===============================================================================',&!01
         'Copyright (C) Stichting Deltares, 2005-2017.                                   ',&!02
         '                                                                               ',&!03
         'This Deltares-software executable is part of iMOD. iMOD is Deltares-software;  ',&!04 
         'the source code of iMOD is also available as free open source software at      ',&!05 
         'oss.deltares.nl. You may use the Deltares-software executables of iMOD without ',&!06 
         'any remuneration to be paid to Deltares if you accepted the iMOD Software      ',&!07 
         'License Agreement (iMOD License) which is offered to you as a PDF-file; you    ',&!08 
         'should have received a copy of this PDF-file with this Deltares-software       ',&!09 
         'executable. If not, see                                                        ',&!10
         'http://oss.deltares.nl/web/iMOD/iMOD_Software_License_Agreement.               ' ,&!01 
         'Please go to the PDF-file of the iMOD License, read it and decide whether you  ',&!02 
         'want or do not want to accept the iMOD License.                                ',&!03 
         '                                                                               ',&!04 
         'If you accept the iMOD License, please enter "Y" or "y" below this text and hit',&!05 
         'the Enter-key.                                                                 ',&!06 
         '                                                                               ',&!07 
         'If you do not accept the iMOD License, please do NOT enter "Y" or "y" below    ',&!08 
         'this text and hit the Enter-key and refrain from using the Deltares-software   ',&!09
         'executables of iMOD; you may find a solution in downloading the source code of ',&!10
         'the iMOD-software and compile the executables yourself (see oss.deltares.nl).  ',&!01
         '                                                                               ',&!02
         'Without your acceptance of the iMOD License the use of the Deltares-executables',&!03
         'of the iMOD-software is prohibited and illegal.                                ',&!04
         '                                                                               ',&!05
         'The iMOD software is distributed in the hope that it will be useful, but       ',&!06
         'WITHOUT ANY GUARANTEE OR (IMPLIED) WARRANTY. Any use of the                    ',&!07
         'Deltares-executables of the iMOD-software is for your own risk. See the iMOD   ',&!08
         'License for more details.                                                      ',&!09 
         '                                                                               ',&!10
         'For more info, please contact: Stichting Deltares, P.O. Box 177, 2600 MH Delft,',&!01
         'The Netherlands. Email: imod.support@deltares.nl.                              ',&!02
         '==============================================================================='/ !03

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
 INTEGER FUNCTION IMOD_UTL_GETUNIT(INI)
 !###===================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN),OPTIONAL :: INI
 LOGICAL :: LEX
 INTEGER :: I
 
 I=20; IF(PRESENT(INI))I=INI
 
 IMOD_UTL_GETUNIT=I
 DO ! I,MAXFILES
  INQUIRE(UNIT=IMOD_UTL_GETUNIT,OPENED=LEX)
  IF(.NOT.LEX)RETURN
  IMOD_UTL_GETUNIT=IMOD_UTL_GETUNIT+1
 ENDDO

! CALL IMOD_UTL_PRINTTEXT('INCREASE IMOD_UTL_GETUNIT, MAX. FILES TO BE OPENED = '// &
!      TRIM(IMOD_UTL_ITOS(MAXFILES))//'!',2)

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
 CHARACTER(LEN=20) FUNCTION IMOD_UTL_DBL_ITOS(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER(KIND=8),INTENT(IN) :: I
 CHARACTER(LEN=20)  :: TXT
 CHARACTER(LEN=1),DIMENSION(20) :: A
 INTEGER :: J,K

 WRITE(TXT,'(I20)') I
 READ(TXT,'(20A1)') A(1:20)
 DO J=1,20
  IF(A(J).NE.' ')EXIT
 END DO
 K=20-J+1
 WRITE(IMOD_UTL_DBL_ITOS(1:K),'(20A1)') A(J:20)
 IMOD_UTL_DBL_ITOS(K+1:)=''

 END FUNCTION IMOD_UTL_DBL_ITOS

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
 INTEGER :: IU, IOS, N
 CHARACTER(LEN=256) :: FNAME

 IMOD_UTL_DIREXIST=.FALSE.
 !## try to create a file in folder
 IU=IMOD_UTL_GETUNIT()
 
 FNAME = TRIM(DIRNAME)//'\tmp.tmp#0#1#2'
#ifdef PKSMPI 
 N = LEN_TRIM(FNAME)
 CALL PKS7MPIFNAME(FNAME,N)
#endif 
 OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',IOSTAT=IOS)
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
 INTEGER :: IY,IM,ID

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
! INTEGER :: CFN_GETLUN

! IU=0
! IF(ACT.NE.'R'.AND.ACT.NE.'r'.AND. &   !read
!    ACT.NE.'W'.AND.ACT.EQ.'w')RETURN   !write

 IF(IU.EQ.0)IU=IMOD_UTL_GETUNIT() ! CFN_GETLUN(10,99)
 MESSAGE=''

 IF(ACT.EQ.'W'.OR.ACT.EQ.'w')THEN
!  OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='WRITE',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nrec,IOSTAT=IOS)
  MESSAGE='REPLACE,UNFORMATTED,DIRECT'
 ELSEIF(ACT.EQ.'R'.OR.ACT.EQ.'r')THEN
  CALL IMOD_UTL_MODEL1CHECKFNAME(FNAME,0)
!  OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',ACTION='READ',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nrec,IOSTAT=IOS)
  MESSAGE='READONLY,SHARED,OLD,UNFORMATTED,DIRECT'
 ENDIF

 IOS=OSD_OPEN2(IU,NREC,FNAME,TRIM(MESSAGE))  !unitnumber,recl,fname,string(options)

 !#error occured opening idf-file
 IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Can not open indexed file: '//TRIM(FNAME),2)

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
! INTEGER :: CFN_GETLUN

! IU=0
! IF(ACT.NE.'R'.AND.ACT.NE.'r'.AND. &   !read
!    ACT.NE.'W'.AND.ACT.NE.'w'.AND. &   !write
!    ACT.NE.'A'.AND.ACT.NE.'a')RETURN   !append

 !read,denywrite -> 'unknown,formatted,readonly,shared,append'  !rest default

! !## number of units 10-9999 for timeseries
! IF(IU.EQ.0)
 IU=GETUNIT() !CFN_GETLUN(10,9999)
 MESSAGE=''

 !#dos
 SELECT CASE (ACT)
  CASE ('W','w')
!   OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE',IOSTAT=IOS)
  MESSAGE='WRITE,UNKNOWN,FORMATTED'
  CASE ('R','r')
  CALL IMOD_UTL_MODEL1CHECKFNAME(FNAME,0)
!   OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='FORMATTED',ACTION='READ',IOSTAT=IOS)
  MESSAGE='READONLY,SHARED,OLD,FORMATTED'
  CASE ('A','a')
  CALL IMOD_UTL_MODEL1CHECKFNAME(FNAME,0)
   OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='FORMATTED',ACCESS='APPEND',IOSTAT=IOS)
  MESSAGE='OLD,FORMATTED,APPEND'
  CASE DEFAULT
   CALL IMOD_UTL_PRINTTEXT('Wrong open statement, cannot open sequential file: ['//TRIM(FNAME)//']',2)
 END SELECT

 IOS=OSD_OPEN2(IU,0,FNAME,TRIM(MESSAGE))  !unitnumber,recl,fname,string(options)

 !#error occured opening idf-file
 IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Cannot open sequential file: ['//TRIM(FNAME)//']',2)

 END SUBROUTINE

 !###====================================================
 SUBROUTINE IMOD_UTL_GETMED(X,NX,NODATA,PERC,NPERC,MX,XMED)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NX,NPERC            !## size array,number of percentiles to be comp.
 INTEGER,INTENT(OUT) :: MX                 !## number of values ne nodata
 REAL,INTENT(IN),DIMENSION(:) :: PERC  !## percentile 0-100%
 REAL,INTENT(OUT),DIMENSION(:) :: XMED !## yielding percentile(s)
 REAL,INTENT(IN) :: NODATA                 !## nodata value !,PERC
 REAL,DIMENSION(NX),INTENT(INOUT) :: X     !## array
 INTEGER :: I,J,IP
 REAL :: FRAC

 XMED=NODATA

 IF(NX.LE.0)RETURN

!## only one sample
IF(NX.EQ.1)THEN
  MX=0
  IF(X(1).NE.NODATA)THEN
   XMED=X(1)
   MX  =1
  ENDIF
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
 RECURSIVE SUBROUTINE IMOD_UTL_QKSORT3(A,AI)
 !###====================================================
 IMPLICIT NONE
 INTEGER, INTENT(IN OUT) :: A(:), AI(:)
 INTEGER :: SPLIT

 IF(SIZE(A) > 1) THEN
    CALL IMOD_UTL_PARTITION(A, AI, SPLIT)
    CALL IMOD_UTL_QKSORT3(A(:SPLIT-1),AI(:SPLIT-1))
    CALL IMOD_UTL_QKSORT3(A(SPLIT:),AI(SPLIT:))
 END IF

 END SUBROUTINE IMOD_UTL_QKSORT3

 !###====================================================
 SUBROUTINE IMOD_UTL_PARTITION(A,AI,MARKER)
 !###====================================================
 IMPLICIT NONE
 INTEGER, INTENT(IN OUT) :: A(:), AI(:)
 INTEGER, INTENT(OUT) :: MARKER
 INTEGER :: LEFT, RIGHT, PIVOT, TEMP

 PIVOT = (A(1) + A(SIZE(A))) / 2  ! AVERAGE OF FIRST AND LAST ELEMENTS TO PREVENT QUADRATIC
 LEFT = 0                         ! BEHAVIOR WITH SORTED OR REVERSE SORTED DATA
 RIGHT = SIZE(A) + 1

 DO WHILE (LEFT < RIGHT)
    RIGHT = RIGHT - 1
    DO WHILE (A(RIGHT) > PIVOT)
       RIGHT = RIGHT-1
    END DO
    LEFT = LEFT + 1
    DO WHILE (A(LEFT) < PIVOT)
       LEFT = LEFT + 1
    END DO
    IF (LEFT < RIGHT) THEN
       TEMP = A(LEFT)
       A(LEFT) = A(RIGHT)
       A(RIGHT) = TEMP

       TEMP = AI(LEFT)
       AI(LEFT) = AI(RIGHT)
       AI(RIGHT) = TEMP
    END IF
 END DO

 IF (LEFT == RIGHT) THEN
    MARKER = LEFT + 1
 ELSE
    MARKER = LEFT
 END IF

 END SUBROUTINE IMOD_UTL_PARTITION

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
   IF(X(ICOL,IROW).NE.NODATA)THEN      
    X1=(DELR(ICOL-1)+DELR(ICOL))/2.0
    X2=(DELC(IROW-1)+DELC(IROW))/2.0
    CALL IMOD_UTL_POL2DINT(XCRD,YCRD,ZCRD,C,D,NMAX,YMTMP,YNTMP,NPC,NPR,X1,X2,Y,DY,IINT,NODATA)
    X(ICOL,IROW)=Y
   ENDIF 
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
 SUBROUTINE IMOD_UTL_POL1LOCATEI(XX,N,X,J)
 !###==================================================================
 !return position such that x is within xx(j) and xx(j+1)
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: J
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(IN) :: X
 INTEGER,INTENT(IN),DIMENSION(N) :: XX
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

 END SUBROUTINE IMOD_UTL_POL1LOCATEI
 
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
! VOS=OSD_GET_OS()
! OS =0
! IF(VOS.EQ.3)OS=1
! IF(VOS.EQ.2)OS=2
! IF(VOS.EQ.4)OS=2
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
 logical :: pks7mpimasterwrite, lwrite

 LOGICAL :: LEX
 
#ifdef PKSMPI 
 lwrite = pks7mpimasterwrite()
#else
 lwrite = .true.
#endif
 
  SELECT CASE (TXTTYPE)
  !## file
  CASE (0)
   WRITE(*,'(A)') TRIM(TXT)
  !## information
  CASE (-1,1)
   WRITE(*,'(A)') TRIM(TXT)
   !IF(IFLAG(1).EQ.1)PAUSE
  !## error
  CASE (-2) ! NO STANDARD OUTPUT
  CASE (2)
   WRITE(*,'(A)')
   WRITE(*,'(A)') 'Error occured!'
   WRITE(*,'(A)') TRIM(TXT)
   IF(IFLAG(1).EQ.1)PAUSE
  CASE (3) 
   if (lwrite) WRITE(*,'(A)') TRIM(TXT) 
  CASE (-3)
   if (lwrite) then   
    WRITE(*,'(A)')
    WRITE(*,'(A)') 'Error occured!'
    WRITE(*,'(A)') TRIM(TXT) 
    IF(IFLAG(1).EQ.1)PAUSE
   end if
  CASE DEFAULT
   WRITE(*,'(A)') TRIM(TXT)
   !IF(IFLAG(1).EQ.1)PAUSE
  END SELECT

 IF(TXTTYPE.LE.0)THEN !TXTTYPE.EQ.-1.OR.TXTTYPE.EQ.-2)THEN
  IF (PRESENT(IU)) THEN
   INQUIRE(UNIT=IU,OPENED=LEX)
   IF(LEX)THEN
    WRITE(IU,'(A)') TRIM(TXT)
    CALL FLUSH(IU)
   END IF
  END IF
 END IF

 IF(TXTTYPE.EQ.2.OR.TXTTYPE.EQ.-3)THEN
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

  !###====================================================
 SUBROUTINE UTL_GETUNIQUE_INT(IX,N,NU,NODATA)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT) :: NU
 INTEGER,INTENT(INOUT),DIMENSION(N) :: IX
 INTEGER,INTENT(IN),OPTIONAL :: NODATA
 INTEGER :: I

 CALL SHELLSORT_INT(N,IX)

 !## determine number of unique classes
 IF(PRESENT(NODATA))THEN
  NU=0
  DO I=1,N
   IF(NU.EQ.0)THEN
    IF(IX(I).NE.NODATA)THEN
     NU=NU+1
     IX(NU)=IX(I)
    ENDIF
   ELSE
    IF(IX(I).NE.IX(NU).AND.IX(I).NE.NODATA)THEN
     NU    =NU+1
     IX(NU)=IX(I)
    ENDIF
   ENDIF
  END DO
 ELSE 
  !## determine number of unique classes
  NU=1
  DO I=2,N
   IF(IX(I).NE.IX(NU))THEN
    NU    =NU+1
    IX(NU)=IX(I)
   ENDIF
  END DO
 ENDIF
 
 END SUBROUTINE UTL_GETUNIQUE_INT

 !###====================================================
 SUBROUTINE SHELLSORT_INT(N,A)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,DIMENSION(N) :: A
 INTEGER :: I,J,INC
 INTEGER :: V

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

 RETURN
 END SUBROUTINE
 
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
 SUBROUTINE IMOD_UTL_READIPF(STIME,ETIME,QT,FNAME,ISS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER(KIND=8),INTENT(IN) :: stime,etime 
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 REAL,INTENT(OUT) :: QT
 INTEGER, INTENT(IN) :: ISS
 INTEGER :: IR,I,I1,I2,IU,NR,NC,IDATE,JDATE,NDATE,NAJ,N,IOS 
 REAL :: QQ,FRAC,Q1,RTIME,TTIME 
 CHARACTER(LEN=8),DIMENSION(:),ALLOCATABLE :: ATTRIB
 REAL,DIMENSION(:),ALLOCATABLE :: NODATA
 INTEGER(KIND=8) :: DBL_EDATE,DBL_SDATE 
 CHARACTER(LEN=52),DIMENSION(:),ALLOCATABLE :: QD

! STIME=SDATE
! ETIME=EDATE

 QT=0.0 
 !## transient
 IF(ISS.EQ.2)TTIME=UTL_DIFFTIME(stime,etime) !EDATE-SDATE
 
 !## open textfiles with pump information
 IU=IMOD_UTL_GETUNIT()
 OPEN(IU,FILE=FNAME,FORM='FORMATTED',STATUS='OLD',ACTION='READ',SHARE='DENYNONE')
 
 READ(IU,*,IOSTAT=IOS) NR
 IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Can not read number of rows in '//TRIM(FNAME),2)

 IF(NR.GT.0.0)THEN
  READ(IU,*,IOSTAT=IOS) NC
  IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Can not read number of columns in '//TRIM(FNAME),2)

  ALLOCATE(ATTRIB(NC),NODATA(NC),QD(NC))

  DO I=1,NC
   READ(IU,*,IOSTAT=IOS) ATTRIB(I),NODATA(I)
   IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Can not read attribute '//TRIM(IMOD_UTL_ITOS(I))//' in '//TRIM(FNAME),2)
  END DO

  I1=1
  QQ=0.0
  
  DBL_SDATE=STIME
  DO IR=1,NR
   READ(IU,*) DBL_EDATE,(QD(I),I=2,NC)
   !##steady-state
   IF(ISS.EQ.1)THEN
    !## get volume
    READ(QD(2),*) QQ; IF(QQ.EQ.NODATA(2))QQ=0.0
    QT=QT+QQ       
   ELSE
    !## make double if needed
    IF(DBL_EDATE.LT.100000000)DBL_EDATE=DBL_EDATE*1000000
    IF(DBL_EDATE.GT.STIME)THEN
     DBL_SDATE=MAX(DBL_SDATE,STIME)
     DBL_EDATE=MIN(DBL_EDATE,ETIME)
     RTIME=UTL_DIFFTIME(DBL_SDATE,DBL_EDATE)
     QT=QT+RTIME*QQ
    ENDIF
    IF(DBL_EDATE.LE.ETIME)THEN
     !## get volume
     READ(QD(2),*) QQ; IF(QQ.EQ.NODATA(2))QQ=0.0
    ENDIF
    DBL_SDATE=DBL_EDATE 
    !## stop
    IF(DBL_EDATE.GE.ETIME)EXIT
   ENDIF
  ENDDO

  !## last record probably read, extent extraction up to end of stress-period
  IF(QQ.NE.0.0.AND.IR.GT.NR)THEN
   RTIME=UTL_DIFFTIME(DBL_SDATE,ETIME)
   QT=QT+MIN(TTIME,RTIME)*QQ
  ENDIF
  
  !## steady-state
  IF(ISS.EQ.1)THEN
   IF(NR.GT.0)QT=QT/REAL(NR)
  ELSE
   QT=QT/TTIME
  ENDIF

  DEALLOCATE(ATTRIB,NODATA,QD)

 ENDIF

 CLOSE(IU)

 RETURN
 END SUBROUTINE IMOD_UTL_READIPF

  !###====================================================================
 SUBROUTINE imod_utl_ITIMETOGDATE(IDATE,IYR,IMH,IDY,IHR,IMT,ISC)
 !###====================================================================
 IMPLICIT NONE
 INTEGER(KIND=8),INTENT(IN) :: IDATE
 INTEGER,INTENT(OUT) :: IYR,IMH,IDY,IHR,IMT,ISC

 IYR =      IDATE                / 10000000000
 IMH = MOD( IDATE, 10000000000 ) / 100000000
 IDY = MOD( IDATE, 100000000 )   / 1000000
 IHR = MOD( IDATE, 1000000 )     / 10000
 IMT = MOD( IDATE, 10000 )       / 100
 ISC = MOD( IDATE, 100 )

 END SUBROUTINE imod_utl_ITIMETOGDATE

  !###====================================================================
 SUBROUTINE imod_utl_ITIMETOGTIME(ITIME,IH,IM,IS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITIME !## time seconds
 INTEGER,INTENT(OUT) :: IH,IM,IS

 IH =      ITIME         / 3600
 IM = MOD( ITIME, 3600 ) / 60
 IS = MOD( ITIME, 60 ) 

 END SUBROUTINE imod_utl_ITIMETOGTIME
 
 !###====================================================================
 SUBROUTINE IMOD_UTL_STIMETOETIME(STIME,DELT,ETIME)
 !###====================================================================
 IMPLICIT NONE
 INTEGER(KIND=8),INTENT(IN) :: STIME
 INTEGER(KIND=8),INTENT(OUT) :: ETIME
 REAL,INTENT(IN) :: DELT
 REAL :: NSR
 INTEGER :: NDAY,NS,NS1,NS2,IYR,IMH,IDY,IHR,IMT,ISC,SD

 !## get julian date
 SD=STIME/1000000
 SD=IMOD_UTL_IDATETOJDATE(SD)
 
 CALL IMOD_UTL_ITIMETOGDATE(STIME,IYR,IMH,IDY,IHR,IMT,ISC)
 !## number of seconds in that day
 NS1=REAL(IHR)*3600.0+REAL(IMT)*60.0+REAL(ISC)

 !## number of days in timestep
 NDAY= INT(DELT)

 !## increase julian date
 SD=SD+NDAY

 !## remaining timesteps in seconds
 NS2=(DELT-REAL(NDAY))*86400D0
 
 !## add to seconds in current timestep
 NS=NS1+NS2
 !## fraction in days
 NSR=REAL(NS)/86400.0
 
 NDAY=INT(NSR)
 SD=SD+NDAY
 
 !## net seconds in new timestep
 NSR=NSR-REAL(NDAY)
 
 !## get number of hours/minutes/seconds
 NS1=NSR*86400
 CALL imod_utl_ITIMETOGTIME(NS1,IHR,IMT,ISC)
 
 !## increase julian date
 ETIME=INT(IMOD_UTL_JDATETOIDATE(SD),8)
 ETIME=ETIME*1000000+IHR*10000+IMT*100+ISC
 
 END SUBROUTINE IMOD_UTL_STIMETOETIME
 
 !###====================================================================
 REAL FUNCTION UTL_DIFFTIME(SDATE,EDATE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER(KIND=8),INTENT(IN) :: SDATE,EDATE
 INTEGER :: IYR1,IMH1,IDY1,IHR1,IMT1,ISC1, &
            IYR2,IMH2,IDY2,IHR2,IMT2,ISC2
 INTEGER :: SD,ED,DD,IHR,IMT,ISC
 REAL :: F1,F2,F
 
 SD=SDATE/1000000; ED=EDATE/1000000
 SD=IMOD_UTL_IDATETOJDATE(SD); ED=IMOD_UTL_IDATETOJDATE(ED)
 DD=ED-SD

 !## start time
 CALL IMOD_UTL_ITIMETOGDATE(SDATE,IYR1,IMH1,IDY1,IHR1,IMT1,ISC1)
 F1=(REAL(IHR1)*3600.0+REAL(IMT1)*60.0+REAL(ISC1))/86400

 !## end   time
 CALL IMOD_UTL_ITIMETOGDATE(EDATE,IYR2,IMH2,IDY2,IHR2,IMT2,ISC2)
 F2=(REAL(IHR2)*3600.0+REAL(IMT2)*60.0+REAL(ISC2))/86400
  
 !## same day
 IF(SD.EQ.ED)THEN
  F=F2-F1
 ELSE
  F=1.0-F1+F2+(DD-1)
 ENDIF

 UTL_DIFFTIME=F 
 
 END FUNCTION UTL_DIFFTIME

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
 REAL,PARAMETER :: MINP=0.0 ! 5
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

!## OBSOLUTE since MAXC is 1.000.000 by default
! !## correct whenever significant resistant layer is active
! TL=0.0; JLAY=1; IL=0
! DO ILAY=1,N
!  IF(L(ILAY).GT.0.0)THEN
!   TL(JLAY)=TL(JLAY)+L(ILAY); IL(ILAY)=JLAY
!   IF(ILAY.LT.N)THEN; IF(C(ILAY).GT.MAXC)JLAY=JLAY+1; ENDIF
!  ENDIF
! ENDDO
! !## filter over more layers, significant clay in between, take longest in aquifer, MAKE OTHER ZERO!
! IF(JLAY.GT.1)THEN
!  K=0; ZM=0.0; DO ILAY=1,JLAY; IF(TL(ILAY).GT.ZM)THEN; ZM=TL(ILAY); K=ILAY ; ENDIF; ENDDO
!  DO ILAY=1,N; IF(IL(ILAY).NE.K)L(ILAY)=0.0; ENDDO
! ENDIF

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

 !## remove small transmissivities
 IF(MINKH.GT.0.0)THEN
  ZT=SUM(TLP)
  DO ILAY=1,N
   DZ=TOP(ILAY)-BOT(ILAY)
   IF(KH(ILAY)*DZ.LT.MINKH)TLP(ILAY)=0.0
  ENDDO
  IF(SUM(TLP).GT.0.0)THEN
   ZT=ZT/SUM(TLP)
   !## normalize tlp() again
   TLP=ZT*TLP
  ENDIF
 ENDIF

 !## normalize tlp() again
 IF(SUM(TLP).GT.0.0)TLP=(1.0/SUM(TLP))*TLP

 

 !## make sure only one layer is assigned whenever z1.eq.z2
 IF(IDIFF.EQ.1)THEN
  K=0; ZT=0.0; DO ILAY=1,N
   IF(ABS(TLP(ILAY)).GT.ZT)THEN
    ZT=ABS(TLP(ILAY)); K=ILAY
   ENDIF
  ENDDO
  !## not assigned to any layer
  IF(K.NE.0)THEN
  ZT=TLP(K)
  TLP=0.0; TLP(K)=1.0
  IF(ZT.LT.0.0)TLP(K)=-1.0*TLP(K)
 ENDIF
 ENDIF

!## nothing in model, whenever system on top of model, put them in first modellayer with thickness
 IF(SUM(TLP).EQ.0.0)THEN
  IF(Z1.GE.TOP(1))THEN
   TLP(1)=1.0
  ENDIF
 ENDIF

 !## if no layers has been used for the assignment, try to allocate it to aquifer of this interbed
 IF(SUM(TLP).LE.0)THEN
  TLP=0
  DO ILAY=1,N-1
   IF(BOT(ILAY).GE.Z1.AND.TOP(ILAY+1).LE.Z2)THEN; TLP(ILAY)=1.0; EXIT; ENDIF
  ENDDO
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
 SUBROUTINE IMOD_UTL_REL_TO_ABS(ROOT,FNAME)
 !###======================================================================
 
 IMPLICIT NONE

 CHARACTER(LEN=*), INTENT(INOUT) :: ROOT
 CHARACTER(LEN=*), INTENT(INOUT) :: FNAME
 INTEGER :: M, N, IL, I
 CHARACTER(LEN=1) :: SLASH
 LOGICAL :: LREL  
 
 N = LEN_TRIM(FNAME)
 IF (N==0) RETURN
 CALL IMOD_UTL_SWAPSLASH(FNAME)
 CALL IMOD_UTL_GETSLASH(SLASH)
 FNAME = ADJUSTL(FNAME)
 N = LEN_TRIM(FNAME)
 
 CALL IMOD_UTL_SWAPSLASH(ROOT)
 M = LEN_TRIM(ROOT)
 IF(ROOT(M:M).EQ.SLASH) THEN
    ROOT = ROOT(1:M-1)  
    M = M - 1
 END IF
 IL = M + 1
 
 LREL = .FALSE.
 DO WHILE(.TRUE.)
    IF(FNAME(1:1).NE.'.')EXIT 
    IF(FNAME(1:2).EQ.'.'//SLASH)THEN
       LREL = .TRUE. 
       FNAME = FNAME(3:N)
       N = LEN_TRIM(FNAME)
    END IF
    IF(FNAME(1:3).EQ.'..'//SLASH)THEN
       LREL = .TRUE. 
       FNAME = FNAME(4:N)
       N = LEN_TRIM(FNAME)
       IL = INDEX(ROOT(1:IL-1),SLASH,BACK=.TRUE.)
    END IF
 END DO
 IF(LREL) THEN
    FNAME = ROOT(1:IL-1)//SLASH//TRIM(FNAME)
 END IF
 
 END SUBROUTINE IMOD_UTL_REL_TO_ABS
 
 !###====================================================================== 
 SUBROUTINE IMOD_UTL_ABS_PATH(PATH)
 !###======================================================================
 
 IMPLICIT NONE
 CHARACTER(LEN=*), INTENT(INOUT) :: PATH
 INTEGER :: LUN, I, N
 CHARACTER(LEN=1024) :: ABSPATH

 CALL IMOD_UTL_CREATEDIR(PATH)
 N = LEN_TRIM(PATH)
 IF(PATH(N:N).EQ.'\'.OR.PATH(N:N).EQ.'/')THEN
    ABSPATH = TRIM(PATH)//'TEST.TXT'
 ELSE    
    ABSPATH = TRIM(PATH)//'\TEST.TXT'
 END IF
 N = LEN_TRIM(ABSPATH)
#ifdef PKSMPI
 CALL PKS7MPIFNAME(ABSPATH,N)
#endif 
 CALL IMOD_UTL_SWAPSLASH(ABSPATH)
 IF(ABSPATH(1:2).EQ.'./')THEN
    ABSPATH = ABSPATH(3:LEN_TRIM(ABSPATH))
 END IF   
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

 DOUBLE PRECISION,PARAMETER :: TINY=0.001D0
 DOUBLE PRECISION :: XD, YD

 REAL :: DX,DY
 INTEGER :: I

 ICOL=0; IROW=0
 XD = DBLE(XC); YD = DBLE(YC)
  
 IF(IEQ.EQ.0)THEN

  dx=xc-delr(0)
  i=0; if(mod(dx,simcsize).ne.0.0)i=1
  if(xc+TINY.gt.delr(0).and.xc-TINY.lt.delr(ncol))icol=int(dx/simcsize)+i

  dy=delc(0)-yc
  i=0; if(mod(dy,simcsize).ne.0.0)i=1
  if(yc+TINY.gt.delc(nrow).and.yc-TINY.lt.delc(0))irow=int(dy/simcsize)+i

  icol=min(icol,ncol); irow=min(irow,nrow)

 ELSE

  CALL IMOD_UTL_POL1LOCATED(DELR,NCOL+1,XD,ICOL)
  CALL IMOD_UTL_POL1LOCATED(DELC,NROW+1,YD,IROW)
  IF(ICOL.LT.0.OR.ICOL.GT.NCOL) ICOL=0
  IF(IROW.LT.0.OR.IROW.GT.NROW) IROW=0
  
 ENDIF

 END SUBROUTINE IMOD_UTL_GETICIR

!###======================================================================
 SUBROUTINE IMOD_UTL_INTERSECT_EQUI(XMIN,XMAX,YMIN,YMAX,CSX_IN,CSY_IN,XIN1,XIN2,YIN1,YIN2,N,LHFB)  
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: XMIN,XMAX,YMIN,YMAX,CSX_IN,CSY_IN
 REAL,INTENT(IN) :: XIN1,XIN2,YIN1,YIN2
 INTEGER,INTENT(OUT) :: N
 LOGICAL,INTENT(IN) :: LHFB
 REAL :: X,Y,XMN,XMX,YMN,YMX,DX,DY,LENG,TD,X1,X2,Y1,Y2,CSX,CSY
 INTEGER :: I,ICOL,IROW,ID,N_IN
 
 N=0; X1=XIN1; Y1=YIN1; X2=XIN2; Y2=YIN2
 
 IF(.NOT.ASSOCIATED(XA))ALLOCATE(XA(1000)); IF(.NOT.ASSOCIATED(YA))ALLOCATE(YA(1000))
 IF(.NOT.ASSOCIATED(FA))ALLOCATE(FA(1000)); IF(.NOT.ASSOCIATED(LN))ALLOCATE(LN(1000))
 IF(.NOT.ASSOCIATED(CA))ALLOCATE(CA(1000)); IF(.NOT.ASSOCIATED(RA))ALLOCATE(RA(1000))

 CSX=CSX_IN
 CSY=CSY_IN
 IF(LHFB)THEN
  CSX=CSX/2.0
  CSY=CSY/2.0
 ENDIF
 
 N_IN=N

 IF(.NOT.INTERSECT_EQUATION(XMIN,XMAX,YMIN,YMAX,X1,X2,Y1,Y2,N)) RETURN !,XA,YA,ZA,FA,LN,CA,RA))RETURN
 
 !## find search box - result can be negative, does not matter!
 IF(X1.LT.XMIN)THEN
  XMN=XMIN
 ELSE 
  I=1
  XMN=XMIN+CSX*(INT((X1-XMIN)/CSX)+I)
 ENDIF
 IF(X2.GT.XMAX)THEN
  XMX=XMAX
 ELSE 
  I=0
  DX=X2-XMIN
  IF(MOD(DX,CSX).EQ.0.0)I=-1
  XMX=XMIN+CSX*(INT((X2-XMIN)/CSX)+I) 
 ENDIF
 
 Y=MIN(Y1,Y2)
 IF(Y.LT.YMIN)THEN
  YMN=YMIN
 ELSE
  I =0
  DY=YMAX-Y
  IF(MOD(DY,CSY).EQ.0.0)I=-1
  YMN=YMAX-CSY*(INT((YMAX-Y)/CSY)+I)
 ENDIF
 Y=MAX(Y1,Y2)
 IF(Y.GT.YMAX)THEN
  YMX=Y
 ELSE
  I =1
  YMX=YMAX-CSY*(INT((YMAX-Y)/CSY)+I)
 ENDIF
 
 !## not for horizontal lines
 IF(IHOR.EQ.0)THEN
 
  !## continue seach rest of intersections
  !## try intersections with x-axes firstly
  Y=YMN-CSY
  DO
   Y=Y+CSY
   IF(Y.GT.YMX)EXIT

   !## array overwritten
   N=N+1; CALL INTERSECT_RESIZEVECTORS(N) !,XA,YA,ZA,FA,LN,CA,RA) 

   IF(IVER.EQ.1)THEN
    XA(N)=X1 !## same as xmx
   ELSE
    XA(N)=(Y-B)/A
   ENDIF
   YA(N)=Y
 
  ENDDO

 ENDIF

 !## not for vertical lines
 IF(IVER.EQ.0)THEN 

  !## try intersections with y-axes secondly
  X=XMN-CSX
  DO
   X=X+CSX
   IF(X.GT.XMX)EXIT

   !## array overwritten
   N=N+1; CALL INTERSECT_RESIZEVECTORS(N) !,XA,YA,ZA,FA,LN,CA,RA) 
   XA(N)=X
   IF(IHOR.EQ.1)THEN
    YA(N)=Y1  !## same as ymx
   ELSE
    YA(N)=A*X+B
   ENDIF
 
  ENDDO
 
 ENDIF
 
 CSX=CSX_IN
 CSY=CSY_IN

 DX=X1-X2; DY=Y2-Y1
 CALL INTERSECT_SORT(DX,DY,N_IN+1,N)

 !## sample each of the point to determine irow/icol, overwrite point with this
 !## skip first and last, they represented already by the second and one-last point
 DO I=N_IN+2,N 

  !## mid point
  X   =(XA(I-1)+XA(I))/2.0
  Y   =(YA(I-1)+YA(I))/2.0
  IF(X.GE.XMIN)THEN
   ICOL=INT((X-XMIN)/CSX)+1
  ELSE
   ICOL=INT((X-XMIN)/CSX)-1
  ENDIF
  IF(Y.LE.YMAX)THEN  
   IROW=INT((YMAX-Y)/CSY)+1
  ELSE
   IROW=INT((YMAX-Y)/CSY)-1
  ENDIF
  
  TD=CSX*CSY; ID=0 !## fraction
  CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,XMIN+(ICOL-1)*CSX,YMAX-(IROW-1)*CSY,X,Y,1)
  CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,XMIN+ ICOL   *CSX,YMAX-(IROW-1)*CSY,X,Y,2)
  CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,XMIN+ ICOL   *CSX,YMAX- IROW   *CSY,X,Y,3)
  CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,XMIN+(ICOL-1)*CSX,YMAX- IROW   *CSY,X,Y,4)
  FA(I-1)=REAL(ID) 
  
  DX  =XA(I)-XA(I-1)
  DY  =YA(I)-YA(I-1)
  LENG=DX**2.0+DY**2.0; IF(LENG.GT.0.0)LENG=SQRT(LENG)

  !## store results in row/column indices
  CA(I-1)=REAL(ICOL)
  RA(I-1)=REAL(IROW)

  LN(I-1)=LENG
 END DO
 N=N-1

 END SUBROUTINE IMOD_UTL_INTERSECT_EQUI

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
 SUBROUTINE IMOD_UTL_INTERSECT_NONEQUI(DELR,DELC,NROW,NCOL,XIN1,XIN2,YIN1,YIN2,N,LHFB)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL
 REAL,INTENT(IN),DIMENSION(0:NCOL) :: DELR
 REAL,INTENT(IN),DIMENSION(0:NROW) :: DELC
 REAL,INTENT(IN) :: XIN1,XIN2,YIN1,YIN2
 INTEGER,INTENT(OUT) :: N
 LOGICAL,INTENT(IN) :: LHFB
 REAL :: X,Y,XMN,XMX,YMN,YMX,DX,DY,LENG,XMIN,YMIN,XMAX,YMAX,X1,X2,Y1,Y2,CS,TD
 INTEGER :: I,ICOL,IROW,IMN,JMN,ID,N_IN
 
 X1=XIN1; Y1=YIN1; X2=XIN2; Y2=YIN2
 
 IF(.NOT.ASSOCIATED(XA))ALLOCATE(XA(1000)); IF(.NOT.ASSOCIATED(YA))ALLOCATE(YA(1000))
 IF(.NOT.ASSOCIATED(FA))ALLOCATE(FA(1000)); IF(.NOT.ASSOCIATED(LN))ALLOCATE(LN(1000))
 IF(.NOT.ASSOCIATED(CA))ALLOCATE(CA(1000)); IF(.NOT.ASSOCIATED(RA))ALLOCATE(RA(1000))

 XMIN=DELR(0)
 XMAX=DELR(NCOL)
 YMIN=DELC(NROW)
 YMAX=DELC(0)

 N_IN=N
 
 IF(.NOT.INTERSECT_EQUATION(XMIN,XMAX,YMIN,YMAX,X1,X2,Y1,Y2,N))RETURN
 
 !## continue search rest of intersections
 !## try intersections with y-axes firstly
 IMN=0; DO I=1,NROW 
  IF(DELC(I).GT.MIN(Y1,Y2))THEN; YMN=DELC(I); IMN=I; ENDIF
 END DO
 JMN=0; DO I=NROW,1,-1 
  IF(DELC(I).LT.MAX(Y1,Y2))THEN; YMX=DELC(I); JMN=I; ENDIF
 END DO

 IF(IMN.GT.0.AND.JMN.GT.0)THEN

  !## not for horizontal lines
  IF(IHOR.EQ.0)THEN

   CS=DELC(IMN-1)-DELC(IMN)
   IF(LHFB)CS=CS/2.0
   Y =YMN-CS
   DO

    Y=Y+CS
    IF(Y.GT.YMX)EXIT
    N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
    IF(IVER.EQ.1)THEN
     XA(N)=X1 !## same as xmx
    ELSE
     XA(N)=(Y-B)/A
    ENDIF
    YA(N)=Y

    !## double intersections, for better estimate for hfb
    IF(LHFB)THEN
     Y=Y+CS
     !## array overwritten
     N=N+1; CALL INTERSECT_RESIZEVECTORS(N)
     IF(IVER.EQ.1)THEN
      XA(N)=X1 !## same as xmx
     ELSE
      XA(N)=(Y-B)/A
     ENDIF
     YA(N)=Y
    ENDIF
  
    IMN  =IMN-1
    !## model is not bigger than line-segment
    IF(IMN.LE.0)EXIT
    CS=DELC(IMN-1)-DELC(IMN)
    IF(LHFB)CS=CS/2.0
   ENDDO

  ENDIF
 ENDIF

 !## try intersections with x-axes secondly
 IMN=0; DO I=NCOL,1,-1 
  IF(DELR(I).GT.X1)THEN; XMN=DELR(I); IMN=I; ENDIF
 END DO
 JMN=0; DO I=0,NCOL
  IF(DELR(I).LT.X2)THEN; XMX=DELR(I); JMN=I; ENDIF
 END DO

 IF(IMN.GT.0.AND.JMN.GT.0)THEN

  !## not for vertical lines
  IF(IVER.EQ.0)THEN

   CS=DELR(IMN-1)-DELR(IMN)
   IF(LHFB)CS=CS/2.0
   X =XMN-CS
   DO
   
    X=X+CS
    IF(X.GT.XMX)EXIT
    N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
    XA(N)=X
    IF(IHOR.EQ.1)THEN
     YA(N)=Y1  !## same as ymx
    ELSE
     YA(N)=A*X+B
    ENDIF

    !## double intersections, for better estimate for hfb
    IF(LHFB)THEN
     X=X+CS
     !## array overwritten
     N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
     XA(N)=X
     IF(IHOR.EQ.1)THEN
      YA(N)=Y1  !## same as ymx
     ELSE
      YA(N)=A*X+B
     ENDIF
    ENDIF
  
    IMN  =IMN+1
    !## model is not bigger than line-segment
    IF(IMN.GT.NCOL)EXIT
    CS=DELR(IMN)-DELR(IMN-1)
    IF(LHFB)CS=CS/2.0
   ENDDO

  ENDIF
 ENDIF

 !## sort intersections, determined by the one with the largest difference
 DX=X1-X2; DY=Y2-Y1
 CALL INTERSECT_SORT(DX,DY,N_IN+1,N)

 !## sample each of the point to determine irow/icol, overwrite point with this
 !## skip first and last, they represented already by the second and one-last point
 DO I=N_IN+2,N
  !## mid point
  X   =(XA(I-1)+XA(I))/2.0
  Y   =(YA(I-1)+YA(I))/2.0

  CALL IMOD_UTL_POL1LOCATED(DELR,NCOL+1,REAL(X,8),ICOL)
  CALL IMOD_UTL_POL1LOCATED(DELC,NROW+1,REAL(Y,8),IROW)
  
  TD=CS*2.0; ID=0 !## fraction
  IF(ICOL.GE.1.AND.ICOL.LE.NCOL.AND. &
     IROW.GE.1.AND.IROW.LE.NROW)THEN
   CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,DELR(ICOL-1),DELC(IROW-1),X,Y,1)
   CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,DELR(ICOL)  ,DELC(IROW-1),X,Y,2)
   CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,DELR(ICOL)  ,DELC(IROW)  ,X,Y,3)
   CALL IMOD_UTL_INTERSECT_NCORNER(ID,TD,DELR(ICOL-1),DELC(IROW)  ,X,Y,4)
  ELSE
   ICOL=0; IROW=0
  ENDIF
  
  FA(I-1)=REAL(ID) 

  DX  =XA(I)-XA(I-1)
  DY  =YA(I)-YA(I-1)
  LENG=DX**2.0+DY**2.0; IF(LENG.GT.0.0)LENG=SQRT(LENG)

  !## store results in row/column indices
  CA(I-1)=REAL(ICOL)
  RA(I-1)=REAL(IROW)

  LN(I-1)=LENG
 END DO
 N=N-1

 END SUBROUTINE IMOD_UTL_INTERSECT_NONEQUI
!###======================================================================
 SUBROUTINE INTERSECT_NULLIFY() 
 !###======================================================================
 IMPLICIT NONE
 
 NULLIFY(XA,YA,FA,LN,CA,RA)
  
 END SUBROUTINE INTERSECT_NULLIFY

 !###======================================================================
 SUBROUTINE INTERSECT_DEALLOCATE() 
 !###======================================================================
 IMPLICIT NONE

 IF(ASSOCIATED(XA))DEALLOCATE(XA)
 IF(ASSOCIATED(YA))DEALLOCATE(YA) 
 IF(ASSOCIATED(FA))DEALLOCATE(FA) 
 IF(ASSOCIATED(LN))DEALLOCATE(LN)
 IF(ASSOCIATED(CA))DEALLOCATE(CA)
 IF(ASSOCIATED(RA))DEALLOCATE(RA)
  
 END SUBROUTINE INTERSECT_DEALLOCATE
 
 !###======================================================================
 SUBROUTINE INTERSECT_RESIZEVECTORS(N) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: DN=100
 INTEGER,INTENT(IN) :: N
 INTEGER :: MM,NN,I
 
 IF(N.LE.SIZE(XA))RETURN
 
 NN=SIZE(XA)
 MM=NN+DN
 
 ALLOCATE(XA_DUMMY(MM),YA_DUMMY(MM),FA_DUMMY(MM),LN_DUMMY(MM),CA_DUMMY(MM),RA_DUMMY(MM))

 DO I=1,NN
  XA_DUMMY(I)=XA(I)
  YA_DUMMY(I)=YA(I)
  FA_DUMMY(I)=FA(I)
  LN_DUMMY(I)=LN(I)
  CA_DUMMY(I)=CA(I)
  RA_DUMMY(I)=RA(I)
 ENDDO
 
 DEALLOCATE(XA,YA,FA,LN,CA,RA)
 
 XA=>XA_DUMMY
 YA=>YA_DUMMY
 FA=>FA_DUMMY
 LN=>LN_DUMMY
 CA=>CA_DUMMY
 RA=>RA_DUMMY
 
 END SUBROUTINE INTERSECT_RESIZEVECTORS
 
 !###======================================================================
 LOGICAL FUNCTION INTERSECT_EQUATION(XMIN,XMAX,YMIN,YMAX,X1,X2,Y1,Y2,N)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: XMIN,XMAX,YMIN,YMAX
 REAL,INTENT(INOUT) :: X1,X2,Y1,Y2
 INTEGER,INTENT(INOUT) :: N
 DOUBLE PRECISION :: X,Y,DX,DY

 INTERSECT_EQUATION=.FALSE.

 XBEGIN=X1; YBEGIN=Y1

 !## arrange x1,x2,y1,y2 such that x1<x2
 IF(X1.GT.X2)THEN
  X =X1; Y =Y1
  X1=X2; Y1=Y2
  X2=X;  Y2=Y
 ENDIF

 !## use always mid between point x1,y1 and x2,y2 as first position
 N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
 XA(N)= X1; YA(N)= Y1
 N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
 XA(N)= X1; YA(N)= Y1

 N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
 XA(N)= (X1+X2)/2.0; YA(N)= (Y1+Y2)/2.0

 N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
 XA(N)= X2; YA(N)= Y2
 N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
 XA(N)= X2; YA(N)= Y2

 !## find mathematical expression for line: y=ax+b
 DX=X2-X1; DY=Y2-Y1
 IVER=0; IHOR=0
 IF(DX.EQ.0.0)IVER=1
 IF(DY.EQ.0.0)IHOR=1
 IF(DX.NE.0.0)THEN 
  A=DY/DX
  B=Y1-A*X1
 ENDIF

 INTERSECT_EQUATION=.TRUE.

 END FUNCTION INTERSECT_EQUATION

 !###======================================================================
 SUBROUTINE INTERSECT_SORT(DX,DY,N_IN,N)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: DX,DY
 INTEGER,INTENT(IN) :: N,N_IN
 DOUBLE PRECISION :: X,Y
 REAL :: LENG
 INTEGER :: I

 !## sort intersections, determined by the one with the largest difference
 IF(ABS(DX).GE.ABS(DY))THEN
  CALL QKSORTDOUBLE2(XA(N_IN:),YA(N_IN:),(N-N_IN)+1)
 ELSE
  CALL QKSORTDOUBLE2(YA(N_IN:),XA(N_IN:),(N-N_IN)+1)
 ENDIF

 !## resort - if neccessary
 IF(XA(N_IN).NE.XBEGIN.OR.YA(N_IN).NE.YBEGIN)THEN
  DO I=N_IN,N_IN+((N-N_IN)/2)
   X           =XA(I)
   XA(I)       =XA(N-I+N_IN) 
   XA(N-I+N_IN)=X
   Y           =YA(I)
   YA(I)       =YA(N-I+N_IN) 
   YA(N-I+N_IN)=Y
   LENG        =LN(I)
   LN(I)       =LN(N-I+N_IN)
   LN(N-I+N_IN)=LENG
  END DO
 ENDIF

 END SUBROUTINE INTERSECT_SORT

 !====================================================
 SUBROUTINE QKSORTDOUBLE2(ARR,BRR,N)
 !====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 DOUBLE PRECISION,INTENT(INOUT),DIMENSION(N) :: ARR,BRR
 INTEGER   M,NSTACK
 PARAMETER (M=7,NSTACK=50)
 INTEGER   I,IR,J,JSTACK,K,L,ISTACK(NSTACK)
 DOUBLE PRECISION :: A,B,ATEMP,BTEMP

 JSTACK=0
 L=1
 IR=N
 1 IF(IR-L.LT.M)THEN
  DO J=L+1,IR
   A=ARR(J)
   B=BRR(J)
   DO I=J-1,1,-1
    IF(ARR(I).LE.A)GOTO 2
    ARR(I+1)=ARR(I)
    BRR(I+1)=BRR(I)
   ENDDO
   I=0
 2 ARR(I+1)=A
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
 3  CONTINUE
  I=I+1
  IF(ARR(I).LT.A)GOTO 3
 4  CONTINUE
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
 5  ARR(L)=ARR(J)
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

 END SUBROUTINE

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
 
 !###====================================================================
 SUBROUTINE IMOD_UTL_DELETE_BY_UNIT(IU)
 !###====================================================================
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: IU
 CHARACTER(LEN=256) :: FNAME
 LOGICAL :: LOP
 INTEGER :: JU
 
 INQUIRE(UNIT=IU,OPENED=LOP,NAME=FNAME)
 IF(.NOT.LOP)RETURN
 CLOSE(IU)
 JU = GETUNIT()
 OPEN(UNIT=JU,FILE=FNAME,STATUS='UNKNOWN')
 CLOSE(JU,STATUS='DELETE')

 RETURN
 END SUBROUTINE IMOD_UTL_DELETE_BY_UNIT

END MODULE IMOD_UTL