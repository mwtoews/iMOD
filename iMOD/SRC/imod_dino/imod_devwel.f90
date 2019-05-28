!!  Copyright (C) Stichting Deltares, 2005-2019.
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
!!
MODULE MOD_DEVWEL

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : DP_KIND,SP_KIND
USE MOD_IDFPLOT
USE MOD_MANAGER_UTL
USE MOD_UTL, ONLY : UTL_CREATEDIR,UTL_GENLABELSREAD,NV,NL,VAR,UTL_GENLABELSDEALLOCATE,UTL_GETUNIT, &
   MAXLEN,ITOS,UTL_CAP,UTL_ROTATE_XYZ,PI,RTOS,UTL_DATA_CSV,ICOL_VAR,IACT_VAR,CCNST,UTL_WSELECTFILE,UTL_MF2005_MAXNO
USE MOD_GENPLOT, ONLY : GEN_INIT
USE MOD_DEVWEL_PAR

CONTAINS

 !###======================================================================
 SUBROUTINE DEVFAULT_IMPORT
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU,JU,IOS,I,J,M,NP
 CHARACTER(LEN=256) :: LINE,FNAME
 REAL(KIND=DP_KIND),DIMENSION(:,:),ALLOCATABLE :: X,Y,Z

 IF(.NOT.UTL_WSELECTFILE('Load ASC File (*.asc)|*.asc|',&
                  LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                  'Load ASC File (*.asc)'))RETURN

 !## process data 
 IU=UTL_GETUNIT(); OPEN(IU,FILE=FNAME,STATUS='OLD'    ,ACTION='READ' )
 FNAME=FNAME(:INDEX(FNAME,'.',.TRUE.))//'GEN'
 JU=UTL_GETUNIT(); OPEN(JU,FILE=FNAME,STATUS='UNKNOWN',ACTION='WRITE')

 DO I=1,7 ; READ(IU,*); ENDDO
 NP=0; DO
  READ(IU,'(10X,A1)') LINE
  IF(UTL_CAP(LINE(1:1),'U').NE.'X')EXIT
  NP=NP+1
 ENDDO
 DO I=1,8 ; READ(IU,*); ENDDO

 ALLOCATE(X(NP,2),Y(NP,2),Z(NP,2))
 
 READ(IU,'(A5)',IOSTAT=IOS) LINE

 !## start processing the lines
 M=0; DO
  IF(IOS.NE.0)EXIT

  !## start new fault
  IF(TRIM(UTL_CAP(LINE(1:5),'U')).EQ.'FAULT')THEN
   READ(IU,'(A256)') LINE
   J=0; DO
    J=J+1
    READ(LINE,'(10X,3(F15.0,1X))') X(1,2),Y(1,2),Z(1,2)
    DO I=2,NP
     READ(IU,'(10X,3(F15.0,1X))') X(I,2),Y(I,2),Z(I,2)
    ENDDO
    DO I=1,3; READ(IU,*); ENDDO

    READ(IU,'(A256)',IOSTAT=IOS) LINE

    !## write GEN-file
    IF(J.GT.1)THEN
     DO I=1,NP-1
      M=M+1
      WRITE(JU,*) M
      WRITE(JU,'(3(F15.7,1X))') X(I  ,1),Y(I  ,1),Z(I  ,1)
      WRITE(JU,'(3(F15.7,1X))') X(I+1,1),Y(I+1,1),Z(I+1,1)
      WRITE(JU,'(3(F15.7,1X))') X(I+1,2),Y(I+1,2),Z(I+1,2)
      WRITE(JU,'(3(F15.7,1X))') X(I  ,2),Y(I  ,2),Z(I  ,2)
      WRITE(JU,'(3(F15.7,1X))') X(I  ,1),Y(I  ,1),Z(I  ,1)
      WRITE(JU,'(A)') 'END'
     ENDDO
    ENDIF

    IF(IOS.NE.0)EXIT
    !## stop reading pillars
    IF(TRIM(UTL_CAP(LINE(1:6),'U')).NE.'PILLAR')EXIT

    DO I=1,NP; X(I,1)=X(I,2); Y(I,1)=Y(I,2); Z(I,1)=Z(I,2); ENDDO

   ENDDO
   
  ENDIF
 
 ENDDO 
 WRITE(JU,'(A)') 'END'
 
!FAULT     "Fault 1" 0    
!PILLAR    445205.72190970 6151162.82328465 -1910.15419048
!          445205.72190970 6151162.82328465 -1568.19109181
!          445205.72190970 6151162.82328465 -1226.22799315
!          445205.72190970 6151162.82328465 -884.26489449
!          445205.72190970 6151162.82328465 -542.30179583
!          0.00000000 0.00000000
!          0     UNDEF UNDEF UNDEF 
!          Linear A-DIR UNDEF SEGMENT FALSE
!PILLAR    445610.19806679 6151456.78699928 -1905.14043167
!          445610.19806679 6151456.78699928 -1563.54074399
!          445610.19806679 6151456.78699928 -1221.94105630
!          445610.19806679 6151456.78699928 -880.34136862
!          445610.19806679 6151456.78699928 -538.74168093
!          0.00000000 0.00000000
!          0     UNDEF UNDEF UNDEF 
!          Linear A-DIR UNDEF SEGMENT FALSE

 CLOSE(IU); CLOSE(JU)
 DEALLOCATE(X,Y,Z)

 CALL GEN_INIT(GENNAME=FNAME,GENCOLOUR=WRGB(255,0,0))
 CALL IDFPLOTFAST(0)

 END SUBROUTINE DEVFAULT_IMPORT

 !###======================================================================
 SUBROUTINE DEVWELL_IMPORT_MULTIPLE()
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I,NLC
 CHARACTER(LEN=256),DIMENSION(4) :: FNAME
 CHARACTER(LEN=256) :: IPFFNAME
 LOGICAL :: LEX
 INTEGER,DIMENSION(:),POINTER :: ICOLS

 CALL WDIALOGLOAD(ID_DIMPORTDEVCSV,ID_DIMPORTDEVCSV)
 CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONOPEN,1); CALL WDIALOGPUTIMAGE(ID_OPEN2,ID_ICONOPEN,1)
 CALL WDIALOGPUTIMAGE(ID_OPEN3,ID_ICONOPEN,1); CALL WDIALOGPUTIMAGE(ID_OPEN4,ID_ICONOPEN,1)
 CALL WDIALOGSHOW(-1,-1,0,3)
  
 CALL WDIALOGPUTSTRING(IDF_STRING1,'D:\IMOD-MODELS\ALBERTA\AGS\DBASE\CSV\TEST\HEADERS_10TM.CSV')
 CALL WDIALOGPUTSTRING(IDF_STRING2,'D:\IMOD-MODELS\ALBERTA\AGS\DBASE\CSV\TEST\DIRECTIONAL_SURVEYS.CSV')
 CALL WDIALOGPUTSTRING(IDF_STRING3,'D:\IMOD-MODELS\ALBERTA\AGS\DBASE\CSV\TEST\COMPLETIONS.CSV')
 CALL WDIALOGPUTSTRING(IDF_STRING4,'D:\IMOD-MODELS\ALBERTA\AGS\DBASE\CSV\TEST\CORES.CSV')

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDOK)
      FNAME=''
      CALL WDIALOGGETSTRING(IDF_STRING1,FNAME(1)); INQUIRE(FILE=FNAME(1),EXIST=LEX)
      IF(.NOT.LEX)THEN; CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Given filename '//TRIM(FNAME(1))//CHAR(13)//'does not exist','Error'); CYCLE; ENDIF
      CALL WDIALOGGETSTRING(IDF_STRING2,FNAME(2)); INQUIRE(FILE=FNAME(2),EXIST=LEX)
      IF(.NOT.LEX)THEN; CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Given filename '//TRIM(FNAME(2))//CHAR(13)//'does not exist','Error'); CYCLE; ENDIF
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
      IF(I.EQ.1)THEN
       CALL WDIALOGGETSTRING(IDF_STRING3,FNAME(3)); INQUIRE(FILE=FNAME(3),EXIST=LEX)
       IF(.NOT.LEX)THEN; CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Given filename '//TRIM(FNAME(3))//CHAR(13)//'does not exist','Error'); CYCLE; ENDIF
      ELSE
       FNAME(3)=''
      ENDIF
      CALL WDIALOGGETCHECKBOX(IDF_CHECK2,I)
      IF(I.EQ.1)THEN
       CALL WDIALOGGETSTRING(IDF_STRING4,FNAME(4)); INQUIRE(FILE=FNAME(4),EXIST=LEX)
       IF(.NOT.LEX)THEN; CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Given filename '//TRIM(FNAME(4))//CHAR(13)//'does not exist','Error'); CYCLE; ENDIF
      ELSE
       FNAME(4)=''
      ENDIF
      EXIT
     CASE (IDCANCEL)
      EXIT
     CASE (IDHELP)
    END SELECT
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (ID_OPEN1)
      IF(.NOT.UTL_WSELECTFILE('Comma Separated File (*.csv)|*.csv|',LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,&
      FNAME(1),'Open CSV for Well Headers (*.csv)'))CYCLE
      CALL WDIALOGPUTSTRING(IDF_STRING1,TRIM(FNAME(1)))
     CASE (ID_OPEN2)
      IF(.NOT.UTL_WSELECTFILE('Comma Separated File (*.csv)|*.csv|',LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,&
      FNAME(2),'Open CSV for Directional Survey (*.csv)'))CYCLE
      CALL WDIALOGPUTSTRING(IDF_STRING2,TRIM(FNAME(2)))
     CASE (ID_OPEN3)
      IF(.NOT.UTL_WSELECTFILE('Comma Separated File (*.csv)|*.csv|',LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,&
      FNAME(3),'Open CSV for Completion Intervals (*.csv)'))CYCLE
      CALL WDIALOGPUTSTRING(IDF_STRING3,TRIM(FNAME(3)))
     CASE (ID_OPEN4)
      IF(.NOT.UTL_WSELECTFILE('Comma Separated File (*.csv)|*.csv|',LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,&
      FNAME(4),'Open CSV for Cored Intervals (*.csv)'))CYCLE
      CALL WDIALOGPUTSTRING(IDF_STRING4,TRIM(FNAME(4)))
     CASE (IDF_CHECK1)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
      CALL WDIALOGFIELDSTATE(IDF_STRING3,I)
      CALL WDIALOGFIELDSTATE(ID_OPEN3,I)
     CASE (IDF_CHECK2)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK2,I)
      CALL WDIALOGFIELDSTATE(IDF_STRING4,I)
      CALL WDIALOGFIELDSTATE(ID_OPEN4,I)
    END SELECT
  END SELECT
 ENDDO

 CALL WDIALOGUNLOAD(); IF(MESSAGE%VALUE1.EQ.IDCANCEL)RETURN
 CALL UTL_MESSAGEHANDLE(0)
 LEX=.FALSE.
 IF(DEVWELL_IMPORT_MULTIPLE_FILE1(FNAME(1)))THEN
  IF(DEVWELL_IMPORT_MULTIPLE_FILE2(FNAME(2)))THEN
   IF(DEVWELL_IMPORT_MULTIPLE_FILE3(FNAME(3)))THEN
    IF(DEVWELL_IMPORT_MULTIPLE_FILE4(FNAME(4)))THEN
     LEX=DEVWELL_IMPORT_MULTIPLE_WRITE(LEN_TRIM(FNAME(3)),LEN_TRIM(FNAME(4)))
    ENDIF
   ENDIF
  ENDIF
 ENDIF
 CALL UTL_MESSAGEHANDLE(1)

 IF(LEX)THEN

  !## construct temporary CSV file and import it via devwell_import()
  IF(UTL_WSELECTFILE('iMOD IPF File (*.ipf)|*.ipf|',SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,&
    IPFFNAME,'Save iMOD IPF file (*.ipf)'))THEN

   NLC=0
   IF(LEN_TRIM(FNAME(3))+LEN_TRIM(FNAME(3)).GT.0)NLC=NLC+1
   ALLOCATE(ICOLS(7+NLC))
   ICOLS=0; DO I=1,7+NLC; ICOLS(I)=I; ENDDO
   CALL DEVWELL_IMPORT(TRIM(PREFVAL(1))//'\TMP\DEVWEL_DUMMY.CSV',IPFFNAME,ICOLS,1)
   IF(ASSOCIATED(ICOLS))DEALLOCATE(ICOLS)
   !## file to the iMOD Manager
   CALL MANAGER_UTL_ADDFILE(IDFNAMEGIVEN=IPFFNAME)
   CALL IDFPLOTFAST(0)
  ENDIF
 
 ENDIF
 
 CALL DEVWELL_IMPORT_MULTIPLE_DEALLOCATE()
 
 END SUBROUTINE DEVWELL_IMPORT_MULTIPLE
 
 !###======================================================================
 LOGICAL FUNCTION DEVWELL_IMPORT_MULTIPLE_WRITE(INT1,INT2)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: INT1,INT2
 CHARACTER(LEN=256) :: FNAME,LINE
 CHARACTER(LEN=52) :: CTYPE
 INTEGER :: II,IOS,IU,I,J,JJ,K,N,N1,N2,N3,IVERTICAL,K1,K2,NERROR
 REAL(KIND=DP_KIND) :: Z1,Z2,Z,MINZ,DZ
 LOGICAL :: LEX
 
 DEVWELL_IMPORT_MULTIPLE_WRITE=.FALSE.
 
 FNAME=TRIM(PREFVAL(1))//'\TMP\DEVWEL_DUMMY.CSV'
 
 !## write csv
 IU=UTL_GETUNIT(); OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED',IOSTAT=IOS) 
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open '//TRIM(FNAME)//CHAR(13)//'for writing','Error')
  RETURN
 ENDIF
 
 LINE='Name,X_Coordinate,Y_Coordinate,Z_Coordinate,MDepth,Inclination,Azimuth'
 IF(INT1+INT2.GT.0)LINE=TRIM(LINE)//',Intervals'
 WRITE(IU,'(A)') TRIM(LINE)
  
 NERROR=0; LEX=.TRUE.
 
 MAINLOOP: DO I=1,SIZE(HEADER)  

  !## see whether this is a vertical well
  IVERTICAL=0; IF(TRIM(UTL_CAP(HEADER(I)%CTYPE,'U')).EQ.'VERTICAL')IVERTICAL=1

  DO II=1,2

   !## get list of items for this well
   N=0
   
   !## add regular intervals from directional survey
   N1=0
   IF(IVERTICAL.EQ.0)THEN
    HEADER(I)%TDEPTH=0.0D0
    DO J=1,SIZE(DRC)
     IF(TRIM(DRC(J)%CID).EQ.TRIM(HEADER(I)%CID))THEN
      N=N+1; N1=N1+1; IF(II.EQ.2)THEN; IORDER(N1)=J; ZD1(N1)=DRC(J)%MD; ZDD(N)=DRC(J)%MD; HEADER(I)%TDEPTH=MAX(HEADER(I)%TDEPTH,ZDD(N)); ENDIF
     ENDIF
    ENDDO
    IF(N1.EQ.0)THEN
     IF(NERROR.EQ.0)THEN
      CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'The well called: [ '//TRIM(HEADER(I)%CID)//' ]'//CHAR(13)// &
      'does not have an entry in the directional survey file'//CHAR(13)// &
      'Do you want to skip this [YES] or quit the import [NO]'//CHAR(13),'Question')
      IF(WINFODIALOG(4).NE.1)THEN
       LEX=.FALSE.; EXIT MAINLOOP
      ELSE
       NERROR=NERROR+1; CYCLE MAINLOOP
      ENDIF
     ELSEIF(NERROR.EQ.1)THEN
      CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'The well called: [ '//TRIM(HEADER(I)%CID)//' ]'//CHAR(13)// &
      'does not have an entry in the directional survey file'//CHAR(13)// &
      'Do you want to skip this and all others that are missing [YES] or continue [NO]'//CHAR(13),'Question')
      IF(WINFODIALOG(4).NE.1)THEN
       CYCLE MAINLOOP
      ELSE
       NERROR=NERROR+1; CYCLE MAINLOOP
      ENDIF
     ELSE
      NERROR=NERROR+1; CYCLE MAINLOOP
     ENDIF
    ENDIF
   ELSE
    N =N +1; IF(II.EQ.2)ZDD(N)=0.0D0
    N =N +1; IF(II.EQ.2)ZDD(N)=HEADER(I)%TDEPTH
    N1=N1+1
   ENDIF
   
   !## get list of intervals for completion intervals
   IF(INT1.GT.0)THEN   
    N2=0
    DO J=1,SIZE(CINT1)
     !## positive numbers are intervals
     IF(TRIM(CINT1(J)%CID).EQ.TRIM(HEADER(I)%CID))THEN
      N=N+1; N2=N2+1; IF(II.EQ.2)THEN; JORDER(N2)=J; ZD2(N2)=CINT1(J)%UD; ZDD(N)=CINT1(J)%UD; ENDIF
      N=N+1;          IF(II.EQ.2)THEN;                                    ZDD(N)=CINT1(J)%LD; ENDIF
     ENDIF
    ENDDO
   ENDIF

   !## get list of intervals for cored intervals
   IF(INT2.GT.0)THEN   
    N3=0
    DO J=1,SIZE(CINT2)
     !## positive numbers are intervals
     IF(TRIM(CINT2(J)%CID).EQ.TRIM(HEADER(I)%CID))THEN
      N=N+1; N3=N3+1; IF(II.EQ.2)THEN; KORDER(N3)=J; ZD3(N3)=CINT2(J)%UD; ZDD(N)=CINT2(J)%UD; ENDIF
      N=N+1;          IF(II.EQ.2)THEN;                                    ZDD(N)=CINT2(J)%LD; ENDIF
     ENDIF
    ENDDO
   ENDIF

   IF(II.EQ.1)THEN
    ALLOCATE(ZDD(N),      ZD1(N1),IORDER(N1))
    IF(INT1.GT.0)ALLOCATE(ZD2(N2),JORDER(N2))
    IF(INT2.GT.0)ALLOCATE(ZD3(N3),KORDER(N3))
   ENDIF
  ENDDO
  
  !## get list sorted
  CALL QKSORT(N,ZDD)
  CALL QKSORT(N1,ZD1,V2=IORDER)
  IF(INT1.GT.0)CALL QKSORT(N2,ZD2,V2=JORDER)
  IF(INT2.GT.0)CALL QKSORT(N3,ZD3,V2=KORDER)

  !## depth more than are impossible
  DO II=1,N; ZDD(II)=MIN(ZDD(II),HEADER(I)%TDEPTH); ENDDO
  
  !## get rid of duplicates
  J=1; DO II=2,N
   IF(ZDD(II).NE.ZDD(II-1))THEN
    J=J+1
    IF(II.NE.J)ZDD(J)=ZDD(II)
   ENDIF
  ENDDO
  N=J

  !## write intervals - start with default values for intervals
  CTYPE=CHAR(39)//CHAR(39)
  DO JJ=1,N

   LINE=TRIM(HEADER(I)%CID)//','//TRIM(RTOS(HEADER(I)%X,'G',12))//','//TRIM(RTOS(HEADER(I)%Y,'G',12))//','//TRIM(RTOS(HEADER(I)%ELEV,'G',7))
         
   IF(JJ.LT.N)THEN
    Z1=ZDD(JJ)
    Z2=ZDD(JJ+1)
   ELSE
    Z1=ZDD(JJ-1)
    Z2=ZDD(JJ)
   ENDIF
   !## mid of current interval (directional_survey+cores+completetion intervals)
   Z =(Z1+Z2)/2.0D0     

   !## which deviation fits in here
   IF(IVERTICAL.EQ.0)THEN
    MINZ=HUGE(1.0D0); J=0
    DO K=1,N1-1
     K1=INT(IORDER(K  ),4)
     K2=INT(IORDER(K+1),4)
     Z1=DRC(K1)%MD; Z2=DRC(K2)%MD
     !## fits
     IF(Z.GE.Z1.AND.Z.LT.Z2)THEN
      DZ=ABS(Z1-Z)
      IF(DZ.LT.MINZ)THEN
       MINZ=DZ; J=IORDER(K)
      ENDIF
     ENDIF
    ENDDO
   ENDIF 
   
   !## write regular data
   LINE=TRIM(LINE)//','//TRIM(RTOS(ZDD(JJ),'G',7))//','//TRIM(RTOS(DRC(J)%INC,'G',7))//','//TRIM(RTOS(DRC(J)%AZIM,'G',7))
   
   CTYPE=CHAR(39)//'Unknown'//CHAR(39) 

   !## who fits in here most accurate
   MINZ=HUGE(1.0D0)

   IF(INT1.GT.0)THEN   
    DO K=1,N2 
     K1=INT(JORDER(K),4)
     Z1=CINT1(K1)%UD; Z2=CINT1(K1)%LD
     !## fits
     IF(Z.GE.Z1.AND.Z.LT.Z2)THEN
      DZ=ABS(Z1-Z)
      IF(DZ.LT.MINZ)THEN
       MINZ=DZ; CTYPE=CINT1(K1)%CTYPE
      ENDIF
     ENDIF
    ENDDO
   ENDIF

   IF(INT2.GT.0)THEN   
    DO K=1,N3 
     K1=INT(KORDER(K),4)
     Z1=CINT2(K1)%UD; Z2=CINT2(K1)%LD
     !## fits
     IF(Z.GE.Z1.AND.Z.LT.Z2)THEN
      DZ=ABS(Z1-Z)
      IF(DZ.LT.MINZ)THEN
       MINZ=DZ; CTYPE=CINT2(K1)%CTYPE  
      ENDIF
     ENDIF
    ENDDO
   ENDIF 

   IF(INT1+INT2.GT.0)LINE=TRIM(LINE)//','//TRIM(CTYPE)
    
   WRITE(IU,'(A)') TRIM(LINE)

  ENDDO
  DEALLOCATE(ZDD,ZD1,IORDER)
  IF(INT1.GT.0)DEALLOCATE(ZD2,JORDER)
  IF(INT2.GT.0)DEALLOCATE(ZD3,KORDER)
 ENDDO MAINLOOP

 CLOSE(IU)
 
 IF(NERROR.GT.0)THEN
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'iMOD found '//TRIM(ITOS(NERROR))//' missing entries in the directional survey','Information')
 ENDIF
 
 DEVWELL_IMPORT_MULTIPLE_WRITE=LEX

 END FUNCTION DEVWELL_IMPORT_MULTIPLE_WRITE
 
 !###======================================================================
 LOGICAL FUNCTION DEVWELL_IMPORT_MULTIPLE_FILE1(FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU,IOS,N,I
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=256) :: LINE
 
 DEVWELL_IMPORT_MULTIPLE_FILE1=.FALSE.
 
 !## read well headers
 IU=UTL_GETUNIT(); OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',FORM='FORMATTED',IOSTAT=IOS) 
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open '//TRIM(FNAME)//CHAR(13)//'for reading','Error')
  RETURN
 ENDIF

 DO I=1,2
  IF(I.EQ.2)REWIND(IU)
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  N=0
  DO
   N=N+1
   READ(IU,'(A256)',IOSTAT=IOS) LINE
   IF(IOS.NE.0)EXIT
   IF(I.EQ.2)THEN
    CALL UTL_CLEANLINE(LINE)
    READ(LINE,*,IOSTAT=IOS) HEADER(N)%CID,HEADER(N)%LIC,HEADER(N)%ELEV,HEADER(N)%TDEPTH,HEADER(N)%CTYPE,HEADER(N)%X,HEADER(N)%Y
    HEADER(N)%CID=UTL_CAP(HEADER(N)%CID,'U')
   ENDIF
  ENDDO
  N=N-1
  IF(I.EQ.1)ALLOCATE(HEADER(N))
 ENDDO
 
 CLOSE(IU)
 
 DEVWELL_IMPORT_MULTIPLE_FILE1=.TRUE.

 END FUNCTION  DEVWELL_IMPORT_MULTIPLE_FILE1

 !###======================================================================
 LOGICAL FUNCTION DEVWELL_IMPORT_MULTIPLE_FILE2(FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU,IOS,N,I
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=256) :: LINE
 
 DEVWELL_IMPORT_MULTIPLE_FILE2=.FALSE.
 
 !## read well headers
 IU=UTL_GETUNIT(); OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',FORM='FORMATTED',IOSTAT=IOS) 
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open '//TRIM(FNAME)//CHAR(13)//'for reading','Error')
  RETURN
 ENDIF

 DO I=1,2
  IF(I.EQ.2)REWIND(IU)
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  N=0
  DO
   N=N+1
   READ(IU,'(A256)',IOSTAT=IOS) LINE
   IF(IOS.NE.0)EXIT
   IF(I.EQ.2)THEN
    CALL UTL_CLEANLINE(LINE)
    DRC(N)%MD=0.0D0; DRC(N)%INC=0.0D0; DRC(N)%AZIM=0.0D0
    READ(LINE,*,IOSTAT=IOS) DRC(N)%CID,DRC(N)%MD,DRC(N)%INC,DRC(N)%AZIM
    DRC(N)%CID=UTL_CAP(DRC(N)%CID,'U')
   ENDIF
  ENDDO
  N=N-1
  IF(I.EQ.1)ALLOCATE(DRC(N))
 ENDDO
 
 CLOSE(IU)
  
 DEVWELL_IMPORT_MULTIPLE_FILE2=.TRUE.

 END FUNCTION  DEVWELL_IMPORT_MULTIPLE_FILE2
 
 !###======================================================================
 LOGICAL FUNCTION DEVWELL_IMPORT_MULTIPLE_FILE3(FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU,IOS,N,I
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=256) :: LINE
 
 DEVWELL_IMPORT_MULTIPLE_FILE3=.TRUE.; IF(LEN_TRIM(FNAME).EQ.0)RETURN
 
 DEVWELL_IMPORT_MULTIPLE_FILE3=.FALSE.
 
 !## read well headers
 IU=UTL_GETUNIT(); OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',FORM='FORMATTED',IOSTAT=IOS) 
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open '//TRIM(FNAME)//CHAR(13)//'for reading','Error')
  RETURN
 ENDIF

 DO I=1,2
  IF(I.EQ.2)REWIND(IU)
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  N=0
  DO
   N=N+1
   READ(IU,'(A256)',IOSTAT=IOS) LINE
   IF(IOS.NE.0)EXIT
   IF(I.EQ.2)THEN
    CALL UTL_CLEANLINE(LINE)
    CINT1(N)%UD=0.0D0; CINT1(N)%LD=0.0D0
    READ(LINE,*,IOSTAT=IOS) CINT1(N)%CID,CINT1(N)%CDATE,CINT1(N)%CTYPE,CINT1(N)%UD,CINT1(N)%LD
    CINT1(N)%CID=UTL_CAP(CINT1(N)%CID,'U')
   ENDIF
  ENDDO
  N=N-1
  IF(I.EQ.1)ALLOCATE(CINT1(N))
 ENDDO
 
 CLOSE(IU)
 
 DEVWELL_IMPORT_MULTIPLE_FILE3=.TRUE.

 END FUNCTION  DEVWELL_IMPORT_MULTIPLE_FILE3
 
 !###======================================================================
 LOGICAL FUNCTION DEVWELL_IMPORT_MULTIPLE_FILE4(FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU,IOS,N,I
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=256) :: LINE
 
 DEVWELL_IMPORT_MULTIPLE_FILE4=.TRUE.; IF(LEN_TRIM(FNAME).EQ.0)RETURN
 
 DEVWELL_IMPORT_MULTIPLE_FILE4=.FALSE.
 
 !## read well headers
 IU=UTL_GETUNIT(); OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',FORM='FORMATTED',IOSTAT=IOS) 
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open '//TRIM(FNAME)//CHAR(13)//'for reading','Error')
  RETURN
 ENDIF

 DO I=1,2
  IF(I.EQ.2)REWIND(IU)
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  N=0
  DO
   N=N+1
   READ(IU,'(A256)',IOSTAT=IOS) LINE
   IF(IOS.NE.0)EXIT
   IF(I.EQ.2)THEN
    CALL UTL_CLEANLINE(LINE)
    CINT2(N)%UD=0.0D0; CINT2(N)%LD=0.0D0
    READ(LINE,*,IOSTAT=IOS) CINT2(N)%CID,CINT2(N)%CDATE,CINT2(N)%UD,CINT2(N)%LD
    CINT2(N)%CID=UTL_CAP(CINT2(N)%CID,'U')
    CINT2(N)%CTYPE='CORED'
   ENDIF
  ENDDO
  N=N-1
  IF(I.EQ.1)ALLOCATE(CINT2(N))
 ENDDO
 
 CLOSE(IU)
 
 DEVWELL_IMPORT_MULTIPLE_FILE4=.TRUE.

 END FUNCTION  DEVWELL_IMPORT_MULTIPLE_FILE4

 !###======================================================================
 SUBROUTINE DEVWELL_IMPORT_MULTIPLE_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 
 IF(ALLOCATED(HEADER))DEALLOCATE(HEADER)
 IF(ALLOCATED(DRC))DEALLOCATE(DRC)
 IF(ALLOCATED(CINT1))DEALLOCATE(CINT1)
 IF(ALLOCATED(CINT2))DEALLOCATE(CINT2)
 IF(ALLOCATED(ZDD))DEALLOCATE(ZDD)
 
 END SUBROUTINE DEVWELL_IMPORT_MULTIPLE_DEALLOCATE
 
 !###======================================================================
 SUBROUTINE DEVWELL_IMPORT(CSVFNAME,IPFFNAME,ICOLS,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: G2R=360.0D0/(2.0*PI)
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),INTENT(IN) :: CSVFNAME,IPFFNAME
 INTEGER,DIMENSION(:),POINTER,INTENT(INOUT) :: ICOLS
 CHARACTER(LEN=256) :: DIR,LINE,FNAME
 CHARACTER(LEN=6) :: CT
 REAL(KIND=DP_KIND) :: X,Y,AX,AY,AZ,X1,Y1,Z1,L,DX,DY,DZ,TL1,TL2
 INTEGER :: I,II,J,I1,I2,N,IU,JU,NLC,IOS
 CHARACTER(LEN=MAXLEN) :: CL1,CL2,CL

 IF(IBATCH.EQ.0)THEN     
  IF(.NOT.UTL_DATA_CSV((/'Name        ','X Coordinate','Y Coordinate', &
                         'Z Coordinate','Depth       ','Inclination ', &
                         'Azimuth     ','Add. Label 1','Add. Label 2', &
                         'Add. Label 3','Add. Label 4'/),VAR,ICOL_VAR,IACT_VAR,CCNST))RETURN

  IF(.NOT.UTL_WSELECTFILE('Save IPF File (*.ipf)|*.ipf|',&
                   SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                   'Save IPF File (*.ipf)'))THEN
   CALL UTL_GENLABELSDEALLOCATE(); RETURN
  ENDIF
  
  J=0; DO I=1,SIZE(IACT_VAR); IF(IACT_VAR(I).EQ.1)J=J+1; ENDDO
  ALLOCATE(ICOLS(J))
  J=0; DO I=1,SIZE(IACT_VAR)
   IF(IACT_VAR(I).EQ.1)THEN; J=J+1; ICOLS(J)=ICOL_VAR(I); ENDIF
  ENDDO
  NLC=0; DO I=8,SIZE(IACT_VAR); IF(IACT_VAR(I).EQ.1)NLC=NLC+1; ENDDO
 
 ELSE
 
  NLC=SIZE(ICOLS)-7
 
  FNAME=IPFFNAME
  
  WRITE(*,'(A)') 'Reading '//TRIM(CSVFNAME)//' ...'
  CALL UTL_GENLABELSREAD(CSVFNAME,VAR,NL,NV)

  WRITE(*,'(/A)') 'Read info:'
  WRITE(*,'(A,I10)') 'Number of records',NL
  WRITE(*,'(A,I10)') 'Number of columns',NV
 
  DO J=1,SIZE(ICOLS)
   CT=''
   DO I=1,NV
    IF(ICOLS(J).EQ.I)THEN
     SELECT CASE (J)
      CASE (1);  CT='NAME'; EXIT
      CASE (2);  CT='XCRD'; EXIT
      CASE (3);  CT='YCRD'; EXIT
      CASE (4);  CT='ZCRD'; EXIT
      CASE (5);  CT='DEPT'; EXIT
      CASE (6);  CT='INCL'; EXIT
      CASE (7);  CT='AZIM'; EXIT
      CASE (8:); CT='LABL'//TRIM(ITOS(J-7)); EXIT
     END SELECT
    ENDIF
   ENDDO
   IF(LEN_TRIM(CT).NE.0)THEN
    WRITE(*,'(A6,A9,2X,A)') CT,' COLUMN'//TRIM(ITOS(I)),VAR(I,0)
   ENDIF  
  ENDDO

 ENDIF

 DIR=FNAME(:INDEX(FNAME,'\',.TRUE.)-1)
 CALL UTL_CREATEDIR(DIR)

 !## process data 
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(FNAME)//'_',STATUS='UNKNOWN',ACTION='WRITE')
 WRITE(IU,'(A)') 'NaN1#'
 WRITE(IU,'(A)') '5'
 WRITE(IU,'(A)') 'XCRD'
 WRITE(IU,'(A)') 'YCRD'
 WRITE(IU,'(A)') 'ZCRD'
 WRITE(IU,'(A)') 'NAME'
 WRITE(IU,'(A)') 'ID'
 WRITE(IU,'(A)') '5,TXT'
 
 N=0; I1=0; DO
  I1=I1+1
  
  CL1=UTL_CAP(VAR(ICOLS(1),I1),'U')  
  !## determine entire borehole
  I2=I1; DO
   CL2=UTL_CAP(VAR(ICOLS(1),I2),'U')  
   IF(TRIM(CL1).NE.TRIM(CL2))EXIT
   I2=I2+1; IF(I2.GT.NL)EXIT
  ENDDO
  I2=I2-1
  
  !## clean cl for forbidden characters
  DO; II=INDEX(TRIM(CL1),'/'); IF(II.EQ.0)EXIT; CL1(II:II)='_'; ENDDO
  DO; II=INDEX(TRIM(CL1),':'); IF(II.EQ.0)EXIT; CL1(II:II)='_'; ENDDO
  DO; II=INDEX(TRIM(CL1),' '); IF(II.EQ.0)EXIT; CL1(II:II)='_'; ENDDO
    
  JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(DIR)//'\'//TRIM(CL1)//'.TXT',STATUS='UNKNOWN',ACTION='WRITE')
  WRITE(JU,'(I4.4)') I2-I1+1
  WRITE(JU,'(I3,A)') 3+NLC,',4'
  WRITE(JU,'(A)') 'DX,-999.99'
  WRITE(JU,'(A)') 'DY,-999.99'
  WRITE(JU,'(A)') 'Z ,-999.99'
  DO I=1,NLC
   WRITE(JU,'(A,F7.2)') TRIM(VAR(ICOLS(7+I),0))//',',-999.99
  ENDDO
  !## process current borehole
  X1=0.0D0; Y1=0.0D0; TL1=0.0D0
  IF(ICOLS(4).LE.0)THEN
   Z1=0.0D0
  ELSE
   !## read z value
   READ(VAR(ICOLS(4),I1),*,IOSTAT=IOS) Z1
   IF(IOS.NE.0)THEN; WRITE(*,'(/1X,A/)') 'Error reading elevation from '//TRIM(VAR(ICOLS(4),I1)); STOP; ENDIF
  ENDIF
  DO II=I1,I2

   !## read total length through well
   READ(VAR(ICOLS(5),II),*) TL2

   !## no rotation for x-axes
   AX=0.0D0
   !## read inclination - rotation for y-axes
   READ(VAR(ICOLS(6),II),*) AY
   !## read azimuth - rotation for z-axes
   READ(VAR(ICOLS(7),II),*) AZ
   
   !## convert to radians
   AX=AX/G2R; AY=AY/G2R; AZ=AZ/G2R
   AY=AY-0.5D0*PI

   !## correction to be sure north is 0.0D0 and east is 90 degrees   
   AZ=AZ-0.5D0*PI

   !## length
   L=TL2-TL1
   
   !## get point in depth
   X =L*COS(AY)
   Y =0.0D0
   DZ=L*SIN(AY)

   !## rotate point for azimuth 
   DX= COS(AZ)*X+SIN(AZ)*Y
   DY=-SIN(AZ)*X+COS(AZ)*Y

   X1=X1+DX
   Y1=Y1+DY
   Z1=Z1+DZ
   
   !## add labels
   LINE=TRIM(RTOS(X1,'F',2))//','//TRIM(RTOS(Y1,'F',2))//','//TRIM(RTOS(Z1,'F',2))
   DO I=1,NLC
    !## add dummy label if missing
    IF(ICOLS(7+I).LE.0)THEN
     CL='S'
    ELSE
     READ(VAR(ICOLS(7+I),II),*) CL
    ENDIF
    LINE=TRIM(LINE)//',"'//TRIM(CL)//'"'
   ENDDO
   WRITE(JU,'(A)') TRIM(LINE)
   
   TL1=TL2
   
  ENDDO        
  CLOSE(JU)

  !## increase number of wells
  N=N+1

  !## write information to ipf: x,y,name,id
  IF(ICOLS(4).LE.0)THEN
   WRITE(IU,'(A)') TRIM(VAR(ICOLS(2),I1))//',' // &
                   TRIM(VAR(ICOLS(3),I1))//',' // &
                   TRIM(RTOS(0.0D0,'F',1))//',"'// &
                   TRIM(VAR(ICOLS(1),I1))//'",'//TRIM(CL1)
  ELSE
   WRITE(IU,'(A)') TRIM(VAR(ICOLS(2),I1))//',' // &
                   TRIM(VAR(ICOLS(3),I1))//',' // &
                   TRIM(VAR(ICOLS(4),I1))//',"'// &
                   TRIM(VAR(ICOLS(1),I1))//'",'//TRIM(CL1)
  ENDIF
  
  !## continue with rest
  I1=I2
  !## stop - finished
  IF(I1.EQ.NL)EXIT  
 ENDDO

 CLOSE(IU)
 
 CALL UTL_MF2005_MAXNO(TRIM(FNAME)//'_',(/N/))
  
 CALL UTL_GENLABELSDEALLOCATE(); DEALLOCATE(ICOLS)
 
 !## load ipf into imod manager
 IF(IBATCH.EQ.0)CALL MANAGER_UTL_ADDFILE(FNAME)
 CALL IDFPLOTFAST(1) 
 
 END SUBROUTINE DEVWELL_IMPORT
 
END MODULE MOD_DEVWEL