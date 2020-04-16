!!  Copyright (C) Stichting Deltares, 2005-2020.
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
MODULE MOD_MATH

USE WINTERACTER
USE RESOURCE
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MODPLOT
USE MOD_IDFPLOT
USE MOD_MANAGER_UTL
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFDEALLOCATE
USE MOD_UTL, ONLY : RTOS,ITOS,UTL_GETUNIT,UTL_CHECKNAME,UTL_WSELECTFILE,UTL_CAP,UTL_GETHELP
USE MOD_MATH_CALC, ONLY : MATH1PLOTFUNC,MATH1CALC,MATH1CALCCLOSE
USE MOD_MATH_SCALE, ONLY : MATH1GETISCALE,MATH1SCALE,MATH1GETSCALETXT,MATH1SCALECLOSE
USE MOD_MATH_MERGE, ONLY : MATH1MERGE,MATH1MERGECLOSE
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_QKSORT

CONTAINS

!###======================================================================
SUBROUTINE MATH1MAIN()
!###======================================================================
IMPLICIT NONE
TYPE(WIN_MESSAGE) :: MESSAGE
INTEGER :: ITYPE,I,J,ITAB
CHARACTER(LEN=256) :: IDFNAME
LOGICAL :: LEX
CHARACTER(LEN=50) :: FUNC

CALL WDIALOGLOAD(ID_DMATH,ID_DMATH); CALL UTL_DIALOGSHOW(-1,-1,0,3)

CALL WDIALOGFIELDSTATE(IDF_STRING1,0)
CALL WDIALOGPUTSTRING(IDF_STRING1,TRIM(RTOS(MPW%XMIN,'F',2))//','//TRIM(RTOS(MPW%YMIN,'F',2))//','// &
                                  TRIM(RTOS(MPW%XMAX,'F',2))//','//TRIM(RTOS(MPW%YMAX,'F',2)))
CALL WDIALOGPUTSTRING(IDF_GROUP1,'Select the extent for which the computation applies:')

CALL WDIALOGSELECT(ID_DMATHTAB1)
CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONOPENIDF,1)
CALL WDIALOGPUTIMAGE(ID_OPEN2,ID_ICONOPENIDF,1)
CALL WDIALOGPUTIMAGE(ID_OPEN3,ID_ICONOPENIDF,1)
CALL WDIALOGPUTIMAGE(ID_OPEN4,ID_ICONOPENIDF,1)
CALL WDIALOGFIELDOPTIONS(ID_FUNC,EDITFIELDCHANGED,ENABLED)

CALL WDIALOGSELECT(ID_DMATHTAB2)
CALL WDIALOGPUTMENU(IDF_MENU1,(/'Boundary       ','Arithmetic Mean','Geometric Mean ','Sum            ', &
                                'Sum Conductance','Inverse        ','Most.Freq.Occur','Sum Inverse    ', &
                                'Percentile     ','Block Value    '/),10,2) !,'Darcian Simul. ','Periodic Bound.', &
                                !'Local-Global   '/),11,2)

 ! 1 = SPECIAAL (IBOUNDARY)
 ! 2 = REKENKUNDIG (SHEAD/VCONT/S)
 ! 3 = GEOMETRISCH (KD)
 ! 4 = SUM(Q)
 ! 5 = SUM(COND)*RATIO (RIV/DRN/GHB CONDUCTANCE; RCH MM/DAY)
 ! 6 = INVERSE (C)
 ! 7 = MOST FREQUENT OCCURENCE
 ! 8 = SUM (1/c)*RATIO
 ! 9 = PERCENTILE 
 !10 = BLOCKVALUE
 !16 = rekenkundig gemiddelde excl. nodata
                                
CALL WDIALOGPUTMENU(IDF_MENU2,(/'Arithm. average','Block Value    '/),2,1)
CALL WDIALOGFIELDOPTIONS(IDF_REAL2,EDITFIELDCHANGED,1)

!## get empty iplot-location
DO I=1,MXMPLOT
 IF(MP(I)%IACT.AND.MP(I)%ISEL.AND.MP(I)%IPLOT.EQ.1)THEN
  CALL WDIALOGSELECT(ID_DMATHTAB1)
  CALL WDIALOGPUTSTRING(ID_FILE1,MP(I)%IDFNAME)
  CALL WDIALOGPUTSTRING(ID_FUNC,'C=A')
  CALL WDIALOGSELECT(ID_DMATHTAB2)
  CALL WDIALOGPUTSTRING(ID_FILE1,MP(I)%IDFNAME)
  CALL MATH1FIELDS2GETSCALE(MP(I)%IDFNAME)
  EXIT
 ENDIF
END DO
DO J=I+1,MXMPLOT
 IF(MP(J)%IACT.AND.MP(J)%ISEL.AND.MP(J)%IPLOT.EQ.1)THEN
  CALL WDIALOGSELECT(ID_DMATHTAB1)
  CALL WDIALOGPUTSTRING(ID_FILE2,MP(J)%IDFNAME)
  CALL WDIALOGPUTSTRING(ID_FUNC,'C=A-B')
  EXIT
 ENDIF
END DO

CALL WDIALOGSELECT(ID_DMATHTAB1)
CALL WDIALOGPUTSTRING(IDF_LABEL5,'Examples: C=abs(A-B); C=sgn(A-B); C=log(4*A); C=5*A-0.5+B; C=A<B')
CALL WDIALOGPUTSTRING(IDF_LABEL4,'Operators:'//CHAR(9)//'+ - / * ? < >'//CHAR(13)// &
                                 'Functions:'//CHAR(9)//'abs(),log(),exp(),gtp(),sgn()')
CALL WDIALOGPUTSTRING(ID_FILE3,TRIM(PREFVAL(1))//'\TMP\DIFF.IDF')

CALL WDIALOGSELECT(ID_DMATHTAB2)
CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONOPENIDF,1)
CALL WDIALOGSELECT(ID_DMATHTAB1)
CALL WDIALOGGETSTRING(ID_FUNC,FUNC)
CALL MATH1PLOTFUNC(FUNC)

CALL WDIALOGSELECT(ID_DMATHTAB3)
CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONOPENIDF,1)
CALL WDIALOGPUTMENU(IDF_MENU1,MP%ALIAS,MPW%NACT,ACTLIST)

CALL WDIALOGSELECT(ID_DMATHTAB1)

DO

 CALL WMESSAGE(ITYPE,MESSAGE)

 SELECT CASE (MESSAGE%WIN)
  CASE (ID_DMATH,ID_DMATHTAB1,ID_DMATHTAB2,ID_DMATHTAB3)
   CALL WDIALOGSELECT(MESSAGE%WIN)
 END SELECT

 SELECT CASE(ITYPE)

  CASE (TABCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
    CASE (ID_DMATHTAB1)
     CALL WDIALOGSELECT(ID_DMATH)
     CALL WDIALOGFIELDSTATE(IDOK,1)
     CALL WDIALOGSELECT(ID_DMATHTAB1)  
     CALL WDIALOGGETSTRING(ID_FUNC,FUNC)
     CALL MATH1PLOTFUNC(FUNC)
    CASE (ID_DMATHTAB2)
     CALL MATH1FIELDS_SCALE1()
     CALL WDIALOGSELECT(ID_DMATH)
     CALL WDIALOGPUTSTRING(IDF_RADIO2,'Map A')
    CASE (ID_DMATHTAB3)
     CALL WDIALOGPUTSTRING(IDF_RADIO2,'Selected Maps')
     CALL MATH1FIELDS_MERGE1()
     CALL MATH1FIELDS_MERGE2()
   END SELECT

  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%WIN)
    !## main
    CASE (ID_DMATH)
     SELECT CASE (MESSAGE%VALUE2)
      CASE (IDF_RADIO1,IDF_RADIO2,IDF_RADIO3)
       CALL WDIALOGSELECT(ID_DMATH)
       CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
!       CALL WDIALOGCOLOUR(IDF_RADIO1,-1,WRGB(255,0,0))
       J=0; IF(I.EQ.3)J=1; CALL WDIALOGFIELDSTATE(IDF_STRING1,J)
     END SELECT
    !## calc
    CASE (ID_DMATHTAB1)
     SELECT CASE (MESSAGE%VALUE2)
      CASE(ID_FUNC)
       CALL WDIALOGGETSTRING(ID_FUNC,FUNC)
       CALL MATH1PLOTFUNC(FUNC)
      CASE(IDF_CHECK3)
       CALL WDIALOGGETCHECKBOX(IDF_CHECK3,I)
       CALL WDIALOGFIELDSTATE(IDF_LABEL7,I)
       CALL WDIALOGFIELDSTATE(ID_FILE4,I)
       CALL WDIALOGFIELDSTATE(ID_OPEN3,I)
      CASE(IDF_RADIO1,IDF_RADIO2)
       CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
       CALL WDIALOGFIELDSTATE(IDF_REAL1,ABS(I-1))
     END SELECT
    !## scale
    CASE (ID_DMATHTAB2)
     SELECT CASE (MESSAGE%VALUE2)
      !## change type of scaling
      CASE (IDF_REAL2)
       CALL MATH1FIELDS_SCALE2()
      !## change scale
      CASE (IDF_MENU2,IDF_MENU1)
       CALL MATH1FIELDS_SCALE1()
     END SELECT
    !## merge
    CASE (ID_DMATHTAB3)
     SELECT CASE (MESSAGE%VALUE2)
      CASE (IDF_MENU1,IDF_MENU2)
       CALL MATH1FIELDS_MERGE1()
       CALL MATH1FIELDS_MERGE2()
      CASE (IDF_CHECK1)
       CALL MATH1FIELDS_MERGE2()
     END SELECT
   END SELECT

  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)

    CASE (ID_OPEN1,ID_OPEN2)
     IDFNAME=''
     IF(UTL_WSELECTFILE('iMOD IDF-File (*.idf)|*.idf|',LOADDIALOG+PROMPTON+DIRCHANGE+MUSTEXIST, &
                   IDFNAME,'Select IDF File (*.idf)'))THEN
      IF(MESSAGE%VALUE1.EQ.ID_OPEN1)CALL WDIALOGPUTSTRING(ID_FILE1,IDFNAME)
      IF(MESSAGE%WIN.EQ.ID_DMATHTAB2)THEN
       CALL MATH1FIELDS2GETSCALE(IDFNAME)
       CALL MATH1FIELDS_SCALE2()
       CALL MATH1FIELDS_SCALE1()
      ELSEIF(MESSAGE%WIN.EQ.ID_DMATHTAB3)THEN

      ENDIF
      IF(MESSAGE%VALUE1.EQ.ID_OPEN2)CALL WDIALOGPUTSTRING(ID_FILE2,IDFNAME)
     ENDIF
    CASE (ID_OPEN3)
     IDFNAME=''
     IF(UTL_WSELECTFILE('iMOD GEN-File (*.gen)|*.gen|',LOADDIALOG+PROMPTON+DIRCHANGE+MUSTEXIST, &
                   IDFNAME,'Select GEN File (*.gen)'))CALL WDIALOGPUTSTRING(ID_FILE4,IDFNAME)
    CASE (ID_OPEN4)
     IDFNAME=''
     IF(UTL_WSELECTFILE('iMOD IDF-File (*.idf)|*.idf|',SAVEDIALOG+PROMPTON+DIRCHANGE, &
                   IDFNAME,'Select IDF File (*.idf)'))CALL WDIALOGPUTSTRING(ID_FILE3,IDFNAME)

    CASE (IDOK)
     CALL WDIALOGSELECT(ID_DMATH)
     CALL WDIALOGHIDE()
     CALL WDIALOGGETTAB(ID_MATHTAB,ITAB)
     IF(ITAB.EQ.ID_DMATHTAB1)CALL MATH1COMPUTEMAIN(LEX)
     IF(ITAB.EQ.ID_DMATHTAB2)CALL MATH1SCALEMAIN(LEX)
     IF(ITAB.EQ.ID_DMATHTAB3)THEN
      CALL WDIALOGSELECT(ID_DMATHTAB3)
      CALL WDIALOGGETMENU(IDF_MENU2,I)
      IF(I.EQ.1)CALL MATH1MERGEMAIN(LEX)
      IF(I.GE.2)CALL MATH1SUMMAIN(LEX,I)
     ENDIF
     IF(LEX)EXIT
     CALL WDIALOGSELECT(ID_DMATH)
     CALL UTL_DIALOGSHOW(-1,-1,0,3)

    CASE (IDCANCEL)
     EXIT
    CASE (IDHELP) 
     CALL UTL_GETHELP('4.1.3','MMO.IDO.IDFCalc')

   END SELECT
 END SELECT
ENDDO

CALL WDIALOGSELECT(ID_DMATH)
CALL WDIALOGUNLOAD()

END SUBROUTINE MATH1MAIN

!###======================================================================
SUBROUTINE MATH1FIELDS2GETSCALE(FNAME)
!###======================================================================
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: FNAME
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
INTEGER :: I

ALLOCATE(IDF(1))

IF(.NOT.IDFREAD(IDF(1),FNAME,0))RETURN
CLOSE(IDF(1)%IU)

CALL WDIALOGSELECT(ID_DMATHTAB2)
CALL WDIALOGPUTDOUBLE(IDF_REAL2,IDF(1)%DX)

I=INDEXNOCASE(FNAME,'.',.TRUE.)-1
CALL WDIALOGPUTSTRING(ID_FILE3,FNAME(:I)//'_SCALED.IDF')

IF(ALLOCATED(IDF))THEN
 CALL IDFDEALLOCATE(IDF,SIZE(IDF))
 DEALLOCATE(IDF)
ENDIF

END SUBROUTINE MATH1FIELDS2GETSCALE

!###======================================================================
SUBROUTINE MATH1FIELDS_SCALE1()
!###======================================================================
IMPLICIT NONE
CHARACTER(LEN=20) :: STRING
CHARACTER(LEN=256) :: TXT
INTEGER :: I,J !,K
REAL(KIND=DP_KIND) :: X

CALL WDIALOGSELECT(ID_DMATHTAB2)

!## upscaling
CALL WDIALOGGETMENU(IDF_MENU1,I)
J=1 !; K=3
SELECT CASE (I)
 CASE (9)
  STRING='percentile (0-1)'
  X     =0.5
 CASE DEFAULT
  STRING=''
  J     =3
!  CASE (2,3,4,5,6)
!   STRING='weigh'
!   X     =1.0D0
END SELECT

TXT=MATH1GETSCALETXT(1,I)
CALL WDIALOGPUTSTRING(IDF_LABEL12,TRIM(TXT))
CALL WDIALOGPUTSTRING(IDF_LABEL7,STRING)
CALL WDIALOGFIELDSTATE(IDF_LABEL7,J)
CALL WDIALOGFIELDSTATE(IDF_REAL1,J)
IF(J.EQ.1)CALL WDIALOGPUTDOUBLE(IDF_REAL1,X)

!CALL WDIALOGFIELDSTATE(IDF_LABEL10,K)
!CALL WDIALOGFIELDSTATE(IDF_MENU3,K)
!IF(K.EQ.1)CALL WDIALOGPUTOPTION(IDF_MENU3,1)

!## downscaling
CALL WDIALOGGETMENU(IDF_MENU2,I)
TXT=MATH1GETSCALETXT(-1,I)
CALL WDIALOGPUTSTRING(IDF_LABEL15,TRIM(TXT))

END SUBROUTINE MATH1FIELDS_SCALE1

!###======================================================================
SUBROUTINE MATH1FIELDS_SCALE2()
!###======================================================================
IMPLICIT NONE
INTEGER :: I
REAL(KIND=DP_KIND) :: X

CALL WDIALOGSELECT(ID_DMATHTAB2)
CALL WDIALOGGETDOUBLE(IDF_REAL2,X)
I=1; IF(X.LE.0.0D0)I=0

CALL WDIALOGSELECT(ID_DMATH); CALL WDIALOGFIELDSTATE(IDOK,I)

END SUBROUTINE MATH1FIELDS_SCALE2

!###======================================================================
SUBROUTINE MATH1FIELDS_MERGE1()
!###======================================================================
IMPLICIT NONE
INTEGER :: I,J,K
CHARACTER(LEN=10),DIMENSION(5) :: TXT
DATA TXT/'merged','sum','mean','max','min'/

J=1
CALL WDIALOGSELECT(ID_DMATHTAB3); CALL WDIALOGGETMENU(IDF_MENU1,ACTLIST)
CALL WDIALOGGETMENU(IDF_MENU2,K)

DO I=1,MXMPLOT
 IF(ACTLIST(I).EQ.1.AND.MP(I)%IPLOT.EQ.1)THEN
  J=INDEX(MP(I)%IDFNAME,'.',.TRUE.)-1
  CALL WDIALOGPUTSTRING(ID_FILE2,MP(I)%IDFNAME(:J)//'_'//TRIM(TXT(K))//'.IDF')
  EXIT
 ENDIF
ENDDO

IF(SUM(ACTLIST).EQ.0)J=0
DO I=1,MXMPLOT
 IF(ACTLIST(I).EQ.1.AND.MP(I)%IPLOT.NE.1)J=0
END DO
CALL WDIALOGSELECT(ID_DMATH)
CALL WDIALOGFIELDSTATE(IDOK,J)

I=1; IF(K.NE.1)I=0
CALL WDIALOGSELECT(ID_DMATHTAB3)
CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,0)
CALL WDIALOGFIELDSTATE(IDF_CHECK1,I)

!I=1; IF(K.EQ.1)I=0
!CALL WDIALOGSELECT(ID_DMATH)
!CALL WDIALOGFIELDSTATE(IDF_RADIO1,I); CALL WDIALOGFIELDSTATE(IDF_RADIO2,I)

END SUBROUTINE MATH1FIELDS_MERGE1

!###======================================================================
SUBROUTINE MATH1FIELDS_MERGE2()
!###======================================================================
IMPLICIT NONE
INTEGER :: I,J
CHARACTER(LEN=1) :: IDFNAME

J=1

CALL WDIALOGSELECT(ID_DMATHTAB3)

CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
CALL WDIALOGFIELDSTATE(ID_FILE1,I)
CALL WDIALOGFIELDSTATE(ID_OPEN1,I)
CALL WDIALOGFIELDSTATE(IDF_LABEL4,I)

!## get idfname for mask
IF(I.EQ.1)THEN
 CALL WDIALOGGETSTRING(ID_FILE1,IDFNAME)
 IF(LEN_TRIM(IDFNAME).EQ.0)J=0
ENDIF

CALL WDIALOGSELECT(ID_DMATHTAB3)
CALL WDIALOGGETSTRING(ID_FILE2,IDFNAME)
IF(LEN_TRIM(IDFNAME).EQ.0)J=0

CALL WDIALOGSELECT(ID_DMATH)
CALL WDIALOGFIELDSTATE(IDOK,J)

END SUBROUTINE MATH1FIELDS_MERGE2

!###======================================================================
SUBROUTINE MATH1COMPUTEMAIN(LEX)
!###======================================================================
USE MOD_MATH_PAR
IMPLICIT NONE
LOGICAL,INTENT(OUT) :: LEX
INTEGER,DIMENSION(3) :: IG
INTEGER :: IOS,IP
CHARACTER(LEN=256) :: LEGNAME
CHARACTER(LEN=80) :: PARSEDFUNC
REAL(KIND=DP_KIND) :: X1,X2,Y1,Y2

CALL WDIALOGSELECT(ID_DMATHTAB1)

!## get function
CALL WDIALOGGETSTRING(ID_FUNC,FUNC)
FUNC=UTL_CAP(FUNC,'U')

!## function should be equal to parsed result
CALL WDIALOGGETSTRING(IDF_LABEL5,PARSEDFUNC)
IP = INDEX(PARSEDFUNC,'C=')
IF(IP.GT.0)THEN
  PARSEDFUNC = PARSEDFUNC(IP:)
  IF(PARSEDFUNC .NE. FUNC) THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Parsed expression does not match input formula: '&
       &//NEWLINE//NEWLINE//ACHAR(9)//TRIM(PARSEDFUNC)//' is not equal to '//TRIM(FUNC)//NEWLINE//NEWLINE//&
       &'Did you use the correct order of operands: constants before maps, e.g. 0.5*A instead of A/2?)','Error')
     LEX=.FALSE.
     RETURN
  ENDIF 
ELSE
  !## for safety only: compute button shouldn't be active when the formula isn't parsed
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Invalid expression ('//PARSEDFUNC//')','Error')
  LEX=.FALSE.
  RETURN
ENDIF

IG(1)=INDEX(FUNC,'A')
IG(2)=INDEX(FUNC,'B')
IG(3)=INDEX(FUNC,'C')

X1=MPW%XMIN; X2=MPW%XMAX; Y1=MPW%YMIN; Y2=MPW%YMAX

CALL WDIALOGSELECT(ID_DMATH); CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IIEXT)

!## extent
IF(IIEXT.EQ.3)THEN
 CALL WDIALOGGETSTRING(IDF_STRING1,LEGNAME)
 READ(LEGNAME,*,IOSTAT=IOS) MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot read window correctly','Error')
  MPW%XMIN=X1; MPW%XMAX=X2; MPW%YMIN=Y1; MPW%YMAX=Y2; RETURN
 ENDIF
ENDIF

!## gen usage
CALL WDIALOGSELECT(ID_DMATHTAB1)
CALL WDIALOGGETCHECKBOX(IDF_CHECK3,IGEN)
IF(IGEN.EQ.1)CALL WDIALOGGETSTRING(ID_FILE4,GENNAME)
!## iuse nodata
CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,INODATA); INODATA=INODATA-1 
NODATA_VALUE=0.0D0; IF(INODATA.EQ.1)CALL WDIALOGGETDOUBLE(IDF_REAL1,NODATA_VALUE)
CALL WDIALOGGETDOUBLE(IDF_REAL2,TRIM_VALUE)
!## transform into equidist. idf
CALL WDIALOGGETCHECKBOX(IDF_CHECK2,IEQUI)

!## number idf to be processed
ALLOCATE(IDFNAMES(1,3))
IDFNAMES=''
!##get idfnames
IF(IG(1).GT.0)CALL WDIALOGGETSTRING(ID_FILE1,IDFNAMES(1,1))
IF(IG(2).GT.0)CALL WDIALOGGETSTRING(ID_FILE2,IDFNAMES(1,2))
IF(IG(3).GT.0)THEN
 CALL WDIALOGGETSTRING(ID_FILE3,IDFNAMES(1,3))
 CALL UTL_CHECKNAME(IDFNAMES(1,3),'IDF')
ENDIF

LEX=MATH1CALC(0,LEGNAME)
IF(LEX)THEN
 CALL MANAGER_UTL_ADDFILE(IDFNAMEGIVEN=IDFNAMES(1,3),LEGNAME=LEGNAME)
 CALL IDFPLOTFAST(1)       
ENDIF
CALL MATH1CALCCLOSE(0)

MPW%XMIN=X1; MPW%XMAX=X2; MPW%YMIN=Y1; MPW%YMAX=Y2

END SUBROUTINE MATH1COMPUTEMAIN

!###======================================================================
SUBROUTINE MATH1SCALEMAIN(LEX)
!###======================================================================
USE MOD_MATH_SCALE_PAR
IMPLICIT NONE
LOGICAL,INTENT(OUT) :: LEX
CHARACTER(LEN=256) :: LINE
INTEGER :: I,IOS
REAL(KIND=DP_KIND) :: X1,X2,Y1,Y2

LEX=.FALSE.

IBUFFER=0
DHX=0.0D0
DHY=0.0D0
DHZ=0.0D0
HOR_FCT=1.0D0

X1=MPW%XMIN; X2=MPW%XMAX; Y1=MPW%YMIN; Y2=MPW%YMAX

CALL WDIALOGSELECT(ID_DMATHTAB2)
!## get factor
CALL WDIALOGGETDOUBLE(IDF_REAL2,SCLSIZE)

CALL WDIALOGGETDOUBLE(IDF_REAL1,SFCT)
!CALL WDIALOGGETMENU(IDF_MENU3,IINT,STRING)
!READ(STRING,*) IINT
IINT=4

!## get option
CALL WDIALOGGETMENU(IDF_MENU1,SCLTYPE_UP)
CALL WDIALOGGETMENU(IDF_MENU2,SCLTYPE_DOWN)
!SCLTYPE=SCLTYPE*ISCALE

ALLOCATE(IDFNAMES(1),OUTNAMES(1))

!## get idfnames
CALL WDIALOGGETSTRING(ID_FILE1,IDFNAMES(1))
CALL WDIALOGGETSTRING(ID_FILE3,OUTNAMES(1))

CALL WDIALOGSELECT(ID_DMATH); CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IIEXT)
IF(IIEXT.EQ.3)THEN
 CALL WDIALOGGETSTRING(IDF_STRING1,LINE)
 READ(LINE,*,IOSTAT=IOS) MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot read window correctly','Error')
  MPW%XMIN=X1; MPW%XMAX=X2; MPW%YMIN=Y1; MPW%YMAX=Y2; RETURN
 ENDIF
ENDIF

IF(MATH1SCALE(0,0.0D0,0.0D0))THEN
 SELECT CASE (SCLTYPE_UP)
  CASE (11,12,13,14)
   I=INDEX(OUTNAMES(1),'.',.TRUE.)-1
   CALL MANAGER_UTL_ADDFILE(IDFNAMEGIVEN=OUTNAMES(1)(:I)//'_minimal.idf')
   CALL MANAGER_UTL_ADDFILE(IDFNAMEGIVEN=OUTNAMES(1)(:I)//'_maximal.idf')
   CALL IDFPLOTFAST(1)
  CASE DEFAULT
   CALL MANAGER_UTL_ADDFILE(IDFNAMEGIVEN=OUTNAMES(1))
   CALL IDFPLOTFAST(1)
 END SELECT
 LEX=.TRUE.
ENDIF

CALL MATH1SCALECLOSE(0)
MPW%XMIN=X1; MPW%XMAX=X2; MPW%YMIN=Y1; MPW%YMAX=Y2

END SUBROUTINE MATH1SCALEMAIN

!###======================================================================
SUBROUTINE MATH1MERGEMAIN(LEX)
!###======================================================================
USE MOD_MATH_MERGE_PAR
IMPLICIT NONE
LOGICAL,INTENT(OUT) :: LEX
CHARACTER(LEN=256) :: LINE
INTEGER :: I,J,IOS
REAL(KIND=DP_KIND) :: X1,X2,Y1,Y2

CALL WDIALOGSELECT(ID_DMATHTAB3); CALL WDIALOGGETMENU(IDF_MENU1,ACTLIST)

IF(SUM(ACTLIST).EQ.0)THEN
 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should select at least two idf"s!','Error')
 RETURN
ENDIF
DO I=1,MXMPLOT
 IF(ACTLIST(I).EQ.1.AND.MP(I)%IPLOT.NE.1)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should select idf"s only!','Error')
  RETURN
 ENDIF
END DO

ALLOCATE(IDFNAMES(SUM(ACTLIST)))

X1=MPW%XMIN; X2=MPW%XMAX; Y1=MPW%YMIN; Y2=MPW%YMAX

CALL WDIALOGSELECT(ID_DMATH); CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IEXT)

IF(IEXT.EQ.3)THEN
 CALL WDIALOGGETSTRING(IDF_STRING1,LINE)
 READ(LINE,*,IOSTAT=IOS) MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot read window correctly','Error')
  MPW%XMIN=X1; MPW%XMAX=X2; MPW%YMIN=Y1; MPW%YMAX=Y2; RETURN
 ENDIF
 IEXT=1
ENDIF

CALL WDIALOGSELECT(ID_DMATHTAB3)
CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IMASK)

!## get idfname for mask
IF(IMASK.EQ.1)CALL WDIALOGGETSTRING(ID_FILE1,MSKNAME)

J=0
DO I=1,MXMPLOT
 IF(ACTLIST(I).EQ.1.AND.MP(I)%IPLOT.EQ.1)THEN
  J=J+1
  IDFNAMES(J)=MP(I)%IDFNAME
 ENDIF
ENDDO

!## output
CALL WDIALOGGETSTRING(ID_FILE2,OUTNAME)
IF(LEN_TRIM(OUTNAME).EQ.0)THEN
 CALL MATH1MERGECLOSE(0)
 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should specify a yielding IDF filename.','Error')
 RETURN
ENDIF

LEX=MATH1MERGE(0)
IF(LEX)THEN
 CALL MANAGER_UTL_ADDFILE(IDFNAMEGIVEN=OUTNAME)
 CALL IDFPLOTFAST(1) 
ENDIF
CALL MATH1MERGECLOSE(0)
MPW%XMIN=X1; MPW%XMAX=X2; MPW%YMIN=Y1; MPW%YMAX=Y2

END SUBROUTINE MATH1MERGEMAIN

 !###======================================================================
 SUBROUTINE MATH1SUMMAIN(LEX,IOPTION)
 !###======================================================================
 USE MOD_MEAN_CLC, ONLY : MEAN1COMPUTE_SUM
 IMPLICIT NONE
 LOGICAL,INTENT(OUT) :: LEX
 INTEGER,INTENT(IN) :: IOPTION
 CHARACTER(LEN=256),DIMENSION(:),ALLOCATABLE :: IDFNAMES
 CHARACTER(LEN=256) :: LINE
 INTEGER :: I,J,IOS,ISIZE
 REAL(KIND=DP_KIND) :: X1,X2,Y1,Y2

 LEX=.FALSE.
 
 CALL WDIALOGSELECT(ID_DMATHTAB3);CALL WDIALOGGETMENU(IDF_MENU1,ACTLIST)
 IF(SUM(ACTLIST).EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should select at least two idf"s!','Error')
  RETURN
 ENDIF
 DO I=1,MXMPLOT
  IF(ACTLIST(I).EQ.1.AND.MP(I)%IPLOT.NE.1)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should select idf"s only!','Error')
   RETURN
  ENDIF
 END DO

 X1=MPW%XMIN; X2=MPW%XMAX; Y1=MPW%YMIN; Y2=MPW%YMAX

 CALL WDIALOGSELECT(ID_DMATH); CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,ISIZE)
 IF(ISIZE.EQ.3)THEN
  CALL WDIALOGGETSTRING(IDF_STRING1,LINE)
  READ(LINE,*,IOSTAT=IOS) MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX
  IF(IOS.NE.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot read window correctly','Error')
   MPW%XMIN=X1; MPW%XMAX=X2; MPW%YMIN=Y1; MPW%YMAX=Y2; RETURN
  ENDIF
  ISIZE=1
 ENDIF

 ALLOCATE(IDFNAMES(0:SUM(ACTLIST)))

 J=0; DO I=1,MXMPLOT
  IF(ACTLIST(I).EQ.1.AND.MP(I)%IPLOT.EQ.1)THEN
   J=J+1; IDFNAMES(J)=MP(I)%IDFNAME
  ENDIF
 ENDDO

 !## output
 CALL WDIALOGSELECT(ID_DMATHTAB3)
 CALL WDIALOGGETSTRING(ID_FILE2,IDFNAMES(0))
 IF(LEN_TRIM(IDFNAMES(0)).EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should specify a yielding IDF filename.','Error')
  DEALLOCATE(IDFNAMES); RETURN
 ENDIF

 LEX=MEAN1COMPUTE_SUM(IDFNAMES,SIZE(IDFNAMES)-1,ISIZE,IOPTION)
 IF(LEX)CALL MANAGER_UTL_ADDFILE(IDFNAMEGIVEN=IDFNAMES(0))
 !call idfplotfast()       

 DEALLOCATE(IDFNAMES)
 !## reset window coordinates
 MPW%XMIN=X1; MPW%XMAX=X2; MPW%YMIN=Y1; MPW%YMAX=Y2

 END SUBROUTINE MATH1SUMMAIN

END MODULE MOD_MATH
