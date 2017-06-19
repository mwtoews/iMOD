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
!!
MODULE MOD_IDFTIMESERIE_UTL

USE WINTERACTER
USE RESOURCE
USE DATEVAR
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MODPLOT, ONLY : MXMPLOT,MP
USE MOD_UTL, ONLY : JD,UTL_JDATETOIDATE,UTL_FILLDATES,IDATETOGDATE,FTIMETOCTIME,CTIMETOFTIME,ITOS,UTL_IDATETOJDATE, &
    UTL_DIRINFO_POINTER,UTL_IDFGETDATES,UTL_MESSAGEHANDLE,UTL_IDFGETDATE,UTL_IMODFILLMENU,UTL_CREATEDIR,UTL_DEBUGLEVEL
USE MOD_IDFTIMESERIE_PAR

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION IDFTIMESERIE_DATES(IOPTION,DIR,ISEC,IEXT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPTION
 CHARACTER(LEN=*),INTENT(OUT),OPTIONAL :: DIR
 INTEGER,INTENT(OUT),OPTIONAL :: ISEC,IEXT
 INTEGER :: I,J,K,ID1,ID2,IPLOT,O,M,IM,IY,ID,IDFDATE,N
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: IDFNAMES
 CHARACTER(LEN=8) :: CDATES
 CHARACTER(LEN=50) :: ROOT
 INTEGER :: ITYPE,DSKIP  
 REAL :: DAYFRACTION
 TYPE(WIN_MESSAGE) :: MESSAGE
 CHARACTER(LEN=1000) :: LINE

 IDFTIMESERIE_DATES=.FALSE.

 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(3,'Initialisation ...')
 CALL UTL_MESSAGEHANDLE(0)

 MINDATE=21000101
 MAXDATE=19000101

 !## get them all and read date
 N=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)THEN
   N=N+1
   IDFDATE=UTL_IDFGETDATE(MP(IPLOT)%IDFNAME,DAYFRACTION)
   IF(IDFDATE.NE.0)THEN
    WRITE(CDATES,'(I8)') IDFDATE
    I=INDEX(MP(IPLOT)%IDFNAME,'\',.TRUE.)
    J=INDEX(MP(IPLOT)%IDFNAME,CDATES,.TRUE.)
    !## get root
    ROOT=MP(IPLOT)%IDFNAME(I+1:J-1)
    !## get them all
    IF(DAYFRACTION.LT.0.0)THEN
     IF(UTL_DIRINFO_POINTER(MP(IPLOT)%IDFNAME(:I-1),TRIM(ROOT)//'????????'//TRIM(MP(IPLOT)%IDFNAME(J+8:)),IDFNAMES,'F'))THEN; ENDIF
    ELSE
     IF(UTL_DIRINFO_POINTER(MP(IPLOT)%IDFNAME(:I-1),TRIM(ROOT)//'??????????????'//TRIM(MP(IPLOT)%IDFNAME(J+14:)),IDFNAMES,'F'))THEN; ENDIF
    ENDIF
    NFILES(N)=SIZE(IDFNAMES)
    CALL UTL_IDFGETDATES(IDFNAMES,NFILES(N),M,O,ID1,ID2,0)
    MINDATE=MIN(ID1,MINDATE)
    MAXDATE=MAX(ID2,MAXDATE)
    NFILES(N)=O
   ENDIF
  ENDIF
 ENDDO

 CALL WINDOWOUTSTATUSBAR(3,'')
 CALL WINDOWOUTSTATUSBAR(4,'')

 IF(N.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should select at least one IDF with timevariant information!','Warning')
  RETURN
 ENDIF

 IF(SUM(NFILES).EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No extra files found for the selected idf"s.','Error')
  RETURN
 ENDIF
  
 IF(MAXVAL(NFILES).LE.1)THEN
  IF(MAXVAL(NFILES).EQ.0)LINE='No files with date-info (8 numbers repr. [yyyymmdd]) found that match the selected idf"s.'
  !## minimal one of the selected idf files need to contain time-variant information
  IF(MAXVAL(NFILES).EQ.1)LINE='No other files found than the one selected:'
  DO IPLOT=1,MXMPLOT
   IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)THEN
    LINE=TRIM(LINE)//CHAR(13)//TRIM(MP(IPLOT)%IDFNAME)
   ENDIF
  ENDDO
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(LINE),'Warning')
  RETURN
 ENDIF
 
 CALL UTL_MESSAGEHANDLE(1)

 CALL WDIALOGLOAD(ID_DTIMESERIESDATES,ID_DTIMESERIESDATES)
 CALL WDIALOGPUTSTRING(IDF_RADIO1,'Use ALL available dates ('//TRIM(ITOS(SUM(NFILES)))//'-files)')
 CALL IDATETOGDATE(MINDATE,IY,IM,ID)
 CALL WDIALOGPUTMENU(IDF_MENU1,CDATE,12,IM)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,ID)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER2,IY)
 CALL IDATETOGDATE(MAXDATE,IY,IM,ID)
 CALL WDIALOGPUTMENU(IDF_MENU2,CDATE,12,IM)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER3,ID)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER4,IY)

 SELECT CASE (IOPTION)
  CASE (1)
   CALL WDIALOGFIELDSTATE(IDF_LABEL4,3)
   CALL WDIALOGFIELDSTATE(IDF_LABEL3,3)
   CALL WDIALOGFIELDSTATE(IDF_INTEGER6,3)
   CALL WDIALOGFIELDSTATE(IDF_MENU3,3)
   CALL WDIALOGFIELDSTATE(IDF_GROUP2,3)
   CALL WDIALOGFIELDSTATE(IDF_MENU4,3)
!   CALL WDIALOGFIELDSTATE(IDF_RADIO4,3)
!   CALL WDIALOGFIELDSTATE(IDF_RADIO5,3)
  CASE (2)
   !## fill menu with existing movies
   CALL UTL_CREATEDIR(TRIM(PREFVAL(1))//'\MOVIES')
   CALL UTL_IMODFILLMENU(IDF_MENU3,TRIM(PREFVAL(1))//'\MOVIES','*','D',N,0,0)
   CALL WDIALOGFIELDSTATE(IDF_MENU3,1)
   CALL WDIALOGPUTINTEGER(IDF_INTEGER6,30)
 END SELECT
 
 CALL WDIALOGSHOW(-1,-1,0,3)
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)

  SELECT CASE (ITYPE)
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_CHECK1)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
      CALL WDIALOGFIELDSTATE(IDF_INTEGER5,I)    
     CASE (IDF_RADIO1,IDF_RADIO2)
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
      I=I-1
      CALL WDIALOGFIELDSTATE(IDF_INTEGER1,I)
      CALL WDIALOGFIELDSTATE(IDF_INTEGER2,I)
      CALL WDIALOGFIELDSTATE(IDF_INTEGER3,I)
      CALL WDIALOGFIELDSTATE(IDF_INTEGER4,I)
      CALL WDIALOGFIELDSTATE(IDF_MENU1,I)
      CALL WDIALOGFIELDSTATE(IDF_MENU2,I)
      CALL WDIALOGFIELDSTATE(IDF_LABEL1,I)
      CALL WDIALOGFIELDSTATE(IDF_LABEL2,I)
     CASE (IDF_INTEGER1,IDF_MENU1,IDF_INTEGER2)
      CALL UTL_FILLDATES(IDF_INTEGER2,IDF_MENU1,IDF_INTEGER1)
     CASE (IDF_INTEGER3,IDF_MENU2,IDF_INTEGER4)
      CALL UTL_FILLDATES(IDF_INTEGER4,IDF_MENU2,IDF_INTEGER3)

    END SELECT
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDOK)
      CALL WDIALOGGETINTEGER(IDF_INTEGER1,ID)
      CALL WDIALOGGETMENU(IDF_MENU1,IM)
      CALL WDIALOGGETINTEGER(IDF_INTEGER2,IY)
      MINDATE=JD(IY,IM,ID)
      CALL WDIALOGGETINTEGER(IDF_INTEGER3,ID)
      CALL WDIALOGGETMENU(IDF_MENU2,IM)
      CALL WDIALOGGETINTEGER(IDF_INTEGER4,IY)
      MAXDATE=JD(IY,IM,ID)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
      DSKIP=1; IF(I.EQ.1)CALL WDIALOGGETINTEGER(IDF_INTEGER5,DSKIP)    
      IF(IOPTION.EQ.2)THEN
       CALL UTL_DEBUGLEVEL(0)
       CALL WDIALOGGETMENU(IDF_MENU3,I,DIR)
       CALL UTL_DEBUGLEVEL(1)
       IF(TRIM(DIR).EQ.'')THEN
        CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to Select an existing or Specify a NEW output folder.','Error')
       ELSE
        CALL WDIALOGGETINTEGER(IDF_INTEGER6,ISEC)
        IF(ISEC.LE.0)THEN
         CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to specify the length of the movie to be more than 0 seconds.','Error')
        ELSE
         CALL WDIALOGGETMENU(IDF_MENU4,IEXT)
         EXIT
        ENDIF
       ENDIF
      ELSE
       EXIT
      ENDIF
     CASE (IDCANCEL)
!      MINDATE=UTL_IDATETOJDATE(MINDATE)
!      MAXDATE=UTL_IDATETOJDATE(MAXDATE)
      EXIT
     CASE (IDHELP)
       CALL IMODGETHELP('5.2','TMO.TimeTool')
    END SELECT
  END SELECT
 ENDDO

 CALL WDIALOGUNLOAD()
 IF(MESSAGE%VALUE1.EQ.IDCANCEL)RETURN
  
 CALL UTL_MESSAGEHANDLE(0)

 N=SIZE(NFILES); M=MAXVAL(NFILES); ALLOCATE(LISTFILES(M,N)); LISTFILES=''
 NFILES=0
  
 !## get them all
 N=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)THEN
   N=N+1
   IDFDATE=UTL_IDFGETDATE(MP(IPLOT)%IDFNAME,DAYFRACTION)
   IF(IDFDATE.NE.0)THEN
    WRITE(CDATES,'(I8)') IDFDATE
    I=INDEX(MP(IPLOT)%IDFNAME,'\',.TRUE.)
    J=INDEX(MP(IPLOT)%IDFNAME,CDATES,.TRUE.)
    !## get root
    ROOT=MP(IPLOT)%IDFNAME(I+1:J-1)
    !## get them all
    IF(DAYFRACTION.LT.0.0)THEN
     IF(UTL_DIRINFO_POINTER(MP(IPLOT)%IDFNAME(:I-1),TRIM(ROOT)//'????????'//TRIM(MP(IPLOT)%IDFNAME(J+8:)),IDFNAMES,'F'))THEN; ENDIF
    ELSE
     IF(UTL_DIRINFO_POINTER(MP(IPLOT)%IDFNAME(:I-1),TRIM(ROOT)//'??????????????'//TRIM(MP(IPLOT)%IDFNAME(J+14:)),IDFNAMES,'F'))THEN; ENDIF
    ENDIF

    !## see if file fits in selection
    K=0
    DO J=1,SIZE(IDFNAMES),DSKIP
     IDFDATE=UTL_IDFGETDATE(IDFNAMES(J)) 
     IDFDATE=UTL_IDATETOJDATE(IDFDATE)
     !## proper idf name and date is inside mindate/maxdate
     IF(IDFDATE.NE.0.AND.(IDFDATE.GE.MINDATE.AND.IDFDATE.LE.MAXDATE))THEN
      K=K+1; NFILES(N)=NFILES(N)+1
      LISTFILES(K,N)=MP(IPLOT)%IDFNAME(:I-1)//'\'//TRIM(IDFNAMES(J))
     ENDIF
    ENDDO
   ELSE
    K=1; NFILES(N)=1
    LISTFILES(K,N)=MP(IPLOT)%IDFNAME
   ENDIF

  ENDIF
 ENDDO

 CALL UTL_MESSAGEHANDLE(1)

 IF(SUM(NFILES).EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No extra files found for the selected idf"s.','Error')
  RETURN
 ENDIF

 IDFTIMESERIE_DATES=.TRUE.

 END FUNCTION IDFTIMESERIE_DATES

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_GETMINMAXX(XMIN,XMAX,XINT,IFX,IDURATION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDURATION
 INTEGER,INTENT(OUT) :: IFX
 REAL,INTENT(OUT) :: XMIN,XMAX,XINT
 INTEGER :: IY,IM,ID
 CHARACTER(LEN=12) :: CTIME

 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IFX)
 IF(IFX.EQ.0)RETURN

 IF(IDURATION.EQ.0)THEN

  !## get axes information on dialog
  CALL WDIALOGGETINTEGER(IDF_INTEGER1,ID)
  CALL WDIALOGGETMENU(IDF_MENU1,IM)
  CALL WDIALOGGETINTEGER(IDF_INTEGER2,IY)
  XMIN=REAL(JD(IY,IM,ID))-MINDATE
  
  CALL WDIALOGGETSTRING(IDF_STRING4,CTIME)
  XMIN=XMIN+CTIMETOFTIME(CTIME)
  
  CALL WDIALOGGETINTEGER(IDF_INTEGER3,ID)
  CALL WDIALOGGETMENU(IDF_MENU2,IM)
  CALL WDIALOGGETINTEGER(IDF_INTEGER4,IY)
  XMAX=REAL(JD(IY,IM,ID))-MINDATE

  CALL WDIALOGGETSTRING(IDF_STRING5,CTIME)
  XMAX=XMAX+CTIMETOFTIME(CTIME)

  CALL WDIALOGGETREAL(IDF_REAL3,XINT)

 ELSE

  CALL WDIALOGGETREAL(IDF_REAL6,XMIN)
  CALL WDIALOGGETREAL(IDF_REAL5,XMAX)
  CALL WDIALOGGETREAL(IDF_REAL7,XINT)

 ENDIF

 XINT=MAX(XINT,(XMAX-XMIN)/100.0)

 END SUBROUTINE IDFTIMESERIE_GETMINMAXX

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_GETMINMAXY(YMIN,YMAX,YINT,IFY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IFY
 REAL,INTENT(OUT) :: YMIN,YMAX,YINT

 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,IFY)
 IF(IFY.EQ.0)RETURN

 !## get axes information on dialog
 CALL WDIALOGGETREAL(IDF_REAL1,YMIN)
 CALL WDIALOGGETREAL(IDF_REAL2,YMAX)
 CALL WDIALOGGETREAL(IDF_REAL4,YINT)

 IF(YINT.LE.0.0) YINT=(YMAX-YMIN)/10.0

 END SUBROUTINE IDFTIMESERIE_GETMINMAXY

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_GETMINMAXY2(Y2MIN,Y2MAX,Y2INT,IFY2)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IFY2
 REAL,INTENT(OUT) :: Y2MIN,Y2MAX,Y2INT

 CALL WDIALOGGETCHECKBOX(IDF_CHECK5,IFY2)
 IF(IFY2.EQ.0)RETURN

 !## get axes information on dialog
 CALL WDIALOGGETREAL(IDF_REAL10,Y2MIN)
 CALL WDIALOGGETREAL(IDF_REAL9 ,Y2MAX)
 CALL WDIALOGGETREAL(IDF_REAL8 ,Y2INT)

 IF(Y2INT.LE.0.0)  Y2INT=(Y2MAX-Y2MIN)/10.0

 END SUBROUTINE IDFTIMESERIE_GETMINMAXY2

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_PUTMINMAXX(XMIN,XMAX,XINT,IDURATION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDURATION
 REAL,INTENT(IN) :: XMIN,XMAX,XINT
 INTEGER :: IY,IM,ID,IDATE,IFX,NDAY
 REAL :: FTIME
 CHARACTER(LEN=12) :: CTIME
 
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IFX); IF(IFX.EQ.1)RETURN

 IF(IDURATION.EQ.0)THEN !## put axes information on dialog

  IDATE=UTL_JDATETOIDATE(INT(XMIN)+MINDATE)
  CALL IDATETOGDATE(IDATE,IY,IM,ID)
  CALL WDIALOGPUTOPTION(IDF_MENU1,IM)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER2,IY)
  NDAY=WDATEDAYSINMONTH(IY,IM)
  CALL WDIALOGRANGEINTEGER(IDF_INTEGER1,1,NDAY)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER1,MIN(ID,NDAY))
  
  FTIME=XMIN-FLOOR(XMIN); CALL FTIMETOCTIME(FTIME,CTIME)
  CALL WDIALOGPUTSTRING(IDF_STRING4,TRIM(CTIME))
  
  IDATE=UTL_JDATETOIDATE(INT(XMAX)+MINDATE)
  CALL IDATETOGDATE(IDATE,IY,IM,ID)
  CALL WDIALOGPUTOPTION(IDF_MENU2,IM)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER4,IY)

  NDAY=WDATEDAYSINMONTH(IY,IM)
  CALL WDIALOGRANGEINTEGER(IDF_INTEGER3,1,NDAY)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER3,MIN(ID,NDAY))
  CALL WDIALOGPUTREAL(IDF_REAL3,XINT,'(F10.2)')

  FTIME=XMAX-FLOOR(XMAX); CALL FTIMETOCTIME(FTIME,CTIME)
  CALL WDIALOGPUTSTRING(IDF_STRING5,TRIM(CTIME))

 ELSE

  CALL WDIALOGPUTREAL(IDF_REAL6,XMIN)
  CALL WDIALOGPUTREAL(IDF_REAL5,XMAX)
  CALL WDIALOGPUTREAL(IDF_REAL7,XINT)

 ENDIF

 END SUBROUTINE IDFTIMESERIE_PUTMINMAXX

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_PUTMINMAXY(YMIN,YMAX,YINT)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: YMIN,YMAX,YINT
 INTEGER :: IFY

 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,IFY); IF(IFY.EQ.1)RETURN

 CALL WDIALOGPUTREAL(IDF_REAL1,YMIN)
 CALL WDIALOGPUTREAL(IDF_REAL2,YMAX)
 CALL WDIALOGPUTREAL(IDF_REAL4,YINT,'(F10.2)')

 END SUBROUTINE IDFTIMESERIE_PUTMINMAXY

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_PUTMINMAXY2(Y2MIN,Y2MAX,Y2INT)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: Y2MIN,Y2MAX,Y2INT
 INTEGER :: IFY2

 CALL WDIALOGGETCHECKBOX(IDF_CHECK5,IFY2); IF(IFY2.EQ.1)RETURN

 CALL WDIALOGPUTREAL(IDF_REAL10,Y2MIN)
 CALL WDIALOGPUTREAL(IDF_REAL9 ,Y2MAX)
 CALL WDIALOGPUTREAL(IDF_REAL8 ,Y2INT,'(F10.2)')

 END SUBROUTINE IDFTIMESERIE_PUTMINMAXY2

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_FIELDS(IDURATION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDURATION
 INTEGER :: I,J 
 INTEGER,DIMENSION(26) :: ID
 DATA ID/IDF_INTEGER1,IDF_INTEGER2,IDF_INTEGER3,IDF_INTEGER4, &
         IDF_MENU1,IDF_MENU2,IDF_LABEL1,IDF_LABEL2,IDF_LABEL5,IDF_LABEL6,IDF_REAL3,IDF_STRING4,IDF_STRING5, &   !## x-as
         IDF_LABEL3,IDF_LABEL4,IDF_REAL1,IDF_REAL2,IDF_LABEL7,IDF_REAL4,    &           !## y-as
         IDF_LABEL9,IDF_LABEL10,IDF_LABEL11,IDF_LABEL12,IDF_REAL5,IDF_REAL6,IDF_REAL7/                          !## x-as (perc)

 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
 IF(IDURATION.EQ.0)THEN
  DO J=1,13; CALL WDIALOGFIELDSTATE(ID(J),I); END DO
 ELSE
  DO J=20,26; CALL WDIALOGFIELDSTATE(ID(J),I); END DO
 ENDIF

 !## y-as
 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,I)
 DO J=14,19; CALL WDIALOGFIELDSTATE(ID(J),I); END DO

 !## from date
 CALL UTL_FILLDATES(IDF_INTEGER2,IDF_MENU1,IDF_INTEGER1) 
 !## to date
 CALL UTL_FILLDATES(IDF_INTEGER4,IDF_MENU2,IDF_INTEGER3) 

 END SUBROUTINE IDFTIMESERIE_FIELDS

END MODULE MOD_IDFTIMESERIE_UTL

