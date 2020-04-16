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
MODULE MOD_IDFTIMESERIE_UTL

USE WINTERACTER
USE RESOURCE
USE MOD_DBL
USE DATEVAR
USE MODPLOT, ONLY : MPW
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MODPLOT, ONLY : MXMPLOT,MP
USE MOD_IDF
USE MOD_MOVIE_PAR, ONLY : EXT
USE MOD_UTL
USE MOD_IDFTIMESERIE_PAR
USE IMODVAR, ONLY : DP_KIND,SP_KIND
USE MOD_IPF_PAR, ONLY : NIPF,IPF,ASSF
USE MOD_MANAGER_UTL
USE MOD_IPFASSFILE_UTL

CONTAINS

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_FIELDSMAINMENU(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_FILE,1).NE.I)CALL WMENUSETSTATE(ID_FILE,1,I)
 IF(WMENUGETSTATE(ID_EDIT,1).NE.I)CALL WMENUSETSTATE(ID_EDIT,1,I)
 IF(WMENUGETSTATE(ID_VIEW,1).NE.I)CALL WMENUSETSTATE(ID_VIEW,1,I)
 IF(WMENUGETSTATE(ID_MAP,1).NE.I)CALL WMENUSETSTATE(ID_MAP,1,I)
 IF(WMENUGETSTATE(ID_TOOLBOX,1).NE.I)CALL WMENUSETSTATE(ID_TOOLBOX,1,I)

 IF(WMENUGETSTATE(ID_NEW,1).NE.I)CALL WMENUSETSTATE(ID_NEW,1,I)
 IF(WMENUGETSTATE(ID_OPEN,1).NE.I)CALL WMENUSETSTATE(ID_OPEN,1,I)
 IF(WMENUGETSTATE(ID_SAVE,1).NE.I)CALL WMENUSETSTATE(ID_SAVE,1,I)
 IF(WMENUGETSTATE(ID_SAVEAS,1).NE.I)CALL WMENUSETSTATE(ID_SAVEAS,1,I)
 IF(WMENUGETSTATE(ID_COPY,1).NE.I)CALL WMENUSETSTATE(ID_COPY,1,I)
 IF(WMENUGETSTATE(ID_MANAGER,1).NE.I)CALL WMENUSETSTATE(ID_MANAGER,1,I)
 IF(WMENUGETSTATE(ID_IRDATABASE,1).NE.I)CALL WMENUSETSTATE(ID_IRDATABASE,1,I)
 IF(I.EQ.0)THEN
  IF(WMENUGETSTATE(ID_PROFILE,1).NE.I)CALL WMENUSETSTATE(ID_PROFILE,1,I)
  IF(WMENUGETSTATE(ID_IMODINFO,1).NE.I)CALL WMENUSETSTATE(ID_IMODINFO,1,I)
  IF(WMENUGETSTATE(ID_TIMESERIES,1).NE.I)CALL WMENUSETSTATE(ID_TIMESERIES,1,I)
  IF(WMENUGETSTATE(ID_OPENIDF,1).NE.I)CALL WMENUSETSTATE(ID_OPENIDF,1,I)
  IF(WMENUGETSTATE(ID_3DTOOL,1).NE.I)CALL WMENUSETSTATE(ID_3DTOOL,1,I)
  IF(WMENUGETSTATE(ID_MOVIE,1).NE.I)CALL WMENUSETSTATE(ID_MOVIE,1,I)
 ENDIF
 IF(I.EQ.1)CALL MANAGER_UTL_UPDATE()

 END SUBROUTINE IDFTIMESERIE_FIELDSMAINMENU
 
 !###======================================================================
 SUBROUTINE IDFTIMESERIES_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J
 LOGICAL :: LEX

 IF(ALLOCATED(IDF))THEN
  DO I=1,SIZE(IDF,1)
   DO J=1,SIZE(IDF,2)
    CALL IDFDEALLOCATEX(IDF(I,J))
    INQUIRE(UNIT=IDF(I,J)%IU,OPENED=LEX)
    IF(LEX)CLOSE(IDF(I,J)%IU)
   END DO
  END DO
  DEALLOCATE(IDF)
 ENDIF

 IF(ALLOCATED(TS))THEN
  DO I=1,SIZE(TS)
   IF(ASSOCIATED(TS(I)%IDATE))DEALLOCATE(TS(I)%IDATE) !#IDATA
   IF(ASSOCIATED(TS(I)%VALUE))DEALLOCATE(TS(I)%VALUE) !#VALUE
   IF(ASSOCIATED(TS(I)%IYR))DEALLOCATE(TS(I)%IYR) !#IYR
   IF(ASSOCIATED(TS(I)%IMH))DEALLOCATE(TS(I)%IMH) !#IMH
   IF(ASSOCIATED(TS(I)%IDY))DEALLOCATE(TS(I)%IDY) !#IDY
   IF(ASSOCIATED(TS(I)%IHR))DEALLOCATE(TS(I)%IHR) !#IHR
   IF(ASSOCIATED(TS(I)%IMT))DEALLOCATE(TS(I)%IMT) !#IMT
   IF(ASSOCIATED(TS(I)%ISC))DEALLOCATE(TS(I)%ISC) !#ISC  
  ENDDO
  DEALLOCATE(TS)
 ENDIF

 IF(ALLOCATED(LEGENDNAME))DEALLOCATE(LEGENDNAME)
 IF(ALLOCATED(LISTFILES))DEALLOCATE(LISTFILES)
 IF(ALLOCATED(NFILES))DEALLOCATE(NFILES)
 IF(ALLOCATED(MFILES))DEALLOCATE(MFILES)
 IF(ALLOCATED(LTYPE))DEALLOCATE(LTYPE)

 IF(NIPF.GT.0)THEN
  CALL IPFCLOSEASSFILE()
  !## make copy into
  IF(ALLOCATED(TSIPF))THEN
   DO I=1,SIZE(TSIPF)
    IF(ASSOCIATED(TSIPF(I)%IDATE))DEALLOCATE(TSIPF(I)%IDATE)
    IF(ASSOCIATED(TSIPF(I)%VALUE))DEALLOCATE(TSIPF(I)%VALUE)
    IF(ASSOCIATED(TSIPF(I)%IYR))DEALLOCATE(TSIPF(I)%IYR)
    IF(ASSOCIATED(TSIPF(I)%IMH))DEALLOCATE(TSIPF(I)%IMH)
    IF(ASSOCIATED(TSIPF(I)%IDY))DEALLOCATE(TSIPF(I)%IDY)
    IF(ASSOCIATED(TSIPF(I)%IHR))DEALLOCATE(TSIPF(I)%IHR)
    IF(ASSOCIATED(TSIPF(I)%IMT))DEALLOCATE(TSIPF(I)%IMT)
    IF(ASSOCIATED(TSIPF(I)%ISC))DEALLOCATE(TSIPF(I)%ISC)
   END DO
   DEALLOCATE(TSIPF)
  ENDIF
 ENDIF

 !## make copy from assf() into tsipf()  --- for duration purposes
 IF(ALLOCATED(TSDIFF))THEN
  DO I=1,SIZE(TSDIFF)
   IF(ASSOCIATED(TSDIFF(I)%IDATE))DEALLOCATE(TSDIFF(I)%IDATE)
   IF(ASSOCIATED(TSDIFF(I)%VALUE))DEALLOCATE(TSDIFF(I)%VALUE)
  END DO
  DEALLOCATE(TSDIFF)
 ENDIF

 !## deallocate ipfplus-pointer to store ipf
 IF(ASSOCIATED(IPFPLUS))DEALLOCATE(IPFPLUS)
 IF(ALLOCATED(GRAPHUNITS))DEALLOCATE(GRAPHUNITS)
 IF(ALLOCATED(GRAPHAREA))DEALLOCATE(GRAPHAREA)
 
 END SUBROUTINE
 
 !###======================================================================
 SUBROUTINE IDFTIMESERIE_CLOSE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT
 
 CALL WINDOWOUTSTATUSBAR(3,'Closing ...')

 IF(ALLOCATED(LTYPE))THEN
  !## save all colouring
  NIDF=0
  !## idf
  DO IPLOT=1,MXMPLOT
   IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)THEN
    !## asign colour to it (overwrite default)
    NIDF=NIDF+1; MP(IPLOT)%SCOLOR=LTYPE(NIDF)%ICLR
   ENDIF
  ENDDO
  !## ipf
  DO IPLOT=1,MXMPLOT
   IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.2)THEN
    !## asign colour to it (overwrite default)
    NIDF=NIDF+1; MP(IPLOT)%SCOLOR=LTYPE(NIDF)%ICLR
   ENDIF
  ENDDO
 ENDIF
 
 CALL IDFTIMESERIES_DEALLOCATE()

 CALL WDIALOGSELECT(ID_DTIMESERIES)
 CALL WDIALOGUNLOAD()

 CALL WINDOWSELECT(0)
 CALL WINDOWOUTSTATUSBAR(3,'')

 CALL WMENUSETSTATE(ID_TIMESERIES,2,0)

 IF(IDFS_IBITMAP.NE.0)CALL WBITMAPDESTROY(IDFS_IBITMAP)

 !## reset to entire window
 CALL WINDOWSELECT(MPW%IWIN)
 CALL IGRSELECT(DRAWWIN,MPW%IWIN)
 CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
 CALL DBL_IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX,IOFFSET=1)

 CALL IDFTIMESERIE_FIELDSMAINMENU(1)

 CALL UTL_MESSAGEHANDLE(1)
 
 CALL UTL_HIDESHOWDIALOG(ID_DMANAGER,2)

 END SUBROUTINE IDFTIMESERIE_CLOSE

 !###====================================================================
 SUBROUTINE IDFTIMESERIE_PLUSPLOTPOINT()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I

 !## nothing activated yet!
 IF(.NOT.ASSOCIATED(IPFPLUS))RETURN

 CALL UTL_PLOT1BITMAP()

 CALL IGRCOLOURN(WRGB(0,0,0))
 DO I=1,NPLUS
  CALL IDFTIMESERIE_PLOTPOINT(IPFPLUS(I)%X,IPFPLUS(I)%Y,2,2.0D0)
 END DO

 CALL UTL_PLOT2BITMAP()

 END SUBROUTINE IDFTIMESERIE_PLUSPLOTPOINT

 !###===============================================================================
 SUBROUTINE IDFTIMESERIE_PLOTPOINT(X,Y,SYMBOL,THICKNESS)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: SYMBOL
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y,THICKNESS

 CALL DBL_WGRTEXTFONT(IFAMILY=0,TWIDTH=(THICKNESS*0.4D0)/75.0D0,THEIGHT=(THICKNESS*0.2D0)/25.0D0,ISTYLE=0)
 CALL DBL_IGRMARKER(X,Y,SYMBOL)

 END SUBROUTINE IDFTIMESERIE_PLOTPOINT

 !###======================================================================
 LOGICAL FUNCTION IDFTIMESERIE_DATES(IOPTION,DIR,ISEC,IEXT,RFRAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPTION
 CHARACTER(LEN=*),INTENT(OUT),OPTIONAL :: DIR
 INTEGER,INTENT(OUT),OPTIONAL :: ISEC,IEXT
 REAL(KIND=DP_KIND),INTENT(OUT),OPTIONAL :: RFRAME
 INTEGER :: I,J,K,IPLOT,O,M,IM,IY,ID,IDATE,N,IHR,IMT,ISC
 INTEGER(KIND=8) :: ID1,ID2,DIDATE,MINDATETIME,MAXDATETIME
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: IDFNAMES
 CHARACTER(LEN=50) :: SSTRING !ROOT
 INTEGER :: ITYPE,DSKIP 
 REAL(KIND=DP_KIND) :: DAYFRACTION
 TYPE(WIN_MESSAGE) :: MESSAGE
 CHARACTER(LEN=1000) :: LINE

 IDFTIMESERIE_DATES=.FALSE.

 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(3,'Initialisation ...')
 CALL UTL_MESSAGEHANDLE(0)

 MINDATETIME=22000101000000
 MAXDATETIME=18000101000000

 !## get them all and read date
 N=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)THEN
   N=N+1
   IDATE=UTL_IDFGETDATE(MP(IPLOT)%IDFNAME,DAYFRACTION)
   IF(IDATE.NE.0)THEN

    I=INDEX(MP(IPLOT)%IDFNAME,'\',.TRUE.)+1
    SSTRING=UTL_SUBST(MP(IPLOT)%IDFNAME(I:),TRIM(ITOS(IDATE)),'*')
!    I=INDEX(MP(IPLOT)%IDFNAME,TRIM(ITOS(IDATE)),.TRUE.)
!    J=I+LEN_TRIM(ITOS(IDATE))-1

!    I=INDEX(MP(IPLOT)%IDFNAME,'_',.TRUE.)
!    J=INDEX(MP(IPLOT)%IDFNAME(:I-1),'_',.TRUE.)
!    K=INDEX(MP(IPLOT)%IDFNAME(:J-1),'\',.TRUE.)

    !## get root
!    K=INDEX(MP(IPLOT)%IDFNAME,'\',.TRUE.)
!    ROOT=MP(IPLOT)%IDFNAME(:I-1)

    !## get them all
    I=INDEX(MP(IPLOT)%IDFNAME,'\',.TRUE.)-1
    IF(UTL_DIRINFO_POINTER(MP(IPLOT)%IDFNAME(:I),TRIM(SSTRING),IDFNAMES,'F',CORDER='N'))THEN; ENDIF
!    IF(UTL_DIRINFO_POINTER(MP(IPLOT)%IDFNAME(:K-1),TRIM(ROOT)//'_*_'//TRIM(MP(IPLOT)%IDFNAME(I+1:)),IDFNAMES,'F',CORDER='N'))THEN; ENDIF
    
    NFILES(N)=SIZE(IDFNAMES)
    CALL UTL_IDFGETDATES(IDFNAMES,NFILES(N),M,O,ID1,ID2,0)
    MINDATETIME=MIN(ID1,MINDATETIME)
    MAXDATETIME=MAX(ID2,MAXDATETIME)
    NFILES(N)=O
   ENDIF
  ENDIF
 ENDDO

 CALL WINDOWOUTSTATUSBAR(3,'')
 CALL WINDOWOUTSTATUSBAR(4,'')
 CALL UTL_MESSAGEHANDLE(1)

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

 CALL WDIALOGLOAD(ID_DTIMESERIESDATES,ID_DTIMESERIESDATES)
 CALL WDIALOGPUTSTRING(IDF_RADIO1,'Use ALL available dates ('//TRIM(ITOS(SUM(NFILES)))//'-files)')
 CALL ITIMETOGDATE(MINDATETIME,IY,IM,ID,IHR,IMT,ISC)
 CALL WDIALOGPUTMENU(IDF_MENU1,CDATE,12,IM)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,ID)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER2,IY)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER7,IHR)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER8,IMT)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER9,ISC)
 CALL ITIMETOGDATE(MAXDATETIME,IY,IM,ID,IHR,IMT,ISC)
 CALL WDIALOGPUTMENU(IDF_MENU2,CDATE,12,IM)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER3,ID)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER4,IY)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER12,IHR)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER10,IMT)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER11,ISC)

 SELECT CASE (IOPTION)
  CASE (1)
   CALL WDIALOGFIELDSTATE(IDF_LABEL4,3)
   CALL WDIALOGFIELDSTATE(IDF_INTEGER6,3)
   CALL WDIALOGFIELDSTATE(IDF_REAL1,3)
   CALL WDIALOGFIELDSTATE(IDF_MENU3,3)
   CALL WDIALOGFIELDSTATE(IDF_GROUP2,3)
   CALL WDIALOGFIELDSTATE(IDF_MENU4,3)
   CALL WDIALOGFIELDSTATE(IDF_LABEL5,3)
   CALL WDIALOGFIELDSTATE(IDF_RADIO3,3)
   CALL WDIALOGFIELDSTATE(IDF_RADIO4,3)
   CALL WDIALOGFIELDSTATE(IDF_LABEL6,3)
   CALL WDIALOGFIELDSTATE(IDF_LABEL7,3)
  CASE (2)
   !## fill menu with existing movies
   CALL UTL_CREATEDIR(TRIM(PREFVAL(1))//'\MOVIES')
   CALL UTL_IMODFILLMENU(IDF_MENU3,TRIM(PREFVAL(1))//'\MOVIES','*','D',N,0,0)
   CALL WDIALOGPUTMENU(IDF_MENU4,EXT,SIZE(EXT)-1,1)
   CALL WDIALOGFIELDSTATE(IDF_MENU3,1)
   CALL WDIALOGPUTINTEGER(IDF_INTEGER6,30)
   CALL WDIALOGPUTDOUBLE(IDF_REAL1,24.0D0)
 END SELECT
 
 IF(IOPTION.EQ.1)CALL WDIALOGTITLE('Available Dates for Time Series')
 IF(IOPTION.EQ.2)CALL WDIALOGTITLE('Available Dates for Movies')
 CALL UTL_DIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)

  SELECT CASE (ITYPE)
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_RADIO3,IDF_RADIO4)
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,I)
      CALL WDIALOGFIELDSTATE(IDF_INTEGER7,I)
      CALL WDIALOGFIELDSTATE(IDF_LABEL7,I)    
      I=I-1
      CALL WDIALOGFIELDSTATE(IDF_INTEGER6,I)
      CALL WDIALOGFIELDSTATE(IDF_LABEL6,I)
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
      CALL WDIALOGFIELDSTATE(IDF_INTEGER7,I)
      CALL WDIALOGFIELDSTATE(IDF_INTEGER8,I)
      CALL WDIALOGFIELDSTATE(IDF_INTEGER9,I)
      CALL WDIALOGFIELDSTATE(IDF_INTEGER10,I)
      CALL WDIALOGFIELDSTATE(IDF_INTEGER11,I)
      CALL WDIALOGFIELDSTATE(IDF_INTEGER12,I)
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
      CALL WDIALOGGETINTEGER(IDF_INTEGER7,IHR)
      CALL WDIALOGGETINTEGER(IDF_INTEGER8,IMT)
      CALL WDIALOGGETINTEGER(IDF_INTEGER9,ISC)
      MINDATETIME=YMDHMSTOITIME(IY,IM,ID,IHR,IMT,ISC)
      !## axes round for a day
      MINDATE=JD(IY,IM,ID)
      
      CALL WDIALOGGETINTEGER(IDF_INTEGER3,ID)
      CALL WDIALOGGETMENU(IDF_MENU2,IM)
      CALL WDIALOGGETINTEGER(IDF_INTEGER4,IY)
      CALL WDIALOGGETINTEGER(IDF_INTEGER12,IHR)
      CALL WDIALOGGETINTEGER(IDF_INTEGER10,IMT)
      CALL WDIALOGGETINTEGER(IDF_INTEGER11,ISC)
      MAXDATETIME=YMDHMSTOITIME(IY,IM,ID,IHR,IMT,ISC)
      !## axes round for a day
      MAXDATE=JD(IY,IM,ID)
      
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
      DSKIP=1; IF(I.EQ.1)CALL WDIALOGGETINTEGER(IDF_INTEGER5,DSKIP)    
      IF(IOPTION.EQ.2)THEN
       CALL UTL_DEBUGLEVEL(0)
       CALL WDIALOGGETMENU(IDF_MENU3,I,DIR)
       CALL UTL_DEBUGLEVEL(1)
       IF(TRIM(DIR).EQ.''.OR.INDEX(DIR,':').GT.0.OR.INDEX(DIR,'\').GT.0)THEN
        CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'The output main folder is fixed. Create or Select a subfolder.','Error')
       ELSE
        CALL WDIALOGGETINTEGER(IDF_INTEGER6,ISEC)
        CALL WDIALOGGETDOUBLE(IDF_REAL1,RFRAME)
        CALL WDIALOGGETMENU(IDF_MENU4,IEXT)
        CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,I)
        IF(I.EQ.1)ISEC=0; IF(I.EQ.2)RFRAME=0.0D0
        EXIT
       ENDIF
      ELSE
       EXIT
      ENDIF
     CASE (IDCANCEL)
      EXIT
     CASE (IDHELP)
      CALL UTL_GETHELP('5.2','TMO.TimeTool')
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
   IDATE=UTL_IDFGETDATE(MP(IPLOT)%IDFNAME,DAYFRACTION)
   IF(IDATE.NE.0)THEN

    I=INDEX(MP(IPLOT)%IDFNAME,'\',.TRUE.)+1
    SSTRING=UTL_SUBST(MP(IPLOT)%IDFNAME(I:),TRIM(ITOS(IDATE)),'*')

    !## get them all
    I=INDEX(MP(IPLOT)%IDFNAME,'\',.TRUE.)-1
    IF(UTL_DIRINFO_POINTER(MP(IPLOT)%IDFNAME(:I),TRIM(SSTRING),IDFNAMES,'F',CORDER='N'))THEN; ENDIF
!    I=INDEX(MP(IPLOT)%IDFNAME,'_',.TRUE.)
!    J=INDEX(MP(IPLOT)%IDFNAME(:I-1),'_',.TRUE.)
!    K=INDEX(MP(IPLOT)%IDFNAME(:J-1),'\',.TRUE.)

!    !## get root
!    ROOT=MP(IPLOT)%IDFNAME(K+1:J-1)

!    !## get them all
!    IF(UTL_DIRINFO_POINTER(MP(IPLOT)%IDFNAME(:K-1),TRIM(ROOT)//'_*_'//TRIM(MP(IPLOT)%IDFNAME(I+1:)),IDFNAMES,'F',CORDER='N'))THEN; ENDIF

    !## see if file fits in selection
    I=INDEX(MP(IPLOT)%IDFNAME,'\',.TRUE.)-1
!    I=K
    K=0
    DO J=1,SIZE(IDFNAMES),DSKIP

     IDATE=UTL_IDFGETDATE(IDFNAMES(J),DAYFRACTION,IY,IM,ID,IHR,IMT,ISC)
     IF(IDATE.NE.0)THEN
      DIDATE=YMDHMSTOITIME(IY,IM,ID,IHR,IMT,ISC)
      !## proper idf name and date is inside mindate/maxdate
      IF(DIDATE.NE.0.AND.(DIDATE.GE.MINDATETIME.AND.DIDATE.LE.MAXDATETIME))THEN
       K=K+1; NFILES(N)=NFILES(N)+1
       IF(IHR.EQ.0.AND.IMT.EQ.0.AND.ISC.EQ.0)THEN
       ENDIF
       LISTFILES(K,N)=MP(IPLOT)%IDFNAME(:I)//'\'//TRIM(IDFNAMES(J))
      ENDIF
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

! !## sort files for their date - probably not neccessary but to be sure ...
! DO I=1,N
!  CALL WSORT(LISTFILES(:,I),1,NFILES(I),IFLAGS=SORTNOCASE)
! ENDDO
 
 IDFTIMESERIE_DATES=.TRUE.

 END FUNCTION IDFTIMESERIE_DATES

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_GETMINMAXX(XMIN,XMAX,XINT,IFX,IDURATION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDURATION
 INTEGER,INTENT(OUT) :: IFX
 REAL(KIND=DP_KIND),INTENT(OUT) :: XMIN,XMAX,XINT
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

  CALL WDIALOGGETDOUBLE(IDF_REAL3,XINT)

 ELSE

  CALL WDIALOGGETDOUBLE(IDF_REAL6,XMIN)
  CALL WDIALOGGETDOUBLE(IDF_REAL5,XMAX)
  CALL WDIALOGGETDOUBLE(IDF_REAL7,XINT)

 ENDIF

 XINT=MAX(XINT,(XMAX-XMIN)/100.0D0)

 END SUBROUTINE IDFTIMESERIE_GETMINMAXX

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_GETMINMAXY(YMIN,YMAX,YINT,IFY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IFY
 REAL(KIND=DP_KIND),INTENT(OUT) :: YMIN,YMAX,YINT

 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,IFY)
 IF(IFY.EQ.0)RETURN

 !## get axes information on dialog
 CALL WDIALOGGETDOUBLE(IDF_REAL1,YMIN)
 CALL WDIALOGGETDOUBLE(IDF_REAL2,YMAX)
 CALL WDIALOGGETDOUBLE(IDF_REAL4,YINT)

 IF(YINT.LE.0.0D0) YINT=(YMAX-YMIN)/10.0D0

 END SUBROUTINE IDFTIMESERIE_GETMINMAXY

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_GETMINMAXY2(Y2MIN,Y2MAX,Y2INT,IFY2)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IFY2
 REAL(KIND=DP_KIND),INTENT(OUT) :: Y2MIN,Y2MAX,Y2INT

 CALL WDIALOGGETCHECKBOX(IDF_CHECK5,IFY2)
 IF(IFY2.EQ.0)RETURN

 !## get axes information on dialog
 CALL WDIALOGGETDOUBLE(IDF_REAL10,Y2MIN)
 CALL WDIALOGGETDOUBLE(IDF_REAL9 ,Y2MAX)
 CALL WDIALOGGETDOUBLE(IDF_REAL8 ,Y2INT)

 IF(Y2INT.LE.0.0D0)  Y2INT=(Y2MAX-Y2MIN)/10.0D0

 END SUBROUTINE IDFTIMESERIE_GETMINMAXY2

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_PUTMINMAXX(XMIN,XMAX,XINT,IDURATION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDURATION
 REAL(KIND=DP_KIND),INTENT(IN) :: XMIN,XMAX,XINT
 INTEGER :: IY,IM,ID,IDATE,IFX,NDAY
 REAL(KIND=DP_KIND) :: FTIME
 CHARACTER(LEN=12) :: CTIME
 
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IFX); IF(IFX.EQ.1)RETURN

 IF(IDURATION.EQ.0)THEN !## put axes information on dialog

!  CALL ITIMETOGDATE(INT(XMIN)+MINDATE,IY,IM,ID,IHR,IMT,ISC)

  IDATE=UTL_JDATETOIDATE(INT(XMIN)+MINDATE)
  CALL IDATETOGDATE(IDATE,IY,IM,ID)
  CALL WDIALOGPUTOPTION(IDF_MENU1,IM)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER2,IY)
  NDAY=WDATEDAYSINMONTH(IY,IM)
  CALL WDIALOGRANGEINTEGER(IDF_INTEGER1,1,NDAY)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER1,MIN(ID,NDAY))
  
  FTIME=XMIN-FLOOR(XMIN); CALL FTIMETOCTIME(FTIME,CTIME)
  CALL WDIALOGPUTSTRING(IDF_STRING4,TRIM(CTIME))
  
!  CALL ITIMETOGDATE(INT(XMAX)+MINDATE,IY,IM,ID,IHR,IMT,ISC)

  IDATE=UTL_JDATETOIDATE(INT(XMAX)+MINDATE)
  CALL IDATETOGDATE(IDATE,IY,IM,ID)
  CALL WDIALOGPUTOPTION(IDF_MENU2,IM)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER4,IY)

  NDAY=WDATEDAYSINMONTH(IY,IM)
  CALL WDIALOGRANGEINTEGER(IDF_INTEGER3,1,NDAY)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER3,MIN(ID,NDAY))
  CALL WDIALOGPUTDOUBLE(IDF_REAL3,XINT,'(F15.3)')

  FTIME=XMAX-FLOOR(XMAX); CALL FTIMETOCTIME(FTIME,CTIME)
  CALL WDIALOGPUTSTRING(IDF_STRING5,TRIM(CTIME))

 ELSE

  CALL WDIALOGPUTDOUBLE(IDF_REAL6,XMIN)
  CALL WDIALOGPUTDOUBLE(IDF_REAL5,XMAX)
  CALL WDIALOGPUTDOUBLE(IDF_REAL7,XINT)

 ENDIF

 END SUBROUTINE IDFTIMESERIE_PUTMINMAXX

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_PUTMINMAXY(YMIN,YMAX,YINT)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: YMIN,YMAX,YINT
 INTEGER :: IFY

 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,IFY); IF(IFY.EQ.1)RETURN

 CALL WDIALOGPUTDOUBLE(IDF_REAL1,YMIN)
 CALL WDIALOGPUTDOUBLE(IDF_REAL2,YMAX)
 CALL WDIALOGPUTDOUBLE(IDF_REAL4,YINT,'(F15.3)')

 END SUBROUTINE IDFTIMESERIE_PUTMINMAXY

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_PUTMINMAXY2(Y2MIN,Y2MAX,Y2INT)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: Y2MIN,Y2MAX,Y2INT
 INTEGER :: IFY2

 CALL WDIALOGGETCHECKBOX(IDF_CHECK5,IFY2); IF(IFY2.EQ.1)RETURN

 CALL WDIALOGPUTDOUBLE(IDF_REAL10,Y2MIN)
 CALL WDIALOGPUTDOUBLE(IDF_REAL9 ,Y2MAX)
 CALL WDIALOGPUTDOUBLE(IDF_REAL8 ,Y2INT,'(F15.3)')

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

