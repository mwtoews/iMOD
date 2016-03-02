!!  Copyright (C) Stichting Deltares, 2005-2016.
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
MODULE MOD_GENPLOT

USE WINTERACTER
USE RESOURCE
USE MODPLOT, ONLY : MPW,MP,MXMPLOT,DRWLIST
USE IMODVAR
USE MOD_UTL, ONLY : UTL_WAITMESSAGE,UTL_MESSAGEHANDLE,UTL_GETUNIT,ITOS,UTL_GETRELEVANTDIR,UTL_FILLARRAY,UTL_IDFGETCLASS,UTL_SETTEXTSIZE,&
  UTL_READARRAY,UTL_WSELECTFILE,RTOS,UTL_INSIDEPOLYGON,NV,NL,IVAR,VAR,VAR_TMP,IV,DVAR,MAXLEN,UTL_GENLABELSREAD,UTL_GENLABELSGET,UTL_GENLABELSDEALLOCATE, &
  UTL_GENLABELSWRITE
USE MOD_IPF, ONLY : IPFPLOTLABEL
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_GENPLOT_PAR
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_POLYGON_PAR

CONTAINS

 !###======================================================================
 SUBROUTINE TOPOGENINIT(GENNAME,LPLOT,LDEACTIVATE,GENCOLOUR)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: GENNAME
 LOGICAL,INTENT(IN),OPTIONAL :: LPLOT,LDEACTIVATE
 INTEGER,INTENT(IN),OPTIONAL :: GENCOLOUR
 INTEGER :: IPLOT,IGEN,NIGEN,I,J,K
 CHARACTER(LEN=2000) :: GENFNAME,GENLIST
 LOGICAL :: LEX

 IF(PRESENT(GENNAME))THEN
  GENFNAME=GENNAME
  INQUIRE(FILE=GENFNAME,EXIST=LEX)
  IF(.NOT.LEX)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Given file does not exist:'//CHAR(13)// &
     TRIM(GENFNAME),'Error')
   RETURN
  ENDIF
 ELSE
  GENFNAME=''
  !IF(LEN_TRIM(PREFVAL(3)).GT.0)GENFNAME=TRIM(PREFVAL(3))//'\'
  IF(.NOT.UTL_WSELECTFILE('All Possible Files (*.gen;*.shp;*.ipf)|*.gen;*.shp;*.ipf|ESRI Generate File (*.gen)|*.gen|'// &
      'ESRI Shape Files (*.shp)|*.shp|iMOD Point File (*.ipf)|*.ipf|',&
       LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT+MULTIFILE,GENFNAME,'Load background Map (*.gen,*.ipf,*.shp)'))RETURN
 ENDIF

 CALL UTL_MESSAGEHANDLE(0)

 K=INDEX(GENFNAME,CHAR(0))
 IF(K.GT.0)THEN
  GENLIST=GENFNAME
  NIGEN=0
  I=K+1
  DO WHILE(.TRUE.)
   J=INDEX(GENLIST(I:),CHAR(0))
   NIGEN=NIGEN+1
   IF(J.EQ.0)EXIT
   I=I+J
  END DO
 ELSE
  NIGEN=1
 ENDIF

 !## inactivate all
 IF(PRESENT(LDEACTIVATE))THEN
  GEN%ISEL=.NOT.LDEACTIVATE
 ELSE
  GEN%ISEL=.FALSE.
 ENDIF
 
 DO IGEN=1,NIGEN

  !## construct new name in multi-file selection mode
  IF(NIGEN.GT.1)THEN
   I=INDEX(GENLIST,CHAR(0))+1

   DO K=1,IGEN-1
    J=INDEX(GENLIST(I:),CHAR(0))
    I=I+J
   END DO

   J=INDEX(GENLIST(I:),CHAR(0))
   K=INDEX(GENLIST,CHAR(0))-1
   IF(J.EQ.0)THEN
    GENFNAME=GENLIST(:K)//'\'//GENLIST(I:)
   ELSE
    J=J+I
    GENFNAME=GENLIST(:K)//'\'//GENLIST(I:J-1)
   ENDIF
  ENDIF

  CALL IUPPERCASE(GENFNAME)

  !## check whether file already opened ... overwrite it otherwise
  DO IPLOT=1,MXGEN; IF(GEN(IPLOT)%IACT.AND.GEN(IPLOT)%GENFNAME.EQ.GENFNAME)EXIT; END DO
  !## get empty iplot-location
  IF(IPLOT.GT.MXGEN)THEN
   DO IPLOT=1,MXGEN; IF(.NOT.GEN(IPLOT)%IACT)EXIT; END DO
  ELSE
   GEN(IPLOT)%ISEL=.TRUE.
   CYCLE
  ENDIF

  !## determine what kind of file *.idf, *.ipf or *.shp
  I=INDEXNOCASE(GENFNAME,'.',.TRUE.)+1
  SELECT CASE (GENFNAME(I:I+2))
   CASE ('GEN')
    GEN(IPLOT)%ITYPE=1
   CASE ('IPF')
    GEN(IPLOT)%ITYPE=2
   CASE ('SHP')
    !## transform shp/dbf -> gen/dat
    IF(.NOT.TOPOSHPTOGEN(TRIM(GENFNAME)))CYCLE
    GEN(IPLOT)%ITYPE=1
    GENFNAME=GENFNAME(:INDEX(GENFNAME,'.',.TRUE.)-1)//'.GEN' 
   CASE DEFAULT
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not recognize current extension '//GENFNAME(I:I+2),'Error')
  END SELECT

  !## initialize plot-variables
  NGEN=NGEN
  GEN(IPLOT)%IACT      =.TRUE.               !## plot active
  GEN(IPLOT)%ISEL      =.TRUE.               !## selected
  GEN(IPLOT)%GENFNAME   =GENFNAME            !## name of the idf-file

  GEN(IPLOT)%SYMBOL=0     !## full-line
  GEN(IPLOT)%THICKNESS=1  !## default
  GEN(IPLOT)%XMIN=0.0
  GEN(IPLOT)%XMAX=1.0
  GEN(IPLOT)%YMIN=0.0
  GEN(IPLOT)%YMAX=1.0
  IF(PRESENT(GENCOLOUR))THEN
   GEN(IPLOT)%RGB=GENCOLOUR !black
  ELSE
   GEN(IPLOT)%RGB=WRGB(0,0,0) !black
  ENDIF
  
  !## increase number of active plots
  IF(NGEN.GE.MXGEN)EXIT

 ENDDO 

 CALL UTL_MESSAGEHANDLE(1)

 LEX=.TRUE.
 IF(PRESENT(LPLOT))LEX=LPLOT

 !## plot selected sets
 IF(LEX)CALL IDFPLOTFAST(0)

 !## update manager
 CALL TOPOGENFILL()
 CALL TOPOGENUPDATE()

 END SUBROUTINE TOPOGENINIT

 !###======================================================================
 SUBROUTINE TOPOGENFILL()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IGEN,IOPT

 CALL WDIALOGSELECT(ID_DMANAGERPROPERTIES)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IOPT)

 ACTGEN    =''
 ACTLISTGEN=0
 NGEN      =0
 DO IGEN=1,MXGEN
  IF(GEN(IGEN)%IACT)THEN
   NGEN=NGEN+1
   I=INDEXNOCASE(GEN(IGEN)%GENFNAME,'\',.TRUE.)+1
   SELECT CASE (IOPT)
    CASE (1)
     ACTGEN(NGEN)=GEN(IGEN)%GENFNAME(I:)
    CASE (2)
     ACTGEN(NGEN)=TRIM(GEN(IGEN)%GENFNAME(I:))//'     ('//GEN(IGEN)%GENFNAME(:I-2)//')'
    CASE (3,4)
     ACTGEN(NGEN)=GEN(IGEN)%GENFNAME
   END SELECT
   IF(GEN(IGEN)%ISEL)ACTLISTGEN(NGEN)=1
  ENDIF
 ENDDO

 IF(IOPT.EQ.4)CALL UTL_GETRELEVANTDIR(ACTGEN,NGEN)

 CALL WDIALOGSELECT(ID_DMANAGERTAB2)
 IF(NGEN.GT.0)THEN
  IF(WINFODIALOGFIELD(ID_DMTABMENU,FIELDSTATE).NE.1)CALL WDIALOGFIELDSTATE(ID_DMTABMENU,1)
  CALL WDIALOGPUTMENU(ID_DMTABMENU,ACTGEN,NGEN,ACTLISTGEN)
 ELSE
  CALL WDIALOGCLEARFIELD(ID_DMTABMENU)
  IF(WINFODIALOGFIELD(ID_DMTABMENU,FIELDSTATE).NE.2)CALL WDIALOGFIELDSTATE(ID_DMTABMENU,2)
 ENDIF

 END SUBROUTINE TOPOGENFILL

 !###======================================================================
 SUBROUTINE TOPOGENUPDATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IGEN,I,J,NOGEN,NOIPF 

 CALL WDIALOGSELECT(ID_DMANAGERTAB2)

 GEN%ISEL=.FALSE.
 ACTLISTGEN=0
 NOGEN=0
 NOIPF=0
 IF(NGEN.GT.0)THEN
  CALL WDIALOGGETMENU(ID_DMTABMENU,ACTLISTGEN)
  DO IGEN=1,MXGEN
   IF(ACTLISTGEN(IGEN).EQ.1)THEN
    GEN(IGEN)%ISEL=.TRUE.
    IF(GEN(IGEN)%ITYPE.EQ.1)NOGEN=NOGEN+1
    IF(GEN(IGEN)%ITYPE.EQ.2)NOIPF=NOIPF+1
   ENDIF
  END DO
 ENDIF

 I=1
 IF(NGEN.GE.MXGEN)I=0
 CALL WDIALOGSELECT(ID_DMANAGERTAB2)
 CALL WDIALOGFIELDSTATE(ID_OPEN,I)

 I=0
 IF(SUM(ACTLISTGEN).GT.0)I=1
 IF(WINFODIALOGFIELD(ID_DRAW,FIELDSTATE).NE.I)  CALL WDIALOGFIELDSTATE(ID_DRAW,I)
 IF(WINFODIALOGFIELD(ID_DELETE,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_DELETE,I)
 IF(SUM(ACTLISTGEN).GT.1)I=0
 IF(WINFODIALOGFIELD(ID_LEGEND,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_LEGEND,I)

! I=0
! IF(NOGEN.EQ.1.AND.SUM(ACTLISTGEN).EQ.1)I=1
! IF(WMENUGETSTATE(ID_GENOPTIONS,1).NE.I)CALL WMENUSETSTATE(ID_GENOPTIONS,1,I)

 !## movement of gen in data manager
 I=0
 J=0
 IF(SUM(ACTLISTGEN).GT.0)THEN
  IF(NGEN.GT.1.AND.NGEN.LT.MXGEN)THEN
   IF(.NOT.GEN(1)%ISEL)   I=1
   IF(.NOT.GEN(NGEN)%ISEL)J=1
  ENDIF
 ENDIF
 IF(WINFODIALOGFIELD(ID_MOVEUP,FIELDSTATE).NE.I)  CALL WDIALOGFIELDSTATE(ID_MOVEUP,I)
 IF(WINFODIALOGFIELD(ID_MOVEDOWN,FIELDSTATE).NE.J)CALL WDIALOGFIELDSTATE(ID_MOVEDOWN,J)

 END SUBROUTINE TOPOGENUPDATE

 !###======================================================================
 SUBROUTINE TOPOGENDELETE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT,JPLOT

 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to remove the selected files from the iMOD manager?','Question')
 IF(WINFODIALOG(4).NE.1)RETURN

 !###delete all selections
 IPLOT=1
 DO WHILE(IPLOT.LE.MXGEN)
 !## remove plot
  IF(GEN(IPLOT)%ISEL)THEN
   DO JPLOT=IPLOT,MXGEN-1
    GEN(JPLOT)       =GEN(JPLOT+1)
    ACTLISTGEN(JPLOT)=ACTLISTGEN(JPLOT+1)
   END DO
   ACTLISTGEN(MXGEN)=0
   GEN(MXGEN)%ISEL=.FALSE.
   GEN(MXGEN)%IACT=.FALSE.
  ELSE
   IPLOT=IPLOT+1
  ENDIF
 END DO

 CALL TOPOGENFILL()
 CALL TOPOGENUPDATE()

 END SUBROUTINE TOPOGENDELETE

 !###======================================================================
 SUBROUTINE TOPOGENMOVE(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER            :: IPLOT

 IF(ID.EQ.ID_MOVEUP)THEN
  DO IPLOT=1,MXGEN
   IF(GEN(IPLOT)%ISEL)THEN
    GEN(MXGEN)  =GEN(IPLOT-1)
    GEN(IPLOT-1)=GEN(IPLOT)
    GEN(IPLOT)  =GEN(MXGEN)
   ENDIF
  ENDDO
 ELSEIF(ID.EQ.ID_MOVEDOWN)THEN
  DO IPLOT=MXGEN-1,1,-1
   IF(GEN(IPLOT)%ISEL)THEN
    GEN(MXGEN)  =GEN(IPLOT+1)
    GEN(IPLOT+1)=GEN(IPLOT)
    GEN(IPLOT)  =GEN(MXGEN)
   ENDIF
  ENDDO
 ENDIF
 
 GEN(MXGEN)%IACT=.FALSE.; GEN(MXGEN)%ISEL=.FALSE.
 CALL TOPOGENFILL(); CALL TOPOGENUPDATE()

 END SUBROUTINE TOPOGENMOVE

 !###======================================================================
 SUBROUTINE TOPOGENDRAW(IWIN)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IWIN
 INTEGER :: IU,IGEN,IOS
 CHARACTER(LEN=10) :: GEOM
 LOGICAL :: LEX

 DO IGEN=1,MXGEN
  IF(GEN(IGEN)%IACT.AND.GEN(IGEN)%ISEL)THEN

   INQUIRE(FILE=GEN(IGEN)%GENFNAME,EXIST=LEX)
   IF(LEX)THEN
    IU=UTL_GETUNIT()
    CALL WINDOWSELECT(IWIN)
    CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(GEN(IGEN)%GENFNAME)//'...')

    !## gens
    IF(GEN(IGEN)%ITYPE.EQ.1)THEN

     CALL OSD_OPEN(IU,FILE=GEN(IGEN)%GENFNAME,STATUS='OLD',FORM='FORMATTED',ACTION='READ,DENYWRITE', &
       IOSTAT=IOS,ACCESS='SEQUENTIAL')
     IF(IOS.NE.0)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not open/read '//CHAR(13)//TRIM(GEN(IGEN)%GENFNAME),'Error')
      EXIT
     ENDIF
     CALL TOPOGENPLOT(IU,IGEN)

    !## ipf
    ELSEIF(GEN(IGEN)%ITYPE.EQ.2)THEN

     CALL OSD_OPEN(IU,FILE=GEN(IGEN)%GENFNAME,STATUS='OLD',FORM='FORMATTED',ACTION='READ,DENYWRITE', &
       IOSTAT=IOS,ACCESS='SEQUENTIAL')
     IF(IOS.NE.0)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not open/read '//CHAR(13)//TRIM(GEN(IGEN)%GENFNAME),'Error')
      EXIT
     ENDIF
     CALL TOPOIPFPLOT(IU,IGEN)

    ENDIF

    CLOSE(IU)
    CALL WINDOWOUTSTATUSBAR(4,'')
    
   ENDIF
  ENDIF
 ENDDO

!#default settings
 CALL IGRCOLOURN(WRGB(0,0,0))
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRLINEWIDTH(1)

 CALL WINDOWSELECT(IWIN)
 CALL WINDOWOUTSTATUSBAR(4,'')

 END SUBROUTINE TOPOGENDRAW
 
 !###======================================================================
 SUBROUTINE TOPOGENPLOT(IU,IGEN)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,IGEN
 INTEGER :: IOS,ITEL,I,JTEL
 REAL :: FROMX,FROMY,TOX,TOY,RADIUS,X,Y
 CHARACTER(LEN=256) :: LINE
 
 RADIUS=SQRT((MPW%XMAX-MPW%XMIN)**2.0+(MPW%YMAX-MPW%YMIN)**2.0)/250.0

 CALL IGRCOLOURN(GEN(IGEN)%RGB)
 CALL IGRLINETYPE(GEN(IGEN)%SYMBOL)
 CALL IGRLINEWIDTH(GEN(IGEN)%THICKNESS)

 GEN(IGEN)%XMIN= 10.0E10; GEN(IGEN)%YMIN= 10.0E10
 GEN(IGEN)%XMAX=-10.0E10; GEN(IGEN)%YMAX=-10.0E10

 DO
  READ(IU,*,IOSTAT=IOS); IF(IOS.NE.0)EXIT

  !## read first point
  READ(IU,*,IOSTAT=IOS) FROMX,FROMY; IF(IOS.NE.0)EXIT
  DO
   READ(IU,*,IOSTAT=IOS) TOX,TOY; IF(IOS.NE.0)EXIT
   CALL IGRJOIN(FROMX,FROMY,TOX,TOY)
   GEN(IGEN)%XMIN=MIN(TOX,GEN(IGEN)%XMIN); GEN(IGEN)%YMIN=MIN(TOY,GEN(IGEN)%YMIN)
   GEN(IGEN)%XMAX=MAX(TOX,GEN(IGEN)%XMAX); GEN(IGEN)%YMAX=MAX(TOY,GEN(IGEN)%YMAX)
   FROMX=TOX; FROMY=TOY
  ENDDO
 ENDDO

 END SUBROUTINE TOPOGENPLOT

 !###===============================================================================
 SUBROUTINE TOPOIPFPLOT(IU,IGEN)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,IGEN
 INTEGER :: NROW,NCOL,I,IOS
 REAL :: X,Y

 IF(GEN(IGEN)%SYMBOL.LE.0.OR.GEN(IGEN)%SYMBOL.GT.40)GEN(IGEN)%SYMBOL=14

 !## read header
 READ(IU,*) NROW
 READ(IU,*) NCOL
 DO I=1,NCOL+1; READ(IU,*); ENDDO

 CALL IGRCOLOURN(GEN(IGEN)%RGB)
 CALL IGRLINETYPE(SOLIDLINE)
 I=GEN(IGEN)%THICKNESS
 CALL WGRTEXTFONT(WIDTH=(REAL(I)*.4)/75.,HEIGHT=(REAL(I)*0.2)/25.)

 GEN(IGEN)%XMIN= 10.0E10; GEN(IGEN)%YMIN= 10.0E10
 GEN(IGEN)%XMAX=-10.0E10; GEN(IGEN)%YMAX=-10.0E10
 
 DO I=1,NROW
  READ(IU,*,IOSTAT=IOS) X,Y; IF(IOS.NE.0)EXIT
  IF(X.GE.MPW%XMIN.AND.X.LE.MPW%XMAX.AND.Y.GE.MPW%YMIN.AND.Y.LE.MPW%YMAX)CALL IGRMARKER(X,Y,GEN(IGEN)%SYMBOL)
  GEN(IGEN)%XMIN=MIN(X,GEN(IGEN)%XMIN); GEN(IGEN)%YMIN=MIN(Y,GEN(IGEN)%YMIN)
  GEN(IGEN)%XMAX=MAX(X,GEN(IGEN)%XMAX); GEN(IGEN)%YMAX=MAX(Y,GEN(IGEN)%YMAX)
 ENDDO

 END SUBROUTINE TOPOIPFPLOT

 !###===============================================================================
 LOGICAL FUNCTION TOPOGENTOSHP(FNAME)
 !###===============================================================================
! USE fortranc
! USE gdal 
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 
 TOPOGENTOSHP=.FALSE.
 TOPOGENTOSHP=.TRUE.
 
 END FUNCTION TOPOGENTOSHP
 
 !###===============================================================================
 LOGICAL FUNCTION TOPOSHPTOGEN(FNAME)
 !###===============================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: I,J,K,IOS,IREC
 INTEGER,DIMENSION(2) :: IU,JU
 DOUBLE PRECISION :: X,Y
 DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: MP,ZP !XP,YP,
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IP
 INTEGER :: NUMPARTS,NUMPOINTS,I1,I2,MAXNPOINTS
 INTEGER :: ICODE,ILEN,IVERSION,ISHP
 INTEGER,DIMENSION(5) :: INOTUSED
 DOUBLE PRECISION :: XMIN,YMIN,XMAX,YMAX,ZMIN,ZMAX,MMIN,MMAX
 CHARACTER(LEN=1256) :: LINE,STRING
 TYPE DBFTYPE
  CHARACTER(LEN=11) :: COLNAME
  CHARACTER(LEN=1) :: COLTYPE
  INTEGER :: COLWIDTH
  INTEGER :: COLDEC
  INTEGER :: COLOFF
 END TYPE DBFTYPE
 INTEGER(KIND=SELECTED_INT_KIND(3)) :: LHEAD,LENREC  ! =INTEGER*2
 TYPE(DBFTYPE),ALLOCATABLE,DIMENSION(:) :: DBF
 INTEGER :: MAXCOL,INDFLD,DATAOFF
 CHARACTER(LEN=52),ALLOCATABLE,DIMENSION(:) :: CID
 
 INTEGER :: NRECS,IOFFSET
 CHARACTER(LEN=1) :: VERSION,YEAR,MONTH,DAY
 CHARACTER(LEN=4) :: CA
 CHARACTER :: CWIDTH, CDEC
 
 TOPOSHPTOGEN=.FALSE.
 
 IU=0
 JU=0
 
 !## open shapefile
 IU(1)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(1),FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED',ACTION='READ,DENYWRITE',ACCESS='TRANSPARENT',IOSTAT=IOS) !,RECLEN=1)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not open/read '//CHAR(13)//TRIM(FNAME),'Error')
  GOTO 10
 ENDIF
 !## open dbf file accompanying the shapefile
 IU(2)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(2),FILE=FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.dbf',STATUS='OLD',FORM='UNFORMATTED', &
   ACTION='READ,DENYWRITE',ACCESS='TRANSPARENT',IOSTAT=IOS)
 IF(IOS.NE.0)IU(2)=0
 !## open gen file
 JU(1)=UTL_GETUNIT()
 CALL OSD_OPEN(JU(1),FILE=FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.gen',ACTION='WRITE',STATUS='UNKNOWN',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not open/read '//CHAR(13)//FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.gen','Error')
  GOTO 12
 ENDIF
 IF(IU(2).GT.0)THEN
  !## open dat file accompanying the genfile
  JU(2)=UTL_GETUNIT()
  CALL OSD_OPEN(JU(2),FILE=FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.dat',ACTION='WRITE',STATUS='UNKNOWN',FORM='FORMATTED',IOSTAT=IOS)
  IF(IOS.NE.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not open/read '//CHAR(13)//FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.dat','Error')
   GOTO 13
  ENDIF
 ENDIF
 
 READ(IU(1)) ICODE
 READ(IU(1)) INOTUSED(1)
 READ(IU(1)) INOTUSED(2)
 READ(IU(1)) INOTUSED(3)
 READ(IU(1)) INOTUSED(4)
 READ(IU(1)) INOTUSED(5)
 READ(IU(1)) ILEN
 READ(IU(1)) IVERSION
 READ(IU(1)) ISHP
 READ(IU(1)) XMIN
 READ(IU(1)) XMAX
 READ(IU(1)) YMIN
 READ(IU(1)) YMAX
 READ(IU(1)) ZMIN
 READ(IU(1)) ZMAX
 READ(IU(1)) MMIN
 READ(IU(1)) MMAX

 IF(IU(2).GT.0)THEN
  !## 1+1+1+1+4+2+2=12
  READ(IU(2)) VERSION,YEAR,MONTH,DAY,NRECS,LHEAD,LENREC
  MAXCOL=(LHEAD-32)/32
  IF(ALLOCATED(DBF))DEALLOCATE(DBF); ALLOCATE(DBF(MAXCOL))
  IF(ALLOCATED(CID))DEALLOCATE(CID); ALLOCATE(CID(NRECS))
  
  !## read remaining bytes
  READ(IU(2)) (DAY,I=1,20)
   
  IOFFSET=1
  DO I=1,MAXCOL
   !## is 15 bytes 11+1+4+1+1=18
   READ(IU(2)) DBF(I)%COLNAME,DBF(I)%COLTYPE,CA,CWIDTH,CDEC
   DO J=1,11; IF(ICHAR(DBF(I)%COLNAME(J:J)).EQ.0)DBF(I)%COLNAME(J:J)=CHAR(32); ENDDO
   DBF(I)%COLWIDTH=ICHAR(CWIDTH)
   DBF(I)%COLDEC  =ICHAR(CDEC)
   DBF(I)%COLOFF  =IOFFSET
   IOFFSET        =IOFFSET+DBF(I)%COLWIDTH
   !## read remaining bytes
   READ(IU(2)) (DAY,J=1,32-18) 
  ENDDO

  !## write header in dat-file
  LINE=CHAR(39)//TRIM(DBF(1)%COLNAME)//CHAR(39)
  DO I=2,MAXCOL; LINE=TRIM(LINE)//','//CHAR(39)//TRIM(ADJUSTL(DBF(I)%COLNAME))//CHAR(39); ENDDO
  WRITE(JU(2),'(A)') TRIM(LINE)
    
  !## read remaining bytes
  REWIND(IU(2))
  DATAOFF=32*MAXCOL+35-1
  READ(IU(2))(DAY,J=1,DATAOFF)
  DO IREC=1,NRECS
   LINE=''
   DO I=1,MAXCOL
    STRING=''
    READ(IU(2)) STRING(1:DBF(I)%COLWIDTH)    
    IF(I.NE.1)LINE=TRIM(LINE)//','//CHAR(39)//TRIM(ADJUSTL(STRING(1:DBF(I)%COLWIDTH)))//CHAR(39)
    IF(I.EQ.1)THEN
     CID(IREC)=STRING(1:DBF(I)%COLWIDTH)
     LINE=CHAR(39)//TRIM(ADJUSTL(STRING(1:DBF(I)%COLWIDTH)))//CHAR(39)
    ENDIF
   ENDDO
   READ(IU(2),IOSTAT=IOS) DAY 
   IF(IOS.NE.0)EXIT
   WRITE(JU(2),'(A)') TRIM(LINE)
  ENDDO
 ENDIF
 
 K=0
 DO
  READ(IU(1),IOSTAT=IOS) I1,I2; IF(IOS.NE.0)EXIT
  READ(IU(1),IOSTAT=IOS) ISHP;  IF(IOS.NE.0)EXIT
  SELECT CASE (ISHP)
   !## polyline(3),polygon(5),PolylineZ(13),PolygonZ(15),PolylineM(23)
   CASE (3,5,13,15,23)
    READ(IU(1)) XMIN
    READ(IU(1)) YMIN
    READ(IU(1)) XMAX
    READ(IU(1)) YMAX

    READ(IU(1)) NUMPARTS,NUMPOINTS
    IF(ALLOCATED(IP))THEN; IF(SIZE(IP).LT.NUMPARTS+1)DEALLOCATE(IP); ENDIF
    IF(.NOT.ALLOCATED(IP))ALLOCATE(IP(NUMPARTS+1))
    
    IF(ISHP.EQ.13.OR.ISHP.EQ.15)THEN
     IF(ALLOCATED(ZP))THEN; IF(SIZE(ZP).LT.NUMPOINTS)DEALLOCATE(ZP); ENDIF
     IF(.NOT.ALLOCATED(ZP))ALLOCATE(ZP(NUMPOINTS))
    ENDIF

    IF(ISHP.EQ.23)THEN
     IF(ALLOCATED(MP))THEN; IF(SIZE(MP).LT.NUMPOINTS)DEALLOCATE(MP); ENDIF
     IF(.NOT.ALLOCATED(MP))ALLOCATE(MP(NUMPOINTS))
    ENDIF
         
    READ(IU(1)) (IP(I),I=1,NUMPARTS)
    IP(NUMPARTS+1)=NUMPOINTS
    
    K=K+1   
    DO I=1,NUMPARTS
     WRITE(JU(1),'(A,A4,I3.3)') TRIM(CID(K)),',part',I
     DO J=IP(I),IP(I+1)-1
      READ(IU(1)) X,Y
      WRITE(JU(1),*) X,Y
     ENDDO
     IF(ISHP.EQ.13.OR.ISHP.EQ.15)WRITE(JU(1),'(A3)') 'END'
     WRITE(JU(1),'(A3)') 'END'
    ENDDO

    IF(ISHP.EQ.13.OR.ISHP.EQ.15)THEN
     READ(IU(1)) ZMIN
     READ(IU(1)) ZMAX
     DO I=1,NUMPOINTS; READ(IU(1)) ZP(I); ENDDO
    ENDIF

    IF(ISHP.EQ.23)THEN
     READ(IU(1)) MMIN
     READ(IU(1)) MMAX
     DO I=1,NUMPOINTS; READ(IU(1)) MP(I); ENDDO
    ENDIF
    
   !## points -> gen
   CASE (1)    
    READ(IU(1)) X,Y
    K=K+1
    WRITE(JU(1),*) CID(K),X,Y

   !## multipoints -> gen
   CASE (8)
    READ(IU(1)) XMIN
    READ(IU(1)) YMIN
    READ(IU(1)) XMAX
    READ(IU(1)) YMAX

    READ(IU(1)) NUMPOINTS
    DO I=1,NUMPOINTS
     READ(IU(1)) X,Y
     K=K+1
     WRITE(JU(1),'(A)') K,X,Y
    ENDDO

   CASE DEFAULT
    CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'Shape '//TRIM(ITOS(ISHP))//' not supported yet!','Error')
    EXIT !RETURN
  END SELECT
 ENDDO

 IF(ALLOCATED(IP))DEALLOCATE(IP)
 IF(ALLOCATED(MP))DEALLOCATE(MP)

 WRITE(JU(1),'(A3)') 'END'

 TOPOSHPTOGEN=.TRUE.
 
13 IF(JU(2).GT.0)CLOSE(JU(2))
12 IF(JU(1).GT.0)CLOSE(JU(1))
11 IF(IU(2).GT.0)CLOSE(IU(2))
10 IF(IU(1).GT.0)CLOSE(IU(1))
 
 END FUNCTION TOPOSHPTOGEN

 !###======================================================================
 SUBROUTINE GENDRAW(IPOLYGON)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPOLYGON
 INTEGER :: IPLOT,I 
 LOGICAL :: LEX

 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.6)THEN

   INQUIRE(FILE=MP(IPLOT)%IDFNAME,EXIST=LEX)
   IF(LEX)THEN

    !## file exists
    CALL WINDOWSELECT(0)
    CALL IGRAREA(0.0,0.0,1.0,1.0)
    CALL IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)
    CALL GENDRAWPLOT(IPLOT,IPOLYGON)
    DRWLIST(IPLOT)=1

   ENDIF

  ENDIF
 END DO

 END SUBROUTINE GENDRAW

 !###======================================================================
 SUBROUTINE GENDRAWPLOT(IPLOT,IPOLYGON)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPLOT,IPOLYGON
 INTEGER :: IOS,MAXP,N,ID,JL,IU
 LOGICAL :: LEX
 CHARACTER(LEN=52) :: CID !STRING
 REAL :: TWIDTH,THEIGHT,XP,YP,RADIUS
 
 IF(MP(IPLOT)%PRFTYPE.NE.IPOLYGON)RETURN
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=MP(IPLOT)%IDFNAME,STATUS='OLD',FORM='FORMATTED',ACTION='READ,DENYWRITE', &
       ACCESS='SEQUENTIAL',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not open/read file:'//CHAR(13)// &
   TRIM(MP(IPLOT)%IDFNAME),'Error')
  RETURN
 ENDIF
 
 CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(MP(IPLOT)%IDFNAME)//'...')
  
 !## make sure variables are in correct bandwidth
 IF(MP(IPLOT)%SCOLOR.LE.0)MP(IPLOT)%SCOLOR=WRGB(100,100,100)
 IF(MP(IPLOT)%IATTRIB.LE.0)MP(IPLOT)%IATTRIB=1

 NV=0; NL=0

 !## open associated dat files in case of abs(ieq).eq.1.or.ileg.eq.1
 IF(ABS(MP(IPLOT)%IEQ).GE.1.OR.MP(IPLOT)%ILEG.EQ.1)THEN
  !## reading labels
  CALL UTL_GENLABELSREAD(MP(IPLOT)%IDFNAME (:INDEX(MP(IPLOT)%IDFNAME,'.',.TRUE.)-1)//'.dat' )
 ENDIF
 IF(NV.GT.0)THEN
  ALLOCATE(IVAR(NV))
  !## see what labels are selected for plotting purposes
  CALL UTL_FILLARRAY(IVAR,NV,ABS(MP(IPLOT)%IEQ))
  !## determine textsize
  CALL UTL_SETTEXTSIZE(TWIDTH,THEIGHT,REAL(MP(IPLOT)%TSIZE)*0.01)!2)!TSIZE)
  IF(MP(IPLOT)%IATTRIB.GT.NV)MP(IPLOT)%IATTRIB=NV  !## label used for colouring
 ENDIF

 RADIUS=SQRT((MPW%XMAX-MPW%XMIN)**2.0+(MPW%YMAX-MPW%YMIN)**2.0)/250.0

 MP(IPLOT)%XMIN= 10.0E10
 MP(IPLOT)%YMIN= 10.0E10
 MP(IPLOT)%XMAX=-10.0E10
 MP(IPLOT)%YMAX=-10.0E10
 
 NULLIFY(X,Y,XDUMMY,YDUMMY)
 
 MAXP=0
 
 DO

  !## header
  READ(IU,'(A)',IOSTAT=IOS) CID
  IF(IOS.NE.0)EXIT
  CID=ADJUSTL(CID)

!  READ(STRING,'(A)',IOSTAT=IOS) CID !,XP,YP
  
  IOS=-1 !## nooit point 
  !## point
  IF(IOS.EQ.0)THEN
   !## extent for current set of polygon(s)
   MP(IPLOT)%XMIN=MIN(XP,MP(IPLOT)%XMIN)
   MP(IPLOT)%YMIN=MIN(YP,MP(IPLOT)%YMIN)
   MP(IPLOT)%XMAX=MAX(XP,MP(IPLOT)%XMAX)
   MP(IPLOT)%YMAX=MAX(YP,MP(IPLOT)%YMAX)
   IF(XP.GT.MPW%XMIN.AND.XP.LT.MPW%XMAX.AND. &
      YP.GT.MPW%YMIN.AND.YP.LT.MPW%YMAX)THEN
    !## get proper label for id
    CALL UTL_GENLABELSGET(CID,JL)
    !## get colour of point (e.g. grey or based upon selected attribute)
    CALL IGRCOLOURN(GENLABELGETCOLOR(JL,IPLOT))
    IF(MP(IPLOT)%PRFTYPE.EQ.0)CALL IGRFILLPATTERN(OUTLINE)
    IF(MP(IPLOT)%PRFTYPE.EQ.1)CALL IGRFILLPATTERN(SOLID)
    CALL IGRLINETYPE(MP(IPLOT)%SYMBOL); CALL IGRLINEWIDTH(MP(IPLOT)%THICKNESS)
    CALL IGRCIRCLE(XP,YP,RADIUS)
    !## put labels if jl.ne.0
    CALL IGRCOLOURN(WRGB(0,0,0)); CALL IGRFILLPATTERN(OUTLINE); CALL IGRLINETYPE(SOLIDLINE); CALL IGRLINEWIDTH(1)
    IF(JL.GT.0.AND.MP(IPLOT)%IEQ.NE.0)THEN
     CALL IPFPLOTLABEL(XP,YP,VAR(:,JL),IVAR,NV,TWIDTH,THEIGHT,VAR(:,0),.FALSE.,MP(IPLOT)%IEQ)
    ENDIF
!    CALL GENLABELSPLOT(JL,N,IPLOT,TWIDTH,THEIGHT) 
   ENDIF
    
  ELSE

   N=0
   DO

    N=N+1

    !## increase memory
    IF(N.GT.MAXP)THEN
     ALLOCATE(XDUMMY(MAXP+1000),YDUMMY(MAXP+1000))
     IF(MAXP.GT.0)THEN
      XDUMMY(1:MAXP)=X
      YDUMMY(1:MAXP)=Y
      DEALLOCATE(X,Y)
     ENDIF
     X=>XDUMMY
     Y=>YDUMMY
     MAXP=MAXP+1000
     NULLIFY(XDUMMY,YDUMMY)
    ENDIF

    READ(IU,*,IOSTAT=IOS) X(N),Y(N)
    !## read 'end' (should be!)
    IF(IOS.NE.0)EXIT

   END DO
  
   !## actual number of polygon vertices
   N=N-1

   !## extent for current set of polygon(s)
   MP(IPLOT)%XMIN=MIN(MINVAL(X(1:N)),MP(IPLOT)%XMIN)
   MP(IPLOT)%YMIN=MIN(MINVAL(Y(1:N)),MP(IPLOT)%YMIN)
   MP(IPLOT)%XMAX=MAX(MAXVAL(X(1:N)),MP(IPLOT)%XMAX)
   MP(IPLOT)%YMAX=MAX(MAXVAL(Y(1:N)),MP(IPLOT)%YMAX)

   LEX=.FALSE.

   IF(MAXVAL(X(1:N)).GT.MPW%XMIN.AND.MINVAL(X(1:N)).LT.MPW%XMAX.AND. &
      MAXVAL(Y(1:N)).GT.MPW%YMIN.AND.MINVAL(Y(1:N)).LT.MPW%YMAX)LEX=.TRUE. 

   !## if at least one point of a polygon is inside the map, then draw the polygon
   IF(LEX)THEN
    !## get proper label for id
    CALL UTL_GENLABELSGET(CID,JL)
    !## get colour of polygon (e.g. grey or based upon selected attribute)
    CALL IGRCOLOURN(GENLABELGETCOLOR(JL,IPLOT))

    IF(MP(IPLOT)%PRFTYPE.EQ.0)CALL IGRFILLPATTERN(OUTLINE)
    IF(MP(IPLOT)%PRFTYPE.EQ.1)CALL IGRFILLPATTERN(SOLID)
    CALL IGRLINETYPE(MP(IPLOT)%SYMBOL); CALL IGRLINEWIDTH(MP(IPLOT)%THICKNESS)
    IF(X(1).NE.X(N).OR.Y(1).NE.Y(N))THEN
     CALL IGRPOLYLINE(X,Y,N)
    ELSE
     CALL IGRPOLYGONCOMPLEX(X,Y,N)
    ENDIF
    !## put labels if jl.ne.0
    CALL IGRCOLOURN(WRGB(0,0,0)); CALL IGRFILLPATTERN(OUTLINE); CALL IGRLINETYPE(SOLIDLINE); CALL IGRLINEWIDTH(1)
    CALL GENLABELSPLOT(JL,N,IPLOT,TWIDTH,THEIGHT) 
   ENDIF
  
  ENDIF
 END DO
 
 CLOSE(IU)
 
 IF(ASSOCIATED(X))DEALLOCATE(X); IF(ASSOCIATED(Y))DEALLOCATE(Y)
 CALL UTL_GENLABELSDEALLOCATE()

 !## default settings
 CALL IGRCOLOURN(WRGB(0,0,0)); CALL IGRFILLPATTERN(OUTLINE); CALL WINDOWOUTSTATUSBAR(4,'')
 CALL IGRLINETYPE(SOLIDLINE); CALL IGRLINEWIDTH(1)
 
 END SUBROUTINE GENDRAWPLOT

 !###======================================================================
 INTEGER FUNCTION GENLABELGETCOLOR(JL,IPLOT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: JL,IPLOT
 REAL :: X
 INTEGER :: I

 IF(MP(IPLOT)%ILEG.EQ.0)THEN
  GENLABELGETCOLOR=MP(IPLOT)%SCOLOR
 ELSE
  I=INFOERROR(1)
  CALL ISTRINGTOREAL(VAR(MP(IPLOT)%IATTRIB,JL),X)
  GENLABELGETCOLOR=WRGB(200,200,200)
  IF(INFOERROR(1).EQ.0)THEN
   GENLABELGETCOLOR=UTL_IDFGETCLASS(MP(IPLOT)%LEG,X)
   MP(IPLOT)%UMIN=MIN(MP(IPLOT)%UMIN,X)
   MP(IPLOT)%UMAX=MAX(MP(IPLOT)%UMAX,X)
  ENDIF
 ENDIF

 END FUNCTION GENLABELGETCOLOR

 !###======================================================================
 SUBROUTINE GENLABELSPLOT(JL,N,IPLOT,TWIDTH,THEIGHT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: JL,N,IPLOT
 REAL,INTENT(IN) :: TWIDTH,THEIGHT
 REAL :: XC,YC,X1,Y1,X2,Y2,INT
 INTEGER :: I,J
 
 IF(JL.LE.0)RETURN
 IF(MP(IPLOT)%IEQ.EQ.0)RETURN

 XC= ( MAXVAL(X(1:N)) + MINVAL(X(1:N)) ) / 2.0
 YC= ( MAXVAL(Y(1:N)) + MINVAL(Y(1:N)) ) / 2.0
 
 !## check whether point is inside polygon, if not try others!
 IF(UTL_INSIDEPOLYGON(XC,YC,X,Y,N).NE.1)THEN
  INT=10
DOLOOP: DO
   X1=MINVAL(X(1:N))
   Y1=MINVAL(Y(1:N))
   X2=MAXVAL(X(1:N))
   Y2=MAXVAL(Y(1:N))
   DX=(X2-X1)/INT
   DY=(Y2-Y1)/INT
   DO I=1,INT
    YC=Y1+DY
    DO J=1,INT
     XC=X1+DX     
     IF(UTL_INSIDEPOLYGON(XC,YC,X,Y,N).NE.1)EXIT DOLOOP
    ENDDO  
   ENDDO
   INT=INT+10
  ENDDO DOLOOP
 ENDIF

 CALL IPFPLOTLABEL(XC,YC,VAR(:,JL),IVAR,NV,TWIDTH,THEIGHT,VAR(:,0),.FALSE.,MP(IPLOT)%IEQ)

 END SUBROUTINE GENLABELSPLOT

 !###======================================================================
 SUBROUTINE GENLABELSDEFINE(IPLOT,ATTRIB,NATTRIB)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPLOT,NATTRIB
 CHARACTER(LEN=*),DIMENSION(NATTRIB),INTENT(IN) :: ATTRIB
 INTEGER :: ITYPE,I
 INTEGER,DIMENSION(:),ALLOCATABLE :: ILIST
 TYPE(WIN_MESSAGE) :: MESSAGE

 CALL WDIALOGLOAD(ID_DIPFLABELS,ID_DIPFLABELS)
 CALL WDIALOGFIELDSTATE(IDF_GROUP2,0)
 CALL WDIALOGFIELDSTATE(IDF_LABEL5,0)
 CALL WDIALOGFIELDSTATE(IDF_LABEL6,0)
 CALL WDIALOGFIELDSTATE(IDF_INTEGER2,0)
 CALL WDIALOGFIELDSTATE(IDF_INTEGER3,0) 
 
 CALL WDIALOGFIELDSTATE(IDF_GROUP3,0)
 CALL WDIALOGFIELDSTATE(IDF_LABEL3,0)
 CALL WDIALOGFIELDSTATE(IDF_LABEL4,0)
 CALL WDIALOGFIELDSTATE(IDF_LABEL7,0)
 CALL WDIALOGFIELDSTATE(IDF_REAL1,0)
 CALL WDIALOGFIELDSTATE(IDF_INTEGER1,0)
 CALL WDIALOGFIELDSTATE(IDF_CHECK3,0)     
 CALL WDIALOGFIELDSTATE(IDF_CHECK4,0)     
 CALL WDIALOGFIELDSTATE(IDF_MENU3,0)
  
 ALLOCATE(ILIST(NATTRIB)) 

 ILIST=0
 IF(ABS(MP(IPLOT)%IEQ).GT.0)THEN
  CALL UTL_FILLARRAY(ILIST,NATTRIB,ABS(MP(IPLOT)%IEQ))
 ENDIF
 CALL WDIALOGPUTMENU(IDF_MENU1,ATTRIB,NATTRIB,ILIST)   !## plot
 IF(MP(IPLOT)%IEQ.LT.0)CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,0)  !## use colouring for labels
 IF(MP(IPLOT)%IEQ.GE.0)CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,1)  !## use colouring for labels
 MP(IPLOT)%TSIZE=MIN(MAX(MP(IPLOT)%TSIZE,1),10)
 CALL WDIALOGPUTOPTION(IDF_MENU2,MP(IPLOT)%TSIZE)  !textsize
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,IBACKSLASH)

 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDHELP)
       CALL IMODGETHELP('4.2.1','MMO.IPO.IPFCon') 
     CASE (IDOK)
      CALL WDIALOGGETMENU(IDF_MENU1,ILIST)
      CALL UTL_READARRAY(ILIST,NATTRIB,MP(IPLOT)%IEQ)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)  !## use colouring for labels
      IF(I.EQ.0)MP(IPLOT)%IEQ=-1*MP(IPLOT)%IEQ
      CALL WDIALOGGETMENU(IDF_MENU2,MP(IPLOT)%TSIZE)  !second z
      CALL WDIALOGGETCHECKBOX(IDF_CHECK2,IBACKSLASH)
      EXIT
     CASE (IDCANCEL)
      EXIT
    END SELECT
  END SELECT
 ENDDO

 CALL WDIALOGUNLOAD()
 DEALLOCATE(ILIST)

 END SUBROUTINE GENLABELSDEFINE
 
 !###======================================================================
 SUBROUTINE GENDATAGRID(FNAME) 
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: ITYPE,I,J,K,IU,ICOL,IROW,NLL,NP
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER,ALLOCATABLE,DIMENSION(:) :: ICOLS
 CHARACTER(LEN=MAXLEN) :: LABELNAME
  
 NP=0; DO I=1,SHPNO; IF(SHPTYPE(I).NE.ID_POINT)NP=NP+1; ENDDO 
 
 !## reading labels
 IF(LEN_TRIM(FNAME).GT.0)THEN
  CALL UTL_GENLABELSREAD(FNAME) 
  IF(NV.LE.0.OR.NL.LE.0.OR..NOT.ASSOCIATED(VAR))RETURN
 ELSE
  IF(.NOT.ASSOCIATED(VAR))THEN
   CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONYES,'Do you want to add additional data to the shapes?','Question')
   IF(WINFODIALOG(4).NE.1)RETURN
   NV=3; NL=SHPNO; ALLOCATE(VAR(NV,0:NL)); VAR=''
   !## not all equal to points
   IF(NP.NE.0)THEN
    VAR(1,0)='ShapeID'; VAR(2,0)='ShapeType'; VAR(3,0)='Variable'
   ELSE
    VAR(1,0)='X-crd (UTM)'; VAR(2,0)='Y-crd (UTM)'; VAR(3,0)='Variable'
   ENDIF
   DO I=1,SHPNO  
    SELECT CASE (SHPTYPE(I))
     CASE (ID_POLYGON); VAR(1,I)=TRIM(ITOS(SHPID(I)));   VAR(2,I)='Polygon'
     CASE (ID_POINT);   VAR(1,I)=RTOS(SHPXC(1,I),'F',3); VAR(2,I)=RTOS(SHPYC(1,I),'F',3)
     CASE (ID_LINE);    VAR(1,I)=TRIM(ITOS(SHPID(I)));   VAR(2,I)='Lines'
    END SELECT
    VAR(3,I)=TRIM(SHPNAME(I))
   ENDDO
  ELSE
   !## update coordinates for points
   DO I=1,SHPNO  
    SELECT CASE (SHPTYPE(I))
     CASE (ID_POINT)
      VAR(1,I)=RTOS(SHPXC(1,I),'F',2); VAR(2,I)=RTOS(SHPYC(1,I),'F',2)
    END SELECT
   ENDDO
  ENDIF
 ENDIF
 
 !## copy of dbase
 ALLOCATE(DVAR(NV,0:NL)); DVAR=VAR
 
 !## nothing selected, take everything
 NLL=SUM(SHPIACT(1:SHPNO)); IF(NLL.EQ.0)SHPIACT=1; NLL=SUM(SHPIACT(1:SHPNO))
 
 CALL WDIALOGLOAD(ID_DGENDATA)
 CALL WDIALOGTITLE('Content of file: '//TRIM(FNAME))
 IF(NLL.GT.WINFOGRID(IDF_GRID1,GRIDROWSMAX))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not display all data ('//TRIM(ITOS(NL))//' records) in:'//CHAR(13)// &
  TRIM(FNAME)//'.'//CHAR(13)//'Only first '//TRIM(ITOS(WINFOGRID(IDF_GRID1,GRIDROWSMAX)))//&
  ' records will be displayed','Error')
 ENDIF

 ALLOCATE(ICOLS(NV)); ICOLS=1; CALL WGRIDCOLUMNS(IDF_GRID1,NV,ICOLS); DEALLOCATE(ICOLS)
 CALL WGRIDROWS(IDF_GRID1,NLL)
 CALL WDIALOGPUTIMAGE(ID_PLUS,ID_ICONPLUS)
 CALL WDIALOGPUTIMAGE(ID_MIN,ID_ICONMIN)
 CALL WDIALOGPUTIMAGE(ID_RENAME,ID_ICONRENAME)
 
 !## put labels
 DO I=1,NV; CALL WGRIDLABELCOLUMN(IDF_GRID1,I,TRIM(VAR(I,0))); END DO
 K=0; DO I=1,NL; IF(SHPIACT(I).EQ.0)CYCLE; K=K+1; DO J=1,NV; CALL WGRIDPUTCELLSTRING(IDF_GRID1,J,K,TRIM(VAR(J,I))); END DO; END DO
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,MAX(0,MIN(IV,1)))
 IV=MIN(NV,MAX(1,IV)); CALL WDIALOGPUTMENU(IDF_MENU1,VAR(:,0),NV,IV)
 
 !## outgrey the first column, they are used internally
 IF(NP.NE.0)THEN
  CALL WGRIDSTATE(IDF_GRID1,1,0)
  CALL WGRIDSTATE(IDF_GRID1,2,0)
 ENDIF
 
 ICOL=0; CALL WDIALOGSHOW(-1,-1,0,3)
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_GRID1)
      CALL WGRIDPOS(MESSAGE%Y,ICOL,IROW)     
    END SELECT
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (ID_PLUS)
      CALL WDIALOGLOAD(ID_POLYGONSHAPENAME,ID_POLYGONSHAPENAME)
      CALL WDIALOGSHOW(-1,-1,0,3)
      CALL WDIALOGPUTSTRING(IDF_LABEL1,'Give an attribute name')
      CALL WDIALOGPUTSTRING(IDF_STRING1,'Attribute'//TRIM(ITOS(NV+1)))
      DO
       CALL WMESSAGE(ITYPE,MESSAGE)
       SELECT CASE (ITYPE)
        CASE (PUSHBUTTON)
         SELECT CASE (MESSAGE%VALUE1)
          CASE (IDOK,IDCANCEL)
           CALL WDIALOGGETSTRING(IDF_STRING1,LABELNAME); EXIT
         END SELECT
       END SELECT
      ENDDO
      CALL WDIALOGUNLOAD(); CALL WDIALOGSELECT(ID_DGENDATA)
      K=0; DO I=1,NL; IF(SHPIACT(I).EQ.0)CYCLE; K=K+1; DO J=2,NV; CALL WGRIDGETCELLSTRING(IDF_GRID1,J,K,VAR(J,I)); ENDDO; ENDDO
      ALLOCATE(VAR_TMP(NV+1,0:NL)); VAR_TMP(1:NV,0:NL)=VAR(1:NV,0:NL); DEALLOCATE(VAR); VAR=>VAR_TMP
      NV=NV+1; VAR(NV,:)=''; VAR(NV,0)=TRIM(LABELNAME) 
      CALL WDIALOGCLEARFIELD(IDF_GRID1)
      ALLOCATE(ICOLS(NV)); ICOLS=1; CALL WGRIDCOLUMNS(IDF_GRID1,NV,ICOLS); DEALLOCATE(ICOLS)
      DO I=1,NV; CALL WGRIDLABELCOLUMN(IDF_GRID1,I,TRIM(VAR(I,0))); END DO
      K=0; DO I=1,NL; IF(SHPIACT(I).EQ.0)CYCLE; K=K+1; DO J=1,NV; CALL WGRIDPUTCELLSTRING(IDF_GRID1,J,K,TRIM(VAR(J,I))); END DO; END DO
      CALL WDIALOGPUTMENU(IDF_MENU1,VAR(:,0),NV,IV)

     CASE (ID_MIN)
      CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to remove the attribute column '//TRIM(VAR(ICOL,0)),'Question')
      IF(WINFODIALOG(4).EQ.1)THEN
       K=0; DO I=1,NL; IF(SHPIACT(I).EQ.0)CYCLE; K=K+1 ;DO J=2,NV; CALL WGRIDGETCELLSTRING(IDF_GRID1,J,K,VAR(J,I)); ENDDO; ENDDO
       ALLOCATE(VAR_TMP(NV-1,0:NL))
       K=1
       DO I=1,NV
        IF(I.NE.ICOL)THEN; DO J=0,NL; VAR_TMP(K,J)=VAR(I,J); ENDDO; K=K+1; ENDIF
       ENDDO
       DEALLOCATE(VAR); VAR=>VAR_TMP
       CALL WDIALOGCLEARFIELD(IDF_GRID1)
       NV=NV-1; ALLOCATE(ICOLS(NV)); ICOLS=1; CALL WGRIDCOLUMNS(IDF_GRID1,NV,ICOLS); DEALLOCATE(ICOLS)
       DO I=1,NV; CALL WGRIDLABELCOLUMN(IDF_GRID1,I,TRIM(VAR(I,0))); END DO
       K=0; DO I=1,NL; IF(SHPIACT(I).EQ.0)CYCLE; K=K+1; DO J=1,NV; CALL WGRIDPUTCELLSTRING(IDF_GRID1,J,K,TRIM(VAR(J,I))); END DO; END DO
       CALL WDIALOGPUTMENU(IDF_MENU1,VAR(:,0),NV,IV)
      ENDIF
     CASE (ID_RENAME)
      IF(ICOL.LE.0)THEN
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Select a column to rename it','Warning')
      ELSE
       CALL WDIALOGLOAD(ID_POLYGONSHAPENAME,ID_POLYGONSHAPENAME)
       CALL WDIALOGSHOW(-1,-1,0,3)
       CALL WDIALOGPUTSTRING(IDF_LABEL1,'Rename Attribute')
       CALL WDIALOGPUTSTRING(IDF_STRING1,TRIM(VAR(ICOL,0)))
       DO
        CALL WMESSAGE(ITYPE,MESSAGE)
        SELECT CASE (ITYPE)
         CASE (PUSHBUTTON)
          SELECT CASE (MESSAGE%VALUE1)
           CASE (IDOK,IDCANCEL)
            CALL WDIALOGGETSTRING(IDF_STRING1,VAR(ICOL,0)); EXIT
          END SELECT
        END SELECT
       ENDDO
       CALL WDIALOGUNLOAD(); CALL WDIALOGSELECT(ID_DGENDATA)
       DO I=1,NV; CALL WGRIDLABELCOLUMN(IDF_GRID1,I,TRIM(VAR(I,0))); END DO 
       CALL WDIALOGPUTMENU(IDF_MENU1,VAR(:,0),NV,IV)
      ENDIF
     CASE (IDHELP)
      CALL IMODGETHELP('4.1.4','MMO.IDO.IDFEdit')
     !## get data
     CASE (IDOK)
      CALL WDIALOGGETMENU(IDF_MENU1,IV)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I); IV=IV*I
      K=0; DO I=1,NL; IF(SHPIACT(I).EQ.0)CYCLE; K=K+1; DO J=2,NV; CALL WGRIDGETCELLSTRING(IDF_GRID1,J,K,VAR(J,I)); END DO; END DO; EXIT
     CASE (IDCANCEL)
      DEALLOCATE(VAR); NV=SIZE(DVAR,1); NL=SIZE(DVAR,2)-1
      ALLOCATE(VAR(NV,0:NL)); VAR=DVAR
      EXIT
    END SELECT
  END SELECT
 ENDDO
 DEALLOCATE(DVAR); CALL WDIALOGUNLOAD()

! !## write dat file so it will goes right with zoom-full extent etc.
! IF(ASSOCIATED(VAR))CALL UTL_GENLABELSWRITE(FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.dat' )

 END SUBROUTINE GENDATAGRID

END MODULE MOD_GENPLOT
