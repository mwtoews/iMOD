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
MODULE MOD_GENPLOT

USE WINTERACTER
USE RESOURCE
USE MOD_COLOURS
USE MOD_DBL
USE MODPLOT, ONLY : MPW,MP,MXMPLOT,DRWLIST
USE IMODVAR, ONLY : DP_KIND,SP_KIND,IBACKSLASH
USE MOD_UTL
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_GENPLOT_PAR
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_POLYGON_PAR
USE MOD_POLYGON_UTL

CONTAINS

 !###======================================================================
 SUBROUTINE GEN_INIT(GENNAME,LDEACTIVATE,GENCOLOUR,GENTHICKNESS)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: GENNAME
 LOGICAL,INTENT(IN),OPTIONAL :: LDEACTIVATE
 INTEGER,INTENT(IN),OPTIONAL :: GENCOLOUR,GENTHICKNESS
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
  IF(.NOT.UTL_WSELECTFILE('All Possible Files (*.gen)|*.gen|iMOD/ESRI GEN File (*.gen)|*.gen|',&
       LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT+MULTIFILE,GENFNAME,'Load background Map (*.gen)'))RETURN
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

  GENFNAME=UTL_CAP(GENFNAME,'U')

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
   CASE DEFAULT
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot recognize current extension '//GENFNAME(I:I+2),'Error')
  END SELECT

  !## initialize plot-variables
  NGEN=NGEN
  GEN(IPLOT)%IACT      =.TRUE.               !## plot active
  GEN(IPLOT)%ISEL      =.TRUE.               !## selected
  GEN(IPLOT)%GENFNAME   =GENFNAME            !## name of the idf-file

  GEN(IPLOT)%SYMBOL=0     !## full-line
  IF(PRESENT(GENTHICKNESS))THEN
   GEN(IPLOT)%THICKNESS=GENTHICKNESS !thickness
  ELSE
   GEN(IPLOT)%THICKNESS=1  !## default
  ENDIF
  GEN(IPLOT)%XMIN=0.0D0
  GEN(IPLOT)%XMAX=1.0D0
  GEN(IPLOT)%YMIN=0.0D0
  GEN(IPLOT)%YMAX=1.0D0
  IF(PRESENT(GENCOLOUR))THEN
   GEN(IPLOT)%RGB=GENCOLOUR !black
  ELSE
   GEN(IPLOT)%RGB=WRGB(0,0,0) !black
  ENDIF
  GEN(IPLOT)%TSIZE=12
  GEN(IPLOT)%TFORMAT='F10.2'
  GEN(IPLOT)%ILABELS=0
  GEN(IPLOT)%IFILL=0
    
  !## increase number of active plots
  IF(NGEN.GE.MXGEN)EXIT

 ENDDO 

 CALL UTL_MESSAGEHANDLE(1)

 !## update manager
 CALL GEN_FILL()
 CALL GEN_UPDATE()

 END SUBROUTINE GEN_INIT

 !###======================================================================
 SUBROUTINE GEN_FILL()
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
    CASE (1,5)
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

 END SUBROUTINE GEN_FILL

 !###======================================================================
 SUBROUTINE GEN_UPDATE()
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

 END SUBROUTINE GEN_UPDATE

 !###======================================================================
 SUBROUTINE GEN_DELETE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT,JPLOT

 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to remove the selected files from the iMOD manager?','Question')
 IF(WINFODIALOG(4).NE.1)RETURN

 !## delete all selections
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

 CALL GEN_FILL()
 CALL GEN_UPDATE()

 END SUBROUTINE GEN_DELETE

 !###======================================================================
 SUBROUTINE GEN_MOVE(ID)
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
 CALL GEN_FILL(); CALL GEN_UPDATE()

 END SUBROUTINE GEN_MOVE

 !###======================================================================
 SUBROUTINE GEN_DRAW(IWIN)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IWIN
 INTEGER :: IU,IGEN,IFORMAT
 LOGICAL :: LEX
 
 DO IGEN=1,MXGEN
  IF(GEN(IGEN)%IACT.AND.GEN(IGEN)%ISEL)THEN

   INQUIRE(FILE=GEN(IGEN)%GENFNAME,EXIST=LEX)
   IF(LEX)THEN

    CALL WINDOWSELECT(IWIN); CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(GEN(IGEN)%GENFNAME)//'...')

    IF(POLYGON_UTL_OPENGEN(GEN(IGEN)%GENFNAME,IFORMAT,IU))THEN
    
     !## gens
     IF(GEN(IGEN)%ITYPE.EQ.1)CALL GEN_PLOT(IU,IFORMAT,GEN(IGEN)%XMIN,GEN(IGEN)%YMIN,GEN(IGEN)%XMAX,GEN(IGEN)%YMAX, &
         GEN(IGEN)%RGB,GEN(IGEN)%IFILL,GEN(IGEN)%SYMBOL,GEN(IGEN)%THICKNESS,GEN(IGEN)%ILABELS,0,TSIZE=GEN(IGEN)%TSIZE,TFORMAT=GEN(IGEN)%TFORMAT)

     CLOSE(IU)
    
    ENDIF
    CALL WINDOWOUTSTATUSBAR(4,'')
    
   ELSE
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot find '//CHAR(13)//TRIM(GEN(IGEN)%GENFNAME),'Warning')
   ENDIF
  ENDIF
 ENDDO

 !## default settings
 CALL IGRCOLOURN(WRGB(0,0,0))
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRLINEWIDTH(1)

 CALL WINDOWSELECT(IWIN)
 CALL WINDOWOUTSTATUSBAR(4,'')

 END SUBROUTINE GEN_DRAW
 
 !###======================================================================
 SUBROUTINE GENDRAW()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT,IFORMAT,IU
 LOGICAL :: LEX

 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.6)THEN

   INQUIRE(FILE=MP(IPLOT)%IDFNAME,EXIST=LEX)
   IF(LEX)THEN

    IF(POLYGON_UTL_OPENGEN(MP(IPLOT)%IDFNAME,IFORMAT,IU))THEN
     CALL GEN_PLOT(IU,IFORMAT,MP(IPLOT)%XMIN,MP(IPLOT)%YMIN,MP(IPLOT)%XMAX,MP(IPLOT)%YMAX, &
                MP(IPLOT)%SCOLOR,MP(IPLOT)%PRFTYPE,MP(IPLOT)%SYMBOL,MP(IPLOT)%THICKNESS,MP(IPLOT)%IEQ,MP(IPLOT)%ILEG, &
                IATTR=MP(IPLOT)%IATTRIB,TSIZE=MP(IPLOT)%TSIZE,TFORMAT=MP(IPLOT)%TFORMAT,LEG=MP(IPLOT)%LEG, &
                UMIN=MP(IPLOT)%DMIN,UMAX=MP(IPLOT)%DMAX)

!    MP(IPLOT)%IEQ    =0    !no value plotted <--- used as binaire pointer for label plotting
!    MP(IPLOT)%ILEG   =0    !no legend used for plotting, use colour in %scolor
!    MP(IPLOT)%IATTRIB=1    !initial first label for colouring
!    MP(IPLOT)%SCOLOR =WRGB(100,100,100)! single - colour    !no colouring, attribute colouring
!    MP(IPLOT)%TSIZE  =2    !textsize
!    MP(IPLOT)%PRFTYPE=0    !filled in (0=no,1=yes)
!    MP(IPLOT)%THICKNESS=1  !line thickness

     DRWLIST(IPLOT)=1
     CLOSE(IU)
    ENDIF
    
   ENDIF

  ENDIF
 END DO

 END SUBROUTINE GENDRAW
 
! !###======================================================================
! LOGICAL FUNCTION POLYGON_UTL_OPENGEN(FNAME,IFORMAT,IU)
! !###======================================================================
! IMPLICIT NONE
! CHARACTER(LEN=256) :: FNAME
! INTEGER,INTENT(OUT) :: IFORMAT,IU
! INTEGER :: IOS
! REAL(KIND=DP_KIND) :: X
! 
! POLYGON_UTL_OPENGEN=.FALSE.
!
! IU=UTL_GETUNIT()
!
! IFORMAT=0
! CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED',ACTION='READ,DENYWRITE',IOSTAT=IOS)
! READ(IU,IOSTAT=IOS) X; IF(IOS.NE.0)IFORMAT=1; CLOSE(IU)
!     
! IF(IFORMAT.EQ.1)THEN 
!  CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='FORMATTED',ACTION='READ,DENYWRITE',IOSTAT=IOS)
! ELSEIF(IFORMAT.EQ.0)THEN 
!  CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED',ACTION='READ,DENYWRITE',IOSTAT=IOS)
! ELSE
!  IOS=-1
! ENDIF
! IF(IOS.NE.0)THEN
!  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open/read '//CHAR(13)//TRIM(FNAME),'Error')
!  RETURN
! ENDIF
! 
! POLYGON_UTL_OPENGEN=.TRUE.
! 
! END FUNCTION POLYGON_UTL_OPENGEN

! !###======================================================================
! SUBROUTINE POLYGON_UTL_GETLABELSGEN(IU,IFORMAT,LABELS)
! !###======================================================================
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: IU,IFORMAT
! CHARACTER(LEN=11),DIMENSION(:),POINTER,INTENT(OUT) :: LABELS
! INTEGER :: MAXPOL,MAXCOL,I,J
! 
! IF(ASSOCIATED(LABELS))DEALLOCATE(LABELS)
!
! !## binary format
! IF(IFORMAT.EQ.0)THEN
!  READ(IU)
!  READ(IU) MAXPOL,MAXCOL
!  ALLOCATE(LABELS(MAXCOL))
!  READ(IU) (J,I=1,MAXCOL)
!  READ(IU) (LABELS(I),I=1,MAXCOL)
! ENDIF
!  
! CLOSE(IU)
! 
! END SUBROUTINE POLYGON_UTL_GETLABELSGEN
 
 !###======================================================================
 SUBROUTINE GEN_PLOT(IU,IFORMAT,XMIN,YMIN,XMAX,YMAX,ICLR,IFILL,ISYMBOL,ITHK,ILABELS,ILEG,IATTR,TSIZE,LEG,UMIN,UMAX,TFORMAT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,IFORMAT,ITHK,ILABELS,ILEG,IFILL,ISYMBOL
 INTEGER,INTENT(INOUT) :: ICLR
 INTEGER,INTENT(INOUT),OPTIONAL :: IATTR,TSIZE
 REAL(DP_KIND),INTENT(INOUT),OPTIONAL :: UMIN,UMAX
 TYPE(LEGENDOBJ),INTENT(IN),OPTIONAL :: LEG
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: TFORMAT
 REAL(KIND=DP_KIND),INTENT(OUT) :: XMIN,YMIN,XMAX,YMAX
 INTEGER :: I,J,IOS,MAXCOL,N,STRLEN,MAXPOL,ITYPE,JCLR,JLABEL
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IVAR
 REAL(KIND=DP_KIND) :: FROMX,FROMY,TOX,TOY,TWIDTH,THEIGHT,X,XMID,YMID,RAT
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: XC,YC
 CHARACTER(LEN=11),ALLOCATABLE,DIMENSION(:) :: COLNAMES
 LOGICAL :: LEX
 INTEGER,ALLOCATABLE,DIMENSION(:) :: COLWIDTH
 TYPE LBLOBJ
  CHARACTER(LEN=:),ALLOCATABLE :: STRING
 END TYPE LBLOBJ
 TYPE(LBLOBJ),DIMENSION(:),ALLOCATABLE :: LBL
 CHARACTER(LEN=128),ALLOCATABLE,DIMENSION(:) :: ECHO
 CHARACTER(LEN=52) :: CID
 CHARACTER(LEN=256) :: LINE
 
 IF(ICLR.LE.0)ICLR=WRGB(100,100,100)

 CALL IGRCOLOURN(ICLR)
 CALL IGRLINETYPE(ISYMBOL)
 CALL IGRLINEWIDTH(ITHK)

 !## ascii format - no labeling possible
 IF(IFORMAT.EQ.1)THEN
 
  XMIN= HUGE(1.0D0); YMIN= HUGE(1.0D0)
  XMAX=-HUGE(1.0D0); YMAX=-HUGE(1.0D0)

  DO
   READ(IU,'(A256)',IOSTAT=IOS) LINE
   !## check whether gen file is point
   READ(LINE,*,IOSTAT=IOS) CID,FROMX,FROMY
   !## next point
   IF(IOS.EQ.0)THEN
    XMIN=MIN(FROMX,XMIN); YMIN=MIN(FROMY,YMIN)
    XMAX=MAX(FROMX,XMAX); YMAX=MAX(FROMY,YMAX)
    CALL UTL_PLOTPOINT(FROMX,FROMY,14,1.0D0,.FALSE.)
    CYCLE
   ENDIF
   READ(LINE,*,IOSTAT=IOS) CID; IF(IOS.NE.0)EXIT
   !## read first point
   READ(IU,*,IOSTAT=IOS) FROMX,FROMY; IF(IOS.NE.0)EXIT
   DO
    READ(IU,*,IOSTAT=IOS) TOX,TOY; IF(IOS.NE.0)EXIT
    IF(MIN(FROMX,TOX).LT.MPW%XMAX.AND. &
       MAX(FROMX,TOX).GT.MPW%XMIN.AND. & 
       MIN(FROMY,TOY).LT.MPW%YMAX.AND. &
       MAX(FROMY,TOY).GT.MPW%YMIN)THEN
     CALL DBL_IGRJOIN(FROMX,FROMY,TOX,TOY,IOFFSET=1)
    ENDIF
    XMIN=MIN(TOX,XMIN); YMIN=MIN(TOY,YMIN)
    XMAX=MAX(TOX,XMAX); YMAX=MAX(TOY,YMAX)
    FROMX=TOX; FROMY=TOY
   ENDDO
  ENDDO
 
 !## binary format
 ELSEIF(IFORMAT.EQ.0)THEN
 
  !## read overall extent of polygons
  READ(IU) XMIN,YMIN,XMAX,YMAX
  !## some polygons are within current zoomwindow
  IF(XMAX.GE.MPW%XMIN.AND.XMIN.LE.MPW%XMAX.AND. & 
     YMAX.GE.MPW%YMIN.AND.YMIN.LE.MPW%YMAX)THEN   

   READ(IU) MAXPOL,MAXCOL
   !## labels available
   IF(MAXCOL.GT.0)THEN

    ALLOCATE(COLWIDTH(MAXCOL),LBL(MAXCOL),COLNAMES(MAXCOL))
    READ(IU) (COLWIDTH(I),I=1,MAXCOL)
    !## read column names
    DO I=1,MAXCOL; STRLEN=COLWIDTH(I); ALLOCATE(CHARACTER(LEN=STRLEN) :: LBL(I)%STRING); ENDDO
    READ(IU) (COLNAMES(I),I=1,MAXCOL)

    !## plot labels
    IF(ABS(ILABELS).GE.1.OR.ILEG.EQ.1)THEN
     !## see what labels are selected for plotting purposes
     ALLOCATE(IVAR(MAXCOL)); CALL UTL_FILLARRAY(IVAR,MAXCOL,ABS(ILABELS))
     !## determine textsize
     CALL UTL_SETTEXTSIZE(TWIDTH,THEIGHT,FCT=REAL(TSIZE,8)) 
     !## label used for colouring
     IF(PRESENT(IATTR))THEN
      IF(IATTR.LE.0)IATTR=1; IF(IATTR.GT.MAXCOL)IATTR=MAXCOL
     ENDIF
     ALLOCATE(ECHO(MAXCOL)); ECHO=''
    ENDIF

   ENDIF

!open(99,file='d:\tmp.ipf',status='unknown',action='write')

   DO J=1,MAXPOL
    READ(IU) N,ITYPE
    !## read labels
    IF(MAXCOL.GT.0)READ(IU) (LBL(I)%STRING,I=1,MAXCOL)
    IF(ALLOCATED(XC))THEN; IF(N.GT.SIZE(XC))DEALLOCATE(XC,YC); ENDIF
    IF(.NOT.ALLOCATED(XC))ALLOCATE(XC(N),YC(N))

    READ(IU) FROMX,FROMY,TOX,TOY

    LEX=.FALSE.
    IF(MIN(FROMX,TOX).LT.MPW%XMAX.AND. &
       MAX(FROMX,TOX).GT.MPW%XMIN.AND. & 
       MIN(FROMY,TOY).LT.MPW%YMAX.AND. &
       MAX(FROMY,TOY).GT.MPW%YMIN)LEX=.TRUE.  
   
    READ(IU) (XC(I),YC(I),I=1,N)

    !## skip this one
    IF(.NOT.LEX)CYCLE
        
    IF(IFILL.EQ.0)CALL IGRFILLPATTERN(OUTLINE); IF(IFILL.EQ.1)CALL IGRFILLPATTERN(SOLID); CALL IGRLINETYPE(ISYMBOL); CALL IGRLINEWIDTH(ITHK)

    IF(ILEG.EQ.1)THEN
     READ(LBL(IATTR)%STRING,*,IOSTAT=IOS) X
     IF(IOS.EQ.0)THEN
      JCLR=UTL_IDFGETCLASS(LEG,X)
      CALL IGRCOLOURN(JCLR)    
      UMIN=MIN(UMIN,X); UMAX=MAX(UMAX,X)
     ELSE
      CALL IGRCOLOURN(WRGB(255,255,255))    
     ENDIF
    ELSE
     CALL IGRCOLOURN(ICLR)    
    ENDIF
    
    IF(ITYPE.EQ.ID_POLYGON)THEN
     CALL DBL_IGRPOLYGONSIMPLE(XC,YC,N,IOFFSET=1)
    ELSEIF(ITYPE.EQ.ID_LINE)THEN
     CALL DBL_IGRMOVETO(XC(1),YC(1),IOFFSET=1)
     DO I=2,N; CALL DBL_IGRLINETO(XC(I),YC(I),IOFFSET=1); ENDDO
    ELSEIF(ITYPE.EQ.ID_POINT)THEN
     DO I=1,N; CALL UTL_PLOTPOINT(XC(I),YC(I),14,1.0D0,.FALSE.); ENDDO
    ELSEIF(ITYPE.EQ.ID_RECTANGLE)THEN
     CALL DBL_IGRRECTANGLE(XC(1),YC(1),XC(2),YC(2),IOFFSET=1)    
    ELSEIF(ITYPE.EQ.ID_CIRCLE)THEN
     RAT=(XC(1)-XC(2))**2.0D0+(YC(1)-YC(2))**2.0D0
     IF(RAT.GT.0.0D0)THEN
      RAT=SQRT(RAT); CALL DBL_IGRCIRCLE(XC(1),YC(1),RAT,IOFFSET=1)
     ENDIF
    ENDIF
    
    CALL IGRCOLOURN(WRGB(0,0,0)); CALL IGRFILLPATTERN(OUTLINE); CALL IGRLINETYPE(SOLIDLINE); CALL IGRLINEWIDTH(1)

    IF(ABS(ILABELS).GE.1)THEN
     IF(ITYPE.EQ.ID_POINT.OR.ITYPE.EQ.ID_CIRCLE)THEN
      XMID=XC(1); YMID=YC(1)
     ELSEIF(ITYPE.EQ.ID_RECTANGLE)THEN
      XMID=(XC(1)+XC(2))/2.0D0
      YMID=(YC(1)+YC(2))/2.0D0
     ELSEIF(ITYPE.EQ.ID_LINE)THEN
      XMID=XC(1)
      YMID=YC(1)
     ELSEIF(ITYPE.EQ.ID_POLYGON)THEN
      CALL UTL_GETMIDPOINT(XC,YC,N,XMID,YMID)
     ELSE
      XMID=XC(1)
      YMID=YC(1)
     ENDIF
     DO I=1,MAXCOL
      ECHO(I)=''; ECHO(I)=ADJUSTL(LBL(I)%STRING)
     ENDDO
     JLABEL=ILABELNAME; ILABELNAME=1
     IF(PRESENT(TFORMAT))THEN
      CALL UTL_PLOTLABEL(XMID,YMID,ECHO,IVAR,MAXCOL,TWIDTH,THEIGHT,COLNAMES,.FALSE.,ILABELS,ALIGNLEFT,CFORMAT=TFORMAT) !'(F10.2)')
     ELSE
      CALL UTL_PLOTLABEL(XMID,YMID,ECHO,IVAR,MAXCOL,TWIDTH,THEIGHT,COLNAMES,.FALSE.,ILABELS,ALIGNLEFT,CFORMAT='(F10.2)')
     ENDIF
     ILABELNAME=JLABEL
    ENDIF
    
   ENDDO
!   close(99)
   
  ENDIF
  
  IF(ALLOCATED(IVAR))DEALLOCATE(IVAR)
  IF(ALLOCATED(XC))DEALLOCATE(XC); IF(ALLOCATED(YC))DEALLOCATE(YC)
  IF(ALLOCATED(COLWIDTH))DEALLOCATE(COLWIDTH)
  IF(ALLOCATED(COLNAMES))DEALLOCATE(COLNAMES)
  IF(ALLOCATED(ECHO))DEALLOCATE(ECHO)
  IF(ALLOCATED(LBL))THEN; DO I=1,MAXCOL; DEALLOCATE(LBL(I)%STRING); ENDDO; DEALLOCATE(LBL); ENDIF

 ENDIF
 
 END SUBROUTINE GEN_PLOT
 
 !###===============================================================================
 SUBROUTINE TOPOSHPTOGEN()
 !###===============================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,IOS,IREC,III
 INTEGER,DIMENSION(2) :: IU
 INTEGER :: JU
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: MP,ZP 
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IP
 INTEGER :: NUMPARTS,NUMPOINTS,I1,I2
 INTEGER :: ICODE,ILEN,IVERSION,ISHP
 INTEGER,DIMENSION(5) :: INOTUSED
 REAL(KIND=DP_KIND) :: XMIN,YMIN,XMAX,YMAX,ZMIN,ZMAX,MMIN,MMAX,Z,M
 REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: XC,YC
 CHARACTER(LEN=256) :: FNAME
 CHARACTER(LEN=11),ALLOCATABLE,DIMENSION(:) :: COLNAMES
 TYPE DBFTYPE
  CHARACTER(LEN=:),ALLOCATABLE :: STRING
 END TYPE DBFTYPE
 INTEGER(KIND=SELECTED_INT_KIND(3)) :: LHEAD,LENREC  
 INTEGER :: COLDEC,MAXPOL
 CHARACTER(LEN=1) :: COLTYPE
 INTEGER :: COLOFF
 
 TYPE(DBFTYPE),ALLOCATABLE,DIMENSION(:,:) :: DBF
 INTEGER,ALLOCATABLE,DIMENSION(:) :: COLWIDTH
 
 INTEGER :: MAXCOL,DATAOFF,N,II
 INTEGER :: NRECS,IOFFSET,STRLEN
 CHARACTER(LEN=1) :: VERSION,YEAR,MONTH,DAY
 CHARACTER(LEN=4) :: CA
 CHARACTER :: CWIDTH, CDEC
  
 FNAME=''
 IF(.NOT.UTL_WSELECTFILE('ESRI Shape File (*.shp)|*.shp|',&
       LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,'Load ESRI Shape File (*.shp)'))RETURN

 IU=0; JU=0

 !## open shapefile
 IU(1)=UTL_GETUNIT(); CALL OSD_OPEN(IU(1),FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED',ACTION='READ,DENYWRITE',ACCESS='TRANSPARENT',IOSTAT=IOS) 
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'IMOD CANNOT READ '//CHAR(13)//TRIM(FNAME),'ERROR'); RETURN
 ENDIF
 !## open dbf file accompanying the shapefile
 IU(2)=UTL_GETUNIT(); CALL OSD_OPEN(IU(2),FILE=FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.DBF',STATUS='OLD',FORM='UNFORMATTED', &
   ACTION='READ,DENYWRITE',ACCESS='TRANSPARENT',IOSTAT=IOS)
 IF(IOS.NE.0)IU(2)=0

 !## open unformatted gen file
 JU=UTL_GETUNIT()
 CALL OSD_OPEN(JU,FILE=FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.GEN',ACTION='WRITE',STATUS='UNKNOWN',FORM='UNFORMATTED',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  DO I=1,SIZE(IU); IF(IU(I).GT.0)CLOSE(IU(I)); ENDDO
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open '//CHAR(13)//FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.gen','Error')
  RETURN
 ENDIF

 CALL UTL_MESSAGEHANDLE(0)
 
 !## read dbf file
 IF(IU(2).GT.0)THEN
  !## 1+1+1+1+4+2+2=12
  READ(IU(2)) VERSION,YEAR,MONTH,DAY,NRECS,LHEAD,LENREC
  MAXCOL=(LHEAD-32)/32
  IF(ALLOCATED(DBF))DEALLOCATE(DBF); ALLOCATE(DBF(MAXCOL,1:NRECS))
  ALLOCATE(COLWIDTH(MAXCOL),COLNAMES(MAXCOL))
 
  !## read remaining bytes
  READ(IU(2)) (DAY,I=1,20)
   
  IOFFSET=1
  DO I=1,MAXCOL
   !## is 15 bytes 11+1+4+1+1=18
   READ(IU(2)) COLNAMES(I),COLTYPE,CA,CWIDTH,CDEC
   COLWIDTH(I)=ICHAR(CWIDTH)
   COLDEC  =ICHAR(CDEC)
   COLOFF  =IOFFSET

   STRLEN=COLWIDTH(I)
   DO IREC=1,NRECS
    ALLOCATE(CHARACTER(LEN=STRLEN) :: DBF(I,IREC)%STRING)
   ENDDO
   
   IOFFSET        =IOFFSET+COLWIDTH(I)
   !## read remaining bytes
   READ(IU(2)) (DAY,J=1,32-18) 
  ENDDO

  !## read remaining bytes
  REWIND(IU(2))

  DATAOFF=32*MAXCOL+35-1
  READ(IU(2)) (DAY,J=1,DATAOFF)

  DO IREC=1,NRECS
   DO I=1,MAXCOL; READ(IU(2)) DBF(I,IREC)%STRING; ENDDO
   READ(IU(2),IOSTAT=IOS) DAY 
   IF(IOS.NE.0)EXIT
  ENDDO

 ELSE

  MAXCOL=0

 ENDIF

 DO II=1,2

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

  !## write header in pol-file
  IF(II.EQ.2)THEN
   WRITE(JU)  XMIN,XMAX,YMIN,YMAX
   WRITE(JU)  MAXPOL,MAXCOL
   IF(MAXCOL.GT.0)THEN
    WRITE(JU) (COLWIDTH(I),I=1,MAXCOL) 
    WRITE(JU) (COLNAMES(I),I=1,MAXCOL) 
   ENDIF
  ENDIF

  K=0; IF(II.EQ.1)MAXPOL=0
  DO

   READ(IU(1),IOSTAT=IOS) I1,I2; IF(IOS.NE.0)EXIT
   READ(IU(1),IOSTAT=IOS) ISHP;  IF(IOS.NE.0)EXIT

   K=K+1   

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
    
     SELECT CASE (ISHP)
      CASE (13,15)
       IF(ALLOCATED(ZP))THEN; IF(SIZE(ZP).LT.NUMPOINTS)DEALLOCATE(ZP); ENDIF
       IF(.NOT.ALLOCATED(ZP))ALLOCATE(ZP(NUMPOINTS))
     END SELECT

     SELECT CASE (ISHP)
      CASE (13,15,23)
       IF(ALLOCATED(MP))THEN; IF(SIZE(MP).LT.NUMPOINTS)DEALLOCATE(MP); ENDIF
       IF(.NOT.ALLOCATED(MP))ALLOCATE(MP(NUMPOINTS))
     END SELECT
         
     READ(IU(1)) (IP(I),I=1,NUMPARTS)
     IP(NUMPARTS+1)=NUMPOINTS
 
     DO I=1,NUMPARTS
      IF(II.EQ.1)MAXPOL=MAXPOL+1     
          
      N=((IP(I+1)-1)-IP(I))+1; ALLOCATE(XC(N),YC(N))

      III=0
      DO J=IP(I),IP(I+1)-1
       III=III+1
       READ(IU(1)) XC(III),YC(III)
      ENDDO

      !## write unformatted genfile
      IF(II.EQ.2)THEN
       IF(ISHP.EQ.5.OR.ISHP.EQ.15)THEN
        WRITE(JU) N,ID_POLYGON
       ELSE
        WRITE(JU) N,ID_LINE
       ENDIF
       IF(MAXCOL.GT.0)WRITE(JU) (DBF(III,K)%STRING,III=1,MAXCOL)
       WRITE(JU) MINVAL(XC(1:N)),MINVAL(YC(1:N)),MAXVAL(XC(1:N)),MAXVAL(YC(1:N))
       WRITE(JU) (XC(III),YC(III),III=1,N)
      ENDIF
      DEALLOCATE(XC,YC)
     ENDDO

     SELECT CASE (ISHP)
      CASE (13,15)
       READ(IU(1)) ZMIN
       READ(IU(1)) ZMAX
       DO I=1,NUMPOINTS; READ(IU(1)) ZP(I); ENDDO
     END SELECT

     !## optional for ishp.eq.13
     SELECT CASE (ISHP)
      CASE (13,15,23)
       READ(IU(1)) MMIN
       READ(IU(1)) MMAX
       DO I=1,NUMPOINTS; READ(IU(1)) MP(I); ENDDO
     END SELECT

    !## points -> gen
    CASE (1,11) 
     N=1; ALLOCATE(XC(N),YC(N))
     IF(ISHP.EQ.1)THEN
      READ(IU(1)) XC(1),YC(1)
     ELSE
      READ(IU(1)) XC(1),YC(1),Z,M
     ENDIF    
     IF(II.EQ.1)THEN
      MAXPOL=MAXPOL+1    
     ELSE
      WRITE(JU) N,ID_POINT
      IF(MAXCOL.GT.0)WRITE(JU) (DBF(III,K)%STRING,III=1,MAXCOL) 
      WRITE(JU) MINVAL(XC(1:N)),MINVAL(YC(1:N)),MAXVAL(XC(1:N)),MAXVAL(YC(1:N))
      WRITE(JU) (XC(III),YC(III),III=1,N)
     ENDIF
     DEALLOCATE(XC,YC)
     
    !## multipoints -> ipf
    CASE (8)
     READ(IU(1)) XMIN
     READ(IU(1)) YMIN
     READ(IU(1)) XMAX
     READ(IU(1)) YMAX

     READ(IU(1)) N
     IF(II.EQ.2)THEN
      WRITE(JU) N,ID_POINT
      IF(MAXCOL.GT.0)WRITE(JU) (DBF(III,K)%STRING,III=1,MAXCOL) 
     ENDIF
     ALLOCATE(XC(N),YC(N))
     DO I=1,N
      READ(IU(1)) XC(I),YC(I)
     ENDDO
     IF(II.EQ.2)THEN
      WRITE(JU) MINVAL(XC(1:N)),MINVAL(YC(1:N)),MAXVAL(XC(1:N)),MAXVAL(YC(1:N))
      WRITE(JU) (XC(III),YC(III),III=1,N)
     ENDIF
     DEALLOCATE(XC,YC)
    
    CASE DEFAULT
     CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'Shape '//TRIM(ITOS(ISHP))//' not supported yet!','Error')
     EXIT 

   END SELECT
  ENDDO

  REWIND(IU(1))
  
 ENDDO
 
 IF(ALLOCATED(COLWIDTH))DEALLOCATE(COLWIDTH)
 IF(ALLOCATED(COLNAMES))DEALLOCATE(COLNAMES)
 IF(ALLOCATED(IP))DEALLOCATE(IP)
 IF(ALLOCATED(MP))DEALLOCATE(MP)
 IF(ALLOCATED(DBF))THEN
  DO I=1,MAXCOL
   DO J=1,NRECS
    IF(ALLOCATED(DBF(I,J)%STRING))DEALLOCATE(DBF(I,J)%STRING)
   ENDDO
  ENDDO
  DEALLOCATE(DBF)
 ENDIF
 
 DO I=1,SIZE(IU); IF(IU(I).GT.0)CLOSE(IU(I)); ENDDO
 IF(JU.GT.0)CLOSE(JU)
 
 CALL GEN_INIT(FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.GEN',LDEACTIVATE=.FALSE.,GENCOLOUR=COLOUR_RANDOM(),GENTHICKNESS=1)

 CALL UTL_MESSAGEHANDLE(1)

 END SUBROUTINE TOPOSHPTOGEN

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
 CALL WDIALOGFIELDSTATE(IDF_LABEL8,0)
 CALL WDIALOGFIELDSTATE(IDF_REAL1,0)
 CALL WDIALOGFIELDSTATE(IDF_REAL2,0)
 CALL WDIALOGFIELDSTATE(IDF_INTEGER1,0)
 CALL WDIALOGFIELDSTATE(IDF_CHECK3,0)     
 CALL WDIALOGFIELDSTATE(IDF_CHECK4,0)     
 CALL WDIALOGFIELDSTATE(IDF_MENU3,0)
  
 ALLOCATE(ILIST(NATTRIB)) 

 ILIST=0
 IF(ABS(MP(IPLOT)%IEQ).GT.0)THEN
  CALL UTL_FILLARRAY(ILIST,NATTRIB,ABS(MP(IPLOT)%IEQ))
 ENDIF
 CALL WDIALOGPUTMENU(IDF_MENU1,ATTRIB,NATTRIB,ILIST)          !## plot
 IF(MP(IPLOT)%IEQ.LT.0)CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,0)  !## use colouring for labels
 IF(MP(IPLOT)%IEQ.GE.0)CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,1)  !## use colouring for labels
 MP(IPLOT)%TSIZE=MAX(1,MIN(MP(IPLOT)%TSIZE,25))
 CALL WDIALOGPUTINTEGER(IDF_INTEGER4,MP(IPLOT)%TSIZE)
 IF(TRIM(MP(IPLOT)%TFORMAT).EQ.'')MP(IPLOT)%TFORMAT='F10.2'
 CALL WDIALOGPUTSTRING(IDF_STRING1,MP(IPLOT)%TFORMAT)
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,IBACKSLASH)

 CALL UTL_DIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDHELP)
       CALL UTL_GETHELP('4.2.1','MMO.IPO.IPFCon') 
     CASE (IDOK)
      CALL WDIALOGGETMENU(IDF_MENU1,ILIST)
      CALL UTL_READARRAY(ILIST,NATTRIB,MP(IPLOT)%IEQ)
      !## use colouring for labels
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)  
      IF(I.EQ.0)MP(IPLOT)%IEQ=-1*MP(IPLOT)%IEQ
      CALL WDIALOGGETINTEGER(IDF_INTEGER4,MP(IPLOT)%TSIZE)  !second z
      CALL WDIALOGPUTSTRING(IDF_STRING1,MP(IPLOT)%TFORMAT)
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
 
END MODULE MOD_GENPLOT
