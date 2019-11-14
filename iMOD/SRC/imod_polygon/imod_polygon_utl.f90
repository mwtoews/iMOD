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
MODULE MOD_POLYGON_UTL

USE WINTERACTER
USE RESOURCE
USE MODPLOT, ONLY : MPW,MP,MXMPLOT
USE IMODVAR, ONLY : OFFSETX,OFFSETY
USE MOD_POLYGON_PAR
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_UTL, ONLY : ITOS,RTOS,UTL_GETUNIT,UTL_WSELECTFILE,UTL_CREATEDIR,UTL_GENLABELSREAD,UTL_CAP, &
          UTL_GENLABELSDEALLOCATE,UTL_GENLABELSWRITE,CCNST,UTL_GETHELP,UTL_PLOT1BITMAP,UTL_PLOT2BITMAP,VAR
USE MOD_IDF, ONLY : IDFDEALLOCATE
USE MOD_OSD, ONLY : OSD_OPEN

CONTAINS

 !###======================================================================
 SUBROUTINE POLYGON1_UTL_EXPORTGEN()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT,IFORMAT,IU
 
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL)THEN
   IF(MP(IPLOT)%IPLOT.EQ.6)EXIT
  ENDIF
 ENDDO
 !## nothing find
 IF(IPLOT.GT.MXMPLOT)RETURN
 
 !## initialise gen
 CALL POLYGON1INIT()

 IF(.NOT.POLYGON_UTL_OPENGEN(MP(IPLOT)%IDFNAME,IFORMAT,IU))RETURN
 !## ascii
 IF(IFORMAT.EQ.1)THEN
  CALL POLYGON_UTL_CONVERTGEN(MP(IPLOT)%IDFNAME)
 ELSE
  !## read binary genfile
  CALL POLYGON1SAVELOADSHAPE(ID_LOADSHAPE,MP(IPLOT)%IDFNAME,'GEN') 
  !## save gen 
  IF(SHP%NPOL.GT.0)CALL POLYGON1SAVELOADSHAPE(ID_SAVESHAPE,'','GEN')
 ENDIF
 !## clear gen
 CALL POLYGON1CLOSE()
 
 END SUBROUTINE POLYGON1_UTL_EXPORTGEN
 
 !###======================================================================
 LOGICAL FUNCTION POLYGON_UTL_OPENGEN(FNAME,IFORMAT,IU)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(OUT) :: IFORMAT,IU
 INTEGER :: IOS
 REAL(KIND=DP_KIND) :: X
 
 POLYGON_UTL_OPENGEN=.FALSE.

 IU=UTL_GETUNIT()

 IFORMAT=0
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED',ACTION='READ,DENYWRITE',IOSTAT=IOS)
 !## error in opening gen file
 IF(IU.EQ.0)RETURN
 READ(IU,IOSTAT=IOS) X; IF(IOS.NE.0)IFORMAT=1; CLOSE(IU)
     
 IF(IFORMAT.EQ.1)THEN 
  CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='FORMATTED',ACTION='READ,DENYWRITE',IOSTAT=IOS)
 ELSEIF(IFORMAT.EQ.0)THEN 
  CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED',ACTION='READ,DENYWRITE',IOSTAT=IOS)
 ELSE
  IOS=-1
 ENDIF
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open/read '//CHAR(13)//TRIM(FNAME),'Error')
  RETURN
 ENDIF
 
 POLYGON_UTL_OPENGEN=.TRUE.
 
 END FUNCTION POLYGON_UTL_OPENGEN
 
 !###======================================================================
 SUBROUTINE POLYGON_UTL_GETLABELSGEN(IU,IFORMAT,LABELS)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,IFORMAT
 CHARACTER(LEN=11),DIMENSION(:),POINTER,INTENT(OUT) :: LABELS
 INTEGER :: MAXPOL,MAXCOL,I,J
 
 IF(ASSOCIATED(LABELS))DEALLOCATE(LABELS)

 !## binary format
 IF(IFORMAT.EQ.0)THEN
  READ(IU)
  READ(IU) MAXPOL,MAXCOL
  ALLOCATE(LABELS(MAXCOL))
  READ(IU) (J,I=1,MAXCOL)
  READ(IU) (LABELS(I),I=1,MAXCOL)
 ELSE
  ALLOCATE(LABELS(1)); LABELS(1)='ID'
 ENDIF
  
 CLOSE(IU)
 
 END SUBROUTINE POLYGON_UTL_GETLABELSGEN
 
 !###======================================================================
 SUBROUTINE POLYGON_UTL_CONVERTGEN(FNAME,OFNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT),OPTIONAL :: FNAME
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: OFNAME
 INTEGER :: IOS,IU,JU,N,M,I,J,K,MAXCOL,MAXPOL,IFORMAT,IERROR,IPOL
 CHARACTER(LEN=256) :: GENFNAME,DATFNAME
 CHARACTER(LEN=52) :: CID
 REAL(KIND=DP_KIND) :: X,Y,X1,Y1,X2,Y2
 INTEGER,DIMENSION(:),ALLOCATABLE :: ICOLWIDTH
 REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: XC,YC
 CHARACTER(LEN=11),DIMENSION(:),ALLOCATABLE :: LABELS
 LOGICAL :: LEX
   
 GENFNAME=''
 IF(PRESENT(FNAME))THEN
  IF(LEN_TRIM(FNAME).EQ.0)THEN
   IF(.NOT.UTL_WSELECTFILE('ESRI ASCII File (*.GEN)|*.GEN|',&
      LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,GENFNAME,'Load ESRI ASCII File (*.GEN)'))RETURN
  ELSE
   GENFNAME=FNAME
  ENDIF
 ELSE
  IF(.NOT.UTL_WSELECTFILE('ESRI ASCII File (*.GEN)|*.GEN|',&
     LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,GENFNAME,'Load ESRI ASCII File (*.GEN)'))RETURN
 ENDIF
 
 IF(.NOT.POLYGON_UTL_OPENGEN(GENFNAME,IFORMAT,IU))RETURN
 IF(IFORMAT.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'This selected GEN file is an iMOD BINARY GEN file allready.','Information')
  RETURN
 ENDIF
 DATFNAME=GENFNAME(:INDEX(GENFNAME,'.',.TRUE.)-1)//'.DAT'
 !## possible grid with values from dat file
 INQUIRE(FILE=DATFNAME,EXIST=LEX)
 IF(LEX)THEN
  CALL UTL_GENLABELSREAD(DATFNAME,VAR,MAXPOL,MAXCOL)
  ALLOCATE(LABELS(MAXCOL),ICOLWIDTH(MAXCOL))
  !## get max. label length per column
  DO I=1,MAXCOL
   N=0
   DO J=1,MAXPOL
    N=MAX(N,LEN_TRIM(VAR(I,J)) ) 
   ENDDO
   !## apply minimal column width of 11
   ICOLWIDTH(I)=MAX(11,N)
   !## label maximal 11 characters
   WRITE(LABELS(I),'(A11)') VAR(I,0)
  ENDDO
 ELSE
  MAXCOL=1
  ALLOCATE(LABELS(MAXCOL),ICOLWIDTH(MAXCOL)); ICOLWIDTH=11; LABELS='ID'
  ALLOCATE(VAR(1,MAXCOL)); VAR=''
 ENDIF

 IF(.NOT.PRESENT(OFNAME))THEN
  IF(.NOT.UTL_WSELECTFILE('iMOD BINARY File (*.GEN)|*.GEN|',&
     SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,GENFNAME,'Save iMOD BINARY File (*.GEN)'))THEN
   CLOSE(IU); RETURN
  ENDIF
 ELSE
  GENFNAME=OFNAME
 ENDIF
 
 IERROR=0; JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=GENFNAME,STATUS='UNKNOWN',FORM='UNFORMATTED',ACTION='WRITE')
  
 X1= HUGE(1.0D0); Y1= HUGE(1.0D0)
 X2=-HUGE(1.0D0); Y2=-HUGE(1.0D0)
 M = 0
 MAXPOL=0
 
ILOOP: DO I=1,2

  IPOL=0
  IF(I.EQ.2)THEN
   WRITE(JU) X1,Y1,X2,Y2
   !## write header
   WRITE(JU) MAXPOL,MAXCOL
   WRITE(JU) ICOLWIDTH
   WRITE(JU) LABELS
  ENDIF
  
  DO
   READ(IU,*,IOSTAT=IOS) CID; IF(IOS.NE.0)EXIT

   N=0
   DO
    READ(IU,*,IOSTAT=IOS) X,Y; IF(IOS.NE.0)EXIT
    X1=MIN(X1,X); X2=MAX(X2,X); Y1=MIN(Y1,Y); Y2=MAX(Y2,Y)
    N=N+1
    IF(I.EQ.2)THEN; XC(N)=X; YC(N)=Y; ENDIF
   ENDDO
   M=MAX(M,N)
   IF(I.EQ.1.AND.N.GT.0)MAXPOL=MAXPOL+1
   IPOL=IPOL+1
   
   IF(I.EQ.2.AND.N.GT.0)THEN

    !## get correct k
    IF(LEX)THEN
     DO K=1,MAXPOL
      IF(TRIM(ADJUSTL(CID)).EQ.TRIM(ADJUSTL(VAR(1,K))))EXIT
     ENDDO
     IF(K.GT.MAXPOL)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'There is an entry in the DAT file that cannot be related to any entry in the GEN file','Error')
      IERROR=1; EXIT ILOOP
      !## no label found
     ENDIF
    ENDIF
    
    IF(XC(1).EQ.XC(N).AND.YC(1).EQ.YC(N))THEN
     WRITE(JU)  N,ID_POLYGON
    ELSE
     WRITE(JU)  N,ID_LINE
    ENDIF
    IF(LEX)THEN
     WRITE(JU) (VAR(J,K)(1:ICOLWIDTH(J)),J=1,MAXCOL)
    ELSE
     WRITE(VAR(1,1),'(I11)') IPOL !K
     WRITE(JU) VAR(1,1)(1:11)
    ENDIF
    X1=MINVAL(XC(1:N)); Y1=MINVAL(YC(1:N))
    X2=MAXVAL(XC(1:N)); Y2=MAXVAL(YC(1:N))
    WRITE(JU)  X1,Y1,X2,Y2
    WRITE(JU) (XC(J),YC(J),J=1,N)
   ENDIF
   
  ENDDO
  IF(I.EQ.1)ALLOCATE(XC(M),YC(M))
  
  REWIND(IU)
 ENDDO ILOOP

 DEALLOCATE(XC,YC,ICOLWIDTH,LABELS,VAR)
 CLOSE(IU)
 IF(IERROR.EQ.0)THEN
  CLOSE(JU)
 ELSE
  CLOSE(JU,STATUS='DELETE')
 ENDIF
 
 IF(PRESENT(FNAME))FNAME=GENFNAME

 END SUBROUTINE POLYGON_UTL_CONVERTGEN
 
 !###======================================================================
 SUBROUTINE POLYGON_UTL_GETSHAPE(ISHAPE,IACT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: ISHAPE
 INTEGER,INTENT(IN),DIMENSION(6) :: IACT
 INTEGER :: ITYPE,I,N
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER,DIMENSION(6) :: ID,JD
 DATA ID/ID_POINT,ID_RECTANGLE,ID_POLYGON,ID_CIRCLE,ID_LINE,ID_GRID/
 DATA JD/IDF_PICTURE1,IDF_PICTURE2,IDF_PICTURE3,IDF_PICTURE4,IDF_PICTURE5,IDF_PICTURE9/

 N=0; DO I=1,SIZE(IACT); IF(IACT(I).EQ.1)N=N+1; ENDDO
 
 IF(N.EQ.1)THEN
  DO I=1,SIZE(ID); IF(IACT(I).EQ.1)ISHAPE=ID(I); END DO
 ELSE
 !## get tool first
  CALL WDIALOGLOAD(ID_DTOOLS)
  CALL WDIALOGPUTIMAGE(IDF_PICTURE1,ID_ICONPOINT)
  CALL WDIALOGPUTIMAGE(IDF_PICTURE2,ID_ICONRECTANGLE)
  CALL WDIALOGPUTIMAGE(IDF_PICTURE3,ID_ICONPOLYGON)
  CALL WDIALOGPUTIMAGE(IDF_PICTURE4,ID_ICONCIRCLE)
  CALL WDIALOGPUTIMAGE(IDF_PICTURE5,ID_ICONLINE)
  CALL WDIALOGPUTIMAGE(IDF_PICTURE9,ID_ICONGRID)
  DO I=1,SIZE(ID); CALL WDIALOGFIELDSTATE(ID(I),IACT(I)); END DO
  DO I=1,SIZE(JD); CALL WDIALOGFIELDSTATE(JD(I),IACT(I)); END DO
  DO I=1,SIZE(ID)
   IF(IACT(I).EQ.1)THEN
    CALL WDIALOGPUTRADIOBUTTON(ID(I))
    ISHAPE=ID(I); EXIT
   ENDIF
  END DO
  CALL WDIALOGSHOW(ITYPE=SEMIMODELESS)
  DO WHILE(.TRUE.)
   CALL WMESSAGE(ITYPE, MESSAGE)
   SELECT CASE (ITYPE)
    CASE (FIELDCHANGED)
     ISHAPE=MESSAGE%VALUE1
    CASE(PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (IDOK,IDCANCEL)
       EXIT
     END SELECT
   END SELECT
  ENDDO
  CALL WDIALOGUNLOAD()
  IF(MESSAGE%VALUE1.EQ.IDCANCEL)THEN
   ISHAPE=0; RETURN
  ENDIF
 ENDIF

 SELECT CASE (ISHAPE)
  CASE (ID_POINT)
   CALL WCURSORSHAPE(ID_CURSORIDFVALUE)
  CASE (ID_CIRCLE)
   CALL WCURSORSHAPE(ID_CURSORCIRCLE)
  CASE (ID_RECTANGLE)
  CALL WCURSORSHAPE(ID_CURSORRECTANGLE)
  CASE (ID_POLYGON)
   CALL WCURSORSHAPE(ID_CURSORPOLYGON)
  CASE (ID_LINE)
   CALL WCURSORSHAPE(ID_CURSORLINE)
  CASE (ID_GRID)
!   CALL WCURSORSHAPE(ID_CURSORLINE)
 END SELECT

 END SUBROUTINE POLYGON_UTL_GETSHAPE

 !###======================================================================
 SUBROUTINE POLYGON_UTL_FILLDATAGRID()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ITYPE,I,J,K,L,ICOL,IROW,ID,MAXCOL,IWIDTH,STRLEN,NPOL
 TYPE(WIN_MESSAGE) :: MESSAGE
 CHARACTER(LEN=11) :: LABELNAME
 CHARACTER(LEN=:),ALLOCATABLE :: STRING
 LOGICAL :: LEX
 
 !## current selected dialog
 ID=WINFODIALOG(CURRENTDIALOG)

 !## nothing selected, take everything
 NPOL=0; IF(SHP%NPOL.GT.0)NPOL=SUM(SHP%POL(1:SHP%NPOL)%IACT)
 IF(NPOL.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Select at least a single polygon before modifying attributes','Error')
  RETURN
 ENDIF
  
 CALL WDIALOGLOAD(ID_DGENDATA)
 CALL WDIALOGTITLE('Attribute Values for Current (selected) Polygons/Lines/Points')
 IF(NPOL.GT.WINFOGRID(IDF_GRID1,GRIDROWSMAX))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot display all selected data ('//TRIM(ITOS(NPOL))//' records)'//CHAR(13)// &
   'There is a maximum of '//TRIM(ITOS(WINFOGRID(IDF_GRID1,GRIDROWSMAX)))//' rows'//CHAR(13)// &
  'Select less before entering this dialog in order to continue.','Error')
  CALL WDIALOGUNLOAD(); CALL WDIALOGSELECT(ID); RETURN
 ENDIF

 CALL WDIALOGPUTIMAGE(ID_PLUS,ID_ICONPLUS)
 CALL WDIALOGPUTIMAGE(ID_MIN,ID_ICONMIN)
 CALL WDIALOGPUTIMAGE(ID_RENAME,ID_ICONRENAME)

 CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,SHP%NLBL)
 SHP%ILBL=MAX(1,SHP%ILBL)

 CALL POLYGON_UTL_FILLDATAGRID_GRID(NPOL)
 
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
      MAXCOL=SIZE(SHP%COLNAMES)
      CALL WDIALOGPUTSTRING(IDF_STRING1,'Attribute'//TRIM(ITOS(MAXCOL+1)))
      DO
       CALL WMESSAGE(ITYPE,MESSAGE)
       SELECT CASE (ITYPE)
        CASE (PUSHBUTTON)
         SELECT CASE (MESSAGE%VALUE1)
          CASE (IDOK,IDCANCEL)
           CALL WDIALOGGETINTEGER(IDF_INTEGER1,IWIDTH)
           CALL WDIALOGGETSTRING(IDF_STRING1,LABELNAME); EXIT
         END SELECT
       END SELECT
      ENDDO
      CALL WDIALOGUNLOAD(); CALL WDIALOGSELECT(ID_DGENDATA)

      !## increase memory
      MAXCOL=SIZE(SHP%COLNAMES)
      ALLOCATE(SHP%COLNAMES_BU(MAXCOL+1)); DO I=1,MAXCOL; SHP%COLNAMES_BU(I)=SHP%COLNAMES(I); ENDDO; DEALLOCATE(SHP%COLNAMES); SHP%COLNAMES=>SHP%COLNAMES_BU
      ALLOCATE(SHP%LWIDTH_BU(MAXCOL+1));   DO I=1,MAXCOL; SHP%LWIDTH_BU(I)  =SHP%LWIDTH(I);   ENDDO; DEALLOCATE(SHP%LWIDTH);   SHP%LWIDTH  =>SHP%LWIDTH_BU

      !## add label
      SHP%COLNAMES(MAXCOL+1)=LABELNAME
      SHP%LWIDTH(MAXCOL+1)  =IWIDTH

      !## increase number of attributes for all polygons
      DO J=1,SHP%NPOL; ALLOCATE(SHP%POL(J)%LBL_BU(MAXCOL+1)); ENDDO
      DO I=1,MAXCOL+1
       STRLEN=SHP%LWIDTH(I)
       DO J=1,SHP%NPOL
        ALLOCATE(SHP%POL(J)%LBL_BU(I)%STRING(STRLEN))
        !## reset new column empty
        IF(I.EQ.MAXCOL+1)THEN
         SHP%POL(J)%LBL_BU(I)%STRING=''
        ELSE
         SHP%POL(J)%LBL_BU(I)%STRING=SHP%POL(J)%LBL(I)%STRING
        ENDIF
       ENDDO
      ENDDO
      DO J=1,SHP%NPOL; DEALLOCATE(SHP%POL(J)%LBL); SHP%POL(J)%LBL=>SHP%POL(J)%LBL_BU; ENDDO
          
      CALL POLYGON_UTL_FILLDATAGRID_GRID(NPOL)
       
     CASE (ID_MIN)
      CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to remove the attribute column '//TRIM(SHP%COLNAMES(ICOL)),'Question')
      IF(WINFODIALOG(4).EQ.1)THEN

       !## decrease memory
       MAXCOL=SIZE(SHP%COLNAMES)
       ALLOCATE(SHP%COLNAMES_BU(MAXCOL-1))
       J=0; DO I=1,MAXCOL; IF(I.NE.ICOL)THEN; J=J+1; SHP%COLNAMES_BU(J)=SHP%COLNAMES(I); ENDIF; ENDDO; DEALLOCATE(SHP%COLNAMES); SHP%COLNAMES=>SHP%COLNAMES_BU
       ALLOCATE(SHP%LWIDTH_BU(MAXCOL-1))
       J=0; DO I=1,MAXCOL; IF(I.NE.ICOL)THEN; J=J+1; SHP%LWIDTH_BU(J)  =SHP%LWIDTH(I);   ENDIF; ENDDO; DEALLOCATE(SHP%LWIDTH);   SHP%LWIDTH  =>SHP%LWIDTH_BU
       !## increase number of attributes for all polygon
       DO J=1,SHP%NPOL; ALLOCATE(SHP%POL(J)%LBL_BU(MAXCOL-1)); ENDDO
       K=0
       DO I=1,MAXCOL
        IF(I.NE.ICOL)THEN
         K=K+1
         STRLEN=SHP%LWIDTH(I)
         DO J=1,SHP%NPOL
          ALLOCATE(SHP%POL(J)%LBL_BU(K)%STRING(STRLEN))
          SHP%POL(J)%LBL_BU(K)%STRING=SHP%POL(J)%LBL(I)%STRING
         ENDDO
        ENDIF
       ENDDO
       DO J=1,SHP%NPOL; DEALLOCATE(SHP%POL(J)%LBL); SHP%POL(J)%LBL=>SHP%POL(J)%LBL_BU; ENDDO

       CALL POLYGON_UTL_FILLDATAGRID_GRID(NPOL)

      ENDIF

     CASE (ID_RENAME)
      IF(ICOL.LE.0)THEN
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Select a column to rename it','Warning')
      ELSE
       CALL WDIALOGLOAD(ID_POLYGONSHAPENAME,ID_POLYGONSHAPENAME)
       CALL WDIALOGSHOW(-1,-1,0,3)
       CALL WDIALOGPUTSTRING(IDF_LABEL1,'Rename Attribute')
       CALL WDIALOGPUTINTEGER(IDF_INTEGER1,SHP%LWIDTH(ICOL))
       CALL WDIALOGPUTSTRING(IDF_STRING1,TRIM(SHP%COLNAMES(ICOL)))
       DO
        CALL WMESSAGE(ITYPE,MESSAGE)
        SELECT CASE (ITYPE)
         CASE (PUSHBUTTON)
          SELECT CASE (MESSAGE%VALUE1)
           CASE (IDOK,IDCANCEL)
            CALL WDIALOGGETINTEGER(IDF_INTEGER1,IWIDTH)
            CALL WDIALOGGETSTRING(IDF_STRING1,SHP%COLNAMES(ICOL))
            IF(IWIDTH.LT.SHP%LWIDTH(ICOL))THEN
             CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'If you decrease the size of a field, you might loose existing information.'//CHAR(13)// &
               'Are you sure to continue ?','Question')
             IF(WINFODIALOG(4).EQ.1)EXIT
            ELSE
             EXIT
            ENDIF
          END SELECT
        END SELECT
       ENDDO
       CALL WDIALOGUNLOAD(); CALL WDIALOGSELECT(ID_DGENDATA)
       MAXCOL=SIZE(SHP%COLNAMES)
       IF(IWIDTH.NE.SHP%LWIDTH(ICOL))THEN
        DO J=1,SHP%NPOL; ALLOCATE(SHP%POL(J)%LBL_BU(MAXCOL)); ENDDO
        DO J=1,SHP%NPOL
         DO I=1,MAXCOL
          STRLEN=SHP%LWIDTH(I); IF(I.EQ.ICOL)STRLEN=IWIDTH 
          ALLOCATE(SHP%POL(J)%LBL_BU(I)%STRING(STRLEN)); SHP%POL(J)%LBL_BU(I)%STRING=''
          DO K=1,MIN(STRLEN,SHP%LWIDTH(I))
           SHP%POL(J)%LBL_BU(I)%STRING(K)=SHP%POL(J)%LBL(I)%STRING(K)
          ENDDO
         ENDDO
        ENDDO
        DO J=1,SHP%NPOL; DEALLOCATE(SHP%POL(J)%LBL); SHP%POL(J)%LBL=>SHP%POL(J)%LBL_BU; ENDDO
        SHP%LWIDTH(ICOL)=IWIDTH
       ENDIF
       CALL POLYGON_UTL_FILLDATAGRID_GRID(NPOL)
      ENDIF

     CASE (IDHELP)
      CALL UTL_GETHELP('4.1.4','MMO.IDO.IDFEdit')

     !## get data
     CASE (IDOK)
      CALL WDIALOGGETMENU(IDF_MENU1,SHP%ILBL)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,SHP%NLBL)
      MAXCOL=SIZE(SHP%COLNAMES); LEX=.TRUE.
      DO J=1,MAXCOL
       STRLEN=256; ALLOCATE(CHARACTER(LEN=STRLEN) :: STRING)
       K=0; DO I=1,SHP%NPOL
        IF(SHP%POL(I)%IACT.EQ.0)CYCLE
        K=K+1
        CALL WGRIDGETCELLSTRING(IDF_GRID1,J,K,STRING)
        IF(LEN_TRIM(STRING).GT.SHP%LWIDTH(J).AND.LEX)THEN
         CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'The entry '//TRIM(STRING)//CHAR(13)//' exceeds the current field width ('//TRIM(ITOS(SHP%LWIDTH(J)))//')'//CHAR(13)// &
          'Do you want to continue or return to the previous window?','Question')
         IF(WINFODIALOG(4).EQ.1)THEN; LEX=.FALSE.; ELSE; EXIT; ENDIF
        ENDIF
        DO L=1,SHP%LWIDTH(J)
         SHP%POL(I)%LBL(J)%STRING(L)=STRING(L:L)
        ENDDO
       END DO
       DEALLOCATE(STRING)
       IF(I.LE.SHP%NPOL)EXIT
      END DO
      IF(J.GT.MAXCOL)THEN
       DO I=1,SHP%NPOL
        SHP%POL(I)%PNAME=''
        DO J=1,MIN(52,SHP%LWIDTH(1))
         SHP%POL(I)%PNAME(J:J)=SHP%POL(I)%LBL(1)%STRING(J)
        ENDDO
       ENDDO
       EXIT
      ENDIF
     CASE (IDCANCEL)
      EXIT

    END SELECT
  END SELECT
 ENDDO

 CALL WDIALOGUNLOAD(); CALL WDIALOGSELECT(ID)
 
 END SUBROUTINE POLYGON_UTL_FILLDATAGRID

 !###======================================================================
 SUBROUTINE POLYGON_UTL_FILLDATAGRID_GRID(NPOL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NPOL
 INTEGER,ALLOCATABLE,DIMENSION(:) :: ICOLS
 INTEGER :: I,J,K,L,MAXCOL,STRLEN
 CHARACTER(LEN=:),ALLOCATABLE :: STRING

 MAXCOL=SIZE(SHP%COLNAMES)
 ALLOCATE(ICOLS(MAXCOL)); ICOLS=1; CALL WGRIDCOLUMNS(IDF_GRID1,MAXCOL,ICOLS); DEALLOCATE(ICOLS)
 CALL WGRIDROWS(IDF_GRID1,NPOL)
 
 !## put labels
 DO I=1,MAXCOL; CALL WGRIDLABELCOLUMN(IDF_GRID1,I,TRIM(SHP%COLNAMES(I))); END DO
 DO J=1,MAXCOL
  STRLEN=SHP%LWIDTH(J)
  ALLOCATE(CHARACTER(LEN=STRLEN) :: STRING)
  K=0; DO I=1,SHP%NPOL
   IF(SHP%POL(I)%IACT.EQ.0)CYCLE
   K=K+1
   DO L=1,SHP%LWIDTH(J)
    STRING(L:L)=TRIM(SHP%POL(I)%LBL(J)%STRING(L))
   ENDDO
   CALL WGRIDPUTCELLSTRING(IDF_GRID1,J,K,STRING)
   SELECT CASE (SHP%POL(I)%ITYPE)
    CASE (ID_POLYGON); CALL WGRIDLABELROW(IDF_GRID1,K,'Polygon')
    CASE (ID_POINT); CALL WGRIDLABELROW(IDF_GRID1,K,'Point')
    CASE (ID_LINE); CALL WGRIDLABELROW(IDF_GRID1,K,'Line')
    CASE (ID_RECTANGLE); CALL WGRIDLABELROW(IDF_GRID1,K,'Rectangle')
    CASE (ID_CIRCLE); CALL WGRIDLABELROW(IDF_GRID1,K,'Circle')
   END SELECT
  END DO
  DEALLOCATE(STRING)
 END DO

 CALL WDIALOGPUTMENU(IDF_MENU1,SHP%COLNAMES,SIZE(SHP%COLNAMES),SHP%ILBL)
 
 END SUBROUTINE POLYGON_UTL_FILLDATAGRID_GRID

 !###======================================================================
 SUBROUTINE POLYGON1SAVELOADSHAPE(CODE,GENFNAME,CFTYPE,ISAVESEL)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: GENFNAME,CFTYPE
 INTEGER,INTENT(IN),OPTIONAL :: ISAVESEL
 INTEGER,INTENT(IN) :: CODE
 INTEGER :: IU,JU,I,J,K,N,II,JJ,IOS,STRLEN,FTYPE
 CHARACTER(LEN=256) :: FNAME
 CHARACTER(LEN=104) :: LSTR,FSTR
 INTEGER :: IFORMAT,MAXCOL
 TYPE STROBJ
  CHARACTER(LEN=52) :: STRING
 END TYPE
 TYPE(STROBJ),ALLOCATABLE,DIMENSION(:) :: STR
 LOGICAL :: LEX
 
 LEX=.FALSE.; IF(PRESENT(ISAVESEL))THEN; IF(ISAVESEL.EQ.1)LEX=.TRUE.; ENDIF

 SELECT CASE (CFTYPE)
  CASE ('GEN')
   FSTR='iMOD BINARY GEN Files (*.GEN)|*.GEN|ESRI ASCII GEN Files (*.GEN)|*.GEN|'
   LSTR='GEN Files'
  CASE ('IPF')
   FSTR='IPF Files (*.IPF)|*.IPF|'
   LSTR='IPF Files'
  CASE ('GEN/IPF')
   FSTR='iMOD BINARY GEN Files (*.GEN)|*.GEN|ESRI ASCII GEN Files (*.GEN)|*.GEN|IPF Files (*.IPF)|*.IPF|'
   LSTR='GEN/IPF Files'
 END SELECT
 
 FNAME=TRIM(PREFVAL(1))//'\SHAPES'

 IF(CODE.EQ.ID_LOADSHAPE)THEN

  IF(GENFNAME.EQ.'')THEN
   IF(.NOT.UTL_WSELECTFILE(FSTR,LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,'Load '//TRIM(LSTR)))RETURN
  ELSE
   FNAME=GENFNAME
  ENDIF

  FNAME=UTL_CAP(FNAME,'U')

  IF(INDEX(FNAME,'.GEN').GT.0)THEN
  
   IF(.NOT.POLYGON_UTL_OPENGEN(FNAME,IFORMAT,IU))RETURN
   IF(IFORMAT.EQ.1)THEN
    CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'You want to read an old ASCII GEN file.'//CHAR(13)// &
      'Select YES to convert this into a BINARY GEN file.'//CHAR(13)// &
      'Select NO to cancel. You might convert this file via [FILE] [IMPORT] at a later stage.','Question')
    IF(WINFODIALOG(4).NE.1)RETURN
    CALL POLYGON_UTL_CONVERTGEN(FNAME)
    IF(.NOT.POLYGON_UTL_OPENGEN(FNAME,IFORMAT,IU))RETURN
   ENDIF
    
   !## reset memory
   CALL POLYGON1INIT()
   DEALLOCATE(SHP%COLNAMES,SHP%LWIDTH)

   !## xmin,ymin,xmax,ymax
   READ(IU) SHP%XMIN,SHP%YMIN,SHP%XMAX,SHP%YMAX
   !## maxpol,maxcol
   READ(IU) SHP%NPOL,MAXCOL

   IF(SHP%NPOL.GE.MAXSHAPES)THEN
    !## reset memory allocation if too small
    MAXSHAPES=SHP%NPOL+10
    CALL POLYGON1INIT()
    REWIND(IU)
    READ(IU) SHP%XMIN,SHP%YMIN,SHP%XMAX,SHP%YMAX
    READ(IU) SHP%NPOL,MAXCOL
   ENDIF

   IF(MAXCOL.GT.0)THEN
    ALLOCATE(SHP%LWIDTH(MAXCOL))
    READ(IU) (SHP%LWIDTH(I),I=1,MAXCOL)
    ALLOCATE(SHP%COLNAMES(MAXCOL))
    DO J=1,SHP%NPOL
     ALLOCATE(SHP%POL(J)%LBL(MAXCOL))
     DO I=1,MAXCOL
      STRLEN=SHP%LWIDTH(I)
      ALLOCATE(SHP%POL(J)%LBL(I)%STRING(STRLEN))
     ENDDO
    ENDDO
    READ(IU) (SHP%COLNAMES(I),I=1,MAXCOL)
   ENDIF
   !## allocate coordinates
   DO I=1,SHP%NPOL
    READ(IU) SHP%POL(I)%N,SHP%POL(I)%ITYPE
    IF(MAXCOL.GT.0)THEN
     READ(IU) (SHP%POL(I)%LBL(J)%STRING,J=1,MAXCOL)
     SHP%POL(I)%PNAME=''
     DO J=1,MIN(52,SHP%LWIDTH(1))
      SHP%POL(I)%PNAME(J:J)=SHP%POL(I)%LBL(1)%STRING(J)
     ENDDO
    ENDIF
    ALLOCATE(SHP%POL(I)%X(SHP%POL(I)%N),SHP%POL(I)%Y(SHP%POL(I)%N))
    !## xmin,ymin,xmax,ymax
    READ(IU) SHP%POL(I)%XMIN,SHP%POL(I)%YMIN,SHP%POL(I)%XMAX,SHP%POL(I)%YMAX
    READ(IU) (SHP%POL(I)%X(J),SHP%POL(I)%Y(J),J=1,SHP%POL(I)%N)
    SHP%POL(I)%IACT =1
    SHP%POL(I)%IWIDTH=2
    SHP%POL(I)%ICOLOR=ICLRPOLG
    IF(SHP%POL(I)%ITYPE.EQ.ID_POINT)THEN
     IF(MAXCOL.EQ.0)SHP%POL(I)%PNAME='Point '//TRIM(ITOS(I))
    ELSEIF(SHP%POL(I)%ITYPE.EQ.ID_POLYGON)THEN
     IF(MAXCOL.EQ.0)SHP%POL(I)%PNAME='Polygon '//TRIM(ITOS(I))
    ELSEIF(SHP%POL(I)%ITYPE.EQ.ID_RECTANGLE)THEN
     IF(MAXCOL.EQ.0)SHP%POL(I)%PNAME='Rectangle '//TRIM(ITOS(I))
    ELSEIF(SHP%POL(I)%ITYPE.EQ.ID_LINE)THEN
     IF(MAXCOL.EQ.0)SHP%POL(I)%PNAME='Line '//TRIM(ITOS(I))
    ELSEIF(SHP%POL(I)%ITYPE.EQ.ID_CIRCLE)THEN
     IF(MAXCOL.EQ.0)SHP%POL(I)%PNAME='Circle '//TRIM(ITOS(I))
    ELSE
     IF(MAXCOL.EQ.0)SHP%POL(I)%PNAME='Unknown Type - Converted to Point '//TRIM(ITOS(I))
     SHP%POL(I)%ITYPE=ID_POINT
    ENDIF
   ENDDO

  !## read ipf file
  ELSE
  
   IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',FORM='FORMATTED',IOSTAT=IOS)
   IF(IOS.NE.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open file for reading:'//CHAR(13)//'['//TRIM(FNAME)//']','Error')
    RETURN
   ENDIF  

   CALL POLYGON1INIT()
   DEALLOCATE(SHP%COLNAMES,SHP%LWIDTH)

   READ(IU,*) SHP%NPOL
   READ(IU,*) MAXCOL; MAXCOL=MAXCOL-2

   IF(SHP%NPOL.GE.MAXSHAPES)THEN
    !## reset memory allocation if too small
    MAXSHAPES=SHP%NPOL+10
    CALL POLYGON1INIT()
   ENDIF
   
   ALLOCATE(SHP%LWIDTH(MAXCOL))
   !## set uniform lwidth
   SHP%LWIDTH=52
   ALLOCATE(SHP%COLNAMES(MAXCOL))
   !## skip xy
   DO I=1,2; READ(IU,*); ENDDO
   DO I=1,MAXCOL; READ(IU,*) SHP%COLNAMES(I); ENDDO; READ(IU,*) 
   DO J=1,SHP%NPOL
    ALLOCATE(SHP%POL(J)%LBL(MAXCOL))
    DO I=1,MAXCOL
     STRLEN=SHP%LWIDTH(I)
     ALLOCATE(SHP%POL(J)%LBL(I)%STRING(STRLEN))
    ENDDO
   ENDDO

   ALLOCATE(STR(MAXCOL))
   
   !## read points
   DO I=1,SHP%NPOL
    SHP%POL(I)%ITYPE=ID_POINT
    SHP%POL(I)%N=1
    ALLOCATE(SHP%POL(I)%X(SHP%POL(I)%N),SHP%POL(I)%Y(SHP%POL(I)%N))
    READ(IU,*) SHP%POL(I)%X(1),SHP%POL(I)%Y(1),(STR(II)%STRING,II=1,MAXCOL)
    DO II=1,MAXCOL
     DO JJ=1,SHP%LWIDTH(II)
      SHP%POL(I)%LBL(II)%STRING(JJ)=STR(II)%STRING(JJ:JJ)
     ENDDO
    ENDDO
    SHP%POL(I)%PNAME=''
    DO J=1,MIN(52,SHP%LWIDTH(1))
     SHP%POL(I)%PNAME(J:J)=SHP%POL(I)%LBL(1)%STRING(J)
    ENDDO
    SHP%POL(I)%XMIN=SHP%POL(I)%X(1); SHP%POL(I)%YMIN=SHP%POL(I)%Y(1)
    SHP%POL(I)%XMAX=SHP%POL(I)%X(1); SHP%POL(I)%YMAX=SHP%POL(I)%Y(1)
    SHP%POL(I)%IACT =1
    SHP%POL(I)%IWIDTH=2
    SHP%POL(I)%ICOLOR=ICLRPOLG
   ENDDO
   
   DEALLOCATE(STR)
  
  ENDIF
  
  CLOSE(IU)

 ELSEIF(CODE.EQ.ID_SAVESHAPE)THEN 
  
  FTYPE=1
  IF(GENFNAME.EQ.'')THEN
   IF(.NOT.UTL_WSELECTFILE(FSTR,SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,'Save '//TRIM(LSTR),FTYPE=FTYPE))RETURN
  ELSE
   FNAME=GENFNAME
   CALL UTL_CREATEDIR(FNAME(:INDEX(FNAME,'\',.TRUE.)-1))
  ENDIF
  
  !## ipf IPF make FTYPE=3
  FNAME=UTL_CAP(FNAME,'U'); IF(INDEX(FNAME,'.IPF').GT.0)FTYPE=3
  
  !## binary gen file
  IF(FTYPE.EQ.1)THEN

   IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='REPLACE',ACTION='WRITE,DENYREAD',FORM='UNFORMATTED',IOSTAT=IOS)
   IF(IOS.NE.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open file for writing:'//CHAR(13)//'['//TRIM(FNAME)//']','Error')
    RETURN
   ENDIF  
 
   !## get min/max per polygon
   SHP%XMIN= 10.0D10; SHP%YMIN= 10.0D10
   SHP%XMAX=-10.0D10; SHP%YMAX=-10.0D10

   DO I=1,SHP%NPOL

    IF(LEX)THEN; IF(SHP%POL(I)%IACT.EQ.0)CYCLE; ENDIF

    SHP%POL(I)%XMIN=MINVAL(SHP%POL(I)%X(1:SHP%POL(I)%N))
    SHP%POL(I)%XMAX=MAXVAL(SHP%POL(I)%X(1:SHP%POL(I)%N))
    SHP%POL(I)%YMIN=MINVAL(SHP%POL(I)%Y(1:SHP%POL(I)%N))
    SHP%POL(I)%YMAX=MAXVAL(SHP%POL(I)%Y(1:SHP%POL(I)%N))

    SHP%XMIN=MIN(SHP%POL(I)%XMIN,SHP%XMIN)
    SHP%XMAX=MAX(SHP%POL(I)%XMAX,SHP%XMAX)
    SHP%YMIN=MIN(SHP%POL(I)%YMIN,SHP%YMIN)
    SHP%YMAX=MAX(SHP%POL(I)%YMAX,SHP%YMAX)

   ENDDO
  
   WRITE(IU) SHP%XMIN,SHP%YMIN,SHP%XMAX,SHP%YMAX
   MAXCOL=0; IF(ASSOCIATED(SHP%COLNAMES))MAXCOL=SIZE(SHP%COLNAMES)
   
   IF(LEX)THEN
    J=0; DO I=1,SHP%NPOL; IF(SHP%POL(I)%IACT.EQ.1)J=J+1; ENDDO
    WRITE(IU) J       ,MAXCOL
   ELSE
    WRITE(IU) SHP%NPOL,MAXCOL
   ENDIF
   
   IF(MAXCOL.GT.0)THEN
    WRITE(IU) SHP%LWIDTH
    WRITE(IU) SHP%COLNAMES
   ENDIF
   DO I=1,SHP%NPOL

    IF(LEX)THEN; IF(SHP%POL(I)%IACT.EQ.0)CYCLE; ENDIF

    WRITE(IU) SHP%POL(I)%N,SHP%POL(I)%ITYPE
    IF(MAXCOL.GT.0)WRITE(IU) (SHP%POL(I)%LBL(J)%STRING,J=1,MAXCOL)
    WRITE(IU) SHP%POL(I)%XMIN,SHP%POL(I)%YMIN,SHP%POL(I)%XMAX,SHP%POL(I)%YMAX
    WRITE(IU) (SHP%POL(I)%X(J),SHP%POL(I)%Y(J),J=1,SHP%POL(I)%N)
   ENDDO
  
  !## ascii gen file
  ELSEIF(FTYPE.EQ.2)THEN
  
   IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='REPLACE',ACTION='WRITE,DENYREAD',FORM='FORMATTED',IOSTAT=IOS)
   IF(IOS.NE.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open file for writing:'//CHAR(13)//'['//TRIM(FNAME)//']','Error')
    RETURN
   ENDIF  

   MAXCOL=0; IF(ASSOCIATED(SHP%COLNAMES))MAXCOL=SIZE(SHP%COLNAMES)

   JU=0; IF(MAXCOL.GT.0)THEN
    JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.DAT',STATUS='REPLACE',ACTION='WRITE,DENYREAD',FORM='FORMATTED',IOSTAT=IOS)
    IF(IOS.NE.0)THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open file for writing:'//CHAR(13)//'['//FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.DAT'//']','Error')
     RETURN
    ENDIF  
   ENDIF
   
   ALLOCATE(STR(MAXCOL))

   DO I=1,SHP%NPOL
    
    IF(LEX)THEN; IF(SHP%POL(I)%IACT.EQ.0)CYCLE; ENDIF
    
    DO II=1,MAXCOL
     STR(II)%STRING=''
     DO JJ=1,SHP%LWIDTH(II)
      STR(II)%STRING(JJ:JJ)=SHP%POL(I)%LBL(II)%STRING(JJ)
     ENDDO
    ENDDO

    WRITE(IU,'(I10)') I
    IF(JU.GT.0)WRITE(JU,'(99A)') ('"'//TRIM(STR(J)%STRING)//'",',J=1,MAXCOL-1),'"'//TRIM(STR(MAXCOL)%STRING)//'"'

    IF(SHP%POL(I)%ITYPE.EQ.ID_LINE.OR.SHP%POL(I)%ITYPE.EQ.ID_POLYGON)THEN

    DO J=1,SHP%POL(I)%N
      WRITE(IU,'(2(F15.3,A1))') SHP%POL(I)%X(J),',',SHP%POL(I)%Y(J)
     ENDDO
     WRITE(IU,'(A3)') 'END'

    ELSEIF(SHP%POL(I)%ITYPE.EQ.ID_RECTANGLE)THEN

     WRITE(IU,'(2(F15.3,A1))') SHP%POL(I)%X(1),',',SHP%POL(I)%Y(1)
     WRITE(IU,'(2(F15.3,A1))') SHP%POL(I)%X(2),',',SHP%POL(I)%Y(1)
     WRITE(IU,'(2(F15.3,A1))') SHP%POL(I)%X(2),',',SHP%POL(I)%Y(2)
     WRITE(IU,'(2(F15.3,A1))') SHP%POL(I)%X(1),',',SHP%POL(I)%Y(2)
     WRITE(IU,'(A3)') 'END'

    ENDIF

   ENDDO
   
   WRITE(IU,'(A3)') 'END'
   IF(JU.GT.0)CLOSE(JU)
   
   DEALLOCATE(STR)

  !## ipf file
  ELSEIF(FTYPE.EQ.3)THEN
  
   IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='REPLACE',ACTION='WRITE,DENYREAD',FORM='FORMATTED',IOSTAT=IOS)
   IF(IOS.NE.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open file for writing:'//CHAR(13)//'['//TRIM(FNAME)//']','Error')
    RETURN
   ENDIF  

   N=0
   DO K=1,2
    MAXCOL=0; IF(ASSOCIATED(SHP%COLNAMES))MAXCOL=SIZE(SHP%COLNAMES)
    
    IF(K.EQ.1)THEN
     ALLOCATE(STR(MAXCOL))
    ENDIF
    
    DO I=1,SHP%NPOL
     IF(SHP%POL(I)%ITYPE.EQ.ID_POINT)THEN

      IF(K.EQ.2)THEN
       DO II=1,MAXCOL
        STR(II)%STRING=''
        DO JJ=1,SHP%LWIDTH(II)
         STR(II)%STRING(JJ:JJ)=SHP%POL(I)%LBL(II)%STRING(JJ)
        ENDDO
       ENDDO
      ENDIF
      
      DO J=1,SHP%POL(I)%N
       N=N+1
       IF(K.EQ.2)THEN
        WRITE(IU,'(F15.3,A1,F15.3,99A)') SHP%POL(I)%X(J),',',SHP%POL(I)%Y(J),(','//TRIM(STR(II)%STRING),II=1,MAXCOL)
       ENDIF
      ENDDO
     ENDIF
    ENDDO
    IF(K.EQ.1)THEN
     WRITE(IU,'(I10)') N
     WRITE(IU,'(I10)') 2+MAXCOL
     WRITE(IU,'(A)') 'X'
     WRITE(IU,'(A)') 'Y'
     DO J=1,MAXCOL; WRITE(IU,'(A)') TRIM(SHP%COLNAMES(J)); ENDDO
     WRITE(IU,'(A)') '0,TXT'
    ENDIF
   ENDDO

   DEALLOCATE(STR)
   
  ENDIF
  
  CLOSE(IU)

 ENDIF

 END SUBROUTINE POLYGON1SAVELOADSHAPE

 !###======================================================================
 SUBROUTINE POLYGON1INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL POLYGON1CLOSE()
 ALLOCATE(SHP%POL(MAXSHAPES))

 !## allocate first column - required to have at least a single column
 DO I=1,MAXSHAPES
  SHP%POL(I)%N=0
  NULLIFY(SHP%POL(I)%X)
  NULLIFY(SHP%POL(I)%Y)
  NULLIFY(SHP%POL(I)%CX)
  NULLIFY(SHP%POL(I)%CY)
  NULLIFY(SHP%POL(I)%LBL)
 ENDDO 

 ALLOCATE(SHP%COLNAMES(1),SHP%LWIDTH(1))
 SHP%COLNAMES(1)='ID         '
 SHP%LWIDTH(1)=11

 !## no label used
 SHP%NLBL=0
 !## default first column selected for gridding
 SHP%ILBL=1
 !## no polygons active
 SHP%NPOL =0

 END SUBROUTINE POLYGON1INIT

 !###======================================================================
 SUBROUTINE POLYGON1ALLOCATEXY(IPOL,M)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPOL,M
 INTEGER,PARAMETER :: NN=10
 INTEGER :: N,I
 
 !## associated, see whether large enough
 IF(ASSOCIATED(SHP%POL(IPOL)%X))THEN

  N=SIZE(SHP%POL(IPOL)%X)
  !## too small, increase memory
  IF(N.LT.M)THEN
   ALLOCATE(SHP%POL(IPOL)%CX(M+NN),SHP%POL(IPOL)%CY(M+NN))
   DO I=1,N
    SHP%POL(IPOL)%CX(I)=SHP%POL(IPOL)%X(I); SHP%POL(IPOL)%CY(I)=SHP%POL(IPOL)%Y(I)
   ENDDO
   DEALLOCATE(SHP%POL(IPOL)%X,SHP%POL(IPOL)%Y)
   SHP%POL(IPOL)%X=>SHP%POL(IPOL)%CX
   SHP%POL(IPOL)%Y=>SHP%POL(IPOL)%CY
  ENDIF
  
 !## not associated, allocate memory
 ELSE

  N=M; ALLOCATE(SHP%POL(IPOL)%X(N),SHP%POL(IPOL)%Y(N))

 ENDIF
  
 END SUBROUTINE POLYGON1ALLOCATEXY

 !###======================================================================
 SUBROUTINE POLYGON1DEALLOCATE_SELIDF()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(SELIDF))THEN
  CALL IDFDEALLOCATE(SELIDF,SIZE(SELIDF))
  DEALLOCATE(SELIDF)
 ENDIF

 END SUBROUTINE POLYGON1DEALLOCATE_SELIDF

 !###======================================================================
 SUBROUTINE POLYGON1CLOSE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 IF(ALLOCATED(SHP%POL))THEN
  DO I=1,SIZE(SHP%POL)
   IF(ASSOCIATED(SHP%POL(I)%X))  DEALLOCATE(SHP%POL(I)%X)
   IF(ASSOCIATED(SHP%POL(I)%Y))  DEALLOCATE(SHP%POL(I)%Y)
   IF(ASSOCIATED(SHP%LWIDTH))    DEALLOCATE(SHP%LWIDTH)
   IF(ASSOCIATED(SHP%COLNAMES))  DEALLOCATE(SHP%COLNAMES)
   IF(ASSOCIATED(SHP%POL(I)%LBL))DEALLOCATE(SHP%POL(I)%LBL)
  ENDDO
  DEALLOCATE(SHP%POL)
 ENDIF
 
 CALL POLYGON1DEALLOCATE_SELIDF()
 CALL UTL_GENLABELSDEALLOCATE()
 SHP%NPOL=0

 END SUBROUTINE POLYGON1CLOSE

 !###======================================================================
 SUBROUTINE POLYGON1FIELDS(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER :: I

 SHP%NPOL=MAX(0,SHP%NPOL)

 CALL WDIALOGSELECT(ID)

 IF(SHP%NPOL.GT.0)THEN
  CALL WDIALOGGETMENU(IDF_MENU1,SHP%POL%IACT)
 ELSE
  SHP%POL%IACT=0
 ENDIF

 I=0
 IF(SUM(SHP%POL(1:SHP%NPOL)%IACT).GT.0)I=1
 CALL WDIALOGFIELDSTATE(ID_SAVESHAPE,I)
 CALL WDIALOGFIELDSTATE(ID_DELETE,I)
 CALL WDIALOGFIELDSTATE(ID_ZOOMSELECT,I)
 I=0
 IF(SUM(SHP%POL(1:SHP%NPOL)%IACT).EQ.1)I=1
 CALL WDIALOGFIELDSTATE(ID_RENAME,I)

 END SUBROUTINE POLYGON1FIELDS
 
 !###======================================================================
 SUBROUTINE POLYGON1IMAGES(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID

 CALL WDIALOGSELECT(ID)
 CALL WDIALOGPUTIMAGE(ID_SAVESHAPE,ID_ICONSAVEAS)
 CALL WDIALOGPUTIMAGE(ID_LOADSHAPE,ID_ICONOPEN)
 CALL WDIALOGPUTIMAGE(ID_DRAW,ID_ICONDRAW)
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE)
 CALL WDIALOGPUTIMAGE(ID_RENAME,ID_ICONRENAME)
 CALL WDIALOGPUTIMAGE(ID_ZOOMSELECT,ID_ICONZOOMFULL)

 END SUBROUTINE POLYGON1IMAGES

END MODULE MOD_POLYGON_UTL

