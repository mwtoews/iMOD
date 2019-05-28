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
MODULE MOD_SPOINTS

USE WINTERACTER
USE RESOURCE
USE MOD_DBL
USE MOD_COLOURS
USE MODPLOT
USE MOD_POLYGON_PAR
USE MOD_POLYGON, ONLY : POLYGON1MAIN,POLYGON1CREATESHAPE
USE MOD_POLYGON_DRAW, ONLY : POLYGON1DRAWSHAPE
USE MOD_POLYGON_UTL, ONLY : POLYGON1CLOSE,POLYGON1INIT,POLYGON1FIELDS,POLYGON1IMAGES
USE IMODVAR, ONLY : DP_KIND,SP_KIND,IDIAGERROR,PI
USE MOD_SPOINTS_PAR
USE MOD_UTL, ONLY : ITOS,RTOS,UTL_GETUNIT,UTL_PLOT1BITMAP,UTL_PLOT2BITMAP,UTL_CREATEDIR,UTL_IMODFILLMENU, &
         UTL_WSELECTFILE,DBL_IGRINSIDEPOLYGON,UTL_GETHELP
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_SPOINTS_UTL
USE MOD_MAIN_UTL
USE MOD_IDFPLOT

CONTAINS

 !###======================================================================
 SUBROUTINE STARTP1_MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 CHARACTER(LEN=256) :: IDFNAME

 CALL WDIALOGSELECT(MESSAGE%WIN)
 SELECT CASE(ITYPE)

  CASE (TABCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
    CASE (ID_DSPTAB1)
     IF(.NOT.STARTP1_UTL_FIELDS_GETTAB2(1))THEN
      CALL WDIALOGSELECT(ID_DSPOINTS)
      CALL WDIALOGSETTAB(ID_DTAB,ID_DSPTAB2)
     ENDIF
    CASE (ID_DSPTAB2)
     CALL STARTP1_UTL_FIELDS_PUTTAB2()
     CALL STARTP1_UTL_FIELDS()
   END SELECT

  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%WIN)
    CASE (ID_DSPOINTS)
    CASE (ID_DSPTAB1)
     CALL POLYGON1MAIN(ITYPE,MESSAGE)
     CALL STARTP1_UTL_FIELDS()
    CASE (ID_DSPTAB2)
     SELECT CASE (MESSAGE%VALUE2)
      CASE (IDF_CHECK1)
       CALL STARTP1_UTL_FIELDS()
      CASE (IDF_CHECK2)
       CALL WDIALOGGETCHECKBOX(IDF_CHECK2,SPNT(SHPI)%ISNAP)
       CALL STARTP1_UTL_FIELDS()
     END SELECT
   END SELECT

  CASE(PUSHBUTTON)
   SELECT CASE (MESSAGE%WIN)
    CASE (ID_DSPOINTS)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_SHOW)
       IF(STARTP1_UTL_FIELDS_GETTAB2(0))THEN
        CALL STARTP1_SHOW()
       ELSE
        CALL WDIALOGSELECT(ID_DSPOINTS)
        CALL WDIALOGSETTAB(ID_DTAB,ID_DSPTAB2)
       ENDIF
      CASE (IDCANCEL)
       CALL STARTP1_UTL_CLOSE(1) 
       CALL IDFPLOTFAST(0)
      CASE (IDHELP)
       CALL UTL_GETHELP('5.10','TMO.DefStartP')
     END SELECT

    CASE (ID_DSPTAB1)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_DRAW)
       IACTSHAPES=(/1,1,1,1,1,1/)
       CALL POLYGON1CREATESHAPE(ID_DSPTAB1)
       CALL POLYGON1FIELDS(ID_DSPTAB1)
       !## set raduis for circle option
       IF(SHP%POL(SHP%NPOL)%ITYPE.EQ.ID_CIRCLE)THEN
        SPNT(SHP%NPOL)%IRADIUS=UTL_DIST(SHP%POL(SHP%NPOL)%X(1),SHP%POL(SHP%NPOL)%Y(1),SHP%POL(SHP%NPOL)%X(2),SHP%POL(SHP%NPOL)%Y(2))
       ENDIF
      CASE DEFAULT
       CALL POLYGON1MAIN(ITYPE,MESSAGE)
       IF(MESSAGE%VALUE1.EQ.ID_ZOOMSELECT)THEN
        CALL IDFZOOM(ID_DGOTOXY,0.0D0,0.0D0,0)
        CALL IDFPLOT(1)
       ENDIF
       !## load in a gen-file, might be necessary to increase spnt()
       IF(MESSAGE%VALUE1.EQ.ID_LOADSHAPE)THEN
        CALL STARTP1_ALLOCATE()
       ENDIF
     END SELECT
     CALL STARTP1_UTL_FIELDS()

    CASE (ID_DSPTAB2)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_OPEN1,ID_OPEN2,ID_OPEN3)
       IF(UTL_WSELECTFILE('iMOD IDF Files|*.idf|',&
                   LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,IDFNAME,&
                   'Load iMOD Map (*.idf)'))THEN
        IF(MESSAGE%VALUE1.EQ.ID_OPEN1)CALL WDIALOGPUTSTRING(IDF_STRING2,TRIM(IDFNAME))
        IF(MESSAGE%VALUE1.EQ.ID_OPEN2)CALL WDIALOGPUTSTRING(IDF_STRING3,TRIM(IDFNAME))
        IF(MESSAGE%VALUE1.EQ.ID_OPEN3)CALL WDIALOGPUTSTRING(IDF_STRING4,TRIM(IDFNAME))
       ENDIF
     END SELECT
   END SELECT

 END SELECT

 END SUBROUTINE STARTP1_MAIN

 !###======================================================================
 LOGICAL FUNCTION STARTP1_OPENSP()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,N
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE

 STARTP1_OPENSP=.FALSE.

 CALL WDIALOGLOAD(ID_DSCENNAME,ID_DSCENNAME)
 CALL UTL_IMODFILLMENU(IDF_MENU1,TRIM(PREFVAL(1))//'\STARTPOINTS','*.isd','F',N,0,0)
 CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
 CALL WDIALOGTITLE('Open/Create a StartPoint Definition')
 CALL WDIALOGPUTSTRING(IDOK,'Open / Create ...')
 CALL WDIALOGPUTSTRING(IDCANCEL,'Close')

 CALL WDIALOGPUTSTRING(IDF_GROUP1,'Select a StartPoint Definition, or Enter a new name')
 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDOK)
      CALL WDIALOGUNDEFINED(IVALUE=-999) 
      CALL UTL_DEBUGLEVEL(0)
      CALL WDIALOGGETMENU(IDF_MENU1,I,SDFFNAME)
      CALL UTL_DEBUGLEVEL(0)
      IF(I.GE.0)THEN
       IF(LEN_TRIM(SDFFNAME).NE.0)THEN
        I=INDEX(SDFFNAME,'.',.TRUE.)
        IF(I.EQ.0)THEN
         SDFFNAME=TRIM(SDFFNAME)//'.isd'
        ELSE
         SDFFNAME=TRIM(SDFFNAME(:I-1))//'.isd'
        ENDIF
        EXIT
       ELSE
        CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to select an existing ISD file or entering a non-existing one.','Error')
       ENDIF
      ELSE
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to select an existing ISD file or entering a non-existing one.','Error')
      ENDIF
     CASE (IDHELP)
       CALL UTL_GETHELP('5.10','TMO.DefStartP')
     CASE (IDCANCEL)
      EXIT
    END SELECT
  END SELECT
 ENDDO

 CALL WDIALOGUNLOAD()
 IF(MESSAGE%VALUE1.EQ.IDCANCEL)RETURN

 !## get folder name
 SDFFNAME=TRIM(PREFVAL(1))//'\STARTPOINTS\'//TRIM(SDFFNAME)
 I      =INDEXNOCASE(SDFFNAME,'\',.TRUE.)-1
 SPDIR  =SDFFNAME(:I)

 STARTP1_OPENSP=.TRUE.

 END FUNCTION STARTP1_OPENSP

 !###======================================================================
 SUBROUTINE STARTP1_SHOW()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,NOR
 REAL(KIND=DP_KIND) :: XC,YC,RADIUS,GRAD2RAD,DIFFX,DIFFY,TL,DL
 REAL(KIND=DP_KIND) :: XCMIN,YCMIN,XCMAX,YCMAX,OR
 
 !## refresh
 CALL IDFPLOTFAST(0)
 CALL UTL_PLOT1BITMAP()

 CALL IGRPLOTMODE(MODECOPY)
 CALL IGRFILLPATTERN(SOLID)
 CALL IGRLINETYPE(SOLIDLINE)

 CALL WDIALOGSELECT(ID_DSPTAB1)
 CALL WDIALOGGETMENU(IDF_MENU1,SHP%POL%IACT)
 
 CALL IGRFILLPATTERN(SOLID)
 CALL IGRLINETYPE(SOLIDLINE)

 DO I=1,SHP%NPOL
  IF(SHP%POL(I)%IACT.EQ.0)CYCLE
  CALL IGRCOLOURN(SHP%POL(I)%ICOLOR)

  RADIUS=SQRT((MPW%XMAX-MPW%XMIN)**2.0D0+(MPW%YMAX-MPW%YMIN)**2.0D0)/1000.0D0

  YCMIN=MINVAL(SHP%POL(I)%Y(1:SHP%POL(I)%N))
  YCMAX=MAXVAL(SHP%POL(I)%Y(1:SHP%POL(I)%N))
  XCMIN=MINVAL(SHP%POL(I)%X(1:SHP%POL(I)%N))
  XCMAX=MAXVAL(SHP%POL(I)%X(1:SHP%POL(I)%N))

  SELECT CASE (SHP%POL(I)%ITYPE)
   CASE (ID_POLYGON,ID_RECTANGLE)

    K=((YCMAX-YCMIN)/SPNT(I)%IDX)*((XCMAX-XCMIN)/SPNT(I)%IDY)
    IF(K.GT.10000)THEN
     CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to display '//TRIM(ITOS(K))//' points!','Question')
     IF(WINFODIALOG(4).NE.1)RETURN
    ENDIF

    YC=YCMAX
    DO
     YC=YC-SPNT(I)%IDY
     IF(YC.LT.YCMIN)EXIT
     XC=XCMIN
     DO
      XC=XC+SPNT(I)%IDX
      IF(XC.GT.XCMAX)EXIT
      IF(SHP%POL(I)%ITYPE.EQ.ID_POLYGON)THEN
       IF(DBL_IGRINSIDEPOLYGON(XC,YC,SHP%POL(I)%X,SHP%POL(I)%Y,SHP%POL(I)%N).EQ.1)THEN
        CALL DBL_IGRCIRCLE(XC,YC,RADIUS,IOFFSET=1)
       ENDIF
      ELSE
       IF(XC.GT.MIN(SHP%POL(I)%X(1),SHP%POL(I)%X(2)).AND. &
          XC.LT.MAX(SHP%POL(I)%X(1),SHP%POL(I)%X(2)).AND. &
          YC.GT.MIN(SHP%POL(I)%Y(1),SHP%POL(I)%Y(2)).AND. &
          YC.LT.MAX(SHP%POL(I)%Y(1),SHP%POL(I)%Y(2)))CALL DBL_IGRCIRCLE(XC,YC,RADIUS,IOFFSET=1)
      ENDIF
     ENDDO
    ENDDO

   CASE (ID_POINT)

    !## length between points on circle
    DIFFX=(2.0D0*PI*REAL(SPNT(I)%IRADIUS))/REAL(SPNT(I)%ISX)
    !## number of points on circle (minimal = 1)
    NOR  =INT(DIFFX)+1

    GRAD2RAD=360.0D0/(2.0D0*PI)
    DO J=1,SHP%POL(I)%N
     OR =360.0D0/NOR 
     DO K=1,NOR
      IF(K*OR.GT.360.0D0)EXIT
      DIFFX=SPNT(I)%IRADIUS*COS((K*OR)/GRAD2RAD)
      DIFFY=SPNT(I)%IRADIUS*SIN((K*OR)/GRAD2RAD)
      CALL DBL_IGRCIRCLE(SHP%POL(I)%X(J)+DIFFX,SHP%POL(I)%Y(J)+DIFFY,RADIUS,IOFFSET=1)
     END DO
    END DO

   CASE (ID_CIRCLE)

    SPNT(I)%IRADIUS=UTL_DIST(SHP%POL(I)%X(1),SHP%POL(I)%Y(1),SHP%POL(I)%X(2),SHP%POL(I)%Y(2))

    !## length between points on circle
    DIFFX=(2.0D0*PI*REAL(SPNT(I)%IRADIUS))/REAL(SPNT(I)%ISX)
    !## number of points on circle (minimal = 1)
    NOR  =INT(DIFFX)+1

    GRAD2RAD=360.0D0/(2.0D0*PI)
    DO J=1,1
     OR =360.0D0/NOR 
     DO K=1,NOR
      IF(K*OR.GT.360.0D0)EXIT
      DIFFX=SPNT(I)%IRADIUS*COS((K*OR)/GRAD2RAD)
      DIFFY=SPNT(I)%IRADIUS*SIN((K*OR)/GRAD2RAD)
      CALL DBL_IGRCIRCLE(SHP%POL(I)%X(J)+DIFFX,SHP%POL(I)%Y(J)+DIFFY,RADIUS,IOFFSET=1)
     END DO
    END DO

   CASE (ID_LINE)

!OPEN(99,FILE='D:\BENNIE.IPF',STATUS='UNKNOWN',ACTION='WRITE')
    CALL DBL_IGRCIRCLE(SHP%POL(I)%X(1),SHP%POL(I)%Y(1),RADIUS,IOFFSET=1)
    DL=0.0D0
    DO J=2,SHP%POL(I)%N
     DIFFX=SHP%POL(I)%X(J)-SHP%POL(I)%X(J-1)
     DIFFY=SHP%POL(I)%Y(J)-SHP%POL(I)%Y(J-1)
     TL   =SQRT(DIFFX**2.0D0+DIFFY**2.0D0)
     DIFFX=DIFFX/TL
     DIFFY=DIFFY/TL
     DO
      DL=DL+REAL(SPNT(I)%ISX)
      IF(DL.LE.TL)THEN
       CALL DBL_IGRCIRCLE(SHP%POL(I)%X(J-1)+DIFFX*DL,SHP%POL(I)%Y(J-1)+DIFFY*DL,RADIUS,IOFFSET=1)
!  WRITE(99,*) SHP%POL(I)%X(J-1)+DIFFX*DL,SHP%POL(I)%Y(J-1)+DIFFY*DL
      ELSE
       !## remaining part
       DL=DL-TL-REAL(SPNT(I)%ISX)
       EXIT
      ENDIF
     ENDDO
    ENDDO
!CLOSE(99)

  END SELECT

 ENDDO

 CALL UTL_PLOT2BITMAP()

 END SUBROUTINE STARTP1_SHOW
 
 !###======================================================================
 SUBROUTINE STARTP1_INIT()
 !###======================================================================
 IMPLICIT NONE

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_SPOINTS,2).EQ.1)THEN
  CALL STARTP1_UTL_CLOSE(1)
  CALL IDFPLOTFAST(0)
  RETURN
 ENDIF

 CALL MAIN_UTL_INACTMODULE(ID_SPOINTS)

 !## other module no closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_SPOINTS,2,1)

 IF(.NOT.IOSDIREXISTS(TRIM(PREFVAL(1))//'\STARTPOINTS'))CALL UTL_CREATEDIR(TRIM(PREFVAL(1))//'\STARTPOINTS')

 CALL POLYGON1INIT()
 CALL STARTP1_ALLOCATE()
 
 CALL WDIALOGLOAD(ID_DSPOINTS,ID_DSPOINTS)
 CALL POLYGON1IMAGES(ID_DSPTAB1)
 CALL POLYGON1FIELDS(ID_DSPTAB1)

 CALL WDIALOGSELECT(ID_DSPTAB2)
 CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONOPENIDF)
 CALL WDIALOGPUTIMAGE(ID_OPEN2,ID_ICONOPENIDF)
 CALL WDIALOGPUTIMAGE(ID_OPEN3,ID_ICONOPENIDF)
 CALL WDIALOGSPINNERSTEP(IDF_REAL1,1.0D0,10.0D0)
 CALL WDIALOGSPINNERSTEP(IDF_REAL3,1.0D0,10.0D0)
 CALL WDIALOGRANGEDOUBLE(IDF_REAL1,0.0D0,10000.0D0)
 CALL WDIALOGRANGEDOUBLE(IDF_REAL3,0.0D0,10000.0D0)

 CALL STARTP1_UTL_FIELDS()

 !## open scenario
 IF(STARTP1_OPENSP())THEN
  IF(STARTP1_UTL_SAVELOAD(0))THEN
   CALL IDFPLOTFAST(0)
   CALL WDIALOGSELECT(ID_DSPOINTS)
   CALL WDIALOGPUTSTRING(IDF_LABEL1,'StartPoint Definition: '//TRIM(SDFFNAME(INDEX(SDFFNAME,'\',.TRUE.)+1:)))
   CALL WDIALOGSHOW(-1,-1,0,2)
   RETURN
  ENDIF
 ENDIF

 CALL STARTP1_UTL_CLOSE(0)
 CALL IDFPLOTFAST(0)
 
 END SUBROUTINE STARTP1_INIT

 !###======================================================================
 SUBROUTINE STARTP1_ALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 
 IF(ALLOCATED(SPNT))DEALLOCATE(SPNT)

 ALLOCATE(SPNT(MAXSHAPES))

 SPNT%IDX=25.0D0
 SPNT%IDY=25.0D0
 SPNT%ISX=25.0D0
 SPNT%ISY=25.0D0
 SPNT%ISZ=1
 SPNT%IRADIUS=100
 SPNT%TOPIDF='1.0'
 SPNT%BOTIDF='0.0'
 SPNT%REFIDF=''
 SPNT%IREF=0

 END SUBROUTINE STARTP1_ALLOCATE

END MODULE MOD_SPOINTS

