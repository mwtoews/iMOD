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
MODULE MOD_IPFANALYSE

USE WINTERACTER
USE RESOURCE
USE MOD_DBL
USE MOD_COLOURS
USE MODPLOT
USE MOD_UTL, ONLY : UTL_JDATETOIDATE,ITOS,RTOS,UTL_CAP,UTL_GETUNIT,UTL_GDATE,FTIMETOCTIME
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_IPFASSFILE, ONLY : ASSF,IPFPLOTASSFILE,IPFINITASSFILE,IPFDIMENSIONASSFILE, &
   IPFDRAWITOPIC2_ICLR,AXES
USE MOD_IPFASSFILE_UTL
USE MOD_PROFILE_UTL, ONLY : GRAPHUNITS,GRAPHAREA
USE MOD_IPF_PAR

CONTAINS

 !###====================================================================
 SUBROUTINE IPFANALYSE_MAIN()
 !###====================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,ICLOSE,I,IIPF,J,NP,NA

 !## remember settings
 J=3
 IF(WMENUGETSTATE(ID_SERIESIMPLE,2).EQ.1)  J=1
 IF(WMENUGETSTATE(ID_SERIEADVANCED,2).EQ.1)J=2
 !## select all "opened associated files" to be analysed
 NP=0
 NA=0
 DO IIPF=1,NIPF
  IF(IPF(IIPF)%ACOL.NE.0)THEN
   NA=NA+1
   DO I=1,IPF(IIPF)%NROW
    IF(IPF(IIPF)%IP(I).NE.INT(0,1))THEN
     IPF(IIPF)%IP(I)=INT(3,1)
     NP=NP+1
    ENDIF
   END DO
  ENDIF
 ENDDO

 IF(NA.EQ.0)THEN; CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No associated files available','Warning'); RETURN; ENDIF
 IF(NP.EQ.0)THEN; CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No points selected','Warning'); RETURN; ENDIF

 CALL IPFANALYSE_INIT()
 CALL IPFANALYSE_GETLIST()
 CALL IPFANALYSE_PLOTINIT()
 CALL IPFANALYSE_PLOT(0,1,0)
 CALL IPFANALYSE_FILLGRID()

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  CALL IPFANALYSE_DIALOG(ITYPE,MESSAGE,ICLOSE,0)
  IF(ICLOSE.EQ.1)EXIT
 ENDDO

 CALL IPFANALYSE_CLOSE()

 !## reselect all "opened associatd files" to be analysed
 DO IIPF=1,NIPF
  DO I=1,IPF(IIPF)%NROW
   IF(IPF(IIPF)%IP(I).EQ.INT(3,1))IPF(IIPF)%IP(I)=INT(J,1)
  END DO
 ENDDO

 END SUBROUTINE IPFANALYSE_MAIN

 !###====================================================================
 SUBROUTINE IPFANALYSE_DIALOG(ITYPE,MESSAGE,ICLOSE,IPROF)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE,IPROF
 INTEGER,INTENT(OUT) :: ICLOSE
 TYPE(WIN_MESSAGE),INTENT(INOUT) :: MESSAGE
 INTEGER :: IASSF,I,IY,IM,ID
 CHARACTER(LEN=10) :: CDATE
 REAL(KIND=DP_KIND) :: X,Y

 ICLOSE=0
 SELECT CASE(ITYPE)

  CASE(CLOSEREQUEST)
   ICLOSE=1
   
  CASE (MENUSELECT)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (ID_PRINT)

    CASE (ID_QUIT)
     ICLOSE=1
    CASE (ID_COPY)
     CALL WCLIPBOARDPUTBITMAP(IBITMAP)
    CASE (ID_ZOOMFULL)
     CALL IPFANALYSE_PLOTINIT()
     CALL IPFANALYSE_PLOT(0,1,IPROF)
    CASE (ID_REDRAW)
     CALL IPFANALYSE_PLOT(1,1,IPROF)
    CASE (ID_ZOOMWINDOW,ID_ZOOMIN,ID_ZOOMOUT,ID_MOVE)
     CALL IPFANALYSE_ZOOM(MESSAGE%VALUE1)
     CALL IPFANALYSE_PLOT(0,1,0)
    CASE (ID_CONTLINES,ID_BLOCKLINES)
     CALL WMENUSETSTATE(ID_CONTLINES,2,0)
     CALL WMENUSETSTATE(ID_BLOCKLINES,2,0)
     CALL WMENUSETSTATE(MESSAGE%VALUE1,2,1)
     CALL IPFANALYSE_PLOTINIT()
     CALL IPFANALYSE_PLOT(0,1,IPROF)
    CASE (ID_MARKDATA)
     I=WMENUGETSTATE(MESSAGE%VALUE1,2)
     I=ABS(I-1)
     CALL WMENUSETSTATE(MESSAGE%VALUE1,2,I)
     CALL IPFANALYSE_PLOTINIT()
     CALL IPFANALYSE_PLOT(0,1,IPROF)
   END SELECT

  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%WIN)
    CASE (ID_DIPFINFOSERIE)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (IDF_MENU1,IDF_CHECK1)
       CALL IPFANALYSE_PLOTINIT()
       CALL IPFANALYSE_PLOT(0,1,IPROF)
       CALL IPFANALYSE_FILLGRID()
      CASE (IDF_CHECK2)
       CALL WDIALOGSELECT(ID_DIPFINFOSERIE)
       CALL WDIALOGGETCHECKBOX(IDF_CHECK2,GRAPHLINESXAXES)     
       CALL WDIALOGFIELDSTATE(IDF_REAL1,GRAPHLINESXAXES);  CALL WDIALOGFIELDSTATE(IDF_REAL2,GRAPHLINESXAXES)
       CALL WDIALOGFIELDSTATE(IDF_LABEL2,GRAPHLINESXAXES); CALL WDIALOGFIELDSTATE(IDF_LABEL3,GRAPHLINESXAXES)
      CASE (IDF_CHECK3)
       CALL WDIALOGSELECT(ID_DIPFINFOSERIE)
       CALL WDIALOGGETCHECKBOX(IDF_CHECK3,GRAPHLINESYAXES)     
       CALL WDIALOGFIELDSTATE(IDF_REAL3,GRAPHLINESYAXES);  CALL WDIALOGFIELDSTATE(IDF_REAL4,GRAPHLINESYAXES)
       CALL WDIALOGFIELDSTATE(IDF_LABEL4,GRAPHLINESYAXES); CALL WDIALOGFIELDSTATE(IDF_LABEL5,GRAPHLINESYAXES)
     END SELECT
    CASE (ID_DIPFINFOSERIEGRID)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (IDF_MENU1)
       CALL IPFANALYSE_FILLGRID_SETTINGS()
       CALL IPFANALYSE_PLOTINIT()
       CALL IPFANALYSE_PLOT(0,1,IPROF)
      CASE (IDF_CHECK1,IDF_CHECK3,IDF_CHECK4)
       CALL IPFANALYSE_FILLGRID_SETTINGSONOFF()
       CALL IPFANALYSE_PLOTINIT()
       CALL IPFANALYSE_PLOT(0,1,IPROF)
      CASE (IDF_RADIO1,IDF_RADIO2)
       CALL IPFANALYSE_FILLGRID_SETTINGSTHICKNESS()
       CALL IPFANALYSE_PLOTINIT()
       CALL IPFANALYSE_PLOT(0,1,IPROF)       
     END SELECT  
     SELECT CASE (MESSAGE%VALUE2)
      CASE (IDF_INTEGER1)
       CALL IPFANALYSE_FILLGRID_SETTINGSCOLOUR()
       CALL IPFANALYSE_PLOTINIT()
       CALL IPFANALYSE_PLOT(0,1,IPROF)      
     END SELECT
   END SELECT

  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
     CASE (IDCANCEL)
      ICLOSE=1
      IF(ITOPICASSF.EQ.3)THEN
       CALL WDIALOGSELECT(ID_DIPFINFOSERIEGRID)
       CALL WDIALOGGETMENU(IDF_MENU1,I) 
       CALL WDIALOGGETDOUBLE(IDF_REAL1,SCALED(I))
       IF(SCALED(I).GT.99.9)THEN
        CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,"Multiplying by more than a factor > 99.9 is a little too much don't you think?",'Error')
        ICLOSE=0
       ENDIF
      ENDIF
     CASE (ID_REDRAW)
      CALL IPFANALYSE_FILLGRID_SCALED()
      CALL IPFANALYSE_PLOTINIT()
      CALL IPFANALYSE_PLOT(0,1,IPROF)
     CASE (ID_RESTORE)
      CALL IPFANALYSE_FILLGRID_SCALEDRESET()
      CALL IPFANALYSE_PLOTINIT()
      CALL IPFANALYSE_PLOT(0,1,IPROF)      
     CASE (ID_SCALEALL) 
      CALL IPFANALYSE_FILLGRID_SCALEDALL()
      CALL IPFANALYSE_PLOTINIT()
      CALL IPFANALYSE_PLOT(0,1,IPROF)      
    END SELECT

  CASE(MOUSEMOVE)
   IF(MESSAGE%WIN.EQ.MPW%IWIN)THEN
    CALL WCURSORSHAPE(CURARROW)
   ELSE
    CALL WCURSORSHAPE(CURCROSSHAIR)
    CALL WINDOWSELECT(IWIN)
    IF(NPOS.GT.0)THEN
     X=DBLE(MESSAGE%GX)
     Y=DBLE(MESSAGE%GY)
     CALL IPFANALYSE_GETXY(X,Y,IASSF)
     IF(IASSF.NE.0)THEN
      IF(ASSF(IASSF)%ITOPIC.EQ.1)THEN
       CALL UTL_GDATE(INT(X),IY,IM,ID)
       CDATE=TRIM(ITOS(ID))//'/'//TRIM(ITOS(IM))//'/'//TRIM(ITOS(IY))
       CALL WINDOWOUTSTATUSBAR(1,'X:'//TRIM(CDATE)//', Y:'//TRIM(RTOS(Y,'F',3))//'m')
      ELSE
       CALL WINDOWOUTSTATUSBAR(1,'X:'//TRIM(RTOS(X,'F',3))//', Y:'//TRIM(RTOS(Y,'F',3))//'m')
      ENDIF
      CALL WINDOWOUTSTATUSBAR(2,'Selected '//TRIM(CNAMESEL(IASSF)))
     ELSE
      CALL WINDOWOUTSTATUSBAR(1,'X:'//TRIM(RTOS(X,'F',3))//', Y:'//TRIM(RTOS(Y,'F',3))//'m')
     ENDIF
    ENDIF
   ENDIF

  CASE (EXPOSE,RESIZE)
   CALL IPFANALYSE_PLOTINIT()
   CALL IPFANALYSE_PLOT(0,1,IPROF)

 END SELECT

 END SUBROUTINE IPFANALYSE_DIALOG

 !###====================================================================
 SUBROUTINE IPFANALYSE_INIT()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: MAXNROW,IIPF,I

 CALL WINDOWOPENCHILD(IWIN,SYSMENUON+MINBUTTON+MAXBUTTON+STATUSBAR+OWNEDBYPARENT, &
                      MENUID=ID_MENU7,TOOLID=(/ID_TOOLBAR2,0,0,0/),DIALOGID=ID_DIPFINFOSERIE,   &
                      TITLE='IPF Analyse Figure')
 CALL WINDOWSTATUSBARPARTS(2,(/6000,-1/))
 CALL WDIALOGSELECT(ID_DIPFINFOSERIE)
 I=0; IF(GRAPHLINESXAXES.EQ.1)I=1; CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,I)
 CALL WDIALOGFIELDSTATE(IDF_REAL1,I);  CALL WDIALOGFIELDSTATE(IDF_REAL2,I)
 CALL WDIALOGFIELDSTATE(IDF_LABEL2,I); CALL WDIALOGFIELDSTATE(IDF_LABEL3,I)
 I=0; IF(GRAPHLINESYAXES.EQ.1)I=1; CALL WDIALOGPUTCHECKBOX(IDF_CHECK3,I)
 CALL WDIALOGFIELDSTATE(IDF_REAL3,I);  CALL WDIALOGFIELDSTATE(IDF_REAL4,I)
 CALL WDIALOGFIELDSTATE(IDF_LABEL4,I); CALL WDIALOGFIELDSTATE(IDF_LABEL5,I)
 CALL WDIALOGPUTDOUBLE(IDF_REAL1,GRAPHLINESXMIN); CALL WDIALOGPUTDOUBLE(IDF_REAL2,GRAPHLINESXMAX)
 CALL WDIALOGPUTDOUBLE(IDF_REAL3,GRAPHLINESYMIN); CALL WDIALOGPUTDOUBLE(IDF_REAL4,GRAPHLINESYMAX)

 CALL WDIALOGLOAD(ID_DIPFINFOSERIEGRID,ID_DIPFINFOSERIEGRID)
 CALL WDIALOGSHOW(-1,-1,0,2)
 
 CALL WINDOWSELECT(IWIN)
 CALL WMENUSETSTATE(ID_PRINT,1,0)
 CALL WMENUSETSTATE(ID_SAVEAS,1,0)
 CALL WMENUSETSTATE(ID_SELECT,1,0)
! CALL WMENUSETSTATE(ID_2DPLOT,1,0)
 CALL WMENUSETSTATE(ID_BITMAP,1,0)

 MAXNROW=0
 DO IIPF=1,NIPF
  IF(IIPF.LE.MXTAB)THEN
   CALL WDIALOGSELECT(IDTAB(IIPF))
  ENDIF
  MAXNROW=MAX(MAXNROW,(WINFOGRID(IDF_GRID1,GRIDROWSMAX)))
 ENDDO
 
 ALLOCATE(IREF(MAXNROW),JREF(MAXNROW),CNAME(MAXNROW),ILIST(MAXNROW))
 IF(ALLOCATED(GRAPHAREA)) DEALLOCATE(GRAPHAREA)
 IF(ALLOCATED(GRAPHUNITS))DEALLOCATE(GRAPHUNITS)
 ALLOCATE(GRAPHAREA(4,1),GRAPHUNITS(6,1))
 GRAPHUNITS(1,1)=0.0D0
 GRAPHUNITS(2,1)=0.0D0
 GRAPHUNITS(3,1)=1.0D0
 GRAPHUNITS(4,1)=1.0D0
 GRAPHUNITS(5,1)=0.0D0
 GRAPHUNITS(6,1)=1.0D0
 GRAPHAREA(1,1) =0.0D0
 GRAPHAREA(2,1) =0.0D0
 GRAPHAREA(3,1) =1.0D0
 GRAPHAREA(4,1) =1.0D0
 
 IBITMAP=0

! IF(.NOT.ALLOCATED(GRAPHLINESONOFF))THEN
!  ALLOCATE(GRAPHLINESONOFF(GRAPHMAXLINES)); GRAPHLINESONOFF=1
! ENDIF
! IF(.NOT.ALLOCATED(GRAPHLINESCOLOUR))THEN
!  ALLOCATE(GRAPHLINESCOLOUR(GRAPHMAXLINES))
!  DO I=1,SIZE(GRAPHLINESCOLOUR); GRAPHLINESCOLOUR(I)=ICOLOR(I); ENDDO
!  GRAPHLINESCOLOUR(2)=WRGB(0,0,0) !## CONUS (zwart,dikker)
!  GRAPHLINESCOLOUR(3)=WRGB(255,255,0) !## KLEEF (geel)
!  GRAPHLINESCOLOUR(4)=WRGB(200,0,0) !## WRIJVINGSGETAL (rood)
!  GRAPHLINESCOLOUR(5)=WRGB(0,0,220) !## SPANNING (blauw)
! ENDIF
! IF(.NOT.ALLOCATED(GRAPHLINESTHICKNESS))THEN
!  ALLOCATE(GRAPHLINESTHICKNESS(GRAPHMAXLINES)); GRAPHLINESTHICKNESS=1
!  GRAPHLINESTHICKNESS(2)=2
! ENDIF
  
 END SUBROUTINE IPFANALYSE_INIT

 !###====================================================================
 SUBROUTINE IPFANALYSE_INIT_GRAPHVARIABLES()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I
  
 IF(.NOT.ALLOCATED(GRAPHLINESONOFF))THEN
  ALLOCATE(GRAPHLINESONOFF(GRAPHMAXLINES)); GRAPHLINESONOFF=1
 ENDIF
 IF(.NOT.ALLOCATED(GRAPHLINESCOLOUR))THEN
  ALLOCATE(GRAPHLINESCOLOUR(GRAPHMAXLINES))
  DO I=1,SIZE(GRAPHLINESCOLOUR); GRAPHLINESCOLOUR(I)=ICOLOR(I); ENDDO

  GRAPHLINESCOLOUR(2)=WRGB(255,0,0)   !## red
  GRAPHLINESCOLOUR(3)=WRGB(0,255,0)   !## green
  GRAPHLINESCOLOUR(4)=WRGB(0,0,255)   !## blue
  GRAPHLINESCOLOUR(5)=WRGB(0,255,255) !## ?

 ENDIF
 IF(.NOT.ALLOCATED(GRAPHLINESTHICKNESS))THEN
  ALLOCATE(GRAPHLINESTHICKNESS(GRAPHMAXLINES)); GRAPHLINESTHICKNESS=1
  GRAPHLINESTHICKNESS(2)=1 
 ENDIF
 IF(.NOT.ALLOCATED(GRAPHLINESSCALED))THEN
  ALLOCATE(GRAPHLINESSCALED(GRAPHMAXLINES)); GRAPHLINESSCALED=0
 ENDIF
 IF(.NOT.ALLOCATED(SCALED))THEN
  ALLOCATE(SCALED(GRAPHMAXLINES)); SCALED=1.0D0
 ENDIF
 
 END SUBROUTINE IPFANALYSE_INIT_GRAPHVARIABLES
 
 !###====================================================================
 SUBROUTINE IPFANALYSE_GETLIST()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,IIPF

 NLIST=0
 DO IIPF=1,NIPF
  DO I=1,IPF(IIPF)%NROW
   IF(IPF(IIPF)%IP(I).EQ.INT(3,1))THEN
    IF(IPF(IIPF)%ACOL.GT.0)THEN
     K=NLIST
     K=K+1
     NLIST=K
     IREF(NLIST) =I
     JREF(NLIST) =IIPF
     J           =INDEX(IPF(IIPF)%FNAME,'\',.TRUE.)+1
     CNAME(NLIST)=TRIM(IPF(IIPF)%INFO(IPF(IIPF)%ACOL,I))//' ['//TRIM(UTL_CAP(IPF(IIPF)%FNAME(J:),'L'))//']'
    ENDIF
   ENDIF
  END DO
 ENDDO
 ILIST=0
 ILIST(1:NLIST)=1
 CALL WDIALOGSELECT(ID_DIPFINFOSERIE)
 CALL WDIALOGPUTMENU(IDF_MENU1,CNAME,NLIST,ILIST)

 END SUBROUTINE IPFANALYSE_GETLIST

 !###====================================================================
 SUBROUTINE IPFANALYSE_CLOSE()
 !###====================================================================
 IMPLICIT NONE

 IF(ALLOCATED(IREF))DEALLOCATE(IREF)
 IF(ALLOCATED(JREF))DEALLOCATE(JREF)
 IF(ALLOCATED(CNAME))DEALLOCATE(CNAME)
 IF(ALLOCATED(ILIST))DEALLOCATE(ILIST)
 IF(ALLOCATED(CNAMESEL))DEALLOCATE(CNAMESEL)
 IF(ALLOCATED(GRAPHAREA))DEALLOCATE(GRAPHAREA)
 IF(ALLOCATED(GRAPHUNITS))DEALLOCATE(GRAPHUNITS)
 
 !## deallocate memory associated files
 CALL IPFCLOSEASSFILE()

 CALL WDIALOGSELECT(ID_DIPFINFOSERIEGRID); CALL WDIALOGUNLOAD()
 CALL WINDOWCLOSECHILD(IWIN)
 CALL IGRSELECT(DRAWWIN,0)
 CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
 CALL DBL_IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX,IOFFSET=1)
 CALL IGRPALETTEINIT()

 CALL WCURSORSHAPE(CURARROW)

 END SUBROUTINE IPFANALYSE_CLOSE

 !###====================================================================
 SUBROUTINE IPFANALYSE_GETXY(X,Y,IASSF)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(INOUT) :: X,Y
 INTEGER,INTENT(OUT) :: IASSF
 REAL(KIND=DP_KIND) :: DX,DY
 INTEGER :: I

 IASSF=0; IF(.NOT.ALLOCATED(ASSF))RETURN
 
 DO I=1,NPOS
  IF(X.GE.XPOS(I,1).AND.X.LE.XPOS(I,3).AND. &
     Y.GE.XPOS(I,2).AND.Y.LE.XPOS(I,4))THEN
   DX   =(X-XPOS(I,1))*(1.0D0/(XPOS(I,3)-XPOS(I,1)))
   DY   =(Y-XPOS(I,2))*(1.0D0/(XPOS(I,4)-XPOS(I,2)))
   IASSF=I
   X    =ASSF(IASSF)%XMIN+(DX*(ASSF(IASSF)%XMAX-ASSF(IASSF)%XMIN))
   Y    =ASSF(IASSF)%YMIN+(DY*(ASSF(IASSF)%YMAX-ASSF(IASSF)%YMIN))
   EXIT
  ENDIF
 ENDDO

 END SUBROUTINE

 !###====================================================================
 SUBROUTINE IPFANALYSE_PLOTINIT()
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256) :: DIR
 INTEGER :: I,J,IIPF,IX,IY,K,IFRAME

 CALL WDIALOGSELECT(ID_DIPFINFOSERIE)
 CALL WDIALOGGETMENU(IDF_MENU1,ILIST)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IFRAME)

 !## (de)allocate memory associated files
 CALL IPFINITASSFILE()

 I=1
 IF(SUM(ILIST(1:NLIST)).EQ.0)I=0
 CALL WINDOWSELECT(IWIN)
 CALL WMENUSETSTATE(ID_ZOOMIN,1,I)
 CALL WMENUSETSTATE(ID_COPY,1,I)
 CALL WMENUSETSTATE(ID_ZOOMOUT,1,I)
 CALL WMENUSETSTATE(ID_ZOOMWINDOW,1,I)
 CALL WMENUSETSTATE(ID_ZOOMFULL,1,I)
 CALL WMENUSETSTATE(ID_MOVE,1,I)

 CALL WINDOWOUTSTATUSBAR(1,'')

 IF(SUM(ILIST).EQ.0)THEN
  CALL WINDOWOUTSTATUSBAR(2,'No ipf selected to be drawn')
  NPOS=0
  RETURN
 ENDIF

 CALL WINDOWOUTSTATUSBAR(2,'')

 NX=SUM(ILIST(1:NLIST))
 NY=1

 IF(ALLOCATED(XPOS))DEALLOCATE(XPOS)
 IF(ALLOCATED(ASSF))DEALLOCATE(ASSF)
 IF(ALLOCATED(CNAMESEL))DEALLOCATE(CNAMESEL)
 NPOS=NX*NY
 ALLOCATE(XPOS(NPOS,4),CNAMESEL(NPOS))
 CALL IPFASSFILEALLOCATE(NPOS)

 !## get dimensions off figures
 IY  =1
 IX  =0
 NPOS=0
 DO I=1,NLIST
  IF(ILIST(I).EQ.1)THEN

   !## position current figure
   IF(IFRAME.EQ.1)THEN
    IX=1
    IY=1
    NX=1
    NY=1
   ELSE
    IX=IX+1
    IF(IX.GT.NX)THEN
     IY=IY+1
     IX=1
    ENDIF
   ENDIF

   IIPF=JREF(I)
   J   =IREF(I)

   K   =INDEXNOCASE(IPF(IIPF)%FNAME,'\',.TRUE.)
   DIR =IPF(IIPF)%FNAME(1:K-1)
   DIR =TRIM(DIR)//'\'//TRIM(IPF(IIPF)%INFO(IPF(IIPF)%ACOL,J))//'.'//TRIM(ADJUSTL(IPF(IIPF)%FEXT))
   NPOS=NPOS+1
   ASSF(NPOS)%FNAME=TRIM(IPF(IIPF)%INFO(IPF(IIPF)%ACOL,J))//'.'//TRIM(ADJUSTL(IPF(IIPF)%FEXT))
   CALL IPFDIMENSIONASSFILE(NPOS,DIR,IPF(IIPF)%IAXES)!,SIZE(IPF(IIPF)%IAXES))
   ASSF(NPOS)%ASSCOL1=IPF(IIPF)%ASSCOL1 !## column used with dlf
   ASSF(NPOS)%ASSCOL2=IPF(IIPF)%ASSCOL2 !## on default not used --- border rings
   ASSF(NPOS)%ILEGDLF=IPF(IIPF)%ILEGDLF !## legend to be used for colouring
   XPOS(NPOS,1)=REAL(IX-1)*1.0D0/NX
   XPOS(NPOS,3)=REAL(IX)*1.0D0/NX
   XPOS(NPOS,2)=REAL(IY-1)*1.0D0/NY
   XPOS(NPOS,4)=REAL(IY)*1.0D0/NY
   CNAMESEL(NPOS)=CNAME(I)
  ENDIF
 END DO

 END SUBROUTINE IPFANALYSE_PLOTINIT

 !###====================================================================
 SUBROUTINE IPFANALYSE_PLOT(IZOOM,IWINDOW,IPROF)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IZOOM,IWINDOW,IPROF
 INTEGER :: PLOTSTYLE,IMARKDATA,IW,IH
 INTEGER :: JBITMAP,IWID,IHGT,IPOS,IFRAME
 INTEGER,DIMENSION(:),ALLOCATABLE :: ICOPYCLR

 !## define bitmap on size of picture-area
 CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
 IW=WINFODRAWABLE(DRAWABLEWIDTH)
 IH=WINFODRAWABLE(DRAWABLEHEIGHT)
 IF(IBITMAP.NE.0)CALL WBITMAPDESTROY(IBITMAP)
 CALL WBITMAPCREATE(IBITMAP,IW,IH)

 IF(NPOS.GT.0)THEN

  CALL WDIALOGSELECT(ID_DIPFINFOSERIE)
  IMARKDATA=WMENUGETSTATE(ID_MARKDATA,2)
  IF(WMENUGETSTATE(ID_CONTLINES,2).EQ.1) PLOTSTYLE=1
  IF(WMENUGETSTATE(ID_BLOCKLINES,2).EQ.1)PLOTSTYLE=2
  CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IFRAME)

  !## create temporary bitmaps
  IWID=IW/NX
  IHGT=IH/NY

  !## based upon drills/sonderingen
  CALL IPFANALYSE_ADJUSTAXES((/2,3/),2)
  !## based upon measures
  CALL IPFANALYSE_ADJUSTAXES((/1/),1)

  !## change plotmode
  CALL IGRPLOTMODE(MODECOPY)

  CALL WBITMAPCREATE(JBITMAP,IWID,IHGT)
  CALL WBITMAPPLOTMODE(MODEAND)

  ALLOCATE(ICOPYCLR(SIZE(ICOLOR)))
  ICOPYCLR=ICOLOR
  
  DO IPOS=1,NPOS

   ICOLOR(1)=ICOPYCLR(MIN(SIZE(ICOLOR),IPOS))

   CALL IGRSELECT(DRAWBITMAP,JBITMAP)

   CALL IPFPLOTASSFILE(0.0D0,0.0D0,1.0D0,IPOS,ABS(INT(3,1)),PLOTSTYLE,0.0D0,1.0D0,0.0D0,1.0D0,IMARKDATA,.FALSE., & 
               0.0D0,1.0D0,0.0D0,1.0D0,IZOOM,(IPOS-1)*IFRAME,0.0D0,1,(/0.0D0,0.0D0,0.0D0,0.0D0/),0.0D0,0,0.0D0,0.0D0,IPROF)

   CALL IGRSELECT(DRAWBITMAP,IBITMAP)
   CALL DBL_IGRAREA(XPOS(IPOS,1),XPOS(IPOS,2),XPOS(IPOS,3),XPOS(IPOS,4))
   CALL WBITMAPPUT(JBITMAP,2,1)
   CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)

  END DO
  
  ICOLOR=ICOPYCLR
  DEALLOCATE(ICOPYCLR)
  
  CALL WBITMAPDESTROY(JBITMAP)
  CALL WBITMAPPLOTMODE(MODECOPY)

 ELSE

  CALL IGRSELECT(DRAWBITMAP,IBITMAP)
  CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
  CALL DBL_IGRUNITS(0.0D0,0.0D0,1.0D0,1.0D0)
  CALL IGRAREACLEAR()

 ENDIF

 !## put entire bitmap for each box
 CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
 CALL WBITMAPPUT(IBITMAP,0,1)

 !## main graphics window
 IF(IWINDOW.EQ.0)THEN
  CALL IGRSELECT(DRAWWIN,MPW%IWIN)
  CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
  CALL DBL_IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX,IOFFSET=1)
 !## analyse window
 ELSEIF(IWINDOW.EQ.1)THEN
  CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
  CALL DBL_IGRUNITS(0.0D0,0.0D0,1.0D0,1.0D0)
 ENDIF

 END SUBROUTINE IPFANALYSE_PLOT

 !###====================================================================
 SUBROUTINE IPFANALYSE_ADJUSTAXES(ITOPIC,NTOPIC)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NTOPIC
 INTEGER,DIMENSION(NTOPIC),INTENT(IN) :: ITOPIC
 REAL(KIND=DP_KIND) :: AX1,AX2,AY1,AY2
 INTEGER :: IPOS,I

 !## adjust scale to fit similar type
 AX1= HUGE(1.0D0) 
 AX2=-HUGE(1.0D0) 
 AY1= HUGE(1.0D0) 
 AY2=-HUGE(1.0D0) 
 DO IPOS=1,NPOS
  IF(ASSF(IPOS)%NRASS.LE.0)CYCLE
  DO I=1,NTOPIC
   IF(ITOPIC(I).EQ.ASSF(IPOS)%ITOPIC)THEN
    AX1=MIN(AX1,ASSF(IPOS)%XMIN)
    AX2=MAX(AX2,ASSF(IPOS)%XMAX)
    AY1=MIN(AY1,ASSF(IPOS)%YMIN)
    AY2=MAX(AY2,ASSF(IPOS)%YMAX)
   ENDIF
  END DO
 ENDDO
 DO IPOS=1,NPOS
  IF(ASSF(IPOS)%NRASS.LE.0)CYCLE
  DO I=1,NTOPIC
   IF(ITOPIC(I).EQ.ASSF(IPOS)%ITOPIC)THEN
    ASSF(IPOS)%XMIN=AX1
    ASSF(IPOS)%YMIN=AY1
    ASSF(IPOS)%XMAX=AX2
    ASSF(IPOS)%YMAX=AY2
   ENDIF
  ENDDO
 ENDDO

 END SUBROUTINE IPFANALYSE_ADJUSTAXES

 !###====================================================================
 SUBROUTINE IPFANALYSE_FILLGRID() 
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: MAXNROW,IASSF,I,J,ICLRCOL,ICLR,IOS 
 REAL(KIND=DP_KIND) :: IWIDTH
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IC,WC
 REAL(KIND=DP_KIND) :: FTIME
 
 CALL WDIALOGSELECT(ID_DIPFINFOSERIEGRID)
 CALL WDIALOGPUTIMAGE(ID_REDRAW,ID_ICONREDRAW)
 CALL WDIALOGPUTIMAGE(ID_RESTORE,ID_ICONUNDO)
 CALL WDIALOGPUTIMAGE(ID_SCALEALL,ID_ICONSCALEALL)
 
 IASSF=1

 !## plot content of txt-file
 IF(SUM(ILIST).EQ.1)THEN
  IF(ASSF(IASSF)%NRASS.GT.0)THEN

   CALL WDIALOGCLEARFIELD(IDF_GRID1)
   !## define number of rows
   MAXNROW=WINFOGRID(IDF_GRID1,GRIDROWSMAX)
   CALL WGRIDSETCELL(IDF_GRID1,1,1)
   CALL WGRIDROWS(IDF_GRID1,MIN(MAXNROW,ASSF(IASSF)%NRASS))
   CALL WDIALOGFIELDSTATE(IDF_GRID1,1)
   CALL WDIALOGFIELDSTATE(IDF_LABEL2,1)

   ALLOCATE(IC(ASSF(IASSF)%NCASS),WC(ASSF(IASSF)%NCASS))
   IC=1
   SELECT CASE (ASSF(IASSF)%ITOPIC)
    CASE (2)
     WC=25
     WC(1)=50
     DO J=2,ASSF(IASSF)%NCASS
      DO I=1,MIN(MAXNROW,ASSF(IASSF)%NRASS)
       WC(J)=MAX(WC(J),5*LEN_TRIM(ASSF(IASSF)%L(J-1,I)))
      ENDDO   
     ENDDO
    CASE DEFAULT
     WC=40
   END SELECT

   CALL WGRIDCOLUMNS(IDF_GRID1,ASSF(IASSF)%NCASS,IC,WC)

   DEALLOCATE(IC,WC)
   DO I=1,ASSF(IASSF)%NCASS
    CALL WGRIDLABELCOLUMN(IDF_GRID1,I,ASSF(IASSF)%ATTRIB(I))
   ENDDO
   CALL WDIALOGCLEARFIELD(IDF_MENU1)
   CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
   CALL WDIALOGFIELDSTATE(IDF_CHECK1,1)
   CALL WDIALOGFIELDSTATE(IDF_RADIO1,1)
   CALL WDIALOGFIELDSTATE(IDF_RADIO2,1)
   CALL WDIALOGFIELDSTATE(IDF_INTEGER1,1)
   CALL WDIALOGFIELDSTATE(IDF_GROUP1,1)
   CALL WDIALOGPUTMENU(IDF_MENU1,ASSF(IASSF)%ATTRIB,ASSF(IASSF)%NCASS,1)

   CALL IPFANALYSE_FILLGRID_SETTINGS()
   
   !## itopic
   ITOPICASSF=ASSF(IASSF)%ITOPIC
      
   SELECT CASE (ASSF(IASSF)%ITOPIC)
    !## measures
    CASE (1)
     DO I=1,MIN(MAXNROW,ASSF(IASSF)%NRASS)
      DO J=1,ASSF(IASSF)%NCASS
       IF(J.EQ.1)THEN
        WRITE(TXT,'(I8)') UTL_JDATETOIDATE(INT(ASSF(IASSF)%IDATE(I)))
        FTIME=ASSF(IASSF)%IDATE(I)-FLOOR(ASSF(IASSF)%IDATE(I))
        IF(FTIME.NE.0.0D0)THEN
         CALL FTIMETOCTIME(FTIME,CTIME)
         TXT=TRIM(TXT)//' '//TRIM(CTIME)
        ENDIF
       ELSE
        WRITE(TXT,'(F15.3)',IOSTAT=IOS) ASSF(IASSF)%MEASURE(J-1,I)
        IF(IOS.NE.0)TXT='NaN'
        TXT=ADJUSTL(TXT)
       ENDIF
       CALL WGRIDPUTCELLSTRING(IDF_GRID1,J,I,TXT)
      END DO
     END DO
    
    !## boreholes
    CASE (2)
     ICLRCOL=ASSF(IASSF)%ASSCOL1
     IF(ICLRCOL.LT.0.OR.ICLRCOL.GT.SIZE(ASSF(IASSF)%L,1))ICLRCOL=0

     DO I=1,MIN(MAXNROW,ASSF(IASSF)%NRASS)
      IF(ICLRCOL.NE.0)THEN
       CALL IPFDRAWITOPIC2_ICLR(I,IASSF,ICLR,IWIDTH)
       CALL WGRIDCOLOURCELL(IDF_GRID1,ICLRCOL,I,-1,ICLR)
      ENDIF
      DO J=1,ASSF(IASSF)%NCASS
       !## vertical coordinates
       IF(J.EQ.1)THEN
        CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,I,TRIM(RTOS(ASSF(IASSF)%Z(I),'F',3))) 
       ELSE
        CALL WGRIDPUTCELLSTRING(IDF_GRID1,J,I,TRIM(ASSF(IASSF)%L(J-1,I)))
       ENDIF
      ENDDO
     END DO

    !## sonderingen
    CASE (3)
     DO I=1,MIN(MAXNROW,ASSF(IASSF)%NRASS)
      DO J=1,ASSF(IASSF)%NCASS
       !## wrijvingsgetal
       WRITE(TXT,'(F15.3)') ASSF(IASSF)%MEASURE(J,I)
       TXT=ADJUSTL(TXT)
       CALL WGRIDPUTCELLSTRING(IDF_GRID1,J,I,TXT) 
      END DO
     END DO

   END SELECT

   IF(ASSF(IASSF)%NRASS.GT.MAXNROW)THEN
    CALL WDIALOGPUTSTRING(IDF_LABEL2,'Displayed only '//TRIM(ITOS(MAXNROW))// &
              ' records out of total '//TRIM(ITOS(ASSF(IASSF)%NRASS))//' records')
   ELSE
    CALL WDIALOGPUTSTRING(IDF_LABEL2,'All Data for above selected file')
   ENDIF

  ELSE
   CALL WDIALOGCLEARFIELD(IDF_GRID1)
   CALL WDIALOGFIELDSTATE(IDF_GRID1,3)
   CALL WDIALOGFIELDSTATE(IDF_LABEL2,1)
   CALL WDIALOGPUTSTRING(IDF_LABEL2,'Selected file does not contain any record')
   CALL WDIALOGCLEARFIELD(IDF_MENU1)
   CALL WDIALOGFIELDSTATE(IDF_MENU1,3)
   CALL WDIALOGFIELDSTATE(IDF_CHECK1,3)
   CALL WDIALOGFIELDSTATE(IDF_CHECK3,3)
   CALL WDIALOGFIELDSTATE(IDF_CHECK4,3)
   CALL WDIALOGFIELDSTATE(IDF_REAL1,3)
   CALL WDIALOGFIELDSTATE(ID_REDRAW,3)
   CALL WDIALOGFIELDSTATE(ID_RESTORE,3)
   CALL WDIALOGFIELDSTATE(ID_SCALEALL,3)
   CALL WDIALOGFIELDSTATE(IDF_RADIO1,3)
   CALL WDIALOGFIELDSTATE(IDF_RADIO2,3)
!   CALL WDIALOGFIELDSTATE(IDF_STRING1,3)
   CALL WDIALOGFIELDSTATE(IDF_INTEGER1,3)
   CALL WDIALOGFIELDSTATE(IDF_GROUP1,3)
  ENDIF
 ELSE
  CALL WDIALOGCLEARFIELD(IDF_GRID1)
  CALL WDIALOGFIELDSTATE(IDF_GRID1,3)
  CALL WDIALOGFIELDSTATE(IDF_LABEL2,1)
  CALL WDIALOGPUTSTRING(IDF_LABEL2,'Select a single file above to display records')
  CALL WDIALOGCLEARFIELD(IDF_MENU1)
  CALL WDIALOGFIELDSTATE(IDF_MENU1,3)
  CALL WDIALOGFIELDSTATE(IDF_RADIO1,3)
  CALL WDIALOGFIELDSTATE(IDF_RADIO2,3)
  CALL WDIALOGFIELDSTATE(IDF_CHECK1,3)
  CALL WDIALOGFIELDSTATE(IDF_CHECK3,3)
  CALL WDIALOGFIELDSTATE(IDF_CHECK4,3)
  CALL WDIALOGFIELDSTATE(IDF_REAL1,3)
  CALL WDIALOGFIELDSTATE(ID_REDRAW,3)
  CALL WDIALOGFIELDSTATE(ID_RESTORE,3)
  CALL WDIALOGFIELDSTATE(ID_SCALEALL,3)
  CALL WDIALOGFIELDSTATE(IDF_INTEGER1,3)
  CALL WDIALOGFIELDSTATE(IDF_GROUP1,3)
 ENDIF

 END SUBROUTINE IPFANALYSE_FILLGRID

! !###==================================================================== 
! SUBROUTINE IPFANALYSE_FILLGRID_UPDATE(IASSF)
! !###====================================================================
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: IASSF
! INTEGER :: I,J
! CHARACTER(LEN=MAXLEN) :: TXT
! 
! CALL WDIALOGSELECT(ID_DIPFINFOSERIEGRID)
! DO I=2,ASSF(IASSF)%NCASS
!  DO J=1,ASSF(IASSF)%NRASS
!   CALL WGRIDCLEARCELL(IDF_GRID1,I,J) 
!   WRITE(TXT,'(F15.3)') ASSF(IASSF)%MEASURE(I,J)
!   TXT=ADJUSTL(TXT)
!   CALL WGRIDPUTCELLSTRING(IDF_GRID1,I,J,TXT) 
!  ENDDO
! ENDDO
! 
! END SUBROUTINE IPFANALYSE_FILLGRID_UPDATE
 
 !###====================================================================
 SUBROUTINE IPFANALYSE_FILLGRID_SETTINGS()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WDIALOGSELECT(ID_DIPFINFOSERIEGRID)
 CALL WDIALOGGETMENU(IDF_MENU1,I)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,GRAPHLINESCOLOUR(I))   
 IF(I.EQ.1)THEN
  CALL WDIALOGFIELDSTATE(IDF_CHECK1,0)
  CALL WDIALOGFIELDSTATE(IDF_INTEGER1,3)
  CALL WDIALOGFIELDSTATE(IDF_RADIO1,3)
  CALL WDIALOGFIELDSTATE(IDF_RADIO2,3)
  CALL WDIALOGFIELDSTATE(IDF_CHECK3,1)
  CALL WDIALOGFIELDSTATE(IDF_CHECK4,0)
  CALL WDIALOGFIELDSTATE(IDF_REAL1,0)
 ELSE
  CALL WDIALOGFIELDSTATE(IDF_CHECK1,1)
  CALL WDIALOGFIELDSTATE(IDF_INTEGER1,1)
  CALL WDIALOGFIELDSTATE(IDF_RADIO1,1)
  CALL WDIALOGFIELDSTATE(IDF_RADIO2,1)
  CALL WDIALOGFIELDSTATE(IDF_CHECK3,1)
  CALL WDIALOGFIELDSTATE(IDF_CHECK4,1)
  CALL WDIALOGFIELDSTATE(IDF_REAL1,GRAPHLINESSCALED(I))
 ENDIF
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,GRAPHLINESONOFF(I))   
 CALL WDIALOGCOLOUR(IDF_INTEGER1,GRAPHLINESCOLOUR(I),GRAPHLINESCOLOUR(I))
 IF(GRAPHLINESTHICKNESS(I).EQ.1)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO1)   
 IF(GRAPHLINESTHICKNESS(I).EQ.2)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO2)   
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK3,GRAPHLINESSCALE)
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK4,GRAPHLINESSCALED(I))
 CALL WDIALOGPUTDOUBLE(IDF_REAL1,SCALED(I))
 
 END SUBROUTINE IPFANALYSE_FILLGRID_SETTINGS

 !###====================================================================
 SUBROUTINE IPFANALYSE_FILLGRID_SETTINGSONOFF()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WDIALOGSELECT(ID_DIPFINFOSERIEGRID)
 CALL WDIALOGGETMENU(IDF_MENU1,I)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,GRAPHLINESONOFF(I))
 CALL WDIALOGGETCHECKBOX(IDF_CHECK3,GRAPHLINESSCALE)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK4,GRAPHLINESSCALED(I))
 CALL WDIALOGFIELDSTATE(IDF_REAL1,GRAPHLINESSCALED(I))
 
 END SUBROUTINE IPFANALYSE_FILLGRID_SETTINGSONOFF
 
 !###====================================================================
 SUBROUTINE IPFANALYSE_FILLGRID_SETTINGSCOLOUR()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,IRGB

 CALL WDIALOGSELECT(ID_DIPFINFOSERIEGRID)
 CALL WDIALOGGETMENU(IDF_MENU1,I)
 CALL WDIALOGGETINTEGER(IDF_INTEGER1,IRGB)
 CALL WSELECTCOLOUR(IRGB); IF(WINFODIALOG(4).NE.1)RETURN
 GRAPHLINESCOLOUR(I)=IRGB
 CALL WDIALOGCOLOUR(IDF_INTEGER1,GRAPHLINESCOLOUR(I),GRAPHLINESCOLOUR(I))

 END SUBROUTINE IPFANALYSE_FILLGRID_SETTINGSCOLOUR
 
 !###====================================================================
 SUBROUTINE IPFANALYSE_FILLGRID_SCALED()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WDIALOGSELECT(ID_DIPFINFOSERIEGRID)
 CALL WDIALOGGETMENU(IDF_MENU1,I)
 CALL WDIALOGGETDOUBLE(IDF_REAL1,SCALED(I))
 
 END SUBROUTINE IPFANALYSE_FILLGRID_SCALED
 
 !###====================================================================
 SUBROUTINE IPFANALYSE_FILLGRID_SCALEDALL()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WDIALOGSELECT(ID_DIPFINFOSERIEGRID)
 CALL WDIALOGGETMENU(IDF_MENU1,I)
 CALL WDIALOGGETDOUBLE(IDF_REAL1,SCALED(I))
 SCALED=SCALED(I)
 
 END SUBROUTINE IPFANALYSE_FILLGRID_SCALEDALL 

 !###====================================================================
 SUBROUTINE IPFANALYSE_FILLGRID_SCALEDRESET()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WDIALOGSELECT(ID_DIPFINFOSERIEGRID)
 CALL WDIALOGGETMENU(IDF_MENU1,I)
 SCALED=1.0D0
 CALL WDIALOGPUTDOUBLE(IDF_REAL1,SCALED(I))
 
 END SUBROUTINE IPFANALYSE_FILLGRID_SCALEDRESET  
 
 !###====================================================================
 SUBROUTINE IPFANALYSE_FILLGRID_SETTINGSTHICKNESS()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DIPFINFOSERIEGRID)
 CALL WDIALOGGETMENU(IDF_MENU1,I)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,GRAPHLINESTHICKNESS(I))

 END SUBROUTINE IPFANALYSE_FILLGRID_SETTINGSTHICKNESS 

 !###====================================================================
 SUBROUTINE IPFANALYSE_ZOOM(IDZ)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: FZIN =0.75
 REAL(KIND=DP_KIND),PARAMETER :: FZOUT=1.5
 INTEGER,INTENT(IN) :: IDZ
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,IDOWN,IDCURSOR,IY,IM,ID,IASSF
 REAL(KIND=DP_KIND) :: FZ,XC1,YC1,XC2,YC2,XC3,YC3,DX,DY,X,Y
 LOGICAL :: LEX
 CHARACTER(LEN=10) :: CDATE

 CALL WINDOWSELECT(IWIN)
 CALL WMENUSETSTATE(ID_ZOOMIN,1,0)
 CALL WMENUSETSTATE(ID_ZOOMOUT,1,0)
 CALL WMENUSETSTATE(ID_ZOOMWINDOW,1,0)
 CALL WMENUSETSTATE(ID_ZOOMFULL,1,0)
 CALL WMENUSETSTATE(ID_MOVE,1,0)
! CALL WMENUSETSTATE(ID_PRINT,1,0)
 CALL WMENUSETSTATE(ID_COPY,1,0)

 IF(IDZ.EQ.ID_ZOOMIN)THEN
  FZ=FZIN
  IDCURSOR=ID_CURSORPOINTPLUS
  CALL WMENUSETSTATE(ID_ZOOMIN,2,1)
  CALL WINDOWOUTSTATUSBAR(2,'Click your left-mouse button to zoom in, use your right-mouse to exit')
 ELSEIF(IDZ.EQ.ID_ZOOMOUT)THEN
  FZ=FZOUT
  IDCURSOR=ID_CURSORPOINTMIN
  CALL WMENUSETSTATE(ID_ZOOMOUT,2,1)
  CALL WINDOWOUTSTATUSBAR(2,'Click your left-mouse button to zoom out, use your right-mouse to exit')
 ELSEIF(IDZ.EQ.ID_ZOOMWINDOW)THEN
  IDCURSOR=ID_CURSORZOOMRECTANGLE
  CALL WMENUSETSTATE(ID_ZOOMWINDOW,2,1)
  CALL WINDOWOUTSTATUSBAR(2,'Click your left-mouse button to zoom in rectangle')
  CALL IGRPLOTMODE(MODEXOR)
  CALL IGRCOLOURN(WRGB(255,255,255))
  CALL IGRFILLPATTERN(OUTLINE)
  CALL IGRLINETYPE(DASHED)
 ELSEIF(IDZ.EQ.ID_MOVE)THEN
  IDCURSOR=ID_CURSORHAND
  CALL WINDOWOUTSTATUSBAR(2,'Click your left mouse-button to move, use your right-mouse button to exit')
  CALL WMENUSETSTATE(ID_MOVE,2,1)
 ENDIF

 CALL WCURSORSHAPE(IDCURSOR)

 IDOWN=0
 LEX  =.FALSE.
 XC1  =0.0D0
 YC1  =0.0D0
 DO

  CALL WMESSAGE(ITYPE, MESSAGE)
  
  IF(MESSAGE%WIN.EQ.ID_DIPFINFOSERIE)THEN 

   SELECT CASE(ITYPE)

    CASE(MOUSEMOVE)

     X=DBLE(MESSAGE%GX); Y=DBLE(MESSAGE%GY) 
     CALL IPFANALYSE_GETXY(X,Y,IASSF)
     MESSAGE%GX=X; MESSAGE%GY=Y

     IF(IASSF.NE.0)THEN

      CALL WINDOWSELECT(IWIN)
      IF(IASSF.NE.0)THEN
       IF(ASSF(IASSF)%ITOPIC.EQ.1)THEN
        CALL UTL_GDATE(INT(MESSAGE%GX+AXES%XOFFSET),IY,IM,ID)
        CDATE=TRIM(ITOS(ID))//'/'//TRIM(ITOS(IM))//'/'//TRIM(ITOS(IY))
        CALL WINDOWOUTSTATUSBAR(1,'X:'//TRIM(CDATE)//', Y:'//TRIM(RTOS(REAL(MESSAGE%GY,8),'F',3))//'m')
       ELSE
        CALL WINDOWOUTSTATUSBAR(1,'X:'//TRIM(RTOS(DBLE(MESSAGE%GX),'F',3))//', Y:'//TRIM(RTOS(DBLE(MESSAGE%GY),'F',3))//'m')
       ENDIF
      ELSE
       CALL WINDOWOUTSTATUSBAR(1,'X:'//TRIM(RTOS(DBLE(MESSAGE%GX),'F',3))//', Y:'//TRIM(RTOS(DBLE(MESSAGE%GY),'F',3))//'m')
      ENDIF

      XC2=DBLE(MESSAGE%GX)
      YC2=DBLE(MESSAGE%GY)

      IF(IDZ.EQ.ID_MOVE)THEN
       IF(IDOWN.GT.0)THEN
        DX=XC1-XC2
        DY=YC1-YC2
        ASSF(IASSF)%XMAX=ASSF(IASSF)%XMAX+DX
        ASSF(IASSF)%XMIN=ASSF(IASSF)%XMIN+DX
        ASSF(IASSF)%YMAX=ASSF(IASSF)%YMAX+DY
        ASSF(IASSF)%YMIN=ASSF(IASSF)%YMIN+DY
        CALL IPFANALYSE_COPY(IASSF)
        CALL IPFANALYSE_PLOT(1,1,0)
       ENDIF
      ELSEIF(IDZ.EQ.ID_ZOOMWINDOW)THEN
       IF(IDOWN.GT.0)THEN

        CALL DBL_IGRAREA(XPOS(IDOWN,1),XPOS(IDOWN,2),XPOS(IDOWN,3),XPOS(IDOWN,4))
        CALL DBL_IGRUNITS(ASSF(IDOWN)%XMIN,ASSF(IDOWN)%YMIN,ASSF(IDOWN)%XMAX,ASSF(IDOWN)%YMAX)

        IF(LEX)CALL DBL_IGRRECTANGLE(XC1,YC1,XC3,YC3)
        LEX=.FALSE.

        IF(IDOWN.EQ.IASSF)THEN
         IF(XC1.NE.XC2.AND.YC1.NE.YC2)LEX=.TRUE.
         IF(LEX)CALL DBL_IGRRECTANGLE(XC1,YC1,XC2,YC2)
        ENDIF

        CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
        CALL DBL_IGRUNITS(0.0D0,0.0D0,1.0D0,1.0D0)

       ENDIF
      ENDIF

      XC3=XC2
      YC3=YC2

     ENDIF

    CASE (MOUSEBUTUP)
     IF(IDZ.EQ.ID_MOVE)THEN
      SELECT CASE (MESSAGE%VALUE1)
       CASE (1)
        CALL WCURSORSHAPE(ID_CURSORHAND)
        IDOWN=0
      END SELECT
     ENDIF

    CASE (MOUSEBUTDOWN)

     IF(IDZ.EQ.ID_ZOOMIN.OR.IDZ.EQ.ID_ZOOMOUT)THEN
      SELECT CASE (MESSAGE%VALUE1)
       CASE (1)
        IF(IASSF.GT.0)THEN
         XC2=(ASSF(IASSF)%XMAX+ASSF(IASSF)%XMIN)/2.0D0
         YC2=(ASSF(IASSF)%YMAX+ASSF(IASSF)%YMIN)/2.0D0
         DX =ASSF(IASSF)%XMAX-ASSF(IASSF)%XMIN
         DY =ASSF(IASSF)%YMAX-ASSF(IASSF)%YMIN
         ASSF(IASSF)%XMAX=XC2+0.5D0*DX*FZ
         ASSF(IASSF)%XMIN=XC2-0.5D0*DX*FZ
         ASSF(IASSF)%YMIN=YC2-0.5D0*DY*FZ
         ASSF(IASSF)%YMAX=YC2+0.5D0*DY*FZ
         CALL IPFANALYSE_COPY(IASSF)
         CALL IPFANALYSE_PLOT(1,1,0)
        ENDIF
       CASE (3)
        EXIT
      END SELECT
     ELSEIF(IDZ.EQ.ID_MOVE)THEN
      SELECT CASE (MESSAGE%VALUE1)
       CASE (1)
        IF(IDOWN.EQ.0.AND.IASSF.GT.0)THEN
         XC1=XC2
         YC1=YC2
         IDOWN=IASSF
         CALL WCURSORSHAPE(ID_CURSORHANDGREP)
        ENDIF
       CASE (3)
        EXIT
      END SELECT
     ELSEIF(IDZ.EQ.ID_ZOOMWINDOW)THEN
      SELECT CASE (MESSAGE%VALUE1)
       CASE (1)
        IF(IASSF.GT.0)THEN
         IF(IDOWN.EQ.0)THEN
          XC1=XC2
          YC1=YC2
          IDOWN=IASSF
         ELSEIF(IASSF.EQ.IDOWN)THEN
          ASSF(IASSF)%XMAX=MAX(XC1,XC3)
          ASSF(IASSF)%XMIN=MIN(XC1,XC3)
          ASSF(IASSF)%YMAX=MAX(YC1,YC3)
          ASSF(IASSF)%YMIN=MIN(YC1,YC3)
          EXIT
         ENDIF
        ENDIF
      END SELECT
     ENDIF

   END SELECT

  ENDIF
 ENDDO

 CALL WCURSORSHAPE(CURARROW)

 CALL WINDOWSELECT(IWIN)
 CALL WMENUSETSTATE(ID_ZOOMIN,1,1)
 CALL WMENUSETSTATE(ID_ZOOMOUT,1,1)
 CALL WMENUSETSTATE(ID_ZOOMWINDOW,1,1)
 CALL WMENUSETSTATE(ID_ZOOMFULL,1,1)
 CALL WMENUSETSTATE(ID_MOVE,1,1)
 !CALL WMENUSETSTATE(ID_PRINT,1,1)
 CALL WMENUSETSTATE(ID_COPY,1,1)

 IF(IDZ.EQ.ID_ZOOMIN)THEN
  CALL WMENUSETSTATE(ID_ZOOMIN,2,0)
 ELSEIF(IDZ.EQ.ID_ZOOMOUT)THEN
  CALL WMENUSETSTATE(ID_ZOOMOUT,2,0)
 ELSEIF(IDZ.EQ.ID_ZOOMWINDOW)THEN
  CALL WMENUSETSTATE(ID_ZOOMWINDOW,2,0)
  CALL IGRPLOTMODE(MODECOPY)
  CALL IGRLINETYPE(SOLIDLINE)
 ELSEIF(IDZ.EQ.ID_MOVE)THEN
  CALL WMENUSETSTATE(ID_MOVE,2,0)
 ENDIF

 CALL WINDOWOUTSTATUSBAR(2,'')

 CALL IPFANALYSE_COPY(IASSF)

 END SUBROUTINE IPFANALYSE_ZOOM

 !###====================================================================
 SUBROUTINE IPFANALYSE_COPY(IASSF)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IASSF
 INTEGER :: I

 IF(IASSF.EQ.0)RETURN

 !## copy all
 DO I=1,NPOS
  IF(ASSF(I)%ITOPIC.EQ.ASSF(IASSF)%ITOPIC.AND.I.NE.IASSF)THEN
   ASSF(I)%XMAX=ASSF(IASSF)%XMAX
   ASSF(I)%XMIN=ASSF(IASSF)%XMIN
   ASSF(I)%YMIN=ASSF(IASSF)%YMIN
   ASSF(I)%YMAX=ASSF(IASSF)%YMAX
  ENDIF
 ENDDO

 END SUBROUTINE IPFANALYSE_COPY

END MODULE MOD_IPFANALYSE
