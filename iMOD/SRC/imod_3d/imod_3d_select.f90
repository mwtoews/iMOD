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
MODULE MOD_3D_SELECT

USE IMODVAR, ONLY : IDIAGERROR
USE MOD_3D_PAR
USE MODPLOT, ONLY : MPW,MXMPLOT,MP
USE MOD_UTL, ONLY : UTL_MESSAGEHANDLE,UTL_CAP,INVERSECOLOUR,ITOS,RTOS,UTL_GETUNIT
USE MOD_IDF, ONLY : IDFREAD,IDFDEALLOCATE,IDFGETVAL,IDFREADPART,IDFIROWICOL,IDFGETLOC
USE MOD_IPFASSFILE, ONLY : IPFOPENASSFILE,IPFREADASSFILELABEL,IPFREADASSFILE,IPFCLOSEASSFILE,IPFDRAWITOPIC2_ICLR
USE MOD_IPF_PAR, ONLY : ASSF,IPF,NIPF,MAXLITHO,BH
USE MOD_3D_SETTINGS, ONLY : IMOD3D_SETTINGSALL,IMOD3D_SETTINGSMAIN
USE MOD_3D_DISPLAY, ONLY : IMOD3D_DISPLAY,IMOD3D_DISPLAY_IPF
USE MOD_3D_UTL, ONLY : IMOD3D_RETURNCOLOR,IMOD3D_SETCOLOR,IMOD3D_GETCOLOR
USE MOD_COLOURS, ONLY : ICOLOR
USE MOD_3D_PROCESS

CHARACTER(LEN=3),DIMENSION(:),ALLOCATABLE :: NANSTRING
CHARACTER(LEN=50),DIMENSION(:),ALLOCATABLE :: TXTCOLUMN
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),PRIVATE :: IDF_SAMPLE

CONTAINS

 !###======================================================================
 SUBROUTINE IMOD3D_SELECTOBJECT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER(KIND=GLINT) :: IPOSX,IPOSY
 INTEGER(KIND=GLINT),PARAMETER :: IDX=2,IDY=2  !## selection window
 INTEGER(KIND=GLSIZEI),PARAMETER :: NDX=IDX*2+1,NDY=IDY*2+1  !## selection window
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I,IRGB 
 REAL(KIND=GLFLOAT),DIMENSION(NDX*NDY*3) :: FRGB
 INTEGER :: IWHITE,IDOWN
 INTEGER,DIMENSION(4) :: ID
 DATA ID/ID_3DFILE,ID_EDIT,ID_3DOPTIONS,ID_3DVIEW/
 REAL(KIND=GLFLOAT),DIMENSION(3) :: GLCOLOR

 DO I=1,SIZE(ID); CALL WMENUSETSTATE(ID(I),1,0); END DO

 CALL WDIALOGLOAD(ID_D3DIPFINFO,ID_D3DIPFINFO)
! CALL WDIALOGSIZE(IHEIGHT=IWINHEIGHT)
 CALL WDIALOGPUTIMAGE(ID_SELECT,ID_ICONSELECTPOINT,1)

 !## set tab2 by default if available
 IF(NASSLIST.GT.0)CALL WDIALOGSETTAB(IDF_TAB1,ID_D3DIPFINFOTAB2)
 !## block tab2
 IF(NASSLIST.EQ.0)CALL WDIALOGTABSTATE(IDF_TAB1,ID_D3DIPFINFOTAB2,0)

 !## enable field contect check on!
 CALL WDIALOGSELECT(ID_D3DIPFINFOTAB2)
 CALL WDIALOGFIELDOPTIONS(IDF_GRID1,EDITFIELDCHANGED,1)

 !## fill in selected idf"s
 CALL WDIALOGSELECT(ID_D3DIPFINFOTAB1)
 CALL WDIALOGPUTIMAGE(ID_PROPERTIES,ID_ICONPROPERTIES,1)

 IF(NIDFLIST.GT.0)THEN
  CALL WGRIDROWS(IDF_GRID1,NIDFLIST)
  CALL WGRIDPUTSTRING(IDF_GRID1,1,IDFPLOT%ALIAS,NIDFLIST)
  ALLOCATE(NANSTRING(NIDFLIST))
  NANSTRING='NaN'
  CALL WGRIDPUTSTRING(IDF_GRID1,2,NANSTRING,NIDFLIST)
  IF(ALLOCATED(IDF_SAMPLE))DEALLOCATE(IDF_SAMPLE)
  ALLOCATE(IDF_SAMPLE(NIDFLIST))
  DO I=1,NIDFLIST; CALL IDFNULLIFY(IDF_SAMPLE(I)); ENDDO
  DO I=1,NIDFLIST; IF(.NOT.IDFREAD(IDF_SAMPLE(I),IDFPLOT(I)%FNAME,0))EXIT; ENDDO
 ELSE
  CALL WDIALOGFIELDSTATE(IDF_GRID1,2)
 ENDIF

 IWHITE=WRGB(255,255,255)

 JSELECTED=0
 ISELECTED=0

 !## selection mode default activated!
 IDOWN=1
 CALL WCURSORSHAPE(ID_CURSORIDFVALUE)
 CALL IMOD3D_SELECTOBJECT_BACK(IWHITE)
 IMOUSEMOVE=1
 
 CALL WDIALOGSELECT(ID_D3DIPFINFO)
 CALL WDIALOGSHOW(IXPOS=IWINWIDTH,IYPOS=0,IFIELD=0,ITYPE=2)

 CALL WINDOWSELECT(IWIN)
 CALL WDIALOGSELECT(ID_D3DSETTINGS)
 
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)

  SELECT CASE (ITYPE)

   CASE (KEYDOWN)
    CALL IMOD3D_PROCESSKEYS(MESSAGE%VALUE1)

   CASE (MOUSEBUTDOWN,MOUSEBUTUP)
    IF(IDOWN.EQ.0)THEN
     CALL IMOD3D_PROCESSMOUSEBUTTON(ITYPE,MESSAGE%VALUE1,MESSAGE%X,MESSAGE%Y)
    ELSE
     SELECT CASE (MESSAGE%VALUE1)
      !## left mouse button pressed - freeze selecting, stop selecting
      CASE (1)
       IDOWN=0 
       CALL WCURSORSHAPE(CURARROW)
       CALL IMOD3D_SELECTOBJECT_FRONT()
       IMOUSEMOVE=0
      !## right mouse button pressed - QUIT selecting
      CASE (3)
     END SELECT
    ENDIF

   CASE (MOUSEMOVE)
    IF(IDOWN.EQ.0)THEN
     CALL IMOD3D_PROCESSMOUSEMOVE(MESSAGE%X,MESSAGE%Y)
    ELSE
!    CALL WINDOWUNITSTOPIXELS(X_WINT,Y_WINT,IX,IY)
     CALL WINDOWUNITSTOPIXELS(MESSAGE%X,MESSAGE%Y,IPOSX,IPOSY)
write(*,'(99(i8,1x))') iposx,iposy,MESSAGE%X,MESSAGE%Y,WINFODIALOGFIELD(IDF_PICTURE2,FIELDHEIGHT), &
WINFODIALOGFIELD(IDF_PICTURE2,FIELDwidth), &
WINFODIALOGFIELD(IDF_PICTURE2,FieldXPos), &
WINFODIALOGFIELD(IDF_PICTURE2,FieldyPos),WInfoDrawable(DrawableHeight ),wInfoDrawable(DrawableWidth )

     IPOSY=WINFODIALOGFIELD(IDF_PICTURE2,FIELDHEIGHT)-IPOSY+1

!     IPOSY=WINFOWINDOW(WINDOWHEIGHT)-IPOSY+1

     IPOSX=IPOSX-IDX
     IPOSY=IPOSY-IDY

     CALL GLREADPIXELS(IPOSX,IPOSY,NDX,NDY,GL_RGB,GL_FLOAT,FRGB)

     ISELECTED=0
     DO I=0,(IDX*IDY)-1
      GLCOLOR(1)=FRGB(I*3+1); GLCOLOR(2)=FRGB(I*3+2); GLCOLOR(3)=FRGB(I*3+3)
      CALL IMOD3D_GETCOLOR(IRGB,GLCOLOR)
!      CALL IMOD3D_GETCOLOR(IRGB,(/FRGB(I*3+1),FRGB(I*3+2),FRGB(I*3+3)/))
      IF(IRGB.NE.IWHITE)THEN
       ISELECTED=IRGB
       EXIT
      ENDIF
     ENDDO
     IF(ISELECTED.NE.JSELECTED)THEN
      !## draw selected
      CALL IMOD3D_SELECTOBJECT_FRONT()
      CALL IMOD3D_SELECTOBJECT_BACK(IWHITE)
      CALL IMOD3D_SELECTOBJECTPUTGRID() 
      JSELECTED=ISELECTED
     ENDIF
    ENDIF

   CASE (RESIZE)
    CALL IMOD3D_SELECTOBJECT_FRONT()
    CALL IMOD3D_PROCESSRESIZE(MESSAGE%VALUE1,MESSAGE%VALUE2)
    IF(IDOWN.EQ.1)CALL IMOD3D_SELECTOBJECT_BACK(IWHITE)

   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE1)
    END SELECT

   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     !## start selection again
     CASE (ID_SELECT)
      IMOUSEMOVE=1
      IDOWN=1
      CALL WCURSORSHAPE(ID_CURSORIDFVALUE)
      CALL IMOD3D_SELECTOBJECT_BACK(IWHITE)
     CASE (IDCANCEL)
      IDOWN=0
      EXIT
    END SELECT

  END SELECT
 ENDDO

 CALL WCURSORSHAPE(CURARROW)
 IF(ALLOCATED(NANSTRING))DEALLOCATE(NANSTRING)

 CALL WDIALOGSELECT(ID_D3DIPFINFO)
 CALL WDIALOGUNLOAD()

 ISELECTED=0
 CALL IMOD3D_SELECTOBJECT_FRONT()

 IMOUSEMOVE=0
 CALL WMESSAGEENABLE(MOUSEBUTUP,1)

 IF(ALLOCATED(IDF_SAMPLE))CALL IDFDEALLOCATE(IDF_SAMPLE,SIZE(IDF_SAMPLE))
 DO I=1,SIZE(ID); CALL WMENUSETSTATE(ID(I),1,1); END DO

 END SUBROUTINE IMOD3D_SELECTOBJECT

 !###======================================================================
 SUBROUTINE IMOD3D_SELECTOBJECTPUTGRID() 
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IIPF,ISEL,IROW,ICOL,ICLR
 REAL :: X,IWIDTH
 
 IF(ISELECTED.LE.0.OR.ISELECTED.GT.SIZE(IPFDLIST))THEN
  !## clear point info
  CALL WDIALOGSELECT(ID_D3DIPFINFOTAB1)
  IF(NIDFLIST.GT.0)THEN
   CALL WGRIDPUTSTRING(IDF_GRID1,2,NANSTRING,NIDFLIST)
  ENDIF
  CALL WDIALOGCLEARFIELD(IDF_MENU1)
  !## clear drill info
  CALL WDIALOGSELECT(ID_D3DIPFINFOTAB2)
  CALL WDIALOGCLEARFIELD(IDF_GRID1)
  CALL WGRIDROWS(IDF_GRID1,1)
  CALL WGRIDCOLOURCOLUMN(IDF_GRID1,2,-1,-1) 
  RETURN
 ENDIF

 CALL WDIALOGSELECT(ID_D3DIPFINFOTAB1)

 IIPF=IPFDLIST(1,ISELECTED) !## selected ipf
 ISEL=IPFDLIST(2,ISELECTED) !## selected irow in ipf

 ALLOCATE(TXTCOLUMN(IPF(IIPF)%NCOL))

 DO J=1,IPF(IIPF)%NCOL
  TXTCOLUMN(J)=TRIM(IPF(IIPF)%ATTRIB(J))//'='//TRIM(IPF(IIPF)%INFO(J,ISEL))
 END DO
 !## plot label information
 CALL WDIALOGPUTMENU(IDF_MENU1,TXTCOLUMN,IPF(IIPF)%NCOL,1)

 !## sample selected idf
 DO J=1,NIDFLIST
  CALL IDFIROWICOL(IDF_SAMPLE(J),IROW,ICOL,IPF(IIPF)%XYZ(1,ISEL),IPF(IIPF)%XYZ(2,ISEL))
  IF(IROW.GE.1.AND.IROW.LE.IDF_SAMPLE(J)%NROW.AND. &
     ICOL.GE.1.AND.ICOL.LE.IDF_SAMPLE(J)%NCOL)THEN
   X=IDFGETVAL(IDF_SAMPLE(J),IROW,ICOL)
   CALL WGRIDPUTCELLSTRING(IDF_GRID1,2,J,TRIM(RTOS(X,'G',7)))
  ELSE
   CALL WGRIDPUTCELLSTRING(IDF_GRID1,2,J,'NaN')
  ENDIF
 END DO

 DEALLOCATE(TXTCOLUMN)

 !## nothing to do, no drills available
 IF(NASSLIST.EQ.0)RETURN

 CALL WDIALOGSELECT(ID_D3DIPFINFOTAB2)

 CALL WDIALOGPUTSTRING(IDF_LABEL1,'Drill: '//TRIM(IPF(IIPF)%INFO(IPF(IIPF)%ACOL,ISEL)))
 CALL WGRIDROWS(IDF_GRID1,ASSF(ISELECTED)%NRASS)

 CALL WGRIDCOLUMNS(IDF_GRID1,ASSF(ISELECTED)%NCASS,IC)

 DO I=1,ASSF(ISELECTED)%NCASS
  CALL WGRIDLABELCOLUMN(IDF_GRID1,I,TRIM(ASSF(ISELECTED)%ATTRIB(I)))
 ENDDO
 
 ICOL=ASSF(ISELECTED)%ASSCOL1
 
 DO I=1,ASSF(ISELECTED)%NRASS
  CALL WGRIDPUTCELLREAL(IDF_GRID1,1,I,ASSF(ISELECTED)%Z(I),'(F10.2)')
  DO J=1,ASSF(ISELECTED)%NCASS-1
   CALL WGRIDPUTCELLSTRING(IDF_GRID1,J+1,I,ASSF(ISELECTED)%L(J,I))
  ENDDO
  CALL IPFDRAWITOPIC2_ICLR(I,ISELECTED,ICLR,IWIDTH)
  CALL WGRIDCOLOURCELL(IDF_GRID1,ICOL,I,-1,ICLR) 
!  CALL WGRIDCOLOURCELL(IDF_GRID1,2,I,-1,ICLR) 
 END DO

 END SUBROUTINE IMOD3D_SELECTOBJECTPUTGRID

 !###======================================================================
 SUBROUTINE IMOD3D_SELECTOBJECT_BACK(IWHITE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IWHITE
 INTEGER :: I

 !## adjust settings for idf'plotting --- turn them into white background, no shade
 DO I=1,NIDFLIST
  IF(IDFPLOT(I)%ISEL.EQ.1)THEN
   IDFPLOT(I)%JFILL  =IDFPLOT(I)%IFILL
   IDFPLOT(I)%JCOLOR =IDFPLOT(I)%ICOLOR
   IDFPLOT(I)%JLEG   =IDFPLOT(I)%ILEG
   IDFPLOT(I)%JSHADED=IDFPLOT(I)%ISHADED
   IDFPLOT(I)%IFILL  =1
   IDFPLOT(I)%ILEG   =1
   IDFPLOT(I)%ICOLOR =IWHITE  !## white
   IDFPLOT(I)%ISHADED=0
  ENDIF
 ENDDO
 JACOLOR=ACOLOR
 ACOLOR =IWHITE

 CALL IMOD3D_DISPLAY(2)

 END SUBROUTINE IMOD3D_SELECTOBJECT_BACK

 !###======================================================================
 SUBROUTINE IMOD3D_SELECTOBJECT_FRONT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 !## recover settings for idf'plotting
 DO I=1,NIDFLIST
  IF(IDFPLOT(I)%ISEL.EQ.1)THEN
   IDFPLOT(I)%IFILL  =IDFPLOT(I)%JFILL
   IDFPLOT(I)%ICOLOR =IDFPLOT(I)%JCOLOR
   IDFPLOT(I)%ILEG   =IDFPLOT(I)%JLEG
   IDFPLOT(I)%ISHADED=IDFPLOT(I)%JSHADED
  ENDIF
 ENDDO
 ACOLOR=JACOLOR

 CALL IMOD3D_DISPLAY(1)

 END SUBROUTINE IMOD3D_SELECTOBJECT_FRONT

 !###======================================================================
 SUBROUTINE IMOD3D_SELECTOBJECT_INIT()
 !###======================================================================
 IMPLICIT NONE

 !## associated files to be used
 IF(NASSLIST.GT.0)THEN
  !## max. columns in associated files
  MAXIC=MAXVAL(ASSF(1:NASSLIST)%NCASS)+3
  ALLOCATE(IC(MAXIC))
  IC(1)=1
  IC(2:MAXIC)=2
  !## ipf read, get grid-values
  !## non-associated files used
 ELSE

 ENDIF

 END SUBROUTINE IMOD3D_SELECTOBJECT_INIT

END MODULE MOD_3D_SELECT

