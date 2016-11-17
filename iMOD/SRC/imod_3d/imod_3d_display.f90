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
MODULE MOD_3D_DISPLAY

USE IMODVAR, ONLY : IDIAGERROR
USE MODPLOT, ONLY : MXMPLOT,MP
USE MOD_UTL, ONLY : UTL_MESSAGEHANDLE3D,UTL_MESSAGEHANDLE,RTOS,UTL_WSELECTFILE,UTL_CAP,UTL_FADEOUTCOLOUR
USE MOD_IDF, ONLY : IDFDEALLOCATE
USE MOD_3D_PAR
USE MOD_3D_UTL, ONLY : IMOD3D_SETCOLOR,IMOD3D_RETURNCOLOR,IMOD3D_GETCOLOR
USE MOD_IPF_PAR, ONLY : ASSF
USE MOD_DEMO_PAR
USE MOD_MDF 
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_PLINES_PAR, ONLY : PL,SP

CONTAINS

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY(IMODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMODE
 REAL(KIND=GLFLOAT) :: XR,XG,XB
 INTEGER :: I,IR,IG,IB,J
 REAL(KIND=GLDOUBLE) :: Z
 INTEGER(GLINT) :: IVIEWPORT(4)
 TYPE (SPHERE3D) :: SLOOKFROM
 LOGICAL(GLBOOLEAN) :: LRED,LGREEN,LBLUE,LALPHA
 
 CALL IMOD3D_ERROR('IMOD3D_DISPLAY_ENTRY')

 IF(IMODE.EQ.1)THEN
  CALL GLENABLE(GL_DITHER)
  !## antialiasing points
  CALL GLENABLE(GL_POINT_SMOOTH)
  !## antialiasing lines
  CALL GLENABLE(GL_LINE_SMOOTH)
  !## enable blending
  CALL GLENABLE(GL_BLEND)
  !## fastest mode
  CALL GLHINT(GL_POINT_SMOOTH_HINT,GL_FASTEST)
  !## fastest mode
  CALL GLHINT(GL_LINE_SMOOTH_HINT,GL_FASTEST)
 !## select mode --- force plotting idf/iff/gen/axes with color zero!
 ELSEIF(IMODE.EQ.2)THEN
  CALL GLDISABLE(GL_POINT_SMOOTH)
  CALL GLDISABLE(GL_LINE_SMOOTH)
  CALL GLDISABLE(GL_POLYGON_SMOOTH)
  CALL GLDISABLE(GL_BLEND)
  CALL GLDISABLE(GL_DITHER)
 ENDIF

 !## set light at position "pos" --- stationary
 CALL GLMATRIXMODE(GL_MODELVIEW)
 CALL GLPOPMATRIX()   !## pops off the top matrix second-from-the top becomes the top
 CALL GLPUSHMATRIX()  !## pushes all matrices in the current stack down one level, topmost is copied
 CALL GLLIGHTFV(GL_LIGHT0,GL_POSITION,POS)

 DO J=1,IANAGLYPHS+1
  
  !## set right image if anaglyphs are active
  IF(J.EQ.2)CALL IMOD3D_ANAGLYPHS()
  
  !## set modelview matrix
  CALL IMOD3D_RESET_VIEW()

  !## set background
  CALL WRGBSPLIT(BGCOLOR,IR,IG,IB)
  XR=REAL(IR)/255.0_GLFLOAT; XG=REAL(IG)/255.0_GLFLOAT; XB=REAL(IB)/255.0_GLFLOAT
  CALL GLCLEARCOLOR(XR,XG,XB,0.0_GLFLOAT)

  CALL GLCOLORMASK(.TRUE._GLBOOLEAN,.TRUE._GLBOOLEAN,.TRUE._GLBOOLEAN,.TRUE._GLBOOLEAN)
  CALL GLCLEAR(IOR(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))   !## clear color and depth buffer each time a drawing is drawn

  IF(IANAGLYPHS.EQ.1)THEN
   IF(J.EQ.1)THEN
    LRED=  .FALSE._GLBOOLEAN  !## do not write red buffer
    LGREEN= .TRUE._GLBOOLEAN  !## do write green buffer
   ELSE
    LRED=  .TRUE._GLBOOLEAN   !## do write red buffer
    LGREEN= .FALSE._GLBOOLEAN !## do not write green buffer
   ENDIF
  ELSE
   LRED=   .TRUE._GLBOOLEAN   !## do write red buffer
   LGREEN= .TRUE._GLBOOLEAN   !## do write green buffer 
  ENDIF
  LBLUE =.TRUE._GLBOOLEAN
  LALPHA=.TRUE._GLBOOLEAN
  
  CALL GLCOLORMASK(LRED,LGREEN,LBLUE,LALPHA)
 
   !## draw axes,roundbox
  IF(IMODE.EQ.1.AND.IBNDBOX.EQ.1)THEN
   CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination
  !## draw filled background in gl_cull_face mode
   CALL IMOD3D_SETCOLOR(BCOLOR) !,100)
   CALL GLLINEWIDTH(1.0_GLFLOAT)
   !## draw box
   IF(AXESINDEX(1).GT.0)CALL GLCALLLIST(AXESINDEX(1))
   !## draw axes
   IF(AXESINDEX(2).GT.0)CALL GLCALLLIST(AXESINDEX(2))
  ENDIF
 
  !## draw axes text,roundbox
  IF(IMODE.EQ.1.AND.IAXES.EQ.1)THEN
   CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination
   !## draw labels
   CALL IMOD3D_SETCOLOR(ACOLOR) 
   IF(AXESINDEX(3).GT.0)CALL GLCALLLIST(AXESINDEX(3))
  ENDIF

  DO I=1,NCLPLIST
   IF(CLPPLOT(I)%ISEL.EQ.1)THEN
    CALL GLENABLE(CLPPLANES(I))
    CALL GLPUSHMATRIX()
    CALL GLTRANSLATED(CLPPLOT(I)%X,CLPPLOT(I)%Y,CLPPLOT(I)%Z)
    CALL GLCLIPPLANE(CLPPLANES(I),CLPPLOT(I)%EQN)
    CALL GLPOPMATRIX()
   ELSE
    CALL GLDISABLE(CLPPLANES(I))
   ENDIF
  END DO

  !## draw ipf-drills/points  
  CALL GLBLENDFUNC(GL_ONE,GL_ZERO) 
  IF(NIPFLIST.GT.0)CALL IMOD3D_DISPLAY_IPF(IMODE)
  !## draw iff's
  IF(NIFFLIST.GT.0)CALL IMOD3D_DISPLAY_IFF()
  !## draw sol's
  IF(NSOLLIST.GT.0)CALL IMOD3D_DISPLAY_SOL()
  !## draw interactive flowlines
  IF(IPATHLINE_3D.GT.0)CALL IMOD3D_DISPLAY_PL()

!  !## draw axes,roundbox
!  IF(IMODE.EQ.1.AND.IBNDBOX.EQ.1)THEN
!   CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination
!  !## draw filled background in gl_cull_face mode
!   CALL IMOD3D_SETCOLOR(BCOLOR,100)
!   CALL GLLINEWIDTH(1.0_GLFLOAT)
!   !## draw box
!   IF(AXESINDEX(1).GT.0)CALL GLCALLLIST(AXESINDEX(1))
!   !## draw axes
!   IF(AXESINDEX(2).GT.0)CALL GLCALLLIST(AXESINDEX(2))
!  ENDIF
! 
!  !## draw axes text,roundbox
!  IF(IMODE.EQ.1.AND.IAXES.EQ.1)THEN
!   CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination
!   !## draw labels
!   CALL IMOD3D_SETCOLOR(ACOLOR,100)
!   IF(AXESINDEX(3).GT.0)CALL GLCALLLIST(AXESINDEX(3))
!  ENDIF

  !## draw idf's - last cause antialiasing and blending not for polygons
  IF(NIDFLIST.GT.0)CALL IMOD3D_DISPLAY_IDF(IMODE)
  !## draw gen's
  IF(NGENLIST.GT.0)CALL IMOD3D_DISPLAY_GEN()
  
  !## draw bmp
  CALL IMOD3D_DISPLAY_BMP()
 
  DO I=1,NCLPLIST; CALL GLDISABLE(CLPPLANES(I)); END DO

 ENDDO
 
 !## draw axes,roundbox
 IF(IORIENT.EQ.1)THEN

  CALL GLMATRIXMODE(GL_MODELVIEW)
  CALL GLPOPMATRIX()   !## pops off the top matrix second-from-the top becomes the top
  CALL GLPUSHMATRIX()  !## pushes all matrices in the current stack down one level, topmost is copied

  !## lookat is the coordinate 0,0,0
  !## lookfrom is the coordinate 10,-20,5
  !## scale factors 1,1,1

  SLOOKFROM=CART2SPHERE(LOOKFROM-LOOKAT)
  Z        =INIT_SHIFTZ-SLOOKFROM%RHO

  !## displacement in the left-corner
  CALL GLTRANSLATED(-2.0_GLDOUBLE,-2.0_GLDOUBLE,-2.0_GLDOUBLE)
  !## no moving and/or zooming
  CALL GLTRANSLATED(0.0_GLDOUBLE,0.0_GLDOUBLE,Z)  !%z affected by zoom
  CALL GLROTATED(ANGLE%X, 0.0_GLDOUBLE, 0.0_GLDOUBLE, 1.0_GLDOUBLE)
  CALL GLROTATED(ANGLE%Y, COS(PI*ANGLE%X/180.0_GLDOUBLE), &
                 -SIN(PI*ANGLE%X/180.0_GLDOUBLE), 0.0_GLDOUBLE)
  CALL GLTRANSLATED(-LOOKAT%X, -LOOKAT%Y, -LOOKAT%Z)

  CALL IMOD3D_SETCOLOR(OCOLOR)
  CALL GLLINEWIDTH(1.0_GLFLOAT)
  CALL GLCALLLIST(ORIENTINDEX)

 ENDIF

 CALL IMOD3D_DISPLAY_LEGEND()

! write(*,*) WInfoDrawable(DrawableType )
! write(*,*) WInfoDrawable(DrawableID )
! write(*,*) WInfoDrawable(DrawableDialog)

! CALL WDIALOGSELECT(ID_D3DSETTINGS)
! CALL WGLSELECT(3,IDF_PICTURE2,WGLDOUBLEBUFFER)
 !## show buffer only in imode.eq.1, other mode will be used for selecting in false-colour mode
 IF(IMODE.EQ.1)CALL WGLSWAPBUFFERS()

!call IMOD3D_ERROR('swpapbuffer')

 CALL GLMATRIXMODE(GL_PROJECTION)
 CALL GLLOADIDENTITY()
 
 IF(IORTHO.EQ.0)THEN
  CALL GLUPERSPECTIVE(FOVY,RAT,ZNEAR,ZFAR)    !## angle(fovy), aspect (ratio:w/h), near () and far()
 ELSE
  CALL GLORTHO(ZLEFT,ZRIGHT,ZBOTTOM,ZTOP,ZNEAR,ZFAR)
 ENDIF
 
 CALL IMOD3D_ERROR('IMOD3D_DISPLAY')

 END SUBROUTINE IMOD3D_DISPLAY
 
 !###======================================================================
 SUBROUTINE IMOD3D_PLOT_INDPOS()
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=GLFLOAT) :: X1,X2,Y1,Y2,Z1,Z2

 X1=-XYZAXES(1)
 X2= XYZAXES(1)
 Y1=-XYZAXES(2)
 Y2= XYZAXES(2)
 Z1= BOT%Z
 Z2= TOP%Z
 
 !## plot lookat-point, midpos
 CALL GLPOINTSIZE(5.0_GLFLOAT)
 CALL GLBEGIN(GL_POINTS)
 CALL IMOD3D_SETCOLOR(WRGB(0,255,0))
 IF(INDPOS%X.EQ.0.0_GLFLOAT.AND.INDPOS%Y.EQ.0.0_GLFLOAT.AND.INDPOS%Z.EQ.0.0_GLFLOAT)THEN
  INDPOS%X=LOOKAT%X; INDPOS%Y=LOOKAT%Y; INDPOS%Z=LOOKAT%Z
 ENDIF
 CALL IMOD3D_SETTINGSPUT_INDPOS()
 CALL GLVERTEX3F(INDPOS%X,INDPOS%Y,INDPOS%Z)
 CALL GLEND()
 
 CALL IMOD3D_SETCOLOR(WRGB(0,200,0))
 CALL GLLINEWIDTH(1.0_GLFLOAT)
 CALL GLBEGIN(GL_LINE_STRIP)
  CALL GLVERTEX3F(INDPOS%X,      Y2,      Z1)
  CALL GLVERTEX3F(INDPOS%X,INDPOS%Y,      Z1)
  CALL GLVERTEX3F(INDPOS%X,INDPOS%Y,INDPOS%Z)
  CALL GLVERTEX3F(INDPOS%X,      Y2,INDPOS%Z)
  CALL GLVERTEX3F(INDPOS%X,      Y2,      Z1)
 CALL GLEND()
 CALL GLBEGIN(GL_LINE_STRIP)
  CALL GLVERTEX3F(INDPOS%X,INDPOS%Y,      Z1)
  CALL GLVERTEX3F(X2      ,INDPOS%Y,      Z1)
  CALL GLVERTEX3F(X2      ,INDPOS%Y,INDPOS%Z)
  CALL GLVERTEX3F(INDPOS%X,INDPOS%Y,INDPOS%Z)
  CALL GLVERTEX3F(INDPOS%X,INDPOS%Y,      Z1)
 CALL GLEND()
  
 END SUBROUTINE IMOD3D_PLOT_INDPOS

 !###======================================================================
 SUBROUTINE IMOD3D_SETTINGSPUT_INDPOS()
 !###======================================================================
 IMPLICIT NONE
 REAL :: X,DX
 
 DX=TOP%X-BOT%X
 X=BOT%X+DX*(INDPOS%X+XYZAXES(1))/(XYZAXES(1)*2.0_GLFLOAT)
! CALL WDIALOGPUTREAL(IDF_REAL7,X,'(F8.1)')
 DX=TOP%Y-BOT%Y
 X=BOT%Y+DX*(INDPOS%Y+XYZAXES(2))/(XYZAXES(2)*2.0_GLFLOAT)
! CALL WDIALOGPUTREAL(IDF_REAL8,X,'(F8.1)')
! X=INDPOS%Z; CALL WDIALOGPUTREAL(IDF_REAL9,X,'(F8.1)')
 
 END SUBROUTINE IMOD3D_SETTINGSPUT_INDPOS
  
 !###======================================================================
 SUBROUTINE IMOD3D_LIGHT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB5)

 CALL WDIALOGGETTRACKBAR(IDF_TRACKBAR2,I); F_L1_AMBIENT=REAL(I)/100.0
 CALL WDIALOGGETTRACKBAR(IDF_TRACKBAR3,I); F_L1_DIFFUSE=REAL(I)/100.0
 CALL WDIALOGGETTRACKBAR(IDF_TRACKBAR4,I); F_L1_SPECULAR=REAL(I)/100.0
 
 L1_AMBIENT= (/F_L1_AMBIENT,F_L1_AMBIENT,F_L1_AMBIENT,1.0_GLFLOAT/)
 L1_DIFFUSE= (/F_L1_DIFFUSE,F_L1_DIFFUSE,F_L1_DIFFUSE,1.0_GLFLOAT/)
 L1_SPECULAR=(/F_L1_SPECULAR,F_L1_SPECULAR,F_L1_SPECULAR,1.0_GLFLOAT/)

!The ambient component is the light from that source that's been scattered so much by the environment
!that its direction is impossible to determine - it seems to come from all directions. Backlighting in a room
!has a large ambient component, since most of the light that reaches your eye has bounced off many
!surfaces first. A spotlight outdoors has a tiny ambient component; most of the light travels in the same
!direction, and since you're outdoors, very little of the light reaches your eye after bouncing off other
!objects. When ambient light strikes a surface, it's scattered equally in all directions.

!--- flattens image ----
 CALL GLLIGHTFV(GL_LIGHT0, GL_AMBIENT, L1_AMBIENT) 
!Diffuse light comes from one direction, so it's brighter if it comes squarely down on a surface than if it
!barely glances off the surface. Once it hits a surface, however, it's scattered equally in all directions, so it
!appears equally bright, no matter where the eye is located. Any light coming from a particular position or
!direction probably has a diffuse component.
 CALL GLLIGHTFV(GL_LIGHT0, GL_DIFFUSE,L1_DIFFUSE)
!Finally, specular light comes from a particular direction, and it tends to bounce off the surface in a
!preferred direction. A well-collimated laser beam bouncing off a high-quality mirror produces almost 100
!percent specular reflection. Shiny metal or plastic has a high specular component, and chalk or carpet has
!almost none. You can think of specularity as shininess.
 CALL GLLIGHTFV(GL_LIGHT0, GL_SPECULAR,L1_SPECULAR)

 END SUBROUTINE IMOD3D_LIGHT

 !###======================================================================
 SUBROUTINE IMOD3D_RESET_TO_INIT()
 !###======================================================================
 !## This resets the view to the initial configuration
 IMPLICIT NONE
 TYPE (SPHERE3D) :: SLOOKFROM

 !## lookat is the coordinate 0,0,0
 !## lookfrom is the coordinate 10,-20,5
 !## scale factors 1,1,1

 SLOOKFROM    = CART2SPHERE(LOOKFROM-LOOKAT)
 ANGLE%X      =-180.0_GLDOUBLE*SLOOKFROM%THETA/PI - 90.0_GLDOUBLE
 ANGLE%Y      =-180.0_GLDOUBLE*SLOOKFROM%PHI/PI
 SHIFT%X      = INIT_SHIFTX
 SHIFT%Y      = INIT_SHIFTY
 SHIFT%Z      = INIT_SHIFTZ-SLOOKFROM%RHO
 XSCALE_FACTOR= INIT_XSCALE_FACTOR
 YSCALE_FACTOR= INIT_YSCALE_FACTOR
 ZSCALE_FACTOR= INIT_ZSCALE_FACTOR

 CALL IMOD3D_ERROR('IMOD3D_RESET_TO_INIT')

 END SUBROUTINE IMOD3D_RESET_TO_INIT

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_IPF(IMODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMODE
 INTEGER :: I,IIPF
 REAL(KIND=GLFLOAT) :: LT !## linethickness
   
 !## associated file drawn
 DO I=1,NIPFLIST
  !## active based upon selection
  IF(IPFDLIST(5,I).EQ.0)CYCLE
  !## part of selected ipf
  IIPF=IPFDLIST(1,I); IF(IIPF.EQ.0)CYCLE
  !## selected in menu-field
  IF(IPFPLOT(IIPF)%ISEL.NE.1.OR.IPFLISTINDEX(I,IMODE).EQ.0)CYCLE
   
  IF(IMODE.EQ.1)then
   !## line width
   IF(ISELECTED.EQ.I)THEN
    IF(IPFPLOT(IIPF)%IFANCY.EQ.0)THEN
     CALL GLLINEWIDTH(10.0_GLFLOAT)
     CALL GLPOINTSIZE(10.0_GLFLOAT)
    ELSEIF(IPFPLOT(IIPF)%IFANCY.EQ.1)THEN
     CALL GLPOLYGONMODE(GL_FRONT,GL_LINE); CALL GLPOLYGONMODE(GL_BACK, GL_LINE)
     CALL GLLINEWIDTH(1.0_GLFLOAT)
    ENDIF
   ELSE
    !## mark whenever drill has been selected/activated to be interpolated
    LT=2.0_GLFLOAT
    CALL GLLINEWIDTH(LT)
    CALL GLPOINTSIZE(4.0_GLFLOAT)
   ENDIF
  
   IF(IPFPLOT(IIPF)%IFANCY.EQ.1)THEN
    IF(IPFPLOT(IIPF)%ISHADE.EQ.1.AND.IMODE.EQ.1)THEN
     CALL GLSHADEMODEL(GL_FLAT); CALL GLENABLE(GL_LIGHTING); CALL GLDISABLE(GL_BLEND)
    ENDIF
   ENDIF

  ENDIF

  CALL GLCALLLIST(IPFLISTINDEX(I,IMODE))

  IF(IMODE.EQ.1)THEN
   IF(IPFPLOT(IIPF)%IFANCY.EQ.1)THEN
    IF(IPFPLOT(IIPF)%ISHADE.EQ.1.AND.IMODE.EQ.1)THEN
     CALL GLDISABLE(GL_LIGHTING); CALL GLENABLE(GL_BLEND)
    ENDIF
   ENDIF
   CALL GLPOLYGONMODE(GL_BACK, GL_FILL); CALL GLPOLYGONMODE(GL_FRONT,GL_FILL)
  ENDIF

 END DO

 !## draw ipf labels
 IF(IMODE.EQ.1)THEN
  !## associated file drawn
  DO I=1,NIPFLIST
   !## active based upon selection
   IF(IPFDLIST(5,I).EQ.0)CYCLE
   !## part of selected ipf
   IIPF=IPFDLIST(1,I) 
   !## cycle if not selected to plot labels
   IF(IPFPLOT(IIPF)%IPLOTLABELS.EQ.0)CYCLE
   !## selected in menu-field
   IF(IPFPLOT(IIPF)%ISEL.EQ.0.OR.IPFLISTINDEX(I,3).EQ.0)CYCLE
   IF(ISELECTED.EQ.I)THEN
    CALL IMOD3D_SETCOLOR(WRGB(255,0,0))
   ELSE
    CALL IMOD3D_SETCOLOR(LCOLOR)
   ENDIF
   CALL GLCALLLIST(IPFLISTINDEX(I,3))
  ENDDO
 ENDIF

 CALL IMOD3D_ERROR('IMOD3D_DISPLAY_IPF')

 END SUBROUTINE IMOD3D_DISPLAY_IPF

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_IDF(IMODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J
 INTEGER,INTENT(IN) :: IMODE
 REAL(KIND=GLDOUBLE) :: TSTACK
 
 TSTACK=0.0_GLDOUBLE
 DO I=1,SIZE(IDFLISTINDEX) 
  IF(IDFPLOT(I)%ISEL.NE.1.OR.IDFLISTINDEX(I).EQ.0)CYCLE
 
  IF(IDFPLOT(I)%ISTACKED.GT.0)THEN
   CALL GLPUSHMATRIX()  !## pushes all matrices in the current stack down one level, topmost is copied
   TSTACK=TSTACK+(IDFPLOT(I)%ISTACKED*5.0_GLDOUBLE)
   CALL GLTRANSLATED(0.0_GLDOUBLE, 0.0_GLDOUBLE, -TSTACK) !SHIFTZ)
  ENDIF
  
  IF(IDFPLOT(I)%IFILL.EQ.1.OR.IDFPLOT(I)%IFILL.EQ.3)THEN

   !## blend mode 
   IF(IMODE.EQ.1.AND.IDFPLOT(I)%ITRANSPARANCY.LT.100)THEN
    !## draw furthers first
    CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination
   ELSE
    !## opaque mode
    CALL GLBLENDFUNC(GL_ONE,GL_ZERO)  !## (1) source (2) destination
   ENDIF
  
   CALL GLPOLYGONMODE(GL_BACK, GL_FILL); CALL GLPOLYGONMODE(GL_FRONT,GL_FILL) 

   IF(IDFPLOT(I)%ILEG.EQ.1)THEN
    !## apply transparancy
    IF(IDFPLOT(I)%ITRANSPARANCY.LT.100)THEN
     CALL IMOD3D_SETCOLOR(IDFPLOT(I)%ICOLOR,IDFPLOT(I)%ITRANSPARANCY) 
    ELSE
     CALL IMOD3D_SETCOLOR(IDFPLOT(I)%ICOLOR)
    ENDIF
   ENDIF

   !## turn on light if neccessary
   IF(IDFPLOT(I)%ISHADED.EQ.1)THEN

    !## set material ...

!    !## one single colour used
!    IF(IDFPLOT(I)%ILEG.EQ.1)THEN
!     !## show shaded surface
!     CALL IMOD3D_RETURNCOLOR(IDFPLOT(I)%ICOLOR,AMBIENT)
!     CALL GLMATERIALFV(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE,AMBIENT)
!    ENDIF

    !## flat shading
    CALL GLSHADEMODEL(GL_FLAT) !## GL_SMOOTH
    CALL GLENABLE(GL_LIGHTING)

   ENDIF

   CALL GLCALLLIST(IDFLISTINDEX(I))

   !## turn of light
   IF(IDFPLOT(I)%ISHADED.EQ.1)CALL GLDISABLE(GL_LIGHTING)
  
  ENDIF

  !## draw mesh
  IF(IDFPLOT(I)%IFILL.EQ.2.OR.IDFPLOT(I)%IFILL.EQ.3)THEN

!   !## blend mode
!   CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination

   !## show lines to represent rectangles/triangles
   IF(IDFPLOT(I)%IFILL.EQ.2)CALL IMOD3D_SETCOLOR(IDFPLOT(I)%ICOLOR)
   IF(IDFPLOT(I)%IFILL.EQ.3)CALL IMOD3D_SETCOLOR(WRGB(0,0,0))
   CALL GLLINEWIDTH(1.0_GLFLOAT)
   !## outline (showing rectangles)
   CALL GLPOLYGONMODE(GL_FRONT,GL_LINE); CALL GLPOLYGONMODE(GL_BACK, GL_LINE)

   CALL GLCALLLIST(IDFLISTINDEX(I))

  ENDIF

  !## pops off the top matrix second-from-the top becomes the top
  IF(IDFPLOT(I)%ISTACKED.GT.0)CALL GLPOPMATRIX()   

 END DO

 !## default
 CALL GLPOLYGONMODE(GL_BACK, GL_FILL); CALL GLPOLYGONMODE(GL_FRONT,GL_FILL)
! CALL GLENABLE(GL_DEPTH_TEST)

 END SUBROUTINE IMOD3D_DISPLAY_IDF

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_IFF()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 REAL(KIND=GLFLOAT) :: XW

 !## opaque mode
 CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination

 DO I=1,NIFFLIST
  IF(IFFPLOT(I)%ISEL.NE.1.OR.IFFLISTINDEX(I).EQ.0)CYCLE

  IF(MP(IFFPLOT(I)%IPLOT)%ILEG.EQ.0)THEN
   CALL IMOD3D_SETCOLOR(MP(IFFPLOT(I)%IPLOT)%SCOLOR)
  ENDIF
  XW=REAL(IFFPLOT(I)%ITHICKNESS)
  CALL GLLINEWIDTH(XW)
  CALL GLCALLLIST(IFFLISTINDEX(I))

 END DO

 END SUBROUTINE IMOD3D_DISPLAY_IFF

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_SOL()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 !## opaque mode
 !CALL GLBLENDFUNC(GL_ONE,GL_ZERO)  !## (1) source (2) destination

 CALL GLENABLE(GL_LIGHTING)
 CALL GLSHADEMODEL(GL_FLAT) !## heeft te maken met invullen kleuren

!It depends on the effect you're trying to achieve.
!
!If you want blending to occur after the texture has been applied, then use the OpenGL blending feature. Try this:
!
!glEnable (GL_BLEND);
!glBlendFunc (GL_ONE, GL_ONE);
!You might want to use the alpha values that result from texture mapping in the blend function. If so,
!(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA) is always a good function to start with.
!
!However, if you want blending to occur when the primitive is texture mapped (i.e., you want parts of the
!texture map to allow the underlying color of the primitive to show through), then don't use OpenGL blending.
!Instead, you'd use glTexEnv(), and set the texture environment mode to GL_BLEND. In this case, you'd want to leave
!the texture environment color to its default value of (0,0,0,0).
!
!Column Header
! CALL GLENABLE(GL_BLEND)

 !## plot filled in cross-sections
 DO I=1,NSOLLIST

  IF(SOLPLOT(I)%ISEL.EQ.0)CYCLE
  IF(SOLPLOT(I)%IBITMAP.EQ.1)CYCLE
  IF(SOLLISTINDEX(I,1).EQ.0)CYCLE
  
!  !## blend mode 
!  IF(SOLPLOT(I)%IBLEND.LT.100)THEN
   !## draw furthers first
!   CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_SRC_ALPHA) !GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination
!  ELSE
!   !## opaque mode
!   CALL GLBLENDFUNC(GL_ONE,GL_ZERO)  !## (1) source (2) destination
!  ENDIF

  !## not showing interfaces
  IF(SOLPLOT(I)%IINTERFACE.EQ.0)THEN 
   CALL GLENABLE(GL_LIGHTING)
!   IF(SOLPLOT(I)%IBLEND.EQ.1)THEN
!    CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination
!   ENDIF
  ELSE
   CALL GLDISABLE(GL_LIGHTING)
  ENDIF
  CALL GLCALLLIST(SOLLISTINDEX(I,1))
 END DO

 CALL GLDISABLE(GL_LIGHTING)

 CALL GLCOLOR4F(1.0_GLFLOAT,1.0_GLFLOAT,1.0_GLFLOAT,0.0_GLFLOAT)

 !## plot bitmaps/interfaces
 DO I=1,NSOLLIST
  IF(SOLPLOT(I)%ISEL.EQ.0)CYCLE
  IF(SOLPLOT(I)%IBITMAP.EQ.0)CYCLE
  IF(SOLLISTINDEX(I,2).EQ.0)CYCLE
  !## interface
  IF(SOLPLOT(I)%IINTERFACE.EQ.1)THEN
   CALL GLPOLYGONMODE(GL_FRONT,GL_LINE); CALL GLPOLYGONMODE(GL_BACK, GL_LINE)
  !## bitmaps
  ELSE
!   CALL GLENABLE(GL_ALPHA_TEST)
!   CALL GLALPHAFUNC(GL_GREATER,0.0_GLFLOAT)
!   CALL GLPOLYGONMODE(GL_BACK, GL_FILL); CALL GLPOLYGONMODE(GL_FRONT,GL_FILL)
  ENDIF
  CALL GLCALLLIST(SOLLISTINDEX(I,2))
!  CALL GLDISABLE(GL_ALPHA_TEST)
 ENDDO
 
 CALL GLENABLE(GL_BLEND)
 
 END SUBROUTINE IMOD3D_DISPLAY_SOL

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_CLP()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 DO I=1,NCLPLIST
  IF(CLPPLOT(I)%ISEL.EQ.1)THEN
   CALL GLCALLLIST(CLPLISTINDEX(I))
  ENDIF
 END DO

 END SUBROUTINE IMOD3D_DISPLAY_CLP

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_PL()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,ICLR,N,IG
 REAL :: FCT
 REAL(KIND=GLFLOAT) :: XWIDTH
 
 !## draw start-locations
 IF(PL%IPLOTSP.EQ.1)THEN
  DO IG=1,SIZE(STPLISTINDEX)
   IF(STPLISTINDEX(IG).NE.0)THEN
    !## skip current particle-startpoints
    IF(SP(IG)%IACT.EQ.0)CYCLE !SPWIDTH.LE.0.0)CYCLE
    CALL IMOD3D_SETCOLOR(SP(IG)%ICLR)
    XWIDTH=SP(IG)%SPWIDTH; CALL GLPOINTSIZE(XWIDTH)
    CALL GLCALLLIST(STPLISTINDEX(IG))
   ENDIF
  ENDDO
 ENDIF
  
 IF(ALLOCATED(PLLISTINDEX))THEN
  !## draw particles upto pl_iper
  N=SIZE(PLLISTINDEX,1)
  DO I=1,SIZE(PLLISTINDEX,1)
   DO J=1,SIZE(PLLISTINDEX,2)
    IF(PLLISTINDEX(I,J).NE.0)THEN
     XWIDTH=SP(J)%PWIDTH; CALL GLPOINTSIZE(XWIDTH); CALL GLLINEWIDTH(XWIDTH)
     ICLR=SP(J)%ICLR; FCT=REAL(PLLISTCLR(I))/REAL(N)
     CALL UTL_FADEOUTCOLOUR(ICLR,FCT)
     CALL IMOD3D_SETCOLOR(ICLR)
     CALL GLCALLLIST(PLLISTINDEX(I,J))
    ENDIF
   ENDDO
  ENDDO
 ENDIF
 
 CALL GLPOINTSIZE(1.0_GLFLOAT); CALL GLLINEWIDTH(1.0_GLFLOAT)
 
 END SUBROUTINE IMOD3D_DISPLAY_PL
 
 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_GEN()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 REAL(KIND=GLFLOAT) :: XW,XR,XG,XB
 INTEGER :: IR,IG,IB

 !## blend mode
 CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination

 DO I=1,NGENLIST
  IF(GENPLOT(I)%ISEL.NE.1.OR.GENLISTINDEX(I).EQ.0)CYCLE

  IF(GENPLOT(I)%L3D)THEN
   !## show shaded surface
   CALL IMOD3D_SETCOLOR(GENPLOT(I)%ICOLOR,GENPLOT(I)%ITRANSPARANCY)
  ELSE
   CALL IMOD3D_SETCOLOR(GENPLOT(I)%ICOLOR)
   XW=REAL(GENPLOT(I)%ITHICKNESS)
   CALL GLLINEWIDTH(XW)
  ENDIF
  CALL GLCALLLIST(GENLISTINDEX(I))

 END DO

 END SUBROUTINE IMOD3D_DISPLAY_GEN

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_BMP()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 REAL(GLFLOAT) :: ALPHA
 
 !## not background bitmap plotting
 IF(IACTBITMAP.EQ.0)RETURN

 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB5)
 CALL WDIALOGGETTRACKBAR(IDF_TRACKBAR6,ITRANSPARANCYBITMAP)
 
! CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)

 !## blend mode 
 IF(ITRANSPARANCYBITMAP.LT.100)THEN
  !## draw furthers first
  CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination
  ALPHA=REAL(ITRANSPARANCYBITMAP)/100.0
 ELSE
  !## opaque mode
  CALL GLBLENDFUNC(GL_ONE,GL_ZERO)  !## (1) source (2) destination
 ENDIF

 CALL GLCOLOR4F(1.0_GLFLOAT,1.0_GLFLOAT,1.0_GLFLOAT,ALPHA) 

 I=1
 CALL GLENABLE(GL_TEXTURE_2D)
 CALL GLCALLLIST(BMPLISTINDEX(I))
 CALL GLDISABLE(GL_TEXTURE_2D)

 END SUBROUTINE IMOD3D_DISPLAY_BMP

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_LEGEND() 
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,N,M

 IF(LEGENDINDEX.EQ.0)RETURN

 !## save projection transformation matrix
 CALL GLMATRIXMODE(GL_PROJECTION); CALL GLPUSHMATRIX()
 !## set new projection transformation matrix
 CALL GLLOADIDENTITY()
 CALL GLORTHO(0.0_GLDOUBLE,100.0_GLDOUBLE,0.0_GLDOUBLE,100.0_GLDOUBLE,-1.0_GLDOUBLE,1.0_GLDOUBLE)
 !## save modelview transformation matrix
 CALL GLMATRIXMODE(GL_MODELVIEW); CALL GLPUSHMATRIX()
 !## set new modelview transformation matrix
 CALL GLLOADIDENTITY()

 CALL GLDISABLE(GL_BLEND)
 CALL GLDISABLE(GL_LIGHT0)
 CALL GLPOLYGONMODE(GL_FRONT_AND_BACK,GL_FILL)
 CALL GLDISABLE(GL_DEPTH_TEST)

 CALL GLCALLLIST(LEGENDINDEX)

 CALL GLENABLE(GL_BLEND)
 CALL GLENABLE(GL_LIGHT0) 
 CALL GLDEPTHFUNC(GL_LESS)
 CALL GLENABLE(GL_DEPTH_TEST)
  
 !## restore transformations matrices
 CALL GLMATRIXMODE(GL_MODELVIEW);  CALL GLPOPMATRIX()
 CALL GLMATRIXMODE(GL_PROJECTION); CALL GLPOPMATRIX()

 END SUBROUTINE IMOD3D_DISPLAY_LEGEND

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_SAVE(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER(KIND=GLSIZEI) :: NDX,NDY 
 INTEGER(GLINT) :: IVIEWPORT(4) 
 REAL(KIND=GLFLOAT),DIMENSION(:),ALLOCATABLE :: FRGB
 INTEGER,DIMENSION(:),ALLOCATABLE :: IRGB 
 INTEGER :: IHANDLE,I
 CHARACTER(LEN=256) :: FNAME
 REAL(KIND=GLFLOAT),DIMENSION(3) :: GLCOLOR
 
 IF(ID.EQ.ID_SAVEAS)THEN
  IF(.NOT.UTL_WSELECTFILE('All Known Files (*.bmp;*.pcx;*.png;*.jpg)|*.bmp;*.pcx;*.png;*.jpg|BitMap (*.bmp)|*.bmp| &
                           ZSoft PC Paintbrush (*.pcx)|*.pcx|Portable Network Graphic image (*.png)|*.png|JPEG Image (*.jpg)|*.jpg|',&
                   SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                   'Save Current Image to BitMap (*.bmp;*.pcx;*.png;*.jpg)'))RETURN
 ENDIF
 
 !## capture entire screen
 CALL GLGETINTEGERV(GL_VIEWPORT,IVIEWPORT)
 NDX=IVIEWPORT(3); NDY=IVIEWPORT(4)
 ALLOCATE(FRGB(NDX*NDY*3),IRGB(NDX*NDY))
 
 CALL GLREADBUFFER(GL_FRONT)
 CALL GLREADPIXELS(0_GLINT,0_GLINT,NDX,NDY,GL_RGB,GL_FLOAT,FRGB)
 
 DO I=0,(NDX*NDY)-1
  GLCOLOR(1)=FRGB(I*3+1); GLCOLOR(2)=FRGB(I*3+2); GLCOLOR(3)=FRGB(I*3+3)
  CALL IMOD3D_GETCOLOR(IRGB(I+1),GLCOLOR)
 ENDDO
 
 CALL WBITMAPCREATE(IHANDLE,NDX,NDY)  
 CALL WBITMAPGETDATA(IHANDLE,IRGB)   
 !## switch vertically
 CALL WBITMAPMIRROR(IHANDLE,1)

 IF(ID.EQ.ID_SAVEAS)THEN
  CALL WBITMAPSAVE(IHANDLE,FNAME)
 ELSE
  CALL WBITMAPCLIPBOARD(IHANDLE)
 ENDIF
 CALL WBITMAPDESTROY(IHANDLE)
 
 DEALLOCATE(FRGB,IRGB)
 CALL GLREADBUFFER(GL_BACK)
 
 IF(ID.EQ.ID_SAVEAS)THEN
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Successfully saved the current image to'//CHAR(13)//TRIM(FNAME),'Information')
 ELSE
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Successfully copied the current image to'//CHAR(13)//'the Windows Clipboard','Information')
 ENDIF
 
 END SUBROUTINE IMOD3D_DISPLAY_SAVE
 
  !###======================================================================
 SUBROUTINE IMOD3D_DEMO_SAVE(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 CHARACTER(LEN=256) :: FNAME
 CHARACTER(LEN=3) :: EXT
 INTEGER :: IPLOT,N,I
  
 IF(ID.EQ.ID_SAVEAS_DEMO)THEN
  FNAME=TRIM(PREFVAL(1))//'\IMFILES\*.imf'
  IF(.NOT.UTL_WSELECTFILE('iMOD Project (*.imf)|*.imf|',SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                   'Save Current Settings into an DEMO iMOD project (*.imf)'))RETURN
 ENDIF
 
 EXT=UTL_CAP(FNAME(INDEX(FNAME,'.',.TRUE.)+1:),'U')
 SELECT CASE (TRIM(EXT))
  CASE('IMF')
   IF(DEMO%IDEMO.NE.2)THEN
    DEMO%IDEMO = 2
   ENDIF

   NIDFLIST=0
   DO IPLOT=1,MXMPLOT
    IF(MP(IPLOT)%ISEL.AND.(MP(IPLOT)%IPLOT.EQ.1.OR.MP(IPLOT)%IPLOT.EQ.5))THEN
     !## get idf for mdf file
     IF(MP(IPLOT)%IPLOT.EQ.5)THEN
      IF(READMDF(MP(IPLOT)%IDFNAME,N))THEN
       DO I=1,N
        NIDFLIST=NIDFLIST+1
        MDF(I)%LEG       =IDFPLOT(NIDFLIST)%LEG
        MDF(I)%SCOLOR    =IDFPLOT(NIDFLIST)%ICOLOR
       ENDDO
       IF(.NOT.WRITEMDF(MP(IPLOT)%IDFNAME,N))THEN
       ENDIF
       CALL MDFDEALLOCATE()
      ENDIF
     ELSE
      NIDFLIST=NIDFLIST+1
      MP(IPLOT)%SCOLOR=IDFPLOT(NIDFLIST)%ICOLOR
      MP(IPLOT)%ILEG=IDFPLOT(NIDFLIST)%ILEG
      IF(IDFPLOT(NIDFLIST)%ICONFIG.LT.0)THEN
       DEMO%CONFLAG = 1 !##use "Interfaces" as default setting
      ELSE
       DEMO%CONFLAG = IDFPLOT(NIDFLIST)%ICONFIG
      ENDIF
      DEMO%IFILL = IDFPLOT(NIDFLIST)%IFILL
      DEMO%ACCFLAG=IDFPLOT(NIDFLIST)%IACC
     ENDIF
    ENDIF
   ENDDO

   CALL IMODSAVEIMF(FNAME)
   CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Successfully saved 3D-plot as DEMO iMOD project (*.imf) to:'//CHAR(13)//TRIM(FNAME),'Info')
   DEMO%IDEMO = 0
 END SELECT
 
 END SUBROUTINE IMOD3D_DEMO_SAVE

 !###======================================================================
 SUBROUTINE IMOD3D_ANAGLYPHS()
 !###======================================================================
 IMPLICIT NONE

!   STEP      =MINSTEP
!   HEADING   =HEADING+PERP
!   LOOKFROM%X=LOOKFROM%X-COS(HEADING)*STEP
!   LOOKFROM%Y=LOOKFROM%Y-SIN(HEADING)*STEP
!   LOOKAT%X  =LOOKAT%X-COS(HEADING)*STEP
!   LOOKAT%Y  =LOOKAT%Y-SIN(HEADING)*STEP
!   HEADING   =HEADING-PERP

! LOOKAT%X=LOOKFROM%X+COS(HEADING)*DX
! LOOKAT%Y=LOOKFROM%Y+SIN(HEADING)*DX 
! LOOKAT%Z=LOOKFROM%Z+SIN(TILT)*DZ 

 !## set transformation parameters according to the lookfrom() and lookat() variables
! CALL IMOD3D_RESET_TO_INIT()
! CALL IMOD3D_DISPLAY(1)

 END SUBROUTINE IMOD3D_ANAGLYPHS

 !###======================================================================
 SUBROUTINE IMOD3D_RESET_VIEW() 
 !###======================================================================
 IMPLICIT NONE
 
 CALL IMOD3D_ERROR('IMOD3D_RESET_VIEW_ENTRY')

 CALL GLTRANSLATED(SHIFT%X, SHIFT%Y, SHIFT%Z)
 CALL GLROTATED(ANGLE%X, 0.0_GLDOUBLE, 0.0_GLDOUBLE, 1.0_GLDOUBLE)
 CALL GLROTATED(ANGLE%Y, COS(PI*ANGLE%X/180.0_GLDOUBLE), &
                -SIN(PI*ANGLE%X/180.0_GLDOUBLE), 0.0_GLDOUBLE)
 CALL GLTRANSLATED(-LOOKAT%X, -LOOKAT%Y, -LOOKAT%Z)
 CALL GLSCALED(XSCALE_FACTOR,YSCALE_FACTOR,ZSCALE_FACTOR)
 
 CALL IMOD3D_ERROR('IMOD3D_RESET_VIEW')

 END SUBROUTINE IMOD3D_RESET_VIEW

END MODULE MOD_3D_DISPLAY

