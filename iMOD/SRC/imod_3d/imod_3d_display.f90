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
MODULE MOD_3D_DISPLAY

USE IMODVAR, ONLY : DP_KIND,SP_KIND,IDIAGERROR
USE MODPLOT, ONLY : MXMPLOT,MP
USE MOD_UTL, ONLY : UTL_MESSAGEHANDLE3D,UTL_MESSAGEHANDLE,RTOS,UTL_WSELECTFILE,UTL_CAP,UTL_FADEOUTCOLOUR
USE MOD_IDF, ONLY : IDFDEALLOCATE
USE MOD_3D_PAR
USE MOD_3D_UTL, ONLY : IMOD3D_SETCOLOR,IMOD3D_GETCOLOR,IMOD3D_SETNORMALVECTOR
USE MOD_IPF_PAR, ONLY : ASSF
USE MOD_DEMO_PAR
USE MOD_MDF 
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_PLINES_PAR, ONLY : PL,SP
USE MOD_MAIN_UTL

CONTAINS

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY(IMODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMODE
 REAL(KIND=GLFLOAT) :: XR,XG,XB
 INTEGER :: I,IR,IG,IB,J
 LOGICAL(GLBOOLEAN) :: LRED,LGREEN,LBLUE,LALPHA,LDMASK
 
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

  !## set background - white
  CALL WRGBSPLIT(BGCOLOR,IR,IG,IB)
  XR=REAL(IR)/255.0_GLFLOAT; XG=REAL(IG)/255.0_GLFLOAT; XB=REAL(IB)/255.0_GLFLOAT
  CALL GLCLEARCOLOR(XR,XG,XB,0.0_GLFLOAT)
!  CALL GLCLEARCOLOR(0.0_GLFLOAT,0.0_GLFLOAT,0.0_GLFLOAT,0.0_GLFLOAT)

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
 
  !## plot axes/orientation box
  CALL IMOD3D_DISPLAY_AXES(IMODE)

  !## apply clipping planes
  CALL IMOD3D_DISPLAY_CLP()

  !## draw ipf-drills/points  
  CALL GLBLENDFUNC(GL_ONE,GL_ZERO) 
  IF(NIPFLIST.GT.0)CALL IMOD3D_DISPLAY_IPF(IMODE)
  !## draw iff's
  IF(NIFFLIST.GT.0)CALL IMOD3D_DISPLAY_IFF()
  !## draw sol's
  IF(NSOLLIST.GT.0)CALL IMOD3D_DISPLAY_SOL()
  !## draw interactive flowlines
  IF(IPATHLINE_3D.GT.0)CALL IMOD3D_DISPLAY_PL()
  !## draw idf's
  IF(NIDFLIST.GT.0)CALL IMOD3D_DISPLAY_IDF(IMODE,0)
  !## draw time variant idfs
  IF(NIDFLIST.GT.0)CALL IMOD3D_DISPLAY_TIME_IDF()
  
  !## draw gen's
  IF(NGENLIST.GT.0)CALL IMOD3D_DISPLAY_GEN(0)
  !## draw bmp
  CALL IMOD3D_DISPLAY_BMP() 
 
  !## put transparancy last - used fixed depth mask and plot all transparant images
  !## freeze depthmask for transluscent plotting
  LDMASK=.FALSE.; CALL GLDEPTHMASK(LDMASK)
  IF(NIDFLIST.GT.0)CALL IMOD3D_DISPLAY_IDF(IMODE,1)
  IF(NGENLIST.GT.0)CALL IMOD3D_DISPLAY_GEN(1)
  LDMASK=.TRUE.; CALL GLDEPTHMASK(LDMASK)

  DO I=1,NCLPLIST; CALL GLDISABLE(CLPPLANES(I)); END DO

  !## plot point of mouse
  CALL IMOD3D_PLOT_INDPOS()
  !## plot artificial wells
  CALL IMOD3D_PLOT_WELLS()
  !## plot filled rectangle
  CALL IMOD3D_PLOT_CROSSSECTION()

 ENDDO
 
 !## draw axes,roundbox
 IF(IORIENT.EQ.1.AND.IMODE.EQ.1)CALL IMOD3D_DISPLAY_ORIENT()

 !## plot legend
 IF(IMODE.EQ.1)CALL IMOD3D_DISPLAY_LEGEND()

 !## show buffer only in imode.eq.1, other mode will be used for selecting in false-colour mode
 IF(IMODE.EQ.1)CALL WGLSWAPBUFFERS()

 CALL GLMATRIXMODE(GL_PROJECTION)
 CALL GLLOADIDENTITY()
 
 IF(IORTHO.EQ.0)THEN
  CALL GLUPERSPECTIVE(FOVY,RAT,ZNEAR,ZFAR)    !## angle(fovy), aspect (ratio:w/h), near () and far()
 ELSE
  CALL GLORTHO(BOT%X,TOP%X,BOT%Y,TOP%Y,ZNEAR,ZFAR) 
 ENDIF

 !## Always put zNear as far from the eye as you can tolerate. 
 
 CALL IMOD3D_ERROR('IMOD3D_DISPLAY')

 END SUBROUTINE IMOD3D_DISPLAY
 
 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_ORIENT()
 !###======================================================================
 IMPLICIT NONE
 TYPE (SPHERE3D) :: SLOOKFROM
 REAL(KIND=GLDOUBLE) :: Z
 
 IF(ORIENTINDEX.NE.0)RETURN
 
 CALL GLMATRIXMODE(GL_MODELVIEW)
 CALL GLPOPMATRIX()   !## pops off the top matrix second-from-the top becomes the top
 CALL GLPUSHMATRIX()  !## pushes all matrices in the current stack down one level, topmost is copied

 !## lookat is the coordinate 0,0,0
 !## lookfrom is the coordinate 10,-20,5
 !## scale factors 1,1,1

 SLOOKFROM=CART2SPHERE(LOOKFROM-LOOKAT)
 Z        =INIT_SHIFTZ-SLOOKFROM%RHO

 !## displacement in the left-corner
 CALL GLTRANSLATED(BOT%X,BOT%Y,BOT%Z)
! CALL GLTRANSLATED(-2.0_GLDOUBLE,-2.0_GLDOUBLE,-2.0_GLDOUBLE)
 !## no moving and/or zooming
 CALL GLTRANSLATED(0.0_GLDOUBLE,0.0_GLDOUBLE,Z)  !%z affected by zoom
 CALL GLROTATED(ANGLE%X, 0.0_GLDOUBLE, 0.0_GLDOUBLE, 1.0_GLDOUBLE)
 CALL GLROTATED(ANGLE%Y                              , COS(PI_OPENGL*ANGLE%X/180.0_GLDOUBLE), &
               -SIN(PI_OPENGL*ANGLE%X/180.0_GLDOUBLE), 0.0_GLDOUBLE                         )
 CALL GLTRANSLATED(-LOOKAT%X, -LOOKAT%Y, -LOOKAT%Z)

 CALL IMOD3D_SETCOLOR(OCOLOR)
 CALL GLLINEWIDTH(1.0_GLFLOAT)
 CALL GLCALLLIST(ORIENTINDEX)

 END SUBROUTINE IMOD3D_DISPLAY_ORIENT
 
 !###======================================================================
 SUBROUTINE IMOD3D_PLOT_CROSSSECTION()
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=GLDOUBLE) :: Z1,Z2 
 INTEGER :: I,J
 
 !## do not show 3d coordinates
 IF(.NOT.ASSOCIATED(XYZCROSS))RETURN

 Z1=TOP%Z
 Z2=BOT%Z

 !## if cross-section drawing is not activated
 IF(IDRAWCROSS.EQ.0)RETURN
 
 !## draw point of mouse position
 CALL GLPOINTSIZE(5.0_GLFLOAT)
 CALL GLBEGIN(GL_POINTS)
  CALL IMOD3D_SETCOLOR(WRGB(255,0,0))
  CALL GLVERTEX3D(INDPOS%X,INDPOS%Y,INDPOS%Z)
 CALL GLEND()

 DO J=1,SIZE(NXYZCROSS) 
  
  !## draw rest a filled polygon
  IF(NXYZCROSS(J).GT.1)THEN

  !## flat shading
!  CALL GLSHADEMODEL(GL_SMOOTH) !## GL_SMOOTH
   CALL GLENABLE(GL_LIGHTING)
   CALL GLCOLORMATERIAL(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE)
   CALL GLENABLE(GL_COLOR_MATERIAL)

   DO I=1,NXYZCROSS(J)-1
   !## top
!   CALL IMOD3D_SETCOLOR(WRGB(0,0,0))
!   CALL GLPOLYGONMODE(GL_BACK, GL_LINE); CALL GLPOLYGONMODE(GL_FRONT,GL_LINE) 
!   CALL GLLINEWIDTH(2.0_GLDOUBLE)
!   CALL GLBEGIN(GL_QUADS)
!    CALL GLVERTEX3D(XYZCROSS(I)%X  ,XYZCROSS(I)%Y  ,Z2)
!    CALL GLVERTEX3D(XYZCROSS(I)%X  ,XYZCROSS(I)%Y  ,Z1)
!    CALL GLVERTEX3D(XYZCROSS(I+1)%X,XYZCROSS(I+1)%Y,Z1)
!    CALL GLVERTEX3D(XYZCROSS(I+1)%X,XYZCROSS(I+1)%Y,Z2)
!   CALL GLEND()   
    CALL IMOD3D_SETCOLOR(WRGB(200,0,0))
    CALL GLPOLYGONMODE(GL_BACK, GL_FILL); CALL GLPOLYGONMODE(GL_FRONT,GL_FILL) 
    CALL IMOD3D_SETNORMALVECTOR((/XYZCROSS(I,J)%X  ,XYZCROSS(I,J)%Y,  Z2/), &
                                (/XYZCROSS(I,J)%X  ,XYZCROSS(I,J)%Y,  Z1/), &
                                (/XYZCROSS(I+1,J)%X,XYZCROSS(I+1,J)%Y,Z1/))
    CALL GLBEGIN(GL_QUADS)
     CALL GLVERTEX3D(XYZCROSS(I,J)%X  ,XYZCROSS(I,J)%Y  ,Z2)
     CALL GLVERTEX3D(XYZCROSS(I,J)%X  ,XYZCROSS(I,J)%Y  ,Z1)
     CALL GLVERTEX3D(XYZCROSS(I+1,J)%X,XYZCROSS(I+1,J)%Y,Z1)
     CALL GLVERTEX3D(XYZCROSS(I+1,J)%X,XYZCROSS(I+1,J)%Y,Z2)
    CALL GLEND()   

   ENDDO
 
   CALL GLDISABLE(GL_LIGHTING)
   CALL GLDISABLE(GL_COLOR_MATERIAL)

  ENDIF
 ENDDO
 
 CALL GLLINEWIDTH(2.0_GLFLOAT)
 CALL IMOD3D_SETCOLOR(WRGB(255,0,0))

 !## draw vertical line for last point
 DO J=1,SIZE(NXYZCROSS)
  IF(NXYZCROSS(J).LE.0)CYCLE
  CALL GLBEGIN(GL_LINES)
   I=NXYZCROSS(J)
   !## bottom
   CALL GLVERTEX3D(XYZCROSS(I,J)%X,XYZCROSS(I,J)%Y,Z1) 
   CALL GLVERTEX3D(XYZCROSS(I,J)%X,XYZCROSS(I,J)%Y,Z2) 
  CALL GLEND()
 ENDDO
 
 END SUBROUTINE IMOD3D_PLOT_CROSSSECTION
  
 !###======================================================================
 SUBROUTINE IMOD3D_PLOT_INDPOS()
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=GLDOUBLE) :: X1,X2,Y1,Y2,Z1,Z2

 !## do not show 3d coordinates
 IF(ISHOW3DCRD.NE.1)RETURN
 
 !## position unknown
 IF(INDPOS%X.EQ.INDPOS%Y.AND.INDPOS%X.EQ.INDPOS%Z)RETURN
 
 X1=BOT%X
 X2=TOP%X
 Y1=BOT%Y
 Y2=TOP%Y
 Z1=BOT%Z
 Z2=TOP%Z
 
 CALL GLPOINTSIZE(5.0_GLFLOAT)
 CALL GLBEGIN(GL_POINTS)
 CALL IMOD3D_SETCOLOR(WRGB(255,0,0))
 CALL GLVERTEX3D(INDPOS%X,INDPOS%Y,INDPOS%Z)
 CALL GLEND()

 CALL GLLINEWIDTH(1.0_GLFLOAT)
 CALL GLBEGIN(GL_LINE_STRIP)
  CALL GLVERTEX3D(INDPOS%X,      Y2,      Z1)
  CALL GLVERTEX3D(INDPOS%X,INDPOS%Y,      Z1)
  CALL GLVERTEX3D(INDPOS%X,INDPOS%Y,INDPOS%Z)
  CALL GLVERTEX3D(INDPOS%X,      Y2,INDPOS%Z)
  CALL GLVERTEX3D(INDPOS%X,      Y2,      Z1)
 CALL GLEND()
 CALL GLBEGIN(GL_LINE_STRIP)
  CALL GLVERTEX3D(INDPOS%X,INDPOS%Y,      Z1)
  CALL GLVERTEX3D(X2      ,INDPOS%Y,      Z1)
  CALL GLVERTEX3D(X2      ,INDPOS%Y,INDPOS%Z)
  CALL GLVERTEX3D(INDPOS%X,INDPOS%Y,INDPOS%Z)
  CALL GLVERTEX3D(INDPOS%X,INDPOS%Y,      Z1)
 CALL GLEND()
  
 END SUBROUTINE IMOD3D_PLOT_INDPOS
  
 !###======================================================================
 SUBROUTINE IMOD3D_PLOT_WELLS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 !## do not location of artificial wells
 IF(IDRAWWELL.EQ.0)RETURN

 CALL GLPOINTSIZE(7.5_GLFLOAT)
 CALL GLBEGIN(GL_POINTS)
 CALL IMOD3D_SETCOLOR(WRGB(25,255,0))
 DO I=1,NXYZWELL
  CALL GLVERTEX3D(XYZWELL(I)%X,XYZWELL(I)%Y,XYZWELL(I)%Z)
 ENDDO
 CALL GLEND()
  
 END SUBROUTINE IMOD3D_PLOT_WELLS
 
 !###======================================================================
 SUBROUTINE IMOD3D_LIGHT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB5)

 CALL WDIALOGGETTRACKBAR(IDF_TRACKBAR2,I); F_L1_AMBIENT=REAL(I)/100.0D0
 CALL WDIALOGGETTRACKBAR(IDF_TRACKBAR3,I); F_L1_DIFFUSE=REAL(I)/100.0D0
 CALL WDIALOGGETTRACKBAR(IDF_TRACKBAR4,I); F_L1_SPECULAR=REAL(I)/100.0D0
 
 L1_AMBIENT= (/F_L1_AMBIENT ,F_L1_AMBIENT ,F_L1_AMBIENT ,1.0_GLFLOAT/)
 L1_DIFFUSE= (/F_L1_DIFFUSE ,F_L1_DIFFUSE ,F_L1_DIFFUSE ,1.0_GLFLOAT/)
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

 SLOOKFROM    = CART2SPHERE(LOOKFROM-LOOKAT)
 ANGLE%X      =-180.0_GLDOUBLE*SLOOKFROM%THETA/PI_OPENGL - 90.0_GLDOUBLE
 ANGLE%Y      =-180.0_GLDOUBLE*SLOOKFROM%PHI/PI_OPENGL
 SHIFT%X      = INIT_SHIFTX
 SHIFT%Y      = INIT_SHIFTY
 SHIFT%Z      = INIT_SHIFTZ-SLOOKFROM%RHO
 XSCALE_FACTOR= INIT_XSCALE_FACTOR
 YSCALE_FACTOR= INIT_YSCALE_FACTOR
 ZSCALE_FACTOR= INIT_ZSCALE_FACTOR

 CALL IMOD3D_ERROR('IMOD3D_RESET_TO_INIT')

 END SUBROUTINE IMOD3D_RESET_TO_INIT

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_AXES(IMODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMODE

 !## draw axes,roundbox
 IF(IMODE.EQ.1.AND.IBNDBOX.EQ.1)THEN
  CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination
 !## draw filled background in gl_cull_face mode
  CALL IMOD3D_SETCOLOR(BCOLOR)
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

 END SUBROUTINE IMOD3D_DISPLAY_AXES

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_CLP()
 !###======================================================================
 IMPLICIT NONE
 REAL(GLDOUBLE) :: DX,DY,DZ
 REAL(GLDOUBLE) :: P
 INTEGER :: I
   
 DO I=1,NCLPLIST
  IF(CLPPLOT(I)%ISEL.EQ.1)THEN

   !## turn clipping plane on
   CALL GLENABLE(CLPPLANES(I))
   !## move clipping plane to appropriate location
   CALL GLPUSHMATRIX()

   !## west/east clipping plane
   IF(ABS(CLPPLOT(I)%EQN(1)).EQ.1.0_GLDOUBLE)THEN
    DX=(TOP%X-BOT%X)*DBLE(CLPPLOT(I)%IPOS)/500.0D0
    P = CLPPLOT(I)%X+DX*CLPPLOT(I)%EQN(1)
    CALL GLTRANSLATED(P,CLPPLOT(I)%Y,CLPPLOT(I)%Z)
   ENDIF
   !## south/north clipping plane
   IF(ABS(CLPPLOT(I)%EQN(2)).EQ.1.0_GLDOUBLE)THEN
    DY=(TOP%Y-BOT%Y)*DBLE(CLPPLOT(I)%IPOS)/500.0D0
    P = CLPPLOT(I)%Y+DY*CLPPLOT(I)%EQN(2)
    CALL GLTRANSLATED(CLPPLOT(I)%X,P,CLPPLOT(I)%Z)
   ENDIF
   !## top/bottom clipping plane
   IF(ABS(CLPPLOT(I)%EQN(3)).EQ.1.0_GLDOUBLE)THEN
    DZ=(TOP%Z-BOT%Z)*DBLE(CLPPLOT(I)%IPOS)/500.0D0
    P = CLPPLOT(I)%Z+DZ*CLPPLOT(I)%EQN(3)
    CALL GLTRANSLATED(CLPPLOT(I)%X,CLPPLOT(I)%Y,P)
   ENDIF

   CALL GLCLIPPLANE(CLPPLANES(I),CLPPLOT(I)%EQN)
   CALL GLPOPMATRIX()

  ELSE
 
   CALL GLDISABLE(CLPPLANES(I))
 
  ENDIF
 END DO

 !## plot clipping planes first than activate them
 DO I=1,NCLPLIST
  !## do not use inactive clipping planes
  IF(CLPPLOT(I)%ISEL.EQ.0)CYCLE
  !## do not plot clipping planes with zero thickness
  IF(CLPPLOT(I)%ITHICKNESS.LE.0)CYCLE
  !## turn on clipping plane again
  CALL GLDISABLE(CLPPLANES(I))
  CALL IMOD3D_DISPLAY_CLP_DRAW(I,0)
  !## turn on clipping plane again
  CALL GLENABLE(CLPPLANES(I))

 END DO

 CALL GLPOLYGONMODE(GL_BACK, GL_FILL); CALL GLPOLYGONMODE(GL_FRONT,GL_FILL)

 END SUBROUTINE IMOD3D_DISPLAY_CLP
 
 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_CLP_DRAW(I,ICOLOR)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I,ICOLOR
 INTEGER :: J
 REAL(GLDOUBLE),DIMENSION(4) :: X,Y,Z
 REAL(GLDOUBLE) :: DX,DY,DZ,OX,OY,OZ
 REAL(GLFLOAT) :: T
 LOGICAL :: LEX
    
 !## west/east clipping plane
 IF(ABS(CLPPLOT(I)%EQN(1)).EQ.1.0_GLDOUBLE)THEN
  DX=(TOP%X-BOT%X)*DBLE(CLPPLOT(I)%IPOS)/500.0D0
  DX=CLPPLOT(I)%X+DX*CLPPLOT(I)%EQN(1)
  OX=0.0_GLDOUBLE 
  X(1)=DX+OX; X(2)=X(1);  X(3)=X(1);  X(4)=X(1)
  Y(1)=BOT%Y; Y(2)=Y(1);  Y(3)=TOP%Y; Y(4)=Y(3)
  Z(1)=BOT%Z; Z(2)=TOP%Z; Z(3)=Z(2);  Z(4)=Z(1)
 ENDIF
 !## south/north clipping plane
 IF(ABS(CLPPLOT(I)%EQN(2)).EQ.1.0_GLDOUBLE)THEN
  DY=(TOP%Y-BOT%Y)*DBLE(CLPPLOT(I)%IPOS)/500.0D0
  DY=CLPPLOT(I)%Y+DY*CLPPLOT(I)%EQN(2)
  OY=0.0_GLDOUBLE 
  Y(1)=DY+OY; Y(2)=Y(1);  Y(3)=Y(1);  Y(4)=Y(1)
  X(1)=BOT%X; X(2)=X(1);  X(3)=TOP%X; X(4)=X(3)
  Z(1)=BOT%Z; Z(2)=TOP%Z; Z(3)=Z(2);  Z(4)=Z(1)
 ENDIF
 !## top/bottom clipping plane
 IF(ABS(CLPPLOT(I)%EQN(3)).EQ.1.0_GLDOUBLE)THEN
  DZ=(TOP%Z-BOT%Z)*DBLE(CLPPLOT(I)%IPOS)/500.0D0
  DZ=CLPPLOT(I)%Z+DZ*CLPPLOT(I)%EQN(3)
  OZ=0.0_GLDOUBLE 
  Z(1)=DZ+OZ; Z(2)=Z(1);  Z(3)=Z(1);  Z(4)=Z(1)
  X(1)=BOT%X; X(2)=X(1);  X(3)=TOP%X; X(4)=X(3)
  Y(1)=BOT%Y; Y(2)=TOP%Y; Y(3)=Y(2);  Y(4)=Y(1)
 ENDIF
 
 LEX=.FALSE.
 
 !## outline clipping planes
 IF(ICOLOR.EQ.0)THEN

  CALL GLPOLYGONMODE(GL_FRONT,GL_LINE); CALL GLPOLYGONMODE(GL_BACK, GL_LINE)
  T=REAL(CLPPLOT(I)%ITHICKNESS)
  IF(T.GT.0)THEN
   CALL IMOD3D_SETCOLOR(CLPPLOT(I)%ICOLOR)
   CALL GLLINEWIDTH(T)
   LEX=.TRUE.
  ENDIF
  
 !## solid fill of clipping planes
 ELSE

  LEX=.TRUE.
  CALL GLPOLYGONMODE(GL_BACK, GL_FILL); CALL GLPOLYGONMODE(GL_FRONT,GL_FILL)

  !## show shaded surface
  CALL GLCOLORMATERIAL(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE)
  CALL GLENABLE(GL_COLOR_MATERIAL)
  CALL IMOD3D_SETCOLOR(ICOLOR)
  CALL IMOD3D_SETNORMALVECTOR((/X(1),Y(1),Z(1)/),(/X(2),Y(2),Z(2)/),(/X(3),Y(3),Z(3)/))

 ENDIF
 
 IF(LEX)THEN
  CALL GLBEGIN(GL_QUADS)
  DO J=1,4
   CALL GLVERTEX3D(X(J),Y(J),Z(J))
  ENDDO
  CALL GLEND()
 ENDIF
 
 IF(ICOLOR.EQ.1)CALL GLDISABLE(GL_COLOR_MATERIAL)
 
 END SUBROUTINE IMOD3D_DISPLAY_CLP_DRAW

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_IPF(IMODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMODE
 INTEGER :: I,J,IIPF
 REAL(KIND=GLFLOAT) :: R
  
 !## associated file drawn
 DO I=1,NIPFLIST
  !## active based upon selection
  IF(IPFDLIST(3,I).EQ.0)CYCLE
  !## part of selected ipf
  IIPF=IPFDLIST(1,I); IF(IIPF.EQ.0)CYCLE
  !## selected in menu-field
  IF(IPFPLOT(IIPF)%ISEL.NE.1.OR.IPFLISTINDEX(I,IMODE).EQ.0)CYCLE
   
  !## turn off clipping as it is not effected by this selected IPF file
  IF(IPFPLOT(IIPF)%ICLIP.EQ.0)THEN
   DO J=1,NCLPLIST; CALL GLDISABLE(CLPPLANES(J)); END DO
  ENDIF

  IF(IMODE.EQ.1)then
   R=IPFPLOT(IIPF)%RADIUS
   !## line width
   IF(ISELECTED.EQ.I)THEN
    IF(IPFPLOT(IIPF)%IFANCY.EQ.0)THEN
     R=MIN(10.0D0,R*10.0D0)
     CALL GLLINEWIDTH(R)
     CALL GLPOINTSIZE(R)
    ELSEIF(IPFPLOT(IIPF)%IFANCY.EQ.1)THEN
     CALL GLPOLYGONMODE(GL_FRONT,GL_LINE); CALL GLPOLYGONMODE(GL_BACK, GL_LINE)
     CALL GLLINEWIDTH(1.0_GLFLOAT)
    ENDIF
   ELSE
    !## mark whenever drill has been selected/activated to be interpolated
    CALL GLLINEWIDTH(R) 
    CALL GLPOINTSIZE(R)
   ENDIF
  
   IF(IPFPLOT(IIPF)%IFANCY.EQ.1)THEN
    IF(IPFPLOT(IIPF)%ISHADE.EQ.1.AND.IMODE.EQ.1)THEN
     CALL GLENABLE(GL_LIGHTING); CALL GLDISABLE(GL_BLEND)
    ENDIF
   ENDIF

  ENDIF

  !## draw mesh for borehole or selected
  IF(IPFPLOT(IIPF)%IFILL.EQ.1.OR. &
     IPFPLOT(IIPF)%IFILL.EQ.3.OR.ISELECTED.EQ.I)CALL GLCALLLIST(IPFLISTINDEX(I,IMODE))
  
  IF(IMODE.EQ.1)THEN
   
   IF(IPFPLOT(IIPF)%IFANCY.EQ.1.AND.IPFPLOT(IIPF)%ISHADE.EQ.1)THEN 
    IF(IPFPLOT(IIPF)%IFILL.EQ.3)CALL GLDISABLE(GL_LIGHTING)
    CALL GLENABLE(GL_BLEND)

    !## draw mesh for borehole not selected
    IF(IPFPLOT(IIPF)%IFILL.EQ.2.OR.IPFPLOT(IIPF)%IFILL.EQ.3.AND.ISELECTED.NE.I)THEN

     IF(IPFPLOT(IIPF)%IFILL.EQ.3)CALL IMOD3D_SETCOLOR(WRGB(0,0,0))
     CALL GLLINEWIDTH(1.0_GLFLOAT)
     !## outline (showing rectangles)
     CALL GLPOLYGONMODE(GL_FRONT,GL_LINE); CALL GLPOLYGONMODE(GL_BACK, GL_LINE)

     CALL GLCALLLIST(IPFLISTINDEX(I,IMODE))

    ENDIF
   ENDIF
   
   CALL GLDISABLE(GL_LIGHTING)
   CALL GLPOLYGONMODE(GL_BACK, GL_FILL); CALL GLPOLYGONMODE(GL_FRONT,GL_FILL)

  ENDIF

  !## turn on clipping as it was not effected by this selected IPF file
  IF(IPFPLOT(IIPF)%ICLIP.EQ.0)THEN
   DO J=1,NCLPLIST; IF(CLPPLOT(J)%ISEL.EQ.1)CALL GLENABLE(CLPPLANES(J)); END DO
  ENDIF

 END DO

 !## draw ipf labels
 IF(IMODE.EQ.1)THEN
  !## associated file drawn
  DO I=1,NIPFLIST
   !## active based upon selection
   IF(IPFDLIST(3,I).EQ.0)CYCLE
   !## part of selected ipf
   IIPF=IPFDLIST(1,I) 
   !## cycle if not selected to plot labels
   IF(IPFPLOT(IIPF)%IPLOTLABELS.EQ.0)CYCLE
   !## selected in menu-field IPFLISTINDEX(:,3)
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
 SUBROUTINE IMOD3D_DISPLAY_IDF(IMODE,IT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,JJ,T
 REAL(KIND=DP_KIND) :: NT,FT,DZ,SDZ,TS,FS
 INTEGER,INTENT(IN) :: IMODE,IT
 REAL(KIND=GLDOUBLE) :: TSTACK
 LOGICAL :: LCLIP
  
 !## get stack-thickness
 SDZ=TOP%Z-BOT%Z; TS=0.0D0; FS=0.0D0
 DO I=1,SIZE(IDFLISTINDEX) 
  IF(IDFPLOT(I)%ISEL.NE.1.OR.IDFLISTINDEX(I)%INDEX(1).EQ.0)CYCLE
  TS=TS+REAL(IDFPLOT(I)%ISTACKED)/100.0D0
  FS=FS+1.0D0
 ENDDO
 IF(TS.GT.0.0D0)THEN
  !## get delta-z between interfaces
  DZ=0.0D0
  DO I=1,SIZE(IDFLISTINDEX)
   IDFPLOT(I)%FSTACKED=DZ
   !## correct dz for stacked idf file
   IF(IDFPLOT(I)%ISEL.EQ.1.AND.IDFLISTINDEX(I)%INDEX(1).NE.0)THEN
    TS=REAL(IDFPLOT(I)%ISTACKED)/100.0D0
    DZ=DZ+TS*SDZ
   ENDIF
  ENDDO
  DZ=DZ/2.0D0
  DO I=1,SIZE(IDFLISTINDEX)
   IDFPLOT(I)%FSTACKED=IDFPLOT(I)%FSTACKED-DZ
  ENDDO      
 ELSE
  IDFPLOT%FSTACKED=0.0D0; FS=0.0D0
 ENDIF
 
 !## get total transparancy
 FT=1.0D0
 IF(IT.EQ.1)THEN
  NT=0.0D0; DO I=1,SIZE(IDFLISTINDEX) 
   IF(IDFPLOT(I)%ISEL.NE.1.OR.IDFLISTINDEX(I)%INDEX(1).EQ.0)CYCLE
   NT=NT+1.0D0
  ENDDO
  !## fraction of the transparancy for each IDF-file (100% fraction)
  IF(NT.GT.0.0D0)FT=1.0D0/NT
 ENDIF
 
 DO I=1,SIZE(IDFLISTINDEX) 
  IF(IDFPLOT(I)%ISEL.NE.1.OR.IDFLISTINDEX(I)%INDEX(1).EQ.0)CYCLE
  !## skip time variant idffiles
  IF(IDFPLOT(I)%ICUBE.GT.5)CYCLE
  
  !## blend mode 
  IF(IMODE.EQ.1)THEN
   IF(IDFPLOT(I)%ITRANSPARANCY.EQ.1)THEN
    !## skip all transparant images in this cycle
    IF(IT.EQ.0)CYCLE
   ELSE
    !## skip all opaque images in this cycle
    IF(IT.EQ.1)CYCLE
   ENDIF
  ENDIF 
  
  !## turn off clipping as it is not effected by this selected IDF file
  LCLIP=.FALSE.
  IF(IDFPLOT(I)%ICLIP.EQ.0)THEN
   DO J=1,NCLPLIST; CALL GLDISABLE(CLPPLANES(J)); ENDDO
  ELSE
   DO J=1,NCLPLIST; IF(CLPPLOT(J)%ISEL.EQ.1)THEN; LCLIP=.TRUE.; EXIT; ENDIF; ENDDO
  ENDIF

  IF(FS.GT.0.0D0)THEN 
   CALL GLPUSHMATRIX()  !## pushes all matrices in the current stack down one level, topmost is copied
   TSTACK=IDFPLOT(I)%FSTACKED
   CALL GLTRANSLATED(0.0_GLDOUBLE, 0.0_GLDOUBLE, -TSTACK)
  ENDIF
  
  IF(IDFPLOT(I)%IFILL.EQ.1.OR.IDFPLOT(I)%IFILL.EQ.3)THEN

   !## blend mode 
   IF(IMODE.EQ.1.AND.IDFPLOT(I)%ITRANSPARANCY.EQ.1)THEN
    !## draw furthers first
    CALL GLENABLE(GL_BLEND)
    CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination
   ELSE
    !## full opaque mode
    CALL GLBLENDFUNC(GL_ONE,GL_ZERO)  !## (1) source (2) destination
   ENDIF
  
   CALL GLPOLYGONMODE(GL_BACK, GL_FILL); CALL GLPOLYGONMODE(GL_FRONT,GL_FILL) 

   IF(IDFPLOT(I)%ILEG.EQ.1)THEN
    !## apply transparancy
    IF(IDFPLOT(I)%ITRANSPARANCY.EQ.1)THEN
     T=MIN(50,INT(100*FT))
     CALL IMOD3D_SETCOLOR(IDFPLOT(I)%ICOLOR,T)
    ELSE
     CALL IMOD3D_SETCOLOR(IDFPLOT(I)%ICOLOR)
    ENDIF
   ENDIF

   !## turn on light if neccessary
   IF(IDFPLOT(I)%ISHADED.EQ.1)CALL GLENABLE(GL_LIGHTING)

   !## capping if clipping is active
   IF(LCLIP)THEN
    
    CALL IMOD3D_ERROR('STENCIL')

    !## process each clipping plane
    DO JJ=1,NCLPLIST
     IF(CLPPLOT(JJ)%ISEL.EQ.0)CYCLE
 
     !## not to be capped
     IF(CLPPLOT(JJ)%ICAP.EQ.1)CYCLE
     !## draw model
     CALL GLCALLLIST(IDFLISTINDEX(I)%INDEX(1))

    ENDDO
    
    !IF(.false.)THEN
    !
    ! CALL GLENABLE(GL_STENCIL_TEST)
    !
    ! !## process each clipping plane
    ! DO JJ=1,NCLPLIST
    !  IF(CLPPLOT(JJ)%ISEL.EQ.0)CYCLE; IF(CLPPLOT(JJ)%ICAP.EQ.0)CYCLE
    !  CALL GLCLEARSTENCIL(0_GLINT); CALL GLCLEAR(GL_STENCIL_BUFFER_BIT)
    !  
    !  !## to specify the conditions under which a fragment passes the stencil test, thus always except
    !  CALL GLSTENCILFUNC(GL_ALWAYS,1_GLINT,3_GLUINT)
    !  !## invert value each time (order: stencilfail,stencilpass-depthfail,stenciltest-and-depthpass)
    !  CALL GLSTENCILOP(GL_KEEP,GL_INVERT,GL_INVERT)
    !
    !  !## disable all clipping planes except current
    !  DO II=1,NCLPLIST; IF(CLPPLOT(II)%ISEL.EQ.1)CALL GLDISABLE(CLPPLANES(II)); ENDDO; CALL GLENABLE(CLPPLANES(JJ))
    !  !## colour buffer disabled
    !  CALL GLCOLORMASK(.FALSE._GLBOOLEAN,.FALSE._GLBOOLEAN,.FALSE._GLBOOLEAN,.FALSE._GLBOOLEAN)
    !
    !  !## draw model
    !  CALL GLCALLLIST(IDFLISTINDEX(I)%INDEX(1))
    !
    !  !## colour buffer enabled
    !  CALL GLCOLORMASK(.TRUE._GLBOOLEAN,.TRUE._GLBOOLEAN,.TRUE._GLBOOLEAN,.TRUE._GLBOOLEAN)
    ! 
    !  !## draw only where stencil is 1
    !  CALL GLSTENCILFUNC(GL_NOTEQUAL,0_GLINT,3_GLUINT)
    !
    !  !## turn on all clipping planes except current
    !  DO II=1,NCLPLIST; IF(CLPPLOT(II)%ISEL.EQ.1)CALL GLENABLE(CLPPLANES(II)); ENDDO; CALL GLDISABLE(CLPPLANES(JJ))
    !
    !  !## draw cap - use the clipplane vertices
    !  CALL IMOD3D_DISPLAY_CLP_DRAW(JJ,IDFPLOT(I)%ICOLOR)
    ! 
    ! ENDDO
    ! 
    ! CALL GLDISABLE(GL_STENCIL_TEST)
    !
    !ELSE

    !## process each clipping plane or quadratic order
    CALL GLENABLE(GL_STENCIL_TEST)
    DO JJ=1,NCLPLIST

     IF(CLPPLOT(JJ)%ISEL.EQ.0)CYCLE
     !## not to be capped
     IF(CLPPLOT(JJ)%ICAP.EQ.0)CYCLE

     CALL GLCLEARSTENCIL(0_GLINT)
     CALL GLCLEAR(GL_STENCIL_BUFFER_BIT)

     !## to specify the conditions under which a fragment passes the stencil test, thus always except
     CALL GLSTENCILFUNC(GL_ALWAYS,1_GLINT,3_GLUINT)
     !## invert value each time (order: stencilfail,stencilpass-depthfail,stenciltest-and-depthpass)
     CALL GLSTENCILOP(GL_KEEP,GL_INVERT,GL_INVERT)

     !## colour buffer disabled
     CALL GLCOLORMASK(.FALSE._GLBOOLEAN,.FALSE._GLBOOLEAN,.FALSE._GLBOOLEAN,.FALSE._GLBOOLEAN)
     CALL GLDEPTHMASK(.FALSE._GLBOOLEAN)
     !## draw model
     CALL GLCALLLIST(IDFLISTINDEX(I)%INDEX(1))
     !## colour buffer enabled
     CALL GLCOLORMASK(.TRUE._GLBOOLEAN,.TRUE._GLBOOLEAN,.TRUE._GLBOOLEAN,.TRUE._GLBOOLEAN)
     CALL GLDEPTHMASK(.TRUE._GLBOOLEAN)

     !## deactivate current clipping plane
     CALL GLDISABLE(CLPPLANES(JJ))

     !## draw only where stencil is 1
     CALL GLSTENCILFUNC(GL_NOTEQUAL,0_GLINT,3_GLUINT)

     !## draw cap - use the clipplane vertices
     CALL IMOD3D_DISPLAY_CLP_DRAW(JJ,IDFPLOT(I)%ICOLOR)
     CALL GLENABLE(CLPPLANES(JJ))
      
    ENDDO
    CALL GLDISABLE(GL_STENCIL_TEST)

    !## draw model
    CALL GLCALLLIST(IDFLISTINDEX(I)%INDEX(1))

!    ENDIF
    
   ELSE

    CALL GLCALLLIST(IDFLISTINDEX(I)%INDEX(1))

   ENDIF
   
   !## turn off light
   IF(IDFPLOT(I)%ISHADED.EQ.1)CALL GLDISABLE(GL_LIGHTING)

  ENDIF

  !## draw mesh
  IF(IDFPLOT(I)%IFILL.EQ.2.OR.IDFPLOT(I)%IFILL.EQ.3)THEN

   CALL GLENABLE(GL_LIGHTING)
   !## show lines to represent rectangles/triangles
   IF(IDFPLOT(I)%IFILL.EQ.2)CALL IMOD3D_SETCOLOR(IDFPLOT(I)%ICOLOR)
   IF(IDFPLOT(I)%IFILL.EQ.3)CALL IMOD3D_SETCOLOR(WRGB(0,0,0))
   CALL GLLINEWIDTH(1.0_GLFLOAT)
   !## outline (showing rectangles)
   CALL GLPOLYGONMODE(GL_FRONT,GL_LINE); CALL GLPOLYGONMODE(GL_BACK, GL_LINE)

   CALL GLCALLLIST(IDFLISTINDEX(I)%INDEX(1))
   CALL GLDISABLE(GL_LIGHTING)

  ENDIF

  !## turn on clipping as it was not effected by this selected IDF file
  IF(IDFPLOT(I)%ICLIP.EQ.0)THEN
   DO J=1,NCLPLIST; IF(CLPPLOT(J)%ISEL.EQ.1)CALL GLENABLE(CLPPLANES(J)); END DO
  ENDIF

  !## pops off the top matrix second-from-the top becomes the top
  IF(FS.GT.0.0D0)CALL GLPOPMATRIX()   

 END DO

 !## default
 CALL GLPOLYGONMODE(GL_BACK, GL_FILL); CALL GLPOLYGONMODE(GL_FRONT,GL_FILL)

 END SUBROUTINE IMOD3D_DISPLAY_IDF

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_TIME_IDF()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,II,J,JJ,IU,JD,IYR,IMH,IDY,IHR,IMT,ISC,NC,NF
 
 DO I=1,SIZE(IDFLISTINDEX) 
 
  IF(IDFPLOT(I)%ISEL.NE.1)CYCLE
  !## skip time variant idffiles
  IF(IDFPLOT(I)%ICUBE.LT.5)CYCLE

  !## turn off clipping as it is not effected by this selected IDF file
  IF(IDFPLOT(I)%ICLIP.EQ.0)THEN
   DO J=1,NCLPLIST; CALL GLDISABLE(CLPPLANES(J)); ENDDO
  ENDIF
  
  IF(IDFPLOT(I)%IFILL.EQ.1.OR.IDFPLOT(I)%IFILL.EQ.3)THEN

   !## full opaque mode
   CALL GLBLENDFUNC(GL_ONE,GL_ZERO)  !## (1) source (2) destination
   CALL GLPOLYGONMODE(GL_BACK, GL_FILL); CALL GLPOLYGONMODE(GL_FRONT,GL_FILL) 
   CALL GLCOLORMATERIAL(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE)
   CALL GLENABLE(GL_COLOR_MATERIAL)
 
   !## turn on light if neccessary
   IF(IDFPLOT(I)%ISHADED.EQ.1)CALL GLENABLE(GL_LIGHTING)

   IF(IDFPLOT(I)%D4ITIME.NE.IDFPLOT(I)%D4JTIME)THEN
   
    !## use always column of legend
    IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(PREFVAL(1))//'\TMP\TVAR_IDF_COLOUR_F'//TRIM(ITOS(I))//'.4DV',FORM='UNFORMATTED',STATUS='OLD',ACTION='READ')
    READ(IU) NC,NF

    DO II=1,NF 

     READ(IU) JD,IYR,IMH,IDY,IHR,IMT,ISC 

     !## read colours
     IF(II.EQ.IDFPLOT(I)%D4ITIME)THEN
      READ(IU) (IDFPLOT(I)%ICLR(JJ),JJ=1,NC); EXIT
!      READ(IU) ((IDFPLOT(I)%ICLR(JJ,JJJ),JJJ=1,3),JJ=1,NC); EXIT
     ELSE
      READ(IU); CYCLE
     ENDIF
   
    ENDDO
    CLOSE(IU); IDFPLOT(I)%D4JTIME=IDFPLOT(I)%D4ITIME

   ELSE
    
    NC=SIZE(IDFPLOT(I)%ICLR,1)
    
   ENDIF
   
   DO J=1,NC 
  
    !## nodata pixel
    IF(IDFLISTINDEX(I)%INDEX(J).EQ.0)CYCLE
    !## not selected
    IF(IDFPLOT(I)%IACT(J).EQ.INT(0,1))CYCLE
    
    IF(IDFPLOT(I)%ICLR(J).EQ.WRGB(255,255,255))CYCLE
!    IF(IDFPLOT(I)%ICLR(J,1).EQ.-1)CYCLE
    CALL IMOD3D_SETCOLOR(IDFPLOT(I)%ICLR(J))
!    XR=REAL(IDFPLOT(I)%ICLR(J,1))/255.0_GLDOUBLE
!    XG=REAL(IDFPLOT(I)%ICLR(J,2))/255.0_GLDOUBLE
!    XB=REAL(IDFPLOT(I)%ICLR(J,3))/255.0_GLDOUBLE
!    CALL GLCOLOR3D(XR,XG,XB)
    CALL GLCALLLIST(IDFLISTINDEX(I)%INDEX(J))
     
   ENDDO
 
   !## turn off light
   IF(IDFPLOT(I)%ISHADED.EQ.1)CALL GLDISABLE(GL_LIGHTING)
   CALL GLDISABLE(GL_COLOR_MATERIAL)

  ENDIF

  !!## draw mesh
  !IF(IDFPLOT(I)%IFILL.EQ.2.OR.IDFPLOT(I)%IFILL.EQ.3)THEN
  !
  ! CALL GLENABLE(GL_LIGHTING)
  ! !## show lines to represent rectangles/triangles
  ! IF(IDFPLOT(I)%IFILL.EQ.2)CALL IMOD3D_SETCOLOR(IDFPLOT(I)%ICOLOR)
  ! IF(IDFPLOT(I)%IFILL.EQ.3)CALL IMOD3D_SETCOLOR(WRGB(0,0,0))
  ! CALL GLLINEWIDTH(1.0_GLFLOAT)
  ! !## outline (showing rectangles)
  ! CALL GLPOLYGONMODE(GL_FRONT,GL_LINE); CALL GLPOLYGONMODE(GL_BACK, GL_LINE)
  !
  ! DO J=1,SIZE(IDFLISTINDEX(I)%INDEX)
  !
  !  IF(IDFLISTINDEX(I)%INDEX(J).EQ.0)CYCLE
  !  CALL GLCALLLIST(IDFLISTINDEX(I)%INDEX(J))
  !
  ! ENDDO
  !
  ! CALL GLCALLLIST(IDFLISTINDEX(I)%INDEX(1))
  !
  ! CALL GLDISABLE(GL_LIGHTING)
  !
  !ENDIF
  
  !## turn on clipping as it was not effected by this selected IDF file
  IF(IDFPLOT(I)%ICLIP.EQ.0)THEN
   DO J=1,NCLPLIST; IF(CLPPLOT(J)%ISEL.EQ.1)CALL GLENABLE(CLPPLANES(J)); END DO
  ENDIF

 END DO

 !## default
 CALL GLPOLYGONMODE(GL_BACK, GL_FILL); CALL GLPOLYGONMODE(GL_FRONT,GL_FILL)

 END SUBROUTINE IMOD3D_DISPLAY_TIME_IDF

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_IFF()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J
 REAL(KIND=GLFLOAT) :: XW

 !## opaque mode
 CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination

 DO I=1,NIFFLIST
  IF(IFFPLOT(I)%ISEL.NE.1.OR.IFFLISTINDEX(I).EQ.0)CYCLE

  !## turn off clipping as it is not effected by this selected IFF file
  IF(IFFPLOT(I)%ICLIP.EQ.0)THEN
   DO J=1,NCLPLIST; CALL GLDISABLE(CLPPLANES(J)); END DO
  ENDIF

  IF(MP(IFFPLOT(I)%IPLOT)%ILEG.EQ.0)THEN
   CALL IMOD3D_SETCOLOR(MP(IFFPLOT(I)%IPLOT)%SCOLOR)
  ENDIF
  XW=REAL(IFFPLOT(I)%ITHICKNESS)
  CALL GLLINEWIDTH(XW)
  CALL GLCALLLIST(IFFLISTINDEX(I))

  !## turn on clipping as it was not effected by this selected IFF file
  IF(IFFPLOT(I)%ICLIP.EQ.0)THEN
   DO J=1,NCLPLIST; IF(CLPPLOT(J)%ISEL.EQ.1)CALL GLENABLE(CLPPLANES(J)); END DO
  ENDIF

 END DO

 END SUBROUTINE IMOD3D_DISPLAY_IFF

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_SOL() 
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J

 !## plot filled in cross-sections
 DO I=1,NSOLLIST

  IF(SOLPLOT(I)%ISEL.EQ.0)CYCLE
  IF(SOLLISTINDEX(I,1).EQ.0)CYCLE

  !## turn off clipping as it is not effected by this selected SOL file
  IF(SOLPLOT(I)%ICLIP.EQ.0)THEN
   DO J=1,NCLPLIST; CALL GLDISABLE(CLPPLANES(J)); END DO
  ENDIF

  IF(SOLPLOT(I)%IBITMAP.EQ.0)THEN

   !## showing surfaces (polygons)
   IF(SOLPLOT(I)%IINTERFACE.EQ.0)THEN 

    CALL GLENABLE(GL_LIGHTING)
    !## opaque mode
    CALL GLBLENDFUNC(GL_ONE,GL_ZERO)  !## (1) source (2) destination
    CALL GLPOLYGONMODE(GL_FRONT,GL_FILL); CALL GLPOLYGONMODE(GL_BACK, GL_FILL)

    !## draw cross-section
    CALL GLCALLLIST(SOLLISTINDEX(I,1))

   ENDIF

  ELSE

   IF(SOLLISTINDEX(I,2).NE.0)THEN
    !## bitmaps
    CALL GLCALLLIST(SOLLISTINDEX(I,2))
   ENDIF

  ENDIF
  
  !## show lines to represent rectangles/triangles
  IF(SOLPLOT(I)%IINTERFACE.EQ.1)THEN
   CALL GLLINEWIDTH(1.0_GLFLOAT)
   !## outline (showing rectangles)
   CALL GLPOLYGONMODE(GL_FRONT,GL_LINE); CALL GLPOLYGONMODE(GL_BACK, GL_LINE)
   CALL GLDISABLE(GL_LIGHTING)
   CALL GLCALLLIST(SOLLISTINDEX(I,1))
  ENDIF

  !## turn on clipping as it was not effected by this selected SOL file
  IF(SOLPLOT(I)%ICLIP.EQ.0)THEN
   DO J=1,NCLPLIST; IF(CLPPLOT(J)%ISEL.EQ.1)CALL GLENABLE(CLPPLANES(J)); END DO
  ENDIF

 END DO

 CALL GLDISABLE(GL_LIGHTING)

 CALL GLCOLOR4D(1.0_GLDOUBLE,1.0_GLDOUBLE,1.0_GLDOUBLE,0.0_GLDOUBLE)

 END SUBROUTINE IMOD3D_DISPLAY_SOL

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_PL()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,ICLR,N,IG
 REAL(KIND=DP_KIND) :: FCT
 REAL(KIND=GLFLOAT) :: XWIDTH
 
 !## draw start-locations
 IF(PL%IPLOTSP.EQ.1)THEN
  DO IG=1,SIZE(STPLISTINDEX)
   IF(STPLISTINDEX(IG).NE.0)THEN
    !## skip current particle-startpoints
    IF(SP(IG)%IACT.EQ.0)CYCLE 
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
 SUBROUTINE IMOD3D_DISPLAY_GEN(IT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IT
 INTEGER :: I,J
 REAL(KIND=GLFLOAT) :: XW

 DO I=1,NGENLIST
  IF(GENPLOT(I)%ISEL.NE.1.OR.GENLISTINDEX(I).EQ.0)CYCLE

  !## blend mode 
  IF(GENPLOT(I)%ITRANSPARANCY.LT.100)THEN
   !## skip all transparant images in this cycle
   IF(IT.EQ.0)CYCLE
  ELSE
   !## skip all opaque images in this cycle
   IF(IT.EQ.1)CYCLE
  ENDIF

  !## turn off clipping as it is not effected by this selected GEN file
  IF(GENPLOT(I)%ICLIP.EQ.0)THEN
   DO J=1,NCLPLIST; CALL GLDISABLE(CLPPLANES(J)); END DO
  ENDIF

  !## blend mode 
  IF(GENPLOT(I)%ITRANSPARANCY.LT.100)THEN
   !## draw furthers first
   CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination
  ELSE
   !## opaque mode
   CALL GLBLENDFUNC(GL_ONE,GL_ZERO)  !## (1) source (2) destination
  ENDIF
  
  CALL GLPOLYGONMODE(GL_BACK, GL_FILL); CALL GLPOLYGONMODE(GL_FRONT,GL_FILL) 

  IF(GENPLOT(I)%L3D)THEN
   !## show shaded surface
   IF(GENPLOT(I)%ITRANSPARANCY.LT.100)THEN
    CALL IMOD3D_SETCOLOR(GENPLOT(I)%ICOLOR,GENPLOT(I)%ITRANSPARANCY) 
   ELSE
    CALL IMOD3D_SETCOLOR(GENPLOT(I)%ICOLOR)
   ENDIF
  ELSE
   CALL IMOD3D_SETCOLOR(GENPLOT(I)%ICOLOR)
   XW=REAL(GENPLOT(I)%ITHICKNESS)
   CALL GLLINEWIDTH(XW)
  ENDIF

  !## turn on light if neccessary
  IF(GENPLOT(I)%ISHADE.EQ.1)THEN
   !## flat shading
   CALL GLENABLE(GL_LIGHTING)
  ENDIF

  !## vertical shift the polygon
  IF(.NOT.GENPLOT(I)%L3D)THEN
   CALL GLPUSHMATRIX()
   CALL GLTRANSLATED(0.0_GLDOUBLE, 0.0_GLDOUBLE, TOP%Z)
  ENDIF
  
  CALL GLCALLLIST(GENLISTINDEX(I))

  IF(.NOT.GENPLOT(I)%L3D)THEN
   CALL GLPOPMATRIX()
  ENDIF

  !## turn on clipping as it was not effected by this selected GEN file
  IF(GENPLOT(I)%ICLIP.EQ.0)THEN
   DO J=1,NCLPLIST; IF(CLPPLOT(J)%ISEL.EQ.1)CALL GLENABLE(CLPPLANES(J)); END DO
  ENDIF

 END DO

 !## turn of light
 CALL GLDISABLE(GL_LIGHTING)

 END SUBROUTINE IMOD3D_DISPLAY_GEN

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_BMP()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 REAL(GLDOUBLE) :: ALPHA
 
 !## not background bitmap plotting
 IF(IACTBITMAP.EQ.0)RETURN

 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB5)
 CALL WDIALOGGETTRACKBAR(IDF_TRACKBAR6,ITRANSPARANCYBITMAP)
 
 !## blend mode 
 IF(ITRANSPARANCYBITMAP.LT.100)THEN
  !## draw furthers first
  CALL GLBLENDFUNC(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)  !## (1) source (2) destination
  ALPHA=REAL(ITRANSPARANCYBITMAP)/100.0D0
 ELSE
  !## opaque mode
  CALL GLBLENDFUNC(GL_ONE,GL_ZERO)  !## (1) source (2) destination
 ENDIF

 CALL GLCOLOR4D(1.0_GLDOUBLE,1.0_GLDOUBLE,1.0_GLDOUBLE,ALPHA) 

 I=1
 CALL GLENABLE(GL_TEXTURE_2D)
 CALL GLCALLLIST(BMPLISTINDEX(I))
 CALL GLDISABLE(GL_TEXTURE_2D)

 END SUBROUTINE IMOD3D_DISPLAY_BMP

 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_LEGEND() 
 !###======================================================================
 IMPLICIT NONE

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

! CALL GLDISABLE(GL_BLEND)
 CALL GLDISABLE(GL_LIGHT0)
 CALL GLENABLE(GL_BLEND)
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
 INTEGER(KIND=GLFLOAT),DIMENSION(:),ALLOCATABLE :: INDX
 INTEGER,DIMENSION(:),ALLOCATABLE :: IRGB 
 INTEGER :: IHANDLE,I
 CHARACTER(LEN=256) :: FNAME
 REAL(KIND=GLDOUBLE),DIMENSION(3) :: GLCOLOR
 LOGICAL :: LEX
 
 LEX=.TRUE.
 
 IF(ID.EQ.ID_SAVEAS)THEN
  IF(.NOT.UTL_WSELECTFILE('All Known Files (*.bmp;*.pcx;*.png;*.jpg)|*.bmp;*.pcx;*.png;*.jpg|BitMap (*.bmp)|*.bmp| &
                           ZSoft PC Paintbrush (*.pcx)|*.pcx|Portable Network Graphic image (*.png)|*.png|JPEG Image (*.jpg)|*.jpg|',&
                   SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                   'Save Current Image to BitMap (*.bmp;*.pcx;*.png;*.jpg)'))RETURN
 ENDIF
 
 !## capture entire screen
 CALL GLGETINTEGERV(GL_VIEWPORT,IVIEWPORT)
 NDX=IVIEWPORT(3); NDY=IVIEWPORT(4)

 ALLOCATE(IRGB(NDX*NDY))

 IF(LEX)THEN
  ALLOCATE(FRGB(NDX*NDY*3))
 ELSE
  ALLOCATE(INDX(NDX*NDY))
 ENDIF
 
!Specifies the format of the pixel data. The following symbolic values are accepted: GL_STENCIL_INDEX, GL_DEPTH_COMPONENT, GL_DEPTH_STENCIL,
!GL_RED, GL_GREEN, GL_BLUE, GL_RGB, GL_BGR, GL_RGBA, and GL_BGRA. 
 
 CALL GLREADBUFFER(GL_FRONT)
 IF(LEX)THEN
  CALL GLREADPIXELS(0_GLINT,0_GLINT,NDX,NDY,GL_RGB,GL_FLOAT,FRGB)
 ELSE
  CALL GLREADPIXELS(0_GLINT,0_GLINT,NDX,NDY,GL_STENCIL_INDEX,GL_FLOAT,INDX)
 ENDIF
 
 DO I=0,(NDX*NDY)-1
  IF(LEX)THEN
   GLCOLOR(1)=FRGB(I*3+1); GLCOLOR(2)=FRGB(I*3+2); GLCOLOR(3)=FRGB(I*3+3)
  ELSE
   GLCOLOR(1)=INDX(I+1)
   GLCOLOR(2)=INDX(I+1)
   GLCOLOR(3)=INDX(I+1)
  ENDIF
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
 
 DEALLOCATE(IRGB)
 IF(LEX)THEN
  DEALLOCATE(FRGB)
 ELSE
  DEALLOCATE(INDX)
 ENDIF
 CALL GLREADBUFFER(GL_BACK)
 
 IF(ID.EQ.ID_SAVEAS)THEN
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Successfully saved the current image to'//CHAR(13)//TRIM(FNAME),'Information')
 ELSE
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Successfully copied the current image to'//CHAR(13)//'the Windows Clipboard','Information')
 ENDIF
 
 END SUBROUTINE IMOD3D_DISPLAY_SAVE
 
 !###======================================================================
 SUBROUTINE IMOD3D_DISPLAY_SAVE_STENCIL()
 !###======================================================================
 IMPLICIT NONE
 INTEGER(KIND=GLSIZEI) :: NDX,NDY 
 INTEGER(GLINT) :: IVIEWPORT(4) 
! REAL(KIND=GLDOUBLE),DIMENSION(:),ALLOCATABLE :: FRGB
 INTEGER(KIND=GLINT),DIMENSION(:),ALLOCATABLE :: INDX
 INTEGER,DIMENSION(:),ALLOCATABLE :: IRGB 
 INTEGER :: IHANDLE,I
 CHARACTER(LEN=256) :: FNAME
 REAL(KIND=GLDOUBLE),DIMENSION(3) :: GLCOLOR
 
 FNAME='D:\STENCIL.BMP'
  
 !## capture entire screen
 CALL GLGETINTEGERV(GL_VIEWPORT,IVIEWPORT)
 NDX=IVIEWPORT(3); NDY=IVIEWPORT(4)

 ALLOCATE(IRGB(NDX*NDY))
 ALLOCATE(INDX(NDX*NDY))
 
!Specifies the format of the pixel data. The following symbolic values are accepted: GL_STENCIL_INDEX, GL_DEPTH_COMPONENT, GL_DEPTH_STENCIL,
!GL_RED, GL_GREEN, GL_BLUE, GL_RGB, GL_BGR, GL_RGBA, and GL_BGRA. 
 
! CALL GLPIXELSTOREI(GL_UNPACK_ALIGNMENT, 1)
 CALL GLREADBUFFER(GL_BACK)
 CALL GLREADPIXELS(0_GLINT,0_GLINT,NDX,NDY,GL_STENCIL_INDEX,GLINT,INDX)
! CALL GLREADBUFFER(GL_FRONT)
! CALL GLREADPIXELS(0_GLINT,0_GLINT,NDX,NDY,GL_STENCIL_INDEX,GLINT,INDX)

 CALL IMOD3D_ERROR('SAVE STENCIL')

 DO I=0,(NDX*NDY)-1
  GLCOLOR(1)=INDX(I+1)
  GLCOLOR(2)=INDX(I+1)
  GLCOLOR(3)=INDX(I+1)
  CALL IMOD3D_GETCOLOR(IRGB(I+1),GLCOLOR)
 ENDDO
 
 CALL WBITMAPCREATE(IHANDLE,NDX,NDY)  
 CALL WBITMAPGETDATA(IHANDLE,IRGB)   
 !## switch vertically
 CALL WBITMAPMIRROR(IHANDLE,1)

 CALL WBITMAPSAVE(IHANDLE,FNAME)
 CALL WBITMAPDESTROY(IHANDLE)
 
 DEALLOCATE(IRGB)
 DEALLOCATE(INDX)
 CALL GLREADBUFFER(GL_BACK)

 END SUBROUTINE IMOD3D_DISPLAY_SAVE_STENCIL
 
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
       DEMO%CONFLAG = 1 !## use "interfaces" as default setting
      ELSE
       DEMO%CONFLAG = IDFPLOT(NIDFLIST)%ICONFIG
      ENDIF
      DEMO%IFILL = IDFPLOT(NIDFLIST)%IFILL
      DEMO%ACCFLAG=IDFPLOT(NIDFLIST)%IACC
     ENDIF
    ENDIF
   ENDDO

   CALL MAIN_UTL_SAVE_IMF(FNAME,1)
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
 CALL GLROTATED(ANGLE%Y                              , COS(PI_OPENGL*ANGLE%X/180.0_GLDOUBLE), &
               -SIN(PI_OPENGL*ANGLE%X/180.0_GLDOUBLE), 0.0_GLDOUBLE                          )
 CALL GLTRANSLATED(-LOOKAT%X, -LOOKAT%Y, -LOOKAT%Z)
 CALL GLSCALED(XSCALE_FACTOR,YSCALE_FACTOR,ZSCALE_FACTOR)
 
 CALL IMOD3D_ERROR('IMOD3D_RESET_VIEW')

 END SUBROUTINE IMOD3D_RESET_VIEW

END MODULE MOD_3D_DISPLAY

