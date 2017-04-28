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
MODULE MOD_3D

USE MOD_3D_PAR
USE MODPLOT, ONLY : MPW,MXMPLOT,MP
USE MOD_UTL, ONLY : UTL_MESSAGEHANDLE,UTL_CAP,INVERSECOLOUR,ITOS,UTL_GETUNIT,UTL_MESSAGEHANDLE3D,UTL_IDFGETCLASS
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_3D_SETTINGS, ONLY : IMOD3D_SETTINGSALL,IMOD3D_SETTINGSMAIN,IMOD3D_SETTINGSINIT
USE MOD_3D_DISPLAY, ONLY : IMOD3D_DISPLAY,IMOD3D_DISPLAY_IPF,IMOD3D_RESET_TO_INIT,IMOD3D_LIGHT,IMOD3D_DISPLAY_SAVE,IMOD3D_DEMO_SAVE
USE MOD_3D_UTL, ONLY : IMOD3D_CLOSE,IMOD3D_RETURNCOLOR,IMOD3D_SETCOLOR
USE MOD_3D_ENGINE
USE MOD_3D_PROCESS
USE MOD_PLINES_PAR, ONLY : IDF

INTEGER(GLINT),DIMENSION(1) :: MAXSTDEPTH,STDEPTH !## max./current stack depth selection
INTEGER(GLINT),DIMENSION(1) :: DUMMY
INTEGER(GLINT),DIMENSION(1) :: MAXCPLANES,MBITS
REAL,PARAMETER :: G2R=360.0/(2.0*PI) 

CONTAINS

 !###======================================================================
 SUBROUTINE IMOD3D_MAIN_MESSAGES(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 
 !## process messages from the 3d-settings dialog
 IF(MESSAGE%WIN.EQ.ID_D3DSETTINGS     .OR.MESSAGE%WIN.EQ.ID_D3DSETTINGS_TAB1.OR.MESSAGE%WIN.EQ.ID_D3DSETTINGS_TAB2.OR. &
    MESSAGE%WIN.EQ.ID_D3DSETTINGS_TAB3.OR.MESSAGE%WIN.EQ.ID_D3DSETTINGS_TAB4.OR.MESSAGE%WIN.EQ.ID_D3DSETTINGS_TAB5.OR. &
    MESSAGE%WIN.EQ.ID_D3DSETTINGS_TAB6.OR.MESSAGE%WIN.EQ.ID_D3DSETTINGS_TAB7.OR.MESSAGE%WIN.EQ.ID_D3DSETTINGS_TAB8.OR. &
    MESSAGE%WIN.EQ.ID_D3DSETTINGS_TAB9)THEN
   CALL IMOD3D_SETTINGSMAIN(ITYPE,MESSAGE)

 !## process only those within the current 3D-window
 ELSEIF(MESSAGE%WIN.EQ.IWIN)THEN

  CALL WINDOWSELECT(MESSAGE%WIN)
  
  SELECT CASE(ITYPE)
   CASE (MENUSELECT)
    CALL IMOD3D_MENUSELECT(MESSAGE%VALUE1)
   CASE (CLOSEREQUEST)
    CALL IMOD3D_CLOSE()
  END SELECT
 ENDIF

 END SUBROUTINE IMOD3D_MAIN_MESSAGES
 
 !###======================================================================
 SUBROUTINE IMOD3D_MENUSELECT(ID) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER :: IOPT,DID,ITYPE
 TYPE(WIN_MESSAGE) :: MESSAGE
 REAL(KIND=GLDOUBLE) :: DX,DZ
 
 SELECT CASE (ID)

  CASE (ID_FLYPATH)
   DID=WINFODIALOG(CURRENTDIALOG)
   CALL WDIALOGSELECT(ID_D3DSETTINGS_RENDER)       
   CALL WDIALOGSHOW(-1,-1,0,3)
   DO
    CALL WMESSAGE(ITYPE,MESSAGE)
    SELECT CASE (ITYPE)
     CASE (FIELDCHANGED)
      SELECT CASE (MESSAGE%VALUE2)
      END SELECT
     CASE (EXPOSE)
      CALL IMOD3D_DISPLAY(1)
     CASE (PUSHBUTTON)
      SELECT CASE (MESSAGE%VALUE1)
       CASE (IDOK,IDCANCEL)
        EXIT
      END SELECT
    END SELECT
   ENDDO
   CALL WDIALOGHIDE(); CALL WDIALOGSELECT(DID)

  CASE (ID_ZRATIO1,ID_ZRATIO2,ID_ZRATIO3,ID_ZRATIO4,ID_ZRATIO5)
   CALL WMENUSETSTATE(ID_ZRATIO1,ITEMCHECKED,0)
   CALL WMENUSETSTATE(ID_ZRATIO2,ITEMCHECKED,0)
   CALL WMENUSETSTATE(ID_ZRATIO3,ITEMCHECKED,0)
   CALL WMENUSETSTATE(ID_ZRATIO4,ITEMCHECKED,0)
   CALL WMENUSETSTATE(ID_ZRATIO5,ITEMCHECKED,0)
   CALL WMENUSETSTATE(ID,ITEMCHECKED,1)
   CALL IMOD3D_SETUPDISPLAY_MISC(.TRUE.)
   CALL IMOD3D_DISPLAY(1)

  CASE (ID_WALK)
   WALK_MODE=WMENUGETSTATE(ID_WALK,ITEMCHECKED)
   WALK_MODE=ABS(WALK_MODE-1)
   CALL WMENUSETSTATE(ID_WALK,ITEMCHECKED,WALK_MODE)
   CALL WMENUSETSTATE(ID_3DLEFT,ITEMENABLED,ABS(WALK_MODE-1))
   CALL WMENUSETSTATE(ID_3DRIGHT,ITEMENABLED,ABS(WALK_MODE-1))
   CALL WMENUSETSTATE(ID_3DWHEEL,ITEMENABLED,ABS(WALK_MODE-1))
   CALL WMENUSETSTATE(ID_3DCURSOR,ITEMENABLED,ABS(WALK_MODE-1))
   !## reset view ...
   IF(WALK_MODE.EQ.0)THEN
    CALL GLMATRIXMODE(GL_PROJECTION); CALL GLLOADIDENTITY()
    FOVY=10_GLDOUBLE; CALL GLUPERSPECTIVE(FOVY,RAT,ZNEAR,ZFAR)    !## angle(fovy), aspect (ratio:w/h), near () and far()    
    CALL IMOD3D_SETLOOKAT_LOOKFROM()
    CALL IMOD3D_RESET_TO_INIT()
    CALL IMOD3D_DISPLAY(1)
   ELSE
    CALL IMOD3D_GET_HEADING_TILE(LENXY=DX,LENXZ=DZ)
    HEADING=(90.0-ANGLE%X)/(360.0/(2.0*PI))
    TILT   =(ANGLE%Y-90.0)/(360.0/(2.0*PI))
    LOOKFROM%X=LOOKAT%X-COS(HEADING)*DX
    LOOKFROM%Y=LOOKAT%Y-SIN(HEADING)*DX 
    LOOKFROM%Z=LOOKAT%Z-SIN(TILT)*DZ 
    CALL GLMATRIXMODE(GL_PROJECTION); CALL GLLOADIDENTITY()
    FOVY=60_GLDOUBLE; CALL GLUPERSPECTIVE(FOVY,RAT,ZNEAR,ZFAR)    !## angle(fovy), aspect (ratio:w/h), near () and far()
    CALL IMOD3D_RESET_TO_INIT()
    CALL IMOD3D_DISPLAY(1)
   ENDIF
!  CASE (ID_PRINT)
!   CALL WglSelect(Disabled) 
!   CALL IGrSelect(DrawWin,IWIN)  ! Select window as Winteracter target
!   CALL IGrPrintImageSelect(10)
!   CALL WPrintImageOptions(HcControlPanel)
!   CALL IGrPrintImage()        ! Print window   
!   CALL WGLSELECT(1,IWIN,WGLDOUBLEBUFFER)
!   CALL IMOD3D_DISPLAY(1)
  CASE (ID_SAVEAS,ID_CLIPBOARD)
   CALL IMOD3D_DISPLAY_SAVE(ID)
  CASE (ID_SAVEAS_DEMO)
   CALL IMOD3D_DEMO_SAVE(ID)
  CASE (ID_3DQUIT)
   CALL IMOD3D_CLOSE() 
  CASE (ID_3DLEFT_ROTATE:ID_3DLEFT_XY) 
   DO IOPT = ID_3DLEFT_ROTATE,ID_3DLEFT_XY 
    CALL WMENUSETSTATE(IOPT,ITEMCHECKED,WINTOFF)
   END DO
   CALL WMENUSETSTATE(ID,ITEMCHECKED,WINTON)
   LEFT_BUTTON_FUNC = ID - ID_3DLEFT
  CASE (ID_3DRIGHT_ROTATE:ID_3DRIGHT_XY) 
   DO IOPT = ID_3DRIGHT_ROTATE,ID_3DRIGHT_XY 
    CALL WMENUSETSTATE(IOPT,ITEMCHECKED,WINTOFF)
   END DO
   CALL WMENUSETSTATE(ID,ITEMCHECKED,WINTON)
   RIGHT_BUTTON_FUNC = ID - ID_3DRIGHT
  CASE (ID_3DCURSOR_ROTATE:ID_3DCURSOR_XY) 
   DO IOPT = ID_3DCURSOR_ROTATE,ID_3DCURSOR_XY 
    CALL WMENUSETSTATE(IOPT,ITEMCHECKED,WINTOFF)
   END DO
   CALL WMENUSETSTATE(ID,ITEMCHECKED,WINTON)
   CURSOR_KEY_FUNC = ID - ID_3DCURSOR
  CASE (ID_3DWHEEL_ROTATE,ID_3DWHEEL_PAN,ID_3DWHEEL_ZOOM,ID_3DWHEEL_X,ID_3DWHEEL_Z,ID_3DWHEEL_Y,ID_3DWHEEL_XY)
   CALL WMENUSETSTATE(ID_3DWHEEL_ROTATE,ITEMCHECKED,WINTOFF)
   CALL WMENUSETSTATE(ID_3DWHEEL_PAN,ITEMCHECKED,WINTOFF)
   CALL WMENUSETSTATE(ID_3DWHEEL_ZOOM,ITEMCHECKED,WINTOFF)
   CALL WMENUSETSTATE(ID_3DWHEEL_X,ITEMCHECKED,WINTOFF)
   CALL WMENUSETSTATE(ID_3DWHEEL_Y,ITEMCHECKED,WINTOFF)
   CALL WMENUSETSTATE(ID_3DWHEEL_Z,ITEMCHECKED,WINTOFF)
   CALL WMENUSETSTATE(ID_3DWHEEL_XY,ITEMCHECKED,WINTOFF)
   CALL WMENUSETSTATE(ID,ITEMCHECKED,WINTON)
   IF(ID.EQ.ID_3DWHEEL_ROTATE)MIDDLE_BUTTON_FUNC=1
   IF(ID.EQ.ID_3DWHEEL_PAN)   MIDDLE_BUTTON_FUNC=2
   IF(ID.EQ.ID_3DWHEEL_ZOOM)  MIDDLE_BUTTON_FUNC=3
   IF(ID.EQ.ID_3DWHEEL_X)     MIDDLE_BUTTON_FUNC=4
   IF(ID.EQ.ID_3DWHEEL_Y)     MIDDLE_BUTTON_FUNC=5
   IF(ID.EQ.ID_3DWHEEL_Z)     MIDDLE_BUTTON_FUNC=6
   IF(ID.EQ.ID_3DWHEEL_XY)    MIDDLE_BUTTON_FUNC=7
  CASE (ID_3DVIEW_SHADES_ON,ID_3DVIEW_AXES_ON,ID_3DVIEW_ORIENTATION_ON)
   CALL IMOD3D_SETTINGSALL(ID,1)
   CALL IMOD3D_DISPLAY(1)
  CASE (ID_3DVIEW_SHADES_OFF,ID_3DVIEW_AXES_OFF,ID_3DVIEW_ORIENTATION_OFF)
   CALL IMOD3D_SETTINGSALL(ID,0)
   CALL IMOD3D_DISPLAY(1)
  CASE (ID_3DVIEW_SINGLECOLOUR,ID_3DVIEW_LEGENDCOLOUR)
   CALL IMOD3D_SETTINGSALL(ID,1)
   CALL UTL_MESSAGEHANDLE(0)
   !## recompute current idf
   IF(.NOT.IMOD3D_REDRAWIDF(0))THEN; ENDIF
   IF(.NOT.IMOD3D_REDRAWIDF(1))THEN; ENDIF
   CALL UTL_MESSAGEHANDLE(1)
   CALL IMOD3D_DISPLAY(1)
  CASE (ID_3DVIEW_LEGEND)
   IOPT=WMENUGETSTATE(ID_3DVIEW_LEGEND,ITEMCHECKED)
   IOPT=ABS(IOPT-1); CALL WMENUSETSTATE(ID_3DVIEW_LEGEND,ITEMCHECKED,IOPT)
   CALL IMOD3D_SETTINGSALL(ID,IOPT)
   CALL IMOD3D_LEGEND_MAIN()
   CALL IMOD3D_DISPLAY(1) 
  CASE (ID_3DVIEW_SOLID,ID_3DVIEW_WIREFRAME,ID_3DVIEW_SOLIDWIREFRAME)
   CALL IMOD3D_SETTINGSALL(ID,1)
   CALL IMOD3D_DISPLAY(1)
  CASE (ID_ANAGLYPHS)
   IANAGLYPHS=WMENUGETSTATE(ID_ANAGLYPHS,ITEMCHECKED); IANAGLYPHS=ABS(IANAGLYPHS-1)
   CALL WMENUSETSTATE(ID_ANAGLYPHS,ITEMCHECKED,IANAGLYPHS)
   CALL IMOD3D_DISPLAY(1)
  CASE (ID_ORTHO)
   IORTHO=WMENUGETSTATE(ID_ORTHO,ITEMCHECKED); IORTHO=ABS(IORTHO-1)
   CALL WMENUSETSTATE(ID_ORTHO,ITEMCHECKED,IORTHO) 
   !## Set the perspective PROJECTION mode
   CALL GLMATRIXMODE(GL_PROJECTION)
   CALL GLLOADIDENTITY()
   IF(IORTHO)THEN
    CALL GLORTHO(BOT%X,TOP%X,BOT%Y,TOP%Y,ZNEAR,ZFAR)  !ZLEFT,ZRIGHT,ZTOP,ZBOTTOM
   ELSE
    CALL GLUPERSPECTIVE(FOVY,RAT,ZNEAR,ZFAR)    !## angle(fovy), aspect (ratio:w/h), near () and far()
   ENDIF
   CALL IMOD3D_DISPLAY(1) 
  CASE (ID_3DVIEW_RESET)
   CALL IMOD3D_SETLOOKAT_LOOKFROM()
   CALL IMOD3D_RESET_TO_INIT()
   !## initialize light
   CALL IMOD3D_LIGHT()
   !## redraw
   CALL IMOD3D_DISPLAY(1)
  CASE (ID_HELP)
   CALL IMODGETHELP('5.3.2','TMO.3DT.PlotSet') 
 END SELECT

 END SUBROUTINE IMOD3D_MENUSELECT

 !###======================================================================
 SUBROUTINE IMOD3D_RENDER()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: DID
 REAL(KIND=GLDOUBLE) :: DX,DZ,XC,YC,DEGREES
 REAL :: DEGR,DDEGR
  
 !## get current heading and tilt
 CALL IMOD3D_GET_HEADING_TILE(LENXY=DX,LENXZ=DZ)
 
 XC=LOOKAT%X; YC=LOOKAT%Y
 
 DID=WINFODIALOG(CURRENTDIALOG)
 CALL WDIALOGSELECT(ID_D3DSETTINGS_RENDER)       

 CALL WDIALOGGETREAL(IDF_REAL1,DEGR)
 CALL WDIALOGGETREAL(IDF_REAL2,DDEGR)

 DEGR=DEGR+DDEGR 
 IF(DEGR.GT.360.0)DEGR=DEGR-360.0
 CALL WDIALOGPUTREAL(IDF_REAL1,DEGR)

 DEGREES=DEGR/G2R 

! DEGREES=DDEGR !0.0_GLDOUBLE

! DO
  
!  DEGREES=DEGREES+DDEGREES
!## circle

  LOOKFROM%X=XC+DX*COS(DEGREES) !LOOKFROM%X+COS(HEADING)*DX
  LOOKFROM%Y=YC+DX*SIN(DEGREES) !LOOKFROM%Y+SIN(HEADING)*DX 
!  LOOKFROM%Z=LOOKFROM%Z+SIN(TILT)*DZ 

!## moving in direction
!  LOOKFROM%X=LOOKFROM%X+COS(HEADING)*DX
!  LOOKFROM%Y=LOOKFROM%Y+SIN(HEADING)*DX 
!  LOOKFROM%Z=LOOKFROM%Z+SIN(TILT)*DZ 

!  LOOKAT%X=LOOKAT%X+COS(HEADING)*DX
!  LOOKAT%Y=LOOKAT%Y+SIN(HEADING)*DX 
!  LOOKAT%Z=LOOKAT%Z+SIN(TILT)*DZ 

  CALL IMOD3D_RESET_TO_INIT()
  CALL IMOD3D_DISPLAY(1)

! ENDDO
 
 CALL WDIALOGSELECT(DID)

! BEGIN_LEFT%X=WX
! BEGIN_LEFT%Y=WY

 END SUBROUTINE IMOD3D_RENDER

 !###======================================================================
 SUBROUTINE IMOD3D_INIT(IACTSOLID,IACTPATHLINE)
 !###======================================================================
! USE IFOPNGL
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IACTSOLID,IACTPATHLINE
 INTEGER :: IFLAGS,I
 CHARACTER(LEN=256) :: TITLE
! LOGICAL(KIND=GLBOOLEAN),DIMENSION(1) :: LEX
 
 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_3DTOOL,2).EQ.1)THEN
  CALL IMOD3D_CLOSE()
  RETURN
 ENDIF

 CALL MAIN1INACTMODULE(ID_3DTOOL)

 CALL WMENUSETSTATE(ID_3DTOOL,2,1)

 ISOLID_3D=IACTSOLID
 IPATHLINE_3D=IACTPATHLINE
 
 !other module no closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 !## initially no background loaded
 IACTBITMAP=0
 ITRANSPARANCYBITMAP=50 !## transparant 50%
 IREADBMP=0
 IRENDER_3D=-1
 
 CALL WDIALOGLOAD(ID_D3DSETTINGS_RENDER)

 IFLAGS=SYSMENUON+MINBUTTON+MAXBUTTON+STATUSBAR 
 TITLE='3D Tool'

 I=WINFOERROR(1)

 CALL WINDOWOPENCHILD(IWIN,FLAGS=IFLAGS,MENUID=ID_MENU9,DIALOGID=ID_D3DSETTINGS,    &
                      TITLE=TITLE)
 CALL WINDOWSTATUSBARPARTS(2,(/2000,-1/),(/1,1/))
 
 CALL WINDOWSELECT(IWIN)
 CALL WMENUSETSTATE(ID_PRINT,1,0)
 IORTHO=WMENUGETSTATE(ID_ORTHO,ITEMCHECKED)

 I=WINFOERROR(1)
 
 IF(WINFOSCREEN(SCREENCOLOURS).LE.16)CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,  &
    'Your video driver is configured'//CHAR(13)//'for 16 colours or less.'//CHAR(13)//  &
    'For best results run in a higher colour mode.','Limited Colour Warning')

 RAT=REAL(IWINWIDTH)/REAL(IWINHEIGHT)
 
 CALL IGRAREA(0.0,0.0,1.0,1.0)
 CALL IGRUNITS(0.0,0.0,1.0,1.0)

 !## initialize number of ipf/idf/iff/gen
 NIPFLIST=0 !## number of ipf files
 NASSLIST=0 !## number of associated files
 NIDFLIST=0 !## number of idf files
 NIFFLIST=0 !## number of iff files
 NGENLIST=0 !## number of gen files
 NSOLLIST=0 !## number of solids
 NCLPLIST=0 !## number of clipping planes

 IDFDATA(1)=1  !## low resolution
 IDFDATA(2)=10 !## scaling blockvalue
 
 !## initialise axes index for 3d plotting
 IAXES=1   !## axes box on by default
 IBNDBOX=1
 IORIENT=0 !## orientation box off by default
 AXESINDEX=0
 LEGENDINDEX=0
 ORIENTINDEX=0
 SPHEREINDEX=0
 INDPOS%X=0.0_GLFLOAT
 INDPOS%Y=0.0_GLFLOAT
 INDPOS%Z=0.0_GLFLOAT
 
 !## neccessary for axeslabels
 INIT_ZSCALE_FACTOR= 1.0_GLDOUBLE

 LEFT_BUTTON_FUNC  =ROTATE
 RIGHT_BUTTON_FUNC =ZOOM
 CURSOR_KEY_FUNC   =SCALEXY
 MIDDLE_BUTTON_FUNC=PAN
 
 !## select openGL as the current driver for plotting purposes
 CALL WDIALOGSELECT(ID_D3DSETTINGS); I=WINFOERROR(1)
 CALL WGLSELECT(3,IDF_PICTURE2,WGLDOUBLEBUFFER); I=WINFOERROR(1)
 IWINWIDTH=WINFODIALOGFIELD(IDF_PICTURE2,FIELDWIDTH)
 IWINHEIGHT=WINFODIALOGFIELD(IDF_PICTURE2,FIELDHEIGHT)
 
 CALL GLGETINTEGERV(GL_MAX_NAME_STACK_DEPTH,MAXSTDEPTH)
 CALL GLGETINTEGERV(GL_MAX_CLIP_PLANES,MAXCPLANES)
 
! CALL glutInitDisplayMode(GL_RGB)
 
! CALL GLDISABLE(GL_DEPTH_TEST)
! call setpixelformat()
! 
! call glutinit()
 
! CALL GLENABLE(GL_STENCIL_TEST)
! CALL GLGETINTEGERV(GL_STENCIL_BITS,MBITS)   !0
! CALL GLGETINTEGERV(GL_GREEN_BITS,MBITS)
! CALL GLGETINTEGERV(GL_RED_BITS,MBITS)
! CALL GLGETINTEGERV(GL_BLUE_BITS,MBITS)
! CALL GLGETINTEGERV(GL_ALPHA_BITS,MBITS)
! CALL GLGETINTEGERV(GL_DEPTH_BITS,MBITS)
! CALL GLGETINTEGERV(GL_STENCIL_VALUE_MASK,mbits)  !-1
! CALL GLGETINTEGERV(GL_STENCIL_BACK_WRITEMASK,mbits)
! CALL GLGETBOOLEANV(GL_STENCIL_TEST,LEX)  !TRUE
! CALL GLDISABLE(GL_STENCIL_TEST)
! CALL GLGETINTEGERV(GL_DEPTH_BITS,MBITS)

 !## get display-list pointers for the clipping planes
 ALLOCATE(CLPPLOT(MAXCPLANES(1)))

 !## label color
 ACOLOR=WRGB(131,131,131)
 !## axes color
 BCOLOR=ACOLOR
 !## label colour (wit)
 LCOLOR=WRGB(255,255,255)
 !## orientation colour (wit)
 OCOLOR=WRGB(255,255,255)
 !## background colour (black)
 BGCOLOR=WRGB(255,255,255) !0,0,0)
  
 F_L1_AMBIENT =0.4_GLFLOAT
 F_L1_DIFFUSE =0.8_GLFLOAT
 F_L1_SPECULAR=0.0_GLFLOAT

 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB5)
 CALL WDIALOGRANGETRACKBAR(IDF_TRACKBAR2,0,100,10)
 CALL WDIALOGRANGETRACKBAR(IDF_TRACKBAR3,0,100,10)
 CALL WDIALOGRANGETRACKBAR(IDF_TRACKBAR4,0,100,10)
 I=INT(100.0*REAL(F_L1_AMBIENT)) ; CALL WDIALOGPUTTRACKBAR(IDF_TRACKBAR2,I)
 I=INT(100.0*REAL(F_L1_DIFFUSE)) ; CALL WDIALOGPUTTRACKBAR(IDF_TRACKBAR3,I)
 I=INT(100.0*REAL(F_L1_SPECULAR)); CALL WDIALOGPUTTRACKBAR(IDF_TRACKBAR4,I)

 !## starts to build the drawing
 IF(.NOT.IMOD3D_SETUPDISPLAY())THEN; CALL IMOD3D_CLOSE(); RETURN; ENDIF
 
 IF(NIDFLIST.EQ.0)THEN
  CALL WMENUSETSTATE(ID_3DVIEW_SOLID,ITEMENABLED,0)
  CALL WMENUSETSTATE(ID_3DVIEW_WIREFRAME,ITEMENABLED,0)
  CALL WMENUSETSTATE(ID_3DVIEW_SHADES,ITEMENABLED,0)
  CALL WMENUSETSTATE(ID_3DVIEW_SINGLECOLOUR,ITEMENABLED,0)
  CALL WMENUSETSTATE(ID_3DVIEW_LEGENDCOLOUR,ITEMENABLED,0)
 ENDIF

 IF(WINFOMOUSE(MOUSEBUTTONS).LE.2)CALL WMENUSETSTATE(ID_3DWHEEL,1,0)
 IMOUSEMOVE=0
 
 !## show settings dialog
 CALL IMOD3D_SETTINGSINIT()
 
 CALL WINDOWSELECT(0); CALL WINDOWSIZEPOS(ISTATE=WINMINIMISED)
 CALL WINDOWSELECT(IWIN); CALL WINDOWRAISE(IWIN)
 
 END SUBROUTINE IMOD3D_INIT

!I've an 25 years old Fortran app running nicely under Windows 7 and Classic mode and complied and linked under Visual Studio 2010. 
!
!Now I make a new project under Visual Studio 2015 and it compiles and links nicely under Win32 but during running the follwing happens:
!
!All GDI graphics appears as it should
!All OpenGL call run without any crash and they seem to consume computer time but nothing is rendered on the screen
!My PixelFormatDescriptor is the one I've always used under Windows 7
!
!      DATA pfd / T_PIXELFORMATDESCRIPTOR (
!     %   40,           !sizeof(T_PIXELFORMATDESCRIPTOR),
!     %   1,
!     %   IOR(PFD_DRAW_TO_WINDOW,
!     %    IOR(PFD_SWAP_COPY,
!     %     IOR(PFD_SUPPORT_GDI,
!     %      IOR(PFD_SUPPORT_OPENGL,
!     %          PFD_DOUBLEBUFFER)))),
!     %   PFD_TYPE_RGBA,
!     %   24,
!     %   0, 0, 0, 0, 0, 0,
!     %   0, 0,
!     %   0, 0, 0, 0, 0,
!     %   32,
!     %   0,
!     %   0,
!     %   PFD_MAIN_PLANE,
!     %   0,
!     %   0, 0, 0) /
!
! Anyone having the same problem or having a solution to it?

!The next section of code describes a Pixel Format. We choose a format that supports OpenGL and
!double buffering, along with RGBA (red, green, blue, alpha channel). We try to find a pixel format
!that matches the bits we decided on (16bit,24bit,32bit). Finally we set up a 16bit Z-Buffer. The
!remaining parameters are either not used or are not important (aside from the stencil buffer and the
!(slow) accumulation buffer). 
!static PIXELFORMATDESCRIPTOR pfd= // pfd Tells Windows How We Want Things To Be
!{
!sizeof(PIXELFORMATDESCRIPTOR),
!1,
!PFD_DRAW_TO_WINDOW |
!PFD_SUPPORT_OPENGL |
!PFD_DOUBLEBUFFER, // Must Support Double Buffering
!PFD_TYPE_RGBA,
!bits,
!0, 0, 0, 0, 0, 0, // Color Bits Ignored
!0,
!0,
!0,
!0, 0, 0, 0,
!16,
!0,
!0,
!PFD_MAIN_PLANE,
!0,
!0, 0, 0
!};!If there were no errors while creating the window, we'll attempt to get an OpenGL Device Context. If
!we can't get a DC an error message will pop onto the screen, and the program will quit (return
!FALSE). 

 !###======================================================================
 LOGICAL FUNCTION IMOD3D_SETUPDISPLAY()
 !###======================================================================
 IMPLICIT NONE
 INTEGER(GLINT) :: IDUM
 REAL :: Z1,Z2,X1,X2,Y1,Y2,DXY
 INTEGER :: I,J
 LOGICAL :: LEX
 REAL(KIND=GLDOUBLE) :: XD,ZRATIO

 IMOD3D_SETUPDISPLAY=.FALSE.

 !## turn all message off
 CALL UTL_MESSAGEHANDLE(0)

 !## get viewable area of current display
 TOP%X=-10.0E10; BOT%X= 10.0E10
 TOP%Y=-10.0E10; BOT%Y= 10.0E10
 J=0
 
 DO I=1,MXMPLOT
  IF(MP(I)%IACT.AND.MP(I)%ISEL)THEN
   SELECT CASE (MP(I)%IPLOT)
    CASE (1,5) !## idf/mdf (first only)
     J=1
     IF(MP(I)%IDF%XMAX.GT.MP(I)%IDF%XMIN.OR. &
        MP(I)%IDF%YMAX.GT.MP(I)%IDF%YMIN)THEN
      TOP%X=MAX(TOP%X,MP(I)%IDF%XMAX); TOP%Y=MAX(TOP%Y,MP(I)%IDF%YMAX)
      BOT%X=MIN(BOT%X,MP(I)%IDF%XMIN); BOT%Y=MIN(BOT%Y,MP(I)%IDF%YMIN)
     ENDIF
    CASE (2,3) !## ipf/iff
     J=1
     IF(MP(I)%XMAX.GE.MP(I)%XMIN.OR. &
        MP(I)%YMAX.GE.MP(I)%YMIN)THEN
      TOP%X=MAX(TOP%X,MP(I)%XMAX); TOP%Y=MAX(TOP%Y,MP(I)%YMAX)
      BOT%X=MIN(BOT%X,MP(I)%XMIN); BOT%Y=MIN(BOT%Y,MP(I)%YMIN)
     ENDIF
   END SELECT
  ENDIF
 ENDDO
 
 IF(J.EQ.0)THEN
  TOP%X=MPW%XMAX; TOP%Y=MPW%YMAX; BOT%X=MPW%XMIN; BOT%Y=MPW%YMIN
 ENDIF
 
 !## finally the viewable extent becomes
 TOP%X=MIN(TOP%X,MPW%XMAX); TOP%Y=MIN(TOP%Y,MPW%YMAX)
 BOT%X=MAX(BOT%X,MPW%XMIN); BOT%Y=MAX(BOT%Y,MPW%YMIN)

 !## adjust xy for current cookie-cutter
 J=0; DO I=1,SIZE(MP); IF(MP(I)%ISEL.AND.MP(I)%IPLOT.EQ.6)J=J+1; ENDDO
 IF(J.GT.0)THEN
  CALL POLYGON1INIT()
  X1=10.0E10; X2=-10.0E10; Y1=10.0E10; Y2=-10.0E10
  DO I=1,SIZE(MP)
   IF(MP(I)%IACT.AND.MP(I)%ISEL.AND.MP(I)%IPLOT.EQ.6)THEN
    INQUIRE(FILE=MP(I)%IDFNAME,EXIST=LEX)
    IF(LEX)THEN
     CALL POLYGON1SAVELOADSHAPE(ID_LOADSHAPE,0,MP(I)%IDFNAME)
     DO J=1,SHPNO
      X1=MIN(X1,MINVAL(SHPXC(1:SHPNCRD(J),J)))
      X2=MAX(X2,MAXVAL(SHPXC(1:SHPNCRD(J),J)))
      Y1=MIN(Y1,MINVAL(SHPYC(1:SHPNCRD(J),J)))
      Y2=MAX(Y2,MAXVAL(SHPYC(1:SHPNCRD(J),J)))
     ENDDO
    ENDIF 
   ENDIF
  ENDDO
  CALL POLYGON1CLOSE() 
  BOT%X=MIN(MAX(BOT%X,X1),TOP%X); TOP%X=MAX(MIN(TOP%X,X2),BOT%X)
  BOT%Y=MIN(MAX(BOT%Y,Y1),TOP%Y); TOP%Y=MAX(MIN(TOP%Y,Y2),BOT%Y)
 ENDIF
 
 !## if IPS is active, take that dimensions
 IF(IPATHLINE_3D.EQ.1)THEN
  TOP%X=IDF%XMAX; TOP%Y=IDF%YMAX
  BOT%X=IDF%XMIN; BOT%Y=IDF%YMIN
 ENDIF

! !## increase window a little bit ... 2.5%
! DXY=(TOP%X-BOT%X); DXY=DXY*0.025
! IF(DXY.EQ.0.0)DXY=10.0
! !## make sure coordinates are distinguishable
! DO
!  IF(.NOT.UTL_EQUALS_REAL(REAL(BOT%X)-DXY,REAL(BOT%X)))EXIT
!  DXY=DXY*10.0
! ENDDO
! BOT%X=BOT%X-DXY
! TOP%X=TOP%X+DXY
! DXY=(TOP%Y-BOT%Y); DXY=DXY*0.025
! IF(DXY.EQ.0.0)DXY=10.0
! !## make sure coordinates are distinguishable
! DO
!  IF(.NOT.UTL_EQUALS_REAL(REAL(BOT%X)-DXY,REAL(BOT%X)))EXIT
!  DXY=DXY*10.0
! ENDDO
! BOT%Y=BOT%Y-DXY
! TOP%Y=TOP%Y+DXY
!
! !## set mid-location of view
! MIDPOS%X=(TOP%X+BOT%X)/2.0_GLDOUBLE
! MIDPOS%Y=(TOP%Y+BOT%Y)/2.0_GLDOUBLE

 !## normalize normalvector after initialisation
 CALL GLENABLE(GL_NORMALIZE)

 !## Since we are using animation we will use double-buffering to eliminate
 !## flicker. To do this we should set the Draw and Read buffers to the back
 !## buffer.
 CALL GLREADBUFFER(GL_BACK)
 CALL GLDRAWBUFFER(GL_BACK)

 !## Set the initial view
 CALL GLMATRIXMODE(GL_MODELVIEW)

 CALL GLPUSHMATRIX()
 CALL GLLOADIDENTITY()

! CALL glViewPort(0_glsizei,0_glsizei,IWINWIDTH,IWINHEIGHT) 

 CALL WGLTEXTFONT(IFAMILY=FFHELVETICA,ISTYLE=0,ZEXTR=ZEXT)

 IDUM=GLRENDERMODE(GL_RENDER)

 !## neccessary for axeslabels
 XSCALE_FACTOR= INIT_XSCALE_FACTOR
 YSCALE_FACTOR= INIT_YSCALE_FACTOR
 ZSCALE_FACTOR= INIT_ZSCALE_FACTOR

 BOT%Z= 1.0E10_GLFLOAT
 TOP%Z=-1.0E10_GLFLOAT

 !## fill display with idf's
 IF(.NOT.IMOD3D_IDF_INIT())RETURN

 !## fill display with drills
 IF(.NOT.IMOD3D_IPF_INIT())RETURN

! !## activate selection dialog
! IF(NIPFLIST.GT.0)CALL IMOD3D_SELECTOBJECT_INIT()

 !## fill display with pathlines
 IF(.NOT.IMOD3D_IFF())RETURN

 IF(ISOLID_3D.NE.0)THEN
  NSOLLIST=NSPF
 ELSE
  NSOLLIST=50
 ENDIF

 ALLOCATE(SOLPLOT(NSOLLIST))
 !## get display-list pointers
 ALLOCATE(SOLLISTINDEX(NSOLLIST,2))

 SOLPLOT%ISEL=0; IF(ISOLID_3D.NE.0)SOLPLOT%ISEL=1       !## activate cross-section
 SOLPLOT%IBLEND=100   !## opaque
 SOLPLOT%IINTERFACE=0 !## filled polygon drawn
 SOLPLOT%IBITMAP=0    !## don't show bitmap, but if available, show them
 !## fill display with solids
 IF(.NOT.IMOD3D_SOL())RETURN
 
 !## increase window a little bit ... 2.5%
 DXY=(TOP%X-BOT%X); DXY=DXY*0.025
 IF(DXY.EQ.0.0)DXY=10.0
 !## make sure coordinates are distinguishable
 DO
  IF(.NOT.UTL_EQUALS_REAL(REAL(BOT%X)-DXY,REAL(BOT%X)))EXIT
  DXY=DXY*10.0
 ENDDO
 BOT%X=BOT%X-DXY
 TOP%X=TOP%X+DXY
 DXY=(TOP%Y-BOT%Y); DXY=DXY*0.025
 IF(DXY.EQ.0.0)DXY=10.0
 !## make sure coordinates are distinguishable
 DO
  IF(.NOT.UTL_EQUALS_REAL(REAL(BOT%X)-DXY,REAL(BOT%X)))EXIT
  DXY=DXY*10.0
 ENDDO
 BOT%Y=BOT%Y-DXY
 TOP%Y=TOP%Y+DXY

 !## set mid-location of view
 MIDPOS%X=(TOP%X+BOT%X)/2.0_GLDOUBLE
 MIDPOS%Y=(TOP%Y+BOT%Y)/2.0_GLDOUBLE

 !## define top%z/bot%z, nothing found from previous data, set top%z/bot%z=0.0
 IF(TOP%Z.LT.BOT%Z)THEN
  TOP%Z =1.0; BOT%Z=-1.0
 ENDIF
 !## whenever dz.eq.0
 IF(TOP%Z.EQ.BOT%Z)THEN
  TOP%Z=TOP%Z+1.0; BOT%Z=BOT%Z-1.0
 ENDIF

 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB5)
 Z1=TOP%Z; Z2=BOT%Z; CALL WDIALOGPUTREAL(IDF_REAL1,Z1); CALL WDIALOGPUTREAL(IDF_REAL2,Z2)

 !## construct clipping planes
 CALL IMOD3D_CLP_ADD( 0.0_GLDOUBLE, 0.0_GLDOUBLE,-1.0_GLDOUBLE, 0.0_GLDOUBLE, 0.0_GLDOUBLE,TOP%Z,'ClippingPlane Up')
 CALL IMOD3D_CLP_ADD( 0.0_GLDOUBLE, 0.0_GLDOUBLE, 1.0_GLDOUBLE, 0.0_GLDOUBLE, 0.0_GLDOUBLE,BOT%Z,'ClippingPlane Down')
 XD=BOT%X 
 CALL IMOD3D_CLP_ADD( 1.0_GLDOUBLE, 0.0_GLDOUBLE, 0.0_GLDOUBLE,           XD, 0.0_GLDOUBLE,0.0_GLDOUBLE,'ClippingPlane West')
 XD=TOP%X 
 CALL IMOD3D_CLP_ADD(-1.0_GLDOUBLE, 0.0_GLDOUBLE, 0.0_GLDOUBLE,           XD, 0.0_GLDOUBLE,0.0_GLDOUBLE,'ClippingPlane East')
 XD=BOT%Y 
 CALL IMOD3D_CLP_ADD( 0.0_GLDOUBLE, 1.0_GLDOUBLE, 0.0_GLDOUBLE, 0.0_GLDOUBLE,           XD,0.0_GLDOUBLE,'ClippingPlane South')
 XD=TOP%Y 
 CALL IMOD3D_CLP_ADD( 0.0_GLDOUBLE,-1.0_GLDOUBLE, 0.0_GLDOUBLE, 0.0_GLDOUBLE,           XD,0.0_GLDOUBLE,'ClippingPlane North')

 !## if ipf files are active, activate bottom and top clipping planes
 IF(NIPF.GT.0)THEN; CLPPLOT(1)%ISEL=1; CLPPLOT(2)%ISEL=1; ENDIF
 
 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB7)
 CALL WDIALOGPUTMENU(IDF_MENU1,CLPPLOT%FNAME,NCLPLIST,CLPPLOT%ISEL)

 !## construct predefined ratios
 ZRATIO=(TOP%X-BOT%X)**2.0_GLDOUBLE+(TOP%Y-BOT%Y)**2.0_GLDOUBLE; IF(ZRATIO.NE.0.0)ZRATIO=SQRT(ZRATIO)
 ZRATIO=ZRATIO/(TOP%Z-BOT%Z)

 CALL UTL_GETAXESCALES(0.0,0.0,REAL(ZRATIO),REAL(ZRATIO))

 !## maximal, this ratio becomes a square box
 J=2; DO I=1,SIZE(IDZR)
  ZR(I)=SXVALUE(J)
  CALL WMENUSETSTRING(IDZR(I),'Scale 1:'//TRIM(RTOS(REAL(ZR(I)),'F',2)))
  J=J+1  
 ENDDO
  
 !## construct miscellaneous features
 CALL IMOD3D_SETUPDISPLAY_MISC(.TRUE.)

 !## reset IPF as its appearance depends on zscale and that parameter is know by now
 CALL UTL_MESSAGEHANDLE(0); IF(.NOT.IMOD3D_IPF())THEN; ENDIF; CALL UTL_MESSAGEHANDLE(1)
! DO IIPF=1,NIPF; IF(IPFPLOT(IIPF)%ISEL.EQ.1)EXIT; ENDDO; CALL IMOD3D_IPF_SELECTION(IIPF)

 !## construct legend
 CALL IMOD3D_LEGEND_MAIN()
 
 !## enable light number zero
 CALL GLENABLE(GL_LIGHT0)

 !## enable two-sided lighting
 CALL GLLIGHTMODELI(GL_LIGHT_MODEL_TWO_SIDE,GL_TRUE)

 !## default counter-clock-wise orientation
 CALL GLFRONTFACE(GL_CCW)

 !## enables hidden-surface removeal
 CALL GLENABLE(GL_DEPTH_TEST)
 CALL GLDEPTHFUNC(GL_LESS)

 !## completely enclosed surface constructed from opaque polygons with a consistent
 !## orientation, none of the back-facing polygons are ever visible
! CALL GLENABLE(GL_CULL_FACE)
! CALL GLCULLFACE(GL_FRONT_AND_BACK)
  
 !## set the perspective PROJECTION mode
 CALL GLMATRIXMODE(GL_PROJECTION)
 CALL GLLOADIDENTITY()

 IF(IORTHO)THEN  
  CALL GLORTHO(BOT%X,TOP%X,BOT%Y,TOP%Y,ZNEAR,ZFAR)  
 ELSE
  CALL GLUPERSPECTIVE(FOVY,RAT,ZNEAR,ZFAR)    !## angle(fovy), aspect (ratio:w/h), near () and far()
 ENDIF

 !## reset viewing properties and draw image
 CALL IMOD3D_RESET_TO_INIT()
 !## initialize light
 CALL IMOD3D_LIGHT()

 !## set up viewport and display image
 CALL IMOD3D_PROCESSRESIZE(IWINWIDTH,IWINHEIGHT)

 !## turn all message on
 CALL UTL_MESSAGEHANDLE(1)

 CALL IMOD3D_ERROR('IMOD3D_SETUPDISPLAY')
 
 IMOD3D_SETUPDISPLAY=.TRUE.

 END FUNCTION IMOD3D_SETUPDISPLAY

END MODULE MOD_3D
