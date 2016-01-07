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
MODULE MOD_3D_PAR

USE WINTERACTER
USE RESOURCE
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE OPENGL
USE MODPLOT, ONLY : LEGENDOBJ

INTEGER,SAVE :: IORTHO

REAL(KIND=GLDOUBLE),SAVE :: HEADING,TILT
INTEGER,SAVE :: WALK_MODE

INTEGER,SAVE :: IANAGLYPHS !## usage of true 3D using anaglyphs

INTEGER(GLENUM),DIMENSION(6) :: CLPPLANES
DATA CLPPLANES/GL_CLIP_PLANE0,GL_CLIP_PLANE1,GL_CLIP_PLANE2,GL_CLIP_PLANE3, &
               GL_CLIP_PLANE4,GL_CLIP_PLANE5/

REAL,PARAMETER :: ZEXT = 0.0 !.2 (thickness)   !## extrusion factor
INTEGER :: IFAM1 ! = FFHelvetica!  FFCourier,FFTimes

INTEGER,DIMENSION(:),ALLOCATABLE :: IC
INTEGER :: MAXIC

INTEGER,PARAMETER :: ROTATE = 1
INTEGER,PARAMETER :: PAN    = 2
INTEGER,PARAMETER :: ZOOM   = 3
INTEGER,PARAMETER :: SCALEX = 4
INTEGER,PARAMETER :: SCALEY = 5
INTEGER,PARAMETER :: SCALEZ = 6
INTEGER,PARAMETER :: SCALEXY= 7

INTEGER :: IMOUSEMOVE=0 !## whether mousemove need to be processed in 3d window

INTEGER :: IWIN  !## window-number

TYPE IDFSETTING
 INTEGER :: IFILL,JFILL      !## use colouring (copy for selecting mode)
 INTEGER :: ICOLOR,JCOLOR    !## colornumber (copy for selecting mode)
 INTEGER :: ILEG,JLEG        !## use legend  (copy for selecting mode)
 INTEGER :: ISHADED,JSHADED  !## use shading (copy for selecting mode)
 INTEGER :: IPLOT     !## iplot in datamanager
 INTEGER :: ICOMBINE  !## (0)=none, (1-nidf) idf
 INTEGER :: IDFLEGEND !## legend by selected idf
 INTEGER :: ISEL,DISP_ISEL      !## selected
 INTEGER :: ILIST     !## list sequence
 INTEGER :: DISP_ILIST !## list sequence
 INTEGER :: IPLOTLEGEND !## plotlegend
 REAL    :: ZMAX,ZMIN !,NODATA
 INTEGER :: ICUBE     !## (0)flat;(1)cube
 INTEGER :: ICONFIG  !## configuration number
 INTEGER :: IACC    !## accuracy (=resolution) number
 CHARACTER(LEN=50) :: ALIAS,DISP_ALIAS
 CHARACTER(LEN=256) :: FNAME
 TYPE(LEGENDOBJ) :: LEG
END TYPE IDFSETTING
TYPE(IDFSETTING),ALLOCATABLE,DIMENSION(:) :: IDFPLOT
TYPE IPFSETTING
 INTEGER :: ISEL     !## selected
 INTEGER :: IPLOT    !## iplot in datamanager
 INTEGER :: ISUB     !## number of subdivision
 INTEGER :: IFANCY   !## number of subdivision
 INTEGER :: ISHADE   !## number of subdivision
 INTEGER :: ASSCOL1  !## associated column # 1
 INTEGER :: ASSCOL2  !## associated column # 2
 INTEGER :: IPLOTLEGEND !## plotlegend
 INTEGER :: IPLOTLABELS !## plot labels
 INTEGER :: ILEGDLF  !## legend dlf
 INTEGER :: ISTYLE   !## plotstyle
 REAL :: RADIUS      !## percentage
 INTEGER,DIMENSION(3) :: ISELECT
 REAL,DIMENSION(3) :: RSELECT
 CHARACTER(LEN=50) :: FNAME
END TYPE IPFSETTING
TYPE(IPFSETTING),ALLOCATABLE,DIMENSION(:) :: IPFPLOT
TYPE IFFSETTING
 INTEGER :: ISEL         !## selected
 INTEGER :: ITHICKNESS   !## thickness
 INTEGER :: IPLOT        !## iplot in datamanager
 INTEGER :: IPLOTLEGEND !## plotlegend
 CHARACTER(LEN=50) :: FNAME
END TYPE IFFSETTING
TYPE(IFFSETTING),ALLOCATABLE,DIMENSION(:) :: IFFPLOT
TYPE GENSETTING
 CHARACTER(LEN=256) :: GENFNAME
 INTEGER :: ICOLOR       !## colornumber
 INTEGER :: ITHICKNESS   !## thickness
 INTEGER :: ISEL         !## selected
 INTEGER :: ITRANSPARANCY
 LOGICAL :: L3D          !## 3d gen
 CHARACTER(LEN=50) :: FNAME
END TYPE GENSETTING
TYPE(GENSETTING),ALLOCATABLE,DIMENSION(:) :: GENPLOT
TYPE SOLSETTING
 INTEGER :: IBLEND
 INTEGER :: IBITMAP
 INTEGER :: IINTERFACE
 INTEGER :: ISEL         !## selected
END TYPE SOLSETTING
TYPE(SOLSETTING),ALLOCATABLE,DIMENSION(:) :: SOLPLOT
TYPE CLPSETTING
 INTEGER :: ICOLOR       !## colornumber
 INTEGER :: ITHICKNESS   !## thickness
 INTEGER :: ISEL         !## selected
 CHARACTER(LEN=50) :: FNAME
 REAL(KIND=GLDOUBLE) :: X,Y,Z
 REAL(KIND=GLDOUBLE),DIMENSION(4) :: EQN
END TYPE CLPSETTING
TYPE(CLPSETTING),ALLOCATABLE,DIMENSION(:) :: CLPPLOT

INTEGER(KIND=GLSIZEI) :: NIPFLIST,NIDFLIST,DISP_NIDFLIST,NIFFLIST,NGENLIST,NSOLLIST,NCLPLIST,NASSLIST
INTEGER :: IACTBITMAP  !# bitmap used
INTEGER :: ITRANSPARANCYBITMAP !# transparancy bitmap
INTEGER :: IREADBMP  !# bitmap read/allocated

INTEGER(GLSIZEI) :: IWINWIDTH,IWINHEIGHT

!## named parameter for pi
REAL(KIND=GLDOUBLE), PARAMETER :: PI = 3.141592653589793_GLDOUBLE

REAL(KIND=GLFLOAT), DIMENSION(3) :: AMBIENT 
REAL(KIND=GLFLOAT), DIMENSION(3) :: DIFFUSE 
REAL(KIND=GLFLOAT), DIMENSION(3) :: SPECULAR
REAL(KIND=GLFLOAT), DIMENSION(4) :: POS   = (/-1.0_GLFLOAT, &
                                              -1.0_GLFLOAT, &
                                              -1.0_GLFLOAT, &
                                               0.0_GLFLOAT/)!<--- DIRECTIONAL ONE!
REAL(KIND=GLFLOAT), DIMENSION(4) :: WHITE = (/1.0_GLFLOAT, &
                                              1.0_GLFLOAT, &
                                              1.0_GLFLOAT, &
                                              1.0_GLFLOAT/)

REAL(KIND=GLFLOAT), DIMENSION(4) :: L1_AMBIENT  
REAL(KIND=GLFLOAT), DIMENSION(4) :: L1_DIFFUSE  
REAL(KIND=GLFLOAT), DIMENSION(4) :: L1_SPECULAR 
REAL(KIND=GLFLOAT) :: F_L1_AMBIENT,F_L1_DIFFUSE,F_L1_SPECULAR 

!## derived types for co-ordinates
TYPE :: CART2D ! 2D CARTESIAN COORDINATES
 REAL(KIND=GLDOUBLE) :: X
 REAL(KIND=GLDOUBLE) :: Y
END TYPE CART2D
TYPE :: CART3D ! 3D CARTESIAN COORDINATES
 REAL(KIND=GLDOUBLE) :: X
 REAL(KIND=GLDOUBLE) :: Y
 REAL(KIND=GLDOUBLE) :: Z
END TYPE CART3D
TYPE :: CART3R ! 3D CARTESIAN COORDINATES (FLOAT)
 REAL(KIND=GLFLOAT) :: X
 REAL(KIND=GLFLOAT) :: Y
 REAL(KIND=GLFLOAT) :: Z
END TYPE CART3R
TYPE :: SPHERE3D ! 3D SPHERICAL COORDINATES
 REAL(KIND=GLDOUBLE) :: THETA
 REAL(KIND=GLDOUBLE) :: PHI
 REAL(KIND=GLDOUBLE) :: RHO
END TYPE SPHERE3D
!## co-ordinate variables
TYPE (CART2D), SAVE :: ANGLE
TYPE (CART3D), SAVE :: SHIFT

REAL(KIND=GLDOUBLE), SAVE :: XSCALE_FACTOR
REAL(KIND=GLDOUBLE), SAVE :: YSCALE_FACTOR
REAL(KIND=GLDOUBLE), SAVE :: ZSCALE_FACTOR

LOGICAL, SAVE :: MOVING_LEFT
LOGICAL, SAVE :: MOVING_RIGHT
LOGICAL, SAVE :: MOVING_MIDDLE

TYPE(CART2D), SAVE :: BEGIN_LEFT
TYPE(CART2D), SAVE :: BEGIN_MIDDLE
TYPE(CART2D), SAVE :: BEGIN_RIGHT

!## real position of mid of view
TYPE(CART3D), SAVE :: MIDPOS !## mid of box
TYPE(CART3D), SAVE :: TOP  !## upper right top corner
TYPE(CART3D), SAVE :: BOT  !## lower left  bottom corner

REAL(KIND=GLFLOAT) :: VIEWDX,VIEWDY
REAL(KIND=GLDOUBLE) :: ZLEFT,ZRIGHT,ZTOP,ZBOTTOM
REAL(KIND=GLDOUBLE) :: FOVY
REAL(KIND=GLDOUBLE) :: ZFAR
REAL(KIND=GLDOUBLE) :: ZNEAR
REAL(KIND=GLDOUBLE) :: RAT  !## ratio of screen
REAL(KIND=GLFLOAT),DIMENSION(3) :: XYZAXES   !## length of scaled axes around origin

!## overloaded operators for adding and subtracting 3d co-ordinates
INTERFACE OPERATOR(+)
 MODULE PROCEDURE CART3D_PLUS_CART3D
END INTERFACE
INTERFACE OPERATOR(-)
 MODULE PROCEDURE CART3D_MINUS_CART3D
END INTERFACE

!## ------- initial configuration -------
!## set the initial operation performed by each button and the cursor keys.
!## the operations are zoom, pan, rotate, scalex, scaley, and scalez
INTEGER, SAVE :: LEFT_BUTTON_FUNC   = ROTATE
INTEGER, SAVE :: RIGHT_BUTTON_FUNC  = ZOOM
INTEGER, SAVE :: MIDDLE_BUTTON_FUNC = PAN
INTEGER, SAVE :: CURSOR_KEY_FUNC    = SCALEXY

!## set the initial view as the point you are looking at, the point you are
!## looking from, and the scale factors
TYPE (CART3D) :: LOOKAT,LOOKFROM
TYPE (CART3R),SAVE :: INDPOS

!## initial scale factors
REAL(KIND=GLDOUBLE), PARAMETER :: INIT_XSCALE_FACTOR = 1.0_GLDOUBLE
REAL(KIND=GLDOUBLE), PARAMETER :: INIT_YSCALE_FACTOR = 1.0_GLDOUBLE
REAL(KIND=GLDOUBLE) :: INIT_ZSCALE_FACTOR = 1.0_GLDOUBLE
!## initial shift factors
REAL(KIND=GLDOUBLE) :: INIT_SHIFTX = 0.0_GLDOUBLE
REAL(KIND=GLDOUBLE) :: INIT_SHIFTY = 0.0_GLDOUBLE
REAL(KIND=GLDOUBLE) :: INIT_SHIFTZ = 0.0_GLDOUBLE
!## display axes
INTEGER :: IAXES   !## axes on by default
INTEGER :: IBNDBOX !## box on by default
INTEGER :: IORIENT !## orientation box off by default
INTEGER,SAVE :: ACOLOR,JACOLOR,OCOLOR,LCOLOR,BCOLOR,BGCOLOR !## colour of axes (copy) / color of orientation / color of label
INTEGER(KIND=GLUINT),SAVE :: ORIENTINDEX,LEGENDINDEX,SPHEREINDEX  
INTEGER(KIND=GLUINT),DIMENSION(0:10),SAVE :: AXESINDEX
INTEGER :: ISELECTED,JSELECTED
INTEGER,DIMENSION(3) :: IDFDATA
INTEGER :: IINDPOS !## identifier position off by default

INTEGER(KIND=GLUINT),ALLOCATABLE,DIMENSION(:) ::   GENLISTINDEX,IFFLISTINDEX,BMPLISTINDEX,IDFLISTINDEX,CLPLISTINDEX,STPLISTINDEX
INTEGER(KIND=GLUINT),ALLOCATABLE,DIMENSION(:,:) :: IPFLISTINDEX,SOLLISTINDEX,PLLISTINDEX
INTEGER,ALLOCATABLE,DIMENSION(:,:) :: IPFDLIST !## selected features of the 
INTEGER,ALLOCATABLE,DIMENSION(:) :: PLLISTCLR  !## color fraction of current time in drawing list
REAL,ALLOCATABLE,DIMENSION(:) :: PLLISTAGE     !## age of current time in drawing list

TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF_CC  !## idf (part)

INTEGER :: ISOLID_3D
INTEGER :: IPATHLINE_3D

REAL(KIND=GLFLOAT) :: ALPHA
INTEGER :: IALPHA

INTEGER,DIMENSION(5) :: IDZR
DATA IDZR/ID_ZRATIO1,ID_ZRATIO2,ID_ZRATIO3,ID_ZRATIO4,ID_ZRATIO5/
REAL(KIND=GLDOUBLE),DIMENSION(5) :: ZR

CONTAINS

 !###======================================================================
 FUNCTION CART2SPHERE(CPOINT) RESULT(SPOINT)
 !###======================================================================
 TYPE(CART3D),INTENT(IN) :: CPOINT
 TYPE(SPHERE3D) :: SPOINT
 REAL(KIND=GLDOUBLE) :: X,Y,Z

 X = CPOINT%X
 Y = CPOINT%Y
 Z = CPOINT%Z

 SPOINT%RHO = SQRT(X*X+Y*Y+Z*Z)
 IF (X==0.0_GLDOUBLE .AND. Y==0.0_GLDOUBLE) THEN
  SPOINT%THETA = 0.0_GLDOUBLE
 ELSE
  SPOINT%THETA = ATAN2(Y,X)
 END IF
 IF (SPOINT%RHO == 0.0_GLDOUBLE) THEN
  SPOINT%PHI = 0.0_GLDOUBLE
 ELSE
  SPOINT%PHI = ACOS(Z/SPOINT%RHO)
 END IF

 END FUNCTION CART2SPHERE

 !###======================================================================
 FUNCTION CART3D_PLUS_CART3D(CART1,CART2) RESULT(CART3)
 !###======================================================================
 TYPE(CART3D), INTENT(IN) :: CART1, CART2
 TYPE(CART3D) :: CART3

 CART3%X = CART1%X + CART2%X
 CART3%Y = CART1%Y + CART2%Y
 CART3%Z = CART1%Z + CART2%Z

 END FUNCTION CART3D_PLUS_CART3D

 !###======================================================================
 FUNCTION CART3D_MINUS_CART3D(CART1,CART2) RESULT(CART3)
 !###======================================================================
 TYPE(CART3D), INTENT(IN) :: CART1, CART2
 TYPE(CART3D) :: CART3

 CART3%X = CART1%X - CART2%X
 CART3%Y = CART1%Y - CART2%Y
 CART3%Z = CART1%Z - CART2%Z

 END FUNCTION CART3D_MINUS_CART3D

 !###======================================================================
 SUBROUTINE IMOD3D_ERROR(TXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*) :: TXT
 CHARACTER(LEN=100) :: STRING
 INTEGER(GLENUM) :: IERROR

 RETURN
 
 IERROR=GLGETERROR()
 IF(IERROR.EQ.GL_NO_ERROR)RETURN

 STRING=TXT
 WRITE(*,*) GLUERRORSTRING(IERROR),TRIM(TXT)
 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(STRING)//CHAR(13)//'Subroutine/Function: '//TRIM(TXT),'OpenGL Error')

 END SUBROUTINE

END MODULE MOD_3D_PAR
