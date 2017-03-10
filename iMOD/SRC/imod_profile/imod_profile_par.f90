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
MODULE MOD_PROF_PAR

USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MODPLOT, ONLY : LEGENDOBJ

INTEGER,SAVE :: ISOLID !## 0=no solid modeling 1=yes solid modeling

!## parameters
INTEGER,PARAMETER :: MXCRD    =250 !## max. coord.
INTEGER,PARAMETER :: MXSERIE  =200 !## max.pnt on line (initally, increases if necessary!

TYPE SERIETYPE
 INTEGER :: N
 REAL,POINTER,DIMENSION(:) :: X,Y,COPX,COPY
END TYPE SERIETYPE
TYPE(SERIETYPE),ALLOCATABLE,DIMENSION(:),SAVE :: SERIE

INTEGER,SAVE :: ICLRRASTER    !## color of raster
INTEGER,SAVE :: ICLRKNIKCP    !## color of knick-points
INTEGER,SAVE :: ICLRVIEWAR    !## view of sight in iff/ipf mode
INTEGER,SAVE :: LINEWIDTHPLOT !## thickness of profile-line
INTEGER,SAVE :: LINECOLORPLOT !## color of profile-line

INTEGER :: IBLOCKLINES
INTEGER :: IBLOCKFILLS
INTEGER :: ISKIPSHORTS
INTEGER,SAVE :: LINETHICKNESS=1
INTEGER,SAVE :: ILINEBLACK

INTEGER,SAVE :: MXNIDF,MXNIFF,MXSAMPLING 
LOGICAL,DIMENSION(:),ALLOCATABLE,SAVE :: LISEL
INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: KPLOT,MPLOT,KU       !JPLOT=IDF,KPLOT=IPF,MPLOT=IFF
REAL,DIMENSION(:,:),POINTER,SAVE :: XY,XYDUMMY
CHARACTER(LEN=52),DIMENSION(:),POINTER,SAVE :: XYLABEL,XYLABELDUMMY
REAL,DIMENSION(4),SAVE :: AREA

INTEGER,ALLOCATABLE,DIMENSION(:),SAVE :: IBITMAP     !## bitmap of the profile(s)
INTEGER,ALLOCATABLE,DIMENSION(:),SAVE :: IWINPROFILE !## maximum three windows
INTEGER,SAVE :: ISNAP     ! snap to locations
INTEGER,SAVE :: ICCOL     ! usage of colouring in cross-section
INTEGER,SAVE :: IFADE     ! fade out iff/ipf data
REAL,SAVE :: XSIGHT       ! sight depth
REAL,SAVE :: XCRD,YCRD    ! coordinates
REAL,SAVE :: PROFX,PROFY  ! profile coordinates
LOGICAL,SAVE :: LMOVEPROF ! move profile
INTEGER,SAVE :: IP        ! profile line drawn
INTEGER,SAVE :: IDOWN     ! mouse pressed?
INTEGER,SAVE :: IXY       ! move current point of profile
LOGICAL,SAVE :: LLINE     ! line drawn
REAL,SAVE :: XPOS         ! x position on profile
INTEGER,SAVE :: NXY       ! number of point is profile
REAL,SAVE :: XMIN,XMAX,YMIN,YMAX

TYPE PROFOBJ
 TYPE(IDFOBJ) :: IDF    ! idf-structure
 INTEGER :: PRFTYPE     ! prof.type line(0)/filled(1),coloured,points,legend?
 INTEGER :: SCOLOR      ! color number for plotting
 INTEGER :: ISCREEN     ! plotted on screen i
 INTEGER :: UNITS       ! units of idf
 CHARACTER(LEN=50) :: ALIAS  ! alias of filename
 CHARACTER(LEN=50) :: LEGNAME  ! legendname
 TYPE(LEGENDOBJ) :: LEG
END TYPE PROFOBJ
TYPE(PROFOBJ),DIMENSION(:),ALLOCATABLE,SAVE :: PROFIDF   !## idf to be processed as cross-sections
INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: PROFNIDF        !## number of idfs in mdf
INTEGER,SAVE :: NSCREEN,ISCREEN !## scherm

INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: IPIPET
REAL,SAVE :: DWIDTHCOL

TYPE BITMAPOBJ
 INTEGER :: IACT,ITYPE,NCOL,NROW,NCLR,COMPR,CDEPT,IBITMAP,IW,IH
 INTEGER :: IX1,IX2,IY1,IY2
 REAL :: GX1,GX2,GY1,GY2
 CHARACTER(LEN=256) :: FNAME
END TYPE BITMAPOBJ
TYPE(BITMAPOBJ) :: PBITMAP

END MODULE MOD_PROF_PAR

