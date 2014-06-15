!!  Copyright (C) Stichting Deltares, 2005-2014.
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
MODULE MOD_IPF_PAR

USE RESOURCE, ONLY : ID_DIPFINFO_TAB1TAB1,ID_DIPFINFO_TAB1TAB2,ID_DIPFINFO_TAB1TAB3, &
                     ID_DIPFINFO_TAB1TAB4,ID_DIPFINFO_TAB1TAB5
INTEGER,PARAMETER :: MAXLEN=50

CHARACTER(LEN=MAXLEN),ALLOCATABLE,DIMENSION(:,:) :: CIPF

INTEGER :: NIPF     !## number of ips"s active for plotting/analysing
TYPE IPFOBJ
 INTEGER :: IU         !## unit number assigned to ipf
 INTEGER :: NROW,NCOL  !## number of rows and columns within ipf
 INTEGER :: XCOL,YCOL,ZCOL,Z2COL,QCOL !## column number of x,y,z coordinates
 INTEGER :: ACOL       !## column number of associated files
 INTEGER,DIMENSION(10) :: IAXES  !## which axes to be used for each column in the associated files
 INTEGER :: SYMBOL     !## plot symbol
 INTEGER :: THICKNESS  !## plot symbol grootte
 CHARACTER(LEN=10) :: FEXT !## extent of associated files
 CHARACTER(LEN=256) :: FNAME !## fname of ipf
 CHARACTER(LEN=50) :: ALIAS !## fname of ipf, without root
 INTEGER(KIND=1),POINTER,DIMENSION(:) :: IP !## selected rows within ipf
 INTEGER :: ASSCOL1 !## columns used to plot the associated files
 INTEGER :: ASSCOL2 !## columns used to plot the associated files
 !## ip=1 simple figure
 !## ip=2 advanced figure
 !## ip=3 selected
 !## ip=4
 !## ip=5
 REAL,POINTER,DIMENSION(:,:) :: XYZ  !## xyz coordinates of ipf
 CHARACTER(LEN=MAXLEN),POINTER,DIMENSION(:,:) :: INFO !## data witin ipf
 CHARACTER(LEN=MAXLEN),POINTER,DIMENSION(:,:) :: DUMMY_INFO !## data witin ipf
 CHARACTER(LEN=MAXLEN),POINTER,DIMENSION(:) :: ATTRIB !## attribute names
 CHARACTER(LEN=MAXLEN),POINTER,DIMENSION(:) :: DUMMY_ATTRIB !## atrribute names
 INTEGER(KIND=1),POINTER,DIMENSION(:) :: IPOS   !## drawn point
 REAL,POINTER,DIMENSION(:,:) :: XYPOS           !## drawn coordinate (could be different in profiles)
 INTEGER :: ITYPE !## 0=standard IPF, 1=CSV
END TYPE IPFOBJ
TYPE(IPFOBJ),ALLOCATABLE,DIMENSION(:) :: IPF

INTEGER,PARAMETER :: MAXLITHO=250 !## size of grid on 3d-settings and within profile-setting
INTEGER :: NLITHO 
TYPE BHOBJ
 CHARACTER(LEN=20) :: LITHO
 CHARACTER(LEN=50) :: LITHOTXT
 INTEGER :: LITHOCLR
 REAL :: LITHOWIDTH
END TYPE BHOBJ
TYPE(BHOBJ),DIMENSION(MAXLITHO) :: BH

TYPE TYPE_IPF
 CHARACTER(LEN=30),POINTER,DIMENSION(:) :: ATTRIB
 REAL,POINTER,DIMENSION(:) :: NODATA

 REAL(KIND=8),POINTER,DIMENSION(:) :: IDATE               !## time-data in timeseries
! REAL,POINTER,DIMENSION(:) :: ITIME               !## time-data in timeseries
 REAL,POINTER,DIMENSION(:,:) :: MEASURE           !## data in timeseries/sonderingen

 REAL,POINTER,DIMENSION(:) :: Z                   !## z-coordinate in drills/sonderingen
 CHARACTER(LEN=256),POINTER,DIMENSION(:,:) :: L    !## data from drills
! CHARACTER(LEN=20),POINTER,DIMENSION(:,:) :: L    !## data from drills

 INTEGER :: ASSCOL1 !## columns used to plot the associated files
 INTEGER :: ASSCOL2 !## columns used to plot the associated files

 INTEGER :: IROW !## row number in ipf that trickers this drill/measure e.g.
 INTEGER :: NRASS,NCASS,ITOPIC
 REAL :: XMIN,XMAX  !## for top  and bottom axes
 REAL :: YMIN,YMAX  !## for left and right  axes
 REAL :: Y2MIN,Y2MAX  !## second yaxes
 INTEGER,DIMENSION(10) :: IAXES !## 1=left,bottom ;2=right
 CHARACTER(LEN=50) :: FNAME
END TYPE TYPE_IPF
TYPE(TYPE_IPF),DIMENSION(:),ALLOCATABLE :: ASSF

INTEGER,PARAMETER :: MXTAB=5
INTEGER,DIMENSION(MXTAB) :: IDTAB
DATA IDTAB/ID_DIPFINFO_TAB1TAB1,ID_DIPFINFO_TAB1TAB2,ID_DIPFINFO_TAB1TAB3, &
           ID_DIPFINFO_TAB1TAB4,ID_DIPFINFO_TAB1TAB5/

INTEGER,ALLOCATABLE,DIMENSION(:),SAVE :: GRAPHLINESCOLOUR,GRAPHLINESONOFF,GRAPHLINESTHICKNESS
INTEGER,PARAMETER :: GRAPHMAXLINES=10
INTEGER,SAVE :: GRAPHLINESXAXES,GRAPHLINESYAXES
REAL,SAVE :: GRAPHLINESXMIN,GRAPHLINESXMAX,GRAPHLINESYMIN,GRAPHLINESYMAX

END MODULE MOD_IPF_PAR

