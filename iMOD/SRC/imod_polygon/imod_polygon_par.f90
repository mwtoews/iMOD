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
MODULE MOD_POLYGON_PAR

USE MOD_IDF_PAR, ONLY : IDFOBJ
USE IMODVAR, ONLY : DP_KIND,SP_KIND
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: SELIDF

TYPE POLSTROBJ
 CHARACTER(LEN=1),ALLOCATABLE,DIMENSION(:) :: STRING
END TYPE POLSTROBJ

TYPE POLINDOBJ
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: X,Y,CX,CY
 REAL(KIND=DP_KIND) :: XMIN,YMIN,XMAX,YMAX
 INTEGER :: N
 INTEGER :: ITYPE,IACT,ICOLOR,IWIDTH
 !## used to display in menu-field of first attribute
 CHARACTER(LEN=52) :: PNAME
 TYPE(POLSTROBJ),POINTER,DIMENSION(:) :: LBL,LBL_BU
END TYPE POLINDOBJ

TYPE POLOBJ
 REAL(KIND=DP_KIND) :: XMIN,YMIN,XMAX,YMAX
 INTEGER,POINTER,DIMENSION(:) :: LWIDTH,LWIDTH_BU
 CHARACTER(LEN=11),POINTER,DIMENSION(:) :: COLNAMES,COLNAMES_BU
 TYPE(POLINDOBJ),ALLOCATABLE,DIMENSION(:) :: POL
 !## number of active polygons
 INTEGER :: NPOL
 !## flag/checkbox if ILBL must be used for gridding (0/1)
 INTEGER :: NLBL
 !## selected attribute for gridding (number of label)
 INTEGER :: ILBL
END TYPE POLOBJ
TYPE(POLOBJ) :: SHP

!## current selected polygon
INTEGER :: SHPI

INTEGER :: MAXSHAPES 

INTEGER,DIMENSION(6) :: IACTSHAPES

REAL(KIND=DP_KIND),POINTER,DIMENSION(:,:) :: CSHPXC,CSHPYC
INTEGER,POINTER,DIMENSION(:) :: CSHPNCRD

CHARACTER(LEN=256) :: SHPFILE

INTEGER :: ICRD      !## icrd=node of the polygon
INTEGER :: CRDITYPE  !## crditype=1(movenode);2(add point);3(3 move polygon)
INTEGER :: ISHPEDIT  !## editable of polygons (0=no;1=yes)
INTEGER :: ICLRPOLG  !## colour to be used to plot the polygon

LOGICAL :: LPLOTYSEL

END MODULE MOD_POLYGON_PAR

