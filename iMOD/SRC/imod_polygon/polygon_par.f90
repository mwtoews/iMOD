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
MODULE MOD_POLYGON_PAR

USE MOD_IDF_PAR, ONLY : IDFOBJ
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: SELIDF

INTEGER                         :: MAXSHAPES   !MAX. SHAPES IN PROJ.LIJST
INTEGER                         :: MAXSHPCRD   !MAX. COORD. IN SHAPE

INTEGER,DIMENSION(6)            :: IACTSHAPES
REAL,POINTER,DIMENSION(:,:) :: SHPXC,SHPYC,CSHPXC,CSHPYC
INTEGER,ALLOCATABLE,DIMENSION(:) :: SHPNCRD,SHPTYPE,SHPIACT,SHPCOLOR,SHPWIDTH,SHPID
CHARACTER(LEN=50),ALLOCATABLE,DIMENSION(:) :: SHPNAME
INTEGER :: SHPNO,SHPI

CHARACTER(LEN=256) :: SHPFILE

INTEGER :: ICRD      !## icrd=node of the polygon
INTEGER :: CRDITYPE  !## crditype=1(movenode);2(add point);3(3 move polygon)
INTEGER :: ISHPEDIT  !## editable of polygons (0=no;1=yes)
INTEGER :: ICLRPOLG  !## colour to be used to plot the polygon

!TYPE DBFTYPE
! CHARACTER(LEN=11) :: COLNAME
! CHARACTER(LEN=1) :: COLTYPE
! INTEGER :: COLWIDTH
! INTEGER :: COLDEC
! INTEGER :: COLOFF
!END TYPE DBFTYPE
!INTEGER(KIND=SELECTED_INT_KIND(3)) :: LHEAD,LENREC  ! =INTEGER*2
!TYPE(DBFTYPE),ALLOCATABLE,DIMENSION(:) :: DBF
!INTEGER :: MAXCOL,INDFLD

LOGICAL                         :: LPLOTYSEL

END MODULE MOD_POLYGON_PAR

