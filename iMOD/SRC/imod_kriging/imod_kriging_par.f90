!!  Copyright (C) Stichting Deltares, 2005-2018.
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
MODULE MOD_KRIGING_PAR

USE IMODVAR

REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: ZLAG,XLAG
REAL(KIND=DP_KIND),POINTER,DIMENSION(:,:) :: XY=>NULL(),XYDUMMY=>NULL()
REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: FFCT=>NULL(),FFCTDUMMY=>NULL()
INTEGER,POINTER,DIMENSION(:) :: IXY=>NULL(),IXYDUMMY=>NULL()
INTEGER,ALLOCATABLE,DIMENSION(:) :: SELID
INTEGER,ALLOCATABLE,DIMENSION(:,:) :: SELQID
REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: SELD 
REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: B,C
REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: A,L,U,AI

INTEGER :: IBLANKOUT
REAL(KIND=DP_KIND) :: RANGE,SILL,NUGGET,LAGDISTANCE,COINCIDENTDIST
INTEGER :: MAXPNT,KTYPE,PNTSEARCH,LAGINTERVAL,ILOG,COINCIDENT,IQUADRANT,IBLNTYPE

END MODULE MOD_KRIGING_PAR