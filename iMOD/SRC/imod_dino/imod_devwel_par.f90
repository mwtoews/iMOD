!!  Copyright (C) Stichting Deltares, 2005-2019.
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
MODULE MOD_DEVWEL_PAR

USE IMODVAR, ONLY : DP_KIND

TYPE HEADEROBJ
 CHARACTER(LEN=52) :: CID,LIC,CTYPE
 REAL(KIND=DP_KIND) :: ELEV,TDEPTH,X,Y
END TYPE HEADEROBJ
TYPE(HEADEROBJ),ALLOCATABLE,DIMENSION(:) :: HEADER

TYPE DIROBJ
 CHARACTER(LEN=52) :: CID
 REAL(KIND=DP_KIND) :: MD,INC,AZIM
END TYPE DIROBJ
TYPE(DIROBJ),ALLOCATABLE,DIMENSION(:) :: DRC

TYPE CINT1OBJ
 CHARACTER(LEN=52) :: CID,CDATE,CTYPE
 REAL(KIND=DP_KIND) :: UD,LD
END TYPE CINT1OBJ
TYPE(CINT1OBJ),ALLOCATABLE,DIMENSION(:) :: CINT1

TYPE CINT2OBJ
 CHARACTER(LEN=52) :: CID,CDATE,CTYPE
 REAL(KIND=DP_KIND) :: UD,LD
END TYPE CINT2OBJ
TYPE(CINT2OBJ),ALLOCATABLE,DIMENSION(:) :: CINT2
REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: ZDD,ZD1,ZD2,ZD3
REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: IORDER,JORDER,KORDER

END MODULE MOD_DEVWEL_PAR