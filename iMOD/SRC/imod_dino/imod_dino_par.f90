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

MODULE MOD_DINO_PAR

INTEGER,PARAMETER :: MAXSUB=2500
CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: CSVFNAME
CHARACTER(LEN=256) :: IPFFNAME,GENFNAME
INTEGER,ALLOCATABLE,DIMENSION(:) :: ND
TYPE DINOOBJ
 CHARACTER(LEN=50) :: DATE
 CHARACTER(LEN=20) :: ID
 CHARACTER(LEN=5) :: LITH,OPB,ZM,ZMK,AK,AS,AZ,AG,AH,SH,SHFR,PLANTFR,MICAFR,GLAUCFR,ORG, &
         CACO3,CONS,COLOR,PLANTS,SHELLS
 CHARACTER(LEN=512) :: DESC
 REAL :: MV,YEND,YTOP,YBOT,YTOPMV,YBOTMV
 INTEGER :: IX,IY
END TYPE DINOOBJ
TYPE(DINOOBJ),ALLOCATABLE,DIMENSION(:) :: DINO
CHARACTER(LEN=5),DIMENSION(:),POINTER :: LUNIQUE,TMP
INTEGER :: NUNIQUE

END MODULE MOD_DINO_PAR