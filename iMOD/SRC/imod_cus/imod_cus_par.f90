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
MODULE MOD_CUS_PAR

USE IMODVAR, ONLY : DP_KIND,SP_KIND
USE MOD_IDF_PAR, ONLY : IDFOBJ

INTEGER,PARAMETER :: NPERC=16
REAL(KIND=DP_KIND),SAVE :: ZCRIT,PERCENTAGE,CRIT_THICKNESS,MIN_THICKNESS
REAL(KIND=DP_KIND),DIMENSION(NPERC),SAVE :: XMED,PERC
DATA PERC/0.0D0,10.0D0,20.0D0,30.0D0,40.0D0,50.0D0,55.0,60.0D0,65.0,70.0D0,75.0,80.0D0,85.0,90.0D0,95.0,100.0D0/
INTEGER,SAVE :: NFORM,ICPOINTERS,NLAY,NCLIP,IEXPZONE,MINEXTENT

REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: DZTOP,DZBOT
INTEGER,ALLOCATABLE,DIMENSION(:,:) :: ICLIP
INTEGER(KIND=2),ALLOCATABLE,DIMENSION(:,:) :: IZTOP,IZBOT,ILTOP,ILBOT

CHARACTER(LEN=256),SAVE :: REGISTOP,REGISBOT,OUTPUTFOLDER,FDISTANCES,TOPSYSTEM,BOTSYSTEM
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: TOPIDF,BOTIDF,MDLIDF,ZIDF,MDLTOP,MDLBOT
TYPE(IDFOBJ),SAVE :: CLIPIDF
CHARACTER(LEN=256),DIMENSION(:),POINTER :: REGISFILES
TYPE ZIDFOBJ
 INTEGER :: NZ                      !## number of zones
 INTEGER,POINTER,DIMENSION(:) :: NP !## number of points in zone
END TYPE ZIDFOBJ
TYPE(ZIDFOBJ),ALLOCATABLE,DIMENSION(:) :: ZINFO
TYPE DOBJ
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: D     !## distance
 INTEGER,POINTER,DIMENSION(:) :: IZ               !## zone
 INTEGER :: NZ
END TYPE DOBJ
TYPE(DOBJ),ALLOCATABLE,DIMENSION(:) :: DZ
TYPE SMPLX_OBJ
 INTEGER :: IF1,IF2,IZ1,IZ2,IAREA,IVAR1,IVAR2
 REAL(KIND=DP_KIND),DIMENSION(NPERC) :: PERC
END TYPE SMPLX_OBJ
TYPE(SMPLX_OBJ),POINTER,DIMENSION(:) :: SMPLX,SMPLX_DUMMY
INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:) :: ILP

END MODULE MOD_CUS_PAR
