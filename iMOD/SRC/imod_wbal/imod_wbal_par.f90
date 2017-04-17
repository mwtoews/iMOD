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
MODULE MOD_WBAL_PAR

 USE MOD_IDF_PAR, ONLY : IDFOBJ
 
 INTEGER :: IBATCH
 
 !## type?
 INTEGER :: WBAL_FYR,WBAL_TYR,WBAL_ISTEADY,WBAL_NLAYER,WBAL_NYEAR,WBAL_NPERIOD,WBAL_ISEL,WBAL_WBEX
 INTEGER,DIMENSION(:),POINTER :: WBAL_IYEAR
 INTEGER,POINTER,DIMENSION(:,:) :: WBAL_IPERIOD
 INTEGER,POINTER,DIMENSION(:) :: WBAL_ILAYER
 CHARACTER(LEN=256) :: WBAL_RESDIR,WBAL_OUTFNAME,WBAL_IDFNAME,WBAL_GENFNAME,WBAL_RESNAME

 TYPE WBALOBJ
  CHARACTER(LEN=8),POINTER,DIMENSION(:) :: CLAY=>NULL(),CZONE=>NULL()
  REAL,POINTER,DIMENSION(:,:) :: Q=>NULL()
  CHARACTER(LEN=52),POINTER,DIMENSION(:) :: TXT=>NULL()
  CHARACTER(LEN=16),POINTER,DIMENSION(:) :: CDATE=>NULL()
 END TYPE WBALOBJ
 TYPE(WBALOBJ),ALLOCATABLE,DIMENSION(:) :: GWBAL !## (1)=readin; (2)=selected
 TYPE WBUDGETOBJ
  CHARACTER(LEN=52) :: LABEL,FLUXTERM
  INTEGER :: ICLR,IACT,IGROUP
 END TYPE WBUDGETOBJ
 TYPE(WBUDGETOBJ),ALLOCATABLE,DIMENSION(:) :: BUDGET
           
 INTEGER,ALLOCATABLE,DIMENSION(:) :: LILAY,LIZONE,LIDATE,CLRIZONE
 CHARACTER(LEN=16),ALLOCATABLE,DIMENSION(:) :: CILAY,CIZONE,CIDATE 
 CHARACTER(LEN=16),ALLOCATABLE,DIMENSION(:) :: CMLAY,CMZONE,CMDATE 
 INTEGER :: NLAY,NZONE,NDATE,NRECORDS !## number of categories in original dataset
 INTEGER :: MLAY,MZONE,MDATE !## number of catgories in selection
 INTEGER :: NBUDGET,MBUDGET,MGROUP
 
 TYPE(IDFOBJ) :: IDFP
 
END MODULE MOD_WBAL_PAR