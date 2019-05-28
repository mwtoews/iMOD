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
MODULE MOD_WBAL_PAR

 USE MOD_IDF_PAR, ONLY : IDFOBJ
 USE IMODVAR, ONLY : DP_KIND,SP_KIND 

 TYPE WCTPOBJ
  CHARACTER(LEN=52) :: BDGNAME,UNIT
  INTEGER,DIMENSION(:),POINTER :: ISYS=>NULL()  !## system numbers  *_sys{i}.idf
  INTEGER :: NSYS !## number of systems in waterbalance
 END TYPE WCTPOBJ
 TYPE(WCTPOBJ),DIMENSION(:),ALLOCATABLE :: WCTP

 TYPE WBALTYPE
  REAL(KIND=DP_KIND) :: QIN,QOUT
  INTEGER :: IACT
  CHARACTER(LEN=52) :: FNAME
 END TYPE WBALTYPE
 TYPE(WBALTYPE),ALLOCATABLE,DIMENSION(:,:,:) :: WBAL
 TYPE(WBALTYPE),ALLOCATABLE,DIMENSION(:,:) :: WBEX
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IPLIST
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: AREA
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IUTXT
 INTEGER(KIND=2) :: IP
 INTEGER :: NIP,NWBAL,NPER
 CHARACTER(LEN=2000) :: HSTRING

 TYPE(IDFOBJ) :: IDF,WBALIDF,IPIDF

 CHARACTER(LEN=256) :: CSVFNAME

 INTEGER :: IBATCH
 
 INTEGER(KIND=DP_KIND) :: WBAL_FYR,WBAL_TYR
 INTEGER :: WBAL_ISTEADY,WBAL_NLAYER,WBAL_NYEAR,WBAL_NPERIOD,WBAL_ISEL,WBAL_WBEX
 INTEGER,DIMENSION(:),POINTER :: WBAL_IYEAR
 INTEGER,POINTER,DIMENSION(:,:) :: WBAL_IPERIOD
 INTEGER,POINTER,DIMENSION(:) :: WBAL_ILAYER
 CHARACTER(LEN=256) :: WBAL_RESDIR,WBAL_OUTFNAME,WBAL_IDFNAME,WBAL_GENFNAME,WBAL_RESNAME

 TYPE WBALOBJ
  CHARACTER(LEN=8),POINTER,DIMENSION(:) :: CLAY=>NULL(),CZONE=>NULL()
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:,:) :: Q=>NULL()
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: AREA=>NULL()
  CHARACTER(LEN=52),POINTER,DIMENSION(:) :: TXT=>NULL()
  CHARACTER(LEN=16),POINTER,DIMENSION(:) :: CDATE=>NULL()
 END TYPE WBALOBJ
 TYPE(WBALOBJ),ALLOCATABLE,DIMENSION(:) :: GWBAL !## (1)=readin; (2)=selected
 TYPE WBUDGETOBJ
  CHARACTER(LEN=52) :: LABEL,FLUXTERM
  INTEGER :: ICLR,IACT,IGROUP
 END TYPE WBUDGETOBJ
 TYPE(WBUDGETOBJ),ALLOCATABLE,DIMENSION(:) :: BUDGET
           
 INTEGER,ALLOCATABLE,DIMENSION(:) :: LILAY,LIZONE,LIDATE,CLRIZONE,CLILAY,CLIZONE
 CHARACTER(LEN=16),ALLOCATABLE,DIMENSION(:) :: CILAY,CIZONE,CIDATE 
 CHARACTER(LEN=16),ALLOCATABLE,DIMENSION(:) :: CMLAY,CMZONE,CMDATE 
 INTEGER :: NLAY,NZONE,NDATE,NRECORDS !## number of categories in original dataset
 INTEGER :: MLAY,MZONE,MDATE          !## number of catgories in selection
 INTEGER :: NBUDGET,MBUDGET,MGROUP
 
 TYPE(IDFOBJ) :: IDFP
 
 REAL(KIND=DP_KIND),DIMENSION(:,:), ALLOCATABLE :: QSUBREGIO 
 INTEGER,DIMENSION(:),ALLOCATABLE :: IPLG,IPOL
 
END MODULE MOD_WBAL_PAR