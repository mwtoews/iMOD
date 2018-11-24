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
MODULE MOD_MAP2IDF_PAR

USE ISO_C_BINDING
USE MOD_IDF_PAR, ONLY : IDFOBJ

!## NEEDED PARAMETERS FOR MOD_MAP2IDF MODULE
 INTEGER, PARAMETER :: CR_UINT1 = 1 ! 0
 INTEGER, PARAMETER :: CR_UINT2 = 2 ! 17
 INTEGER, PARAMETER :: CR_UINT4 = 3 ! 34
 INTEGER, PARAMETER :: NCRUINT = CR_UINT4
 INTEGER, PARAMETER :: CR_INT1  = 4 ! 4
 INTEGER, PARAMETER :: CR_INT2  = 5 ! 21
 INTEGER, PARAMETER :: CR_INT4  = 6 ! 38
 INTEGER, PARAMETER :: NCRINT = CR_INT4-NCRUINT
 INTEGER, PARAMETER :: CR_REAL4 = 7 ! 90
 INTEGER, PARAMETER :: CR_REAL8 = 8 ! 219
 INTEGER, PARAMETER :: CR_UNDEF = 9 ! 100
 INTEGER, PARAMETER :: NCR = CR_UNDEF

 INTEGER, DIMENSION(NCR) :: CRVAL
 DATA CRVAL/0,17,34,4,21,38,90,219,100/

 CHARACTER(LEN=5), DIMENSION(NCR) :: CRSTR
 DATA CRSTR/'UINT1','UINT2','UINT4','INT1 ','INT2 ','INT4 ','REAL4','REAL8','UNDEF'/

 INTEGER(C_INT), DIMENSION(NCRUINT) :: CRUINTMV
 DATA CRUINTMV/255,65535,4294967294/

 INTEGER(KIND=4), DIMENSION(NCRINT) :: CRINTMV
 DATA CRINTMV/-2147483648,-256,-32768/

 INTEGER, PARAMETER :: PT_XY     = 1
 INTEGER, PARAMETER :: PT_UTM    = 2
 INTEGER, PARAMETER :: PT_LATLON = 3
 INTEGER, PARAMETER :: PT_CART   = 4
 INTEGER, PARAMETER :: PT_RDM    = 5
 INTEGER, PARAMETER :: NPT       = PT_RDM

 INTEGER, DIMENSION(NPT) :: PTVAL
 DATA PTVAL/0,1,2,3,4/

 CHARACTER(LEN=4), DIMENSION(NPT) :: PTSTR
 DATA PTSTR/'XY    ','UTM   ','LATLON','CART  ','RDM   '/

 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF

END MODULE MOD_MAP2IDF_PAR

