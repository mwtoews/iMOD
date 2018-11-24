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
MODULE MOD_TS_PAR

USE MOD_IDF_PAR, ONLY : IDFOBJ
USE IMODVAR, ONLY : DP_KIND,SP_KIND

INTEGER,PARAMETER :: MAXLEN=50

CHARACTER(LEN=256),DIMENSION(:),POINTER :: IDFNAMES
CHARACTER(LEN=256) :: IPFNAME1,IPFNAME2,LINE,FNAME,TSDIR
CHARACTER(LEN=MAXLEN) :: CTS
CHARACTER(LEN=3) :: CEXT
INTEGER :: LCOL,IEXT
INTEGER :: IU(3),TSILAY
CHARACTER(LEN=MAXLEN) :: CDUM
CHARACTER(LEN=MAXLEN),DIMENSION(3) :: CF
DATA CF/'Date','Calculated','Measure'/
CHARACTER(LEN=MAXLEN),ALLOCATABLE,DIMENSION(:) :: ATTRIB

TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF

TYPE OBSTYPE
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: OBS     ! observation value
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: RES     ! calculated residue
 REAL(KIND=DP_KIND) :: NODATA
 INTEGER :: NPER
 INTEGER(KIND=DP_KIND),POINTER,DIMENSION(:) :: IDATE
END TYPE OBSTYPE
TYPE(OBSTYPE) :: OBS,MSR

INTEGER :: IASSF !,IBATCH
INTEGER(KIND=DP_KIND) :: SDATE,EDATE
CHARACTER(LEN=MAXLEN),ALLOCATABLE,DIMENSION(:) :: STRING

END MODULE MOD_TS_PAR
