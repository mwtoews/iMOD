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
MODULE MOD_TS_PAR

USE MOD_IDF_PAR, ONLY : IDFOBJ

INTEGER,PARAMETER :: MAXLEN=50

CHARACTER(LEN=256) :: IPFNAME1,IPFNAME2,LINE,FNAME,TSDIR
CHARACTER(LEN=MAXLEN) :: CTS
CHARACTER(LEN=3) :: CEXT
INTEGER :: NPER,NCOL,NROW,IEXT,LCOL
INTEGER :: IU(3),TSILAY
REAL :: NODATA,NODATA_DAT

TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF

TYPE OBSTYPE
 REAL :: OBS     ! observation value
 REAL :: CALC    ! calculated value
 REAL :: RES     ! calculated residue
 INTEGER :: IDATE !gregorian data
END TYPE OBSTYPE
TYPE(OBSTYPE),ALLOCATABLE,DIMENSION(:)  :: OBS
INTEGER,DIMENSION(:),ALLOCATABLE :: IPER
REAL,DIMENSION(3) :: MEAN

INTEGER :: JD1,JD2,IASSF,IBATCH
CHARACTER(LEN=MAXLEN),ALLOCATABLE,DIMENSION(:) :: STRING

END MODULE MOD_TS_PAR
