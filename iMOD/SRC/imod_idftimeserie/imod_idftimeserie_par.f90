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
MODULE MOD_IDFTIMESERIE_PAR

USE MOD_PROFILE_UTL, ONLY : AXESOBJ
USE MOD_IDF_PAR, ONLY : IDFOBJ

TYPE(AXESOBJ) :: AXES
INTEGER :: ICLRRASTER
INTEGER,PARAMETER :: IDELIM=44

TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:,:) :: IDF
INTEGER :: NIDF,IBITMAP
TYPE LINETYPE
 INTEGER :: ICLR
 INTEGER :: IWIDTH
 INTEGER :: ITYPE
 INTEGER :: ISTYLE
 INTEGER :: IAXES
 INTEGER :: UNITS
END TYPE LINETYPE
TYPE(LINETYPE),ALLOCATABLE,DIMENSION(:) :: LTYPE

TYPE IPFPLUSTYPE
 INTEGER :: ID
 REAL :: X,Y
END TYPE IPFPLUSTYPE
TYPE(IPFPLUSTYPE),POINTER,DIMENSION(:) :: IPFPLUS,IPFDUM

CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:,:) :: LISTFILES
!INTEGER,INTENT(OUT),DIMENSION(:) :: NFILES

INTEGER,ALLOCATABLE,DIMENSION(:) :: NFILES,MFILES
!CHARACTER(LEN=256),DIMENSION(:),POINTER :: IDFNAMES !,LEGENDNAME
CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: LEGENDNAME
TYPE TSTYPE
 REAL,POINTER,DIMENSION(:) :: IDATE
 REAL,POINTER,DIMENSION(:) :: VALUE
 REAL :: NODATA
 INTEGER,POINTER,DIMENSION(:) :: IDY,IMH,IYR
 INTEGER,POINTER,DIMENSION(:) :: IHR,IMT,ISC 
END TYPE TSTYPE
TYPE(TSTYPE),ALLOCATABLE,DIMENSION(:) :: TS,TSIPF,TSDIFF
INTEGER :: IRESIDUAL,NPLUS,IPFIU,MINDATE,MAXDATE

END MODULE MOD_IDFTIMESERIE_PAR
