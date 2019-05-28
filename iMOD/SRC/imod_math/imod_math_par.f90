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
MODULE MOD_MATH_PAR

USE IMODVAR, ONLY : DP_KIND,SP_KIND
CHARACTER(LEN=80),SAVE :: FUNC  !## function
INTEGER,SAVE :: IIEXT           !## usage of window
INTEGER,SAVE :: IGEN            !## usage of polygon
INTEGER,SAVE :: INODATA         !## usage of nodata value
REAL(KIND=DP_KIND),SAVE :: NODATA_VALUE       !## nodata value to be used in computation
REAL(KIND=DP_KIND),SAVE :: TRIM_VALUE         !## trim value for results
INTEGER,SAVE :: IEQUI           !## make equidist. idf as result
CHARACTER(LEN=256),DIMENSION(:,:),ALLOCATABLE,SAVE :: IDFNAMES
CHARACTER(LEN=256),SAVE :: GENNAME

END MODULE MOD_MATH_PAR
