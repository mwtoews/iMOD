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
!!
MODULE MOD_ASC2IDF_PAR
 USE MOD_IDF_PAR, ONLY : IDFOBJ
 
 INTEGER(KIND=8) :: I,N
 CHARACTER(LEN=256) :: IDFFILE,SOURCEDIR,TARGETDIR,STDEVIDF,GENFILE
 INTEGER :: IGRIDFUNC,MINP,MAXP,KTYPE,IXCOL,IYCOL,IZCOL,NOSEARCH,IEXPVARIOGRAM,LAGINTERVAL, &
       ASSF_COLUMN,ASSF_STARTDATE,ASSF_ENDDATE,ASSF_MTYPE,ASSF_DDATE,ILOG
 CHARACTER(LEN=1) :: ASSF_CDDATE
 REAL :: CS,NODATA,PERCENTILE,RANGE,SILL,NUGGET,LAGDISTANCE,SEARCHDISTANCE
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: XYZFNAMES
 TYPE(IDFOBJ) :: TRIMDEPTH_IDF
 
 REAL,POINTER,DIMENSION(:) :: XP,XP_DUMMY,YP,YP_DUMMY,ZP,ZP_DUMMY
 
END MODULE MOD_ASC2IDF_PAR

