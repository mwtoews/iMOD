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
MODULE MOD_ASC2IDF_PAR
 USE MOD_IDF_PAR, ONLY : IDFOBJ
 USE IMODVAR, ONLY : DP_KIND,SP_KIND
 
 INTEGER(KIND=8) :: I,N
 CHARACTER(LEN=256) :: IDFFILE,SOURCEDIR,TARGETDIR,STDEVIDF,GENFILE
 CHARACTER(LEN=256),DIMENSION(:),ALLOCATABLE :: BLNFILE
 INTEGER :: IGRIDFUNC,MINP,KTYPE,IXCOL,IYCOL,IZCOL,PNTSEARCH,LAGINTERVAL,ASSF_INDICATOR,ASSF_NTHRESHOLD,  &
       ASSF_COLUMN,ASSF_STARTDATE,ASSF_ENDDATE,ASSF_DDATE,ILOG,COINCIDENT,IQUADRANT,ASSF_IDEPTH,IINT_IDF, &
       IBLNTYPE
 CHARACTER(LEN=1) :: ASSF_CDDATE
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: ASSF_KH_THRESHOLD,FCTBLNFILE,ASSF_KV_THRESHOLD
 CHARACTER(LEN=52),ALLOCATABLE,DIMENSION(:) :: ASSF_THRESHOLD
 REAL(KIND=DP_KIND) :: CS,NODATA,PERCENTILE,RANGE,SILL,NUGGET,LAGDISTANCE,COINCIDENTDIST,ASSF_TOP,ASSF_BOT,ASSF_DZ,ASSF_ZPLUS
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: XYZFNAMES
 TYPE(IDFOBJ),DIMENSION(2) :: TRIMDEPTH_IDF
 TYPE(IDFOBJ),DIMENSION(3) :: ELLIPS_IDF
 TYPE(IDFOBJ),DIMENSION(:),ALLOCATABLE :: INT_IDF
 
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: XP,XP_DUMMY,YP,YP_DUMMY,ZP,ZP_DUMMY,WP,WP_DUMMY,FP,FP_DUMMY
 
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IOS
 CHARACTER(LEN=20),ALLOCATABLE,DIMENSION(:) :: TXT
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 INTEGER :: IBLANKOUT

END MODULE MOD_ASC2IDF_PAR

