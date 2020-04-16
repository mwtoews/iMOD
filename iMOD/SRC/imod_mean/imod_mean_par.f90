!!  Copyright (C) Stichting Deltares, 2005-2020.
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
MODULE MOD_MEAN_PAR

 USE IMODVAR, ONLY : DP_KIND,SP_KIND
 USE MOD_IDF_PAR
 
 INTEGER :: IBATCH
 INTEGER :: MEAN_FYR,MEAN_TYR,MEAN_NYEAR,MEAN_NPERIOD,MEAN_ISEL,MEAN_NLAYER
 REAL(KIND=DP_KIND) :: PERCVALUE
 INTEGER,DIMENSION(:,:),POINTER :: MEAN_IPERIOD
 INTEGER,DIMENSION(:),POINTER :: MEAN_IYEAR,MEAN_ILAYER
 CHARACTER(LEN=256) :: MEAN_RESDIR,MEAN_IDFNAME,MEAN_GENFNAME
 CHARACTER(LEN=52) :: CFUNC
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: MEAN_FMEAN,MEAN_FTOTAL,MEAN_IDFNAMES

 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 TYPE(IDFOBJ) :: IDFCP !## pointer idf to be computed
 TYPE(IDFOBJ) :: IDFRP !## pointer idf to be read
 INTEGER,ALLOCATABLE,DIMENSION(:) :: MEAN_ILIST,MEAN_JLIST
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: MEAN_LISTNAME

END MODULE MOD_MEAN_PAR