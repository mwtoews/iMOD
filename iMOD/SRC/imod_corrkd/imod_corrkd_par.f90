!!  Copyright (C) Stichting Deltares, 2005-2016.
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

MODULE MOD_CORRKD_PAR 
 USE MOD_IDF_PAR, ONLY : IDFOBJ
 REAL :: MINVC,MAXK
 REAL,ALLOCATABLE,DIMENSION(:) :: MINZ !,MAXZ
 INTEGER :: NLAY
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: KD_IDF,C_IDF,TOP_IDF,BOT_IDF,IBND_IDF, &
    TOPNEW_IDF,BOTNEW_IDF,KHNEW_IDF,KVNEW_IDF,IBNDNEW_IDF,KDNEW_IDF,AQFNEW_IDF,  &
    AQTNEW_IDF,CNEW_IDF,ANIF_IDF
 CHARACTER(LEN=256) :: OUTPUTMAP
END MODULE MOD_CORRKD_PAR 
