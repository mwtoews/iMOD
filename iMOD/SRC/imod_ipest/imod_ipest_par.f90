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
MODULE MOD_IPEST_PAR

USE IMODVAR, ONLY : DP_KIND,SP_KIND

TYPE MEASOBJ
 REAL(KIND=DP_KIND) :: X,Y,COM,OBS,W
 INTEGER :: ILAY,ISEL
 INTEGER(KIND=DP_KIND) :: IDATE
END TYPE MEASOBJ

TYPE IPESTOBJ
 REAL(KIND=DP_KIND) :: J,T,DETERMINANT,PJ,RJ
 REAL(KIND=DP_KIND),POINTER,DIMENSION (:) :: ALPHA=>NULL()
 REAL(KIND=DP_KIND),POINTER,DIMENSION (:) :: UPPER=>NULL()
 REAL(KIND=DP_KIND),POINTER,DIMENSION (:) :: LOWER=>NULL()
 REAL(KIND=DP_KIND),POINTER,DIMENSION (:) :: SENSI=>NULL()
 REAL(KIND=DP_KIND),POINTER,DIMENSION (:) :: EIGVALUES=>NULL()
 REAL(KIND=DP_KIND),POINTER,DIMENSION (:,:) :: CORR=>NULL()
 CHARACTER(LEN=15),POINTER,DIMENSION (:) :: CPARAM=>NULL()
 TYPE(MEASOBJ),POINTER,DIMENSION(:) :: MEASURE
 INTEGER :: NMEASURE
END TYPE IPESTOBJ

TYPE PARAMOBJ
 INTEGER :: IGROUP
 CHARACTER(LEN=15) :: ACRONYM
END TYPE PARAMOBJ 

TYPE(IPESTOBJ),ALLOCATABLE,DIMENSION(:) :: IPEST
TYPE(PARAMOBJ),ALLOCATABLE,DIMENSION(:) :: PARAM
LOGICAL :: LSS

REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: HCLASSES !## (user) defined histogram classes
REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: XCLASSES !## amount of points per defined histogram class for x-array

CHARACTER(LEN=16),ALLOCATABLE,DIMENSION(:) :: CPERIOD
CHARACTER(LEN=16),ALLOCATABLE,DIMENSION(:) :: CLAY
INTEGER,ALLOCATABLE,DIMENSION(:) :: IPERIOD,ILAY,LPARAM
INTEGER(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: LPERIOD
INTEGER,ALLOCATABLE,DIMENSION(:) :: LLAY
REAL(KIND=DP_KIND) :: X1G6,Y1G6,X2G6,Y2G6

END MODULE MOD_IPEST_PAR
