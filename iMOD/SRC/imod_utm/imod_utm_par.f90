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
MODULE MOD_UTM_PAR

USE IMODVAR, ONLY : DP_KIND,SP_KIND
INTEGER,PARAMETER :: NUTM=2
CHARACTER(LEN=32) :: DATUM

TYPE UTMOBJ
 CHARACTER(LEN=32) :: DATUM

 REAL(KIND=DP_KIND) :: A     !## (a) equatorial radius (km)
 REAL(KIND=DP_KIND) :: B     !## (b) polar radius (km)
 REAL(KIND=DP_KIND) :: FLAT  !## flattening (a-b)/a
 REAL(KIND=DP_KIND) :: K0    !## scale along long0
 REAL(KIND=DP_KIND) :: E0  
 REAL(KIND=DP_KIND) :: N0  
 REAL(KIND=DP_KIND) :: E     !## eccentricity of earth's elliptical cross-section
 REAL(KIND=DP_KIND) :: EQ    !## 
 REAL(KIND=DP_KIND) :: N     !## 
 REAL(KIND=DP_KIND) :: LBAND !## 
 
 REAL(KIND=DP_KIND) :: AA    
 REAL(KIND=DP_KIND),DIMENSION(3) :: ALPHA
 REAL(KIND=DP_KIND),DIMENSION(3) :: BETA
 REAL(KIND=DP_KIND),DIMENSION(3) :: DELTA

END TYPE UTMOBJ
TYPE(UTMOBJ),DIMENSION(NUTM) :: UTM

END MODULE MOD_UTM_PAR