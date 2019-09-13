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
MODULE MOD_IPEST_GLM_PAR

 USE IMODVAR, ONLY : DP_KIND
 
 REAL(KIND=DP_KIND) :: DAMPINGFACTOR=1.5D0
 REAL(KIND=DP_KIND),PARAMETER :: XPBND=0.01D0
 
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: RNG,RNL
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: IPROC
 INTEGER,ALLOCATABLE,DIMENSION(:) :: GPARAM,LPARAM,ISTATUS
 INTEGER :: IUPESTOUT,IUPESTPROGRESS,IUPESTEFFICIENCY,IUPESTSENSITIVITY,IUPESTRESIDUAL,IUPESTRUNFILE,IUJACOBIAN,IUPDEBUG
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: GF_H,GF_O
 CHARACTER(LEN=2100) :: BLINE
 CHARACTER(LEN=256) :: LINE
 CHARACTER(LEN=52) :: SLINE
 
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: JQJ
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: U
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: QPP
 REAL(KIND=DP_KIND) :: MARQUARDT

 TYPE MSROBJ
  CHARACTER(LEN=32),POINTER,DIMENSION(:) :: CLABEL 
  INTEGER,POINTER,DIMENSION(:) :: L 
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: X 
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: Y 
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: W 
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:,:) :: DHG,DHL 
  REAL(KIND=DP_KIND) :: TJ,PJ,RJ
  INTEGER :: NOBS
 END TYPE MSROBJ
 TYPE(MSROBJ) :: MSR
 
 LOGICAL :: LSENS
 
END MODULE MOD_IPEST_GLM_PAR