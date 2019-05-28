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
MODULE MOD_SMPLX_PAR

USE SMPLX_CONSTANTS_NSWC, ONLY : DP
USE IMODVAR, ONLY : DP_KIND,SP_KIND

CHARACTER(LEN=5000),DIMENSION(:),ALLOCATABLE :: IN_CON
CHARACTER(LEN=1000) :: IN_OBJ
CHARACTER(LEN=50) :: LPSTATUS
INTEGER :: NCON,NOBJ,NVAR,ICNVG,NUMLE,NUMGE
REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: XVAR,XSLK
INTEGER,ALLOCATABLE,DIMENSION(:) :: ISORT
REAL(KIND=DP_KIND) :: ZOBJ

INTEGER,ALLOCATABLE,DIMENSION(:) :: CONSTR_TYPE
REAL (DP),ALLOCATABLE,DIMENSION(:,:) :: A
REAL (DP),ALLOCATABLE,DIMENSION(:) :: B,C

END MODULE MOD_SMPLX_PAR