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
MODULE MOD_MATH_SCALE_PAR

USE MOD_IDF_PAR, ONLY : IDFOBJ

REAL,SAVE :: SCLSIZE    !## cellsize
INTEGER,SAVE :: SCLTYPE_UP,SCLTYPE_DOWN
!iscale=+1 (upscaling)
! WRITE(*,'(1X,A)') ' 1 = special (iboundary)'
! WRITE(*,'(1X,A)') ' 2 = arithmetic mean (shead/vcont/s)'
! WRITE(*,'(1X,A)') ' 3 = geometric (kd)'
! WRITE(*,'(1X,A)') ' 4 = inverse (c)'
! WRITE(*,'(1X,A)') ' 5 = sum (conductances)'
! WRITE(*,'(1X,A)') ' 6 = sum inverse'
! WRITE(*,'(1X,A)') ' 7 = most frequent occurence (landuse)'
! WRITE(*,'(1X,A)') ' 8 = percentile'
! WRITE(*,'(1X,A)') ' 9 = darcian method (simulation)'
! WRITE(*,'(1X,A)') '10 = homogenization (simulation)'
! WRITE(*,'(1X,A)') '11 = global-local method' 
! WRITE(*,'(1X,A)') '12 = 3d' 
!iscale=-1 (downscaling)
! WRITE(*,'(1X,A)') '-1 = arithmetic average'
! WRITE(*,'(1X,A)') '-2 = block values'
REAL,SAVE :: HOR_FCT  !## multiplication idfvalue*hor_fct to compute kd
REAL,SAVE :: VER_FCT  !## multiplication idfvalue*hor_fct to compute kd
REAL,SAVE :: DHX,DHY,DHZ  !## gradients in x,y,z direction
REAL,SAVE :: MAXK !## maximum k value
INTEGER,SAVE :: IBUFFER   !## usage of buffer (for darcian simulations), delta-cell
INTEGER,DIMENSION(2) :: ITRIM !## usage of trimfiles
INTEGER,SAVE :: IIEXT  !## usage of a window
INTEGER,SAVE :: IINT  !##  size of scaling block
INTEGER,SAVE :: ISURFWATER !## assign nodata value to surface water ... total nodata column only.
REAL,SAVE :: SFCT     !## new cellsize
REAL,SAVE :: QRATE    !## strength of extraction
REAL,SAVE :: AQFR_KD  !## transmissivity of aquifer
CHARACTER(LEN=256),DIMENSION(:),ALLOCATABLE :: IDFNAMES,OUTNAMES
TYPE(IDFOBJ),DIMENSION(:),ALLOCATABLE :: TRIMIDF
REAL :: KMIN
INTEGER :: ILGROUP

END MODULE MOD_MATH_SCALE_PAR

