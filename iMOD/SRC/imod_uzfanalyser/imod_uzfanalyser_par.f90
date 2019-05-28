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
MODULE MOD_UZFANALYSER_PAR

USE IMODVAR, ONLY : DP_KIND,SP_KIND

TYPE MCTYPE
 INTEGER :: ILAY
 REAL(KIND=DP_KIND) :: TIME,GWH,UZT
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: Z,M
END TYPE MCTYPE
TYPE(MCTYPE),ALLOCATABLE,DIMENSION(:) :: MC
CHARACTER(LEN=52),ALLOCATABLE,DIMENSION(:) :: CPER
REAL(KIND=DP_KIND) :: XMIN,YMIN,XMAX,YMAX

END MODULE MOD_UZFANALYSER_PAR