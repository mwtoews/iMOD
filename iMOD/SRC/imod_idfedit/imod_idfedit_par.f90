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
MODULE MOD_IDFEDIT_PAR

USE MOD_IDF_PAR
USE IMODVAR, ONLY : DP_KIND,SP_KIND,IDIAGERROR

TYPE(IDFOBJ) :: IDFSEL
INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:) :: ITHRD
INTEGER :: IACTIONISEL  !## number of selection-action
REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: XGRIDSIZE  !## gridsize inside the polygon
INTEGER :: ICALC

END MODULE MOD_IDFEDIT_PAR
