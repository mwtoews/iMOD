!!  Copyright (C) Stichting Deltares, 2005-2017.
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
MODULE MOD_STOMP_PAR

USE MOD_IDF_PAR

TYPE(IDFOBJ),DIMENSION(:),ALLOCATABLE :: BND
TYPE(IDFOBJ),DIMENSION(:,:),ALLOCATABLE :: K,TB
CHARACTER(LEN=5),DIMENSION(6) :: TXT
DATA TXT/'MIK_X','MIK_Y','MIK_Z','FIK_X','FIK_Y','FIK_Z'/
CHARACTER(LEN=256) :: OUTPUTFILE
INTEGER :: NCOL,NROW,NLAY

END MODULE MOD_STOMP_PAR