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

MODULE MOD_LUDCMP

CONTAINS

 !###====================================================================
 SUBROUTINE LUDCMP(AA,INDX,N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN)   :: N
 INTEGER,DIMENSION(N),INTENT(OUT) :: INDX
 REAL,DIMENSION(N,N),INTENT(INOUT)  :: AA
 
 END SUBROUTINE LUDCMP

 !###====================================================================
 SUBROUTINE LUBKSB(AA,INDX,BB,N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN)   :: N
 REAL,DIMENSION(N,N),INTENT(INOUT)  :: AA
 REAL,DIMENSION(N),INTENT(INOUT) :: BB
 INTEGER,DIMENSION(N),INTENT(IN) :: INDX

 END SUBROUTINE LUBKSB

END MODULE MOD_LUDCMP

