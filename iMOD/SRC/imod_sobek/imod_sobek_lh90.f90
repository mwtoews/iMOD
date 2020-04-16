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
MODULE MOD_SOBEK_PAR
 CHARACTER(LEN=256) :: SOBEKDIR,CALCPNTHISNAME,STRUCHISNAME,ISGNAME
 INTEGER :: IBATCH
END MODULE MOD_SOBEK_PAR

MODULE MOD_SOBEK

INTEGER,PARAMETER :: ISOBEK=0

 CONTAINS
 
 !###======================================================================
 SUBROUTINE SOBEK1MAIN()
 !###======================================================================
 IMPLICIT NONE
 
 END SUBROUTINE SOBEK1MAIN

 !###======================================================================
 LOGICAL FUNCTION SOBEK1CALC()
 !###======================================================================
 IMPLICIT NONE
 
 SOBEK1CALC=.FALSE.
 
 WRITE(*,'(//A//)') 'Cannot use this function in this version of iMOD'
 
 SOBEK1CALC=.TRUE. 
 
 END FUNCTION SOBEK1CALC

END MODULE MOD_SOBEK
