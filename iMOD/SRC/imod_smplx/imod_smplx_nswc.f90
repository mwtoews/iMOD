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
 MODULE SMPLX_CONSTANTS_NSWC

 IMPLICIT NONE
 INTEGER, PARAMETER     :: DP = SELECTED_REAL_KIND(15, 60)

 CONTAINS

 !###======================================================================
 FUNCTION IPMPAR (I) RESULT(FN_VAL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: I
 INTEGER :: FN_VAL

 RETURN
 END FUNCTION IPMPAR

 !###======================================================================
 FUNCTION SPMPAR (I) RESULT(FN_VAL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: I
 REAL :: FN_VAL

 RETURN
 END FUNCTION SPMPAR

 !###======================================================================
 FUNCTION DPMPAR (I) RESULT(FN_VAL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: I
 REAL (DP) :: FN_VAL

 RETURN
 END FUNCTION DPMPAR

 !###======================================================================
 FUNCTION EPSLN () RESULT(FN_VAL)
 IMPLICIT NONE
 REAL :: FN_VAL

 RETURN
 END FUNCTION EPSLN

 !###======================================================================
 FUNCTION EXPARG (L) RESULT(FN_VAL)
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: L
 REAL :: FN_VAL

 RETURN
 END FUNCTION EXPARG

 !###======================================================================
 FUNCTION DEPSLN () RESULT(FN_VAL)
 !###======================================================================
 IMPLICIT NONE
 REAL (DP) :: FN_VAL

 RETURN
 END FUNCTION DEPSLN

 !###======================================================================
 FUNCTION DXPARG (L) RESULT(FN_VAL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: L
 REAL (DP)           :: FN_VAL

 RETURN
 END FUNCTION DXPARG

 END MODULE SMPLX_CONSTANTS_NSWC

