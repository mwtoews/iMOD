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
MODULE SMPLX_COMPUTE

USE SMPLX_CONSTANTS_NSWC

CONTAINS

 !###======================================================================
 SUBROUTINE SMPLX (A, B0, C, KA, XDIM, N0, IND, IBASIS, X, Z, ITER, MXITER,   &
                   NUMLE, NUMGE, BI, RERR)
 !###======================================================================
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: KA, XDIM, N0, MXITER, NUMLE, NUMGE
 INTEGER, INTENT(IN OUT) :: IND
 INTEGER, INTENT(OUT) :: ITER
 REAL (DP), INTENT(OUT) :: Z, RERR
 INTEGER, DIMENSION(KA), INTENT(IN OUT)  :: IBASIS
 REAL (DP), DIMENSION(KA,N0), INTENT(IN)  :: A
 REAL (DP), DIMENSION(KA), INTENT(IN) :: B0
 REAL (DP), DIMENSION(N0), INTENT(IN) :: C
 REAL (DP), DIMENSION(XDIM), INTENT(OUT)   :: X
 REAL (DP), DIMENSION(KA,KA), INTENT(OUT) :: BI

 END SUBROUTINE SMPLX

 !###======================================================================
 SUBROUTINE SMPLX1 (A, B0, C, KA, XDIM, N0, IND, IBASIS, R, Z, ITER, MXITER,  &
                    EPS0, RERRMN, RERRMX, RERR, NUMLE, NUMGE, BI)
 !###======================================================================
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: KA, XDIM, N0, MXITER, NUMLE, NUMGE
 INTEGER, INTENT(OUT) :: IND, ITER
 REAL (DP), INTENT(OUT) :: Z, RERR
 REAL (DP), INTENT(IN) :: EPS0, RERRMN, RERRMX
 REAL (DP), DIMENSION(KA,N0), INTENT(IN)  :: A !
 REAL (DP), DIMENSION(KA), INTENT(IN) :: B0 !
 REAL (DP), DIMENSION(N0), INTENT(IN) :: C !
 REAL (DP), DIMENSION(KA,KA), INTENT(OUT) :: BI !
 REAL (DP), DIMENSION(XDIM), INTENT(OUT) :: R
 INTEGER, DIMENSION(KA), INTENT(IN OUT) :: IBASIS


 END SUBROUTINE SMPLX1

 !###======================================================================
 SUBROUTINE CROUT1(A, KA, N, IEND, INDX, TEMP, IERR)
 !###======================================================================
 IMPLICIT NONE
 INTEGER, INTENT(IN)                     :: KA, N, IEND
 INTEGER, INTENT(OUT)                    :: IERR
 REAL (DP), DIMENSION(KA*N), INTENT(IN OUT) :: A!, TEMP
 REAL (DP), DIMENSION(:), INTENT(IN OUT) :: TEMP
 INTEGER, DIMENSION(:), INTENT(IN OUT)   :: INDX

 END SUBROUTINE CROUT1

END MODULE SMPLX_COMPUTE
