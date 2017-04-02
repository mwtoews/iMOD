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
 MODULE SMPLX_CONSTANTS_NSWC
 ! CONTAINS THE NSWC FUNCTIONS IPMPAR, SPMPAR, DPMPAR, EPSLN, DEPSLN,
 ! EXPARG & DXPARG
 !-----------------------------------------------------------------------
 !     WRITTEN USING F90 INTRINSICS BY
 !        ALAN MILLER
 !        CSIRO MATHEMATICAL & INFORMATION SCIENCES
 !        CLAYTON, VICTORIA, AUSTRALIA 3169
 !     LATEST REVISION - 1 FEBRUARY 1997
 !-----------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, PARAMETER     :: DP = SELECTED_REAL_KIND(15, 60)

 CONTAINS

 !###======================================================================
 FUNCTION IPMPAR (I) RESULT(FN_VAL)
 !###======================================================================
 !-----------------------------------------------------------------------

 !     IPMPAR PROVIDES THE INTEGER MACHINE CONSTANTS FOR THE COMPUTER
 !     THAT IS USED. IT IS ASSUMED THAT THE ARGUMENT I IS AN INTEGER
 !     HAVING ONE OF THE VALUES 1-10. IPMPAR(I) HAS THE VALUE ...

 !  INTEGERS.

 !     ASSUME INTEGERS ARE REPRESENTED IN THE N-DIGIT, BASE-A FORM

 !               SIGN ( X(N-1)*A**(N-1) + ... + X(1)*A + X(0) )

 !               WHERE 0 .LE. X(I) .LT. A FOR I=0,...,N-1.

 !     IPMPAR(1) = A, THE BASE (RADIX).

 !     IPMPAR(2) = N, THE NUMBER OF BASE-A DIGITS (DIGITS).

 !     IPMPAR(3) = A**N - 1, THE LARGEST MAGNITUDE (HUGE).

 !  FLOATING-POINT NUMBERS.

 !     IT IS ASSUMED THAT THE SINGLE AND DOUBLE PRECISION FLOATING
 !     POINT ARITHMETICS HAVE THE SAME BASE, SAY B, AND THAT THE
 !     NONZERO NUMBERS ARE REPRESENTED IN THE FORM

 !               SIGN (B**E) * (X(1)/B + ... + X(M)/B**M)

 !               WHERE X(I) = 0,1,...,B-1 FOR I=1,...,M,
 !               X(1) .GE. 1, AND EMIN .LE. E .LE. EMAX.

 !     IPMPAR(4) = B, THE BASE.

 !  SINGLE-PRECISION

 !     IPMPAR(5) = M, THE NUMBER OF BASE-B DIGITS.

 !     IPMPAR(6) = EMIN, THE SMALLEST EXPONENT E.

 !     IPMPAR(7) = EMAX, THE LARGEST EXPONENT E.

 !  DOUBLE-PRECISION

 !     IPMPAR(8) = M, THE NUMBER OF BASE-B DIGITS.

 !     IPMPAR(9) = EMIN, THE SMALLEST EXPONENT E.

 !     IPMPAR(10) = EMAX, THE LARGEST EXPONENT E.

 !-----------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, INTENT(IN) :: I
 INTEGER             :: FN_VAL

 SELECT CASE(I)
   CASE( 1)
     FN_VAL = RADIX(I)
   CASE( 2)
     FN_VAL = DIGITS(I)
   CASE( 3)
     FN_VAL = HUGE(I)
   CASE( 4)
     FN_VAL = RADIX(1.0)
   CASE( 5)
     FN_VAL = DIGITS(1.0)
   CASE( 6)
     FN_VAL = MINEXPONENT(1.0)
   CASE( 7)
     FN_VAL = MAXEXPONENT(1.0)
   CASE( 8)
     FN_VAL = DIGITS(1.0D0)
   CASE( 9)
     FN_VAL = MINEXPONENT(1.0D0)
   CASE(10)
     FN_VAL = MAXEXPONENT(1.0D0)
   CASE DEFAULT
     RETURN
 END SELECT

 RETURN
 END FUNCTION IPMPAR

 !###======================================================================
 FUNCTION SPMPAR (I) RESULT(FN_VAL)
 !###======================================================================
 !-----------------------------------------------------------------------

 !     SPMPAR PROVIDES THE SINGLE PRECISION MACHINE CONSTANTS FOR
 !     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT
 !     I IS AN INTEGER HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE
 !     SINGLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND
 !     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN

 !        SPMPAR(1) = B**(1 - M), THE MACHINE PRECISION,

 !        SPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,

 !        SPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE.
 !-----------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, INTENT(IN) :: I
 REAL                :: FN_VAL

 ! LOCAL VARIABLE
 REAL                :: ONE = 1.0

 SELECT CASE (I)
   CASE (1)
     FN_VAL = EPSILON(ONE)
   CASE (2)
     FN_VAL = TINY(ONE)
   CASE (3)
     FN_VAL = HUGE(ONE)
 END SELECT

 RETURN
 END FUNCTION SPMPAR

 !###======================================================================
 FUNCTION DPMPAR (I) RESULT(FN_VAL)
 !###======================================================================
 !-----------------------------------------------------------------------

 !     DPMPAR PROVIDES THE DOUBLE PRECISION MACHINE CONSTANTS FOR
 !     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT
 !     I IS AN INTEGER HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE
 !     DOUBLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND
 !     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN

 !        DPMPAR(1) = B**(1 - M), THE MACHINE PRECISION,

 !        DPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,

 !        DPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE.
 !-----------------------------------------------------------------------

 IMPLICIT NONE
 INTEGER, INTENT(IN) :: I
 REAL (DP)           :: FN_VAL

 ! LOCAL VARIABLE
 REAL (DP)    :: ONE = 1._DP

 SELECT CASE (I)
   CASE (1)
     FN_VAL = EPSILON(ONE)
   CASE (2)
     FN_VAL = TINY(ONE)
   CASE (3)
     FN_VAL = HUGE(ONE)
 END SELECT

 RETURN
 END FUNCTION DPMPAR

 !###======================================================================
 FUNCTION EPSLN () RESULT(FN_VAL)
 !###======================================================================
 !--------------------------------------------------------------------
 !     THE EVALUATION OF LN(EPS) WHERE EPS IS THE SMALLEST NUMBER
 !     SUCH THAT 1.0 + EPS .GT. 1.0 .  L IS A DUMMY ARGUMENT.
 !--------------------------------------------------------------------
 IMPLICIT NONE
 REAL                :: FN_VAL

 ! LOCAL VARIABLE
 REAL                :: ONE = 1.0

 FN_VAL = LOG( EPSILON(ONE) )
 RETURN
 END FUNCTION EPSLN

 !###======================================================================
 FUNCTION EXPARG (L) RESULT(FN_VAL)
 !###======================================================================
 !--------------------------------------------------------------------
 !     IF L = 0 THEN  EXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
 !     EXP(W) CAN BE COMPUTED.
 !
 !     IF L IS NONZERO THEN  EXPARG(L) = THE LARGEST NEGATIVE W FOR
 !     WHICH THE COMPUTED VALUE OF EXP(W) IS NONZERO.
 !
 !     NOTE... ONLY AN APPROXIMATE VALUE FOR EXPARG(L) IS NEEDED.
 !--------------------------------------------------------------------
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: L
 REAL                :: FN_VAL

 ! LOCAL VARIABLE
 REAL                :: ONE = 1.0

 IF (L == 0) THEN
   FN_VAL = LOG( HUGE(ONE) )
 ELSE
   FN_VAL = LOG( TINY(ONE) )
 END IF
 RETURN
 END FUNCTION EXPARG

 !###======================================================================
 FUNCTION DEPSLN () RESULT(FN_VAL)
 !###======================================================================
 !--------------------------------------------------------------------
 !     THE EVALUATION OF LN(EPS) WHERE EPS IS THE SMALLEST NUMBER
 !     SUCH THAT 1.D0 + EPS .GT. 1.D0 .  L IS A DUMMY ARGUMENT.
 !--------------------------------------------------------------------
 IMPLICIT NONE
 REAL (DP)           :: FN_VAL

 ! LOCAL VARIABLE
 REAL (DP)    :: ONE = 1._DP

 FN_VAL = LOG( EPSILON(ONE) )
 RETURN
 END FUNCTION DEPSLN

 !###======================================================================
 FUNCTION DXPARG (L) RESULT(FN_VAL)
 !###======================================================================
 !--------------------------------------------------------------------
 !     IF L = 0 THEN  DXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
 !     DEXP(W) CAN BE COMPUTED.
 !
 !     IF L IS NONZERO THEN  DXPARG(L) = THE LARGEST NEGATIVE W FOR
 !     WHICH THE COMPUTED VALUE OF DEXP(W) IS NONZERO.
 !
 !     NOTE... ONLY AN APPROXIMATE VALUE FOR DXPARG(L) IS NEEDED.
 !--------------------------------------------------------------------
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: L
 REAL (DP)           :: FN_VAL

 ! LOCAL VARIABLE
 REAL (DP)    :: ONE = 1._DP

 IF (L == 0) THEN
   FN_VAL = LOG( HUGE(ONE) )
 ELSE
   FN_VAL = LOG( TINY(ONE) )
 END IF
 RETURN
 END FUNCTION DXPARG

 END MODULE SMPLX_CONSTANTS_NSWC

