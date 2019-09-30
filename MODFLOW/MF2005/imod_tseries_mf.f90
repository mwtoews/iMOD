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

!###====================================================================
REAL FUNCTION MF2005_TSERIE1HMEAN(X,Y,ICOL,IROW,ILAY)
!###====================================================================
USE IMOD_UTL, ONLY: IMOD_UTL_POL1INTMAIN
USE GLOBAL, ONLY: NROW,NCOL,HNEW,NLAY
USE GWFBASMODULE, ONLY: HNOFLO
use gwfmetmodule, only: cdelr, cdelc
IMPLICIT NONE
INTEGER,INTENT(IN) :: ICOL,IROW,ILAY
INTEGER :: I,J,IR,IC,JR,JC,JLAY
REAL(KIND=8),INTENT(IN) :: X,Y
REAL(KIND=8),DIMENSION(3) :: XCRD,YCRD
REAL(KIND=8),DIMENSION(3,3) :: ZCRD
REAL(KIND=8),DIMENSION(1,1) :: XINT

XCRD=0.0D0; YCRD=0.0D0; ZCRD=HNOFLO

J=0
DO IR=IROW-1,IROW+1
 J=J+1; I=0
 IF(IR.LT.1)THEN
  YCRD(J)=CDELR(IR)+(CDELC(IR)-CDELC(IR+1))/2.0D0
 ELSEIF(IR.GT.NROW)THEN
  YCRD(J)=CDELC(IR-1)-(CDELC(IR-2)-CDELC(IR-1))/2.0D0
 ELSE
  YCRD(J)=(CDELC(IR)+CDELC(IR-1))/2.0
 ENDIF
 DO IC=ICOL-1,ICOL+1
  I=I+1
  IF(IC.LT.1)THEN
   XCRD(I)=CDELR(IC)-(CDELR(IC+1)-CDELR(IC))/2.0D0
  ELSEIF(IC.GT.NCOL)THEN
   XCRD(I)=CDELR(IC-1)+(CDELR(IC-2)-CDELR(IC-1))/2.0D0
  ELSE
   XCRD(I)=(CDELR(IC)+CDELR(IC-1))/2.0D0
  ENDIF
  !## get first active modellayer
  JC=MIN(NCOL,MAX(1,IC)); JR=MIN(NROW,MAX(1,IR))
  IF(ILAY.EQ.0)THEN
   DO JLAY=1,NLAY
    IF(HNEW(JC,JR,JLAY).NE.HNOFLO)THEN; ZCRD(I,J)=HNEW(JC,JR,JLAY); EXIT; ENDIF
   ENDDO
  ELSE
   ZCRD(I,J)=HNEW(JC,JR,ILAY) !HNEW(MIN(NCOL,MAX(1,IC)),MIN(NROW,MAX(1,IR)),ILAY)
  ENDIF
 ENDDO
ENDDO

XINT=0.0D0

CALL IMOD_UTL_POL1INTMAIN(1,1,3,3,XCRD,YCRD,ZCRD,(/X,X/),(/Y,Y/),XINT,4,DBLE(HNOFLO))

MF2005_TSERIE1HMEAN=XINT(1,1)

END FUNCTION

