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
MODULE MOD_SMPLX

USE SMPLX_COMPUTE, ONLY : SMPLX
USE MOD_SMPLX_PAR

CONTAINS

 !###======================================================================
 SUBROUTINE SMPLX_MAIN()
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: MXITER=5000
 INTEGER :: POS,I,IND,ITER,J,XDIM,K  
 REAL (DP) :: Z,RERR
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IBASIS 
 REAL (DP), ALLOCATABLE,DIMENSION(:,:) :: BI 
 REAL (DP), ALLOCATABLE,DIMENSION(:) :: X,TEMP,IMULT 
 REAL(KIND=DP_KIND) :: XX
 INTEGER ::  KA,N0

 KA=NCON; N0=NVAR

 IF(.NOT.ALLOCATED(A))ALLOCATE(A(KA,N0))
 IF(.NOT.ALLOCATED(B))ALLOCATE(B(KA)) 
 IF(.NOT.ALLOCATED(C))ALLOCATE(C(N0)) 
 IF(.NOT.ALLOCATED(CONSTR_TYPE))ALLOCATE(CONSTR_TYPE(KA))

 ALLOCATE(TEMP(KA),ISORT(KA),IMULT(KA),IBASIS(KA),BI(KA,KA))

 !## initialize
 IBASIS     =0
 BI         =0.0D0
 TEMP       =0.0D0
 ISORT      =0
 IMULT      =1.0D0

 IF(ALLOCATED(IN_CON))THEN
  CONSTR_TYPE=0; A=0.0D0; B=0.0D0; C=0.0D0; NUMLE=0; NUMGE=0
  DO I=1,NCON
   READ(IN_CON(I)(:), *) A(I,1:NVAR)
   ISORT(I)=I
   POS=INDEX(IN_CON(I), '=')
   IF(POS.EQ.0) THEN
    WRITE(*,'(A)') 'YOU MUST ENTER THE CONSTRAINT TYPE (<=, >= OR =)'
    ICNVG=1; LPSTATUS='Input error detected'
    DEALLOCATE(A,B,C,X,TEMP,CONSTR_TYPE,IBASIS,IMULT,BI,ISORT); RETURN
   END IF
   IF(IN_CON(I)(POS-1:POS-1).EQ.'<') THEN
    CONSTR_TYPE(I) = 1
    NUMLE = NUMLE + 1
   ELSE IF (IN_CON(I)(POS-1:POS-1).EQ.'>') THEN
    CONSTR_TYPE(I) = 2
    NUMGE = NUMGE + 1
   ELSE
    CONSTR_TYPE(I) = 3
   END IF
   READ(IN_CON(I)(POS+1:), *) B(I)
  END DO
  READ(IN_OBJ, *) C(1:NVAR)
 ENDIF
 
 DO I=1,KA
  IF(B(I).LT.0.0D0)THEN
   A(I,:)=-1.0D0*A(I,:)
   B(I)  =-1.0D0*B(I)
   IF(CONSTR_TYPE(I).EQ.1)THEN
    CONSTR_TYPE(I)=2
    NUMLE=NUMLE-1
    NUMGE=NUMGE+1
   ELSEIF(CONSTR_TYPE(I).EQ.2)THEN
    CONSTR_TYPE(I)=1
    NUMLE=NUMLE+1
    NUMGE=NUMGE-1
   ENDIF
   IMULT(I)=-1.0D0*IMULT(I)
  ENDIF
 END DO

 !     RE-ORDER THE CONSTRAINTS IF NECESSARY
 DO I = 1, NUMLE
   IF (CONSTR_TYPE(I) /= 1) THEN
     DO J = NUMLE+1, NCON
       IF (CONSTR_TYPE(J) == 1) THEN
         TEMP(1:NVAR) = A(I, 1:NVAR)
         A(I, 1:NVAR) = A(J, 1:NVAR)
         A(J, 1:NVAR) = TEMP(1:NVAR)
         Z = B(I)
         B(I) = B(J)
         B(J) = Z
         CONSTR_TYPE(J) = CONSTR_TYPE(I)
         CONSTR_TYPE(I) = 1
         K = ISORT(I)
         ISORT(I) = ISORT(J)
         ISORT(J) = K
         K = IMULT(I)
         IMULT(I) = IMULT(J)
         IMULT(J) = K
         EXIT
       END IF
     END DO
   END IF
 END DO
 DO I = NUMLE+1, NUMLE+NUMGE
   IF (CONSTR_TYPE(I) /= 2) THEN
     DO J = NUMLE+NUMGE+1, NCON
       IF (CONSTR_TYPE(J) == 2) THEN
         TEMP(1:NVAR) = A(I, 1:NVAR)
         A(I, 1:NVAR) = A(J, 1:NVAR)
         A(J, 1:NVAR) = TEMP(1:NVAR)
         Z = B(I)
         B(I) = B(J)
         B(J) = Z
         CONSTR_TYPE(J) = CONSTR_TYPE(I)
         CONSTR_TYPE(I) = 2
         K = ISORT(I)
         ISORT(I) = ISORT(J)
         ISORT(J) = K
         K = IMULT(I)
         IMULT(I) = IMULT(J)
         IMULT(J) = K
         EXIT
       END IF
     END DO
   END IF
 END DO

 !## copy to be used in main-programm

 XDIM=NVAR+NUMLE+NUMGE
 ALLOCATE(X(XDIM))

 !## initialize
 X=0.0D0

 !## one variable
 IF(N0.EQ.1)THEN
  X(1)=-999.0
  DO I=1,NUMGE; X(1)=MAX(B(NUMLE+I)/A(NUMLE+I,1),X(1)); END DO
  IND=0; DO I=1,NUMLE; IF(X(1)*A(I,1).GT.B(I))IND=1; END DO
  Z=X(1)
 ELSE
  !## select its own basis, so initialize, thanks Wilbert
  IND=0
  CALL SMPLX (A, B, C, KA, XDIM, N0, IND, IBASIS, X, Z, ITER, MXITER,   &
              NUMLE, NUMGE, BI, RERR)
 ENDIF

 ICNVG=1
 SELECT CASE (IND)
  CASE (0)
   LPSTATUS='The problem was solved'
   ICNVG=0
  CASE (1)
   LPSTATUS='The problem has no solution'
  CASE (2)
   LPSTATUS='Maximum number of iterations were performed; more needed'
  CASE (3)
   LPSTATUS='Sufficient accuracy could not be maintained to solve the problem'
  CASE (4)
   LPSTATUS='The problem has an unbounded solution'
  CASE (5)
   LPSTATUS='Input error detected, number of variable should be >1'
  CASE (6)
   LPSTATUS='The solution may have been obtained'
  CASE DEFAULT
   LPSTATUS='Unknown error!'
 END SELECT

 !## error control
 IF(.NOT.ALLOCATED(XVAR))ALLOCATE(XVAR(NVAR))
 IF(.NOT.ALLOCATED(XSLK))ALLOCATE(XSLK(NCON))

 !## initialize
 XVAR=0.0D0
 XSLK=0.0D0

 DO I=1,NCON
  XX=0.0D0; DO J=1,NVAR; XX=XX+(A(I,J)*X(J)*IMULT(I)); END DO
  XSLK(I)=XX
 END DO

 DO I=1,NVAR; XVAR(I)=X(I); END DO
 ZOBJ=Z

 DEALLOCATE(X,TEMP,CONSTR_TYPE,IBASIS,IMULT,BI) !,XSLK) !,ISORT)
 
 END SUBROUTINE SMPLX_MAIN

END MODULE MOD_SMPLX
