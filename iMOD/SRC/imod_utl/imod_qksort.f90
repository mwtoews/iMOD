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
MODULE MOD_QKSORT

USE IMODVAR, ONLY : DP_KIND,SP_KIND

CONTAINS

 !###====================================================
 SUBROUTINE QKSORT(N,V1,V2,V3,V4,V5,V6,V7)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(N) :: V1
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(N),OPTIONAL :: V2,V3,V4,V5,V6,V7
 INTEGER :: I,M
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: ARR

 M=1
 IF(PRESENT(V2))M=M+1; IF(PRESENT(V3))M=M+1; IF(PRESENT(V4))M=M+1
 IF(PRESENT(V5))M=M+1; IF(PRESENT(V6))M=M+1; IF(PRESENT(V7))M=M+1
 ALLOCATE(ARR(N,M))

 !## copy first vector in array
 DO I=1,N; ARR(I,1)=V1(I); ENDDO
 
 IF(M.EQ.1)THEN

  CALL QKSORT_SGL(N,ARR(:,1))
  DO I=1,N; V1(I)=ARR(I,1); ENDDO

 ELSE

  M=1
  IF(PRESENT(V2))THEN; M=M+1; DO I=1,N; ARR(I,M)=V2(I); ENDDO; ENDIF
  IF(PRESENT(V3))THEN; M=M+1; DO I=1,N; ARR(I,M)=V3(I); ENDDO; ENDIF
  IF(PRESENT(V4))THEN; M=M+1; DO I=1,N; ARR(I,M)=V4(I); ENDDO; ENDIF
  IF(PRESENT(V5))THEN; M=M+1; DO I=1,N; ARR(I,M)=V5(I); ENDDO; ENDIF
  IF(PRESENT(V6))THEN; M=M+1; DO I=1,N; ARR(I,M)=V6(I); ENDDO; ENDIF
  IF(PRESENT(V7))THEN; M=M+1; DO I=1,N; ARR(I,M)=V7(I); ENDDO; ENDIF
  CALL QKSORT_DBL(N,ARR)
  M=1; DO I=1,N; V1(I)=ARR(I,M); ENDDO
  IF(PRESENT(V2))THEN; M=M+1; DO I=1,N; V2(I)=ARR(I,M); ENDDO; ENDIF
  IF(PRESENT(V3))THEN; M=M+1; DO I=1,N; V3(I)=ARR(I,M); ENDDO; ENDIF
  IF(PRESENT(V4))THEN; M=M+1; DO I=1,N; V4(I)=ARR(I,M); ENDDO; ENDIF
  IF(PRESENT(V5))THEN; M=M+1; DO I=1,N; V5(I)=ARR(I,M); ENDDO; ENDIF
  IF(PRESENT(V6))THEN; M=M+1; DO I=1,N; V6(I)=ARR(I,M); ENDDO; ENDIF
  IF(PRESENT(V7))THEN; M=M+1; DO I=1,N; V7(I)=ARR(I,M); ENDDO; ENDIF

 ENDIF
  
 DEALLOCATE(ARR)

 END SUBROUTINE QKSORT

 !###====================================================
 SUBROUTINE QKSORT_INT(N,V1,V2,V3,V4,V5,V6)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER(KIND=SP_KIND),INTENT(INOUT),DIMENSION(:) :: V1
 INTEGER(KIND=SP_KIND),INTENT(INOUT),DIMENSION(:),OPTIONAL :: V2,V3,V4,V5,V6
 INTEGER :: I,M
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: ARR

 M=1; IF(PRESENT(V2))M=M+1; IF(PRESENT(V3))M=M+1; IF(PRESENT(V4))M=M+1; IF(PRESENT(V5))M=M+1; IF(PRESENT(V6))M=M+1
 ALLOCATE(ARR(N,M))

 !## copy first vector in array
 DO I=1,N; ARR(I,1)=DBLE(V1(I)); ENDDO

 IF(M.EQ.1)THEN
  
  CALL QKSORT_SGL(N,ARR(:,1))
  DO I=1,N; V1(I)=INT(ARR(I,M),4); ENDDO
 
 ELSE
 
  M=1
  IF(PRESENT(V2))THEN; M=M+1; DO I=1,N; ARR(I,M)=DBLE(V2(I)); ENDDO; ENDIF
  IF(PRESENT(V3))THEN; M=M+1; DO I=1,N; ARR(I,M)=DBLE(V3(I)); ENDDO; ENDIF
  IF(PRESENT(V4))THEN; M=M+1; DO I=1,N; ARR(I,M)=DBLE(V4(I)); ENDDO; ENDIF
  IF(PRESENT(V5))THEN; M=M+1; DO I=1,N; ARR(I,M)=DBLE(V5(I)); ENDDO; ENDIF
  IF(PRESENT(V6))THEN; M=M+1; DO I=1,N; ARR(I,M)=DBLE(V6(I)); ENDDO; ENDIF
  CALL QKSORT_DBL(N,ARR)
  M=1; DO I=1,N; V1(I)=ARR(I,M); ENDDO
  IF(PRESENT(V2))THEN; M=M+1; DO I=1,N; V2(I)=INT(ARR(I,M),4); ENDDO; ENDIF
  IF(PRESENT(V3))THEN; M=M+1; DO I=1,N; V3(I)=INT(ARR(I,M),4); ENDDO; ENDIF
  IF(PRESENT(V4))THEN; M=M+1; DO I=1,N; V4(I)=INT(ARR(I,M),4); ENDDO; ENDIF
  IF(PRESENT(V5))THEN; M=M+1; DO I=1,N; V5(I)=INT(ARR(I,M),4); ENDDO; ENDIF
  IF(PRESENT(V6))THEN; M=M+1; DO I=1,N; V6(I)=INT(ARR(I,M),4); ENDDO; ENDIF
 
 ENDIF
  
 DEALLOCATE(ARR)

 END SUBROUTINE QKSORT_INT

 !###====================================================
 SUBROUTINE QKSORT_INT8(N,V1,V2,V3,V4,V5,V6)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER(KIND=DP_KIND),INTENT(INOUT),DIMENSION(:) :: V1
 INTEGER(KIND=DP_KIND),INTENT(INOUT),DIMENSION(:),OPTIONAL :: V2,V3,V4,V5,V6
 INTEGER :: I,M
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: ARR

 M=1; IF(PRESENT(V2))M=M+1; IF(PRESENT(V3))M=M+1; IF(PRESENT(V4))M=M+1; IF(PRESENT(V5))M=M+1; IF(PRESENT(V6))M=M+1
 ALLOCATE(ARR(N,M))
 
 !## copy first vector in array
 DO I=1,N; ARR(I,1)=DBLE(V1(I)); ENDDO
 
 IF(M.EQ.1)THEN

  CALL QKSORT_SGL(N,ARR(:,1))
  DO I=1,N; V1(I)=INT(ARR(I,1),8); ENDDO

 ELSE

  M=1
  IF(PRESENT(V2))THEN; M=M+1; DO I=1,N; ARR(I,M)=DBLE(V2(I)); ENDDO; ENDIF
  IF(PRESENT(V3))THEN; M=M+1; DO I=1,N; ARR(I,M)=DBLE(V3(I)); ENDDO; ENDIF
  IF(PRESENT(V4))THEN; M=M+1; DO I=1,N; ARR(I,M)=DBLE(V4(I)); ENDDO; ENDIF
  IF(PRESENT(V5))THEN; M=M+1; DO I=1,N; ARR(I,M)=DBLE(V5(I)); ENDDO; ENDIF
  IF(PRESENT(V6))THEN; M=M+1; DO I=1,N; ARR(I,M)=DBLE(V6(I)); ENDDO; ENDIF
  CALL QKSORT_DBL(N,ARR)
  M=1; DO I=1,N; V1(I)=ARR(I,M); ENDDO
  IF(PRESENT(V2))THEN; M=M+1; DO I=1,N; V2(I)=INT(ARR(I,M),8); ENDDO; ENDIF
  IF(PRESENT(V3))THEN; M=M+1; DO I=1,N; V3(I)=INT(ARR(I,M),8); ENDDO; ENDIF
  IF(PRESENT(V4))THEN; M=M+1; DO I=1,N; V4(I)=INT(ARR(I,M),8); ENDDO; ENDIF
  IF(PRESENT(V5))THEN; M=M+1; DO I=1,N; V5(I)=INT(ARR(I,M),8); ENDDO; ENDIF
  IF(PRESENT(V6))THEN; M=M+1; DO I=1,N; V6(I)=INT(ARR(I,M),8); ENDDO; ENDIF
 ENDIF
  
 DEALLOCATE(ARR)

 END SUBROUTINE QKSORT_INT8
 
 !###====================================================
 SUBROUTINE QKSORT_INT4REAL8(N,V1,V2,V3,V4,V5,V6)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER(KIND=SP_KIND),INTENT(INOUT),DIMENSION(:) :: V1
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(:),OPTIONAL :: V2,V3,V4,V5,V6
 INTEGER :: I,M
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: ARR

 M=1; IF(PRESENT(V2))M=M+1; IF(PRESENT(V3))M=M+1; IF(PRESENT(V4))M=M+1; IF(PRESENT(V5))M=M+1; IF(PRESENT(V6))M=M+1
 ALLOCATE(ARR(N,M))
 
 DO I=1,N; ARR(I,1)=DBLE(V1(I)); ENDDO

 IF(M.EQ.1)THEN

  CALL QKSORT_SGL(N,ARR(:,1))
  DO I=1,N; V1(I)=INT(ARR(I,M),4); ENDDO

 ELSE

  M=1
  IF(PRESENT(V2))THEN; M=M+1; DO I=1,N; ARR(I,M)=V2(I); ENDDO; ENDIF
  IF(PRESENT(V3))THEN; M=M+1; DO I=1,N; ARR(I,M)=V3(I); ENDDO; ENDIF
  IF(PRESENT(V4))THEN; M=M+1; DO I=1,N; ARR(I,M)=V4(I); ENDDO; ENDIF
  IF(PRESENT(V5))THEN; M=M+1; DO I=1,N; ARR(I,M)=V5(I); ENDDO; ENDIF
  IF(PRESENT(V6))THEN; M=M+1; DO I=1,N; ARR(I,M)=V6(I); ENDDO; ENDIF
  CALL QKSORT_DBL(N,ARR)
  M=1; DO I=1,N; V1(I)=ARR(I,M); ENDDO
  IF(PRESENT(V2))THEN; M=M+1; DO I=1,N; V2(I)=ARR(I,M); ENDDO; ENDIF
  IF(PRESENT(V3))THEN; M=M+1; DO I=1,N; V3(I)=ARR(I,M); ENDDO; ENDIF
  IF(PRESENT(V4))THEN; M=M+1; DO I=1,N; V4(I)=ARR(I,M); ENDDO; ENDIF
  IF(PRESENT(V5))THEN; M=M+1; DO I=1,N; V5(I)=ARR(I,M); ENDDO; ENDIF
  IF(PRESENT(V6))THEN; M=M+1; DO I=1,N; V6(I)=ARR(I,M); ENDDO; ENDIF

 ENDIF
  
 DEALLOCATE(ARR)

 END SUBROUTINE QKSORT_INT4REAL8
  
 !###====================================================
 SUBROUTINE QKSORT_P_INT4REAL8(N,V1,V2,V3,V4,V5,V6)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER(KIND=SP_KIND),INTENT(INOUT),DIMENSION(:),POINTER :: V1
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(:),OPTIONAL,POINTER :: V2,V3,V4,V5,V6
 INTEGER :: I,M
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: ARR

 M=1; IF(PRESENT(V2))M=M+1; IF(PRESENT(V3))M=M+1; IF(PRESENT(V4))M=M+1; IF(PRESENT(V5))M=M+1; IF(PRESENT(V6))M=M+1
 ALLOCATE(ARR(N,M))
 
 DO I=1,N; ARR(I,1)=DBLE(V1(I)); ENDDO

 IF(M.EQ.1)THEN

  CALL QKSORT_SGL(N,ARR(:,1))
  DO I=1,N; V1(I)=INT(ARR(I,M),4); ENDDO

 ELSE

  M=1
  IF(PRESENT(V2))THEN; M=M+1; DO I=1,N; ARR(I,M)=V2(I); ENDDO; ENDIF
  IF(PRESENT(V3))THEN; M=M+1; DO I=1,N; ARR(I,M)=V3(I); ENDDO; ENDIF
  IF(PRESENT(V4))THEN; M=M+1; DO I=1,N; ARR(I,M)=V4(I); ENDDO; ENDIF
  IF(PRESENT(V5))THEN; M=M+1; DO I=1,N; ARR(I,M)=V5(I); ENDDO; ENDIF
  IF(PRESENT(V6))THEN; M=M+1; DO I=1,N; ARR(I,M)=V6(I); ENDDO; ENDIF
  CALL QKSORT_DBL(N,ARR)
  M=1; DO I=1,N; V1(I)=ARR(I,M); ENDDO
  IF(PRESENT(V2))THEN; M=M+1; DO I=1,N; V2(I)=ARR(I,M); ENDDO; ENDIF
  IF(PRESENT(V3))THEN; M=M+1; DO I=1,N; V3(I)=ARR(I,M); ENDDO; ENDIF
  IF(PRESENT(V4))THEN; M=M+1; DO I=1,N; V4(I)=ARR(I,M); ENDDO; ENDIF
  IF(PRESENT(V5))THEN; M=M+1; DO I=1,N; V5(I)=ARR(I,M); ENDDO; ENDIF
  IF(PRESENT(V6))THEN; M=M+1; DO I=1,N; V6(I)=ARR(I,M); ENDDO; ENDIF

 ENDIF
  
 DEALLOCATE(ARR)

 END SUBROUTINE QKSORT_P_INT4REAL8

 !###====================================================
 SUBROUTINE QKSORT_SGL(N,ARR)
 !###====================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: M=7,NSTACK=50
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(:) :: ARR
 INTEGER :: I,IR,J,JSTACK,K,L
 INTEGER,DIMENSION(NSTACK) :: ISTACK
 REAL(KIND=DP_KIND) :: A,ATEMP
 
 JSTACK=0
 L=1
 IR=N
 1 IF(IR-L.LT.M)THEN
  DO J=L+1,IR
   A=ARR(J)
   DO I=J-1,1,-1
    IF(ARR(I).LE.A)GOTO 2
    ARR(I+1)=ARR(I)
   ENDDO
   I=0
 2 CONTINUE
   ARR(I+1)=A
  ENDDO
  IF(JSTACK.EQ.0)RETURN
  IR=ISTACK(JSTACK)
  L =ISTACK(JSTACK-1)
  JSTACK=JSTACK-2
 ELSE
  K=(L+IR)/2
  ATEMP   =ARR(K)
  ARR(K)  =ARR(L+1)
  ARR(L+1)=ATEMP
  IF(ARR(L+1).GT.ARR(IR))THEN
   ATEMP   =ARR(L+1)
   ARR(L+1)=ARR(IR)
   ARR(IR)=ATEMP
  ENDIF
  IF(ARR(L).GT.ARR(IR))THEN
   ATEMP  =ARR(L)
   ARR(L) =ARR(IR)
   ARR(IR)=ATEMP
  ENDIF
  IF(ARR(L+1).GT.ARR(L))THEN
   ATEMP   =ARR(L+1)
   ARR(L+1)=ARR(L)
   ARR(L)  =ATEMP
  ENDIF
  I=L+1
  J=IR
  A=ARR(L)
 3  CONTINUE
  I=I+1
  IF(ARR(I).LT.A)GOTO 3
 4  CONTINUE
  J=J-1
  IF(ARR(J).GT.A)GOTO 4
  IF(J.LT.I)GOTO 5
  ATEMP =ARR(I)
  ARR(I)=ARR(J)
  ARR(J)=ATEMP
  GOTO 3
 5 CONTINUE
  ARR(L)=ARR(J)
  ARR(J)=A
  JSTACK=JSTACK+2
  IF(JSTACK.GT.NSTACK)PAUSE 'NSTACK TOO SMALL'
  IF(IR-I+1.GT.J-L)THEN
   ISTACK(JSTACK)=IR
   ISTACK(JSTACK-1)=I
   IR=J-1
  ELSE
   ISTACK(JSTACK)=J-1
   ISTACK(JSTACK-1)=L
   L=I
  ENDIF
 ENDIF
 GOTO 1

 END SUBROUTINE QKSORT_SGL
 
 !###====================================================
 SUBROUTINE QKSORT_DBL(N,ARR)
 !###====================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: M=7,NSTACK=50
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(:,:) :: ARR
 INTEGER :: I,IR,J,JSTACK,K,L,NVAR,IVAR
 INTEGER,DIMENSION(NSTACK) :: ISTACK
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: A,ATEMP
 
 NVAR=SIZE(ARR,2); ALLOCATE(A(NVAR),ATEMP(NVAR))

 JSTACK=0
 L=1
 IR=N
 1 IF(IR-L.LT.M)THEN
  DO J=L+1,IR
   DO IVAR=1,NVAR; A(IVAR)=ARR(J,IVAR); ENDDO
   DO I=J-1,1,-1
    IF(ARR(I,1).LE.A(1))GOTO 2
    DO IVAR=1,NVAR; ARR(I+1,IVAR)=ARR(I,IVAR); ENDDO
   ENDDO
   I=0
 2 CONTINUE
   DO IVAR=1,NVAR; ARR(I+1,IVAR)=A(IVAR); ENDDO
  ENDDO
  IF(JSTACK.EQ.0)RETURN
  IR=ISTACK(JSTACK)
  L =ISTACK(JSTACK-1)
  JSTACK=JSTACK-2
 ELSE
  K=(L+IR)/2
  DO IVAR=1,NVAR; ATEMP(IVAR)=ARR(K,IVAR); ENDDO
  DO IVAR=1,NVAR; ARR(K,IVAR)=ARR(L+1,IVAR); ENDDO
  DO IVAR=1,NVAR; ARR(L+1,IVAR)=ATEMP(IVAR); ENDDO
  IF(ARR(L+1,1).GT.ARR(IR,1))THEN
   DO IVAR=1,NVAR; ATEMP(IVAR)=ARR(L+1,IVAR); ENDDO
   DO IVAR=1,NVAR; ARR(L+1,IVAR)=ARR(IR,IVAR); ENDDO
   DO IVAR=1,NVAR; ARR(IR,IVAR)=ATEMP(IVAR); ENDDO
  ENDIF
  IF(ARR(L,1).GT.ARR(IR,1))THEN
   DO IVAR=1,NVAR; ATEMP(IVAR)=ARR(L,IVAR); ENDDO
   DO IVAR=1,NVAR; ARR(L,IVAR)=ARR(IR,IVAR); ENDDO
   DO IVAR=1,NVAR; ARR(IR,IVAR)=ATEMP(IVAR); ENDDO
  ENDIF
  IF(ARR(L+1,1).GT.ARR(L,1))THEN
   DO IVAR=1,NVAR; ATEMP(IVAR)=ARR(L+1,IVAR); ENDDO
   DO IVAR=1,NVAR; ARR(L+1,IVAR)=ARR(L,IVAR); ENDDO
   DO IVAR=1,NVAR; ARR(L,IVAR)=ATEMP(IVAR); ENDDO
  ENDIF
  I=L+1
  J=IR
  DO IVAR=1,NVAR; A(IVAR)=ARR(L,IVAR); ENDDO
 3  CONTINUE
  I=I+1
  IF(ARR(I,1).LT.A(1))GOTO 3
 4  CONTINUE
  J=J-1
  IF(ARR(J,1).GT.A(1))GOTO 4
  IF(J.LT.I)GOTO 5
   DO IVAR=1,NVAR; ATEMP(IVAR)=ARR(I,IVAR); ENDDO
   DO IVAR=1,NVAR; ARR(I,IVAR)=ARR(J,IVAR); ENDDO
   DO IVAR=1,NVAR; ARR(J,IVAR)=ATEMP(IVAR); ENDDO
  GOTO 3
 5 CONTINUE
  DO IVAR=1,NVAR; ARR(L,IVAR)=ARR(J,IVAR); ENDDO
  DO IVAR=1,NVAR; ARR(J,IVAR)=A(IVAR); ENDDO
  JSTACK=JSTACK+2
  IF(JSTACK.GT.NSTACK)PAUSE 'NSTACK TOO SMALL'
  IF(IR-I+1.GT.J-L)THEN
   ISTACK(JSTACK)=IR
   ISTACK(JSTACK-1)=I
   IR=J-1
  ELSE
   ISTACK(JSTACK)=J-1
   ISTACK(JSTACK-1)=L
   L=I
  ENDIF
 ENDIF
 GOTO 1

 DEALLOCATE(A,ATEMP)

 END SUBROUTINE QKSORT_DBL

 !###====================================================
 SUBROUTINE SHELLSORT_INT(N,A)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,DIMENSION(N) :: A
 INTEGER :: I,J,INC
 INTEGER :: V

 INC=1
 1 INC=3*INC+1
 IF(INC.LE.N)GOTO 1
  2 CONTINUE
  INC=INC/3
  DO I=INC+1,N
   V=A(I)
   J=I
   3 IF(A(J-INC).GT.V)THEN
    A(J)=A(J-INC)
    J=J-INC
   IF(J.LE.INC)GOTO 4
    GOTO 3
   ENDIF
   4  A(J)=V
 END DO
 IF(INC.GT.1)GOTO 2

 END SUBROUTINE SHELLSORT_INT

 !###====================================================
 SUBROUTINE SHELLSORT_DINT(N,A)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER(KIND=8),DIMENSION(N) :: A
 INTEGER(KIND=8) :: I,J,INC
 INTEGER(KIND=8) :: V

 INC=1
 1 INC=3*INC+1
 IF(INC.LE.N)GOTO 1
  2 CONTINUE
  INC=INC/3
  DO I=INC+1,N
   V=A(I)
   J=I
   3 IF(A(J-INC).GT.V)THEN
    A(J)=A(J-INC)
    J=J-INC
   IF(J.LE.INC)GOTO 4
    GOTO 3
   ENDIF
   4  A(J)=V
 END DO
 IF(INC.GT.1)GOTO 2

 END SUBROUTINE SHELLSORT_DINT
 
END MODULE MOD_QKSORT
