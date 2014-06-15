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
!###==================================================
SUBROUTINE UTL_QKSORT(NDIM,N,ARR)
!###==================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: N,NDIM
REAL,INTENT(INOUT),DIMENSION(NDIM) :: ARR
INTEGER :: M,NSTACK
PARAMETER (M=7,NSTACK=50)
INTEGER :: I,IR,J,JSTACK,K,L,ISTACK(NSTACK)
REAL :: A,TEMP

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
2   ARR(I+1)=A
 ENDDO
 IF(JSTACK.EQ.0)RETURN
 IR=ISTACK(JSTACK)
 L=ISTACK(JSTACK-1)
 JSTACK=JSTACK-2
ELSE
 K=(L+IR)/2
 TEMP=ARR(K)
 ARR(K)=ARR(L+1)
 ARR(L+1)=TEMP
 IF(ARR(L+1).GT.ARR(IR))THEN
  TEMP=ARR(L+1)
  ARR(L+1)=ARR(IR)
  ARR(IR)=TEMP
 ENDIF
 IF(ARR(L).GT.ARR(IR))THEN
  TEMP=ARR(L)
  ARR(L)=ARR(IR)
  ARR(IR)=TEMP
 ENDIF
 IF(ARR(L+1).GT.ARR(L))THEN
  TEMP=ARR(L+1)
  ARR(L+1)=ARR(L)
  ARR(L)=TEMP
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
 TEMP=ARR(I)
 ARR(I)=ARR(J)
 ARR(J)=TEMP
 GOTO 3
5  ARR(L)=ARR(J)
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

END SUBROUTINE UTL_QKSORT

!====================================================
SUBROUTINE UTL_QKSORT2(ARR,BRR,NDIM,N)
!====================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: NDIM,N
REAL,INTENT(INOUT),DIMENSION(NDIM) :: ARR,BRR
INTEGER   M,NSTACK
PARAMETER (M=7,NSTACK=50)
INTEGER   I,IR,J,JSTACK,K,L,ISTACK(NSTACK)
REAL      A,B,ATEMP,BTEMP

JSTACK=0
L=1
IR=N
1 IF(IR-L.LT.M)THEN
 DO J=L+1,IR
  A=ARR(J)
  B=BRR(J)
  DO I=J-1,1,-1
   IF(ARR(I).LE.A)GOTO 2
   ARR(I+1)=ARR(I)
   BRR(I+1)=BRR(I)
  ENDDO
  I=0
2 ARR(I+1)=A
  BRR(I+1)=B
 ENDDO
 IF(JSTACK.EQ.0)RETURN
 IR=ISTACK(JSTACK)
 L =ISTACK(JSTACK-1)
 JSTACK=JSTACK-2
ELSE
 K=(L+IR)/2
 ATEMP   =ARR(K)
 BTEMP   =BRR(K)
 ARR(K)  =ARR(L+1)
 BRR(K)  =BRR(L+1)
 ARR(L+1)=ATEMP
 BRR(L+1)=BTEMP
 IF(ARR(L+1).GT.ARR(IR))THEN
  ATEMP   =ARR(L+1)
  BTEMP   =BRR(L+1)
  ARR(L+1)=ARR(IR)
  BRR(L+1)=BRR(IR)
  ARR(IR)=ATEMP
  BRR(IR)=BTEMP
 ENDIF
 IF(ARR(L).GT.ARR(IR))THEN
  ATEMP  =ARR(L)
  BTEMP  =BRR(L)
  ARR(L) =ARR(IR)
  BRR(L) =BRR(IR)
  ARR(IR)=ATEMP
  BRR(IR)=BTEMP
 ENDIF
 IF(ARR(L+1).GT.ARR(L))THEN
  ATEMP   =ARR(L+1)
  BTEMP   =BRR(L+1)
  ARR(L+1)=ARR(L)
  BRR(L+1)=BRR(L)
  ARR(L)  =ATEMP
  BRR(L)  =BTEMP
 ENDIF
 I=L+1
 J=IR
 A=ARR(L)
 B=BRR(L)
3  CONTINUE
 I=I+1
 IF(ARR(I).LT.A)GOTO 3
4  CONTINUE
 J=J-1
 IF(ARR(J).GT.A)GOTO 4
 IF(J.LT.I)GOTO 5
 ATEMP =ARR(I)
 BTEMP =BRR(I)
 ARR(I)=ARR(J)
 BRR(I)=BRR(J)
 ARR(J)=ATEMP
 BRR(J)=BTEMP
 GOTO 3
5  ARR(L)=ARR(J)
 BRR(L)=BRR(J)
 ARR(J)=A
 BRR(J)=B
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

END SUBROUTINE

!====================================================
SUBROUTINE UTL_QKSORT_INT(ARR,BRR,NDIM,N)
!====================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: NDIM,N
INTEGER,INTENT(INOUT),DIMENSION(NDIM) :: ARR
REAL,INTENT(INOUT),DIMENSION(NDIM) :: BRR
INTEGER,PARAMETER :: M=7,NSTACK=50
!PARAMETER (M=7,NSTACK=50)
INTEGER :: I,IR,J,JSTACK,K,L,ISTACK(NSTACK)
REAL :: B,BTEMP
INTEGER :: A,ATEMP

JSTACK=0
L=1
IR=N
1 IF(IR-L.LT.M)THEN
 DO J=L+1,IR
  A=ARR(J)
  B=BRR(J)
  DO I=J-1,1,-1
   IF(ARR(I).LE.A)GOTO 2
   ARR(I+1)=ARR(I)
   BRR(I+1)=BRR(I)
  ENDDO
  I=0
2 ARR(I+1)=A
  BRR(I+1)=B
 ENDDO
 IF(JSTACK.EQ.0)RETURN
 IR=ISTACK(JSTACK)
 L =ISTACK(JSTACK-1)
 JSTACK=JSTACK-2
ELSE
 K=(L+IR)/2
 ATEMP   =ARR(K)
 BTEMP   =BRR(K)
 ARR(K)  =ARR(L+1)
 BRR(K)  =BRR(L+1)
 ARR(L+1)=ATEMP
 BRR(L+1)=BTEMP
 IF(ARR(L+1).GT.ARR(IR))THEN
  ATEMP   =ARR(L+1)
  BTEMP   =BRR(L+1)
  ARR(L+1)=ARR(IR)
  BRR(L+1)=BRR(IR)
  ARR(IR)=ATEMP
  BRR(IR)=BTEMP
 ENDIF
 IF(ARR(L).GT.ARR(IR))THEN
  ATEMP  =ARR(L)
  BTEMP  =BRR(L)
  ARR(L) =ARR(IR)
  BRR(L) =BRR(IR)
  ARR(IR)=ATEMP
  BRR(IR)=BTEMP
 ENDIF
 IF(ARR(L+1).GT.ARR(L))THEN
  ATEMP   =ARR(L+1)
  BTEMP   =BRR(L+1)
  ARR(L+1)=ARR(L)
  BRR(L+1)=BRR(L)
  ARR(L)  =ATEMP
  BRR(L)  =BTEMP
 ENDIF
 I=L+1
 J=IR
 A=ARR(L)
 B=BRR(L)
3  CONTINUE
 I=I+1
 IF(ARR(I).LT.A)GOTO 3
4  CONTINUE
 J=J-1
 IF(ARR(J).GT.A)GOTO 4
 IF(J.LT.I)GOTO 5
 ATEMP =ARR(I)
 BTEMP =BRR(I)
 ARR(I)=ARR(J)
 BRR(I)=BRR(J)
 ARR(J)=ATEMP
 BRR(J)=BTEMP
 GOTO 3
5  ARR(L)=ARR(J)
 BRR(L)=BRR(J)
 ARR(J)=A
 BRR(J)=B
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

END SUBROUTINE

!====================================================
SUBROUTINE UTL_QKSORT_INT2(ARR,BRR,NDIM,N)
!====================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: NDIM,N
INTEGER,INTENT(INOUT),DIMENSION(NDIM) :: ARR
INTEGER,INTENT(INOUT),DIMENSION(NDIM) :: BRR
INTEGER,PARAMETER :: M=7,NSTACK=50
INTEGER :: I,IR,J,JSTACK,K,L,ISTACK(NSTACK)
INTEGER :: B,BTEMP
INTEGER :: A,ATEMP

JSTACK=0
L=1
IR=N
1 IF(IR-L.LT.M)THEN
 DO J=L+1,IR
  A=ARR(J)
  B=BRR(J)
  DO I=J-1,1,-1
   IF(ARR(I).LE.A)GOTO 2
   ARR(I+1)=ARR(I)
   BRR(I+1)=BRR(I)
  ENDDO
  I=0
2 ARR(I+1)=A
  BRR(I+1)=B
 ENDDO
 IF(JSTACK.EQ.0)RETURN
 IR=ISTACK(JSTACK)
 L =ISTACK(JSTACK-1)
 JSTACK=JSTACK-2
ELSE
 K=(L+IR)/2
 ATEMP   =ARR(K)
 BTEMP   =BRR(K)
 ARR(K)  =ARR(L+1)
 BRR(K)  =BRR(L+1)
 ARR(L+1)=ATEMP
 BRR(L+1)=BTEMP
 IF(ARR(L+1).GT.ARR(IR))THEN
  ATEMP   =ARR(L+1)
  BTEMP   =BRR(L+1)
  ARR(L+1)=ARR(IR)
  BRR(L+1)=BRR(IR)
  ARR(IR)=ATEMP
  BRR(IR)=BTEMP
 ENDIF
 IF(ARR(L).GT.ARR(IR))THEN
  ATEMP  =ARR(L)
  BTEMP  =BRR(L)
  ARR(L) =ARR(IR)
  BRR(L) =BRR(IR)
  ARR(IR)=ATEMP
  BRR(IR)=BTEMP
 ENDIF
 IF(ARR(L+1).GT.ARR(L))THEN
  ATEMP   =ARR(L+1)
  BTEMP   =BRR(L+1)
  ARR(L+1)=ARR(L)
  BRR(L+1)=BRR(L)
  ARR(L)  =ATEMP
  BRR(L)  =BTEMP
 ENDIF
 I=L+1
 J=IR
 A=ARR(L)
 B=BRR(L)
3  CONTINUE
 I=I+1
 IF(ARR(I).LT.A)GOTO 3
4  CONTINUE
 J=J-1
 IF(ARR(J).GT.A)GOTO 4
 IF(J.LT.I)GOTO 5
 ATEMP =ARR(I)
 BTEMP =BRR(I)
 ARR(I)=ARR(J)
 BRR(I)=BRR(J)
 ARR(J)=ATEMP
 BRR(J)=BTEMP
 GOTO 3
5  ARR(L)=ARR(J)
 BRR(L)=BRR(J)
 ARR(J)=A
 BRR(J)=B
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

END SUBROUTINE

!====================================================
SUBROUTINE QKSORTDOUBLE2(ARR,BRR,N)
!====================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: N
DOUBLE PRECISION,INTENT(INOUT),DIMENSION(N) :: ARR,BRR
INTEGER   M,NSTACK
PARAMETER (M=7,NSTACK=50)
INTEGER   I,IR,J,JSTACK,K,L,ISTACK(NSTACK)
DOUBLE PRECISION :: A,B,ATEMP,BTEMP

JSTACK=0
L=1
IR=N
1 IF(IR-L.LT.M)THEN
 DO J=L+1,IR
  A=ARR(J)
  B=BRR(J)
  DO I=J-1,1,-1
   IF(ARR(I).LE.A)GOTO 2
   ARR(I+1)=ARR(I)
   BRR(I+1)=BRR(I)
  ENDDO
  I=0
2 ARR(I+1)=A
  BRR(I+1)=B
 ENDDO
 IF(JSTACK.EQ.0)RETURN
 IR=ISTACK(JSTACK)
 L =ISTACK(JSTACK-1)
 JSTACK=JSTACK-2
ELSE
 K=(L+IR)/2
 ATEMP   =ARR(K)
 BTEMP   =BRR(K)
 ARR(K)  =ARR(L+1)
 BRR(K)  =BRR(L+1)
 ARR(L+1)=ATEMP
 BRR(L+1)=BTEMP
 IF(ARR(L+1).GT.ARR(IR))THEN
  ATEMP   =ARR(L+1)
  BTEMP   =BRR(L+1)
  ARR(L+1)=ARR(IR)
  BRR(L+1)=BRR(IR)
  ARR(IR)=ATEMP
  BRR(IR)=BTEMP
 ENDIF
 IF(ARR(L).GT.ARR(IR))THEN
  ATEMP  =ARR(L)
  BTEMP  =BRR(L)
  ARR(L) =ARR(IR)
  BRR(L) =BRR(IR)
  ARR(IR)=ATEMP
  BRR(IR)=BTEMP
 ENDIF
 IF(ARR(L+1).GT.ARR(L))THEN
  ATEMP   =ARR(L+1)
  BTEMP   =BRR(L+1)
  ARR(L+1)=ARR(L)
  BRR(L+1)=BRR(L)
  ARR(L)  =ATEMP
  BRR(L)  =BTEMP
 ENDIF
 I=L+1
 J=IR
 A=ARR(L)
 B=BRR(L)
3  CONTINUE
 I=I+1
 IF(ARR(I).LT.A)GOTO 3
4  CONTINUE
 J=J-1
 IF(ARR(J).GT.A)GOTO 4
 IF(J.LT.I)GOTO 5
 ATEMP =ARR(I)
 BTEMP =BRR(I)
 ARR(I)=ARR(J)
 BRR(I)=BRR(J)
 ARR(J)=ATEMP
 BRR(J)=BTEMP
 GOTO 3
5  ARR(L)=ARR(J)
 BRR(L)=BRR(J)
 ARR(J)=A
 BRR(J)=B
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

END SUBROUTINE

!###====================================================
SUBROUTINE SHELLSORT(N,A)
!###====================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: N
REAL,DIMENSION(N)  :: A
INTEGER :: I,J,INC
REAL :: V

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

RETURN
END SUBROUTINE

!###====================================================
SUBROUTINE SHELLSORTDOUBLE2(N,A,B)
!###====================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: N
DOUBLE PRECISION,DIMENSION(N)  :: A,B
INTEGER            :: I,J,INC
REAL               :: V,W

INC=1
1 INC=3*INC+1
IF(INC.LE.N)GOTO 1
 2 CONTINUE
 INC=INC/3
 DO I=INC+1,N
  V=A(I)
  W=B(I)
  J=I
  3 IF(A(J-INC).GT.V)THEN
   A(J)=A(J-INC)
   B(J)=B(J-INC)
   J=J-INC
  IF(J.LE.INC)GOTO 4
   GOTO 3
  ENDIF
  4  A(J)=V
  B(J)=W
END DO
IF(INC.GT.1)GOTO 2

RETURN
END SUBROUTINE

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

RETURN
END SUBROUTINE
