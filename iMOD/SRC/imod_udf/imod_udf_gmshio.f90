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
!!
MODULE MESHMOD

 USE WINTERACTER
 USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS
 USE KDTREE2_PRECISION_MODULE
 USE KDTREE2_MODULE
 USE IMODVAR, ONLY : DP_KIND,SP_KIND

 IMPLICIT NONE

 INTEGER, PARAMETER :: MXLEN = 1024
 INTEGER, PARAMETER :: NDIM = 2

 !## elements 
 TYPE TELEM
   LOGICAL :: LOUTPUT = .FALSE.
   INTEGER :: TYP = 0
   INTEGER :: N = 0
   INTEGER, DIMENSION(:), POINTER :: NUM => NULL() 
   INTEGER :: NTAG = 0
   INTEGER, DIMENSION(:), POINTER :: TAG
   INTEGER, DIMENSION(:), POINTER :: ITAG
 END TYPE TELEM

 !## tags
 TYPE TTAG
   INTEGER :: DIM = 0
   INTEGER :: NUM = 0
   CHARACTER(LEN=MXLEN) :: NAME = ''
 END TYPE TTAG
 
 TYPE TNODE2ELEM
   LOGICAL :: LOUTPUT = .FALSE.  
   INTEGER :: N = 0
   INTEGER, DIMENSION(:), POINTER :: NUM => NULL()
 END TYPE TNODE2ELEM      
      
 TYPE TMESH
   ! NAME 
   CHARACTER(LEN=MXLEN) :: FNAME = ''  
   ! NODES  
   INTEGER :: NNOD = 0
   REAL(KDKIND), DIMENSION(:,:), POINTER :: NODE => NULL()
   ! NODE --> ELEMENT
   TYPE(TNODE2ELEM), DIMENSION(:), POINTER :: NODE2ELEM => NULL()    
   ! ELEMENTS  
   INTEGER :: NELEM = 0
   TYPE(TELEM), DIMENSION(:), POINTER :: ELEM => NULL()    
   ! TAGS
   INTEGER :: NTAG = 0
   INTEGER :: MINTAG =  99999
   INTEGER :: MAXTAG = -99999
   TYPE(TTAG), DIMENSION(:), POINTER :: TAG => NULL()
   ! BOUNDING BOX
   REAL(KDKIND) :: XMIN
   REAL(KDKIND) :: XMAX
   REAL(KDKIND) :: YMIN
   REAL(KDKIND) :: YMAX
 END TYPE TMESH    
 
 TYPE TMESHARRAY
   TYPE(TMESH), POINTER :: MESH => NULL()
 END TYPE TMESHARRAY  
 
 SAVE

CONTAINS

 !##===========================================================
 LOGICAL FUNCTION READGMSH(MESH,FNAME,IREAD)
 !##===========================================================
 IMPLICIT NONE
 TYPE(TMESH),POINTER,INTENT(OUT) :: MESH
 INTEGER,INTENT(IN) :: IREAD
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: LUN,I,J,K,N,IOS,MT
 REAL(KIND=DP_KIND) :: X1,Y1
 TYPE(TELEM),DIMENSION(:),POINTER :: ELEM
 TYPE(TTAG),DIMENSION(:),POINTER :: TAG
 CHARACTER(LEN=7) :: FORM
 INTEGER,DIMENSION(15) :: NT
 DATA NT/2,3,4,0,0,0,0,0,0,0,0,0,0,0,1/
 
 READGMSH=.FALSE.
 
 MT=MAXVAL(NT)
 
 WRITE(FORM,'(A1,I4.4,A2)') '(',MXLEN,'A)'

 ALLOCATE(MESH)    
 MESH%FNAME = TRIM(FNAME)

 LUN=UTL_GETUNIT(); OPEN(LUN,FILE=FNAME,FORM='FORMATTED',ACTION='READ',IOSTAT=IOS) 
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot read file '//TRIM(FNAME),'Error')
  LUN=0; RETURN
 ENDIF

 !## read header
 DO I=1,4; READ(LUN,*); END DO
 
 READ(LUN,*) MESH%NTAG
 ALLOCATE(MESH%TAG(MESH%NTAG)); TAG=>MESH%TAG

 DO I=1,MESH%NTAG
  READ(LUN,*) TAG(I)%DIM,TAG(I)%NUM,TAG(I)%NAME     
  MESH%MINTAG = MIN(MESH%MINTAG,TAG(I)%NUM)
  MESH%MAXTAG = MAX(MESH%MAXTAG,TAG(I)%NUM)
 END DO
 IF(MESH%MINTAG.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Tag numbers <= not allowed','Error')
  CLOSE(LUN); LUN=0; RETURN
 ENDIF
 READ(LUN,*)
 READ(LUN,*)
 READ(LUN,*) MESH%NNOD
 ALLOCATE(MESH%NODE(NDIM,MESH%NNOD))
 
! MESH%XMIN= HUGE(1.0D0)
! MESH%YMIN= HUGE(1.0D0)
! MESH%XMAX=-HUGE(1.0D0)
! MESH%YMAX=-HUGE(1.0D0)
 DO I = 1, MESH%NNOD
!  READ(LUN,'(A)') S
!  READ(S,*) J, X1, Y1
  READ(LUN,*) J,X1,Y1
  MESH%NODE(1,J) = X1
  MESH%NODE(2,J) = Y1
!  MESH%XMIN = MIN(MESH%XMIN,X1)  
!  MESH%XMAX = MAX(MESH%XMAX,X1)  
!  MESH%YMIN = MIN(MESH%YMIN,Y1)  
!  MESH%YMAX = MAX(MESH%YMAX,Y1)  
 END DO
 READ(LUN,*)
 READ(LUN,*)
 READ(LUN,*) MESH%NELEM
 
! !## read coordinates only
! IF(IREAD.EQ.1)THEN; CLOSE(LUN); READGMSH=.TRUE.; RETURN; ENDIF
 
 ALLOCATE(MESH%ELEM(MESH%NELEM)); ELEM=>MESH%ELEM

 DO I=1,MESH%NELEM
!  READ(LUN,FORM) S !'(A)') S  

  ALLOCATE(ELEM(I)%NUM(MT))
  N=MAX(MESH%NTAG,2);   ALLOCATE(ELEM(I)%TAG(N))
  N=MAX(MESH%MAXTAG,1); ALLOCATE(ELEM(I)%ITAG(N))
  ELEM(I)%ITAG = 0

  READ(LUN,*) J, ELEM(J)%TYP, ELEM(J)%NTAG, (ELEM(J)%TAG(K),K=1,ELEM(J)%NTAG), (ELEM(J)%NUM(K),K=1,NT(ELEM(J)%TYP)) 

  !## set number of coordinates
  ELEM(J)%N=NT(ELEM(J)%TYP)
  
!  SELECT CASE(ELEM(J)%TYP)
!   CASE(15) !## point
!    ELEM(J)%N = 1
!   CASE(1) !## line  
!    ELEM(J)%N = 2
!   CASE(2) !## triangle
!    ELEM(J)%N = 3
!   CASE(3) !## quad
!    ELEM(J)%N = 4
!   CASE DEFAULT
!    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error, wrong element type '//TRIM(ITOS(ELEM(J)%TYP)),'Error')
!    CLOSE(LUN); LUN=0; RETURN
!  END SELECT    

!  ALLOCATE(ELEM(J)%NUM(ELEM(J)%N))  
!  READ(S,*) IDUM, IDUM, IDUM, (ELEM(J)%TAG(K),K=1,ELEM(J)%NTAG), (ELEM(J)%NUM(K),K=1,ELEM(J)%N) 
  !## ignore the last tag
  ELEM(J)%NTAG=ELEM(J)%NTAG-1
  DO K=1,ELEM(J)%NTAG
   ELEM(J)%ITAG(ELEM(J)%TAG(K)) = 1    
  END DO

 END DO
 
 !## close gmsh file
 CLOSE(LUN)  
    
 !## node 2 elem - for kdtree necessary
 IF(IREAD.EQ.2)CALL NODE2ELEM(MESH)
    
 READGMSH=.TRUE.

 END FUNCTION READGMSH

 !##===========================================================
 SUBROUTINE NODE2ELEM(MESH)
 !##===========================================================
 TYPE(TMESH),POINTER,INTENT(INOUT) :: MESH
 INTEGER :: I,J,K,N
 INTEGER,DIMENSION(:),ALLOCATABLE :: IWRK 
        
 !## node --> element mapping
 ALLOCATE(IWRK(MESH%NNOD))
 ALLOCATE(MESH%NODE2ELEM(MESH%NNOD))
 IWRK = 0
 DO I = 1, MESH%NELEM
  DO J = 1, MESH%ELEM(I)%N
   K = MESH%ELEM(I)%NUM(J)
   IWRK(K) = IWRK(K) + 1
  END DO
 END DO
 DO I = 1, MESH%NNOD
  J = IWRK(I)  
  IF (J > 0)THEN   
   ALLOCATE(MESH%NODE2ELEM(I)%NUM(J))
  END IF  
 END DO
 DO I = 1, MESH%NELEM
  DO J = 1, MESH%ELEM(I)%N
   K = MESH%ELEM(I)%NUM(J)
   N = MESH%NODE2ELEM(K)%N
   N = N + 1
   MESH%NODE2ELEM(K)%NUM(N) = I
   MESH%NODE2ELEM(K)%N = N
  END DO
 END DO
 
 DEALLOCATE(IWRK)

 END SUBROUTINE NODE2ELEM
  
 !##===========================================================
 SUBROUTINE WRITEGMSH(MESH,FNAME,TAGNUM)
 !##===========================================================
 IMPLICIT NONE
 TYPE(TMESH), POINTER, INTENT(IN) :: MESH
 CHARACTER(LEN=*), INTENT(IN) :: FNAME 
 INTEGER, INTENT(IN), OPTIONAL :: TAGNUM
 CHARACTER(LEN=MXLEN) :: S  
 CHARACTER(LEN=MXLEN), DIMENSION(10) :: SA  
 INTEGER :: LUN, I, J, K, N, NNOD, NELEM
 LOGICAL :: ALLTAGS, LFOUND
  
    WRITE(*,*) 'WRITING ',TRIM(FNAME),'...'
    
 ALLTAGS = .TRUE.
 IF (PRESENT(TAGNUM)) ALLTAGS = .FALSE.
 IF (.NOT.ALLTAGS) THEN
  DO I = 1, MESH%NNOD
   MESH%NODE2ELEM(I)%LOUTPUT = .FALSE.  
  END DO  
  DO I = 1, MESH%NELEM
   MESH%ELEM(I)%LOUTPUT = .FALSE.  
  END DO  
 END IF

 LUN=UTL_GETUNIT()  
 OPEN(LUN,FILE=FNAME) 

 WRITE(LUN,'(A)') "$MESHFORMAT"
 WRITE(LUN,'(A)') "2.2 0 8"
 WRITE(LUN,'(A)') "$ENDMESHFORMAT"
 WRITE(LUN,'(A)') "$PHYSICALNAMES"
 IF (.NOT.ALLTAGS) THEN
  WRITE(S,*) 1 
 ELSE      
  WRITE(S,*) MESH%NTAG 
 END IF  
 WRITE(LUN,'(A)') TRIM(ADJUSTL(S))
 DO I = 1, MESH%NTAG
  WRITE(SA(1),*) MESH%TAG(I)%DIM 
  WRITE(SA(2),*) MESH%TAG(I)%NUM 
  WRITE(SA(3),*) '"'//TRIM(MESH%TAG(I)%NAME)//'"' 
  CALL MAKESTR(SA(1:3),S)
  IF (.NOT.ALLTAGS) THEN
   IF (MESH%TAG(I)%NUM == TAGNUM) THEN
    WRITE(LUN,'(A)') TRIM(S)
   END IF          
  ELSE      
   WRITE(LUN,'(A)') TRIM(S)
  END IF  
 END DO    
 WRITE(LUN,'(A)') "$ENDPHYSICALNAMES"
 WRITE(LUN,'(A)') "$NODES"
 IF (.NOT.ALLTAGS) THEN
  NNOD = 0
  DO I = 1, MESH%NNOD
   LFOUND = .FALSE.  
   DO J = 1, MESH%NODE2ELEM(I)%N    
    K = MESH%NODE2ELEM(I)%NUM(J) ! ELEMEN
    DO N = 1, MESH%ELEM(K)%NTAG
     IF (MESH%ELEM(K)%TAG(N) == TAGNUM) THEN
      LFOUND = .TRUE. 
      MESH%ELEM(K)%LOUTPUT = .TRUE.
     END IF   
    END DO  
   END DO
   IF (LFOUND) THEN
    MESH%NODE2ELEM(I)%LOUTPUT = .TRUE.
    NNOD = NNOD + 1
   END IF   
  END DO
 ELSE
  NNOD = MESH%NNOD
 END IF  
 WRITE(S,*) NNOD 
 WRITE(LUN,'(A)') TRIM(ADJUSTL(S))
 DO I = 1, MESH%NNOD
  IF (.NOT.ALLTAGS) THEN
   IF (.NOT.MESH%NODE2ELEM(I)%LOUTPUT) CYCLE
  END IF    
  WRITE(SA(1),*) I
  WRITE(SA(2),*) MESH%NODE(1,I)
  WRITE(SA(3),*) MESH%NODE(2,I)
  WRITE(SA(4),*) 0
  CALL MAKESTR(SA(1:4),S)
  WRITE(LUN,'(A)') TRIM(S)
 END DO
 WRITE(LUN,'(A)') "$ENDNODES"
 WRITE(LUN,'(A)') "$ELEMENTS"
 IF (.NOT.ALLTAGS) THEN
  NELEM = 0
  DO I = 1, MESH%NELEM
   IF (MESH%ELEM(I)%LOUTPUT) NELEM = NELEM + 1   
  END DO  
 ELSE
  NELEM = MESH%NELEM 
 END IF
 WRITE(S,*) NELEM
 WRITE(LUN,'(A)') TRIM(ADJUSTL(S))
 DO I = 1, MESH%NELEM 
  IF (.NOT.ALLTAGS) THEN
   IF (.NOT.MESH%ELEM(I)%LOUTPUT) CYCLE
  END IF    
  N = 1; WRITE(SA(N),*) I    
  N = N + 1; WRITE(SA(N),*) MESH%ELEM(I)%TYP 
  N = N + 1; WRITE(SA(N),*) MESH%ELEM(I)%NTAG
  DO J = 1, MESH%ELEM(I)%NTAG
   N = N + 1; WRITE(SA(N),*) MESH%ELEM(I)%TAG(J) 
  END DO
  DO J = 1, MESH%ELEM(I)%N
   N = N + 1; WRITE(SA(N),*) MESH%ELEM(I)%NUM(J) 
  END DO  
  CALL MAKESTR(SA(1:N),S)
  WRITE(LUN,'(A)') TRIM(S)
 END DO
 WRITE(LUN,'(A)') "$ENDELEMENTS"
 CLOSE(LUN)

 END SUBROUTINE WRITEGMSH
 
 !##===========================================================
 SUBROUTINE KDTREE_REMOVE_DOUBLES(XY,NXY)
 !##===========================================================
 IMPLICIT NONE
!    USE KDTREE2_MODULE
 INTEGER, INTENT(INOUT) :: NXY
 REAL(KIND=DP_KIND), DIMENSION(2,NXY), INTENT(INOUT) :: XY
 INTEGER, PARAMETER :: NS = 4
 REAL(KIND=DP_KIND), PARAMETER :: EPS = 1E-20
 TYPE(KDTREE2), POINTER :: WRKTREE
 REAL(KDKIND), DIMENSION(:,:), ALLOCATABLE :: RWRK  
 INTEGER, DIMENSION(:), ALLOCATABLE :: IWRK
 TYPE(KDTREE2_RESULT), ALLOCATABLE :: RESULTS(:)
 INTEGER, DIMENSION(:), ALLOCATABLE :: IFND
 INTEGER :: I, J, IDX, MXY, N
 LOGICAL :: JFIRST

 ALLOCATE(RWRK(2,NXY))
 DO I = 1, NXY
  RWRK(1,I) = XY(1,I)    
  RWRK(2,I) = XY(2,I)    
 END DO
 WRKTREE => KDTREE2_CREATE(RWRK,SORT=.TRUE.,REARRANGE=.FALSE.)  ! THIS IS HOW YOU CREATE A TREE. 
    
 ! REMOVE DUPLICATES
 ALLOCATE(IWRK(NXY)); IWRK = 1
 ALLOCATE(RESULTS(NS),IFND(NS)) 
 DO I = 1, NXY
  CALL KDTREE2_N_NEAREST(TP=WRKTREE,QV=RWRK(:,I),NN=NS,RESULTS=RESULTS) 
  ! DOUBLE
  IFND = 0; JFIRST = 0
  DO J = 1, NS
   IF (RESULTS(J)%DIS < EPS) THEN
    IF (JFIRST.EQ.0) JFIRST = J
    IDX = RESULTS(J)%IDX 
    IFND(J) = 1         
   END IF   
  END DO
  IF (SUM(IFND).GT.1) THEN ! HANDLE DOUBLE
   IDX = RESULTS(JFIRST)%IDX ! FIRST FOUND
   IF (IWRK(IDX).EQ.1) THEN
    DO J = 1, NS
     IF (IFND(J).EQ.1) THEN 
      IF (J.NE.JFIRST) THEN
       IDX = RESULTS(J)%IDX
       IWRK(IDX) = 0
      END IF   
     END IF        
    END DO     
   END IF    
  END IF
 END DO
 ! NUMBER OF UNIQUE COORDINATES
 MXY = SUM(IWRK)    
      
 N = 0
 DO I = 1, NXY
  IF (IWRK(I) == 1) THEN
   N = N + 1
   XY(1,N) = RWRK(1,I); XY(2,N) = RWRK(2,I)
  END IF    
 END DO
 NXY = MXY
 DEALLOCATE(IWRK,RWRK)
 CALL KDTREE2_DESTROY(WRKTREE) ! DETROY TREE  

 END SUBROUTINE KDTREE_REMOVE_DOUBLES 

 !##===========================================================
 SUBROUTINE MAKESTR(SA,S)
 !##===========================================================
 IMPLICIT NONE
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: SA
 CHARACTER(LEN=*), INTENT(OUT) :: S
 CHARACTER(LEN=MXLEN) :: FMT
 INTEGER :: N, J

 N = SIZE(SA)
 ! (N-1(A,1X),A)
 WRITE(FMT,'(A,I,A)') '(',N-1,'(A,1X),A)'   
 WRITE(S,FMT) ( TRIM(ADJUSTL(SA(J))),J=1,N )

 END SUBROUTINE MAKESTR
  
END MODULE MESHMOD
