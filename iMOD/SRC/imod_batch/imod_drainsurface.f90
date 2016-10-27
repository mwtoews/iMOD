!!  Copyright (C) Stichting Deltares, 2005-2016.
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

MODULE MOD_DRNSURF

USE MODPLOT, ONLY : MPW
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDF_EXTENT,IDFREADSCALE,IDFCOPY,IDFWRITE
USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS,UTL_GETMED,UTL_CAP,UTL_IDFSNAPTOGRID

INTEGER :: I,NLGN
INTEGER,ALLOCATABLE,DIMENSION(:) :: ILGN
LOGICAL :: LEX
REAL :: CLB,CRITAHN,SFCT,CS
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF

CONTAINS

 !###======================================================================
 SUBROUTINE DRNSURF_MAIN()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IROW,ICOL
 
 INTEGER,DIMENSION(3) :: SCLTYPE_UP,SCLTYPE_DOWN
 DATA SCLTYPE_UP/2,7,7/
 DATA SCLTYPE_DOWN/2,2,2/
 
 DO I=1,3
  IF(.NOT.IDFREAD(IDF(I),IDF(I)%FNAME,0))RETURN
 ENDDO

 !## get minimal overlapping extent
 IF(MPW%XMAX-MPW%XMIN.EQ.0.0)THEN
  IF(.NOT.IDF_EXTENT(3,IDF,IDF(4),2))RETURN
 ELSE
  IDF(4)%XMIN=MPW%XMIN
  IDF(4)%XMAX=MPW%XMAX
  IDF(4)%YMIN=MPW%YMIN
  IDF(4)%YMAX=MPW%YMAX
  IDF(4)%DX=CS; IDF(4)%DY=CS
 ENDIF
 CALL UTL_IDFSNAPTOGRID(IDF(4)%XMIN,IDF(4)%XMAX,IDF(4)%YMIN,IDF(4)%YMAX,IDF(4)%DX,IDF(4)%NCOL,IDF(4)%NROW)

 !## read/scale files
 DO I=1,3
  WRITE(*,'(A)') 'Reading/scaling '//TRIM(IDF(I)%FNAME)//'...'
  CALL IDFCOPY(IDF(4),IDF(I))
  IF(.NOT.IDFREADSCALE(IDF(I)%FNAME,IDF(I),SCLTYPE_UP(I),SCLTYPE_DOWN(I),0.0,0))RETURN
 ENDDO
 DO IROW=1,IDF(4)%NROW; DO ICOL=1,IDF(4)%NCOL 
  DO I=1,3; IF(IDF(I)%X(ICOL,IROW).EQ.IDF(I)%NODATA)EXIT; ENDDO
  !## apply adjustments
  IF(I.LE.3)THEN; DO I=1,3; IDF(I)%X(ICOL,IROW)=IDF(I)%NODATA; ENDDO; ENDIF
 ENDDO; ENDDO

 CALL DRN_EL1RP()

 IF(.NOT.IDFWRITE(IDF(1),IDF(4)%FNAME,1))RETURN

 END SUBROUTINE DRNSURF_MAIN

 !###======================================================================
 SUBROUTINE DRN_EL1RP()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IROW,ICOL
 INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:) :: ISPEC
 INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:) :: Y
 INTEGER(KIND=2),ALLOCATABLE,DIMENSION(:,:) :: THREAD !,LGN
 INTEGER,ALLOCATABLE,DIMENSION(:) :: JC,JR
 REAL,ALLOCATABLE,DIMENSION(:) :: MV
 INTEGER :: JP,MP,NP,ILB,I,J,K,NN
 REAL :: RLB
 REAL,DIMENSION(1) :: DMV
 CHARACTER(LEN=256) :: LINE

 !## inital quess # point for one area
 MP=IDF(4)%NCOL+IDF(4)%NROW
 ALLOCATE(THREAD(2,MP),ISPEC(MP),Y(IDF(4)%NCOL,IDF(4)%NROW),JC(MP),JR(MP),MV(MP))

 Y=INT(0,1)

 DO IROW=1,IDF(4)%NROW
  DO ICOL=1,IDF(4)%NCOL
   !## found location
   IF(IDF(2)%X(ICOL,IROW).NE.IDF(2)%NODATA)THEN

    !## pointer (kadaster)
    JP=INT(IDF(2)%X(ICOL,IROW))

    DO
     !## trace connected kadaster-elements
     CALL DRN_EL1TRACE(IROW,ICOL,IDF(2)%X,Y,THREAD,ISPEC,JC,JR,IDF(2)%NROW,IDF(2)%NCOL,MP,IDF(2)%NODATA,NP)

     !## reset y-array
     DO I=1,ABS(NP)
      Y(JC(I),JR(I))=INT(0,1)
!      Y(ABS(JC(I)),ABS(JR(I)))=INT(0,1)
     END DO
     !## tracing finished correctly
     IF(NP.GT.0)EXIT
     !## reallocate memory
     MP=MP*2; DEALLOCATE(THREAD,ISPEC,JC,JR,MV)
     ALLOCATE(THREAD(2,MP),ISPEC(MP),JC(MP),JR(MP),MV(MP))
    ENDDO

    !## evaluate landuse within connected kadaster map
    ILB=0
    DO I=1,NP; DO J=1,NLGN
     IF(INT(IDF(3)%X(JC(I),JR(I))).EQ.ILGN(J))ILB=ILB+1
    ENDDO; ENDDO

    RLB=REAL(ILB*100)/REAL(NP)
! if(ILB.EQ.0)THEN
! PAUSE
! ENDIF
    !## enough amount of landbouw within kadaster
    IF(RLB.GT.CLB)THEN

     !## process area stored in jc/jr
     DO I=1,NP
      IF(INT(IDF(2)%X(JC(I),JR(I))).EQ.JP)THEN
       !## get areas with equal surfacelevel distribution within zone jp
       CALL DRN_EL2TRACE(IROW,ICOL,IDF(1)%X,IDF(2)%X,Y,THREAD,ISPEC,JC,JR,IDF(2)%NROW,IDF(2)%NCOL,MP,IDF(2)%NODATA,NP,CRITAHN)
       !## efficient to reset y-array directly, not necessary anymore
       DO J=1,NP
        Y(ABS(JC(J)),ABS(JR(J)))=INT(0,1)
       END DO

       !## get surface levels witin trace-area denoted by negative values
       K=0
       DO J=1,NP
        IF(JC(J).LT.0.AND.JR(J).LT.0)THEN
         K    =K+1
         MV(K)=IDF(1)%X(ABS(JC(J)),ABS(JR(J)))
        ENDIF
       END DO
       !## get median value of those
       CALL UTL_GETMED(MV,K,IDF(1)%NODATA,(/SFCT/),1,NN,DMV)

       !## turn off pointer not to be used again!
       DO J=1,NP
        IF(JC(J).LT.0.AND.JR(J).LT.0)THEN
         JC(J)=ABS(JC(J))
         JR(J)=ABS(JR(J))
         IDF(1)%X(JC(J),JR(J))=DMV(1)
         IDF(2)%X(JC(J),JR(J))=IDF(2)%NODATA
        ENDIF
       END DO

      ENDIF
     ENDDO

    ELSE
     !## clean results
     DO J=1,NP
      IDF(2)%X(JC(J),JR(J))=IDF(2)%NODATA
      IDF(1)%X(JC(J),JR(J))=IDF(1)%NODATA
     END DO
    ENDIF

   ENDIF
  END DO

  LINE='(memory '//TRIM(ITOS(MP))//') searching '
  WRITE(6,'(A,F10.3,A)') '+Progress '//TRIM(LINE),REAL(IROW*100)/REAL(IDF(4)%NROW),' %         '   

 ENDDO

 DEALLOCATE(THREAD,ISPEC,Y,JR,JC)

 END SUBROUTINE DRN_EL1RP

 !###======================================================================
 SUBROUTINE DRN_EL1TRACE(IROW,ICOL,IP,Y,THREAD,ISPEC,JC,JR,NROW,NCOL,MP,NODATA,NP)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: NP
 INTEGER,INTENT(IN) :: NCOL,NROW,MP,IROW,ICOL
 REAL,INTENT(IN) :: NODATA
 INTEGER(KIND=1),DIMENSION(MP) :: ISPEC
 INTEGER(KIND=1),DIMENSION(NCOL,NROW) :: Y
 INTEGER,DIMENSION(MP),INTENT(INOUT) :: JR,JC
 REAL,INTENT(INOUT),DIMENSION(NCOL,NROW) :: IP
 INTEGER(KIND=2),INTENT(INOUT),DIMENSION(2,MP) :: THREAD
 INTEGER :: NTHREAD,IR,IC,IDIR,JROW,JCOL

 !## copy current location
 JCOL=ICOL; JROW=IROW

 !## define first point in thread
 NTHREAD          =1
 THREAD(1,NTHREAD)=JCOL
 THREAD(2,NTHREAD)=JROW
 ISPEC(NTHREAD)   =0
 Y(JCOL,JROW)     =1

 NP=1; JR(NP)=JROW; JC(NP)=JCOL

 DO WHILE(NTHREAD.GT.0)

  !## get row/column number current location in thread
  JCOL=THREAD(1,NTHREAD); JROW=THREAD(2,NTHREAD)

  !## get direction and do not use this direction again!
  IDIR          =ISPEC(NTHREAD)+1
  ISPEC(NTHREAD)=IDIR
  CALL DRN_EL1GETDIR(JCOL,JROW,IR,IC,IDIR)

  !## possible direction found
  IF(IDIR.LE.8)THEN

   !## existing location in grid
   IF(IR.GE.1.AND.IR.LE.NROW.AND.IC.GE.1.AND.IC.LE.NCOL)THEN
    IF(Y(IC,IR).EQ.0.AND. &             !##not yet been there
       IP(IC,IR).NE.NODATA)THEN         !##correct location
     IF(IP(IC,IR).EQ.IP(JCOL,JROW))THEN !##equal to

      IF(NP+1.LE.MP)THEN
       NP    =NP+1
       JR(NP)=IR
       JC(NP)=IC
      ELSE
       NP=-1*NP
       RETURN
      ENDIF

      Y(IC,IR)         =1
      NTHREAD          =NTHREAD+1
      THREAD(1,NTHREAD)=IC
      THREAD(2,NTHREAD)=IR
      ISPEC(NTHREAD)   =0

     ENDIF
    ENDIF
   ENDIF
  ELSE
   !## no more places to go, move one step backwards in thread
   NTHREAD=NTHREAD-1
  ENDIF
 END DO

 END SUBROUTINE DRN_EL1TRACE

 !###======================================================================
 SUBROUTINE DRN_EL2TRACE(IROW,ICOL,AHN,IP,Y,THREAD,ISPEC,JC,JR,NROW,NCOL,MP,NODATA,NP,CRITAHN)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: NP
 INTEGER,INTENT(IN) :: NCOL,NROW,MP,IROW,ICOL
 REAL,INTENT(IN) :: NODATA,CRITAHN
 REAL,INTENT(INOUT),DIMENSION(NCOL,NROW) :: AHN
 INTEGER(KIND=1),DIMENSION(MP),INTENT(INOUT) :: ISPEC
 INTEGER(KIND=1),DIMENSION(NCOL,NROW),INTENT(INOUT) :: Y
 INTEGER,INTENT(INOUT),DIMENSION(MP) :: JR,JC
 REAL,INTENT(INOUT),DIMENSION(NCOL,NROW) :: IP
 INTEGER(KIND=2),DIMENSION(2,MP),INTENT(INOUT) :: THREAD
 INTEGER :: NTHREAD,IR,IC,IDIR,JROW,JCOL
 REAL :: AHNP

 !## copy current location
 JCOL=ICOL; JROW=IROW

 !## define first point in thread
 NTHREAD          =1
 THREAD(1,NTHREAD)=JCOL
 THREAD(2,NTHREAD)=JROW
 ISPEC(NTHREAD)   =0
 Y(JCOL,JROW)     =1
 AHNP             =AHN(JCOL,JROW)
 NP               =1
 JR(NP)           =-JROW
 JC(NP)           =-JCOL

 DO WHILE(NTHREAD.GT.0)

  !## get row/column number current location in thread
  JCOL=THREAD(1,NTHREAD); JROW=THREAD(2,NTHREAD)

  !## get direction and do not use this direction again!
  IDIR          =ISPEC(NTHREAD)+1
  ISPEC(NTHREAD)=IDIR
  CALL DRN_EL1GETDIR(JCOL,JROW,IR,IC,IDIR)

  !## possible direction found
  IF(IDIR.LE.8)THEN

   !## existing location in grid
   IF(IR.GE.1.AND.IR.LE.NROW.AND.IC.GE.1.AND.IC.LE.NCOL)THEN
    IF(Y(IC,IR).EQ.0.AND. &             !##not yet been there
       IP(IC,IR).NE.NODATA)THEN         !##correct location
     IF(IP(IC,IR).EQ.IP(JCOL,JROW).AND. &
        ABS(AHN(JCOL,JROW)-AHNP).LE.CRITAHN)THEN !##equal to

      NP    =NP+1
      JR(NP)=-IR
      JC(NP)=-IC

      Y(IC,IR)         =1
      NTHREAD          =NTHREAD+1
      THREAD(1,NTHREAD)=IC
      THREAD(2,NTHREAD)=IR
      ISPEC(NTHREAD)   =0

     ENDIF
    ENDIF
   ENDIF
  ELSE
   !## no more places to go, move one step backwards in thread
   NTHREAD=NTHREAD-1
  ENDIF
 END DO

 END SUBROUTINE DRN_EL2TRACE

 !###====================================================
 SUBROUTINE DRN_EL1GETDIR(JCOL,JROW,IR,IC,IDIR)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: JCOL,JROW
 INTEGER,INTENT(OUT) :: IC,IR
 INTEGER,INTENT(IN OUT) :: IDIR

 !## get new direction to search in
 IC=JCOL; IR=JROW
 SELECT CASE (IDIR)
  CASE (1)
   IR=IR-1
  CASE (2)
   IC=IC+1
   IR=IR-1
  CASE (3)
   IC=IC+1
  CASE (4)
   IC=IC+1
   IR=IR+1
  CASE (5)
   IR=IR+1
  CASE (6)
   IC=IC-1
   IR=IR+1
  CASE (7)
   IC=IC-1
  CASE (8)
   IC=IC-1
   IR=IR-1
 END SELECT

 END SUBROUTINE DRN_EL1GETDIR
 
END MODULE MOD_DRNSURF
