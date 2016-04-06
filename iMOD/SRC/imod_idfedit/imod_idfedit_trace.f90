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

MODULE MOD_IDFEDIT_TRACE

USE WINTERACTER
USE MOD_UTL, ONLY : ITOS
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFGETVAL

CONTAINS

 !###======================================================================
 SUBROUTINE IDFEDITTRACE(IDF,IDFP,THREAD,YSEL,ISPEC,DTERM,IMENU, &
                         MAXTHREAD,MAXN,XCRIT,NTHREAD,IPZ,JPZ,X2CRIT,&
                         THRESHOLD,PTRACE,ITRACE,STRACE,JTRACE,NTRACE,&
                         MINTHREAD,DZ,STOPVALUE)
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: TRATIO=1.0
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 TYPE(IDFOBJ),INTENT(INOUT) :: IDFP 
 TYPE(IDFOBJ),INTENT(INOUT),OPTIONAL :: PTRACE,STRACE
 TYPE(IDFOBJ),INTENT(INOUT),OPTIONAL :: THRESHOLD
 REAL,INTENT(IN) :: XCRIT
 REAL,INTENT(IN),OPTIONAL :: X2CRIT,STOPVALUE
 REAL,INTENT(INOUT),DIMENSION(:),OPTIONAL :: DZ
 INTEGER,INTENT(INOUT) :: NTHREAD,MAXTHREAD,MAXN
 INTEGER,INTENT(IN) :: DTERM,IMENU,IPZ
 INTEGER,INTENT(OUT),OPTIONAL :: JPZ,ITRACE,JTRACE
 INTEGER,INTENT(IN),OPTIONAL,DIMENSION(:) :: NTRACE
 INTEGER,INTENT(IN),OPTIONAL :: MINTHREAD
 INTEGER :: N,IR,IC,JR,JC,IDIR,JROW,JCOL,IREC,I,II,JJ,N1,N2,N3,M1,M2,MAXDIR
 INTEGER(KIND=1),POINTER,DIMENSION(:) :: ISPEC,ISPEC_BU 
 INTEGER(KIND=2),POINTER,DIMENSION(:,:) :: THREAD,YSEL,THREAD_BU,YSEL_BU
 REAL :: IDFV,ZCRIT,ZMAX,XTHRESHOLD
 LOGICAL :: LEX,LTHRESHOLD,LPTRACE,LSTOP
 
 MAXDIR=4; IF(DTERM.EQ.1.OR.DTERM.EQ.3)MAXDIR=8
 
 !## whenever trace reaches a value equal to stopvalue, it goes one step back in thread
 LSTOP=.FALSE.; IF(PRESENT(STOPVALUE))LSTOP=.TRUE.
 
 IF((DTERM.EQ.2.OR.DTERM.EQ.3))THEN
  IF(.NOT.PRESENT(JPZ))THEN; WRITE(*,*) 'ERROR IDFEDITTRACE'; STOP; ENDIF
 ENDIF
 
 LPTRACE=.FALSE.
 IF(PRESENT(PTRACE))THEN
  IF(.NOT.PRESENT(ITRACE))THEN;    WRITE(*,*) 'ERROR IDFEDITTRACE, ITRACE SHOULD BE KNOWN'; STOP; ENDIF
  IF(.NOT.PRESENT(JTRACE))THEN;    WRITE(*,*) 'ERROR IDFEDITTRACE, JTRACE SHOULD BE KNOWN'; STOP; ENDIF
  IF(.NOT.PRESENT(STRACE))THEN;    WRITE(*,*) 'ERROR IDFEDITTRACE, STRACE SHOULD BE KNOWN'; STOP; ENDIF
  IF(.NOT.PRESENT(NTRACE))THEN;    WRITE(*,*) 'ERROR IDFEDITTRACE, NTRACE SHOULD BE KNOWN'; STOP; ENDIF
  IF(.NOT.PRESENT(MINTHREAD))THEN; WRITE(*,*) 'ERROR MINTHREAD   , STRACE SHOULD BE KNOWN'; STOP; ENDIF
  LPTRACE=.TRUE.; ITRACE=0; JTRACE=0
 ENDIF
 
 ZCRIT=XCRIT
 LEX=.FALSE.; IF(ASSOCIATED(IDF%X))LEX=.TRUE.
 !## use threshold to continue in that direction that has a drop less than the threshold
 LTHRESHOLD=.FALSE.; IF(PRESENT(THRESHOLD))LTHRESHOLD=.TRUE.; XTHRESHOLD=0.0
  
 !## copy current location
 JCOL=YSEL(1,NTHREAD); JROW=YSEL(2,NTHREAD)
 
 !## annotate first point
 IF(DTERM.EQ.2.OR.DTERM.EQ.3)IDFP%X(JCOL,JROW)=REAL(IPZ)
 
 !## define first point in thread
 N=1; THREAD(1,N)=INT(JCOL,2); THREAD(2,N)=INT(JROW,2); ISPEC(N)=INT(0,1)

 NLOOP: DO WHILE(N.GT.0)

  !## get row/column number current location in thread
  JCOL =INT(THREAD(1,N)); JROW =INT(THREAD(2,N))
  !## zcrit according to last location 
  IF(IMENU.LT.0)THEN
   IF(LEX)THEN; ZCRIT=IDF%X(JCOL,JROW); ELSE; ZCRIT=IDFGETVAL(IDF,JROW,JCOL); ENDIF
  ENDIF
  !## use threshold for stepsize (thickness)
  IF(LTHRESHOLD)XTHRESHOLD=THRESHOLD%X(JCOL,JROW)
  
  !## get direction and do not use this direction again!
  IDIR    =ISPEC(N)+1
  CALL IDFEDITGETDIR(JCOL,JROW,IR,IC,IDIR,DTERM,YSEL,NTHREAD,MAXTHREAD,IPZ,IDF=IDF,IDFP=IDFP,DZ=DZ) 
  ISPEC(N)=INT(IDIR,1)

  !## possible direction found
  IF(IDIR.LE.MAXDIR)THEN

   !## existing location in grid
   IF(IR.GE.1.AND.IR.LE.IDF%NROW.AND.IC.GE.1.AND.IC.LE.IDF%NCOL)THEN

    !## get location in thread()
    IF(LGETY(YSEL,IDFP,NTHREAD,MAXTHREAD,IC,IR,DTERM))THEN

     !## get current value in grid
     IF(LEX)THEN; IDFV=IDF%X(IC,IR); ELSE; IDFV=IDFGETVAL(IDF,IR,IC); ENDIF

     !## appropriate location to go to?
     IF(LCORLOC(IDFV,ZCRIT,IDF%NODATA,ABS(IMENU),XTHRESHOLD,X2CRIT))THEN

      IF(NTHREAD+1.GT.MAXTHREAD)THEN
       M1=SIZE(YSEL,1); M2=SIZE(YSEL,2)
       MAXTHREAD=MIN(IDF%NROW*IDF%NCOL,2*MAXTHREAD)
       ALLOCATE(YSEL_BU(M1,MAXTHREAD))
       DO II=1,M1; DO JJ=1,M2; YSEL_BU(II,JJ)=YSEL(II,JJ); ENDDO; ENDDO
       DEALLOCATE(YSEL); YSEL=>YSEL_BU 
      ENDIF

      !## store selected cells
      NTHREAD        =NTHREAD+1
      YSEL(1,NTHREAD)=INT(IC,2)
      YSEL(2,NTHREAD)=INT(IR,2)
      !## store thread of visited cells
      N              =N+1
      
      IF(N+1.GT.MAXN)THEN
       N1=SIZE(THREAD,1); N2=SIZE(THREAD,2); N3=SIZE(ISPEC,1)
       MAXN=MIN(IDF%NROW*IDF%NCOL,2*MAXN)
       ALLOCATE(THREAD_BU(N1,MAXN),ISPEC_BU(MAXN))
       DO II=1,N1; DO JJ=1,N2; THREAD_BU(II,JJ)=THREAD(II,JJ); ENDDO; ENDDO
       DO II=1,N3; ISPEC_BU(II)=ISPEC(II); ENDDO
       DEALLOCATE(ISPEC,THREAD); THREAD=>THREAD_BU; ISPEC=>ISPEC_BU
      ENDIF

      THREAD(1,N)    =INT(IC,2)
      THREAD(2,N)    =INT(IR,2)
      ISPEC(N)       =INT(0,1)
          
      !## steepest descent method  
      IF(DTERM.EQ.2.OR.DTERM.EQ.3)THEN
       I=NTHREAD; JC=INT(YSEL(1,I)); JR=INT(YSEL(2,I)); ZMAX=IDF%X(JC,JR)
       DO I=NTHREAD,1,-1
        JC=INT(YSEL(1,I)); JR=INT(YSEL(2,I)); IF(IDF%X(JC,JR).GT.ZMAX)EXIT; IDF%X(JC,JR)=ZMAX
       ENDDO

       IF(LPTRACE)THEN
        IF(PTRACE%X(IC,IR).GT.0.0)THEN
         IF(NTHREAD.LT.MINTHREAD)THEN
          ITRACE=INT(PTRACE%X(IC,IR)); JTRACE=INT(STRACE%X(IC,IR)); NTHREAD=NTHREAD-1; RETURN 
         ELSE
          ITRACE=INT(PTRACE%X(IC,IR)); JTRACE=INT(STRACE%X(IC,IR))
          !## only whenever trapped trace is big, take it as well
          IF(NTRACE(JTRACE)-ITRACE.GT.10*NTHREAD)THEN; NTHREAD=NTHREAD-1; RETURN; ENDIF
          !## start a new branch of search, don't evaluate anymore
          ITRACE=0; JTRACE=0; 
          LPTRACE=.FALSE.
         ENDIF
        ENDIF
       ENDIF

       !## area already processed by previous thread(jz), stop tracing
       JPZ=INT(IDFP%X(IC,IR)); IF(JPZ.NE.IDFP%NODATA.AND.JPZ.NE.IPZ)RETURN
                   
      ENDIF
       
      !## stop criterium reached
      IF(LSTOP)THEN
       IF(IDFV.EQ.STOPVALUE)ISPEC(N)=MAXDIR+1
      ENDIF

      !## assign current ipz to thread(.)
      IDFP%X(IC,IR)=IPZ     
      
     ENDIF

    ELSE
     IF(DTERM.EQ.2.OR.DTERM.EQ.3)RETURN
    ENDIF
   ELSE
    !## return whenever steepest descent method is used
    IF(DTERM.EQ.2.OR.DTERM.EQ.3)RETURN
   ENDIF

  ELSE
   !## return whenever steepest descent method is used
   IF(DTERM.EQ.2.OR.DTERM.EQ.3)RETURN
   N=N-1   !## no more places to go, move one step backwards in thread
  ENDIF
 END DO NLOOP
 
 END SUBROUTINE IDFEDITTRACE

 !###====================================================
 SUBROUTINE IDFEDITGETDIR(JCOL,JROW,IR,IC,IDIR,DTERM,YSEL,NTHREAD,MAXTHREAD,IPZ,IDF,IDFP,DZ) 
 !###====================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN),OPTIONAL :: IDF,IDFP 
 REAL,DIMENSION(:),INTENT(OUT),OPTIONAL :: DZ
 INTEGER,INTENT(IN) :: JCOL,JROW,DTERM,MAXTHREAD,IPZ
 INTEGER,INTENT(INOUT) :: NTHREAD
 INTEGER,INTENT(OUT) :: IC,IR
 INTEGER,INTENT(INOUT) :: IDIR
 INTEGER(KIND=2),POINTER :: YSEL(:,:) 
 INTEGER :: I,IZ,JC,JR,IDZ,ITOT,ISPILL
 REAL :: MINZ,MAXZ,Z,MZ
 
 !## get new direction to search in
 IC=JCOL;IR=JROW
 IF(DTERM.EQ.0)THEN
  IF(IDIR.EQ.1)IR=IR-1 !## north
  IF(IDIR.EQ.2)IC=IC+1 !## east
  IF(IDIR.EQ.3)IR=IR+1 !## south
  IF(IDIR.EQ.4)IC=IC-1 !## west
 ELSEIF(DTERM.EQ.1)THEN
  SELECT CASE (IDIR)
   CASE (1); IR=IR-1
   CASE (2); IC=IC+1; IR=IR-1
   CASE (3); IC=IC+1
   CASE (4); IC=IC+1; IR=IR+1
   CASE (5); IR=IR+1
   CASE (6); IC=IC-1; IR=IR+1
   CASE (7); IC=IC-1
   CASE (8); IC=IC-1; IR=IR-1
  END SELECT
 !## steepest descent downwards
 ELSEIF(DTERM.EQ.2.OR.DTERM.EQ.3)THEN

  !## get least highest treshold around defined location
  CALL IDFEDIT_GETDZ(IDF%X(IC,IR),IDF,IDFP,IC,IR,DZ,IDZ,IPZ,YSEL,DTERM)
  IF(IDFEDIT_DETERMINEDIR(1,DZ,IC,IR))THEN
   ic=0; ir=0
  ENDIF  
     
!  !## get gradient for current point
!  CALL IDFEDIT_GETGRAD(IDF%X(JCOL,JROW),IDF,IDFP,JCOL,JROW,DZ,IDZ,IPZ,YSEL,DTERM)
!  !## steepest descent okay?
!  IF(IDFEDIT_DETERMINEDIR(-1,DZ,IC,IR))THEN
!   !## exit point found, stop!
!   ic=0; ir=0
!   RETURN
!  ENDIF
   
  !## no direction downwards anymore, if border, stop!
  IF(IC.EQ.1.OR.IC.EQ.IDF%NCOL.OR.IR.EQ.1.OR.IR.EQ.IDF%NROW)THEN; IC=0; RETURN; ENDIF

!RETURN
  !## pit location reached

  !## get least highest treshold around current location
  MINZ=10.0E10; IZ=0
  !## search for possible lower spill level further stream upwards till it meets the z-requirement
  DO I=NTHREAD,1,-1
   JC=YSEL(1,I); JR=YSEL(2,I); IF(IDF%X(JC,JR).GT.MINZ)EXIT
   CALL IDFEDIT_GETDZ(IDF%X(JC,JR),IDF,IDFP,JC,JR,DZ,IDZ,IPZ,YSEL,DTERM)
   MZ=MINVAL(DZ)

   !## ispill=1: spill level; ispill=0: dropdown
   ISPILL=1; IF(MZ.LT.0.0)ISPILL=0

   !## spill level
   Z=IDF%X(JC,JR)+MZ
   IF(Z.LT.MINZ)THEN 
    MINZ=Z; IZ=I
   ENDIF 
  ENDDO

!  !## adjust level again for lower spill-level
!  DO I=NTHREAD,IZ,-1
!   JC=YSEL(1,I); JR=YSEL(2,I); IDF%X(JC,JR)=MINZ
!  ENDDO
  
  !## restart somewhere else along the thread
  IF(IZ.GT.0)THEN
   IC=YSEL(1,IZ); IR=YSEL(2,IZ)
  ELSE
   IC=JCOL;IR=JROW
  ENDIF
  
  !## spill level
  IF(ISPILL.EQ.1)THEN

   !## get least highest treshold around defined location
   CALL IDFEDIT_GETDZ(IDF%X(IC,IR),IDF,IDFP,IC,IR,DZ,IDZ,IPZ,YSEL,DTERM)
   IF(IDFEDIT_DETERMINEDIR(1,DZ,IC,IR))THEN; ENDIF  

  !## drop down different location, use steepest descent
  ELSE

   !## stop
   ic=0; ir=0

!   !## get gradient for current point
!   CALL IDFEDIT_GETGRAD(IDF%X(IC,IR),IDF,IDFP,IC,IR,DZ,IDZ,IPZ,YSEL,DTERM)
!   !## steepest descent okay?
!   IF(IDFEDIT_DETERMINEDIR(-1,DZ,IC,IR))THEN
!    !## exit point found, stop!
!    RETURN
!   ENDIF
   
  ENDIF

 ENDIF

 END SUBROUTINE IDFEDITGETDIR

 !###======================================================================
 LOGICAL FUNCTION IDFEDIT_DETERMINEDIR(IUD,DZ,IC,IR) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IUD 
 INTEGER,INTENT(INOUT) :: IC,IR
 REAL,INTENT(INOUT),DIMENSION(:) :: DZ
 INTEGER :: IDIR,I
 REAL :: MINZ,MAXZ
        
 IDFEDIT_DETERMINEDIR=.FALSE.

 IF(IUD.EQ.-1)THEN

  !## steepest gradient downwards(-1)
  MAXZ=MAXVAL(DZ); DO IDIR=1,SIZE(DZ); IF(DZ(IDIR).EQ.MAXZ)EXIT; ENDDO
  !## need gradient downwards but not available, (all point upwards) so return
!  IF(MAXZ.LT.0.0)RETURN
  IF(MAXZ.LE.0.0)RETURN

 !## get lowest step upwards to get out of pit location
 ELSEIF(IUD.EQ.1)THEN

  !## least step upwards
  MINZ=MINVAL(DZ); DO IDIR=1,SIZE(DZ); IF(DZ(IDIR).EQ.MINZ)EXIT; ENDDO
  IF(MINZ.LT.0.0)RETURN
  
 ENDIF
 
 IF(IDIR.EQ.1)IR=IR-1 !## north
 IF(IDIR.EQ.2)IC=IC+1 !## east
 IF(IDIR.EQ.3)IR=IR+1 !## south
 IF(IDIR.EQ.4)IC=IC-1 !## west
 
 IF(SIZE(DZ).EQ.8)THEN
  IF(IDIR.EQ.5)THEN; IR=IR-1; IC=IC-1; ENDIF !## north-west
  IF(IDIR.EQ.6)THEN; IR=IR-1; IC=IC+1; ENDIF !## north-east
  IF(IDIR.EQ.7)THEN; IR=IR+1; IC=IC+1; ENDIF !## south-east
  IF(IDIR.EQ.8)THEN; IR=IR+1; IC=IC-1; ENDIF !## south-west
 ENDIF

 IDFEDIT_DETERMINEDIR=.TRUE.
 
 END FUNCTION IDFEDIT_DETERMINEDIR

 !###====================================================
 SUBROUTINE IDFEDIT_GETGRAD(Z,IDF,IDFP,JCOL,JROW,DZ,IDZ,IPZ,YSEL,DTERM)
 !###====================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: Z
 TYPE(IDFOBJ),INTENT(IN) :: IDF,IDFP
 INTEGER,INTENT(IN) :: JCOL,JROW,IPZ,DTERM
 REAL,DIMENSION(:),INTENT(OUT) :: DZ
 INTEGER,INTENT(OUT) :: IDZ
 INTEGER(KIND=2),POINTER :: YSEL(:,:) !2,MAXTHREAD)
 INTEGER :: I
 REAL :: DXY
 
 !## differences
 DZ(1)=Z-IDF%X(JCOL,MAX(1,JROW-1))       !## north
 DZ(2)=Z-IDF%X(MIN(JCOL+1,IDF%NCOL),JROW)!## east
 DZ(3)=Z-IDF%X(JCOL,MIN(IDF%NROW,JROW+1))!## south
 DZ(4)=Z-IDF%X(MAX(1,JCOL-1),JROW)       !## west
 !## gradient
 DZ(1)=DZ(1)/IDF%DY
 DZ(2)=DZ(2)/IDF%DX
 DZ(3)=DZ(3)/IDF%DY
 DZ(4)=DZ(4)/IDF%DX

 IF(DTERM.EQ.3)THEN
  DZ(5)=Z-IDF%X(MAX(1,JCOL-1),MAX(1,JROW-1))               !## north-west
  DZ(6)=Z-IDF%X(MIN(JCOL+1,IDF%NCOL),MAX(1,JROW-1))        !## north-east
  DZ(7)=Z-IDF%X(MIN(JCOL+1,IDF%NCOL),MIN(IDF%NROW,JROW+1)) !## south-east
  DZ(8)=Z-IDF%X(MAX(1,JCOL-1),MIN(IDF%NROW,JROW+1))        !## south-west
  DXY=SQRT(IDF%DX**2.0+IDF%DY**2.0)
  DZ(5)=DZ(5)/DXY
  DZ(6)=DZ(6)/DXY
  DZ(7)=DZ(7)/DXY
  DZ(8)=DZ(8)/DXY
 ENDIF
 
 !## check whether not yet visited, ipz
 IDZ=0
 IF(JROW.GT.1)        THEN; IF(IDFP%X(JCOL,JROW-1).EQ.IPZ)THEN; DZ(1)=-10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## north
 IF(JCOL.LT.IDFP%NCOL)THEN; IF(IDFP%X(JCOL+1,JROW).EQ.IPZ)THEN; DZ(2)=-10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## east
 IF(JROW.LT.IDFP%NROW)THEN; IF(IDFP%X(JCOL,JROW+1).EQ.IPZ)THEN; DZ(3)=-10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## south
 IF(JCOL.GT.1)        THEN; IF(IDFP%X(JCOL-1,JROW).EQ.IPZ)THEN; DZ(4)=-10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## west

 IF(DTERM.EQ.3)THEN
  IF(JROW.GT.1)THEN
   IF(JCOL.GT.1)        THEN; IF(IDFP%X(JCOL-1,JROW-1).EQ.IPZ)THEN; DZ(5)=-10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## north-west
   IF(JCOL.LT.IDFP%NCOL)THEN; IF(IDFP%X(JCOL+1,JROW-1).EQ.IPZ)THEN; DZ(6)=-10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## north-east
  ENDIF
  IF(JROW.LT.IDFP%NROW)THEN
   IF(JCOL.LT.IDFP%NCOL)THEN; IF(IDFP%X(JCOL+1,JROW+1).EQ.IPZ)THEN; DZ(7)=-10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## south-east
   IF(JCOL.GT.1)        THEN; IF(IDFP%X(JCOL-1,JROW+1).EQ.IPZ)THEN; DZ(8)=-10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## south-west
  ENDIF
 ENDIF
  
 END SUBROUTINE IDFEDIT_GETGRAD

 !###====================================================
 SUBROUTINE IDFEDIT_GETDZ(Z,IDF,IDFP,JCOL,JROW,DZ,IDZ,IPZ,YSEL,DTERM)
 !###====================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: Z
 TYPE(IDFOBJ),INTENT(IN) :: IDF,IDFP
 INTEGER,INTENT(IN) :: JCOL,JROW,IPZ,DTERM
 REAL,DIMENSION(:),INTENT(OUT) :: DZ
 INTEGER,INTENT(OUT) :: IDZ
 INTEGER(KIND=2),POINTER :: YSEL(:,:) 
 INTEGER :: I
 
 !## differences
 DZ(1)=IDF%X(JCOL,MAX(1,JROW-1))       -Z !## north
 DZ(2)=IDF%X(MIN(JCOL+1,IDF%NCOL),JROW)-Z !## east
 DZ(3)=IDF%X(JCOL,MIN(IDF%NROW,JROW+1))-Z !## south
 DZ(4)=IDF%X(MAX(1,JCOL-1),JROW)       -Z !## west

 IF(DTERM.EQ.3)THEN
  DZ(5)=IDF%X(MAX(1,JCOL-1),MAX(1,JROW-1))              -Z  !## north-west
  DZ(6)=IDF%X(MIN(JCOL+1,IDF%NCOL),MAX(1,JROW-1))       -Z  !## north-east
  DZ(7)=IDF%X(MIN(JCOL+1,IDF%NCOL),MIN(IDF%NROW,JROW+1))-Z  !## south-east
  DZ(8)=IDF%X(MAX(1,JCOL-1),MIN(IDF%NROW,JROW+1))       -Z  !## south-west
 ENDIF
 
 !## check whether not yet visited, ipz
 IDZ=0
 IF(JROW.GT.1)        THEN; IF(IDFP%X(JCOL,JROW-1).EQ.IPZ)THEN; DZ(1)=+10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## north
 IF(JCOL.LT.IDFP%NCOL)THEN; IF(IDFP%X(JCOL+1,JROW).EQ.IPZ)THEN; DZ(2)=+10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## east
 IF(JROW.LT.IDFP%NROW)THEN; IF(IDFP%X(JCOL,JROW+1).EQ.IPZ)THEN; DZ(3)=+10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## south
 IF(JCOL.GT.1)        THEN; IF(IDFP%X(JCOL-1,JROW).EQ.IPZ)THEN; DZ(4)=+10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## west

 IF(DTERM.EQ.3)THEN
  IF(JROW.GT.1)THEN
   IF(JCOL.GT.1)        THEN; IF(IDFP%X(JCOL-1,JROW-1).EQ.IPZ)THEN; DZ(5)=+10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## north-west
   IF(JCOL.LT.IDFP%NCOL)THEN; IF(IDFP%X(JCOL+1,JROW-1).EQ.IPZ)THEN; DZ(6)=+10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## north-east
  ENDIF
  IF(JROW.LT.IDFP%NROW)THEN
   IF(JCOL.LT.IDFP%NCOL)THEN; IF(IDFP%X(JCOL+1,JROW+1).EQ.IPZ)THEN; DZ(7)=+10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## south-east
   IF(JCOL.GT.1)        THEN; IF(IDFP%X(JCOL-1,JROW+1).EQ.IPZ)THEN; DZ(8)=+10.0E14; IDZ=IDZ+1; ENDIF; ENDIF  !## south-west
  ENDIF
 ENDIF

 END SUBROUTINE IDFEDIT_GETDZ

 !###======================================================================
 LOGICAL FUNCTION LCORLOC(IDFVAL,ZCRIT,NODATA,IMENU,XTHRESHOLD,Z2CRIT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMENU
 REAL,INTENT(IN) :: ZCRIT,IDFVAL,NODATA,XTHRESHOLD
 REAL,INTENT(IN),OPTIONAL :: Z2CRIT
 
 LCORLOC=.FALSE.

 IF(IDFVAL.EQ.NODATA)RETURN

 SELECT CASE (IMENU)
  CASE (1) !## equal
   IF(IDFVAL.EQ.ZCRIT)LCORLOC=.TRUE.
  CASE (2) !## less than
   IF(IDFVAL.LT.ZCRIT)LCORLOC=.TRUE.
   IF(PRESENT(Z2CRIT))THEN
    IF(LCORLOC.AND.IDFVAL.LT.Z2CRIT)LCORLOC=.FALSE.
   ENDIF
  CASE (3) !## less or equal
   IF(IDFVAL.LE.ZCRIT)LCORLOC=.TRUE.
   IF(PRESENT(Z2CRIT))THEN
    IF(LCORLOC.AND.IDFVAL.LE.Z2CRIT)LCORLOC=.FALSE.
   ENDIF
  CASE (4) !## greater than
   IF(IDFVAL.GT.ZCRIT)LCORLOC=.TRUE.
   IF(PRESENT(Z2CRIT))THEN
    IF(LCORLOC.AND.IDFVAL.GT.Z2CRIT)LCORLOC=.FALSE.
   ENDIF
  CASE (5) !## greater than or equal
   IF(IDFVAL.GE.ZCRIT)LCORLOC=.TRUE.
   IF(PRESENT(Z2CRIT))THEN
    IF(LCORLOC.AND.IDFVAL.GE.Z2CRIT)LCORLOC=.FALSE.
   ENDIF
  CASE (6) !## ne
   IF(IDFVAL.NE.ZCRIT)LCORLOC=.TRUE.
  CASE (7) !## never mind
   LCORLOC=.TRUE.
 END SELECT
 
 IF(XTHRESHOLD.LE.0.0)RETURN
 
 !## do not go there whenever difference of step is larger than threshold
 IF(ABS(IDFVAL-ZCRIT).GT.XTHRESHOLD)LCORLOC=.FALSE.
 
 END FUNCTION LCORLOC

 !###======================================================================
 LOGICAL FUNCTION LGETY(YSEL,IDFP,NTHREAD,MAXTHREAD,IC,IR,DTERM)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDFP
 INTEGER,INTENT(IN) :: NTHREAD,MAXTHREAD,IC,IR,DTERM
 INTEGER(KIND=2) :: YSEL(2,MAXTHREAD)
 INTEGER :: I

 IF(DTERM.EQ.2.OR.DTERM.EQ.3)THEN
  LGETY=.FALSE. 
  DO I=1,NTHREAD
   IF(YSEL(1,I).EQ.IC.AND.YSEL(2,I).EQ.IR)RETURN
  END DO
  LGETY=.TRUE.
 ENDIF
 
 !## not yet visited
 LGETY=IDFP%X(IC,IR).EQ.IDFP%NODATA

 END FUNCTION LGETY

END MODULE MOD_IDFEDIT_TRACE
