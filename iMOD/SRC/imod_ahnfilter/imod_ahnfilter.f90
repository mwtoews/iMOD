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

MODULE MOD_AHNFILTER

USE WINTERACTER
USE MOD_AHNFILTER_PAR
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFOPEN,IDFNULLIFY,IDFDEALLOCATE,IDFREADPART,IDFDEALLOCATEX,IDFALLOCATEX, &
      IDFWRITEPART,IDFWRITEDIM,IDFCOPY,IDFWRITE,IDFPART
USE MOD_UTL, ONLY : UTL_IDFSNAPTOGRID,UTL_GETUNIT,ITOS,UTL_GETMED
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_IDFEDIT_TRACE, ONLY : IDFEDITGETDIR !,IDFEDITTRACE
USE MOD_SOLID_PCG, ONLY : SOLID_PCGINT,HCLOSE

TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),PRIVATE :: IDFAHN
TYPE(IDFOBJ),PRIVATE :: IDFX  !## original data
TYPE(IDFOBJ),PRIVATE :: IDFY  !## result data
INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:),PRIVATE :: Y   !## pointer to monitor previous position(s)
REAL,ALLOCATABLE,DIMENSION(:),PRIVATE :: XP   !## percentile window (local upconing)
INTEGER(KIND=1),POINTER,DIMENSION(:),PRIVATE :: ISPEC
INTEGER(KIND=2),POINTER,DIMENSION(:,:),PRIVATE :: THREAD,ISCRIT !,YSEL

REAL,PRIVATE :: DX,DY
REAL,PARAMETER :: NODATA=-9999.99  
INTEGER,PARAMETER :: IUPC    =-3 !upconing
INTEGER,PARAMETER :: IDEP    =-2 !depression
INTEGER,PARAMETER :: INODATA =-1 !nodata
INTEGER,PARAMETER :: IINI    = 0 !initial
INTEGER,PARAMETER :: IPOTSURF= 1 !potential surface
INTEGER,PARAMETER :: ISURF   = 2 !surface

CONTAINS

 !###====================================================
 LOGICAL FUNCTION AHNFILTER_MAIN(IFILTER)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFILTER
 INTEGER :: N 
 
 AHNFILTER_MAIN=.FALSE.

 !## allocate idf obj
 CALL AHNFILTER_ALLOC()
 !## read idf files
 IF(.NOT.AHNFILTER_INIT())RETURN

 !## allocate memory for current part
 IF(AHNFILTER_ALLOC_DATA())THEN
  !## read data
  IF(AHNFILTER_READDATA())THEN
   !## filter ahn (get depressions/upconings/flat areas)
   CALL AHNFILTER_SURFACE1MAIN(IFILTER) 

   IF(.NOT.IDFWRITE(IDFY,IDFY%FNAME(:INDEX(IDFY%FNAME,'\',.TRUE.))//'y'//TRIM(ITOS(IFILTER))//'.idf',1))then
   ENDIF

   !## put initial values
   CALL AHNFILTER_INT_INITIAL()
   DO
    CALL AHNFILTER_INT()
    CALL AHNFILTER_INT_COR(N)
    IF(N.LE.CORCRIT)EXIT  !## nothing remaining
   ENDDO  

   IF(BUFFER.GT.0.0)THEN
    IF(.NOT.IDFPART(IDFY,IDFX%XMIN+BUFFER,IDFX%YMIN+BUFFER,IDFX%XMAX-BUFFER,IDFX%YMAX-BUFFER))RETURN
   ENDIF
   !## write results first idf inside second idf (exclude buffer?)
   IDFY%NODATA=IDFAHN(1)%NODATA; IF(IDFWRITE(IDFY,IDFY%FNAME,1))THEN
   ENDIF
   AHNFILTER_MAIN=.TRUE. !.TRUE. 
  ENDIF
 ENDIF
 
 CALL AHNFILTER_CLOSE()

 END FUNCTION AHNFILTER_MAIN

 !###====================================================
 SUBROUTINE AHNFILTER_INT_INITIAL()
 !###====================================================
 IMPLICIT NONE
 INTEGER :: IROW,ICOL,N
 REAL :: M

 !## get surface-level interpolated
 IDFY%X=IDFX%X

 !## interpolate nodata
 IF(INTNODATA.EQ.1)THEN
  M=0.0
  N=0
  DO IROW=1,IDFY%NROW; DO ICOL=1,IDFY%NCOL
   IF(Y(ICOL,IROW).EQ.INT(ISURF,1))THEN
    M=M+IDFY%X(ICOL,IROW)
    N=N+1
   ENDIF
  ENDDO; ENDDO
  M=M/REAL(N)
 
  DO IROW=1,IDFY%NROW
   DO ICOL=1,IDFY%NCOL
    IF(Y(ICOL,IROW).EQ.INT(INODATA,1))IDFY%X(ICOL,IROW)=M
   ENDDO
  ENDDO
 
 ELSE
 
  DO IROW=1,IDFY%NROW
   DO ICOL=1,IDFY%NCOL
    IF(Y(ICOL,IROW).EQ.INT(INODATA,1))Y(ICOL,IROW)=INT(ISURF,1)
   ENDDO
  ENDDO

 ENDIF
 
 END SUBROUTINE AHNFILTER_INT_INITIAL
 
 !###====================================================
 SUBROUTINE AHNFILTER_INT()
 !###====================================================
 IMPLICIT NONE
 INTEGER :: IROW,ICOL,IR,IC,ITER,DX,N,IERROR
 REAL :: XC,TY,NY,MAXD
 REAL,POINTER,DIMENSION(:) :: XA,YA,ZA
 
 !## fixate true surface levels
 N=0; DO IROW=1,IDFY%NROW; DO ICOL=1,IDFY%NCOL
  IF(Y(ICOL,IROW).EQ.INT(ISURF,1))N=N+1
 ENDDO; ENDDO
 ALLOCATE(XA(N),YA(N),ZA(N))
 N=0; DO IROW=1,IDFY%NROW; DO ICOL=1,IDFY%NCOL
  IF(Y(ICOL,IROW).EQ.INT(ISURF,1))THEN
   N=N+1; XA(N)=REAL(ICOL); YA(N)=REAL(IROW); ZA(N)=IDFY%X(ICOL,IROW)
  ENDIF
 ENDDO; ENDDO

 HCLOSE=XCRIT/100.0
 CALL SOLID_PCGINT(XA,YA,ZA,INT(N,4),IERROR,IDFY,-1)
 IF(IERROR.EQ.1)STOP 'ERROR IN PCG'
 DEALLOCATE(XA,YA,ZA)
 
 RETURN
   
 ITER=0
 DX  =1
 DO
  MAXD=0.0
  ITER=ITER+1

  DO IROW=1,IDFY%NROW
   DO ICOL=1,IDFY%NCOL
    !## interpolate help-surface for no surface level (.ne.2)
    IF(Y(ICOL,IROW).NE.INT(ISURF,1))THEN
     TY=0.0
     NY=0.0
     DO IR=MAX(1,IROW-DX),MIN(IROW+DX,IDFY%NROW)
      DO IC=MAX(1,ICOL-DX),MIN(ICOL+DX,IDFY%NCOL)
       IF(IDFY%X(IC,IR).NE.NODATA)THEN
        TY=TY+IDFY%X(IC,IR)
        NY=NY+1.0
       ENDIF
      END DO
     END DO
     XC=IDFY%X(ICOL,IROW)
     IDFY%X(ICOL,IROW)=TY/NY
     !## max difference
     MAXD=MAX(MAXD,ABS(XC-IDFY%X(ICOL,IROW)))
    ENDIF
   END DO
  END DO
  WRITE(*,*) ITER,MAXD
  IF(MAXD.LE.INTXCRIT)EXIT
 ENDDO

 END SUBROUTINE AHNFILTER_INT

 !###====================================================
 SUBROUTINE AHNFILTER_INT_COR(N)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: N
 INTEGER :: IR,IC

 N=0
 DO IR=1,IDFY%NROW
  DO IC=1,IDFY%NCOL
   !## only interpolated fields - and mark them as surface level!
   IF(Y(IC,IR).NE.INT(ISURF,1).AND.ABS(IDFX%X(IC,IR)-IDFY%X(IC,IR)).LE.CORXCRIT)THEN
    IDFY%X(IC,IR)=IDFX%X(IC,IR)
    Y(IC,IR)=INT(ISURF,1)
    N=N+1
   ENDIF
  END DO
 END DO
 
 WRITE(*,*) 'Number of surface migrations ',N
 
 END SUBROUTINE AHNFILTER_INT_COR

 !###====================================================
 LOGICAL FUNCTION AHNFILTER_READDATA()
 !###====================================================
 IMPLICIT NONE
 INTEGER :: I,ICOL,IROW,IC,IC1,IC2,IR,IR1,IR2
 REAL :: D,HDX,HDY
 
 AHNFILTER_READDATA=.FALSE.
 
 DO I=1,SIZE(IDFAHN)
  
  IF(.NOT.IDFREAD(IDFAHN(I),IDFAHN(I)%FNAME,0))RETURN
  HDX=IDFX%DX/2.0
  HDY=IDFX%DY/2.0
  IF(.NOT.IDFREADPART(IDFAHN(I),IDFX%XMIN+HDX,IDFX%YMIN+HDY,IDFX%XMAX-HDX,IDFX%YMAX-HDY))RETURN
  
  !## copy in right position -- translate nodata
  D  =IDFAHN(I)%XMIN-IDFX%XMIN
  IC1=D/IDFX%DX
  IF(MOD(D,IDFX%DX).EQ.0)IC1=IC1+1
  IC1=MAX(IC1,1)

  D  =IDFAHN(I)%XMAX-IDFX%XMIN
  IC2=D/IDFX%DX
  IF(MOD(D,IDFX%DX).NE.0)IC2=IC2+1
  IC2=MIN(IC2,IDFX%NCOL)

  D =IDFX%YMAX-IDFAHN(I)%YMAX
  IR1=D/IDFX%DY
  IF(MOD(D,IDFX%DY).EQ.0)IR1=IR1+1
  IR1=MAX(IR1,1)

  D =IDFX%YMAX-IDFAHN(I)%YMIN
  IR2=D/IDFX%DY
  IF(MOD(D,IDFX%DY).NE.0)IR2=IR2+1
  IR2=MIN(IR2,IDFX%NROW)

  IROW=0
  DO IR=IR1,IR2
   IROW=IROW+1
   ICOL=0
   DO IC=IC1,IC2
    ICOL=ICOL+1
    IF(IDFAHN(I)%X(ICOL,IROW).EQ.IDFAHN(I)%NODATA)IDFAHN(I)%X(ICOL,IROW)=NODATA
    IDFX%X(IC,IR)=IDFAHN(I)%X(ICOL,IROW)
   ENDDO
  ENDDO

 ENDDO
 
 AHNFILTER_READDATA=.TRUE.
 
 END FUNCTION AHNFILTER_READDATA

 !###======================================================================
 SUBROUTINE AHNFILTER_SURFACE1MAIN(IFILTER)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFILTER
 INTEGER :: IROW,ICOL

 !## no locations visited initially
 Y=INT(IINI,1)

 CALL IDFCOPY(IDFX,IDFY)
 
 DO IROW=1,IDFX%NROW
  DO ICOL=1,IDFX%NCOL
   IF(IDFX%X(ICOL,IROW).NE.NODATA)THEN
    !## process only those not yet visited
    IF(Y(ICOL,IROW).EQ.INT(IINI,1))THEN
     CALL AHNFILTER_TRACESURFACE(IROW,ICOL)
    ENDIF
   ELSE
    !## nodata
    Y(ICOL,IROW)=INT(INODATA,1)
   ENDIF
  ENDDO
  WRITE(*,*) 'Tracing ',REAL(IROW)/REAL(IDFX%NROW)*100.0,'%'
 ENDDO

 IDFY%X=REAL(Y)

 IF(IAGGREGATEY.EQ.0)RETURN

 IF(.NOT.IDFWRITE(IDFY,IDFY%FNAME(:INDEX(IDFY%FNAME,'\',.TRUE.))//'y'//TRIM(ITOS(IFILTER))//'_'//TRIM(ITOS(1))//'.idf',1))THEN
 ENDIF

 Y=INT(IINI,1)
 !## get potential surfaces together, of aligned
 DO IROW=1,IDFX%NROW
  DO ICOL=1,IDFX%NCOL
   IF(Y(ICOL,IROW).EQ.INT(IINI,1))THEN
    CALL AHNFILTER_TRACEPOINTER(IROW,ICOL)
   ENDIF
  ENDDO
 ENDDO
 !## combine adjustments 
 DO IROW=1,IDFX%NROW
  DO ICOL=1,IDFX%NCOL
   IF(Y(ICOL,IROW).NE.INT(ISURF,1))Y(ICOL,IROW)=INT(IDFY%X(ICOL,IROW),1) !=REAL(ISURF)
  ENDDO
 ENDDO

 IDFY%X=REAL(Y)
 IF(.NOT.IDFWRITE(IDFY,IDFY%FNAME(:INDEX(IDFY%FNAME,'\',.TRUE.))//'y'//TRIM(ITOS(IFILTER))//'_'//TRIM(ITOS(2))//'.idf',1))THEN
 ENDIF

 END SUBROUTINE AHNFILTER_SURFACE1MAIN

 !###======================================================================
 SUBROUTINE AHNFILTER_TRACESURFACE(IROW,ICOL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IROW,ICOL
 INTEGER :: NTHREAD,IR,IC,IDIR,JROW,JCOL,IP,I,J
 INTEGER :: IIP,TDEPR,DTERM
 LOGICAL :: LOKE
 REAL :: XC

 !## usage diagonal terms in searching
 DTERM=1

 !## copy current location
 JCOL             =ICOL
 JROW             =IROW

 !## define first point in thread
 NTHREAD          =1
 THREAD(1,NTHREAD)=JCOL
 THREAD(2,NTHREAD)=JROW
 ISPEC(NTHREAD)   =0

 !## initial less than surface-area 
 IP=IPOTSURF
 I=0

 !## mark start position
 Y(JCOL,JROW)=IP

 DO WHILE(NTHREAD.GT.0)

  !## get row/column number current location in thread
  JCOL=THREAD(1,NTHREAD)
  JROW=THREAD(2,NTHREAD)
  !## current vertical position
  XC  =IDFX%X(JCOL,JROW)

  !## get direction and do not use this direction again!
  IDIR          =ISPEC(NTHREAD)+1
  ISPEC(NTHREAD)=IDIR
  CALL IDFEDITGETDIR(JCOL,JROW,IR,IC,IDIR,DTERM,THREAD,NTHREAD,IDFX%NCOL*IDFX%NROW,0) 

  !## possible direction found
  IF(IDIR.LE.4+(4*DTERM))THEN

   !## existing location in grid
   IF(IR.GE.1.AND.IR.LE.IDFX%NROW.AND.IC.GE.1.AND.IC.LE.IDFX%NCOL)THEN

    IF(Y(IC,IR)     .EQ.INT(IINI,1).AND. &    !## not yet been there
       IDFX%X(IC,IR).NE.NODATA)THEN           !## correct location

     IIP=0
     LOKE=.FALSE.

     !## first criterium - too steep!
     IF(ABS(IDFX%X(IC,IR)-XC).LE.XCRIT)LOKE=.TRUE. !## justifies criteria to be flat

     !## determine whether local depression - river = max-min=rivcrit
     IF(LOKE)THEN
      TDEPR=AHNFILTER_IDEPR(IC,IR)
      IF(TDEPR.EQ.1)THEN
       LOKE=.FALSE.    !## local upconing
       IIP =IUPC
      ELSEIF(TDEPR.EQ.2)THEN
       LOKE=.FALSE.    !## local depression
       IIP =IDEP
      ENDIF
     ENDIF

     IF(LOKE)THEN
      !## surface level?
      I=I+1
      IF(IP.EQ.IPOTSURF)THEN
       IF(I.GT.NSCRIT)THEN
        !## make surface
        IP=ISURF
        DO J=1,NSCRIT; Y(ISCRIT(1,J),ISCRIT(2,J))=IP; END DO
       ELSE
        ISCRIT(1,I)=IC; ISCRIT(2,I)=IR
       ENDIF
      ENDIF

      Y(IC,IR)         =IP
      NTHREAD          =NTHREAD+1
      THREAD(1,NTHREAD)=IC
      THREAD(2,NTHREAD)=IR
      ISPEC(NTHREAD)   =0
     ELSE
      !## edge
      Y(IC,IR)=IIP
     ENDIF

    ENDIF
   ENDIF
  ELSE
   !## no more places to go, move one step backwards in thread
   NTHREAD=NTHREAD-1
  ENDIF
 END DO

 END SUBROUTINE AHNFILTER_TRACESURFACE

 !###======================================================================
 SUBROUTINE AHNFILTER_TRACEPOINTER(IROW,ICOL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IROW,ICOL
 INTEGER :: NTHREAD,IR,IC,IDIR,JROW,JCOL,IP,I,J,DTERM

 !## usage diagonal terms in searching
 DTERM=1
 !## copy current location
 JCOL             =ICOL
 JROW             =IROW

 !## define first point in thread
 NTHREAD          =1
 THREAD(1,NTHREAD)=JCOL
 THREAD(2,NTHREAD)=JROW
 ISPEC(NTHREAD)   =0

 !## initial less than surface-area 
 IP=IPOTSURF
 I=0

 !## mark start position
 Y(JCOL,JROW)=IP

 DO WHILE(NTHREAD.GT.0)

  !## get row/column number current location in thread
  JCOL=THREAD(1,NTHREAD)
  JROW=THREAD(2,NTHREAD)

  !## get direction and do not use this direction again!
  IDIR          =ISPEC(NTHREAD)+1
  ISPEC(NTHREAD)=IDIR
!  CALL IDFEDITGETDIR(JCOL,JROW,IR,IC,IDIR,DTERM)

  !## possible direction found
  IF(IDIR.LE.4+(4*DTERM))THEN

   !## existing location in grid
   IF(IR.GE.1.AND.IR.LE.IDFX%NROW.AND.IC.GE.1.AND.IC.LE.IDFX%NCOL)THEN

    IF(Y(IC,IR).EQ.INT(IINI,1).AND. &        !## not yet been there
       IDFY%X(IC,IR).EQ.IPOTSURF)THEN        !## correct location: potential surface

     I=I+1
     IF(IP.EQ.IPOTSURF)THEN
      IF(I.GT.NSCRIT)THEN
       IP=ISURF !## make surface
       DO J=1,NSCRIT; Y(ISCRIT(1,J),ISCRIT(2,J))=IP; END DO
      ELSE
       ISCRIT(1,I)=IC; ISCRIT(2,I)=IR
      ENDIF
     ENDIF

     Y(IC,IR)         =REAL(IP)
     NTHREAD          =NTHREAD+1
     THREAD(1,NTHREAD)=IC
     THREAD(2,NTHREAD)=IR
     ISPEC(NTHREAD)   =0
    ENDIF

   ENDIF
  ELSE
   !## no more places to go, move one step backwards in thread
   NTHREAD=NTHREAD-1
  ENDIF
 END DO

 END SUBROUTINE AHNFILTER_TRACEPOINTER

 !###====================================================
 INTEGER FUNCTION AHNFILTER_IDEPR(IC,IR)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IC,IR
 INTEGER :: I,J,NP,N
 REAL,DIMENSION(2) :: XMED

 !## no local-depression
 AHNFILTER_IDEPR=0

 NP=0
 XP=0.0
 DO J=MAX(1,IR-DPW),MIN(IR+DPW,IDFX%NROW)
  DO I=MAX(1,IC-DPW),MIN(IC+DPW,IDFX%NCOL)
   NP    =NP+1
   XP(NP)=IDFX%X(I,J)
  END DO
 END DO

 IF(MAXVAL(XP(1:NP))-MINVAL(XP(1:NP)).GT.LOCCRIT)RETURN

 !## percentile
 CALL UTL_GETMED(XP,NP,NODATA,(/DP1,DP2/),2,N,XMED)
 !## need at least half of the percentile window
 IF(N.LE.SIZE(XP)/2)RETURN

 IF(IDFX%X(IC,IR).GE.XMED(2))AHNFILTER_IDEPR=1  !##local upconing
 IF(IDFX%X(IC,IR).LE.XMED(1))AHNFILTER_IDEPR=2  !##local depression (negative)

 END FUNCTION AHNFILTER_IDEPR

 !###====================================================
 LOGICAL FUNCTION AHNFILTER_INIT()
 !###====================================================
 IMPLICIT NONE
 INTEGER :: I,IOS
 
 AHNFILTER_INIT=.FALSE.
 
 IF(IWINDOW.EQ.1)THEN
  IDFX%XMIN=XMIN
  IDFX%YMIN=YMIN
  IDFX%XMAX=XMAX
  IDFX%YMAX=YMAX
 ENDIF
 
 DO I=1,SIZE(IDFAHN)
  IDFAHN(I)%FNAME=AHN(I)
  IF(.NOT.IDFREAD(IDFAHN(I),IDFAHN(I)%FNAME,0))RETURN
  CLOSE(IDFAHN(I)%IU)
 ENDDO
 IDFY%FNAME=OUTFILE
 
 IF(IWINDOW.EQ.0)THEN
  !## get size of idf to be created
  IDFX%XMIN=MINVAL(IDFAHN(1:SIZE(IDFAHN))%XMIN)
  IDFX%YMIN=MINVAL(IDFAHN(1:SIZE(IDFAHN))%YMIN)
  IDFX%XMAX=MAXVAL(IDFAHN(1:SIZE(IDFAHN))%XMAX)
  IDFX%YMAX=MAXVAL(IDFAHN(1:SIZE(IDFAHN))%YMAX)
 ENDIF

 IDFX%DX  =IDFAHN(1)%DX
 IDFX%DY  =IDFAHN(1)%DY
 IDFX%IEQ =IDFAHN(1)%IEQ
 IDFX%ITB =0
 IDFX%IXV =0
 IDFX%DMIN=10.0E10
 IDFX%DMAX=-10.0E10
 
 !## get dimensions of yielding idf
 CALL UTL_IDFSNAPTOGRID(IDFX%XMIN,IDFX%XMAX,IDFX%YMIN,IDFX%YMAX,IDFX%DX,IDFX%NCOL,IDFX%NROW)

 !## add buffer (fit cellsize)
 IF(BUFFER.GT.IDFX%DX)THEN
  IF(MOD(BUFFER,IDFX%DX).NE.0.0)BUFFER=IDFX%DX*INT(BUFFER/IDFX%DX)
  IDFX%XMIN= IDFX%XMIN-BUFFER
  IDFX%XMAX= IDFX%XMAX+BUFFER
  IDFX%YMIN= IDFX%YMIN-BUFFER
  IDFX%YMAX= IDFX%YMAX+BUFFER
  IDFX%NCOL=(IDFX%XMAX-IDFX%XMIN)/IDFX%DX
  IDFX%NROW=(IDFX%YMAX-IDFX%YMIN)/IDFX%DX
 ENDIF
  
 ALLOCATE(ISCRIT(2,NSCRIT),STAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not allocate enough memory.'//CHAR(13)// &
  'Array ISCRIT[2,'//TRIM(ITOS(NSCRIT))//']','Error')
  RETURN
 ENDIF
 ALLOCATE(XP((DPW*2+1)**2),STAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not allocate enough memory.'//CHAR(13)// &
  'Array XP[('//TRIM(ITOS(DPW))//'*2+1)**2='//TRIM(ITOS((DPW*2+1)**2))//']','Error')
  RETURN
 ENDIF

 AHNFILTER_INIT=.TRUE.
 
 END FUNCTION AHNFILTER_INIT
 
 !###====================================================
 SUBROUTINE AHNFILTER_ALLOC()
 !###====================================================
 IMPLICIT NONE
 INTEGER :: I
 
 ALLOCATE(IDFAHN(SIZE(AHN)))
 DO I=1,SIZE(IDFAHN); CALL IDFNULLIFY(IDFAHN(I)); ENDDO
 CALL IDFNULLIFY(IDFX)
 CALL IDFNULLIFY(IDFY)
 
 END SUBROUTINE AHNFILTER_ALLOC

 !###====================================================
 LOGICAL FUNCTION AHNFILTER_ALLOC_DATA()
 !###====================================================
 IMPLICIT NONE
 INTEGER :: IOS
 
 AHNFILTER_ALLOC_DATA=.FALSE.
 
 IF(.NOT.IDFALLOCATEX(IDFX))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not allocate enough memory.'//CHAR(13)// &
  'Array IDFX%X[nrow='//TRIM(ITOS(IDFX%NROW))//' x ncol='//TRIM(ITOS(IDFX%NCOL))//']','Error')
  RETURN
 ENDIF
 !## clean idfx%x
 IDFX%X=NODATA
 ALLOCATE(THREAD(2,IDFX%NCOL*IDFX%NROW),STAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not allocate enough memory.'//CHAR(13)// &
  'Array THREAD[nrow*ncol='//TRIM(ITOS(IDFX%NROW*IDFX%NCOL))//']','Error')
  RETURN
 ENDIF
 ALLOCATE(ISPEC(IDFX%NCOL*IDFX%NROW),STAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not allocate enough memory.'//CHAR(13)// &
  'Array ISPEC[nrow*ncol='//TRIM(ITOS(IDFX%NROW*IDFX%NCOL))//']','Error')
  RETURN
 ENDIF
 ALLOCATE(Y(IDFX%NCOL,IDFX%NROW),STAT=IOS) 
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not allocate enough memory.'//CHAR(13)// &
  'Array Y[nrow='//TRIM(ITOS(IDFX%NROW))//' x ncol='//TRIM(ITOS(IDFX%NCOL))//']','Error')
  RETURN
 ENDIF

 AHNFILTER_ALLOC_DATA=.TRUE.
 
 END FUNCTION AHNFILTER_ALLOC_DATA

 !###====================================================
 SUBROUTINE AHNFILTER_CLOSE()
 !###====================================================
 IMPLICIT NONE
 
 IF(ALLOCATED(IDFAHN))THEN
  CALL IDFDEALLOCATE(IDFAHN,SIZE(IDFAHN))
  DEALLOCATE(IDFAHN)
 ENDIF 
 CALL IDFDEALLOCATEX(IDFX)
 CALL IDFDEALLOCATEX(IDFY)
 IF(ALLOCATED(Y))DEALLOCATE(Y)
 IF(ALLOCATED(XP))DEALLOCATE(XP)
 IF(ASSOCIATED(ISCRIT))DEALLOCATE(ISCRIT)
 IF(ASSOCIATED(ISPEC))DEALLOCATE(ISPEC)
 IF(ASSOCIATED(THREAD))DEALLOCATE(THREAD)
 
 END SUBROUTINE AHNFILTER_CLOSE 

END MODULE MOD_AHNFILTER