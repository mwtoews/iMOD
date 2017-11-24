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
MODULE MOD_KRIGING

!## simple kriging assumes a mean constant over the entire domain
!## ordinary kriging assumes that the mean is constant in the neighborhoud of the estimated point
USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : PI
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFIROWICOL,IDFGETLOC,IDFALLOCATEX,IDFWRITE,IDFCOPY,IDFDEALLOCATEX,IDFGETXYVAL
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_IDFSNAPTOGRID,UTL_STDEF,UTL_MESSAGEHANDLE,UTL_WAITMESSAGE,UTL_DIST,ITOS,RTOS, &
      UTL_CAP,UTL_INSIDEELLIPSE,UTL_POINTELLIPSE,UTL_INSIDEPOLYGON
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_GRAPH, ONLY : GRAPH,GRAPH_PLOT,GRAPH_DEALLOCATE,GRAPH_ALLOCATE
USE MOD_ASC2IDF_PAR, ONLY : IDFFILE,ASSF_INDICATOR
USE MOD_INTERSECT_PAR
USE MOD_INTERSECT, ONLY : INTERSECT_EQUI,INTERSECT_DEALLOCATE

REAL,ALLOCATABLE,DIMENSION(:),PRIVATE :: ZLAG,XLAG
REAL,POINTER,DIMENSION(:,:),PRIVATE :: XY=>NULL(),XYDUMMY=>NULL()
INTEGER,POINTER,DIMENSION(:),PRIVATE :: IXY=>NULL(),IXYDUMMY=>NULL()
INTEGER,ALLOCATABLE,DIMENSION(:),PRIVATE :: SELID
INTEGER,ALLOCATABLE,DIMENSION(:,:),PRIVATE :: SELQID
REAL,ALLOCATABLE,DIMENSION(:,:),PRIVATE :: SELD 
REAL,ALLOCATABLE,DIMENSION(:),PRIVATE :: B,C
REAL,ALLOCATABLE,DIMENSION(:,:),PRIVATE :: A,L,U,AI

CONTAINS

 !###======================================================================
 SUBROUTINE KRIGING_MAIN(MD,XD,YD,ZD,IDF,IDFV,MINP,RANGE,SILL,NUGGET,KTYPE, &
               PNTSEARCH,LAGINTERVAL,LAGDISTANCE,COINCIDENT,COINCIDENTDIST, &
               IQUADRANT,IBATCH,IBLANKOUT,BO_VALUE,IBLNTYPE,ELLIPS)     
 !###======================================================================
! USE OMP_LIB
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF,IDFV
 TYPE(IDFOBJ),DIMENSION(:),INTENT(INOUT),OPTIONAL :: ELLIPS
 INTEGER,INTENT(IN) :: IBATCH,IBLANKOUT,IBLNTYPE
 INTEGER,INTENT(IN) :: MD,KTYPE,LAGINTERVAL,COINCIDENT,IQUADRANT
 INTEGER,INTENT(INOUT) :: PNTSEARCH
 INTEGER,INTENT(INOUT) :: MINP
 REAL,INTENT(IN) :: LAGDISTANCE,COINCIDENTDIST,BO_VALUE
 REAL,INTENT(INOUT),DIMENSION(MD) :: XD,YD,ZD
 REAL,INTENT(INOUT) :: RANGE,SILL,NUGGET
 INTEGER :: ND,IROW,ICOL,IRAT,IRAT1
 REAL,ALLOCATABLE,DIMENSION(:) :: XCD,YCD 
 REAL :: X,Y,MZ,KEST,KVAR,ANI,RAT,R
 INTEGER :: ITYPE,NR,NC,MX,I,J
 
! NCORESV=OMP_GET_MAX_THREADS()-1
! write(*,*) NCORESV

! call omp_set_num_threads(NCORESV)
! write(*,*) omp_get_num_procs()
! write(*,*) omp_get_max_threads()
! write(*,*) omp_get_num_threads()
 
! IF(ASSOCIATED(IXY))PNTSEARCH=1
 IF(IBLANKOUT.EQ.1.OR.ASSOCIATED(IXY))THEN
  IF(PNTSEARCH.EQ.0)MINP=ND
  PNTSEARCH=1
 ENDIF
 
 !## clean points for duplicates and get mean for simple kriging
 CALL KRIGING_INIT(MD,XD,YD,ZD,ND,IDF,KTYPE,COINCIDENT,COINCIDENTDIST,IBLANKOUT,BO_VALUE,IBLNTYPE) 

 !## compute mean and substract values from mean - simple kriging
 IF(KTYPE.GT.0)THEN
  MZ=0.0; DO I=1,ND; MZ=MZ+ZD(I); ENDDO; IF(ND.GT.0.0)MZ=MZ/REAL(ND)
  DO I=1,ND; ZD(I)=ZD(I)-MZ; ENDDO
 ENDIF

 !## solve system
 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Number of kriging points '//TRIM(ITOS(ND)))
 
 ALLOCATE(SELID(ND),SELD(4,MINP),SELQID(4,MINP),A(10,10),B(10),L(10,10),U(10,10))
 CALL WINDOWOUTSTATUSBAR(1,'Press ESC to terminate')
 ALLOCATE(XCD(ND),YCD(ND))

!if(idfwrite(idf,'d:\tmp.idf',1))then; endif

 IRAT=0; IRAT1=IRAT
 DO IROW=1,IDF%NROW
  DO ICOL=1,IDF%NCOL

! DO IROW=41,41 !66,66 !313,313
!  DO ICOL=221,221 !285,285

   !## skip locations allready filled in
   IF(IDF%X(ICOL,IROW).NE.IDF%NODATA)CYCLE 
   CALL IDFGETLOC(IDF,IROW,ICOL,X,Y)
   !## isotropic
   ANI=0.0; RAT=1.0
   IF(PRESENT(ELLIPS))THEN
    ANI=IDFGETXYVAL(ELLIPS(1),X,Y) 
    RAT=IDFGETXYVAL(ELLIPS(2),X,Y)
    R  =IDFGETXYVAL(ELLIPS(3),X,Y)
   ELSE
    R=RANGE
   ENDIF
   CALL KRIGING_FILLSYSTEM(XD,XCD,YD,YCD,ZD,ND,MINP,X,Y,R,SILL,NUGGET,KTYPE,MZ,KEST,KVAR,NR,NC,MX,PNTSEARCH, &
        IQUADRANT,IDF,IBLANKOUT,BO_VALUE,RAT,ANI,IBLNTYPE)
   IDF%X(ICOL,IROW) =KEST
   !## standard deviation
   IDFV%X(ICOL,IROW)=IDFV%NODATA; IF(KVAR.GT.0.0)IDFV%X(ICOL,IROW)=SQRT(KVAR)
  ENDDO
  IF(IBATCH.EQ.0)THEN
   IF(KTYPE.GT.0)THEN
    CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW,IDF%NROW,'Progress Simple Kriging ('//TRIM(ITOS(ND))//') points')
   ELSE
    CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW,IDF%NROW,'Progress Ordinary Kriging ('//TRIM(ITOS(ND))//') points')
   ENDIF  
  ELSE
   WRITE(6,'(A,F10.3,A)') '+Progress ',REAL(100*IROW)/REAL(IDF%NROW),' % ('//TRIM(ITOS(ND))//') points'
  ENDIF
 ENDDO

 IF(ALLOCATED(XLAG))DEALLOCATE(XLAG); IF(ALLOCATED(ZLAG))DEALLOCATE(ZLAG)
 IF(ASSOCIATED(XY))DEALLOCATE(XY);    IF(ASSOCIATED(IXY))DEALLOCATE(IXY)
 
 DEALLOCATE(SELID,SELD,SELQID,A,B,L,U,XCD,YCD)
 IF(ALLOCATED(AI))DEALLOCATE(AI,C)
 IF(ALLOCATED(C))DEALLOCATE(C)
  
 END SUBROUTINE KRIGING_MAIN

 !###======================================================================
 SUBROUTINE KRIGING_READGEN(ILAY,NGEN,GENFNAME,GENILAY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ILAY,NGEN
 CHARACTER(LEN=*),INTENT(IN),DIMENSION(NGEN) :: GENFNAME
 INTEGER,INTENT(IN),DIMENSION(NGEN) :: GENILAY
 INTEGER :: IU,IOS,MAXP,MAXN,I,J,N,M,IGEN
 CHARACTER(LEN=12) :: CID
 
 IF(NGEN.LE.0)RETURN
 
 !## store coordinaten per line i nxy and ixy
 MAXP=100; MAXN=10; ALLOCATE(XY(MAXP,2),IXY(MAXN)); IXY=0; XY=0.0

 !## read polygon in GENfile is available
 N=0; M=0
 DO IGEN=1,NGEN

  !## include proper layers only
  IF(GENILAY(IGEN).NE.ILAY)CYCLE

  IF(LEN_TRIM(GENFNAME(IGEN)).EQ.0)CYCLE

  IU=UTL_GETUNIT()
  CALL OSD_OPEN(IU,FILE=GENFNAME(IGEN),STATUS='OLD',FORM='FORMATTED',ACTION='READ',IOSTAT=IOS)
  IF(IOS.NE.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open/read file:'//CHAR(13)// &
    TRIM(GENFNAME(IGEN)),'Error'); RETURN
  ENDIF
   
  DO

   !## header
   READ(IU,'(A)',IOSTAT=IOS) CID; IF(IOS.NE.0)EXIT
   IF(TRIM(UTL_CAP(CID,'U')).EQ.'END')EXIT
  
   N=N+1

   !## increase memory
   IF(N.GT.MAXN)THEN
    ALLOCATE(IXYDUMMY(MAXN+10)); DO I=1,MAXN; IXYDUMMY(I)=IXY(I); ENDDO
    DEALLOCATE(IXY); IXY=>IXYDUMMY; MAXN=MAXN+10
   ENDIF
   
   DO
    M=M+1

    !## increase memory
    IF(M.GT.MAXP)THEN
     ALLOCATE(XYDUMMY(MAXP+1000,2)); DO I=1,MAXP; DO J=1,2; XYDUMMY(I,J)=XY(I,J); ENDDO; ENDDO
     DEALLOCATE(XY); XY=>XYDUMMY; MAXP=MAXP+1000
    ENDIF

    READ(IU,*,IOSTAT=IOS) XY(M,1),XY(M,2)
    !## read END
    IF(IOS.NE.0)EXIT

   END DO
   M=M-1; IXY(N)=M

  END DO
 
  CLOSE(IU)
 ENDDO
 
 IF(M.GT.0)THEN
 
  !## fix vector length to 
  IF(N.LT.MAXN)THEN
   ALLOCATE(IXYDUMMY(N)); DO I=1,N; IXYDUMMY(I)=IXY(I); ENDDO; DEALLOCATE(IXY); IXY=>IXYDUMMY
  ENDIF
  IF(M.LT.MAXP)THEN
   ALLOCATE(XYDUMMY(M,2)); DO I=1,M; DO J=1,2; XYDUMMY(I,J)=XY(I,J); ENDDO; ENDDO; DEALLOCATE(XY); XY=>XYDUMMY
  ENDIF
 ELSE
  DEALLOCATE(IXY,XY)
 ENDIF
 
 END SUBROUTINE KRIGING_READGEN

 !###======================================================================
 SUBROUTINE KRIGING_VARIOGRAM(NDD,XD,YD,ZD,ND,IDF,LAGINTERVAL,LAGDISTANCE,IBATCH,SNAME)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: NDD
 INTEGER,INTENT(OUT) :: ND
 INTEGER,INTENT(IN),OPTIONAL :: IBATCH,LAGINTERVAL
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: SNAME
 REAL,INTENT(INOUT),DIMENSION(NDD) :: XD,YD,ZD
 REAL,INTENT(IN),OPTIONAL :: LAGDISTANCE
 REAL,ALLOCATABLE,DIMENSION(:) :: DVG,ZVG
 REAL :: NLAG,VAR,MEAN,LAGDIST,DX,DY,MZ
 INTEGER :: I,J,K,IU,NPOP,LAGINT,IRAT,IRAT1,IS,IOS
 INTEGER(KIND=8) :: N,NN
 
 IS=1
 DO
  !## get row/column data for number of unique data sets
  J=0; DO I=1,NDD,IS
   IF(XD(I).GE.IDF%XMIN.AND.XD(I).LE.IDF%XMAX.OR. &
      YD(I).GE.IDF%YMIN.AND.YD(I).LE.IDF%YMAX)J=J+1
  ENDDO
  NN=J

  !## n.pairs is n(n-1)/2
  N=(NN*(NN-1))/2
  ALLOCATE(DVG(N),STAT=IOS)
  IF(IOS.EQ.0)ALLOCATE(ZVG(N),STAT=IOS)
  IF(IOS.EQ.0)EXIT

  !## try again with less points
  IF(ALLOCATED(DVG))DEALLOCATE(DVG); IF(ALLOCATED(ZVG))DEALLOCATE(ZVG); IS=IS+1
 ENDDO
 
 ND=INT(NN,4)
 
 !## get row/column data for number of unique data sets
 J=0; DO I=1,NDD,IS
  IF(XD(I).GE.IDF%XMIN.AND.XD(I).LE.IDF%XMAX.OR. &
     YD(I).GE.IDF%YMIN.AND.YD(I).LE.IDF%YMAX)THEN
   J=J+1; XD(J)=XD(I); YD(J)=YD(I); ZD(J)=ZD(I)
  ENDIF 
 ENDDO
 ND=J

 !## compute mean and substract values from mean
 MZ=0.0; DO I=1,ND; MZ=MZ+ZD(I); ENDDO; MZ=MZ/REAL(ND)
 DO I=1,ND; ZD(I)=ZD(I)-MZ; ENDDO
  
 DVG=0.0; ZVG=0.0; K=0
 DO I=1,ND,IS
  DO J=I+1,ND,IS
   DX=(XD(I)-XD(J))**2.0; DY=(YD(I)-YD(J))**2.0
   IF(DX+DY.GT.0.0)THEN
    K=K+1
    DVG(K)=SQRT(DX+DY)
    ZVG(K)=(ZD(I)-ZD(J))**2.0
   ENDIF
  ENDDO
 ENDDO
 
 !## laginterval=number of intervals
 !## lagdistance=size of interval
 IF(PRESENT(LAGDISTANCE).AND.PRESENT(LAGINTERVAL))THEN
  LAGDIST=LAGDISTANCE
  LAGINT=LAGINTERVAL
 ELSE
  CALL UTL_STDEF(DVG,K,-999.99,VAR,MEAN,NPOP)
  LAGINT=20
  LAGDIST=(MEAN+(1.0*VAR))/LAGINT
 ENDIF

 IF(ALLOCATED(XLAG))DEALLOCATE(XLAG)
 IF(ALLOCATED(ZLAG))DEALLOCATE(ZLAG)
  
 ALLOCATE(ZLAG(0:LAGINT),XLAG(0:LAGINT)) 
 XLAG(0)=0.0; ZLAG=0.0
 
 !## compute semivariogram
 IRAT=0; IRAT1=IRAT
 DO I=1,LAGINT
  XLAG(I)=XLAG(I-1)+LAGDIST; NLAG=0.0
  DO J=1,N 
   IF(DVG(J).GE.XLAG(I-1).AND.DVG(J).LT.XLAG(I))THEN
    NLAG   =NLAG   +1.0
    ZLAG(I)=ZLAG(I)+ZVG(J)
   ENDIF
  ENDDO
  IF(NLAG.GT.0.0)ZLAG(I)=ZLAG(I)/(2.0*NLAG)
  CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,LAGINT,'Progress computing Semivariance')
 ENDDO
 
 IF(IBATCH.EQ.0)THEN
  IF(ALLOCATED(GRAPH))CALL GRAPH_DEALLOCATE()
  CALL GRAPH_ALLOCATE(1,1)
  ALLOCATE(GRAPH(1,1)%RX(LAGINT))
  ALLOCATE(GRAPH(1,1)%RY(LAGINT))
  GRAPH(1,1)%RX(1:LAGINT)=XLAG(1:LAGINT)
  GRAPH(1,1)%RY(1:LAGINT)=ZLAG(1:LAGINT)
  GRAPH(1,1)%NP=LAGINT
  GRAPH(1,1)%GTYPE=1 
  GRAPH(1,1)%LEGTXT='Value'
  GRAPH(1,1)%ICLR=WRGB(56,180,176)
  CALL UTL_MESSAGEHANDLE(1)
  CALL GRAPH_PLOT('Distance','Semivariance (m2)',.FALSE.,.FALSE.)
  IF(ALLOCATED(GRAPH))CALL GRAPH_DEALLOCATE()
 ELSE
  IF(TRIM(SNAME).NE.'')THEN
   IU=UTL_GETUNIT()
   CALL OSD_OPEN(IU,FILE=SNAME(:INDEX(SNAME,'\',.TRUE.))//'variogram.txt',STATUS='UNKNOWN',ACTION='WRITE')
   WRITE(IU,'(2A15)') 'LAG_DISTANCE','VARIOGRAM'
   DO I=1,LAGINT; WRITE(IU,'(2F15.7)') XLAG(I),ZLAG(I); ENDDO
   CLOSE(IU)
  ENDIF
 ENDIF
 
 DEALLOCATE(DVG,ZVG) 
 
 END SUBROUTINE KRIGING_VARIOGRAM

 !###======================================================================
 SUBROUTINE KRIGING_FILLSYSTEM(XD,XCD,YD,YCD,ZD,ND,MINP,X,Y,RANGE,SILL,NUGGET, &
          KTYPE,MZ,KEST,KVAR,NR,NC,MX,PNTSEARCH,IQUADRANT,IDF,IBLANKOUT,BO_VALUE,RAT,ANI,IBLNTYPE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: ND,MINP,KTYPE,MX,PNTSEARCH,IQUADRANT,IBLANKOUT,IBLNTYPE
 REAL,INTENT(OUT) :: KEST,KVAR
 REAL,INTENT(IN) :: X,Y,SILL,NUGGET,MZ,RANGE,BO_VALUE,RAT,ANI
 REAL,INTENT(IN),DIMENSION(ND) :: XD,YD,ZD
 REAL,INTENT(OUT),DIMENSION(ND) :: XCD,YCD
 INTEGER,INTENT(IN) :: NR,NC
 INTEGER :: I,J,K,IC,IR,NP,N,ID,JD,IROW,ICOL,IQ,ICMPINV,IU
 REAL :: DX,DY,DXY,GAMMA,H,MZZ,F,USERANGE,C0,C1,NODATA,Z
 
 NODATA=IDF%NODATA
 
 USERANGE=RANGE
 C0      =NUGGET
 C1      =SILL-NUGGET
 
 !## already computed inverse of A
 ICMPINV=0; IF(ALLOCATED(AI))ICMPINV=1

 !## copy points 
 XCD=XD
 YCD=YD

 IF(PNTSEARCH.EQ.1)THEN
  
  !## maximal search is minp - get them
  SELQID=0; SELD=HUGE(1.0)
  DO ID=1,ND
   
   !## get distance between points - remove whenever intersected by faults
   DXY=KRIGING_DIST(2,XCD(ID),YCD(ID),X,Y,0,IDF,IBLANKOUT,BO_VALUE,USERANGE,RAT,ANI,IBLNTYPE)
!   DXY=KRIGING_DIST(XD(ID),YD(ID),X,Y,0,IDF,IBLANKOUT,BO_VALUE,USERANGE,RAT,ANI)
   !## otherside of fault or distance too big, take next
   IF(DXY.GT.USERANGE)CYCLE
   
   !## determine quadrant
   IQ=1; IF(IQUADRANT.EQ.1)IQ=KRIGING_QUADRANT(XCD(ID),YCD(ID),X,Y)
!   IQ=1; IF(IQUADRANT.EQ.1)IQ=KRIGING_QUADRANT(XD(ID),YD(ID),X,Y)
   
   !## not in current list
   IF(DXY.GE.SELD(IQ,MINP))CYCLE
   !## get position in sort
   DO I=1,MINP; IF(DXY.LT.SELD(IQ,I))EXIT; ENDDO
   !## make place for new least-distance point
   DO J=MINP,I+1,-1; SELQID(IQ,J)=SELQID(IQ,J-1); SELD(IQ,J)=SELD(IQ,J-1); ENDDO
   !## put new location in nearest list
   SELQID(IQ,I)=ID; SELD(IQ,I)=DXY
  ENDDO
  
  SELID=0; NP=0 
  DO IQ=1,1+(IQUADRANT*3)
   DO J=1,MINP
    IF(SELQID(IQ,J).GT.0)THEN
     NP=NP+1; SELID(NP)=SELQID(IQ,J) 
    ENDIF
   ENDDO 
  ENDDO 
    
 ELSE

  J=0
  DO ID=1,ND
   J=J+1; SELID(J)=ID
  ENDDO
  NP=J 
  
 ENDIF
  
 !## no points left, interpolated value equals nodata
 IF(NP.LE.0)THEN; KEST=NODATA; KVAR=0.0; RETURN; ENDIF
  
! !## save points
! IU=UTL_GETUNIT()
! CALL OSD_OPEN(IU,FILE='d:\iMOD-TEST\IMODBATCH_KRIGING\pospunt.ipf',STATUS='UNKNOWN',ACTION='WRITE')
! WRITE(IU,'(A)') 'X,Y,Z,id'; WRITE(IU,*) X,Y,0.0,0
! DO I=1,NP
!  ID=SELID(I)
!  WRITE(IU,*) XCD(ID),YcD(ID),ZD(ID),id
! ENDDO
! CLOSE(IU)
! CALL OSD_OPEN(IU,FILE='d:\iMOD-TEST\IMODBATCH_KRIGING\orgpunt.ipf',STATUS='UNKNOWN',ACTION='WRITE')
! WRITE(IU,'(A)') 'X,Y,Z,id'; WRITE(IU,*) X,Y,0.0,0
! DO I=1,NP
!  ID=SELID(I)
!  WRITE(IU,*) XD(ID),YD(ID),ZD(ID),id
! ENDDO
! CLOSE(IU)

 !## simple kriging (ktype.gt.0) and ordinary kriging (ktype.lt.0)
 N=NP; IF(KTYPE.LT.0)N=NP+1
 
 IF(ICMPINV.EQ.0)THEN

  IF(N.GT.SIZE(B))THEN
   DEALLOCATE(A,B,L,U); ALLOCATE(A(N,N),L(N,N),U(N,N),B(N))
  ENDIF
 
  !## fill in A()
  A=0.0
  DO I=1,NP 
   ID=SELID(I)
   !## semivariance
   A(I,I)=SILL 
   DO J=I+1,NP 
    JD=SELID(J)
    GAMMA=KRIGING_GETGAMMA(XCD(ID),YCD(ID),XCD(JD),YCD(JD),USERANGE,C1,C0,KTYPE)
!    GAMMA=KRIGING_GETGAMMA(XD(ID),YD(ID),XD(JD),YD(JD),USERANGE,C1,C0,KTYPE)
    A(I,J)=SILL-GAMMA; A(J,I)=A(I,J)
   ENDDO 
  ENDDO
  !## ordinary kriging, fill for lambda
  IF(KTYPE.LT.0)THEN
   DO I=1,N; A(I,N)=1.0; A(N,I)=1.0; ENDDO; A(N,N)=0.0
  ENDIF
 
 ENDIF
 
 !## fill in B()
 B=0.0
 DO I=1,NP
  ID=SELID(I)
  GAMMA=KRIGING_GETGAMMA(XCD(ID),YCD(ID),X,Y,USERANGE,C1,C0,KTYPE)
!  GAMMA=KRIGING_GETGAMMA(XD(ID),YD(ID),X,Y,USERANGE,C1,C0,KTYPE)
  B(I)=SILL-GAMMA  
 ENDDO
 
 !## ordinary kriging, fill for lambda
 IF(KTYPE.LT.0)B(N)=1.0
!  !## fill for lambda
!  J=N 
!  DO I=1,J; A(I,J)=1.0; A(J,I)=1.0; ENDDO; A(J,J)=0.0; B(J)=1.0
! ENDIF
 
 IF(ICMPINV.EQ.0)THEN
  IF(PNTSEARCH.EQ.0)ALLOCATE(AI(N,N),C(N))
  CALL KRIGING_LUDCMP(N,ABS(PNTSEARCH-1))
 ENDIF
 
 IF(PNTSEARCH.EQ.0)THEN
  C=MATMUL(AI,B); B=C
 ENDIF
 
 !## ordinary kriging compute the local mean mzz instead of the global mean mz
 IF(KTYPE.LT.0)THEN
  MZZ=0.0; DO I=1,NP; ID=SELID(I); MZZ=MZZ+ZD(ID); ENDDO; MZZ=MZZ/REAL(NP)
 ENDIF

 KEST=0.0
 DO I=1,NP 
  ID=SELID(I)
  IF(KTYPE.GT.0)THEN
   KEST=KEST+B(I)* ZD(ID)
  ELSE  
   KEST=KEST+B(I)*(ZD(ID)-MZZ)
  ENDIF
 ENDDO
 !## global mean (simple kriging)
 IF(KTYPE.GT.0)KEST=KEST+MZ
 !## local mean (ordinary kriging)
 IF(KTYPE.LT.0)KEST=KEST+MZZ
 
 !## standard deviation
 KVAR=0.0
 DO I=1,NP
  ID=SELID(I)
  GAMMA=KRIGING_GETGAMMA(XCD(ID),YCD(ID),X,Y,USERANGE,C1,C0,KTYPE)
!  GAMMA=KRIGING_GETGAMMA(XD(ID),YD(ID),X,Y,USERANGE,C1,C0,KTYPE)
  KVAR=KVAR+B(I)*(SILL-GAMMA) 
 ENDDO
 KVAR=SILL-KVAR
   
 END SUBROUTINE KRIGING_FILLSYSTEM

 !###======================================================================
 INTEGER FUNCTION KRIGING_QUADRANT(XD,YD,X,Y)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: XD,YD,X,Y
 
 IF(XD.GT.X)THEN
  KRIGING_QUADRANT=1; IF(YD.LT.Y)KRIGING_QUADRANT=2
 ELSE
  KRIGING_QUADRANT=4; IF(YD.LT.Y)KRIGING_QUADRANT=3
 ENDIF
 
 END FUNCTION KRIGING_QUADRANT

 !###======================================================================
 REAL FUNCTION KRIGING_DIST(IMODE,X1,Y1,X0,Y0,IDATA,IDF,IBLANKOUT,BO_VALUE,MAXDIST,RAT,ANI,IBLNTYPE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: IDATA  !=0 FOR INSERTION IN KRIGING MATRIX  !=1 FOR AVERAGE POINT VALUES
 INTEGER,INTENT(IN) :: IBLANKOUT
 INTEGER,INTENT(IN) :: IMODE !## 1=init 2=interpolate
 INTEGER,INTENT(IN) :: IBLNTYPE
 REAL,INTENT(INOUT) :: X1,Y1
 REAL,INTENT(IN) :: X0,Y0,BO_VALUE,MAXDIST,RAT,ANI
 REAL :: X3,Y3,X4,Y4,XINTER,YINTER,A,SSX,DX,DY,D,CA,SA,F
 INTEGER :: I,J,I1,I2,ISTATUS,N,IROW,ICOL,IR,IC,IPOL1,IPOL2
 
 KRIGING_DIST=UTL_DIST(X1,Y1,X0,Y0)

 !## to far away (include 1.0*maxdist on edge - for smooth edges)
 IF(KRIGING_DIST.GE.MAXDIST)RETURN !THEN
 IF(KRIGING_DIST.LE.0.0)THEN
!  IF(IMODE.EQ.1)RETURN
! if(imode.eq.2)then
!write(*,*)
! endif
  RETURN
!  WRITE(*,'(/A/)') ' Distance equal to zero, not possible !'; STOP
 ENDIF
  
 !## see whether nodata-areas are crossed
 IF(IBLANKOUT.EQ.1)THEN
  !## aspect
  DY=(Y0-Y1); DX=(X0-X1); A=ATAN2(DY,DX)
  SSX=IDF%DX
  D=KRIGING_DIST
  CALL IDFIROWICOL(IDF,IROW,ICOL,X1,Y1)
  DX=COS(A)*SSX
  DY=SIN(A)*SSX
  X3=X1; Y3=Y1
  DO
   X3=X3+DX; Y3=Y3+DY
   !## get new grid location
   CALL IDFIROWICOL(IDF,IROW,ICOL,X3,Y3)
   !## outside window
   IF(IROW.NE.0.AND.ICOL.NE.0)THEN
    !## if crossed-idf value is equal nodata increase distance
    IF(IDF%X(ICOL,IROW).EQ.BO_VALUE)THEN 
     KRIGING_DIST=MAXDIST+1.0
     EXIT
    ENDIF
   ENDIF
   SSX=SSX+IDF%DX; IF(SSX.GT.D)EXIT
  ENDDO
 ENDIF
 
 !## including a genfile
 IF(ASSOCIATED(IXY))THEN

  !## process as a fault
  IF(IBLNTYPE.EQ.0)THEN

   DO J=1,SIZE(IXY) 
    !## see whether point intersect a fault
    I1=1; IF(J.GT.1)I1=IXY(J-1); I2=IXY(J)
    DO I=I1+1,I2
     X3=XY(I-1,1); Y3=XY(I-1,2)
     X4=XY(I  ,1); Y4=XY(I  ,2)
     CALL IGRINTERSECTLINE(X1,Y1,X0,Y0,X3,Y3,X4,Y4,XINTER,YINTER,ISTATUS)
     !## if line intersect, increase distance as a penalty
     IF(ISTATUS.EQ.5)THEN
      KRIGING_DIST=MAXDIST+1.0
      RETURN
     ENDIF
    ENDDO
   ENDDO

  !## process as a polygon
  ELSEIF(IBLNTYPE.EQ.1)THEN
   
   !## what polygon is x0,y0 ?
   DO J=1,SIZE(IXY)
    I1=1; IF(J.GT.1)I1=IXY(J-1); I2=IXY(J); N=I2-I1+1
    IF(UTL_INSIDEPOLYGON(X0,Y0,XY(I1:I2,1),XY(I1:I2,2),N).EQ.1)EXIT
   ENDDO
   IPOL1=J; IF(IPOL1.GT.SIZE(IXY))IPOL1=0
   
   !## what polygon is x1,y1
   DO J=1,SIZE(IXY)
    I1=1; IF(J.GT.1)I1=IXY(J-1); I2=IXY(J); N=I2-I1+1
    IF(UTL_INSIDEPOLYGON(X1,Y1,XY(I1:I2,1),XY(I1:I2,2),N).EQ.1)EXIT
   ENDDO
   IPOL2=J; IF(IPOL2.GT.SIZE(IXY))IPOL2=0

   IF(IPOL1.NE.IPOL2)KRIGING_DIST=MAXDIST+1.0
   
  ENDIF
  
 ENDIF
 
 !## in case of an ellips see if it is inside the current ellips
 IF(RAT.NE.1.0)THEN
  IF(.NOT.UTL_INSIDEELLIPSE(X0,Y0,MAXDIST/2.0,RAT*MAXDIST/2.0,ANI-90.0,X1,Y1))THEN
   KRIGING_DIST=MAXDIST+1.0
  ELSE
   !## adjust distance for perfect circle
   DY=(Y0-Y1); DX=(X0-X1); A=ATAN2(DY,DX)

   A=-(ANI+90.0)/(360.0/(2.0*PI))
   X1=         COS(A)*DX+        SIN(A)*DY
   Y1=-1.0/RAT*SIN(A)*DX+1.0/RAT*COS(A)*DY
   X1=X0+X1
   Y1=Y0+Y1
   
!   !## get point on ellips for current aspect
!   CALL UTL_POINTELLIPSE(X0,Y0,A,RAT,MAXDIST,ANI,X3,Y3)
!   D=UTL_DIST(X0,Y0,X3,Y3)
!   F=MAXDIST/D
!   KRIGING_DIST=KRIGING_DIST*F
!   !## set temporary new location
!   X1=X0+KRIGING_DIST*COS(A)
!   Y1=Y0-KRIGING_DIST*SIN(A)
  
   KRIGING_DIST=UTL_DIST(X1,Y1,X0,Y0)
  
  ENDIF
 ENDIF
  
 END FUNCTION KRIGING_DIST

 !###======================================================================
 REAL FUNCTION KRIGING_GETGAMMA(X1,Y1,X2,Y2,RANGE,C1,C0,KTYPE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: KTYPE
 REAL,INTENT(IN) :: X1,Y1,X2,Y2,RANGE,C1,C0
 REAL :: DX,DY,DXY,H,F
 INTEGER :: I
 
 DXY=UTL_DIST(X1,Y1,X2,Y2)
! DX=(X1-X2)**2.0; DY=(Y1-Y2)**2.0; DXY=DX+DY
! IF(DXY.GT.0.0)DXY=SQRT(DXY)
 
 IF(DXY.GE.RANGE)THEN
  H=0.999 
 ELSE

  !## no part of kriging, beyond given range, equal to sill
  SELECT CASE (ABS(KTYPE))
   CASE (1) !## linear
    H=DXY/RANGE
   CASE (2) !## spherical
    H=(3.0*DXY)/(2.0*RANGE)-(0.5*(DXY/RANGE)**3.0)
   CASE (3) !## exponential
    H=1.0-EXP(-3.0*(DXY/RANGE))
   CASE (4) !## gaussian
    H=1.0-EXP(-3.0*(DXY**2.0)/(RANGE**2.0))
   CASE (5) !## power
    H=DXY**0.5
   CASE DEFAULT
    WRITE(*,*) 'UNKNOWN KTYPE',KTYPE; PAUSE; STOP
  END SELECT
 ENDIF
 
 KRIGING_GETGAMMA=C0+C1*H
 
 END FUNCTION KRIGING_GETGAMMA

 !###======================================================================
 SUBROUTINE KRIGING_INIT(MD,XD,YD,ZD,ND,IDF,KTYPE,COINCIDENT,COINCIDENTDIST,IBLANKOUT,BO_VALUE,IBLNTYPE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: MD,KTYPE,COINCIDENT,IBLANKOUT,IBLNTYPE
 REAL,INTENT(INOUT),DIMENSION(MD) :: XD,YD,ZD 
 REAL,INTENT(IN) :: COINCIDENTDIST,BO_VALUE
 INTEGER,INTENT(OUT) :: ND
 INTEGER :: I,J,N,IE,IROW,ICOL
 REAL,ALLOCATABLE,DIMENSION(:) :: XCOL,YROW
 REAL :: XYCRIT,XP,YP,ZP
 
 XYCRIT=0.1*IDF%DX

 !## apply a user specified critical distance
 IF(COINCIDENT.EQ.1)XYCRIT=MAX(COINCIDENTDIST,XYCRIT)
 
 !## mark doubles with nodata and compute mean for those double points
 DO I=1,MD
  !## done allready
  IF(ZD(I).EQ.IDF%NODATA)CYCLE
  N=1; XP=XD(I); YP=YD(I); ZP=ZD(I)
  DO J=I+1,MD
   !## done allready
   IF(ZD(J).EQ.IDF%NODATA)CYCLE
   !## get distance between points - increase distance whenever points are intersected by fault
   IF(KRIGING_DIST(1,XD(I),YD(I),XD(J),YD(J),1,IDF,IBLANKOUT,BO_VALUE,XYCRIT,1.0,0.0,IBLNTYPE).LE.XYCRIT)THEN
    N=N+1

    !## take maximal indicator value
    IF(ASSF_INDICATOR.EQ.1)THEN
     ZP=MAX(ZP,ZD(J))
    !## take average value
    ELSE
     ZP=ZP+ZD(J)
    ENDIF

    !## add point to existing point and turn location off
    XP=XP+XD(J); YP=YP+YD(J); ZD(J)=IDF%NODATA
   ENDIF
  ENDDO
  !## determine average spot
  XD(I)=XP/REAL(N); YD(I)=YP/REAL(N)

  IF(ASSF_INDICATOR.EQ.1)THEN
   ZD(I)=ZP
  ELSE
   ZD(I)=ZP/REAL(N)
  ENDIF

 END DO
  
 !## eliminate doubles
 J=0
 DO I=1,MD
  IF(ZD(I).NE.IDF%NODATA)THEN
   J=J+1
   IF(J.NE.I)THEN
    XD(J)=XD(I); YD(J)=YD(I); ZD(J)=ZD(I)
   ENDIF
  ENDIF
 END DO
 ND=J

 END SUBROUTINE KRIGING_INIT

 !###======================================================================
 SUBROUTINE KRIGING_UNITTEST() 
 !###======================================================================
 !http://www.unigis.ac.at/fernstudien/UNIGIS_professional/traun/spatial_interpolation/kriging4.htm
 IMPLICIT NONE
 INTEGER,PARAMETER :: MD=3
 REAL,DIMENSION(MD) :: XD,YD,ZD,XCD,YCD
 DATA XD/100.0, 50.0,250.0/
 DATA YD/100.0,250.0,200.0/
 DATA ZD/5.0,20.0,32.0/

! DATA XD/2.0,3.0,9.0,6.0,5.0/
! DATA YD/2.0,7.0,9.0,5.0,3.0/
! DATA ZD/3.0,4.0,2.0,4.0,6.0/
! DATA XD/825.0 ,2700.0,3700.0,2300.0,825.0 ,500.0/
! DATA YD/3700.0,4300.0,5200.0,5700.0,5150.0,4950.0/
! DATA ZD/13.84 ,12.15 ,12.87 ,12.68 ,14.41 ,14.59/
 REAL :: EX,EY,RANGE,NUGGET,SILL,MZ,X,Y,KEST,KVAR,LAGDISTANCE,COINCIDENTDIST
 INTEGER :: MINP,KTYPE,IROW,ICOL,I,PNTSEARCH,ND,NR,NC,MX,LAGINTERVAL,COINCIDENT,IQUADRANT
 TYPE(IDFOBJ) :: IDF,IDFV
 
 IDF%DX=1.0; IDF%DY=1.0
 IDF%XMIN=MINVAL(XD)-5.0; IDF%XMAX=MAXVAL(XD)+5.0
 IDF%YMIN=MINVAL(YD)-5.0; IDF%YMAX=MAXVAL(YD)+5.0
 IDF%NCOL=(IDF%XMAX-IDF%XMIN)/IDF%DX; IDF%NROW=(IDF%YMAX-IDF%YMIN)/IDF%DY
 IF(.NOT.IDFALLOCATEX(IDF))RETURN
 CALL IDFCOPY(IDF,IDFV)

! MINP=MD; RANGE=4141.0; NUGGET=0.01; SILL=0.78; KTYPE=2; PNTSEARCH=0; NR=0; NC=0
! LAGINTERVAL=20; LAGDISTANCE=RANGE/REAL(LAGINTERVAL); COINCIDENT=0; IQUADRANT=0

 MINP=MD; RANGE=400.0; NUGGET=4.0; SILL=9.0; KTYPE=-2; PNTSEARCH=0; NR=0; NC=0
 LAGINTERVAL=20; LAGDISTANCE=RANGE/REAL(LAGINTERVAL); COINCIDENT=0; IQUADRANT=0; COINCIDENT=0.0
 MX=RANGE/IDF%DX
 
! SILL=SILL-NUGGET
  
! sill=0.78
! sill=10.0
! nugget=5.0
  
 CALL KRIGING_INIT(MD,XD,YD,ZD,ND,IDF,KTYPE,COINCIDENT,COINCIDENTDIST,0,0.0,0)
 
 !## compute mean and substract values from mean - simple kriging
 IF(KTYPE.GT.0)THEN
  MZ=0.0; DO I=1,ND; MZ=MZ+ZD(I); ENDDO; MZ=MZ/REAL(ND)
  DO I=1,ND; ZD(I)=ZD(I)-MZ; ENDDO
 ENDIF

 ND=MD
 ALLOCATE(SELID(ND),SELD(4,MINP),SELQID(4,MINP),A(10,10),B(10),L(10,10),U(10,10))

 X=150.0; Y=150.0
 CALL KRIGING_FILLSYSTEM(XD,XCD,YD,YCD,ZD,ND,MINP,X,Y,RANGE,SILL,NUGGET,KTYPE,MZ,KEST,KVAR,NR,NC,MX,PNTSEARCH,IQUADRANT,IDF,0,0.0,1.0,0.0,0)
 write(*,*) kest,sqrt(kvar)
 
 DO IROW=1,IDF%NROW
  DO ICOL=1,IDF%NCOL
  ! X=2000.0; Y=4700.0
   X=5.0; Y=5.0
   CALL IDFGETLOC(IDF,IROW,ICOL,X,Y)
   CALL KRIGING_FILLSYSTEM(XD,XCD,YD,YCD,ZD,ND,MINP,X,Y,RANGE,SILL,NUGGET,KTYPE,MZ,KEST,KVAR,NR,NC,MX,PNTSEARCH,IQUADRANT,IDF,0,0.0,1.0,0.0,0)
   IDF%X(ICOL,IROW)=KEST
   IDFV%X(ICOL,IROW)=SQRT(KVAR)
  ENDDO
 ENDDO
 WRITE(*,*) KEST,KVAR,SQRT(KVAR)
 IF(.NOT.IDFWRITE(IDF ,'D:\unittest_kriging.idf',1))RETURN
 IF(.NOT.IDFWRITE(IDFV,'D:\unittest_krigingstdev.idf',1))RETURN
 OPEN(10,FILE='D:\unittest_kriging_point.ipf',STATUS='UNKNOWN')
 WRITE(10,*) MD; WRITE(10,*) 3; WRITE(10,*) 'X'; WRITE(10,*) 'Y'; WRITE(10,*) 'Z'; WRITE(10,*) '0,TXT'
 DO I=1,MD; WRITE(10,*) XD(I),YD(I),ZD(I); ENDDO
 CLOSE(10)

 STOP
 END SUBROUTINE KRIGING_UNITTEST

 !###====================================================================
 SUBROUTINE KRIGINGSETTINGS(MINP,KTYPE,RANGE,SILL,NUGGET,PNTSEARCH,COINCIDENT,COINCIDENTDIST,IQUADRANT,NGEN)
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(INOUT) :: RANGE,SILL,NUGGET,COINCIDENTDIST
 INTEGER,INTENT(INOUT) :: MINP,KTYPE,PNTSEARCH,COINCIDENT,IQUADRANT
 INTEGER,INTENT(IN) :: NGEN
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I,IERROR
 
 CALL WDIALOGLOAD(ID_DKRIGING,ID_DKRIGING)
 CALL WDIALOGSHOW(-1,-1,0,3)
 
 IF(NGEN.GT.0)THEN; PNTSEARCH=1; CALL WDIALOGFIELDSTATE(IDF_CHECK1,0); ENDIF
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,PNTSEARCH)
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,COINCIDENT)
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK3,IQUADRANT)

 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,MINP)
 CALL WDIALOGPUTREAL(IDF_REAL4,COINCIDENTDIST,'(F10.2)')  
 CALL WDIALOGPUTREAL(IDF_REAL1,SILL,'(F10.2)')  
 CALL WDIALOGPUTREAL(IDF_REAL2,RANGE,'(F10.2)') 
 CALL WDIALOGPUTREAL(IDF_REAL3,NUGGET,'(F10.2)')  
 !## put semivariogram type
 IF(KTYPE.GT.5)KTYPE=2; IF(KTYPE.LT.-5)KTYPE=-2; IF(KTYPE.EQ.0)KTYPE=2
 CALL WDIALOGPUTOPTION(IDF_MENU1,ABS(KTYPE))
 !## type of kriging ktype>0 simple kriging, ktype<0 ordinary kriging
 IF(KTYPE.GT.0)CALL WDIALOGPUTOPTION(IDF_MENU2,1)
 IF(KTYPE.LT.0)CALL WDIALOGPUTOPTION(IDF_MENU2,2)
 
 CALL WDIALOGFIELDOPTIONS(IDF_INTEGER1,EDITFIELDCHANGED,ENABLED)

 !## choice has been made in calling program
 CALL KRIGINGSETTINGSFIELDS()
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE(FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_INTEGER1,IDF_CHECK1,IDF_CHECK2,IDF_CHECK3,IDF_MENU1,IDF_MENU2)
      CALL KRIGINGSETTINGSFIELDS()
    END SELECT

   CASE(PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDOK)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,PNTSEARCH)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK2,COINCIDENT)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK3,IQUADRANT) 
      CALL WDIALOGGETINTEGER(IDF_INTEGER1,MINP)
      CALL WDIALOGGETREAL(IDF_REAL1,SILL) 
      CALL WDIALOGGETREAL(IDF_REAL2,RANGE)  
      CALL WDIALOGGETREAL(IDF_REAL3,NUGGET)  
      CALL WDIALOGGETREAL(IDF_REAL4,COINCIDENTDIST)  
      !## get semivariogram type
      CALL WDIALOGGETMENU(IDF_MENU1,KTYPE)
      CALL WDIALOGGETMENU(IDF_MENU2,I)
      IF(I.EQ.2)KTYPE=-1*KTYPE
      IERROR=0
      IF(PNTSEARCH.EQ.1.AND.MINP.LE.0)THEN
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Maximum number of points is '//TRIM(ITOS(MINP))//CHAR(13)// &
         'and need to larger than zero','Error'); IERROR=1
      ENDIF
      IF(RANGE.LE.0.0)THEN
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Range is '//TRIM(RTOS(RANGE,'F',2))//CHAR(13)// &
         'and need to larger than zero','Error'); IERROR=1
      ENDIF
      IF(SILL.LE.0.0)THEN
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Sill is '//TRIM(RTOS(SILL,'F',2))//CHAR(13)// &
         'and need to larger than zero','Error'); IERROR=1
      ENDIF
      IF(NUGGET.LT.0.0)THEN
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Nugget is '//TRIM(RTOS(NUGGET,'F',2))//CHAR(13)// &
         'and need to be larger or equal to zero','Error'); IERROR=1
      ENDIF
      IF(SILL-NUGGET.LE.0.0)THEN
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Sill minus Nugget is '//TRIM(RTOS(SILL-NUGGET,'F',2))//CHAR(13)// &
         'and need to larger than zero','Error'); IERROR=1
      ENDIF
      IF(IERROR.EQ.0)EXIT
     CASE (IDCANCEL)
      EXIT
    END SELECT
  END SELECT

 ENDDO

 CALL WDIALOGUNLOAD()
 
 END SUBROUTINE KRIGINGSETTINGS

 !###====================================================================
 SUBROUTINE KRIGINGSETTINGSFIELDS()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,J
 
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
 CALL WDIALOGFIELDSTATE(IDF_INTEGER1,I)
 CALL WDIALOGFIELDSTATE(IDF_CHECK3,I)
 
 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,J)
 CALL WDIALOGFIELDSTATE(IDF_REAL4,J)
 CALL WDIALOGFIELDSTATE(IDF_LABEL13,J)
 
 CALL WDIALOGGETCHECKBOX(IDF_CHECK3,I)
 IF(I.EQ.1)THEN
  CALL WDIALOGGETINTEGER(IDF_INTEGER1,J)
  CALL WDIALOGPUTSTRING(IDF_CHECK3,'Apply Quadrants (total points maximal '//TRIM(ITOS(J*4))//')')
 ELSE
  CALL WDIALOGPUTSTRING(IDF_CHECK3,'Apply Quadrants')
 ENDIF
 
 CALL WDIALOGGETMENU(IDF_MENU1,I)
 SELECT CASE (I)
  CASE (1)
    CALL WDIALOGPUTSTRING(IDF_LABEL11,'Linear act linearly. Appropriate for representing properties with higher level of short-range variability.')
  CASE (2)
    CALL WDIALOGPUTSTRING(IDF_LABEL11,'Spherical acts linear in the beginning. '// &
       'Appropriate for representing properties with higher level of short-range variability')
  CASE (3)
    CALL WDIALOGPUTSTRING(IDF_LABEL11,'Exponential acts linear in the beginning but curves rapidly for points further away and approaces the sill asymptotically.')
  CASE (4)
    CALL WDIALOGPUTSTRING(IDF_LABEL11,'Gaussian weights the points nearest least heavy as a "S"-curve and approaces the sill asymptotically. '// &
       'It gives very smooth varying properties.')
  CASE (5)
    CALL WDIALOGPUTSTRING(IDF_LABEL11,'Power models are appropriate for properties exhibiting fractal behaviour.')
 END SELECT

 CALL WDIALOGGETMENU(IDF_MENU2,I)
 SELECT CASE (I)
  CASE (1)
    CALL WDIALOGPUTSTRING(IDF_LABEL12,'Simple Kriging assumes a mean constant over the entire domain. The result tend to fullfill the total mean.')
  CASE (2)
    CALL WDIALOGPUTSTRING(IDF_LABEL12,'Ordinary Kriging assumes that the mean is constant in the neighborhoud of the estimated point. '// &
      'This option gives more smooth result.')
 END SELECT

 END SUBROUTINE KRIGINGSETTINGSFIELDS

 !###====================================================================
 SUBROUTINE KRIGING_LUDCMP(N,IINV)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,IINV
 INTEGER :: I,II,J,K
 REAL :: X

 L=0.0; U=0.0

 !## transform first column
 DO I=1,N; L(I,1)=A(I,1); ENDDO
 !## transform first row
 DO I=1,N; U(1,I)=A(1,I)/A(1,1); ENDDO
 DO I=1,N; U(I,I)=1.0; ENDDO
 
 DO J=2,N-1
  DO I=J,N
   X=0.0
   DO K=1,J-1
    X=X+L(I,K)*U(K,J)
   ENDDO
   L(I,J)=A(I,J)-X
  ENDDO
  DO K=J+1,N
   X=0.0
   DO I=1,J-1
    X=X+L(J,I)*U(I,K)
   ENDDO
   U(J,K)=(A(J,K)-X)/L(J,J)
  ENDDO

 ENDDO
 
 X=0.0
 DO K=1,N-1
  X=X+L(N,K)*U(K,N)
 ENDDO
 L(N,N)=A(N,N)-X
 
 !## compute inverse matrix of A called AI
 IF(IINV.EQ.1)THEN

  AI=0.0; DO II=1,N; AI(II,II)=1.0; ENDDO
  DO II=1,N

   !## forward substitution
   AI(II,1)=AI(II,1)/L(1,1)
   DO I=2,N
    X=0.0
    DO J=1,I-1
     X=X+L(I,J)*AI(II,J)
    ENDDO
    AI(II,I)=(AI(II,I)-X)/L(I,I)
   ENDDO

   !## backward substitution
   DO I=N-1,1,-1
    X=0.0
    DO J=I+1,N
     X=X+U(I,J)*AI(II,J)
    ENDDO
    AI(II,I)=AI(II,I)-X
   ENDDO
  
  ENDDO
  
 ELSE

  !## forward substitution
  B(1)=B(1)/L(1,1)
  DO I=2,N
   X=0.0
   DO J=1,I-1
    X=X+L(I,J)*B(J)
   ENDDO
   B(I)=(B(I)-X)/L(I,I)
  ENDDO

  !## backward substitution
  DO I=N-1,1,-1
   X=0.0
   DO J=I+1,N
    X=X+U(I,J)*B(J)
   ENDDO
   B(I)=B(I)-X
  ENDDO

 ENDIF
 
 END SUBROUTINE KRIGING_LUDCMP

END MODULE MOD_KRIGING