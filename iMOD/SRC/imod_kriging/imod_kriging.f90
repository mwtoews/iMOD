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
MODULE MOD_KRIGING

!## simple kriging assumes a mean constant over the entire domain
!## ordinary kriging assumes that the mean is constant in the neighborhoud of the estimated point
USE WINTERACTER
USE RESOURCE
USE MOD_DBL
USE IMODVAR
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF
USE MOD_UTL
USE MOD_OSD
USE MOD_GRAPH
USE MOD_ASC2IDF_PAR, ONLY : IDFFILE,ASSF_INDICATOR,ELLIPS_IDF,ZONE_IDF,ASSF_IDEPTH,ASSF_TOP,ASSF_BOT,ASSF_ZPLUS, &
   IINT_IDF,INT_IDF
USE MOD_INTERSECT_PAR
USE MOD_INTERSECT
USE MOD_KRIGING_PAR
USE MOD_POLYGON_UTL
USE MOD_LUDCMP, ONLY : LUDCMP_CALC

CONTAINS

 !###======================================================================
 SUBROUTINE KRIGING_MAIN(MD,XD,YD,ZD,PD,WD,IDF,IDFV,IBATCH,BO_VALUE) 
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF,IDFV
 INTEGER,INTENT(IN) :: IBATCH 
 INTEGER,INTENT(IN) :: MD 
 REAL(KIND=DP_KIND),INTENT(IN) :: BO_VALUE
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(MD) :: XD,YD,ZD,PD,WD
 INTEGER :: ND,IROW,ICOL,IRAT,IRAT1,NACTP,JROW,JCOL
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: XCD,YCD,ZCD 
 REAL(KIND=DP_KIND) :: X,Y,Z,MP,KEST,KVAR,R,Z1,Z2
 REAL(KIND=DP_KIND),DIMENSION(3) :: AGL,RAN
 REAL(KIND=DP_KIND),DIMENSION(3,3) :: ROT
 INTEGER :: I,J,MNACTP
 LOGICAL :: LT
 
! NCORESV=OMP_GET_MAX_THREADS()-1
! write(*,*) NCORESV

! call omp_set_num_threads(NCORESV)
! write(*,*) omp_get_num_procs()
! write(*,*) omp_get_max_threads()
! write(*,*) omp_get_num_threads()
 
 !## clean points for duplicates and get mean for simple kriging
 ROT=0.0D0; RAN=0.0D0
 CALL KRIGING_INIT(MD,XD,YD,ZD,PD,WD,ND,IDF,COINCIDENT,COINCIDENTDIST,IBLANKOUT,BO_VALUE,IBLNTYPE,ROT,RAN) 

 IF(IBLANKOUT.EQ.1.OR.ASSOCIATED(IXY))THEN
  IF(PNTSEARCH.EQ.0)MAXPNT=ND; PNTSEARCH=1
 ENDIF

 !## compute mean and substract values from mean - simple kriging
 IF(KTYPE.GT.0)THEN
  MP=0.0D0; DO I=1,ND; MP=MP+PD(I); ENDDO; IF(ND.GT.0.0D0)MP=MP/REAL(ND,8)
  DO I=1,ND; PD(I)=PD(I)-MP; ENDDO
 ENDIF

 !## solve system
 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Number of kriging points '//TRIM(ITOS(ND)))
 
 ALLOCATE(SELID(ND),SELD(4,MAXPNT),SELQID(4,MAXPNT),A(10,10),B(10),C(10)) 
 CALL WINDOWOUTSTATUSBAR(1,'Press ESC to terminate')
 ALLOCATE(XCD(ND),YCD(ND),ZCD(ND))

!if(idfwrite(idf,'d:\tmp.idf',1))then; endif

 IRAT=0; IRAT1=IRAT
 DO IROW=1,IDF%NROW
  MNACTP=0
  DO ICOL=1,IDF%NCOL
   !## skip locations allready filled in
   IF(IDF%X(ICOL,IROW).NE.IDF%NODATA)CYCLE 
   CALL IDFGETLOC(IDF,IROW,ICOL,X,Y)

   LT=.TRUE.
   SELECT CASE (ASSF_IDEPTH)
    !## time
    CASE (0)
     Z1=0.0D0; Z2=Z1
    !## uniform thickness
    CASE (1)
     !## depth in centimeters
     Z1=REAL(ASSF_TOP+ASSF_ZPLUS,8)
     Z2=REAL(ASSF_BOT-ASSF_ZPLUS,8)
    !## spatial thickness
    CASE (2)
     !## depth in centimeters
     Z1=IDFGETXYVAL(INT_IDF(IINT_IDF),X,Y);   IF(Z1.EQ.INT_IDF(IINT_IDF)%NODATA  )LT=.FALSE.
     Z2=IDFGETXYVAL(INT_IDF(IINT_IDF+1),X,Y); IF(Z2.EQ.INT_IDF(IINT_IDF+1)%NODATA)LT=.FALSE.
    !## absolute depth of interface
    CASE (3)
     Z1=REAL(IINT_IDF,8)
     Z2=REAL(IINT_IDF,8)
    !## thickness of interface
    CASE (4)
     Z1=-REAL(IINT_IDF,8)
     Z2=-REAL(IINT_IDF,8)
   END SELECT
   
   IF(.NOT.LT)CYCLE
   
   Z=(Z1+Z2)/2.0D0
   
   !## isotropic
   IF(ASSOCIATED(ELLIPS_IDF(1,1)%X))THEN
    !## set up rotation matrix backwards
    AGL=0.0D0; DO I=1,SIZE(ELLIPS_IDF,1); DO J=1,SIZE(ELLIPS_IDF,2)
     CALL IDFIROWICOL(ELLIPS_IDF(I,J),JROW,JCOL,X,Y)
     IF(JROW.GT.0.AND.JROW.LE.ELLIPS_IDF(I,J)%NROW.AND. &
        JCOL.GT.0.AND.JCOL.LE.ELLIPS_IDF(I,J)%NCOL)THEN
      IF(ELLIPS_IDF(I,J)%X(JCOL,JROW).NE.ELLIPS_IDF(I,J)%NODATA)THEN
       IF(J.EQ.1)THEN
        AGL(I)=ELLIPS_IDF(I,J)%X(JCOL,JROW)
        !## project reverse on major x/y/z-axis
        AGL(I)=-1.0D0*AGL(I)
        !## convert to radians
        AGL(I)=AGL(I)/(360.0D0/(2.0D0*PI))
       ELSE
        RAN(I)=ELLIPS_IDF(I,J)%X(JCOL,JROW)
       ENDIF
      ENDIF
     ENDIF
    ENDDO; ENDDO
    R=MAXVAL(RAN)
   ELSE
    R=RANGE; AGL=0.0D0; RAN=0.0D0
   ENDIF
!   IF(ELLIPS_IDF(1)%IU.NE.0)ANI=IDFGETXYVAL(ELLIPS_IDF(1),X,Y) 
!   IF(ELLIPS_IDF(2)%IU.NE.0)RAT=IDFGETXYVAL(ELLIPS_IDF(2),X,Y)
!   IF(ELLIPS_IDF(3)%IU.NE.0)R  =IDFGETXYVAL(ELLIPS_IDF(3),X,Y)
   CALL KRIGING_FILLSYSTEM(XD,XCD,YD,YCD,ZD,ZCD,PD,WD,ND,MAXPNT,X,Y,Z,R,SILL,NUGGET,KTYPE,MP,KEST,KVAR,PNTSEARCH, &
        IQUADRANT,IDF,IBLANKOUT,BO_VALUE,AGL,RAN,IBLNTYPE,NACTP)
!   if(icol.eq.126.and.irow.eq.55)then
!    write(*,*) 
!   endif        
   IDF%X(ICOL,IROW) =KEST
   !## standard deviation (L)
   IDFV%X(ICOL,IROW)=IDFV%NODATA; IF(KVAR.GE.0.0D0)IDFV%X(ICOL,IROW)=SQRT(KVAR)
   MNACTP=MAX(MNACTP,NACTP)
  ENDDO
  
  IF(IBATCH.EQ.0)THEN
   IF(KTYPE.GT.0)THEN
    CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW,IDF%NROW,'Progress Simple Kriging (~'//TRIM(ITOS(MNACTP))//') points')
   ELSE
    CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW,IDF%NROW,'Progress Ordinary Kriging (~'//TRIM(ITOS(MNACTP))//') points')
   ENDIF  
  ELSE
   WRITE(6,'(A,F10.3,A)') '+Progress ',REAL(100*IROW)/REAL(IDF%NROW),' % (~'//TRIM(ITOS(MNACTP))//') points           '
  ENDIF
 ENDDO

 IF(ALLOCATED(XLAG))DEALLOCATE(XLAG); IF(ALLOCATED(ZLAG))DEALLOCATE(ZLAG)
 IF(ASSOCIATED(XY))DEALLOCATE(XY);    IF(ASSOCIATED(IXY))DEALLOCATE(IXY)
 IF(ASSOCIATED(FFCT))DEALLOCATE(FFCT)
 
 IF(ALLOCATED(SELID)) DEALLOCATE(SELID)
 IF(ALLOCATED(SELD))  DEALLOCATE(SELD)
 IF(ALLOCATED(SELQID))DEALLOCATE(SELQID)
 IF(ALLOCATED(A))     DEALLOCATE(A)
 IF(ALLOCATED(B))     DEALLOCATE(B)
 IF(ALLOCATED(C))     DEALLOCATE(C)
 IF(ALLOCATED(AI))    DEALLOCATE(AI)
 IF(ALLOCATED(XCD))   DEALLOCATE(XCD)
 IF(ALLOCATED(YCD))   DEALLOCATE(YCD)
 IF(ALLOCATED(ZCD))   DEALLOCATE(ZCD)
  
 END SUBROUTINE KRIGING_MAIN

 !###======================================================================
 SUBROUTINE UTL_SETUP_ROTATIONMATRIX(R,ROT)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(3) :: R
 REAL(KIND=DP_KIND),DIMENSION(3,3),INTENT(OUT) :: ROT
 REAL(KIND=DP_KIND) :: A,B,C
 
 A=R(1); B=R(2); C=R(3)

 ROT(1,1)= COS(A)*COS(B)
 ROT(2,1)=(COS(A)*SIN(B)*SIN(C))-(SIN(A)*COS(C))
 ROT(3,1)=(COS(A)*SIN(B)*COS(C))+(SIN(A)*SIN(C))
 
 ROT(1,2)= SIN(A)*COS(B)
 ROT(2,2)=(SIN(A)*SIN(B)*SIN(C))+(COS(A)*COS(C))
 ROT(3,2)=(SIN(A)*SIN(B)*COS(C))-(COS(A)*SIN(C))
 
 ROT(1,3)=-SIN(B)
 ROT(2,3)=(COS(B)*SIN(C))
 ROT(3,3)=(COS(B)*COS(C))
 
 END SUBROUTINE UTL_SETUP_ROTATIONMATRIX

 !###======================================================================
 SUBROUTINE KRIGING_READGEN(ILAY,NGEN,GENFNAME,FGEN,GENILAY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ILAY,NGEN
 CHARACTER(LEN=*),INTENT(IN),DIMENSION(NGEN) :: GENFNAME
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(NGEN) :: FGEN
 INTEGER,INTENT(IN),DIMENSION(NGEN) :: GENILAY
 REAL(KIND=DP_KIND) :: XMIN,YMIN,XMAX,YMAX
 INTEGER :: IU,IOS,MAXP,MAXN,I,J,K,N,M,IGEN,IFORMAT,MAXPOL,MAXCOL,STRLEN,ITYPE,NP
 CHARACTER(LEN=12) :: CID
 CHARACTER(LEN=11),ALLOCATABLE,DIMENSION(:) :: COLNAMES
 INTEGER,ALLOCATABLE,DIMENSION(:) :: COLWIDTH
 TYPE LBLOBJ
  CHARACTER(LEN=:),ALLOCATABLE :: STRING
 END TYPE LBLOBJ
 TYPE(LBLOBJ),DIMENSION(:),ALLOCATABLE :: LBL
 
 IF(NGEN.LE.0)RETURN
 
 !## store coordinaten per line i nxy and ixy
 MAXP=0; MAXN=0

 !## read polygon in GENfile is available
 N=0; M=0
 DO IGEN=1,NGEN

  !## include proper layers only
  IF(GENILAY(IGEN).NE.ILAY)CYCLE
  IF(LEN_TRIM(GENFNAME(IGEN)).EQ.0)CYCLE
  IF(.NOT.POLYGON_UTL_OPENGEN(GENFNAME(IGEN),IFORMAT,IU))CYCLE
  
  IF(IFORMAT.EQ.0)THEN
  
   !## read overall extent of polygons
   READ(IU) XMIN,YMIN,XMAX,YMAX
   READ(IU) MAXPOL,MAXCOL
   !## labels available
   IF(MAXCOL.GT.0)THEN
    ALLOCATE(COLWIDTH(MAXCOL),LBL(MAXCOL),COLNAMES(MAXCOL))
    READ(IU) (COLWIDTH(I),I=1,MAXCOL)
    DO I=1,MAXCOL; STRLEN=COLWIDTH(I); ALLOCATE(CHARACTER(LEN=STRLEN) :: LBL(I)%STRING); ENDDO
    READ(IU) (COLNAMES(I),I=1,MAXCOL)
   ENDIF

   N=N+MAXPOL

   !## increase memory
   IF(N.GT.MAXN)THEN
    ALLOCATE(IXYDUMMY(N)); DO I=1,MAXN; IXYDUMMY(I)=IXY(I); ENDDO
    IF(ASSOCIATED(IXY))DEALLOCATE(IXY); IXY=>IXYDUMMY
    ALLOCATE(FFCTDUMMY(N)); DO I=1,MAXN; FFCTDUMMY(I)=FFCT(I); ENDDO
    IF(ASSOCIATED(FFCT))DEALLOCATE(FFCT); FFCT=>FFCTDUMMY; MAXN=N
   ENDIF

   N=N-MAXPOL
   
   DO J=1,MAXPOL
    READ(IU) NP,ITYPE
    !## read labels
    IF(MAXCOL.GT.0)READ(IU) (LBL(I)%STRING,I=1,MAXCOL)

    M=M+NP
    
    !## increase memory
    IF(M.GT.MAXP)THEN
     ALLOCATE(XYDUMMY(M,2)); DO I=1,MAXP; DO K=1,2; XYDUMMY(I,K)=XY(I,K); ENDDO; ENDDO
     IF(ASSOCIATED(XY))DEALLOCATE(XY); XY=>XYDUMMY; MAXP=M
    ENDIF

    READ(IU) XMIN,YMIN,XMAX,YMAX
    READ(IU) (XY(I,1),XY(I,2),I=(M-NP)+1,M)

    N=N+1
    IXY(N)=M
    FFCT(N)=FGEN(IGEN)
    
   ENDDO
  
   IF(ALLOCATED(COLWIDTH))DEALLOCATE(COLWIDTH)
   IF(ALLOCATED(COLNAMES))DEALLOCATE(COLNAMES)
   IF(ALLOCATED(LBL))THEN; DO I=1,MAXCOL; DEALLOCATE(LBL(I)%STRING); ENDDO; DEALLOCATE(LBL); ENDIF
  
  ELSEIF(IFORMAT.EQ.1)THEN

   DO

    !## header
    READ(IU,'(A)',IOSTAT=IOS) CID; IF(IOS.NE.0)EXIT
    IF(TRIM(UTL_CAP(CID,'U')).EQ.'END')EXIT
   
    N=N+1

    !## increase memory
    IF(N.GT.MAXN)THEN
     ALLOCATE(IXYDUMMY(MAXN+10)); DO I=1,MAXN; IXYDUMMY(I)=IXY(I); ENDDO
     IF(ASSOCIATED(IXY))DEALLOCATE(IXY); IXY=>IXYDUMMY
     ALLOCATE(FFCTDUMMY(MAXN+10)); DO I=1,MAXN; FFCTDUMMY(I)=FFCT(I); ENDDO
     IF(ASSOCIATED(FFCT))DEALLOCATE(FFCT); FFCT=>FFCTDUMMY; MAXN=MAXN+10
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
    M=M-1; IXY(N)=M; FFCT(N)=FGEN(IGEN)

   END DO
 
   CLOSE(IU)
  ENDIF
  
 ENDDO
 
 IF(M.GT.0)THEN
 
  !## fix vector length to 
  IF(N.LT.MAXN)THEN
   ALLOCATE(IXYDUMMY(N));  DO I=1,N; IXYDUMMY(I)=IXY(I);   ENDDO; DEALLOCATE(IXY);  IXY =>IXYDUMMY
   ALLOCATE(FFCTDUMMY(N)); DO I=1,N; FFCTDUMMY(I)=FFCT(I); ENDDO; DEALLOCATE(FFCT); FFCT=>FFCTDUMMY
  ENDIF
  IF(M.LT.MAXP)THEN
   ALLOCATE(XYDUMMY(M,2)); DO I=1,M; DO J=1,2; XYDUMMY(I,J)=XY(I,J); ENDDO; ENDDO; DEALLOCATE(XY); XY=>XYDUMMY
  ENDIF
 ELSE
  DEALLOCATE(IXY,XY,FFCT)
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
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(NDD) :: XD,YD,ZD
 REAL(KIND=DP_KIND),INTENT(IN),OPTIONAL :: LAGDISTANCE
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: DVG,ZVG
 REAL(KIND=DP_KIND) :: NLAG,VAR,MEAN,LAGDIST,DX,DY,MZ
 INTEGER :: I,J,K,IU,NPOP,LAGINT,IRAT,IRAT1,IS,IOS,ITYPE,IEXIT
 INTEGER(KIND=8) :: N,NN
 TYPE(WIN_MESSAGE) :: MESSAGE
 
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
 MZ=0.0D0; DO I=1,ND; MZ=MZ+ZD(I); ENDDO; MZ=MZ/REAL(ND)
 DO I=1,ND; ZD(I)=ZD(I)-MZ; ENDDO
  
 DVG=0.0D0; ZVG=0.0D0; K=0
 DO I=1,ND,IS
  DO J=I+1,ND,IS
   DX=(XD(I)-XD(J))**2.0D0; DY=(YD(I)-YD(J))**2.0D0
   IF(DX+DY.GT.0.0D0)THEN
    K=K+1
    DVG(K)=SQRT(DX+DY)
    ZVG(K)=(ZD(I)-ZD(J))**2.0D0
   ENDIF
  ENDDO
 ENDDO
 
 !## laginterval=number of intervals
 !## lagdistance=size of interval
 IF(PRESENT(LAGDISTANCE).AND.PRESENT(LAGINTERVAL))THEN
  LAGDIST=LAGDISTANCE
  LAGINT=LAGINTERVAL
 ELSE
  CALL UTL_STDEF(DVG,K,-999.99D0,VAR,MEAN,NPOP)
  LAGINT=20
  LAGDIST=(MEAN+(1.0D0*VAR))/LAGINT
 ENDIF

 IF(ALLOCATED(XLAG))DEALLOCATE(XLAG)
 IF(ALLOCATED(ZLAG))DEALLOCATE(ZLAG)
  
 ALLOCATE(ZLAG(0:LAGINT),XLAG(0:LAGINT)) 
 XLAG(0)=0.0D0; ZLAG=0.0D0
 
 !## compute semivariogram
 IRAT=0; IRAT1=IRAT
 DO I=1,LAGINT
  XLAG(I)=XLAG(I-1)+LAGDIST; NLAG=0.0D0
  DO J=1,N 
   IF(DVG(J).GE.XLAG(I-1).AND.DVG(J).LT.XLAG(I))THEN
    NLAG   =NLAG   +1.0D0
    ZLAG(I)=ZLAG(I)+ZVG(J)
   ENDIF
  ENDDO
  IF(NLAG.GT.0.0D0)ZLAG(I)=ZLAG(I)/(2.0*NLAG)
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
  GRAPHDIM(1)%GRAPHNAMES=''; GRAPHDIM(1)%IFIXX=0; GRAPHDIM(1)%IFIXY=0; GRAPHDIM(1)%XTITLE='Distance'; GRAPHDIM(1)%YTITLE='Semivariance (m2)'; GRAPHDIM%LDATE=.FALSE.
  GRAPHDIM(1)%IGROUP=1; GRAPHDIM(1)%TEXTSIZE=5.0D0
  CALL GRAPH_INIT(3)
  DO
   CALL WMESSAGE(ITYPE,MESSAGE)
   CALL GRAPH_MAIN(ITYPE,MESSAGE,IEXIT=IEXIT)
   IF(IEXIT.EQ.1)EXIT
  ENDDO
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
 SUBROUTINE KRIGING_FILLSYSTEM(XD,XCD,YD,YCD,ZD,ZCD,PD,WD,ND,MAXPNT,X,Y,Z,RANGE,SILL,NUGGET, &
          KTYPE,MP,KEST,KVAR,PNTSEARCH,IQUADRANT,IDF,IBLANKOUT,BO_VALUE,AGL,RAN,IBLNTYPE,NACTP)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: ND,MAXPNT,KTYPE,PNTSEARCH,IQUADRANT,IBLANKOUT,IBLNTYPE
 INTEGER,INTENT(OUT) :: NACTP
 REAL(KIND=DP_KIND),INTENT(OUT) :: KEST,KVAR
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y,Z,SILL,NUGGET,MP,RANGE,BO_VALUE
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(ND) :: XD,YD,ZD,PD,WD
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(ND) :: XCD,YCD,ZCD
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(3) :: RAN,AGL
 INTEGER :: I,J,NP,N,ID,JD,IQ,ICMPINV,IU
 REAL(KIND=DP_KIND) :: DXY,GAMMA,MPP,USERANGE,C0,C1,NODATA
 REAL(KIND=DP_KIND),DIMENSION(3,3) :: ROT
 
 NODATA=IDF%NODATA
 
 !## copy points 
 XCD=XD; YCD=YD; ZCD=ZD
 
 !## if 2d interpolation, skip zcd
 IF(RAN(3).EQ.0.0D0)ZCD=Z

 !## check if location coincides perfectly with point than take that value and return
 DO ID=1,ND
  IF(UTL_DIST_3D(XCD(ID),YCD(ID),ZCD(ID),X,Y,Z).EQ.0.0D0)THEN
   KEST=PD(ID)
   KVAR=0.0D0
   RETURN
  ENDIF
 ENDDO
 
 !## set up rotation matrice
 CALL UTL_SETUP_ROTATIONMATRIX(AGL,ROT)
 
 USERANGE=RANGE
 C0      =NUGGET
 C1      =SILL-NUGGET
 
 !## already computed inverse of A
 ICMPINV=0; IF(ALLOCATED(AI))ICMPINV=1

 IF(PNTSEARCH.EQ.1)THEN
  
  !## maximal search is MAXPNT - get them
  SELQID=0; SELD=HUGE(1.0D0)
  DO ID=1,ND
   !## get distance between points - remove whenever intersected by faults
   DXY=KRIGING_DIST(XCD(ID),YCD(ID),ZCD(ID),X,Y,Z,WD(ID),IDF,IBLANKOUT,BO_VALUE,USERANGE,IBLNTYPE,ROT,RAN)
   !## otherside of fault or distance too big, take next
   IF(DXY.GT.USERANGE)CYCLE

   !## determine quadrant
   IQ=1; IF(IQUADRANT.EQ.1)IQ=KRIGING_QUADRANT(XCD(ID),YCD(ID),X,Y)

   !## not in current list
   IF(DXY.GE.SELD(IQ,MAXPNT))CYCLE
   !## get position in sort
   DO I=1,MAXPNT; IF(DXY.LT.SELD(IQ,I))EXIT; ENDDO
   !## make place for new least-distance point
   DO J=MAXPNT,I+1,-1; SELQID(IQ,J)=SELQID(IQ,J-1); SELD(IQ,J)=SELD(IQ,J-1); ENDDO
   !## put new location in nearest list
   SELQID(IQ,I)=ID; SELD(IQ,I)=DXY
  ENDDO
  
  SELID=0; NP=0 
  DO IQ=1,1+(IQUADRANT*3)
   DO J=1,MAXPNT
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
 IF(NP.LE.0)THEN; KEST=NODATA; KVAR=0.0D0; NACTP=NP; RETURN; ENDIF
  
 !## save points
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE='d:\IMOD-MODELS\TKI_SLR\results\pospunt.ipf',STATUS='UNKNOWN',ACTION='WRITE')
 WRITE(IU,'(A)') 'X,Y,Z,PD,ID'; WRITE(IU,'(4f15.3,i10)') X,Y,0.0D0,0.0D0,0
 DO I=1,NP
  ID=SELID(I)
  WRITE(IU,'(4F15.3,I10)') XCD(ID),YCD(ID),ZCD(ID),PD(ID),ID
 ENDDO
 CLOSE(IU)
 CALL OSD_OPEN(IU,FILE='d:\IMOD-MODELS\TKI_SLR\results\orgpunt.ipf',STATUS='UNKNOWN',ACTION='WRITE')
 WRITE(IU,'(A)') 'X,Y,Z,P,ID'; WRITE(IU,'(4f15.3,i10)') X,Y,0.0D0,0.0D0,0
 DO I=1,NP
  ID=SELID(I)
  WRITE(IU,'(4F15.3,I10)') XD(ID),YD(ID),ZD(ID),PD(ID),ID
 ENDDO
 CLOSE(IU)

 !## simple kriging (ktype.gt.0) and ordinary kriging (ktype.lt.0)
 N=NP; IF(KTYPE.LT.0)N=NP+1
 
 IF(ICMPINV.EQ.0)THEN

  IF(N.NE.SIZE(B))THEN
   DEALLOCATE(A,B,C); ALLOCATE(A(N,N),B(N),C(N)) 
  ENDIF
 
  !## fill in A()
  A=0.0D0
  DO I=1,NP 
   ID=SELID(I)
   !## semivariance
   A(I,I)=SILL 
   DO J=I+1,NP 
    JD=SELID(J)
    GAMMA=KRIGING_GETGAMMA(XCD(ID),YCD(ID),ZCD(ID),XCD(JD),YCD(JD),ZCD(JD),USERANGE,C1,C0,KTYPE)
    A(I,J)=SILL-GAMMA; A(J,I)=A(I,J)
   ENDDO 
  ENDDO
  !## ordinary kriging, fill for lambda
  IF(KTYPE.LT.0)THEN
   DO I=1,N; A(I,N)=1.0D0; A(N,I)=1.0D0; ENDDO; A(N,N)=0.0D0
  ENDIF
 
 ENDIF
 
 !## fill in B()
 B=0.0D0
 DO I=1,NP
  ID=SELID(I)
  GAMMA=KRIGING_GETGAMMA(XCD(ID),YCD(ID),ZCD(ID),X,Y,Z,USERANGE,C1,C0,KTYPE)
  B(I)=SILL-GAMMA  
 ENDDO
 
 !## ordinary kriging, fill for lambda
 IF(KTYPE.LT.0)B(N)=1.0D0
 
 IF(ICMPINV.EQ.0)THEN
  IF(ABS(PNTSEARCH-1).EQ.0)THEN
   CALL LUDCMP_CALC(N,SIZE(A,1),A,B=B) 
  ELSE
   IF(PNTSEARCH.EQ.0)ALLOCATE(AI(N,N)) 
   CALL LUDCMP_CALC(N,SIZE(A,1),A,AI=AI) 
  ENDIF
 ENDIF
 
 IF(PNTSEARCH.EQ.0)THEN
!  CALL UTL_MATMUL(AI,B,C); B=C
  C=MATMUL(AI,B); B=C
 ENDIF
 
 !## ordinary kriging compute the local mean mzz instead of the global mean mz
 IF(KTYPE.LT.0)THEN
  MPP=0.0D0; DO I=1,NP; ID=SELID(I); MPP=MPP+PD(ID); ENDDO; MPP=MPP/REAL(NP,8)
 ENDIF

 KEST=0.0D0
 DO I=1,NP 
  ID=SELID(I)
  IF(KTYPE.GT.0)THEN
   KEST=KEST+B(I)* PD(ID)
  ELSE  
   KEST=KEST+B(I)*(PD(ID)-MPP)
  ENDIF
 ENDDO
 !## global mean (simple kriging)
 IF(KTYPE.GT.0)KEST=KEST+MP
 !## local mean (ordinary kriging)
 IF(KTYPE.LT.0)KEST=KEST+MPP
 
 !## standard deviation
 KVAR=0.0D0
 DO I=1,NP
  ID=SELID(I)
  GAMMA=KRIGING_GETGAMMA(XCD(ID),YCD(ID),ZCD(ID),X,Y,Z,USERANGE,C1,C0,KTYPE)
  KVAR=KVAR+B(I)*(SILL-GAMMA) 
 ENDDO
 KVAR=SILL-KVAR
 
 !## actual points used
 NACTP=NP
 
 END SUBROUTINE KRIGING_FILLSYSTEM

 !###======================================================================
 INTEGER FUNCTION KRIGING_QUADRANT(XD,YD,X,Y)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: XD,YD,X,Y
 
 IF(XD.GT.X)THEN
  KRIGING_QUADRANT=1; IF(YD.LT.Y)KRIGING_QUADRANT=2
 ELSE
  KRIGING_QUADRANT=4; IF(YD.LT.Y)KRIGING_QUADRANT=3
 ENDIF
 
 END FUNCTION KRIGING_QUADRANT

 !###======================================================================
 REAL(KIND=DP_KIND) FUNCTION KRIGING_DIST(X1,Y1,Z1,X0,Y0,Z0,W,IDF,IBLANKOUT,BO_VALUE, &
             MAXDIST,IBLNTYPE,ROT,RAN)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: IBLANKOUT
 INTEGER,INTENT(IN) :: IBLNTYPE
 REAL(KIND=DP_KIND),DIMENSION(3),INTENT(IN) :: RAN
 REAL(KIND=DP_KIND),DIMENSION(3,3),INTENT(IN) :: ROT
 REAL(KIND=DP_KIND),INTENT(INOUT) :: X1,Y1,Z1
 REAL(KIND=DP_KIND),INTENT(IN) :: X0,Y0,Z0,BO_VALUE,MAXDIST,W
 REAL(KIND=DP_KIND) :: X3,Y3,X4,Y4,XINTER,YINTER,A,SSX,DX,DY,DZ,D,R,AXY,AZX,DXY,D1,D2,F
 INTEGER :: I,J,I1,I2,ISTATUS,N,IROW,ICOL,IPOL1,IPOL2
 REAL(KIND=DP_KIND),DIMENSION(3) :: XYZC,XYZE
 
 KRIGING_DIST=UTL_DIST_3D(X1,Y1,Z1,X0,Y0,Z0)

 !## change position of point temporarily
 IF(W.NE.0.0D0)THEN
  X1=X0+W*(X1-X0); Y1=Y0+W*(Y1-Y0); Z1=Z0+W*(Z1-Z0)
  KRIGING_DIST=UTL_DIST_3D(X1,Y1,Z1,X0,Y0,Z0)
 ENDIF
 
 !## to far away (include 1.0D0*maxdist on edge - for smooth edges)
 IF(KRIGING_DIST.GE.MAXDIST)RETURN
 IF(KRIGING_DIST.LE.0.0D0)RETURN

 !## see whether nodata-areas are crossed/similar area
 IF(IBLANKOUT.EQ.1)THEN

  !## include in case point can be "seen" - fault method
  IF(IBLNTYPE.EQ.0)THEN

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
      KRIGING_DIST=MAXDIST+1.0D0
      EXIT
     ENDIF
    ENDIF
    SSX=SSX+IDF%DX; IF(SSX.GT.D)EXIT
   ENDDO

  !## include in case of a similar area
  ELSEIF(IBLNTYPE.EQ.1)THEN

   CALL IDFIROWICOL(IDF,IROW,ICOL,X0,Y0); X3=IDF%X(ICOL,IROW)
   CALL IDFIROWICOL(IDF,IROW,ICOL,X1,Y1); X4=IDF%X(ICOL,IROW)
   !## not similar area
   IF(X3.EQ.BO_VALUE.OR.X4.EQ.BO_VALUE)KRIGING_DIST=MAXDIST+1.0D0

  ENDIF
 ENDIF
 
 !## including a genfile
 IF(ASSOCIATED(IXY))THEN

  !## process as a fault
  IF(IBLNTYPE.EQ.0)THEN

   DO J=1,SIZE(IXY) 
    !## see whether point intersect a fault
    I1=1; IF(J.GT.1)I1=IXY(J-1)+1; I2=IXY(J)
    DO I=I1+1,I2
     X3=XY(I-1,1); Y3=XY(I-1,2)
     X4=XY(I  ,1); Y4=XY(I  ,2)
     CALL DBL_IGRINTERSECTLINE(X1,Y1,X0,Y0,X3,Y3,X4,Y4,XINTER,YINTER,ISTATUS)
     !## if line intersect, increase distance as a penalty
     IF(ISTATUS.EQ.5)THEN
      IF(FFCT(J).EQ.0.0D0)THEN
       KRIGING_DIST=MAXDIST+1.0D0
      ELSE
       KRIGING_DIST=KRIGING_DIST*FFCT(J)
       !## change position of point temporarily
       X1=X0+KRIGING_DIST*(X1-X0)
       Y1=Y0+KRIGING_DIST*(Y1-Y0)
      ENDIF
      RETURN
     ENDIF
    ENDDO
   ENDDO

  !## process as a polygon
  ELSEIF(IBLNTYPE.EQ.1)THEN
   
   !## what polygon is x0,y0 ?
   DO J=1,SIZE(IXY)
    I1=1; IF(J.GT.1)I1=IXY(J-1); I2=IXY(J); N=I2-I1+1
    IF(DBL_IGRINSIDEPOLYGON(X0,Y0,XY(I1:I2,1),XY(I1:I2,2),N).EQ.1)EXIT
   ENDDO
   IPOL1=J; IF(IPOL1.GT.SIZE(IXY))IPOL1=0
   
   !## what polygon is x1,y1
   DO J=1,SIZE(IXY)
    I1=1; IF(J.GT.1)I1=IXY(J-1); I2=IXY(J); N=I2-I1+1
    IF(DBL_IGRINSIDEPOLYGON(X1,Y1,XY(I1:I2,1),XY(I1:I2,2),N).EQ.1)EXIT
   ENDDO
   IPOL2=J; IF(IPOL2.GT.SIZE(IXY))IPOL2=0

   IF(IPOL1.NE.IPOL2)KRIGING_DIST=MAXDIST+1.0D0
   
  ENDIF
  
 ENDIF
 
 !## including a zone idf
 IF(ZONE_IDF%IU.NE.0)THEN

  X3=IDFGETXYVAL(ZONE_IDF,X0,Y0) 
  X4=IDFGETXYVAL(ZONE_IDF,X1,Y1) 
  !## not similar area
  IF(X3.NE.X4)KRIGING_DIST=MAXDIST+1.0D0
  !## skip locations with nodata
  IF(X3.EQ.ZONE_IDF%NODATA.OR.X4.EQ.ZONE_IDF%NODATA)KRIGING_DIST=MAXDIST+1.0D0
  
 ENDIF

 !## in case of an ellips(oid) see if it is inside the current ellipsoid
 IF(.NOT.UTL_CHECK_UNITY(ROT))THEN
  IF(.NOT.UTL_INSIDEELLIPSOID(X0,Y0,Z0,X1,Y1,Z1,ROT,RAN))THEN

  !## outside ellipsoid, ignore point
   KRIGING_DIST=MAXDIST+1.0D0

  ELSE

  !## http://spatial-analyst.net/ILWIS/htm/ilwisapp/anisotropic_kriging_algorithm.htm  
   !## compute coordinates on sphere with max. range
   R=MAXVAL(RAN)
   
   !## backwards rotate points first
   XYZE(1)=X1-X0
   XYZE(2)=Y1-Y0
   XYZE(3)=Z1-Z0
   XYZE=MATMUL(XYZE,ROT)
!   X1=XYZE(1)
!   Y1=XYZE(2)
!   Z1=XYZE(3)

   !!## find angle for point
   !DY=(Y0-Y1); DX=(X0-X1); DZ=(Z0-Z1)
   !DXY=UTL_DIST(X0,Y0,X1,Y1)
   !AXY=ATAN2(DY,DX)
   !AZX=ATAN2(DZ,DXY)
   !
   !!## find point on sphere for those angles
   !XYZC(1)=R*SIN(AZX)*COS(AXY)
   !XYZC(2)=R*SIN(AZX)*SIN(AXY)
   !XYZC(3)=R*COS(AZX)
   !
   !!## find point on ellipsoid
   !XYZE(1)=RAN(1)*SIN(AZX)*COS(AXY)
   !XYZE(2)=RAN(2)*SIN(AZX)*SIN(AXY)
   !XYZE(3)=RAN(3)*COS(AZX)
   !
   !!## ratio is
   !D1=UTL_DIST_3D(XYZC(1),XYZC(2),XYZC(3),X0,Y0,Z0)
   !D2=UTL_DIST_3D(XYZE(1),XYZE(2),XYZE(3),X0,Y0,Z0)
   !
   !!## ratio is
   !F=D1/D2
   !
   !!## distance of original point to centre, increase distance with f
   !R=F*UTL_DIST_3D(X0,Y0,Z0,X1,Y1,Z1)
   !
   !!## compute new point in sphere at correct distance
   !XYZC(1)=R*SIN(AZX)*COS(AXY)
   !XYZC(2)=R*SIN(AZX)*SIN(AXY)
   !XYZC(3)=R*COS(AZX)
   !
   !!## rotate them appropriately
   !XYZE=MATMUL(XYZC,ROT)

   X1=X0+XYZE(1)
   R=1.0D0; IF(RAN(2).NE.0.0D0)R=RAN(1)/RAN(2); Y1=Y0+XYZE(2)*R
   R=1.0D0; IF(RAN(3).NE.0.0D0)R=RAN(1)/RAN(3); Z1=Z0+XYZE(3)*R
   
!  A=-(ANI+90.0D0)/(360.0D0/(2.0D0*PI))
!  X1=           COS(A)*DX+          SIN(A)*DY
!  Y1=-1.0D0/RAT*SIN(A)*DX+1.0D0/RAT*COS(A)*DY
!  X1=X0+X1
!  Y1=Y0+Y1
   
   KRIGING_DIST=UTL_DIST_3D(X1,Y1,Z1,X0,Y0,Z0)
 ! 
  ENDIF
 ENDIF
  
 END FUNCTION KRIGING_DIST

 !###======================================================================
 FUNCTION UTL_CHECK_UNITY(A) RESULT (LUNITY)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(:,:) :: A
 LOGICAL :: LUNITY
 INTEGER :: I,J
 
 !## false unless proven otherwise
 LUNITY=.FALSE.
 
 IF(SIZE(A,1).EQ.SIZE(A,2))THEN
  DO I=1,SIZE(A,1); DO J=1,SIZE(A,2)
   IF(I.EQ.J)THEN
    IF(A(I,J).NE.1.0D0)RETURN
   ELSE
    IF(A(I,J).NE.0.0D0)RETURN
   ENDIF
  ENDDO; ENDDO
 ENDIF
 
 LUNITY=.TRUE.

 END FUNCTION UTL_CHECK_UNITY

 !###======================================================================
 REAL(KIND=DP_KIND) FUNCTION KRIGING_GETGAMMA(X1,Y1,Z1,X2,Y2,Z2,RANGE,C1,C0,KTYPE)  !c1=sill c0=nugget
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: KTYPE
 REAL(KIND=DP_KIND),INTENT(IN) :: X1,Y1,Z1,X2,Y2,Z2,RANGE,C1,C0
 REAL(KIND=DP_KIND) :: DXY,H
 
 DXY=UTL_DIST_3D(X1,Y1,Z1,X2,Y2,Z2)
 
 IF(DXY.GE.RANGE)THEN
  H=1.0D0 
 ELSE

  !## no part of kriging, beyond given range, equal to sill
  SELECT CASE (ABS(KTYPE))
   CASE (1) !## linear
    H=DXY/RANGE
   CASE (2) !## spherical
    H=(3.0D0*DXY)/(2.0D0*RANGE)-(0.5D0*(DXY/RANGE)**3.0D0)
   CASE (3) !## exponential
    H=1.0D0-10.0D0**(-3.0D0*(DXY/RANGE))
   CASE (4) !## gaussian
    H=1.0D0-10.0D0**(-3.0D0*(DXY**2.0D0)/(RANGE**2.0D0))
   CASE (5) !## power
    H=DXY**0.5D0
   CASE DEFAULT
    WRITE(*,*) 'UNKNOWN KTYPE',KTYPE; PAUSE; STOP
  END SELECT
 ENDIF
 
 KRIGING_GETGAMMA=C0+C1*H
 
 END FUNCTION KRIGING_GETGAMMA

 !###======================================================================
 SUBROUTINE KRIGING_INIT(MD,XD,YD,ZD,PD,WD,ND,IDF,COINCIDENT,COINCIDENTDIST, &
    IBLANKOUT,BO_VALUE,IBLNTYPE,AGL,RAN)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: MD,COINCIDENT,IBLANKOUT,IBLNTYPE
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(3) :: RAN,AGL
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(MD) :: XD,YD,ZD,PD,WD
 REAL(KIND=DP_KIND),INTENT(IN) :: COINCIDENTDIST,BO_VALUE
 INTEGER,INTENT(OUT) :: ND
 INTEGER :: I,J,N
 REAL(KIND=DP_KIND) :: XYCRIT,XP,YP,ZP,PP,WP,W
 REAL(KIND=DP_KIND),DIMENSION(3,3) :: ROT
 
 XYCRIT=0.1D0*IDF%DX

 !## apply a user specified critical distance
 IF(COINCIDENT.EQ.1)XYCRIT=MAX(COINCIDENTDIST,XYCRIT)
 
 !## mark doubles with nodata and compute mean for those double points
 DO I=1,MD
  !## done allready
  IF(PD(I).EQ.IDF%NODATA)CYCLE
  N=1; XP=XD(I); YP=YD(I); ZP=ZD(I); PP=PD(I); WP=WD(I)
  DO J=I+1,MD
   !## done allready
   IF(PD(J).EQ.IDF%NODATA)CYCLE
   !## irrelevant here - this is only used to thinout point which are too near
   W=0.0D0
   !## get distance between points - increase distance whenever points are intersected by fault - is only meant to aggregate if too close
   IF(KRIGING_DIST(XD(I),YD(I),ZD(I),XD(J),YD(J),ZD(J),W,IDF,IBLANKOUT,BO_VALUE,XYCRIT,IBLNTYPE,ROT,RAN).LE.XYCRIT)THEN
    N=N+1

    !## take maximal indicator value
    IF(ASSF_INDICATOR.EQ.1)THEN
     PP=MAX(PP,PD(J))
    !## take average value
    ELSE
     PP=PP+PD(J)
    ENDIF

    !## add point to existing point and turn location off
    XP=XP+XD(J); YP=YP+YD(J); ZP=ZP+ZD(J); PD(J)=IDF%NODATA; WP=WP+WD(J)
   ENDIF
  ENDDO
  !## determine average spot
  XD(I)=XP/REAL(N,8); YD(I)=YP/REAL(N,8); ZD(I)=ZP/REAL(N,8); WD(I)=WP/REAL(N,8)

  IF(ASSF_INDICATOR.EQ.1)THEN
   PD(I)=PP
  ELSE
   PD(I)=PP/REAL(N,8)
  ENDIF

 END DO
  
 !## eliminate doubles
 J=0
 DO I=1,MD
  IF(PD(I).NE.IDF%NODATA)THEN
   J=J+1
   IF(J.NE.I)THEN
    XD(J)=XD(I); YD(J)=YD(I); ZD(J)=ZD(I); PD(J)=PD(I); WD(J)=WD(I)
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
 INTEGER :: NACTP
 REAL(KIND=DP_KIND),DIMENSION(MD) :: XD,YD,ZD,PD,WD,XCD,YCD,ZCD !,PCD
 REAL(KIND=DP_KIND),DIMENSION(3) :: RAN,AGL
 DATA XD/100.0D0, 50.0D0,250.0D0/
 DATA YD/100.0D0,250.0D0,200.0D0/
 DATA ZD/  0.0D0,  0.0D0,  0.0D0/
 DATA PD/5.0D0  , 20.0D0, 32.0D0/
 DATA WD/1.0D0  ,  1.0D0,  1.0D0/
! DATA XD/2.0,3.0,9.0,6.0,5.0/
! DATA YD/2.0,7.0,9.0,5.0,3.0/
! DATA ZD/3.0,4.0,2.0,4.0,6.0/
! DATA XD/825.0 ,2700.0D0,3700.0D0,2300.0D0,825.0 ,500.0D0/
! DATA YD/3700.0D0,4300.0D0,5200.0D0,5700.0D0,5150.0D0,4950.0D0/
! DATA ZD/13.84 ,12.15 ,12.87 ,12.68 ,14.41 ,14.59/
 REAL(KIND=DP_KIND) :: RANGE,NUGGET,SILL,MP,X,Y,Z,KEST,KVAR,LAGDISTANCE,COINCIDENTDIST
 INTEGER :: MAXPNT,KTYPE,IROW,ICOL,I,PNTSEARCH,ND,NR,NC,MX,LAGINTERVAL,COINCIDENT,IQUADRANT
 TYPE(IDFOBJ) :: IDF,IDFV
 
 AGL=0.0D0; RAN=1.0D0

 IDF%DX=1.0D0; IDF%DY=1.0D0
 IDF%XMIN=MINVAL(XD)-5.0D0; IDF%XMAX=MAXVAL(XD)+5.0D0
 IDF%YMIN=MINVAL(YD)-5.0D0; IDF%YMAX=MAXVAL(YD)+5.0D0
 IDF%NCOL=(IDF%XMAX-IDF%XMIN)/IDF%DX; IDF%NROW=(IDF%YMAX-IDF%YMIN)/IDF%DY
 IF(.NOT.IDFALLOCATEX(IDF))RETURN
 CALL IDFCOPY(IDF,IDFV)

! MAXPNT=MD; RANGE=4141.0D0; NUGGET=0.01D0; SILL=0.78; KTYPE=2; PNTSEARCH=0; NR=0; NC=0
! LAGINTERVAL=20; LAGDISTANCE=RANGE/REAL(LAGINTERVAL); COINCIDENT=0; IQUADRANT=0

 MAXPNT=MD; RANGE=400.0D0; NUGGET=4.0; SILL=9.0; KTYPE=-2; PNTSEARCH=0; NR=0; NC=0
 LAGINTERVAL=20; LAGDISTANCE=RANGE/REAL(LAGINTERVAL); COINCIDENT=0; IQUADRANT=0; COINCIDENT=0.0D0
 MX=RANGE/IDF%DX
 
! SILL=SILL-NUGGET
  
! sill=0.78
! sill=10.0D0
! nugget=5.0
  
 CALL KRIGING_INIT(MD,XD,YD,ZD,PD,WD,ND,IDF,COINCIDENT,COINCIDENTDIST,0,0.0D0,0,AGL,RAN)
 
 !## compute mean and substract values from mean - simple kriging
 IF(KTYPE.GT.0)THEN
  MP=0.0D0; DO I=1,ND; MP=MP+PD(I); ENDDO; MP=MP/REAL(ND)
  DO I=1,ND; PD(I)=PD(I)-MP; ENDDO
 ENDIF

 ND=MD
 ALLOCATE(SELID(ND),SELD(4,MAXPNT),SELQID(4,MAXPNT),A(10,10),B(10)) !,L(10,10),U(10,10))

 X=150.0D0; Y=150.0D0; Z=0.0D0
 CALL KRIGING_FILLSYSTEM(XD,XCD,YD,YCD,ZD,ZCD,PD,WD,ND,MAXPNT,X,Y,Z,RANGE,SILL,NUGGET,KTYPE,MP,&
    KEST,KVAR,PNTSEARCH,IQUADRANT,IDF,0,0.0D0,AGL,RAN,0,NACTP)
 
 DO IROW=1,IDF%NROW
  DO ICOL=1,IDF%NCOL
   X=5.0D0; Y=5.0D0; Z=0.0D0
   CALL IDFGETLOC(IDF,IROW,ICOL,X,Y)
   CALL KRIGING_FILLSYSTEM(XD,XCD,YD,YCD,ZD,ZCD,PD,WD,ND,MAXPNT,X,Y,Z,RANGE,SILL,NUGGET,KTYPE,MP,KEST, &
     KVAR,PNTSEARCH,IQUADRANT,IDF,0,0.0D0,AGL,RAN,0,NACTP)
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
 SUBROUTINE KRIGINGSETTINGS(MAXPNT,KTYPE,RANGE,SILL,NUGGET,PNTSEARCH,COINCIDENT,COINCIDENTDIST,IQUADRANT,NGEN,IBREAK)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(INOUT) :: RANGE,SILL,NUGGET,COINCIDENTDIST
 INTEGER,INTENT(OUT),OPTIONAL :: IBREAK
 INTEGER,INTENT(INOUT) :: MAXPNT,KTYPE,PNTSEARCH,COINCIDENT,IQUADRANT
 INTEGER,INTENT(IN) :: NGEN
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I,IERROR
 
 CALL WDIALOGLOAD(ID_DKRIGING,ID_DKRIGING)
 CALL UTL_DIALOGSHOW(-1,-1,0,3)
 
 IF(NGEN.GT.0)THEN; PNTSEARCH=1; CALL WDIALOGFIELDSTATE(IDF_CHECK1,0); ENDIF
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,PNTSEARCH)
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,COINCIDENT)
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK3,IQUADRANT)

 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,MAXPNT)
 CALL WDIALOGPUTDOUBLE(IDF_REAL4,COINCIDENTDIST,'(F15.3)')  
 CALL WDIALOGPUTDOUBLE(IDF_REAL1,SILL,'(F15.3)')  
 CALL WDIALOGPUTDOUBLE(IDF_REAL2,RANGE,'(F15.3)') 
 CALL WDIALOGPUTDOUBLE(IDF_REAL3,NUGGET,'(F15.3)')  
 !## put semivariogram type
 IF(KTYPE.GT.5)KTYPE=2; IF(KTYPE.LT.-5)KTYPE=-2; IF(KTYPE.EQ.0)KTYPE=2
 CALL WDIALOGPUTOPTION(IDF_MENU1,ABS(KTYPE))
 !## type of kriging ktype>0 simple kriging, ktype<0 ordinary kriging
 IF(KTYPE.GT.0)CALL WDIALOGPUTOPTION(IDF_MENU2,1)
 IF(KTYPE.LT.0)CALL WDIALOGPUTOPTION(IDF_MENU2,2)
 
 IF(PRESENT(IBREAK))IBREAK=0
 
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
      CALL WDIALOGGETINTEGER(IDF_INTEGER1,MAXPNT)
      CALL WDIALOGGETDOUBLE(IDF_REAL1,SILL) 
      CALL WDIALOGGETDOUBLE(IDF_REAL2,RANGE)  
      CALL WDIALOGGETDOUBLE(IDF_REAL3,NUGGET)  
      CALL WDIALOGGETDOUBLE(IDF_REAL4,COINCIDENTDIST)  
      !## get semivariogram type
      CALL WDIALOGGETMENU(IDF_MENU1,KTYPE)
      CALL WDIALOGGETMENU(IDF_MENU2,I)
      IF(I.EQ.2)KTYPE=-1*KTYPE
      IERROR=0
      IF(PNTSEARCH.EQ.1.AND.MAXPNT.LE.0)THEN
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Maximum number of points is '//TRIM(ITOS(MAXPNT))//CHAR(13)// &
         'and need to larger than zero','Error'); IERROR=1
      ENDIF
      IF(RANGE.LE.0.0D0)THEN
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Range is '//TRIM(RTOS(RANGE,'F',2))//CHAR(13)// &
         'and need to larger than zero','Error'); IERROR=1
      ENDIF
      IF(SILL.LE.0.0D0)THEN
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Sill is '//TRIM(RTOS(SILL,'F',2))//CHAR(13)// &
         'and need to larger than zero','Error'); IERROR=1
      ENDIF
      IF(NUGGET.LT.0.0D0)THEN
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Nugget is '//TRIM(RTOS(NUGGET,'F',2))//CHAR(13)// &
         'and need to be larger or equal to zero','Error'); IERROR=1
      ENDIF
      IF(SILL-NUGGET.LE.0.0D0)THEN
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Sill minus Nugget is '//TRIM(RTOS(SILL-NUGGET,'F',2))//CHAR(13)// &
         'and need to larger than zero','Error'); IERROR=1
      ENDIF
      IF(IERROR.EQ.0)EXIT
     CASE (IDCANCEL)
      IF(PRESENT(IBREAK))IBREAK=1; EXIT 
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

END MODULE MOD_KRIGING