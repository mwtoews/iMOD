!!  Copyright (C) Stichting Deltares, 2005-2018.
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
MODULE MOD_ISG_UTL

USE WINTERACTER
USE RESOURCE
USE MOD_DBL
USE IMODVAR
USE MOD_ISG_PAR 
USE MOD_QKSORT
USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS,UTL_WSELECTFILE,UTL_MESSAGEHANDLE,UTL_CREATEDIR,UTL_COUNT_COLUMNS,UTL_DIST,UTL_IDATETOJDATE,UTL_GETMED, &
   UTL_CREATEDIR
USE MOD_POLYGON_PAR
USE MOD_POLYGON_UTL
USE MOD_POLYGON
USE MOD_OSD, ONLY : OSD_OPEN,OSD_IOSTAT_MSG
USE MOD_IDF, ONLY : IDFALLOCATEX,IDFNULLIFY,IDFIROWICOL,IDFCOPY
USE MODPLOT, ONLY : MP,MXMPLOT
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_GRAPH, ONLY : GRAPHUNITS,GRAPHAREA
!USE MOD_IDFPLOT

CONTAINS

 !###======================================================================
 SUBROUTINE ISGEDITCLOSE(ICODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICODE

 IDIAGERROR=1

 CALL ISGISPSTOP(ICODE)
 IF(IDIAGERROR.EQ.1)RETURN

 IDIAGERROR=1

 CALL POLYGON1DRAWSHAPE(1,SHP%NPOL)
 CALL POLYGON1CLOSE()

 !## deallocate memory
 CALL ISGDEAL(1); IF(ALLOCATED(ISGIU))DEALLOCATE(ISGIU)

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_ISGEDIT,2).EQ.1)THEN
  !## legend plotting active, remove it from memory
  CALL WDIALOGSELECT(ID_DISGEDITTAB1)
  IF(WINFODIALOGFIELD(ID_LEGEND,FIELDSTATE).EQ.0)THEN
   CALL WDIALOGSELECT(ID_DISGEDITLEGEND); CALL WDIALOGUNLOAD()
  ENDIF
  CALL WDIALOGSELECT(ID_DISGEDIT); CALL WDIALOGUNLOAD()
 ENDIF
 CALL WMENUSETSTATE(ID_ISGEDIT,2,0)

 IDIAGERROR=0

 IF(ALLOCATED(GRAPHUNITS))DEALLOCATE(GRAPHUNITS)
 IF(ALLOCATED(GRAPHAREA))DEALLOCATE(GRAPHAREA)

! CALL IDFPLOTFAST(1)

 END SUBROUTINE ISGEDITCLOSE

  !###======================================================================
 SUBROUTINE ISGISPSTOP(CODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: CODE
 CHARACTER(LEN=256) :: ISGFILE

 !## stop isg-segment editing
 CALL ISGISP_MENUFIELDS(0)
 
 IF(CODE.EQ.0)THEN
  IDIAGERROR=0; IF(ISELISG.EQ.0)RETURN
 ENDIF

 IDIAGERROR=1

 IF(CODE.EQ.1)THEN
  CALL WMESSAGEBOX(YESNOCANCEL,QUESTIONICON,COMMONCANCEL,'Would you like to SAVE to file before leaving choose [Yes]'//CHAR(13)// &
      'Leave without saving choose [No]','Question')
  !## cancel
  IF(WINFODIALOG(4).EQ.0)RETURN
  !## save first before leaving
  IF(WINFODIALOG(4).EQ.1)THEN
   ISGFILE=ISGFNAME; CALL ISGSAVE(ISGFILE,2)
  ENDIF

 ENDIF

! !## none selected
! ISELISG=0

 !## remove current line
 CALL POLYGON1DRAWSHAPE(SHP%NPOL,SHP%NPOL)
 SHP%NPOL=ISGSHAPES

! CALL WCURSORSHAPE(CURARROW)

 IDIAGERROR=0

 END SUBROUTINE ISGISPSTOP
 
  !###======================================================================
 SUBROUTINE ISGISP_MENUFIELDS(ICODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICODE
 INTEGER :: I,J

 CALL WINDOWSELECT(0)
 
 !## stop editing
 IF(ICODE.EQ.0)THEN
  I=1; J=0  
 !## start editing
 ELSE
  I=0; J=1
 ENDIF

 !## deactivate start editing option
 CALL WMENUSETSTATE(ID_ISGISPSTART,1,I)
 !## activate options
 CALL WMENUSETSTATE(ID_ISGISPRESET,1,J)
 CALL WMENUSETSTATE(ID_ISGISPSAVE,1,J)
 CALL WMENUSETSTATE(ID_ISGISPSTOP,1,J)

 END SUBROUTINE ISGISP_MENUFIELDS
 
 !###====================================================================
 SUBROUTINE ISGGRIDGETSTREAMDATA(X,Y,DIST,IPOS,RVAL,ICLC,NCLC,ISTW,NSTW, &
                                 NSEG,MAXNSEG,QSORT,XNR,NDATA,TTIME,NDIM,&
                                 ISTEADY,SDATE,EDATE,IAVERAGE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: MAXNSEG,ICLC,ISTW,NCLC,NSTW,TTIME,NDIM,ISTEADY,SDATE, &
                       EDATE,IAVERAGE
 INTEGER,INTENT(INOUT) :: NSEG
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(MAXNSEG) :: X,Y,DIST
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(NDIM,0:MAXNSEG) :: RVAL
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(NDIM) :: XNR
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(NDIM) :: NDATA
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(TTIME,NDIM) :: QSORT
 INTEGER,INTENT(OUT),DIMENSION(MAXNSEG) :: IPOS
 INTEGER :: I,IREF
 
 IPOS=0; RVAL=0.0D0

 !## include calculation nodes in between segments
 CALL ISG2GRIDINCLUDECLCNODES(ICLC,NCLC,NSEG,MAXNSEG,X,Y,DIST,IPOS,+1)
 !## include structure nodes in between segments
 CALL ISG2GRIDINCLUDECLCNODES(ISTW,NSTW,NSEG,MAXNSEG,X,Y,DIST,IPOS,-1)

 !## read/prepare all information for current date/segment where information is connected to
 I=0
 DO
  I=I+1
  IF(IPOS(I).GT.0)THEN
   IREF=ICLC+IPOS(I)-1
   CALL ISG2GRIDGETDATA(SDATE,EDATE,TTIME,QSORT,XNR,NDIM,RVAL(1,I),ISD(IREF)%N,ISD(IREF)%IREF,ISTEADY, 1,NDATA,IAVERAGE)
  ENDIF
  IF(IPOS(I).LT.0)THEN
   IREF=ISTW+ABS(IPOS(I))-1
   CALL ISG2GRIDGETDATA(SDATE,EDATE,TTIME,QSORT,XNR,NDIM,RVAL(1,I),IST(IREF)%N,IST(IREF)%IREF,ISTEADY,-1,NDATA,IAVERAGE)
   I=I+1
   !## replace waterlevel_down to waterlevel structure
   RVAL(1,I)=RVAL(2,I-1)
  ENDIF
  IF(I.EQ.NSEG)EXIT
 END DO

 !## interpolate all segments in between and give proper values!
 CALL ISG2GRIDINTSEGMENT(X,Y,DIST,NSEG,MAXNSEG)

 END SUBROUTINE ISGGRIDGETSTREAMDATA

 !###====================================================================
 SUBROUTINE ISG2GRIDINTSEGMENT(X,Y,DIST,NSEG,MAXNSEG)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NSEG,MAXNSEG
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(MAXNSEG) :: X,Y
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(MAXNSEG) :: DIST
 INTEGER :: ISEG
 REAL(KIND=DP_KIND) :: DXY

 DIST=0.0D0
 DO ISEG=2,NSEG
  DXY=UTL_DIST(X(ISEG),Y(ISEG),X(ISEG-1),Y(ISEG-1))
  DIST(ISEG)=DIST(ISEG-1)+DXY
 END DO

 END SUBROUTINE ISG2GRIDINTSEGMENT

 !###====================================================================
 SUBROUTINE ISG2GRIDGETDATA(SDATE,EDATE,TTIME,QSORT,XNR,NITEMS,RVAL, &
                            NR,IREF,ISS,ITYPE,NDATA,IAVERAGE)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(NITEMS) :: NDATA
 INTEGER,INTENT(IN) :: SDATE,EDATE,TTIME,NITEMS,NR,IREF,ISS,ITYPE,IAVERAGE
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(TTIME,NITEMS) :: QSORT
 REAL(KIND=DP_KIND),DIMENSION(NITEMS),INTENT(OUT) :: XNR
 REAL(KIND=DP_KIND),DIMENSION(NITEMS),INTENT(INOUT) :: RVAL
 REAL(KIND=DP_KIND) :: F
 INTEGER :: IR,N,I,II,J,I1,I2,IDATE,NAJ,IREC,NDATE,IH,IM,IS

 IREC=IREF-1

 IF(NR.LE.0)RETURN

 IF(ISS.EQ.1)QSORT=0.0D0
 IF(ISS.EQ.2)THEN
  IF(IAVERAGE.EQ.1)THEN
   QSORT=0.0D0
  ELSE
   DO I=1,TTIME; DO J=1,NITEMS; QSORT(I,J)=NDATA(J); ENDDO; ENDDO
  ENDIF
 ENDIF
 I1   = 1

 XNR=0.0D0
 DO IR=1,NR

  IREC=IREC+1

  IF(ITYPE.EQ.1)THEN
   IF(ISFR.EQ.0)THEN
    IDATE   =DATISD(IREC)%IDATE
    RVAL(1) =DATISD(IREC)%WLVL
    RVAL(2) =DATISD(IREC)%BTML
    RVAL(3) =DATISD(IREC)%RESIS
    RVAL(4) =DATISD(IREC)%INFF
   ELSEIF(ISFR.EQ.1)THEN
    IDATE   =DATISD(IREC)%IDATE
    READ(DATISD(IREC)%CTIME,'(3(I2,1X))') IH,IM,IS
    RVAL(1) =DATISD(IREC)%WLVL
    RVAL(2) =DATISD(IREC)%BTML
    RVAL(3) =DATISD(IREC)%WIDTH
    RVAL(4) =DATISD(IREC)%THCK
    RVAL(5) =DATISD(IREC)%HCND
    RVAL(6) =DATISD(IREC)%UPSG
    RVAL(7) =DATISD(IREC)%DWNS
    RVAL(8) =DATISD(IREC)%ICLC
    RVAL(9) =DATISD(IREC)%IPRI
    RVAL(10)=DATISD(IREC)%QFLW
    RVAL(11)=DATISD(IREC)%QROF
    RVAL(12)=DATISD(IREC)%PPTSW
    RVAL(13)=DATISD(IREC)%ETSW
   ENDIF
  ELSEIF(ITYPE.EQ.-1)THEN
   IDATE  =DATIST(IREC)%IDATE
   RVAL(1)=DATIST(IREC)%WLVL_UP
   RVAL(2)=DATIST(IREC)%WLVL_DOWN
   RVAL(3)=0.0D0
   RVAL(4)=0.0D0
  ENDIF

  !## don't bother for steady-state, take the mean!
  IF(ISS.EQ.1)THEN
   DO I=1,NITEMS; IF(RVAL(I).NE.NDATA(I))THEN; QSORT(1,I)=QSORT(1,I)+RVAL(I); XNR(I)=XNR(I)+1.0D0; ENDIF; ENDDO
  !## transient simulation
  ELSEIF(ISS.EQ.2)THEN

   NDATE=EDATE
   IF(IR.LT.NR)THEN
    IF(ITYPE.EQ.1) NDATE=DATISD(IREC+1)%IDATE
    IF(ITYPE.EQ.-1)NDATE=DATIST(IREC+1)%IDATE
    NDATE=UTL_IDATETOJDATE(NDATE)
   ENDIF

   NDATE=MIN(EDATE,NDATE)
   IDATE=UTL_IDATETOJDATE(IDATE)

   !## stop searching for data, outside modeling window!
   IF(IDATE.GE.EDATE)EXIT

   !## within modeling window
   IF(NDATE.GE.SDATE)THEN

    N=NDATE-SDATE
    IF(IDATE.GT.SDATE)N=N-(IDATE-SDATE)
    I2=I1+N-1

    IF(I2.GE.MAX(1,I1))THEN
     DO I=1,NITEMS
      IF(IAVERAGE.EQ.1)THEN
       F=REAL(I2-MAX(1,I1)+1)
       QSORT(1,I)=QSORT(1,I)+RVAL(I)*F
       XNR(I)=XNR(I)+F
      ELSEIF(IAVERAGE.EQ.2)THEN
       DO II=MAX(1,I1),I2
        QSORT(II,I)=RVAL(I)
       ENDDO
      ENDIF
     END DO
    ENDIF

    I1=I2+1

   ENDIF

  ENDIF

 END DO

 !## steady-state, take the mean of all
 IF(ISS.EQ.1)THEN
  !## determine for each period appropriate attribute term
  DO I=1,NITEMS
   IF(XNR(I).GT.0.0D0)THEN
    RVAL(I)=QSORT(1,I)/REAL(XNR(I))
   ELSE
    RVAL(I)=NDATA(I)
   ENDIF
  ENDDO
 !## take the mean (better than median)
 ELSEIF(ISS.EQ.2)THEN
  !## arithmetic mean (weighted)
  IF(IAVERAGE.EQ.1)THEN
   DO I=1,NITEMS
    IF(XNR(I).GT.0.0D0)THEN
     RVAL(I)=QSORT(1,I)/XNR(I)
    ELSE
     RVAL(I)=NDATA(I)
    ENDIF
   ENDDO
  !## median - exclude nodata
  ELSEIF(IAVERAGE.EQ.2)THEN
   DO I=1,NITEMS
    CALL UTL_GETMED(QSORT(:,I),TTIME,NDATA(I),(/50.0D0/),1,NAJ,RVAL(I:))
   ENDDO
  ENDIF
 ENDIF

 END SUBROUTINE ISG2GRIDGETDATA
 
 !###====================================================================
 SUBROUTINE ISG2GRIDINCLUDECLCNODES(ITYP,NTYP,NSEG,MAXNSEG,X,Y,DIST,IPOS,ITYPE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYP,NTYP,MAXNSEG,ITYPE
 INTEGER,INTENT(INOUT) :: NSEG
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(MAXNSEG) :: DIST,X,Y
 INTEGER,INTENT(OUT),DIMENSION(MAXNSEG) :: IPOS
 INTEGER :: IREC,ISEG,I,II
 REAL(KIND=DP_KIND) :: DXY,D1,D2,F,XC,YC

 !## include calculation nodes as segments!
 IREC=ITYP-1
 !## determine which nodes possess heads/etc.
 DO I=1,NTYP
  !## get tot distances
  CALL ISG2GRIDINTSEGMENT(X,Y,DIST,NSEG,MAXNSEG)
  IREC=IREC+1
  IF(ITYPE.EQ. 1)DXY=ISD(IREC)%DIST
  IF(ITYPE.EQ.-1)DXY=IST(IREC)%DIST

  !## find position in between segments
  DO ISEG=2,NSEG
   IF(DXY.GE.DIST(ISEG-1).AND.DXY.LE.DIST(ISEG))EXIT
  END DO

  !## caused by inaccuracy of comparison of dxy and dist()
  ISEG=MIN(ISEG,NSEG)

  !## distance current segment
  D1=DIST(ISEG)-DIST(ISEG-1)
  D2=DXY-DIST(ISEG-1)
  F =0.0D0
  IF(D1.NE.0.0D0)F=D2/D1

  !## put in extra coordinate
  IF(F.LE.0.0D0.AND.ITYPE.EQ.1)THEN
   !## put data to current node
   IPOS(ISEG-1)=I*ITYPE
  ELSEIF(F.GE.1.0D0.AND.ITYPE.EQ.1)THEN
   !## put data to current node
   IPOS(ISEG)  =I*ITYPE
  ELSE
   XC=X(ISEG-1)+((X(ISEG)-X(ISEG-1))*F)
   YC=Y(ISEG-1)+((Y(ISEG)-Y(ISEG-1))*F)
   !## position coordinates in between
   DO II=NSEG+1,ISEG+1,-1
    X(II)   =X(II-1)
    Y(II)   =Y(II-1)
    IPOS(II)=IPOS(II-1)
   ENDDO
   X(ISEG)            =XC
   Y(ISEG)            =YC
   !## increase number of segments
   NSEG               =NSEG+1
   !## put data to current node
   IPOS(ISEG)         =I*ITYPE
  ENDIF

  !## duplicate point in case of structure
  IF(ITYPE.EQ.-1)THEN
   !## position coordinates in between
   DO II=NSEG+1,ISEG+1,-1
    X(II)   =X(II-1)
    Y(II)   =Y(II-1)
    IPOS(II)=IPOS(II-1)
   ENDDO
   X(ISEG)            =XC
   Y(ISEG)            =YC
   !## increase number of segments
   NSEG               =NSEG+1
   !## put data to current node
   IPOS(ISEG)         =I*ITYPE
  ENDIF

 END DO

 !## put last ipos() for security reasons ... could happen in case of accuray of coordinates
 IF(IPOS(NSEG).EQ.0)THEN

  DO I=NSEG,1,-1
   IF(IPOS(I).GT.0)THEN
    IPOS(NSEG)=IPOS(I)
    EXIT
   ENDIF
  END DO

 ENDIF

 END SUBROUTINE ISG2GRIDINCLUDECLCNODES
 
 !###====================================================================
 SUBROUTINE ISGGRIDINTSTREAMDATA(DIST,IPOS,RVAL,NSEG,MAXNSEG,NDIM,NODATA,NSTW,ICDIST)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: MAXNSEG,NSEG,NDIM,NSTW,ICDIST
 REAL(KIND=DP_KIND),INTENT(IN) :: NODATA
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(MAXNSEG) :: DIST
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(NDIM,0:MAXNSEG) :: RVAL
 INTEGER,INTENT(INOUT),DIMENSION(MAXNSEG) :: IPOS
 INTEGER :: I,II,J,K
 REAL(KIND=DP_KIND) :: FCT,D
 
 DO I=1,NDIM
  DO J=1,NSEG
   IF(IPOS(J).GT.0.AND.RVAL(I,J).NE.NODATA)THEN
    DO II=J+1,NSEG
     IF(IPOS(II).GT.0.AND.RVAL(I,II).NE.NODATA)EXIT
    ENDDO
    D=MAX(0.0D0,DIST(II)-DIST(J))
    DO K=J+1,II-1
     FCT=0.0D0
     IF(D.GT.0.0D0)FCT=(DIST(K)-DIST(J))/D
     RVAL(I,K)=RVAL(I,J)+((RVAL(I,II)-RVAL(I,J))*FCT)
    END DO
   ENDIF
  ENDDO
 ENDDO

 !## interpolate with structures if not included via icdist
 IF(NSTW.GT.0.AND.ICDIST.EQ.0)THEN
  DO J=1,NSEG
   IF(IPOS(J).NE.0.AND.RVAL(1,J).NE.NODATA)THEN
    D=0.0D0
    DO II=J+1,NSEG
     D=D+(DIST(II)-DIST(J))
     IF(IPOS(II).NE.0.AND.RVAL(1,II).NE.NODATA)EXIT
    ENDDO
    DO K=J+1,II-1
     FCT=0.0D0; IF(D.GT.0.0D0)FCT=(DIST(K)-DIST(J))/D
     RVAL(1,K)=RVAL(1,J)+((RVAL(1,II)-RVAL(1,J))*FCT)
    END DO
   ENDIF
  END DO
 ENDIF

 END SUBROUTINE ISGGRIDINTSTREAMDATA
 
 !###====================================================================
 REAL(KIND=DP_KIND) FUNCTION ISG_UTL_LEAKAGE(CD,DXY,INFF,TD,WETPER,C0,C1,D,KH)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(INOUT) :: INFF
 REAL(KIND=DP_KIND),INTENT(IN) :: DXY,TD,WETPER,C0,C1,D,KH,CD
 REAL(KIND=DP_KIND) :: KV,COND_INF,COND_DRN
 
 !## no adjustments
 ISG_UTL_LEAKAGE=1.0D0

 !## no resistance
 IF(C1.LE.0.0D0)THEN; ISG_UTL_LEAKAGE=0.0D0; RETURN; ENDIF
 
 KV =KH/10.0D0                                   !## vertical permeability 

 !## process formulae van de Lange - infiltratie weerstand
 COND_INF=0.0D0; IF(INFF.GT.0.0D0.AND.INFF.LE.1.0D0)COND_INF=ISG_UTL_LEAKAGE_Y(DXY,D,KV,KH,C1,TD,WETPER,C0/INFF)

 !## process formulae van de Lange - drainage weerstand
 COND_DRN=ISG_UTL_LEAKAGE_Y(DXY,D,KV,KH,C1,TD,WETPER,C0)

 !## infiltration factor
 IF(COND_DRN.GT.0.0D0)INFF=COND_INF/COND_DRN
 
 ISG_UTL_LEAKAGE=COND_DRN/CD

 END FUNCTION ISG_UTL_LEAKAGE

 !###==================================================================== 
 REAL(KIND=DP_KIND) FUNCTION ISG_UTL_LEAKAGE_Y(A,H0,KV,KH,C1,LI,BIN,C0)
 !###====================================================================
 !-----------------------------------------------------------------------
 !* DESCRIPTION:
 !*    Calculates de lekconductance according to De Lange
 !*    A = celoppervlak (m2)
 !*    H0 = doorstroomde dikte (m)
 !*    kv = verticale doorlatendheid (m/d)
 !*    kh = horizontale doorlatendheid (m/d)
 !*    c1 = deklaagweerstand (d)
 !*    li = lengte van de waterlopen (m)
 !*    Bin = Bodembreedte (m)
 !*    c0 = slootbodemweerstand (d)
 !*    Uitkomst: Lekconductance (m2/d)
 !*
 !*    Conductances are easier to handle than resistances
 !*    NB: L may not exceed SQRT(A), Lrad may because it is fair to assume that the total radial resistance is concentrated within A
 !*
 !-----------------------------------------------------------------------
 !
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: A,H0,KV,KH,C1,LI,BIN,C0
 REAL(KIND=DP_KIND) :: L,BCOR,LABDAL,FL,LABDAB,FB,CL,CB,CRAD,WP,ACOR,PSL
 REAL(KIND=DP_KIND) :: X,Y,PI

 PI = 4.0 * ATAN(1.0D0)
 ISG_UTL_LEAKAGE_Y=0.0D0

 IF(LI.GT.0.01D0.AND.BIN.GT.0.01D0.AND.A.GT.0.01D0)THEN
  ACOR  = A
  BCOR  = MAX(BIN,0.01D0)
  L     = A/LI-BCOR
  Y     = C1+H0/KV
  LABDAL= SQRT(Y*KH*H0)
  X     = 0.5*L/LABDAL
  FL    = X*ISG_UTL_LEAKAGE_CTNH(X)
  LABDAB= SQRT(Y*KH*H0*C0/(Y+C0))
  X     = 0.5*BCOR/LABDAB
  FB    = X*ISG_UTL_LEAKAGE_CTNH(X)
  CL    =(C0+Y)*FL+(C0*L/BCOR)*FB
  CB    =(C1+C0+H0/KV)/(CL-C0*L/BCOR)*CL
  CRAD  = MAX(0.,L/(PI*SQRT(KV*KH))*LOG(4*H0/(PI*BCOR)))
  PSL   = BCOR*LI/A
  WP    = 1.0D0/((1.0D0-PSL)/CL+PSL/CB)+CRAD-Y
  ISG_UTL_LEAKAGE_Y = A/WP
 ENDIF

 END FUNCTION ISG_UTL_LEAKAGE_Y

 !###====================================================================
 REAL(KIND=DP_KIND) FUNCTION ISG_UTL_LEAKAGE_CTNH(X)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X

 IF(X.LT.-4.0)THEN
  ISG_UTL_LEAKAGE_CTNH=-1.0D0
 ELSEIF(X.GT.4.0)THEN
  ISG_UTL_LEAKAGE_CTNH=1.0D0
 ELSE
  ISG_UTL_LEAKAGE_CTNH=(EXP(X)+EXP(-1.0D0*X))/(EXP(X)-EXP(-1.0D0*X))
 ENDIF

 END FUNCTION ISG_UTL_LEAKAGE_CTNH
 
 !###======================================================================
 LOGICAL FUNCTION ISGATTRIBUTES_2DCROSS_READ(J,IDF,PIDF,ZCHK)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: J
 REAL(KIND=DP_KIND),INTENT(OUT) :: ZCHK
 INTEGER :: I,IROW,ICOL,ICHK
 TYPE(IDFOBJ),INTENT(OUT) :: IDF,PIDF
 
 ISGATTRIBUTES_2DCROSS_READ=.FALSE.
 
 CALL IDFNULLIFY(IDF); CALL IDFNULLIFY(PIDF)
 IF(.NOT.ISGATTRIBUTES_2DCROSS_IDFDIM(IDF,J))THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot present results in table.'//CHAR(13)// &
    'no cellsizes can be determined','Warning')
  RETURN
 ENDIF
 
 CALL IDFCOPY(IDF,PIDF)
 IF(.NOT.IDFALLOCATEX(IDF)) RETURN
 IF(.NOT.IDFALLOCATEX(PIDF))RETURN

 IDF%NODATA =-9999; IDF%X=IDF%NODATA
 PIDF%NODATA=-9999; PIDF%X=0.0D0  !## on default, inundated whenever stage higher than riverbed
 ICHK=0; IF(DATISC2(J,1)%DISTANCE.LT.0.0D0.AND.DATISC2(J,1)%BOTTOM.LT.0.0D0)THEN
  ICHK=1; ZCHK=DATISC2(J,1)%MRC
 ENDIF
 
 !## skip first (dx,dy)
 DO I=2,TISC(J)
  CALL IDFIROWICOL(IDF,IROW,ICOL,DATISC2(J,I)%DISTANCE,DATISC2(J,I)%BOTTOM)
  IDF%X(ICOL,IROW) =DATISC2(J,I)%MRC
  !## store inundation pointer
  IF(ICHK.EQ.1)PIDF%X(ICOL,IROW)=DATISC2(J,I)%ZP
 ENDDO 
 
 ISGATTRIBUTES_2DCROSS_READ=.TRUE.
 
 END FUNCTION ISGATTRIBUTES_2DCROSS_READ
   
 !###======================================================================
 LOGICAL FUNCTION ISGATTRIBUTES_2DCROSS_IDFDIM(IDF,J)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: J
 TYPE(IDFOBJ),INTENT(OUT) :: IDF
 
 ISGATTRIBUTES_2DCROSS_IDFDIM=.FALSE. 

 IDF%DX=ABS(DATISC2(J,1)%DISTANCE)
 IDF%DY=ABS(DATISC2(J,1)%BOTTOM)
 
 IF(IDF%DX.LE.0.0D0.OR.IDF%DY.LE.0.0D0)RETURN
 
 IDF%XMIN=MINVAL(DATISC2(J,2:TISC(J))%DISTANCE)-0.5*IDF%DX 
 IDF%XMAX=MAXVAL(DATISC2(J,2:TISC(J))%DISTANCE)+0.5*IDF%DX 
 IDF%YMIN=MINVAL(DATISC2(J,2:TISC(J))%BOTTOM)  -0.5*IDF%DY
 IDF%YMAX=MAXVAL(DATISC2(J,2:TISC(J))%BOTTOM)  +0.5*IDF%DY

 IDF%IEQ=0; IDF%ITB=0 !; IDF%IVF=0
 IDF%NCOL=(IDF%XMAX-IDF%XMIN)/IDF%DX
 IDF%NROW=(IDF%YMAX-IDF%YMIN)/IDF%DY
  
 ISGATTRIBUTES_2DCROSS_IDFDIM=.TRUE.
 
 END FUNCTION ISGATTRIBUTES_2DCROSS_IDFDIM
 
 !##=====================================================================
 SUBROUTINE ISGGETXY(X,Y,N,TDIST,XCRD,YCRD)
 !##=====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),INTENT(IN) :: TDIST
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(N) :: X,Y   
 REAL(KIND=DP_KIND),INTENT(OUT) :: XCRD,YCRD
 INTEGER :: I
 REAL(KIND=DP_KIND) :: D,F,TD
 
 TD=0.0D0; DO I=2,N
  D=(X(I)-X(I-1))**2.0+(Y(I)-Y(I-1))**2.0
  IF(D.GT.0.0D0)D=SQRT(D); TD=TD+D
  IF(TD.GT.TDIST)THEN
   F=(D-(TD-TDIST))/D
   XCRD=X(I-1)+F*(X(I)-X(I-1))
   YCRD=Y(I-1)+F*(Y(I)-Y(I-1))
   EXIT
  ENDIF
 ENDDO
 
 END SUBROUTINE ISGGETXY

 !###===============================================================================
 SUBROUTINE ISGSTUWEN_INTERSECT(MAXDIST,XC,YC,PISG,NI) 
 !###===============================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(:,:) :: PISG !## selected isg segments all within radius
 REAL(KIND=DP_KIND),INTENT(IN) :: MAXDIST,XC,YC !## maximum distance allowed and location of cross-section
 INTEGER,INTENT(OUT) :: NI
 INTEGER :: I,J,K,ISTATUS,ISMALL,NJ
 REAL(KIND=DP_KIND) :: DX,DY,TD,D,MD,X,Y,DIST
 
 !## initialize
 ISGX=XC; ISGY=YC; DIST=-1.0D0; PISG=0; MD=MAXDIST; NI=0

 !## smallest only
 ISMALL=0; IF(SIZE(PISG).EQ.1)ISMALL=1

 DO I=1,NISG
  TD=0.0D0; K=ISG(I)%ISEG-1; NJ=0
  DO J=1,ISG(I)%NSEG-1

   K=K+1; DX=ISP(K+1)%X-ISP(K)%X; DY=ISP(K+1)%Y-ISP(K)%Y
   !## perform intersection
   CALL DBL_IGRINTERSECTLINE(ISP(K)%X,ISP(K)%Y,ISP(K+1)%X,ISP(K+1)%Y,XC,YC,XC+DY,YC-DX,X,Y,ISTATUS)

   !## intersect isgline and/or intersect (5)
   IF(ISTATUS.EQ.3.OR.ISTATUS.EQ.5)THEN
    !## compute distance
    D=SQRT((X-XC)**2.0+(Y-YC)**2.0)
    !## first time to put results, or replace it whenever new point is closer
    IF(D.LT.MD)THEN
     IF(ISMALL.EQ.1)MD=D
     DIST=TD+SQRT((ISP(K)%X-X)**2.0+(ISP(K)%Y-Y)**2.0)
     ISGX=X; ISGY=Y 
     IF(ISMALL.EQ.0.AND.NJ.EQ.0)THEN; NI=NI+1; NJ=1; ENDIF
     PISG(NI,1)=REAL(I); PISG(NI,2)=DIST; PISG(NI,3)=D
    ENDIF
   ELSE
    !## include position of nodes
    D=SQRT((XC-ISP(K)%X)**2.0+(YC-ISP(K)%Y)**2.0)
    IF(D.LT.MD)THEN
     IF(ISMALL.EQ.1)MD=D
     DIST=0.0D0; IF(J.GT.1)DIST=TD+SQRT(DX**2.0+DY**2.0)
     ISGX=ISP(K)%X; ISGY=ISP(K)%Y
     IF(ISMALL.EQ.0.AND.NJ.EQ.0)THEN; NI=NI+1; NJ=1; ENDIF
     PISG(NI,1)=REAL(I); PISG(NI,2)=DIST; PISG(NI,3)=D
    ENDIF
    !## evaluate last point
    IF(J.EQ.ISG(I)%NSEG-1)THEN
     D=SQRT((XC-ISP(K+1)%X)**2.0+(YC-ISP(K+1)%Y)**2.0)
     IF(D.LT.MD)THEN
      IF(ISMALL.EQ.1)MD=D
      DIST=TD+SQRT(DX**2.0+DY**2.0)
      ISGX=ISP(K+1)%X; ISGY=ISP(K+1)%Y 
      IF(ISMALL.EQ.0.AND.NJ.EQ.0)THEN; NI=NI+1; NJ=1; ENDIF
      PISG(NI,1)=REAL(I); PISG(NI,2)=DIST; PISG(NI,3)=D
     ENDIF
    ENDIF
   ENDIF

   !## segment found - stop searching for this segment to be within range
   IF(NJ.EQ.1)EXIT
   
   !## get total distance
   TD=TD+SQRT(DX**2.0+DY**2.0)

  ENDDO
 END DO

 !## sort iisg() 
 IF(ISMALL.EQ.1)RETURN
 
 !## sort for nearest distance
 CALL QKSORT(NI,PISG(:,3),V2=PISG(:,1),V3=PISG(:,2))

 END SUBROUTINE ISGSTUWEN_INTERSECT

 !###======================================================================
 SUBROUTINE UTL_GETUNITSISG(IU,ISGNAME,TSTAT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: ISGNAME
 CHARACTER(LEN=*),INTENT(IN) :: TSTAT
 INTEGER,DIMENSION(MAXFILES),INTENT(OUT) :: IU
 LOGICAL :: LEX,LOPEN
 CHARACTER(LEN=10) :: TSTATUS
 CHARACTER(LEN=50) :: MESSAGE,TACT
 CHARACTER(LEN=256) :: FNAME
 INTEGER :: IOS,I,J,IRECL,IBYTE,JRECL

 TSTATUS=TSTAT; CALL IUPPERCASE(TSTATUS)
 TACT='READWRITE'; IF(TSTATUS(1:3).EQ.'OLD')TACT='READ,DENYWRITE'

 !## single precision by default
 IF(TSTATUS.EQ.'OLD')ISGDOUBLE=4

 !## initialise all
 IU=0

 DO I=1,MAXFILES

  J=INDEX(ISGNAME,'.',.TRUE.)-1; FNAME=ISGNAME(:J)//'.'//TRIM(EXT(I))

  INQUIRE(FILE=FNAME,OPENED=LOPEN)
  IF(LOPEN)THEN; INQUIRE(FILE=FNAME,NUMBER=IU(I)); CLOSE(IU(I)); ENDIF
  
  IBYTE=0; JRECL=RECLEN(I)
  !## sfr compliant isg file
  IF(ISFR.EQ.1.AND.I.EQ.4)JRECL=9*4+5*4+8
  
  !## open for reading
  IF(TSTATUS.EQ.'OLD')THEN

   INQUIRE(FILE=FNAME,EXIST=LEX)
   !## skip if file cannot be found
   IF(.NOT.LEX)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot find'//CHAR(13)//TRIM(FNAME),'Error'); EXIT
   ENDIF

   !## examine size of binary files
   IF(I.GT.1)THEN

    IBYTE=UTL_GETRECORDLENGTH(FNAME)

    IF(ISGDOUBLE.EQ.8.AND.IBYTE.EQ.JRECL)THEN 
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Inconsistent record length found: '//TRIM(ITOS(IBYTE))//' bytes'//CHAR(13)// &
       'Record length needed: '//TRIM(ITOS(JRECL))//' bytes.'//CHAR(13)//'Wrong record length for double precision, cannot open'//CHAR(13)//&
       TRIM(FNAME),'Error'); EXIT
    ENDIF

    !## sfr file
    IF(I.EQ.4.AND.ISFR.EQ.1)THEN
     IF(IBYTE.EQ.9*8+5*4+8)ISGDOUBLE=8
    ELSE
     !## switch to double precision
     IF(IBYTE.EQ.RECLND(I))ISGDOUBLE=8
    ENDIF
    
    IF(ISGDOUBLE.EQ.4.AND.IBYTE.NE.JRECL)THEN !RECLEN(I))THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Inconsistent record length found: '//TRIM(ITOS(IBYTE))//' bytes'//CHAR(13)// &
       'Record length needed: '//TRIM(ITOS(RECLND(I)))//' bytes.'//CHAR(13)//'Wrong record length for single precision, cannot open'//CHAR(13)//&
       TRIM(FNAME),'Error'); EXIT
    ENDIF

   ENDIF
  
  !## open for writing
  ELSE
  
   IF(ISGDOUBLE.EQ.4)IBYTE=RECLEN(I) !## record length in bytes for single precision
   IF(ISGDOUBLE.EQ.8)IBYTE=RECLND(I) !## record length in bytes for double precision

   !## isd file different between sfr/isg compliant file
   IF(I.EQ.4.AND.ISFR.EQ.1)THEN
    IF(ISGDOUBLE.EQ.4)IBYTE=9*4+5*4+8
    IF(ISGDOUBLE.EQ.8)IBYTE=9*8+5*4+8
   ENDIF
    
  ENDIF

  IU(I)=UTL_GETUNIT()
  IRECL=IBYTE/4 !## words for INTEL
  CALL UTL_CREATEDIR(FNAME(:INDEX(FNAME,'\',.TRUE.)-1))
  IF(IRECL.EQ.0)CALL OSD_OPEN(IU(I),FILE=FNAME,STATUS=TSTATUS,FORM=TFORM(I),ACTION=TACT,IOSTAT=IOS)
  IF(IRECL.GT.0)CALL OSD_OPEN(IU(I),FILE=FNAME,STATUS=TSTATUS,FORM=TFORM(I),ACTION=TACT,ACCESS='DIRECT',RECL=IRECL,IOSTAT=IOS)

  IF(IOS.NE.0)THEN
   CALL OSD_IOSTAT_MSG(IOS,MESSAGE)
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open'//CHAR(13)//TRIM(FNAME)//CHAR(13)// &
   TRIM(MESSAGE),'Error'); EXIT
  ENDIF
  
  !## read type of ISG
  IF(TSTATUS.EQ.'OLD')THEN
   IF(I.EQ.1)THEN
    READ(IU(1),*,IOSTAT=IOS) J,ISFR
    IF(IOS.NE.0)ISFR=0; REWIND(IU(1))
   ENDIF
  ENDIF
   
 ENDDO

 IF(MINVAL(IU).LE.0)THEN
  DO I=1,MAXFILES
   IF(IU(I).GT.0)THEN
    INQUIRE(UNIT=IU(I),OPENED=LEX)
    IF(LEX)CLOSE(IU(I))
   ENDIF
  END DO
  IU=0
 ENDIF

 END SUBROUTINE UTL_GETUNITSISG

! !###======================================================================
! LOGICAL FUNCTION ISGREWRITEFILE(FNAME,TFORM,IRECL)
! !###======================================================================
! IMPLICIT NONE
! CHARACTER(LEN=*),INTENT(IN) :: FNAME,TFORM
! INTEGER,INTENT(IN),DIMENSION(2) :: IRECL
! INTEGER,DIMENSION(2) :: IU,IOS,TLEN
! CHARACTER(LEN=256) :: STRING
! INTEGER :: IREC,IREF,N,I
! REAL(KIND=DP_KIND) :: DIST
!
! ISGREWRITEFILE=.FALSE.
!
! IU(1)=UTL_GETUNIT()
! CALL OSD_OPEN(IU(1),FILE=FNAME,STATUS='OLD',FORM=TFORM,ACTION='READ,DENYWRITE',ACCESS='TRANSPARENT',IOSTAT=IOS(1))
! IF(IOS(1).NE.0)THEN
!  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open file: '//CHAR(13)// &
!    TRIM(FNAME),'Error')
!  RETURN
! ENDIF
!
! IU(2)=UTL_GETUNIT()
! IREC=IRECL(2)
! IF(ICF.EQ.1)IREC=IREC/4 !## words for INTEL
! CALL OSD_OPEN(IU(2),FILE=TRIM(PREFVAL(1))//'\tmp\xxx.tmp',STATUS='REPLACE',FORM=TFORM,ACTION='WRITE', &
!      ACCESS='DIRECT',RECL=IREC,IOSTAT=IOS(2))  !## record length in words/bytes!
! IF(IOS(2).NE.0)THEN
!  CLOSE(IU(1))
!  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot create temporary file: '//CHAR(13)// &
!    TRIM(PREFVAL(1))//'\tmp\xxx.tmp','Error')
!  RETURN
! ENDIF
! !## record length in bytes (for Lahey)
!
! WRITE(IU(2),REC=1) IRECL(2)*256+247
! TLEN=IRECL
!
! READ(IU(1),IOSTAT=IOS(1)) IREC,STRING(1:(TLEN(1)-4)) !## record length in bytes (according to Lahey)
!
! TLEN=TLEN-(3*4)
!
! !## make sure string is empty
! STRING=''
!
! IREC=0
! DO
!
!  READ(IU(1),IOSTAT=IOS(1))  N,IREF,DIST,STRING(1:TLEN(1))
!  IF(IOS(1).NE.0)EXIT
!
!  IREC=IREC+1
!  WRITE(IU(2),REC=IREC,IOSTAT=IOS(2)) N,IREF,DIST,STRING(1:TLEN(2))
!  IF(IOS(2).NE.0)THEN
!   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error writing record '//TRIM(ITOS(IREC))//' from file: '//CHAR(13)// &
!     TRIM(FNAME),'Error')
!   RETURN
!  ENDIF
! ENDDO
!
! CLOSE(IU(1))
! CLOSE(IU(2))
!
! I=INFOERROR(1)
! CALL IOSCOPYFILE(TRIM(FNAME),TRIM(FNAME)//'_old_imod_version')
! IF(WINFOERROR(1).EQ.13)THEN
!  CALL INTEGERTOSTRING(WINFOERROR(3),STRING,'(I5)')
!  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Failed to back up file. Code='//TRIM(STRING),'Copy Error')
!  RETURN
! ENDIF
! I=INFOERROR(1)
! CALL IOSCOPYFILE(TRIM(PREFVAL(1))//'\tmp\xxx.tmp',TRIM(FNAME))
! IF(WINFOERROR(1).EQ.13)THEN
!  CALL INTEGERTOSTRING(WINFOERROR(3),STRING,'(I5)')
!  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Failed to copy new file. Code='//TRIM(STRING),'Copy Error')
!  RETURN
! ENDIF
!
! ISGREWRITEFILE=.TRUE.
!
! END FUNCTION ISGREWRITEFILE
!
 !###======================================================================
 LOGICAL FUNCTION ISGOPENFILES(ISGFILE,CSTATUS)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: CSTATUS
 CHARACTER(LEN=*),INTENT(IN) :: ISGFILE
 INTEGER :: NISGFILES
 
 ISGOPENFILES=.FALSE.

 NISGFILES=1

 IF(ALLOCATED(ISGIU))DEALLOCATE(ISGIU)
 ALLOCATE(ISGIU(MAXFILES,NISGFILES))

 !## open isg-file
 CALL UTL_GETUNITSISG(ISGIU(:,1),ISGFILE,CSTATUS)
 !## failed to open
 IF(MINVAL(ISGIU(:,1)).LE.0)THEN
  CALL ISGCLOSEFILES()
 ELSE
  ISGOPENFILES=.TRUE.
 ENDIF

 END FUNCTION ISGOPENFILES

 !###======================================================================
 SUBROUTINE ISGCLOSEFILES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J
 LOGICAL :: LEX
 
 DO I=1,SIZE(ISGIU,2)  
  DO J=1,SIZE(ISGIU,1) 
   IF(ISGIU(J,I).GT.0)THEN
    INQUIRE(UNIT=ISGIU(J,I),OPENED=LEX)
    IF(LEX)CLOSE(ISGIU(J,I))
   ENDIF
  END DO
 END DO
 IF(ALLOCATED(ISGIU))DEALLOCATE(ISGIU)

 END SUBROUTINE ISGCLOSEFILES

 !###===============================================================================
 LOGICAL FUNCTION ISGREAD(FNAME,IBATCH)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: FNAME
 CHARACTER(LEN=256) :: LINE
 INTEGER :: IS,I,J,ISGFILES,IREC,NISGFILES,NV
 INTEGER,DIMENSION(5) :: IOS
 REAL(KIND=SP_KIND) :: DIST
 
 ISGREAD=.FALSE.
 
 !## deallocate memory
 CALL ISGDEAL(1)

 NISGFILES=SIZE(FNAME)
 ALLOCATE(ISGIU(MAXFILES,NISGFILES))

 DO ISGFILES=1,NISGFILES

  CALL UTL_GETUNITSISG(ISGIU(:,ISGFILES),FNAME(ISGFILES),'OLD')
  IF(MINVAL(ISGIU(:,ISGFILES)).LE.0)THEN
   CALL ISGCLOSEFILES()
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot find ISG files for reading','Error')
   IF(IBATCH.EQ.1)WRITE(*,*) 'Cannot find ISG files for reading ! '
   RETURN
  ENDIF
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(FNAME(ISGFILES))//' ...')
  IF(IBATCH.EQ.1)WRITE(*,'(/1X,A/)') 'Reading '//TRIM(FNAME(ISGFILES))//' ...'
 ENDDO
 
 ALLOCATE(NISGF(NISGFILES),NISFR(NISGFILES))

 !#----------------*.isg

 DIMISG=0
 DO ISGFILES=1,NISGFILES
  !#read segment-dimensions
  READ(ISGIU(1,ISGFILES),'(A256)',IOSTAT=IOS(1)) LINE
  READ(LINE,*,IOSTAT=IOS(1)) NISGF(ISGFILES),NISFR(ISGFILES)
  IF(IOS(1).NE.0)THEN
   NISFR(ISGFILES)=0
   READ(LINE,*,IOSTAT=IOS(1)) NISGF(ISGFILES)
   IF(IOS(1).NE.0)THEN
    CALL ISGDEAL(1)
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD not read the number: '//TRIM(ITOS(ISGFILES))// &
                   ', of the selected isg-files properly!','Error')
    RETURN
   ENDIF
   IF(.NOT.ALLOCATED(ISDLABELS))THEN
    ALLOCATE(ISDLABELS(SIZE(TATTRIB1))); DO I=1,SIZE(ISDLABELS); ISDLABELS(I)=TATTRIB1(I); ENDDO
   ENDIF
  ELSE
   !## try to read label for the isd-file - use labels from the first isg given only
   IF(ISGFILES.EQ.1)THEN
    IF(.NOT.ALLOCATED(ISDLABELS))THEN
     IF(NISFR(ISGFILES).EQ.0)THEN
      ALLOCATE(ISDLABELS(SIZE(TATTRIB1))); DO I=1,SIZE(ISDLABELS); ISDLABELS(I)=TATTRIB1(I); ENDDO
     ELSE
      ALLOCATE(ISDLABELS(SIZE(TATTRIB2))); DO I=1,SIZE(ISDLABELS); ISDLABELS(I)=TATTRIB2(I); ENDDO
     ENDIF
    ENDIF
    NV=UTL_COUNT_COLUMNS(LINE,','); NV=NV-2
    !## overwrite default labels
    READ(LINE,*) NISGF(ISGFILES),NISFR(ISGFILES),(ISDLABELS(I),I=1,NV)
   ENDIF
  ENDIF
  DIMISG=DIMISG+NISGF(ISGFILES)
 END DO

 ALLOCATE(OFFSD(0:NISGFILES) ,OFFSC(0:NISGFILES) ,OFFST(0:NISGFILES), &
          OFFSQ(0:NISGFILES) ,OFFISG(0:NISGFILES),OFFISD(0:NISGFILES),OFFISC(0:NISGFILES),&
          OFFIST(0:NISGFILES),OFFISQ(0:NISGFILES))

 !## allocate memory for ISG()
 ALLOCATE(ISG(DIMISG))
 NISG=DIMISG
 ISFR=NISFR(1)
 
 OFFISG=0; OFFSD =0; OFFSC =0; OFFST =0; OFFSQ =0

 !## read segment-file
 IS=0
 ISGLOOP: DO ISGFILES=1,NISGFILES
  DO I=1,NISGF(ISGFILES)
   IS=IS+1
   READ(ISGIU(1,ISGFILES),*,IOSTAT=IOS(1)) ISG(IS)%SNAME,ISG(IS)%ISEG,ISG(IS)%NSEG,ISG(IS)%ICLC,ISG(IS)%NCLC,ISG(IS)%ICRS, &
                                           ISG(IS)%NCRS, ISG(IS)%ISTW,ISG(IS)%NSTW,ISG(IS)%IQHR,ISG(IS)%NQHR
   IF(IOS(1).NE.0)EXIT ISGLOOP
   IF(ISG(IS)%ISEG+ISG(IS)%NSEG.GT.OFFISG(ISGFILES))OFFISG(ISGFILES)=ISG(IS)%ISEG+ISG(IS)%NSEG
   IF(ISG(IS)%ICLC+ISG(IS)%NCLC.GT.OFFSD(ISGFILES)) OFFSD(ISGFILES) =ISG(IS)%ICLC+ISG(IS)%NCLC
   IF(ISG(IS)%ICRS+ISG(IS)%NCRS.GT.OFFSC(ISGFILES)) OFFSC(ISGFILES) =ISG(IS)%ICRS+ISG(IS)%NCRS
   IF(ISG(IS)%ISTW+ISG(IS)%NSTW.GT.OFFST(ISGFILES)) OFFST(ISGFILES) =ISG(IS)%ISTW+ISG(IS)%NSTW
   IF(ISG(IS)%IQHR+ISG(IS)%NQHR.GT.OFFSQ(ISGFILES)) OFFSQ(ISGFILES) =ISG(IS)%IQHR+ISG(IS)%NQHR
  ENDDO
  OFFISG(ISGFILES)=MAX(-1,OFFISG(ISGFILES-1)+OFFISG(ISGFILES)-1)
  OFFSD(ISGFILES) =MAX(-1,OFFSD(ISGFILES-1) +OFFSD(ISGFILES)-1)
  OFFSC(ISGFILES) =MAX(-1,OFFSC(ISGFILES-1) +OFFSC(ISGFILES)-1)
  OFFST(ISGFILES) =MAX(-1,OFFST(ISGFILES-1) +OFFST(ISGFILES)-1)
  OFFSQ(ISGFILES) =MAX(-1,OFFSQ(ISGFILES-1) +OFFSQ(ISGFILES)-1)
 ENDDO ISGLOOP

 IF(IOS(1).NE.0)THEN
  CALL ISGDEAL(1); NISG=-1
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD not read the number '//TRIM(ITOS(ISGFILES))// &
                 ' of the selected isg-files properly!','Error')
  RETURN
 ENDIF

 !## determine amount of memory consumed by *.isp
 DIMISP=SUM(ISG(1:NISG)%NSEG)
 ALLOCATE(ISP(DIMISP),STAT=IOS(1))

 !## determine amount of memory consumed by *.isd part I
 DIMISD=SUM(ISG(1:NISG)%NCLC)
 ALLOCATE(ISD(DIMISD),STAT=IOS(2))

 !## determine amount of memory consumed by *.isc part I
 DIMISC=SUM(ISG(1:NISG)%NCRS)
 ALLOCATE(ISC(DIMISC),STAT=IOS(3))

 !## determine amount of memory consumed by *.ist part I
 DIMIST=SUM(ISG(1:NISG)%NSTW)
 ALLOCATE(IST(DIMIST),STAT=IOS(4))

 !## determine amount of memory consumed by *.isq part I
 DIMISQ=SUM(ISG(1:NISG)%NQHR)
 ALLOCATE(ISQ(DIMISQ),STAT=IOS(5))

 IF(SUM(IOS).NE.0)THEN
  CALL ISGDEAL(1); NISG=-1
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD not able to allocate memory to read entire isg(s)!','Error')
  RETURN
 ENDIF

 !## determine total number of datapoints
 NDISD =0
 OFFISD=0
 IS    =0
 DO ISGFILES=1,NISGFILES
  DO I=1,NISGF(ISGFILES)
   IS=IS+1

   IREC=ISG(IS)%ICLC-1
   DO J=1,ISG(IS)%NCLC
    NISD=IREC+J+OFFSD(ISGFILES-1)
    IF(ISGDOUBLE.EQ.4)THEN
     READ(ISGIU(3,ISGFILES),REC=IREC+J+ICF) ISD(NISD)%N,ISD(NISD)%IREF,DIST          ,ISD(NISD)%CNAME
     ISD(NISD)%DIST=REAL(DIST,8)
    ELSE
     READ(ISGIU(3,ISGFILES),REC=IREC+J+ICF) ISD(NISD)%N,ISD(NISD)%IREF,ISD(NISD)%DIST,ISD(NISD)%CNAME
    ENDIF
    IF(ISD(NISD)%IREF+ISD(NISD)%N.GT.OFFISD(ISGFILES))OFFISD(ISGFILES)=ISD(NISD)%IREF+ISD(NISD)%N
    NDISD=NDISD+ISD(NISD)%N
   ENDDO
   !## correct in case of multiply isg files
   ISG(IS)%ICLC=MAX(0,ISG(IS)%ICLC+OFFSD(ISGFILES-1))

  ENDDO
  OFFISD(ISGFILES)=MAX(-1,OFFISD(ISGFILES-1)+OFFISD(ISGFILES)-1)
 ENDDO

 !## determine total number of cross-section points
 NDISC =0
 OFFISC=0
 IS    =0
 DO ISGFILES=1,NISGFILES
  DO I=1,NISGF(ISGFILES)
   IS=IS+1

   IREC=ISG(IS)%ICRS-1
   DO J=1,ISG(IS)%NCRS
    NISC=IREC+J+OFFSC(ISGFILES-1)
    IF(ISGDOUBLE.EQ.4)THEN
     READ(ISGIU(5,ISGFILES),REC=IREC+J+ICF) ISC(NISC)%N,ISC(NISC)%IREF,DIST,          ISC(NISC)%CNAME
     ISC(NISC)%DIST=REAL(DIST,8)
    ELSE
     READ(ISGIU(5,ISGFILES),REC=IREC+J+ICF) ISC(NISC)%N,ISC(NISC)%IREF,ISC(NISC)%DIST,ISC(NISC)%CNAME
    ENDIF
    IF(ISC(NISC)%IREF+ABS(ISC(NISC)%N).GT.OFFISC(ISGFILES))OFFISC(ISGFILES)=ISC(NISC)%IREF+ABS(ISC(NISC)%N)
    NDISC=NDISC+ABS(ISC(NISC)%N)
   ENDDO

   !## correct in case of multiply isg files
   ISG(IS)%ICRS=MAX(0,ISG(IS)%ICRS+OFFSC(ISGFILES-1))

  ENDDO
  OFFISC(ISGFILES)=MAX(-1,OFFISC(ISGFILES-1)+OFFISC(ISGFILES)-1)
 ENDDO

 !## determine total number of struct points
 NDIST =0
 OFFIST=0
 IS    =0
 DO ISGFILES=1,NISGFILES
  DO I=1,NISGF(ISGFILES)
   IS=IS+1

   IREC=ISG(IS)%ISTW-1
   DO J=1,ISG(IS)%NSTW
    NIST=IREC+J+OFFST(ISGFILES-1)
    IF(ISGDOUBLE.EQ.4)THEN
     READ(ISGIU(7,ISGFILES),REC=IREC+J+ICF) IST(NIST)%N,IST(NIST)%IREF,DIST          ,IST(NIST)%CNAME
     IST(NIST)%DIST=REAL(DIST,8)
    ELSE
     READ(ISGIU(7,ISGFILES),REC=IREC+J+ICF) IST(NIST)%N,IST(NIST)%IREF,IST(NIST)%DIST,IST(NIST)%CNAME
    ENDIF
    IF(IST(NIST)%IREF+IST(NIST)%N.GT.OFFIST(ISGFILES))OFFIST(ISGFILES)=IST(NIST)%IREF+IST(NIST)%N
    NDIST=NDIST+IST(NIST)%N
   ENDDO
   !## correct in case of multiply isg files
   ISG(IS)%ISTW=MAX(0,ISG(IS)%ISTW+OFFST(ISGFILES-1))

  ENDDO
  OFFIST(ISGFILES)=MAX(-1,OFFIST(ISGFILES-1)+OFFIST(ISGFILES)-1)
 ENDDO

 !## determine total number of qh relationships
 NDISQ =0
 OFFISQ=0
 IS    =0
 DO ISGFILES=1,NISGFILES
  DO I=1,NISGF(ISGFILES)
   IS=IS+1

   IREC=ISG(IS)%IQHR-1
   DO J=1,ISG(IS)%NQHR
    NISQ=IREC+J+OFFSQ(ISGFILES-1)
    IF(ISGDOUBLE.EQ.4)THEN
     READ(ISGIU(9,ISGFILES),REC=IREC+J+ICF) ISQ(NISQ)%N,ISQ(NISQ)%IREF,DIST          ,ISQ(NISQ)%CNAME
     ISQ(NISQ)%DIST=REAL(DIST,8)
    ELSE
     READ(ISGIU(9,ISGFILES),REC=IREC+J+ICF) ISQ(NISQ)%N,ISQ(NISQ)%IREF,ISQ(NISQ)%DIST,ISQ(NISQ)%CNAME
    ENDIF
    IF(ISQ(NISQ)%IREF+ISQ(NISQ)%N.GT.OFFISQ(ISGFILES))OFFISQ(ISGFILES)=ISQ(NISQ)%IREF+ISQ(NISQ)%N
    NDISQ=NDISQ+ISQ(NISQ)%N
   ENDDO
   !## correct in case of multiply isg files
   ISG(IS)%IQHR=MAX(0,ISG(IS)%IQHR+OFFSQ(ISGFILES-1))

  ENDDO
  OFFISQ(ISGFILES)=MAX(-1,OFFISQ(ISGFILES-1)+OFFISQ(ISGFILES)-1)
 ENDDO

 DIMDATISD=NDISD; DIMDATISC=NDISC; DIMDATIST=NDIST; DIMDATISQ=NDISQ
 ALLOCATE(DATISD(DIMDATISD)); ALLOCATE(DATISC(DIMDATISC))
 ALLOCATE(DATIST(DIMDATIST)); ALLOCATE(DATISQ(DIMDATISQ))

 !## read *.isp,*.isd,*.isc

 NISD=0; NISC=0; NIST=0; NISP=0; NISQ=0

 IS=0
 DO ISGFILES=1,NISGFILES
  DO J=1,NISGF(ISGFILES)

   IS=IS+1

   CALL ISGREADISP(IS,ISGFILES,OFFISG(ISGFILES-1))
   CALL ISGREADISD(IS,ISGFILES,OFFISD(ISGFILES-1))
   CALL ISGREADISC(IS,ISGFILES,OFFISC(ISGFILES-1))
   CALL ISGREADIST(IS,ISGFILES,OFFIST(ISGFILES-1))
   CALL ISGREADISQ(IS,ISGFILES,OFFISQ(ISGFILES-1))

  END DO
 ENDDO

 CALL ISGDEAL(0)
 
 !## isg files name will be first in line, or empty if isg files were merged
 IF(NISGFILES.EQ.1)THEN
  ISGFNAME=FNAME(1)
 ELSE
  ISGFNAME=''
 ENDIF
 
 ISGREAD=.TRUE.

 END FUNCTION ISGREAD

 !###===============================================================================
 SUBROUTINE ISGREADISP(IS,ISGFILES,IOFF)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IS,ISGFILES,IOFF
 INTEGER :: IREC,JREC,I
 
 !## read segments
 DO IREC=ISG(IS)%ISEG,ISG(IS)%ISEG+ISG(IS)%NSEG-1
  NISP=NISP+1
  JREC=IREC+IOFF
  IF(ISGDOUBLE.EQ.4)THEN
   READ(ISGIU(2,ISGFILES),REC=IREC+ICF) (X_SP(I),I=1,2)
   ISP(JREC)%X=REAL(X_SP(1),8)
   ISP(JREC)%Y=REAL(X_SP(2),8)
  ELSE
   READ(ISGIU(2,ISGFILES),REC=IREC+ICF) ISP(JREC)%X,ISP(JREC)%Y
  ENDIF
 END DO
 ISG(IS)%ISEG=MAX(0,ISG(IS)%ISEG+IOFF)

 END SUBROUTINE ISGREADISP

 !###===============================================================================
 SUBROUTINE ISGREADISD(IS,ISGFILES,IOFF)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IS,ISGFILES,IOFF
 INTEGER :: IREC,JREC,KREC,I,J,K
 
 !## read *.isd

 JREC=ISG(IS)%ICLC-1
 DO I=1,ISG(IS)%NCLC
  NISD=NISD+1
  JREC=JREC+1
  IREC=ISD(JREC)%IREF-1
  DO J=1,ISD(JREC)%N
   IREC =IREC+1
   KREC =IREC+IOFF
   IF(ISFR.EQ.0)THEN
    IF(ISGDOUBLE.EQ.4)THEN
     READ(ISGIU(4,ISGFILES),REC=IREC+ICF) DATISD(KREC)%IDATE,(X_SP(K),K=1,4)
     DATISD(KREC)%WLVL= REAL(X_SP(1),8)
     DATISD(KREC)%BTML= REAL(X_SP(2),8)
     DATISD(KREC)%RESIS=REAL(X_SP(3),8)
     DATISD(KREC)%INFF= REAL(X_SP(4),8)
    ELSE
     READ(ISGIU(4,ISGFILES),REC=IREC+ICF) DATISD(KREC)%IDATE,DATISD(KREC)%WLVL,DATISD(KREC)%BTML, &
                                      DATISD(KREC)%RESIS,DATISD(KREC)%INFF
    ENDIF
   ELSEIF(ISFR.EQ.1)THEN
    IF(ISGDOUBLE.EQ.4)THEN
     READ(ISGIU(4,ISGFILES),REC=IREC+ICF) DATISD(KREC)%IDATE,DATISD(KREC)%CTIME,(X_SP(K),K=1,5), &   
                                      DATISD(KREC)%UPSG, DATISD(KREC)%DWNS, DATISD(KREC)%ICLC, &
                                      DATISD(KREC)%IPRI ,(X_SP(K),K=6,9)
     DATISD(KREC)%WLVL =REAL(X_SP(1),8) 
     DATISD(KREC)%BTML= REAL(X_SP(2),8)
     DATISD(KREC)%WIDTH=REAL(X_SP(3),8)
     DATISD(KREC)%THCK= REAL(X_SP(4),8)
     DATISD(KREC)%HCND= REAL(X_SP(5),8)
     DATISD(KREC)%QFLW= REAL(X_SP(6),8)
     DATISD(KREC)%QROF= REAL(X_SP(7),8)
     DATISD(KREC)%PPTSW=REAL(X_SP(8),8)
     DATISD(KREC)%ETSW =REAL(X_SP(9),8)                       
    ELSE
     READ(ISGIU(4,ISGFILES),REC=IREC+ICF) DATISD(KREC)%IDATE,DATISD(KREC)%CTIME,DATISD(KREC)%WLVL, &   
                                      DATISD(KREC)%BTML, DATISD(KREC)%WIDTH, &
                                      DATISD(KREC)%THCK ,DATISD(KREC)%HCND, &
                                      DATISD(KREC)%UPSG, DATISD(KREC)%DWNS, DATISD(KREC)%ICLC, &
                                      DATISD(KREC)%IPRI ,DATISD(KREC)%QFLW, DATISD(KREC)%QROF, &
                                      DATISD(KREC)%PPTSW, DATISD(KREC)%ETSW
    ENDIF
   ENDIF
  ENDDO
  ISD(JREC)%IREF=MAX(0,ISD(JREC)%IREF+IOFF)
 ENDDO

 END SUBROUTINE ISGREADISD

 !###===============================================================================
 SUBROUTINE ISGREADISC(IS,ISGFILES,IOFF)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IS,ISGFILES,IOFF
 INTEGER :: IREC,JREC,KREC,I,J,K,ICSTYPE
 INTEGER(KIND=2) :: ZM
 INTEGER(KIND=1) :: ZC,ZP
 
 !## read *.isc
 JREC=ISG(IS)%ICRS-1
 DO I=1,ISG(IS)%NCRS
  
  NISC=NISC+1
  JREC=JREC+1
  IREC=ISC(JREC)%IREF-1
  ICSTYPE=0
  
  DO J=1,ABS(ISC(JREC)%N)
   IREC =IREC+1
   KREC =IREC+IOFF
   IF(J.EQ.1.OR.ICSTYPE.EQ.0)THEN
    IF(ISGDOUBLE.EQ.4)THEN
     READ(ISGIU(6,ISGFILES),REC=IREC+ICF) (X_SP(K),K=1,3)
     DATISC(KREC)%DISTANCE=REAL(X_SP(1),8)
     DATISC(KREC)%BOTTOM  =REAL(X_SP(2),8)
     DATISC(KREC)%MRC     =REAL(X_SP(3),8)
    ELSE
     READ(ISGIU(6,ISGFILES),REC=IREC+ICF) DATISC(KREC)%DISTANCE,DATISC(KREC)%BOTTOM,DATISC(KREC)%MRC
    ENDIF
    !## read bathemetry with thressholds
    IF(DATISC(KREC)%DISTANCE.LT.0.AND.DATISC(KREC)%BOTTOM.LT.0.0D0)ICSTYPE=1
    DATISC(KREC)%ZP=0.0D0 
   ELSE
    IF(ISGDOUBLE.EQ.4)THEN
     READ(ISGIU(6,ISGFILES),REC=IREC+ICF) (X_SP(K),K=1,2),ZM,ZC,ZP 
     DATISC(KREC)%DISTANCE=REAL(X_SP(1),8)
     DATISC(KREC)%BOTTOM  =REAL(X_SP(2),8)
    ELSE
     READ(ISGIU(6,ISGFILES),REC=IREC+ICF) DATISC(KREC)%DISTANCE,DATISC(KREC)%BOTTOM,ZM,ZC,ZP 
    ENDIF
    DATISC(KREC)%MRC=REAL(ZM)+REAL(ZC)/100.0D0
    DATISC(KREC)%ZP=REAL(ZP) 
   ENDIF
  END DO
  ISC(JREC)%IREF=MAX(0,ISC(JREC)%IREF+IOFF)
 END DO

 END SUBROUTINE ISGREADISC

 !###===============================================================================
 SUBROUTINE ISGREADIST(IS,ISGFILES,IOFF)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IS,ISGFILES,IOFF
 INTEGER :: IREC,JREC,KREC,I,J,K

 !## read *.ist

 JREC=ISG(IS)%ISTW-1
 DO I=1,ISG(IS)%NSTW
  NIST=NIST+1
  JREC=JREC+1
  IREC=IST(JREC)%IREF-1
  DO J=1,IST(JREC)%N
   IREC =IREC+1
   KREC =IREC+IOFF
   IF(ISGDOUBLE.EQ.4)THEN
    READ(ISGIU(8,ISGFILES),REC=IREC+ICF) (X_SP(K),K=1,3)
    DATIST(KREC)%IDATE    =REAL(X_SP(1),8)
    DATIST(KREC)%WLVL_UP  =REAL(X_SP(2),8)
    DATIST(KREC)%WLVL_DOWN=REAL(X_SP(3),8)
   ELSE
    READ(ISGIU(8,ISGFILES),REC=IREC+ICF) DATIST(KREC)%IDATE,DATIST(KREC)%WLVL_UP,DATIST(KREC)%WLVL_DOWN
   ENDIF
  END DO
  IST(JREC)%IREF=MAX(0,IST(JREC)%IREF+IOFF)
 END DO

 END SUBROUTINE ISGREADIST

 !###===============================================================================
 SUBROUTINE ISGREADISQ(IS,ISGFILES,IOFF)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IS,ISGFILES,IOFF
 INTEGER :: IREC,JREC,KREC,I,J,K

 !## read *.isq

 JREC=ISG(IS)%IQHR-1
 DO I=1,ISG(IS)%NQHR
  NISQ=NISQ+1
  JREC=JREC+1
  IREC=ISQ(JREC)%IREF-1
  DO J=1,ISQ(JREC)%N
   IREC =IREC+1
   KREC =IREC+IOFF
   IF(ISGDOUBLE.EQ.4)THEN
    READ(ISGIU(10,ISGFILES),REC=IREC+ICF) (X_SP(K),K=1,4)
    DATISQ(KREC)%Q=REAL(X_SP(1),8)
    DATISQ(KREC)%W=REAL(X_SP(2),8)
    DATISQ(KREC)%D=REAL(X_SP(3),8)
    DATISQ(KREC)%F=REAL(X_SP(4),8)
   ELSE
    READ(ISGIU(10,ISGFILES),REC=IREC+ICF) DATISQ(KREC)%Q,DATISQ(KREC)%W,DATISQ(KREC)%D,DATISQ(KREC)%F
   ENDIF
  END DO
  ISQ(JREC)%IREF=MAX(0,ISQ(JREC)%IREF+IOFF)
 END DO

 END SUBROUTINE ISGREADISQ

 !###====================================================================
 LOGICAL FUNCTION ISGATTRIBUTESREADISDVALUE()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,L

 ISGATTRIBUTESREADISDVALUE=.FALSE.

 IF(ALLOCATED(DATISD2))DEALLOCATE(DATISD2)
 IF(ALLOCATED(TISD))DEALLOCATE(TISD)
 ALLOCATE(DATISD2(ISG(ISELISG)%NCLC,ISDMAXROW))
 ALLOCATE(TISD(ISG(ISELISG)%NCLC))

 K=ISG(ISELISG)%ICLC-1

 I=K+1
 J=K+ISG(ISELISG)%NCLC
 CALL WDIALOGSELECT(ID_DISGATTRIBUTESTAB1)
 CALL WDIALOGPUTMENU(IDF_MENU1,ISD(I:J)%CNAME,ISG(ISELISG)%NCLC,1)

 DO I=1,ISG(ISELISG)%NCLC

  K=K+1

  IF(ISDMAXROW.LT.ISD(K)%N)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Maximum number of records for Waterlevels '//TRIM(ISD(K)%CNAME)//' is '//TRIM(ITOS(ISDMAXROW))//CHAR(13)// &
                   'iMOD is now reading '//TRIM(ITOS(ISD(K)%N))//' records, rest will be left out!.'//CHAR(13)// &
                    'Be aware that once you save the properties, the rest will be ignored','Warning')
  ENDIF

  TISD(I)=0
  L      =ISD(K)%IREF-1

  DO J=1,MIN(ISDMAXROW,ISD(K)%N)

   L           =L+1
   TISD(I)     =TISD(I)+1
   DATISD2(I,J)=DATISD(L)

  ENDDO

 ENDDO

 ISGATTRIBUTESREADISDVALUE=.TRUE.

 END FUNCTION ISGATTRIBUTESREADISDVALUE

 !###====================================================================
 LOGICAL FUNCTION ISGATTRIBUTESREADISCVALUE(IDIALOG)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDIALOG
 INTEGER :: I,J,K,L

 ISGATTRIBUTESREADISCVALUE=.FALSE.

 IF(ALLOCATED(DATISC2))DEALLOCATE(DATISC2)
 IF(ALLOCATED(TISC))DEALLOCATE(TISC)
 IF(ALLOCATED(ISCN))DEALLOCATE(ISCN)
 
 IF(IDIALOG.EQ.0)THEN
  K=ISG(ISELISG)%ICRS; L=K+ISG(ISELISG)%NCRS-1
  ISCMAXROW=MAXVAL(ABS(ISC(K:L)%N))
 ENDIF
 ALLOCATE(DATISC2(ISG(ISELISG)%NCRS,ISCMAXROW))
 ALLOCATE(TISC(ISG(ISELISG)%NCRS),ISCN(ISG(ISELISG)%NCRS))

 K=ISG(ISELISG)%ICRS-1

 I=K+1
 J=K+ISG(ISELISG)%NCRS
 
 IF(IDIALOG.GT.0)THEN
  CALL WDIALOGSELECT(IDIALOG)
  CALL WDIALOGPUTMENU(IDF_MENU1,ISC(I:J)%CNAME,ISG(ISELISG)%NCRS,1)
 ENDIF
 
 DO I=1,ISG(ISELISG)%NCRS

  K=K+1

  IF(IDIALOG.GT.0)THEN
   IF(ISCMAXROW.LT.ABS(ISC(K)%N))THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Maximum number of records for Cross-Sections '//TRIM(ISC(K)%CNAME)//' is '//TRIM(ITOS(ISCMAXROW))//CHAR(13)// &
                    'iMOD is now reading '//TRIM(ITOS(ABS(ISC(K)%N)))//' records, rest will be left out.'//CHAR(13)// &
                    'Be aware that once you save the properties, the rest will be ignored','Warning')
   ENDIF
  ENDIF
  
  TISC(I)=0; ISCN(I)=SIGN(1,ISC(K)%N)
  
  L      =ISC(K)%IREF-1

  DO J=1,MIN(ISCMAXROW,ABS(ISC(K)%N))

   L           =L+1
   TISC(I)     =TISC(I)+1
   DATISC2(I,J)=DATISC(L)

  ENDDO
  
 ENDDO

 ISGATTRIBUTESREADISCVALUE=.TRUE.

 END FUNCTION ISGATTRIBUTESREADISCVALUE

 !###====================================================================
 LOGICAL FUNCTION ISGATTRIBUTESREADISTVALUE()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,L

 ISGATTRIBUTESREADISTVALUE=.TRUE.
 
 IF(ALLOCATED(DATIST2))DEALLOCATE(DATIST2)
 IF(ALLOCATED(TIST))DEALLOCATE(TIST)

 I=1
 IF(ISG(ISELISG)%NSTW.EQ.0)I=0
 CALL WDIALOGSELECT(ID_DISGATTRIBUTES)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DISGATTRIBUTESTAB4,I)
 IF(I.EQ.0)RETURN

 ISGATTRIBUTESREADISTVALUE=.FALSE.
 
 ALLOCATE(DATIST2(ISG(ISELISG)%NSTW,ISTMAXROW))
 ALLOCATE(TIST(ISG(ISELISG)%NSTW))

 K=ISG(ISELISG)%ISTW-1

 I=K+1
 J=K+ISG(ISELISG)%NSTW
 CALL WDIALOGSELECT(ID_DISGATTRIBUTESTAB4)
 CALL WDIALOGPUTMENU(IDF_MENU1,IST(I:J)%CNAME,ISG(ISELISG)%NSTW,1)

 DO I=1,ISG(ISELISG)%NSTW

  K=K+1

  IF(ISTMAXROW.LT.IST(K)%N)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Maximum number of records for Structures '//TRIM(IST(K)%CNAME)//' is '//TRIM(ITOS(ISTMAXROW))//CHAR(13)// &
                   'iMOD is now reading '//TRIM(ITOS(IST(K)%N))//' records, rest will be left out!'//CHAR(13)// &
                    'Be aware that once you save the properties, the rest will be ignored','Warning')
  ENDIF

  TIST(I)=0
  L      =IST(K)%IREF-1

  DO J=1,MIN(ISTMAXROW,IST(K)%N)

   L           =L+1
   TIST(I)     =TIST(I)+1
   DATIST2(I,J)=DATIST(L)

  ENDDO

 ENDDO

 ISGATTRIBUTESREADISTVALUE=.TRUE.

 END FUNCTION ISGATTRIBUTESREADISTVALUE

 !###====================================================================
 LOGICAL FUNCTION ISGATTRIBUTESREADISQVALUE()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,L

 ISGATTRIBUTESREADISQVALUE=.TRUE.

 IF(ALLOCATED(DATISQ2))DEALLOCATE(DATISQ2)
 IF(ALLOCATED(TISQ))DEALLOCATE(TISQ)

 I=1
 IF(ISG(ISELISG)%NQHR.EQ.0)I=0
 CALL WDIALOGSELECT(ID_DISGATTRIBUTES)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DISGATTRIBUTESTAB5,I)
 IF(I.EQ.0)RETURN

 ISGATTRIBUTESREADISQVALUE=.FALSE.
 
 ALLOCATE(DATISQ2(ISG(ISELISG)%NQHR,ISQMAXROW))
 ALLOCATE(TISQ(ISG(ISELISG)%NQHR))

 K=ISG(ISELISG)%IQHR-1

 I=K+1
 J=K+ISG(ISELISG)%NQHR
 CALL WDIALOGSELECT(ID_DISGATTRIBUTESTAB5)
 CALL WDIALOGPUTMENU(IDF_MENU1,ISQ(I:J)%CNAME,ISG(ISELISG)%NQHR,1)

 DO I=1,ISG(ISELISG)%NQHR

  K=K+1

  IF(ISQMAXROW.LT.ISQ(K)%N)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Maximum number of records for Relationships '//TRIM(ISQ(K)%CNAME)//' is '//TRIM(ITOS(ISQMAXROW))//CHAR(13)// &
                   'iMOD is now reading '//TRIM(ITOS(ISQ(K)%N))//' records, rest will be left out!'//CHAR(13)// &
                    'Be aware that once you save the properties, the rest will be ignored','Warning')
  ENDIF

  TISQ(I)=0
  L      =ISQ(K)%IREF-1

  DO J=1,MIN(ISQMAXROW,ISQ(K)%N)

   L           =L+1
   TISQ(I)     =TISQ(I)+1
   DATISQ2(I,J)=DATISQ(L)

  ENDDO

 ENDDO

 ISGATTRIBUTESREADISQVALUE=.TRUE.

 END FUNCTION ISGATTRIBUTESREADISQVALUE

 !###====================================================================
 LOGICAL FUNCTION ISGATTRIBUTESREADISPVALUE()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: ISEG,NSEG,I

 ISGATTRIBUTESREADISPVALUE=.FALSE.

 IF(ALLOCATED(ISP2))DEALLOCATE(ISP2)
 ALLOCATE(ISP2(ISPMAXROW))

 NSEG=ISG(ISELISG)%NSEG

 IF(ISPMAXROW.LT.NSEG)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Maximum number of records for Segment Points is '//TRIM(ITOS(ISPMAXROW))//CHAR(13)// &
                  'iMOD wants to read '//TRIM(ITOS(NSEG))//' records, rest will be left out!'//CHAR(13)// &
                  'Be aware that once you save the properties, the rest will be ignored','Warning')
 ENDIF

 ISEG=ISG(ISELISG)%ISEG-1
 TISP=0
 DO I=1,MIN(ISPMAXROW,NSEG)

  TISP   =TISP+1
  ISP2(I)=ISP(ISEG+I)

 ENDDO

 ISGATTRIBUTESREADISPVALUE=.TRUE.

 END FUNCTION ISGATTRIBUTESREADISPVALUE
 
 !###====================================================================
 SUBROUTINE ISGDELISD(IISG,IPOS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IISG,IPOS
 INTEGER :: IREF,N,I,J

 !## remove from current ipos
 IREF            =ISD(IPOS)%IREF
 N               =ISD(IPOS)%N

 !## adjust isd variable
 DO I=IPOS,NISD-1
  ISD(I)=ISD(I+1)
 ENDDO
 NISD            =NISD-1
 ISG(IISG)%NCLC  =ISG(IISG)%NCLC-1

 !## adjust other references to selected calc.pnt definition from ISG
 DO I=1,NISG
  IF(ISG(I)%ICLC.GT.ISG(IISG)%ICLC)ISG(I)%ICLC=ISG(I)%ICLC-1
 END DO

 IF(ISG(IISG)%NCLC.EQ.0)ISG(IISG)%ICLC=0

 !## find other references to selected calc.pnt definition
 DO J=1,NISD
  IF(ISD(J)%IREF.EQ.IREF)EXIT
 ENDDO

 !## remove from iscdat = only whenever no other refers to calc.pnt
 IF(J.GT.NISD.AND.N.GT.0)THEN
  DO I=IREF,NDISD-N
   DATISD(I)=DATISD(I+N)
  ENDDO
  NDISD               =NDISD-N
  !## adjust other references to selected calc.pnt definition
  DO I=1,NISD
   IF(ISD(I)%IREF.GT.IREF)ISD(I)%IREF=ISD(I)%IREF-N
  ENDDO
 ENDIF

 END SUBROUTINE ISGDELISD

 !###====================================================================
 SUBROUTINE ISGDELISC(IISG,IPOS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IISG,IPOS
 INTEGER :: IREF,N,I,J

 !## remove from current ipos
 IREF            =ISC(IPOS)%IREF
 N               =ABS(ISC(IPOS)%N)

 !## adjust isc variable
 DO I=IPOS,NISC-1
  ISC(I)=ISC(I+1)
 ENDDO
 NISC            =NISC-1
 ISG(IISG)%NCRS  =ISG(IISG)%NCRS-1

 !## adjust other references to selected calc.pnt definition from ISG
 DO I=1,NISG
  IF(ISG(I)%ICRS.GT.ISG(IISG)%ICRS)ISG(I)%ICRS=ISG(I)%ICRS-1
 END DO

 IF(ISG(IISG)%NCRS.EQ.0)ISG(IISG)%ICRS=0

 !## find other references to selected cross-section definition
 DO J=1,NISC
  IF(ISC(J)%IREF.EQ.IREF)EXIT
 ENDDO

 !## remove from iscdat = only whenever no other refers to cross-section
 IF(J.GT.NISC.AND.N.GT.0)THEN
  DO I=IREF,NDISC-N
   DATISC(I)=DATISC(I+N)
  ENDDO
  NDISC               =NDISC-N
  !## adjust other references to selected cross-section definition
  DO I=1,NISC
   IF(ISC(I)%IREF.GT.IREF)ISC(I)%IREF=ISC(I)%IREF-N
  ENDDO
 ENDIF

 END SUBROUTINE ISGDELISC

 !###====================================================================
 SUBROUTINE ISGDELIST(IISG,IPOS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IISG,IPOS
 INTEGER :: IREF,N,I,J

 !## remove from current ipos
 IREF            =IST(IPOS)%IREF
 N               =IST(IPOS)%N

 !## adjust ist variable
 DO I=IPOS,NIST-1
  IST(I)=IST(I+1)
 ENDDO
 NIST            =NIST-1
 ISG(IISG)%NSTW  =ISG(IISG)%NSTW-1

 !## adjust other references to selected calc.pnt definition from ISG
 DO I=1,NISG
  IF(ISG(I)%ISTW.GT.ISG(IISG)%ISTW)ISG(I)%ISTW=ISG(I)%ISTW-1
 END DO

 IF(ISG(IISG)%NSTW.EQ.0)ISG(IISG)%ISTW=0

 !## find other references to selected cross-section definition
 DO J=1,NIST
  IF(IST(J)%IREF.EQ.IREF)EXIT
 ENDDO

 !## remove from iscdat = only whenever no other refers to cross-section
 IF(J.GT.NIST.AND.N.GT.0)THEN
  DO I=IREF,NDIST-N
   DATIST(I)=DATIST(I+N)
  ENDDO
  NDIST               =NDIST-N
 !## adjust other references to selected cross-section definition
  DO I=1,NIST
   IF(IST(I)%IREF.GT.IREF)IST(I)%IREF=IST(I)%IREF-N
  ENDDO
 ENDIF

 END SUBROUTINE ISGDELIST

 !###====================================================================
 SUBROUTINE ISGDELISQ(IISG,IPOS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IISG,IPOS
 INTEGER :: IREF,N,I,J

 !## remove from current ipos
 IREF            =ISQ(IPOS)%IREF
 N               =ISQ(IPOS)%N

 !## adjust isq variable
 DO I=IPOS,NISQ-1
  ISQ(I)=ISQ(I+1)
 ENDDO
 NISQ            =NISQ-1
 ISG(IISG)%NQHR  =ISG(IISG)%NQHR-1

 !## adjust other references to selected calc.pnt definition from ISG
 DO I=1,NISG
  IF(ISG(I)%IQHR.GT.ISG(IISG)%IQHR)ISG(I)%IQHR=ISG(I)%IQHR-1
 END DO

 IF(ISG(IISG)%NQHR.EQ.0)ISG(IISG)%IQHR=0

 !## find other references to selected cross-section definition
 DO J=1,NISQ
  IF(ISQ(J)%IREF.EQ.IREF)EXIT
 ENDDO

 !## remove from iscdat = only whenever no other refers to cross-section
 IF(J.GT.NISQ.AND.N.GT.0)THEN
  DO I=IREF,NDISQ-N
   DATISQ(I)=DATISQ(I+N)
  ENDDO
  NDISQ               =NDISQ-N
 !## adjust other references to selected cross-section definition
  DO I=1,NISQ
   IF(ISQ(I)%IREF.GT.IREF)ISQ(I)%IREF=ISQ(I)%IREF-N
  ENDDO
 ENDIF

 END SUBROUTINE ISGDELISQ

 !###====================================================================
 SUBROUTINE ISGDELISP(IISG)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IISG
 INTEGER :: ISEG,NSEG,I

 ISEG=ISG(IISG)%ISEG
 NSEG=ISG(IISG)%NSEG

 !##adjust isp variable
 DO I=ISEG,NISP-NSEG
  ISP(I)=ISP(I+NSEG)
 ENDDO
 NISP               =NISP-NSEG
 ISG(IISG)%NSEG     =ISG(IISG)%NSEG-NSEG

 !##adjust other references to selected nodes definition from ISG
 DO I=1,NISG
  IF(ISG(I)%ISEG.GT.ISG(IISG)%ISEG)ISG(I)%ISEG=ISG(I)%ISEG-NSEG
 END DO

 END SUBROUTINE ISGDELISP

 !###====================================================================
 SUBROUTINE ISGGETPOSID(TDIST,IPOS,ITYPE)
 !###====================================================================
 !determine id of nearest feature (crosssection/calculationpoint)
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IPOS
 INTEGER,INTENT(IN) :: ITYPE
 REAL(KIND=DP_KIND),INTENT(IN) :: TDIST
 REAL(KIND=DP_KIND) :: DIST,MINDIST
 INTEGER :: IREC,NREC,I

 IF(ITYPE.EQ.1)THEN
  IREC=ISG(ISELISG)%ICLC-1
  NREC=ISG(ISELISG)%NCLC
 ELSEIF(ITYPE.EQ.2)THEN
  IREC=ISG(ISELISG)%ICRS-1
  NREC=ISG(ISELISG)%NCRS
 ELSEIF(ITYPE.EQ.3)THEN
  IREC=ISG(ISELISG)%ISTW-1
  NREC=ISG(ISELISG)%NSTW
 ELSEIF(ITYPE.EQ.4)THEN
  IREC=ISG(ISELISG)%IQHR-1
  NREC=ISG(ISELISG)%NQHR
 ENDIF

 !## initial value
 IPOS=1
 DO I=1,NREC
  IREC=IREC+1
  IF(ITYPE.EQ.1)DIST=ABS(ISD(IREC)%DIST-TDIST)
  IF(ITYPE.EQ.2)DIST=ABS(ISC(IREC)%DIST-TDIST)
  IF(ITYPE.EQ.3)DIST=ABS(IST(IREC)%DIST-TDIST)
  IF(ITYPE.EQ.4)DIST=ABS(ISQ(IREC)%DIST-TDIST)
  IF(I.EQ.1)THEN
   MINDIST=DIST
   IPOS   =IREC
  ELSE
   IF(DIST.LE.MINDIST)THEN
    MINDIST=DIST
    IPOS   =IREC
   ENDIF
  ENDIF
 ENDDO

 END SUBROUTINE ISGGETPOSID

 !###====================================================================
 SUBROUTINE ISGGETPOSDISTANCE(XINTER,YINTER,TDIST,IDIST)
 !###====================================================================
 !determine cumulative distance on segment for current location xinter/yinter
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDIST
 REAL(KIND=DP_KIND),INTENT(OUT) :: TDIST
 REAL(KIND=DP_KIND),INTENT(IN) :: XINTER,YINTER
 REAL(KIND=DP_KIND) :: DIST
 INTEGER :: ISEG,NSEG,I

 !## enough cross-sections both side of split-point?
 ISEG =ISG(ISELISG)%ISEG
 NSEG =ISG(ISELISG)%NSEG
 TDIST=0.0D0
 DO I=2,NSEG
  IF(ISEG+1.EQ.IDIST)THEN
   ISEG=ISEG+1
   DIST=(XINTER-ISP(ISEG-1)%X)**2.0+(YINTER-ISP(ISEG-1)%Y)**2.0
   IF(DIST.GT.0.0D0)DIST=SQRT(DIST)
   TDIST=TDIST+DIST
   EXIT
  ENDIF
  ISEG=ISEG+1
  DIST=(ISP(ISEG)%X-ISP(ISEG-1)%X)**2.0+(ISP(ISEG)%Y-ISP(ISEG-1)%Y)**2.0
  IF(DIST.GT.0.0D0)DIST=SQRT(DIST)
  TDIST=TDIST+DIST
 END DO

 END SUBROUTINE ISGGETPOSDISTANCE

 !###===============================================================================
 SUBROUTINE ISGSAVE(ISGFILE,ISAVE) !- saving ONLY *.ISG, *.isp, *.isd, *.isc
 !###===============================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: ISGFILE
 INTEGER,INTENT(IN) :: ISAVE

 IF(ISAVE.EQ.0)THEN
  CALL WMESSAGEBOX(YESNOCANCEL,QUESTIONICON,COMMONNO,'Do you want to SAVE adjustments to: '//CHAR(13)// &
      TRIM(ISGFILE)//' ?','Question')
  IF(WINFODIALOG(4).NE.1)RETURN
 ELSEIF(ISAVE.EQ.1)THEN
  IF(.NOT.UTL_WSELECTFILE('iMOD segment-River File (*.isg)|*.isg|',&
                   SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,ISGFILE,&
                   'Save iMOD segment-River File (*.isg)'))RETURN
 ENDIF

 IF(.NOT.ISGOPENFILES(ISGFILE,'REPLACE'))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot (re)write ISG file:'//CHAR(13)//TRIM(ISGFILE),'Error')
  RETURN
 ENDIF

 CALL UTL_MESSAGEHANDLE(0)
 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,'Saving '//TRIM(ISGFILE)//'...')

 CALL ISGSAVEIT()

 CALL UTL_MESSAGEHANDLE(1)

 CALL ISGCLOSEFILES()
 
 END SUBROUTINE ISGSAVE

 !###===============================================================================
 SUBROUTINE ISGSAVEIT() !- saving ONLY *.ISG, *.isp, *.isd, *.isc
 !###===============================================================================
 IMPLICIT NONE
 INTEGER :: IS,I,J,K,IREC,JREC
 INTEGER(KIND=2) :: ZM
 INTEGER(KIND=1) :: ZC,ZP
 CHARACTER(LEN=256) :: LINE

 !## write header *.ISG file
 LINE=TRIM(ITOS(NISG))//','//TRIM(ITOS(ISFR))
 IF(ALLOCATED(ISDLABELS))THEN
  DO I=1,SIZE(ISDLABELS)
   LINE=TRIM(LINE)//',"'//TRIM(ISDLABELS(I))//'"'
  ENDDO
 ENDIF
 WRITE(ISGIU(1,1),'(A)') TRIM(LINE)

 !## write header *.ISG file
 CALL ISGSAVEHEADERS()

 DO IS=1,NISG

  LINE='"'//TRIM(ISG(IS)%SNAME)//'",'//TRIM(ITOS(ISG(IS)%ISEG))//','//TRIM(ITOS(ISG(IS)%NSEG))//','// &
                                       TRIM(ITOS(ISG(IS)%ICLC))//','//TRIM(ITOS(ISG(IS)%NCLC))//','// &
                                       TRIM(ITOS(ISG(IS)%ICRS))//','//TRIM(ITOS(ISG(IS)%NCRS))//','// &
                                       TRIM(ITOS(ISG(IS)%ISTW))//','//TRIM(ITOS(ISG(IS)%NSTW))//','// &
                                       TRIM(ITOS(ISG(IS)%IQHR))//','//TRIM(ITOS(ISG(IS)%NQHR))
  WRITE(ISGIU(1,1),'(A)') TRIM(LINE)

  !## write segments
  IREC=ISG(IS)%ISEG-1
  DO I=1,ISG(IS)%NSEG
   IREC=IREC+1
   IF(ISGDOUBLE.EQ.4)THEN
    X_SP(1)=REAL(ISP(IREC)%X,4)
    X_SP(2)=REAL(ISP(IREC)%Y,4)
    WRITE(ISGIU(2,1),REC=IREC+ICF) (X_SP(K),K=1,2)
   ELSE
    WRITE(ISGIU(2,1),REC=IREC+ICF) ISP(IREC)%X,ISP(IREC)%Y
   ENDIF
  END DO

  !## write isd1
  IREC=ISG(IS)%ICLC-1
  DO I=1,ISG(IS)%NCLC
   IREC=IREC+1
   IF(ISGDOUBLE.EQ.4)THEN
    X_SP(1)=REAL(ISD(IREC)%DIST,4)
    WRITE(ISGIU(3,1),REC=IREC+ICF) ISD(IREC)%N,ISD(IREC)%IREF,X_SP(1)       ,ISD(IREC)%CNAME
   ELSE
    WRITE(ISGIU(3,1),REC=IREC+ICF) ISD(IREC)%N,ISD(IREC)%IREF,ISD(IREC)%DIST,ISD(IREC)%CNAME
   ENDIF
   !## write isd2
   JREC=ISD(IREC)%IREF-1
   DO J=1,ISD(IREC)%N
    JREC=JREC+1
    IF(ISFR.EQ.0)THEN
     IF(ISGDOUBLE.EQ.4)THEN
      X_SP(1)=REAL(DATISD(JREC)%WLVL ,4)
      X_SP(2)=REAL(DATISD(JREC)%BTML ,4)
      X_SP(3)=REAL(DATISD(JREC)%RESIS,4)
      X_SP(4)=REAL(DATISD(JREC)%INFF ,4)
      WRITE(ISGIU(4,1),REC=JREC+ICF) DATISD(JREC)%IDATE,(X_SP(K),K=1,4)
     ELSE
      WRITE(ISGIU(4,1),REC=JREC+ICF) DATISD(JREC)%IDATE,DATISD(JREC)%WLVL, &
                                 DATISD(JREC)%BTML,DATISD(JREC)%RESIS,DATISD(JREC)%INFF
     ENDIF
    ELSEIF(ISFR.EQ.1)THEN
     IF(ISGDOUBLE.EQ.4)THEN
      X_SP(1)=REAL(DATISD(JREC)%WLVL ,4)
      X_SP(2)=REAL(DATISD(JREC)%BTML ,4)
      X_SP(3)=REAL(DATISD(JREC)%WIDTH,4)
      X_SP(4)=REAL(DATISD(JREC)%THCK ,4)
      X_SP(5)=REAL(DATISD(JREC)%HCND ,4)
      X_SP(6)=REAL(DATISD(JREC)%QFLW ,4)
      X_SP(7)=REAL(DATISD(JREC)%QROF ,4)
      X_SP(8)=REAL(DATISD(JREC)%PPTSW,4)
      X_SP(9)=REAL(DATISD(JREC)%ETSW ,4)
      WRITE(ISGIU(4,1),REC=JREC+ICF) DATISD(JREC)%IDATE,DATISD(JREC)%CTIME,(X_SP(K),K=1,5), &
                                 DATISD(JREC)%UPSG, DATISD(JREC)%DWNS ,DATISD(JREC)%ICLC, &
                                 DATISD(JREC)%IPRI,(X_SP(K),K=6,9)
     ELSE
      WRITE(ISGIU(4,1),REC=JREC+ICF) DATISD(JREC)%IDATE,DATISD(JREC)%CTIME,DATISD(JREC)%WLVL, &
                                 DATISD(JREC)%BTML, DATISD(JREC)%WIDTH, &
                                 DATISD(JREC)%THCK ,DATISD(JREC)%HCND, &
                                 DATISD(JREC)%UPSG, DATISD(JREC)%DWNS ,DATISD(JREC)%ICLC, &
                                 DATISD(JREC)%IPRI, DATISD(JREC)%QFLW ,DATISD(JREC)%QROF, &
                                 DATISD(JREC)%PPTSW, DATISD(JREC)%ETSW
     ENDIF
    ENDIF
   ENDDO
  ENDDO

  !## write isc1
  IREC=ISG(IS)%ICRS-1
  DO I=1,ISG(IS)%NCRS
   IREC=IREC+1
   IF(ISGDOUBLE.EQ.4)THEN
    X_SP(1)=REAL(ISC(IREC)%DIST,4)
    WRITE(ISGIU(5,1),REC=IREC+ICF) ISC(IREC)%N,ISC(IREC)%IREF,X_SP(1)       ,ISC(IREC)%CNAME
   ELSE
    WRITE(ISGIU(5,1),REC=IREC+ICF) ISC(IREC)%N,ISC(IREC)%IREF,ISC(IREC)%DIST,ISC(IREC)%CNAME
   ENDIF
   !## write isc2
   JREC=ISC(IREC)%IREF-1
   
   IF(ABS(ISC(IREC)%N).GT.0)THEN
   
    JREC=JREC+1
    IF(ISGDOUBLE.EQ.4)THEN
     X_SP(1)=REAL(DATISC(JREC)%DISTANCE,4)
     X_SP(2)=REAL(DATISC(JREC)%BOTTOM  ,4)
     X_SP(3)=REAL(DATISC(JREC)%MRC     ,4)
     WRITE(ISGIU(6,1),REC=JREC+ICF) (X_SP(K),K=1,3)   
    ELSE
     WRITE(ISGIU(6,1),REC=JREC+ICF) DATISC(JREC)%DISTANCE,DATISC(JREC)%BOTTOM,DATISC(JREC)%MRC   
    ENDIF
    IF(DATISC(JREC)%DISTANCE.LT.0.0D0.AND.DATISC(JREC)%BOTTOM.LT.0.0D0)THEN
     DO J=2,ABS(ISC(IREC)%N)
      JREC=JREC+1
      ZM=INT(DATISC(JREC)%MRC,2)
      ZC=INT(100.0D0*(DATISC(JREC)%MRC-REAL(ZM)),1)
      ZP=INT(DATISC(JREC)%ZP,1)
      IF(ISGDOUBLE.EQ.4)THEN
       X_SP(1)=REAL(DATISC(JREC)%DISTANCE,4)
       X_SP(2)=REAL(DATISC(JREC)%BOTTOM  ,4)
       WRITE(ISGIU(6,1),REC=JREC+ICF) (X_SP(K),K=1,2),ZM,ZC,ZP
      ELSE
       WRITE(ISGIU(6,1),REC=JREC+ICF) DATISC(JREC)%DISTANCE,DATISC(JREC)%BOTTOM,ZM,ZC,ZP
      ENDIF
     ENDDO 
    ELSE   
     DO J=2,ABS(ISC(IREC)%N)
      JREC=JREC+1
      IF(ISGDOUBLE.EQ.4)THEN
       X_SP(1)=REAL(DATISC(JREC)%DISTANCE,4)
       X_SP(2)=REAL(DATISC(JREC)%BOTTOM  ,4)
       X_SP(3)=REAL(DATISC(JREC)%MRC     ,4)
       WRITE(ISGIU(6,1),REC=JREC+ICF) (X_SP(K),K=1,3)   
      ELSE
       WRITE(ISGIU(6,1),REC=JREC+ICF) DATISC(JREC)%DISTANCE,DATISC(JREC)%BOTTOM,DATISC(JREC)%MRC
      ENDIF
     ENDDO
    ENDIF
   
   ENDIF  
  ENDDO

  !## write ist1
  IREC=ISG(IS)%ISTW-1
  DO I=1,ISG(IS)%NSTW
   IREC=IREC+1
   IF(ISGDOUBLE.EQ.4)THEN
    X_SP(1)=REAL(IST(IREC)%DIST,4)
    WRITE(ISGIU(7,1),REC=IREC+ICF) IST(IREC)%N,IST(IREC)%IREF,X_SP(1)       ,IST(IREC)%CNAME
   ELSE
    WRITE(ISGIU(7,1),REC=IREC+ICF) IST(IREC)%N,IST(IREC)%IREF,IST(IREC)%DIST,IST(IREC)%CNAME
   ENDIF
   !## write isc2
   JREC=IST(IREC)%IREF-1
   DO J=1,IST(IREC)%N
    JREC=JREC+1
    IF(ISGDOUBLE.EQ.4)THEN
     X_SP(1)=REAL(DATIST(JREC)%WLVL_UP  ,4)
     X_SP(2)=REAL(DATIST(JREC)%WLVL_DOWN,4)
     WRITE(ISGIU(8,1),REC=JREC+ICF) DATIST(JREC)%IDATE,(X_SP(K),K=1,2)
    ELSE
     WRITE(ISGIU(8,1),REC=JREC+ICF) DATIST(JREC)%IDATE,DATIST(JREC)%WLVL_UP,DATIST(JREC)%WLVL_DOWN
    ENDIF
   ENDDO
  ENDDO

  !## write isq1
  IREC=ISG(IS)%IQHR-1
  DO I=1,ISG(IS)%NQHR
   IREC=IREC+1
   IF(ISGDOUBLE.EQ.4)THEN
    X_SP(1)=REAL(ISQ(IREC)%DIST,4)
    WRITE(ISGIU(9,1),REC=IREC+ICF) ISQ(IREC)%N,ISQ(IREC)%IREF,X_SP(1)       ,ISQ(IREC)%CNAME
   ELSE
    WRITE(ISGIU(9,1),REC=IREC+ICF) ISQ(IREC)%N,ISQ(IREC)%IREF,ISQ(IREC)%DIST,ISQ(IREC)%CNAME
   ENDIF
   !## write isc2
   JREC=ISQ(IREC)%IREF-1
   DO J=1,ISQ(IREC)%N
    JREC=JREC+1
    IF(ISGDOUBLE.EQ.4)THEN
     X_SP(1)=REAL(DATISQ(JREC)%Q,4)
     X_SP(2)=REAL(DATISQ(JREC)%W,4)
     X_SP(3)=REAL(DATISQ(JREC)%D,4)
     X_SP(4)=REAL(DATISQ(JREC)%F,4)
     WRITE(ISGIU(10,1),REC=JREC+ICF) (X_SP(K),K=1,4)
    ELSE
     WRITE(ISGIU(10,1),REC=JREC+ICF) DATISQ(JREC)%Q,DATISQ(JREC)%W,DATISQ(JREC)%D,DATISQ(JREC)%F
    ENDIF
   ENDDO
  ENDDO

 ENDDO

 DO I=1,MAXFILES; CLOSE(ISGIU(I,1)); END DO

 END SUBROUTINE ISGSAVEIT

 !###===============================================================================
 SUBROUTINE ISGSAVEHEADERS()
 !###===============================================================================
 IMPLICIT NONE
 INTEGER :: IREC

 IF(ISGDOUBLE.EQ.4)WRITE(ISGIU(2,1),REC=1) UTL_PUTRECORDLENGTH(RECLEN(2))  
 IF(ISGDOUBLE.EQ.8)WRITE(ISGIU(2,1),REC=1) UTL_PUTRECORDLENGTH(RECLND(2))  

 IF(ISGDOUBLE.EQ.4)WRITE(ISGIU(3,1),REC=1) UTL_PUTRECORDLENGTH(RECLEN(3))  
 IF(ISGDOUBLE.EQ.8)WRITE(ISGIU(3,1),REC=1) UTL_PUTRECORDLENGTH(RECLND(3))  

 !## depends on type of ISG what size of database is applied
 IF(ISGDOUBLE.EQ.4)THEN
  IF(ISFR.EQ.0)IREC=RECLEN(4) 
  IF(ISFR.EQ.1)IREC=9*4 + 5*4 + 8
 ELSEIF(ISGDOUBLE.EQ.8)THEN
  IF(ISFR.EQ.0)IREC=RECLND(4) 
  IF(ISFR.EQ.1)IREC=9*8 + 5*4 + 8
 ENDIF

 WRITE(ISGIU(4,1),REC=1)  UTL_PUTRECORDLENGTH(IREC)       

 IF(ISGDOUBLE.EQ.4)WRITE(ISGIU(5,1),REC=1)  UTL_PUTRECORDLENGTH(RECLEN(5))  
 IF(ISGDOUBLE.EQ.8)WRITE(ISGIU(5,1),REC=1)  UTL_PUTRECORDLENGTH(RECLND(5))  

 IF(ISGDOUBLE.EQ.4)WRITE(ISGIU(6,1),REC=1)  UTL_PUTRECORDLENGTH(RECLEN(6))  
 IF(ISGDOUBLE.EQ.8)WRITE(ISGIU(6,1),REC=1)  UTL_PUTRECORDLENGTH(RECLND(6))  

 IF(ISGDOUBLE.EQ.4)WRITE(ISGIU(7,1),REC=1)  UTL_PUTRECORDLENGTH(RECLEN(7))  
 IF(ISGDOUBLE.EQ.8)WRITE(ISGIU(7,1),REC=1)  UTL_PUTRECORDLENGTH(RECLND(7))  

 IF(ISGDOUBLE.EQ.4)WRITE(ISGIU(8,1),REC=1)  UTL_PUTRECORDLENGTH(RECLEN(8))  
 IF(ISGDOUBLE.EQ.8)WRITE(ISGIU(8,1),REC=1)  UTL_PUTRECORDLENGTH(RECLND(8))  

 IF(ISGDOUBLE.EQ.4)WRITE(ISGIU(9,1),REC=1)  UTL_PUTRECORDLENGTH(RECLEN(9))  
 IF(ISGDOUBLE.EQ.8)WRITE(ISGIU(9,1),REC=1)  UTL_PUTRECORDLENGTH(RECLND(9))  

 IF(ISGDOUBLE.EQ.4)WRITE(ISGIU(10,1),REC=1) UTL_PUTRECORDLENGTH(RECLEN(10)) 
 IF(ISGDOUBLE.EQ.8)WRITE(ISGIU(10,1),REC=1) UTL_PUTRECORDLENGTH(RECLND(10)) 

 END SUBROUTINE ISGSAVEHEADERS

 !###===============================================================================
 SUBROUTINE ISGMEMORYISG(DN)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN
 INTEGER :: I
 
 NISG=NISG+DN

 IF(NISG.GT.DIMISG)THEN

  IF(ALLOCATED(DUMISG))DEALLOCATE(DUMISG)
  ALLOCATE(DUMISG(DIMISG))
  DUMISG=ISG

  IF(ALLOCATED(ISG))DEALLOCATE(ISG)
  ALLOCATE(ISG(NISG))
  DO I=1,DIMISG
   ISG(I)=DUMISG(I)
  ENDDO
  IF(ALLOCATED(DUMISG))DEALLOCATE(DUMISG)

  DIMISG=NISG

 ENDIF

 END SUBROUTINE ISGMEMORYISG

 !###===============================================================================
 SUBROUTINE ISGMEMORYISD(DN,K,ICLC)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: ICLC
 INTEGER :: I,N,IREF

 NISD=NISD+DN

 IF(NISD.GT.DIMISD)THEN

  IF(ALLOCATED(DUMISD))DEALLOCATE(DUMISD)
  ALLOCATE(DUMISD(DIMISD))
  DUMISD=ISD

  IF(ALLOCATED(ISD))DEALLOCATE(ISD)
  ALLOCATE(ISD(NISD))
  DO I=1,DIMISD
   ISD(I)=DUMISD(I)
  ENDDO
  IF(ALLOCATED(DUMISD))DEALLOCATE(DUMISD)

  DIMISD=NISD

 ENDIF

 !## copy data to create appropriate space
 IREF=MAX(1,ISG(K)%ICLC)

 !## copy data to create appropriate space
 IF(IREF.GT.0)THEN
  !## current space
  N=ABS(ISG(K)%NCLC)
  !## add space
  IF(DN.GT.0)THEN
   DO I=NISD,IREF+DN,-1
    IF(I-DN.GT.0)ISD(I)=ISD(I-DN)
   ENDDO
  !## remove space
  ELSEIF(DN.LT.0)THEN
   DO I=IREF+N+DN,NISD
    IF(I-DN.GT.0)ISD(I)=ISD(I-DN)
   ENDDO
  ENDIF
 ENDIF

! IF(DN.GT.0)THEN
!  IF(IREF+DN.LE.NISD)ISD(IREF+DN:NISD)=ISD(IREF:NISD-DN)
! ELSEIF(DN.LT.0)THEN
!  N=ISG(K)%NCLC
!  ISD(IREF+N+DN:NISD+DN)=ISD(IREF+N:NISD)
! ENDIF

 ISG(K)%NCLC=ISG(K)%NCLC+DN

 !## change all citations greater than isd(iseg)%iref
 ICLC=ISG(K)%ICLC
 DO I=1,NISG
  IF(ISG(I)%ICLC.GT.ICLC)ISG(I)%ICLC=ISG(I)%ICLC+DN
 END DO

 ISG(K)%ICLC=MAX(1,ISG(K)%ICLC)
 ICLC       =ISG(K)%ICLC

 END SUBROUTINE ISGMEMORYISD

 !###===============================================================================
 SUBROUTINE ISGMEMORYISC(DN,K,ICRS)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: ICRS
 INTEGER :: I,N,IREF

 NISC=NISC+DN

 IF(NISC.GT.DIMISC)THEN

  IF(ALLOCATED(DUMISC))DEALLOCATE(DUMISC)
  ALLOCATE(DUMISC(DIMISC))
  DUMISC=ISC

  IF(ALLOCATED(ISC))DEALLOCATE(ISC)
  ALLOCATE(ISC(NISC))
  DO I=1,DIMISC
   ISC(I)=DUMISC(I)
  ENDDO
  IF(ALLOCATED(DUMISC))DEALLOCATE(DUMISC)

  DIMISC=NISC

 ENDIF  

 !## copy data to create appropriate space
 IREF=MAX(1,ISG(K)%ICRS)

 !## copy data to create appropriate space
 IF(IREF.GT.0)THEN
  !## current space
  N=ABS(ISG(K)%NCRS)
  !## add space
  IF(DN.GT.0)THEN
   DO I=NISC,IREF+DN,-1
    IF(I-DN.GT.0)ISC(I)=ISC(I-DN)
   ENDDO
  !## remove space
  ELSEIF(DN.LT.0)THEN
   DO I=IREF+N+DN,NISC
    IF(I-DN.GT.0)ISC(I)=ISC(I-DN)
   ENDDO
  ENDIF
 ENDIF

! IF(DN.GT.0)THEN
!  IF(ICRS+DN.LE.NISC)ISC(ICRS+DN:NISC)=ISC(ICRS:NISC-DN)
! ELSEIF(DN.LT.0)THEN
!  N=ISG(K)%NCRS
!  ISC(ICRS+N+DN:NISC+DN)=ISC(ICRS+N:NISC)
! ENDIF
 
 ISG(K)%NCRS=ISG(K)%NCRS+DN

 !## change all citations greater than iref
 ICRS=ISG(K)%ICRS
 DO I=1,NISG
  IF(ISG(I)%ICRS.GT.ICRS)ISG(I)%ICRS=ISG(I)%ICRS+DN
!  IF(ISG(I)%ICRS.GT.IREF)ISG(I)%ICRS=ISG(I)%ICRS+DN
 END DO

 ISG(K)%ICRS=MAX(1,ISG(K)%ICRS)
 ICRS       =ISG(K)%ICRS

 END SUBROUTINE ISGMEMORYISC

 !###===============================================================================
 SUBROUTINE ISGMEMORYIST(DN,K,ISTW)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: ISTW
 INTEGER :: I,N,IREF

 NIST=NIST+DN

 IF(NIST.GT.DIMIST)THEN

  IF(ALLOCATED(DUMIST))DEALLOCATE(DUMIST)
  ALLOCATE(DUMIST(DIMIST))
  DUMIST=IST

  IF(ALLOCATED(IST))DEALLOCATE(IST)
  ALLOCATE(IST(NIST))
  DO I=1,DIMIST
   IST(I)=DUMIST(I)
  ENDDO
  IF(ALLOCATED(DUMIST))DEALLOCATE(DUMIST)

  DIMIST=NIST

 ENDIF

 !## copy data to create appropriate space
 IREF=MAX(1,ISG(K)%ISTW)

 !## copy data to create appropriate space
 IF(IREF.GT.0)THEN
  !## current space
  N=ABS(ISG(K)%NSTW)
  !## add space
  IF(DN.GT.0)THEN
   DO I=NIST,IREF+DN,-1
    IF(I-DN.GT.0)IST(I)=IST(I-DN)
   ENDDO
  !## remove space
  ELSEIF(DN.LT.0)THEN
   DO I=IREF+N+DN,NIST
    IF(I-DN.GT.0)IST(I)=IST(I-DN)
   ENDDO
  ENDIF
 ENDIF

! IF(DN.GT.0)THEN
!  IF(ISTW+DN.LE.NIST)IST(ISTW+DN:NIST)=IST(ISTW:NIST-DN)
! ELSEIF(DN.LT.0)THEN
!  N=ISG(K)%NSTW
!  IST(ISTW+N+DN:NIST+DN)=IST(ISTW+N:NIST)
! ENDIF

 ISG(K)%NSTW=ISG(K)%NSTW+DN

 !## change all citations greater than isd(iseg)%iref
 ISTW=ISG(K)%ISTW
 DO I=1,NISG
  IF(ISG(I)%ISTW.GT.ISTW)ISG(I)%ISTW=ISG(I)%ISTW+DN
!  IF(ISG(I)%ISTW.GT.IREF)ISG(I)%ISTW=ISG(I)%ISTW+DN
 END DO

 ISG(K)%ISTW=MAX(1,ISG(K)%ISTW)
 ISTW       =ISG(K)%ISTW

 END SUBROUTINE ISGMEMORYIST

 !###===============================================================================
 SUBROUTINE ISGMEMORYISQ(DN,K,IQHR)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: IQHR
 INTEGER :: I,N,IREF

 NISQ=NISQ+DN

 IF(NISQ.GT.DIMISQ)THEN

  IF(ALLOCATED(DUMISQ))DEALLOCATE(DUMISQ)
  ALLOCATE(DUMISQ(DIMISQ))
  DUMISQ=ISQ

  IF(ALLOCATED(ISQ))DEALLOCATE(ISQ)
  ALLOCATE(ISQ(NISQ))
  DO I=1,DIMISQ
   ISQ(I)=DUMISQ(I)
  ENDDO
  IF(ALLOCATED(DUMISQ))DEALLOCATE(DUMISQ)

  DIMISQ=NISQ

 ENDIF

 !## copy data to create appropriate space
 IREF=MAX(1,ISG(K)%IQHR)

 !## copy data to create appropriate space
 IF(IREF.GT.0)THEN
  !## current space
  N=ABS(ISG(K)%NQHR)
  !## add space
  IF(DN.GT.0)THEN
   DO I=NISQ,IREF+DN,-1
    IF(I-DN.GT.0)ISQ(I)=ISQ(I-DN)
   ENDDO
  !## remove space
  ELSEIF(DN.LT.0)THEN
   DO I=IREF+N+DN,NISQ
    IF(I-DN.GT.0)ISQ(I)=ISQ(I-DN)
   ENDDO
  ENDIF
 ENDIF
 
! IF(DN.GT.0)THEN
!  IF(IQHR+DN.LT.NISQ)ISQ(IQHR+DN:NISQ)=ISQ(IQHR:NISQ-DN)
! ELSEIF(DN.LT.0)THEN
!  N=ISG(K)%NQHR
!  ISQ(IQHR+N+DN:NISQ+DN)=ISQ(IQHR+N:NISQ)
! ENDIF

 ISG(K)%NQHR=ISG(K)%NQHR+DN

 !## change all citations greater than isd(iseg)%iref
 IQHR=ISG(K)%IQHR
 DO I=1,NISG
  IF(ISG(I)%IQHR.GT.IQHR)ISG(I)%IQHR=ISG(I)%IQHR+DN
!  IF(ISG(I)%IQHR.GT.IREF)ISG(I)%IQHR=ISG(I)%IQHR+DN
 END DO

 ISG(K)%IQHR=MAX(1,ISG(K)%IQHR)
 IQHR       =ISG(K)%IQHR

 END SUBROUTINE ISGMEMORYISQ

 !###===============================================================================
 SUBROUTINE ISGMEMORYISP(DN,K,ISEG)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: ISEG
 INTEGER,INTENT(IN) :: DN,K
 INTEGER :: I,N,IREF

 NISP=NISP+DN

 IF(NISP.GT.DIMISP)THEN

  IF(ALLOCATED(DUMISP))DEALLOCATE(DUMISP)
  ALLOCATE(DUMISP(DIMISP))
  DUMISP=ISP

  IF(ALLOCATED(ISP))DEALLOCATE(ISP)
  ALLOCATE(ISP(NISP))
  DO I=1,DIMISP
   ISP(I)=DUMISP(I)
  ENDDO
  IF(ALLOCATED(DUMISP))DEALLOCATE(DUMISP)

  DIMISP=NISP

 ENDIF

 !## copy data to create appropriate space
 IREF=MAX(1,ISG(K)%ISEG)

 !## copy data to create appropriate space
 IF(IREF.GT.0)THEN
  !## current space
  N=ABS(ISG(K)%NSEG)
  !## add space
  IF(DN.GT.0)THEN
   DO I=NISP,IREF+DN,-1
    IF(I-DN.GT.0)ISP(I)=ISP(I-DN)
   ENDDO
  !## remove space
  ELSEIF(DN.LT.0)THEN
   DO I=IREF+N+DN,NISP
    IF(I-DN.GT.0)ISP(I)=ISP(I-DN)
   ENDDO
  ENDIF
 ENDIF

! IF(DN.GT.0)THEN
!  IF(ISEG+DN.LE.NISP)ISP(ISEG+DN:NISP)=ISP(ISEG:NISP-DN)
! ELSEIF(DN.LT.0)THEN
!  N=ISG(K)%NSEG
!  ISP(ISEG+N+DN:NISP+DN)=ISP(ISEG+N:NISP)
! ENDIF

 ISG(K)%NSEG=ISG(K)%NSEG+DN

 !## change all citations greater than isg(k)%iseg
 ISEG=ISG(K)%ISEG
 DO I=1,NISG
  IF(ISG(I)%ISEG.GT.ISEG)ISG(I)%ISEG=ISG(I)%ISEG+DN
!  IF(ISG(I)%ISEG.GT.IREF)ISG(I)%ISEG=ISG(I)%ISEG+DN
 END DO

 ISG(K)%ISEG=MAX(1,ISG(K)%ISEG)
 ISEG       =ISG(K)%ISEG

 END SUBROUTINE ISGMEMORYISP

 !###===============================================================================
 SUBROUTINE ISGMEMORYDATISD(DN,K,ICLC)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: ICLC
 INTEGER :: I,N,IREF

 NDISD=NDISD+DN

 IF(NDISD.GT.DIMDATISD)THEN

  IF(ALLOCATED(DUMDATISD))DEALLOCATE(DUMDATISD)
  ALLOCATE(DUMDATISD(DIMDATISD))
  DUMDATISD=DATISD

  IF(ALLOCATED(DATISD))DEALLOCATE(DATISD)
  ALLOCATE(DATISD(NDISD))
  DO I=1,DIMDATISD
   DATISD(I)=DUMDATISD(I)
  ENDDO
  IF(ALLOCATED(DUMDATISD))DEALLOCATE(DUMDATISD)

  DIMDATISD=NDISD

 ENDIF

 !## copy data to create appropriate space
 IREF =MAX(1,ISD(K)%IREF)

 !## copy data to create appropriate space
 IF(IREF.GT.0)THEN
  !## current space
  N=ABS(ISD(K)%N)
  !## add space
  IF(DN.GT.0)THEN
   DO I=NDISD,IREF+DN,-1
    IF(I-DN.GT.0)DATISD(I)=DATISD(I-DN)
   ENDDO
  !## remove space
  ELSEIF(DN.LT.0)THEN
   DO I=IREF+N+DN,NDISD
    IF(I-DN.GT.0)DATISD(I)=DATISD(I-DN)
   ENDDO
  ENDIF
 ENDIF
 
! IF(ICLC.GT.0)THEN
!  IF(DN.GT.0)THEN
!   IF(ICLC+DN.LE.NDISD)DATISD(ICLC+DN:NDISD)=DATISD(ICLC:NDISD-DN)
!  ELSEIF(DN.LT.0)THEN
!   N=ISD(K)%N
!   DATISD(ICLC+N+DN:NDISD+DN)=DATISD(ICLC+N:NDISD)
!  ENDIF
! ENDIF

 ISD(K)%N=ISD(K)%N+DN

 !## change all citations greater than isd(k)%iref
 IREF=ISD(K)%IREF
 DO I=1,NISD
  IF(ISD(I)%IREF.GT.IREF)ISD(I)%IREF=ISD(I)%IREF+DN
 END DO

 ISD(K)%IREF=MAX(1,ISD(K)%IREF)
 ICLC       =ISD(K)%IREF

 END SUBROUTINE ISGMEMORYDATISD

 !###===============================================================================
 SUBROUTINE ISGMEMORYDATISC(DN,K,ICRS)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: ICRS
 INTEGER :: I,N,J,IREF

 NDISC=NDISC+DN

 IF(NDISC.GT.DIMDATISC)THEN

  IF(ALLOCATED(DUMDATISC))DEALLOCATE(DUMDATISC)
  ALLOCATE(DUMDATISC(DIMDATISC))
  DUMDATISC=DATISC

  IF(ALLOCATED(DATISC))DEALLOCATE(DATISC)
  ALLOCATE(DATISC(NDISC))
  DO I=1,DIMDATISC
   DATISC(I)=DUMDATISC(I)
  ENDDO
  IF(ALLOCATED(DUMDATISC))DEALLOCATE(DUMDATISC)

  DIMDATISC=NDISC

 ENDIF

 !## copy data to create appropriate space
 IREF =MAX(1,ISC(K)%IREF)

 !## copy data to create appropriate space
 IF(IREF.GT.0)THEN
  !## current space
  N=ABS(ISC(K)%N)
  !## add space
  IF(DN.GT.0)THEN
   DO I=NDISC,IREF+DN,-1
    IF(I-DN.GT.0)DATISC(I)=DATISC(I-DN)
   ENDDO
  !## remove space
  ELSEIF(DN.LT.0)THEN
   DO I=IREF+N+DN,NDISC
    IF(I-DN.GT.0)DATISC(I)=DATISC(I-DN)
   ENDDO
  ENDIF
 ENDIF

! IF(ICLC.GT.0)THEN
!  IF(DN.GT.0)THEN
!   IF(ICLC+DN.LE.NDISD)DATISD(ICLC+DN:NDISD)=DATISD(ICLC:NDISD-DN)
!  ELSEIF(DN.LT.0)THEN
!   N=ISD(K)%N
!   DATISD(ICLC+N+DN:NDISD+DN)=DATISD(ICLC+N:NDISD)
!  ENDIF
! ENDIF

 J=1; IF(ISC(K)%N.LT.0)J=-1
 ISC(K)%N=J*(ABS(ISC(K)%N)+DN)
 
 !## change all citations greater than isc(k)%iref
 IREF =ISC(K)%IREF
 DO I=1,NISC
  IF(ISC(I)%IREF.GT.IREF)ISC(I)%IREF=ISC(I)%IREF+DN
 END DO

 ISC(K)%IREF=MAX(1,ISC(K)%IREF)
 ICRS       =ISC(K)%IREF

 END SUBROUTINE ISGMEMORYDATISC

 !###===============================================================================
 SUBROUTINE ISGMEMORYDATIST(DN,K,ISTW)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: ISTW
 INTEGER :: I,N,IREF

 NDIST=NDIST+DN

 IF(NDIST.GT.DIMDATIST)THEN

  IF(ALLOCATED(DUMDATIST))DEALLOCATE(DUMDATIST)
  ALLOCATE(DUMDATIST(DIMDATIST))
  DUMDATIST=DATIST

  IF(ALLOCATED(DATIST))DEALLOCATE(DATIST)
  ALLOCATE(DATIST(NDIST))
  DO I=1,DIMDATIST
   DATIST(I)=DUMDATIST(I)
  ENDDO
   DATIST(1:DIMDATIST)=DUMDATIST(1:DIMDATIST)
  IF(ALLOCATED(DUMDATIST))DEALLOCATE(DUMDATIST)

  DIMDATIST=NDIST

 ENDIF

 IREF =MAX(1,IST(K)%IREF)

 !## copy data to create appropriate space
 IF(IREF.GT.0)THEN
  !## current space
  N=ABS(IST(K)%N)
  !## add space
  IF(DN.GT.0)THEN
   DO I=NDIST,IREF+DN,-1
    IF(I-DN.GT.0)DATIST(I)=DATIST(I-DN)
   ENDDO
  !## remove space
  ELSEIF(DN.LT.0)THEN
   DO I=IREF+N+DN,NDIST
    IF(I-DN.GT.0)DATIST(I)=DATIST(I-DN)
   ENDDO
  ENDIF
 ENDIF

! !## copy data to create appropriate space
! IF(ISTW.GT.0)THEN
!  IF(DN.GT.0)THEN
!   IF(ISTW+DN.LE.NDIST)DATIST(ISTW+DN:NDIST)=DATIST(ISTW:NDIST-DN)
!  ELSEIF(DN.LT.0)THEN
!   N=IST(K)%N
!   DATIST(ISTW+N+DN:NDIST+DN)=DATIST(ISTW+N:NDIST)
!  ENDIF
! ENDIF

 IST(K)%N=IST(K)%N+DN

 !## change all citations greater than ist(k)%iref
 IREF =IST(K)%IREF
 DO I=1,NIST
  IF(IST(I)%IREF.GT.IREF)IST(I)%IREF=IST(I)%IREF+DN
 END DO

 IST(K)%IREF=MAX(1,IST(K)%IREF)
 ISTW       =IST(K)%IREF

 END SUBROUTINE ISGMEMORYDATIST

 !###===============================================================================
 SUBROUTINE ISGMEMORYDATISQ(DN,K,IQHR)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: IQHR
 INTEGER :: I,N,IREF

 NDISQ=NDISQ+DN

 IF(NDISQ.GT.DIMDATISQ)THEN

  IF(ALLOCATED(DUMDATISQ))DEALLOCATE(DUMDATISQ)
  ALLOCATE(DUMDATISQ(DIMDATISQ))
  DUMDATISQ=DATISQ

  IF(ALLOCATED(DATISQ))DEALLOCATE(DATISQ)
  ALLOCATE(DATISQ(NDISQ))
  DO I=1,DIMDATISQ
   DATISQ(I)=DUMDATISQ(I)
  ENDDO
  IF(ALLOCATED(DUMDATISQ))DEALLOCATE(DUMDATISQ)

  DIMDATISQ=NDISQ

 ENDIF

 IREF =MAX(1,ISQ(K)%IREF)

 !## copy data to create appropriate space
 IF(IREF.GT.0)THEN
  !## current space
  N=ABS(ISQ(K)%N)
  !## add space
  IF(DN.GT.0)THEN
   DO I=NDISQ,IREF+DN,-1
    IF(I-DN.GT.0)DATISQ(I)=DATISQ(I-DN)
   ENDDO
  !## remove space
  ELSEIF(DN.LT.0)THEN
   DO I=IREF+N+DN,NDISQ
    IF(I-DN.GT.0)DATISQ(I)=DATISQ(I-DN)
   ENDDO
  ENDIF
 ENDIF

! !## copy data to create appropriate space
! IF(IQHR.GT.0)THEN
!  IF(DN.GT.0)THEN
!   IF(IQHR+DN.LE.NDISQ)DATISQ(IQHR+DN:NDISQ)=DATISQ(IQHR:NDISQ-DN)
!  ELSEIF(DN.LT.0)THEN
!   N=ISQ(K)%N
!   DATISQ(IQHR+N+DN:NDISQ+DN)=DATISQ(IQHR+N:NDISQ)
!  ENDIF
! ENDIF

 ISQ(K)%N=ISQ(K)%N+DN

 !## change all citations greater than ist(k)%iref
 IREF =ISQ(K)%IREF
 DO I=1,NISQ
  IF(ISQ(I)%IREF.GT.IREF)ISQ(I)%IREF=ISQ(I)%IREF+DN
 END DO

 ISQ(K)%IREF=MAX(1,ISQ(K)%IREF)
 IQHR       =ISQ(K)%IREF

 END SUBROUTINE ISGMEMORYDATISQ

 !###===============================================================================
 SUBROUTINE ISGDEAL(IOPTION)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPTION

 IF(ALLOCATED(NISGF)) DEALLOCATE(NISGF);  IF(ALLOCATED(OFFSD)) DEALLOCATE(OFFSD)
 IF(ALLOCATED(OFFSC)) DEALLOCATE(OFFSC);  IF(ALLOCATED(OFFST)) DEALLOCATE(OFFST)
 IF(ALLOCATED(OFFSQ)) DEALLOCATE(OFFSQ);  IF(ALLOCATED(NISFR)) DEALLOCATE(NISFR)
 IF(ALLOCATED(OFFISG))DEALLOCATE(OFFISG); IF(ALLOCATED(OFFISD))DEALLOCATE(OFFISD)
 IF(ALLOCATED(OFFISC))DEALLOCATE(OFFISC); IF(ALLOCATED(OFFIST))DEALLOCATE(OFFIST)
 IF(ALLOCATED(OFFISQ))DEALLOCATE(OFFISQ); IF(ALLOCATED(ISGLEG))DEALLOCATE(ISGLEG)
 
 !## only remove temporary arrays
 IF(IOPTION.EQ.0)RETURN

 IF(ALLOCATED(ISDLABELS))DEALLOCATE(ISDLABELS)
 
 IF(ALLOCATED(TISC))DEALLOCATE(TISC)
 IF(ALLOCATED(ISCN))DEALLOCATE(ISCN)
 IF(ALLOCATED(TISD))DEALLOCATE(TISD)
 IF(ALLOCATED(TIST))DEALLOCATE(TIST)
 IF(ALLOCATED(TISQ))DEALLOCATE(TISQ)

 IF(ALLOCATED(ISG)) DEALLOCATE(ISG)
 IF(ALLOCATED(ISP)) DEALLOCATE(ISP)
 IF(ALLOCATED(ISP2))DEALLOCATE(ISP2)
 IF(ALLOCATED(ISC)) DEALLOCATE(ISC)
 IF(ALLOCATED(ISD)) DEALLOCATE(ISD)
 IF(ALLOCATED(IST)) DEALLOCATE(IST)
 IF(ALLOCATED(ISQ)) DEALLOCATE(ISQ)

 IF(ALLOCATED(DUMISG))DEALLOCATE(DUMISG)
 IF(ALLOCATED(DUMISP))DEALLOCATE(DUMISP)
 IF(ALLOCATED(DUMISC))DEALLOCATE(DUMISC)
 IF(ALLOCATED(DUMISD))DEALLOCATE(DUMISD)
 IF(ALLOCATED(DUMIST))DEALLOCATE(DUMIST)
 IF(ALLOCATED(DUMISQ))DEALLOCATE(DUMISQ)

 IF(ALLOCATED(DATISD))DEALLOCATE(DATISD)
 IF(ALLOCATED(DATISC))DEALLOCATE(DATISC)
 IF(ALLOCATED(DATIST))DEALLOCATE(DATIST)
 IF(ALLOCATED(DATISQ))DEALLOCATE(DATISQ)

 IF(ALLOCATED(DATISD2))DEALLOCATE(DATISD2)
 IF(ALLOCATED(DATISC2))DEALLOCATE(DATISC2)
 IF(ALLOCATED(DATIST2))DEALLOCATE(DATIST2)
 IF(ALLOCATED(DATISQ2))DEALLOCATE(DATISQ2)

 IF(ALLOCATED(DUMDATISD))DEALLOCATE(DUMDATISD)
 IF(ALLOCATED(DUMDATISC))DEALLOCATE(DUMDATISC)
 IF(ALLOCATED(DUMDATIST))DEALLOCATE(DUMDATIST)
 IF(ALLOCATED(DUMDATISQ))DEALLOCATE(DUMDATISQ)

 IF(ALLOCATED(ISGADJ))DEALLOCATE(ISGADJ)

 IF(ALLOCATED(ISGEDITISD))DEALLOCATE(ISGEDITISD)
 IF(ALLOCATED(ISGEDITISC))DEALLOCATE(ISGEDITISC)
 IF(ALLOCATED(ISGEDITIST))DEALLOCATE(ISGEDITIST)
 IF(ALLOCATED(ISGEDITISQ))DEALLOCATE(ISGEDITISQ)

 IF(ALLOCATED(ISEGMENTS))DEALLOCATE(ISEGMENTS)
 IF(ALLOCATED(IDATES))DEALLOCATE(IDATES)
 IF(ALLOCATED(CDATES))DEALLOCATE(CDATES)
 IF(ALLOCATED(XPOL))DEALLOCATE(XPOL)
 IF(ALLOCATED(YPOL))DEALLOCATE(YPOL)

 END SUBROUTINE ISGDEAL

END MODULE MOD_ISG_UTL