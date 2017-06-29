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

!####============================================================
MODULE MOD_GLBVAR
REAL,SAVE :: DELT,SIMCSIZE
INTEGER,SAVE :: ISS       !## 1=STEADYSTATE 2=TRANSIENT
CHARACTER(LEN=20),SAVE :: CDATE
CHARACTER(LEN=256),SAVE :: LINE
REAL,DIMENSION(4),SAVE :: SIMBOX
LOGICAL,SAVE :: LQD
END MODULE MOD_GLBVAR

!####============================================================
MODULE MOD_ISGVAR
!####============================================================
 INTEGER,PARAMETER :: MAXITEMS=4 !excl. date
 INTEGER :: NISGH,ISEG,NSEG,ID,NXY
 CHARACTER(LEN=50) :: CISGH
 CHARACTER(LEN=16),DIMENSION(MAXITEMS) :: ATTRIB
 REAL,ALLOCATABLE,DIMENSION(:) :: X,Y
 REAL :: X1,Y1,X2,Y2,ISGX,ISGY
 TYPE DWPTYPE
  REAL :: DIST
  INTEGER :: NCROS
 END TYPE DWPTYPE
 TYPE CROSTYPE
  REAL :: DIST,KM,BOTTOM,ZP
 END TYPE CROSTYPE
 TYPE(DWPTYPE),DIMENSION(:),ALLOCATABLE :: DWP
 TYPE(CROSTYPE),DIMENSION(:,:),ALLOCATABLE :: CROS
END MODULE MOD_ISGVAR

!####============================================================
MODULE MOD_BASVAR
!####============================================================
 INTEGER,SAVE :: NCOL,NROW
 REAL,ALLOCATABLE,DIMENSION(:),SAVE :: DELR,DELC
END MODULE MOD_BASVAR

      subroutine isginit()
! description:
! ------------------------------------------------------------------------------
! init isg
!

! declaration section
! ------------------------------------------------------------------------------
      use mod_glbvar, only: iss,cdate,delt,simcsize,simbox,lqd
      use mod_basvar, only: delr,delc,nrow,ncol
      use global, only: iunit
      use m_mf2005_iu, only: iumet

      implicit none

! program section
! ------------------------------------------------------------------------------

! check if MET package is activated
      if (IUNIT(IUMET).le.0) then
         write(*,*) 'ISG cannot be used without met-package.'
         call ustop(' ')
      end if

      call isginit1(nrow,ncol)
      call isginit2(iss,cdate,delt,simcsize,simbox,delr,delc,nrow,ncol,lqd)

! end of program
      return
      end subroutine

      subroutine isginit1(im_nrow, im_ncol)
      use global, only: ncol, nrow
      use mod_basvar, only: delr, delc
      use pksmpi_mod,only: nrproc, gncol, gnrow     
                           
      implicit none
      integer, intent(out) :: im_nrow, im_ncol
      
      if(nrproc.gt.1) then
         im_ncol = gncol
         im_nrow = gnrow
      else
         im_ncol = ncol
         im_nrow = nrow
      end if   

      if(.not.allocated(delr)) allocate(delr(0:im_ncol))
      if(.not.allocated(delc)) allocate(delc(0:im_nrow))

! end of program
      return
      end subroutine

      subroutine isginit2(im_iss,im_cdate,im_delt,im_simcsize,im_simbox,im_delr,im_delc,im_nrow,im_ncol,lqd)

      use global, only: ncol, nrow, delr, delc, iunit, issflg, nper
      use gwfmetmodule, only: coord_xll, coord_yll, coord_xur, coord_yur,&
                              gcoord_xll, gcoord_yll, gcoord_xur, gcoord_yur,&
                              time_cstring, iss, ieq, cdelr, cdelc, ieq
      use m_mf2005_main, only: timesteptime, kper
      use gwfbasmodule, only: delt
      use pksmpi_mod,only: nrproc, gncol, gnrow 

      implicit none

! arguments
      real, intent(inout) :: im_delt, im_simcsize
      integer, intent(inout) :: im_iss
      character(len=20), intent(inout) :: im_cdate
      real, dimension(4), intent(inout) :: im_simbox
      real, dimension(0:im_ncol), intent(inout) :: im_delr
      real, dimension(0:im_nrow), intent(inout) :: im_delc
      integer, intent(inout) :: im_ncol, im_nrow
      logical, intent(inout) :: lqd

! local variables
      double precision :: t
      integer :: i, icol, irow, date, hour, minute, second

! program section
! ------------------------------------------------------------------------------

! find out or model is steady state or not
      im_iss = 1
      if (.not.associated(iss)) then
         write(*,*) 'Error, initialization ISG'
         call ustop(' ')
      else
         im_iss = iss
      end if
      if (im_iss.eq.0) im_iss = 2

      im_delt = delt

! check if grid is uniform
      lqd = .true.
      if (.not.associated(ieq)) then
         write(*,*) 'Error, initialization ISG'
         call ustop(' ')
      else
         if (ieq.eq.1) lqd = .false.
      end if

      if (lqd) then
         im_simcsize = delr(1)
      else
         write(*,*) 'Error: non-uniform grids not yet supported.'
         call ustop(' ')
      end if

      if (.not.associated(cdelr).or..not.associated(cdelc)) then
         write(*,*) 'Error, initialization ISG'
         call ustop(' ')
      else
         if (nrproc.eq.1) then 
            im_delr(0:im_ncol) = cdelr(0:ncol)
            im_delc(0:im_nrow) = cdelc(0:nrow)
         else    
            im_delr(0) = gcoord_xll
            do icol = 1, im_ncol
               im_delr(icol) = gcoord_xll + real(icol)*im_simcsize
            end do
            im_delc(0) = gcoord_yur
            do irow = 1, im_nrow
               im_delc(irow) = gcoord_yur - real(irow)*im_simcsize
            end do
         end if      
      end if

      if (nrproc.eq.1) then 
         im_simbox(1) = coord_xll
         im_simbox(2) = coord_yll
         im_simbox(3) = coord_xur
         im_simbox(4) = coord_yur
      else
         im_simbox(1) = gcoord_xll
         im_simbox(2) = gcoord_yll
         im_simbox(3) = gcoord_xur
         im_simbox(4) = gcoord_yur
      end if          
      
      t = timesteptime
      call cfn_mjd2datehms(t,date,hour,minute,second)
      write(im_cdate,'(i8)') date
      im_cdate = adjustl(im_cdate)

! end of program
      return
      end subroutine

      subroutine isgfinalize()
      use mod_basvar, only: delr, delc
      implicit none

      if(allocated(delr)) deallocate(delr)
      if(allocated(delc)) deallocate(delc)

! end of program
      return
      end subroutine

!###====================================================================
SUBROUTINE PCK1RPISG(ISGLIST,mxisg,NISG,FNAME,ILAY,iact)
!###====================================================================
USE IMOD_UTL, ONLY : IMOD_UTL_IDATETOJDATE,IMOD_UTL_FILENAME,IMOD_UTL_PRINTTEXT,IMOD_UTL_ITOS, &
    IMOD_UTL_OPENIDF,IMOD_UTL_OPENASC,IMOD_UTL_INTERSECT_EQUI,IMOD_UTL_INTERSECT_NONEQUI,IMOD_UTL_GETRCL
USE MOD_GLBVAR, ONLY : LINE,ISS,CDATE,DELT,SIMCSIZE,SIMBOX,LQD
USE IMOD_UTL, ONLY : ICF
USE MOD_BASVAR, ONLY : DELR,DELC,NROW,NCOL
USE MOD_ISGVAR
USE IMOD_IDF_PAR
USE IMOD_IDF, ONLY : IDFNULLIFY,IDFALLOCATEX,IDFIROWICOL,IDFDEALLOCATEX,IDFDEALLOCATE
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: FNAME
CHARACTER(LEN=32) :: CNAME
INTEGER,INTENT(IN) :: ILAY
integer, intent(in) :: iact
integer, intent(in) :: mxisg
REAL, DIMENSION(mxisg,11), INTENT(OUT) :: ISGLIST
INTEGER, INTENT(OUT) :: NISG
INTEGER,DIMENSION(8) :: ISGIU
INTEGER :: I,II,III,J,JJ,K,SDATE,EDATE,TTIME,MX,N,IROW,ICOL,ICRS,NCRS,IREC,ICROS,N2DIM,IC,IR
INTEGER :: MAXCROS,JCRS,NCLC,IREF,JREC,ICLC,NR,ISTW,NSTW,NS1,NS2,NN,NODE,ICSTYPE
INTEGER(KIND=2) :: ZM
INTEGER(KIND=1) :: ZC,ZP
REAL :: DXY,C,WP,BH,RWIDTH,WETPER,ISGLEN,MIND,D,FCT,H1,H2,Z,WL,INFF,ZCHK
REAL,ALLOCATABLE,DIMENSION(:,:) :: QSORT,RVAL
REAL,ALLOCATABLE,DIMENSION(:) :: DIST
REAL,ALLOCATABLE,DIMENSION(:) :: XA,YA,FA,LN,XNR
INTEGER,ALLOCATABLE,DIMENSION(:) :: IPOS
REAL :: NODATA=-9999.0
LOGICAL :: LNODAT
REAL :: WATERDEPTH=0.10 !## minimal waterdepth
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
TYPE(IDFOBJ) :: ICROSS,PCROSS
LOGICAL :: ISGATTRIBUTES_2DCROSS_READ
INTEGER :: IOS

IF(ALLOCATED(QSORT))DEALLOCATE(QSORT)
IF(ALLOCATED(XA))DEALLOCATE(XA); IF(ALLOCATED(YA))DEALLOCATE(YA); IF(ALLOCATED(LN))DEALLOCATE(LN)
MX=1000; ALLOCATE(XA(MX),YA(MX),FA(MX),LN(MX))

!## translate cdate in to julian date - for transient simulations only!
IF(ISS.EQ.2)THEN
 IF(LEN_TRIM(CDATE).EQ.8)THEN
  READ(CDATE,*,IOSTAT=IOS) SDATE
 ELSE
  !## make sure sdate is only 8 integer (yyyymmdd)
  READ(CDATE,'(I8)',IOSTAT=IOS) SDATE
 ENDIF
 IF(IOS.EQ.0)THEN
  SDATE=IMOD_UTL_IDATETOJDATE(SDATE)
  EDATE=SDATE+MAX(1,INT(DELT))
  TTIME=EDATE-SDATE !+1
 ELSE
  CALL IMOD_UTL_PRINTTEXT('iMODFLOW can not convert entered date ('//TRIM(CDATE)//') into an integer value.',2)
 ENDIF
ELSEIF(ISS.EQ.1)THEN
 TTIME=1
ENDIF
ALLOCATE(QSORT(TTIME,MAXITEMS),XNR(MAXITEMS))

I=INDEX(FNAME,'.',.TRUE.)-1
!## open *.ISG file
LINE=FNAME(:I)//'.ISG'
CALL IMOD_UTL_FILENAME(LINE)
CALL IMOD_UTL_OPENASC(ISGIU(1),LINE,'R')
!## open *.ISP file
LINE=FNAME(:I)//'.ISP'
CALL IMOD_UTL_FILENAME(LINE)
CALL IMOD_UTL_OPENIDF(ISGIU(2),LINE,'R',8)
!## open *.ISD1 file
LINE=FNAME(:I)//'.ISD1'
CALL IMOD_UTL_FILENAME(LINE)
CALL IMOD_UTL_OPENIDF(ISGIU(3),LINE,'R',44)
!## open *.ISD2 file
LINE=FNAME(:I)//'.ISD2'
CALL IMOD_UTL_FILENAME(LINE)
CALL IMOD_UTL_OPENIDF(ISGIU(4),LINE,'R',20)
!## open *.ISC1 file
LINE=FNAME(:I)//'.ISC1'
CALL IMOD_UTL_FILENAME(LINE)
CALL IMOD_UTL_OPENIDF(ISGIU(5),LINE,'R',44)
!## open *.ISC2 file
LINE=FNAME(:I)//'.ISC2'
CALL IMOD_UTL_FILENAME(LINE)
CALL IMOD_UTL_OPENIDF(ISGIU(6),LINE,'R',12)
!## open *.IST1 file
LINE=FNAME(:I)//'.IST1'
CALL IMOD_UTL_FILENAME(LINE)
CALL IMOD_UTL_OPENIDF(ISGIU(7),LINE,'R',44)
!## open *.IST2 file
LINE=FNAME(:I)//'.IST2'
CALL IMOD_UTL_FILENAME(LINE)
CALL IMOD_UTL_OPENIDF(ISGIU(8),LINE,'R',12)

READ(ISGIU(1),*) NISGH

NXY=0
NISG=0

ALLOCATE(DWP(1),CROS(1,1))

!## number of two-dimensional cross-sections
N2DIM=0

DO I=1,NISGH
 READ(ISGIU(1),*) CISGH,ISEG,NSEG,ICLC,NCLC,ICRS,NCRS,ISTW,NSTW
 !## skip elements with le 1 segment
 IF(NSEG.LT.2)CYCLE; IF(NCLC.LT.2)CYCLE

 !## increase memory for segments (including points from calculation-nodes!)
 IF(NSEG+NCLC+2*NSTW.GT.NXY)THEN
  NXY=NSEG+NCLC+2*NSTW
  IF(ALLOCATED(X))   DEALLOCATE(X);    IF(ALLOCATED(Y))   DEALLOCATE(Y)
  IF(ALLOCATED(RVAL))DEALLOCATE(RVAL); IF(ALLOCATED(DIST))DEALLOCATE(DIST)
  IF(ALLOCATED(IPOS))DEALLOCATE(IPOS)
  ALLOCATE(X(NXY),Y(NXY),DIST(NXY),IPOS(NXY),RVAL(MAXITEMS,0:NXY))
 ENDIF

 !## read segments
 IREC=ISEG-1
 DO J=1,NSEG; READ(ISGIU(2),REC=IREC+J+ICF) X(J),Y(J); END DO

 !## evaluate whether line is within model areas, otherwise skip rest of procedure ...
 IF(MAXVAL(X(1:NSEG)).GT.SIMBOX(1).AND.MAXVAL(Y(1:NSEG)).GT.SIMBOX(2).AND. &
    MINVAL(X(1:NSEG)).LT.SIMBOX(3).AND.MINVAL(Y(1:NSEG)).LT.SIMBOX(4))THEN

  !## read cross-section information from *.isc
  IF(NCRS.GT.SIZE(DWP))THEN 
   IF(ALLOCATED(DWP))DEALLOCATE(DWP); ALLOCATE(DWP(NCRS))
  ENDIF

  !## read cross-sectional data
  MAXCROS=0
  IREC=ICRS-1
  DO J=1,NCRS
   READ(ISGIU(5),REC=IREC+J+ICF) DWP(J)%NCROS
   !## do not read in two-dimensional cross-sections
   MAXCROS=MAXCROS+MAX(0,DWP(J)%NCROS)
  END DO

  IF(MAXCROS.GT.SIZE(CROS,2).OR.NCRS.GT.SIZE(CROS,1))THEN !MAXCROS.GT.DIMCROS.OR.NCRS.GT.DIMDWP)THEN
   IF(MAXCROS.GT.0.AND.NCRS.GT.0)THEN
    IF(ALLOCATED(CROS))DEALLOCATE(CROS)
    ALLOCATE(CROS(NCRS,MAXCROS))
   ENDIF
  ENDIF

  IPOS=0
  !## include calculation nodes in between segments
  CALL PCK7RPISG(ICLC,ISGIU(3),NCLC,NXY,NSEG,X,Y,DIST,IPOS,+1)
  !## include structure nodes in between segments
  CALL PCK7RPISG(ISTW,ISGIU(7),NSTW,NXY,NSEG,X,Y,DIST,IPOS,-1)

  !## read/prepare all information for current date/segment where information is connected to
  J=0
  DO
   J=J+1
   IF(IPOS(J).GT.0)THEN
    READ(ISGIU(3),REC=IPOS(J)+ICF) NR,IREF
    CALL PCK2RPISG(SDATE,EDATE,TTIME,QSORT,XNR,ISGIU(4),MAXITEMS,RVAL(1,J),NR,IREF,I,1,NODATA)
   ENDIF
   IF(IPOS(J).LT.0)THEN
    READ(ISGIU(7),REC=ABS(IPOS(J))+ICF) NR,IREF
    CALL PCK2RPISG(SDATE,EDATE,TTIME,QSORT,XNR,ISGIU(8),MAXITEMS,RVAL(1,J),NR,IREF,I,-1,NODATA)
    J=J+1
    !## put waterlevel down on waterlevel
    RVAL(1,J)=RVAL(2,J-1)
   ENDIF
   IF(J.EQ.NSEG)EXIT
  END DO

  !## interpolate all segments in between and give proper values!
  CALL PCK5RPISG(X,Y,DIST,NXY,NSEG)

  !## determine flow-direction
  LNODAT=.TRUE.
  DO JJ=1,4; IF(RVAL(JJ,1)   .EQ.NODATA)THEN; LNODAT=.FALSE.; EXIT; ENDIF; ENDDO
  DO JJ=1,4; IF(RVAL(JJ,NSEG).EQ.NODATA)THEN; LNODAT=.FALSE.; EXIT; ENDIF; ENDDO
  IF(LNODAT)THEN
   H1=RVAL(1,1)
   H2=RVAL(1,NSEG)
   !## flow direction other-way-around
   J=0
   DO
    J=J+1
    IF(J.GT.NSEG)EXIT
    !## structure found
    IF(IPOS(J).LT.0)THEN
     Z          =RVAL(1,J)
     RVAL(1,J)  =RVAL(1,J+1)
     RVAL(1,J+1)=Z
     J          =J+1
    ENDIF
   ENDDO

   !## interpolate waterlevel,waterbottom,inf.factor,c-value - do not interupt it by structures!
   DO JJ=1,4
    DO J=1,NSEG-1
     IF(IPOS(J).GT.0.AND.RVAL(JJ,J).NE.NODATA)THEN
      DO II=J+1,NSEG
       IF(IPOS(II).GT.0.AND.RVAL(JJ,II).NE.NODATA)EXIT
      ENDDO
      D=MAX(0.0,DIST(II)-DIST(J))
      DO K=J+1,II-1
       FCT=0.0; IF(D.GT.0.0)FCT=(DIST(K)-DIST(J))/D
       RVAL(JJ,K)=RVAL(JJ,J)+((RVAL(JJ,II)-RVAL(JJ,J))*FCT)
      END DO
     ENDIF
    END DO
   ENDDO

   !## interpolate with structures
   IF(NSTW.GT.0)THEN
    DO J=1,NSEG-1
     IF(IPOS(J).NE.0.AND.RVAL(1,J).NE.NODATA)THEN
      D=0.0
      DO II=J+1,NSEG
       D=D+(DIST(II)-DIST(J))
       IF(IPOS(II).NE.0.AND.RVAL(1,II).NE.NODATA)EXIT
      ENDDO
      DO K=J+1,II-1
       FCT=0.0; IF(D.GT.0.0)FCT=(DIST(K)-DIST(J))/D
       RVAL(1,K)=RVAL(1,J)+((RVAL(1,II)-RVAL(1,J))*FCT)
      END DO
     ENDIF
    END DO
   ENDIF

   !## read cross-sectional data
   IREC=ICRS-1
   DO J=1,NCRS
    READ(ISGIU(5),REC=IREC+J+ICF) DWP(J)%NCROS,IREF,DWP(J)%DIST
    !## skip two-dimensional cross-sections
    IF(DWP(J)%NCROS.LE.0)THEN; N2DIM=N2DIM+1; CYCLE; ENDIF
    JREC=IREF-1
    DO ICROS=1,DWP(J)%NCROS
     JREC=JREC+1
     READ(ISGIU(6),REC=JREC+ICF) CROS(J,ICROS)%DIST,CROS(J,ICROS)%BOTTOM,CROS(J,ICROS)%KM
    END DO
    !## make sure lowest bottom = 0.0
    MIND=MINVAL(CROS(J,1:DWP(J)%NCROS)%BOTTOM)
    DO JJ=1,DWP(J)%NCROS
     CROS(J,JJ)%BOTTOM=CROS(J,JJ)%BOTTOM-MIND
    ENDDO
!    CROS(J,1:DWP(J)%NCROS)%BOTTOM=CROS(J,1:DWP(J)%NCROS)%BOTTOM-MIND
   END DO

   !## start to intersect all segment/segmentpoints to the model-grid
   ISGLEN=0.0
   DO ISEG=2,NSEG

    X1 =X(ISEG-1); Y1 =Y(ISEG-1); X2 =X(ISEG); Y2 =Y(ISEG)
    !## distance between two points with information
    DXY=((X(ISEG)-X(ISEG-1))**2.0)+((Y(ISEG)-Y(ISEG-1))**2.0)
    IF(DXY.GT.0.0)THEN
     DXY=SQRT(DXY)

     DO JJ=1,4
      RVAL(JJ,0)=(RVAL(JJ,ISEG)-RVAL(JJ,ISEG-1))/DXY
     ENDDO
!     RVAL(:,0)=(RVAL(:,ISEG)-RVAL(:,ISEG-1))/DXY

     DO
      IF(LQD)THEN
       !## intersect line with rectangular-regular-equidistantial-grid
       IF(IMOD_UTL_INTERSECT_EQUI(DELR(0),DELR(NCOL),DELC(NROW),DELC(0),SIMCSIZE,X1,X2,Y1,Y2,MX,N,XA,YA,FA,LN,.FALSE.))EXIT
      ELSE
       IF(IMOD_UTL_INTERSECT_NONEQUI(DELR,DELC,NROW,NCOL,X1,X2,Y1,Y2,MX,N,XA,YA,FA,LN,.FALSE.))EXIT
      ENDIF
      DEALLOCATE(XA,YA,FA,LN); MX=2*MX
      ALLOCATE(XA(MX),YA(MX),LN(MX),FA(MX))
     ENDDO

     !## fill result array
     DXY=0.0
     DO J=1,N

      ISGLEN=ISGLEN+LN(J)
      !## which cross-section is active within current segment
      CALL PCK6RPISG(JCRS,NCRS,ISGLEN)

      IF(LN(J).GT.0.0)THEN
       ICOL=INT(XA(J))
       IROW=INT(YA(J))
       !## within model-domain
       IF(ICOL.GE.1.AND.IROW.GE.1.AND. &
          ICOL.LE.NCOL.AND.IROW.LE.NROW)THEN
        !## interpolate to centre of line ...
        DXY=DXY+(LN(J)/2.0)

        WP=RVAL(1,ISEG-1)+DXY*RVAL(1,0) !waterlevel
        BH=RVAL(2,ISEG-1)+DXY*RVAL(2,0) !waterbottom

        !## correct bottomlevel in case bottomlevel is above waterlevel
        IF(WP-BH.LT.WATERDEPTH)BH=WP-WATERDEPTH

        !## translate from local to global coordinates and get proper wetted perimeter and width of channel!
        CALL PCK4RPISG(JCRS,BH,WP,RWIDTH,WETPER)

        IF(WETPER.GT.0.0)THEN

         NISG = NISG+1

         if (iact.gt.0) then
          ISGLIST(NISG,1) = ILAY
          ISGLIST(NISG,2) = IROW
          ISGLIST(NISG,3) = ICOL
          ISGLIST(NISG,4) = WP
          C               = RVAL(3,ISEG-1)+DXY*RVAL(3,0)  !c-value
          ISGLIST(NISG,5) = LN(J)*WETPER/C                !conductances
          ISGLIST(NISG,6) = BH
          ISGLIST(NISG,7) = RVAL(4,ISEG-1)+DXY*RVAL(4,0)  !inf.factors

          ! necessary for IFVDL=1
          ISGLIST(NISG,8)  = LN(J)
          ISGLIST(NISG,9)  = WETPER
          ISGLIST(NISG,10) = C
          ISGLIST(NISG,11) = RWIDTH

         end if

        ENDIF

        DXY=DXY+(LN(J)/2.0)

       ENDIF
      ENDIF
     ENDDO
    ENDIF
   ENDDO
  ENDIF
 ENDIF
ENDDO

IF(iact.gt.0)THEN

 ALLOCATE(IDF(10)); DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); IDF(I)%IU=0; ENDDO
 IDF%XMIN=SIMBOX(1); IDF%YMIN=SIMBOX(2); IDF%XMAX=SIMBOX(3); IDF%YMAX=SIMBOX(4); IDF%NODATA=NODATA
 IDF%DX=SIMCSIZE; IDF%IEQ=0; IDF%DY=IDF%DX; IDF%IXV=0; IDF%ITB=0; IDF%NCOL=NCOL; IDF%NROW=NROW
 DO I=1,SIZE(IDF);
  IF(.NOT.IDFALLOCATEX(IDF(I)))THEN
   CALL IMOD_UTL_PRINTTEXT('Can not allocate memory for idf(i)',1)
  ENDIF
  IDF(I)%X=0.0
 ENDDO

 !## create grid with waterlevels,bottomlevels and resistances
 DO I=1,NISG
  IROW=ISGLIST(I,2); ICOL=ISGLIST(I,3)
  !## skip nodata
  IF(ISGLIST(I,5).EQ.NODATA)CYCLE
  IDF(1) %X(ICOL,IROW)=IDF(1) %X(ICOL,IROW)+ ISGLIST(I,5)                !## conductance
  IDF(2) %X(ICOL,IROW)=IDF(2) %X(ICOL,IROW)+(ISGLIST(I,4)*ISGLIST(I,5))  !## wl
  IDF(3) %X(ICOL,IROW)=IDF(3) %X(ICOL,IROW)+(ISGLIST(I,6)*ISGLIST(I,5))  !## bh
  IDF(4) %X(ICOL,IROW)=IDF(4) %X(ICOL,IROW)+(ISGLIST(I,7)*ISGLIST(I,5))  !## inf
  IDF(5) %X(ICOL,IROW)=IDF(5) %X(ICOL,IROW)+(ISGLIST(I,10)*ISGLIST(I,5)) !## c
  IDF(7) %X(ICOL,IROW)=IDF(7) %X(ICOL,IROW)+(ISGLIST(I,11)*ISGLIST(I,5)) !## rwidth
  IDF(9) %X(ICOL,IROW)=IDF(9) %X(ICOL,IROW)+ ISGLIST(I,8)                !## total length
  IDF(10)%X(ICOL,IROW)=IDF(10)%X(ICOL,IROW)+(ISGLIST(I,9)*ISGLIST(I,5))  !## wetper
 ENDDO
 DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(IDF(1)%X(ICOL,IROW).GT.0.0)THEN
   IDF(2) %X(ICOL,IROW)=IDF(2) %X(ICOL,IROW)/IDF(1)%X(ICOL,IROW)
   IDF(3) %X(ICOL,IROW)=IDF(3) %X(ICOL,IROW)/IDF(1)%X(ICOL,IROW)
   IDF(4) %X(ICOL,IROW)=IDF(4) %X(ICOL,IROW)/IDF(1)%X(ICOL,IROW)
   IDF(5) %X(ICOL,IROW)=IDF(5) %X(ICOL,IROW)/IDF(1)%X(ICOL,IROW)
   IDF(7) %X(ICOL,IROW)=IDF(7) %X(ICOL,IROW)/IDF(1)%X(ICOL,IROW)
   IDF(9) %X(ICOL,IROW)=IDF(9) %X(ICOL,IROW) 
   IDF(10)%X(ICOL,IROW)=IDF(10)%X(ICOL,IROW)/IDF(1)%X(ICOL,IROW)
  ENDIF
 ENDDO; ENDDO
  
 !## existence of two-dimensional cross-sections
 IF(N2DIM.GT.0)THEN 

  REWIND(ISGIU(1)); READ(ISGIU(1),*) NISGH
  DO I=1,NISGH
   READ(ISGIU(1),*) CISGH,ISEG,NSEG,ICLC,NCLC,ICRS,NCRS,ISTW,NSTW
   IF(NSEG.LT.2)CYCLE; IF(NCLC.LT.2)CYCLE

   !## read segments
   IREC=ISEG-1; DO J=1,NSEG; READ(ISGIU(2),REC=IREC+J+ICF) X(J),Y(J); END DO

   !## evaluate whether line is within model areas, otherwise skip rest of procedure ...
   IF(MAXVAL(X(1:NSEG)).LE.SIMBOX(1).OR.MAXVAL(Y(1:NSEG)).LE.SIMBOX(2).OR. &
      MINVAL(X(1:NSEG)).GE.SIMBOX(3).OR.MINVAL(Y(1:NSEG)).GE.SIMBOX(4))CYCLE

   !## read cross-section information from *.isc
   IF(NCRS.GT.SIZE(DWP))THEN; IF(ALLOCATED(DWP))DEALLOCATE(DWP);ALLOCATE(DWP(NCRS)); ENDIF

   !## read cross-sectional data
   MAXCROS=0; IREC=ICRS-1
   DO J=1,NCRS
    READ(ISGIU(5),REC=IREC+J+ICF) DWP(J)%NCROS
    !## take two-dimensional cross-sections only
    MAXCROS=MAXCROS+MIN(0,DWP(J)%NCROS)
   END DO
   MAXCROS=ABS(MAXCROS)
   !## skip this segment since no two-dimensional cross-sections available
   IF(MAXCROS.EQ.0)CYCLE

   IF(MAXCROS.GT.SIZE(CROS,2).OR.NCRS.GT.SIZE(CROS,1))THEN
    IF(ALLOCATED(CROS))DEALLOCATE(CROS); ALLOCATE(CROS(NCRS,MAXCROS))
   ENDIF

   !## read cross-sectional data
   IREC=ICRS-1
   DO J=1,NCRS
    READ(ISGIU(5),REC=IREC+J+ICF) DWP(J)%NCROS,IREF,DWP(J)%DIST,CNAME
    IF(DWP(J)%NCROS.GE.0)CYCLE

    IF(DWP(J)%NCROS.GT.0)THEN
     IF(DWP(J)%NCROS.LE.2)THEN
      CALL IMOD_UTL_PRINTTEXT('iMODFLOW can not apply a 1D cross-section with less or equal 2 points',1)
      CALL IMOD_UTL_PRINTTEXT('Cross-section name '//TRIM(CNAME),2)
     ENDIF
    ELSE
     IF(ABS(DWP(J)%NCROS).LE.1)THEN
      CALL IMOD_UTL_PRINTTEXT('iMODFLOW can not apply a 2D cross-section with no locations',1)
      CALL IMOD_UTL_PRINTTEXT('Cross-section name '//TRIM(CNAME),2)
     ENDIF
    ENDIF

    JREC=IREF-1; ICSTYPE=0
    DO ICROS=1,ABS(DWP(J)%NCROS)
     JREC=JREC+1
     IF(ICROS.EQ.1.OR.ICSTYPE.EQ.0)THEN
     READ(ISGIU(6),REC=JREC+ICF) CROS(J,ICROS)%DIST,CROS(J,ICROS)%BOTTOM,CROS(J,ICROS)%KM
      !## read bathemetry with thressholds
      IF(CROS(J,ICROS)%DIST.LT.0.AND.CROS(J,ICROS)%BOTTOM.LT.0.0)ICSTYPE=1
      CROS(J,ICROS)%ZP=0.0
     ELSE
      READ(ISGIU(6),REC=JREC+ICF) CROS(J,ICROS)%DIST,CROS(J,ICROS)%BOTTOM,ZM,ZC,ZP
      CROS(J,ICROS)%KM=REAL(ZM)+REAL(ZC)/100.0
      CROS(J,ICROS)%ZP=REAL(ZP) 
     ENDIF
!     READ(ISGIU(6),REC=JREC+ICF) CROS(J,ICROS)%DIST,CROS(J,ICROS)%BOTTOM,CROS(J,ICROS)%KM
    END DO

    CALL ISGADJUSTCOMPUTEXY(J)         !## compute correct x/y coordinate of current cross-section
    CALL IDFIROWICOL(IDF(1),IROW,ICOL,ISGX,ISGY)               !## get location in raster
    !## skip if outside current model network
    IF(IROW.EQ.0.OR.ICOL.EQ.0)CYCLE
    IF(ISGATTRIBUTES_2DCROSS_READ(J,ICROSS,PCROSS,ZCHK))THEN               !## read bathymetry current cross-section
     WL  =IDF(2)%X(ICOL,IROW)                                  !## waterlevel at cross-section
     INFF=IDF(4)%X(ICOL,IROW)                                  !## infiltration factor at location of cross-section
     C   =IDF(5)%X(ICOL,IROW)                                  !## resistance at location of cross-section
     !## intersection migth miss the cell
     IF(C.LE.0.0)THEN
      !## look around
IRLOOP: DO IR=MAX(1,IROW-1),MIN(NROW,IROW+1)
       DO IC=MAX(1,ICOL-1),MIN(NCOL,ICOL+1)
        !## infiltration factor at location of cross-section
        INFF=IDF(4)%X(IC,IR)                                  
        !## waterlevel at cross-section
        WL=IDF(2)%X(IC,IR)   
        !## resistance
        C=IDF(8)%X(IC,IR)
        IF(C.NE.0.0)EXIT IRLOOP
       ENDDO
      ENDDO IRLOOP
     ENDIF
     CALL ISG2GRID_BATHEMETRY(IDF,SIZE(IDF),ICROSS,PCROSS,ZCHK,WL,C,INFF)  !## adjust stage grid for bathemetrie
     CALL IDFDEALLOCATEX(ICROSS); CALL IDFDEALLOCATEX(PCROSS)
    ENDIF

   END DO
  ENDDO
 
 ENDIF
 
 !## extent grids based upon their width
 CALL ISG2GRID_EXTENT_WITH_WIDTH(SIZE(IDF),IDF)

 !## clean list
! DO I=1,10; ISGLIST(:,I)=0.0; ENDDO
 DO J=1,SIZE(ISGLIST,1); DO I=1,SIZE(ISGLIST,2); ISGLIST(J,I)=0.0; ENDDO; ENDDO

 !## reuse isg from the grid, that is the consequence of using 2d cross-sections
 NISG=0
 DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(IDF(1)%X(ICOL,IROW).GT.0.0)THEN
   NISG=NISG+1
   ISGLIST(NISG,1) = ILAY
   ISGLIST(NISG,2) = IROW
   ISGLIST(NISG,3) = ICOL
   ISGLIST(NISG,5)= IDF(1)%X(ICOL,IROW)  ! cond
   ISGLIST(NISG,4)= IDF(2)%X(ICOL,IROW)  ! stage
   ISGLIST(NISG,6)= IDF(3)%X(ICOL,IROW)  ! rbot
   ISGLIST(NISG,7)= IDF(4)%X(ICOL,IROW)  ! inf.
   ISGLIST(NISG,8)= IDF(9)%X(ICOL,IROW)  ! length
   ISGLIST(NISG,9)= IDF(10)%X(ICOL,IROW) ! wettedperimeter
   ISGLIST(NISG,10)=IDF(5)%X(ICOL,IROW)  ! resis

  ENDIF
 ENDDO; ENDDO

 CALL IDFDEALLOCATE(IDF,SIZE(IDF))
 CALL IMOD_UTL_PRINTTEXT('Finished gridding 2d cross-sections, no. of isg elements '//TRIM(IMOD_UTL_ITOS(NISG)),3)

ENDIF

DO I=1,8; CLOSE(ISGIU(I)); END DO

IF(ALLOCATED(QSORT))DEALLOCATE(QSORT); IF(ALLOCATED(DIST))DEALLOCATE(DIST)
IF(ALLOCATED(CROS))DEALLOCATE(CROS);   IF(ALLOCATED(RVAL))DEALLOCATE(RVAL)
IF(ALLOCATED(IPOS))DEALLOCATE(IPOS);   IF(ALLOCATED(DWP))DEALLOCATE(DWP)
DEALLOCATE(XA,YA,FA,LN,XNR)

RETURN
END SUBROUTINE

!###====================================================================
SUBROUTINE PCK2RPISG(SDATE,EDATE,TTIME,QSORT,XNR,LU,MAXITEMS,RVAL, &
                     NR,IREF,IISGH,ITYPE,NODATA)
!###====================================================================
USE MOD_GLBVAR, ONLY : LINE,ISS
USE IMOD_UTL, ONLY: IMOD_UTL_IDATETOJDATE,IMOD_UTL_GETMED2,IMOD_UTL_ITOS,IMOD_UTL_PRINTTEXT,ICF
IMPLICIT NONE
REAL,INTENT(IN) :: NODATA
INTEGER,INTENT(IN) :: SDATE,EDATE,TTIME,MAXITEMS,LU,IISGH,NR,IREF,ITYPE
REAL,INTENT(INOUT),DIMENSION(TTIME,MAXITEMS) :: QSORT
REAL,DIMENSION(MAXITEMS),INTENT(INOUT) :: RVAL,XNR
INTEGER :: IR,I,J,I1,I2,IDATE,NDATE,NAJ,IREC,N
INTEGER :: IAVERAGE=2 !## median values
REAL :: X

IREC=IREF-1

IF(NR.GT.0)THEN

 IF(ISS.EQ.1)QSORT= 0.0
 IF(ISS.EQ.2)QSORT=NODATA 

 I1=1; IR=0; XNR=0.0
 
 DO 
  
  IR=IR+1; IF(IR.GT.NR)EXIT
    
  IREC=IREC+1

  RVAL=0.0
  IF(ITYPE.EQ. 1)READ(LU,REC=IREC+ICF) IDATE,(RVAL(I),I=1,MAXITEMS)
  IF(ITYPE.EQ.-1)READ(LU,REC=IREC+ICF) IDATE,(RVAL(I),I=1,2)

  !## don't bother for steady-state, take the mean!
  IF(ISS.EQ.1)THEN
   DO I=1,MAXITEMS; IF(RVAL(I).NE.NODATA)THEN; QSORT(1,I)=QSORT(1,I)+RVAL(I); XNR(I)=XNR(I)+1.0; ENDIF; ENDDO
  !## transient simulation
  ELSEIF(ISS.EQ.2)THEN
   
   IDATE=IMOD_UTL_IDATETOJDATE(IDATE)
   
   !## try to speed up the search for transient searches
   IF(IR.EQ.1)THEN
    READ(LU,REC=IREC+1+ICF) NDATE
    !## number of days in between
    N=IMOD_UTL_IDATETOJDATE(NDATE)-IDATE
    N=(SDATE-IDATE)/N; N=MAX(N-1,0)
    IF(IR+N.LT.NR)THEN
     READ(LU,REC=IREC+1+N+ICF) NDATE
     !## okay, take this jump in time
     IF(IMOD_UTL_IDATETOJDATE(NDATE).LE.SDATE)THEN
      IR  =IR  +N
      IREC=IREC+N
     ENDIF
    ENDIF
   ENDIF

   NDATE=EDATE
   IF(IR.LT.NR)THEN
    READ(LU,REC=IREC+1+ICF) NDATE
    NDATE=IMOD_UTL_IDATETOJDATE(NDATE)
   ENDIF
   NDATE=MIN(NDATE,EDATE)

   !## stop searchin for data, outside modeling window!
   IF(IDATE.GT.EDATE)EXIT

   !## within modeling window
   IF(NDATE.GE.SDATE)THEN

    N=NDATE-SDATE
    IF(IDATE.GT.SDATE)N=N-(IDATE-SDATE)
    I2=I1+N-1

    DO I=1,MAXITEMS
     QSORT(I1:I2,I)=RVAL(I)
    END DO

    I1=I2+1

   ENDIF
  ENDIF

 END DO

 IF(ISS.EQ.1)THEN
  !## determine for each period appropriate attribute term
  IF(NR.EQ.0)CALL IMOD_UTL_PRINTTEXT('No data found for steady-state in ISG file!',2)
  DO I=1,MAXITEMS
   IF(XNR(I).GT.0.0)THEN
    RVAL(I)=QSORT(1,I)/XNR(I)
   ELSE
    RVAL(I)=NODATA
   ENDIF
  ENDDO
 ELSEIF(ISS.EQ.2)THEN
  IF(IAVERAGE.EQ.1)THEN
   !## arithmetic mean
   XNR=0.0; RVAL=0.0
   DO I=1,MAXITEMS
    DO J=1,TTIME
     IF(QSORT(J,I).NE.NODATA)THEN
      RVAL(I)=RVAL(I)+QSORT(J,I)
      XNR(I) =XNR(I)+1.0
     ENDIF
    ENDDO
    IF(XNR(I).GT.0.0)THEN
     RVAL(I)=RVAL(I)/XNR(I)
    ELSE
     RVAL(I)=NODATA
    ENDIF
   ENDDO 
  ELSEIF(IAVERAGE.EQ.2)THEN
   !## median
   DO I=1,MAXITEMS
    RVAL(I)=IMOD_UTL_GETMED2(QSORT(:,I),TTIME,NODATA,NAJ) 
    IF(NAJ.EQ.0)CALL IMOD_UTL_PRINTTEXT('No data found for current stress period in ISG file!',2)
   ENDDO
  ENDIF
 ENDIF

ELSE
 IF(ITYPE.EQ.1)CALL IMOD_UTL_PRINTTEXT('No waterlevels found for SEGMENT '//TRIM(IMOD_UTL_ITOS(IISGH)),2)
ENDIF

RETURN
END SUBROUTINE

!###====================================================================
SUBROUTINE PCK4RPISG(JCRS,BH,WP,RWIDTH,WETPER)
!###====================================================================
!RE.: In case waterlevel exceeds cross-section, rwidth and wetper will
!     yield max. values based upon cross-section information only!
!###====================================================================
USE MOD_ISGVAR, ONLY : DWP,CROS
IMPLICIT NONE
INTEGER,INTENT(IN) :: JCRS
REAL,INTENT(IN) :: BH,WP
REAL,INTENT(OUT) :: RWIDTH,WETPER
INTEGER :: ICROS
REAL :: DISTB,DISTW,FACTOR,X1,X2,WPCOR

WPCOR =WP-BH
WETPER=0.0

!## use default cross-section whenever no cross-section has been fouond
IF(JCRS.EQ.0)THEN
 RWIDTH=5.0
 WETPER=WPCOR+RWIDTH
 RETURN
ENDIF

!## find left-x-coordinate
X1=CROS(JCRS,1)%DIST
DO ICROS=1,DWP(JCRS)%NCROS-1
 IF(CROS(JCRS,ICROS)%BOTTOM  .GE.WPCOR.AND. &
    CROS(JCRS,ICROS+1)%BOTTOM.LE.WPCOR)THEN
  DISTB  = CROS(JCRS,ICROS)%BOTTOM-CROS(JCRS,ICROS+1)%BOTTOM
  DISTW  = CROS(JCRS,ICROS)%BOTTOM-WPCOR
  
  IF(DISTB.NE.0.0)THEN
  FACTOR = DISTW/DISTB
  X1     = FACTOR*(CROS(JCRS,ICROS+1)%DIST-CROS(JCRS,ICROS)%DIST)
  X1     = CROS(JCRS,ICROS)%DIST+X1

  !## wetted perimeter
  FACTOR  =(X1-CROS(JCRS,ICROS+1)%DIST)**2.0+(WPCOR-CROS(JCRS,ICROS+1)%BOTTOM)**2.0
  IF(FACTOR.NE.0.0)FACTOR=SQRT(FACTOR)
  WETPER=WETPER+FACTOR
  ENDIF
  
 ENDIF
END DO

!## find right-x-coordinate
X2=CROS(JCRS,DWP(JCRS)%NCROS)%DIST
DO ICROS=DWP(JCRS)%NCROS-1,1,-1
 IF(CROS(JCRS,ICROS+1)%BOTTOM.GE.WPCOR.AND. &
    CROS(JCRS,ICROS)%BOTTOM  .LE.WPCOR)THEN
  DISTB  = CROS(JCRS,ICROS+1)%BOTTOM-CROS(JCRS,ICROS)%BOTTOM
  DISTW  = CROS(JCRS,ICROS+1)%BOTTOM-WPCOR

  IF(DISTB.NE.0.0)THEN
  FACTOR = DISTW/DISTB
  X2     = FACTOR*(CROS(JCRS,ICROS+1)%DIST-CROS(JCRS,ICROS)%DIST)
  X2     = CROS(JCRS,ICROS+1)%DIST-X2

  !## wetted perimeter
  FACTOR =(X2-CROS(JCRS,ICROS)%DIST)**2.0+(WPCOR-CROS(JCRS,ICROS)%BOTTOM)**2.0
  IF(FACTOR.NE.0.0)FACTOR=SQRT(FACTOR)
  WETPER=WETPER+FACTOR
  ENDIF
  
 ENDIF
END DO

!## proces wetted perimeter for 'in-between' sections
DO ICROS=1,DWP(JCRS)%NCROS-1
 IF(CROS(JCRS,ICROS)%BOTTOM  .LT.WPCOR.AND. &
    CROS(JCRS,ICROS+1)%BOTTOM.LT.WPCOR)THEN
  !## wetted perimeter
  FACTOR =(CROS(JCRS,ICROS)%DIST-CROS(JCRS,ICROS+1)%DIST)**2.0+ &
          (CROS(JCRS,ICROS)%BOTTOM-CROS(JCRS,ICROS+1)%BOTTOM)**2.0
  IF(FACTOR.NE.0.0)FACTOR=SQRT(FACTOR)
  WETPER=WETPER+FACTOR
 ENDIF
END DO

RWIDTH=X2-X1

RETURN
END SUBROUTINE

!###====================================================================
SUBROUTINE PCK5RPISG(X,Y,DIST,NXY,NSEG)
!###====================================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: NXY,NSEG
REAL,INTENT(IN),DIMENSION(NXY) :: X,Y
REAL,INTENT(OUT),DIMENSION(NXY) :: DIST
INTEGER :: ISEG
REAL :: DXY

DIST=0.0
DO ISEG=2,NSEG
 DXY=((X(ISEG)-X(ISEG-1))**2.0)+((Y(ISEG)-Y(ISEG-1))**2.0)
 IF(DXY.GT.0.0)DXY=SQRT(DXY)
 DIST(ISEG)=DIST(ISEG-1)+DXY
END DO

RETURN
END SUBROUTINE

!###====================================================================
SUBROUTINE PCK6RPISG(JCRS,NCRS,ISGLEN)
!###====================================================================
USE MOD_ISGVAR, ONLY : DWP
IMPLICIT NONE
REAL,INTENT(IN) :: ISGLEN
INTEGER,INTENT(IN) :: NCRS
INTEGER,INTENT(OUT) :: JCRS
INTEGER :: IC1,IC2,ICRS
REAL :: DC,D,DCL,DCR

JCRS=0

IF(NCRS.EQ.1.AND.DWP(1)%NCROS.GT.0)THEN; JCRS=1; RETURN; ENDIF

IC1 =0  !## cross-section left
IC2 =0  !## cross-section right
DCL =10.0E10  !left
DCR =10.0E10  !right
DC  =10.0E10
DO ICRS=1,NCRS
 !## take only one-dimensional cross-sections
 IF(DWP(ICRS)%NCROS.LE.0)CYCLE
 D=DWP(ICRS)%DIST-ISGLEN
 IF(D.GT.0)THEN !## lies right
  !## take whenever closer than next cross-section in the front
  IF(D.LE.DCL)THEN
   DCL=D
   IC2=ICRS
  ENDIF
 ELSE           !## lies behind
  !## take whenever closer than previous cross-section in the back
  IF(ABS(D).LE.DCR)THEN
   DCR=ABS(D)
   IC1=ICRS
  ENDIF
 ENDIF
ENDDO
IF(IC1.EQ.0)IC1=IC2
JCRS=IC1

RETURN
END SUBROUTINE

!###====================================================================
SUBROUTINE PCK7RPISG(ITYP,LU,NTYP,NXY,NSEG,X,Y,DIST,IPOS,ITYPE)
!###====================================================================
USE IMOD_UTL, ONLY: ICF
IMPLICIT NONE
INTEGER,INTENT(IN) :: NXY,ITYP,LU,NTYP,ITYPE
INTEGER,INTENT(INOUT) :: NSEG
REAL,INTENT(INOUT),DIMENSION(NXY) :: X,Y
REAL,INTENT(INOUT),DIMENSION(NXY) :: DIST
INTEGER,INTENT(OUT),DIMENSION(NXY) :: IPOS
INTEGER :: IREC,ISEG,IREF,I,J
REAL :: DXY,D1,D2,F,XC,YC

!#include calculation nodes as segments!
IREC=ITYP-1
!#determine which nodes possess heads/etc.
DO I=1,NTYP
!#get tot distances
 CALL PCK5RPISG(X,Y,DIST,NXY,NSEG)
 IREC=IREC+1
 READ(LU,REC=IREC+ICF) J,IREF,DXY  !k=number,dxy=distance from beginning

!#find position in between segments
 DO ISEG=2,NSEG
  IF(DXY.GE.DIST(ISEG-1).AND.DXY.LE.DIST(ISEG))EXIT
 END DO

!#distance current segment
 D1= DIST(ISEG)-DIST(ISEG-1)
 D2= DXY-DIST(ISEG-1)
 !## segment itself is zero
 IF(D1.LE.0.0)THEN
  F=1.0
 ELSE
  F=D2/D1
 ENDIF
 
!#put in extra coordinate
 IF(F.LE.0.01)THEN
  IPOS(ISEG-1)=IREC*ITYPE  !##put data to current node
 ELSEIF(F.GE.0.99)THEN
  IPOS(ISEG)=IREC*ITYPE    !##put data to current node
 ELSE
  XC=X(ISEG-1)+((X(ISEG)-X(ISEG-1))*F)
  YC=Y(ISEG-1)+((Y(ISEG)-Y(ISEG-1))*F)
!##position coordinates in between
  X(ISEG+1:NSEG+1)   =X(ISEG:NSEG)
  Y(ISEG+1:NSEG+1)   =Y(ISEG:NSEG)
  IPOS(ISEG+1:NSEG+1)=IPOS(ISEG:NSEG)
  X(ISEG)            =XC
  Y(ISEG)            =YC
  NSEG               =NSEG+1      !##increase number of segments
  IPOS(ISEG)         =IREC*ITYPE         !##put data to current node
 ENDIF

!## duplicate point in case of structure
 IF(ITYPE.EQ.-1)THEN
  X(ISEG+1:NSEG+1)   =X(ISEG:NSEG)
  Y(ISEG+1:NSEG+1)   =Y(ISEG:NSEG)
  IPOS(ISEG+1:NSEG+1)=IPOS(ISEG:NSEG)
  X(ISEG)            =XC
  Y(ISEG)            =YC
  NSEG               =NSEG+1     !##increase number of segments
  IPOS(ISEG)         =I*ITYPE    !##put data to current node
 ENDIF

END DO

!## put last ipos() for secerity reasons ... could happen in case of accuray of coordinates
IF(IPOS(NSEG).EQ.0)THEN
 DO I=NSEG,1,-1
  IF(IPOS(I).GT.0)THEN
   IPOS(NSEG)=IPOS(I)
   EXIT
  ENDIF
 END DO
ENDIF

RETURN
END SUBROUTINE

!###====================================================================
SUBROUTINE PCK3RPISG(ISGLIST,NISG)
!###====================================================================
USE IMOD_UTL, ONLY : IMOD_UTL_PRINTTEXT,IMOD_UTL_RTOS
USE MOD_BASVAR, ONLY : DELR,DELC
USE GLOBAL, ONLY: CC, CV, BOTM, LBOTM
USE GWFRIVMODULE, ONLY: IFVDL,ISFT,SFT

IMPLICIT NONE

INTEGER, INTENT(IN) :: NISG
REAL,DIMENSION(NISG,11),INTENT(INOUT) :: ISGLIST

REAL :: LEKCONDUCTANCE_Y

INTEGER :: I, ILAY, IROW, ICOL
REAL :: DXY,LI,BIN,C0,C1,KH,KV,COND_IN,FCT,D
REAL, PARAMETER :: TINY=1.0E-20
LOGICAL :: LUSED ! PKS

IF (IFVDL.EQ.0) RETURN

DO I = 1, NISG
 ILAY = ISGLIST(I,1)
 IROW = ISGLIST(I,2)
 ICOL = ISGLIST(I,3)
 DXY=(DELR(ICOL)-DELR(ICOL-1))*(DELC(IROW-1)-DELC(IROW))
 CALL PKS7MPITRN(ICOL,IROW,ILAY,LUSED) ! PKS
 IF (LUSED) THEN ! PKS
 FCT=ISGLIST(I,7)   !## infiltration factor
 LI =ISGLIST(I,8)   !## length of river segment
 BIN=ISGLIST(I,9)   !## wetted perimeter
 C0 = ISGLIST(I,10) !## resistance
 C1=CV(ICOL,IROW,ILAY)
 IF(C1.GT.0.0)THEN
  C1 =DXY/C1                                    !## resistance of aquitard (1/day -> day)
  IF(ISFT.EQ.1)THEN
   D  =SFT(ICOL,IROW,1)                         !## thickness of aquifer
   KH =SFT(ICOL,IROW,2)                         !## permeability
  ELSE
   D = BOTM(ICOL,IROW,LBOTM(ILAY)-1)-BOTM(ICOL,IROW,LBOTM(ILAY)) !## thickness of aquifer
   KH =CC(ICOL,IROW,ILAY)/(D+TINY)              !## permeability
  ENDIF
  KV =KH/10.0                                   !## vertical permeability

  IF(KH.LE.0.0)CALL IMOD_UTL_PRINTTEXT('Error KH='//TRIM(IMOD_UTL_RTOS(KH,'F',7)),0)
  IF(D .LE.0.0)CALL IMOD_UTL_PRINTTEXT('Error D ='//TRIM(IMOD_UTL_RTOS(D ,'F',7)),0)

  !## process formulae van de Lange - infiltratie weerstand
  COND_IN=0.0; IF(FCT.GT.0.0.AND.FCT.LE.1.0)COND_IN=LEKCONDUCTANCE_Y(DXY,D,KV,KH,C1,LI,BIN,C0/FCT)
  !## process formulae van de Lange - drainage weerstand
  ISGLIST(I,5)=LEKCONDUCTANCE_Y(DXY,D,KV,KH,C1,LI,BIN,C0)
  !## infiltration factor
  ISGLIST(I,7)=FCT; IF(ISGLIST(I,5).GT.0.0)ISGLIST(I,7)=COND_IN/ISGLIST(I,5)

  IF(ISGLIST(I,7).GT.1.0.OR.ISGLIST(I,7).LT.0.0)THEN
   CALL IMOD_UTL_PRINTTEXT('Error C0/FCT    ='//TRIM(IMOD_UTL_RTOS(C0/FCT,'F',7)),0)
   CALL IMOD_UTL_PRINTTEXT('Error COND_INF  ='//TRIM(IMOD_UTL_RTOS(COND_IN,'F',7)),0)
   CALL IMOD_UTL_PRINTTEXT('Error C0        ='//TRIM(IMOD_UTL_RTOS(C0,'F',7)),0)
   CALL IMOD_UTL_PRINTTEXT('Error COND_DRN  ='//TRIM(IMOD_UTL_RTOS(ISGLIST(NISG,5),'F',7)),0)
   CALL IMOD_UTL_PRINTTEXT('Error Inf.Factor='//TRIM(IMOD_UTL_RTOS(ISGLIST(NISG,7),'F',7)),0)
  ENDIF
 ELSE
  ISGLIST(I,5)=0.0
 ENDIF
 ENDIF !PKS
END DO

RETURN
END SUBROUTINE

!###===============================================================================
 SUBROUTINE ISGADJUSTCOMPUTEXY(J)
 !###===============================================================================
 USE MOD_ISGVAR
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: J
 INTEGER :: I,K
 REAL :: DXY,F,TD,DIST

 DIST=DWP(J)%DIST

 TD=0.0
 DO I=2,NSEG
  DXY=((X(I)-X(I-1))**2.0)+((Y(I)-Y(I-1))**2.0)
  IF(DXY.GT.0.0)DXY=SQRT(DXY)
  TD=TD+DXY; IF(TD.GE.DIST)EXIT
 END DO

 !## distance current segment
 F   =(DIST-(TD-DXY))/DXY
 ISGX= X(I-1)+(X(I)-X(I-1))*F
 ISGY= Y(I-1)+(Y(I)-Y(I-1))*F

! !## compute rest of segment length to yield total length
! K=I+1
! DO I=K,NPNT
!  J  =  J+1
!  DXY=((ISP(J)%X-ISP(J-1)%X)**2.0)+((ISP(J)%Y-ISP(J-1)%Y)**2.0)
!  IF(DXY.GT.0.0)DXY=SQRT(DXY)
!  TD=TD+DXY
! END DO

 END SUBROUTINE ISGADJUSTCOMPUTEXY

 !###======================================================================
 LOGICAL FUNCTION ISGATTRIBUTES_2DCROSS_READ(J,IDF,PIDF,ZCHK)
 !###======================================================================
 USE IMOD_UTL, ONLY : IMOD_UTL_PRINTTEXT
 USE IMOD_IDF_PAR
 USE IMOD_IDF
 USE MOD_ISGVAR
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: J
 REAL,INTENT(OUT) :: ZCHK
 INTEGER :: I,IROW,ICOL,ICHK
 TYPE(IDFOBJ),INTENT(OUT) :: IDF,PIDF
 LOGICAL :: ISGATTRIBUTES_2DCROSS_IDFDIM

 ISGATTRIBUTES_2DCROSS_READ=.FALSE.

 CALL IDFNULLIFY(IDF); CALL IDFNULLIFY(PIDF)
 IF(.NOT.ISGATTRIBUTES_2DCROSS_IDFDIM(IDF,J))THEN
  CALL IMOD_UTL_PRINTTEXT('iMOD can not present results in table. No cellsizes can be determined',1)
 ENDIF

 CALL IDFCOPY(IDF,PIDF)
 IF(.NOT.IDFALLOCATEX(IDF))RETURN
 IF(.NOT.IDFALLOCATEX(PIDF))RETURN
 IDF%NODATA=-9999; IDF%X=IDF%NODATA
 PIDF%NODATA=-9999; PIDF%X=0.0  !## on default, inundated whenever stage higher than riverbed

 ICHK=0; IF(CROS(J,1)%DIST.LT.0.0.AND.CROS(J,1)%BOTTOM.LT.0.0)THEN
  ICHK=1; ZCHK=CROS(J,1)%KM
 ENDIF

 !## skip first (dx,dy)
 DO I=2,ABS(DWP(J)%NCROS)
  CALL IDFIROWICOL(IDF,IROW,ICOL,CROS(J,I)%DIST,CROS(J,I)%BOTTOM) 
  IDF%X(ICOL,IROW)=CROS(J,I)%KM 
  !## store inundation pointer
  PIDF%X(ICOL,IROW)=1.0; IF(ICHK.EQ.1)PIDF%X(ICOL,IROW)=CROS(J,I)%ZP
 ENDDO 

 ISGATTRIBUTES_2DCROSS_READ=.TRUE.

 END FUNCTION ISGATTRIBUTES_2DCROSS_READ

 !###======================================================================
 LOGICAL FUNCTION ISGATTRIBUTES_2DCROSS_IDFDIM(IDF,J)
 !###======================================================================
 USE IMOD_IDF_PAR
 USE MOD_ISGVAR
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: J
 TYPE(IDFOBJ),INTENT(OUT) :: IDF
 INTEGER :: I,II
 REAL :: D

 ISGATTRIBUTES_2DCROSS_IDFDIM=.FALSE.

 IDF%DX=ABS(CROS(J,1)%DIST)
 IDF%DY=ABS(CROS(J,1)%BOTTOM)

 IF(IDF%DX.LE.0.0.OR.IDF%DY.LE.0.0)RETURN

 IDF%XMIN=MINVAL(CROS(J,2:ABS(DWP(J)%NCROS))%DIST)  -0.5*IDF%DX
 IDF%XMAX=MAXVAL(CROS(J,2:ABS(DWP(J)%NCROS))%DIST)  +0.5*IDF%DX
 IDF%YMIN=MINVAL(CROS(J,2:ABS(DWP(J)%NCROS))%BOTTOM)-0.5*IDF%DY
 IDF%YMAX=MAXVAL(CROS(J,2:ABS(DWP(J)%NCROS))%BOTTOM)+0.5*IDF%DY

 IDF%IEQ=0; IDF%ITB=0; IDF%IXV=0
 IDF%NCOL=(IDF%XMAX-IDF%XMIN)/IDF%DX
 IDF%NROW=(IDF%YMAX-IDF%YMIN)/IDF%DY

 ISGATTRIBUTES_2DCROSS_IDFDIM=.TRUE.

 END FUNCTION ISGATTRIBUTES_2DCROSS_IDFDIM

 !###====================================================================
 SUBROUTINE ISG2GRID_BATHEMETRY(IDF,NIDF,ICROSS,PCROSS,ZCHK,WL,C,INFF)
 !###====================================================================
 USE IMOD_IDF_PAR
 USE IMOD_IDF, ONLY : IDFGETAREA,IDFGETLOC,IDFIROWICOL
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NIDF
 TYPE(IDFOBJ),DIMENSION(NIDF),INTENT(INOUT) :: IDF
 TYPE(IDFOBJ),INTENT(INOUT) :: ICROSS,PCROSS
 REAL,INTENT(IN) :: WL,C,INFF,ZCHK
 INTEGER :: IR1,IR2,IC1,IC2,IROW,ICOL,JROW,JCOL
 REAL :: XC,YC,CR

 !## defined cross-sections are finer than model network
 IF(ICROSS%DX.LE.IDF(1)%DX)THEN
  !## spottify cross-section (actual bathemetry of current 2d cross-section) in mother idf
  DO IROW=1,ICROSS%NROW; DO ICOL=1,ICROSS%NCOL
   !## nodata found for waterlevel
   IF(ICROSS%X(ICOL,IROW).EQ.ICROSS%NODATA)CYCLE
   !## waterlevel equal or less than riverbot
   IF(WL.LE.ICROSS%X(ICOL,IROW))CYCLE
   !## if inundation-criterion applied, only inundate if zchk criterion is met
   IF(PCROSS%X(ICOL,IROW).LT.0.0.AND.WL.LE.ZCHK)CYCLE
   CALL IDFGETLOC(ICROSS  ,IROW,ICOL,XC,YC)
   CALL IDFIROWICOL(IDF(1),JROW,JCOL,XC,YC)
   IF(JROW.NE.0.AND.JCOL.NE.0)THEN
    !## manipulate resistance 
    CR=C*ABS(PCROSS%X(ICOL,IROW))
    IF(IDF(6)%X(JCOL,JROW).EQ.0.0)THEN
     IDF(1)%X(JCOL,JROW)=IDFGETAREA(ICROSS,ICOL,IROW)/CR
    ELSE
     IDF(1)%X(JCOL,JROW)=IDF(1)%X(JCOL,JROW)+IDFGETAREA(ICROSS,ICOL,IROW)/CR
    ENDIF
    IDF(2)%X(JCOL,JROW)=WL                   !## waterlevel
    IDF(3)%X(JCOL,JROW)=ICROSS%X(ICOL,IROW)  !## bottomlevel
    IDF(4)%X(JCOL,JROW)=INFF
    IDF(6)%X(JCOL,JROW)=IDF(6)%X(JCOL,JROW)+1.0
   ENDIF
  ENDDO; ENDDO
 !## defined cross-sections are coarser than model network
 ELSE
  CALL IDFIROWICOL(IDF(1),IR2,IC1,ICROSS%XMIN,ICROSS%YMIN)
  CALL IDFIROWICOL(IDF(1),IR1,IC2,ICROSS%XMAX,ICROSS%YMAX)
  IF(IC2.EQ.0)IC2=IDF(1)%NCOL; IF(IR2.EQ.0)IR2=IDF(1)%NROW   
  IC1=MAX(1,IC1); IC2=MIN(IC2,IDF(1)%NCOL)
  IR1=MAX(1,IR1); IR2=MIN(IR2,IDF(1)%NROW)
  !## spottify cross-section (actual bathemetry of current 2d cross-section) in mother idf
  DO IROW=IR1,IR2; DO ICOL=IC1,IC2
   CALL IDFGETLOC(IDF(1)  ,IROW,ICOL,XC,YC)
   CALL IDFIROWICOL(ICROSS,JROW,JCOL,XC,YC)
   IF(JROW.NE.0.AND.JCOL.NE.0)THEN
    !## nodata found for waterlevel
    IF(ICROSS%X(JCOL,JROW).EQ.ICROSS%NODATA)CYCLE
    !## waterlevel equal or less than riverbot
    IF(WL.LT.ICROSS%X(JCOL,JROW))CYCLE
    !## if inundation-criterion applied, only inundate if zchk criterion is met
    IF(PCROSS%X(JCOL,JROW).LT.0.0.AND.WL.LE.ZCHK) CYCLE
    !## manipulate resistance
    CR=C*ABS(PCROSS%X(JCOL,JROW))
    IF(IDF(6)%X(ICOL,IROW).EQ.0.0)THEN
     IDF(1)%X(ICOL,IROW)=IDFGETAREA(IDF(1),ICOL,IROW)/CR
    ELSE
     IDF(1)%X(ICOL,IROW)=IDF(1)%X(ICOL,IROW)+IDFGETAREA(IDF(1),ICOL,IROW)/CR
    ENDIF
    IDF(2)%X(ICOL,IROW)=WL
    IDF(3)%X(ICOL,IROW)=ICROSS%X(JCOL,JROW)
    IDF(4)%X(ICOL,IROW)=INFF
    IDF(6)%X(ICOL,IROW)=IDF(6)%X(ICOL,IROW)+1.0
   ENDIF
  ENDDO; ENDDO
 ENDIF

 END SUBROUTINE ISG2GRID_BATHEMETRY

 !###====================================================================
 SUBROUTINE ISG2GRID_EXTENT_WITH_WIDTH(NIDF,IDF)
 !###====================================================================
 USE IMOD_IDF_PAR
 USE IMOD_IDF, ONLY : IDFGETLOC
 IMPLICIT NONE
 REAL,PARAMETER :: MAXWIDTH=1000.0
 INTEGER,INTENT(IN) :: NIDF
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(NIDF) :: IDF
 INTEGER :: IROW,ICOL,N,NN,IR,IC,IRR,ICC,I,IRAT,IRAT1
 REAL :: W,L,X,Y,X1,Y1,F
 REAL,ALLOCATABLE,DIMENSION(:,:) :: MM
 REAL,DIMENSION(4) :: V
 logical :: pks7mpimasterwrite ! PKS
 
 W=0; DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  W=MAX(W,IDF(7)%X(ICOL,IROW))
 ENDDO; ENDDO
 N=(W/IDF(1)%DX)+2; ALLOCATE(MM(N,N)); NN=0
 
 IDF(8)%X=0.0
 DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  IF(IDF(1)%X(ICOL,IROW).NE.0.0)IDF(8)%X(ICOL,IROW)=1.0
  DO I=1,4; IF(IDF(I)%X(ICOL,IROW).EQ.IDF(I)%NODATA)IDF(I)%X(ICOL,IROW)=0.0; ENDDO
  W=IDF(7)%X(ICOL,IROW)
  L=IDF(5)%X(ICOL,IROW)
  !## conductance assume to be cell-filled; f=dx/length-segment*dx/width
  IF(W.GT.IDF(1)%DX)THEN
   IDF(1)%X(ICOL,IROW)=IDF(1)%X(ICOL,IROW)*IDF(5)%DX/L*IDF(5)%DX/W
  ENDIF
 ENDDO; ENDDO
 
 if (pks7mpimasterwrite()) WRITE(*,'(A)') 'Start computing erosion matrix' 
 DO IROW=1,IDF(1)%NROW
  DO ICOL=1,IDF(1)%NCOL
   !## river available
   IF(IDF(1)%X(ICOL,IROW).NE.0.0)THEN
    !## get mean width
    W=IDF(7)%X(ICOL,IROW)
    
    W=MIN(W,MAXWIDTH)
    
    !## current cellsize is less than current width of river
    IF(W.GT.IDF(1)%DX)THEN
     DO I=1,4; V(I)=IDF(I)%X(ICOL,IROW)/IDF(8)%X(ICOL,IROW); ENDDO
    
     !## create antialiased erosion/fattening matrix
     CALL IDFGETLOC(IDF(1),IROW,ICOL,X,Y)
     CALL ISG2GRID_EROSION_MATRIX(N,NN,MM,W,X,Y,IDF(1)%DX)
 
     !## apply multiplication matrix
     IF(IROW-NN/2.GE.1.AND.IROW+NN/2.LE.IDF(8)%NROW.AND. &
        ICOL-NN/2.GE.1.AND.ICOL+NN/2.LE.IDF(8)%NCOL)THEN
      IRR=IROW-NN/2
      DO IR=1,NN; ICC=ICOL-NN/2; DO IC=1,NN
       
       X1=IDF(8)%XMIN+(IDF(8)%DX*(ICC-1))+(0.5*IDF(8)%DX)
       IF(ICC.LT.ICOL)X1=IDF(8)%XMIN+(IDF(8)%DX*ICC-1) !## left  border
       IF(ICC.GT.ICOL)X1=IDF(8)%XMIN+ IDF(8)%DX*ICC    !## right border
       Y1=IDF(8)%YMAX-(IDF(8)%DY*(IRR+1))-(0.5*IDF(8)%DY)
       IF(IRR.LT.IROW)Y1=IDF(8)%YMAX-(IDF(8)%DY*IRR+1) !## top    border
       IF(IRR.GT.IROW)Y1=IDF(8)%YMAX- IDF(8)%DY*IRR    !## bottom border
       
       F=(X-X1)**2.0+(Y-Y1)**2.0
       IF(F.GT.0.0)F=SQRT(F)
       F=MIN(1.0,(W/2.0)/F)
       !## count for doublicates (assumption)
       F=F**2.0
       
       IDF(8)%X(ICC,IRR)=IDF(8)%X(ICC,IRR)+F*MM(IR,IC)
       DO I=1,4; IDF(I)%X(ICC,IRR)=IDF(I)%X(ICC,IRR)+V(I)*F*MM(IR,IC); ENDDO
       ICC=ICC+1
      ENDDO; IRR=IRR+1; ENDDO
     ELSE
      IRR=IROW-NN/2
      DO IR=1,NN; ICC=ICOL-NN/2; DO IC=1,NN
       IF(IRR.GE.1.AND.IRR.LE.IDF(8)%NROW.AND. &
          ICC.GE.1.AND.ICC.LE.IDF(8)%NCOL)THEN

        X1=IDF(8)%XMIN+(IDF(8)%DX*(ICC-1))+(0.5*IDF(8)%DX)
        IF(ICC.LT.ICOL)X1=IDF(8)%XMIN+(IDF(8)%DX*ICC-1) !## left  border
        IF(ICC.GT.ICOL)X1=IDF(8)%XMIN+ IDF(8)%DX*ICC    !## right border
        Y1=IDF(8)%YMAX-(IDF(8)%DY*(IRR+1))-(0.5*IDF(8)%DY)
        IF(IRR.LT.IROW)Y1=IDF(8)%YMAX-(IDF(8)%DY*IRR+1) !## top    border
        IF(IRR.GT.IROW)Y1=IDF(8)%YMAX- IDF(8)%DY*IRR    !## bottom border
       
        F=(X-X1)**2.0+(Y-Y1)**2.0
        IF(F.GT.0.0)F=SQRT(F)
        F=MIN(1.0,(W/2.0)/F)
        !## count for doublicates (assumption)
        F=F**2.0

        IDF(8)%X(ICC,IRR)=IDF(8)%X(ICC,IRR)+F*MM(IR,IC)
        DO I=1,4; IDF(I)%X(ICC,IRR)=IDF(I)%X(ICC,IRR)+V(I)*F*MM(IR,IC); ENDDO
       ENDIF
       ICC=ICC+1
      ENDDO; IRR=IRR+1; ENDDO
     ENDIF
     
    ENDIF
   ENDIF
  ENDDO
 ENDDO
 if (pks7mpimasterwrite()) WRITE(*,'(A)') 'Finished computing erosion matrix' 

 DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  DO I=1,4
   IF(IDF(8)%X(ICOL,IROW).GT.0.0)IDF(I)%X(ICOL,IROW)=IDF(I)%X(ICOL,IROW)/IDF(8)%X(ICOL,IROW)
   IF(IDF(8)%X(ICOL,IROW).EQ.0.0)IDF(I)%X(ICOL,IROW)=IDF(I)%NODATA
  ENDDO
 ENDDO; ENDDO

 !## correct to be sure multiplication matrix does not exceed factor 1.0 and multiply conductance with erosion matrix
 DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  IF(IDF(8)%X(ICOL,IROW).GT.1.0)IDF(8)%X(ICOL,IROW)=1.0
  IDF(1)%X(ICOL,IROW)=IDF(1)%X(ICOL,IROW)*IDF(8)%X(ICOL,IROW)
 ENDDO; ENDDO

 !## clean for conductances le zero
 DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  IF(IDF(1)%X(ICOL,IROW).LE.0.0)THEN
   DO I=1,4; IDF(I)%X(ICOL,IROW)=IDF(I)%NODATA; ENDDO
  ENDIF
 ENDDO; ENDDO

 DEALLOCATE(MM)
  
 END SUBROUTINE ISG2GRID_EXTENT_WITH_WIDTH
 
  !###====================================================================
 SUBROUTINE ISG2GRID_EROSION_MATRIX(N,NN,MM,W,X,Y,DX)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(INOUT) :: NN
 REAL,INTENT(IN) :: W,X,Y,DX
 REAL,DIMENSION(N,N),INTENT(INOUT) :: MM
 INTEGER :: NNN,I,IROW,ICOL
 REAL :: XMIN,YMAX,X1,X2,Y1,Y2,RADIUS,TRAD
 
 NNN=INT(W/DX)+1; IF(MOD(NNN,2).EQ.0)NNN=NNN+1
 IF(SUM(MM).NE.0.0.AND.NNN.EQ.NN)RETURN
 
 NN=NNN; MM=0.0; RADIUS=(REAL(NN)*DX)/2.0; TRAD=W/2.0 !W/2.0
 
 XMIN=X-RADIUS; YMAX=Y+RADIUS
 
 Y2=YMAX; Y1=YMAX-DX
 DO IROW=1,NNN
  X1=XMIN; X2=XMIN+DX
  DO ICOL=1,NNN 

   I=0;
   IF(SQRT((X1-X)**2.0+(Y2-Y)**2.0).LE.TRAD)I=I+1 !## top left
   IF(SQRT((X2-X)**2.0+(Y2-Y)**2.0).LE.TRAD)I=I+1 !## top right
   IF(SQRT((X2-X)**2.0+(Y1-Y)**2.0).LE.TRAD)I=I+1 !## bottom right
   IF(SQRT((X1-X)**2.0+(Y1-Y)**2.0).LE.TRAD)I=I+1 !## bottom left
   MM(ICOL,IROW)=REAL(I)/4.0
   X1=X1+DX; X2=X2+DX
  
  ENDDO
  Y2=Y2-DX; Y1=Y1-DX
 ENDDO
 
 END SUBROUTINE ISG2GRID_EROSION_MATRIX