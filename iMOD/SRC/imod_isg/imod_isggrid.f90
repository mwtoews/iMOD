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
!!
MODULE MOD_ISG_GRID

USE WINTERACTER
USE RESOURCE
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_INTERSECT, ONLY : INTERSECT_EQUI,INTERSECT_DEALLOCATE
USE MOD_INTERSECT_PAR, ONLY : XA,YA,LN,CA,RA
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFWRITE,IDFALLOCATEX,IDFDEALLOCATE,IDFREADDIM,IDFREAD,IDFNULLIFY,IDFIROWICOL,IDFPUTVAL,IDFGETXYVAL,IDFCOPY,IDFGETLOC, &
                    IDFREADSCALE,IDFGETAREA,IDF_EXTENT,IDFDEALLOCATEX
USE MODPLOT
USE MOD_ISG_PAR
USE MOD_PMANAGER_PAR, ONLY : SIM
USE MOD_UTL, ONLY : ITOS,RTOS,UTL_IDATETOJDATE,UTL_JDATETOIDATE,JDATETOGDATE,UTL_GETUNIT,UTL_WAITMESSAGE,UTL_IDFSNAPTOGRID,UTL_GETMED, &
               PEUCKER_SIMPLIFYLINE,UTL_DIST,UTL_GETCURRENTDATE
USE MOD_OSD, ONLY : OSD_OPEN,OSD_TIMER,ICF
USE MOD_IDFEDIT_TRACE, ONLY : IDFEDITTRACE
USE MOD_ISG_TRAPEZIUM, ONLY : ISGCOMPUTETRAPEZIUM
USE MOD_ISG_ADJ, ONLY : ISGADJUSTCOMPUTEXY
USE MOD_ISG_UTL, ONLY : UTL_GETUNITSISG,ISGREAD,ISGDEAL,ISGDELISD,ISGSTUWEN_INTERSECT,ISGGETPOSID,ISGMEMORYISC,ISGMEMORYDATISC,ISGDELISC,ISGGETXY, &
             ISGATTRIBUTESREADISCVALUE,ISGATTRIBUTES_2DCROSS_READ,ISGMEMORYDATISD,ISGSAVEHEADERS,ISGOPENFILES
USE MOD_POLYGON_UTL, ONLY : POLYGON1DEALLOCATE_SELIDF
USE MOD_POLYGON_PAR, ONLY : SELIDF
USE MOD_IPF_PAR, ONLY : NIPF,IPF
USE MOD_IPF, ONLY : IPFREAD2,IPFALLOCATE,IPFDEALLOCATE

CONTAINS

 !##=====================================================================
 SUBROUTINE ISG_EXPORT(ISGFILE,EXPORTFNAME,IEXPORT,IBATCH)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: ISGFILE
 CHARACTER(LEN=*),INTENT(IN) :: EXPORTFNAME
 INTEGER,INTENT(IN) :: IEXPORT,IBATCH
 INTEGER :: IU,NPNT,IPNT,NCLC,ICLC,J,ICRS,NCRS
 REAL :: DIST,TD

 !## read entire ISG file
 IF(ISGREAD((/ISGFILE/),IBATCH))THEN; ENDIF

 !## read associated textfile for selected location
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=EXPORTFNAME,STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE,DENYREAD',ACCESS='SEQUENTIAL')
 IF(IEXPORT.EQ.1)THEN
  WRITE(IU,'(A11,A51,4A11,A32)') 'ISEGMENT,','SEGNAME,','ICALC,','DISTANCE,','X,','Y,','CALC.NAME'
 ELSEIF(IEXPORT.EQ.2)THEN
  WRITE(IU,'(A11,A51,4A11,A32)') 'ISEGMENT,','SEGNAME,','ICROSS,','DISTANCE,','X,','Y,','CROSS.NAME'
 ENDIF

 !## proces each calculation point in the ISG
 DO ISELISG=1,NISG

  !## number of nodes on segment
  NPNT=ISG(ISELISG)%NSEG; IPNT=ISG(ISELISG)%ISEG

  IF(IEXPORT.EQ.1)THEN

   NCLC=ISG(ISELISG)%NCLC; ICLC=ISG(ISELISG)%ICLC

   DO J=1,NCLC

    DIST=ISD(ICLC+J-1)%DIST
    !## compute correct x/y coordinate of current computational node
    CALL ISGADJUSTCOMPUTEXY(IPNT,NPNT,DIST,TD)

    WRITE(IU,'(I10,A1,A50,A1,I10,3(A1,F10.2),A1,A32)') ISELISG,',',ADJUSTR(ISG(ISELISG)%SNAME),',',J,',', &
           DIST,',',ISGX,',',ISGY,',',ADJUSTR(ISD(ICLC+J-1)%CNAME)

   ENDDO

  ELSEIF(IEXPORT.EQ.2)THEN

   NCRS=ISG(ISELISG)%NCRS; ICRS=ISG(ISELISG)%ICRS

   DO J=1,NCRS

    DIST=ISC(ICRS+J-1)%DIST
    !## compute correct x/y coordinate of current computational node
    CALL ISGADJUSTCOMPUTEXY(IPNT,NPNT,DIST,TD)

    WRITE(IU,'(I10,A1,A50,A1,I10,3(A1,F10.2),A1,A32)') ISELISG,',',ADJUSTR(ISG(ISELISG)%SNAME),',',J,',', &
           DIST,',',ISGX,',',ISGY,',',ADJUSTR(ISC(ICRS+J-1)%CNAME)

   ENDDO
  ENDIF
 ENDDO

 END SUBROUTINE ISG_EXPORT

 !##=====================================================================
 SUBROUTINE ISG_ADDSTAGES(ISGFILE,IPFFILE,IBATCH)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: ISGFILE,IPFFILE
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: IPNT,NPNT,ICLC,NCLC,I,J,K,IPOS,ISEG,N,ID,NTXT,MTXT,IU,IREF
 INTEGER,ALLOCATABLE,DIMENSION(:) :: ISORT
 REAL :: MD,DIST,TD,DX,INFF,BTML,RESIS,STAGE
 CHARACTER(LEN=256) :: FNAME
 CHARACTER(LEN=14) :: CDATE
 TYPE FTXTOBJ
  INTEGER,POINTER,DIMENSION(:) :: IDATE
  INTEGER,POINTER,DIMENSION(:) :: ITYPE
  REAL,POINTER,DIMENSION(:) :: STAGE
  REAL,POINTER,DIMENSION(:) :: NODATA
  CHARACTER(LEN=52),POINTER,DIMENSION(:) :: LABEL
 END TYPE FTXTOBJ
 TYPE(FTXTOBJ) :: FTXT

 !## allocate memory for ipf-plotting, they will be read in memory and drawn from that
 NIPF=1; CALL IPFALLOCATE()

 IPF(1)%FNAME=IPFFILE
 IPF(1)%XCOL=1; IPF(1)%YCOL=2; IPF(1)%ZCOL=2; IPF(1)%Z2COL=2; IPF(1)%QCOL=2; IPF(1)%ITYPE=0
 IF(.NOT.IPFREAD2(1,1,1))RETURN

 !## read entire ISG file
 IF(ISGREAD((/ISGFILE/),IBATCH))THEN; ENDIF

 !## proces each calculation point in the ISG
 DO ISELISG=1,NISG

  !## number of nodes on segment
  NPNT=ISG(ISELISG)%NSEG; IPNT=ISG(ISELISG)%ISEG
  NCLC=ISG(ISELISG)%NCLC; ICLC=ISG(ISELISG)%ICLC

  DO J=1,NCLC

   DIST=ISD(ICLC+J-1)%DIST
   !## compute correct x/y coordinate of current computational node
   CALL ISGADJUSTCOMPUTEXY(IPNT,NPNT,DIST,TD)

   !## get closest point from ipf
   ID=0
   DO I=1,IPF(1)%NROW
    DX=SQRT((ISGX-IPF(1)%XYZ(1,I))**2.0+(ISGY-IPF(1)%XYZ(2,I))**2.0)
    IF(I.EQ.1)THEN; MD=DX; ID=I; ENDIF
    IF(DX.LT.MD)THEN
     MD=DX; ID=I
    ENDIF
   ENDDO

   WRITE(*,'(A,F10.2)') 'Location found for calculation node '//TRIM(ISD(ICLC+J-1)%CNAME)//';distance ',MD
   FNAME=IPFFILE(:INDEX(IPFFILE,'\',.TRUE.)-1)//'\'//TRIM(IPF(1)%INFO(IPF(1)%ACOL,ID))//'.txt'
   WRITE(*,'(A)') ' - Reading associated file: '//TRIM(FNAME)

   !## read associated textfile for selected location
   IU=UTL_GETUNIT()
   CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='FORMATTED',ACTION='READ,DENYWRITE',ACCESS='SEQUENTIAL')
   READ(IU,*) NTXT; READ(IU,*) MTXT

   !## get location of calculation node
   IPOS=ISG(ISELISG)%ICLC-1+J

   !## store total content
   K=NTXT+ISD(IPOS)%N
   ALLOCATE(FTXT%ITYPE(K),FTXT%IDATE(K),FTXT%STAGE(K),FTXT%LABEL(MTXT),FTXT%NODATA(MTXT),ISORT(K))

   DO I=1,MTXT; READ(IU,*) FTXT%LABEL(I),FTXT%NODATA(I); ENDDO
   K=0
   DO I=1,NTXT
    READ(IU,*) CDATE,STAGE
    IF(STAGE.NE.FTXT%NODATA(2))THEN
     K=K+1
     READ(CDATE,'(I8)') FTXT%IDATE(K)
     FTXT%STAGE(K)=STAGE
     FTXT%ITYPE(K)=1
    ENDIF
   ENDDO
   NTXT=K
   CLOSE(IU)

   !## get available date TO start insertion
   IREF=ISD(IPOS)%IREF-1
   DO I=1,ISD(IPOS)%N
    IREF=IREF+1
    K=K+1
    FTXT%IDATE(K)=DATISD(IREF)%IDATE
    FTXT%STAGE(K)=DATISD(IREF)%WLVL
    FTXT%ITYPE(K)=0
   ENDDO

   !## store latest information
   BTML =DATISD(IREF)%BTML
   RESIS=DATISD(IREF)%RESIS
   INFF =DATISD(IREF)%INFF

   !## sort date
   CALL WSORT(FTXT%IDATE,1,K,IORDER=ISORT)

   !## get number of unique dates
   N=0
   DO I=1,K
    !## for doubles only use itype.eq.1
    IF(I.LT.K)THEN
     IF(FTXT%IDATE(I+1).EQ.FTXT%IDATE(I))THEN
      IF(FTXT%ITYPE(ISORT(I)).EQ.0)CYCLE
     ENDIF
    ENDIF
    IF(I.GT.1)THEN
     IF(FTXT%IDATE(I-1).EQ.FTXT%IDATE(I))THEN
      IF(FTXT%ITYPE(ISORT(I)).EQ.0)CYCLE
     ENDIF
    ENDIF
    N=N+1
   ENDDO

   !## increase/decrease memory data calculation node - data will be replaced to the back
   CALL ISGMEMORYDATISD(N-ISD(IPOS)%N,IPOS,ISEG)

   !## rewrite data
   IREF=ISEG-1

   DO I=1,K

    !## for doubles only use itype.eq.1
    IF(I.LT.K)THEN
     IF(FTXT%IDATE(I+1).EQ.FTXT%IDATE(I))THEN
      IF(FTXT%ITYPE(ISORT(I)).EQ.0)CYCLE
     ENDIF
    ENDIF
    IF(I.GT.1)THEN
     IF(FTXT%IDATE(I-1).EQ.FTXT%IDATE(I))THEN
      IF(FTXT%ITYPE(ISORT(I)).EQ.0)CYCLE
     ENDIF
    ENDIF

    IREF=IREF+1
    DATISD(IREF)%IDATE=FTXT%IDATE(I)
    DATISD(IREF)%WLVL =FTXT%STAGE(ISORT(I))
    DATISD(IREF)%BTML =BTML
    DATISD(IREF)%RESIS=RESIS
    DATISD(IREF)%INFF =INFF
   ENDDO

   DEALLOCATE(FTXT%LABEL,FTXT%ITYPE,FTXT%NODATA,FTXT%STAGE,FTXT%IDATE,ISORT)

  ENDDO
 ENDDO

 CALL IPFDEALLOCATE()

 END SUBROUTINE ISG_ADDSTAGES

 !##=====================================================================
 SUBROUTINE ISG_ADDCROSSSECTION(ISGFILE,FNAME,WIDTHFNAME,MAXDIST,CROSS_PNTR, &
                                CROSS_BATH,CROSS_ZCHK,CROSS_CVAL,CELL_SIZE,IBATCH,ICLEAN)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: ISGFILE,FNAME,WIDTHFNAME,CROSS_PNTR,CROSS_BATH,CROSS_ZCHK,CROSS_CVAL
 REAL,INTENT(IN) :: MAXDIST,CELL_SIZE
 INTEGER,INTENT(IN) :: IBATCH,ICLEAN
 INTEGER :: IU,IOS,N,I,J,IISG,IPOS,ISEG,NSEG,ICRS,NCRS,IPNT,NPNT,IROW,ICOL,IP,ZCHK,CVAL
 INTEGER(KIND=1) :: CF
 CHARACTER(LEN=5000) :: STRING
 REAL,DIMENSION(1000) :: X,Y
 REAL :: XCRD,YCRD,TDIST,DIST,W,TD,XC,YC
 CHARACTER(LEN=MAXLEN) :: LABEL
 TYPE(IDFOBJ) :: IDF
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: ICROSS

 !## read entire ISG file
 IF(ISGREAD((/ISGFILE/),IBATCH))THEN; ENDIF

 IF(CROSS_PNTR.EQ.'')THEN

  IF(TRIM(WIDTHFNAME).NE.'')THEN
   IF(.NOT.IDFREAD(IDF,WIDTHFNAME,0))STOP 'Cannot read width fname'
  ENDIF

  !## remove all cross-sections on segment
  IF(ICLEAN.EQ.1)THEN
   WRITE(*,'(/A/)') 'Removing all cross-sections ...'
   DO ISELISG=1,NISG
    ICRS=ISG(ISELISG)%ICRS; NCRS=ISG(ISELISG)%NCRS
    DO I=1,NCRS; CALL ISGDELISC(ISELISG,ICRS); END DO
   ENDDO
   ISC%N=0; ISC%IREF=0; ISC%DIST=0.0; ISC%CNAME=''
  ENDIF
 
  WRITE(*,'(/A/)') 'Adding cross-sections from '//TRIM(FNAME)

  IU=UTL_GETUNIT()
  CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ')
  DO
  
   READ(IU,'(A5000)',IOSTAT=IOS) STRING; IF(IOS.NE.0)EXIT; IF(LEN_TRIM(STRING).EQ.0)EXIT
   IF(STRING(LEN(STRING):LEN(STRING)).NE.'')THEN
    WRITE(*,'(A)') 'Reduce number of cross-section definitions to be less than 5000 characters'
   ENDIF
   N=1; DO
    READ(STRING,*,IOSTAT=IOS) XCRD,YCRD,LABEL,(X(I),I=1,N),(Y(I),I=1,N); IF(IOS.NE.0)EXIT; N=N+1
   ENDDO; N=N-1
   !## not enough points
   IF(N.LE.2)THEN
!    WRITE(*,*) XCRD,YCRD,TRIM(LABEL)
!    DO I=1,N/2; WRITE(*,*) X(I),X(I+N/2); ENDDO !; STOP 'N.LE.4'
    CYCLE
   ENDIF

   !## try to simplify the cross-section
!   IF(ISIMPLE.GT.0)THEN
!    CALL ISGCOMPUTEEIGHTPOINTS(X,Y,N,XSIMPLE,YSIMPLE,NSIMPLE,AORG,ASIMPLE)
!   ENDIF

!   WRITE(*,*) XCRD,YCRD,TRIM(LABEL)
!   DO I=1,N/2; WRITE(*,*) X(I),X(I+N/2); ENDDO

   !## find distance of point on segment iisg that is closest
   CALL ISGSTUWEN_INTERSECT(MAXDIST,XCRD,YCRD,IISG,TDIST)

   !## segment found within maxdist
   IF(TDIST.GT.0.0)THEN

    !## remove cross-sections if exists on current segment
    IF(ICLEAN.EQ.2)THEN
     ICRS=ISG(IISG)%ICRS; NCRS=ISG(IISG)%NCRS
     DO I=1,NCRS; CALL ISGDELISC(IISG,ICRS); END DO
    ENDIF

    ISELISG=IISG

!    N=N/2

    !## increase memory location cross-section
    CALL ISGMEMORYISC(1,ISELISG,IPOS)
    ISC(IPOS)%N    =0
    ISC(IPOS)%IREF =NDISC+1
    ISC(IPOS)%DIST =TDIST
    ISC(IPOS)%CNAME=TRIM(LABEL)

    !## increase memory data cross-section
    CALL ISGMEMORYDATISC(N,IPOS,ISEG)
    DO I=1,N
     DATISC(ISEG+I-1)%DISTANCE=X(I)
     DATISC(ISEG+I-1)%BOTTOM  =Y(I) !X(I+N)
     DATISC(ISEG+I-1)%MRC     =0.03
    ENDDO

   ENDIF
  ENDDO

  CLOSE(IU)

  IF(TRIM(WIDTHFNAME).NE.'')THEN
   WRITE(*,'(/A/)') 'Adding default cross-sections for remaining segments'

   !## put cross-sections on segment that did not got a cross-section
   DO ISELISG=1,NISG

    ICRS=ISG(ISELISG)%ICRS; NCRS=ISG(ISELISG)%NCRS
    IF(NCRS.EQ.0)THEN

     !## get distance of segment, put cross-section in mid
     ISEG =ISG(ISELISG)%ISEG; NSEG =ISG(ISELISG)%NSEG
     TDIST=0.0
     DO I=2,NSEG
      ISEG=ISEG+1; DIST=(ISP(ISEG)%X-ISP(ISEG-1)%X)**2.0+(ISP(ISEG)%Y-ISP(ISEG-1)%Y)**2.0
      IF(DIST.GT.0.0)DIST=SQRT(DIST); TDIST=TDIST+DIST
     END DO
     TDIST=TDIST/2.0

     !## get x,y coordinates for current cross-section location
     ISEG=ISG(ISELISG)%ISEG
     CALL ISGGETXY(ISP(ISEG:)%X,ISP(ISEG:)%Y,NSEG,TDIST,XCRD,YCRD)
     W=ABS(IDFGETXYVAL(IDF,XCRD,YCRD))

     N=4
     !## increase memory
     CALL ISGMEMORYISC(1,ISELISG,IPOS)
     ISC(IPOS)%N    =0
     ISC(IPOS)%IREF =NDISC+1
     ISC(IPOS)%DIST =TDIST
     ISC(IPOS)%CNAME='Cross-Section '//TRIM(ITOS(ISELISG))
     CALL ISGMEMORYDATISC(N,IPOS,ISEG)
     DATISC(ISEG)%DISTANCE  =-W/2.0
     DATISC(ISEG)%BOTTOM    = 5.0
     DATISC(ISEG)%MRC        =0.03
     DATISC(ISEG+1)%DISTANCE=-W/2.0
     DATISC(ISEG+1)%BOTTOM  = 0.0
     DATISC(ISEG+1)%MRC      =0.03
     DATISC(ISEG+2)%DISTANCE= W/2.0
     DATISC(ISEG+2)%BOTTOM  = 0.0
     DATISC(ISEG+2)%MRC      =0.03
     DATISC(ISEG+3)%DISTANCE= W/2.0
     DATISC(ISEG+3)%BOTTOM  = 5.0
     DATISC(ISEG+3)%MRC      =0.03
    ENDIF
   ENDDO
  ENDIF
  
 ELSE

  ALLOCATE(ICROSS(5)); DO I=1,SIZE(ICROSS); CALL IDFNULLIFY(ICROSS(I)); ENDDO

  !## select finest resolution
  IF(.NOT.IDFREAD(ICROSS(1),CROSS_PNTR,0))RETURN
  IF(.NOT.IDFREAD(ICROSS(2),CROSS_BATH,0))RETURN
  CLOSE(ICROSS(1)%IU); CLOSE(ICROSS(2)%IU)

  !## read additional reference heights
  ZCHK=0; IF(TRIM(CROSS_ZCHK).NE.'')THEN
   ZCHK=1; IF(.NOT.IDFREAD(ICROSS(3),CROSS_ZCHK,0))RETURN
   CLOSE(ICROSS(3)%IU)
  ENDIF
  !## read additional resistances
  CVAL=0; IF(TRIM(CROSS_CVAL).NE.'')THEN
   CVAL=1; IF(.NOT.IDFREAD(ICROSS(4),CROSS_CVAL,0))RETURN
   CLOSE(ICROSS(4)%IU)
  ENDIF

  IF(.NOT.IDF_EXTENT(2+ZCHK+CVAL,ICROSS,ICROSS(5),2))RETURN

  IF(CELL_SIZE.NE.0.0)THEN
   ICROSS(5)%DX=CELL_SIZE; ICROSS(5)%DY=ICROSS(5)%DX
   CALL UTL_IDFSNAPTOGRID(ICROSS(5)%XMIN,ICROSS(5)%XMAX,ICROSS(5)%YMIN,ICROSS(5)%YMAX,ICROSS(5)%DX,ICROSS(5)%NCOL,ICROSS(5)%NROW)
  ENDIF

  !## read pointer
  CALL IDFCOPY(ICROSS(5),ICROSS(1))
  IF(.NOT.IDFREADSCALE(CROSS_PNTR,ICROSS(1),7,0,0.0,0))RETURN !## most frequent occurence
  !## read zval at pointer scale
  CALL IDFCOPY(ICROSS(5),ICROSS(2))
  IF(.NOT.IDFREADSCALE(CROSS_BATH,ICROSS(2),2,1,0.0,0))RETURN !## average value
  IF(ZCHK.EQ.1)THEN
   !## read zchk values
   CALL IDFCOPY(ICROSS(5),ICROSS(3))
   IF(.NOT.IDFREADSCALE(CROSS_ZCHK,ICROSS(3),2,1,0.0,0))RETURN !## average value
  ENDIF
  IF(CVAL.EQ.1)THEN
   !## read cval values
   CALL IDFCOPY(ICROSS(5),ICROSS(4))
   IF(.NOT.IDFREADSCALE(CROSS_CVAL,ICROSS(4),2,1,0.0,0))RETURN !## average value
  ENDIF

  DO ISELISG=1,NISG

   !## number of nodes on segment
   NPNT=ISG(ISELISG)%NSEG; IPNT=ISG(ISELISG)%ISEG
   ICRS=ISG(ISELISG)%ICRS; NCRS=ISG(ISELISG)%NCRS

   WRITE(6,'(2(A,I10),A)') '+Busy with segment ',ISELISG,' adding ',NCRS,' cross-sections'

   DO J=1,NCRS

    DIST=ISC(ICRS+J-1)%DIST
    !## compute correct x/y coordinate of current cross-section
    CALL ISGADJUSTCOMPUTEXY(IPNT,NPNT,DIST,TD)
    !## get irow/icol for current location of cross-section
    CALL IDFIROWICOL(ICROSS(1),IROW,ICOL,ISGX,ISGY)
    IF(IROW.NE.0.AND.ICOL.NE.0)THEN
     IP=ICROSS(1)%X(ICOL,IROW)
     IF(IP.NE.ICROSS(1)%NODATA.AND.ABS(IP).GT.0)THEN

      !## get number of cross-section locations
      N=0; DO IROW=1,ICROSS(1)%NROW; DO ICOL=1,ICROSS(1)%NCOL
       !## location of gridcell equal to pointer value at location of cross-section
       IF(ABS(ICROSS(1)%X(ICOL,IROW)).EQ.ABS(IP))THEN
        IF(ICROSS(2)%X(ICOL,IROW).NE.ICROSS(2)%NODATA)N=N+1
       ENDIF
      ENDDO; ENDDO

      !## add extra record to store dx,dy
      N=N+1

!      !## increase memory location cross-section
!      CALL ISGMEMORYISC(1,ISELISG,IPOS)

      !## get location of cross-sections
      IPOS=ISG(ISELISG)%ICRS-1+J
      !## increase/decrease memory data cross-section
      N=N-ABS(ISC(IPOS)%N)
      CALL ISGMEMORYDATISC(N,IPOS,ISEG)
      ISC(IPOS)%N=-1.0*ABS(ISC(IPOS)%N)

      IF(ZCHK.EQ.0)THEN
       DATISC(ISEG)%DISTANCE= ICROSS(1)%DX
       DATISC(ISEG)%BOTTOM  = ICROSS(1)%DY
       DATISC(ISEG)%MRC      = 0.0 !## empty, not to be used (yet)
      ELSE
       DATISC(ISEG)%DISTANCE=-ICROSS(1)%DX
       DATISC(ISEG)%BOTTOM  =-ICROSS(1)%DY
      ENDIF

      N=0
      DO IROW=1,ICROSS(1)%NROW; DO ICOL=1,ICROSS(1)%NCOL
       !## location of gridcell equal to pointer value at location of cross-section
       IF(ABS(ICROSS(1)%X(ICOL,IROW)).EQ.ABS(IP))THEN
        IF(ICROSS(2)%X(ICOL,IROW).NE.ICROSS(2)%NODATA)THEN
         N=N+1
         CALL IDFGETLOC(ICROSS(1),IROW,ICOL,XC,YC)
         DATISC(ISEG+N)%DISTANCE=XC
         DATISC(ISEG+N)%BOTTOM  =YC
         DATISC(ISEG+N)%MRC      =ICROSS(2)%X(ICOL,IROW)
         IF(ZCHK.EQ.1)THEN
          !## store z-threshold
          IF(N.EQ.1)DATISC(ISEG)%MRC=ICROSS(3)%X(ICOL,IROW) !## threshold
          CF=INT(1,1)
          IF(CVAL.EQ.1)THEN
           IF(INT(ICROSS(4)%X(ICOL,IROW)).LE.HUGE(CF))CF=INT(ICROSS(4)%X(ICOL,IROW))
          ENDIF
          IF(ICROSS(1)%X(ICOL,IROW).GT.0)DATISC(ISEG+N)%ZP=INT( CF,1)
          IF(ICROSS(1)%X(ICOL,IROW).LT.0)DATISC(ISEG+N)%ZP=INT(-CF,1)
         ENDIF
        ENDIF
       ENDIF
      ENDDO; ENDDO

     ELSE
      WRITE(*,'(/A,I10)') 'Pointer value is equal to nodata value/or le 0, IP=',IP
      WRITE(*,'(2(A,I10)/)') 'For cross-section ',J,' on segment ',ISELISG
     ENDIF
    ELSE
     WRITE(*,'(/2(A,I10),A/)') 'Position of cross-section ',J,' on segment ',ISELISG,' is outside Pointer IDF'
    ENDIF
   ENDDO
  ENDDO
  CALL IDFDEALLOCATE(ICROSS,SIZE(ICROSS)); DEALLOCATE(ICROSS)
 ENDIF

 END SUBROUTINE ISG_ADDCROSSSECTION

 !##=====================================================================
 SUBROUTINE ISG_SIMPLIFYMAIN(ISGFILE,ZTOLERANCE,NODATA,IBATCH)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: ISGFILE
 REAL,INTENT(IN) :: ZTOLERANCE,NODATA
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: I,J,K,ITYPE,ISS,IREF,NCLC,IAVERAGE
 REAL,DIMENSION(4) :: RVAL,XNR
 REAL,DIMENSION(1,4) :: QSORT,NDATA
 REAL,ALLOCATABLE,DIMENSION(:) :: ZDIST,XDIST,GCODE
 INTEGER,ALLOCATABLE,DIMENSION(:) :: ILIST

 !## read entire ISG file
 IF(ISGREAD((/ISGFILE/),IBATCH))THEN; ENDIF

 ITYPE=1 !## itype=1: isd
 ISS=1   !## mean value
 NCLC=MAXVAL(ISG%NCLC); ALLOCATE(ZDIST(NCLC),XDIST(NCLC),GCODE(NCLC),ILIST(NCLC))

 NDATA=NODATA
 !## arithmetic mean
 IAVERAGE=1
 
 !## process each segment in ISG file to simplify calculation points
 DO I=1,NISG

  !## get mean waterlevels in segment
  IREF=ISG(I)%ICLC-1; NCLC=ISG(I)%NCLC
  DO J=1,NCLC
   IREF=IREF+1
   CALL ISG2GRIDGETDATA(0,0,1,QSORT,XNR,4,RVAL,ISD(IREF)%N,ISD(IREF)%IREF,ISS,ITYPE,NDATA,IAVERAGE)
   XDIST(J)=ISD(IREF)%DIST; ZDIST(J)=RVAL(1)
   IF(SUM(XNR)/4.NE.XNR(1))ZDIST(J)=NODATA
  ENDDO

  !## remove nodata --- interpolate
  ILIST=0; K=0; DO J=1,NCLC
   IF(ZDIST(J).NE.NODATA)THEN
    K=K+1; ILIST(J)=K; ZDIST(K)=ZDIST(J); XDIST(K)=XDIST(J)
   ENDIF
  ENDDO
  !## process line
  CALL PEUCKER_SIMPLIFYLINE(XDIST,ZDIST,GCODE,K)
  !## reset GCODE
  DO J=NCLC,1,-1
   IF(ILIST(J).NE.0)THEN
    GCODE(J)=ABS(GCODE(ILIST(J)))
   ELSE
    GCODE(J)=0.0
   ENDIF
  ENDDO

  IREF=ISG(I)%ICLC-1

  !## never remove the first or last
  GCODE(1)   =ZTOLERANCE+1.0
  GCODE(NCLC)=ZTOLERANCE+1.0

  K=1
  DO J=2,NCLC
   K=K+1
   !## remove point from Urs-Douglas-Peucker algorithm (less then given tolerance)
   IF(GCODE(J).LT.ZTOLERANCE)THEN
    CALL ISGDELISD(I,IREF+K)
    !## reset pointer one backwards
    K=K-1
   ENDIF
  ENDDO

  WRITE(*,'(A,F10.2,A)') 'Progress ',REAL(I*100)/REAL(NISG),'%'

 ENDDO

 DEALLOCATE(ZDIST,XDIST,GCODE,ILIST)

 END SUBROUTINE ISG_SIMPLIFYMAIN

 !###====================================================================
 LOGICAL FUNCTION ISG2GRIDMAIN(ISGFILE,IBATCH,NLAY,TOP,BOT,GRIDISG) !ISTEADY,SDATE,EDATE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH,NLAY !,ISTEADY,SDATE,EDATE
 TYPE(GRIDISGOBJ),INTENT(INOUT) :: GRIDISG
 CHARACTER(LEN=*),INTENT(IN) :: ISGFILE
 TYPE(IDFOBJ),DIMENSION(NLAY),INTENT(INOUT) :: TOP,BOT
 INTEGER :: NROW,NCOL,SIMDATE1,SIMDATE2,SIMDATE3,SDATE,EDATE
 INTEGER,DIMENSION(1) :: MP
 CHARACTER(LEN=52) :: PPOSTFIX

 ISG2GRIDMAIN=.FALSE.

 IF(IBATCH.EQ.1)THEN
  !## deallocate memory
  IF(ISGREAD((/ISGFILE/),IBATCH))THEN; ENDIF
 ENDIF

 CALL ISG2GRIDGETDIMENSION(GRIDISG%IDIM,GRIDISG%XMIN,GRIDISG%YMIN,GRIDISG%XMAX,GRIDISG%YMAX,NROW,NCOL,GRIDISG%CS)
 CALL UTL_IDFSNAPTOGRID(GRIDISG%XMIN,GRIDISG%XMAX,GRIDISG%YMIN,GRIDISG%YMAX,GRIDISG%CS,NCOL,NROW)

 SIMDATE1=UTL_IDATETOJDATE(GRIDISG%SDATE) !## begin juliandate
 SIMDATE3=UTL_IDATETOJDATE(GRIDISG%EDATE) !## eind  juliandate
 DO

  IF(GRIDISG%DDATE.EQ.0)THEN
   SIMDATE2=SIMDATE3
  ELSE
   SIMDATE2=SIMDATE1+MAX(0,GRIDISG%DDATE-1)
  ENDIF
  IF(SIMDATE2.GT.SIMDATE3.AND.GRIDISG%ISTEADY.EQ.2)EXIT

  GRIDISG%SDATE=UTL_JDATETOIDATE(SIMDATE1)
  GRIDISG%EDATE=UTL_JDATETOIDATE(SIMDATE2)

  IF(GRIDISG%DDATE.EQ.0)PPOSTFIX=GRIDISG%POSTFIX
  IF(GRIDISG%DDATE.NE.0)PPOSTFIX=TRIM(GRIDISG%POSTFIX)//'_'//TRIM(ITOS(GRIDISG%SDATE))

  IF(IBATCH.EQ.1.AND.GRIDISG%DDATE.NE.0)WRITE(*,'(/A,I8,A,I8,A/)') 'Gridding between ',GRIDISG%SDATE,' and ',GRIDISG%EDATE,' using ppostfix:'//TRIM(PPOSTFIX)
  IF(GRIDISG%ISTEADY.EQ.2.AND.GRIDISG%EDATE.LT.GRIDISG%SDATE)THEN
   IF(IBATCH.EQ.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Enddate '//TRIM(ITOS(GRIDISG%EDATE))//' is less than startdate '// &
      TRIM(ITOS(GRIDISG%SDATE)),'Error')
    RETURN
   ELSE
    STOP 'Enddate if less than start date'
   ENDIF
  ENDIF
  ISG2GRIDMAIN=ISG2GRID(PPOSTFIX,NROW,NCOL,NLAY,0,TOP,BOT,IBATCH,MP,0,GRIDISG) !,ISTEADY,SDATE,EDATE)
  IF(.NOT.ISG2GRIDMAIN)RETURN
  IF(GRIDISG%ISIMGRO.EQ.1)CALL ISG2GRIDMAIN_SVAT(GRIDISG)

  !## no multiply griddings
  IF(GRIDISG%DDATE.EQ.0)EXIT
  SIMDATE1=SIMDATE2+1
 END DO

 END FUNCTION ISG2GRIDMAIN

 !###====================================================================
 SUBROUTINE ISG2GRIDMAIN_SVAT(GRIDISG)
 !###====================================================================
 IMPLICIT NONE
 TYPE(GRIDISGOBJ),INTENT(INOUT) :: GRIDISG
 TYPE(IDFOBJ) :: AHN,THIESSEN
 TYPE TRAPTYPE
  INTEGER :: IROW,ICOL
  REAL :: X,Y
  REAL :: LN,BH,BW,CT,COND_IN,COND_OUT
  CHARACTER(LEN=30) :: CSEGMENT
 END TYPE TRAPTYPE
 TYPE(TRAPTYPE),DIMENSION(2,20) :: TRAP
 INTEGER,DIMENSION(2) :: NETTRAP
 INTEGER :: IU,JU,ISWNR,I,J,IOS,ITRAP,ISVAT
 LOGICAL :: LEX
 REAL :: MV

 !## reading ahn
 WRITE(*,'(A)') 'Opening '//TRIM(GRIDISG%AHNFNAME)//' ...'
 CALL IDFNULLIFY(AHN); IF(.NOT.IDFREAD(AHN,GRIDISG%AHNFNAME,0))THEN; ENDIF
 WRITE(*,'(A)') 'Opening '//TRIM(GRIDISG%THIESSENFNAME)//' ...'
 CALL IDFNULLIFY(THIESSEN); IF(.NOT.IDFREAD(THIESSEN,GRIDISG%THIESSENFNAME,0))THEN; ENDIF
 CALL ISG2GRID_SEGREAD(GRIDISG)

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(GRIDISG%ROOT)//'\'//TRIM(GRIDISG%SVAT2SWNR_DRNG),STATUS='OLD',FORM='FORMATTED', &
               ACTION='READ,DENYWRITE',ACCESS='SEQUENTIAL')
 JU=UTL_GETUNIT()
 CALL OSD_OPEN(JU,FILE=TRIM(GRIDISG%ROOT)//'\'//GRIDISG%SVAT2SWNR_DRNG(:INDEX(GRIDISG%SVAT2SWNR_DRNG,'.',.TRUE.))//'_tmp', &
               STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE,DENYREAD',ACCESS='SEQUENTIAL')

 ISWNR=0
 DO J=1,GRIDISG%NSVATS

  READ(IU,'(I10)',IOSTAT=IOS) NETTRAP(2)
  IF(IOS.NE.0)THEN; WRITE(*,*) TRIM(GRIDISG%SVAT2SWNR_DRNG)//' file does contain probably variable nettrap'; STOP; ENDIF

  DO ITRAP=1,NETTRAP(2)
   READ(IU,'(2F10.0,2I10,6F10.0,A30)',IOSTAT=IOS) TRAP(2,ITRAP)%X,TRAP(2,ITRAP)%Y,TRAP(2,ITRAP)%IROW,TRAP(2,ITRAP)%ICOL, &
                                           TRAP(2,ITRAP)%LN,TRAP(2,ITRAP)%BH,TRAP(2,ITRAP)%BW,TRAP(2,ITRAP)%CT, &
                                           TRAP(2,ITRAP)%COND_IN,TRAP(2,ITRAP)%COND_OUT,TRAP(2,ITRAP)%CSEGMENT
   IF(IOS.NE.0)THEN; WRITE(*,*) TRIM(GRIDISG%SVAT2SWNR_DRNG)//' file does contain probably ***"s'; STOP; ENDIF
   CALL IUPPERCASE(TRAP(2,ITRAP)%CSEGMENT)
   TRAP(2,ITRAP)%CSEGMENT=ADJUSTL(TRAP(2,ITRAP)%CSEGMENT)
  END DO

  IF(J.GT.1)THEN

   LEX=.FALSE.
   IF(NETTRAP(2).EQ.NETTRAP(1))THEN
    LEX=.TRUE.
    DO ITRAP=1,NETTRAP(1)
     IF(TRAP(1,ITRAP)%IROW.NE.TRAP(2,ITRAP)%IROW)        LEX=.FALSE.
     IF(TRAP(1,ITRAP)%ICOL.NE.TRAP(2,ITRAP)%ICOL)        LEX=.FALSE.
     IF(TRAP(1,ITRAP)%BH.NE.TRAP(2,ITRAP)%BH)            LEX=.FALSE.
     IF(TRAP(1,ITRAP)%BW.NE.TRAP(2,ITRAP)%BW)            LEX=.FALSE.
     IF(TRAP(1,ITRAP)%CSEGMENT.NE.TRAP(2,ITRAP)%CSEGMENT)LEX=.FALSE.
    END DO
    IF(LEX)THEN
     TRAP(1,1:NETTRAP(1))%LN      =TRAP(1,1:NETTRAP(1))%LN      +TRAP(2,1:NETTRAP(1))%LN
     TRAP(1,1:NETTRAP(1))%COND_IN =TRAP(1,1:NETTRAP(1))%COND_IN +TRAP(2,1:NETTRAP(1))%COND_IN
     TRAP(1,1:NETTRAP(1))%COND_OUT=TRAP(1,1:NETTRAP(1))%COND_OUT+TRAP(2,1:NETTRAP(1))%COND_OUT
    ENDIF
   ENDIF

   IF(.NOT.LEX)THEN

    I=0
    DO
     ISWNR=ISWNR+1
     IF(ISWNR.GT.GRIDISG%NSWNR)THEN
      ISWNR=1
      I    =I+1
     ENDIF
     IF(TRIM(GRIDISG%CSOBEK(ISWNR)).EQ.TRIM(TRAP(1,1)%CSEGMENT))EXIT
     IF(I.GT.2)EXIT
    END DO

    IF(I.GT.2)THEN
     WRITE(*,'(A)') 'Cannot find label '//TRIM(TRAP(1,1)%CSEGMENT)//' in '//TRIM(GRIDISG%SEGMENTCSVFNAME)
    ELSE
     DO ITRAP=1,NETTRAP(1)

      !## determine svat unit
      ISVAT=INT(IDFGETXYVAL(THIESSEN,TRAP(1,ITRAP)%X,TRAP(1,ITRAP)%Y))

      !## determine surface level
      MV=IDFGETXYVAL(AHN,TRAP(1,ITRAP)%X,TRAP(1,ITRAP)%Y)

      !## translate m2/day -> day
      TRAP(1,ITRAP)%COND_IN  =1.0/(TRAP(1,ITRAP)%COND_IN/(GRIDISG%CS**2.0))
      TRAP(1,ITRAP)%COND_OUT =1.0/(TRAP(1,ITRAP)%COND_OUT/(GRIDISG%CS**2.0))
      TRAP(1,ITRAP)%COND_IN  =MIN(TRAP(1,ITRAP)%COND_IN,99999.99)
      TRAP(1,ITRAP)%COND_OUT =MIN(TRAP(1,ITRAP)%COND_OUT,99999.99)

      WRITE(JU,'(I10,I6,3F8.2,8X,2F8.2,8X,F8.2,8X,I10,8X,A30)') ISVAT,GRIDISG%SYSID,MV-TRAP(1,ITRAP)%BH,TRAP(1,ITRAP)%BW,TRAP(1,ITRAP)%CT, &
                     TRAP(1,ITRAP)%LN,TRAP(1,ITRAP)%COND_IN,TRAP(1,ITRAP)%COND_OUT,ISWNR,TRAP(1,ITRAP)%CSEGMENT
     ENDDO

    ENDIF

    !## big change that next will be neighbourhood
    ISWNR=ISWNR-1

    TRAP(1,:) =TRAP(2,:)
    NETTRAP(1)=NETTRAP(2)

   ENDIF

  ELSE

   NETTRAP(1)=NETTRAP(2)
   TRAP(1,:) =TRAP(2,:)

  ENDIF
 ENDDO

 CLOSE(IU,STATUS='DELETE'); CLOSE(JU)

 CALL IOSRENAMEFILE(TRIM(GRIDISG%ROOT)//'\'//GRIDISG%SVAT2SWNR_DRNG(:INDEX(GRIDISG%SVAT2SWNR_DRNG,'.',.TRUE.))//'_tmp', &
                    TRIM(GRIDISG%ROOT)//'\'//TRIM(GRIDISG%SVAT2SWNR_DRNG))

 END SUBROUTINE ISG2GRIDMAIN_SVAT

 !###======================================================================
 SUBROUTINE ISG2GRID_SEGREAD(GRIDISG)
 !###======================================================================
 IMPLICIT NONE
 TYPE(GRIDISGOBJ),INTENT(INOUT) :: GRIDISG
 INTEGER :: IU,I,IOS

 IU=UTL_GETUNIT(); OPEN(IU,FILE=GRIDISG%SEGMENTCSVFNAME,STATUS='OLD',ACTION='READ')

 READ(IU,*); GRIDISG%NSWNR=0; DO; GRIDISG%NSWNR=GRIDISG%NSWNR+1; READ(IU,*,IOSTAT=IOS); IF(IOS.NE.0)EXIT; ENDDO

 ALLOCATE(GRIDISG%CSOBEK(GRIDISG%NSWNR),GRIDISG%SWNR(GRIDISG%NSWNR))
 REWIND(IU); READ(IU,*)
 DO I=1,GRIDISG%NSWNR
  READ(IU,*,IOSTAT=IOS) GRIDISG%SWNR(I),GRIDISG%CSOBEK(I)
  CALL IUPPERCASE(GRIDISG%CSOBEK(I)); GRIDISG%CSOBEK(I)=ADJUSTL(GRIDISG%CSOBEK(I))
 ENDDO

 CLOSE(IU)

 END SUBROUTINE ISG2GRID_SEGREAD

 !###====================================================================
 LOGICAL FUNCTION ISG2GRID(PPOSTFIX,NROW,NCOL,NLAY,ILAY,TOP,BOT,IBATCH, &
                           MP,JU,GRIDISG) 
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL,NLAY,ILAY,IBATCH,JU
 INTEGER,INTENT(INOUT),DIMENSION(:) :: MP
 TYPE(IDFOBJ),DIMENSION(NLAY),INTENT(INOUT) :: TOP,BOT
 TYPE(GRIDISGOBJ),INTENT(INOUT) :: GRIDISG
 CHARACTER(LEN=*),INTENT(IN) :: PPOSTFIX
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: I,J,K,II,JJ,TTIME,IROW,ICOL,NETTRAP,ITYPE,N,ISTW,IR,IC,MDIM
 INTEGER :: JCRS,MAXNSEG,IRAT,IRAT1
 REAL :: C,INFF,DXY,RWIDTH,WETPER,ISGLEN,AORG,ATRAP,XSTW,YSTW,GSTW,ZCHK
 REAL,ALLOCATABLE,DIMENSION(:,:) :: QSORT,RVAL
 REAL,ALLOCATABLE,DIMENSION(:) :: DIST,XNR,NDATA
 REAL,ALLOCATABLE,DIMENSION(:) :: X,Y
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IPOS
 REAL,ALLOCATABLE,DIMENSION(:) :: XIN,YIN,XSYM,YSYM
 REAL,ALLOCATABLE,DIMENSION(:,:) :: XTRAP,YTRAP
 INTEGER :: ISEG,JSEG,NSEG,IREF,MAXDIM,NDIM,NSYM,NTRAP,ITRAP,JQHR,NITEMS,LU,IOS,ICRS,NCRS,IUSIMGRO
 REAL :: X1,X2,Y1,Y2,D,FCT,CT,BH,BW,NETWD,COND,H1,H2,Z,WL,TD,TC,XC,YC
 LOGICAL :: LEX,LNODAT
 CHARACTER(LEN=256) :: LINE,TMPFNAME
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 TYPE(IDFOBJ) :: ICROSS,PCROSS
 REAL,PARAMETER :: NODATAIDF=0.0 !## do not change !!!

 ISG2GRID=.FALSE.
 GRIDISG%NSVATS=0

 IF(IBATCH.EQ.0)THEN
  CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(2,''); CALL WINDOWOUTSTATUSBAR(4,'Initialisation 0%')
 ENDIF

 IF(GRIDISG%ISIMGRO.EQ.1)THEN
  CALL OSD_OPEN(IUSIMGRO,FILE=TRIM(GRIDISG%ROOT)//'\'//TRIM(GRIDISG%SVAT2SWNR_DRNG),STATUS='UNKNOWN', &
                FORM='FORMATTED',ACTION='WRITE,DENYREAD',ACCESS='SEQUENTIAL')
 ENDIF

 !## compute structure influences between sdate and edate
 IF(GRIDISG%ICDIST.EQ.1)DATISD%WL_STW=DATISD%WLVL

 NITEMS=MAXITEMS; IF(GRIDISG%ICDIST.EQ.0)NITEMS=9

 !## open idf-filename
 ALLOCATE(IDF(NITEMS))

 DO I=1,NITEMS
  IDF(I)%NROW=  NROW
  IDF(I)%NCOL  =NCOL
  IDF(I)%IEQ   =0
  IDF(I)%ITB   =0
  IDF(I)%DX    =GRIDISG%CS
  IDF(I)%DY    =GRIDISG%CS
  IDF(I)%XMIN  =GRIDISG%XMIN
  IDF(I)%XMAX  =GRIDISG%XMAX
  IDF(I)%YMIN  =GRIDISG%YMIN
  IDF(I)%YMAX  =GRIDISG%YMAX
  IDF(I)%NODATA=NODATAIDF
  IDF(I)%IXV   =0
  CALL IDFNULLIFY(IDF(I))
  IF(.NOT.IDFALLOCATEX(IDF(I)))THEN
   IF(IBATCH.EQ.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot allocate memory for IDF '//TRIM(FNAME(I))//TRIM(PPOSTFIX)//'.IDF'//CHAR(13)// &
      'Row/Column='//TRIM(ITOS(IDF(I)%NROW))//'-'//TRIM(ITOS(IDF(I)%NCOL)),'Error')
   ELSEIF(IBATCH.EQ.1)THEN
    WRITE(*,*) 'Cannot allocate memory for IDF '//TRIM(FNAME(I))//TRIM(PPOSTFIX)//'.IDF'
    WRITE(*,*) 'Nrow/Ncolumn=',IDF(I)%NCOL,IDF(I)%NROW
   ENDIF
   CALL IDFDEALLOCATE(IDF,SIZE(IDF)); DEALLOCATE(IDF)
   RETURN
  ENDIF
  IDF(I)%X=0.0
 END DO

 !## translate cdate in to julian date - for transient simulations only!
 IF(GRIDISG%ISTEADY.EQ.2)THEN
  GRIDISG%SDATE=UTL_IDATETOJDATE(GRIDISG%SDATE)
  GRIDISG%EDATE=UTL_IDATETOJDATE(GRIDISG%EDATE)+1
  TTIME=GRIDISG%EDATE-GRIDISG%SDATE
 ELSEIF(GRIDISG%ISTEADY.EQ.1)THEN
  TTIME=1
 ENDIF

 IF(ALLOCATED(QSORT))DEALLOCATE(QSORT); IF(ALLOCATED(XNR))DEALLOCATE(XNR); IF(ALLOCATED(NDATA))DEALLOCATE(NDATA)
 MDIM=4; IF(ISFR.EQ.1)MDIM=11; ALLOCATE(QSORT(TTIME,MDIM),XNR(MDIM),NDATA(MDIM))

 IF(ALLOCATED(X))DEALLOCATE(X);       IF(ALLOCATED(Y))DEALLOCATE(Y)
 IF(ALLOCATED(RVAL))DEALLOCATE(RVAL); IF(ALLOCATED(DIST))DEALLOCATE(DIST)
 IF(ALLOCATED(IPOS))DEALLOCATE(IPOS)
 !## max. numbers of coordinates AND number of calculation points AND number of structures
 MAXNSEG=MAXVAL(ISG(1:NISG)%NSEG)+MAXVAL(ISG(1:NISG)%NCLC)+2*MAXVAL(ISG(1:NISG)%NSTW)
 ALLOCATE(DIST(MAXNSEG),IPOS(MAXNSEG),RVAL(MDIM,0:MAXNSEG),X(MAXNSEG),Y(MAXNSEG))

 MAXDIM=0

 !## compute structures
 IF(GRIDISG%ICDIST.EQ.1)THEN
  LU=UTL_GETUNIT()
  I =0
  DO
   I       =I+1
   TMPFNAME=TRIM(PREFVAL(1))//'\tmp\tmp_stuwen_'//TRIM(ITOS(I))//'.ipf'
   CALL OSD_OPEN(LU,FILE=TMPFNAME,STATUS='UNKNOWN',FORM='FORMATTED',IOSTAT=IOS)
   IF(IOS.EQ.0)EXIT
  ENDDO

  WRITE(LU,*) SUM(ISG(1:NISG)%NSTW)
  WRITE(LU,'(A)') '7'
  WRITE(LU,'(A)') 'X-COORD.'
  WRITE(LU,'(A)') 'Y-COORD.'
  WRITE(LU,'(A)') 'Z-COORD.'
  WRITE(LU,'(A)') 'ORIENTATION_STUWING'
  WRITE(LU,'(A)') 'H1-H2'
  WRITE(LU,'(A)') 'NUMBER_IDENT'
  WRITE(LU,'(A)') 'STUWNAME'
  WRITE(LU,'(A)') '0,TXT'
 ENDIF

 IF(IBATCH.EQ.0)THEN
  CALL WINDOWSELECT(0)
  CALL WINDOWOUTSTATUSBAR(2,'Press Escape to stop!')
  CALL WINDOWOUTSTATUSBAR(4,'Progress Gridding 0%')
 ENDIF

 IRAT1=0
 ISTW =0

 ISGLOOP: DO I=1,NISG

  IF(IBATCH.EQ.0)THEN
   CALL WMESSAGEPEEK(ITYPE,MESSAGE)
   IF(ITYPE.EQ.KEYDOWN.AND.MESSAGE%VALUE1.EQ.KEYESCAPE)EXIT
  ENDIF

  LEX=.TRUE.
  IF(GRIDISG%IDIM.EQ.3)THEN
   LEX=.FALSE.
   IF(ISG(I)%ILIST.EQ.1)LEX=.TRUE.
  ENDIF

  !## could be existing, an element with 1 coordinate (point)
  IF(ISG(I)%NSEG.LE.1)LEX=.FALSE.

  IF(LEX)THEN

   NSEG=ISG(I)%NSEG
   ISEG=ISG(I)%ISEG
   JSEG=ISEG+NSEG-1
   
   !## copy coordinates
   K=0; DO J=ISEG,JSEG; K=K+1; X(K)=ISP(J)%X; Y(K)=ISP(J)%Y; END DO

   CALL ISGGRIDGETSTREAMDATA(X,Y,DIST,IPOS,RVAL,ISG(I)%ICLC,ISG(I)%NCLC,ISG(I)%ISTW,ISG(I)%NSTW,NSEG,MAXNSEG, &
                             QSORT,XNR,NDATA,TTIME,MDIM,GRIDISG%ISTEADY,GRIDISG%SDATE,GRIDISG%EDATE, &
                             GRIDISG%IAVERAGE)
  
   !## determine flow-direction
   LNODAT=.TRUE.
   DO JJ=1,4; IF(RVAL(JJ,1)   .EQ.GRIDISG%NODATA)THEN; LNODAT=.FALSE. ; EXIT; ENDIF; ENDDO
   DO JJ=1,4; IF(RVAL(JJ,NSEG).EQ.GRIDISG%NODATA)THEN; LNODAT=.FALSE. ; EXIT; ENDIF; ENDDO
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
      IF(H2.GT.H1)THEN
       Z          =RVAL(1,J)
       RVAL(1,J)  =RVAL(1,J+1)
       RVAL(1,J+1)=Z
      ELSE
       Z          =RVAL(1,J)
      ENDIF

      !## write ipf for structures
      IF(GRIDISG%ICDIST.EQ.1)THEN
       IREF=ISG(I)%ISTW+ABS(IPOS(J))-1
       CALL ISG2GRIDCOMPUTE_GETXYORIENT(I,IREF,XSTW,YSTW,GSTW)
       IF(H2.GT.H1)GSTW=GSTW+180.0
       GSTW=ISG2GRIDSTUWEN_GETORIENT(GSTW)
       ISTW=ISTW+1
       LINE=TRIM(RTOS(XSTW,'G',7))//','//TRIM(RTOS(YSTW,'G',7)) //','//TRIM(RTOS(Z,'G',7))//','// &
            TRIM(RTOS(GSTW,'G',7))//','//TRIM(RTOS(H1-H2,'G',7))//','//TRIM(ITOS(ISTW))//',"'//TRIM(IST(IREF)%CNAME)//'"'
       WRITE(LU,'(A)') TRIM(LINE)
      ENDIF

      J=J+1
     ENDIF
    ENDDO

    !## interpolate waterlevel,waterbottom,inf.factor,c-value - do not interupt it by structures (ipos().gt.0)!
    CALL ISGGRIDINTSTREAMDATA(DIST,IPOS,RVAL,NSEG,MAXNSEG,MDIM,GRIDISG%NODATA)

    !## start to intersect all segment/segmentpoints to the model-grid
    ISGLEN=0.0
    DO ISEG=2,NSEG

     X1 =X(ISEG-1); Y1=Y(ISEG-1); X2=X(ISEG); Y2=Y(ISEG)

     !## distance between two points with information
     DXY=((X(ISEG)-X(ISEG-1))**2.0)+((Y(ISEG)-Y(ISEG-1))**2.0)
     IF(DXY.GT.0.0)THEN
      DXY=SQRT(DXY)

      DO J=1,4; RVAL(J,0)=(RVAL(J,ISEG)-RVAL(J,ISEG-1))/DXY; ENDDO

      !## intersect line with rectangular-regular-equidistantial-grid
      N=0; CALL INTERSECT_EQUI(GRIDISG%XMIN,GRIDISG%XMAX,GRIDISG%YMIN,GRIDISG%YMAX, &
                               GRIDISG%CS,GRIDISG%CS,X1,X2,Y1,Y2,N,.FALSE.) !,.TRUE.)

      !## fill result array
      DXY=0.0
      DO J=1,N

       ISGLEN=ISGLEN+LN(J)

       !## which cross-section is active within current segment
       CALL ISG2GRIDGETCROSS(JCRS,ISG(I)%ICRS,ISG(I)%NCRS,ISGLEN)
       IF(JCRS.GT.0)THEN
        !## start of cross-section
        JSEG=ISG(I)%ICRS+JCRS-1
        NDIM=ABS(ISC(JSEG)%N)
       ELSE
        !## use default cross-section for current segment since no 1d cross-section or other has been found
        NDIM=4
       ENDIF

       !## there will be no cross-section
       IF(NDIM.LE.0)CYCLE

       IF(NDIM*2.GT.MAXDIM)THEN
        MAXDIM=NDIM*2
        IF(ALLOCATED(XIN))DEALLOCATE(XIN,YIN,XSYM,YSYM,XTRAP,YTRAP)
        ALLOCATE(XIN(MAXDIM),YIN(MAXDIM),XSYM(MAXDIM),YSYM(MAXDIM))
        ALLOCATE(XTRAP(4,MAXDIM),YTRAP(4,MAXDIM))
       ENDIF

       IF(JCRS.GT.0)THEN
        XIN(1:NDIM)=DATISC(ISC(JSEG)%IREF:ISC(JSEG)%IREF+NDIM-1)%DISTANCE
        YIN(1:NDIM)=DATISC(ISC(JSEG)%IREF:ISC(JSEG)%IREF+NDIM-1)%BOTTOM
        NSYM       =NDIM
       ELSE
        XIN(1)=-5.0; XIN(2)=-5.0; XIN(3)=5.0; XIN(4)=5.0
        YIN(1)= 5.0; YIN(2)= 0.0; YIN(3)=0.0; YIN(4)=0.0
        NSYM  = 4
       ENDIF

       !## make sure cross-section its minimal z-coordinate is zero!
       Z=MINVAL(YIN(1:NSYM)); YIN(1:NSYM)=YIN(1:NSYM)-Z

       !## compute trapezia
       IF(GRIDISG%ISIMGRO.EQ.1)THEN
        CALL ISGCOMPUTETRAPEZIUM(XIN,YIN,XSYM,YSYM,XTRAP,YTRAP,NTRAP,MAXDIM,NSYM,AORG,ATRAP)

        IF(NSYM.LE.0)THEN
         IF(IBATCH.EQ.0)THEN
          CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Crosssection: ['//TRIM(ISC(JSEG)%CNAME)//'] within: ['// &
                           TRIM(ISG(I)%SNAME)//'] incorrect','Error')
         ELSE
          WRITE(*,*) 'Crosssection: ['//TRIM(ISC(JSEG)%CNAME)//'] within: ['// &
                           TRIM(ISG(I)%SNAME)//'] incorrect'
         ENDIF
         EXIT ISGLOOP
        ENDIF
       ENDIF

       IF(LN(J).GT.0.0)THEN
        ICOL=CA(J); IROW=RA(J) !INT(XA(J)); IROW=INT(YA(J))
!        ICOL=INT(XA(J)); IROW=INT(YA(J))

        !## within model-domain
        IF(ICOL.GE.1.AND.IROW.GE.1.AND. &
           ICOL.LE.NCOL.AND.IROW.LE.NROW)THEN

         !## interpolate to centre of line ...
         DXY=DXY+(LN(J)/2.0)

         ISGVALUE(1,2)=RVAL(1,ISEG-1)+DXY*RVAL(1,0)    !waterlevel
         ISGVALUE(1,3)=RVAL(2,ISEG-1)+DXY*RVAL(2,0)    !waterbottom

         !## translate from local to global coordinates and get proper wetted perimeter and width of channel!
         CALL ISG2GRIDGETPARAM(XIN,YIN,NDIM,ISGVALUE(1,3),ISGVALUE(1,2),RWIDTH,WETPER,GRIDISG%MINDEPTH)

         ISGVALUE(1,4)= RVAL(4,ISEG-1)+DXY*RVAL(4,0)   !inf.factors
         C            = RVAL(3,ISEG-1)+DXY*RVAL(3,0)   !c-value
         !## minimal c-value
         C=MAX(0.001,C)
         ISGVALUE(1,1)=(LN(J)*WETPER)/C    !conductances

         ISGVALUE(1,5)= LN(J)
         ISGVALUE(1,6)= WETPER
         ISGVALUE(1,7)= RWIDTH
         ISGVALUE(1,8)= C

         IF(GRIDISG%ICDIST.EQ.1)THEN
          ISGVALUE(1,10)=RVAL(1,ISEG-1)+DXY*RVAL(1,0)   !waterlevels
         ENDIF

         !## export data for simgro
         IF(GRIDISG%ISIMGRO.EQ.1)THEN
          !## which qhpoint is active within current segment
          CALL ISG2GRIDGETQHR(JQHR,ISG(I)%IQHR,ISG(I)%NQHR,ISGLEN)
          !## start of qhr
          JSEG =ISG(I)%IQHR+JQHR-1

          !## get number of trapezia
          TC=0.0
          DO ITRAP=1,NTRAP
           !## compute nett waterdepth for current part of trapezium
           IF(ITRAP.LT.NTRAP)THEN; NETWD=YTRAP(3,ITRAP+1)-YTRAP(3,ITRAP)
           ELSE; NETWD=GRIDISG%WDEPTH-YTRAP(3,ITRAP); ENDIF
           IF(NETWD.LE.0.0)EXIT
           !## compute wetted perimeter,bottomwidth,cotanges
           CALL ISG2GRIDPERIMETERTRAPEZIUM(XTRAP(:,ITRAP),YTRAP(:,ITRAP),WETPER,CT,BW,NETWD)
           TC=TC+(LN(J)*WETPER)/C    !conductance(m2/dag)
          ENDDO
          NETTRAP=ITRAP-1; WRITE(IUSIMGRO,'(I10)') NETTRAP

!          TC=0.0
!          DO ITRAP=1,NTRAP
!           !## compute nett waterdepth for current part of trapezium
!           IF(ITRAP.LT.NTRAP)THEN
!            NETWD=YTRAP(3,ITRAP+1)-YTRAP(3,ITRAP)
!           ELSE
!            NETWD=WDEPTH-YTRAP(3,ITRAP)
!           ENDIF
!           !## still water left ...
!           IF(NETWD.GT.0.0)THEN
!            !## compute wetted perimeter,bottomwidth,cotanges
!            CALL ISG2GRIDPERIMETERTRAPEZIUM(XTRAP(:,ITRAP),YTRAP(:,ITRAP),WETPER,CT,BW,NETWD)
!            TC=TC+(LN(J)*WETPER)/C    !conductance(m2/dag)
!           ENDIF
!          ENDDO

          !## correction factor
          TC=ISGVALUE(1,1)/TC
          DO ITRAP=1,NTRAP

           !## compute nett waterdepth for current part of trapezium
           IF(ITRAP.LT.NTRAP)THEN
            NETWD=YTRAP(3,ITRAP+1)-YTRAP(3,ITRAP)
           ELSE
            NETWD=GRIDISG%WDEPTH-YTRAP(3,ITRAP)
           ENDIF

           !## still water left ...
           IF(NETWD.GT.0.0)THEN
            !## compute wetted perimeter,bottomwidth,cotanges
            CALL ISG2GRIDPERIMETERTRAPEZIUM(XTRAP(:,ITRAP),YTRAP(:,ITRAP),WETPER,CT,BW,NETWD)
            !## get bottom height (bh)
            BH  = ISGVALUE(1,3)+YTRAP(3,ITRAP)
            COND=(LN(J)*WETPER)/C    !conductance(m2/dag)
            COND= TC*COND

            !## minimal value cond=0.001
            CALL IDFGETLOC(IDF(1),IROW,ICOL,XC,YC)
            WRITE(IUSIMGRO,'(2F10.2,2I10,6F10.2,A30,3F10.2)') XC,YC,IROW,ICOL,LN(J),BH,BW,CT,MAX(0.001,COND),MAX(0.001,COND*ISGVALUE(1,4)), &
                                                         ISQ(JSEG)%CNAME,AORG-ATRAP,AORG,ATRAP
           ENDIF

          ENDDO
          GRIDISG%NSVATS=GRIDISG%NSVATS+1
         ENDIF

         !## level=conductance weighed mean
         IF(IDF(1)%X(ICOL,IROW).EQ.IDF(1)%NODATA)IDF(1)%X(ICOL,IROW)=0.0
         DO II=2,NITEMS
          IF(IDF(II)%X(ICOL,IROW).EQ.IDF(II)%NODATA)IDF(II)%X(ICOL,IROW)=0.0
          !## read previous level/bottom/inf.factor
          ISGVALUE(2,II)=IDF(II)%X(ICOL,IROW)
          !## multiply with conductance (5-total length)
          IF(II.NE.5)ISGVALUE(1,II)=ISGVALUE(1,II)*ISGVALUE(1,1)
          !## conductance weighed
          IF(ISGVALUE(2,II).NE.IDF(II)%NODATA)ISGVALUE(1,II)=ISGVALUE(2,II)+ISGVALUE(1,II)
          IDF(II)%X(ICOL,IROW)=ISGVALUE(1,II)
         END DO
         !## sum conductance
         ISGVALUE(2,1)=IDF(1)%X(ICOL,IROW)
         IF(ISGVALUE(2,1).NE.IDF(1)%NODATA)ISGVALUE(1,1)=ISGVALUE(2,1)+ISGVALUE(1,1)
         IDF(1)%X(ICOL,IROW)=ISGVALUE(1,1)

         DXY=DXY+(LN(J)/2.0)

        ENDIF
       ENDIF
      ENDDO
     ENDIF
    ENDDO
   ENDIF
  ENDIF
  IF(IBATCH.EQ.0)CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,NISG,'Progress gridding ')
 ENDDO ISGLOOP
 IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Finished gridding'

 !## devide by conductance again
 DO IROW=1,NROW
  DO ICOL=1,NCOL
   IF(IDF(1)%X(ICOL,IROW).NE.IDF(1)%NODATA)THEN
    ISGVALUE(1,1)=IDF(1)%X(ICOL,IROW)
    DO II=2,NITEMS
     !## skip length
     IF(II.EQ.5)CYCLE
     ISGVALUE(2,II)=IDF(II)%X(ICOL,IROW)
!IF(ISGVALUE(1,1).LE.0.0)THEN
!WRITE(*,*)
!ENDIF
     ISGVALUE(1,II)=ISGVALUE(2,II)/ISGVALUE(1,1)
     IDF(II)%X(ICOL,IROW)=ISGVALUE(1,II)
    ENDDO
   ENDIF
  ENDDO
 ENDDO

 !## add 2d cross-section
 IRAT=0; IRAT1=0
 DO ISELISG=1,NISG
  LEX=.TRUE.; IF(GRIDISG%IDIM.EQ.3)THEN; LEX=.FALSE.; IF(ISG(ISELISG)%ILIST.EQ.1)LEX=.TRUE.; ENDIF
  IF(.NOT.LEX)CYCLE
  IF(IBATCH.EQ.0)THEN
   CALL WMESSAGEPEEK(ITYPE,MESSAGE)
   IF(ITYPE.EQ.KEYDOWN.AND.MESSAGE%VALUE1.EQ.KEYESCAPE)EXIT
  ENDIF
  NSEG=ISG(ISELISG)%NSEG; ISEG=ISG(ISELISG)%ISEG; ICRS=ISG(ISELISG)%ICRS-1; NCRS=ISG(ISELISG)%NCRS
  !## cross-sections
  IF(.NOT.ISGATTRIBUTESREADISCVALUE(0))EXIT
  DO I=1,NCRS
   ICRS=ICRS+1
   !## found 2d cross-section
   IF(ISC(ICRS)%N.GE.0)CYCLE

   IF(ISC(ICRS)%N.GT.0)THEN
    IF(ISC(ICRS)%N.LE.2)THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMODFLOW cannot apply a 1D cross-section with less or equal 2 points'//CHAR(13)// &
       'Cross-section name '//TRIM(ISC(ICRS)%CNAME),'Error')
    ENDIF
   ELSE
    IF(ABS(ISC(ICRS)%N).LE.1)THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMODFLOW cannot apply a 2D cross-section with no locations'//CHAR(13)// &
      'Cross-section name '//TRIM(ISC(ICRS)%CNAME),'Error')
    ENDIF
   ENDIF

   CALL ISGADJUSTCOMPUTEXY(ISEG,NSEG,ISC(ICRS)%DIST,TD)       !## compute correct x/y coordinate of current cross-section
   CALL IDFIROWICOL(IDF(2),IROW,ICOL,ISGX,ISGY)               !## get location in raster
   !## skip if outside current model network
   IF(IROW.EQ.0.OR.ICOL.EQ.0)CYCLE
   IF(ISGATTRIBUTES_2DCROSS_READ(I,ICROSS,PCROSS,ZCHK))THEN   !## read bathymetry current cross-section
    WL=IDF(2)%X(ICOL,IROW)                                    !## waterlevel at cross-section
    !## infiltration factor at location of cross-section
    INFF=IDF(4)%X(ICOL,IROW)
    C =IDF(8)%X(ICOL,IROW)                                    !## resistance at location of cross-section
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
    CALL ISG2GRID_BATHEMETRY(IDF,SIZE(IDF),ICROSS,PCROSS,ZCHK,WL,C,INFF)  !## adjust stage grid for bathymetry
    CALL IDFDEALLOCATEX(ICROSS); CALL IDFDEALLOCATEX(PCROSS)
   ENDIF
   IF(IBATCH.EQ.0)CALL UTL_WAITMESSAGE(IRAT,IRAT1,ISELISG,NISG,'Progress gridding 2d cross-sections')
  ENDDO
 ENDDO
 IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Finished gridding 2d cross-sections'

 IF(ALLOCATED(DATISC2))DEALLOCATE(DATISC2); IF(ALLOCATED(TISC))DEALLOCATE(TISC); IF(ALLOCATED(ISCN))DEALLOCATE(ISCN)

 IF(GRIDISG%ICDIST.EQ.1)CLOSE(LU)

 DO IROW=1,NROW; DO ICOL=1,NCOL
  !## turn into nodata for conductances equal to zero
  IF(IDF(1)%X(ICOL,IROW).EQ.IDF(1)%NODATA)THEN
   IDF(2)%X(ICOL,IROW)=IDF(2)%NODATA
   IDF(3)%X(ICOL,IROW)=IDF(3)%NODATA
   IF(GRIDISG%ICDIST.EQ.1)IDF(10)%X(ICOL,IROW)=IDF(10)%NODATA
  ENDIF
 ENDDO; ENDDO

 IF(ALLOCATED(RVAL))DEALLOCATE(RVAL)
 IF(ALLOCATED(DIST))DEALLOCATE(DIST)
 IF(ALLOCATED(IPOS))DEALLOCATE(IPOS)
 IF(ALLOCATED(X))DEALLOCATE(X); IF(ALLOCATED(Y))DEALLOCATE(Y)

 IF(ALLOCATED(QSORT))DEALLOCATE(QSORT)
 IF(ALLOCATED(XNR))DEALLOCATE(XNR)
 IF(ALLOCATED(NDATA))DEALLOCATE(NDATA)
 CALL INTERSECT_DEALLOCATE()

 IF(GRIDISG%ISIMGRO.EQ.1)THEN
  IF(ALLOCATED(XIN))DEALLOCATE(XIN)
  IF(ALLOCATED(YIN))DEALLOCATE(YIN)
  IF(ALLOCATED(XSYM))DEALLOCATE(XSYM)
  IF(ALLOCATED(YSYM))DEALLOCATE(YSYM)
  IF(ALLOCATED(XTRAP))DEALLOCATE(XTRAP)
  IF(ALLOCATED(YTRAP))DEALLOCATE(YTRAP)
  GRIDISG%EDATE=GRIDISG%NSVATS
 ENDIF

 IF(GRIDISG%ICDIST.EQ.1)THEN
  IF(.NOT.IDFWRITE(IDF(10),TRIM(GRIDISG%ROOT)//'\'//TRIM(FNAME(10))//TRIM(PPOSTFIX)//'.IDF',1))THEN; RETURN; ENDIF
  IF(.NOT.IDFWRITE(IDF(11),TRIM(GRIDISG%ROOT)//'\'//TRIM(FNAME(11))//TRIM(PPOSTFIX)//'.IDF',1))THEN; RETURN; ENDIF
  CALL ISG2GRIDCOMPUTESTUWEN(TRIM(TMPFNAME),TRIM(GRIDISG%ROOT)//'\'//TRIM(FNAME(10))//TRIM(PPOSTFIX)//'.IDF',  & !## effect
                                            TRIM(GRIDISG%ROOT)//'\'//TRIM(FNAME(11))//TRIM(PPOSTFIX)//'.IDF') ! & !## current_id
!                                            TRIM(GRIDISG%ROOT)//'\'//TRIM(FNAME(12))//TRIM(PPOSTFIX)//'.IDF')    !## next_id
 ENDIF

 !## extent grids based upon their width
 CALL ISG2GRID_EXTENT_WITH_WIDTH(SIZE(IDF),IDF,IBATCH,GRIDISG%MAXWIDTH)

 !## clean all
 DO IROW=1,NROW; DO ICOL=1,NCOL
  !## turn into nodata for conductances equal to zero
  IF(IDF(1)%X(ICOL,IROW).EQ.IDF(1)%NODATA)THEN
   DO I=1,SIZE(IDF); IDF(I)%X(ICOL,IROW)=-9999.00; ENDDO
  ENDIF
 ENDDO; ENDDO
 IDF%NODATA=-9999.00

 IF(GRIDISG%IEXPORT.EQ.0)THEN
  DO I=1,9
   IF(GRIDISG%ISAVE(I).EQ.0)CYCLE
   IF(IBATCH.EQ.1)WRITE(*,*) 'Saving '//TRIM(GRIDISG%ROOT)//'\'//TRIM(FNAME(I))//TRIM(PPOSTFIX)//'.IDF ...'
   IF(.NOT.IDFWRITE(IDF(I),TRIM(GRIDISG%ROOT)//'\'//TRIM(FNAME(I))//TRIM(PPOSTFIX)//'.IDF',1))THEN
    !##error
    IF(IBATCH.EQ.1)WRITE(*,*) '---- ERROR saving file ----'
   ENDIF
  ENDDO
 ELSEIF(GRIDISG%IEXPORT.EQ.1)THEN
  CALL ISG2GRID_EXPORTRIVER(JU,IDF,NLAY,ILAY,TOP,BOT,MP,GRIDISG)
 ENDIF

 CALL IDFDEALLOCATE(IDF,SIZE(IDF)); DEALLOCATE(IDF)
 IF(GRIDISG%ISIMGRO.EQ.1)CLOSE(IUSIMGRO)

 ISG2GRID=.TRUE.

 END FUNCTION ISG2GRID

 !###====================================================================
 SUBROUTINE ISGGRIDGETSTREAMDATA(X,Y,DIST,IPOS,RVAL,ICLC,NCLC,ISTW,NSTW, &
                                 NSEG,MAXNSEG,QSORT,XNR,NDATA,TTIME,NDIM,&
                                 ISTEADY,SDATE,EDATE,IAVERAGE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: MAXNSEG,ICLC,ISTW,NCLC,NSTW,TTIME,NDIM,ISTEADY,SDATE, &
                       EDATE,IAVERAGE
 INTEGER,INTENT(INOUT) :: NSEG
 REAL,INTENT(OUT),DIMENSION(MAXNSEG) :: X,Y,DIST
 REAL,INTENT(OUT),DIMENSION(NDIM,0:MAXNSEG) :: RVAL
 REAL,INTENT(OUT),DIMENSION(NDIM) :: XNR
 REAL,INTENT(IN),DIMENSION(NDIM) :: NDATA
 REAL,INTENT(OUT),DIMENSION(TTIME,NDIM) :: QSORT
 INTEGER,INTENT(OUT),DIMENSION(MAXNSEG) :: IPOS
 INTEGER :: I,IREF
 
 IPOS=0; RVAL=0.0

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
 SUBROUTINE ISGGRIDINTSTREAMDATA(DIST,IPOS,RVAL,NSEG,MAXNSEG,NDIM,NODATA)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: MAXNSEG,NSEG,NDIM
 REAL,INTENT(IN) :: NODATA
 REAL,INTENT(INOUT),DIMENSION(MAXNSEG) :: DIST
 REAL,INTENT(INOUT),DIMENSION(NDIM,0:MAXNSEG) :: RVAL
 INTEGER,INTENT(INOUT),DIMENSION(MAXNSEG) :: IPOS
 INTEGER :: I,II,J,K
 REAL :: FCT,D
 
 DO I=1,NDIM
  DO J=1,NSEG
   IF(IPOS(J).GT.0.AND.RVAL(I,J).NE.NODATA)THEN
    DO II=J+1,NSEG
     IF(IPOS(II).GT.0.AND.RVAL(I,II).NE.NODATA)EXIT
    ENDDO
    D=MAX(0.0,DIST(II)-DIST(J))
    DO K=J+1,II-1
     FCT=0.0
     IF(D.GT.0.0)FCT=(DIST(K)-DIST(J))/D
     RVAL(I,K)=RVAL(I,J)+((RVAL(I,II)-RVAL(I,J))*FCT)
    END DO
   ENDIF
  ENDDO
 ENDDO

 END SUBROUTINE ISGGRIDINTSTREAMDATA

 !###====================================================================
 LOGICAL FUNCTION ISG2SFR(NROW,NCOL,NLAY,ILAY,TOP,BOT,IPER,NPER,MP,JU,GRIDISG,EXFNAME)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: EXFNAME
 INTEGER,INTENT(IN) :: NROW,NCOL,NLAY,ILAY,JU,IPER,NPER
 TYPE(GRIDISGOBJ),INTENT(INOUT) :: GRIDISG
 INTEGER,INTENT(INOUT),DIMENSION(:) :: MP
 TYPE(IDFOBJ),DIMENSION(NLAY),INTENT(INOUT) :: TOP,BOT
 INTEGER :: I,J,K,II,JJ,KK,KKK,TTIME,IROW,ICOL,N,ISEG,JSEG,NSEG,IREF,NDIM,KSEG,MSEG,IDATE,IISG, &
       ICALC,OUTSEG,IUPSEG,IPRIOR,NSTRPTS,ICRS,ICLC,IQHR,NREACH,NSTREAM,KCRS,CRSREF,KCLC,CLCREF
 REAL :: DXY,X1,X2,Y1,Y2,QFLOW,QROFF,EVT,PREC,ROUGHCH,ROUGHBK,CDPTH,FDPTH,AWDTH,BWDTH,DIST, &
       HC1FCT,THICKM1,ELEVUP,WIDTH1,DEPTH1,HC2FCT,THICKM2,ELEVDN,WIDTH2,DEPTH2,WLVLUP,WLVLDN
 REAL,ALLOCATABLE,DIMENSION(:,:) :: QSORT,RVAL
 REAL,ALLOCATABLE,DIMENSION(:) :: XNR,NDATA
 REAL,ALLOCATABLE,DIMENSION(:) :: XCRS,ZCRS,MCRS,QCRS,WCRS,DCRS
 LOGICAL :: LEX
 CHARACTER(LEN=512) :: LINE
 CHARACTER(LEN=MAXLEN) :: CNAME

 ISG2SFR=.FALSE.

 !## only specify for first stress-period - write output to regular ISG as well
 IF(IPER.EQ.1)THEN

  DO KK=1,2
  
   IF(KK.EQ.2)THEN
    !## create copy to store results
    ISFR=0; IF(.NOT.ISGOPENFILES(EXFNAME,'REPLACE'))THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot (re)write ISG file: '//CHAR(13)//TRIM(EXFNAME),'Error')
     RETURN
    ENDIF

    !## write header *.ISG file in result-ISG
    CALL ISGSAVEHEADERS()

    LINE=TRIM(ITOS(IISG))//','//TRIM(ITOS(ISFR))//',Date,StreamLevel,StreamDepth,StreamWidth,StreamDischarge'
    WRITE(ISGIU(1,1),'(A)') TRIM(LINE)
  
    ISFR=1
   ENDIF
   
   IISG=0
  
   KSEG=1; KCRS=1; CRSREF=1; KCLC=1; CLCREF=1

   !## variable used to stored number of reaches per segment
   IF(ALLOCATED(ISTR))DEALLOCATE(ISTR); ALLOCATE(ISTR(NISG)); ISTR=0

   !## count number of streams
   ALLOCATE(IACTSTREAM(NISG)); IACTSTREAM=0; NSTREAM=0

   DO I=1,NISG
    NSEG=ISG(I)%NSEG; ISEG=ISG(I)%ISEG; JSEG=ISEG+NSEG-1
    !## start to intersect all segment/segmentpoints to the model-grid
    ISTR(I)=0
    N=0
    !## intersect complete line
    DO J=ISEG+1,JSEG
     !## get coordinates of current reach in segment
     X1 =ISP(J-1)%X; Y1=ISP(J-1)%Y; X2=ISP(J)%X; Y2=ISP(J)%Y
     !## distance between two points with information
     DXY=(X2-X1)**2.0+(Y2-Y1)**2.0; IF(DXY.LE.0.0)CYCLE; DXY=SQRT(DXY)
     !## intersect line with rectangular-regular-equidistantial-grid
     CALL INTERSECT_EQUI(GRIDISG%XMIN,GRIDISG%XMAX,GRIDISG%YMIN,GRIDISG%YMAX,GRIDISG%CS, &
                         GRIDISG%CS,X1,X2,Y1,Y2,N,.FALSE.)
    ENDDO

    !## fill result array
    MSEG=0; DIST=0.0
    K=1; DO 
  
     !## skip outside model domain
     IF(CA(K).LT.1.OR.CA(K).GT.NCOL.OR.RA(K).LT.1.OR.RA(K).GT.NROW)THEN
      K=K+1; IF(K.GT.N)EXIT; CYCLE
     ENDIF

     !## found segment inside model
     IF(LN(K).GT.0.0)THEN

      !## increase number of stream reaches
      ISTR(I)=ISTR(I)+1
      !## increase number of streams
      IF(ISTR(I).EQ.1)NSTREAM=NSTREAM+1
      !## total number of reaches per segment
      IACTSTREAM(I)=IACTSTREAM(I)+1
      !## total segments
      IISG=IISG+1
       
      DIST=0.0; KKK=K; DO
       IF(CA(KKK).NE.CA(K).OR.RA(KKK).NE.RA(K))EXIT
       DIST=DIST+LN(KKK); KKK=KKK+1
      ENDDO

      IF(KK.EQ.2)THEN

       ICOL=CA(K); IROW=RA(K)  
      
       !## write into sfr package
       LINE=TRIM(ITOS(ILAY))   //','//TRIM(ITOS(IROW))   //','//TRIM(ITOS(ICOL))//','// &
            TRIM(ITOS(NSTREAM))//','//TRIM(ITOS(ISTR(I)))//','//TRIM(RTOS(DIST,'G',7))
       WRITE(JU,'(A)') TRIM(LINE)
       
       !## write coordinate-couple in isp (use all coordinates)
       DO JJ=K,KKK-1
        KSEG=KSEG+1; MSEG=MSEG+1; WRITE(ISGIU(2,1),REC=KSEG) REAL(XA(JJ))  ,REAL(YA(JJ))
        KSEG=KSEG+1; MSEG=MSEG+1; WRITE(ISGIU(2,1),REC=KSEG) REAL(XA(JJ+1)),REAL(YA(JJ+1))
       ENDDO

       !## add cross-section
       ICRS=ISG(I)%ICRS       !## position in isc that starts cross-section
       NDIM=ABS(ISC(ICRS)%N)  !## number of cross-sectional data-points

       KCRS=KCRS+1
       WRITE(ISGIU(5,1),REC=KCRS) ISC(ICRS)%N,CRSREF,0.5*DIST,ISC(ICRS)%CNAME

       !## write cross-section
       II=ISC(ICRS)%IREF-1 ; DO JJ=1,NDIM
        II=II+1; CRSREF=CRSREF+1; WRITE(ISGIU(6,1),REC=CRSREF) DATISC(II)%DISTANCE,DATISC(II)%BOTTOM,DATISC(II)%MRC
       ENDDO

       ICLC=ISG(I)%ICLC       !## position in isd that starts calculation node

       !## add space for time-variant data - put data into it
       DO II=1,2
        KCLC=KCLC+1
        IF(II.EQ.1)THEN
         CNAME='From_Node'
         WRITE(ISGIU(3,1),REC=KCLC) NPER,CLCREF,0.0,CNAME 
        ELSEIF(II.EQ.2)THEN
         CNAME='To_Node'
         WRITE(ISGIU(3,1),REC=KCLC) NPER,CLCREF,DIST,CNAME
        ENDIF
        DO JJ=1,NPER
         CLCREF=CLCREF+1
         IF(SIM(JJ)%DELT.GT.0.0)THEN
          IDATE=SIM(JJ)%IYR*10000+SIM(JJ)%IMH*100+SIM(JJ)%IDY
         ELSE
          IDATE=UTL_GETCURRENTDATE()
         ENDIF
         WRITE(ISGIU(4,1),REC=CLCREF) IDATE,10.0,10.0,10.0
        ENDDO
       ENDDO
     
       !## each reach is a segment in the isg-file
       LINE='"'//TRIM(ISG(I)%SNAME)//'_reach'//TRIM(ITOS(ISTR(I)))//'",'//TRIM(ITOS(KSEG-MSEG))//','// &
                 TRIM(ITOS(MSEG))  //','//TRIM(ITOS(KCLC-2))//','//TRIM(ITOS(2))//','// &
                 TRIM(ITOS(KCRS-1))  //','//TRIM(ITOS(1))//',0,0,0,0'
       WRITE(ISGIU(1,1),*) TRIM(LINE)
       MSEG=0 !; DIST=0.0
      ENDIF
     
      K=KKK !-1
     ELSE
      K=K+1
     ENDIF

     IF(K.GT.N)EXIT

    ENDDO
   ENDDO
   IF(KK.EQ.1)DEALLOCATE(ISTR,IACTSTREAM)
  ENDDO
 
 ENDIF

 !## total number of reaches - determines for stress-period 1, stays the same
 NREACH=SUM(ISTR)

 !## translate cdate in to julian date - for transient simulations only!
 IF(GRIDISG%ISTEADY.EQ.2)THEN
  GRIDISG%SDATE=UTL_IDATETOJDATE(GRIDISG%SDATE)
  GRIDISG%EDATE=UTL_IDATETOJDATE(GRIDISG%EDATE)+1
  TTIME=GRIDISG%EDATE-GRIDISG%SDATE
 ELSEIF(GRIDISG%ISTEADY.EQ.1)THEN
  TTIME=1
 ENDIF

 IF(ALLOCATED(QSORT))DEALLOCATE(QSORT); IF(ALLOCATED(XNR))DEALLOCATE(XNR); IF(ALLOCATED(NDATA))DEALLOCATE(NDATA)
 NDIM=11; ALLOCATE(QSORT(TTIME,NDIM),XNR(NDIM),NDATA(NDIM))

 !## allocate storage for cross-sectional data
 ALLOCATE(XCRS(NDIM),ZCRS(NDIM),MCRS(NDIM),QCRS(NDIM),WCRS(NDIM),DCRS(NDIM))

 !## variable used to store segment information
 IF(ALLOCATED(RVAL))DEALLOCATE(RVAL); ALLOCATE(RVAL(NDIM,2)); RVAL=0.0

 !## to be filled in later - number of streams
 LINE='NaN1,'//TRIM(ITOS(IRDFLG))//','//TRIM(ITOS(IPTFLG))//',0'
 WRITE(JU,'(A)') TRIM(LINE)

 !## write cross-sectional data
 NSTREAM=0; DO I=1,NISG

  !## skip this if not in current model domain
  IF(IACTSTREAM(I).LE.0)CYCLE
  
  !## subsequent numbering of segments
  NSTREAM=NSTREAM+1
  
  !## get this information from the cross-sectional data (only one cross-section available)
  ICRS=ISG(I)%ICRS       !## position in isc that starts cross-section
  NDIM=ABS(ISC(ICRS)%N)  !## number of cross-sectional data-points

  IF(NDIM.GT.SIZE(XCRS))THEN
   DEALLOCATE(XCRS,ZCRS,MCRS)
   ALLOCATE(XCRS(NDIM),ZCRS(NDIM),MCRS(NDIM))
  ENDIF

  J=ISC(ICRS)%IREF-1 ; DO K=1,NDIM
   J=J+1; XCRS(K)=DATISC(J)%DISTANCE; ZCRS(K)=DATISC(J)%BOTTOM; MCRS(K)=DATISC(J)%MRC
  ENDDO

  RVAL=0.0
  !## read data for start-point
  IREF=ISG(I)%ICLC
  CALL ISG2GRIDGETDATA(GRIDISG%SDATE,GRIDISG%EDATE,TTIME,QSORT,XNR,SIZE(XNR),RVAL(1,1),ISD(IREF)%N, &
                       ISD(IREF)%IREF,GRIDISG%ISTEADY,1,NDATA,GRIDISG%IAVERAGE)
  !## read data for end-point
  IREF=ISG(I)%ICLC+1
  CALL ISG2GRIDGETDATA(GRIDISG%SDATE,GRIDISG%EDATE,TTIME,QSORT,XNR,SIZE(XNR),RVAL(1,2),ISD(IREF)%N, & 
                       ISD(IREF)%IREF,GRIDISG%ISTEADY,1,NDATA,GRIDISG%IAVERAGE)

  WLVLUP =RVAL(1,1); WLVLDN =RVAL(1,2)
  ELEVUP =RVAL(2,1); ELEVDN =RVAL(2,2)
  WIDTH1 =RVAL(3,1); WIDTH2 =RVAL(3,2)
  THICKM1=RVAL(4,1); THICKM2=RVAL(4,2)
  HC1FCT =RVAL(5,1); HC2FCT =RVAL(5,2)
  ICALC =INT(RVAL(8,1)) !## calculation option streamdepth
  IUPSEG=INT(RVAL(6,1)) !## upstream segment
  OUTSEG=INT(RVAL(7,2)) !## downstream segment
  IPRIOR=INT(RVAL(9,1)) !## dividing option
  QFLOW =RVAL(10,1)     !## inflow
  QROFF =RVAL(11,1)     !## runoff flow

  !## corrections for reding out of a menu
  ICALC=ICALC-1; IPRIOR=IPRIOR-1

  EVT=0.0
  PREC=0.0
  LINE=TRIM(ITOS(NSTREAM))//','//TRIM(ITOS(ICALC))//','//TRIM(ITOS(OUTSEG))//','//TRIM(ITOS(IUPSEG))

  IF(IUPSEG.GT.0)LINE=TRIM(LINE)//','//TRIM(ITOS(IPRIOR))
  !##
  IF(ICALC.EQ.4)THEN

   !## get this information from the q-width/depth relationships data (only one q-width/depth relationships available)
   IQHR=ISG(I)%IQHR          !## position in isq that starts q-width/depth relationships
   NSTRPTS=ABS(ISQ(IQHR)%N)  !## number of q-width/depth relationships
   LINE=TRIM(LINE)//','//TRIM(ITOS(NSTRPTS))

  ENDIF
  LINE=TRIM(LINE)//','//TRIM(RTOS(QFLOW,'G',7))//','//TRIM(RTOS(QROFF,'G',7))//','// &
                        TRIM(RTOS(EVT,'G',7))//','//TRIM(RTOS(PREC,'G',7))
  !## riverbed mannings coefficient
  IF(ICALC.EQ.1.OR.ICALC.EQ.2)THEN
   ROUGHCH=MCRS(4)
   LINE=TRIM(LINE)//','//TRIM(RTOS(ROUGHCH,'G',7))
  ENDIF
  !## riverbank mannings coefficient
  IF(ICALC.EQ.2)THEN
   ROUGHBK=MCRS(1)
   LINE=TRIM(LINE)//','//TRIM(RTOS(ROUGHBK,'G',7))
  ENDIF
  !## function
  IF(ICALC.EQ.3)THEN
   !## not supported
   CDPTH=0.3
   FDPTH=0.35
   AWDTH=3.8
   BWDTH=0.6
   LINE=TRIM(LINE)//','//TRIM(RTOS(CDPTH,'G',7))//','//TRIM(RTOS(FDPTH,'G',7))//','// &
                         TRIM(RTOS(AWDTH,'G',7))//','//TRIM(RTOS(BWDTH,'G',7))
  ENDIF
  WRITE(JU,'(A)') TRIM(LINE)

  !## waterdepth up- and downstream
  DEPTH1 =WLVLUP-ELEVUP
  DEPTH2 =WLVLDN-ELEVDN

  LINE=TRIM(RTOS(HC1FCT,'G',7))//','//TRIM(RTOS(THICKM1,'G',7))//','//TRIM(RTOS(ELEVUP,'G',7))
  IF(ICALC.LE.1)LINE=TRIM(LINE)//','//TRIM(RTOS(WIDTH1,'G',7))
  IF(ICALC.EQ.0)LINE=TRIM(LINE)//','//TRIM(RTOS(DEPTH1,'G',7))
  WRITE(JU,'(A)') TRIM(LINE)

  LINE=TRIM(RTOS(HC2FCT,'G',7))//','//TRIM(RTOS(THICKM2,'G',7))//','//TRIM(RTOS(ELEVDN,'G',7))
  IF(ICALC.LE.1)LINE=TRIM(LINE)//','//TRIM(RTOS(WIDTH2,'G',7))
  IF(ICALC.EQ.0)LINE=TRIM(LINE)//','//TRIM(RTOS(DEPTH2,'G',7))
  WRITE(JU,'(A)') TRIM(LINE)

  !## eight points cross-section
  IF(ICALC.EQ.2)THEN
   LINE=TRIM(RTOS(0.0,'G',7)); DO J=2,NDIM; LINE=TRIM(LINE)//','//TRIM(RTOS(XCRS(J)-XCRS(1),'G',7)); ENDDO
   WRITE(JU,'(A)') TRIM(LINE)
   LINE=TRIM(RTOS(ZCRS(1),'G',7)); DO J=2,NDIM; LINE=TRIM(LINE)//','//TRIM(RTOS(ZCRS(J),'G',7)); ENDDO
   WRITE(JU,'(A)') TRIM(LINE)
  !## q-width/depth relationships
  ELSEIF(ICALC.EQ.4)THEN

   !## get this information from the q-width/depth relationships data
   IQHR=ISG(I)%IQHR

   IF(NSTRPTS.GT.SIZE(QCRS))THEN
    DEALLOCATE(QCRS,DCRS,WCRS)
    ALLOCATE(QCRS(NSTRPTS),DCRS(NSTRPTS),WCRS(NSTRPTS))
   ENDIF

   J=ISC(IQHR)%IREF-1 ; DO K=1,NSTRPTS
    J=J+1; QCRS(K)=DATISQ(J)%Q; WCRS(K)=DATISQ(J)%W; DCRS(K)=DATISQ(J)%D
   ENDDO

   LINE=TRIM(RTOS(QCRS(1),'G',7)); DO J=2,NSTRPTS; LINE=TRIM(LINE)//','//TRIM(RTOS(QCRS(J),'G',7)); ENDDO
   WRITE(JU,'(A)') TRIM(LINE)
   LINE=TRIM(RTOS(DCRS(1),'G',7)); DO J=2,NSTRPTS; LINE=TRIM(LINE)//','//TRIM(RTOS(DCRS(J),'G',7)); ENDDO
   WRITE(JU,'(A)') TRIM(LINE)
   LINE=TRIM(RTOS(WCRS(1),'G',7)); DO J=2,NSTRPTS; LINE=TRIM(LINE)//','//TRIM(RTOS(WCRS(J),'G',7)); ENDDO
   WRITE(JU,'(A)') TRIM(LINE)

  ENDIF

 ENDDO

 MP(1)=NSTREAM
 MP(2)=NREACH

 IF(ALLOCATED(XCRS))DEALLOCATE(XCRS,ZCRS,MCRS)
 IF(ALLOCATED(QCRS))DEALLOCATE(QCRS,WCRS,DCRS)
 IF(ALLOCATED(RVAL))DEALLOCATE(RVAL)
 IF(ALLOCATED(QSORT))DEALLOCATE(QSORT); IF(ALLOCATED(XNR))DEALLOCATE(XNR); IF(ALLOCATED(NDATA))DEALLOCATE(NDATA)
 !## clean up
 IF(IPER.EQ.NPER)THEN
  DEALLOCATE(ISTR,IACTSTREAM)
 ENDIF
 CALL INTERSECT_DEALLOCATE()

 ISG2SFR=.TRUE.

 END FUNCTION ISG2SFR

 !###====================================================================
 SUBROUTINE ISG2GRID_EXPORTRIVER(JU,IDF,NLAY,ILAY,TOP,BOT,MP,GRIDISG)
 !###====================================================================
 IMPLICIT NONE
 TYPE(GRIDISGOBJ),INTENT(INOUT) :: GRIDISG
 INTEGER,INTENT(IN) :: NLAY,ILAY,JU
 INTEGER,INTENT(INOUT),DIMENSION(:) :: MP
 TYPE(IDFOBJ),DIMENSION(:),INTENT(INOUT) :: IDF
 TYPE(IDFOBJ),DIMENSION(NLAY),INTENT(INOUT) :: TOP,BOT
 INTEGER :: IROW,ICOL,N,IU,I
 REAL :: T,B,WL,BL,CD,F
 CHARACTER(LEN=25) :: FRM

 IF(ILAY.EQ.0)THEN
  !## read in all top/bottom layers
  DO I=1,NLAY
   IF(.NOT.ASSOCIATED(TOP(I)%X))THEN
    WRITE(*,'(A)') 'Reading '//TRIM(TOP(I)%FNAME)//' ...'
    CALL IDFCOPY(IDF(1),TOP(I))
    IF(.NOT.IDFREADSCALE(TOP(I)%FNAME,TOP(I),2,1,0.0,0))RETURN
   ENDIF
   IF(.NOT.ASSOCIATED(BOT(I)%X))THEN
    WRITE(*,'(A)') 'Reading '//TRIM(BOT(I)%FNAME)//' ...'
    CALL IDFCOPY(IDF(1),BOT(I))
    IF(.NOT.IDFREADSCALE(BOT(I)%FNAME,BOT(I),2,1,0.0,0))RETURN
   ENDIF
  ENDDO
 ENDIF

 !## idf(1)=cond
 !## idf(2)=stage
 !## idf(3)=bottom
 !## idf(4)=inffct
 N=0
 DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  IF(IDF(1)%X(ICOL,IROW).GT.0.0)THEN
   IF(ILAY.NE.0)THEN
    N=N+1
   ELSE
    WL=IDF(2)%X(ICOL,IROW); BL=IDF(3)%X(ICOL,IROW)
    DO I=1,NLAY
     T=TOP(I)%X(ICOL,IROW); B =BOT(I)%X(ICOL,IROW); IF(I.EQ.1)T=MAX(WL,T)
     IF(WL.GT.B.AND.BL.LT.T)N=N+1
    ENDDO
   ENDIF
  ENDIF
 ENDDO; ENDDO

 MP(1)=MP(1)+N

 IF(JU.EQ.0)THEN
  IU=UTL_GETUNIT()
  IF(GRIDISG%DDATE.EQ.0)THEN
   CALL OSD_OPEN(IU,FILE=TRIM(GRIDISG%ROOT)//'\modflow.riv',STATUS='UNKNOWN',ACTION='WRITE')
   WRITE(IU,'(A)') 'SCD 1 '
  ELSE
   CALL OSD_OPEN(IU,FILE=TRIM(GRIDISG%ROOT)//'\modflow_'//TRIM(JDATETOGDATE(GRIDISG%SDATE,2))//'.riv', &
                 STATUS='UNKNOWN',ACTION='WRITE')
   WRITE(IU,'(A)') 'SCD 1 '//TRIM(JDATETOGDATE(GRIDISG%SDATE,2))//'-'//TRIM(JDATETOGDATE(GRIDISG%EDATE,2))
  ENDIF
  WRITE(IU,'(I10)') N
 ELSE
  IU=JU
 ENDIF

 WRITE(FRM,'(A9,I2.2,A14)') '(3(I5,1X),',4,'(F15.7,1X),I5)'

 DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  IF(IDF(1)%X(ICOL,IROW).GT.0.0)THEN
   !## assign to modellayer 1 by default, when no layers are read in
   IF(ILAY.NE.0)THEN

    WRITE(IU,FRM) ILAY,IROW,ICOL,IDF(2)%X(ICOL,IROW),IDF(1)%X(ICOL,IROW),IDF(3)%X(ICOL,IROW),IDF(4)%X(ICOL,IROW),1

   ELSE
    WL=IDF(2)%X(ICOL,IROW); BL=IDF(3)%X(ICOL,IROW)
    DO I=1,NLAY
     T=TOP(I)%X(ICOL,IROW); B=BOT(I)%X(ICOL,IROW); IF(I.EQ.1)T=MAX(WL,T)
     IF(WL.GT.B.AND.BL.LT.T)THEN
      CD=IDF(1)%X(ICOL,IROW)
      F =(MIN(WL,T)-MAX(BL,B))/(WL-BL)

      WRITE(IU,FRM) I,IROW,ICOL,IDF(2)%X(ICOL,IROW),CD*F,IDF(3)%X(ICOL,IROW),IDF(4)%X(ICOL,IROW),1

     ENDIF
    ENDDO
   ENDIF
  ENDIF
 ENDDO; ENDDO

 IF(JU.EQ.0)CLOSE(IU)

 END SUBROUTINE ISG2GRID_EXPORTRIVER

  !###====================================================================
 SUBROUTINE ISG2GRID_BATHEMETRY(IDF,NIDF,ICROSS,PCROSS,ZCHK,WL,C,INFF)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NIDF
 TYPE(IDFOBJ),DIMENSION(NIDF),INTENT(INOUT) :: IDF
 TYPE(IDFOBJ),INTENT(IN) :: ICROSS,PCROSS
 REAL,INTENT(IN) :: WL,C,INFF,ZCHK
 INTEGER :: IR1,IR2,IC1,IC2,IROW,ICOL,JROW,JCOL
 REAL :: XC,YC,CR

 !## defined cross-sections are finer than model network
 IF(ICROSS%DX.LE.IDF(1)%DX)THEN
  !## spotify cross-section (actual bathemetry of current 2d cross-section) in mother idf
  DO IROW=1,ICROSS%NROW; DO ICOL=1,ICROSS%NCOL
   !## nodata found for waterlevel
   IF(ICROSS%X(ICOL,IROW).EQ.ICROSS%NODATA)CYCLE
   !## waterlevel equal or less than riverbot
   IF(WL.LE.ICROSS%X(ICOL,IROW))CYCLE
   !## if inundation-criterion applied, only inundate if zchk criterion is met
   IF(PCROSS%X(ICOL,IROW).LT.0.0.AND.WL.LE.ZCHK)CYCLE
   !## manipulate resistance
   CR=C*ABS(PCROSS%X(ICOL,IROW))
   CALL IDFGETLOC(ICROSS  ,IROW,ICOL,XC,YC)
   CALL IDFIROWICOL(IDF(1),JROW,JCOL,XC,YC)
   IF(JROW.NE.0.AND.JCOL.NE.0)THEN
    IF(IDF(9)%X(JCOL,JROW).EQ.0.0)THEN
     IDF(1)%X(JCOL,JROW)=IDFGETAREA(ICROSS,ICOL,IROW)/CR
    ELSE
     IDF(1)%X(JCOL,JROW)=IDF(1)%X(JCOL,JROW)+IDFGETAREA(ICROSS,ICOL,IROW)/CR
    ENDIF
    IDF(9)%X(JCOL,JROW)=IDF(9)%X(JCOL,JROW)+1.0 !## counter how many times it passes
    IDF(2)%X(JCOL,JROW)=WL                      !## waterlevel
    IDF(3)%X(JCOL,JROW)=ICROSS%X(ICOL,IROW)     !## bottomlevel
    IDF(4)%X(JCOL,JROW)=INFF                    !## infiltration factor
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
    IF(WL.LE.ICROSS%X(JCOL,JROW))CYCLE
    !## if inundation-criterion applied, only inundate if zchk criterion is met
    IF(PCROSS%X(JCOL,JROW).LT.0.0.AND.WL.LE.ZCHK) CYCLE
    !## manipulate resistance
    CR=C*ABS(PCROSS%X(JCOL,JROW))
    IF(IDF(9)%X(ICOL,IROW).EQ.0.0)THEN
     IDF(1)%X(ICOL,IROW)=IDFGETAREA(ICROSS,ICOL,IROW)/CR
    ELSE
     IDF(1)%X(ICOL,IROW)=IDF(1)%X(JCOL,JROW)+IDFGETAREA(ICROSS,ICOL,IROW)/CR
    ENDIF
!    IDF(1)%X(ICOL,IROW)=IDFGETAREA(ICROSS,ICOL,IROW)/CR
    IDF(2)%X(ICOL,IROW)=WL
    IDF(3)%X(ICOL,IROW)=ICROSS%X(JCOL,JROW)
    IDF(4)%X(ICOL,IROW)=INFF
    IDF(9)%X(ICOL,IROW)=IDF(9)%X(ICOL,IROW)+1.0    !## counter how many times it passes
   ENDIF
  ENDDO; ENDDO
 ENDIF

 END SUBROUTINE ISG2GRID_BATHEMETRY

 !###====================================================================
 SUBROUTINE ISG2GRID_EXTENT_WITH_WIDTH(NIDF,IDF,IBATCH,MAXWIDTH)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NIDF,IBATCH
 REAL,INTENT(IN) :: MAXWIDTH
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(NIDF) :: IDF
 INTEGER :: IROW,ICOL,N,NN,IR,IC,IRR,ICC,I,IRAT,IRAT1
 REAL :: W,L,X,Y,X1,Y1,F
 REAL,ALLOCATABLE,DIMENSION(:,:) :: MM
 REAL,DIMENSION(4) :: V

 !## allocate memory for maximal erosion matrix
 W=0; DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  W=MAX(W,IDF(7)%X(ICOL,IROW))
 ENDDO; ENDDO
 N=(W/IDF(1)%DX)+2; ALLOCATE(MM(N,N)); NN=0

 DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  !## already visited by bathymetry routine - so skip it
  IF(IDF(9)%X(ICOL,IROW).NE.0.0)THEN
   IDF(9)%X(ICOL,IROW)=-1.0*IDF(9)%X(ICOL,IROW)
   CYCLE
  ENDIF
  IF(IDF(1)%X(ICOL,IROW).NE.IDF(1)%NODATA)IDF(9)%X(ICOL,IROW)=1.0
  DO I=1,4; IF(IDF(I)%X(ICOL,IROW).EQ.IDF(I)%NODATA)IDF(I)%X(ICOL,IROW)=0.0; ENDDO
  W=IDF(7)%X(ICOL,IROW)
  W=MIN(W,MAXWIDTH)
  L=IDF(5)%X(ICOL,IROW)
  !## conductance assume to be cell-filled; f=dx/length-segment*dx/width
  IF(W.GT.IDF(1)%DX)THEN
   IDF(1)%X(ICOL,IROW)=IDF(1)%X(ICOL,IROW)*IDF(5)%DX/L*IDF(5)%DX/W
  ENDIF
 ENDDO; ENDDO

 IRAT=0; IRAT1=0
 DO IROW=1,IDF(1)%NROW
  DO ICOL=1,IDF(1)%NCOL
   !## already process by bathymetry
   IF(IDF(9)%X(ICOL,IROW).LE.0.0)CYCLE
   !## river available
   IF(IDF(1)%X(ICOL,IROW).NE.IDF(I)%NODATA)THEN
    !## get mean width
    W=IDF(7)%X(ICOL,IROW)
    W=MIN(W,MAXWIDTH)

    !## current cellsize is less than current width of river
    IF(W.GT.IDF(1)%DX)THEN
     DO I=1,4; V(I)=IDF(I)%X(ICOL,IROW)/IDF(9)%X(ICOL,IROW); ENDDO

     !## create antialiased erosion/fattening matrix
     CALL IDFGETLOC(IDF(1),IROW,ICOL,X,Y)
     CALL ISG2GRID_EROSION_MATRIX(N,NN,MM,W,X,Y,IDF(1)%DX)

     !## apply multiplication matrix
     IF(IROW-NN/2.GE.1.AND.IROW+NN/2.LE.IDF(9)%NROW.AND. &
        ICOL-NN/2.GE.1.AND.ICOL+NN/2.LE.IDF(9)%NCOL)THEN
      IRR=IROW-NN/2
      DO IR=1,NN; ICC=ICOL-NN/2; DO IC=1,NN

       X1=IDF(9)%XMIN+(IDF(9)%DX*(ICC-1))+(0.5*IDF(9)%DX)
       IF(ICC.LT.ICOL)X1=IDF(9)%XMIN+(IDF(9)%DX*ICC-1) !## left  border
       IF(ICC.GT.ICOL)X1=IDF(9)%XMIN+ IDF(9)%DX*ICC    !## right border
       Y1=IDF(9)%YMAX-(IDF(9)%DY*(IRR+1))-(0.5*IDF(9)%DY)
       IF(IRR.LT.IROW)Y1=IDF(9)%YMAX-(IDF(9)%DY*IRR+1) !## top    border
       IF(IRR.GT.IROW)Y1=IDF(9)%YMAX- IDF(9)%DY*IRR    !## bottom border

       F=(X-X1)**2.0+(Y-Y1)**2.0
       IF(F.GT.0.0)F=SQRT(F)
       F=MIN(1.0,(W/2.0)/F)
       !## count for doublicates (assumption)
       F=F**2.0

       IDF(9)%X(ICC,IRR)=IDF(9)%X(ICC,IRR)+F*MM(IR,IC)
       DO I=1,4; IDF(I)%X(ICC,IRR)=IDF(I)%X(ICC,IRR)+V(I)*F*MM(IR,IC); ENDDO
       ICC=ICC+1
      ENDDO; IRR=IRR+1; ENDDO
     ELSE
      IRR=IROW-NN/2
      DO IR=1,NN; ICC=ICOL-NN/2; DO IC=1,NN
       IF(IRR.GE.1.AND.IRR.LE.IDF(9)%NROW.AND. &
          ICC.GE.1.AND.ICC.LE.IDF(9)%NCOL)THEN

        X1=IDF(9)%XMIN+(IDF(9)%DX*(ICC-1))+(0.5*IDF(9)%DX)
        IF(ICC.LT.ICOL)X1=IDF(9)%XMIN+(IDF(9)%DX*ICC-1) !## left  border
        IF(ICC.GT.ICOL)X1=IDF(9)%XMIN+ IDF(9)%DX*ICC    !## right border
        Y1=IDF(9)%YMAX-(IDF(9)%DY*(IRR+1))-(0.5*IDF(9)%DY)
        IF(IRR.LT.IROW)Y1=IDF(9)%YMAX-(IDF(9)%DY*IRR+1) !## top    border
        IF(IRR.GT.IROW)Y1=IDF(9)%YMAX- IDF(9)%DY*IRR    !## bottom border

        F=(X-X1)**2.0+(Y-Y1)**2.0
        IF(F.GT.0.0)F=SQRT(F)
        F=MIN(1.0,(W/2.0)/F)
        !## count for doublicates (assumption)
        F=F**2.0

        IDF(9)%X(ICC,IRR)=IDF(9)%X(ICC,IRR)+F*MM(IR,IC)
        DO I=1,4; IDF(I)%X(ICC,IRR)=IDF(I)%X(ICC,IRR)+V(I)*F*MM(IR,IC); ENDDO
       ENDIF
       ICC=ICC+1
      ENDDO; IRR=IRR+1; ENDDO
     ENDIF

    ENDIF
   ENDIF
  ENDDO
  IF(IBATCH.EQ.0)CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW,IDF(1)%NROW,'Progress computing erosion matrix')
 ENDDO
 IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Finished computing erosion matrix'

 DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  DO I=1,4
   IF(IDF(9)%X(ICOL,IROW).GT.0.0)IDF(I)%X(ICOL,IROW)=IDF(I)%X(ICOL,IROW)/IDF(9)%X(ICOL,IROW)
   IF(IDF(9)%X(ICOL,IROW).EQ.0.0)IDF(I)%X(ICOL,IROW)=IDF(I)%NODATA
  ENDDO
 ENDDO; ENDDO

 !## correct to be sure multiplication matrix does not exceed factor 1.0 and multiply conductance with erosion matrix
 DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  IF(IDF(9)%X(ICOL,IROW).GT.1.0)THEN
   IDF(9)%X(ICOL,IROW)=1.0
  ELSE
   IDF(9)%X(ICOL,IROW)=-1.0*IDF(9)%X(ICOL,IROW)
  ENDIF
 ENDDO; ENDDO

 DEALLOCATE(MM)

 !## clean for conductances le zero
 DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  IF(IDF(1)%X(ICOL,IROW).LE.0.0)THEN
   DO I=1,4; IDF(I)%X(ICOL,IROW)=IDF(I)%NODATA; ENDDO
  ENDIF
 ENDDO; ENDDO

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

 NN=NNN; MM=0.0; RADIUS=(REAL(NN)*DX)/2.0; TRAD=W/2.0

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

 !###====================================================================
 SUBROUTINE ISG2GRIDGETDIMENSION(IDIM,XMIN,YMIN,XMAX,YMAX,NROW,NCOL,CS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDIM
 REAL,INTENT(IN) :: CS
 REAL,INTENT(OUT) :: XMIN,YMIN,XMAX,YMAX
 INTEGER,INTENT(OUT) :: NROW,NCOL
 INTEGER :: I,J,NSEG,ISEG,JSEG
 LOGICAL :: LEX

 SELECT CASE (IDIM)
  CASE (1)    !## current domain
   XMIN=MPW%XMIN
   YMIN=MPW%YMIN
   XMAX=MPW%XMAX
   YMAX=MPW%YMAX
  CASE (2,3)  !## entire isg, current selection within isg
   J=0
   DO I=1,NISG

    LEX=.TRUE.
    IF(IDIM.EQ.3)THEN
     LEX=.FALSE.
     IF(ISG(I)%ILIST.EQ.1)LEX=.TRUE.
    ENDIF

    IF(LEX)THEN
     NSEG=ISG(I)%NSEG
     ISEG=ISG(I)%ISEG
     JSEG=ISEG+NSEG-1
     J   =J+1
     IF(J.EQ.1)THEN
      XMIN=MINVAL(ISP(ISEG:JSEG)%X)
      XMAX=MAXVAL(ISP(ISEG:JSEG)%X)
      YMIN=MINVAL(ISP(ISEG:JSEG)%Y)
      YMAX=MAXVAL(ISP(ISEG:JSEG)%Y)
     ELSE
      XMIN=MIN(XMIN,MINVAL(ISP(ISEG:JSEG)%X))
      XMAX=MAX(XMAX,MAXVAL(ISP(ISEG:JSEG)%X))
      YMIN=MIN(YMIN,MINVAL(ISP(ISEG:JSEG)%Y))
      YMAX=MAX(YMAX,MAXVAL(ISP(ISEG:JSEG)%Y))
     ENDIF
    ENDIF

   ENDDO

   !## increase it a little bit to make sure everything is captured
   XMIN=XMIN-CS
   XMAX=XMAX+CS
   YMIN=YMIN-CS
   YMAX=YMAX+CS

 END SELECT

 CALL UTL_IDFSNAPTOGRID(XMIN,XMAX,YMIN,YMAX,CS,NCOL,NROW)

 END SUBROUTINE ISG2GRIDGETDIMENSION

 !###======================================================================
 SUBROUTINE ISG2GRIDCOMPUTESTUWEN(IPFFNAME,IDFFNAME1,IDFFNAME2)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IPFFNAME,IDFFNAME1,IDFFNAME2
 REAL :: XC,YC,Z,ORIENT,DH
 INTEGER :: I,NSTUW,NC,IROW,ICOL,NTHREAD,DTERM,MAXTHREAD,J,IMENU,IRAT,IRAT1,IU,IOS,MAXN
 INTEGER(KIND=1),POINTER,DIMENSION(:) :: ISPEC
 INTEGER(KIND=2),POINTER,DIMENSION(:,:) :: THREAD,YSEL
 TYPE(IDFOBJ),DIMENSION(:),ALLOCATABLE :: IDF

 IF(ALLOCATED(IDF))DEALLOCATE(IDF)
 ALLOCATE(IDF(2)); DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO

 IF(.NOT.IDFREAD(IDF(1),IDFFNAME1,1))RETURN
 IF(.NOT.IDFREAD(IDF(2),IDFFNAME2,1))RETURN

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=IPFFNAME,STATUS='OLD')
 READ(IU,*) NSTUW
 READ(IU,*) NC
 DO I=1,NC; READ(IU,*); END DO
 READ(IU,*)

 CALL POLYGON1DEALLOCATE_SELIDF(); ALLOCATE(SELIDF(1)); CALL IDFNULLIFY(SELIDF(1))
 CALL IDFCOPY(IDF(1),SELIDF(1))
 IF(.NOT.ASSOCIATED(SELIDF(1)%X))THEN
  ALLOCATE(SELIDF(1)%X(SELIDF(1)%NCOL,SELIDF(1)%NROW))
 ENDIF
 SELIDF(1)%X=SELIDF(1)%NODATA

 MAXTHREAD=1000; MAXN=MAXTHREAD; ALLOCATE(ISPEC(MAXTHREAD),THREAD(3,MAXTHREAD),YSEL(2,MAXTHREAD))

 IMENU=3 !le
! IMENU=2 !lt
 DTERM=0
 IRAT =0

 DO I=1,NSTUW

  READ(IU,*,IOSTAT=IOS) XC,YC,Z,ORIENT,DH
  !## can be less, whenever current window/segment is gridded
  IF(IOS.NE.0)EXIT

  IF(DH.NE.0.0)THEN

   CALL IDFIROWICOL(IDF(1),IROW,ICOL,XC,YC)
   IF(IROW.GT.0.AND.ICOL.GT.0)THEN

    NTHREAD=0; CALL ISG2GRIDCOMPUTESTUWENANGLE(ORIENT,NTHREAD,XC,YC,YSEL,MAXTHREAD,IDF(1))
    !## only whenever current waterlevel is less that structure level
    IF(IDF(1)%X(ICOL,IROW).LT.Z)THEN
     CALL IDFEDITTRACE(IDF(1),SELIDF(1),THREAD,YSEL,ISPEC,DTERM,IMENU,MAXTHREAD,MAXN,Z,NTHREAD,1, &
          X2CRIT=IDF(1)%X(ICOL,IROW)) !-0.1)
    ENDIF
    DO J=1,NTHREAD
     ICOL=INT(YSEL(1,J)); IROW=INT(YSEL(2,J))
     IF(ICOL.GT.0.AND.ICOL.LE.IDF(1)%NCOL.AND. &
        IROW.GT.0.AND.IROW.LE.IDF(1)%NROW)THEN
      IF(SELIDF(1)%X(ICOL,IROW).GT.0)THEN
       IDF(1)%X(ICOL,IROW)=Z
       IDF(2)%X(ICOL,IROW)=REAL(I)
      ENDIF
      SELIDF(1)%X(ICOL,IROW)=SELIDF(1)%NODATA
     ENDIF
    END DO

    CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,NSTUW,'Progress searching structures ')

   ENDIF
  ENDIF
 ENDDO

 IF(.NOT.IDFWRITE(IDF(1),IDFFNAME1,1))RETURN
 IF(.NOT.IDFWRITE(IDF(2),IDFFNAME2,1))RETURN

 CALL IDFDEALLOCATE(IDF,SIZE(IDF))
 CALL POLYGON1DEALLOCATE_SELIDF()

 CLOSE(IU)

 DEALLOCATE(ISPEC,THREAD,YSEL,IDF)

 END SUBROUTINE ISG2GRIDCOMPUTESTUWEN

 !###===============================================================================
 SUBROUTINE ISG2GRIDCOMPUTE_GETXYORIENT(IISG,IIST,X,Y,ANGLE)
 !###===============================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: R2G=360.0/(2.0*3.1415)  !1 rad = 57.15 degrees
 REAL,INTENT(OUT) :: X,Y,ANGLE
 INTEGER,INTENT(IN) :: IISG,IIST
 INTEGER :: I,J
 REAL :: DXY,F,TD

 TD=0.0
 J =ISG(IISG)%ISEG
 DO I=2,ISG(IISG)%NSEG
  J=J+1
  DXY=((ISP(J)%X-ISP(J-1)%X)**2.0)+((ISP(J)%Y-ISP(J-1)%Y)**2.0)
  IF(DXY.GT.0.0)DXY=SQRT(DXY)
  TD=TD+DXY
  IF(TD.GE.IST(IIST)%DIST)EXIT
 END DO

 !## distance current segment
 F   = (IST(IIST)%DIST-(TD-DXY))/DXY
 X   =  ISP(J-1)%X+(ISP(J)%X-ISP(J-1)%X)*F
 Y   =  ISP(J-1)%Y+(ISP(J)%Y-ISP(J-1)%Y)*F
 ANGLE= ATAN2(ISP(J)%Y-ISP(J-1)%Y,ISP(J)%X-ISP(J-1)%X)
 ANGLE= R2G*ANGLE

 END SUBROUTINE ISG2GRIDCOMPUTE_GETXYORIENT

 !###===============================================================================
 REAL FUNCTION ISG2GRIDSTUWEN_GETORIENT(OR)
 !###===============================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: OR

 ISG2GRIDSTUWEN_GETORIENT=OR
 DO
  IF(ISG2GRIDSTUWEN_GETORIENT.LT.0.0)  ISG2GRIDSTUWEN_GETORIENT=OR+360.0
  IF(ISG2GRIDSTUWEN_GETORIENT.GT.360.0)ISG2GRIDSTUWEN_GETORIENT=OR-360.0
  IF(ISG2GRIDSTUWEN_GETORIENT.GE.0.0.AND.ISG2GRIDSTUWEN_GETORIENT.LE.360.0)EXIT
 ENDDO

 END FUNCTION ISG2GRIDSTUWEN_GETORIENT

 !###======================================================================
 SUBROUTINE ISG2GRIDCOMPUTESTUWENANGLE(ORIENT,NTHREAD,XC,YC,YSEL,MAXTHREAD,IDF)
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: GRAD =360.0/(2.0*3.1415)
 REAL,INTENT(IN) :: ORIENT,XC,YC
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(INOUT) :: NTHREAD
 INTEGER,INTENT(IN) :: MAXTHREAD
 INTEGER(KIND=2),DIMENSION(2,MAXTHREAD) :: YSEL
 REAL :: OR,X,Y,DXY
 INTEGER :: ICOL,IROW

 DXY=0.5*(IDF%DX+IDF%DY)
 DXY=1.0*(IDF%DX+IDF%DY)

 !## block correct side of structure
 OR=90.0
 DO
  OR=OR+5.0; IF(OR.GT.270.0)EXIT
  X=XC-COS((ORIENT-OR)/GRAD)*DXY
  Y=YC-SIN((ORIENT-OR)/GRAD)*DXY
  CALL IDFIROWICOL(IDF,IROW,ICOL,X,Y)
  IF(IROW.GT.0.AND.ICOL.GT.0)THEN
   !## been there ... artificially
   IF(IDF%X(ICOL,IROW).NE.IDF%NODATA)THEN
    SELIDF(1)%X(ICOL,IROW)=-1.0; NTHREAD=NTHREAD+1; YSEL(1,NTHREAD)=INT(ICOL,2); YSEL(2,NTHREAD)=INT(IROW,2)
   ENDIF
  ENDIF
 END DO

 !## begin position
 CALL IDFIROWICOL(IDF,IROW,ICOL,XC,YC)
 NTHREAD=NTHREAD+1; YSEL(1,NTHREAD)=INT(ICOL,2); YSEL(2,NTHREAD)=INT(IROW,2); SELIDF(1)%X(ICOL,IROW)=1.0

 END SUBROUTINE ISG2GRIDCOMPUTESTUWENANGLE

 !###======================================================================
 SUBROUTINE ISG2GRIDPERIMETERTRAPEZIUM(XTRAP,YTRAP,WETPER,CT,BW,WDEPTH)
 !###======================================================================
 IMPLICIT NONE
 REAL,DIMENSION(4),INTENT(IN) :: XTRAP,YTRAP
 REAL,INTENT(IN) :: WDEPTH
 REAL,INTENT(OUT) :: WETPER,BW,CT
 REAL :: DX,DY

 BW=XTRAP(3)-XTRAP(4)
 DX=XTRAP(2)-XTRAP(3)
 DY=YTRAP(2)-YTRAP(3)

 IF(DY.LE.0.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'ERROR DY.LE.0.0 in ISG2GRIDPERIMETERTRAPEZIUM() !!!'//CHAR(13)// &
   'iMOD will be terminated, call 030-2564766','Error')
 !## cotanges
 CT=DX/DY

 DX=0.0
 IF(CT.NE.0.0)DX=WDEPTH*CT !/(1.0/CT)
 WETPER=SQRT((DX**2.0)+(WDEPTH**2.0))
 WETPER=WETPER*2.0+BW

 END SUBROUTINE ISG2GRIDPERIMETERTRAPEZIUM

 !###====================================================================
 SUBROUTINE ISG2GRIDGETDATA(SDATE,EDATE,TTIME,QSORT,XNR,NITEMS,RVAL, &
                            NR,IREF,ISS,ITYPE,NDATA,IAVERAGE)
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(IN),DIMENSION(NITEMS) :: NDATA
 INTEGER,INTENT(IN) :: SDATE,EDATE,TTIME,NITEMS,NR,IREF,ISS,ITYPE,IAVERAGE
 REAL,INTENT(INOUT),DIMENSION(TTIME,NITEMS) :: QSORT
 REAL,DIMENSION(NITEMS),INTENT(OUT) :: XNR
 REAL,DIMENSION(NITEMS),INTENT(INOUT) :: RVAL
 INTEGER :: IR,N,I,J,I1,I2,IDATE,NAJ,IREC,NDATE,IH,IM,IS

 IREC=IREF-1

 IF(NR.LE.0)RETURN

 IF(ISS.EQ.1)QSORT=0.0
 IF(ISS.EQ.2)THEN; DO I=1,TTIME; DO J=1,NITEMS; QSORT(I,J)=NDATA(J); ENDDO; ENDDO; ENDIF
 I1   = 1

 XNR=0.0
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
   ENDIF
  ELSEIF(ITYPE.EQ.-1)THEN
   IDATE  =DATIST(IREC)%IDATE
   RVAL(1)=DATIST(IREC)%WLVL_UP
   RVAL(2)=DATIST(IREC)%WLVL_DOWN
   RVAL(3)=0.0
   RVAL(4)=0.0
  ENDIF

  !## don't bother for steady-state, take the mean!
  IF(ISS.EQ.1)THEN
   DO I=1,NITEMS; IF(RVAL(I).NE.NDATA(I))THEN; QSORT(1,I)=QSORT(1,I)+RVAL(I); XNR(I)=XNR(I)+1.0; ENDIF; ENDDO
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
      QSORT(MAX(1,I1):I2,I)=RVAL(I)
     END DO
    ENDIF

    I1=I2+1

   ENDIF

  ENDIF

 END DO

 IF(ISS.EQ.1)THEN
  !## determine for each period appropriate attribute term
  DO I=1,NITEMS
   IF(XNR(I).GT.0.0)THEN
    RVAL(I)=QSORT(1,I)/REAL(XNR(I))
   ELSE
    RVAL(I)=NDATA(I)
   ENDIF
  ENDDO
 !## take the mean (better than median)
 ELSEIF(ISS.EQ.2)THEN
  !## arithmetic mean
  IF(IAVERAGE.EQ.1)THEN
   DO I=1,NITEMS
    XNR(I)=0.0; RVAL(I)=0.0
    DO J=1,TTIME
     IF(QSORT(J,I).NE.NDATA(I))THEN
      RVAL(I)=RVAL(I)+QSORT(J,I)
      XNR(I) =XNR(I)+1.0
     ENDIF
    ENDDO
    IF(XNR(I).GT.0.0)THEN
     RVAL(I)=RVAL(I)/XNR(I)
    ELSE
     RVAL(I)=NDATA(I)
    ENDIF
   ENDDO
  !## median - exclude nodata
  ELSEIF(IAVERAGE.EQ.2)THEN
   DO I=1,NITEMS
    CALL UTL_GETMED(QSORT(:,I),TTIME,NDATA(I),(/50.0/),1,NAJ,RVAL(I))
   ENDDO
  ENDIF
 ENDIF

 END SUBROUTINE ISG2GRIDGETDATA

 !###====================================================================
 SUBROUTINE ISG2GRIDGETPARAM(DISTANCE,BOTTOM,N,BH,WP,RWIDTH,WETPER,MINDEPTH)
 !###====================================================================
 !RE.: In case waterlevel exceeds cross-section, rwidth and wetper will
 !     yield max. values based upon cross-section information only!
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL,INTENT(IN),DIMENSION(N) :: DISTANCE,BOTTOM
 REAL,INTENT(IN) :: BH,WP,MINDEPTH
 REAL,INTENT(OUT) :: RWIDTH,WETPER
 INTEGER :: I !,ISEG
 REAL :: DISTB,DISTW,FACTOR,X1,X2,WPCOR,X

 WPCOR =WP-BH !## waterdepth interpolated
 WETPER=0.0
 RWIDTH=0.0

 !## riverbed is dry - make it minimal equal to mindepth
 WPCOR=MAX(MINDEPTH,WPCOR)

 !## find left-x-coordinate - downgoing direction
 X1=DISTANCE(N)
 DO I=1,N-1
  IF(BOTTOM(I).GE.WPCOR.AND.BOTTOM(I+1).LE.WPCOR)THEN
   DISTB  = BOTTOM(I)-BOTTOM(I+1)
   DISTW  = BOTTOM(I)-WPCOR
   FACTOR=1.0; IF(DISTB.NE.0.0)FACTOR=DISTW/DISTB
   X =FACTOR*(DISTANCE(I+1)-DISTANCE(I))
   X =DISTANCE(I)+X
   X1=MIN(X1,X)
   !## wetted perimeter
   FACTOR =(X-DISTANCE(I+1))**2.0+(WPCOR-BOTTOM(I+1))**2.0
   IF(FACTOR.NE.0.0)FACTOR=SQRT(FACTOR)
   WETPER=WETPER+FACTOR
  ENDIF
 END DO
 !## cross-section not high enough left
 IF(X1.EQ.DISTANCE(N))X1=DISTANCE(1)

 !## find right-x-coordinate - upwards direction
 X2=DISTANCE(1)
 DO I=N-1,1,-1
  IF(BOTTOM(I+1).GE.WPCOR.AND.BOTTOM(I).LE.WPCOR)THEN
   DISTB  = BOTTOM(I+1)-BOTTOM(I)
   DISTW  = BOTTOM(I+1)-WPCOR
   FACTOR=1.0; IF(DISTB.NE.0.0)FACTOR=DISTW/DISTB
   X =FACTOR*(DISTANCE(I+1)-DISTANCE(I))
   X =DISTANCE(I+1)-X
   X2=MAX(X2,X)
   !## wetted perimeter
   FACTOR =(X-DISTANCE(I))**2.0+(WPCOR-BOTTOM(I))**2.0
   IF(FACTOR.NE.0.0)FACTOR=SQRT(FACTOR)
   WETPER=WETPER+FACTOR
  ENDIF
 END DO
 !## cross-section not high enough right
 IF(X2.EQ.DISTANCE(1))X2=DISTANCE(N)

 !## proces wetted perimeter for 'in-between' sections
 DO I=1,N-1
  IF(BOTTOM(I).LT.WPCOR.AND.BOTTOM(I+1).LT.WPCOR)THEN
   !## wetted perimeter
   FACTOR=(DISTANCE(I)-DISTANCE(I+1))**2.0+(BOTTOM(I)-BOTTOM(I+1))**2.0
   IF(FACTOR.NE.0.0)FACTOR=SQRT(FACTOR)
   WETPER=WETPER+FACTOR
  ENDIF
 END DO

 RWIDTH=X2-X1

 END SUBROUTINE ISG2GRIDGETPARAM

 !###====================================================================
 SUBROUTINE ISG2GRIDINTSEGMENT(X,Y,DIST,NSEG,MAXNSEG)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NSEG,MAXNSEG
 REAL,INTENT(IN),DIMENSION(MAXNSEG) :: X,Y
 REAL,INTENT(OUT),DIMENSION(MAXNSEG) :: DIST
 INTEGER :: ISEG
 REAL :: DXY

 DIST=0.0
 DO ISEG=2,NSEG
  DXY=UTL_DIST(X(ISEG),Y(ISEG),X(ISEG-1),Y(ISEG-1))
!  DXY=((X(ISEG)-X(ISEG-1))**2.0)+((Y(ISEG)-Y(ISEG-1))**2.0)
!  IF(DXY.GT.0.0)DXY=SQRT(DXY)
  DIST(ISEG)=DIST(ISEG-1)+DXY
 END DO

 END SUBROUTINE ISG2GRIDINTSEGMENT

 !###====================================================================
 SUBROUTINE ISG2GRIDGETCROSS(JCRS,ICRS,NCRS,ISGLEN)
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: ISGLEN
 INTEGER,INTENT(IN) :: NCRS,ICRS
 INTEGER,INTENT(OUT) :: JCRS
 INTEGER :: IC1,IC2,ISEG,I
 REAL :: DCL,DCR,D

 JCRS=0

 IF(NCRS.EQ.1)THEN; JCRS=1; RETURN; ENDIF

 IC1 =0  !cross-section left
 IC2 =0  !cross-section right
 DCL =10.0E10  !left
 DCR =10.0E10  !right
 ISEG=ICRS-1
 DO I=1,NCRS
  ISEG=ISEG+1
  !## take 1d cross-sections only
  IF(ISC(ISEG)%N.GT.0)THEN
   D   =ISC(ISEG)%DIST-ISGLEN
   IF(D.GT.0)THEN !## lies after
    !## take whenever closer than next cross-section in the front
    IF(D.LE.DCL)THEN
     DCL=D
     IC2=I
    ENDIF
   ELSE           !## lies behind
    !## take whenever closer than previous cross-section in the back
    IF(ABS(D).LE.DCR)THEN
     DCR=ABS(D)
     IC1=I
    ENDIF
   ENDIF
  ENDIF
 ENDDO
 !## only use cross-section in front if nothing found in the back
 IF(IC1.EQ.0)IC1=IC2
 JCRS=IC1

 END SUBROUTINE ISG2GRIDGETCROSS

 !###====================================================================
 SUBROUTINE ISG2GRIDGETQHR(JQHR,IQHR,NQHR,ISGLEN)
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: ISGLEN
 INTEGER,INTENT(IN) :: NQHR,IQHR
 INTEGER,INTENT(OUT) :: JQHR
 INTEGER :: ISEG,I
 REAL :: D,MAXD

 JQHR=1

 IF(NQHR.EQ.1)RETURN

 !#take qh-point nearest by
 MAXD=10.0E10
 ISEG=IQHR-1
 DO I=1,NQHR
  ISEG=ISEG+1
  D   =ABS(ISQ(ISEG)%DIST-ISGLEN)
  IF(D.LT.MAXD)THEN
   MAXD=D
   JQHR=I
  ENDIF
 END DO

 END SUBROUTINE ISG2GRIDGETQHR

 !###====================================================================
 SUBROUTINE ISG2GRIDINCLUDECLCNODES(ITYP,NTYP,NSEG,MAXNSEG,X,Y,DIST,IPOS,ITYPE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYP,NTYP,MAXNSEG,ITYPE
 INTEGER,INTENT(INOUT) :: NSEG
 REAL,INTENT(INOUT),DIMENSION(MAXNSEG) :: DIST,X,Y
 INTEGER,INTENT(OUT),DIMENSION(MAXNSEG) :: IPOS
 INTEGER :: IREC,ISEG,I,II,JJ
 REAL :: DXY,D1,D2,F,XC,YC

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
  F =0.0
  IF(D1.NE.0.0)F=D2/D1

  !## put in extra coordinate
  IF(F.LE.0.0.AND.ITYPE.EQ.1)THEN
   !## put data to current node
   IPOS(ISEG-1)=I*ITYPE
  ELSEIF(F.GE.1.0.AND.ITYPE.EQ.1)THEN
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
!   X(ISEG+1:NSEG+1)   =X(ISEG:NSEG)
!   Y(ISEG+1:NSEG+1)   =Y(ISEG:NSEG)
!   IPOS(ISEG+1:NSEG+1)=IPOS(ISEG:NSEG)
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
!   X(ISEG+1:NSEG+1)   =X(ISEG:NSEG)
!   Y(ISEG+1:NSEG+1)   =Y(ISEG:NSEG)
!   IPOS(ISEG+1:NSEG+1)=IPOS(ISEG:NSEG)
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

END MODULE MOD_ISG_GRID

