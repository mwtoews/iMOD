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
USE MOD_INTERSECT_PAR, ONLY : XA,YA,LN
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFWRITE,IDFALLOCATEX,IDFDEALLOCATE,IDFREADDIM,IDFREAD,IDFNULLIFY,IDFIROWICOL,IDFPUTVAL,IDFGETXYVAL,IDFCOPY,IDFGETLOC, &
                    IDFREADSCALE,IDFGETAREA,IDF_EXTENT,IDFDEALLOCATEX
USE MODPLOT
USE MOD_ISG_PAR
USE MOD_UTL, ONLY : ITOS,RTOS,UTL_IDATETOJDATE,UTL_JDATETOIDATE,JDATETOGDATE,UTL_GETUNIT,UTL_WAITMESSAGE,UTL_IDFSNAPTOGRID,UTL_GETMED, &
               PEUCKER_SIMPLIFYLINE
USE MOD_OSD, ONLY : OSD_OPEN,OSD_TIMER,ICF
USE MOD_IDFEDIT_TRACE, ONLY : IDFEDITTRACE
USE MOD_ISG_TRAPEZIUM, ONLY : ISGCOMPUTETRAPEZIUM
USE MOD_ISG_ADJ, ONLY : ISGADJUSTCOMPUTEXY
USE MOD_ISG_UTL, ONLY : UTL_GETUNITSISG,ISGREAD,ISGDEAL,ISGDELISD,ISGSTUWEN_INTERSECT,ISGGETPOSID,ISGMEMORYISC,ISGMEMORYDATISC,ISGDELISC,ISGGETXY, &
             ISGATTRIBUTESREADISCVALUE,ISGATTRIBUTES_2DCROSS_READ,ISGMEMORYDATISD
USE MOD_POLYGON_UTL, ONLY : POLYGON1DEALLOCATE_SELIDF
USE MOD_POLYGON_PAR, ONLY : SELIDF
USE MOD_IPF_PAR, ONLY : NIPF,IPF
USE MOD_IPF, ONLY : IPFREAD2,IPFALLOCATE,IPFDEALLOCATE

CONTAINS

 !##=====================================================================
 SUBROUTINE ISG_EXPORT(EXPORTFNAME,IEXPORT)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: EXPORTFNAME
 INTEGER,INTENT(IN) :: IEXPORT
 INTEGER :: IU,NPNT,IPNT,NCLC,ICLC,J,ICRS,NCRS
 REAL :: DIST,TD 
  
 NISGFILES=1; IF(ALLOCATED(ISGIU))DEALLOCATE(ISGIU); ALLOCATE(ISGIU(MAXFILES,NISGFILES))
 CALL UTL_GETUNITSISG(ISGIU,ISGFNAME,'OLD'); IF(MINVAL(ISGIU).LE.0)RETURN

 !## read entire ISG file
 CALL ISGREAD()
     
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
 SUBROUTINE ISG_ADDSTAGES(IPFFILE)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IPFFILE
 INTEGER :: IPNT,NPNT,ICLC,NCLC,I,J,K,IPOS,ISEG,N,ID,NTXT,MTXT,IU,IREF
 REAL :: MD,D,DIST,TD,DX,INFF,BTML,RESIS,STAGE
 CHARACTER(LEN=256) :: FNAME
 CHARACTER(LEN=14) :: CDATE
 TYPE FTXTOBJ
  INTEGER,POINTER,DIMENSION(:) :: IDATE
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

 NISGFILES=1
 IF(ALLOCATED(ISGIU))DEALLOCATE(ISGIU); ALLOCATE(ISGIU(MAXFILES,NISGFILES))
 CALL UTL_GETUNITSISG(ISGIU,ISGFNAME,'OLD')
 IF(MINVAL(ISGIU).LE.0)RETURN
 !## read entire ISG file
 CALL ISGREAD()
     
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
   ALLOCATE(FTXT%IDATE(NTXT),FTXT%STAGE(NTXT),FTXT%LABEL(MTXT),FTXT%NODATA(MTXT))
   DO I=1,MTXT; READ(IU,*) FTXT%LABEL(I),FTXT%NODATA(I); ENDDO
   K=0
   DO I=1,NTXT
    READ(IU,*) CDATE,STAGE
    IF(STAGE.NE.FTXT%NODATA(2))THEN
     K=K+1
     READ(CDATE,'(I8)') FTXT%IDATE(K)
     FTXT%STAGE(K)=STAGE
    ENDIF
   ENDDO
   NTXT=K
   CLOSE(IU)

   !## get location of calculation node
   IPOS=ISG(ISELISG)%ICLC-1+J

   !## get available date TO start insertion
   IREF=ISD(IPOS)%IREF-1
   DO I=1,ISD(IPOS)%N
    IREF=IREF+1
    IF(DATISD(IREF)%IDATE.GE.FTXT%IDATE(1))EXIT
   ENDDO

   !## number to be re-used
   N=1+(ISD(IPOS)%N-I)

   !## add number of extra records to it
   N=NTXT-N
   
   !## increase/decrease memory data calculation node - data will be replaced to the back
   CALL ISGMEMORYDATISD(N,IPOS,ISEG)
!   ISD(IPOS)%N=ISD(IPOS)%N+1

   !## rewrite data
   IREF=ISEG-1
   DO I=1,ISD(IPOS)%N-NTXT
    IREF=IREF+1
    DATISD(IREF)=DATISD(IREF+NTXT)
!    DATISD(IREF)%WLVL=DATISD(IREF+NTXT)%WLVL
   ENDDO

   !## store latest information
   BTML =DATISD(IREF)%BTML
   RESIS=DATISD(IREF)%RESIS
   INFF =DATISD(IREF)%INFF

   DO I=1,NTXT
    IREF=IREF+1    
    DATISD(IREF)%IDATE=FTXT%IDATE(I)
    DATISD(IREF)%WLVL =FTXT%STAGE(I)
    DATISD(IREF)%BTML =BTML
    DATISD(IREF)%RESIS=RESIS
    DATISD(IREF)%INFF =INFF
   ENDDO
    
   DEALLOCATE(FTXT%LABEL,FTXT%NODATA,FTXT%STAGE,FTXT%IDATE)
 
  ENDDO
 ENDDO 
 
 CALL IPFDEALLOCATE()
 
 END SUBROUTINE ISG_ADDSTAGES
 
 !##=====================================================================
 SUBROUTINE ISG_ADDCROSSSECTION(FNAME,WIDTHFNAME,MAXDIST,CROSS_PNTR,CROSS_BATH,CELL_SIZE)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME,WIDTHFNAME,CROSS_PNTR,CROSS_BATH
 REAL,INTENT(IN) :: MAXDIST,CELL_SIZE
 INTEGER :: IU,IOS,N,I,J,IISG,IPOS,ISEG,NSEG,ICRS,NCRS,IPNT,NPNT,IROW,ICOL,IP
 CHARACTER(LEN=1000) :: STRING
 REAL,DIMENSION(1000) :: X
 REAL :: XCRD,YCRD,TDIST,DIST,W,TD,XC,YC,ZVAL
 CHARACTER(LEN=MAXLEN) :: LABEL
 TYPE(IDFOBJ) :: IDF
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: ICROSS
 
 NISGFILES=1
 IF(ALLOCATED(ISGIU))DEALLOCATE(ISGIU); ALLOCATE(ISGIU(MAXFILES,NISGFILES))
 CALL UTL_GETUNITSISG(ISGIU,ISGFNAME,'OLD')
 IF(MINVAL(ISGIU).LE.0)RETURN
 !## read entire ISG file
 CALL ISGREAD()

 IF(CROSS_PNTR.EQ.'')THEN
  IF(.NOT.IDFREAD(IDF,WIDTHFNAME,0))STOP 'CAN NOT READ WIDTHFNAME' 
 
  !## remove all cross-sections on segment
  WRITE(*,'(/A/)') 'Removing all cross-sections ...'

  DO ISELISG=1,NISG
   ICRS=ISG(ISELISG)%ICRS; NCRS=ISG(ISELISG)%NCRS
   DO I=1,NCRS; CALL ISGDELISC(ISELISG,ICRS); END DO
  ENDDO
  ISC%N=0; ISC%IREF=0; ISC%DIST=0.0; ISC%CNAME=''
 
  WRITE(*,'(/A/)') 'Adding cross-sections from '//TRIM(FNAME)

  IU=UTL_GETUNIT()
  CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ')
  DO 
   READ(IU,'(A1000)',IOSTAT=IOS) STRING; IF(IOS.NE.0)EXIT; IF(LEN_TRIM(STRING).EQ.0)EXIT
   N=6; DO
    READ(STRING,*,IOSTAT=IOS) XCRD,YCRD,LABEL,(X(I),I=1,N); IF(IOS.NE.0)EXIT; N=N+2
   ENDDO; N=N-2
   IF(N.LE.4)THEN
    WRITE(*,*) XCRD,YCRD,TRIM(LABEL)
    DO I=1,N/2; WRITE(*,*) X(I),X(I+N/2); ENDDO; STOP 'N.LE.4'
   ENDIF
 
   !## find distance of point on segment iisg that is closest
   CALL ISGSTUWEN_INTERSECT(MAXDIST,XCRD,YCRD,IISG,TDIST)

   !## segment found within maxdist
   IF(TDIST.GT.0.0)THEN
    ISELISG=IISG

    N=N/2
   
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
     DATISC(ISEG+I-1)%BOTTOM  =X(I+N)
     DATISC(ISEG+I-1)%KM      =25.0
    ENDDO
   
   ENDIF
  ENDDO
 
  CLOSE(IU)
 
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
    DATISC(ISEG)%KM        =25.0
    DATISC(ISEG+1)%DISTANCE=-W/2.0
    DATISC(ISEG+1)%BOTTOM  = 0.0
    DATISC(ISEG+1)%KM      =25.0
    DATISC(ISEG+2)%DISTANCE= W/2.0
    DATISC(ISEG+2)%BOTTOM  = 0.0
    DATISC(ISEG+2)%KM      =25.0
    DATISC(ISEG+3)%DISTANCE= W/2.0
    DATISC(ISEG+3)%BOTTOM  = 5.0
    DATISC(ISEG+3)%KM      =25.0
   ENDIF
  ENDDO
 
 ELSE
 
  ALLOCATE(ICROSS(3)); DO I=1,SIZE(ICROSS); CALL IDFNULLIFY(ICROSS(I)); ENDDO
  !## select finest resolution
  IF(.NOT.IDFREAD(ICROSS(1),CROSS_PNTR,0))RETURN
  IF(.NOT.IDFREAD(ICROSS(2),CROSS_BATH,0))RETURN    
  CLOSE(ICROSS(1)%IU); CLOSE(ICROSS(2)%IU)
  IF(.NOT.IDF_EXTENT(2,ICROSS,ICROSS(3),2))RETURN
  
  IF(CELL_SIZE.NE.0.0)THEN
   ICROSS(3)%DX=CELL_SIZE; ICROSS(3)%DY=ICROSS(3)%DX
   CALL UTL_IDFSNAPTOGRID(ICROSS(3)%XMIN,ICROSS(3)%XMAX,ICROSS(3)%YMIN,ICROSS(3)%YMAX,ICROSS(3)%DX,ICROSS(3)%NCOL,ICROSS(3)%NROW)
  ENDIF
  
  !## read pointer
  CALL IDFCOPY(ICROSS(3),ICROSS(1))
  IF(.NOT.IDFREADSCALE(CROSS_PNTR,ICROSS(1),7,0,0.0,0))RETURN !## most frequent occurence
  !## read zval at pointer scale
  CALL IDFCOPY(ICROSS(3),ICROSS(2))
  IF(.NOT.IDFREADSCALE(CROSS_BATH,ICROSS(2),2,1,0.0,0))RETURN
   
  DO ISELISG=1,NISG

   !## number of nodes on segment
   NPNT=ISG(ISELISG)%NSEG; IPNT=ISG(ISELISG)%ISEG
   ICRS=ISG(ISELISG)%ICRS; NCRS=ISG(ISELISG)%NCRS
   
   DO J=1,NCRS

    DIST=ISC(ICRS+J-1)%DIST
    !## compute correct x/y coordinate of current cross-section
    CALL ISGADJUSTCOMPUTEXY(IPNT,NPNT,DIST,TD)
    !## get irow/icol for current location of cross-section
    CALL IDFIROWICOL(ICROSS(1),IROW,ICOL,ISGX,ISGY)
    IF(IROW.NE.0.AND.ICOL.NE.0)THEN
     IP=ICROSS(1)%X(ICOL,IROW)
     IF(IP.NE.ICROSS(1)%NODATA.AND.IP.GT.0)THEN
      !## get number of cross-section locations
      N=0; DO IROW=1,ICROSS(1)%NROW; DO ICOL=1,ICROSS(1)%NCOL
       !## location of gridcell equal to pointer value at location of cross-section
       IF(ICROSS(1)%X(ICOL,IROW).EQ.IP)THEN
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
      
      N=1
      DATISC(ISEG+N-1)%DISTANCE=ICROSS(1)%DX
      DATISC(ISEG+N-1)%BOTTOM  =ICROSS(1)%DY
      DATISC(ISEG+N-1)%KM      =0.0 !## empty, not to be used (yet)

      DO IROW=1,ICROSS(1)%NROW; DO ICOL=1,ICROSS(1)%NCOL
       !## location of gridcell equal to pointer value at location of cross-section
       IF(ICROSS(1)%X(ICOL,IROW).EQ.IP)THEN
        IF(ICROSS(2)%X(ICOL,IROW).NE.ICROSS(2)%NODATA)THEN
         N=N+1
         CALL IDFGETLOC(ICROSS(1),IROW,ICOL,XC,YC)
         DATISC(ISEG+N-1)%DISTANCE=XC
         DATISC(ISEG+N-1)%BOTTOM  =YC
         DATISC(ISEG+N-1)%KM      =ICROSS(2)%X(ICOL,IROW)
        ENDIF
       ENDIF
      ENDDO; ENDDO
    
     ELSE
      WRITE(*,'(A,I10)') 'Pointer value is equal to nodata value/or le 0, IP=',IP
     ENDIF
    ELSE
     WRITE(*,'(A)') 'Position of cross-section is outside Pointer IDF'
    ENDIF
   ENDDO
  ENDDO
  CALL IDFDEALLOCATE(ICROSS,SIZE(ICROSS)); DEALLOCATE(ICROSS)
 ENDIF 
 
 END SUBROUTINE ISG_ADDCROSSSECTION
 
 !##=====================================================================
 SUBROUTINE ISG_SIMPLIFYMAIN(ZTOLERANCE,NODATA)
 !##=====================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: ZTOLERANCE,NODATA
 INTEGER :: I,J,K,ITYPE,ISS,IREF,NCLC
 REAL,DIMENSION(4) :: RVAL,XNR
 REAL,DIMENSION(1,4) :: QSORT
 REAL,ALLOCATABLE,DIMENSION(:) :: ZDIST,XDIST,GCODE
 INTEGER,ALLOCATABLE,DIMENSION(:) :: ILIST
   
 NISGFILES=1
 IF(ALLOCATED(ISGIU))DEALLOCATE(ISGIU); ALLOCATE(ISGIU(MAXFILES,NISGFILES))
 CALL UTL_GETUNITSISG(ISGIU,ISGFNAME,'OLD')
 IF(MINVAL(ISGIU).LE.0)RETURN
 !## read entire ISG file
 CALL ISGREAD()

 ITYPE=1 !## itype=1: isd
 ISS=1   !## mean value
 NCLC=MAXVAL(ISG%NCLC); ALLOCATE(ZDIST(NCLC),XDIST(NCLC),GCODE(NCLC),ILIST(NCLC))
 
 !## process each segment in ISG file to simplify calculation points
 DO I=1,NISG
  
  !## get mean waterlevels in segment
  IREF=ISG(I)%ICLC-1; NCLC=ISG(I)%NCLC
  DO J=1,NCLC
   IREF=IREF+1
   CALL ISG2GRIDGETDATA(0,0,1,QSORT,XNR,4,RVAL,ISD(IREF)%N,ISD(IREF)%IREF,ISS,ITYPE,NODATA) 
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
  CALL PEUCKER_SIMPLIFYLINE(XDIST,ZDIST,GCODE,K) !ISG(I)%NCLC)
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
 LOGICAL FUNCTION ISG2GRIDMAIN(IBATCH,NLAY,TOP,BOT) 
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH,NLAY
 TYPE(IDFOBJ),DIMENSION(NLAY),INTENT(INOUT) :: TOP,BOT
 INTEGER :: NROW,NCOL,IU,SIMDATE1,SIMDATE2,SIMDATE3,MP
 CHARACTER(LEN=52) :: PPOSTFIX 
 LOGICAL :: LEX
 
 ISG2GRIDMAIN=.FALSE.
 
 IF(IBATCH.EQ.1)THEN
  !## deallocate memory
  CALL ISGDEAL()
  NISGFILES=1
  IF(ALLOCATED(ISGIU))DEALLOCATE(ISGIU); ALLOCATE(ISGIU(MAXFILES,NISGFILES))
  CALL UTL_GETUNITSISG(ISGIU(:,1),ISGFNAME,'OLD')
  IF(MINVAL(ISGIU(:,1)).LE.0)STOP 'error opening files'
  WRITE(*,'(/1X,A/)') 'Opened '//TRIM(ISGFNAME)//'...'
  WRITE(*,'(/1X,A/)') 'Reading ISG files ...'
  CALL ISGREAD()
 ENDIF

 CALL ISG2GRIDGETDIMENSION(IDIM,XMIN,YMIN,XMAX,YMAX,NROW,NCOL,CS)
 CALL UTL_IDFSNAPTOGRID(XMIN,XMAX,YMIN,YMAX,CS,NCOL,NROW)

 SIMDATE1=UTL_IDATETOJDATE(SDATE) !## begin juliandate
 SIMDATE3=UTL_IDATETOJDATE(EDATE) !## eind  juliandate
 DO

  IF(DDATE.EQ.0)THEN
   SIMDATE2=SIMDATE3
  ELSE
   SIMDATE2=SIMDATE1+MAX(0,DDATE-1) 
  ENDIF
  IF(SIMDATE2.GT.SIMDATE3.AND.ISS.EQ.2)EXIT

  SDATE=UTL_JDATETOIDATE(SIMDATE1)
  EDATE=UTL_JDATETOIDATE(SIMDATE2)

  IF(DDATE.EQ.0)PPOSTFIX=POSTFIX
  IF(DDATE.NE.0)PPOSTFIX=TRIM(POSTFIX)//'_'//TRIM(ITOS(SDATE))

  IF(IBATCH.EQ.1.AND.DDATE.NE.0)WRITE(*,'(/A,I8,A,I8,A/)') 'Gridding between ',SDATE,' and ',EDATE,' using ppostfix:'//TRIM(PPOSTFIX)
  IF(ISS.EQ.2.AND.EDATE.LT.SDATE)THEN
   IF(IBATCH.EQ.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Enddate '//TRIM(ITOS(EDATE))//' is less than startdate '// &
      TRIM(ITOS(SDATE)),'Error')
    RETURN
   ELSE
    STOP 'Enddate if less than start date'
   ENDIF
  ENDIF
  ISG2GRIDMAIN=ISG2GRID(PPOSTFIX,NROW,NCOL,NLAY,0,TOP,BOT,IBATCH,MP,0)
  IF(.NOT.ISG2GRIDMAIN)RETURN
  IF(ISIMGRO.EQ.1)CALL ISG2GRIDMAIN_SVAT()
  
  !## no multiply griddings
  IF(DDATE.EQ.0)EXIT
  SIMDATE1=SIMDATE2+1
 END DO

 END FUNCTION ISG2GRIDMAIN

 !###====================================================================
 SUBROUTINE ISG2GRIDMAIN_SVAT()
 !###====================================================================
 IMPLICIT NONE
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
 WRITE(*,'(A)') 'Opening '//TRIM(AHNFNAME)//' ...'
 CALL IDFNULLIFY(AHN); IF(.NOT.IDFREAD(AHN,AHNFNAME,0))THEN; ENDIF
 WRITE(*,'(A)') 'Opening '//TRIM(THIESSENFNAME)//' ...'
 CALL IDFNULLIFY(THIESSEN); IF(.NOT.IDFREAD(THIESSEN,THIESSENFNAME,0))THEN; ENDIF
 CALL ISG2GRID_SEGREAD()
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(ROOT)//'\'//TRIM(SVAT2SWNR_DRNG),STATUS='OLD'    ,FORM='FORMATTED',ACTION='READ,DENYWRITE',ACCESS='SEQUENTIAL')
 JU=UTL_GETUNIT()
 CALL OSD_OPEN(JU,FILE=TRIM(ROOT)//'\'//SVAT2SWNR_DRNG(:INDEX(SVAT2SWNR_DRNG,'.',.TRUE.))//'_tmp',STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE,DENYREAD',ACCESS='SEQUENTIAL')

 ISWNR=0
 DO J=1,NSVATS

  READ(IU,'(I10)',IOSTAT=IOS) NETTRAP(2)
  IF(IOS.NE.0)THEN; WRITE(*,*) TRIM(SVAT2SWNR_DRNG)//' file does contain probably variable nettrap'; STOP; ENDIF

  DO ITRAP=1,NETTRAP(2)
   READ(IU,'(2F10.0,2I10,6F10.0,A30)',IOSTAT=IOS) TRAP(2,ITRAP)%X,TRAP(2,ITRAP)%Y,TRAP(2,ITRAP)%IROW,TRAP(2,ITRAP)%ICOL, &
                                           TRAP(2,ITRAP)%LN,TRAP(2,ITRAP)%BH,        &
                                           TRAP(2,ITRAP)%BW,TRAP(2,ITRAP)%CT,TRAP(2,ITRAP)%COND_IN,TRAP(2,ITRAP)%COND_OUT, &
                                           TRAP(2,ITRAP)%CSEGMENT
!   READ(IU,'(2I10,6F10.2,A30)',IOSTAT=IOS) TRAP(2,ITRAP)%IROW,TRAP(2,ITRAP)%ICOL,TRAP(2,ITRAP)%LN,TRAP(2,ITRAP)%BH,        &
!                                                TRAP(2,ITRAP)%BW,TRAP(2,ITRAP)%CT,TRAP(2,ITRAP)%COND_IN,TRAP(2,ITRAP)%COND_OUT, &
!                                                TRAP(2,ITRAP)%CSEGMENT
   IF(IOS.NE.0)THEN; WRITE(*,*) TRIM(SVAT2SWNR_DRNG)//' file does contain probably ***"s'; STOP; ENDIF
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
     IF(ISWNR.GT.NSWNR)THEN
      ISWNR=1
      I    =I+1
     ENDIF
     IF(TRIM(CSOBEK(ISWNR)).EQ.TRIM(TRAP(1,1)%CSEGMENT))EXIT
     IF(I.GT.2)EXIT
    END DO

    IF(I.GT.2)THEN
     WRITE(*,'(A)') 'Can not find label '//TRIM(TRAP(1,1)%CSEGMENT)//' in '//TRIM(SEGMENTCSVFNAME)
    ELSE
     DO ITRAP=1,NETTRAP(1)

      !## determine svat unit
      ISVAT=INT(IDFGETXYVAL(THIESSEN,TRAP(1,ITRAP)%X,TRAP(1,ITRAP)%Y))
!      ISVAT=THIESSEN%X(TRAP(1,ITRAP)%ICOL,TRAP(1,ITRAP)%IROW)
!      CALL IDFGETLOC(THIESSEN,TRAP(1,ITRAP)%IROW,TRAP(1,ITRAP)%ICOL,XC,YC)

      !## determine surface level
      MV=IDFGETXYVAL(AHN,TRAP(1,ITRAP)%X,TRAP(1,ITRAP)%Y)

      !## translate m2/day -> day
      TRAP(1,ITRAP)%COND_IN  =1.0/(TRAP(1,ITRAP)%COND_IN/(CS**2.0))
      TRAP(1,ITRAP)%COND_OUT =1.0/(TRAP(1,ITRAP)%COND_OUT/(CS**2.0))
      TRAP(1,ITRAP)%COND_IN  =MIN(TRAP(1,ITRAP)%COND_IN,99999.99)
      TRAP(1,ITRAP)%COND_OUT =MIN(TRAP(1,ITRAP)%COND_OUT,99999.99) 

      WRITE(JU,'(I10,I6,3F8.2,8X,2F8.2,8X,F8.2,8X,I10,8X,A30)') ISVAT,SYSID,MV-TRAP(1,ITRAP)%BH,TRAP(1,ITRAP)%BW,TRAP(1,ITRAP)%CT, &
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

 CALL IOSRENAMEFILE(TRIM(ROOT)//'\'//SVAT2SWNR_DRNG(:INDEX(SVAT2SWNR_DRNG,'.',.TRUE.))//'_tmp', &
                    TRIM(ROOT)//'\'//TRIM(SVAT2SWNR_DRNG))
 
 END SUBROUTINE ISG2GRIDMAIN_SVAT
 
 !###======================================================================
 SUBROUTINE ISG2GRID_SEGREAD()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU,I,IOS

 IU=UTL_GETUNIT(); OPEN(IU,FILE=SEGMENTCSVFNAME,STATUS='OLD',ACTION='READ')

 READ(IU,*); NSWNR=0; DO; NSWNR=NSWNR+1; READ(IU,*,IOSTAT=IOS); IF(IOS.NE.0)EXIT; ENDDO

 ALLOCATE(CSOBEK(NSWNR),SWNR(NSWNR))
 REWIND(IU); READ(IU,*)
 DO I=1,NSWNR
  READ(IU,*,IOSTAT=IOS) SWNR(I),CSOBEK(I)
  CALL IUPPERCASE(CSOBEK(I)); CSOBEK(I)=ADJUSTL(CSOBEK(I))
 ENDDO

 CLOSE(IU)

 END SUBROUTINE ISG2GRID_SEGREAD

 !###====================================================================
 LOGICAL FUNCTION ISG2GRID(PPOSTFIX,NROW,NCOL,NLAY,ILAY,TOP,BOT,IBATCH,MP,JU)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL,NLAY,ILAY,IBATCH,JU
 INTEGER,INTENT(INOUT) :: MP
 TYPE(IDFOBJ),DIMENSION(NLAY),INTENT(INOUT) :: TOP,BOT
 CHARACTER(LEN=*),INTENT(IN) :: PPOSTFIX 
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: I,J,K,II,JJ,TTIME,IROW,ICOL,NETTRAP,ITYPE,N,ISTW
 INTEGER :: JCRS,MAXNSEG,IRAT,IRAT1
 REAL :: C,INFF,DXY,RWIDTH,WETPER,ISGLEN,AORG,ATRAP,XSTW,YSTW,GSTW
 REAL,ALLOCATABLE,DIMENSION(:,:) :: QSORT,RVAL
 REAL,ALLOCATABLE,DIMENSION(:) :: DIST,XNR
 REAL,ALLOCATABLE,DIMENSION(:) :: X,Y
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IPOS
 REAL,ALLOCATABLE,DIMENSION(:) :: XIN,YIN,XSYM,YSYM
 REAL,ALLOCATABLE,DIMENSION(:,:) :: XTRAP,YTRAP
 INTEGER :: ISEG,JSEG,NSEG,IREF,MAXDIM,NDIM,NSYM,NTRAP,ITRAP,JQHR,NITEMS,LU,IOS,ICRS,NCRS,IUSIMGRO
 REAL :: X1,X2,Y1,Y2,D,FCT,CT,BH,BW,NETWD,COND,H1,H2,Z,WL,TD,TC,XC,YC
 LOGICAL :: LEX,LNODAT
 CHARACTER(LEN=256) :: LINE,TMPFNAME
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 TYPE(IDFOBJ) :: ICROSS
 REAL,PARAMETER :: NODATAIDF=0.0 !## do not change !!!
 
 ISG2GRID=.FALSE.
 NSVATS=0
 
 IF(IBATCH.EQ.0)THEN
  CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(2,''); CALL WINDOWOUTSTATUSBAR(4,'Initialisation 0%')
 ENDIF

 IF(ISIMGRO.EQ.1)THEN
  CALL OSD_OPEN(IUSIMGRO,FILE=TRIM(ROOT)//'\'//TRIM(SVAT2SWNR_DRNG),STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE,DENYREAD',ACCESS='SEQUENTIAL')
 ENDIF
 
 !## compute structure influences between sdate and edate
 IF(ICDIST.EQ.1)DATISD%WL_STW=DATISD%WLVL

 NITEMS=MAXITEMS; IF(ICDIST.EQ.0)NITEMS=9 
 
 !## open idf-filename
 ALLOCATE(IDF(NITEMS))

 DO I=1,NITEMS
  IDF(I)%NROW=  NROW
  IDF(I)%NCOL  =NCOL
  IDF(I)%IEQ   =0
  IDF(I)%ITB   =0
  IDF(I)%DX    =CS
  IDF(I)%DY    =CS
  IDF(I)%XMIN  =XMIN
  IDF(I)%XMAX  =XMAX
  IDF(I)%YMIN  =YMIN
  IDF(I)%YMAX  =YMAX
  IDF(I)%NODATA=NODATAIDF
  IDF(I)%IXV   =0
  CALL IDFNULLIFY(IDF(I))
  IF(.NOT.IDFALLOCATEX(IDF(I)))THEN
   IF(IBATCH.EQ.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not allocate memory for IDF '//TRIM(FNAME(I))//TRIM(PPOSTFIX)//'.IDF'//CHAR(13)// &
      'Row/Column='//TRIM(ITOS(IDF(I)%NROW))//'-'//TRIM(ITOS(IDF(I)%NCOL)),'Error')
   ELSEIF(IBATCH.EQ.1)THEN
    WRITE(*,*) 'Can not allocate memory for IDF '//TRIM(FNAME(I))//TRIM(PPOSTFIX)//'.IDF'
    WRITE(*,*) 'Nrow/Ncolumn=',IDF(I)%NCOL,IDF(I)%NROW
   ENDIF
   CALL IDFDEALLOCATE(IDF,SIZE(IDF)); DEALLOCATE(IDF)
   RETURN
  ENDIF
  IDF(I)%X=0.0
 END DO
 
 !## translate cdate in to julian date - for transient simulations only!
 IF(ISS.EQ.2)THEN
  SDATE=UTL_IDATETOJDATE(SDATE)
  EDATE=UTL_IDATETOJDATE(EDATE)+1
  TTIME=EDATE-SDATE
 ELSEIF(ISS.EQ.1)THEN
  TTIME=1
 ENDIF

 IF(ALLOCATED(QSORT))DEALLOCATE(QSORT)
 IF(ALLOCATED(XNR))DEALLOCATE(XNR)
 ALLOCATE(QSORT(TTIME,4),XNR(4))

 IF(ALLOCATED(X))DEALLOCATE(X)
 IF(ALLOCATED(Y))DEALLOCATE(Y)
 IF(ALLOCATED(RVAL))DEALLOCATE(RVAL)
 IF(ALLOCATED(DIST))DEALLOCATE(DIST)
 IF(ALLOCATED(IPOS))DEALLOCATE(IPOS)
 !## max. numbers of coordinates AND number of calculation points AND number of structures
 MAXNSEG=MAXVAL(ISG(1:NISG)%NSEG)+MAXVAL(ISG(1:NISG)%NCLC)+2*MAXVAL(ISG(1:NISG)%NSTW)
 ALLOCATE(DIST(MAXNSEG),IPOS(MAXNSEG),RVAL(4,0:MAXNSEG), & !NITEMS,0:MAXNSEG), &
          X(MAXNSEG),Y(MAXNSEG))

 MAXDIM=0

 !## compute structures
 IF(ICDIST.EQ.1)THEN
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
  IF(IDIM.EQ.3)THEN
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
   K=0
   DO J=ISEG,JSEG
    K   =K+1
    X(K)=ISP(J)%X
    Y(K)=ISP(J)%Y
   END DO

   IPOS=0
   RVAL=0.0

   !## include calculation nodes in between segments
   CALL ISG2GRIDINCLUDECLCNODES(ISG(I)%ICLC,ISG(I)%NCLC,NSEG,MAXNSEG,X,Y,DIST,IPOS,+1)
   !## include structure nodes in between segments
   CALL ISG2GRIDINCLUDECLCNODES(ISG(I)%ISTW,ISG(I)%NSTW,NSEG,MAXNSEG,X,Y,DIST,IPOS,-1)

   !## read/prepare all information for current date/segment where information is connected to
   J=0
   DO
    J=J+1
    IF(IPOS(J).GT.0)THEN
     IREF=ISG(I)%ICLC+IPOS(J)-1
     CALL ISG2GRIDGETDATA(SDATE,EDATE,TTIME,QSORT,XNR,4,RVAL(1,J),ISD(IREF)%N,ISD(IREF)%IREF,ISS,1,NODATA) 
    ENDIF
    IF(IPOS(J).LT.0)THEN
     IREF=ISG(I)%ISTW+ABS(IPOS(J))-1
     CALL ISG2GRIDGETDATA(SDATE,EDATE,TTIME,QSORT,XNR,4,RVAL(1,J),IST(IREF)%N,IST(IREF)%IREF,ISS,-1,NODATA)
     J=J+1
     !## replace waterlevel_down to waterlevel structure
     RVAL(1,J)=RVAL(2,J-1)
    ENDIF
    IF(J.EQ.NSEG)EXIT
   END DO

   !## interpolate all segments in between and give proper values!
   CALL ISG2GRIDINTSEGMENT(X,Y,DIST,NSEG,MAXNSEG)

   !## determine flow-direction
   LNODAT=.TRUE.
   DO JJ=1,4; IF(RVAL(JJ,1)   .EQ.NODATA)THEN; LNODAT=.FALSE. ; EXIT; ENDIF; ENDDO
   DO JJ=1,4; IF(RVAL(JJ,NSEG).EQ.NODATA)THEN; LNODAT=.FALSE. ; EXIT; ENDIF; ENDDO
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
      IF(ICDIST.EQ.1)THEN
       IREF=ISG(I)%ISTW+ABS(IPOS(J))-1
       CALL ISG2GRIDCOMPUTE_GETXYORIENT(I,IREF,XSTW,YSTW,GSTW)
       IF(H2.GT.H1)GSTW=GSTW+180.0
       GSTW=ISG2GRIDSTUWEN_GETORIENT(GSTW)
       ISTW=ISTW+1
       LINE=TRIM(RTOS(XSTW,'F',2))//','//TRIM(RTOS(YSTW,'F',2)) //','//TRIM(RTOS(Z,'F',2))//','// &
            TRIM(RTOS(GSTW,'F',2))//','//TRIM(RTOS(H1-H2,'F',2))//','//TRIM(ITOS(ISTW))//',"'//TRIM(IST(IREF)%CNAME)//'"'
       WRITE(LU,'(A)') TRIM(LINE)
      ENDIF

      J=J+1
     ENDIF
    ENDDO

    !## interpolate waterlevel,waterbottom,inf.factor,c-value - do not interupt it by structures (ipos().gt.0)!
    DO JJ=1,4
     DO J=1,NSEG
      IF(IPOS(J).GT.0.AND.RVAL(JJ,J).NE.NODATA)THEN
       DO II=J+1,NSEG
        IF(IPOS(II).GT.0.AND.RVAL(JJ,II).NE.NODATA)EXIT
       ENDDO
       D=MAX(0.0,DIST(II)-DIST(J))
       DO K=J+1,II-1
        FCT=0.0
        IF(D.GT.0.0)FCT=(DIST(K)-DIST(J))/D
        RVAL(JJ,K)=RVAL(JJ,J)+((RVAL(JJ,II)-RVAL(JJ,J))*FCT)
       END DO
      ENDIF
     ENDDO
    ENDDO

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
      N=0; CALL INTERSECT_EQUI(XMIN,XMAX,YMIN,YMAX,CS,X1,X2,Y1,Y2,N,.FALSE.,.TRUE.)

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
       IF(ISIMGRO.EQ.1)THEN
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
        ICOL=INT(XA(J)); IROW=INT(YA(J))

        !## within model-domain
        IF(ICOL.GE.1.AND.IROW.GE.1.AND. &
           ICOL.LE.NCOL.AND.IROW.LE.NROW)THEN

         !## interpolate to centre of line ...
         DXY=DXY+(LN(J)/2.0)

         ISGVALUE(1,2)=RVAL(1,ISEG-1)+DXY*RVAL(1,0)    !waterlevel
         ISGVALUE(1,3)=RVAL(2,ISEG-1)+DXY*RVAL(2,0)    !waterbottom

         !## translate from local to global coordinates and get proper wetted perimeter and width of channel!
         CALL ISG2GRIDGETPARAM(XIN,YIN,NDIM,ISGVALUE(1,3),ISGVALUE(1,2),RWIDTH,WETPER,MINDEPTH)
         
         ISGVALUE(1,4)= RVAL(4,ISEG-1)+DXY*RVAL(4,0)   !inf.factors
         C            = RVAL(3,ISEG-1)+DXY*RVAL(3,0)   !c-value
         !## minimal c-value
         IF(C.EQ.0.0)C=0.001
         ISGVALUE(1,1)=(LN(J)*WETPER)/C    !conductances
         
         ISGVALUE(1,5)= LN(J)
         ISGVALUE(1,6)= WETPER
         ISGVALUE(1,7)= RWIDTH
         ISGVALUE(1,8)= C

         IF(ICDIST.EQ.1)THEN
          ISGVALUE(1,10)=RVAL(1,ISEG-1)+DXY*RVAL(1,0)   !waterlevels
         ENDIF

         !## export data for simgro
         IF(ISIMGRO.EQ.1)THEN
          !## which qhpoint is active within current segment
          CALL ISG2GRIDGETQHR(JQHR,ISG(I)%IQHR,ISG(I)%NQHR,ISGLEN)
          !## start of qhr
          JSEG =ISG(I)%IQHR+JQHR-1

          !## get number of trapezia
          TC=0.0
          DO ITRAP=1,NTRAP
           !## compute nett waterdepth for current part of trapezium
           IF(ITRAP.LT.NTRAP)THEN; NETWD=YTRAP(3,ITRAP+1)-YTRAP(3,ITRAP)
           ELSE; NETWD=WDEPTH-YTRAP(3,ITRAP); ENDIF
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
            NETWD=WDEPTH-YTRAP(3,ITRAP)
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
          NSVATS=NSVATS+1
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
     ISGVALUE(1,II)=ISGVALUE(2,II)/ISGVALUE(1,1)
     IDF(II)%X(ICOL,IROW)=ISGVALUE(1,II)
    ENDDO
   ENDIF
  ENDDO
 ENDDO
 
 !## add 2d cross-section
 IRAT=0; IRAT1=0
 DO ISELISG=1,NISG
  LEX=.TRUE.; IF(IDIM.EQ.3)THEN; LEX=.FALSE.; IF(ISG(ISELISG)%ILIST.EQ.1)LEX=.TRUE.; ENDIF
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
   CALL ISGADJUSTCOMPUTEXY(ISEG,NSEG,ISC(ICRS)%DIST,TD)       !## compute correct x/y coordinate of current cross-section
   CALL IDFIROWICOL(IDF(2),IROW,ICOL,ISGX,ISGY)               !## get location in raster
   !## skip if outside current model network
   IF(IROW.EQ.0.OR.ICOL.EQ.0)CYCLE
   IF(ISGATTRIBUTES_2DCROSS_READ(I,ICROSS))THEN               !## read bathymetry current cross-section
    WL=IDF(2)%X(ICOL,IROW)                                    !## waterlevel at cross-section
    C =IDF(8)%X(ICOL,IROW)                                    !## resistance at location of cross-section
    INFF=IDF(4)%X(ICOL,IROW)                                  !## infiltration factor at location of cross-section
    CALL ISG2GRID_BATHEMETRY(IDF,SIZE(IDF),ICROSS,WL,C,INFF)  !## adjust stage grid for bathymetry
    CALL IDFDEALLOCATEX(ICROSS)
   ENDIF
   IF(IBATCH.EQ.0)CALL UTL_WAITMESSAGE(IRAT,IRAT1,ISELISG,NISG,'Progress gridding 2d cross-sections')
  ENDDO 
 ENDDO
 IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Finished gridding 2d cross-sections' 

 IF(ALLOCATED(DATISC2))DEALLOCATE(DATISC2); IF(ALLOCATED(TISC))DEALLOCATE(TISC); IF(ALLOCATED(ISCN))DEALLOCATE(ISCN)

 IF(ICDIST.EQ.1)CLOSE(LU)

 DO IROW=1,NROW; DO ICOL=1,NCOL
  !## turn into nodata for conductances equal to zero
  IF(IDF(1)%X(ICOL,IROW).EQ.IDF(1)%NODATA)THEN
   IDF(2)%X(ICOL,IROW)=IDF(2)%NODATA
   IDF(3)%X(ICOL,IROW)=IDF(3)%NODATA
   IF(ICDIST.EQ.1)IDF(10)%X(ICOL,IROW)=IDF(10)%NODATA
  ENDIF
 ENDDO; ENDDO

 IF(ALLOCATED(RVAL))DEALLOCATE(RVAL)
 IF(ALLOCATED(DIST))DEALLOCATE(DIST)
 IF(ALLOCATED(IPOS))DEALLOCATE(IPOS)
 IF(ALLOCATED(X))DEALLOCATE(X); IF(ALLOCATED(Y))DEALLOCATE(Y)

 IF(ALLOCATED(QSORT))DEALLOCATE(QSORT)
 IF(ALLOCATED(XNR))DEALLOCATE(XNR)
 CALL INTERSECT_DEALLOCATE()

 IF(ISIMGRO.EQ.1)THEN
  IF(ALLOCATED(XIN))DEALLOCATE(XIN)
  IF(ALLOCATED(YIN))DEALLOCATE(YIN)
  IF(ALLOCATED(XSYM))DEALLOCATE(XSYM)
  IF(ALLOCATED(YSYM))DEALLOCATE(YSYM)
  IF(ALLOCATED(XTRAP))DEALLOCATE(XTRAP)
  IF(ALLOCATED(YTRAP))DEALLOCATE(YTRAP)
  EDATE=NSVATS
 ENDIF

 IF(ICDIST.EQ.1)THEN
  IF(.NOT.IDFWRITE(IDF(10),TRIM(ROOT)//'\'//TRIM(FNAME(10))//TRIM(PPOSTFIX)//'.IDF',1))THEN; RETURN; ENDIF
  IF(.NOT.IDFWRITE(IDF(11),TRIM(ROOT)//'\'//TRIM(FNAME(11))//TRIM(PPOSTFIX)//'.IDF',1))THEN; RETURN; ENDIF
  CALL ISG2GRIDCOMPUTESTUWEN(TRIM(TMPFNAME),TRIM(ROOT)//'\'//TRIM(FNAME(10))//TRIM(PPOSTFIX)//'.IDF',  & !## effect
                                            TRIM(ROOT)//'\'//TRIM(FNAME(11))//TRIM(PPOSTFIX)//'.IDF',  & !## current_id
                                            TRIM(ROOT)//'\'//TRIM(FNAME(12))//TRIM(PPOSTFIX)//'.IDF')    !## next_id
 ENDIF
 
 !## extent grids based upon their width
 CALL ISG2GRID_EXTENT_WITH_WIDTH(SIZE(IDF),IDF,IBATCH)

 !## clean all
 DO IROW=1,NROW; DO ICOL=1,NCOL
  !## turn into nodata for conductances equal to zero
  IF(IDF(1)%X(ICOL,IROW).EQ.IDF(1)%NODATA)THEN
   DO I=1,SIZE(IDF); IDF(I)%X(ICOL,IROW)=-9999.00; ENDDO
  ENDIF
 ENDDO; ENDDO
 IDF%NODATA=-9999.00
 
 IF(IEXPORT.EQ.0)THEN
  DO I=1,9
   IF(ISAVE(I).EQ.0)CYCLE
   IF(IBATCH.EQ.1)WRITE(*,*) 'Saving '//TRIM(ROOT)//'\'//TRIM(FNAME(I))//TRIM(PPOSTFIX)//'.IDF ...'
   IF(.NOT.IDFWRITE(IDF(I),TRIM(ROOT)//'\'//TRIM(FNAME(I))//TRIM(PPOSTFIX)//'.IDF',1))THEN
    !##error
    IF(IBATCH.EQ.1)WRITE(*,*) '---- ERROR saving file ----'
   ENDIF
  ENDDO
 ELSEIF(IEXPORT.EQ.1)THEN
  CALL ISG2GRID_EXPORTRIVER(JU,IDF,NLAY,ILAY,TOP,BOT,MP)
 ENDIF
 
 CALL IDFDEALLOCATE(IDF,SIZE(IDF)); DEALLOCATE(IDF)
 IF(ISIMGRO.EQ.1)CLOSE(IUSIMGRO)
 
 ISG2GRID=.TRUE.

 END FUNCTION ISG2GRID

 !###====================================================================
 SUBROUTINE ISG2GRID_EXPORTRIVER(JU,IDF,NLAY,ILAY,TOP,BOT,MP)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NLAY,ILAY,JU
 INTEGER,INTENT(INOUT) :: MP
 TYPE(IDFOBJ),DIMENSION(:),INTENT(INOUT) :: IDF
 TYPE(IDFOBJ),DIMENSION(NLAY),INTENT(INOUT) :: TOP,BOT
 INTEGER :: IROW,ICOL,N,IU,I
 REAL :: T,B,WL,BL,CD,F
 CHARACTER(LEN=256) :: LINE
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
     T=TOP(I)%X(ICOL,IROW);  B =BOT(I)%X(ICOL,IROW); IF(I.EQ.1)T=MAX(WL,T)
     IF(WL.GT.B.AND.BL.LT.T)N=N+1
    ENDDO
   ENDIF
  ENDIF
 ENDDO; ENDDO

 MP=MP+N
 
 IF(JU.EQ.0)THEN
  IU=UTL_GETUNIT()
  IF(DDATE.EQ.0)THEN
   CALL OSD_OPEN(IU,FILE=TRIM(ROOT)//'\modflow.riv',STATUS='UNKNOWN',ACTION='WRITE')
   WRITE(IU,'(A)') 'SCD 1 '
  ELSE
   CALL OSD_OPEN(IU,FILE=TRIM(ROOT)//'\modflow_'//TRIM(JDATETOGDATE(SDATE,2))//'.riv',STATUS='UNKNOWN',ACTION='WRITE')
   WRITE(IU,'(A)') 'SCD 1 '//TRIM(JDATETOGDATE(SDATE,2))//'-'//TRIM(JDATETOGDATE(EDATE,2)) !N
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

! DO I=1,NLAY
!  CALL IDFDEALLOCATE(TOP,SIZE(TOP))
!  CALL IDFDEALLOCATE(BOT,SIZE(BOT))
! ENDDO
 
 END SUBROUTINE ISG2GRID_EXPORTRIVER
 
  !###====================================================================
 SUBROUTINE ISG2GRID_BATHEMETRY(IDF,NIDF,ICROSS,WL,C,INFF) 
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NIDF
 TYPE(IDFOBJ),DIMENSION(NIDF),INTENT(INOUT) :: IDF
 TYPE(IDFOBJ),INTENT(IN) :: ICROSS
 REAL,INTENT(IN) :: WL,C,INFF
 INTEGER :: IR1,IR2,IC1,IC2,IROW,ICOL,JROW,JCOL
 REAL :: XC,YC
   
 !## defined cross-sections are finer than model network
 IF(ICROSS%DX.LE.IDF(1)%DX)THEN
  !## spotify cross-section (actual bathemetry of current 2d cross-section) in mother idf
  DO IROW=1,ICROSS%NROW; DO ICOL=1,ICROSS%NCOL
   IF(ICROSS%X(ICOL,IROW).EQ.ICROSS%NODATA)CYCLE
   IF(ICROSS%X(ICOL,IROW).GT.WL)CYCLE
   CALL IDFGETLOC(ICROSS  ,IROW,ICOL,XC,YC)
   CALL IDFIROWICOL(IDF(1),JROW,JCOL,XC,YC)
   IF(JROW.NE.0.AND.JCOL.NE.0)THEN
    IF(IDF(9)%X(JCOL,JROW).EQ.0.0)THEN
     IDF(1)%X(JCOL,JROW)=IDFGETAREA(ICROSS,ICOL,IROW)/C  
    ELSE
     IDF(1)%X(JCOL,JROW)=IDF(1)%X(JCOL,JROW)+IDFGETAREA(ICROSS,ICOL,IROW)/C  
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
    IF(ICROSS%X(JCOL,JROW).EQ.ICROSS%NODATA)CYCLE  !## no data available in 2d cross-section
    IF(WL.LT.ICROSS%X(JCOL,JROW))CYCLE             !## waterlevel lower than bathemetrie
    IDF(1)%X(ICOL,IROW)=IDFGETAREA(ICROSS,ICOL,IROW)/C 
    IDF(2)%X(ICOL,IROW)=WL
    IDF(3)%X(ICOL,IROW)=ICROSS%X(JCOL,JROW)
    IDF(4)%X(ICOL,IROW)=INFF
    IDF(9)%X(ICOL,IROW)=IDF(9)%X(ICOL,IROW)+1.0    !## counter how many times it passes
   ENDIF
  ENDDO; ENDDO
 ENDIF
 
 END SUBROUTINE ISG2GRID_BATHEMETRY
 
 !###====================================================================
 SUBROUTINE ISG2GRID_EXTENT_WITH_WIDTH(NIDF,IDF,IBATCH)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NIDF,IBATCH
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
 SUBROUTINE ISG2GRIDCOMPUTESTUWEN(IPFFNAME,IDFFNAME1,IDFFNAME2,IDFFNAME3)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IPFFNAME,IDFFNAME1,IDFFNAME2,IDFFNAME3
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
 REAL :: NOR,OR,X,Y,DXY
 INTEGER :: I,IDX,IDY,ICOL,IROW

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
                            NR,IREF,ISS,ITYPE,NODATA) 
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: NODATA
 INTEGER,INTENT(IN) :: SDATE,EDATE,TTIME,NITEMS,NR,IREF,ISS,ITYPE
 REAL,INTENT(INOUT),DIMENSION(TTIME,NITEMS) :: QSORT
 REAL,DIMENSION(NITEMS),INTENT(OUT) :: XNR
 REAL,DIMENSION(NITEMS),INTENT(INOUT) :: RVAL
 INTEGER :: IR,N,I,I1,I2,IDATE,NAJ,IREC,NDATE 

 IREC=IREF-1

 IF(NR.LE.0)RETURN

 IF(ISS.EQ.1)QSORT=0.0
 IF(ISS.EQ.2)QSORT=NODATA 
 I1   = 1
 
 XNR=0.0
 DO IR=1,NR
  
  IREC=IREC+1

  IF(ITYPE.EQ.1)THEN
   IDATE  =DATISD(IREC)%IDATE
   RVAL(1)=DATISD(IREC)%WLVL
   RVAL(2)=DATISD(IREC)%BTML
   RVAL(3)=DATISD(IREC)%RESIS
   RVAL(4)=DATISD(IREC)%INFF
!   IF(NITEMS.GT.8)RVAL(5)=DATISD(IREC)%WL_STW
  ELSEIF(ITYPE.EQ.-1)THEN
   IDATE  =DATIST(IREC)%IDATE
   RVAL(1)=DATIST(IREC)%WLVL_UP
   RVAL(2)=DATIST(IREC)%WLVL_DOWN
   RVAL(3)=0.0
   RVAL(4)=0.0
  ENDIF

  !## don't bother for steady-state, take the mean!
  IF(ISS.EQ.1)THEN
   DO I=1,NITEMS; IF(RVAL(I).NE.NODATA)THEN; QSORT(1,I)=QSORT(1,I)+RVAL(I); XNR(I)=XNR(I)+1.0; ENDIF; ENDDO
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

  ENDIF !## ELSEIF(ISS.EQ.2)THEN

 END DO

 IF(ISS.EQ.1)THEN
  !## determine for each period appropriate attribute term
  DO I=1,NITEMS
   IF(XNR(I).GT.0.0)THEN
    RVAL(I)=QSORT(1,I)/REAL(XNR(I))
   ELSE
    RVAL(I)=NODATA
   ENDIF
  ENDDO
 !## take the mean (better than median)
 ELSEIF(ISS.EQ.2)THEN
  IF(IAVERAGE.EQ.1)THEN
   !## arithmetic mean
   DO I=1,NITEMS
    RVAL(I)=SUM(QSORT(:,I))/REAL(TTIME)
   ENDDO 
  ELSEIF(IAVERAGE.EQ.2)THEN
   !## median
   DO I=1,NITEMS
    CALL UTL_GETMED(QSORT(:,I),TTIME,NODATA,(/50.0/),1,NAJ,RVAL(I))
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
  DXY=((X(ISEG)-X(ISEG-1))**2.0)+((Y(ISEG)-Y(ISEG-1))**2.0)
  IF(DXY.GT.0.0)DXY=SQRT(DXY)
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
 SUBROUTINE ISG2GRIDINCLUDECLCNODES(ITYP,NTYP,NSEG,MAXNSEG,X,Y,DIST,IPOS,&
                                    ITYPE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYP,NTYP,MAXNSEG,ITYPE
 INTEGER,INTENT(INOUT) :: NSEG
 REAL,INTENT(INOUT),DIMENSION(MAXNSEG) :: DIST,X,Y
 INTEGER,INTENT(OUT),DIMENSION(MAXNSEG) :: IPOS
 INTEGER :: IREC,ISEG,I
 REAL :: DXY,D1,D2,F,XC,YC

 !#include calculation nodes as segments!
 IREC=ITYP-1
 !#determine which nodes possess heads/etc.
 DO I=1,NTYP
 !#get tot distances
  CALL ISG2GRIDINTSEGMENT(X,Y,DIST,NSEG,MAXNSEG)
  IREC=IREC+1
  IF(ITYPE.EQ.1) DXY=ISD(IREC)%DIST
  IF(ITYPE.EQ.-1)DXY=IST(IREC)%DIST

 !#find position in between segments
  DO ISEG=2,NSEG
   IF(DXY.GE.DIST(ISEG-1).AND.DXY.LE.DIST(ISEG))EXIT
  END DO

  !##caused by inaccuracy of comparison of dxy and dist()
  ISEG=MIN(ISEG,NSEG)

 !#distance current segment
  D1=DIST(ISEG)-DIST(ISEG-1)
  D2=DXY-DIST(ISEG-1)
  F =0.0
  IF(D1.NE.0.0)F=D2/D1

 !#put in extra coordinate
  IF(F.LE.0.0.AND.ITYPE.EQ.1)THEN
   IPOS(ISEG-1)=I*ITYPE  !##put data to current node
  ELSEIF(F.GE.1.0.AND.ITYPE.EQ.1)THEN
   IPOS(ISEG)  =I*ITYPE  !##put data to current node
  ELSE
   XC=X(ISEG-1)+((X(ISEG)-X(ISEG-1))*F)
   YC=Y(ISEG-1)+((Y(ISEG)-Y(ISEG-1))*F)
 !##position coordinates in between
   X(ISEG+1:NSEG+1)   =X(ISEG:NSEG)
   Y(ISEG+1:NSEG+1)   =Y(ISEG:NSEG)
   IPOS(ISEG+1:NSEG+1)=IPOS(ISEG:NSEG)
   X(ISEG)            =XC
   Y(ISEG)            =YC
   NSEG               =NSEG+1     !##increase number of segments
   IPOS(ISEG)         =I*ITYPE    !##put data to current node
  ENDIF

 !#duplicate point in case of structure
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

 !#put last ipos() for secerity reasons ... could happen in case of accuray of coordinates
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

