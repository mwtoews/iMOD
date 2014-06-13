!!  Copyright (C) Stichting Deltares, 2005-2014.
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

!###====================================================================
MODULE MOD_PEST
!###====================================================================
!USE MOD_ISIMGRO
USE PESTVAR
USE IMOD_UTL, ONLY : IMOD_UTL_PRINTTEXT,IMOD_UTL_CAPF,IMOD_UTL_ITOS,IMOD_UTL_GETREAL,&
 IMOD_UTL_FILENAME,IMOD_UTL_RTOS,IMOD_UTL_STRING,IMOD_UTL_OPENASC,IMOD_UTL_SWAPSLASH,&
 IMOD_UTL_GETUNIT,IMOD_UTL_CREATEDIR,IMOD_UTL_GETRCL,IMOD_UTL_LUDECOMP
USE MOD_RF2MF, ONLY: NLINES, CMOD, PPST, SIMBOX, SIMCSIZE, IURUN
USE TSVAR, ONLY : TS,CIPFTYPE,TS,ROOTRES, IIPF, ROOTRES
use idfmodule

CHARACTER(LEN=256),PRIVATE :: LINE
REAL,PARAMETER :: RCH_IMPULSE_SIZE=0.01    !## impulse of 0.01 m/day = 10.00 mm/day
REAL,PARAMETER :: RRECHSTRUCNIV   =0.00075 !## structural recharge of   0.75 mm/day
CHARACTER(LEN=15),PARAMETER :: CVERSION='mf2005'
CHARACTER(LEN=2100),PRIVATE :: BLINE
CHARACTER(LEN=52),PRIVATE :: SLINE
REAL,PRIVATE :: DAMPINGFACTOR=1.5
!REAL,PRIVATE,PARAMETER :: ININDAMPING=1.5
!REAL,PRIVATE :: NDAMPING=ININDAMPING

CONTAINS

 !###====================================================================
 SUBROUTINE PEST1INIT()
 !###====================================================================
 use rf2mf_module, only: ncol, nrow, nlay, nper
 IMPLICIT NONE
 INTEGER :: IOS,I,J,N,IZ,IROW,ICOL
 REAL :: NODATA,TF,F
 logical :: lop

 IF(IIPF.EQ.0)CALL IMOD_UTL_PRINTTEXT('You should specify IIPF>0 in combination with PST module',2)

 !## next pest run ... skip part of runfile with pest information
 IF(ALLOCATED(PARAM))RETURN

 CALL PESTOPENFILE(IUPESTOUT,'log_pest_','txt',PEST_ITER)
 CALL PESTOPENFILE(IUPESTPROGRESS,'log_pest_progress_','txt',PEST_ITER)
 IUPESTRESIDUAL=0

 WRITE(IUPESTOUT,'(A)') 'PEST-LOG'
 DO I=1,ABS(IIPF)
  WRITE(IUPESTOUT,'(/A)') ' Reading:  '//TRIM(TS(I)%IPFNAME)
  LINE=' IPF-type: ('//TRIM(IMOD_UTL_ITOS(TS(I)%IPFTYPE))//') '//TRIM(CIPFTYPE(TS(I)%IPFTYPE))
  WRITE(IUPESTOUT,'(A/)') TRIM(LINE)
 ENDDO

 ALLOCATE(PARAM(NLINES))
 DO I=1,SIZE(PARAM); PARAM(I)%NODES=0; PARAM(I)%IBND=0; ENDDO
 DO I=1,SIZE(PARAM); NULLIFY(PARAM(I)%X,PARAM(I)%IROW,PARAM(I)%ICOL,PARAM(I)%ALPHA_HISTORY); ENDDO

 READ(IURUN,'(A256)',IOSTAT=IOS) LINE
 IF(IOS.EQ.0)READ(LINE,*,IOSTAT=IOS) PEST_NITER,PEST_JSTOP,PEST_SENSITIVITY, &
   PEST_NPERIOD,PEST_NBATCH,(PEST_ITARGET(I),I=1,SIZE(PEST_ITARGET)),PEST_ISCALING,PEST_PADJ
 IF(IOS.NE.0)THEN
  PEST_PADJ=0.0 !## optional parameter adjustment
  READ(LINE,*,IOSTAT=IOS) PEST_NITER,PEST_JSTOP,PEST_SENSITIVITY, &
    PEST_NPERIOD,PEST_NBATCH,(PEST_ITARGET(I),I=1,SIZE(PEST_ITARGET)),PEST_ISCALING
  IF(IOS.NE.0)THEN
   PEST_ISCALING=0
   READ(LINE,*,IOSTAT=IOS) PEST_NITER,PEST_JSTOP,PEST_SENSITIVITY, &
    PEST_NPERIOD,PEST_NBATCH,(PEST_ITARGET(I),I=1,SIZE(PEST_ITARGET))
  ENDIF
 ENDIF
 IF(IOS.NE.0)THEN
  CALL IMOD_UTL_PRINTTEXT('Error reading NITER,JSTOP,SENSITIVITY,NPERIOD,NBATCH,ITARGET[.]:',0)
  CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),0)
  CALL IMOD_UTL_PRINTTEXT('Busy processing module: '//TRIM(CMOD(PPST)),2)
 ENDIF

 CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT(' Pest-Settings',-1,IUPESTOUT)
 CALL IMOD_UTL_PRINTTEXT(' @@@ Number of Pest Iterations: '//TRIM(IMOD_UTL_ITOS(PEST_NITER)),-1,IUPESTOUT)
 CALL IMOD_UTL_PRINTTEXT(' @@@ Stop Criterium Objective Function Value: '//TRIM(IMOD_UTL_RTOS(PEST_JSTOP,'F',3)),-1,IUPESTOUT)
 CALL IMOD_UTL_PRINTTEXT(' @@@ Sensitivity to Exclude Parameter (temporarily): '//TRIM(IMOD_UTL_RTOS(PEST_SENSITIVITY,'F',3)),-1,IUPESTOUT)
 SELECT CASE (PEST_ISCALING)
  CASE (0)
   CALL IMOD_UTL_PRINTTEXT(' @@@ No  Scaling, No  SVD',-1,IUPESTOUT)
  CASE (1)
   CALL IMOD_UTL_PRINTTEXT(' @@@ Yes Scaling, No  SVD',-1,IUPESTOUT)
  CASE (2)
   CALL IMOD_UTL_PRINTTEXT(' @@@ Yes Scaling, Yes SVD',-1,IUPESTOUT)
  CASE (3)
   CALL IMOD_UTL_PRINTTEXT(' @@@ No  Scaling, Yes SVD',-1,IUPESTOUT)
 END SELECT
 CALL IMOD_UTL_PRINTTEXT(' @@@ Termination Criterion for Parameter Adjustments (vectorlength): '//TRIM(IMOD_UTL_RTOS(PEST_PADJ,'F',3)),-1,IUPESTOUT)

 DO I=1,SIZE(PEST_ITARGET)
  IF(PEST_ITARGET(I).LT.0.0)CALL IMOD_UTL_PRINTTEXT('Error PEST_ITARGET('//TRIM(IMOD_UTL_ITOS(I))//') < 0.0',2)
 ENDDO

 TF=SUM(PEST_ITARGET)

 CALL IMOD_UTL_PRINTTEXT(' @@@ Number of BatchFiles Used: '//TRIM(IMOD_UTL_ITOS(PEST_NBATCH)),-1,IUPESTOUT)
 IF(PEST_NBATCH.GT.0)THEN
  ALLOCATE(PEST_IBATCH(PEST_NBATCH))
  DO I=1,PEST_NBATCH
   READ(IURUN,'(A256)',IOSTAT=IOS) LINE
   IF(IOS.NE.0)THEN
    CALL IMOD_UTL_PRINTTEXT('Error reading PERIOD1,PERIOD2: '//TRIM(LINE),0)
    CALL IMOD_UTL_PRINTTEXT('Busy processing module: '//TRIM(CMOD(PPST)),2)
   ENDIF
   CALL IMOD_UTL_STRING(LINE)
   READ(LINE,*,IOSTAT=IOS) PEST_IBATCH(I)%FRACTION,PEST_IBATCH(I)%BATCHFILE,PEST_IBATCH(I)%OUTFILE
   IF(PEST_IBATCH(I)%FRACTION.LT.0.0)CALL IMOD_UTL_PRINTTEXT('Error PEST_IBATCH('//TRIM(IMOD_UTL_ITOS(I))//')%FRACTION < 0.0',2)
   TF=TF+PEST_IBATCH(I)%FRACTION
  ENDDO
 ENDIF

 TF=1.0/TF
 PEST_ITARGET=TF*PEST_ITARGET
 DO I=1,PEST_NBATCH; PEST_IBATCH(I)%FRACTION=TF*PEST_IBATCH(I)%FRACTION; ENDDO

 !## write fractions
 CALL IMOD_UTL_PRINTTEXT(' @@@ Fraction Heads: '//TRIM(IMOD_UTL_RTOS(PEST_ITARGET(1),'F',3)),-1,IUPESTOUT)
 CALL IMOD_UTL_PRINTTEXT(' @@@ Fraction DynHd: '//TRIM(IMOD_UTL_RTOS(PEST_ITARGET(2),'F',3)),-1,IUPESTOUT)
 DO I=1,PEST_NBATCH
  CALL IMOD_UTL_PRINTTEXT(' @@@ Fraction Batch '//TRIM(IMOD_UTL_ITOS(I))//': '//TRIM(IMOD_UTL_RTOS(PEST_IBATCH(I)%FRACTION,'F',3)),-1,IUPESTOUT)
  CALL IMOD_UTL_PRINTTEXT(' @@@ BatchFile      '//TRIM(IMOD_UTL_ITOS(I))//': '//TRIM(PEST_IBATCH(I)%BATCHFILE),-1,IUPESTOUT)
  CALL IMOD_UTL_PRINTTEXT(' @@@ BatchOutFile   '//TRIM(IMOD_UTL_ITOS(I))//': '//TRIM(PEST_IBATCH(I)%OUTFILE),-1,IUPESTOUT)
 ENDDO

 CALL IMOD_UTL_PRINTTEXT(' @@@ Number of Periods Used: '//TRIM(IMOD_UTL_ITOS(PEST_NPERIOD)),-1,IUPESTOUT)
 IF(PEST_NPERIOD.GT.0)THEN
  ALLOCATE(PEST_IPERIOD(PEST_NPERIOD,2))
  DO I=1,PEST_NPERIOD
   READ(IURUN,'(A256)',IOSTAT=IOS) LINE
   IF(IOS.EQ.0)READ(LINE,*,IOSTAT=IOS) PEST_IPERIOD(I,1),PEST_IPERIOD(I,2)
   IF(IOS.NE.0)THEN
    CALL IMOD_UTL_PRINTTEXT('Error reading PERIOD1,PERIOD2: '//TRIM(LINE),0)
    CALL IMOD_UTL_PRINTTEXT('Busy processing module: '//TRIM(CMOD(PPST)),2)
   ENDIF
   CALL IMOD_UTL_PRINTTEXT(' @@@ Period '//TRIM(IMOD_UTL_ITOS(I))//': '//TRIM(IMOD_UTL_ITOS(PEST_IPERIOD(I,1)))//'-'//TRIM(IMOD_UTL_ITOS(PEST_IPERIOD(I,2))),-1,IUPESTOUT)
  ENDDO
 ENDIF
 CALL IMOD_UTL_PRINTTEXT('',0)

 DO I=1,SIZE(PARAM)
  READ(IURUN,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.EQ.0)THEN
   READ(LINE,*,IOSTAT=IOS) PARAM(I)%IACT, &  !## iact
        PARAM(I)%PTYPE, & !## ptype
        PARAM(I)%ILS, &   !## ilayer/system
        PARAM(I)%IZONE, & !## zone number
        PARAM(I)%INI, &   !## initial value
        PARAM(I)%DELTA, & !## finite difference step
        PARAM(I)%MIN, &   !## minimal value
        PARAM(I)%MAX,&    !## maximal value
        PARAM(I)%FADJ,&   !## maximal adjust factor
        PARAM(I)%IGROUP   !## group number
   IF(IOS.NE.0)THEN
    READ(LINE,*,IOSTAT=IOS) PARAM(I)%IACT, &  !## iact
        PARAM(I)%PTYPE, & !## ptype
        PARAM(I)%ILS, &   !## ilayer/system
        PARAM(I)%IZONE, & !## zone number
        PARAM(I)%INI, &   !## initial value
        PARAM(I)%DELTA, & !## finite difference step
        PARAM(I)%MIN, &   !## minimal value
        PARAM(I)%MAX,&    !## maximal value
        PARAM(I)%FADJ     !## maximal adjust factor
    PARAM(I)%IGROUP=I     !## group number
   ENDIF
   PARAM(I)%IACT=MIN(1,MAX(0,PARAM(I)%IACT))
  ENDIF
  IF(IOS.NE.0)THEN
   CALL IMOD_UTL_PRINTTEXT('Missing parameter in: PTYPE,ILS,IZONE,INI,DELTA,MIN,MAX,FADJ',0)
   CALL IMOD_UTL_PRINTTEXT(' reading: '//TRIM(LINE),0)
   CALL IMOD_UTL_PRINTTEXT('Busy processing module: '//TRIM(CMOD(PPST)),2)
  ENDIF
  ALLOCATE(PARAM(I)%ALPHA_HISTORY(0:PEST_NITER))
  PARAM(I)%ALPHA_HISTORY(0)=PARAM(I)%INI
  PARAM(I)%PTYPE=IMOD_UTL_CAPF(PARAM(I)%PTYPE,'U')
 ENDDO

 READ(IURUN,'(A256)',IOSTAT=IOS) LINE
 IF(IOS.EQ.0)READ(LINE,*,IOSTAT=IOS) N
 IF(IOS.NE.0)THEN
  CALL IMOD_UTL_PRINTTEXT('Error reading NZONE'//TRIM(LINE),0)
  CALL IMOD_UTL_PRINTTEXT('Busy processing module: '//TRIM(CMOD(PPST)),2)
 ENDIF

 IF(ALLOCATED(ZONE))THEN
  DO I=1,SIZE(ZONE)
   IF(ASSOCIATED(ZONE(I)%IROW))DEALLOCATE(ZONE(I)%IROW)
   IF(ASSOCIATED(ZONE(I)%ICOL))DEALLOCATE(ZONE(I)%ICOL)
  ENDDO
 ENDIF
 IF(ALLOCATED(BUFPST))DEALLOCATE(BUFPST)
 ALLOCATE(ZONE(N),BUFPST(NCOL,NROW,N))

 DO I=1,SIZE(ZONE)

  NULLIFY(ZONE(I)%IROW,ZONE(I)%ICOL)
  READ(IURUN,*,IOSTAT=IOS) LINE
  IF(IOS.NE.0)THEN
   CALL IMOD_UTL_PRINTTEXT('Error reading '//TRIM(LINE),0)
   CALL IMOD_UTL_PRINTTEXT('Busy processing module: '//TRIM(CMOD(I)),2)
  ENDIF

  IZ=INT(IMOD_UTL_GETREAL(LINE,IOS))
  IF(IOS.EQ.0)THEN
   CALL IMOD_UTL_PRINTTEXT('Read Constant Value '//TRIM(IMOD_UTL_ITOS(IZ)),0)
   BUFPST(:,:,I)=IZ
  ELSE
   CALL IMOD_UTL_FILENAME(LINE)
   CALL IMOD_UTL_PRINTTEXT('Assigned '//TRIM(LINE),0)

   if (.not.idfread(idfc,line,0)) CALL IMOD_UTL_PRINTTEXT('idfread',2)
   call idfnullify(idfm)
   idfm%ieq=0
   idfm%dx=simcsize
   idfm%dy=simcsize
   idfm%ncol=ncol
   idfm%nrow=nrow
   idfm%xmin = simbox(1)
   idfm%ymin = simbox(2)
   idfm%xmax = simbox(3)
   idfm%ymax = simbox(4)
   nodata = idfc%nodata
   idfm%nodata = nodata
   if (.not.idfreadscale(idfc,idfm,9,0)) CALL IMOD_UTL_PRINTTEXT('idfreadscale',2)
   bufpst(:,:,i) = idfm%x
   call idfdeallocatex(idfc)
   if (idfc%iu.gt.0) then
      inquire(unit=idfc%iu,opened=lop); if(lop)close(idfc%iu)
   endif
  ENDIF
 ENDDO

 CALL IMOD_UTL_PRINTTEXT('Parameters',-1,IUPESTOUT)
 WRITE(LINE,'(A2,1X,A5,2(1X,A3),5(1X,A15),2A10)') 'AC','PTYPE','ILS','IZN','INITIAL','DELTA','MINIMUM','MAXIMUM','FADJ','IGROUP','NODES'
 CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),-1,IUPESTOUT)

 !## check number of zones and missing zone (if any)
 DO I=1,SIZE(PARAM)
  IF(PARAM(I)%IACT.EQ.1)THEN
   DO IROW=1,NROW; DO ICOL=1,NCOL; DO J=1,SIZE(BUFPST,3)
    !## check whether it's integer value is equal to param(i)%izone, if so use fraction from idf-file
    IF(PARAM(I)%IZONE.EQ.INT(BUFPST(ICOL,IROW,J)))THEN
     PARAM(I)%NODES=PARAM(I)%NODES+1
     EXIT
    ENDIF
   ENDDO; ENDDO; ENDDO
  ENDIF
  IF(PARAM(I)%PTYPE.EQ.'HF')THEN
   PARAM(I)%NODES=0 !## one single cell used as zone for horizontal barrier module
   WRITE(LINE,'(I2,1X,A5,2(1X,I3),5(1X,F15.7),I10,A10)') PARAM(I)%IACT,PARAM(I)%PTYPE,PARAM(I)%ILS, &
     PARAM(I)%IZONE,PARAM(I)%INI,PARAM(I)%DELTA,PARAM(I)%MIN,PARAM(I)%MAX,PARAM(I)%FADJ,PARAM(I)%IGROUP,'EntireLine'
  ELSE
   IF(PARAM(I)%NODES.EQ.0)THEN
    CALL IMOD_UTL_PRINTTEXT('No area/zone assigned to parameter no. '//TRIM(IMOD_UTL_ITOS(I))//' ptype= '//TRIM(PARAM(I)%PTYPE),0)
    PARAM(I)%IACT=0
   ENDIF
   WRITE(LINE,'(I2,1X,A5,2(1X,I3),5(1X,F15.7),2I10)') PARAM(I)%IACT,PARAM(I)%PTYPE,PARAM(I)%ILS, &
     PARAM(I)%IZONE,PARAM(I)%INI,PARAM(I)%DELTA,PARAM(I)%MIN,PARAM(I)%MAX,PARAM(I)%FADJ,PARAM(I)%IGROUP,PARAM(I)%NODES
  ENDIF
  CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),-1,IUPESTOUT)
  CALL PEST1CHK(I)
  PARAM(I)%ALPHA(1)=PARAM(I)%INI !## current  alpha
  PARAM(I)%ALPHA(2)=PARAM(I)%INI !## previous alpha
  !## parameter is at the boundary whenever less than 1% away
  IF(ABS(PARAM(I)%ALPHA(1)-PARAM(I)%MIN).LT.XPBND)PARAM(I)%IBND=-1 !## min
  IF(ABS(PARAM(I)%MAX)-PARAM(I)%ALPHA(1).LT.XPBND)PARAM(I)%IBND= 1 !## max
 ENDDO
 N=0; DO I=1,SIZE(PARAM)
  IF(PARAM(I)%IACT.EQ.0)CYCLE
  SELECT CASE (PARAM(I)%PTYPE)
   CASE ('HF')
    N=N+1
   CASE DEFAULT
    N=N+PARAM(I)%NODES
  END SELECT
 ENDDO
 IF(N.EQ.0)CALL IMOD_UTL_PRINTTEXT('Nothing to do',2)

 !## fill array zone and set appropriate pointers in type
 DO I=1,SIZE(PARAM)
  IF(PARAM(I)%NODES.GT.0)THEN
   CALL IMOD_UTL_PRINTTEXT('Parameter '//TRIM(IMOD_UTL_ITOS(I))//' no. of zones no. '//TRIM(IMOD_UTL_ITOS(PARAM(I)%NODES))//' assigned to ptype= '//TRIM(PARAM(I)%PTYPE),0)
   ALLOCATE(PARAM(I)%IROW(PARAM(I)%NODES),PARAM(I)%ICOL(PARAM(I)%NODES))
   ALLOCATE(PARAM(I)%F(PARAM(I)%NODES))
   N=0; DO IROW=1,NROW; DO ICOL=1,NCOL; DO J=1,SIZE(BUFPST,3)
    IF(PARAM(I)%IZONE.EQ.INT(BUFPST(ICOL,IROW,J)))THEN
     F=MOD(BUFPST(ICOL,IROW,J),1.0); IF(F.EQ.0.0)F=1.0
     N=N+1; PARAM(I)%IROW(N)=INT(IROW,2); PARAM(I)%ICOL(N)=INT(ICOL,2); PARAM(I)%F(N)=F
     EXIT
    ENDIF
   ENDDO; ENDDO; ENDDO
  ELSE
   IF(PARAM(I)%PTYPE.NE.'HF')PARAM(I)%IACT=0
  ENDIF
 ENDDO

 !## set igroup lt 0 for followers in group
 DO I=1,SIZE(PARAM)
  IF(PARAM(I)%IACT.EQ.0)CYCLE
  DO J=1,I-1 !I+1,SIZE(PARAM)
   IF(PARAM(J)%IACT.EQ.0)CYCLE
   IF(PARAM(J)%IGROUP.EQ.PARAM(I)%IGROUP)THEN
    PARAM(I)%IGROUP=-1*PARAM(I)%IGROUP; EXIT
   ENDIF
  ENDDO
 ENDDO

 DEALLOCATE(BUFPST)

 !## initialize process-flags
 LGRAD       =.TRUE.  !## gradient computation
 PEST_IGRAD  = 0      !## current simulation is default
 LLNSRCH     =.FALSE. !## no line search

 !## sensitivity - if pest_niter=0
 LSENS=.FALSE.; IF(PEST_NITER.EQ.0)LSENS=.TRUE. !; IF(LSENS)PARAM%ALPHA(1)=PARAM%MIN

 WRITE(BLINE,'(3A5,A15)') 'IT','GD','LS','TOT_J'
 DO J=1,SIZE(PARAM)
  IF(ABS(PARAM(J)%IACT).EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
   WRITE(SLINE,'(2X,A2,2I3.3)') PARAM(J)%PTYPE,PARAM(J)%ILS,PARAM(J)%IZONE
   BLINE=TRIM(BLINE)//TRIM(SLINE)
  ENDIF
 ENDDO

 WRITE(IUPESTPROGRESS,'(A)') TRIM(BLINE) !(3A5,A15,999(2X,A2,2I3.3))') ' IT',' GD',' LS','TOT_J',(PARAM(J)%PTYPE ,PARAM(J)%ILS,PARAM(J)%IZONE,J=1,SIZE(PARAM))

 BLINE=''
 DO J=1,SIZE(PARAM)
  IF(ABS(PARAM(J)%IACT).EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
   WRITE(SLINE,'(7X,I3.3)') PARAM(J)%IGROUP
   BLINE=TRIM(BLINE)//TRIM(SLINE)
  ENDIF
 ENDDO
 WRITE(IUPESTPROGRESS,'(30X,A)') TRIM(BLINE) !30X    ,999(7X,    I3.3))')                           (PARAM(J)%IGROUP                            ,J=1,SIZE(PARAM))
 FLUSH(IUPESTPROGRESS)

 !## dump initial factors
 CALL PESTDUMPFCT()

 END SUBROUTINE PEST1INIT

 !###====================================================================
 SUBROUTINE PEST1ALPHA_METASWAP(nrow,ncol,lpwt)
 !###====================================================================
 IMPLICIT NONE
 integer, intent(in) :: nrow, ncol
 logical, intent(in) :: lpwt
 INTEGER :: IUSCL,IOS,I,J,IROW,ICOL,NUND,N
 REAL :: FCT,THETA,PRZ,COND,PWT
 TYPE SCLOBJ1
  INTEGER :: NUND,IROW,ICOL
 END TYPE SCLOBJ1
 TYPE SCLOBJ2
  REAL :: THETA,COND,PRZ,PWT
 END TYPE SCLOBJ2
 TYPE(SCLOBJ1),DIMENSION(:),ALLOCATABLE :: SCL1
 TYPE(SCLOBJ2),DIMENSION(:,:),ALLOCATABLE :: SCL2

 !## check whether metaswap parameters are optimized whatsoever
 J=0; DO I=1,SIZE(PARAM); SELECT CASE (TRIM(PARAM(I)%PTYPE)); CASE ('MS','MC'); J=J+1; END SELECT; ENDDO
 IF(J.EQ.0)RETURN

 !## count number of uscl_svat-units
 CALL IMOD_UTL_OPENASC(IUSCL,'uscl_svat.inp','R')
 N=0; DO
  READ(IUSCL,'(I10,4F8.0,2I10)',IOSTAT=IOS) NUND,THETA,COND,PRZ,PWT,ICOL,IROW
  IF(IOS.NE.0)EXIT; N=N+1
 ENDDO; CLOSE(IUSCL)
 IF(N.EQ.0)CALL IMOD_UTL_PRINTTEXT('Error no SVAT-units found',2)

 ALLOCATE(SCL1(N)); SCL1%NUND=0; SCL1%ICOL=0; SCL1%IROW=0
 ALLOCATE(SCL2(NCOL,NROW)); SCL2%THETA=0.0; SCL2%COND=0.0; SCL2%PRZ=0.0

 CALL IMOD_UTL_OPENASC(IUSCL,'uscl_svat.inp','R')
 DO I=1,N; READ(IUSCL,'(I10,4F8.0,2I10)',IOSTAT=IOS) NUND,THETA,COND,PRZ,PWT,ICOL,IROW
  SCL1(I)%NUND =NUND
  SCL1(I)%IROW =IROW
  SCL1(I)%ICOL =ICOL
  !## initialize again
  SCL2(ICOL,IROW)%THETA=THETA !1.0 !THETA
  SCL2(ICOL,IROW)%COND =COND  !1.0 !COND
  SCL2(ICOL,IROW)%PRZ  =PRZ   !1.0 !PRZ
  SCL2(ICOL,IROW)%PWT  =PWT   !1.0 !PWT
 ENDDO; CLOSE(IUSCL)

 !## apply parameters estimation adjustments
 DO I=1,SIZE(PARAM)
  SELECT CASE (TRIM(PARAM(I)%PTYPE))
   CASE ('MS','MC')
    FCT=EXP(PARAM(I)%ALPHA(1)) !; ILAY=PARAM(I)%ILS !## <-- not to be used

    !## allocate memory to make copy of data
    IF(.NOT.ASSOCIATED(PARAM(I)%X))ALLOCATE(PARAM(I)%X(PARAM(I)%NODES))
    SELECT CASE (TRIM(PARAM(I)%PTYPE))
     CASE ('MS')  !## soil moisture
      DO J=1,PARAM(I)%NODES
       IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
       IF(PEST_ITER.EQ.0)PARAM(I)%X(J)=SCL2(ICOL,IROW)%THETA
       SCL2(ICOL,IROW)%THETA=PARAM(I)%X(J)*FCT
      ENDDO
     CASE ('MC')  !## conductivity
      DO J=1,PARAM(I)%NODES
       IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
       IF(PEST_ITER.EQ.0)PARAM(I)%X(J)=SCL2(ICOL,IROW)%COND
       SCL2(ICOL,IROW)%COND=PARAM(I)%X(J)*FCT
      ENDDO
    END SELECT
   CASE DEFAULT
    CYCLE
  END SELECT
  LINE=' * '//PARAM(I)%PTYPE//' adjusted ('//TRIM(IMOD_UTL_ITOS(PARAM(I)%NODES))//') with alpha='//TRIM(IMOD_UTL_RTOS(FCT,'F',7))
  CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),-1,IUPESTOUT) !; CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),1)
 ENDDO

 CALL IMOD_UTL_OPENASC(IUSCL,'uscl_svat.inp','W')
 DO I=1,N
  IROW=SCL1(I)%IROW; ICOL=SCL1(I)%ICOL
  IF(lpwt)THEN
   WRITE(IUSCL,'(I10,3F8.3,8X,2I10)') SCL1(I)%NUND,SCL2(ICOL,IROW)%THETA, &
                                      SCL2(ICOL,IROW)%COND,SCL2(ICOL,IROW)%PRZ,ICOL,IROW
  ELSE
   WRITE(IUSCL,'(I10,4F8.3,2I10)') SCL1(I)%NUND,SCL2(ICOL,IROW)%THETA, &
                                   SCL2(ICOL,IROW)%COND,SCL2(ICOL,IROW)%PRZ,SCL2(ICOL,IROW)%PWT,ICOL,IROW
  ENDIF
 ENDDO
 CLOSE(IUSCL)

 DEALLOCATE(SCL1,SCL2)

 END SUBROUTINE PEST1ALPHA_METASWAP


 !#####=================================================================
 SUBROUTINE PEST1_METEO_METASWAP()
 !#####=================================================================
 IMPLICIT NONE
 INTEGER :: IU,JU,I,J,IOS,IYR
 TYPE METEOOBJ
  INTEGER :: IS,IY
  REAL :: D,N,E
 END TYPE METEOOBJ
 TYPE(METEOOBJ),DIMENSION(:),POINTER :: METEO,METEO_BU
 CHARACTER(LEN=256) :: LINE

 DO I=1,ABS(IIPF); IF(TS(I)%IPFTYPE.NE.3)RETURN; ENDDO

 IF(PEST_ITER.EQ.0)THEN
  CALL IMOD_UTL_OPENASC(IU,'mete_svat.inp'    ,'R')
 ELSE
  CALL IMOD_UTL_OPENASC(IU,'mete_svat_org.inp','R')
 ENDIF

 ALLOCATE(METEO(100)); J=SIZE(METEO)
 I=0
 DO
  I=I+1
  IF(I.GT.J)THEN
   ALLOCATE(METEO_BU(I+100))
   METEO_BU(1:J)=METEO
   DEALLOCATE(METEO); METEO=>METEO_BU
   NULLIFY(METEO_BU); J=SIZE(METEO)
  ENDIF
  READ(IU,'(F15.0,I5,2F10.0,I10)',IOSTAT=IOS) METEO(I)%D,METEO(I)%IY,METEO(I)%N,METEO(I)%E,METEO(I)%IS
  IF(IOS.NE.0)EXIT
 ENDDO
 I=I-1; CLOSE(IU)

 IF(PEST_ITER.EQ.0)THEN
  CALL IMOD_UTL_OPENASC(IU,'mete_svat_org.inp','W')
  DO J=1,I; WRITE(IU,'(F15.2,I5,2F10.2,I10)',IOSTAT=IOS) METEO(J)%D,METEO(I)%IY,METEO(J)%N,METEO(J)%E,METEO(J)%IS; ENDDO
 ENDIF
 CLOSE(IU)

 CALL IMOD_UTL_OPENASC(IU,'mete_svat.inp','W')
 DO J=1,I
  METEO(J)%N=RRECHSTRUCNIV*1000.0
  IF(METEO(J)%D.EQ.1.0)METEO(J)%N=METEO(J)%N+(RCH_IMPULSE_SIZE*1000.0)  !## impulse in meters
  WRITE(IU,'(F15.2,I5,2F10.2,I10)',IOSTAT=IOS) METEO(J)%D,METEO(I)%IY,METEO(J)%N,METEO(J)%E,METEO(J)%IS
 ENDDO
 CLOSE(IU)

 DEALLOCATE(METEO); CLOSE(IU)

 LINE=' * adjusted mete_svat.inp with impulse of '//TRIM(IMOD_UTL_RTOS(RCH_IMPULSE_SIZE,'G',4))
 CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),-1,IUPESTOUT) !; CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),1)

 END SUBROUTINE PEST1_METEO_METASWAP

 !#####=================================================================
 LOGICAL FUNCTION PESTNEXT()
 !#####=================================================================
 IMPLICIT NONE
 REAL :: IMPROVEMENT
 INTEGER :: I

 PESTNEXT=.FALSE.

 IF(PEST_IGRAD.EQ.0)CALL PESTOPENFILE(IUPESTRESIDUAL,'log_pest_residual_'//TRIM(IMOD_UTL_ITOS(PEST_ITER)),'txt',0)

 IF(PEST_ITER.EQ.0)PEST_ITER=1

 !## compute objective function
 CALL PEST_GETJ()

 IF(LSENS)THEN
!  !## next parameter combination
!  IF(.NOT.PESTNEXTSENS())STOP
  IF(.NOT.PESTNEXTGRAD())STOP
 ELSEIF(LGRAD)THEN
  !## what proces is going on?
  IF(.NOT.PESTNEXTGRAD())THEN
   !## get gradient
   CALL PESTGRADIENT()
   LLNSRCH=.TRUE.; PEST_ILNSRCH=1; LGRAD=.FALSE.; PEST_IGRAD=0
  ENDIF
 ELSEIF(LLNSRCH)THEN
  !## no reduction of objection function, change (u(i))
  IF(TJ.GT.TJOBJ)THEN
!   DAMPINGFACTOR=DAMPINGFACTOR*NDAMPING
!   NDAMPING=1.0 !## do it onces only
   IF(.NOT.PESTUPGRADEVECTOR(0.5,.TRUE.))THEN !,.TRUE.))THEN
    STOP 'ERROR PESTUPGRADEVECTOR IN LINESEARCH'
   ENDIF !# half of current search-gradient
   !## start next line-search
   PEST_ILNSRCH=PEST_ILNSRCH+1
  ELSE
!   DAMPINGFACTOR=DAMPINGFACTOR/NDAMPING
!   NDAMPING=ININDAMPING !## do it onces only

   !## continue ?
   IF(PEST_ITER+1.GT.PEST_NITER)THEN
    PESTNEXT=.TRUE.  !## max. number of iterations reached
    CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT); CALL IMOD_UTL_PRINTTEXT('Pest iteration terminated: PEST_ITER ('//TRIM(IMOD_UTL_ITOS(PEST_ITER))//') = PEST_NITER ('//TRIM(IMOD_UTL_ITOS(PEST_NITER))//')',-1,IUPESTOUT)
   ENDIF
   IF(TJ.LE.0.0)THEN
    PESTNEXT=.TRUE.
    CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT); CALL IMOD_UTL_PRINTTEXT('Objective Function <= 0.0 ('//TRIM(IMOD_UTL_RTOS(REAL(TJ),'G',7))//')',-1,IUPESTOUT)
   ENDIF

   IMPROVEMENT=0; DO I=1,SIZE(PARAM)
    SELECT CASE (PARAM(I)%PTYPE)
     CASE ('KD','KH','KV','VC','SC','RC','RI','IC','II','DC','AF','MS','MC','VA','HF')
      IMPROVEMENT=IMPROVEMENT+ABS(EXP(PARAM(I)%ALPHA(2))-EXP(PARAM(I)%ALPHA(1)))/EXP(PARAM(I)%ALPHA(2))
     CASE DEFAULT
      IMPROVEMENT=IMPROVEMENT+ABS(PARAM(I)%ALPHA(2)-PARAM(I)%ALPHA(1))/PARAM(I)%ALPHA(2)
    END SELECT
   ENDDO

   CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT);
   CALL IMOD_UTL_PRINTTEXT('Total Parameter Adjustment (%)          '//IMOD_UTL_RTOS(IMPROVEMENT*100.0,'G',7),-1,IUPESTOUT)
   IMPROVEMENT=(TJOBJ-TJ)*IMPROVEMENT
   CALL IMOD_UTL_PRINTTEXT('Total Parameter Adjustment*dObjFunc (%) '//IMOD_UTL_RTOS(IMPROVEMENT*100.0,'G',7),-1,IUPESTOUT)
   IF(IMPROVEMENT.LE.PEST_JSTOP)THEN
    PESTNEXT=.TRUE.  !## min. improvement reached
    CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT); CALL IMOD_UTL_PRINTTEXT('Pest iteration terminated decrease objective function ('//TRIM(IMOD_UTL_RTOS(100.0*IMPROVEMENT,'G',7))// &
     '%) > PEST_JSTOP ('//TRIM(IMOD_UTL_RTOS(100.0*PEST_JSTOP,'G',7))//'%)',-1,IUPESTOUT)
   ENDIF

   CALL IMOD_UTL_PRINTTEXT('Objective Function Value (m2)           '//TRIM(IMOD_UTL_RTOS(REAL(TJ),'G',7)),-1,IUPESTOUT)
   CALL IMOD_UTL_PRINTTEXT('Objective Function Value (m)            '//TRIM(IMOD_UTL_RTOS(REAL(SQRT(TJ)),'G',7)),-1,IUPESTOUT)
   CALL IMOD_UTL_PRINTTEXT('Mean Objective Function Value (m2)      '//TRIM(IMOD_UTL_RTOS(REAL(TJ)/REAL(PEST_NOBS),'G',7)),-1,IUPESTOUT)
   CALL IMOD_UTL_PRINTTEXT('Mean Objective Function Value (m)       '//TRIM(IMOD_UTL_RTOS(REAL(SQRT(TJ))/REAL(PEST_NOBS),'G',7)),-1,IUPESTOUT)
   CALL IMOD_UTL_PRINTTEXT('Previous Objective Function Value (m2)  '//TRIM(IMOD_UTL_RTOS(REAL(TJOBJ),'G',7)),-1,IUPESTOUT)

   TJOBJ=TJ
   !## replace old by new parameter values
   PARAM%ALPHA(2)=PARAM%ALPHA(1)
   CALL PESTDUMPFCT()

   !## next iteration
   PEST_ITER=PEST_ITER+1
   IF(.NOT.PESTNEXT)THEN; CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT); CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT) ; CALL IMOD_UTL_PRINTTEXT(' * Next Outer Iteration *',-1,IUPESTOUT); ENDIF
   LLNSRCH=.FALSE.; LGRAD=.TRUE.; PEST_IGRAD=0; PEST_ILNSRCH=0
   IF(.NOT.PESTNEXTGRAD())THEN
   ENDIF
  ENDIF

 ENDIF

 CALL FLUSH(IUPESTOUT); CALL FLUSH(IUPESTPROGRESS)
 IF(IUPESTRESIDUAL.GT.0)THEN; CLOSE(IUPESTRESIDUAL); IUPESTRESIDUAL=0; ENDIF

 !IMULT=MAX(0,IMULT-1)

 END FUNCTION PESTNEXT

 !#####=================================================================
 SUBROUTINE PESTOPENFILE(IU,FNAME,EXT,INEW)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME,EXT
 INTEGER,INTENT(IN) :: INEW
 INTEGER,INTENT(OUT) :: IU
 INTEGER :: IOS
 LOGICAL :: LEX

 !## open pest result file
 LINE=TRIM(ROOTRES)//CHAR(92)//'pest'//CHAR(92)//TRIM(FNAME)//TRIM(CVERSION)//'.'//TRIM(EXT)
 
 CALL IMOD_UTL_CREATEDIR(LINE(:INDEX(LINE,CHAR(92),.TRUE.)-1))
 CALL IMOD_UTL_FILENAME(LINE); INQUIRE(FILE=LINE,EXIST=LEX)
 IF(INEW.EQ.0)THEN
  CALL IMOD_UTL_OPENASC(IU,LINE,'W')
 ELSE
  IU=IMOD_UTL_GETUNIT()
  OPEN(IU,FILE=LINE,STATUS='OLD',ACCESS='APPEND',IOSTAT=IOS)
  IF(IOS.NE.0)IU=0
 ENDIF
 IF(IU.LE.0)CALL IMOD_UTL_PRINTTEXT('Cannot open PEST-progress file '//TRIM(LINE),2)

 END SUBROUTINE PESTOPENFILE

 !#####=================================================================
 SUBROUTINE PESTDUMPFCT()
 !#####=================================================================
 use rf2mf_module, only: ncol, nrow, nlay, nper
 IMPLICIT NONE
 CHARACTER(LEN=256) :: FNAME,DIR
 INTEGER :: I,J,IROW,ICOL
 REAL,ALLOCATABLE,DIMENSION(:,:) :: X

 !## open pest factor files
 DIR=TRIM(ROOTRES)//CHAR(92)//'pest'//CHAR(92)//'factors'//TRIM(IMOD_UTL_ITOS(PEST_ITER))
 CALL IMOD_UTL_CREATEDIR(DIR)

 ALLOCATE(X(NCOL,NROW))
 DO I=1,SIZE(PARAM)
  !## do not save non-optimized parameters
  IF(PARAM(I)%IACT.EQ.0)CYCLE
  X=-999.0
  SELECT CASE (PARAM(I)%PTYPE)
   CASE ('KD','KH','KV','VC','SC','RC','RI','IC','II','DC','AF','MS','MC','VA','HF')
    DO J=1,PARAM(I)%NODES
     IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
     X(ICOL,IROW)=EXP(PARAM(I)%ALPHA(2))
    ENDDO
   CASE ('AA')
    DO J=1,PARAM(I)%NODES
     IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
     X(ICOL,IROW)=PARAM(I)%ALPHA(2)
    ENDDO
   CASE DEFAULT
    STOP 'PARAMETER TYPE NOT RECOGNIZED'
  END SELECT
  WRITE(FNAME,'(A,2I5.5,A)') TRIM(DIR)//CHAR(92)//PARAM(I)%PTYPE,PARAM(I)%ILS,PARAM(I)%IZONE,'.IDF'

 ENDDO
 DEALLOCATE(X)

 END SUBROUTINE PESTDUMPFCT

 !#####=================================================================
 SUBROUTINE PESTPROGRESS()
 !#####=================================================================
 IMPLICIT NONE
 REAL,ALLOCATABLE,DIMENSION(:) :: X
 INTEGER :: I,J

 ALLOCATE(X(SIZE(PARAM)))

 DO I=1,SIZE(PARAM)
  SELECT CASE (TRIM(PARAM(I)%PTYPE))
   CASE ('KD','KH','KV','VC','SC','RC','RI','IC','II','DC','AF','MS','MC','VA','HF'); X(I)=EXP(PARAM(I)%ALPHA(1))
   CASE DEFAULT; X(I)=PARAM(I)%ALPHA(1)
  END SELECT
 ENDDO

 WRITE(BLINE,'(3I5,E15.7)') PEST_ITER,PEST_IGRAD,PEST_ILNSRCH,TJ
 DO I=1,SIZE(PARAM)
  IF(ABS(PARAM(I)%IACT).EQ.1.AND.PARAM(I)%IGROUP.GT.0)THEN
   WRITE(SLINE,'(F10.3)') X(I)
   BLINE=TRIM(BLINE)//TRIM(SLINE)
  ENDIF
 ENDDO

 CALL PESTOPENFILE(IUPESTPROGRESS,'log_pest_progress_','txt',PEST_ITER)
 WRITE(IUPESTPROGRESS,'(A)') TRIM(BLINE) !3I5,E15.7,999F10.3)') PEST_ITER,PEST_IGRAD,PEST_ILNSRCH,TJ,(X(I),I=1,SIZE(PARAM))
 DEALLOCATE(X)

 CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT)

 END SUBROUTINE PESTPROGRESS

 !#####=================================================================
 LOGICAL FUNCTION PESTNEXTGRAD()
 !#####=================================================================
 IMPLICIT NONE
 INTEGER :: I

 PESTNEXTGRAD=.TRUE.

 DO
  PEST_IGRAD=PEST_IGRAD+1
  !## all gradients processed
  IF(PEST_IGRAD.GT.SIZE(PARAM))EXIT
  !## zero gradient in case parameter is fixed
  IF(PARAM(PEST_IGRAD)%IACT.EQ.0)THEN
   DH(PEST_IGRAD,:)=DH(0,:)
  !## check whether the parameters has been modified allready since it belongs to the same group
  ELSEIF(PARAM(PEST_IGRAD)%IGROUP.LT.0)THEN
   DH(PEST_IGRAD,:)=DH(0,:)
  !## possible candidate found
  ELSE
   EXIT
  ENDIF
 ENDDO
 !## proceed gradient
 IF(PEST_IGRAD.LE.SIZE(PARAM))THEN
  !## reset all alpha's
  PARAM%ALPHA(1)=PARAM%ALPHA(2)
  !## adjust all parameters within the same group
  DO I=PEST_IGRAD,SIZE(PARAM)
   IF(ABS(PARAM(I)%IGROUP).EQ.ABS(PARAM(PEST_IGRAD)%IGROUP))PARAM(I)%ALPHA(1)=PARAM(I)%ALPHA(2)+PARAM(I)%DELTA
  ENDDO
 ELSE
  PESTNEXTGRAD=.FALSE.
 ENDIF

 END FUNCTION PESTNEXTGRAD

 !###====================================================================
 SUBROUTINE PESTGRADIENT()
 !###====================================================================
 IMPLICIT NONE
 DOUBLE PRECISION :: DJ1,DJ2
 REAL :: B1,TS,DF1,DF2,BETA,EIGWTHRESHOLD
 INTEGER :: I,II,J,K,L,NP,MP,IP1,IP2,NE,ISING
 INTEGER,ALLOCATABLE,DIMENSION(:) :: INDX,ICOR
 REAL,ALLOCATABLE,DIMENSION(:,:) :: B,TDJ,C,JS,EIGV,P,M,PT
 REAL,ALLOCATABLE,DIMENSION(:) :: S,GAMMA,EIGW,N,RU !,DJ
 CHARACTER(LEN=8),ALLOCATABLE,DIMENSION(:) :: TXT
 CHARACTER(LEN=8) :: DTXT
 LOGICAL :: LSCALING,LSVD

 SELECT CASE (PEST_ISCALING)
  CASE (0)
   LSCALING=.FALSE.; LSVD=.FALSE.
  CASE (1)
   LSCALING=.TRUE.;  LSVD=.FALSE.
  CASE (2)
   LSCALING=.TRUE.;  LSVD=.TRUE.
  CASE (3)
   LSCALING=.FALSE.; LSVD=.TRUE.
 END SELECT

 NP=SIZE(PARAM)
 !## sensitivity
 IF(.NOT.ALLOCATED(S)) ALLOCATE(S (NP)); S =0.0
! IF(.NOT.ALLOCATED(DJ))ALLOCATE(DJ(NP)); DJ=0.0
 DO IP1=1,NP
  IF(ABS(PARAM(IP1)%IACT).NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
  SELECT CASE (TRIM(PARAM(IP1)%PTYPE))
   CASE ('KD','KH','KV','VC','SC','RC','RI','DC','IC','II','AF','VA','HF','MS','MC');
    DF1=EXP(PARAM(IP1)%ALPHA(2)+PARAM(IP1)%DELTA)-EXP(PARAM(IP1)%ALPHA(2))
   CASE DEFAULT
    DF1=   (PARAM(IP1)%ALPHA(2)+PARAM(IP1)%DELTA)-    PARAM(IP1)%ALPHA(2)
  END SELECT
  DO J=1,PEST_NOBS
   S(IP1)=S(IP1)+W(J)*((DH(IP1,J)-DH(0,J))/DF1)
!   S(IP1)=S(IP1)+((DH(IP1,J)-DH(0,J))/DF1)
  ENDDO
 ENDDO
! DJ=S;
 DO I=1,NP; S(I)=S(I)/REAL(PEST_NOBS); ENDDO

 WRITE(BLINE,'(A30)') '              Sensitivity (.):'
 DO I=1,NP
  IF(ABS(PARAM(I)%IACT).NE.1.OR.PARAM(I)%IGROUP.LE.0)CYCLE
  WRITE(SLINE,'(E10.4)') S(I)
  BLINE=TRIM(BLINE)//TRIM(SLINE)
 ENDDO
 WRITE(IUPESTPROGRESS,'(A)') TRIM(BLINE) !A15,15X,999E10.4)') 'Sensitivity (.)',(S(I),I=1,NP)

 TS=SUM(ABS(S)); DO IP1=1,NP
  IF(ABS(PARAM(IP1)%IACT).NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
  IF(TS.NE.0.0)S(IP1)=S(IP1)/TS
 ENDDO
 S=ABS(S)*100.0

 WRITE(BLINE,'(A30)') '              Sensitivity (%):'
 DO I=1,NP
  IF(ABS(PARAM(I)%IACT).NE.1.OR.PARAM(I)%IGROUP.LE.0)CYCLE
  WRITE(SLINE,'(F10.3)') S(I)
  BLINE=TRIM(BLINE)//TRIM(SLINE)
 ENDDO
 WRITE(IUPESTPROGRESS,'(A)') TRIM(BLINE) !A15,15X,999F10.3)') 'Sensitivity (%)',(S(I),I=1,NP)

 CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT); CALL IMOD_UTL_PRINTTEXT('Parameter Upgrade Vector (gradient)',-1,IUPESTOUT)

 !## reset parameters
 DO I=1,SIZE(PARAM); PARAM(I)%ALPHA(1)=PARAM(I)%ALPHA(2); ENDDO

 !## melt frozen parameters
 DO IP1=1,SIZE(PARAM); IF(PARAM(IP1)%IACT.EQ.-1.AND.PARAM(IP1)%IGROUP.GT.0)PARAM(IP1)%IACT=1; ENDDO

 !## freeze-insensitive parameters
 DO I=1,SIZE(PARAM)
  IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0.AND.S(I).LT.PEST_SENSITIVITY)PARAM(I)%IACT=-1
 ENDDO

 !## freeze parameters that bounced against the boundary in the previous iteration and point in that direction again
 DO

  NP=0; DO I=1,SIZE(PARAM); IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0)NP=NP+1; ENDDO

  IF(ALLOCATED(JQJ))DEALLOCATE(JQJ);   ALLOCATE(JQJ (NP,NP))
  IF(ALLOCATED(JQR))DEALLOCATE(JQR);   ALLOCATE(JQR (NP))
  IF(ALLOCATED(U  ))DEALLOCATE(U);     ALLOCATE(U   (NP))
  IF(ALLOCATED(EIGW))DEALLOCATE(EIGW); ALLOCATE(EIGW(NP))
  IF(ALLOCATED(EIGV))DEALLOCATE(EIGV); ALLOCATE(EIGV(NP,NP))
  IF(ALLOCATED(C   ))DEALLOCATE(C);    ALLOCATE(C   (NP,NP))

  !## construct jTqr (<--- r is residual for current parameter set)
  JQR=0.0; I=0
  DO IP1=1,SIZE(PARAM)  !## row

   IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE

   DF1=   (PARAM(IP1)%ALPHA(2)+PARAM(IP1)%DELTA)-    PARAM(IP1)%ALPHA(2)

   I=I+1
   DO J=1,PEST_NOBS
    DJ1=(DH(IP1,J)-DH(0,J))/DF1
    DJ2= DH(0 ,J)
    JQR(I)=JQR(I)+(DJ1*W(J)*DJ2)
   ENDDO
  ENDDO

  !## melt parameters that point inwards again since their last bump on the boundary
  I=0; DO IP1=1,SIZE(PARAM)
   IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
   I=I+1
   IF(ABS(PARAM(IP1)%IBND).EQ.1)THEN
    IF(PARAM(IP1)%IBND.EQ.-1.AND.-1.0*JQR(I).LT.0.0)PARAM(IP1)%IACT=-1 !## still outside parameter domain(min)
    IF(PARAM(IP1)%IBND.EQ. 1.AND.-1.0*JQR(I).GT.0.0)PARAM(IP1)%IACT=-1 !## still outside parameter domain(max)
   ENDIF
  ENDDO

  MP=0; DO I=1,SIZE(PARAM); IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0)MP=MP+1; ENDDO
  IF(MP.EQ.NP)EXIT

 ENDDO

 !## compute hessian and covariance, correlation matrix and eigenvalues/eigenvectors
 CALL PEST1JQJ(JQJ,EIGW,EIGV,NP,.TRUE.)

 IF(LSCALING)THEN
  IF(ALLOCATED(JS))DEALLOCATE(JS); ALLOCATE(JS(NP,PEST_NOBS))
 ENDIF

 !## find until parameter update within hypersphere of parameters
 !## initiate marquardt as small as possible
 MARQUARDT=0.001
 DO

  !## construct jqj - NORMAL MATRIX/HESSIAN
  CALL PEST1JQJ(JQJ,EIGW,EIGV,NP,.FALSE.)

  IF(.NOT.LSCALING)THEN

  !## levenberg
   DO I=1,NP; JQJ(I,I)=JQJ(I,I)+MARQUARDT; ENDDO

   !## apply scaling
  ELSE
   !## compute scaling matrix
   C=0.0; DO I=1,NP; C(I,I)=1.0/SQRT(JQJ(I,I)); ENDDO

   !## construct JS matrix, scaled
   JS=0.0
   DO I=1,PEST_NOBS    !## row
    J=0
    DO IP1=1,SIZE(PARAM)
     IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
     DF1=(PARAM(IP1)%ALPHA(2)+PARAM(IP1)%DELTA)-PARAM(IP1)%ALPHA(2)
     J=J+1
     DJ1=(DH(IP1,I)-DH(0,I))/DF1
     JS(J,I)=JS(J,I)+DJ1*C(J,J)
    ENDDO
   ENDDO
   !## construct JS-Q-JS - SCALED NORMAL MATRIX
   JQJ=0.0
   DO I=1,NP     !## row
    DO J=1,NP    !## column
     DO II=1,PEST_NOBS
      DJ1=JS(I,II)
      DJ2=JS(J,II)
      JQJ(J,I)=JQJ(J,I)+(DJ1*W(II)*DJ2)
     ENDDO
    ENDDO
   ENDDO

   !## construct jTqr (<--- r is residual for current parameter set)
   JQR=0.0; I=0
   DO IP1=1,SIZE(PARAM)  !## row
    IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
    DF1=   (PARAM(IP1)%ALPHA(2)+PARAM(IP1)%DELTA)-    PARAM(IP1)%ALPHA(2)
    I=I+1
    DO J=1,PEST_NOBS
     DJ1=JS(I,J) !(DH(IP1,J)-DH(0,J))/DF1
     DJ2= DH(0 ,J)
     JQR(I)=JQR(I)+(DJ1*W(J)*DJ2)
    ENDDO
   ENDDO

   !## add levenberg/marquardt
   DO I=1,NP; JQJ(I,I)=JQJ(I,I)+MARQUARDT*C(I,I)**2.0; ENDDO

  ENDIF

  !## project on important singular values
  IF(LSVD)THEN

   EIGWTHRESHOLD=0.0 !% explained variance
   DO NE=1,NP
    EIGWTHRESHOLD=EIGWTHRESHOLD+EIGW(NE)
    IF(EIGWTHRESHOLD.GT.99.0)EXIT
   ENDDO
   ALLOCATE(P(NP,NE)); P(:,1:NE)=EIGV(:,1:NE); ALLOCATE(M(NE,NE),N(NE),RU(NE),PT(NE,NP))

   !## compute pp=pt(jqj) on eigen-space
   PT=0.0; DO I=1,NE; DO J=1,NP
    DO K=1,NP
     PT(I,J)=PT(I,J)+P(K,I)*JQJ(K,J)
    ENDDO
   ENDDO; ENDDO
   !## project jqj on eigen-space
   M=0.0; DO I=1,NE; DO J=1,NE
    DO K=1,NP
     M(I,J)=M(I,J)+PT(I,K)*P(K,J)
    ENDDO
   ENDDO; ENDDO
   !## project right hand side on eigenspace
   N=0.0; DO I=1,NE
    DO K=1,NP
     N(I)=N(I)+P(K,I)*JQR(K)
    END DO
   ENDDO

   !## compute inverse of (Pt(JQJ)P)-1 -> B
   IF(ALLOCATED(INDX))DEALLOCATE(INDX); ALLOCATE(INDX(NE))
   IF(ALLOCATED(B   ))DEALLOCATE(B);    ALLOCATE(B   (NE,NE))
   CALL IMOD_UTL_LUDECOMP(M,INDX,NE,ISING)
   B=0.0; DO I=1,NE; B(I,I)=1.0; ENDDO
   DO I=1,NE; CALL IMOD_UTL_LUBACKSUB(M,INDX,B(1,I),NE); ENDDO

   !## compute U=(M)-1*N
   RU=0.0; DO I=1,NE; DO J=1,NE
    RU(I)=RU(I)+(B(J,I)*N(J))
   ENDDO; ENDDO

   !## reproject reduced gradient on original space
   !## compute U=(M)-1*N
   U=0.0; DO I=1,NP; DO J=1,NE
    U(I)=U(I)+(P(I,J)*RU(J))
   ENDDO; ENDDO

   DEALLOCATE(P,PT,M,N,RU,INDX,B)
  ELSE

   !## compute inverse of (JQJ)-1 -> B
   IF(ALLOCATED(INDX))DEALLOCATE(INDX); ALLOCATE(INDX(NP))
   IF(ALLOCATED(B   ))DEALLOCATE(B);    ALLOCATE(B   (NP,NP))
   CALL IMOD_UTL_LUDECOMP(JQJ,INDX,NP,ISING)
   B=0.0; DO I=1,NP; B(I,I)=1.0; ENDDO
   DO I=1,NP; CALL IMOD_UTL_LUBACKSUB(JQJ,INDX,B(1,I),NP); ENDDO

   !## compute (JQJ)-1*JQR
   U=0.0
   DO I=1,NP; DO J=1,NP
    U(I)=U(I)+(B(J,I)*JQR(J))
   ENDDO; ENDDO

   DEALLOCATE(INDX,B)

  ENDIF

  !## apply scaling
  IF(LSCALING)THEN
   DO I=1,NP
    U(I)=U(I)*C(I,I)
   ENDDO
  ENDIF

  U=-1.0*U !# pointing downhill

  !## within parameter adjust-limits
  IF(PESTUPGRADEVECTOR(1.0,.TRUE.))EXIT
  MARQUARDT=MARQUARDT*DAMPINGFACTOR

 ENDDO !## marquardt-loop

 WRITE(IUPESTPROGRESS,*) 'Lambda/Damping Factor = ',MARQUARDT
 IF(LSCALING)WRITE(IUPESTPROGRESS,*) 'Scaling Value = ',MAXVAL(C)
 IF(LSVD)WRITE(IUPESTPROGRESS,*) 'Number of eigenvalues used: ',NE

 DEALLOCATE(S,C,EIGW,EIGV)  !,DJ
 IF(ALLOCATED(JS))DEALLOCATE(JS)

 END SUBROUTINE PESTGRADIENT

 !###====================================================================
 SUBROUTINE PEST1JQJ(JQJ,EIGW,EIGV,NP,LVARIANCE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NP
 LOGICAL,INTENT(IN) :: LVARIANCE
 REAL,DIMENSION(NP,NP),INTENT(INOUT) :: JQJ,EIGV
 REAL,DIMENSION(NP),INTENT(INOUT) :: EIGW
 INTEGER :: I,J,IP1,IP2,II,ISING
 REAL :: DF1,DF2,DJ1,DJ2,B1,R1,R2,TV,TEV
 REAL,ALLOCATABLE,DIMENSION(:,:) :: COR,B
 REAL,ALLOCATABLE,DIMENSION(:) :: E,X
 INTEGER,ALLOCATABLE,DIMENSION(:) :: INDX

 !## construct jqj - NORMAL MATRIX/HESSIAN
 JQJ=0.0; I=0
 DO IP1=1,SIZE(PARAM)    !## row
  IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
  DF1=   (PARAM(IP1)%ALPHA(2)+PARAM(IP1)%DELTA)-    PARAM(IP1)%ALPHA(2)
  I=I+1; II=0; DO IP2=1,SIZE(PARAM)  !## column
   IF(PARAM(IP2)%IACT.NE.1.OR.PARAM(IP2)%IGROUP.LE.0)CYCLE
   DF2=   (PARAM(IP2)%ALPHA(2)+PARAM(IP2)%DELTA)-    PARAM(IP2)%ALPHA(2)
   II=II+1
   DO J=1,PEST_NOBS
    DJ1=(DH(IP1,J)-DH(0,J))/DF1
    DJ2=(DH(IP2,J)-DH(0,J))/DF2
    JQJ(II,I)=JQJ(II,I)+(DJ1*W(J)*DJ2)
   ENDDO
  ENDDO
 ENDDO

 IF(LVARIANCE)THEN

  IF(ALLOCATED(E  ))DEALLOCATE(E);     ALLOCATE(E   (NP))
  IF(ALLOCATED(COR ))DEALLOCATE(COR);  ALLOCATE(COR(NP,NP))
  IF(ALLOCATED(INDX))DEALLOCATE(INDX); ALLOCATE(INDX(NP))
  IF(ALLOCATED(B   ))DEALLOCATE(B);    ALLOCATE(B(NP,NP))

  !## compute inverse of (JQJ)-1 -> B
  CALL IMOD_UTL_LUDECOMP(JQJ,INDX,NP,ISING)
  B=0.0; DO I=1,NP; B(I,I)=1.0; ENDDO
  DO I=1,NP; CALL IMOD_UTL_LUBACKSUB(JQJ,INDX,B(1,I),NP); ENDDO

  !## parameter covariance matrix
  B1=(TJ/(MAX(1,PEST_NOBS-NP))) !**2.0
  DO I=1,NP; DO J=1,NP; B(I,J)=B1*B(I,J); ENDDO; ENDDO

  WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'Parameter Covariance Matrix'; WRITE(IUPESTOUT,*)

  BLINE=''
  DO J=1,SIZE(PARAM)
   IF(PARAM(J)%IACT.EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
    WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PARAM(J)%PTYPE,PARAM(J)%ILS,PARAM(J)%IZONE,'-',PARAM(J)%IGROUP
    BLINE=TRIM(BLINE)//TRIM(SLINE)
   ENDIF
  ENDDO
  WRITE(IUPESTOUT,'(15X,A)') TRIM(BLINE)
  WRITE(IUPESTOUT,'(A)')

  I=0
  DO IP1=1,SIZE(PARAM)
   IF(PARAM(IP1)%IACT.EQ.1.AND.PARAM(IP1)%IGROUP.GT.0)THEN
    I=I+1
    WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PARAM(IP1)%PTYPE,PARAM(IP1)%ILS,PARAM(IP1)%IZONE,'-',PARAM(IP1)%IGROUP
    WRITE(IUPESTOUT,'(A15,999F15.7)') TRIM(SLINE),(B(I,J),J=1,NP)
   ENDIF
  ENDDO

  !## parameter correlation matrix
  WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'Parameter Correlation Matrix'; WRITE(IUPESTOUT,*)

  BLINE=''
  DO J=1,SIZE(PARAM)
   IF(PARAM(J)%IACT.EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
    WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PARAM(J)%PTYPE,PARAM(J)%ILS,PARAM(J)%IZONE,'-',PARAM(J)%IGROUP
    BLINE=TRIM(BLINE)//TRIM(SLINE)
   ENDIF
  ENDDO
  WRITE(IUPESTOUT,'(15X,A)') TRIM(BLINE)
  WRITE(IUPESTOUT,'(A)')

  COR=0.0; I=0
  DO IP1=1,SIZE(PARAM)
   IF(PARAM(IP1)%IACT.EQ.1.AND.PARAM(IP1)%IGROUP.GT.0)THEN
    I=I+1
    DO J=1,NP; COR(I,J)=B(I,J)/SQRT(B(I,I)*B(J,J)); ENDDO
    WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PARAM(IP1)%PTYPE,PARAM(IP1)%ILS,PARAM(IP1)%IZONE,'-',PARAM(IP1)%IGROUP
    WRITE(IUPESTOUT,'(A15,999F15.7)') TRIM(SLINE),(COR(I,J),J=1,NP)
   ENDIF
  ENDDO

  !## eigenvalue of covariance matrix
  CALL RED1TRED2(B,NP,NP,EIGW,E)
  CALL RED1TQLI(EIGW,E,NP,NP,B)
  CALL RED1EIGSRT(EIGW,B,NP,NP)
  WRITE(IUPESTOUT,'(/10X,4A15)') 'Eigenvalues','Sing.Values','Variance','Explained Var.'
  DO I=1,NP; IF(EIGW(I).LE.0.0)EIGW(I)=0.0; ENDDO; TEV=SUM(EIGW)
  TV=0.0
  DO I=1,NP
   TV=TV+(EIGW(I)*100.0/TEV)
   IF(EIGW(I).GT.0.0)THEN
    WRITE(IUPESTOUT,'(I10,4F15.7)') I,EIGW(I),SQRT(EIGW(I)),EIGW(I)*100.0/TEV,TV
   ELSE
    WRITE(IUPESTOUT,'(I10,4F15.7)') I,EIGW(I),     EIGW(I) ,EIGW(I)*100.0/TEV,TV
   ENDIF
  ENDDO
  EIGV= B
  EIGW=(EIGW*100.0)/SUM(EIGW)

  IF(ALLOCATED(E   ))DEALLOCATE(E)
  IF(ALLOCATED(COR ))DEALLOCATE(COR)
  IF(ALLOCATED(INDX))DEALLOCATE(INDX)
  IF(ALLOCATED(B   ))DEALLOCATE(B)

 ENDIF

 END SUBROUTINE PEST1JQJ

 !###====================================================================
 LOGICAL FUNCTION PESTUPGRADEVECTOR(FCT,LCHECK)
 !###====================================================================
 IMPLICIT NONE
 LOGICAL :: LCHECK
 REAL,INTENT(IN) :: FCT
 REAL :: F,G,MINP,MAXP,MINAP,MAXAP
 INTEGER :: I,J,IP1,IP2,N
 REAL,ALLOCATABLE,DIMENSION(:) :: GRADUPDATE

 !## exit code
 PESTUPGRADEVECTOR=.FALSE.

! PARAM%IBND=0
 I=0; DO IP1=1,SIZE(PARAM)
  !## inactive parameter
  IF(PARAM(IP1)%IACT.NE.1)THEN
   G=0.0
  !## find gradient for group
  ELSEIF(PARAM(IP1)%IGROUP.LE.0)THEN
   J=0; DO IP2=1,SIZE(PARAM)
    IF(PARAM(IP2)%IACT.EQ.1.AND.PARAM(IP2)%IGROUP.GT.0)THEN
     J=J+1
     IF(ABS(PARAM(IP1)%IGROUP).EQ.PARAM(IP1)%IGROUP)THEN
      G=U(J); EXIT
     ENDIF
    ENDIF
   ENDDO
  ELSE
   I=I+1; G=U(I)
  ENDIF

  PARAM(IP1)%ALPHA(1)=PARAM(IP1)%ALPHA(2)+G*FCT !## update parameters

  !## adjustment too large
  IF(LCHECK)THEN
   SELECT CASE (PARAM(IP1)%PTYPE)
    CASE ('KD','KH','KV','VC','SC','RC','IC','DC','AF','MS','MC','VA','HF')
     F=EXP(PARAM(IP1)%ALPHA(1))/EXP(PARAM(IP1)%ALPHA(2))
    CASE DEFAULT
     F=PARAM(IP1)%ALPHA(1)/PARAM(IP1)%ALPHA(2)
   END SELECT

   IF(F.LT.1.0/PARAM(IP1)%FADJ.OR. &
      F.GT.PARAM(IP1)%FADJ)THEN
    RETURN
   ENDIF
  ENDIF

 ENDDO

 !## check whether boundary has been hit or maximum adjustment exceeds
 IF(LCHECK)THEN

  DO IP1=1,SIZE(PARAM)
   IF(PARAM(IP1)%IACT.NE.1)CYCLE

   MINP=PARAM(IP1)%MIN
   MAXP=PARAM(IP1)%MAX

   SELECT CASE (PARAM(IP1)%PTYPE)
    CASE ('KD','KH','KV','VC','SC','RC','IC','DC','AF','MS','MC','VA','HF')
     MINAP=1.0/PARAM(IP1)%FADJ*EXP(PARAM(IP1)%ALPHA(2))
     MAXAP=PARAM(IP1)%FADJ    *EXP(PARAM(IP1)%ALPHA(2))
     MINAP=LOG(MINAP)
     MAXAP=LOG(MAXAP)
    CASE DEFAULT
     MINAP=1.0/PARAM(IP1)%FADJ*PARAM(IP1)%ALPHA(2)
     MAXAP=PARAM(IP1)%FADJ    *PARAM(IP1)%ALPHA(2)
   END SELECT

   MINP=MAX(MINP,MINAP)
   MAXP=MIN(MAXP,MAXAP)

   IF(PARAM(IP1)%ALPHA(1).LT.MINP.OR. &
      PARAM(IP1)%ALPHA(1).GT.MAXP)THEN

    !## correct gradient
    G=PARAM(IP1)%ALPHA(1)-PARAM(IP1)%ALPHA(2)
    IF(PARAM(IP1)%ALPHA(1).LT.MINP)F=MINP-PARAM(IP1)%ALPHA(2)
    IF(PARAM(IP1)%ALPHA(1).GT.MAXP)F=MAXP-PARAM(IP1)%ALPHA(2)

    IF(PARAM(IP1)%IBND.EQ.0)THEN
     !## correct all gradients with this factor
     F=F/G
     !## echo correction factor
     CALL IMOD_UTL_PRINTTEXT('Parameter '//TRIM(IMOD_UTL_ITOS(IP1))//' causing a',-1,IUPESTOUT)
     CALL IMOD_UTL_PRINTTEXT('Correction factor '//TRIM(IMOD_UTL_RTOS(F,'F',3))//' of Upgrade Vector caused by Bumping on the Parameter Boundary',-1,IUPESTOUT)
    ELSE
     CALL IMOD_UTL_PRINTTEXT('Parameter '//TRIM(IMOD_UTL_ITOS(IP1))//' causing a increase of the Marquardt factor',-1,IUPESTOUT)
     CALL IMOD_UTL_PRINTTEXT('Conflict on the boundary between a Steepest Descent and Gauss-Newton approach',-1,IUPESTOUT)
     !## get another values for the marquardt such that this will not happen
     RETURN
    ENDIF

    DO IP2=1,SIZE(PARAM)
     G=PARAM(IP2)%ALPHA(1)-PARAM(IP2)%ALPHA(2)
     G=G*F
     PARAM(IP2)%ALPHA(1)=PARAM(IP2)%ALPHA(2)+G !## update parameters
    ENDDO
   ENDIF

  ENDDO

 ENDIF

 PESTUPGRADEVECTOR=.TRUE.

 CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT); CALL IMOD_UTL_PRINTTEXT('Upgrade Vector Parameter:',-1,IUPESTOUT)
 WRITE(LINE,'(A5,1X,A2,1X,2A15,A4)') 'NO','PT','NEW.FACTOR','PREV.FACTOR','BND'
 CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),-1,IUPESTOUT)

 PARAM%IBND=0
 DO IP1=1,SIZE(PARAM)

  !## parameter is at the boundary whenever less than 1% away
  IF(ABS(PARAM(IP1)%ALPHA(1)-PARAM(IP1)%MIN).LT.XPBND)PARAM(IP1)%IBND=-1 !## min
  IF(ABS(PARAM(IP1)%MAX)-PARAM(IP1)%ALPHA(1).LT.XPBND)PARAM(IP1)%IBND= 1 !## max

  SELECT CASE (PARAM(IP1)%PTYPE)
   CASE ('KD','KH','KV','VC','SC','RC','RI','IC','II','DC','AF','MS','MC','VA','HF')
    WRITE(LINE,'(I5,1X,A2,1X,2F15.7,I4)') IP1,PARAM(IP1)%PTYPE,EXP(PARAM(IP1)%ALPHA(1)),EXP(PARAM(IP1)%ALPHA(2)),PARAM(IP1)%IBND
    PARAM(IP1)%ALPHA_HISTORY(PEST_ITER)=EXP(PARAM(IP1)%ALPHA(1))
   CASE DEFAULT
    WRITE(LINE,'(I5,1X,A2,1X,2F15.7,I4)') IP1,PARAM(IP1)%PTYPE,    PARAM(IP1)%ALPHA(1),     PARAM(IP1)%ALPHA(2), PARAM(IP1)%IBND
    PARAM(IP1)%ALPHA_HISTORY(PEST_ITER)=PARAM(IP1)%ALPHA(1)
  END SELECT
  CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),-1,IUPESTOUT)
 ENDDO

 CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT); CALL IMOD_UTL_PRINTTEXT('Upgrade Vector Parameter History:',-1,IUPESTOUT)
 WRITE(BLINE,'(A5,1X,A2,1X,99(A7,I3.3))') 'NO','PT',('   ITER',I,I=PEST_ITER,0,-1)
 CALL IMOD_UTL_PRINTTEXT(TRIM(BLINE),-1,IUPESTOUT)
 ALLOCATE(GRADUPDATE(PEST_ITER)); GRADUPDATE=0.0
 N=0
 DO IP1=1,SIZE(PARAM)
  WRITE(BLINE,'(I5,1X,A2,1X,99(F10.3))') IP1,PARAM(IP1)%PTYPE,(PARAM(IP1)%ALPHA_HISTORY(I),I=PEST_ITER,0,-1)
  CALL IMOD_UTL_PRINTTEXT(TRIM(BLINE),-1,IUPESTOUT)
  IF(PARAM(IP1)%IGROUP.NE.0)THEN
   N=N+1
   DO I=1,PEST_ITER
    GRADUPDATE(I)=GRADUPDATE(I)+(PARAM(IP1)%ALPHA_HISTORY(I)-PARAM(IP1)%ALPHA_HISTORY(I-1))**2.0
   ENDDO
  ENDIF
 ENDDO
 GRADUPDATE=SQRT(GRADUPDATE)
 WRITE(BLINE,'(9X,99F10.3)') (GRADUPDATE(I),I=PEST_ITER,1,-1)
 CALL IMOD_UTL_PRINTTEXT(TRIM(BLINE),-1,IUPESTOUT)

 IF(GRADUPDATE(PEST_ITER).LT.PEST_PADJ)THEN
  CALL IMOD_UTL_PRINTTEXT('Proces stopped, less than '//TRIM(IMOD_UTL_RTOS(PEST_PADJ,'F',3))//' of vector length',2)
 ENDIF

 DEALLOCATE(GRADUPDATE)

 I=0; DO IP1=1,SIZE(PARAM)
  IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
  I=I+1; U(I)=U(I)*FCT
!  PARAM(IP1)%ALPHA(1)=PARAM(IP1)%ALPHA(2)+U(I) !## update parameters
 ENDDO

 END FUNCTION PESTUPGRADEVECTOR

 !###====================================================================
 SUBROUTINE PEST_GETJ()
 !###====================================================================
 use rf2mf_module, only: ncol, nrow, nlay, nper
 IMPLICIT NONE
 INTEGER :: I,II,III,J,JJ,K,KK,ILAY,NROWIPFTXT,IUIPFTXT,NCOLIPFTXT,ITYPE,NHEAD,IOS
 REAL :: X,Y,Z,H,WW,MC,MM,DHH,XCOR,YCOR,ZCOR,XCROSS
 REAL :: RBASELEVEL,RIR_MO,RIR_MU,RIR_SIGMA,RTIMESTEP,STRUCNIV
 REAL,DIMENSION(4) :: RES
 CHARACTER(LEN=52) :: ID
 DOUBLE PRECISION :: DHW
 REAL,ALLOCATABLE,DIMENSION(:) :: TSNODATA,M,C
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IDATE
 LOGICAL :: LBASECALCULATED

 DO JJ=1,ABS(IIPF)

  !## read ipf
  IF(NPER.EQ.1)THEN
   I=INDEX(TS(JJ)%IPFNAME,CHAR(92),.TRUE.)+1; LINE=TRIM(ROOTRES)//CHAR(92)//TS(JJ)%IPFNAME(I:)
  ELSE
   I=INDEX(TS(JJ)%IPFNAME,CHAR(92),.TRUE.)+1
   LINE=TRIM(ROOTRES)//CHAR(92)//'timeseries'//CHAR(92)//TS(JJ)%IPFNAME(I:)
  ENDIF
  CALL IMOD_UTL_SWAPSLASH(LINE);  CALL IMOD_UTL_OPENASC(TS(JJ)%IUIPF,LINE,'R')
  READ(TS(JJ)%IUIPF,'(A256)') LINE; CALL IMOD_UTL_STRING(LINE); READ(LINE,*) TS(JJ)%NROWIPF
  READ(TS(JJ)%IUIPF,'(A256)') LINE; CALL IMOD_UTL_STRING(LINE); READ(LINE,*) TS(JJ)%NCOLIPF
  DO I=1,TS(JJ)%NCOLIPF; READ(TS(JJ)%IUIPF,*); ENDDO
  !## read iext,ext
  READ(TS(JJ)%IUIPF,'(A256)') LINE
  READ(LINE,*) TS(JJ)%IEXT,TS(JJ)%EXT

 ENDDO

 IF(.NOT.ASSOCIATED(DH))ALLOCATE(DH(0:SIZE(PARAM),SUM(TS%NROWIPF)*NPER))
 IF(.NOT.ASSOCIATED(W ))ALLOCATE(W (SUM(TS%NROWIPF)*NPER))

 !## initialise head-differences
 IF(PEST_IGRAD.EQ.0)DH(PEST_IGRAD,:)=0.0
 IF(PEST_IGRAD.NE.0)DH(PEST_IGRAD,:)=DH(0,:) !## zero gradient in case parameter is fixed

 W=0.0
 TJ=0.0

 II=0
 DO JJ=1,ABS(IIPF)

  IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(/A/)') TRIM(TS(JJ)%IPFNAME)

  IF(TS(JJ)%IEXT.EQ.0)THEN
   IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(3A10,6A15)') 'X','Y','ILAY','MSR','MDL','J','WMDL','WRESIDUAL','WEIGH'
   DO I=1,TS(JJ)%NROWIPF
    II=II+1
    READ(TS(JJ)%IUIPF,*) X,Y,ILAY,Z,W(II),H    !## w(i)=variance
    !## weigh=1/sqrt(variance)
    IF(TS(JJ)%IVCOL.GT.0)THEN
     IF(W(II).LE.0.0)THEN
      W(II)=0.0
     ELSE
      W(II)=1.0/SQRT(W(II))
     ENDIF
    ENDIF

    DH(PEST_IGRAD,II)=(H-Z) !## calculated - measured
    DHW              = W(II) *((H-Z)**2.0)
    TJ               = TJ+DHW
    IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(2F15.7,I10,6F15.7)') X,Y,ILAY,Z,H,DHW,W(II)*H,W(II)*(H-Z),W(II)
   ENDDO
  ELSE
   IF(TS(JJ)%IPFTYPE.EQ.3)THEN
    IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(2A15,3A10,6A15)') 'X','Y','ILAY','MSR','MDL','J','WEIGH','NHEAD'
   ELSE
    IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(A10,2A15,A10,7A15)') 'IDATE','X','Y','ILAY','MSR','MDL','WEIGH','J','MEAN_MSR','MEAN_MDL','CROSS-COR'
   ENDIF

   I=0
   XCROSS=0.0
   DO J=1,TS(JJ)%NROWIPF
    IF(TS(JJ)%IPFTYPE.EQ.2)THEN
     READ(TS(JJ)%IUIPF,*) X,Y,ILAY,ID,WW !,H    !## w(i)=variance
    !## moments, itype definieert het type z, dus moment0, drainagebasis, etc.
    ELSEIF(TS(JJ)%IPFTYPE.EQ.3)THEN
     READ(TS(JJ)%IUIPF,*) X,Y,ILAY,ID,WW !,H    !## w(i)=variance
    ENDIF
    !## weigh=1/variance
    IF(TS(JJ)%IVCOL.GT.0)THEN
     IF(WW.LE.0.0)THEN
      WW=0.0
     ELSE
      WW=1.0/SQRT(WW)
     ENDIF
    ENDIF
    LINE=TRIM(ROOTRES)//CHAR(92)//'timeseries'//CHAR(92)//TRIM(ID)//'.'//TRIM(TS(JJ)%EXT)
    CALL IMOD_UTL_SWAPSLASH(LINE); CALL IMOD_UTL_OPENASC(IUIPFTXT,LINE,'R')
    READ(IUIPFTXT,*) NROWIPFTXT; READ(IUIPFTXT,*) NCOLIPFTXT
    ALLOCATE(TSNODATA(MAX(3,NCOLIPFTXT)))
    DO K=1,NCOLIPFTXT; READ(IUIPFTXT,*) ID,TSNODATA(K); ENDDO
    ALLOCATE(M(NROWIPFTXT),C(NROWIPFTXT),IDATE(NROWIPFTXT)); IDATE=0; C=0.0; M=0.0
    IF(NCOLIPFTXT.LT.3)TSNODATA(3)=TSNODATA(2)
    !## get mean measure
    KK=0; DO K=1,NROWIPFTXT
     KK=KK+1
     IF(TS(JJ)%IPFTYPE.EQ.2)      READ(IUIPFTXT,*,IOSTAT=IOS) IDATE(KK),M(KK),C(KK)
     IF(TS(JJ)%IPFTYPE.EQ.3)THEN; READ(IUIPFTXT,*,IOSTAT=IOS) IDATE(KK),M(KK); C(KK)=M(KK); ENDIF
     !## error reading, skip it (can be caused by steady-state periods in between)
     IF(IOS.NE.0)THEN; KK=KK-1; CYCLE; ENDIF

     !## check period (if available)
     IF(PEST_NPERIOD.GT.0)THEN
      DO III=1,PEST_NPERIOD
       IF(IDATE(KK).GE.PEST_IPERIOD(III,1).AND.IDATE(KK).LE.PEST_IPERIOD(III,2))EXIT
      ENDDO
      IF(III.GT.PEST_NPERIOD)C(KK)=TSNODATA(3)
     ENDIF
     IF(M(KK).EQ.TSNODATA(2).OR.C(KK).EQ.TSNODATA(3))KK=KK-1
    ENDDO

    IF(TS(JJ)%IPFTYPE.EQ.3)THEN
    ELSE

     !## compute mean measurement in period
     MM=-9999.99; MC=-9999.99; XCOR=-9999.99
     IF(KK.GT.0)THEN
      MM=SUM(M(1:KK))/REAL(KK)
      MC=SUM(C(1:KK))/REAL(KK)
      !## compute cross-correlation
      IF(KK.GT.1)THEN
       XCOR=0.0; YCOR=0.0; ZCOR=0.0
       DO K=1,KK
        XCOR=XCOR+(MM-M(K))*(MC-C(K))
        YCOR=YCOR+(MM-M(K))**2.0
        ZCOR=ZCOR+(MC-C(K))**2.0
       ENDDO
       IF(YCOR.NE.0.0.AND.ZCOR.NE.0.0)XCOR=XCOR/(SQRT(YCOR)*SQRT(ZCOR))
       XCROSS=XCROSS+XCOR
      ENDIF
     ENDIF

     DO K=1,KK

      II               = II+1
      DHH              =(C(K)-M(K))
      DH(PEST_IGRAD,II)= DHH       !## - sensitivity

      !## weight, pest_itarget(.) should/will be summed to one
      W(II)=WW

      !## target is residual (calculated minus measured)
      DHH=    PEST_ITARGET(1)*(C(K)-M(K))
      !## target is dynamics (calculated minus measured)
      DHH=DHH+PEST_ITARGET(2)*((C(K)-MC)-(M(K)-MM))
      DHW=W(II)*(DHH**2.0) !## -
      TJ=TJ+DHW

      IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(I10,2F15.7,I10,7F15.7)') IDATE(K),X,Y,ILAY,M(K),C(K),W(II),DHW,MM,MC,XCOR
     ENDDO
    ENDIF
    DEALLOCATE(TSNODATA,C,M,IDATE)
    CLOSE(IUIPFTXT)
   ENDDO
  ENDIF
  CLOSE(TS(JJ)%IUIPF)
  IF(TS(JJ)%NROWIPF.GT.0)THEN
   IF(NPER.GT.1.AND.TS(JJ)%IPFTYPE.NE.3)CALL IMOD_UTL_PRINTTEXT('MEAN Cross-Correlation         : '//TRIM(IMOD_UTL_RTOS(XCROSS/REAL(TS(JJ)%NROWIPF),'F',7))//' (n='//TRIM(IMOD_UTL_ITOS(TS(JJ)%NROWIPF))//')',-1,IUPESTOUT)
  ENDIF
 ENDDO
 PEST_NOBS=II

 !## run batch files
 CALL PEST_BATCHFILES()

 CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT)
 CALL IMOD_UTL_PRINTTEXT('TOTAL Objective Function Value : '//TRIM(IMOD_UTL_RTOS(REAL(TJ),'E',7)),-1,IUPESTOUT)
 CALL IMOD_UTL_PRINTTEXT('MEAN Objective Function Value  : '//TRIM(IMOD_UTL_RTOS(REAL(TJ)/REAL(PEST_NOBS),'E',7))//' (n='//TRIM(IMOD_UTL_ITOS(PEST_NOBS))//')',-1,IUPESTOUT)

 CALL PESTPROGRESS()

 IF(LGRAD)THEN
  IF(PEST_IGRAD.EQ.0)THEN
   TJOBJ=TJ
  ELSE
   PARAM(PEST_IGRAD)%TJOBJ=TJ
  ENDIF
 ENDIF
 IF(LLNSRCH)THEN

 ENDIF

 END SUBROUTINE PEST_GETJ

 !###====================================================================
 SUBROUTINE PEST_BATCHFILES()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,N,IUBAT,S1,S2
 REAL :: Z,WW,H,DHW

 DO I=1,PEST_NBATCH
  CALL IMOD_UTL_PRINTTEXT(' Executing:'//TRIM(PEST_IBATCH(I)%BATCHFILE),0)
  CALL SYSTEM(PEST_IBATCH(I)%BATCHFILE)
  CALL IMOD_UTL_PRINTTEXT(' Reading  :'//TRIM(PEST_IBATCH(I)%OUTFILE),0)
  IUBAT=IMOD_UTL_GETUNIT(); OPEN(IUBAT,FILE=PEST_IBATCH(I)%OUTFILE,STATUS='OLD')
  READ(IUBAT,*) N

  S1=SIZE(DH,1)-1; S2=SIZE(DH,2)
  IF(PEST_NOBS+N.GT.S2)THEN
   ALLOCATE(DH_DUMMY(0:S1,PEST_NOBS+N))
   DO K=0,S1; DO J=1,S2
    DH_DUMMY(K,J)=DH(K,J)
   ENDDO; ENDDO
   DEALLOCATE(DH); DH=>DH_DUMMY
   ALLOCATE(W_DUMMY(PEST_NOBS+N))
   DO J=1,S2; W_DUMMY(J)=W(J); ENDDO
   DEALLOCATE(W); W=>W_DUMMY
  ENDIF

  DO J=1,N
   PEST_NOBS=PEST_NOBS+1
   READ(IUBAT,*) Z,WW,H
   IF(WW.LE.0.0)THEN
    WW=0.0
   ELSE
    !## weigh=1/variance
    WW=1.0/SQRT(WW)
   ENDIF
   DH(PEST_IGRAD,PEST_NOBS)=(H-Z) !## calculated - measured
   W(PEST_NOBS)            = WW
   DHW                     = WW*((H-Z)**2.0)
   TJ                      = TJ+DHW
   IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(30X,6F15.7)') Z,H,DHW,WW*Z,WW*(H-Z),WW
  ENDDO
  CLOSE(IUBAT)

 ENDDO

 END SUBROUTINE PEST_BATCHFILES

 !###====================================================================
 SUBROUTINE PEST1CHK(IP)
 !###====================================================================
 use rf2mf_module, only: ncol, nrow, nlay, nper
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IP
 INTEGER :: I

 PARAM(IP)%PTYPE=IMOD_UTL_CAPF(PARAM(IP)%PTYPE,'U')
 DO I=1,MXPTYPE; IF(TRIM(PARAM(IP)%PTYPE).EQ.TRIM(PTYPE(I)))EXIT; ENDDO
 IF(I.GT.MXPTYPE)THEN
  CALL IMOD_UTL_PRINTTEXT('',0)
  CALL IMOD_UTL_PRINTTEXT('Error can not recognize parameter type:'//TRIM(PARAM(IP)%PTYPE),0)
  CALL IMOD_UTL_PRINTTEXT(' Choose from:',0)
  DO I=1,MXPTYPE; CALL IMOD_UTL_PRINTTEXT(' - '//TRIM(PTYPE(I)),0); ENDDO
  CALL IMOD_UTL_PRINTTEXT('',2)
 ENDIF
 IF(PARAM(IP)%MIN.GE.PARAM(IP)%MAX)THEN
  CALL IMOD_UTL_PRINTTEXT('No proper parameter width defined for parameter '//TRIM(IMOD_UTL_ITOS(IP)),2)
 ENDIF
 IF(PARAM(IP)%INI.LT.PARAM(IP)%MIN.OR. &
    PARAM(IP)%INI.GT.PARAM(IP)%MAX)THEN
  CALL IMOD_UTL_PRINTTEXT('Parameter '//TRIM(IMOD_UTL_ITOS(IP))//' outside parameter width',2)
 ENDIF
 SELECT CASE (TRIM(PARAM(IP)%PTYPE))
  CASE ('KD','KH','SC')
   IF(PARAM(IP)%ILS.LE.0.OR.PARAM(IP)%ILS.GT.NLAY)   &
    CALL IMOD_UTL_PRINTTEXT('Parameter '//TRIM(IMOD_UTL_ITOS(IP))//': ILS exceeds NLAY',2)
  CASE ('VC','KV')
   IF(PARAM(IP)%ILS.LE.0.OR.PARAM(IP)%ILS.GT.NLAY-1) &
    CALL IMOD_UTL_PRINTTEXT('Parameter '//TRIM(IMOD_UTL_ITOS(IP))//': ILS exceeds NLAY-1',2)
 END SELECT

 !## scaling
 SELECT CASE (TRIM(PARAM(IP)%PTYPE))
  CASE ('KD','KH','KV','VC','SC','RC','RI','IC','II','DC','AF','MS','MC','VA','HF')
   IF(PARAM(IP)%DELTA.EQ.1.0)CALL IMOD_UTL_PRINTTEXT('You can not specify delta alpha eq 1.0 for KD,KH,KV,C,S,RC,RI,IC,II,DC,AF,HF,MS,MC',2)
   IF(PARAM(IP)%MIN  .EQ.0.0)CALL IMOD_UTL_PRINTTEXT('You can not specify minimal value eq 0.0 for KD,KH,KV,C,S,RC,RI,IC,II,DC,AF,HF,MS,MC',2)
   PARAM(IP)%INI  =LOG(PARAM(IP)%INI)
   PARAM(IP)%MIN  =LOG(PARAM(IP)%MIN)
   PARAM(IP)%MAX  =LOG(PARAM(IP)%MAX)
   PARAM(IP)%DELTA=LOG(PARAM(IP)%DELTA)
 END SELECT

 END SUBROUTINE PEST1CHK

 !###========================================================================
 SUBROUTINE RED1EIGSRT(D,A,N,NP)
 !###========================================================================
 !### NUMERICAL RECIPIES: SORTS EIGENVALUES/EIGENVECTOREN
 !###========================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,NP
 REAL,DIMENSION(NP),INTENT(INOUT) :: D
 REAL,DIMENSION(NP,NP),INTENT(INOUT) :: A
 INTEGER :: I,J,K
 REAL :: P

 DO I=1,N-1
  K=I
  P=D(I)
  DO J=I+1,N
   IF(D(J).GE.P)THEN
    K=J
    P=D(J)
   ENDIF
  END DO
  IF(K.NE.I)THEN
   D(K)=D(I)
   D(I)=P
   DO J=1,N
    P=A(J,I)
    A(J,I)=A(J,K)
    A(J,K)=P
   END DO
  ENDIF
 END DO

 END SUBROUTINE RED1EIGSRT

!###========================================================================
 SUBROUTINE RED1TRED2(A,N,NP,D,E)
!###========================================================================
!### NUMERICAL RECIPIES
!### HOUSEHOLDER METHOD TO REDUCE A
!### SYMMATRIC MATRIX INTO A TRIDIAGONAL
!###========================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,NP
 REAL,DIMENSION(NP,NP),INTENT(INOUT) :: A
 REAL,DIMENSION(NP),INTENT(INOUT) :: D,E
 INTEGER :: I,J,K,L
 REAL :: F,G,H,HH,SCALE

 DO I=N,2,-1
  L=I-1
  H=0.
  SCALE=0.
  IF(L.GT.1)THEN
   DO K=1,L
    SCALE=SCALE+ABS(A(I,K))
   ENDDO
   IF(SCALE.EQ.0.)THEN
    E(I)=A(I,L)
   ELSE
    DO K=1,L
     A(I,K)=A(I,K)/SCALE
     H=H+A(I,K)**2.
    ENDDO
    F=A(I,L)
    G=-SIGN(SQRT(H),F)
    E(I)=SCALE*G
    H=H-F*G
    A(I,L)=F-G
    F=0.
    DO J=1,L
     A(J,I)=A(I,J)/H
     G=0.
     DO K=1,J
      G=G+A(J,K)*A(I,K)
     ENDDO
     DO K=J+1,L
      G=G+A(K,J)*A(I,K)
     ENDDO
     E(J)=G/H
     F=F+E(J)*A(I,J)
    ENDDO
    HH=F/(H+H)
    DO J=1,L
     F=A(I,J)
     G=E(J)-HH*F
     E(J)=G
     DO K=1,J
      A(J,K)=A(J,K)-F*E(K)-G*A(I,K)
     ENDDO
    ENDDO
   ENDIF
  ELSE
   E(I)=A(I,L)
  ENDIF
  D(I)=H
 ENDDO

 D(1)=0.
 E(1)=0.
 DO I=1,N
  L=I-1
  IF(D(I).NE.0.)THEN
   DO J=1,L
    G=0.
    DO K=1,L
     G=G+A(I,K)*A(K,J)
    END DO
    DO K=1,L
     A(K,J)=A(K,J)-G*A(K,I)
    END DO
   END DO
  ENDIF
  D(I)=A(I,I)
  A(I,I)=1.
  DO J=1,L
   A(I,J)=0.
   A(J,I)=0.
  END DO
 END DO

 END SUBROUTINE RED1TRED2

 !###========================================================================
 REAL FUNCTION PYTHAG(A,B)
 !###========================================================================
 !### NUMERICAL RECIPIES
 !###========================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: A,B
 REAL :: ABSA,ABSB

 ABSA=ABS(A)
 ABSB=ABS(B)
 IF(ABSA.GT.ABSB)THEN
  PYTHAG=ABSA*SQRT(1.+(ABSB/ABSA)**2.)
 ELSE
  IF(ABSB.EQ.0.)THEN
   PYTHAG=0.
  ELSE
   PYTHAG=ABSB*SQRT(1.+(ABSA/ABSB)**2.)
  ENDIF
 ENDIF

 END FUNCTION PYTHAG

 !###========================================================================
 SUBROUTINE RED1TQLI(D,E,N,NP,A)
 !###========================================================================
 !### NUMERICAL RECIPIES: QR ALGORITHM WITH IMPLICIT SHIFTS IN TRIDIAGONAL
 !### EIGENVALUES/EIGENVECTOREN
 !###========================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,NP
 REAL,DIMENSION(NP),INTENT(INOUT) :: D,E
 REAL,DIMENSION(NP,NP),INTENT(INOUT) :: A
 INTEGER :: I,ITER,K,L,M
 REAL :: B,C,DD,F,G,P,R,S

 DO I=2,N
  E(I-1)=E(I)
 ENDDO
 E(N)=0.
 DO L=1,N
  ITER=0
1  DO M=L,N-1
   DD=ABS(D(M))+ABS(D(M+1))
   IF(ABS(E(M))+DD.EQ.DD)GOTO 2
  END DO
  M=N
2  IF(M.NE.L)THEN
   IF(ITER.EQ.100)PAUSE 'TOO MANY ITERATIONS IN TQLI'
   ITER=ITER+1
   G=(D(L+1)-D(L))/(2.*E(L))
   R=PYTHAG(G,1.)
   G=D(M)-D(L)+E(L)/(G+SIGN(R,G))
   S=1.
   C=1.
   P=0.
   DO I=M-1,L,-1
    F=S*E(I)
    B=C*E(I)
    R=PYTHAG(F,G)
    E(I+1)=R
    IF(R.EQ.0.)THEN
     D(I+1)=D(I+1)-P
     E(M)=0.
     GOTO 1
    ENDIF
    S=F/R
    C=G/R
    G=D(I+1)-P
    R=(D(I)-G)*S+2.*C*B
    P=S*R
    D(I+1)=G+P
    G=C*R-B
    DO K=1,N
     F=A(K,I+1)
     A(K,I+1)=S*A(K,I)+C*F
     A(K,I)=C*A(K,I)-S*F
    END DO
   END DO
   D(L)=D(L)-P
   E(L)=G
   E(M)=0.
   GOTO 1
  ENDIF
 END DO

 END SUBROUTINE RED1TQLI

 !###====================================================================
 SUBROUTINE PEST_GETMOMENT_UNITTEST()
 !###====================================================================
 IMPLICIT NONE
 REAL,DIMENSION(:),ALLOCATABLE :: RHEAD
 REAL :: RIMPULSESIZE,RTIMESTEP,RCHSTRUCNIV,RBASELEVEL,RIR_M0,RIR_MU,RIR_SIGMA,STRUCNIV
 INTEGER :: NHEAD,N,I,IDATE
 LOGICAL :: LBASECALCULATED

 !RHEAD(1)  = 10.01
 !RHEAD(2)  = 10.05
 !RHEAD(3)  = 10.09
 !RHEAD(4)  = 10.11
 !RHEAD(5)  = 10.10
 !RHEAD(6)  = 10.08
 !RHEAD(7)  = 10.05
 !RHEAD(8)  = 10.03
 !RHEAD(9)  = 10.01
 !STRUCNIV  = 10.0

 OPEN(10,FILE= 'd:\IMOD-MODEL\NHI\measures\respons\B05G0218_001.txt',STATUS='OLD',ACTION='READ')
 READ(10,*) NHEAD
 ALLOCATE(RHEAD(NHEAD))
 READ(10,*) N; DO I=1,N; READ(10,*); ENDDO
 DO I=1,NHEAD; READ(10,*) IDATE,RHEAD(I); ENDDO
 STRUCNIV=RHEAD(1)

 RHEAD=RHEAD-STRUCNIV   !## niveau dat behoort bij het structureel niveau
 RCHSTRUCNIV = 0.00075  !## recharge dat behoort bij het structureel niveau
 RIMPULSESIZE = 0.01    !## impulse (m/dag)
 RTIMESTEP = 1.0        !## tijdstap grootte
 LBASECALCULATED=.TRUE. !## compute drainage basis, alleen als structureel niveau aanwezig is.

 CALL PEST_GETMOMENT(RHEAD,RIMPULSESIZE,RTIMESTEP,RCHSTRUCNIV, &
          LBASECALCULATED,STRUCNIV,RBASELEVEL,RIR_M0,RIR_MU,RIR_SIGMA)

 WRITE(*,*) RBASELEVEL
 WRITE(*,*) RIR_M0
 WRITE(*,*) RIR_MU
 WRITE(*,*) RIR_SIGMA

 IF(.NOT.PEST_GETMOMENT_EXTENT(RHEAD,RIMPULSESIZE,RTIMESTEP,RCHSTRUCNIV, &
          LBASECALCULATED,STRUCNIV,RBASELEVEL,RIR_M0,RIR_MU,RIR_SIGMA,NHEAD))THEN; ENDIF

 WRITE(*,*) RBASELEVEL
 WRITE(*,*) RIR_M0
 WRITE(*,*) RIR_MU
 WRITE(*,*) RIR_SIGMA
 WRITE(*,*) NHEAD
 PAUSE

 END SUBROUTINE PEST_GETMOMENT_UNITTEST

 !###====================================================================
 LOGICAL FUNCTION PEST_GETMOMENT_EXTENT(RHEAD,RIMPULSESIZE,RTIMESTEP,RCHSTRUCNIV, &
          LBASECALCULATED,STRUCNIV,RBASELEVEL,RIR_M0,RIR_MU,RIR_SIGMA,NHEAD)
 !###====================================================================
 ! ROUTINE TO DETERMINE CHARACTERISTICS OF IMPULSE-RESPONSE AND
 ! OPTIONALLY THE BASELEVEL FROM HEAD SERIES:
 ! PROGRAMMER: WJZ (WILLEMJAN.ZAADNOORDIJK@KWRWATER.NL)
 IMPLICIT NONE
 REAL,INTENT(IN),DIMENSION(:) :: RHEAD
 REAL,INTENT(IN):: RIMPULSESIZE,RTIMESTEP,RCHSTRUCNIV,STRUCNIV
 LOGICAL,INTENT(IN):: LBASECALCULATED
 REAL,INTENT(OUT):: RBASELEVEL,RIR_M0,RIR_MU,RIR_SIGMA
 INTEGER,INTENT(OUT) :: NHEAD
 REAL, PARAMETER :: RMAXFRAC = 0.75     ! maximum ratio of last to max for extending series
 REAL, PARAMETER :: RMINFRAC = 0.001    ! minimum ratio of last to max for extending series
 REAL, PARAMETER :: RTOLFRAC = RMINFRAC ! tolerance used to terminate extending series
 LOGICAL :: LEXTEND
 INTEGER :: I,J
 REAL :: RM1,RM2,RTIME,RTSOIS,RTMP0,RTMP1,RTMP2
 REAL :: RCHCK,RHAMAX,RHAMIN,RHN,RHNM1,RIRFA,RIRFB

 PEST_GETMOMENT_EXTENT=.FALSE.

 !## determine nhead
 RHAMAX=MAXVAL(RHEAD)
 RHAMIN=MINVAL(RHEAD)
 NHEAD =0

 !## impulse signal available
 IF(RHAMAX-RHAMIN.EQ.0.0)RETURN

 J=0; DO I=1,SIZE(RHEAD)
  IF(RHEAD(I).EQ.RHAMAX)J=1
  RCHCK=RHEAD(I)/RHAMAX
  IF(J.EQ.1.AND.RCHCK.LT.RMINFRAC)EXIT
 ENDDO
 NHEAD=I-1

 !## calculate moments
 RIR_M0=  0.0
 RM1   =  0.0
 RM2   =  0.0
 RTIME = -0.5*RTIMESTEP             ! STARTS LOOP HALFWAY FIRST TIME STEP
 RTSOIS= RTIMESTEP/RIMPULSESIZE*0.5 ! 0.5 FOR SUM->AVERAGE IN INTEGRATION

 DO I=0,NHEAD
  RTIME=RTIME+RTIMESTEP
  IF(I.EQ.0)               RTMP0=RTSOIS* RHEAD(I+1)
  IF(I.GT.0.AND.I.LT.NHEAD)RTMP0=RTSOIS*(RHEAD(I)+RHEAD(I+1))
  IF(I.EQ.NHEAD)           RTMP0=RTSOIS* RHEAD(I)
  RTMP1  = RTMP0*RTIME
  RTMP2  = RTMP1*RTIME
  RIR_M0 = RIR_M0+RTMP0
  RM1    = RM1+RTMP1
  RM2    = RM2+RTMP2
 ENDDO

 !## extend series with negative e-power
 IF(RCHCK.GT.RMINFRAC)THEN
  RHN    =RHEAD(NHEAD)
  RHNM1  =RHEAD(NHEAD-1)
  LEXTEND=.TRUE.
  IF(RCHCK.GT.RMAXFRAC)     LEXTEND=.FALSE.  ! respons in RHEAD is too short
  IF(ABS(RHN).GE.ABS(RHNM1))LEXTEND=.FALSE.  ! respons not decreasing
  IF(SIGN(RHN,RHNM1).NE.RHN)LEXTEND=.FALSE.  ! respons not approaching zero (last
                                             ! two values have opposite signs)
  IF(.NOT.LEXTEND)THEN
   WRITE(IUPESTRESIDUAL,*) RCHCK.GT.RMAXFRAC,RCHCK,RMAXFRAC,'RCHCK.GT.RMAXFRAC'
   WRITE(IUPESTRESIDUAL,*) ABS(RHN).GE.ABS(RHNM1),ABS(RHN),ABS(RHNM1),'ABS(RHN).GE.ABS(RHNM1)'
   WRITE(IUPESTRESIDUAL,*) SIGN(RHN,RHNM1).NE.RHN,SIGN(RHN,RHNM1),RHN,'SIGN(RHN,RHNM1).NE.RHN'
   RIR_M0    =1.D+30
   RIR_MU    =1.D+30
   RIR_SIGMA =1.D+30
   RBASELEVEL=1.D+30
   RETURN
  END IF

  !## determine coefficients for extension of IRfunction IRF(t)=A*exp(-B*t)
  RTIME=NHEAD*RTIMESTEP
  RIRFB=LOG(RHNM1/RHN)/RTIMESTEP
  RIRFA=RHN/EXP(-RIRFB*RTIME)

  !## extend series with approximate IR function
  RHN=0.0
  DO
   RHNM1 =RHN                                      ! VALUE AT BEGINNING OF CURRENT TIME STEP
   RHN   =RIRFA*EXP(-RIRFB*(RTIME+RTIMESTEP*0.5))  ! VALUE AT END OF CURRENT TIMES STEP
   RTMP0 =RTSOIS*(RHN+RHNM1)  ! AVERAGE VALUE DURING CURRENT TIME STEP DIVIDED BY IMPULSE SIZE
   RTMP1 =RTMP0*RTIME
   RTMP2 =RTMP1*RTIME
   RIR_M0=RIR_M0+RTMP0
   RM1   =RM1+RTMP1
   RM2   =RM2+RTMP2
   RCHCK =ABS(RHN)/RHAMAX
   IF(RCHCK.LT.RTOLFRAC)EXIT
   RTIME =RTIME+RTIMESTEP  ! CENTER OF NEXT TIMESTEP
  END DO

 END IF

 !## convert m1 and m2 to mu and sigma
 RIR_MU   =RM1/RIR_M0
! RIR_SIGMA=SQRT((RM2-RIR_MU**2.0)/RIR_M0)
 RIR_SIGMA=SQRT(RM2/RIR_M0-RIR_MU**2.0)

 !## calculate baselevel
 RBASELEVEL=1.D+30
 IF(LBASECALCULATED)RBASELEVEL=STRUCNIV-RCHSTRUCNIV*RIR_M0

 PEST_GETMOMENT_EXTENT=.TRUE.

 END FUNCTION PEST_GETMOMENT_EXTENT

 !###====================================================================
 SUBROUTINE PEST_GETMOMENT(RHEAD,RIMPULSESIZE,RTIMESTEP,RCHSTRUCNIV, &
          LBASECALCULATED,STRUCNIV,RBASELEVEL,RIR_M0,RIR_MU,RIR_SIGMA)
 !###====================================================================
 ! ROUTINE TO DETERMINE CHARACTERISTICS OF IMPULSE-RESPONSE AND
 ! OPTIONALLY THE BASELEVEL FROM HEAD SERIES:
 ! PROGRAMMER: WJZ (WILLEMJAN.ZAADNOORDIJK@KWRWATER.NL)
 IMPLICIT NONE
 REAL,INTENT(IN),DIMENSION(:) :: RHEAD
 REAL,INTENT(IN):: RIMPULSESIZE,RTIMESTEP,RCHSTRUCNIV,STRUCNIV
 LOGICAL,INTENT(IN):: LBASECALCULATED
 REAL,INTENT(OUT):: RBASELEVEL,RIR_M0,RIR_MU,RIR_SIGMA
 INTEGER :: I
 REAL :: RM1,RM2,RTIME,RTSOIS,RTMP0,RTMP1,RTMP2

 !## calculate moments
 RIR_M0= 0.0
 RM1   = 0.0
 RM2   = 0.0
 RTSOIS= RTIMESTEP/RIMPULSESIZE*0.5 ! 0.5 FOR SUM->AVERAGE IN INTEGRATION
 RTIME = -0.5*RTIMESTEP             ! STARTS LOOP HALFWAY FIRST TIME STEP

! RTIME=RTIMESTEP
 DO I=0,SIZE(RHEAD)
  RTIME=RTIME+RTIMESTEP
  IF(I.EQ.0)                     RTMP0=RTSOIS* RHEAD(I+1)
  IF(I.GT.0.AND.I.LT.SIZE(RHEAD))RTMP0=RTSOIS*(RHEAD(I)+RHEAD(I+1))
  IF(I.EQ.SIZE(RHEAD))           RTMP0=RTSOIS* RHEAD(I)
  RTMP1  = RTMP0*RTIME
  RTMP2  = RTMP1*RTIME
  RIR_M0 = RIR_M0+RTMP0
  RM1    = RM1+RTMP1
  RM2    = RM2+RTMP2
 ENDDO

 !## convert m1 and m2 to mu and sigma
 RIR_MU   =RM1/RIR_M0
! RIR_SIGMA=SQRT((RM2-RIR_MU**2.0)/RIR_M0)
 RIR_SIGMA=SQRT(RM2/RIR_M0-RIR_MU**2.0) !RIR_MU)

 !## calculate baselevel
 RBASELEVEL=1.D+30
 IF(LBASECALCULATED)RBASELEVEL=STRUCNIV-RCHSTRUCNIV*RIR_M0

 END SUBROUTINE PEST_GETMOMENT

 !###====================================================
 LOGICAL FUNCTION HEADSTATISTICS(H,RES,ITYPE)
 !###====================================================
 IMPLICIT NONE
 REAL,INTENT(INOUT),DIMENSION(:) :: H
 INTEGER,INTENT(IN) :: ITYPE
 REAL,INTENT(OUT) :: RES
 REAL,ALLOCATABLE,DIMENSION(:) :: DH
 INTEGER :: NH,I
 REAL :: MEAN

 HEADSTATISTICS=.FALSE.

 IF(ALLOCATED(DH))DEALLOCATE(DH)
 NH=SIZE(H); ALLOCATE(DH(NH)); DH=0.0

 DO I=2,NH; DH(I)=ABS(H(I)-H(I-1)); ENDDO
 DO I=2,NH; H(I)=H(I-1)+DH(I); ENDDO

 SELECT CASE(ITYPE)
  !## BASIS
  CASE (4)
   RES=H(1)
  !## MEDIAAN
  CASE (1)
   MEAN=MINVAL(H)+(MAXVAL(H)-MINVAL(H))*(1.0/2.0)
   DO I=1,NH; IF(H(I).GE.MEAN)EXIT; ENDDO; RES=REAL(I)
  CASE (2)
   MEAN=MINVAL(H)+(MAXVAL(H)-MINVAL(H))*(3.0/4.0)
   DO I=1,NH; IF(H(I).GE.MEAN)EXIT; ENDDO; RES=REAL(I)
  CASE (3)
   RES=(MAXVAL(H)-MINVAL(H))*(3.0/4.0)
  CASE DEFAULT
   RETURN
 END SELECT

 DEALLOCATE(DH)

 HEADSTATISTICS=.TRUE.

 END FUNCTION HEADSTATISTICS

!###====================================================================
 SUBROUTINE PEST1APPENDLOGFILE()
 !###====================================================================
 IMPLICIT NONE

 CALL PESTOPENFILE(IUPESTOUT,'log_pest_','txt',1)

 END SUBROUTINE PEST1APPENDLOGFILE

  END MODULE MOD_PEST


