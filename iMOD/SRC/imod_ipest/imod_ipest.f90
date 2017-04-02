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
MODULE MOD_PEST

!USE MOD_ISIMGRO
USE MOD_IPEST_PAR
!USE IMOD_UTL, ONLY : IMOD_UTL_GETMED
!USE MOD_UTL, ONLY : PRINTTEXT,UTL_CAPF,ITOS,GETREAL,UTL_FILENAME,RTOS,UTL_STRING,OPENASC,UTL_SWAPSLASH,GETUNIT,CREATEDIR,GETRCL, &
!             UTL_CAPF,KRIGING_MAIN,KRIGING_GETGAMMA
!USE GLBVAR, ONLY : IU,LINE,CMOD,PPST,PCAP,NLINES,NLAY,MMOD,IIPF,ROOTRES,ROOT,CSUBMODEL,NPER,SIMCSIZE,LQD, &
!   RCH_IMPULSE_SIZE,RRECHSTRUCNIV,MPCK,PRCH,NPCK,KPER,IMULT,PPWT
!USE BASVAR, ONLY : NROW,NCOL,CC,CV,SC,KHV,KVV,KVA,DELR,DELC,HNOFLOW,NODES
!USE TSVAR, ONLY : TS,CIPFTYPE,STVALUE
!USE VERSION, ONLY : CVERSION
!USE PCKVAR, ONLY : IRCH,XRCH

CHARACTER(LEN=2100),PRIVATE :: BLINE
CHARACTER(LEN=52),PRIVATE :: SLINE
REAL,PRIVATE :: DAMPINGFACTOR=1.5

CONTAINS
 
 !###====================================================================
 SUBROUTINE PEST1INIT()
 !###====================================================================
! USE GLBVAR, ONLY : CDATE_SIM,LINE
! IMPLICIT NONE
! INTEGER :: IOS,I,J,K,N,IZ,IROW,ICOL,JU,NIPF,MIPF,NP,IUCOV
! REAL :: NODATA,TF,F
! DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:) :: COV
! CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:) :: TXTCOV
!
! IF(MMOD(PPST).LE.0)RETURN
! 
! IF(IIPF.EQ.0)CALL PRINTTEXT('You should specify IIPF>0 in combination with PST module',2)
! 
! !## next pest run ... skip part of runfile with pest information
! IF(ALLOCATED(PARAM))RETURN 
! 
! CALL PESTOPENFILE(IUPESTOUT,'log_pest_','txt',PEST_ITER)
! CALL PESTOPENFILE(IUPESTPROGRESS,'log_pest_progress_','txt',PEST_ITER)
! CALL PESTOPENFILE(IUPESTEFFICIENCY,'log_pest_efficiency_','txt',PEST_ITER)
! CALL PESTOPENFILE(IUPESTSENSITIVITY,'log_pest_sensitivity_','txt',PEST_ITER)
! CALL PESTOPENFILE(IUPESTRUNFILE,'log_pest_runfile_','txt',PEST_ITER)
!
! IUPESTRESIDUAL=0
! 
! WRITE(IUPESTEFFICIENCY,'(6A15)') 'TJ','SQRT(TJ)','MEAN(TJ)','MEAN(SQRT(TJ))','ADJUSTMENTS','EFFICIENCY'
! WRITE(IUPESTEFFICIENCY,'(6A15)') '(L2)','(L)','MEAN(L2)','MEAN(L)','(%)','-'
! WRITE(IUPESTSENSITIVITY,'(A)')   'Sensitivity (%):'
!
! WRITE(IUPESTOUT,'(A)') 'PEST-LOG' 
! DO I=1,ABS(IIPF)
!  WRITE(IUPESTOUT,'(/A)') ' Reading:  '//TRIM(TS(I)%IPFNAME)
!  LINE=' IPF-type: ('//TRIM(ITOS(TS(I)%IPFTYPE))//') '//TRIM(CIPFTYPE(TS(I)%IPFTYPE))
!  WRITE(IUPESTOUT,'(A/)') TRIM(LINE)
! ENDDO
! 
! ALLOCATE(PARAM(NLINES))
! DO I=1,SIZE(PARAM); PARAM(I)%NODES=0; PARAM(I)%IBND=0; ENDDO
! DO I=1,SIZE(PARAM)
!  NULLIFY(PARAM(I)%X,PARAM(I)%IROW,PARAM(I)%ICOL,PARAM(I)%ALPHA_HISTORY,PARAM(I)%ALPHA_ERROR_VARIANCE)
! ENDDO
! 
! PEST_IREGULARISATION=0
! 
! READ(IU,'(A256)',IOSTAT=IOS) LINE
! IF(IOS.EQ.0)READ(LINE,*,IOSTAT=IOS) PEST_NITER,PEST_JSTOP,PEST_SENSITIVITY, &
!   PEST_NPERIOD,PEST_NBATCH,(PEST_ITARGET(I),I=1,SIZE(PEST_ITARGET)),PEST_ISCALING,PEST_PADJ,PEST_DRES,PEST_KTYPE
! IF(IOS.NE.0)THEN
!  !## simple kriging
!  PEST_KTYPE=1
!  IF(IOS.EQ.0)READ(LINE,*,IOSTAT=IOS) PEST_NITER,PEST_JSTOP,PEST_SENSITIVITY, &
!    PEST_NPERIOD,PEST_NBATCH,(PEST_ITARGET(I),I=1,SIZE(PEST_ITARGET)),PEST_ISCALING,PEST_PADJ,PEST_DRES
!  IF(IOS.NE.0)THEN
!   PEST_DRES=0.0
!   IF(IOS.EQ.0)READ(LINE,*,IOSTAT=IOS) PEST_NITER,PEST_JSTOP,PEST_SENSITIVITY, &
!     PEST_NPERIOD,PEST_NBATCH,(PEST_ITARGET(I),I=1,SIZE(PEST_ITARGET)),PEST_ISCALING,PEST_PADJ
!   IF(IOS.NE.0)THEN
!    PEST_PADJ=0.0 !## optional parameter adjustment
!    READ(LINE,*,IOSTAT=IOS) PEST_NITER,PEST_JSTOP,PEST_SENSITIVITY, &
!      PEST_NPERIOD,PEST_NBATCH,(PEST_ITARGET(I),I=1,SIZE(PEST_ITARGET)),PEST_ISCALING
!    IF(IOS.NE.0)THEN
!     PEST_ISCALING=0
!     READ(LINE,*,IOSTAT=IOS) PEST_NITER,PEST_JSTOP,PEST_SENSITIVITY, &
!      PEST_NPERIOD,PEST_NBATCH,(PEST_ITARGET(I),I=1,SIZE(PEST_ITARGET))
!    ENDIF
!   ENDIF
!  ENDIF
! ENDIF
! IF(IOS.NE.0)THEN
!  CALL PRINTTEXT('Error reading NITER,JSTOP,SENSITIVITY,NPERIOD,NBATCH,ITARGET[.]:',0)
!  CALL PRINTTEXT(TRIM(LINE),0)
!  CALL PRINTTEXT('Busy processing module: '//TRIM(CMOD(PPST)),2)
! ENDIF
! !## ordinary kriging
! IF(PEST_KTYPE.EQ.2)PEST_KTYPE=-2
! !## simple kriging
! IF(PEST_KTYPE.EQ.1)PEST_KTYPE= 2
!
! CALL PRINTTEXT('',0); CALL PRINTTEXT(' Pest-Settings',-1)
! CALL PRINTTEXT(' @@@ Number of Pest Iterations: '//TRIM(ITOS(PEST_NITER)),-1)
! CALL PRINTTEXT(' @@@ Stop Criterium Objective Function Value: '//TRIM(RTOS(PEST_JSTOP,'F',3)),-1)
! CALL PRINTTEXT(' @@@ Sensitivity to Exclude Parameter (temporarily): '//TRIM(RTOS(PEST_SENSITIVITY,'F',3)),-1)
! SELECT CASE (PEST_ISCALING)
!  CASE (0)
!   CALL PRINTTEXT(' @@@ No  Scaling, No  SVD',-1)
!  CASE (1)
!   CALL PRINTTEXT(' @@@ Yes Scaling, No  SVD',-1)
!  CASE (2)
!   CALL PRINTTEXT(' @@@ Yes Scaling, Yes SVD',-1)
!  CASE (3)
!   CALL PRINTTEXT(' @@@ No  Scaling, Yes SVD',-1)
! END SELECT
! SELECT CASE (PEST_KTYPE)
!  CASE ( 2)
!   CALL PRINTTEXT(' @@@ Simple Kriging is used (if neccessary)',-1)
!  CASE (-2)
!   CALL PRINTTEXT(' @@@ Ordinary Kriging is used (if neccessary)',-1)
!  CASE DEFAULT
!   CALL PRINTTEXT(' Select 1 or 2 for Kriging Type',2)
! END SELECT
! CALL PRINTTEXT(' @@@ Termination Criterion for Parameter Adjustments (vectorlength): '//TRIM(RTOS(PEST_PADJ,'F',3)),-1)
!  
! DO I=1,SIZE(PEST_ITARGET)
!  IF(PEST_ITARGET(I).LT.0.0)CALL PRINTTEXT('Error PEST_ITARGET('//TRIM(ITOS(I))//') < 0.0',2)
! ENDDO
! 
! TF=SUM(PEST_ITARGET)
! 
! CALL PRINTTEXT(' @@@ Number of BatchFiles Used: '//TRIM(ITOS(PEST_NBATCH)),-1)
! IF(PEST_NBATCH.GT.0)THEN
!  ALLOCATE(PEST_IBATCH(PEST_NBATCH))
!  DO I=1,PEST_NBATCH
!   READ(IU,'(A256)',IOSTAT=IOS) LINE
!   IF(IOS.NE.0)THEN
!    CALL PRINTTEXT('Error reading PERIOD1,PERIOD2: '//TRIM(LINE),0)
!    CALL PRINTTEXT('Busy processing module: '//TRIM(CMOD(PPST)),2)
!   ENDIF
!   CALL UTL_STRING(LINE)
!   READ(LINE,*,IOSTAT=IOS) PEST_IBATCH(I)%FRACTION,PEST_IBATCH(I)%BATCHFILE,PEST_IBATCH(I)%OUTFILE
!   IF(PEST_IBATCH(I)%FRACTION.LT.0.0)CALL PRINTTEXT('Error PEST_IBATCH('//TRIM(ITOS(I))//')%FRACTION < 0.0',2)
!   TF=TF+PEST_IBATCH(I)%FRACTION
!  ENDDO
! ENDIF
!
! TF=1.0/TF
! PEST_ITARGET=TF*PEST_ITARGET
! DO I=1,PEST_NBATCH; PEST_IBATCH(I)%FRACTION=TF*PEST_IBATCH(I)%FRACTION; ENDDO
!
! !## write fractions
! CALL PRINTTEXT(' @@@ Fraction Heads: '//TRIM(RTOS(PEST_ITARGET(1),'F',3)),-1)
! CALL PRINTTEXT(' @@@ Fraction DynHd: '//TRIM(RTOS(PEST_ITARGET(2),'F',3)),-1)
! DO I=1,PEST_NBATCH
!  CALL PRINTTEXT(' @@@ Fraction Batch '//TRIM(ITOS(I))//': '//TRIM(RTOS(PEST_IBATCH(I)%FRACTION,'F',3)),-1)
!  CALL PRINTTEXT(' @@@ BatchFile      '//TRIM(ITOS(I))//': '//TRIM(PEST_IBATCH(I)%BATCHFILE),-1)
!  CALL PRINTTEXT(' @@@ BatchOutFile   '//TRIM(ITOS(I))//': '//TRIM(PEST_IBATCH(I)%OUTFILE),-1)
! ENDDO
! 
! CALL PRINTTEXT(' @@@ Number of Periods Used: '//TRIM(ITOS(PEST_NPERIOD)),-1)
! IF(PEST_NPERIOD.GT.0)THEN
!  ALLOCATE(PEST_IPERIOD(PEST_NPERIOD,2))
!  DO I=1,PEST_NPERIOD
!   READ(IU,'(A256)',IOSTAT=IOS) LINE
!   IF(IOS.EQ.0)READ(LINE,*,IOSTAT=IOS) PEST_IPERIOD(I,1),PEST_IPERIOD(I,2)
!   IF(IOS.NE.0)THEN
!    CALL PRINTTEXT('Error reading PERIOD1,PERIOD2: '//TRIM(LINE),0)
!    CALL PRINTTEXT('Busy processing module: '//TRIM(CMOD(PPST)),2)
!   ENDIF
!   CALL PRINTTEXT(' @@@ Period '//TRIM(ITOS(I))//': '//TRIM(ITOS(PEST_IPERIOD(I,1)))//'-'//TRIM(ITOS(PEST_IPERIOD(I,2))),-1)
!  ENDDO
! ENDIF
! CALL PRINTTEXT('',0)
! 
! DO I=1,SIZE(PARAM)
!  READ(IU,'(A256)',IOSTAT=IOS) LINE
!  IF(IOS.EQ.0)THEN
!   READ(LINE,*,IOSTAT=IOS) PARAM(I)%IACT, &  !## iact
!        PARAM(I)%PTYPE, &  !## ptype
!        PARAM(I)%ILS, &    !## ilayer/system
!        PARAM(I)%IZONE, &  !## zone number
!        PARAM(I)%INI, &    !## initial value
!        PARAM(I)%DELTA, &  !## finite difference step
!        PARAM(I)%MIN, &    !## minimal value
!        PARAM(I)%MAX,&     !## maximal value
!        PARAM(I)%FADJ,&    !## maximal adjust factor
!        PARAM(I)%IGROUP, & !## group number
!        J
!    IF(IOS.EQ.0)THEN
!     IF(J.EQ.0)PARAM(I)%LOG=.FALSE.
!     IF(J.EQ.1)PARAM(I)%LOG=.TRUE.
!    ENDIF
!  ENDIF
!  IF(IOS.NE.0)THEN
!   READ(LINE,*,IOSTAT=IOS) PARAM(I)%IACT, &  !## iact
!        PARAM(I)%PTYPE, & !## ptype
!        PARAM(I)%ILS, &   !## ilayer/system
!        PARAM(I)%IZONE, & !## zone number
!        PARAM(I)%INI, &   !## initial value
!        PARAM(I)%DELTA, & !## finite difference step
!        PARAM(I)%MIN, &   !## minimal value
!        PARAM(I)%MAX,&    !## maximal value
!        PARAM(I)%FADJ,&   !## maximal adjust factor
!        PARAM(I)%IGROUP   !## group number
!   !## overrule default settings for log normal
!   SELECT CASE (PARAM(I)%PTYPE)
!    CASE ('KD','KH','KV','VC','SC','RC','RI','IC','II','DC','AF','MS','MC','VA','HF','EX','GC')
!     PARAM(I)%LOG=.TRUE.
!    CASE DEFAULT
!     PARAM(I)%LOG=.FALSE.    
!   END SELECT
!  ENDIF
!  IF(IOS.NE.0)THEN
!   READ(LINE,*,IOSTAT=IOS) PARAM(I)%IACT, &  !## iact
!       PARAM(I)%PTYPE, & !## ptype
!       PARAM(I)%ILS, &   !## ilayer/system
!       PARAM(I)%IZONE, & !## zone number
!       PARAM(I)%INI, &   !## initial value
!       PARAM(I)%DELTA, & !## finite difference step
!       PARAM(I)%MIN, &   !## minimal value
!       PARAM(I)%MAX,&    !## maximal value
!       PARAM(I)%FADJ     !## maximal adjust factor
!   PARAM(I)%IGROUP=I     !## group number
!  ENDIF
!  IF(IOS.NE.0)THEN
!   CALL PRINTTEXT('Missing parameter in: PTYPE,ILS,IZONE,INI,DELTA,MIN,MAX,FADJ',0)
!   CALL PRINTTEXT(' reading: '//TRIM(LINE),0)
!   CALL PRINTTEXT('Busy processing module: '//TRIM(CMOD(PPST)),2)
!  ENDIF
!  !## read external filename for external parameters
!  IF(PARAM(I)%PTYPE.EQ.'EX')THEN
!   READ(IU,'(A256)',IOSTAT=IOS) LINE
!   READ(LINE,'(A256)',IOSTAT=IOS) PARAM(I)%EXBATFILE
!  ENDIF
!  !## maximal adjust factor
!  IF(PARAM(I)%FADJ.LE.1.0)THEN
!   CALL PRINTTEXT('Maximal Adjustment Factor should be at least greater than 1.0',0)
!   CALL PRINTTEXT('Parameter '//TRIM(ITOS(I))//' has a value of '//TRIM(RTOS(PARAM(I)%FADJ,'F',3)),2)
!  ENDIF
!  PARAM(I)%IACT=MIN(1,MAX(0,PARAM(I)%IACT))
!  ALLOCATE(PARAM(I)%ALPHA_HISTORY(0:PEST_NITER))
!  ALLOCATE(PARAM(I)%ALPHA_ERROR_VARIANCE(0:PEST_NITER))
!  PARAM(I)%ALPHA_HISTORY(0)=PARAM(I)%INI
!  PARAM(I)%ALPHA_ERROR_VARIANCE=0.0
!  PARAM(I)%PTYPE=UTL_CAPF(PARAM(I)%PTYPE,'U')
! ENDDO
!  
! READ(IU,'(A256)',IOSTAT=IOS) LINE
! IF(IOS.EQ.0)READ(LINE,*,IOSTAT=IOS) N
! IF(IOS.NE.0)THEN
!  CALL PRINTTEXT('Error reading NZONE'//TRIM(LINE),0)
!  CALL PRINTTEXT('Busy processing module: '//TRIM(CMOD(PPST)),2)
! ENDIF
! 
! !## call fosm if covariance is known and jacobians are there
! IF(.FALSE.)THEN
!  !## set igroup lt 0 for followers in group
!  DO I=1,SIZE(PARAM)
!!   IF(PARAM(I)%IACT.EQ.0)CYCLE
!   DO J=1,I-1 
!!    IF(PARAM(J)%IACT.EQ.0)CYCLE
!    IF(PARAM(J)%IGROUP.EQ.PARAM(I)%IGROUP)THEN
!     PARAM(I)%IGROUP=-1*PARAM(I)%IGROUP; EXIT
!    ENDIF
!   ENDDO
!  ENDDO
!!  CALL OPENASC(IUCOV,'d:\IMOD-MODELS\IBRAHYM_V2\DBASE\IMOD_USER\MODELS\ibV2_stat_fosm\totaal250\covariance_250.txt','R')
!!  CALL OPENASC(IUCOV,'d:\IMOD-MODELS\IBRAHYM_V2\DBASE\IMOD_USER\MODELS\ibV2_stat_fosm\totaal250\covariance_1000.txt','R')
!
!!## ibrahym
!!  CALL OPENASC(IUCOV,'d:\IMOD-MODELS\IBRAHYM_V2\DBASE\IMOD_USER\MODELS\ibV2_stat_fosm\totaal250\covariance_250_opt_4iter.txt','R')
!
!!## tilburg
!!  CALL OPENASC(IUCOV,'p:\1210574-wilhelmina-kanaal\20150224_Tilburg\model2imod\output\v4_08_calib_02_peter\cov_iter0.txt','R')
!  CALL OPENASC(IUCOV,'p:\1210574-wilhelmina-kanaal\20150224_Tilburg\model2imod\output\v4_08_calib_02_peter_optimalisatie\cov_iter5.txt','R')
!  READ(IUCOV,*) NP; ALLOCATE(COV(NP,NP),TXTCOV(NP))
!  READ(IUCOV,*); READ(IUCOV,*); READ(IUCOV,*)
!  DO I=1,NP; READ(IUCOV,*) TXTCOV(I),(COV(I,J),J=1,NP); ENDDO
!  !## deactive parameter that are not represented by txtcov()
!  J=1; DO I=1,SIZE(PARAM)
!   WRITE(LINE,'(A2,2I3.3,A1,I3.3)') PARAM(I)%PTYPE,PARAM(I)%ILS,PARAM(I)%IZONE,'-',PARAM(I)%IGROUP
!   IF(TRIM(LINE).EQ.TRIM(TXTCOV(J)))THEN
!    J=J+1; IF(J.GT.NP)J=1
!   ELSE
!    PARAM(I)%IACT=0
!   ENDIF
!  ENDDO
!  CDATE_SIM(1)='STEADY-STATE'
!  CALL PESTWRITESTATISTICS_FOSM(NP,COV)
! ENDIF 
! 
! ALLOCATE(ZONE(N))
! 
! DO I=1,SIZE(ZONE)
!  
!  NULLIFY(ZONE(I)%X,ZONE(I)%XY,ZONE(I)%IZ) 
!  READ(IU,*,IOSTAT=IOS) LINE
!  IF(IOS.NE.0)THEN
!   CALL PRINTTEXT('Error reading '//TRIM(LINE),0)
!   CALL PRINTTEXT('Busy processing module: '//TRIM(CMOD(I)),2)
!  ENDIF
!  
!  IZ=INT(GETREAL(LINE,IOS))
!  IF(IOS.EQ.0)THEN
!   CALL PRINTTEXT('Read Constant Value '//TRIM(ITOS(IZ)),0)
!   ALLOCATE(ZONE(I)%X(NCOL,NROW))
!   ZONE(I)%ZTYPE=0
!   ZONE(I)%X=REAL(IZ) 
!  ELSE
!   CALL UTL_FILENAME(LINE)
!   CALL PRINTTEXT('Assigned '//TRIM(LINE),0)
!   IF(INDEX(UTL_CAPF(LINE,'U'),'.IDF').GT.0)THEN
!    ALLOCATE(ZONE(I)%X(NCOL,NROW))
!    ZONE(I)%ZTYPE=0
!    SELECT CASE (TRIM(PARAM(I)%PTYPE))
!     CASE ('KD','KH','KV','VC','SC','AF','AA','MS','MC','VA','HF','EX')
!      CALL READBLOCK_R(ZONE(I)%X,NCOL,NROW,LINE,NODATA,9,0,1,(/1/),0)   !## zonation upscaling, no smooting
!     CASE DEFAULT
!      CALL READBLOCK_R(ZONE(I)%X,NCOL,NROW,LINE,NODATA,7,0,1,(/1/),0)   !## most frequent value, no smooting
!    END SELECT
!   ELSEIF(INDEX(UTL_CAPF(LINE,'U'),'.IPF').GT.0)THEN
!    ZONE(I)%ZTYPE=1
!    !## read in ipf and put coordinates in bufpst()
!    JU=GETUNIT(); CALL OPENASC(JU,LINE,'R')
!    READ(JU,*) NIPF; READ(JU,*) MIPF; DO K=1,MIPF+1; READ(JU,*); ENDDO
!    ALLOCATE(ZONE(I)%XY(NIPF,2),ZONE(I)%IZ(NIPF))  
!    DO K=1,NIPF; READ(JU,*) ZONE(I)%XY(K,1),ZONE(I)%XY(K,2),ZONE(I)%IZ(K); ENDDO  
!    CLOSE(JU)
!   ELSE
!    CALL PRINTTEXT('No supported file format found',2)
!   ENDIF
!  ENDIF
! ENDDO
! 
! CALL PRINTTEXT('Parameters',-1)
! WRITE(LINE,'(A2,1X,A5,2(1X,A3),5(1X,A15),3A10)') 'AC','PTYPE','ILS','IZN','INITIAL','DELTA','MINIMUM','MAXIMUM','FADJ','IGROUP','LTRANS','NODES'
! CALL PRINTTEXT(TRIM(LINE),-1)
!
! !## check number of zones and missing zone (if any)
! DO I=1,SIZE(PARAM)
!  !## parameter active and main of group
!!  IF(PARAM(I)%IACT.NE.0.OR.)CYCLE
!  IF(PARAM(I)%IGROUP.LE.0)CYCLE
!  DO IROW=1,NROW; DO ICOL=1,NCOL; DO J=1,SIZE(ZONE) 
!   IF(ZONE(J)%ZTYPE.EQ.0)THEN
!    !## check whether it's integer value is equal to param(i)%izone, if so use fraction from idf-file
!    IF(PARAM(I)%IZONE.EQ.INT(ZONE(J)%X(ICOL,IROW)))THEN
!     PARAM(I)%NODES=PARAM(I)%NODES+1
!     PARAM(I)%ZTYPE=0
!     EXIT
!    ENDIF
!   ENDIF
!  ENDDO; ENDDO; ENDDO
!  !## check pilotpoints
!  DO J=1,SIZE(ZONE)
!   IF(ZONE(J)%ZTYPE.EQ.1)THEN
!    DO IROW=1,SIZE(ZONE(J)%IZ)
!     !## check whether it's integer value is equal to param(i)%izone, if so use fraction from idf-file
!     IF(PARAM(I)%IZONE.EQ.INT(ZONE(J)%IZ(IROW)))THEN
!      SELECT CASE (TRIM(PARAM(I)%PTYPE))
!       CASE ('KD','KH','KV','VC','SC','VA','RE')
!       CASE DEFAULT
!        CALL PRINTTEXT('Cannot use PilotPoints for other than KD,KH,KV,VC,SC and VA',2)
!      END SELECT
!      PARAM(I)%NODES=PARAM(I)%NODES+1
!      PARAM(I)%ZTYPE=1
!     ENDIF
!    ENDDO
!   ENDIF
!  ENDDO
!  IF(PARAM(I)%PTYPE.EQ.'HF')THEN
!   PARAM(I)%NODES=0 !## one single cell used as zone for horizontal barrier module
!   WRITE(LINE,'(I2,1X,A5,2(1X,I3),5(1X,F15.7),I10,L10,A10)') PARAM(I)%IACT,PARAM(I)%PTYPE,PARAM(I)%ILS, &
!     PARAM(I)%IZONE,PARAM(I)%INI,PARAM(I)%DELTA,PARAM(I)%MIN,PARAM(I)%MAX,PARAM(I)%FADJ,PARAM(I)%IGROUP,PARAM(I)%LOG,'EntireLine'
!  ELSE
!   IF(PARAM(I)%NODES.EQ.0)THEN
!    CALL PRINTTEXT('No area/zone assigned to parameter no. '//TRIM(ITOS(I))//' ptype= '//TRIM(PARAM(I)%PTYPE),0)
!    PARAM(I)%IACT=0
!   ENDIF
!   WRITE(LINE,'(I2,1X,A5,2(1X,I3),5(1X,F15.7),I10,L10,I10)') PARAM(I)%IACT,PARAM(I)%PTYPE,PARAM(I)%ILS, &
!     PARAM(I)%IZONE,PARAM(I)%INI,PARAM(I)%DELTA,PARAM(I)%MIN,PARAM(I)%MAX,PARAM(I)%FADJ,PARAM(I)%IGROUP,PARAM(I)%LOG,PARAM(I)%NODES
!  ENDIF
!  CALL PRINTTEXT(TRIM(LINE),-1)
!  IF(PARAM(I)%PTYPE.EQ.'EX')CALL PRINTTEXT(TRIM(PARAM(I)%EXBATFILE),-1)
!  CALL PEST1CHK(I)
!  PARAM(I)%ALPHA(1)=PARAM(I)%INI !## current  alpha
!  PARAM(I)%ALPHA(2)=PARAM(I)%INI !## previous alpha
!  !## parameter is at the boundary whenever less than 1% away
!  IF(ABS(PARAM(I)%ALPHA(1)-PARAM(I)%MIN).LT.XPBND)PARAM(I)%IBND=-1 !## min
!  IF(ABS(PARAM(I)%MAX)-PARAM(I)%ALPHA(1).LT.XPBND)PARAM(I)%IBND= 1 !## max
! ENDDO
!
! N=0; DO I=1,SIZE(PARAM)
!  IF(PARAM(I)%IACT.EQ.0.AND.PARAM(I)%IGROUP.LE.0)CYCLE
!  SELECT CASE (PARAM(I)%PTYPE)
!   CASE ('HF')
!    N=N+1
!   CASE DEFAULT
!    N=N+PARAM(I)%NODES
!  END SELECT
! ENDDO 
! IF(N.EQ.0)CALL PRINTTEXT('Nothing to do',2)
!   
! !## fill array zone and set appropriate pointers in type 
! DO I=1,SIZE(PARAM)
!  IF(PARAM(I)%NODES.GT.0)THEN
!   CALL PRINTTEXT('Parameter '//TRIM(ITOS(I))//' no. of zones no. '//TRIM(ITOS(PARAM(I)%NODES))//' assigned to ptype= '//TRIM(PARAM(I)%PTYPE),0)
!
!   IF(PARAM(I)%ZTYPE.EQ.0)THEN
!    
!    ALLOCATE(PARAM(I)%IROW(PARAM(I)%NODES),PARAM(I)%ICOL(PARAM(I)%NODES))
!    ALLOCATE(PARAM(I)%F(PARAM(I)%NODES))
!
!    N=0; DO IROW=1,NROW; DO ICOL=1,NCOL; DO J=1,SIZE(ZONE) 
!     IF(ZONE(J)%ZTYPE.EQ.0)THEN
!      IF(PARAM(I)%IZONE.EQ.INT(ZONE(J)%X(ICOL,IROW)))THEN
!
!       SELECT CASE (TRIM(PARAM(I)%PTYPE))
!        CASE ('KD','KH','KV','VC','SC','AF','AA','MS','MC','VA','HF','EX')
!         F=MOD(ZONE(J)%X(ICOL,IROW),1.0); IF(F.EQ.0.0)F=1.0
!        CASE DEFAULT
!         F=1.0
!       END SELECT
! 
!       N=N+1; PARAM(I)%IROW(N)=INT(IROW,2); PARAM(I)%ICOL(N)=INT(ICOL,2); PARAM(I)%F(N)=F
!       EXIT
!      ENDIF
!     ENDIF
!    ENDDO; ENDDO; ENDDO
!
!   ELSEIF(PARAM(I)%ZTYPE.EQ.1)THEN
!   
!    ALLOCATE(PARAM(I)%XY(PARAM(I)%NODES,2))
!
!    !## check pilotpoints
!    N=0; DO J=1,SIZE(ZONE)
!     IF(ZONE(J)%ZTYPE.EQ.1)THEN
!      DO IROW=1,SIZE(ZONE(J)%IZ)
!       !## check whether it's integer value is equal to param(i)%izone
!       IF(PARAM(I)%IZONE.EQ.INT(ZONE(J)%IZ(IROW)))THEN
!        N=N+1; PARAM(I)%XY(N,1)=ZONE(J)%XY(IROW,1); PARAM(I)%XY(N,2)=ZONE(J)%XY(IROW,2) 
!       ENDIF
!      ENDDO
!     ENDIF
!    ENDDO
!   
!   ENDIF
!   
!  ELSE
!   IF(PARAM(I)%PTYPE.NE.'HF')PARAM(I)%IACT=0
!  ENDIF 
! ENDDO
! 
! !## set igroup lt 0 for followers in group
! DO I=1,SIZE(PARAM)
!  IF(PARAM(I)%IACT.EQ.0)CYCLE
!  DO J=1,I-1 
!   IF(PARAM(J)%IACT.EQ.0)CYCLE
!   IF(PARAM(J)%IGROUP.EQ.PARAM(I)%IGROUP)THEN
!    PARAM(I)%IGROUP=-1*PARAM(I)%IGROUP; EXIT
!   ENDIF
!  ENDDO
! ENDDO
!
! DO I=1,SIZE(ZONE)
!  IF(ZONE(I)%ZTYPE.EQ.0)THEN
!   DEALLOCATE(ZONE(I)%X)
!  ELSEIF(ZONE(I)%ZTYPE.EQ.1)THEN
!   DEALLOCATE(ZONE(I)%XY,ZONE(I)%IZ)
!  ENDIF
! ENDDO
! DEALLOCATE(ZONE)
!   
! !## initialize process-flags
! LGRAD       =.TRUE.  !## gradient computation
! PEST_IGRAD  = 0      !## current simulation is default
! LLNSRCH     =.FALSE. !## no line search
! 
! !## sensitivity - if pest_niter=0
! LSENS=.FALSE.; IF(PEST_NITER.EQ.0)LSENS=.TRUE. 
! 
! WRITE(BLINE,'(3A5,A15)') 'IT','GD','LS','TOT_J'
! DO J=1,SIZE(PARAM)
!  IF(ABS(PARAM(J)%IACT).EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
!   WRITE(SLINE,'(2X,A2,2I3.3)') PARAM(J)%PTYPE,PARAM(J)%ILS,PARAM(J)%IZONE
!   BLINE=TRIM(BLINE)//TRIM(SLINE)
!  ENDIF
! ENDDO
! 
! WRITE(IUPESTPROGRESS   ,'(A)') TRIM(BLINE) 
! WRITE(IUPESTSENSITIVITY,'(A10,A)') 'Iteration',TRIM(BLINE(31:)) 
!
! BLINE=''
! DO J=1,SIZE(PARAM)
!  IF(ABS(PARAM(J)%IACT).EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
!   WRITE(SLINE,'(7X,I3.3)') PARAM(J)%IGROUP
!   BLINE=TRIM(BLINE)//TRIM(SLINE)
!  ENDIF
! ENDDO
! WRITE(IUPESTPROGRESS,'(30X,A)') TRIM(BLINE) 
! FLUSH(IUPESTPROGRESS)
! 
! !## dump initial factors
! CALL PESTDUMPFCT()
!  
 END SUBROUTINE PEST1INIT
!
! !###====================================================================
! SUBROUTINE PEST1ALPHA_MOD(HCOF,NODES,IPACKAGE)
! !###====================================================================
! USE MOD_UTL, ONLY : GETRCL
! USE GLBVAR, ONLY : PANI,NMOD,PCK
! USE BASVAR, ONLY : IBOUND
! USE PCKVAR, ONLY : XANI,IANI,XRCH,IRCH
! USE HFBVAR, ONLY : HFBFCT
! IMPLICIT NONE 
! INTEGER,INTENT(IN) :: NODES,IPACKAGE
! REAL,DIMENSION(NODES),INTENT(OUT) :: HCOF
! INTEGER :: ICOL,IROW,ILAY,I,J,K,IP,INODE,IPP,NXYZ,IP1,IP2,IS
! REAL :: FCT,PPART,NODATA,RANGE
! REAL,DIMENSION(:,:),ALLOCATABLE :: XYZ,XPP
! CHARACTER(LEN=2),DIMENSION(7) :: PPPARAM
! CHARACTER(LEN=256) :: DIR,FNAME
! DATA PPPARAM/'KD','KH','KV','VC','SC','VA','RE'/ !## variable for pilotpoints
! 
! IF(MMOD(PPST).LE.0)RETURN
! 
! IF(IPACKAGE.EQ.0)THEN
! 
!  CALL PRINTTEXT('',1)
!  IF(LGRAD)THEN
!   IF(PEST_ITER.EQ.0)THEN
!    CALL PRINTTEXT('Initial simulation',-1)
!   ELSE
!    LINE='Cycle '//TRIM(ITOS(PEST_IGRAD))//': Derivatives by Forward Finite-Difference'
!    CALL PRINTTEXT(TRIM(LINE),-1)
!   ENDIF
!  ENDIF
!  IF(LLNSRCH)THEN
!   CALL PRINTTEXT('Line Search',-1)
!  ENDIF
! 
!  !## initialize parameters
!  IF(PEST_ITER.EQ.0)THEN
!   DO I=1,SIZE(PARAM)
!
!    SELECT CASE (TRIM(PARAM(I)%PTYPE))
!     CASE ('RC','IC','DC','MS','MC','RI','II','RE','EX','GC'); CYCLE
!    END SELECT
!
!    ILAY=PARAM(I)%ILS
!
!    IF(PARAM(I)%ZTYPE.EQ.0)THEN
!
!     !## allocate memory to make copy of data
!     IF(.NOT.ASSOCIATED(PARAM(I)%X))ALLOCATE(PARAM(I)%X(PARAM(I)%NODES))
!
!     SELECT CASE (TRIM(PARAM(I)%PTYPE))
!      CASE ('KD')  !## transmissivities
!       DO J=1,PARAM(I)%NODES
!        IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J); PARAM(I)%X(J)=CC(ICOL,IROW,ILAY)*PARAM(I)%F(J)
!       ENDDO  
!      CASE ('KH')  !## horizontal k-values
!       DO J=1,PARAM(I)%NODES
!        IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J); PARAM(I)%X(J)=KHV(ICOL,IROW,ILAY)*PARAM(I)%F(J)
!       ENDDO  
!      CASE ('VC')   !## vertical c values
!       DO J=1,PARAM(I)%NODES
!        IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J); PARAM(I)%X(J)=CV(ICOL,IROW,ILAY)*PARAM(I)%F(J)
!       ENDDO  
!      CASE ('KV')  !## vertical k-values
!       DO J=1,PARAM(I)%NODES
!        IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J); PARAM(I)%X(J)=KVV(ICOL,IROW,ILAY)*PARAM(I)%F(J)
!       ENDDO  
!      CASE ('VA')  !## vertical anisotropy
!       DO J=1,PARAM(I)%NODES
!        IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J); PARAM(I)%X(J)=KVA(ICOL,IROW,ILAY)*PARAM(I)%F(J)
!       ENDDO  
!      CASE ('SC')   !## storage coefficient
!       DO J=1,PARAM(I)%NODES
!        IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J); PARAM(I)%X(J)=SC(ICOL,IROW,ILAY)*PARAM(I)%F(J)
!       ENDDO  
!      CASE ('AF','AA')   !## anisotropy factor/angle
!       HCOF=0.0
!       DO IP=1,NMOD(PANI)
!        IF(XANI(IP,1).LT.1.0)THEN
!         CALL GETRCL(IANI(IP),NROW,NCOL,ILAY,IROW,ICOL)
!         INODE=(IROW-1)*NCOL+ICOL
!         HCOF(INODE)=REAL(IP)
!        ENDIF   
!       ENDDO
!       IF(TRIM(PARAM(I)%PTYPE).EQ.'AF')K=1
!       IF(TRIM(PARAM(I)%PTYPE).EQ.'AA')K=2
!       DO J=1,PARAM(I)%NODES
!        IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
!        INODE=(IROW-1)*NCOL+ICOL
!        IP=INT(HCOF(INODE))
!        IF(IP.GT.0)THEN
!         PARAM(I)%X(J)=XANI(IP,K)*PARAM(I)%F(J)
!        ENDIF
!       ENDDO  
!        
!     END SELECT
!
!!    !## pilot points, save initial values for f -- hoeft toch niet???
!!    ELSEIF(PARAM(I)%ZTYPE.EQ.1)THEN
!!    !## allocate memory to make copy of data
!!    IF(.NOT.ASSOCIATED(PARAM(I)%CF))ALLOCATE(PARAM(I)%CF(SIZE(PARAM(I)%F)))
!!    PARAM(I)%CF=PARAM(I)%F
!!
!    ENDIF
!   ENDDO
!  ENDIF
!
!  DO I=1,SIZE(PARAM)
!
!   SELECT CASE (TRIM(PARAM(I)%PTYPE))
!    CASE ('RC','IC','DC','MS','MC','RI','II','RE','EX'); CYCLE
!   END SELECT
!
!   !## skip pilot-points
!   IF(PARAM(I)%ZTYPE.EQ.1)CYCLE
!  
!   FCT=PARAM(I)%ALPHA(1); ILAY=PARAM(I)%ILS
!   !## scaling
!   IF(PARAM(I)%LOG)FCT=EXP(FCT)
!
!   SELECT CASE (TRIM(PARAM(I)%PTYPE))
!    CASE ('KD')  !## transmissivities
!     DO J=1,PARAM(I)%NODES
!      IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
!      PPART             =CC(ICOL,IROW,ILAY)-PARAM(I)%X(J)
!      CC(ICOL,IROW,ILAY)=PPART+PARAM(I)%X(J)*FCT
!     ENDDO  
!    CASE ('KH')  !## horizontal k-values
!     DO J=1,PARAM(I)%NODES
!      IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
!      PPART              =KHV(ICOL,IROW,ILAY)-PARAM(I)%X(J)
!      KHV(ICOL,IROW,ILAY)=PPART+PARAM(I)%X(J)*FCT
!     ENDDO  
!    CASE ('VC')  !## vertical c values
!     DO J=1,PARAM(I)%NODES
!      IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
!      PPART             =CV(ICOL,IROW,ILAY)-PARAM(I)%X(J)
!      CV(ICOL,IROW,ILAY)=PPART+PARAM(I)%X(J)*FCT
!     ENDDO  
!    CASE ('KV')  !## vertical k-values
!     DO J=1,PARAM(I)%NODES
!      IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
!      PPART              =KVV(ICOL,IROW,ILAY)-PARAM(I)%X(J)
!      KVV(ICOL,IROW,ILAY)=PPART+PARAM(I)%X(J)*FCT
!     ENDDO  
!    CASE ('VA')  !## vertical anisotropy
!     DO J=1,PARAM(I)%NODES
!      IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
!      PPART              =KVA(ICOL,IROW,ILAY)-PARAM(I)%X(J)
!      KVA(ICOL,IROW,ILAY)=PPART+PARAM(I)%X(J)*FCT
!     ENDDO  
!    CASE ('SC')  !## storage coefficient
!     DO J=1,PARAM(I)%NODES
!      IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
!      PPART             =SC(ICOL,IROW,ILAY)-PARAM(I)%X(J)
!      SC(ICOL,IROW,ILAY)=PPART+PARAM(I)%X(J)*FCT
!     ENDDO  
!    CASE ('AF','AA')   !## anisotropy factor/angle
!     HCOF=0.0
!     DO IP=1,NMOD(PANI)
!      IF(XANI(IP,1).LT.1.0)THEN
!       CALL GETRCL(IANI(IP),NROW,NCOL,ILAY,IROW,ICOL)
!       INODE=(IROW-1)*NCOL+ICOL
!       HCOF(INODE)=REAL(IP)
!      ENDIF   
!     ENDDO
!     IF(TRIM(PARAM(I)%PTYPE).EQ.'AF')K=1
!     IF(TRIM(PARAM(I)%PTYPE).EQ.'AA')K=2
!     DO J=1,PARAM(I)%NODES
!      IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
!      INODE=(IROW-1)*NCOL+ICOL
!      IP=INT(HCOF(INODE))
!      IF(IP.GT.0)THEN
!       PPART     =XANI(IP,K)-PARAM(I)%X(J)
!       XANI(IP,K)=PPART+PARAM(I)%X(J)*FCT
!      ENDIF
!     ENDDO  
!    CASE ('HF')  !## horizontal barrier factor
!     !## does not work with zones, the factor for the entire line will be adjusted
!     HFBFCT(ILAY)=HFBFCT(ILAY)*FCT
!      
!   END SELECT
!   LINE=' * Module '//PARAM(I)%PTYPE//' adjusted ('//TRIM(ITOS(PARAM(I)%NODES))//') with alpha='//TRIM(RTOS(FCT,'F',7))
!   CALL PRINTTEXT(TRIM(LINE),1) 
!  ENDDO
! 
! ENDIF
! 
! !## open pest factor files
! IF(TRIM(CSUBMODEL).EQ.'')THEN
!  DIR=TRIM(ROOT)//CHAR(92)//'pest'//CHAR(92)//'factors'//TRIM(ITOS(PEST_ITER))
! ELSE
!  DIR=TRIM(ROOT)//CHAR(92)//TRIM(CSUBMODEL)//CHAR(92)//'pest'//CHAR(92)//'factors'//TRIM(ITOS(PEST_ITER))
! ENDIF
! CALL CREATEDIR(DIR)
! 
! !## process any pilotpoints per modellayer/ adjustable parameter
! DO IPP=1,SIZE(PPPARAM)
!  
!  IF(IPP.EQ.7)THEN
!   IF(IPACKAGE.EQ.0)CYCLE
!  ELSE
!   IF(IPACKAGE.EQ.1)CYCLE
!  ENDIF
!  
!  DO ILAY=1,NLAY; 
!   DO K=1,2
!    NXYZ=0
!    DO I=1,SIZE(PARAM)
!   
!     !## skip NONE pilot-points
!     IF(PARAM(I)%ZTYPE.NE.1)CYCLE
!     !## not similar parameter
!     IF(PARAM(I)%PTYPE.NE.PPPARAM(IPP))CYCLE
!     !## not correct modellayer
!     IF(PARAM(I)%ILS.NE.ILAY)CYCLE
!
!     FCT=PARAM(I)%ALPHA(1)
!     IF(K.EQ.2)THEN
!      IF(IPP.EQ.7)THEN
!       LINE=' * Module '//PARAM(I)%PTYPE//' adjusted ('//TRIM(ITOS(SIZE(PARAM(I)%XY,1)))//') location(s) as PILOTPOINT with alpha='//TRIM(RTOS(FCT,'F',7))
!      ELSE
!       LINE=' * Module '//PARAM(I)%PTYPE//' adjusted ('//TRIM(ITOS(SIZE(PARAM(I)%XY,1)))//') location(s) as PILOTPOINT with alpha='//TRIM(RTOS(EXP(FCT),'F',7))
!      ENDIF
!      CALL PRINTTEXT(TRIM(LINE),1) 
!     ENDIF
!    
!     DO J=1,SIZE(PARAM(I)%XY,1)
!      NXYZ=NXYZ+1
!      IF(K.EQ.2)THEN
!       XYZ(NXYZ,1)=PARAM(I)%XY(J,1); XYZ(NXYZ,2)=PARAM(I)%XY(J,2); XYZ(NXYZ,3)=FCT 
!      ENDIF
!     ENDDO
!    ENDDO
!    IF(NXYZ.EQ.0)EXIT
!    IF(K.EQ.1)ALLOCATE(XYZ(NXYZ,3))
!   ENDDO
!   !# next parameter, this one is not used
!   IF(NXYZ.EQ.0)CYCLE
!
!   NODATA=-999.99; ALLOCATE(XPP(NCOL,NROW)); XPP=NODATA
!   RANGE=PEST_GETRANGE()
!    
!   CALL PRINTTEXT('Kriging applied Range:'//TRIM(RTOS(RANGE,'F',2))//' meter',1)
!
!   !## apply kriging interpolation
!   CALL KRIGING_MAIN(NXYZ,XYZ(:,1),XYZ(:,2),XYZ(:,3),DELR,DELC,NROW,NCOL,XPP,NODATA,RANGE,PEST_KTYPE)
!   DO IROW=1,NROW; DO ICOL=1,NCOL
!    IF(IBOUND(ICOL,IROW,ILAY).EQ.0)THEN
!     XPP(ICOL,IROW)=NODATA
!    ELSE
!     !## not equal to recharge
!     IF(PARAM(IPP)%LOG)XPP(ICOL,IROW)=EXP(XPP(ICOL,IROW))
!     !## areas outside range do get a value of 0.0, convert to 1.0
!     IF(XPP(ICOL,IROW).EQ.0.0)XPP(ICOL,IROW)=1.0
!    ENDIF
!   ENDDO; ENDDO
!   
!   WRITE(FNAME,'(A,I5.5,A)') TRIM(DIR)//CHAR(92)//PPPARAM(IPP),ILAY,'.IDF'
!   CALL WRITEIDF(XPP,1,NCOL,1,NROW,1,NROW,NCOL,1,DELR(0),DELC(NROW),SIMCSIZE,NODATA,FNAME)
!
!   SELECT CASE (PPPARAM(IPP))
!    CASE ('KD')  !## transmissivities
!     CC(:,:,ILAY)=CC(:,:,ILAY)*XPP
!!     !## write results
!!     CALL WRITEIDF(CC(:,:,ILAY),1,NCOL,1,NROW,1,NROW,NCOL,1,DELR(0),DELC(NROW),SIMCSIZE,NODATA,FNAME)
!    CASE ('KH')
!     KHV(:,:,ILAY)=KHV(:,:,ILAY)*XPP
!!     !## write results
!!     CALL WRITEIDF(KHV(:,:,ILAY),1,NCOL,1,NROW,1,NROW,NCOL,1,DELR(0),DELC(NROW),SIMCSIZE,NODATA,FNAME)
!    CASE ('VC')
!     CV(:,:,ILAY)=CV(:,:,ILAY)*XPP
!!     !## write results
!!     CALL WRITEIDF(CV(:,:,ILAY),1,NCOL,1,NROW,1,NROW,NCOL,1,DELR(0),DELC(NROW),SIMCSIZE,NODATA,FNAME)
!    CASE ('KV')
!     KVV(:,:,ILAY)=KVV(:,:,ILAY)*XPP
!!     !## write results
!!     CALL WRITEIDF(KVV(:,:,ILAY),1,NCOL,1,NROW,1,NROW,NCOL,1,DELR(0),DELC(NROW),SIMCSIZE,NODATA,FNAME)
!    CASE ('SC')
!     SC(:,:,ILAY)=SC(:,:,ILAY)*XPP
!!     !## write results
!!     CALL WRITEIDF(SC(:,:,ILAY),1,NCOL,1,NROW,1,NROW,NCOL,1,DELR(0),DELC(NROW),SIMCSIZE,NODATA,FNAME)
!    CASE ('VA')
!     KVA(:,:,ILAY)=KVA(:,:,ILAY)*XPP
!!     !## write results
!!     CALL WRITEIDF(KVA(:,:,ILAY),1,NCOL,1,NROW,1,NROW,NCOL,1,DELR(0),DELC(NROW),SIMCSIZE,NODATA,FNAME)
!    CASE ('RE')
!     IP1=1; IP2=0
!     DO IS=1,PCK(PRCH)%NIP
!      IF(IS.GT.1)IP1=PCK(PRCH)%IP(IS-1)+1
!      IP2=PCK(PRCH)%IP(IS)
!      DO IP=IP1,IP2
!       INODE=IRCH(IP)
!       CALL GETRCL(INODE,NROW,NCOL,ILAY,IROW,ICOL)
!       XRCH(IP,1)=XRCH(IP,1)*XPP(ICOL,IROW)
!      ENDDO
!      DO IP=IP1,IP2
!       INODE=IRCH(IP)
!       CALL GETRCL(INODE,NROW,NCOL,ILAY,IROW,ICOL)
!       XPP(ICOL,IROW)=XRCH(IP,1)
!      ENDDO
!     ENDDO
!     !## write results
!     CALL WRITEIDF(XPP,1,NCOL,1,NROW,1,NROW,NCOL,1,DELR(0),DELC(NROW),SIMCSIZE,NODATA,FNAME)
!   END SELECT
!
!   DEALLOCATE(XYZ,XPP)
! 
!  ENDDO
! ENDDO
! 
! END SUBROUTINE PEST1ALPHA_MOD
! 
! !###====================================================================
! REAL FUNCTION PEST_GETRANGE()
! !###====================================================================
! IMPLICIT NONE
!    
! PEST_GETRANGE=0.9*SQRT((DELR(NCOL)-DELR(0))**2.0+(DELC(0)-DELC(NROW))**2.0)
! 
! PEST_GETRANGE=PEST_GETRANGE/4.0
!  
! END FUNCTION PEST_GETRANGE
!
! !###====================================================================
! SUBROUTINE PEST1ALPHA_PCK(HCOF,NODES)
! !###====================================================================
! USE MOD_UTL, ONLY : GETRCL
! USE GLBVAR, ONLY : PCK,PRIV,PISG,PDRN,PGHB,KPER
! USE PCKVAR, ONLY : XRIV,IRIV,XISG,IISG,XDRN,IDRN,XGHB,IGHB
! IMPLICIT NONE 
! INTEGER,INTENT(IN) :: NODES
! REAL,DIMENSION(NODES),INTENT(OUT) :: HCOF
! INTEGER :: ICOL,IROW,ILS,I,J,INODE,IP,IP1,IP2,IS,IPCK,NADJ,ILAY
! REAL :: FCT,PPART
! 
! IF(MMOD(PPST).LE.0)RETURN
! 
! DO I=1,SIZE(PARAM)
!
!  SELECT CASE (TRIM(PARAM(I)%PTYPE))
!   CASE ('KD','KH','KV','VC','SC','AF','AA','MS','MC','VA','HF','EX'); CYCLE
!  END SELECT
!
!  !## skip pilot-points
!  IF(PARAM(I)%ZTYPE.EQ.1)CYCLE
!
!  FCT=PARAM(I)%ALPHA(1); ILS=PARAM(I)%ILS
!  IF(PARAM(I)%LOG)FCT=EXP(FCT); 
!  !## scaling
!  SELECT CASE (TRIM(PARAM(I)%PTYPE))
!   CASE ('RC','RI'); IPCK=PRIV
!   CASE ('IC','II'); IPCK=PISG
!   CASE ('DC');      IPCK=PDRN
!   CASE ('RE');      IPCK=PRCH
!   CASE ('GC');      IPCK=PGHB
!  END SELECT
!
!  !## evaluate each system for the package
!  IP1=1
!  DO IS=1,PCK(IPCK)%NIP
!   IF(ILS.NE.IS)CYCLE
!   
!   HCOF=0.0; NADJ=0
!   
!   IF(IS.GT.1)IP1=PCK(IPCK)%IP(IS-1)+1
!   IP2=PCK(IPCK)%IP(IS)   
!
!   !## put location of package in hcof(:,:)
!   SELECT CASE (TRIM(PARAM(I)%PTYPE))
!    CASE ('RC','RI')  !## river conductances/infiltration factor
!     DO IP=IP1,IP2
!      IF(XRIV(IP,1).GT.0.0)THEN
!       CALL GETRCL(IRIV(IP),NROW,NCOL,ILAY,IROW,ICOL)
!       INODE=(IROW-1)*NCOL+ICOL
!       HCOF(INODE)=REAL(IP)
!      ENDIF
!     ENDDO
!    CASE ('IC','II')  !## isg conductances/infiltration factor
!     DO IP=IP1,IP2
!      IF(XISG(IP,1).GT.0.0)THEN
!       CALL GETRCL(IISG(IP),NROW,NCOL,ILAY,IROW,ICOL)
!       INODE=(IROW-1)*NCOL+ICOL
!       HCOF(INODE)=REAL(IP) 
!      ENDIF
!     ENDDO
!    CASE ('DC')  !## drain conductances
!     DO IP=IP1,IP2
!      IF(XDRN(IP,1).GT.0.0)THEN
!       CALL GETRCL(IDRN(IP),NROW,NCOL,ILAY,IROW,ICOL)
!       INODE=(IROW-1)*NCOL+ICOL
!       IF(IDRN(IP).GT.0)HCOF(INODE)=REAL(IP)
!      ENDIF
!     ENDDO
!    CASE ('GC')  !## general head conductances
!     DO IP=IP1,IP2
!      IF(XGHB(IP,1).GT.0.0)THEN
!       CALL GETRCL(IGHB(IP),NROW,NCOL,ILAY,IROW,ICOL)
!       INODE=(IROW-1)*NCOL+ICOL
!       IF(IGHB(IP).GT.0)HCOF(INODE)=REAL(IP)
!      ENDIF
!     ENDDO
!    CASE ('RE')  !## recharge
!     DO IP=IP1,IP2
!      CALL GETRCL(IRCH(IP),NROW,NCOL,ILAY,IROW,ICOL)
!      INODE=(IROW-1)*NCOL+ICOL
!      IF(IRCH(IP).GT.0)HCOF(INODE)=REAL(IP) 
!     ENDDO
!   END SELECT
!   EXIT
!  ENDDO
!
!  !## allocate memory to make copy of data
!  IF(.NOT.ASSOCIATED(PARAM(I)%X))ALLOCATE(PARAM(I)%X(PARAM(I)%NODES))
!
!  IF(PEST_ITER.EQ.0)THEN
!   DO J=1,PARAM(I)%NODES
!    IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
!    INODE=(IROW-1)*NCOL+ICOL
!    IP=INT(HCOF(INODE))
!    IF(IP.GT.0)THEN
!     !## do not apply factor for packages on list-bases
!     SELECT CASE (TRIM(PARAM(I)%PTYPE))
!      CASE ('RC')  !## river conductances
!       PARAM(I)%X(J)=XRIV(IP,1) !*PARAM(I)%F(J)
!      CASE ('RI')  !## river infiltration factor
!       PARAM(I)%X(J)=XRIV(IP,4) !*PARAM(I)%F(J)
!      CASE ('IC')  !## isg conductances
!       PARAM(I)%X(J)=XISG(IP,1) !*PARAM(I)%F(J)
!      CASE ('II')  !## isg infiltration factor
!       PARAM(I)%X(J)=XISG(IP,4) !*PARAM(I)%F(J)
!      CASE ('DC')  !## river conductances
!       PARAM(I)%X(J)=XDRN(IP,1) !*PARAM(I)%F(J)
!      CASE ('GC')  !## general head conductances
!       PARAM(I)%X(J)=XGHB(IP,1) !*PARAM(I)%F(J)
!      CASE ('RE')  !## recharge
!       PARAM(I)%X(J)=XRCH(IP,1) !*PARAM(I)%F(J)
!     END SELECT
!    ENDIF
!   ENDDO
!  ENDIF
! 
!  DO J=1,PARAM(I)%NODES
!   IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
!   INODE=(IROW-1)*NCOL+ICOL
!   IP=INT(HCOF(INODE))
!   IF(IP.GT.0)THEN
!    NADJ=NADJ+1
!    SELECT CASE (TRIM(PARAM(I)%PTYPE))
!     CASE ('RC')  !## river conductances
!      PPART     =XRIV(IP,1)-PARAM(I)%X(J)
!      XRIV(IP,1)=PPART+PARAM(I)%X(J)*FCT
!     CASE ('RI')  !## river infiltration factor
!      PPART     =XRIV(IP,4)-PARAM(I)%X(J)
!      XRIV(IP,4)=PPART+PARAM(I)%X(J)*FCT
!     CASE ('IC')  !## isg conductances
!      PPART     =XISG(IP,1)-PARAM(I)%X(J)
!      XISG(IP,1)=PPART+PARAM(I)%X(J)*FCT
!     CASE ('II')  !## isg infiltration factor
!      PPART     =XISG(IP,4)-PARAM(I)%X(J)
!      XISG(IP,4)=PPART+PARAM(I)%X(J)*FCT
!     CASE ('DC')  !## river conductances
!      PPART     =XDRN(IP,1)-PARAM(I)%X(J)
!      XDRN(IP,1)=PPART+PARAM(I)%X(J)*FCT
!     CASE ('GC')  !## river conductances
!      PPART     =XGHB(IP,1)-PARAM(I)%X(J)
!      XGHB(IP,1)=PPART+PARAM(I)%X(J)*FCT
!     CASE ('RE')  !## recharge
!      PPART     =XRCH(IP,1)-PARAM(I)%X(J)
!      XRCH(IP,1)=PPART+PARAM(I)%X(J)*FCT
!    END SELECT
!   ENDIF
!  ENDDO
!
!  LINE=' * Package '//PARAM(I)%PTYPE//' adjusted ('//TRIM(ITOS(NADJ))//') with alpha='//TRIM(RTOS(FCT,'F',7))
!  CALL PRINTTEXT(TRIM(LINE),1)
!
! ENDDO
!     
! END SUBROUTINE PEST1ALPHA_PCK
!
! !###====================================================================
! SUBROUTINE PEST1ALPHA_METASWAP()
! !###====================================================================
! USE BASVAR, ONLY : NROW,NCOL
! IMPLICIT NONE
! INTEGER :: IUSCL,IOS,I,J,IROW,ICOL,NUND,N
! REAL :: FCT,THETA,PRZ,COND,PWT
! TYPE SCLOBJ1
!  INTEGER :: NUND,IROW,ICOL
! END TYPE SCLOBJ1
! TYPE SCLOBJ2
!  REAL :: THETA,COND,PRZ,PWT
! END TYPE SCLOBJ2
! TYPE(SCLOBJ1),DIMENSION(:),ALLOCATABLE :: SCL1
! TYPE(SCLOBJ2),DIMENSION(:,:),ALLOCATABLE :: SCL2 
! 
! IF(MMOD(PPST).LE.0)RETURN 
! IF(MMOD(PCAP).EQ.0)RETURN
! !## check whether metaswap parameters are optimized whatsoever
! J=0; DO I=1,SIZE(PARAM); SELECT CASE (TRIM(PARAM(I)%PTYPE)); CASE ('MS','MC'); J=J+1; END SELECT; ENDDO
! IF(J.EQ.0)RETURN
! 
! !## count number of uscl_svat-units
! CALL OPENASC(IUSCL,'uscl_svat.inp','R')
! N=0; DO
!  READ(IUSCL,'(I10,4F8.0,2I10)',IOSTAT=IOS) NUND,THETA,COND,PRZ,PWT,ICOL,IROW
!  IF(IOS.NE.0)EXIT; N=N+1
! ENDDO; CLOSE(IUSCL)
! IF(N.EQ.0)CALL PRINTTEXT('Error no SVAT-units found',2)
! 
! ALLOCATE(SCL1(N)); SCL1%NUND=0; SCL1%ICOL=0; SCL1%IROW=0
! ALLOCATE(SCL2(NCOL,NROW)); SCL2%THETA=0.0; SCL2%COND=0.0; SCL2%PRZ=0.0
!
! CALL OPENASC(IUSCL,'uscl_svat.inp','R')
! DO I=1,N; READ(IUSCL,'(I10,4F8.0,2I10)',IOSTAT=IOS) NUND,THETA,COND,PRZ,PWT,ICOL,IROW
!  SCL1(I)%NUND =NUND
!  SCL1(I)%IROW =IROW
!  SCL1(I)%ICOL =ICOL
!  !## initialize again
!  SCL2(ICOL,IROW)%THETA=THETA !1.0 !THETA
!  SCL2(ICOL,IROW)%COND =COND  !1.0 !COND
!  SCL2(ICOL,IROW)%PRZ  =PRZ   !1.0 !PRZ
!  SCL2(ICOL,IROW)%PWT  =PWT   !1.0 !PWT
! ENDDO; CLOSE(IUSCL)
! 
! !## apply parameters estimation adjustments
! DO I=1,SIZE(PARAM)
!  SELECT CASE (TRIM(PARAM(I)%PTYPE))
!   CASE ('MS','MC')
!    FCT=PARAM(I)%ALPHA(1) 
!    IF(PARAM(I)%LOG)FCT=EXP(FCT)
!
!    !## allocate memory to make copy of data
!    IF(.NOT.ASSOCIATED(PARAM(I)%X))ALLOCATE(PARAM(I)%X(PARAM(I)%NODES))
!    SELECT CASE (TRIM(PARAM(I)%PTYPE))
!     CASE ('MS')  !## soil moisture
!      DO J=1,PARAM(I)%NODES
!       IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
!       IF(PEST_ITER.EQ.0)PARAM(I)%X(J)=SCL2(ICOL,IROW)%THETA
!       SCL2(ICOL,IROW)%THETA=PARAM(I)%X(J)*FCT
!      ENDDO   
!     CASE ('MC')  !## conductivity
!      DO J=1,PARAM(I)%NODES
!       IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
!       IF(PEST_ITER.EQ.0)PARAM(I)%X(J)=SCL2(ICOL,IROW)%COND
!       SCL2(ICOL,IROW)%COND=PARAM(I)%X(J)*FCT
!      ENDDO   
!    END SELECT
!   CASE DEFAULT
!    CYCLE
!  END SELECT
!  LINE=' * '//PARAM(I)%PTYPE//' adjusted ('//TRIM(ITOS(PARAM(I)%NODES))//') with alpha='//TRIM(RTOS(FCT,'F',7))
!  CALL PRINTTEXT(TRIM(LINE),1) 
! ENDDO
!  
! CALL OPENASC(IUSCL,'uscl_svat.inp','W')
! DO I=1,N
!  IROW=SCL1(I)%IROW; ICOL=SCL1(I)%ICOL
!  IF(MMOD(PPWT).EQ.0)THEN
!   WRITE(IUSCL,'(I10,3F8.3,8X,2I10)') SCL1(I)%NUND,SCL2(ICOL,IROW)%THETA, &
!                                      SCL2(ICOL,IROW)%COND,SCL2(ICOL,IROW)%PRZ,ICOL,IROW
!  ELSE
!   WRITE(IUSCL,'(I10,4F8.3,2I10)') SCL1(I)%NUND,SCL2(ICOL,IROW)%THETA, &
!                                   SCL2(ICOL,IROW)%COND,SCL2(ICOL,IROW)%PRZ,SCL2(ICOL,IROW)%PWT,ICOL,IROW
!  ENDIF
! ENDDO
! CLOSE(IUSCL)
! 
! DEALLOCATE(SCL1,SCL2)
!  
! END SUBROUTINE PEST1ALPHA_METASWAP
! 
! !###====================================================================
! SUBROUTINE PEST1_METEO_RCH()
! !###====================================================================
! IMPLICIT NONE
! INTEGER :: I,NODE,IROW,ICOL,ILAY
! REAL :: DXY
!   
! IF(MMOD(PPST).EQ.0)RETURN !## no parameter estimation active
! IF(MPCK(PRCH).EQ.0)RETURN !## no recharge available
! IF(MMOD(PCAP).NE.0)RETURN !## metaswap prefers
! DO I=1,ABS(IIPF)
!  IF(TS(I)%IPFTYPE.NE.3)RETURN !## no moment estimation
! ENDDO
! IF(KPER.NE.1)      RETURN !## no the first stressperiod?
!  
! DO I=1,NPCK(PRCH)
!  NODE=IRCH(I)
!  IF(NODE.GT.0.AND.NODE.LE.NODES)THEN
!   CALL GETRCL(NODE,NROW,NCOL,ILAY,IROW,ICOL)
!   DXY=(DELR(ICOL)-DELR(ICOL-1))*(DELC(IROW-1)-DELC(IROW))
!   !## transform from mm/day towards m3/day
!   XRCH(I,1)=XRCH(I,1)+RCH_IMPULSE_SIZE*DXY
!  ENDIF
! END DO
! 
! END SUBROUTINE PEST1_METEO_RCH
!
! !#####=================================================================
! SUBROUTINE PEST1_METEO_METASWAP()
! !#####=================================================================
! IMPLICIT NONE
! INTEGER :: IU,I,J,IOS
! TYPE METEOOBJ
!  INTEGER :: IS,IY
!  REAL :: D,N,E
! END TYPE METEOOBJ
! TYPE(METEOOBJ),DIMENSION(:),POINTER :: METEO,METEO_BU
! CHARACTER(LEN=256) :: LINE
! 
! IF(MMOD(PPST).EQ.0)RETURN
! DO I=1,ABS(IIPF); IF(TS(I)%IPFTYPE.NE.3)RETURN; ENDDO
! IF(MMOD(PCAP).EQ.0)RETURN
! 
! IF(PEST_ITER.EQ.0)THEN
!  CALL OPENASC(IU,'mete_svat.inp'    ,'R')
! ELSE
!  CALL OPENASC(IU,'mete_svat_org.inp','R')
! ENDIF
! 
! ALLOCATE(METEO(100)); J=SIZE(METEO)
! I=0
! DO
!  I=I+1
!  IF(I.GT.J)THEN
!   ALLOCATE(METEO_BU(I+100))
!   METEO_BU(1:J)=METEO
!   DEALLOCATE(METEO); METEO=>METEO_BU
!   NULLIFY(METEO_BU); J=SIZE(METEO)
!  ENDIF
!  READ(IU,'(F15.0,I5,2F10.0,I10)',IOSTAT=IOS) METEO(I)%D,METEO(I)%IY,METEO(I)%N,METEO(I)%E,METEO(I)%IS
!  IF(IOS.NE.0)EXIT
! ENDDO
! I=I-1; CLOSE(IU)
!
! IF(PEST_ITER.EQ.0)THEN 
!  CALL OPENASC(IU,'mete_svat_org.inp','W')
!  DO J=1,I; WRITE(IU,'(F15.2,I5,2F10.2,I10)',IOSTAT=IOS) METEO(J)%D,METEO(I)%IY,METEO(J)%N,METEO(J)%E,METEO(J)%IS; ENDDO
! ENDIF
! CLOSE(IU)
! 
! CALL OPENASC(IU,'mete_svat.inp','W')
! DO J=1,I
!  METEO(J)%N=RRECHSTRUCNIV*1000.0
!  IF(METEO(J)%D.EQ.1.0)METEO(J)%N=METEO(J)%N+(RCH_IMPULSE_SIZE*1000.0)  !## impulse in meters
!  WRITE(IU,'(F15.2,I5,2F10.2,I10)',IOSTAT=IOS) METEO(J)%D,METEO(I)%IY,METEO(J)%N,METEO(J)%E,METEO(J)%IS
! ENDDO
! CLOSE(IU)
! 
! DEALLOCATE(METEO); CLOSE(IU)
!
! LINE=' * adjusted mete_svat.inp with impulse of '//TRIM(RTOS(RCH_IMPULSE_SIZE,'G',4))
! CALL PRINTTEXT(TRIM(LINE),1) 
! 
! END SUBROUTINE PEST1_METEO_METASWAP
!
! !#####=================================================================
! LOGICAL FUNCTION PESTRUNEXT()
! !#####=================================================================
! IMPLICIT NONE
! INTEGER :: I
! LOGICAL :: LEX
! REAL :: FCT
! CHARACTER(LEN=256) :: LINE
!  
! PESTRUNEXT=.TRUE.
! IF(PEST_ITER.EQ.0)RETURN
! 
! DO I=1,SIZE(PARAM)
!  SELECT CASE (TRIM(PARAM(I)%PTYPE))
!   CASE ('EX')
!    INQUIRE(FILE=PARAM(I)%EXBATFILE,EXIST=LEX)
!    IF(.NOT.LEX)THEN
!     CALL PRINTTEXT('Batchfile does not exists',0)
!     CALL PRINTTEXT(LINE,2)
!    ENDIF
!    FCT=PARAM(I)%ALPHA(1)
!    IF(PARAM(I)%LOG)FCT=EXP(FCT)
!    LINE=TRIM(PARAM(I)%EXBATFILE)//','//TRIM(RTOS(FCT,'F',5))
!    CALL SYSTEM(TRIM(LINE),2)
!  END SELECT
! ENDDO
! 
! END FUNCTION PESTRUNEXT
! 
! !#####=================================================================
! LOGICAL FUNCTION PESTNEXT()
! !#####=================================================================
! IMPLICIT NONE
! REAL :: IMPROVEMENT,F
! INTEGER :: I
! 
! IF(MMOD(PPST).LE.0)THEN ; PESTNEXT=.TRUE.; RETURN; ENDIF
! 
! PESTNEXT=.FALSE.
!
! IF(PEST_IGRAD.EQ.0)CALL PESTOPENFILE(IUPESTRESIDUAL,'log_pest_residual_'//TRIM(ITOS(PEST_ITER)),'txt',0)
!
! IF(PEST_ITER.EQ.0)PEST_ITER=1
! 
! !## compute objective function
! CALL PEST_GETJ()
! 
! IF(LSENS)THEN
!!  !## next parameter combination
!!  IF(.NOT.PESTNEXTSENS())STOP
!!  IF(.NOT.PESTNEXTGRAD())STOP
!!stop
!  IF(.NOT.PESTNEXTGRAD())CALL PESTGRADIENT()
!
! ELSEIF(LGRAD)THEN
!  !## what proces is going on?
!  IF(.NOT.PESTNEXTGRAD())THEN
!   !## get gradient
!   CALL PESTGRADIENT()
!   LLNSRCH=.TRUE.; PEST_ILNSRCH=1; LGRAD=.FALSE.; PEST_IGRAD=0 
!  ENDIF
! ELSEIF(LLNSRCH)THEN
!  !## no reduction of objection function, change (u(i))
!  IF(TJ.GT.TJOBJ)THEN
!   IF(.NOT.PESTUPGRADEVECTOR(0.5,.TRUE.))THEN !,.TRUE.))THEN
!    STOP 'ERROR PESTUPGRADEVECTOR IN LINESEARCH'
!   ENDIF !# half of current search-gradient
!   !## start next line-search
!   PEST_ILNSRCH=PEST_ILNSRCH+1
!  ELSE
!
!   !## continue ?
!   IF(PEST_ITER+1.GT.PEST_NITER)THEN
!    PESTNEXT=.TRUE.  !## max. number of iterations reached
!    CALL PRINTTEXT('',-1); CALL PRINTTEXT('Pest iteration terminated: PEST_ITER ('//TRIM(ITOS(PEST_ITER))//') = PEST_NITER ('//TRIM(ITOS(PEST_NITER))//')',-1)
!   ENDIF
!   IF(TJ.LE.0.0)THEN
!    PESTNEXT=.TRUE.
!    CALL PRINTTEXT('',-1); CALL PRINTTEXT('Objective Function <= 0.0 ('//TRIM(RTOS(REAL(TJ),'G',7))//')',-1)
!   ENDIF    
!    
!   IMPROVEMENT=0; DO I=1,SIZE(PARAM)
!    IF(PARAM(I)%LOG)THEN
!     F=(EXP(PARAM(I)%ALPHA(1))/EXP(PARAM(I)%ALPHA(2)))*100.0
!     F=ABS(F-100.0)
!     IMPROVEMENT=IMPROVEMENT+F
!    ELSE
!     F=(PARAM(I)%ALPHA(1)/PARAM(I)%ALPHA(2))*100.0
!     F=ABS(F-100.0)
!     IMPROVEMENT=IMPROVEMENT+F
!    ENDIF 
!   ENDDO
!
!   WRITE(IUPESTEFFICIENCY,'(6E15.7)') TJ,SQRT(TJ),TJ/REAL(PEST_NOBS),REAL(SQRT(TJ))/REAL(PEST_NOBS),IMPROVEMENT,(TJOBJ/TJ) 
!
!   WRITE(IUPESTRUNFILE,'(/A,I10/)') 'Copy in the runfile, iteration ',PEST_ITER
!   DO I=1,SIZE(PARAM)
!    IF(PARAM(I)%LOG)THEN
!     WRITE(IUPESTRUNFILE,'(I3,1X,A,1X,2(I5,1X),5(F10.3,1X),I3,L10)') PARAM(I)%IACT, &  !## iact
!         PARAM(I)%PTYPE, &         !## ptype
!         PARAM(I)%ILS, &           !## ilayer/system
!         PARAM(I)%IZONE, &         !## zone number
!         EXP(PARAM(I)%ALPHA(1)), & !## initial value
!         EXP(PARAM(I)%DELTA), &    !## finite difference step
!         EXP(PARAM(I)%MIN), &      !## minimal value
!         EXP(PARAM(I)%MAX),&       !## maximal value
!         PARAM(I)%FADJ,&           !## maximal adjust factor
!         ABS(PARAM(I)%IGROUP),&    !## group number
!         PARAM(I)%LOG              !## log transformed
!    ELSE
!     WRITE(IUPESTRUNFILE,'(I3,1X,A,1X,2(I5,1X),5(F10.3,1X),I3,L10)') PARAM(I)%IACT, &  !## iact
!         PARAM(I)%PTYPE, & !## ptype
!         PARAM(I)%ILS, &   !## ilayer/system
!         PARAM(I)%IZONE, & !## zone number
!         PARAM(I)%ALPHA(1), &   !## initial value
!         PARAM(I)%DELTA, & !## finite difference step
!         PARAM(I)%MIN, &   !## minimal value
!         PARAM(I)%MAX,&    !## maximal value
!         PARAM(I)%FADJ,&   !## maximal adjust factor
!         ABS(PARAM(I)%IGROUP),& !## group number
!         PARAM(I)%LOG      !## log transformed
!    ENDIF 
!   ENDDO
!   
!   IF(IMPROVEMENT.LE.PEST_JSTOP)THEN
!    PESTNEXT=.TRUE.  !## min. improvement reached
!    CALL PRINTTEXT('',-1); CALL PRINTTEXT('Pest iteration terminated decrease objective function ('//TRIM(RTOS(100.0*IMPROVEMENT,'G',7))// &
!     '%) > PEST_JSTOP ('//TRIM(RTOS(100.0*PEST_JSTOP,'G',7))//'%)',-1)
!   ENDIF
!
!   TJOBJ=TJ 
!   !## replace old by new parameter values
!   PARAM%ALPHA(2)=PARAM%ALPHA(1)
!   CALL PESTDUMPFCT()
!    
!   !## next iteration
!   PEST_ITER=PEST_ITER+1
!   IF(.NOT.PESTNEXT)THEN; CALL PRINTTEXT('',-1) ; CALL PRINTTEXT(' * Next Outer Iteration *',-1); ENDIF
!   LLNSRCH=.FALSE.; LGRAD=.TRUE.; PEST_IGRAD=0; PEST_ILNSRCH=0
!   IF(.NOT.PESTNEXTGRAD())THEN
!   ENDIF
!  ENDIF
!
! ENDIF
! 
! CALL FLUSH(IUPESTOUT); CALL FLUSH(IUPESTPROGRESS); CALL FLUSH(IUPESTEFFICIENCY)
! CALL FLUSH(IUPESTSENSITIVITY); CALL FLUSH(IUPESTRUNFILE)
! IF(IUPESTRESIDUAL.GT.0)THEN; CLOSE(IUPESTRESIDUAL); IUPESTRESIDUAL=0; ENDIF
!
! IMULT=MAX(0,IMULT-1)
! 
! END FUNCTION PESTNEXT
!
! !#####=================================================================
! SUBROUTINE PESTOPENFILE(IU,FNAME,EXT,INEW)
! !#####=================================================================
! IMPLICIT NONE
! CHARACTER(LEN=*),INTENT(IN) :: FNAME,EXT
! INTEGER,INTENT(IN) :: INEW
! INTEGER,INTENT(OUT) :: IU
! INTEGER :: IOS
! LOGICAL :: LEX
! 
! !## open pest result file
! IF(TRIM(CSUBMODEL).EQ.'')THEN
!  LINE=TRIM(ROOT)//CHAR(92)//'pest'//CHAR(92)//TRIM(FNAME)//TRIM(CVERSION)//'.'//TRIM(EXT)
! ELSE
!  LINE=TRIM(ROOT)//CHAR(92)//TRIM(CSUBMODEL)//CHAR(92)//'pest'//CHAR(92)//TRIM(FNAME)//TRIM(CVERSION)//'.'//TRIM(EXT)
! ENDIF
! 
! CALL CREATEDIR(LINE(:INDEX(LINE,CHAR(92),.TRUE.)-1))
! CALL UTL_FILENAME(LINE); INQUIRE(FILE=LINE,EXIST=LEX)
! IF(INEW.EQ.0)THEN
!  CALL OPENASC(IU,LINE,'W')
! ELSE
!  IU=GETUNIT()
!  OPEN(IU,FILE=LINE,STATUS='OLD',ACCESS='APPEND',IOSTAT=IOS)
!  IF(IOS.NE.0)IU=0
! ENDIF
! IF(IU.LE.0)CALL PRINTTEXT('Cannot open PEST-progress file '//TRIM(LINE),2)
!
! END SUBROUTINE PESTOPENFILE
! 
! !#####=================================================================
! SUBROUTINE PESTDUMPFCT()
! !#####=================================================================
! IMPLICIT NONE
! CHARACTER(LEN=256) :: FNAME,DIR
! INTEGER :: I,J,IROW,ICOL
! REAL :: XVAR,XF
! REAL,ALLOCATABLE,DIMENSION(:,:) :: X
! 
! !## open pest factor files
! IF(TRIM(CSUBMODEL).EQ.'')THEN
!  DIR=TRIM(ROOT)//CHAR(92)//'pest'//CHAR(92)//'factors'//TRIM(ITOS(PEST_ITER))
! ELSE
!  DIR=TRIM(ROOT)//CHAR(92)//TRIM(CSUBMODEL)//CHAR(92)//'pest'//CHAR(92)//'factors'//TRIM(ITOS(PEST_ITER))
! ENDIF
! CALL CREATEDIR(DIR)
! 
! ALLOCATE(X(NCOL,NROW))
! DO I=1,SIZE(PARAM)
!  !## do not save non-optimized parameters
!  IF(PARAM(I)%IACT.EQ.0)CYCLE
! 
!  !## regular-grid
!  IF(PARAM(I)%ZTYPE.EQ.0)THEN
!   WRITE(FNAME,'(A,2I5.5,A)') TRIM(DIR)//CHAR(92)//PARAM(I)%PTYPE,PARAM(I)%ILS,PARAM(I)%IZONE,'.IDF'
!   X=-999.0
!   !## log transformed
!   IF(PARAM(I)%LOG)THEN
!    DO J=1,PARAM(I)%NODES
!     IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
!     X(ICOL,IROW)=EXP(PARAM(I)%ALPHA(2))
!    ENDDO
!   ELSE
!    DO J=1,PARAM(I)%NODES
!     IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
!     X(ICOL,IROW)=PARAM(I)%ALPHA(2)
!    ENDDO
!   ENDIF 
!   IF(LQD)THEN
!    CALL WRITEIDF(X,1,NCOL,1,NROW,1,NROW,NCOL,1,DELR(0),DELC(NROW),SIMCSIZE,-999.0,FNAME)
!   ELSE
!    CALL WRITEIDF_NONEQUI(X,1,NCOL,1,NROW,1,DELR,DELC,NROW,NCOL,1,-999.0,FNAME)
!   ENDIF
!  ELSE
!   WRITE(FNAME,'(A,2I5.5,A)') TRIM(DIR)//CHAR(92)//PARAM(I)%PTYPE,PARAM(I)%ILS,PARAM(I)%IZONE,'.IPF'
!   XVAR=PARAM(I)%ALPHA_ERROR_VARIANCE(PEST_ITER)
!   XF  =PARAM(I)%ALPHA(2)
!   CALL WRITEIPF(SIZE(PARAM(I)%XY,1),SIZE(PARAM(I)%XY,2),PARAM(I)%XY,XF,XVAR,FNAME)  
!  ENDIF
! ENDDO
! DEALLOCATE(X)
! 
! END SUBROUTINE PESTDUMPFCT
!
! !#####=================================================================
! SUBROUTINE PESTPROGRESS()
! !#####=================================================================
! IMPLICIT NONE
! REAL,ALLOCATABLE,DIMENSION(:) :: X
! INTEGER :: I
!
! ALLOCATE(X(SIZE(PARAM)))
!
! DO I=1,SIZE(PARAM)
!  IF(PARAM(I)%LOG)THEN
!   X(I)=EXP(PARAM(I)%ALPHA(1))
!  ELSE
!   X(I)=PARAM(I)%ALPHA(1)
!  ENDIF
! ENDDO
!
! WRITE(BLINE,'(3I5,E15.7)') PEST_ITER,PEST_IGRAD,PEST_ILNSRCH,TJ
! DO I=1,SIZE(PARAM)
!  IF(ABS(PARAM(I)%IACT).EQ.1.AND.PARAM(I)%IGROUP.GT.0)THEN
!   WRITE(SLINE,'(F10.3)') X(I)
!   BLINE=TRIM(BLINE)//TRIM(SLINE)
!  ENDIF
! ENDDO
!
! WRITE(IUPESTPROGRESS,'(A)') TRIM(BLINE) 
! DEALLOCATE(X)
! 
! END SUBROUTINE PESTPROGRESS
!
!! !#####=================================================================
!! LOGICAL FUNCTION PESTNEXTSENS()
!! !#####=================================================================
!! IMPLICIT NONE
!! 
!! PESTNEXTSENS=.FALSE.
!! 
!! IF(PARAM(1)%ALPHA(1).GT.PARAM(1)%MAX)THEN
!!  PARAM(1)%ALPHA(1)=PARAM(1)%MIN-LOG(PARAM(1)%FADJ)
!!  PARAM(2)%ALPHA(1)=PARAM(2)%ALPHA(1)+LOG(PARAM(2)%FADJ)
!!  IF(PARAM(2)%ALPHA(1).GT.PARAM(2)%MAX)THEN
!!   RETURN
!!  ENDIF
!! ENDIF
!! PARAM(1)%ALPHA(1)=PARAM(1)%ALPHA(1)+LOG(PARAM(1)%FADJ)
!!
!!! IF(PARAM(1)%ALPHA(1).GT.PARAM(1)%MAX)THEN
!!!  PARAM(1)%ALPHA(1)=PARAM(1)%MIN-LOG(PARAM(1)%FADJ)
!!!  PARAM(2)%ALPHA(1)=PARAM(2)%ALPHA(1)+LOG(PARAM(2)%FADJ)
!!!  IF(PARAM(2)%ALPHA(1).GT.PARAM(2)%MAX)THEN
!!!   RETURN
!!!  ENDIF
!!! ENDIF
!!! PARAM(1)%ALPHA(1)=PARAM(1)%ALPHA(1)+LOG(PARAM(1)%FADJ)
!! 
!! PESTNEXTSENS=.TRUE.
!!  
!! END FUNCTION PESTNEXTSENS
!
! !#####=================================================================
! SUBROUTINE PESTWRITESTATISTICS_PERROR(NP,COV)
! !#####=================================================================
! USE VERSION, ONLY : CVERSION
! USE IMOD_IDF
! USE GLBVAR, ONLY : CDATE_SIM
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: NP
! DOUBLE PRECISION,INTENT(IN),DIMENSION(NP,NP) :: COV
! INTEGER :: I,J,K,IP1
! REAL :: Z1,Z2,Z,ZW
!
! !## The asymptotic standard parameter error is a measure of how unexplained variability in the
! !## data propagates to variability in the parameters, and is essentially an error measure for the
! !## parameters. The variance indicates the range over which a parameter value could extend without affecting model fit too adversely.
!
! WRITE(IUPESTOUT,'(/A)') 'Parameter Variance - Standard Parameter Error (standard deviation)'
! WRITE(IUPESTOUT,'(A/)')  'Indicates the range over which a parameter value could extend without affecting model fit too much'
!
! WRITE(BLINE,'(A15,99(A7,I3.3))') 'PARAMETER',('   ITER',I,I=PEST_ITER,1,-1)
! WRITE(IUPESTOUT,'(A/)') TRIM(BLINE)
!    
! J=0
! DO I=1,SIZE(PARAM)
!  IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0)THEN
!   J=J+1
!   IF(COV(J,J).GT.0.0)THEN
!    PARAM(I)%ALPHA_ERROR_VARIANCE(PEST_ITER)=SQRT(COV(J,J))
!   ELSE
!    !## error value - should not happen
!    PARAM(I)%ALPHA_ERROR_VARIANCE(PEST_ITER)=-999.99 
!   ENDIF
!   !##check whether current other parameters belong to this group
!   DO IP1=1,SIZE(PARAM)
!    !## active and follower of group
!    IF(PARAM(IP1)%IACT.EQ.1.AND.PARAM(IP1)%IGROUP.LT.0)THEN
!     IF(ABS(PARAM(IP1)%IGROUP).EQ.PARAM(I)%IGROUP)THEN
!      PARAM(IP1)%ALPHA_ERROR_VARIANCE(PEST_ITER)=PARAM(I)%ALPHA_ERROR_VARIANCE(PEST_ITER)
!     ENDIF
!    ENDIF
!   ENDDO
!  ELSEIF(PARAM(I)%IGROUP.EQ.0)THEN
!   PARAM(I)%ALPHA_ERROR_VARIANCE(PEST_ITER)=0.0
!  ENDIF
!  WRITE(BLINE,'(99(F10.3))') (PARAM(I)%ALPHA_ERROR_VARIANCE(K),K=PEST_ITER,1,-1)
!  WRITE(IUPESTOUT,'(3X,A2,2I3.3,A1,I3.3,A)') PARAM(I)%PTYPE,PARAM(I)%ILS,PARAM(I)%IZONE,'-',ABS(PARAM(I)%IGROUP),TRIM(BLINE)
! ENDDO
!
! WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'Confidence Limits'; WRITE(IUPESTOUT,*)                                                      
! WRITE(IUPESTOUT,'(2A15,A30)') 'Parameter','Estimated','95% percent confidence limits '
! WRITE(IUPESTOUT,'(15X,3A15)') 'Value','Lower limit','Upper limit'
! DO I=1,SIZE(PARAM)
!  ZW=PARAM(I)%ALPHA_ERROR_VARIANCE(PEST_ITER)*1.96
!  IF(PARAM(I)%LOG)THEN
!   Z =EXP(PARAM(I)%ALPHA(2))
!   Z1=TINY(1.0)
!   IF(PARAM(I)%ALPHA(2)-ZW.LT.LOG(HUGE(1.0)))THEN
!    Z1=EXP(PARAM(I)%ALPHA(2)-ZW)
!   ENDIF
!   Z2=HUGE(1.0)
!   IF(PARAM(I)%ALPHA(2)+ZW.LT.LOG(HUGE(1.0)))THEN
!    Z2=EXP(PARAM(I)%ALPHA(2)+ZW)
!   ENDIF
!!   Z1=EXP(PARAM(I)%ALPHA(2)-ZW)
!!   Z2=EXP(PARAM(I)%ALPHA(2)+ZW)
!!   !## lower/upper border in case exp() results into NaNs
!!   IF(Z1.NE.Z1)Z1=TINY(1.0)
!!   IF(Z2.NE.Z2)Z2=HUGE(1.0)
!  ELSE
!   Z= PARAM(I)%ALPHA(2)
!   Z1=PARAM(I)%ALPHA(2)-ZW
!   Z2=PARAM(I)%ALPHA(2)+ZW
!  ENDIF 
!  WRITE(BLINE,'(3G15.7)') Z1,Z,Z2
!  WRITE(IUPESTOUT,'(3X,A2,2I3.3,A1,I3.3,A)') PARAM(I)%PTYPE,PARAM(I)%ILS,PARAM(I)%IZONE,'-',ABS(PARAM(I)%IGROUP),TRIM(BLINE)
! ENDDO
! 
! WRITE(IUPESTOUT,'(A)') 'Note: confidence limits provide only an indication of parameter uncertainty.'
! WRITE(IUPESTOUT,'(A)') '      They rely on a linearity assumption which  may not extend as far in'
! WRITE(IUPESTOUT,'(A)') '      parameter space as the confidence limits themselves.'
! 
! END SUBROUTINE PESTWRITESTATISTICS_PERROR
!  
! !#####=================================================================
! SUBROUTINE PESTWRITESTATISTICS_FOSM(NP,COV)
! !#####=================================================================
! USE VERSION, ONLY : CVERSION
! USE IMOD_IDF
! USE GLBVAR, ONLY : CDATE_SIM
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: NP
! DOUBLE PRECISION,INTENT(IN),DIMENSION(NP,NP) :: COV
! INTEGER :: II,I,J,IPER,ILAY,IROW,ICOL
! TYPE(IDFOBJ),DIMENSION(1) :: H
! CHARACTER(LEN=256) :: FNAME
! REAL,ALLOCATABLE,DIMENSION(:,:) :: JCBN
! REAL,ALLOCATABLE,DIMENSION(:) :: PROW
!  
! J=NP
! ALLOCATE(JCBN(NCOL*NROW*NLAY,J),PROW(J))
!
! CALL CREATEDIR(TRIM(ROOTRES)//CHAR(92)//'uncertainty')
!
! DO IPER=1,NPER  
!
!  J=0
!  DO I=1,SIZE(PARAM)
!   IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0)THEN
!    J=J+1
!
!    FNAME=TRIM(ROOTRES)//CHAR(92)//'head\head_'//TRIM(CDATE_SIM(IPER))//'_l*_sens_'// &
!          TRIM(PARAM(I)%PTYPE)//'_igroup'//TRIM(ITOS(PARAM(I)%IGROUP))//'.idf'
!    CALL PRINTTEXT('Reading '//TRIM(FNAME),0) 
!
!    DO ILAY=1,NLAY
!     
!     FNAME=TRIM(ROOTRES)//CHAR(92)//'head\head_'//TRIM(CDATE_SIM(IPER))//'_l'//TRIM(ITOS(ILAY))//'_sens_'// &
!           TRIM(PARAM(I)%PTYPE)//'_igroup'//TRIM(ITOS(PARAM(I)%IGROUP))//'.idf'
!  
!     IF(.NOT.IDFREAD(H(1),FNAME,1))THEN
!      CALL PRINTTEXT('Can not open: '//TRIM(FNAME),0)
!      H(1)%X=0.0      
!     ENDIF 
!
!     II=(ILAY-1)*NCOL*NROW
!     DO IROW=1,NROW; DO ICOL=1,NCOL
!      II=II+1
!      JCBN(II,J)=H(1)%X(ICOL,IROW)
!     ENDDO; ENDDO
! 
!    ENDDO
!   ENDIF
!  ENDDO
!  
!  !## received the variance per location
!  H(1)%X=0.0
!
!  !## compute jcbn*cov*jcbn, process per row
!  IROW=1; ICOL=0; ILAY=1
!  DO II=1,NODES
!   ICOL=ICOL+1
!   IF(ICOL.GT.NCOL)THEN
!    IROW=IROW+1
!    IF(IROW.GT.NROW)THEN
!     FNAME=TRIM(ROOTRES)//CHAR(92)//'uncertainty\uncertainty_'//TRIM(CDATE_SIM(IPER))//'_l'//TRIM(ITOS(ILAY))//'.idf'
!     CALL PRINTTEXT('Writing '//TRIM(FNAME),0) 
!     IF(.NOT.IDFWRITE(H(1),FNAME,0))CALL PRINTTEXT('Can not write: '//TRIM(FNAME),2)
!     IROW=1; ILAY=ILAY+1
!    ENDIF
!    ICOL=1
!   ENDIF
!   PROW=0.0;
!   DO I=1,NP
!    DO J=1,NP
!     PROW(I)=PROW(I)+JCBN(II,I)*COV(I,J)
!    ENDDO
!   ENDDO
!   H(1)%X(ICOL,IROW)=0.0
!   DO I=1,NP
!    H(1)%X(ICOL,IROW)=H(1)%X(ICOL,IROW)+PROW(I)*PROW(I)
!   ENDDO
!   H(1)%X(ICOL,IROW)=SQRT(H(1)%X(ICOL,IROW))
!  
!  ENDDO
! 
!  !## schrijf laatste modellayer
!  FNAME=TRIM(ROOTRES)//CHAR(92)//'uncertainty\uncertainty_'//TRIM(CDATE_SIM(IPER))//'_l'//TRIM(ITOS(ILAY))//'.idf'
!  CALL PRINTTEXT('Writing '//TRIM(FNAME),0) 
!  IF(.NOT.IDFWRITE(H(1),FNAME,0))CALL PRINTTEXT('Can not write: '//TRIM(FNAME),2)
!  
! ENDDO 
!
! CALL IDFDEALLOCATE(H,SIZE(H))
! DEALLOCATE(JCBN)
!
! !## variance in head due to the variance (uncertainty) in the input variable
!
! CALL PRINTTEXT('Sensitivity finished',2)
! 
! END SUBROUTINE PESTWRITESTATISTICS_FOSM
!
! !#####=================================================================
! LOGICAL FUNCTION PESTNEXTGRAD()
! !#####=================================================================
! IMPLICIT NONE
! INTEGER :: I
! 
! PESTNEXTGRAD=.TRUE.
! 
! DO
!  PEST_IGRAD=PEST_IGRAD+1  
!  !## all gradients processed
!  IF(PEST_IGRAD.GT.SIZE(PARAM))EXIT
!  !## zero gradient in case parameter is fixed
!  IF(PARAM(PEST_IGRAD)%IACT.EQ.0)THEN
!   DH(PEST_IGRAD,:)=DH(0,:) 
!  !## check whether the parameters has been modified allready since it belongs to the same group
!  ELSEIF(PARAM(PEST_IGRAD)%IGROUP.LT.0)THEN
!   DH(PEST_IGRAD,:)=DH(0,:) 
!  !## possible candidate found
!  ELSE
!   EXIT
!  ENDIF
! ENDDO
! !## proceed gradient
! IF(PEST_IGRAD.LE.SIZE(PARAM))THEN
!  !## reset all alpha's
!  PARAM%ALPHA(1)=PARAM%ALPHA(2)
!  !## adjust all parameters within the same group
!  DO I=PEST_IGRAD,SIZE(PARAM)
!   IF(ABS(PARAM(I)%IGROUP).EQ.ABS(PARAM(PEST_IGRAD)%IGROUP))THEN
!    IF(PARAM(I)%LOG)THEN
!     PARAM(I)%ALPHA(1)=PARAM(I)%ALPHA(2)+PARAM(I)%DELTA
!    ELSE
!     PARAM(I)%ALPHA(1)=PARAM(I)%ALPHA(2)*PARAM(I)%DELTA
!    ENDIF
!   ENDIF
!  ENDDO
! ELSE
!  PESTNEXTGRAD=.FALSE.
! ENDIF
! 
! END FUNCTION PESTNEXTGRAD
!
! !###====================================================================
! SUBROUTINE PESTGRADIENT()
! !###====================================================================
! IMPLICIT NONE
! DOUBLE PRECISION :: DJ1,DJ2
! REAL :: TS,DF1,EIGWTHRESHOLD
! INTEGER :: I,II,J,K,NP,MP,IP1,NE
! INTEGER,ALLOCATABLE,DIMENSION(:) :: INDX
! REAL,ALLOCATABLE,DIMENSION(:,:) :: C,JS,P,PT
! DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:) :: EIGV,COV,B,M
! REAL,ALLOCATABLE,DIMENSION(:) :: S,N,RU 
! DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: EIGW
! LOGICAL :: LSCALING,LSVD
! 
! SELECT CASE (PEST_ISCALING)
!  CASE (0)
!   LSCALING=.FALSE.; LSVD=.FALSE.
!  CASE (1)
!   LSCALING=.TRUE.;  LSVD=.FALSE.
!  CASE (2)
!   LSCALING=.TRUE.;  LSVD=.TRUE.
!  CASE (3)
!   LSCALING=.FALSE.; LSVD=.TRUE.
! END SELECT
!   
! NP=SIZE(PARAM)
! !## sensitivity
! IF(.NOT.ALLOCATED(S)) ALLOCATE(S(NP)); S =0.0
!
! DO IP1=1,NP
!  IF(ABS(PARAM(IP1)%IACT).NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
!  DF1=PARAM(IP1)%DELTA
!  DO J=1,PEST_NOBS
!   S(IP1)=S(IP1)+W(J)*((DH(IP1,J)-DH(0,J))/DF1)
!  ENDDO
! ENDDO
! DO I=1,NP; S(I)=S(I)/REAL(PEST_NOBS); ENDDO
!                       
! WRITE(BLINE,'(A30)') '              Sensitivity (.):'
! DO I=1,NP
!  IF(ABS(PARAM(I)%IACT).NE.1.OR.PARAM(I)%IGROUP.LE.0)CYCLE
!  WRITE(SLINE,'(1X,F9.6)') S(I)
!  BLINE=TRIM(BLINE)//TRIM(SLINE)
! ENDDO
! WRITE(IUPESTPROGRESS   ,'(A)') TRIM(BLINE)
!  
! TS=SUM(ABS(S)); DO IP1=1,NP 
!  IF(ABS(PARAM(IP1)%IACT).NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
!  IF(TS.NE.0.0)S(IP1)=S(IP1)/TS
! ENDDO
! S=ABS(S)*100.0
!
! WRITE(BLINE,'(A30)') '              Sensitivity (%):'
! DO I=1,NP
!  IF(ABS(PARAM(I)%IACT).NE.1.OR.PARAM(I)%IGROUP.LE.0)CYCLE
!  WRITE(SLINE,'(F10.3)') S(I)
!  BLINE=TRIM(BLINE)//TRIM(SLINE)
! ENDDO
! WRITE(IUPESTPROGRESS,'(A)') TRIM(BLINE) 
! WRITE(IUPESTSENSITIVITY,'(I10,A)') PEST_ITER,TRIM(BLINE(31:))  
!  
! !## reset parameters
! DO I=1,SIZE(PARAM); PARAM(I)%ALPHA(1)=PARAM(I)%ALPHA(2); ENDDO
!
! !## melt frozen parameters
! DO IP1=1,SIZE(PARAM); IF(PARAM(IP1)%IACT.EQ.-1.AND.PARAM(IP1)%IGROUP.GT.0)PARAM(IP1)%IACT=1; ENDDO
!  
! !## freeze-insensitive parameters
! DO I=1,SIZE(PARAM)
!  IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0.AND.S(I).LT.PEST_SENSITIVITY)PARAM(I)%IACT=-1
! ENDDO
!
! !## freeze parameters that bounced against the boundary in the previous iteration and point in that direction again
! DO
! 
!  NP=0; DO I=1,SIZE(PARAM); IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0)NP=NP+1; ENDDO
!   
!  IF(ALLOCATED(JQJ))DEALLOCATE(JQJ);   ALLOCATE(JQJ (NP,NP))
!  IF(ALLOCATED(JQR))DEALLOCATE(JQR);   ALLOCATE(JQR (NP))
!  IF(ALLOCATED(U  ))DEALLOCATE(U);     ALLOCATE(U   (NP))
!  IF(ALLOCATED(EIGW))DEALLOCATE(EIGW); ALLOCATE(EIGW(NP))
!  IF(ALLOCATED(EIGV))DEALLOCATE(EIGV); ALLOCATE(EIGV(NP,NP))
!  IF(ALLOCATED(C   ))DEALLOCATE(C);    ALLOCATE(C   (NP,NP))
!
!  !## construct jTqr (<--- r is residual for current parameter set)
!  JQR=0.0; I=0
!  DO IP1=1,SIZE(PARAM)  !## row
!
!   IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
!
!   DF1=PARAM(IP1)%DELTA
!
!   I=I+1
!   DO J=1,PEST_NOBS
!    DJ1=(DH(IP1,J)-DH(0,J))/DF1
!    DJ2= DH(0 ,J)
!    JQR(I)=JQR(I)+(DJ1*W(J)*DJ2) 
!   ENDDO
!  ENDDO
! 
!  !## melt parameters that point inwards again since their last bump on the boundary
!  I=0; DO IP1=1,SIZE(PARAM)
!   IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
!   I=I+1
!   IF(ABS(PARAM(IP1)%IBND).EQ.1)THEN
!    IF(PARAM(IP1)%IBND.EQ.-1.AND.-1.0*JQR(I).LT.0.0)PARAM(IP1)%IACT=-1 !## still outside parameter domain(min)
!    IF(PARAM(IP1)%IBND.EQ. 1.AND.-1.0*JQR(I).GT.0.0)PARAM(IP1)%IACT=-1 !## still outside parameter domain(max)
!   ENDIF
!  ENDDO
!    
!  MP=0; DO I=1,SIZE(PARAM); IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0)MP=MP+1; ENDDO
!  IF(MP.EQ.NP)EXIT
!
! ENDDO
!
! IF(NP.EQ.0)CALL PRINTTEXT('All parameters are insensitive, process stopped!',2)
! 
! !## compute hessian and covariance, correlation matrix and eigenvalues/eigenvectors
! ALLOCATE(COV(NP,NP))
! CALL PEST1JQJ(JQJ,EIGW,EIGV,COV,NP,.TRUE.)
!
! !## multiply lateral sensitivities with sensitivities in case pest_niter=0
! CALL PESTWRITESTATISTICS_PERROR(NP,COV)
! IF(LSENS)CALL PESTWRITESTATISTICS_FOSM(NP,COV)
! 
! IF(LSCALING)THEN
!  IF(ALLOCATED(JS))DEALLOCATE(JS); ALLOCATE(JS(NP,PEST_NOBS))
! ENDIF
! 
! !## find until parameter update within hypersphere of parameters
! !## initiate marquardt as small as possible
! MARQUARDT=0.001
! DO
!
!  !## construct jqj - NORMAL MATRIX/HESSIAN
!  CALL PEST1JQJ(JQJ,EIGW,EIGV,COV,NP,.FALSE.)
!    
!  IF(.NOT.LSCALING)THEN
!
!!   !## levenberg
!!   DO I=1,NP; JQJ(I,I)=JQJ(I,I)+MARQUARDT; ENDDO
!   !## marquardt
!   DO I=1,NP; JQJ(I,I)=JQJ(I,I)+MARQUARDT*COV(I,I); ENDDO
! 
!   !## apply scaling
!  ELSE
!   !## compute scaling matrix
!   C=0.0; DO I=1,NP; C(I,I)=1.0/SQRT(JQJ(I,I)); ENDDO
!   
!   !## construct JS matrix, scaled
!   JS=0.0
!   DO I=1,PEST_NOBS    !## row
!    J=0
!    DO IP1=1,SIZE(PARAM)
!     IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
!     DF1=PARAM(IP1)%DELTA
!     J=J+1
!     DJ1=(DH(IP1,I)-DH(0,I))/DF1
!     JS(J,I)=JS(J,I)+DJ1*C(J,J)
!    ENDDO
!   ENDDO
!   !## construct JS-Q-JS - SCALED NORMAL MATRIX
!   JQJ=0.0
!   DO I=1,NP     !## row
!    DO J=1,NP    !## column
!     DO II=1,PEST_NOBS 
!      DJ1=JS(I,II)
!      DJ2=JS(J,II)
!      JQJ(J,I)=JQJ(J,I)+(DJ1*W(II)*DJ2)  
!     ENDDO
!    ENDDO
!   ENDDO
!
!   !## construct jTqr (<--- r is residual for current parameter set)
!   JQR=0.0; I=0
!   DO IP1=1,SIZE(PARAM)  !## row
!    IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
!    DF1=PARAM(IP1)%DELTA
!    I=I+1
!    DO J=1,PEST_NOBS 
!     DJ1=JS(I,J) 
!     DJ2= DH(0 ,J)
!     JQR(I)=JQR(I)+(DJ1*W(J)*DJ2)
!    ENDDO
!   ENDDO  
!
!   !## add levenberg/marquardt
!   DO I=1,NP; JQJ(I,I)=JQJ(I,I)+MARQUARDT*C(I,I)**2.0; ENDDO
!  
!  ENDIF
!    
!  !## project on important singular values
!  IF(LSVD)THEN
!  
!   EIGWTHRESHOLD=0.0 !% explained variance
!   WRITE(IUPESTOUT,'(/A10,2A15)') 'NE','EIGW(NE)','EIGWTHRESHOLD'
!   DO NE=1,NP
!    EIGWTHRESHOLD=EIGWTHRESHOLD+EIGW(NE)
!    WRITE(IUPESTOUT,'(I10,2F15.7)') NE,EIGW(NE),EIGWTHRESHOLD
!    IF(EIGWTHRESHOLD.GT.99.0)EXIT
!   ENDDO
!   WRITE(IUPESTOUT,'(/A/)') 'Use selected eigenvalues to project on limited space'
!
!   ALLOCATE(P(NP,NE)); P(:,1:NE)=EIGV(:,1:NE); ALLOCATE(M(NE,NE),N(NE),RU(NE),PT(NE,NP))
!   
!   !## compute pp=pt(jqj) on eigen-space
!   PT=0.0; DO I=1,NE; DO J=1,NP
!    DO K=1,NP
!     PT(I,J)=PT(I,J)+P(K,I)*JQJ(K,J)
!    ENDDO
!   ENDDO; ENDDO
!   !## project jqj on eigen-space
!   M=0.0; DO I=1,NE; DO J=1,NE
!    DO K=1,NP
!     M(I,J)=M(I,J)+PT(I,K)*P(K,J)
!    ENDDO
!   ENDDO; ENDDO
!   !## project right hand side on eigenspace
!   N=0.0; DO I=1,NE
!    DO K=1,NP
!     N(I)=N(I)+P(K,I)*JQR(K)
!    END DO
!   ENDDO
!
!   !## compute inverse of (Pt(JQJ)P)-1 -> B
!   IF(ALLOCATED(INDX))DEALLOCATE(INDX); ALLOCATE(INDX(NE))
!   IF(ALLOCATED(B   ))DEALLOCATE(B);    ALLOCATE(B   (NE,NE))
!   CALL LUDCMP_DBL(M,INDX,NE)
!   B=0.0; DO I=1,NE; B(I,I)=1.0; ENDDO
!   DO I=1,NE; CALL LUBKSB_DBL(M,INDX,B(1,I),NE); ENDDO
!
!   !## compute U=(M)-1*N
!   RU=0.0; DO I=1,NE; DO J=1,NE
!    RU(I)=RU(I)+(B(J,I)*N(J))
!   ENDDO; ENDDO
!
!   !## reproject reduced gradient on original space
!   !## compute U=(M)-1*N
!   U=0.0; DO I=1,NP; DO J=1,NE
!    U(I)=U(I)+(P(I,J)*RU(J))
!   ENDDO; ENDDO
!   
!   DEALLOCATE(P,PT,M,N,RU,INDX,B)
!  ELSE
!  
!   !## compute inverse of (JQJ)-1 -> B
!   IF(ALLOCATED(INDX))DEALLOCATE(INDX); ALLOCATE(INDX(NP))
!   IF(ALLOCATED(B   ))DEALLOCATE(B);    ALLOCATE(B   (NP,NP))
!   IF(NP.EQ.1)THEN
!    B(1,1)=1.0/JQJ(1,1)
!   ELSE
!    CALL LUDCMP_DBL(JQJ,INDX,NP)
!    B=0.0; DO I=1,NP; B(I,I)=1.0; ENDDO
!    DO I=1,NP; CALL LUBKSB_DBL(JQJ,INDX,B(1,I),NP); ENDDO
!   ENDIF
!   
!   !## compute (JQJ)-1*JQR
!   U=0.0
!   DO I=1,NP; DO J=1,NP
!    U(I)=U(I)+(B(J,I)*JQR(J))
!   ENDDO; ENDDO
!  
!   DEALLOCATE(INDX,B)
!
!  ENDIF
!  
!  !## apply scaling
!  IF(LSCALING)THEN
!   DO I=1,NP
!    U(I)=U(I)*C(I,I)
!   ENDDO
!  ENDIF
!  
!  U=-1.0*U !# pointing downhill
!
!  !## within parameter adjust-limits
!  IF(PESTUPGRADEVECTOR(1.0,.TRUE.))EXIT 
!  MARQUARDT=MARQUARDT*DAMPINGFACTOR 
!
! ENDDO !## marquardt-loop
! 
!! !## stepsize
!! IF(LSCALING)THEN
!!  IF(ALLOCATED(GAMMA))DEALLOCATE(GAMMA); ALLOCATE(GAMMA(PEST_NOBS))
!!  GAMMA=0
!!  DO I=1,PEST_NOBS
!!   J=0
!!   DO IP1=1,SIZE(PARAM)    !## row
!!    IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
!!    DF1=(PARAM(IP1)%ALPHA(2)+PARAM(IP1)%DELTA)-PARAM(IP1)%ALPHA(2)
!!    J=J+1
!!    DJ1=(DH(IP1,I)-DH(0,I))/DF1
!!    GAMMA(I)=GAMMA(I)+DJ1*U(J)
!!   ENDDO
!!  ENDDO
!!  DF1=0.0
!!  DO I=1,PEST_NOBS
!!   DF1=DF1+DH(0,I)*W(I)**2.0*GAMMA(I)
!!  ENDDO
!!  DF2=0.0
!!  DO I=1,PEST_NOBS
!!   DF2=DF2+(W(I)*GAMMA(I))**2.0
!!  ENDDO
!!  BETA=DF1/DF2
!!  DEALLOCATE(GAMMA) 
!! ENDIF
! 
! WRITE(IUPESTPROGRESS,*) 'Lambda/Damping Factor = ',MARQUARDT
! IF(LSCALING)WRITE(IUPESTPROGRESS,*) 'Scaling Value = ',MAXVAL(C)
! IF(LSVD)WRITE(IUPESTPROGRESS,*) 'Number of eigenvalues used: ',NE
!
! DEALLOCATE(S,C,EIGW,EIGV)  
! IF(ALLOCATED(JS))DEALLOCATE(JS)
! 
! END SUBROUTINE PESTGRADIENT
!
! !###====================================================================
! SUBROUTINE PEST1JQJ(JQJ,EIGW,EIGV,COV,NP,LVARIANCE)
! !###====================================================================
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: NP
! LOGICAL,INTENT(IN) :: LVARIANCE
! DOUBLE PRECISION,DIMENSION(NP,NP),INTENT(INOUT) :: JQJ,EIGV,COV
! DOUBLE PRECISION,DIMENSION(NP),INTENT(INOUT) :: EIGW
! INTEGER :: I,J,IP1,IP2,II,N
! REAL :: DF1,DF2,DJ1,DJ2,B1,TV,TEV,CB,DET,KAPPA
! DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:) :: B
! REAL,ALLOCATABLE,DIMENSION(:,:) :: COR
! DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: E
! INTEGER,ALLOCATABLE,DIMENSION(:) :: INDX
! 
! !## construct jqj - NORMAL MATRIX/HESSIAN
! JQJ=0.0; I=0
! DO IP1=1,SIZE(PARAM)    !## row
!  IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
!  DF1=PARAM(IP1)%DELTA
!  I=I+1; II=0; DO IP2=1,SIZE(PARAM)  !## column
!   IF(PARAM(IP2)%IACT.NE.1.OR.PARAM(IP2)%IGROUP.LE.0)CYCLE
!   DF2=PARAM(IP2)%DELTA
!   II=II+1
!   DO J=1,PEST_NOBS
!    DJ1=(DH(IP1,J)-DH(0,J))/DF1
!    DJ2=(DH(IP2,J)-DH(0,J))/DF2
!    JQJ(II,I)=JQJ(II,I)+(DJ1*W(J)*DJ2)  
!   ENDDO
!  ENDDO
! ENDDO
!
! !## construct covariance on the pilotpoints
! IF(PEST_IREGULARISATION.EQ.1)THEN
!  CALL PEST_GETQPP(NP,.FALSE.)
!  JQJ=JQJ+QPP
! ENDIF
! 
! IF(LVARIANCE)THEN
!
!  IF(ALLOCATED(E  ))DEALLOCATE(E);     ALLOCATE(E   (NP))
!  IF(ALLOCATED(COR ))DEALLOCATE(COR);  ALLOCATE(COR(NP,NP))
!  IF(ALLOCATED(INDX))DEALLOCATE(INDX); ALLOCATE(INDX(NP))
!  IF(ALLOCATED(B   ))DEALLOCATE(B);    ALLOCATE(B(NP,NP))
!
!  !## write JQJ matrix
!  WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'Parameter JQJ Matrix'; WRITE(IUPESTOUT,*)
!  BLINE=''
!  DO J=1,SIZE(PARAM)
!   IF(PARAM(J)%IACT.EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
!    WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PARAM(J)%PTYPE,PARAM(J)%ILS,PARAM(J)%IZONE,'-',PARAM(J)%IGROUP
!    BLINE=TRIM(BLINE)//TRIM(SLINE)
!   ENDIF
!  ENDDO
!  WRITE(IUPESTOUT,'(15X,A)') TRIM(BLINE) 
!  WRITE(IUPESTOUT,'(A)')
!  I=0
!  DO IP1=1,SIZE(PARAM)
!   IF(PARAM(IP1)%IACT.EQ.1.AND.PARAM(IP1)%IGROUP.GT.0)THEN
!    I=I+1
!    WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PARAM(IP1)%PTYPE,PARAM(IP1)%ILS,PARAM(IP1)%IZONE,'-',PARAM(IP1)%IGROUP
!    WRITE(IUPESTOUT,'(A15,999E15.7)') TRIM(SLINE),(JQJ(I,J),J=1,NP)
!   ENDIF
!  ENDDO
!
!  !## compute determinant of JQJ
!  DET=PEST_FIND_DET(JQJ,NP)
!  WRITE(IUPESTOUT,'(/A15,E15.7/)') 'Determinant JQJ = ',DET
!
!  !## copy data
!  B=JQJ
!  
!  !## eigenvalue of covariance matrix 
!  CALL RED1TRED2_DBL(B,NP,NP,EIGW,E)
!  CALL RED1TQLI_DBL(EIGW,E,NP,NP,B)
!  CALL RED1EIGSRT_DBL(EIGW,B,NP,NP)
!  WRITE(IUPESTOUT,'(/10X,4A15)') 'Eigenvalues','Sing.Values','Variance','Explained Var.'
!  DO I=1,NP; IF(EIGW(I).LE.0.0)EIGW(I)=0.0; ENDDO; TEV=SUM(EIGW)
!  TV=0.0
!  DO I=1,NP
!   TV=TV+(EIGW(I)*100.0/TEV)
!   IF(EIGW(I).GT.0.0)THEN
!    WRITE(IUPESTOUT,'(I10,4F15.7)') I,EIGW(I),SQRT(EIGW(I)),EIGW(I)*100.0/TEV,TV
!   ELSE
!    WRITE(IUPESTOUT,'(I10,4F15.7)') I,EIGW(I),     EIGW(I) ,EIGW(I)*100.0/TEV,TV
!   ENDIF
!  ENDDO
!  EIGV= B  
!  EIGW=(EIGW*100.0)/SUM(EIGW)  
!
!  !## condition number
!  KAPPA=SQRT(EIGW(1))/SQRT(EIGW(NP))
!  WRITE(IUPESTOUT,'(/A,3F15.7/)') 'Condition Number:',SQRT(EIGW(1)),SQRT(EIGW(NP)),KAPPA
!  WRITE(IUPESTOUT,'(/A,3F15.7/)') 'Condition Number (kappa):',LOG(KAPPA)
!  IF(LOG(KAPPA).GT.15)WRITE(IUPESTOUT,'(/A/)') '>>> Inversion is a concern due to multicollinearity (kappa>15) <<<'
!  IF(LOG(KAPPA).GT.30)WRITE(IUPESTOUT,'(/A/)') '>>> Inversion is highly questionable due to multicollinearity (kappa>30) <<<'
!
!  !## compute inverse of (JQJ)-1 -> B - covariance matrix
!  CALL LUDCMP_DBL(JQJ,INDX,NP)
!  B=0.0; DO I=1,NP; B(I,I)=1.0; ENDDO
!  DO I=1,NP; CALL LUBKSB_DBL(JQJ,INDX,B(1,I),NP); ENDDO
! 
!  !## parameter covariance matrix
!  
!  N=MAX(1,PEST_NOBS-NP)
!  B1=TJ/REAL(N)
!
!  WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'B1/TJ/N=',B1,TJ,N; WRITE(IUPESTOUT,*)
!
!  DO I=1,NP; DO J=1,NP; B(I,J)=B1*B(I,J); ENDDO; ENDDO
!
!  WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'Parameter Covariance Matrix (m2):'; WRITE(IUPESTOUT,*)
!
!  BLINE=''
!  DO J=1,SIZE(PARAM)
!   IF(PARAM(J)%IACT.EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
!    WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PARAM(J)%PTYPE,PARAM(J)%ILS,PARAM(J)%IZONE,'-',PARAM(J)%IGROUP
!    BLINE=TRIM(BLINE)//TRIM(SLINE)
!   ENDIF
!  ENDDO
!  WRITE(IUPESTOUT,'(15X,A)') TRIM(BLINE) 
!  WRITE(IUPESTOUT,'(A)')
!
!  I=0
!  DO IP1=1,SIZE(PARAM)
!   IF(PARAM(IP1)%IACT.EQ.1.AND.PARAM(IP1)%IGROUP.GT.0)THEN
!    I=I+1
!    WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PARAM(IP1)%PTYPE,PARAM(IP1)%ILS,PARAM(IP1)%IZONE,'-',PARAM(IP1)%IGROUP
!    WRITE(IUPESTOUT,'(A15,999E15.7)') TRIM(SLINE),(B(I,J),J=1,NP)
!    DO J=1,NP; COV(I,J)=B(I,J); ENDDO
!   ENDIF
!  ENDDO
!  
!  !## parameter correlation matrix
!  WRITE(IUPESTOUT,'(/A)') 'Parameter Correlation Matrix (-)'
!  WRITE(IUPESTOUT,'(A)')  'Indicates whether coordinated changes in the parameter values could produce the same simulated values and'
!  WRITE(IUPESTOUT,'(A/)') '  therefore, the same model fit'
!
!  BLINE=''
!  DO J=1,SIZE(PARAM)
!   IF(PARAM(J)%IACT.EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
!    WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PARAM(J)%PTYPE,PARAM(J)%ILS,PARAM(J)%IZONE,'-',PARAM(J)%IGROUP
!    BLINE=TRIM(BLINE)//TRIM(SLINE)
!   ENDIF
!  ENDDO
!  WRITE(IUPESTOUT,'(15X,A)') TRIM(BLINE) 
!  WRITE(IUPESTOUT,'(A)')
!
!  COR=0.0; I=0
!  DO IP1=1,SIZE(PARAM)
!   IF(PARAM(IP1)%IACT.EQ.1.AND.PARAM(IP1)%IGROUP.GT.0)THEN
!    I=I+1
!    DO J=1,NP
!     CB=B(I,I)*B(J,J)
!     IF(CB.GT.0.0)THEN
!      COR(I,J)=B(I,J)/SQRT(CB)
!     ELSE
!      COR(I,J)=0.0
!     ENDIF
!    ENDDO
!    WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PARAM(IP1)%PTYPE,PARAM(IP1)%ILS,PARAM(IP1)%IZONE,'-',PARAM(IP1)%IGROUP
!    WRITE(IUPESTOUT,'(A15,999F15.7)') TRIM(SLINE),(COR(I,J),J=1,NP)
!   ENDIF
!  ENDDO
!
!  !## write per parameter highly correlated other parameter
!  WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'Parameter Correlated to (correlation > 0.95):'; WRITE(IUPESTOUT,*)
!  IP1=0
!  DO I=1,SIZE(PARAM)
!   WRITE(BLINE,'(3X,A2,2I3.3,A1,I3.3)') PARAM(I)%PTYPE,PARAM(I)%ILS,PARAM(I)%IZONE,'-',PARAM(I)%IGROUP
!   IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0)THEN
!    IP1=IP1+1
!    IP2=0
!    DO J=1,SIZE(PARAM)
!     IF(PARAM(J)%IACT.EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
!      IP2=IP2+1
!      IF(I.NE.J)THEN
!       WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PARAM(J)%PTYPE,PARAM(J)%ILS,PARAM(J)%IZONE,'-',PARAM(J)%IGROUP
!       IF(ABS(COR(IP1,IP2)).GE.0.95)BLINE=TRIM(BLINE)//','//TRIM(SLINE)
!      ENDIF
!     ENDIF
!    ENDDO
!   ENDIF  
!   WRITE(IUPESTOUT,'(A)') TRIM(BLINE)
!  ENDDO
!  
!!  !## convergence criteria
!!  COR=0.0
!!  DO I=1,NP
!!   DO J=1,PEST_NOBS
!!    DF1=B(J,I)
!!    DF2=DH(0,J)
!!    COR(I,1)=COR(I,1)+(DF1*DF2) 
!!   ENDDO
!!  ENDDO
!!  R1=0.0; DO I=1,NP; R1=R1+COR(I,1)**2.0; ENDDO
!!  R2=0.0; DO I=1,PEST_NOBS; R2=R2+DH(0,I)**2.0; ENDDO
!!  WRITE(*,*) R1/R2,COS(R1/R2)
!  
!  IF(ALLOCATED(E   ))DEALLOCATE(E)
!  IF(ALLOCATED(COR ))DEALLOCATE(COR)
!  IF(ALLOCATED(INDX))DEALLOCATE(INDX)
!  IF(ALLOCATED(B   ))DEALLOCATE(B)
!
! ENDIF
!
! END SUBROUTINE PEST1JQJ
!
! !###====================================================================
! LOGICAL FUNCTION PESTUPGRADEVECTOR(FCT,LCHECK)
! !###====================================================================
! IMPLICIT NONE
! LOGICAL :: LCHECK
! REAL,INTENT(IN) :: FCT
! REAL :: F,G,MINP,MAXP,MINAP,MAXAP
! INTEGER :: I,J,IP1,IP2,N
! REAL,ALLOCATABLE,DIMENSION(:) :: GRADUPDATE
! 
! !## exit code
! PESTUPGRADEVECTOR=.FALSE.
! 
! I=0; DO IP1=1,SIZE(PARAM)
!  !## inactive parameter
!  IF(PARAM(IP1)%IACT.NE.1)THEN
!   G=0.0
!  !## find gradient for group
!  ELSEIF(PARAM(IP1)%IGROUP.LE.0)THEN
!   J=0; DO IP2=1,SIZE(PARAM)
!    IF(PARAM(IP2)%IACT.EQ.1.AND.PARAM(IP2)%IGROUP.GT.0)THEN
!     J=J+1
!     IF(PARAM(IP2)%IGROUP.EQ.ABS(PARAM(IP1)%IGROUP))THEN
!      G=U(J); EXIT
!     ENDIF
!    ENDIF
!   ENDDO
!  ELSE
!   I=I+1; G=U(I)
!  ENDIF
!  
!  PARAM(IP1)%ALPHA(1)=PARAM(IP1)%ALPHA(2)+G*FCT !## update parameters
!
!  !## adjustment too large
!  IF(LCHECK)THEN
!   IF(PARAM(IP1)%LOG)THEN
!    F=EXP(PARAM(IP1)%ALPHA(1))/EXP(PARAM(IP1)%ALPHA(2))
!   ELSE
!    F=PARAM(IP1)%ALPHA(1)/PARAM(IP1)%ALPHA(2)
!   ENDIF 
!  
!   IF(F.LT.1.0/PARAM(IP1)%FADJ.OR. &
!      F.GT.PARAM(IP1)%FADJ)THEN
!    RETURN
!   ENDIF
!  ENDIF
!
! ENDDO
!
! !## check whether boundary has been hit or maximum adjustment exceeds
! IF(LCHECK)THEN
!
!  DO IP1=1,SIZE(PARAM)
!   IF(PARAM(IP1)%IACT.NE.1)CYCLE 
!
!   MINP=PARAM(IP1)%MIN
!   MAXP=PARAM(IP1)%MAX
!
!   IF(PARAM(IP1)%LOG)THEN
!    MINAP=1.0/PARAM(IP1)%FADJ*EXP(PARAM(IP1)%ALPHA(2))
!    MAXAP=PARAM(IP1)%FADJ    *EXP(PARAM(IP1)%ALPHA(2))
!    MINAP=LOG(MINAP)
!    MAXAP=LOG(MAXAP)    
!   ELSE
!    MINAP=1.0/PARAM(IP1)%FADJ*PARAM(IP1)%ALPHA(2)
!    MAXAP=PARAM(IP1)%FADJ    *PARAM(IP1)%ALPHA(2)
!   ENDIF 
!  
!   MINP=MAX(MINP,MINAP)
!   MAXP=MIN(MAXP,MAXAP)
!   
!   IF(PARAM(IP1)%ALPHA(1).LT.MINP.OR. & 
!      PARAM(IP1)%ALPHA(1).GT.MAXP)THEN  
!
!    !## correct gradient
!    G=PARAM(IP1)%ALPHA(1)-PARAM(IP1)%ALPHA(2)
!    IF(PARAM(IP1)%ALPHA(1).LT.MINP)F=MINP-PARAM(IP1)%ALPHA(2)
!    IF(PARAM(IP1)%ALPHA(1).GT.MAXP)F=MAXP-PARAM(IP1)%ALPHA(2)
!    
!    IF(PARAM(IP1)%IBND.EQ.0)THEN  
!     !## correct all gradients with this factor
!     F=F/G
!     !## echo correction factor
!     CALL PRINTTEXT('',-1)
!     CALL PRINTTEXT('Parameter '//TRIM(ITOS(IP1))//' causing a',-1)
!     CALL PRINTTEXT('Correction factor '//TRIM(RTOS(F,'F',3))//' of Upgrade Vector caused by Bumping on the Parameter Boundary',-1)
!    ELSE
!     CALL PRINTTEXT('',-1)
!     CALL PRINTTEXT('Parameter '//TRIM(ITOS(IP1))//' causing an increase of the Marquardt factor',-1)
!     CALL PRINTTEXT('Conflict on the boundary between a Steepest Descent and Gauss-Newton approach',-1)
!     !## get another values for the marquardt such that this will not happen
!     RETURN
!    ENDIF
!    
!    DO IP2=1,SIZE(PARAM)
!     G=PARAM(IP2)%ALPHA(1)-PARAM(IP2)%ALPHA(2)
!     G=G*F
!     PARAM(IP2)%ALPHA(1)=PARAM(IP2)%ALPHA(2)+G !## update parameters
!    ENDDO
!   ENDIF
!   
!  ENDDO
!
! ENDIF
! 
! PESTUPGRADEVECTOR=.TRUE.
!
! CALL PRINTTEXT('',-1); CALL PRINTTEXT('Upgrade Vector Parameter:',-1)
! WRITE(LINE,'(A5,1X,A2,1X,2A15,A4)') 'NO','PT','NEW.FACTOR','PREV.FACTOR','BND'
! CALL PRINTTEXT(TRIM(LINE),-1) 
!
! PARAM%IBND=0
! DO IP1=1,SIZE(PARAM)
!  
!  !## parameter is at the boundary whenever less than 1% away
!  IF(ABS(PARAM(IP1)%ALPHA(1)-PARAM(IP1)%MIN).LT.XPBND)PARAM(IP1)%IBND=-1 !## min
!  IF(ABS(PARAM(IP1)%MAX)-PARAM(IP1)%ALPHA(1).LT.XPBND)PARAM(IP1)%IBND= 1 !## max
!
!  IF(PARAM(IP1)%LOG)THEN
!   WRITE(LINE,'(I5,1X,A2,1X,2F15.7,I4)') IP1,PARAM(IP1)%PTYPE,EXP(PARAM(IP1)%ALPHA(1)),EXP(PARAM(IP1)%ALPHA(2)),PARAM(IP1)%IBND
!   PARAM(IP1)%ALPHA_HISTORY(PEST_ITER)=EXP(PARAM(IP1)%ALPHA(1))
!  ELSE
!   WRITE(LINE,'(I5,1X,A2,1X,2F15.7,I4)') IP1,PARAM(IP1)%PTYPE,    PARAM(IP1)%ALPHA(1),     PARAM(IP1)%ALPHA(2), PARAM(IP1)%IBND
!   PARAM(IP1)%ALPHA_HISTORY(PEST_ITER)=PARAM(IP1)%ALPHA(1)
!  ENDIF 
!  CALL PRINTTEXT(TRIM(LINE),-1)
! ENDDO
!
! CALL PRINTTEXT('',-1); CALL PRINTTEXT('Upgrade Vector Parameter History:',-1)
! WRITE(BLINE,'(A5,1X,A2,1X,99(A7,I3.3))') 'NO','PT',('   ITER',I,I=PEST_ITER,0,-1)
! CALL PRINTTEXT(TRIM(BLINE),-1) 
! ALLOCATE(GRADUPDATE(PEST_ITER)); GRADUPDATE=0.0
! N=0
! DO IP1=1,SIZE(PARAM)
!  WRITE(BLINE,'(I5,1X,A2,1X,99(F10.3))') IP1,PARAM(IP1)%PTYPE,(PARAM(IP1)%ALPHA_HISTORY(I),I=PEST_ITER,0,-1)
!  CALL PRINTTEXT(TRIM(BLINE),-1)
!  IF(PARAM(IP1)%IGROUP.NE.0)THEN
!   N=N+1
!   DO I=1,PEST_ITER
!    GRADUPDATE(I)=GRADUPDATE(I)+(PARAM(IP1)%ALPHA_HISTORY(I)-PARAM(IP1)%ALPHA_HISTORY(I-1))**2.0
!   ENDDO
!  ENDIF
! ENDDO
! GRADUPDATE=SQRT(GRADUPDATE)
! WRITE(BLINE,'(9X,99F10.3)') (GRADUPDATE(I),I=PEST_ITER,1,-1)
! CALL PRINTTEXT(TRIM(BLINE),-1)
!  
! IF(GRADUPDATE(PEST_ITER).LT.PEST_PADJ)THEN
!  CALL PRINTTEXT('Process stopped, less than '//TRIM(RTOS(PEST_PADJ,'F',3))//' of vector length',2)
! ENDIF
! 
! DEALLOCATE(GRADUPDATE)
! 
! I=0; DO IP1=1,SIZE(PARAM)
!  IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
!  I=I+1; U(I)=U(I)*FCT
! ENDDO
! 
! END FUNCTION PESTUPGRADEVECTOR
!
! !###====================================================================
! SUBROUTINE PEST_GETJ()
! !###====================================================================
! IMPLICIT NONE
! INTEGER :: I,II,III,J,JJ,K,KK,ILAY,NROWIPFTXT,IUIPFTXT,NCOLIPFTXT, &
!     IOS,NAJ,NP
! REAL :: X,Y,Z,H,WW,MC,MM,DHH,XCOR,YCOR,ZCOR,XCROSS,RFIT !,FR1,FR2
! CHARACTER(LEN=52) :: ID
! DOUBLE PRECISION :: DHW
! REAL,ALLOCATABLE,DIMENSION(:) :: TSNODATA,M,C,GF_H,GF_O
! INTEGER,ALLOCATABLE,DIMENSION(:) :: IDATE
! REAL,DIMENSION(2) :: PC,PM,DYN !## percentiles computed/measured
!  
! DO JJ=1,ABS(IIPF)
!  
!  !## read ipf
!  IF(NPER.EQ.1)THEN
!   I=INDEX(TS(JJ)%IPFNAME,CHAR(92),.TRUE.)+1; LINE=TRIM(ROOTRES)//CHAR(92)//TS(JJ)%IPFNAME(I:)
!  ELSE
!   I=INDEX(TS(JJ)%IPFNAME,CHAR(92),.TRUE.)+1
!   LINE=TRIM(ROOTRES)//CHAR(92)//'timeseries'//CHAR(92)//TS(JJ)%IPFNAME(I:)
!  ENDIF
!  CALL UTL_SWAPSLASH(LINE); CALL OPENASC(TS(JJ)%IUIPF,LINE,'R')
!  READ(TS(JJ)%IUIPF,'(A256)') LINE; CALL UTL_STRING(LINE); READ(LINE,*) TS(JJ)%NROWIPF
!  READ(TS(JJ)%IUIPF,'(A256)') LINE; CALL UTL_STRING(LINE); READ(LINE,*) TS(JJ)%NCOLIPF
!  DO I=1,TS(JJ)%NCOLIPF; READ(TS(JJ)%IUIPF,*); ENDDO
!  !## read iext,ext
!  READ(TS(JJ)%IUIPF,'(A256)') LINE
!  READ(LINE,*) TS(JJ)%IEXT,TS(JJ)%EXT
! 
! ENDDO
!
! !## only one value per measurement
! IF(.NOT.ASSOCIATED(DH))ALLOCATE(DH(0:SIZE(PARAM),SUM(TS%NROWIPF)))
! IF(.NOT.ASSOCIATED(W ))ALLOCATE(W (SUM(TS%NROWIPF))) 
! IF(.NOT.ALLOCATED(GF_H))ALLOCATE(GF_H(SUM(TS%NROWIPF)))
! IF(.NOT.ALLOCATED(GF_O))ALLOCATE(GF_O(SUM(TS%NROWIPF)))
!
! !## initialise head-differences
! IF(PEST_IGRAD.EQ.0)DH(PEST_IGRAD,:)=0.0
! IF(PEST_IGRAD.NE.0)DH(PEST_IGRAD,:)=DH(0,:) !## zero gradient in case parameter is fixed
!
! W=0.0
! TJ=0.0
!  
! II=0
! DO JJ=1,ABS(IIPF)
! 
!  IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(/A/)') TRIM(TS(JJ)%IPFNAME)
!
!  IF(TS(JJ)%IEXT.EQ.0)THEN
!   IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(2A15,A10,6A15)') 'X','Y','ILAY','MSR','MDL','J','WMDL','WRESIDUAL','WEIGH'
!   DO I=1,TS(JJ)%NROWIPF
!    II=II+1
!    READ(TS(JJ)%IUIPF,*) X,Y,ILAY,Z,W(II),H    !## w(i)=variance
!    !## weigh=1/sqrt(variance)
!    IF(TS(JJ)%IVCOL.GT.0)THEN
!     IF(W(II).LE.0.0)THEN
!      !## insert measurement only whenever h.gt.z
!      IF(H.GT.Z)THEN
!       W(II)=ABS(W(II))
!      ELSE
!       W(II)=0.0
!      ENDIF
!     ELSE
!      W(II)=1.0/SQRT(W(II))
!     ENDIF
!    ENDIF
!
!    DHH=0.0
!    IF(ABS(H-Z).GT.PEST_DRES)THEN
!     DHH=H-Z
!    ENDIF
!    DH(PEST_IGRAD,II)=DHH  !## calculated - measured
!    DHW              =W(II)*(DHH**2.0)
!
!    GF_H(II)         =W(II)*H
!    GF_O(II)         =W(II)*Z
!
!    TJ               = TJ+DHW
!    IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(2F15.7,I10,6F15.7)') X,Y,ILAY,Z,H,DHW,W(II)*H,W(II)*(H-Z),W(II)
!   ENDDO
!  ELSE
!   IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(2A15,A10,8A15)') 'X','Y','ILAY','WEIGH','MSR','MDL','MDL-MSR','DYNMSR','DYNMDL','DYNMSR-DYNMDL','CROSS-COR'
!   
!   I=0
!   XCROSS=0.0
!   DO J=1,TS(JJ)%NROWIPF
!    READ(TS(JJ)%IUIPF,*) X,Y,ILAY,ID,WW     !## w(i)=variance
!    !## weigh=1/stdev=1/sqrt(variance)
!    IF(TS(JJ)%IVCOL.GT.0)THEN
!     IF(WW.LE.0.0)THEN
!      WW=0.0
!     ELSE
!      WW=1.0/SQRT(WW)
!     ENDIF
!    ENDIF
!    LINE=TRIM(ROOTRES)//CHAR(92)//'timeseries'//CHAR(92)//TRIM(ID)//'.'//TRIM(TS(JJ)%EXT)
!    IUIPFTXT=GETUNIT(); OPEN(IUIPFTXT,FILE=LINE,STATUS='OLD',ACTION='READ')
!    READ(IUIPFTXT,*) NROWIPFTXT; READ(IUIPFTXT,*) NCOLIPFTXT
!    ALLOCATE(TSNODATA(MAX(3,NCOLIPFTXT)))
!    DO K=1,NCOLIPFTXT; READ(IUIPFTXT,*) ID,TSNODATA(K); ENDDO
!    ALLOCATE(M(NROWIPFTXT),C(NROWIPFTXT),IDATE(NROWIPFTXT)); IDATE=0; C=0.0; M=0.0
!    IF(NCOLIPFTXT.LT.3)TSNODATA(3)=TSNODATA(2)
!    
!    !## get mean measure
!    KK=0; DO K=1,NROWIPFTXT
!     KK=KK+1
!     IF(TS(JJ)%IPFTYPE.EQ.2)      READ(IUIPFTXT,*,IOSTAT=IOS) IDATE(KK),M(KK),C(KK) 
!     IF(TS(JJ)%IPFTYPE.EQ.3)THEN; READ(IUIPFTXT,*,IOSTAT=IOS) IDATE(KK),M(KK); C(KK)=M(KK); ENDIF
!     !## error reading, skip it (can be caused by steady-state periods in between)
!     IF(IOS.NE.0)THEN; KK=KK-1; CYCLE; ENDIF
!
!     !## check period (if available)
!     IF(PEST_NPERIOD.GT.0)THEN
!      DO III=1,PEST_NPERIOD
!       IF(IDATE(KK).GE.PEST_IPERIOD(III,1).AND.IDATE(KK).LE.PEST_IPERIOD(III,2))EXIT
!      ENDDO
!      IF(III.GT.PEST_NPERIOD)C(KK)=TSNODATA(3)
!     ENDIF
!     IF(M(KK).EQ.TSNODATA(2).OR.C(KK).EQ.TSNODATA(3))KK=KK-1
!    ENDDO 
!
!    !## compute mean measurement in period
!    MM=-9999.99; MC=-9999.99; XCOR=-9999.99
!    IF(KK.GT.0)THEN
!     !## mean values
!     MM=SUM(M(1:KK))/REAL(KK) !## MEASUREMENTS
!     MC=SUM(C(1:KK))/REAL(KK) !## COMPUTED
!     !## percentiles
!     CALL IMOD_UTL_GETMED(M,KK,-999.99,(/10.0,90.0/),2,NAJ,PM)
!     CALL IMOD_UTL_GETMED(C,KK,-999.99,(/10.0,90.0/),2,NAJ,PC)
!     DYN(1)=PM(2)-PM(1) !## MEASUREMENTS
!     DYN(2)=PC(2)-PC(1) !## COMPUTED
!     !## compute cross-correlation
!     IF(KK.GT.1)THEN
!      XCOR=0.0; YCOR=0.0; ZCOR=0.0
!      DO K=1,KK
!       XCOR=XCOR+(MM-M(K))*(MC-C(K))
!       YCOR=YCOR+(MM-M(K))**2.0
!       ZCOR=ZCOR+(MC-C(K))**2.0
!      ENDDO
!      IF(YCOR.NE.0.0.AND.ZCOR.NE.0.0)XCOR=XCOR/(SQRT(YCOR)*SQRT(ZCOR))
!      XCROSS=XCROSS+XCOR
!     ENDIF
!    ENDIF
!
!    II =II+1
!    DHH=0.0
!
!    !## accept residuals less than 0.1 
!!    IF(ABS(C(K)-M(K)).GT.PEST_DRES)THEN
!    IF(ABS(MC-MM).GT.PEST_DRES)THEN
!      !## target is residual (calculated minus measured)
!     DHH=DHH+PEST_ITARGET(1)*(MC-MM)
!!     DHH=DHH+PEST_ITARGET(1)*(C(K)-M(K))
!    ENDIF
!
!    IF(ABS(DYN(2)-DYN(1)).GT.PEST_DRES)THEN
!     !## target is dynamics (calculated minus measured)
!     DHH=DHH+PEST_ITARGET(2)*(DYN(2)-DYN(1))
!    ENDIF
!
!    DH(PEST_IGRAD,II)=DHH       !## - total sensitivity
!
!    !## weight, pest_itarget(.) should/will be summed to one
!    W(II)=WW
!
!    DHW=W(II)*(DHH**2.0)
!    TJ=TJ+DHW
!
!    GF_H(II)=W(II)*MC !(K)
!    GF_O(II)=W(II)*MM !(K)
!
!    IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(2F15.7,I10,8F15.7)') X,Y,ILAY,W(II),MM,MC,MM-MC,DYN(1),DYN(2),DYN(2)-DYN(1),XCOR
!!    IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(I10,2F15.7,I10,9F15.7)') IDATE(K),X,Y,ILAY,M(K),C(K),W(II),DHW,MM,MC,XCOR,DYN(1),DYN(2)
!    DEALLOCATE(TSNODATA,C,M,IDATE)
!    CLOSE(IUIPFTXT)
!   ENDDO
!  ENDIF
!  CLOSE(TS(JJ)%IUIPF)
!  IF(TS(JJ)%NROWIPF.GT.0)THEN
!   IF(NPER.GT.1.AND.TS(JJ)%IPFTYPE.NE.3)CALL PRINTTEXT('MEAN Cross-Correlation         : '//TRIM(RTOS(XCROSS/REAL(TS(JJ)%NROWIPF),'F',7))//' (n='//TRIM(ITOS(TS(JJ)%NROWIPF))//')',1)
!  ENDIF
! ENDDO
! PEST_NOBS=II
! 
! !## run batch files
! CALL PEST_BATCHFILES()
!
! !## insert regularisation to objective function
! NP=0
! DO I=1,SIZE(PARAM)
!  IF(PARAM(I)%NODES.EQ.0.OR.PARAM(I)%IACT.EQ.0.OR.PARAM(I)%IGROUP.LE.0)CYCLE
!  NP=NP+1
! ENDDO
! 
! PJ=0.0D0
! IF(PEST_IREGULARISATION.EQ.1)CALL PEST_GETQPP(NP,.TRUE.)
!  
! CALL PRINTTEXT('Best Match Value   : '//TRIM(RTOS(REAL(TJ),'E',7)),-1)
! CALL PRINTTEXT('Plausibility Value : '//TRIM(RTOS(REAL(PJ),'E',7)),-1)
! TJ=TJ+PJ
!  
! CALL PRINTTEXT('TOTAL Objective Function Value : '//TRIM(RTOS(REAL(TJ),'E',7)),-1)
! CALL PRINTTEXT('MEAN Objective Function Value  : '//TRIM(RTOS(REAL(TJ)/REAL(PEST_NOBS),'E',7))//' (n='//TRIM(ITOS(PEST_NOBS))//')',-1)
!
! RFIT=PEST_GOODNESS_OF_FIT(GF_H,GF_O,PEST_NOBS)
! CALL PRINTTEXT('Goodness of Fit : '//TRIM(RTOS(RFIT,'E',7))//' (n='//TRIM(ITOS(PEST_NOBS))//')',-1)
! CALL PRINTTEXT('>> Provides a measure of the extent to which variability of field measurements is explained',-1)
! CALL PRINTTEXT('   by the calibrated model compared to that which can be constructed as purely random. <<',-1)
!
! IF(ALLOCATED(GF_H))DEALLOCATE(GF_H)
! IF(ALLOCATED(GF_O))DEALLOCATE(GF_O)
!  
! CALL PESTPROGRESS()
!
! IF(LGRAD)THEN
!  IF(PEST_IGRAD.EQ.0)THEN
!   IF(PEST_ITER.EQ.1)WRITE(IUPESTEFFICIENCY,'(3E15.7)') TJ,SQRT(TJ),TJ/REAL(PEST_NOBS)
!   TJOBJ=TJ
!  ELSE
!   PARAM(PEST_IGRAD)%TJOBJ=TJ
!  ENDIF
! ENDIF
! IF(LLNSRCH)THEN
!
! ENDIF
! 
! END SUBROUTINE PEST_GETJ
! 
!! !###====================================================================
!! REAL FUNCTION PEST_GOODNESS_OF_FIT(GF_H,GF_O,PEST_NOBS)
!! !###====================================================================
!! IMPLICIT NONE
!! INTEGER,INTENT(IN) :: PEST_NOBS
!! REAL,INTENT(IN),DIMENSION(:) :: GF_H,GF_O
!! REAL :: MU_H,MU_O,X1,X2
!! INTEGER :: I
!! 
!! MU_H=SUM(GF_H(1:PEST_NOBS))/REAL(PEST_NOBS)
!! MU_O=SUM(GF_O(1:PEST_NOBS))/REAL(PEST_NOBS)
!! 
!! X1=0.0; X2=0.0
!! DO I=1,PEST_NOBS
!!  X1=X1+(GF_H(I)-MU_H)*(GF_O(I)-MU_O)
!!  X2=X2+(GF_H(I)-MU_H)**2.0*(GF_O(I)-MU_O)**2.0
!! ENDDO
!!
!! WRITE(*,*) MU_H,MU_O,X1,X2
!! 
!! PEST_GOODNESS_OF_FIT=X1/SQRT(X2)
!!  
!! END FUNCTION PEST_GOODNESS_OF_FIT
!
! !###====================================================================
! REAL FUNCTION PEST_GOODNESS_OF_FIT(X,Y,N)
! !###====================================================================
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: N
! REAL,INTENT(IN),DIMENSION(N) :: X,Y
! REAL :: XN,YN,X1,X2,X3
! INTEGER :: I
! 
! XN=SUM(X)/REAL(N)
! YN=SUM(Y)/REAL(N)
! 
! X1=0.0; X2=0.0; X3=0.0
! DO I=1,N
!  X1=X1+(X(I)-XN)*(Y(I)-YN)
!  X2=X2+(X(I)-XN)**2.0
!  X3=X3+(Y(I)-YN)**2.0
! ENDDO
!
! IF(X2.NE.0.0.AND.X3.NE.0.0)PEST_GOODNESS_OF_FIT=X1/(SQRT(X2)*SQRT(X3))
!  
! END FUNCTION PEST_GOODNESS_OF_FIT
! 
! !###====================================================================
! SUBROUTINE PEST_GETQPP(NP,LPJ)
! !###====================================================================
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: NP
! LOGICAL,INTENT(IN) :: LPJ
! INTEGER :: I,J,II,JJ
! REAL :: ALPHA,GAMMA,X1,X2,Y1,Y2,RANGE
! REAL,ALLOCATABLE,DIMENSION(:) :: AQPP
! 
! RANGE=PEST_GETRANGE()
! 
! IF(ALLOCATED(QPP))DEALLOCATE(QPP)
! ALLOCATE(QPP(NP,NP)); QPP =0.0
! !## fill array zone and set appropriate pointers in type 
! II=0; DO I=1,SIZE(PARAM)
!  !## skip zero-zones, inactive parameters/groupmembers
!  IF(PARAM(I)%NODES.LE.0.OR.PARAM(I)%IACT.NE.1.OR.PARAM(I)%IGROUP.LE.0)CYCLE
!  
!  II=II+1
!  
!  !## zones
!  IF(PARAM(I)%ZTYPE.EQ.0)THEN
!   !## add covariance ... if known - now leave it zero
!   
!  !## pilotpoints
!  ELSEIF(PARAM(I)%ZTYPE.EQ.1)THEN
!   !## fill in covariance matrix based upon semivariogram
!   X1=PARAM(I)%XY(1,1); Y1=PARAM(I)%XY(1,2)
!   JJ=0; DO J=1,SIZE(PARAM)
!    !## skip zero-zones, inactive parameters/groupmembers
!    IF(PARAM(J)%NODES.LE.0.OR.PARAM(J)%IACT.NE.1.OR.PARAM(J)%IGROUP.LE.0)CYCLE
!    JJ=JJ+1
!    X2=PARAM(J)%XY(1,1); Y2=PARAM(J)%XY(1,2)
!    GAMMA=KRIGING_GETGAMMA(X1,Y1,X2,Y2,RANGE,SILL,NUGGET,PEST_KTYPE)
!    GAMMA=SILL-GAMMA
!    QPP(II,JJ)=1.0/(2.0*SQRT(GAMMA))
!   ENDDO
!  ENDIF
! ENDDO
!
! !## compute plausibility value
! IF(LPJ)THEN
! 
!  !## multiply with residual pilotpoints, homogeneous criterion
!  ALLOCATE(AQPP(NP)); AQPP=0.0
!  JJ=0; DO J=1,SIZE(PARAM)
!
!   !## skip zero-zones, inactive parameters/groupmembers
!   IF(PARAM(J)%NODES.LE.0.OR.PARAM(J)%IACT.NE.1.OR.PARAM(J)%IGROUP.LE.0)CYCLE
!   
!   JJ=JJ+1
!   
!   II=0; DO I=1,SIZE(PARAM)
!
!    !## skip zero-zones, inactive parameters/groupmembers
!    IF(PARAM(I)%NODES.LE.0.OR.PARAM(I)%IACT.NE.1.OR.PARAM(I)%IGROUP.LE.0)CYCLE
!    
!    II=II+1
!    
!    ALPHA=0.0-PARAM(I)%ALPHA(1)
!    AQPP(JJ)=AQPP(JJ)+ALPHA*QPP(JJ,II)
!
!   ENDDO
!  ENDDO
! 
!  !## multiply with residual pilotpoints, homogeneous criterion
!  PJ=0.0D0
!  II=0; DO I=1,SIZE(PARAM)
!   !## skip zero-zones, inactive parameters/groupmembers
!   IF(PARAM(I)%NODES.LE.0.OR.PARAM(I)%IACT.NE.1.OR.PARAM(I)%IGROUP.LE.0)CYCLE
!   II=II+1
!   ALPHA=0.0-PARAM(I)%ALPHA(1)
!   PJ=PJ+AQPP(II)*ALPHA
!  ENDDO
!
!  DEALLOCATE(AQPP)
! 
! ENDIF
!   
! END SUBROUTINE PEST_GETQPP
!
! !###====================================================================
! SUBROUTINE PEST_BATCHFILES()
! !###====================================================================
! IMPLICIT NONE
! INTEGER :: I,J,K,N,IUBAT,S1,S2
! REAL :: Z,WW,H,DHW
! 
! DO I=1,PEST_NBATCH
!  CALL PRINTTEXT(' Executing:'//TRIM(PEST_IBATCH(I)%BATCHFILE),0)
!  CALL SYSTEM(PEST_IBATCH(I)%BATCHFILE)  
!  CALL PRINTTEXT(' Reading  :'//TRIM(PEST_IBATCH(I)%OUTFILE),0)
!  IUBAT=GETUNIT(); OPEN(IUBAT,FILE=PEST_IBATCH(I)%OUTFILE,STATUS='OLD')
!  WRITE(IUPESTRESIDUAL,'(A)') TRIM(PEST_IBATCH(I)%OUTFILE)
!
!  READ(IUBAT,*) N
!
!  S1=SIZE(DH,1)-1; S2=SIZE(DH,2)
!  IF(PEST_NOBS+N.GT.S2)THEN
!   ALLOCATE(DH_DUMMY(0:S1,PEST_NOBS+N))
!   DO K=0,S1; DO J=1,S2
!    DH_DUMMY(K,J)=DH(K,J)
!   ENDDO; ENDDO
!   DEALLOCATE(DH); DH=>DH_DUMMY
!   ALLOCATE(W_DUMMY(PEST_NOBS+N))
!   DO J=1,S2; W_DUMMY(J)=W(J); ENDDO
!   DEALLOCATE(W); W=>W_DUMMY
!  ENDIF
!
!  DO J=1,N
!   PEST_NOBS=PEST_NOBS+1
!   READ(IUBAT,*) Z,WW,H
!   IF(WW.LE.0.0)THEN
!    WW=0.0
!   ELSE
!    !## weigh=1/variance
!    WW=1.0/SQRT(WW)
!   ENDIF
!   DH(PEST_IGRAD,PEST_NOBS)=(H-Z) !## calculated - measured
!   W(PEST_NOBS) = WW
!   DHW = WW*((H-Z)**2.0)
!   TJ = TJ+DHW
!   IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(30X,I10,6F15.7)') J,Z,H,DHW,WW*Z,WW*(H-Z),WW
!  ENDDO
!  CLOSE(IUBAT)
!
! ENDDO
! 
! END SUBROUTINE PEST_BATCHFILES
! 
! !###====================================================================
! SUBROUTINE PEST1CHK(IP)
! !###====================================================================
! IMPLICIT NONE 
! INTEGER,INTENT(IN) :: IP
! INTEGER :: I
! 
! PARAM(IP)%PTYPE=UTL_CAPF(PARAM(IP)%PTYPE,'U')
! DO I=1,MXPTYPE; IF(TRIM(PARAM(IP)%PTYPE).EQ.TRIM(PTYPE(I)))EXIT; ENDDO
! IF(I.GT.MXPTYPE)THEN
!  CALL PRINTTEXT('',0)
!  CALL PRINTTEXT('Error can not recognize parameter type:'//TRIM(PARAM(IP)%PTYPE),0)
!  CALL PRINTTEXT(' Choose from:',0)
!  DO I=1,MXPTYPE; CALL PRINTTEXT(' - '//TRIM(PTYPE(I)),0); ENDDO
!  CALL PRINTTEXT('',2)
! ENDIF
! IF(PARAM(IP)%MIN.GE.PARAM(IP)%MAX)THEN
!  CALL PRINTTEXT('No proper parameter width defined for parameter '//TRIM(ITOS(IP)),2)
! ENDIF
! IF(PARAM(IP)%INI.LT.PARAM(IP)%MIN.OR. &
!    PARAM(IP)%INI.GT.PARAM(IP)%MAX)THEN
!  CALL PRINTTEXT('Parameter '//TRIM(ITOS(IP))//' outside parameter width',2)
! ENDIF
! SELECT CASE (TRIM(PARAM(IP)%PTYPE))
!  CASE ('KD','KH','SC','AF','VA')
!   IF(PARAM(IP)%ILS.LE.0.OR.PARAM(IP)%ILS.GT.NLAY)   &
!    CALL PRINTTEXT('Parameter '//TRIM(ITOS(IP))//': ILS exceeds NLAY',2)
!  CASE ('VC','KV')
!   IF(PARAM(IP)%ILS.LE.0.OR.PARAM(IP)%ILS.GT.NLAY-1) &
!    CALL PRINTTEXT('Parameter '//TRIM(ITOS(IP))//': ILS exceeds NLAY-1',2)
! END SELECT
! 
! !## scaling
! IF(PARAM(IP)%LOG)THEN
!  IF(PARAM(IP)%DELTA.EQ.1.0)CALL PRINTTEXT('You can not specify delta alpha eq 1.0 for log-transformed parameters',2)
!  IF(PARAM(IP)%MIN  .EQ.0.0)CALL PRINTTEXT('You can not specify minimal value eq 0.0 for log-transformed parameters',2)
!  PARAM(IP)%INI  =LOG(PARAM(IP)%INI)
!  PARAM(IP)%MIN  =LOG(PARAM(IP)%MIN)
!  PARAM(IP)%MAX  =LOG(PARAM(IP)%MAX)
!  PARAM(IP)%DELTA=LOG(PARAM(IP)%DELTA)
! ENDIF
!
! END SUBROUTINE PEST1CHK
!
! !###========================================================================
! REAL FUNCTION PEST_FIND_DET(JQJ,N)
! !###========================================================================
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: N
! DOUBLE PRECISION,DIMENSION(N,N) :: JQJ
! DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE :: MATRIX
! DOUBLE PRECISION :: M, TEMP
! INTEGER :: I, J, K, L
! LOGICAL :: DETEXISTS = .TRUE.
!
! ALLOCATE(MATRIX(N,N))
! MATRIX=JQJ
! 
! L = 1
! !## convert to upper triangular form
! DO K = 1, N-1
!  IF (MATRIX(K,K) == 0) THEN
!   DETEXISTS = .FALSE.
!   DO I = K+1, N
!    IF (MATRIX(I,K) /= 0) THEN
!     DO J = 1, N
!      TEMP = MATRIX(I,J)
!      MATRIX(I,J)= MATRIX(K,J)
!      MATRIX(K,J) = TEMP
!     END DO
!     DETEXISTS = .TRUE.
!     L=-L
!     EXIT
!    ENDIF
!   END DO
!   IF (DETEXISTS .EQV. .FALSE.) THEN
!    PEST_FIND_DET = 0.0
!    DEALLOCATE(MATRIX)
!    RETURN
!   END IF
!  ENDIF
!  DO J = K+1, N
!   M = MATRIX(J,K)/MATRIX(K,K)
!   DO I = K+1, N
!    MATRIX(J,I) = MATRIX(J,I) - M*MATRIX(K,I)
!   END DO
!  END DO
! END DO
!    
! !## calculate determinant by finding product of diagonal elements
! PEST_FIND_DET = L
! DO I = 1, N
!  PEST_FIND_DET = PEST_FIND_DET * MATRIX(I,I)
! END DO
!
! DEALLOCATE(MATRIX)
!    
! END FUNCTION PEST_FIND_DET
!
! !###========================================================================
! SUBROUTINE RED1EIGSRT(D,A,N,NP)
! !###========================================================================
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: N,NP
! REAL,DIMENSION(NP),INTENT(INOUT) :: D
! REAL,DIMENSION(NP,NP),INTENT(INOUT) :: A
! INTEGER :: I,J,K
! REAL :: P
!
! DO I=1,N-1
!  K=I
!  P=D(I)
!  DO J=I+1,N
!   IF(D(J).GE.P)THEN
!    K=J
!    P=D(J)
!   ENDIF
!  END DO
!  IF(K.NE.I)THEN
!   D(K)=D(I)
!   D(I)=P
!   DO J=1,N
!    P=A(J,I)
!    A(J,I)=A(J,K)
!    A(J,K)=P
!   END DO
!  ENDIF
! END DO
!
! END SUBROUTINE RED1EIGSRT
!
! !###========================================================================
! SUBROUTINE RED1EIGSRT_DBL(D,A,N,NP)
! !###========================================================================
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: N,NP
! DOUBLE PRECISION,DIMENSION(NP),INTENT(INOUT) :: D
! DOUBLE PRECISION,DIMENSION(NP,NP),INTENT(INOUT) :: A
! INTEGER :: I,J,K
! DOUBLE PRECISION :: P
!
! DO I=1,N-1
!  K=I
!  P=D(I)
!  DO J=I+1,N
!   IF(D(J).GE.P)THEN
!    K=J
!    P=D(J)
!   ENDIF
!  END DO
!  IF(K.NE.I)THEN
!   D(K)=D(I)
!   D(I)=P
!   DO J=1,N
!    P=A(J,I)
!    A(J,I)=A(J,K)
!    A(J,K)=P
!   END DO
!  ENDIF
! END DO
!
! END SUBROUTINE RED1EIGSRT_DBL
!
! !###========================================================================
! SUBROUTINE RED1TRED2(A,N,NP,D,E)
! !###========================================================================
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: N,NP
! REAL,DIMENSION(NP,NP),INTENT(INOUT) :: A
! REAL,DIMENSION(NP),INTENT(INOUT) :: D,E
! INTEGER :: I,J,K,L
! REAL :: F,G,H,HH,SCALE
!
! DO I=N,2,-1
!  L=I-1
!  H=0.
!  SCALE=0.
!  IF(L.GT.1)THEN
!   DO K=1,L
!    SCALE=SCALE+ABS(A(I,K))
!   ENDDO
!   IF(SCALE.EQ.0.)THEN
!    E(I)=A(I,L)
!   ELSE
!    DO K=1,L
!     A(I,K)=A(I,K)/SCALE
!     H=H+A(I,K)**2.
!    ENDDO
!    F=A(I,L)
!    G=-SIGN(SQRT(H),F)
!    E(I)=SCALE*G
!    H=H-F*G
!    A(I,L)=F-G
!    F=0.
!    DO J=1,L
!     A(J,I)=A(I,J)/H
!     G=0.
!     DO K=1,J
!      G=G+A(J,K)*A(I,K)
!     ENDDO
!     DO K=J+1,L
!      G=G+A(K,J)*A(I,K)
!     ENDDO
!     E(J)=G/H
!     F=F+E(J)*A(I,J)
!    ENDDO
!    HH=F/(H+H)
!    DO J=1,L
!     F=A(I,J)
!     G=E(J)-HH*F
!     E(J)=G
!     DO K=1,J
!      A(J,K)=A(J,K)-F*E(K)-G*A(I,K)
!     ENDDO
!    ENDDO
!   ENDIF
!  ELSE
!   E(I)=A(I,L)
!  ENDIF
!  D(I)=H
! ENDDO
!
! D(1)=0.
! E(1)=0.
! DO I=1,N
!  L=I-1
!  IF(D(I).NE.0.)THEN
!   DO J=1,L
!    G=0.
!    DO K=1,L
!     G=G+A(I,K)*A(K,J)
!    END DO
!    DO K=1,L
!     A(K,J)=A(K,J)-G*A(K,I)
!    END DO
!   END DO
!  ENDIF
!  D(I)=A(I,I)
!  A(I,I)=1.
!  DO J=1,L
!   A(I,J)=0.
!   A(J,I)=0.
!  END DO
! END DO
!
! END SUBROUTINE RED1TRED2
!
! !###========================================================================
! SUBROUTINE RED1TRED2_DBL(A,N,NP,D,E)
! !###========================================================================
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: N,NP
! DOUBLE PRECISION,DIMENSION(NP,NP),INTENT(INOUT) :: A
! DOUBLE PRECISION,DIMENSION(NP),INTENT(INOUT) :: D,E
! INTEGER :: I,J,K,L
! DOUBLE PRECISION :: F,G,H,HH,SCALE
!
! DO I=N,2,-1
!  L=I-1
!  H=0.
!  SCALE=0.
!  IF(L.GT.1)THEN
!   DO K=1,L
!    SCALE=SCALE+ABS(A(I,K))
!   ENDDO
!   IF(SCALE.EQ.0.)THEN
!    E(I)=A(I,L)
!   ELSE
!    DO K=1,L
!     A(I,K)=A(I,K)/SCALE
!     H=H+A(I,K)**2.
!    ENDDO
!    F=A(I,L)
!    G=-SIGN(SQRT(H),F)
!    E(I)=SCALE*G
!    H=H-F*G
!    A(I,L)=F-G
!    F=0.
!    DO J=1,L
!     A(J,I)=A(I,J)/H
!     G=0.
!     DO K=1,J
!      G=G+A(J,K)*A(I,K)
!     ENDDO
!     DO K=J+1,L
!      G=G+A(K,J)*A(I,K)
!     ENDDO
!     E(J)=G/H
!     F=F+E(J)*A(I,J)
!    ENDDO
!    HH=F/(H+H)
!    DO J=1,L
!     F=A(I,J)
!     G=E(J)-HH*F
!     E(J)=G
!     DO K=1,J
!      A(J,K)=A(J,K)-F*E(K)-G*A(I,K)
!     ENDDO
!    ENDDO
!   ENDIF
!  ELSE
!   E(I)=A(I,L)
!  ENDIF
!  D(I)=H
! ENDDO
!
! D(1)=0.
! E(1)=0.
! DO I=1,N
!  L=I-1
!  IF(D(I).NE.0.)THEN
!   DO J=1,L
!    G=0.
!    DO K=1,L
!     G=G+A(I,K)*A(K,J)
!    END DO
!    DO K=1,L
!     A(K,J)=A(K,J)-G*A(K,I)
!    END DO
!   END DO
!  ENDIF
!  D(I)=A(I,I)
!  A(I,I)=1.
!  DO J=1,L
!   A(I,J)=0.
!   A(J,I)=0.
!  END DO
! END DO
!
! END SUBROUTINE RED1TRED2_DBL
!
! !###========================================================================
! REAL FUNCTION PYTHAG(A,B)
! !###========================================================================
! IMPLICIT NONE
! REAL,INTENT(IN) :: A,B
! REAL :: ABSA,ABSB
!
! ABSA=ABS(A)
! ABSB=ABS(B)
! IF(ABSA.GT.ABSB)THEN
!  PYTHAG=ABSA*SQRT(1.+(ABSB/ABSA)**2.)
! ELSE
!  IF(ABSB.EQ.0.)THEN
!   PYTHAG=0.
!  ELSE
!   PYTHAG=ABSB*SQRT(1.+(ABSA/ABSB)**2.)
!  ENDIF
! ENDIF
!
! END FUNCTION PYTHAG
!
! !###========================================================================
! REAL FUNCTION PYTHAG_DBL(A,B)
! !###========================================================================
! IMPLICIT NONE
! DOUBLE PRECISION,INTENT(IN) :: A,B
! DOUBLE PRECISION :: ABSA,ABSB
!
! ABSA=ABS(A)
! ABSB=ABS(B)
! IF(ABSA.GT.ABSB)THEN
!  PYTHAG_DBL=ABSA*SQRT(1.+(ABSB/ABSA)**2.)
! ELSE
!  IF(ABSB.EQ.0.)THEN
!   PYTHAG_DBL=0.
!  ELSE
!   PYTHAG_DBL=ABSB*SQRT(1.+(ABSA/ABSB)**2.)
!  ENDIF
! ENDIF
!
! END FUNCTION PYTHAG_DBL
! 
! !###========================================================================
! SUBROUTINE RED1TQLI(D,E,N,NP,A)
! !###========================================================================
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: N,NP
! REAL,DIMENSION(NP),INTENT(INOUT) :: D,E
! REAL,DIMENSION(NP,NP),INTENT(INOUT) :: A
! INTEGER :: I,ITER,K,L,M
! REAL :: B,C,DD,F,G,P,R,S
!
! DO I=2,N
!  E(I-1)=E(I)
! ENDDO
! E(N)=0.
! DO L=1,N
!  ITER=0
!1  DO M=L,N-1
!   DD=ABS(D(M))+ABS(D(M+1))
!   IF(ABS(E(M))+DD.EQ.DD)GOTO 2
!  END DO
!  M=N
!2  IF(M.NE.L)THEN
!   IF(ITER.EQ.100)PAUSE 'TOO MANY ITERATIONS IN TQLI'
!   ITER=ITER+1
!   G=(D(L+1)-D(L))/(2.*E(L))
!   R=PYTHAG(G,1.)
!   G=D(M)-D(L)+E(L)/(G+SIGN(R,G))
!   S=1.
!   C=1.
!   P=0.
!   DO I=M-1,L,-1
!    F=S*E(I)
!    B=C*E(I)
!    R=PYTHAG(F,G)
!    E(I+1)=R
!    IF(R.EQ.0.)THEN
!     D(I+1)=D(I+1)-P
!     E(M)=0.
!     GOTO 1
!    ENDIF
!    S=F/R
!    C=G/R
!    G=D(I+1)-P
!    R=(D(I)-G)*S+2.*C*B
!    P=S*R
!    D(I+1)=G+P
!    G=C*R-B
!    DO K=1,N
!     F=A(K,I+1)
!     A(K,I+1)=S*A(K,I)+C*F
!     A(K,I)=C*A(K,I)-S*F
!    END DO
!   END DO
!   D(L)=D(L)-P
!   E(L)=G
!   E(M)=0.
!   GOTO 1
!  ENDIF
! END DO
!
! END SUBROUTINE RED1TQLI
!
! !###========================================================================
! SUBROUTINE RED1TQLI_DBL(D,E,N,NP,A)
! !###========================================================================
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: N,NP
! DOUBLE PRECISION,DIMENSION(NP),INTENT(INOUT) :: D,E
! DOUBLE PRECISION,DIMENSION(NP,NP),INTENT(INOUT) :: A
! INTEGER :: I,ITER,K,L,M
! DOUBLE PRECISION :: B,C,DD,F,G,P,R,S
!
! DO I=2,N
!  E(I-1)=E(I)
! ENDDO
! E(N)=0.
! DO L=1,N
!  ITER=0
!1  DO M=L,N-1
!   DD=ABS(D(M))+ABS(D(M+1))
!   IF(ABS(E(M))+DD.EQ.DD)GOTO 2
!  END DO
!  M=N
!2  IF(M.NE.L)THEN
!   IF(ITER.EQ.100)PAUSE 'TOO MANY ITERATIONS IN TQLI'
!   ITER=ITER+1
!   G=(D(L+1)-D(L))/(2.*E(L))
!   R=PYTHAG_DBL(G,1.0D0)
!   G=D(M)-D(L)+E(L)/(G+SIGN(R,G))
!   S=1.
!   C=1.
!   P=0.
!   DO I=M-1,L,-1
!    F=S*E(I)
!    B=C*E(I)
!    R=PYTHAG_DBL(F,G)
!    E(I+1)=R
!    IF(R.EQ.0.)THEN
!     D(I+1)=D(I+1)-P
!     E(M)=0.
!     GOTO 1
!    ENDIF
!    S=F/R
!    C=G/R
!    G=D(I+1)-P
!    R=(D(I)-G)*S+2.*C*B
!    P=S*R
!    D(I+1)=G+P
!    G=C*R-B
!    DO K=1,N
!     F=A(K,I+1)
!     A(K,I+1)=S*A(K,I)+C*F
!     A(K,I)=C*A(K,I)-S*F
!    END DO
!   END DO
!   D(L)=D(L)-P
!   E(L)=G
!   E(M)=0.
!   GOTO 1
!  ENDIF
! END DO
!
! END SUBROUTINE RED1TQLI_DBL
 
END MODULE MOD_PEST
