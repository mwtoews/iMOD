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
USE PESTVAR
USE IMOD_UTL, ONLY : IMOD_UTL_PRINTTEXT,IMOD_UTL_CAPF,IMOD_UTL_ITOS,IMOD_UTL_GETREAL,IMOD_UTL_DBL_ITOS, &
 IMOD_UTL_FILENAME,IMOD_UTL_DTOS,IMOD_UTL_STRING,IMOD_UTL_OPENASC,IMOD_UTL_SWAPSLASH,&
 IMOD_UTL_GETUNIT,IMOD_UTL_CREATEDIR,IMOD_UTL_GETRCL,IMOD_UTL_LUDECOMP_DBL,IMOD_UTL_LUBACKSUB_DBL
USE MOD_RF2MF, ONLY: CMOD, PPST, SIMBOX, SIMCSIZE, IURUN 
USE TSVAR, ONLY : TS,CIPFTYPE,ROOTRES, IIPF, FWIIPF
USE IDFMODULE

CHARACTER(LEN=256),PRIVATE :: LINE
REAL(KIND=8),PARAMETER :: RCH_IMPULSE_SIZE=0.01D0    !## impulse of 0.01 m/day = 10.00 mm/day
REAL(KIND=8),PARAMETER :: RRECHSTRUCNIV   =0.00075D0 !## structural recharge of   0.75 mm/day
CHARACTER(LEN=15),PARAMETER :: CVERSION='mf2005'
CHARACTER(LEN=2100),PRIVATE :: BLINE
CHARACTER(LEN=52),PRIVATE :: SLINE
REAL(KIND=8),PRIVATE :: DAMPINGFACTOR=1.5D0

CONTAINS

 !###====================================================================
 SUBROUTINE PEST1INIT(ioption,infile,IOUT,root,pdir,idf,nparam)
 !###====================================================================
 USE RF2MF_MODULE, ONLY: NCOL, NROW, NLAY, NPER
 IMPLICIT NONE
 type(idfobj),intent(inout) :: idf
 character(len=*),intent(in) :: infile,root,pdir
 character(len=52),dimension(2) :: ipffname
 integer,intent(in) :: ioption,iout
 integer,intent(in) :: nparam
 integer,dimension(:),allocatable :: idipf
 INTEGER :: IOS,I,J,JJ,N,IZ,IROW,ICOL,JU,NIPF,MIPF,K,JS,NLINES,NUZONE,IP,ND,IUDUMP
 REAL(KIND=DP_KIND) :: NODATA,TF,F
 REAL(KIND=SP_KIND),ALLOCATABLE,DIMENSION(:,:) :: X
 LOGICAL :: LOP,LEX
 CHARACTER(LEN=52) :: CL
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: NLOCS
 INTEGER,ALLOCATABLE,DIMENSION(:) :: NUZ,ILOCS
 LOGICAL :: LCOMPUTE
 
! if(present(nparam))then
!  nlines=nparam
! endif
 
 IF(ioption.eq.1)THEN
 
  if(len_trim(infile).eq.0)stop 'No ipest file given'

  !## open file
  IURUN=IMOD_UTL_GETUNIT()
  OPEN(IURUN,FILE=infile,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
  IF(IOS.NE.0)THEN
   WRITE(*,'(A)') 'Can not find ipest file: ['//TRIM(infile)//']'
   PAUSE; STOP
  ENDIF

  !## skip comment
  DO
   READ(IURUN,'(A52)') CL
   IF(CL(1:1).NE.'#')EXIT
  ENDDO
  
  !## read steady/transient
  READ(CL,*) IDFM%NCOL,IDFM%NROW,NLAY,NPER
  !## read dimensions
  READ(IURUN,*) IDFM%XMIN,IDFM%YMIN,IDFM%XMAX,IDFM%YMAX,IDFM%IEQ
  !## cellsize
  if(idfm%ieq.EQ.0)then
   READ(IURUN,*) IDFM%DX
   IDFM%DY=IDFM%DX
  else
   allocate(idfm%sx(0:idfm%ncol))
   Read(IURUN,*) (IDFM%SX(ICOL),ICOL=0,IDFM%NCOL)
   allocate(idfm%sy(0:idfm%nrow))
   Read(IURUN,*) (IDFM%SY(IROW),IROW=0,IDFM%NROW)
  endif
    
  READ(IURUN,'(A256)') LINE
  READ(LINE,*,IOSTAT=IOS) IIPF,FWIIPF
  IF(IOS.NE.0)THEN
   FWIIPF=1.0; READ(LINE,*) IIPF
  ENDIF
  
  IF(IIPF.NE.0)THEN
   IF(ALLOCATED(TS))DEALLOCATE(TS)
   ALLOCATE(TS(ABS(IIPF)))
   DO JJ=1,ABS(IIPF)
    READ(IURUN,'(A256)',IOSTAT=IOS) LINE
    IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('ERROR DataSet 3 (see manual):'//TRIM(LINE),2)
    !## swap / -> \ in case of Linux (temporary)
    CALL IMOD_UTL_STRING(LINE); JS=OS; OS=1; CALL IMOD_UTL_SWAPSLASH(LINE)
    READ(LINE,*,IOSTAT=IOS) TS(JJ)%IPFNAME,TS(JJ)%IPFTYPE,TS(JJ)%IXCOL,TS(JJ)%IYCOL,TS(JJ)%ILCOL,TS(JJ)%IMCOL,TS(JJ)%IVCOL,TS(JJ)%IDCOL
    IF(IOS.NE.0)THEN
     TS(JJ)%IDCOL=0
     READ(LINE,*,IOSTAT=IOS) TS(JJ)%IPFNAME,TS(JJ)%IPFTYPE,TS(JJ)%IXCOL,TS(JJ)%IYCOL,TS(JJ)%ILCOL,TS(JJ)%IMCOL,TS(JJ)%IVCOL
     IF(IOS.NE.0)THEN
      TS(JJ)%IXCOL=1; TS(JJ)%IYCOL=2; TS(JJ)%ILCOL=3; TS(JJ)%IMCOL=0; TS(JJ)%IVCOL=0
      READ(LINE,*,IOSTAT=IOS) TS(JJ)%IPFNAME,TS(JJ)%IPFTYPE
     ENDIF
     IF(IOS.NE.0)THEN
      IF(NPER.EQ.1)TS(JJ)%IPFTYPE=1; IF(NPER.GT.1)TS(JJ)%IPFTYPE=2
      READ(LINE,*,IOSTAT=IOS) TS(JJ)%IPFNAME
     ENDIF
    ENDIF
    !## swap back again
    CALL IMOD_UTL_SWAPSLASH(TS(JJ)%IPFNAME)
    IF(NPER.EQ.1.AND. TS(JJ)%IPFTYPE.GE.2)CALL IMOD_UTL_PRINTTEXT('For steady-state simulation IPFTYPE(.)=1',2)
    IF(NPER.GT.1.AND.(TS(JJ)%IPFTYPE.LT.2))CALL IMOD_UTL_PRINTTEXT('for transient simulations IPFTYPE(.)=2',2)
    INQUIRE(FILE=TS(JJ)%IPFNAME,EXIST=LEX)
    CALL IMOD_UTL_PRINTTEXT('  - '//TRIM(TS(JJ)%IPFNAME(INDEX(TS(JJ)%IPFNAME,'\',.TRUE.)+1:)),0)
    IF(.NOT.LEX)CALL IMOD_UTL_PRINTTEXT('IPF-file does not exist',2)
   ENDDO
  ENDIF

  READ(IURUN,*) NLINES
  IF(NLINES.EQ.0)THEN; CLOSE(IURUN); RETURN; ENDIF
  
 else
 
  IF(IIPF.EQ.0)CALL IMOD_UTL_PRINTTEXT('You should specify IIPF>0 in combination with PST module',2)

! if(present(nparam))then
  nlines=nparam
! endif

 ENDIF
 
 ncol=idfm%ncol
 nrow=idfm%nrow

 !## next pest run ... skip part of runfile with pest information
 IF(ALLOCATED(PARAM))RETURN

 CALL PESTOPENFILE(IUPESTOUT        ,'log_pest_'            ,'txt',0,root) 
 CALL PESTOPENFILE(IUPESTPROGRESS   ,'log_pest_progress_'   ,'txt',0,root) 
 CALL PESTOPENFILE(IUPESTEFFICIENCY ,'log_pest_efficiency_' ,'txt',0,root)
 CALL PESTOPENFILE(IUPESTSENSITIVITY,'log_pest_sensitivity_','txt',0,root) 
 CALL PESTOPENFILE(IUPESTRUNFILE    ,'log_pest_runfile_'    ,'txt',0,root) 

 IUPESTRESIDUAL=0

 WRITE(IUPESTEFFICIENCY,'(6A15)') 'TJ','SQRT(TJ)','MEAN(TJ)','MEAN(SQRT(TJ))','ADJUSTMENTS','EFFICIENCY'
 WRITE(IUPESTEFFICIENCY,'(6A15)') '(L2)','(L)','MEAN(L2)','MEAN(L)','(%)','-'
 WRITE(IUPESTSENSITIVITY,'(A)')   'Sensitivity (%):'
 WRITE(IUPESTRUNFILE,'(A)')       'To be copied in the runfile:'

 WRITE(IUPESTOUT,'(A)') 'PEST-LOG'
 DO I=1,ABS(IIPF)
  WRITE(IUPESTOUT,'(/A)') ' Reading:  '//TRIM(TS(I)%IPFNAME)
  LINE=' IPF-type: ('//TRIM(IMOD_UTL_ITOS(TS(I)%IPFTYPE))//') '//TRIM(CIPFTYPE(TS(I)%IPFTYPE))
  WRITE(IUPESTOUT,'(A/)') TRIM(LINE)
 ENDDO

 !## check dupicate filenames - which is not allowed as all timeseries will be saved in a single folder.
 ALLOCATE(IDIPF(ABS(IIPF)))
 DO I=1,ABS(IIPF)
  IPFFNAME(1)=TS(I)%IPFNAME(INDEX(TS(I)%IPFNAME,'\',.TRUE.):)
  N=0; DO J=1,ABS(IIPF)
   IPFFNAME(2)=TS(J)%IPFNAME(INDEX(TS(J)%IPFNAME,'\',.TRUE.):)
   IF(TRIM(IPFFNAME(1)).EQ.TRIM(IPFFNAME(2)))THEN; N=N+1; IDIPF(N)=I; ENDIF
  ENDDO
  IF(N.GT.1)THEN
   WRITE(*,'(/A)') 'Duplicate filename found, this is not allowed:'
   DO J=1,N; WRITE(*,'(A)') TRIM(TS(IDIPF(J))%IPFNAME); ENDDO
   STOP
  ENDIF
 ENDDO
 DEALLOCATE(IDIPF)
 
 ALLOCATE(PARAM(NLINES))
 DO I=1,SIZE(PARAM); PARAM(I)%NODES=0; ENDDO
 DO I=1,SIZE(PARAM)
  NULLIFY(PARAM(I)%X,PARAM(I)%IROW,PARAM(I)%ICOL,PARAM(I)%ALPHA_HISTORY,PARAM(I)%ALPHA_ERROR_VARIANCE)
 ENDDO

 PEST_IREGULARISATION=0

 READ(IURUN,'(A256)',IOSTAT=IOS) LINE

 READ(LINE,*,IOSTAT=IOS) PEST_NITER,PEST_JSTOP,PEST_SENSITIVITY, &
  PEST_NPERIOD,PEST_NBATCH,(PEST_ITARGET(I),I=1,SIZE(PEST_ITARGET)),PEST_ISCALING,PEST_PADJ,PEST_DRES,PEST_KTYPE,PEST_KRANGE
 IF(IOS.NE.0)THEN
  PEST_KRANGE=0.0 !# apply default as 1.5 times the number of points in selected set
  IF(IOS.EQ.0)READ(LINE,*,IOSTAT=IOS) PEST_NITER,PEST_JSTOP,PEST_SENSITIVITY, &
    PEST_NPERIOD,PEST_NBATCH,(PEST_ITARGET(I),I=1,SIZE(PEST_ITARGET)),PEST_ISCALING,PEST_PADJ,PEST_DRES,PEST_KTYPE
  IF(IOS.NE.0)THEN
   !## simple kriging
   PEST_KTYPE=1
   IF(IOS.EQ.0)READ(LINE,*,IOSTAT=IOS) PEST_NITER,PEST_JSTOP,PEST_SENSITIVITY, &
     PEST_NPERIOD,PEST_NBATCH,(PEST_ITARGET(I),I=1,SIZE(PEST_ITARGET)),PEST_ISCALING,PEST_PADJ,PEST_DRES
   IF(IOS.NE.0)THEN
    PEST_DRES=0.0
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
   ENDIF
  ENDIF
 ENDIF
 IF(IOS.NE.0)THEN
  CALL IMOD_UTL_PRINTTEXT('Error reading NITER,JSTOP,SENSITIVITY,NPERIOD,NBATCH,ITARGET[.]:',0)
  CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),0)
  CALL IMOD_UTL_PRINTTEXT('Busy processing module: '//TRIM(CMOD(PPST)),2)
 ENDIF
 IF(PEST_MAXPNT.GT.1.AND.PEST_KRANGE.GT.0.0)THEN
  CALL IMOD_UTL_PRINTTEXT('',0)
  CALL IMOD_UTL_PRINTTEXT('=======================================================================',0)
  CALL IMOD_UTL_PRINTTEXT('NOTE: It is adviced to have the RANGE for the KRIGING to be 0.0 and let',0)
  CALL IMOD_UTL_PRINTTEXT('      iMODFLOW determing it automatically',0)
  CALL IMOD_UTL_PRINTTEXT('=======================================================================',0)
  CALL IMOD_UTL_PRINTTEXT('',0)
 ENDIF
 
 !## number of pilotpoint to be used in interpolation
 PEST_MAXPNT=10
 !## use nearest-neighbour, set maxpnt=1
 IF(ABS(PEST_KTYPE).EQ.3)PEST_MAXPNT=1

 !## include blank out array icm pilotpoitns
 PPBLANKOUT=''; NULLIFY(BLNKOUT%X)
 if(ioption.eq.0)then
  CALL IDFCOPY(IDF,BLNKOUT); ALLOCATE(BLNKOUT%X(IDF%NCOL,IDF%NROW))
 else
  CALL IDFCOPY(IDFM,BLNKOUT); ALLOCATE(BLNKOUT%X(NCOL,NROW))
 endif
 IF(PEST_KTYPE.LT.0)THEN

  IF(IOPTION.EQ.0)THEN
  
   READ(IURUN,'(A256)',IOSTAT=IOS) PPBLANKOUT
   CALL IMOD_UTL_FILENAME(PPBLANKOUT)
   CALL IMOD_UTL_PRINTTEXT('Assigned '//TRIM(PPBLANKOUT),0)
   IF (.NOT.IDFREAD(IDFC,PPBLANKOUT,0)) CALL IMOD_UTL_PRINTTEXT('IDFREAD',2)
   IF(ASSOCIATED(IDFM%X))DEALLOCATE(IDFM%X)
   NODATA = IDFC%NODATA
   IDFM%NODATA = NODATA
   IF (.NOT.IDFREADSCALE(IDFC,IDFM,10,0)) CALL IMOD_UTL_PRINTTEXT('IDFREADSCALE',2)

   BLNKOUT%X=IDFM%X
   CALL IDFDEALLOCATEX(IDFC)
   IF (IDFC%IU.GT.0) THEN
    INQUIRE(UNIT=IDFC%IU,OPENED=LOP); IF(LOP)CLOSE(IDFC%IU)
   ENDIF

  ELSEIF(IOPTION.EQ.1)THEN

   ALLOCATE(X(BLNKOUT%NCOL,BLNKOUT%NROW))
   CALL U2DREL(X, 'blnkout', NROW, NCOL, 0, IURUN, IOUT)
   BLNKOUT%X=REAL(X,8)
   DEALLOCATE(X)

  ENDIF
 ELSE
  !## default value 
  BLNKOUT%X=1.0D0
 ENDIF

 PEST_KTYPE=ABS(PEST_KTYPE)
 !## ordinary kriging - exponential instead of spherical
 IF(PEST_KTYPE.EQ.2)PEST_KTYPE=-3 !2
 !## simple kriging
 IF(PEST_KTYPE.EQ.1)PEST_KTYPE= 3 !2
 !## terminate after a single run
 PEST_SINGLE=0; IF(PEST_NITER.LT.0)THEN; PEST_NITER=1; PEST_SINGLE=1; ENDIF
 
 !## overrule entered minimal sensitivy in case of sensitivy-analyses
 IF(PEST_NITER.EQ.0)PEST_SENSITIVITY=0.0D0
 
 CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT(' Pest-Settings',-1)
 CALL IMOD_UTL_PRINTTEXT(' - Internal Multiplication Factor for Weights: '//TRIM(IMOD_UTL_DTOS(FWIIPF,'G',5)),-1,IUPESTOUT)
 CALL IMOD_UTL_PRINTTEXT(' - Number of Pest Iterations: '//TRIM(ITOS(PEST_NITER)),-1,IUPESTOUT)
 IF(PEST_NITER.EQ.0)CALL IMOD_UTL_PRINTTEXT(' - Sensitivity Analyses Started',-1,IUPESTOUT)
 CALL IMOD_UTL_PRINTTEXT(' - Stop Criterium Objective Function Value: '//TRIM(IMOD_UTL_DTOS(PEST_JSTOP,'F',3)),-1,IUPESTOUT)
 CALL IMOD_UTL_PRINTTEXT(' - Sensitivity to Exclude Parameter (temporarily): '//TRIM(IMOD_UTL_DTOS(PEST_SENSITIVITY,'F',3)),-1,IUPESTOUT)
 SELECT CASE (PEST_ISCALING)
  CASE (0)
   CALL IMOD_UTL_PRINTTEXT(' - No  Scaling, No  SVD',-1,IUPESTOUT)
  CASE (1)
   CALL IMOD_UTL_PRINTTEXT(' - Yes Scaling, No  SVD',-1,IUPESTOUT)
  CASE (2)
   CALL IMOD_UTL_PRINTTEXT(' - Yes Scaling, Yes SVD',-1,IUPESTOUT)
  CASE (3)
   CALL IMOD_UTL_PRINTTEXT(' - No  Scaling, Yes SVD',-1,IUPESTOUT)
 END SELECT
 SELECT CASE (PEST_KTYPE)
  CASE ( 3) !2)
   CALL IMOD_UTL_PRINTTEXT(' - Simple Kriging using Exponential Semivariogram is used (if neccessary)',-1,IUPESTOUT)
  CASE (-3) !2)
   CALL IMOD_UTL_PRINTTEXT(' - Ordinary Kriging using Exponential Semivariogram is used (if neccessary)',-1,IUPESTOUT)
  CASE DEFAULT
   CALL IMOD_UTL_PRINTTEXT(' Select 1 or 2 for Kriging Type',2)
 END SELECT
 CALL IMOD_UTL_PRINTTEXT(' - Termination Criterion for Parameter Adjustments (vectorlength): '//TRIM(IMOD_UTL_DTOS(PEST_PADJ,'F',3)),-1,IUPESTOUT)
  
 DO I=1,SIZE(PEST_ITARGET)
  IF(PEST_ITARGET(I).LT.0.0)CALL IMOD_UTL_PRINTTEXT('Error PEST_ITARGET('//TRIM(ITOS(I))//') < 0.0',2)
 ENDDO
 
 TF=SUM(PEST_ITARGET)

 CALL IMOD_UTL_PRINTTEXT(' - Number of BatchFiles Used: '//TRIM(ITOS(PEST_NBATCH)),-1,IUPESTOUT)
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
   IF(PEST_IBATCH(I)%FRACTION.LT.0.0)CALL IMOD_UTL_PRINTTEXT('Error PEST_IBATCH('//TRIM(ITOS(I))//')%FRACTION < 0.0',2)
   TF=TF+PEST_IBATCH(I)%FRACTION
  ENDDO
 ENDIF

 TF=1.0/TF
 PEST_ITARGET=TF*PEST_ITARGET
 DO I=1,PEST_NBATCH; PEST_IBATCH(I)%FRACTION=TF*PEST_IBATCH(I)%FRACTION; ENDDO

 !## write fractions
 CALL IMOD_UTL_PRINTTEXT(' - Fraction Heads: '//TRIM(IMOD_UTL_DTOS(PEST_ITARGET(1),'F',3)),-1,IUPESTOUT)
 CALL IMOD_UTL_PRINTTEXT(' - Fraction DynHd: '//TRIM(IMOD_UTL_DTOS(PEST_ITARGET(2),'F',3)),-1,IUPESTOUT)
 DO I=1,PEST_NBATCH
  CALL IMOD_UTL_PRINTTEXT(' - Fraction Batch '//TRIM(IMOD_UTL_ITOS(I))//': '//TRIM(IMOD_UTL_DTOS(PEST_IBATCH(I)%FRACTION,'F',3)),-1,IUPESTOUT)
  CALL IMOD_UTL_PRINTTEXT(' - BatchFile      '//TRIM(IMOD_UTL_ITOS(I))//': '//TRIM(PEST_IBATCH(I)%BATCHFILE),-1,IUPESTOUT)
  CALL IMOD_UTL_PRINTTEXT(' - BatchOutFile   '//TRIM(IMOD_UTL_ITOS(I))//': '//TRIM(PEST_IBATCH(I)%OUTFILE),-1,IUPESTOUT)
 ENDDO

 CALL IMOD_UTL_PRINTTEXT(' - Number of Periods Used: '//TRIM(IMOD_UTL_ITOS(PEST_NPERIOD)),-1,IUPESTOUT)
 IF(PEST_NPERIOD.GT.0)THEN
  ALLOCATE(PEST_IPERIOD(PEST_NPERIOD,2))
  DO I=1,PEST_NPERIOD
   READ(IURUN,'(A256)',IOSTAT=IOS) LINE
   IF(IOS.EQ.0)READ(LINE,*,IOSTAT=IOS) PEST_IPERIOD(I,1),PEST_IPERIOD(I,2)
   IF(IOS.NE.0)THEN
    CALL IMOD_UTL_PRINTTEXT('Error reading PERIOD1,PERIOD2: '//TRIM(LINE),0)
    CALL IMOD_UTL_PRINTTEXT('Busy processing module: '//TRIM(CMOD(PPST)),2)
   ENDIF
   CALL IMOD_UTL_PRINTTEXT(' - Period '//TRIM(IMOD_UTL_ITOS(I))//': '//TRIM(IMOD_UTL_DBL_ITOS(PEST_IPERIOD(I,1)))// &
     '-'//TRIM(IMOD_UTL_DBL_ITOS(PEST_IPERIOD(I,2))),-1,IUPESTOUT)
  ENDDO
 ENDIF
 CALL IMOD_UTL_PRINTTEXT('',0)

 DO I=1,SIZE(PARAM)
  
  PARAM(I)%ACRONYM=''

  READ(IURUN,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.EQ.0)THEN
   READ(LINE,*,IOSTAT=IOS) PARAM(I)%IACT, &  !## iact
        PARAM(I)%PTYPE, &  !## ptype
        PARAM(I)%ILS, &    !## ilayer/system
        PARAM(I)%IZONE, &  !## zone number
        PARAM(I)%INI, &    !## initial value
        PARAM(I)%DELTA, &  !## finite difference step
        PARAM(I)%MIN, &    !## minimal value
        PARAM(I)%MAX,&     !## maximal value
        PARAM(I)%FADJ,&    !## maximal adjust factor
        PARAM(I)%IGROUP, & !## group number
        J, &
        PARAM(I)%ACRONYM   !## acronym
   IF(IOS.EQ.0)THEN
    PARAM(I)%ACRONYM=ADJUSTR(PARAM(I)%ACRONYM)
    IF(J.EQ.0)PARAM(I)%LOG=.FALSE.
    IF(J.EQ.1)PARAM(I)%LOG=.TRUE.
   ENDIF
  ENDIF
  IF(IOS.NE.0)THEN
   READ(LINE,*,IOSTAT=IOS) PARAM(I)%IACT, &  !## iact
        PARAM(I)%PTYPE, & !## ptype
        PARAM(I)%ILS, &   !## ilayer/system
        PARAM(I)%IZONE, & !## zone number
        PARAM(I)%INI, &   !## initial value
        PARAM(I)%DELTA, & !## finite difference step
        PARAM(I)%MIN, &   !## minimal value
        PARAM(I)%MAX,&    !## maximal value
        PARAM(I)%FADJ,&   !## maximal adjust factor
        PARAM(I)%IGROUP,& !## group number
        J
   IF(IOS.EQ.0)THEN
    IF(J.EQ.0)PARAM(I)%LOG=.FALSE.
    IF(J.EQ.1)PARAM(I)%LOG=.TRUE.
   ELSE
    !## overrule default settings for log normal
    SELECT CASE (PARAM(I)%PTYPE)
     CASE ('KD','KH','KV','VC','SC','RC','RI','IC','II','DC','AF','MS','MC','VA','HF','EX','EP','QR','MQ','GC','SY')
      PARAM(I)%LOG=.TRUE.
     CASE DEFAULT
      PARAM(I)%LOG=.FALSE.    
    END SELECT
   ENDIF
  ENDIF
  
  IF(IOS.NE.0)THEN
   CALL IMOD_UTL_PRINTTEXT('Missing parameter in: PTYPE,ILS,IZONE,INI,DELTA,MIN,MAX,FADJ',0)
   CALL IMOD_UTL_PRINTTEXT(' reading: '//TRIM(LINE),0)
   CALL IMOD_UTL_PRINTTEXT('Busy processing module: PST',2)
  ENDIF
 
  !## read external filename for external parameters
  IF(PARAM(I)%PTYPE.EQ.'EX')THEN
   READ(IURUN,'(A256)',IOSTAT=IOS) LINE
   READ(LINE,'(A256)',IOSTAT=IOS) PARAM(I)%EXBATFILE
  ENDIF
  !## maximal adjust factor
  IF(PARAM(I)%FADJ.LE.1.0D0)THEN
   CALL IMOD_UTL_PRINTTEXT('Maximal Adjustment Factor should be at least greater than 1.0',0)
   CALL IMOD_UTL_PRINTTEXT('Parameter '//TRIM(ITOS(I))//' has a value of '//TRIM(IMOD_UTL_DTOS(PARAM(I)%FADJ,'F',3)),2)
  ENDIF
  PARAM(I)%IACT=MIN(1,MAX(0,PARAM(I)%IACT))
  ALLOCATE(PARAM(I)%ALPHA_HISTORY(0:PEST_NITER))
  ALLOCATE(PARAM(I)%ALPHA_ERROR_VARIANCE(0:PEST_NITER))
  PARAM(I)%ALPHA_HISTORY(0)=PARAM(I)%INI
  PARAM(I)%ALPHA_ERROR_VARIANCE=0.0D0
  PARAM(I)%PTYPE=IMOD_UTL_CAPF(PARAM(I)%PTYPE,'U')
 ENDDO

 READ(IURUN,'(A256)',IOSTAT=IOS) LINE
 IF(IOS.EQ.0)READ(LINE,*,IOSTAT=IOS) N
 IF(IOS.NE.0)THEN
  CALL IMOD_UTL_PRINTTEXT('Error reading NZONE'//TRIM(LINE),0)
  CALL IMOD_UTL_PRINTTEXT('Busy processing module: '//TRIM(CMOD(PPST)),2)
 ENDIF
 
 ALLOCATE(ZONE(N))

 IF(IOPTION.EQ.0)THEN
  
  NCOL=IDF%NCOL
  NROW=IDF%NROW
  
  DO I=1,SIZE(ZONE)

   NULLIFY(ZONE(I)%X,ZONE(I)%XY,ZONE(I)%IZ) 
   READ(IURUN,'(A)',IOSTAT=IOS) LINE
   IF(IOS.NE.0)THEN
    CALL IMOD_UTL_PRINTTEXT('Error reading '//TRIM(LINE),0)
    CALL IMOD_UTL_PRINTTEXT('Busy processing module: '//TRIM(CMOD(I)),2)
   ENDIF

   IZ=INT(IMOD_UTL_GETREAL(LINE,IOS))
   IF(IOS.EQ.0)THEN
    CALL IMOD_UTL_PRINTTEXT('Read Constant Value '//TRIM(IMOD_UTL_ITOS(IZ)),0)
    ALLOCATE(ZONE(I)%X(NCOL,NROW))
    ZONE(I)%ZTYPE=0
    ZONE(I)%X=REAL(IZ,8) 
   ELSE
    CALL IMOD_UTL_FILENAME(LINE)
    CALL IMOD_UTL_PRINTTEXT('Assigned '//TRIM(LINE),0)
    IF(INDEX(IMOD_UTL_CAPF(LINE,'U'),'.IDF').GT.0)THEN
     IF (.NOT.IDFREAD(IDFC,LINE,0)) CALL IMOD_UTL_PRINTTEXT('IDFREAD',2)
     CALL IDFNULLIFY(IDFM)
     CALL IDFCOPY(IDF,IDFM)
     IF(ASSOCIATED(IDFM%X))DEALLOCATE(IDFM%X)
     NODATA = IDFC%NODATA
     IDFM%NODATA = NODATA
     !## special zone scaling
     IF (.NOT.IDFREADSCALE(IDFC,IDFM,10,0)) CALL IMOD_UTL_PRINTTEXT('IDFREADSCALE',2)
         
     ALLOCATE(ZONE(I)%X(NCOL,NROW))
     ZONE(I)%ZTYPE=0
     ZONE(I)%X=IDFM%X
     CALL IDFDEALLOCATEX(IDFC)
     IF (IDFC%IU.GT.0) THEN
      INQUIRE(UNIT=IDFC%IU,OPENED=LOP); IF(LOP)CLOSE(IDFC%IU)
     ENDIF

    ELSEIF(INDEX(IMOD_UTL_CAPF(LINE,'U'),'.IPF').GT.0)THEN

     ZONE(I)%ZTYPE=1
     !## read in ipf
     JU=GETUNIT(); CALL IMOD_UTL_OPENASC(JU,LINE,'R')
     READ(JU,*) NIPF; READ(JU,*) MIPF; DO K=1,MIPF+1; READ(JU,*); ENDDO
     ALLOCATE(ZONE(I)%XY(NIPF,2),ZONE(I)%IZ(NIPF))  
     DO K=1,NIPF; READ(JU,*) ZONE(I)%XY(K,1),ZONE(I)%XY(K,2),ZONE(I)%IZ(K); ENDDO  
     CLOSE(JU)

    ELSE 

     CALL IMOD_UTL_PRINTTEXT('No supported file format found',2)

    ENDIF
   ENDIF
  ENDDO
 
 else
  
  NCOL=IDFM%NCOL
  NROW=IDFM%NROW
  
  DO I=1,SIZE(ZONE)

   NULLIFY(ZONE(I)%X,ZONE(I)%XY,ZONE(I)%IZ) 

   !## check whether IPF or IDF (arr-file)
   READ(IURUN,'(A256)') LINE
   
   IF(INDEX(IMOD_UTL_CAPF(LINE,'U'),'.IPF',.TRUE.).GT.0)THEN
   
    ZONE(I)%ZTYPE=1
    !## read in ipf
    JU=GETUNIT(); CALL IMOD_UTL_OPENASC(JU,LINE,'R')
    READ(JU,*) NIPF; READ(JU,*) MIPF; DO K=1,MIPF+1; READ(JU,*); ENDDO
    ALLOCATE(ZONE(I)%XY(NIPF,2),ZONE(I)%IZ(NIPF))  
    DO K=1,NIPF; READ(JU,*) ZONE(I)%XY(K,1),ZONE(I)%XY(K,2),ZONE(I)%IZ(K); ENDDO  
    CLOSE(JU)

   ELSE
   
    !## use u2drel
    ALLOCATE(ZONE(I)%X(NCOL,NROW)); ZONE(I)%ZTYPE=0  
    BACKSPACE(IURUN)
    CALL U2DREL(ZONE(I)%X, 'zone', NROW, NCOL, 0, IURUN, IOUT)

   ENDIF
  ENDDO

  CLOSE(IURUN)
  
 endif
  
 !## compute zone distribution
 INQUIRE(FILE=TRIM(pdir)//'\PARAM_DUMP_IPEST.DAT',EXIST=LCOMPUTE)
 IF(LCOMPUTE)THEN
  WRITE(*,'(/A)') 'Read zones allocated to parameters from the found file:'
  WRITE(*,'(A/)') TRIM(pdir)//'\PARAM_DUMP_IPEST.DAT'
 ENDIF
 
 CALL IMOD_UTL_PRINTTEXT('Parameters',-1,iu=IUPESTOUT)
 WRITE(LINE,'(A2,1X,A5,2(1X,A5),5(1X,A15),3A10,A15)') 'AC','PTYPE','ILS','IZN','INITIAL','DELTA','MINIMUM','MAXIMUM','FADJ','IGROUP','LTRANS','NODES','ACRONYM'
 CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),-1,iu=IUPESTOUT)

 IF(.NOT.LCOMPUTE)THEN
 
  !## get number of unique zones
  ALLOCATE(NUZ(SIZE(PARAM))); NUZ=0
  DO I=1,SIZE(PARAM); NUZ(I)=PARAM(I)%IZONE; ENDDO
  CALL UTL_GETUNIQUE_INT(NUZ,SIZE(PARAM),NUZONE,0)
 
  WRITE(*,'(A)') 'Found '//TRIM(IMOD_UTL_ITOS(NUZONE))//' unique zones, getting number of location per zone ...'
 
  ALLOCATE(ILOCS(NUZ(NUZONE))); ILOCS=0
  ALLOCATE(NLOCS(NUZONE,SIZE(ZONE))); NLOCS=0
  !## set reference to zones
  DO I=1,NUZONE; ILOCS(NUZ(I))=I; ENDDO

  !## see how many locations per unique zone
  DO IROW=1,NROW; DO ICOL=1,NCOL; DO J=1,SIZE(ZONE) 
   !## zones 
   IF(ASSOCIATED(ZONE(J)%X))THEN
    IZ=INT(ZONE(J)%X(ICOL,IROW))
    IF(IZ.LE.0)CYCLE
    !## zone in files but not used by current set of parameters
    IF(IZ.GT.SIZE(ILOCS))CYCLE
    IP=ILOCS(IZ)
    IF(IP.GT.0)THEN
     NLOCS(IP,J)=NLOCS(IP,J)+1
    ENDIF
   !## pilot points
   ELSE
    DO K=1,SIZE(ZONE(J)%IZ)
     IZ=ZONE(J)%IZ(K)
     IP=0; IF(IZ.GT.0.AND.IZ.LE.SIZE(ILOCS))IP=ILOCS(IZ) 
     IF(IP.GT.0)NLOCS(IP,J)=1
    ENDDO
   ENDIF
  ENDDO; ENDDO; ENDDO
  
  !## check number of zones and missing zone (if any)
  DO I=1,SIZE(PARAM)
 
   !## parameter active and main of group
   IZ=PARAM(I)%IZONE
   IP=ILOCS(IZ)
   ND=0; DO J=1,SIZE(ZONE)
    ND=ND+NLOCS(IP,J)
    IF(NLOCS(IP,J).GT.0)PARAM(I)%ZTYPE=ZONE(J)%ZTYPE
   ENDDO

   PARAM(I)%NODES=ND 

   !## not applicable for pilotpoints
   IF(PARAM(I)%ZTYPE.EQ.1)THEN
    SELECT CASE (TRIM(PARAM(I)%PTYPE))
     CASE ('KD','KH','KV','VC','SC','VA','RE','SY')
     CASE DEFAULT
      CALL IMOD_UTL_PRINTTEXT('Cannot use PilotPoints for other than KD,KH,KV,VC,SC,SY and VA',2)
     END SELECT
   ENDIF
 
   IF(PARAM(I)%PTYPE.EQ.'HF')THEN
    PARAM(I)%NODES=0 !## one single cell used as zone for horizontal barrier module
    WRITE(LINE,'(I2,1X,A5,2(1X,I5),5(1X,F15.7),I10,L10,A10,A15)') PARAM(I)%IACT,PARAM(I)%PTYPE,PARAM(I)%ILS, &
      PARAM(I)%IZONE,PARAM(I)%INI,PARAM(I)%DELTA,PARAM(I)%MIN,PARAM(I)%MAX,PARAM(I)%FADJ,PARAM(I)%IGROUP,PARAM(I)%LOG,'EntireLine',PARAM(I)%ACRONYM
   ELSE
    IF(PARAM(I)%NODES.EQ.0)THEN
     CALL IMOD_UTL_PRINTTEXT('No area/zone assigned to parameter no. '//TRIM(ITOS(I))//' ptype= '//TRIM(PARAM(I)%PTYPE),0)
     PARAM(I)%IACT=0
    ENDIF
    WRITE(LINE,'(I2,1X,A5,2(1X,I5),5(1X,F15.7),I10,L10,I10,A15)') PARAM(I)%IACT,PARAM(I)%PTYPE,PARAM(I)%ILS, &
      PARAM(I)%IZONE,PARAM(I)%INI,PARAM(I)%DELTA,PARAM(I)%MIN,PARAM(I)%MAX,PARAM(I)%FADJ,PARAM(I)%IGROUP,PARAM(I)%LOG,PARAM(I)%NODES,PARAM(I)%ACRONYM
   ENDIF

   CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),-1,IU=IUPESTOUT)
   IF(PARAM(I)%PTYPE.EQ.'EX')CALL IMOD_UTL_PRINTTEXT(TRIM(PARAM(I)%EXBATFILE),-1)
   CALL PEST1CHK(I)
   PARAM(I)%ALPHA(1)=PARAM(I)%INI !## current  alpha
   PARAM(I)%ALPHA(2)=PARAM(I)%INI !## previous alpha

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

  !## only in active-optimization mode
  IF(PEST_SINGLE.EQ.0)THEN
   IF(N.EQ.0)CALL IMOD_UTL_PRINTTEXT('Nothing to do',2)
  ENDIF
 
  !## fill array zone and set appropriate pointers in type 
  DO I=1,SIZE(PARAM)
   IF(PARAM(I)%NODES.GT.0)THEN
    CALL IMOD_UTL_PRINTTEXT('Parameter '//TRIM(ITOS(I))//' number of locations '//TRIM(ITOS(PARAM(I)%NODES))//' assigned to ptype= '//TRIM(PARAM(I)%PTYPE),0)

    !## get number of zone in list of unique zone numbers
    IZ=PARAM(I)%IZONE
    IP=ILOCS(IZ)

    IF(PARAM(I)%ZTYPE.EQ.0)THEN
    
     ALLOCATE(PARAM(I)%IROW(PARAM(I)%NODES),PARAM(I)%ICOL(PARAM(I)%NODES))
     ALLOCATE(PARAM(I)%F(PARAM(I)%NODES))

     !## loop to see zones
     N=0; DO J=1,SIZE(ZONE)

      !## particular zone not in this file
      IF(NLOCS(IP,J).EQ.0)CYCLE

      DO IROW=1,NROW; DO ICOL=1,NCOL
       IF(ZONE(J)%ZTYPE.EQ.0)THEN
        IF(PARAM(I)%IZONE.EQ.INT(ZONE(J)%X(ICOL,IROW)))THEN

         SELECT CASE (TRIM(PARAM(I)%PTYPE))
          CASE ('KD','KH','KV','VC','SC','AF','AA','MS','MC','VA','HF','EX','SY')
           F=MOD(ZONE(J)%X(ICOL,IROW),1.0); IF(F.EQ.0.0)F=1.0
          CASE DEFAULT
           F=1.0
         END SELECT
 
         N=N+1; PARAM(I)%IROW(N)=INT(IROW,2); PARAM(I)%ICOL(N)=INT(ICOL,2); PARAM(I)%F(N)=F
        ENDIF
       ENDIF
      ENDDO; ENDDO
    
      IF(N.NE.PARAM(I)%NODES)THEN
       WRITE(*,'(/A,I10,A,I10)') 'SOMETHING GOES WRONG NUMBER OF PARAMETER INITIAL ARE ',PARAM(I)%NODES
       WRITE(*,'(A,I10/)') 'PARAMETERS ACTUALLY FOUND ARE ',N
      ENDIF

     ENDDO

    ELSEIF(PARAM(I)%ZTYPE.EQ.1)THEN
   
     ALLOCATE(PARAM(I)%XY(PARAM(I)%NODES,2))

     !## check pilotpoints
     N=0; DO J=1,SIZE(ZONE)

      !## particular zone not in this file
      IF(NLOCS(IP,J).EQ.0)CYCLE

      IF(ZONE(J)%ZTYPE.EQ.1)THEN
       DO K=1,SIZE(ZONE(J)%IZ)
        !## check whether it's integer value is equal to param(i)%izone
        IF(PARAM(I)%IZONE.EQ.INT(ZONE(J)%IZ(K)))THEN
         N=N+1; PARAM(I)%XY(N,1)=ZONE(J)%XY(K,1); PARAM(I)%XY(N,2)=ZONE(J)%XY(K,2) 
        ENDIF
       ENDDO
      ENDIF

     ENDDO
   
    ENDIF
   
   ELSE
    IF(PARAM(I)%PTYPE.NE.'HF')PARAM(I)%IACT=0
   ENDIF 
  ENDDO

  !!## set igroup lt 0 for followers in group - check whether factors within group are equal --- need to be
  !!## make sure group with active nodes is positive rest is negative
  !DO I=1,SIZE(PARAM)
  ! IF(PARAM(I)%IACT.EQ.0)THEN
  !  CYCLE
  ! ENDIF
  ! DO J=1,I-1 
  !  IF(PARAM(J)%IACT.EQ.0)CYCLE
  !  IF(PARAM(J)%IGROUP.EQ.PARAM(I)%IGROUP)THEN
  !   !## check factor
  !   IF(PARAM(J)%INI.NE.PARAM(I)%INI)THEN
  !    CALL IMOD_UTL_PRINTTEXT('Initial factor in an group need to be identicial',0)
  !    CALL IMOD_UTL_PRINTTEXT('Check initial factors for group '//TRIM(IMOD_UTL_ITOS(PARAM(J)%IGROUP)),2)
  !   ENDIF
  !   PARAM(I)%IGROUP=-1*PARAM(I)%IGROUP; EXIT
  !  ENDIF
  ! ENDDO
  !ENDDO

  DO I=1,SIZE(ZONE)
   IF(ZONE(I)%ZTYPE.EQ.0)THEN
    DEALLOCATE(ZONE(I)%X)
   ELSEIF(ZONE(I)%ZTYPE.EQ.1)THEN
    DEALLOCATE(ZONE(I)%XY,ZONE(I)%IZ)
   ENDIF
  ENDDO

  DEALLOCATE(ZONE,NLOCS,NUZ,ILOCS)
  !## dump everything
  IUDUMP=IMOD_UTL_GETUNIT()
  OPEN(IUDUMP,FILE=TRIM(pdir)//'\PARAM_DUMP_IPEST.DAT',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED',IOSTAT=IOS)
  IF(IOS.NE.0)THEN
   WRITE(*,'(/A)') 'Cannot save the dumpfile for iPESTP in the following folder:'
   WRITE(*,'(A/)') '['//TRIM(pdir)//'\PARAM_DUMP_IPEST.DAT]'
   STOP
  ENDIF
  DO I=1,SIZE(PARAM)
   WRITE(IUDUMP,*) PARAM(I)%NODES,PARAM(I)%ZTYPE
   DO J=1,PARAM(I)%NODES
    WRITE(IUDUMP,*) PARAM(I)%IROW(J),PARAM(I)%ICOL(J),PARAM(I)%F(J)
   ENDDO
  ENDDO
  CLOSE(IUDUMP)  
 !## read from dumpfile
 ELSE
 
  IUDUMP=IMOD_UTL_GETUNIT()
  OPEN(IUDUMP,FILE=TRIM(pdir)//'\PARAM_DUMP_IPEST.DAT',STATUS='OLD',ACTION='READ',FORM='FORMATTED')
  DO I=1,SIZE(PARAM)
   READ(IUDUMP,*) PARAM(I)%NODES,PARAM(I)%ZTYPE
   ALLOCATE(PARAM(I)%IROW(PARAM(I)%NODES),PARAM(I)%ICOL(PARAM(I)%NODES),PARAM(I)%F(PARAM(I)%NODES))
   DO J=1,PARAM(I)%NODES
    READ(IUDUMP,*) PARAM(I)%IROW(J),PARAM(I)%ICOL(J),PARAM(I)%F(J)
   ENDDO
   IF(PARAM(I)%PTYPE.EQ.'HF')THEN
    PARAM(I)%NODES=0 !## one single cell used as zone for horizontal barrier module
    WRITE(LINE,'(I2,1X,A5,2(1X,I5),5(1X,F15.7),I10,L10,A10,A15)') PARAM(I)%IACT,PARAM(I)%PTYPE,PARAM(I)%ILS, &
      PARAM(I)%IZONE,PARAM(I)%INI,PARAM(I)%DELTA,PARAM(I)%MIN,PARAM(I)%MAX,PARAM(I)%FADJ,PARAM(I)%IGROUP,PARAM(I)%LOG,'EntireLine',PARAM(I)%ACRONYM
   ELSE
    IF(PARAM(I)%NODES.EQ.0)THEN
     CALL IMOD_UTL_PRINTTEXT('No area/zone assigned to parameter no. '//TRIM(ITOS(I))//' ptype= '//TRIM(PARAM(I)%PTYPE),0)
     PARAM(I)%IACT=0
    ENDIF
    WRITE(LINE,'(I2,1X,A5,2(1X,I5),5(1X,F15.7),I10,L10,I10,A15)') PARAM(I)%IACT,PARAM(I)%PTYPE,PARAM(I)%ILS, &
      PARAM(I)%IZONE,PARAM(I)%INI,PARAM(I)%DELTA,PARAM(I)%MIN,PARAM(I)%MAX,PARAM(I)%FADJ,PARAM(I)%IGROUP,PARAM(I)%LOG,PARAM(I)%NODES,PARAM(I)%ACRONYM
   ENDIF

   CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),-1,IU=IUPESTOUT)
   IF(PARAM(I)%PTYPE.EQ.'EX')CALL IMOD_UTL_PRINTTEXT(TRIM(PARAM(I)%EXBATFILE),-1)
   CALL PEST1CHK(I)
   PARAM(I)%ALPHA(1)=PARAM(I)%INI !## current  alpha
   PARAM(I)%ALPHA(2)=PARAM(I)%INI !## previous alpha

  ENDDO
  CLOSE(IUDUMP)  
  
 ENDIF
 
 !## set igroup lt 0 for followers in group - check whether factors within group are equal --- need to be
 !## make sure group with active nodes is positive rest is negative
 DO I=1,SIZE(PARAM)
  IF(PARAM(I)%IACT.EQ.0)THEN
!   WRITE(*,*) I,PARAM(I)%IACT,PARAM(I)%IGROUP
   CYCLE
  ENDIF
  DO J=1,I-1 
   IF(PARAM(J)%IACT.EQ.0)CYCLE
   IF(PARAM(J)%IGROUP.EQ.PARAM(I)%IGROUP)THEN
    !## check factor
    IF(PARAM(J)%INI.NE.PARAM(I)%INI)THEN
     CALL IMOD_UTL_PRINTTEXT('Initial factor in an group need to be identicial',0)
     CALL IMOD_UTL_PRINTTEXT('Check initial factors for group '//TRIM(IMOD_UTL_ITOS(PARAM(J)%IGROUP)),2)
    ENDIF
    PARAM(I)%IGROUP=-1*ABS(PARAM(I)%IGROUP); EXIT
   ENDIF
  ENDDO
!  WRITE(*,*) I,PARAM(I)%IACT,PARAM(I)%IGROUP
!  PAUSE
 ENDDO
 
 !## initialize process-flags
 LGRAD       =.TRUE.  !## gradient computation
 PEST_IGRAD  = 0      !## current simulation is default
 LLNSRCH     =.FALSE. !## no line search

 !## sensitivity - if pest_niter=0
 LSENS=.FALSE.; IF(PEST_NITER.EQ.0)LSENS=.TRUE. 

 WRITE(BLINE,'(3A5,A15)') 'IT','GD','LS','TOT_J'
 DO J=1,SIZE(PARAM)
  IF(ABS(PARAM(J)%IACT).EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
   IF(TRIM(PARAM(J)%ACRONYM).EQ.'')THEN
    WRITE(SLINE,'(3X,A2,2I5.5)') PARAM(J)%PTYPE,PARAM(J)%ILS,PARAM(J)%IZONE
   ELSE
    WRITE(SLINE,'(A15)') PARAM(J)%ACRONYM
   ENDIF
   BLINE=TRIM(BLINE)//TRIM(SLINE)
  ENDIF
 ENDDO

 WRITE(IUPESTSENSITIVITY,'(A10,A)') 'Iteration',TRIM(BLINE(31:)) 
 WRITE(IUPESTPROGRESS,'(A)') 

 if(ioption.eq.1)call idfcopy(idfm,idf)
 
 !## close all files ...
 CALL PEST1CLOSELOGFILES()

 END SUBROUTINE PEST1INIT

 !###====================================================================
 SUBROUTINE PEST1ALPHA_METASWAP(nrow,ncol,lpwt)
 !###====================================================================
 IMPLICIT NONE
 integer, intent(in) :: nrow, ncol
 logical, intent(in) :: lpwt
 INTEGER :: IUSCL,IOS,I,J,IROW,ICOL,NUND,N
 REAL(KIND=8) :: FCT,THETA,PRZ,COND,PWT
 TYPE SCLOBJ1
  INTEGER :: NUND,IROW,ICOL
 END TYPE SCLOBJ1
 TYPE SCLOBJ2
  REAL(KIND=8) :: THETA,COND,PRZ,PWT
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
  LINE=' * '//PARAM(I)%PTYPE//' adjusted ('//TRIM(IMOD_UTL_ITOS(PARAM(I)%NODES))//') with alpha='//TRIM(IMOD_UTL_DTOS(FCT,'F',7))
  CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),-1,IUPESTOUT)
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
  REAL(KIND=8) :: D,N,E
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

 LINE=' * adjusted mete_svat.inp with impulse of '//TRIM(IMOD_UTL_DTOS(RCH_IMPULSE_SIZE,'G',4))
 CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),-1,IUPESTOUT) !; CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),1)

 END SUBROUTINE PEST1_METEO_METASWAP

 !#####=================================================================
 LOGICAL FUNCTION PESTRUNEXT()
 !#####=================================================================
 IMPLICIT NONE
 INTEGER :: I
 LOGICAL :: LEX
 REAL(KIND=8) :: FCT
 CHARACTER(LEN=256) :: LINE
  
 PESTRUNEXT=.TRUE.
 IF(PEST_ITER.EQ.0)RETURN
 
 DO I=1,SIZE(PARAM)
  SELECT CASE (TRIM(PARAM(I)%PTYPE))
   CASE ('EX','NX')
    INQUIRE(FILE=PARAM(I)%EXBATFILE,EXIST=LEX)
    IF(.NOT.LEX)THEN
     CALL IMOD_UTL_PRINTTEXT('Batchfile does not exists',0)
     CALL IMOD_UTL_PRINTTEXT(LINE,2)
    ENDIF
    FCT=PARAM(I)%ALPHA(1)
    IF(PARAM(I)%LOG)FCT=EXP(FCT)
    LINE=TRIM(PARAM(I)%EXBATFILE)//','//TRIM(IMOD_UTL_DTOS(FCT,'F',5))
    CALL SYSTEM(TRIM(LINE),2)
  END SELECT
 ENDDO
 
 END FUNCTION PESTRUNEXT
 
 !#####=================================================================
 LOGICAL FUNCTION PESTNEXT(LSS,root,IDF)
 !#####=================================================================
 use rf2mf_module, only: nper
 IMPLICIT NONE
 type(idfobj),intent(in) :: idf
 logical,intent(in) :: LSS
 character(len=*),intent(in) :: root
 REAL(KIND=8) :: IMPROVEMENT,F,GUPDATE
 INTEGER :: I,J,ILOG,ISTOP

 IF(.NOT.ALLOCATED(PARAM))STOP

 PESTNEXT=.FALSE.

 call pest1appendlogfile(root)

 IF(PEST_IGRAD.EQ.0)CALL PESTOPENFILE(IUPESTRESIDUAL,'log_pest_residual_'//TRIM(IMOD_UTL_ITOS(PEST_ITER)),'txt',0,root)

 IF(PEST_ITER.EQ.0)PEST_ITER=1

 !## compute objective function
 CALL PEST_GETJ(LSS,ROOT,idf)

 IF(PEST_SINGLE.EQ.1)STOP
 
 IF(LSENS)THEN
!  !## next parameter combination
!  IF(.NOT.PESTNEXTSENS())STOP
!  IF(.NOT.PESTNEXTGRAD())STOP
  IF(.NOT.PESTNEXTGRAD())CALL PESTGRADIENT(root,idf)
 ELSEIF(LGRAD)THEN
  !## what proces is going on?
  IF(.NOT.PESTNEXTGRAD())THEN
   !## get gradient
   CALL PESTGRADIENT(ROOT,idf)
   CALL PEST_ECHOPARAMETERS(GUPDATE)
   LLNSRCH=.TRUE.; PEST_ILNSRCH=1; LGRAD=.FALSE.; PEST_IGRAD=0
  ENDIF
 ELSEIF(LLNSRCH)THEN
  !## no reduction of objection function, change (u(i))
  IF(TJ.GT.TJOBJ)THEN
!   DAMPINGFACTOR=DAMPINGFACTOR*NDAMPING
!   NDAMPING=1.0 !## do it onces only
   IF(.NOT.PESTUPGRADEVECTOR(0.5D0,.FALSE.))THEN
    STOP 'ERROR PESTUPGRADEVECTOR IN LINESEARCH'
   ENDIF !# half of current search-gradient
!   CALL PEST_ECHOPARAMETERS(GUPDATE)
   !## start next line-search
   PEST_ILNSRCH=PEST_ILNSRCH+1
  ELSE
!   DAMPINGFACTOR=DAMPINGFACTOR/NDAMPING
!   NDAMPING=ININDAMPING !## do it onces only

   CALL PEST_ECHOPARAMETERS(GUPDATE)

   !## update alpha for parameters in same group
   DO I=1,SIZE(PARAM)
    !## skip inactive parameters
    IF(PARAM(I)%IACT.EQ.0)CYCLE
    IF(PARAM(I)%IGROUP.GT.0)THEN
     DO J=1,SIZE(PARAM)
      IF(PARAM(I)%IGROUP.EQ.ABS(PARAM(J)%IGROUP))PARAM(J)%ALPHA(1)=PARAM(I)%ALPHA(1)
     ENDDO
    ENDIF
   ENDDO
   
   IMPROVEMENT=0; DO I=1,SIZE(PARAM)
    IF(PARAM(I)%IACT.EQ.0)CYCLE
    IF(PARAM(I)%IGROUP.LT.0)CYCLE
    IF(PARAM(I)%LOG)THEN
     F=(EXP(PARAM(I)%ALPHA(1))/EXP(PARAM(I)%ALPHA(2)))*100.0D0
     F=ABS(F-100.0D0)
     IMPROVEMENT=IMPROVEMENT+F
    ELSE
     F=(PARAM(I)%ALPHA(1)/PARAM(I)%ALPHA(2))*100.0D0
     F=ABS(F-100.0D0)
     IMPROVEMENT=IMPROVEMENT+F
    ENDIF 
   ENDDO

   WRITE(IUPESTEFFICIENCY,'(6E15.7)') TJ,SQRT(TJ),TJ/REAL(PEST_NOBS),REAL(SQRT(TJ))/REAL(PEST_NOBS),IMPROVEMENT,(TJOBJ/TJ) 

   WRITE(IUPESTRUNFILE,'(/A,I10/)') 'Copy in the runfile, iteration ',PEST_ITER
   DO I=1,SIZE(PARAM)
    ILOG=0; IF(PARAM(I)%LOG)ILOG=1
    IF(PARAM(I)%LOG)THEN
     WRITE(IUPESTRUNFILE,'(I2,1X,A,1X,2(I4,1X),5(F10.3,1X),I4,1X,I2,1X,A)') ABS(PARAM(I)%IACT), &  !## iact
         PARAM(I)%PTYPE, &         !## ptype
         PARAM(I)%ILS, &           !## ilayer/system
         PARAM(I)%IZONE, &         !## zone number
         EXP(PARAM(I)%ALPHA(1)), & !## initial value
         EXP(PARAM(I)%DELTA), &    !## finite difference step
         EXP(PARAM(I)%MIN), &      !## minimal value
         EXP(PARAM(I)%MAX),&       !## maximal value
         PARAM(I)%FADJ,&           !## maximal adjust factor
         ABS(PARAM(I)%IGROUP),&    !## group number
         ILOG,&                    !## log transformed
         TRIM(PARAM(I)%ACRONYM)
    ELSE
     WRITE(IUPESTRUNFILE,'(I2,1X,A,1X,2(I4,1X),5(F10.3,1X),I4,1X,I2,1X,A)') ABS(PARAM(I)%IACT), &  !## iact
         PARAM(I)%PTYPE, & !## ptype
         PARAM(I)%ILS, &   !## ilayer/system
         PARAM(I)%IZONE, & !## zone number
         PARAM(I)%ALPHA(1), &   !## initial value
         PARAM(I)%DELTA, & !## finite difference step
         PARAM(I)%MIN, &   !## minimal value
         PARAM(I)%MAX,&    !## maximal value
         PARAM(I)%FADJ,&   !## maximal adjust factor
         ABS(PARAM(I)%IGROUP),& !## group number
         ILOG, &            !## log transformed
         TRIM(PARAM(I)%ACRONYM)
    ENDIF 
   ENDDO

   !## length of gradient update vector
   IF(GUPDATE.LT.PEST_PADJ)THEN
    CALL IMOD_UTL_PRINTTEXT('Process stopped, less than '//TRIM(IMOD_UTL_DTOS(PEST_PADJ,'F',3))//' of vector length',-1,IUPESTOUT); STOP
   ENDIF

   !## continue ?
   IF(PEST_ITER+1.GT.PEST_NITER)THEN
    PESTNEXT=.TRUE.  !## max. number of iterations reached
    CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT); CALL IMOD_UTL_PRINTTEXT('Pest iteration terminated: PEST_ITER (='// &
        TRIM(IMOD_UTL_ITOS(PEST_ITER))//') = PEST_NITER (='//TRIM(IMOD_UTL_ITOS(PEST_NITER))//')',-1,IUPESTOUT); STOP
   ENDIF
   IF(TJ.LE.0.0D0)THEN
    PESTNEXT=.TRUE.
    CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT); CALL IMOD_UTL_PRINTTEXT('Objective Function <= 0.0 ('// &
       TRIM(IMOD_UTL_DTOS(TJ,'G',7))//')',-1,IUPESTOUT); STOP
   ENDIF

   IF(IMPROVEMENT.LE.PEST_JSTOP)THEN
    PESTNEXT=.TRUE.  !## min. improvement reached
    CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT); CALL IMOD_UTL_PRINTTEXT('Pest iteration terminated decrease objective function ('// &
        TRIM(IMOD_UTL_DTOS(100.0D0*IMPROVEMENT,'G',7))//'%) > PEST_JSTOP ('//TRIM(IMOD_UTL_DTOS(100.0D0*PEST_JSTOP,'G',7))//'%)',-1,IUPESTOUT); STOP
   ENDIF

   TJOBJ=TJ
   !## replace old by new parameter values
   PARAM%ALPHA(2)=PARAM%ALPHA(1)

   !## next iteration
   PEST_ITER=PEST_ITER+1
   IF(.NOT.PESTNEXT)THEN
    CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT); CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT)
    CALL IMOD_UTL_PRINTTEXT(' *** Next Outer Iteration ***',-1,IUPESTOUT)
   ENDIF
   LLNSRCH=.FALSE.; LGRAD=.TRUE.; PEST_IGRAD=0; PEST_ILNSRCH=0
   IF(.NOT.PESTNEXTGRAD())THEN
   ENDIF
  ENDIF

 ENDIF
 
 CALL PEST1CLOSELOGFILES()

 END FUNCTION PESTNEXT

 !#####=================================================================
 SUBROUTINE PESTOPENFILE(IU,FNAME,EXT,INEW,root)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME,EXT,root
 INTEGER,INTENT(IN) :: INEW
 INTEGER,INTENT(OUT) :: IU
 INTEGER :: IOS
 LOGICAL :: LEX

 LINE=TRIM(ROOT)//CHAR(92)//'pest'//CHAR(92)//TRIM(FNAME)//TRIM(CVERSION)//'.'//TRIM(EXT)
 
 CALL IMOD_UTL_CREATEDIR(LINE(:INDEX(LINE,CHAR(92),.TRUE.)-1))
 CALL IMOD_UTL_FILENAME(LINE)
 IOS=-1; IU=IMOD_UTL_GETUNIT()
 IF(INEW.EQ.0)THEN
  OPEN(IU,FILE=LINE,STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
 ELSE
  INQUIRE(FILE=LINE,EXIST=LEX)
  IF(LEX)THEN
   OPEN(IU,FILE=LINE,STATUS='OLD',ACCESS='APPEND',IOSTAT=IOS)
  ELSE
   OPEN(IU,FILE=LINE,STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
  ENDIF
 ENDIF

 IF(IOS.NE.0)IU=0
 IF(IU.LE.0)CALL IMOD_UTL_PRINTTEXT('Cannot open PEST-progress file '//TRIM(LINE),2)
 
 END SUBROUTINE PESTOPENFILE

 !#####=================================================================
 SUBROUTINE PESTDUMPFCT(ROOT,IOUT,IDF)
 !#####=================================================================
 USE RF2MF_MODULE, ONLY: NCOL, NROW, NLAY, NPER
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 CHARACTER(LEN=256),INTENT(IN) :: ROOT
 INTEGER,INTENT(IN) :: IOUT
 CHARACTER(LEN=1024) :: FNAME,DIR
 INTEGER :: I,J,IROW,ICOL
 REAL(KIND=8) :: XVAR,XF
 REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: X
 LOGICAL :: LOK

 !## dump only at the beginning of each iteration cycle
 IF(PEST_IGRAD.GT.1)RETURN
 
 !## if too many parameters, skip the output
 IF(SIZE(PARAM).GT.100)RETURN
 
 !## open pest factor files
 DIR=TRIM(ROOT)//CHAR(92)//'PEST'//CHAR(92)//'FACTORS'//TRIM(ITOS(PEST_ITER))
 CALL IMOD_UTL_CREATEDIR(DIR)

 ALLOCATE(X(NCOL,NROW))
 DO I=1,SIZE(PARAM)
  
  !## do not save non-optimized parameters
  IF(PARAM(I)%IACT.EQ.0)CYCLE

  !## regular-grid
  IF(PARAM(I)%ZTYPE.EQ.0)THEN
   WRITE(FNAME,'(A,2I5.5,A)') TRIM(DIR)//CHAR(92)//PARAM(I)%PTYPE,PARAM(I)%ILS,PARAM(I)%IZONE,'.IDF'
   X=-999.0D0
   !## log transformed
   IF(PARAM(I)%LOG)THEN
    DO J=1,PARAM(I)%NODES
     IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
     X(ICOL,IROW)=EXP(PARAM(I)%ALPHA(1))
    ENDDO
   ELSE
    DO J=1,PARAM(I)%NODES
     IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
     X(ICOL,IROW)=PARAM(I)%ALPHA(1) 
    ENDDO
   ENDIF 
   !## divide parameter for area
   SELECT CASE (PARAM(I)%PTYPE)
    CASE ('SC','SY')
     DO J=1,PARAM(I)%NODES
      IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
      X(ICOL,IROW)=X(ICOL,IROW)/IDFGETAREA(IDF,ICOL,IROW)
     ENDDO   
    CASE ('RE')
     DO J=1,PARAM(I)%NODES
      IROW=PARAM(I)%IROW(J); ICOL=PARAM(I)%ICOL(J)
      X(ICOL,IROW)=X(ICOL,IROW)/IDFGETAREA(IDF,ICOL,IROW)*1000.0D0
     ENDDO   
   END SELECT
   CALL MET1WRTIDF(FNAME,X,NCOL,NROW,-999.0D0,IOUT)

  ELSE

   XVAR=PARAM(I)%ALPHA_ERROR_VARIANCE(PEST_ITER)
   IF(PARAM(I)%LOG)THEN
    XF  =EXP(PARAM(I)%ALPHA(1)) 
   ELSE
    XF  =PARAM(I)%ALPHA(1) 
   ENDIF
   !## no output of IPF-file- takes too long
!   WRITE(FNAME,'(A,2I5.5,A)') TRIM(DIR)//CHAR(92)//PARAM(I)%PTYPE,PARAM(I)%ILS,PARAM(I)%IZONE,'.IPF'
!   CALL WRITEIPF(SIZE(PARAM(I)%XY,1),SIZE(PARAM(I)%XY,2),PARAM(I)%XY,XF,XVAR,FNAME)  

  ENDIF
 ENDDO
 DEALLOCATE(X)

 END SUBROUTINE PESTDUMPFCT

 !###====================================================================
SUBROUTINE WRITEIPF(N,M,XY,F,BW,FNAME) 
!###====================================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: N,M
REAL(KIND=8),DIMENSION(N,M),INTENT(IN) :: XY
REAL(KIND=8),INTENT(IN) :: F,BW
CHARACTER(LEN=*),INTENT(IN) :: FNAME
INTEGER :: IU,I

IU=IMOD_UTL_GETUNIT()
OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',ACTION='WRITE')
WRITE(IU,'(A)') TRIM(IMOD_UTL_ITOS(N))
WRITE(IU,'(A)') '4'
WRITE(IU,'(A)') 'X'
WRITE(IU,'(A)') 'Y'
WRITE(IU,'(A)') 'F'
WRITE(IU,'(A)') 'ERROR_VARIANCE'
WRITE(IU,'(A)') '0,TXT'
DO I=1,SIZE(XY,1)
 WRITE(IU,'(4(F15.7,1X))') XY(I,1),XY(I,2),F,BW
ENDDO

CLOSE(IU)

END SUBROUTINE WRITEIPF

 !#####=================================================================
 SUBROUTINE PESTPROGRESS()
 !#####=================================================================
 IMPLICIT NONE
 REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: X
 INTEGER :: I,J
 LOGICAL :: LEX
 
 ALLOCATE(X(SIZE(PARAM)))

 DO I=1,SIZE(PARAM)
  IF(PARAM(I)%LOG)THEN
   X(I)=EXP(PARAM(I)%ALPHA(1))
  ELSE
   X(I)=PARAM(I)%ALPHA(1)
  ENDIF
 ENDDO

 IF(PEST_IGRAD.EQ.0)THEN
  WRITE(BLINE,'(3A5,A15)') 'IT','GD','LS','TOT_J'
  DO J=1,SIZE(PARAM)
   IF(ABS(PARAM(J)%IACT).EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
    WRITE(SLINE,'(3X,A2,2I5.5)') PARAM(J)%PTYPE,PARAM(J)%ILS,PARAM(J)%IZONE
    BLINE=TRIM(BLINE)//TRIM(SLINE)
   ENDIF
  ENDDO
  WRITE(IUPESTPROGRESS,'(/A)') TRIM(BLINE) 
  BLINE=''
  DO J=1,SIZE(PARAM)
   IF(ABS(PARAM(J)%IACT).EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
    WRITE(SLINE,'(12X,I3.3)') PARAM(J)%IGROUP
    BLINE=TRIM(BLINE)//TRIM(SLINE)
   ENDIF
  ENDDO
  WRITE(IUPESTPROGRESS,'(30X,A)') TRIM(BLINE) 
  BLINE=''
  DO J=1,SIZE(PARAM)
   IF(ABS(PARAM(J)%IACT).EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
    WRITE(SLINE,'(A15)') PARAM(J)%ACRONYM
    BLINE=TRIM(BLINE)//TRIM(SLINE)
   ENDIF
  ENDDO
  WRITE(IUPESTPROGRESS,'(30X,A)') TRIM(BLINE) 
 ENDIF
 
 WRITE(BLINE,'(3I5,E15.7)') PEST_ITER,PEST_IGRAD,PEST_ILNSRCH,TJ
 DO I=1,SIZE(PARAM)
  IF(ABS(PARAM(I)%IACT).EQ.1.AND.PARAM(I)%IGROUP.GT.0)THEN
   WRITE(SLINE,'(E15.7)') X(I)
   BLINE=TRIM(BLINE)//TRIM(SLINE)
  ENDIF
 ENDDO

 WRITE(IUPESTPROGRESS,'(A)') TRIM(BLINE) 
 DEALLOCATE(X)

 END SUBROUTINE PESTPROGRESS

 !#####=================================================================
 LOGICAL FUNCTION PESTWRITESTATISTICS_PERROR(NP,COV,LPRINT)
 !#####=================================================================
 IMPLICIT NONE
 REAL(KIND=8),PARAMETER :: XBANDW=5.0D0
 INTEGER,INTENT(IN) :: NP
 REAL(KIND=8),INTENT(IN),DIMENSION(NP,NP) :: COV
 LOGICAL,INTENT(IN) :: LPRINT
 INTEGER :: I,J,K,IP1,IERROR
 REAL(KIND=8) :: Z1,Z2,Z,ZW
 LOGICAL :: LLOG
 
 PESTWRITESTATISTICS_PERROR=.FALSE.
 
 !## The asymptotic standard parameter error is a measure of how unexplained variability in the
 !## data propagates to variability in the parameters, and is essentially an error measure for the
 !## parameters. The variance indicates the range over which a parameter value could extend without affecting model fit too adversely.

 IF(LPRINT)THEN 
  WRITE(IUPESTOUT,'(/A)') 'Parameter Variance - Standard Parameter Error (standard deviation)'
  WRITE(IUPESTOUT,'(A/)')  'Indicates the range over which a parameter value could extend without affecting model fit too much'

!  WRITE(BLINE,'(A15,99(A7,I3.3))') 'PARAMETER',('   ITER',I,I=PEST_ITER,1,-1)
!  WRITE(IUPESTOUT,'(A/)') TRIM(BLINE)
 ENDIF
 
 J=0; IERROR=0
 DO I=1,SIZE(PARAM)
  IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0)THEN
   J=J+1
   IF(COV(J,J).GT.0.0)THEN
    PARAM(I)%ALPHA_ERROR_VARIANCE(PEST_ITER)=SQRT(COV(J,J))
   ELSE
    !## error value - should not happen
    PARAM(I)%ALPHA_ERROR_VARIANCE(PEST_ITER)=-999.99D0 
    IERROR=IERROR+1
   ENDIF
   !## check whether current other parameters belong to this group
   DO IP1=1,SIZE(PARAM)
    !## active and follower of group
    IF(PARAM(IP1)%IACT.EQ.1.AND.PARAM(IP1)%IGROUP.LT.0)THEN
     IF(ABS(PARAM(IP1)%IGROUP).EQ.PARAM(I)%IGROUP)THEN
      PARAM(IP1)%ALPHA_ERROR_VARIANCE(PEST_ITER)=PARAM(I)%ALPHA_ERROR_VARIANCE(PEST_ITER)
     ENDIF
    ENDIF
   ENDDO
!   IF(LPRINT)THEN
!    WRITE(BLINE,'(99(F10.3))') (PARAM(I)%ALPHA_ERROR_VARIANCE(K),K=PEST_ITER,1,-1)
!    WRITE(IUPESTOUT,'(3X,A2,2I5.5,A1,I3.3,A)') PARAM(I)%PTYPE,PARAM(I)%ILS,PARAM(I)%IZONE,'-',ABS(PARAM(I)%IGROUP),TRIM(BLINE)
!   ENDIF
  ELSEIF(PARAM(I)%IGROUP.EQ.0)THEN
   PARAM(I)%ALPHA_ERROR_VARIANCE(PEST_ITER)=0.0D0
  ENDIF
 ENDDO
 
 IF(IERROR.GT.0)THEN
  WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'Errors ('//TRIM(IMOD_UTL_ITOS(IERROR))//') found in the Covariance Matrix:'; WRITE(IUPESTOUT,*)
  CALL IMOD_UTL_PRINTTEXT('Errors found in the computation of the Covariance Matrix',2)
 ENDIF
 
 IF(LPRINT)THEN; WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'Confidence Limits (96%):'; WRITE(IUPESTOUT,*); ENDIF

 DO I=1,SIZE(PARAM)
  IF(PARAM(I)%IGROUP.LT.0)CYCLE
  IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0)THEN
   ZW=PARAM(I)%ALPHA_ERROR_VARIANCE(PEST_ITER)*1.96D0
   IF(PARAM(I)%LOG)THEN
    Z =EXP(PARAM(I)%ALPHA(1)) 
    Z1=TINY(1.0)
    IF(PARAM(I)%ALPHA(1)-ZW.LT.LOG(HUGE(1.0)))THEN
     Z1=EXP(PARAM(I)%ALPHA(1)-ZW) 
    ENDIF
    Z2=HUGE(1.0)
    IF(PARAM(I)%ALPHA(1)+ZW.LT.LOG(HUGE(1.0)))THEN
     Z2=EXP(PARAM(I)%ALPHA(1)+ZW) 
    ENDIF
   ELSE
    Z= PARAM(I)%ALPHA(1) 
    Z1=PARAM(I)%ALPHA(1)-ZW 
    Z2=PARAM(I)%ALPHA(1)+ZW 
   ENDIF 

   IF(LPRINT)THEN
    WRITE(BLINE,'(3G15.7)') Z1,Z,Z2
    IF(PARAM(I)%ACRONYM.EQ.'')THEN
     WRITE(IUPESTOUT,'(A2,2I5.5,I3.3,A)') PARAM(I)%PTYPE,PARAM(I)%ILS,PARAM(I)%IZONE,ABS(PARAM(I)%IGROUP),TRIM(BLINE)
    ELSE
     WRITE(IUPESTOUT,'(A15,A)') PARAM(I)%ACRONYM,TRIM(BLINE)
    ENDIF
   ENDIF

   LLOG=.FALSE.
!   IF(Z1.NE.0.0.AND.Z2.NE.0.0)THEN
!    !## ignore parameter with too high of a band for unreliability, turn if off
!    LLOG=LOG10(Z2)-LOG10(Z1).GT.XBANDW
!   ELSE
!    LLOG=.TRUE.
!   ENDIF
   IF(LLOG)THEN
    WRITE(IUPESTOUT,'(/3X,A2,2I5.5,I3.3)') PARAM(I)%PTYPE,PARAM(I)%ILS,PARAM(I)%IZONE,ABS(PARAM(I)%IGROUP)
    WRITE(IUPESTOUT,'(A,2G15.7)') 'This parameter too unreliable to estimate: ',Z1,Z2
    WRITE(IUPESTOUT,'(A/)') 'Parameter will be turned off for this cycle'
    PARAM(I)%IACT=-1; RETURN
   ENDIF
   
  ELSE
   IF(LPRINT.AND.PARAM(I)%IACT.EQ.-1)THEN
    WRITE(BLINE,'(3A15)') 'Insens.','Insens.','Insens.'
    IF(PARAM(I)%ACRONYM.EQ.'')THEN
     WRITE(IUPESTOUT,'(A2,2I5.5,I3.3,A)') PARAM(I)%PTYPE,PARAM(I)%ILS,PARAM(I)%IZONE,ABS(PARAM(I)%IGROUP),TRIM(BLINE)
    ELSE
     WRITE(IUPESTOUT,'(A15,A)') PARAM(I)%ACRONYM,TRIM(BLINE)
    ENDIF
   ENDIF
  ENDIF
 ENDDO

 PESTWRITESTATISTICS_PERROR=.TRUE.
 
 END FUNCTION PESTWRITESTATISTICS_PERROR
  
! IF(MMOD(PPST).EQ.1.AND.LSENS)THEN
!  IF(PEST_IGRAD.EQ.0)THEN
!   FNAME=TRIM(FNAME)//'_sens_'//TRIM(ITOS(PEST_IGRAD)) 
!  ELSE
!   PNAME='_sens_'//TRIM(PARAM(PEST_IGRAD)%PTYPE)//'_igroup'//TRIM(ITOS(PARAM(PEST_IGRAD)%IGROUP))
!   FNAME=TRIM(FNAME)//TRIM(PNAME)
!  ENDIF
! ENDIF
! 
! !## compute sensitivities
! IF(LSAVED.AND.MMOD(PPST).EQ.1.AND.PEST_IGRAD.GT.0.AND.LSENS)THEN
!  !## read results under parameter adjustment (conditioned)
!  IF(.NOT.IDFREAD(H(1),FNAME,1))CALL PRINTTEXT('Can not open: '//TRIM(FNAME),2)
!  !## read unconditioned results
!  FNAME=IMOD_UTL_SUBST(FNAME,TRIM(PNAME),'_sens_'//TRIM(ITOS(0)))
!  IF(.NOT.IDFREAD(H(2),FNAME,1))CALL PRINTTEXT('Can not open: '//TRIM(FNAME),2)
!
!!  D=(PARAM(PEST_IGRAD)%ALPHA(2)+PARAM(PEST_IGRAD)%DELTA)-PARAM(PEST_IGRAD)%ALPHA(2)
!  D=PARAM(PEST_IGRAD)%DELTA
!
!  !## gradient - perturbation/zero
!  H(1)%X=(H(1)%X-H(2)%X)/D 
!
!  !## overwrite conditioned results as jacobian values
!  FNAME=IMOD_UTL_SUBST(FNAME,'_sens_'//TRIM(ITOS(0)),TRIM(PNAME))
!  IF(.NOT.IDFWRITE(H(1),FNAME,0))CALL PRINTTEXT('Can not write to: '//TRIM(FNAME),2)
!  CALL IDFDEALLOCATE(H,SIZE(H))
! ENDIF
 
 !!#####=================================================================
 !SUBROUTINE PESTWRITESTATISTICS_FOSM(NP,COV)
 !!#####=================================================================
 !USE VERSION, ONLY : CVERSION
 !USE IMOD_IDF
 !USE GLBVAR, ONLY : CDATE_SIM
 !IMPLICIT NONE
 !INTEGER,INTENT(IN) :: NP
 !DOUBLE PRECISION,INTENT(IN),DIMENSION(NP,NP) :: COV
 !INTEGER :: II,I,J,IPER,ILAY,IROW,ICOL
 !TYPE(IDFOBJ),DIMENSION(1) :: H
 !CHARACTER(LEN=256) :: FNAME
 !REAL,ALLOCATABLE,DIMENSION(:,:) :: JCBN
 !REAL,ALLOCATABLE,DIMENSION(:) :: PROW
 ! 
 !J=NP
 !ALLOCATE(JCBN(NCOL*NROW*NLAY,J),PROW(J))
 !
 !CALL CREATEDIR(TRIM(ROOTRES)//CHAR(92)//'uncertainty')
 !
 !DO IPER=1,NPER  
 !
 ! J=0
 ! DO I=1,SIZE(PARAM)
 !  IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0)THEN
 !   J=J+1
 !
 !   FNAME=TRIM(ROOTRES)//CHAR(92)//'head\head_'//TRIM(CDATE_SIM(IPER))//'_l*_sens_'// &
 !         TRIM(PARAM(I)%PTYPE)//'_igroup'//TRIM(ITOS(PARAM(I)%IGROUP))//'.idf'
 !   CALL IMOD_UTL_PRINTTEXT('Reading '//TRIM(FNAME),0) 
 !
 !   DO ILAY=1,NLAY
 !    
 !    FNAME=TRIM(ROOTRES)//CHAR(92)//'head\head_'//TRIM(CDATE_SIM(IPER))//'_l'//TRIM(ITOS(ILAY))//'_sens_'// &
 !          TRIM(PARAM(I)%PTYPE)//'_igroup'//TRIM(ITOS(PARAM(I)%IGROUP))//'.idf'
 ! 
 !    IF(.NOT.IDFREAD(H(1),FNAME,1))THEN
 !     CALL IMOD_UTL_PRINTTEXT('Can not open: '//TRIM(FNAME),0)
 !     H(1)%X=0.0      
 !    ENDIF 
 !
 !    II=(ILAY-1)*NCOL*NROW
 !    DO IROW=1,NROW; DO ICOL=1,NCOL
 !     II=II+1
 !     JCBN(II,J)=H(1)%X(ICOL,IROW)
 !    ENDDO; ENDDO
 !
 !   ENDDO
 !  ENDIF
 ! ENDDO
 ! 
 ! !## received the variance per location
 ! H(1)%X=0.0
 !
 ! !## compute jcbn*cov*jcbn, process per row
 ! IROW=1; ICOL=0; ILAY=1
 ! DO II=1,NODES
 !  ICOL=ICOL+1
 !  IF(ICOL.GT.NCOL)THEN
 !   IROW=IROW+1
 !   IF(IROW.GT.NROW)THEN
 !    FNAME=TRIM(ROOTRES)//CHAR(92)//'uncertainty\uncertainty_'//TRIM(CDATE_SIM(IPER))//'_l'//TRIM(ITOS(ILAY))//'.idf'
 !    CALL IMOD_UTL_PRINTTEXT('Writing '//TRIM(FNAME),0) 
 !    IF(.NOT.IDFWRITE(H(1),FNAME,0))CALL IMOD_UTL_PRINTTEXT('Can not write: '//TRIM(FNAME),2)
 !    IROW=1; ILAY=ILAY+1
 !   ENDIF
 !   ICOL=1
 !  ENDIF
 !  PROW=0.0;
 !  DO I=1,NP
 !   DO J=1,NP
 !    PROW(I)=PROW(I)+JCBN(II,I)*COV(I,J)
 !   ENDDO
 !  ENDDO
 !  H(1)%X(ICOL,IROW)=0.0
 !  DO I=1,NP
 !   H(1)%X(ICOL,IROW)=H(1)%X(ICOL,IROW)+PROW(I)*PROW(I)
 !  ENDDO
 !  H(1)%X(ICOL,IROW)=SQRT(H(1)%X(ICOL,IROW))
 ! 
 ! ENDDO
 !
 ! !## schrijf laatste modellayer
 ! FNAME=TRIM(ROOTRES)//CHAR(92)//'uncertainty\uncertainty_'//TRIM(CDATE_SIM(IPER))//'_l'//TRIM(ITOS(ILAY))//'.idf'
 ! CALL IMOD_UTL_PRINTTEXT('Writing '//TRIM(FNAME),0) 
 ! IF(.NOT.IDFWRITE(H(1),FNAME,0))CALL IMOD_UTL_PRINTTEXT('Can not write: '//TRIM(FNAME),2)
 ! 
 !ENDDO 
 !
 !CALL IDFDEALLOCATE(H,SIZE(H))
 !DEALLOCATE(JCBN)
 !
 !!## variance in head due to the variance (uncertainty) in the input variable
 !
 !CALL IMOD_UTL_PRINTTEXT('Sensitivity finished',2)
 !
 !END SUBROUTINE PESTWRITESTATISTICS_FOSM
 
 !#####=================================================================
 LOGICAL FUNCTION PESTNEXTGRAD()
 !#####=================================================================
 IMPLICIT NONE
 INTEGER :: I
 REAL(KIND=8) :: FCT
 
 PESTNEXTGRAD=.TRUE.

 DO
  PEST_IGRAD=PEST_IGRAD+1
  !## all gradients processed
  IF(PEST_IGRAD.GT.SIZE(PARAM))EXIT
  !## zero gradient in case parameter is fixed
  IF(PARAM(PEST_IGRAD)%IACT.EQ.0)THEN
   DO I=1,SIZE(MSR%DH,2); MSR%DH(PEST_IGRAD,I)=MSR%DH(0,I); ENDDO
  !## check whether the parameters has been modified allready since it belongs to the same group
  ELSEIF(PARAM(PEST_IGRAD)%IGROUP.LT.0)THEN
   DO I=1,SIZE(MSR%DH,2); MSR%DH(PEST_IGRAD,I)=MSR%DH(0,I); ENDDO
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
   IF(PARAM(I)%IACT.EQ.0)CYCLE
   IF(ABS(PARAM(I)%IGROUP).EQ.ABS(PARAM(PEST_IGRAD)%IGROUP))THEN
    IF(PARAM(I)%LOG)THEN
     PARAM(I)%ALPHA(1)=PARAM(I)%ALPHA(2)+PARAM(I)%DELTA
     FCT=EXP(PARAM(I)%ALPHA(1))
    ELSE
     PARAM(I)%ALPHA(1)=PARAM(I)%ALPHA(2)*PARAM(I)%DELTA
     FCT=PARAM(I)%ALPHA(1)
    ENDIF
    CALL IMOD_UTL_PRINTTEXT('Adjusting Parameter '//TRIM(PARAM(I)%PTYPE)// &
                 ';ils='//TRIM(IMOD_UTL_ITOS(PARAM(I)%ILS))// &
                 ';izone='//TRIM(IMOD_UTL_ITOS(PARAM(I)%IZONE))// &
                 ';igroup='//TRIM(IMOD_UTL_ITOS(PARAM(I)%IGROUP))// &
                 ';factor='//TRIM(IMOD_UTL_DTOS(FCT,'*',1)),0)
    WRITE(IUPESTOUT,'(A)') 'Adjusting Parameter '//TRIM(PARAM(I)%PTYPE)// &
                 '['//TRIM(PARAM(I)%ACRONYM)//']'// &
                 ';ils='//TRIM(IMOD_UTL_ITOS(PARAM(I)%ILS))// &
                 ';izone='//TRIM(IMOD_UTL_ITOS(PARAM(I)%IZONE))// &
                 ';igroup='//TRIM(IMOD_UTL_ITOS(PARAM(I)%IGROUP))// &
                 ';factor='//TRIM(IMOD_UTL_DTOS(FCT,'*',1))
   ENDIF
  ENDDO
 ELSE
  PESTNEXTGRAD=.FALSE.
 ENDIF

 END FUNCTION PESTNEXTGRAD

 !###====================================================================
 SUBROUTINE PESTGRADIENT(root,idf)
 !###====================================================================
 IMPLICIT NONE
 type(idfobj),intent(in) :: idf
 CHARACTER(LEN=*),INTENT(IN) :: ROOT
 REAL(KIND=8) :: DJ1,DJ2
 REAL(KIND=8) :: B1,TS,DF1,DF2,BETA,EIGWTHRESHOLD,W,DH1,DH2
 INTEGER :: I,II,J,K,L,NP,MP,IP1,IP2,NE,ISING,ITRIES,IBND
 INTEGER,ALLOCATABLE,DIMENSION(:) :: INDX,ICOR
 REAL(KIND=8) :: P1,P2,PMIN,PMAX
 REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: TDJ,C,JS,P,PT
 REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: GAMMA,N,RU 
 REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: S
 CHARACTER(LEN=8),ALLOCATABLE,DIMENSION(:) :: TXT
 REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: EIGV,COV,B,M
 REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: EIGW,JQR
 CHARACTER(LEN=8) :: DTXT
 LOGICAL :: LSCALING,LSVD,LAMBDARESET

 SELECT CASE (PEST_ISCALING)
  CASE (0); LSCALING=.FALSE.; LSVD=.FALSE.
  CASE (1); LSCALING=.TRUE.;  LSVD=.FALSE.
  CASE (2); LSCALING=.TRUE.;  LSVD=.TRUE.
  CASE (3); LSCALING=.FALSE.; LSVD=.TRUE.
 END SELECT

 NP=SIZE(PARAM)
 !## sensitivity
 IF(.NOT.ALLOCATED(S)) ALLOCATE(S (NP)); S =0.0

 DO IP1=1,NP
  IF(ABS(PARAM(IP1)%IACT).NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
  DF1=REAL(PARAM(IP1)%DELTA,8)
  DO J=1,PEST_NOBS
   W  =REAL(MSR%W(J),8)
   DH1=REAL(MSR%DH(IP1,J),8)
   DH2=REAL(MSR%DH(0,J),8)
   S(IP1)=S(IP1)+W*((DH1-DH2)/DF1)
  ENDDO
 ENDDO
 DO I=1,NP; S(I)=S(I)/REAL(PEST_NOBS,8); ENDDO

 WRITE(BLINE,'(A30)') '              Sensitivity (.):'
 DO I=1,NP
  IF(ABS(PARAM(I)%IACT).NE.1.OR.PARAM(I)%IGROUP.LE.0)CYCLE
  WRITE(SLINE,'(E15.7)') S(I)
  BLINE=TRIM(BLINE)//TRIM(SLINE)
 ENDDO
 WRITE(IUPESTPROGRESS,'(A)') TRIM(BLINE) 

 TS=SUM(ABS(S)); DO IP1=1,NP
  IF(ABS(PARAM(IP1)%IACT).NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
  IF(TS.NE.0.0D0)S(IP1)=S(IP1)/TS
 ENDDO
 S=ABS(S)*100.0D0

 WRITE(BLINE,'(A30)') '              Sensitivity (%):'
 DO I=1,NP
  IF(ABS(PARAM(I)%IACT).NE.1.OR.PARAM(I)%IGROUP.LE.0)CYCLE
  WRITE(SLINE,'(E15.7)') S(I)
  BLINE=TRIM(BLINE)//TRIM(SLINE)
 ENDDO
 WRITE(IUPESTPROGRESS,'(A)') TRIM(BLINE) 
 WRITE(IUPESTSENSITIVITY,'(I10,A)') PEST_ITER,TRIM(BLINE(31:))  

 !## reset parameters - alpha(2)=previous alpha
 DO I=1,SIZE(PARAM); PARAM(I)%ALPHA(1)=PARAM(I)%ALPHA(2); ENDDO

 !## melt frozen parameters
 DO IP1=1,SIZE(PARAM)
  !## current state of the boundaries
  CALL PEST_GETBOUNDARY(IP1,IBND,P1,P2,PMIN,PMAX)
  PARAM(IP1)%IBND=IBND
  IF(PARAM(IP1)%IACT.EQ.-1.AND.ABS(PARAM(IP1)%IGROUP).GT.0)THEN
   PARAM(IP1)%IACT=1
  ENDIF
 ENDDO

 !## freeze-insensitive parameters
 DO I=1,SIZE(PARAM)
  IF(PARAM(I)%IACT.EQ.1.AND.ABS(PARAM(I)%IGROUP).GT.0.AND.S(I).LT.PEST_SENSITIVITY)PARAM(I)%IACT=-1
 ENDDO

 NP=0; DO I=1,SIZE(PARAM); IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0)NP=NP+1; ENDDO
 IF(NP.EQ.0)CALL IMOD_UTL_PRINTTEXT('All parameters are insensitive, process stopped!',2)

 !## find until parameter update within hypersphere of parameters
 !## initiate marquardt as small as possible
 MARQUARDT=0.001D0
 ITRIES=0
 DO

  ITRIES=ITRIES+1
  
  !## allocate arrays for current selection  
  NP=0; DO I=1,SIZE(PARAM); IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0)NP=NP+1; ENDDO

  IF(NP.EQ.0)CALL IMOD_UTL_PRINTTEXT('No parameters left, process stopped!',2)
  
  IF(ALLOCATED(JQJ))DEALLOCATE(JQJ);   ALLOCATE(JQJ (NP,NP))
  IF(ALLOCATED(JQR))DEALLOCATE(JQR);   ALLOCATE(JQR (NP))
  IF(ALLOCATED(U  ))DEALLOCATE(U);     ALLOCATE(U   (NP))
  IF(ALLOCATED(EIGW))DEALLOCATE(EIGW); ALLOCATE(EIGW(NP))
  IF(ALLOCATED(EIGV))DEALLOCATE(EIGV); ALLOCATE(EIGV(NP,NP))
  IF(ALLOCATED(COV ))DEALLOCATE(COV);  ALLOCATE(COV (NP,NP))
  IF(LSCALING)THEN
   IF(ALLOCATED(C  ))DEALLOCATE(C);    ALLOCATE(C (NP,NP       ))
   IF(ALLOCATED(JS ))DEALLOCATE(JS);   ALLOCATE(JS(NP,PEST_NOBS))
  ENDIF

  !## construct jqj - normal matrix/hessian
  CALL PEST1JQJ(JQJ,EIGW,EIGV,COV,NP,LSENS,ROOT,idf)

  !## multiply lateral sensitivities with sensitivities in case pest_niter=0
!  IF(ITRIES.EQ.1)THEN
  IF(PEST_NITER.EQ.0)THEN
   !## print all first time
   IF(.NOT.PESTWRITESTATISTICS_PERROR(NP,COV,.TRUE.))CYCLE
  ELSE
   IF(.NOT.PESTWRITESTATISTICS_PERROR(NP,COV,.FALSE.))CYCLE  
  ENDIF

  !## construct jTqr (<--- r is residual for current parameter set)
  JQR=0.0; I=0
  DO IP1=1,SIZE(PARAM)  !## row
  
   IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
  
   DF1=REAL(PARAM(IP1)%DELTA,8)
  
   I=I+1
   DO J=1,PEST_NOBS
    DH1=REAL(MSR%DH(IP1,J),8)
    DH2=REAL(MSR%DH(0,J),8)
    DJ1=(DH1-DH2)/DF1
    DJ2=REAL(MSR%DH(0 ,J),8)
    W  =REAL(MSR%W(J),8)
    JQR(I)=JQR(I)+(DJ1*W*DJ2)
   ENDDO

  ENDDO
  
  IF(.NOT.LSCALING)THEN

   !## levenberg-marquardt
   DO I=1,NP; JQJ(I,I)=JQJ(I,I)+MARQUARDT*COV(I,I); ENDDO

  !## apply scaling
  ELSE

   !## compute scaling matrix
   C=0.0D0; DO I=1,NP; C(I,I)=1.0/SQRT(JQJ(I,I)); ENDDO

   !## construct JS matrix, scaled
   JS=0.0D0
   DO I=1,PEST_NOBS    !## row
    J=0
    DO IP1=1,SIZE(PARAM)
     IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
     DF1=REAL(PARAM(IP1)%DELTA,8)
     J=J+1
     DH1=REAL(MSR%DH(IP1,I),8)
     DH2=REAL(MSR%DH(0,I),8)
     DJ1=(DH1-DH2)/DF1
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
      W  =REAL(MSR%W(II),8)
      JQJ(J,I)=JQJ(J,I)+(DJ1*W*DJ2)
     ENDDO
    ENDDO
   ENDDO

   !## construct jTqr (<--- r is residual for current parameter set)
   JQR=0.0; I=0
   DO IP1=1,SIZE(PARAM)  !## row
    IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
    I=I+1
    DO J=1,PEST_NOBS
     DJ1=JS(I,J)
     DJ2=REAL(MSR%DH(0 ,J),8)
     W  =REAL(MSR%W(J),8)
     JQR(I)=JQR(I)+(DJ1*W*DJ2)
    ENDDO
   ENDDO

   !## add levenberg-marquardt
   DO I=1,NP; JQJ(I,I)=JQJ(I,I)+MARQUARDT*C(I,I)**2.0; ENDDO

  ENDIF

  !## project on important singular values
  IF(LSVD)THEN

   EIGWTHRESHOLD=0.0 !% explained variance
   DO NE=1,NP
    EIGWTHRESHOLD=EIGWTHRESHOLD+EIGW(NE)
    IF(EIGWTHRESHOLD.GT.99.0D0)EXIT
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
   CALL IMOD_UTL_LUDECOMP_DBL(M,INDX,NE,ISING)
   B=0.0D0; DO I=1,NE; B(I,I)=1.0D0; ENDDO
   DO I=1,NE; CALL IMOD_UTL_LUBACKSUB_DBL(M,INDX,B(1,I),NE); ENDDO

   !## compute U=(M)-1*N
   RU=0.0D0; DO I=1,NE; DO J=1,NE
    RU(I)=RU(I)+(B(J,I)*N(J))
   ENDDO; ENDDO

   !## reproject reduced gradient on original space
   !## compute U=(M)-1*N
   U=0.0D0; DO I=1,NP; DO J=1,NE
    U(I)=U(I)+(P(I,J)*RU(J))
   ENDDO; ENDDO

   DEALLOCATE(P,PT,M,N,RU,INDX,B)
  
  ELSE

   !## compute inverse of (JQJ)-1 -> B
   IF(ALLOCATED(INDX))DEALLOCATE(INDX); ALLOCATE(INDX(NP))
   IF(ALLOCATED(B   ))DEALLOCATE(B);    ALLOCATE(B   (NP,NP))
   IF(NP.EQ.1)THEN
    B(1,1)=1.0D0/JQJ(1,1)
   ELSE
    CALL IMOD_UTL_LUDECOMP_DBL(JQJ,INDX,NP,ISING)
    B=0.0D0; DO I=1,NP; B(I,I)=1.0D0; ENDDO
    DO I=1,NP; CALL IMOD_UTL_LUBACKSUB_DBL(JQJ,INDX,B(1,I),NP); ENDDO
   ENDIF
   
   !## compute (JQJ)-1*JQR
   U=0.0D0
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

  !## pointing downhill
  U=-1.0D0*U 

  !## within parameter adjust-limits
  IF(PESTUPGRADEVECTOR(1.0D0,.TRUE.,LAMBDARESET=LAMBDARESET))THEN 
   !## check whether number of parameters is equal to the number started this loop with
   MP=0; DO I=1,SIZE(PARAM); IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0)MP=MP+1; ENDDO
   IF(MP.EQ.NP)EXIT
  ENDIF
  !## reset marquardt lambda
  IF(LAMBDARESET)THEN
   MARQUARDT=0.001D0
  ELSE
   !## increase marquardt
   MARQUARDT=MARQUARDT*DAMPINGFACTOR
  ENDIF
  WRITE(IUPESTOUT,*) 'LAMBDARESET,MARQUARDT',LAMBDARESET,MARQUARDT

 ENDDO !## marquardt-loop

 !## write statistics
 CALL PEST1JQJ(JQJ,EIGW,EIGV,COV,NP,.TRUE.,ROOT,idf)

 !## multiply lateral sensitivities with sensitivities in case pest_niter=0
 IF(.NOT.PESTWRITESTATISTICS_PERROR(NP,COV,.TRUE.))THEN; ENDIF
! IF(LSENS)CALL PESTWRITESTATISTICS_FOSM(NP,COV)

 EIGWTHRESHOLD=0.0 !% explained variance
 WRITE(IUPESTOUT,'(/A10,2A15)') 'NE','EIGW(NE)','EIGWTHRESHOLD'
 DO NE=1,NP
  EIGWTHRESHOLD=EIGWTHRESHOLD+EIGW(NE)
  WRITE(IUPESTOUT,'(I10,2F15.7)') NE,EIGW(NE),EIGWTHRESHOLD
  IF(LSVD.AND.EIGWTHRESHOLD.GT.99.0D0)EXIT
 ENDDO
 IF(LSVD)THEN
  WRITE(IUPESTOUT,'(/A,I5,A/)') 'Used ',NE,' Eigenvalues (<99%) to project on limited number of basisfunctions'
 ELSE
  WRITE(IUPESTOUT,'(/A)') 'Consider using the SVD-option to ignore tiny eigenvalues to'
  WRITE(IUPESTOUT,'(A/)') 'make the optimization more robuust, numerically.'
 ENDIF
 
 WRITE(IUPESTPROGRESS,*)
 WRITE(IUPESTPROGRESS,*) 'Lambda/Damping Marquardt Factor = ',MARQUARDT
 WRITE(IUPESTPROGRESS,*) 'Marquardt Factor small: Gradient-Descent (near optimum)'
 WRITE(IUPESTPROGRESS,*) 'Marquardt Factor large: Gauss-Newton (far away optimum)'
 
 IF(LSCALING)WRITE(IUPESTPROGRESS,*) 'Scaling Value = ',MAXVAL(C)
 IF(LSVD)WRITE(IUPESTPROGRESS,*) 'Number of eigenvalues used: ',NE

 IF(ALLOCATED(EIGW))DEALLOCATE(EIGW)
 IF(ALLOCATED(EIGV))DEALLOCATE(EIGV)
 IF(ALLOCATED(JQR ))DEALLOCATE(JQR )
 IF(ALLOCATED(S   ))DEALLOCATE(S   )
 IF(ALLOCATED(C   ))DEALLOCATE(C   )
 IF(ALLOCATED(JS  ))DEALLOCATE(JS  )

 END SUBROUTINE PESTGRADIENT
 
 !###====================================================================
 SUBROUTINE PEST1JQJ(JQJ,EIGW,EIGV,COV,NP,LPRINT,ROOT,idf)
 !###====================================================================
 IMPLICIT NONE
 type(idfobj),intent(in) :: idf
 CHARACTER(LEN=*),INTENT(IN) :: ROOT
 INTEGER,INTENT(IN) :: NP
 LOGICAL,INTENT(IN) :: LPRINT
 REAL(KIND=8),DIMENSION(NP,NP),INTENT(OUT) :: JQJ,EIGV,COV
 REAL(KIND=8),DIMENSION(NP),INTENT(OUT) :: EIGW
 REAL(KIND=8) :: DET
 INTEGER :: I,J,K,IP1,IP2,II,N,M,ISING,IU,IIU,JUPESTOUT
 REAL(KIND=8) :: DF1,DF2,DJ1,DJ2,B1,TV,TEV,CB,KAPPA,W,DH1,DH2
 REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: B,JQJB
 REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: COR
 REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: E
 INTEGER,ALLOCATABLE,DIMENSION(:) :: INDX
 
 !## if sensisivities need to be computed generate csv file and stop
 IF(LSENS)THEN
  CALL PESTOPENFILE(iu,'log_jacobian_','txt',0,root)
  WRITE(IU,*) 'POSITIVE numbers means that an INcreasement of the parameter raises the head'
  WRITE(IU,*) 'NEGATIVE numbers means that an DEcreasement of the parameter raises the head'
  BLINE=''; M=0
  DO IP1=1,SIZE(PARAM)
   IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
   WRITE(SLINE,'(3X,A2,2I5.5,I3.3,A1)') PARAM(IP1)%PTYPE,PARAM(IP1)%ILS,PARAM(IP1)%IZONE,PARAM(IP1)%IGROUP,','
   BLINE=TRIM(BLINE)//TRIM(SLINE); M=M+1
  ENDDO
  WRITE(IU,'(4A11,A32,A)') 'X,','Y,','ILAY,','WEIGTH,','LABEL,',TRIM(BLINE)
  BLINE=''
  DO IP1=1,SIZE(PARAM)
   IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
   WRITE(SLINE,'(A15,A1)') PARAM(IP1)%ACRONYM
   BLINE=TRIM(BLINE)//TRIM(SLINE)
  ENDDO
  WRITE(IU,'(76X,A)') TRIM(BLINE)

  JQJ=0.0D0
  DO I=1,PEST_NOBS
   N=0
   DO IP1=1,SIZE(PARAM)               
    IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
    DF1=REAL(PARAM(IP1)%DELTA,8)
    DH1=REAL(MSR%DH(IP1,I),8)
    DH2=REAL(MSR%DH(0,I),8)
    DJ1=(DH1-DH2)/DF1
    N=N+1
    JQJ(N,1)=DJ1
    IF(M.GT.1)JQJ(N,2)=JQJ(N,2)+ABS(JQJ(N,1))
   ENDDO
   WRITE(IU,'(2(F10.2,A1),I10,A1,F10.2,A,A32,999(G15.7,A1))') MSR%X(I),',',MSR%Y(I),',',MSR%L(I),',', &
           MSR%W(I),',',TRIM(MSR%CLABEL(I))//',',(JQJ(J,1),',',J=1,N)
  ENDDO
  IF(M.EQ.1)THEN; WRITE(IU,'(/44X,A32,999(G15.7,A1))') 'TOTAL,',(ABS(JQJ(J,1)),',',J=1,N)
  ELSE; WRITE(IU,'(/44X,A32,999(G15.7,A1))') 'TOTAL,',(JQJ(J,2),',',J=1,N); ENDIF
 ENDIF
 
 !## save msr%dh() vector per parameter as csv ... to be read in again
 
 !## construct jqj - NORMAL MATRIX/HESSIAN
 JQJ=0.0; I=0
 DO IP1=1,SIZE(PARAM)                !## row
  IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
  DF1=REAL(PARAM(IP1)%DELTA,8)
  I=I+1; II=0; DO IP2=1,SIZE(PARAM)  !## column
   IF(PARAM(IP2)%IACT.NE.1.OR.PARAM(IP2)%IGROUP.LE.0)CYCLE
   DF2=REAL(PARAM(IP2)%DELTA,8)
   II=II+1
   DO J=1,PEST_NOBS
    DH1=REAL(MSR%DH(IP1,J),8)
    DH2=REAL(MSR%DH(0,J),8)
    DJ1=(DH1-DH2)/DF1
    DH1=REAL(MSR%DH(IP2,J),8)
    DJ2=(DH1-DH2)/DF2
    W=REAL(MSR%W(J),8)
    JQJ(II,I)=JQJ(II,I)+(DJ1*W*DJ2)  
   ENDDO
  ENDDO
 ENDDO

 !## construct covariance on the pilotpoints
 IF(PEST_IREGULARISATION.EQ.1)THEN
  CALL PEST_GETQPP(NP,.FALSE.,idf)
  JQJ=JQJ+QPP
 ENDIF
  
 IF(ALLOCATED(E   ))DEALLOCATE(E);    ALLOCATE(E   (NP))
 IF(ALLOCATED(COR ))DEALLOCATE(COR);  ALLOCATE(COR(NP,NP))
 IF(ALLOCATED(INDX))DEALLOCATE(INDX); ALLOCATE(INDX(NP))
 IF(ALLOCATED(B   ))DEALLOCATE(B);    ALLOCATE(B(NP,NP))
 IF(ALLOCATED(JQJB))DEALLOCATE(JQJB); ALLOCATE(JQJB(NP,NP))
 
 !## copy jqj to jqjb
 JQJB=JQJ
 !## copy jqj to b
 B=JQJB
 
! IF(LPRINT)THEN
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
! ENDIF

 !## compute determinant of JQJ
 DET=PEST_FIND_DET(JQJB,NP)

 IF(LPRINT)THEN
  WRITE(IUPESTOUT,'(/A18,E15.7)') 'Determinant JQJ = ',DET
  WRITE(IUPESTOUT,'(A/)') 'A small value for the Determinant indicates Singularity of the Matrix'
 ENDIF
  
 !## eigenvalue of covariance matrix 
 CALL RED1TRED2_DBL(B,NP,NP,EIGW,E)
 CALL RED1TQLI_DBL(EIGW,E,NP,NP,B)
 CALL RED1EIGSRT_DBL(EIGW,B,NP,NP)
 IF(LPRINT)WRITE(IUPESTOUT,'(/10X,4A15)') 'Eigenvalues','Sing.Values','Variance','Explained Var.'
 DO I=1,NP; IF(EIGW(I).LE.0.0)EIGW(I)=0.0; ENDDO; TEV=SUM(EIGW)
 TV=0.0D0
 DO I=1,NP
  TV=TV+(EIGW(I)*100.0D0/TEV)
  IF(EIGW(I).GT.0.0D0)THEN
   IF(LPRINT)WRITE(IUPESTOUT,'(I10,4F15.7)') I,EIGW(I),SQRT(EIGW(I)),EIGW(I)*100.0/TEV,TV
  ELSE
   IF(LPRINT)WRITE(IUPESTOUT,'(I10,4F15.7)') I,EIGW(I),     EIGW(I) ,EIGW(I)*100.0/TEV,TV
  ENDIF
 ENDDO
 EIGV= B  
 IF(SUM(EIGW).LT.0.0D0)THEN
  CALL IMOD_UTL_PRINTTEXT('Warning, there is NO information in parameter perturbation',0)
  CALL IMOD_UTL_PRINTTEXT('Optimization of parameters stopped',2)
 ENDIF
 EIGW=(EIGW*100.0D0)/SUM(EIGW)  

 !## condition number
 !## get lowest non-zero
 DO I=NP,1,-1; IF(EIGW(I).GT.0.0D0)EXIT; ENDDO
 KAPPA=SQRT(EIGW(1))/SQRT(EIGW(I))
 IF(LPRINT)THEN
  WRITE(IUPESTOUT,'(/A,F15.7/)') 'Condition Number (kappa):',LOG(KAPPA)
  WRITE(IUPESTOUT,'(/A)') '>>> If Kappa > 15, inversion is a concern due to parameters that are highly correlated <<<'
  WRITE(IUPESTOUT,'(A/)') '>>> If Kappa > 30, inversion is highly questionable due to parameters that are highly correlated <<<'
 ENDIF 

 !## compute inverse of (JQJB)-1 -> B - covariance matrix
 CALL IMOD_UTL_LUDECOMP_DBL(JQJB,INDX,NP,ISING)
 B=0.0D0; DO I=1,NP; B(I,I)=1.0D0; ENDDO
 DO I=1,NP; CALL IMOD_UTL_LUBACKSUB_DBL(JQJB,INDX,B(1,I),NP); ENDDO
 
 !## parameter covariance matrix
  
 N=MAX(1,PEST_NOBS-NP)
 B1=TJ/REAL(N,8)

! IF(LPRINT)THEN
!  WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'B1/TJ/N=',B1,TJ,N; WRITE(IUPESTOUT,*)
! ENDIF

 DO I=1,NP; DO J=1,NP; B(I,J)=B1*B(I,J); ENDDO; ENDDO

 IF(LPRINT)THEN
  DO K=1,2
   IF(K.EQ.1)IIU=IUPESTOUT 
   IF(K.EQ.2)THEN
    !## write covariance 
    IF(.NOT.LSENS)EXIT
    JUPESTOUT=IMOD_UTL_GETUNIT()
    OPEN(JUPESTOUT,FILE=TRIM(ROOT)//CHAR(92)//'PEST'//CHAR(92)//'COVARIANCE_'//TRIM(IMOD_UTL_ITOS(PEST_NITER))//'.TXT',STATUS='UNKNOWN',ACTION='WRITE')
    IIU=JUPESTOUT 
    WRITE(IIU,*) NP
    DO J=1,SIZE(PARAM)
     IF(PARAM(J)%IACT.EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
      WRITE(IIU,'(A2,3I3,A16)') PARAM(J)%PTYPE,PARAM(J)%ILS,PARAM(J)%IZONE,PARAM(J)%IGROUP,PARAM(J)%ACRONYM
     ENDIF
    ENDDO
   ENDIF
   WRITE(IIU,*); WRITE(IIU,*) 'Parameter Covariance Matrix (m2):'; WRITE(IUPESTOUT,*)

   BLINE=''
   DO J=1,SIZE(PARAM)
    IF(PARAM(J)%IACT.EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
     IF(PARAM(J)%ACRONYM.EQ.'')THEN
      WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PARAM(J)%PTYPE,PARAM(J)%ILS,PARAM(J)%IZONE,'-',PARAM(J)%IGROUP
     ELSE
      WRITE(SLINE,'(A15)') PARAM(J)%ACRONYM
     ENDIF
     BLINE=TRIM(BLINE)//TRIM(SLINE)
    ENDIF
   ENDDO
   WRITE(IIU,'(15X,A)') TRIM(BLINE) 
   WRITE(IIU,'(A)')
  
  ENDDO
 ENDIF

 I=0
 DO IP1=1,SIZE(PARAM)
  IF(PARAM(IP1)%IACT.EQ.1.AND.PARAM(IP1)%IGROUP.GT.0)THEN
   I=I+1
   IF(LPRINT)THEN
    IF(PARAM(IP1)%ACRONYM.EQ.'')THEN
     WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PARAM(IP1)%PTYPE,PARAM(IP1)%ILS,PARAM(IP1)%IZONE,'-',PARAM(IP1)%IGROUP
    ELSE
     WRITE(SLINE,'(A15)') PARAM(IP1)%ACRONYM
    ENDIF
    DO K=1,2
     IF(K.EQ.1)IIU=IUPESTOUT 
     IF(K.EQ.2)IIU=JUPESTOUT 
     WRITE(IIU,'(A15,999E15.7)') TRIM(SLINE),(B(I,J),J=1,NP)
     IF(.NOT.LSENS)EXIT
    ENDDO
   ENDIF
   DO J=1,NP; COV(I,J)=B(I,J); ENDDO
  ENDIF
 ENDDO
  
 IF(LPRINT)THEN
  !## parameter correlation matrix
  WRITE(IUPESTOUT,'(/A)') 'Parameter Correlation Matrix (-)'
  WRITE(IUPESTOUT,'(A)')  'Indicates whether coordinated changes in the parameter values could produce the same simulated values and'
  WRITE(IUPESTOUT,'(A/)') '  therefore, the same model fit'

  BLINE=''
  DO J=1,SIZE(PARAM)
   IF(PARAM(J)%IACT.EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
    IF(PARAM(J)%ACRONYM.EQ.'')THEN
     WRITE(SLINE,'(3X,A2,2I5.5,I3.3)') PARAM(J)%PTYPE,PARAM(J)%ILS,PARAM(J)%IZONE,PARAM(J)%IGROUP
    ELSE
     WRITE(SLINE,'(A15)') PARAM(J)%ACRONYM
    ENDIF
    BLINE=TRIM(BLINE)//TRIM(SLINE)
   ENDIF
  ENDDO
  WRITE(IUPESTOUT,'(15X,A)') TRIM(BLINE) 
  WRITE(IUPESTOUT,'(A)')

  COR=0.0D0; I=0
  DO IP1=1,SIZE(PARAM)
   IF(PARAM(IP1)%IACT.EQ.1.AND.PARAM(IP1)%IGROUP.GT.0)THEN
    I=I+1
    DO J=1,NP
     CB=B(I,I)*B(J,J)
     IF(CB.GT.0.0D0)THEN
      COR(I,J)=B(I,J)/SQRT(CB)
     ELSE
      COR(I,J)=0.0D0
     ENDIF
    ENDDO
    IF(PARAM(IP1)%ACRONYM.EQ.'')THEN
     WRITE(SLINE,'(3X,A2,2I5.5,I3.3)') PARAM(IP1)%PTYPE,PARAM(IP1)%ILS,PARAM(IP1)%IZONE,PARAM(IP1)%IGROUP
    ELSE
     WRITE(SLINE,'(A15)') PARAM(IP1)%ACRONYM
    ENDIF
    WRITE(IUPESTOUT,'(A15,999F15.7)') TRIM(SLINE),(COR(I,J),J=1,NP)
   ENDIF
  ENDDO

  !## write per parameter highly correlated other parameter
  WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'Parameter Correlated to (correlation > 0.95):'; WRITE(IUPESTOUT,*)
  IP1=0
  DO I=1,SIZE(PARAM)
   IF(PARAM(I)%IACT.EQ.1.AND.PARAM(I)%IGROUP.GT.0)THEN
    IF(PARAM(I)%ACRONYM.EQ.'')THEN
     WRITE(BLINE,'(3X,A2,2I5.5,I3.3)') PARAM(I)%PTYPE,PARAM(I)%ILS,PARAM(I)%IZONE,PARAM(I)%IGROUP
    ELSE
     WRITE(BLINE,'(A15)') PARAM(I)%ACRONYM
    ENDIF
    IP1=IP1+1
    IP2=0
    DO J=1,SIZE(PARAM)
     IF(PARAM(J)%IACT.EQ.1.AND.PARAM(J)%IGROUP.GT.0)THEN
      IP2=IP2+1
      IF(I.NE.J)THEN
       IF(PARAM(J)%ACRONYM.EQ.'')THEN
        WRITE(SLINE,'(3X,A2,2I5.5,I3.3)') PARAM(J)%PTYPE,PARAM(J)%ILS,PARAM(J)%IZONE,PARAM(J)%IGROUP
       ELSE
        WRITE(SLINE,'(A15)') PARAM(J)%ACRONYM
       ENDIF
       IF(ABS(COR(IP1,IP2)).GE.0.95D0)BLINE=TRIM(BLINE)//','//TRIM(SLINE)
      ENDIF
     ENDIF
    ENDDO
    WRITE(IUPESTOUT,'(A)') TRIM(BLINE)
   ENDIF  
  ENDDO
 ENDIF 
 
 IF(ALLOCATED(E   ))DEALLOCATE(E)
 IF(ALLOCATED(COR ))DEALLOCATE(COR)
 IF(ALLOCATED(INDX))DEALLOCATE(INDX)
 IF(ALLOCATED(B   ))DEALLOCATE(B)
 IF(ALLOCATED(JQJB))DEALLOCATE(JQJB)

 IF(LSENS)STOP
 IF(.FALSE.)CLOSE(IIU)
 
 END SUBROUTINE PEST1JQJ

 !###====================================================================
 LOGICAL FUNCTION PESTUPGRADEVECTOR(FCT,LCHECK,LAMBDARESET)
 !###====================================================================
 IMPLICIT NONE
 LOGICAL,INTENT(IN) :: LCHECK
 LOGICAL,INTENT(OUT),OPTIONAL :: LAMBDARESET
 REAL(KIND=8),INTENT(IN) :: FCT
 REAL(KIND=8) :: AF,F,G,MINAP,MAXAP,P1,P2,PMIN,PMAX
 INTEGER :: I,J,IP1,IP2,N,IBND

 !## exit code
 PESTUPGRADEVECTOR=.FALSE.
 IF(PRESENT(LAMBDARESET))LAMBDARESET=.FALSE.
 
 !## adjust vector for fct (line-search)
 I=0; DO IP1=1,SIZE(PARAM)
  IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
  I=I+1; U(I)=U(I)*FCT
  WRITE(IUPESTOUT,'(3I5,1X,G15.8)') IP1,PARAM(IP1)%IACT,PARAM(IP1)%IGROUP,U(I)
 ENDDO

 !## fill in by default 
 DO IP1=1,SIZE(PARAM)
  PARAM(IP1)%ALPHA(1)=PARAM(IP1)%ALPHA(2)
 ENDDO
 
 I=0
 DO IP1=1,SIZE(PARAM)
  IF(PARAM(IP1)%IACT.EQ.1)THEN
   I=I+1; DO IP2=1,SIZE(PARAM)
    IF(PARAM(IP1)%IGROUP.EQ.ABS(PARAM(IP2)%IGROUP))THEN
     PARAM(IP2)%ALPHA(1)=PARAM(IP2)%ALPHA(2)+U(I)
    ENDIF
   ENDDO
  ENDIF
 ENDDO  

! DO IP1=1,SIZE(PARAM)
!  WRITE(IUPESTOUT,'(3I5,3(1X,G15.9))') IP1,PARAM(IP1)%IACT,PARAM(IP1)%IGROUP,PARAM(IP1)%ALPHA(1),PARAM(IP1)%ALPHA(2)
! ENDDO

 !## check whether boundary has been hit or maximum adjustment exceeds
 IF(LCHECK)THEN

  !## check for size of adjustment
  DO IP1=1,SIZE(PARAM)
   
   !## inactive parameter
   IF(PARAM(IP1)%IACT.NE.1)CYCLE
   
   !## check size of adjustment
   IF(PARAM(IP1)%LOG)THEN
    F=EXP(PARAM(IP1)%ALPHA(1))/EXP(PARAM(IP1)%ALPHA(2))
   ELSE
    F=PARAM(IP1)%ALPHA(1)/PARAM(IP1)%ALPHA(2)
   ENDIF 

   !## adjustment too large -causes to get another lambda
   IF(F.LT.1.0D0/PARAM(IP1)%FADJ.OR.F.GT.PARAM(IP1)%FADJ)THEN
    IF(TRIM(PARAM(IP1)%ACRONYM).EQ.'')THEN
     WRITE(IUPESTOUT,'(A,G15.8)') 'Increase Lambda - adjustment for parameter '//TRIM(IMOD_UTL_ITOS(IP1))//' to small/large ',F
    ELSE
     WRITE(IUPESTOUT,'(A,G15.8)') 'Increase Lambda - adjustment for parameter '//TRIM(PARAM(IP1)%ACRONYM)//' to small/large ',F
    ENDIF
    FLUSH(IUPESTOUT)
    RETURN
   ENDIF
  ENDDO
  
  F=1.0D0

  !## check for boundary of parameter
  DO IP1=1,SIZE(PARAM)
   
   !## inactive parameter
   IF(PARAM(IP1)%IACT.NE.1)CYCLE

   CALL PEST_GETBOUNDARY(IP1,IBND,P1,P2,PMIN,PMAX)

   !## parameter hits the boundary
   IF(IBND.NE.0)THEN
    !## hits the same boundary as before - skip it
    IF(IBND.EQ.PARAM(IP1)%IBND)THEN 
     !## ignore this parameter (group) for now - reset lambda and search another update vector
     DO IP2=1,SIZE(PARAM)
      IF(PARAM(IP2)%IACT.EQ.0)CYCLE
      IF(ABS(PARAM(IP2)%IGROUP).EQ.ABS(PARAM(IP1)%IGROUP))PARAM(IP2)%IACT=-1
     ENDDO
     IF(TRIM(PARAM(IP1)%ACRONYM).EQ.'')THEN
      WRITE(IUPESTOUT,'(A)') 'Reset lambda - parameter '//TRIM(IMOD_UTL_ITOS(IP1))//' hits boundary.'
     ELSE
      WRITE(IUPESTOUT,'(A)') 'Reset lambda - parameter '//TRIM(PARAM(IP1)%ACRONYM)//' hits boundary.'
     ENDIF
     FLUSH(IUPESTOUT)
     LAMBDARESET=.TRUE.; RETURN
    ELSE
     AF=1.0D0
     IF(P1.LT.PMIN)AF=(P2-PMIN)/(P2-P1)
     IF(P1.GT.PMAX)AF=(PMAX-P2)/(P1-P2)
     write(IUPESTOUT,'(A,I10,4F10.3)') 'parameter,p1,p2,pmin,pmax',ip1,p1,p2,pmin,pmax
     !## keep track of minimal adjustment of vector
     F=MIN(AF,F)
     write(IUPESTOUT,'(A,2F10.3)') 'af,f',af,f
     !## recompute gradient and set this parameter on boundary
     IF(F.LT.0.1D0)THEN
      G=PARAM(IP1)%ALPHA(1)-PARAM(IP1)%ALPHA(2)
      G=G*F
      DO IP2=1,SIZE(PARAM)
       IF(PARAM(IP2)%IACT.EQ.0)CYCLE
       IF(ABS(PARAM(IP2)%IGROUP).EQ.ABS(PARAM(IP1)%IGROUP))THEN
        PARAM(IP2)%ALPHA(1)=PARAM(IP2)%ALPHA(2)+G
        PARAM(IP2)%IACT=-1
       ENDIF
      ENDDO
!      PARAM(IP1)%ALPHA(1)=PARAM(IP1)%ALPHA(2)+G
      !## ignore this parameter
      IF(TRIM(PARAM(IP1)%ACRONYM).EQ.'')THEN
       WRITE(IUPESTOUT,'(A,F10.2,A)') 'Reset lambda - parameter '//TRIM(IMOD_UTL_ITOS(IP1))//' close by boundary f=',F,' snapped to it'
      ELSE
       WRITE(IUPESTOUT,'(A,F10.2,A)') 'Reset lambda - parameter '//TRIM(PARAM(IP1)%ACRONYM)//' close by boundary f=',F,' snapped to it'
      ENDIF
      FLUSH(IUPESTOUT)
      !      PARAM(IP1)%IACT=-1; 
      LAMBDARESET=.TRUE.; RETURN
     ENDIF
    ENDIF
   ENDIF  
  ENDDO
 
  !## corrects all gradients with this factor
  IF(F.LT.1.0D0)THEN
   
   WRITE(*,*) 'Correct update vector=',F
   
   !## adjust all parameters
   DO IP2=1,SIZE(PARAM) 
    IF(PARAM(IP2)%IACT.EQ.0)CYCLE
   
    G=PARAM(IP2)%ALPHA(1)-PARAM(IP2)%ALPHA(2)
    G=G*F

    !## update parameters
    PARAM(IP2)%ALPHA(1)=PARAM(IP2)%ALPHA(2)+G
   ENDDO

  ENDIF
 
 ENDIF
  
 !## correct update gradient found
 PESTUPGRADEVECTOR=.TRUE.

 J=0; DO IP1=1,SIZE(PARAM)
  IF(PARAM(IP1)%IACT.EQ.0)CYCLE

  IF(PARAM(IP1)%LOG)THEN
   PARAM(IP1)%ALPHA_HISTORY(PEST_ITER)=EXP(PARAM(IP1)%ALPHA(1))
  ELSE
   PARAM(IP1)%ALPHA_HISTORY(PEST_ITER)=PARAM(IP1)%ALPHA(1)
  ENDIF
  
  !## active parameter
  IF(PARAM(IP1)%IACT.EQ.1.AND.PARAM(IP1)%IGROUP.GT.0)THEN
   J=J+1
   !## store final gradient
   U(J)=PARAM(IP1)%ALPHA(1)-PARAM(IP1)%ALPHA(2)
  ENDIF
  
 ENDDO

 WRITE(IUPESTOUT,*) 'final ones'
 DO I=1,SIZE(PARAM)
  WRITE(IUPESTOUT,'(3I5,1x,2G15.8)') I,PARAM(I)%IACT,PARAM(I)%IGROUP,PARAM(I)%ALPHA(1),PARAM(I)%ALPHA(2)
 ENDDO
 WRITE(IUPESTOUT,*) 
 FLUSH(IUPESTOUT)
 
 END FUNCTION PESTUPGRADEVECTOR

 !###====================================================================
 SUBROUTINE PEST_GETBOUNDARY(IP1,IBND,P1,P2,PMIN,PMAX)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IP1
 INTEGER,INTENT(OUT) :: IBND
 REAL(KIND=8),INTENT(OUT) :: P1,P2,PMIN,PMAX
 
 !## parameter adjustment hit the parameter boundary
 P1  =PARAM(IP1)%ALPHA(1)
 P2  =PARAM(IP1)%ALPHA(2)
 PMIN=PARAM(IP1)%MIN
 PMAX=PARAM(IP1)%MAX
   
 IBND=0
 !## shoot over
 IF(P1.LE.PMIN)           IBND=-1; IF(P1.GE.PMAX)           IBND= 1
 !## too close
 IF(ABS(P1-PMIN).LE.XPBND)IBND=-1; IF(ABS(PMAX-P1).LE.XPBND)IBND= 1
 
 END SUBROUTINE PEST_GETBOUNDARY

 !###====================================================================
 SUBROUTINE PEST_ECHOPARAMETERS(GUPDATE)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=8),INTENT(OUT) :: GUPDATE
 INTEGER :: IP1,N,I
 REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: GRADUPDATE
 
 WRITE(IUPESTOUT,'(/A)') 'Upgrade Vector Parameter History:'
 WRITE(BLINE,'(A19,99(A7,I3.3))') 'Parameter',('   ITER',I,I=PEST_ITER,0,-1)
 WRITE(IUPESTOUT,'(A)') TRIM(BLINE)
  
 ALLOCATE(GRADUPDATE(PEST_ITER)); GRADUPDATE=0.0D0
 N=0
 DO IP1=1,SIZE(PARAM)

  WRITE(BLINE,'(99(F10.5))') (PARAM(IP1)%ALPHA_HISTORY(I),I=PEST_ITER,0,-1)
  
  IF(ABS(PARAM(IP1)%IACT).EQ.1.AND.PARAM(IP1)%IGROUP.GT.0)THEN
   IF(PARAM(IP1)%ACRONYM.EQ.'')THEN
    WRITE(IUPESTOUT,'(3X,A2,2I5.5,A1,I3.3,A)') PARAM(IP1)%PTYPE,PARAM(IP1)%ILS,PARAM(IP1)%IZONE,'-',ABS(PARAM(IP1)%IGROUP),TRIM(BLINE)
   ELSE
    WRITE(IUPESTOUT,'(4X,A15,A)') PARAM(IP1)%ACRONYM,TRIM(BLINE)
   ENDIF
  
!  !## use only active groups
!  IF(PARAM(IP1)%IGROUP.GT.0)THEN
   N=N+1
   DO I=1,PEST_ITER
    GRADUPDATE(I)=GRADUPDATE(I)+(PARAM(IP1)%ALPHA_HISTORY(I)-PARAM(IP1)%ALPHA_HISTORY(I-1))**2.0D0
   ENDDO
  ENDIF

 ENDDO
 
 GRADUPDATE=SQRT(GRADUPDATE)
 WRITE(BLINE,'(19X,99E10.3)') (GRADUPDATE(I),I=PEST_ITER,1,-1)
 WRITE(IUPESTOUT,'(A)') TRIM(BLINE)

 GUPDATE=GRADUPDATE(PEST_ITER)
! IF(GRADUPDATE(PEST_ITER).LT.PEST_PADJ)THEN
!  CALL IMOD_UTL_PRINTTEXT('Process stopped, less than '//TRIM(IMOD_UTL_DTOS(PEST_PADJ,'F',3))//' of vector length',-1,IUPESTOUT)
!  STOP
! ENDIF

 DEALLOCATE(GRADUPDATE)

! I=0; DO IP1=1,SIZE(PARAM)
!  IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
!  I=I+1; U(I)=U(I)*FCT
! ENDDO

 END SUBROUTINE PEST_ECHOPARAMETERS

 !###====================================================================
 SUBROUTINE PEST_GETJ(LSS,root,idf)
 !###====================================================================
 USE RF2MF_MODULE, ONLY: NPER
 IMPLICIT NONE
 type(idfobj),intent(in) :: idf
 LOGICAL,INTENT(IN) :: LSS
 CHARACTER(LEN=*),INTENT(IN) :: ROOT
 INTEGER :: I,II,III,J,JJ,K,KK,ILAY,NROWIPFTXT,NCOLIPFTXT, &
     IOS,NAJ,NP,N,IUIPFTXT
 REAL(KIND=8) :: X,Y,Z,H,WW,MC,MM,DHH,XCOR,YCOR,ZCOR,XCROSS,RFIT,DTH,DTD,D,DRES
 CHARACTER(LEN=52) :: ID,TXT
 DOUBLE PRECISION :: DHW
 REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: TSNODATA,M,C,GF_H,GF_O,MCOPY,CCOPY
 INTEGER(KIND=8),ALLOCATABLE,DIMENSION(:) :: IDATE
 REAL(KIND=8),DIMENSION(2) :: PC,PM,DYN !## percentiles computed/measured
 LOGICAL :: LEX
 
 CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT(' Getting residuals from IPF files ...',0)

 DO I=1,ABS(IIPF)

!  !## read ipf steady-state
!  IF(LSS)THEN
!   J=INDEX(TS(I)%IPFNAME,CHAR(92),.TRUE.)+1; LINE=TRIM(ROOT)//CHAR(92)//TS(I)%IPFNAME(J:)
!  ELSE
  J=INDEX(TS(I)%IPFNAME,CHAR(92),.TRUE.)+1
  LINE=TRIM(ROOT)//CHAR(92)//'timeseries'//CHAR(92)//TS(I)%IPFNAME(J:)
!  ENDIF
  TS(I)%IUIPF=0; CALL IMOD_UTL_SWAPSLASH(LINE); CALL IMOD_UTL_OPENASC(TS(I)%IUIPF,LINE,'R')
  READ(TS(I)%IUIPF,'(A256)') LINE; CALL IMOD_UTL_STRING(LINE); READ(LINE,*) TS(I)%NROWIPF
  READ(TS(I)%IUIPF,'(A256)') LINE; CALL IMOD_UTL_STRING(LINE); READ(LINE,*) TS(I)%NCOLIPF
  DO J=1,TS(I)%NCOLIPF; READ(TS(I)%IUIPF,*); ENDDO
  !## read iext,ext
  READ(TS(I)%IUIPF,'(A256)') LINE
  READ(LINE,*) TS(I)%IEXT,TS(I)%EXT

 ENDDO

 !## only one value per measurement
 N=SUM(TS%NROWIPF)*NPER
 IF(.NOT.ASSOCIATED(MSR%DH)) ALLOCATE(MSR%DH(0:SIZE(PARAM),N))
 IF(.NOT.ASSOCIATED(MSR%W )) ALLOCATE(MSR%W (N)) 
 IF(.NOT.ASSOCIATED(MSR%X )) ALLOCATE(MSR%X (N)) 
 IF(.NOT.ASSOCIATED(MSR%Y )) ALLOCATE(MSR%Y (N)) 
 IF(.NOT.ASSOCIATED(MSR%L )) ALLOCATE(MSR%L (N)) 
 IF(.NOT.ASSOCIATED(MSR%CLABEL))ALLOCATE(MSR%CLABEL(N)) 
 IF(.NOT.ALLOCATED(GF_H))ALLOCATE(GF_H(N))
 IF(.NOT.ALLOCATED(GF_O))ALLOCATE(GF_O(N))

 !## initialise head-differences
 IF(PEST_IGRAD.EQ.0)THEN
  DO I=1,SIZE(MSR%DH,2)
   MSR%DH(PEST_IGRAD,I)=0.0
  ENDDO
 ELSE
  DO I=1,SIZE(MSR%DH,2)
   MSR%DH(PEST_IGRAD,I)=MSR%DH(0,I) !## zero gradient in case parameter is fixed
  ENDDO
 ENDIF
 
 MSR%W=0.0
 TJ=0.0D0

 DO I=1,ABS(IIPF) 
  IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(I10,A)') I,','//TRIM(TS(I)%IPFNAME)
 ENDDO

 !## steady-state
 IF(LSS)THEN
  IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(2A16,A11,6A16,A11,A32)') 'X,','Y,','ILAY,','MSR,','MDL,', &
         'J,','WMDL,','WRESIDUAL,','WEIGH,','IPF,','LABEL'
 !## transient
 ELSE  
  IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(2A16,A11,8A16,A11,A32,1X,A15)') 'X,','Y,','ILAY,','WEIGH,','MSR,','MDL,','MDL-MSR,', &
                                   'DYNMSR,','DYNMDL,','DYNMSR-DYNMDL,','CROSS-COR,','IPF,','LABEL','DATE'
 ENDIF
 
 II=0
 
 DO I=1,ABS(IIPF)
 
  IF(TS(I)%IEXT.EQ.0)THEN

   DO J=1,TS(I)%NROWIPF
   
    II=II+1
    IF(TS(I)%IDCOL.EQ.0)THEN
     D=0.0D0
     READ(TS(I)%IUIPF,*) X,Y,ILAY,Z,MSR%W(II),H    !## w(i)=variance
    ELSE
     READ(TS(I)%IUIPF,*) X,Y,ILAY,Z,MSR%W(II),H,D  !## w(i)=variance
    ENDIF
    !## weigh=1/variance
    IF(TS(I)%IVCOL.GT.0)THEN
     IF(MSR%W(II).LE.0.0D0)THEN
      !## insert measurement only whenever h.gt.z
      IF(H.GT.Z)THEN
       MSR%W(II)=ABS(MSR%W(II))
      ELSE
       MSR%W(II)=0.0D0
      ENDIF
     ELSE
      MSR%W(II)=1.0D0/MSR%W(II)
     ENDIF
    ENDIF
    
    !## apply general multiplication for weight values
    MSR%W(II)=FWIIPF*MSR%W(II)
    
    !## calculated - measured
    DHH=H-Z
    
    DRES=PEST_DRES; IF(DRES.EQ.0.0)DRES=D
    
    IF(ABS(DHH).LT.DRES)THEN
     DHH=0.0D0
    ELSE
     IF(DHH.GT. DRES)DHH=DHH-DRES
     IF(DHH.LT.-DRES)DHH=DHH+DRES
    ENDIF

    MSR%DH(PEST_IGRAD,II)=DHH  !## calculated - measured
    DHW=MSR%W(II)*(DHH**2.0D0)

    MSR%X(II)=X
    MSR%Y(II)=Y
    MSR%L(II)=ILAY
    MSR%CLABEL(II)='Measure'//TRIM(ITOS(J))//'_ipf'//TRIM(ITOS(I))

    GF_H(II)         =MSR%W(II)*H
    GF_O(II)         =MSR%W(II)*Z

    TJ               = TJ+DHW
    IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(2(G15.7,1X),I10,1X,6(G15.7,1X),I10,1X,A32)') &
        X,Y,ILAY,Z,H,DHW,MSR%W(II)*H,MSR%W(II)*(H-Z),MSR%W(II),I,MSR%CLABEL(II)

   ENDDO
  
  !## transient
  ELSE
    
   XCROSS=0.0D0
   DO J=1,TS(I)%NROWIPF
    IF(TS(I)%IDCOL.EQ.0)THEN
     D=0.0D0
     READ(TS(I)%IUIPF,*) X,Y,ILAY,ID,WW     !## w(i)=variance
    ELSE
     READ(TS(I)%IUIPF,*) X,Y,ILAY,ID,WW,D     !## w(i)=variance
    ENDIF
    !## weigh=1/stdev=1/variance
    IF(TS(I)%IVCOL.GT.0)THEN
     IF(WW.LE.0.0D0)THEN
      WW=0.0D0
     ELSE
      WW=1.0/WW
     ENDIF
    ENDIF
    
    !## apply general multiplication for weight values
    WW=FWIIPF*WW

    LINE=TRIM(ROOT)//CHAR(92)//'timeseries'//CHAR(92)//TRIM(ID)//'.'//TRIM(TS(I)%EXT)
    IUIPFTXT=GETUNIT(); OPEN(IUIPFTXT,FILE=LINE,STATUS='OLD',ACTION='READ')
    
    READ(IUIPFTXT,*) NROWIPFTXT
    READ(IUIPFTXT,*) NCOLIPFTXT
    ALLOCATE(TSNODATA(MAX(3,NCOLIPFTXT)))
    DO K=1,NCOLIPFTXT; READ(IUIPFTXT,*) TXT,TSNODATA(K); ENDDO
    ALLOCATE(M(NROWIPFTXT),C(NROWIPFTXT),IDATE(NROWIPFTXT),MCOPY(NROWIPFTXT),CCOPY(NROWIPFTXT))
    IDATE=0; C=0.0; M=0.0; MCOPY=M; CCOPY=C
    IF(NCOLIPFTXT.LT.3)TSNODATA(3)=TSNODATA(2)
    
    !## get mean measure
    KK=0
    DO K=1,NROWIPFTXT
     KK=KK+1
     READ(IUIPFTXT,*,IOSTAT=IOS) IDATE(KK),M(KK),C(KK) 

     !## make double precision dates - if needed
     IF(IDATE(KK).LT.100000000)IDATE(KK)=IDATE(KK)*1000000

     !## error reading, skip it (can be caused by steady-state periods in between)
     IF(IOS.NE.0)THEN; KK=KK-1; CYCLE; ENDIF

     !## check period (if available)
     IF(PEST_NPERIOD.GT.0)THEN
      DO III=1,PEST_NPERIOD
!       write(*,*) kk,iii,idate(kk),pest_iperiod(iii,1),pest_iperiod(iii,2)
       IF(IDATE(KK).GE.PEST_IPERIOD(III,1).AND.IDATE(KK).LE.PEST_IPERIOD(III,2))EXIT
      ENDDO
      IF(III.GT.PEST_NPERIOD)C(KK)=TSNODATA(3)
     ENDIF
     IF(M(KK).EQ.TSNODATA(2).OR.C(KK).EQ.TSNODATA(3))KK=KK-1
    ENDDO 

    !## add this measurement
    IF(KK.GT.0)THEN
    
     !## compute mean measurement in period
     XCOR=-9999.99D0

     !## mean values
     MM=SUM(M(1:KK))/REAL(KK) !## measurements
     MC=SUM(C(1:KK))/REAL(KK) !## computed
     DO K=1,KK
      MCOPY(K)=M(K)
      CCOPY(K)=C(K)
     ENDDO
     !## percentiles
     CALL IMOD_UTL_GETMED(MCOPY,KK,-999.99D0,(/10.0D0,90.0D0/),2,NAJ,PM)
     CALL IMOD_UTL_GETMED(CCOPY,KK,-999.99D0,(/10.0D0,90.0D0/),2,NAJ,PC)
     DYN(1)=PM(2)-PM(1) !## measurements
     DYN(2)=PC(2)-PC(1) !## computed
     !## compute cross-correlation
     IF(KK.GT.1)THEN
      XCOR=0.0D0; YCOR=0.0D0; ZCOR=0.0D0
      DO K=1,KK
       XCOR=XCOR+(MM-M(K))*(MC-C(K))
       YCOR=YCOR+(MM-M(K))**2.0D0
       ZCOR=ZCOR+(MC-C(K))**2.0D0
      ENDDO
      IF(YCOR.NE.0.0.AND.ZCOR.NE.0.0)XCOR=XCOR/(SQRT(YCOR)*SQRT(ZCOR))
      XCROSS=XCROSS+XCOR
     ENDIF

     !## add obseravtion
     DO K=1,KK
      II =II+1

      DTH=0.0D0
      DRES=PEST_DRES; IF(DRES.EQ.0.0)DRES=D

      !## target is residual (calculated minus measured)
      IF(PEST_ITARGET(1).GT.0.0D0)THEN
       DTH=C(K)-M(K)
       IF(ABS(DTH).LT.DRES)THEN
        DTH=0.0D0
       ELSE
        IF(DTH.GT. DRES)DTH=DTH-DRES
        IF(DTH.LT.-DRES)DTH=DTH+DRES
       ENDIF
       DTH=PEST_ITARGET(1)*DTH
      ENDIF
      
      !## target is dynamics (calculated minus measured)
      DTD=0.0D0
      IF(PEST_ITARGET(2).GT.0.0D0)THEN
       DTD=DYN(2)-DYN(1)
       IF(ABS(DTD).LT.DRES)THEN
        DTD=0.0D0
       ELSE
        IF(DTD.GT. DRES)DTD=DTD-DRES
        IF(DTD.LT.-DRES)DTD=DTD+DRES
       ENDIF
       DTD=PEST_ITARGET(2)*DTD
      ENDIF
      
      DHH=DTH+DTD      

      MSR%DH(PEST_IGRAD,II)=DHH       !## - total sensitivity

      MSR%X(II)=X
      MSR%Y(II)=Y
      MSR%L(II)=ILAY
      MSR%CLABEL(II)=TRIM(ID)

      !## weight, pest_itarget(.) should/will be summed to one
      MSR%W(II)=WW

      !## difference
      DHW=MSR%W(II)*(DHH**2.0D0)
      TJ=TJ+DHW

      GF_H(II)=MSR%W(II)*C(K) 
      GF_O(II)=MSR%W(II)*M(K)

      IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(2(G15.7,1X),I10,1X,8(G15.7,1X),I10,1X,A32,1X,I15)') &
         X,Y,ILAY,WW,M(K),C(K),C(K)-M(K),DYN(1),DYN(2),DYN(2)-DYN(1),XCOR,I,MSR%CLABEL(II),IDATE(K)

      IF(PEST_ITARGET(1).EQ.0.0D0.AND.PEST_ITARGET(2).GT.0.0D0)EXIT
     
     ENDDO
     
    ENDIF
    
    DEALLOCATE(TSNODATA,C,M,MCOPY,CCOPY,IDATE)
    CLOSE(IUIPFTXT); IUIPFTXT=0

   ENDDO
  ENDIF

  CLOSE(TS(I)%IUIPF)

  IF(TS(I)%NROWIPF.GT.0)THEN
   IF(.NOT.LSS)CALL IMOD_UTL_PRINTTEXT('MEAN Cross-Correlation         : '// &
          TRIM(IMOD_UTL_DTOS(REAL(XCROSS)/REAL(TS(I)%NROWIPF,8),'F',7))//' (n='//TRIM(ITOS(TS(I)%NROWIPF))//')',1)
  ENDIF

 ENDDO
 PEST_NOBS=II
 
 IF(PEST_NOBS.LE.0)THEN
  CALL IMOD_UTL_PRINTTEXT('No measurements available within current spatial/temporal space.',2)
 ENDIF
 
 !## run batch files
 CALL PEST_BATCHFILES()

 !## insert regularisation to objective function
 NP=0
 DO I=1,SIZE(PARAM)
  IF(PARAM(I)%NODES.EQ.0.OR.PARAM(I)%IACT.EQ.0.OR.PARAM(I)%IGROUP.LE.0)CYCLE
  NP=NP+1
 ENDDO
 
 PJ=0.0D0
 IF(PEST_IREGULARISATION.EQ.1)CALL PEST_GETQPP(NP,.TRUE.,idf)
  
 IF(LGRAD.AND.PEST_IGRAD.EQ.0)THEN
  CALL IMOD_UTL_PRINTTEXT('Best Match Value   :             '//TRIM(IMOD_UTL_DTOS(TJ,'G',7)),-1,IUPESTOUT)
  CALL IMOD_UTL_PRINTTEXT('Plausibility Value :             '//TRIM(IMOD_UTL_DTOS(PJ,'G',7)),-1,IUPESTOUT)
 ENDIF
 
 TJ=TJ+PJ

 IF(LGRAD.AND.PEST_IGRAD.EQ.0)THEN
  CALL IMOD_UTL_PRINTTEXT('TOTAL Objective Function Value : '//TRIM(IMOD_UTL_DTOS(TJ,'G',7)),-1,IUPESTOUT)
  CALL IMOD_UTL_PRINTTEXT('MEAN Objective Function Value  : '//TRIM(IMOD_UTL_DTOS(TJ/REAL(PEST_NOBS,8),'G',7))// &
          ' (n='//TRIM(IMOD_UTL_ITOS(PEST_NOBS))//')',-1,IUPESTOUT)

  RFIT=PEST_GOODNESS_OF_FIT(GF_H,GF_O,PEST_NOBS)
  CALL IMOD_UTL_PRINTTEXT('Goodness of Fit:                 '// &
      TRIM(IMOD_UTL_DTOS(RFIT,'G',7))//' (n='//TRIM(IMOD_UTL_ITOS(PEST_NOBS))//')',-1,IUPESTOUT)
  CALL IMOD_UTL_PRINTTEXT('>> Provides a measure of the extent to which variability of field measurements is explained',-1,IUPESTOUT)
  CALL IMOD_UTL_PRINTTEXT('   by the calibrated model compared to that which can be constructed as purely random. <<',-1,IUPESTOUT)
 ENDIF

 IF(ALLOCATED(GF_H))DEALLOCATE(GF_H)
 IF(ALLOCATED(GF_O))DEALLOCATE(GF_O)
  
 CALL PESTPROGRESS()

 IF(LGRAD)THEN
  IF(PEST_IGRAD.EQ.0)THEN
   IF(PEST_ITER.EQ.1)WRITE(IUPESTEFFICIENCY,'(3E15.7)') TJ,SQRT(TJ),TJ/REAL(PEST_NOBS)
   TJOBJ=TJ
  ELSE
   PARAM(PEST_IGRAD)%TJOBJ=TJ
  ENDIF
 ENDIF
 IF(LLNSRCH)THEN

 ENDIF
 
 CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT(' Finished Getting residuals from IPF files ...',0)

 END SUBROUTINE PEST_GETJ

 !###====================================================================
 REAL FUNCTION PEST_GOODNESS_OF_FIT(X,Y,N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=8),INTENT(IN),DIMENSION(N) :: X,Y !## x=head; y=obs
 REAL(KIND=8) :: XN,YN,X1,X2,X3,YA
 INTEGER :: I
  
 PEST_GOODNESS_OF_FIT=0.0D0

 XN=SUM(X)/DBLE(N)
 YN=SUM(Y)/DBLE(N)

 X1=0.0D0; X2=0.0D0; X3=0.0D0
 DO I=1,N
  X1=X1+(X(I)-XN)*(Y(I)-YN)
  X2=X2+(X(I)-XN)**2.0D0
  X3=X3+(Y(I)-YN)**2.0D0
 ENDDO

 !## sample correlation coefficient
 IF(X2.NE.0.0D0.AND.X3.NE.0.0D0)PEST_GOODNESS_OF_FIT=X1/SQRT(X2*X3)

 END FUNCTION PEST_GOODNESS_OF_FIT
 
 !###====================================================================
 SUBROUTINE PEST_GETQPP(NP,LPJ,idf)
 !###====================================================================
 IMPLICIT NONE
 type(idfobj),intent(in) :: idf
 INTEGER,INTENT(IN) :: NP
 LOGICAL,INTENT(IN) :: LPJ
 INTEGER :: I,J,II,JJ
 REAL(KIND=8) :: ALPHA,GAMMA,X1,X2,Y1,Y2,RANGE,NUGGET,SILL
 REAL,ALLOCATABLE,DIMENSION(:) :: AQPP
 
 SILL=100.0D0
 NUGGET=0.0D0
 
 IF(ALLOCATED(QPP))DEALLOCATE(QPP); ALLOCATE(QPP(NP,NP)); QPP =0.0D0

 !## fill array zone and set appropriate pointers in type 
 II=0; DO I=1,SIZE(PARAM)
  !## skip zero-zones, inactive parameters/groupmembers
  IF(PARAM(I)%NODES.LE.0.OR.PARAM(I)%IACT.NE.1.OR.PARAM(I)%IGROUP.LE.0)CYCLE
  
  II=II+1
  
  !## zones
  IF(PARAM(I)%ZTYPE.EQ.0)THEN
   !## add covariance ... if known - now leave it zero
   
  !## pilotpoints
  ELSEIF(PARAM(I)%ZTYPE.EQ.1)THEN
 
   RANGE=0.9D0*UTL_DIST(IDF%XMIN,IDF%YMIN,IDF%XMAX,IDF%YMAX) !SQRT((X2-X1)**2.0D0+(Y2-Y1)**2.0D0)   
!   UTL_KRIGING_RANGE(IDF%XMIN,IDF%YMIN,IDF%XMAX,IDF%YMAX) !CDELR(0),CDELR(NCOL),CDELC(NROW),CDELC(0))
   !## fill in covariance matrix based upon semivariogram
   X1=PARAM(I)%XY(1,1); Y1=PARAM(I)%XY(1,2)
   JJ=0; DO J=1,SIZE(PARAM)
    !## skip zero-zones, inactive parameters/groupmembers
    IF(PARAM(J)%NODES.LE.0.OR.PARAM(J)%IACT.NE.1.OR.PARAM(J)%IGROUP.LE.0)CYCLE
    JJ=JJ+1
    X2=PARAM(J)%XY(1,1); Y2=PARAM(J)%XY(1,2)
    GAMMA=UTL_KRIGING_GETGAMMA(X1,Y1,X2,Y2,RANGE,SILL,NUGGET,PEST_KTYPE)
    GAMMA=SILL-GAMMA
    QPP(II,JJ)=1.0D0/(2.0D0*SQRT(GAMMA))
   ENDDO
 
  ENDIF
 ENDDO
 
 !## compute plausibility value
 IF(LPJ)THEN
 
  !## multiply with residual pilotpoints, homogeneous criterion
  ALLOCATE(AQPP(NP)); AQPP=0.0
  JJ=0; DO J=1,SIZE(PARAM)
 
   !## skip zero-zones, inactive parameters/groupmembers
   IF(PARAM(J)%NODES.LE.0.OR.PARAM(J)%IACT.NE.1.OR.PARAM(J)%IGROUP.LE.0)CYCLE
   
   JJ=JJ+1
   
   II=0; DO I=1,SIZE(PARAM)
 
    !## skip zero-zones, inactive parameters/groupmembers
    IF(PARAM(I)%NODES.LE.0.OR.PARAM(I)%IACT.NE.1.OR.PARAM(I)%IGROUP.LE.0)CYCLE
    
    II=II+1
    
    ALPHA=0.0D0-PARAM(I)%ALPHA(1)
    AQPP(JJ)=AQPP(JJ)+ALPHA*QPP(JJ,II)
 
   ENDDO
  ENDDO
 
  !## multiply with residual pilotpoints, homogeneous criterion
  PJ=0.0D0
  II=0; DO I=1,SIZE(PARAM)
   !## skip zero-zones, inactive parameters/groupmembers
   IF(PARAM(I)%NODES.LE.0.OR.PARAM(I)%IACT.NE.1.OR.PARAM(I)%IGROUP.LE.0)CYCLE
   II=II+1
   ALPHA=0.0D0-PARAM(I)%ALPHA(1)
   PJ=PJ+AQPP(II)*ALPHA
  ENDDO
 
  DEALLOCATE(AQPP)
 
 ENDIF
 
 END SUBROUTINE PEST_GETQPP
 
 !###====================================================================
 SUBROUTINE PEST_BATCHFILES()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,N,IUBAT,S1,S2
 REAL(KIND=8) :: Z,WW,H,DHW

 DO I=1,PEST_NBATCH
  CALL IMOD_UTL_PRINTTEXT(' Executing:'//TRIM(PEST_IBATCH(I)%BATCHFILE),0)
  CALL SYSTEM(PEST_IBATCH(I)%BATCHFILE)
  CALL IMOD_UTL_PRINTTEXT(' Reading  :'//TRIM(PEST_IBATCH(I)%OUTFILE),0)
  IUBAT=IMOD_UTL_GETUNIT(); OPEN(IUBAT,FILE=PEST_IBATCH(I)%OUTFILE,STATUS='OLD')
  READ(IUBAT,*) N

  S1=SIZE(MSR%DH,1)-1; S2=SIZE(MSR%DH,2)
  IF(PEST_NOBS+N.GT.S2)THEN

   ALLOCATE(MSR%DH_DUMMY(0:S1,PEST_NOBS+N))
   DO K=0,S1; DO J=1,S2; MSR%DH_DUMMY(K,J)=MSR%DH(K,J); ENDDO; ENDDO
   DEALLOCATE(MSR%DH); MSR%DH=>MSR%DH_DUMMY

   ALLOCATE(MSR%W_DUMMY(PEST_NOBS+N))
   DO J=1,S2; MSR%W_DUMMY(J)=MSR%W(J); ENDDO
   DEALLOCATE(MSR%W); MSR%W=>MSR%W_DUMMY

   ALLOCATE(MSR%X_DUMMY(PEST_NOBS+N))
   DO J=1,S2; MSR%X_DUMMY(J)=MSR%X(J); ENDDO
   DEALLOCATE(MSR%X); MSR%X=>MSR%X_DUMMY

   ALLOCATE(MSR%Y_DUMMY(PEST_NOBS+N))
   DO J=1,S2; MSR%Y_DUMMY(J)=MSR%Y(J); ENDDO
   DEALLOCATE(MSR%Y); MSR%Y=>MSR%Y_DUMMY

   ALLOCATE(MSR%L_DUMMY(PEST_NOBS+N))
   DO J=1,S2; MSR%L_DUMMY(J)=MSR%L(J); ENDDO
   DEALLOCATE(MSR%L); MSR%L=>MSR%L_DUMMY

   ALLOCATE(MSR%CLABEL_DUMMY(PEST_NOBS+N))
   DO J=1,S2; MSR%CLABEL_DUMMY(J)=MSR%CLABEL(J); ENDDO
   DEALLOCATE(MSR%CLABEL); MSR%CLABEL=>MSR%CLABEL_DUMMY

  ENDIF

  DO J=1,N
   PEST_NOBS=PEST_NOBS+1
   READ(IUBAT,*) Z,WW,H
   IF(WW.LE.0.0D0)THEN
    WW=0.0D0
   ELSE
    !## weigh=1/variance
    WW=1.0D0/SQRT(WW)
   ENDIF
   MSR%X(PEST_NOBS)=0.0D0
   MSR%Y(PEST_NOBS)=0.0D0
   MSR%L(PEST_NOBS)=0
   MSR%CLABEL(PEST_NOBS)=''
   MSR%DH(PEST_IGRAD,PEST_NOBS)=(H-Z) !## calculated - measured
   MSR%W(PEST_NOBS)            = WW
   DHW                     = WW*((H-Z)**2.0D0)
   TJ                      = TJ+DHW
   IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(30X,I10,6F15.7)') J,Z,H,DHW,WW*Z,WW*(H-Z),WW
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
  CASE ('KD','KH','SC','AF','VA','SY')
   IF(PARAM(IP)%ILS.LE.0.OR.PARAM(IP)%ILS.GT.NLAY)   &
    CALL IMOD_UTL_PRINTTEXT('Parameter '//TRIM(IMOD_UTL_ITOS(IP))//': ILS exceeds NLAY',2)
  CASE ('VC','KV')
   IF(PARAM(IP)%ILS.LE.0.OR.PARAM(IP)%ILS.GT.NLAY-1) &
    CALL IMOD_UTL_PRINTTEXT('Parameter '//TRIM(IMOD_UTL_ITOS(IP))//': ILS exceeds NLAY-1',2)
  CASE ('EP','RE')
   IF(PARAM(IP)%ILS.NE.1) &
    CALL IMOD_UTL_PRINTTEXT('Parameter '//TRIM(IMOD_UTL_ITOS(IP))//': ILS need to be equal to 1',2)
 END SELECT

 !## scaling
 IF(PARAM(IP)%LOG)THEN
  IF(PARAM(IP)%DELTA.EQ.1.0)CALL IMOD_UTL_PRINTTEXT('You can not specify delta alpha eq 1.0 for log-transformed parameters',2)
  IF(PARAM(IP)%MIN  .EQ.0.0)CALL IMOD_UTL_PRINTTEXT('You can not specify minimal value eq 0.0 for log-transformed parameters',2)
  PARAM(IP)%INI  =LOG(PARAM(IP)%INI)
  PARAM(IP)%MIN  =LOG(PARAM(IP)%MIN)
  PARAM(IP)%MAX  =LOG(PARAM(IP)%MAX)
  PARAM(IP)%DELTA=LOG(PARAM(IP)%DELTA)
 ENDIF

 END SUBROUTINE PEST1CHK

 !###========================================================================
 DOUBLE PRECISION FUNCTION PEST_FIND_DET(JQJ,N)
 !###========================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 DOUBLE PRECISION,DIMENSION(N,N) :: JQJ
 DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE :: MATRIX
 DOUBLE PRECISION :: M, TEMP
 INTEGER :: I, J, K, L
 LOGICAL :: DETEXISTS = .TRUE.

 ALLOCATE(MATRIX(N,N))
 MATRIX=JQJ
 
 L = 1
 !## convert to upper triangular form
 DO K = 1, N-1
  IF (MATRIX(K,K) == 0) THEN
   DETEXISTS = .FALSE.
   DO I = K+1, N
    IF (MATRIX(I,K) /= 0) THEN
     DO J = 1, N
      TEMP = MATRIX(I,J)
      MATRIX(I,J)= MATRIX(K,J)
      MATRIX(K,J) = TEMP
     END DO
     DETEXISTS = .TRUE.
     L=-L
     EXIT
    ENDIF
   END DO
   IF (DETEXISTS .EQV. .FALSE.) THEN
    PEST_FIND_DET = 0.0
    DEALLOCATE(MATRIX)
    RETURN
   END IF
  ENDIF
  DO J = K+1, N
   M = MATRIX(J,K)/MATRIX(K,K)
   DO I = K+1, N
    MATRIX(J,I) = MATRIX(J,I) - M*MATRIX(K,I)
   END DO
  END DO
 END DO
    
 !## calculate determinant by finding product of diagonal elements
 PEST_FIND_DET = L
 DO I = 1, N
  PEST_FIND_DET = PEST_FIND_DET * MATRIX(I,I)
 END DO

 DEALLOCATE(MATRIX)
    
 END FUNCTION PEST_FIND_DET
 
 !###========================================================================
 SUBROUTINE RED1EIGSRT(D,A,N,NP)
 !###========================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,NP
 REAL,DIMENSION(NP),INTENT(INOUT) :: D
 REAL,DIMENSION(NP,NP),INTENT(INOUT) :: A
 INTEGER :: I,J,K
 REAL(KIND=8) :: P

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
 SUBROUTINE RED1EIGSRT_DBL(D,A,N,NP)
 !###========================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,NP
 DOUBLE PRECISION,DIMENSION(NP),INTENT(INOUT) :: D
 DOUBLE PRECISION,DIMENSION(NP,NP),INTENT(INOUT) :: A
 INTEGER :: I,J,K
 DOUBLE PRECISION :: P

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

 END SUBROUTINE RED1EIGSRT_DBL
 
!###========================================================================
 SUBROUTINE RED1TRED2(A,N,NP,D,E)
!###========================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,NP
 REAL,DIMENSION(NP,NP),INTENT(INOUT) :: A
 REAL,DIMENSION(NP),INTENT(INOUT) :: D,E
 INTEGER :: I,J,K,L
 REAL(KIND=8) :: F,G,H,HH,SCALE

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
 SUBROUTINE RED1TRED2_DBL(A,N,NP,D,E)
 !###========================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,NP
 DOUBLE PRECISION,DIMENSION(NP,NP),INTENT(INOUT) :: A
 DOUBLE PRECISION,DIMENSION(NP),INTENT(INOUT) :: D,E
 INTEGER :: I,J,K,L
 DOUBLE PRECISION :: F,G,H,HH,SCALE

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

 END SUBROUTINE RED1TRED2_DBL

 !###========================================================================
 REAL FUNCTION PYTHAG(A,B)
 !###========================================================================
 IMPLICIT NONE
 REAL(KIND=8),INTENT(IN) :: A,B
 REAL(KIND=8) :: ABSA,ABSB

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
 REAL FUNCTION PYTHAG_DBL(A,B)
 !###========================================================================
 IMPLICIT NONE
 DOUBLE PRECISION,INTENT(IN) :: A,B
 DOUBLE PRECISION :: ABSA,ABSB

 ABSA=ABS(A)
 ABSB=ABS(B)
 IF(ABSA.GT.ABSB)THEN
  PYTHAG_DBL=ABSA*SQRT(1.+(ABSB/ABSA)**2.)
 ELSE
  IF(ABSB.EQ.0.)THEN
   PYTHAG_DBL=0.
  ELSE
   PYTHAG_DBL=ABSB*SQRT(1.+(ABSA/ABSB)**2.)
  ENDIF
 ENDIF

 END FUNCTION PYTHAG_DBL
 
 !###========================================================================
 SUBROUTINE RED1TQLI(D,E,N,NP,A)
 !###========================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,NP
 REAL(KIND=8),DIMENSION(NP),INTENT(INOUT) :: D,E
 REAL(KIND=8),DIMENSION(NP,NP),INTENT(INOUT) :: A
 INTEGER :: I,ITER,K,L,M
 REAL(KIND=8) :: B,C,DD,F,G,P,R,S

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
   R=PYTHAG(G,1.0D0)
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

 !###========================================================================
 SUBROUTINE RED1TQLI_DBL(D,E,N,NP,A)
 !###========================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,NP
 DOUBLE PRECISION,DIMENSION(NP),INTENT(INOUT) :: D,E
 DOUBLE PRECISION,DIMENSION(NP,NP),INTENT(INOUT) :: A
 INTEGER :: I,ITER,K,L,M
 DOUBLE PRECISION :: B,C,DD,F,G,P,R,S

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
   R=PYTHAG_DBL(G,1.0D0)
   G=D(M)-D(L)+E(L)/(G+SIGN(R,G))
   S=1.
   C=1.
   P=0.
   DO I=M-1,L,-1
    F=S*E(I)
    B=C*E(I)
    R=PYTHAG_DBL(F,G)
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

 END SUBROUTINE RED1TQLI_DBL

 !###====================================================================
 SUBROUTINE PEST1CLOSELOGFILES()
 !###====================================================================
 IMPLICIT NONE
 LOGICAL :: LEX
 
 IF(IUPESTOUT.GT.0)THEN
  INQUIRE(UNIT=IUPESTOUT,OPENED=LEX)
  IF(LEX)CLOSE(IUPESTOUT); IUPESTOUT=0
 ENDIF
 IF(IUPESTPROGRESS.GT.0)THEN
  INQUIRE(UNIT=IUPESTPROGRESS,OPENED=LEX)
  IF(LEX)CLOSE(IUPESTPROGRESS); IUPESTPROGRESS=0
 ENDIF
 IF(IUPESTEFFICIENCY.GT.0)THEN
  INQUIRE(UNIT=IUPESTEFFICIENCY,OPENED=LEX)
  IF(LEX)CLOSE(IUPESTEFFICIENCY); IUPESTEFFICIENCY=0
 ENDIF
 IF(IUPESTSENSITIVITY.GT.0)THEN
  INQUIRE(UNIT=IUPESTSENSITIVITY,OPENED=LEX)
  IF(LEX)CLOSE(IUPESTSENSITIVITY); IUPESTSENSITIVITY=0
 ENDIF
 IF(IUPESTRUNFILE.GT.0)THEN
  INQUIRE(UNIT=IUPESTRUNFILE,OPENED=LEX)
  IF(LEX)CLOSE(IUPESTRUNFILE); IUPESTRUNFILE=0
 ENDIF
 IF(IUPESTRESIDUAL.GT.0)THEN
  INQUIRE(UNIT=IUPESTRESIDUAL,OPENED=LEX)
  IF(LEX)CLOSE(IUPESTRESIDUAL); IUPESTRESIDUAL=0
 ENDIF
 
 END SUBROUTINE PEST1CLOSELOGFILES

 !###====================================================================
 SUBROUTINE PEST1APPENDLOGFILE(root)
 !###====================================================================
 IMPLICIT NONE
 character(len=*),intent(in) :: root

 CALL PEST1CLOSELOGFILES()

 CALL PESTOPENFILE(IUPESTOUT,'log_pest_','txt',1,root)
 CALL PESTOPENFILE(IUPESTPROGRESS,'log_pest_progress_','txt',1,root)
 CALL PESTOPENFILE(IUPESTEFFICIENCY,'log_pest_efficiency_','txt',1,root)
 CALL PESTOPENFILE(IUPESTSENSITIVITY,'log_pest_sensitivity_','txt',1,root)
 CALL PESTOPENFILE(IUPESTRUNFILE,'log_pest_runfile_','txt',1,root)
  
 END SUBROUTINE PEST1APPENDLOGFILE

END MODULE MOD_PEST


