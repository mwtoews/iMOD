!!  Copyright (C) Stichting Deltares, 2005-2019.
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
MODULE MOD_IPEST_GLM

USE WINTERACTER
USE MOD_IDF_PAR
USE MOD_IDF, ONLY : IDFNULLIFY,IDFALLOCATEX,IDFWRITE
USE IMODVAR, ONLY : DP_KIND
USE MOD_PMANAGER_PAR, ONLY : PEST,SIM,PARAM,PRJNLAY,PBMAN,PRJNPER
USE MOD_UTL, ONLY  : UTL_GETUNIT,UTL_CAP,ITOS,RTOS,UTL_GETMED,UTL_CREATEDIR,UTL_GOODNESS_OF_FIT,UTL_NASH_SUTCLIFFE
USE MOD_IPEST_GLM_PAR
USE MOD_LUDCMP

CONTAINS

 !#####=================================================================
 SUBROUTINE IPEST_GLM_MAIN(DIR,MNAME,IBATCH)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),INTENT(IN) :: DIR,MNAME
 REAL(KIND=DP_KIND) :: RFIT,TNSC,LAMBDA
 INTEGER :: I,J,ITER,N,M,IFLAGS,IEXCOD,IGRAD,ILIN,NCPU,IU,NDONE,IX
 INTEGER,DIMENSION(2) :: IDPROC
 
 PEST%PE_SCALING=PEST%PE_SCALING-1
 LSENS=.FALSE.; IF(PEST%PE_MXITER.EQ.0)THEN; LSENS=.TRUE.; PEST%PE_MXITER=1; ENDIF
 
 !## allocate memory for running the models
 N=0 ; DO I=1,SIZE(PEST%PARAM)
  !## associated parameters to existing groups inactive for gradient computation
  IF(PEST%PARAM(I)%PIGROUP.LT.0)PEST%PARAM(I)%PACT=2; IF(PEST%PARAM(I)%PACT.EQ.1)N=N+1
 ENDDO
 CALL IPEST_GLM_ALLOCATE(N)
 
 !## set initial values for alpha()
 DO I=1,SIZE(PEST%PARAM); IF(.NOT.IPEST_GLM_CHK(I,IBATCH))RETURN; ENDDO

 !## open output files
 CALL UTL_CREATEDIR(TRIM(DIR)//'\IPEST')
 IF(PBMAN%PTEST.EQ.0)THEN
  IUPESTOUT=UTL_GETUNIT();         OPEN(IUPESTOUT,        FILE=TRIM(DIR)//'\IPEST\LOG_PEST.TXT'            ,STATUS='UNKNOWN',ACTION='WRITE')
  IUPESTPROGRESS=UTL_GETUNIT();    OPEN(IUPESTPROGRESS,   FILE=TRIM(DIR)//'\IPEST\LOG_PEST_PROGRESS.TXT'   ,STATUS='UNKNOWN',ACTION='WRITE')
  IUPESTEFFICIENCY=UTL_GETUNIT();  OPEN(IUPESTEFFICIENCY, FILE=TRIM(DIR)//'\IPEST\LOG_PEST_EFFICIENCY.TXT' ,STATUS='UNKNOWN',ACTION='WRITE')
  IUPESTSENSITIVITY=UTL_GETUNIT(); OPEN(IUPESTSENSITIVITY,FILE=TRIM(DIR)//'\IPEST\LOG_PEST_SENSITIVITY.TXT',STATUS='UNKNOWN',ACTION='WRITE')
  IUPESTRUNFILE=UTL_GETUNIT();     OPEN(IUPESTRUNFILE,    FILE=TRIM(DIR)//'\IPEST\LOG_PEST_RUNFILE.TXT'    ,STATUS='UNKNOWN',ACTION='WRITE')
  
  WRITE(IUPESTOUT,'(A)') 'Parameters'
  WRITE(IUPESTOUT,'(A2,1X,A5,2(1X,A5),5(1X,A15),3A10,2A15)') 'AC','PTYPE','ILS','IZN','INITIAL','DELTA','MINIMUM','MAXIMUM','FADJ','IGROUP','LTRANS','NODES','ACRONYM','PPRIOR'
  
  DO I=1,SIZE(PEST%PARAM); CALL IPEST_GLM_ECHO_PARAMETERS(I); ENDDO
  
  WRITE(IUPESTEFFICIENCY,'(6A15)') 'TJ','SQRT(TJ)','MEAN(TJ)','RMSE','ADJUSTMENTS','EFFICIENCY'
  WRITE(IUPESTEFFICIENCY,'(6A15)') '(L2)','(L)','(L2)','(L)','(%)','-'
  WRITE(IUPESTSENSITIVITY,'(A)')   'Sensitivity (%):'

  N=0; DO J=1,SIZE(PEST%PARAM)
   IF(ABS(PEST%PARAM(J)%PACT).EQ.1.AND.PEST%PARAM(J)%PIGROUP.GT.0)N=N+1
  ENDDO

  CALL IPEST_GLM_WRITEHEADER('Iteration      ',N,IUPESTSENSITIVITY)
 ELSE
  IUPESTOUT=UTL_GETUNIT(); OPEN(IUPESTOUT,FILE=TRIM(DIR)//'\IPEST\LOG_PEST_TEST.CSV',STATUS='UNKNOWN',ACTION='WRITE')
 ENDIF
 
 !## set linesearch-runbatch-files
 DO I=1,PBMAN%NLINESEARCH; RNL(I)=TRIM(DIR)//'\RUN_L#'//TRIM(ITOS(I))//'.BAT'; LPARAM(I)=I; ENDDO

 !## set gradient-runbatch-files - equal to number of parameter to be estimated
 N=0; DO I=1,SIZE(PEST%PARAM)
  !## parameter
  IF(PEST%PARAM(I)%PACT.EQ.0.OR.PEST%PARAM(I)%PIGROUP.LT.0)CYCLE; N=N+1; RNG(N)=TRIM(DIR)//'\RUN_P#'//TRIM(ITOS(I))//'.BAT'; GPARAM(N)=I
 ENDDO

 !## open files and get total maximum number of observations
 N=0; DO I=1,SIZE(PEST%MEASURES)
  IU=UTL_GETUNIT(); OPEN(IU,FILE=PEST%MEASURES(I)%IPFNAME,STATUS='OLD',ACTION='READ')
  READ(IU,*) M; N=N+M; CLOSE(IU)
 ENDDO

 !## allocate memory
 IF(.NOT.IPEST_GLM_ALLOCATEMSR(N))RETURN
  
 IF(PBMAN%PTEST.EQ.1)THEN; CALL IPEST_GLM_TEST(DIR,MNAME,IBATCH); RETURN; ENDIF

 IUPESTRESIDUAL=0; CALL WMESSAGEENABLE(TIMEREXPIRED,1); MSR%PJ=HUGE(1.0D0); ITER=0

 !## start optimization cycle
 DO 

   !## executes on commandtool such that commands alike 'dir' etc. works
   IFLAGS=PROCBLOCKED; IF(PBMAN%CMDHIDE.EQ.1)IFLAGS=IFLAGS+PROCSILENT

!  !## executes lamdba-testing on commandtool such that commands alike 'dir' etc. works
!  IFLAGS=0; IF(PBMAN%CMDHIDE.EQ.1)IFLAGS=IFLAGS+PROCSILENT+PROCCMDPROC

  !## compute the initial simulation first (number of lamda-tests - initially this is one)
   
  IGRAD=0
  DO ILIN=1,PBMAN%NLINESEARCH

   IF(IUPESTRESIDUAL.GT.0)CLOSE(IUPESTRESIDUAL)
   IUPESTRESIDUAL=UTL_GETUNIT(); OPEN(IUPESTRESIDUAL,FILE=TRIM(DIR)//'\IPEST\LOG_PEST_RESIDUAL_'//TRIM(ITOS(ITER))//'.TXT',STATUS='UNKNOWN',ACTION='WRITE')

   !## define update in pst file
   IF(.NOT.IPEST_GLM_PST(DIR,MNAME,ILIN,LPARAM(ILIN),'L'))THEN
    CALL IPEST_GLM_ERROR(IBATCH,'ERROR CREATED PST1 FILE FOR L#'//TRIM(ITOS(LPARAM(ILIN)))); RETURN
   ENDIF
   !## run model
   I=WINFOERROR(1); CALL IOSCOMMAND(TRIM(RNL(ILIN)),IFLAGS=IFLAGS,IEXCOD=IEXCOD)
   IF(WINFOERROR(1).EQ.ERROSCOMMAND)THEN; CALL IPEST_GLM_ERROR(IBATCH,'FAILED TO START MODEL L#'//TRIM(ITOS(LPARAM(ILIN)))); RETURN; ENDIF
   IF(IEXCOD.NE.0)THEN; CALL IPEST_GLM_ERROR(IBATCH,'ERROR OCCURED RUNNING MODEL L#'//TRIM(ITOS(LPARAM(ILIN)))); RETURN; ENDIF

   !## get objective function value
   IF(.NOT.IPEST_GLM_GETJ(DIR,ILIN,LPARAM(ILIN),'L',IBATCH,TNSC))RETURN
   CALL IPEST_GLM_PROGRESS(ITER,IGRAD,ILIN,'L')
   FLUSH(IUPESTPROGRESS)
   !## evaluate whether in line-search objective function value is reduced compared to previous objective function value
   IF(MSR%TJ.LE.MSR%PJ)EXIT
!   !## meerdere line-searches tegelijkertijd ---
!   IF(.NOT.IPEST_GLM_UPGRADEVECTOR(0.5D0,.FALSE.,ITER))THEN
!    CALL IPEST_GLM_ERROR(IBATCH,'STOP ERROR IN LINE-SEARCH'); RETURN
!   ENDIF
  ENDDO
  
! !## save alphas for history-logging
! J=0; DO IP1=1,SIZE(PEST%PARAM)
!  IF(PEST%PARAM(IP1)%PACT.EQ.0)CYCLE

!  IF(PEST%PARAM(IP1)%PLOG.EQ.1)THEN
!   PEST%PARAM(IP1)%ALPHA_HISTORY(ITER)=EXP(PEST%PARAM(IP1)%ALPHA(1))
!  ELSE
!   PEST%PARAM(IP1)%ALPHA_HISTORY(ITER)=PEST%PARAM(IP1)%ALPHA(1)
!  ENDIF
  
!  !## active parameter
!  IF(PEST%PARAM(IP1)%PACT.EQ.1)THEN
!   !## store final gradient
!   J=J+1; U(J)=PEST%PARAM(IP1)%ALPHA(1)-PEST%PARAM(IP1)%ALPHA(2)
!  ENDIF
  
! ENDDO
  
  WRITE(IUPESTOUT,'(/A)') 'Best Residual Value      : '//TRIM(RTOS(MSR%TJ-MSR%RJ,'G',7))
  WRITE(IUPESTOUT,'(A)')  'Best Plausibility Value  : '//TRIM(RTOS(MSR%RJ,'G',7))
  WRITE(IUPESTOUT,'(A)')  'Total Objective Value    : '//TRIM(RTOS(MSR%TJ,'G',7))
  WRITE(IUPESTOUT,'(A)')  'Mean Objective Value     : '//TRIM(RTOS(MSR%TJ/REAL(MSR%NOBS,8),'G',7))
  RFIT=UTL_GOODNESS_OF_FIT(GF_H,GF_O,MSR%NOBS)
  WRITE(IUPESTOUT,'( A)') 'Goodness of Fit          : '//TRIM(RTOS(RFIT,'G',7))
  RFIT=UTL_NASH_SUTCLIFFE(GF_H,GF_O,MSR%NOBS)
  WRITE(IUPESTOUT,'( A)') 'Nash Sutcliffe (total)   : '//TRIM(RTOS(RFIT,'G',7))
  WRITE(IUPESTOUT,'( A)') 'Nash Sutcliffe (measures): '//TRIM(RTOS(RFIT,'G',7))
  WRITE(IUPESTOUT,'( A)') 'Number of Observations   : '//TRIM(ITOS(MSR%NOBS))

  !## initial lambda
  IF(ITER.EQ.0)THEN
   LAMBDA=MSR%TJ/DBLE(2.0D0*MSR%NOBS); LAMBDA=LOG10(LAMBDA); IX=FLOOR(LAMBDA); LAMBDA=10.0D0**IX
  ENDIF

  WRITE(IUPESTOUT,'(/A/)') '*** Next Outer Iteration ***'
  
  !## "melt" all parameters for next cycle
  DO  I=1,SIZE(PEST%PARAM); IF(PEST%PARAM(I)%PACT.EQ.-1)PEST%PARAM(I)%PACT=1; ENDDO

  !## select correct linesearch result and update alphas
  ILIN=MIN(ILIN,PBMAN%NLINESEARCH)
  IF(.NOT.IPEST_GLM_NEXT(ITER,ILIN,IBATCH))RETURN

  FLUSH(IUPESTOUT); FLUSH(IUPESTPROGRESS); FLUSH(IUPESTEFFICIENCY); FLUSH(IUPESTSENSITIVITY); FLUSH(IUPESTRUNFILE)

  !## next cycle
  ITER=ITER+1; IF(ITER.GT.PEST%PE_MXITER)EXIT

  !## executes on commandtool such that commands alike 'dir' etc. works
  IFLAGS=0; IF(PBMAN%CMDHIDE.EQ.1)IFLAGS=IFLAGS+PROCSILENT+PROCCMDPROC

  !## start finite difference approximation for parameters
  CALL WMESSAGETIMER(NCSECS,IREPEAT=1); ILIN=0; IGRAD=0; NCPU=0; IPROC=0; ISTATUS=-1
  
  !## start the sensitivity analyses
  DO 

   !## start processes
   DO

    !## find gradient simulation to be (re)carried out
    DO IGRAD=1,SIZE(RNG); IF(ISTATUS(IGRAD).EQ.-1)EXIT; ENDDO; IF(IGRAD.GT.SIZE(RNG))EXIT
    
    !## number of cpu full
    NCPU=NCPU+1

    !## adjust alpha for current igrad
    CALL IPEST_GLM_NEXTGRAD(GPARAM(IGRAD))
    !## define update in pst file
    IF(.NOT.IPEST_GLM_PST(DIR,MNAME,IGRAD,GPARAM(IGRAD),'P'))THEN
     CALL IPEST_GLM_ERROR(IBATCH,'ERROR CREATED PST1 FILE FOR P#'//TRIM(ITOS(GPARAM(IGRAD)))); RETURN
    ENDIF
 
    !## wait before starting a new process
    IF(PBMAN%NSWAIT.GT.0)CALL IOSWAIT(PBMAN%NSWAIT)
    !## clear error
    I=WINFOERROR(1); IDPROC=0; CALL IOSCOMMAND(TRIM(RNG(IGRAD)),IFLAGS=IFLAGS,IDPROC=IDPROC); IPROC(:,IGRAD)=IDPROC
    IF(WINFOERROR(1).EQ.ERROSCOMMAND)THEN
     CALL IPEST_GLM_ERROR(IBATCH,'FAILED TO START MODEL P#'//TRIM(ITOS(GPARAM(IGRAD)))); RETURN
    ENDIF
    ISTATUS(IGRAD)=1
    
    !## all started
    DO IGRAD=1,SIZE(RNG); IF(ISTATUS(IGRAD).EQ.-1)EXIT; ENDDO; IF(IGRAD.GT.SIZE(RNG))EXIT
    !## maximum number of cpu reached
    IF(NCPU.GE.PBMAN%NCPU)EXIT

   ENDDO
   
   !## evaluate processes that are finished
   NDONE=IPEST_GLM_EVALUATE(IBATCH,NCPU,DIR,ITER,RNG)
   !## finished if all succesfully completed
   IF(NDONE.EQ.SIZE(RNG))EXIT

  ENDDO
  
  !## determine new gradient
  IF(.NOT.IPEST_GLM_GRADIENT(IBATCH,ITER,LAMBDA))EXIT

 ENDDO
 CALL WMESSAGETIMER(0); CALL WMESSAGEENABLE(TIMEREXPIRED,0)
 
 END SUBROUTINE IPEST_GLM_MAIN

 !###====================================================================
 INTEGER FUNCTION IPEST_GLM_EVALUATE(IBATCH,NCPU,DIR,ITER,RN)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH,ITER
 INTEGER,INTENT(INOUT) :: NCPU
 CHARACTER(LEN=*),INTENT(IN),DIMENSION(:) :: RN
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: ITYPE,N,IGRAD,JGRAD,IEXCOD
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER,DIMENSION(2) :: IDPROC
 INTEGER :: ILIN,NDONE
 REAL(KIND=DP_KIND) :: F,TNSC
 
 ILIN=0
 
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  !## timer expired
  IF(ITYPE.EQ.TIMEREXPIRED)THEN
   N=0; DO JGRAD=1,SIZE(RN) !G)
    !## all handled process
    IF(IPROC(1,JGRAD)+IPROC(2,JGRAD).EQ.0)CYCLE
    !## check running status
    IDPROC=IPROC(:,JGRAD); CALL IOSCOMMANDCHECK(IDPROC,ISTATUS(JGRAD),IEXCOD=IEXCOD)
    !## stopped running
    IF(ISTATUS(JGRAD).EQ.0)THEN
     !## error occured 
     IF(IEXCOD.NE.0)THEN
      CALL IPEST_GLM_ERROR(IBATCH,'ERROR OCCURED RUNNING MODEL P#'//TRIM(ITOS(GPARAM(JGRAD))))
      !## try again - need to run again
      ISTATUS(JGRAD)=-1
     ENDIF
     !## set part of objective function
     IF(ISTATUS(JGRAD).EQ.0)THEN
      IF(.NOT.IPEST_GLM_GETJ(DIR,JGRAD,GPARAM(JGRAD),'P',IBATCH,TNSC))RETURN
      !## write echo
      CALL IPEST_GLM_PROGRESS(ITER,JGRAD,ILIN,'P'); FLUSH(IUPESTPROGRESS)
     ENDIF
     !## reset running handle proces
     IPROC(:,JGRAD)=0
     !## release cpu so another can be started
     NCPU=NCPU-1
    ELSE
     !# still running
     N=N+1
    ENDIF
   ENDDO   
   !## how much done?
   NDONE=0; DO IGRAD=1,SIZE(RN); IF(ISTATUS(IGRAD).EQ.0)NDONE=NDONE+1; ENDDO
!   NDONE=0; DO IGRAD=1,SIZE(RNG); IF(ISTATUS(IGRAD).EQ.0)NDONE=NDONE+1; ENDDO
   F=DBLE(NDONE)*100.0D0/DBLE(SIZE(RNG))
   WRITE(6,'(A)') '+Still running '//TRIM(ITOS(N))//'; models completed: '//TRIM(RTOS(F,'F',2))//'% (total '// &
       TRIM(ITOS(NDONE))//' out of '//TRIM(ITOS(SIZE(RN)))//' simulations)'
!       TRIM(ITOS(NDONE))//' out of '//TRIM(ITOS(SIZE(RNG)))//' simulations)'
   !## nothing running anymore
   IF(N.EQ.0)EXIT
   !## start another one as a proces has been stopped and there is still one waiting in the que
   IF(NCPU.LT.PBMAN%NCPU.AND.NDONE.LT.SIZE(RN))EXIT
!   IF(NCPU.LT.PBMAN%NCPU.AND.NDONE.LT.SIZE(RNG))EXIT
  ENDIF
 ENDDO
 
 IPEST_GLM_EVALUATE=NDONE
 
 END FUNCTION IPEST_GLM_EVALUATE
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_ERROR(IBATCH,TXT)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),INTENT(IN) :: TXT
 
 IF(IBATCH.EQ.1)WRITE(*,'(/A/)') TRIM(TXT)
 IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(TXT),'Error')
 WRITE(IUPESTOUT,'(/A/)') TRIM(TXT)
 
 END SUBROUTINE IPEST_GLM_ERROR
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_TEST(DIR,MNAME,IBATCH)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,MNAME
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: II,I,J,K,IFLAGS,ILIN,NINT,IPARAM
 REAL(KIND=DP_KIND) :: TNSC
 REAL(KIND=DP_KIND),DIMENSION(6) :: XSTAT
 REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: IP,PVAL
 TYPE(IDFOBJ) :: IDF
 
 WRITE(IUPESTOUT,'(99A15)') ('P('//TRIM(ITOS(I))//'),',I=1,SIZE(RNG)),'MEASURE_J','PARAMETER_J','TOTAL_J','MEAN_J','GOODNESS_OF_FIT','NASH_SUTCLIFFE'
 IUPESTRESIDUAL=0
 
 !## set delta
 DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PACT.EQ.0)CYCLE
  IF(PEST%PARAM(I)%PLOG.EQ.1)THEN
   PEST%PARAM(I)%PDELTA=LOG(2.0D0)
   PEST%PARAM(I)%PINI  =LOG(0.0625D0)
   PEST%PARAM(I)%PMIN  =LOG(0.0100D0)
   PEST%PARAM(I)%PMAX  =LOG(100.00D0)
  ELSE
   PEST%PARAM(I)%PDELTA= 2.0D0
   PEST%PARAM(I)%PINI  = 0.0625D0
   PEST%PARAM(I)%PMIN  = 0.0100D0
   PEST%PARAM(I)%PMAX  = 100.00D0
  ENDIF
 ENDDO

 !## number of intervals per parameter
 NINT=9

 ALLOCATE(IP(SIZE(RNG)),PVAL(SIZE(RNG))); IP=1.0D0; IP(1)=0.0D0

 !## executes on commandtool such that commands alike 'dir' etc. works
 IFLAGS=PROCBLOCKED; IF(PBMAN%CMDHIDE.EQ.1)IFLAGS=IFLAGS+PROCSILENT; ILIN=1

MAINLOOP: DO

  !# what set of parameters
  I=1; DO
   IP(I)=IP(I)+1.0D0
   IF(IP(I).LE.NINT)THEN
    EXIT
   ELSE
    IP(I)=1.0D0; I=I+1; IF(I.GT.SIZE(IP))EXIT MAINLOOP
   ENDIF
  ENDDO
  WRITE(*,*) IP

  !## reset all alpha's
  PEST%PARAM%ALPHA(1)=PEST%PARAM%ALPHA(2)

  IPARAM=0; DO I=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(I)%PACT.EQ.0)CYCLE; IPARAM=IPARAM+1
   DO J=1,SIZE(PEST%PARAM)
    IF(ABS(PEST%PARAM(J)%PIGROUP).EQ.ABS(PEST%PARAM(I)%PIGROUP))THEN
     PEST%PARAM(J)%ALPHA(1)=PEST%PARAM(J)%PINI+IP(IPARAM)*PEST%PARAM(J)%PDELTA
    ENDIF
   ENDDO
  ENDDO
  !## define update in pst file
  IF(.NOT.IPEST_GLM_PST(DIR,MNAME,ILIN,LPARAM(ILIN),'L'))THEN; WRITE(*,'(/A/)') 'ERROR CREATED PST1 FILE FOR L#'//TRIM(ITOS(LPARAM(ILIN))); STOP; ENDIF
  !## run model
  I=WINFOERROR(1); CALL IOSCOMMAND(TRIM(RNL(ILIN)),IFLAGS=IFLAGS)
  IF(WINFOERROR(1).EQ.ERROSCOMMAND)THEN; WRITE(*,'(/A/)') 'FAILED TO START MODEL L#'//TRIM(ITOS(LPARAM(ILIN))); STOP; ENDIF
  !## get objective function value
  IF(.NOT.IPEST_GLM_GETJ(DIR,ILIN,LPARAM(ILIN),'L',IBATCH,TNSC))RETURN
  XSTAT(1)=MSR%TJ-MSR%RJ; XSTAT(2)=MSR%RJ; XSTAT(3)=MSR%TJ; XSTAT(4)=MSR%TJ/REAL(MSR%NOBS,8); XSTAT(5)=UTL_GOODNESS_OF_FIT(GF_H,GF_O,MSR%NOBS); XSTAT(6)=UTL_NASH_SUTCLIFFE(GF_H,GF_O,MSR%NOBS)
 
  IPARAM=0; DO I=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(I)%PACT.EQ.0)CYCLE; IPARAM=IPARAM+1
   DO J=1,SIZE(PEST%PARAM)
    IF(ABS(PEST%PARAM(J)%PIGROUP).EQ.ABS(PEST%PARAM(I)%PIGROUP))THEN
     IF(PEST%PARAM(J)%PLOG.EQ.0)THEN
      PVAL(IPARAM)=PEST%PARAM(J)%ALPHA(1)
     ELSE
      PVAL(IPARAM)=EXP(PEST%PARAM(J)%ALPHA(1))
     ENDIF
    ENDIF
   ENDDO
  ENDDO

  WRITE(IUPESTOUT,'(99F15.7)') (PVAL(I),I=1,SIZE(IP)),(XSTAT(I),I=1,6)

 ENDDO MAINLOOP
 CLOSE(IUPESTOUT)

 !## create idf
  IF(SIZE(IP).EQ.2)THEN
  
  CALL IDFNULLIFY(IDF)
  IDF%XMIN=LOG(0.0625D0); IDF%YMIN=LOG(0.0625D0)
  IDF%DX=LOG(2.0D0); IDF%DY=IDF%DX
  IDF%NODATA=-999.99D0
  IDF%XMAX=LOG(32.0D0); IDF%YMAX=LOG(32.0D0)
  IDF%NCOL=9; IDF%NROW=9
  IF(.NOT.IDFALLOCATEX(IDF))STOP
  
  IUPESTOUT=UTL_GETUNIT(); OPEN(IUPESTOUT,FILE=TRIM(DIR)//'\IPEST\LOG_PEST_TEST.CSV',STATUS='OLD',ACTION='READ')

  DO II=1,6
   READ(IUPESTOUT,*)
   IDF%X=IDF%NODATA
   DO I=IDF%NROW,1,-1
    DO J=1,IDF%NCOL
     READ(IUPESTOUT,'(99F15.7)') (PVAL(K),K=1,SIZE(IP)),(XSTAT(K),K=1,6)
     IDF%X(J,I)=XSTAT(II)
    ENDDO
   ENDDO
   IF(II.EQ.1)THEN; IDF%FNAME=TRIM(DIR)//'\IPEST\MEASURE_J.IDF'      ; IF(.NOT.IDFWRITE(IDF,IDF%FNAME,1))STOP; ENDIF  
   IF(II.EQ.2)THEN; IDF%FNAME=TRIM(DIR)//'\IPEST\PARAMETER_J.IDF'    ; IF(.NOT.IDFWRITE(IDF,IDF%FNAME,1))STOP; ENDIF  
   IF(II.EQ.3)THEN; IDF%FNAME=TRIM(DIR)//'\IPEST\TOTAL_J.IDF'        ; IF(.NOT.IDFWRITE(IDF,IDF%FNAME,1))STOP; ENDIF
   IF(II.EQ.4)THEN; IDF%FNAME=TRIM(DIR)//'\IPEST\AVERAGE_TOTAL_J.IDF'; IF(.NOT.IDFWRITE(IDF,IDF%FNAME,1))STOP; ENDIF
   IF(II.EQ.5)THEN; IDF%FNAME=TRIM(DIR)//'\IPEST\GOODNESS_OF_FIT.IDF'; IF(.NOT.IDFWRITE(IDF,IDF%FNAME,1))STOP; ENDIF
   IF(II.EQ.6)THEN; IDF%FNAME=TRIM(DIR)//'\IPEST\NASH_SUTCLIFF.IDF'  ; IF(.NOT.IDFWRITE(IDF,IDF%FNAME,1))STOP; ENDIF
   REWIND(IUPESTOUT)
  ENDDO
 ENDIF
 
 CLOSE(IUPESTOUT)
 
 END SUBROUTINE IPEST_GLM_TEST

 !###====================================================================
 LOGICAL FUNCTION IPEST_GLM_CHK(IP,IBATCH)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IP,IBATCH
 INTEGER :: I,N
 
 IPEST_GLM_CHK=.FALSE.
 
 DO I=1,SIZE(PARAM); IF(TRIM(PEST%PARAM(IP)%PPARAM).EQ.TRIM(PARAM(I)))EXIT; ENDDO
 IF(I.GT.SIZE(PARAM))THEN
  IF(IBATCH.EQ.1)THEN
   WRITE(*,'(/A)') 'Error can not recognize parameter type:'//TRIM(PEST%PARAM(IP)%PPARAM)
   WRITE(*,'(/A)') ' Choose from:'
   DO I=1,SIZE(PARAM); WRITE(*,'(A)') ' - '//TRIM(PARAM(I)); ENDDO
   WRITE(*,'(A)')
  ELSEIF(IBATCH.EQ.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error can not recognize parameter type:'//TRIM(PEST%PARAM(IP)%PPARAM),'Error')
  ENDIF
  RETURN
 ENDIF
 IF(PEST%PARAM(IP)%PMIN.GE.PEST%PARAM(IP)%PMAX)THEN
  IF(IBATCH.EQ.1)WRITE(*,'(/A/)') 'No proper parameter width defined for parameter '//TRIM(ITOS(IP))
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No proper parameter width defined for parameter '//TRIM(ITOS(IP)),'Error')
  RETURN
 ENDIF
 IF(PEST%PARAM(IP)%PINI.LT.PEST%PARAM(IP)%PMIN.OR.PEST%PARAM(IP)%PINI.GT.PEST%PARAM(IP)%PMAX)THEN
  IF(IBATCH.EQ.1)WRITE(*,'(/A/)') 'Parameter '//TRIM(ITOS(IP))//' outside parameter width'
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Parameter '//TRIM(ITOS(IP))//' outside parameter width','Error')
  RETURN
 ENDIF
 SELECT CASE (TRIM(PEST%PARAM(IP)%PPARAM))
  CASE ('KD','KH','SC','AF','VA','SY')
   IF(PEST%PARAM(IP)%PILS.LE.0.OR.PEST%PARAM(IP)%PILS.GT.PRJNLAY)THEN
    IF(IBATCH.EQ.1)WRITE(*,'(/A/)') 'Parameter '//TRIM(ITOS(IP))//': ILS exceeds NLAY'
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Parameter '//TRIM(ITOS(IP))//': ILS exceeds NLAY','Error')
    RETURN
   ENDIF
  CASE ('VC','KV')
   IF(PEST%PARAM(IP)%PILS.LE.0.OR.PEST%PARAM(IP)%PILS.GT.PRJNLAY-1)THEN
    IF(IBATCH.EQ.1)WRITE(*,'(/A/)') 'Parameter '//TRIM(ITOS(IP))//': ILS exceeds NLAY-1'
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Parameter '//TRIM(ITOS(IP))//': ILS exceeds NLAY-1','Error')
    RETURN
   ENDIF
  CASE ('EP','RE')
   IF(PEST%PARAM(IP)%PILS.NE.1)THEN
    IF(IBATCH.EQ.1)WRITE(*,'(/A/)') 'Parameter '//TRIM(ITOS(IP))//': ILS need to be equal to 1'
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Parameter '//TRIM(ITOS(IP))//': ILS need to be equal to 1','Error')
    RETURN
   ENDIF
 END SELECT

 !## scaling
 PEST%PARAM(IP)%CONVERTLOG=0
 IF(PEST%PARAM(IP)%PLOG.EQ.1)THEN
  IF(PEST%PARAM(IP)%PDELTA.EQ.1.0)WRITE(*,'(/A/)') 'You can not specify delta alpha eq 1.0 for log-transformed parameters'
  IF(PEST%PARAM(IP)%PMIN  .EQ.0.0)WRITE(*,'(/A/)') 'You can not specify minimal value eq 0.0 for log-transformed parameters'
  PEST%PARAM(IP)%PINI  =LOG(PEST%PARAM(IP)%PINI)
  PEST%PARAM(IP)%PMIN  =LOG(PEST%PARAM(IP)%PMIN)
  PEST%PARAM(IP)%PMAX  =LOG(PEST%PARAM(IP)%PMAX)
  PEST%PARAM(IP)%PDELTA=LOG(PEST%PARAM(IP)%PDELTA)
  PEST%PARAM(IP)%PPRIOR=LOG(PEST%PARAM(IP)%PPRIOR)
  PEST%PARAM(IP)%CONVERTLOG=1
 ENDIF
 PEST%PARAM(IP)%ALPHA(1)=PEST%PARAM(IP)%PINI !## current  alpha
 PEST%PARAM(IP)%ALPHA(2)=PEST%PARAM(IP)%PINI !## previous alpha
 ALLOCATE(PEST%PARAM(IP)%ALPHA_HISTORY(0:PEST%PE_MXITER)); PEST%PARAM(IP)%ALPHA_HISTORY=0.0D0
! ALLOCATE(PEST%PARAM(IP)%ALPHA_ERROR_VARIANCE(0:PEST%PE_MXITER)); PEST%PARAM(IP)%ALPHA_ERROR_VARIANCE=0.0D0
 N=SIZE(RNG); ALLOCATE(PEST%PARAM(IP)%GALPHA(N)); PEST%PARAM(IP)%GALPHA=0.0D0
 N=SIZE(RNL); ALLOCATE(PEST%PARAM(IP)%LALPHA(N)); PEST%PARAM(IP)%LALPHA=0.0D0

 PEST%PARAM(IP)%ACRONYM=ADJUSTR(PEST%PARAM(IP)%ACRONYM)
  
 IPEST_GLM_CHK=.TRUE.

 END FUNCTION IPEST_GLM_CHK
 
 !!###====================================================================
 !SUBROUTINE IPEST_GLM_APPLY_LHC()
 !!###====================================================================
 !IMPLICIT NONE
 !INTEGER :: I,J,N,SEED
 !REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: SLEGMIN,SLEGMAX,SLEGMEAN
 !
 !!## no monte carlo
 !IF(PBMAN%MC.EQ.0)RETURN
 !
 !N=0; DO J=1,SIZE(PEST%PARAM)
 ! IF(ABS(PEST%PARAM(J)%PACT).EQ.1.AND.PEST%PARAM(J)%PIGROUP.GT.0)N=N+1
 !ENDDO
 !
 !IF(ALLOCATED(LHC_TABLE))DEALLOCATE(LHC_TABLE)
 !ALLOCATE(SLEGMIN(N),SLEGMAX(N),SLEGMEAN(N),LHC_TABLE(N,N))
 !
 !N=0; DO I=1,SIZE(PEST%PARAM)
 ! IF(PEST%PARAM(I)%PACT.NE.1)CYCLE; N=N+1
 ! SLEGMIN(N) =PEST%PARAM(I)%ALPHA(2)-PEST%PARAM(I)%PDELTA
 ! SLEGMAX(N) =PEST%PARAM(I)%ALPHA(2)+PEST%PARAM(I)%PDELTA
 ! SLEGMEAN(N)=PEST%PARAM(I)%ALPHA(2)
 !ENDDO
 !
 !!## number of points/samples
 !SEED=2; CALL LHC(N,N,SEED,LHC_TABLE)
 !
 !!## fill in set of parameters
 !DO I=1,N
 ! SLEGMEAN(I)=0.5D0*(SLEGMAX(I)+SLEGMIN(I))
 ! DO J=1,N
 !  LHC_TABLE(I,J)=SLEGMIN(I)+(LHC_TABLE(I,J)*(SLEGMAX(I)-SLEGMIN(I)))
 ! ENDDO
 !ENDDO
 !
 !WRITE(*,'(A10,999F10.3)') 'MEAN',(SLEGMEAN(J),J=1,N)
 !WRITE(*,'(A10,999F10.3)') 'MIN',(SLEGMIN(J),J=1,N)
 !WRITE(*,'(A10,999F10.3)') 'MAX',(SLEGMAX(J),J=1,N)
 !WRITE(*,'(999A1)') ('-',I=1,N*11)
 !DO I=1,N
 ! WRITE(*,'(10X,999F10.3)') (LHC_TABLE(J,I),J=1,N)
 !ENDDO
 !
 !DEALLOCATE(SLEGMIN,SLEGMAX,SLEGMEAN)
 !
 !END SUBROUTINE IPEST_GLM_APPLY_LHC
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_RESET_PARAMETER()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I

 !## scaling
 DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PLOG.EQ.1)THEN
   PEST%PARAM(I)%PINI  =EXP(PEST%PARAM(I)%PINI)
   PEST%PARAM(I)%PMIN  =EXP(PEST%PARAM(I)%PMIN)
   PEST%PARAM(I)%PMAX  =EXP(PEST%PARAM(I)%PMAX)
   PEST%PARAM(I)%PDELTA=EXP(PEST%PARAM(I)%PDELTA)
   PEST%PARAM(I)%PPRIOR=EXP(PEST%PARAM(I)%PPRIOR)
  ENDIF
 ENDDO
 
 IF(IUPESTOUT.GT.0)        CLOSE(IUPESTOUT); IUPESTOUT=0
 IF(IUPESTPROGRESS.GT.0)   CLOSE(IUPESTPROGRESS); IUPESTPROGRESS=0
 IF(IUPESTEFFICIENCY.GT.0) CLOSE(IUPESTEFFICIENCY); IUPESTEFFICIENCY=0
 IF(IUPESTSENSITIVITY.GT.0)CLOSE(IUPESTSENSITIVITY); IUPESTSENSITIVITY=0
 IF(IUPESTRUNFILE.GT.0)    CLOSE(IUPESTRUNFILE); IUPESTRUNFILE=0
 IF(IUPESTRESIDUAL.GT.0)   CLOSE(IUPESTRESIDUAL); IUPESTRESIDUAL=0

 END SUBROUTINE IPEST_GLM_RESET_PARAMETER
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_ECHO_PARAMETERS(IP)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IP
 INTEGER :: I
 
 I=PEST%PARAM(IP)%PACT; IF(PEST%PARAM(IP)%PACT.EQ.2)I=1
 IF(PBMAN%PTEST.EQ.0)THEN
  IF(PEST%PARAM(IP)%PLOG.EQ.0)THEN
   WRITE(IUPESTOUT,'(I2,1X,A5,2(1X,I5),5(1X,F15.7),I10,I10,A10,A15,F15.7)') I,PEST%PARAM(IP)%PPARAM,PEST%PARAM(IP)%PILS, &
      PEST%PARAM(IP)%PIZONE,PEST%PARAM(IP)%PINI,PEST%PARAM(IP)%PDELTA,PEST%PARAM(IP)%PMIN,PEST%PARAM(IP)%PMAX,PEST%PARAM(IP)%PINCREASE, &
      ABS(PEST%PARAM(IP)%PIGROUP),PEST%PARAM(IP)%PLOG,'Unknown',PEST%PARAM(IP)%ACRONYM,PEST%PARAM(IP)%PPRIOR
   PEST%PARAM(IP)%ALPHA_HISTORY(0)=PEST%PARAM(IP)%PINI
  ELSE
   WRITE(IUPESTOUT,'(I2,1X,A5,2(1X,I5),5(1X,F15.7),I10,I10,A10,A15,F15.7)') I,PEST%PARAM(IP)%PPARAM,PEST%PARAM(IP)%PILS, &
      PEST%PARAM(IP)%PIZONE,EXP(PEST%PARAM(IP)%PINI),EXP(PEST%PARAM(IP)%PDELTA),EXP(PEST%PARAM(IP)%PMIN),EXP(PEST%PARAM(IP)%PMAX),PEST%PARAM(IP)%PINCREASE, &
      ABS(PEST%PARAM(IP)%PIGROUP),PEST%PARAM(IP)%PLOG,'Unknown',PEST%PARAM(IP)%ACRONYM,EXP(PEST%PARAM(IP)%PPRIOR)
   PEST%PARAM(IP)%ALPHA_HISTORY(0)=EXP(PEST%PARAM(IP)%PINI)
  ENDIF
 ENDIF
 
 END SUBROUTINE IPEST_GLM_ECHO_PARAMETERS
  
 !#####=================================================================
 SUBROUTINE IPEST_GLM_SETGROUPS()
 !#####=================================================================
 IMPLICIT NONE
 INTEGER :: I,J
 
 !## set igroup lt 0 for followers in group - check whether factors within group are equal --- need to be
 !## make sure group with active nodes is positive rest is negative
 DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PACT.EQ.0)THEN
   CYCLE
  ENDIF
  DO J=1,I-1 
   IF(PEST%PARAM(J)%PACT.EQ.0)CYCLE
   IF(PEST%PARAM(J)%PIGROUP.EQ.PEST%PARAM(I)%PIGROUP)THEN
    !## check factor
    IF(PEST%PARAM(J)%PINI.NE.PEST%PARAM(I)%PINI)THEN
     WRITE(*,'(/A)') 'Initial factor in an group need to be identicial'
     WRITE(*,'(A/)') 'Check initial factors for group '//TRIM(ITOS(PEST%PARAM(J)%PIGROUP))
    ENDIF
    PEST%PARAM(I)%PIGROUP=-1*PEST%PARAM(I)%PIGROUP; EXIT
   ENDIF
  ENDDO
 ENDDO
 
 END SUBROUTINE IPEST_GLM_SETGROUPS
 
 !#####=================================================================
 LOGICAL FUNCTION IPEST_GLM_PST(DIR,MNAME,IGRAD,IPARAM,CTYPE)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,CTYPE,MNAME
 INTEGER,INTENT(IN) :: IGRAD,IPARAM
 CHARACTER(LEN=256) :: DIRNAME,FN1,FN2
 INTEGER :: I,N,IU,JU,IOS,NCOL,NROW,ICOL,IROW,IEQ
 REAL(KIND=DP_KIND) :: X1,Y1,X2,Y2,DX
 REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: SX,SY
 
 IPEST_GLM_PST=.FALSE.
 
 DIRNAME=TRIM(DIR)//'\MODELINPUT\'//TRIM(MNAME)//'_'//TRIM(CTYPE)//'#'//TRIM(ITOS(IPARAM))//'.PST1'
 IU=UTL_GETUNIT(); OPEN(IU,FILE=DIRNAME,STATUS='OLD',ACTION='READ')
 DIRNAME=TRIM(DIR)//'\MODELINPUT\'//TRIM(MNAME)//'_'//TRIM(CTYPE)//'#'//TRIM(ITOS(IPARAM))//'.PST1_'
 JU=UTL_GETUNIT(); OPEN(JU,FILE=DIRNAME,STATUS='UNKNOWN',ACTION='WRITE')
 
 !## copy header
 READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)RETURN;     WRITE(JU,'(A)') TRIM(LINE)
 READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)RETURN;     WRITE(JU,'(A)') TRIM(LINE)
 READ(LINE,*,IOSTAT=IOS) NCOL,NROW; IF(IOS.NE.0)RETURN
 READ(IU,*,IOSTAT=IOS) X1,Y1,X2,Y2,IEQ; IF(IOS.NE.0)RETURN; WRITE(JU,*) X1,Y1,X2,Y2,IEQ
 IF(IEQ.EQ.0)THEN 
  READ(IU,*,IOSTAT=IOS) DX; IF(IOS.NE.0)RETURN; WRITE(JU,*) DX
 ELSE
  ALLOCATE(SX(0:NCOL)); READ(IU,*,IOSTAT=IOS) (SX(ICOL),ICOL=0,NCOL); IF(IOS.NE.0)RETURN; WRITE(JU,*) (SX(ICOL),ICOL=0,NCOL); DEALLOCATE(SX)
  ALLOCATE(SY(0:NROW)); READ(IU,*,IOSTAT=IOS) (SY(IROW),IROW=0,NROW); IF(IOS.NE.0)RETURN; WRITE(JU,*) (SY(IROW),IROW=0,NROW); DEALLOCATE(SY)
 ENDIF

 !## copy measurements
 READ(IU,*,IOSTAT=IOS) N; IF(IOS.NE.0)RETURN; WRITE(JU,*) N
 DO I=1,ABS(N); READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)RETURN; WRITE(JU,'(A)') TRIM(LINE); ENDDO
 
 !## copy parameters
 READ(IU,*,IOSTAT=IOS) N; IF(IOS.NE.0)RETURN; WRITE(JU,*) N
 READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)RETURN; WRITE(JU,'(A)') TRIM(LINE)

 !## write blankout idf
 IF(PEST%PE_KTYPE.LT.0)THEN
  READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)RETURN; WRITE(JU,'(A)') TRIM(LINE)
 ENDIF
 
 N=0; IF(ASSOCIATED(PEST%S_PERIOD))N=SIZE(PEST%S_PERIOD)
 IF(N.GT.0)THEN
  DO I=1,SIZE(PEST%S_PERIOD)
   READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)RETURN; WRITE(JU,'(A)') TRIM(LINE)
  ENDDO
 ENDIF
 
 N=0; IF(ASSOCIATED(PEST%B_FRACTION))N=SIZE(PEST%B_FRACTION)
 IF(N.GT.0)THEN
  DO I=1,SIZE(PEST%B_FRACTION)
   READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)RETURN; WRITE(JU,'(A)') TRIM(LINE) 
  ENDDO
 ENDIF

 DO I=1,SIZE(PEST%PARAM)
  !## read old settings
  READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)RETURN
  IF(PEST%PARAM(I)%PLOG.EQ.0)THEN
   LINE=TRIM(ITOS(MIN(1,ABS(PEST%PARAM(I)%PACT))))//','// &
        TRIM(PEST%PARAM(I)%PPARAM)               //','// &
        TRIM(ITOS(PEST%PARAM(I)%PILS))           //','// &
        TRIM(ITOS(PEST%PARAM(I)%PIZONE))         //','// &    
        TRIM(RTOS(PEST%PARAM(I)%ALPHA(1),'G',7)) //','// &
        TRIM(RTOS(PEST%PARAM(I)%PDELTA,'G',7))   //','// &
        TRIM(RTOS(PEST%PARAM(I)%PMIN/10.0D0,'G',7))     //','// &
        TRIM(RTOS(PEST%PARAM(I)%PMAX*10.0D0,'G',7))     //','// &
        TRIM(RTOS(PEST%PARAM(I)%PINCREASE,'G',7))//','// &
        TRIM(ITOS(ABS(PEST%PARAM(I)%PIGROUP)))   //','// &
        TRIM(ITOS(PEST%PARAM(I)%PLOG))//','// &
        TRIM(PEST%PARAM(I)%ACRONYM)//','// &
        TRIM(RTOS(PEST%PARAM(I)%PPRIOR,'G',7))        
  ELSE
   LINE=TRIM(ITOS(MIN(1,ABS(PEST%PARAM(I)%PACT))))//','// &
        TRIM(PEST%PARAM(I)%PPARAM)                 //','// &
        TRIM(ITOS(PEST%PARAM(I)%PILS))             //','// &
        TRIM(ITOS(PEST%PARAM(I)%PIZONE))           //','// &    
        TRIM(RTOS(EXP(PEST%PARAM(I)%ALPHA(1)),'G',7))//','// &
        TRIM(RTOS(EXP(PEST%PARAM(I)%PDELTA),'G',7))//','// &
        TRIM(RTOS(EXP(PEST%PARAM(I)%PMIN)/10.0D0,'G',7))  //','// &
        TRIM(RTOS(EXP(PEST%PARAM(I)%PMAX)*10.0D0,'G',7))  //','// &
        TRIM(RTOS(PEST%PARAM(I)%PINCREASE,'G',7))  //','// &
        TRIM(ITOS(ABS(PEST%PARAM(I)%PIGROUP)))     //','// &
        TRIM(ITOS(PEST%PARAM(I)%PLOG))//','// &
        TRIM(PEST%PARAM(I)%ACRONYM)//','// &
        TRIM(RTOS(EXP(PEST%PARAM(I)%PPRIOR),'G',7))    
  ENDIF
  WRITE(JU,'(A)') TRIM(LINE)

  IF(TRIM(CTYPE).EQ.'L')THEN
   PEST%PARAM(I)%LALPHA(IGRAD)=PEST%PARAM(I)%ALPHA(1)
  ELSEIF(TRIM(CTYPE).EQ.'P')THEN
   PEST%PARAM(I)%GALPHA(IGRAD)=PEST%PARAM(I)%ALPHA(1)
  ENDIF
  
 ENDDO
 
 !## copy zones
 READ(IU,*,IOSTAT=IOS) N; IF(IOS.NE.0)RETURN; WRITE(JU,*) N
 DO I=1,N; READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)RETURN; WRITE(JU,'(A)') TRIM(LINE); ENDDO
 
 CLOSE(IU,STATUS='DELETE'); CLOSE(JU)
 FN1=TRIM(DIR)//'\MODELINPUT\'//TRIM(MNAME)//'_'//TRIM(CTYPE)//'#'//TRIM(ITOS(IPARAM))//'.PST1_'
 FN2=TRIM(DIR)//'\MODELINPUT\'//TRIM(MNAME)//'_'//TRIM(CTYPE)//'#'//TRIM(ITOS(IPARAM))//'.PST1'
 CALL IOSRENAMEFILE(FN1,FN2)
 
 IPEST_GLM_PST=.TRUE.
 
 END FUNCTION IPEST_GLM_PST

 !#####=================================================================
 SUBROUTINE IPEST_GLM_ALLOCATE(N)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER :: M
 
 CALL IPEST_GLM_DEALLOCATE()
 
 M=PBMAN%NLINESEARCH
 ALLOCATE(RNG(N),RNL(M),GPARAM(N),LPARAM(M))

 !## allocate memory for process-status memory
 M=MAX(M,N); ALLOCATE(ISTATUS(N),IPROC(2,N))
 
 END SUBROUTINE IPEST_GLM_ALLOCATE

 !#####=================================================================
 SUBROUTINE IPEST_GLM_DEALLOCATE()
 !#####=================================================================
 IMPLICIT NONE

 IF(ALLOCATED(RNG))      DEALLOCATE(RNG)
 IF(ALLOCATED(RNL))      DEALLOCATE(RNL)
 IF(ALLOCATED(IPROC))    DEALLOCATE(IPROC)
 IF(ALLOCATED(GPARAM))   DEALLOCATE(GPARAM)
 IF(ALLOCATED(LPARAM))   DEALLOCATE(LPARAM)
 IF(ALLOCATED(ISTATUS))  DEALLOCATE(ISTATUS)
 IF(ALLOCATED(LHC_TABLE))DEALLOCATE(LHC_TABLE)
 
 END SUBROUTINE IPEST_GLM_DEALLOCATE
 
 !#####=================================================================
 LOGICAL FUNCTION IPEST_GLM_NEXT(ITER,ILIN,IBATCH)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER,ILIN,IBATCH
 REAL(KIND=DP_KIND) :: IMPROVEMENT,F,GUPDATE
 INTEGER :: I,J,ILOG

 IPEST_GLM_NEXT=.FALSE.
 
 !## set current dh on correct position
 DO I=1,MSR%NOBS; MSR%DHL(0,I)=MSR%DHL(ILIN,I); ENDDO

 !## not necessary for first cycle
 IF(ITER.EQ.0)THEN
  MSR%PJ=MSR%TJ
  WRITE(IUPESTEFFICIENCY,'(4E15.7,2(14X,A1))') MSR%TJ,SQRT(MSR%TJ),MSR%TJ/REAL(MSR%NOBS),REAL(SQRT(MSR%TJ))/REAL(MSR%NOBS,8),'-','-'
  IPEST_GLM_NEXT=.TRUE.
  RETURN
 ENDIF
 
! IF(LSENS)THEN
!!  !## next parameter combination
!!  IF(.NOT.PESTNEXTSENS())STOP
!!  IF(.NOT.PESTNEXTGRAD())STOP
!  IF(.NOT.PESTNEXTGRAD())CALL PESTGRADIENT(root,idf)
! ELSEIF(LGRAD)THEN
!  !## what proces is going on?
!  IF(.NOT.PESTNEXTGRAD())THEN
!   !## get gradient
!   CALL PESTGRADIENT(ROOT,idf)
!   CALL PEST_ECHOPARAMETERS(GUPDATE)
!   LLNSRCH=.TRUE.; PEST_ILNSRCH=1; LGRAD=.FALSE.; PEST_IGRAD=0
!  ENDIF

 CALL IPEST_GLM_ECHOPARAMETERS(GUPDATE,ITER)

 !## update alpha for parameters in same group
 DO I=1,SIZE(PEST%PARAM)
  !## skip inactive parameters
  IF(PEST%PARAM(I)%PACT.LE.0)CYCLE
  DO J=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(I)%PIGROUP.EQ.ABS(PEST%PARAM(J)%PIGROUP))PEST%PARAM(J)%ALPHA(1)=PEST%PARAM(I)%ALPHA(1)
  ENDDO
 ENDDO
   
 IMPROVEMENT=0; DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PACT.LT.0)CYCLE
  IF(PEST%PARAM(I)%PLOG.EQ.1)THEN
   F=(EXP(PEST%PARAM(I)%ALPHA(1))/EXP(PEST%PARAM(I)%ALPHA(2)))*100.0D0
   F=ABS(F-100.0D0)
   IMPROVEMENT=IMPROVEMENT+F
  ELSE
   F=(PEST%PARAM(I)%ALPHA(1)/PEST%PARAM(I)%ALPHA(2))*100.0D0
   F=ABS(F-100.0D0)
   IMPROVEMENT=IMPROVEMENT+F
  ENDIF 
 ENDDO

 WRITE(IUPESTEFFICIENCY,'(6E15.7)') MSR%TJ,SQRT(MSR%TJ),MSR%TJ/REAL(MSR%NOBS),REAL(SQRT(MSR%TJ))/REAL(MSR%NOBS,8),IMPROVEMENT,(MSR%PJ/MSR%TJ) 
 
 WRITE(IUPESTRUNFILE,'(/A,I10/)') 'Copy in the runfile, iteration ',ITER
 DO I=1,SIZE(PEST%PARAM)
  ILOG=PEST%PARAM(I)%PLOG
  IF(PEST%PARAM(I)%PLOG.EQ.1)THEN
   WRITE(IUPESTRUNFILE,'(I2,1X,A,1X,2(I5,1X),5(F10.3,1X),I5,1X,I2,1X,A,F10.3)') MIN(1,ABS(PEST%PARAM(I)%PACT)), &  !## iact
       PEST%PARAM(I)%PPARAM, &         !## ptype
       PEST%PARAM(I)%PILS, &           !## ilayer/system
       PEST%PARAM(I)%PIZONE, &         !## zone number
       EXP(PEST%PARAM(I)%ALPHA(1)), & !## initial value
       EXP(PEST%PARAM(I)%PDELTA), &    !## finite difference step
       EXP(PEST%PARAM(I)%PMIN), &      !## minimal value
       EXP(PEST%PARAM(I)%PMAX),&       !## maximal value
       PEST%PARAM(I)%PINCREASE,&           !## maximal adjust factor
       ABS(PEST%PARAM(I)%PIGROUP),&    !## group number
       ILOG,&                    !## log transformed
       TRIM(PEST%PARAM(I)%ACRONYM), &
       EXP(PEST%PARAM(I)%PPRIOR)
  ELSE
   WRITE(IUPESTRUNFILE,'(I2,1X,A,1X,2(I5,1X),5(F10.3,1X),I5,1X,I2,1X,A,F10.3)') MIN(1,ABS(PEST%PARAM(I)%PACT)), &  !## iact
       PEST%PARAM(I)%PPARAM, & !## ptype
       PEST%PARAM(I)%PILS, &   !## ilayer/system
       PEST%PARAM(I)%PIZONE, & !## zone number
       PEST%PARAM(I)%ALPHA(1), &   !## initial value
       PEST%PARAM(I)%PDELTA, & !## finite difference step
       PEST%PARAM(I)%PMIN, &   !## minimal value
       PEST%PARAM(I)%PMAX,&    !## maximal value
       PEST%PARAM(I)%PINCREASE,&   !## maximal adjust factor
       ABS(PEST%PARAM(I)%PIGROUP),& !## group number
       ILOG, &            !## log transformed
       TRIM(PEST%PARAM(I)%ACRONYM), &
       PEST%PARAM(I)%PPRIOR
  ENDIF 
 ENDDO

 !## length of gradient update vector
 IF(GUPDATE.LT.PEST%PE_PADJ)THEN
  IF(IBATCH.EQ.1)WRITE(IUPESTOUT,'(/A/)') 'Process stopped, less than '//TRIM(RTOS(PEST%PE_PADJ,'F',3))//' of vector length'
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Process stopped, less than '//TRIM(RTOS(PEST%PE_PADJ,'F',3))//' of vector length','Error')
  RETURN
 ENDIF
 !## continue ?
 IF(ITER+1.GT.PEST%PE_MXITER)THEN
  IF(IBATCH.EQ.1)WRITE(IUPESTOUT,'(/A/)') 'Pest iteration terminated: PEST_ITER (='//TRIM(ITOS(PEST%PE_MXITER))//') = PEST_NITER (='//TRIM(ITOS(PEST%PE_MXITER))//')'
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Pest iteration terminated: PEST_ITER (='//TRIM(ITOS(PEST%PE_MXITER))//') = PEST_NITER (='//TRIM(ITOS(PEST%PE_MXITER))//')','Error')
  RETURN
 ENDIF
 IF(MSR%TJ.LE.0.0D0)THEN
  IF(IBATCH.EQ.1)WRITE(IUPESTOUT,'(/A/)') 'Objective Function <= 0.0 ('//TRIM(RTOS(MSR%TJ,'G',7))//')'
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Objective Function <= 0.0 ('//TRIM(RTOS(MSR%TJ,'G',7))//')','Error')
  RETURN
 ENDIF
 IF(IMPROVEMENT.LE.PEST%PE_STOP)THEN
  IF(IBATCH.EQ.1)WRITE(IUPESTOUT,'(/A/)') 'Pest iteration terminated decrease objective function ('//TRIM(RTOS(100.0D0*IMPROVEMENT,'G',7))// &
       '%) > PEST_JSTOP ('//TRIM(RTOS(100.0D0*PEST%PE_STOP,'G',7))//'%)'
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Pest iteration terminated decrease objective function ('//TRIM(RTOS(100.0D0*IMPROVEMENT,'G',7))// &
       '%) > PEST_JSTOP ('//TRIM(RTOS(100.0D0*PEST%PE_STOP,'G',7))//'%)','Error')
  RETURN
 ENDIF
 
 !## copy current objective function value to previous objective function value
 MSR%PJ=MSR%TJ

 !## replace old by new parameter values
 PEST%PARAM%ALPHA(2)=PEST%PARAM%ALPHA(1)
 
 !## next iteration
 WRITE(*,'(/A/)') ' *** Next Outer Iteration ***'
 
 IPEST_GLM_NEXT=.TRUE.
 
 END FUNCTION IPEST_GLM_NEXT

 !#####=================================================================
 SUBROUTINE IPEST_GLM_NEXTGRAD(IPARAM)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPARAM
 INTEGER :: I
 REAL(KIND=DP_KIND) :: FCT
 
 !## reset all alpha's
 PEST%PARAM%ALPHA(1)=PEST%PARAM%ALPHA(2)

 !## adjust all parameters within the same group
 DO I=1,SIZE(PEST%PARAM)
  !## skip inactive parameters
  IF(PEST%PARAM(I)%PACT.EQ.0)CYCLE
  IF(ABS(PEST%PARAM(I)%PIGROUP).EQ.ABS(PEST%PARAM(IPARAM)%PIGROUP))THEN
   IF(PEST%PARAM(I)%PLOG.EQ.1)THEN
    PEST%PARAM(I)%ALPHA(1)=PEST%PARAM(I)%ALPHA(2)+PEST%PARAM(I)%PDELTA  !!!!<<< WHY ++++ INSTEAD OF ***
    FCT=EXP(PEST%PARAM(I)%ALPHA(1))
   ELSE
    PEST%PARAM(I)%ALPHA(1)=PEST%PARAM(I)%ALPHA(2)*PEST%PARAM(I)%PDELTA
    FCT=PEST%PARAM(I)%ALPHA(1)
   ENDIF
   IF(PEST%PARAM(I)%PIGROUP.GT.0)THEN
    WRITE(*,'(A)') 'Adjusting Parameter '//TRIM(PEST%PARAM(I)%PPARAM)// &
                 ';ils='//TRIM(ITOS(PEST%PARAM(I)%PILS))// &
                 ';izone='//TRIM(ITOS(PEST%PARAM(I)%PIZONE))// &
                 ';igroup='//TRIM(ITOS(PEST%PARAM(I)%PIGROUP))// &
                 ';factor='//TRIM(RTOS(FCT,'*',1))
   ENDIF
   WRITE(IUPESTOUT,'(A)') 'Adjusting Parameter '//TRIM(PEST%PARAM(I)%PPARAM)// &
                '['//TRIM(PEST%PARAM(I)%ACRONYM)//']'// &
                ';ils='//TRIM(ITOS(PEST%PARAM(I)%PILS))// &
                ';izone='//TRIM(ITOS(PEST%PARAM(I)%PIZONE))// &
                ';igroup='//TRIM(ITOS(PEST%PARAM(I)%PIGROUP))// &
                ';factor='//TRIM(RTOS(FCT,'*',1))
  ENDIF
 ENDDO
 
 END SUBROUTINE IPEST_GLM_NEXTGRAD
 
 !!###====================================================================
 !LOGICAL FUNCTION IPEST_GLM_MONTECARLO(ITER)
 !!###====================================================================
 !IMPLICIT NONE
 !INTEGER,INTENT(IN) :: ITER
 !INTEGER :: IP,NP,I,J,JP
 !REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: JAC
 !REAL(KIND=DP_KIND) :: DH,W,MJ
 !
 !IPEST_GLM_MONTECARLO=.FALSE.
 !
 !NP=0; DO I=1,SIZE(PEST%PARAM); IF(PEST%PARAM(I)%PACT.NE.1)CYCLE; NP=NP+1; ENDDO
 !ALLOCATE(JAC(NP)); JAC=0.0D0
 !
 !!## get scores
 !IP=0
 !DO I=1,SIZE(PEST%PARAM)
 ! IF(PEST%PARAM(I)%PACT.NE.1)CYCLE; IP=IP+1
 ! DO J=1,MSR%NOBS
 !  W=MSR%W(J); DH=MSR%DHG(IP,J); JAC(IP)=JAC(IP)+W*DH**2.0D0
 ! ENDDO
 !ENDDO
 !
 !!## get best one
 !MJ=JAC(1); JP=1; DO IP=2,NP
 ! IF(JAC(IP).LT.MJ)THEN
 !  JP=IP; MJ=JAC(IP)
 ! ENDIF
 !ENDDO
 !
 !IP=0
 !DO I=1,SIZE(PEST%PARAM)
 ! IF(PEST%PARAM(I)%PACT.NE.1)CYCLE; IP=IP+1
 ! PEST%PARAM(I)%ALPHA(1)=LHC_TABLE(IP,JP)
 !ENDDO
 !
 !!## copy gradients to all groups
 !DO I=1,SIZE(PEST%PARAM)
 ! IF(PEST%PARAM(I)%PACT.NE.1)CYCLE
 ! DO J=1,SIZE(PEST%PARAM)
 !  IF(ABS(PEST%PARAM(J)%PIGROUP).EQ.PEST%PARAM(I)%PIGROUP)PEST%PARAM(J)%ALPHA(1)=PEST%PARAM(I)%ALPHA(1)
 ! ENDDO
 !ENDDO
 !
 !!## save alphas for history-logging
 !DO IP=1,SIZE(PEST%PARAM)
 ! IF(PEST%PARAM(IP)%PACT.EQ.0)CYCLE
 !
 ! IF(PEST%PARAM(IP)%PLOG.EQ.1)THEN
 !  PEST%PARAM(IP)%ALPHA_HISTORY(ITER)=EXP(PEST%PARAM(IP)%ALPHA(1))
 ! ELSE
 !  PEST%PARAM(IP)%ALPHA_HISTORY(ITER)=PEST%PARAM(IP)%ALPHA(1)
 ! ENDIF
 !
 !ENDDO
 !
 !DEALLOCATE(JAC)
 !
 !IPEST_GLM_MONTECARLO=.TRUE.
 !
 !END FUNCTION IPEST_GLM_MONTECARLO
 
 !###====================================================================
 LOGICAL FUNCTION IPEST_GLM_GRADIENT(IBATCH,ITER,LAMBDA)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER,IBATCH
 REAL(KIND=DP_KIND),INTENT(IN) :: LAMBDA
 REAL(KIND=DP_KIND) :: DJ1,DJ2
 REAL(KIND=DP_KIND) :: TS,DF1,EIGWTHRESHOLD,W,DH1,DH2,MARQUARDT
 INTEGER :: I,J,K,NP,IP1,NE,ISING,ILAMBDA,IBND,IPARAM
 INTEGER,ALLOCATABLE,DIMENSION(:) :: INDX,IJAC
 REAL(KIND=DP_KIND) :: P1,P2,PMIN,PMAX
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: JS,P,PT
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: N,RU 
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: S
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: EIGV,B,M,JAC
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: EIGW,JQR
 LOGICAL :: LSVD

 IPEST_GLM_GRADIENT=.FALSE.
 
 SELECT CASE (PEST%PE_SCALING)
  CASE (0,1); LSVD=.FALSE.
  CASE (2,3); LSVD=.TRUE.
 END SELECT

 !## sensitivity
 NP=SIZE(RNG); IF(.NOT.ALLOCATED(S)) ALLOCATE(S(NP)); S=0.0D0
 
 IF(ALLOCATED(JAC))DEALLOCATE(JAC); IF(ALLOCATED(IJAC))DEALLOCATE(IJAC)
 ALLOCATE(JAC(NP,MSR%NOBS),IJAC(MSR%NOBS))

 IPARAM=0
 DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PACT.NE.1)CYCLE; DF1=PEST%PARAM(I)%PDELTA; IPARAM=IPARAM+1
  DO J=1,MSR%NOBS
   W=MSR%W(J); DH1=MSR%DHG(IPARAM,J); DH2=MSR%DHL(0,J); JAC(IPARAM,J)=W*((DH1-DH2)/DF1); S(IPARAM)=S(IPARAM)+JAC(IPARAM,J)
  ENDDO
 ENDDO
 DO I=1,NP; S(I)=S(I)/DBLE(MSR%NOBS); ENDDO
 
 !## check linear dependency of dhg()
 CALL IPEST_ECHELON_DBL(JAC,IJAC,MSR%NOBS,NP)
 !## remove columns (parameters) that are linear combinations of others
 I=0; DO IPARAM=1,NP; IF(IJAC(IPARAM).EQ.0)I=I+1; S(IPARAM)=DBLE(IJAC(IPARAM))*S(IPARAM); ENDDO
 WRITE(IUPESTOUT,'(/A/)') 'Filtered out '//TRIM(ITOS(I))//' linear dependencies out of '//TRIM(ITOS(NP))//' from Jacobian Matrix'
 IF(ALLOCATED(JAC))DEALLOCATE(JAC); IF(ALLOCATED(IJAC))DEALLOCATE(IJAC)
 
 TS=SUM(ABS(S)); IPARAM=0; DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PACT.NE.1)CYCLE; IPARAM=IPARAM+1; IF(TS.NE.0.0D0)S(IPARAM)=S(IPARAM)/TS
 ENDDO; S=ABS(S)*100.0D0

 WRITE(IUPESTSENSITIVITY,'(I10,99999F15.7)') ITER,(S(I),I=1,NP)

 !##===================
 !## write down statistics of ALL parameters prior to the update
 !##===================
 NP=0; DO I=1,SIZE(PEST%PARAM); IF(PEST%PARAM(I)%PACT.EQ.1)NP=NP+1; ENDDO
 IF(ALLOCATED(JQJ))DEALLOCATE(JQJ);   ALLOCATE(JQJ (NP,NP))
 IF(ALLOCATED(EIGW))DEALLOCATE(EIGW); ALLOCATE(EIGW(NP))
 IF(ALLOCATED(EIGV))DEALLOCATE(EIGV); ALLOCATE(EIGV(NP,NP))
 !## write statistics (covariance/correlation)
 CALL IPEST_GLM_JQJ(IBATCH,MARQUARDT,JQJ,NP,.TRUE.)
 
 !##===================
 
 !## reset parameters - alpha(2)=previous alpha
 DO I=1,SIZE(PEST%PARAM); PEST%PARAM(I)%ALPHA(1)=PEST%PARAM(I)%ALPHA(2); ENDDO

 !## set boundaries for parameters
 DO I=1,SIZE(PEST%PARAM); CALL IPEST_GLM_GETBOUNDARY(I,IBND,P1,P2,PMIN,PMAX); PEST%PARAM(I)%IBND=IBND; ENDDO

 !## "freeze"-insensitive parameters
 WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'List of Insensitive Parameter (Sensitivity <= '//TRIM(RTOS(PEST%PE_SENS,'F',7))//' %):'; WRITE(IUPESTOUT,*)
 IPARAM=0; DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PACT.NE.1)CYCLE; IPARAM=IPARAM+1
  IF(S(IPARAM).LE.PEST%PE_SENS)THEN
   PEST%PARAM(I)%PACT=-1
   IF(PEST%PARAM(I)%ACRONYM.EQ.'')THEN
    WRITE(IUPESTOUT,'(A2,2I5.5,I3.3,F15.7,A2)') PEST%PARAM(I)%PPARAM,PEST%PARAM(I)%PILS,PEST%PARAM(I)%PIZONE,ABS(PEST%PARAM(I)%PIGROUP),S(IPARAM),' %'
   ELSE
    WRITE(IUPESTOUT,'(A15,F15.7,A2)') PEST%PARAM(I)%ACRONYM,S(IPARAM),' %'
   ENDIF
  ENDIF
 ENDDO

 !## initiate number of parameters
 NP=0; DO I=1,SIZE(PEST%PARAM); IF(PEST%PARAM(I)%PACT.EQ.1)NP=NP+1; ENDDO
 IF(NP.EQ.0)THEN; CALL IPEST_GLM_ERROR(IBATCH,'ALL PARAMETERS ARE INSENSITIVE, PROCESS STOPPED!'); RETURN; ENDIF

 !## allocate arrays for current selection
 NP=0; DO I=1,SIZE(PEST%PARAM); IF(PEST%PARAM(I)%PACT.EQ.1)NP=NP+1; ENDDO
 IF(NP.EQ.0)THEN; CALL IPEST_GLM_ERROR(IBATCH,'NO PARAMETERS LEFT THAT ARE SENSITIVE, PROCESS STOPPED!'); RETURN; ENDIF
  
 IF(ALLOCATED(JQJ))DEALLOCATE(JQJ);   ALLOCATE(JQJ (NP,NP))
 IF(ALLOCATED(JQR))DEALLOCATE(JQR);   ALLOCATE(JQR (NP))
 IF(ALLOCATED(U  ))DEALLOCATE(U);     ALLOCATE(U   (NP))
 IF(ALLOCATED(EIGW))DEALLOCATE(EIGW); ALLOCATE(EIGW(NP))
 IF(ALLOCATED(EIGV))DEALLOCATE(EIGV); ALLOCATE(EIGV(NP,NP))
! IF(ALLOCATED(COV ))DEALLOCATE(COV);  ALLOCATE(COV (NP,NP))

 !## construct jTqr (<--- r is residual for current parameter set)
 JQR=0.0; I=0; IPARAM=0
 DO IP1=1,SIZE(PEST%PARAM)  !## row
  
  IF(ABS(PEST%PARAM(IP1)%PACT).NE.1)CYCLE
  IPARAM=IPARAM+1; IF(PEST%PARAM(IP1)%PACT.NE.1)CYCLE
   
  DF1=PEST%PARAM(IP1)%PDELTA
  
  I=I+1
  DO J=1,MSR%NOBS
   DH1=MSR%DHG(IPARAM,J); DH2=MSR%DHL(0,J)
   DJ1=(DH1-DH2)/DF1    ; DJ2=MSR%DHL(0,J)
   W  =MSR%W(J)
   JQR(I)=JQR(I)+(DJ1*W*DJ2)
  ENDDO

 ENDDO
  
 !!## add parameter regularisation
 !IF(PEST%PE_REGULARISATION.EQ.1)THEN
 ! I=0
 ! DO IP1=1,SIZE(PEST%PARAM)  !## row
 !  IF(PEST%PARAM(IP1)%PACT.NE.1)CYCLE
 !
 !  I=I+1
 !  DP=PEST%PARAM(IP1)%ALPHA(2)-PEST%PARAM(IP1)%PPRIOR
 !  DP=DP*PEST%PE_REGFACTOR
 !  JQR(I)=JQR(I)+DP
 !    
 ! ENDDO
 !ENDIF

 !## compute update vector for lambdas
 DO ILAMBDA=1,PBMAN%NLINESEARCH
  
  !## set marquardt-lambda
  MARQUARDT=LAMBDA*PBMAN%LAMBDA_TEST(ILAMBDA)

  !## construct jqj - normal matrix/hessian
  CALL IPEST_GLM_JQJ(IBATCH,MARQUARDT,JQJ,NP,.FALSE.) 
  !## add eigenvalue decomposition
  CALL IPEST_GLM_EIGDECOM(IBATCH,JQJ,EIGW,EIGV,NP,.TRUE.) 
  
  !## project on important singular values
  IF(LSVD)THEN

   EIGWTHRESHOLD=0.0 !% explained variance
   DO NE=1,NP; EIGWTHRESHOLD=EIGWTHRESHOLD+EIGW(NE); IF(EIGWTHRESHOLD.GT.PBMAN%EIGV)EXIT; ENDDO

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
   CALL IPEST_LUDECOMP_DBL(M,INDX,NE,ISING)
   IF(ISING.EQ.1)THEN; CALL IPEST_GLM_ERROR(IBATCH,'Singular matrix after projection on eigenvectors which is rather odd, stopped.'); RETURN; ENDIF
   B=0.0D0; DO I=1,NE; B(I,I)=1.0D0; ENDDO
   DO I=1,NE; CALL IPEST_LUBACKSUB_DBL(M,INDX,B(1,I),NE); ENDDO

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
   IF(ALLOCATED(B))DEALLOCATE(B); ALLOCATE(B(NP,NP))
   IF(NP.EQ.1)THEN
    B(1,1)=1.0D0/JQJ(1,1)
   ELSE
    CALL IPEST_LUDECOMP_DBL(JQJ,INDX,NP,ISING)
    IF(ISING.EQ.1)THEN; CALL IPEST_GLM_ERROR(IBATCH,'Singular matrix,try activating the SVD option to avoid this, stopped.'); RETURN; ENDIF
    B=0.0D0; DO I=1,NP; B(I,I)=1.0D0; ENDDO
    DO I=1,NP; CALL IPEST_LUBACKSUB_DBL(JQJ,INDX,B(1,I),NP); ENDDO
   ENDIF
   
   !## compute (JQJ)-1*JQR
   U=0.0D0
   DO I=1,NP; DO J=1,NP
    U(I)=U(I)+(B(J,I)*JQR(J))
   ENDDO; ENDDO

   DEALLOCATE(INDX,B)

  ENDIF

  !## pointing downhill
  U=-1.0D0*U 

  !## store gradient update vector in list
  IF(IPEST_GLM_UPGRADEVECTOR_LAMBDA(ILAMBDA))THEN 
  ENDIF 
!  !## within parameter adjust-limits
!  IF(IPEST_GLM_UPGRADEVECTOR(1.0D0,.TRUE.,ITER,LAMBDARESET=LAMBDARESET))THEN 
!   !## check whether number of parameters is equal to the number started this loop with
!   MP=0; DO I=1,SIZE(PEST%PARAM); IF(PEST%PARAM(I)%PACT.EQ.1.AND.PEST%PARAM(I)%PIGROUP.GT.0)MP=MP+1; ENDDO
!   !## nothing changed in number of active parameters
!   IF(MP.EQ.NP)EXIT
!  ENDIF
!  !## reset marquardt lambda
!  IF(LAMBDARESET)THEN
!   MARQUARDT=0.001D0
!  ELSE
!   !## increase marquardt
!   MARQUARDT=MARQUARDT*DAMPINGFACTOR
!  ENDIF

 ENDDO !## lambda-test-loop

 IF(ALLOCATED(EIGW))DEALLOCATE(EIGW)
 IF(ALLOCATED(EIGV))DEALLOCATE(EIGV)
 IF(ALLOCATED(JQR ))DEALLOCATE(JQR )
 IF(ALLOCATED(S   ))DEALLOCATE(S   )
! IF(ALLOCATED(C   ))DEALLOCATE(C   )
 IF(ALLOCATED(JS  ))DEALLOCATE(JS  )

 IPEST_GLM_GRADIENT=.TRUE.

 END FUNCTION IPEST_GLM_GRADIENT
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_GETBOUNDARY(IP1,IBND,P1,P2,PMIN,PMAX)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IP1
 INTEGER,INTENT(OUT) :: IBND
 REAL(KIND=8),INTENT(OUT) :: P1,P2,PMIN,PMAX
 
 !## parameter adjustment hit the parameter boundary
 P1  =PEST%PARAM(IP1)%ALPHA(1)
 P2  =PEST%PARAM(IP1)%ALPHA(2)
 PMIN=PEST%PARAM(IP1)%PMIN
 PMAX=PEST%PARAM(IP1)%PMAX
   
 IBND=0
 !## shoot over
 IF(P1.LE.PMIN)           IBND=-1; IF(P1.GE.PMAX)           IBND= 1
 !## too close
 IF(ABS(P1-PMIN).LE.XPBND)IBND=-1; IF(ABS(PMAX-P1).LE.XPBND)IBND= 1
 
 END SUBROUTINE IPEST_GLM_GETBOUNDARY
 
 !###====================================================================
 LOGICAL FUNCTION IPEST_GLM_UPGRADEVECTOR_LAMBDA(ILAMBDA)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ILAMBDA
 INTEGER :: I,IP1,IP2

 !## exit code
 IPEST_GLM_UPGRADEVECTOR_LAMBDA=.FALSE.

 !## fill in by default 
 DO IP1=1,SIZE(PEST%PARAM); PEST%PARAM(IP1)%ALPHA(1)=PEST%PARAM(IP1)%ALPHA(2); ENDDO
 
 I=0; DO IP1=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(IP1)%PACT.NE.1)CYCLE
  I=I+1; DO IP2=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(IP1)%PIGROUP.EQ.ABS(PEST%PARAM(IP2)%PIGROUP))THEN
    PEST%PARAM(IP2)%ALPHA(1)=PEST%PARAM(IP2)%ALPHA(2)+U(I)
   ENDIF
  ENDDO
 ENDDO  

! !## check for size of adjustment
! DO IP1=1,SIZE(PEST%PARAM)
!   
!  !## inactive parameter
!  IF(PEST%PARAM(IP1)%PACT.NE.1)CYCLE
!   
!  !## check size of adjustment
!  IF(PEST%PARAM(IP1)%PLOG.EQ.1)THEN
!   F=EXP(PEST%PARAM(IP1)%ALPHA(1))/EXP(PEST%PARAM(IP1)%ALPHA(2))
!  ELSE
!   F=    PEST%PARAM(IP1)%ALPHA(1) /    PEST%PARAM(IP1)%ALPHA(2)
!  ENDIF 
!
!!  !## adjustment too large decrease stepsize
!!  IF(F.LT.1.0D0/PEST%PARAM(IP1)%PINCREASE)THEN
!!  ELSEIF(F.GT.PEST%PARAM(IP1)%PINCREASE)THEN
!!  ENDIF
!
!  CALL IPEST_GLM_GETBOUNDARY(IP1,IBND,P1,P2,PMIN,PMAX)
!
!  !## parameter hits the boundary
!  IF(IBND.NE.0)THEN
!   !## hits the same boundary as before - skip it
!   IF(IBND.EQ.PEST%PARAM(IP1)%IBND)THEN 
!    !## ignore this parameter (group) for now - reset lambda and search another update vector
!    PEST%PARAM(IP1)%PACT=-1
!    LAMBDARESET=.TRUE.; RETURN
!
!   ELSE
!
!    AF=1.0D0
!    IF(P1.LT.PMIN)AF=(P2-PMIN)/(P2-P1)
!    IF(P1.GT.PMAX)AF=(PMAX-P2)/(P1-P2)
!    !## keep track of minimal adjustment of vector
!    F=MIN(AF,F)
!    !## recompute gradient and set this parameter on boundary
!    IF(F.LT.0.1D0)THEN
!     G=PEST%PARAM(IP1)%ALPHA(1)-PEST%PARAM(IP1)%ALPHA(2)
!     G=G*F
!     PEST%PARAM(IP1)%ALPHA(1)=PEST%PARAM(IP1)%ALPHA(2)+G
!     PEST%PARAM(IP1)%PACT=-1
!     LAMBDARESET=.TRUE.; RETURN
!    ENDIF
!   ENDIF
!  ENDIF  
! ENDDO
! 
! !## corrects all gradients with this factor
! IF(F.LT.1.0D0)THEN
!   
!  !## adjust all parameters
!  DO IP2=1,SIZE(PEST%PARAM) 
!   IF(PEST%PARAM(IP2)%PACT.NE.1)CYCLE
!  
!   G=PEST%PARAM(IP2)%ALPHA(1)-PEST%PARAM(IP2)%ALPHA(2)
!   G=G*F
!
!   !## update parameters
!   PEST%PARAM(IP2)%ALPHA(1)=PEST%PARAM(IP2)%ALPHA(2)+G
!  ENDDO
!
! ENDIF
  
 !## copy gradients to all groups
 DO IP1=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(IP1)%PACT.NE.1)CYCLE
  DO IP2=1,SIZE(PEST%PARAM)
   IF(ABS(PEST%PARAM(IP2)%PIGROUP).EQ.PEST%PARAM(IP1)%PIGROUP)PEST%PARAM(IP2)%ALPHA(1)=PEST%PARAM(IP1)%ALPHA(1)
  ENDDO
 ENDDO
 
! !## save alphas for history-logging
! J=0; DO IP1=1,SIZE(PEST%PARAM)
!  IF(PEST%PARAM(IP1)%PACT.EQ.0)CYCLE

!  IF(PEST%PARAM(IP1)%PLOG.EQ.1)THEN
!   PEST%PARAM(IP1)%ALPHA_HISTORY(ITER)=EXP(PEST%PARAM(IP1)%ALPHA(1))
!  ELSE
!   PEST%PARAM(IP1)%ALPHA_HISTORY(ITER)=PEST%PARAM(IP1)%ALPHA(1)
!  ENDIF
  
!  !## active parameter
!  IF(PEST%PARAM(IP1)%PACT.EQ.1)THEN
!   !## store final gradient
!   J=J+1; U(J)=PEST%PARAM(IP1)%ALPHA(1)-PEST%PARAM(IP1)%ALPHA(2)
!  ENDIF
  
! ENDDO

! WRITE(IUPESTOUT,*) 'final ones'
! DO I=1,SIZE(PEST%PARAM)
!  WRITE(IUPESTOUT,'(3I5,1x,2G15.8)') I,PEST%PARAM(I)%PACT,PEST%PARAM(I)%PIGROUP,PEST%PARAM(I)%ALPHA(1),PEST%PARAM(I)%ALPHA(2)
! ENDDO
! WRITE(IUPESTOUT,*) 
! FLUSH(IUPESTOUT)
 
 !## copy alphas to correct vector with updates per lambda
 DO IP1=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(IP1)%PLOG.EQ.1)THEN
   PEST%PARAM(IP1)%LALPHA(ILAMBDA)=EXP(PEST%PARAM(IP1)%ALPHA(1))
  ELSE
   PEST%PARAM(IP1)%LALPHA(ILAMBDA)=PEST%PARAM(IP1)%ALPHA(1)
  ENDIF
 ENDDO
 
 !## correct update gradient found
 IPEST_GLM_UPGRADEVECTOR_LAMBDA=.TRUE.

 END FUNCTION IPEST_GLM_UPGRADEVECTOR_LAMBDA
 
 !###====================================================================
 LOGICAL FUNCTION IPEST_GLM_UPGRADEVECTOR(FCT,LCHECK,ITER,LAMBDARESET)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER
 LOGICAL,INTENT(IN) :: LCHECK
 LOGICAL,INTENT(OUT),OPTIONAL :: LAMBDARESET
 REAL(KIND=8),INTENT(IN) :: FCT
 REAL(KIND=8) :: AF,F,G,P1,P2,PMIN,PMAX
 INTEGER :: I,J,IP1,IP2,IBND

 !## exit code
 IPEST_GLM_UPGRADEVECTOR=.FALSE.
 
 IF(PRESENT(LAMBDARESET))LAMBDARESET=.FALSE.
 
 !## adjust vector for fct (line-search)
 I=0; DO IP1=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(IP1)%PACT.NE.1)CYCLE
  I=I+1; U(I)=U(I)*FCT
 ENDDO

 !## fill in by default 
 DO IP1=1,SIZE(PEST%PARAM); PEST%PARAM(IP1)%ALPHA(1)=PEST%PARAM(IP1)%ALPHA(2); ENDDO
 
 I=0; DO IP1=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(IP1)%PACT.NE.1)CYCLE
  I=I+1; DO IP2=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(IP1)%PIGROUP.EQ.ABS(PEST%PARAM(IP2)%PIGROUP))THEN
    PEST%PARAM(IP2)%ALPHA(1)=PEST%PARAM(IP2)%ALPHA(2)+U(I)
   ENDIF
  ENDDO
 ENDDO  

 !## check whether boundary has been hit or maximum adjustment exceeds
 IF(LCHECK)THEN

  !## check for size of adjustment
  DO IP1=1,SIZE(PEST%PARAM)
   
   !## inactive parameter
   IF(PEST%PARAM(IP1)%PACT.NE.1)CYCLE
   
   !## check size of adjustment
   IF(PEST%PARAM(IP1)%PLOG.EQ.1)THEN
    F=EXP(PEST%PARAM(IP1)%ALPHA(1))/EXP(PEST%PARAM(IP1)%ALPHA(2))
   ELSE
    F=    PEST%PARAM(IP1)%ALPHA(1) /    PEST%PARAM(IP1)%ALPHA(2)
   ENDIF 

   !## adjustment too large -causes to get another lambda
   IF(F.LT.1.0D0/PEST%PARAM(IP1)%PINCREASE.OR.F.GT.PEST%PARAM(IP1)%PINCREASE)THEN
!    IF(TRIM(PEST%PARAM(IP1)%ACRONYM).EQ.'')THEN
!     WRITE(IUPESTOUT,'(A,G15.8)') 'Increase Lambda - adjustment for parameter '//TRIM(ITOS(IP1))//' to small/large ',F
!    ELSE
!     WRITE(IUPESTOUT,'(A,G15.8)') 'Increase Lambda - adjustment for parameter '//TRIM(PEST%PARAM(IP1)%ACRONYM)//' to small/large ',F
!    ENDIF
!    FLUSH(IUPESTOUT)
    RETURN
   ENDIF
  ENDDO
  
  F=1.0D0

  !## check for boundary of parameter
  DO IP1=1,SIZE(PEST%PARAM)
   
   !## inactive parameter
   IF(PEST%PARAM(IP1)%PACT.NE.1)CYCLE

   CALL IPEST_GLM_GETBOUNDARY(IP1,IBND,P1,P2,PMIN,PMAX)

   !## parameter hits the boundary
   IF(IBND.NE.0)THEN
    !## hits the same boundary as before - skip it
    IF(IBND.EQ.PEST%PARAM(IP1)%IBND)THEN 
     !## ignore this parameter (group) for now - reset lambda and search another update vector
     PEST%PARAM(IP1)%PACT=-1
!     IF(TRIM(PEST%PARAM(IP1)%ACRONYM).EQ.'')THEN
!      WRITE(IUPESTOUT,'(A)') 'Reset lambda - parameter '//TRIM(ITOS(IP1))//' hits boundary.'
!     ELSE
!      WRITE(IUPESTOUT,'(A)') 'Reset lambda - parameter '//TRIM(PEST%PARAM(IP1)%ACRONYM)//' hits boundary.'
!     ENDIF
!     FLUSH(IUPESTOUT)
     LAMBDARESET=.TRUE.; RETURN

    ELSE

     AF=1.0D0
     IF(P1.LT.PMIN)AF=(P2-PMIN)/(P2-P1)
     IF(P1.GT.PMAX)AF=(PMAX-P2)/(P1-P2)
!     WRITE(IUPESTOUT,'(A,I10,4F10.3)') 'PARAMETER,P1,P2,PMIN,PMAX',IP1,P1,P2,PMIN,PMAX
     !## keep track of minimal adjustment of vector
     F=MIN(AF,F)
!     WRITE(IUPESTOUT,'(A,2F10.3)') 'AF,F',AF,F
     !## recompute gradient and set this parameter on boundary
     IF(F.LT.0.1D0)THEN
      G=PEST%PARAM(IP1)%ALPHA(1)-PEST%PARAM(IP1)%ALPHA(2)
      G=G*F
      PEST%PARAM(IP1)%ALPHA(1)=PEST%PARAM(IP1)%ALPHA(2)+G
      PEST%PARAM(IP1)%PACT=-1
      !## ignore this parameter
!      IF(TRIM(PEST%PARAM(IP1)%ACRONYM).EQ.'')THEN
!       WRITE(IUPESTOUT,'(A,F10.2,A)') 'Reset lambda - parameter '//TRIM(ITOS(IP1))//' close by boundary f=',F,' snapped to it'
!      ELSE
!       WRITE(IUPESTOUT,'(A,F10.2,A)') 'Reset lambda - parameter '//TRIM(PEST%PARAM(IP1)%ACRONYM)//' close by boundary f=',F,' snapped to it'
!      ENDIF
!      FLUSH(IUPESTOUT)
      LAMBDARESET=.TRUE.; RETURN
     ENDIF
    ENDIF
   ENDIF  
  ENDDO
 
  !## corrects all gradients with this factor
  IF(F.LT.1.0D0)THEN
   
!   WRITE(*,*) 'Correct update vector=',F
   
   !## adjust all parameters
   DO IP2=1,SIZE(PEST%PARAM) 
    IF(PEST%PARAM(IP2)%PACT.NE.1)CYCLE
   
    G=PEST%PARAM(IP2)%ALPHA(1)-PEST%PARAM(IP2)%ALPHA(2)
    G=G*F

    !## update parameters
    PEST%PARAM(IP2)%ALPHA(1)=PEST%PARAM(IP2)%ALPHA(2)+G
   ENDDO

  ENDIF
 
 ENDIF
  
 !## correct update gradient found
 IPEST_GLM_UPGRADEVECTOR=.TRUE.

 !## copy gradients to all groups
 DO IP1=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(IP1)%PACT.NE.1)CYCLE
  DO IP2=1,SIZE(PEST%PARAM)
   IF(ABS(PEST%PARAM(IP2)%PIGROUP).EQ.PEST%PARAM(IP1)%PIGROUP)PEST%PARAM(IP2)%ALPHA(1)=PEST%PARAM(IP1)%ALPHA(1)
  ENDDO
 ENDDO
 
 !## save alphas for history-logging
 J=0; DO IP1=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(IP1)%PACT.EQ.0)CYCLE

  IF(PEST%PARAM(IP1)%PLOG.EQ.1)THEN
   PEST%PARAM(IP1)%ALPHA_HISTORY(ITER)=EXP(PEST%PARAM(IP1)%ALPHA(1))
  ELSE
   PEST%PARAM(IP1)%ALPHA_HISTORY(ITER)=PEST%PARAM(IP1)%ALPHA(1)
  ENDIF
  
  !## active parameter
  IF(PEST%PARAM(IP1)%PACT.EQ.1)THEN
   !## store final gradient
   J=J+1; U(J)=PEST%PARAM(IP1)%ALPHA(1)-PEST%PARAM(IP1)%ALPHA(2)
  ENDIF
  
 ENDDO

! WRITE(IUPESTOUT,*) 'final ones'
! DO I=1,SIZE(PEST%PARAM)
!  WRITE(IUPESTOUT,'(3I5,1x,2G15.8)') I,PEST%PARAM(I)%PACT,PEST%PARAM(I)%PIGROUP,PEST%PARAM(I)%ALPHA(1),PEST%PARAM(I)%ALPHA(2)
! ENDDO
! WRITE(IUPESTOUT,*) 
! FLUSH(IUPESTOUT)
 
 END FUNCTION IPEST_GLM_UPGRADEVECTOR
 
! !#####=================================================================
! LOGICAL FUNCTION IPEST_GLM_WRITESTAT_PERROR(NP,COV,LPRINT,ITER,IBATCH)
! !#####=================================================================
! IMPLICIT NONE
! REAL(KIND=DP_KIND),PARAMETER :: XBANDW=5.0D0
! INTEGER,INTENT(IN) :: NP,ITER,IBATCH
! REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(NP,NP) :: COV
! LOGICAL,INTENT(IN) :: LPRINT
! INTEGER :: I,J,IP1,IERROR
! REAL(KIND=DP_KIND) :: Z1,Z2,Z,ZW
!! LOGICAL :: LLOG
! 
! IPEST_GLM_WRITESTAT_PERROR=.FALSE.
! 
! !## The asymptotic standard parameter error is a measure of how unexplained variability in the
! !## data propagates to variability in the parameters, and is essentially an error measure for the
! !## parameters. The variance indicates the range over which a parameter value could extend without affecting model fit too adversely.
!
! IF(LPRINT)THEN 
!  WRITE(IUPESTOUT,'(/A)') 'Parameter Variance - Standard Parameter Error (standard deviation)'
!  WRITE(IUPESTOUT,'(A/)')  'Indicates the range over which a parameter value could extend without affecting model fit too much'
! ENDIF
! 
! J=0; IERROR=0
! DO I=1,SIZE(PEST%PARAM)
!  IF(PEST%PARAM(I)%PACT.EQ.1)THEN
!   J=J+1
!   IF(COV(J,J).GT.0.0)THEN 
!    PEST%PARAM(I)%ALPHA_ERROR_VARIANCE(ITER)=SQRT(COV(J,J))
!   ELSE
!    !## error value - should not happen
!    PEST%PARAM(I)%ALPHA_ERROR_VARIANCE(ITER)=-999.99D0 
!    WRITE(IUPESTOUT,*) 'Active Parameter#,',J,' Variance ',COV(J,J)
!    IERROR=IERROR+1
!   ENDIF
!   !## check whether current other parameters belong to this group
!   DO IP1=1,SIZE(PEST%PARAM)
!    !## active and follower of group
!    IF(PEST%PARAM(IP1)%PACT.EQ.2)THEN
!     IF(ABS(PEST%PARAM(IP1)%PIGROUP).EQ.PEST%PARAM(I)%PIGROUP)THEN
!      PEST%PARAM(IP1)%ALPHA_ERROR_VARIANCE(ITER)=PEST%PARAM(I)%ALPHA_ERROR_VARIANCE(ITER)
!     ENDIF
!    ENDIF
!   ENDDO
!  ENDIF
! ENDDO
! 
! IF(IERROR.GT.0)THEN
!  CALL IPEST_GLM_ERROR(IBATCH,'Errors (#'//TRIM(ITOS(IERROR))//') found in the Covariance Matrix, check your matrix, might by singular'); RETURN
! ENDIF
! 
! IF(LPRINT)THEN
!  WRITE(IUPESTOUT,*) 'Confidence Limits (96%):'; WRITE(IUPESTOUT,*)
!
!  DO I=1,SIZE(PEST%PARAM)
!   IF(PEST%PARAM(I)%PACT.EQ.1)THEN
!    ZW=PEST%PARAM(I)%ALPHA_ERROR_VARIANCE(ITER)*1.96D0
!    IF(PEST%PARAM(I)%PLOG)THEN
!     Z =EXP(PEST%PARAM(I)%ALPHA(1)) 
!     Z1=TINY(1.0)
!     IF(PEST%PARAM(I)%ALPHA(1)-ZW.LT.LOG(HUGE(1.0)))THEN
!      Z1=EXP(PEST%PARAM(I)%ALPHA(1)-ZW) 
!     ENDIF
!     Z2=HUGE(1.0)
!     IF(PEST%PARAM(I)%ALPHA(1)+ZW.LT.LOG(HUGE(1.0)))THEN
!      Z2=EXP(PEST%PARAM(I)%ALPHA(1)+ZW) 
!     ENDIF
!    ELSE
!     Z= PEST%PARAM(I)%ALPHA(1) 
!     Z1=PEST%PARAM(I)%ALPHA(1)-ZW 
!     Z2=PEST%PARAM(I)%ALPHA(1)+ZW 
!    ENDIF 
!
!    WRITE(BLINE,'(3G15.7)') Z1,Z,Z2
!    IF(PEST%PARAM(I)%ACRONYM.EQ.'')THEN
!     WRITE(IUPESTOUT,'(A2,2I5.5,I3.3,A)') PEST%PARAM(I)%PPARAM,PEST%PARAM(I)%PILS,PEST%PARAM(I)%PIZONE,ABS(PEST%PARAM(I)%PIGROUP),TRIM(BLINE)
!    ELSE
!     WRITE(IUPESTOUT,'(A15,A)') PEST%PARAM(I)%ACRONYM,TRIM(BLINE)
!    ENDIF
!
!!    LLOG=.FALSE.
!!   IF(Z1.NE.0.0.AND.Z2.NE.0.0)THEN
!!    !## ignore parameter with too high of a band for unreliability, turn if off
!!    LLOG=LOG10(Z2)-LOG10(Z1).GT.XBANDW
!!   ELSE
!!    LLOG=.TRUE.
!!   ENDIF
!!   IF(LLOG)THEN
!!   WRITE(IUPESTOUT,'(/3X,A2,2I5.5,A1,I3.3)') PEST%PARAM(I)%PPARAM,PEST%PARAM(I)%PILS,PEST%PARAM(I)%PIZONE,'-',ABS(PEST%PARAM(I)%PIGROUP)
!!    WRITE(IUPESTOUT,'(A,2G15.7)') 'This parameter too unreliable to estimate: ',Z1,Z2
!!    WRITE(IUPESTOUT,'(A/)') 'Parameter will be turned off for this cycle'
!!    PEST%PARAM(I)%PACT=-1; RETURN
!!   ENDIF
!   
!!  ELSE
!!   IF(LPRINT.AND.PEST%PARAM(I)%PACT.EQ.-1)THEN
!    IF(PEST%PARAM(I)%PACT.EQ.-1)THEN
!     WRITE(BLINE,'(3A15)') 'Insens.','Insens.','Insens.'
!     IF(PEST%PARAM(I)%ACRONYM.EQ.'')THEN
!      WRITE(IUPESTOUT,'(A2,2I5.5,I3.3,A)') PEST%PARAM(I)%PPARAM,PEST%PARAM(I)%PILS,PEST%PARAM(I)%PIZONE,ABS(PEST%PARAM(I)%PIGROUP),TRIM(BLINE)
!     ELSE
!      WRITE(IUPESTOUT,'(A15,A)') PEST%PARAM(I)%ACRONYM,TRIM(BLINE)
!     ENDIF
!    ENDIF
!   ENDIF
!  ENDDO
! ENDIF
! 
! IPEST_GLM_WRITESTAT_PERROR=.TRUE.
! 
! END FUNCTION IPEST_GLM_WRITESTAT_PERROR
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_ECHOPARAMETERS(GUPDATE,ITER)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER
 REAL(KIND=DP_KIND),INTENT(OUT) :: GUPDATE
 INTEGER :: IP1,N,I
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: GRADUPDATE
 
 WRITE(IUPESTOUT,'(/A/)') 'Upgrade Vector Parameter History:'
 WRITE(BLINE,'(A19,99(A7,I3.3))') 'Parameter',('   ITER',I,I=ITER,0,-1)
 WRITE(IUPESTOUT,'(A)') TRIM(BLINE)
  
 ALLOCATE(GRADUPDATE(ITER)); GRADUPDATE=0.0D0
 N=0
 DO IP1=1,SIZE(PEST%PARAM)

  WRITE(BLINE,'(99(F10.3))') (PEST%PARAM(IP1)%ALPHA_HISTORY(I),I=ITER,0,-1)
  
  IF(ABS(PEST%PARAM(IP1)%PACT).EQ.1.AND.PEST%PARAM(IP1)%PIGROUP.GT.0)THEN
   IF(PEST%PARAM(IP1)%ACRONYM.EQ.'')THEN
    WRITE(IUPESTOUT,'(4X,A2,2I5.5,I3.3,A)') PEST%PARAM(IP1)%PPARAM,PEST%PARAM(IP1)%PILS,PEST%PARAM(IP1)%PIZONE,ABS(PEST%PARAM(IP1)%PIGROUP),TRIM(BLINE)
   ELSE
    WRITE(IUPESTOUT,'(4X,A15,A)') PEST%PARAM(IP1)%ACRONYM,TRIM(BLINE)
   ENDIF
  
   N=N+1
   DO I=1,ITER
    GRADUPDATE(I)=GRADUPDATE(I)+(PEST%PARAM(IP1)%ALPHA_HISTORY(I)-PEST%PARAM(IP1)%ALPHA_HISTORY(I-1))**2.0D0
   ENDDO
  ENDIF

 ENDDO
 
 GRADUPDATE=SQRT(GRADUPDATE)
 WRITE(BLINE,'(19X,99F10.3)') (GRADUPDATE(I),I=ITER,1,-1)
 WRITE(IUPESTOUT,'(A/)') TRIM(BLINE)

 GUPDATE=GRADUPDATE(ITER)

 DEALLOCATE(GRADUPDATE)

 END SUBROUTINE IPEST_GLM_ECHOPARAMETERS
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_JQJ(IBATCH,MARQUARDT,JQJ,NP,LCOV) 
 !###====================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: PRINTLIMIT=50
 REAL(KIND=DP_KIND),INTENT(IN) :: MARQUARDT
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(NP,NP) :: JQJ
 INTEGER,INTENT(IN) :: NP,IBATCH
 LOGICAL,INTENT(IN) :: LCOV
 INTEGER :: I,J,IP1,IP2,II,N,IPARAM,JPARAM,ISING,IERROR
 REAL(KIND=DP_KIND) :: DF1,DF2,DJ1,DJ2,B1,CB,W,DH1,DH2,ZW,Z,Z1,Z2
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: B,JQJB
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: COR,COV
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: DIAG
 INTEGER,ALLOCATABLE,DIMENSION(:) :: INDX
 
 !## construct jqj - NORMAL MATRIX/HESSIAN
 JQJ=0.0D0; I=0; IPARAM=0
 DO IP1=1,SIZE(PEST%PARAM)                !## row

  IF(ABS(PEST%PARAM(IP1)%PACT).NE.1)CYCLE
  IPARAM=IPARAM+1; IF(PEST%PARAM(IP1)%PACT.NE.1)CYCLE

  DF1=PEST%PARAM(IP1)%PDELTA
  I=I+1; II=0; JPARAM=0
  DO IP2=1,SIZE(PEST%PARAM)  !## column

   IF(ABS(PEST%PARAM(IP2)%PACT).NE.1)CYCLE
   JPARAM=JPARAM+1; IF(PEST%PARAM(IP2)%PACT.NE.1)CYCLE

   DF2=PEST%PARAM(IP2)%PDELTA
   II=II+1
   DO J=1,MSR%NOBS
    DH1=MSR%DHG(IPARAM,J); DH2=MSR%DHL(0,J)
    DJ1=(DH1-DH2)/DF1; DH1=MSR%DHG(JPARAM,J)
    DJ2=(DH1-DH2)/DF2; W=MSR%W(J)
    JQJ(II,I)=JQJ(II,I)+(DJ1*W*DJ2)  
   ENDDO
  ENDDO
 ENDDO
 
 !## copy jqj to jqjb for covariance - further
 IF(LCOV)THEN
  IF(ALLOCATED(JQJB))DEALLOCATE(JQJB); ALLOCATE(JQJB(NP,NP))
  JQJB=JQJ
 ENDIF
 
 !## levenberg-marquardt
 ALLOCATE(DIAG(NP)); DO I=1,NP; DIAG(I)=JQJ(I,I); ENDDO !; DIAG=1.0D0
 DO I=1,NP
  DO J=1,NP
   JQJ(J,I)=JQJ(J,I)+MARQUARDT*DIAG(I)
  ENDDO
 ENDDO
 DEALLOCATE(DIAG)
 
 IF(.NOT.LCOV)RETURN

 IF(ALLOCATED(COR ))DEALLOCATE(COR);  ALLOCATE(COR(NP,NP))
 IF(ALLOCATED(COV ))DEALLOCATE(COV);  ALLOCATE(COV(NP,NP))
 IF(ALLOCATED(INDX))DEALLOCATE(INDX); ALLOCATE(INDX(NP))
 IF(ALLOCATED(B   ))DEALLOCATE(B);    ALLOCATE(B(NP,NP))

 !## compute inverse of (JQJB)-1 -> B = covariance matrix
 CALL IPEST_LUDECOMP_DBL(JQJB,INDX,NP,ISING)
 !## matrix not singular - compute inverse
 IF(ISING.EQ.0)THEN
  B=0.0D0; DO I=1,NP; B(I,I)=1.0D0; ENDDO
  DO I=1,NP; CALL IPEST_LUBACKSUB_DBL(JQJB,INDX,B(1,I),NP); ENDDO
 
  !## parameter covariance matrix
  N=MAX(1,MSR%NOBS-NP); B1=MSR%TJ/REAL(N,8)
  DO I=1,NP; DO J=1,NP; B(I,J)=B1*B(I,J); ENDDO; ENDDO

  WRITE(IUPESTOUT,'(/A/)') 'Parameter Covariance Matrix (m2):'
  CALL IPEST_GLM_WRITEHEADER('               ',NP,IUPESTOUT)
  WRITE(IUPESTOUT,'(A)')

  I=0
  DO IP1=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(IP1)%PACT.EQ.1)THEN
    I=I+1
    IF(NP.LT.PRINTLIMIT)THEN
     IF(PEST%PARAM(IP1)%ACRONYM.EQ.'')THEN
      WRITE(SLINE,'(A2,2I5.5,I3.3)') PEST%PARAM(IP1)%PPARAM,PEST%PARAM(IP1)%PILS,PEST%PARAM(IP1)%PIZONE,PEST%PARAM(IP1)%PIGROUP
     ELSE
      WRITE(SLINE,'(A15)') PEST%PARAM(IP1)%ACRONYM
     ENDIF
     WRITE(IUPESTOUT,'(A15,99999E15.7)') TRIM(SLINE),(B(I,J),J=1,NP)
    ENDIF
    DO J=1,NP; COV(I,J)=B(I,J); ENDDO
   ENDIF
  ENDDO
 
  !## parameter correlation matrix
  IF(NP.LT.PRINTLIMIT)THEN
   WRITE(IUPESTOUT,'(/A)') 'Parameter Correlation Matrix (-)'
   WRITE(IUPESTOUT,'(A)')  'Indicates whether coordinated changes in the parameter values could produce the same simulated values and'
   WRITE(IUPESTOUT,'(A/)') '  therefore, the same model fit'
   CALL IPEST_GLM_WRITEHEADER('               ',NP,IUPESTOUT)
   WRITE(IUPESTOUT,'(A)')
  ENDIF
  
  COR=0.0D0; I=0
  DO IP1=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(IP1)%PACT.EQ.1)THEN
    I=I+1
    DO J=1,NP
     CB=B(I,I)*B(J,J)
     IF(CB.GT.0.0D0)THEN
      COR(I,J)=B(I,J)/SQRT(CB)
     ELSE
      COR(I,J)=0.0D0
     ENDIF
    ENDDO
    IF(NP.LT.PRINTLIMIT)THEN
     IF(PEST%PARAM(IP1)%ACRONYM.EQ.'')THEN
      WRITE(SLINE,'(A2,2I5.5,I3.3)') PEST%PARAM(IP1)%PPARAM,PEST%PARAM(IP1)%PILS,PEST%PARAM(IP1)%PIZONE,PEST%PARAM(IP1)%PIGROUP
     ELSE
      WRITE(SLINE,'(A15)') PEST%PARAM(IP1)%ACRONYM
     ENDIF
     WRITE(IUPESTOUT,'(A15,99999F15.7)') TRIM(SLINE),(COR(I,J),J=1,NP)
    ENDIF
   ENDIF
  ENDDO

  !## write per parameter highly correlated other parameter
  WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'List of Parameter Highly Correlated (correlation > 0.95):'; WRITE(IUPESTOUT,*)
  IP1=0
  DO I=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(I)%PACT.EQ.1)THEN
    IF(PEST%PARAM(I)%ACRONYM.EQ.'')THEN
     WRITE(BLINE,'(A2,2I5.5,I3.3)') PEST%PARAM(I)%PPARAM,PEST%PARAM(I)%PILS,PEST%PARAM(I)%PIZONE,PEST%PARAM(I)%PIGROUP
    ELSE
     WRITE(BLINE,'(A15)') PEST%PARAM(I)%ACRONYM
    ENDIF
    IP1=IP1+1
    IP2=0
    II=0
    DO J=1,SIZE(PEST%PARAM)
     IF(PEST%PARAM(J)%PACT.EQ.1)THEN
      IP2=IP2+1
      IF(I.NE.J)THEN
       IF(PEST%PARAM(J)%ACRONYM.EQ.'')THEN
        WRITE(SLINE,'(A2,2I5.5,I3.3)') PEST%PARAM(J)%PPARAM,PEST%PARAM(J)%PILS,PEST%PARAM(J)%PIZONE,PEST%PARAM(J)%PIGROUP
       ELSE
        WRITE(SLINE,'(A15)') PEST%PARAM(J)%ACRONYM
       ENDIF
       IF(ABS(COR(IP1,IP2)).GE.0.95D0)THEN
        II=II+1
        BLINE=TRIM(BLINE)//','//TRIM(SLINE)
       ENDIF
      ENDIF
     ENDIF
    ENDDO
    IF(II.GT.0)WRITE(IUPESTOUT,'(A)') TRIM(BLINE)
   ENDIF  
  ENDDO
  
  WRITE(IUPESTOUT,'(/A)') 'Parameter Variance - Standard Parameter Error (standard deviation)'
  WRITE(IUPESTOUT,'(A/)')  'Indicates the range over which a parameter value could extend without affecting model fit too much'
 
  J=0; IERROR=0
  DO I=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(I)%PACT.EQ.1)THEN
    J=J+1
    IF(COV(J,J).GT.0.0)THEN 
     PEST%PARAM(I)%ALPHA_ERROR_VARIANCE=SQRT(COV(J,J))
    ELSE
     !## error value - should not happen
     PEST%PARAM(I)%ALPHA_ERROR_VARIANCE=-999.99D0 
     WRITE(IUPESTOUT,*) 'Active Parameter#,',J,' Variance ',COV(J,J)
     IERROR=IERROR+1
    ENDIF
    !## check whether current other parameters belong to this group
    DO IP1=1,SIZE(PEST%PARAM)
     !## active and follower of group
     IF(PEST%PARAM(IP1)%PACT.EQ.2)THEN
      IF(ABS(PEST%PARAM(IP1)%PIGROUP).EQ.PEST%PARAM(I)%PIGROUP)THEN
       PEST%PARAM(IP1)%ALPHA_ERROR_VARIANCE=PEST%PARAM(I)%ALPHA_ERROR_VARIANCE
      ENDIF
     ENDIF
    ENDDO
   ENDIF
  ENDDO
 
  IF(IERROR.GT.0)THEN
   CALL IPEST_GLM_ERROR(IBATCH,'Errors (#'//TRIM(ITOS(IERROR))//') found in the Covariance Matrix, check your matrix, might by singular'); RETURN
  ENDIF
 
  WRITE(IUPESTOUT,*) 'Confidence Limits (96%):'; WRITE(IUPESTOUT,*)

  DO I=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(I)%PACT.EQ.1)THEN
    ZW=PEST%PARAM(I)%ALPHA_ERROR_VARIANCE*1.96D0
    IF(PEST%PARAM(I)%PLOG)THEN
     Z =EXP(PEST%PARAM(I)%ALPHA(1)) 
     Z1=TINY(1.0)
     IF(PEST%PARAM(I)%ALPHA(1)-ZW.LT.LOG(HUGE(1.0)))THEN
      Z1=EXP(PEST%PARAM(I)%ALPHA(1)-ZW) 
     ENDIF
     Z2=HUGE(1.0)
     IF(PEST%PARAM(I)%ALPHA(1)+ZW.LT.LOG(HUGE(1.0)))THEN
      Z2=EXP(PEST%PARAM(I)%ALPHA(1)+ZW) 
     ENDIF
    ELSE
     Z= PEST%PARAM(I)%ALPHA(1) 
     Z1=PEST%PARAM(I)%ALPHA(1)-ZW 
     Z2=PEST%PARAM(I)%ALPHA(1)+ZW 
    ENDIF 

    WRITE(BLINE,'(3G15.7)') Z1,Z,Z2
    IF(PEST%PARAM(I)%ACRONYM.EQ.'')THEN
     WRITE(IUPESTOUT,'(A2,2I5.5,I3.3,A)') PEST%PARAM(I)%PPARAM,PEST%PARAM(I)%PILS,PEST%PARAM(I)%PIZONE,ABS(PEST%PARAM(I)%PIGROUP),TRIM(BLINE)
    ELSE
     WRITE(IUPESTOUT,'(A15,A)') PEST%PARAM(I)%ACRONYM,TRIM(BLINE)
    ENDIF

    IF(PEST%PARAM(I)%PACT.EQ.-1)THEN
     WRITE(BLINE,'(3A15)') 'Insens.','Insens.','Insens.'
     IF(PEST%PARAM(I)%ACRONYM.EQ.'')THEN
      WRITE(IUPESTOUT,'(A2,2I5.5,I3.3,A)') PEST%PARAM(I)%PPARAM,PEST%PARAM(I)%PILS,PEST%PARAM(I)%PIZONE,ABS(PEST%PARAM(I)%PIGROUP),TRIM(BLINE)
     ELSE
      WRITE(IUPESTOUT,'(A15,A)') PEST%PARAM(I)%ACRONYM,TRIM(BLINE)
     ENDIF
    ENDIF
   ENDIF
  ENDDO
 ELSE
  CALL IPEST_GLM_ERROR(IBATCH,'Singular matrix, cannot compute covariance matrix: skipped.')
 ENDIF

 IF(ALLOCATED(COV ))DEALLOCATE(COV)
 IF(ALLOCATED(COR ))DEALLOCATE(COR)
 IF(ALLOCATED(INDX))DEALLOCATE(INDX)
 IF(ALLOCATED(JQJB))DEALLOCATE(JQJB)
 IF(ALLOCATED(B))   DEALLOCATE(B)
 
 END SUBROUTINE IPEST_GLM_JQJ
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_EIGDECOM(IBATCH,JQJ,EIGW,EIGV,NP,LPRINT) 
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NP,IBATCH
 LOGICAL,INTENT(IN) :: LPRINT
 REAL(KIND=DP_KIND),DIMENSION(NP,NP),INTENT(IN) :: JQJ
 REAL(KIND=DP_KIND),DIMENSION(NP,NP),INTENT(OUT) :: EIGV
 REAL(KIND=DP_KIND),DIMENSION(NP),INTENT(OUT) :: EIGW
 REAL(KIND=DP_KIND) :: DET
 INTEGER :: I
 REAL(KIND=DP_KIND) :: TV,TEV,KAPPA
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: B
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: E
 
 IF(ALLOCATED(E))DEALLOCATE(E); ALLOCATE(E(NP))
 IF(ALLOCATED(B))DEALLOCATE(B); ALLOCATE(B(NP,NP))

 !## compute determinant of JQJ
 DET=IPEST_GLM_DET(JQJ,NP)

 IF(LPRINT)THEN
  WRITE(IUPESTOUT,'(/A18,E15.7)') 'Determinant JQJ+LAMBDA*DIAG(JQJ) = ',DET
  WRITE(IUPESTOUT,'(A/)') 'A small value for the Determinant indicates Singularity of the Matrix'
 ENDIF

 !## copy jqj to b for eigenvalue decomposition
 B=JQJ
  
 !## eigenvalue of covariance matrix 
 CALL LUDCMP_TRED2(B,NP,NP,EIGW,E)
 CALL LUDCMP_TQLI(EIGW,E,NP,NP,B)
 CALL LUDCMP_EIGSRT(EIGW,B,NP,NP)

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
  CALL IPEST_GLM_ERROR(IBATCH,'Warning, there is NO information (no eigenvalues) in parameter perturbation'); STOP
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
 
 IF(ALLOCATED(E))DEALLOCATE(E)
 IF(ALLOCATED(B))DEALLOCATE(B)
 
 END SUBROUTINE IPEST_GLM_EIGDECOM
 
 !###========================================================================
 SUBROUTINE IPEST_GLM_WRITEHEADER(TXT,NP,IU)
 !###========================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: TXT
 INTEGER,INTENT(IN) :: IU,NP
 INTEGER :: I,J,N
 CHARACTER(LEN=15),ALLOCATABLE,DIMENSION(:) :: CTMP
   
 ALLOCATE(CTMP(NP))
   
 N=0
 DO J=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(J)%PACT.EQ.1.AND.PEST%PARAM(J)%PIGROUP.GT.0)THEN
   N=N+1
   IF(TRIM(PEST%PARAM(J)%ACRONYM).EQ.'')THEN
    WRITE(CTMP(N),'(A2,2I5.5,I3.3)') PEST%PARAM(J)%PPARAM,PEST%PARAM(J)%PILS,PEST%PARAM(J)%PIZONE,PEST%PARAM(J)%PIGROUP
   ELSE
    WRITE(CTMP(N),'(A15)') PEST%PARAM(J)%ACRONYM
   ENDIF
  ENDIF
 ENDDO
 WRITE(IU,'(A15,99999A15)') TXT,(CTMP(I),I=1,NP) 
 
 DEALLOCATE(CTMP)
 
 END SUBROUTINE IPEST_GLM_WRITEHEADER
  
 !###========================================================================
 DOUBLE PRECISION FUNCTION IPEST_GLM_DET(JQJ,N)
 !###========================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 DOUBLE PRECISION,DIMENSION(N,N),INTENT(IN) :: JQJ
 DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE :: MATRIX
 DOUBLE PRECISION :: M, TEMP
 INTEGER :: I, J, K, L
 LOGICAL :: DETEXISTS = .TRUE.

 ALLOCATE(MATRIX(N,N))
 MATRIX=JQJ
 
 L = 1
 !## convert to upper triangular form
 DO K = 1, N-1
  IF (MATRIX(K,K).EQ.0.0D0) THEN
   DETEXISTS = .FALSE.
   DO I = K+1, N
    IF (MATRIX(I,K).NE.0.0D0) THEN
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
    IPEST_GLM_DET = 0.0D0
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
 IPEST_GLM_DET = L
 DO I = 1, N
  IPEST_GLM_DET = IPEST_GLM_DET * MATRIX(I,I)
 END DO

 DEALLOCATE(MATRIX)
    
 END FUNCTION IPEST_GLM_DET
 
 !###====================================================================
 LOGICAL FUNCTION IPEST_GLM_GETJ(DIR,IGRAD,IPARAM,CTYPE,IBATCH,TNSC)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,CTYPE
 REAL(KIND=DP_KIND),INTENT(OUT) :: TNSC
 INTEGER,INTENT(IN) :: IGRAD,IPARAM,IBATCH
 INTEGER :: I,J,II,NC,NP,NPERIOD,III,K,KK,ILAY,NROWIPFTXT,IUIPFTXT,NCOLIPFTXT,IOS,NAJ,NNSC,IERROR,SEED
 REAL(KIND=DP_KIND) :: X,Y,Z,H,WW,MC,MM,DHH,XCOR,YCOR,ZCOR,DHW,DP,F,DTH,DTD,NSC,SIGMA
 CHARACTER(LEN=256) :: DIRNAME,FNAME
 CHARACTER(LEN=52) :: CID,TXT
 CHARACTER(LEN=12) :: CEXT
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: TSNODATA,M,C,MCOPY,CCOPY
 INTEGER(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: IDATE,ID1,ID2
 REAL(KIND=DP_KIND),DIMENSION(2) :: PC,PM,DYN !## percentiles computed/measured
 INTEGER :: IU,NR,IEXT

 IPEST_GLM_GETJ=.FALSE.
 
 SEED=12345
 
 IF(.NOT.ASSOCIATED(PEST%MEASURES))RETURN
 
 IF(PBMAN%IIES.EQ.1)THEN
  DIRNAME=TRIM(DIR)//'\IIES_'//TRIM(CTYPE)//'#'//TRIM(ITOS(IPARAM))//'\TIMESERIES'
 ELSE  
  DIRNAME=TRIM(DIR)//'\IPEST_'//TRIM(CTYPE)//'#'//TRIM(ITOS(IPARAM))//'\TIMESERIES'
 ENDIF
 
 WRITE(*,'(A)') 'Getting residual for '//TRIM(CTYPE)//'#'//TRIM(ITOS(IPARAM))//' ...'

 !## initialise variables
 II=0; MSR%TJ=0.0D0; MSR%RJ=0.0D0

 NPERIOD=0; IF(ASSOCIATED(PEST%S_PERIOD))NPERIOD=SIZE(PEST%S_PERIOD)
 IF(NPERIOD.GT.0)THEN
  ALLOCATE(ID1(NPERIOD),ID2(NPERIOD))
  DO I=1,NPERIOD
   READ(PEST%S_PERIOD(I),*) ID1
   READ(PEST%E_PERIOD(I),*) ID2
  ENDDO
 ENDIF
 
 !## write header
 IF(TRIM(CTYPE).EQ.'L'.AND.IUPESTRESIDUAL.GT.0)THEN
  DO I=1,SIZE(PEST%MEASURES)
   J=INDEX(PEST%MEASURES(I)%IPFNAME,'\',.TRUE.)+1
   FNAME=TRIM(DIRNAME)//'\'//TRIM(PEST%MEASURES(I)%IPFNAME(J:))
   WRITE(IUPESTRESIDUAL,'(I10,A)') I,','//TRIM(FNAME)
  ENDDO
 ENDIF
 
 !## process files
 TNSC=0.0D0; NNSC=0
 
 DO I=1,SIZE(PEST%MEASURES)

  J=INDEX(PEST%MEASURES(I)%IPFNAME,'\',.TRUE.)+1
  FNAME=TRIM(DIRNAME)//'\'//TRIM(PEST%MEASURES(I)%IPFNAME(J:))
  IU=UTL_GETUNIT(); OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ')
  READ(IU,*) NR
  READ(IU,*) NC
  DO J=1,NC; READ(IU,*); ENDDO; READ(IU,*) IEXT,CEXT
  
  IF(TRIM(CTYPE).EQ.'L'.OR.TRIM(CTYPE).EQ.'R'.AND.I.EQ.1)THEN
   IF(IUPESTRESIDUAL.GT.0)THEN
    !## steady-state
    IF(IEXT.EQ.0)THEN
     WRITE(IUPESTRESIDUAL,'(2A16,A11,6A16,A11,A5)') 'X,','Y,','ILAY,','MSR,','MDL,', &
            'J,','WMDL,','WRESIDUAL,','WEIGH,','IPF,','LABEL'
    !## transient
    ELSE  
     WRITE(IUPESTRESIDUAL,'(2A16,A11,8A16,A11,A5,27X,1X,A15)') 'X,','Y,','ILAY,','WEIGH,','MSR,','MDL,','MDL-MSR,', &
            'DYNMSR,','DYNMDL,','DYNMSR-DYNMDL,','NASH-SUTCLIFF,','IPF,','LABEL','DATE'
    ENDIF
   ENDIF
  ENDIF

  IF(IEXT.EQ.0)THEN

   DO J=1,NR
   
    II=II+1
    READ(IU,*) X,Y,ILAY,Z,WW,H

    !## entries in stdev - weight is 1/stdev2
    IF(PEST%MEASURES(I)%IVCOL.GT.0)THEN
     IF(WW.LT.0.0D0)THEN; WW=0.0D0; ELSE; WW=1.0D0/WW**2.0D0; ENDIF
    ENDIF

    !## weight as ... weigth    
    MSR%W(II)=WW
    
    !## random error not yet set
    IF(PBMAN%IIES.EQ.1)THEN
     IERROR=IGRAD
     IF(MSR%E(IERROR,II).LT.0.0D0)THEN
      IF(WW.NE.0.0D0)THEN
       !## compute stdev from weights again
       SIGMA=1.0D0/SQRT(WW); CALL IPEST_NORMAL_MS_SAMPLE(0.0D0,SIGMA,SEED,MSR%E(IERROR,II))
      ELSE
       MSR%E(IERROR,II)=0.0D0
      ENDIF
     ENDIF
    ELSE
     IERROR=1; MSR%E(IERROR,II)=0.0D0
    ENDIF
    
!    IU=UTL_GETUNIT(); OPEN(IU,FILE='d:\IMOD-MODELS\IES\CREATEENSEMBLES\NORMALPDF.TXT',STATUS='UNKNOWN',ACTION='WRITE')
!    DO K=1,1000
!     CALL IPEST_NORMAL_MS_SAMPLE(0.0D0,SIGMA,SEED,MSR%E(II,IERROR))
!     WRITE(IU,*) K,MSR%E(II,IERROR)
!    ENDDO
!    CLOSE(IU)
!    STOP
    
    !## add random error
    Z=Z+MSR%E(IERROR,II)
    
    !## calculated - measured
    DHH=H-Z
    IF(ABS(DHH).LT.PEST%PE_DRES)THEN
     DHH=0.0D0
    ELSE
     IF(DHH.GT. PEST%PE_DRES)DHH=DHH-PEST%PE_DRES
     IF(DHH.LT.-PEST%PE_DRES)DHH=DHH+PEST%PE_DRES
    ENDIF

    !## save information for measurement
    IF(TRIM(CTYPE).EQ.'P'.OR.TRIM(CTYPE).EQ.'R')THEN
     MSR%DHG(IGRAD,II)=DHH  
    ELSEIF(TRIM(CTYPE).EQ.'L')THEN
     MSR%DHL(IGRAD,II)=DHH  
    ENDIF
    
    MSR%X(II)=X; MSR%Y(II)=Y; MSR%L(II)=ILAY
    MSR%CLABEL(II)='MEASURE'//TRIM(ITOS(J))//'_IPF'//TRIM(ITOS(I))

    GF_H(II)=MSR%W(II)*H; GF_O(II)=MSR%W(II)*Z

    !## add to total objective function
    DHW=MSR%W(II)*(DHH**2.0D0); MSR%TJ=MSR%TJ+DHW
    IF(IUPESTRESIDUAL.GT.0.AND.TRIM(CTYPE).EQ.'L'.OR.TRIM(CTYPE).EQ.'R')THEN
     WRITE(IUPESTRESIDUAL,'(2(F15.2,A1),I10,A1,6(F15.3,A1),I10,A1,A32)') &
        X,',',Y,',',ILAY,',',Z,',',H,',',DHW,',',MSR%W(II)*H,',',MSR%W(II)*DHH,',',MSR%W(II),',',I,',',MSR%CLABEL(II)
    ENDIF
    
   ENDDO
  
  !## transient
  ELSE
    
   IUIPFTXT=0
   DO J=1,NR

    READ(IU,*) X,Y,ILAY,CID,WW   
    !## input is a variance - convert it to a weight as w=1/stdev^2
    IF(PEST%MEASURES(I)%IVCOL.GT.0)THEN
     IF(WW.LE.0.0D0)THEN; WW=0.0D0; ELSE; WW=1.0D0/WW**2.0D0; ENDIF
    ENDIF

    LINE=TRIM(DIRNAME)//CHAR(92)//TRIM(CID)//'.'//TRIM(CEXT)
    IF(IUIPFTXT.EQ.0)IUIPFTXT=UTL_GETUNIT()
    OPEN(IUIPFTXT,FILE=LINE,STATUS='OLD',ACTION='READ')
    
    READ(IUIPFTXT,*) NROWIPFTXT; READ(IUIPFTXT,*) NCOLIPFTXT

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

     !## error reading, skip it (can be caused by steady-state periods in between)
     IF(IOS.NE.0)THEN; KK=KK-1; CYCLE; ENDIF

     !## make double precision dates - if needed
     IF(IDATE(KK).LT.100000000)IDATE(KK)=IDATE(KK)*1000000

     !## check period (if available)
     IF(NPERIOD.GT.0)THEN
      DO III=1,NPERIOD; IF(IDATE(KK).GE.ID1(III).AND.IDATE(KK).LE.ID2(III))EXIT; ENDDO
      IF(III.GT.NPERIOD)C(KK)=TSNODATA(3)
     ENDIF

     IF(M(KK).EQ.TSNODATA(2).OR.C(KK).EQ.TSNODATA(3))KK=KK-1
    ENDDO 

    !## add this measurement
    IF(KK.GT.0)THEN
    
     !## compute mean measurement in period
     XCOR=-9999.99D0
     NSC =-9999.99D0
     DYN =-9999.99D0

     !## mean values
     MM=SUM(M(1:KK))/DBLE(KK) !## measurements
     MC=SUM(C(1:KK))/DBLE(KK) !## computed

     IF(PEST%PE_TARGET(2).GT.0.0D0)THEN
      DO K=1,KK; MCOPY(K)=M(K); CCOPY(K)=C(K); ENDDO
      !## percentiles
      CALL UTL_GETMED(MCOPY,KK,-999.99D0,(/10.0D0,90.0D0/),2,NAJ,PM)
      CALL UTL_GETMED(CCOPY,KK,-999.99D0,(/10.0D0,90.0D0/),2,NAJ,PC)
      !## measurements
      DYN(1)=PM(2)-PM(1) 
      !## computed
      DYN(2)=PC(2)-PC(1) 
     ENDIF
     !## compute cross-correlation
     IF(KK.GT.1)THEN
      XCOR=0.0D0; YCOR=0.0D0; ZCOR=0.0D0
      DO K=1,KK
       XCOR=XCOR+(MM-M(K))*(MC-C(K)); YCOR=YCOR+(MM-M(K))**2.0D0; ZCOR=ZCOR+(MC-C(K))**2.0D0
      ENDDO
      IF(YCOR.NE.0.0.AND.ZCOR.NE.0.0)XCOR=XCOR/(SQRT(YCOR)*SQRT(ZCOR))
      NSC =UTL_NASH_SUTCLIFFE(M,C,KK)
      TNSC=TNSC+NSC
      NNSC=NNSC+1
     ENDIF
     
     !## add observation
     DO K=1,KK
      II =II+1

      !## random error not yet set
      IF(PBMAN%IIES.EQ.1)THEN
       IERROR=IGRAD
       IF(MSR%E(IERROR,II).LT.0.0D0)THEN
        IF(WW.NE.0.0D0)THEN
         !## variance
         SIGMA=1.0D0/SQRT(WW); CALL IPEST_NORMAL_MS_SAMPLE(0.0D0,SIGMA,SEED,MSR%E(IERROR,II))
        ELSE
         MSR%E(IERROR,II)=0.0D0
        ENDIF
       ENDIF
      ELSE
       IERROR=1; MSR%E(IERROR,II)=0.0D0
      ENDIF
      
      !## add measurement noise
      C(K)=C(K)+MSR%E(IERROR,II)
      
      DTH=0.0D0
      !## target is residual (calculated minus measured)
      IF(PEST%PE_TARGET(1).GT.0.0D0)THEN
       DTH=C(K)-M(K)
       IF(ABS(DTH).LT.PEST%PE_DRES)THEN
        DTH=0.0D0
       ELSE
        IF(DTH.GT. PEST%PE_DRES)DTH=DTH-PEST%PE_DRES
        IF(DTH.LT.-PEST%PE_DRES)DTH=DTH+PEST%PE_DRES
       ENDIF
       DTH=PEST%PE_TARGET(1)*DTH
      ENDIF
      
      !## target is dynamics (calculated minus measured)
      DTD=0.0D0
      IF(PEST%PE_TARGET(2).GT.0.0D0)THEN
       DTD=DYN(2)-DYN(1)
       IF(ABS(DTD).LT.PEST%PE_DRES)THEN
        DTD=0.0D0
       ELSE
        IF(DTD.GT. PEST%PE_DRES)DTD=DTD-PEST%PE_DRES
        IF(DTD.LT.-PEST%PE_DRES)DTD=DTD+PEST%PE_DRES
       ENDIF
       DTD=PEST%PE_TARGET(2)*DTD
      ENDIF
      
      DHH=DTH+DTD
      
      !## calculated - measured
      IF(TRIM(CTYPE).EQ.'P'.OR.TRIM(CTYPE).EQ.'R')THEN
       MSR%DHG(IGRAD,II)=DHH
      ELSEIF(TRIM(CTYPE).EQ.'L')THEN
       MSR%DHL(IGRAD,II)=DHH
      ENDIF

      !## save information for measurement
      MSR%X(II)=X
      MSR%Y(II)=Y
      MSR%L(II)=ILAY
      MSR%CLABEL(II)=TRIM(CID)

      !## weight, pest_itarget(.) should/will be summed to one
      MSR%W(II)=WW

      !## add to total objective function
      DHW=MSR%W(II)*(DHH**2.0D0); MSR%TJ=MSR%TJ+DHW

      GF_H(II)=MSR%W(II)*C(K); GF_O(II)=MSR%W(II)*M(K)

      IF(IUPESTRESIDUAL.GT.0.AND.TRIM(CTYPE).EQ.'L'.OR.TRIM(CTYPE).EQ.'R')THEN
       WRITE(IUPESTRESIDUAL,'(2(F15.2,A1),I10,A1,8(F15.2,A1),I10,A1,A32,A1,I15)') &
         X,',',Y,',',ILAY,',',WW,',',M(K),',',C(K),',',DTH,',',DYN(1),',',DYN(2),',',DTD,',',XCOR,',',I,',',MSR%CLABEL(II),',',IDATE(K)
      ENDIF
      
      IF(PEST%PE_TARGET(1).EQ.0.0D0.AND.PEST%PE_TARGET(2).GT.0.0D0)EXIT
     
     ENDDO
     
    ENDIF
    
    DEALLOCATE(TSNODATA,C,M,MCOPY,CCOPY,IDATE)
    CLOSE(IUIPFTXT)

   ENDDO
  ENDIF

  CLOSE(IU)

 ENDDO
 MSR%NOBS=II
 
 IF(MSR%NOBS.LE.0)THEN
  IF(IBATCH.EQ.1)WRITE(*,'(/A/)') 'No measurements available within current spatial/temporal space.'
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No measurements available within current spatial/temporal space.','Error')
  RETURN
 ENDIF
 
! !## run batch files
! CALL PEST_BATCHFILES()
!
 !## insert regularisation to objective function
 NP=0; DO I=1,SIZE(PEST%PARAM); IF(PEST%PARAM(I)%PACT.EQ.0.OR.PEST%PARAM(I)%PIGROUP.LE.0)CYCLE; NP=NP+1; ENDDO
 
 !## add parameter regularisation
 IF(PEST%PE_REGULARISATION.EQ.1)THEN
  DO I=1,SIZE(PEST%PARAM)  !## row
   !## skip inactive
   IF(PEST%PARAM(I)%PACT.EQ.0)CYCLE
   !## skip others parts of parameter
   IF(PEST%PARAM(I)%PIGROUP.LE.0)CYCLE
   IF(PEST%PARAM(I)%PLOG.EQ.1)THEN
    F=EXP(PEST%PARAM(I)%ALPHA(1))-EXP(PEST%PARAM(I)%PPRIOR)
   ELSE
    F=    PEST%PARAM(I)%ALPHA(1) -    PEST%PARAM(I)%PPRIOR
   ENDIF
   DP=F**2.0D0
   DP=DP*PEST%PE_REGFACTOR
   MSR%RJ=MSR%RJ+DP
  ENDDO
 ENDIF

 MSR%TJ=MSR%TJ+MSR%RJ

 WRITE(*,'(A)') '>>> Finished <<<'

 IPEST_GLM_GETJ=.TRUE.

 END FUNCTION IPEST_GLM_GETJ

 !#####=================================================================
 SUBROUTINE IPEST_GLM_PROGRESS(ITER,IGRAD,ILIN,CTYPE)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: CTYPE
 INTEGER,INTENT(IN) :: ITER,IGRAD,ILIN
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: X
 CHARACTER(LEN=15),ALLOCATABLE,DIMENSION(:) :: CTMP
 INTEGER :: I,J,N
 
 ALLOCATE(X(SIZE(PEST%PARAM)))

 DO I=1,SIZE(PEST%PARAM)
  IF(TRIM(CTYPE).EQ.'P')THEN
   X(I)=PEST%PARAM(I)%GALPHA(IGRAD)
  ELSEIF(TRIM(CTYPE).EQ.'L')THEN
   X(I)=PEST%PARAM(I)%LALPHA(ILIN)
  ENDIF
  IF(PEST%PARAM(I)%PLOG)X(I)=EXP(X(I))
 ENDDO

 N=0; DO J=1,SIZE(PEST%PARAM)
  IF(ABS(PEST%PARAM(J)%PACT).EQ.1)N=N+1
 ENDDO
 ALLOCATE(CTMP(N))

 IF(ILIN.EQ.1.AND.TRIM(CTYPE).EQ.'L')THEN
  WRITE(BLINE,'(3A5,2A15)') 'IT','GD','LS','TOT_J','RED_J'
  N=0; DO J=1,SIZE(PEST%PARAM)
   IF(ABS(PEST%PARAM(J)%PACT).EQ.1)THEN
    N=N+1; WRITE(CTMP(N),'(3X,A2,2I5.5)') PEST%PARAM(J)%PPARAM,PEST%PARAM(J)%PILS,PEST%PARAM(J)%PIZONE
   ENDIF
  ENDDO
  WRITE(IUPESTPROGRESS,'(/A,99999A15)') TRIM(BLINE),(CTMP(I),I=1,N)
  N=0; DO J=1,SIZE(PEST%PARAM)
   IF(ABS(PEST%PARAM(J)%PACT).EQ.1)THEN
    N=N+1; WRITE(CTMP(N),'(10X,I5.5)') PEST%PARAM(J)%PIGROUP
   ENDIF
  ENDDO
  WRITE(IUPESTPROGRESS,'(45X,99999A15)') (CTMP(I),I=1,N) 
  N=0; DO J=1,SIZE(PEST%PARAM)
   IF(ABS(PEST%PARAM(J)%PACT).EQ.1)THEN
    N=N+1; WRITE(CTMP(N),'(A15)') PEST%PARAM(J)%ACRONYM
   ENDIF
  ENDDO
  WRITE(IUPESTPROGRESS,'(45X,99999A15/)') (CTMP(I),I=1,N)
 ENDIF
  
 IF(ITER.EQ.0)THEN
  WRITE(BLINE,'(3I5, F15.2,14X,A1)') ITER,IGRAD,ILIN,MSR%TJ,'-'
 ELSE
  WRITE(BLINE,'(3I5,2F15.2)') ITER,IGRAD,ILIN,MSR%TJ,MSR%TJ-MSR%PJ
 ENDIF
 N=0; DO I=1,SIZE(PEST%PARAM)
  IF(ABS(PEST%PARAM(I)%PACT).EQ.1)THEN
   N=N+1; WRITE(CTMP(N),'(F15.4)') X(I)
  ENDIF
 ENDDO

 WRITE(IUPESTPROGRESS,'(A45,99999A15)') TRIM(BLINE),(CTMP(I),I=1,N)
 
 DEALLOCATE(X,CTMP)
 
 END SUBROUTINE IPEST_GLM_PROGRESS
 
 !###====================================================================
 LOGICAL FUNCTION IPEST_GLM_ALLOCATEMSR(N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER :: N1,N2,M,IOS
 
 IPEST_GLM_ALLOCATEMSR=.FALSE.
 
 !## get dimension of possible number of observations
 M=N*PRJNPER 
 
 CALL IPEST_GLM_DEALLOCATEMSR()
 !## number of line-searches (-) and number of gradients-simulations (+)
 N1=SIZE(RNL); N2=SIZE(RNG)
 ALLOCATE(MSR%DHL(0:N1,M),STAT=IOS); IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%DHL';    RETURN; ENDIF
 ALLOCATE(MSR%DHG(N2,M),STAT=IOS);   IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%DHG';    RETURN; ENDIF
 ALLOCATE(MSR%W(M),STAT=IOS);        IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%W';      RETURN; ENDIF
 ALLOCATE(MSR%X(M),STAT=IOS);        IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%X';      RETURN; ENDIF
 ALLOCATE(MSR%Y(M),STAT=IOS);        IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%Y';      RETURN; ENDIF
 ALLOCATE(MSR%L(M),STAT=IOS);        IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%L';      RETURN; ENDIF
 ALLOCATE(MSR%CLABEL(M),STAT=IOS);   IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%CLABEL'; RETURN; ENDIF
 ALLOCATE(GF_H(M),STAT=IOS);         IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR GF_H';       RETURN; ENDIF
 ALLOCATE(GF_O(M),STAT=IOS);         IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR GF_O';       RETURN; ENDIF
 
 !## allocate stochastic error per measurement/ensemble
 IF(PBMAN%IIES.EQ.1)THEN
  ALLOCATE(MSR%E(PEST%NREALS,M))
 ELSE
  ALLOCATE(MSR%E(1,M)) 
 ENDIF
 !# initialise error
 MSR%E=-999.99D0

 IPEST_GLM_ALLOCATEMSR=.TRUE.

 END FUNCTION IPEST_GLM_ALLOCATEMSR
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_DEALLOCATEMSR()
 !###====================================================================
 IMPLICIT NONE

 IF(ASSOCIATED(MSR%DHL))   DEALLOCATE(MSR%DHL)
 IF(ASSOCIATED(MSR%DHG))   DEALLOCATE(MSR%DHG)
 IF(ASSOCIATED(MSR%W ))    DEALLOCATE(MSR%W) 
 IF(ASSOCIATED(MSR%X ))    DEALLOCATE(MSR%X) 
 IF(ASSOCIATED(MSR%Y ))    DEALLOCATE(MSR%Y) 
 IF(ASSOCIATED(MSR%L ))    DEALLOCATE(MSR%L) 
 IF(ASSOCIATED(MSR%CLABEL))DEALLOCATE(MSR%CLABEL) 
 IF(ALLOCATED(GF_H))       DEALLOCATE(GF_H)
 IF(ALLOCATED(GF_O))       DEALLOCATE(GF_O)
 
 END SUBROUTINE IPEST_GLM_DEALLOCATEMSR
 
! !###====================================================================
! REAL FUNCTION IPEST_GOODNESS_OF_FIT(X,Y,N)
! !###====================================================================
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: N
! REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(N) :: X,Y !## x=head; y=obs
! REAL(KIND=DP_KIND) :: XN,YN,YA
! INTEGER :: I
 
! !## compute nash-sutcliff
! IPEST_GOODNESS_OF_FIT=0.0D0
 
! !## average observation
! YA=0.0D0; DO I=1,N; YA=YA+Y(I)          ; ENDDO; YA=YA/REAL(N)
! XN=0.0D0; DO I=1,N; XN=XN+ABS(Y(I)-X(I)); ENDDO; XN=XN**2.0D0
! YN=0.0D0; DO I=1,N; YN=YN+ABS(Y(I)-YA)  ; ENDDO; YN=YN**2.0D0
 
! IPEST_GOODNESS_OF_FIT=1.0D0-XN/YN

! END FUNCTION IPEST_GOODNESS_OF_FIT 
 
 !###====================================================================
 SUBROUTINE IPEST_LUDECOMP_DBL(AA,IDX,N,ISING)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: NMAX=2000
 REAL(KIND=DP_KIND),PARAMETER :: TINY=1.0D-20
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT) :: ISING
 INTEGER,DIMENSION(N),INTENT(OUT) :: IDX
 REAL(KIND=DP_KIND),DIMENSION(N,N),INTENT(INOUT) :: AA
 REAL(KIND=DP_KIND),DIMENSION(NMAX) :: VV
 REAL(KIND=DP_KIND) :: AAMAX,DUM,SUM
 INTEGER :: I,IMAX,J,K

 DO I=1,N
  IDX(I)=0
 END DO
 ISING=0

 DO I=1,N
  AAMAX=0.0D0
  DO J=1,N
   IF(ABS(AA(I,J)).GT.AAMAX)AAMAX=ABS(AA(I,J))
  ENDDO
  IF(AAMAX.EQ.0.0D0)THEN
   WRITE(*,*) 'Matrix is singular'
   ISING=1
   RETURN
  ENDIF
  VV(I)=1.0D0/AAMAX
 ENDDO
 DO J=1,N
  DO I=1,J-1
   SUM=AA(I,J)
   DO K=1,I-1
    SUM=SUM-AA(I,K)*AA(K,J)
   ENDDO
   AA(I,J)=SUM
  ENDDO
  AAMAX=0.0D0
  DO I=J,N
   SUM=AA(I,J)
   DO K=1,J-1
    SUM=SUM-AA(I,K)*AA(K,J)
   ENDDO
   AA(I,J)=SUM
   DUM=VV(I)*ABS(SUM)
   IF(DUM.GE.AAMAX)THEN
    IMAX=I
    AAMAX=DUM
   ENDIF
  ENDDO
  IF(J.NE.IMAX)THEN
   DO K=1,N
    DUM=AA(IMAX,K)
    AA(IMAX,K)=AA(J,K)
    AA(J,K)=DUM
   ENDDO
   VV(IMAX)=VV(J)
  ENDIF
  IDX(J)=IMAX
  IF(AA(J,J).EQ.0.0D0)AA(J,J)=TINY
  IF(J.NE.N)THEN
   DUM=1.0D0/AA(J,J)
   DO I=J+1,N
    AA(I,J)=AA(I,J)*DUM
   ENDDO
  ENDIF
 ENDDO

 END SUBROUTINE IPEST_LUDECOMP_DBL

 !###====================================================================
 SUBROUTINE IPEST_LUBACKSUB_DBL(AA,IDX,BB,N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),DIMENSION(N,N),INTENT(IN) :: AA
 REAL(KIND=DP_KIND),DIMENSION(N),INTENT(INOUT) :: BB
 INTEGER,DIMENSION(N),INTENT(IN) :: IDX
 INTEGER :: I,II,J,LL
 REAL(KIND=DP_KIND) :: SUM

 II=0
 DO I=1,N
  LL=IDX(I)
  SUM=BB(LL)
  BB(LL)=BB(I)
  IF(II.NE.0)THEN
   DO J=II,I-1
    SUM=SUM-AA(I,J)*BB(J)
   ENDDO
  ELSE IF(SUM.NE.0.0D0)THEN
   II=I
  ENDIF
  BB(I)=SUM
 ENDDO
 DO I=N,1,-1
  SUM=BB(I)
  DO J=I+1,N
   SUM=SUM-AA(I,J)*BB(J)
  ENDDO
  BB(I)=SUM/AA(I,I)
 ENDDO

 END SUBROUTINE IPEST_LUBACKSUB_DBL
 
 !###====================================================================
 SUBROUTINE IPEST_ECHELON_DBL(AO,BO,NROW,NCOL)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL
 REAL(KIND=DP_KIND),DIMENSION(NCOL,NROW),INTENT(IN) :: AO
 INTEGER,DIMENSION(NROW),INTENT(OUT) :: BO
 REAL(KIND=DP_KIND),PARAMETER :: TINY=1.0D-10
 REAL(KIND=DP_KIND),DIMENSION(:,:),ALLOCATABLE :: A
 INTEGER,DIMENSION(:),ALLOCATABLE :: B
 INTEGER :: IROW,JROW,KROW,ICOL,PCOL
 REAL(KIND=DP_KIND) :: AMAX,ATMP,F
 
 !## copy matrix
 ALLOCATE(A(NCOL,NROW),B(NROW)); A=AO
 
 PCOL=0
MLOOP: DO IROW=1,NROW
  !## assume next column to be pivot column
  PCOL=PCOL+1; IF(PCOL.GT.NCOL)EXIT
  !## find pivot icol
PLOOP: DO
   DO JROW=IROW+1,NROW
    IF(ABS(A(PCOL,JROW)).GT.TINY)EXIT PLOOP
   ENDDO
   PCOL=PCOL+1
   !## finished
   IF(PCOL.GT.NCOL)EXIT MLOOP
  ENDDO PLOOP

  !## find row with largest pivot value
  KROW=IROW; AMAX=0.0D0; DO JROW=IROW,NROW !+1,NROW
   IF(ABS(A(PCOL,JROW)).GT.ABS(AMAX))THEN; AMAX=A(PCOL,JROW); KROW=JROW; ENDIF
  ENDDO

  !## interchange rows
  IF(KROW.NE.IROW)THEN
   DO ICOL=PCOL,NCOL
    ATMP        =A(ICOL,IROW)
    A(ICOL,IROW)=A(ICOL,KROW)
    A(ICOL,KROW)=ATMP
   ENDDO
  ENDIF
  !## reduce all rows using the pivot row
  AMAX=A(PCOL,IROW)
  DO JROW=IROW+1,NROW
   F=A(PCOL,JROW)/AMAX
   DO ICOL=PCOL,NCOL
    A(ICOL,JROW)=A(ICOL,JROW)-A(ICOL,IROW)*F
   ENDDO
  ENDDO
 ENDDO MLOOP

! DO IROW=1,NROW
!  WRITE(*,'(99F10.2)') (A(ICOL,IROW),ICOL=1,NCOL)
! ENDDO
 
 !## determine over/bad dimension of matrix
 B=0; DO IROW=1,NROW
  DO ICOL=1,NCOL; IF(ABS(A(ICOL,IROW)).GT.TINY)THEN; B(IROW)=ICOL; EXIT; ENDIF; ENDDO
 ENDDO
 BO=0; DO IROW=1,NROW; ICOL=B(IROW); IF(ICOL.GT.0)BO(ICOL)=1; ENDDO
 
 DEALLOCATE(A,B)
 
 END SUBROUTINE IPEST_ECHELON_DBL
 
END MODULE MOD_IPEST_GLM
