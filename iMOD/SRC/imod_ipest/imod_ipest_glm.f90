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
MODULE MOD_IPEST_GLM

USE WINTERACTER
USE MOD_IDF_PAR
USE MOD_IDF, ONLY : IDFNULLIFY,IDFALLOCATEX,IDFWRITE,IDFREAD,IDFDEALLOCATEX,IDFWRITEFREE_ROW
USE IMODVAR, ONLY : DP_KIND
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_PMANAGER_PAR, ONLY : PEST,SIM,PARAM,PRJNLAY,PBMAN,PRJNPER,BND
USE MOD_PMANAGER_UTL, ONLY : PMANAGER_SAVEMF2005_MOD_U2DREL
USE MOD_UTL, ONLY  : UTL_GETUNIT,UTL_CAP,ITOS,RTOS,UTL_GETMED,UTL_CREATEDIR,UTL_GOODNESS_OF_FIT,UTL_NASH_SUTCLIFFE
USE MOD_IPEST_GLM_PAR
USE MOD_LUDCMP

CONTAINS

 !#####=================================================================
 SUBROUTINE IPEST_GLM_MAIN(DIR,MNAME,IBATCH)
 !#####=================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND) :: GAMMA=4.0D0 !## lambda increase/decrease
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),INTENT(IN) :: DIR,MNAME
 REAL(KIND=DP_KIND) :: LAMBDA
 INTEGER :: I,J,ITER,N,IX
 
 PEST%PE_SCALING=PEST%PE_SCALING-1

 !## organise groups
 CALL IPEST_GLM_SETGROUPS()
 
 !## allocate memory for simulations
 CALL IPEST_GLM_ALLOCATE(DIR)

 !## set initial values for alpha()
 DO I=1,SIZE(PEST%PARAM); IF(.NOT.IPEST_GLM_CHK(I,IBATCH))RETURN; ENDDO

 !## open output files
 CALL UTL_CREATEDIR(TRIM(DIR)//'\IPEST')
 IUPESTOUT=UTL_GETUNIT();         OPEN(IUPESTOUT,        FILE=TRIM(DIR)//'\IPEST\LOG_PEST.TXT'            ,STATUS='UNKNOWN',ACTION='WRITE')
 IUPESTPROGRESS=UTL_GETUNIT();    OPEN(IUPESTPROGRESS,   FILE=TRIM(DIR)//'\IPEST\LOG_PEST_PROGRESS.TXT'   ,STATUS='UNKNOWN',ACTION='WRITE')
 IUPESTEFFICIENCY=UTL_GETUNIT();  OPEN(IUPESTEFFICIENCY, FILE=TRIM(DIR)//'\IPEST\LOG_PEST_EFFICIENCY.TXT' ,STATUS='UNKNOWN',ACTION='WRITE')
 IUPESTSENSITIVITY=UTL_GETUNIT(); OPEN(IUPESTSENSITIVITY,FILE=TRIM(DIR)//'\IPEST\LOG_PEST_SENSITIVITY.TXT',STATUS='UNKNOWN',ACTION='WRITE')
 IUPESTRUNFILE=UTL_GETUNIT();     OPEN(IUPESTRUNFILE,    FILE=TRIM(DIR)//'\IPEST\LOG_PEST_RUNFILE.TXT'    ,STATUS='UNKNOWN',ACTION='WRITE')
  
 WRITE(IUPESTOUT,'(A)') 'Parameters'
 WRITE(IUPESTOUT,'(A2,1X,A5,2(1X,A5),5(1X,A15),3A10,3A15)') 'AC','PTYPE','ILS','IZN','INITIAL','DELTA','MINIMUM','MAXIMUM','FADJ','IGROUP','LTRANS','NODES','ACRONYM','PPRIOR','STDEV'
  
 DO I=1,SIZE(PEST%PARAM); CALL IPEST_GLM_ECHO_PARAMETERS(I); ENDDO;  WRITE(IUPESTOUT,*)
  
 WRITE(IUPESTEFFICIENCY,'(8(A15,1X))') 'TOTAL_J','MEAS._J','PARAM._J','RMSE_TJ','ADJUSTMENTS','CUR_IMPROVEMENT','TOT_IMPROVEMENT'
 WRITE(IUPESTEFFICIENCY,'(8(A15,1X))') '(L2)',   '(L2)',   '(L2)',    '(L)',    '(-)',        '(%)',            '(%)'
 WRITE(IUPESTSENSITIVITY,'(A)')   'Sensitivity (%):'

 N=0; DO J=1,SIZE(PEST%PARAM); IF(ABS(PEST%PARAM(J)%PACT).EQ.1.AND.PEST%PARAM(J)%PIGROUP.GT.0)N=N+1; ENDDO

 CALL IPEST_GLM_WRITEHEADER('Iteration      ',N,IUPESTSENSITIVITY)
 
 !## allocate memory
 IF(.NOT.IPEST_GLM_ALLOCATEMSR())RETURN
  
 CALL WMESSAGEENABLE(TIMEREXPIRED,1); MSR%PJ=HUGE(1.0D0)
 
 !## start optimization cycle
 ITER=0
MAINLOOP: DO 

  !## run and process all lambda-testing
  !## save initial residuals
  IF(ITER.EQ.0)THEN
   IF(IUPESTRESIDUAL.GT.0)CLOSE(IUPESTRESIDUAL); IUPESTRESIDUAL=UTL_GETUNIT()
   OPEN(IUPESTRESIDUAL,FILE=TRIM(DIR)//'\IPEST\LOG_PEST_RESIDUAL_0.TXT',STATUS='UNKNOWN',ACTION='WRITE')
  ELSE
   IUPESTRESIDUAL=0
  ENDIF 
  CALL IPEST_GLM_RUNMODELS(IBATCH,RNL,LPARAM,'L',DIR,MNAME,ITER,LAMBDA,0)
  
  !## initial lambda
  IF(ITER.GT.0)THEN
   !## determine best lambda and continue, otherwise determine another set of lambda's
   IF(.NOT.IPEST_GLM_NEXT(IBATCH,ITER,DIR,LAMBDA))THEN
    LAMBDA=LAMBDA*GAMMA
    !## determine new gradient
    I=IPEST_GLM_GRADIENT(IBATCH,ITER,LAMBDA,GAMMA)
    SELECT CASE (I)
     !## quit
     CASE (-1); EXIT MAINLOOP
     !## start next cycle
     CASE (0 ); EXIT
     !## repeat current cycle
     CASE (1 ); CYCLE
    END SELECT
   ENDIF
  ELSE
   !## set current dh on correct position
   DO I=1,MSR%NOBS; MSR%DHL(0,I)=MSR%DHL(1,I); ENDDO
   LAMBDA=MSR%TJ/DBLE(2.0D0*MSR%NOBS); LAMBDA=LOG10(LAMBDA); IX=FLOOR(LAMBDA); LAMBDA=10.0D0**IX
   !## write initial lambda
   WRITE(IUPESTOUT,'(/A/)') 'Initial Lambda_0 computed as '//TRIM(RTOS(LAMBDA,'G',3))
   MSR%TJ_H(ITER)=MSR%TJ; MSR%RJ_H(ITER)=MSR%RJ; MSR%GOF_H(ITER)=MSR%GOF(1); MSR%NSC_H(ITER)=MSR%NSC(1)
   !## save initial heads with these
   CALL IPEST_GLM_UPDATEHEADS(DIR,1)
  ENDIF
  
  IF(.NOT.IPEST_GLM_ECHOPARAMETERS(IBATCH,ITER))EXIT

  !## next cycle
  ITER=ITER+1
  
  !## copy current objective function value to previous objective function value
  MSR%PJ=MSR%TJ
  
  !## "melt" all parameters for next cycle
  DO  I=1,SIZE(PEST%PARAM); IF(PEST%PARAM(I)%PACT.EQ.-1)PEST%PARAM(I)%PACT=1; ENDDO

  !## run and process all lambda-testing
  IUPESTRESIDUAL=0; CALL IPEST_GLM_RUNMODELS(IBATCH,RNG,GPARAM,'P',DIR,MNAME,ITER,LAMBDA,0)
  
  !## determine new gradient
  IF(.NOT.IPEST_GLM_GRADIENT(IBATCH,ITER,LAMBDA,GAMMA))EXIT

 ENDDO MAINLOOP
 CALL WMESSAGETIMER(0); CALL WMESSAGEENABLE(TIMEREXPIRED,0)
 
 END SUBROUTINE IPEST_GLM_MAIN
 
 !#####=================================================================
 SUBROUTINE IPEST_GLM_RUNMODELS(IBATCH,RN,RPARAM,RT,DIR,MNAME,ITER,LAMBDA,ILAMBDA)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN),DIMENSION(:) :: RN
 REAL(KIND=DP_KIND),INTENT(IN) :: LAMBDA
 INTEGER,INTENT(IN),DIMENSION(:) :: RPARAM
 INTEGER,INTENT(IN) :: ITER,IBATCH,ILAMBDA
 CHARACTER(LEN=1),INTENT(IN) :: RT
 CHARACTER(LEN=*),INTENT(IN) :: DIR,MNAME
 INTEGER :: IGRAD,NDONE,NCPU,IFLAGS,I
 INTEGER,DIMENSION(2) :: IDPROC
 LOGICAL :: LRESTART
 
 LRESTART=.FALSE.
   
 IF(RT.EQ.'L')THEN
  WRITE(IUPESTPROGRESS,'(/A5,4A15,15X,3A15)') 'LS','PARAMETER','LAMBDA','TOT_J','RED_J','GOODNESS FIT','NASH SUTCLIFFE','SIM_TIME(SEC)' 
  WRITE(IUPESTOUT,'(/A/)') ' *** Lambda Cycle ***'; WRITE(*,'(/A/)') ' *** Lambda Cycle ***'
 ELSEIF(RT.EQ.'P')THEN
  WRITE(IUPESTPROGRESS,'(/A5,5A15,30X,A15)') 'GD','PARAMETER','FACTOR','TOT_J','RED_J','CUR_PARAMETER','SIM_TIME(SEC)' 
  WRITE(IUPESTOUT,'(/A/)') ' *** Sensitivity Cycle ***'; WRITE(*,'(/A/)') ' *** Sensitivity Cycle ***'
 ELSEIF(RT.EQ.'R')THEN
  WRITE(IUPESTPROGRESS,'(/A5,4A15,15X,3A15)') 'RS','REALIZATION','LAMBDA','TOT_J','RED_J','GOODNESS FIT','NASH SUTCLIFFE','SIM_TIME(SEC)' 
  WRITE(IUPESTOUT,'(/A/)') ' *** Realization Lambda Cycle '//TRIM(ITOS(ILAMBDA))//' ***'
  WRITE(*,        '(/A/)') ' *** Realization Lambda Cycle '//TRIM(ITOS(ILAMBDA))//' ***'
 ENDIF
 
 CALL WMESSAGETIMER(NCSECS,IREPEAT=1); IGRAD=0; NCPU=0; IPROC=0; ISTATUS=-1
 !## executes lamdba-testing on commandtool such that commands alike 'dir' etc. works
 IFLAGS=0; IF(PBMAN%CMDHIDE.EQ.1)IFLAGS=IFLAGS+PROCSILENT+PROCCMDPROC

 !## all done initially except the first
 IF(RT.EQ.'L'.AND.ITER.EQ.0)THEN; ISTATUS=1; ISTATUS(1)=-1; ENDIF
 
 !## start the lambda-analyses
 DO 

 !## start processes
  DO
  
   !## find gradient simulation to be (re)carried out
   DO IGRAD=1,SIZE(RN); IF(ISTATUS(IGRAD).EQ.-1)EXIT; ENDDO; IF(IGRAD.GT.SIZE(RN))EXIT
    
   !## number of cpu full
   NCPU=NCPU+1

   SELECT CASE (RT)
    CASE ('L','P')
     !## adjust alpha for current igrad
     CALL IPEST_GLM_NEXTGRAD(RPARAM(IGRAD),RT,IGRAD)
     IF(PBMAN%IFORMAT.EQ.3)THEN
      !## update files for this simulation
      CALL IPEST_GLM_SAVE_PARAMETERS(DIR,ITER,IGRAD,RT,MNAME) 
     ELSE
      !## define update in pst file
      IF(.NOT.IPEST_GLM_PST(DIR,MNAME,IGRAD,RPARAM(IGRAD),RT))THEN
       CALL IPEST_GLM_ERROR(IBATCH,'ERROR CREATED PST1 FILE FOR '//RT//'#'//TRIM(ITOS(RPARAM(IGRAD)))); RETURN
      ENDIF
     ENDIF
   END SELECT
   
   !## wait before starting a new process
   IF(PBMAN%NSWAIT.GT.0)CALL IOSWAIT(PBMAN%NSWAIT)
   !## clear error
   I=WINFOERROR(1); IDPROC=0
   IF(LRESTART)THEN
    IPROC(:,IGRAD)=1
   ELSE
    CALL IOSCOMMAND(TRIM(RN(IGRAD)),IFLAGS=IFLAGS,IDPROC=IDPROC); IPROC(:,IGRAD)=IDPROC
    IF(WINFOERROR(1).EQ.ERROSCOMMAND)THEN
     CALL IPEST_GLM_ERROR(IBATCH,'FAILED TO START MODEL '//RT//'#'//TRIM(ITOS(RPARAM(IGRAD)))); RETURN
    ENDIF
   ENDIF

   !## save start time
   CALL SYSTEM_CLOCK (STIME(IGRAD))
   !## started
   ISTATUS(IGRAD)=1

   !## all started
   DO IGRAD=1,SIZE(RN); IF(ISTATUS(IGRAD).EQ.-1)EXIT; ENDDO; IF(IGRAD.GT.SIZE(RN))EXIT
   !## maximum number of cpu reached
   IF(NCPU.GE.PBMAN%NCPU)EXIT

  ENDDO
   
  !## evaluate processes that are finished
  NDONE=IPEST_GLM_EVALUATE(IBATCH,ITER,NCPU,DIR,RN,RT,RPARAM,LAMBDA,LRESTART,ILAMBDA)
  !## finished if all succesfully completed
  IF(NDONE.EQ.SIZE(RN))EXIT
  !## first iteration only need a single run
  IF(RT.EQ.'L'.AND.ITER.EQ.0.AND.ISTATUS(1).EQ.0)EXIT
 ENDDO
  
 END SUBROUTINE IPEST_GLM_RUNMODELS
 
 !###====================================================================
 INTEGER FUNCTION IPEST_GLM_EVALUATE(IBATCH,ITER,NCPU,DIR,RN,RT,RPARAM,LAMBDA,LRESTART,ILAMBDA)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH,ITER,ILAMBDA
 INTEGER,INTENT(IN),DIMENSION(:) :: RPARAM
 INTEGER,INTENT(INOUT) :: NCPU
 REAL(KIND=DP_KIND),INTENT(IN) :: LAMBDA
 LOGICAL,INTENT(IN) :: LRESTART
 CHARACTER(LEN=*),INTENT(IN),DIMENSION(:) :: RN
 CHARACTER(LEN=*),INTENT(IN) :: DIR,RT
 INTEGER :: ITYPE,N,IGRAD,JGRAD,IEXCOD
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER,DIMENSION(2) :: IDPROC
 INTEGER :: ILIN,NDONE
 REAL(KIND=DP_KIND) :: F
 
 ILIN=0
 
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  !## timer expired
  IF(ITYPE.EQ.TIMEREXPIRED)THEN
   N=0; DO JGRAD=1,SIZE(RN) 
    !## all handled process
    IF(IPROC(1,JGRAD)+IPROC(2,JGRAD).EQ.0)CYCLE
    IF(LRESTART)THEN
     ISTATUS(JGRAD)=0; IEXCOD=0
    ELSE
     !## check running status
     IDPROC=IPROC(:,JGRAD)
     CALL IOSCOMMANDCHECK(IDPROC,ISTATUS(JGRAD),IEXCOD=IEXCOD)
    ENDIF
    !## stopped running
    IF(ISTATUS(JGRAD).EQ.0)THEN
     !## error occured 
     IF(IEXCOD.NE.0)THEN
      CALL IPEST_GLM_ERROR(IBATCH,'ERROR OCCURED RUNNING MODEL '//RT//'#'//TRIM(ITOS(RPARAM(JGRAD))))
      !## try again - need to run again
      ISTATUS(JGRAD)=-1
     ENDIF
     !## set part of objective function
     IF(ISTATUS(JGRAD).EQ.0)THEN
      IF(.NOT.IPEST_GLM_GETJ(DIR,JGRAD,RPARAM(JGRAD),RT,IBATCH,ILAMBDA))RETURN 
      IF(RT.EQ.'L'.OR.RT.EQ.'R')THEN
       MSR%DHL_J(JGRAD)=MSR%TJ
       MSR%GOF(JGRAD)=UTL_GOODNESS_OF_FIT(GF_H,GF_O,MSR%NOBS)
       MSR%NSC(JGRAD)=UTL_NASH_SUTCLIFFE(GF_H,GF_O,MSR%NOBS)
      ELSEIF(RT.EQ.'P')THEN
       MSR%DHG_J(JGRAD)=MSR%TJ
      ENDIF
      !## write echo
      CALL IPEST_GLM_PROGRESS(ITER,JGRAD,RPARAM(JGRAD),RT,LAMBDA); FLUSH(IUPESTPROGRESS)
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
   F=DBLE(NDONE)*100.0D0/DBLE(SIZE(RN))
   WRITE(6,'(A)') '+Still running '//TRIM(ITOS(N))//'; models completed: '//TRIM(RTOS(F,'F',2))//'% (total '// &
       TRIM(ITOS(NDONE))//' out of '//TRIM(ITOS(SIZE(RN)))//' simulations)'
   !## nothing running anymore
   IF(N.EQ.0)EXIT
   !## start another one as a proces has been stopped and there is still one waiting in the que
   IF(NCPU.LT.PBMAN%NCPU.AND.NDONE.LT.SIZE(RN))EXIT
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
 LOGICAL FUNCTION IPEST_GLM_CHK(IP,IBATCH)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IP,IBATCH
 INTEGER :: I,N
 REAL(KIND=DP_KIND) :: K1,K2
 
 IPEST_GLM_CHK=.FALSE.
 
 DO I=1,SIZE(PARAM)
  PEST%PARAM(IP)%PPARAM=UTL_CAP(PEST%PARAM(IP)%PPARAM,'U')
  IF(TRIM(PEST%PARAM(IP)%PPARAM).EQ.TRIM(PARAM(I)))EXIT
 ENDDO
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

 !## estimate variance of parameter
 IF(PEST%PARAM(IP)%PLOG.EQ.1)THEN
  K1=LOG10(ABS(PEST%PARAM(IP)%PMIN))
  K2=LOG10(ABS(PEST%PARAM(IP)%PMAX))
 ELSE
  K1=PEST%PARAM(IP)%PMIN
  K2=PEST%PARAM(IP)%PMAX
 ENDIF
 PEST%PARAM(IP)%PARSTD=(K2-K1)/4.0D0
 
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
 IF(PEST%PARAM(IP)%PLOG.EQ.1)THEN
  IF(PEST%PARAM(IP)%PDELTA.EQ.1.0)WRITE(*,'(/A/)') 'You can not specify delta alpha eq 1.0'
  IF(PEST%PARAM(IP)%PMIN  .EQ.0.0)WRITE(*,'(/A/)') 'You can not specify minimal value eq 0.0 for log-transformed parameters'
  PEST%PARAM(IP)%PINI  =LOG10(PEST%PARAM(IP)%PINI)
  PEST%PARAM(IP)%PMIN  =LOG10(PEST%PARAM(IP)%PMIN)
  PEST%PARAM(IP)%PMAX  =LOG10(PEST%PARAM(IP)%PMAX)
  PEST%PARAM(IP)%PDELTA=LOG10(PEST%PARAM(IP)%PDELTA)
  PEST%PARAM(IP)%PPRIOR=LOG10(PEST%PARAM(IP)%PPRIOR)
 ENDIF
 PEST%PARAM(IP)%ALPHA(1)=PEST%PARAM(IP)%PINI !## current  alpha
 PEST%PARAM(IP)%ALPHA(2)=PEST%PARAM(IP)%PINI !## previous alpha
 ALLOCATE(PEST%PARAM(IP)%ALPHA_HISTORY(0:PEST%PE_MXITER)); PEST%PARAM(IP)%ALPHA_HISTORY=0.0D0
 N=SIZE(RNG); ALLOCATE(PEST%PARAM(IP)%GALPHA(N)); PEST%PARAM(IP)%GALPHA=0.0D0
 !## set initial lalpha to initial value
 N=SIZE(RNL); ALLOCATE(PEST%PARAM(IP)%LALPHA(N)); PEST%PARAM(IP)%LALPHA=PEST%PARAM(IP)%ALPHA(1) 
 
 !## fill in default acronym
 IF(PEST%PARAM(IP)%ACRONYM.EQ.'')THEN
  WRITE(PEST%PARAM(IP)%ACRONYM,'(A2,2I5.5,I3.3)') PEST%PARAM(IP)%PPARAM,PEST%PARAM(IP)%PILS,PEST%PARAM(IP)%PIZONE,ABS(PEST%PARAM(IP)%PIGROUP)
 ENDIF

 PEST%PARAM(IP)%ACRONYM=ADJUSTR(PEST%PARAM(IP)%ACRONYM)
  
 IPEST_GLM_CHK=.TRUE.

 END FUNCTION IPEST_GLM_CHK
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_READ_ZONES(DIR)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: JU,I,J
 
 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(DIR)//'\PARAM_DUMP_IPEST.DAT',STATUS='OLD',ACTION='READ',FORM='FORMATTED')
 DO I=1,SIZE(PEST%PARAM)
  READ(JU,*) PEST%PARAM(I)%NODES,PEST%PARAM(I)%ZTYPE
  IF(PEST%PARAM(I)%ZTYPE.EQ.0)THEN
   ALLOCATE(PEST%PARAM(I)%IROW(PEST%PARAM(I)%NODES), &
            PEST%PARAM(I)%ICOL(PEST%PARAM(I)%NODES), &
            PEST%PARAM(I)%F(   PEST%PARAM(I)%NODES))
   DO J=1,PEST%PARAM(I)%NODES
    READ(JU,*) PEST%PARAM(I)%IROW(J),PEST%PARAM(I)%ICOL(J),PEST%PARAM(I)%F(J)
   ENDDO
  ELSE
   ALLOCATE(PEST%PARAM(I)%XY(PEST%PARAM(I)%NODES,2))
   DO J=1,PEST%PARAM(I)%NODES
    READ(JU,*) PEST%PARAM(I)%XY(J,1),PEST%PARAM(I)%XY(J,2)
   ENDDO
  ENDIF
  IF(PEST%PARAM(I)%PPARAM.EQ.'HF')THEN
   PEST%PARAM(I)%NODES=0 !## one single cell used as zone for horizontal barrier module
  ELSE
   IF(PEST%PARAM(I)%NODES.EQ.0)PEST%PARAM(I)%PACT=0
  ENDIF
 
 ENDDO
 CLOSE(JU)  
  
 END SUBROUTINE IPEST_GLM_READ_ZONES
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_RESET_PARAMETER()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I

 !## scaling
 DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PLOG.EQ.1)THEN
   PEST%PARAM(I)%PINI  =10.0D0**(PEST%PARAM(I)%PINI)
   PEST%PARAM(I)%PMIN  =10.0D0**(PEST%PARAM(I)%PMIN)
   PEST%PARAM(I)%PMAX  =10.0D0**(PEST%PARAM(I)%PMAX)
   PEST%PARAM(I)%PDELTA=10.0D0**(PEST%PARAM(I)%PDELTA)
   PEST%PARAM(I)%PPRIOR=10.0D0**(PEST%PARAM(I)%PPRIOR)
  ENDIF
 ENDDO
 
 IF(IUPESTOUT.GT.0)        CLOSE(IUPESTOUT);         IUPESTOUT=0
 IF(IUPESTPROGRESS.GT.0)   CLOSE(IUPESTPROGRESS);    IUPESTPROGRESS=0
 IF(IUPESTEFFICIENCY.GT.0) CLOSE(IUPESTEFFICIENCY);  IUPESTEFFICIENCY=0
 IF(IUPESTSENSITIVITY.GT.0)CLOSE(IUPESTSENSITIVITY); IUPESTSENSITIVITY=0
 IF(IUPESTRUNFILE.GT.0)    CLOSE(IUPESTRUNFILE);     IUPESTRUNFILE=0
 IF(IUPESTRESIDUAL.GT.0)   CLOSE(IUPESTRESIDUAL);    IUPESTRESIDUAL=0

 END SUBROUTINE IPEST_GLM_RESET_PARAMETER
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_ECHO_PARAMETERS(IP)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IP
 INTEGER :: I
 
 I=PEST%PARAM(IP)%PACT; IF(PEST%PARAM(IP)%PACT.EQ.2)I=1
 IF(PEST%PARAM(IP)%PLOG.EQ.0)THEN
  WRITE(IUPESTOUT,'(I2,1X,A5,2(1X,I5),5(1X,F15.7),I10,I10,A10,A15,2F15.7)') I,PEST%PARAM(IP)%PPARAM,PEST%PARAM(IP)%PILS, &
     PEST%PARAM(IP)%PIZONE,PEST%PARAM(IP)%PINI,PEST%PARAM(IP)%PDELTA,PEST%PARAM(IP)%PMIN,PEST%PARAM(IP)%PMAX,PEST%PARAM(IP)%PINCREASE, &
     ABS(PEST%PARAM(IP)%PIGROUP),PEST%PARAM(IP)%PLOG,'Unknown',PEST%PARAM(IP)%ACRONYM,PEST%PARAM(IP)%PPRIOR,PEST%PARAM(IP)%PARSTD
  PEST%PARAM(IP)%ALPHA_HISTORY(0)=PEST%PARAM(IP)%PINI
 ELSE
  WRITE(IUPESTOUT,'(I2,1X,A5,2(1X,I5),5(1X,F15.7),I10,I10,A10,A15,2F15.7)') I,PEST%PARAM(IP)%PPARAM,PEST%PARAM(IP)%PILS, &
     PEST%PARAM(IP)%PIZONE,10.0D0**(PEST%PARAM(IP)%PINI),10.0D0**PEST%PARAM(IP)%PDELTA,10.0D0**(PEST%PARAM(IP)%PMIN),10.0D0**(PEST%PARAM(IP)%PMAX),PEST%PARAM(IP)%PINCREASE, &
     ABS(PEST%PARAM(IP)%PIGROUP),PEST%PARAM(IP)%PLOG,'Unknown',PEST%PARAM(IP)%ACRONYM,10.0D0**(PEST%PARAM(IP)%PPRIOR),PEST%PARAM(IP)%PARSTD
  PEST%PARAM(IP)%ALPHA_HISTORY(0)=10.0D0**(PEST%PARAM(IP)%PINI)
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
        TRIM(RTOS(10.0D0**(PEST%PARAM(I)%ALPHA(1)),'G',7))//','// &
        TRIM(RTOS(10.0D0**PEST%PARAM(I)%PDELTA,'G',7))//','// &
        TRIM(RTOS(10.0D0**(PEST%PARAM(I)%PMIN)/10.0D0,'G',7))  //','// &
        TRIM(RTOS(10.0D0**(PEST%PARAM(I)%PMAX)*10.0D0,'G',7))  //','// &
        TRIM(RTOS(PEST%PARAM(I)%PINCREASE,'G',7))  //','// &
        TRIM(ITOS(ABS(PEST%PARAM(I)%PIGROUP)))     //','// &
        TRIM(ITOS(PEST%PARAM(I)%PLOG))//','// &
        TRIM(PEST%PARAM(I)%ACRONYM)//','// &
        TRIM(RTOS(10.0D0**PEST%PARAM(I)%PPRIOR,'G',7))    
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
 SUBROUTINE IPEST_GLM_ALLOCATE(DIR)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: I,N,M
 
 CALL IPEST_GLM_DEALLOCATE()
 
 !## allocate memory for running the models
 N=0 ; DO I=1,SIZE(PEST%PARAM)
  !## associated parameters to existing groups inactive for gradient computation
  IF(PEST%PARAM(I)%PIGROUP.LT.0)PEST%PARAM(I)%PACT=2; IF(PEST%PARAM(I)%PACT.EQ.1)N=N+1
 ENDDO

 M=PBMAN%NLINESEARCH
 ALLOCATE(RNG(N),RNL(M),GPARAM(N),LPARAM(M))

 !## allocate memory for process-status memory
 M=MAX(M,N); ALLOCATE(ISTATUS(M),IPROC(2,M),STIME(M),ETIME(M))
 
 !## set linesearch-runbatch-files
 DO I=1,PBMAN%NLINESEARCH; RNL(I)=TRIM(DIR)//'\RUN_L#'//TRIM(ITOS(I))//'.BAT'; LPARAM(I)=I; ENDDO

 !## set gradient-runbatch-files - equal to number of parameter to be estimated
 N=0; DO I=1,SIZE(PEST%PARAM)
  !## parameter
  IF(PEST%PARAM(I)%PACT.EQ.0.OR.PEST%PARAM(I)%PIGROUP.LT.0)CYCLE; N=N+1; RNG(N)=TRIM(DIR)//'\RUN_P#'//TRIM(ITOS(I))//'.BAT'; GPARAM(N)=I
 ENDDO

 END SUBROUTINE IPEST_GLM_ALLOCATE

 !#####=================================================================
 SUBROUTINE IPEST_GLM_DEALLOCATE()
 !#####=================================================================
 IMPLICIT NONE

 IF(ALLOCATED(RNG))    DEALLOCATE(RNG)
 IF(ALLOCATED(RNL))    DEALLOCATE(RNL)
 IF(ALLOCATED(IPROC))  DEALLOCATE(IPROC)
 IF(ALLOCATED(GPARAM)) DEALLOCATE(GPARAM)
 IF(ALLOCATED(LPARAM)) DEALLOCATE(LPARAM)
 IF(ALLOCATED(ISTATUS))DEALLOCATE(ISTATUS)
 IF(ALLOCATED(STIME))  DEALLOCATE(STIME)
 IF(ALLOCATED(ETIME))  DEALLOCATE(ETIME)
 
 END SUBROUTINE IPEST_GLM_DEALLOCATE
 
 !#####=================================================================
 LOGICAL FUNCTION IPEST_GLM_NEXT(IBATCH,ITER,DIR,LAMBDA)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER,IBATCH
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 REAL(KIND=DP_KIND),INTENT(INOUT) :: LAMBDA
 REAL(KIND=DP_KIND) :: MJ
 INTEGER :: I,IJ
 
 IPEST_GLM_NEXT=.FALSE.
 
 !## get optimal model with lowest objective function value
 IJ=0; MJ=HUGE(1.0D0); DO I=1,PBMAN%NLINESEARCH
  IF(MSR%DHL_J(I).LT.MJ)THEN
   MJ=MSR%DHL_J(I)
   IJ=I
  ENDIF
 ENDDO

 !## is there no reduction achieved - return and try another set of lambdas
 IF(MJ.GE.MSR%PJ)THEN
  RETURN
 ENDIF

 !## update lambda
 LAMBDA=LAMBDA*PBMAN%LAMBDA_TEST(IJ)
 WRITE(IUPESTPROGRESS,'(/A)') 'New Lambda_0 '//TRIM(RTOS(LAMBDA,'F',7))//'; Objective Function Value '//TRIM(RTOS(MJ,'F',7))
 
 !## update new objective function value to be minimized
 MSR%TJ=MJ
 
 !## save residuals
 IF(IUPESTRESIDUAL.GT.0)CLOSE(IUPESTRESIDUAL)
 IUPESTRESIDUAL=UTL_GETUNIT(); OPEN(IUPESTRESIDUAL,FILE=TRIM(DIR)//'\IPEST\LOG_PEST_RESIDUAL_'// &
   TRIM(ITOS(ITER))//'.TXT',STATUS='UNKNOWN',ACTION='WRITE')
 IF(.NOT.IPEST_GLM_GETJ(DIR,IJ,LPARAM(IJ),'L',IBATCH,0))RETURN
 CLOSE(IUPESTRESIDUAL)
 
 !## set current dh on correct position
 DO I=1,MSR%NOBS; MSR%DHL(0,I)=MSR%DHL(IJ,I); ENDDO
 MSR%GOF(1)=MSR%GOF(IJ); MSR%NSC(1)=MSR%NSC(IJ)

 MSR%GOF_H(ITER)=MSR%GOF(1); MSR%NSC_H(ITER)=MSR%NSC(1)
 MSR%TJ_H(ITER)=MSR%TJ;      MSR%RJ_H(ITER)=MSR%RJ
  
 !## set correct set of parameters
 DO I=1,SIZE(PEST%PARAM)
  PEST%PARAM(I)%ALPHA(1)=PEST%PARAM(I)%LALPHA(IJ)
 ENDDO

 CALL IPEST_GLM_UPDATEHEADS(DIR,IJ)
 
 IPEST_GLM_NEXT=.TRUE.
 
 END FUNCTION IPEST_GLM_NEXT

 !#####=================================================================
 SUBROUTINE IPEST_GLM_UPDATEHEADS(DIR,IJ)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER,INTENT(IN) :: IJ
 INTEGER :: ILAY
 TYPE(IDFOBJ) :: SHD
 
 !## copy results from "winner" as starting heads
 ALLOCATE(BND(1)); ALLOCATE(BND(1)%X(1,1))
 DO ILAY=1,PRJNLAY
  IF(IDFREAD(SHD,TRIM(DIR)//'\IPEST_L#'//TRIM(ITOS(IJ))//'\HEAD\HEAD_STEADY-STATE_L'//TRIM(ITOS(ILAY))//'.IDF',1))THEN
   IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\MODELINPUT\BAS6\STRT_L'//TRIM(ITOS(ILAY))//'.ARR', &
      SHD,0,0,1,-1))THEN
   ENDIF
  ENDIF
  CALL IDFDEALLOCATEX(SHD)
 ENDDO
 DEALLOCATE(BND(1)%X); DEALLOCATE(BND)
 
 END SUBROUTINE IPEST_GLM_UPDATEHEADS
 
 !#####=================================================================
 SUBROUTINE IPEST_GLM_NEXTGRAD(IPARAM,RTYPE,ISIM)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPARAM,ISIM
 CHARACTER(LEN=1),INTENT(IN) :: RTYPE
 INTEGER :: I
 REAL(KIND=DP_KIND) :: FCT
 
 IF(RTYPE.EQ.'L')THEN
 
  DO I=1,SIZE(PEST%PARAM)
   PEST%PARAM(I)%ALPHA(1)=PEST%PARAM(I)%LALPHA(ISIM)
  ENDDO
  
 ELSEIF(RTYPE.EQ.'P')THEN
  !## reset all alpha's
  PEST%PARAM%ALPHA(1)=PEST%PARAM%ALPHA(2)

  !## adjust all parameters within the same group
  DO I=1,SIZE(PEST%PARAM)
   !## skip inactive parameters
   IF(PEST%PARAM(I)%PACT.EQ.0)CYCLE
   IF(ABS(PEST%PARAM(I)%PIGROUP).EQ.ABS(PEST%PARAM(IPARAM)%PIGROUP))THEN
    IF(PEST%PARAM(I)%PLOG.EQ.1)THEN
     PEST%PARAM(I)%ALPHA(1)=PEST%PARAM(I)%ALPHA(2)+PEST%PARAM(I)%PDELTA
     FCT=10.0D0**(PEST%PARAM(I)%ALPHA(1))
    ELSE
     PEST%PARAM(I)%ALPHA(1)=PEST%PARAM(I)%ALPHA(2)*PEST%PARAM(I)%PDELTA
     FCT=PEST%PARAM(I)%ALPHA(1)
    ENDIF
    IF(PEST%PARAM(I)%PIGROUP.GT.0)THEN
     WRITE(IUPESTOUT,'(A)') 'Adjusting Parameter '//TRIM(PEST%PARAM(I)%PPARAM)// &
                  '['//TRIM(PEST%PARAM(I)%ACRONYM)//']'// &
                  ';ils='//TRIM(ITOS(PEST%PARAM(I)%PILS))// &
                  ';izone='//TRIM(ITOS(PEST%PARAM(I)%PIZONE))// &
                  ';igroup='//TRIM(ITOS(PEST%PARAM(I)%PIGROUP))// &
                  ';factor='//TRIM(RTOS(FCT,'*',1))
    ENDIF
   ENDIF
  ENDDO
 ENDIF
 
 END SUBROUTINE IPEST_GLM_NEXTGRAD
 
 !#####=================================================================
 SUBROUTINE IPEST_GLM_SAVE_PARAMETERS(DIR,ITER,IGRAD,RT,MNAME) 
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER
 INTEGER,INTENT(IN) :: IGRAD
 CHARACTER(LEN=*),INTENT(IN) :: DIR,MNAME,RT
 INTEGER :: I,J, ILAY,ISUB
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:,:) :: X 
 CHARACTER(LEN=2),DIMENSION(1) :: IPTYPE
 !,J,K,ILAY,SCL_UP,SCL_D,IU,IOS
 CHARACTER(LEN=256) :: FNAME
! CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: LINE
! 
 IPTYPE=['KH']
 
! WRITE(IUPESTOUT,'(/A/)') 'Compute Realizations'
!
 !## modify parameters per submodel
 DO ISUB=1,PBMAN%NSUBMODEL
 
  !## read the zones per submodel
  CALL IPEST_GLM_READ_ZONES(TRIM(DIR)//'\GWF_'//TRIM(ITOS(ISUB))//'\MODELINPUT')
 
  DO I=1,SIZE(IPTYPE)
   SELECT CASE (IPTYPE(I))
    CASE ('KH')
     !## read all permeability-values
     DO ILAY=1,PRJNLAY
      FNAME=TRIM(DIR)//'\GWF_'//TRIM(ITOS(ISUB))//'\MODELINPUT\NPF\K_L'//TRIM(ITOS(ILAY))//'.ARR'
      IF(.NOT.IPEST_GLM_READ_ARRFILE(FNAME,X))RETURN
     ENDDO
   END SELECT
  ENDDO
  
  !## process parameters
  DO I=1,SIZE(PEST%PARAM)
! 
!  DO J=1,SIZE(PEST%PARAM)
!   DO K=1,SIZE(PEST%PARAM(J)%ILS)
!    ILAY=PEST%PARAM(J)%ILS(K)
!    IF(ILAY.LE.0.OR.ILAY.GT.PRJNLAY)CYCLE
!    
!    CALL IPEST_IES_GETENSEMBLENAME(ITER,DIR,PEST%PARAM(J)%REALSFNAME(I),FNAME,ILAMBDA,'Read')
!    
!    SELECT CASE (PEST%PARAM(J)%PPARAM)
!     CASE ('KH')
! 
!      !## it is not allowed to scale here as the covariance matrix does not scale with it
!      SCL_UP=10; SCL_D=0      
!      
!      !## read/clip/scale idf file
!      IF(.NOT.IDFREADSCALE(FNAME,PRJIDF,SCL_UP,SCL_D,1.0D0,0))RETURN
!      !## save array, do not correct for boundary condition as we not yet know for what layer the zone will apply
!      IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\MODELINPUT\LPF7\HK_L'//TRIM(ITOS(ILAY))//'_R#'//TRIM(ITOS(I))//'.ARR',PRJIDF,0,0,1,0))RETURN
!
!    END SELECT
!   ENDDO
  ENDDO
  
  !## save 'set of parameters
  DO I=1,SIZE(IPTYPE)
   SELECT CASE (IPTYPE(I))
    CASE ('KH')
     !## read all permeability-values
     DO ILAY=1,PRJNLAY
      FNAME=TRIM(DIR)//'\GWF_'//TRIM(ITOS(ISUB))//'\MODELINPUT\NPF\K_L'//TRIM(ITOS(ILAY))//'_'//RT//'#'//TRIM(ITOS(IGRAD))//'.ARR'
      IF(.NOT.IPEST_GLM_WRITE_ARRFILE(FNAME,X))RETURN
     ENDDO
   END SELECT
  ENDDO
  
  !## clean memory
  DO I=1,SIZE(PEST%PARAM)
   IF(ASSOCIATED(PEST%PARAM(I)%IROW))DEALLOCATE(PEST%PARAM(I)%IROW)
   IF(ASSOCIATED(PEST%PARAM(I)%ICOL))DEALLOCATE(PEST%PARAM(I)%ICOL)
   IF(ASSOCIATED(PEST%PARAM(I)%F))   DEALLOCATE(PEST%PARAM(I)%F)
   IF(ASSOCIATED(PEST%PARAM(I)%XY))  DEALLOCATE(PEST%PARAM(I)%XY)
  ENDDO
  
 ENDDO
 
 END SUBROUTINE IPEST_GLM_SAVE_PARAMETERS
 
 !###====================================================================
 LOGICAL FUNCTION IPEST_GLM_READ_ARRFILE(FNAME,X)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:,:) :: X
 INTEGER :: IU,NLINE,IOS,NCOL,NROW,IROW,ICOL
 CHARACTER(LEN=52) :: LINE
 
 IPEST_GLM_READ_ARRFILE=.FALSE.

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='FORMATTED',ACTION='READ',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open file: '//CHAR(13)// &
   '['//TRIM(FNAME)//']'//CHAR(13)//'for reading','Error')
  RETURN
 ENDIF

 !## look for network dimensions
 NLINE=0; DO
  READ(IU,'(A52)') LINE
  IF(INDEX(LINE,'DIMENSIONS').GT.0)EXIT
  NLINE=NLINE+1
 ENDDO
 
 READ(IU,*) NCOL
 READ(IU,*) NROW
 ALLOCATE(X(NCOL,NROW))
 REWIND(IU)
 DO IROW=1,NROW
  READ(IU,*) (X(ICOL,IROW),ICOL=1,NCOL)
 ENDDO
 CLOSE(IU)
 
 IPEST_GLM_READ_ARRFILE=.TRUE.
 
 END FUNCTION IPEST_GLM_READ_ARRFILE
 
 !###====================================================================
 LOGICAL FUNCTION IPEST_GLM_WRITE_ARRFILE(FNAME,X)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:,:) :: X
 INTEGER :: IU,NLINE,IOS,NCOL,NROW,IROW,ICOL
 CHARACTER(LEN=52) :: LINE
 REAL(KIND=DP_KIND) :: NODATA
 
 IPEST_GLM_WRITE_ARRFILE=.FALSE.

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open file: '//CHAR(13)// &
   '['//TRIM(FNAME)//']'//CHAR(13)//'for writing','Error')
  RETURN
 ENDIF

 NCOL=SIZE(X,1)
 NROW=SIZE(X,2)
! DO IROW=1,NROW
!  WRITE(IU,*) (X(ICOL,IROW),ICOL=1,NCOL)
! ENDDO
 
 NODATA=3.402823466385289E+038
 DO IROW=1,NROW
  CALL IDFWRITEFREE_ROW(IU,X(:,IROW),NCOL,NODATA,0,'*')  
 ENDDO
! CALL IDFWRITEFREE_HEADER(IU,IDF)
 
 !## add dummy values for the dimensions 
 WRITE(IU,'(A)') 'DIMENSIONS'
 WRITE(IU,'(A)') TRIM(ITOS(NCOL))
 WRITE(IU,'(A)') TRIM(ITOS(NROW))
 WRITE(IU,'(A)') '0.0'
 WRITE(IU,'(A)') '0.0'
 WRITE(IU,'(F15.3)') REAL(NCOL,8)
 WRITE(IU,'(F15.3)') REAL(NROW,8)
 WRITE(IU,'(A)') '3.402823466385289E+038'
 WRITE(IU,'(A)') '0'
 WRITE(IU,'(A)') '1.000000000000'
 WRITE(IU,'(A)') '1.000000000000'    

 CLOSE(IU)
 
 IPEST_GLM_WRITE_ARRFILE=.TRUE.
 
 END FUNCTION IPEST_GLM_WRITE_ARRFILE
 
 !###====================================================================
 INTEGER FUNCTION IPEST_GLM_GRADIENT(IBATCH,ITER,LAMBDA,GAMMA)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER,IBATCH
 REAL(KIND=DP_KIND),INTENT(INOUT) :: LAMBDA
 REAL(KIND=DP_KIND),INTENT(IN) :: GAMMA
 REAL(KIND=DP_KIND) :: DJ1,DJ2,DP,WW,TS,DF1,EIGWTHRESHOLD,W,DH1,DH2,MARQUARDT,P1,P2,PMIN,PMAX,WF
 INTEGER :: I,J,K,NP,IP1,NE,ISING,ILAMBDA,IBND,IPARAM
 INTEGER,ALLOCATABLE,DIMENSION(:) :: INDX 
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: JS,P,PT
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: N,RU 
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: S
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: EIGV,B,M 
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: EIGW,JQR
 LOGICAL :: LSVD

 IPEST_GLM_GRADIENT=0
 
 SELECT CASE (PEST%PE_SCALING)
  CASE (0,1); LSVD=.FALSE.
  CASE (2,3); LSVD=.TRUE.
 END SELECT

 !## sensitivity
 NP=SIZE(RNG); IF(.NOT.ALLOCATED(S)) ALLOCATE(S(NP)); S=0.0D0
 
 IPARAM=0
 DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PACT.NE.1)CYCLE
  DF1=PEST%PARAM(I)%PDELTA
  IPARAM=IPARAM+1
  DO J=1,MSR%NOBS
   W=MSR%W(J); DH1=MSR%DHG(IPARAM,J); DH2=MSR%DHL(0,J) 
   S(IPARAM)=S(IPARAM)+(W*((DH1-DH2)/DF1))
  ENDDO
 ENDDO
 DO I=1,NP; S(I)=S(I)/DBLE(MSR%NOBS); ENDDO

 TS=SUM(ABS(S)); IPARAM=0; DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PACT.NE.1)CYCLE; IPARAM=IPARAM+1; IF(TS.NE.0.0D0)S(IPARAM)=S(IPARAM)/TS
 ENDDO; S=ABS(S)*100.0D0

 WRITE(IUPESTSENSITIVITY,'(I10,99999F15.7)') ITER,(S(I),I=1,NP)

 !##===================
 !## write statistics of ALL parameters prior to the update
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

 !## "freeze"-insensitive parameters
 K=0; DO J=1,2
  IF(J.EQ.2)WRITE(IUPESTOUT,'(/A/)') 'List of Insensitive Parameter (Sensitivity <= '//TRIM(RTOS(PEST%PE_SENS,'F',7))//' %)'
  IPARAM=0; DO I=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(I)%PACT.NE.1)CYCLE; IPARAM=IPARAM+1
   IF(S(IPARAM).LE.PEST%PE_SENS)THEN
    PEST%PARAM(I)%PACT=-1; K=K+1
    IF(J.EQ.2)WRITE(IUPESTOUT,'(A15,F15.7,A2)') PEST%PARAM(I)%ACRONYM,S(IPARAM),' %'
   ENDIF
  ENDDO
  IF(K.EQ.0)EXIT
 ENDDO

 WRITE(IUPESTOUT,'(/70A1)') ('=',I=1,70); WRITE(IUPESTOUT,'(A)') 'Start Lambda Testing'; WRITE(IUPESTOUT,'(70A1/)') ('=',I=1,70)
 
 !## set boundaries for parameters
 DO I=1,SIZE(PEST%PARAM); CALL IPEST_GLM_GETBOUNDARY(I,IBND,P1,P2,PMIN,PMAX); PEST%PARAM(I)%IBND=IBND; ENDDO

 !## compute update vector for lambdas
 ILAMBDA=0
 DO 
  
  ILAMBDA=ILAMBDA+1
  !## finished
  IF(ILAMBDA.GT.PBMAN%NLINESEARCH)EXIT
  
  !## initiate number of parameters
  NP=0; DO I=1,SIZE(PEST%PARAM); IF(PEST%PARAM(I)%PACT.EQ.1)NP=NP+1; ENDDO
  IF(NP.EQ.0)THEN; CALL IPEST_GLM_ERROR(IBATCH,'ALL (REMAINING) PARAMETERS ARE INSENSITIVE, PROCESS STOPPED!'); IPEST_GLM_GRADIENT=-1; RETURN; ENDIF
  
  IF(ALLOCATED(JQJ))DEALLOCATE(JQJ);   ALLOCATE(JQJ (NP,NP))
  IF(ALLOCATED(JQR))DEALLOCATE(JQR);   ALLOCATE(JQR (NP))
  IF(ALLOCATED(U  ))DEALLOCATE(U);     ALLOCATE(U   (NP))
  IF(ALLOCATED(EIGW))DEALLOCATE(EIGW); ALLOCATE(EIGW(NP))
  IF(ALLOCATED(EIGV))DEALLOCATE(EIGV); ALLOCATE(EIGV(NP,NP))

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

  !## add weight to balance number of observations and parameters
  K=0; DO IP1=1,SIZE(PEST%PARAM)  !## row
   !## skip inactive; skip others parts of parameter
   IF(PEST%PARAM(IP1)%PACT.EQ.0)CYCLE; IF(PEST%PARAM(IP1)%PIGROUP.LE.0)CYCLE; K=K+1
  ENDDO
  WF=REAL(MSR%NOBS,8)/REAL(K,8)

  !## add parameter regularisation
  IF(PEST%PE_REGULARISATION.EQ.1)THEN
   I=0
   DO IP1=1,SIZE(PEST%PARAM)  !## row
    IF(PEST%PARAM(IP1)%PACT.NE.1)CYCLE
 
    I=I+1
    WW=1.0D0/PEST%PARAM(IP1)%PARSTD**2.0D0

    !## add balance weighting
    WW=WF*WW
    
    DP=(PEST%PARAM(IP1)%ALPHA(2)-PEST%PARAM(IP1)%PPRIOR)*WW
    JQR(I)=JQR(I)+DP
     
   ENDDO
  ENDIF
  
  WRITE(IUPESTOUT,'(A)') 'LAMBDA_0 PRIOR TO THE UPDATE VECTOR '//TRIM(RTOS(LAMBDA,'G',3))

  !## try to come with some suitable lambdas
  DO

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
    IF(ISING.EQ.1)THEN; CALL IPEST_GLM_ERROR(IBATCH,'Singular matrix after projection on eigenvectors which is rather odd, stopped.'); IPEST_GLM_GRADIENT=-1; RETURN; ENDIF
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
     IF(ISING.EQ.1)THEN; CALL IPEST_GLM_ERROR(IBATCH,'Singular matrix,try activating the SVD option to avoid this, stopped.'); IPEST_GLM_GRADIENT=-1; RETURN; ENDIF
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
   I=IPEST_GLM_UPGRADEVECTOR_LAMBDA(ILAMBDA)
   SELECT CASE (I)
    CASE (0)
     !## step inappropriate (too large) try another lambda   
     LAMBDA=LAMBDA*GAMMA
    CASE (1)
     EXIT
    CASE (2)
     !## try the same lambda without the excluded parameter due to a bondary hit
     ILAMBDA=ILAMBDA-1 !DEFAULT
     EXIT
   END SELECT
 
  ENDDO
 ENDDO !## lambda-test-loop

 WRITE(IUPESTOUT,'(A)') 'LAMBDA_0 POSTERIOR TO THE UPDATE VECTOR '//TRIM(RTOS(LAMBDA,'G',3))
 
 !## print parameters for lambda testing
 WRITE(IUPESTOUT,'(15X,99A15)') ('LAMBDA'//TRIM(ITOS(J)),J=1,PBMAN%NLINESEARCH)
 WRITE(IUPESTOUT,'(A15,99G15.7)') 'Parameters',(LAMBDA*PBMAN%LAMBDA_TEST(J),J=1,PBMAN%NLINESEARCH)
 WRITE(IUPESTOUT,'(999A1)') ('-',I=1,15*(PBMAN%NLINESEARCH+1))
 DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PACT.EQ.0.OR.PEST%PARAM(I)%PIGROUP.LT.0)CYCLE
  IF(PEST%PARAM(I)%PLOG.EQ.0)THEN
   WRITE(IUPESTOUT,'(A15,99F15.7)') PEST%PARAM(I)%ACRONYM,(PEST%PARAM(I)%LALPHA(J),J=1,PBMAN%NLINESEARCH)
  ELSE
   WRITE(IUPESTOUT,'(A15,99F15.7)') PEST%PARAM(I)%ACRONYM,(10.0D0**(PEST%PARAM(I)%LALPHA(J)),J=1,PBMAN%NLINESEARCH)
  ENDIF
 ENDDO

 WRITE(IUPESTOUT,'(/70A1)') ('=',I=1,70); WRITE(IUPESTOUT,'(A)') 'End Lambda Testing'; WRITE(IUPESTOUT,'(70A1/)') ('=',I=1,70)

 FLUSH(IUPESTOUT)
 
 IF(ALLOCATED(EIGW))DEALLOCATE(EIGW)
 IF(ALLOCATED(EIGV))DEALLOCATE(EIGV)
 IF(ALLOCATED(JQR ))DEALLOCATE(JQR )
 IF(ALLOCATED(S   ))DEALLOCATE(S   )
 IF(ALLOCATED(JS  ))DEALLOCATE(JS  )

 IPEST_GLM_GRADIENT=1

 END FUNCTION IPEST_GLM_GRADIENT
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_GETBOUNDARY(IP1,IBND,P1,P2,PMIN,PMAX)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IP1
 INTEGER,INTENT(OUT) :: IBND
 REAL(KIND=DP_KIND),INTENT(OUT) :: P1,P2,PMIN,PMAX
 
 !## parameter adjustment hit the parameter boundary
 P1  =PEST%PARAM(IP1)%ALPHA(1)
 P2  =PEST%PARAM(IP1)%ALPHA(2)
 PMIN=PEST%PARAM(IP1)%PMIN
 PMAX=PEST%PARAM(IP1)%PMAX
   
 IBND=0
 !## shoot over
 IF(P1.LE.PMIN)IBND=-1
 IF(P1.GE.PMAX)IBND= 1

! !## too close
! IF(ABS(P1-PMIN).LE.XPBND)IBND=-1; IF(ABS(PMAX-P1).LE.XPBND)IBND= 1
 
 END SUBROUTINE IPEST_GLM_GETBOUNDARY
 
 !###====================================================================
 INTEGER FUNCTION IPEST_GLM_UPGRADEVECTOR_LAMBDA(ILAMBDA)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ILAMBDA
 INTEGER :: I,IP1,IP2,IBND
 REAL(KIND=DP_KIND) :: F,PMAX,PMIN,P1,P2,AF,G
 
 !## exit code
 IPEST_GLM_UPGRADEVECTOR_LAMBDA=0

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

 AF=1.0D0
 
 !## check for size of adjustment
 DO IP1=1,SIZE(PEST%PARAM)
  !## inactive parameter
  IF(PEST%PARAM(IP1)%PACT.NE.1)CYCLE
  !## check size of adjustment
  IF(PEST%PARAM(IP1)%PLOG.EQ.1)THEN
   F=10.0D0**(PEST%PARAM(IP1)%ALPHA(1))/10.0D0**(PEST%PARAM(IP1)%ALPHA(2))
  ELSE
   F=PEST%PARAM(IP1)%ALPHA(1)/PEST%PARAM(IP1)%ALPHA(2)
  ENDIF 
  !## adjustment too large try another lambda
  IF(F.LT.1.0D0/PEST%PARAM(IP1)%PINCREASE.OR. &
     F.GT.      PEST%PARAM(IP1)%PINCREASE)RETURN

  CALL IPEST_GLM_GETBOUNDARY(IP1,IBND,P1,P2,PMIN,PMAX)

  !## parameter hits the boundary
  IF(IBND.NE.0)THEN
   !## hits the same boundary as before - skip it
   IF(IBND.EQ.PEST%PARAM(IP1)%IBND)THEN 
    !## ignore this parameter (group) for now - reuse current lambda and search another update vector ignoring this parameters again
    PEST%PARAM(IP1)%PACT=-1; IPEST_GLM_UPGRADEVECTOR_LAMBDA=2; RETURN
   ELSE
    !## reduce size of update vector to hit boundary
    IF(P1.LT.PMIN)F=(P2-PMIN)/(P2-P1)
    IF(P1.GT.PMAX)F=(PMAX-P2)/(P1-P2)
    !## keep track of minimal adjustment of vector
    AF=MIN(AF,F)
   ENDIF
  ENDIF  
 ENDDO
  
 !## corrects all gradients with this factor
 IF(AF.LT.1.0D0)THEN
  !## adjust all parameters
  DO IP2=1,SIZE(PEST%PARAM) 
   IF(PEST%PARAM(IP2)%PACT.NE.1)CYCLE
  
   G=PEST%PARAM(IP2)%ALPHA(1)-PEST%PARAM(IP2)%ALPHA(2)
   G=G*AF

   !## update parameters
   PEST%PARAM(IP2)%ALPHA(1)=PEST%PARAM(IP2)%ALPHA(2)+G
  ENDDO
 ENDIF
 
 !## copy gradients to all groups
 DO IP1=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(IP1)%PACT.NE.1)CYCLE
  DO IP2=1,SIZE(PEST%PARAM)
   IF(ABS(PEST%PARAM(IP2)%PIGROUP).EQ.PEST%PARAM(IP1)%PIGROUP)PEST%PARAM(IP2)%ALPHA(1)=PEST%PARAM(IP1)%ALPHA(1)
  ENDDO
 ENDDO
 
 !## copy alphas to correct vector with updates per lambda
 DO IP1=1,SIZE(PEST%PARAM)
  PEST%PARAM(IP1)%LALPHA(ILAMBDA)=PEST%PARAM(IP1)%ALPHA(1)
 ENDDO
 
 !## correct update gradient found
 IPEST_GLM_UPGRADEVECTOR_LAMBDA=1

 END FUNCTION IPEST_GLM_UPGRADEVECTOR_LAMBDA

 !###====================================================================
 LOGICAL FUNCTION IPEST_GLM_ECHOPARAMETERS(IBATCH,ITER)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER,IBATCH
 INTEGER :: IP1,N,I,ILOG
 REAL(KIND=DP_KIND) :: C1,C2,C3 !,F
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: GRADUPDATE
 
 IPEST_GLM_ECHOPARAMETERS=.FALSE.
 
 N=0; C1=0.0D0
 DO I=1,SIZE(PEST%PARAM)
  IF(ABS(PEST%PARAM(I)%PACT).EQ.1.AND.PEST%PARAM(I)%PIGROUP.GT.0)THEN
   N=N+1
   C1=C1+(PEST%PARAM(I)%ALPHA(2)-PEST%PARAM(I)%ALPHA(1))**2.0D0
  ENDIF
 ENDDO
 C1=SQRT(C1)

 C2=(1.0D0-MSR%TJ/MSR%PJ)*100.0D0
 C3=(1.0D0-MSR%TJ/MSR%TJ_H(0))*100.0D0
 IF(ITER.EQ.0)THEN
  WRITE(IUPESTEFFICIENCY,'(4(F15.3,1X))') MSR%TJ,MSR%TJ-MSR%RJ,MSR%RJ,REAL(SQRT(MSR%TJ))/REAL(MSR%NOBS,8)
 ELSE
  WRITE(IUPESTEFFICIENCY,'(8(F15.3,1X))') MSR%TJ,MSR%TJ-MSR%RJ,MSR%RJ,REAL(SQRT(MSR%TJ))/REAL(MSR%NOBS,8),C1,C2,C3
 ENDIF
 
 !## save alphas for history-logging
 DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PACT.EQ.0)CYCLE

  IF(PEST%PARAM(I)%PLOG.EQ.1)THEN
   PEST%PARAM(I)%ALPHA_HISTORY(ITER)=10.0D0**(PEST%PARAM(I)%ALPHA(1))
  ELSE
   PEST%PARAM(I)%ALPHA_HISTORY(ITER)=PEST%PARAM(I)%ALPHA(1)
  ENDIF
  
 ENDDO
 
 !## replace old by new parameter values
 PEST%PARAM%ALPHA(2)=PEST%PARAM%ALPHA(1)
 
 WRITE(IUPESTOUT,'(A/)') 'Optimization History:'
 WRITE(IUPESTOUT,'(A15,999(5X,A7,I3.3))') 'Statistics',('ITER',I,I=ITER,0,-1)
 WRITE(IUPESTOUT,'(999A1)') ('-',I=1,19+(ITER+1)*15)
  
 WRITE(IUPESTOUT,'(A15,99F15.3)') 'Total Obj. Val.',(MSR%TJ_H(I),I=ITER,0,-1)
 WRITE(IUPESTOUT,'(A15,99F15.3)') 'Meas. Obj. Val.',(MSR%TJ_H(I)-MSR%RJ_H(I),I=ITER,0,-1)
 WRITE(IUPESTOUT,'(A15,99F15.3)') 'Param Obj. Val.',(MSR%RJ_H(I),I=ITER,0,-1)
 WRITE(IUPESTOUT,'(A15,99I15)')   'Number of Obs. ',(MSR%NOBS,I=ITER,0,-1)
 WRITE(IUPESTOUT,'(A15,99F15.3)') 'Goodness of Fit',(MSR%GOF_H(I),I=ITER,0,-1)
 WRITE(IUPESTOUT,'(A15,99F15.3)') 'Nash Sutcliffe ',(MSR%NSC_H(I),I=ITER,0,-1)
 
 WRITE(IUPESTOUT,'(/A15,999(5X,A7,I3.3))') 'Parameter',('ITER',I,I=ITER,0,-1)
 WRITE(IUPESTOUT,'(999A1)') ('-',I=1,15+(ITER+1)*15)

 ALLOCATE(GRADUPDATE(ITER)); GRADUPDATE=0.0D0
 N=0
 DO IP1=1,SIZE(PEST%PARAM)

  WRITE(BLINE,'(99(F15.7))') (PEST%PARAM(IP1)%ALPHA_HISTORY(I),I=ITER,0,-1)
  
  IF(ABS(PEST%PARAM(IP1)%PACT).EQ.1.AND.PEST%PARAM(IP1)%PIGROUP.GT.0)THEN
   WRITE(IUPESTOUT,'(A15,A)') PEST%PARAM(IP1)%ACRONYM,TRIM(BLINE)
   N=N+1
   DO I=1,ITER
    GRADUPDATE(I)=GRADUPDATE(I)+(PEST%PARAM(IP1)%ALPHA_HISTORY(I)-PEST%PARAM(IP1)%ALPHA_HISTORY(I-1))**2.0D0
   ENDDO
  ENDIF

 ENDDO
 
 GRADUPDATE=SQRT(GRADUPDATE)
 WRITE(IUPESTOUT,'(999A1)') ('-',I=1,15+(ITER+1)*15)
 WRITE(IUPESTOUT,'(A15,99F15.7/)') 'Adjustment',(GRADUPDATE(I),I=ITER,1,-1)

 DEALLOCATE(GRADUPDATE)

 WRITE(IUPESTRUNFILE,'(/A,I10/)') 'Copy in the runfile, iteration ',ITER
 DO I=1,SIZE(PEST%PARAM)
  ILOG=PEST%PARAM(I)%PLOG
  IF(PEST%PARAM(I)%PLOG.EQ.1)THEN
   WRITE(IUPESTRUNFILE,'(I2,1X,A,1X,2(I5,1X),5(F10.3,1X),I5,1X,I2,1X,A,F10.3)') MIN(1,ABS(PEST%PARAM(I)%PACT)), &  !## iact
       PEST%PARAM(I)%PPARAM, &         !## ptype
       PEST%PARAM(I)%PILS, &           !## ilayer/system
       PEST%PARAM(I)%PIZONE, &         !## zone number
       10.0D0**(PEST%PARAM(I)%ALPHA(1)), &  !## initial value
       10.0D0**(PEST%PARAM(I)%PDELTA), &         !## finite difference step
       10.0D0**(PEST%PARAM(I)%PMIN), &      !## minimal value
       10.0D0**(PEST%PARAM(I)%PMAX),&       !## maximal value
       PEST%PARAM(I)%PINCREASE,&       !## maximal adjust factor
       ABS(PEST%PARAM(I)%PIGROUP),&    !## group number
       ILOG,&                          !## log transformed
       TRIM(PEST%PARAM(I)%ACRONYM), &
       10.0D0**(PEST%PARAM(I)%PPRIOR)
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

 FLUSH(IUPESTOUT); FLUSH(IUPESTPROGRESS); FLUSH(IUPESTEFFICIENCY); FLUSH(IUPESTSENSITIVITY); FLUSH(IUPESTRUNFILE)

 WRITE(*,'(/A/)') 'Current Obj.Func. '//TRIM(RTOS(MSR%TJ,'F',7))//'; current/total improvement '//TRIM(RTOS(C2,'F',7))//';'//TRIM(RTOS(C3,'F',7))//'%'

 !## continue ?
 IF(ITER+1.GT.PEST%PE_MXITER)THEN
  CALL IPEST_GLM_ERROR(IBATCH,'Pest iteration terminated: PEST_ITER (='//TRIM(ITOS(PEST%PE_MXITER))//') = PEST_NITER (='// &
    TRIM(ITOS(PEST%PE_MXITER))//')'); RETURN
 ENDIF
 IF(MSR%TJ.LE.0.0D0)THEN
  CALL IPEST_GLM_ERROR(IBATCH,'Objective Function <= 0.0 ('//TRIM(RTOS(MSR%TJ,'G',7))//')'); RETURN
 ENDIF
 IF(C2.LT.PEST%PE_STOP)THEN
  CALL IPEST_GLM_ERROR(IBATCH,'Pest iteration terminated decrease objective function ('//TRIM(RTOS(C2,'G',7))// &
       '%) > PEST_JSTOP ('//TRIM(RTOS(PEST%PE_STOP,'G',7))//'%)'); RETURN
 ENDIF

 !## next iteration
 WRITE(IUPESTOUT,'(/A/)') ' *** Next Optimization Cycle '//TRIM(ITOS(ITER+1))//' ***'
 WRITE(*,'(/A/)')         ' *** Next Optimization Cycle '//TRIM(ITOS(ITER+1))//' ***'

 IPEST_GLM_ECHOPARAMETERS=.TRUE.
 
 END FUNCTION IPEST_GLM_ECHOPARAMETERS
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_JQJ(IBATCH,MARQUARDT,JQJ,NP,LCOV) 
 !###====================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: PRINTLIMIT=50
 REAL(KIND=DP_KIND),INTENT(IN) :: MARQUARDT
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(NP,NP) :: JQJ
 INTEGER,INTENT(IN) :: NP,IBATCH
 LOGICAL,INTENT(IN) :: LCOV
 INTEGER :: I,J,IP1,IP2,II,JJ,N,IPARAM,JPARAM,ISING,IERROR
 REAL(KIND=DP_KIND) :: DF1,DF2,DJ1,DJ2,B1,CB,W,DH1,DH2,ZW,Z,Z1,Z2
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: B 
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: COR,COV
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: DIAG
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
 
 IF(.NOT.LCOV)THEN
  IF(PEST%PE_REGULARISATION.EQ.0)THEN
   !## levenberg
   DO I=1,NP
    JQJ(I,I)=JQJ(I,I)+MARQUARDT
   ENDDO
  ELSE
   !## levenberg-marquardt
   ALLOCATE(DIAG(NP,NP)); DIAG=0.0D0
   I=0
   DO IP1=1,SIZE(PEST%PARAM) 
    IF(PEST%PARAM(IP1)%PACT.NE.1)CYCLE
    I=I+1; DIAG(I,I)=1.0D0/PEST%PARAM(IP1)%PARSTD**2.0D0
   ENDDO
   !## levenberg-marquardt (use parameter prior covariance)
   DO I=1,NP
    DO J=1,NP
     JQJ(J,I)=JQJ(J,I)+MARQUARDT*DIAG(J,I)
    ENDDO
   ENDDO
   DEALLOCATE(DIAG)
  ENDIF
  RETURN
 ENDIF 

 IF(ALLOCATED(COR ))DEALLOCATE(COR);  ALLOCATE(COR(NP,NP))
 IF(ALLOCATED(COV ))DEALLOCATE(COV);  ALLOCATE(COV(NP,NP))
 IF(ALLOCATED(INDX))DEALLOCATE(INDX); ALLOCATE(INDX(NP))
 IF(ALLOCATED(B   ))DEALLOCATE(B);    ALLOCATE(B(NP,NP))

 !## compute inverse of (JQJ)-1 -> B = covariance matrix
 CALL IPEST_LUDECOMP_DBL(JQJ,INDX,NP,ISING)
 !## matrix not singular - compute inverse
 IF(ISING.EQ.0)THEN
  B=0.0D0; DO I=1,NP; B(I,I)=1.0D0; ENDDO
  DO I=1,NP; CALL IPEST_LUBACKSUB_DBL(JQJ,INDX,B(1,I),NP); ENDDO
 
  !## b1 is variance of each of model response
  N=MAX(1,MSR%NOBS-NP); B1=MSR%TJ/REAL(N,8)
  !## parameter covariance matrix scaled to this variance
  DO I=1,NP; DO J=1,NP; B(I,J)=B1*B(I,J); ENDDO; ENDDO

  IF(NP.LT.PRINTLIMIT)THEN
   WRITE(IUPESTOUT,'(/A/)') 'Parameter Covariance Matrix (m2):'
   CALL IPEST_GLM_WRITEHEADER('               ',NP,IUPESTOUT)
   WRITE(IUPESTOUT,'(9999A1)') ('-',I=1,15+NP*15)
  ENDIF

  I=0
  DO IP1=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(IP1)%PACT.EQ.1)THEN
    I=I+1
    IF(NP.LT.PRINTLIMIT)THEN
     WRITE(IUPESTOUT,'(A15,99999F15.7)') PEST%PARAM(IP1)%ACRONYM,(B(I,J),J=1,NP)
    ENDIF
    DO J=1,NP; COV(I,J)=B(I,J); ENDDO
   ENDIF
  ENDDO
 
  !## parameter correlation matrix
  IF(NP.LT.PRINTLIMIT)THEN
   WRITE(IUPESTOUT,'(/A/)') 'Parameter Correlation Matrix (-)'
   CALL IPEST_GLM_WRITEHEADER('               ',NP,IUPESTOUT)
   WRITE(IUPESTOUT,'(9999A1)') ('-',I=1,15+NP*15)
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
     WRITE(IUPESTOUT,'(A15,99999F15.7)') PEST%PARAM(IP1)%ACRONYM,(COR(I,J),J=1,NP)
    ENDIF
   ENDIF
  ENDDO

  !## write per parameter highly correlated other parameter
  DO JJ=1,2
   IF(JJ.EQ.2)WRITE(IUPESTOUT,'(/A/)') 'List of Parameter Highly Correlated (correlation > 95%)'
   IP1=0; N=0
   DO I=1,SIZE(PEST%PARAM)
    IF(PEST%PARAM(I)%PACT.EQ.1)THEN
     WRITE(BLINE,'(A15)') PEST%PARAM(I)%ACRONYM
     IP1=IP1+1
     IP2=0
     II=0
     DO J=1,SIZE(PEST%PARAM)
      IF(PEST%PARAM(J)%PACT.EQ.1)THEN
       IP2=IP2+1
       IF(I.NE.J)THEN
        WRITE(SLINE,'(A15)') PEST%PARAM(J)%ACRONYM
        IF(ABS(COR(IP1,IP2)).GE.0.95D0)THEN
         II=II+1
         BLINE=TRIM(BLINE)//','//TRIM(SLINE)//'('//TRIM(RTOS(COR(IP1,IP2)*100.0D0,'F',1))//'%)'
        ENDIF
       ENDIF
      ENDIF
     ENDDO
     IF(II.GT.0)THEN
      N=N+1; IF(JJ.EQ.2)WRITE(IUPESTOUT,'(A)') TRIM(BLINE)
     ENDIF
    ENDIF  
   ENDDO
   IF(N.EQ.0)EXIT
  ENDDO
  
  WRITE(IUPESTOUT,'(/A/)') 'Parameter Variance - Standard Parameter Error (Confidence Limits ~96%)'
 
  J=0; IERROR=0
  DO I=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(I)%PACT.EQ.1)THEN
    J=J+1
    IF(COV(J,J).GT.0.0D0)THEN
     !## stdev
     PEST%PARAM(I)%ALPHA_ERROR_STDEV=SQRT(COV(J,J))
    ELSE
     !## error value - should not happen
     PEST%PARAM(I)%ALPHA_ERROR_STDEV=-999.99D0 
     WRITE(IUPESTOUT,*) 'Active Parameter#,',J,' Variance ',COV(J,J)
     IERROR=IERROR+1
    ENDIF
    !## check whether current other parameters belong to this group
    DO IP1=1,SIZE(PEST%PARAM)
     !## active and follower of group
     IF(PEST%PARAM(IP1)%PACT.EQ.2)THEN
      IF(ABS(PEST%PARAM(IP1)%PIGROUP).EQ.PEST%PARAM(I)%PIGROUP)THEN
       PEST%PARAM(IP1)%ALPHA_ERROR_STDEV=PEST%PARAM(I)%ALPHA_ERROR_STDEV
      ENDIF
     ENDIF
    ENDDO
   ENDIF
  ENDDO
 
  IF(IERROR.GT.0)THEN
   CALL IPEST_GLM_ERROR(IBATCH,'Errors (#'//TRIM(ITOS(IERROR))//') found in the Covariance Matrix, check your matrix, might by singular'); RETURN
  ENDIF
 
  WRITE(IUPESTOUT,'(15X,3A15)') 'Lower_Limit','Average','Upper Limit'
  WRITE(IUPESTOUT,'(60A1)') ('-',I=1,60)

  DO I=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(I)%PACT.EQ.1)THEN
    ZW=PEST%PARAM(I)%ALPHA_ERROR_STDEV*1.96D0
    IF(PEST%PARAM(I)%PLOG)THEN
     Z =10.0D0**(PEST%PARAM(I)%ALPHA(2)) 
     Z1=TINY(1.0)
     IF(PEST%PARAM(I)%ALPHA(2)-ZW.LT.LOG10(HUGE(1.0)))THEN
      Z1=10.0D0**(PEST%PARAM(I)%ALPHA(2)-ZW) 
     ENDIF
     Z2=HUGE(1.0)
     IF(PEST%PARAM(I)%ALPHA(2)+ZW.LT.LOG10(HUGE(1.0)))THEN
      Z2=10.0D0**(PEST%PARAM(I)%ALPHA(2)+ZW) 
     ENDIF
    ELSE
     Z= PEST%PARAM(I)%ALPHA(2) 
     Z1=PEST%PARAM(I)%ALPHA(2)-ZW 
     Z2=PEST%PARAM(I)%ALPHA(2)+ZW 
    ENDIF 

    WRITE(BLINE,'(3G15.7)') Z1,Z,Z2
    WRITE(IUPESTOUT,'(A15,A)') PEST%PARAM(I)%ACRONYM,TRIM(BLINE)

    IF(PEST%PARAM(I)%PACT.EQ.-1)THEN
     WRITE(BLINE,'(3A15)') 'Insens.','Insens.','Insens.'
     WRITE(IUPESTOUT,'(A15,A)') PEST%PARAM(I)%ACRONYM,TRIM(BLINE)
    ENDIF
   ENDIF
  ENDDO
 ELSE
  CALL IPEST_GLM_ERROR(IBATCH,'Singular matrix, cannot compute covariance matrix: skipped.')
 ENDIF

 IF(ALLOCATED(COV ))DEALLOCATE(COV)
 IF(ALLOCATED(COR ))DEALLOCATE(COR)
 IF(ALLOCATED(INDX))DEALLOCATE(INDX)
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
! REAL(KIND=DP_KIND) :: DET
 INTEGER :: I
 REAL(KIND=DP_KIND) :: TV,TEV,KAPPA
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: B
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: E
 
 IF(ALLOCATED(E))DEALLOCATE(E); ALLOCATE(E(NP))
 IF(ALLOCATED(B))DEALLOCATE(B); ALLOCATE(B(NP,NP))

! !## compute determinant of JQJ
! DET=IPEST_GLM_DET(JQJ,NP)

! IF(LPRINT)THEN
!  WRITE(IUPESTOUT,'(A,E15.7)') 'Determinant JQJ + '//TRIM(RTOS(MARQUARDT,'F',7))//' * I = ',DET
! ENDIF

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
   IF(LPRINT)WRITE(IUPESTOUT,'(I10,G15.7,3F15.7)') I,EIGW(I),SQRT(EIGW(I)),EIGW(I)*100.0/TEV,TV
  ELSE
   IF(LPRINT)WRITE(IUPESTOUT,'(I10,G15.7,3F15.7)') I,EIGW(I),     EIGW(I) ,EIGW(I)*100.0/TEV,TV
  ENDIF
 ENDDO
 EIGV= B  

 IF(SUM(EIGW).LT.0.0D0)THEN
  CALL IPEST_GLM_ERROR(IBATCH,'Warning, there is NO information (no eigenvalues) in parameter perturbation'); RETURN
 ENDIF
 EIGW=(EIGW*100.0D0)/SUM(EIGW)  

 !## condition number
 !## get lowest non-zero
 DO I=NP,1,-1; IF(EIGW(I).GT.0.0D0)EXIT; ENDDO
 KAPPA=SQRT(EIGW(1))/SQRT(EIGW(I)); KAPPA=LOG10(KAPPA)
 IF(LPRINT)THEN
  WRITE(IUPESTOUT,'(/A,F15.7/)') 'Condition Number (kappa):',KAPPA
  IF(KAPPA.GT.15.0D0)THEN
   WRITE(IUPESTOUT,'(A)') '>>> If Kappa > 15, inversion is a concern due to parameters that are highly correlated <<<'
  ELSEIF(KAPPA.GT.30.0D0)THEN
   WRITE(IUPESTOUT,'(A)') '>>> If Kappa > 30, inversion is highly questionable due to parameters that are highly correlated <<<'
  ENDIF
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
   WRITE(CTMP(N),'(A15)') PEST%PARAM(J)%ACRONYM
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
 
 open(99,file='d:\tmp.m',status='unknown',action='write')
 write(99,*) '['
 do j=1,n
  write(99,'(999f15.7)') (jqj(i,j),i=1,n)
 enddo
 close(99)
 
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
 LOGICAL FUNCTION IPEST_GLM_GETJ(DIR,IGRAD,IPARAM,CTYPE,IBATCH,ILAMBDA) 
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,CTYPE
 INTEGER,INTENT(IN) :: IGRAD,IPARAM,IBATCH,ILAMBDA
 INTEGER :: I,J,II,NC,NP,NPERIOD,III,K,KK,ILAY,NROWIPFTXT,IUIPFTXT,NCOLIPFTXT,IOS,NAJ,IERROR,SEED
 REAL(KIND=DP_KIND) :: X,Y,Z,H,WW,MC,MM,DHH,XCOR,YCOR,ZCOR,DHW,DTH,DTD,NSC,SIGMA,F,DRES,D,WF
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
  DIRNAME=TRIM(DIR)//'\IIES_'//TRIM(CTYPE)//'#'//TRIM(ITOS(IPARAM))//'_L#'//TRIM(ITOS(ILAMBDA))//'\TIMESERIES'
 ELSE  
  DIRNAME=TRIM(DIR)//'\IPEST_'//TRIM(CTYPE)//'#'//TRIM(ITOS(IPARAM))//'\TIMESERIES'
 ENDIF
 
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
 DO I=1,SIZE(PEST%MEASURES)

  J=INDEX(PEST%MEASURES(I)%IPFNAME,'\',.TRUE.)+1
  FNAME=TRIM(DIRNAME)//'\'//TRIM(PEST%MEASURES(I)%IPFNAME(J:))
  IU=UTL_GETUNIT(); OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ')
  READ(IU,*) NR
  READ(IU,*) NC
  DO J=1,NC; READ(IU,*); ENDDO; READ(IU,*) IEXT,CEXT
  
  IF(TRIM(CTYPE).EQ.'L'.OR.TRIM(CTYPE).EQ.'R'.AND.I.EQ.1)THEN
   !## write head for the first
   IF(IUPESTRESIDUAL.GT.0.AND.I.EQ.1)THEN
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
    IF(PEST%MEASURES(I)%IDCOL.EQ.0)THEN
     D=0.0D0
     READ(IU,*) X,Y,ILAY,Z,WW,H
    ELSE
     READ(IU,*) X,Y,ILAY,Z,WW,H,D
    ENDIF
    
    !## entries in stdev - weight is 1/stdev2
    IF(PEST%MEASURES(I)%IVCOL.GT.0)THEN
     IF(WW.LT.0.0D0)THEN; WW=0.0D0; ELSE; WW=1.0D0/WW**2.0D0; ENDIF
    ENDIF

    !## weight as ... weigth    
    MSR%W(II)=WW
    
    !## random error not yet set
    IF(PBMAN%IIES.EQ.1)THEN
     IERROR=IGRAD
     IF(MSR%E(IERROR,II).LT.-999.0D0)THEN
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
    DRES=PEST%PE_DRES; IF(DRES.EQ.0.0D0)DRES=D
    
    IF(ABS(DHH).LT.DRES)THEN
     DHH=0.0D0
    ELSE
     IF(DHH.GT. DRES)DHH=DHH-DRES
     IF(DHH.LT.-DRES)DHH=DHH+DRES
    ENDIF

    !## save information for measurement
    IF(TRIM(CTYPE).EQ.'P'.OR.TRIM(CTYPE).EQ.'R')THEN
     MSR%DHG(IGRAD,II)=DHH  
    ELSEIF(TRIM(CTYPE).EQ.'L')THEN
     MSR%DHL(IGRAD,II)=DHH  
    ENDIF
    
    MSR%X(II)=X; MSR%Y(II)=Y; MSR%L(II)=ILAY
    MSR%CLABEL(II)='MEASURE'//TRIM(ITOS(J))//'_IPF'//TRIM(ITOS(I))

    GF_H(II)=H
    GF_O(II)=Z

    !## add to total objective function
    DHW=MSR%W(II)*(DHH**2.0D0); MSR%TJ=MSR%TJ+DHW
    IF(IUPESTRESIDUAL.GT.0.AND.(TRIM(CTYPE).EQ.'L'.OR.TRIM(CTYPE).EQ.'R'))THEN
     WRITE(IUPESTRESIDUAL,'(2(F15.2,A1),I10,A1,6(F15.3,A1),I10,A1,A32)') &
        X,',',Y,',',ILAY,',',Z,',',H,',',DHW,',',MSR%W(II)*H,',',MSR%W(II)*DHH,',',MSR%W(II),',',I,',',MSR%CLABEL(II)
    ENDIF
    
   ENDDO
  
  !## transient
  ELSE
    
   IUIPFTXT=0
   DO J=1,NR
    IF(PEST%MEASURES(I)%IDCOL.EQ.0)THEN
     READ(IU,*) X,Y,ILAY,CID,WW
     D=0.0D0
    ELSE
     READ(IU,*) X,Y,ILAY,CID,WW,D
    ENDIF
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
     ENDIF
     
     !## add observation
     DO K=1,KK
      II =II+1

      !## random error not yet set
      IF(PBMAN%IIES.EQ.1)THEN
       IERROR=IGRAD
       IF(MSR%E(IERROR,II).LT.-999.0D0)THEN
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
      DRES=PEST%PE_DRES; IF(DRES.EQ.0.0D0)DRES=D

      !## target is residual (calculated minus measured)
      IF(PEST%PE_TARGET(1).GT.0.0D0)THEN
       DTH=C(K)-M(K)
       IF(ABS(DTH).LT.DRES)THEN
        DTH=0.0D0
       ELSE
        IF(DTH.GT. DRES)DTH=DTH-DRES
        IF(DTH.LT.-DRES)DTH=DTH+DRES
       ENDIF
       DTH=PEST%PE_TARGET(1)*DTH
      ENDIF
      
      !## target is dynamics (calculated minus measured)
      DTD=0.0D0
      IF(PEST%PE_TARGET(2).GT.0.0D0)THEN
       DTD=DYN(2)-DYN(1)
       IF(ABS(DTD).LT.DRES)THEN
        DTD=0.0D0
       ELSE
        IF(DTD.GT. DRES)DTD=DTD-DRES
        IF(DTD.LT.-DRES)DTD=DTD+DRES
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

      GF_H(II)=C(K)
      GF_O(II)=M(K)

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
  CALL IPEST_GLM_ERROR(IBATCH,'No measurements available within current spatial/temporal space.'); RETURN
 ENDIF
 
! !## run batch files
! CALL PEST_BATCHFILES()
!
 !## insert regularisation to objective function
 NP=0; DO I=1,SIZE(PEST%PARAM); IF(PEST%PARAM(I)%PACT.EQ.0.OR.PEST%PARAM(I)%PIGROUP.LE.0)CYCLE; NP=NP+1; ENDDO
 
 !## add parameter regularisation
 IF(PEST%PE_REGULARISATION.EQ.1)THEN
 
  !## add weight to balance number of observations and parameters
  NC=0; DO I=1,SIZE(PEST%PARAM)  !## row
   !## skip inactive; skip others parts of parameter
   IF(PEST%PARAM(I)%PACT.EQ.0)CYCLE; IF(PEST%PARAM(I)%PIGROUP.LE.0)CYCLE; NC=NC+1
  ENDDO
  WF=REAL(MSR%NOBS,8)/REAL(NC,8)
  
  DO I=1,SIZE(PEST%PARAM)  !## row
   !## skip inactive
   IF(PEST%PARAM(I)%PACT.EQ.0)CYCLE
   !## skip others parts of parameter
   IF(PEST%PARAM(I)%PIGROUP.LE.0)CYCLE
   IF(CTYPE.EQ.'L')THEN
    F=(PEST%PARAM(I)%LALPHA(IGRAD)-PEST%PARAM(I)%PPRIOR)**2.0D0
   ELSEIF(CTYPE.EQ.'P')THEN
    F=(PEST%PARAM(I)%GALPHA(IGRAD)-PEST%PARAM(I)%PPRIOR)**2.0D0
   ENDIF
   WW=1.0D0/PEST%PARAM(I)%PARSTD**2.0D0
   !## add balance-weigting
   WW=WW*WF
   MSR%RJ=MSR%RJ+F*WW
  ENDDO
 ENDIF

 MSR%TJ=MSR%TJ+MSR%RJ
 
 IPEST_GLM_GETJ=.TRUE.

 END FUNCTION IPEST_GLM_GETJ

 !#####=================================================================
 SUBROUTINE IPEST_GLM_PROGRESS(ITER,JGRAD,IGRAD,CTYPE,LAMBDA)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=1),INTENT(IN) :: CTYPE
 INTEGER,INTENT(IN) :: JGRAD,IGRAD,ITER
 REAL(KIND=DP_KIND),INTENT(IN) :: LAMBDA
 REAL(KIND=DP_KIND) :: X1,X2,ET
 INTEGER :: CLOCK_RATE
 
 IF(CTYPE.EQ.'P')THEN
  !## present parameter value
  X1=PEST%PARAM(IGRAD)%GALPHA(JGRAD)
  IF(PEST%PARAM(IGRAD)%PLOG)X1=10.0D0**X1
  X2=PEST%PARAM(IGRAD)%ALPHA(2)
  IF(PEST%PARAM(IGRAD)%PLOG)X2=10.0D0**X2
 ELSEIF(CTYPE.EQ.'R')THEN
  !## present lambda
  X1=LAMBDA 
 ELSEIF(CTYPE.EQ.'L')THEN
  !## present lambda
  X1=LAMBDA*PBMAN%LAMBDA_TEST(JGRAD) 
 ENDIF

 CALL SYSTEM_CLOCK(ETIME(JGRAD),CLOCK_RATE)
 ET=REAL(ETIME(JGRAD)-STIME(JGRAD),8)/REAL(CLOCK_RATE,8)

 !## sensitivity
 IF(CTYPE.EQ.'P')THEN
  WRITE(IUPESTPROGRESS,'(I5,A15,4F15.3,30X,F15.3)') IGRAD,PEST%PARAM(IGRAD)%ACRONYM,X1,MSR%TJ,MSR%TJ-MSR%PJ,X2,ET
 !## lambda testing
 ELSEIF(CTYPE.EQ.'L')THEN
  IF(ITER.EQ.0)THEN
   WRITE(IUPESTPROGRESS,'(I5,30X,F15.3,30X,3F15.3)') 0,MSR%TJ,MSR%GOF(JGRAD),MSR%NSC(JGRAD),ET
  ELSE
   WRITE(IUPESTPROGRESS,'(I5,A15,G15.7,2F15.3,15X,3F15.3)') IGRAD,'LAMBDA'//TRIM(ITOS(JGRAD)),X1,MSR%TJ,MSR%TJ-MSR%PJ, &
                                        MSR%GOF(JGRAD),MSR%NSC(JGRAD),ET
  ENDIF
 !## realization
 ELSEIF(CTYPE.EQ.'R')THEN
  IF(ITER.EQ.0)THEN
   WRITE(IUPESTPROGRESS,'(I5,A15,2F15.3,30X,3F15.3)')       IGRAD,'REALS'//TRIM(ITOS(JGRAD)),X1,MSR%TJ,MSR%GOF(JGRAD),MSR%NSC(JGRAD),ET
  ELSE
   WRITE(IUPESTPROGRESS,'(I5,A15,2F15.3,F15.3,15X,3F15.3)') IGRAD,'REALS'//TRIM(ITOS(JGRAD)),X1,MSR%TJ,MSR%TJ-JE(IGRAD,ITER-1),&
                                        MSR%GOF(JGRAD),MSR%NSC(JGRAD),ET
  ENDIF
  JE(IGRAD,ITER)=MSR%TJ
  WRITE(IUPESTOUT,'(I5,7X,F15.3)') ITER,MSR%TJ
 ENDIF
 
 END SUBROUTINE IPEST_GLM_PROGRESS
 
 !###====================================================================
 LOGICAL FUNCTION IPEST_GLM_ALLOCATEMSR() 
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,N1,N2,N,M,O,IOS,IU
 
 IPEST_GLM_ALLOCATEMSR=.FALSE.
 
 !## open files and get total maximum number of observations
 N=0; DO I=1,SIZE(PEST%MEASURES)
  IU=UTL_GETUNIT(); OPEN(IU,FILE=PEST%MEASURES(I)%IPFNAME,STATUS='OLD',ACTION='READ')
  READ(IU,*) M; N=N+M; CLOSE(IU)
 ENDDO

 !## get dimension of possible maximal number of observations
 M=N*PRJNPER 
 
 CALL IPEST_GLM_DEALLOCATEMSR()
 !## number of line-searches (-) and number of gradients-simulations (+)
 N1=0; IF(ALLOCATED(RNL))N1=SIZE(RNL)
 N2=0; IF(ALLOCATED(RNG))N2=SIZE(RNG)

 O=PEST%PE_MXITER
 ALLOCATE(MSR%GOF_H(0:O),STAT=IOS);  IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%GOF_H';   RETURN; ENDIF
 ALLOCATE(MSR%NSC_H(0:O),STAT=IOS);  IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%NSC_H';   RETURN; ENDIF
 ALLOCATE(MSR%TJ_H(0:O),STAT=IOS);   IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%TJ_H';    RETURN; ENDIF
 ALLOCATE(MSR%RJ_H(0:O),STAT=IOS);   IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%RJ_H';    RETURN; ENDIF
 IF(N1.GT.0)THEN
  ALLOCATE(MSR%GOF(N1),STAT=IOS);     IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%GOF';    RETURN; ENDIF
  ALLOCATE(MSR%NSC(N1),STAT=IOS);     IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%NSC';    RETURN; ENDIF
  ALLOCATE(MSR%DHL_J(N1),STAT=IOS);   IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%DHL_J';  RETURN; ENDIF
  IF(M.GT.0)THEN
   ALLOCATE(MSR%DHL(0:N1,M),STAT=IOS); IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%DHL';   RETURN; ENDIF
  ENDIF
 ENDIF
 IF(PBMAN%IIES.EQ.1)THEN
  ALLOCATE(MSR%GOF(N2),STAT=IOS);     IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%GOF';    RETURN; ENDIF
  ALLOCATE(MSR%NSC(N2),STAT=IOS);     IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%NSC';    RETURN; ENDIF
  ALLOCATE(MSR%DHL_J(N2),STAT=IOS);   IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%DHL_J';  RETURN; ENDIF
 ENDIF
 IF(N2.GT.0)THEN
  ALLOCATE(MSR%DHG_J(N2),STAT=IOS);   IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%DHG_J';  RETURN; ENDIF
  IF(M.GT.0)THEN
   ALLOCATE(MSR%DHG(N2,M),STAT=IOS);   IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%DHG';   RETURN; ENDIF
  ENDIF
 ENDIF
 IF(M.GT.0)THEN
  ALLOCATE(MSR%W(M),STAT=IOS);        IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%W';      RETURN; ENDIF
  ALLOCATE(MSR%X(M),STAT=IOS);        IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%X';      RETURN; ENDIF
  ALLOCATE(MSR%Y(M),STAT=IOS);        IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%Y';      RETURN; ENDIF
  ALLOCATE(MSR%L(M),STAT=IOS);        IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%L';      RETURN; ENDIF
  ALLOCATE(MSR%CLABEL(M),STAT=IOS);   IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR MSR%CLABEL'; RETURN; ENDIF
  ALLOCATE(GF_H(M),STAT=IOS);         IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR GF_H';       RETURN; ENDIF
  ALLOCATE(GF_O(M),STAT=IOS);         IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR GF_O';       RETURN; ENDIF
 ENDIF
 
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

 IF(ASSOCIATED(MSR%TJ_H))  DEALLOCATE(MSR%TJ_H)
 IF(ASSOCIATED(MSR%RJ_H))  DEALLOCATE(MSR%RJ_H)
 IF(ASSOCIATED(MSR%GOF_H)) DEALLOCATE(MSR%GOF_H)
 IF(ASSOCIATED(MSR%NSC_H)) DEALLOCATE(MSR%NSC_H)
 IF(ASSOCIATED(MSR%GOF))   DEALLOCATE(MSR%GOF)
 IF(ASSOCIATED(MSR%NSC))   DEALLOCATE(MSR%NSC)
 IF(ASSOCIATED(MSR%DHL_J)) DEALLOCATE(MSR%DHL_J)
 IF(ASSOCIATED(MSR%DHG_J)) DEALLOCATE(MSR%DHG_J)
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
 
 !###====================================================================
 SUBROUTINE IPEST_LUDECOMP_DBL(AA,IDX,N,ISING)
 !###====================================================================
 IMPLICIT NONE
! INTEGER,PARAMETER :: NMAX=2000
 REAL(KIND=DP_KIND),PARAMETER :: TINY=1.0D-20
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT) :: ISING
 INTEGER,DIMENSION(N),INTENT(OUT) :: IDX
 REAL(KIND=DP_KIND),DIMENSION(N,N),INTENT(INOUT) :: AA
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: VV
 REAL(KIND=DP_KIND) :: AAMAX,DUM,SUM
 INTEGER :: I,IMAX,J,K

 ALLOCATE(VV(N)); VV=0.0D0
 
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

 DEALLOCATE(VV)
 
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
