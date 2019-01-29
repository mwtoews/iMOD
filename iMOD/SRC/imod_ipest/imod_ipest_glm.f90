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
MODULE MOD_IPEST_GLM

USE WINTERACTER
USE IMODVAR, ONLY : DP_KIND
USE MOD_PMANAGER_PAR, ONLY : PEST,SIM,PARAM,PRJNLAY,PBMAN
USE MOD_UTL, ONLY  : UTL_GETUNIT,UTL_CAP,ITOS,RTOS,UTL_GETMED,UTL_CREATEDIR
USE MOD_IPEST_GLM_PAR

CONTAINS

 !#####=================================================================
 SUBROUTINE IPEST_GLM_MAIN(DIR)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: MXLNSRCH=20 !# maximum of sequential linesearches
 INTEGER,PARAMETER :: NCSECS=1000
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: I,ITER,N,IFLAGS,ISTATUS,IEXCOD,ITYPE,IGRAD,JGRAD,ILNSRCH,NCPU
 TYPE(WIN_MESSAGE) :: MESSAGE

 !## open output files
 CALL UTL_CREATEDIR(TRIM(DIR)//'\IPEST')
 IUPESTOUT=UTL_GETUNIT();         OPEN(IUPESTOUT,        FILE=TRIM(DIR)//'\IPEST\LOG_PEST.TXT'            ,STATUS='UNKNOWN',ACTION='WRITE')
 IUPESTPROGRESS=UTL_GETUNIT();    OPEN(IUPESTPROGRESS,   FILE=TRIM(DIR)//'\IPEST\LOG_PEST_PROGRESS.TXT'   ,STATUS='UNKNOWN',ACTION='WRITE')
 IUPESTEFFICIENCY=UTL_GETUNIT();  OPEN(IUPESTEFFICIENCY, FILE=TRIM(DIR)//'\IPEST\LOG_PEST_EFFICIENCY.TXT' ,STATUS='UNKNOWN',ACTION='WRITE')
 IUPESTSENSITIVITY=UTL_GETUNIT(); OPEN(IUPESTSENSITIVITY,FILE=TRIM(DIR)//'\IPEST\LOG_PEST_SENSITIVITY.TXT',STATUS='UNKNOWN',ACTION='WRITE')
 IUPESTRUNFILE=UTL_GETUNIT();     OPEN(IUPESTRUNFILE,    FILE=TRIM(DIR)//'\IPEST\LOG_PEST_RUNFILE.TXT'    ,STATUS='UNKNOWN',ACTION='WRITE')
 
 N=0 ; DO I=1,SIZE(PEST%PARAM); IF(PEST%PARAM(I)%PACT.EQ.0.OR.PEST%PARAM(I)%PIGROUP.LT.0)CYCLE; N=N+1; ENDDO
 CALL IPEST_GLM_ALLOCATE(N)
 N=-1; DO I=0,SIZE(PEST%PARAM)
  IF(I.GT.0)THEN; IF(PEST%PARAM(I)%PACT.EQ.0.OR.PEST%PARAM(I)%PIGROUP.LT.0)CYCLE; ENDIF; N=N+1; RN(N)=TRIM(DIR)//'\RUN_P#'//TRIM(ITOS(I))//'.BAT'
 ENDDO

 !## set initial values for alpha()
 DO I=1,SIZE(PEST%PARAM); CALL IPEST_GLM_CHK(I); ENDDO
 
 IUPESTRESIDUAL=0; CALL WMESSAGEENABLE(TIMEREXPIRED,1); MSR%PJ=HUGE(1.0D0)
 DO ITER=1,PEST%PE_MXITER

  IF(IUPESTRESIDUAL.GT.0)CLOSE(IUPESTRESIDUAL)
  IUPESTRESIDUAL=UTL_GETUNIT(); OPEN(IUPESTRESIDUAL,FILE=TRIM(DIR)//'\IPEST\LOG_PEST_RESIDUAL_'//TRIM(ITOS(ITER))//'.TXT',STATUS='UNKNOWN',ACTION='WRITE')

  !## executes on commandtool such that commands alike 'dir' etc. works
  IFLAGS=PROCCMDPROC+PROCBLOCKED

  !## compute the initial simulation first
  IGRAD=0
  DO ILNSRCH=1,MXLNSRCH
   !## define update in pst file
   IF(.NOT.IPEST_GLM_PST(DIR,0))THEN; WRITE(*,'(/A/)') 'ERROR CREATED PST1 FILE FOR P#'//TRIM(ITOS(IGRAD)); STOP; ENDIF
   !## run model
   CALL IOSCOMMAND(TRIM(RN(IGRAD)),IFLAGS=IFLAGS)
   IF(WINFOERROR(1).EQ.ERROSCOMMAND)THEN; WRITE(*,'(/A/)') 'FAILED TO START MODEL P#'//TRIM(ITOS(IGRAD)); STOP; ENDIF
   !## get objective function value
   CALL IPEST_GLM_GETJ(DIR,IGRAD)
   CALL IPEST_GLM_PROGRESS(ITER,IGRAD,ILNSRCH)
   !## evaluate whether in line-search objective function value is reduced compared to previous objective function value
   IF(MSR%TJ.LE.MSR%PJ)EXIT
   !## modify alpha ...
!   DAMPINGFACTOR=DAMPINGFACTOR*NDAMPING
!!   NDAMPING=1.0 !## do it onces only

   !## meerdere line-searches tegelijkertijd ---
   IF(.NOT.IPEST_GLM_UPGRADEVECTOR(0.5D0,.FALSE.,ITER))THEN;  WRITE(*,'(/A/)') 'STOP ERROR IN LINE-SEARCH'; STOP; ENDIF

  ENDDO
  
  !## update alphas
  CALL IPEST_GLM_NEXT(ITER-1)

  !## executes on commandtool such that commands alike 'dir' etc. works
  IFLAGS=PROCCMDPROC

  !## start finite difference approximation for parameters
  CALL WMESSAGETIMER(NCSECS,IREPEAT=1); ILNSRCH=0; IGRAD=0

  !## number of cpu occupied
  NCPU=0; IGRAD=0; IPROC=0
  DO 

   DO
    IGRAD=IGRAD+1
    !## number of cpu full
    NCPU=NCPU+1
    !## rekening houden met op te geven aantal processoren
    !## adjust alpha for current igrad
    CALL IPEST_GLM_NEXTGRAD(IGRAD)
    !## define update in pst file
    IF(.NOT.IPEST_GLM_PST(DIR,IGRAD))THEN; WRITE(*,'(/A/)') 'ERROR CREATED PST1 FILE FOR P#'//TRIM(ITOS(IGRAD)); STOP; ENDIF
    CALL IOSCOMMAND(TRIM(RN(IGRAD)),IFLAGS=IFLAGS,IDPROC=IPROC(:,IGRAD))
    IF(WINFOERROR(1).EQ.ERROSCOMMAND)THEN; WRITE(*,'(/A/)') 'FAILED TO START MODEL P#'//TRIM(ITOS(IGRAD)); STOP; ENDIF
    !## all started
    IF(IGRAD.EQ.SIZE(RN)-1)EXIT
    !## maximum number of cpu reached
    IF(NCPU.GE.PBMAN%NCPU)EXIT
   ENDDO

   !## check processes
   DO
    CALL WMESSAGE(ITYPE,MESSAGE)
    !## timer expired
    IF(ITYPE.EQ.TIMEREXPIRED)THEN

     N=0; DO JGRAD=1,SIZE(RN)-1
      !## all handled process
      IF(IPROC(1,JGRAD)+IPROC(2,JGRAD).EQ.0)CYCLE
      !## check running status
      CALL IOSCOMMANDCHECK(IPROC(:,JGRAD),ISTATUS,IEXCOD=IEXCOD)
      !## stopped running
      IF(ISTATUS.EQ.0)THEN
       !## error occured 
       IF(IEXCOD.NE.0)THEN; WRITE(*,'(/A/)') 'ERROR OCCURED RUNNING MODEL P#'//TRIM(ITOS(JGRAD)); STOP; ENDIF
       !## set part of objective function
       CALL IPEST_GLM_GETJ(DIR,JGRAD)
       !## write echo
       CALL IPEST_GLM_PROGRESS(ITER,JGRAD,ILNSRCH)
       !## reset running handle proces
       IPROC(:,JGRAD)=0
       !## release cpu so another can be started
       NCPU=NCPU-1
      ELSE
       !# still running
       N=N+1
      ENDIF
     ENDDO   
     !## nothing running anymore
     IF(N.EQ.0)EXIT
     IF(N.GT.0)WRITE(*,'(/A/)') 'STILL RUNNING '//TRIM(ITOS(N))//' MODELS'
     !## start another one as a proces has been stopped and there is still one waiting in the que
     IF(NCPU.LT.PBMAN%NCPU.AND.IGRAD.LT.SIZE(RN)-1)EXIT
    ENDIF
   ENDDO
   !## finished
   IF(IGRAD.EQ.SIZE(RN)-1)EXIT
  ENDDO
  
  !## determine new gradient
  CALL IPEST_GLM_GRADIENT(ITER)

 ENDDO
 CALL WMESSAGETIMER(0); CALL WMESSAGEENABLE(TIMEREXPIRED,0)
 
 END SUBROUTINE IPEST_GLM_MAIN

  !###====================================================================
 SUBROUTINE IPEST_GLM_CHK(IP)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IP
 INTEGER :: I
 
 DO I=1,SIZE(PARAM); IF(TRIM(PEST%PARAM(IP)%PPARAM).EQ.TRIM(PARAM(I)))EXIT; ENDDO
 IF(I.GT.SIZE(PARAM))THEN
  WRITE(*,'(/A)') 'Error can not recognize parameter type:'//TRIM(PEST%PARAM(IP)%PPARAM)
  WRITE(*,'(/A)') ' Choose from:'
  DO I=1,SIZE(PARAM); WRITE(*,'(A)') ' - '//TRIM(PARAM(I)); ENDDO
  WRITE(*,'(A)'); STOP
 ENDIF
 IF(PEST%PARAM(IP)%PMIN.GE.PEST%PARAM(IP)%PMAX)THEN; WRITE(*,'(/A/)') 'No proper parameter width defined for parameter '//TRIM(ITOS(IP)); STOP; ENDIF
 IF(PEST%PARAM(IP)%PINI.LT.PEST%PARAM(IP)%PMIN.OR.PEST%PARAM(IP)%PINI.GT.PEST%PARAM(IP)%PMAX)THEN
  WRITE(*,'(/A/)') 'Parameter '//TRIM(ITOS(IP))//' outside parameter width'; STOP
 ENDIF
 SELECT CASE (TRIM(PEST%PARAM(IP)%PPARAM))
  CASE ('KD','KH','SC','AF','VA')
   IF(PEST%PARAM(IP)%PILS.LE.0.OR.PEST%PARAM(IP)%PILS.GT.PRJNLAY)THEN; WRITE(*,'(/A/)') 'Parameter '//TRIM(ITOS(IP))//': ILS exceeds NLAY'; STOP; ENDIF
  CASE ('VC','KV')
   IF(PEST%PARAM(IP)%PILS.LE.0.OR.PEST%PARAM(IP)%PILS.GT.PRJNLAY-1)THEN; WRITE(*,'(/A/)') 'Parameter '//TRIM(ITOS(IP))//': ILS exceeds NLAY-1'; STOP; ENDIF
  CASE ('EP','RE')
   IF(PEST%PARAM(IP)%PILS.NE.1)THEN; WRITE(*,'(/A/)') 'Parameter '//TRIM(ITOS(IP))//': ILS need to be equal to 1'; STOP; ENDIF
 END SELECT

 !## scaling
 IF(PEST%PARAM(IP)%PLOG.EQ.1)THEN
  IF(PEST%PARAM(IP)%PDELTA.EQ.1.0)WRITE(*,'(/A/)') 'You can not specify delta alpha eq 1.0 for log-transformed parameters'
  IF(PEST%PARAM(IP)%PMIN  .EQ.0.0)WRITE(*,'(/A/)') 'You can not specify minimal value eq 0.0 for log-transformed parameters'
  PEST%PARAM(IP)%PINI  =LOG(PEST%PARAM(IP)%PINI)
  PEST%PARAM(IP)%PMIN  =LOG(PEST%PARAM(IP)%PMIN)
  PEST%PARAM(IP)%PMAX  =LOG(PEST%PARAM(IP)%PMAX)
  PEST%PARAM(IP)%PDELTA=LOG(PEST%PARAM(IP)%PDELTA)
 ENDIF
 PEST%PARAM(IP)%ALPHA(1)=PEST%PARAM(IP)%PINI !## current  alpha
 PEST%PARAM(IP)%ALPHA(2)=PEST%PARAM(IP)%PINI !## previous alpha
 ALLOCATE(PEST%PARAM(IP)%ALPHA_HISTORY(0:PEST%PE_MXITER)); PEST%PARAM(IP)%ALPHA_HISTORY=0.0D0
 ALLOCATE(PEST%PARAM(IP)%ALPHA_ERROR_VARIANCE(0:PEST%PE_MXITER)); PEST%PARAM(IP)%ALPHA_ERROR_VARIANCE=0.0D0

 END SUBROUTINE IPEST_GLM_CHK
 
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
 LOGICAL FUNCTION IPEST_GLM_PST(DIR,IGRAD)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER,INTENT(IN) :: IGRAD
 CHARACTER(LEN=256) :: DIRNAME
 CHARACTER(LEN=52) :: MNAME
 INTEGER :: I,N,IU,JU,IOS
 
 IPEST_GLM_PST=.FALSE.
 
 MNAME=DIR(INDEX(DIR,'\',.TRUE.)+1:)
 DIRNAME=TRIM(DIR)//'\MODELINPUT\'//TRIM(MNAME)//'_P#'//TRIM(ITOS(IGRAD))//'.PST1'
 IU=UTL_GETUNIT(); OPEN(IU,FILE=DIRNAME,STATUS='OLD',ACTION='READ')
 DIRNAME=TRIM(DIR)//'\MODELINPUT\'//TRIM(MNAME)//'_P#'//TRIM(ITOS(IGRAD))//'.PST1_'
 JU=UTL_GETUNIT(); OPEN(JU,FILE=DIRNAME,STATUS='UNKNOWN',ACTION='WRITE')
 
 !## copy header
 DO I=1,5; READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)RETURN; WRITE(JU,'(A)') TRIM(LINE); ENDDO

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
   LINE=TRIM(ITOS(PEST%PARAM(I)%PACT))           //','// &
        TRIM(PEST%PARAM(I)%PPARAM)               //','// &
        TRIM(ITOS(PEST%PARAM(I)%PILS))           //','// &
        TRIM(ITOS(PEST%PARAM(I)%PIZONE))         //','// &    
        TRIM(RTOS(PEST%PARAM(I)%ALPHA(1),'G',7))     //','// &
        TRIM(RTOS(PEST%PARAM(I)%PDELTA,'G',7))   //','// &
        TRIM(RTOS(PEST%PARAM(I)%PMIN,'G',7))     //','// &
        TRIM(RTOS(PEST%PARAM(I)%PMAX,'G',7))     //','// &
        TRIM(RTOS(PEST%PARAM(I)%PINCREASE,'G',7))//','// &
        TRIM(ITOS(PEST%PARAM(I)%PIGROUP))        //','// &
        TRIM(ITOS(PEST%PARAM(I)%PLOG))
   IF(TRIM(PEST%PARAM(I)%ACRONYM).NE.'')LINE=TRIM(LINE)//','//TRIM(PEST%PARAM(I)%ACRONYM)
  ELSE
   LINE=TRIM(ITOS(PEST%PARAM(I)%PACT))             //','// &
        TRIM(PEST%PARAM(I)%PPARAM)                 //','// &
        TRIM(ITOS(PEST%PARAM(I)%PILS))             //','// &
        TRIM(ITOS(PEST%PARAM(I)%PIZONE))           //','// &    
        TRIM(RTOS(EXP(PEST%PARAM(I)%ALPHA(1)),'G',7))  //','// &
        TRIM(RTOS(EXP(PEST%PARAM(I)%PDELTA),'G',7))//','// &
        TRIM(RTOS(EXP(PEST%PARAM(I)%PMIN),'G',7))  //','// &
        TRIM(RTOS(EXP(PEST%PARAM(I)%PMAX),'G',7))  //','// &
        TRIM(RTOS(PEST%PARAM(I)%PINCREASE,'G',7))  //','// &
        TRIM(ITOS(PEST%PARAM(I)%PIGROUP))          //','// &
        TRIM(ITOS(PEST%PARAM(I)%PLOG))
   IF(TRIM(PEST%PARAM(I)%ACRONYM).NE.'')LINE=TRIM(LINE)//','//TRIM(PEST%PARAM(I)%ACRONYM)
  ENDIF
  WRITE(JU,'(A)') TRIM(LINE)
 ENDDO
 
 !## copy zones
 READ(IU,*,IOSTAT=IOS) N; IF(IOS.NE.0)RETURN; WRITE(JU,*) N
 READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)RETURN; WRITE(JU,'(A)') TRIM(LINE)
 
 CLOSE(IU,STATUS='DELETE'); CLOSE(JU)
 CALL IOSRENAMEFILE(TRIM(DIR)//'\MODELINPUT\'//TRIM(MNAME)//'_P#'//TRIM(ITOS(IGRAD))//'.PST1_', &
                    TRIM(DIR)//'\MODELINPUT\'//TRIM(MNAME)//'_P#'//TRIM(ITOS(IGRAD))//'.PST1')
 
 IPEST_GLM_PST=.TRUE.
 
 END FUNCTION IPEST_GLM_PST

 !#####=================================================================
 SUBROUTINE IPEST_GLM_ALLOCATE(N)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 
 CALL IPEST_GLM_DEALLOCATE()
 ALLOCATE(RN(0:N),IPROC(2,0:N))!,ICPU(N))
 
 END SUBROUTINE IPEST_GLM_ALLOCATE

 !#####=================================================================
 SUBROUTINE IPEST_GLM_DEALLOCATE()
 !#####=================================================================
 IMPLICIT NONE

 IF(ALLOCATED(RN))   DEALLOCATE(RN)
 IF(ALLOCATED(IPROC))DEALLOCATE(IPROC)
! IF(ALLOCATED(ICPU))  DEALLOCATE(ICPU)
 
 END SUBROUTINE IPEST_GLM_DEALLOCATE
 
 !#####=================================================================
 SUBROUTINE IPEST_GLM_NEXT(ITER)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER
 REAL(KIND=DP_KIND) :: IMPROVEMENT,F,GUPDATE
 INTEGER :: I,J,ILOG

 !## not necessary for first cycle
 IF(ITER.EQ.0)RETURN
  
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
  IF(PEST%PARAM(I)%PACT.EQ.0)CYCLE
  IF(PEST%PARAM(I)%PIGROUP.GT.0)THEN
   DO J=1,SIZE(PEST%PARAM)
    IF(PEST%PARAM(I)%PIGROUP.EQ.ABS(PEST%PARAM(J)%PIGROUP))PEST%PARAM(J)%ALPHA(1)=PEST%PARAM(I)%ALPHA(1)
   ENDDO
  ENDIF
 ENDDO
   
 IMPROVEMENT=0; DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PACT.EQ.0)CYCLE
  IF(PEST%PARAM(I)%PIGROUP.LT.0)CYCLE
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
   WRITE(IUPESTRUNFILE,'(I2,1X,A,1X,2(I4,1X),5(F10.3,1X),I4,1X,I2,1X,A)') ABS(PEST%PARAM(I)%PACT), &  !## iact
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
       TRIM(PEST%PARAM(I)%ACRONYM)
  ELSE
   WRITE(IUPESTRUNFILE,'(I2,1X,A,1X,2(I4,1X),5(F10.3,1X),I4,1X,I2,1X,A)') ABS(PEST%PARAM(I)%PACT), &  !## iact
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
       TRIM(PEST%PARAM(I)%ACRONYM)
  ENDIF 
 ENDDO

 !## length of gradient update vector
 IF(GUPDATE.LT.PEST%PE_PADJ)THEN; WRITE(IUPESTOUT,'(/A/)') 'Process stopped, less than '//TRIM(RTOS(PEST%PE_PADJ,'F',3))//' of vector length'; STOP; ENDIF
 !## continue ?
 IF(ITER+1.GT.PEST%PE_MXITER)THEN
  WRITE(IUPESTOUT,'(/A/)') 'Pest iteration terminated: PEST_ITER (='//TRIM(ITOS(PEST%PE_MXITER))//') = PEST_NITER (='//TRIM(ITOS(PEST%PE_MXITER))//')'; STOP
 ENDIF
 IF(MSR%TJ.LE.0.0D0)THEN
  WRITE(IUPESTOUT,'(/A/)') 'Objective Function <= 0.0 ('//TRIM(RTOS(MSR%TJ,'G',7))//')'; STOP
 ENDIF
 IF(IMPROVEMENT.LE.PEST%PE_STOP)THEN
  WRITE(IUPESTOUT,'(/A/)') 'Pest iteration terminated decrease objective function ('//TRIM(RTOS(100.0D0*IMPROVEMENT,'G',7))// &
       '%) > PEST_JSTOP ('//TRIM(RTOS(100.0D0*PEST%PE_STOP,'G',7))//'%)'; STOP
 ENDIF
 
 !## copy current objective function value to previous objective function value
 MSR%PJ=MSR%TJ

 !## replace old by new parameter values
 PEST%PARAM%ALPHA(2)=PEST%PARAM%ALPHA(1)

 !## next iteration
 WRITE(*,'(/A/)') ' *** Next Outer Iteration ***'
 
 END SUBROUTINE IPEST_GLM_NEXT

 !#####=================================================================
 SUBROUTINE IPEST_GLM_NEXTGRAD(IGRAD)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER :: I
 INTEGER,INTENT(IN) :: IGRAD
 REAL(KIND=DP_KIND) :: FCT
 
 !DO
 ! IGRAD=IGRAD+1
 ! !## all gradients processed
 ! IF(IGRAD.GT.SIZE(PEST%PARAM))EXIT
 ! !## zero gradient in case parameter is fixed
 ! IF(PEST%PARAM(IGRAD)%PACT.EQ.0)THEN
 !  DO I=1,SIZE(MSR%DH,2); MSR%DH(IGRAD,I)=MSR%DH(0,I); ENDDO
 ! !## check whether the parameters has been modified allready since it belongs to the same group
 ! ELSEIF(PEST%PARAM(IGRAD)%PIGROUP.LT.0)THEN
 !  DO I=1,SIZE(MSR%DH,2); MSR%DH(IGRAD,I)=MSR%DH(0,I); ENDDO
 ! !## possible candidate found
 ! ELSE
 !  EXIT
 ! ENDIF
 !ENDDO
 !!## proceed gradient
 !IF(IGRAD.LE.SIZE(PEST%PARAM))THEN

 !## reset all alpha's
 PEST%PARAM%ALPHA(1)=PEST%PARAM%ALPHA(2)

 !## adjust all parameters within the same group
 DO I=1,SIZE(PEST%PARAM)
  !## skip inactive parameters
  IF(PEST%PARAM(I)%PACT.EQ.0)CYCLE
  IF(ABS(PEST%PARAM(I)%PIGROUP).EQ.ABS(PEST%PARAM(IGRAD)%PIGROUP))THEN
   IF(PEST%PARAM(I)%PLOG.EQ.1)THEN
    PEST%PARAM(I)%ALPHA(1)=PEST%PARAM(I)%ALPHA(2)+PEST%PARAM(I)%PDELTA
    FCT=EXP(PEST%PARAM(I)%ALPHA(1))
   ELSE
    PEST%PARAM(I)%ALPHA(1)=PEST%PARAM(I)%ALPHA(2)*PEST%PARAM(I)%PDELTA
    FCT=PEST%PARAM(I)%ALPHA(1)
   ENDIF
   WRITE(*,'(A)') 'Adjusting Parameter '//TRIM(PEST%PARAM(I)%PPARAM)// &
                ';ils='//TRIM(ITOS(PEST%PARAM(I)%PILS))// &
                ';izone='//TRIM(ITOS(PEST%PARAM(I)%PIZONE))// &
                ';igroup='//TRIM(ITOS(PEST%PARAM(I)%PIGROUP))// &
                ';factor='//TRIM(RTOS(FCT,'*',1))
   WRITE(IUPESTOUT,'(A)') 'Adjusting Parameter '//TRIM(PEST%PARAM(I)%PPARAM)// &
                '['//TRIM(PEST%PARAM(I)%ACRONYM)//']'// &
                ';ils='//TRIM(ITOS(PEST%PARAM(I)%PILS))// &
                ';izone='//TRIM(ITOS(PEST%PARAM(I)%PIZONE))// &
                ';igroup='//TRIM(ITOS(PEST%PARAM(I)%PIGROUP))// &
                ';factor='//TRIM(RTOS(FCT,'*',1))
  ENDIF
 ENDDO

 END SUBROUTINE IPEST_GLM_NEXTGRAD
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_GRADIENT(ITER)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER
 REAL(KIND=DP_KIND) :: DJ1,DJ2
 REAL(KIND=DP_KIND) :: TS,DF1,EIGWTHRESHOLD,W,DH1,DH2
 INTEGER :: I,II,J,K,NP,MP,IP1,NE,ISING,ITRIES,IBND
 INTEGER,ALLOCATABLE,DIMENSION(:) :: INDX
 REAL(KIND=DP_KIND) :: P1,P2,PMIN,PMAX
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: C,JS,P,PT
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: N,RU 
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: S
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: EIGV,COV,B,M
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: EIGW,JQR
 LOGICAL :: LSCALING,LSVD,LAMBDARESET

 SELECT CASE (PEST%PE_SCALING)
  CASE (0); LSCALING=.FALSE.; LSVD=.FALSE.
  CASE (1); LSCALING=.TRUE.;  LSVD=.FALSE.
  CASE (2); LSCALING=.TRUE.;  LSVD=.TRUE.
  CASE (3); LSCALING=.FALSE.; LSVD=.TRUE.
 END SELECT

 !## sensitivity
 NP=SIZE(PEST%PARAM); IF(.NOT.ALLOCATED(S)) ALLOCATE(S (NP)); S =0.0

 DO IP1=1,NP
  IF(ABS(PEST%PARAM(IP1)%PACT).NE.1.OR.PEST%PARAM(IP1)%PIGROUP.LE.0)CYCLE
  DF1=PEST%PARAM(IP1)%PDELTA
  DO J=1,MSR%NOBS
   W=MSR%W(J); DH1=MSR%DH(IP1,J); DH2=MSR%DH(0,J); S(IP1)=S(IP1)+W*((DH1-DH2)/DF1)
  ENDDO
 ENDDO
 DO I=1,NP; S(I)=S(I)/DBLE(MSR%NOBS); ENDDO

 WRITE(BLINE,'(A30)') '              Sensitivity (.):'
 DO I=1,NP
  IF(ABS(PEST%PARAM(I)%PACT).NE.1.OR.PEST%PARAM(I)%PIGROUP.LE.0)CYCLE
  WRITE(SLINE,'(E15.7)') S(I); BLINE=TRIM(BLINE)//TRIM(SLINE)
 ENDDO
 WRITE(IUPESTPROGRESS,'(A)') TRIM(BLINE) 

 TS=SUM(ABS(S)); DO IP1=1,NP
  IF(ABS(PEST%PARAM(IP1)%PACT).NE.1.OR.PEST%PARAM(IP1)%PIGROUP.LE.0)CYCLE
  IF(TS.NE.0.0D0)S(IP1)=S(IP1)/TS
 ENDDO
 S=ABS(S)*100.0D0

 WRITE(BLINE,'(A30)') '              Sensitivity (%):'
 DO I=1,NP
  IF(ABS(PEST%PARAM(I)%PACT).NE.1.OR.PEST%PARAM(I)%PIGROUP.LE.0)CYCLE
  WRITE(SLINE,'(E15.7)') S(I); BLINE=TRIM(BLINE)//TRIM(SLINE)
 ENDDO
 WRITE(IUPESTPROGRESS,'(A)') TRIM(BLINE) 
 WRITE(IUPESTSENSITIVITY,'(I10,A)') ITER,TRIM(BLINE(31:))  

 !## reset parameters - alpha(2)=previous alpha
 DO I=1,SIZE(PEST%PARAM); PEST%PARAM(I)%ALPHA(1)=PEST%PARAM(I)%ALPHA(2); ENDDO

 !## melt frozen parameters
 DO IP1=1,SIZE(PEST%PARAM)
  !## current state of the boundaries
  CALL IPEST_GLM_GETBOUNDARY(IP1,IBND,P1,P2,PMIN,PMAX)
  PEST%PARAM(IP1)%IBND=IBND
  IF(PEST%PARAM(IP1)%PACT.EQ.-1.AND.ABS(PEST%PARAM(IP1)%PIGROUP).GT.0)PEST%PARAM(IP1)%PACT=1
 ENDDO

 !## freeze-insensitive parameters
 DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PACT.EQ.1.AND.ABS(PEST%PARAM(I)%PIGROUP).GT.0.AND.S(I).LT.PEST%PE_SENS)PEST%PARAM(I)%PACT=-1
 ENDDO

 NP=0; DO I=1,SIZE(PEST%PARAM); IF(PEST%PARAM(I)%PACT.EQ.1.AND.PEST%PARAM(I)%PIGROUP.GT.0)NP=NP+1; ENDDO
 IF(NP.EQ.0)THEN; WRITE(*,'(/A/)') 'All parameters are insensitive, process stopped!'; STOP; ENDIF

 !## find until parameter update within hypersphere of parameters
 !## initiate marquardt as small as possible
 MARQUARDT=0.001D0; ITRIES=0
 DO

  ITRIES=ITRIES+1
  
  !## allocate arrays for current selection  
  NP=0; DO I=1,SIZE(PEST%PARAM); IF(PEST%PARAM(I)%PACT.EQ.1.AND.PEST%PARAM(I)%PIGROUP.GT.0)NP=NP+1; ENDDO

  IF(NP.EQ.0)THEN; WRITE(*,'(/A/)') 'No parameters left, process stopped!'; STOP; ENDIF
  
  IF(ALLOCATED(JQJ))DEALLOCATE(JQJ);   ALLOCATE(JQJ (NP,NP))
  IF(ALLOCATED(JQR))DEALLOCATE(JQR);   ALLOCATE(JQR (NP))
  IF(ALLOCATED(U  ))DEALLOCATE(U);     ALLOCATE(U   (NP))
  IF(ALLOCATED(EIGW))DEALLOCATE(EIGW); ALLOCATE(EIGW(NP))
  IF(ALLOCATED(EIGV))DEALLOCATE(EIGV); ALLOCATE(EIGV(NP,NP))
  IF(ALLOCATED(COV ))DEALLOCATE(COV);  ALLOCATE(COV (NP,NP))
  IF(LSCALING)THEN
   IF(ALLOCATED(C  ))DEALLOCATE(C);    ALLOCATE(C (NP,NP))
   IF(ALLOCATED(JS ))DEALLOCATE(JS);   ALLOCATE(JS(NP,MSR%NOBS))
  ENDIF

  !## construct jqj - normal matrix/hessian
  CALL IPEST_GLM_JQJ(JQJ,EIGW,EIGV,COV,NP,.FALSE.) 

  !## multiply lateral sensitivities with sensitivities in case pest_niter=0
  IF(PEST%PE_MXITER.EQ.0)THEN
   !## print all first time
   IF(.NOT.IPEST_GLM_WRITESTAT_PERROR(NP,COV,.TRUE.,ITER))CYCLE
  ELSE
   IF(.NOT.IPEST_GLM_WRITESTAT_PERROR(NP,COV,.FALSE.,ITER))CYCLE  
  ENDIF

  !## construct jTqr (<--- r is residual for current parameter set)
  JQR=0.0; I=0
  DO IP1=1,SIZE(PEST%PARAM)  !## row
  
   IF(PEST%PARAM(IP1)%PACT.NE.1.OR.PEST%PARAM(IP1)%PIGROUP.LE.0)CYCLE
  
   DF1=PEST%PARAM(IP1)%PDELTA
  
   I=I+1
   DO J=1,MSR%NOBS
    DH1=MSR%DH(IP1,J); DH2=MSR%DH(0,J)
    DJ1=(DH1-DH2)/DF1; DJ2=MSR%DH(0,J)
    W  =MSR%W(J)
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
   DO I=1,MSR%NOBS    !## row
    J=0
    DO IP1=1,SIZE(PEST%PARAM)
     IF(PEST%PARAM(IP1)%PACT.NE.1.OR.PEST%PARAM(IP1)%PIGROUP.LE.0)CYCLE
     DF1=PEST%PARAM(IP1)%PDELTA
     J=J+1
     DH1=MSR%DH(IP1,I); DH2=MSR%DH(0,I)
     DJ1=(DH1-DH2)/DF1
     JS(J,I)=JS(J,I)+DJ1*C(J,J)
    ENDDO
   ENDDO

   !## construct JS-Q-JS - SCALED NORMAL MATRIX
   JQJ=0.0
   DO I=1,NP     !## row
    DO J=1,NP    !## column
     DO II=1,MSR%NOBS
      DJ1=JS(I,II); DJ2=JS(J,II); W=MSR%W(II)
      JQJ(J,I)=JQJ(J,I)+(DJ1*W*DJ2)
     ENDDO
    ENDDO
   ENDDO

   !## construct jTqr (<--- r is residual for current parameter set)
   JQR=0.0; I=0
   DO IP1=1,SIZE(PEST%PARAM)  !## row
    IF(PEST%PARAM(IP1)%PACT.NE.1.OR.PEST%PARAM(IP1)%PIGROUP.LE.0)CYCLE
    I=I+1
    DO J=1,MSR%NOBS
     DJ1=JS(I,J); DJ2=MSR%DH(0,J); W=MSR%W(J)
     JQR(I)=JQR(I)+(DJ1*W*DJ2)
    ENDDO
   ENDDO

   !## add levenberg-marquardt
   DO I=1,NP; JQJ(I,I)=JQJ(I,I)+MARQUARDT*C(I,I)**2.0D0; ENDDO

  ENDIF

  !## project on important singular values
  IF(LSVD)THEN

   EIGWTHRESHOLD=0.0 !% explained variance
   DO NE=1,NP; EIGWTHRESHOLD=EIGWTHRESHOLD+EIGW(NE); IF(EIGWTHRESHOLD.GT.99.0D0)EXIT; ENDDO

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
   IF(ALLOCATED(B   ))DEALLOCATE(B);    ALLOCATE(B   (NP,NP))
   IF(NP.EQ.1)THEN
    B(1,1)=1.0D0/JQJ(1,1)
   ELSE
    CALL IPEST_LUDECOMP_DBL(JQJ,INDX,NP,ISING)
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

  !## apply scaling
  IF(LSCALING)THEN
   DO I=1,NP
    U(I)=U(I)*C(I,I)
   ENDDO
  ENDIF

  !## pointing downhill
  U=-1.0D0*U 

  !## within parameter adjust-limits
  IF(IPEST_GLM_UPGRADEVECTOR(1.0D0,.TRUE.,ITER,LAMBDARESET=LAMBDARESET))THEN 
   !## check whether number of parameters is equal to the number started this loop with
   MP=0; DO I=1,SIZE(PEST%PARAM); IF(PEST%PARAM(I)%PACT.EQ.1.AND.PEST%PARAM(I)%PIGROUP.GT.0)MP=MP+1; ENDDO
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
 CALL IPEST_GLM_JQJ(JQJ,EIGW,EIGV,COV,NP,.TRUE.)

 !## multiply lateral sensitivities with sensitivities in case pest_niter=0
 IF(.NOT.IPEST_GLM_WRITESTAT_PERROR(NP,COV,.TRUE.,ITER))THEN; ENDIF
! IF(LSENS)CALL PESTWRITESTATISTICS_FOSM(NP,COV)

 EIGWTHRESHOLD=0.0 !% explained variance
 WRITE(IUPESTOUT,'(/A10,2A15)') 'NE','EIGW(NE)','EIGWTHRESHOLD'
 DO NE=1,NP
  EIGWTHRESHOLD=EIGWTHRESHOLD+EIGW(NE); WRITE(IUPESTOUT,'(I10,2F15.7)') NE,EIGW(NE),EIGWTHRESHOLD
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

 END SUBROUTINE IPEST_GLM_GRADIENT
 
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
  IF(PEST%PARAM(IP1)%PACT.NE.1.OR.PEST%PARAM(IP1)%PIGROUP.LE.0)CYCLE
  I=I+1; U(I)=U(I)*FCT
  WRITE(IUPESTOUT,'(3I5,1X,G15.8)') IP1,PEST%PARAM(IP1)%PACT,PEST%PARAM(IP1)%PIGROUP,U(I)
 ENDDO

 !## fill in by default 
 DO IP1=1,SIZE(PEST%PARAM); PEST%PARAM(IP1)%ALPHA(1)=PEST%PARAM(IP1)%ALPHA(2); ENDDO
 
 I=0
 DO IP1=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(IP1)%PACT.EQ.1)THEN
   I=I+1; DO IP2=1,SIZE(PEST%PARAM)
    IF(PEST%PARAM(IP1)%PIGROUP.EQ.ABS(PEST%PARAM(IP2)%PIGROUP))THEN
     PEST%PARAM(IP2)%ALPHA(1)=PEST%PARAM(IP2)%ALPHA(2)+U(I)
    ENDIF
   ENDDO
  ENDIF
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
    F=PEST%PARAM(IP1)%ALPHA(1)/PEST%PARAM(IP1)%ALPHA(2)
   ENDIF 

   !## adjustment too large -causes to get another lambda
   IF(F.LT.1.0D0/PEST%PARAM(IP1)%PINCREASE.OR.F.GT.PEST%PARAM(IP1)%PINCREASE)THEN
    IF(TRIM(PEST%PARAM(IP1)%ACRONYM).EQ.'')THEN
     WRITE(IUPESTOUT,'(A,G15.8)') 'Increase Lambda - adjustment for parameter '//TRIM(ITOS(IP1))//' to small/large ',F
    ELSE
     WRITE(IUPESTOUT,'(A,G15.8)') 'Increase Lambda - adjustment for parameter '//TRIM(PEST%PARAM(IP1)%ACRONYM)//' to small/large ',F
    ENDIF
    FLUSH(IUPESTOUT)
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
     DO IP2=1,SIZE(PEST%PARAM)
      IF(PEST%PARAM(IP2)%PACT.EQ.0)CYCLE
      IF(ABS(PEST%PARAM(IP2)%PIGROUP).EQ.ABS(PEST%PARAM(IP1)%PIGROUP))PEST%PARAM(IP2)%PACT=-1
     ENDDO
     IF(TRIM(PEST%PARAM(IP1)%ACRONYM).EQ.'')THEN
      WRITE(IUPESTOUT,'(A)') 'Reset lambda - parameter '//TRIM(ITOS(IP1))//' hits boundary.'
     ELSE
      WRITE(IUPESTOUT,'(A)') 'Reset lambda - parameter '//TRIM(PEST%PARAM(IP1)%ACRONYM)//' hits boundary.'
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
      G=PEST%PARAM(IP1)%ALPHA(1)-PEST%PARAM(IP1)%ALPHA(2)
      G=G*F
      DO IP2=1,SIZE(PEST%PARAM)
       IF(PEST%PARAM(IP2)%PACT.EQ.0)CYCLE
       IF(ABS(PEST%PARAM(IP2)%PIGROUP).EQ.ABS(PEST%PARAM(IP1)%PIGROUP))THEN
        PEST%PARAM(IP2)%ALPHA(1)=PEST%PARAM(IP2)%ALPHA(2)+G
        PEST%PARAM(IP2)%PACT=-1
       ENDIF
      ENDDO
      !## ignore this parameter
      IF(TRIM(PEST%PARAM(IP1)%ACRONYM).EQ.'')THEN
       WRITE(IUPESTOUT,'(A,F10.2,A)') 'Reset lambda - parameter '//TRIM(ITOS(IP1))//' close by boundary f=',F,' snapped to it'
      ELSE
       WRITE(IUPESTOUT,'(A,F10.2,A)') 'Reset lambda - parameter '//TRIM(PEST%PARAM(IP1)%ACRONYM)//' close by boundary f=',F,' snapped to it'
      ENDIF
      FLUSH(IUPESTOUT)
      LAMBDARESET=.TRUE.; RETURN
     ENDIF
    ENDIF
   ENDIF  
  ENDDO
 
  !## corrects all gradients with this factor
  IF(F.LT.1.0D0)THEN
   
   WRITE(*,*) 'Correct update vector=',F
   
   !## adjust all parameters
   DO IP2=1,SIZE(PEST%PARAM) 
    IF(PEST%PARAM(IP2)%PACT.EQ.0)CYCLE
   
    G=PEST%PARAM(IP2)%ALPHA(1)-PEST%PARAM(IP2)%ALPHA(2)
    G=G*F

    !## update parameters
    PEST%PARAM(IP2)%ALPHA(1)=PEST%PARAM(IP2)%ALPHA(2)+G
   ENDDO

  ENDIF
 
 ENDIF
  
 !## correct update gradient found
 IPEST_GLM_UPGRADEVECTOR=.TRUE.

 J=0; DO IP1=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(IP1)%PACT.EQ.0)CYCLE

  IF(PEST%PARAM(IP1)%PLOG.EQ.1)THEN
   PEST%PARAM(IP1)%ALPHA_HISTORY(ITER)=EXP(PEST%PARAM(IP1)%ALPHA(1))
  ELSE
   PEST%PARAM(IP1)%ALPHA_HISTORY(ITER)=PEST%PARAM(IP1)%ALPHA(1)
  ENDIF
  
  !## active parameter
  IF(PEST%PARAM(IP1)%PACT.EQ.1.AND.PEST%PARAM(IP1)%PIGROUP.GT.0)THEN
   J=J+1
   !## store final gradient
   U(J)=PEST%PARAM(IP1)%ALPHA(1)-PEST%PARAM(IP1)%ALPHA(2)
  ENDIF
  
 ENDDO

 WRITE(IUPESTOUT,*) 'final ones'
 DO I=1,SIZE(PEST%PARAM)
  WRITE(IUPESTOUT,'(3I5,1x,2G15.8)') I,PEST%PARAM(I)%PACT,PEST%PARAM(I)%PIGROUP,PEST%PARAM(I)%ALPHA(1),PEST%PARAM(I)%ALPHA(2)
 ENDDO
 WRITE(IUPESTOUT,*) 
 FLUSH(IUPESTOUT)
 
 END FUNCTION IPEST_GLM_UPGRADEVECTOR
 
 !#####=================================================================
 LOGICAL FUNCTION IPEST_GLM_WRITESTAT_PERROR(NP,COV,LPRINT,ITER)
 !#####=================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: XBANDW=5.0D0
 INTEGER,INTENT(IN) :: NP,ITER
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(NP,NP) :: COV
 LOGICAL,INTENT(IN) :: LPRINT
 INTEGER :: I,J,IP1,IERROR
 REAL(KIND=DP_KIND) :: Z1,Z2,Z,ZW
 LOGICAL :: LLOG
 
 IPEST_GLM_WRITESTAT_PERROR=.FALSE.
 
 !## The asymptotic standard parameter error is a measure of how unexplained variability in the
 !## data propagates to variability in the parameters, and is essentially an error measure for the
 !## parameters. The variance indicates the range over which a parameter value could extend without affecting model fit too adversely.

 IF(LPRINT)THEN 
  WRITE(IUPESTOUT,'(/A)') 'Parameter Variance - Standard Parameter Error (standard deviation)'
  WRITE(IUPESTOUT,'(A/)')  'Indicates the range over which a parameter value could extend without affecting model fit too much'
 ENDIF
 
 J=0; IERROR=0
 DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PACT.EQ.1.AND.PEST%PARAM(I)%PIGROUP.GT.0)THEN
   J=J+1
   IF(COV(J,J).GT.0.0)THEN
    PEST%PARAM(I)%ALPHA_ERROR_VARIANCE(ITER)=SQRT(COV(J,J))
   ELSE
    !## error value - should not happen
    PEST%PARAM(I)%ALPHA_ERROR_VARIANCE(ITER)=-999.99D0 
    IERROR=IERROR+1
   ENDIF
   !## check whether current other parameters belong to this group
   DO IP1=1,SIZE(PEST%PARAM)
    !## active and follower of group
    IF(PEST%PARAM(IP1)%PACT.EQ.1.AND.PEST%PARAM(IP1)%PIGROUP.LT.0)THEN
     IF(ABS(PEST%PARAM(IP1)%PIGROUP).EQ.PEST%PARAM(I)%PIGROUP)THEN
      PEST%PARAM(IP1)%ALPHA_ERROR_VARIANCE(ITER)=PEST%PARAM(I)%ALPHA_ERROR_VARIANCE(ITER)
     ENDIF
    ENDIF
   ENDDO
  ELSEIF(PEST%PARAM(I)%PIGROUP.EQ.0)THEN
   PEST%PARAM(I)%ALPHA_ERROR_VARIANCE(ITER)=0.0D0
  ENDIF
 ENDDO
 
 IF(IERROR.GT.0)THEN
  WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'Errors ('//TRIM(ITOS(IERROR))//') found in the Covariance Matrix:'; WRITE(IUPESTOUT,*)
  WRITE(*,'(/A/)') 'Errors found in the computation of the Covariance Matrix'; STOP
 ENDIF
 
 IF(LPRINT)THEN; WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'Confidence Limits (96%):'; WRITE(IUPESTOUT,*); ENDIF

 DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PIGROUP.LT.0)CYCLE
  IF(PEST%PARAM(I)%PACT.EQ.1.AND.PEST%PARAM(I)%PIGROUP.GT.0)THEN
   ZW=PEST%PARAM(I)%ALPHA_ERROR_VARIANCE(ITER)*1.96D0
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

   IF(LPRINT)THEN
    WRITE(BLINE,'(3G15.7)') Z1,Z,Z2
    IF(PEST%PARAM(I)%ACRONYM.EQ.'')THEN
     WRITE(IUPESTOUT,'(A2,2I5.5,I3.3,A)') PEST%PARAM(I)%PPARAM,PEST%PARAM(I)%PILS,PEST%PARAM(I)%PIZONE,ABS(PEST%PARAM(I)%PIGROUP),TRIM(BLINE)
    ELSE
     WRITE(IUPESTOUT,'(A15,A)') PEST%PARAM(I)%ACRONYM,TRIM(BLINE)
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
    WRITE(IUPESTOUT,'(/3X,A2,2I5.5,A1,I3.3)') PEST%PARAM(I)%PPARAM,PEST%PARAM(I)%PILS,PEST%PARAM(I)%PIZONE,'-',ABS(PEST%PARAM(I)%PIGROUP)
    WRITE(IUPESTOUT,'(A,2G15.7)') 'This parameter too unreliable to estimate: ',Z1,Z2
    WRITE(IUPESTOUT,'(A/)') 'Parameter will be turned off for this cycle'
    PEST%PARAM(I)%PACT=-1; RETURN
   ENDIF
   
  ELSE
   IF(LPRINT.AND.PEST%PARAM(I)%PACT.EQ.-1)THEN
    WRITE(BLINE,'(3A15)') 'Insens.','Insens.','Insens.'
    IF(PEST%PARAM(I)%ACRONYM.EQ.'')THEN
     WRITE(IUPESTOUT,'(A2,2I5.5,I3.3,A)') PEST%PARAM(I)%PPARAM,PEST%PARAM(I)%PILS,PEST%PARAM(I)%PIZONE,ABS(PEST%PARAM(I)%PIGROUP),TRIM(BLINE)
    ELSE
     WRITE(IUPESTOUT,'(A15,A)') PEST%PARAM(I)%ACRONYM,TRIM(BLINE)
    ENDIF
   ENDIF
  ENDIF
 ENDDO

 IPEST_GLM_WRITESTAT_PERROR=.TRUE.
 
 END FUNCTION IPEST_GLM_WRITESTAT_PERROR
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_ECHOPARAMETERS(GUPDATE,ITER)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER
 REAL(KIND=DP_KIND),INTENT(OUT) :: GUPDATE
 INTEGER :: IP1,N,I
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: GRADUPDATE
 
 WRITE(IUPESTOUT,'(/A)') 'Upgrade Vector Parameter History:'
 WRITE(BLINE,'(A19,99(A7,I3.3))') 'Parameter',('   ITER',I,I=ITER,0,-1)
 WRITE(IUPESTOUT,'(A)') TRIM(BLINE)
  
 ALLOCATE(GRADUPDATE(ITER)); GRADUPDATE=0.0D0
 N=0
 DO IP1=1,SIZE(PEST%PARAM)

  WRITE(BLINE,'(99(F10.5))') (PEST%PARAM(IP1)%ALPHA_HISTORY(I),I=ITER,0,-1)
  
  IF(ABS(PEST%PARAM(IP1)%PACT).EQ.1.AND.PEST%PARAM(IP1)%PIGROUP.GT.0)THEN
   IF(PEST%PARAM(IP1)%ACRONYM.EQ.'')THEN
    WRITE(IUPESTOUT,'(3X,A2,2I5.5,A1,I3.3,A)') PEST%PARAM(IP1)%PPARAM,PEST%PARAM(IP1)%PILS,PEST%PARAM(IP1)%PIZONE,'-',ABS(PEST%PARAM(IP1)%PIGROUP),TRIM(BLINE)
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
 WRITE(BLINE,'(19X,99E10.3)') (GRADUPDATE(I),I=ITER,1,-1)
 WRITE(IUPESTOUT,'(A)') TRIM(BLINE)

 GUPDATE=GRADUPDATE(ITER)

 DEALLOCATE(GRADUPDATE)

 END SUBROUTINE IPEST_GLM_ECHOPARAMETERS
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_JQJ(JQJ,EIGW,EIGV,COV,NP,LPRINT) 
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NP
 LOGICAL,INTENT(IN) :: LPRINT
 REAL(KIND=DP_KIND),DIMENSION(NP,NP),INTENT(OUT) :: JQJ,EIGV,COV
 REAL(KIND=DP_KIND),DIMENSION(NP),INTENT(OUT) :: EIGW
 REAL(KIND=DP_KIND) :: DET
 INTEGER :: I,J,K,IP1,IP2,II,N,ISING,IIU !,JUPESTOUT
 REAL(KIND=DP_KIND) :: DF1,DF2,DJ1,DJ2,B1,TV,TEV,CB,KAPPA,W,DH1,DH2
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: B,JQJB
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: COR
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: E
 INTEGER,ALLOCATABLE,DIMENSION(:) :: INDX
 
 !!## if sensisivities need to be computed generate csv file and stop
 !IF(LSENS)THEN
 ! CALL PESTOPENFILE(iu,'log_jacobian_','txt',0,root)
 ! WRITE(IU,*) 'POSITIVE numbers means that an INcreasement of the parameter raises the head'
 ! WRITE(IU,*) 'NEGATIVE numbers means that an DEcreasement of the parameter raises the head'
 ! BLINE=''; M=0
 ! DO IP1=1,SIZE(PARAM)
 !  IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
 !  WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3,A1)') PARAM(IP1)%PPARAM,PARAM(IP1)%ILS,PARAM(IP1)%IZONE,'-',PARAM(IP1)%IGROUP,','
 !  BLINE=TRIM(BLINE)//TRIM(SLINE); M=M+1
 ! ENDDO
 ! WRITE(IU,'(4A11,A32,A)') 'X,','Y,','ILAY,','WEIGTH,','LABEL,',TRIM(BLINE)
 ! BLINE=''
 ! DO IP1=1,SIZE(PARAM)
 !  IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
 !  WRITE(SLINE,'(A15,A1)') PARAM(IP1)%ACRONYM
 !  BLINE=TRIM(BLINE)//TRIM(SLINE)
 ! ENDDO
 ! WRITE(IU,'(76X,A)') TRIM(BLINE)
 !
 ! JQJ=0.0D0
 ! DO I=1,PEST_NOBS
 !  N=0
 !  DO IP1=1,SIZE(PARAM)               
 !   IF(PARAM(IP1)%IACT.NE.1.OR.PARAM(IP1)%IGROUP.LE.0)CYCLE
 !   DF1=REAL(PARAM(IP1)%DELTA,8)
 !   DH1=REAL(MSR%DH(IP1,I),8)
 !   DH2=REAL(MSR%DH(0,I),8)
 !   DJ1=(DH1-DH2)/DF1
 !   N=N+1
 !   JQJ(N,1)=DJ1
 !   IF(M.GT.1)JQJ(N,2)=JQJ(N,2)+ABS(JQJ(N,1))
 !  ENDDO
 !  WRITE(IU,'(2(F10.2,A1),I10,A1,F10.2,A,A32,999(G15.7,A1))') MSR%X(I),',',MSR%Y(I),',',MSR%L(I),',', &
 !          MSR%W(I),',',TRIM(MSR%CLABEL(I))//',',(JQJ(J,1),',',J=1,N)
 ! ENDDO
 ! IF(M.EQ.1)THEN; WRITE(IU,'(/44X,A32,999(G15.7,A1))') 'TOTAL,',(ABS(JQJ(J,1)),',',J=1,N)
 ! ELSE; WRITE(IU,'(/44X,A32,999(G15.7,A1))') 'TOTAL,',(JQJ(J,2),',',J=1,N); ENDIF
 !ENDIF
 
 !## save msr%dh() vector per parameter as csv ... to be read in again
 
 !## construct jqj - NORMAL MATRIX/HESSIAN
 JQJ=0.0; I=0
 DO IP1=1,SIZE(PEST%PARAM)                !## row
  IF(PEST%PARAM(IP1)%PACT.NE.1.OR.PEST%PARAM(IP1)%PIGROUP.LE.0)CYCLE
  DF1=PEST%PARAM(IP1)%PDELTA
  I=I+1; II=0; DO IP2=1,SIZE(PEST%PARAM)  !## column
   IF(PEST%PARAM(IP2)%PACT.NE.1.OR.PEST%PARAM(IP2)%PIGROUP.LE.0)CYCLE
   DF2=PEST%PARAM(IP2)%PDELTA
   II=II+1
   DO J=1,MSR%NOBS
    DH1=MSR%DH(IP1,J); DH2=MSR%DH(0,J)
    DJ1=(DH1-DH2)/DF1; DH1=MSR%DH(IP2,J)
    DJ2=(DH1-DH2)/DF2; W=MSR%W(J)
    JQJ(II,I)=JQJ(II,I)+(DJ1*W*DJ2)  
   ENDDO
  ENDDO
 ENDDO

! !## construct covariance on the pilotpoints
! IF(PEST_IREGULARISATION.EQ.1)THEN
!  CALL PEST_GETQPP(NP,.FALSE.,idf)
!  JQJ=JQJ+QPP
! ENDIF
  
 IF(ALLOCATED(E   ))DEALLOCATE(E);    ALLOCATE(E   (NP))
 IF(ALLOCATED(COR ))DEALLOCATE(COR);  ALLOCATE(COR(NP,NP))
 IF(ALLOCATED(INDX))DEALLOCATE(INDX); ALLOCATE(INDX(NP))
 IF(ALLOCATED(B   ))DEALLOCATE(B);    ALLOCATE(B(NP,NP))
 IF(ALLOCATED(JQJB))DEALLOCATE(JQJB); ALLOCATE(JQJB(NP,NP))
 
 !## copy jqj to jqjb
 JQJB=JQJ
 !## copy jqj to b
 B=JQJB
 
 !## compute determinant of JQJ
 DET=IPEST_GLM_DET(JQJB,NP)

 IF(LPRINT)THEN
  WRITE(IUPESTOUT,'(/A15,E15.7)') 'Determinant JQJ = ',DET
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
  WRITE(*,'(/A/)') 'Warning, there is NO information (no eigenvalues) in parameter perturbation'; STOP
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
 CALL IPEST_LUDECOMP_DBL(JQJB,INDX,NP,ISING)
 B=0.0D0; DO I=1,NP; B(I,I)=1.0D0; ENDDO
 DO I=1,NP; CALL IPEST_LUBACKSUB_DBL(JQJB,INDX,B(1,I),NP); ENDDO
 
 !## parameter covariance matrix
  
 N=MAX(1,MSR%NOBS-NP); B1=MSR%TJ/REAL(N,8)

 DO I=1,NP; DO J=1,NP; B(I,J)=B1*B(I,J); ENDDO; ENDDO

 IF(LPRINT)THEN
  DO K=1,2
   IF(K.EQ.1)IIU=IUPESTOUT 
!   IF(K.EQ.2)THEN
!    !## write covariance 
!    IF(.NOT.LSENS)EXIT
!    JUPESTOUT=UTL_GETUNIT()
!    OPEN(JUPESTOUT,FILE=TRIM(ROOT)//CHAR(92)//'PEST'//CHAR(92)//'COVARIANCE_'//TRIM(ITOS(PEST_NITER))//'.TXT',STATUS='UNKNOWN',ACTION='WRITE')
!    IIU=JUPESTOUT 
!    WRITE(IIU,*) NP
!    DO J=1,SIZE(PEST%PARAM)
!     IF(PEST%PARAM(J)%PACT.EQ.1.AND.PEST%PARAM(J)%PIGROUP.GT.0)THEN
!      WRITE(IIU,'(A2,3I3,A16)') PEST%PARAM(J)%PPARAM,PEST%PARAM(J)%PILS,PEST%PARAM(J)%PIZONE,PEST%PARAM(J)%PIGROUP,PEST%PARAM(J)%ACRONYM
!     ENDIF
!    ENDDO
!   ENDIF
   WRITE(IIU,*); WRITE(IIU,*) 'Parameter Covariance Matrix (m2):'; WRITE(IUPESTOUT,*)
 
   BLINE=''
   DO J=1,SIZE(PEST%PARAM)
    IF(PEST%PARAM(J)%PACT.EQ.1.AND.PEST%PARAM(J)%PIGROUP.GT.0)THEN
     IF(PEST%PARAM(J)%ACRONYM.EQ.'')THEN
      WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PEST%PARAM(J)%PPARAM,PEST%PARAM(J)%PILS,PEST%PARAM(J)%PIZONE,'-',PEST%PARAM(J)%PIGROUP
     ELSE
      WRITE(SLINE,'(A15)') PEST%PARAM(J)%ACRONYM
     ENDIF
     BLINE=TRIM(BLINE)//TRIM(SLINE)
    ENDIF
   ENDDO
   WRITE(IIU,'(15X,A)') TRIM(BLINE) 
   WRITE(IIU,'(A)')
  
  ENDDO
 ENDIF

 I=0
 DO IP1=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(IP1)%PACT.EQ.1.AND.PEST%PARAM(IP1)%PIGROUP.GT.0)THEN
   I=I+1
   IF(LPRINT)THEN
    IF(PEST%PARAM(IP1)%ACRONYM.EQ.'')THEN
     WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PEST%PARAM(IP1)%PPARAM,PEST%PARAM(IP1)%PILS,PEST%PARAM(IP1)%PIZONE,'-',PEST%PARAM(IP1)%PIGROUP
    ELSE
     WRITE(SLINE,'(A15)') PEST%PARAM(IP1)%ACRONYM
    ENDIF
!    DO K=1,2
!     IF(K.EQ.1)
    IIU=IUPESTOUT 
!     IF(K.EQ.2)IIU=JUPESTOUT 
     WRITE(IIU,'(A15,999E15.7)') TRIM(SLINE),(B(I,J),J=1,NP)
!     IF(.NOT.LSENS)EXIT
!    ENDDO
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
  DO J=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(J)%PACT.EQ.1.AND.PEST%PARAM(J)%PIGROUP.GT.0)THEN
    IF(PEST%PARAM(J)%ACRONYM.EQ.'')THEN
     WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PEST%PARAM(J)%PPARAM,PEST%PARAM(J)%PILS,PEST%PARAM(J)%PIZONE,'-',PEST%PARAM(J)%PIGROUP
    ELSE
     WRITE(SLINE,'(A15)') PEST%PARAM(J)%ACRONYM
    ENDIF
    BLINE=TRIM(BLINE)//TRIM(SLINE)
   ENDIF
  ENDDO
  WRITE(IUPESTOUT,'(15X,A)') TRIM(BLINE) 
  WRITE(IUPESTOUT,'(A)')

  COR=0.0D0; I=0
  DO IP1=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(IP1)%PACT.EQ.1.AND.PEST%PARAM(IP1)%PIGROUP.GT.0)THEN
    I=I+1
    DO J=1,NP
     CB=B(I,I)*B(J,J)
     IF(CB.GT.0.0D0)THEN
      COR(I,J)=B(I,J)/SQRT(CB)
     ELSE
      COR(I,J)=0.0D0
     ENDIF
    ENDDO
    IF(PEST%PARAM(IP1)%ACRONYM.EQ.'')THEN
     WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PEST%PARAM(IP1)%PPARAM,PEST%PARAM(IP1)%PILS,PEST%PARAM(IP1)%PIZONE,'-',PEST%PARAM(IP1)%PIGROUP
    ELSE
     WRITE(SLINE,'(A15)') PEST%PARAM(IP1)%ACRONYM
    ENDIF
    WRITE(IUPESTOUT,'(A15,999F15.7)') TRIM(SLINE),(COR(I,J),J=1,NP)
   ENDIF
  ENDDO

  !## write per parameter highly correlated other parameter
  WRITE(IUPESTOUT,*); WRITE(IUPESTOUT,*) 'Parameter Correlated to (correlation > 0.95):'; WRITE(IUPESTOUT,*)
  IP1=0
  DO I=1,SIZE(PEST%PARAM)
   IF(PEST%PARAM(I)%PACT.EQ.1.AND.PEST%PARAM(I)%PIGROUP.GT.0)THEN
    IF(PEST%PARAM(I)%ACRONYM.EQ.'')THEN
     WRITE(BLINE,'(3X,A2,2I3.3,A1,I3.3)') PEST%PARAM(I)%PPARAM,PEST%PARAM(I)%PILS,PEST%PARAM(I)%PIZONE,'-',PEST%PARAM(I)%PIGROUP
    ELSE
     WRITE(BLINE,'(A15)') PEST%PARAM(I)%ACRONYM
    ENDIF
    IP1=IP1+1
    IP2=0
    DO J=1,SIZE(PEST%PARAM)
     IF(PEST%PARAM(J)%PACT.EQ.1.AND.PEST%PARAM(J)%PIGROUP.GT.0)THEN
      IP2=IP2+1
      IF(I.NE.J)THEN
       IF(PEST%PARAM(J)%ACRONYM.EQ.'')THEN
        WRITE(SLINE,'(3X,A2,2I3.3,A1,I3.3)') PEST%PARAM(J)%PPARAM,PEST%PARAM(J)%PILS,PEST%PARAM(J)%PIZONE,'-',PEST%PARAM(J)%PIGROUP
       ELSE
        WRITE(SLINE,'(A15)') PEST%PARAM(J)%ACRONYM
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
 
 END SUBROUTINE IPEST_GLM_JQJ
 
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
 
 !###========================================================================
 DOUBLE PRECISION FUNCTION IPEST_GLM_DET(JQJ,N)
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
    IPEST_GLM_DET = 0.0
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
 SUBROUTINE IPEST_GLM_GETJ(DIR,IGRAD)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER,INTENT(IN) :: IGRAD
 INTEGER :: I,J,II,NC,NP,MX,NPERIOD,III,K,KK,ILAY,NROWIPFTXT,IUIPFTXT,NCOLIPFTXT,IOS,NAJ
 REAL(KIND=DP_KIND) :: X,Y,Z,H,WW,MC,MM,DHH,XCOR,YCOR,ZCOR,XCROSS,DHW,RFIT
 CHARACTER(LEN=256) :: DIRNAME,FNAME
 CHARACTER(LEN=52) :: CID,TXT
 CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:) :: CEXT
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: TSNODATA,M,C,MCOPY,CCOPY
 INTEGER(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: IDATE,ID1,ID2
 REAL(KIND=DP_KIND),DIMENSION(2) :: PC,PM,DYN !## percentiles computed/measured
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IU,NR,IEXT

 IF(.NOT.ASSOCIATED(PEST%MEASURES))RETURN
 
 DIRNAME=TRIM(DIR)//'\IPEST_P#'//TRIM(ITOS(IGRAD))//'\TIMESERIES'

 WRITE(*,'(A)') 'Getting residual for P#'//TRIM(ITOS(IGRAD))//' ...'
 
 !## open files and get total number of observations
 MX=0; NC=SIZE(PEST%MEASURES); ALLOCATE(IU(NC),NR(NC),IEXT(NC),CEXT(NC))
 DO I=1,SIZE(PEST%MEASURES)
  J=INDEX(PEST%MEASURES(I)%IPFNAME,'\',.TRUE.)+1
  FNAME=TRIM(DIRNAME)//'\'//TRIM(PEST%MEASURES(I)%IPFNAME(J:))
  IU(I)=UTL_GETUNIT(); OPEN(IU(I),FILE=FNAME,STATUS='OLD',ACTION='READ')
  READ(IU(I),*) NR(I); MX=MX+NR(I)
  READ(IU(I),*) NC
  DO J=1,NC; READ(IU(I),*); ENDDO; READ(IU(I),*) IEXT(I),CEXT(I)
 ENDDO

 !## allocate memory
 CALL IPEST_GLM_ALLOCATEMSR(MX,IGRAD)

 !## initialise head-differences
 IF(IGRAD.EQ.0)THEN
  DO I=1,SIZE(MSR%DH,2); MSR%DH(IGRAD,I)=0.0D0; ENDDO
 ELSE
  !## zero gradient in case parameter is fixed
  DO I=1,SIZE(MSR%DH,2); MSR%DH(IGRAD,I)=MSR%DH(0,I); ENDDO
 ENDIF
 
 WRITE(IUPESTRESIDUAL,'(I10,A)') I,','//TRIM(FNAME)
 !## steady-state
 IF(IEXT(1).EQ.0)THEN
  WRITE(IUPESTRESIDUAL,'(2A16,A11,6A16,A11,A32)') 'X,','Y,','ILAY,','MSR,','MDL,', &
         'J,','WMDL,','WRESIDUAL,','WEIGH,','IPF,','LABEL'
 !## transient
 ELSE  
  WRITE(IUPESTRESIDUAL,'(2A16,A11,8A16,A11,A32,1X,A15)') 'X,','Y,','ILAY,','WEIGH,','MSR,','MDL,','MDL-MSR,', &
         'DYNMSR,','DYNMDL,','DYNMSR-DYNMDL,','CROSS-COR,','IPF,','LABEL','DATE'
 ENDIF

 !## initialise variables
 II=0; MSR%TJ=0.0D0

 NPERIOD=0; IF(ASSOCIATED(PEST%S_PERIOD))NPERIOD=SIZE(PEST%S_PERIOD)
 IF(NPERIOD.GT.0)THEN
  ALLOCATE(ID1(NPERIOD),ID2(NPERIOD))
  DO I=1,NPERIOD
   READ(PEST%S_PERIOD(I),*) ID1
   READ(PEST%E_PERIOD(I),*) ID2
  ENDDO
 ENDIF
 
 !## process files
 DO I=1,SIZE(PEST%MEASURES)

  IF(IEXT(I).EQ.0)THEN

   DO J=1,NR(I)
   
    II=II+1
    READ(IU(I),*) X,Y,ILAY,Z,WW,H
    !## weigh=1/sqrt(variance)
    MSR%W(II)=WW
    IF(PEST%MEASURES(I)%IVCOL.GT.0)THEN
     IF(WW.LT.0.0D0)THEN; WW=0.0D0; ELSE; MSR%W(II)=1.0D0/SQRT(WW); ENDIF
    ENDIF
    
    !## calculated - measured
    DHH=0.0D0; IF(ABS(H-Z).GT.PEST%PE_DRES)DHH=H-Z; MSR%DH(IGRAD,II)=DHH  
    !## save information for measurement
    MSR%X(II)=X; MSR%Y(II)=Y; MSR%L(II)=ILAY
    MSR%CLABEL(II)='MEASURE'//TRIM(ITOS(J))//'_IPF'//TRIM(ITOS(I))

    GF_H(II)=MSR%W(II)*H; GF_O(II)=MSR%W(II)*Z

    !## add to total objective function
    DHW=MSR%W(II)*(DHH**2.0D0); MSR%TJ=MSR%TJ+DHW
    WRITE(IUPESTRESIDUAL,'(2(G15.7,1X),I10,1X,6(G15.7,1X),I10,1X,A32)') &
        X,Y,ILAY,Z,H,DHW,MSR%W(II)*H,MSR%W(II)*(H-Z),MSR%W(II),I,MSR%CLABEL(II)

   ENDDO
  
  !## transient
  ELSE
    
   XCROSS=0.0D0
   DO J=1,NR(I)

    READ(IU(I),*) X,Y,ILAY,CID,WW   
    !## weigh=1/stdev=1/sqrt(variance)
    IF(PEST%MEASURES(I)%IVCOL.GT.0)THEN
     IF(WW.LE.0.0D0)THEN; WW=0.0D0; ELSE; WW=1.0D0/SQRT(WW); ENDIF
    ENDIF

    LINE=TRIM(DIRNAME)//CHAR(92)//TRIM(CID)//'.'//TRIM(CEXT(I))
    IUIPFTXT=UTL_GETUNIT(); OPEN(IUIPFTXT,FILE=LINE,STATUS='OLD',ACTION='READ')
    
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

     !## make double precision dates - if needed
     IF(IDATE(KK).LT.100000000)IDATE(KK)=IDATE(KK)*1000000

     !## error reading, skip it (can be caused by steady-state periods in between)
     IF(IOS.NE.0)THEN; KK=KK-1; CYCLE; ENDIF

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

     !## mean values
     MM=SUM(M(1:KK))/REAL(KK) !## measurements
     MC=SUM(C(1:KK))/REAL(KK) !## computed
     DO K=1,KK; MCOPY(K)=M(K); CCOPY(K)=C(K); ENDDO
     !## percentiles
     CALL UTL_GETMED(MCOPY,KK,-999.99D0,(/10.0D0,90.0D0/),2,NAJ,PM)
     CALL UTL_GETMED(CCOPY,KK,-999.99D0,(/10.0D0,90.0D0/),2,NAJ,PC)
     !## measurements
     DYN(1)=PM(2)-PM(1) 
     !## computed
     DYN(2)=PC(2)-PC(1) 
     !## compute cross-correlation
     IF(KK.GT.1)THEN
      XCOR=0.0D0; YCOR=0.0D0; ZCOR=0.0D0
      DO K=1,KK
       XCOR=XCOR+(MM-M(K))*(MC-C(K)); YCOR=YCOR+(MM-M(K))**2.0D0; ZCOR=ZCOR+(MC-C(K))**2.0D0
      ENDDO
      IF(YCOR.NE.0.0.AND.ZCOR.NE.0.0)XCOR=XCOR/(SQRT(YCOR)*SQRT(ZCOR))
      XCROSS=XCROSS+XCOR
     ENDIF

     !## add observation
     DO K=1,KK
      II =II+1
      DHH=0.0D0

      !## target is residual (calculated minus measured)
      IF(ABS(C(K)-M(K)).GT.PEST%PE_DRES)DHH=DHH+PEST%PE_TARGET(1)*(C(K)-M(K)) 

      !## target is dynamics (calculated minus measured)
      IF(ABS(DYN(2)-DYN(1)).GT.PEST%PE_DRES)DHH=DHH+PEST%PE_TARGET(2)*(DYN(2)-DYN(1))

      !## calculated - measured
      MSR%DH(IGRAD,II)=DHH       

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

      WRITE(IUPESTRESIDUAL,'(2(G15.7,1X),I10,1X,8(G15.7,1X),I10,1X,A32,1X,I15)') &
         X,Y,ILAY,WW,M(K),C(K),C(K)-M(K),DYN(1),DYN(2),DYN(2)-DYN(1),XCOR,I,MSR%CLABEL(II),IDATE(K)

      IF(PEST%PE_TARGET(1).EQ.0.0D0.AND.PEST%PE_TARGET(2).GT.0.0D0)EXIT
     
     ENDDO
     
    ENDIF
    
    DEALLOCATE(TSNODATA,C,M,MCOPY,CCOPY,IDATE)
    CLOSE(IUIPFTXT)

   ENDDO
  ENDIF

  CLOSE(IU(I))

  IF(NR(I).GT.0)THEN
   IF(IEXT(I).GT.0)WRITE(IUPESTOUT,'(/A/)') 'MEAN Cross-Correlation: '// &
          TRIM(RTOS(REAL(XCROSS)/DBLE(NR(I)),'F',7))//' (n='//TRIM(ITOS(NR(I)))//')'
  ENDIF

 ENDDO
 MSR%NOBS=II
 
 IF(MSR%NOBS.LE.0)THEN; WRITE(*,'(/A/)') 'No measurements available within current spatial/temporal space.'; STOP; ENDIF
 
! !## run batch files
! CALL PEST_BATCHFILES()
!
 !## insert regularisation to objective function
 NP=0; DO I=1,SIZE(PEST%PARAM); IF(PEST%PARAM(I)%PACT.EQ.0.OR.PEST%PARAM(I)%PIGROUP.LE.0)CYCLE; NP=NP+1; ENDDO
 
 MSR%RJ=0.0D0
! IF(PEST_IREGULARISATION.EQ.1)CALL PEST_GETQPP(NP,.TRUE.,idf)
  
 IF(IGRAD.EQ.0)THEN
  WRITE(IUPESTOUT,'(/A)') 'Best Match Value   : '//TRIM(RTOS(MSR%TJ,'G',7))
  WRITE(IUPESTOUT,'(A/)') 'Plausibility Value : '//TRIM(RTOS(MSR%RJ,'G',7))
 ENDIF
 
 MSR%TJ=MSR%TJ+MSR%RJ
  
 IF(IGRAD.EQ.0)THEN
  WRITE(IUPESTOUT,'(/A)') 'TOTAL Objective Function Value : '//TRIM(RTOS(MSR%TJ,'G',7))
  WRITE(IUPESTOUT,'( A)') 'MEAN Objective Function Value  : '//TRIM(RTOS(MSR%TJ/REAL(MSR%NOBS,8),'G',7))//' (n='//TRIM(ITOS(MSR%NOBS))//')'
          
  RFIT=IPEST_GOODNESS_OF_FIT(GF_H,GF_O,MSR%NOBS)
  WRITE(IUPESTOUT,'( A)') 'Goodness of Fit                : '//TRIM(RTOS(RFIT,'G',7))//' (n='//TRIM(ITOS(MSR%NOBS))//')'
  WRITE(IUPESTOUT,'( A)') '>> Provides a measure of the extent to which variability of field measurements is explained'
  WRITE(IUPESTOUT,'(A/)') '   by the calibrated model compared to that which can be constructed as purely random. <<'
 ENDIF

! IF(LGRAD)THEN
!  IF(IGRAD.EQ.0)THEN
!   IF(ITER.EQ.1)WRITE(IUPESTEFFICIENCY,'(3E15.7)') MSR%TJ,SQRT(MSR%TJ),MSR%TJ/REAL(MSR%NOBS,8)
!  ELSE
!   PEST%PARAM(IGRAD)%TJOBJ=TJ
!  ENDIF
! ENDIF
! IF(LLNSRCH)THEN
! ENDIF
 
 WRITE(*,'(A)') '>>> Finished <<<'

 END SUBROUTINE IPEST_GLM_GETJ

 !#####=================================================================
 SUBROUTINE IPEST_GLM_PROGRESS(ITER,IGRAD,ILNSRCH)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER,IGRAD,ILNSRCH
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: X
 INTEGER :: I,J
 
 ALLOCATE(X(SIZE(PEST%PARAM)))

 DO I=1,SIZE(PEST%PARAM)
  IF(PEST%PARAM(I)%PLOG)THEN
   X(I)=EXP(PEST%PARAM(I)%ALPHA(1))
  ELSE
   X(I)=PEST%PARAM(I)%ALPHA(1)
  ENDIF
 ENDDO

 IF(IGRAD.EQ.0)THEN
  WRITE(BLINE,'(3A5,A15)') 'IT','GD','LS','TOT_J'
  DO J=1,SIZE(PEST%PARAM)
   IF(ABS(PEST%PARAM(J)%PACT).EQ.1.AND.PEST%PARAM(J)%PIGROUP.GT.0)THEN
    WRITE(SLINE,'(3X,A2,2I5.5)') PEST%PARAM(J)%PPARAM,PEST%PARAM(J)%PILS,PEST%PARAM(J)%PIZONE
    BLINE=TRIM(BLINE)//TRIM(SLINE)
   ENDIF
  ENDDO
  WRITE(IUPESTPROGRESS,'(/A)') TRIM(BLINE) 
  BLINE=''
  DO J=1,SIZE(PEST%PARAM)
   IF(ABS(PEST%PARAM(J)%PACT).EQ.1.AND.PEST%PARAM(J)%PIGROUP.GT.0)THEN
    WRITE(SLINE,'(12X,I3.3)') PEST%PARAM(J)%PIGROUP
    BLINE=TRIM(BLINE)//TRIM(SLINE)
   ENDIF
  ENDDO
  WRITE(IUPESTPROGRESS,'(30X,A)') TRIM(BLINE) 
  BLINE=''
  DO J=1,SIZE(PEST%PARAM)
   IF(ABS(PEST%PARAM(J)%PACT).EQ.1.AND.PEST%PARAM(J)%PIGROUP.GT.0)THEN
    WRITE(SLINE,'(A15)') PEST%PARAM(J)%ACRONYM
    BLINE=TRIM(BLINE)//TRIM(SLINE)
   ENDIF
  ENDDO
  WRITE(IUPESTPROGRESS,'(30X,A)') TRIM(BLINE) 
 ENDIF
 
 WRITE(BLINE,'(3I5,E15.7)') ITER,IGRAD,ILNSRCH,MSR%TJ
 DO I=1,SIZE(PEST%PARAM)
  IF(ABS(PEST%PARAM(I)%PACT).EQ.1.AND.PEST%PARAM(I)%PIGROUP.GT.0)THEN
   WRITE(SLINE,'(E15.7)') X(I)
   BLINE=TRIM(BLINE)//TRIM(SLINE)
  ENDIF
 ENDDO

 WRITE(IUPESTPROGRESS,'(A)') TRIM(BLINE) 
 DEALLOCATE(X)

 END SUBROUTINE IPEST_GLM_PROGRESS
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_ALLOCATEMSR(N,IGRAD)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,IGRAD
 INTEGER :: M
 
 !## only one value per measurement
 M=N*SIZE(SIM)
 
 CALL IPEST_GLM_DEALLOCATEMSR(IGRAD)
 IF(IGRAD.EQ.0)ALLOCATE(MSR%DH(0:SIZE(PEST%PARAM),M))
 ALLOCATE(MSR%W (M)) 
 ALLOCATE(MSR%X (M)) 
 ALLOCATE(MSR%Y (M)) 
 ALLOCATE(MSR%L (M)) 
 ALLOCATE(MSR%CLABEL(M)) 
 ALLOCATE(GF_H(M))
 ALLOCATE(GF_O(M))

 END SUBROUTINE IPEST_GLM_ALLOCATEMSR
 
 !###====================================================================
 SUBROUTINE IPEST_GLM_DEALLOCATEMSR(IGRAD)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IGRAD

 !## reset for next cycle
 IF(IGRAD.EQ.0)THEN; IF(ASSOCIATED(MSR%DH))DEALLOCATE(MSR%DH); ENDIF
 IF(ASSOCIATED(MSR%W ))    DEALLOCATE(MSR%W) 
 IF(ASSOCIATED(MSR%X ))    DEALLOCATE(MSR%X) 
 IF(ASSOCIATED(MSR%Y ))    DEALLOCATE(MSR%Y) 
 IF(ASSOCIATED(MSR%L ))    DEALLOCATE(MSR%L) 
 IF(ASSOCIATED(MSR%CLABEL))DEALLOCATE(MSR%CLABEL) 
 IF(ALLOCATED(GF_H))       DEALLOCATE(GF_H)
 IF(ALLOCATED(GF_O))       DEALLOCATE(GF_O)
 
 END SUBROUTINE IPEST_GLM_DEALLOCATEMSR
 
 !###====================================================================
 REAL FUNCTION IPEST_GOODNESS_OF_FIT(X,Y,N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=8),INTENT(IN),DIMENSION(N) :: X,Y !## x=head; y=obs
 REAL(KIND=8) :: XN,YN,YA
 INTEGER :: I
 
 !## compute nash-sutcliff
 IPEST_GOODNESS_OF_FIT=0.0D0
 
 !## average observation
 YA=0.0D0; DO I=1,N; YA=YA+Y(I)          ; ENDDO; YA=YA/REAL(N)
 XN=0.0D0; DO I=1,N; XN=XN+ABS(Y(I)-X(I)); ENDDO; XN=XN**2.0D0
 YN=0.0D0; DO I=1,N; YN=YN+ABS(Y(I)-YA)  ; ENDDO; YN=YN**2.0D0
 
 IPEST_GOODNESS_OF_FIT=1.0D0-XN/YN

 END FUNCTION IPEST_GOODNESS_OF_FIT 
 
 !###====================================================================
 SUBROUTINE IPEST_LUDECOMP_DBL(AA,IDX,N,ISING)
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: NMAX
 DOUBLE PRECISION :: TINY
 PARAMETER( TINY=1.0E-20, NMAX=2000)
 INTEGER :: N
 INTEGER :: ISING
 INTEGER :: IDX(N)
 DOUBLE PRECISION :: AA(N,N)
 DOUBLE PRECISION :: VV(NMAX)
 INTEGER :: I,IMAX,J,K
 DOUBLE PRECISION :: AAMAX,DUM,SUM

 DO I=1,N
  IDX(I)=0
 END DO
 ISING=0

 DO I=1,N
  AAMAX=0.
  DO J=1,N
   IF(ABS(AA(I,J)).GT.AAMAX)AAMAX=ABS(AA(I,J))
  ENDDO
  IF(AAMAX.EQ.0.)THEN
   WRITE(*,*) 'Matrix is singular'
   ISING=1
   RETURN
  ENDIF
  VV(I)=1./AAMAX
 ENDDO
 DO J=1,N
  DO I=1,J-1
   SUM=AA(I,J)
   DO K=1,I-1
    SUM=SUM-AA(I,K)*AA(K,J)
   ENDDO
   AA(I,J)=SUM
  ENDDO
  AAMAX=0.
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
  IF(AA(J,J).EQ.0.)AA(J,J)=TINY
  IF(J.NE.N)THEN
   DUM=1./AA(J,J)
   DO I=J+1,N
    AA(I,J)=AA(I,J)*DUM
   ENDDO
  ENDIF
 ENDDO

 RETURN
 END SUBROUTINE IPEST_LUDECOMP_DBL

 !###====================================================================
 SUBROUTINE IPEST_LUBACKSUB_DBL(AA,IDX,BB,N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: N
 DOUBLE PRECISION :: AA(N,N)
 DOUBLE PRECISION :: BB(N)
 INTEGER :: IDX(N)
 INTEGER :: I,II,J,LL
 DOUBLE PRECISION :: SUM

 II=0
 DO I=1,N
  LL=IDX(I)
  SUM=BB(LL)
  BB(LL)=BB(I)
  IF(II.NE.0)THEN
   DO J=II,I-1
    SUM=SUM-AA(I,J)*BB(J)
   ENDDO
  ELSE IF(SUM.NE.0.)THEN
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

 RETURN
 END SUBROUTINE IPEST_LUBACKSUB_DBL
 
END MODULE MOD_IPEST_GLM