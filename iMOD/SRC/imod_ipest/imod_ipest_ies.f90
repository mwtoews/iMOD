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
MODULE MOD_IPEST_IES

USE WINTERACTER
USE MOD_IDF_PAR
USE MOD_IDF, ONLY : IDFREAD,IDFREADSCALE,IDFALLOCATEX,IDFNULLIFY,IDFWRITE,IDFDEALLOCATEX,IDFGETLOC,IDFCOPY
USE IMODVAR, ONLY : DP_KIND,IDPROC
USE MOD_PMANAGER_PAR, ONLY : PEST,PBMAN,PRJIDF,PRJNLAY
USE MOD_UTL, ONLY  : UTL_GETUNIT,UTL_CAP,ITOS,RTOS,UTL_GETMED,UTL_CREATEDIR,UTL_GOODNESS_OF_FIT,UTL_NASH_SUTCLIFFE,UTL_STDEF, &
 UTL_GETGAMMA,UTL_MATMUL
USE MOD_IPEST_GLM_PAR
USE MOD_IPEST_GLM, ONLY : IPEST_GLM_GETJ,IPEST_GLM_ALLOCATEMSR
USE MOD_PMANAGER_UTL, ONLY : PMANAGER_SAVEMF2005_MOD_U2DREL,PMANAGER_SAVEMF2005_DEALLOCATE
USE MOD_OSD
USE MOD_IPEST_IES_PAR
USE MOD_LUDCMP
USE MOD_BATCH_UTL, ONLY : IMODBATCH_CREATEENSEMBLES_CHOLESKY

CONTAINS

 !#####=================================================================
 SUBROUTINE IPEST_IES_MAIN(DIR,IBATCH)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITER,IFLAGS,IGRAD,NCPU,I,ITYPE,JGRAD,IEXCOD,NDONE,N
 REAL(KIND=DP_KIND) :: F,DBLE,TNSC
  
 IF(.NOT.IPEST_IES_ALLOCATE(DIR))RETURN 
 IF(PEST%ICREATEENSEMBLES.EQ.1)THEN; IF(.NOT.IPEST_IES_CREATEENSEMBLES(DIR))RETURN; ENDIF
 
 CALL UTL_CREATEDIR(TRIM(DIR)//'\STATHEADS')
 CALL UTL_CREATEDIR(TRIM(DIR)//'\ENSEMBLES')

 CALL WMESSAGEENABLE(TIMEREXPIRED,1); MSR%PJ=HUGE(1.0D0); ITER=0
 DO WHILE (ITER.LE.PEST%PE_MXITER)

  !## read from idf file and save the realisations in lpf package
  CALL IPEST_IES_SAVEREALS(DIR,ITER) 

  !## executes on commandtool such that commands alike 'dir' etc. works
  IFLAGS=0; IF(PBMAN%CMDHIDE.EQ.1)IFLAGS=IFLAGS+PROCSILENT+PROCCMDPROC

  !## start finite difference approximation for parameters
  CALL WMESSAGETIMER(NCSECS,IREPEAT=1); IGRAD=0; NCPU=0; IPROC=0
  
  WRITE(IUPESTOUT,'(2(A5,1X),A15)') 'ITER','REALS','OBJECTIVEFUNC.'
  
  ISTATUS=-1
  DO 

   !## start processes
   DO

    !## find realisation to be (re)carried out
    DO IGRAD=1,SIZE(RNG); IF(ISTATUS(IGRAD).EQ.-1)EXIT; ENDDO
    IF(IGRAD.GT.SIZE(RNG))EXIT
    
    !## number of cpu full
    NCPU=NCPU+1

    !## wait before starting a new process
    IF(PBMAN%NSWAIT.GT.0)CALL IOSWAIT(PBMAN%NSWAIT)
    !## clear error
    I=WINFOERROR(1); IDPROC=0; CALL IOSCOMMAND(TRIM(RNG(IGRAD)),IFLAGS=IFLAGS,IDPROC=IDPROC); IPROC(:,IGRAD)=IDPROC
    IF(WINFOERROR(1).EQ.ERROSCOMMAND)THEN
     IF(IBATCH.EQ.1)WRITE(*,'(/A/)') 'FAILED TO START MODEL R#'//TRIM(ITOS(GPARAM(IGRAD)))
     IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'FAILED TO START MODEL R#'//TRIM(ITOS(GPARAM(IGRAD))),'Error')
     RETURN
    ENDIF
    ISTATUS(IGRAD)=1
    
    !## all started
    DO IGRAD=1,SIZE(RNG); IF(ISTATUS(IGRAD).EQ.-1)EXIT; ENDDO; IF(IGRAD.GT.SIZE(RNG))EXIT
    !## maximum number of cpu reached
    IF(NCPU.GE.PBMAN%NCPU)EXIT
   ENDDO
   
   !## evaluate processes
   DO
    CALL WMESSAGE(ITYPE,MESSAGE)
    !## timer expired
    IF(ITYPE.EQ.TIMEREXPIRED)THEN
     N=0; DO JGRAD=1,SIZE(RNG)
      !## all handled process
      IF(IPROC(1,JGRAD)+IPROC(2,JGRAD).EQ.0)CYCLE
      !## check running status
      IDPROC=IPROC(:,JGRAD)
      CALL IOSCOMMANDCHECK(IDPROC,ISTATUS(JGRAD),IEXCOD=IEXCOD)
      !## stopped running
      IF(ISTATUS(JGRAD).EQ.0)THEN
       !## error occured 
       IF(IEXCOD.NE.0)THEN
        IF(IBATCH.EQ.1)WRITE(*,'(/A/)') 'ERROR OCCURED RUNNING MODEL P#'//TRIM(ITOS(GPARAM(JGRAD)))
        IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'ERROR OCCURED RUNNING MODEL P#'//TRIM(ITOS(GPARAM(JGRAD))),'Error')
        !## try again - need to run again
        ISTATUS(JGRAD)=-1
       ENDIF
       !## set part of objective function
       IF(ISTATUS(JGRAD).EQ.0)THEN
        IF(IUPESTRESIDUAL.GT.0)CLOSE(IUPESTRESIDUAL)
        IUPESTRESIDUAL=UTL_GETUNIT()
        OPEN(IUPESTRESIDUAL,FILE=TRIM(DIR)//'\IIES\LOG_PEST_RESIDUAL_'//TRIM(ITOS(ITER))//'_R#'//TRIM(ITOS(JGRAD))//'.TXT',STATUS='UNKNOWN',ACTION='WRITE')
        IF(.NOT.IPEST_GLM_GETJ(DIR,JGRAD,GPARAM(JGRAD),'R',IBATCH,TNSC))RETURN
        CLOSE(IUPESTRESIDUAL)
        !## write echo
        CALL IPEST_IES_PROGRESS(ITER,JGRAD)
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
     NDONE=0; DO IGRAD=1,SIZE(RNG); IF(ISTATUS(IGRAD).EQ.0)NDONE=NDONE+1; ENDDO
     F=DBLE(NDONE)*100.0D0/DBLE(SIZE(RNG))
     WRITE(6,'(A)') '+Still running '//TRIM(ITOS(N))//'; models completed: '//TRIM(RTOS(F,'F',2))//'% (total '//TRIM(ITOS(NDONE))//' out of '//TRIM(ITOS(SIZE(RNG)))//' simulations)'
     !## nothing running anymore
     IF(N.EQ.0)EXIT
     !## start another one as a proces has been stopped and there is still one waiting in the que
     IF(NCPU.LT.PBMAN%NCPU.AND.NDONE.LT.SIZE(RNG))EXIT
    ENDIF
   ENDDO
   !## finished if all succesfully completed
   IF(NDONE.EQ.SIZE(RNG))EXIT
  ENDDO
  
  !## get update of realisations
  IF(.NOT.IPEST_IES_UPDATE_ENSEMBLES(DIR,ITER))EXIT
  !## save mean/stdev heads
  CALL IPEST_IES_SAVE_RESULT_STATS(DIR,ITER)

  !## next cycle
  ITER=ITER+1

 ENDDO
 
 !## finished write summary of objective function values
 WRITE(IUPESTOUT,'(A10,999(1X,A15))') 'ITER',('OBJF_SIM'//TRIM(ITOS(IGRAD)),IGRAD=1,PEST%NREALS)
 DO ITER=0,PEST%PE_MXITER
  WRITE(IUPESTOUT,'(I10,999(1X,F15.7))') ITER,(JE(IGRAD,ITER),IGRAD=1,PEST%NREALS)
 ENDDO

 CALL WMESSAGETIMER(0); CALL WMESSAGEENABLE(TIMEREXPIRED,0)
 PBMAN%IIES=0; CALL PMANAGER_SAVEMF2005_DEALLOCATE(); CALL IPEST_IES_DEALLOCATE
 
 END SUBROUTINE IPEST_IES_MAIN

 !#####=================================================================
 LOGICAL FUNCTION IPEST_IES_CREATEENSEMBLES(DIR)!,IBATCH)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: I,J
 TYPE(IDFOBJ) :: STDEV,MEAN
 
 IPEST_IES_CREATEENSEMBLES=.FALSE.
 CALL IDFCOPY(PRJIDF,STDEV); CALL IDFCOPY(PRJIDF,MEAN)
 
 DO I=1,SIZE(PEST%PARAM)
  STDEV%X=PEST%PARAM(I)%PARSTD
  MEAN%X=PEST%PARAM(I)%PARMEAN
  CALL IMODBATCH_CREATEENSEMBLES_CHOLESKY(STDEV,MEAN,TRIM(DIR)//'\ENSEMBLES', &
      PEST%PARAM(I)%PARRANGE,PEST%NREALS,'PARAMETER'//TRIM(ITOS(I)))
  DO J=1,PEST%NREALS
   PEST%PARAM(I)%REALSFNAME(J)=TRIM(DIR)//'\ENSEMBLES\PARAMETER'//TRIM(ITOS(I))//'_R'//TRIM(ITOS(J))//'.IDF'
  ENDDO
!  PEST%PARAM(I)%ICREATEENSEMBLES
 ENDDO
 
 IPEST_IES_CREATEENSEMBLES=.TRUE.
 
 END FUNCTION IPEST_IES_CREATEENSEMBLES

 !#####=================================================================
 SUBROUTINE IPEST_IES_SAVE_RESULT_STATS(DIR,ITER)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER,INTENT(IN) :: ITER
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: TMP1
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: HDS
 TYPE(IDFOBJ) :: MHD,VHD
 REAL(KIND=DP_KIND) :: AVG,STD
 INTEGER :: I,IROW,ICOL,N,ILAY
 
 !## compute mean and variance heads
 ALLOCATE(HDS(PEST%NREALS)); DO I=1,PEST%NREALS; CALL IDFNULLIFY(HDS(I)); ENDDO; CALL IDFNULLIFY(MHD); CALL IDFNULLIFY(VHD)
 DO ILAY=1,PRJNLAY
  DO I=1,PEST%NREALS
   HDS(I)%FNAME=TRIM(DIR)//'\IIES_R#'//TRIM(ITOS(I))//'\HEAD\HEAD_STEADY-STATE_L'//TRIM(ITOS(ILAY))//'.IDF'
   IF(.NOT.IDFREAD(HDS(I),HDS(I)%FNAME,1))STOP
  ENDDO
  IF(ILAY.EQ.1)THEN; CALL IDFCOPY(HDS(1),MHD); CALL IDFCOPY(HDS(1),VHD); ENDIF
  ALLOCATE(TMP1(PEST%NREALS)); DO IROW=1,MHD%NROW; DO ICOL=1,MHD%NCOL
   TMP1(1)=0.0D0; DO I=1,PEST%NREALS; TMP1(I)=HDS(I)%X(ICOL,IROW); ENDDO
   CALL UTL_STDEF(TMP1,PEST%NREALS,HDS(1)%NODATA,STD,AVG,N)
   MHD%X(ICOL,IROW)=AVG; VHD%X(ICOL,IROW)=STD
  ENDDO; ENDDO
  DEALLOCATE(TMP1)
  CALL UTL_CREATEDIR(TRIM(DIR)//'\STATHEADS')
  MHD%FNAME=TRIM(DIR)//'\STATHEADS\MEAN_HEAD_STEADY-STATE_L'//TRIM(ITOS(ILAY))//'_ITER_'//TRIM(ITOS(ITER))//'.IDF'
  IF(.NOT.IDFWRITE(MHD,MHD%FNAME,1))STOP
  VHD%FNAME=TRIM(DIR)//'\STATHEADS\STDEV_HEAD_STEADY-STATE_L'//TRIM(ITOS(ILAY))//'_ITER_'//TRIM(ITOS(ITER))//'.IDF'
  IF(.NOT.IDFWRITE(VHD,VHD%FNAME,1))STOP
 ENDDO
 
 END SUBROUTINE IPEST_IES_SAVE_RESULT_STATS
 
 !#####=================================================================
 LOGICAL FUNCTION IPEST_IES_UPDATE_ENSEMBLES(DIR,ITER) 
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER,INTENT(INOUT) :: ITER
 INTEGER :: I,II,III,J,JJ,K,N,SPACEDIM,TIMEDIM,OBSDIM,IROW,ICOL,IX
 REAL(KIND=DP_KIND) :: X,VAR,A,L,TL,MAXTL,GAMMA
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: DM 
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: SO,O,TMP1,TMP2,PCLASS
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: REALS
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: PDF 
 CHARACTER(LEN=256) :: FNAME
  
 !## apply log transformation
 KLOG=.TRUE.
 GAMMA=4.0D0
 MAXTL=100.0D0
 IPEST_IES_UPDATE_ENSEMBLES=.TRUE.

 SPACEDIM=PRJIDF%NROW*PRJIDF%NCOL
 TIMEDIM=PEST%NREALS
 OBSDIM=MSR%NOBS

 X=-10.5D0; N=1; DO; N=N+1; X=X+0.5D0; IF(X.GT.10.0D0)EXIT; ENDDO
 ALLOCATE(PCLASS(N)); PCLASS=0.0D0
 PCLASS(1)=-10.5D0; N=1; DO; N=N+1; PCLASS(N)=PCLASS(N-1)+0.5D0; IF(PCLASS(N).GT.10.0D0)EXIT; ENDDO
 IF(ALLOCATED(PDF))DEALLOCATE(PDF); ALLOCATE(PDF(N,TIMEDIM+1))

 ALLOCATE(TMP1(OBSDIM),TMP2(OBSDIM*TIMEDIM))

 !## get residual objective function value
 ALLOCATE(SO(TIMEDIM),O(TIMEDIM)); SO=0.0D0; O=0.0D0
 !## use weigth (get variance from weights) - get matrix of objective function values
 TMP2=0.0D0; II=0; DO J=1,TIMEDIM
  TMP1=0.0D0; I=0
  DO K=1,OBSDIM
   I=I+1; TMP1(I)=MSR%DHG(J,K); II=II+1; TMP2(II)=TMP1(I)
   VAR=1.0D0/MSR%W(K); SO(J)=SO(J)+MSR%DHG(J,K)**2.0D0*(1.0D0/VAR)
   O(J)=O(J)+MSR%DHG(J,K)**2.0D0
  ENDDO
  !## get pdf of error distribution
  CALL IPEST_IES_GETPDF(TMP1,TIMEDIM,PDF(:,J),PCLASS,'N')
 ENDDO
 !## get pdf of error distribution
 CALL IPEST_IES_GETPDF(TMP2,OBSDIM*TIMEDIM,PDF(:,TIMEDIM+1),PCLASS,'N')
 WRITE(IUPESTOUT,'(/A/)') 'PDF OBJECTIVE FUNCTION VALUES'
 WRITE(IUPESTOUT,'(A15,999(1X,A10))') 'CLASS','ALL',('REALS'//TRIM(ITOS(II)),II=1,TIMEDIM)
 DO JJ=1,SIZE(PDF,1)
  WRITE(IUPESTOUT,'(F15.7,999(1X,I10))') PCLASS(JJ),PDF(JJ,TIMEDIM+1),(PDF(JJ,II),II=1,TIMEDIM) 
 ENDDO
 DEALLOCATE(TMP1,TMP2)
 
 !## get current sum of weighted objective function values
 MSR%TJ=SUM(SO); MSR%J=SUM(O)/DBLE(TIMEDIM)/DBLE(OBSDIM)

 !## determine initial lambda
 IF(ITER.EQ.0)THEN
  X=MSR%TJ/DBLE(2.0D0*OBSDIM); X=LOG10(X); IX=FLOOR(X); LAMBDA=10.0D0**IX
 ENDIF
 
 !## evaluate progression
 IF(ITER.GT.0)THEN

  !## do something with statistics, mean and stdev
  WRITE(IUPESTOUT,'(A10,5(1X,A15))') 'ITER','CURRENT_TJ','PREVIOUS_PJ','AVG-RESIDUAL','IMPROVEMENT(%)','LAMBDA'
  WRITE(IUPESTOUT,'(I10,5(1X,F15.7))') ITER,MSR%TJ,MSR%PJ,MSR%J,100.0D0*(1.0D0-MSR%TJ/MSR%PJ),LAMBDA
  WRITE(IUPESTPROGRESS,'(I10,99(1X,F15.7))') ITER,MSR%TJ,MSR%PJ,MSR%J,100.0D0*(1.0D0-MSR%TJ/MSR%PJ),LAMBDA, &
     (PEST%PARAM(J)%MEAN(TIMEDIM+1),PEST%PARAM(J)%STD(TIMEDIM+1),J=1,SIZE(PEST%PARAM))
  WRITE(IUPESTOUT,'(2A10,3A15)') 'ITER','ENSEMBLE','MEAN','STDEV','VARIANCE' 
  DO J=1,SIZE(PEST%PARAM)
   DO I=1,TIMEDIM
    WRITE(IUPESTOUT,'(2I10,3F15.7)') I,J,PEST%PARAM(J)%MEAN(I),PEST%PARAM(J)%STD(I),PEST%PARAM(J)%STD(I)**2.0D0
   ENDDO
   I=TIMEDIM+1
   WRITE(IUPESTOUT,'(/2I10,3F15.7/)') I,0,PEST%PARAM(J)%MEAN(I),PEST%PARAM(J)%STD(I),PEST%PARAM(J)%STD(I)**2.0D0
  ENDDO

  IF(MSR%TJ.LE.MSR%PJ)THEN
   !## too less improvement
   IF(1.0D0-MSR%TJ/MSR%PJ.LT.PEST%PE_STOP)THEN
    IPEST_IES_UPDATE_ENSEMBLES=.FALSE.; RETURN
   ELSE
    !## YES!!! Continue to next iteration
    LAMBDA=LAMBDA/GAMMA
    WRITE(IUPESTOUT,'(/A/)') 'Awesome, significant reduction of the objective function reduction: proceed'
    !## save previous objective function value
    MSR%PJ=MSR%TJ
   ENDIF
  ELSE
   LAMBDA=LAMBDA*GAMMA
   !## reset ensembles to previous iteration
   ITER=ITER-1
   WRITE(IUPESTOUT,'(/A/)') 'No objective function reduction: reset to previous iteration'
  ENDIF
 ENDIF
 
 !## try to find acceptable ensemble update
 DO
 
  TL=0.0D0; A=LAMBDA+1.0D0

  !## processing the inverse of the prior parameter covariance matrix
  DO I=1,SIZE(PEST%PARAM)

   !## initialisation
   IF(ITER.EQ.0)CALL IPEST_IES_INITIATE_INVERSE_MATRICES(DIR,I,TIMEDIM,SPACEDIM)
  
   !## compute dm-matrix
   IF(ALLOCATED(REALS))DEALLOCATE(REALS); IF(ALLOCATED(DM))DEALLOCATE(DM)
   ALLOCATE(REALS(TIMEDIM,SPACEDIM),DM(TIMEDIM,SPACEDIM))
   CALL IPEST_IES_COMPUTE_DM(DIR,I,TIMEDIM,SPACEDIM,REALS,DM,ITER) 

   !## store prior-estimated parameters
   IF(ITER.EQ.0)THEN
    !## might be coming here twice as improvement of objective function is not enough
    IF(.NOT.ALLOCATED(MPR))THEN
! IF(ALLOCATED(MPR))DEALLOCATE(MPR)
! CALL IPEST_IES_COMPUTE_GETREALS(DIR,I,TIMEDIM,SPACEDIM,REALS,0)
     ALLOCATE(MPR(TIMEDIM,SPACEDIM)); MPR=REALS
     !## compute am-matrix initially through svd
     CALL IPEST_IES_COMPUTE_AM(TIMEDIM,SPACEDIM,DM)
    ENDIF
   ENDIF
  
!   !## processing the inverse of the prior parameter covariance matrix
!   CALL IPEST_IES_COMPUTE_GETREALS(DIR,I,TIMEDIM,SPACEDIM,REALS,ITER)
   CALL IPEST_IES_COMPUTE_PARTIAL_M(ITER,I,TIMEDIM,SPACEDIM,OBSDIM,DM,A,MPR,REALS,L); TL=TL+L
   WRITE(IUPESTOUT,'(/A,F15.7/)') 'LENGTH OF INDIVIDUAL ENSEMBLE UPDATE=',L

   !## save the updates for the next iteration
   IF(.NOT.IDFALLOCATEX(PRJIDF))STOP
   ALLOCATE(TMP1(SPACEDIM),TMP2(SPACEDIM*TIMEDIM))

   III=0; TMP2=0.0D0
   DO K=1,TIMEDIM
    II=0; DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL; II=II+1; III=III+1
     TMP1(II)=REALS(K,II); IF(KLOG)TMP1(II)=EXP(TMP1(II)); TMP2(III)=TMP1(II)
!     TMP1(II)=REALS(K,II); IF(KLOG)TMP1(II)=10.0D0**(TMP1(II)); TMP2(III)=TMP1(II)
     PRJIDF%X(ICOL,IROW)=TMP1(II)
    ENDDO; ENDDO
    !## get statistics (variance is outcome)
    CALL UTL_STDEF(TMP1,SPACEDIM,PRJIDF%NODATA,PEST%PARAM(I)%STD(K),PEST%PARAM(I)%MEAN(K),N)
    !## get pdf of parameters
    CALL IPEST_IES_GETPDF(TMP1,SPACEDIM,PDF(:,K),PCLASS,'L')
    !## get name of ensemble
    CALL IPEST_IES_GETENSEMBLENAME(ITER+1,DIR,PEST%PARAM(I)%REALSFNAME(K),FNAME)
    IF(.NOT.IDFWRITE(PRJIDF,FNAME,1))STOP
   ENDDO

   !## get statistics (variance is outcome)
   CALL UTL_STDEF(TMP2,SPACEDIM*TIMEDIM,PRJIDF%NODATA,PEST%PARAM(I)%STD(TIMEDIM+1),PEST%PARAM(I)%MEAN(TIMEDIM+1),N)
   !## get pdf of parameters
   CALL IPEST_IES_GETPDF(TMP2,SPACEDIM*TIMEDIM,PDF(:,TIMEDIM+1),PCLASS,'L')
  
   WRITE(IUPESTOUT,'(/A/)') 'PDF PARAMETERS '//TRIM(ITOS(I))
   WRITE(IUPESTOUT,'(A15,999(1X,A10))') 'CLASS','ALL',('REAL'//TRIM(ITOS(II)),II=1,TIMEDIM)
   DO JJ=1,SIZE(PDF,1)
    WRITE(IUPESTOUT,'(F15.7,999(1X,I10))') PCLASS(JJ),PDF(JJ,TIMEDIM+1),(PDF(JJ,II),II=1,TIMEDIM) 
   ENDDO
  
   DEALLOCATE(TMP1,TMP2); CALL IDFDEALLOCATEX(PRJIDF)

  ENDDO
   
  !## do not allow enormous updates
  IF(TL.LT.MAXTL)EXIT
  
  LAMBDA=LAMBDA*GAMMA
  WRITE(IUPESTOUT,'(/A,F15.7/)') 'No acceptable ensemble update (too large), increasing lambda ',LAMBDA

 ENDDO

 !## possible update vector is less that criterion
 WRITE(IUPESTOUT,'(/A,F15.7/)') 'TOTAL LENGTH OF ENSEMBLE UPDATE=',TL
 IF(TL.LT.PEST%PE_PADJ)IPEST_IES_UPDATE_ENSEMBLES=.FALSE.
  
 DEALLOCATE(SO,DM,REALS)
 
 FLUSH(IUPESTOUT); FLUSH(IUPESTPROGRESS)
 
 END FUNCTION IPEST_IES_UPDATE_ENSEMBLES
 
 !#####=================================================================
 SUBROUTINE IPEST_IES_INITIATE_INVERSE_MATRICES(DIR,IPARAM,TIMEDIM,SPACEDIM)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER,INTENT(IN) :: IPARAM,TIMEDIM,SPACEDIM
 INTEGER :: IU,N,J,K,IROW,ICOL,IROW2,ICOL2
 TYPE(IDFOBJ) :: IDF
 REAL(KIND=DP_KIND) :: X1,Y1,X2,Y2,GAMMA,XCOR,LV,HV
 
 IF(ASSOCIATED(PEST%PARAM(IPARAM)%SQRTCOV))RETURN

 ALLOCATE(PEST%PARAM(IPARAM)%MEAN(TIMEDIM+1)); PEST%PARAM(IPARAM)%MEAN=0.0D0
 ALLOCATE(PEST%PARAM(IPARAM)%STD(TIMEDIM+1));  PEST%PARAM(IPARAM)%STD=0.0D0
 IF(TRIM(PEST%PARAM(IPARAM)%ICOVFNAME).NE.'')THEN
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=PEST%PARAM(IPARAM)%ICOVFNAME,STATUS='OLD',ACTION='READ')
  READ(IU,*); READ(IU,*) N; IF(N.NE.SPACEDIM)STOP 'N.NE.SPACEDIM IN COVARIANCE MATRIX'; ALLOCATE(PEST%PARAM(IPARAM)%COV(SPACEDIM,SPACEDIM))
  DO J=1,SPACEDIM; READ(IU,*) (PEST%PARAM(IPARAM)%COV(J,K),K=1,SPACEDIM); ENDDO; CLOSE(IU)
  IF(KLOG)THEN
   WRITE(*,'(/A/)') 'IS THIS COVARIANCE-MATRIX LOG-TRANSFORMED, I BET NOT'; STOP
  ENDIF
 ELSE
  !## fill in the covariance matrix with covariates using the exponential variogram with range ... sill=1.0
  ALLOCATE(PEST%PARAM(IPARAM)%COV(SPACEDIM,SPACEDIM))
  PEST%PARAM(IPARAM)%COV=0.0D0
  IF(KLOG)THEN
   DO J=1,SPACEDIM
!    !## low- and high values captures 95% of distribution
!    LV=PEST%PARAM(IPARAM)%PARMEAN-2.0D0*PEST%PARAM(IPARAM)%PARSTD; LV=MAX(LV,MINK); LV=LOG10(ABS(LV))
!    HV=PEST%PARAM(IPARAM)%PARMEAN+2.0D0*PEST%PARAM(IPARAM)%PARSTD; HV=MAX(HV,MINK); HV=LOG10(ABS(HV))
!    PEST%PARAM(IPARAM)%COV(J,J)=((HV-LV)/4.0D0)**2.0D0
    PEST%PARAM(IPARAM)%COV(J,J)=LOG(PEST%PARAM(IPARAM)%PARSTD)**2.0D0
!    PEST%PARAM(IPARAM)%COV(J,J)=LOG10(PEST%PARAM(IPARAM)%PARSTD)
   ENDDO
  ELSE
   DO J=1,SPACEDIM
!    !## low- and high values captures 95% of distribution
!    LV=PEST%PARAM(IPARAM)%PARMEAN-2.0D0*PEST%PARAM(IPARAM)%PARSTD
!    HV=PEST%PARAM(IPARAM)%PARMEAN+2.0D0*PEST%PARAM(IPARAM)%PARSTD
!    PEST%PARAM(IPARAM)%COV(J,J)=((HV-LV)/4.0D0)**2.0D0
    PEST%PARAM(IPARAM)%COV(J,J)=PEST%PARAM(IPARAM)%PARSTD**2.0D0
   ENDDO
  ENDIF
  IROW=1; ICOL=0; DO J=1,SPACEDIM
   ICOL=ICOL+1; IF(ICOL.GT.PRJIDF%NCOL)THEN; ICOL=1; IROW=IROW+1; ENDIF
   CALL IDFGETLOC(PRJIDF,IROW,ICOL,X1,Y1)
   IROW2=IROW; ICOL2=ICOL; DO K=J+1,SPACEDIM
    ICOL2=ICOL2+1; IF(ICOL2.GT.PRJIDF%NCOL)THEN; ICOL2=1; IROW2=IROW2+1; ENDIF
    CALL IDFGETLOC(PRJIDF,IROW2,ICOL2,X2,Y2)
    !## gets the variogram values
    GAMMA=UTL_GETGAMMA(X1,Y1,X2,Y2,PEST%PARAM(IPARAM)%PARRANGE,1.0D0,0.0D0,3)
    XCOR=1.0D0-GAMMA
    PEST%PARAM(IPARAM)%COV(J,K)=SQRT(PEST%PARAM(IPARAM)%COV(J,J))*SQRT(PEST%PARAM(IPARAM)%COV(K,K))*XCOR
    PEST%PARAM(IPARAM)%COV(K,J)=PEST%PARAM(IPARAM)%COV(J,K)
   ENDDO
  ENDDO
  N=SPACEDIM
 ENDIF
 !## save as IDF (for fun)
 CALL IDFNULLIFY(IDF)
 IDF%DX=1.0D0; IDF%DY=1.0D0; IDF%NCOL=N; IDF%NROW=N
 IDF%XMIN=0.0D0; IDF%XMAX=IDF%XMIN+IDF%NCOL*IDF%DX
 IDF%YMIN=0.0D0; IDF%YMAX=IDF%YMIN+IDF%NROW*IDF%DY
 IF(.NOT.IDFALLOCATEX(IDF))RETURN
 DO J=1,N; DO K=1,N; IDF%X(J,K)=PEST%PARAM(IPARAM)%COV(J,K); ENDDO; ENDDO
 IDF%FNAME=TRIM(DIR)//'\ENSEMBLES\COV.IDF'; IF(.NOT.IDFWRITE(IDF,IDF%FNAME,1))STOP
 CALL IDFDEALLOCATEX(IDF)
  
 ALLOCATE(PEST%PARAM(IPARAM)%ISQRTCOV(N,N),PEST%PARAM(IPARAM)%SQRTCOV(N,N))
 CALL LUDCMP_CALC_SQRTROOTINVERSE(N,PEST%PARAM(IPARAM)%COV,PEST%PARAM(IPARAM)%ISQRTCOV,PEST%PARAM(IPARAM)%SQRTCOV)
 IF(.NOT.IDFALLOCATEX(IDF))RETURN
 DO J=1,N; DO K=1,N; IDF%X(J,K)=PEST%PARAM(IPARAM)%ISQRTCOV(J,K); ENDDO; ENDDO
 IDF%FNAME=TRIM(DIR)//'\ENSEMBLES\ISQRTCOV.IDF'; IF(.NOT.IDFWRITE(IDF,IDF%FNAME,1))STOP
 CALL IDFDEALLOCATEX(IDF)
 DEALLOCATE(PEST%PARAM(IPARAM)%COV)
 
 END SUBROUTINE IPEST_IES_INITIATE_INVERSE_MATRICES
  
 !#####=================================================================
 SUBROUTINE IPEST_IES_GETPDF(X,SPACEDIM,PDF,XCLASS,CT)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: SPACEDIM
 REAL(KIND=DP_KIND),DIMENSION(:) :: XCLASS
 CHARACTER(LEN=1),INTENT(IN) :: CT
 REAL(KIND=DP_KIND),DIMENSION(SPACEDIM) :: X
 INTEGER,DIMENSION(:) :: PDF
 REAL(KIND=DP_KIND) :: LX 
 INTEGER :: I,J 
 
 PDF=0; DO I=1,SPACEDIM
  DO J=1,SIZE(XCLASS)-1
   IF(CT.EQ.'L')THEN
    LX=LOG(X(I))
!    LX=LOG10(X(I))
   ELSE
    LX=X(I)
   ENDIF
   IF(LX.GE.XCLASS(J).AND.LX.LT.XCLASS(J+1))PDF(J)=PDF(J)+1
  ENDDO
 ENDDO
 
 END SUBROUTINE IPEST_IES_GETPDF
 
 !#####=================================================================
 SUBROUTINE IPEST_IES_COMPUTE_PARTIAL_M(ITER,IPARAM,TIMEDIM,SPACEDIM,OBSDIM,DM,A,MPR,REALS,L)  
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER,IPARAM,TIMEDIM,SPACEDIM,OBSDIM
 REAL(KIND=DP_KIND),INTENT(OUT) :: L
 REAL(KIND=DP_KIND),INTENT(IN) :: A
 REAL(KIND=DP_KIND),DIMENSION(TIMEDIM,SPACEDIM),INTENT(IN) :: DM,MPR
 REAL(KIND=DP_KIND),DIMENSION(TIMEDIM,SPACEDIM),INTENT(INOUT) :: REALS
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: X1,X2,X3,X4,X5,X6,X7,PM1,PM2,DD
 REAL(KIND=DP_KIND) :: RN,XSUM,VAR
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: W
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: V,VV,U,TMP1
 INTEGER :: I,J,MAXDIM
 
 MAXDIM=MAX(TIMEDIM,OBSDIM)
 ALLOCATE(X1(TIMEDIM,OBSDIM),DD(TIMEDIM,OBSDIM),X2(OBSDIM,OBSDIM))

 RN=SQRT(DBLE(TIMEDIM)-1.0D0)

 !## compute ensemble mean
 DO I=1,OBSDIM
  !## compute average observational error
  XSUM=0.0D0; DO J=1,TIMEDIM; XSUM=XSUM+MSR%DHG(J,I); ENDDO; XSUM=XSUM/DBLE(TIMEDIM)
  DO J=1,TIMEDIM; X1(J,I)=(MSR%DHG(J,I)-XSUM)/RN; ENDDO
 ENDDO

 X2=0.0D0; DO I=1,OBSDIM; VAR=1.0D0/MSR%W(I); X2(I,I)=SQRT(1.0D0/VAR); ENDDO
 !## dd is [timedim,obsdim]
 CALL UTL_MATMUL(X2,X1,DD); DEALLOCATE(X1)

 ALLOCATE(W(MAXDIM),U(OBSDIM,OBSDIM),VV(TIMEDIM,OBSDIM),TMP1(OBSDIM,OBSDIM))
 !## compute svd
 TMP1=DD; CALL LUDCMP_SVD_MAIN(OBSDIM,TIMEDIM,MAXDIM,TMP1,U,W,VV)
 
 !## truncate for non-zeros
 DD_TRUNCATED=0; DO I=1,MAXDIM; IF(W(I).LE.0.0001D0)EXIT; DD_TRUNCATED=DD_TRUNCATED+1; ENDDO
 
 !## store actual vector v
 ALLOCATE(V(DD_TRUNCATED,TIMEDIM))
 DO I=1,DD_TRUNCATED; DO J=1,TIMEDIM
  V(I,J)=VV(J,I)
 ENDDO; ENDDO

 !## compute x1 from chen et al.
 ALLOCATE(X1(OBSDIM,DD_TRUNCATED),X3(OBSDIM,DD_TRUNCATED)); X1=0.0D0
 DO I=1,OBSDIM; DO J=1,DD_TRUNCATED; X1(I,J)=U(J,I); ENDDO; ENDDO 
 CALL UTL_MATMUL(X1,X2,X3)
 DEALLOCATE(X1); ALLOCATE(X1(TIMEDIM,DD_TRUNCATED))
 CALL UTL_MATMUL(X3,MSR%DHG,X1)
 
 !## compute x2 from chen et al.
 DEALLOCATE(X3); ALLOCATE(X3(DD_TRUNCATED,DD_TRUNCATED)); X3=0.0D0
 DO I=1,DD_TRUNCATED; X3(I,I)=1.0D0/(A+W(I)**2.0D0); ENDDO
 DEALLOCATE(X2); ALLOCATE(X2(TIMEDIM,DD_TRUNCATED))
 CALL UTL_MATMUL(X3,X1,X2)
 
 !## compute x3 from chen et al.
 DEALLOCATE(X3); ALLOCATE(X3(DD_TRUNCATED,DD_TRUNCATED)); X3=0.0D0
 DO I=1,DD_TRUNCATED; X3(I,I)=W(I); ENDDO
 ALLOCATE(X4(DD_TRUNCATED,TIMEDIM)); X4=0.0D0
 DO I=1,DD_TRUNCATED; DO J=1,TIMEDIM; X4(I,J)=V(I,J); ENDDO; ENDDO
 ALLOCATE(X5(DD_TRUNCATED,TIMEDIM))
 CALL UTL_MATMUL(X4,X3,X5)
 DEALLOCATE(X3,X4)
 ALLOCATE(X3(TIMEDIM,TIMEDIM))
 CALL UTL_MATMUL(X5,X2,X3)
 DEALLOCATE(X1,X2,X5)

 ALLOCATE(X1(TIMEDIM,SPACEDIM))
 CALL UTL_MATMUL(-PEST%PARAM(IPARAM)%SQRTCOV,DM,X1)
 ALLOCATE(PM1(TIMEDIM,SPACEDIM))
 !## compute pm1
 CALL UTL_MATMUL(X1,X3,PM1); DEALLOCATE(X1,X3)

 IF(ITER.GT.0)THEN
  !## calculate x4 from paper of chen et al.
  ALLOCATE(X1(SPACEDIM,AM_TRUNCATED)); X1=0.0D0
  DO I=1,SPACEDIM; DO J=1,AM_TRUNCATED; X1(I,J)=AM(J,I); ENDDO; ENDDO
  ALLOCATE(X2(SPACEDIM,AM_TRUNCATED))
  CALL UTL_MATMUL(X1,PEST%PARAM(IPARAM)%ISQRTCOV,X2)
  DEALLOCATE(X1)
  ALLOCATE(X4(TIMEDIM,AM_TRUNCATED),X1(TIMEDIM,SPACEDIM))
  DO I=1,TIMEDIM; DO J=1,SPACEDIM; X1(I,J)=REALS(I,J)-MPR(I,J); ENDDO; ENDDO
  CALL UTL_MATMUL(X2,X1,X4)
  DEALLOCATE(X1,X2)

  !## calculate x5 from paper of chen et al.
  ALLOCATE(X5(TIMEDIM,SPACEDIM))
  CALL UTL_MATMUL(AM,X4,X5)
  DEALLOCATE(X4)

  !## calculate x6 from paper of chen et al.
  ALLOCATE(X4(SPACEDIM,TIMEDIM))
  DO I=1,SPACEDIM; DO J=1,TIMEDIM; X4(I,J)=DM(J,I); ENDDO; ENDDO
  ALLOCATE(X6(TIMEDIM,TIMEDIM))
  CALL UTL_MATMUL(X4,X5,X6)
  DEALLOCATE(X4,X5)

  !## calculate x7 from paper of chen et al.
  !## part 1
  ALLOCATE(X1(DD_TRUNCATED,DD_TRUNCATED)); X1=0.0D0 
  DO I=1,DD_TRUNCATED; IF(W(I).GT.0.0D0)X1(I,I)=1.0D0/(A+W(I)**2.0D0); ENDDO
  ALLOCATE(X2(DD_TRUNCATED,TIMEDIM),X3(DD_TRUNCATED,TIMEDIM)); X2=0.0D0; X3=0.0D0
  DO I=1,DD_TRUNCATED; DO J=1,TIMEDIM; X3(I,J)=V(I,J); ENDDO; ENDDO
  CALL UTL_MATMUL(X3,X1,X2)
  DEALLOCATE(X1,X3)

  ALLOCATE(X1(TIMEDIM,DD_TRUNCATED),X3(TIMEDIM,TIMEDIM))
  DO I=1,TIMEDIM; DO J=1,DD_TRUNCATED; X1(I,J)=V(J,I); ENDDO; ENDDO
  CALL UTL_MATMUL(X2,X1,X3)
  DEALLOCATE(X2,X1)

  ALLOCATE(X7(TIMEDIM,TIMEDIM))
  CALL UTL_MATMUL(X3,X6,X7)
  DEALLOCATE(X3,X6)

  ALLOCATE(X1(TIMEDIM,SPACEDIM))
  CALL UTL_MATMUL(-PEST%PARAM(IPARAM)%SQRTCOV,DM,X1)
  ALLOCATE(PM2(TIMEDIM,SPACEDIM))
  CALL UTL_MATMUL(X1,X7,PM2) 
  DEALLOCATE(X1,X7)
 ENDIF
 
 ALLOCATE(X1(TIMEDIM,SPACEDIM))
 X1=REALS+PM1; IF(ITER.GT.0)X1=X1+PM2
 DEALLOCATE(PM1); IF(ALLOCATED(PM2))DEALLOCATE(PM2)

 !## check update
 DO I=1,TIMEDIM; DO J=1,SPACEDIM; L=L+(X1(I,J)-REALS(I,J))**2.0D0; ENDDO; ENDDO; L=SQRT(L)
 
 !## copy update to reals
 REALS=X1
 
 DEALLOCATE(X1,TMP1,U,V,W)
 
 END SUBROUTINE IPEST_IES_COMPUTE_PARTIAL_M
 
 !#####=================================================================
 SUBROUTINE IPEST_IES_COMPUTE_AM(TIMEDIM,SPACEDIM,DM) 
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: TIMEDIM,SPACEDIM
 REAL(KIND=DP_KIND),DIMENSION(TIMEDIM,SPACEDIM),INTENT(IN) :: DM
 INTEGER :: I,J,NT,NS
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: W
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: A,V,U
 LOGICAL :: LTEST
 
 LTEST=.FALSE.

 IF(LTEST)THEN
  NT=2; NS=4
 ELSE
  NT=TIMEDIM; NS=SPACEDIM
 ENDIF
 
 ALLOCATE(W(MAX(NT,NS)),A(NT,NS),U(NS,NS),V(NT,NS))
 IF(.NOT.LTEST)THEN
  A=DM
 ELSE
  A=0.0D0
!  A(1,1)=2.0D0
!  A(2,1)=4.0D0
!  A(1,2)=1.0D0
!  A(2,2)=3.0D0
 ENDIF

 !## svd computation
 CALL LUDCMP_SVD_MAIN(NS,NT,MAX(NS,NT),A,U,W,V)
 
 !## truncate all zero and get inverse of singular values
 IF(.NOT.LTEST)THEN
  !## truncate for non-zeros
  AM_TRUNCATED=0; DO I=1,SPACEDIM; IF(W(I).LE.0.0001D0)EXIT; AM_TRUNCATED=AM_TRUNCATED+1; ENDDO
  
  IF(ALLOCATED(AM))DEALLOCATE(AM); ALLOCATE(AM(AM_TRUNCATED,SPACEDIM)); AM=0.0D0

  DO I=1,SPACEDIM; DO J=1,AM_TRUNCATED
   IF(W(J).GT.0.0D0)AM(J,I)=U(J,I)*1.0D0/W(J)
  ENDDO; ENDDO

 ENDIF
 
 IF(ALLOCATED(U))DEALLOCATE(U); IF(ALLOCATED(W))DEALLOCATE(W)
 IF(ALLOCATED(V))DEALLOCATE(V); IF(ALLOCATED(A))DEALLOCATE(A)

 END SUBROUTINE IPEST_IES_COMPUTE_AM

 !#####=================================================================
 SUBROUTINE IPEST_IES_COMPUTE_GETREALS(DIR,IPARAM,TIMEDIM,SPACEDIM,REALS,ITER) 
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER,INTENT(IN) :: IPARAM,TIMEDIM,SPACEDIM,ITER
 REAL(KIND=DP_KIND),DIMENSION(TIMEDIM,SPACEDIM) :: REALS
 INTEGER :: J,K,IROW,ICOL
 CHARACTER(LEN=256) :: FNAME
 
 !## compute delta-m
 REALS=0.0D0
 DO J=1,TIMEDIM
  CALL IPEST_IES_GETENSEMBLENAME(ITER,DIR,PEST%PARAM(IPARAM)%REALSFNAME(J),FNAME)
  IF(.NOT.IDFREADSCALE(FNAME,PRJIDF,10,0,1.0D0,0))RETURN
  K=0; DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
   K=K+1
   REALS(J,K)=PRJIDF%X(ICOL,IROW)
   IF(KLOG)REALS(J,K)=LOG(REALS(J,K))
!   IF(KLOG)REALS(J,K)=LOG10(REALS(J,K))
  ENDDO; ENDDO
  CALL IDFDEALLOCATEX(PRJIDF)
 ENDDO
 
 END SUBROUTINE IPEST_IES_COMPUTE_GETREALS
 
 !#####=================================================================
 SUBROUTINE IPEST_IES_COMPUTE_DM(DIR,IPARAM,TIMEDIM,SPACEDIM,REALS,DM,ITER) 
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER,INTENT(INOUT) :: ITER
 INTEGER,INTENT(IN) :: IPARAM,TIMEDIM,SPACEDIM
 REAL(KIND=DP_KIND),DIMENSION(TIMEDIM,SPACEDIM) :: DM,REALS
 INTEGER :: J,K
 REAL(KIND=DP_KIND) :: RN,XSUM
 
 !## read realisatons
 CALL IPEST_IES_COMPUTE_GETREALS(DIR,IPARAM,TIMEDIM,SPACEDIM,REALS,ITER) 
 
 !## compute delta-m
 RN=SQRT(DBLE(TIMEDIM)-1.0D0)

 DO K=1,SPACEDIM
  XSUM=0.0D0; DO J=1,TIMEDIM; XSUM=XSUM+REALS(J,K); ENDDO; XSUM=XSUM/DBLE(TIMEDIM)
  DO J=1,TIMEDIM
   REALS(J,K)=(REALS(J,K)-XSUM)/RN
  ENDDO
 ENDDO

! IF(.NOT.IDFALLOCATEX(PRJIDF))STOP
! DO J=1,TIMEDIM
!  K=0; DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
!   K=K+1; PRJIDF%X(ICOL,IROW)=REALS(J,K)
!   IF(KLOG)PRJIDF%X(ICOL,IROW)=10.0D0**(PRJIDF%X(ICOL,IROW))
!  ENDDO; ENDDO
!  PRJIDF%FNAME=TRIM(DIR)//'ENSEMBLES\VAR_ENSEMBLE'//TRIM(ITOS(J))//'.IDF'; IF(.NOT.IDFWRITE(PRJIDF,PRJIDF%FNAME,1))STOP
! ENDDO
 
 CALL UTL_MATMUL(PEST%PARAM(IPARAM)%ISQRTCOV,REALS,DM)

 !## read realisatons again
 CALL IPEST_IES_COMPUTE_GETREALS(DIR,IPARAM,TIMEDIM,SPACEDIM,REALS,ITER) 

 END SUBROUTINE IPEST_IES_COMPUTE_DM
 
 !#####=================================================================
 SUBROUTINE IPEST_IES_SAVEREALS(DIR,ITER) 
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: ITER
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: I,J,K,ILAY,SCL_UP,SCL_D
 CHARACTER(LEN=256) :: FNAME
 
 !## read idf
 DO I=1,PEST%NREALS
 
  DO J=1,SIZE(PEST%PARAM)
   DO K=1,SIZE(PEST%PARAM(J)%ILS)
    ILAY=PEST%PARAM(J)%ILS(K)
    IF(ILAY.LE.0.OR.ILAY.GT.PRJNLAY)CYCLE
    
    CALL IPEST_IES_GETENSEMBLENAME(ITER,DIR,PEST%PARAM(J)%REALSFNAME(I),FNAME)
    
    SELECT CASE (PEST%PARAM(J)%PPARAM)
     CASE ('KH')
 
      !## it is not allowed to scale here as the covariance matrix does not scale with it
      SCL_UP=10; SCL_D=0      
      
      !## read/clip/scale idf file
      IF(.NOT.IDFREADSCALE(FNAME,PRJIDF,SCL_UP,SCL_D,1.0D0,0))RETURN
      !## save array, do not correct for boundary condition as we not yet know for what layer the zone will apply
      IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\MODELINPUT\LPF7\HK_L'//TRIM(ITOS(ILAY))//'_R#'//TRIM(ITOS(I))//'.ARR',PRJIDF,0,0,1,0))RETURN

    END SELECT
   ENDDO
  ENDDO
 ENDDO
 
 END SUBROUTINE IPEST_IES_SAVEREALS
 
 !#####=================================================================
 SUBROUTINE IPEST_IES_GETENSEMBLENAME(ITER,DIR,FNAME_IN,FNAME_OUT)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER
 CHARACTER(LEN=*),INTENT(IN) :: FNAME_IN,DIR
 CHARACTER(LEN=*),INTENT(OUT) :: FNAME_OUT

 IF(ITER.EQ.0)THEN
  FNAME_OUT=FNAME_IN
 ELSE
  FNAME_OUT=FNAME_IN(INDEX(FNAME_IN,'\',.TRUE.)+1:)
  FNAME_OUT=FNAME_OUT(:INDEX(FNAME_OUT,'.',.TRUE.)-1)//'_ITER'//TRIM(ITOS(ITER))//'.IDF'
  FNAME_OUT=TRIM(DIR)//'\ENSEMBLES\'//TRIM(FNAME_OUT)
 ENDIF

 END SUBROUTINE IPEST_IES_GETENSEMBLENAME
 
 !#####=================================================================
 SUBROUTINE IPEST_IES_PROGRESS(ITER,IGRAD)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER,IGRAD
 
 WRITE(IUPESTOUT,'(2(I5,1X),2(F15.7,1X))') ITER,IGRAD,MSR%TJ
 JE(IGRAD,ITER)=MSR%TJ
 FLUSH(IUPESTOUT)
 
 END SUBROUTINE IPEST_IES_PROGRESS
 
 !#####=================================================================
 LOGICAL FUNCTION IPEST_IES_ALLOCATE(DIR)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: I,N,M,IU
 
 IPEST_IES_ALLOCATE=.FALSE.
 
 PEST%PE_REGULARISATION=0
 
 CALL IPEST_IES_DEALLOCATE()
 N=PEST%NREALS; ALLOCATE(RNG(N),IPROC(2,N),GPARAM(N),ISTATUS(N))
 
 ALLOCATE(JE(N,0:PEST%PE_MXITER)); JE=0.0D0
 
 !## set realisation runbatch-files
 DO I=1,PEST%NREALS; RNG(I)=TRIM(DIR)//'\RUN_R#'//TRIM(ITOS(I))//'.BAT'; GPARAM(I)=I; ENDDO

 !## open files and get total maximum number of observations
 N=0; DO I=1,SIZE(PEST%MEASURES)
  IU=UTL_GETUNIT(); OPEN(IU,FILE=PEST%MEASURES(I)%IPFNAME,STATUS='OLD',ACTION='READ')
  READ(IU,*) M; N=N+M; CLOSE(IU)
 ENDDO

 IF(.NOT.IPEST_GLM_ALLOCATEMSR(N))RETURN
 
 !## open output files
 CALL UTL_CREATEDIR(TRIM(DIR)//'\IIES')
 IUPESTOUT=UTL_GETUNIT();         OPEN(IUPESTOUT,        FILE=TRIM(DIR)//'\IIES\LOG_PEST.TXT'            ,STATUS='UNKNOWN',ACTION='WRITE')
 IUPESTPROGRESS=UTL_GETUNIT();    OPEN(IUPESTPROGRESS,   FILE=TRIM(DIR)//'\IIES\LOG_PEST_PROGRESS.TXT'   ,STATUS='UNKNOWN',ACTION='WRITE')
 WRITE(IUPESTPROGRESS,'(A10,5(1X,A15))') 'ITER','CURRENT_TJ','PREVIOUS_PJ','AVG-RESIDUAL','IMPROVEMENT(%)','LAMBDA'

 IPEST_IES_ALLOCATE=.TRUE.
 
 END FUNCTION IPEST_IES_ALLOCATE

 !#####=================================================================
 SUBROUTINE IPEST_IES_DEALLOCATE()
 !#####=================================================================
 IMPLICIT NONE

 IF(ALLOCATED(RNG))    DEALLOCATE(RNG)
 IF(ALLOCATED(IPROC))  DEALLOCATE(IPROC)
 IF(ALLOCATED(GPARAM)) DEALLOCATE(GPARAM)
 IF(ALLOCATED(ISTATUS))DEALLOCATE(ISTATUS)
 IF(ALLOCATED(AM))     DEALLOCATE(AM)
 IF(ALLOCATED(DM))     DEALLOCATE(DM)
 IF(ALLOCATED(JE))     DEALLOCATE(JE)
 
 END SUBROUTINE IPEST_IES_DEALLOCATE
 
END MODULE MOD_IPEST_IES