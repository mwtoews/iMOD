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
USE MOD_IDF, ONLY : IDFREADSCALE,IDFALLOCATEX,IDFNULLIFY,IDFWRITE,IDFDEALLOCATEX
USE IMODVAR, ONLY : DP_KIND,IDPROC
USE MOD_PMANAGER_PAR, ONLY : PEST,PBMAN,PRJIDF,PRJNLAY !SIM,PARAM,PRJNPER
USE MOD_UTL, ONLY  : UTL_GETUNIT,UTL_CAP,ITOS,RTOS,UTL_GETMED,UTL_CREATEDIR,UTL_GOODNESS_OF_FIT,UTL_NASH_SUTCLIFFE,UTL_STDEF
USE MOD_IPEST_GLM_PAR
USE MOD_IPEST_GLM, ONLY : IPEST_GLM_GETJ,IPEST_GLM_ALLOCATEMSR
USE MOD_PMANAGER_UTL, ONLY : PMANAGER_SAVEMF2005_MOD_U2DREL,PMANAGER_SAVEMF2005_DEALLOCATE
USE MOD_OSD
USE MOD_IPEST_IES_PAR
USE MOD_LUDCMP

CONTAINS

 !#####=================================================================
 SUBROUTINE IPEST_IES_MAIN(DIR,MNAME,IBATCH)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),INTENT(IN) :: DIR,MNAME
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITER,IFLAGS,IGRAD,NCPU,I,ITYPE,JGRAD,IEXCOD,NDONE,N
 REAL(KIND=DP_KIND) :: F,DBLE,TNSC
  
 IF(.NOT.IPEST_IES_ALLOCATE(DIR))RETURN 

 CALL WMESSAGEENABLE(TIMEREXPIRED,1); MSR%PJ=HUGE(1.0D0); ITER=0
 DO WHILE (ITER.LE.PEST%PE_MXITER)

  !## read and save the realisations
  CALL IPEST_IES_SAVEREALS(DIR,ITER) 

  !## executes on commandtool such that commands alike 'dir' etc. works
  IFLAGS=0; IF(PBMAN%CMDHIDE.EQ.1)IFLAGS=IFLAGS+PROCSILENT+PROCCMDPROC

  !## start finite difference approximation for parameters
  CALL WMESSAGETIMER(NCSECS,IREPEAT=1); IGRAD=0; NCPU=0; IPROC=0

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
        CALL IPEST_IES_PROGRESS(ITER,JGRAD); FLUSH(IUPESTPROGRESS)
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
  IF(.NOT.IPEST_IES_UPDATE_ENSEMBLES(ITER))EXIT
  
  !## next cycle
  ITER=ITER+1

 ENDDO
 CALL WMESSAGETIMER(0); CALL WMESSAGEENABLE(TIMEREXPIRED,0)
 PBMAN%IIES=0; CALL PMANAGER_SAVEMF2005_DEALLOCATE(); CALL IPEST_IES_DEALLOCATE
 
 END SUBROUTINE IPEST_IES_MAIN

 !#####=================================================================
 LOGICAL FUNCTION IPEST_IES_UPDATE_ENSEMBLES(ITER) 
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: ITER
 INTEGER :: I,II,J,K,IU,N,SPACEDIM,TIMEDIM,OBSDIM,IROW,ICOL,IX
 TYPE(IDFOBJ) :: IDF
 REAL(KIND=DP_KIND) :: X,VAR,A,L,TL,GAMMA
 REAL(KIND=DP_KIND),SAVE :: LAMBDA
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: DM !,TMP1,TMP2,TMP3
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: SO,TMP1
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: REALS
 LOGICAL :: LTEST
 CHARACTER(LEN=256) :: FNAME
 
! ALLOCATE(TMP1(2,2),TMP2(1,2),TMP3(1,2)); TMP1=0.0D0; TMP2=0.0D0; TMP3=0.0D0
! TMP1(1,1)=5.0D0
! TMP1(2,1)=6.0D0
! TMP1(1,2)=3.0D0
! TMP1(2,2)=2.0D0
! TMP2(1,1)=1.0D0
! TMP2(1,2)=8.0D0
! CALL UTL_MATMUL(TMP1,TMP2,TMP3)
! DEALLOCATE(TMP2,TMP3); ALLOCATE(TMP2(2,2),TMP3(2,2)); TMP2=0.0D0; TMP3=0.0D0
! TMP2(1,1)=1.0D0
! TMP2(2,1)=3.0D0
! TMP2(1,2)=8.0D0
! TMP2(2,2)=2.0D0
! CALL UTL_MATMUL(TMP1,TMP2,TMP3)
! DEALLOCATE(TMP1,TMP2,TMP3); ALLOCATE(TMP1(2,1),TMP2(1,2),TMP3(1,1)); TMP1=0.0D0; TMP2=0.0D0; TMP3=0.0D0
! TMP1(1,1)=1.0D0
! TMP1(2,1)=8.0D0
! TMP2(1,1)=3.0D0
! TMP2(1,2)=7.0D0
! CALL UTL_MATMUL(TMP1,TMP2,TMP3)
! DEALLOCATE(TMP1,TMP2,TMP3); ALLOCATE(TMP1(1,2),TMP2(2,1),TMP3(2,2)); TMP1=0.0D0; TMP2=0.0D0; TMP3=0.0D0
! TMP1(1,1)=1.0D0
! TMP1(1,2)=8.0D0
! TMP2(1,1)=3.0D0
! TMP2(2,1)=7.0D0
! CALL UTL_MATMUL(TMP1,TMP2,TMP3)
 
 LTEST=.FALSE.
 IPEST_IES_UPDATE_ENSEMBLES=.TRUE.

 SPACEDIM=PRJIDF%NROW*PRJIDF%NCOL
 TIMEDIM=PEST%NREALS
 OBSDIM=MSR%NOBS
 
 !## get residual objective function value
 ALLOCATE(SO(TIMEDIM)); SO=0.0D0
 !## use weigth (get variance from weights) - get matrix of objective function values
 DO J=1,TIMEDIM; DO K=1,OBSDIM
  VAR=1.0D0/MSR%W(K); SO(J)=SO(J)+MSR%DHG(J,K)**2.0D0*(1.0D0/VAR)
 ENDDO; ENDDO

 !## get current sum of objective function values
 MSR%TJ=SUM(SO)

 !## determine initial lambda
 IF(ITER.EQ.0)THEN
  X=MSR%TJ/DBLE(2.0D0*OBSDIM); X=LOG10(X); IX=FLOOR(X); LAMBDA=10.0**IX
 ENDIF
 
 A=LAMBDA+1.0D0

 GAMMA=4.0D0

 !## evaluate progression
 IF(ITER.GT.0)THEN

  !## do something with statistics, mean and stdev
  WRITE(IUPESTPROGRESS,'(I10,4(1X,F15.7))') ITER,MSR%TJ,MSR%PJ,1.0D0-MSR%TJ/MSR%PJ,LAMBDA
  DO J=1,SIZE(PEST%PARAM); DO I=1,TIMEDIM
   WRITE(IUPESTPROGRESS,'(2I10,2F15.7)') I,J,PEST%PARAM(J)%MEAN(I),PEST%PARAM(J)%VAR(I)
  ENDDO; ENDDO
  IF(MSR%TJ.LE.MSR%PJ)THEN
   !## too less improvement
   IF(1.0D0-MSR%TJ/MSR%PJ.LT.PEST%PE_STOP)THEN
    IPEST_IES_UPDATE_ENSEMBLES=.FALSE.; RETURN
   ELSE
    !## YES!!! Continue to next iteration
    LAMBDA=LAMBDA/GAMMA
    !## save previous objective function value
    MSR%PJ=MSR%TJ
   ENDIF
  ELSE
   LAMBDA=LAMBDA*GAMMA
   !## reset ensembles to previous iteration
   ITER=ITER-1
  ENDIF
 ENDIF

 !## processing the inverse of the prior parameter covariance matrix
 TL=0.0D0; DO I=1,SIZE(PEST%PARAM)

  !## initialisation
  IF(ITER.EQ.0)THEN
   IF(.NOT.ASSOCIATED(PEST%PARAM(I)%SQRTCOV))THEN
    ALLOCATE(PEST%PARAM(I)%MEAN(TIMEDIM)); PEST%PARAM(I)%MEAN=0.0D0
    ALLOCATE(PEST%PARAM(I)%VAR(TIMEDIM)); PEST%PARAM(I)%VAR=0.0D0
    IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=PEST%PARAM(I)%ICOVFNAME,STATUS='OLD',ACTION='READ')
    READ(IU,*); READ(IU,*) N; ALLOCATE(PEST%PARAM(I)%COV(N,N))  
    DO J=1,N; READ(IU,*) (PEST%PARAM(I)%COV(J,K),K=1,N); ENDDO; CLOSE(IU)
    !## save as IDF (for fun)
    CALL IDFNULLIFY(IDF)
    IDF%DX=1.0D0; IDF%DY=1.0D0; IDF%NCOL=N; IDF%NROW=N
    IDF%XMIN=0.0D0; IDF%XMAX=IDF%XMIN+IDF%NCOL*IDF%DX
    IDF%YMIN=0.0D0; IDF%YMAX=IDF%YMIN+IDF%NROW*IDF%DY
    IF(.NOT.IDFALLOCATEX(IDF))RETURN
    DO J=1,N; DO K=1,N; IDF%X(J,K)=PEST%PARAM(I)%COV(J,K); ENDDO; ENDDO
    IDF%FNAME='d:\IMOD-MODELS\IES\CREATEENSEMBLES\COV.IDF'; IF(.NOT.IDFWRITE(IDF,IDF%FNAME,1))STOP
    CALL IDFDEALLOCATEX(IDF)
   
    !## used for testing sqrt-root inverse
    IF(LTEST)THEN
     N=2
     DEALLOCATE(PEST%PARAM(I)%COV); ALLOCATE(PEST%PARAM(I)%COV(N,N))
     PEST%PARAM(I)%COV(1,1)=5.0
     PEST%PARAM(I)%COV(2,1)=4.0
     PEST%PARAM(I)%COV(1,2)=4.0
     PEST%PARAM(I)%COV(2,2)=8.0
    !## used for testing
    ENDIF
    ALLOCATE(PEST%PARAM(I)%ISQRTCOV(N,N),PEST%PARAM(I)%SQRTCOV(N,N))
    CALL LUDCMP_CALC_SQRTROOTINVERSE(N,PEST%PARAM(I)%COV,PEST%PARAM(I)%ISQRTCOV,PEST%PARAM(I)%SQRTCOV)
    IF(.NOT.LTEST)THEN
     IF(.NOT.IDFALLOCATEX(IDF))RETURN
     DO J=1,N; DO K=1,N; IDF%X(J,K)=PEST%PARAM(I)%ISQRTCOV(J,K); ENDDO; ENDDO
     IDF%FNAME='d:\IMOD-MODELS\IES\CREATEENSEMBLES\ISQRTCOV.IDF'; IF(.NOT.IDFWRITE(IDF,IDF%FNAME,1))STOP
     CALL IDFDEALLOCATEX(IDF)
    ENDIF
    DEALLOCATE(PEST%PARAM(I)%COV)
   ENDIF
  ENDIF
  
  !## compute dm-matrix
  ALLOCATE(REALS(TIMEDIM,SPACEDIM),DM(TIMEDIM,SPACEDIM))
  CALL IPEST_IES_COMPUTE_DM(I,TIMEDIM,SPACEDIM,REALS,DM,ITER) 

  !## store prior-estimated parameters
  IF(ITER.EQ.0)THEN
   !## might be coming here twice as improvement of objective function is not enough
   IF(.NOT.ALLOCATED(MPR))THEN
    ALLOCATE(MPR(TIMEDIM,SPACEDIM)); MPR=REALS
    !## compute am-matrix initially through svd
!    IF(.NOT.ALLOCATED(AM))ALLOCATE(AM(SPACEDIM,SPACEDIM)); AM=0.0D0
    CALL IPEST_IES_COMPUTE_AM(TIMEDIM,SPACEDIM,DM)
   ENDIF
  ENDIF
  
  !## processing the inverse of the prior parameter covariance matrix
  CALL IPEST_IES_COMPUTE_PARTIAL_M(ITER,I,TIMEDIM,SPACEDIM,OBSDIM,DM,A,MPR,REALS,L) 
  TL=TL+L
  
  !# save the updates for the next iteration and declare to be done
  IF(.NOT.IDFALLOCATEX(PRJIDF))STOP
  ALLOCATE(TMP1(SPACEDIM))
  DO J=1,SIZE(PEST%PARAM)
   DO K=1,TIMEDIM
    II=0; DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL; II=II+1
     PRJIDF%X(ICOL,IROW)=REALS(K,II); TMP1(II)=REALS(K,II)
    ENDDO; ENDDO
    !## get statistics
    CALL UTL_STDEF(TMP1,SPACEDIM,PRJIDF%NODATA,PEST%PARAM(J)%VAR(K),PEST%PARAM(J)%MEAN(K),N)
    FNAME=PEST%PARAM(J)%REALSFNAME(K); FNAME=FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'_ITER'//TRIM(ITOS(ITER+1))//'.IDF'
    IF(.NOT.IDFWRITE(PRJIDF,FNAME,1))STOP
   ENDDO
  ENDDO
  CALL IDFDEALLOCATEX(PRJIDF)
 
 ENDDO

 !## possible update vector is less that criterion
 WRITE(IUPESTPROGRESS,'(A,F15.7)') 'TL=',TL
 IF(TL.LT.PEST%PE_PADJ)IPEST_IES_UPDATE_ENSEMBLES=.FALSE.
 
 DEALLOCATE(SO,DM)
 
 END FUNCTION IPEST_IES_UPDATE_ENSEMBLES
 
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
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: V,U 
 INTEGER :: I,J,MAXDIM
 
 MAXDIM=MAX(TIMEDIM,OBSDIM)
 ALLOCATE(X1(TIMEDIM,OBSDIM),DD(TIMEDIM,OBSDIM),X2(OBSDIM,OBSDIM))
 
 RN=SQRT(DBLE(TIMEDIM)-1.0D0)
 DO I=1,OBSDIM; XSUM=SUM(MSR%DHG(I,:))/DBLE(TIMEDIM)
  DO J=1,TIMEDIM; X1(J,I)=(MSR%DHG(J,I)-XSUM)/RN; ENDDO
 ENDDO
 X2=0.0D0; DO I=1,OBSDIM; VAR=MSR%W(I); X2(I,I)=SQRT(1.0D0/VAR); ENDDO
 !## dd is [timedim,obsdim]
 CALL UTL_MATMUL(X2,X1,DD); DEALLOCATE(X1)
! DD=MATMUL(X2,X1); DEALLOCATE(X1)

 ALLOCATE(W(MAXDIM),V(OBSDIM,OBSDIM),U(TIMEDIM,OBSDIM))
 U=DD; CALL LUDCMP_SVD_MAIN(OBSDIM,TIMEDIM,MAXDIM,U,W,V)
 !## UT=
 
 !## truncate for non-zeros
 DD_TRUNCATED=0; DO I=1,MAXDIM; IF(W(I).LE.0.0001D0)EXIT; DD_TRUNCATED=DD_TRUNCATED+1; ENDDO
 
 !## compute x3 as UTC=-1/2
 ALLOCATE(X1(OBSDIM,DD_TRUNCATED),X3(OBSDIM,DD_TRUNCATED)); X1=0.0D0
! ALLOCATE(X1(OBSDIM,OBSDIM),X3(OBSDIM,OBSDIM)); X1=0.0D0
 DO I=1,OBSDIM; DO J=1,DD_TRUNCATED; X1(I,J)=V(J,I); ENDDO; ENDDO
! DO I=1,OBSDIM; DO J=1,OBSDIM; X1(I,J)=V(J,I); ENDDO; ENDDO

 !## x3=[obsdim,obsdim]
 CALL UTL_MATMUL(X1,X2,X3)
! X3=MATMUL(X1,X2)

 DEALLOCATE(X1); ALLOCATE(X1(TIMEDIM,DD_TRUNCATED))
 !## x1=[timedim,obsdim]
 CALL UTL_MATMUL(X3,MSR%DHG,X1)
! X1=MATMUL(X3,MSR%DHG)
 
! DEALLOCATE(X3); ALLOCATE(X3(MAXDIM,MAXDIM)); X3=0.0D0
 DEALLOCATE(X3); ALLOCATE(X3(DD_TRUNCATED,DD_TRUNCATED)); X3=0.0D0
 DO I=1,DD_TRUNCATED; X3(I,I)=1.0D0/(A+W(I)**2.0D0); ENDDO
! DO I=1,MAXDIM; X3(I,I)=1.0D0/(A+W(I)**2.0D0); ENDDO
 !## x2=[timedim,obsdim]
 DEALLOCATE(X2); ALLOCATE(X2(TIMEDIM,DD_TRUNCATED))
! DEALLOCATE(X2); ALLOCATE(X2(TIMEDIM,OBSDIM))
 CALL UTL_MATMUL(X3,X1,X2)
! X2=MATMUL(X3,X1)
 
 DEALLOCATE(X3); ALLOCATE(X3(DD_TRUNCATED,DD_TRUNCATED)); X3=0.0D0
 DO I=1,DD_TRUNCATED; X3(I,I)=W(I); ENDDO
! DO I=1,MAXDIM; X3(I,I)=W(I); ENDDO
 ALLOCATE(X4(DD_TRUNCATED,OBSDIM))
! ALLOCATE(X4(OBSDIM,TIMEDIM))
 DO I=1,DD_TRUNCATED; DO J=1,OBSDIM; X4(I,J)=U(J,I); ENDDO; ENDDO
! DO I=1,OBSDIM; DO J=1,TIMEDIM; X4(I,J)=U(J,I); ENDDO; ENDDO
 ALLOCATE(X5(DD_TRUNCATED,OBSDIM))
! ALLOCATE(X5(OBSDIM,TIMEDIM))
 CALL UTL_MATMUL(X4,X3,X5)
! X5=MATMUL(X4,X3)
 DEALLOCATE(X3,X4)
 ALLOCATE(X3(OBSDIM,TIMEDIM))
 CALL UTL_MATMUL(X5,X2,X3)
! X3=MATMUL(X5,X2)
 
 DEALLOCATE(X1,X2,X5)
 
 ALLOCATE(X1(TIMEDIM,SPACEDIM))
 !## wrong
 CALL UTL_MATMUL(-PEST%PARAM(IPARAM)%SQRTCOV,DM,X1)
! X1=MATMUL(-PEST%PARAM(IPARAM)%SQRTCOV,DM)
 ALLOCATE(PM1(TIMEDIM,SPACEDIM))
  
 CALL UTL_MATMUL(X1,X3,PM1); DEALLOCATE(X1,X3)
! PM1=MATMUL(X1,X3); DEALLOCATE(X1,X3)

 IF(ITER.GT.0)THEN
!  ALLOCATE(X1(SPACEDIM,SPACEDIM)); X1=0.0D0
  !## am is filled with zero ... save truncation am somewhere
  ALLOCATE(X1(SPACEDIM,AM_TRUNCATED)); X1=0.0D0
  DO I=1,SPACEDIM; DO J=1,AM_TRUNCATED; X1(I,J)=AM(J,I); ENDDO; ENDDO

  ALLOCATE(X2(SPACEDIM,AM_TRUNCATED))
!  ALLOCATE(X2(SPACEDIM,SPACEDIM))
  CALL UTL_MATMUL(X1,PEST%PARAM(IPARAM)%ISQRTCOV,X2)
!  X2=MATMUL(X1,PEST%PARAM(IPARAM)%ISQRTCOV)
  DEALLOCATE(X1)
  ALLOCATE(X4(TIMEDIM,AM_TRUNCATED),X1(TIMEDIM,SPACEDIM))
!  ALLOCATE(X4(TIMEDIM,SPACEDIM),X1(TIMEDIM,SPACEDIM))
  DO I=1,TIMEDIM; DO J=1,SPACEDIM; X1(I,J)=REALS(I,J)-MPR(I,J); ENDDO; ENDDO
  CALL UTL_MATMUL(X2,X1,X4)
!  X4=MATMUL(X2,MPR)
  DEALLOCATE(X1,X2)
  ALLOCATE(X5(TIMEDIM,SPACEDIM))
  CALL UTL_MATMUL(AM,X4,X5)
!  X5=MATMUL(AM,X4)
  DEALLOCATE(X4)
  ALLOCATE(X4(SPACEDIM,TIMEDIM))
  DO I=1,SPACEDIM; DO J=1,TIMEDIM; X4(I,J)=DM(J,I); ENDDO; ENDDO
  ALLOCATE(X6(TIMEDIM,TIMEDIM))
  CALL UTL_MATMUL(X4,X5,X6)
!  X6=MATMUL(X4,X5)
  DEALLOCATE(X4,X5)

  ALLOCATE(X1(OBSDIM,OBSDIM)); X1=0.0D0
  DO I=1,TIMEDIM; IF(W(I).GT.0.0D0)X1(I,I)=1.0D0/(A+W(I)**2.0D0); ENDDO
  ALLOCATE(X2(TIMEDIM,OBSDIM))
  CALL UTL_MATMUL(U,X1,X2)
!  X2=MATMUL(U,X1)
  DEALLOCATE(X1)
  ALLOCATE(X1(OBSDIM,TIMEDIM))
  DO I=1,OBSDIM; DO J=1,TIMEDIM; X1(I,J)=U(J,I); ENDDO; ENDDO
  ALLOCATE(X3(OBSDIM,TIMEDIM))
  CALL UTL_MATMUL(X2,X1,X3)
!  X3=MATMUL(X2,X1)
  DEALLOCATE(X2,X1)
  ALLOCATE(X1(TIMEDIM,TIMEDIM))
  CALL UTL_MATMUL(X3,U,X1)
!  X1=MATMUL(X3,U)
  DEALLOCATE(X3)
  ALLOCATE(X7(TIMEDIM,TIMEDIM))
  CALL UTL_MATMUL(X1,X6,X7)
!  X7=MATMUL(X1,X6)
  DEALLOCATE(X1,X6)
  ALLOCATE(X1(TIMEDIM,SPACEDIM))
  CALL UTL_MATMUL(-PEST%PARAM(IPARAM)%SQRTCOV,DM,X1)
!  X1=MATMUL(-PEST%PARAM(IPARAM)%SQRTCOV,DM)
  ALLOCATE(PM2(TIMEDIM,SPACEDIM))
  CALL UTL_MATMUL(X1,X7,PM2) 
!  PM2=MATMUL(X1,X7) 
  DEALLOCATE(X1,X7)
 ENDIF
 
 ALLOCATE(X1(TIMEDIM,SPACEDIM))
 X1=REALS+PM1; IF(ITER.GT.0)X1=X1+PM2
 DEALLOCATE(PM1); IF(ALLOCATED(PM2))DEALLOCATE(PM2)

 !## check update
 DO I=1,TIMEDIM; DO J=1,SPACEDIM; L=L+(X1(I,J)-REALS(I,J))**2.0D0; ENDDO; ENDDO; L=SQRT(L)
 
 !## copy update to reals
 REALS=X1
 
 DEALLOCATE(X1)
 
 END SUBROUTINE IPEST_IES_COMPUTE_PARTIAL_M
 
 !#####=================================================================
 SUBROUTINE IPEST_IES_COMPUTE_AM(TIMEDIM,SPACEDIM,DM) 
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: TIMEDIM,SPACEDIM
 REAL(KIND=DP_KIND),DIMENSION(TIMEDIM,SPACEDIM),INTENT(IN) :: DM
 INTEGER :: I,J,NT,NS,NTRUNCATED
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: W
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: V,U
 LOGICAL :: LTEST
 
 LTEST=.FALSE.

 IF(LTEST)THEN
  NT=2; NS=4
 ELSE
  NT=TIMEDIM; NS=SPACEDIM
 ENDIF
 
 ALLOCATE(W(MAX(NT,NS)),V(NS,NS),U(NT,NS))
 IF(.NOT.LTEST)THEN
  U=DM
 ELSE
  U=0.0D0
!  U(1,1)=2.0D0
!  U(2,1)=4.0D0
!  U(1,2)=1.0D0
!  U(2,2)=3.0D0
 ENDIF
 !## SVD - be aware V=U and U=V (nice!)
 CALL LUDCMP_SVD_MAIN(NS,NT,MAX(NS,NT),U,W,V)
! DO J=1,NT; W(J)=1.0D0/W(J); ENDDO
! AM=0.0D0; DO J=1,NS; DO K=1,NS; AM(J,K)=V(K,J)*W(J); ENDDO; ENDDO
 
 !## truncate all zero and get inverse of singular values
 IF(.NOT.LTEST)THEN
  !## get this more efficient
!  ALLOCATE(S(NS,NS)); S=0.0D0; DO I=1,NS; IF(W(I).GT.0.0D0)S(I,I)=1.0D0/W(I); ENDDO
!  CALL UTL_MATMUL(V,S,AM)
!  AM=0.0D0
  !## truncate for non-zeros
  AM_TRUNCATED=0; DO I=1,SPACEDIM; IF(W(I).LE.0.0001D0)EXIT; AM_TRUNCATED=AM_TRUNCATED+1; ENDDO
  
  ALLOCATE(AM(AM_TRUNCATED,SPACEDIM)); AM=0.0D0

  DO I=1,SPACEDIM; DO J=1,AM_TRUNCATED !SPACEDIM
   IF(W(J).GT.0.0D0)AM(J,I)=V(J,I)*1.0D0/W(J)
  ENDDO; ENDDO

 ENDIF
 
 IF(ALLOCATED(U))DEALLOCATE(U); IF(ALLOCATED(W))DEALLOCATE(W); IF(ALLOCATED(V))DEALLOCATE(V)

 END SUBROUTINE IPEST_IES_COMPUTE_AM

 !#####=================================================================
 SUBROUTINE IPEST_IES_COMPUTE_GETREALS(IPARAM,TIMEDIM,SPACEDIM,REALS,ITER) 
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPARAM,TIMEDIM,SPACEDIM,ITER
 REAL(KIND=DP_KIND),DIMENSION(TIMEDIM,SPACEDIM) :: REALS
 INTEGER :: J,K,IROW,ICOL
 CHARACTER(LEN=256) :: FNAME
 
 !## compute delta-m
 REALS=0.0D0
 DO J=1,TIMEDIM
  IF(ITER.EQ.0)THEN
   FNAME=PEST%PARAM(IPARAM)%REALSFNAME(J)
  ELSE
   FNAME=PEST%PARAM(IPARAM)%REALSFNAME(J); FNAME=FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'_ITER'//TRIM(ITOS(ITER))//'.IDF'
  ENDIF
  IF(.NOT.IDFREADSCALE(FNAME,PRJIDF,10,0,1.0D0,0))RETURN
  K=0; DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL; K=K+1; REALS(J,K)=PRJIDF%X(ICOL,IROW); ENDDO; ENDDO
  CALL IDFDEALLOCATEX(PRJIDF)
 ENDDO
 
 END SUBROUTINE IPEST_IES_COMPUTE_GETREALS
 
 !#####=================================================================
 SUBROUTINE IPEST_IES_COMPUTE_DM(IPARAM,TIMEDIM,SPACEDIM,REALS,DM,ITER) 
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: ITER
 INTEGER,INTENT(IN) :: IPARAM,TIMEDIM,SPACEDIM
 REAL(KIND=DP_KIND),DIMENSION(TIMEDIM,SPACEDIM) :: DM,REALS
 INTEGER :: J,K,IROW,ICOL
 REAL(KIND=DP_KIND) :: RN,XSUM
 
 !## read realisatons
 CALL IPEST_IES_COMPUTE_GETREALS(IPARAM,TIMEDIM,SPACEDIM,REALS,ITER) 
 
 !## compute delta-m
 RN=SQRT(DBLE(TIMEDIM)-1.0D0)
 DO K=1,SPACEDIM
  XSUM=SUM(REALS(:,K))/DBLE(TIMEDIM)
  DO J=1,TIMEDIM
   REALS(J,K)=(REALS(J,K)-XSUM)/RN
  ENDDO
 ENDDO
 IF(.NOT.IDFALLOCATEX(PRJIDF))STOP
 DO J=1,TIMEDIM
  K=0; DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL; K=K+1; PRJIDF%X(ICOL,IROW)=REALS(J,K); ENDDO; ENDDO
  PRJIDF%FNAME='D:\IMOD-MODELS\IES\CREATEENSEMBLES\VAR_ENSEMBLE'//TRIM(ITOS(J))//'.IDF'; IF(.NOT.IDFWRITE(PRJIDF,PRJIDF%FNAME,1))STOP
 ENDDO
 
 CALL UTL_MATMUL(PEST%PARAM(IPARAM)%ISQRTCOV,REALS,DM)
! DM=MATMUL(PEST%PARAM(IPARAM)%ISQRTCOV,REALS)

 !## read realisatons again
 CALL IPEST_IES_COMPUTE_GETREALS(IPARAM,TIMEDIM,SPACEDIM,REALS,ITER) 

 END SUBROUTINE IPEST_IES_COMPUTE_DM
 
 !#####=================================================================
 SUBROUTINE UTL_MATMUL(A,B,C)
 !#####=================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),DIMENSION(:,:) :: A
 REAL(KIND=DP_KIND),DIMENSION(:,:) :: B
 REAL(KIND=DP_KIND),DIMENSION(:,:) :: C
 INTEGER :: NCA,NRA,NCB,NRB,NCC,NRC,I,J,K
 
 NCA=SIZE(A,1); NRA=SIZE(A,2)
 NCB=SIZE(B,1); NRB=SIZE(B,2)
 NCC=SIZE(C,1); NRC=SIZE(C,2)
 
 !## check
 IF(NCA.NE.NRB)THEN; WRITE(*,*) 'COLUMN DIMENSIONS ARRAY A NEED TO BE EQUAL TO ROWS   ARRAY B'; PAUSE; STOP; ENDIF
 IF(NRA.NE.NRC)THEN; WRITE(*,*) 'ROWS   DIMENSIONS ARRAY A NEED TO BE EQUAL TO ROWS   ARRAY C'; PAUSE; STOP; ENDIF
 IF(NCB.NE.NCC)THEN; WRITE(*,*) 'COLUMN DIMENSIONS ARRAY B NEED TO BE EQUAL TO COLUMN ARRAY C'; PAUSE; STOP; ENDIF

 C=0.0D0
 DO I=1,NCB; DO J=1,NRC
  DO K=1,NCA
   C(I,J)=C(I,J)+A(K,J)*B(I,K)
  ENDDO
 ENDDO; ENDDO
 
 END SUBROUTINE UTL_MATMUL
 
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
    SELECT CASE (PEST%PARAM(J)%PPARAM)
     CASE ('KH')
 
      !## it is not allowed to scale as the covariance matrix does not scale with it
      IF(ITER.EQ.0)THEN
       FNAME=PEST%PARAM(J)%REALSFNAME(I)
      ELSE
       FNAME=PEST%PARAM(J)%REALSFNAME(I); FNAME=FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'_ITER'//TRIM(ITOS(ITER))//'.IDF'
      ENDIF
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
 SUBROUTINE IPEST_IES_PROGRESS(ITER,IGRAD)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITER,IGRAD
 
 WRITE(IUPESTPROGRESS,'(2(I5,1X),2(F15.7,1X))') ITER,IGRAD,MSR%TJ !,MSR%TJ-MSR%PJ

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
 
 END SUBROUTINE IPEST_IES_DEALLOCATE
 
END MODULE MOD_IPEST_IES