MODULE MOD_IPEST_IES

USE WINTERACTER
USE MOD_IDF_PAR
!USE MOD_IDF, ONLY : IDFNULLIFY,IDFALLOCATEX,IDFWRITE
USE IMODVAR, ONLY : DP_KIND,IDPROC
USE MOD_PMANAGER_PAR, ONLY : PEST,PBMAN !SIM,PARAM,PRJNLAY,PBMAN,PRJNPER
USE MOD_UTL, ONLY  : UTL_GETUNIT,UTL_CAP,ITOS,RTOS,UTL_GETMED,UTL_CREATEDIR,UTL_GOODNESS_OF_FIT,UTL_NASH_SUTCLIFFE
USE MOD_IPEST_GLM_PAR
USE MOD_IPEST_GLM, ONLY : IPEST_GLM_GETJ

CONTAINS

 !#####=================================================================
 SUBROUTINE IPEST_IES_MAIN(DIR,MNAME,IBATCH)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),INTENT(IN) :: DIR,MNAME
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITER,IFLAGS,IGRAD,NCPU,NCSECS,I,ITYPE,JGRAD,IEXCOD,NDONE,N
 REAL(KIND=DP_KIND) :: F,DBLE,TNSC
 
 ITER=0
 DO
  !## next cycle
  ITER=ITER+1; IF(ITER.GT.PEST%PE_MXITER)EXIT

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

!    !## adjust alpha for current igrad
!    CALL IPEST_GLM_NEXTGRAD(GPARAM(IGRAD),IGRAD)
!    !## define update in pst file
!    IF(.NOT.IPEST_GLM_PST(DIR,MNAME,IGRAD,GPARAM(IGRAD),'P'))THEN
!     IF(IBATCH.EQ.1)WRITE(*,'(/A/)') 'ERROR CREATED PST1 FILE FOR P#'//TRIM(ITOS(GPARAM(IGRAD)))
!     IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'ERROR CREATED PST1 FILE FOR P#'//TRIM(ITOS(GPARAM(IGRAD))),'Error')
!     RETURN
!    ENDIF

    !## wait before starting a new process
    IF(PBMAN%NSWAIT.GT.0)CALL IOSWAIT(PBMAN%NSWAIT)
    !## clear error
    I=WINFOERROR(1); IDPROC=0; CALL IOSCOMMAND(TRIM(RNG(IGRAD)),IFLAGS=IFLAGS,IDPROC=IDPROC); IPROC(:,IGRAD)=IDPROC
    IF(WINFOERROR(1).EQ.ERROSCOMMAND)THEN
     IF(IBATCH.EQ.1)WRITE(*,'(/A/)') 'FAILED TO START MODEL P#'//TRIM(ITOS(GPARAM(IGRAD)))
     IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'FAILED TO START MODEL P#'//TRIM(ITOS(GPARAM(IGRAD))),'Error')
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
        IF(.NOT.IPEST_GLM_GETJ(DIR,JGRAD,GPARAM(JGRAD),'P',IBATCH,TNSC))RETURN
!        !## write echo
!        CALL IPEST_GLM_PROGRESS(ITER,JGRAD,0,'P')
!        FLUSH(IUPESTPROGRESS)
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
  
!  !## determine new gradient
!  IF(PBMAN%MC.EQ.0)THEN
!   IF(.NOT.IPEST_GLM_GRADIENT(ITER,IBATCH))EXIT
!  ELSE
!   IF(.NOT.IPEST_GLM_MONTECARLO(ITER))EXIT
!  ENDIF

 ENDDO
 CALL WMESSAGETIMER(0); CALL WMESSAGEENABLE(TIMEREXPIRED,0)

 END SUBROUTINE IPEST_IES_MAIN

END MODULE MOD_IPEST_IES