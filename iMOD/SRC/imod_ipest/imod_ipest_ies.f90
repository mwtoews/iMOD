MODULE MOD_IPEST_IES

USE WINTERACTER
USE MOD_IDF_PAR
USE MOD_IDF, ONLY : IDFREADSCALE !IDFNULLIFY,IDFALLOCATEX,IDFWRITE
USE IMODVAR, ONLY : DP_KIND,IDPROC
USE MOD_PMANAGER_PAR, ONLY : PEST,PBMAN,PRJIDF,PRJNLAY !SIM,PARAM,PRJNPER
USE MOD_UTL, ONLY  : UTL_GETUNIT,UTL_CAP,ITOS,RTOS,UTL_GETMED,UTL_CREATEDIR,UTL_GOODNESS_OF_FIT,UTL_NASH_SUTCLIFFE
USE MOD_IPEST_GLM_PAR
USE MOD_IPEST_GLM, ONLY : IPEST_GLM_GETJ,IPEST_GLM_ALLOCATEMSR
USE MOD_PMANAGER_UTL, ONLY : PMANAGER_SAVEMF2005_MOD_U2DREL,PMANAGER_SAVEMF2005_DEALLOCATE

CONTAINS

 !#####=================================================================
 SUBROUTINE IPEST_IES_MAIN(DIR,MNAME,IBATCH)
 !#####=================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),INTENT(IN) :: DIR,MNAME
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITER,IFLAGS,IGRAD,NCPU,I,ITYPE,JGRAD,IEXCOD,NDONE,N,IU
 REAL(KIND=DP_KIND) :: F,DBLE,TNSC
  
 IF(.NOT.IPEST_IES_ALLOCATE(DIR))RETURN 

 CALL WMESSAGEENABLE(TIMEREXPIRED,1); MSR%PJ=HUGE(1.0D0); ITER=0
 DO WHILE (ITER.LE.PEST%PE_MXITER)

  !## next cycle
  ITER=ITER+1

  !## save the realisations
  CALL IPEST_IES_SAVEREALS(DIR) 

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
  
  !## get update of realisations
  CALL IPEST_IES_UPDATE_ENSEMBLES()
  
 ENDDO
 CALL WMESSAGETIMER(0); CALL WMESSAGEENABLE(TIMEREXPIRED,0)
 PBMAN%IIES=0; CALL PMANAGER_SAVEMF2005_DEALLOCATE(); CALL IPEST_IES_DEALLOCATE
 
 END SUBROUTINE IPEST_IES_MAIN

 !#####=================================================================
 SUBROUTINE IPEST_IES_UPDATE_ENSEMBLES() 
 !#####=================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,IU,N

 !## processing the inverse of the prior parameter covariance matrix
 DO I=1,SIZE(PEST%PARAM)
  IU=UTL_GETUNIT(); OPEN(IU,FILE=PEST%PARAM(I)%ICOVFNAME,STATUS='OLD',ACTION='READ')
  READ(IU,*); READ(IU,*) N; ALLOCATE(PEST%PARAM(I)%COV(N,N))  
  DO J=1,N; READ(IU,*) (PEST%PARAM(I)%COV(J,K),K=1,N); ENDDO
  CLOSE(IU)
 ENDDO
 
 END SUBROUTINE IPEST_IES_UPDATE_ENSEMBLES
 
 !#####=================================================================
 SUBROUTINE IPEST_IES_SAVEREALS(DIR) 
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: I,J,K,ILAY,SCL_UP,SCL_D
 
 !## read idf
 DO I=1,PEST%NREALS
 
  DO J=1,SIZE(PEST%PARAM)
   DO K=1,SIZE(PEST%PARAM(J)%ILS)
    ILAY=PEST%PARAM(J)%ILS(K)
    IF(ILAY.LE.0.OR.ILAY.GT.PRJNLAY)CYCLE
    SELECT CASE (PEST%PARAM(J)%PPARAM)
     CASE ('KH')
 
      !## it is not allowed to scale as the covariance matrix does not scale with it
      PRJIDF%FNAME=PEST%PARAM(J)%REALSFNAME(I); SCL_UP=10; SCL_D=0
      !## read/clip/scale idf file
      IF(.NOT.IDFREADSCALE(PRJIDF%FNAME,PRJIDF,SCL_UP,SCL_D,1.0D0,0))RETURN
      !## save array, do not correct for boundary condition as we not yet know for what layer the zone will apply
      IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\MODELINPUT\LPF7\HK_L'//TRIM(ITOS(ILAY))//'_R#'//TRIM(ITOS(I))//'.ARR',PRJIDF,0,0,1,0))RETURN

    END SELECT
   ENDDO
  ENDDO
 ENDDO
 
 END SUBROUTINE IPEST_IES_SAVEREALS
 
 !#####=================================================================
 LOGICAL FUNCTION IPEST_IES_ALLOCATE(DIR)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: I,N,M,IU
 
 IPEST_IES_ALLOCATE=.FALSE.
 
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
 
 END SUBROUTINE IPEST_IES_DEALLOCATE
 
END MODULE MOD_IPEST_IES