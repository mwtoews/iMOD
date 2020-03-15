MODULE MOD_BATCH_UTL

USE WINTERACTER
USE MOD_PMANAGER_PAR, ONLY : PBMAN,TOP,BOT,TOPICS,TKHV
USE MOD_IDF, ONLY : IDFNULLIFY,IDFALLOCATEX,IDFWRITE,IDFGETLOC
USE MOD_UTL, ONLY : UTL_READPOINTER,UTL_READINITFILE,ITOS,UTL_REALTOSTRING,UTL_GETGAMMA,UTL_STDEF, &
  UTL_CREATEDIR,UTL_MATMUL,UTL_READPOINTER_REAL
USE MOD_IDF_PAR
USE MOD_LUDCMP !, ONLY : CHOLESKYDECOMPOSITION,IPEST_NORMAL_MS_SAMPLE,LOG_NORMAL_SAMPLE
USE MOD_IPEST_IES_PAR

CHARACTER(LEN=3*256),PRIVATE :: LINE

CONTAINS

 !###======================================================================
 SUBROUTINE IMODBATCH_RUNFILE_INITPBMAN()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 PBMAN%MODFLOW=''
 PBMAN%IMOD_WQ=''
 PBMAN%MODFLOW6=''
 
 !## initialize all pbman-variables
 PBMAN%BNDFILE=''
 PBMAN%OUTPUT=''
 PBMAN%TIMFNAME=''
 PBMAN%GENFNAME=''

 PBMAN%IFORMAT=0; 
 PBMAN%ISOLVE=1
 PBMAN%SSYSTEM=0
 PBMAN%NSTEP=1
 PBMAN%NMULT=1.0D0
 PBMAN%ISTEADY=0
 PBMAN%IWINDOW=1
 PBMAN%ISS=0
 PBMAN%IDOUBLE=0
 PBMAN%ICONSISTENCY=2
 PBMAN%MINTHICKNESS=0.1D0
 PBMAN%DMMFILE=0

 PBMAN%NLINESEARCH=3
 ALLOCATE(PBMAN%LAMBDA_TEST(3)); PBMAN%LAMBDA_TEST=[0.5D0,1.0D0,2.0D0]

 PBMAN%NCPU=1
 PBMAN%NSWAIT=0
 PBMAN%CMDHIDE=1
 !## set all to interpolate
 DO I=1,SIZE(PBMAN%INT); PBMAN%INT(I)=1; ENDDO
 PBMAN%EIGV=99.0D0
 PBMAN%IGENMF6=0

 PBMAN%IPKS=0
 PBMAN%NSUBMODEL=1
 PBMAN%ISUBMODEL=1
 PBMAN%IPEST=0
 PBMAN%IPESTP=0
 PBMAN%IIES=0
 PBMAN%EIGV=99.9D0

 PBMAN%IFVDL=0
 PBMAN%INT=1
 PBMAN%SSYSTEM=0
 PBMAN%MINKD=0.0D0
 PBMAN%MINC =0.0D0
 PBMAN%ICHKCHD=0
 PBMAN%ICONCHK=0
 PBMAN%DWEL=1
 PBMAN%DISG=1
 PBMAN%DSFR=0
 PBMAN%ISAVEENDDATE=0
  
 !## default packages
 PBMAN%ITT=9
 !## default number of selected interval
 PBMAN%IDT=1
 
 PBMAN%SDATE=0
 PBMAN%EDATE=0
 
 PBMAN%SMTYPE=0
 
 !## iMOD-WQ    
 PBMAN%BTN%NPROBS=1     !## btn
 PBMAN%BTN%NPRMAS=1     !## btn 
 PBMAN%ADV%MIXELM=-1    !## adv
 PBMAN%ADV%NADVFD=0     !## adv
 PBMAN%ADV%PERCEL=1     !## adv
 PBMAN%SSM%MXSS= 100000 !## ssm

 END SUBROUTINE IMODBATCH_RUNFILE_INITPBMAN
 
 !###======================================================================
 LOGICAL FUNCTION IMODBATCH_RUNFILE_READ(IU,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH,IU
 INTEGER :: I,J,N
 INTEGER,POINTER,DIMENSION(:) :: FLXILAY
 
 IMODBATCH_RUNFILE_READ=.FALSE.

 CALL IMODBATCH_RUNFILE_INITPBMAN()
 
 !## model dimensions from first IDF from PRJ file
 PBMAN%IWINDOW=1

 PBMAN%IGENMF6=0; PBMAN%IFORMAT=2
 IF(UTL_READINITFILE('SIM_TYPE',LINE,IU,1))READ(LINE,*) PBMAN%IFORMAT
 SELECT CASE (PBMAN%IFORMAT)
  !## runfile
  CASE (1)
   IF(IBATCH.EQ.1)WRITE(*,'(/A/)') 'Export to a iMOD RUNFILE'
   IF(.NOT.UTL_READINITFILE('RUNFILE_OUT',LINE,IU,0))RETURN
   READ(LINE,*) PBMAN%RUNFILE; IF(IBATCH.EQ.1)WRITE(*,'(2A)') 'RUNFILE_OUT='//TRIM(PBMAN%RUNFILE)
   IF(UTL_READINITFILE('OUTPUT_FOLDER',LINE,IU,1))THEN
    READ(LINE,*) PBMAN%OUTPUT; IF(IBATCH.EQ.1)WRITE(*,'(2A)') 'OUTPUT_FOLDER='//TRIM(PBMAN%OUTPUT)
   ENDIF
  !## mf2005
  CASE (2)
   IF(IBATCH.EQ.1)WRITE(*,'(/A/)') 'Export to MODFLOW2005 files' 
   IF(.NOT.UTL_READINITFILE('NAMFILE_OUT',LINE,IU,0))RETURN
   READ(LINE,*) PBMAN%RUNFILE; IF(IBATCH.EQ.1)WRITE(*,'(2A)') 'NAMFILE_OUT='//TRIM(PBMAN%RUNFILE)
  !## mf6
  CASE (3)
   IF(IBATCH.EQ.1)WRITE(*,'(/A/)') 'Export to MODFLOW6 files' 
   IF(.NOT.UTL_READINITFILE('NAMFILE_OUT',LINE,IU,0))RETURN
   READ(LINE,*) PBMAN%RUNFILE; IF(IBATCH.EQ.1)WRITE(*,'(2A)') 'NAMFILE_OUT='//TRIM(PBMAN%RUNFILE)
   !## usage of genfiles for submodelling icm with mf6
   IF(UTL_READINITFILE('IGENMF6',LINE,IU,1))READ(LINE,*) PBMAN%IGENMF6
   !## read model network
   IF(PBMAN%IGENMF6.EQ.1)THEN
    PBMAN%IWINDOW=4
    IF(IBATCH.EQ.1)WRITE(*,'(A,I1)') 'IGENMF6=',PBMAN%IGENMF6
    IF(.NOT.UTL_READINITFILE('GENFNAME',LINE,IU,0))RETURN
    READ(LINE,*) PBMAN%GENFNAME; IF(IBATCH.EQ.1)WRITE(*,'(A)') 'GENFNAME='//TRIM(PBMAN%GENFNAME)
    PBMAN%NSUBMODEL=0; IF(UTL_READINITFILE('NSUBMODEL',LINE,IU,1))READ(LINE,*) PBMAN%NSUBMODEL
    !## entre number of layers per submodel
    IF(PBMAN%NSUBMODEL.GT.0)THEN
     PBMAN%SMTYPE=1
     ALLOCATE(PBMAN%SM(PBMAN%NSUBMODEL))
     DO I=1,PBMAN%NSUBMODEL
      IF(.NOT.UTL_READPOINTER(IU,N,PBMAN%SM(I)%ILAY,'ILAY_SM'//TRIM(ITOS(I)),0,EXCLVALUE=0))RETURN
      !## allocate memory for storage of submodel-idf files
      ALLOCATE(PBMAN%SM(I)%IDF(N)); DO J=1,N; CALL IDFNULLIFY(PBMAN%SM(I)%IDF(J)); ENDDO
     ENDDO
    ENDIF
   ENDIF
  !## seawat
  CASE (4)
   IF(IBATCH.EQ.1)WRITE(*,'(/A/)') 'Export to an iMOD SEAWAT RUNFILE'
   IF(.NOT.UTL_READINITFILE('RUNFILE_OUT',LINE,IU,0))RETURN
   READ(LINE,*) PBMAN%RUNFILE; IF(IBATCH.EQ.1)WRITE(*,'(A)') 'RUNFILE_OUT='//TRIM(PBMAN%RUNFILE)
   IF(UTL_READINITFILE('OUTPUT_FOLDER',LINE,IU,1))THEN
    READ(LINE,*) PBMAN%OUTPUT; IF(IBATCH.EQ.1)WRITE(*,'(A)') 'OUTPUT_FOLDER='//TRIM(PBMAN%OUTPUT)
   ENDIF
  !## mt3d
  CASE (5)
   IF(IBATCH.EQ.1)WRITE(*,'(/A/)') 'Export to an iMOD MT3D RUNFILE'
   IF(.NOT.UTL_READINITFILE('RUNFILE_OUT',LINE,IU,0))RETURN
   READ(LINE,*) PBMAN%RUNFILE; IF(IBATCH.EQ.1)WRITE(*,'(A)') 'RUNFILE_OUT='//TRIM(PBMAN%RUNFILE)
   IF(.NOT.UTL_READINITFILE('RESULT_DIR',LINE,IU,0))RETURN
   READ(LINE,*) PBMAN%OUTPUT; IF(IBATCH.EQ.1)WRITE(*,'(A)') 'RESULT_DIR='//TRIM(PBMAN%OUTPUT)
   IF(.NOT.UTL_READINITFILE('FLOW_RESULT_DIR',LINE,IU,0))RETURN
   READ(LINE,*) PBMAN%FLOW_RESULT_DIR; IF(IBATCH.EQ.1)WRITE(*,'(A)') 'FLOW_RESULT_DIR='//TRIM(PBMAN%FLOW_RESULT_DIR)

   IF(UTL_READINITFILE('NPROBS',LINE,IU,1))READ(LINE,*) PBMAN%BTN%NPROBS
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'NPROBS='//TRIM(ITOS(PBMAN%BTN%NPROBS))
   IF(UTL_READINITFILE('NPRMAS',LINE,IU,1))READ(LINE,*) PBMAN%BTN%NPRMAS
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'NPRMAS='//TRIM(ITOS(PBMAN%BTN%NPRMAS))
   IF(UTL_READINITFILE('MIXELM',LINE,IU,1))READ(LINE,*) PBMAN%ADV%MIXELM
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'MIXELM='//TRIM(ITOS(PBMAN%ADV%MIXELM))
   IF(UTL_READINITFILE('NADVFD',LINE,IU,1))READ(LINE,*) PBMAN%ADV%NADVFD
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'NADVFD='//TRIM(ITOS(PBMAN%ADV%NADVFD))
   IF(UTL_READINITFILE('PERCEL',LINE,IU,1))READ(LINE,*) PBMAN%ADV%PERCEL
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'PERCEL='//TRIM(UTL_REALTOSTRING(PBMAN%ADV%PERCEL))
   IF(UTL_READINITFILE('MXSS',LINE,IU,1))READ(LINE,*) PBMAN%SSM%MXSS
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'MXSS='//TRIM(ITOS(PBMAN%SSM%MXSS))

  CASE DEFAULT
   IF(IBATCH.EQ.1)WRITE(*,'(/A,I5/)') 'Incorrect Output option, iformat=', PBMAN%IFORMAT   
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Incorrect Output option, iformat='//TRIM(ITOS(PBMAN%IFORMAT)),'Error'); RETURN
 END SELECT
 
 IF(PBMAN%IGENMF6.EQ.0)THEN
  IF(UTL_READINITFILE('NETWORKIDF',LINE,IU,1))THEN
   READ(LINE,*) PBMAN%BNDFILE
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'NETWORKIDF='//TRIM(PBMAN%BNDFILE)
   PBMAN%IWINDOW=3
  ELSE
   IF(UTL_READINITFILE('WINDOW',LINE,IU,1))THEN
    PBMAN%IWINDOW=2
    READ(LINE,*) PBMAN%XMIN,PBMAN%YMIN,PBMAN%XMAX,PBMAN%YMAX
    IF(IBATCH.EQ.1)WRITE(*,'(A,4F15.3)') 'WINDOW=',PBMAN%XMIN,PBMAN%YMIN,PBMAN%XMAX,PBMAN%YMAX
    IF(.NOT.UTL_READINITFILE('CELLSIZE',LINE,IU,0))RETURN
    READ(LINE,*) PBMAN%CELLSIZE; IF(IBATCH.EQ.1)WRITE(*,'(A,F15.3)') 'CELLSIZE=',PBMAN%CELLSIZE
   ELSE
    !## cellsize optional
    IF(UTL_READINITFILE('CELLSIZE',LINE,IU,1))READ(LINE,*) PBMAN%CELLSIZE
    IF(IBATCH.EQ.1)WRITE(*,'(A,F15.3)') 'CELLSIZE=',PBMAN%CELLSIZE
   ENDIF
   PBMAN%BUFFER=0.0D0; IF(UTL_READINITFILE('BUFFER',LINE,IU,1))READ(LINE,*) PBMAN%BUFFER
   IF(IBATCH.EQ.1)WRITE(*,'(A,F15.3)') 'BUFFER=',PBMAN%BUFFER
   IF(PBMAN%BUFFER.GT.0.0D0)THEN
    PBMAN%BUFFERCS=0.0D0; IF(UTL_READINITFILE('BUFFERCS',LINE,IU,1))READ(LINE,*) PBMAN%BUFFERCS
    IF(IBATCH.EQ.1)WRITE(*,'(A,F15.3)') 'BUFFERCS=',PBMAN%BUFFERCS
   ENDIF
  ENDIF
 ENDIF
    
 SELECT CASE (PBMAN%IFORMAT)
  CASE (1,2,3)
   !## more specific options to create a runfile
   IF(UTL_READINITFILE('ISS',LINE,IU,1))READ(LINE,*) PBMAN%ISS
   IF(IBATCH.EQ.1)WRITE(*,'(A,I1)') 'ISS=',PBMAN%ISS
  !## always transient for seawat/mt3d
  CASE (4,5)
   PBMAN%ISS=1
 END SELECT
 
 !## transient                              
 IF(PBMAN%ISS.EQ.1)THEN
  !## tim-file?
  IF(UTL_READINITFILE('TIMFNAME',LINE,IU,1))THEN
   READ(LINE,*) PBMAN%TIMFNAME; IF(IBATCH.EQ.1)WRITE(*,'(A,A)') 'TIMFNAME=',TRIM(PBMAN%TIMFNAME)
  ELSE
   !## sdate
   IF(.NOT.UTL_READINITFILE('SDATE',LINE,IU,0))RETURN
   READ(LINE,*) PBMAN%SDATE; IF(IBATCH.EQ.1)WRITE(*,'(A,I14)') 'SDATE=',PBMAN%SDATE
   !## edate
   IF(.NOT.UTL_READINITFILE('EDATE',LINE,IU,0))RETURN
   READ(LINE,*) PBMAN%EDATE; IF(IBATCH.EQ.1)WRITE(*,'(A,I14)') 'EDATE=',PBMAN%EDATE
   !## type of interval
   IF(.NOT.UTL_READINITFILE('ITT',LINE,IU,0))RETURN
   READ(LINE,*) PBMAN%ITT; IF(IBATCH.EQ.1)WRITE(*,'(A,I14)') 'ITT=',PBMAN%ITT
   SELECT CASE (PBMAN%ITT)
    CASE (1:8)
     !## timesteps
     IF(.NOT.UTL_READINITFILE('IDT',LINE,IU,0))RETURN
     READ(LINE,*) PBMAN%IDT; IF(IBATCH.EQ.1)WRITE(*,'(A,I14)') 'IDT=',PBMAN%IDT
   END SELECT
   SELECT CASE (PBMAN%ITT)
    CASE (1)
     IF(IBATCH.EQ.1)WRITE(*,'(A)')      'Time step: ',PBMAN%IDT,' minutes'
    CASE (2)
     IF(IBATCH.EQ.1)WRITE(*,'(A,I5,A)') 'Time step: ',PBMAN%IDT,' hours'
    CASE (3)
     IF(IBATCH.EQ.1)WRITE(*,'(A,I5,A)') 'Time step: ',PBMAN%IDT,' days'
    CASE (4)
     IF(IBATCH.EQ.1)WRITE(*,'(A,I5,A)') 'Time step: ',PBMAN%IDT,' weeks'
    CASE (5)
     IF(IBATCH.EQ.1)WRITE(*,'(A,I5,A)') 'Time step: decades',PBMAN%IDT,' days'
    CASE (6)
     IF(IBATCH.EQ.1)WRITE(*,'(A)')      'Time step: every 14/28'
    CASE (7)
     IF(IBATCH.EQ.1)WRITE(*,'(A,I5,A)') 'Time step: ',PBMAN%IDT,' months'
    CASE (8)
     IF(IBATCH.EQ.1)WRITE(*,'(A,I5,A)') 'Time step: ',PBMAN%IDT,' years'
    CASE (9)
     IF(IBATCH.EQ.1)WRITE(*,'(A)')      'Time step: depending on packages'; PBMAN%IDT=1
   END SELECT

   IF(UTL_READINITFILE('NSTEP',LINE,IU,1))READ(LINE,*) PBMAN%NSTEP
   IF(IBATCH.EQ.1)WRITE(*,'(A,I10)') 'NSTEP=',PBMAN%NSTEP
   IF(UTL_READINITFILE('NMULT',LINE,IU,1))READ(LINE,*) PBMAN%NMULT
   IF(IBATCH.EQ.1)WRITE(*,'(A,F10.2)') 'NMULT=',PBMAN%NMULT
   
   SELECT CASE (PBMAN%IFORMAT)
    CASE (1,2,3)
    IF(UTL_READINITFILE('ISTEADY',LINE,IU,1))READ(LINE,*) PBMAN%ISTEADY
    IF(IBATCH.EQ.1)WRITE(*,'(A,I10)') 'ISTEADY=',PBMAN%ISTEADY
   END SELECT 
   
  ENDIF    
 
 ENDIF
  
 IF(UTL_READINITFILE('SSYSTEM',LINE,IU,1))READ(LINE,*) PBMAN%SSYSTEM
 IF(IBATCH.EQ.1)WRITE(*,'(A,I1)') 'SSYSTEM=',PBMAN%SSYSTEM
  
 !## get isave options
 DO I=1,SIZE(TOPICS)
  IF(.NOT.UTL_READPOINTER(IU,N,PBMAN%ISAVE(I)%ILAY,'SAVE'//TRIM(TOPICS(I)%CMOD),1,EXCLVALUE=0))RETURN
 ENDDO
 !## try to read old fashioned save settings
 IF(.NOT.UTL_READPOINTER(IU,N,FLXILAY,'SAVEFLX',1,EXCLVALUE=0))RETURN
 IF(ASSOCIATED(FLXILAY))THEN
  IF(ASSOCIATED(PBMAN%ISAVE(TKHV)%ILAY))DEALLOCATE(PBMAN%ISAVE(TKHV)%ILAY)
  ALLOCATE(PBMAN%ISAVE(TKHV)%ILAY(SIZE(FLXILAY)))
  PBMAN%ISAVE(TKHV)%ILAY=FLXILAY; DEALLOCATE(FLXILAY)
 ENDIF
 
 !## get interpolation options
 IF(PBMAN%IFORMAT.EQ.2.OR.PBMAN%IFORMAT.EQ.3)THEN
  DO I=1,SIZE(TOPICS)
   IF(UTL_READINITFILE('INT'//TRIM(TOPICS(I)%CMOD),LINE,IU,1))READ(LINE,*) PBMAN%INT(I)
   IF(PBMAN%INT(I).EQ.0.AND.IBATCH.EQ.1)WRITE(*,'(A,I1)') 'INT'//TRIM(TOPICS(I)%CMOD)//'=',PBMAN%INT(I)
  ENDDO
 ENDIF
  
 IF(PBMAN%IFORMAT.EQ.1.OR.PBMAN%IFORMAT.EQ.2)THEN
  IF(UTL_READINITFILE('ICONCHK',LINE,IU,1))READ(LINE,*) PBMAN%ICONCHK
  IF(IBATCH.EQ.1)WRITE(*,'(A,I10)') 'ICONCHK=',PBMAN%ICONCHK
  IF(UTL_READINITFILE('IDOUBLE',LINE,IU,1))READ(LINE,*) PBMAN%IDOUBLE
  IF(IBATCH.EQ.1)WRITE(*,'(A,I10)') 'IDOUBLE=',PBMAN%IDOUBLE
  IF(UTL_READINITFILE('ISAVEENDDATE',LINE,IU,1))READ(LINE,*) PBMAN%ISAVEENDDATE
  IF(IBATCH.EQ.1)WRITE(*,'(A,I1)') 'ISAVEENDDATE=',PBMAN%ISAVEENDDATE
  IF(UTL_READINITFILE('IPKS',LINE,IU,1))READ(LINE,*) PBMAN%IPKS
  IF(IBATCH.EQ.1)WRITE(*,'(A,I1)') 'IPKS=',PBMAN%IPKS
 ENDIF
  
 !## specify unconfined/parameter estimation
 IF(PBMAN%IFORMAT.EQ.2.OR.PBMAN%IFORMAT.EQ.3)THEN
  IF(.NOT.UTL_READPOINTER(IU,N,PBMAN%UNCONFINED,'UNCONFINED',1))RETURN 
  IF(UTL_READINITFILE('ICHKCHD',LINE,IU,1))READ(LINE,*) PBMAN%ICHKCHD
  IF(IBATCH.EQ.1)WRITE(*,'(A,I1)') 'ICHKCHD=',PBMAN%ICHKCHD
  IF(UTL_READINITFILE('ICONSISTENCY',LINE,IU,1))READ(LINE,*) PBMAN%ICONSISTENCY
  IF(IBATCH.EQ.1)WRITE(*,'(A,I1)') 'ICONSISTENCY=',PBMAN%ICONSISTENCY
  IF(PBMAN%ICONSISTENCY.EQ.2)THEN
   IF(UTL_READINITFILE('MINTHICKNESS',LINE,IU,1))READ(LINE,*) PBMAN%MINTHICKNESS
   IF(IBATCH.EQ.1)WRITE(*,'(A,G15.7)') 'MINTHICKNESS=',PBMAN%MINTHICKNESS
  ENDIF
 ENDIF
  
 !## if not defined, than all confined
 IF(.NOT.ASSOCIATED(PBMAN%UNCONFINED))THEN; ALLOCATE(PBMAN%UNCONFINED(1)); PBMAN%UNCONFINED(1)=0; ENDIF

 !## usage of dflow-fm
 IF(PBMAN%IFORMAT.EQ.2)THEN
  IF(UTL_READINITFILE('DMMFILE',LINE,IU,1))READ(LINE,*) PBMAN%DMMFILE
  IF(IBATCH.EQ.1)WRITE(*,'(A,I1)') 'DMMFILE=',PBMAN%DMMFILE
 ENDIF

 !## usage of ipest
 IF(PBMAN%IFORMAT.EQ.1.OR.PBMAN%IFORMAT.EQ.2)THEN
  IF(UTL_READINITFILE('IPEST',LINE,IU,1))READ(LINE,*) PBMAN%IPEST
  IF(PBMAN%IPEST.EQ.1.AND.IBATCH.EQ.1)WRITE(*,'(A,I10)') 'IPEST=',PBMAN%IPEST
 ENDIF

 !## usage of parrallel ipest or ies
 IF(PBMAN%IPEST.EQ.0.AND.PBMAN%IFORMAT.EQ.2)THEN
  IF(UTL_READINITFILE('IIES',LINE,IU,1))READ(LINE,*) PBMAN%IIES
  IF(PBMAN%IIES.AND.IBATCH.EQ.1)WRITE(*,'(A,I10)') 'IIES=',PBMAN%IIES
  IF(PBMAN%IIES.EQ.0)THEN
   IF(UTL_READINITFILE('IPESTP',LINE,IU,1))READ(LINE,*) PBMAN%IPESTP
   IF(PBMAN%IPESTP.EQ.1.AND.IBATCH.EQ.1)WRITE(*,'(A,I10)') 'IPESTP=',PBMAN%IPESTP
  ENDIF
  IF(PBMAN%IPESTP.EQ.1.OR.PBMAN%IIES.EQ.1)THEN
!   IF(UTL_READINITFILE('NLINESEARCH',LINE,IU,1))READ(LINE,*) PBMAN%NLINESEARCH
!   IF(IBATCH.EQ.1)WRITE(*,'(A,I10)') 'NLINESEARCH=',PBMAN%NLINESEARCH

   IF(.NOT.UTL_READPOINTER_REAL(IU,N,PBMAN%LAMBDA_TEST,'LAMBDA_TEST',1))RETURN
   PBMAN%NLINESEARCH=SIZE(PBMAN%LAMBDA_TEST)
   
   IF(UTL_READINITFILE('CMDHIDE',LINE,IU,1))READ(LINE,*) PBMAN%CMDHIDE
   IF(IBATCH.EQ.1)WRITE(*,'(A,I10)') 'CMDHIDE=',PBMAN%CMDHIDE
   IF(.NOT.UTL_READINITFILE('NCPU',LINE,IU,0))RETURN
   READ(LINE,*) PBMAN%NCPU; IF(IBATCH.EQ.1)WRITE(*,'(A,I10)') 'NCPU=',PBMAN%NCPU
   IF(UTL_READINITFILE('NSWAIT',LINE,IU,1))READ(LINE,*) PBMAN%NSWAIT
   IF(IBATCH.EQ.1)WRITE(*,'(A,I10)') 'NSWAIT=',PBMAN%NSWAIT
   IF(UTL_READINITFILE('PDEBUG',LINE,IU,1))READ(LINE,*) PBMAN%PDEBUG
   IF(IBATCH.EQ.1)WRITE(*,'(A,I10)') 'PDEBUG=',PBMAN%PDEBUG
   IF(UTL_READINITFILE('SVD_EIGV',LINE,IU,1))READ(LINE,*) PBMAN%EIGV
   IF(IBATCH.EQ.1)WRITE(*,'(A,F10.4)') 'SVD_EIGV=',PBMAN%EIGV
  ENDIF
 ENDIF

 PBMAN%PTEST=0
 IF(PBMAN%IPEST+PBMAN%IPESTP.GT.0)THEN
  IF(UTL_READINITFILE('PTEST',LINE,IU,1))READ(LINE,*) PBMAN%PTEST
  IF(PBMAN%PTEST.EQ.1.AND.IBATCH.EQ.1)WRITE(*,'(A,I10)') 'PTEST=',PBMAN%PTEST
 ENDIF
 !## write warning for system in case of ipest - ssystem=sumsystem
 IF(PBMAN%IPEST.EQ.1.AND.PBMAN%SSYSTEM.EQ.1)THEN
  IF(IBATCH.EQ.1)WRITE(*,'(/1X,A/)') '>>> Be aware summing systems might influence iPEST if you optimize packages <<<'
 ENDIF
  
 IF(PBMAN%IFORMAT.EQ.1.OR.PBMAN%IFORMAT.EQ.2)THEN
  IF(UTL_READINITFILE('MINKD',LINE,IU,1))READ(LINE,*) PBMAN%MINKD
  IF(IBATCH.EQ.1)WRITE(*,'(A,G15.7)') 'MINKD=',PBMAN%MINKD
  IF(UTL_READINITFILE('MINC',LINE,IU,1))READ(LINE,*) PBMAN%MINC
  IF(IBATCH.EQ.1)WRITE(*,'(A,G15.7)') 'MINC=',PBMAN%MINC
  IF(UTL_READINITFILE('IFVDL',LINE,IU,1))READ(LINE,*) PBMAN%IFVDL
  IF(IBATCH.EQ.1)WRITE(*,'(A,I1)') 'IFVDL=',PBMAN%IFVDL
 ENDIF
 IF(UTL_READINITFILE('DWEL',LINE,IU,1))READ(LINE,*) PBMAN%DWEL
 IF(IBATCH.EQ.1)WRITE(*,'(A,I1)') 'DWEL=',PBMAN%DWEL
 IF(UTL_READINITFILE('DISG',LINE,IU,1))READ(LINE,*) PBMAN%DISG
 IF(IBATCH.EQ.1)WRITE(*,'(A,I1)') 'DISG=',PBMAN%DISG
 IF(UTL_READINITFILE('DSFR',LINE,IU,1))READ(LINE,*) PBMAN%DSFR
 IF(IBATCH.EQ.1)WRITE(*,'(A,I1)') 'DSFR=',PBMAN%DSFR

 IF(UTL_READINITFILE('ISOLVE',LINE,IU,1))READ(LINE,*) PBMAN%ISOLVE
 IF(IBATCH.EQ.1)WRITE(*,'(A,I1)') 'ISOLVE=',PBMAN%ISOLVE
 IF(PBMAN%ISOLVE.EQ.1)THEN
  SELECT CASE (PBMAN%IFORMAT)
   !## mf2005-run,mf2005-nam
   CASE (1,2)
    IF(.NOT.UTL_READINITFILE('MODFLOW',LINE,IU,0))RETURN
    READ(LINE,*) PBMAN%MODFLOW; IF(IBATCH.EQ.1)WRITE(*,'(A)') 'MODFLOW='//TRIM(PBMAN%MODFLOW)
   !## modflow6
   CASE (3)
    IF(.NOT.UTL_READINITFILE('MODFLOW6',LINE,IU,0))RETURN
    READ(LINE,*) PBMAN%MODFLOW6; IF(IBATCH.EQ.1)WRITE(*,'(A)') 'MODFLOW6='//TRIM(PBMAN%MODFLOW6)
   !## seawat,mt3d
   CASE (4,5)
    IF(.NOT.UTL_READINITFILE('IMOD-WQ',LINE,IU,0))RETURN
    READ(LINE,*) PBMAN%IMOD_WQ; IF(IBATCH.EQ.1)WRITE(*,'(A)') 'IMOD-WQ='//TRIM(PBMAN%IMOD_WQ)
  END SELECT
 ENDIF 

 IMODBATCH_RUNFILE_READ=.TRUE.

 END FUNCTION IMODBATCH_RUNFILE_READ
 
 !###======================================================================
 SUBROUTINE IMODBATCH_CREATEENSEMBLES_CHOLESKY(STDEV,MEAN,DIR,RANGE,NSIM,FNAME,ILOG)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: STDEV,MEAN
 CHARACTER(LEN=*),INTENT(IN) :: DIR,FNAME
 REAL(KIND=DP_KIND),INTENT(IN) :: RANGE
 INTEGER,INTENT(IN) :: NSIM,ILOG
 INTEGER :: N,I,J,IROW,ICOL,IROW2,ICOL2,ISIM,KTYPE,NPOP,SEED
 REAL(KIND=DP_KIND),DIMENSION(:,:),ALLOCATABLE :: COV,COR,EV,EVAL
 REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: V,M,X,E,EW
 REAL(KIND=DP_KIND) :: GAMMA,XCOR,X1,Y1,X2,Y2,STDV,XT,SMALL,NUGGET,SILL !,HV,LV
 TYPE(IDFOBJ) :: IDF
 LOGICAL :: LCHOL
 
 LCHOL=.TRUE.
 
 CALL UTL_CREATEDIR(DIR)
 
 !## fill in variance (kriging)
 SILL=1.0D0; NUGGET=0.0D0; KTYPE=3 !exponential STOPS @ 1
 SMALL=0.01D0
 
 !## in log space sill cannot be 1.0
 
 N=STDEV%NROW*STDEV%NCOL; ALLOCATE(COV(N,N),COR(N,N),V(N),M(N)); COR=0.0D0; COV=0.0D0; V=0.0D0; M=0.0D0
 I=0; DO IROW=1,STDEV%NROW; DO ICOL=1,STDEV%NCOL
  IF(STDEV%X(ICOL,IROW).EQ.STDEV%NODATA.OR. &
     MEAN%X(ICOL ,IROW).EQ.MEAN%NODATA)THEN
   STDEV%X(ICOL,IROW)=STDEV%NODATA
   MEAN%X(ICOL ,IROW)=MEAN%NODATA
  ENDIF
  I=I+1; V(I)=STDEV%X(ICOL,IROW); M(I)=MEAN%X(ICOL,IROW)
 ENDDO; ENDDO
  
 COV=0.0D0
 DO I=1,N
  IF(V(I).NE.STDEV%NODATA)THEN
   COV(I,I)=V(I)*V(I)
   COR(I,I)=1.0D0
   COV(I,I)=COV(I,I)*COR(I,I)
  ENDIF
 ENDDO
 IROW=1; ICOL=0; DO I=1,N
  ICOL=ICOL+1; IF(ICOL.GT.STDEV%NCOL)THEN; ICOL=1; IROW=IROW+1; ENDIF

  !## skip nodata
  IF(COV(I,I).EQ.STDEV%NODATA)CYCLE
  
  CALL IDFGETLOC(STDEV,IROW,ICOL,X1,Y1)
  IROW2=IROW; ICOL2=ICOL; DO J=I+1,N
   ICOL2=ICOL2+1; IF(ICOL2.GT.STDEV%NCOL)THEN; ICOL2=1; IROW2=IROW2+1; ENDIF
  
   !## skip nodata
   IF(COV(J,J).EQ.STDEV%NODATA)CYCLE
   
   CALL IDFGETLOC(STDEV,IROW2,ICOL2,X2,Y2)
   !## get the variogram values
   GAMMA=UTL_GETGAMMA(X1,Y1,X2,Y2,RANGE,SILL,NUGGET,KTYPE)
   
   !## if log dna log10() ipv log() 
   !## if log also nugget might be 1 en sill 2?
   XCOR=1.0D0-GAMMA
   COR(I,J)=XCOR; COR(J,I)=XCOR
   COV(I,J)=SQRT(COV(I,I))*SQRT(COV(J,J))*XCOR
   COV(J,I)=COV(I,J)
  
  ENDDO
 ENDDO

! OPEN(10,FILE=TRIM(DIR)//'\COV.TXT',STATUS='UNKNOWN',ACTION='WRITE')
! WRITE(10,*) 'COVARIANCE MATRIX'; WRITE(10,*) N
! DO I=1,N; DO J=1,N; WRITE(10,*) COV(I,J); ENDDO; ENDDO
! CLOSE(10)

 !## save as IDF (for fun)
 CALL IDFNULLIFY(IDF)
 IDF%DX=1.0D0; IDF%DY=1.0D0; IDF%NCOL=N; IDF%NROW=N
 IDF%XMIN=0.0D0; IDF%XMAX=IDF%XMIN+IDF%NCOL*IDF%DX
 IDF%YMIN=0.0D0; IDF%YMAX=IDF%YMIN+IDF%NROW*IDF%DY
 IF(.NOT.IDFALLOCATEX(IDF))RETURN
 DO I=1,N; DO J=1,N; IDF%X(I,J)=COV(I,J); ENDDO; ENDDO
 IDF%FNAME=TRIM(DIR)//'\COV.IDF'; IF(.NOT.IDFWRITE(IDF,IDF%FNAME,1))STOP
  
! IF(N.LT.200)THEN
!  OPEN(10,FILE=TRIM(DIR)//'\COV.M',STATUS='UNKNOWN',ACTION='WRITE')
!  WRITE(10,*) 'COR=['
!  DO J=1,N; WRITE(10,'(999G17.5)') (COR(J,I),I=1,N); ENDDO
!  WRITE(10,*) ']'
!  WRITE(10,*) 'C=['
!  DO J=1,N; WRITE(10,'(999G17.5)') (COV(J,I),I=1,N); ENDDO
!  WRITE(10,*) ']'
!  FLUSH(10)
! ENDIF
 
 IF(LCHOL)THEN
 
  !## perform choleski-decomposition A=LTL - delivers upper triangle is LT
  CALL CHOLESKYDECOMPOSITION(COV,N) 
 
  DO I=1,N; DO J=1,N; IDF%X(I,J)=COV(I,J); ENDDO; ENDDO
  IDF%FNAME=TRIM(DIR)//'\CHOL.IDF'; IF(.NOT.IDFWRITE(IDF,IDF%FNAME,1))STOP
 ELSE

  ALLOCATE(EV(N,N),E(N),EW(N),EVAL(N,N))
  !## copy covariance matrix
  EV=COV
  
  !## perform eigenvalue decomposition
  CALL LUDCMP_TRED2(EV,N,N,EW,E)
  !## ev are the eigenvectors
  CALL LUDCMP_TQLI(EW,E,N,N,EV)
  !## sort
  CALL LUDCMP_EIGSRT(EW,EV,N,N) 

!  !## vector lengte - klopt zijn allemaal 1.0
!  DO I=1,N
!   X1=0.0D0; DO J=1,N
!    X1=X1+EV(I,J)**2.0D0
!   ENDDO
!   WRITE(*,*) I,X1
!  ENDDO
  EVAL=0.0D0; DO I=1,N
   IF(EW(I).LE.1.0D-10)EW(I)=0.0D0
   EVAL(I,I)=SQRT(EW(I))
  ENDDO
  CALL UTL_MATMUL(EV,EVAL,COV)
  DEALLOCATE(EV,EVAL,EW,E)
 
 ENDIF
 
! IF(N.LT.200)THEN
!  WRITE(10,*) 'CHOL=['
!  DO J=1,N; WRITE(10,'(999G17.5)') (COV(J,I),I=1,N); ENDDO
!  WRITE(10,*) ']'
!  FLUSH(10)
! ENDIF

 WRITE(*,'(A10,3A15)') 'NPOP','MEAN','STDV','VARIANCE'

 ALLOCATE(X(N)); X=0.0D0
 
 !## generate ensembles
 SEED=12345
 DO ISIM=1,NSIM
!  OPEN(10,FILE=TRIM(DIR)//'\COV.M',STATUS='UNKNOWN',ACTION='WRITE')
  DO I=1,N
!## PEST-IES IS DOING RANDOM-NORMAL SAMPLING AND THEN USE EIGENVECTORS
!   IF(KLOG)THEN
!    CALL LOG_NORMAL_SAMPLE (0.0D0, 1.0D0, SEED, X(I) )
!    !## ln(samples) geeft normale verdeling
!    X(I)=LOG(X(I))
!   ELSE
   !## generates number -3 to +3 - in log-space permeability is normal-distributed
   CALL IPEST_NORMAL_MS_SAMPLE(0.0D0,1.0D0,SEED,X(I))
!   ENDIF
!   WRITE(10,*) I,X(I)
  ENDDO
!  CLOSE(10)

!### NOTE
! het is de bedoeling om wel de normal-sampling te gebruiken maar de variance moet dan juist geschaal worden
! dus ingeven minimale- en maximale waarde, dat levert kleine variance op geschaald met log10()
 
!  !## this creates similar ensembles as last succesfull run
  !## creates values from 0 to 1
!  CALL RANDOM_NUMBER(X); X=X-0.5D0
  V=0.0D0

!  IF(N.LT.200)THEN
!   WRITE(10,*) 'X=['
!   DO J=1,N; WRITE(10,'(999G17.5)') X(J); ENDDO
!   WRITE(10,*) ']'
!   WRITE(10,*) 'M=['
!   DO J=1,N; WRITE(10,'(999G17.5)') M(J); ENDDO
!   WRITE(10,*) ']'
!  ENDIF
 
  !## z=m*Lx
  DO I=1,N
   V(I)=0.0D0
   IF(LCHOL)THEN
    DO J=1,N !I
     !## cholesky uses lower
     V(I)=V(I)+COV(I,J)*X(J)
    ENDDO
   ELSE
    !## eigenvalue decomposition multiplication eigenvalues with random number
    DO J=1,N
     V(I)=V(I)+COV(J,I)*X(J)
    ENDDO
   ENDIF
   IF(M(I).NE.MEAN%NODATA)THEN
    V(I)=M(I)+V(I)
   ELSE
    V(I)=MEAN%NODATA
   ENDIF
  ENDDO

  !## all of the ensembles go to stdv - statistics is for log10 distribution
  CALL UTL_STDEF(V,N,-999.99D0,STDV,XT,NPOP)
  
  WRITE(*,'(I10,3F15.7)') NPOP,XT,STDV,STDV**2.0D0
   
  IF(ILOG)THEN
   DO I=1,N
    V(I)=10.0**V(I)
   ENDDO
  ENDIF

  I=0; DO IROW=1,STDEV%NROW; DO ICOL=1,STDEV%NCOL
   I=I+1
   MEAN%X(ICOL,IROW)=V(I)
  ENDDO; ENDDO
  MEAN%FNAME=TRIM(DIR)//'\'//TRIM(FNAME)//'_R'//TRIM(ITOS(ISIM))//'.IDF'
  IF(.NOT.IDFWRITE(MEAN,MEAN%FNAME,1))STOP
 ENDDO
! IF(N.LT.200)CLOSE(10)

 DEALLOCATE(COV,COR,X,V)

 END SUBROUTINE IMODBATCH_CREATEENSEMBLES_CHOLESKY
 
 END MODULE MOD_BATCH_UTL