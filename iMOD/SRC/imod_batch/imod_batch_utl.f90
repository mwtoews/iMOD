MODULE MOD_BATCH_UTL

USE WINTERACTER
USE MOD_PMANAGER_PAR, ONLY : PBMAN,TOP,BOT,TOPICS,TKHV
USE MOD_IDF, ONLY : IDFNULLIFY
USE MOD_UTL, ONLY : UTL_READPOINTER,UTL_READINITFILE,ITOS,UTL_REALTOSTRING

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
 PBMAN%NLINESEARCH=10
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
 PBMAN%MC=0
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

 !## usage of parrallel ipest
 IF(PBMAN%IPEST.EQ.0.AND.PBMAN%IFORMAT.EQ.2)THEN
  IF(UTL_READINITFILE('IPESTP',LINE,IU,1))READ(LINE,*) PBMAN%IPESTP
  IF(PBMAN%IPESTP.EQ.1.AND.IBATCH.EQ.1)WRITE(*,'(A,I10)') 'IPESTP=',PBMAN%IPESTP
  IF(PBMAN%IPESTP.EQ.1)THEN
   IF(UTL_READINITFILE('NLINESEARCH',LINE,IU,1))READ(LINE,*) PBMAN%NLINESEARCH
   IF(IBATCH.EQ.1)WRITE(*,'(A,I10)') 'NLINESEARCH=',PBMAN%NLINESEARCH
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
 
END MODULE MOD_BATCH_UTL