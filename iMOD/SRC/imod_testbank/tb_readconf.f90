MODULE MOD_TB_READCONF

USE MOD_TB_PAR
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFWRITE,IDFNULLIFY,IDFDEALLOCATE
USE MOD_UTL, ONLY : UTL_PRINTTEXT,UTL_CREATEDIR,UTL_GETUNIT, &
    UTL_CAP,UTL_SUBST,UTL_DIRINFO_POINTER,UTL_GETMED,ITOS

CONTAINS

 !###======================================================================
 SUBROUTINE TB_MAIN()
 !###======================================================================
 IMPLICIT NONE

 CALL TB_READCONFIG()
 CALL TB_START()

 END SUBROUTINE TB_MAIN

 !###======================================================================
 SUBROUTINE TB_READCONFIG()
 !###====================================================================== 
 IMPLICIT NONE
 INTEGER :: I,IU,IOS
 CHARACTER(LEN=256) :: LINE
 
 IU=UTL_GETUNIT()
 OPEN(IU,FILE=CONFNAME,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 IF(IOS.NE.0)CALL UTL_PRINTTEXT('OPENING CONFIGURATION FILE',2)
 READ(IU,*,IOSTAT=IOS) OUTMAP 
 IF(IOS.NE.0)CALL UTL_PRINTTEXT('READING OUTMAP',2)
 READ(IU,*,IOSTAT=IOS) REPLACESTRING 
 IF(IOS.NE.0)CALL UTL_PRINTTEXT('READING REPLACESTRING',2)
 READ(IU,*,IOSTAT=IOS) THRESHOLD 
 IF(IOS.NE.0)CALL UTL_PRINTTEXT('READING THRESHOLD',2)
 READ(IU,*,IOSTAT=IOS) NEXE 
 IF(IOS.NE.0)CALL UTL_PRINTTEXT('READING NEXE',2)
 ALLOCATE(EXE(NEXE))
 DO I=1,NEXE
  READ(IU,*,IOSTAT=IOS) EXE(I)%IACT,EXE(I)%ALIAS,EXE(I)%FNAME,EXE(I)%IMAP
  IF(IOS.NE.0)CALL UTL_PRINTTEXT('ERROR READING FILE',2)
  LINE=','//TRIM(EXE(I)%ALIAS)//','//TRIM(EXE(I)%FNAME)//','//TRIM(ITOS(EXE(I)%IMAP))
  WRITE(*,'(I1,A)') EXE(I)%IACT,TRIM(LINE)
 ENDDO
 IF(IOS.NE.0)CALL UTL_PRINTTEXT('READING NRUN',2)
 NRUN=50; ALLOCATE(RUN(NRUN))
 I=0; DO
  I=I+1; READ(IU,'(A)',IOSTAT=IOS) RUN(I)%FNAME
  IF(IOS.NE.0)EXIT
  IF(LEN_TRIM(RUN(I)%FNAME).EQ.0)EXIT
  IF(I.EQ.SIZE(RUN))THEN
   ALLOCATE(RUN_BU(NRUN*2))
   RUN_BU(1:NRUN)=RUN(1:NRUN)
   DEALLOCATE(RUN); RUN=>RUN_BU
   NRUN=SIZE(RUN)
  ENDIF
  WRITE(*,'(A)') TRIM(RUN(I)%FNAME)
 ENDDO
 NRUN=I-1
 CLOSE(IU)
 
 CALL UTL_CREATEDIR(OUTMAP)
  
 IUOUTG=UTL_GETUNIT()
 OUTDIR=TRIM(OUTMAP)//'\Summary_Testresults'
 CALL UTL_CREATEDIR(OUTDIR)
 OPEN(IUOUTG,FILE=TRIM(OUTMAP)//'\testbank_out.txt',STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
 !IF(IOS.NE.0)CALL UTL_PRINTTEXT('OPENING OUTPUT FILE',2)

 END SUBROUTINE TB_READCONFIG
 
 !###======================================================================
 SUBROUTINE TB_START()
 !###====================================================================== 
 IMPLICIT NONE
 INTEGER :: IRUN,IEXE,IOS,I,J
 LOGICAL :: LEX
 CHARACTER(LEN=256) :: FNAME
 
 DO IRUN=1,NRUN
  IUOUT=UTL_GETUNIT()
  I=INDEX(RUN(IRUN)%FNAME,'\',.true.)+1
  J=INDEX(RUN(IRUN)%FNAME,'.',.true.)-1
  FNAME=TRIM(RUN(IRUN)%FNAME(I:J))
  OPEN(IUOUT,FILE=TRIM(OUTDIR)//'\testbank_out_'//TRIM(FNAME)//'.txt',STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
  IF(IOS.NE.0)THEN
   CALL UTL_PRINTTEXT('OPENING OUTPUT FILE',2)
  ENDIF
  DO IEXE=1,NEXE
   IF(EXE(IEXE)%IACT.EQ.1)THEN
    INQUIRE(FILE=TRIM(REPLACESTRING)//'\'//RUN(IRUN)%FNAME,EXIST=LEX)
    IF(.NOT.LEX)CALL UTL_PRINTTEXT('Can not find '//TRIM(REPLACESTRING)//'\'//TRIM(RUN(IRUN)%FNAME),2)
    CALL TB_WRITERUN(IEXE,IRUN)
   ENDIF
  ENDDO
  CALL TB_DIFFRUN(IRUN)
  CLOSE(IUOUT)
 ENDDO
  
 END SUBROUTINE TB_START
 
 !###====================================================================== 
 SUBROUTINE TB_WRITERUN(IEXE,IRUN)
 !###====================================================================== 
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IEXE,IRUN
 INTEGER :: IU,JU,IOS
 CHARACTER(LEN=256) :: LINE,RESDIR
 
 !## get rid of "\"'s, make them "_"'s
 RESDIR=RUN(IRUN)%FNAME(:INDEX(RUN(IRUN)%FNAME,'.',.TRUE.)-1)
  
 !## open new runfile
 RESDIR=TRIM(OUTMAP)//'\'//TRIM(RESDIR)//'\'//TRIM(EXE(IEXE)%ALIAS)
 !## create folder
 CALL UTL_CREATEDIR(RESDIR)
 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(RESDIR)//'\'//'imodflow.run',STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)CALL UTL_PRINTTEXT('Error opening file '//TRIM(RESDIR)//'\'//'imodflow.run',2)
 
 IU=UTL_GETUNIT()   ; OPEN(IU,FILE=TRIM(REPLACESTRING)//'\'//TRIM(RUN(IRUN)%FNAME),STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 IF(IOS.NE.0)CALL UTL_PRINTTEXT('Error opening file '//TRIM(REPLACESTRING)//'\'//TRIM(RUN(IRUN)%FNAME),2)
 !## read old result-folder
 READ(IU,'(A)',IOSTAT=IOS) 

 WRITE(JU,'(A)') TRIM(RESDIR)
 DO
  READ(IU,'(A)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  LINE=UTL_CAP(LINE,'U') 
  LINE=UTL_SUBST(LINE,'$DBASE$',TRIM(REPLACESTRING))
  WRITE(JU,'(A)') TRIM(LINE)
 ENDDO
 CLOSE(IU); CLOSE(JU)

 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(RESDIR)//'\run.bat',STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)CALL UTL_PRINTTEXT('Error opening file '//TRIM(RESDIR)//'\run.bat',2)
 LINE=TRIM(EXE(IEXE)%FNAME)//' '//TRIM(RESDIR)//'\imodflow.run'
 WRITE(IU,'(A)') 'REM clean file'
 WRITE(IU,'(A)') 'IF EXIST '//TRIM(RESDIR)//'\ERROR.TXT DEL '//TRIM(RESDIR)//'\ERROR.TXT'
 WRITE(IU,'(A)') 'REM execute program'
 WRITE(IU,'(A)') TRIM(LINE)
 WRITE(IU,'(A)') 'REM echo exitcode'
 WRITE(IU,'(A)') 'IF ERRORLEVEL == 1 ECHO %ERRORLEVEL% > '//TRIM(RESDIR)//'\error.txt'
 CLOSE(IU)

 LINE=TRIM(RESDIR)//'\run.bat'
 CALL SYSTEM(TRIM(LINE))
  
 END SUBROUTINE TB_WRITERUN

 !###====================================================================== 
 SUBROUTINE TB_DIFFRUN(IRUN)
 !###====================================================================== 
 IMPLICIT NONE
 INTEGER,PARAMETER :: MXMAP=14
 INTEGER,INTENT(IN) :: IRUN
 INTEGER :: IU,IOS,IEXE,JEXE,I,J,NNODATA,IMAP,N,IROW,ICOL
 REAL :: XMABS,THDX
 CHARACTER(LEN=256) :: RESDIR,RES,FNAME
 CHARACTER(LEN=12),DIMENSION(MXMAP) :: CMAP
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: LISTNAME
 REAL,DIMENSION(5) :: XMED,PERC
 TYPE(IDFOBJ),DIMENSION(2) :: IDF
 DATA PERC/0.0,25.0,50.0,75.0,100.0/ 
 DATA CMAP/'head','bdgfrf','bdgflf','bdgfff','bdgsto','bdgwel','bdgdrn','bdgriv','bdgghb','bdgrch', &
           'bdgevt','bdgisg','bdgolf','bdgbnd'/  
 CHARACTER(LEN=12) :: CCODE
 LOGICAL :: LEX1,LEX2

 NULLIFY(LISTNAME)
 
 !## get rid of "\"'s, make them "_"'s
 RESDIR=RUN(IRUN)%FNAME(:INDEX(RUN(IRUN)%FNAME,'.',.TRUE.)-1)
  
 DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO
 
 DO IEXE=1,NEXE
  IF(EXE(IEXE)%IACT.EQ.1)THEN
   DO JEXE=IEXE+1,NEXE
    IF(IEXE.NE.JEXE.AND.EXE(JEXE)%IACT.EQ.1)THEN

     !## open new result folder
     RES=TRIM(OUTMAP)//'\'//TRIM(RESDIR)//'\'//TRIM(EXE(IEXE)%ALIAS)//'-'//TRIM(EXE(JEXE)%ALIAS)
     !## create folder
     CALL UTL_CREATEDIR(RES)

     WRITE(IUOUT,'(/131A1)') ('-',I=1,131)
     WRITE(IUOUTG,'(/131A1)') ('-',I=1,131)
     WRITE(IUOUT,'(A)') 'Results for:'//TRIM(RES)
     WRITE(IUOUTG,'(A)') 'Results for:'//TRIM(RES)
     WRITE(IUOUT,'(131A1)') ('-',I=1,131)
     WRITE(IUOUTG,'(131A1)') ('-',I=1,131)
     WRITE(IUOUT,'(A35,5(F15.1,A1))') 'Result',(PERC(I),'%',I=1,SIZE(PERC)) !,'ABS_MEAN_DIFF'
     WRITE(IUOUTG,'(A35,5(F15.1,A1))') 'Result',(PERC(I),'%',I=1,SIZE(PERC)) !,'ABS_MEAN_DIFF'
     
     !## see whether error occured
     RES=TRIM(OUTMAP)//'\'//TRIM(RESDIR)//'\'//TRIM(EXE(IEXE)%ALIAS)
     INQUIRE(FILE=TRIM(RES)//'\error.txt',EXIST=LEX1)
     RES=TRIM(OUTMAP)//'\'//TRIM(RESDIR)//'\'//TRIM(EXE(JEXE)%ALIAS)
     INQUIRE(FILE=TRIM(RES)//'\error.txt',EXIST=LEX2)
     IF(.NOT.LEX1.AND..NOT.LEX2)THEN
 
      DO IMAP=1,SIZE(CMAP)
       !## get list of idf's
       IF(EXE(IEXE)%IMAP.EQ.0)THEN
        RES=TRIM(OUTMAP)//'\'//TRIM(RESDIR)//'\'//TRIM(EXE(IEXE)%ALIAS)//'\'//TRIM(CMAP(IMAP))
       ELSE
        RES=TRIM(OUTMAP)//'\'//TRIM(RESDIR)//'\'//TRIM(EXE(IEXE)%ALIAS)//'\MODFLOW\'//TRIM(CMAP(IMAP))
       ENDIF
       IF(UTL_DIRINFO_POINTER(RES,TRIM(CMAP(IMAP))//'*l*.idf',LISTNAME,'F'))THEN
     
        DO I=1,SIZE(LISTNAME)

         !## read idf - exe 1
         IF(EXE(IEXE)%IMAP.EQ.0)THEN
          FNAME=TRIM(OUTMAP)//'\'//TRIM(RESDIR)//'\'//TRIM(EXE(IEXE)%ALIAS)//'\'//TRIM(CMAP(IMAP))//'\'//TRIM(LISTNAME(I))
         ELSE
          FNAME=TRIM(OUTMAP)//'\'//TRIM(RESDIR)//'\'//TRIM(EXE(IEXE)%ALIAS)//'\MODFLOW\'//TRIM(CMAP(IMAP))//'\'//TRIM(LISTNAME(I))
         ENDIF
         IF(IDFREAD(IDF(1),FNAME,1))THEN

          !## read idf - exe 2
          IF(EXE(JEXE)%IMAP.EQ.0)THEN
           FNAME=TRIM(OUTMAP)//'\'//TRIM(RESDIR)//'\'//TRIM(EXE(JEXE)%ALIAS)//'\'//TRIM(CMAP(IMAP))//'\'//TRIM(LISTNAME(I))
          ELSE
           FNAME=TRIM(OUTMAP)//'\'//TRIM(RESDIR)//'\'//TRIM(EXE(JEXE)%ALIAS)//'\'//TRIM(CMAP(IMAP))//'\MODFLOW\'//TRIM(LISTNAME(I))
          ENDIF

          IF(IDFREAD(IDF(2),FNAME,1))THEN
           !## diff
           
           !## define threshold for fluxes (higher threshold is required fluxes than for heads)
!           IDF(1)%DX=MIN(IDF(1)%DX,IDF(2)%DX)
!           IF(TRIM(CMAP(IMAP)).EQ.'head')THEN
!            THDX=THRESHOLD
!           ELSE
!            THDX=THRESHOLD*IDF(1)%DX
!           ENDIF
       
           IDF(1)%X=IDF(1)%X-IDF(2)%X
           IF(MAXVAL(ABS(IDF(1)%X)).GE.THRESHOLD)THEN
            FNAME=TRIM(OUTMAP)//'\'//TRIM(RESDIR)//'\'//TRIM(EXE(IEXE)%ALIAS)//'-'//TRIM(EXE(JEXE)%ALIAS)// &
                  '\diff_'//TRIM(LISTNAME(I))
            IF(.NOT.IDFWRITE(IDF(1),FNAME,1))CALL UTL_PRINTTEXT('Can not write '//TRIM(IDF(2)%FNAME),2)
            !## get statistics
            CALL UTL_GETMED(IDF(1)%X,IDF(1)%NCOL*IDF(1)%NROW,IDF(1)%NODATA,PERC,SIZE(PERC),NNODATA,XMED)
            !## get mean absolute difference, skip nodata
            XMABS=0.0; N=0
            DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
             IF(IDF(1)%X(ICOL,IROW).NE.IDF(1)%NODATA)THEN
              XMABS=XMABS+ABS(IDF(1)%X(ICOL,IROW)); N=N+1
             ENDIF
            ENDDO; ENDDO
            IF(N.GT.0)XMABS=XMABS/REAL(N)
           ELSE
            XMED=0.0
           ENDIF       
           WRITE(IUOUT,'(A35,5(F15.7,1X))') TRIM(LISTNAME(I)),(XMED(J),J=1,SIZE(PERC)) !,XMABS
           WRITE(IUOUTG,'(A35,5(F15.7,1X))') TRIM(LISTNAME(I)),(XMED(J),J=1,SIZE(PERC)) !,XMABS
          ENDIF
          CALL IDFDEALLOCATE(IDF,SIZE(IDF))
          
         ENDIF
        ENDDO
      
       ENDIF
      ENDDO
     ELSE

      !## see what error occured
      IF(LEX1)THEN
       IU=UTL_GETUNIT()
       RES=TRIM(OUTMAP)//'\'//TRIM(RESDIR)//'\'//TRIM(EXE(IEXE)%ALIAS)      
       OPEN(IU,FILE=TRIM(RES)//'\error.txt',STATUS='OLD',ACTION='READ',IOSTAT=IOS)
       CCODE='NaN'
       IF(IOS.EQ.0)THEN; READ(IU,*) CCODE; CLOSE(IU); ENDIF
       WRITE(IUOUT,'(2X,A)') TRIM(RES)//' did NOT run Successfully, EXITCODE= '//TRIM(CCODE)
       WRITE(IUOUTG,'(2X,A)') TRIM(RES)//' did NOT run Successfully, EXITCODE= '//TRIM(CCODE)
      ENDIF
      IF(LEX2)THEN
       IU=UTL_GETUNIT()
       RES=TRIM(OUTMAP)//'\'//TRIM(RESDIR)//'\'//TRIM(EXE(JEXE)%ALIAS)      
       OPEN(IU,FILE=TRIM(RES)//'\error.txt',STATUS='OLD',ACTION='READ',IOSTAT=IOS)
       CCODE='NaN'
       IF(IOS.EQ.0)THEN; READ(IU,*) CCODE; CLOSE(IU); ENDIF
       WRITE(IUOUT,'(2X,A)') TRIM(RES)//' did NOT run Successfully, EXITCODE= '//TRIM(CCODE)
       WRITE(IUOUTG,'(2X,A)') TRIM(RES)//' did NOT run Successfully, EXITCODE= '//TRIM(CCODE)
      ENDIF
     
     ENDIF
    ENDIF
   ENDDO
  ENDIF
 ENDDO
 
 IF(ASSOCIATED(LISTNAME))DEALLOCATE(LISTNAME)
 
 END SUBROUTINE TB_DIFFRUN
 
END MODULE MOD_TB_READCONF