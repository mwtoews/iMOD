MODULE MOD_AGGREGATE
 
 USE MOD_UTL, ONLY : UTL_CREATEDIR,UTL_IDFSNAPTOGRID_LLC
 USE MOD_IDF
 USE MOD_AGGREGATE_PAR

 CONTAINS

 !###===========================
 SUBROUTINE LHM_CONVERTREGIS_MAIN()
 !###===========================
 IMPLICIT NONE

 !## initialize
 CALL LHM_CONVERTREGIS_SORTFILES(FLIST,SORTFILE,INDIR,0)
 
 SELECT CASE (IMODE)
  !## make dataset consistent
  CASE (1); CALL LHM_CONVERTREGIS()
  !## include additional data
  CASE (2); CALL LHM_CONVERTREGIS_SORTFILES(ALIST,IWHBFILE,IWHBDIR,1); CALL LHM_ADDIWHB()
  !## aggregate dataset
  CASE (3); CALL LHM_CONVERTREGIS_AGGREGATE(); IF(IPRJ.EQ.1)CALL LHM_CONVERTREGIS_WRITEPRJ()
 END SELECT

 END SUBROUTINE LHM_CONVERTREGIS_MAIN

 !###===========================
 SUBROUTINE LHM_CONVERTREGIS_SORTFILES(SLIST,OFILE,DIR,IMETHOD)
 !###===========================
 IMPLICIT NONE
 TYPE(FOBJ),INTENT(INOUT),POINTER,DIMENSION(:) :: SLIST
 CHARACTER(LEN=*),INTENT(IN) :: OFILE,DIR
 INTEGER,INTENT(IN) :: IMETHOD
 INTEGER :: I,J,N,M,IU,IOS
 LOGICAL :: LEX
 
 !## open unit for same line printing of echo (is equal to screen or '*')
 OPEN(UNIT=6,CARRIAGECONTROL='FORTRAN') 

 IU=UTL_GETUNIT(); OPEN(IU,FILE=OFILE,STATUS='OLD',ACTION='READ'); ALLOCATE(SLIST(1)); READ(IU,*)
 DO I=1,2; M=1; N=1; DO
   IF(IMETHOD.EQ.0)THEN
    READ(IU,*,IOSTAT=IOS) (SLIST(M)%FILE(J),J=1,SIZE(SLIST(M)%FILE)); IF(IOS.NE.0)EXIT
   ELSE
    READ(IU,*,IOSTAT=IOS) (SLIST(M)%FILE(J),J=1,SIZE(SLIST(M)%FILE)),SLIST(M)%ITOP,SLIST(M)%IBOT,SLIST(M)%METH; IF(IOS.NE.0)EXIT
   ENDIF
   N=N+1; IF(I.EQ.2)M=N
  ENDDO
  IF(I.EQ.1)THEN
   DEALLOCATE(SLIST)
   IF(N-1.LE.0)THEN
    WRITE(*,'(/A/)') 'CANNOT FIND APPROPRIATE FILES FROM '//TRIM(OFILE); STOP
   ENDIF
   ALLOCATE(SLIST(N-1))
  ENDIF
  REWIND(IU); READ(IU,*)
 ENDDO; CLOSE(IU)

 !## check files
 DO I=1,SIZE(SLIST)
  DO J=1,SIZE(SLIST(I)%FILE)
   READ(SLIST(I)%FILE(J),*,IOSTAT=IOS) SLIST(I)%XVAL(J)
   IF(IOS.NE.0)THEN
    SLIST(I)%ITYPE(J)=0; SLIST(I)%XVAL(J)=HUGE(1.0D0)
    INQUIRE(FILE=TRIM(DIR)//'\'//TRIM(SLIST(I)%FILE(J)),EXIST=LEX)
    IF(.NOT.LEX)THEN; WRITE(*,'(/A/)') 'Cannot find file '//TRIM(INDIR)//'\'//TRIM(SLIST(I)%FILE(J)); STOP; ENDIF
    SLIST(I)%FILE(J)=UTL_CAP(SLIST(I)%FILE(J),'U')
   ELSE
    SLIST(I)%ITYPE(J)=1
   ENDIF
  ENDDO
 ENDDO
 CLOSE(IU)
 
 END SUBROUTINE LHM_CONVERTREGIS_SORTFILES

 !###===========================
 SUBROUTINE LHM_ADDIWHB()
 !###===========================
 IMPLICIT NONE
 INTEGER :: MF=10 !# maximum number of splitting per formation
 INTEGER :: I,J,L,N,M,P,IROW,ICOL,IU
 REAL(KIND=DP_KIND) :: F,T,B
 TYPE(IDFOBJ) :: IDF,TPIDF,BTIDF,KHIDF,KVIDF,VAIDF

 CALL IDFNULLIFY(TPIDF); CALL IDFNULLIFY(BTIDF); CALL IDFNULLIFY(KHIDF)
 CALL IDFNULLIFY(KVIDF); CALL IDFNULLIFY(IDF);   CALL IDFNULLIFY(VAIDF)
 
 !## specify window
 IF(IWINDOW.EQ.1)THEN
  IDF%XMIN=XMIN; IDF%XMAX=XMAX; IDF%YMIN=YMIN; IDF%YMAX=YMAX; IDF%DX=CELLSIZE; IDF%DY=IDF%DX
  CALL UTL_IDFSNAPTOGRID_LLC(IDF%XMIN,IDF%XMAX,IDF%YMIN,IDF%YMAX,IDF%DX,IDF%DY,IDF%NCOL,IDF%NROW,.TRUE.)
 ENDIF
 
 N=SIZE(FLIST); ALLOCATE(FM(N))
 
 P=0
 DO I=1,N

  !## read data
  IF(.NOT.LHM_ADDIWHB_READFLIST(IDF,TPIDF,BTIDF,KHIDF,KVIDF,VAIDF,SIZE(FLIST(I)%FILE),FLIST(I)%FILE,INDIR))STOP

  DO J=1,2
   M=0; DO IROW=1,TPIDF%NROW; DO ICOL=1,TPIDF%NCOL
    T=TPIDF%X(ICOL,IROW); B=BTIDF%X(ICOL,IROW)
    IF(T-B.GT.0.0D0)THEN
     M=M+1
     IF(J.EQ.2)THEN
      FM(I)%SF(1)%AT(M)%IROW=INT(IROW,2)
      FM(I)%SF(1)%AT(M)%ICOL=INT(ICOL,2)
      FM(I)%SF(1)%AT(M)%TP  =T
      FM(I)%SF(1)%AT(M)%BT  =B
      FM(I)%SF(1)%AT(M)%KH  =KHIDF%X(ICOL,IROW)
      FM(I)%SF(1)%AT(M)%KV  =KVIDF%X(ICOL,IROW)
     ENDIF
    ENDIF
   ENDDO; ENDDO
   IF(J.EQ.1)THEN; ALLOCATE(FM(I)%SF(MF)); ALLOCATE(FM(I)%SF(1)%AT(M)); ENDIF
  ENDDO
  
  P=P+M 
  CALL IDFDEALLOCATEX(TPIDF); CALL IDFDEALLOCATEX(KHIDF); CALL IDFDEALLOCATEX(KVIDF)
  CALL IDFDEALLOCATEX(BTIDF); CALL IDFDEALLOCATEX(VAIDF)

  F=100.0D0*DBLE(I)/DBLE(N)
  WRITE(*,'(1X,A)') 'READING '//TRIM(FLIST(I)%FILE(1))//'('//TRIM(RTOS(F,'F',2))//'%)'

 ENDDO

 !## modifying the solid
 DO I=1,SIZE(ALIST)
  !## read data
  IF(.NOT.LHM_ADDIWHB_READFLIST(IDF,TPIDF,BTIDF,KHIDF,KVIDF,VAIDF,SIZE(ALIST(I)%FILE),ALIST(I)%FILE,IWHBDIR))STOP 
  !## check maximum layer to insert layer
  L=LHM_ADDIWHB_GETMAXLAYER(TPIDF,BTIDF)
  IF(L.EQ.0)THEN
   WRITE(*,'(/A/)') 'CURRENT ADDED LAYER '//TRIM(ALIST(I)%FILE(1))//' CANNOT IS ENTIRELY BELOW BOTTOM OF ENTIRE MODEL'; STOP
  ENDIF
 ENDDO
 
 IF(.NOT.IDFALLOCATEX(TPIDF))STOP; IF(.NOT.IDFALLOCATEX(BTIDF))STOP
 IF(.NOT.IDFALLOCATEX(KHIDF))STOP; IF(.NOT.IDFALLOCATEX(KVIDF))STOP
 IF(VAIDF%NCOL.LE.0)CALL IDFCOPY(TPIDF,VAIDF); IF(.NOT.IDFALLOCATEX(VAIDF))STOP
 
 CALL UTL_CREATEDIR(OUTDIR)
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(OUTDIR)//'\IWHB_SORTFILE.TXT',STATUS='UNKNOWN',ACTION='WRITE')
 WRITE(IU,'(5A32)') 'TOP','BOT','KH','KV','VA'

 !## need to be completed ... fill with previous values
 IF(.NOT.LHM_ADDIWHB_READFLIST(IDF,TPIDF,BTIDF,KHIDF,KVIDF,VAIDF,SIZE(FLIST(1)%FILE),FLIST(1)%FILE,INDIR))STOP
 BTIDF%X=TPIDF%X
 
 !## number of formations
 DO I=1,N
  !## number of subformations
  DO J=1,SIZE(FM(I)%SF)
 
   M=SIZE(FM(I)%SF(J)%AT); IF(M.EQ.0)EXIT

   KHIDF%X=KHIDF%NODATA; KVIDF%X=KVIDF%NODATA; VAIDF%X=VAIDF%NODATA
    
   DO L=1,M
    IROW=INT(FM(I)%SF(J)%AT(L)%IROW,4)
    ICOL=INT(FM(I)%SF(J)%AT(L)%ICOL,4)
    TPIDF%X(ICOL,IROW)=FM(I)%SF(J)%AT(L)%TP
    BTIDF%X(ICOL,IROW)=FM(I)%SF(J)%AT(L)%BT  
    KHIDF%X(ICOL,IROW)=FM(I)%SF(J)%AT(L)%KH
    KVIDF%X(ICOL,IROW)=FM(I)%SF(J)%AT(L)%KV
    VAIDF%X(ICOL,IROW)=KVIDF%X(ICOL,IROW)/KHIDF%X(ICOL,IROW)
   ENDDO
    
   !## save idf files
   CALL LHM_CONVERTREGIS_OUTPUT(I,TPIDF,BTIDF,KHIDF,KVIDF,VAIDF) !,IBIDF)
   WRITE(IU,'(5A32)') (TRIM(FLIST(I)%FILE(L)),L=1,5)

   DO L=1,M
    IROW=INT(FM(I)%SF(J)%AT(L)%IROW,4)
    ICOL=INT(FM(I)%SF(J)%AT(L)%ICOL,4)
    TPIDF%X(ICOL,IROW)=FM(I)%SF(J)%AT(L)%BT
   ENDDO

  ENDDO

  F=100.0D0*DBLE(I)/DBLE(N)
  WRITE(*,'(1X,A)') 'WRITING '//TRIM(FLIST(I)%FILE(1))//'('//TRIM(RTOS(F,'F',2))//'%)'
 
 ENDDO
 CLOSE(IU)
 
 CALL IDFDEALLOCATEX(TPIDF); CALL IDFDEALLOCATEX(KHIDF); CALL IDFDEALLOCATEX(KVIDF)
 CALL IDFDEALLOCATEX(BTIDF); CALL IDFDEALLOCATEX(VAIDF)

 DO I=1,N
  DO J=1,SIZE(FM(I)%SF)
   IF(ASSOCIATED(FM(I)%SF(J)%AT))DEALLOCATE(FM(I)%SF(J)%AT)
  ENDDO
  DEALLOCATE(FM(I)%SF)
 ENDDO
 DEALLOCATE(FM)
 
 END SUBROUTINE LHM_ADDIWHB

 !###===========================
 INTEGER FUNCTION LHM_ADDIWHB_GETMAXLAYER(TPIDF,BTIDF)  
 !###===========================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: TPIDF,BTIDF
 INTEGER :: IROW,ICOL,I,J,K,M
 REAL(KIND=DP_KIND) :: T,B,BF
 
 LHM_ADDIWHB_GETMAXLAYER=0
 
 !## check formations
ILOOP: DO I=1,SIZE(FM)
  !## number of subformations
  DO J=1,SIZE(FM(I)%SF)
   M=SIZE(FM(I)%SF(J)%AT); IF(M.EQ.0)EXIT
   DO K=1,M
    IROW=INT(FM(I)%SF(J)%AT(K)%IROW,4)
    ICOL=INT(FM(I)%SF(J)%AT(K)%ICOL,4)
    
    T   =TPIDF%X(ICOL,IROW)
    B   =BTIDF%X(ICOL,IROW)
    !## no thickness of current iwhb layer
    IF(T-B.LE.0.0D0)CYCLE

!    TF  =FM(I)%SF(J)%AT(K)%TP
    BF  =FM(I)%SF(J)%AT(K)%BT
    IF(T.GE.BF)THEN !.AND.BF.LE.T)THEN
     !## current iwhb layer in this formation layer - stop is highest formation, no need to look further
     LHM_ADDIWHB_GETMAXLAYER=I; EXIT ILOOP
    ENDIF

   ENDDO
  ENDDO
 ENDDO ILOOP
  
 END FUNCTION LHM_ADDIWHB_GETMAXLAYER

 !###===========================
 LOGICAL FUNCTION LHM_ADDIWHB_READFLIST(IDF,TP,BT,KH,KV,VA,N,LIST,DIR)  
 !###===========================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF,TP,BT,KH,KV,VA
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER,INTENT(IN) :: N
 CHARACTER(LEN=256) :: FN
 INTEGER :: ICOL,IROW
 CHARACTER(LEN=*),DIMENSION(N) :: LIST
 
 LHM_ADDIWHB_READFLIST=.FALSE.
 
 FN=TRIM(DIR)//'\'//TRIM(LIST(1))
 IF(IWINDOW.EQ.1)THEN
  CALL IDFCOPY(IDF,TP); IF(.NOT.IDFREADSCALE(FN,TP,10,1,0.0D0,0))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
 ELSE
  IF(.NOT.IDFREAD(TP,FN,1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
 ENDIF
  
 FN=TRIM(DIR)//'\'//TRIM(LIST(2)) 
 IF(IWINDOW.EQ.1)THEN
  CALL IDFCOPY(IDF,BT); IF(.NOT.IDFREADSCALE(FN,BT,10,1,0.0D0,0))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
 ELSE
  IF(.NOT.IDFREAD(BT,FN,1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF
 ENDIF

 FN=TRIM(DIR)//'\'//TRIM(LIST(3)) 
 IF(IWINDOW.EQ.1)THEN
  CALL IDFCOPY(IDF,KH); IF(.NOT.IDFREADSCALE(FN,KH,10,1,0.0D0,0))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
 ELSE
  IF(.NOT.IDFREAD(KH,FN,1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF  
 ENDIF
  
 FN=TRIM(DIR)//'\'//TRIM(LIST(4)) 
 IF(IWINDOW.EQ.1)THEN
  CALL IDFCOPY(IDF,KV); IF(.NOT.IDFREADSCALE(FN,KV,10,1,0.0D0,0))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
 ELSE
  IF(.NOT.IDFREAD(KV,FN,1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF
 ENDIF
 
 !## if kh and kv are not both read, use va
 IF(TRIM(LIST(3)).EQ.''.OR.TRIM(LIST(4)).EQ.'')THEN
  FN=TRIM(DIR)//'\'//TRIM(LIST(5)) 
  IF(IWINDOW.EQ.1)THEN
   CALL IDFCOPY(IDF,VA); IF(.NOT.IDFREADSCALE(FN,VA,10,1,0.0D0,0))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
  ELSE
   IF(.NOT.IDFREAD(VA,FN,1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF
  ENDIF
  !## fill in values for kh or kv
  IF(TRIM(LIST(3)).EQ.'')THEN
   DO IROW=1,VA%NROW; DO ICOL=1,VA%NCOL
    IF(VA%X(ICOL,IROW).NE.VA%NODATA.AND.KH%X(ICOL,IROW).NE.KH%NODATA)THEN
     KV%X(ICOL,IROW)=KH%X(ICOL,IROW)*VA%X(ICOL,IROW)
    ENDIF
   ENDDO; ENDDO
  ELSE
   DO IROW=1,VA%NROW; DO ICOL=1,VA%NCOL
    IF(VA%X(ICOL,IROW).NE.VA%NODATA.AND.KV%X(ICOL,IROW).NE.KH%NODATA)THEN
     KH%X(ICOL,IROW)=KV%X(ICOL,IROW)/VA%X(ICOL,IROW)
    ENDIF
   ENDDO; ENDDO
  ENDIF
 ENDIF
 
 LHM_ADDIWHB_READFLIST=.TRUE.
 
 END FUNCTION LHM_ADDIWHB_READFLIST 

 !###===========================
 SUBROUTINE LHM_CONVERTREGIS()
 !###===========================
 IMPLICIT NONE
 INTEGER :: I,J,N,ICOL,IROW,IU
 REAL(KIND=DP_KIND) :: T,B,K1,K2,X,Y
 TYPE(IDFOBJ),DIMENSION(:,:),ALLOCATABLE :: TPK
 TYPE(IDFOBJ) :: TPIDF,BTIDF,KHIDF,KVIDF,VAIDF,IBIDF
  
 CALL UTL_CREATEDIR(OUTDIR)
 
 N=SIZE(FLIST); ALLOCATE(TPK(N,5)) 
 DO I=1,N; DO J=1,5; CALL IDFNULLIFY(TPK(I,J)); ENDDO; ENDDO
 CALL IDFNULLIFY(TPIDF); CALL IDFNULLIFY(BTIDF); CALL IDFNULLIFY(KHIDF)
 CALL IDFNULLIFY(KVIDF); CALL IDFNULLIFY(VAIDF); CALL IDFNULLIFY(IBIDF)
 
 DO I=1,N
  DO J=1,SIZE(FLIST(I)%FILE)
   WRITE(6,*) 'Reading '//TRIM(FLIST(I)%FILE(J))//' ...'
   IF(FLIST(I)%ITYPE(J).EQ.0)THEN
    TPK(I,J)%FNAME=TRIM(INDIR)//'\'//TRIM(FLIST(I)%FILE(J))
    IF(.NOT.IDFREAD(TPK(I,J),TPK(I,J)%FNAME,0))THEN; WRITE(6,'(/A/)') 'ERROR READING '//TRIM(TPK(I,J)%FNAME); STOP; ENDIF
   ENDIF
  ENDDO
 ENDDO

 IF(IWINDOW.EQ.1)THEN
  TPIDF%XMIN=XMIN; TPIDF%XMAX=XMAX; TPIDF%YMIN=YMIN; TPIDF%YMAX=YMAX; TPIDF%DX=CELLSIZE; TPIDF%DY=TPIDF%DX
  CALL UTL_IDFSNAPTOGRID_LLC(TPIDF%XMIN,TPIDF%XMAX,TPIDF%YMIN,TPIDF%YMAX,TPIDF%DX,TPIDF%DY,TPIDF%NCOL,TPIDF%NROW,.TRUE.)
 ELSE
  CALL IDFCOPY(TPK(1,1),TPIDF)
 ENDIF

 CALL IDFCOPY(TPIDF,BTIDF); CALL IDFCOPY(TPIDF,KHIDF); CALL IDFCOPY(TPIDF,KVIDF); CALL IDFCOPY(TPIDF,IBIDF); CALL IDFCOPY(TPIDF,VAIDF)
 IF(.NOT.IDFALLOCATEX(TPIDF))STOP; IF(.NOT.IDFALLOCATEX(BTIDF))STOP; IF(.NOT.IDFALLOCATEX(KHIDF))STOP
 IF(.NOT.IDFALLOCATEX(KVIDF))STOP; IF(.NOT.IDFALLOCATEX(IBIDF))STOP; IF(.NOT.IDFALLOCATEX(VAIDF))STOP

 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(OUTDIR)//'\CONSISTENT_SORTFILE.TXT',STATUS='UNKNOWN',ACTION='WRITE')
 WRITE(IU,'(5A32)') 'TOP','BOT','KH','KV','VA'
 
  !## fill in values
 TPIDF%X=TPIDF%NODATA; BTIDF%X=BTIDF%NODATA; KHIDF%X=KHIDF%NODATA; KVIDF%X=KVIDF%NODATA; IBIDF%X=IBIDF%NODATA; VAIDF%X=VAIDF%NODATA
 DO I=1,N

  DO IROW=1,TPIDF%NROW; DO ICOL=1,TPIDF%NCOL
   CALL IDFGETLOC(TPIDF,IROW,ICOL,X,Y)

   !## nodata remains nodata 
   IF(I.GT.1.AND.TPIDF%X(ICOL,IROW).EQ.TPIDF%NODATA)CYCLE

   IF(FLIST(I)%ITYPE(1).EQ.1)THEN; T=FLIST(I)%XVAL(1); ELSE; T=IDFGETXYVAL(TPK(I,1),X,Y); ENDIF
   IF(FLIST(I)%ITYPE(2).EQ.1)THEN; B=FLIST(I)%XVAL(2); ELSE; B=IDFGETXYVAL(TPK(I,2),X,Y); ENDIF
   !## look for first no data
   IF(T.EQ.TPK(I,1)%NODATA)THEN
    IF(I.EQ.1)THEN
     !## check below
     DO J=I+1,N
      IF(FLIST(J)%ITYPE(1).EQ.1)THEN; T=FLIST(J)%XVAL(1); ELSE; T=IDFGETXYVAL(TPK(J,1),X,Y); ENDIF
      IF(T.NE.TPK(J,1)%NODATA)THEN
       B=T; EXIT
      ENDIF
     ENDDO
    ELSE
     T=BTIDF%X(ICOL,IROW); B=BTIDF%X(ICOL,IROW)
    ENDIF
    K1=TPK(I,3)%NODATA; K2=TPK(I,4)%NODATA
   ELSE
    IF(FLIST(I)%ITYPE(3).EQ.1)THEN; K1=FLIST(I)%XVAL(3); ELSE; K1=IDFGETXYVAL(TPK(I,3),X,Y); ENDIF
    IF(FLIST(I)%ITYPE(4).EQ.1)THEN; K2=FLIST(I)%XVAL(4); ELSE; K2=IDFGETXYVAL(TPK(I,4),X,Y); ENDIF
    IF(K1.EQ.TPK(I,3)%NODATA)K1=EPS; IF(K2.EQ.TPK(I,4)%NODATA)K2=EPS
    IF(K1.EQ.0.0D0)K1=K2; IF(K2.EQ.0.0D0)K2=K1
    IF(K1.EQ.0.0D0.AND.K2.EQ.0.0D0)THEN
     K1=TPK(I,3)%NODATA; K2=K1; T=BTIDF%X(ICOL,IROW); B=BTIDF%X(ICOL,IROW)
    ELSE
     !## make sure it connects
     IF(I.GT.1)T=BTIDF%X(ICOL,IROW)
    ENDIF
   ENDIF

   TPIDF%X(ICOL,IROW)=T; BTIDF%X(ICOL,IROW)=B; IBIDF%X(ICOL,IROW)=1
   IF(K1.NE.TPK(I,3)%NODATA.AND.K2.NE.TPK(I,4)%NODATA)THEN
    KHIDF%X(ICOL,IROW)=K1; KVIDF%X(ICOL,IROW)=K2; VAIDF%X(ICOL,IROW)=K2/K1
   ELSE
    KHIDF%X(ICOL,IROW)=1.0D0; KVIDF%X(ICOL,IROW)=1.0D0; VAIDF%X(ICOL,IROW)=1.0D0
   ENDIF
   
  ENDDO; WRITE(6,'(A,F10.3,A)') '+Progress ',REAL(100*IROW)/REAL(TPIDF%NROW),'%     '; ENDDO

  DO J=1,5; FLIST(I)%FILE(J)=TRIM(ITOS(I))//'_'//FLIST(I)%FILE(J); ENDDO
  CALL LHM_CONVERTREGIS_OUTPUT(I,TPIDF,BTIDF,KHIDF,KVIDF,VAIDF) !,IBIDF)
  WRITE(IU,'(5A32)') (TRIM(FLIST(I)%FILE(J)),J=1,5)

 ENDDO
 CLOSE(IU)
 
 END SUBROUTINE LHM_CONVERTREGIS

 !###===========================
 SUBROUTINE LHM_CONVERTREGIS_OUTPUT(I,TPIDF,BTIDF,KHIDF,KVIDF,VAIDF) !,IBIDF)
 !###===========================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I
 TYPE(IDFOBJ),INTENT(INOUT) :: TPIDF,BTIDF,KHIDF,KVIDF,VAIDF !,IBIDF
 INTEGER :: IROW,ICOL,J
 REAL(KIND=DP_KIND) :: K
 LOGICAL :: LEX
 CHARACTER(LEN=24) :: FTXT
 
 TPIDF%FNAME=TRIM(OUTDIR)//'\'//TRIM(FLIST(I)%FILE(1)); WRITE(*,'(A)') 'WRITING '//TRIM(TPIDF%FNAME)
 IF(.NOT.IDFWRITE(TPIDF,TPIDF%FNAME,1,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(TPIDF%FNAME); STOP; ENDIF  
 BTIDF%FNAME=TRIM(OUTDIR)//'\'//TRIM(FLIST(I)%FILE(2)); WRITE(*,'(A)') 'WRITING '//TRIM(BTIDF%FNAME)
 IF(.NOT.IDFWRITE(BTIDF,BTIDF%FNAME,1,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(BTIDF%FNAME); STOP; ENDIF  
 KHIDF%FNAME=TRIM(OUTDIR)//'\'//TRIM(FLIST(I)%FILE(3)); WRITE(*,'(A)') 'WRITING '//TRIM(KHIDF%FNAME)
 IF(.NOT.IDFWRITE(KHIDF,KHIDF%FNAME,1,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(KHIDF%FNAME); STOP; ENDIF  
 KVIDF%FNAME=TRIM(OUTDIR)//'\'//TRIM(FLIST(I)%FILE(4)); WRITE(*,'(A)') 'WRITING '//TRIM(KVIDF%FNAME)
 IF(.NOT.IDFWRITE(KVIDF,KVIDF%FNAME,1,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(KVIDF%FNAME); STOP; ENDIF  
 !## check whether this is variable
 LEX=.TRUE.; K=VAIDF%NODATA; DO IROW=1,VAIDF%NROW; DO ICOL=1,VAIDF%NCOL
  IF(VAIDF%X(ICOL,IROW).NE.VAIDF%NODATA)THEN
   IF(K.EQ.VAIDF%NODATA)K=VAIDF%X(ICOL,IROW)
   IF(K.NE.VAIDF%X(ICOL,IROW))THEN; LEX=.FALSE.; EXIT; ENDIF
  ENDIF
 ENDDO; IF(.NOT.LEX)EXIT; ENDDO
 IF(.NOT.LEX)THEN
  IF(INDEX(FLIST(I)%FILE(5),'.IDF').LE.0)THEN
   J=INDEX(FLIST(I)%FILE(1),'-'); FTXT=FLIST(I)%FILE(1)(:J-1); FLIST(I)%FILE(5)=TRIM(FTXT)//'-VA-C.IDF' 
  ENDIF
  VAIDF%FNAME=TRIM(OUTDIR)//'\'//TRIM(FLIST(I)%FILE(5)); WRITE(*,'(A)') 'WRITING '//TRIM(VAIDF%FNAME)
  IF(.NOT.IDFWRITE(VAIDF,VAIDF%FNAME,1,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(VAIDF%FNAME); STOP; ENDIF  
 ELSE
  WRITE(FLIST(I)%FILE(5),*) K
 ENDIF
 
 END SUBROUTINE LHM_CONVERTREGIS_OUTPUT
 
 !###===========================
 SUBROUTINE LHM_CONVERTREGIS_AGGREGATE()
 !###===========================
 IMPLICIT NONE
 INTEGER :: III,II,I,J,JJ,N,BSIZE,NODES,INODE
 REAL(KIND=DP_KIND) :: KE,CE
 REAL(KIND=DP_KIND),DIMENSION(2) :: TKDW,TVCW
 REAL(KIND=DP_KIND),DIMENSION(:,:),ALLOCATABLE :: TOP,KHV,KVV,KVA
 REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: VCV,KDW
 TYPE(IDFOBJ),DIMENSION(:,:),ALLOCATABLE :: TP,KH,KV
 REAL(KIND=SP_KIND),DIMENSION(:),ALLOCATABLE :: X
  
 WRITE(OUTPUTDIR,'(A,I5.5)') TRIM(OUTDIR)//'\CMIN_',INT(CMIN)
 CALL UTL_CREATEDIR(OUTPUTDIR)
 
 N=SIZE(FLIST); ALLOCATE(TP(N+1,2),KH(N,2),KV(N,2))
 DO I=1,N+1; DO J=1,2; CALL IDFNULLIFY(TP(I,J)); ENDDO; ENDDO
 DO I=1,N;   DO J=1,2; CALL IDFNULLIFY(KH(I,J)); CALL IDFNULLIFY(KV(I,J)); ENDDO; ENDDO

 DO I=1,N
  
!  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(OUTDIR)//'\'//TRIM(FNAME)//'-T_L'//TRIM(ITOS(I))//'.IDF'
  IF(.NOT.IDFREAD(TP(I,1),FNAME,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF 
!  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-T_L'//TRIM(ITOS(I))//'.IDF'
  CALL IDFCOPY(TP(I,1),TP(I,2))
  IF(.NOT.IDFWRITE(TP(I,2),FNAME,1,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF
  
!  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(OUTDIR)//'\'//TRIM(FNAME)//'-KH_L'//TRIM(ITOS(I))//'.IDF'
  IF(.NOT.IDFREAD(KH(I,1),FNAME,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF  
!  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-KH_L'//TRIM(ITOS(I))//'.IDF'
  CALL IDFCOPY(KH(I,1),KH(I,2))
  IF(.NOT.IDFWRITE(KH(I,2),FNAME,1,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF

!  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(OUTDIR)//'\'//TRIM(FNAME)//'-KV_L'//TRIM(ITOS(I))//'.IDF'
  IF(.NOT.IDFREAD(KV(I,1),FNAME,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF
!  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-VA_L'//TRIM(ITOS(I))//'.IDF'
  CALL IDFCOPY(KV(I,1),KV(I,2))
  IF(.NOT.IDFWRITE(KV(I,2),FNAME,1,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF
 
  IF(I.EQ.N)THEN
!   FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(OUTDIR)//'\'//TRIM(FNAME)//'-B_L'//TRIM(ITOS(I))//'.IDF'
   IF(.NOT.IDFREAD(TP(I+1,1),FNAME,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF
!   FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-B_L'//TRIM(ITOS(I))//'.IDF'
   CALL IDFCOPY(TP(I+1,1),TP(I+1,2))
   IF(.NOT.IDFWRITE(TP(I+1,2),FNAME,1,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF
  ENDIF
 
 ENDDO
 
 ALLOCATE(TOP(N+1,BLOCKSIZE),KHV(N,BLOCKSIZE),KVV(N,BLOCKSIZE),KVA(N,BLOCKSIZE),VCV(N),KDW(N),X(BLOCKSIZE))
 
 NODES=TP(1,1)%NROW*TP(1,1)%NCOL
 INODE=0
 DO 
  
  BSIZE=MIN(BLOCKSIZE,NODES-INODE)

  !## finished
  IF(BSIZE.LE.0)EXIT
  
  !## read variables at currrent location
  DO I=1,N
   IF(TP(I,1)%ITYPE.EQ.8)THEN
    READ(TP(I,1)%IU) (TOP(I,J),J=1,BSIZE)
   ELSE
    READ(TP(I,1)%IU) (X(J),J=1,BSIZE)
    DO J=1,BSIZE; TOP(I,J)=X(J); ENDDO
   ENDIF
   IF(KH(I,1)%IU.GT.0)THEN
    IF(KH(I,1)%ITYPE.EQ.8)THEN
     READ(KH(I,1)%IU) (KHV(I,J),J=1,BSIZE)
    ELSE
     READ(KH(I,1)%IU) (X(J),J=1,BSIZE)
     DO J=1,BSIZE; KHV(I,J)=X(J); ENDDO
    ENDIF
   ENDIF
   IF(KV(I,1)%IU.GT.0)THEN
    IF(KV(I,1)%ITYPE.EQ.8)THEN
     READ(KV(I,1)%IU) (KVV(I,J),J=1,BSIZE)
    ELSE
     READ(KV(I,1)%IU) (X(J),J=1,BSIZE)
     DO J=1,BSIZE; KVV(I,J)=X(J); ENDDO
    ENDIF
   ENDIF
  ENDDO
  IF(TP(N+1,1)%ITYPE.EQ.8)THEN
   READ(TP(N+1,1)%IU) (TOP(N+1,J),J=1,BSIZE)
  ELSE
   READ(TP(N+1,1)%IU) (X(J),J=1,BSIZE)
   DO J=1,BSIZE; TOP(I,J)=X(J); ENDDO
  ENDIF

  DO J=1,BSIZE

   !## compute c- and t-values
   DO I=1,N
    VCV(I)=(TOP(I,J)-TOP(I+1,J))/KVV(I,J)
    KDW(I)=(TOP(I,J)-TOP(I+1,J))*KHV(I,J)
   ENDDO
   TKDW(1)=SUM(KDW); TVCW(1)=SUM(VCV)
  
   !## skip this as it is all nodata
   IF(TKDW(1).LE.0.0)CYCLE
  
   !## aggregate from layer 1 onwards
   I=0; DO
    I=I+1
    DO II=I+1,N
     IF(VCV(II).GT.CMIN.OR.II.EQ.N)THEN
      !## shift bottoms down
      JJ=1; IF(II.EQ.N)JJ=0
      DO III=I+1,II-JJ !1
       TOP(III,J)=TOP(II,J)
       VCV(I)    =VCV(I)+VCV(III); VCV(III)=0.0
       KDW(I)    =KDW(I)+KDW(III); KDW(III)=0.0
      ENDDO
      EXIT
     ENDIF
    ENDDO
    I=II
    IF(I.GE.N)EXIT
   ENDDO
   
   TKDW(2)=SUM(KDW); TVCW(2)=SUM(VCV)
  
   KE=100.0*(TKDW(1)-TKDW(2))/TKDW(1)
   CE=100.0*(TVCW(1)-TVCW(2))/TVCW(1)
   IF(KE.GT.KERROR.OR.CE.GT.CERROR)THEN
    WRITE(*,'(/A)') 'Something went wrong'
    WRITE(*,*) TKDW(1),TKDW(2),TKDW(1)-TKDW(2),KE
    WRITE(*,*) TVCW(1),TVCW(2),TVCW(1)-TVCW(2),CE
   ENDIF
  
   !## compute khv and kvv-values
   DO I=1,N
    IF((TOP(I,J)-TOP(I+1,J)).GT.0.0)THEN
     KHV(I,J)=KDW(I)/(TOP(I,J)-TOP(I+1,J))
     KVV(I,J)=(TOP(I,J)-TOP(I+1,J))*VCV(I)
    ELSE
     KHV(I,J)=EPS
     KVV(I,J)=EPS
    ENDIF
   ENDDO  
  ENDDO
  
  !## save variables at currrent location
  DO I=1,N
   IF(TP(I,1)%ITYPE.EQ.8)THEN
    WRITE(TP(I,1)%IU) (TOP(I,J),J=1,BSIZE)
   ELSE
    DO J=1,BSIZE; X(J)=TOP(I,J); ENDDO
    WRITE(TP(I,2)%IU) (X(J),J=1,BSIZE)
   ENDIF
   IF(KH(I,1)%ITYPE.EQ.8)THEN
    WRITE(KH(I,2)%IU) (KHV(I,J),J=1,BSIZE)
   ELSE
    DO J=1,BSIZE; X(J)=KHV(I,J); ENDDO
    WRITE(KH(I,2)%IU) (X(J),J=1,BSIZE)
   ENDIF
   DO J=1,BSIZE; KVA(I,J)=KVV(I,J)/KHV(I,J); ENDDO
   IF(KV(I,1)%ITYPE.EQ.8)THEN
    WRITE(KV(I,2)%IU) (KVA(I,J),J=1,BSIZE)
   ELSE
    DO J=1,BSIZE; X(J)=KVA(I,J); ENDDO
    WRITE(KV(I,2)%IU) (X(J),J=1,BSIZE)
   ENDIF
  ENDDO
  IF(TP(N+1,1)%ITYPE.EQ.8)THEN
   WRITE(TP(N+1,2)%IU) (TOP(I,J),J=1,BSIZE)
  ELSE
   DO J=1,BSIZE; X(J)=TOP(I,J); ENDDO
   WRITE(TP(N+1,2)%IU) (X(J),J=1,BSIZE)
  ENDIF
 
  INODE=INODE+BSIZE

  WRITE(6,'(A,F10.3,A)') '+Progress Aggregation ',REAL(100*INODE)/REAL(NODES),'%     '

 ENDDO
 
 DEALLOCATE(TOP,KHV,KVV,VCV,KDW,X)
 
 END SUBROUTINE LHM_CONVERTREGIS_AGGREGATE

 !###===========================
 SUBROUTINE LHM_CONVERTREGIS_WRITEPRJ()
 !###===========================
 IMPLICIT NONE
 INTEGER :: I,IU,N
 
 N=SIZE(FLIST)
 CALL UTL_CREATEDIR(OUTPUTDIR)
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(OUTPUTDIR)//'\MODEL_v5.PRJ',STATUS='UNKNOWN',ACTION='WRITE')

 WRITE(IU,'(/A)') '0001,(BND),1, BOUNDARY CONDITION,[BND]'
 WRITE(IU,'(A,I10)') '001,',N
 DO I = 1,N
!  FNAME = FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:)
  FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-IB_L'//TRIM(ITOS(I))//'.IDF'
  WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
 ENDDO
 
 WRITE(IU,'(/A)') '0001,(TOP),1, TOP ELEVATION,[TOP]'
 WRITE(IU,'(A,I10)') '001,',N
 DO I = 1,N
!  FNAME = FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:)
  FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-T_L'//TRIM(ITOS(I))//'.IDF'
  WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
 ENDDO
 
 WRITE(IU,'(/A)') '0001,(BOT),1, BOTTOM ELEVATION,[BOT]'
 WRITE(IU,'(A,I10)') '001,',N
 DO I = 1,N
  IF(I.EQ.N)THEN
!   FNAME = FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:)
   FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-B_L'//TRIM(ITOS(I))//'.IDF'
   WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
  ELSE
!   FNAME = FLIST(I+1)(INDEX(FLIST(I+1),'\',.TRUE.)+1:)
   FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-T_L'//TRIM(ITOS(I+1))//'.IDF'
   WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
  ENDIF
 ENDDO

 WRITE(IU,'(/A)') '0001,(KHV),1, HORIZONTAL PERMEABILITY,[KHV]'
 WRITE(IU,'(A,I10)') '001,',N
 DO I = 1,N
!  FNAME = FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:)
  FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-KH_L'//TRIM(ITOS(I))//'.IDF'
  WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
 ENDDO

 WRITE(IU,'(/A)') '0001,(KVA),1, VERTICAL ANISOTROPY,[KVA]'
 WRITE(IU,'(A,I10)') '001,',N
 
 DO I = 1,N
!  FNAME = FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:)
  FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-VA_L'//TRIM(ITOS(I))//'.IDF'
  WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
 ENDDO
 
 WRITE(IU,'(/A)') '0001,(KVV),1, VERTICAL PERMEABILITY,[KVV]'
 WRITE(IU,'(A,I10)') '001,',N
 
 DO I = 1,N
  WRITE(IU,'(A,I3,A)') '1,1,',I,',1.0,0.0,1.0,""'
 ENDDO
 
 WRITE(IU,'(/A)') '0001 ,(SHD),1, STARTING HEADS,[SHD]'
 WRITE(IU,'(A,I10)') '001,',N
 
 DO I = 1,N
!  FNAME = FLIST(1)(INDEX(FLIST(1),'\',.TRUE.)+1:)
  FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-T_L'//TRIM(ITOS(1))//'.IDF'
  WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
 ENDDO

!! WRITE(IU,'(/A)') '0001,(RIV),1, RIVER'
! WRITE(IU,'(A)') 'STEADY-STATE'
! WRITE(IU,'(A)') '004,005'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\COND_HL1_250.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\COND_HL2_250.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\COND_P_L0.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\COND_S_L0.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\COND_T_L0.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\PEIL_HW_250.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\PEIL_HW_250.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\PEIL_P1W_250.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\PEIL_S1W_250.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\PEIL_T1W_250.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\BOTH_W_L1.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\BOTH_W_L2.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\BODH_P1W_250.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\BODH_S1W_250.IDF' 
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\PEIL_T1W_250.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\INFMZ_H_250_L1.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\INFMZ_H_250_L2.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\INFMZ_P1_250.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\INFMZ_S1_250.IDF'
! WRITE(IU,'(A)') '1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\INFMZ_T1_250.IDF'
! 
! WRITE(IU,'(/A)') '0001,(DRN),1, DRAINAGE'
! WRITE(IU,'(A)') 'STEADY-STATE'
! WRITE(IU,'(A)') '002,001'
! WRITE(IU,'(A)') ' 1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\COND_B_250.IDF'
! WRITE(IU,'(A)') ' 1,2, -001,   1.000000    ,   0.000000    ,   8.000000    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\BODH_B_250.IDF'
! 
! WRITE(IU,'(/A)') '0001,(GHB),1, GENERAL HEAD BOUNDARY,[CON,LVL]'
! WRITE(IU,'(A)') 'STEADY-STATE'
! WRITE(IU,'(A)') '002,001'
! WRITE(IU,'(A)') ' 1,2, -001,   1.000000    ,   0.000000    ,  -999.9900    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\GHB_COND_REGIS_L01.IDF'
! WRITE(IU,'(A)') ' 1,2, -001,   1.000000    ,   0.000000    ,   8.000000    ,'//TRIM(WORKDIR)//'\LHM\LHMV1\GHB_STAGE_REGIS_L01.IDF'
! 
! WRITE(IU,'(/A)') '0001,(RCH),1, RECHARGE,[RCH]'
! WRITE(IU,'(A)') 'STEADY-STATE'
! WRITE(IU,'(A)') '001,001'
! WRITE(IU,'(A)') ' 1,1, -001,   1.000000    ,   0.000000    ,   0.700000    ,""'
 
 WRITE(IU,'(/A)') ' 0001,(PCG),1, PRECONDITION CONJUGATE-GRADIENT []'
 WRITE(IU,'(A)')  '50,150,0.10000E-02,10.00000,0.98000,1,0,1,1.0000,1.0000,1,5.00000'
 
 WRITE(IU,'(/A)') 'PERIODS'
 CLOSE(IU)
 
 END SUBROUTINE LHM_CONVERTREGIS_WRITEPRJ
 
END MODULE MOD_AGGREGATE