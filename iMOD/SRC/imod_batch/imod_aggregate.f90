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
 CALL LHM_CONVERTREGIS_INIT_FLIST()
 !## make dataset consistent
 IF(ICONSISTENT.EQ.1)CALL LHM_CONVERTREGIS()
 !## include additional data
 IF(IWHB.EQ.1)CALL LHM_ADDIWHB()
 !## aggregate dataset
 IF(IAGGREGATE.EQ.1)CALL LHM_CONVERTREGIS_AGGREGATE()
 !## write prj file
 IF(IPRJ.EQ.1)CALL LHM_CONVERTREGIS_WRITEPRJ()

 END SUBROUTINE LHM_CONVERTREGIS_MAIN

 !###===========================
 SUBROUTINE LHM_CONVERTREGIS_INIT_FLIST()
 !###===========================
 IMPLICIT NONE
 INTEGER :: I,N,IU,IOS

 !## open unit for same line printing of echo (is equal to screen or '*')
 OPEN(UNIT=6,CARRIAGECONTROL='FORTRAN') 

 IU=UTL_GETUNIT(); OPEN(IU,FILE=SORTFILE,STATUS='OLD',ACTION='READ')
 DO I=1,2; N=0; DO
   READ(IU,*,IOSTAT=IOS) FNAME; IF(IOS.NE.0)EXIT
   N=N+1; IF(I.EQ.2)FLIST(N)=TRIM(INDIR)//'\'//TRIM(FNAME)
  ENDDO; IF(I.EQ.1)ALLOCATE(FLIST(N))
  REWIND(IU)
 ENDDO; CLOSE(IU)
 
 END SUBROUTINE LHM_CONVERTREGIS_INIT_FLIST
 
 !###===========================
 SUBROUTINE LHM_ADDIWHB()
 !###===========================
 IMPLICIT NONE
 INTEGER :: MF=10 !# maximum number of splitting per formation
 INTEGER :: I,J,K,L,N,M,P,IROW,ICOL
 REAL(KIND=DP_KIND) :: F,T,B
 TYPE(IDFOBJ) :: IDF,TP,BT,KH,KV
 TYPE ATOBJ
  INTEGER(KIND=2) :: IROW,ICOL
  REAL(KIND=SP_KIND) :: TP,BT,KH,KV
 END TYPE ATOBJ
 TYPE FMMOBJ
  TYPE(ATOBJ),POINTER,DIMENSION(:) :: AT => NULL()
 END TYPE FMMOBJ
 TYPE FMOBJ
  TYPE(FMMOBJ),POINTER,DIMENSION(:) :: SF => NULL()
 END TYPE FMOBJ
 TYPE(FMOBJ),ALLOCATABLE,DIMENSION(:) :: FM

 CALL IDFNULLIFY(TP); CALL IDFNULLIFY(BT); CALL IDFNULLIFY(KH); CALL IDFNULLIFY(KV); CALL IDFNULLIFY(IDF)
 
 !## specify window
 IF(IWINDOW.EQ.1)THEN
  IDF%XMIN=XMIN; IDF%XMAX=XMAX; IDF%YMIN=YMIN; IDF%YMAX=YMAX; IDF%DX=CELLSIZE; IDF%DY=IDF%DX
  CALL UTL_IDFSNAPTOGRID_LLC(IDF%XMIN,IDF%XMAX,IDF%YMIN,IDF%YMAX,IDF%DX,IDF%DY,IDF%NCOL,IDF%NROW,.TRUE.)
 ENDIF
 
 N=SIZE(FLIST); ALLOCATE(FM(N))
 
 P=0
 DO I=1,N

  !## read data
  IF(.NOT.LHM_ADDIWHB_READFLIST(IDF,TP,BT,KH,KV,I,SIZE(FLIST),FLIST))STOP

  DO J=1,2
   M=0; DO IROW=1,TP%NROW; DO ICOL=1,TP%NCOL
    T=TP%X(ICOL,IROW)
    B=BT%X(ICOL,IROW)
    IF(T-B.GT.0.0D0)THEN
     M=M+1
     IF(J.EQ.2)THEN
      FM(I)%SF(1)%AT(M)%IROW=INT(IROW,2)
      FM(I)%SF(1)%AT(M)%ICOL=INT(ICOL,2)
      FM(I)%SF(1)%AT(M)%TP  =T
      FM(I)%SF(1)%AT(M)%BT  =B
      FM(I)%SF(1)%AT(M)%KH  =KH%X(ICOL,IROW)
      FM(I)%SF(1)%AT(M)%KV  =KV%X(ICOL,IROW)
     ENDIF
    ENDIF
   ENDDO; ENDDO
   IF(J.EQ.1)THEN; ALLOCATE(FM(I)%SF(MF)); ALLOCATE(FM(I)%SF(1)%AT(M)); ENDIF
  ENDDO
  
  P=P+M 
  CALL IDFDEALLOCATEX(TP); CALL IDFDEALLOCATEX(KH); CALL IDFDEALLOCATEX(KV); CALL IDFDEALLOCATEX(BT)

  F=100.0D0*DBLE(I)/DBLE(N)
  WRITE(*,'(1X,A)') 'READING '//TRIM(FLIST(I))//'('//TRIM(RTOS(F,'F',2))//'%)'

 ENDDO

 !## modifying the solid
 DO J=1,SIZE(ALIST)
  !## need to be in order of sort
  I=J
  !## read data
  IF(.NOT.LHM_ADDIWHB_READFLIST(IDF,TP,BT,KH,KV,I,SIZE(ALIST),ALIST))STOP 

 ENDDO
 
 IF(.NOT.IDFALLOCATEX(TP))STOP; IF(.NOT.IDFALLOCATEX(BT))STOP
 IF(.NOT.IDFALLOCATEX(KH))STOP; IF(.NOT.IDFALLOCATEX(KV))STOP
 
 !## need to be completed ... fill with previous values
 FNAME=FLIST(1)(INDEX(FLIST(1),'\',.TRUE.)+1:); FNAME=TRIM(CONDIR)//'\'//TRIM(FNAME)//'-T_L'//TRIM(ITOS(1))//'.IDF'
 IF(IWINDOW.EQ.1)THEN
  CALL IDFCOPY(IDF,TP)
  IF(.NOT.IDFREADSCALE(FNAME,TP,10,1,0.0D0,0))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF 
 ELSE
  IF(.NOT.IDFREAD(TP,FNAME,1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF 
 ENDIF
 BT%X=TP%X
 
 DO I=1,N
  DO J=1,SIZE(FM(I)%SF)
 
   KH%X=KH%NODATA; KV%X=KV%NODATA
   DO K=1,SIZE(FM(I)%SF)

    M=SIZE(FM(I)%SF(K)%AT)
    
    IF(K.GE.2.AND.M.EQ.0)EXIT
    
    DO L=1,M
     IROW=INT(FM(I)%SF(K)%AT(L)%IROW,4)
     ICOL=INT(FM(I)%SF(K)%AT(L)%ICOL,4)
     TP%X(ICOL,IROW)=FM(I)%SF(K)%AT(L)%TP
     BT%X(ICOL,IROW)=FM(I)%SF(K)%AT(L)%BT  
     KH%X(ICOL,IROW)=FM(I)%SF(K)%AT(L)%KH
     KV%X(ICOL,IROW)=FM(I)%SF(K)%AT(L)%KV
    ENDDO
    
    FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(MODELDIR)//'\'//TRIM(FNAME)//'-T_L'//TRIM(ITOS(I))//'.IDF'
    IF(.NOT.IDFWRITE(TP,FNAME,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(FNAME); STOP; ENDIF 
    FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(MODELDIR)//'\'//TRIM(FNAME)//'-KH_L'//TRIM(ITOS(I))//'.IDF'
    IF(.NOT.IDFWRITE(KH,FNAME,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(FNAME); STOP; ENDIF  
    FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(MODELDIR)//'\'//TRIM(FNAME)//'-KV_L'//TRIM(ITOS(I))//'.IDF'
    IF(.NOT.IDFWRITE(KV,FNAME,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(FNAME); STOP; ENDIF
    IF(I.LT.N)THEN
     FNAME=FLIST(I+1)(INDEX(FLIST(I+1),'\',.TRUE.)+1:); FNAME=TRIM(MODELDIR)//'\'//TRIM(FNAME)//'-T_L'//TRIM(ITOS(I+1))//'.IDF'
    ELSE
     FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(MODELDIR)//'\'//TRIM(FNAME)//'-B_L'//TRIM(ITOS(I))//'.IDF'
    ENDIF
    IF(.NOT.IDFWRITE(BT,FNAME,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(FNAME); STOP; ENDIF
    
   ENDDO
  ENDDO

  F=100.0D0*DBLE(I)/DBLE(N)
  WRITE(*,'(1X,A)') 'WRITING '//TRIM(FLIST(I))//'('//TRIM(RTOS(F,'F',2))//'%)'
 
 ENDDO
 
 DO I=1,N
  DO J=1,SIZE(FM(I)%SF)
   IF(ASSOCIATED(FM(I)%SF(J)%AT))DEALLOCATE(FM(I)%SF(J)%AT)
  ENDDO
  DEALLOCATE(FM(I)%SF)
 ENDDO
 DEALLOCATE(FM)
 
 END SUBROUTINE LHM_ADDIWHB

 !###===========================
 LOGICAL FUNCTION LHM_ADDIWHB_READFLIST(IDF,TP,BT,KH,KV,I,N,LIST)  
 !###===========================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF,TP,BT,KH,KV
 INTEGER,INTENT(IN) :: I,N
 CHARACTER(LEN=256) :: FN
 CHARACTER(LEN=*),DIMENSION(N) :: LIST
 
 LHM_ADDIWHB_READFLIST=.FALSE.
 
 FN=TRIM(LIST(I))//'-T_L'//TRIM(ITOS(I))//'.IDF'
 IF(IWINDOW.EQ.1)THEN
  CALL IDFCOPY(IDF,TP); IF(.NOT.IDFREADSCALE(FN,TP,10,1,0.0D0,0))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
 ELSE
  IF(.NOT.IDFREAD(TP,FN,1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
 ENDIF
  
 FN=TRIM(LIST(I))//'-KH_L'//TRIM(ITOS(I))//'.IDF'
 IF(IWINDOW.EQ.1)THEN
  CALL IDFCOPY(IDF,KH); IF(.NOT.IDFREADSCALE(FN,KH,10,1,0.0D0,0))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
 ELSE
  IF(.NOT.IDFREAD(KH,FN,1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF  
 ENDIF
  
 FN=TRIM(LIST(I))//'-KV_L'//TRIM(ITOS(I))//'.IDF'
 IF(IWINDOW.EQ.1)THEN
  CALL IDFCOPY(IDF,KV); IF(.NOT.IDFREADSCALE(FN,KV,10,1,0.0D0,0))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
 ELSE
  IF(.NOT.IDFREAD(KV,FN,1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF
 ENDIF
 
 IF(I.LT.N)THEN
  FN=LIST(I+1); FN=TRIM(FN)//'-T_L'//TRIM(ITOS(I+1))//'.IDF'
 ELSE
  FN=LIST(I); FN=TRIM(FN)//'-B_L'//TRIM(ITOS(I))//'.IDF'
 ENDIF
 IF(IWINDOW.EQ.1)THEN
  CALL IDFCOPY(IDF,BT); IF(.NOT.IDFREADSCALE(FN,BT,10,1,0.0D0,0))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
 ELSE
  IF(.NOT.IDFREAD(BT,FN,1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF
 ENDIF
 
 LHM_ADDIWHB_READFLIST=.TRUE.
 
 END FUNCTION LHM_ADDIWHB_READFLIST 

 !###===========================
 SUBROUTINE LHM_CONVERTREGIS()
 !###===========================
 IMPLICIT NONE
 INTEGER :: I,J,N,IREC,ICOL,IROW
 REAL :: T,B,K1,K2
 TYPE(IDFOBJ),DIMENSION(:),ALLOCATABLE :: TP,BT,KH,KV
 TYPE(IDFOBJ) :: TPIDF,BTIDF,KHIDF,KVIDF,IBIDF
  
 CALL UTL_CREATEDIR(CONDIR)
 
 N=SIZE(FLIST); ALLOCATE(TP(N),BT(N),KH(N),KV(N))
 DO I=1,N; CALL IDFNULLIFY(TP(I)); CALL IDFNULLIFY(BT(I)); CALL IDFNULLIFY(KH(I)); CALL IDFNULLIFY(KV(I)); ENDDO
 CALL IDFNULLIFY(TPIDF); CALL IDFNULLIFY(BTIDF); CALL IDFNULLIFY(KHIDF); CALL IDFNULLIFY(KVIDF); CALL IDFNULLIFY(IBIDF)
 
 DO I=1,N
  WRITE(6,*) 'Reading '//TRIM(FLIST(I))//' ...'
  FNAME=TRIM(FLIST(I))//'-T-C.IDF'; IF(.NOT.IDFREAD(TP(I),FNAME,0))THEN; WRITE(6,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF
  FNAME=TRIM(FLIST(I))//'-B-C.IDF'; IF(.NOT.IDFREAD(BT(I),FNAME,0))THEN; WRITE(6,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF
  FNAME=TRIM(FLIST(I))//'-KH-S.IDF'; IF(.NOT.IDFREAD(KH(I),FNAME,0,IQ=1))THEN; ENDIF
  FNAME=TRIM(FLIST(I))//'-KV-S.IDF'; IF(.NOT.IDFREAD(KV(I),FNAME,0,IQ=1))THEN; ENDIF
 ENDDO

 DO I=2,N
  IF(.NOT.IDFEQUAL(TP(I),TP(1),0))THEN; WRITE(*,'(/A/)') 'IDF '//TRIM(TP(I)%FNAME)//' NE TO '//TRIM(TP(1)%FNAME); STOP; ENDIF
  IF(.NOT.IDFEQUAL(BT(I),TP(1),0))THEN; WRITE(*,'(/A/)') 'IDF '//TRIM(BT(I)%FNAME)//' NE TO '//TRIM(BT(1)%FNAME); STOP; ENDIF
  IF(KH(I)%NCOL.GT.0)THEN
   IF(.NOT.IDFEQUAL(KH(I),TP(1),0))THEN; WRITE(*,'(/A/)') 'IDF '//TRIM(KH(I)%FNAME)//' NE TO '//TRIM(KH(1)%FNAME); STOP; ENDIF
  ENDIF
  IF(KV(I)%NCOL.GT.0)THEN
   IF(.NOT.IDFEQUAL(KV(I),TP(1),0))THEN; WRITE(*,'(/A/)') 'IDF '//TRIM(KV(I)%FNAME)//' NE TO '//TRIM(KV(1)%FNAME); STOP; ENDIF
  ENDIF
 ENDDO

 CALL IDFCOPY(TP(1),TPIDF); CALL IDFCOPY(TP(1),BTIDF); CALL IDFCOPY(TP(1),KHIDF); CALL IDFCOPY(TP(1),KVIDF); CALL IDFCOPY(TP(1),IBIDF)
 IF(.NOT.IDFALLOCATEX(TPIDF))STOP; IF(.NOT.IDFALLOCATEX(BTIDF))STOP; IF(.NOT.IDFALLOCATEX(KHIDF))STOP; IF(.NOT.IDFALLOCATEX(KVIDF))STOP
 IF(.NOT.IDFALLOCATEX(IBIDF))STOP

 !## fill in values
 TPIDF%X=TP(1)%NODATA; BTIDF%X=BT(1)%NODATA; KHIDF%X=TP(1)%NODATA; KVIDF%X=TP(1)%NODATA; IBIDF%X=TP(1)%NODATA
 DO I=1,N
  WRITE(*,'(/A)') 'Processing '//TRIM(FLIST(I))
  IREC=11  +ABS(TPIDF%IEQ-1) *2    +TPIDF%IEQ*(TPIDF%NROW+TPIDF%NCOL) +TPIDF%ITB*2
  DO IROW=1,TP(1)%NROW; DO ICOL=1,TP(1)%NCOL
   IREC=IREC+1; IBIDF%X(ICOL,IROW)=0
   !## nodata remains nodata 
   IF(I.GT.1.AND.TPIDF%X(ICOL,IROW).EQ.TPIDF%NODATA)CYCLE
   READ(TP(I)%IU,REC=IREC) T; READ(BT(I)%IU,REC=IREC) B
   !## look for first no data
   IF(T.EQ.TP(I)%NODATA)THEN
    IF(I.EQ.1)THEN
     !## check below
     DO J=I+1,N
      READ(TP(J)%IU,REC=IREC) T
      IF(T.NE.TP(J)%NODATA)THEN
       B=T; EXIT
      ENDIF
     ENDDO
    ELSE
     T=BTIDF%X(ICOL,IROW); B=BTIDF%X(ICOL,IROW)
    ENDIF
    K1=TP(1)%NODATA; K2=TP(1)%NODATA
   ELSE
    K1=EPS; IF(KH(I)%IU.GT.0)READ(KH(I)%IU,REC=IREC) K1; IF(K1.EQ.KH(I)%NODATA)K1=EPS
    K2=EPS; IF(KV(I)%IU.GT.0)READ(KV(I)%IU,REC=IREC) K2; IF(K2.EQ.KV(I)%NODATA)K2=EPS
    IF(K1.EQ.0.0)K1=K2; IF(K2.EQ.0.0)K2=K1
    IF(K1.EQ.0.0.AND.K2.EQ.0.0)THEN
     K1=TP(1)%NODATA; K2=K1; T=BTIDF%X(ICOL,IROW); B=BTIDF%X(ICOL,IROW)
    ELSE
     !## make sure it connects
     IF(I.GT.1)T=BTIDF%X(ICOL,IROW)
    ENDIF
   ENDIF
   TPIDF%X(ICOL,IROW)=T; BTIDF%X(ICOL,IROW)=B; IBIDF%X(ICOL,IROW)=1
   IF(K1.NE.TP(1)%NODATA.AND.K2.NE.TP(1)%NODATA)THEN
    KHIDF%X(ICOL,IROW)=K1; KVIDF%X(ICOL,IROW)=K2
   ELSE
    KHIDF%X(ICOL,IROW)=TP(1)%NODATA; KVIDF%X(ICOL,IROW)=1.0D0
   ENDIF
  ENDDO; WRITE(6,'(A,F10.3,A)') '+Progress ',REAL(100*IROW)/REAL(TP(1)%NROW),'%     '; ENDDO

  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(CONDIR)//'\'//TRIM(FNAME)//'-IB_L'//TRIM(ITOS(I))//'.IDF'; WRITE(*,*) TRIM(FNAME)
  IF(.NOT.IDFWRITE(IBIDF,FNAME,1,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(FNAME); STOP; ENDIF  
  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(CONDIR)//'\'//TRIM(FNAME)//'-T_L'//TRIM(ITOS(I))//'.IDF'; WRITE(*,*) TRIM(FNAME)
  IF(.NOT.IDFWRITE(TPIDF,FNAME,1,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(FNAME); STOP; ENDIF  
  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(CONDIR)//'\'//TRIM(FNAME)//'-KH_L'//TRIM(ITOS(I))//'.IDF'; WRITE(*,*) TRIM(FNAME)
  IF(.NOT.IDFWRITE(KHIDF,FNAME,1,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(FNAME); STOP; ENDIF  
  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(CONDIR)//'\'//TRIM(FNAME)//'-KV_L'//TRIM(ITOS(I))//'.IDF'; WRITE(*,*) TRIM(FNAME)
  IF(.NOT.IDFWRITE(KVIDF,FNAME,1,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(FNAME); STOP; ENDIF  
  IF(I.EQ.N)THEN
   FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(CONDIR)//'\'//TRIM(FNAME)//'-B_L'//TRIM(ITOS(I))//'.IDF'; WRITE(*,*) TRIM(FNAME)
   IF(.NOT.IDFWRITE(BTIDF,FNAME,1,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(FNAME); STOP; ENDIF  
  ENDIF

 ENDDO

 END SUBROUTINE LHM_CONVERTREGIS

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
  
 WRITE(OUTPUTDIR,'(A,I5.5)') TRIM(MODELDIR)//'\CMIN_',INT(CMIN)
 CALL UTL_CREATEDIR(OUTPUTDIR)
 
 N=SIZE(FLIST); ALLOCATE(TP(N+1,2),KH(N,2),KV(N,2))
 DO I=1,N+1; DO J=1,2; CALL IDFNULLIFY(TP(I,J)); ENDDO; ENDDO
 DO I=1,N;   DO J=1,2; CALL IDFNULLIFY(KH(I,J)); CALL IDFNULLIFY(KV(I,J)); ENDDO; ENDDO

 DO I=1,N
  
  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(CONDIR)//'\'//TRIM(FNAME)//'-T_L'//TRIM(ITOS(I))//'.IDF'
  IF(.NOT.IDFREAD(TP(I,1),FNAME,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF 
  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-T_L'//TRIM(ITOS(I))//'.IDF'
  CALL IDFCOPY(TP(I,1),TP(I,2))
  IF(.NOT.IDFWRITE(TP(I,2),FNAME,1,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF
  
  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(CONDIR)//'\'//TRIM(FNAME)//'-KH_L'//TRIM(ITOS(I))//'.IDF'
  IF(.NOT.IDFREAD(KH(I,1),FNAME,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF  
  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-KH_L'//TRIM(ITOS(I))//'.IDF'
  CALL IDFCOPY(KH(I,1),KH(I,2))
  IF(.NOT.IDFWRITE(KH(I,2),FNAME,1,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF

  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(CONDIR)//'\'//TRIM(FNAME)//'-KV_L'//TRIM(ITOS(I))//'.IDF'
  IF(.NOT.IDFREAD(KV(I,1),FNAME,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF
  FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-VA_L'//TRIM(ITOS(I))//'.IDF'
  CALL IDFCOPY(KV(I,1),KV(I,2))
  IF(.NOT.IDFWRITE(KV(I,2),FNAME,1,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF
 
  IF(I.EQ.N)THEN
   FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(CONDIR)//'\'//TRIM(FNAME)//'-B_L'//TRIM(ITOS(I))//'.IDF'
   IF(.NOT.IDFREAD(TP(I+1,1),FNAME,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF
   FNAME=FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:); FNAME=TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-B_L'//TRIM(ITOS(I))//'.IDF'
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
  FNAME = FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:)
  FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-IB_L'//TRIM(ITOS(I))//'.IDF'
  WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
 ENDDO
 
 WRITE(IU,'(/A)') '0001,(TOP),1, TOP ELEVATION,[TOP]'
 WRITE(IU,'(A,I10)') '001,',N
 DO I = 1,N
  FNAME = FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:)
  FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-T_L'//TRIM(ITOS(I))//'.IDF'
  WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
 ENDDO
 
 WRITE(IU,'(/A)') '0001,(BOT),1, BOTTOM ELEVATION,[BOT]'
 WRITE(IU,'(A,I10)') '001,',N
 DO I = 1,N
  IF(I.EQ.N)THEN
   FNAME = FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:)
   FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-B_L'//TRIM(ITOS(I))//'.IDF'
   WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
  ELSE
   FNAME = FLIST(I+1)(INDEX(FLIST(I+1),'\',.TRUE.)+1:)
   FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-T_L'//TRIM(ITOS(I+1))//'.IDF'
   WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
  ENDIF
 ENDDO

 WRITE(IU,'(/A)') '0001,(KHV),1, HORIZONTAL PERMEABILITY,[KHV]'
 WRITE(IU,'(A,I10)') '001,',N
 DO I = 1,N
  FNAME = FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:)
  FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-KH_L'//TRIM(ITOS(I))//'.IDF'
  WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
 ENDDO

 WRITE(IU,'(/A)') '0001,(KVA),1, VERTICAL ANISOTROPY,[KVA]'
 WRITE(IU,'(A,I10)') '001,',N
 
 DO I = 1,N
  FNAME = FLIST(I)(INDEX(FLIST(I),'\',.TRUE.)+1:)
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
  FNAME = FLIST(1)(INDEX(FLIST(1),'\',.TRUE.)+1:)
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