MODULE MOD_AGGREGATE
 
 USE MOD_UTL, ONLY : UTL_CREATEDIR,UTL_IDFSNAPTOGRID_LLC
 USE MOD_IDF
 USE MOD_AGGREGATE_PAR

 CONTAINS

 !###===========================
 SUBROUTINE LHM_CONVERTREGIS_MAIN(OBSICOL,OBSIROW)
 !###===========================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: OBSICOL,OBSIROW

 !## initialize
 CALL LHM_CONVERTREGIS_SORTFILES(FLIST,SORTFILE,INDIR,0)
 
 SELECT CASE (IMODE)
  !## make dataset consistent
  CASE (1)
   CALL LHM_CONVERTREGIS()
  !## include additional data
  CASE (2)
   CALL LHM_CONVERTREGIS_SORTFILES(ALIST,IWHBFILE,IWHBDIR,SIZE(FLIST))
   CALL LHM_ADDIWHB_RASTER(OBSICOL,OBSIROW) !CALL LHM_ADDIWHB()
  !## aggregate dataset
  CASE (3)
   CALL LHM_CONVERTREGIS_AGGREGATE()
   IF(IPRJ.EQ.1)CALL LHM_CONVERTREGIS_WRITEPRJ()
 END SELECT

 END SUBROUTINE LHM_CONVERTREGIS_MAIN

 !###===========================
 SUBROUTINE LHM_CONVERTREGIS_SORTFILES(SLIST,OFILE,DIR,IMETHOD)
 !###===========================
 IMPLICIT NONE
 TYPE(FOBJ),INTENT(INOUT),POINTER,DIMENSION(:) :: SLIST
 CHARACTER(LEN=*),INTENT(IN) :: OFILE,DIR
 INTEGER,INTENT(IN) :: IMETHOD
 INTEGER :: I,II,J,JJ,N,M,IU,IOS
 LOGICAL :: LEX
 CHARACTER(LEN=512) :: LINE
 
 !## open unit for same line printing of echo (is equal to screen or '*')
 OPEN(UNIT=6,CARRIAGECONTROL='FORTRAN') 

 IU=UTL_GETUNIT(); OPEN(IU,FILE=OFILE,STATUS='OLD',ACTION='READ'); ALLOCATE(SLIST(1)); READ(IU,*)
 DO I=1,2; M=1; N=1
  DO
   IF(I.EQ.2.AND.M.GT.SIZE(SLIST))EXIT
   SLIST(M)%FILE=''; READ(IU,'(A512)',IOSTAT=IOS) LINE
   IF(IOS.NE.0)EXIT
   IF(IMETHOD.EQ.0)THEN
    READ(LINE,*,IOSTAT=IOS) (SLIST(M)%FILE(J),J=1,SIZE(SLIST(M)%FILE)); IF(IOS.NE.0)EXIT
   ELSE
    READ(LINE,*,IOSTAT=IOS) (SLIST(M)%FILE(J),J=1,SIZE(SLIST(M)%FILE)),SLIST(M)%ILAYER,SLIST(M)%METH; IF(IOS.NE.0)EXIT
   ENDIF
   N=N+1; IF(I.EQ.2)M=N
  ENDDO
  IF(I.EQ.1)THEN
   DEALLOCATE(SLIST)
   IF(N-1.LE.0)THEN; WRITE(*,'(/A/)') 'CANNOT FIND APPROPRIATE FILES FROM '//TRIM(OFILE); EXIT; ENDIF
   IF(N-1.GT.0)ALLOCATE(SLIST(N-1))
  ENDIF
  REWIND(IU); READ(IU,*)
 ENDDO; CLOSE(IU)

 !## check files
 IF(ASSOCIATED(SLIST))THEN
  DO I=1,SIZE(SLIST)
   DO J=1,SIZE(SLIST(I)%FILE)
    IF(TRIM(SLIST(I)%FILE(J)).NE.'')THEN
     READ(SLIST(I)%FILE(J),*,IOSTAT=IOS) SLIST(I)%XVAL(J)
     IF(IOS.NE.0)THEN
      SLIST(I)%ITYPE(J)=0; SLIST(I)%XVAL(J)=HUGE(1.0D0)
      INQUIRE(FILE=TRIM(DIR)//'\'//TRIM(SLIST(I)%FILE(J)),EXIST=LEX)
      IF(.NOT.LEX)THEN
       WRITE(*,'(/A/)') 'CANNOT FIND FILE '//TRIM(DIR)//'\'//TRIM(SLIST(I)%FILE(J))
       STOP
      ENDIF
      SLIST(I)%FILE(J)=UTL_CAP(SLIST(I)%FILE(J),'U')
     ELSE
      SLIST(I)%ITYPE(J)=1
     ENDIF
    ELSE
     !## missing value
     SLIST(I)%ITYPE(J)=-1; SLIST(I)%XVAL(J)=-999.0D0
    ENDIF
   ENDDO
  ENDDO
 
  !## mark duplicate files
  DO I=1,SIZE(SLIST); DO J=1,SIZE(SLIST(I)%FILE)
   SLIST(I)%DUPL_F(J)=0; SLIST(I)%DUPL_T(J)=0
   !## skip constant values
   IF(SLIST(I)%ITYPE(J).NE.0)CYCLE
   DO II=SIZE(SLIST),1,-1; DO JJ=SIZE(SLIST(II)%FILE),1,-1
    !## skip constant values
    IF(SLIST(II)%ITYPE(JJ).NE.0)CYCLE
    IF(TRIM(SLIST(I)%FILE(J)).EQ.TRIM(SLIST(II)%FILE(JJ)))THEN
     IF(II.LT.I)THEN
      SLIST(I)%DUPL_F(J)=II+IMETHOD; SLIST(I)%DUPL_T(J)=JJ
     ELSEIF(II.EQ.I.AND.JJ.LT.J)THEN
      SLIST(I)%DUPL_F(J)=II+IMETHOD; SLIST(I)%DUPL_T(J)=JJ
     ENDIF
    ENDIF
   ENDDO; IF(SLIST(I)%DUPL_F(J).NE.0)EXIT; ENDDO
  ENDDO; ENDDO

  WRITE(*,'(/A/)') 'NUMBER OF DUPLICATE FILES FOUND'
  DO I=1,SIZE(SLIST)
   IF(SUM(SLIST(I)%DUPL_F+SLIST(I)%DUPL_T).GT.0)THEN
    WRITE(*,'(99I4)') I+IMETHOD,(SLIST(I)%DUPL_F(J),SLIST(I)%DUPL_T(J),J=1,SIZE(SLIST(I)%FILE))
   ENDIF

   !## BE WARE - THIS IS DIRTY
   IF(IMETHOD.GT.0)THEN; IF(SLIST(I)%ILAYER.GT.12)SLIST(I)%ILAYER=SLIST(I)%ILAYER+4; ENDIF
   !## BE WARE - THIS IS DIRTY
   
  ENDDO
 
 ENDIF
 CLOSE(IU)
 
 IF(IMETHOD.GT.0)THEN
  PAUSE 'TRICK WITH LAYER NUMBER IS PERFORMED'
 ENDIF
 
 END SUBROUTINE LHM_CONVERTREGIS_SORTFILES

 !###=====================================================
 SUBROUTINE LHM_ADDIWHB_RASTER(OBSICOL,OBSIROW)
 !###=====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: OBSICOL,OBSIROW
 INTEGER :: I,J,K,N,BSIZE,NODES,INODE,ICOL,IROW,NF,NA
 REAL(KIND=SP_KIND),DIMENSION(:),ALLOCATABLE :: TMP
 TYPE(IDFOBJ),DIMENSION(:,:),ALLOCATABLE :: IDF,ODF
 TYPE(IDFOBJ) :: MDL,TRV,VRV
 INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:) :: IACTIVE
  
 !## specify window
 IF(IWINDOW.EQ.1)THEN
  MDL%XMIN=XMIN; MDL%XMAX=XMAX; MDL%YMIN=YMIN; MDL%YMAX=YMAX; MDL%DX=CELLSIZE; MDL%DY=MDL%DX; MDL%ITYPE=4
  MDL%NODATA=HUGE(1.0)
  CALL UTL_IDFSNAPTOGRID_LLC(MDL%XMIN,MDL%XMAX,MDL%YMIN,MDL%YMAX,MDL%DX,MDL%DY,MDL%NCOL,MDL%NROW,.TRUE.)
 !## read entire model
 ELSE
  IF(.NOT.IDFREAD(MDL,TRIM(INDIR)//'\'//FLIST(1)%FILE(1),0))STOP; CLOSE(MDL%IU)
 ENDIF
 
 !## allocate inout/output
 NF=SIZE(FLIST); NA=0; IF(ASSOCIATED(ALIST))NA=SIZE(ALIST); N=NF+NA; ALLOCATE(IDF(5,N),ODF(5,N))
 DO I=1,N; DO J=1,5; CALL IDFNULLIFY(IDF(J,I)); ENDDO; ENDDO
 DO I=1,N; DO J=1,5; CALL IDFNULLIFY(ODF(J,I)); ENDDO; ENDDO

 !## open all base files for reading
 DO I=1,SIZE(FLIST)
  CALL LHM_ADDIWHB_RASTER_OPENFILES_READ(FLIST(I)%FILE,FLIST(I)%DUPL_F,FLIST(I)%DUPL_T,INDIR,IDF(:,I))
 ENDDO; I=I-1
 !## open all iwhb files for reading
 DO J=1,NA; I=I+1
  CALL LHM_ADDIWHB_RASTER_OPENFILES_READ(ALIST(J)%FILE,ALIST(J)%DUPL_F,ALIST(J)%DUPL_T,IWHBDIR,IDF(:,I))
 ENDDO
 !## copy dimensions from input idf to output idf
 DO I=1,SIZE(IDF,1); DO J=1,SIZE(IDF,2); CALL IDFCOPY(MDL,ODF(I,J)); ODF(I,J)%FNAME=IDF(I,J)%FNAME; ENDDO; ENDDO
 !## store total values transmissivities/vertical resistances
 CALL IDFCOPY(MDL,TRV); CALL IDFCOPY(MDL,VRV)
 IF(.NOT.IDFALLOCATEX(TRV))WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR TRV - TOTAL TRANSMISSIVITY VALUES'
 IF(.NOT.IDFALLOCATEX(VRV))WRITE(*,'(/A/)') 'CANNOT ALLOCATE MEMORY FOR VRV - TOTAL VERTICAL RESISTANCE VALUES'
 IF(ASSOCIATED(TRV%X))THEN; TRV%X=0.0D0; TRV%NODATA=0.0D0; ENDIF
 IF(ASSOCIATED(VRV%X))THEN; VRV%X=0.0D0; VRV%NODATA=0.0D0; ENDIF
 
 !## open all base files for writing
 DO I=1,NF
  CALL LHM_ADDIWHB_RASTER_OPENFILES_WRITE(FLIST(I)%FILE,FLIST(I)%DUPL_F,FLIST(I)%DUPL_T,I,OUTDIR,ODF(:,I))
 ENDDO
 
 NODES=MDL%NROW*MDL%NCOL; BLOCKSIZE=1000; BLOCKSIZE=MIN(NODES,BLOCKSIZE)
 ALLOCATE(TMP(BLOCKSIZE))
 
 !## allocate memory to store in blocksize
 DO I=1,SIZE(IDF,1); DO J=1,SIZE(IDF,2); IF(.NOT.ASSOCIATED(IDF(I,J)%V))ALLOCATE(IDF(I,J)%V(BLOCKSIZE)); ENDDO; ENDDO
 DO I=1,SIZE(ODF,1); DO J=1,SIZE(ODF,2); IF(.NOT.ASSOCIATED(ODF(I,J)%V))ALLOCATE(ODF(I,J)%V(BLOCKSIZE)); ENDDO; ENDDO
 
 !## store locations to be processed/saved eventually
 ALLOCATE(IACTIVE(NODES)); IACTIVE=0
 
 INODE=0; ICOL=0; IROW=1
 DO 
  
  BSIZE=MIN(BLOCKSIZE,NODES-INODE)
 
  !## finished
  IF(BSIZE.LE.0)EXIT

  !## read all data for current block
  DO I=1,NF
   CALL LHM_ADDIWHB_RASTER_READ_DATA(I,IDF,FLIST(I)%DUPL_F,FLIST(I)%DUPL_T,BSIZE,FLIST(I)%FILE,TMP)
  ENDDO; I=I-1
  DO J=1,NA; I=I+1
   CALL LHM_ADDIWHB_RASTER_READ_DATA(I,IDF,ALIST(J)%DUPL_F,ALIST(J)%DUPL_T,BSIZE,ALIST(J)%FILE,TMP)
  ENDDO
  
  !## copy original data from input idf objects (idf) to output idf objects (odf)
  DO I=1,SIZE(IDF,1); DO J=1,SIZE(IDF,2); ODF(I,J)%V=IDF(I,J)%V; ODF(I,J)%NODATA=IDF(I,J)%NODATA; ENDDO; ENDDO

  DO K=1,BSIZE
   
   ICOL=ICOL+1; IF(ICOL.GT.MDL%NCOL)THEN; ICOL=1; IROW=IROW+1; ENDIF
   
   IF(OBSICOL.NE.0.AND.OBSIROW.NE.0)THEN
    IF(OBSICOL.NE.ICOL.OR.IROW.NE.OBSIROW)CYCLE
    !## check whether base data is nodata and make it sequentially
    CALL LHM_ADDIWHB_RASTER_CHECK_CONSISTENCY(ODF,K,IROW,ICOL,OBSICOL+OBSIROW)
   ENDIF

   !## check base data
   N=0; DO I=1,NF; CALL LHM_ADDIWHB_RASTER_CHECK(ODF,I,K,ICOL,IROW,N); ENDDO
   
   !## check insert data
   I=NF; DO J=1,NA; I=I+1; CALL LHM_ADDIWHB_RASTER_CHECK(ODF,I,K,ICOL,IROW,N); ENDDO
   
   !## all nodata - nothing to do
   IF(N.EQ.NF+NA)CYCLE
   
   !## check whether base data is nodata and make it sequentially
   CALL LHM_ADDIWHB_RASTER_CHECK_CONSISTENCY(ODF,K,IROW,ICOL,OBSICOL+OBSIROW)

   I=NF; DO J=1,NA
    I=I+1
    SELECT CASE (ALIST(J)%METH)
     !## determining layer automatically
     CASE (1)
!      ALIST(J)%ILAYER=
     CASE (2:4)
      !## replace data from the ith iwhb layer
      CALL  LHM_ADDIWHB_RASTER_REPLACE(ODF,I,J,K)
    END SELECT
    !## correct data
    CALL LHM_ADDIWHB_RASTER_CORRECT(ODF,J,K)
   ENDDO
  
   !## check whether base data is nodata and make it sequentially
   CALL LHM_ADDIWHB_RASTER_CHECK_CONSISTENCY(ODF,K,IROW,ICOL,OBSICOL+OBSIROW)
   
   !## compute total transmissivities/total vertical resistances
   IF(ASSOCIATED(TRV%X).AND.ASSOCIATED(VRV%X))CALL LHM_ADDIWHB_RASTER_COMPUTE_TOTALS(ODF,K,IROW,ICOL,TRV,VRV)

  ENDDO
  
  !## write all data for current block
  DO I=1,NF; CALL LHM_ADDIWHB_RASTER_WRITE_DATA(ODF(:,I),BSIZE); ENDDO

  INODE=INODE+BSIZE
 
  WRITE(6,'(A,F10.3,A)') '+Progress Aggregation ',REAL(100*INODE)/REAL(NODES),'%                 '
 
 ENDDO
 
 TRV%FNAME=TRIM(OUTDIR)//'\TRV.IDF'; IF(.NOT.IDFWRITE(TRV,TRV%FNAME,1))THEN; ENDIF
 VRV%FNAME=TRIM(OUTDIR)//'\VRV.IDF'; IF(.NOT.IDFWRITE(VRV,VRV%FNAME,1))THEN; ENDIF
 
 DEALLOCATE(TMP,IACTIVE)
 
 END SUBROUTINE LHM_ADDIWHB_RASTER

 !###===========================
 SUBROUTINE LHM_ADDIWHB_RASTER_CORRECT(ODF,IWHB,K)
 !###===========================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IWHB,K
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:,:) :: ODF 
 INTEGER :: I,ILAY
 REAL(KIND=DP_KIND) :: T,B,TP,BT
 
 !## layer of insertion
 ILAY =ALIST(IWHB)%ILAYER
 
 T=ODF(1,ILAY)%V(K)
 B=ODF(2,ILAY)%V(K)
 
 !## correct above inserted layer
 DO I=ILAY-1,1,-1
  IF(ODF(2,I)%V(K).GT.T)EXIT
  ODF(1,I)%V(K)=MAX(ODF(1,I)%V(K),T)
  ODF(2,I)%V(K)=MAX(ODF(2,I)%V(K),T)
 ENDDO
 !## correct below inserted layer
 DO I=ILAY+1,SIZE(ODF,2)
  IF(ODF(1,I)%V(K).LT.B)EXIT
  ODF(1,I)%V(K)=MIN(ODF(1,I)%V(K),B)
  ODF(2,I)%V(K)=MIN(ODF(2,I)%V(K),B)
 ENDDO
 
 !## downlift bottom of first layer
 DO I=ILAY-1,1,-1
  TP=ODF(1,I)%V(K)
  BT=ODF(2,I)%V(K)
  IF(TP-BT.LE.0.0D0)THEN
   ODF(1,I)%V(K)=T
   ODF(2,I)%V(K)=T
  ELSE
   ODF(2,I)%V(K)=T; EXIT
  ENDIF
 ENDDO
 !## uplift top of first layer
 DO I=ILAY+1,SIZE(ODF,2)
  TP=ODF(1,I)%V(K)
  BT=ODF(2,I)%V(K)
  IF(TP-BT.LE.0.0D0)THEN
   ODF(1,I)%V(K)=B
   ODF(2,I)%V(K)=B
  ELSE
   ODF(1,I)%V(K)=B; EXIT
  ENDIF
 ENDDO
 
 END SUBROUTINE LHM_ADDIWHB_RASTER_CORRECT
 
 !###===========================
 SUBROUTINE LHM_ADDIWHB_RASTER_REPLACE(ODF,I,IWHB,K)
 !###===========================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IWHB,I,K
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:,:) :: ODF 
 INTEGER :: IMETH,ILAY
 REAL(KIND=DP_KIND) :: T,B,TP,BT,D,TR,VC,KH,KV
 
 !## layer of insertion
 ILAY =ALIST(IWHB)%ILAYER
 IMETH=ALIST(IWHB)%METH

 !## new dimensions
 TP=ODF(1,I)%V(K)
 BT=ODF(2,I)%V(K)

 !## dimensions of current layer
 T=ODF(1,ILAY)%V(K)
 B=ODF(2,ILAY)%V(K)

 !## new data not available here
 IF(TP.EQ.ODF(1,I)%NODATA.OR.BT.EQ.ODF(2,I)%NODATA)RETURN
 
 IF(TP-BT.LE.0.0D0)THEN
  !## remove if imethod=4
  IF(IMETH.EQ.4)THEN
   T=(T+B)/2.0D0
   ODF(1,ILAY)%V(K)=T
   ODF(2,ILAY)%V(K)=T
   RETURN
  ENDIF
 ENDIF

 TR=0.0D0; VC=0.0D0; D=0.0
    
 !## add to an existing layer
 IF(IMETH.EQ.3)THEN
  KH=ODF(3,ILAY)%V(K)
  KV=ODF(4,ILAY)%V(K)
  !## remaining original thickness
  D=T-B
  !## remaining transmissivity
  TR=D*KH
  !## remaining vertical resistance
  VC=D/KV
  !## update top- and bottom values
  T=MAX(TP,T)
  B=MIN(BT,B)
 ELSE
  !## added thickness
  T=TP
  B=BT
  !## new parameters
  KH=ODF(3,I)%V(K)
  KV=ODF(4,I)%V(K)
 ENDIF

 !## added nett thickness
 D=(T-B)-D
 IF(D.GT.0.0D0)THEN

  !## new parameters
  KH=ODF(3,I)%V(K)
  KV=ODF(4,I)%V(K)

  !## compute total transmissivity
  TR=TR+D*KH
  !## compute total vertical resistance
  VC=VC+D/KV

  !## recompute representative k-values
  D =T-B
  KH=TR/D
  KV=D/VC
 
 ENDIF
 
 !## save updated values
 ODF(1,ILAY)%V(K)=T
 ODF(2,ILAY)%V(K)=B
 ODF(3,ILAY)%V(K)=KH
 ODF(4,ILAY)%V(K)=KV
 ODF(5,ILAY)%V(K)=KV/KH
 
 END SUBROUTINE LHM_ADDIWHB_RASTER_REPLACE

 !###=====================================================
 SUBROUTINE LHM_ADDIWHB_RASTER_COMPUTE_TOTALS(IDF,K,IROW,ICOL,TRV,VRV)
 !###=====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICOL,IROW,K
 TYPE(IDFOBJ),INTENT(INOUT) :: TRV,VRV
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:,:) :: IDF
 INTEGER :: I
 REAL(KIND=DP_KIND) :: TP,BT,KH,KV
 
 DO I=1,SIZE(FLIST)
  TP=IDF(1,I)%V(K); BT=IDF(2,I)%V(K)
  IF(TP.GT.BT)THEN
   KH=IDF(3,I)%V(K); KV=IDF(4,I)%V(K)
   TRV%X(ICOL,IROW)=TRV%X(ICOL,IROW)+(TP-BT)*KH
   VRV%X(ICOL,IROW)=VRV%X(ICOL,IROW)+(TP-BT)/KV
  ENDIF
 ENDDO

 END SUBROUTINE LHM_ADDIWHB_RASTER_COMPUTE_TOTALS
 
 !###=====================================================
 SUBROUTINE LHM_ADDIWHB_RASTER_CHECK_CONSISTENCY(IDF,K,IROW,ICOL,ICHECK)
 !###=====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICOL,IROW,K,ICHECK
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:,:) :: IDF
 INTEGER :: I,J
 REAL(KIND=DP_KIND) :: TP,BT
 
 IF(ICHECK.GT.0)THEN
  DO J=1,SIZE(FLIST)
   WRITE(*,'(2I5,5F15.7)') K,J,IDF(1,J)%V(K),IDF(2,J)%V(K),IDF(3,J)%V(K),IDF(4,J)%V(K),IDF(5,J)%V(K)
  ENDDO
 ENDIF

 !## top to bottom
 DO I=1,SIZE(FLIST)
  IF(I.GT.1)THEN
   !## top equal to bottom previous layer
   IF(IDF(1,I)%V(K).EQ.IDF(1,I)%NODATA)IDF(1,I)%V(K)=IDF(2,I-1)%V(K)
  ENDIF
  !## bottom equal to top of current layer
  IF(IDF(2,I)%V(K).EQ.IDF(2,I)%NODATA)IDF(2,I)%V(K)=IDF(1,I)%V(K)
 ENDDO
 !## bottom to top
 DO I=SIZE(FLIST),1,-1
  IF(I.LT.SIZE(FLIST))THEN
   !## bottom equal to top previous layer
   IF(IDF(2,I)%V(K).EQ.IDF(2,I)%NODATA)IDF(2,I)%V(K)=IDF(1,I+1)%V(K)
  ENDIF
  !## top equal to bottom of current layer
  IF(IDF(1,I)%V(K).EQ.IDF(1,I)%NODATA)IDF(1,I)%V(K)=IDF(2,I)%V(K)
 ENDDO
  
 DO I=1,SIZE(FLIST)-1
  BT=IDF(2,I)%V(K); TP=IDF(1,I+1)%V(K)
  IF(BT.NE.TP)THEN
   WRITE(*,'(/A,2I5)') 'ERROR ON IROW,ICOL: ',IROW,ICOL
   DO J=1,SIZE(FLIST)
    WRITE(*,'(2I5,5F15.7)') K,J,IDF(1,J)%V(K),IDF(2,J)%V(K),IDF(3,J)%V(K),IDF(4,J)%V(K),IDF(5,J)%V(K)
   ENDDO
   PAUSE; STOP
  ENDIF
 ENDDO
 
 IF(ICHECK.GT.0)THEN
  DO J=1,SIZE(FLIST)
   WRITE(*,'(2I5,5F15.7)') K,J,IDF(1,J)%V(K),IDF(2,J)%V(K),IDF(3,J)%V(K),IDF(4,J)%V(K),IDF(5,J)%V(K)
  ENDDO
 ENDIF
 
 END SUBROUTINE LHM_ADDIWHB_RASTER_CHECK_CONSISTENCY
 
 !###=====================================================
 SUBROUTINE LHM_ADDIWHB_RASTER_CHECK(IDF,IM,K,ICOL,IROW,N)
 !###=====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICOL,IROW,K,IM
 INTEGER,INTENT(INOUT) :: N
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:,:) :: IDF
 INTEGER :: I,J
 LOGICAL :: LEX
 
 LEX=IDF(1,IM)%V(K).EQ.IDF(1,IM)%NODATA.OR.IDF(2,IM)%V(K).EQ.IDF(2,IM)%NODATA
 
 !## thickness available - need k-values
 IF(.NOT.LEX.AND.IDF(1,IM)%V(K)-IDF(2,IM)%V(K).GT.0.0D0)THEN
  IF(IDF(3,IM)%V(K).LE.0.0D0.OR. &
     IDF(4,IM)%V(K).LE.0.0D0.OR. &
     IDF(5,IM)%V(K).LE.0.0D0)THEN
   !## try to fix it
   IF(IDF(3,IM)%V(K).LE.0.0D0.AND.(IDF(4,IM)%V(K).GT.0.0D0.AND.IDF(5,IM)%V(K).GT.0.0D0))THEN
    IDF(3,IM)%V(K)=IDF(4,IM)%V(K)/IDF(5,IM)%V(K)
   ELSEIF(IDF(4,IM)%V(K).LE.0.0D0.AND.(IDF(3,IM)%V(K).GT.0.0D0.AND.IDF(5,IM)%V(K).GT.0.0D0))THEN
    IDF(4,IM)%V(K)=IDF(3,IM)%V(K)*IDF(5,IM)%V(K)
   ELSE
    WRITE(*,'(/A,3I5)') 'ERROR ICOL,IROW,IM:',ICOL,IROW,IM
    WRITE(*,'(/A,2I5,5F15.7/)') 'TP,BT,(KH,KV,VA LE 0.0):',IDF(1,IM)%V(K),IDF(2,IM)%V(K),IDF(3,IM)%V(K),IDF(4,IM)%V(K),IDF(5,IM)%V(K)
    DO J=1,SIZE(FLIST)
     WRITE(*,'(3I5,5F15.7)') K,J,J,IDF(1,J)%V(K),IDF(2,J)%V(K),IDF(3,J)%V(K),IDF(4,J)%V(K),IDF(5,J)%V(K)
    ENDDO; J=J-1
    WRITE(*,'(/A/)') 'IWHB LAYERS'
    IF(ASSOCIATED(ALIST))THEN
     DO I=1,SIZE(ALIST)
      J=J+1; 
      IF(J.EQ.IM)WRITE(*,'(/A/)') 'ISSUE HERE:'
      WRITE(*,'(3I5,5F15.7)') K,I,J,IDF(1,J)%V(K),IDF(2,J)%V(K),IDF(3,J)%V(K),IDF(4,J)%V(K),IDF(5,J)%V(K)
      IF(J.EQ.IM)WRITE(*,'(A/)')
     ENDDO
    ENDIF
    STOP
   ENDIF
  ENDIF
 ELSE
  N=N+1
!  !## set all to nodata
!  DO I=1,5; IDF(I,IM)%V(K)=IDF(I,IM)%NODATA; ENDDO
 ENDIF
 
 END SUBROUTINE LHM_ADDIWHB_RASTER_CHECK
 
 !###=====================================================
 SUBROUTINE LHM_ADDIWHB_RASTER_OPENFILES_READ(LIST,DUPL_F,DUPL_T,DIR,IDF)
 !###=====================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: LIST
 INTEGER,INTENT(IN),DIMENSION(:) :: DUPL_F,DUPL_T
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: IDF
 INTEGER :: I,IOS
 REAL(KIND=DP_KIND) :: X
 
 DO I=1,SIZE(IDF)
  READ(LIST(I),*,IOSTAT=IOS) X
  IF(IOS.NE.0)THEN
   IF(TRIM(LIST(I)).NE.'')THEN
    IDF(I)%FNAME=TRIM(DIR)//'\'//TRIM(LIST(I))
    !## open, otherwise already opened
    IF(DUPL_F(I).EQ.0.AND.DUPL_T(I).EQ.0)THEN
     IF(.NOT.IDFREAD(IDF(I),TRIM(IDF(I)%FNAME),-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(IDF(I)%FNAME); STOP; ENDIF 
    ENDIF
   ENDIF
  ELSE
   ALLOCATE(IDF(I)%X(1,1)); IDF(I)%X(1,1)=X
  ENDIF
 ENDDO
 
 !## if idf(i)%iu.gt.0) --- read from idf
 !## if allocated(idf(i)%x) --- read constant value from idf(i)%x(1,1)
 !## else nothing read --- apply conversion from other
 
 END SUBROUTINE LHM_ADDIWHB_RASTER_OPENFILES_READ

 !###=====================================================
 SUBROUTINE LHM_ADDIWHB_RASTER_OPENFILES_WRITE(LIST,DUPL_F,DUPL_T,IFL,DIR,IDF)
 !###=====================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: LIST
 INTEGER,INTENT(IN),DIMENSION(:) :: DUPL_F,DUPL_T
 INTEGER,INTENT(IN) :: IFL
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 CHARACTER(LEN=52) :: FTXT
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: IDF
 INTEGER :: I,II,J,JJ
 CHARACTER(LEN=5),DIMENSION(5) :: TXT=['-T-C','-B-C','-KH-S','-KV-S','-VA-S'] 
 
 DO I=1,SIZE(IDF)
  IF(INDEX(LIST(I),'.IDF').LE.0)THEN
   !## use name from top, bot, kh,kv,va if needed
   DO II=1,SIZE(IDF)
    IF(INDEX(LIST(II),'.IDF').GT.0)THEN
     J= INDEX(LIST(II),'_',.TRUE.)
     JJ=INDEX(LIST(II)(J+1:),'-'); JJ=JJ+J
     FTXT=LIST(II)(:JJ-1); IDF(I)%FNAME=TRIM(DIR)//'\'//TRIM(FTXT)//TRIM(TXT(I))//'.IDF' 
     EXIT
    ENDIF
   ENDDO
   IF(II.GT.SIZE(IDF))IDF(I)%FNAME=TRIM(DIR)//'\'//TRIM(ITOS(IFL))//'_FORMATION'//TRIM(TXT(I))//'.IDF' 
  ELSE
   IDF(I)%FNAME=TRIM(DIR)//'\'//TRIM(LIST(I))
  ENDIF
  IF(DUPL_F(I).EQ.0.AND.DUPL_T(I).EQ.0)THEN
   IF(.NOT.IDFWRITE(IDF(I),TRIM(IDF(I)%FNAME),1,-1))THEN
    WRITE(*,'(/A/)') 'ERROR READING '//TRIM(IDF(I)%FNAME); STOP
   ENDIF
  ENDIF
 ENDDO
 
 END SUBROUTINE LHM_ADDIWHB_RASTER_OPENFILES_WRITE

 !###=====================================================
 SUBROUTINE LHM_ADDIWHB_RASTER_READ_DATA(IM,IDF,DUPL_F,DUPL_T,BSIZE,LIST,TMP)
 !###=====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IM
 TYPE(IDFOBJ),DIMENSION(:,:),INTENT(INOUT) :: IDF
 INTEGER,INTENT(IN),DIMENSION(:) :: DUPL_F,DUPL_T
 REAL(KIND=SP_KIND),DIMENSION(:) :: TMP
 CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: LIST
 INTEGER,INTENT(IN) :: BSIZE
 INTEGER :: I,J
 
 DO I=1,SIZE(IDF,1)
  IF(DUPL_F(I).EQ.0.AND.DUPL_T(I).EQ.0)THEN
   !## read block
   IF(IDF(I,IM)%IU.GT.0)THEN
    IF(IDF(I,IM)%ITYPE.EQ.4)THEN
     READ(IDF(I,IM)%IU) (TMP(J),J=1,BSIZE)
     DO J=1,BSIZE; IDF(I,IM)%V(J)=TMP(J); ENDDO
    ELSE
     READ(IDF(I,IM)%IU) (IDF(I,IM)%V(J),J=1,BSIZE)
    ENDIF
   ELSE
    IF(ASSOCIATED(IDF(I,IM)%X))IDF(I,IM)%V=IDF(I,IM)%X(1,1); IDF(I,IM)%NODATA=HUGE(1.0)
   ENDIF
  ELSE
   !## copy data and nodata value
   IDF(I,IM)%V=IDF(DUPL_T(I),DUPL_F(I))%V; IDF(I,IM)%NODATA=IDF(DUPL_T(I),DUPL_F(I))%NODATA
  ENDIF
 ENDDO
 
 !## fill in missing data for kh,kv and/or va
 IF(TRIM(LIST(5)).EQ.'')THEN
  !## va missing compute from kh and kv
  IDF(5,IM)%V=IDF(4,IM)%V/IDF(3,IM)%V
 ELSEIF(TRIM(LIST(4)).EQ.'')THEN
  IDF(4,IM)%V=IDF(3,IM)%V*IDF(5,IM)%V
 ELSEIF(TRIM(LIST(3)).EQ.'')THEN
  IDF(3,IM)%V=IDF(4,IM)%V/IDF(5,IM)%V
 ENDIF
 
 END SUBROUTINE LHM_ADDIWHB_RASTER_READ_DATA

 !###=====================================================
 SUBROUTINE LHM_ADDIWHB_RASTER_WRITE_DATA(IDF,BSIZE)
 !###=====================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),DIMENSION(:),INTENT(INOUT) :: IDF
 INTEGER,INTENT(IN) :: BSIZE
 INTEGER :: I,J
 
 DO I=1,SIZE(IDF)
  !## write block
  IF(IDF(I)%ITYPE.EQ.4)THEN
   WRITE(IDF(I)%IU) (REAL(IDF(I)%V(J),4),J=1,BSIZE)
  ELSE
   WRITE(IDF(I)%IU) (IDF(I)%V(J),J=1,BSIZE)
  ENDIF
 ENDDO
 
 END SUBROUTINE LHM_ADDIWHB_RASTER_WRITE_DATA

 !###=====================================================
 SUBROUTINE LHM_ADDIWHB()
!###=====================================================
 IMPLICIT NONE
 INTEGER :: I,II,III,J,K,L,N,M,O,IROW,ICOL,IU,NS
 REAL(KIND=DP_KIND) :: F,T,B
 INTEGER,DIMENSION(:),ALLOCATABLE :: NL
 TYPE(IDFOBJ) :: MDL,SURFL
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 
 CALL IDFNULLIFY(MDL); ALLOCATE(IDF(5)); DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO

 !## specify window
 IF(IWINDOW.EQ.1)THEN
  MDL%XMIN=XMIN; MDL%XMAX=XMAX; MDL%YMIN=YMIN; MDL%YMAX=YMAX; MDL%DX=CELLSIZE; MDL%DY=MDL%DX
  CALL UTL_IDFSNAPTOGRID_LLC(MDL%XMIN,MDL%XMAX,MDL%YMIN,MDL%YMAX,MDL%DX,MDL%DY,MDL%NCOL,MDL%NROW,.TRUE.)
 ELSE
  IF(.NOT.IDFREAD(MDL,FLIST(1)%FILE(1),0))STOP
 ENDIF
 
 N=SIZE(FLIST)+SIZE(ALIST); ALLOCATE(FM(N),NL(N))

 !## allocate surface level
 IF(.NOT.LHM_ADDIWHB_READFLIST(MDL,IDF(1),IDF(2),IDF(3),IDF(4),IDF(5),SIZE(FLIST(1)%FILE),FLIST(1)%FILE,INDIR))STOP
 CALL IDFCOPY(IDF(1),SURFL)

 N=SIZE(FLIST); DO I=1,N

  !## read data
  IF(.NOT.LHM_ADDIWHB_READFLIST(MDL,IDF(1),IDF(2),IDF(3),IDF(4),IDF(5),SIZE(FLIST(I)%FILE),FLIST(I)%FILE,INDIR))STOP
  CALL LHM_ADDIWHB_ADDDATA(I,FLIST(I)%FILE,IDF,SURFL); FM(I)%IORDER=I
  
  F=100.0D0*DBLE(I)/DBLE(N); WRITE(6,'(A)') '+READING '//TRIM(FLIST(I)%FILE(1))//'('//TRIM(RTOS(F,'F',2))//'%)         '
  WRITE(*,'(99A)') (TRIM(FLIST(I)%FILE(J))//',',J=1,4),TRIM(FLIST(I)%FILE(5))
 ENDDO

 IF(ASSOCIATED(ALIST))THEN
  !## modifying the solid
  N=SIZE(FLIST)+SIZE(ALIST); J=0; DO I=SIZE(FLIST)+1,N
   !## read data
   J=J+1
   IF(.NOT.LHM_ADDIWHB_READFLIST(MDL,IDF(1),IDF(2),IDF(3),IDF(4),IDF(5),SIZE(ALIST(J)%FILE),ALIST(J)%FILE,IWHBDIR))STOP
   F=100.0D0*DBLE(J)/DBLE(SIZE(ALIST)); WRITE(6,'(A)') '+READING '//TRIM(ALIST(J)%FILE(1))//'('//TRIM(RTOS(F,'F',2))//'%)          '

   !## read in data
   CALL LHM_ADDIWHB_ADDDATA(I,ALIST(J)%FILE,IDF,SURFL)

   SELECT CASE (ALIST(J)%METH)
    !## find location
    CASE (1)
     !## check minimum and maximum layer to insert layer
     CALL LHM_ADDIWHB_GETMAXLAYER(I,SIZE(NL),NL,IDF); IF(SUM(NL).EQ.0)CYCLE
     !## get layer
     L=0; K=0; DO II=1,SIZE(NL); IF(NL(II).GT.K)THEN; K=NL(II); L=II; ENDIF; ENDDO; FM(I)%IORDER=L
     !## shift all others
     DO II=1,I-1; IF(FM(II)%IORDER.GE.L)FM(II)%IORDER=FM(II)%IORDER+1; ENDDO
    !## fixed location
    CASE (2:4)
     DO L=1,SIZE(NL); IF(FM(L)%IORDER.EQ.ALIST(J)%ILAYER)EXIT; ENDDO; FM(I)%IORDER=L
   END SELECT
   
   SELECT CASE (ALIST(J)%METH)
    CASE (1)
     !## correct all others for this inserted layer
     CALL LHM_ADDIWHB_INSERT_IWHB(I,IDF) !,SURFL)
    CASE (2:4)
     !## fixed location
     CALL LHM_ADDIWHB_REPLACE_IWHB(I,ALIST(J)%METH,IDF)
     !## correct all others for this inserted layer
     CALL LHM_ADDIWHB_INSERT_IWHB(I,IDF) !,SURFL) !,ALIST(J)%METH)
   END SELECT
  
  ENDDO
 ENDIF

 CALL UTL_CREATEDIR(OUTDIR); IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(OUTDIR)//'\IWHB_SORTFILE.TXT',STATUS='UNKNOWN',ACTION='WRITE')
 WRITE(IU,'(5A32)') 'TOP','BOT','KH','KV','VA'

 !## need to be completed ... fill with previous values
 IF(.NOT.LHM_ADDIWHB_READFLIST(MDL,IDF(1),IDF(2),IDF(3),IDF(4),IDF(5),SIZE(FLIST(1)%FILE),FLIST(1)%FILE,INDIR))STOP
 IF(IDF(5)%NCOL.LE.0)CALL IDFCOPY(IDF(1),IDF(5)); IDF(2)%X=IDF(1)%X
 
 !## number of formations
 N=SIZE(FM); DO III=1,N
  
  !## get correct number
  DO I=1,N; IF(FM(I)%IORDER.EQ.III)EXIT; ENDDO
  !## nothing to do for this layer
  IF(I.GT.N)CYCLE
  
  !## number of subformations
  IF(.NOT.ASSOCIATED(FM(I)%SF))CYCLE
  M=SIZE(FM(I)%SF)
  DO J=1,M 
   IF(.NOT.ASSOCIATED(FM(I)%SF(J)%AT))CYCLE
   O=SIZE(FM(I)%SF(J)%AT); IF(O.EQ.0)EXIT

   DO II=3,5; IDF(II)%X=IDF(II)%NODATA; ENDDO
   
   NS=0
   DO L=1,O
    IROW=INT(FM(I)%SF(J)%AT(L)%IROW,4)
    ICOL=INT(FM(I)%SF(J)%AT(L)%ICOL,4)
    T=FM(I)%SF(J)%AT(L)%TP; B=FM(I)%SF(J)%AT(L)%BT

    !## take minimal value of current level (idf) - inserted level can above surfacelevel
    T=MIN(T,IDF(1)%X(ICOL,IROW)); B=MIN(B,IDF(1)%X(ICOL,IROW))
!      if(t.eq.idf(1)%nodata.or.b.eq.idf(2)%nodata)then
!        write(*,*) t,idf(1)%nodata,b,idf(2)%nodata,irow,icol,i,iii
!      endif
    !## correct them
    FM(I)%SF(J)%AT(L)%TP=T; FM(I)%SF(J)%AT(L)%BT=B
    !## due to aggregation layers can become zero thickness
    IF(T-B.GT.0.0D0)THEN
     NS=NS+1
     !## take minimal value as the first layer can have correction for "empty" spaces
     IDF(1)%X(ICOL,IROW)=FM(I)%SF(J)%AT(L)%TP
       IF(FM(I)%SF(J)%AT(L)%TP.LT.-9000.0D0)THEN
        WRITE(*,*) FM(I)%SF(J)%AT(L)%TP,i,icol,irow
        PAUSE
       endif
     IDF(2)%X(ICOL,IROW)=FM(I)%SF(J)%AT(L)%BT  
     IDF(3)%X(ICOL,IROW)=FM(I)%SF(J)%AT(L)%KH
     IDF(4)%X(ICOL,IROW)=FM(I)%SF(J)%AT(L)%KV
     IDF(5)%X(ICOL,IROW)=IDF(4)%X(ICOL,IROW)/IDF(3)%X(ICOL,IROW)
    ENDIF
   ENDDO
   
   !## nothing left from this layer
   IF(NS.EQ.0)CYCLE
   
   !## save idf files
   DO L=1,5
    IF(I.LE.SIZE(FLIST))THEN
     IDF(L)%FNAME=TRIM(OUTDIR)//'\'//TRIM(FLIST(I)%FILE(L))
    ELSE
     II=I-SIZE(FLIST)
     IDF(L)%FNAME=TRIM(OUTDIR)//'\'//TRIM(ALIST(II)%FILE(L))
    ENDIF
   ENDDO
   CALL LHM_CONVERTREGIS_OUTPUT(IDF(1),IDF(2),IDF(3),IDF(4),IDF(5),I)
   DO L=1,5
    II=INDEX(IDF(L)%FNAME,'\',.TRUE.); IF(II.NE.0)IDF(L)%FNAME=IDF(L)%FNAME(II+1:)
   ENDDO
   WRITE(IU,'(5A32)') (TRIM(IDF(L)%FNAME),L=1,5)

   DO L=1,O
    IROW=INT(FM(I)%SF(J)%AT(L)%IROW,4)
    ICOL=INT(FM(I)%SF(J)%AT(L)%ICOL,4)
    IDF(1)%X(ICOL,IROW)=FM(I)%SF(J)%AT(L)%BT
   ENDDO

  ENDDO

  II=LEN_TRIM(OUTDIR)
  F=100.0D0*DBLE(I)/DBLE(N); WRITE(6,'(A)') '+WRITING '//TRIM(IDF(1)%FNAME(II+2:))//'('//TRIM(RTOS(F,'F',2))//'%)         '
 
 ENDDO
 CLOSE(IU)

 DO I=1,SIZE(FM)
  IF(ASSOCIATED(FM(I)%SF))THEN
   DO J=1,SIZE(FM(I)%SF)
    IF(ASSOCIATED(FM(I)%SF(J)%AT))DEALLOCATE(FM(I)%SF(J)%AT)
   ENDDO
   DEALLOCATE(FM(I)%SF)
  ENDIF
 ENDDO
 DO I=1,SIZE(IDF); CALL IDFDEALLOCATEX(IDF(I)); CALL IDFDEALLOCATESX(IDF(I)); ENDDO
 
 END SUBROUTINE LHM_ADDIWHB
 
 !###======================================================
 SUBROUTINE LHM_ADDIWHB_INSERT_IWHB(IFM,IDF)
 !###======================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFM
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(5) :: IDF
 INTEGER :: I,II,J,JJ,K,N,IROW,ICOL,IORDER,M
 REAL(KIND=DP_KIND) :: T,B,VC,TR,DZ,TF,BF
 LOGICAL :: LTOP,LBOT
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: DZT,DZB
 INTEGER,ALLOCATABLE,DIMENSION(:,:,:) :: BFM,IFP
 
 !## location of inserted layer
 IORDER=FM(IFM)%IORDER

 !## process current iwhb layer
 DO II=1,IFM

  !## skip this one as it is the current iwhb to be processed
  IF(II.EQ.IORDER)CYCLE

  LTOP=II.LT.IORDER
  LBOT=II.GT.IORDER

  !## get correct number
  DO I=1,IFM; IF(FM(I)%IORDER.EQ.II)EXIT; ENDDO
  !## nothing to insert
  IF(I.GT.IFM)CYCLE
  
  !## number of subformations
  IF(.NOT.ASSOCIATED(FM(I)%SF))CYCLE
  DO J=1,SIZE(FM(I)%SF)
   IF(.NOT.ASSOCIATED(FM(I)%SF(J)%AT))CYCLE
   N=SIZE(FM(I)%SF(J)%AT); IF(N.EQ.0)EXIT
   DO K=1,N
    IROW=INT(FM(I)%SF(J)%AT(K)%IROW,4)
    ICOL=INT(FM(I)%SF(J)%AT(K)%ICOL,4)

    T=IDF(1)%X(ICOL,IROW); B=IDF(2)%X(ICOL,IROW)
    !## iwhb has a thickness here - correct if needed
    IF(T-B.GT.0.0D0)THEN
     
     DZ=FM(I)%SF(J)%AT(K)%TP-FM(I)%SF(J)%AT(K)%BT
     TR=DZ*FM(I)%SF(J)%AT(K)%KH
     VC=DZ/FM(I)%SF(J)%AT(K)%KV
     
     !## layers above
     IF(LTOP)THEN
      FM(I)%SF(J)%AT(K)%TP=MAX(FM(I)%SF(J)%AT(K)%TP,T)
      FM(I)%SF(J)%AT(K)%BT=MAX(FM(I)%SF(J)%AT(K)%BT,T)
     !## layer underneath
     ELSEIF(LBOT)THEN
      FM(I)%SF(J)%AT(K)%TP=MIN(FM(I)%SF(J)%AT(K)%TP,B)
      FM(I)%SF(J)%AT(K)%BT=MIN(FM(I)%SF(J)%AT(K)%BT,B)
     ENDIF

     !## new dz
     DZ=FM(I)%SF(J)%AT(K)%TP-FM(I)%SF(J)%AT(K)%BT
     !## correct if possible
     IF(DZ.LE.0.0D0)THEN; FM(I)%SF(J)%AT(K)%KH=0.0D0; FM(I)%SF(J)%AT(K)%KV=0.0D0; ENDIF

    ENDIF
   ENDDO
  ENDDO
 ENDDO
 
! return
! SELECT CASE (IMETH)
!  CASE (1,2,3,4)
 !## set zero-thickness to nodata
 DO J=1,SIZE(FM(IFM)%SF)
  IF(.NOT.ASSOCIATED(FM(IFM)%SF(J)%AT))CYCLE
  DEALLOCATE(FM(IFM)%SF(J)%AT)
 ENDDO
! END SELECT
 
 !## find formation(s) nearest from top and bottom
 ALLOCATE(DZT(IDF(1)%NCOL,IDF(1)%NROW)); DZT=HUGE(1.0)
 ALLOCATE(DZB(IDF(1)%NCOL,IDF(1)%NROW)); DZB=HUGE(1.0)
 ALLOCATE(BFM(IDF(1)%NCOL,IDF(1)%NROW,2)); BFM=0
 ALLOCATE(IFP(IDF(1)%NCOL,IDF(1)%NROW,2)); IFP=0
 
 !## correct for caps
 DO II=1,IFM-1
  IF(.NOT.ASSOCIATED(FM(II)%SF))CYCLE
  DO J=1,SIZE(FM(II)%SF)
   IF(.NOT.ASSOCIATED(FM(II)%SF(J)%AT))CYCLE
   N=SIZE(FM(II)%SF(J)%AT); IF(N.EQ.0)EXIT
   DO K=1,N
    !## skip location of current formation without thickness
    TF=FM(II)%SF(J)%AT(K)%TP; BF=FM(II)%SF(J)%AT(K)%BT; IF(TF-BF.LE.0.0D0)CYCLE
    IROW=INT(FM(II)%SF(J)%AT(K)%IROW,4); ICOL=INT(FM(II)%SF(J)%AT(K)%ICOL,4)
    
    IF(IROW.EQ.5.AND.ICOL.EQ.6)THEN
     WRITE(*,*) 
    ENDIF
    
    T=IDF(1)%X(ICOL,IROW); B=IDF(2)%X(ICOL,IROW)
    !## skip nodata location - continue with location that need to be removed and/or are new
    IF(T.EQ.IDF(1)%NODATA.OR.B.EQ.IDF(2)%NODATA)CYCLE
!    !## skip if not a new location for a potential "leak"
!    IF(T-B.LE.0.0D0)CYCLE
    !## compute distance above
    DZ=BF-T
    IF(DZ.GE.0.0D0.AND.DZ.LE.DZT(ICOL,IROW))THEN
     DZT(ICOL,IROW)=DZ; BFM(ICOL,IROW,1)=II; IFP(ICOL,IROW,1)=K
    ENDIF
    !## compute distance below
    DZ=B-TF
    IF(DZ.GE.0.0D0.AND.DZ.LE.DZB(ICOL,IROW))THEN
!     if(ii.eq.96)then
!     write(*,*) 
!     endif
     DZB(ICOL,IROW)=DZ; BFM(ICOL,IROW,2)=II; IFP(ICOL,IROW,2)=K
    ENDIF
   ENDDO
  ENDDO
 ENDDO

 !## get pointer where first layer is defined
 CALL IDFCOPY(IDF(1),IDF(5)); IF(.NOT.IDFALLOCATEX(IDF(5)))RETURN
 IDF(5)%X=0.0D0; N=SIZE(FM(1)%SF(1)%AT)
 DO I=1,N
  IROW=FM(1)%SF(1)%AT(I)%IROW 
  ICOL=FM(1)%SF(1)%AT(I)%ICOL
  T   =FM(1)%SF(1)%AT(I)%TP
  B   =FM(1)%SF(1)%AT(I)%BT
  IF(T-B.GT.0.0D0)IDF(5)%X(ICOL,IROW)=1.0D0
 ENDDO
 
 !## correct single formation above and below
 N=0; DO I=1,2
  DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL

   !## skip nodata location - continue with location that need to be removed and/or are new
   T=IDF(1)%X(ICOL,IROW); B=IDF(2)%X(ICOL,IROW)
   IF(T.EQ.IDF(1)%NODATA.OR.B.EQ.IDF(2)%NODATA)CYCLE

   !## check upper face
   IF(BFM(ICOL,IROW,1).NE.0)THEN
    IF(I.EQ.1)THEN
     II=BFM(ICOL,IROW,1); JJ=IFP(ICOL,IROW,1); FM(II)%SF(1)%AT(JJ)%BT=IDF(1)%X(ICOL,IROW)
       if(IDF(1)%X(ICOL,IROW).lt.-9000.0d0)then
        write(*,*) icol,irow,IDF(1)%X(ICOL,IROW),idf(1)%nodata,ii,jj
        pause
       endif
    ENDIF
   ELSE

    !## cannot enter twice a value - original is added to it
    IF(IDF(5)%X(ICOL,IROW).GT.0.0D0)CYCLE
    
    T=IDF(1)%X(ICOL,IROW); B=IDF(2)%X(ICOL,IROW)
    !## process location ne nodata
    IF(T.NE.IDF(1)%NODATA.AND.B.NE.IDF(2)%NODATA)THEN
     N=N+1
!     write(*,*) n,t,b,idf(1)%nodata,idf(2)%nodata
     IF(I.EQ.2)THEN
      FM(1)%SF(1)%AT(N)%IROW=INT(IROW,2)
      FM(1)%SF(1)%AT(N)%ICOL=INT(ICOL,2)
      !## this value will be trimmed
      FM(1)%SF(1)%AT(N)%TP=1000.0 !SURFL%X(ICOL,IROW) !10000.0 !HUGE(1.0)
      !## bot is used - top is used from the previous layer
      FM(1)%SF(1)%AT(N)%BT=T !MIN(T,SURFL%X(ICOL,IROW))  !T
      FM(1)%SF(1)%AT(N)%KH=1.0
      FM(1)%SF(1)%AT(N)%KV=1.0
      FM(1)%SF(1)%AT(N)%VA=1.0
     ENDIF
    ENDIF
   ENDIF
   !## check lower face
   IF(BFM(ICOL,IROW,2).NE.0)THEN
    IF(I.EQ.1)THEN
     II=BFM(ICOL,IROW,2); JJ=IFP(ICOL,IROW,2); FM(II)%SF(1)%AT(JJ)%TP=IDF(2)%X(ICOL,IROW)
       if(IDF(2)%X(ICOL,IROW).lt.-9000.0d0)then
        write(*,*) icol,irow,IDF(2)%X(ICOL,IROW),idf(2)%nodata,ii,jj
        pause !dzb(13,21)
       endif     
    ENDIF
   ENDIF
  ENDDO; ENDDO
  
!  if(i.eq.1)exit
  
  !## add artificial thickness to layer - nothing found on top
  IF(I.EQ.1.AND.N.GT.0)THEN
   M=SIZE(FM(1)%SF(1)%AT); ALLOCATE(FM(1)%SF(1)%AT_DUMMY(N+M))
   WRITE(*,*) 'SIZES ',M,N,N+M
   DO J=1,M; FM(1)%SF(1)%AT_DUMMY(J)=FM(1)%SF(1)%AT(J); ENDDO
   DEALLOCATE(FM(1)%SF(1)%AT); FM(1)%SF(1)%AT=>FM(1)%SF(1)%AT_DUMMY; N=M !-1
   WRITE(*,*) 'START @ ',N
  ENDIF
  
 ENDDO
 
 DEALLOCATE(DZT,DZB,IFP,BFM)

!! SELECT CASE (IMETH)
!!  CASE (1,2,3,4)
 !## set zero-thickness to nodata
 DO J=1,SIZE(FM(IFM)%SF)
  IF(.NOT.ASSOCIATED(FM(IFM)%SF(J)%AT))CYCLE
  DEALLOCATE(FM(IFM)%SF(J)%AT)
 ENDDO
!! END SELECT
 
 END SUBROUTINE LHM_ADDIWHB_INSERT_IWHB
 
 !###======================================================
 SUBROUTINE LHM_ADDIWHB_REPLACE_IWHB(IFM,IMETH,IDF)
 !###======================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFM,IMETH
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(5) :: IDF 
 INTEGER :: I,J,N,IROW,ICOL,IORDER
 REAL(KIND=DP_KIND) :: T,B,TP,BT,D,TR,VC,KH,KV
 
 !## location of inserted layer
 IORDER=FM(IFM)%IORDER

 !## combine with existing data
 IF(ASSOCIATED(FM(IORDER)%SF))THEN
  DO I=1,SIZE(FM(IORDER)%SF)
   IF(.NOT.ASSOCIATED(FM(IORDER)%SF(I)%AT))CYCLE
   
   WRITE(*,*) 'A ',IORDER,'n=',SIZE(FM(IORDER)%SF(I)%AT)
   
   DO J=1,SIZE(FM(IORDER)%SF(I)%AT)
  
    IROW=INT(FM(IORDER)%SF(I)%AT(J)%IROW,4)
    ICOL=INT(FM(IORDER)%SF(I)%AT(J)%ICOL,4)
    TP  =REAL(FM(IORDER)%SF(I)%AT(J)%TP,8)
    BT  =REAL(FM(IORDER)%SF(I)%AT(J)%BT,8)
    KH  =REAL(FM(IORDER)%SF(I)%AT(J)%KH,8)
    KV  =REAL(FM(IORDER)%SF(I)%AT(J)%KV,8)
!    if(icol.eq.13.and.irow.eq.21)then
!    write(*,*)
!    endif
    !## top and bottom of iwhb layer
    T=IDF(1)%X(ICOL,IROW); B=IDF(2)%X(ICOL,IROW)

    !## new data available here
    IF(T.NE.IDF(1)%NODATA.AND.B.NE.IDF(2)%NODATA.AND.(T-B).GT.0.0D0)THEN
     TR=0.0D0; VC=0.0D0
    
     !## add to an existing layer
     IF(IMETH.EQ.3)THEN
      !## remaining original thickness
      D=MAX(0.0D0,(TP-BT)-(T-B))
      !## remaining transmissivity
      TR=D*KH
      !## remaining vertical resistance
      VC=D/KV
     ENDIF
    
     !## compute thickness
     D=T-B

     KH=IDF(3)%X(ICOL,IROW)
     KV=IDF(4)%X(ICOL,IROW)
     !## compute total transmissivity
     TR=TR+D*KH
     !## compute total vertical resistance
     VC=VC+D/KV

     IF(IMETH.EQ.3)THEN
      !## update top- and bottom values
      T=MAX(TP,T)
      B=MIN(BT,B)
     ENDIF
    
     !## recompute representative k-values
     D =T-B
     KH=TR/D
     KV=D/VC

     !## save updated values
     IDF(1)%X(ICOL,IROW)=T
     IDF(2)%X(ICOL,IROW)=B
     IDF(3)%X(ICOL,IROW)=KH
     IDF(4)%X(ICOL,IROW)=KV
   
    !## no update here needed, copy (imethod=2/3) or remove (imethod=4) existing data
    ELSE
     
     IDF(1)%X(ICOL,IROW)=TP
     !## only if imethod is 4
     IF(IMETH.EQ.4)THEN
      !## give a zero thickness - needed to shift all layers here
      IDF(2)%X(ICOL,IROW)=TP
     ELSE
      !## keep existing layer
      IDF(2)%X(ICOL,IROW)=BT
     ENDIF
     IDF(3)%X(ICOL,IROW)=KH; IDF(4)%X(ICOL,IROW)=KV

    ENDIF
   
   ENDDO
  ENDDO
 ENDIF
 
 !## get size of the total combined inserted layer
 N=0; DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  T=IDF(1)%X(ICOL,IROW); B=IDF(2)%X(ICOL,IROW)
  !## skip nodata locations
  IF(T.EQ.IDF(1)%NODATA.OR.B.EQ.IDF(2)%NODATA)CYCLE
  N=N+1
 ENDDO; ENDDO
 WRITE(*,*) 'B ',IORDER,'n=',N
 
 !## remove current layer
 IF(ASSOCIATED(FM(IORDER)%SF))THEN
  DO J=1,SIZE(FM(IORDER)%SF); IF(ASSOCIATED(FM(IORDER)%SF(J)%AT))DEALLOCATE(FM(IORDER)%SF(J)%AT); ENDDO
 ENDIF
 
 !## fill in new object of this layer
 ALLOCATE(FM(IORDER)%SF(1)%AT(N))
 
 !## add new layer to it (include zero thickness of layer removed) - correct top/bot en k-values
 N=0; DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  T=IDF(1)%X(ICOL,IROW); B=IDF(2)%X(ICOL,IROW)
  !## skip nodata locations
  IF(T.EQ.IDF(1)%NODATA.OR.B.EQ.IDF(2)%NODATA)CYCLE
  N=N+1
  FM(IORDER)%SF(1)%AT(N)%IROW=INT(IROW,2)
  FM(IORDER)%SF(1)%AT(N)%ICOL=INT(ICOL,2)
  FM(IORDER)%SF(1)%AT(N)%TP  =REAL(IDF(1)%X(ICOL,IROW),4)
  FM(IORDER)%SF(1)%AT(N)%BT  =REAL(IDF(2)%X(ICOL,IROW),4)
  FM(IORDER)%SF(1)%AT(N)%KH  =REAL(IDF(3)%X(ICOL,IROW),4)
  FM(IORDER)%SF(1)%AT(N)%KV  =REAL(IDF(4)%X(ICOL,IROW),4)
 ENDDO; ENDDO
 
 END SUBROUTINE LHM_ADDIWHB_REPLACE_IWHB

 !###======================================================
 SUBROUTINE LHM_ADDIWHB_ADDDATA(IFM,FNAME,IDF,SURFL)
 !###======================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFM
 TYPE(IDFOBJ),INTENT(IN) :: SURFL
 CHARACTER(LEN=*),INTENT(IN),DIMENSION(:) :: FNAME
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(5) :: IDF
 INTEGER :: I,M,IROW,ICOL
 REAL(KIND=DP_KIND) :: T,B
 
 !## make sure data is nodata for zero thickness - set all equal to equal nodata
 M=0
 DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  T=MIN(SURFL%X(ICOL,IROW),IDF(1)%X(ICOL,IROW))
  B=MIN(SURFL%X(ICOL,IROW),IDF(2)%X(ICOL,IROW))
!  if(icol.eq.13.and.irow.eq.21)then
!  write(*,*)
!  endif
  IF(T.EQ.IDF(1)%NODATA.OR.B.EQ.IDF(2)%NODATA.OR.T-B.LE.0.0D0)THEN
   IDF(1)%X(ICOL,IROW)=IDF(1)%NODATA
   IDF(2)%X(ICOL,IROW)=IDF(2)%NODATA
   IDF(3)%X(ICOL,IROW)=IDF(3)%NODATA
   IDF(4)%X(ICOL,IROW)=IDF(4)%NODATA
  ELSE
!   WRITE(*,'(A,3F10.3,2I5)') 'T-B=',T,B,T-B,ICOL,IROW
   M=M+1
  ENDIF
 ENDDO; ENDDO
 WRITE(*,*) M
 
 DO I=1,2
  M=0; DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
   T=IDF(1)%X(ICOL,IROW); B=IDF(2)%X(ICOL,IROW)
   IF(T.NE.IDF(1)%NODATA.AND.B.NE.IDF(2)%NODATA)THEN
    IF(T-B.GT.0.0D0)THEN
     M=M+1
     IF(I.EQ.2)THEN
      FM(IFM)%SF(1)%AT(M)%IROW=INT(IROW,2)
      FM(IFM)%SF(1)%AT(M)%ICOL=INT(ICOL,2)
      FM(IFM)%SF(1)%AT(M)%TP  =REAL(T,4)
      FM(IFM)%SF(1)%AT(M)%BT  =REAL(B,4)
      FM(IFM)%SF(1)%AT(M)%KH  =REAL(IDF(3)%X(ICOL,IROW),4)
      FM(IFM)%SF(1)%AT(M)%KV  =REAL(IDF(4)%X(ICOL,IROW),4)
     ENDIF
    ENDIF
   ENDIF
  ENDDO; ENDDO
  IF(I.EQ.1)THEN; ALLOCATE(FM(IFM)%SF(MF)); ALLOCATE(FM(IFM)%SF(1)%AT(M)); ENDIF
 ENDDO
 DO I=1,5; IDF(I)%FNAME=ADJUSTL(FNAME(I)); ENDDO
 
 END SUBROUTINE LHM_ADDIWHB_ADDDATA
 
 !###======================================================
 SUBROUTINE LHM_ADDIWHB_GETMAXLAYER(IFM,N,NL,IDF)
 !###======================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFM,N
 INTEGER,INTENT(OUT),DIMENSION(N) :: NL
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(5) :: IDF
 INTEGER :: IROW,ICOL,I,J,K,NNODATA,M,NTHICKN
 REAL(KIND=DP_KIND) :: T,B,TF,BF
 
 NL=0; NNODATA=0; NTHICKN=0
 
 !## check formations till now
ILOOP: DO I=1,IFM-1
  IF(.NOT.ASSOCIATED(FM(I)%SF))CYCLE
  !## number of subformations
  DO J=1,SIZE(FM(I)%SF)
   IF(.NOT.ASSOCIATED(FM(I)%SF(J)%AT))CYCLE
   M=SIZE(FM(I)%SF(J)%AT); IF(M.EQ.0)EXIT
   DO K=1,M
    IROW=INT(FM(I)%SF(J)%AT(K)%IROW,4)
    ICOL=INT(FM(I)%SF(J)%AT(K)%ICOL,4)
    
    T   =IDF(1)%X(ICOL,IROW)
    B   =IDF(2)%X(ICOL,IROW)
    !## no thickness of current iwhb layer
    IF(T-B.LE.0.0D0)CYCLE

    NTHICKN=NTHICKN+1
    NNODATA=1
    TF  =FM(I)%SF(J)%AT(K)%TP
    BF  =FM(I)%SF(J)%AT(K)%BT
    IF(TF.GT.T.AND.BF.LE.T)NL(I)=NL(I)+1
    IF(BF.LE.B.AND.TF.GT.B)NL(I)=NL(I)+1

   ENDDO
  ENDDO
 ENDDO ILOOP
  
 !## find nothing but there is something to add
 IF(SUM(NL).EQ.0.AND.NTHICKN.GT.0)THEN
  IF(NNODATA.EQ.0)NL(1)=1   !## nodata found underneath this formation --- add it to layer 1
  IF(NNODATA.EQ.1)NL(IFM)=1 !## probably underneath existing formations -- add it below layer ifm-1
 ENDIF
 
 END SUBROUTINE LHM_ADDIWHB_GETMAXLAYER

 !###======================================================
 LOGICAL FUNCTION LHM_ADDIWHB_READFLIST(IDF,TP,BT,KH,KV,VA,N,LIST,DIR)  
 !###======================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF,TP,BT,KH,KV,VA
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER,INTENT(IN) :: N
 CHARACTER(LEN=256) :: FN
 INTEGER :: ICOL,IROW,IOS
 REAL(KIND=DP_KIND) :: X
 CHARACTER(LEN=*),DIMENSION(N) :: LIST
 LOGICAL :: LEX
 
 LHM_ADDIWHB_READFLIST=.FALSE.
 
 READ(LIST(1),*,IOSTAT=IOS) X; IF(IOS.NE.0)THEN; FN=TRIM(DIR)//'\'//TRIM(LIST(1)); ELSE; FN=TRIM(LIST(1)); LEX=.TRUE.; ENDIF
 IF(IWINDOW.EQ.1)THEN
  CALL IDFCOPY(IDF,TP); IF(.NOT.IDFREADSCALE(FN,TP,2,1,0.0D0,0))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
 ELSE
  IF(.NOT.IDFREAD(TP,FN,1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
 ENDIF
  
 READ(LIST(2),*,IOSTAT=IOS) X; IF(IOS.NE.0)THEN; FN=TRIM(DIR)//'\'//TRIM(LIST(2)); ELSE; FN=TRIM(LIST(2)); LEX=.TRUE.; ENDIF
 IF(IWINDOW.EQ.1)THEN
  CALL IDFCOPY(IDF,BT); IF(.NOT.IDFREADSCALE(FN,BT,2,1,0.0D0,0))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
 ELSE
  IF(.NOT.IDFREAD(BT,FN,1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF
 ENDIF
 
 !## make sure with constant values not to add dir
 LEX=.FALSE.; IF(TRIM(LIST(3)).NE.'')THEN
  READ(LIST(3),*,IOSTAT=IOS) X; IF(IOS.NE.0)THEN; FN=TRIM(DIR)//'\'//TRIM(LIST(3)); ELSE; FN=TRIM(LIST(3)); LEX=.TRUE.; ENDIF
  IF(IWINDOW.EQ.1)THEN
   CALL IDFCOPY(IDF,KH); IF(.NOT.IDFREADSCALE(FN,KH,3,1,0.0D0,0))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
  ELSE
   IF(.NOT.IDFREAD(KH,FN,1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF  
  ENDIF
 ELSE
  CALL IDFCOPY(IDF,KH); IF(.NOT.IDFALLOCATEX(KH))THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot ALLOCATE memory for a constant value KH.','Error'); RETURN
  ENDIF; LEX=.TRUE.
 ENDIF
 
 !## make sure with constant values not to add dir
 LEX=.FALSE.; IF(TRIM(LIST(4)).NE.'')THEN
  READ(LIST(4),*,IOSTAT=IOS) X; IF(IOS.NE.0)THEN; FN=TRIM(DIR)//'\'//TRIM(LIST(4)); ELSE; FN=TRIM(LIST(4)); LEX=.TRUE.; ENDIF
  IF(IWINDOW.EQ.1)THEN
   CALL IDFCOPY(IDF,KV); IF(.NOT.IDFREADSCALE(FN,KV,3,1,0.0D0,0))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
  ELSE
   IF(.NOT.IDFREAD(KV,FN,1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF
  ENDIF
 ELSE
  CALL IDFCOPY(IDF,KV); IF(.NOT.IDFALLOCATEX(KV))THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot ALLOCATE memory for a constant value KV.','Error'); RETURN
  ENDIF; LEX=.TRUE.
 ENDIF

 !## if kh and kv are not both read, use va
 LEX=.TRUE.; IF(TRIM(LIST(3)).EQ.''.OR.TRIM(LIST(4)).EQ.'')THEN
  IF(TRIM(LIST(5)).EQ.'')THEN
   WRITE(*,'(/A/)') 'ERROR entry for VA is empty'; STOP
  ENDIF
  READ(LIST(5),*,IOSTAT=IOS) X; IF(IOS.NE.0)THEN; FN=TRIM(DIR)//'\'//TRIM(LIST(5)); LEX=.FALSE.; ELSE; FN=TRIM(LIST(5)); ENDIF
  IF(IWINDOW.EQ.1)THEN
   CALL IDFCOPY(IDF,VA); IF(.NOT.IDFREADSCALE(FN,VA,2,1,0.0D0,0))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF 
  ELSE
   IF(.NOT.IDFREAD(VA,FN,1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FN); STOP; ENDIF
  ENDIF
  !## fill in values for kh or kv
  IF(TRIM(LIST(3)).EQ.'')THEN
   !## missing kh, create it from kv
   DO IROW=1,VA%NROW; DO ICOL=1,VA%NCOL
    IF(VA%X(ICOL,IROW).NE.VA%NODATA.AND.KV%X(ICOL,IROW).NE.KV%NODATA)THEN
     KH%X(ICOL,IROW)=KV%X(ICOL,IROW)/VA%X(ICOL,IROW)
    ENDIF
   ENDDO; ENDDO
  ELSE
   !## missing kv, create it from kh
   DO IROW=1,VA%NROW; DO ICOL=1,VA%NCOL
    IF(VA%X(ICOL,IROW).NE.VA%NODATA.AND.KH%X(ICOL,IROW).NE.KH%NODATA)THEN
     KV%X(ICOL,IROW)=KH%X(ICOL,IROW)*VA%X(ICOL,IROW)
    ENDIF
   ENDDO; ENDDO
  ENDIF
 ENDIF
 
 LHM_ADDIWHB_READFLIST=.TRUE.
 
 END FUNCTION LHM_ADDIWHB_READFLIST 

 !###======================================================
 SUBROUTINE LHM_CONVERTREGIS()
 !###======================================================
 IMPLICIT NONE
 INTEGER :: I,J,II,N,ICOL,IROW,IU
 REAL(KIND=DP_KIND) :: T,B,K1,K2,X,Y,VA
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
  TPIDF%NODATA=TPK(1,1)%NODATA
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

   !## constant of variable value
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
    !## absent is -1, 0 is idf, 1=constant value
    IF(FLIST(I)%ITYPE(3).EQ.1)THEN; K1=FLIST(I)%XVAL(3); ELSE; K1=IDFGETXYVAL(TPK(I,3),X,Y); ENDIF
    IF(FLIST(I)%ITYPE(4).EQ.1)THEN; K2=FLIST(I)%XVAL(4); ELSE; K2=IDFGETXYVAL(TPK(I,4),X,Y); ENDIF
    IF(FLIST(I)%ITYPE(5).EQ.1)THEN; VA=FLIST(I)%XVAL(5); ELSE; VA=IDFGETXYVAL(TPK(I,5),X,Y); ENDIF
    !## kh not defined, compute from kv and va
    IF(FLIST(I)%ITYPE(3).EQ.-1)THEN
     K1=K2/VA
    ELSEIF(FLIST(I)%ITYPE(4).EQ.-1)THEN
     K2=K1*VA
    ENDIF
    IF(K1.EQ.TPK(I,3)%NODATA)K1=EPS; IF(K2.EQ.TPK(I,4)%NODATA)K2=EPS
    IF(K1.EQ.0.0D0)K1=K2; IF(K2.EQ.0.0D0)K2=K1
    IF(K1.EQ.0.0D0.AND.K2.EQ.0.0D0)THEN
     K1=TPK(I,3)%NODATA; K2=K1; T=BTIDF%X(ICOL,IROW); B=BTIDF%X(ICOL,IROW)
    ELSE
     !## make sure it connects
     IF(I.GT.1)T=BTIDF%X(ICOL,IROW)
    ENDIF
   ENDIF

   TPIDF%X(ICOL,IROW)=T
   BTIDF%X(ICOL,IROW)=MIN(B,T)
   IBIDF%X(ICOL,IROW)=1
   IF(K1.NE.TPK(I,3)%NODATA.AND.K2.NE.TPK(I,4)%NODATA)THEN
    KHIDF%X(ICOL,IROW)=K1; KVIDF%X(ICOL,IROW)=K2; VAIDF%X(ICOL,IROW)=K2/K1
   ELSE
    KHIDF%X(ICOL,IROW)=1.0D0; KVIDF%X(ICOL,IROW)=1.0D0; VAIDF%X(ICOL,IROW)=1.0D0
   ENDIF
   
  ENDDO; WRITE(6,'(A,F10.3,A)') '+Progress ',REAL(100*IROW)/REAL(TPIDF%NROW),'%         '; ENDDO

  !## save formations
  DO J=1,5; FLIST(I)%FILE(J)=TRIM(ITOS(I))//'_'//FLIST(I)%FILE(J); ENDDO
  TPIDF%FNAME=TRIM(OUTDIR)//'\'//TRIM(FLIST(I)%FILE(1))
  BTIDF%FNAME=TRIM(OUTDIR)//'\'//TRIM(FLIST(I)%FILE(2))
  KHIDF%FNAME=TRIM(OUTDIR)//'\'//TRIM(FLIST(I)%FILE(3))
  KVIDF%FNAME=TRIM(OUTDIR)//'\'//TRIM(FLIST(I)%FILE(4))
  VAIDF%FNAME=TRIM(OUTDIR)//'\'//TRIM(FLIST(I)%FILE(5))
  CALL LHM_CONVERTREGIS_OUTPUT(TPIDF,BTIDF,KHIDF,KVIDF,VAIDF,I)
  FLIST(I)%FILE(1)=TPIDF%FNAME; FLIST(I)%FILE(2)=BTIDF%FNAME; FLIST(I)%FILE(3)=KHIDF%FNAME
  FLIST(I)%FILE(4)=KVIDF%FNAME; FLIST(I)%FILE(5)=VAIDF%FNAME
  DO J=1,5
   II=INDEX(FLIST(I)%FILE(J),'\',.TRUE.)
   IF(II.NE.0)THEN
    FLIST(I)%FILE(J)=FLIST(I)%FILE(J)(II+1:)
   ENDIF
  ENDDO
  WRITE(IU,'(5A32)') (TRIM(FLIST(I)%FILE(J)),J=1,5)
 ENDDO
 CLOSE(IU)
 
 END SUBROUTINE LHM_CONVERTREGIS

 !###======================================================
 SUBROUTINE LHM_CONVERTREGIS_OUTPUT(TPIDF,BTIDF,KHIDF,KVIDF,VAIDF,IFL) 
 !###======================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: TPIDF,BTIDF,KHIDF,KVIDF,VAIDF 
 INTEGER,INTENT(IN) :: IFL
 INTEGER :: IROW,ICOL,J,JJ
 REAL(KIND=DP_KIND) :: K
 LOGICAL :: LEX
 CHARACTER(LEN=256) :: FTXT
 
 IF(INDEX(TPIDF%FNAME,'.IDF').LE.0)THEN
  J= INDEX(TPIDF%FNAME,'\',.TRUE.)
  FTXT=TPIDF%FNAME(:J-1)//'\'//TRIM(ITOS(IFL))//'_FORMATION'; TPIDF%FNAME=TRIM(FTXT)//'-T.IDF' 
 ENDIF
 IF(.NOT.IDFWRITE(TPIDF,TPIDF%FNAME,1,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(TPIDF%FNAME); STOP; ENDIF  

 IF(INDEX(BTIDF%FNAME,'.IDF').LE.0)THEN
  J= INDEX(TPIDF%FNAME,'_',.TRUE.)
  JJ=INDEX(TPIDF%FNAME(J+1:),'-'); JJ=JJ+J
  FTXT=TPIDF%FNAME(:JJ-1); BTIDF%FNAME=TRIM(FTXT)//'-B-C.IDF' 
 ENDIF
 IF(.NOT.IDFWRITE(BTIDF,BTIDF%FNAME,1,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(BTIDF%FNAME); STOP; ENDIF  
 
 IF(INDEX(KHIDF%FNAME,'.IDF').LE.0)THEN
  J= INDEX(TPIDF%FNAME,'_',.TRUE.)
  JJ=INDEX(TPIDF%FNAME(J+1:),'-'); JJ=JJ+J
  FTXT=TPIDF%FNAME(:JJ-1); KHIDF%FNAME=TRIM(FTXT)//'-KH-S.IDF' 
 ENDIF
 IF(.NOT.IDFWRITE(KHIDF,KHIDF%FNAME,1,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(KHIDF%FNAME); STOP; ENDIF  

 IF(INDEX(KVIDF%FNAME,'.IDF').LE.0)THEN
  J= INDEX(TPIDF%FNAME,'_',.TRUE.)
  JJ=INDEX(TPIDF%FNAME(J+1:),'-'); JJ=JJ+J
  FTXT=TPIDF%FNAME(:JJ-1); KVIDF%FNAME=TRIM(FTXT)//'-KV-S.IDF' 
 ENDIF
 IF(.NOT.IDFWRITE(KVIDF,KVIDF%FNAME,1,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(KVIDF%FNAME); STOP; ENDIF  
 
 IF(ASSOCIATED(VAIDF%X))THEN
  !## check whether this is variable
  LEX=.TRUE.; K=VAIDF%NODATA; DO IROW=1,VAIDF%NROW; DO ICOL=1,VAIDF%NCOL
   IF(VAIDF%X(ICOL,IROW).NE.VAIDF%NODATA)THEN
    IF(K.EQ.VAIDF%NODATA)K=VAIDF%X(ICOL,IROW)
    IF(K.NE.VAIDF%X(ICOL,IROW))THEN; LEX=.FALSE.; EXIT; ENDIF
   ENDIF
  ENDDO; IF(.NOT.LEX)EXIT; ENDDO
 ELSE
  LEX=.TRUE.; K=1.0D0
 ENDIF
 IF(.NOT.LEX)THEN
  IF(INDEX(VAIDF%FNAME,'.IDF').LE.0)THEN
   J= INDEX(TPIDF%FNAME,'_',.TRUE.)
   JJ=INDEX(TPIDF%FNAME(J+1:),'-'); JJ=JJ+J
   FTXT=TPIDF%FNAME(:JJ-1); VAIDF%FNAME=TRIM(FTXT)//'-VA-S.IDF' 
  ENDIF
  IF(.NOT.IDFWRITE(VAIDF,VAIDF%FNAME,1,1))THEN; WRITE(*,'(/A/)') 'ERROR WRITING '//TRIM(VAIDF%FNAME); STOP; ENDIF  
 ELSE
  WRITE(VAIDF%FNAME,*) '  ',K
 ENDIF
 
 END SUBROUTINE LHM_CONVERTREGIS_OUTPUT
 
 !###======================================================
 SUBROUTINE LHM_CONVERTREGIS_AGGREGATE()
 !###======================================================
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
  
  IF(.NOT.IDFREAD(TP(I,1),FNAME,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF 
  CALL IDFCOPY(TP(I,1),TP(I,2))
  IF(.NOT.IDFWRITE(TP(I,2),FNAME,1,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF
  
  IF(.NOT.IDFREAD(KH(I,1),FNAME,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF  
  CALL IDFCOPY(KH(I,1),KH(I,2))
  IF(.NOT.IDFWRITE(KH(I,2),FNAME,1,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF

  IF(.NOT.IDFREAD(KV(I,1),FNAME,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF
  CALL IDFCOPY(KV(I,1),KV(I,2))
  IF(.NOT.IDFWRITE(KV(I,2),FNAME,1,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF
 
  IF(I.EQ.N)THEN
   IF(.NOT.IDFREAD(TP(I+1,1),FNAME,-1))THEN; WRITE(*,'(/A/)') 'ERROR READING '//TRIM(FNAME); STOP; ENDIF
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

 !###======================================================
 SUBROUTINE LHM_CONVERTREGIS_WRITEPRJ()
 !###======================================================
 IMPLICIT NONE
 INTEGER :: I,IU,N
 
 N=SIZE(FLIST)
 CALL UTL_CREATEDIR(OUTPUTDIR)
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(OUTPUTDIR)//'\MODEL_v5.PRJ',STATUS='UNKNOWN',ACTION='WRITE')

 WRITE(IU,'(/A)') '0001,(BND),1, BOUNDARY CONDITION,[BND]'
 WRITE(IU,'(A,I10)') '001,',N
 DO I = 1,N
  FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-IB_L'//TRIM(ITOS(I))//'.IDF'
  WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
 ENDDO
 
 WRITE(IU,'(/A)') '0001,(TOP),1, TOP ELEVATION,[TOP]'
 WRITE(IU,'(A,I10)') '001,',N
 DO I = 1,N
  FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-T_L'//TRIM(ITOS(I))//'.IDF'
  WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
 ENDDO
 
 WRITE(IU,'(/A)') '0001,(BOT),1, BOTTOM ELEVATION,[BOT]'
 WRITE(IU,'(A,I10)') '001,',N
 DO I = 1,N
  IF(I.EQ.N)THEN
   FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-B_L'//TRIM(ITOS(I))//'.IDF'
   WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
  ELSE
   FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-T_L'//TRIM(ITOS(I+1))//'.IDF'
   WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
  ENDIF
 ENDDO

 WRITE(IU,'(/A)') '0001,(KHV),1, HORIZONTAL PERMEABILITY,[KHV]'
 WRITE(IU,'(A,I10)') '001,',N
 DO I = 1,N
  FNAME = TRIM(OUTPUTDIR)//'\'//TRIM(FNAME)//'-KH_L'//TRIM(ITOS(I))//'.IDF'
  WRITE(IU,'(A,I3,A)') '1,2,',I,',1.0,0.0,-999.99,'//TRIM(FNAME)
 ENDDO

 WRITE(IU,'(/A)') '0001,(KVA),1, VERTICAL ANISOTROPY,[KVA]'
 WRITE(IU,'(A,I10)') '001,',N
 
 DO I = 1,N
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