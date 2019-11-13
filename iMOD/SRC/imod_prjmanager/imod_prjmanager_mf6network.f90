MODULE MOD_PMANAGER_MF6NETWORK

USE WINTERACTER
USE RESOURCE
USE MOD_ASC2IDF_HFB
USE MOD_ASC2IDF_PAR
USE MOD_ASC2IDF_UTL
USE MOD_POLYGON_UTL
USE MOD_UTL
USE MOD_IDF

CONTAINS

 !###======================================================================
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS(GENFNAME,OUTFOLDER,NSUBMODEL)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: GENFNAME,OUTFOLDER
 INTEGER,INTENT(OUT) :: NSUBMODEL
 INTEGER,ALLOCATABLE,DIMENSION(:) :: ISORT,IPOL
 INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:,:) :: IPC
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: MF6IDF
 INTEGER :: I,J,IU,JU,II,JJ

 !## origin file
 CALL POLYGON1SAVELOADSHAPE(ID_LOADSHAPE,TRIM(GENFNAME),'GEN')
 IF(SHP%NPOL.LE.0)THEN; WRITE(*,'(/A/)') 'No polygons found in GEN file'; RETURN; ENDIF
 !## this file contains a GEN file projected on the correct coordinate system of the submodels
 CALL UTL_CREATEDIR(OUTFOLDER); IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(OUTFOLDER)//'\BND.GEN',ACTION='WRITE',STATUS='UNKNOWN',FORM='FORMATTED')
 IF(IU.EQ.0)THEN; WRITE(*,'(A)') 'Error opening '//TRIM(OUTFOLDER)//'\BND.GEN'; RETURN; ENDIF
 !## this file contains the sub-division of submodel as "faults"
 JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=TRIM(OUTFOLDER)//'\BND.XY',ACTION='WRITE',STATUS='UNKNOWN',FORM='FORMATTED')
 IF(JU.EQ.0)THEN; WRITE(*,'(A)') 'Error opening '//TRIM(OUTFOLDER)//'\BND.XY'; RETURN; ENDIF

 ALLOCATE(ISORT(SHP%NPOL),IPOL(SHP%NPOL)); DO I=1,SHP%NPOL; ISORT(I)=I; IPOL(I)=0; ENDDO
 CALL PMANAGER_GENERATEMFNETWORKS_SORT(SHP%NPOL,ISORT,IPOL)
 ALLOCATE(MF6IDF(SHP%NPOL)); DO I=1,SHP%NPOL; CALL IDFNULLIFY(MF6IDF(I)); ENDDO 
 
 !## get dimensions of the idf files - read them from large cellsizes up to small cell sizes
 !## MF6IDF(1) is biggest; MF6IDF(n) is smallest
 CALL PMANAGER_GENERATEMFNETWORKS_DIMIDF(MF6IDF,ISORT,IPOL)
 
 !## process from big to small ... determine in what polygon the selected polygon is and take that cellsize to generate the boundary polygon
 DO I=1,SIZE(MF6IDF)

  !## current idf/polygon in sort list
  J =ISORT(I)
  !## use cellsize of idf which overlays the current idf
  II=IPOL(J)
  
  CALL ASC2IDF_INT_NULLIFY(); ALLOCATE(XP(100),YP(100),ZP(100),WP(100),FP(100))
  ALLOCATE(IPC(MF6IDF(II)%NCOL,MF6IDF(II)%NROW,2)); IPC=INT(0,1)
  
  !## intersect line and determine ipc()
  CALL ASC2IDF_HFB(MF6IDF(II),MF6IDF(II)%NROW,MF6IDF(II)%NCOL,IPC,(/GENFNAME/),-1,IPOL=J)
  !## write genfiles
  CALL PMANAGER_GENERATEMFNETWORKS_WRITEGEN(MF6IDF(II),IPC,IU,JU,J)
  CALL ASC2IDF_INT_DEALLOCATE(); DEALLOCATE(IPC)
 ENDDO
 WRITE(IU,'(A)') 'END'; CLOSE(IU); CLOSE(JU)

 !## sortgen files to be a polygon, blank out regions in the idf files
 CALL PMANAGER_GENERATEMFNETWORKS_CREATEPOLYGONS(OUTFOLDER,MF6IDF,ISORT)
 
 !## write idf files
 DO I=1,SHP%NPOL
  J=ISORT(I)
  !## first model was a bit too big for the hfb simulations
  IF(J.EQ.1)THEN
   MF6IDF(I)%XMIN=MF6IDF(I)%XMIN+MF6IDF(I)%DX; MF6IDF(I)%XMAX=MF6IDF(I)%XMAX-MF6IDF(I)%DX
   MF6IDF(I)%YMIN=MF6IDF(I)%YMIN+MF6IDF(I)%DY; MF6IDF(I)%YMAX=MF6IDF(I)%YMAX-MF6IDF(I)%DY
   MF6IDF(I)%NCOL=MF6IDF(I)%NCOL-2; MF6IDF(I)%NROW=MF6IDF(I)%NROW-2
   DO II=1,MF6IDF(I)%NROW; DO JJ=1,MF6IDF(I)%NCOL
    MF6IDF(I)%X(JJ,II)=MF6IDF(I)%X(JJ+1,II+1)
   ENDDO; ENDDO
  ENDIF
  MF6IDF(I)%FNAME=TRIM(OUTFOLDER)//'\SUBMODEL_'//TRIM(ITOS(I))//'.IDF'
  IF(.NOT.IDFWRITE(MF6IDF(I),MF6IDF(I)%FNAME,1))STOP
 ENDDO
 
 CALL IDFDEALLOCATE(MF6IDF,SIZE(MF6IDF)); DEALLOCATE(MF6IDF)
 
 DEALLOCATE(ISORT,IPOL)
 
 NSUBMODEL=SHP%NPOL

 END SUBROUTINE PMANAGER_GENERATEMFNETWORKS
 
 !###======================================================================
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS_CREATEPOLYGONS(OUTFOLDER,MF6IDF,ISORT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: OUTFOLDER
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: MF6IDF
 INTEGER,INTENT(IN),DIMENSION(:) :: ISORT
 CHARACTER(LEN=256) :: FNAME
 INTEGER :: IOS,I,J,N,II,JJ,IU,JU,KU,IROW,ICOL,JROW,JCOL
 REAL(KIND=DP_KIND) :: X1,Y1,X2,Y2
 REAL(KIND=DP_KIND),DIMENSION(:,:),POINTER :: XC,YC,XC_TMP,YC_TMP
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IS,JS,ID

 !## open file with "faults" per submodel - need to be "puzzled" to a single feature
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(OUTFOLDER)//'\BND.XY'             ,ACTION='READ' ,STATUS='OLD'    ,FORM='FORMATTED')
 !## results is a GEN per polygon
 JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=TRIM(OUTFOLDER)//'\SUBMODELS_ASCII.GEN',ACTION='WRITE',STATUS='UNKNOWN',FORM='FORMATTED')
 KU=UTL_GETUNIT(); CALL OSD_OPEN(KU,FILE=TRIM(OUTFOLDER)//'\SUBMODELS_ASCII.DAT',ACTION='WRITE',STATUS='UNKNOWN',FORM='FORMATTED')
 WRITE(KU,'(A)') 'ID,CELLSIZE'
 DO I=1,SHP%NPOL
  J=ISORT(I)
  WRITE(KU,'(A)') TRIM(ITOS(J))//','//TRIM(RTOS(MF6IDF(J)%DX,'F',3))
 ENDDO
 CLOSE(KU)

 ALLOCATE(XC(100,2),YC(100,2))
 
 J=1; N=0; DO
  READ(IU,*,IOSTAT=IOS) I,X1,Y1,X2,Y2
  IF(IOS.NE.0)I=I+1
  !## start tracking the connected lines on this polygon
  IF(I.NE.J)THEN
   ALLOCATE(IS(N),JS(N),ID(N))
   CALL PMANAGER_GENERATEMFNETWORKS_PUZZLE(XC,YC,IS,JS,ID,N)
   WRITE(JU,'(I10)') J
   JJ=0; DO II=1,N
    WRITE(JU,'(2(F15.3,A1))') XC(IS(II),1),',',YC(IS(II),1)
    JJ=II
   ENDDO
   WRITE(JU,'(2(F15.3,A1))') XC(IS(JJ),2),',',YC(IS(JJ),2)
   WRITE(JU,'(A3)') 'END'; DEALLOCATE(IS,JS,ID)
   IF(IOS.NE.0)EXIT; J=I; N=0
  ENDIF
  IF(N+1.GT.SIZE(XC,1))THEN
   ALLOCATE(XC_TMP(N+100,2),YC_TMP(N+100,2))
   DO II=1,N; DO JJ=1,2; XC_TMP(II,JJ)=XC(II,JJ); YC_TMP(II,JJ)=YC(II,JJ); ENDDO; ENDDO
   DEALLOCATE(XC,YC); XC=>XC_TMP; YC=>YC_TMP
  ENDIF
  N=N+1; XC(N,1)=X1; YC(N,1)=Y1; XC(N,2)=X2; YC(N,2)=Y2
 ENDDO
 
 WRITE(JU,'(A3)') 'END'; CLOSE(IU,STATUS='DELETE'); CLOSE(JU)

 !## convert ascii to binary genfile
 FNAME=TRIM(OUTFOLDER)//'\SUBMODELS_ASCII.GEN'
 CALL POLYGON_UTL_CONVERTGEN(FNAME=FNAME,OFNAME=TRIM(OUTFOLDER)//'\SUBMODELS_BINAIR.GEN')
 
 !## read binary genfile
 CALL POLYGON1SAVELOADSHAPE(ID_LOADSHAPE,TRIM(OUTFOLDER)//'\SUBMODELS_BINAIR.GEN','GEN')
 
 DO I=SHP%NPOL,1,-1
  IF(SHP%POL(I)%ITYPE.NE.ID_POLYGON)THEN
   WRITE(*,'(/1X,A/)') 'The generated shape need to be a POLYGON'
   SELECT CASE (SHP%POL(I)%ITYPE)
    CASE (ID_LINE);      WRITE(*,'(1X,A)') 'iMOD found a LINE now'
    CASE (ID_POINT);     WRITE(*,'(1X,A)') 'iMOD found a POINT now'
    CASE (ID_CIRCLE);    WRITE(*,'(1X,A)') 'iMOD found a CIRCLE now'
    CASE (ID_RECTANGLE); WRITE(*,'(1X,A)') 'iMOD found a RECTANGLE now'
   END SELECT
   STOP
  ENDIF
 ENDDO
 
 !## start with smallest
 DO I=1,SIZE(IDF); MF6IDF(I)%X=1.0D0; ENDDO
 
 DO I=SHP%NPOL,1,-1
  J=ISORT(I)
  DO IROW=1,MF6IDF(J)%NROW; DO ICOL=1,MF6IDF(J)%NCOL
   !## skip blanked out areas
   IF(MF6IDF(J)%X(ICOL,IROW).EQ.MF6IDF(J)%NODATA)CYCLE 
   CALL IDFGETLOC(MF6IDF(J),IROW,ICOL,X1,Y1)
   IF(DBL_IGRINSIDESHAPE(X1,Y1,SHP%POL(I)).EQ.1)THEN
    MF6IDF(J)%X(ICOL,IROW)=1.0D0
    !## deactivate others on this location
    DO II=I-1,1,-1 
     JJ=ISORT(II)
     IF(DBL_IGRINSIDESHAPE(X1,Y1,SHP%POL(II)).EQ.1)THEN
      CALL IDFIROWICOL(MF6IDF(JJ),JROW,JCOL,X1,Y1)
      MF6IDF(JJ)%X(JCOL,JROW)=MF6IDF(JJ)%NODATA 
     ENDIF
    ENDDO
   ELSE
    MF6IDF(J)%X(ICOL,IROW)=MF6IDF(J)%NODATA
   ENDIF
  ENDDO; ENDDO
 ENDDO
 
 END SUBROUTINE PMANAGER_GENERATEMFNETWORKS_CREATEPOLYGONS
 
 !###======================================================================
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS_PUZZLE(XC,YC,IS,JS,ID,N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: N
 INTEGER,DIMENSION(:),INTENT(OUT) :: IS,JS,ID
 REAL(KIND=DP_KIND),DIMENSION(:,:),INTENT(INOUT) :: XC,YC
 INTEGER :: NS,M,I,J,IPOS,ND,IDIR
 
 DO I=1,N; JS(I)=I; ENDDO
 
 !## identify dangles - they need to be removed first
 DO
  ND=0
  DO I=1,N
   !## already identified as dangle
   IF(JS(I).EQ.-1)CYCLE
   DO IPOS=1,2
    M=0; DO J=1,N
     IF(I.EQ.J)CYCLE
     IF(PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT(XC,YC,IPOS,I,J,IDIR))M=M+1
    ENDDO
    IF(M.EQ.0)THEN
     !## skip this one
     JS(I)=-1; ND=1
    ENDIF
   ENDDO
  ENDDO
  !# dangles found anymore
  IF(ND.EQ.0)EXIT
 ENDDO
 
 !## reshuffle list of point without the dangles
 J=0; DO I=1,N
  IF(JS(I).NE.-1)THEN
   J=J+1; IS(J)=JS(I)
  ENDIF
 ENDDO
 
 !## new number of point - excluding the dangles
 N=J; DO I=1,N; JS(I)=IS(I); ENDDO
 
 !## all are no passed by yet
 ID=0
 !## start a first location
 NS=1; JS(NS)=1; NS=NS+1
 
 !## "walk" along the line, keep track of the direction
 DO
  !## find next point to move towards
  DO I=2,N
!   !## already passed by in two directions
!   IF(JS(I).EQ.2)CYCLE
   !## find appropriate segment connected
   IF(PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT(XC,YC,2,IS(NS),JS(I),IDIR))THEN
    !## already moved in this direction, try another point
    IF(ID(I).EQ.IDIR)CYCLE
    NS=NS+1; IS(NS)=JS(I); ID(I)=IDIR; EXIT
   ENDIF
  ENDDO
 
 ENDDO
 
! !## start at first non-dangle
! IS=0; DO I=1,N; IF(JS(I).NE.-1)THEN; IS(1)=I; EXIT; ENDIF; ENDDO
! NS=1; JS(I)=0

! DO
!  DO I=1,N
!   !## already used   
!   IF(JS(I).LE.0)CYCLE
!   IF(PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT(XC,YC,2,IS(NS),JS(I)))THEN
!    NS=NS+1; IS(NS)=JS(I); JS(I)=0; EXIT
!   ENDIF
!  ENDDO
!  DO I=1,N; IF(JS(I).GT.0)EXIT; ENDDO
!  IF(I.GT.N)EXIT 
! ENDDO
 
! !## reduce number
! DO I=1,N; IF(IS(I).EQ.0)EXIT; ENDDO; N=I-1
 
 END SUBROUTINE PMANAGER_GENERATEMFNETWORKS_PUZZLE
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT(XC,YC,IPOS,IS,JS,IDIR)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),DIMENSION(:,:),INTENT(INOUT) :: XC,YC
 INTEGER,INTENT(IN) :: IS,JS,IPOS
 INTEGER,INTENT(OUT) :: IDIR
 REAL(KIND=DP_KIND) :: X0,Y0,X1,Y1,X2,Y2
 
 PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT=.FALSE.
 
 X0=XC(IS,IPOS); Y0=YC(IS,IPOS)
 X1=XC(JS,1);    Y1=YC(JS,1)
 X2=XC(JS,2);    Y2=YC(JS,2)

 IF(UTL_DIST(X0,Y0,X1,Y1).LE.1.0D-3)THEN
  IDIR= 1; PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT=.TRUE.; RETURN
 !## connected inversely - switch coordinates
 ELSEIF(UTL_DIST(X0,Y0,X2,Y2).LE.1.0D-3)THEN
  XC(JS,1)=X2; YC(JS,1)=Y2; XC(JS,2)=X1; YC(JS,2)=Y1
  IDIR=-1; PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT=.TRUE.; RETURN
 ENDIF
 
 END FUNCTION PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT

 !###======================================================================
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS_DIMIDF(MF6IDF,ISORT,IPOL)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: MF6IDF
 INTEGER,INTENT(IN),DIMENSION(:) :: ISORT,IPOL
 INTEGER :: I,II,JJ,MAXCOL,IOS
 REAL(KIND=DP_KIND) :: CS
 TYPE STROBJ
  CHARACTER(LEN=52) :: STRING
 END TYPE
 TYPE(STROBJ),ALLOCATABLE,DIMENSION(:) :: STR
 
 MAXCOL=0; IF(ASSOCIATED(SHP%COLNAMES))MAXCOL=SIZE(SHP%COLNAMES); ALLOCATE(STR(MAXCOL))
 
 DO I=1,SHP%NPOL
  
  MF6IDF(I)%XMIN=SHP%POL(I)%XMIN; MF6IDF(I)%YMIN=SHP%POL(I)%YMIN
  MF6IDF(I)%XMAX=SHP%POL(I)%XMAX; MF6IDF(I)%YMAX=SHP%POL(I)%YMAX

  DO II=1,MAXCOL
   STR(II)%STRING=''; DO JJ=1,SHP%LWIDTH(II); STR(II)%STRING(JJ:JJ)=SHP%POL(I)%LBL(II)%STRING(JJ); ENDDO
  ENDDO

  !## read cellsize in polygon
  READ(STR(2)%STRING,*,IOSTAT=IOS) MF6IDF(I)%DX
  IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'Cannot read cellsize for polygon #',I; STOP; ENDIF
  MF6IDF(I)%DY=MF6IDF(I)%DX
 
 ENDDO
 
 !## set cell sizes to rasterize along
 DO I=1,SHP%NPOL

  CS=MF6IDF(IPOL(I))%DX
  !## find nice coordinates
  CALL UTL_IDFSNAPTOGRID(MF6IDF(I)%XMIN,MF6IDF(I)%XMAX,MF6IDF(I)%YMIN,MF6IDF(I)%YMAX,CS,MF6IDF(I)%NCOL,MF6IDF(I)%NROW)

  !## increase biggest model as "faults" won't capture the area
  IF(ISORT(I).EQ.1)THEN
   MF6IDF(I)%XMIN=MF6IDF(I)%XMIN-MF6IDF(I)%DX; MF6IDF(I)%XMAX=MF6IDF(I)%XMAX+MF6IDF(I)%DX
   MF6IDF(I)%YMIN=MF6IDF(I)%YMIN-MF6IDF(I)%DY; MF6IDF(I)%YMAX=MF6IDF(I)%YMAX+MF6IDF(I)%DY
  ENDIF

  !## get the right dimensions
  MF6IDF(I)%NCOL=INT((MF6IDF(I)%XMAX-MF6IDF(I)%XMIN)/MF6IDF(I)%DX)
  MF6IDF(I)%NROW=INT((MF6IDF(I)%YMAX-MF6IDF(I)%YMIN)/MF6IDF(I)%DY)
  IF(.NOT.IDFALLOCATEX(MF6IDF(I)))THEN; WRITE(*,'(/A/)') 'Cannot allocate memory idf%x() #',I;               STOP; ENDIF
  IF(.NOT.IDFFILLSXSY(MF6IDF(I)))THEN;  WRITE(*,'(/A/)') 'Cannot allocate memory for idf%sx()/idf%sy() #',I; STOP; ENDIF

 ENDDO

 DEALLOCATE(STR)

 END SUBROUTINE PMANAGER_GENERATEMFNETWORKS_DIMIDF
 
 !###======================================================================
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS_WRITEGEN(MF6IDF,IPC,IU,JU,N)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: MF6IDF
 INTEGER(KIND=1),DIMENSION(:,:,:),INTENT(IN) :: IPC
 INTEGER,INTENT(IN) :: IU,JU,N
 INTEGER :: IROW,ICOL
 
 DO IROW=1,MF6IDF%NROW; DO ICOL=1,MF6IDF%NCOL
 
  !## place vertical wall
  IF(IPC(ICOL,IROW,1).EQ.INT(1,1))THEN
   CALL PMANAGER_GENERATEMFNETWORKS_WRITEXY(1,IU,JU,IPC,MF6IDF,IROW,ICOL,N,0,0.0D0,0.0D0) 
  ENDIF
 
  !## place horizontal wall
  IF(IPC(ICOL,IROW,2).EQ.INT(1,1))THEN
   !## write line in genfile
   CALL PMANAGER_GENERATEMFNETWORKS_WRITEXY(2,IU,JU,IPC,MF6IDF,IROW,ICOL,N,0,0.0D0,0.0D0) 
  ENDIF
 
 ENDDO; ENDDO
 
 END SUBROUTINE PMANAGER_GENERATEMFNETWORKS_WRITEGEN
 
 !###====================================================================
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS_WRITEXY(IT,IU,JU,IPC,MF6IDF,IROW,ICOL,N,I3D,T,B)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: MF6IDF
 INTEGER,INTENT(IN) :: IROW,ICOL,IU,JU,N,IT,I3D
 INTEGER(KIND=1),INTENT(IN),DIMENSION(:,:,:) :: IPC 
 REAL(KIND=DP_KIND),INTENT(IN) :: T,B

 IF(I3D.EQ.0)THEN

  !## place vertical wall
  IF(IT.EQ.1)THEN
   IF(IPC(ICOL,IROW,1).EQ.INT(1,1))THEN
    WRITE(IU,'(I10)') N
    WRITE(IU,'(2(F15.3,A1))') MF6IDF%SX(ICOL),',',MF6IDF%SY(IROW-1)
    WRITE(IU,'(2(F15.3,A1))') MF6IDF%SX(ICOL),',',MF6IDF%SY(IROW)
    WRITE(IU,'(A)') 'END'
    IF(JU.GT.0)WRITE(JU,'(I10,4(A1,F15.3))') N,',',MF6IDF%SX(ICOL),',',MF6IDF%SY(IROW-1),',',MF6IDF%SX(ICOL),',',MF6IDF%SY(IROW)
   ENDIF
  ENDIF
 
  !## place horizontal wall
  IF(IT.EQ.2)THEN
   IF(IPC(ICOL,IROW,2).EQ.INT(1,1))THEN
    WRITE(IU,'(I10)') N  
    WRITE(IU,'(2(F15.3,A1))') MF6IDF%SX(ICOL-1),',',MF6IDF%SY(IROW)
    WRITE(IU,'(2(F15.3,A1))') MF6IDF%SX(ICOL  ),',',MF6IDF%SY(IROW)
    WRITE(IU,'(A)') 'END'
    IF(JU.GT.0)WRITE(JU,'(I10,4(A1,F15.3))') N,',',MF6IDF%SX(ICOL-1),',',MF6IDF%SY(IROW),',',MF6IDF%SX(ICOL  ),',',MF6IDF%SY(IROW)
   ENDIF
  ENDIF

 ELSE

  !## place vertical wall
  IF(IT.EQ.1)THEN
   IF(IPC(ICOL,IROW,1).EQ.INT(1,1))THEN
    WRITE(IU,'(I10)') N
    WRITE(IU,'(3(F15.3,A1))') MF6IDF%SX(ICOL),',',MF6IDF%SY(IROW-1),',',T
    WRITE(IU,'(3(F15.3,A1))') MF6IDF%SX(ICOL),',',MF6IDF%SY(IROW)  ,',',T
    WRITE(IU,'(3(F15.3,A1))') MF6IDF%SX(ICOL),',',MF6IDF%SY(IROW)  ,',',B
    WRITE(IU,'(3(F15.3,A1))') MF6IDF%SX(ICOL),',',MF6IDF%SY(IROW-1),',',B
    WRITE(IU,'(3(F15.3,A1))') MF6IDF%SX(ICOL),',',MF6IDF%SY(IROW-1),',',T
    WRITE(IU,'(A)') 'END'
   ENDIF
  ENDIF
 
  !## place horizontal wall
  IF(IT.EQ.2)THEN
   IF(IPC(ICOL,IROW,2).EQ.INT(1,1))THEN
    WRITE(IU,'(I10)') N  
    WRITE(IU,'(3(F15.3,A1))') MF6IDF%SX(ICOL-1),',',MF6IDF%SY(IROW),',',T
    WRITE(IU,'(3(F15.3,A1))') MF6IDF%SX(ICOL  ),',',MF6IDF%SY(IROW),',',T
    WRITE(IU,'(3(F15.3,A1))') MF6IDF%SX(ICOL  ),',',MF6IDF%SY(IROW),',',B
    WRITE(IU,'(3(F15.3,A1))') MF6IDF%SX(ICOL-1),',',MF6IDF%SY(IROW),',',B
    WRITE(IU,'(3(F15.3,A1))') MF6IDF%SX(ICOL-1),',',MF6IDF%SY(IROW),',',T
    WRITE(IU,'(A)') 'END'
   ENDIF
  ENDIF

 ENDIF
 
 END SUBROUTINE PMANAGER_GENERATEMFNETWORKS_WRITEXY
 
 !###======================================================================
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS_SORT(N,ISORT,IPOL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT),DIMENSION(N) :: ISORT,IPOL
 INTEGER :: I,J
 REAL(KIND=DP_KIND) :: X,Y,AREA
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: POLAREA
 
 ALLOCATE(POLAREA(SHP%NPOL)); POLAREA=0.0D0
 
 DO I=1,SHP%NPOL
  IF(SHP%POL(I)%N.GT.0)THEN
   SELECT CASE (SHP%POL(I)%ITYPE)
    CASE (ID_RECTANGLE)
     POLAREA(I)=ABS(SHP%POL(I)%X(1)-SHP%POL(I)%X(2))*ABS(SHP%POL(I)%Y(1)-SHP%POL(I)%Y(2))
    CASE (ID_POLYGON)
     POLAREA(I)=ABS(UTL_POLYGON1AREA(SHP%POL(I)%X,SHP%POL(I)%Y,SHP%POL(I)%N))
    CASE (ID_CIRCLE)
     POLAREA(I)=2.0D0*UTL_DIST(SHP%POL(I)%X(1),SHP%POL(I)%Y(1),SHP%POL(I)%X(2),SHP%POL(I)%Y(2))
     POLAREA(I)=PI*POLAREA(I)**2.0D0
    CASE DEFAULT
     POLAREA(SHPI)=0.0D0
   END SELECT
  ENDIF
 END DO
 
 CALL WSORT(POLAREA,1,SHP%NPOL,IFLAGS=SORTDESCEND,IORDER=ISORT)

 !## define for each what shape will be the that captures them - smallest area
 DO J=1,SHP%NPOL
  IPOL(J)=J; AREA=HUGE(1.0D0); X=SHP%POL(J)%X(1); Y=SHP%POL(J)%Y(1)
  DO I=1,SHP%NPOL
   IF(I.EQ.J)CYCLE
   IF(DBL_IGRINSIDESHAPE(X,Y,SHP%POL(I)).EQ.1)THEN
    IF(POLAREA(I).LE.AREA)THEN
     AREA=POLAREA(I)
     IPOL(J)=I
    ENDIF
   ENDIF
  ENDDO
 ENDDO
 
 DEALLOCATE(POLAREA)

 END SUBROUTINE PMANAGER_GENERATEMFNETWORKS_SORT

 END MODULE MOD_PMANAGER_MF6NETWORK