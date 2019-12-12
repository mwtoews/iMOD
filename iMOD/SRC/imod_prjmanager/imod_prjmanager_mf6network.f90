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
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS(GENFNAME,OUTFOLDER,NSUBMODEL,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
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
 CALL PMANAGER_GENERATEMFNETWORKS_DIMIDF(MF6IDF,ISORT,IPOL,IBATCH)
 
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
 REAL(KIND=DP_KIND),DIMENSION(:,:),POINTER :: XC=>NULL(),YC=>NULL(),XC_TMP=>NULL(),YC_TMP=>NULL()
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
   ALLOCATE(IS(N*2),JS(N*2),ID(N*2))
   CALL PMANAGER_GENERATEMFNETWORKS_PUZZLE(XC,YC,IS,JS,ID,N,JU)
   DEALLOCATE(IS,JS,ID); IF(IOS.NE.0)EXIT; J=I; N=0
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
 DO I=1,SIZE(MF6IDF); MF6IDF(I)%X=1.0D0; ENDDO
 
 DO I=SHP%NPOL,1,-1
  J=ISORT(I)
  DO IROW=1,MF6IDF(J)%NROW; DO ICOL=1,MF6IDF(J)%NCOL
   !## skip blanked out areas
   IF(MF6IDF(J)%X(ICOL,IROW).EQ.MF6IDF(J)%NODATA)CYCLE
   CALL IDFGETLOC(MF6IDF(J),IROW,ICOL,X1,Y1)
   IF(DBL_IGRINSIDESHAPE(X1,Y1,SHP%POL(I)).EQ.1)THEN
!   IF(TMP_UTL_INSIDEPOLYGON2(X1,Y1,SHP%POL(I)%X,SHP%POL(I)%Y,SHP%POL(I)%N,99).EQ.1)THEN
    MF6IDF(J)%X(ICOL,IROW)=1.0D0
    !## deactivate others on this location
    DO II=I-1,1,-1 
     JJ=ISORT(II)
!      IF(TMP_UTL_INSIDEPOLYGON2(X1,Y1,SHP%POL(II)%X,SHP%POL(II)%Y,SHP%POL(II)%N,99).EQ.1)THEN
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
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS_PUZZLE(XC,YC,IS,JS,ID,N,IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: N
 INTEGER,INTENT(IN) :: IU
 INTEGER,DIMENSION(:),INTENT(OUT) :: IS,JS,ID
 REAL(KIND=DP_KIND),DIMENSION(:,:),INTENT(INOUT) :: XC,YC
 INTEGER :: NS,M,I,II,J,IPOS,ND,IDIR,IPOL
 
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
 
 !## all are no passed by yet, except first, set as minus 1
 ID=0; ID(1)=-1
 !## start at first location
 NS=1; JS(NS)=IS(1)
 
 !## puzzle pieces into multiple polygons
 DO 
  !## "walk" along the line, keep track of the direction; don't walk a line in the same direction
  IPOL=0; I=1
  DO
   !## find next point to move towards
   I=I+1; IF(I.GT.N)I=1
   
   !## skip points that are already processed
   IF(ID(I).EQ.-999)CYCLE
   
   !## find appropriate segment connected (swaps the coordinates as well)
   IF(PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT(XC,YC,2,IS(NS),JS(I),IDIR))THEN
    !## back to the beginning (of number of polygons)
    IF(ID(I).EQ.-1)EXIT
    !## already moved in this direction, try another point
    IF(ID(I).NE.IDIR)THEN
     !## correct location to proceed, set id()=1
     NS=NS+1; IS(NS)=JS(I); ID(I)=1
    ENDIF
   ENDIF
  ENDDO

  !## write polygon
  IPOL=IPOL+1; WRITE(IU,'(I10)') IPOL
  !## start with first point
  WRITE(IU,'(2(F15.3,A1))') XC(IS(1),1),',',YC(IS(1),1)
  DO II=2,NS
   !## see the correct connection - can be swapped as line if twice passed
   IF(PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT(XC,YC,2,IS(II-1),IS(II),IDIR))THEN
    WRITE(IU,'(2(F15.3,A1))') XC(IS(II),1),',',YC(IS(II),1)
   ENDIF
  ENDDO
  WRITE(IU,'(2(F15.3,A1))') XC(IS(1),1),',',YC(IS(1),1)
  WRITE(IU,'(A3)') 'END'
 
  !## see if there are "polygons" left
  NS=0; DO I=1,N
   IF(ID(I).NE.0)THEN
    ID(I)=-999
   ELSE
    !## set new start position of potential next polygon
    NS=NS+1; IF(NS.EQ.1)THEN; ID(I)=-1; II=I; ENDIF
   ENDIF
  ENDDO
  
  !## nothing to do anymore
  IF(NS.LE.2)EXIT
  
  !## start again
  NS=1; I=II; JS(NS)=IS(I)
  
 ENDDO
 
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
  IDIR=1; PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT=.TRUE.; RETURN
 !## connected inversely - switch coordinates
 ELSEIF(UTL_DIST(X0,Y0,X2,Y2).LE.1.0D-3)THEN
  XC(JS,1)=X2; YC(JS,1)=Y2; XC(JS,2)=X1; YC(JS,2)=Y1
  IDIR=-1; PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT=.TRUE.; RETURN
 ENDIF
 
 END FUNCTION PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT

 !###======================================================================
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS_DIMIDF(MF6IDF,ISORT,IPOL,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: MF6IDF
 INTEGER,INTENT(IN),DIMENSION(:) :: ISORT,IPOL
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: I,J,II,JJ,MAXCOL,IOS
 REAL(KIND=DP_KIND) :: CS,F
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

 !## check correct multiplications - they all need to do that
 DO II=2,SIZE(MF6IDF)
  I=ISORT(II-1)
  J=ISORT(II)
  F=MF6IDF(I)%DX/MF6IDF(J)%DX
  IF(MOD(MF6IDF(I)%DX,F).NE.0.0D0)THEN
   IF(IBATCH.EQ.1)THEN
    WRITE(*,'(/A/)') 'It is adviced to keep cellsizes as integer multiplications.'// &
    'Submodel '//TRIM(ITOS(I))//' has a cellsize of '//TRIM(RTOS(MF6IDF(I)%DX,'F',3))//' which is not an '// &
    'integer multiplication of submodel '//TRIM(ITOS(J))//' which has a cellsize of '//TRIM(RTOS(MF6IDF(J)%DX,'F',3))//'. '// &
    'Proceeding with this may yield inaccurate results.'
   ELSE
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'It is adviced to keep cellsizes as integer multiplications.'//CHAR(13)// &
    'Submodel '//TRIM(ITOS(I))//' has a cellsize of '//TRIM(RTOS(MF6IDF(I)%DX,'F',3))//' which is not an'//CHAR(13)// &
    'integer multiplication of submodel '//TRIM(ITOS(J))//' which has a cellsize of '//TRIM(RTOS(MF6IDF(J)%DX,'F',3))//'.'//CHAR(13)// &
    'Proceeding with this may yield inaccurate results.','Warning')
   ENDIF
  ENDIF
 ENDDO
 
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

 !## define for each what shape will be the one that captures them - smallest area
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

! !###======================================================================
! INTEGER FUNCTION TMP_UTL_INSIDEPOLYGON2(PX,PY,XX,YY,N,iu)
! !###======================================================================
! !
! !code extracted from the internet:
! !http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html#Fortran%20Code%20for%20the%20Point%20in%20Polygon%20Test
! !
! !    PURPOSE TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON
! !
! !    DESCRIPTION OF THE PARAMETERS
! !       PX      - X-COORDINATE OF POINT IN QUESTION.
! !       PY      - Y-COORDINATE OF POINT IN QUESTION.
! !       XX      - N LONG VECTOR CONTAINING X-COORDINATES OF
! !                 VERTICES OF POLYGON.
! !       YY      - N LONG VECTOR CONTAING Y-COORDINATES OF
! !                 VERTICES OF POLYGON.
! !       N       - NUMBER OF VERTICES IN THE POLYGON.
! !       UTL_INSIDEPOLYGON   - THE SIGNAL RETURNED:
! !                 -1 IF THE POINT IS OUTSIDE OF THE POLYGON,
! !                  0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,
! !                  1 IF THE POINT IS INSIDE OF THE POLYGON.
!
! !    REMARKS
! !       THE VERTICES MAY BE LISTED CLOCKWISE OR ANTICLOCKWISE.
! !       THE FIRST MAY OPTIONALLY BE REPEATED, IF SO N MAY
! !       OPTIONALLY BE INCREASED BY 1.
! !       THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING
! !       OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX
! !       OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING
! !       N, THESE FIRST VERTICES MUST BE COUNTED TWICE.
! !       INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.
! !       THE SIZE OF THE ARRAYS MUST BE INCREASED IF N > MAXDIM
! !       WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 7/70.
! !
! !
! !    METHOD
! !       A VERTICAL LINE IS DRAWN THRU THE POINT IN QUESTION. IF IT
! !       CROSSES THE POLYGON AN ODD NUMBER OF TIMES, THEN THE
! !       POINT IS INSIDE OF THE POLYGON.
! !
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: N,iu
! REAL(KIND=DP_KIND),INTENT(IN) :: PX,PY
! REAL(KIND=DP_KIND),DIMENSION(N),INTENT(IN) :: XX,YY
! REAL(KIND=DP_KIND),DIMENSION(N) :: X,Y
! LOGICAL :: MX,MY,NX,NY
! INTEGER :: I,J
!
!! DO I=1,N; WRITE(*,'(I10,2F10.3)') I,XX(I),YY(I); ENDDO
!
! DO I=1,N
!  X(I)=XX(I)-PX
!  Y(I)=YY(I)-PY
! ENDDO
!
!! DO I=1,N; WRITE(*,'(I10,2F10.3)') I,X(I),Y(I); ENDDO
! 
! TMP_UTL_INSIDEPOLYGON2=-1
! DO I=1,N
!  J =1+MOD(I,N)
!  MX=X(I).GE.0.0D0
!  NX=X(J).GE.0.0D0
!  MY=Y(I).GE.0.0D0
!  NY=Y(J).GE.0.0D0
!  write(iu,'(a1,4f10.2,4l5,i5)') '-',x(i),y(i),x(j),y(j),mx,nx,my,ny,TMP_UTL_INSIDEPOLYGON2    
!  IF(.NOT.((MY.OR.NY).AND.(MX.OR.NX)).OR.(MX.AND.NX))THEN
!  ELSE
!   IF(.NOT.(MY.AND.NY.AND.(MX.OR.NX).AND..NOT.(MX.AND.NX)))THEN
!    IF(X(J)-X(I).NE.0.0D0)THEN
!     IF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I)).LT.0.0D0)THEN
!      write(iu,'(a1,4f10.2,i5)') '0',x(i),y(i),x(j),y(j),TMP_UTL_INSIDEPOLYGON2    
!     ELSEIF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I)).EQ.0.0D0)THEN
!      TMP_UTL_INSIDEPOLYGON2=0
!      write(iu,'(a1,4f10.2,i5)') '1',x(i),y(i),x(j),y(j),TMP_UTL_INSIDEPOLYGON2
!      EXIT 
!     ELSEIF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I)).GT.0.0D0)THEN
!      TMP_UTL_INSIDEPOLYGON2=-TMP_UTL_INSIDEPOLYGON2
!      write(iu,'(a1,4f10.2,i5)') '2',x(i),y(i),x(j),y(j),TMP_UTL_INSIDEPOLYGON2
!     ENDIF
!    ENDIF
!   ELSE
!    TMP_UTL_INSIDEPOLYGON2=-TMP_UTL_INSIDEPOLYGON2
!    write(iu,'(a1,4f10.2,i5)') '3',x(i),y(i),x(j),y(j),TMP_UTL_INSIDEPOLYGON2
!   ENDIF
!  ENDIF
! ENDDO
!
! END FUNCTION TMP_UTL_INSIDEPOLYGON2

 END MODULE MOD_PMANAGER_MF6NETWORK