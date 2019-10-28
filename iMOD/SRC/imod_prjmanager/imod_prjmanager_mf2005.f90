
    !!  Copyright (C) Stichting Deltares, 2005-2019.
!!
!!  This file is part of iMOD.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License as published by
!!  the Free Software Foundation, either version 3 of the License, or
!!  (at your option) any later version.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.Q*
!!
!!  Contact: imod.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands.
!!
MODULE MOD_PMANAGER_MF2005

USE WINTERACTER
USE RESOURCE
USE MOD_PMANAGER_PAR
USE MOD_PMANAGER_UTL
USE IMODVAR 
USE MOD_IDF 
USE MOD_UTL
USE MOD_IDF_PAR
USE MOD_ISG_PAR
USE MOD_ISG_GRID
USE MOD_ISG_UTL
USE MOD_POLINT
USE MOD_QKSORT
USE MOD_ASC2IDF_HFB
USE MOD_ASC2IDF_PAR 
USE MOD_ASC2IDF_UTL
USE MOD_OSD
USE MOD_IPEST_GLM, ONLY : IPEST_GLM_SETGROUPS

CONTAINS

 !###======================================================================
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS(GENFNAME,OUTFOLDER,NSUBMODEL)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: GENFNAME,OUTFOLDER
 INTEGER,INTENT(OUT) :: NSUBMODEL
 INTEGER,ALLOCATABLE,DIMENSION(:) :: ISORT,IPOL
 INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:,:) :: IPC
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
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
 ALLOCATE(IDF(SHP%NPOL)); DO I=1,SHP%NPOL; CALL IDFNULLIFY(IDF(I)); ENDDO 
 
 !## get dimensions of the idf files - read them from large cellsizes up to small cell sizes
 !## idf(1) is biggest; idf(n) is smallest
 CALL PMANAGER_GENERATEMFNETWORKS_DIMIDF(IDF,ISORT,IPOL)
 
 !## process from big to small ... determine in what polygon the selected polygon is and take that cellsize to generate the boundary polygon
 DO I=1,SIZE(IDF)

  !## current idf/polygon in sort list
  J =ISORT(I)
  !## use cellsize of idf which overlays the current idf
  II=IPOL(J)
  
  CALL ASC2IDF_INT_NULLIFY(); ALLOCATE(XP(100),YP(100),ZP(100),WP(100),FP(100))
  ALLOCATE(IPC(IDF(II)%NCOL,IDF(II)%NROW,2)); IPC=INT(0,1)
  
  !## intersect line and determine ipc()
  CALL ASC2IDF_HFB(IDF(II),IDF(II)%NROW,IDF(II)%NCOL,IPC,(/GENFNAME/),-1,IPOL=J)
  !## write genfiles
  CALL PMANAGER_GENERATEMFNETWORKS_WRITEGEN(IDF(II),IPC,IU,JU,J)
  CALL ASC2IDF_INT_DEALLOCATE(); DEALLOCATE(IPC)
 ENDDO
 WRITE(IU,'(A)') 'END'; CLOSE(IU); CLOSE(JU)

 !## sortgen files to be a polygon, blank out regions in the idf files
 CALL PMANAGER_GENERATEMFNETWORKS_CREATEPOLYGONS(OUTFOLDER,IDF,ISORT)
 
 !## write idf files
 DO I=1,SHP%NPOL
  J=ISORT(I)
  !## first model was a bit too big for the hfb simulations
  IF(J.EQ.1)THEN
   IDF(I)%XMIN=IDF(I)%XMIN+IDF(I)%DX; IDF(I)%XMAX=IDF(I)%XMAX-IDF(I)%DX
   IDF(I)%YMIN=IDF(I)%YMIN+IDF(I)%DY; IDF(I)%YMAX=IDF(I)%YMAX-IDF(I)%DY
   IDF(I)%NCOL=IDF(I)%NCOL-2; IDF(I)%NROW=IDF(I)%NROW-2
   DO II=1,IDF(I)%NROW; DO JJ=1,IDF(I)%NCOL
    IDF(I)%X(JJ,II)=IDF(I)%X(JJ+1,II+1)
   ENDDO; ENDDO
  ENDIF
  IDF(I)%FNAME=TRIM(OUTFOLDER)//'\SUBMODEL_'//TRIM(ITOS(I))//'.IDF'
  IF(.NOT.IDFWRITE(IDF(I),IDF(I)%FNAME,1))STOP
 ENDDO
 
 CALL IDFDEALLOCATE(IDF,SIZE(IDF)); DEALLOCATE(IDF)
 
 DEALLOCATE(ISORT,IPOL)
 
 NSUBMODEL=SHP%NPOL

 END SUBROUTINE PMANAGER_GENERATEMFNETWORKS
 
 !###======================================================================
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS_CREATEPOLYGONS(OUTFOLDER,IDF,ISORT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: OUTFOLDER
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: IDF
 INTEGER,INTENT(IN),DIMENSION(:) :: ISORT
 CHARACTER(LEN=256) :: FNAME
 INTEGER :: IOS,I,J,N,II,JJ,IU,JU,IROW,ICOL,JROW,JCOL
 REAL(KIND=DP_KIND) :: X1,Y1,X2,Y2
 REAL(KIND=DP_KIND),DIMENSION(:,:),POINTER :: XC,YC,XC_TMP,YC_TMP
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IS,JS

 !## open file with "faults" per submodel - need to be "puzzled" to a single feature
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(OUTFOLDER)//'\BND.XY'             ,ACTION='READ' ,STATUS='OLD'    ,FORM='FORMATTED')
 !## results is a GEN per polygon
 JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=TRIM(OUTFOLDER)//'\SUBMODELS_ASCII.GEN',ACTION='WRITE',STATUS='UNKNOWN',FORM='FORMATTED')
 
 ALLOCATE(XC(100,2),YC(100,2))
 
 J=1; N=0; DO
  READ(IU,*,IOSTAT=IOS) I,X1,Y1,X2,Y2
  IF(IOS.NE.0)I=I+1
  IF(I.NE.J)THEN
   ALLOCATE(IS(N),JS(N))
   CALL PMANAGER_GENERATEMFNETWORKS_PUZZLE(XC,YC,IS,JS,N)
   WRITE(JU,'(I10)') J
   JJ=0; DO II=1,N
    IF(JS(II).LT.0)CYCLE
    WRITE(JU,'(2(F15.3,A1))') XC(IS(II),1),',',YC(IS(II),1)
    JJ=II
   ENDDO
   WRITE(JU,'(2(F15.3,A1))') XC(IS(JJ),2),',',YC(IS(JJ),2)
   WRITE(JU,'(A3)') 'END'; DEALLOCATE(IS,JS)
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
 DO I=1,SIZE(IDF); IDF(I)%X=1.0D0; ENDDO
 
 DO I=SHP%NPOL,1,-1
  J=ISORT(I)
  DO IROW=1,IDF(J)%NROW; DO ICOL=1,IDF(J)%NCOL
   !## skip blanked out areas
   IF(IDF(J)%X(ICOL,IROW).EQ.IDF(J)%NODATA)CYCLE 
   CALL IDFGETLOC(IDF(J),IROW,ICOL,X1,Y1)
   IF(DBL_IGRINSIDESHAPE(X1,Y1,SHP%POL(I)).EQ.1)THEN
    IDF(J)%X(ICOL,IROW)=1.0D0
    !## deactivate others on this location
    DO II=I-1,1,-1 
     JJ=ISORT(II)
     IF(DBL_IGRINSIDESHAPE(X1,Y1,SHP%POL(II)).EQ.1)THEN
      CALL IDFIROWICOL(IDF(JJ),JROW,JCOL,X1,Y1)
      IDF(JJ)%X(JCOL,JROW)=IDF(JJ)%NODATA 
     ENDIF
    ENDDO
   ELSE
    IDF(J)%X(ICOL,IROW)=IDF(J)%NODATA
   ENDIF
  ENDDO; ENDDO
 ENDDO
 
 END SUBROUTINE PMANAGER_GENERATEMFNETWORKS_CREATEPOLYGONS
 
 !###======================================================================
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS_PUZZLE(XC,YC,IS,JS,N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,DIMENSION(:),INTENT(OUT) :: IS,JS
 REAL(KIND=DP_KIND),DIMENSION(:,:),INTENT(INOUT) :: XC,YC
 INTEGER :: NS,M,I,J,IPOS
 
 DO I=1,N; JS(I)=I; ENDDO
 
 !## find dangles
 DO I=1,N
  DO IPOS=1,2
   M=0; DO J=1,N
    IF(I.EQ.J)CYCLE
    IF(PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT(XC,YC,IPOS,I,J))M=M+1
   ENDDO
   IF(M.EQ.0)THEN
    !## skip this one
    JS(I)=-1
   ENDIF
  ENDDO
 ENDDO
 
 !## start at first non-dangle
 DO I=1,N; IF(JS(I).NE.-1)THEN; IS(1)=I; EXIT; ENDIF; ENDDO
 NS=1; JS(I)=0
 
 DO
  DO I=1,N
   !## already used   
   IF(JS(I).LE.0)CYCLE
   IF(PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT(XC,YC,2,IS(NS),JS(I)))THEN
    NS=NS+1; IS(NS)=JS(I); JS(I)=0; EXIT
   ENDIF
  ENDDO
  DO I=1,N; IF(JS(I).GT.0)EXIT; ENDDO
  IF(I.GT.N)EXIT 
  !SUM(JS).EQ.0)EXIT
 ENDDO
 
 END SUBROUTINE PMANAGER_GENERATEMFNETWORKS_PUZZLE
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT(XC,YC,IPOS,IS,JS)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),DIMENSION(:,:),INTENT(INOUT) :: XC,YC
 INTEGER,INTENT(IN) :: IS,JS,IPOS
 REAL(KIND=DP_KIND) :: X0,Y0,X1,Y1,X2,Y2
 
 PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT=.FALSE.
 
 X0=XC(IS,IPOS); Y0=YC(IS,IPOS)
 X1=XC(JS,1);    Y1=YC(JS,1)
 X2=XC(JS,2);    Y2=YC(JS,2)

 IF(UTL_DIST(X0,Y0,X1,Y1).LE.1.0D-3)THEN
  PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT=.TRUE.; RETURN
 !## connected inversely - switch coordinates
 ELSEIF(UTL_DIST(X0,Y0,X2,Y2).LE.1.0D-3)THEN
  XC(JS,1)=X2; YC(JS,1)=Y2; XC(JS,2)=X1; YC(JS,2)=Y1
  PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT=.TRUE.; RETURN
 ENDIF
 
 END FUNCTION PMANAGER_GENERATEMFNETWORKS_PUZZLEFIT

 !###======================================================================
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS_DIMIDF(IDF,ISORT,IPOL)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: IDF
 INTEGER,INTENT(IN),DIMENSION(:) :: ISORT,IPOL
 INTEGER :: I,II,JJ,MAXCOL,IOS
 REAL(KIND=DP_KIND) :: CS
 TYPE STROBJ
  CHARACTER(LEN=52) :: STRING
 END TYPE
 TYPE(STROBJ),ALLOCATABLE,DIMENSION(:) :: STR
 
 MAXCOL=0; IF(ASSOCIATED(SHP%COLNAMES))MAXCOL=SIZE(SHP%COLNAMES); ALLOCATE(STR(MAXCOL))
 
 DO I=1,SHP%NPOL
  
  IDF(I)%XMIN=SHP%POL(I)%XMIN; IDF(I)%YMIN=SHP%POL(I)%YMIN
  IDF(I)%XMAX=SHP%POL(I)%XMAX; IDF(I)%YMAX=SHP%POL(I)%YMAX

  DO II=1,MAXCOL
   STR(II)%STRING=''; DO JJ=1,SHP%LWIDTH(II); STR(II)%STRING(JJ:JJ)=SHP%POL(I)%LBL(II)%STRING(JJ); ENDDO
  ENDDO

  !## read cellsize in polygon
  READ(STR(2)%STRING,*,IOSTAT=IOS) IDF(I)%DX
  IF(IOS.NE.0)THEN; WRITE(*,'(/A/)') 'Cannot read cellsize for polygon #',I; STOP; ENDIF
  IDF(I)%DY=IDF(I)%DX
 
 ENDDO
 
 !## set cell sizes to rasterize along
 DO I=1,SHP%NPOL

  CS=IDF(IPOL(I))%DX
  !## find nice coordinates
  CALL UTL_IDFSNAPTONICEGRID(IDF(I)%XMIN,IDF(I)%XMAX,IDF(I)%YMIN,IDF(I)%YMAX,CS,IDF(I)%NCOL,IDF(I)%NROW)

  !## increase biggest model as "faults" won't capture the area
  IF(ISORT(I).EQ.1)THEN
   IDF(I)%XMIN=IDF(I)%XMIN-IDF(I)%DX; IDF(I)%XMAX=IDF(I)%XMAX+IDF(I)%DX
   IDF(I)%YMIN=IDF(I)%YMIN-IDF(I)%DY; IDF(I)%YMAX=IDF(I)%YMAX+IDF(I)%DY
  ENDIF

  !## get the right dimensions
  IDF(I)%NCOL=INT((IDF(I)%XMAX-IDF(I)%XMIN)/IDF(I)%DX)
  IDF(I)%NROW=INT((IDF(I)%YMAX-IDF(I)%YMIN)/IDF(I)%DY)
  IF(.NOT.IDFALLOCATEX(IDF(I)))THEN; WRITE(*,'(/A/)') 'Cannot allocate memory idf%x() #',I;               STOP; ENDIF
  IF(.NOT.IDFFILLSXSY(IDF(I)))THEN;  WRITE(*,'(/A/)') 'Cannot allocate memory for idf%sx()/idf%sy() #',I; STOP; ENDIF

 ENDDO

 DEALLOCATE(STR)

 END SUBROUTINE PMANAGER_GENERATEMFNETWORKS_DIMIDF
 
 !###======================================================================
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS_WRITEGEN(IDF,IPC,IU,JU,N)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER(KIND=1),DIMENSION(:,:,:),INTENT(IN) :: IPC
 INTEGER,INTENT(IN) :: IU,JU,N
 INTEGER :: IROW,ICOL
 
 DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
 
  !## place vertical wall
  IF(IPC(ICOL,IROW,1).EQ.INT(1,1))THEN
   CALL PMANAGER_GENERATEMFNETWORKS_WRITEXY(1,IU,JU,IPC,IDF,IROW,ICOL,N,0,0.0D0,0.0D0) 
  ENDIF
 
  !## place horizontal wall
  IF(IPC(ICOL,IROW,2).EQ.INT(1,1))THEN
   !## write line in genfile
   CALL PMANAGER_GENERATEMFNETWORKS_WRITEXY(2,IU,JU,IPC,IDF,IROW,ICOL,N,0,0.0D0,0.0D0) 
  ENDIF
 
 ENDDO; ENDDO
 
 END SUBROUTINE PMANAGER_GENERATEMFNETWORKS_WRITEGEN
 
 !###====================================================================
 SUBROUTINE PMANAGER_GENERATEMFNETWORKS_WRITEXY(IT,IU,JU,IPC,IDF,IROW,ICOL,N,I3D,T,B)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: IROW,ICOL,IU,JU,N,IT,I3D
 INTEGER(KIND=1),INTENT(IN),DIMENSION(:,:,:) :: IPC 
 REAL(KIND=DP_KIND),INTENT(IN) :: T,B

 IF(I3D.EQ.0)THEN

  !## place vertical wall
  IF(IT.EQ.1)THEN
   IF(IPC(ICOL,IROW,1).EQ.INT(1,1))THEN
    WRITE(IU,'(I10)') N
    WRITE(IU,'(2(F15.3,A1))') IDF%SX(ICOL),',',IDF%SY(IROW-1)
    WRITE(IU,'(2(F15.3,A1))') IDF%SX(ICOL),',',IDF%SY(IROW)
    WRITE(IU,'(A)') 'END'
    IF(JU.GT.0)WRITE(JU,'(I10,4(A1,F15.3))') N,',',IDF%SX(ICOL),',',IDF%SY(IROW-1),',',IDF%SX(ICOL),',',IDF%SY(IROW)
   ENDIF
  ENDIF
 
  !## place horizontal wall
  IF(IT.EQ.2)THEN
   IF(IPC(ICOL,IROW,2).EQ.INT(1,1))THEN
    WRITE(IU,'(I10)') N  
    WRITE(IU,'(2(F15.3,A1))') IDF%SX(ICOL-1),',',IDF%SY(IROW)
    WRITE(IU,'(2(F15.3,A1))') IDF%SX(ICOL  ),',',IDF%SY(IROW)
    WRITE(IU,'(A)') 'END'
    IF(JU.GT.0)WRITE(JU,'(I10,4(A1,F15.3))') N,',',IDF%SX(ICOL-1),',',IDF%SY(IROW),',',IDF%SX(ICOL  ),',',IDF%SY(IROW)
   ENDIF
  ENDIF

 ELSE

  !## place vertical wall
  IF(IT.EQ.1)THEN
   IF(IPC(ICOL,IROW,1).EQ.INT(1,1))THEN
    WRITE(IU,'(I10)') N
    WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL),',',IDF%SY(IROW-1),',',T
    WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL),',',IDF%SY(IROW)  ,',',T
    WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL),',',IDF%SY(IROW)  ,',',B
    WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL),',',IDF%SY(IROW-1),',',B
    WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL),',',IDF%SY(IROW-1),',',T
    WRITE(IU,'(A)') 'END'
   ENDIF
  ENDIF
 
  !## place horizontal wall
  IF(IT.EQ.2)THEN
   IF(IPC(ICOL,IROW,2).EQ.INT(1,1))THEN
    WRITE(IU,'(I10)') N  
    WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL-1),',',IDF%SY(IROW),',',T
    WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL  ),',',IDF%SY(IROW),',',T
    WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL  ),',',IDF%SY(IROW),',',B
    WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL-1),',',IDF%SY(IROW),',',B
    WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL-1),',',IDF%SY(IROW),',',T
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
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEPST(IU,IOPTION,DIR,ISS,IITER)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,IOPTION,ISS,IITER
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: I,N,M,SCL_UP,SCL_D,IOS,ICOL,IROW
 REAL(KIND=DP_KIND) :: Z
 
 PMANAGER_SAVEPST=.FALSE.
 
 !## write model dimensions into pst file
 IF(IOPTION.EQ.2)THEN
  WRITE(IU,*) PRJIDF%NCOL,PRJIDF%NROW,PRJNLAY,PRJNPER,ISS
  WRITE(IU,*) PRJIDF%XMIN,PRJIDF%YMIN,PRJIDF%XMAX,PRJIDF%YMAX,PRJIDF%IEQ
  IF(PRJIDF%IEQ.EQ.0)THEN
   WRITE(IU,*) PRJIDF%DX
  ELSE
   WRITE(IU,*) (PRJIDF%SX(ICOL),ICOL=0,PRJIDF%NCOL)
   WRITE(IU,*) (PRJIDF%SY(IROW),IROW=0,PRJIDF%NROW)
  ENDIF
 ENDIF
 
 IF(IOPTION.NE.1)THEN
  IF(ASSOCIATED(PEST%MEASURES))THEN
   I=SIZE(PEST%MEASURES)
   IF(PEST%IIPF.EQ.1)I=-1*I
   LINE=TRIM(ITOS(I))
   WRITE(IU,'(A)') TRIM(LINE)
   DO I=1,SIZE(PEST%MEASURES)
    LINE=CHAR(39)//TRIM(PEST%MEASURES(I)%IPFNAME)//CHAR(39)//','// &
         TRIM(ITOS(PEST%MEASURES(I)%IPFTYPE))//','// &
         TRIM(ITOS(PEST%MEASURES(I)%IXCOL))  //','// &
         TRIM(ITOS(PEST%MEASURES(I)%IYCOL))  //','// &
         TRIM(ITOS(PEST%MEASURES(I)%ILCOL))  //','// &
         TRIM(ITOS(PEST%MEASURES(I)%IMCOL))  //','// &
         TRIM(ITOS(PEST%MEASURES(I)%IVCOL))
    WRITE(IU,'(A)') TRIM(LINE)
   ENDDO
  ELSE
   LINE=TRIM(ITOS(0))
   WRITE(IU,'(A)') TRIM(LINE)
  ENDIF
 ENDIF
  
 IF(IOPTION.EQ.2)THEN
  LINE=TRIM(ITOS(SIZE(PEST%PARAM))); WRITE(IU,'(A)') TRIM(LINE)
 ENDIF
 
 N=0; IF(ASSOCIATED(PEST%S_PERIOD))  N=SIZE(PEST%S_PERIOD)
 M=0; IF(ASSOCIATED(PEST%B_FRACTION))M=SIZE(PEST%B_FRACTION)
 
 I=PEST%PE_MXITER; IF(IITER.EQ.-1.AND.PBMAN%IPESTP.EQ.1)I=-1
 LINE=TRIM(ITOS(I))                      //','//TRIM(RTOS(PEST%PE_STOP,'G',7))     //','// &
      TRIM(RTOS(PEST%PE_SENS,'G',7))     //','//TRIM(ITOS(N))                      //','// &
      TRIM(ITOS(M))                      //','//TRIM(RTOS(PEST%PE_TARGET(1),'G',7))//','// &
      TRIM(RTOS(PEST%PE_TARGET(2),'G',7))//','//TRIM(ITOS(PEST%PE_SCALING-1))      //','// &
      TRIM(RTOS(PEST%PE_PADJ,'G',7))     //','//TRIM(RTOS(PEST%PE_DRES,'G',7))     //','// &
      TRIM(ITOS(PEST%PE_KTYPE))          //','//TRIM(RTOS(PEST%PE_KRANGE,'G',7))   //','// &
      TRIM(ITOS(PEST%PE_REGULARISATION)) //','//TRIM(RTOS(PEST%PE_REGFACTOR,'G',7))
      
 WRITE(IU,'(A)') TRIM(LINE)

 !## write blankout idf
 IF(PEST%PE_KTYPE.LT.0)THEN
  IF(IOPTION.EQ.1)THEN
   WRITE(IU,'(A)') TRIM(PEST%PPBNDIDF)
  ELSEIF(IOPTION.EQ.2)THEN
   !## upscale is using number 7, most frequent
   SCL_UP=7; SCL_D=0
   !## read/clip/scale idf file
   IF(.NOT.IDFREADSCALE(PEST%PPBNDIDF,PRJIDF,SCL_UP,SCL_D,1.0D0,0))RETURN
   !## save array, do not correct for boundary condition as we not yet know for what layer the zone will apply
   IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\PST1\PPBNDIDF.ARR',PRJIDF,0,IU,1,0))RETURN
  ENDIF
 ENDIF
 
 IF(N.GT.0)THEN
  DO I=1,SIZE(PEST%S_PERIOD)
   LINE=TRIM(PEST%S_PERIOD(I))//','//TRIM(PEST%E_PERIOD(I))
   WRITE(IU,'(A)') TRIM(LINE)   
  ENDDO
 ENDIF
 
 IF(M.GT.0)THEN
  DO I=1,SIZE(PEST%B_FRACTION)
   LINE=TRIM(RTOS(PEST%B_FRACTION(I),'G',7))//','//CHAR(39)//TRIM(PEST%B_BATCHFILE(I))//CHAR(39)//','//CHAR(39)//TRIM(PEST%B_OUTFILE(I))//CHAR(39)
   WRITE(IU,'(A)') TRIM(LINE)   
  ENDDO
 ENDIF

 IF(ASSOCIATED(PEST%PARAM))THEN
  DO I=1,SIZE(PEST%PARAM)
   LINE=TRIM(ITOS(PEST%PARAM(I)%PACT))           //','// &
        TRIM(PEST%PARAM(I)%PPARAM)               //','// &
        TRIM(ITOS(PEST%PARAM(I)%PILS))           //','// &
        TRIM(ITOS(PEST%PARAM(I)%PIZONE))         //','// &    
        TRIM(RTOS(PEST%PARAM(I)%PINI,'G',7))     //','// &
        TRIM(RTOS(PEST%PARAM(I)%PDELTA,'G',7))   //','// &
        TRIM(RTOS(PEST%PARAM(I)%PMIN,'G',7))     //','// &
        TRIM(RTOS(PEST%PARAM(I)%PMAX,'G',7))     //','// &
        TRIM(RTOS(PEST%PARAM(I)%PINCREASE,'G',7))//','// &
        TRIM(ITOS(ABS(PEST%PARAM(I)%PIGROUP)))   //','// &
        TRIM(ITOS(PEST%PARAM(I)%PLOG))           //','// &
        '"'//TRIM(PEST%PARAM(I)%ACRONYM)        //'",'// &
        TRIM(RTOS(PEST%PARAM(I)%PPRIOR,'G',7))
   WRITE(IU,'(A)') TRIM(LINE)
  ENDDO
 ENDIF

 IF(ASSOCIATED(PEST%IDFFILES))THEN
  LINE=TRIM(ITOS(SIZE(PEST%IDFFILES)))
  WRITE(IU,'(A)') TRIM(LINE)    
  DO I=1,SIZE(PEST%IDFFILES)

   LINE=TRIM(PEST%IDFFILES(I))
  
   IF(IOPTION.EQ.2)THEN
    Z=INT(UTL_GETREAL(LINE,IOS))
    IF(IOS.EQ.0)THEN
     PRJIDF%X=Z
     !## save array, do not correct for boundary condition as we not yet know for what layer the zone will apply
     IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\PST1\ZONE_IZ'//TRIM(ITOS(I))//'.ARR',PRJIDF,0,IU,1,0))RETURN
    ELSE
     !## read idf
     IF(INDEX(UTL_CAP(LINE,'U'),'.IDF',.TRUE.).GT.0)THEN
      !## upscale is using number 15 is not completely correct but for reasons of backward compatibility. Undesired results can be overcome through additional file
      PRJIDF%FNAME=LINE; SCL_UP=15; SCL_D=0
      !## read/clip/scale idf file
      IF(.NOT.IDFREADSCALE(PRJIDF%FNAME,PRJIDF,SCL_UP,SCL_D,1.0D0,0))RETURN
      !## replace nodata for zero
      DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
       IF(PRJIDF%X(ICOL,IROW).EQ.PRJIDF%NODATA)PRJIDF%X(ICOL,IROW)=0.0D0
      ENDDO; ENDDO
      !## save array, do not correct for boundary condition as we not yet know for what layer the zone will apply
      IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\PST1\ZONE_IZ'//TRIM(ITOS(I))//'.ARR',PRJIDF,0,IU,1,0))RETURN
     ELSE
      WRITE(IU,'(A)') TRIM(LINE)    
     ENDIF
    ENDIF
   ELSE
    WRITE(IU,'(A)') TRIM(LINE)    
   ENDIF
   
  ENDDO
 ENDIF
 
 PMANAGER_SAVEPST=.TRUE.

 END FUNCTION PMANAGER_SAVEPST
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUN(FNAME,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=52) :: CDATE1,CDATE2
 CHARACTER(LEN=256) :: BNDFNAME
 INTEGER(KIND=8) :: ITIME,JTIME
 INTEGER :: IU,I,J,K,IPER,KPER,N,NSCL
 LOGICAL :: LDAYS,LEX
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 CHARACTER(LEN=256) :: LINE
 
 PMANAGER_SAVERUN=.FALSE.
 
 !## overrule ipst if not as keyword given
 IF(IBATCH.EQ.1.AND.PBMAN%IPEST.EQ.0)TOPICS(TPST)%IACT_MODEL=0
 
 !## get active packages, set default values
 IF(.NOT.PMANAGER_GETPACKAGES())RETURN

 DO I=1,MAXTOPICS  
  SELECT CASE (I)
   CASE (TFHB,TUZF,TMNW,TSFR,TLAK)
    IF(TOPICS(I)%IACT_MODEL.EQ.1)THEN
     CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'You cannot use the package '//TRIM(TOPICS(I)%TNAME)//CHAR(13)// &
      'to save for a RUN-file. Select the option MODFLOW2005 instead','Information')
     RETURN
    ENDIF
  END SELECT
 ENDDO

 !## remove last timestep sinces it is the final date
 IF(PRJNPER.GT.1)PRJNPER=PRJNPER-1
 
 PRJNLAY=PRJMXNLAY
 
 !## check on RUN file
 CALL UTL_CREATEDIR(FNAME(1:INDEX(FNAME,'\',.TRUE.)-1))
 IF(IBATCH.EQ.0)THEN
  INQUIRE(FILE=FNAME,EXIST=LEX)
  IF(LEX)THEN
   CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to overwrite'//CHAR(13)//TRIM(FNAME),'Question')
   IF(WINFODIALOG(4).NE.1)RETURN
  ENDIF
 ENDIF
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='REPLACE',ACTION='WRITE,DENYREAD',FORM='FORMATTED')
 IF(IU.EQ.0)RETURN

 !## write Data set 1
 IF(IBATCH.EQ.1)THEN
  IF(TRIM(PBMAN%OUTPUT).EQ.'')THEN
   WRITE(IU,'(A)') CHAR(39)//FNAME(1:INDEX(FNAME,'\',.TRUE.)-1)//CHAR(39)
  ELSE
   WRITE(IU,'(A)') CHAR(39)//TRIM(PBMAN%OUTPUT)//CHAR(39)
  ENDIF
 ELSE
  WRITE(IU,'(A)') CHAR(39)//TRIM(PREFVAL(1))//'\MODELS\'//TRIM(MODELNAME)//CHAR(39)
 ENDIF
 
 N=0; IF(ASSOCIATED(PEST%MEASURES))THEN
  N=SIZE(PEST%MEASURES); IF(PEST%IIPF.EQ.1)N=-1*N
 ENDIF
 
 !## metaswap
 IARMWP=0
 IF(TOPICS(TCAP)%IACT_MODEL.EQ.1)THEN
  IF(ASSOCIATED(TOPICS(TCAP)%STRESS))THEN
   LINE=TOPICS(TCAP)%STRESS(1)%FILES(8,1)%FNAME
   IF(INDEX(UTL_CAP(LINE,'U'),'IPF').GT.0)IARMWP=1
  ENDIF
 ENDIF
 NSCL=1
 IF(PBMAN%IWINDOW.EQ.3)NSCL=0
 IF(PBMAN%IWINDOW.EQ.2)THEN
  IF(SUBMODEL(7).GT.0.0D0)NSCL=2
 ENDIF
 WRITE(IU,'(12(I10,1X))') PRJNLAY,PRJMXNLAY,PRJNPER,PBMAN%ISAVEENDDATE,NSCL,0,PBMAN%ICONCHK,N,0,PBMAN%IFVDL,IARMWP

 !## write measures
 IF(N.NE.0)THEN
  DO I=1,SIZE(PEST%MEASURES)
   LINE=TRIM(PEST%MEASURES(I)%IPFNAME)      //','// &
        TRIM(ITOS(PEST%MEASURES(I)%IPFTYPE))//','// &
        TRIM(ITOS(PEST%MEASURES(I)%IXCOL))  //','// &
        TRIM(ITOS(PEST%MEASURES(I)%IYCOL))  //','// &
        TRIM(ITOS(PEST%MEASURES(I)%ILCOL))  //','// &
        TRIM(ITOS(PEST%MEASURES(I)%IMCOL))  //','// &
        TRIM(ITOS(PEST%MEASURES(I)%IVCOL))
   WRITE(IU,'(A)') TRIM(LINE)
  ENDDO
 ENDIF

 !## Data set 4
 IF(PBMAN%IWINDOW.EQ.3)THEN
  LINE='0'
 ELSE
  LINE='1'
 ENDIF
 LINE=TRIM(LINE)//',0,'//TRIM(ITOS(PBMAN%IDOUBLE))//',0,0,'//TRIM(ITOS(PBMAN%SSYSTEM))
 IF(PBMAN%MINKD.NE.0.0D0.OR.PBMAN%MINC.NE.0.0D0)THEN
  LINE=TRIM(LINE)//','//TRIM(RTOS(PBMAN%MINKD,'G',5))//','//TRIM(RTOS(PBMAN%MINC ,'G',5))
 ENDIF
 WRITE(IU,'(A)') TRIM(LINE)

 !## Data set 5
 IF(PCG%PARTOPT.GT.1)PCG%NOUTER=-ABS(PCG%NOUTER)
 LINE=TRIM(ITOS(PCG%NOUTER))//','//TRIM(ITOS(PCG%NINNER))//','// & 
      TRIM(RTOS(PCG%HCLOSE,'E',7))//','//TRIM(RTOS(PCG%RCLOSE,'E',7))//','// &
      TRIM(RTOS(PCG%RELAX,'E',7))
 IF(PCG%PARTOPT.GT.1)THEN
  !## PKS options
  LINE=TRIM(LINE)//','//TRIM(ITOS(PCG%PARTOPT-2))//','//TRIM(ITOS(PCG%IMERGE)) 
 ELSE
  !## PCG option
  LINE=TRIM(LINE)//','//TRIM(ITOS(PCG%NPCOND)) 
 ENDIF
 WRITE(IU,'(A)') TRIM(LINE)
 
 IF(PCG%PARTOPT.EQ.3.AND.TRIM(PCG%MRGFNAME).EQ.'')THEN
  CLOSE(IU); CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to specify a pointer IDF-file when selecting the RCB partition method.','Error')
  RETURN
 ENDIF
 
 IF(PCG%PARTOPT.EQ.3)THEN
  WRITE(IU,'(A)') '"'//TRIM(PCG%MRGFNAME)//'"'
 ENDIF

 !## Data set 6
 !## non-equistantial network
 IF(PBMAN%IWINDOW.EQ.3)THEN

  BNDFNAME=PBMAN%BNDFILE

 ELSE
 
  ALLOCATE(IDF(1)); CALL IDFNULLIFY(IDF(1))
  IF(.NOT.PMANAGER_INIT_SIMAREA(IDF(1),IBATCH))RETURN
  BNDFNAME=IDF(1)%FNAME

  IF(ISUBMODEL.EQ.0)THEN
   WRITE(IU,'(6(F15.3,A1))') IDF(1)%XMIN,',',IDF(1)%YMIN,',',IDF(1)%XMAX,',',IDF(1)%YMAX,',',IDF(1)%DX,',',0.0D0
  ELSE
   IF(SUBMODEL(6).GT.0.0D0.AND.SUBMODEL(7).GT.0.0D0)THEN
    WRITE(IU,'(7(F15.3,A1))') SUBMODEL(1),',',SUBMODEL(2),',',SUBMODEL(3),',',SUBMODEL(4),',',SUBMODEL(5),',',SUBMODEL(7),',',SUBMODEL(6)
   ELSE
    WRITE(IU,'(6(F15.3,A1))') SUBMODEL(1),',',SUBMODEL(2),',',SUBMODEL(3),',',SUBMODEL(4),',',SUBMODEL(5),',',SUBMODEL(6)
   ENDIF
  ENDIF
 
  CALL IDFDEALLOCATE(IDF,SIZE(IDF)); DEALLOCATE(IDF)
 
 ENDIF
 
 WRITE(IU,'(A)') 'ACTIVE MODULES'

 !## Data set 8
 DO I=1,MAXTOPICS  
  IF(TOPICS(I)%IACT_MODEL.EQ.0)CYCLE
  !## skip pcg
  IF(I.EQ.TPCG)CYCLE
  !## pst module is exception
  IF(I.EQ.TPST)THEN; WRITE(IU,'(A)') '1,0 '//TRIM(TOPICS(I)%TNAME); CYCLE; ENDIF
  IF(.NOT.ASSOCIATED(TOPICS(I)%STRESS))CYCLE
  IF(.NOT.ASSOCIATED(TOPICS(I)%STRESS(1)%FILES))CYCLE

  CALL PMANAGER_SAVEMF2005_RUN_ISAVE(PBMAN%ISAVE(I)%ILAY,TOPICS(I)%TNAME(1:5),IU)
  !
  !SELECT CASE (I)
  ! CASE (TSHD)
  !  CALL PMANAGER_SAVEMF2005_RUN_ISAVE(PBMAN%SAVESHD,TOPICS(I)%TNAME(1:5),IU)
  ! CASE (TKHV,TKVA,TKDW,TKVV,TVCW,TSTO,TSPY) !4,6,7,9,10,11)
  !  CALL PMANAGER_SAVEMF2005_RUN_ISAVE(PBMAN%SAVEFLX,TOPICS(I)%TNAME(1:5),IU)
  ! CASE (TWEL) !## wel
  !  CALL PMANAGER_SAVEMF2005_RUN_ISAVE(PBMAN%SAVEWEL,TOPICS(I)%TNAME(1:5),IU)
  ! CASE (TDRN) !## drn
  !  CALL PMANAGER_SAVEMF2005_RUN_ISAVE(PBMAN%SAVEDRN,TOPICS(I)%TNAME(1:5),IU)
  ! CASE (TRIV) !## riv
  !  CALL PMANAGER_SAVEMF2005_RUN_ISAVE(PBMAN%SAVERIV,TOPICS(I)%TNAME(1:5),IU)
  ! CASE (TEVT) !## evt
  !  CALL PMANAGER_SAVEMF2005_RUN_ISAVE(PBMAN%SAVEEVT,TOPICS(I)%TNAME(1:5),IU)
  ! CASE (TGHB) !## ghb
  !  CALL PMANAGER_SAVEMF2005_RUN_ISAVE(PBMAN%SAVEGHB,TOPICS(I)%TNAME(1:5),IU)
  ! CASE (TRCH) !## rch
  !  CALL PMANAGER_SAVEMF2005_RUN_ISAVE(PBMAN%SAVERCH,TOPICS(I)%TNAME(1:5),IU)
  ! CASE (TOLF) !## olf
  !  CALL PMANAGER_SAVEMF2005_RUN_ISAVE(PBMAN%SAVEDRN,TOPICS(I)%TNAME(1:5),IU)
  ! CASE (TISG) !## isg
  !  CALL PMANAGER_SAVEMF2005_RUN_ISAVE(PBMAN%SAVERIV,TOPICS(I)%TNAME(1:5),IU)
  ! CASE DEFAULT
  !  WRITE(IU,'(A)') '1,0 '//TRIM(TOPICS(I)%TNAME)
  !END SELECT

 ENDDO

 !## write bndfile, Data set 9
 WRITE(IU,'(A)') CHAR(39)//TRIM(BNDFNAME)//CHAR(39)

 WRITE(IU,'(A)') 'MODULES FOR EACH LAYER'
  
 !## write modules not timedependent
 DO I=1,MAXTOPICS
  IF(TOPICS(I)%IACT_MODEL.EQ.0)CYCLE
  IF(TOPICS(I)%TIMDEP)CYCLE

  !## skip pcg
  IF(I.EQ.TPCG)CYCLE
  
  !## pst module is exception
  IF(I.EQ.TPST)THEN
   LINE=TRIM(ITOS(SIZE(PEST%PARAM)))//',(PST)'; WRITE(IU,'(A)') TRIM(LINE) 
   IF(.NOT.PMANAGER_SAVEPST(IU,1,'',0,0))THEN; ENDIF; CYCLE
  ENDIF
  
  IF(.NOT.ASSOCIATED(TOPICS(I)%STRESS))CYCLE
  IF(.NOT.ASSOCIATED(TOPICS(I)%STRESS(1)%FILES))CYCLE
  
  !## check the number of active packages
  IF(I.EQ.TCAP)THEN
   N=SIZE(TOPICS(I)%STRESS(1)%FILES,1)
   IF(ASSOCIATED(TOPICS(I)%STRESS(1)%INPFILES))THEN
    N=N+SIZE(TOPICS(I)%STRESS(1)%INPFILES)
   ENDIF
  ELSE
   K=1; N=0
   DO J=1,SIZE(TOPICS(I)%STRESS(1)%FILES,2)
    IF(TOPICS(I)%STRESS(1)%FILES(K,J)%IACT.EQ.1)N=N+1
   ENDDO
  ENDIF
  
  WRITE(IU,'(I3.3,A)') N,','//TRIM(TOPICS(I)%TNAME)

  IF(N.GT.0)THEN
   !## number of subtopics
   DO K=1,SIZE(TOPICS(I)%STRESS(1)%FILES,1)
    !## number of systems
    DO J=1,SIZE(TOPICS(I)%STRESS(1)%FILES,2)
     !## skip temporary deactivated packages
     IF(TOPICS(I)%STRESS(1)%FILES(K,J)%IACT.EQ.0)CYCLE
     !## msp/pwt - skip ilay
     IF(I.EQ.TCAP.OR.I.EQ.TPWT)THEN
      WRITE(LINE,'(5X,     2(G15.7,A1))') &
                                  TOPICS(I)%STRESS(1)%FILES(K,J)%FCT ,',', &
                                  TOPICS(I)%STRESS(1)%FILES(K,J)%IMP ,','
     ELSE
      WRITE(LINE,'(1X,I5,2(A1,G15.7),A1)') &
                                  TOPICS(I)%STRESS(1)%FILES(K,J)%ILAY,',', &
                                  TOPICS(I)%STRESS(1)%FILES(K,J)%FCT ,',', &
                                  TOPICS(I)%STRESS(1)%FILES(K,J)%IMP ,','
     ENDIF
     IF(TOPICS(I)%STRESS(1)%FILES(K,J)%ICNST.EQ.1)THEN
      LINE=TRIM(LINE)//TRIM(RTOS(TOPICS(I)%STRESS(1)%FILES(K,J)%CNST,'G',7))
     ELSEIF(TOPICS(I)%STRESS(1)%FILES(K,J)%ICNST.EQ.2)THEN
      LINE=TRIM(LINE)//CHAR(39)//TRIM(TOPICS(I)%STRESS(1)%FILES(K,J)%FNAME)//CHAR(39)
     ENDIF
     WRITE(IU,'(A)') TRIM(LINE)

    ENDDO
   ENDDO
  
   !## write extra files only for MetaSWAP
   IF(I.EQ.TCAP)THEN
    IF(ASSOCIATED(TOPICS(I)%STRESS(1)%INPFILES))THEN
     K=SIZE(TOPICS(I)%STRESS(1)%INPFILES)
     DO J=1,K; WRITE(IU,'(1X,A)') TRIM(TOPICS(I)%STRESS(1)%INPFILES(J)); ENDDO
    ENDIF
   ENDIF
 
  ENDIF

 ENDDO

 WRITE(IU,'(A)') 'PACKAGES FOR EACH LAYER AND STRESS-PERIOD '

 !## only days available
 LDAYS=.TRUE.
 DO KPER=1,PRJNPER
  IF(SIM(KPER)%IHR+SIM(KPER)%IMT+SIM(KPER)%ISC.GT.0)THEN; LDAYS=.FALSE.; EXIT; ENDIF
 ENDDO

 !## write packages - incl./excl. steady-state
 DO KPER=1,PRJNPER
  
  !## steady-state
  IF(SIM(KPER)%DELT.EQ.0.0D0)THEN
   WRITE(IU,'(I5.5,A1,F15.7,A1,A,2(A1,I1))') KPER,',',SIM(KPER)%DELT,',',TRIM(SIM(KPER)%CDATE),',',SIM(KPER)%ISAVE,',',SIM(KPER)%ISUM
  !## transient (use final date as well, used for labeling file-names!)
  ELSE
   IF(LDAYS)THEN
    WRITE(CDATE1,'(I4.4,2I2.2)') SIM(KPER)%IYR  ,SIM(KPER)%IMH  ,SIM(KPER)%IDY
   ELSE
    WRITE(CDATE1,'(I4.4,5I2.2)') SIM(KPER)%IYR  ,SIM(KPER)%IMH  ,SIM(KPER)%IDY  ,SIM(KPER)%IHR  ,SIM(KPER)%IMT  ,SIM(KPER)%ISC
   ENDIF   
   IF(LDAYS)THEN
    WRITE(CDATE2,'(I4.4,2I2.2)') SIM(KPER+1)%IYR,SIM(KPER+1)%IMH,SIM(KPER+1)%IDY
   ELSE
    WRITE(CDATE2,'(I4.4,5I2.2)') SIM(KPER+1)%IYR,SIM(KPER+1)%IMH,SIM(KPER+1)%IDY,SIM(KPER+1)%IHR,SIM(KPER+1)%IMT,SIM(KPER+1)%ISC
   ENDIF
   WRITE(IU,'(I5.5,A1,F15.7,A1,A,2(A1,I1),A)') KPER,',',SIM(KPER)%DELT,',',TRIM(CDATE1),',',SIM(KPER)%ISAVE,',',SIM(KPER)%ISUM,','//TRIM(CDATE2)
  ENDIF

  DO I=1,MAXTOPICS  
   IF(TOPICS(I)%IACT_MODEL.EQ.0)CYCLE
   IF(.NOT.TOPICS(I)%TIMDEP)CYCLE
   IF(.NOT.ASSOCIATED(TOPICS(I)%STRESS))CYCLE
   IF(.NOT.ASSOCIATED(TOPICS(I)%STRESS(1)%FILES))CYCLE

   IPER=PMANAGER_GETCURRENTIPER(KPER,I,ITIME,JTIME)
   
   !## overrule wel/isg packages per stress-period
   SELECT CASE (I)
    CASE (TWEL); IF(PBMAN%DWEL.EQ.1)IPER=ABS(IPER)
    CASE (TISG); IF(PBMAN%DISG.EQ.1)IPER=ABS(IPER)
    CASE (TSFR); IF(PBMAN%DSFR.EQ.1)IPER=ABS(IPER)
   END SELECT

   !## reuse previous timestep
   IF(IPER.LE.0)THEN

    N=MAX(IPER,-1)
    WRITE(IU,'(I3,A)') N,','//TRIM(TOPICS(I)%TNAME)
   
   ELSE
   
    !## check the number of active packages
    K=1; N=0
    DO J=1,SIZE(TOPICS(I)%STRESS(IPER)%FILES,2)
     IF(TOPICS(I)%STRESS(IPER)%FILES(K,J)%IACT.EQ.1)N=N+1
    ENDDO
    WRITE(IU,'(I3,A)') N,','//TRIM(TOPICS(I)%TNAME)
 
    IF(N.GT.0)THEN
     !## number of subtopics
     DO K=1,SIZE(TOPICS(I)%STRESS(IPER)%FILES,1)
      !## number of systems
      DO J=1,SIZE(TOPICS(I)%STRESS(IPER)%FILES,2)
       !## skip temporary deactivated packages
       IF(TOPICS(I)%STRESS(IPER)%FILES(K,J)%IACT.EQ.0)CYCLE
       IF(TOPICS(I)%STRESS(IPER)%FILES(K,J)%ICNST.EQ.1)THEN
        WRITE(IU,'(1X,I5,3(A1,G15.7))') &
                                    TOPICS(I)%STRESS(IPER)%FILES(K,J)%ILAY,',', &
                                    TOPICS(I)%STRESS(IPER)%FILES(K,J)%FCT ,',', &
                                    TOPICS(I)%STRESS(IPER)%FILES(K,J)%IMP ,',', &
                                    TOPICS(I)%STRESS(IPER)%FILES(K,J)%CNST
       ELSEIF(TOPICS(I)%STRESS(IPER)%FILES(K,J)%ICNST.EQ.2)THEN
        WRITE(IU,'(1X,I5,2(A1,G15.7),A1,A)') &
                                    TOPICS(I)%STRESS(IPER)%FILES(K,J)%ILAY,',', &
                                    TOPICS(I)%STRESS(IPER)%FILES(K,J)%FCT ,',', &
                                    TOPICS(I)%STRESS(IPER)%FILES(K,J)%IMP ,',', &
                     CHAR(39)//TRIM(TOPICS(I)%STRESS(IPER)%FILES(K,J)%FNAME)//CHAR(39)
       ENDIF
      ENDDO
     ENDDO
    ENDIF
    
   ENDIF
  ENDDO
 ENDDO
 
 CLOSE(IU)
 
 PMANAGER_SAVERUN=.TRUE.

 END FUNCTION PMANAGER_SAVERUN

 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ(FNAME,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=52) :: CDATE1,CDATE2
 CHARACTER(LEN=256) :: BNDFNAME
 INTEGER(KIND=8) :: ITIME,JTIME
 INTEGER :: IU,I,J,K,IPER,KPER,N,NSCL
 LOGICAL :: LDAYS,LEX
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 CHARACTER(LEN=256) :: LINE
 
 PMANAGER_SAVERUNWQ=.FALSE.
 
 ! Frans: lijstje PBMAN uitbreiden met iMOD WQ items....
 !    er is bijvoorbeeld PBMAN%IWINDOW,PBMAN%IDOUBLE
 ! Frans: modelwindow: gegevens uit tabblad halen. Daar wordt berekendt welke minmx x y het wordt. 
 
 ! Frans: voorbeeld skippen bij foute invoer
 !IF(PCG%PARTOPT.EQ.3.AND.TRIM(PCG%MRGFNAME).EQ.'')THEN
 ! CLOSE(IU); CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to specify a pointer IDF-file when selecting the RCB partition method.','Error')
 ! RETURN
 !ENDIF

 !!## overrule ipst if not as keyword given
 !IF(IBATCH.EQ.1.AND.PBMAN%IPEST.EQ.0)TOPICS(TPST)%IACT_MODEL=0

 !## get active packages, set default values
 ! Frans: moet deze aangepast voor SEAWAT?
 IF(.NOT.PMANAGER_GETPACKAGES())RETURN

 !# Check if obligatory packages are active for MT3D and SEAWAT
 IF(.NOT.PMANAGER_SAVERUNWQ_CHK())RETURN

 !DO I=1,MAXTOPICS  
 ! SELECT CASE (I)
 !  CASE (TFHB,TUZF,TMNW,TSFR,TLAK)
 !   IF(TOPICS(I)%IACT_MODEL.EQ.1)THEN
 !    CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'You cannot use the package '//TRIM(TOPICS(I)%TNAME)//CHAR(13)// &
 !     'to save for a RUN-file. Select the option MODFLOW2005 instead','Information')
 !    RETURN
 !   ENDIF
 ! END SELECT
 !ENDDO

 !## remove last timestep sinces it is the final date
 IF(PRJNPER.GT.1)PRJNPER=PRJNPER-1
 
 !## Prepare result model file 
 CALL UTL_CREATEDIR(FNAME(1:INDEX(FNAME,'\',.TRUE.)-1))
 IF(IBATCH.EQ.0)THEN
  INQUIRE(FILE=FNAME,EXIST=LEX)
  IF(LEX)THEN
   CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to overwrite'//CHAR(13)//TRIM(FNAME),'Question')
   IF(WINFODIALOG(4).NE.1)RETURN
  ENDIF
 ENDIF
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='REPLACE',ACTION='WRITE,DENYREAD',FORM='FORMATTED')
 IF(IU.EQ.0)RETURN

 WRITE(IU,'(A)') '############################################################################'
 WRITE(IU,'(A)') '# iMOD run-file for SEAWAT '
 WRITE(IU,'(A)') '############################################################################'

 !## write Modflow Packages
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTGEN(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTDIS(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTBAS6(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTBCF6(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTOC(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTLPF(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTRCH(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTDRN(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTRIV(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTGHB(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTWEL(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTCHD(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTPCG(IU))RETURN
  
 !## write MT3D/Seawat Packages
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTBTN(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTADV(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTDSP(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTGCG(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTSSM(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTVDF(IU))RETURN
 
 CLOSE(IU)
 
 PMANAGER_SAVERUNWQ=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTBTN(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTBTN=.FALSE.

 WRITE(IU,'(/A)') '[BTN] # MT3DMS Basic Transport Package'

 PMANAGER_SAVERUNWQ_WRTBTN=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTBTN

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTADV(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTADV=.FALSE.

 WRITE(IU,'(/A)') '[ADV] # MT3DMS ADVection package'

 PMANAGER_SAVERUNWQ_WRTADV=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTADV
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTDSP(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTDSP=.FALSE.

 WRITE(IU,'(/A)') '[DSP] #MT3DMS Dispersion Package'

 PMANAGER_SAVERUNWQ_WRTDSP=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTDSP
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTGCG(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTGCG=.FALSE.

 WRITE(IU,'(/A)') '[GCG] # MT3DMS Generalized Conjugate Gradient Solver Package'

 PMANAGER_SAVERUNWQ_WRTGCG=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTGCG
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTSSM(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTSSM=.FALSE.

 WRITE(IU,'(A)') '[SSM] # MT3DMS Sink Source Mixing Package'

 PMANAGER_SAVERUNWQ_WRTSSM=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTSSM

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTVDF(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTVDF=.FALSE.

 WRITE(IU,'(/A)') '[VDF] # Variable-Density Flow '

 PMANAGER_SAVERUNWQ_WRTVDF=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTVDF

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTPCG(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTPCG=.FALSE.

 WRITE(IU,'(/A)') '[PCG] # MODFLOW Preconditioned Conjugate-Gradient Package'

 PMANAGER_SAVERUNWQ_WRTPCG=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTPCG

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTCHD(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTCHD=.FALSE.

 WRITE(IU,'(/A)') '[CHD]'

 PMANAGER_SAVERUNWQ_WRTCHD=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTCHD

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTWEL(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTWEL=.FALSE.

 WRITE(IU,'(/A)') '[WEL]'

 PMANAGER_SAVERUNWQ_WRTWEL=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTWEL

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTGHB(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTGHB=.FALSE.

 WRITE(IU,'(/A)') '[GHB]'

 PMANAGER_SAVERUNWQ_WRTGHB=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTGHB

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTRIV(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTRIV=.FALSE.

 WRITE(IU,'(/A)') '[RIV]'

 PMANAGER_SAVERUNWQ_WRTRIV=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTRIV

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTDRN(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTDRN=.FALSE.

 WRITE(IU,'(/A)') '[DRN]'

 PMANAGER_SAVERUNWQ_WRTDRN=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTDRN

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTRCH(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTRCH=.FALSE.

 WRITE(IU,'(A)') '[RCH]'

 PMANAGER_SAVERUNWQ_WRTRCH=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTRCH

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTLPF(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTLPF=.FALSE.

 WRITE(IU,'(/A)') '[LPF]'
  
 PMANAGER_SAVERUNWQ_WRTLPF=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTLPF

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTOC(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTOC=.FALSE.

 WRITE(IU,'(/A)') '[OC] # Output Control option'
  
 PMANAGER_SAVERUNWQ_WRTOC=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTOC
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTBCF6(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTBCF6=.FALSE.

 WRITE(IU,'(/A)') '[BCF6] # BLOCK CENTRED FLOW'
 
 WRITE(IU,'(A)') 'IBCFCB = 0 '
 WRITE(IU,'(A)') 'HDRY = -9999 '
 WRITE(IU,'(A)') 'IWDFLG = 0 '
 WRITE(IU,'(A)') 'WETFCT = 1 '
 WRITE(IU,'(A)') 'IWETIT = 1 '
 WRITE(IU,'(A)') 'IHDWET = 0 '
 WRITE(IU,'(A)') 'LTYPE_L? = 0 '
 WRITE(IU,'(A)') 'TRPY_L? = 1 '
 WRITE(IU,'(A)') 'SF1_L? = - '
 WRITE(IU,'(A)') 'TRAN_L? = - '
 WRITE(IU,'(A)') 'HY_L? = - '
 WRITE(IU,'(A)') 'VCONT_L? = - '
 WRITE(IU,'(A)') 'SF2_L? = - '
 WRITE(IU,'(A)') 'WETDRY_L? = - '


 PMANAGER_SAVERUNWQ_WRTBCF6=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTBCF6
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTBAS6(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTBAS6=.FALSE.
 
 WRITE(IU,'(/A)') '[BAS6] # MODFLOW BASic Package'

 DO ILAY=1,PRJNLAY
  ITOPIC=TBND
  IF(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%ICNST.EQ.1)THEN
    WRITE(IU,'(A)') 'IBOUND_L'//TRIM(ITOS(ILAY))//' = '//TRIM(UTL_REALTOSTRING(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%CNST)) 
  ELSE
    WRITE(IU,'(A)') 'IBOUND_L'//TRIM(ITOS(ILAY))//' = '//TRIM(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%FNAME)
  ENDIF  
 ENDDO

 WRITE(IU,'(A)') 'HNOFLO    = -9999.0            '

 DO ILAY=1,PRJNLAY
  ITOPIC=TSHD
  IF(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%ICNST.EQ.1)THEN
    WRITE(IU,'(A)') 'STRT_L'//TRIM(ITOS(ILAY))//' = '//TRIM(UTL_REALTOSTRING(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%CNST)) 
  ELSE
    WRITE(IU,'(A)') 'STRT_L'//TRIM(ITOS(ILAY))//' = '//TRIM(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%FNAME)
  ENDIF  
 ENDDO
 
 PMANAGER_SAVERUNWQ_WRTBAS6=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTBAS6
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTDIS(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ICOL,IROW,LCBD,N,ITOPIC,KPER
 REAL(KIND=DP_KIND) :: DELX,DELY

 PMANAGER_SAVERUNWQ_WRTDIS=.FALSE.

 WRITE(IU,'(/A)') '[DIS] # MODFLOW DIScretization Package'
  
 WRITE(IU,'(A)') 'MODELNAME = '//CHAR(39)//TRIM(MODELNAME)//CHAR(39)
 N=0; DO I=1,SIZE(PBMAN%ILAY); IF(PBMAN%ILAY(I).EQ.1)N=N+1; ENDDO
 WRITE(IU,'(A)') 'NLAY = '//TRIM(ITOS(N)) ! PRJNLAY))
 WRITE(IU,'(A)') 'NROW = '//TRIM(ITOS(PRJIDF%NROW))
 WRITE(IU,'(A)') 'NCOL = '//TRIM(ITOS(PRJIDF%NCOL))
 WRITE(IU,'(A)') 'NPER = '//TRIM(ITOS(PRJNPER))
 WRITE(IU,'(A)') 'ITMUNI = 4' ! default
 WRITE(IU,'(A)') 'LENUNI = 2' ! default
 LCBD=-1
 DO ILAY=1,PRJNLAY
  IF(ILAY.LT.PRJNLAY)THEN
   !## quasi-3d scheme
   IF(LQBD)THEN
    LCBD=1
   !## 3d no quasi confining bed
   ELSE
    LCBD=0
   ENDIF
  ELSE
   !## lowest layer has never a quasi-confining bed
   LCBD=0
  ENDIF
  WRITE(IU,'(A)') 'LAYCBD_L'//TRIM(ITOS(PRJNLAY))//' = '//TRIM(ITOS(LCBD))
 ENDDO

 IF(PRJIDF%IEQ.EQ.0)THEN 
   WRITE(IU,'(A)') '  DELR_C? = '//TRIM(RTOS(PRJIDF%DX,'E',7))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(5)))
 ELSE
  DO ICOL=1,PRJIDF%NCOL 
   DELX=PRJIDF%SX(ICOL)-PRJIDF%SX(ICOL-1)   
   WRITE(IU,'(A)')'  DELR_C'//TRIM(ITOS(ICOL))//' = '//TRIM(RTOS(DELX,'E',7))
  ENDDO
 ENDIF
 IF(PRJIDF%IEQ.EQ.0)THEN
   WRITE(IU,'(A)') '  DELR_R? = '//TRIM(RTOS(PRJIDF%DY,'E',7))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(5)))
 ELSE
  DO IROW=1,PRJIDF%NROW
   DELY=PRJIDF%SY(IROW-1)-PRJIDF%SY(IROW)   
   WRITE(IU,'(A)')'  DELR_R'//TRIM(ITOS(IROW))//' = '//TRIM(RTOS(DELY,'E',7))
  ENDDO
 ENDIF

 DO ILAY=1,PRJNLAY
  ITOPIC=TTOP
  !ITOPIC=2
  !## quasi-3d scheme add top aquifer modellayer
  !IF(LQBD.OR.ILAY.EQ.1)THEN
  IF(ILAY.EQ.1)THEN
    IF(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%ICNST.EQ.1)THEN
      WRITE(IU,'(A)') 'TOP = '//TRIM(UTL_REALTOSTRING(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%CNST)) 
    ELSE
      WRITE(IU,'(A)') 'TOP = '//TRIM(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%FNAME)
    ENDIF  
  ENDIF
  ITOPIC=TBOT
  !ITOPIC=3
  IF(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%ICNST.EQ.1)THEN
    WRITE(IU,'(A)') 'BOT_L'//TRIM(ITOS(ILAY))//' = '//TRIM(UTL_REALTOSTRING(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%CNST)) 
  ELSE
    WRITE(IU,'(A)') 'BOT_L'//TRIM(ITOS(ILAY))//' = '//TRIM(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%FNAME)
  ENDIF  
 ENDDO

 IF(MINVAL(SIM(1:PRJNPER)%DELT).EQ.MAXVAL(SIM(1:PRJNPER)%DELT))THEN
   WRITE(IU,'(A)') 'PERLEN_P? = '//TRIM(UTL_REALTOSTRING(MAXVAL(SIM(:)%DELT)))
 ELSE
   DO KPER=1,PRJNPER
    WRITE(IU,'(A)') 'PERLEN_P'//TRIM(ITOS(KPER))//' = '//TRIM(UTL_REALTOSTRING(SIM(KPER)%DELT))
   ENDDO
 ENDIF

 IF(MINVAL(SIM(1:PRJNPER)%NSTP).EQ.MAXVAL(SIM(1:PRJNPER)%NSTP))THEN
   WRITE(IU,'(A)') 'NSTP_P? = '//TRIM(ITOS(MAXVAL(SIM(1:PRJNPER)%NSTP)))
 ELSE
   DO KPER=1,PRJNPER
    WRITE(IU,'(A)') 'NSTP_P'//TRIM(ITOS(KPER))//' = '//TRIM(ITOS(SIM(KPER)%NSTP))
   ENDDO
 ENDIF

 WRITE(IU,'(A)') 'TSMULT_P? = 1'  ! frans: aanpassen?
 WRITE(IU,'(A)') 'SSTR_P? = TR'   ! frans: aanpassen?

 PMANAGER_SAVERUNWQ_WRTDIS=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTDIS

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTGEN(IU)
 !####====================================================================
 IMPLICIT NONE
 !CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME
 INTEGER,INTENT(IN) :: IU
 INTEGER :: KPER

 PMANAGER_SAVERUNWQ_WRTGEN=.FALSE.
 WRITE(IU,'(/A)') '[GEN] # GENeral settings'
 WRITE(IU,'(A)') 'MODELNAME = '//CHAR(39)//TRIM(MODELNAME)//CHAR(39)
 WRITE(IU,'(A)') 'WRITEHELP     =   F'         
 WRITE(IU,'(A)') 'ECHODEFAULTS  =   F'         
 WRITE(IU,'(A)') 'RESULT_DIR    =   '//CHAR(39)//TRIM(PREFVAL(1))//'\MODELS\'//TRIM(MODELNAME)//CHAR(39)
 WRITE(IU,'(A)') 'IDFDEBUG      =   F'         
 PBMAN%RUNTYPE='SEAWAT' ; IF(PBMAN%IFORMAT.EQ.5) PBMAN%RUNTYPE='MT3DMS'
 WRITE(IU,'(A)') 'RUNTYPE       =   '//PBMAN%RUNTYPE
 WRITE(IU,'(A)') 'PACKAGES      =   -'         
 WRITE(IU,'(A)') 'COORD_XLL     =   '//TRIM(UTL_REALTOSTRING(PBMAN%XMIN))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(1)))   ! Frans vraag: of gebruiken SUBMODEL(x)
 WRITE(IU,'(A)') 'COORD_YLL     =   '//TRIM(UTL_REALTOSTRING(PBMAN%YMIN))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(2))) 
 WRITE(IU,'(A)') 'COORD_XUR     =   '//TRIM(UTL_REALTOSTRING(PBMAN%XMAX))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(3)))
 WRITE(IU,'(A)') 'COORD_YUR     =   '//TRIM(UTL_REALTOSTRING(PBMAN%YMAX))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(4)))

 !## look for first
 DO KPER=1,PRJNPER; IF(SIM(KPER)%DELT.GT.0.0D0)EXIT; ENDDO
 
 WRITE(IU,'(A)') 'START_YEAR    =   '//TRIM(ITOS(SIM(KPER)%IYR))
 WRITE(IU,'(A)') 'START_MONTH   =   '//TRIM(ITOS(SIM(KPER)%IMH))
 WRITE(IU,'(A)') 'START_DAY     =   '//TRIM(ITOS(SIM(KPER)%IDY))
 WRITE(IU,'(A)') 'START_HOUR    =   1'         
 WRITE(IU,'(A)') 'START_MINUTE  =   1'         
 WRITE(IU,'(A)') 'START_SECOND  =   1'         

 PMANAGER_SAVERUNWQ_WRTGEN=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTGEN

 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_CHK()
 !###======================================================================
 ! actie: nog opschonen
 IMPLICIT NONE
 !CHARACTER(LEN=*),INTENT(IN) :: FNAME
 !INTEGER,INTENT(IN) :: IBATCH
 !CHARACTER(LEN=52) :: CDATE1,CDATE2
 !CHARACTER(LEN=256) :: BNDFNAME
 !INTEGER(KIND=8) :: ITIME,JTIME
 !INTEGER :: IU,I,J,K,IPER,KPER,N,NSCL
 !LOGICAL :: LDAYS,LEX
 !TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 !CHARACTER(LEN=256) :: LINE
 
 PMANAGER_SAVERUNWQ_CHK=.FALSE.
! VDF, BTN, ADV ,DSP ,SSM, FTL, GCG, RCT, UDR
! RCT en UDR zijn reactie packages; die zijn wel echt optioneel. En per simulatie kan maar 1 van deze 2 gebruikt worden.
! dus als het makkelijker is zou je gewoon alle MT3DMS packages verplicht kunnen maken.

! BAS6, DIS, WEL, DRN, RIV, GHB, CHD, LPF, BCF6, RCH, EVT, OC, VDF, PCG, , BTN
! verplicht: BAS6, DIS, OC, PCG en  LPF of BCF6
 
 ! NPER > 0 
 
 IF(.NOT.LPCG)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'It is compulsory to add a solver, e.g. PCG','Error')
  RETURN
 ENDIF

 PMANAGER_SAVERUNWQ_CHK=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_CHK
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005(FNAME,IBATCH) 
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=512) :: DIRMNAME,DIR,MAINDIR
 INTEGER(KIND=8) :: ITIME,JTIME
 INTEGER :: IULAK,ISTEADY,IPER,INIPER,LPER,KPER,IINI,IPRT,I,J
 LOGICAL :: LTB

 PMANAGER_SAVEMF2005=.FALSE.; LYESNO=.FALSE.
 
 !## remove final stress as it is the final timestep
 IF(PRJNPER.GT.1)PRJNPER=PRJNPER-1
   
 ISTEADY=0; IF(SIM(1)%DELT.EQ.0.0D0)ISTEADY=1

 !## time information
 ISS=0; DO KPER=1,PRJNPER; IF(SIM(KPER)%DELT.NE.0.0D0)ISS=1; ENDDO

 !## overwrite nstep/nmult in case imodbatch is used
 IF(IBATCH.EQ.1)THEN
  DO KPER=1,PRJNPER; SIM(KPER)%TMULT=PBMAN%NMULT; SIM(KPER)%NSTP=PBMAN%NSTEP; ENDDO
 ENDIF
 
 !## output unit numbers
 IHEDUN =51; IBCFCB =52; IRCHCB =53; IEVTCB =54; IDRNCB =55
 IRIVCB =56; IGHBCB =57; ICHDCB =58; IWELCB =59
 ISFRCB =60  !## output unit numbers for sfr package
 ISFRCB2=61  !## detailed output for sfr package
 IFHBCB =62  !## output fhb package
 ILAKCB =63  !## output lak package
 IUZFCB1=64  !## output uzg package
 IWL2CB =65  !## output mnw package
 
 !## get active packages
 IF(.NOT.PMANAGER_GETPACKAGES())RETURN
 !## organise groups
 CALL IPEST_GLM_SETGROUPS()
 !## write nam file
 IF(.NOT.PMANAGER_SAVEMF2005_NAM(FNAME,MAINDIR,DIR,DIRMNAME,IPRT,ISS))RETURN
 !## get area of simulation / allocate arrays
 IF(.NOT.PMANAGER_SAVEMF2005_SIM(ISS,IBATCH))RETURN
 !## write meta-data file
 IF(.NOT.PMANAGER_SAVEMF2005_MET(DIR,DIRMNAME))RETURN
 !## write time-discretisation file
 IF(.NOT.PMANAGER_SAVEMF2005_TDIS(TRIM(MAINDIR)//'\MFSIM'))RETURN

 !##================
 !## reading section
 !##================
 
 !## read bnd/shd files
 IF(.NOT.PMANAGER_SAVEMF2005_BAS_READ(IPRT))RETURN
 !## read top/bot information
 IF(.NOT.PMANAGER_SAVEMF2005_DIS_READ(LTB,IPRT))RETURN
 !## read bcf
 IF(.NOT.PMANAGER_SAVEMF2005_BCF_READ(ISS,IPRT))RETURN
 !## read lpf
 IF(.NOT.PMANAGER_SAVEMF2005_LPF_READ(ISS,IPRT))RETURN
 !## compute kdw/vcw
 CALL PMANAGER_SAVEMF2005_COMPUTE_KDW_VCW()
 !## read ani
 IF(.NOT.PMANAGER_SAVEMF2005_ANI_READ(IPRT))RETURN
 !## read top/bot information
 IF(.NOT.PMANAGER_SAVEMF2005_LAK_READ(0,IPRT,INIPER))RETURN
 !## read top/kh information
 IF(.NOT.PMANAGER_SAVEMF2005_SFT_READ(IPRT))RETURN

 !##================
 !## checking section
 !##================

 !## apply consistency checks
 CALL PMANAGER_SAVEMF2005_CONSISTENCY(LTB)
 !## recompute kdw/vcw
 CALL PMANAGER_SAVEMF2005_COMPUTE_KDW_VCW()
 !## get lak position and conductances
 IF(.NOT.PMANAGER_SAVEMF2005_LAK_CONFIG())RETURN

 !##================
 !## writing section
 !##================

 !## write pst-file
 IF(.NOT.PMANAGER_SAVEMF2005_PST_READWRITE(DIR,DIRMNAME,IBATCH,ISS))RETURN
 !## write metaswap
 IF(.NOT.PMANAGER_SAVEMF2005_MSP(DIR,DIRMNAME,IBATCH,IPRT))RETURN 
 !## save bas file
 IF(.NOT.PMANAGER_SAVEMF2005_BAS_SAVE(DIR,DIRMNAME,IBATCH))RETURN
 !## save ic file
 IF(.NOT.PMANAGER_SAVEMF2005_IC_SAVE(DIR,DIRMNAME,IBATCH))RETURN
 !## save dis file
 IF(.NOT.PMANAGER_SAVEMF2005_DIS_SAVE(DIR,DIRMNAME,IBATCH))RETURN
 !## save bcf file
 IF(.NOT.PMANAGER_SAVEMF2005_BCF_SAVE(DIR,DIRMNAME,IBATCH,ISS))RETURN
 !## save lpf file
 IF(.NOT.PMANAGER_SAVEMF2005_LPF_SAVE(DIR,DIRMNAME,IBATCH,ISS))RETURN
 !## save npf file
 IF(.NOT.PMANAGER_SAVEMF2005_NPF_SAVE(DIR,DIRMNAME,IBATCH))RETURN
 !## save sto file
 IF(.NOT.PMANAGER_SAVEMF2005_STO_SAVE(DIR,DIRMNAME,IBATCH,ISS))RETURN
 !## save ani file
 IF(.NOT.PMANAGER_SAVEMF2005_ANI_SAVE(DIR,DIRMNAME,IBATCH))RETURN
 !## save hfb file
 IF(.NOT.PMANAGER_SAVEMF2005_HFB(IBATCH,DIRMNAME,IPRT,LTB))RETURN
 !## save pcg file
 IF(.NOT.PMANAGER_SAVEMF2005_IMS(TRIM(MAINDIR)//'\MFSIM'))RETURN
 !## save pcg file
 IF(.NOT.PMANAGER_SAVEMF2005_PCG(DIRMNAME))RETURN
 !## save pks file
 IF(.NOT.PMANAGER_SAVEMF2005_PKS(DIRMNAME))RETURN
 !## save oc file
 IF(.NOT.PMANAGER_SAVEMF2005_OCD(DIRMNAME,MAINDIR))RETURN
 
 !## save uzf package
 IF(.NOT.PMANAGER_SAVEMF2005_PCK(DIR,DIRMNAME,IBATCH,LUZF,TUZF,IUZFCB1,'UZF',(/1,2,3,4,5,6,7,8/),IPRT))RETURN
 !## save mnw package
 IF(.NOT.PMANAGER_SAVEMF2005_MNW(DIRMNAME,IBATCH,LMNW,TMNW,IWL2CB,'MNW',IPRT))RETURN
 !## save wel package
 IF(.NOT.PMANAGER_SAVEMF2005_WEL(DIR,DIRMNAME,IBATCH,LWEL,TWEL,IWELCB,'WEL',IPRT))RETURN
 !## save drn package
 IF(.NOT.PMANAGER_SAVEMF2005_PCK(DIR,DIRMNAME,IBATCH,LDRN,TDRN,IDRNCB,'DRN',(/2,1/),IPRT))RETURN
 !## save isg package (always before riv in case of dmm-files)
 IF(.NOT.LRIV)THEN
  IF(.NOT.PMANAGER_SAVEMF2005_ISG(DIR,DIRMNAME,IBATCH,LISG,TISG,IRIVCB,'RIV',IPRT))RETURN
 ELSE
  IF(.NOT.PMANAGER_SAVEMF2005_ISG(DIR,DIRMNAME,IBATCH,LISG,TISG,IRIVCB,'ISG',IPRT))RETURN
 ENDIF
 !## save riv package
 IF(.NOT.PMANAGER_SAVEMF2005_PCK(DIR,DIRMNAME,IBATCH,LRIV,TRIV,IRIVCB,'RIV',(/2,1,3,4/),IPRT))RETURN
 !## save evt package
 IF(.NOT.PMANAGER_SAVEMF2005_PCK(DIR,DIRMNAME,IBATCH,LEVT,TEVT,IEVTCB,'EVT',(/2,1,3/),IPRT))RETURN
 !## save ghb package
 IF(.NOT.PMANAGER_SAVEMF2005_PCK(DIR,DIRMNAME,IBATCH,LGHB,TGHB,IGHBCB,'GHB',(/2,1/),IPRT))RETURN
 !## save rch package
 IF(.NOT.PMANAGER_SAVEMF2005_PCK(DIR,DIRMNAME,IBATCH,LRCH,TRCH,IRCHCB,'RCH',(/1/),IPRT))RETURN
 !## save olf package
 IF(.NOT.LDRN)THEN
  IF(.NOT.PMANAGER_SAVEMF2005_PCK(DIR,DIRMNAME,IBATCH,LOLF,TOLF,IDRNCB,'DRN',(/1/),IPRT))RETURN
 ELSE
  IF(.NOT.PMANAGER_SAVEMF2005_PCK(DIR,DIRMNAME,IBATCH,LOLF,TOLF,IDRNCB,'OLF',(/1/),IPRT))RETURN
 ENDIF
 !## save chd package
 IF(.NOT.PMANAGER_SAVEMF2005_PCK(DIR,DIRMNAME,IBATCH,LCHD,TCHD,ICHDCB,'CHD',(/1/),IPRT))RETURN
 !## save sfr package
 IF(.NOT.PMANAGER_SAVEMF2005_ISG(DIR,DIRMNAME,IBATCH,LSFR,TSFR,ISFRCB,'SFR',IPRT))RETURN
 !## save fhb package
 IF(.NOT.PMANAGER_SAVEMF2005_PCK(DIR,DIRMNAME,IBATCH,LFHB,TFHB,IFHBCB,'FHB',(/1,2/),IPRT))RETURN
 IF(LLAK)THEN
  !## save rest of lak package
  LPER=0; DO IPER=1,PRJNPER
   !## get appropriate stress-period to store in runfile   
   KPER=PMANAGER_GETCURRENTIPER(IPER,32,ITIME,JTIME)
   !## kper is stress period for which lakes are firstly defined
   IINI=0; IF(KPER.EQ.INIPER)IINI=1
   !## read in new values in case not previous one can be used
   IF(ABS(KPER).NE.LPER)THEN
    KPER=ABS(KPER)
    IF(.NOT.PMANAGER_SAVEMF2005_LAK_READ(IPER,IPRT,KPER))RETURN
   ENDIF
   IF(.NOT.PMANAGER_SAVEMF2005_LAK_SAVE(IULAK,IINI,IBATCH,DIR,KPER=IPER,DIRMNAME=DIRMNAME))RETURN
   !## store previous stress-period information for this timestep
   LPER=ABS(KPER)
  ENDDO
  CLOSE(IULAK)
 ENDIF
  
 !## combine olf/drn and isg/riv
 IF(LOLF.AND.LDRN)THEN
  IF(PBMAN%ICONCHK.EQ.0)THEN
   IF(.NOT.PMANAGER_SAVEMF2005_COMBINE(DIR,DIRMNAME,(/'OLF','DRN','DRN_'/),IDRNCB,'AUX ISUB DSUBSYS ISUB NOPRINT'))RETURN
  ELSE
   IF(.NOT.PMANAGER_SAVEMF2005_COMBINE(DIR,DIRMNAME,(/'OLF','DRN','DRN_'/),IDRNCB,'AUX ISUB DSUBSYS ISUB ICONCHK IC NOPRINT'))RETURN
  ENDIF
 ENDIF
 IF(LISG.AND.LRIV)THEN
  IF(.NOT.PMANAGER_SAVEMF2005_COMBINE(DIR,DIRMNAME,(/'ISG','RIV','RIV_'/),IRIVCB,'AUX RFCT AUX ISUB RFACT RFCT RSUBSYS ISUB NOPRINT'))RETURN
 ENDIF
 
 !## create connections
 IF(PBMAN%IFORMAT.EQ.3.AND.PBMAN%ISUBMODEL.EQ.PBMAN%NSUBMODEL)THEN
  DO; I=LEN_TRIM(MAINDIR); IF(MAINDIR(I:I).NE.'\')EXIT; MAINDIR(I:I)=' '; ENDDO
  DO I=1,PBMAN%NSUBMODEL
   DO J=I+1,PBMAN%NSUBMODEL
    CALL PMANAGER_SAVEMF6_EXG(MAINDIR,I,J)
   ENDDO
  ENDDO
  !## remove from nam if no packages exists anymore
  DO I=1,PBMAN%NSUBMODEL
   CALL PMANAGER_SAVEMF6_CLEANNAM(MAINDIR,I)
  ENDDO
 ENDIF
 
 PMANAGER_SAVEMF2005=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005

 !###======================================================================
 SUBROUTINE PMANAGER_SAVEMF6_CLEANNAM(DIR,M)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER,INTENT(IN) :: M
 INTEGER :: IU,JU,KU,IOS,N
 CHARACTER(LEN=256) :: FNAME,LINE,STRING
 CHARACTER(LEN=52) :: MDLNAME
 CHARACTER(LEN=4),DIMENSION(6) :: PCK
 LOGICAL :: LEX
 DATA PCK/'CHD6','WEL6','DRN6','RCH6','RIV6','HFB6'/
 
 MDLNAME=DIR(INDEX(DIR,'\',.TRUE.)+1:)
 FNAME=TRIM(DIR)//'\GWF_'//TRIM(ITOS(M))//'\'//TRIM(MDLNAME)//'.NAM'
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
 JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=TRIM(FNAME)//'_',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(JU.EQ.0)THEN; CLOSE(IU); RETURN; ENDIF
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  WRITE(JU,'(A)') TRIM(LINE)
  IF(TRIM(LINE).EQ.'BEGIN PACKAGES')THEN
   DO
    READ(IU,'(A256)',IOSTAT=IOS) LINE
    IF(IOS.NE.0)EXIT
    LEX=.FALSE.;
    DO I=1,SIZE(PCK)
     IF(INDEX(LINE,PCK(I)).GT.0)THEN
      !## check whether there are packages defined
      FNAME=TRIM(DIR)//'\GWF_'//TRIM(ITOS(M))//'\MODELINPUT\'//TRIM(MDLNAME)//'.'//TRIM(PCK(I))
      KU=UTL_GETUNIT(); CALL OSD_OPEN(KU,FILE=FNAME,STATUS='OLD',ACTION='READ',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
      LEX=.TRUE.
      DO
       READ(KU,'(A256)') STRING
       IF(INDEX(STRING,'MAXBOUND').GT.0)THEN
        READ(STRING(9:),*) N
        IF(N.GT.0)WRITE(JU,'(A)') TRIM(LINE)
        EXIT
       ENDIF
       IF(INDEX(STRING,'MAXHFB').GT.0)THEN
        READ(STRING(7:),*) N
        IF(N.GT.0)WRITE(JU,'(A)') TRIM(LINE)
        EXIT
       ENDIF
      ENDDO
      CLOSE(KU)
     ENDIF
    ENDDO
    IF(.NOT.LEX)WRITE(JU,'(A)') TRIM(LINE)
   ENDDO
  ENDIF
 ENDDO
 CLOSE(IU,STATUS='DELETE'); CLOSE(JU)
 
 FNAME=TRIM(DIR)//'\GWF_'//TRIM(ITOS(M))//'\'//TRIM(MDLNAME)//'.NAM'
 CALL IOSRENAMEFILE(TRIM(FNAME)//'_',FNAME)
 
 END SUBROUTINE PMANAGER_SAVEMF6_CLEANNAM
 
 !###======================================================================
 SUBROUTINE PMANAGER_SAVEMF6_EXG(DIR,M1,M2)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER,INTENT(IN) :: M1,M2
 REAL(KIND=DP_KIND) :: XP,YP,T,B,Z1,Z2
 INTEGER :: IU,JU,I,J,K,IM,N,IOS,II,ILAY,MAXNLAY,IROW,ICOL,IMDL1,IMDL2,JROW,JCOL
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:,:) :: BND,TOP,BOT
 INTEGER,DIMENSION(2) :: MNLAY,IMDL
 CHARACTER(LEN=256) :: FNAME,LINE
 CHARACTER(LEN=52) :: TXT,MDLNAME
 CHARACTER(LEN=1) :: TLAYMODEL
 LOGICAL :: LSUBMODEL,LEX
 
 MDLNAME=DIR(INDEX(DIR,'\',.TRUE.)+1:)
 
 FNAME=TRIM(DIR)//'\MFSIM_M'//TRIM(ITOS(M1))//'_M'//TRIM(ITOS(M2))//'.EXG'
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
 WRITE(IU,'(A)') '# '//TRIM(FNAME(INDEX(FNAME,'\',.TRUE.)+1:))//' File Generated by '//TRIM(UTL_IMODVERSION())
 WRITE(IU,'(/A/)') '#General Options'
 WRITE(IU,'(A)') 'BEGIN OPTIONS'
 WRITE(IU,'(1X,A)') 'PRINT_INPUT'
 WRITE(IU,'(1X,A)') 'PRINT_FLOWS'
 WRITE(IU,'(1X,A)') 'SAVE_FLOWS'
! WRITE(IU,'(1X,A)') 'HARMONIC' 
! WRITE(IU,'(A)') '[VARIABLECV [DEWATERED]]'
! WRITE(IU,'(A)') '[NEWTON]'
! WRITE(IU,'(A)') '[GNC6 FILEIN <gnc6_filename>]' !## ghost-node correction
! WRITE(IU,'(A)') '[MVR6 FILEIN <mvr6_filename>]' !## water mover
! WRITE(IU,'(A)') '[OBS6 FILEIN <obs6_filename>]' !## observation
 WRITE(IU,'(A)') 'END OPTIONS'

 !## read boundary-files + top/bottom = summary file with bnd/top/bot
 DO II=1,2
  MAXNLAY=0
  DO IM=1,2
   JU=UTL_GETUNIT()
   IF(IM.EQ.1)OPEN(JU,FILE=TRIM(DIR)//'\GWF_'//TRIM(ITOS(M1))//'\MODELINPUT\'//TRIM(MDLNAME)//'.DIS6',STATUS='OLD',ACTION='READ')
   IF(IM.EQ.2)OPEN(JU,FILE=TRIM(DIR)//'\GWF_'//TRIM(ITOS(M2))//'\MODELINPUT\'//TRIM(MDLNAME)//'.DIS6',STATUS='OLD',ACTION='READ')
   DO 
    READ(JU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
    IF(INDEX(LINE,'BEGIN DIMENSIONS').GT.0)THEN
     READ(JU,*) TXT,MNLAY(IM); MAXNLAY=MAX(MAXNLAY,MNLAY(IM))
     IF(II.EQ.2)THEN
      IF(IM.EQ.1)THEN
       DO I=1,1;         IF(.NOT.IDFREAD(TOP(1,I),TRIM(DIR)//'\GWF_'//TRIM(ITOS(M1))//'\MODELINPUT\DIS6\TOP_L'//TRIM(ITOS(I  ))//'.IDF',1))RETURN; ENDDO
       DO I=2,MNLAY(IM); IF(.NOT.IDFREAD(TOP(1,I),TRIM(DIR)//'\GWF_'//TRIM(ITOS(M1))//'\MODELINPUT\DIS6\BOT_L'//TRIM(ITOS(I-1))//'.IDF',1))RETURN; ENDDO
       DO I=1,MNLAY(IM); IF(.NOT.IDFREAD(BND(1,I),TRIM(DIR)//'\GWF_'//TRIM(ITOS(M1))//'\MODELINPUT\DIS6\BND_L'//TRIM(ITOS(I  ))//'.IDF',1))RETURN; ENDDO
       DO I=1,MNLAY(IM); IF(.NOT.IDFREAD(BOT(1,I),TRIM(DIR)//'\GWF_'//TRIM(ITOS(M1))//'\MODELINPUT\DIS6\BOT_L'//TRIM(ITOS(I  ))//'.IDF',1))RETURN; ENDDO
      ELSE
       DO I=1,1;         IF(.NOT.IDFREAD(TOP(2,I),TRIM(DIR)//'\GWF_'//TRIM(ITOS(M2))//'\MODELINPUT\DIS6\TOP_L'//TRIM(ITOS(I  ))//'.IDF',1))RETURN; ENDDO
       DO I=2,MNLAY(IM); IF(.NOT.IDFREAD(TOP(2,I),TRIM(DIR)//'\GWF_'//TRIM(ITOS(M2))//'\MODELINPUT\DIS6\BOT_L'//TRIM(ITOS(I-1))//'.IDF',1))RETURN; ENDDO
       DO I=1,MNLAY(IM); IF(.NOT.IDFREAD(BND(2,I),TRIM(DIR)//'\GWF_'//TRIM(ITOS(M2))//'\MODELINPUT\DIS6\BND_L'//TRIM(ITOS(I  ))//'.IDF',1))RETURN; ENDDO
       DO I=1,MNLAY(IM); IF(.NOT.IDFREAD(BOT(2,I),TRIM(DIR)//'\GWF_'//TRIM(ITOS(M2))//'\MODELINPUT\DIS6\BOT_L'//TRIM(ITOS(I  ))//'.IDF',1))RETURN; ENDDO
      ENDIF
      EXIT
     ENDIF
    ENDIF
   ENDDO
   CLOSE(JU)
  ENDDO
  IF(II.EQ.1)THEN
   ALLOCATE(BND(2,MAXNLAY),TOP(2,MAXNLAY),BOT(2,MAXNLAY))
   DO I=1,SIZE(TOP,1); DO J=1,SIZE(TOP,2); CALL IDFNULLIFY(TOP(I,J)); ENDDO; ENDDO
   DO I=1,SIZE(BOT,1); DO J=1,SIZE(BOT,2); CALL IDFNULLIFY(BOT(I,J)); ENDDO; ENDDO
   DO I=1,SIZE(BND,1); DO J=1,SIZE(BND,2); CALL IDFNULLIFY(BND(I,J)); ENDDO; ENDDO
  ENDIF
 ENDDO

 !## correct the idomain
 DO K=1,2; DO ILAY=1,MNLAY(K); DO IROW=1,BND(K,1)%NROW; DO ICOL=1,BND(K,1)%NCOL
  BND(K,ILAY)%X(ICOL,IROW)=MIN(1.0D0,BND(K,ILAY)%X(ICOL,IROW))
  !## inactive
  IF(BND(K,ILAY)%X(ICOL,IROW).EQ.0.0D0)BND(K,ILAY)%X(ICOL,IROW)=BND(K,ILAY)%NODATA
  !## vertically inactive idomain.le.0
  IF(BND(K,ILAY)%X(ICOL,IROW).LT.0.0D0)BND(K,ILAY)%X(ICOL,IROW)=BND(K,ILAY)%NODATA
 ENDDO; ENDDO; ENDDO; ENDDO
 
 !## who is smallest in cellsize and/or dimension
 IMDL1=1; IMDL2=2; LSUBMODEL=.FALSE.; TLAYMODEL=''; IMDL(IMDL1)=M1; IMDL(IMDL2)=M2
 !## check size first
 IF(BND(2,1)%XMIN.GT.BND(1,1)%XMIN.AND. &
    BND(2,1)%XMAX.LT.BND(1,1)%XMAX.AND. &
    BND(2,1)%YMIN.GT.BND(1,1)%YMIN.AND. &
    BND(2,1)%YMAX.LT.BND(1,1)%YMAX)THEN
  !## throw an error in the case a submodel, is coarser - not supported
  IF(BND(2,1)%DX.GT.BND(1,1)%DX)THEN
   WRITE(*,'(/A/)') 'A submodel need to have at least a cellsize which is equal or smaller than the overlapping model'; STOP 
  ENDIF
  IMDL1=2; IMDL2=1; IMDL(IMDL1)=M2; IMDL(IMDL2)=M1; LSUBMODEL=.TRUE.
 !## check size second
 ELSEIF(BND(1,1)%XMIN.GT.BND(2,1)%XMIN.AND. &
    BND(1,1)%XMAX.LT.BND(2,1)%XMAX.AND. &
    BND(1,1)%YMIN.GT.BND(2,1)%YMIN.AND. &
    BND(1,1)%YMAX.LT.BND(2,1)%YMAX)THEN
  !## throw an error in the case a submodel, is coarser - not supported
  IF(BND(1,1)%DX.GT.BND(2,1)%DX)THEN
   WRITE(*,'(/A/)') 'A submodel need to have at least a cellsize which is equal or smaller than the overlapping model'; STOP 
  ENDIF
  LSUBMODEL=.TRUE.
 !## if not, equal model size but different layers
 ELSEIF(BND(2,1)%XMIN.EQ.BND(1,1)%XMIN.AND. &
    BND(2,1)%XMAX.EQ.BND(1,1)%XMAX.AND. &
    BND(2,1)%YMIN.EQ.BND(1,1)%YMIN.AND. &
    BND(2,1)%YMAX.EQ.BND(1,1)%YMAX)THEN
  IF(BND(2,1)%DX.LT.BND(1,1)%DX)THEN
   IMDL1=2; IMDL2=1; IMDL(IMDL1)=M2; IMDL(IMDL2)=M1
  ENDIF
  !## determine whether submodel is on top or bottom
  DO IROW=1,BND(IMDL1,1)%NROW; DO ICOL=1,BND(IMDL1,1)%NCOL
   IF(BND(IMDL1,1)%X(ICOL,IROW).EQ.1)THEN
    T=TOP(IMDL1,1)%X(ICOL,IROW)
    B=BOT(IMDL1,1)%X(ICOL,IROW)
    Z1=B+0.5D0*(T-B)
    !## get z from other model
    CALL IDFGETLOC(BND(IMDL1,1),IROW,ICOL,XP,YP)
    CALL IDFIROWICOL(BND(IMDL2,1),JROW,JCOL,XP,YP)
    !## outside parent model
    IF(JROW.LE.0.OR.JCOL.LE.0)RETURN
    T=TOP(IMDL2,1)%X(JCOL,JROW)
    B=BOT(IMDL2,1)%X(JCOL,JROW)
    Z2=B+0.5D0*(T-B)
    IF(Z2.GT.Z1)THEN
     IF(TLAYMODEL.EQ.'')THEN
      TLAYMODEL='T' !## other model is on top
     ELSEIF(TLAYMODEL.NE.'T')THEN
      WRITE(*,'(/1X,A/)') 'Vertical TOP inconsistency between two submodels'; STOP
     ENDIF
    ELSEIF(Z2.LT.Z1)THEN
     IF(TLAYMODEL.EQ.'')THEN
      TLAYMODEL='B' !## other model is at bottom
     ELSEIF(TLAYMODEL.NE.'B')THEN
      WRITE(*,'(/1X,A/)') 'Vertical BOT inconsistency between two submodels'; STOP
     ENDIF
    ENDIF
   ENDIF
  ENDDO; ENDDO
  IF(TLAYMODEL.EQ.'')THEN
   WRITE(*,'(/1X,A/)') 'Cannot position model horizontally or vertically'; STOP
  ENDIF
 ENDIF
 
 DO I=1,2
  N=0

  DO ILAY=1,MNLAY(IMDL1)
 
   IF(LSUBMODEL)THEN

    !## isubmodel 1 en isubmodel 2 komen uit grid

    !## north connection
    IF(I.EQ.2)WRITE(IU,'(/A)') '# North Cell Connections'
    DO IROW=1,BND(IMDL1,ILAY)%NROW; DO ICOL=1,BND(IMDL1,ILAY)%NCOL
     !## skip inactive cells
     IF(BND(IMDL1,ILAY)%X(ICOL,IROW).EQ.BND(IMDL1,ILAY)%NODATA)CYCLE
     !## found boundary cell
     LEX=.FALSE.; IF(IROW.EQ.1)LEX=.TRUE.
     IF(IROW.GT.1)THEN; LEX=BND(IMDL1,ILAY)%X(ICOL,IROW-1).EQ.BND(IMDL1,ILAY)%NODATA; ENDIF
     IF(LEX)THEN
      IF(PMANAGER_SAVEMF6_EXG_CONNECTIONS('N',IU,ILAY,IROW,ICOL,IMDL1,IMDL2,MNLAY(IMDL1),MNLAY(IMDL2), &
         BND(IMDL1,ILAY),BND(IMDL2,ILAY),TOP(IMDL1,ILAY),BOT(IMDL1,ILAY),TOP(IMDL2,ILAY),BOT(IMDL2,ILAY),I))N=N+1
     ENDIF
    ENDDO; ENDDO
   
    !## south connection
    IF(I.EQ.2)WRITE(IU,'(/A)') '# South Cell Connections'
    DO IROW=1,BND(IMDL1,ILAY)%NROW; DO ICOL=1,BND(IMDL1,ILAY)%NCOL
     !## skip inactive cells
     IF(BND(IMDL1,ILAY)%X(ICOL,IROW).EQ.BND(IMDL1,ILAY)%NODATA)CYCLE
     !## found boundary cell
     LEX=.FALSE.; IF(IROW.EQ.BND(IMDL1,ILAY)%NROW)LEX=.TRUE.
     IF(IROW.LT.BND(IMDL1,ILAY)%NROW)THEN; LEX=BND(IMDL1,ILAY)%X(ICOL,IROW+1).EQ.BND(IMDL1,ILAY)%NODATA; ENDIF
     IF(LEX)THEN
      IF(PMANAGER_SAVEMF6_EXG_CONNECTIONS('S',IU,ILAY,IROW,ICOL,IMDL1,IMDL2,MNLAY(IMDL1),MNLAY(IMDL2), &
         BND(IMDL1,ILAY),BND(IMDL2,ILAY),TOP(IMDL1,ILAY),BOT(IMDL1,ILAY),TOP(IMDL2,ILAY),BOT(IMDL2,ILAY),I))N=N+1
     ENDIF
    ENDDO; ENDDO

    !## west connection
    IF(I.EQ.2)WRITE(IU,'(/A)') '# West Cell Connections'
    DO IROW=1,BND(IMDL1,ILAY)%NROW; DO ICOL=1,BND(IMDL1,ILAY)%NCOL
     !## skip inactive cells
     IF(BND(IMDL1,ILAY)%X(ICOL,IROW).EQ.BND(IMDL1,ILAY)%NODATA)CYCLE
     !## found boundary cell
     LEX=.FALSE.; IF(ICOL.EQ.1)LEX=.TRUE.
     IF(ICOL.GT.1)THEN; LEX=BND(IMDL1,ILAY)%X(ICOL-1,IROW).EQ.BND(IMDL1,ILAY)%NODATA; ENDIF
     IF(LEX)THEN
      IF(PMANAGER_SAVEMF6_EXG_CONNECTIONS('W',IU,ILAY,IROW,ICOL,IMDL1,IMDL2,MNLAY(IMDL1),MNLAY(IMDL2), &
         BND(IMDL1,ILAY),BND(IMDL2,ILAY),TOP(IMDL1,ILAY),BOT(IMDL1,ILAY),TOP(IMDL2,ILAY),BOT(IMDL2,ILAY),I))N=N+1
     ENDIF
    ENDDO; ENDDO

    !## east connection
    IF(I.EQ.2)WRITE(IU,'(/A)') '# East Cell Connections'
    DO IROW=1,BND(IMDL1,ILAY)%NROW; DO ICOL=1,BND(IMDL1,ILAY)%NCOL
     !## skip inactive cells
     IF(BND(IMDL1,ILAY)%X(ICOL,IROW).EQ.BND(IMDL1,ILAY)%NODATA)CYCLE
     !## found boundary cell
     LEX=.FALSE.; IF(ICOL.EQ.BND(IMDL1,ILAY)%NCOL)LEX=.TRUE.
     IF(ICOL.LT.BND(IMDL1,ILAY)%NCOL)THEN; LEX=BND(IMDL1,ILAY)%X(ICOL+1,IROW).EQ.BND(IMDL1,ILAY)%NODATA; ENDIF
     IF(LEX)THEN
      IF(PMANAGER_SAVEMF6_EXG_CONNECTIONS('E',IU,ILAY,IROW,ICOL,IMDL1,IMDL2,MNLAY(IMDL1),MNLAY(IMDL2), &
         BND(IMDL1,ILAY),BND(IMDL2,ILAY),TOP(IMDL1,ILAY),BOT(IMDL1,ILAY),TOP(IMDL2,ILAY),BOT(IMDL2,ILAY),I))N=N+1
     ENDIF
    ENDDO; ENDDO
     
   ENDIF
   
  ENDDO 
  
  !## define connection from top-bottom
  IF(TRIM(TLAYMODEL).NE.'')THEN
   DO IROW=1,BND(IMDL1,1)%NROW; DO ICOL=1,BND(IMDL1,1)%NCOL
    IF(TLAYMODEL.EQ.'T')THEN
     IF(PMANAGER_SAVEMF6_EXG_CONNECTIONS(TLAYMODEL,IU,1,IROW,ICOL,IMDL1,IMDL2,MNLAY(IMDL1),MNLAY(IMDL2), &
        BND(IMDL1,1),BND(IMDL2,MNLAY(IMDL2)),TOP(IMDL1,1),BOT(IMDL1,1),TOP(IMDL2,MNLAY(IMDL2)),BOT(IMDL2,MNLAY(IMDL2)),I))N=N+1
    ELSEIF(TLAYMODEL.EQ.'B')THEN
     IF(PMANAGER_SAVEMF6_EXG_CONNECTIONS(TLAYMODEL,IU,MNLAY(IMDL1),IROW,ICOL,IMDL1,IMDL2,MNLAY(IMDL1),MNLAY(IMDL2), &
        BND(IMDL1,MNLAY(IMDL1)),BND(IMDL2,1),TOP(IMDL1,MNLAY(IMDL1)),BOT(IMDL1,MNLAY(IMDL1)),TOP(IMDL2,1),BOT(IMDL2,1),I))N=N+1
    ENDIF
   ENDDO; ENDDO
  ENDIF
  
  IF(I.EQ.1)THEN
   WRITE(IU,'(/A/)') '#Dimensions'
   WRITE(IU,'(A)') 'BEGIN DIMENSIONS'
   WRITE(IU,'(1X,A)') 'NEXG '//TRIM(ITOS(N))
   WRITE(IU,'(A)') 'END DIMENSIONS'
   WRITE(IU,'(/A/)') '#Exchange Data'
   WRITE(IU,'(A)') 'BEGIN EXCHANGEDATA'
  ELSE
   WRITE(IU,'(/A)') 'END EXCHANGEDATA'
  ENDIF
 
 ENDDO

 CLOSE(IU)
 
 DO I=1,SIZE(BND,1); DO J=1,SIZE(BND,2); CALL IDFDEALLOCATEX(BND(I,J)); ENDDO; ENDDO
 DO I=1,SIZE(TOP,1); DO J=1,SIZE(TOP,2); CALL IDFDEALLOCATEX(TOP(I,J)); ENDDO; ENDDO
 DO I=1,SIZE(BOT,1); DO J=1,SIZE(BOT,2); CALL IDFDEALLOCATEX(BOT(I,J)); ENDDO; ENDDO
 DEALLOCATE(BND,TOP,BOT)
 
 END SUBROUTINE PMANAGER_SAVEMF6_EXG
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF6_EXG_CONNECTIONS(CDIR,IU,ILAY,IROW,ICOL,IMDL1,IMDL2,NLAY1,NLAY2,BND1,BND2,TOP1,BOT1,TOP2,BOT2,IIU)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: CDIR
 TYPE(IDFOBJ),INTENT(IN) :: BND1,BND2,TOP1,BOT1,TOP2,BOT2
 INTEGER,INTENT(IN) :: ILAY,IROW,ICOL,IMDL1,IMDL2,IU,IIU,NLAY1,NLAY2
 INTEGER,DIMENSION(2,3) :: CELLID
 INTEGER :: JROW,JCOL,JLAY,IHC,I 
 REAL(KIND=DP_KIND) :: HWVA,XP1,YP1,XP2,YP2,X1,X2,Y1,Y2,Z1,Z2,XP,YP,ZP1,ZP2,DX1,DX2,DY1,DY2,DZ1,DZ2, &
                       XINT1,YINT1,ZINT1,XINT2,YINT2,ZINT2
 REAL(KIND=DP_KIND),DIMENSION(2) :: CL

 PMANAGER_SAVEMF6_EXG_CONNECTIONS=.FALSE.
 
 !## current centre location of fine   model
 CALL IDFGETLOC( BND1,IROW,ICOL,XP1,YP1)
 CALL IDFGETEDGE(BND1,IROW,ICOL,X1 ,Y1 ,X2 ,Y2)
 !## get vertical position of node
 Z2=TOP1%X(ICOL,IROW); Z1=BOT1%X(ICOL,IROW)
 DZ1=Z2-Z1; ZP1=Z1+0.5D0*DZ1
 !## get cellsize of fine model
 CALL IDFGETDXDY(BND1,IROW,ICOL,DX1,DY1)

 !## get location of nearest course model
 SELECT CASE (CDIR)
  CASE ('N'); CALL IDFGETLOC( BND1,IROW-1,ICOL,XP,YP); IHC=1; JLAY=ILAY
  CASE ('S'); CALL IDFGETLOC( BND1,IROW+1,ICOL,XP,YP); IHC=1; JLAY=ILAY
  CASE ('W'); CALL IDFGETLOC( BND1,IROW,ICOL-1,XP,YP); IHC=1; JLAY=ILAY
  CASE ('E'); CALL IDFGETLOC( BND1,IROW,ICOL+1,XP,YP); IHC=1; JLAY=ILAY
  CASE ('T'); CALL IDFGETLOC( BND1,IROW,ICOL  ,XP,YP); IHC=0; JLAY=NLAY2
  CASE ('B'); CALL IDFGETLOC( BND1,IROW,ICOL  ,XP,YP); IHC=0; JLAY=1
 END SELECT
 CALL IDFIROWICOL(BND2,JROW,JCOL,XP,YP)
 !## outside parent model
 IF(JROW.LE.0.OR.JCOL.LE.0)RETURN
 
 !## active cell?
 IF(BND2%X(JCOL,JROW).EQ.BND2%NODATA)RETURN
 
 !## get location of cell outside submodel
 CALL IDFGETLOC(BND2,JROW,JCOL,XP2,YP2)
 !## get vertical position of node
 DZ2=TOP2%X(JCOL,JROW)-BOT2%X(JCOL,JROW)
 ZP2=BOT2%X(JCOL,JROW)+0.5D0*DZ2
 !## get cellsize of course model
 CALL IDFGETDXDY(BND2,JROW,JCOL,DX2,DY2)

 CELLID(IMDL1,1)=ILAY
 CELLID(IMDL1,2)=IROW
 CELLID(IMDL1,3)=ICOL
 CELLID(IMDL2,1)=JLAY
 CELLID(IMDL2,2)=JROW
 CELLID(IMDL2,3)=JCOL

 !## find point on shared interface
 SELECT CASE (CDIR)
  CASE ('W')
   XINT1=X1; YINT1=YP1
   XINT2=X1; YINT2=YP2
  CASE ('E')
   XINT1=X2; YINT1=YP1
   XINT2=X2; YINT2=YP2
  CASE ('N')
   XINT1=XP1; YINT1=Y2
   XINT2=XP2; YINT2=Y2
  CASE ('S')
   XINT1=XP1; YINT1=Y1
   XINT2=XP2; YINT2=Y1
  CASE ('T')
   ZINT1=Z2; XINT1=XP1; YINT1=YP1
   ZINT2=Z2; XINT2=XP2; YINT2=YP2
  CASE ('B')
   ZINT1=Z1; XINT1=XP1; YINT1=YP1
   ZINT2=Z1; XINT2=XP2; YINT2=YP2
 END SELECT

 !## area of connection in vertical
 HWVA=0.0D0

 !## width of connection
 IF(IHC.EQ.1)THEN

  !## distance to shared interface
  CL(IMDL1)=UTL_DIST(XP1,YP1,XINT1,YINT1)
  CL(IMDL2)=UTL_DIST(XP2,YP2,XINT2,YINT2)
  HWVA=X2-X1

 !## area of connection
 ELSEIF(IHC.EQ.0)THEN

  !## ook 2d denk ik ... gewoon recht naar het vlak toe
  CL(IMDL1)=UTL_DIST_3D(XP1,YP1,ZP1,XINT1,YINT1,ZINT1)
  CL(IMDL2)=UTL_DIST_3D(XP2,YP2,ZP2,XINT2,YINT2,ZINT2)
  HWVA=(X2-X1)*(Y2-Y1)

 ENDIF

 IF(IIU.EQ.2)WRITE(IU,'(7I10,4G15.7)') (CELLID(1,I),I=1,3),(CELLID(2,I),I=1,3),IHC,CL(1),CL(2),HWVA
     
 PMANAGER_SAVEMF6_EXG_CONNECTIONS=.TRUE.

 END FUNCTION PMANAGER_SAVEMF6_EXG_CONNECTIONS

 !###======================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_CONSISTENCY(LTB)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL,INTENT(IN) :: LTB
 INTEGER :: IROW,ICOL,ILAY,JLAY,N
 REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: TP,BT,HK,VK,VA,TP_BU,BT_BU,HK_BU,VK_BU,VA_BU
 REAL(KIND=DP_KIND),DIMENSION(:,:),ALLOCATABLE :: TH
 INTEGER,DIMENSION(:),ALLOCATABLE :: IB
 REAL(KIND=DP_KIND) :: ST,SB
 
 !## make sure nodata for anisotropy factors is 1.0D0
 IF(LANI)THEN
  !## apply consistency check anisotropy factor to be in between 0.0D0-1.0D0
  DO ILAY=1,PRJNLAY; DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
   ANF(ILAY)%X(ICOL,IROW)=MAX(0.0D0,MIN(1.0D0,ANF(ILAY)%X(ICOL,IROW)))
  ENDDO; ENDDO; ENDDO
 ENDIF
 
 !## clean from bottom to top inactive layers with zero conductance - in case of iconsistency.eq.2
 IF(PBMAN%ICONSISTENCY.NE.2)THEN
  DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
   DO ILAY=PRJNLAY,1,-1
    IF(KDW(ILAY)%X(ICOL,IROW).LE.0.0D0)THEN
     IF(ILAY.GT.1)VCW(ILAY-1)%X(ICOL,IROW)=0.0D0
     KDW(ILAY)%X(ICOL,IROW)=0.0D0   
     BND(ILAY)%X(ICOL,IROW)=0.0D0
    ELSE
     !## stop search for this location
     EXIT
    ENDIF
   ENDDO
  ENDDO; ENDDO
 ENDIF
 
 IF(.NOT.LTB)RETURN

 !## apply consistency check top/bot
 IF(PBMAN%ICONSISTENCY.EQ.1)THEN

  DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL; JLAY=0; DO ILAY=1,PRJNLAY
   IF(BND(ILAY)%X(ICOL,IROW).EQ.0)CYCLE

   SB=BOT(ILAY)%X(ICOL,IROW)
   ST=TOP(ILAY)%X(ICOL,IROW)
   SB=MIN(ST,SB)
   BOT(ILAY)%X(ICOL,IROW)=SB
   
   IF(JLAY.GT.0)THEN
    !## minimal aquifer thickness
    SB=BOT(JLAY)%X(ICOL,IROW)
    ST=TOP(ILAY)%X(ICOL,IROW)
    ST=MIN(SB,ST)
    TOP(ILAY)%X(ICOL,IROW)=ST
   ENDIF

   !## store last active layer
   JLAY=ILAY

  ENDDO; ENDDO; ENDDO

 ELSEIF(PBMAN%ICONSISTENCY.EQ.2)THEN
 
  IF(ALLOCATED(KHV).AND.ALLOCATED(KVA).AND.ALLOCATED(KVV))THEN

   ALLOCATE(TP(PRJNLAY)   ,BT(PRJNLAY)   ,HK(PRJNLAY)   ,VK(PRJNLAY)   ,VA(PRJNLAY)   ,IB(PRJNLAY),TH(PRJNLAY,2), &
            TP_BU(PRJNLAY),BT_BU(PRJNLAY),HK_BU(PRJNLAY),VK_BU(PRJNLAY),VA_BU(PRJNLAY))
   DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
    DO ILAY=1,PRJNLAY  ; IB(ILAY)=BND(ILAY)%X(ICOL,IROW); ENDDO
    DO ILAY=1,PRJNLAY  ; TP(ILAY)=TOP(ILAY)%X(ICOL,IROW); ENDDO
    DO ILAY=1,PRJNLAY  ; BT(ILAY)=BOT(ILAY)%X(ICOL,IROW); ENDDO
    DO ILAY=1,PRJNLAY  ; HK(ILAY)=KHV(ILAY)%X(ICOL,IROW); ENDDO
    DO ILAY=1,PRJNLAY  ; VA(ILAY)=KVA(ILAY)%X(ICOL,IROW); ENDDO
    VK=0.0D0; DO ILAY=1,PRJNLAY-1; VK(ILAY)=KVV(ILAY)%X(ICOL,IROW); ENDDO
    
!    if(icol.eq.114.and.irow.eq.1)then
!write(*,*) 
!    endif
    
    CALL UTL_MINTHICKNESS(TP,BT,HK,VK,VA,TP_BU,BT_BU,HK_BU,VK_BU,VA_BU,IB,TH,PBMAN%MINTHICKNESS,PRJNLAY,ICOL,IROW)
    DO ILAY=1,PRJNLAY  ; IB(ILAY)=BND(ILAY)%X(ICOL,IROW); ENDDO
    DO ILAY=1,PRJNLAY  ; TOP(ILAY)%X(ICOL,IROW)=TP(ILAY); ENDDO
    DO ILAY=1,PRJNLAY  ; BOT(ILAY)%X(ICOL,IROW)=BT(ILAY); ENDDO
    DO ILAY=1,PRJNLAY  ; KHV(ILAY)%X(ICOL,IROW)=HK(ILAY); ENDDO
    DO ILAY=1,PRJNLAY  ; KVA(ILAY)%X(ICOL,IROW)=VA(ILAY); ENDDO
    DO ILAY=1,PRJNLAY-1; KVV(ILAY)%X(ICOL,IROW)=VK(ILAY); ENDDO
    !## clean
    DO ILAY=1,PRJNLAY
     IF(IB(ILAY).EQ.0)THEN
      TOP(ILAY)%X(ICOL,IROW)=TOP(ILAY)%NODATA
      BOT(ILAY)%X(ICOL,IROW)=BOT(ILAY)%NODATA
      KHV(ILAY)%X(ICOL,IROW)=KHV(ILAY)%NODATA
      KVA(ILAY)%X(ICOL,IROW)=KVA(ILAY)%NODATA
      IF(ILAY.LT.PRJNLAY)KVV(ILAY)%X(ICOL,IROW)=KVV(ILAY)%NODATA
     ENDIF
    ENDDO
   ENDDO; ENDDO
   DEALLOCATE(TP,BT,HK,VK,VA,IB,TH,TP_BU,BT_BU,HK_BU,VK_BU,VA_BU)
  ENDIF
 ENDIF
 
 !## apply consistency check constant head and top/bot - only whenever CHD is not active
 IF(PBMAN%ICHKCHD.EQ.1)THEN
  N=0
  DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL; DO ILAY=1,PRJNLAY
   IF(BND(ILAY)%X(ICOL,IROW).LT.0)THEN
    !## head is in within current layer
    IF(SHD(ILAY)%X(ICOL,IROW).GT.BOT(ILAY)%X(ICOL,IROW))CYCLE
    N=N+1
    !## constant head cell dry - becomes active node - shift to an appropriate model layer where the head is actually in
    DO JLAY=ILAY,PRJNLAY
     IF(SHD(ILAY)%X(ICOL,IROW).LE.BOT(JLAY)%X(ICOL,IROW))THEN
      BND(JLAY)%X(ICOL,IROW)=1.0D0
      SHD(JLAY)%X(ICOL,IROW)=SHD(ILAY)%X(ICOL,IROW)
     ELSE
      BND(JLAY)%X(ICOL,IROW)=-99.0D0
      SHD(JLAY)%X(ICOL,IROW)=SHD(ILAY)%X(ICOL,IROW)
      !## exit
      EXIT
     ENDIF
    ENDDO
   ENDIF
  ENDDO; ENDDO; ENDDO
  WRITE(*,'(/A/)') 'iMOD corrected '//TRIM(ITOS(N))//' constant heads cell which were inappropriate regarding there levels.'
 ENDIF
 
 !## if unconfined modify (nodata) head for dry cells, check from bottom to top
 DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL; DO ILAY=PRJNLAY-1,1,-1
  IF(LAYCON(ILAY).NE.2)CYCLE
  IF(SHD(ILAY)%X(ICOL,IROW).EQ.HNOFLOW.AND.BND(ILAY)%X(ICOL,IROW).GT.0)THEN
   SHD(ILAY)%X(ICOL,IROW)=SHD(ILAY+1)%X(ICOL,IROW)
  ENDIF
 ENDDO; ENDDO; ENDDO 

 !## clean from bottom to top inactive layers with zero conductance
 IF(PBMAN%ICONSISTENCY.NE.2)THEN
  DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
   DO ILAY=PRJNLAY,1,-1
    IF(KDW(ILAY)%X(ICOL,IROW).LE.0.0D0)THEN
     IF(ILAY.GT.1)VCW(ILAY-1)%X(ICOL,IROW)=0.0D0
     KDW(ILAY)%X(ICOL,IROW)=0.0D0   
     BND(ILAY)%X(ICOL,IROW)=0.0D0
    ELSE
     !## stop search for this location
     EXIT
    ENDIF
   ENDDO
  ENDDO; ENDDO
 ENDIF
 
 END SUBROUTINE PMANAGER_SAVEMF2005_CONSISTENCY
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_NAM(FNAME,MAINDIR,DIR,DIRMNAME,IPRT,ISS)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ISS
 INTEGER,INTENT(OUT) :: IPRT
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=*),INTENT(OUT) :: DIR,DIRMNAME,MAINDIR
 INTEGER :: IU,I,J,N1,N2
 CHARACTER(LEN=52) :: MNAME
 CHARACTER(LEN=256) :: NAME
 
 PMANAGER_SAVEMF2005_NAM=.FALSE.
 
 !## result main folder
 IF(LEN_TRIM(PBMAN%OUTPUT).EQ.0)THEN
  MAINDIR=FNAME(:INDEX(FNAME,'\',.TRUE.)-1)
 ELSE
  MAINDIR=TRIM(PBMAN%OUTPUT)
 ENDIF
 MAINDIR=UTL_CAP(MAINDIR,'U'); CALL UTL_CREATEDIR(MAINDIR) 

 !## modelname
 MNAME=FNAME(INDEX(FNAME,'\',.TRUE.)+1:INDEX(FNAME,'.',.TRUE.)-1); MNAME=UTL_CAP(MNAME,'U')  

 !## write *.nam file for modflow 6
 IF(PBMAN%IFORMAT.EQ.3.AND.PBMAN%ISUBMODEL.EQ.1)THEN
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(MAINDIR)//'\MFSIM.NAM',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
  WRITE(IU,'(A)') '# MFSIM.NAM File Generated by '//TRIM(UTL_IMODVERSION())
  WRITE(IU,'(/A/)') '#General Options'
  WRITE(IU,'(A)') 'BEGIN OPTIONS'
![CONTINUE]
![NOCHECK]
![MEMORY_PRINT_OPTION <memory_print_option>]
  WRITE(IU,'(A)') 'END OPTIONS'
  WRITE(IU,'(/A/)') '#Timing Options'
  WRITE(IU,'(A)') 'BEGIN TIMING'
  WRITE(IU,'(A)') '  TDIS6 .\MFSIM.TDIS6'
  WRITE(IU,'(A)') 'END TIMING'
  WRITE(IU,'(/A/)') '#List of Models'
  WRITE(IU,'(A)') 'BEGIN MODELS'
  !## multiply models
  DO I=1,PBMAN%NSUBMODEL
   WRITE(IU,'(A)') ' GWF6 .\GWF_'//TRIM(ITOS(I))//'\'//TRIM(MNAME)//'.NAM GWF_'//TRIM(ITOS(I))
  ENDDO
  WRITE(IU,'(A)') 'END MODELS'
  WRITE(IU,'(/A/)') '#List of Exchanges'
  WRITE(IU,'(A)') 'BEGIN EXCHANGES'
  DO I=1,PBMAN%NSUBMODEL
   DO J=I+1,PBMAN%NSUBMODEL
    WRITE(IU,'(A)') ' GWF6-GWF6 .\MFSIM_M'//TRIM(ITOS(I))//'_M'//TRIM(ITOS(J))//'.EXG GWF_'//TRIM(ITOS(I))//' GWF_'//TRIM(ITOS(J))
   ENDDO
  ENDDO
  WRITE(IU,'(A)') 'END EXCHANGES'
  WRITE(IU,'(/A/)') '#Definition of Numerical Solution'
  WRITE(IU,'(A)') 'BEGIN SOLUTIONGROUP 1'
  WRITE(IU,'(A)') ' MXITER 1'
  WRITE(IU,'(A,99A)') ' IMS6 .\MFSIM.IMS6',(' GWF_'//TRIM(ITOS(I)),I=1,PBMAN%NSUBMODEL)
  WRITE(IU,'(A)') 'END SOLUTIONGROUP'
  CLOSE(IU)

 ENDIF
 
 !## loop over multiply models
 DIR=MAINDIR; IF(PBMAN%IFORMAT.EQ.3)DIR=TRIM(MAINDIR)//'\GWF_'//TRIM(ITOS(PBMAN%ISUBMODEL))

 !## result folder including the modelname
 DIRMNAME='MODELINPUT\'//TRIM(MNAME)
 
 CALL UTL_CREATEDIR(TRIM(DIR)//'\MODELINPUT') 
 IF(LMSP)CALL UTL_CREATEDIR(TRIM(DIR)//'\MSWAPINPUT') 

 IF(PBMAN%IFORMAT.EQ.3)THEN

  DIRMNAME='GWF_'//TRIM(ITOS(PBMAN%ISUBMODEL))//'\'//TRIM(DIRMNAME)
  DIRMNAME='.\'//TRIM(DIRMNAME)

  !## write *.nam file
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIR)//'\'//TRIM(MNAME)//'.NAM',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
  WRITE(IU,'(A)') '# '//TRIM(MNAME)//'.NAM File Generated by '//TRIM(UTL_IMODVERSION())
  WRITE(IU,'(/A/)') '#General Options'
  WRITE(IU,'(A)') 'BEGIN OPTIONS'
  WRITE(IU,'(A)') ' LIST .\GWF_'//TRIM(ITOS(PBMAN%ISUBMODEL))//'\'//TRIM(MNAME)//'.LST'
!## debug option
!  IF()WRITE(IU,'(A)') 'PRINT_INPUT'
!## print budgets
!  IF()WRITE(IU,'(A)') 'PRINT_FLOWS'
!SAVE FLOWS�keyword to indicate that all model package flow terms will be written to the file specified
!with �BUDGET FILEOUT� in Output Control.
!  IF()WRITE(IU,'(A)') 'SAVE_FLOWS'
!NEWTON�keyword that activates the Newton-Raphson formulation for groundwater flow between connected,
!convertible groundwater cells and stress packages that support calculation of Newton-
!Raphson terms for groundwater exchanges. Cells will not dry when this option is used. By default,
!the Newton-Raphson formulation is not applied.
!UNDER RELAXATION�keyword that indicates whether the groundwater head in a cell will be underrelaxed
!when water levels fall below the bottom of the model below any given cell. By default,
!Newton-Raphson UNDER RELAXATION is not applied.
!  IF()WRITE(IU,'(A)') 'NEWTON [UNDER_RELAXATION]'
  WRITE(IU,'(A)') 'END OPTIONS'

  WRITE(IU,'(/A/)') '#List of Packages'
  WRITE(IU,'(A)') 'BEGIN PACKAGES'
  WRITE(IU,'(A)') ' DIS6 '//TRIM(DIRMNAME)//'.DIS6'
  WRITE(IU,'(A)') ' IC6  '//TRIM(DIRMNAME)//'.IC6'
  WRITE(IU,'(A)') ' NPF6 '//TRIM(DIRMNAME)//'.NPF6'
  WRITE(IU,'(A)') ' OC6  '//TRIM(DIRMNAME)//'.OC6'
  IF(ISS.EQ.1)WRITE(IU,'(A)') ' STO6 '//TRIM(DIRMNAME)//'.STO6'
  IF(LCHD) WRITE(IU,'(A)') ' CHD6 '//TRIM(DIRMNAME)//'.CHD6'
  IF(LWEL) WRITE(IU,'(A)') ' WEL6 '//TRIM(DIRMNAME)//'.WEL6'
  IF(LDRN) WRITE(IU,'(A)') ' DRN6 '//TRIM(DIRMNAME)//'.DRN6'
  IF(LRCH) WRITE(IU,'(A)') ' RCH6 '//TRIM(DIRMNAME)//'.RCH6'
  IF(LRIV) WRITE(IU,'(A)') ' RIV6 '//TRIM(DIRMNAME)//'.RIV6'
  IF(LISG) WRITE(IU,'(A)') ' RIV6 '//TRIM(DIRMNAME)//'.RIV6'
  IF(LGHB) WRITE(IU,'(A)') ' GHB6 '//TRIM(DIRMNAME)//'.GHB6'
  IF(LHFB) WRITE(IU,'(A)') ' HFB6 '//TRIM(DIRMNAME)//'.HFB6'
  WRITE(IU,'(A)') 'END PACKAGES'

  CLOSE(IU)
  
 ELSE 
  
  DIRMNAME='.\'//TRIM(DIRMNAME)

  !## write *.nam file(s)
  N1=1; N2=1; IF(PBMAN%IPESTP.EQ.1)THEN; N1=-PBMAN%NLINESEARCH; N2=SIZE(PEST%PARAM); ENDIF
  DO I=N1,N2
   !## skip zero
   IF(I.EQ.0)CYCLE
   IU=UTL_GETUNIT()
   IF(PBMAN%IPESTP.EQ.0)THEN
    CALL OSD_OPEN(IU,FILE=TRIM(FNAME),STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
   ELSE
    IF(I.GT.0)THEN
     IF(PEST%PARAM(I)%PACT.EQ.0.OR.PEST%PARAM(I)%PIGROUP.LT.0)CYCLE
     NAME=FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'_P#'//TRIM(ITOS(I))//'.NAM'
    ELSE
     NAME=FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'_L#'//TRIM(ITOS(ABS(I)))//'.NAM'
    ENDIF
    CALL OSD_OPEN(IU,FILE=TRIM(NAME),STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
   ENDIF
   IF(IU.EQ.0)RETURN
   WRITE(IU,'(A)') '# Nam File Generated by '//TRIM(UTL_IMODVERSION())

   IF(PBMAN%IPESTP.EQ.0)THEN
    WRITE(IU,'(A)') 'LIST 10 '//CHAR(39)//'.\'//TRIM(MNAME)//'.LIST'//CHAR(39)
    WRITE(IU,'(A)') 'MET 11  '//CHAR(39)//TRIM(DIRMNAME)//'.MET7'//CHAR(39)
   ELSE
    IF(I.GT.0)THEN
     WRITE(IU,'(A)') 'LIST 10 '//CHAR(39)//'.\'//TRIM(MNAME)//'_P#'//TRIM(ITOS(I))//'.LIST'//CHAR(39)
     WRITE(IU,'(A)') 'MET 11  '//CHAR(39)//TRIM(DIRMNAME)//'_P#'//TRIM(ITOS(I))//'.MET7'//CHAR(39)
    ELSE
     WRITE(IU,'(A)') 'LIST 10 '//CHAR(39)//'.\'//TRIM(MNAME)//'_L#'//TRIM(ITOS(ABS(I)))//'.LIST'//CHAR(39)
     WRITE(IU,'(A)') 'MET 11  '//CHAR(39)//TRIM(DIRMNAME)//'_L#'//TRIM(ITOS(ABS(I)))//'.MET7'//CHAR(39)
    ENDIF
   ENDIF

   WRITE(IU,'(A)') 'BAS6 12 '//CHAR(39)//TRIM(DIRMNAME)//'.BAS6'//CHAR(39)
   WRITE(IU,'(A)') 'DIS  13 '//CHAR(39)//TRIM(DIRMNAME)//'.DIS6'//CHAR(39)
   IF(LBCF) WRITE(IU,'(A)') 'BCF6 14 '//CHAR(39)//TRIM(DIRMNAME)//'.BCF6'//CHAR(39)
   IF(LLPF) WRITE(IU,'(A)') 'LPF  14 '//CHAR(39)//TRIM(DIRMNAME)//'.LPF7'//CHAR(39)
   IF(LPCG) WRITE(IU,'(A)') 'PCG  15 '//CHAR(39)//TRIM(DIRMNAME)//'.PCG7'//CHAR(39)
   IF(LPKS) WRITE(IU,'(A)') 'PKS  15 '//CHAR(39)//TRIM(DIRMNAME)//'.PKS'//CHAR(39)
   WRITE(IU,'(A)') 'OC   16 '//CHAR(39)//TRIM(DIRMNAME)//'.OC'//CHAR(39)
   IF(LRCH) WRITE(IU,'(A)') 'RCH  17 '//CHAR(39)//TRIM(DIRMNAME)//'.RCH7'//CHAR(39)
   IF(LEVT) WRITE(IU,'(A)') 'EVT  18 '//CHAR(39)//TRIM(DIRMNAME)//'.EVT7'//CHAR(39)
   IF(LDRN.OR.LOLF) WRITE(IU,'(A)') 'DRN  19 '//CHAR(39)//TRIM(DIRMNAME)//'.DRN7'//CHAR(39)
   IF(LRIV.OR.LISG) WRITE(IU,'(A)') 'RIV  20 '//CHAR(39)//TRIM(DIRMNAME)//'.RIV7'//CHAR(39)
   IF(LGHB) WRITE(IU,'(A)') 'GHB  21 '//CHAR(39)//TRIM(DIRMNAME)//'.GHB7'//CHAR(39)
   IF(LCHD) WRITE(IU,'(A)') 'CHD  22 '//CHAR(39)//TRIM(DIRMNAME)//'.CHD7'//CHAR(39)
   IF(LWEL) WRITE(IU,'(A)') 'WEL  23 '//CHAR(39)//TRIM(DIRMNAME)//'.WEL7'//CHAR(39)
   IF(LHFB) WRITE(IU,'(A)') 'HFB6 24 '//CHAR(39)//TRIM(DIRMNAME)//'.HFB7'//CHAR(39)
   IF(LSFR) WRITE(IU,'(A)') 'SFR  25 '//CHAR(39)//TRIM(DIRMNAME)//'.SFR7'//CHAR(39)
   IF(LFHB)THEN; WRITE(IU,'(A)') 'FHB  26 '//CHAR(39)//TRIM(DIRMNAME)//'.FHB7'//CHAR(39); IFHBUN=26; ENDIF
   IF(LLAK) WRITE(IU,'(A)') 'LAK  27 '//CHAR(39)//TRIM(DIRMNAME)//'.LAK7'//CHAR(39) 
   IF(LUZF) WRITE(IU,'(A)') 'UZF  28 '//CHAR(39)//TRIM(DIRMNAME)//'.UZF7'//CHAR(39)
   IF(LMNW) WRITE(IU,'(A)') 'MNW2 29 '//CHAR(39)//TRIM(DIRMNAME)//'.MNW7'//CHAR(39)
   IF(LANI) WRITE(IU,'(A)') 'ANI  30 '//CHAR(39)//TRIM(DIRMNAME)//'.ANI1'//CHAR(39)
   IF(LMSP) WRITE(IU,'(A)') 'DXC  31 '//CHAR(39)//TRIM(DIRMNAME)//'.DXC'//CHAR(39) 

   WRITE(IU,'(A,I3,A)')         'DATA(BINARYIDF) ',IHEDUN,' '//CHAR(39)//'HEAD'//CHAR(39)
   WRITE(IU,'(A,I3,A)')         'DATA(BINARYIDF) ',IBCFCB,' '//CHAR(39)//'BDGSTO BDGBND BDGFRF BDGFFF BDGFLF'//CHAR(39) 
   IF(LRCH)WRITE(IU,'(A,I3,A)') 'DATA(BINARYIDF) ',IRCHCB,' '//CHAR(39)//'BDGRCH'//CHAR(39) 
   IF(LEVT)WRITE(IU,'(A,I3,A)') 'DATA(BINARYIDF) ',IEVTCB,' '//CHAR(39)//'BDGEVT'//CHAR(39) 
   IF(LDRN.OR.LOLF)WRITE(IU,'(A,I3,A)') 'DATA(BINARYIDF) ',IDRNCB,' '//CHAR(39)//'BDGDRN'//CHAR(39) 
   IF(LRIV.OR.LISG)WRITE(IU,'(A,I3,A)') 'DATA(BINARYIDF) ',IRIVCB,' '//CHAR(39)//'BDGRIV'//CHAR(39) 
   IF(LGHB)WRITE(IU,'(A,I3,A)') 'DATA(BINARYIDF) ',IGHBCB,' '//CHAR(39)//'BDGGHB'//CHAR(39) 
   IF(LCHD)WRITE(IU,'(A,I3,A)') 'DATA(BINARYIDF) ',ICHDCB,' '//CHAR(39)//'BDGCHD'//CHAR(39) 
   IF(LWEL)WRITE(IU,'(A,I3,A)') 'DATA(BINARYIDF) ',IWELCB,' '//CHAR(39)//'BDGWEL'//CHAR(39) 
   IF(LSFR)THEN
    WRITE(IU,'(A,I3,A)') 'DATA(BINARYIDF) ',ISFRCB,' '//CHAR(39)//'BDGSFR'//CHAR(39) 
    IF(ISFRCB2.GT.0)WRITE(IU,'(A,I3,A)') 'DATA         ',ISFRCB2,' '//CHAR(39)//'.\'//TRIM(MNAME)//'_FSFR.TXT'//CHAR(39) 
   ENDIF
   IF(LFHB)WRITE(IU,'(A,I3,A)') 'DATA(BINARYIDF) ',IFHBCB ,' '//CHAR(39)//'BDGFHB'//CHAR(39) 
   IF(LLAK)WRITE(IU,'(A,I3,A)') 'DATA(BINARYIDF) ',ILAKCB ,' '//CHAR(39)//'BDGLAK'//CHAR(39) 
   IF(LUZF)WRITE(IU,'(A,I3,A)') 'DATA(BINARYIDF) ',IUZFCB1,' '//CHAR(39)//'UZFINF BDGGRC BDGGET UZFRUN UZFET UZFSFR'//CHAR(39) 
   IF(LMNW)WRITE(IU,'(A,I3,A)') 'DATA(BINARYIDF) ',IWL2CB ,' '//CHAR(39)//'BDGMNW'//CHAR(39)
!   IF(LUZF)THEN
!    DO J=1,PBMAN%NLOGLOC
!     WRITE(IU,'(A,I3,A)') 'DATA ',99+J ,' '//CHAR(39)//'UZF_LOG_ROW'//TRIM(ITOS(PBMAN%ILOC(J,1)))//'-COL'//TRIM(ITOS(PBMAN%ILOC(J,2)))//'.TXT'//CHAR(39)
!    ENDDO
!   ENDIF
  ENDDO
 ENDIF

 CLOSE(IU)
 
 !## result folder including the modelname
 DIRMNAME=TRIM(DIR)//'\MODELINPUT\'//TRIM(MNAME)
 DIR     =TRIM(DIR)//'\MODELINPUT'

 !## echo used files from the prj-file
 IPRT=UTL_GETUNIT(); CALL OSD_OPEN(IPRT,FILE=TRIM(DIR)//'\USED_FILES.TXT',STATUS='UNKNOWN',ACTION='WRITE')

 PMANAGER_SAVEMF2005_NAM=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_NAM

 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_SIM(ISS,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ISS,IBATCH
 INTEGER :: ILAY
 REAL(KIND=DP_KIND) :: X1,Y1,X2,Y2
 
 PMANAGER_SAVEMF2005_SIM=.FALSE.
 
 !## read idf for dimensions
 CALL IDFNULLIFY(PRJIDF); IFULL=0
 IF(.NOT.PMANAGER_INIT_SIMAREA(PRJIDF,IBATCH))RETURN

 IF(ISUBMODEL.EQ.1)THEN
  X1=SUBMODEL(1); Y1=SUBMODEL(2); X2=SUBMODEL(3); Y2=SUBMODEL(4)
  !## include buffer to simulation window
  SUBMODEL(1)=SUBMODEL(1)-SUBMODEL(6); SUBMODEL(2)=SUBMODEL(2)-SUBMODEL(6)
  SUBMODEL(3)=SUBMODEL(3)+SUBMODEL(6); SUBMODEL(4)=SUBMODEL(4)+SUBMODEL(6)
  !## make sure size of model (including buffer) does not exceed total model domain
  SUBMODEL(1)=MAX(SUBMODEL(1),PRJIDF%XMIN); SUBMODEL(2)=MAX(SUBMODEL(2),PRJIDF%YMIN)
  SUBMODEL(3)=MIN(SUBMODEL(3),PRJIDF%XMAX); SUBMODEL(4)=MIN(SUBMODEL(4),PRJIDF%YMAX)
  !## see what boundary (submodel?)
  IF(SUBMODEL(1).GT.PRJIDF%XMIN)IFULL(1)=1; IF(SUBMODEL(2).GT.PRJIDF%YMIN)IFULL(2)=1
  IF(SUBMODEL(3).LT.PRJIDF%XMAX)IFULL(3)=1; IF(SUBMODEL(4).LT.PRJIDF%YMAX)IFULL(4)=1
  !## compute dimensions of submodel
  CALL UTL_IDFSNAPTOGRID_LLC(SUBMODEL(1),SUBMODEL(3),SUBMODEL(2),SUBMODEL(4),SUBMODEL(5),SUBMODEL(5),PRJIDF%NCOL,PRJIDF%NROW,LLC=.TRUE.)
  IF(PRJIDF%NCOL.LE.0.OR.PRJIDF%NROW.LE.0)THEN
   IF(IBATCH.EQ.0)WRITE(*,'(A)') 'Model dimensions are outside maximal modeling domain'
   IF(IBATCH.EQ.1)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Model dimensions are outside maximal modeling domain','Error')
   RETURN
  ENDIF
  PRJIDF%XMIN=SUBMODEL(1); PRJIDF%YMIN=SUBMODEL(2); PRJIDF%XMAX=SUBMODEL(3); PRJIDF%YMAX=SUBMODEL(4)
  PRJIDF%DX=SUBMODEL(5); PRJIDF%DY=SUBMODEL(5); 
  IF(SUBMODEL(7).EQ.0.0D0)THEN
   PRJIDF%IEQ=0
  ELSE
   !## create non-equidistantial network
   IF(.NOT.PMANAGER_SAVEMF2005_COARSEGRID(PRJIDF,X1,Y1,X2,Y2,SUBMODEL(7)))RETURN
  ENDIF
 ENDIF
 IF(.NOT.ASSOCIATED(PRJIDF%X))THEN
  IF(.NOT.IDFALLOCATEX(PRJIDF))RETURN
 ENDIF
 !## fill sx/sy variable in idf
 IF(.NOT.IDFFILLSXSY(PRJIDF))RETURN

 ALLOCATE(BND(PRJNLAY)); DO ILAY=1,SIZE(BND); CALL IDFNULLIFY(BND(ILAY)); ENDDO
 ALLOCATE(SHD(PRJNLAY)); DO ILAY=1,SIZE(SHD); CALL IDFNULLIFY(SHD(ILAY)); ENDDO
 ALLOCATE(TOP(PRJNLAY)); DO ILAY=1,SIZE(TOP); CALL IDFNULLIFY(TOP(ILAY)); ENDDO
 ALLOCATE(BOT(PRJNLAY)); DO ILAY=1,SIZE(BOT); CALL IDFNULLIFY(BOT(ILAY)); ENDDO
 ALLOCATE(KDW(PRJNLAY)); DO ILAY=1,SIZE(KDW); CALL IDFNULLIFY(KDW(ILAY)); ENDDO
 ALLOCATE(VCW(PRJNLAY-1)); DO ILAY=1,SIZE(VCW); CALL IDFNULLIFY(VCW(ILAY)); ENDDO
 ALLOCATE(KHV(PRJNLAY)); DO ILAY=1,SIZE(KHV); CALL IDFNULLIFY(KHV(ILAY)); ENDDO

 IF(ISS.EQ.1)THEN
  ALLOCATE(STO(PRJNLAY)); DO ILAY=1,SIZE(STO); CALL IDFNULLIFY(STO(ILAY)); ENDDO
  ALLOCATE(SPY(PRJNLAY)); DO ILAY=1,SIZE(SPY); CALL IDFNULLIFY(SPY(ILAY)); ENDDO
 ENDIF

 IF(LLPF.OR.LNPF)THEN
  ALLOCATE(KVV(PRJNLAY-1)); DO ILAY=1,SIZE(KVV); CALL IDFNULLIFY(KVV(ILAY)); ENDDO
  ALLOCATE(KVA(PRJNLAY)); DO ILAY=1,SIZE(KVA); CALL IDFNULLIFY(KVA(ILAY)); ENDDO
 ENDIF

 IF(LANI)THEN
  ALLOCATE(ANA(PRJNLAY)); DO ILAY=1,SIZE(ANA); CALL IDFNULLIFY(ANA(ILAY)); ENDDO
  ALLOCATE(ANF(PRJNLAY)); DO ILAY=1,SIZE(ANF); CALL IDFNULLIFY(ANF(ILAY)); ENDDO
 ENDIF

 IF(LLAK)THEN
  ALLOCATE(LAK(10)); DO ILAY=1,SIZE(LAK); CALL IDFNULLIFY(LAK(ILAY)); ENDDO
  ALLOCATE(LBD(PRJNLAY)); DO ILAY=1,SIZE(LBD); CALL IDFNULLIFY(LBD(ILAY)); ENDDO
  ALLOCATE(LCD(PRJNLAY)); DO ILAY=1,SIZE(LCD); CALL IDFNULLIFY(LCD(ILAY)); ENDDO
 ENDIF

! IF(LSFT)THEN
 ALLOCATE(SFT(2)); DO ILAY=1,SIZE(SFT); CALL IDFNULLIFY(SFT(ILAY)); ENDDO
! ENDIF

 DO ILAY=1,SIZE(TOP); CALL IDFCOPY(PRJIDF,TOP(ILAY)); ENDDO
 DO ILAY=1,SIZE(BOT); CALL IDFCOPY(PRJIDF,BOT(ILAY)); ENDDO
 DO ILAY=1,SIZE(KDW); CALL IDFCOPY(PRJIDF,KDW(ILAY)); ENDDO
 DO ILAY=1,SIZE(VCW); CALL IDFCOPY(PRJIDF,VCW(ILAY)); ENDDO
 DO ILAY=1,SIZE(KHV); CALL IDFCOPY(PRJIDF,KHV(ILAY)); ENDDO

 IF(LLPF.OR.LNPF)THEN
  DO ILAY=1,SIZE(KVV); CALL IDFCOPY(PRJIDF,KVV(ILAY)); ENDDO
  DO ILAY=1,SIZE(KVA); CALL IDFCOPY(PRJIDF,KVA(ILAY)); ENDDO
 ENDIF

 IF(ISS.EQ.1)THEN
  DO ILAY=1,SIZE(STO); CALL IDFCOPY(PRJIDF,STO(ILAY)); ENDDO
  DO ILAY=1,SIZE(SPY); CALL IDFCOPY(PRJIDF,SPY(ILAY)); ENDDO
 ENDIF

 IF(LANI)THEN
  DO ILAY=1,SIZE(ANF); CALL IDFCOPY(PRJIDF,ANF(ILAY)); ENDDO
  DO ILAY=1,SIZE(ANA); CALL IDFCOPY(PRJIDF,ANA(ILAY)); ENDDO
 ENDIF

 IF(LLAK)THEN
  DO ILAY=1,SIZE(LBD); CALL IDFCOPY(PRJIDF,LBD(ILAY)); ENDDO
  DO ILAY=1,SIZE(LCD); CALL IDFCOPY(PRJIDF,LCD(ILAY)); ENDDO
 ENDIF
  
 IF(LSFT)THEN
  DO ILAY=1,SIZE(SFT); CALL IDFCOPY(PRJIDF,SFT(ILAY)); ENDDO
 ENDIF
 
 PMANAGER_SAVEMF2005_SIM=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_SIM

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_PST_READWRITE(DIR,DIRMNAME,IBATCH,ISS)
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME
 INTEGER,INTENT(IN) :: IBATCH,ISS
 INTEGER :: I,J,N,N1,N2,IU
 CHARACTER(LEN=256) :: CFNAME
 
 PMANAGER_SAVEMF2005_PST_READWRITE=.TRUE.
 
 IF(.NOT.LPST)RETURN
 !## overrule is by imod batch
 IF(IBATCH.EQ.1.AND.PBMAN%IPEST+PBMAN%IPESTP.EQ.0)RETURN

 PMANAGER_SAVEMF2005_PST_READWRITE=.FALSE.

 N=0; IF(ASSOCIATED(PEST%MEASURES))THEN; N=SIZE(PEST%MEASURES); ENDIF

 IF(N.EQ.0.AND.PEST%PE_MXITER.GT.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to specify measurements to use the PST module.','Error'); RETURN
 ENDIF
 
 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.PST1'//'...')
 IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.PST1'//'...'

 N1=1; N2=1; J=0; IF(PBMAN%IPESTP.EQ.1)THEN; N1=-PBMAN%NLINESEARCH; N2=SIZE(PEST%PARAM); ENDIF; CFNAME=''
 DO I=N1,N2
  !## skip zero
  IF(I.EQ.0)CYCLE

  IU=UTL_GETUNIT()
  IF(PBMAN%IPESTP.EQ.0)THEN
   CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.PST1',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
  ELSE
   IF(I.GT.0)THEN
    IF(PEST%PARAM(I)%PACT.EQ.0.OR.PEST%PARAM(I)%PIGROUP.LT.0)CYCLE
    IF(J.EQ.0)THEN
     CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'_P#'//TRIM(ITOS(I))//'.PST1',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
     CFNAME=TRIM(DIRMNAME)//'_P#'//TRIM(ITOS(I))//'.PST1'
    ELSE
     CALL IOSCOPYFILE(CFNAME,TRIM(DIRMNAME)//'_P#'//TRIM(ITOS(I))//'.PST1')
    ENDIF
   ELSE
    IF(J.EQ.0)THEN
     CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'_L#'//TRIM(ITOS(ABS(I)))//'.PST1',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
     CFNAME=TRIM(DIRMNAME)//'_L#'//TRIM(ITOS(ABS(I)))//'.PST1'
    ELSE
     CALL IOSCOPYFILE(CFNAME,TRIM(DIRMNAME)//'_L#'//TRIM(ITOS(ABS(I)))//'.PST1')
    ENDIF
   ENDIF   
  ENDIF
  
  IF(J.EQ.0)THEN
   IF(IU.EQ.0)RETURN
  
   WRITE(IU,'(A)') '# PST1 File Generated by '//TRIM(UTL_IMODVERSION())

   !## pst module is exception
   IF(.NOT.PMANAGER_SAVEPST(IU,2,DIR,ISS,-1))RETURN

   CLOSE(IU)
  ENDIF
  J=1
  
 ENDDO
 
 PMANAGER_SAVEMF2005_PST_READWRITE=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_PST_READWRITE

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_BAS_READ(IPRT)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPRT
 INTEGER :: ITOPIC,SCL_D,SCL_U,ILAY
 
 PMANAGER_SAVEMF2005_BAS_READ=.FALSE.

 ALLOCATE(FNAMES(PRJNLAY),PRJILIST(1))

 !## bnd settings
 ITOPIC=TBND; SCL_D=0; SCL_U=1; PRJILIST=ITOPIC; IF(PMANAGER_GETFNAMES(1,PRJNLAY,0,1,0).LE.0)RETURN

 DO ILAY=1,PRJNLAY
  WRITE(6,'(A)') '+Reading BND-files ('//TRIM(RTOS(REAL(100*ILAY,8)/REAL(PRJNLAY,8),'F',2))//'%)      '

  CALL IDFCOPY(PRJIDF,BND(ILAY))
  IF(.NOT.PMANAGER_SAVEMF2005_MOD_READ(BND(ILAY),ITOPIC,ILAY,SCL_D,SCL_U,0,IPRT))RETURN 
  !## adjust boundary for submodel()
  CALL PMANAGER_SAVEMF2005_BND(ILAY)
 ENDDO

 !## shd settings
 ITOPIC=TSHD; SCL_D=PBMAN%INT(TSHD); SCL_U=2; PRJILIST=ITOPIC; IF(PMANAGER_GETFNAMES(1,PRJNLAY,0,1,0).LE.0)RETURN

 DO ILAY=1,PRJNLAY
  WRITE(6,'(A)') '+Reading SHD-files ('//TRIM(RTOS(REAL(100*ILAY,8)/REAL(PRJNLAY,8),'F',2))//'%)      '
  CALL IDFCOPY(PRJIDF,SHD(ILAY))
  IF(.NOT.PMANAGER_SAVEMF2005_MOD_READ(SHD(ILAY),ITOPIC,ILAY,SCL_D,SCL_U,0,IPRT))RETURN
  CALL PMANAGER_SAVEMF2005_CORRECT(ILAY,BND,SHD(ILAY),0,ITOPIC)
 ENDDO

 DEALLOCATE(FNAMES,PRJILIST)

 PMANAGER_SAVEMF2005_BAS_READ=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_BAS_READ
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_BAS_SAVE(DIR,DIRMNAME,IBATCH)
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: IU,ILAY,IFBND

 PMANAGER_SAVEMF2005_BAS_SAVE=.TRUE.; IF(PBMAN%IFORMAT.EQ.3)RETURN
 PMANAGER_SAVEMF2005_BAS_SAVE=.FALSE.
 
 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.BAS6'//'...')
 IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.BAS6'//'...'

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.BAS6',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
 WRITE(IU,'(A)') '# BAS6 File Generated by '//TRIM(UTL_IMODVERSION())
 LINE='FREE'
 IF(PCG%IQERROR.EQ.0)THEN
  WRITE(IU,'(A)') 'FREE'
 ELSE
  WRITE(IU,'(A,G12.5)') 'FREE STOPERROR ',PCG%QERROR
 ENDIF
 IFBND=0
 DO ILAY=1,PRJNLAY
  IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\BAS6\IBOUND_L'//TRIM(ITOS(ILAY))//'.ARR', &
      BND(ILAY),1,IU,ILAY,IFBND))RETURN
 ENDDO
 WRITE(IU,'(A)') TRIM(RTOS(HNOFLOW,'G',7))
 IFBND=1
 DO ILAY=1,PRJNLAY
  IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\BAS6\STRT_L'//TRIM(ITOS(ILAY))//'.ARR', &
      SHD(ILAY),0,IU,ILAY,IFBND))RETURN
 ENDDO
 CLOSE(IU)
 
 PMANAGER_SAVEMF2005_BAS_SAVE=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_BAS_SAVE

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_IC_SAVE(DIR,DIRMNAME,IBATCH)
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: IU,ILAY,JLAY,IFBND

 PMANAGER_SAVEMF2005_IC_SAVE=.TRUE.; IF(PBMAN%IFORMAT.EQ.2)RETURN
 PMANAGER_SAVEMF2005_IC_SAVE=.FALSE.
 
 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.IC6'//'...')
 IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.IC6'//'...'
 
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.IC6',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
 WRITE(IU,'(A)') '# IC6 File Generated by '//TRIM(UTL_IMODVERSION())
 WRITE(IU,'(/A/)') '#General Options'
 WRITE(IU,'(A)') 'BEGIN OPTIONS'
 WRITE(IU,'(A)') 'END OPTIONS'

 WRITE(IU,'(/A/)') '#Initial Head Data'
 WRITE(IU,'(A)') 'BEGIN GRIDDATA'
 WRITE(IU,'(A)') ' STRT LAYERED'
 JLAY=0; DO ILAY=1,SIZE(PBMAN%ILAY)
  IF(PBMAN%ILAY(ILAY).EQ.0)CYCLE
  JLAY=JLAY+1
  IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\IC6\IC_L'//TRIM(ITOS(JLAY))//'.ARR', &
      SHD(ILAY),0,IU,ILAY,IFBND))RETURN
 ENDDO
 WRITE(IU,'(A)') 'END GRIDDATA'
 CLOSE(IU)
  
 PMANAGER_SAVEMF2005_IC_SAVE=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_IC_SAVE
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_DIS_READ(LTB,IPRT)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPRT
 LOGICAL,INTENT(OUT) :: LTB
 INTEGER :: ILAY,IINV,SCL_D,SCL_U,ITOPIC
 LOGICAL :: LEX

 PMANAGER_SAVEMF2005_DIS_READ=.FALSE.

 ALLOCATE(FNAMES(1),PRJILIST(1))

 !## check top/bottom
 LTB=.TRUE.; IINV=0
 
 !## top settings
 SCL_D=PBMAN%INT(TTOP); SCL_U=2
 DO ILAY=1,PRJNLAY

  WRITE(6,'(A)') '+Reading TOP/BOT-files ('//TRIM(RTOS(REAL(100*ILAY,8)/REAL(PRJNLAY,8),'F',2))//'%)      '

  !## top data
  ITOPIC=TTOP; LEX=.FALSE.
  IF(ASSOCIATED(TOPICS(ITOPIC)%STRESS))THEN
   IF(ASSOCIATED(TOPICS(ITOPIC)%STRESS(1)%FILES))THEN

    PRJILIST=ITOPIC; IF(PMANAGER_GETFNAMES(ILAY,ILAY,0,1,0).LE.0)RETURN

    IF(.NOT.PMANAGER_SAVEMF2005_MOD_READ(TOP(ILAY),ITOPIC,1,SCL_D,SCL_U,IINV,IPRT))RETURN
    LEX=.TRUE.
   ENDIF
  ENDIF
  IF(.NOT.LEX)THEN; TOP(ILAY)%X=0.0D0; LTB=.FALSE.; ENDIF
  
  !## bot data
  ITOPIC=TBOT; LEX=.FALSE.
  IF(ASSOCIATED(TOPICS(ITOPIC)%STRESS))THEN
   IF(ASSOCIATED(TOPICS(ITOPIC)%STRESS(1)%FILES))THEN

    PRJILIST=ITOPIC; IF(PMANAGER_GETFNAMES(ILAY,ILAY,0,1,0).LE.0)RETURN

    IF(.NOT.PMANAGER_SAVEMF2005_MOD_READ(BOT(ILAY),ITOPIC,1,SCL_D,SCL_U,IINV,IPRT))RETURN
    LEX=.TRUE.
   ENDIF
  ENDIF
  IF(.NOT.LEX)THEN; BOT(ILAY)%X=0.0D0; LTB=.FALSE.; ENDIF
  
 ENDDO

 DEALLOCATE(FNAMES,PRJILIST)

 PMANAGER_SAVEMF2005_DIS_READ=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_DIS_READ
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_DIS_SAVE(DIR,DIRMNAME,IBATCH)
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: IU,ILAY,JLAY,KPER,KKPER,ITOPIC,ICOL,IROW,N,I,LHMS
 INTEGER,ALLOCATABLE,DIMENSION(:) :: LCBD
 REAL(KIND=DP_KIND) :: T
 CHARACTER(LEN=52) :: CLINE
 
 PMANAGER_SAVEMF2005_DIS_SAVE=.FALSE.

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.DIS6'//'...')
 IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.DIS6'//'...'
 
 !## construct dis-file
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.DIS6',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
 WRITE(IU,'(A)') '# DIS6 File Generated by '//TRIM(UTL_IMODVERSION())

 IF(PBMAN%IFORMAT.EQ.2)THEN

  LINE=TRIM(ITOS(PRJNLAY))//','//TRIM(ITOS(PRJIDF%NROW))//','//TRIM(ITOS(PRJIDF%NCOL))//','//TRIM(ITOS(PRJNPER))//',4,2 TBCHECK'
  WRITE(IU,'(A)') TRIM(LINE)

  ALLOCATE(LCBD(PRJNLAY))
 
  !## laycbd code
  LINE=''
  DO ILAY=1,PRJNLAY
   IF(ILAY.LT.PRJNLAY)THEN
    !## quasi-3d scheme
    IF(LQBD)THEN
     LCBD(ILAY)=1
    !## 3d no quasi confining bed
    ELSE
     LCBD(ILAY)=0
    ENDIF
   ELSE
    !## lowest layer has never a quasi-confining bed
    LCBD(ILAY)=0
   ENDIF
  ENDDO

  WRITE(IU,'(999I2)') LCBD
  DEALLOCATE(LCBD)
 
  IF(PRJIDF%IEQ.EQ.0)THEN
   WRITE(IU,'(A)') 'CONSTANT '//TRIM(RTOS(PRJIDF%DX,'E',7)); WRITE(IU,'(A)') 'CONSTANT '//TRIM(RTOS(PRJIDF%DY,'E',7))
  ELSE
   WRITE(IU,'(A)') 'INTERNAL,1.0D0,(FREE),-1'
   WRITE(IU,*) (PRJIDF%SX(ICOL)-PRJIDF%SX(ICOL-1),ICOL=1,PRJIDF%NCOL)
   WRITE(IU,'(A)') 'INTERNAL,1.0D0,(FREE),-1'
   WRITE(IU,*) (PRJIDF%SY(IROW-1)-PRJIDF%SY(IROW),IROW=1,PRJIDF%NROW)
  ENDIF

  DO ILAY=1,PRJNLAY
   ITOPIC=TTOP
   !## quasi-3d scheme add top aquifer modellayer
   IF(LQBD.OR.ILAY.EQ.1)THEN
    IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\DIS6\TOP_L'//TRIM(ITOS(ILAY))//'.ARR', &
       TOP(ILAY),0,IU,ILAY,ITOPIC))RETURN
   ENDIF
   ITOPIC=TBOT
   IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\DIS6\BOTM_L'//TRIM(ITOS(ILAY))//'.ARR', &
       BOT(ILAY),0,IU,ILAY,ITOPIC))RETURN
  ENDDO
  
  !## time information
  LHMS=0; DO KPER=1,PRJNPER
   !## set delt.eq.1 otherwise crash in UZF package
   IF(SIM(KPER)%DELT.GT.0.0D0)THEN
    IF(SIM(KPER)%IHR+SIM(KPER)%IMT+SIM(KPER)%ISC.NE.0)THEN; LHMS=1; EXIT; ENDIF
   ENDIF
  ENDDO
  
  !## time information
  DO KPER=1,PRJNPER
   !## set delt.eq.1 otherwise crash in UZF package
   IF(SIM(KPER)%DELT.EQ.0.0D0)THEN
    LINE=TRIM(RTOS(1.0D0,'G',7))//','// &
         TRIM(ITOS(SIM(KPER)%NSTP))      //','// &
         TRIM(RTOS(SIM(KPER)%TMULT,'G',7))
   ELSE
    LINE=TRIM(RTOS(SIM(KPER)%DELT,'G',7))//','// &
         TRIM(ITOS(SIM(KPER)%NSTP))      //','// &
         TRIM(RTOS(SIM(KPER)%TMULT,'G',7))
   ENDIF
   IF(SIM(KPER)%DELT.EQ.0.0D0)LINE=TRIM(LINE)//',SS'
   IF(SIM(KPER)%DELT.NE.0.0D0)LINE=TRIM(LINE)//',TR'
   IF(SIM(KPER)%DELT.EQ.0.0D0)THEN
    CLINE='STEADY-STATE'
   ELSE
    KKPER=KPER; IF(PBMAN%ISAVEENDDATE.EQ.1)KKPER=KKPER+1
    IF(LHMS.EQ.0)THEN
     WRITE(CLINE,'(I4.4,2I2.2)') SIM(KKPER)%IYR,SIM(KKPER)%IMH,SIM(KKPER)%IDY
    ELSE
     WRITE(CLINE,'(I4.4,5I2.2)') SIM(KKPER)%IYR,SIM(KKPER)%IMH,SIM(KKPER)%IDY,SIM(KKPER)%IHR,SIM(KKPER)%IMT,SIM(KKPER)%ISC
    ENDIF
   ENDIF
   LINE=TRIM(LINE)//' ['//TRIM(CLINE)//']'
   WRITE(IU,'(A)') TRIM(LINE)
  ENDDO
 
 ELSE
  
  WRITE(IU,'(/A/)') 'General Options'
  WRITE(IU,'(A)') 'BEGIN OPTIONS'  
  WRITE(IU,'(A)') ' LENGTH_UNITS METERS'  
  !WRITE(IU,'(A)') ' NOGRB'
  WRITE(IU,'(A)') ' XORIGIN '//TRIM(RTOS(PRJIDF%SX(0),'F',3))
  WRITE(IU,'(A)') ' YORIGIN '//TRIM(RTOS(PRJIDF%SY(PRJIDF%NROW),'F',3))
  WRITE(IU,'(A)') ' ANGROT 0.0'
  WRITE(IU,'(A)') 'END OPTIONS'  

  WRITE(IU,'(/A/)') '#Model Dimensions'
  WRITE(IU,'(A)') 'BEGIN DIMENSIONS'
  N=0; DO I=1,SIZE(PBMAN%ILAY); IF(PBMAN%ILAY(I).EQ.1)N=N+1; ENDDO
  WRITE(IU,'(A)') ' NLAY '//TRIM(ITOS(N)) !PRJNLAY))
  WRITE(IU,'(A)') ' NROW '//TRIM(ITOS(PRJIDF%NROW))
  WRITE(IU,'(A)') ' NCOL '//TRIM(ITOS(PRJIDF%NCOL))
  WRITE(IU,'(A)') 'END DIMENSIONS'

  WRITE(IU,'(/A/)') '#Cell Sizes'
  WRITE(IU,'(A)') 'BEGIN GRIDDATA'
  WRITE(IU,'(A)') ' DELR'
  IF(PRJIDF%IEQ.EQ.0)THEN
   WRITE(IU,'(A)') '  CONSTANT '//TRIM(RTOS(PRJIDF%DX,'E',7))
  ELSE
   WRITE(IU,'(A)') '  INTERNAL FACTOR 1.0'
   WRITE(IU,*) (PRJIDF%SX(ICOL)-PRJIDF%SX(ICOL-1),ICOL=1,PRJIDF%NCOL)
  ENDIF
  WRITE(IU,'(A)') ' DELC'
  IF(PRJIDF%IEQ.EQ.0)THEN
   WRITE(IU,'(A)') '  CONSTANT '//TRIM(RTOS(PRJIDF%DY,'E',7))
  ELSE
   WRITE(IU,'(A)') '  INTERNAL FACTOR 1.0'
   WRITE(IU,*) (PRJIDF%SY(IROW-1)-PRJIDF%SY(IROW),IROW=1,PRJIDF%NROW)
  ENDIF

  WRITE(IU,'(/A/)') '#Vertical Configuration'
  WRITE(IU,'(A)') 'TOP'
  ITOPIC=TTOP

  !## get first model layer
  DO I=1,SIZE(PBMAN%ILAY); IF(PBMAN%ILAY(I).EQ.1)EXIT; ENDDO

  !## quasi-3d scheme add top aquifer modellayer
  IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\DIS6\TOP_L'//TRIM(ITOS(1))//'.ARR', &
     TOP(I),0,IU,1,ITOPIC))RETURN  
  
  !## write idf for connection-purposes
  IF(.NOT.IDFWRITE(TOP(I),TRIM(DIR)//'\DIS6\TOP_L'//TRIM(ITOS(1))//'.IDF',1))RETURN
  
  WRITE(IU,'(A)') 'BOTM LAYERED'
  ITOPIC=TBOT
  JLAY=0; DO ILAY=1,SIZE(PBMAN%ILAY)
   IF(PBMAN%ILAY(ILAY).EQ.0)CYCLE
   JLAY=JLAY+1
   IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\DIS6\BOTM_L'//TRIM(ITOS(JLAY))//'.ARR', &
       BOT(ILAY),0,IU,ILAY,ITOPIC))RETURN
 
   !## write idf for connection-purposes
   IF(.NOT.IDFWRITE(BOT(ILAY),TRIM(DIR)//'\DIS6\BOT_L'//TRIM(ITOS(JLAY))//'.IDF',1))RETURN

  ENDDO

  WRITE(IU,'(/A/)') '#Boundary Settings'
  WRITE(IU,'(A)') 'IDOMAIN LAYERED'
  JLAY=0; DO ILAY=1,SIZE(PBMAN%ILAY) 
   IF(PBMAN%ILAY(ILAY).EQ.0)CYCLE
   JLAY=JLAY+1
   
   !## modify bnd for idomain parameter
   PRJIDF%X=BND(ILAY)%X; PRJIDF%NODATA=BND(ILAY)%NODATA

   !## clean idomain which was the boundary condition
   DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
    IF(PRJIDF%X(ICOL,IROW).EQ.PRJIDF%NODATA)PRJIDF%X(ICOL,IROW)=0.0D0
    IF(PRJIDF%X(ICOL,IROW).LT.0.0)          PRJIDF%X(ICOL,IROW)=1.0D0
    IF(PRJIDF%X(ICOL,IROW).GT.1.0)          PRJIDF%X(ICOL,IROW)=1.0D0
   ENDDO; ENDDO

   DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
    IF(BND(ILAY)%X(ICOL,IROW).EQ.0.0D0)CYCLE
    T=TOP(ILAY)%X(ICOL,IROW)-BOT(ILAY)%X(ICOL,IROW)
    IF(T.LE.0.0D0)THEN
     PRJIDF%X(ICOL,IROW)=-1.0D0
     !## make sure an active cells are not allowed on thickness of zero
     BND(ILAY)%X(ICOL,IROW)=0.0
    ENDIF
   ENDDO; ENDDO

!   BND(ILAY)%X=PRJIDF%X

   !## modify idomain a bit in case MF6 is used to force an export to an ARR-file
IRLOOP: DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
    IF(PRJIDF%X(ICOL,IROW).GT.0)THEN
     PRJIDF%X(ICOL,IROW)=2.0D0
     EXIT IRLOOP
    ENDIF
   ENDDO; ENDDO IRLOOP

   IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\DIS6\IBOUND_L'//TRIM(ITOS(JLAY))//'.ARR', &
       PRJIDF,1,IU,ILAY,0))RETURN

   !## write idf for connection-purposes
   IF(.NOT.IDFWRITE(PRJIDF,TRIM(DIR)//'\DIS6\BND_L'//TRIM(ITOS(JLAY))//'.IDF',1))RETURN

!idomain�is an optional array that characterizes the existence status of a cell. If the IDOMAIN array
!is not specified, then all model cells exist within the solution. If the IDOMAIN value for a cell is 0,
!the cell does not exist in the simulation. Input and output values will be read and written for the cell,
!but internal to the program, the cell is excluded from the solution. If the IDOMAIN value for a cell
!is 1, the cell exists in the simulation. If the IDOMAIN value for a cell is -1, the cell does not exist in
!the simulation. Furthermore, the first existing cell above will be connected to the first existing cell
!below. This type of cell is referred to as a �vertical pass through� cell.
  ENDDO
  WRITE(IU,'(A)') 'END GRIDDATA'

 ENDIF
 
 CLOSE(IU)

 PMANAGER_SAVEMF2005_DIS_SAVE=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_DIS_SAVE

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_BCF_READ(ISS,IPRT)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ISS,IPRT
 INTEGER :: ILAY,SCL_D,SCL_U,IINV,ITOPIC
  
 PMANAGER_SAVEMF2005_BCF_READ=.TRUE.
 
 !## use bcf6
 IF(.NOT.LBCF)RETURN

 PMANAGER_SAVEMF2005_BCF_READ=.FALSE.

 ALLOCATE(FNAMES(1),PRJILIST(1))

 DO ILAY=1,PRJNLAY
  
  WRITE(6,'(A)') '+Reading BCF-files ('//TRIM(RTOS(REAL(100*ILAY,8)/REAL(PRJNLAY,8),'F',2))//'%)      '

  !## transient simulation
  IF(ISS.EQ.1)THEN

   !## sf1
   ITOPIC=TSTO; SCL_D=PBMAN%INT(TSTO); SCL_U=2; IINV=0

   PRJILIST=ITOPIC; IF(PMANAGER_GETFNAMES(ILAY,ILAY,0,1,0).LE.0)RETURN
   IF(.NOT.PMANAGER_SAVEMF2005_MOD_READ(STO(ILAY),ITOPIC,1,SCL_D,SCL_U,IINV,IPRT))RETURN
   CALL PMANAGER_SAVEMF2005_CORRECT(ILAY,BND,STO(ILAY),0,ITOPIC)

  ENDIF   

  !## kdw
  ITOPIC=TKDW; SCL_D=PBMAN%INT(TKDW); SCL_U=3; IINV=0

  PRJILIST=ITOPIC; IF(PMANAGER_GETFNAMES(ILAY,ILAY,0,1,0).LE.0)RETURN

  IF(.NOT.PMANAGER_SAVEMF2005_MOD_READ(KDW(ILAY),ITOPIC,1,SCL_D,SCL_U,IINV,IPRT))RETURN
  CALL PMANAGER_SAVEMF2005_CORRECT(ILAY,BND,KDW(ILAY),0,ITOPIC)

  IF(ILAY.NE.PRJNLAY)THEN

   !## vcont
   ITOPIC=TVCW; SCL_D=PBMAN%INT(TVCW); SCL_U=6; IINV=1

   PRJILIST=ITOPIC; IF(PMANAGER_GETFNAMES(ILAY,ILAY,0,1,0).LE.0)RETURN

   IF(.NOT.PMANAGER_SAVEMF2005_MOD_READ(VCW(ILAY),ITOPIC,1,SCL_D,SCL_U,IINV,IPRT))RETURN
   CALL PMANAGER_SAVEMF2005_CORRECT(ILAY,BND,VCW(ILAY),0,ITOPIC)

  ENDIF
 ENDDO  
 
 DEALLOCATE(FNAMES,PRJILIST)

 PMANAGER_SAVEMF2005_BCF_READ=.TRUE.
 
 END FUNCTION PMANAGER_SAVEMF2005_BCF_READ
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_BCF_SAVE(DIR,DIRMNAME,IBATCH,ISS)
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME
 INTEGER,INTENT(IN) :: IBATCH,ISS
 INTEGER :: IU,ILAY,IFBND
 
 PMANAGER_SAVEMF2005_BCF_SAVE=.TRUE.
 
 !## use bcf6
 IF(.NOT.LBCF)RETURN; IF(PBMAN%IFORMAT.EQ.3)RETURN

 PMANAGER_SAVEMF2005_BCF_SAVE=.FALSE.

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.BCF6'//'...')
 IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.BCF6'//'...'

 !## construct bcf6-file
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.BCF6',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
 LINE=TRIM(ITOS(IBCFCB))//','//TRIM(RTOS(HNOFLOW,'G',7))//',0,1.0D0,1,0'
 IF(PBMAN%MINKD.NE.0.0D0)LINE=TRIM(LINE)//',MINKD '//TRIM(RTOS(PBMAN%MINKD,'G',5))
 IF(PBMAN%MINC .NE.0.0D0)LINE=TRIM(LINE)//',MINC ' //TRIM(RTOS(PBMAN%MINC ,'G',5))
 WRITE(IU,'(A)') TRIM(LINE)
 !## ltype code
 LINE=''; DO ILAY=1,PRJNLAY; LINE=TRIM(LINE)//'00,'
  IF(MOD(ILAY,40).EQ.0)THEN; WRITE(IU,'(A)') TRIM(LINE); LINE=''; ENDIF
 ENDDO
 IF(LEN_TRIM(LINE).NE.0)WRITE(IU,'(A)') TRIM(LINE)
 WRITE(IU,'(A)') 'CONSTANT 1.0D0' !## trpy

 IFBND=1

 DO ILAY=1,PRJNLAY

  !## transient simulation
  IF(ISS.EQ.1)THEN

   !## sf1
   IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\BCF6\SF1_L'//TRIM(ITOS(ILAY))//'.ARR', &
       STO(ILAY),0,IU,ILAY,IFBND))RETURN

  ENDIF   

  !## kdw
  IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\BCF6\TRAN_L'//TRIM(ITOS(ILAY))//'.ARR', &
      KDW(ILAY),0,IU,ILAY,IFBND))RETURN

  IF(ILAY.NE.PRJNLAY)THEN

   !## vcont
   IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\BCF6\VCONT_L'//TRIM(ITOS(ILAY))//'.ARR', &
       VCW(ILAY),0,IU,ILAY,IFBND))RETURN

  ENDIF
 ENDDO  

 CLOSE(IU)

 PMANAGER_SAVEMF2005_BCF_SAVE=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_BCF_SAVE

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_LPF_READ(ISS,IPRT)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ISS,IPRT
 INTEGER :: ILAY,SCL_D,SCL_U,IINV,ITOPIC
  
 PMANAGER_SAVEMF2005_LPF_READ=.TRUE.
 
 !## use lpf6
 IF(.NOT.LLPF.AND..NOT.LNPF)RETURN

 ALLOCATE(FNAMES(1),PRJILIST(1))

 PMANAGER_SAVEMF2005_LPF_READ=.FALSE.

 DO ILAY=1,PRJNLAY

  WRITE(6,'(A)') '+Reading LPF-files ('//TRIM(RTOS(REAL(100*ILAY,8)/REAL(PRJNLAY,8),'F',2))//'%)      '

  !## hkv
  ITOPIC=TKHV; SCL_D=PBMAN%INT(TKHV); SCL_U=3; IINV=0

  PRJILIST=ITOPIC; IF(PMANAGER_GETFNAMES(ILAY,ILAY,0,1,0).LE.0)RETURN

  IF(.NOT.PMANAGER_SAVEMF2005_MOD_READ(KHV(ILAY),ITOPIC,1,SCL_D,SCL_U,IINV,IPRT))RETURN
  CALL PMANAGER_SAVEMF2005_CORRECT(ILAY,BND,KHV(ILAY),0,ITOPIC)

   !## vka
   ITOPIC=TKVA; SCL_D=PBMAN%INT(TKVA); SCL_U=2; IINV=1

   PRJILIST=ITOPIC; IF(PMANAGER_GETFNAMES(ILAY,ILAY,0,1,0).LE.0)RETURN

   IF(.NOT.PMANAGER_SAVEMF2005_MOD_READ(KVA(ILAY),ITOPIC,1,SCL_D,SCL_U,IINV,IPRT))RETURN
   CALL PMANAGER_SAVEMF2005_CORRECT(ILAY,BND,KVA(ILAY),0,ITOPIC)
  
  !## transient simulation
  IF(ISS.EQ.1)THEN

   !## sf1 - specific storage
   ITOPIC=TSTO; SCL_D=PBMAN%INT(TSTO); SCL_U=2; IINV=0

   PRJILIST=ITOPIC; IF(PMANAGER_GETFNAMES(ILAY,ILAY,0,1,0).LE.0)RETURN

   IF(.NOT.PMANAGER_SAVEMF2005_MOD_READ(STO(ILAY),ITOPIC,1,SCL_D,SCL_U,IINV,IPRT))RETURN
   CALL PMANAGER_SAVEMF2005_CORRECT(ILAY,BND,STO(ILAY),0,ITOPIC)

   !## sf2 - specific yield in case not confined
   IF(LAYCON(ILAY).NE.1)THEN

    ITOPIC=TSPY; SCL_D=PBMAN%INT(TSPY); SCL_U=2; IINV=0

    PRJILIST=ITOPIC; IF(PMANAGER_GETFNAMES(ILAY,ILAY,0,1,0).LE.0)RETURN

    IF(.NOT.PMANAGER_SAVEMF2005_MOD_READ(SPY(ILAY),ITOPIC,1,SCL_D,SCL_U,IINV,IPRT))RETURN
    CALL PMANAGER_SAVEMF2005_CORRECT(ILAY,BND,SPY(ILAY),0,ITOPIC)

   ENDIF
  ENDIF
   
  !## quasi-3d scheme add vertical hydraulic conductivity of interbed
  IF(LQBD.AND.ILAY.NE.PRJNLAY)THEN

   !## kvv
   ITOPIC=TKVV; SCL_D=PBMAN%INT(TKVV); SCL_U=3; IINV=0

   PRJILIST=ITOPIC; IF(PMANAGER_GETFNAMES(ILAY,ILAY,0,1,0).LE.0)RETURN

   IF(.NOT.PMANAGER_SAVEMF2005_MOD_READ(KVV(ILAY),ITOPIC,1,SCL_D,SCL_U,IINV,IPRT))RETURN
   CALL PMANAGER_SAVEMF2005_CORRECT(ILAY,BND,KVV(ILAY),0,ITOPIC)

  ENDIF
 ENDDO  
 
 DEALLOCATE(FNAMES,PRJILIST)

 PMANAGER_SAVEMF2005_LPF_READ=.TRUE.
 
 END FUNCTION PMANAGER_SAVEMF2005_LPF_READ

 !####====================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_COMPUTE_KDW_VCW()
 !####====================================================================
 IMPLICIT NONE
 INTEGER :: ILAY,IROW,ICOL
 REAL(KIND=DP_KIND) :: T,T1,T2,T3
 
 !## compute transmissivity - could be used by packages to assign to modellayers
 DO ILAY=1,PRJNLAY; DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
  IF(BND(ILAY)%X(ICOL,IROW).NE.0)THEN
   T=TOP(ILAY)%X(ICOL,IROW)-BOT(ILAY)%X(ICOL,IROW)
   KDW(ILAY)%X(ICOL,IROW)=T*KHV(ILAY)%X(ICOL,IROW)
  ELSE
   KDW(ILAY)%X(ICOL,IROW)=HNOFLOW
  ENDIF
 ENDDO; ENDDO; ENDDO
 DO ILAY=1,PRJNLAY-1; DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
  IF(BND(ILAY)%X(ICOL,IROW).NE.0.AND.BND(ILAY+1)%X(ICOL,IROW).NE.0)THEN
   !## top aquifer  
   T =0.5D0*(TOP(ILAY)%X(ICOL,IROW)-BOT(ILAY)%X(ICOL,IROW))
   T1=0.0D0; IF(KHV(ILAY)%X(ICOL,IROW).GT.0.0D0)T1=T/(KHV(ILAY)%X(ICOL,IROW)/KVA(ILAY)%X(ICOL,IROW))
   !## intermediate aquitard
   T =BOT(ILAY)%X(ICOL,IROW)-TOP(ILAY+1)%X(ICOL,IROW)
   T2=0.0D0
   !## zero permeability - make sure resistance is equal to minc
   IF(KVV(ILAY)%X(ICOL,IROW).LE.0.0D0)THEN
    IF(T.GT.0.0D0)THEN
     IF(PBMAN%MINC.GT.0.0D0)THEN
      KVV(ILAY)%X(ICOL,IROW)=T/PBMAN%MINC
     ELSE
      KVV(ILAY)%X(ICOL,IROW)=1.0D0
     ENDIF
    ELSE
     !## irrelevant but need to have some value otherwise MF turns it into inactive nodes
     KVV(ILAY)%X(ICOL,IROW)=1.0D0
    ENDIF
   ENDIF
   T2=T/KVV(ILAY)%X(ICOL,IROW)
   !## bottom aquifer
   T =0.5D0*(TOP(ILAY+1)%X(ICOL,IROW)-BOT(ILAY+1)%X(ICOL,IROW))
   T3=0.0D0; IF(KHV(ILAY+1)%X(ICOL,IROW).GT.0.0D0)T3=T/(KHV(ILAY+1)%X(ICOL,IROW)/KVA(ILAY+1)%X(ICOL,IROW))
   !## total resistance
   VCW(ILAY)%X(ICOL,IROW)=T1+T2+T3
  ELSE
   VCW(ILAY)%X(ICOL,IROW)=HNOFLOW
  ENDIF
 ENDDO; ENDDO; ENDDO
 
 END SUBROUTINE PMANAGER_SAVEMF2005_COMPUTE_KDW_VCW
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_LPF_SAVE(DIR,DIRMNAME,IBATCH,ISS)
 !####====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: WETDRYTHRESS=0.1D0 
 CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME
 INTEGER,INTENT(IN) :: IBATCH,ISS
 REAL(KIND=DP_KIND) :: WETFCT,T,KD,D
 INTEGER :: IU,ILAY,IFBND,IHDWET,IWETIT,IROW,ICOL
 
 PMANAGER_SAVEMF2005_LPF_SAVE=.TRUE.; IF(PBMAN%IFORMAT.EQ.3)RETURN; IF(.NOT.LLPF)RETURN
 
 !## use lpf6
 PMANAGER_SAVEMF2005_LPF_SAVE=.FALSE.

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.LPF7'//'...')
 IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.LPF7'//'...'

 !## construct lpf7-file
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.LPF7',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
 WRITE(IU,'(A)') '# LPF7 File Generated by '//TRIM(UTL_IMODVERSION())
 !## dry cells negative for restart
 LINE=TRIM(ITOS(IBCFCB))//','//TRIM(RTOS(HNOFLOW,'G',7))//',0,STORAGECOEFFICIENT,THICKSTRT,CONSTANTCV,NOCVCORRECTION'
 IF(PBMAN%MINKD.NE.0.0D0)LINE=TRIM(LINE)//',MINKD '//TRIM(RTOS(PBMAN%MINKD,'G',5))
 IF(PBMAN%MINC .NE.0.0D0)LINE=TRIM(LINE)//',MINC ' //TRIM(RTOS(PBMAN%MINC ,'G',5))
 WRITE(IU,'(A)') TRIM(LINE)

 !## laycon=1: 0
 !## laycon=2: 1
 !## laycon=3:-1
 !## laycon=4: constant head

 !## laytyp code
 LINE=''; DO ILAY=1,PRJNLAY
  SELECT CASE (LAYCON(ILAY))
   CASE (1); LINE=TRIM(LINE)//' 0,' !## confined
   CASE (2); LINE=TRIM(LINE)//' 1,' !## convertible head-bot
   CASE (3); LINE=TRIM(LINE)//'-1,' !## convertible shd/top-bot
   CASE (4); LINE=TRIM(LINE)//' 0,' !## constant head
  END SELECT
  IF(MOD(ILAY,40).EQ.0)THEN; WRITE(IU,'(A)') LINE(:LEN_TRIM(LINE)-1); LINE=''; ENDIF
 ENDDO; IF(LEN_TRIM(LINE).NE.0)WRITE(IU,'(A)') LINE(:LEN_TRIM(LINE)-1)
 !## layavg code
 LINE=''; DO ILAY=1,PRJNLAY; LINE=TRIM(LINE)//'0,'
  IF(MOD(ILAY,40).EQ.0)THEN; WRITE(IU,'(A)') LINE(:LEN_TRIM(LINE)-1); LINE=''; ENDIF
 ENDDO; IF(LEN_TRIM(LINE).NE.0)WRITE(IU,'(A)') LINE(:LEN_TRIM(LINE)-1)
 !## chani code
 LINE=''; DO ILAY=1,PRJNLAY; LINE=TRIM(LINE)//'1.0D0,'
  IF(MOD(ILAY,20).EQ.0)THEN; WRITE(IU,'(A)') LINE(:LEN_TRIM(LINE)-1); LINE=''; ENDIF
 ENDDO; IF(LEN_TRIM(LINE).NE.0)WRITE(IU,'(A)') LINE(:LEN_TRIM(LINE)-1)
 !## lvka code
 LINE=''; DO ILAY=1,PRJNLAY; LINE=TRIM(LINE)//'1,'
  IF(MOD(ILAY,20).EQ.0)THEN; WRITE(IU,'(A)') LINE(:LEN_TRIM(LINE)-1); LINE=''; ENDIF
 ENDDO; IF(LEN_TRIM(LINE).NE.0)WRITE(IU,'(A)') LINE(:LEN_TRIM(LINE)-1)

 !## laywet code - if unconfined always use wetdry
 LINE=''; IWETIT=0
 DO ILAY=1,PRJNLAY
  !## not unconfined
  IF(LAYCON(ILAY).NE.2)LINE=TRIM(LINE)//'0,'
  !## unconfined
  IF(LAYCON(ILAY).EQ.2)THEN; LINE=TRIM(LINE)//'1,'; IWETIT=1; ENDIF
  IF(MOD(ILAY,20).EQ.0)THEN; WRITE(IU,'(A)') LINE(:LEN_TRIM(LINE)-1); LINE=''; ENDIF
 ENDDO; IF(LEN_TRIM(LINE).NE.0)WRITE(IU,'(A)') LINE(:LEN_TRIM(LINE)-1)
 !## include wetdry options
 IF(IWETIT.EQ.1)THEN
  WETFCT=0.1 !## multiplication to determine head in dry cell 
  IHDWET=0   !## option to compute rewetted model layers; h = BOT + WETFCT (hn - BOT) 
  LINE=TRIM(RTOS(WETFCT,'F',2))//','//TRIM(ITOS(IWETIT))//','//TRIM(ITOS(IHDWET))
  WRITE(IU,'(A)') TRIM(LINE)
 ENDIF
  
 !## check all on active cells, except wetdry
 IFBND=1
 
 DO ILAY=1,PRJNLAY

  IF(PBMAN%MINKD.GT.0.0D0)THEN
   DO IROW=1,KHV(ILAY)%NROW; DO ICOL=1,KHV(ILAY)%NCOL
    D=TOP(ILAY)%X(ICOL,IROW)-BOT(ILAY)%X(ICOL,IROW)
    IF(D.GT.0.0D0)THEN
     KD=D*KHV(ILAY)%X(ICOL,IROW)
     IF(KD.LT.PBMAN%MINKD)KHV(ILAY)%X(ICOL,IROW)=PBMAN%MINKD/D
    ENDIF
   ENDDO; ENDDO
  ENDIF
  
  !## hk
  IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\LPF7\HK_L'//TRIM(ITOS(ILAY))//'.ARR', &
      KHV(ILAY),0,IU,ILAY,IFBND))RETURN

  !## vka
  IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\LPF7\VKA_L'//TRIM(ITOS(ILAY))//'.ARR', &
      KVA(ILAY),0,IU,ILAY,IFBND))RETURN

  !## transient simulation
  IF(ISS.EQ.1)THEN

   !## sf1 - specific storage
   IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\LPF7\SF1_L'//TRIM(ITOS(ILAY))//'.ARR', &
       STO(ILAY),0,IU,ILAY,IFBND))RETURN

   !## sf2 - specific yield in case not confined
   IF(LAYCON(ILAY).NE.1)THEN
    IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\LPF7\SF2_L'//TRIM(ITOS(ILAY))//'.ARR', &
        SPY(ILAY),0,IU,ILAY,IFBND))RETURN
   ENDIF
   
  ENDIF
   
  !## quasi-3d scheme add vertical hydraulic conductivity of interbed
  IF(LQBD.AND.ILAY.NE.PRJNLAY)THEN

   !## kvv
   IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\LPF7\VKCB_L'//TRIM(ITOS(ILAY))//'.ARR', &
       KVV(ILAY),0,IU,ILAY,IFBND))RETURN

  ENDIF

  !## add wetdry options - lakes/inactive cells cannot be rewetted)
  IF(LAYCON(ILAY).NE.1.AND.IWETIT.EQ.1)THEN
   !## fill wetdry thresholds
   PRJIDF%X=0.0D0 
   DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
    IF(BND(ILAY)%X(ICOL,IROW).GT.0)THEN
     T=TOP(ILAY)%X(ICOL,IROW)-BOT(ILAY)%X(ICOL,IROW)
     !## only cells below can rewet - more stable
     IF(ILAY.LT.PRJNLAY)THEN
      PRJIDF%X(ICOL,IROW)=-MIN(WETDRYTHRESS,MAX(0.0,T))
     ELSE
      PRJIDF%X(ICOL,IROW)= MIN(WETDRYTHRESS,MAX(0.0,T))
     ENDIF
    ENDIF
   ENDDO; ENDDO
   IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\LPF7\WETDRY_L'//TRIM(ITOS(ILAY))//'.ARR', &
       PRJIDF,0,IU,ILAY,0))RETURN
  ENDIF

!The two most important variables that affect stability are the wetting 
!threshold and which neighboring cells are checked to determine if a cell 
!should be wetted. Both of these are controlled through WETDRY. It is 
!often useful to look at the output file and identify cells that convert 
!repeatedly from wet to dry. Try raising the wetting threshold for those 
!cells. It may also be worthwhile looking at the boundary conditions 
!associated with dry cells. 

 ENDDO  
  
 CLOSE(IU)

 PMANAGER_SAVEMF2005_LPF_SAVE=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_LPF_SAVE

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_NPF_SAVE(DIR,DIRMNAME,IBATCH)
 !####====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: WETDRYTHRESS=0.1D0
 CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME
 INTEGER,INTENT(IN) :: IBATCH
 REAL(KIND=DP_KIND) :: WETFCT,T,KDMIN,KD,THICK
 INTEGER :: IU,ILAY,JLAY,IFBND,IHDWET,IWETIT,IROW,ICOL
 LOGICAL :: LEX
 
 PMANAGER_SAVEMF2005_NPF_SAVE=.TRUE.; IF(PBMAN%IFORMAT.EQ.2)RETURN;  IF(.NOT.LNPF)RETURN

 !## use npf6
 PMANAGER_SAVEMF2005_NPF_SAVE=.FALSE.

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.NPF6'//'...')
 IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.NPF6'//'...'

 !## construct npf6-file
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.NPF6',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
 WRITE(IU,'(A)') '# NPF6 File Generated by '//TRIM(UTL_IMODVERSION())

 WRITE(IU,'(/A/)') '#General Options'
 WRITE(IU,'(A)') 'BEGIN OPTIONS'
 LEX=.FALSE.
 IF(ASSOCIATED(PBMAN%ISAVE(TKHV)%ILAY))LEX=.TRUE.
 IF(ASSOCIATED(PBMAN%ISAVE(TKVV)%ILAY))LEX=.TRUE.
 IF(ASSOCIATED(PBMAN%ISAVE(TKVA)%ILAY))LEX=.TRUE.
 IF(LEX)WRITE(IU,'(A)') ' SAVE_FLOWS'
 WRITE(IU,'(A)') ' ALTERNATIVE_CELL_AVERAGING AMT-HMK'
 WRITE(IU,'(A)') ' THICKSTRT'
!THICKSTRT�indicates that cells having a negative ICELLTYPE are confined, and their cell thickness
! WRITE(IU,'(A)') ' [VARIABLECV [DEWATERED]]'
!If these keywords are not specified, then the default condition is to calculate the
!vertical conductance at the start of the simulation using the initial head and the cell properties. The
!vertical conductance remains constant for the entire simulation. WRITE(IU,'(A)') ' [PERCHED]'
 !## see if layer is unconfined and wettable
 WETFCT=0.1 !## multiplication to determine head in dry cell 
 IHDWET=0   !## is a keyword and integer flag that determines which equation is used to define the initial head at cells that become wet.
 IWETIT=0   !## is a keyword and iteration interval for attempting to wet cells
 DO ILAY=1,PRJNLAY
  IF(LAYCON(ILAY).EQ.2)EXIT
 ENDDO
 IF(ILAY.LE.PRJNLAY)THEN
  IWETIT=1
  WRITE(IU,'(A)') ' REWET WETFCT '//TRIM(RTOS(WETFCT,'F',3))// &
     ' IWETIT '//TRIM(ITOS(IWETIT))//' IHDWET '//TRIM(ITOS(IHDWET))
 ENDIF
! WRITE(IU,'(A)') ' [XT3D [RHS]]'
! WRITE(IU,'(A)') ' [SAVE_SPECIFIC_DISCHARGE]'
 WRITE(IU,'(A)') 'END OPTIONS'

 WRITE(IU,'(/A/)') '#Geology Options'
 WRITE(IU,'(A)') 'BEGIN GRIDDATA'
 WRITE(IU,'(A)') ' ICELLTYPE LAYERED'
 DO ILAY=1,SIZE(PBMAN%ILAY) !PRJNLAY
  IF(PBMAN%ILAY(ILAY).EQ.0)CYCLE
  IF(LAYCON(ILAY).EQ.1)WRITE(IU,'(A)') '  CONSTANT 0'    !## confined
  IF(LAYCON(ILAY).EQ.2)WRITE(IU,'(A)') '  CONSTANT 1'    !## convertible head-bot
  IF(LAYCON(ILAY).EQ.3)WRITE(IU,'(A)') '  CONSTANT -1'   !## convertible shd/top-bot
 ENDDO
 
 !## mf6 needs minimal k for layers with thickness of zero
 KDMIN=MAX(0.01D0,PBMAN%MINKD)
 DO ILAY=1,SIZE(PBMAN%ILAY)

  IF(KDMIN.GT.0.0D0)THEN
   DO IROW=1,KHV(ILAY)%NROW; DO ICOL=1,KHV(ILAY)%NCOL
    THICK=TOP(ILAY)%X(ICOL,IROW)-BOT(ILAY)%X(ICOL,IROW)
    IF(THICK.GT.0.0D0)THEN
     KD=THICK*KHV(ILAY)%X(ICOL,IROW)
     IF(KD.LT.KDMIN)KHV(ILAY)%X(ICOL,IROW)=KDMIN/THICK
    ENDIF
   ENDDO; ENDDO
  ENDIF
 ENDDO

 WRITE(IU,'(A)') ' K LAYERED'
 JLAY=0; DO ILAY=1,SIZE(PBMAN%ILAY) 
  IF(PBMAN%ILAY(ILAY).EQ.0)CYCLE
  JLAY=JLAY+1
  !## hk
  IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\NPF\K_L'//TRIM(ITOS(JLAY))//'.ARR', &
      KHV(ILAY),0,IU,ILAY,IFBND))RETURN
 ENDDO

! WRITE(IU,'(A)') ' K22 LAYERED'
!<k22(nodes)> -- READARRAY]

 !## vertical k-value
 WRITE(IU,'(A)') ' K33 LAYERED'
 JLAY=0; DO ILAY=1,SIZE(PBMAN%ILAY) 
  IF(PBMAN%ILAY(ILAY).EQ.0)CYCLE
  JLAY=JLAY+1
  PRJIDF%X=0.0D0
  DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
   IF(BND(ILAY)%X(ICOL,IROW).NE.0)THEN
    PRJIDF%X(ICOL,IROW)=KHV(ILAY)%X(ICOL,IROW)/KVA(ILAY)%X(ICOL,IROW)
   ENDIF
  ENDDO; ENDDO
  IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\NPF\K33_L'//TRIM(ITOS(JLAY))//'.ARR', &
     PRJIDF,0,IU,ILAY,IFBND))RETURN
 ENDDO

! WRITE(IU,'(A)') ' ANGLE1 LAYERED'
! WRITE(IU,'(A)') ' ANGLE2 LAYERED'
! WRITE(IU,'(A)') ' ANGLE3 LAYERED'

 IF(IWETIT.EQ.1)THEN
  WRITE(IU,'(A)') ' WETDRY LAYERED'
  JLAY=0; DO ILAY=1,SIZE(PBMAN%ILAY) 
   IF(PBMAN%ILAY(ILAY).EQ.0)CYCLE
   JLAY=JLAY+1
   !## add wetdry options - lakes/inactive cells cannot be rewetted)
   IF(LAYCON(ILAY).NE.1)THEN
    !## fill wetdry thresholds
    PRJIDF%X=0.0D0 
    DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
     IF(BND(ILAY)%X(ICOL,IROW).GT.0)THEN
      T=TOP(ILAY)%X(ICOL,IROW)-BOT(ILAY)%X(ICOL,IROW)
      !## only cells below can rewet - more stable
      IF(ILAY.LT.PRJNLAY)THEN
       PRJIDF%X(ICOL,IROW)=-MIN(WETDRYTHRESS,T)
      ELSE
       PRJIDF%X(ICOL,IROW)= MIN(WETDRYTHRESS,T)
      ENDIF
     ENDIF
    ENDDO; ENDDO
    IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\NPF\WETDRY_L'//TRIM(ITOS(JLAY))//'.ARR', &
        PRJIDF,0,IU,ILAY,0))RETURN
   ENDIF
  ENDDO
 ENDIF
 WRITE(IU,'(A)') 'END GRIDDATA'
  
 CLOSE(IU)

 PMANAGER_SAVEMF2005_NPF_SAVE=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_NPF_SAVE
 
  !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_STO_SAVE(DIR,DIRMNAME,IBATCH,ISS)
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME
 INTEGER,INTENT(IN) :: IBATCH,ISS
 INTEGER :: IU,ILAY,ISY,KPER
 
 PMANAGER_SAVEMF2005_STO_SAVE=.TRUE.; IF(PBMAN%IFORMAT.EQ.2)RETURN; IF(ISS.EQ.0)RETURN

 !## use sto6
 PMANAGER_SAVEMF2005_STO_SAVE=.FALSE.

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.STO6'//'...')
 IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.STO6'//'...'

 !## construct npf6-file
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.STO6',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
 WRITE(IU,'(A)') '# STO6 File Generated by '//TRIM(UTL_IMODVERSION())

 WRITE(IU,'(/A/)') '#General Options'
 WRITE(IU,'(A)') 'BEGIN OPTIONS'
 WRITE(IU,'(A)') ' SAVE_FLOWS'
 WRITE(IU,'(A)') ' STORAGECOEFFICIENT'  !## specific coefficient given if NOT mentioned
 WRITE(IU,'(A)') 'END OPTIONS'

 WRITE(IU,'(/A/)') '#Geology Options'
 WRITE(IU,'(A)') 'BEGIN GRIDDATA'
 WRITE(IU,'(A)') ' ICONVERT LAYERED'
 ISY=0
 DO ILAY=1,PRJNLAY
  IF(LAYCON(ILAY).EQ.2)THEN
   WRITE(IU,'(A)') '  CONSTANT 1'    !## confined storage
  ELSE
   WRITE(IU,'(A)') '  CONSTANT 0'    !## convertible storage
   ISY=1
  ENDIF
 ENDDO

 WRITE(IU,'(A)') ' SS LAYERED'
 DO ILAY=1,PRJNLAY
  !## hk
  IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\STO\SS_L'//TRIM(ITOS(ILAY))//'.ARR', &
      STO(ILAY),0,IU,ILAY,1))RETURN
 ENDDO
 IF(ISY.EQ.1)THEN
  WRITE(IU,'(A)') ' SY LAYERED'
  DO ILAY=1,PRJNLAY
   !## hk
   IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\STO\SY_L'//TRIM(ITOS(ILAY))//'.ARR', &
       SPY(ILAY),0,IU,ILAY,1))RETURN
  ENDDO
 ENDIF
 WRITE(IU,'(A)') 'END GRIDDATA'
  
 WRITE(IU,'(/A/)') '#Time Storage Options'
 DO KPER=1,PRJNPER
  WRITE(IU,'(A)') 'BEGIN PERIOD '//TRIM(ITOS(KPER))
  IF(SIM(KPER)%DELT.EQ.0.0D0)WRITE(IU,'(A)') ' STEADY-STATE'
  IF(SIM(KPER)%DELT.NE.0.0D0)WRITE(IU,'(A)') ' TRANSIENT'
  WRITE(IU,'(A)') 'END PERIOD'
 ENDDO
 
 CLOSE(IU)

 PMANAGER_SAVEMF2005_STO_SAVE=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_STO_SAVE

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_ANI_READ(IPRT)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPRT
 INTEGER :: ILAY,SCL_D,SCL_U,IINV,ITOPIC,NTOP,NSYS,ISYS,KTOP,ICNST
 REAL(KIND=DP_KIND) :: FCT,CNST,IMP
 CHARACTER(LEN=256) :: SFNAME
  
 PMANAGER_SAVEMF2005_ANI_READ=.TRUE.

 !## use ani1
 IF(.NOT.LANI)RETURN

 WRITE(*,'(/A)') 'Reading ANI-files ...'

 PMANAGER_SAVEMF2005_ANI_READ=.FALSE.

 !## ani angle
 IINV=0; ITOPIC=TANI
 
 !## allocate memory for packages
 NTOP=SIZE(TOPICS(ITOPIC)%STRESS(1)%FILES,1); NSYS=SIZE(TOPICS(ITOPIC)%STRESS(1)%FILES,2)
 
 !## fill with default values 
 DO ILAY=1,PRJNLAY; ANF(ILAY)%X=1.0D0; ANA(ILAY)%X=0.0D0; ANF(ILAY)%NODATA=HUGE(1.0); ANA(ILAY)%NODATA=HUGE(1.0); ENDDO
 
 !## number of systems
 DO ISYS=1,NSYS

  WRITE(6,'(A)') '+Reading ANI-files ('//TRIM(RTOS(REAL(100*ISYS,8)/REAL(NSYS,8),'F',2))//'%)'     

  !## number of subtopics
  DO KTOP=1,NTOP

   ICNST =TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%ICNST 
   CNST  =TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%CNST  
   FCT   =TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%FCT   
   IMP   =TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%IMP   
   ILAY  =TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%ILAY  
   SFNAME=TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%FNAME 

   WRITE(IPRT,'(1X,A,I3,A1,I1,A1,I4.3,3(A1,G15.7),A1,A)') TOPICS(ITOPIC)%TNAME(1:5)//',', &
      ISYS,',',ICNST,',',ILAY,',',FCT,',',IMP,',',CNST,',',CHAR(39)//TRIM(SFNAME)//CHAR(39)

   !## average factor
   IF(KTOP.EQ.1)THEN
   
    !## constant value
    IF(ICNST.EQ.1)THEN
     ANF(ILAY)%X=CNST
    !## read/clip/scale idf file
    ELSEIF(TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%ICNST.EQ.2)THEN
     ANF(ILAY)%FNAME=SFNAME
     SCL_U=2
     SCL_D=PBMAN%INT(TANI) !## factors can be interpolated
     IF(.NOT.IDFREADSCALE(ANF(ILAY)%FNAME,ANF(ILAY),SCL_U,SCL_D,1.0D0,0))RETURN
    ENDIF
    CALL PMANAGER_SAVEMF2005_FCTIMP(0,ICNST,ANF(ILAY),FCT,IMP,SCL_D)
    CALL PMANAGER_SAVEMF2005_CORRECT(ILAY,BND,ANF(ILAY),0,ITOPIC)
   
   !## most frequent occurence for angles
   ELSEIF(KTOP.EQ.2)THEN

    !## constant value
    IF(ICNST.EQ.1)THEN
     ANA(ILAY)%X=CNST
    !## read/clip/scale idf file
    ELSEIF(TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%ICNST.EQ.2)THEN
     ANA(ILAY)%FNAME=SFNAME
     SCL_U=7
     SCL_D=0 !## no interpolation of angles 
     IF(.NOT.IDFREADSCALE(ANA(ILAY)%FNAME,ANA(ILAY),SCL_U,SCL_D,1.0D0,0))RETURN
    ENDIF
    CALL PMANAGER_SAVEMF2005_FCTIMP(0,ICNST,ANA(ILAY),FCT,IMP,SCL_D)
    CALL PMANAGER_SAVEMF2005_CORRECT(ILAY,BND,ANA(ILAY),0,ITOPIC)

   ENDIF   
  
  ENDDO 
 ENDDO  
 
 PMANAGER_SAVEMF2005_ANI_READ=.TRUE.
 
 END FUNCTION PMANAGER_SAVEMF2005_ANI_READ

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_ANI_SAVE(DIR,DIRMNAME,IBATCH)
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: IU,ILAY,IFBND
  
 PMANAGER_SAVEMF2005_ANI_SAVE=.TRUE.

 !## use ani1
 IF(.NOT.LANI)RETURN

 PMANAGER_SAVEMF2005_ANI_SAVE=.FALSE.

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.ANI1'//'...')
 IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.ANI1'//'...'

 !## construct ani1-file
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.ANI1',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN

 DO ILAY=1,PRJNLAY
  !## anisotropy factors
  IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\ANI1\ANF_L'//TRIM(ITOS(ILAY))//'.ARR', &
      ANF(ILAY),0,IU,ILAY,IFBND))RETURN
  !## anisotropy angle
  IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\ANI1\ANA_L'//TRIM(ITOS(ILAY))//'.ARR', &
      ANA(ILAY),0,IU,ILAY,IFBND))RETURN
 ENDDO
 
 CLOSE(IU)

 PMANAGER_SAVEMF2005_ANI_SAVE=.TRUE.
 
 END FUNCTION PMANAGER_SAVEMF2005_ANI_SAVE
  
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_WEL(DIR,DIRMNAME,IBATCH,LEX,ITOPIC,ICB,CPCK,IPRT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH,ICB,ITOPIC,IPRT
 CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME,CPCK
 LOGICAL,INTENT(IN) :: LEX
 REAL(KIND=DP_KIND) :: X,Y,Q,Z1,Z2,FCT,IMP,CNST,NCOUNT
 CHARACTER(LEN=256) :: SFNAME,EXFNAME,ID,CDIR
 CHARACTER(LEN=5) :: EXT
 CHARACTER(LEN=30) :: FRM
 CHARACTER(LEN=52),ALLOCATABLE,DIMENSION(:) :: STRING
 INTEGER :: IU,JU,KU,ILAY,IROW,ICOL,I,J,NROWIPF,NCOLIPF,IEXT,IOS,N,KPER,IPER,NP,MP,ICNST,ISYS,NSYS,ISS
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: TLP,KH,TP,BT
 INTEGER(KIND=8) :: ITIME,JTIME
 REAL(KIND=DP_KIND),PARAMETER :: MINKHT=0.0D0
! INTEGER,PARAMETER :: ICLAY=1 !## shift to nearest aquifer
 CHARACTER(LEN=1) :: VTXT
 
 IF(.NOT.LEX)THEN; PMANAGER_SAVEMF2005_WEL=.TRUE.; RETURN; ENDIF

 PMANAGER_SAVEMF2005_WEL=.FALSE.
 
 VTXT='7'; IF(PBMAN%IFORMAT.EQ.3)VTXT='6'

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.'//CPCK//VTXT//'...')
 IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.'//CPCK//VTXT//'...'
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.'//CPCK//VTXT//'_',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
 IF(IU.EQ.0)RETURN

 IF(PBMAN%IFORMAT.EQ.3)THEN
  WRITE(IU,'(A)') '# '//CPCK//VTXT//' File Generated by '//TRIM(UTL_IMODVERSION())
  WRITE(IU,'(/A/)') '#General Options'  
  WRITE(IU,'(A)') 'BEGIN OPTIONS'
!  WRITE(IU,'(A)') ' AUXILIARY <auxiliary(naux)>'
!  WRITE(IU,'(A)') ' AUXMULTNAME <auxmultname>'
!  WRITE(IU,'(A)') ' BOUNDNAMES'
!  WRITE(IU,'(A)') ' PRINT_INPUT'
!  WRITE(IU,'(A)') ' PRINT_FLOWS'
  IF(ASSOCIATED(PBMAN%ISAVE(TWEL)%ILAY))WRITE(IU,'(A)') ' SAVE_FLOWS'
!  WRITE(IU,'(A)') ' TS6 FILEIN <ts6_filename>'
!  WRITE(IU,'(A)') ' OBS6 FILEIN <obs6_filename>'
!  WRITE(IU,'(A)') ' MOVER'
  WRITE(IU,'(A)') 'END OPTIONS'
  WRITE(IU,'(/A/)') '#General Dimensions'  
  WRITE(IU,'(A)') 'BEGIN DIMENSIONS'
  WRITE(IU,'(1X,A)') ' MAXBOUND NaN1#'
  WRITE(IU,'(A)') 'END DIMENSIONS'
 ENDIF

! IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.'//CPCK//'7...')
! IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.'//CPCK//'7...'

! IU=UTL_GETUNIT()
! CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.'//CPCK//'7_',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
! IF(IU.EQ.0)RETURN

 !## header
 LINE='NaN1#,'//TRIM(ITOS(ICB))//' AUX ISUB WSUBSYS ISUB NOPRINT'
 IF(PBMAN%IFORMAT.EQ.2)WRITE(IU,'(A)') TRIM(LINE) 
 
 !## fill tlp for each modellayer
 ALLOCATE(TLP(PRJNLAY),KH(PRJNLAY),TP(PRJNLAY),BT(PRJNLAY))

 WRITE(FRM,'(A9,I2.2,A15)') '(3(I5,1X),',1,'(G15.7,1X),I10)'

 !## create subfolders
 IF(PBMAN%IFORMAT.EQ.2)CALL UTL_CREATEDIR(TRIM(DIR)//'\'//CPCK//'7')

 !## maximum number of well in simulation
 MP=0
 IOS=0
 
 DO IPER=1,PRJNPER

  !## number of wells per stressperiod
  NP=0
  
  !## get appropriate stress-period to store in runfile   
  KPER=PMANAGER_GETCURRENTIPER(IPER,ITOPIC,ITIME,JTIME)
  
  !## always export wells per stress-period
  IF(PBMAN%DWEL.EQ.1)KPER=ABS(KPER)
  
  !## output
  WRITE(IPRT,'(1X,A,2I10,2(1X,I14))') 'Exporting timestep ',IPER,KPER,ITIME,JTIME  
  IF(IBATCH.EQ.1)WRITE(6,'(A,3I6,2(1X,I14))') '+Exporting timestep ',IPER,PRJNPER,KPER,ITIME,JTIME  
  
  !## reuse previous timestep
  IF(KPER.LE.0)THEN
   IF(PBMAN%IFORMAT.EQ.2)THEN
    IF(IPER.EQ.1)THEN; WRITE(IU,'(I10)') 0
    ELSE; WRITE(IU,'(I10)') -1; ENDIF
   ENDIF
   !## goto next timestep
   CYCLE
  ENDIF

  IF(PBMAN%IFORMAT.EQ.3)WRITE(IU,'(/A)') 'BEGIN PERIOD '//TRIM(ITOS(IPER))

  JU=0
  !## create subfolders
  IF(PBMAN%IFORMAT.EQ.2)THEN
   CALL UTL_CREATEDIR(TRIM(DIR)//'\'//CPCK//'7')
   EXFNAME=TRIM(DIR)//'\'//CPCK//'7'//'\'//CPCK//'_T'//TRIM(ITOS(IPER))//'.ARR'
  ELSE
   CALL UTL_CREATEDIR(TRIM(DIR)//'\'//CPCK//'6')
   EXFNAME=TRIM(DIR)//'\'//CPCK//'6'//'\'//CPCK//'_T'//TRIM(ITOS(IPER))//'.ARR'
  ENDIF
  JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=EXFNAME,STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IOS=0; IF(JU.EQ.0)THEN; IOS=-1; EXIT; ENDIF
!  ELSE
!   JU=IU
!  ENDIF

!  !## create subfolders
!  CALL UTL_CREATEDIR(TRIM(DIR)//'\'//CPCK//'7')

!  EXFNAME=TRIM(DIR)//'\'//CPCK//'7'//'\'//CPCK//'_T'//TRIM(ITOS(IPER))//'.ARR'
!  JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=EXFNAME,STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IOS=0; IF(JU.EQ.0)THEN; IOS=-1; EXIT; ENDIF

  !## number of systems
  NSYS=SIZE(TOPICS(ITOPIC)%STRESS(KPER)%FILES,2)

  DO ISYS=1,NSYS

   ICNST =TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%ICNST
   CNST  =TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%CNST
   FCT   =TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%FCT
   IMP   =TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%IMP
   ILAY  =TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%ILAY
   SFNAME=TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%FNAME

   WRITE(IPRT,'(1X,A,I3,A1,I1,A1,I4.3,3(A1,G15.7),A1,A)') TOPICS(ITOPIC)%TNAME(1:5)//',', &
      ISYS,',',ICNST,',',ILAY,',',FCT,',',IMP,',',CNST,',',CHAR(39)//TRIM(SFNAME)//CHAR(39)
   
   CDIR=SFNAME(:INDEX(SFNAME,'\',.TRUE.)-1)   
 
   KU=UTL_GETUNIT(); CALL OSD_OPEN(KU,SFNAME,STATUS='OLD',ACTION='READ',FORM='FORMATTED'); IF(KU.EQ.0)RETURN
   READ(KU,*,IOSTAT=IOS) NROWIPF; IF(IOS.NE.0)EXIT
   READ(KU,*,IOSTAT=IOS) NCOLIPF; IF(IOS.NE.0)EXIT
   DO I=1,NCOLIPF
    READ(KU,*,IOSTAT=IOS); IF(IOS.NE.0)EXIT
   ENDDO
   READ(KU,*,IOSTAT=IOS) IEXT,EXT; IF(IOS.NE.0)EXIT

   N=MAX(3,IEXT); IF(ILAY.EQ.0)N=MAX(5,IEXT)
   IF(N.GT.NCOLIPF)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD need at least '//TRIM(ITOS(N))//' columns however it reads only '//TRIM(ITOS(NCOLIPF))//' from:'//CHAR(13)// &
       TRIM(SFNAME),'Error'); EXIT
   ENDIF
   ALLOCATE(STRING(N)); STRING=''
    
   !## steady-state/transient timestep
   ISS=1; IF(SIM(IPER)%DELT.GT.0.0D0)ISS=2
   !## overrule in case of steady-state
   IF(ISS.EQ.1)IEXT=0
   
   DO I=1,NROWIPF

    !## start with current given layer number
    ILAY=TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%ILAY

    READ(KU,*,IOSTAT=IOS) (STRING(J),J=1,N); IF(IOS.NE.0)EXIT
    
    READ(STRING(1),*,IOSTAT=IOS) X; IF(IOS.NE.0)EXIT
    READ(STRING(2),*,IOSTAT=IOS) Y; IF(IOS.NE.0)EXIT

    !## get correct cell-indices
    CALL IDFIROWICOL(BND(1),IROW,ICOL,X,Y)
    !## outside current model
    IF(IROW.EQ.0.OR.ICOL.EQ.0)CYCLE

    !## get discharge - always on position 3
    IF(IEXT.EQ.0)THEN
     READ(STRING(3),*,IOSTAT=IOS) Q; IF(IOS.NE.0)EXIT
    ELSE
     !## get id number - can be any column
     READ(STRING(IEXT),*,IOSTAT=IOS) ID; IF(IOS.NE.0)EXIT
    ENDIF

    !## assign to several layer
    IF(ILAY.EQ.0)THEN
      
     READ(STRING(4),*,IOSTAT=IOS) Z1; IF(IOS.NE.0)EXIT
     READ(STRING(5),*,IOSTAT=IOS) Z2; IF(IOS.NE.0)EXIT

     !## get filter fractions
     CALL PMANAGER_SAVEMF2005_PCK_ULSTRD_PARAM(PRJNLAY,ICOL,IROW,BND,TOP,BOT,KDW,TP,BT,KH,.TRUE.)
     CALL UTL_PCK_GETTLP(PRJNLAY,TLP,KH,TP,BT,Z1,Z2,MINKHT) 

    !## find uppermost layer
    ELSE
       
     IF(ILAY.EQ.-1)THEN; DO ILAY=1,PRJNLAY; IF(BND(ILAY)%X(ICOL,IROW).GT.0)EXIT; ENDDO; ENDIF
     !## outside current model dimensions, set ilay=0
     IF(ILAY.GT.PRJNLAY)ILAY=0; TLP=0.0D0; IF(ILAY.NE.0)TLP(ILAY)=1.0D0

    ENDIF
     
    IF(IEXT.GT.0)THEN
     IF(.NOT.UTL_PCK_READTXT(2,ITIME,JTIME,Q,TRIM(CDIR)//'\'//TRIM(ID)//'.'//TRIM(EXT),0,'',ISS,NCOUNT))THEN
      IOS=-1; EXIT
     ENDIF
     IF(NCOUNT.LE.0.0D0)Q=0.0D0
    ENDIF
         
    !## use factor/impulse
    Q=Q*FCT; Q=Q+IMP  

    IF(Q.NE.0.0D0)THEN
    
     !## only active cells
     DO ILAY=1,PRJNLAY
      IF(BND(ILAY)%X(ICOL,IROW).LE.0.0D0)TLP(ILAY)=0.0D0
     ENDDO
     !## normalize tlp() again
     IF(SUM(TLP).GT.0.0D0)TLP=(1.0D0/SUM(TLP))*TLP

     DO ILAY=1,PRJNLAY
      IF(TLP(ILAY).GT.0.0D0)THEN
       WRITE(JU,FRM) ILAY,IROW,ICOL,Q*TLP(ILAY),ISYS
       NP=NP+1
      ENDIF
     ENDDO
    ENDIF
     
   ENDDO
  
   DEALLOCATE(STRING)
   CLOSE(KU)

   IF(IOS.NE.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading IPF file'//CHAR(13)//TRIM(SFNAME)//CHAR(13)// &
       'Linenumber '//TRIM(ITOS(I))//CHAR(13)//'iMOD Probably cannot read values for top and bot in combination with ilay=0','Error'); EXIT
   ENDIF     
   
  ENDDO

  IF(NP.GT.0)THEN
   IF(PBMAN%IFORMAT.EQ.2)CALL IDFWRITEFREE_HEADER(JU,PRJIDF)
   CLOSE(JU)
  ELSE
!   IF(PBMAN%IFORMAT.EQ.2)THEN
   CLOSE(JU,STATUS='DELETE')
!   ELSE
!    !## do not delete for MF6 
!    CLOSE(JU)
!   ENDIF
  ENDIF
  
  IF(IOS.NE.0)EXIT
  
  !## store maximum number of well in simulation
  MP=MAX(MP,NP)
  
  IF(PBMAN%IFORMAT.GE.2)THEN
   IF(PBMAN%IFORMAT.EQ.2)THEN
    LINE=TRIM(ITOS(NP)); WRITE(IU,'(A)') TRIM(LINE)
   ENDIF
   IF(NP.GT.0)THEN
    SFNAME=EXFNAME
    N=3; IF(PBMAN%IFORMAT.EQ.3)N=4; DO I=1,N; SFNAME=SFNAME(:INDEX(SFNAME,'\',.TRUE.)-1); ENDDO
    I=LEN_TRIM(SFNAME); SFNAME='.'//EXFNAME(I+1:)
    WRITE(IU,'(A)') 'OPEN/CLOSE '//TRIM(SFNAME)//' 1.0D0 (FREE) -1'
    IF(PBMAN%IFORMAT.EQ.3)WRITE(IU,'(A)') 'END PERIOD '//TRIM(ITOS(IPER))
   ENDIF

  ENDIF
  
 ENDDO
 
 CLOSE(IU); DEALLOCATE(TLP,TP,BT,KH)

 IF(IOS.EQ.0)THEN
  CALL UTL_MF2005_MAXNO(TRIM(DIRMNAME)//'.'//CPCK//VTXT//'_',(/MP/))
  PMANAGER_SAVEMF2005_WEL=.TRUE.
 ENDIF
 
 END FUNCTION PMANAGER_SAVEMF2005_WEL
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_MNW(DIRMNAME,IBATCH,LEX,ITOPIC,ICB,CPCK,IPRT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH,ICB,ITOPIC,IPRT
 CHARACTER(LEN=*),INTENT(IN) :: DIRMNAME,CPCK
 LOGICAL,INTENT(IN) :: LEX
 REAL(KIND=DP_KIND) :: X,Y,Q,Z1,Z2,FCT,IMP,CNST,RW,RSKIN,KSKIN,NCOUNT
 CHARACTER(LEN=256) :: SFNAME,ID,CDIR
 CHARACTER(LEN=5) :: EXT
 CHARACTER(LEN=30) :: LOSSTYPE
 CHARACTER(LEN=52),ALLOCATABLE,DIMENSION(:) :: STRING
 INTEGER :: IU,KU,ILAY,IROW,ICOL,I,J,ISYS,NROWIPF,NCOLIPF,IEXT,IOS,N,KPER,IPER,LPER,NSYS,ICNST, &
   MNWPRINT,NNODES,ILOSSTYPE,QLIMIT,PPFLAG,PUMPLOC,PUMPCAP,ILOSS,IEQUAL
 INTEGER(KIND=8) :: ITIME,JTIME
  
 IF(.NOT.LEX)THEN; PMANAGER_SAVEMF2005_MNW=.TRUE.; RETURN; ENDIF

 PMANAGER_SAVEMF2005_MNW=.FALSE.
 
 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.'//CPCK//'7...')
 IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.'//CPCK//'7...'

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.'//CPCK//'7_',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
 IF(IU.EQ.0)RETURN

 !## maximal output information
 MNWPRINT=2
 
 !## header
 LINE='NaN1#,'//TRIM(ITOS(ICB))//','//TRIM(ITOS(MNWPRINT))//' NOPRINT'; WRITE(IU,'(A)') TRIM(LINE)
 
 !## search for first mnw definition in time - can be one only !!!
 DO IPER=1,PRJNPER
  !## get appropriate input file for first stress-period 
  KPER=PMANAGER_GETCURRENTIPER(IPER,ITOPIC,ITIME,JTIME)
  !## found appropriate stress-period
  IF(KPER.GT.0)EXIT
 ENDDO
 !## nothing found
 IF(IPER.GT.PRJNPER)KPER=0

 !## store maximum number of well in simulation
 ALLOCATE(NP_IPER(0:PRJNPER)); NP_IPER=0; LPER=0

 !## fill static-time independent information
 DO IPER=0,PRJNPER
  
  IF(IPER.GT.0)THEN

   !## get appropriate stress-period to store in runfile   
   KPER=PMANAGER_GETCURRENTIPER(IPER,ITOPIC,ITIME,JTIME)
   !## output
   WRITE(IPRT,'(1X,A,2I10,2(1X,I14))') 'Exporting timestep ',IPER,KPER,ITIME,JTIME  
   IF(IBATCH.EQ.1)WRITE(6,'(A,3I6,2(1X,I14))') '+Exporting timestep ',IPER,PRJNPER,KPER,ITIME,JTIME  

   !## always export wells per stress-period
   IF(PBMAN%DWEL.EQ.1)KPER=ABS(KPER)

  ENDIF
  
  !## reuse previous timestep
  IF(KPER.LE.0)THEN
   IF(PBMAN%IFORMAT.EQ.2)THEN
    IF(IPER.EQ.1)THEN; WRITE(IU,'(I10)') 0
    ELSE; WRITE(IU,'(I10)') -1; ENDIF
   ENDIF
   !## goto next timestep
   CYCLE
  ENDIF  
  
  IF(IPER.GT.0)THEN; LINE='NaN'//TRIM(ITOS(IPER+1))//'#'; WRITE(IU,'(A)') TRIM(LINE); ENDIF
  
  !## get number of mnw-systems
  NSYS=SIZE(TOPICS(ITOPIC)%STRESS(KPER)%FILES,2)

  DO ISYS=1,NSYS

   ICNST =TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%ICNST
   CNST  =TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%CNST
   FCT   =TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%FCT
   IMP   =TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%IMP
   ILAY  =TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%ILAY
   SFNAME=TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%FNAME

   !## check to see whether equal to previous timestep
   IEQUAL=1
   IF(LPER.GT.0)THEN
    IEQUAL=1
    IF(ICNST.EQ. TOPICS(ITOPIC)%STRESS(LPER)%FILES(1,ISYS)%ICNST.AND. &
       CNST .EQ. TOPICS(ITOPIC)%STRESS(LPER)%FILES(1,ISYS)%CNST.AND.  &
!       FCT.EQ.   TOPICS(ITOPIC)%STRESS(LPER)%FILES(1,ISYS)%FCT.AND.   &
!       IMP .EQ.  TOPICS(ITOPIC)%STRESS(LPER)%FILES(1,ISYS)%IMP.AND.   &
       ILAY.EQ.  TOPICS(ITOPIC)%STRESS(LPER)%FILES(1,ISYS)%ILAY.AND.  &
       SFNAME.EQ.TOPICS(ITOPIC)%STRESS(LPER)%FILES(1,ISYS)%FNAME)IEQUAL=1
   ENDIF
   !## for MNW it is essential that the number of files are similar during simulation
   IF(IEQUAL.EQ.-1)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'For the MNW package it is NOT allowed to specify different input files'//CHAR(13)// &
        'among different stress-periods','Error'); IOS=-1; EXIT
   ENDIF
   
   IF(IPER.GT.0)THEN
    WRITE(IPRT,'(1X,A,I3,A1,I1,A1,I4.3,3(A1,G15.7),A1,A)') TOPICS(ITOPIC)%TNAME(1:5)//',', &
      ISYS,',',ICNST,',',ILAY,',',FCT,',',IMP,',',CNST,',',CHAR(39)//TRIM(SFNAME)//CHAR(39)
   ENDIF
   
   CDIR=SFNAME(:INDEX(SFNAME,'\',.TRUE.)-1)   
 
   KU=UTL_GETUNIT(); CALL OSD_OPEN(KU,SFNAME,STATUS='OLD',ACTION='READ',FORM='FORMATTED'); IF(KU.EQ.0)THEN; IOS=-1; EXIT; ENDIF
   READ(KU,*,IOSTAT=IOS) NROWIPF; IF(IOS.NE.0)EXIT
   READ(KU,*,IOSTAT=IOS) NCOLIPF; IF(IOS.NE.0)EXIT
   DO I=1,NCOLIPF; READ(KU,*,IOSTAT=IOS); IF(IOS.NE.0)EXIT; ENDDO
   READ(KU,*,IOSTAT=IOS) IEXT,EXT; IF(IOS.NE.0)EXIT

   N=NCOLIPF; ALLOCATE(STRING(N)); STRING=''
       
   IF(ILAY.GT.0)ILOSS=4; IF(ILAY.EQ.0)ILOSS=6
   
   DO I=1,NROWIPF
   
    !## start with current given layer number
    ILAY=TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%ILAY

    READ(KU,*,IOSTAT=IOS) (STRING(J),J=1,N); IF(IOS.NE.0)EXIT

    READ(STRING(1),*,IOSTAT=IOS) X; IF(IOS.NE.0)EXIT
    READ(STRING(2),*,IOSTAT=IOS) Y; IF(IOS.NE.0)EXIT
    
    !## get correct cell-indices
    CALL IDFIROWICOL(BND(1),IROW,ICOL,X,Y)
    !## outside current model
    IF(IROW.EQ.0.OR.ICOL.EQ.0)CYCLE

    NP_IPER(IPER)=NP_IPER(IPER)+1
    
    !## write alphanumerical identification of well
    IF(IPER.EQ.0)THEN
     
     IF(ILAY.GT.0)NNODES= 1 !## single well screen layer given
     IF(ILAY.LE.0)NNODES=-1 !## single well screen layer determined
     LINE='WELLID_'//TRIM(ITOS(NP_IPER(IPER)))//','//TRIM(ITOS(NNODES))
     !## identification
     WRITE(IU,'(A)') TRIM(LINE)
     READ(STRING(ILOSS),*,IOSTAT=IOS) LOSSTYPE; IF(IOS.NE.0)EXIT
     !## losstype
     LOSSTYPE=UTL_CAP(LOSSTYPE,'U')
     SELECT CASE (TRIM(LOSSTYPE))
      CASE ('NONE');       ILOSSTYPE=0
      CASE ('THIEM');      ILOSSTYPE=1
      CASE ('SKIN');       ILOSSTYPE=2
!      CASE ('GENERAL');    ILOSSTYPE=3
!      CASE ('SPECIFYCWC'); ILOSSTYPE=4
      CASE DEFAULT
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Specified model ('//TRIM(LOSSTYPE)//') for well loss unknown'//CHAR(13)// &
         'Select one of the following:'//CHAR(13)//'NONE, THIEM, SKIN','Error'); IOS=-1; EXIT
!         'Select one of the following:'//CHAR(13)//'NONE, THIEM, SKIN, GENERAL, SPECIFYCWC','Error'); IOS=-1; EXIT
     END SELECT
     
     IF(ILOSSTYPE.EQ.0.AND.NNODES.LT.0)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Specified model ('//TRIM(LOSSTYPE)//') for well cannot be'//CHAR(13)// &
        'used in combination with ILAY=0','Error'); IOS=-1; EXIT
     ENDIF
     
     PUMPLOC=0 !## no location of pump intake or injection
     QLIMIT=0  !## pumpage not by constraints
     PPFLAG=0  !## head not adjusted for partial penetration of well - error in case ibound is zero
     !IF(NNODES.EQ. 1)PPFLAG=0  !## head not adjusted for partial penetration of well
     !IF(NNODES.EQ.-1)PPFLAG=1  !## head adjusted for partial penetration of well
     PUMPCAP=0 !## discharge not defined by head-capacity relation
     LINE=TRIM(LOSSTYPE)//','//TRIM(ITOS(PUMPLOC))//','//TRIM(ITOS(QLIMIT))//','//TRIM(ITOS(PPFLAG))//','//TRIM(ITOS(PUMPCAP))
     WRITE(IU,'(A)') TRIM(LINE)
     SELECT CASE (ILOSSTYPE)
      !## thiem
      CASE(1)
       READ(STRING(ILOSS+1),*,IOSTAT=IOS) RW; IF(IOS.NE.0)EXIT
       LINE=TRIM(RTOS(RW,'F',2)); WRITE(IU,'(A)') TRIM(LINE)
      !## skin
      CASE(2)
       READ(STRING(ILOSS+1),*,IOSTAT=IOS) RW;    IF(IOS.NE.0)EXIT
       READ(STRING(ILOSS+2),*,IOSTAT=IOS) RSKIN; IF(IOS.NE.0)EXIT
       READ(STRING(ILOSS+3),*,IOSTAT=IOS) KSKIN; IF(IOS.NE.0)EXIT
       LINE=TRIM(RTOS(RW,'F',2))//','//TRIM(RTOS(RSKIN,'F',2))//','//TRIM(RTOS(KSKIN,'F',2)); WRITE(IU,'(A)') TRIM(LINE)
     END SELECT
     IF(NNODES.GT.0)THEN
      LINE=TRIM(ITOS(ILAY))//','//TRIM(ITOS(IROW))//','//TRIM(ITOS(ICOL))
      WRITE(IU,'(A)') TRIM(LINE)
     ELSE
      READ(STRING(4),*,IOSTAT=IOS) Z1; IF(IOS.NE.0)EXIT
      READ(STRING(5),*,IOSTAT=IOS) Z2; IF(IOS.NE.0)EXIT
      Z1=MIN(Z1,TOP(1      )%X(ICOL,IROW)-0.1D0)
      Z2=MAX(Z2,BOT(PRJNLAY)%X(ICOL,IROW)+0.1D0)
      LINE=TRIM(RTOS(Z1,'F',2))//','//TRIM(RTOS(Z2,'F',2))//','//TRIM(ITOS(IROW))//','//TRIM(ITOS(ICOL))
      WRITE(IU,'(A)') TRIM(LINE)
     ENDIF

    ELSE
    
     !## get discharge - always on position 3
     IF(IEXT.EQ.0)THEN
      READ(STRING(3),*,IOSTAT=IOS) Q; IF(IOS.NE.0)EXIT
     ELSE
      !## get id number - can be any column
      READ(STRING(IEXT),*,IOSTAT=IOS) ID; IF(IOS.NE.0)EXIT
      IF(.NOT.UTL_PCK_READTXT(2,ITIME,JTIME,Q,TRIM(CDIR)//'\'//TRIM(ID)//'.'//TRIM(EXT),0,'',2,NCOUNT))THEN
       IOS=-1; EXIT
      ENDIF
      IF(NCOUNT.LE.0.0D0)Q=0.0D0
     ENDIF

     !## use factor/impulse
     Q=Q*FCT; Q=Q+IMP  

     LINE='WELLID_'//TRIM(ITOS(NP_IPER(IPER)))//','//TRIM(RTOS(Q,'G',7))
     WRITE(IU,'(A)') TRIM(LINE)
    
    ENDIF
        
   ENDDO

   DEALLOCATE(STRING); CLOSE(KU)

   IF(IOS.NE.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading IPF file'//CHAR(13)//TRIM(SFNAME)//CHAR(13)// &
       'Linenumber '//TRIM(ITOS(I)),'Error'); EXIT
   ENDIF     
 
  ENDDO
 
  IF(IOS.NE.0)EXIT
 
  !## store previous stress-period information for this timestep
  IF(IPER.GT.0)LPER=KPER

 ENDDO
 
 CLOSE(IU)

 !## store maximum number of well in simulation
 NP_IPER(0)=MAXVAL(NP_IPER(1:PRJNPER))

 IF(IOS.EQ.0)THEN
  CALL UTL_MF2005_MAXNO(TRIM(DIRMNAME)//'.'//CPCK//'7_',NP_IPER) 
  PMANAGER_SAVEMF2005_MNW=.TRUE.
 ENDIF
  
 DEALLOCATE(NP_IPER)
 
 END FUNCTION PMANAGER_SAVEMF2005_MNW
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_ISG(DIR,DIRMNAME,IBATCH,LEX,ITOPIC,ICB,CPCK,IPRT)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: CONST=86400.0D0 !## conversion to m3/day
 REAL(KIND=DP_KIND),PARAMETER :: DLEAK=0.001D0
 INTEGER,INTENT(IN) :: IBATCH,ICB,ITOPIC,IPRT
 CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME,CPCK
 LOGICAL,INTENT(IN) :: LEX
 REAL(KIND=DP_KIND) :: FCT,IMP,CNST
 CHARACTER(LEN=256) :: SFNAME,EXFNAME
 CHARACTER(LEN=30) :: FRM
 INTEGER :: IU,JU,ILAY,I,ISYS,KPER,IPER,NTOP,NSYS,ICNST,ICOL,IROW,JSYS
 INTEGER,DIMENSION(2) :: NP
 INTEGER(KIND=8) :: ITIME,JTIME
 TYPE(GRIDISGOBJ) :: GRIDISG
 CHARACTER(LEN=1) :: VTXT
  
 IF(.NOT.LEX)THEN; PMANAGER_SAVEMF2005_ISG=.TRUE.; RETURN; ENDIF
 
 PMANAGER_SAVEMF2005_ISG=.FALSE.
 
 VTXT='7'; IF(PBMAN%IFORMAT.EQ.3)VTXT='6'

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.'//CPCK//VTXT//'...')
 IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.'//CPCK//VTXT//'...'
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.'//CPCK//VTXT//'_',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')

 !IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.'//CPCK//'7...')
 !IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.'//CPCK//'7...'

 IF(PBMAN%IFORMAT.EQ.3)THEN
  WRITE(IU,'(A)') '# '//CPCK//VTXT//' File Generated by '//TRIM(UTL_IMODVERSION())
  WRITE(IU,'(/A/)') '#General Options'  
  WRITE(IU,'(A)') 'BEGIN OPTIONS'
!  WRITE(IU,'(A)') ' AUXILIARY <auxiliary(naux)>'
!  WRITE(IU,'(A)') ' AUXMULTNAME <auxmultname>'
!  WRITE(IU,'(A)') ' BOUNDNAMES'
!  WRITE(IU,'(A)') ' PRINT_INPUT'
!  WRITE(IU,'(A)') ' PRINT_FLOWS'
  IF(ASSOCIATED(PBMAN%ISAVE(TRIV)%ILAY))WRITE(IU,'(A)') ' SAVE_FLOWS'
!  WRITE(IU,'(A)') ' TS6 FILEIN <ts6_filename>'
!  WRITE(IU,'(A)') ' OBS6 FILEIN <obs6_filename>'
!  WRITE(IU,'(A)') ' MOVER'
  WRITE(IU,'(A)') 'END OPTIONS'
  WRITE(IU,'(/A/)') '#General Dimensions'  
  WRITE(IU,'(A)') 'BEGIN DIMENSIONS'
  WRITE(IU,'(1X,A)') ' MAXBOUND NaN1#'
  WRITE(IU,'(A)') 'END DIMENSIONS'
 ENDIF
 
! IU=UTL_GETUNIT()
! CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.'//CPCK//'7_',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
! IF(IU.EQ.0)RETURN

 IF(PBMAN%IFORMAT.EQ.2)THEN
  SELECT CASE (ITOPIC)
   !## isg 
   CASE (TISG)
    LINE='NaN1#,'//TRIM(ITOS(ICB))//' AUX RFCT AUX ISUB RFACT RFCT RSUBSYS ISUB NOPRINT'
   !## sfr
   CASE (TSFR)
    LINE='NaN2#,NaN1#,0,0,'//TRIM(RTOS(CONST,'G',7))//','//TRIM(RTOS(DLEAK,'E',4))//','// &
         TRIM(ITOS(ICB))//','//TRIM(ITOS(ISFRCB2))//' NOPRINT'
  END SELECT
  WRITE(IU,'(A)') TRIM(LINE)
 ENDIF
 
 WRITE(FRM,'(A9,I2.2,A14)') '(3(I5,1X),',1,'(G15.7,1X),I5)'

 !## create subfolders
 CALL UTL_CREATEDIR(TRIM(DIR)//'\'//CPCK//VTXT)

 CALL PMANAGER_SAVEMF2005_ALLOCATEPCK(PRJNLAY)

 NP=0
 
 DO IPER=1,PRJNPER
  !## reset only for isg to riv conversion
  IF(ITOPIC.EQ.TISG)NP(1)=0

  !## get appropriate stress-period to store in runfile   
  KPER=PMANAGER_GETCURRENTIPER(IPER,ITOPIC,ITIME,JTIME)

  !## always export rivers per stress-period
  IF(ITOPIC.EQ.TISG)THEN; IF(PBMAN%DISG.EQ.1)KPER=ABS(KPER); ENDIF
  !## always export streamflow routing per stress-period
  IF(ITOPIC.EQ.TSFR)THEN; IF(PBMAN%DSFR.EQ.1)KPER=ABS(KPER); ENDIF
  
  !## output
  WRITE(IPRT,'(1X,A,2I10,2(1X,I14))') 'Exporting timestep ',IPER,KPER,ITIME,JTIME  
  IF(IBATCH.EQ.1)WRITE(6,'(A,3I6,2(1X,I14))') '+Exporting timestep ',IPER,PRJNPER,KPER,ITIME,JTIME  

  !## reuse previous timestep
  IF(KPER.LE.0)THEN
   IF(IPER.EQ.1)THEN
    WRITE(IU,'(I10)') 0
   ELSE
    IF(ITOPIC.EQ.TISG)WRITE(IU,'(A)') '-1'
    IF(ITOPIC.EQ.TSFR)WRITE(IU,'(A)') '-1,-1,0,0'
   ENDIF
   !## process next timestep
   CYCLE
  ENDIF
  
  IF(PBMAN%IFORMAT.EQ.3)WRITE(IU,'(/A)') 'BEGIN PERIOD '//TRIM(ITOS(IPER))     

  !## default isg
  IF(ITOPIC.EQ.TISG)THEN
   EXFNAME=TRIM(DIR)//'\'//CPCK//VTXT//'\'//CPCK//'_T'//TRIM(ITOS(IPER))//'.ARR'
   JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=EXFNAME,STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(JU.EQ.0)RETURN
  !## sfr isg
  ELSE
   EXFNAME=TRIM(DIR)//'\'//CPCK//VTXT//'\'//CPCK//'.ARR'
   JU=IU
  ENDIF
  
  !## ISG not yet supports timescales less than 1 day
  GRIDISG%SDATE=SIM(IPER)%IYR*10000+SIM(IPER)%IMH*100+SIM(IPER)%IDY
  GRIDISG%SDATE=UTL_IDATETOJDATE(GRIDISG%SDATE)
  GRIDISG%EDATE=GRIDISG%SDATE+MAX(1,INT(SIM(IPER)%DELT))

  GRIDISG%XMIN=BND(1)%XMIN; GRIDISG%YMIN=BND(1)%YMIN
  GRIDISG%XMAX=BND(1)%XMAX; GRIDISG%YMAX=BND(1)%YMAX
  !## transient (2) or steady-state (1)
  GRIDISG%ISTEADY=2; IF(SIM(IPER)%DELT.EQ.0.0D0)GRIDISG%ISTEADY=1
  GRIDISG%IDIM=0
  GRIDISG%CS=BND(1)%DX !## cellsize
  GRIDISG%MINDEPTH=0.1
  GRIDISG%WDEPTH=0.0D0
  GRIDISG%ICDIST=1     !## compute influence of structures
  GRIDISG%ISIMGRO=0    !## no simgro
  GRIDISG%IEXPORT=1    !## modflow river files

  IF(BND(1)%IEQ.EQ.1)THEN
   GRIDISG%NCOL=BND(1)%NCOL; GRIDISG%NROW=BND(1)%NROW
   ALLOCATE(GRIDISG%DELR(0:BND(1)%NCOL))
   DO ICOL=0,GRIDISG%NCOL; GRIDISG%DELR(ICOL)=BND(1)%SX(ICOL); ENDDO
   ALLOCATE(GRIDISG%DELC(0:BND(1)%NROW))
   DO IROW=0,GRIDISG%NROW; GRIDISG%DELC(IROW)=BND(1)%SY(IROW); ENDDO
  ELSE
   GRIDISG%NCOL=0; GRIDISG%NROW=0
  ENDIF
  
  !## output folder
  GRIDISG%ROOT=EXFNAME(1:INDEX(EXFNAME,'\',.TRUE.)-1)  
  GRIDISG%POSTFIX=''
  GRIDISG%NODATA=-999.99D0
  GRIDISG%ISAVE=1
  GRIDISG%MAXWIDTH=1000.0D0
  GRIDISG%IAVERAGE=1

  !## allocate memory for packages
  NTOP=SIZE(TOPICS(ITOPIC)%STRESS(KPER)%FILES,1); NSYS=SIZE(TOPICS(ITOPIC)%STRESS(KPER)%FILES,2)
  
  !## number of systems
  DO ISYS=1,NSYS 

   ICNST =TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%ICNST
   CNST  =TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%CNST
   FCT   =TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%FCT
   IMP   =TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%IMP
   ILAY  =TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%ILAY
   SFNAME=TOPICS(ITOPIC)%STRESS(KPER)%FILES(1,ISYS)%FNAME

   IF(PBMAN%SSYSTEM.EQ.0)THEN
    JSYS=ISYS
   ELSE
    JSYS=1
   ENDIF
   
   WRITE(IPRT,'(1X,A,I3,A1,I1,A1,I4.3,3(A1,G15.7),A1,A)') TOPICS(ITOPIC)%TNAME(1:5)//',', &
      ISYS,',',ICNST,',',ILAY,',',FCT,',',IMP,',',CNST,',',CHAR(39)//TRIM(SFNAME)//CHAR(39)

   IF(ISGREAD((/SFNAME/),IBATCH))THEN
    !## translate again to idate as it will be convered to jdate in next subroutine
    GRIDISG%SDATE=UTL_JDATETOIDATE(GRIDISG%SDATE)
    GRIDISG%EDATE=UTL_JDATETOIDATE(GRIDISG%EDATE)-1  !<- edate is equal to sdate if one day is meant
    SELECT CASE (ITOPIC)
     !## open isg file
     CASE (TISG)
      IF(.NOT.ISG2GRID(GRIDISG%POSTFIX,BND(1)%NROW,BND(1)%NCOL,PRJNLAY,ILAY,TOP,BOT,KHV,BND,VCW,IBATCH,NP,JU,GRIDISG,SFT,LSFT,JSYS))EXIT 
     !## open sfr file
     CASE (TSFR)
      IF(.NOT.ISG2SFR(BND(1)%NROW,BND(1)%NCOL,PRJNLAY,ILAY,IPER,PRJNPER,NP,JU,GRIDISG,EXFNAME,TOP,BOT))EXIT 
    END SELECT
    CALL ISGDEAL(1); CALL ISGCLOSEFILES()
   ELSE
    !## stop processing
    CLOSE(IU); CALL PMANAGER_SAVEMF2005_DEALLOCATEPCK(); RETURN
   ENDIF

  ENDDO
 
  !## not for sfr
  IF(PBMAN%IFORMAT.EQ.2.AND.ITOPIC.EQ.29)CALL IDFWRITEFREE_HEADER(JU,BND(1))

  !## error occured
  IF(ISYS.LE.NSYS)EXIT
 
  !## only for river package usage of external filename
  IF(ITOPIC.EQ.TISG)THEN
   IF(PBMAN%IFORMAT.GE.2)THEN
    IF(PBMAN%IFORMAT.EQ.2)THEN
     LINE=TRIM(ITOS(NP(1))); WRITE(IU,'(A)') TRIM(LINE)
    ENDIF
    NP(2)=MAXVAL(NP)
    IF(NP(1).GT.0)THEN
     SFNAME=EXFNAME
     N=3; IF(PBMAN%IFORMAT.EQ.3)N=4; DO I=1,N; SFNAME=SFNAME(:INDEX(SFNAME,'\',.TRUE.)-1); ENDDO
     I=LEN_TRIM(SFNAME); SFNAME='.'//EXFNAME(I+1:)
     WRITE(IU,'(A)') 'OPEN/CLOSE '//TRIM(SFNAME)//' 1.0 (FREE) -1'
    ENDIF
    IF(IU.NE.JU)CLOSE(JU)
    IF(PBMAN%IFORMAT.EQ.3)WRITE(IU,'(A)') 'END PERIOD '
   ENDIF
  ENDIF
  
 ENDDO
 
 CLOSE(IU); CALL PMANAGER_SAVEMF2005_DEALLOCATEPCK()
 IF(ASSOCIATED(GRIDISG%DELR))DEALLOCATE(GRIDISG%DELR)
 IF(ASSOCIATED(GRIDISG%DELC))DEALLOCATE(GRIDISG%DELC)

 !## no error occured
 IF(IPER.GT.NPER)THEN
  IF(ITOPIC.EQ.TISG)THEN
   CALL UTL_MF2005_MAXNO(TRIM(DIRMNAME)//'.'//CPCK//VTXT//'_',(/NP(2)/))
  ELSE
   CALL UTL_MF2005_MAXNO(TRIM(DIRMNAME)//'.'//CPCK//VTXT//'_',NP)
  ENDIF
  PMANAGER_SAVEMF2005_ISG=.TRUE.
 ENDIF
  
 END FUNCTION PMANAGER_SAVEMF2005_ISG
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_PCK(DIR,DIRMNAME,IBATCH,LEX,ITOPIC,ICB,CPCKIN,JTOP,IPRT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: IFHBSS=0,NFHBX1=0,NFHBX2=0
 INTEGER,INTENT(IN) :: IBATCH,ITOPIC,ICB,IPRT
 INTEGER,INTENT(IN),DIMENSION(:) :: JTOP
 LOGICAL,INTENT(IN) :: LEX
 CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME,CPCKIN
 REAL(KIND=DP_KIND) :: Z1,Z2,FCT,IMP,CNST,OLFCOND
 CHARACTER(LEN=256) :: SFNAME,EXFNAME
 CHARACTER(LEN=3) :: CPCK
 CHARACTER(LEN=40) :: FRM
 INTEGER :: IU,JU,ILAY,IROW,ICOL,I,J,KTOP,KPER,IPER,NTOP,SCL_D,SCL_U,ICNST,NSYS,ISYS,JSYS,MP,N,IIPER,KKPER, &
    NBDTIM,NHED,NFLW,IFBND,NRCHOP,NEVTOP,NUZTOP,INRECH,INSURF,INEVTR,INEXDP,LPER,NUZF1,NUZF2,NUZF3,NUZF4
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: TLP,KH,TP,BT,XTMP
 INTEGER(KIND=8) :: ITIME,JTIME
 REAL(KIND=DP_KIND),PARAMETER :: MINKHT=0.0D0
! INTEGER,PARAMETER :: ICLAY=1 !## shift to nearest aquifer
 INTEGER :: JD0,JD1,ISEC0,ISEC1,NUZGAG,IRUNFLG,IEQUAL,ICHECK
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: JEQUAL
 REAL(KIND=DP_KIND) :: DDAY,DSEC
 CHARACTER(LEN=1) :: VTXT
 LOGICAL :: LCHKCHD
 
 IF(.NOT.LEX)THEN; PMANAGER_SAVEMF2005_PCK=.TRUE.; RETURN; ENDIF
 
 PMANAGER_SAVEMF2005_PCK=.FALSE.
 
 CPCK=CPCKIN
 
 VTXT='7'; IF(PBMAN%IFORMAT.EQ.3)VTXT='6'

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.'//CPCK//VTXT//'...')
 IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.'//CPCK//VTXT//'...'
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.'//CPCK//VTXT//'_',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
 
 IF(IU.EQ.0)RETURN

 IF(PBMAN%IFORMAT.EQ.3)THEN
  WRITE(IU,'(A)') '# '//CPCK//VTXT//' File Generated by '//TRIM(UTL_IMODVERSION())
  WRITE(IU,'(/A/)') '#General Options'  
  WRITE(IU,'(A)') 'BEGIN OPTIONS'
!  WRITE(IU,'(A)') ' AUXILIARY <auxiliary(naux)>'
!  WRITE(IU,'(A)') ' AUXMULTNAME <auxmultname>'
!  WRITE(IU,'(A)') ' BOUNDNAMES'
!  WRITE(IU,'(A)') ' PRINT_INPUT'
!  WRITE(IU,'(A)') ' PRINT_FLOWS'
  
  IF(ASSOCIATED(PBMAN%ISAVE(ITOPIC)%ILAY))WRITE(IU,'(1X,A)') 'SAVE_FLOWS'
  
!  WRITE(IU,'(A)') ' TS6 FILEIN <ts6_filename>'
!  WRITE(IU,'(A)') ' OBS6 FILEIN <obs6_filename>'
!  WRITE(IU,'(A)') ' MOVER'
  WRITE(IU,'(A)') 'END OPTIONS'
  WRITE(IU,'(/A/)') '#General Dimensions'  
  WRITE(IU,'(A)') 'BEGIN DIMENSIONS'
  WRITE(IU,'(1X,A)') ' MAXBOUND NaN1#'
  WRITE(IU,'(A)') 'END DIMENSIONS'
 ENDIF
 
 !## write header of file
 SELECT CASE (ITOPIC)
  !## uzf
!NUZTOP=1 !## recharge specified to top cell
  CASE (TUZF); NUZGAG=0; IRUNFLG=0; NUZTOP=1  !PBMAN%NLOGLOC
   !## define initial water content
   IF(SIM(1)%DELT.GT.0.0D0)WRITE(IU,'(A)') 'SPECIFYTHTI'
   LINE='NaN1#,2,'//TRIM(ITOS(IRUNFLG))//',1,'//TRIM(ITOS(-IUZFCB1))//',0,20,50,'//TRIM(ITOS(NUZGAG))//',0.5'
   IF(PBMAN%IFORMAT.EQ.2)WRITE(IU,'(A)') TRIM(LINE)
   
!IUZFOPT=2 !## permeabiliy specified in lpf
!irunflg=0 !## water discharge from top removed form the model (usage of SFR/LAK needed)
!ietflg=1 !## et simulated
!iuzfcb1=59 !## writing groundwater recharge (see nam-file)
!iuzfcb2=0 !## alternative output format
!NTRAIL2=10  !## trailing waves
!nsets2=20  !## number of wave sets
!nuzgag=1 !## number of cells to gage
!surfdep=0.5 !## average undulation depth (is stabieler om iets meer te pakken)
!WRITE(iu,'(9I3,f5.1)') NUZTOP,IUZFOPT,irunflg,ietflg,iuzfcb1,iuzfcb2,NTRAIL2,nsets2,nuzgag,surfdep

  !## drn
  CASE (TDRN)
   IF(PBMAN%ICONCHK.EQ.0)THEN
    LINE='NaN1#,'//TRIM(ITOS(ICB))//' AUX ISUB DSUBSYS ISUB NOPRINT'
   ELSE
    LINE='NaN1#,'//TRIM(ITOS(ICB))//' AUX ISUB DSUBSYS ISUB ICONCHK NOPRINT'
   ENDIF
   IF(PBMAN%IFORMAT.EQ.2)WRITE(IU,'(A)') TRIM(LINE)

!## AUX IC ICHONCHK IC
  !## riv
  CASE (TRIV)
   LINE='NaN1#,'//TRIM(ITOS(ICB))//' AUX RFCT AUX ISUB RFACT RFCT RSUBSYS ISUB NOPRINT'
   IF(PBMAN%IFORMAT.EQ.2)WRITE(IU,'(A)') TRIM(LINE)
!## IFVDL SFT RCNC
  !## evt
  CASE (TEVT); NEVTOP=1; LINE='NaN1#,'//TRIM(ITOS(ICB))
  IF(PBMAN%IFORMAT.EQ.2)WRITE(IU,'(A)') TRIM(LINE)
!## NEVTOP moet twee worden voor optie laag = -1
  !## ghb
  CASE (TGHB)
   LINE='NaN1#,'//TRIM(ITOS(ICB))//' AUX ISUB GSUBSYS ISUB NOPRINT'
   IF(PBMAN%IFORMAT.EQ.2)WRITE(IU,'(A)') TRIM(LINE)
  !## rch
  CASE (TRCH); NRCHOP=1; LINE='NaN1#,'//TRIM(ITOS(ICB))
  IF(PBMAN%IFORMAT.EQ.2)WRITE(IU,'(A)') TRIM(LINE)
!## NaN1 moet 3 worden voor optie laag = -1
  !## olf
  CASE (TOLF)
   CPCK='OLF'; IF(.NOT.LDRN)CPCK='DRN';
   IF(PBMAN%ICONCHK.EQ.0)THEN
    LINE='NaN1#,'//TRIM(ITOS(ICB))//' AUX ISUB DSUBSYS ISUB NOPRINT'
   ELSE
    LINE='NaN1#,'//TRIM(ITOS(ICB))//' AUX ISUB DSUBSYS ISUB ICONCHK NOPRINT'
   ENDIF
   IF(PBMAN%IFORMAT.EQ.2)WRITE(IU,'(A)') TRIM(LINE)
  !## chd
  CASE (TCHD)
   LINE='NaN1# NOPRINT NEGBND'
   IF(PBMAN%IFORMAT.EQ.2)WRITE(IU,'(A)') TRIM(LINE)
  !## fhb package
  CASE(TFHB)
   !## check number of boundary type conditions - for fhb package
   NHED=0; NFLW=0
   DO ILAY=1,PRJNLAY
    DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
     IF(BND(ILAY)%X(ICOL,IROW).EQ.-2.0)NHED=NHED+1
     IF(BND(ILAY)%X(ICOL,IROW).EQ. 2.0)NFLW=NFLW+1
    ENDDO; ENDDO
   ENDDO

   !## look for number of stress-periods for boundary package
   ALLOCATE(FHBNBDTIM(PRJNPER)); FHBNBDTIM=0.0D0
   !## get first stress-period
   NBDTIM=0
   DO I=1,PRJNPER; IF(SIM(I)%DELT.NE.0.0D0)EXIT; ENDDO
   !## add steady-state
   IF(I.NE.1)NBDTIM=1 

   !## transient periods still available
   IF(I.LE.PRJNPER)THEN
    !## get first start-date
    JD0  =JD(SIM(I)%IYR,SIM(I)%IMH,SIM(I)%IDY)
    ISEC0=   SIM(I)%IHR*3600+SIM(I)%IMT*60+SIM(I)%ISC
    ISEC0=   86400-ISEC0
    DO J=1,SIZE(TOPICS(ITOPIC)%STRESS)
     IF(.NOT.ASSOCIATED(TOPICS(ITOPIC)%STRESS(J)%FILES))CYCLE
     !## not transient definition
     IF(TOPICS(ITOPIC)%STRESS(J)%IYR+TOPICS(ITOPIC)%STRESS(J)%IMH+TOPICS(ITOPIC)%STRESS(J)%IDY+ &
        TOPICS(ITOPIC)%STRESS(J)%IHR+TOPICS(ITOPIC)%STRESS(J)%IMT+TOPICS(ITOPIC)%STRESS(J)%ISC.LE.0)CYCLE
     !## get date for current period
     JD1   =JD(TOPICS(ITOPIC)%STRESS(J)%IYR,TOPICS(ITOPIC)%STRESS(J)%IMH,TOPICS(ITOPIC)%STRESS(J)%IDY)
     ISEC1 =TOPICS(ITOPIC)%STRESS(J)%IHR*3600+TOPICS(ITOPIC)%STRESS(J)%IMT*60+TOPICS(ITOPIC)%STRESS(J)%ISC
     DDAY  =JD1-JD0
     IF(DDAY.EQ.0.0D0)THEN
      DSEC=ISEC1
     ELSE
      DSEC=ISEC0+ISEC1
     ENDIF
     NBDTIM=NBDTIM+1
     FHBNBDTIM(NBDTIM)=DDAY+REAL(DSEC)/86400.0D0
    ENDDO
   ENDIF
     
   LINE=TRIM(ITOS(NBDTIM))//','//TRIM(ITOS(NFLW))  //','//TRIM(ITOS(NHED))//','//TRIM(ITOS(IFHBSS))//','// &
        TRIM(ITOS(IFHBCB))//','//TRIM(ITOS(NFHBX1))//','//TRIM(ITOS(NFHBX2))
   WRITE(IU,'(A)') TRIM(LINE)
   LINE=TRIM(ITOS(IFHBUN))//',1.0,1'
   WRITE(IU,'(A)') TRIM(LINE)
   WRITE(IU,*) (FHBNBDTIM(I),I=1,NBDTIM)
   !## allocate for fhb package
   IF(NHED.GT.0)ALLOCATE(FHBHED(NHED,NBDTIM))
   IF(NFLW.GT.0)ALLOCATE(FHBFLW(NFLW,NBDTIM))
 END SELECT
 
 !## fill tlp for each modellayer
 ALLOCATE(TLP(PRJNLAY),KH(PRJNLAY),TP(PRJNLAY),BT(PRJNLAY))
 
 !## see whether information is equal to previous timestep - only for rch and evt
 LPER=0

 ALLOCATE(NP_IPER(0:PRJNPER)); NP_IPER=0

 !## maximum number of input per simulation
 MP=0; NBDTIM=0
 DO IPER=1,PRJNPER

  !## number of input per stressperiod
  NP_IPER(IPER)=0
  
  !## get appropriate stress-period to store in runfile   
  KPER=PMANAGER_GETCURRENTIPER(IPER,ITOPIC,ITIME,JTIME)

  !## output
  WRITE(IPRT,'(1X,A,2I10,2(1X,I14))') 'Exporting timestep ',IPER,KPER,ITIME,JTIME  
  IF(IBATCH.EQ.1)WRITE(6,'(A,3I6,2(1X,I14))') '+Exporting timestep ',IPER,PRJNPER,KPER,ITIME,JTIME  

  !## reuse previous timestep
  IF(KPER.LE.0)THEN
   SELECT CASE (ITOPIC)
    !## uzf
    CASE (TUZF)
     IF(IPER.EQ.1)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to start the first stress-period with'//CHAR(13)// &
       'a definition for the UZF package','Error'); RETURN
     ELSE
      DO I=1,4; WRITE(IU,'(A)') '-1';  ENDDO
     ENDIF
    !## evt 
    CASE (TEVT) 
     IF(IPER.EQ.1)THEN
      WRITE(IU,'(A)') '0,0,0'
      DO I=1,3; WRITE(IU,'(A)') 'CONSTANT 0.000000E+00'; ENDDO
     ELSE; WRITE(IU,'(A)') '-1,-1,-1';  ENDIF
    !## rch
    CASE (TRCH) 
     IF(PBMAN%IFORMAT.EQ.2)THEN
      IF(IPER.EQ.1)THEN; WRITE(IU,'(I10)') 0; WRITE(IU,'(A)') 'CONSTANT 0.000000E+00'
      ELSE; WRITE(IU,'(I10)') -1; ENDIF
     ENDIF
    !##  wel,drn,riv,ghb,chd,olf
    CASE (TDRN,TRIV,TGHB,TCHD,TOLF,TISG) ! 22, 23, 25, 27, 28,29) 
     IF(PBMAN%IFORMAT.EQ.2)THEN
      IF(IPER.EQ.1)THEN; WRITE(IU,'(I10)') 0
      ELSE; WRITE(IU,'(I10)') -1; ENDIF
     ENDIF
    !## fhb- skip
    CASE (TFHB)
    CASE DEFAULT
     WRITE(*,*) 'Cannot come here: ERROR PMANAGER_SAVEMF2005_PCK'; PAUSE
   END SELECT  
   !## goto next timestep
   CYCLE
  ENDIF
    
! DATA CMOD/'CAP','TOP','BOT','BND','SHD','KDW','KHV','KVA','VCW','KVV', & ! 1-10
!           'STO','SPY','PWT','ANI','HFB','IBS','SFT','UZF','MNW','PST', & !11-20
!           'WEL','DRN','RIV','EVT','GHB','RCH','OLF','CHD','ISG','SFR', & !21-30
!           'FHB','LAK','PCG'/                                             !31-40

!  !## open external file (not for rch/evt)
!  JU=0
!  SELECT CASE (ITOPIC)
!   CASE (22:23,25,27:29) 
!    !## create subfolders
!    CALL UTL_CREATEDIR(TRIM(DIR)//'\'//CPCK//'7')
!    EXFNAME=TRIM(DIR)//'\'//CPCK//'7\'//CPCK//'_T'//TRIM(ITOS(IPER))//'.ARR'
!    JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=EXFNAME,STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(JU.EQ.0)RETURN
!  END SELECT
   
  !## allocate memory for packages
  NTOP=SIZE(TOPICS(ITOPIC)%STRESS(KPER)%FILES,1); NSYS=SIZE(TOPICS(ITOPIC)%STRESS(KPER)%FILES,2)
  !## used for writing and including the tlp-vector
  IF(ALLOCATED(XTMP))DEALLOCATE(XTMP); ALLOCATE(XTMP(NTOP)); XTMP=0.0D0
  
  SELECT CASE (ITOPIC)
   CASE (TEVT,TRCH)
    IF(NSYS.GT.1)THEN
     CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'You cannot apply more than a single layer to the package '// &
       TRIM(TOPICS(ITOPIC)%TNAME)//'.','Information')
     RETURN
    ENDIF
  END SELECT
  
  SELECT CASE (ITOPIC)
   CASE(TOLF,TCHD); N=NTOP+1
   CASE DEFAULT; N=NTOP
  END SELECT
  
  WRITE(FRM,'(A10,I2.2,A14)') '(3(I5,1X),',N,'(G15.7,1X),I5)'
  
  CALL PMANAGER_SAVEMF2005_ALLOCATEPCK(NTOP)
  
  NHED=0; NFLW=0; NBDTIM=NBDTIM+1

  !## see whether duplicate of definitions happened with current systems, not for wel/isg
  SELECT CASE (ITOPIC)
   !## drn,riv,ghb,chd,olf
   CASE (TDRN,TRIV,TGHB,TCHD,TOLF) !22,23,25,27,28)

    ALLOCATE(JEQUAL(NSYS,NTOP))

    !## search previous entries
    DO IIPER=1,IPER-1

     JEQUAL=0

     !## get appropriate stress-period to store in runfile   
     KKPER=PMANAGER_GETCURRENTIPER(IIPER,ITOPIC,ITIME,JTIME)
     IF(KKPER.LE.0)CYCLE
     
     !## number of systems
     DO ISYS=1,NSYS
      !## number of subtopics
      DO KTOP=1,NTOP
       ICNST =TOPICS(ITOPIC)%STRESS(KPER)%FILES(KTOP,ISYS)%ICNST 
       CNST  =TOPICS(ITOPIC)%STRESS(KPER)%FILES(KTOP,ISYS)%CNST  
       FCT   =TOPICS(ITOPIC)%STRESS(KPER)%FILES(KTOP,ISYS)%FCT   
       IMP   =TOPICS(ITOPIC)%STRESS(KPER)%FILES(KTOP,ISYS)%IMP   
       ILAY  =TOPICS(ITOPIC)%STRESS(KPER)%FILES(KTOP,ISYS)%ILAY  
       SFNAME=TOPICS(ITOPIC)%STRESS(KPER)%FILES(KTOP,ISYS)%FNAME 
       
       !## only whenever number of systems are equal
       IF(NSYS.EQ.SIZE(TOPICS(ITOPIC)%STRESS(KKPER)%FILES,2))THEN
        IF(ICNST.EQ. TOPICS(ITOPIC)%STRESS(KKPER)%FILES(KTOP,ISYS)%ICNST.AND. &
           CNST .EQ. TOPICS(ITOPIC)%STRESS(KKPER)%FILES(KTOP,ISYS)%CNST.AND.  &
           FCT.EQ.   TOPICS(ITOPIC)%STRESS(KKPER)%FILES(KTOP,ISYS)%FCT.AND.   &
           IMP .EQ.  TOPICS(ITOPIC)%STRESS(KKPER)%FILES(KTOP,ISYS)%IMP.AND.   &
           ILAY.EQ.  TOPICS(ITOPIC)%STRESS(KKPER)%FILES(KTOP,ISYS)%ILAY.AND.  &
           SFNAME.EQ.TOPICS(ITOPIC)%STRESS(KKPER)%FILES(KTOP,ISYS)%FNAME)THEN
         JEQUAL(ISYS,KTOP)=IIPER
        ENDIF
       ENDIF
      ENDDO
     ENDDO
     
     !## there is a previous definition of this package exported allready and can be reused
     IF(MINVAL(JEQUAL).EQ.MAXVAL(JEQUAL).AND.MINVAL(JEQUAL).NE.0)THEN
      IF(NP_IPER(IIPER).GT.0)THEN
       EXFNAME=TRIM(DIR)//'\'//CPCK//'7\'//CPCK//'_T'//TRIM(ITOS(IIPER))//'.ARR'
       SFNAME=EXFNAME; DO I=1,3; SFNAME=SFNAME(:INDEX(SFNAME,'\',.TRUE.)-1); ENDDO
       I=LEN_TRIM(SFNAME); SFNAME='.'//EXFNAME(I+1:)
       LINE=TRIM(ITOS(NP_IPER(IIPER))); WRITE(IU,'(A)') TRIM(LINE)
       WRITE(IU,'(A)') 'OPEN/CLOSE '//TRIM(SFNAME)//' 1.0D0 (FREE) -1'
       NP_IPER(IPER)=NP_IPER(IIPER)       
      ENDIF
      EXIT
     ENDIF

    ENDDO
    IF(ALLOCATED(JEQUAL))DEALLOCATE(JEQUAL)

  END SELECT
  
  !## next timestep
  IF(NP_IPER(IPER).GT.0)CYCLE
  
  !## open external file (not for rch/evt)
  IF(PBMAN%IFORMAT.GE.2)THEN
   JU=0
   SELECT CASE (ITOPIC)
    CASE (TDRN,TRIV,TGHB,TOLF,TCHD,TISG) !,25,27:29) 
     !## create subfolders
     CALL UTL_CREATEDIR(TRIM(DIR)//'\'//CPCK//VTXT)
     EXFNAME=TRIM(DIR)//'\'//CPCK//VTXT//'\'//CPCK//'_T'//TRIM(ITOS(IPER))//'.ARR'
     JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=EXFNAME,STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(JU.EQ.0)RETURN
    !## rch/evt for mf6
    CASE (TEVT,TRCH)
     IF(PBMAN%IFORMAT.EQ.3)THEN
      !## create subfolders
      CALL UTL_CREATEDIR(TRIM(DIR)//'\'//CPCK//VTXT)
      EXFNAME=TRIM(DIR)//'\'//CPCK//VTXT//'\'//CPCK//'_T'//TRIM(ITOS(IPER))//'.ARR'
      JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=EXFNAME,STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(JU.EQ.0)RETURN
     ENDIF
   END SELECT
   IF(PBMAN%IFORMAT.EQ.3)WRITE(IU,'(/A)') 'BEGIN PERIOD '//TRIM(ITOS(IPER))     
  ELSE
   JU=IU
  ENDIF

  !## number of systems
  DO ISYS=1,NSYS

   !## number of subtopics
   DO KTOP=1,NTOP

    ICNST =TOPICS(ITOPIC)%STRESS(KPER)%FILES(KTOP,ISYS)%ICNST 
    CNST  =TOPICS(ITOPIC)%STRESS(KPER)%FILES(KTOP,ISYS)%CNST  
    FCT   =TOPICS(ITOPIC)%STRESS(KPER)%FILES(KTOP,ISYS)%FCT   
    IMP   =TOPICS(ITOPIC)%STRESS(KPER)%FILES(KTOP,ISYS)%IMP   
    ILAY  =TOPICS(ITOPIC)%STRESS(KPER)%FILES(KTOP,ISYS)%ILAY  
    SFNAME=TOPICS(ITOPIC)%STRESS(KPER)%FILES(KTOP,ISYS)%FNAME 

    !## ilay equal zero not possible for rch and evt
    IF(ITOPIC.EQ.TEVT.OR.ITOPIC.EQ.TRCH)THEN
     IF(ILAY.EQ.0)THEN
      CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'You cannot apply a layer code of zero for RCH or EVT','Error')
      RETURN
     ENDIF
    ENDIF

    !## check to see whether equal to previous timestep
    IEQUAL=1
    SELECT CASE (ITOPIC)
     !## uzf(18),evt(24),rch(26)
     CASE (TUZF,TEVT,TRCH)
      IF(LPER.GT.0)THEN
       !## only whenever number of systems are equal
       IF(NSYS.EQ.SIZE(TOPICS(ITOPIC)%STRESS(LPER)%FILES,2))THEN
        IF(ICNST.EQ. TOPICS(ITOPIC)%STRESS(LPER)%FILES(KTOP,ISYS)%ICNST.AND. &
           CNST .EQ. TOPICS(ITOPIC)%STRESS(LPER)%FILES(KTOP,ISYS)%CNST.AND.  &
           FCT.EQ.   TOPICS(ITOPIC)%STRESS(LPER)%FILES(KTOP,ISYS)%FCT.AND.   &
           IMP .EQ.  TOPICS(ITOPIC)%STRESS(LPER)%FILES(KTOP,ISYS)%IMP.AND.   &
           ILAY.EQ.  TOPICS(ITOPIC)%STRESS(LPER)%FILES(KTOP,ISYS)%ILAY.AND.  &
           SFNAME.EQ.TOPICS(ITOPIC)%STRESS(LPER)%FILES(KTOP,ISYS)%FNAME)IEQUAL=-1
       ENDIF
      ENDIF
    END SELECT
    
    WRITE(IPRT,'(1X,A,I3,A1,I1,A1,I4.3,3(A1,G15.7),A1,A)') TOPICS(ITOPIC)%TNAME(1:5)//',', &
       ISYS,',',ICNST,',',ILAY,',',FCT,',',IMP,',',CNST,',',CHAR(39)//TRIM(SFNAME)//CHAR(39)
    
    SELECT CASE (ITOPIC)
     !## uzf
     CASE (TUZF)
      SELECT CASE (KTOP)
       CASE (1);   SCL_D=0; SCL_U=7 !## boundary
       CASE (2);   SCL_D=0; SCL_U=7 !## brook-corey
       CASE (3:4); SCL_D=0; SCL_U=2 !## thts/thhi
       CASE (5);   SCL_D=0; SCL_U=2; NUZF1=IEQUAL
       CASE (6);   SCL_D=0; SCL_U=2; NUZF2=IEQUAL
       CASE (7);   SCL_D=0; SCL_U=2; NUZF3=IEQUAL
       CASE (8);   SCL_D=0; SCL_U=2; NUZF4=IEQUAL
      END SELECT
      !## skip uzf package info for coming stress-periods
      IF(KTOP.LE.4.AND.IPER.GT.1)CYCLE
     !## evt
     CASE (TEVT) 
      SCL_D=1
      !## check to see whether equal to previous timestep
      SELECT CASE (KTOP)
       CASE (1); INSURF=IEQUAL; SCL_U=2
       CASE (2); INEVTR=IEQUAL; SCL_U=16
       CASE (3); INEXDP=IEQUAL; SCL_U=2
      END SELECT
     !## rch
     CASE (TRCH) 
      SCL_D=1; SCL_U=16   !## average
      !## equal from previous timestep
      INRECH=IEQUAL
     !## drn,riv,ghb
     CASE (TDRN,TRIV,TGHB) !22,23,25) !## drn,riv,ghb
      IF(KTOP.EQ.1)THEN; SCL_D=0; SCL_U=5; ENDIF
      IF(KTOP.NE.1)THEN; SCL_D=0; SCL_U=2; ENDIF
     !## chd,olf
     CASE (TCHD,TOLF) !27,28)
      SCL_D=1; SCL_U=2
     !## fhb
     CASE (TFHB)
      SCL_D=1
      IF(KTOP.EQ.1)SCL_U=5 !## q - sum (divide if cell is smaller)
      IF(KTOP.EQ.2)SCL_U=2 !## h - average
     CASE DEFAULT 
      STOP 'Cannot come here: ERROR PMANAGER_SAVEMF2005_PCK'
    END SELECT

    PCK(KTOP)%ILAY=ILAY
   
    !## skip this one - no to be read
    IF(IEQUAL.EQ.-1)CYCLE

    !## constant value
    IF(ICNST.EQ.1)THEN
     PCK(KTOP)%X=CNST
    !## read/clip/scale idf file
    ELSEIF(TOPICS(ITOPIC)%STRESS(KPER)%FILES(KTOP,ISYS)%ICNST.EQ.2)THEN
     PCK(KTOP)%FNAME=SFNAME
     IF(.NOT.IDFREADSCALE(PCK(KTOP)%FNAME,PCK(KTOP),SCL_U,SCL_D,1.0D0,0))RETURN
    ENDIF

    !## no checking for inactive cells
    ICHECK=1
    !## rch/evt mm/day -> m/day
    SELECT CASE (ITOPIC)
     !## uzf
     CASE (TUZF)
      IF(KTOP.EQ.5.OR.KTOP.EQ.6)FCT=FCT*0.001D0
      IF(ILAY.LE.0)NUZTOP=3
      !## not checking for inactive cells
      ICHECK=0
     !## evt
     CASE (TEVT)
      IF(KTOP.EQ.1)THEN
       FCT=FCT*0.001D0
       IMP=IMP*0.001D0
      ENDIF
      IF(ILAY.LT.0)NEVTOP=3
      !## checking for inactive cells
      ICHECK=1; IF(ILAY.GT.0)ICHECK=0
     !## rch
     CASE (TRCH)
      IF(KTOP.EQ.1)THEN
       FCT=FCT*0.001D0
       IMP=IMP*0.001D0
      ENDIF
      IF(ILAY.LT.0)NRCHOP=3
      !## checking for inactive cells
      ICHECK=1; IF(ILAY.GT.0)ICHECK=0
    END SELECT
    
    CALL PMANAGER_SAVEMF2005_FCTIMP(0,ICNST,PCK(KTOP),FCT,IMP,SCL_D)
    CALL PMANAGER_SAVEMF2005_CORRECT(ILAY,BND,PCK(KTOP),ICHECK,ITOPIC) 

   ENDDO

   SELECT CASE (ITOPIC)
    !## uzf
    CASE (TUZF)
      IF(IPER.EQ.1)THEN

       !## make sure value for uzbnd is zero for constant head and inactive cells - only if NUZTOP.eq.1
       IF(NUZTOP.EQ.1)THEN
        DO IROW=1,PCK(1)%NROW; DO ICOL=1,PCK(1)%NCOL
         IF(BND(1)%X(ICOL,IROW).LE.0)PCK(1)%X(ICOL,IROW)=0.0D0
        ENDDO; ENDDO
       !## make sure entered uzbnd with top layer is equal to the top elevation - otherwise solve the conflict
       ELSEIF(NUZTOP.EQ.3)THEN
        DO IROW=1,PCK(1)%NROW; DO ICOL=1,PCK(1)%NCOL
         !## assigned layer
         I=PCK(1)%X(ICOL,IROW)
         !## skip this one as it is an inactive cell
         IF(I.LE.0)CYCLE
         !## search first active layer
         DO ILAY=1,PRJNLAY; IF(BND(ILAY)%X(ICOL,IROW).GT.0)EXIT; ENDDO
         !## overrule for the first active layer
         IF(ILAY.LE.PRJNLAY)THEN
          IF(PCK(1)%X(ICOL,IROW).LT.0)PCK(1)%X(ICOL,IROW)=SIGN(ILAY,I)
          IF(ILAY.EQ.1)PCK(1)%X(ICOL,IROW)=1.0D0
         ENDIF
        ENDDO; ENDDO       
       ENDIF
       !## areal extent of uz flow
       IF(.NOT.PMANAGER_SAVEMF2005_PCK_U2DREL(TRIM(DIR)//'\'//CPCK//'7\'//CPCK//'_UZBND_T'//TRIM(ITOS(IPER))//'.ARR',PCK(1),IU,    0,1))RETURN
       !## brooks-corey epsilon
       IF(.NOT.PMANAGER_SAVEMF2005_PCK_U2DREL(TRIM(DIR)//'\'//CPCK//'7\'//CPCK//'_EPS_T'//TRIM(ITOS(IPER))//  '.ARR',PCK(2),IU,IFBND,0))RETURN
       !## thts saturated water content
       IF(.NOT.PMANAGER_SAVEMF2005_PCK_U2DREL(TRIM(DIR)//'\'//CPCK//'7\'//CPCK//'_THTS_T'//TRIM(ITOS(IPER))// '.ARR',PCK(3),IU,IFBND,0))RETURN
       !## skip initial water content if steady-state
       IF(SIM(IPER)%DELT.GT.0.0D0)THEN
        IF(.NOT.PMANAGER_SAVEMF2005_PCK_U2DREL(TRIM(DIR)//'\'//CPCK//'7\'//CPCK//'_THTI_T'//TRIM(ITOS(IPER))// '.ARR',PCK(4),IU,IFBND,0))RETURN
       ENDIF
!       !## log uzf locations       
!       DO I=1,PBMAN%NLOGLOC
!        WRITE(IU,'(4(I10,1X))') PBMAN%ILOC(I,1),PBMAN%ILOC(I,2),99+I,1
!       ENDDO

      ENDIF
      LINE=TRIM(ITOS(NUZF1)); WRITE(IU,'(A)') TRIM(LINE)
      IF(NUZF1.EQ.1)THEN
       IF(.NOT.PMANAGER_SAVEMF2005_PCK_U2DREL(TRIM(DIR)//'\'//CPCK//'7\'//CPCK//'_FINF_T'//TRIM(ITOS(IPER))// '.ARR',PCK(5),IU,IFBND,0))RETURN
      ENDIF
      LINE=TRIM(ITOS(NUZF2)); WRITE(IU,'(A)') TRIM(LINE)
      IF(NUZF2.EQ.1)THEN
       IF(.NOT.PMANAGER_SAVEMF2005_PCK_U2DREL(TRIM(DIR)//'\'//CPCK//'7\'//CPCK//'_PET_T'//TRIM(ITOS(IPER))//  '.ARR',PCK(6),IU,IFBND,0))RETURN
      ENDIF
      LINE=TRIM(ITOS(NUZF3)); WRITE(IU,'(A)') TRIM(LINE)
      IF(NUZF3.EQ.1)THEN
       IF(.NOT.PMANAGER_SAVEMF2005_PCK_U2DREL(TRIM(DIR)//'\'//CPCK//'7\'//CPCK//'_EXDP_T'//TRIM(ITOS(IPER))// '.ARR',PCK(7),IU,IFBND,0))RETURN
      ENDIF
      LINE=TRIM(ITOS(NUZF4)); WRITE(IU,'(A)') TRIM(LINE)
      IF(NUZF4.EQ.1)THEN
       !## make sure this is always larger than residual water content
       IF(.NOT.PMANAGER_SAVEMF2005_PCK_U2DREL(TRIM(DIR)//'\'//CPCK//'7\'//CPCK//'_EXTWC_T'//TRIM(ITOS(IPER))//'.ARR',PCK(8),IU,IFBND,0))RETURN
      ENDIF

    !## rch
    CASE (TRCH)

     IF(PBMAN%IFORMAT.EQ.2)THEN
      LINE=TRIM(ITOS(INRECH)); WRITE(IU,'(A)') TRIM(LINE); IFBND=0; IF(ILAY.GT.0)IFBND=1
      IF(INRECH.EQ.1)THEN
       IF(.NOT.PMANAGER_SAVEMF2005_PCK_U2DREL(TRIM(DIR)//'\'//CPCK//'7\'//CPCK//'_T'//TRIM(ITOS(IPER))//'.ARR',PCK(1),IU,IFBND,0))RETURN
      ENDIF
     ELSEIF(PBMAN%IFORMAT.EQ.3)THEN
      DO IROW=1,PCK(1)%NROW; DO ICOL=1,PCK(1)%NCOL
       !## find uppermost layer
       TLP=0.0D0
       IF(PCK(1)%ILAY.EQ.-1)THEN
        DO ILAY=1,SIZE(PBMAN%ILAY); IF(PBMAN%ILAY(ILAY).EQ.1.AND.BND(ILAY)%X(ICOL,IROW).GT.0)EXIT; ENDDO
        !## assign to uppermost active layer
        IF(ILAY.LE.PRJNLAY)TLP(ILAY)=1.0D0
       ELSE
        !## assign to predefined layer
        TLP(PCK(1)%ILAY)=1.0D0
       ENDIF

       DO ILAY=1,SIZE(PBMAN%ILAY)
        IF(PBMAN%ILAY(ILAY).EQ.0)CYCLE
        !## skip inactive cells
        IF(BND(ILAY)%X(ICOL,IROW).EQ.0.0D0)CYCLE
        !## not put into this model layer
        IF(TLP(ILAY).LE.0.0D0)CYCLE
        WRITE(JU,'(3I10,G15.7)') ILAY,IROW,ICOL,PCK(1)%X(ICOL,IROW)
        NP_IPER(IPER)=NP_IPER(IPER)+1
       ENDDO
      ENDDO; ENDDO
     ENDIF
     
    !## evt
    CASE (TEVT)

     LINE=TRIM(ITOS(INSURF))//','//TRIM(ITOS(INEVTR))//','//TRIM(ITOS(INEXDP)); WRITE(IU,'(A)') TRIM(LINE); IFBND=0; IF(ILAY.GT.0)IFBND=1
     IF(INSURF.EQ.1)THEN
      IF(.NOT.PMANAGER_SAVEMF2005_PCK_U2DREL(TRIM(DIR)//'\'//CPCK//'7\'//CPCK//'_SURF_T'//TRIM(ITOS(IPER))//'.ARR',PCK(2),IU,IFBND,0))RETURN
     ENDIF
     IF(INEVTR.EQ.1)THEN
      IF(.NOT.PMANAGER_SAVEMF2005_PCK_U2DREL(TRIM(DIR)//'\'//CPCK//'7\'//CPCK//'_EVTR_T'//TRIM(ITOS(IPER))//'.ARR',PCK(1),IU,IFBND,0))RETURN
     ENDIF
     IF(INEXDP.EQ.1)THEN
      IF(.NOT.PMANAGER_SAVEMF2005_PCK_U2DREL(TRIM(DIR)//'\'//CPCK//'7\'//CPCK//'_EXDP_T'//TRIM(ITOS(IPER))//'.ARR',PCK(3),IU,IFBND,0))RETURN
     ENDIF
     
    CASE DEFAULT
      
     DO IROW=1,PCK(1)%NROW; DO ICOL=1,PCK(1)%NCOL

      !## skip inactive/constant head cells
      IF(PCK(1)%ILAY.GT.0.AND.ITOPIC.NE.TCHD)THEN
       IF(BND(PCK(1)%ILAY)%X(ICOL,IROW).LE.0.0D0)CYCLE 
      ENDIF
      
      IF(ITOPIC.EQ.TFHB)THEN
       !## check whether one of the two is not equal to nodata
       DO I=1,NTOP; IF(PCK(JTOP(I))%X(ICOL,IROW).NE.HNOFLOW)EXIT; ENDDO
       !## found no data in either dataset - skip data point
       IF(I.GT.NTOP)CYCLE
      ELSE
       !## check nodata in dataset
       DO I=1,NTOP; IF(PCK(JTOP(I))%X(ICOL,IROW).EQ.HNOFLOW)EXIT; ENDDO
       !## found any nodata in dataset - skip data point
       IF(I.LE.NTOP)CYCLE
      ENDIF
       
      !## check bottom river if that is higher than river stage
      IF(ITOPIC.EQ.TRIV)PCK(3)%X(ICOL,IROW)=MIN(PCK(2)%X(ICOL,IROW),PCK(3)%X(ICOL,IROW))
      
      !## initially not assigned to any model layer
      TLP=0.0D0 

      !## assign to several layer based upon top/bot
      IF(PCK(1)%ILAY.EQ.0)THEN

       !## get filter fractions
       CALL PMANAGER_SAVEMF2005_PCK_ULSTRD_PARAM(PRJNLAY,ICOL,IROW,BND,TOP,BOT,KDW,TP,BT,KH,.FALSE.)
       SELECT CASE (ITOPIC)
        CASE (TDRN) !## drn - drainagelevel
         Z1=PCK(2)%X(ICOL,IROW); Z2=Z1
        CASE (TRIV) !## riv - waterlevel and bottom
         Z1=PCK(2)%X(ICOL,IROW); Z2=PCK(3)%X(ICOL,IROW)
        CASE (TOLF) !## olf drainagelevel
         Z1=PCK(1)%X(ICOL,IROW); Z2=Z1
        CASE (TGHB) !## ghb drainagelevel
         Z1=PCK(2)%X(ICOL,IROW); Z2=Z1
        CASE DEFAULT
         WRITE(*,*) 'Cannot come here: ERROR PMANAGER_SAVEMF2005_PCK'; PAUSE
       END SELECT
 
       !## get fraction per model layer
       CALL UTL_PCK_GETTLP(PRJNLAY,TLP,KH,TP,BT,Z1,Z2,MINKHT) 

      !## find uppermost active layer
      ELSEIF(PCK(1)%ILAY.EQ.-1)THEN

       DO ILAY=1,PRJNLAY; IF(BND(ILAY)%X(ICOL,IROW).NE.0)EXIT; ENDDO 
       !## assign to uppermost active layer
       IF(ILAY.LE.PRJNLAY)THEN; IF(BND(ILAY)%X(ICOL,IROW).GT.0)TLP(ILAY)=1.0D0; ENDIF

      ELSE

       !## chd package
       IF(ITOPIC.EQ.TCHD)THEN
        IF(BND(PCK(1)%ILAY)%X(ICOL,IROW).LT.0)TLP(PCK(1)%ILAY)=1.0D0
       !## assign to predefined layer - if not constant or inactive
       ELSE
        IF(BND(PCK(1)%ILAY)%X(ICOL,IROW).GT.0)TLP(PCK(1)%ILAY)=1.0D0
       ENDIF
       
      ENDIF
      
      DO ILAY=1,PRJNLAY
       !## not put into model layer
       IF(TLP(ILAY).LE.0.0D0)CYCLE
       !## skip inactive cells - this can happen whenever ilay=0 and stage is above top_l1 or ilay>0 and layer is inactive
       IF(BND(ILAY)%X(ICOL,IROW).EQ.0)CYCLE

       !## write specific packages
       SELECT CASE (ITOPIC)
        !## chd 
        CASE (TCHD)
         IF(BND(ILAY)%X(ICOL,IROW).LT.0)THEN
          !## check whether constant head is in appropriate cell - if not - skip it.
          LCHKCHD=.TRUE.
          !## head is in within current layer pck(jtop(1))%x(1,1:50)
          IF(PBMAN%ICHKCHD.EQ.1)LCHKCHD=PCK(JTOP(1))%X(ICOL,IROW).GT.BOT(ILAY)%X(ICOL,IROW)
          IF(LCHKCHD)THEN
           IF(PBMAN%SSYSTEM.EQ.0)THEN
            WRITE(JU,FRM) ILAY,IROW,ICOL,PCK(JTOP(1))%X(ICOL,IROW),PCK(JTOP(1))%X(ICOL,IROW),ISYS
           ELSE
            WRITE(JU,FRM) ILAY,IROW,ICOL,PCK(JTOP(1))%X(ICOL,IROW),PCK(JTOP(1))%X(ICOL,IROW),1
           ENDIF
           NP_IPER(IPER)=NP_IPER(IPER)+1
          ENDIF
         ENDIF
        !## olf
        CASE (TOLF)
         OLFCOND=(IDFGETAREA(PCK(JTOP(1)),ICOL,IROW)/COLF)   !## drainage conductance
         IF(PBMAN%SSYSTEM.EQ.0)THEN
          WRITE(JU,FRM) ILAY,IROW,ICOL,PCK(JTOP(1))%X(ICOL,IROW),OLFCOND,ISYS
         ELSE
          WRITE(JU,FRM) ILAY,IROW,ICOL,PCK(JTOP(1))%X(ICOL,IROW),OLFCOND,1
         ENDIF
         NP_IPER(IPER)=NP_IPER(IPER)+1
        !## fhb
        CASE (TFHB)
         IF(BND(ILAY)%X(ICOL,IROW).EQ. 2.0)THEN; NFLW=NFLW+1; FHBFLW(NFLW,NBDTIM)=PCK(JTOP(1))%X(ICOL,IROW); ENDIF      
         IF(BND(ILAY)%X(ICOL,IROW).EQ.-2.0)THEN; NHED=NHED+1; FHBHED(NHED,NBDTIM)=PCK(JTOP(2))%X(ICOL,IROW); ENDIF
        CASE DEFAULT
         IF(PCK(JTOP(2))%X(ICOL,IROW).GT.0.0D0)THEN

          DO I=1,NTOP; XTMP(I)=PCK(I)%X(ICOL,IROW); ENDDO
          XTMP(1)=XTMP(1)*TLP(ILAY)

          !## in current model (layers)
          IF(PBMAN%ILAY(ILAY).EQ.1)THEN
           JSYS=1; IF(PBMAN%SSYSTEM.EQ.0)JSYS=ISYS
           WRITE(JU,FRM) ILAY,IROW,ICOL,(XTMP(JTOP(I)),I=1,NTOP),JSYS
           NP_IPER(IPER)=NP_IPER(IPER)+1
          ENDIF
         
         ENDIF
       END SELECT

     ENDDO 

    ENDDO; ENDDO
    
   END SELECT

  ENDDO

  IF(ITOPIC.NE.TFHB.AND. &
     ITOPIC.NE.TUZF.AND. &
     ITOPIC.NE.TEVT.AND. &
     ITOPIC.NE.TRCH)THEN
   LINE=TRIM(ITOS(NP_IPER(IPER))); IF(PBMAN%IFORMAT.EQ.2)WRITE(IU,'(A)') TRIM(LINE)
  ENDIF
  
  !## maximum input per simulation
  MP=MAX(MP,NP_IPER(IPER))
    
  IF(PBMAN%IFORMAT.EQ.2)THEN
   SELECT CASE (ITOPIC)
    CASE (TDRN,TRIV,TGHB,TOLF,TCHD) !22,23,25,27,28)
     CALL IDFWRITEFREE_HEADER(JU,PRJIDF)
   END SELECT
  ENDIF
  
  CLOSE(JU)

  IF(PBMAN%IFORMAT.GE.2)THEN
   IF(NP_IPER(IPER).GT.0)THEN
    SFNAME=EXFNAME
    N=3; IF(PBMAN%IFORMAT.EQ.3)N=4; DO I=1,N; SFNAME=SFNAME(:INDEX(SFNAME,'\',.TRUE.)-1); ENDDO
    I=LEN_TRIM(SFNAME); SFNAME='.'//EXFNAME(I+1:)
    WRITE(IU,'(A)') 'OPEN/CLOSE '//TRIM(SFNAME)//' 1.0 (FREE) -1'
   ENDIF
  ENDIF
  
  IF(PBMAN%IFORMAT.EQ.3)WRITE(IU,'(A)') 'END PERIOD '

  !## store previous stress-period information for this timestep
  LPER=KPER

 ENDDO

 !## write fhb package
 IF(ITOPIC.EQ.TFHB)THEN
  IF(ALLOCATED(FHBFLW))THEN
   LINE=TRIM(ITOS(IFHBUN))//',1.0,1'; WRITE(IU,'(A)') TRIM(LINE)
   !## store values in fhb package
   I=0; DO ILAY=1,PRJNLAY; DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
    IF(BND(ILAY)%X(ICOL,IROW).EQ. 2)THEN
     I=I+1; WRITE(IU,'(3(I10,1X),F10.1,99(1X,G15.7))') ILAY,IROW,ICOL,1.0,(FHBFLW(I,J),J=1,NBDTIM)
    ENDIF
   ENDDO; ENDDO; ENDDO
  ENDIF
  IF(ALLOCATED(FHBHED))THEN
   LINE=TRIM(ITOS(IFHBUN))//',1.0,1'; WRITE(IU,'(A)') TRIM(LINE)
   !## store values in fhb package
   I=0; DO ILAY=1,PRJNLAY; DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
    IF(BND(ILAY)%X(ICOL,IROW).EQ.-2)THEN
    I=I+1; WRITE(IU,'(3(I10,1X),F10.1,99(1X,G15.7))') ILAY,IROW,ICOL,1.0,(FHBHED(I,J),J=1,NBDTIM)
    ENDIF
   ENDDO; ENDDO; ENDDO
  ENDIF
 ENDIF

 CLOSE(IU)
 IF(ALLOCATED(TLP))    DEALLOCATE(TLP)
 IF(ALLOCATED(TP))     DEALLOCATE(TP)
 IF(ALLOCATED(BT))     DEALLOCATE(BT)
 IF(ALLOCATED(KH))     DEALLOCATE(KH)
 IF(ALLOCATED(XTMP))   DEALLOCATE(XTMP)
 
 CALL PMANAGER_SAVEMF2005_DEALLOCATEPCK()

 !## apply nevtop/nrchop options
 SELECT CASE(ITOPIC)
  CASE (TUZF); NP_IPER(0)=NUZTOP
  CASE (TEVT); NP_IPER(0)=NEVTOP
  CASE (TRCH)
   IF(PBMAN%IFORMAT.EQ.2)NP_IPER(0)=NRCHOP
   IF(PBMAN%IFORMAT.EQ.3)NP_IPER(0)=NP_IPER(1)
  CASE DEFAULT; NP_IPER(0)=MP
 END SELECT

 IF(ITOPIC.EQ.TEVT.OR.ITOPIC.EQ.TRCH)THEN
  IF(LLAK.AND.NP_IPER(0).EQ.1)THEN 
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'It is compulsory to apply the '//TRIM(TOPICS(ITOPIC)%TNAME)//' package to the'//CHAR(13)// &
        'first active modellayer in combination with the LAK package.'//CHAR(13)// &
        'Assign zero (0) as a model layer for the package','Error')
   RETURN
  ENDIF
 ENDIF
  
 !## mf6 does not allow max dimensions to be zero
 IF(PBMAN%IFORMAT.EQ.3)NP_IPER(0)=MAX(1,NP_IPER(0))
 CALL UTL_MF2005_MAXNO(TRIM(DIRMNAME)//'.'//CPCK//VTXT//'_',(/NP_IPER(0)/))

 IF(ALLOCATED(NP_IPER))DEALLOCATE(NP_IPER)

 PMANAGER_SAVEMF2005_PCK=.TRUE.
  
 END FUNCTION PMANAGER_SAVEMF2005_PCK

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_LAK_READ(IPER,IPRT,KPER)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPER,IPRT
 INTEGER,INTENT(INOUT) :: KPER
 INTEGER :: I,ITOPIC,SCL_D,SCL_U,IROW,ICOL,JPER
 INTEGER(KIND=8) :: ITIME,JTIME
 
 PMANAGER_SAVEMF2005_LAK_READ=.TRUE.

 IF(.NOT.LLAK)RETURN
 
 PMANAGER_SAVEMF2005_LAK_READ=.FALSE.

 !## lak settings - use most frequent 
 ITOPIC=TLAK

 !## initialisation of lake package
 IF(IPER.EQ.0)THEN
  !## search for first lake definition in time
  DO JPER=1,PRJNPER
   !## get appropriate input file for first stress-period 
   KPER=PMANAGER_GETCURRENTIPER(JPER,ITOPIC,ITIME,JTIME)
   IF(KPER.GT.0)EXIT
  ENDDO
  !## nothing found
  IF(JPER.GT.PRJNPER)KPER=0
! ELSE
!  !## get appropriate input file for first stress-period 
!  KPER=PMANAGER_GETCURRENTIPER(IPER,ITOPIC,ITIME,JTIME)
!  !## nothing found
!  IF(IPER.EQ.1.AND.KPER.LE.0)KPER=0
 ENDIF
 
! IF(KPER.LT.0)THEN; PMANAGER_SAVEMF2005_LAK_READ=.TRUE.; RETURN; ENDIF
 
 !## get appropriate filename for first system and i-th subsystem for kper-th period
 ALLOCATE(FNAMES(TOPICS(ITOPIC)%NSUBTOPICS),PRJILIST(1)); PRJILIST=ITOPIC
 IF(PMANAGER_GETFNAMES(1,1,1,0,KPER).LE.0)RETURN

 DO I=1,SIZE(LAK)
  SELECT CASE (I)
   CASE (1);     SCL_D=0; SCL_U=7
   CASE DEFAULT; SCL_D=1; SCL_U=2
  END SELECT
  CALL IDFCOPY(PRJIDF,LAK(I))

  IF(.NOT.PMANAGER_SAVEMF2005_MOD_READ(LAK(I),ITOPIC,I,SCL_D,SCL_U,0,IPRT))RETURN 
  IF(I.EQ.1)THEN
   !## remove negative lake-numbers and nodata cells
   DO IROW=1,BND(1)%NROW; DO ICOL=1,BND(1)%NCOL
    IF(LAK(1)%X(ICOL,IROW).LT.0.0D0)LAK(1)%X(ICOL,IROW)=0.0D0
    IF(LAK(1)%X(ICOL,IROW).EQ.LAK(1)%NODATA)LAK(1)%X(ICOL,IROW)=0.0D0
   ENDDO; ENDDO
  ELSE
   !## clean rest of input
   CALL PMANAGER_SAVEMF2005_CORRECT(1,LAK,LAK(I),0,ITOPIC)
  ENDIF
 ENDDO
 
 DEALLOCATE(FNAMES,PRJILIST)

 PMANAGER_SAVEMF2005_LAK_READ=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_LAK_READ
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_LAK_SAVE(IULAK,IINI,IBATCH,DIR,KPER,DIRMNAME)
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: DIRMNAME
 INTEGER,INTENT(IN),OPTIONAL :: KPER
 INTEGER,INTENT(IN) :: IBATCH,IINI
 INTEGER,INTENT(INOUT) :: IULAK
 INTEGER :: NSSITR,I,J,IOP,ILAY,ITMP1,IFBND
 REAL(KIND=DP_KIND) :: THETA,SSCNCR,LVL,FCT,SURFDEPTH

 PMANAGER_SAVEMF2005_LAK_SAVE=.TRUE.

 IF(.NOT.LLAK)RETURN

 PMANAGER_SAVEMF2005_LAK_SAVE=.FALSE.
  
 !## initial timestep - open file and write header
 IF(KPER.EQ.1)THEN
 
  !## a THETA is automatically set to a value of 1.0D0 for all steady-state stress periods
  !## a THETA of 0.5 represents the average lake stage during a time step.
  !## a THETA of 1.0D0 represents the lake stage at the end of the time step.
  !## a negative THETA of applies for a SURFDEPTH decreases the lakebed conductance for vertical flow across a horizontal lakebed
  !## caused both by a groundwater head that is between the lakebed and the lakebed plus SURFDEPTH and a lake stage that is also
  !## between the lakebed and the lakebed plus SURFDEPTH. This method provides a smooth transition from a condition of no groundwater
  !## discharge to a lake, when groundwater head is below the lakebed, to a condition of increasing groundwater discharge to a lake as
  !## groundwater head becomes greater than the elevation of the dry lakebed. The method also allows for the transition of seepage from
  !## a lake to groundwater when the lake stage decreases to the lakebed elevation. Values of SURFDEPTH ranging from 0.01D0 to 0.5 have
  !## been used successfully in test simulations. SURFDEP is read only if THETA is specified as a negative value.
  THETA=-1.0D0; SSCNCR=0.01D0; NSSITR=100; SURFDEPTH=0.25D0

  !## read lake package (also adjust ibound for lakes)
  IULAK=UTL_GETUNIT(); CALL OSD_OPEN(IULAK,FILE=TRIM(DIRMNAME)//'.LAK7',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IULAK.EQ.0)RETURN

  !## set number of lakes
  LINE=TRIM(ITOS(NLAKES))//','//TRIM(ITOS(ILAKCB))
  WRITE(IULAK,'(A)') TRIM(LINE)

  !## set global settings
  LINE=TRIM(RTOS(THETA,'G',5))//','//TRIM(ITOS(NSSITR))//','//TRIM(RTOS(SSCNCR,'G',5))//','//TRIM(RTOS(SURFDEPTH,'G',5))
  WRITE(IULAK,'(A)') TRIM(LINE)
 
 ENDIF
 
 !## initial timestep
 IF(IINI.EQ.1)THEN

  !## get initial, minimal and maximal stages per lake
  DO I=1,NLAKES
   DO J=3,5
    SELECT CASE (J)
     CASE (3); IOP=1 !## initial (take average value)
     CASE (4); IOP=2 !## minimal
     CASE (5); IOP=3 !## maximal
    END SELECT
    IF(.NOT.PMANAGER_SAVEMF2005_LAKE_GETMEANVALUE(LAK(1)%X,LAK(J)%X,ULAKES(I),LVL,IBATCH,IOP))RETURN
    IF(J.EQ.3)THEN
     LINE=TRIM(RTOS(LVL,'G',5))
    ELSE
     LINE=TRIM(LINE)//','//TRIM(RTOS(LVL,'G',5))
    ENDIF
   ENDDO
   WRITE(IULAK,'(A)') TRIM(LINE)//'      ORIGINAL LAKE IDENTIFICATION: '//TRIM(ITOS(ULAKES(I)))
  ENDDO
 
  ITMP1=1; LINE='1,'//TRIM(ITOS(ITMP1))//',0'; WRITE(IULAK,'(A)') TRIM(LINE)
 
  !## save lake identification
  IFBND=0
  DO ILAY=1,PRJNLAY
   IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\LAK7\LKARR_L'//TRIM(ITOS(ILAY))//'.ARR', &
       LBD(ILAY),1,IULAK,ILAY,IFBND))RETURN 
  ENDDO
  !## get lakebed leakance
  IFBND=0
  DO ILAY=1,PRJNLAY
   IF(.NOT.PMANAGER_SAVEMF2005_MOD_U2DREL(TRIM(DIR)//'\LAK7\BDLKNC_L'//TRIM(ITOS(ILAY))//'.ARR', &
       LCD(ILAY),0,IULAK,ILAY,IFBND))RETURN
  ENDDO
  !## no connected lakes
  LINE=TRIM(ITOS(0))
  WRITE(IULAK,'(A)') TRIM(LINE) 
 
 ELSE
 
!  ITMP1=1; IF(KPER.EQ.0)ITMP1=0; IF(KPER.LT.0)ITMP1=-1
  !## iini=-1 to previous usage of lak settings but renewed read in rch/evt
  IF(KPER.GT.0)ITMP1= 1 !SIGN(KPER) !IINI !ABS(IINI)
  IF(KPER.LT.0)ITMP1=-1 !SIGN(KPER) !IINI !ABS(IINI)
  
  !## HIER MOET IINI OOK DE WAARDE 1 KUNNEN KRIJGEN ALS ER WEL RCH.EVT MOET WORDEN INGELZEN
  LINE='-1,'//TRIM(ITOS(ITMP1))//',0'; WRITE(IULAK,'(A)') TRIM(LINE)

 ENDIF

 !## get average prcplk,evaplk sum of rnf,wthdrw
 IF(ITMP1.GT.0)THEN
  IOP=1
  DO I=1,NLAKES
   DO J=7,10
    SELECT CASE (J)
     CASE (7,8); IOP=1; FCT=0.01D0 !## prcplk,evaplk
     CASE (9);   IOP=1; FCT=1.00D0 !## rnf
     CASE (10);  IOP=1; FCT=1.00D0 !## wthdrw
    END SELECT
    IF(.NOT.PMANAGER_SAVEMF2005_LAKE_GETMEANVALUE(LAK(1)%X,LAK(J)%X,ULAKES(I),LVL,IBATCH,IOP))RETURN
    IF(J.EQ.7)THEN
     LINE=TRIM(RTOS(LVL*FCT,'G',5))
    ELSE
     LINE=TRIM(LINE)//','//TRIM(RTOS(LVL*FCT,'G',5))
    ENDIF
   ENDDO
   WRITE(IULAK,'(A)') TRIM(LINE) 
  ENDDO
 ENDIF
 
 PMANAGER_SAVEMF2005_LAK_SAVE=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_LAK_SAVE

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_SFT_READ(IPRT)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPRT
 INTEGER :: ITOPIC,SCL_D,SCL_U,I,IINV,NTOP,NSYS,ISYS,KTOP,ICNST,ILAY
 REAL(KIND=DP_KIND) :: FCT,CNST,IMP
 CHARACTER(LEN=256) :: SFNAME
 
 PMANAGER_SAVEMF2005_SFT_READ=.TRUE.

 !## use sft1
 IF(.NOT.LSFT)RETURN

 PMANAGER_SAVEMF2005_SFT_READ=.FALSE.

 !## sft settings
 ITOPIC=TSFT; IINV=0; SCL_D=1

 DO I=1,SIZE(SFT); CALL IDFCOPY(PRJIDF,SFT(I)); ENDDO
 
 !## allocate memory for packages
 NTOP=SIZE(TOPICS(ITOPIC)%STRESS(1)%FILES,1); NSYS=SIZE(TOPICS(ITOPIC)%STRESS(1)%FILES,2)

 !## number of systems
 DO ISYS=1,NSYS

  !## number of subtopics
  DO KTOP=1,NTOP

   ICNST =TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%ICNST 
   CNST  =TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%CNST  
   FCT   =TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%FCT   
   IMP   =TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%IMP   
   ILAY  =TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%ILAY  
   !## always layer
   ILAY  =1
   SFNAME=TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%FNAME 

   WRITE(IPRT,'(1X,A,I3,A1,I1,A1,I4.3,3(A1,G15.7),A1,A)') TOPICS(ITOPIC)%TNAME(1:5)//',', &
      ISYS,',',ICNST,',',ILAY,',',FCT,',',IMP,',',CNST,',',CHAR(39)//TRIM(SFNAME)//CHAR(39)

   !## thickness
   IF(KTOP.EQ.1)THEN
   
    !## constant value
    IF(ICNST.EQ.1)THEN
     SFT(1)%X=CNST
    !## read/clip/scale idf file
    ELSEIF(TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%ICNST.EQ.2)THEN
     SFT(1)%FNAME=SFNAME
     SCL_U=2
     IF(.NOT.IDFREADSCALE(SFT(1)%FNAME,SFT(1),SCL_U,SCL_D,1.0D0,0))RETURN
    ENDIF
    CALL PMANAGER_SAVEMF2005_FCTIMP(0,ICNST,SFT(1),FCT,IMP,SCL_D)
    CALL PMANAGER_SAVEMF2005_CORRECT(ILAY,BND,SFT(1),0,ITOPIC)
   
   !## most frequent occurence for angles
   ELSEIF(KTOP.EQ.2)THEN

    !## constant value
    IF(ICNST.EQ.1)THEN
     SFT(2)%X=CNST
    !## read/clip/scale idf file
    ELSEIF(TOPICS(ITOPIC)%STRESS(1)%FILES(KTOP,ISYS)%ICNST.EQ.2)THEN
     SFT(2)%FNAME=SFNAME
     SCL_U=3
     IF(.NOT.IDFREADSCALE(SFT(ILAY)%FNAME,SFT(2),SCL_U,SCL_D,1.0D0,0))RETURN
    ENDIF
    CALL PMANAGER_SAVEMF2005_FCTIMP(0,ICNST,SFT(2),FCT,IMP,SCL_D)
    CALL PMANAGER_SAVEMF2005_CORRECT(ILAY,BND,SFT(2),0,ITOPIC)

   ENDIF   
  
  ENDDO 
 ENDDO  
 
 PMANAGER_SAVEMF2005_SFT_READ=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_SFT_READ
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_TDIS(DIRMNAME)
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIRMNAME
 INTEGER :: IU,KPER

 PMANAGER_SAVEMF2005_TDIS=.TRUE.; IF(PBMAN%IFORMAT.EQ.2)RETURN

 !## file already written
 IF(PBMAN%ISUBMODEL.GT.1)RETURN

 PMANAGER_SAVEMF2005_TDIS=.FALSE.
 
 !## construct pcg-file
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.TDIS6',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
 WRITE(IU,'(A)') '# TDIS6 File Generated by '//TRIM(UTL_IMODVERSION())

 WRITE(IU,'(/A/)') '#General Options'
 WRITE(IU,'(A)') 'BEGIN OPTIONS'
 WRITE(IU,'(A)') ' TIME_UNITS DAYS'
 DO KPER=1,PRJNPER; IF(SIM(KPER)%DELT.GT.0.0D0)EXIT; ENDDO
 IF(KPER.LE.PRJNPER)THEN
  WRITE(IU,'(A)') ' START_DATE_TIME '//TRIM(ITOS(SIM(KPER)%IYR))//'-'//TRIM(ITOS(SIM(KPER)%IMH))//'-'//TRIM(ITOS(SIM(KPER)%IDY))// &
    'T00:00:00TZD+01:00'
 ENDIF
 WRITE(IU,'(A)') 'END OPTIONS'

 WRITE(IU,'(/A/)') '#Time Dimensions'
 WRITE(IU,'(A)') 'BEGIN DIMENSIONS'
 WRITE(IU,'(A)') ' NPER '//TRIM(ITOS(PRJNPER))
 WRITE(IU,'(A)') 'END DIMENSIONS'

 WRITE(IU,'(/A/)') '#Stress periods'
 WRITE(IU,'(A)') 'BEGIN PERIODDATA'
 !## time information
 DO KPER=1,PRJNPER
  !## set delt.eq.1 otherwise crash in UZF package
  IF(SIM(KPER)%DELT.EQ.0.0D0)THEN
   LINE=TRIM(RTOS(1.0D0,'G',7))//','// &
        TRIM(ITOS(SIM(KPER)%NSTP))      //','// &
        TRIM(RTOS(SIM(KPER)%TMULT,'G',7))
  ELSE
   LINE=TRIM(RTOS(SIM(KPER)%DELT,'G',7))//','// &
        TRIM(ITOS(SIM(KPER)%NSTP))      //','// &
        TRIM(RTOS(SIM(KPER)%TMULT,'G',7))
  ENDIF
  LINE=TRIM(LINE)//' ['//TRIM(SIM(KPER)%CDATE)//']'
  WRITE(IU,'(A)') ' '//TRIM(LINE)
 ENDDO
 WRITE(IU,'(A)') 'END PERIODDATA'
 CLOSE(IU)

 PMANAGER_SAVEMF2005_TDIS=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_TDIS
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_MET(DIR,DIRMNAME)
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME
 INTEGER :: IU,KPER,I,N1,N2

 PMANAGER_SAVEMF2005_MET=.TRUE.; IF(PBMAN%IFORMAT.EQ.3)RETURN
 PMANAGER_SAVEMF2005_MET=.FALSE.
 
 !## write *.nam file(s)
 N1=1; N2=1; IF(PBMAN%IPESTP.EQ.1)THEN; N1=-PBMAN%NLINESEARCH; N2=SIZE(PEST%PARAM); ENDIF
 DO I=N1,N2
  !## skip zero
  IF(I.EQ.0)CYCLE
  IU=UTL_GETUNIT()
  IF(PBMAN%IPESTP.EQ.0)THEN
   CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.MET7',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
  ELSE
   IF(I.GT.0)THEN
    IF(PEST%PARAM(I)%PACT.EQ.0.OR.PEST%PARAM(I)%PIGROUP.LT.0)CYCLE
    CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'_P#'//TRIM(ITOS(I))//'.MET7',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
   ELSE
    CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'_L#'//TRIM(ITOS(ABS(I)))//'.MET7',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
   ENDIF
  ENDIF
  IF(IU.EQ.0)RETURN

  WRITE(IU,'(A)') '# MET7 File Generated by '//TRIM(UTL_IMODVERSION())
  LINE='COORD_XLL '//TRIM(RTOS(PRJIDF%XMIN,'F',3))   ; WRITE(IU,'(A)') TRIM(LINE)
  LINE='COORD_YLL '//TRIM(RTOS(PRJIDF%YMIN,'F',3))   ; WRITE(IU,'(A)') TRIM(LINE)
  LINE='COORD_XLL_NB '//TRIM(RTOS(PRJIDF%XMIN,'F',3)); WRITE(IU,'(A)') TRIM(LINE)
  LINE='COORD_YLL_NB '//TRIM(RTOS(PRJIDF%YMIN,'F',3)); WRITE(IU,'(A)') TRIM(LINE)
  LINE='COORD_XUR_NB '//TRIM(RTOS(PRJIDF%XMAX,'F',3)); WRITE(IU,'(A)') TRIM(LINE)
  LINE='COORD_YUR_NB '//TRIM(RTOS(PRJIDF%YMAX,'F',3)); WRITE(IU,'(A)') TRIM(LINE)
  !## look for first
  DO KPER=1,PRJNPER; IF(SIM(KPER)%DELT.GT.0.0D0)EXIT; ENDDO
  IF(KPER.LE.PRJNPER)THEN
!   LINE='IDATE_SAVE '//TRIM(ITOS(PBMAN%ISAVEENDDATE))
!   WRITE(IU,'(A)') TRIM(LINE)
   LINE='STARTTIME YEAR '//TRIM(ITOS(SIM(KPER)%IYR))//' MONTH '//TRIM(ITOS(SIM(KPER)%IMH))//' DAY '//TRIM(ITOS(SIM(KPER)%IDY))
   WRITE(IU,'(A)') TRIM(LINE)
  ENDIF
  IF(PBMAN%IPESTP.EQ.0)THEN
   LINE='RESULTDIR "'//TRIM(DIR(:INDEX(DIR,'\',.TRUE.)-1))//'"'; WRITE(IU,'(A)') TRIM(LINE)
  ELSE
   IF(I.GT.0)THEN
    LINE='RESULTDIR "'//TRIM(DIR(:INDEX(DIR,'\',.TRUE.)-1))//'\IPEST_P#'//TRIM(ITOS(I))//'"'; WRITE(IU,'(A)') TRIM(LINE)
   ELSE
    LINE='RESULTDIR "'//TRIM(DIR(:INDEX(DIR,'\',.TRUE.)-1))//'\IPEST_L#'//TRIM(ITOS(ABS(I)))//'"'; WRITE(IU,'(A)') TRIM(LINE)
   ENDIF
  ENDIF
  LINE='SAVEDOUBLE '//TRIM(ITOS(PBMAN%IDOUBLE)); WRITE(IU,'(A)') TRIM(LINE)
  CLOSE(IU)
 ENDDO
 
 PMANAGER_SAVEMF2005_MET=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_MET

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_HFB(IBATCH,DIRMNAME,IPRT,LTB)
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIRMNAME
 INTEGER,INTENT(IN) :: IBATCH,IPRT
 LOGICAL,INTENT(IN) :: LTB
 INTEGER :: IU,JU,ILAY,ITOPIC,NPHFB,MXFB
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IUGEN,IUDAT,NHFBNP
 CHARACTER(LEN=1) :: VTXT
 
 PMANAGER_SAVEMF2005_HFB=.TRUE.

 IF(.NOT.LHFB)RETURN

 PMANAGER_SAVEMF2005_HFB=.FALSE.

 VTXT='7'; IF(PBMAN%IFORMAT.EQ.3)VTXT='6' 
 
 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(DIRMNAME)//'.HFB'//VTXT//'...')
 IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing '//TRIM(DIRMNAME)//'.HFB'//VTXT//'...'

 !## creating and collect all faults
 JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=TRIM(DIRMNAME)//'_HFB.TXT',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
 ITOPIC=THFB; IF(.NOT.PMANAGER_SAVEMF2005_HFB_COMPUTE(PRJIDF,ITOPIC,JU,BND,TOP,BOT,IPRT,IBATCH))RETURN

 !## construct hfb-file
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.HFB'//VTXT//'_',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
 WRITE(IU,'(A)') '# HFB'//VTXT//' File Generated by '//TRIM(UTL_IMODVERSION())

 IF(PBMAN%IFORMAT.EQ.3)THEN
  WRITE(IU,'(/A/)') '#General Options'  
  WRITE(IU,'(A)') 'BEGIN OPTIONS'
!  WRITE(IU,'(A)') ' PRINT_INPUT'
  WRITE(IU,'(A)') 'END OPTIONS'
  WRITE(IU,'(/A/)') '#General Dimensions'  
  WRITE(IU,'(A)') 'BEGIN DIMENSIONS'
  WRITE(IU,'(1X,A)') ' MAXHFB NaN1#'
  WRITE(IU,'(A)') 'END DIMENSIONS'
  WRITE(IU,'(/A)') 'BEGIN PERIOD 1'
 ENDIF

 !## is the number of horizontal-flow barrier parameters
 NPHFB=0 
 !## is the number of HFB barriers not defined by parameters
 MXFB=0
 !## number of faults
 ALLOCATE(NHFBNP(PRJNLAY)); NHFBNP=0
 
 !## apply resistances
 IF(PBMAN%IFORMAT.EQ.2)THEN
  IF(LTB)THEN
   WRITE(IU,'(2I10,A)') NPHFB,MXFB,',NaN1# NOPRINT HFBRESIS SYSTEM'
  ELSE
   WRITE(IU,'(2I10,A)') NPHFB,MXFB,',NaN1# NOPRINT HFBFACT SYSTEM' 
  ENDIF
 ENDIF
 
 ALLOCATE(IUGEN(PRJNLAY),IUDAT(PRJNLAY)); IUGEN=0; IUDAT=0
 DO ILAY=1,PRJNLAY
  IUGEN(ILAY)=UTL_GETUNIT(); CALL OSD_OPEN(IUGEN(ILAY),FILE=TRIM(DIRMNAME)//'_HFB_L'//TRIM(ITOS(ILAY))//'.GEN', &
    STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
  IF(IUGEN(ILAY).EQ.0)RETURN
  IUDAT(ILAY)=UTL_GETUNIT(); CALL OSD_OPEN(IUDAT(ILAY),FILE=TRIM(DIRMNAME)//'_HFB_L'//TRIM(ITOS(ILAY))//'.DAT', &
    STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
  IF(IUDAT(ILAY).EQ.0)RETURN
  IF(LTB)THEN
   WRITE(IUDAT(ILAY),'(A10,3(1X,A15),A10)') 'NO','CONF_RESIS','UNCONF_RESIS','FRACTION','SYSTEM'
  ELSE
   WRITE(IUDAT(ILAY),'(A10,1X,A15,A10)') 'NO','FRACTION','SYSTEM'
  ENDIF
 ENDDO

 !## collect all faults
 JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=TRIM(DIRMNAME)//'_HFB.TXT',STATUS='OLD',ACTION='READ',FORM='FORMATTED')
 CALL PMANAGER_SAVEMF2005_HFB_EXPORT(NHFBNP,IU,JU,IUGEN,IUDAT,PRJIDF,LTB) 

 DO ILAY=1,PRJNLAY
  IF(NHFBNP(ILAY).GT.0)THEN
   CLOSE(IUGEN(ILAY)); CLOSE(IUDAT(ILAY))
  ELSE
   CLOSE(IUGEN(ILAY),STATUS='DELETE'); CLOSE(IUDAT(ILAY),STATUS='DELETE')
  ENDIF
 ENDDO

 DEALLOCATE(IUGEN,IUDAT)
 
 IF(PBMAN%IFORMAT.EQ.3)WRITE(IU,'(A)') 'END PERIOD'

 !## close hfb file
 CLOSE(IU); CLOSE(JU,STATUS='DELETE')

 CALL UTL_MF2005_MAXNO(TRIM(DIRMNAME)//'.HFB'//VTXT//'_',(/SUM(NHFBNP)/)) 
 
 DEALLOCATE(NHFBNP)
 
 PMANAGER_SAVEMF2005_HFB=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_HFB
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_OCD(DIRMNAME,MAINDIR)
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIRMNAME,MAINDIR
 INTEGER :: IU,ILAY,IPER

 PMANAGER_SAVEMF2005_OCD=.FALSE.
  
 IF(PBMAN%IFORMAT.EQ.2)THEN
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.OC',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
  WRITE(IU,'(A)') '# OC File Generated by '//TRIM(UTL_IMODVERSION())
 ELSE
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.OC6',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
  WRITE(IU,'(A)') '# OC6 File Generated by '//TRIM(UTL_IMODVERSION())
 ENDIF
 
 IF(PBMAN%IFORMAT.EQ.3)THEN
  WRITE(IU,'(/A/)') '#General Options'
  WRITE(IU,'(A)') 'BEGIN OPTIONS'
  WRITE(IU,'(1X,A)') 'BUDGET FILEOUT .\GWF_'//TRIM(ITOS(PBMAN%ISUBMODEL))//'\MODELOUTPUT\BUDGET\BUDGET.CBC'
  WRITE(IU,'(1X,A)') 'HEAD FILEOUT .\GWF_'//TRIM(ITOS(PBMAN%ISUBMODEL))//'\MODELOUTPUT\HEAD\HEAD.HED'
  CALL UTL_CREATEDIR(TRIM(MAINDIR)//'\GWF_'//TRIM(ITOS(PBMAN%ISUBMODEL))//'\MODELOUTPUT\BUDGET')
  CALL UTL_CREATEDIR(TRIM(MAINDIR)//'\GWF_'//TRIM(ITOS(PBMAN%ISUBMODEL))//'\MODELOUTPUT\HEAD')
!  WRITE(IU,'(A)') '  HEAD PRINT_FORMAT COLUMNS <columns> WIDTH <width> DIGITS <digits> <format>]'
  WRITE(IU,'(A)') 'END OPTIONS'
 ENDIF
 
 LINE='HEAD SAVE UNIT '//TRIM(ITOS(IHEDUN)); WRITE(IU,'(A)') TRIM(LINE)

 DO IPER=1,PRJNPER
 
  IF(PBMAN%IFORMAT.EQ.2)THEN

   LINE='PERIOD '//TRIM(ITOS(IPER))//' STEP '//TRIM(ITOS(SIM(IPER)%NSTP)); WRITE(IU,'(A)') TRIM(LINE)
   LINE='PRINT BUDGET'; WRITE(IU,'(A)') TRIM(LINE)

   IF(ASSOCIATED(PBMAN%ISAVE(TSHD)%ILAY))THEN
    IF(PBMAN%ISAVE(TSHD)%ILAY(1).EQ.-1)THEN
     LINE='SAVE HEAD'; DO ILAY=1,PRJNLAY; LINE=TRIM(LINE)//' '//TRIM(ITOS(ILAY)); ENDDO; WRITE(IU,'(A)') TRIM(LINE)
    ELSE
     LINE='SAVE HEAD'; DO ILAY=1,SIZE(PBMAN%ISAVE(TSHD)%ILAY); LINE=TRIM(LINE)//' '//TRIM(ITOS(PBMAN%ISAVE(TSHD)%ILAY(ILAY))); ENDDO; WRITE(IU,'(A)') TRIM(LINE)
    ENDIF
   ENDIF

   IF(ASSOCIATED(PBMAN%ISAVE(TKHV)%ILAY))THEN
    CALL PMANAGER_SAVEMF2005_OCD_ISAVE(PBMAN%ISAVE(TKHV)%ILAY,IBCFCB,IU)
   ELSEIF(ASSOCIATED(PBMAN%ISAVE(TKDW)%ILAY))THEN
    CALL PMANAGER_SAVEMF2005_OCD_ISAVE(PBMAN%ISAVE(TKDW)%ILAY,IBCFCB,IU)
   ENDIF
   IF(LUZF)CALL PMANAGER_SAVEMF2005_OCD_ISAVE(PBMAN%ISAVE(TUZF)%ILAY,IUZFCB1,IU)
   IF(LSFR)CALL PMANAGER_SAVEMF2005_OCD_ISAVE(PBMAN%ISAVE(TSFR)%ILAY,ISFRCB,IU)
   IF(LFHB)CALL PMANAGER_SAVEMF2005_OCD_ISAVE(PBMAN%ISAVE(TFHB)%ILAY,IFHBCB,IU)

   IF(LDRN.OR.LOLF)CALL PMANAGER_SAVEMF2005_OCD_ISAVE(PBMAN%ISAVE(TDRN)%ILAY,IDRNCB,IU)
   IF(LRIV.OR.LISG)CALL PMANAGER_SAVEMF2005_OCD_ISAVE(PBMAN%ISAVE(TRIV)%ILAY,IRIVCB,IU)
   IF(LGHB)CALL PMANAGER_SAVEMF2005_OCD_ISAVE(PBMAN%ISAVE(TGHB)%ILAY,IGHBCB,IU)
   IF(LWEL)CALL PMANAGER_SAVEMF2005_OCD_ISAVE(PBMAN%ISAVE(TWEL)%ILAY,IWELCB,IU)
   IF(LRCH)CALL PMANAGER_SAVEMF2005_OCD_ISAVE(PBMAN%ISAVE(TRCH)%ILAY,IRCHCB,IU)
   IF(LEVT)CALL PMANAGER_SAVEMF2005_OCD_ISAVE(PBMAN%ISAVE(TEVT)%ILAY,IEVTCB,IU)
   IF(LMNW)CALL PMANAGER_SAVEMF2005_OCD_ISAVE(PBMAN%ISAVE(TMNW)%ILAY,IWL2CB,IU)
   IF(LLAK)CALL PMANAGER_SAVEMF2005_OCD_ISAVE(PBMAN%ISAVE(TLAK)%ILAY,ILAKCB,IU) 

  ELSE
 
   WRITE(IU,'(/A/)') '#Stressperiod Save Options'
   WRITE(IU,'(A)') 'BEGIN PERIOD '//TRIM(ITOS(IPER))
   WRITE(IU,'(A)') ' SAVE HEAD ALL'
   WRITE(IU,'(A)') ' SAVE BUDGET ALL'
   WRITE(IU,'(A)') 'END PERIOD'
  
  ENDIF
  
 ENDDO
 
 CLOSE(IU)
 
 PMANAGER_SAVEMF2005_OCD=.TRUE.
 
 END FUNCTION PMANAGER_SAVEMF2005_OCD
 
 !####====================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_OCD_ISAVE(ISAVE,ID,IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN),POINTER,DIMENSION(:) :: ISAVE
 INTEGER,INTENT(IN) :: ID,IU
 INTEGER :: I
  
 IF(ASSOCIATED(ISAVE))THEN
  IF(ISAVE(1).EQ.-1)THEN
   LINE='SAVE BUDGET '//TRIM(ITOS(ID)); DO I=1,PRJNLAY; LINE=TRIM(LINE)//' '//TRIM(ITOS(I)); ENDDO
  ELSE
   LINE='SAVE BUDGET '//TRIM(ITOS(ID)); DO I=1,SIZE(ISAVE); LINE=TRIM(LINE)//' '//TRIM(ITOS(ISAVE(I))); ENDDO
  ENDIF
  WRITE(IU,'(A)') TRIM(LINE)
 ENDIF

 END SUBROUTINE PMANAGER_SAVEMF2005_OCD_ISAVE
 
 !####====================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_RUN_ISAVE(ISAVE,CID,IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN),POINTER,DIMENSION(:) :: ISAVE
 CHARACTER(LEN=*),INTENT(IN) :: CID
 INTEGER,INTENT(IN) :: IU
 INTEGER :: I,N
  
 IF(ASSOCIATED(ISAVE))THEN
  IF(ISAVE(1).EQ.-1)THEN
   LINE='1,1,0'
  ELSE
   N=SIZE(ISAVE)
   LINE='1,'//TRIM(ITOS(N)); DO I=1,SIZE(ISAVE); LINE=TRIM(LINE)//','//TRIM(ITOS(ISAVE(I))); ENDDO
  ENDIF
 ELSE
  LINE='1,0'
 ENDIF
 LINE=TRIM(LINE)//' '//TRIM(CID)

 WRITE(IU,'(A)') TRIM(LINE)
 
 END SUBROUTINE PMANAGER_SAVEMF2005_RUN_ISAVE

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_PCG(DIRMNAME)
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIRMNAME
 INTEGER :: IU

 PMANAGER_SAVEMF2005_PCG=.TRUE.

 IF(.NOT.LPCG)RETURN; IF(PBMAN%IFORMAT.EQ.3)RETURN

 PMANAGER_SAVEMF2005_PCG=.FALSE.

 !## construct pcg-file
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.PCG7',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
 WRITE(IU,'(A)') '# PCG7 File Generated by '//TRIM(UTL_IMODVERSION())
 CALL PMANAGER_SAVEPCG(IU,2)
 CLOSE(IU)
 
 PMANAGER_SAVEMF2005_PCG=.TRUE.
 
 END FUNCTION PMANAGER_SAVEMF2005_PCG

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_IMS(DIRMNAME)
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIRMNAME
 INTEGER :: IU

 PMANAGER_SAVEMF2005_IMS=.TRUE.; IF(PBMAN%IFORMAT.EQ.2)RETURN

 PMANAGER_SAVEMF2005_IMS=.FALSE.

 !## construct pcg-file
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.IMS6',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
 WRITE(IU,'(A)') '# IMS6 File Generated by '//TRIM(UTL_IMODVERSION())

 WRITE(IU,'(/A/)') '#General options'
 WRITE(IU,'(A)') 'BEGIN OPTIONS'
 WRITE(IU,'(A)') ' PRINT_OPTION SUMMARY'
! WRITE(IU,'(A)') ' COMPLEXITY '//TRIM(PBMAN%TCOMPLEX) !MODERATE'  !## simple complex
 WRITE(IU,'(A)') ' COMPLEXITY MODERATE'  !## simple complex
 WRITE(IU,'(A)') ' CSV_OUTPUT FILEOUT '//TRIM(DIRMNAME(INDEX(DIRMNAME,'\',.TRUE.)+1:))//'.CSV'
 WRITE(IU,'(A)') 'END OPTIONS'

!## set by complexity
 WRITE(IU,'(/A/)') '#Nonlinear options'
 WRITE(IU,'(A)') 'BEGIN NONLINEAR'
! WRITE(IU,'(A,G15.7)') ' OUTER_HCLOSE ',PCG%HCLOSE
! WRITE(IU,'(A,I10)') ' OUTER_MAXIMUM ',PCG%NOUTER
! WRITE(IU,'(A)') ' [UNDER_RELAXATION <under_relaxation>]'
! WRITE(IU,'(A)') ' [UNDER_RELAXATION_THETA <under_relaxation_theta>]'
! WRITE(IU,'(A)') ' [UNDER_RELAXATION_KAPPA <under_relaxation_kappa>]'
! WRITE(IU,'(A)') ' [UNDER_RELAXATION_GAMMA <under_relaxation_gamma>]'
! WRITE(IU,'(A)') ' [UNDER_RELAXATION_MOMENTUM <under_relaxation_momentum>]'
! WRITE(IU,'(A)') ' [BACKTRACKING_NUMBER <backtracking_number>]'
! WRITE(IU,'(A)') ' [BACKTRACKING_TOLERANCE <backtracking_tolerance>]'
! WRITE(IU,'(A)') ' [BACKTRACKING_REDUCTION_FACTOR <backtracking_reduction_factor>]'
! WRITE(IU,'(A)') ' [BACKTRACKING_RESIDUAL_LIMIT <backtracking_residual_limit>]'
 WRITE(IU,'(A)') 'END NONLINEAR'

 WRITE(IU,'(/A/)') '#Linear options'
 WRITE(IU,'(A)')       'BEGIN LINEAR'
! WRITE(IU,'(A,I10)')   ' INNER_MAXIMUM ',PCG%NINNER
! WRITE(IU,'(A,G15.7)') ' INNER_HCLOSE ',PCG%HCLOSE
! WRITE(IU,'(A,G15.7)') ' INNER_RCLOSE ',PCG%RCLOSE
! WRITE(IU,'(A)')       ' LINEAR_ACCELERATION CG'
! WRITE(IU,'(A,G15.7)') ' RELAXATION_FACTOR ',PCG%RELAX
! WRITE(IU,'(A)') ' [PRECONDITIONER_LEVELS <preconditioner_levels>]'
! WRITE(IU,'(A)') ' [PRECONDITIONER_DROP_TOLERANCE <preconditioner_drop_tolerance>]'
! WRITE(IU,'(A)') ' [NUMBER_ORTHOGONALIZATIONS <number_orthogonalizations>]'
! WRITE(IU,'(A)') ' [SCALING_METHOD <scaling_method>]'
! WRITE(IU,'(A)') ' [REORDERING_METHOD <reordering_method>]'
 WRITE(IU,'(A)')      'END LINEAR'

 CLOSE(IU)
 
 PMANAGER_SAVEMF2005_IMS=.TRUE.
 
 END FUNCTION PMANAGER_SAVEMF2005_IMS

 !###======================================================================
 SUBROUTINE PMANAGER_SAVEPCG(IU,IOPTION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,IOPTION

 !## prj file
 IF(IOPTION.EQ.0)THEN
  LINE=TRIM(ITOS(PCG%NOUTER))         //','// &
       TRIM(ITOS(PCG%NINNER))         //','// &
       TRIM(RTOS(PCG%HCLOSE,'G',5))   //','// &
       TRIM(RTOS(PCG%RCLOSE,'G',5))   //','// &
       TRIM(RTOS(PCG%RELAX ,'G',5))   //','// &
       TRIM(ITOS(PCG%NPCOND))         //','// &
       TRIM(ITOS(PCG%IPRPCG))         //','// &
       TRIM(ITOS(PCG%MUTPCG))         //','// &
       TRIM(RTOS(PCG%DAMPPCG ,'G',5)) //','// &
       TRIM(RTOS(PCG%DAMPPCGT ,'G',5))//','// &
       TRIM(ITOS(PCG%IQERROR))        //','// &
       TRIM(RTOS(PCG%QERROR,'G',5)) 
  WRITE(IU,'(A)') TRIM(LINE)
 !## run file
 ELSEIF(IOPTION.EQ.1)THEN
!  LINE=TRIM(ITOS(PCG%NOUTER))       //','// &
!       TRIM(ITOS(PCG%NINNER))       //','// &
!       TRIM(ITOS(PCG%NPCOND))
!  WRITE(IU,'(A)') TRIM(LINE)
 
 !## mf2005 file
 ELSEIF(IOPTION.EQ.2)THEN
  LINE=TRIM(ITOS(PCG%NOUTER))         //','// &
       TRIM(ITOS(PCG%NINNER))         //','// &
       TRIM(ITOS(PCG%NPCOND))
  WRITE(IU,'(A)') TRIM(LINE)
  LINE=TRIM(RTOS(PCG%HCLOSE,'G',5))   //','// &
       TRIM(RTOS(PCG%RCLOSE,'G',5))   //','// &
       TRIM(RTOS(PCG%RELAX ,'G',5))   //','// &
       TRIM(RTOS(1.0D0,'G',5))          //','// &
       TRIM(ITOS(PCG%IPRPCG))         //','// &
       TRIM(ITOS(PCG%MUTPCG))         //','// &
       TRIM(RTOS(PCG%DAMPPCG ,'G',5)) //','// &
       TRIM(RTOS(PCG%DAMPPCGT ,'G',5))
  WRITE(IU,'(A)') TRIM(LINE)

 ENDIF
 
 END SUBROUTINE PMANAGER_SAVEPCG
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_PKS(DIRMNAME)
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIRMNAME
 INTEGER :: IU,NP

 PMANAGER_SAVEMF2005_PKS=.TRUE.; IF(PBMAN%IFORMAT.EQ.3)RETURN; IF(.NOT.LPKS)RETURN

!## Parallel Krylov Solver Package
!isolver 1
!npc 2
!hclosepks 9.9999997E-05
!rclosepks 100.000
!mxiter 500
!innerit 30
!relax 0.9800000
!end

 PMANAGER_SAVEMF2005_PKS=.FALSE.

 !## a single processor used
 NP=1
 
 !## construct pcg-file
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRMNAME)//'.PKS',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(IU.EQ.0)RETURN
 WRITE(IU,'(A)') '# PKS File Generated by '//TRIM(UTL_IMODVERSION())

 !## number of processors
 LINE='ISOLVER '//TRIM(ITOS(NP)); WRITE(IU,'(A)') TRIM(LINE)

 !## preconditioner
 LINE='NPC '//TRIM(ITOS(2)); WRITE(IU,'(A)') TRIM(LINE)

 LINE='HCLOSEPKS '//TRIM(RTOS(PCG%HCLOSE,'E',7)); WRITE(IU,'(A)') TRIM(LINE)
 LINE='RCLOSEPKS '//TRIM(RTOS(PCG%RCLOSE,'E',7)); WRITE(IU,'(A)') TRIM(LINE)
 LINE='MXITER '//TRIM(ITOS(PCG%NOUTER));          WRITE(IU,'(A)') TRIM(LINE)
 LINE='INNERIT '//TRIM(ITOS(PCG%NINNER));         WRITE(IU,'(A)') TRIM(LINE)
 LINE='RELAX '//TRIM(RTOS(PCG%RELAX,'E',7));      WRITE(IU,'(A)') TRIM(LINE)
 WRITE(IU,'(A)') 'END'

 CLOSE(IU)
 
 PMANAGER_SAVEMF2005_PKS=.TRUE.
 
 END FUNCTION PMANAGER_SAVEMF2005_PKS

 !####====================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_DEALLOCATE()
 !####====================================================================
 IMPLICIT NONE

 IF(ALLOCATED(NP_IPER))DEALLOCATE(NP_IPER)

 CALL IDFDEALLOCATEX(PRJIDF)
 IF(ALLOCATED(BND))THEN
  CALL IDFDEALLOCATE(BND,SIZE(BND)); DEALLOCATE(BND)
 ENDIF
 IF(ALLOCATED(SHD))THEN
  CALL IDFDEALLOCATE(SHD,SIZE(SHD)); DEALLOCATE(SHD)
 ENDIF
 IF(ALLOCATED(KDW))THEN
  CALL IDFDEALLOCATE(KDW,SIZE(KDW)); DEALLOCATE(KDW)
 ENDIF
 IF(ALLOCATED(VCW))THEN
  CALL IDFDEALLOCATE(VCW,SIZE(VCW)); DEALLOCATE(VCW)
 ENDIF
 IF(ALLOCATED(TOP))THEN
  CALL IDFDEALLOCATE(TOP,SIZE(TOP)); DEALLOCATE(TOP)
 ENDIF
 IF(ALLOCATED(BOT))THEN
  CALL IDFDEALLOCATE(BOT,SIZE(BOT)); DEALLOCATE(BOT)
 ENDIF
 IF(ALLOCATED(ANA))THEN
  CALL IDFDEALLOCATE(ANA,SIZE(ANA)); DEALLOCATE(ANA)
 ENDIF
 IF(ALLOCATED(ANF))THEN
  CALL IDFDEALLOCATE(ANF,SIZE(ANF)); DEALLOCATE(ANF)
 ENDIF
 IF(ALLOCATED(KHV))THEN
  CALL IDFDEALLOCATE(KHV,SIZE(KHV)); DEALLOCATE(KHV)
 ENDIF
 IF(ALLOCATED(KVV))THEN
  CALL IDFDEALLOCATE(KVV,SIZE(KVV)); DEALLOCATE(KVV)
 ENDIF
 IF(ALLOCATED(KVA))THEN
  CALL IDFDEALLOCATE(KVA,SIZE(KVA)); DEALLOCATE(KVA)
 ENDIF
 IF(ALLOCATED(STO))THEN
  CALL IDFDEALLOCATE(STO,SIZE(STO)); DEALLOCATE(STO)
 ENDIF
 IF(ALLOCATED(SPY))THEN
  CALL IDFDEALLOCATE(SPY,SIZE(SPY)); DEALLOCATE(SPY)
 ENDIF
 IF(ALLOCATED(LAK))THEN
  CALL IDFDEALLOCATE(LAK,SIZE(LAK)); DEALLOCATE(LAK)
 ENDIF
 IF(ALLOCATED(LBD))THEN
  CALL IDFDEALLOCATE(LBD,SIZE(LBD)); DEALLOCATE(LBD)
 ENDIF
 IF(ALLOCATED(LCD))THEN
  CALL IDFDEALLOCATE(LCD,SIZE(LCD)); DEALLOCATE(LCD)
 ENDIF
 IF(ALLOCATED(SFT))THEN
  CALL IDFDEALLOCATE(SFT,SIZE(SFT)); DEALLOCATE(SFT)
 ENDIF

 IF(ALLOCATED(ULAKES))   DEALLOCATE(ULAKES)
 IF(ALLOCATED(FHBHED))   DEALLOCATE(FHBHED)
 IF(ALLOCATED(FHBFLW))   DEALLOCATE(FHBFLW)
 IF(ALLOCATED(FHBNBDTIM))DEALLOCATE(FHBNBDTIM)
 IF(ASSOCIATED(FNAMES))  DEALLOCATE(FNAMES)
 IF(ALLOCATED(PRJILIST)) DEALLOCATE(PRJILIST)
 
 END SUBROUTINE PMANAGER_SAVEMF2005_DEALLOCATE
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_MSP(DIR,DIRMNAME,IBATCH,IPRT) 
 !####====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME
 INTEGER,INTENT(IN) :: IBATCH,IPRT
 INTEGER :: ISYS,ILAY,ITOPIC,IPER,IINV,SCL_U,SCL_D
 INTEGER :: I,J,NIDF
 REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: NODATA
 CHARACTER(LEN=256) :: FFNAME,DIRMSP,FNNAME

 PMANAGER_SAVEMF2005_MSP=.TRUE.
 
 IF(.NOT.LMSP)RETURN
 
 PMANAGER_SAVEMF2005_MSP=.FALSE.

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing MetaSwap files ...')
 IF(IBATCH.EQ.1)WRITE(*,'(/1X,A)') 'Writing MetaSwap files ...'
  
 NIDF=22; ALLOCATE(NODATA(NIDF))

 !## allocate memory
 IF(ALLOCATED(SIMGRO))DEALLOCATE(SIMGRO); ALLOCATE(SIMGRO(PRJIDF%NCOL,PRJIDF%NROW))

 !## initialize unit numbers
 INDSB=0; IAREA=0; ISELSVAT=0; IGWMP=0; IMODSIM=0; ISCAP=0; IINFI=0; IIDF=0; IDFM_MSWP=0; IMSWP_DFM=0

 DIRMSP=DIR(:INDEX(DIR,'\',.TRUE.)-1)//'\MSWAPINPUT'

 !## open indsb
 FFNAME=TRIM(DIRMSP)//'\SVAT2SWNR_ROFF.INP'; INDSB=UTL_GETUNIT(); CALL OSD_OPEN(INDSB,FILE=FFNAME,STATUS='UNKNOWN',ACTION='WRITE')
 !## OPEN IAREA
 FFNAME=TRIM(DIRMSP)//'\AREA_SVAT.INP'; IAREA=UTL_GETUNIT(); CALL OSD_OPEN(IAREA,FILE=FFNAME,STATUS='UNKNOWN',ACTION='WRITE')
 !## OPEN ISCAP
 FFNAME=TRIM(DIRMSP)//'\SCAP_SVAT.INP'; ISCAP=UTL_GETUNIT(); CALL OSD_OPEN(ISCAP,FILE=FFNAME,STATUS='UNKNOWN',ACTION='WRITE')
 !## OPEN IGWMP
 FFNAME=TRIM(DIRMSP)//'\MOD2SVAT.INP'; IGWMP=UTL_GETUNIT(); CALL OSD_OPEN(IGWMP,FILE=FFNAME,STATUS='UNKNOWN',ACTION='WRITE')
 !## open MODFLOW dxc file
 FFNAME=TRIM(DIRMNAME)//'.DXC'; IDXC=UTL_GETUNIT(); CALL OSD_OPEN(IDXC,FILE=FFNAME,STATUS='UNKNOWN',ACTION='WRITE')
 !## OPEN MOD-SIM.TXT
 FFNAME=TRIM(DIRMSP)//'\MOD-SIM.TXT'; IMODSIM=UTL_GETUNIT(); CALL OSD_OPEN(IMODSIM,FILE=FFNAME,STATUS='UNKNOWN',ACTION='WRITE')
 !## OPEN ISELSVAT
 FFNAME=TRIM(DIRMSP)//'\SEL_SVAT_BDA.INP'; ISELSVAT=UTL_GETUNIT(); CALL OSD_OPEN(ISELSVAT,FILE=FFNAME,STATUS='UNKNOWN',ACTION='WRITE')
 !## OPEN INFI_SVAT.INP
 FFNAME=TRIM(DIRMSP)//'\INFI_SVAT.INP'; IINFI=UTL_GETUNIT(); OPEN(IINFI,FILE=FFNAME,STATUS='UNKNOWN',CARRIAGECONTROL='LIST',ACTION='WRITE')
 !## OPEN IDF_SVAT.INP
 FFNAME=TRIM(DIRMSP)//'\IDF_SVAT.INP'; IIDF=UTL_GETUNIT(); CALL OSD_OPEN(IIDF,FILE=FFNAME,STATUS='UNKNOWN',ACTION='WRITE')
 !## OPEN USCL_SVAT.INP
 FFNAME=TRIM(DIRMSP)//'\USCL_SVAT.INP'; IUSCL=UTL_GETUNIT(); CALL OSD_OPEN(IUSCL,FILE=FFNAME,STATUS='UNKNOWN',ACTION='WRITE')
 !## OPEN Dfm2dToMsw_WL.DMM
 FFNAME=TRIM(DIRMSP)//'\DFM2DTOMSW_WL.DMM_'; IDFM_MSWP=UTL_GETUNIT(); CALL OSD_OPEN(IDFM_MSWP,FILE=FFNAME,STATUS='UNKNOWN',ACTION='WRITE')
 !## OPEN Dfm2dToMsw_WL.DMM
 FFNAME=TRIM(DIRMSP)//'\MSWTODFM2D_DPV.DMM_'; IMSWP_DFM=UTL_GETUNIT(); CALL OSD_OPEN(IMSWP_DFM,FILE=FFNAME,STATUS='UNKNOWN',ACTION='WRITE')

 !## metaswap
 IARMWP=0
 IF(TOPICS(TCAP)%IACT_MODEL.EQ.1)THEN
  IF(ASSOCIATED(TOPICS(TCAP)%STRESS))THEN
   FFNAME=TOPICS(TCAP)%STRESS(1)%FILES(8,1)%FNAME
   IF(INDEX(UTL_CAP(FFNAME,'U'),'IPF').GT.0)IARMWP=1
  ENDIF
 ENDIF

 ISYS=0; ILAY=1; ITOPIC=TCAP; IPER=1; IINV=0

 ALLOCATE(FNAMES(TOPICS(ITOPIC)%NSUBTOPICS),PRJILIST(1)); PRJILIST=ITOPIC
 IF(PMANAGER_GETFNAMES(1,1,1,0,1).LE.0)RETURN
 
 !## open all files
 DO ISYS=1,NIDF
  !## skip ipf for artificial recharge
  IF(IARMWP.EQ.1.AND.ISYS.EQ.8)CYCLE
  
  SELECT CASE (ISYS)
   !## bnd
   CASE (1);          NODATA(ISYS)=-999.99D0; SCL_U=1; SCL_D=0
   !## lgn,root,soil,meteo
   CASE (2:5,7:9);    NODATA(ISYS)=-999.99D0; SCL_U=7; SCL_D=0
   !## surf,ponding,ponding,pwtlevel
   CASE (6,12,13,20); NODATA(ISYS)=-999.99D0; SCL_U=2; SCL_D=1
   !## soilfactor,cond.factor
   CASE (21,22);      NODATA(ISYS)=-999.99D0; SCL_U=2; SCL_D=0
   !## qinfub,qinfru
   CASE (18,19);      NODATA(ISYS)=-999.99D0; SCL_U=7; SCL_D=0 !6; SCL_D=0
   !## runoff,runoff,runon,runon
   CASE (14:17);      NODATA(ISYS)=-999.99D0; SCL_U=7; SCL_D=0 !6; SCL_D=0
   !## wetted area/urban area
   CASE (10,11);      NODATA(ISYS)=-999.99D0; SCL_U=5; SCL_D=0
  END SELECT

  !## read in data
  IF(.NOT.PMANAGER_SAVEMF2005_MOD_READ(PRJIDF,ITOPIC,ISYS,SCL_D,SCL_U,IINV,IPRT))RETURN

  SELECT CASE (ISYS)
   CASE (1);  SIMGRO%IBOUND=INT(PRJIDF%X)
   CASE (2);  SIMGRO%LGN=INT(PRJIDF%X)
   CASE (3);  SIMGRO%RZ=PRJIDF%X
   CASE (4);  SIMGRO%BODEM=INT(PRJIDF%X)
   CASE (5);  SIMGRO%METEO=INT(PRJIDF%X)
   CASE (6);  SIMGRO%MV=PRJIDF%X
   CASE (7);  SIMGRO%BEREGEN=INT(PRJIDF%X)
   CASE (8);  SIMGRO%BER_LAAG=INT(PRJIDF%X)
   CASE (9);  SIMGRO%BEREGEN_Q=PRJIDF%X
   CASE (10); SIMGRO%NOPP=PRJIDF%X
   CASE (11); SIMGRO%SOPP=PRJIDF%X
   CASE (12); SIMGRO%VXMU_SOPP=PRJIDF%X 
   CASE (13); SIMGRO%VXMU_ROPP=PRJIDF%X 
   CASE (14); SIMGRO%CRUNOFF_SOPP=PRJIDF%X
   CASE (15); SIMGRO%CRUNOFF_ROPP=PRJIDF%X
   CASE (16); SIMGRO%CRUNON_SOPP=PRJIDF%X
   CASE (17); SIMGRO%CRUNON_ROPP=PRJIDF%X
   CASE (18); SIMGRO%QINFBASIC_SOPP=PRJIDF%X
   CASE (19); SIMGRO%QINFBASIC_ROPP=PRJIDF%X
   CASE (20); SIMGRO%PWT_LEVEL=PRJIDF%X
   CASE (21); SIMGRO%MOISTURE=PRJIDF%X
   CASE (22); SIMGRO%COND=PRJIDF%X
  END SELECT

 ENDDO

 IF(.NOT.LPWT)SIMGRO%PWT_LEVEL=NODATA(20) 
 
 !## check input parameters
 CALL PMANAGER_SAVEMF2005_MSP_CHECK(NODATA) 
  
 ISYS=8
 CALL PMANAGER_SAVEMF2005_MSP_INPFILES(NODATA(20),TOPICS(ITOPIC)%STRESS(IPER)%FILES(ISYS,ILAY)%FNAME,LPWT,DIRMSP) 
   
 !## write extra files
 IF(ASSOCIATED(TOPICS(ITOPIC)%STRESS(1)%INPFILES))THEN
  J=SIZE(TOPICS(ITOPIC)%STRESS(1)%INPFILES)
  DO I=1,J
   FFNAME=UTL_CAP(TOPICS(ITOPIC)%STRESS(1)%INPFILES(I),'U')
   IF(INDEX(FFNAME,'METE_GRID.INP').GT.0)THEN
    CALL METASWAP_METEGRID1(FFNAME,TRIM(DIRMSP)//'\METE_GRID.INP')     
   ELSEIF(INDEX(FFNAME,'PARA_SIM.INP').GT.0)THEN
    CALL PMANAGER_SAVEMF2005_MSP_PARASIM(FFNAME,DIRMSP)
   ELSE
    FNNAME=TRIM(DIRMSP)//'\'//TRIM(FFNAME(INDEX(FFNAME,'\',.TRUE.)+1:))
    CALL SYSTEM('COPY "'//TRIM(FFNAME)//'" "'//TRIM(FNNAME)//'" /Y ')
   ENDIF
  ENDDO
 ENDIF
 
 !## metaswap 727 computing with recharge (possibility) if mete_grid.inp exists
 CALL METASWAP_METEGRID2(TRIM(DIRMSP))

 DEALLOCATE(SIMGRO,NODATA)
 
 PMANAGER_SAVEMF2005_MSP=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_MSP

 !###====================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_MSP_PARASIM(FNAME,DIRMSP)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME,DIRMSP
 INTEGER :: IU,JU,I,IOS,IC1,IC2,IR1,IR2,SNCOL,SNROW
 REAL(KIND=DP_KIND) :: X1,Y1,TINY
 CHARACTER(LEN=256) :: S,S1,S2,RUNDIR

 I=INDEX(FNAME,'\',.TRUE.)

 !## get working director
 CALL IOSDIRNAME(RUNDIR) 
 
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ')
 JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=TRIM(DIRMSP)//'\PARA_SIM.INP',STATUS='REPLACE',ACTION='WRITE')
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  S=TRIM(ADJUSTL(LINE)); S=UTL_CAP(S,'L')
  IF(S(1:14).EQ.'unsa_svat_path')THEN
   I=INDEX(LINE,'=')
   S1=ADJUSTL(LINE(I+1:LEN_TRIM(LINE)))
   READ(S1,*) S2 
   CALL UTL_REL_TO_ABS(RUNDIR,S2) 
   LINE=LINE(1:I)//' "'//TRIM(S2)//'"'
  END IF
  !## do not copy simgro_opt settings if existing
  IF(INDEX(TRIM(S),'simgro_opt').EQ.0)WRITE(JU,'(A)') TRIM(LINE)
 ENDDO

 CLOSE(IU)

 TINY=0.001D0
 CALL POL1LOCATE(PRJIDF%SX,PRJIDF%NCOL+1,PRJIDF%XMIN+TINY,IC1)
 CALL POL1LOCATE(PRJIDF%SX,PRJIDF%NCOL+1,PRJIDF%XMAX-TINY,IC2)
 CALL POL1LOCATE(PRJIDF%SY,PRJIDF%NROW+1,PRJIDF%YMAX-TINY,IR1)
 CALL POL1LOCATE(PRJIDF%SY,PRJIDF%NROW+1,PRJIDF%YMIN+TINY,IR2)

 !## check to make sure dimensions are within bounds!
 IC1  = MAX(1,IC1); IC2  = MIN(IC2,PRJIDF%NCOL)
 IR1  = MAX(1,IR1); IR2  = MIN(IR2,PRJIDF%NROW)
 SNCOL=(IC2-IC1)+1; SNROW=(IR2-IR1)+1
 
 X1=PRJIDF%XMIN
 Y1=PRJIDF%YMIN
 
 WRITE(JU,'(A)') '*'
 WRITE(JU,'(A)') '*  Parameters for IDF output'
 WRITE(JU,'(A)') '*'
 WRITE(JU,'(A)') '      simgro_opt             =     -1    ! simgro output file'
 WRITE(JU,'(A)') '      idf_per                =      1    ! Writing IDF files' 
 LINE='      idf_xmin                =      '//TRIM(RTOS(X1,'G',7))
 WRITE(JU,'(A)') TRIM(LINE)
 LINE='      idf_ymin                =      '//TRIM(RTOS(Y1,'G',7))
 WRITE(JU,'(A)') TRIM(LINE)
 LINE='      idf_dx                  =      '//TRIM(RTOS(PRJIDF%DX,'G',7))
 WRITE(JU,'(A)') TRIM(LINE)
 LINE='      idf_dy                  =      '//TRIM(RTOS(PRJIDF%DY,'G',7))
 WRITE(JU,'(A)') TRIM(LINE)
 LINE='      idf_ncol                =      '//TRIM(ITOS(SNCOL))
 WRITE(JU,'(A)') TRIM(LINE)
 LINE='      idf_nrow                =      '//TRIM(ITOS(SNROW))
 WRITE(JU,'(A)') TRIM(LINE) 
 LINE='      idf_nodata              =      '//TRIM(RTOS(-9999.00D0,'F',2)) 
 WRITE(JU,'(A)') TRIM(LINE)

 CLOSE(JU)

 END SUBROUTINE PMANAGER_SAVEMF2005_MSP_PARASIM
 
 !###====================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_MSP_INPFILES(NODATA_PWT,IPFFILE,LPWT,DIRMSP) 
 !###====================================================================
 IMPLICIT NONE
 LOGICAL :: LPWT
 REAL(KIND=DP_KIND),INTENT(IN) :: NODATA_PWT
 CHARACTER(LEN=*),INTENT(IN) :: IPFFILE,DIRMSP
 INTEGER,PARAMETER :: AEND=0 !## no surfacewater units
 INTEGER :: NUND,MDND,IROW,ICOL,LYBE,TYBE,BEREGENID,JROW,JCOL,N,M,I,J,JU,IC1,IC2,IR1,IR2
 REAL(KIND=DP_KIND) :: XC,YC,ARND,QBER,FLBE,TINY
 TYPE IPFOBJ
  INTEGER :: ILAY
  REAL(KIND=DP_KIND) :: X,Y,CAP
 END TYPE IPFOBJ
 TYPE(IPFOBJ),ALLOCATABLE,DIMENSION(:) :: IPF
 LOGICAL :: LURBAN
 INTEGER :: NDXC, UNID, IACT
 INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: DXCID
! INTEGER, DIMENSION(:,:), ALLOCATABLE :: RURALSVATID

 IF (ALLOCATED(DXCID)) DEALLOCATE(DXCID)
 ALLOCATE(DXCID(PRJIDF%NCOL,PRJIDF%NROW,PRJNLAY))
! ALLOCATE(RURALSVATID(PRJIDF%NCOL,PRJIDF%NROW))
 DXCID = 0
 NDXC = 0
! RURALSVATID=0
 
 IF(IARMWP.EQ.1)THEN
  JU=UTL_GETUNIT(); MDND=0
  DO J=1,2
   CALL OSD_OPEN(JU,FILE=IPFFILE,ACTION='READ',STATUS='OLD') 
   READ(JU,*) N; READ(JU,*) M
   IF(M.LT.5)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'IPF for artificial recharge should be at least 5 column, x,y,ilay,id,capacity','Error')
    RETURN
   ENDIF
   DO I=1,M+1; READ(JU,*) ; ENDDO
   IF(J.EQ.2)THEN; ALLOCATE(IPF(MDND)); IPF%ILAY=0; IPF%CAP=0.0D0; ENDIF
   DO I=1,N
    READ(JU,*) XC,YC,LYBE,NUND,QBER
    IF(J.EQ.1)MDND=MAX(MDND,NUND)
    IF(J.EQ.2)THEN; IPF(NUND)%X=XC; IPF(NUND)%Y=YC; IPF(NUND)%ILAY=LYBE; IPF(NUND)%CAP=QBER; ENDIF   
   ENDDO
   CLOSE(JU)
  ENDDO
 ENDIF

 !## get window of interest
 TINY=0.001D0
 CALL POL1LOCATE(PRJIDF%SX,PRJIDF%NCOL+1,PRJIDF%XMIN+TINY,IC1)
 CALL POL1LOCATE(PRJIDF%SX,PRJIDF%NCOL+1,PRJIDF%XMAX-TINY,IC2)
 CALL POL1LOCATE(PRJIDF%SY,PRJIDF%NROW+1,PRJIDF%YMAX-TINY,IR1)
 CALL POL1LOCATE(PRJIDF%SY,PRJIDF%NROW+1,PRJIDF%YMIN+TINY,IR2)
 !## check to make sure dimensions are within bounds!
 IC1=MAX(1,IC1); IC2=MIN(IC2,PRJIDF%NCOL)
 IR1=MAX(1,IR1); IR2=MIN(IR2,PRJIDF%NROW)

 WRITE(IDFM_MSWP,'(A)') 'NaN1#'
 WRITE(IMSWP_DFM,'(A)') 'NaN1#'

 DO IACT=1,2
  NUND=0; UNID=0 
  DO IROW=1,PRJIDF%NROW
   DO ICOL=1,PRJIDF%NCOL
    LURBAN=.FALSE.
    IF(SIMGRO(ICOL,IROW)%IBOUND.LE.0)CYCLE
   
    MDND=(IROW-1)*PRJIDF%NCOL+ICOL
    ARND=IDFGETAREA(PRJIDF,ICOL,IROW)
    ARND= ARND-SIMGRO(ICOL,IROW)%NOPP-SIMGRO(ICOL,IROW)%SOPP

    !## rural area > 0
    IF(ARND.GT.0.0D0)THEN
     LURBAN=.TRUE.
     NUND=NUND+1

!     IF(IACT.EQ.1)RURALSVATID(ICOL,IROW)=NUND

     CALL IDFGETLOC(PRJIDF,IROW,ICOL,XC,YC)

     !## write idf_svat.inp - inside area of interest
     IF(ICOL.GE.IC1.AND.ICOL.LE.IC2.AND.IROW.GE.IR1.AND.IROW.LE.IR2)THEN
      IF(IACT.EQ.2)WRITE(IIDF,'(3I10,2F15.3)') NUND,IROW-IR1+1,ICOL-IC1+1,XC,YC
     ENDIF
      
     !## write sel_svat_bda.inp
     IF(IACT.EQ.2)THEN
      WRITE(ISELSVAT,'(I10)') NUND

      WRITE(IDFM_MSWP,'(2(F10.3,1X),I10)') XC,YC,NUND
      WRITE(IMSWP_DFM,'(I10,2(1X,F10.3))') NUND,XC,YC

      !## write area_svat.inp
      WRITE(IAREA,'(I10,F10.1,F8.3,8X,I6,8X,8X,I6,F8.3,I10,2F8.3)') NUND,ARND,SIMGRO(ICOL,IROW)%MV, & 
            SIMGRO(ICOL,IROW)%BODEM,SIMGRO(ICOL,IROW)%LGN,SIMGRO(ICOL,IROW)%RZ/100.0D0,               &
            SIMGRO(ICOL,IROW)%METEO,1.0,1.0

      !## write svat2swnr_roff.inp ------------------
      WRITE(INDSB,'(I10,I10,F8.3,2F8.1)') NUND,AEND,SIMGRO(ICOL,IROW)%VXMU_ROPP,SIMGRO(ICOL,IROW)%CRUNOFF_ROPP, &
                                          SIMGRO(ICOL,IROW)%CRUNON_ROPP 

      !## write infi_svat.inp, infiltratiecapaciteit per cel, de rest -9999.
      WRITE(IINFI,'(I10,F8.3,4F8.1)') NUND,SIMGRO(ICOL,IROW)%QINFBASIC_ROPP,-9999.0,-9999.0,-9999.0,-9999.0

     ENDIF
    
     !## add couple location modflow
     CALL STOREDXC(DXCID,PRJIDF%NCOL,PRJIDF%NROW,PRJNLAY,1,IROW,ICOL,UNID,IACT)

     IF(IACT.EQ.2)THEN
      WRITE(IGWMP,'(I10,2X,I10,I5)')   UNID,NUND,1
      WRITE(IMODSIM,'(I10,2X,I10,I5)') UNID,NUND,1
     ENDIF
    
     !## BEGIN scap_svat.inp - grondwater + ow
     IF(IARMWP.EQ.0)THEN
      LYBE=SIMGRO(ICOL,IROW)%BER_LAAG
      TYBE=SIMGRO(ICOL,IROW)%BEREGEN
      QBER=SIMGRO(ICOL,IROW)%BEREGEN_Q
      JCOL=ICOL; JROW=IROW
     ELSE
      JCOL=0; JROW=0
      BEREGENID=INT(SIMGRO(ICOL,IROW)%BEREGEN)
      IF(BEREGENID.GT.0.AND.BEREGENID.LE.SIZE(IPF))THEN
       QBER=IPF(BEREGENID)%CAP
       LYBE=IPF(BEREGENID)%ILAY
       TYBE=1 !## groundwater
       CALL IDFIROWICOL(PRJIDF,JROW,JCOL,IPF(BEREGENID)%X,IPF(BEREGENID)%Y)
      ENDIF
     ENDIF    
    
     IF(JROW.NE.0.AND.JCOL.NE.0)THEN
    
      FLBE=0.0D0
      IF(TYBE.EQ.1)THEN
       !## maximum groundwater   abstraction mm/day fmmxabgw
       FLBE=QBER
      ELSEIF(TYBE.EQ.2)THEN
       !## maximum surface water abstraction mm/day fmmxabsw
       FLBE=QBER
      ENDIF
      
      !## maximum groundwater   abstraction mm/day fmmxabgw
      IF(FLBE.GT.0.0D0)THEN
       IF(TYBE.EQ.1)THEN
        IF(IACT.EQ.2)THEN
!         MDND2=RURALSVATID(JCOL,JROW)
         WRITE(ISCAP,'(I10,F8.2,24X,I10,I6)') NUND,QBER,NUND,LYBE
        ENDIF
       ELSEIF(TYBE.EQ.2)THEN
        IF(IACT.EQ.2)WRITE(ISCAP,'(I10,8X,F8.2,32X,I10)') NUND,QBER,AEND
       ENDIF
      ENDIF
    
      !## sprinkling from other than modellayer 1 or other location
      IF(TYBE.EQ.1.AND.LYBE.GT.1)THEN
 
       !## add couple location modflow
       CALL STOREDXC(DXCID,PRJIDF%NCOL,PRJIDF%NROW,PRJNLAY,LYBE,JROW,JCOL,UNID,IACT)
 
       IF(IACT.EQ.2)THEN
        WRITE(IGWMP,'(I10,2X,I10,I5)')   UNID,NUND,LYBE
        WRITE(IMODSIM,'(I10,2X,I10,I5)') UNID,NUND,LYBE
       ENDIF
      
      ENDIF 
     ENDIF

     !## END scap_svat.inp - grondwater + ow

     !## BEGIN mod2svat.inp; NB: als opp. water of glas dan laag = 0
    
     IF(.NOT.LPWT)THEN
      IF(IACT.EQ.2)WRITE(IUSCL,'(I10,3F8.3,8X,2I10)')   NUND,SIMGRO(ICOL,IROW)%MOISTURE,SIMGRO(ICOL,IROW)%COND,1.0,ICOL,IROW
     ELSE
      IF(SIMGRO(ICOL,IROW)%PWT_LEVEL.NE.NODATA_PWT)THEN
       IF(IACT.EQ.2)WRITE(IUSCL,'(I10,4F8.3,2I10)')    NUND,SIMGRO(ICOL,IROW)%MOISTURE,SIMGRO(ICOL,IROW)%COND,1.0D0, &
                                                       SIMGRO(ICOL,IROW)%MV-SIMGRO(ICOL,IROW)%PWT_LEVEL,ICOL,IROW
      ELSE
       IF(IACT.EQ.2)WRITE(IUSCL,'(I10,3F8.3,8X,2I10)') NUND,SIMGRO(ICOL,IROW)%MOISTURE,SIMGRO(ICOL,IROW)%COND,1.0D0,ICOL,IROW
      ENDIF
     ENDIF
    
     !## END mod2svat.inp; NB: als opp. water of glas dan laag = 0

    !## end rural area
    ENDIF

    !## urban area (verhard)
    ARND  =IDFGETAREA(PRJIDF,ICOL,IROW)
    ARND  =MIN(ARND,SIMGRO(ICOL,IROW)%SOPP) !< dit komt niet meer terug?
    IF(ARND.GT.0.0D0)THEN
     NUND=NUND+1

     !## write idf_svat.inp - inside area of interest
     IF(ICOL.GE.IC1.AND.ICOL.LE.IC2.AND.IROW.GE.IR1.AND.IROW.LE.IR2) THEN
      IF(IACT.EQ.2)WRITE(IIDF,'(3I10,2F15.3)') NUND,IROW-IR1+1,ICOL-IC1+1,XC,YC
     ENDIF
       
     !## write sel_svat_bda.inp
     IF(IACT.EQ.2)THEN
      WRITE(ISELSVAT,'(I10)') NUND

      CALL IDFGETLOC(PRJIDF,IROW,ICOL,XC,YC)
      WRITE(IDFM_MSWP,'(2(F10.3,1X),I10)') XC,YC,NUND
      WRITE(IMSWP_DFM,'(I10,2(1X,F10.3))') NUND,XC,YC

      WRITE(IAREA,'(I10,F10.1,F8.3,8X,I6,16X,I6,F8.3,I10,2F8.2)') &  
        NUND,ARND,SIMGRO(ICOL,IROW)%MV,SIMGRO(ICOL,IROW)%BODEM,18,0.1,SIMGRO(ICOL,IROW)%METEO,1.0D0,1.0D0

      WRITE(INDSB,'(2I10,F8.3,2F8.1)') NUND,0,SIMGRO(ICOL,IROW)%VXMU_SOPP,SIMGRO(ICOL,IROW)%CRUNOFF_SOPP,SIMGRO(ICOL,IROW)%CRUNON_SOPP 
     ENDIF
    
     !## add couple location modflow
     CALL STOREDXC(DXCID,PRJIDF%NCOL,PRJIDF%NROW,PRJNLAY,1,IROW,ICOL,UNID,IACT)

     IF(IACT.EQ.2)THEN
      WRITE(IGWMP,'(I10,2X,I10,I5)')   UNID,NUND,1
      WRITE(IMODSIM,'(I10,2X,I10,I5)') UNID,NUND,1
     ENDIF
    
     IF(.NOT.LPWT)THEN
      IF(IACT.EQ.2)WRITE(IUSCL,'(I10,3F8.3,8X,2I10)') NUND,SIMGRO(ICOL,IROW)%MOISTURE,SIMGRO(ICOL,IROW)%COND,1.0D0,ICOL,IROW
     ELSE
      IF(SIMGRO(ICOL,IROW)%PWT_LEVEL.NE.NODATA_PWT)THEN
       IF(IACT.EQ.2)WRITE(IUSCL,'(I10,4F8.3,2I10)') NUND,SIMGRO(ICOL,IROW)%MOISTURE,SIMGRO(ICOL,IROW)%COND,1.0D0, &
                                         SIMGRO(ICOL,IROW)%MV-SIMGRO(ICOL,IROW)%PWT_LEVEL,ICOL,IROW
      ELSE
       IF(IACT.EQ.2)WRITE(IUSCL,'(I10,3F8.3,8X,2I10)') NUND,SIMGRO(ICOL,IROW)%MOISTURE,SIMGRO(ICOL,IROW)%COND,1.0D0,ICOL,IROW
      ENDIF
     ENDIF
    
     !## write infi_svat.inp, infiltratiecapaciteit per cel, de rest -9999.
     IF(IACT.EQ.2)WRITE(IINFI,'(I10,F8.3,4F8.1)') NUND,SIMGRO(ICOL,IROW)%QINFBASIC_SOPP,-9999.0,-9999.0,-9999.0,-9999.0

    ENDIF

   ENDDO
  ENDDO
  IF(IACT.EQ.1) CALL GENIDDXC(DXCID,PRJIDF%NCOL,PRJIDF%NROW,PRJNLAY,NDXC)
 ENDDO

 CALL WRITEDXC(IDXC,DXCID,PRJIDF%NCOL,PRJIDF%NROW,PRJNLAY,NDXC)
 DEALLOCATE(DXCID) !,RURALSVATID)

 IF(IARMWP.EQ.1)DEALLOCATE(IPF)
 
 IF(IAREA.GT.0)    CLOSE(IAREA)
 IF(ISELSVAT.GT.0) CLOSE(ISELSVAT)
 IF(INDSB.GT.0)    CLOSE(INDSB)
 IF(ISCAP.GT.0)    CLOSE(ISCAP)
 IF(IGWMP.GT.0)    CLOSE(IGWMP)
 IF(IMODSIM.GT.0)  CLOSE(IMODSIM)
 IF(IINFI.GT.0)    CLOSE(IINFI)
 IF(IIDF.GT.0)     CLOSE(IIDF)
 IF(IUSCL.GT.0)    CLOSE(IUSCL)
 IF(IDFM_MSWP.GT.0)CLOSE(IDFM_MSWP)
 IF(IMSWP_DFM.GT.0)CLOSE(IMSWP_DFM)

 CALL UTL_MF2005_MAXNO(TRIM(DIRMSP)//'\DFM2DTOMSW_WL.DMM_',(/NUND/))
 CALL UTL_MF2005_MAXNO(TRIM(DIRMSP)//'\MSWTODFM2D_DPV.DMM_',(/NUND/))
 
 END SUBROUTINE PMANAGER_SAVEMF2005_MSP_INPFILES

 !###====================================================================
 SUBROUTINE STOREDXC(DXCID,NCOL,NROW,NLAY,ILAY,IROW,ICOL,ID,IACT)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW,NLAY
 INTEGER,INTENT(INOUT) :: ID
 INTEGER,INTENT(IN) :: IACT
 INTEGER,INTENT(INOUT), DIMENSION(NCOL,NROW,NLAY) :: DXCID
 INTEGER,INTENT(IN) :: ILAY, IROW, ICOL

 IF(IACT.EQ.2) THEN
  ID=DXCID(ICOL,IROW,ILAY)
  RETURN
 END IF
 
 IF(DXCID(ICOL,IROW,ILAY).EQ.0) THEN
  DXCID(ICOL,IROW,ILAY)=1
 ENDIF
 
 END SUBROUTINE STOREDXC

 !###====================================================================
 SUBROUTINE GENIDDXC(DXCID,NCOL,NROW,NLAY,ID)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW,NLAY
 INTEGER,INTENT(OUT) :: ID
 INTEGER,INTENT(INOUT), DIMENSION(NCOL,NROW,NLAY) :: DXCID
 INTEGER :: ILAY, ICOL, IROW
 
 ID=0
 DO ILAY=1,NLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(DXCID(ICOL,IROW,ILAY).NE.0)THEN
   ID=ID+1; DXCID(ICOL,IROW,ILAY)=ID
  ENDIF
 ENDDO; ENDDO; ENDDO
 
 END SUBROUTINE
 
 !###====================================================================
 SUBROUTINE WRITEDXC(IDXC,DXCID,NCOL,NROW,NLAY,NDXC)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDXC,NCOL,NROW,NLAY,NDXC 
 INTEGER,INTENT(IN), DIMENSION(NCOL,NROW,NLAY) :: DXCID
 INTEGER :: LUNCB,ICOL,IROW,ILAY,ID

 LUNCB=0
               
 WRITE(IDXC,'(2I10)') NDXC,LUNCB
 WRITE(IDXC,'(I10)')  NDXC 
 
 DO ILAY=1,NLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  ID=DXCID(ICOL,IROW,ILAY)
  IF(ID.NE.0)THEN        
   IF(ID.LT.0)THEN 
    WRITE(IDXC,*) -ILAY,IROW,ICOL,ABS(DXCID(ICOL,IROW,ILAY))
   ELSE
    WRITE(IDXC,*)  ILAY,IROW,ICOL,ABS(DXCID(ICOL,IROW,ILAY))
   ENDIF 
  ENDIF   
 ENDDO; ENDDO; ENDDO

 CLOSE(IDXC)

 END SUBROUTINE WRITEDXC
  
 !###====================================================================
 SUBROUTINE METASWAP_METEGRID1(FNAME,FNAME2)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: NA=11
 CHARACTER(LEN=1024) :: S
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=*),INTENT(IN) :: FNAME2
 INTEGER :: IU,JU,I,IOS
 CHARACTER(LEN=256), DIMENSION(11) :: SA
 CHARACTER(LEN=256) :: RUNDIR
 CHARACTER(LEN=8) :: FRM
 
 WRITE(FRM,'(A1,I2.2,A2)') '(',NA,'A)'
 
 CALL IOSDIRNAME(RUNDIR) 

 IU=UTL_GETUNIT(); OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ')
 JU=UTL_GETUNIT(); OPEN(JU,FILE=FNAME2,STATUS='REPLACE',ACTION='WRITE')
 DO
  READ(IU,'(A1024)',IOSTAT=IOS) S; IF(IOS.NE.0)EXIT
  IF(LEN_TRIM(S).EQ.0)CYCLE
  !## initial value
  SA='NoValue'
  READ(S,*,IOSTAT=IOS)(SA(I),I=1,NA)
  CALL UTL_REL_TO_ABS(RUNDIR,SA(3)) 
  CALL UTL_REL_TO_ABS(RUNDIR,SA(4))
  DO I=3,NA;   SA(I)='"'//TRIM(ADJUSTL(SA(I)))//'"'; END DO
  DO I=1,NA-1; SA(I)=TRIM(SA(I))//','              ; END DO
  WRITE(S,FRM)(TRIM(SA(I)),I=1,NA)
  WRITE(JU,'(A)') TRIM(S)
 ENDDO
 CLOSE(IU)
 CLOSE(JU)

 END SUBROUTINE 
 
 !###====================================================================
 SUBROUTINE METASWAP_METEGRID2(DIRMSP)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIRMSP
 LOGICAL :: LEX
 REAL(KIND=DP_KIND) :: TD
 INTEGER :: IU,IY
 CHARACTER(LEN=256) :: PRECFNAME,ETFNAME

 !## inquire the existence of mete_grid.inp
 INQUIRE(FILE=TRIM(DIRMSP)//'\METE_GRID.INP',EXIST=LEX); IF(.NOT.LEX)RETURN

 !## open mete_grid.inp
 IU=UTL_GETUNIT()
 OPEN(IU,FILE=TRIM(DIRMSP)//'\METE_GRID.INP',STATUS='OLD',ACTION='READ')
 READ(IU,*) TD,IY,PRECFNAME,ETFNAME
 CLOSE(IU)
 
 !## create coupling tables
 CALL METASWAP_METEGRID_INP(PRECFNAME,TRIM(DIRMSP)//'\SVAT2PRECGRID.INP')
 CALL METASWAP_METEGRID_INP(ETFNAME,  TRIM(DIRMSP)//'\SVAT2ETREFGRID.INP')

 END SUBROUTINE METASWAP_METEGRID2

 !###====================================================================
 SUBROUTINE METASWAP_METEGRID_INP(ASCIIFNAME,INPFNAME)
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: IU,A_NROW,A_NCOL,IROW,ICOL,IR1,IR2,IC1,IC2,NUND
 CHARACTER(LEN=*),INTENT(IN) :: ASCIIFNAME,INPFNAME
 REAL(KIND=DP_KIND) :: A_XLLC,A_YLLC,A_NODATA,A_CELLSIZE,IX,IY,ARND
 CHARACTER(LEN=52) :: TXT
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: PDELR,PDELC

 IF(ALLOCATED(PDELR))DEALLOCATE(PDELR)
 IF(ALLOCATED(PDELC))DEALLOCATE(PDELC)
 ALLOCATE(PDELR(2,PRJIDF%NCOL),PDELC(2,PRJIDF%NROW))

 !## read header of ascii file
 IU=UTL_GETUNIT(); OPEN(IU,FILE=ASCIIFNAME,ACTION='READ',STATUS='OLD')
 READ(IU,*) TXT,A_NCOL
 READ(IU,*) TXT,A_NROW
 READ(IU,*) TXT,A_XLLC
 TXT=UTL_CAP(TXT,'U');IX=0.0D0; IF(TRIM(TXT).EQ.'XLLCENTER')IX=1.0D0
 READ(IU,*) TXT,A_YLLC
 TXT=UTL_CAP(TXT,'U'); IY=0.0D0; IF(TRIM(TXT).EQ.'YLLCENTER')IY=1.0D0
 READ(IU,*) TXT,A_CELLSIZE
 READ(IU,*) TXT,A_NODATA
 A_XLLC=A_XLLC-(IX*(A_CELLSIZE/2.0D0)); A_YLLC=A_YLLC-(IY*(A_CELLSIZE/2.0D0))
 CLOSE(IU)

 CALL IMOD_UTL_SCALE1PDELRC(A_XLLC,A_YLLC,A_XLLC+(A_NCOL*A_CELLSIZE),A_YLLC+(A_NROW*A_CELLSIZE), &
           PRJIDF%SX,PRJIDF%SY,PDELR,PDELC,PRJIDF%NROW,PRJIDF%NCOL,A_CELLSIZE,A_NROW,A_NCOL,0,0,0)

 !## write koppeltabel
 IU=UTL_GETUNIT(); OPEN(IU,FILE=INPFNAME,ACTION='WRITE',STATUS='UNKNOWN')

 !## fill svat connection to recharge/et based upon svat-units
 NUND=0 
 DO IROW=1,PRJIDF%NROW
  DO ICOL=1,PRJIDF%NCOL
   !## rural area
   ARND=IDFGETAREA(PRJIDF,ICOL,IROW)
   ARND=ARND-SIMGRO(ICOL,IROW)%NOPP-SIMGRO(ICOL,IROW)%SOPP
   IF(SIMGRO(ICOL,IROW)%IBOUND.GT.0.AND.ARND.GT.0.0)THEN
    NUND  =NUND+1
    IR1=PDELC(1,IROW); IF(IR1.LT.0)IR1=PDELC(1,ABS(IR1))
    IR2=PDELC(2,IROW); IF(IR2.LT.0)IR2=PDELC(2,ABS(IR2))
    IC1=PDELR(1,ICOL); IF(IC1.LT.0)IC1=PDELR(1,ABS(IC1))
    IC2=PDELR(2,ICOL); IF(IC2.LT.0)IC2=PDELR(2,ABS(IC2))
    WRITE(IU,'(3I10,10X,2I10)') NUND,IR1,IC1,IR2,IC2
   ENDIF
   !## urban area
   ARND=IDFGETAREA(PRJIDF,ICOL,IROW)
   ARND=MIN(ARND,SIMGRO(ICOL,IROW)%SOPP)
   IF(SIMGRO(ICOL,IROW)%IBOUND.GT.0.AND.ARND.GT.0.0)THEN
    NUND=NUND+1
    IR1=PDELC(1,IROW); IF(IR1.LT.0)IR1=PDELC(1,ABS(IR1))
    IR2=PDELC(2,IROW); IF(IR2.LT.0)IR2=PDELC(2,ABS(IR2))
    IC1=PDELR(1,ICOL); IF(IC1.LT.0)IC1=PDELR(1,ABS(IC1))
    IC2=PDELR(2,ICOL); IF(IC2.LT.0)IC2=PDELR(2,ABS(IC2))
    WRITE(IU,'(3I10,10X,2I10)') NUND,IR1,IC1,IR2,IC2
   ENDIF
  ENDDO
 ENDDO

 CLOSE(IU)

 IF(ALLOCATED(PDELR))DEALLOCATE(PDELR)
 IF(ALLOCATED(PDELC))DEALLOCATE(PDELC)

 END SUBROUTINE METASWAP_METEGRID_INP
 
 !###====================================================================
 SUBROUTINE IMOD_UTL_SCALE1PDELRC(XMIN,YMIN,XMAX,YMAX,SXX,SYY,PDELR,PDELC,NROW,NCOL,CS,NROWIDF,NCOLIDF,IU,IEQ,ITB)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL,NROWIDF,NCOLIDF,IU,IEQ,ITB
 REAL(KIND=8),INTENT(IN) :: CS,XMIN,YMIN,XMAX,YMAX
 REAL(KIND=8),INTENT(IN),DIMENSION(0:NCOL) :: SXX
 REAL(KIND=8),INTENT(IN),DIMENSION(0:NROW) :: SYY
 REAL(KIND=8) :: DX,DY
 INTEGER,INTENT(OUT),DIMENSION(2,NCOL) :: PDELR
 INTEGER,INTENT(OUT),DIMENSION(2,NROW) :: PDELC
 INTEGER :: I,J,IREC
 CHARACTER(LEN=256) :: IDFNAME
 REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: DELRIDF,DELCIDF

 IF(XMIN.GT.SXX(0).OR.XMAX.LT.SXX(NCOL).OR.YMIN.GT.SYY(NROW).OR.YMAX.LT.SYY(0))THEN
  INQUIRE(UNIT=IU,NAME=IDFNAME)
  WRITE(*,'(A)') '======================================='
  WRITE(*,'(A)') 'Warning!'
  WRITE(*,'(A)') 'File: '//TRIM(IDFNAME)
  WRITE(*,'(A)') 'Undersizes current model dimensions!'
  IF(XMIN.GT.SXX(0))THEN
   WRITE(*,'(A)') 'XMIN IDF '//TRIM(RTOS(XMIN,'F',2))//' > XMIN MODEL '//TRIM(RTOS(SXX(0),'F',2))
  ENDIF
  IF(XMAX.LT.SXX(NCOL))THEN
   WRITE(*,'(A)') 'XMAX IDF '//TRIM(RTOS(XMAX,'F',2))//' < XMAX MODEL '//TRIM(RTOS(SXX(NCOL),'F',2))
  ENDIF
  IF(YMIN.GT.SYY(NROW))THEN
   WRITE(*,'(A)') 'YMIN IDF '//TRIM(RTOS(YMIN,'F',2))//' > YMIN MODEL '//TRIM(RTOS(SYY(NROW),'F',2))
  ENDIF
  IF(YMAX.LT.SYY(0))THEN
   WRITE(*,'(A)') 'YMAX IDF '//TRIM(RTOS(YMAX,'F',2))//' < YMAX MODEL '//TRIM(RTOS(SYY(0),'F',2))
  ENDIF
  WRITE(*,'(A)') '======================================='
  WRITE(*,'(A)') 'Error'
 ENDIF

 IF(ALLOCATED(DELRIDF))DEALLOCATE(DELRIDF)
 IF(ALLOCATED(DELCIDF))DEALLOCATE(DELCIDF)
 ALLOCATE(DELRIDF(0:NCOLIDF),DELCIDF(0:NROWIDF))

 DELRIDF(0)=XMIN
 DELCIDF(0)=YMAX

 IF(IEQ.EQ.0)THEN
  DO I=1,NCOLIDF; DELRIDF(I)=XMIN+REAL(I)*CS; ENDDO
  DO I=1,NROWIDF; DELCIDF(I)=YMAX-REAL(I)*CS; ENDDO
 ELSEIF(IEQ.EQ.1)THEN
  IREC      =10+ITB*2
  DO I=1,NCOLIDF
   IREC=IREC+1
   READ(IU,REC=IREC+ICF) DELRIDF(I)
   DELRIDF(I)=DELRIDF(I-1)+DELRIDF(I)
  END DO
  DO I=1,NROWIDF
   IREC=IREC+1
   READ(IU,REC=IREC+ICF) DELCIDF(I)
   DELCIDF(I)=DELCIDF(I-1)-DELCIDF(I)
  END DO
 ENDIF

 !## start/end column direction
 DO I=1,NCOL
  CALL POL1LOCATE(DELRIDF,NCOLIDF+1,SXX(I-1),PDELR(1,I))
  !## check whether position is exact equally
  J=PDELR(1,I)
  IF(J.LE.NCOLIDF)THEN
   IF(DELRIDF(J).EQ.SXX(I-1))PDELR(1,I)=PDELR(1,I)+1
  ENDIF
  CALL POL1LOCATE(DELRIDF,NCOLIDF+1,SXX(I),PDELR(2,I))
  PDELR(1,I)=MIN(PDELR(1,I),NCOLIDF)
  PDELR(2,I)=MIN(PDELR(2,I),NCOLIDF)
 ENDDO

 DO I=1,NROW
  CALL POL1LOCATE(DELCIDF,NROWIDF+1,SYY(I-1),PDELC(1,I))
  CALL POL1LOCATE(DELCIDF,NROWIDF+1,SYY(I),PDELC(2,I))
  !## check whether position is exact equally
  J=PDELC(2,I)
  IF(J.LE.NROWIDF)THEN
   IF(DELCIDF(J-1).EQ.SYY(I))PDELC(2,I)=PDELC(2,I)-1
  ENDIF
  PDELC(1,I)=MIN(PDELC(1,I),NROWIDF)
  PDELC(2,I)=MIN(PDELC(2,I),NROWIDF)
 ENDDO
 IF(ALLOCATED(DELRIDF))DEALLOCATE(DELRIDF)
 IF(ALLOCATED(DELCIDF))DEALLOCATE(DELCIDF)

 DO I=1,NCOL
  IF(PDELR(2,I).LT.PDELR(1,I))then
   DX        =(SXX(I-1)-XMIN)/CS
   PDELR(1,I)=INT(DX)+1
   DX        =(SXX(I)-XMIN)/CS
   PDELR(2,I)=INT(DX)+1
   DX=SXX(I)-XMIN
   IF(MOD(DX,CS).EQ.0.0)PDELR(2,I)=PDELR(2,I)-1
   WRITE(*,'(A)') 'PDELR(2,I).LT.PDELR(1,I)'
  ENDIF
 ENDDO
 DO I=1,NROW
  IF(PDELC(2,I).LT.PDELC(1,I))THEN
   DY=(YMAX-SYY(I-1))/CS
   PDELC(1,I)=INT(DY)+1
   DY=(YMAX-SYY(I))
   PDELC(2,I)=INT(DY)+1
   DY=YMAX-SYY(I)
   IF(MOD(DY,CS).EQ.0.0)PDELC(2,I)=PDELC(2,I)-1
   WRITE(*,'(A)') 'PDELC(2,I).LT.PDELC(1,I)'
  ENDIF
 ENDDO

 !## adjust pdelr/pdelc in case reading idf is coarser, then you don't need to read it in again, values will be copied in READCOPYVALUES_R()
 J=1
 DO I=2,NCOL
  IF(PDELR(1,I).EQ.PDELR(1,J).AND. &
     PDELR(2,I).EQ.PDELR(2,J))THEN
   PDELR(1,I)=-J
   PDELR(2,I)=-J
  ELSE
   J=I
  ENDIF
 END DO
 J=1
 DO I=2,NROW
  IF(PDELC(1,I).EQ.PDELC(1,J).AND. &
     PDELC(2,I).EQ.PDELC(2,J))THEN
   PDELC(1,I)=-J
   PDELC(2,I)=-J
  ELSE
   J=I
  ENDIF
 END DO

 END SUBROUTINE IMOD_UTL_SCALE1PDELRC
 
 !###====================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_MSP_CHECK(NODATA) 
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),DIMENSION(:),INTENT(IN) :: NODATA
 INTEGER,DIMENSION(:),ALLOCATABLE :: IERROR
 INTEGER :: IROW,ICOL,STRLEN
 REAL(KIND=DP_KIND) :: DXY,ARND
 CHARACTER(LEN=:),ALLOCATABLE :: STR

 !## inactivate constant head boundaries and inactive nodes
 DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
  IF(BND(1)%X(ICOL,IROW).LE.0.0D0)SIMGRO(ICOL,IROW)%IBOUND=0
 ENDDO; ENDDO
 
 !## skip corners irt anisotropy package
 SIMGRO(1       ,1             )%IBOUND=0
 SIMGRO(1       ,PRJIDF%NROW   )%IBOUND=0
 SIMGRO(PRJIDF%NCOL,1          )%IBOUND=0
 SIMGRO(PRJIDF%NCOL,PRJIDF%NROW)%IBOUND=0
 
 !## make sure that for sopp>0 there is a vxmu value, turn nopp otherwise off
 DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
  IF(SIMGRO(ICOL,IROW)%VXMU_SOPP.EQ.NODATA(12))SIMGRO(ICOL,IROW)%SOPP=0.0D0
  IF(SIMGRO(ICOL,IROW)%SOPP.GT.0.0D0)THEN
   IF(SIMGRO(ICOL,IROW)%VXMU_SOPP     .EQ.NODATA(12))SIMGRO(ICOL,IROW)%SOPP=0.0D0
   IF(SIMGRO(ICOL,IROW)%CRUNOFF_SOPP  .EQ.NODATA(14))SIMGRO(ICOL,IROW)%SOPP=0.0D0
   IF(SIMGRO(ICOL,IROW)%CRUNON_SOPP   .EQ.NODATA(16))SIMGRO(ICOL,IROW)%SOPP=0.0D0
   IF(SIMGRO(ICOL,IROW)%QINFBASIC_SOPP.EQ.NODATA(18))SIMGRO(ICOL,IROW)%SOPP=0.0D0
  ENDIF
  DXY=IDFGETAREA(PRJIDF,ICOL,IROW)
  IF(SIMGRO(ICOL,IROW)%VXMU_ROPP.EQ.NODATA(13))SIMGRO(ICOL,IROW)%NOPP=DXY !## surface water, no metaswap
  ARND=DXY-SIMGRO(ICOL,IROW)%NOPP-SIMGRO(ICOL,IROW)%SOPP
  !## rural area
  IF(ARND.GT.0.0D0)THEN
   IF(SIMGRO(ICOL,IROW)%VXMU_ROPP     .EQ.NODATA(13))SIMGRO(ICOL,IROW)%NOPP=ARND !## surface water, no metaswap
   IF(SIMGRO(ICOL,IROW)%CRUNOFF_ROPP  .EQ.NODATA(15))SIMGRO(ICOL,IROW)%NOPP=ARND !## surface water, no metaswap
   IF(SIMGRO(ICOL,IROW)%CRUNON_ROPP   .EQ.NODATA(17))SIMGRO(ICOL,IROW)%NOPP=ARND !## surface water, no metaswap
   IF(SIMGRO(ICOL,IROW)%QINFBASIC_ROPP.EQ.NODATA(19))SIMGRO(ICOL,IROW)%NOPP=ARND !## surface water, no metaswap
  ENDIF
 ENDDO; ENDDO

 !## check input
 ALLOCATE(IERROR(22)); IERROR=0
 DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
  IF(SIMGRO(ICOL,IROW)%IBOUND.GT.0)THEN
   IF(SIMGRO(ICOL,IROW)%LGN.EQ.NODATA(2))            IERROR(2) =IERROR(2)+1
   IF(SIMGRO(ICOL,IROW)%RZ.EQ.NODATA(3))             IERROR(3) =IERROR(3)+1
   IF(SIMGRO(ICOL,IROW)%BODEM.EQ.NODATA(4))          IERROR(4) =IERROR(4)+1
   IF(SIMGRO(ICOL,IROW)%METEO.EQ.NODATA(5))          IERROR(5) =IERROR(5)+1
   IF(SIMGRO(ICOL,IROW)%MV.EQ.NODATA(6))             IERROR(6) =IERROR(6)+1
   IF(SIMGRO(ICOL,IROW)%BEREGEN.EQ.NODATA(7))        IERROR(7) =IERROR(7)+1
   IF(IARMWP.EQ.0)THEN
    IF(SIMGRO(ICOL,IROW)%BER_LAAG.EQ.NODATA(8))      IERROR(8) =IERROR(8)+1
    IF(SIMGRO(ICOL,IROW)%BEREGEN_Q.EQ.NODATA(9))     IERROR(9) =IERROR(9)+1
   ENDIF
   IF(SIMGRO(ICOL,IROW)%NOPP.EQ.NODATA(10))          IERROR(10)=IERROR(10)+1
   IF(SIMGRO(ICOL,IROW)%SOPP.EQ.NODATA(11))          IERROR(11)=IERROR(11)+1
   IF(SIMGRO(ICOL,IROW)%VXMU_ROPP.EQ.NODATA(13))     IERROR(13)=IERROR(13)+1
   IF(SIMGRO(ICOL,IROW)%CRUNOFF_SOPP.EQ.NODATA(14))  IERROR(14)=IERROR(14)+1
   IF(SIMGRO(ICOL,IROW)%SOPP.GT.0)THEN
    IF(SIMGRO(ICOL,IROW)%VXMU_SOPP.EQ.NODATA(12))     IERROR(12)=IERROR(12)+1
    IF(SIMGRO(ICOL,IROW)%CRUNON_SOPP.EQ.NODATA(16))   IERROR(16)=IERROR(16)+1
    IF(SIMGRO(ICOL,IROW)%QINFBASIC_SOPP.EQ.NODATA(18))IERROR(18)=IERROR(18)+1
   ENDIF
   IF(SIMGRO(ICOL,IROW)%CRUNOFF_ROPP.EQ.NODATA(15))  IERROR(15)=IERROR(15)+1
   IF(SIMGRO(ICOL,IROW)%CRUNON_ROPP.EQ.NODATA(17))   IERROR(17)=IERROR(17)+1
   IF(SIMGRO(ICOL,IROW)%QINFBASIC_ROPP.EQ.NODATA(19))IERROR(19)=IERROR(19)+1
   IF(LPWT)THEN
!    IF(SIMGRO(ICOL,IROW)%PWT_LEVEL.EQ.NODATA(20))  IERROR(20)=IERROR(20)+1 <--- nodata is niet erg, is er geen PWT aanwezig
   ENDIF
   IF(SIMGRO(ICOL,IROW)%MOISTURE.EQ.NODATA(21))      IERROR(21)=IERROR(21)+1
   IF(SIMGRO(ICOL,IROW)%COND.EQ.NODATA(22))          IERROR(22)=IERROR(22)+1
  ENDIF
 ENDDO; ENDDO

 !## error in data
 IF(SUM(IERROR).GT.0)THEN
   STRLEN=22*30; ALLOCATE(CHARACTER(LEN=STRLEN) :: STR)
   STR='NodataValues on active modelcells found in :'//NEWLINE// &
   '- Landuse           '//TRIM(ITOS(IERROR(2)))//NEWLINE// &
   '- Rootzone          '//TRIM(ITOS(IERROR(3)))//NEWLINE// &
   '- Soil Types        '//TRIM(ITOS(IERROR(4)))//NEWLINE// &
   '- Meteo Stations    '//TRIM(ITOS(IERROR(5)))//NEWLINE// &
   '- Surface Level     '//TRIM(ITOS(IERROR(6)))//NEWLINE// &
   '- Art. Recharge     '//TRIM(ITOS(IERROR(7)))//NEWLINE// &
   '- Art. Rch. Layer   '//TRIM(ITOS(IERROR(8)))//NEWLINE// &
   '- Art. Rch. Strength'//TRIM(ITOS(IERROR(9)))//NEWLINE// &
   '- Wetted Area       '//TRIM(ITOS(IERROR(10)))//NEWLINE// &
   '- Surf. Urban Area  '//TRIM(ITOS(IERROR(11)))//NEWLINE// &
   '- VXMU SOPP         '//TRIM(ITOS(IERROR(12)))//NEWLINE// &
   '- VXMU ROPP         '//TRIM(ITOS(IERROR(13)))//NEWLINE// &
   '- CRUNOFF SOPP      '//TRIM(ITOS(IERROR(14)))//NEWLINE// &
   '- CRUNOFF ROPP      '//TRIM(ITOS(IERROR(15)))//NEWLINE// &
   '- CRUNON SOPP       '//TRIM(ITOS(IERROR(16)))//NEWLINE// &
   '- CRUNON ROPP       '//TRIM(ITOS(IERROR(17)))//NEWLINE// &
   '- QINFBASIS SOPP    '//TRIM(ITOS(IERROR(18)))//NEWLINE// &
   '- QINFBASIS ROPP    '//TRIM(ITOS(IERROR(19)))//NEWLINE// &
!   '- Pondingdepth      '//TRIM(ITOS(IERROR(12))),1)
!!  IF(LPWT)CALL PRINTTEXT('- PWT Level         '//TRIM(ITOS(IERROR(20))),1)
   '- Moisture Factor   '//TRIM(ITOS(IERROR(21)))//NEWLINE// &
   '- Conductivity      '//TRIM(ITOS(IERROR(22)))//NEWLINE// &
   'Process stopped!'
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(STR),'Error')
  DEALLOCATE(STR,IERROR); RETURN
 ENDIF

 !## change surface water into gras; change urban into gras
 DO IROW=1,PRJIDF%NROW
  DO ICOL=1,PRJIDF%NCOL
   SELECT CASE (SIMGRO(ICOL,IROW)%LGN)
    CASE (8,18:21,23:26)
    SIMGRO(ICOL,IROW)%LGN=1
   CASE (22)
    SIMGRO(ICOL,IROW)%LGN=12
   CASE (:0,45:)
    SIMGRO(ICOL,IROW)%LGN=1
   END SELECT
  ENDDO
 ENDDO
  
 !## minimale beworteling
 DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
  IF(SIMGRO(ICOL,IROW)%RZ.LT.10.0D0)SIMGRO(ICOL,IROW)%RZ=10.0D0
 ENDDO; ENDDO
 
 !## minimal nopp-value
 DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
  SIMGRO(ICOL,IROW)%NOPP=MAX(0.0D0,SIMGRO(ICOL,IROW)%NOPP)
  !## minimal sopp-value
  SIMGRO(ICOL,IROW)%SOPP=MAX(0.0D0,SIMGRO(ICOL,IROW)%SOPP)
 ENDDO; ENDDO
 
 !## bodem 22/23 vertalen naar 9 -> 22 (stedelijk zand?)/23(geen bodem; stad) -> zand
 DO IROW=1,PRJIDF%NROW
  DO ICOL=1,PRJIDF%NCOL
   SELECT CASE (SIMGRO(ICOL,IROW)%BODEM)
    CASE (23,22)
     SIMGRO(ICOL,IROW)%BODEM=9
   END SELECT
   !## kies bodem 22 for lgn stedelijk gebied
   SELECT CASE (SIMGRO(ICOL,IROW)%LGN)
    CASE (18,25)
!     SIMGRO(ICOL,IROW)%BODEM=22
   END SELECT
  ENDDO
 ENDDO

 IF(IARMWP.EQ.0)THEN

  !## turn off beregening whenever layer is zero!
  DO IROW=1,PRJIDF%NROW
   DO ICOL=1,PRJIDF%NCOL
    !## maximal artificial recharge layer is PRJNLAY
    SIMGRO(ICOL,IROW)%BER_LAAG=MIN(SIMGRO(ICOL,IROW)%BER_LAAG,PRJNLAY)
    IF(SIMGRO(ICOL,IROW)%BEREGEN.NE.0.AND.SIMGRO(ICOL,IROW)%BER_LAAG.EQ.0)SIMGRO(ICOL,IROW)%BEREGEN=0
   ENDDO
  ENDDO
 ENDIF
 
 DEALLOCATE(IERROR)
 
 END SUBROUTINE PMANAGER_SAVEMF2005_MSP_CHECK
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_COMBINE(DIR,DIRNAME,PCK,CB,CAUX)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRNAME,CAUX
 INTEGER,INTENT(IN) :: CB
 CHARACTER(LEN=*),INTENT(IN),DIMENSION(3) :: PCK
 INTEGER,DIMENSION(3) :: IU
 INTEGER,DIMENSION(3) :: JU,NO,NO_PREV
 CHARACTER(LEN=256),DIMENSION(3) :: FNAME,FNAME_PREV
 INTEGER :: I,J,IPER
 LOGICAL :: LEX
 
 PMANAGER_SAVEMF2005_COMBINE=.FALSE.
  
 !## read from files
 IU=0
 DO I=1,SIZE(PCK)
  LINE=TRIM(DIRNAME)//'.'//TRIM(PCK(I))//'7'
  IF(I.LE.2)THEN
   IU(I)=UTL_GETUNIT(); CALL OSD_OPEN(IU(I),FILE=LINE,STATUS='OLD',ACTION='READ')
  ELSE
   !## write to file
   IU(I)=UTL_GETUNIT(); CALL OSD_OPEN(IU(I),FILE=LINE,STATUS='UNKNOWN',ACTION='WRITE')
  ENDIF
 ENDDO
 IF(MINVAL(IU).EQ.0)RETURN

 NO=0; DO I=1,2; READ(IU(I),*) NO(I); ENDDO
 LINE=TRIM(ITOS(SUM(NO)))//','//TRIM(ITOS(CB))//' '//TRIM(CAUX)
 WRITE(IU(3),'(A)') TRIM(LINE)
  
 DO IPER=1,PRJNPER
  
  NO=0; DO I=1,2; READ(IU(I),*) NO(I); ENDDO
  !## use previous timestep for both
  IF(NO(1).EQ.-1.AND.NO(2).EQ.-1)THEN
   WRITE(IU(3),'(I2)') -1; CYCLE
  ENDIF 

  FNAME=''

  !## reuse previous values
  DO I=1,2
   IF(NO(I).LT.0)THEN; NO(I)=NO_PREV(I); FNAME(I)=FNAME_PREV(I); ENDIF
  ENDDO
  
  LINE=TRIM(ITOS(SUM(NO)))
  WRITE(IU(3),'(A)') TRIM(LINE)
  
  JU=0
  DO I=1,2
   !## refresh external filename
   IF(NO(I).GT.0)THEN
    IF(LEN_TRIM(FNAME(I)).EQ.0)THEN
     READ(IU(I),'(11X,A)') FNAME(I)
     FNAME(I)=UTL_CAP(FNAME(I),'U')
     J=INDEX(FNAME(I),'.ARR',.TRUE.)-1
     FNAME(I)=DIR(:INDEX(DIR,'\',.TRUE.)-1)//TRIM(FNAME(I)(2:J))//'.ARR'
     FNAME(I)=UTL_CAP(FNAME(I),'U')
    ENDIF
    JU(I)=UTL_GETUNIT(); CALL OSD_OPEN(JU(I),FILE=FNAME(I),STATUS='OLD',ACTION='READ')
   ENDIF
  ENDDO

  !## create (new) output file
  FNAME(3)=TRIM(DIR)//'\'//  TRIM(PCK(2))//'7\'//TRIM(PCK(2))//'_T'//TRIM(ITOS(IPER))//'.ARR'
  FNAME(3)=UTL_CAP(FNAME(3),'U')

  !## append to existing file, create new file otherwise
  JU(3)=UTL_GETUNIT()
  IF(FNAME(3).EQ.FNAME(2))THEN; FNAME(3)=TRIM(FNAME(3))//'_'; ENDIF
  CALL OSD_OPEN(JU(3),FILE=FNAME(3),STATUS='UNKNOWN',ACTION='WRITE')

  IF(JU(1).GT.0)THEN; DO I=1,NO(1); READ(JU(1),'(A256)') LINE; WRITE(JU(3),'(A)') TRIM(LINE); ENDDO; CLOSE(JU(1)); ENDIF
  IF(JU(2).GT.0)THEN; DO I=1,NO(2); READ(JU(2),'(A256)') LINE; WRITE(JU(3),'(A)') TRIM(LINE); ENDDO; CLOSE(JU(2)); ENDIF

  !## add iMOD header at the bottom
  IF(PBMAN%IFORMAT.EQ.2)CALL IDFWRITEFREE_HEADER(JU(3),BND(1))

  CLOSE(JU(3))
  
  J=LEN_TRIM(FNAME(3))
  IF(FNAME(3)(J:J).EQ.'_')THEN
   FNAME(3)(J:J)=' '
   INQUIRE(FILE=FNAME(3),EXIST=LEX); IF(LEX)CALL IOSDELETEFILE(FNAME(3))
   CALL IOSRENAMEFILE(TRIM(FNAME(3))//'_',FNAME(3))
  ENDIF
  
  LINE=FNAME(3); DO J=1,3; LINE=LINE(:INDEX(LINE,'\',.TRUE.)-1); ENDDO
  J=LEN_TRIM(LINE); LINE='.'//FNAME(3)(J+1:)

  IF(SUM(NO).GT.0)WRITE(IU(3),'(A)') 'OPEN/CLOSE '//TRIM(LINE)//' 1.0D0 (FREE) -1'
    
  DO I=1,2; NO_PREV(I)=NO(I); FNAME_PREV(I)=FNAME(I); ENDDO

 ENDDO
 
 CLOSE(IU(1),STATUS='DELETE'); CLOSE(IU(2),STATUS='DELETE'); CLOSE(IU(3))
 
 !## rename file
 FNAME(1)=TRIM(DIRNAME)//'.'//TRIM(PCK(3))//'7'
 FNAME(2)=TRIM(DIRNAME)//'.'//TRIM(PCK(2))//'7'
 CALL IOSRENAMEFILE(FNAME(1),FNAME(2))
 
 PMANAGER_SAVEMF2005_COMBINE=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_COMBINE

 !###======================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_PCK_ULSTRD_PARAM(PRJNLAY,ICOL,IROW,BND,TOP,BOT,KD,TP,BT,KH,LKHV)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: PRJNLAY,ICOL,IROW
 TYPE(IDFOBJ),INTENT(IN),DIMENSION(PRJNLAY) :: BND,TOP,BOT,KD
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(PRJNLAY) :: KH,TP,BT
 LOGICAL,INTENT(IN) :: LKHV
 INTEGER :: ILAY
  
 !## get filter fractions
 DO ILAY=1,PRJNLAY
  TP(ILAY)=TOP(ILAY)%X(ICOL,IROW)
  BT(ILAY)=BOT(ILAY)%X(ICOL,IROW)
  KH(ILAY)=KD (ILAY)%X(ICOL,IROW)
 ENDDO
 DO ILAY=1,PRJNLAY
  !## do not put any in constant or inactive cells
  IF(BND(ILAY)%X(ICOL,IROW).GT.0.AND.TP(ILAY)-BT(ILAY).GT.0.0D0)THEN
   KH(ILAY)=KH(ILAY)/(TP(ILAY)-BT(ILAY))
   !## uniform disctribution
   IF(.NOT.LKHV)KH(ILAY)=1.0D0
  ELSE
   KH(ILAY)=0.0D0
  ENDIF
 ENDDO

 END SUBROUTINE PMANAGER_SAVEMF2005_PCK_ULSTRD_PARAM
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_PCK_U2DREL(EXFNAME,IDF,IU,IFBND,IINT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFBND,IINT
 CHARACTER(LEN=*),INTENT(IN) :: EXFNAME
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 CHARACTER(LEN=256) :: SFNAME
 INTEGER,INTENT(IN) :: IU
 INTEGER :: JU,IROW,ICOL,I
 REAL(KIND=DP_KIND) :: MINV,MAXV
 
 PMANAGER_SAVEMF2005_PCK_U2DREL=.FALSE.

 IF(.NOT.PMANAGER_SAVEMF2005_PCK_GETMINMAX(IDF%X,IDF%NCOL,IDF%NROW,BND(1)%X,MINV,MAXV,IFBND))RETURN
 
 !## constant value
 IF(MAXV.EQ.MINV)THEN

  IF(IINT.EQ.0)WRITE(IU,'(A)') 'CONSTANT '//TRIM(RTOS(MAXV,'E',7))
  IF(IINT.EQ.1)THEN
   LINE='CONSTANT '//TRIM(ITOS(INT(MAXV)))
   WRITE(IU,'(A)') TRIM(LINE)
  ENDIF

 ELSE
  
  CALL UTL_CREATEDIR(EXFNAME(:INDEX(EXFNAME,'\',.TRUE.)-1))
  SFNAME=EXFNAME; DO I=1,3; SFNAME=SFNAME(:INDEX(SFNAME,'\',.TRUE.)-1); ENDDO
  I=LEN_TRIM(SFNAME); SFNAME='.'//EXFNAME(I+1:)
 
  IF(IINT.EQ.0)WRITE(IU,'(A)') 'OPEN/CLOSE '//TRIM(SFNAME)//' 1.0D0 (FREE) -1'
  IF(IINT.EQ.1)WRITE(IU,'(A)') 'OPEN/CLOSE '//TRIM(SFNAME)//' 1   (FREE) -1'
 
  JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=EXFNAME,STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(JU.EQ.0)RETURN

  IF(LFREEFORMAT)THEN
   CALL IDFWRITEFREE(JU,IDF,IINT,'B','*') 
  ELSE
   IF(IINT.EQ.1)THEN
    DO IROW=1,IDF%NROW; WRITE(JU,*) (INT(IDF%X(ICOL,IROW)),ICOL=1,IDF%NCOL); ENDDO
   ELSE
    DO IROW=1,IDF%NROW; WRITE(JU,*)     (IDF%X(ICOL,IROW) ,ICOL=1,IDF%NCOL); ENDDO
   ENDIF
  ENDIF
  
  CLOSE(JU)

 ENDIF
 
 PMANAGER_SAVEMF2005_PCK_U2DREL=.TRUE.
   
 END FUNCTION PMANAGER_SAVEMF2005_PCK_U2DREL
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_HFB_COMPUTE(IDF,ITOPIC,IU,BND,TOP,BOT,IPRT,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITOPIC,IU,IPRT,IBATCH
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 TYPE(IDFOBJ),DIMENSION(PRJNLAY),INTENT(INOUT) :: TOP,BOT,BND
 REAL(KIND=DP_KIND) :: FCT,IMP,CNST
 INTEGER :: ILAY,ISYS,ICNST
 INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:,:) :: IPC
 TYPE(IDFOBJ) :: TIDF,BIDF
 
 PMANAGER_SAVEMF2005_HFB_COMPUTE=.FALSE.
 
 CALL ASC2IDF_INT_NULLIFY(); ALLOCATE(XP(100),YP(100),ZP(100),FP(100),WP(100))
 
 !## compute block-faces
 ALLOCATE(IPC(IDF%NCOL,IDF%NROW,2))
 CALL IDFNULLIFY(TIDF); CALL IDFNULLIFY(BIDF)
 CALL IDFCOPY(IDF,TIDF); CALL IDFCOPY(IDF,BIDF) 

 WRITE(IU,'(5A10,2A15,A10,4A15)') 'ILAY','IROW1','ICOL1','IROW2','ICOL2','RESISTANCE','FRACTION','SYSTEM', &
      'TOP_LAYER','BOT_LAYER','TOP_FAULT','BOT_FAULT'

 !## process per system
 DO ISYS=1,SIZE(TOPICS(ITOPIC)%STRESS(1)%FILES,2)
   
  IPC=INT(0,1)
   
  ICNST    =TOPICS(ITOPIC)%STRESS(1)%FILES(1,ISYS)%ICNST
  CNST     =TOPICS(ITOPIC)%STRESS(1)%FILES(1,ISYS)%CNST
  ILAY     =TOPICS(ITOPIC)%STRESS(1)%FILES(1,ISYS)%ILAY
  FCT      =TOPICS(ITOPIC)%STRESS(1)%FILES(1,ISYS)%FCT
  IMP      =TOPICS(ITOPIC)%STRESS(1)%FILES(1,ISYS)%IMP
  IDF%FNAME=TOPICS(ITOPIC)%STRESS(1)%FILES(1,ISYS)%FNAME

  IF(ICNST.EQ.1)THEN
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'HFB cannot be parameterized via a constant value.','Error')
   WRITE(*,'(A)') 'HFB cannot be parameterized via a constant value.'
   EXIT
  ENDIF
  
  WRITE(IPRT,'(1X,A,I3,A1,I1,A1,I4.3,3(A1,G15.7),A1,A)') TOPICS(ITOPIC)%TNAME(1:5)//',', &
     ISYS,',',ICNST,',',ILAY,',',FCT,',',IMP,',',CNST,',',CHAR(39)//TRIM(IDF%FNAME)//CHAR(39)

  IF(LEN_TRIM(PRJIDF%FNAME).GT.0)THEN
   !## rasterize genfile
   CALL ASC2IDF_HFB(IDF,IDF%NROW,IDF%NCOL,IPC,(/IDF%FNAME/),ILAY,TIDF,BIDF)
   !## collect all fault in a single file with resistances and layer fractions
   CALL PMANAGER_SAVEMF2005_HFB_COLLECT(IPC,IDF%NROW,IDF%NCOL,FCT*IMP,IU,BND,TOP,BOT,ILAY,TIDF,BIDF,ISYS)
  ENDIF
   
 ENDDO

 CALL ASC2IDF_INT_DEALLOCATE(); CLOSE(IU)
 DEALLOCATE(IPC); CALL IDFDEALLOCATEX(TIDF); CALL IDFDEALLOCATEX(BIDF) 

 IF(ISYS.GT.SIZE(TOPICS(ITOPIC)%STRESS(1)%FILES,2))PMANAGER_SAVEMF2005_HFB_COMPUTE=.TRUE.
  
 END FUNCTION PMANAGER_SAVEMF2005_HFB_COMPUTE
 
 !###====================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_HFB_COLLECT(IPC,NROW,NCOL,HFBRESIS, &
               IU,BND,TOP,BOT,ITB,TIDF,BIDF,ISYS) 
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL,IU,ITB,ISYS
 TYPE(IDFOBJ),INTENT(INOUT) :: TIDF,BIDF
 TYPE(IDFOBJ),DIMENSION(PRJNLAY),INTENT(INOUT) :: TOP,BOT,BND
 REAL(KIND=DP_KIND),INTENT(IN) :: HFBRESIS
 INTEGER(KIND=1),INTENT(IN),DIMENSION(NCOL,NROW,2) :: IPC
 INTEGER :: IROW,ICOL,IL1,IL2,ILAY 
 REAL(KIND=DP_KIND) :: NODATA,FDZ,TPV,BTV,TFV,BFV
 
 NODATA=HUGE(1.0D0)
 
 !## determine what layer(s)
 IF(ITB.EQ.0)THEN
  IL1=1; IL2=PRJNLAY
 ELSE
  IL1=ITB; IL2=IL1
 ENDIF

 DO IROW=1,NROW; DO ICOL=1,NCOL; DO ILAY=IL1,IL2

  !## place vertical wall
  IF(IPC(ICOL,IROW,1).EQ.INT(1,1))THEN
   IF(ICOL.LT.NCOL)THEN
 
    !## fraction is minus 1 for given layers
    FDZ=-1.0D0
    IF(ITB.EQ.0)FDZ=PMANAGER_SAVEMF2005_HFB_GETFDZ(BND,TOP,BOT,TIDF%X,BIDF%X,ICOL,IROW,ICOL+1,IROW,NODATA,ILAY,TFV,BFV)

    !## enter fault if occupation > 0.0D0%
    IF(ITB.EQ.0.AND.FDZ.LE.0.0D0)CYCLE
    
    IF(ITB.NE.0)THEN
     TPV=0.0D0
     BTV=0.0D0
     TFV=0.0D0
     BFV=0.0D0
    ELSE
     TPV=(TOP(ILAY)%X(ICOL,IROW)+TOP(ILAY)%X(ICOL+1,IROW))/2.0D0
     BTV=(BOT(ILAY)%X(ICOL,IROW)+BOT(ILAY)%X(ICOL+1,IROW))/2.0D0 
    ENDIF
 
    !## write fault always, as it becomes confused 
    WRITE(IU,'(5I10,2G15.7,I10,4G15.7)') ILAY,IROW,ICOL,IROW,ICOL+1,HFBRESIS,FDZ,ISYS,TPV,BTV,TFV,BFV !## x-direction
    
   ENDIF
  ENDIF

  !## place horizontal wall
  IF(IPC(ICOL,IROW,2).EQ.INT(1,1))THEN
   IF(IROW.LT.NROW)THEN
   
    !## fraction is minus 1 for given layers
    FDZ=-1.0D0
    IF(ITB.EQ.0)FDZ=PMANAGER_SAVEMF2005_HFB_GETFDZ(BND,TOP,BOT,TIDF%X,BIDF%X,ICOL,IROW,ICOL,IROW+1,NODATA,ILAY,TFV,BFV)

    !## enter fault if occupation > 0.0D0%
    IF(ITB.EQ.0.AND.FDZ.LE.0.0D0)CYCLE

    IF(ITB.NE.0)THEN
     TPV=0.0D0
     BTV=0.0D0
     TFV=0.0D0
     BFV=0.0D0
    ELSE
     TPV=(TOP(ILAY)%X(ICOL,IROW)+TOP(ILAY)%X(ICOL,IROW+1))/2.0D0
     BTV=(BOT(ILAY)%X(ICOL,IROW)+BOT(ILAY)%X(ICOL,IROW+1))/2.0D0
    ENDIF

    !## write fault always, as it becomes confused 
    WRITE(IU,'(5I10,2G15.7,I10,4G15.7)') ILAY,IROW,ICOL,IROW+1,ICOL,HFBRESIS,FDZ,ISYS,TPV,BTV,TFV,BFV !## y-direction

   ENDIF
  ENDIF
 
 ENDDO; ENDDO; ENDDO

 END SUBROUTINE PMANAGER_SAVEMF2005_HFB_COLLECT
 
 !###====================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_HFB_EXPORT(NHFBNP,IU,JU,IUGEN,IUDAT,IDF,LTB)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: THICKNESS=0.5D0
 LOGICAL,INTENT(IN) :: LTB
 INTEGER,INTENT(IN) :: IU,JU
 INTEGER,INTENT(IN),DIMENSION(:) :: IUGEN,IUDAT
 INTEGER,INTENT(INOUT),DIMENSION(:) :: NHFBNP
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: IROW,ICOL,ILAY,IOS,JLAY,IC1,IC2,IR1,IR2,ISYS
 REAL(KIND=DP_KIND) :: C,C1,C2,Z,ZZ,TPV,BTV,TFV,BFV
 INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:,:) :: IPC
 INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:) :: SYS
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: RES,FDZ,TF,BF
 LOGICAL :: LINV
 
 !## compute block-faces
 ALLOCATE(IPC(IDF%NCOL,IDF%NROW,2))
 ALLOCATE(RES(IDF%NCOL,IDF%NROW))
 ALLOCATE(FDZ(IDF%NCOL,IDF%NROW))
 ALLOCATE(SYS(IDF%NCOL,IDF%NROW))
 ALLOCATE(TF(IDF%NCOL,IDF%NROW))
 ALLOCATE(BF(IDF%NCOL,IDF%NROW))
 
 !## process each layer
 DO ILAY=1,PRJNLAY
  
  IPC=INT(0,1)
  RES=0.0D0
  FDZ=0.0D0
  SYS=INT(0,1)
  TF=-10.0D10
  BF= 10.0D10
  LINV=.FALSE.
  
  READ(JU,*)
  DO
   !## z=fraction (-1=confined system), c=resistance
   READ(JU,'(5I10,2G15.7,I10,4G15.7)',IOSTAT=IOS) JLAY,IR1,IC1,IR2,IC2,C,Z,ISYS,TPV,BTV,TFV,BFV

   IF(IOS.NE.0)EXIT
   IF(JLAY.NE.ILAY)CYCLE

   !## skip c.lt.zero
   IF(C.LT.0.0D0)CYCLE
   IF(IC1.EQ.IC2)THEN
    IPC(IC1,IR1,2)=INT(1,1)
   ELSE
    IPC(IC1,IR1,1)=INT(1,1)
   ENDIF
   IF(Z.GT.0.0D0)LINV=.TRUE.
   
   !## still some space left in modellayer for an additional fault
   IF(Z.LT.0.0D0.OR.FDZ(IC1,IR1).LT.1.0D0)THEN
    !## available space
    ZZ=1.0D0-FDZ(IC1,IR1)
    !## net available space
    ZZ=MIN(ZZ,Z)
    !## confined system
    IF(Z.LT.0.0D0)ZZ=1.0D0
    !## take system number of largest contribution to c
    IF(RES(IC1,IR1).GT.0.0D0)THEN
     IF(Z.GT.0.0D0)THEN
      !## currently available resistance
      C2=1.0D0/RES(IC1,IR1)*FDZ(IC1,IR1)
      IF(C.GT.C2)SYS(IC1,IR1)=INT(ISYS,1)
     ELSE
      IF(C.GT.RES(IC1,IR1))SYS(IC1,IR1)=INT(ISYS,1)
     ENDIF
    ELSE
     SYS(IC1,IR1)=INT(ISYS,1)
    ENDIF
    !## resistance, sum conductances - ignore resistance of zero days
    IF(Z.GT.0.0D0)THEN
     !## add small fault using arithmetic mean
     IF(TPV-BTV.LE.THICKNESS)THEN
      C1=0.0D0; IF(RES(IC1,IR1).GT.0.0D0)C1=1.0D0/RES(IC1,IR1)*FDZ(IC1,IR2)
      C2=C*ZZ
      !## set conductance
      RES(IC1,IR1)=1.0D0/((C1+C2)/(ZZ+FDZ(IC1,IR2)))
     !## add large fault using harmonic mean
     ELSE
      !## set conductance
      RES(IC1,IR1)=RES(IC1,IR1)+(1.0D0/C)*ZZ
     ENDIF
    ELSE
     !## get largest resistance
     RES(IC1,IR1)=MAX(RES(IC1,IR1),C)
    ENDIF
    !## occupation fraction
    FDZ(IC1,IR1)=MIN(1.0D0,FDZ(IC1,IR1)+ABS(Z))
    
    !## maximum top fault for display
    TF(IC1,IR1)=MAX(TF(IC1,IR1),TF(IC2,IR2),TFV)
    !## minimum bot fault for display
    BF(IC1,IR1)=MIN(BF(IC1,IR1),BF(IC2,IR2),BFV)
    
   ENDIF
   
  ENDDO
  
  DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL

   !## place vertical wall (block in y-direction)
   IF(IPC(ICOL,IROW,1).EQ.INT(1,1))THEN
    IF(ICOL.LT.IDF%NCOL)THEN

     !## skip faults from and/or towards inactive cell
     IF(BND(ILAY)%X(ICOL,IROW).EQ.0.0D0.OR. &
        BND(ILAY)%X(ICOL+1,IROW).EQ.0.0D0)CYCLE
    
     !## transform conductances to resistance - take into account the occupation fraction
     IF(LINV)THEN
      C1=1.0D0/RES(ICOL,IROW)*FDZ(ICOL,IROW)
     ELSE
      C1=RES(ICOL,IROW)
     ENDIF

     !## get total resistance related to thickness of model layer
     IF(FDZ(ICOL,IROW).LT.1.0D0)THEN
      !## take harmonic mean in case of unsaturated thickness of fault 
      C2=1.0D0/((1.0D0/C1*FDZ(ICOL,IROW))+(1.0D0-FDZ(ICOL,IROW)))
     ELSE
      C2=C1
     ENDIF

     !## get systemnumber
     ISYS=SYS(ICOL,IROW)
     !## top fault for display purposes
     TFV=TF(ICOL,IROW)
     !## bottom fault for display purposes
     BFV=BF(ICOL,IROW)
     
     !## add fault
     NHFBNP(ILAY)=NHFBNP(ILAY)+1
     
     IF(PBMAN%IFORMAT.EQ.2)THEN
      WRITE(IU,'(5(I10,1X),G15.7,1X,I10)') ILAY,IROW,ICOL,     IROW,ICOL+1,      C2,ISYS !## y-direction
     ELSE
      WRITE(IU,'(6(I10,1X),G15.7,1X,I10)') ILAY,IROW,ICOL,ILAY,IROW,ICOL+1,      C2,ISYS !## y-direction
     ENDIF
     !## write line in genfile
     CALL PMANAGER_SAVEMF2005_HFB_GENFILES(IUGEN(ILAY),IUDAT(ILAY),IPC,IDF,IDF%NROW,IDF%NCOL,IROW,ICOL, &
               NHFBNP(ILAY),C1,C2,FDZ(ICOL,IROW),ISYS,1,LTB,TFV,BFV) 

    ENDIF 
   ENDIF

   !## place horizontal wall (block in x-direction)
   IF(IPC(ICOL,IROW,2).EQ.INT(1,1))THEN
    IF(IROW.LT.IDF%NROW)THEN

     !## skip faults from and/or towards inactive cell
     IF(BND(ILAY)%X(ICOL,IROW).EQ.0.0D0.OR. &
        BND(ILAY)%X(ICOL,IROW+1).EQ.0.0D0)CYCLE

     !## transform conductances to resistance
     IF(LINV)THEN
      C1=1.0D0/RES(ICOL,IROW)*FDZ(ICOL,IROW)
     ELSE
      C1=RES(ICOL,IROW)
     ENDIF

     !## get total resistance related to thickness of model layer
     IF(FDZ(ICOL,IROW).LT.1.0D0)THEN
      !## take harmonic mean in case of unsaturated thickness of fault 
      C2=1.0D0/((1.0D0/C1*FDZ(ICOL,IROW))+(1.0D0-FDZ(ICOL,IROW)))
     ELSE
      C2=C1
     ENDIF
           
     !## get systemnumber
     ISYS=SYS(ICOL,IROW)

     !## top fault for display purposes
     TFV=TF(ICOL,IROW)
     !## bottom fault for display purposes
     BFV=BF(ICOL,IROW)

     !## add fault
     NHFBNP(ILAY)=NHFBNP(ILAY)+1
     
     IF(PBMAN%IFORMAT.EQ.2)THEN
      WRITE(IU,'(5(I10,1X),G15.7,1X,I10)') ILAY,IROW,ICOL,     IROW+1,ICOL,      C2,ISYS !## x-direction
     ELSE
      WRITE(IU,'(6(I10,1X),G15.7,1X,I10)') ILAY,IROW,ICOL,ILAY,IROW+1,ICOL,      C2,ISYS !## x-direction
     ENDIF
     
     !## write line in genfile
     CALL PMANAGER_SAVEMF2005_HFB_GENFILES(IUGEN(ILAY),IUDAT(ILAY),IPC,IDF,IDF%NROW,IDF%NCOL,IROW,ICOL, &
               NHFBNP(ILAY),C1,C2,FDZ(ICOL,IROW),ISYS,2,LTB,TFV,BFV) 

    ENDIF
   ENDIF
 
  ENDDO; ENDDO
  WRITE(IUGEN(ILAY),'(A)') 'END'
  REWIND(JU)
 ENDDO
 
 DEALLOCATE(IPC,RES,FDZ,SYS,TF,BF)
 
 END SUBROUTINE PMANAGER_SAVEMF2005_HFB_EXPORT
 
 !###====================================================================
 REAL(KIND=DP_KIND) FUNCTION PMANAGER_SAVEMF2005_HFB_GETFDZ(BND,TOP,BOT,TF,BF,IC1,IR1,IC2,IR2,NODATA,ILAY,TFV,BFV)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),DIMENSION(PRJNLAY),INTENT(INOUT) :: TOP,BOT,BND
 REAL(KIND=DP_KIND),INTENT(IN) :: NODATA
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(:,:) :: TF,BF
 REAL(KIND=DP_KIND),INTENT(OUT) :: TFV,BFV
 INTEGER,INTENT(IN) :: IC1,IR1,IC2,IR2,ILAY
 REAL(KIND=DP_KIND) :: TPV,BTV,FDZ 

 PMANAGER_SAVEMF2005_HFB_GETFDZ=0.0D0

 !## determine values
 IF(TF(IC1,IR1).NE.NODATA.AND.TF(IC2,IR2).NE.NODATA)THEN
  TFV=(TF(IC1,IR1)+TF(IC2,IR2))/2.0D0
 ELSEIF(TF(IC1,IR1).NE.NODATA)THEN
  TFV=TF(IC1,IR1)
 ELSE
  TFV=TF(IC2,IR2)
 ENDIF
 IF(BF(IC1,IR1).NE.NODATA.AND.BF(IC2,IR2).NE.NODATA)THEN
  BFV=(BF(IC1,IR1)+BF(IC2,IR2))/2.0D0
 ELSEIF(BF(IC1,IR1).NE.NODATA)THEN
  BFV=BF(IC1,IR1)
 ELSE
  BFV=BF(IC2,IR2)
 ENDIF

 !## skip this fault as it enteres nodata
 IF(BND(ILAY)%X(IC1,IR1).EQ.0.OR.BND(ILAY)%X(IC2,IR2).EQ.0)RETURN
 
 TPV=(TOP(ILAY)%X(IC1,IR1)+TOP(ILAY)%X(IC2,IR2))/2.0D0
 BTV=(BOT(ILAY)%X(IC1,IR1)+BOT(ILAY)%X(IC2,IR2))/2.0D0

 !## nett appearance of fault in modellayer
 FDZ=MIN(TFV,TPV)-MAX(BFV,BTV)

 !## not in current modellayer
 IF(FDZ.LT.0.0D0)RETURN

 IF(TPV-BTV.GT.0.0D0)THEN
  !## fraction of fault in modellayer
  FDZ=FDZ/(TPV-BTV)
 ELSE
  !## completely filled in model layer with thickness of zero
  FDZ=1.0D0
 ENDIF

 !## fraction of layer occupation
 PMANAGER_SAVEMF2005_HFB_GETFDZ=FDZ

 END FUNCTION PMANAGER_SAVEMF2005_HFB_GETFDZ
 
 !###====================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_HFB_GENFILES(IU,JU,IPC,IDF,NROW,NCOL,IROW,ICOL,N, &
                  C,RES,FDZ,ISYS,IT,LTB,TFV,BFV)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 REAL(KIND=DP_KIND),INTENT(IN) :: C,RES,FDZ,TFV,BFV
 LOGICAL,INTENT(IN) :: LTB
 INTEGER,INTENT(IN) :: NROW,NCOL,IROW,ICOL,IU,JU,N,ISYS,IT
 INTEGER(KIND=1),INTENT(IN),DIMENSION(NCOL,NROW,2) :: IPC 
 REAL(KIND=DP_KIND) :: T1,B1

 !## place vertical wall
 IF(IT.EQ.1)THEN
  IF(IPC(ICOL,IROW,1).EQ.INT(1,1).AND.ICOL.LT.NCOL)THEN
   IF(JU.GT.0)THEN
    IF(LTB)THEN
     IF(TFV.GE.BFV)WRITE(JU,'(I10,3(1X,E15.7),I10)') N,C,RES,FDZ,ISYS
    ELSE
     WRITE(JU,'(I10,1X  ,E15.7 ,I10)') N,C,ISYS
    ENDIF
   ENDIF
   IF(ICOL.LT.PRJIDF%NCOL)THEN
    IF(LTB)THEN
     IF(TFV.GE.BFV)THEN
      T1=TFV; B1=BFV
      WRITE(IU,'(I10)') N 
      WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL),',',IDF%SY(IROW-1),',',T1
      WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL),',',IDF%SY(IROW)  ,',',T1
      WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL),',',IDF%SY(IROW)  ,',',B1
      WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL),',',IDF%SY(IROW-1),',',B1
      WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL),',',IDF%SY(IROW-1),',',T1
      WRITE(IU,'(A)') 'END'
     ENDIF
    ELSE
     WRITE(IU,'(I10)') N 
     WRITE(IU,'(2(F15.3,A1))') IDF%SX(ICOL),',',IDF%SY(IROW-1)
     WRITE(IU,'(2(F15.3,A1))') IDF%SX(ICOL),',',IDF%SY(IROW)
     WRITE(IU,'(A)') 'END'
    ENDIF
   ENDIF
  ENDIF
 ENDIF
 
 !## place horizontal wall
 IF(IT.EQ.2)THEN
  IF(IPC(ICOL,IROW,2).EQ.INT(1,1).AND.IROW.LT.NROW)THEN
   IF(JU.GT.0)THEN
    IF(LTB)THEN
     IF(TFV.GE.BFV)WRITE(JU,'(I10,3(1X,E15.7),I10)') N,C,RES,FDZ,ISYS
    ELSE
     WRITE(JU,'(I10,1X  ,E15.7 ,I10)') N,C,ISYS
    ENDIF
   ENDIF
   IF(IROW.LT.PRJIDF%NROW)THEN
    IF(LTB)THEN
     IF(TFV.GE.BFV)THEN
      T1=TFV; B1=BFV
      WRITE(IU,'(I10)') N  
      WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL-1),',',IDF%SY(IROW),',',T1
      WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL  ),',',IDF%SY(IROW),',',T1
      WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL  ),',',IDF%SY(IROW),',',B1
      WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL-1),',',IDF%SY(IROW),',',B1
      WRITE(IU,'(3(F15.3,A1))') IDF%SX(ICOL-1),',',IDF%SY(IROW),',',T1
      WRITE(IU,'(A)') 'END'
     ENDIF
    ELSE
     WRITE(IU,'(I10)') N  
     WRITE(IU,'(2(F15.3,A1))') IDF%SX(ICOL-1),',',IDF%SY(IROW)
     WRITE(IU,'(2(F15.3,A1))') IDF%SX(ICOL  ),',',IDF%SY(IROW)
     WRITE(IU,'(A)') 'END'
    ENDIF
   ENDIF
  ENDIF
 ENDIF
 
 END SUBROUTINE PMANAGER_SAVEMF2005_HFB_GENFILES
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_MOD_READ(IDF,ITOPIC,IFILE,SCL_D,SCL_U,IINV,IPRT) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITOPIC,IFILE,SCL_D,SCL_U,IINV,IPRT
 CHARACTER(LEN=256) :: FNAME
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: ICNST,ILAY 
 REAL(KIND=DP_KIND) :: FCT,IMP,CNST
 
 PMANAGER_SAVEMF2005_MOD_READ=.TRUE.

 FCT  =FNAMES(IFILE)%FCT   
 IMP  =FNAMES(IFILE)%IMP   
 ILAY =FNAMES(IFILE)%ILAY  
 ICNST=FNAMES(IFILE)%ICNST 
 CNST =FNAMES(IFILE)%CNST  
 FNAME=FNAMES(IFILE)%FNAME 
 
 IF(IPRT.GT.0)THEN
  WRITE(IPRT,'(1X,A,I3,A1,I1,A1,I4.3,3(A1,G15.7),A1,A)') TOPICS(ITOPIC)%TNAME(1:5)//',', &
       IFILE,',',ICNST,',',ILAY,',',FCT,',',IMP,',',CNST,',',CHAR(39)//TRIM(FNAME)//CHAR(39)
 ENDIF
 
 IF(ICNST.EQ.1)THEN
  IDF%X=CNST
 ELSEIF(ICNST.EQ.2.OR.ICNST.EQ.3)THEN
  IDF%FNAME=FNAME
  !## read/clip/scale idf file
  PMANAGER_SAVEMF2005_MOD_READ=IDFREADSCALE(IDF%FNAME,IDF,SCL_U,SCL_D,1.0D0,0)
 ENDIF
 !## apply factors if no errors occured
 IF(PMANAGER_SAVEMF2005_MOD_READ)CALL PMANAGER_SAVEMF2005_FCTIMP(IINV,ICNST,IDF,FCT,IMP,SCL_U)

 END FUNCTION PMANAGER_SAVEMF2005_MOD_READ
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_MOD_U2DREL(EXFNAME,IDF,IINT,IU,ILAY,IFBND)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: EXFNAME
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 CHARACTER(LEN=256) :: SFNAME
 INTEGER,INTENT(IN) :: IINT,IU,ILAY,IFBND
 INTEGER :: JU,IROW,ICOL,I,N
 REAL(KIND=DP_KIND) :: MINV,MAXV
 
 PMANAGER_SAVEMF2005_MOD_U2DREL=.FALSE.

 !## correct for boundary conditions
 IF(.NOT.PMANAGER_SAVEMF2005_PCK_GETMINMAX(IDF%X,IDF%NCOL,IDF%NROW,BND(ILAY)%X,MINV,MAXV,IFBND))RETURN
 
 !## constant value
 IF(MAXV.EQ.MINV)THEN

  IF(IINT.EQ.0)THEN
   IF(MAXV.EQ.IDF%NODATA)THEN
    LINE='CONSTANT '//TRIM(RTOS(HNOFLOW,'E',7))
   ELSE
    LINE='CONSTANT '//TRIM(RTOS(MAXV,'E',7))
   ENDIF
  ELSEIF(IINT.EQ.1)THEN
   IF(MAXV.EQ.IDF%NODATA)THEN
    LINE='CONSTANT '//TRIM(ITOS(0))
   ELSE
    LINE='CONSTANT '//TRIM(ITOS(INT(MAXV)))
   ENDIF
  ENDIF
  IF(PBMAN%IFORMAT.EQ.2)WRITE(IU,'(A)') TRIM(LINE)
  IF(PBMAN%IFORMAT.EQ.3)WRITE(IU,'(A)') '  '//TRIM(LINE)
  
 ELSE
 
  CALL UTL_CREATEDIR(EXFNAME(:INDEX(EXFNAME,'\',.TRUE.)-1))
  IF(PBMAN%IFORMAT.EQ.3)THEN; N=4; ELSE; N=3; ENDIF
  SFNAME=EXFNAME; DO I=1,N; SFNAME=SFNAME(:INDEX(SFNAME,'\',.TRUE.)-1); ENDDO
  I=LEN_TRIM(SFNAME); SFNAME='.'//EXFNAME(I+1:)
 
  IF(PBMAN%IFORMAT.EQ.2)THEN
   IF(IINT.EQ.0)WRITE(IU,'(A)') 'OPEN/CLOSE '//TRIM(SFNAME)//' 1.0D0 (FREE) -1'
   IF(IINT.EQ.1)WRITE(IU,'(A)') 'OPEN/CLOSE '//TRIM(SFNAME)//' 1 (FREE) -1'
  ELSE
   IF(IINT.EQ.0)WRITE(IU,'(A)') '  OPEN/CLOSE '//TRIM(SFNAME)//' FACTOR 1.0D0 IPRN -1'
   IF(IINT.EQ.1)WRITE(IU,'(A)') '  OPEN/CLOSE '//TRIM(SFNAME)//' FACTOR 1 IPRN -1'
  ENDIF
  
  IF(TRIM(EXFNAME(INDEX(EXFNAME,'.',.TRUE.)+1:)).EQ.'ASC')THEN

   JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=EXFNAME,STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(JU.EQ.0)RETURN

   WRITE(JU,'(A14,I10)')   'NCOLS'        ,IDF%NCOL
   WRITE(JU,'(A14,I10)')   'NROWS'        ,IDF%NROW
   WRITE(JU,'(A14,G15.7)') 'XLLCORNER'    ,IDF%XMIN
   WRITE(JU,'(A14,G15.7)') 'YLLCORNER'    ,IDF%YMIN
   WRITE(JU,'(A14,G15.7)') 'CELLSIZE'     ,IDF%DX
   WRITE(JU,'(A14,G15.7)') 'NODATA_VALUE ',IDF%NODATA

   IF(IINT.EQ.1)THEN
    DO IROW=1,IDF%NROW; WRITE(JU,*) (INT(IDF%X(ICOL,IROW)),ICOL=1,IDF%NCOL); ENDDO
   ELSE
    DO IROW=1,IDF%NROW; WRITE(JU,*)     (IDF%X(ICOL,IROW) ,ICOL=1,IDF%NCOL); ENDDO
   ENDIF
   CLOSE(JU)
 
  ELSEIF(TRIM(EXFNAME(INDEX(EXFNAME,'.',.TRUE.)+1:)).EQ.'IDF')THEN

   IF(.NOT.IDFWRITE(IDF,EXFNAME,1))RETURN 
   
  ELSE

   JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=EXFNAME,STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(JU.EQ.0)RETURN

   IF(LFREEFORMAT)THEN
    CALL IDFWRITEFREE(JU,IDF,IINT,'B','*') 
   ELSE
    IF(IINT.EQ.1)THEN
     DO IROW=1,IDF%NROW; WRITE(JU,*) (INT(IDF%X(ICOL,IROW)),ICOL=1,IDF%NCOL); ENDDO
    ELSE
     DO IROW=1,IDF%NROW; WRITE(JU,*)     (IDF%X(ICOL,IROW) ,ICOL=1,IDF%NCOL); ENDDO
    ENDIF
   ENDIF
   CLOSE(JU)

  ENDIF
 
 ENDIF
 
 PMANAGER_SAVEMF2005_MOD_U2DREL=.TRUE.
   
 END FUNCTION PMANAGER_SAVEMF2005_MOD_U2DREL
 
 !###======================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_FCTIMP(IINV,ICNST,IDF,FCT,IMP,SCL_U)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IINV,ICNST,SCL_U
 REAL(KIND=DP_KIND),INTENT(IN) :: FCT,IMP
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: IROW,ICOL

 !## replace nodata for hnoflow-value
 DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
  !## not constant value and equal to nodata - skip it
  IF(ICNST.EQ.2.AND.IDF%X(ICOL,IROW).EQ.IDF%NODATA)THEN
   !## geometric will otherwise ignore zero as entry which is allowed
   IF(SCL_U.EQ.3)THEN
    IDF%X(ICOL,IROW)=0.0D0
   ELSE
    IDF%X(ICOL,IROW)=HNOFLOW
   ENDIF
  ELSE
   IDF%X(ICOL,IROW)=IDF%X(ICOL,IROW)*FCT+IMP
  ENDIF
  !## translate from resistance into reciprocal conductance
  !## translate from vka into reciprocal vka
  IF(IINV.EQ.1)THEN
   IF(IDF%X(ICOL,IROW).NE.0.0D0.AND.IDF%X(ICOL,IROW).NE.HNOFLOW)IDF%X(ICOL,IROW)=1.0D0/IDF%X(ICOL,IROW) 
  ENDIF
 ENDDO; ENDDO
 
 IDF%NODATA=HNOFLOW

 END SUBROUTINE PMANAGER_SAVEMF2005_FCTIMP

 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_LAK_CONFIG()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IROW,ICOL,ILAY,I,JROW,JCOL
 REAL(KIND=DP_KIND) :: C,ZT,ZB,X1,X2,Y1,Y2,L,TIB,F,KD1,KD2,OT1,OT2
 INTEGER,DIMENSION(4) :: IR,IC
 DATA IR/-1, 0,0,1/
 DATA IC/ 0,-1,1,0/
 
 PMANAGER_SAVEMF2005_LAK_CONFIG=.TRUE.
 
 IF(.NOT.LLAK)RETURN

 PMANAGER_SAVEMF2005_LAK_CONFIG=.FALSE.
 
 !## lake numbers are integer values only
 DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
  LAK(1)%X(ICOL,IROW)=INT(LAK(1)%X(ICOL,IROW))
 ENDDO; ENDDO
 
 !## get unique number of lakes
 ALLOCATE(DULAKES(PRJIDF%NCOL*PRJIDF%NROW))
 I=0; DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL; I=I+1; DULAKES(I)=INT(LAK(1)%X(ICOL,IROW)); ENDDO; ENDDO
 CALL UTL_GETUNIQUE_INT(DULAKES,PRJIDF%NROW*PRJIDF%NCOL,NLAKES,0)

 ALLOCATE(ULAKES(NLAKES)); DO I=1,NLAKES; ULAKES(I)=DULAKES(I); ENDDO; DEALLOCATE(DULAKES)

 !## reset array lbd - boundary settings, layer becomes lakes as bathymetry of over half of cell
 DO ILAY=1,PRJNLAY; DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL; LBD(ILAY)%X(ICOL,IROW)=0.0D0; ENDDO; ENDDO; ENDDO
 !## reset array lcd - sum of conductance vertically/horizontally
 DO ILAY=1,PRJNLAY; DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL; LCD(ILAY)%X(ICOL,IROW)=0.0D0; ENDDO; ENDDO; ENDDO

 !## get lakebed leakance - combination of resistance and model resistance of depth AROUND lake
 DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
  !## skip non lake cells
  IF(LAK(1)%X(ICOL,IROW).LE.0)CYCLE
  !## find appropriate modellayer underneath bathymetry of lake
  DO ILAY=1,PRJNLAY

   !## apply lakes only for active cells (>0)
   IF(BND(ILAY)%X(ICOL,IROW).LE.0)CYCLE

   ZT=TOP(ILAY)%X(ICOL,IROW)
   !## found appropriate modellayer
   IF(ZT.GT.LAK(2)%X(ICOL,IROW))THEN

    !## cannot have a lake in the lowest model layer
    IF(ILAY.EQ.PRJNLAY)THEN
!     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You cannot put a lake in the lowest model layer'//CHAR(13)// &
!         'Make sure the bathymetry is always higher than the top of'//CHAR(13)// &
!         'your lowest model layer in order to avoid this error message.','Error')
!     RETURN
    ENDIF

    !## lake number is equal to internal number in the sort-list
    DO I=1,NLAKES
     IF(INT(LAK(1)%X(ICOL,IROW)).EQ.ULAKES(I))THEN; LBD(ILAY)%X(ICOL,IROW)=I; EXIT; ENDIF 
    ENDDO
    
    BND(ILAY)%X(ICOL,IROW)=0.0D0

    !## modify existing aquitard due to this displacement - can be removed partly by lake
    IF(ILAY.LT.PRJNLAY)THEN
     !## bottom of current model layer
     ZB=TOP(ILAY+1)%X(ICOL,IROW) 
    ELSE
     ZB=BOT(ILAY)%X(ICOL,IROW) 
    ENDIF
    !## thickness original interbed
    TIB=BOT(ILAY)%X(ICOL,IROW)-ZB

!top =10
!lak = 4
!bot = 2
!zb  = 0
!tib = 2

    !## compute fraction for leakance in case lake bathymetry is higher
    IF(ZB.LT.LAK(2)%X(ICOL,IROW))THEN
     !## add extra resistance to leakance of part of aquifer
     IF(BOT(ILAY)%X(ICOL,IROW).LT.LAK(2)%X(ICOL,IROW))THEN
      C=(LAK(2)%X(ICOL,IROW)-BOT(ILAY)%X(ICOL,IROW))/(KHV(ILAY)%X(ICOL,IROW)/KVA(ILAY)%X(ICOL,IROW))
     ENDIF

     OT1=0.0D0; OT2=0.0D0
     IF(ILAY.LT.PRJNLAY)THEN
      OT1=BOT(ILAY  )%X(ICOL,IROW)-TOP(ILAY+1)%X(ICOL,IROW)
      OT2=TOP(ILAY+1)%X(ICOL,IROW)-BOT(ILAY+1)%X(ICOL,IROW)
     ENDIF
     
     !## adjust bot as the LAK package uses this to create the table input
     BOT(ILAY)%X(ICOL,IROW)=LAK(2)%X(ICOL,IROW)

     !## make sure thickness of interbed remains the same
     IF(TIB.EQ.0.0D0)THEN

      !## increase permeability in ratio in case no interbed and interface is shifted upwards
      IF(ILAY.LT.PRJNLAY)THEN
       TOP(ILAY+1)%X(ICOL,IROW)=BOT(ILAY)%X(ICOL,IROW)

       KD1=KHV(ILAY  )%X(ICOL,IROW)*OT1 
       KD2=KHV(ILAY+1)%X(ICOL,IROW)*OT2 
       KD1=KD1+KD2; KD2=KD1/OT2 
       KHV(ILAY+1)%X(ICOL,IROW)=KHV(ILAY+1)%X(ICOL,IROW)*KD2
      ENDIF

     ELSE
      !## top remains the same but thickness can be enlarged of the interbed, correct with permeability
      F=(BOT(ILAY)%X(ICOL,IROW)-TOP(ILAY+1)%X(ICOL,IROW))/TIB
      KVV(ILAY)%X(ICOL,IROW)=KVV(ILAY)%X(ICOL,IROW)*F
     ENDIF
    ELSE
     C=0.0D0
    ENDIF
    
    !## lake leakance for vertical conductances - excl. the effect of vertical shift, this is taken care of by MF2005
    LCD(ILAY)%X(ICOL,IROW)=1.0D0/LAK(6)%X(ICOL,IROW) 

   ENDIF
  ENDDO
 ENDDO; ENDDO
 
 !## get lakebed lateral leakances
 DO ILAY=1,PRJNLAY; DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
  !## found lake cell
  IF(LBD(ILAY)%X(ICOL,IROW).NE.0)THEN

   !## compute lateral leakances
   DO I=1,SIZE(IC)
    JROW=IR(I)+IROW; JCOL=IC(I)+ICOL
    IF(JROW.GT.PRJIDF%NROW.OR.JROW.LT.1)CYCLE
    IF(JCOL.GT.PRJIDF%NCOL.OR.JCOL.LT.1)CYCLE
    !## not equal a lake, thus next to the lake and not inactive cell
    IF(LBD(ILAY)%X(JCOL,JROW).EQ.0.AND. &
       BND(ILAY)%X(JCOL,JROW).NE.0)THEN
     CALL IDFGETEDGE(PRJIDF,JROW,JCOL,X1,Y1,X2,Y2)
     IF(JROW.EQ.IROW)THEN; L=X2-X1 ; ENDIF
     IF(JCOL.EQ.ICOL)THEN; L=Y2-Y1 ; ENDIF
     !## resistance along lake
     C=L/KHV(ILAY)%X(ICOL,IROW)

     !## lake leakance for vertical conductances - excl. the effect of vertical shift, this is taken care of by MF2005
     LCD(ILAY)%X(JCOL,JROW)=1.0D0/LAK(6)%X(ICOL,IROW)

    ENDIF
   ENDDO
 
  ENDIF
 ENDDO; ENDDO; ENDDO

 PMANAGER_SAVEMF2005_LAK_CONFIG=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_LAK_CONFIG

 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_LAKE_GETMEANVALUE(X,Y,ULAKE,LVL,IBATCH,IOP)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),DIMENSION(:,:),INTENT(IN) :: X,Y
 INTEGER,INTENT(IN) :: ULAKE
 INTEGER,INTENT(IN) :: IBATCH,IOP
 REAL(KIND=DP_KIND),INTENT(OUT) :: LVL
 REAL(KIND=DP_KIND) :: ILVL
 INTEGER :: IROW,ICOL
 
 PMANAGER_SAVEMF2005_LAKE_GETMEANVALUE=.FALSE.
 
 LVL=0.0D0; ILVL=0.0D0

 DO IROW=1,PRJIDF%NROW; DO ICOL=1,PRJIDF%NCOL
  IF(INT(X(ICOL,IROW)).EQ.ULAKE)THEN
   SELECT CASE (IOP)
    !## average/sum
    CASE (1,4); LVL=LVL+Y(ICOL,IROW); ILVL=ILVL+1.0D0
    !## min
    CASE (2); IF(ILVL.EQ.0)THEN; LVL=Y(ICOL,IROW); ELSE; LVL=MIN(LVL,Y(ICOL,IROW)); ENDIF; ILVL=ILVL+1.0D0
    !## max
    CASE (3); IF(ILVL.EQ.0)THEN; LVL=Y(ICOL,IROW); ELSE; LVL=MAX(LVL,Y(ICOL,IROW)); ENDIF; ILVL=ILVL+1.0D0
   END SELECT
  ENDIF
 ENDDO; ENDDO

 IF(ILVL.LE.0.0D0)THEN
  IF(IBATCH.EQ.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot assign a lakelevel for lake '//TRIM(ITOS(ULAKE)),'Error')
   RETURN
  ELSE
   WRITE(*,'(A)') 'iMOD cannot assign a lakelevel for lake '//TRIM(ITOS(ULAKE)); STOP
  ENDIF
 ENDIF

 IF(IOP.EQ.1)LVL=LVL/ILVL

 PMANAGER_SAVEMF2005_LAKE_GETMEANVALUE=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_LAKE_GETMEANVALUE

 !###======================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_BND(ILAY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ILAY
 INTEGER :: IROW,ICOL,NN,NE,NS,NW

 DO IROW=1,BND(ILAY)%NROW
  DO ICOL=1,BND(ILAY)%NCOL
   IF(BND(ILAY)%X(ICOL,IROW).EQ.HNOFLOW)BND(ILAY)%X(ICOL,IROW)=0.0D0
   !## snap to integer
   BND(ILAY)%X(ICOL,IROW)=DBLE(INT(BND(ILAY)%X(ICOL,IROW)))
   !## correct for boundary values from mf6
   IF(PBMAN%NSUBMODEL.GT.1.AND.PBMAN%IFORMAT.EQ.3)THEN
    IF(PRJIDF%X(ICOL,IROW).EQ.PRJIDF%NODATA)BND(ILAY)%X(ICOL,IROW)=0.0D0
   ENDIF
  ENDDO
 ENDDO
 
 NN=0; NW=0; NS=0; NE=0
 
 !## no applicable with submodel via mf6
 IF(PBMAN%NSUBMODEL.GT.1.AND.PBMAN%IFORMAT.EQ.3)RETURN
 
 !## replace ibound for boundaries
 DO IROW=1,BND(ILAY)%NROW
  IF(IFULL(1).EQ.1)THEN; ICOL=1;              IF(BND(ILAY)%X(ICOL,IROW).GT.0)THEN; NW=NW+1; BND(ILAY)%X(ICOL,IROW)=-1; ENDIF; ENDIF
  IF(IFULL(3).EQ.1)THEN; ICOL=BND(ILAY)%NCOL; IF(BND(ILAY)%X(ICOL,IROW).GT.0)THEN; NE=NE+1; BND(ILAY)%X(ICOL,IROW)=-1; ENDIF; ENDIF
 ENDDO
 DO ICOL=1,BND(ILAY)%NCOL
  IF(IFULL(4).EQ.1)THEN; IROW=1;              IF(BND(ILAY)%X(ICOL,IROW).GT.0)THEN; NN=NN+1; BND(ILAY)%X(ICOL,IROW)=-1; ENDIF; ENDIF
  IF(IFULL(2).EQ.1)THEN; IROW=BND(ILAY)%NROW; IF(BND(ILAY)%X(ICOL,IROW).GT.0)THEN; NS=NS+1; BND(ILAY)%X(ICOL,IROW)=-1; ENDIF; ENDIF
 ENDDO
    
 IF(NN+NS+NW+NE.GT.0)THEN
  WRITE(*,'(A)') 'Modified boundary layer '//TRIM(ITOS(ILAY))//' due to submodelling N/S/W/E: ' // &
     TRIM(ITOS(NN))//'/'//TRIM(ITOS(NS))//'/'//TRIM(ITOS(NW))//'/'//TRIM(ITOS(NE))
 ENDIF
 
 END SUBROUTINE PMANAGER_SAVEMF2005_BND

 !###======================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_CORRECT(ILAY,BND,IDF,ITYPE,ITOPIC)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITOPIC,ILAY,ITYPE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: BND
 INTEGER :: IROW,ICOL,JLAY
 LOGICAL :: LEX
 CHARACTER(LEN=1) :: YESNO
 
 IF(ILAY.GT.0)THEN 
  DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL

   !## blank out inactive cells
   IF(BND(ILAY)%X(ICOL,IROW).EQ.0)THEN
    IDF%X(ICOL,IROW)=IDF%NODATA
   ELSE
    IF(ITYPE.EQ.0)THEN
     !## check whether nodata for active location
     IF(IDF%X(ICOL,IROW).EQ.IDF%NODATA)THEN
      LEX=.TRUE.
      !## vcw/kvv might be inactive though boundary underneath is zero
      IF(ITOPIC.EQ.TVCW.OR.ITOPIC.EQ.TKVV)THEN
       IF(BND(ILAY+1)%X(ICOL,IROW).EQ.0)LEX=.FALSE.
      ENDIF
      IF(LEX)THEN
       IF(.NOT.LYESNO)THEN
        WRITE(*,'(/1X,A)') 'Error NodataValue found for active cell'
        WRITE(*,'(A3,3A4,3A15        )') 'VAR','COL','ROW','LAY','IBOUND','X','NODATAVALUE'
        WRITE(*,'(A3,3I4,F15.1,2E15.7)') TOPICS(ITOPIC)%CMOD,ICOL,IROW,ILAY,BND(ILAY)%X(ICOL,IROW),IDF%X(ICOL,IROW),IDF%NODATA
        WRITE(*,'(A$)') 'Continue yes (default value of 1.0D0 is set) / no ?'
        READ(*,'(A1)') YESNO
        IF(UTL_CAP(YESNO,'U').EQ.'N')STOP
        LYESNO=.TRUE.
       ELSE
        !## set dummy value
        IDF%X(ICOL,IROW)=1.0D0
       ENDIF
      ENDIF
     ENDIF  
    ENDIF
   ENDIF

   !## blank out layer below in case of vertical conductance
   IF(ITOPIC.EQ.TVCW.OR.ITOPIC.EQ.TKVV)THEN 
    IF(BND(ILAY+1)%X(ICOL,IROW).EQ.0)IDF%X(ICOL,IROW)=IDF%NODATA
   ENDIF
  ENDDO; ENDDO

 !## find uppermost active cell
 ELSEIF(ILAY.EQ.0)THEN

  DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
   DO JLAY=1,PRJNLAY; IF(BND(JLAY)%X(ICOL,IROW).GT.0)EXIT; ENDDO
   !## skip if location is equal to nodata, completely
   IF(JLAY.GT.PRJNLAY)CYCLE

   IF(ITYPE.EQ.0)THEN
    !## check whether nodata for active location
    IF(IDF%X(ICOL,IROW).EQ.IDF%NODATA)THEN
     WRITE(*,'(/1X,A)') 'Error NodataValue found for active cell'
     WRITE(*,'(A3,3A4,3A15        )') 'VAR','COL','ROW','LAY','IBOUND','X','NODATAVALUE'
     WRITE(*,'(A3,3I4,F15.1,2E15.7)') TOPICS(ITOPIC)%CMOD,ICOL,IROW,JLAY,BND(JLAY)%X(ICOL,IROW),IDF%X(ICOL,IROW),IDF%NODATA
     PAUSE; STOP
    ENDIF  
   ENDIF

  ENDDO; ENDDO
 
 ENDIF

 !## blank out negative values for 'KDW','KHV','KVA','VCW','KVV','STO','SSC'
 SELECT CASE (ITOPIC)
  CASE (TKDW,TKHV,TKVA,TVCW,TKVV,TSTO,TSPY) !6:12)
   DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
    IF(IDF%X(ICOL,IROW).EQ.IDF%NODATA)CYCLE
    IF(IDF%X(ICOL,IROW).LT.0.0D0)IDF%X(ICOL,IROW)=0.0D0 
   ENDDO; ENDDO
 END SELECT
 
 !## remove input for inactive cells
 IF(ILAY.GT.0)THEN
  DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
   IF(BND(ILAY)%X(ICOL,IROW).EQ.0)IDF%X(ICOL,IROW)=IDF%NODATA
  ENDDO; ENDDO
 ENDIF
 
 !## skip fhb(31) / chd(28) package
 IF(ITOPIC.NE.TFHB.AND.ITOPIC.NE.TCHD)THEN
  !## remove packages on constant head cells
  IF(ITYPE.EQ.1.AND.ILAY.GT.0)THEN
   DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
    !## blank out constant head cells
    IF(BND(ILAY)%X(ICOL,IROW).LT.0)IDF%X(ICOL,IROW)=IDF%NODATA
   ENDDO; ENDDO
  ENDIF
 ENDIF
  
 END SUBROUTINE PMANAGER_SAVEMF2005_CORRECT

END MODULE MOD_PMANAGER_MF2005