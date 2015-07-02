!!  Copyright (C) Stichting Deltares, 2005-2014.
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
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!!
!!  Contact: imod.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands.
!!
MODULE MOD_MAP2IDF

USE WINTERACTER
USE MOD_IDF, ONLY     : IDFREAD,IDFWRITE,IDFWRITEDIM,IDFOPEN,IDFALLOCATEX,IDFDEALLOCATE,&
                        IDFDEALLOCATESX,IDFWRITECOMMENT,IDFFILLCOMMENT,IDFNULLIFY
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_MAP2IDF_PAR
USE MOD_UTL, ONLY     : UTL_GETUNIT,ITOS 
USE MOD_OSD, ONLY     : OSD_OPEN

 INTEGER,ALLOCATABLE,DIMENSION(:),PRIVATE :: IOS
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:)    :: IDF

CONTAINS

!#####=================================================================
SUBROUTINE MAP2IDF_IMPORTMAP(IDFNAME,I)
!#####=================================================================

 USE IEEE_ARITHMETIC    
 USE ISO_C_BINDING
  
 IMPLICIT NONE
 CHARACTER(LEN=1024) :: IDFNAME
 INTEGER :: IERROR
 
!## Locals
 INTEGER :: IU,ICOL,IROW,I,J,I4MINVAL,I4MAXVAL,NCOL,NROW
 REAL    :: NODATA,R4MINVAL,R4MAXVAL,RVAL
 REAL    :: XMIN,XMAX,YMIN,YMAX,CSIZE 
 LOGICAL :: LBIG
 CHARACTER(LEN=256) :: LINE

!## Map file structure

!## Main header
 CHARACTER(LEN=32) :: SIGNATURE
 INTEGER(KIND=2)   :: VERSION
 INTEGER(KIND=4)   :: GISFILEID
 INTEGER(KIND=2)   :: PROJECTION
 INTEGER(KIND=4)   :: ATTRTABLE
 INTEGER(KIND=2)   :: DATATYPE
 INTEGER(KIND=4)   :: BYTEORDER
!## Raster header
 INTEGER(KIND=2)  :: VALUESCALE
 INTEGER(KIND=2)  :: CELLREPR
 REAL(KIND=8)     :: XUL
 REAL(KIND=8)     :: YUL
 INTEGER(KIND=4)  :: NRROWS
 INTEGER(KIND=4)  :: NRCOLS
 REAL(KIND=8)     :: CELLSIZEX
 REAL(KIND=8)     :: CELLSIZEY
 INTEGER, DIMENSION(:,:), ALLOCATABLE      :: I4A
 REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: RA

!#####=================================================================

 CHARACTER(C_CHAR) :: C, MOLD
    
 INTEGER(1), PARAMETER :: TEST = INT(Z'FF',1)
 
 C = TRANSFER(I, MOLD)
 WRITE(*,*) HUGE(TEST)

 IERROR=1

 ALLOCATE(IOS(1))    ! Question: IOS(#), what is the meaning of the number??
 
 ALLOCATE(IDF(1)); CALL IDFNULLIFY(IDF(1))
 
 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,'IMPORTING '//TRIM(IDFNAME))
  
!## open and read input file (see for similar routine imod_asc2idf.f90 ln 182-188)

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=IDFNAME,STATUS='OLD',FORM='UNFORMATTED',ACTION='READ',ACCESS='stream',IOSTAT=IOS(1))
 IF(IOS(1).NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'CAN NOT OPEN FILE: '//CHAR(13)// &
   '['//TRIM(IDFNAME)//']'//CHAR(13)//'FOR READING','ERROR')
  RETURN
 ENDIF
 
!## Main header
 READ(IU,POS=  0+1) SIGNATURE
 READ(IU,POS= 32+1) VERSION
 READ(IU,POS= 34+1) GISFILEID
 READ(IU,POS= 38+1) PROJECTION
 DO I = 1, NPT
  IF(PTVAL(I).EQ.PROJECTION)THEN
   WRITE(*,*) 'PROJECTION: ',TRIM(PTSTR(I))
  END IF  
 END DO   

 READ(IU,POS= 40+1) ATTRTABLE
 READ(IU,POS= 44+1) DATATYPE
 READ(IU,POS= 46+1) BYTEORDER
!## Raster header
 READ(IU,POS= 64+1) VALUESCALE
 READ(IU,POS= 66+1) CELLREPR
 
 DO I = 1, NCR
  IF(CRVAL(I).EQ.CELLREPR)THEN
   WRITE(*,*) 'CELL REPRESENTATION: ',TRIM(CRSTR(I))
   CELLREPR=I
   EXIT
  END IF   
 END DO
 
 SELECT CASE(CELLREPR)
 CASE(CR_INT4)
  READ(IU,POS= 68+1) I4MINVAL
  READ(IU,POS= 76+1) I4MAXVAL
 CASE(CR_REAL4)
  READ(IU,POS= 68+1) R4MINVAL
  READ(IU,POS= 76+1) R4MAXVAL
 CASE DEFAULT
  WRITE(*,*) 'NOT SUPPORTED'
  STOP 1
 END SELECT
 
 READ(IU,POS= 84+1) XUL
 READ(IU,POS= 92+1) YUL
 READ(IU,POS=100+1) NRROWS
 READ(IU,POS=104+1) NRCOLS
 READ(IU,POS=108+1) CELLSIZEX
 READ(IU,POS=116+1) CELLSIZEY

!## Checks
 IF (CELLSIZEX.NE.CELLSIZEY) THEN
!## melding
  WRITE(*,*) 'ERROR, CELLSIZEX /= CELLSIZEY'; 
 END IF

!## nodata is optional
  NODATA = 999.0
  IF(IOS(1).NE.0)THEN; NODATA=-99999.99; ENDIF

  IDF(1)%NCOL    =NRCOLS
  IDF(1)%NROW    =NRROWS
  IDF(1)%XMIN    =XUL
  IDF(1)%XMAX    =XUL+NRCOLS*CELLSIZEX
  IDF(1)%YMIN    =YUL
  IDF(1)%YMAX    =YUL-NRROWS*CELLSIZEY
  IDF(1)%NODATA  =NODATA  
  IDF(1)%IEQ     =0
  IDF(1)%DX      =CELLSIZEX
  IDF(1)%DY      =CELLSIZEY
  IDF(1)%IXV     =0
  IDF(1)%ITB     =0
  IF(.NOT.IDFALLOCATEX(IDF(1)))THEN
!GAAT WAT FOUT ...
  ENDIF
  
!!## Read data block
 SELECT CASE(CELLREPR)
  CASE(CR_INT4)
   ALLOCATE(I4A(NRCOLS,NRROWS))
   ALLOCATE(RA(NRCOLS,NRROWS))
   READ(IU,POS=256+1)((I4A(ICOL,IROW),IROW=1,NRROWS),ICOL=1,NRCOLS)
!## replace nan with nodata
   DO IROW = 1, NRROWS
    DO ICOL = 1, NRCOLS
     RA(ICOL,IROW)=REAL(I4A(ICOL,IROW))
    END DO    
   END DO
   
   DEALLOCATE(RA)
   
  CASE(CR_REAL4)
   ALLOCATE(RA(NRROWS,NRCOLS))
   READ(IU,POS=256+1)((RA(irow,icol),IROW=1,NRROWS),ICOL=1,NRCOLS)

!## revert colums with rows
   DO IROW = 1, NRROWS
    DO ICOL = 1, NRCOLS
     IDF(1)%X(ICOL,IROW) = RA(irow,icol)  
    END DO    
   END DO
   
   DEALLOCATE(RA)
   
!## replace nan with nodata
   DO IROW = 1, NRROWS
    DO ICOL = 1, NRCOLS
     IF (IEEE_IS_NAN(IDF(1)%X(ICOL,IROW))) THEN
      IDF(1)%X(ICOL,IROW) = NODATA 
     END IF    
    END DO    
   END DO
    
 END SELECT

 CLOSE(IU)

!## write idf file
 
!## Write IDF
  CALL IDFFILLCOMMENT(IDF(1),'IMPORTED FROM '//TRIM(IDFNAME))
   J=INDEXNOCASE(IDFNAME,'.',.TRUE.)-1
   IF(.NOT.IDFWRITE(IDF(1),IDFNAME(:J)//'.IDF',1))THEN; ENDIF
 
 CALL IDFDEALLOCATE(IDF,SIZE(IDF)); DEALLOCATE(IDF)
 
  IF(ALLOCATED(I4A))DEALLOCATE(I4A)   
   
 
 END SUBROUTINE MAP2IDF_IMPORTMAP
 
END MODULE MOD_MAP2IDF  
