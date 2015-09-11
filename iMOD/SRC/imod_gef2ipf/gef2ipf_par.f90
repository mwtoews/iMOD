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
MODULE MOD_GEF2IPF_PAR

USE WINTERACTER, ONLY : IOSDIRENTRYTYPE,IOSDIRCOUNT
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_UTL, ONLY : ITOS,RTOS,UTL_GETUNIT,UTL_CAP,IOSDIRMAKE,UTL_DIRINFO

CHARACTER(LEN=256),DIMENSION(:),POINTER :: GEFNAMES
CHARACTER(LEN=50),DIMENSION(:),ALLOCATABLE :: ATTRIB1,ATTRIB2
INTEGER,DIMENSION(:),ALLOCATABLE :: IATTRIB
REAL,DIMENSION(:),ALLOCATABLE :: NODATA
REAL,ALLOCATABLE,DIMENSION(:,:) :: GEF
CHARACTER(LEN=256) :: GEFDIR,IPFFNAME,GENFNAME
CHARACTER(LEN=50) :: CID
REAL :: X,Y,Z,ZEND
INTEGER :: N,NCOL,NROW,IU,JU
REAL,DIMENSION(:),ALLOCATABLE :: FMULT

INTEGER,PARAMETER :: NCOLIPF=5

CONTAINS

 !###====================================================================
 LOGICAL FUNCTION LREADGEF(INAME)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: INAME
 INTEGER :: I,ICOL,IROW,IOS
 CHARACTER(LEN=50) :: STRING
 CHARACTER(LEN=256) :: LINE
 
 LREADGEF=.FALSE.

 IU=UTL_GETUNIT(); OPEN(IU,FILE=GEFNAMES(INAME),STATUS='OLD',ACTION='READ')

 WRITE(*,*) TRIM(GEFNAMES(INAME))

 IF(.NOT.LREADKEYWORD('#TESTID=',STRING,0))RETURN
 READ(STRING,*,IOSTAT=IOS) CID
 IF(IOS.NE.0)THEN
  WRITE(*,*) 'TESTID NOT READ PROPERLY '//TRIM(GEFNAMES(INAME))
  RETURN
 ENDIF

 IF(.NOT.LREADKEYWORD('#COLUMN=',STRING,0))RETURN
 READ(STRING,*,IOSTAT=IOS) NCOL
 IF(IOS.NE.0)THEN
  WRITE(*,*) 'NCOL NOT READ PROPERLY '//TRIM(GEFNAMES(INAME))
  RETURN
 ENDIF
 ALLOCATE(ATTRIB1(MAX(NCOLIPF,NCOL)),ATTRIB2(MAX(NCOLIPF,NCOL)),IATTRIB(NCOLIPF),NODATA(MAX(NCOLIPF,NCOL)),FMULT(NCOLIPF))
 DO I=1,NCOL
  IF(.NOT.LREADKEYWORD('#COLUMNINFO='//TRIM(ITOS(I))//',',STRING,0))STOP
  READ(STRING,*,IOSTAT=IOS) ATTRIB1(I),ATTRIB2(I)
  IF(IOS.NE.0)THEN
   WRITE(*,*) 'ATTRIB1/2 NOT READ PROPERLY '//TRIM(GEFNAMES(INAME))
   RETURN
  ENDIF
 END DO

 !## option
 NODATA=-999.99
 DO I=1,NCOL
  IF(LREADKEYWORD('#COLUMNVOID='//TRIM(ITOS(I))//',',STRING,1))THEN
   READ(STRING,*,IOSTAT=IOS) NODATA(I)
   IF(IOS.NE.0)THEN
    WRITE(*,*) 'NODATA NOT READ PROPERLY '//TRIM(GEFNAMES(INAME))
    RETURN
   ENDIF
!  ELSE
!   WRITE(*,*) 'Asigned for #COLUMNVOID='//TRIM(ITOS(I))//',-999.99'
  ENDIF
 END DO

 IF(.NOT.LREADKEYWORD('#XYID=',STRING,0))RETURN
 READ(STRING,*,IOSTAT=IOS) X,X,Y
 IF(IOS.NE.0)THEN
  WRITE(*,*) 'XY NOT READ PROPERLY '//TRIM(GEFNAMES(INAME))
  RETURN
 ENDIF
 IF(X.LT.0.0)THEN
  X=X+155000.0
  Y=Y+463000.0
 ENDIF
 IF(.NOT.LREADKEYWORD('#ZID=',STRING,0))RETURN
 READ(STRING,*,IOSTAT=IOS) Z,Z
 IF(IOS.NE.0)THEN
  WRITE(*,*) 'Z NOT READ PROPERLY '//TRIM(GEFNAMES(INAME))
  RETURN
 ENDIF
 IF(.NOT.LREADKEYWORD('#LASTSCAN=',STRING,0))RETURN
 READ(STRING,*) NROW
 IF(IOS.NE.0)THEN
  WRITE(*,*) 'NROW NOT READ PROPERLY '//TRIM(GEFNAMES(INAME))
  RETURN
 ENDIF
 IF(NROW.EQ.0)THEN
  WRITE(*,*) 'NROW=0 '//TRIM(GEFNAMES(INAME))
  RETURN
 ENDIF
 ALLOCATE(GEF(NCOL,NROW))

 !## data starts
 IF(.NOT.LREADKEYWORD('EOH=',STRING,0))THEN
  WRITE(*,*) 'EOH NOT READ PROPERLY '//TRIM(GEFNAMES(INAME))
  RETURN
 ENDIF
 DO IROW=1,NROW
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)THEN
   WRITE(*,*) 'File not fully available '//TRIM(GEFNAMES(INAME))
   RETURN
  ENDIF

  DO
   I=INDEX(LINE,';')
   IF(I.EQ.0)EXIT
   LINE(I:I)=','
  ENDDO
  
  READ(LINE,*,IOSTAT=IOS) (GEF(ICOL,IROW),ICOL=1,NCOL)
  IF(IOS.NE.0)THEN
   WRITE(*,*) 'File not fully available '//TRIM(GEFNAMES(INAME))
   RETURN
  ENDIF
 END DO

 CLOSE(IU)

 LREADGEF=.TRUE.

 END FUNCTION

 !###====================================================================
 LOGICAL FUNCTION LREADKEYWORD(CKEY,STRING,IOPT)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPT
 CHARACTER(LEN=256) :: LINE
 CHARACTER(LEN=*),INTENT(OUT) :: STRING
 CHARACTER(LEN=*),INTENT(IN) :: CKEY
 INTEGER :: I,J,IOS

 LREADKEYWORD=.FALSE.

 REWIND(IU)
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  !## remove spaces
  CALL LCLEAN(LINE)
  I=INDEX(UTL_CAP(LINE,'U'),CKEY)
  IF(I.NE.0)THEN
   J=LEN_TRIM(CKEY)
   STRING=LINE(I+J:)
   LREADKEYWORD=.TRUE.
   EXIT
  ENDIF
 ENDDO

 IF(IOPT.EQ.0.AND..NOT.LREADKEYWORD)WRITE(*,*) 'KEYWORD:[ '//TRIM(CKEY)//' ] not found'

 END FUNCTION LREADKEYWORD

 !###====================================================================
 SUBROUTINE LCLEAN(LINE)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: LINE
 INTEGER :: I,J

 DO
  J=LEN_TRIM(LINE)
  I=INDEX(LINE(:J),' ')
  IF(I.EQ.0)EXIT
  LINE(I:J-1)=LINE(I+1:J)
  LINE(J:)=' '
 END DO

 END SUBROUTINE LCLEAN

 !###====================================================================
 SUBROUTINE GEFDEALLOCATE()
 !###====================================================================

 IF(ALLOCATED(IATTRIB))DEALLOCATE(IATTRIB)
 IF(ALLOCATED(ATTRIB1))DEALLOCATE(ATTRIB1)
 IF(ALLOCATED(ATTRIB2))DEALLOCATE(ATTRIB2)
 IF(ALLOCATED(NODATA))DEALLOCATE(NODATA)
 IF(ALLOCATED(FMULT))DEALLOCATE(FMULT)
 IF(ALLOCATED(GEF))DEALLOCATE(GEF)

 END SUBROUTINE GEFDEALLOCATE

END MODULE MOD_GEF2IPF_PAR
