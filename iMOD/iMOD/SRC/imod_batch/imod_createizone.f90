!!  Copyright (C) Stichting Deltares, 2005-2018.
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
MODULE MOD_CREATEIZONE

USE MOD_IDF, ONLY : IDFREAD,IDFWRITE,IDFNULLIFY,IDFOBJ,IDFDEALLOCATE
USE MOD_UTL, ONLY : ITOS,UTL_GETUNIT,UTL_CREATEDIR
USE IMODVAR, ONLY : DP_KIND,SP_KIND

CONTAINS

 !###======================================================================
 SUBROUTINE CREATEIZONE_MAIN(FIDF,PFOLDER,OFOLDER,TPARAMETER,NLAY,MINF,IZONEOFFSET,IGROUPOFFSET)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: PFOLDER,OFOLDER,TPARAMETER
 INTEGER,INTENT(IN) :: NLAY,IZONEOFFSET,IGROUPOFFSET
 REAL(KIND=DP_KIND),INTENT(IN) :: MINF
 CHARACTER(LEN=*),INTENT(IN),DIMENSION(:) :: FIDF
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 CHARACTER(LEN=256) :: LINE
 INTEGER :: I,J,JZ,NZ,tz,IROW,ICOL,IG,IU,JU
 REAL(KIND=DP_KIND) :: F
 
 CALL UTL_CREATEDIR(OFOLDER)
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(OFOLDER)//'\PARAM.TXT',STATUS='UNKNOWN',ACTION='WRITE')
 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OFOLDER)//'\ZONES.TXT',STATUS='UNKNOWN',ACTION='WRITE')
  
 ALLOCATE(IDF(NLAY)); DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO
  
 IG=IGROUPOFFSET; NZ=IZONEOFFSET 
 DO J=1,SIZE(FIDF)
  IG=IG+1
  TZ=0
  DO I=1,NLAY
   CALL IDFDEALLOCATE(IDF,SIZE(IDF))
   IDF(I)%FNAME=TRIM(PFOLDER)//'\FRACTIONS_L'//TRIM(ITOS(I))//'\'//FIDF(J)
   IF(IDFREAD(IDF(I),IDF(I)%FNAME,1,IQ=1))THEN
    JZ=0
    DO IROW=1,IDF(I)%NROW; DO ICOL=1,IDF(I)%NCOL
     !## take fraction only
     F=MOD(IDF(I)%X(ICOL,IROW),1.0D0)
     IF(F.GE.MINF)THEN
      JZ=JZ+1
      IF(F.GT.0.99)THEN
       IDF(I)%X(ICOL,IROW)=REAL(NZ+1) 
      ELSE
       IDF(I)%X(ICOL,IROW)=REAL(NZ+1)+IDF(I)%X(ICOL,IROW)
      ENDIF
     ELSEIF(IDF(I)%X(ICOL,IROW).EQ.1.0D0)THEN
      JZ=JZ+1; IDF(I)%X(ICOL,IROW)=REAL(NZ+1)
     ELSE
      IDF(I)%X(ICOL,IROW)=0.0D0 
     ENDIF
    ENDDO; ENDDO
    TZ=TZ+JZ
    IF(JZ.GT.0)THEN
     NZ=NZ+1
     IDF(I)%FNAME=TRIM(OFOLDER)//'\FRACTIONS_L'//TRIM(ITOS(I))//'\'//TRIM(FIDF(J))
     IF(.NOT.IDFWRITE(IDF(I),IDF(I)%FNAME,1))RETURN

     WRITE(LINE,'(I3,A4,2(I3,A1),5(F5.2,A1),I3)') 1,','//TRIM(TPARAMETER)//',',I,',',NZ,',',1.0D0,',',1.1,',',0.1,',',10.0D0,',',10.0D0,',',IG
     WRITE(IU,'(A)') TRIM(LINE)
     WRITE(JU,'(A)') TRIM(IDF(I)%FNAME)
    ENDIF
    
   ENDIF
  ENDDO
  IF(TZ.LE.0)IG=IG-1
 ENDDO

 END SUBROUTINE CREATEIZONE_MAIN
 
END MODULE MOD_CREATEIZONE
