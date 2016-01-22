!!  Copyright (C) Stichting Deltares, 2005-2016.
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
MODULE MOD_BATCH_FLUMY

USE WINTERACTER
USE RESOURCE
USE MOD_IPF_PAR
USE MOD_IPF, ONLY : IPFREAD2,IPFALLOCATE,IPFDEALLOCATE
USE MOD_IPFASSFILE,ONLY : IPFOPENASSFILE,IPFREADASSFILELABEL,IPFREADASSFILE,IPFASSFILEALLOCATE
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_UTL, ONLY : UTL_CAP,UTL_GETUNIT,UTL_CREATEDIR

TYPE FLMOBJ
 CHARACTER(LEN=12) :: GRAIN
 CHARACTER(LEN=2) :: FACL
 INTEGER :: FACN
END TYPE FLMOBJ
TYPE(FLMOBJ),ALLOCATABLE,DIMENSION(:) :: FLM

CONTAINS

 !###======================================================================
 SUBROUTINE FLUMY_MAIN(IPFFNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IPFFNAME
 INTEGER :: IU,JU,I,J,K,M,X,Y,IOS
 CHARACTER(LEN=256) :: FNAME,DIR
 
 NIPF=1; CALL IPFALLOCATE()
 IPF(1)%XCOL =1 !## x
 IPF(1)%YCOL =2 !## y
 IPF(1)%QCOL =1 !## q not used
 IPF(1)%ZCOL =1 !## z not used
 IPF(1)%Z2COL=1 !## z2 not used  
 IPF(1)%FNAME=IPFFNAME
 
 !# read entire ipf
 IF(.NOT.IPFREAD2(1,1,1))RETURN

 !## store each drill in memory for picking purposes
 CALL IPFASSFILEALLOCATE(1)
 
 !## create flumy folder
 J=INDEX(IPF(1)%FNAME,'\',.TRUE.)
 DIR=IPF(1)%FNAME(1:J-1)

 !## Read all lines from IPF-file and open related associated file
 DO I=1,IPF(1)%NROW
  X=IPF(1)%XYZ(1,I) !# x-coordinate borehole
  Y=IPF(1)%XYZ(2,I) !# y-coordinate borehole

  !## read associated file 
  FNAME=TRIM(DIR)//'\'//TRIM(IPF(1)%INFO(IPF(1)%ACOL,I))//'.'//TRIM(ADJUSTL(IPF(1)%FEXT))
  !## read dimensions of associated file
  IF(IPFOPENASSFILE(IU,1,FNAME))THEN
   !## drill found   
   IF(ASSF(1)%ITOPIC.EQ.2)THEN
    IF(IPFREADASSFILELABEL(IU,1,FNAME).AND.IPFREADASSFILE(IU,1,FNAME))THEN

     !## write data to txt-file in flumy-format
     FNAME=TRIM(DIR)//'\FLUMY\'//TRIM(IPF(1)%INFO(IPF(1)%ACOL,I))//'_flumy.txt'
     CALL UTL_CREATEDIR(FNAME(1:INDEX(FNAME,'\',.TRUE.)-1))
     JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=FNAME,STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
     IF(JU.NE.0)THEN
      WRITE(JU,*) '# ====================================================================='
      WRITE(JU,*) '# Well '//TRIM(IPF(1)%INFO(IPF(1)%ACOL,I))
      WRITE(JU,*) '#'
      WRITE(JU,*) '# Coordinates, depth and thickness are expressed in meters'
      WRITE(JU,*) '# ====================================================================='
      WRITE(JU,*) '# Well Location'
      WRITE(JU,*) 'X_WELL=',X
      WRITE(JU,*) 'Y_WELL=',Y
      WRITE(JU,*) '#'
      WRITE(JU,*) '# Bottom elevation'
      WRITE(JU,*) 'Z_BOTTOM=',ASSF(1)%Z(1)  
      WRITE(JU,*) '# Top elevation'
      WRITE(JU,*) 'Z_TOP=',ASSF(1)%Z(ASSF(1)%NRASS)
      WRITE(JU,*) '#'
      WRITE(JU,*) '# Deposits From top to bottom'
      WRITE(JU,*) '# Facies_id Facies Depth'
      WRITE(JU,*) '#   Warning : Depth from top of deposit basis'
      WRITE(JU,*) 'ATTRIBUTE_COLUMN=1'
      WRITE(JU,*) 'DEPTH_COLUMN=3'
      WRITE(JU,*) 'DISCRETE_ATTRIBUTE=1'
      WRITE(JU,*) 'STANDARD_FACIES=1'
      WRITE(JU,*) '~Ascii' 
     
      !#search for lithology class in data string, and compare with GRAIN-variable in batch-file.
      DO K=1,ASSF(1)%NRASS !loop over rows associated file
       DO J=1,SIZE(FLM)
        IF(TRIM(UTL_CAP(ASSF(1)%L(4,K),'U')).EQ.TRIM(FLM(J)%GRAIN))EXIT
       ENDDO
       !## grain found
       IF(J.LE.SIZE(FLM))THEN
!        WRITE(JU,*) FACN(K),TRIM(UTL_CAP(FACL(K),'U')),ASSF(1)%Z(K)
       ENDIF

      ENDDO
      CLOSE(JU)
     ELSE
!@## error message want imod kan bestand niet maken !
     ENDIF   
    ENDIF
   ENDIF
  ENDIF
  
 ENDDO

 CALL IPFDEALLOCATE()
 
 END SUBROUTINE FLUMY_MAIN

END MODULE MOD_BATCH_FLUMY
