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
USE MOD_BATCH_PAR
USE MOD_IPF_PAR
USE MOD_IPF, ONLY : IPFREAD2,IPFALLOCATE,IPFDEALLOCATE
USE MOD_IPFASSFILE,ONLY : IPFOPENASSFILE,IPFREADASSFILELABEL,IPFREADASSFILE,IPFASSFILEALLOCATE
USE MOD_TSTAT_PAR

CONTAINS

 !###======================================================================
 SUBROUTINE FLUMY_MAIN()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU,I,J,K,X,Y,NIPF,ACOL
 CHARACTER(LEN=256) :: FNAME,IPFNAME,DIR
 REAL :: ZTOP,ZBOT
 
 NIPF=1; CALL IPFALLOCATE()
 IPF(1)%XCOL =1 !## x
 IPF(1)%YCOL =2 !## y
 IPF(1)%QCOL =1 !## q not used
 IPF(1)%ZCOL =1 !## z not used
 IPF(1)%Z2COL=1 !## z2 not used  
 IPF(1)%FNAME=IPFNAME
 
 !# read entire ipf
 IF(.NOT.IPFREAD2(1,1,1))RETURN

 !## store each drill in memory for picking purposes
 CALL IPFASSFILEALLOCATE(1)
 
 !## Read all lines from IPF-file and open related associated file
 DO I=1,IPF(1)%NROW
  X=IPF(1)%XYZ(1,I) !# x-coordinate borehole
  Y=IPF(1)%XYZ(2,I) !# y-coordinate borehole
  ACOL=IPF(1)%ACOL !# column with borehole information
  
  !## read associated file
  J=INDEX(IPF(1)%FNAME,'\',.TRUE.)
  DIR=IPF(1)%FNAME(1:J-1)

  FNAME=TRIM(DIR)//'\'//TRIM(IPF(1)%INFO(ACOL,K))//'.'//TRIM(ADJUSTL(IPF(1)%FEXT))
  !## read dimensions of associated file
  IF(IPFOPENASSFILE(IU,1,FNAME))THEN
   !## drill found
   IF(ASSF(1)%ITOPIC.EQ.2)THEN
    IF(IPFREADASSFILELABEL(IU,1,FNAME).AND.  &
     IPFREADASSFILE(IU,1,FNAME))THEN
     ZTOP=ASSF(1)%Z(1) !# top level of borehole
     ZBOT=ASSF(1)%Z(ASSF(1)%NRASS) !# bot level of borehole 
     DO K=1,ASSF(1)%NRASS !loop over rows associated file
      !ASSF(1)%L(:,K) !#search for lithology class in data string, and compare with GRAIN-variable in batch-file.
      !# make type (in iMOD-batch) which contains the grain/faciesl/faciesn parameters, zodat de betreffende kolom direct gevuld kan worden met de juiste waarden gekoppeld aan de gevonden lithologie.
     ENDDO
    ENDIF
   ENDIF
  ENDIF
     
   !## WEGSCHRIJVEN NAAR TXT FILE
!     IPF(1)%INFO(:,J)=IPF(1)%INFO(:,I)

 ENDDO
   
  CALL IPFDEALLOCATE()
 
 END SUBROUTINE FLUMY_MAIN

END MODULE MOD_BATCH_FLUMY
