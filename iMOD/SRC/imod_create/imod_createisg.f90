!!  Copyright (C) Stichting Deltares, 2005-2020.
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
MODULE MOD_CREATEISG

USE RESOURCE
USE MOD_ISG_UTL, ONLY : ISGSAVE
USE MOD_ISG_PAR, ONLY : NISG,ISFR,ISGDOUBLE
USE MOD_ISG, ONLY : ISGEDITINIT
USE MOD_IDFPLOT
USE MOD_MANAGER_UTL

CONTAINS

 !###======================================================================
 SUBROUTINE CREATEISG1INIT(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 CHARACTER(LEN=256) :: ISGFILE
 
 IF(ID.EQ.ID_ISGGEN_CREATEISGRIV)ISFR=0
 IF(ID.EQ.ID_ISGGEN_CREATEISGSFR)ISFR=1
 NISG=0; ISGDOUBLE=4; CALL ISGSAVE(ISGFILE,1)
 IF(ISGFILE.EQ.'')RETURN
 
 CALL MANAGER_UTL_ADDFILE(ISGFILE)
 CALL IDFPLOTFAST(0)
 CALL ISGEDITINIT()
 
 END SUBROUTINE CREATEISG1INIT

END MODULE MOD_CREATEISG
