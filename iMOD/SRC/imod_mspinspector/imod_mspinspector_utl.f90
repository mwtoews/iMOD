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
MODULE MOD_MSPINSPECTOR_UTL

USE WINTERACTER
USE RESOURCE
USE MOD_IDF, ONLY : IDFDEALLOCATEX
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_MSPINSPECTOR_PAR

CONTAINS
 
 !###======================================================================
 SUBROUTINE MSPINSPECTOR_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE

 IF(ASSOCIATED(DXC%INFO)) DEALLOCATE(DXC%INFO)
 IF(ASSOCIATED(DXC%LABEL))DEALLOCATE(DXC%LABEL)
 
 IF(ASSOCIATED(MODSVAT%INFO)) DEALLOCATE(MODSVAT%INFO)
 IF(ASSOCIATED(MODSVAT%LABEL))DEALLOCATE(MODSVAT%LABEL)

 IF(ASSOCIATED(IDFSVAT%INFO)) DEALLOCATE(IDFSVAT%INFO)
 IF(ASSOCIATED(IDFSVAT%LABEL))DEALLOCATE(IDFSVAT%LABEL)

 IF(ASSOCIATED(AREASVAT%INFO)) DEALLOCATE(AREASVAT%INFO)
 IF(ASSOCIATED(AREASVAT%LABEL))DEALLOCATE(AREASVAT%LABEL)

 CALL IDFDEALLOCATEX(MSPIDF)
 
 END SUBROUTINE MSPINSPECTOR_DEALLOCATE

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IDIAGERROR=1
 
 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_MSPANALYSER,2,0)

 CALL MSPINSPECTOR_DEALLOCATE()
 CALL WDIALOGSELECT(ID_DMSPANALYSER); CALL WDIALOGUNLOAD()

 IDIAGERROR=0

 END SUBROUTINE MSPINSPECTOR_CLOSE
  
END MODULE MOD_MSPINSPECTOR_UTL
