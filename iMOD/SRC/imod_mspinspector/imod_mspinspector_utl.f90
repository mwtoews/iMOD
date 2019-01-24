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
 SUBROUTINE MSPINSPECTOR_ALLOCATE()
 !###======================================================================
 IMPLICIT NONE

 END SUBROUTINE MSPINSPECTOR_ALLOCATE
 
 !###======================================================================
 LOGICAL FUNCTION MSPINSPECTOR_ALLOCATE_DXC()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS
 
 MSPINSPECTOR_ALLOCATE_DXC=.FALSE.
 
 NULLIFY(DXC%ILAY); ALLOCATE(DXC%ILAY(DXC%MXID),STAT=IOS); IF(IOS.NE.0)RETURN
 NULLIFY(DXC%IROW); ALLOCATE(DXC%IROW(DXC%MXID),STAT=IOS); IF(IOS.NE.0)RETURN
 NULLIFY(DXC%ICOL); ALLOCATE(DXC%ICOL(DXC%MXID),STAT=IOS); IF(IOS.NE.0)RETURN
 NULLIFY(DXC%ID);   ALLOCATE(DXC%ID(DXC%MXID)  ,STAT=IOS); IF(IOS.NE.0)RETURN
 
 MSPINSPECTOR_ALLOCATE_DXC=.TRUE.
 
 END FUNCTION MSPINSPECTOR_ALLOCATE_DXC

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE

 CALL MSPINSPECTOR_DEALLOCATE_DXC()
 CALL IDFDEALLOCATEX(MSPIDF)
 
 END SUBROUTINE MSPINSPECTOR_DEALLOCATE

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_DEALLOCATE_DXC()
 !###======================================================================
 IMPLICIT NONE

 IF(ASSOCIATED(DXC%ILAY))DEALLOCATE(DXC%ILAY)
 IF(ASSOCIATED(DXC%IROW))DEALLOCATE(DXC%IROW)
 IF(ASSOCIATED(DXC%ICOL))DEALLOCATE(DXC%ICOL)
 IF(ASSOCIATED(DXC%ID))  DEALLOCATE(DXC%ID)
 DXC%MXID=0
 
 END SUBROUTINE MSPINSPECTOR_DEALLOCATE_DXC

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
