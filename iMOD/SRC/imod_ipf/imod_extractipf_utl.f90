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
MODULE MOD_EXTRACTIPF_UTL

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_POLYGON_DRAW
USE MOD_POLYGON_UTL
USE MOD_POLYGON_PAR
USE MOD_IPF_PAR
USE MOD_IPF, ONLY : IPFDEALLOCATE

CONTAINS

 !###======================================================================
 SUBROUTINE EXTRACTIPF1CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IDIAGERROR=1

 IPF(1)%IP=0
 !NSEL=0 !SUM(IPF(1)%IP)
 CALL IPFDEALLOCATE()

 CALL POLYGON1DRAWSHAPE(1,SHP%NPOL)
 CALL POLYGON1CLOSE()

 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_EXTRACTIPF,2,0)

 CALL WDIALOGSELECT(ID_DEXTRACT)
 CALL WDIALOGUNLOAD()
 CALL WDIALOGSELECT(ID_DIPFINFOFIND)
 CALL WDIALOGUNLOAD()

 IDIAGERROR=0

! CALL IDFPLOT(1)

 END SUBROUTINE EXTRACTIPF1CLOSE

END MODULE MOD_EXTRACTIPF_UTL
