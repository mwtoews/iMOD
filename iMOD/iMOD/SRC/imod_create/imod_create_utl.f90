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
MODULE MOD_CREATE_UTL

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_POLYGON_DRAW
USE MOD_POLYGON_UTL
USE MOD_POLYGON_PAR
USE MOD_CREATEIDF_PAR

 CONTAINS
 
! !###======================================================================
! SUBROUTINE CREATEIPF1CLOSE()
! !###======================================================================
! IMPLICIT NONE

! IDIAGERROR=1

! CALL POLYGON1DRAWSHAPE(1,SHP%NPOL); CALL POLYGON1CLOSE()
! CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_CREATEIPF,2,0)
! CALL WDIALOGSELECT(ID_DCREATEIPF); CALL WDIALOGUNLOAD()

! IDIAGERROR=0

! END SUBROUTINE CREATEIPF1CLOSE 
 
 !###======================================================================
 SUBROUTINE CREATEIDF1CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IDIAGERROR=1

 CALL POLYGON1DRAWSHAPE(1,SHP%NPOL)
 CALL POLYGON1CLOSE()

 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(TOOLSID,2,0)
 CALL WDIALOGSELECT(ID_DCREATEIDF); CALL WDIALOGUNLOAD()
 IDIAGERROR=0

 END SUBROUTINE CREATEIDF1CLOSE
 
 !###======================================================================
 SUBROUTINE CREATEGEN1CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IDIAGERROR=1

 CALL POLYGON1DRAWSHAPE(1,SHP%NPOL); CALL POLYGON1CLOSE()
 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_CREATEGEN,2,0)
 CALL WDIALOGSELECT(ID_DCREATEGEN); CALL WDIALOGUNLOAD()

 IDIAGERROR=0

 END SUBROUTINE CREATEGEN1CLOSE
 
END MODULE MOD_CREATE_UTL