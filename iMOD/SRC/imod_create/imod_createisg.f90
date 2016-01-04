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

MODULE MOD_CREATEISG

USE MOD_ISG_UTL, ONLY : ISGSAVE
USE MOD_ISG_PAR, ONLY : NISG,ISGFNAME
USE MOD_ISG, ONLY : ISGEDITINIT
USE IMOD

CONTAINS

 !###======================================================================
 SUBROUTINE CREATEISG1INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ISAVE
 
 NISG=0; ISAVE=2
 CALL ISGSAVE(ISAVE,0)
 IF(ISAVE.EQ.0)RETURN
 
 CALL IDFINIT(ISGFNAME,LPLOT=.FALSE.)
 CALL ISGEDITINIT()
 
 END SUBROUTINE CREATEISG1INIT

END MODULE MOD_CREATEISG
