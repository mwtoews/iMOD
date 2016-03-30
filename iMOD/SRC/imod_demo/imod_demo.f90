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
MODULE MOD_DEMO

USE MOD_DEMO_PAR
USE MOD_PROFILE, ONLY : PROFILE_INIT
USE MOD_UTL, ONLY : UTL_CAP
USE MOD_3D, ONLY : IMOD3D_INIT

CONTAINS

 !###======================================================================
 SUBROUTINE DEMO_MAIN()
 !###======================================================================
 IMPLICIT NONE

 !# Starts cross-section tool if demo type name equals 'cross'
 IF(TRIM(UTL_CAP(DEMO%TDNAME,'U')).EQ.'CROSS')THEN
  CALL PROFILE_INIT()
  IF(DEMO%ISAVEBMP.EQ.0)CALL IDFPLOTFAST(0)
 ELSEIF(TRIM(UTL_CAP(DEMO%TDNAME,'U')).EQ.'3D')THEN
  CALL IMOD3D_INIT(0,0)
 ENDIF
 
 END SUBROUTINE DEMO_MAIN
 
END MODULE MOD_DEMO