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

MODULE MOD_GEOCONNECT_PAR

USE WINTERACTER
USE RESOURCE

!#### Define global variables: ####
!# NLAYM (model), NLAYR (Regis)
!# IDF-types to read IDF-files from given folders: TOPM(.), BOTM(.), IACTM(.), FORMATION(.), FFORMATION(.)

CONTAINS

 !###======================================================================
 SUBROUTINE GC_READ_SETTINGS()
 !###======================================================================
 !# subroutine to read all settings options for either use in iMOD-Batch or in iMOD-GUI
 IMPLICIT NONE

 

 END SUBROUTINE GC_READ_SETTINGS
 
 !###======================================================================
 SUBROUTINE GC_READ_PRE()
 !###======================================================================
 !# subroutine to read all preprocessing options for either use in iMOD-Batch or in iMOD-GUI 
 IMPLICIT NONE

 

 END SUBROUTINE GC_READ_PRE
 
 !###======================================================================
 SUBROUTINE GC_WRITE_SETTINGS()
 !###======================================================================
 !# subroutine to write all settings options into a *.txt file 
 IMPLICIT NONE

 

 END SUBROUTINE GC_WRITE_SETTINGS
 
 !###======================================================================
 SUBROUTINE GC_WRITE_PRE()
 !###======================================================================
 !# subroutine to write all preprocessing options into a *.ini file 
 IMPLICIT NONE

 

 END SUBROUTINE GC_WRITE_PRE
 
END MODULE MOD_GEOCONNECT_PAR