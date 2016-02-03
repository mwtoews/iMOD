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
USE MOD_IDF_PAR, ONLY : IDFOBJ

!#### Define global variables: ####
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: TOPM,BOTM,FFRAC,TBR,KHR,KVR !# IDF-types to read IDF-files from given folders: TOPM(.), BOTM(.), IACTM(.), FORMATION(.), FFORMATION(.)
INTEGER,ALLOCATABLE,DIMENSION(:) :: IACTM
INTEGER,SAVE :: NLAYM,NLAYR,IOPTW !# NLAYM (model), NLAYR (Regis)
CHARACTER(LEN=256),SAVE :: OUTPUTFOLDER,FRACTIONFOLDER
CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:) :: FORMR !# FORMR now stored in CTYPE, this variable may not be needed??
CHARACTER(LEN=256),DIMENSION(:),POINTER,PRIVATE :: REGISTOP,REGISBOT,REGISKHV,REGISKVV

TYPE FRMOBJ
 CHARACTER(LEN=12) :: FORM
 REAL :: FACT
END TYPE
TYPE(FRMOBJ),ALLOCATABLE,DIMENSION(:) :: IPFAC

CONTAINS

 !###======================================================================
 SUBROUTINE GC_READ_SETTINGS()
 !###======================================================================
 !# subroutine to read all settings options for either use in iMOD-Batch or in iMOD-GUI
 IMPLICIT NONE

 IOPTW= !#Read write option from Settingsfile or window checkboxes
 
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