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
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: TOPM,BOTM,FFRAC !,TBR,KHR,KVR !# IDF-types to read IDF-files from given folders: TOPM(.), BOTM(.), IACTM(.), FORMATION(.), FFORMATION(.)
INTEGER,ALLOCATABLE,DIMENSION(:) :: IACTM
INTEGER,SAVE :: NLAYM,NLAYR,IOPTW !# NLAYM (model), NLAYR (Regis)
CHARACTER(LEN=256),SAVE :: OUTPUTFOLDER,FRACTIONFOLDER
CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:) :: FORMR !# FORMR now stored in CTYPE, this variable may not be needed??
CHARACTER(LEN=256),DIMENSION(:),POINTER,PRIVATE :: TOPR,BOTR,KHVR,KVVR
INTEGER :: GC_IFLAG !# IFLAG related to GC computation options 1=identify, 2=preprocessing, 3=postprocessing

TYPE FRMOBJ
 CHARACTER(LEN=12) :: FORM
 REAL :: FACT
END TYPE
TYPE(FRMOBJ),ALLOCATABLE,DIMENSION(:) :: IPFAC

CONTAINS

 !###======================================================================
 SUBROUTINE GC_INIT(UNIT,IFLAG)
 !###======================================================================
 !# subroutine to read all settings options for either use in iMOD-Batch or in iMOD-GUI
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: UNIT !# Unit number of GEOCONNECT.txt or GEOCONNECT.ini 
 INTEGER,INTENT(OUT) :: IFLAG !# IFLAG read from ini-file, related to type of computation
  
 IOPTW= !#Read write option from Settingsfile or window checkboxes
 
 END SUBROUTINE GC_READ_INIT
 
 !###======================================================================
 SUBROUTINE GC_READ_INI()
 !###======================================================================
 !# subroutine to read all preprocessing options for either use in iMOD-Batch or in iMOD-GUI 
 IMPLICIT NONE
 INTEGER :: IU !# Unit number of GEOCONNECT.txt or GEOCONNECT.ini

 CALL GC_READ_INIT(IU)

 END SUBROUTINE GC_READ_INI
 
 !###======================================================================
 SUBROUTINE GC_CLOSE()
 !###======================================================================
 !# subroutine to close all used files and deallocate allocated variables 
 IMPLICIT NONE

 END SUBROUTINE GC_CLOSE 
 
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