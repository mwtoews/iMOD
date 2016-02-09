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
USE MOD_UTL, ONLY : UTL_READINITFILE,UTL_GETUNIT,UTL_SUBST,ITOS,UTL_DIRINFO_POINTER,UTL_CAP
USE MOD_IDF, ONLY : IDFGETXYVAL,IDFREADSCALE,IDFWRITE,IDFREAD

!#### Define global variables: ####
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: TOPM,BOTM,FFRAC,CFRAC,TBR,KHR,KVR,CIDF,KDHIDF,KDVIDF,KHIDF,KVIDF,KVAIDF,KVVIDF !# IDF-types to read IDF-files from given folders
INTEGER,ALLOCATABLE,DIMENSION(:) :: IACTM
INTEGER,SAVE :: NLAYM,NLAYR,IOPTW,IPESTNR,ISAVEK,ISAVEC !# NLAYM (model), NLAYR (Regis)
CHARACTER(LEN=256),SAVE :: OUTPUTFOLDER,REGISFOLDER,TOPFOLDER,BOTFOLDER,IPEST
CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:) :: WC, FORMR !# FORMR now stored in CTYPE, this variable may not be needed??
CHARACTER(LEN=256),DIMENSION(:),POINTER :: TOPR,BOTR,KHVR,KVVR
INTEGER :: GC_IFLAG !# IFLAG related to GC computation options 1=identify, 2=preprocessing, 3=postprocessing
REAL :: X_ID,Y_ID !#X and Y coordinates read from ini-file or window
REAL,ALLOCATABLE,DIMENSION(:) :: FVAL

TYPE FRMOBJ
 CHARACTER(LEN=12) :: FORM
 REAL :: FACT
END TYPE
TYPE(FRMOBJ),ALLOCATABLE,DIMENSION(:) :: IPFAC

CONTAINS

 !###======================================================================
 SUBROUTINE GC_INIT(IU,IFLAG)
 !###======================================================================
 !# subroutine to read all settings options for either use in iMOD-Batch or in iMOD-GUI
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER,INTENT(OUT) :: IFLAG !# Unit number of GEOCONNECT.txt
 INTEGER :: UNIT,I,J
 CHARACTER(LEN=256) :: LINE 
 
 GC_IFLAG=0
 NLAYM=0
 REGISFOLDER=''
 TOPFOLDER=''
 BOTFOLDER=''
 
 IF(.NOT.UTL_READINITFILE('IFLAG',LINE,IU,0))RETURN
 READ(LINE,*) GC_IFLAG; WRITE(*,'(A,I1)') 'IFLAG=',GC_IFLAG
 IFLAG=GC_IFLAG
 
 IF(.NOT.UTL_READINITFILE('NLAY',LINE,IU,0))RETURN
 READ(LINE,*) NLAYM; WRITE(*,'(A,I3)') 'NLAY=',NLAYM
 IF(.NOT.UTL_READINITFILE('REGISFOLDER',LINE,IU,0))RETURN
 READ(LINE,*) REGISFOLDER; WRITE(*,'(A,A)') 'REGISFOLDER=',REGISFOLDER
 IF(.NOT.UTL_READINITFILE('TOPFOLDER',LINE,IU,0))RETURN
 READ(LINE,*) TOPFOLDER; WRITE(*,'(A,A)') 'TOPFOLDER=',TOPFOLDER
 IF(.NOT.UTL_READINITFILE('BOTFOLDER',LINE,IU,0))RETURN
 READ(LINE,*) BOTFOLDER; WRITE(*,'(A,A)') 'BOTFOLDER=',BOTFOLDER
 
 ALLOCATE(IACTM(NLAYM))
 DO I=1,NLAYM; IACTM(I)=1; ENDDO !#on default all layers are activated
 
 IF(UTL_READINITFILE('ACTLAYERS',LINE,IU,1))THEN
  READ(LINE,*) IACTM; WRITE(*,*) 'ACTLAYERS=',IACTM
 ENDIF
 
 !# Call ini-files related to compute options
 IF(IFLAG.EQ.2)THEN !#Preprocessing
  CALL GC_PRE_READ_INI(UNIT)
 ELSEIF(IFLAG.EQ.3)THEN !#Postprocessing
  !CALL GC_POST_READ_INI(UNIT)
 ENDIF
 
 END SUBROUTINE GC_INIT
 
 !###======================================================================
 SUBROUTINE GC_PRE_READ_INI(IU)
 !###======================================================================
 !# subroutine to read all initial options for either use in iMOD-Batch or in iMOD-GUI 
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU !# Unit number of GEOCONNECT.ini
 CHARACTER(LEN=256) :: LINE
 
 IPEST=''
 IPESTNR=0
 ISAVEK=1
 ISAVEC=0
 OUTPUTFOLDER=''
 
 IF(UTL_READINITFILE('IPEST',LINE,IU,1))THEN
  READ(LINE,*) IPEST; WRITE(*,'(A)') 'IPEST=',IPEST
 ENDIF
 IF(TRIM(IPEST).NE.''.AND.UTL_READINITFILE('IPESTNR',LINE,IU,1))THEN
  READ(LINE,*) IPESTNR; WRITE(*,'(I)') 'IPESTNR=',IPESTNR
 ENDIF 
 IF(UTL_READINITFILE('ISAVEK',LINE,IU,1))THEN
  READ(LINE,*) ISAVEK; WRITE(*,'(I)') 'ISAVEK=',ISAVEK
 ENDIF
 IF(UTL_READINITFILE('ISAVEC',LINE,IU,1))THEN
  READ(LINE,*) ISAVEC; WRITE(*,'(I)') 'ISAVEC=',ISAVEC
 ENDIF 

 IF(.NOT.UTL_READINITFILE('OUTPUTFOLDER',LINE,IU,0))RETURN
 READ(LINE,*) OUTPUTFOLDER; WRITE(*,'(A,A)') 'OUTPUTFOLDER=',OUTPUTFOLDER

 END SUBROUTINE GC_PRE_READ_INI
 
 !###======================================================================
 SUBROUTINE GC_CLOSE_DEALLOC(IU)
 !###======================================================================
 !# subroutine to close all used files and deallocate allocated variables 
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 
 IF(IU.NE.0)CLOSE(IU)
 
 !## deallocate
 IF(ALLOCATED(TOPM))THEN; CALL IDFDEALLOCATE(TOPM,SIZE(TOPM)); DEALLOCATE(TOPM); ENDIF
 IF(ALLOCATED(BOTM))THEN; CALL IDFDEALLOCATE(BOTM,SIZE(BOTM)); DEALLOCATE(BOTM); ENDIF
 IF(ALLOCATED(CIDF))THEN; CALL IDFDEALLOCATE(CIDF ,SIZE(CIDF)); DEALLOCATE(CIDF);   ENDIF
 IF(ALLOCATED(KDHIDF))THEN; CALL IDFDEALLOCATE(KDHIDF,SIZE(KDHIDF)); DEALLOCATE(KDHIDF);  ENDIF
 IF(ALLOCATED(KDVIDF))THEN; CALL IDFDEALLOCATE(KDVIDF,SIZE(KDVIDF)); DEALLOCATE(KDVIDF);  ENDIF
 IF(ALLOCATED(KHIDF))THEN; CALL IDFDEALLOCATE(KHIDF,SIZE(KHIDF)); DEALLOCATE(KHIDF);  ENDIF
 IF(ALLOCATED(KVIDF))THEN; CALL IDFDEALLOCATE(KVIDF,SIZE(KVIDF)); DEALLOCATE(KVIDF);  ENDIF
 IF(ALLOCATED(KVAIDF))THEN; CALL IDFDEALLOCATE(KVAIDF,SIZE(KVAIDF)); DEALLOCATE(KVAIDF);  ENDIF
 IF(ALLOCATED(KVVIDF))THEN; CALL IDFDEALLOCATE(KVVIDF,SIZE(KVVIDF)); DEALLOCATE(KVVIDF);  ENDIF 
 IF(ALLOCATED(KHR))THEN; CALL IDFDEALLOCATE(KHR,SIZE(KHR)); DEALLOCATE(KHR); ENDIF
 IF(ALLOCATED(KVR))THEN; CALL IDFDEALLOCATE(KVR,SIZE(KVR)); DEALLOCATE(KVR); ENDIF
 IF(ALLOCATED(TBR))THEN; CALL IDFDEALLOCATE(TBR,SIZE(TBR)); DEALLOCATE(TBR); ENDIF 
 IF(ALLOCATED(FFRAC))THEN; CALL IDFDEALLOCATE(FFRAC,SIZE(FFRAC)); DEALLOCATE(FFRAC); ENDIF 
 IF(ALLOCATED(CFRAC))THEN; CALL IDFDEALLOCATE(CFRAC,SIZE(CFRAC)); DEALLOCATE(CFRAC); ENDIF 
 IF(ALLOCATED(FVAL))THEN; DEALLOCATE(FVAL); ENDIF
 IF(ALLOCATED(IACTM))THEN; DEALLOCATE(IACTM); ENDIF
 
 END SUBROUTINE GC_CLOSE_DEALLOC 
 
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