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
MODULE MOD_GEOCONNECT_PAR

USE WINTERACTER
USE RESOURCE
USE MOD_UTL, ONLY : UTL_READINITFILE
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFNULLIFY,IDFCOPY,IDFDEALLOCATE,IDFDEALLOCATEX
!## IDF-types to read IDF-files from given folders
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: TOPM,BOTM,CIDF,KDHIDF,KDVIDF,KHIDF,KVIDF,KVAIDF,KVVIDF 
!## IDF-types for REGIS-files
TYPE(IDFOBJ),SAVE :: TOPR,BOTR,KHR,KVR
!## store availabel filenames
CHARACTER(LEN=256),DIMENSION(:),POINTER :: REGISFILES

INTEGER,ALLOCATABLE,DIMENSION(:) :: IACTM
INTEGER,SAVE :: NLAYM,NLAYR,IOPTW,IPESTNR,ISAVEK,ISAVEC !# NLAYM (model), NLAYR (Regis)
CHARACTER(LEN=256),SAVE :: OUTPUTFOLDER,REGISFOLDER,TOPFOLDER,BOTFOLDER,IPEST
!CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:) :: WC, FORMR !# FORMR now stored in CTYPE, this variable may not be needed??
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
 LOGICAL FUNCTION GC_INIT(IU)
 !###======================================================================
 !# subroutine to read all settings options for either use in iMOD-Batch or in iMOD-GUI
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: UNIT,I,J
 CHARACTER(LEN=256) :: LINE 

 GC_INIT=.FALSE.
  
 IF(.NOT.UTL_READINITFILE('IFLAG',LINE,IU,0))RETURN
 READ(LINE,*) GC_IFLAG; WRITE(*,'(A,I1)') 'IFLAG=',GC_IFLAG
 
 IF(.NOT.UTL_READINITFILE('NLAY',LINE,IU,0))RETURN
 READ(LINE,*) NLAYM; WRITE(*,'(A,I3)') 'NLAY=',NLAYM
 
 !## allocate arrays
 IF(.NOT.GC_ALLOCATE())RETURN
  
 IF(.NOT.UTL_READINITFILE('REGISFOLDER',LINE,IU,0))RETURN
 READ(LINE,*) REGISFOLDER; WRITE(*,'(A)') 'REGISFOLDER='//TRIM(REGISFOLDER)
 IF(.NOT.UTL_READINITFILE('TOPFOLDER',LINE,IU,0))RETURN
 READ(LINE,*) TOPFOLDER; WRITE(*,'(A)') 'TOPFOLDER='//TRIM(TOPFOLDER)
 IF(.NOT.UTL_READINITFILE('BOTFOLDER',LINE,IU,0))RETURN
 READ(LINE,*) BOTFOLDER; WRITE(*,'(A)') 'BOTFOLDER='//TRIM(BOTFOLDER)
   
 !## on default all layers are activated 
 IACTM=1; IF(UTL_READINITFILE('ACTLAYERS',LINE,IU,1))READ(LINE,*) IACTM
 WRITE(*,'(A,99I1)') 'ACTLAYERS=',IACTM
 
 !## call ini-files related to compute options
 !## preprocessing
 IF(GC_IFLAG.EQ.2)THEN 
  IF(.NOT.GC_INIT_PREPROCESSING(IU))RETURN
 !## postprocessing
 ELSEIF(GC_IFLAG.EQ.3)THEN 
  !CALL GC_POST_READ_INI(UNIT)
 ENDIF

 GC_INIT=.TRUE.

 END FUNCTION GC_INIT
 
 !###======================================================================
 LOGICAL FUNCTION GC_INIT_PREPROCESSING(IU)
 !###======================================================================
 !# subroutine to read all initial options for either use in iMOD-Batch or in iMOD-GUI 
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU !# Unit number of GEOCONNECT.ini
 CHARACTER(LEN=256) :: LINE
 
 GC_INIT_PREPROCESSING=.FALSE.
 
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

 GC_INIT_PREPROCESSING=.TRUE.

 END FUNCTION GC_INIT_PREPROCESSING
 
  !###======================================================================
 LOGICAL FUNCTION GC_ALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IOS
 
 GC_ALLOCATE=.FALSE.

 !## clean memory first
 CALL GC_DEALLOCATE()
 
 !## try to allocate all memory
 ALLOCATE(CIDF(NLAYM-1),KDHIDF(NLAYM),KDVIDF(NLAYM),TOPM(NLAYM),  &
          BOTM(NLAYM)  ,KVIDF(NLAYM) , & !,FFRAC(NLAY) ,CFRAC(NLAY)  &
          KHIDF(NLAYM) ,KVAIDF(NLAYM),KVVIDF(NLAYM),FVAL(NLAYM), &
          IACTM(NLAYM),STAT=IOS)

 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot allocate neccessary memory.','Error')
  RETURN
 ENDIF
 
 !## nullify idf-objects
 DO I=1,SIZE(CIDF);   CALL IDFNULLIFY(CIDF(I)); ENDDO 
 DO I=1,SIZE(KDHIDF); CALL IDFNULLIFY(KDHIDF(I)); ENDDO 
 DO I=1,SIZE(KDVIDF); CALL IDFNULLIFY(KDVIDF(I)); ENDDO
 DO I=1,SIZE(KHIDF);  CALL IDFNULLIFY(KHIDF(I)); ENDDO 
 DO I=1,SIZE(KVIDF);  CALL IDFNULLIFY(KVIDF(I)); ENDDO
 DO I=1,SIZE(KVVIDF); CALL IDFNULLIFY(KVVIDF(I)); ENDDO 
 DO I=1,SIZE(KVAIDF); CALL IDFNULLIFY(KVAIDF(I)); ENDDO 
! DO I=1,SIZE(FFRAC);  CALL IDFNULLIFY(FFRAC(I)); ENDDO
! DO I=1,SIZE(CFRAC);  CALL IDFNULLIFY(CFRAC(I)); ENDDO
 CALL IDFNULLIFY(KHR)
 CALL IDFNULLIFY(KVR)
 CALL IDFNULLIFY(TOPR)
 CALL IDFNULLIFY(BOTR)
 
 !## copy IDF settings
 DO I=1,SIZE(CIDF);   CALL IDFCOPY(TOPM(1),CIDF(I)); ENDDO 
 DO I=1,SIZE(KDHIDF); CALL IDFCOPY(TOPM(1),KDHIDF(I)); ENDDO 
 DO I=1,SIZE(KDVIDF); CALL IDFCOPY(TOPM(1),KDVIDF(I)); ENDDO 
 DO I=1,SIZE(KHIDF);  CALL IDFCOPY(TOPM(1),KHIDF(I)); ENDDO 
 DO I=1,SIZE(KVIDF);  CALL IDFCOPY(TOPM(1),KVIDF(I)); ENDDO
 DO I=1,SIZE(KVVIDF); CALL IDFCOPY(TOPM(1),KVVIDF(I)); ENDDO 
 DO I=1,SIZE(KVAIDF); CALL IDFCOPY(TOPM(1),KVAIDF(I)); ENDDO 
 CALL IDFCOPY(TOPM(1),TOPR)
 CALL IDFCOPY(TOPM(1),BOTR)
 CALL IDFCOPY(TOPM(1),KHR)
 CALL IDFCOPY(TOPM(1),KVR) 

! DO I=1,SIZE(FFRAC); CALL IDFCOPY(TOPM(1),FFRAC(I)); ENDDO 
! DO I=1,SIZE(CFRAC); CALL IDFCOPY(TOPM(1),CFRAC(I)); ENDDO  
   
 GC_ALLOCATE=.TRUE.
 
 END FUNCTION GC_ALLOCATE
 
 !###======================================================================
 SUBROUTINE GC_DEALLOCATE() 
 !###======================================================================
 !# subroutine to close all used files and deallocate allocated variables 
 IMPLICIT NONE
 
 !## deallocate
 IF(ALLOCATED(TOPM))THEN;   CALL IDFDEALLOCATE(TOPM,SIZE(TOPM)); DEALLOCATE(TOPM); ENDIF
 IF(ALLOCATED(BOTM))THEN;   CALL IDFDEALLOCATE(BOTM,SIZE(BOTM)); DEALLOCATE(BOTM); ENDIF
 IF(ALLOCATED(CIDF))THEN;   CALL IDFDEALLOCATE(CIDF ,SIZE(CIDF)); DEALLOCATE(CIDF);   ENDIF
 IF(ALLOCATED(KDHIDF))THEN; CALL IDFDEALLOCATE(KDHIDF,SIZE(KDHIDF)); DEALLOCATE(KDHIDF);  ENDIF
 IF(ALLOCATED(KDVIDF))THEN; CALL IDFDEALLOCATE(KDVIDF,SIZE(KDVIDF)); DEALLOCATE(KDVIDF);  ENDIF
 IF(ALLOCATED(KHIDF))THEN;  CALL IDFDEALLOCATE(KHIDF,SIZE(KHIDF)); DEALLOCATE(KHIDF);  ENDIF
 IF(ALLOCATED(KVIDF))THEN;  CALL IDFDEALLOCATE(KVIDF,SIZE(KVIDF)); DEALLOCATE(KVIDF);  ENDIF
 IF(ALLOCATED(KVAIDF))THEN; CALL IDFDEALLOCATE(KVAIDF,SIZE(KVAIDF)); DEALLOCATE(KVAIDF);  ENDIF
 IF(ALLOCATED(KVVIDF))THEN; CALL IDFDEALLOCATE(KVVIDF,SIZE(KVVIDF)); DEALLOCATE(KVVIDF);  ENDIF 
 CALL IDFDEALLOCATEX(KHR)
 CALL IDFDEALLOCATEX(KVR)
 CALL IDFDEALLOCATEX(TOPR)
 CALL IDFDEALLOCATEX(BOTR)
! IF(ALLOCATED(FFRAC))THEN; CALL IDFDEALLOCATE(FFRAC,SIZE(FFRAC)); DEALLOCATE(FFRAC); ENDIF 
! IF(ALLOCATED(CFRAC))THEN; CALL IDFDEALLOCATE(CFRAC,SIZE(CFRAC)); DEALLOCATE(CFRAC); ENDIF 
 IF(ALLOCATED(FVAL))THEN; DEALLOCATE(FVAL); ENDIF
 IF(ALLOCATED(IACTM))THEN; DEALLOCATE(IACTM); ENDIF
 
 IF(ASSOCIATED(REGISFILES))DEALLOCATE(REGISFILES)

 END SUBROUTINE GC_DEALLOCATE 
 
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