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
USE MOD_UTL, ONLY : UTL_READINITFILE,UTL_GETUNIT
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFNULLIFY,IDFCOPY,IDFDEALLOCATE,IDFDEALLOCATEX
USE MOD_OSD, ONLY : OSD_OPEN
!## IDF-types to read IDF-files from given folders
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: TOPM,BOTM,CIDF,KDHIDF,KDVIDF,KHIDF,KVIDF,KVAIDF,KVVIDF 
!## IDF-types for REGIS-files
TYPE(IDFOBJ),SAVE :: TOPR,BOTR,KHR,KVR
!## store availabel filenames
CHARACTER(LEN=256),DIMENSION(:),POINTER :: REGISFILES

INTEGER,ALLOCATABLE,DIMENSION(:) :: IACTM
INTEGER,SAVE :: NLAYM,NLAYR,IOPTW,IPESTNR !# NLAYM (model), NLAYR (Regis)
CHARACTER(LEN=256),SAVE :: OUTPUTFOLDER,REGISFOLDER,TOPFOLDER,BOTFOLDER,IPEST,TXTFILE
INTEGER :: GC_IFLAG,ISAVEK,ISAVEC !# IFLAG related to GC computation options 1=identify, 2=preprocessing, 3=postprocessing
REAL :: X_ID,Y_ID !#X and Y coordinates read from ini-file or window
REAL,ALLOCATABLE,DIMENSION(:) :: FVAL

TYPE FRMOBJ
 CHARACTER(LEN=12) :: FORM
 REAL :: FACT
END TYPE
TYPE(FRMOBJ),ALLOCATABLE,DIMENSION(:) :: IPFAC

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION GC_INIT()
 !###======================================================================
 !# subroutine to read all settings options for either use in iMOD-Batch or in iMOD-GUI
 IMPLICIT NONE
 INTEGER :: IU,I,J,IOS
 CHARACTER(LEN=256) :: LINE 

 GC_INIT=.FALSE.
 
 !## read *.txt-file
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(TXTFILE),STATUS='OLD',FORM='FORMATTED',ACTION='READ',IOSTAT=IOS)
 IF(IOS.NE.0)RETURN
 
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
 IACTM=1; IF(UTL_READINITFILE('ACTLAYERS',LINE,IU,1))READ(LINE,'(99I1)') (IACTM(I),I=1,NLAYM)
 WRITE(*,'(A,99I1)') 'ACTLAYERS=',IACTM
  
 GC_INIT=.TRUE.

 CLOSE(IU)

 END FUNCTION GC_INIT
 
 !###======================================================================
 LOGICAL FUNCTION GC_INIT_PREPROCESSING(IU)
 !###======================================================================
 !# subroutine to read all initial options for either use in iMOD-Batch or in iMOD-GUI 
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU !# Unit number of GEOCONNECT.ini
 INTEGER :: IOS
 CHARACTER(LEN=256) :: LINE
 
 GC_INIT_PREPROCESSING=.FALSE.
 
 IPEST=''
 OUTPUTFOLDER=''
 IPESTNR=0
 ISAVEK=1
 ISAVEC=0 
 
 IF(UTL_READINITFILE('IPEST',LINE,IU,1))THEN
  READ(LINE,*) IPEST; WRITE(*,'(A)') 'IPEST='//TRIM(IPEST)
 ENDIF
 IF(TRIM(IPEST).NE.''.AND.UTL_READINITFILE('IPESTNR',LINE,IU,1))THEN
  READ(LINE,*) IPESTNR; WRITE(*,'(A8,I1)') 'IPESTNR=',IPESTNR
 ENDIF 
 IF(UTL_READINITFILE('ISAVEK',LINE,IU,1))THEN
  READ(LINE,*) ISAVEK; WRITE(*,'(A7,I1)') 'ISAVEK=',ISAVEK
 ENDIF
 IF(UTL_READINITFILE('ISAVEC',LINE,IU,1))THEN
  READ(LINE,*) ISAVEC; WRITE(*,'(A7,I1)') 'ISAVEC=',ISAVEC
 ENDIF 

 IF(.NOT.UTL_READINITFILE('OUTPUTFOLDER',LINE,IU,0))RETURN
 READ(LINE,*) OUTPUTFOLDER; WRITE(*,'(A)') 'OUTPUTFOLDER='//TRIM(OUTPUTFOLDER)

 !## read name of *.txt-file 
 IF(.NOT.UTL_READINITFILE('TXTFILE',LINE,IU,0))RETURN
 READ(LINE,*) TXTFILE; WRITE(*,'(A)') 'TXTFILE='//TRIM(TXTFILE)
 
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
          BOTM(NLAYM)  ,KVIDF(NLAYM) , &
          KHIDF(NLAYM) ,KVAIDF(NLAYM),KVVIDF(NLAYM),FVAL(NLAYM), &
          IACTM(NLAYM),IPFAC(NLAYM),STAT=IOS)

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
 CALL IDFNULLIFY(KHR)
 CALL IDFNULLIFY(KVR)
 CALL IDFNULLIFY(TOPR)
 CALL IDFNULLIFY(BOTR)
   
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
 IF(ALLOCATED(CIDF))THEN;   CALL IDFDEALLOCATE(CIDF,SIZE(CIDF)); DEALLOCATE(CIDF);   ENDIF
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
 IF(ALLOCATED(FVAL))THEN; DEALLOCATE(FVAL); ENDIF
 IF(ALLOCATED(IACTM))THEN; DEALLOCATE(IACTM); ENDIF
 IF(ALLOCATED(IPFAC))THEN; DEALLOCATE(IPFAC); ENDIF
 IF(ASSOCIATED(REGISFILES))DEALLOCATE(REGISFILES)
 
 END SUBROUTINE GC_DEALLOCATE 
 
 !###======================================================================
 SUBROUTINE GC_INIT_WRITE(FNAME)
 !###======================================================================
 !# subroutine to write all settings options into a *.txt file 
 IMPLICIT NONE
 INTEGER :: I,IU,IOS
 CHARACTER(LEN=256),INTENT(IN) :: FNAME
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(FNAME),STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE,DENYREAD',IOSTAT=IOS)
 IF(IOS.NE.0)RETURN
 
 WRITE(IU,'(A19)') 'FUNCTION=GEOCONNECT'
 WRITE(IU,'(A6,I1)') 'IFLAG=',GC_IFLAG
 WRITE(IU,'(A5,I3)') 'NLAY=',NLAYM
 WRITE(IU,'(A10,99I1)') 'ACTLAYERS=',(IACTM(I),I=1,NLAYM)
 WRITE(IU,'(A)') 'REGISFOLDER='//TRIM(REGISFOLDER)
 WRITE(IU,'(A)') 'TOPFOLDER='//TRIM(TOPFOLDER)
 WRITE(IU,'(A)') 'BOTFOLDER='//TRIM(BOTFOLDER)
 WRITE(IU,'(A)') 'TXTFILE='//TRIM(TXTFILE)
 
 CLOSE(IU)
 
 END SUBROUTINE GC_INIT_WRITE
 
 !###======================================================================
 SUBROUTINE GC_INIT_PREPROCESSING_WRITE(FNAME)
 !###======================================================================
 !# subroutine to write all preprocessing options into a *.ini file 
 IMPLICIT NONE
 INTEGER :: IU,IOS
 CHARACTER(LEN=256),INTENT(IN) :: FNAME
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE,DENYREAD',IOSTAT=IOS)
 IF(IOS.NE.0)RETURN
 
 IF(TRIM(IPEST).NE.'')WRITE(IU,'(A6)') 'IPEST='//TRIM(IPEST)
 IF(IPESTNR.NE.0)WRITE(IU,'(A8,I3)') 'IPESTNR=',IPESTNR
 WRITE(IU,'(A)') 'OUTPUTFOLDER='//TRIM(OUTPUTFOLDER)
 WRITE(IU,'(A,I1)') 'ISAVEK=',ISAVEK
 WRITE(IU,'(A,I1)') 'ISAVEC=',ISAVEC
 
 CLOSE(IU)

 END SUBROUTINE GC_INIT_PREPROCESSING_WRITE
 
 !###======================================================================
 SUBROUTINE GC_INIT_PREPROCESSING_READ()
 !###======================================================================
 !# read initial settings from preprocessing tab
 IMPLICIT NONE
 INTEGER :: I
 
 !## set flag
 GC_IFLAG=2 
 
 CALL WDIALOGGETSTRING(IDF_STRING1,OUTPUTFOLDER) !# Get directory+name of outputfile
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,ISAVEK) !# Get Save option KHV-,KVV,KVA
 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,ISAVEC) !# Get Save option KDW and VCW
 
 !## read formation name from grid
 DO I=1,SIZE(IPFAC%FORM)
  CALL WGRIDGETCELLSTRING(IDF_GRID1,1,I,IPFAC(I)%FORM)
  !## read factor related to formation name from grid
  CALL WGRIDGETCELLREAL(IDF_GRID1,2,I,IPFAC(I)%FACT) 
 ENDDO
 
 END SUBROUTINE GC_INIT_PREPROCESSING_READ 

 !###======================================================================
 SUBROUTINE GC_INIT_READ()
 !###======================================================================
 !# read initial settings from preprocessing tab
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB2)

 !## get amount of model layers
 CALL WDIALOGGETINTEGER(IDF_INTEGER1,NLAYM) 
 
 !## allocate all needed variables based upon NLAYM
 IF(.NOT.GC_ALLOCATE())RETURN 
 
 !## get directory of REGIS files
 CALL WDIALOGGETSTRING(IDF_STRING1,REGISFOLDER)
 !## get directory of TOP files model
 CALL WDIALOGGETSTRING(IDF_STRING2,TOPFOLDER) 
 !## get directory of BOT files model
 CALL WDIALOGGETSTRING(IDF_STRING3,BOTFOLDER) 
 !## read formation name from grid 
 DO I=1,SIZE(IACTM)
  CALL WGRIDGETCELLINTEGER(IDF_GRID1,2,I,IACTM(I))
 ENDDO
 
 END SUBROUTINE GC_INIT_READ 
 
END MODULE MOD_GEOCONNECT_PAR