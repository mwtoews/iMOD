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
USE MOD_UTL, ONLY : UTL_READINITFILE,UTL_GETUNIT,ITOS,UTL_IDFSNAPTOGRID
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFNULLIFY,IDFCOPY,IDFDEALLOCATE,IDFDEALLOCATEX
USE MOD_OSD, ONLY : OSD_OPEN
!## IDF-types to read IDF-files from given folders
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: TOPM,BOTM,CIDF,KDHIDF,KDVIDF,KHVIDF,KVAIDF,KVVIDF 
!## IDF-types for REGIS-files
TYPE(IDFOBJ),SAVE :: TOPR,BOTR,KHR,KVR,IDF,TOP,BOT
!## store available filenames
CHARACTER(LEN=256),DIMENSION(:),POINTER :: REGISFILES

INTEGER,ALLOCATABLE,DIMENSION(:) :: IACTM
INTEGER,SAVE :: NLAYM, &     !## NLAYM (model), 
                NLAYR, &     !## NLAYR (Regis)
                IAGGR, &     !## aggregate number (1,2, or 3)
                IWINDOW, &   !## window specified
                MODELTYPE, & !## type of model results to aggregate
                INPUTTYPE, & !## type of input to aggregate
                IAGGR_DUPLICATES, & !## expression to aggregate (1,2,3,4)
                IOPTW, &
                ISAVETB, &   !## save top/bottom after aggregation
                IPESTNR
CHARACTER(LEN=256),SAVE :: OUTPUTFOLDER,DBASEFOLDER,MODELFOLDER,REGISFOLDER,TOPFOLDER,BOTFOLDER,IPEST,IPFFILE

!TXTFILE

INTEGER :: GC_IFLAG,ISAVEK,ISAVEC !# IFLAG related to GC computation options 1=identify, 2=preprocessing, 3=postprocessing

TYPE FRMOBJ
 CHARACTER(LEN=12) :: FORM
 CHARACTER(LEN=1) :: LITHO
 REAL :: FACT
 INTEGER :: IGRP,LITHOCLR
 REAL,POINTER,DIMENSION(:) :: FVAL
END TYPE
TYPE(FRMOBJ),ALLOCATABLE,DIMENSION(:) :: IPFAC

CONTAINS

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
          BOTM(NLAYM)  ,KHVIDF(NLAYM),KVAIDF(NLAYM),KVVIDF(NLAYM-1), &
          IACTM(NLAYM),STAT=IOS)

 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot allocate neccessary memory.','Error')
  RETURN
 ENDIF
 
 !## nullify idf-objects
 DO I=1,SIZE(CIDF);   CALL IDFNULLIFY(CIDF(I)); ENDDO 
 DO I=1,SIZE(KDHIDF); CALL IDFNULLIFY(KDHIDF(I)); ENDDO 
 DO I=1,SIZE(KDVIDF); CALL IDFNULLIFY(KDVIDF(I)); ENDDO
 DO I=1,SIZE(KHVIDF); CALL IDFNULLIFY(KHVIDF(I)); ENDDO 
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
 IMPLICIT NONE
 INTEGER :: I
 
 !## deallocate all arrays
 IF(ALLOCATED(TOPM))THEN;   CALL IDFDEALLOCATE(TOPM,SIZE(TOPM)); DEALLOCATE(TOPM); ENDIF
 IF(ALLOCATED(BOTM))THEN;   CALL IDFDEALLOCATE(BOTM,SIZE(BOTM)); DEALLOCATE(BOTM); ENDIF
 IF(ALLOCATED(CIDF))THEN;   CALL IDFDEALLOCATE(CIDF,SIZE(CIDF)); DEALLOCATE(CIDF);   ENDIF
 IF(ALLOCATED(KDHIDF))THEN; CALL IDFDEALLOCATE(KDHIDF,SIZE(KDHIDF)); DEALLOCATE(KDHIDF);  ENDIF
 IF(ALLOCATED(KDVIDF))THEN; CALL IDFDEALLOCATE(KDVIDF,SIZE(KDVIDF)); DEALLOCATE(KDVIDF);  ENDIF
 IF(ALLOCATED(KHVIDF))THEN; CALL IDFDEALLOCATE(KHVIDF,SIZE(KHVIDF)); DEALLOCATE(KHVIDF);  ENDIF
 IF(ALLOCATED(KVAIDF))THEN; CALL IDFDEALLOCATE(KVAIDF,SIZE(KVAIDF)); DEALLOCATE(KVAIDF);  ENDIF
 IF(ALLOCATED(KVVIDF))THEN; CALL IDFDEALLOCATE(KVVIDF,SIZE(KVVIDF)); DEALLOCATE(KVVIDF);  ENDIF 
 CALL IDFDEALLOCATEX(KHR)
 CALL IDFDEALLOCATEX(KVR)
 CALL IDFDEALLOCATEX(TOPR)
 CALL IDFDEALLOCATEX(BOTR)
 CALL IDFDEALLOCATEX(IDF)
 CALL IDFDEALLOCATEX(TOP)
 CALL IDFDEALLOCATEX(BOT)
 
 END SUBROUTINE GC_DEALLOCATE 
 
 !###======================================================================
 LOGICAL FUNCTION GC_INIT_READ(JU,FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: JU
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: IU,I,J,IOS
 CHARACTER(LEN=256) :: LINE 

 GC_INIT_READ=.FALSE.
 
 !## read *.txt-file
 IF(JU.EQ.0)THEN
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(FNAME),STATUS='OLD',FORM='FORMATTED',ACTION='READ',IOSTAT=IOS)
  IF(IOS.NE.0)RETURN
 ELSE
  IU=JU
 ENDIF

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
  
 GC_INIT_READ=.TRUE.

 IF(JU.EQ.0)CLOSE(IU)

 END FUNCTION GC_INIT_READ

 !###======================================================================
 SUBROUTINE GC_INIT_WRITE(JU,FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: JU
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: I,IU,IOS
 
 IF(JU.EQ.0)THEN
  IU=UTL_GETUNIT()
  CALL OSD_OPEN(IU,FILE=TRIM(FNAME),STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE,DENYREAD',IOSTAT=IOS)
  IF(IOS.NE.0)RETURN
 ELSE
  IU=JU
 ENDIF
 
 WRITE(IU,'(A5,I3)') 'NLAY=',NLAYM
 WRITE(IU,'(A10,99I1)') 'ACTLAYERS=',(IACTM(I),I=1,NLAYM)
 WRITE(IU,'(A)') 'REGISFOLDER='//TRIM(REGISFOLDER)
 WRITE(IU,'(A)') 'TOPFOLDER='//TRIM(TOPFOLDER)
 WRITE(IU,'(A)') 'BOTFOLDER='//TRIM(BOTFOLDER)
 
 IF(JU.EQ.0)CLOSE(IU)
 
 END SUBROUTINE GC_INIT_WRITE
 
 !###======================================================================
 SUBROUTINE GC_INIT_GET()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB4)

 !## get amount of model layers
 CALL WDIALOGGETINTEGER(IDF_INTEGER1,NLAYM) 
  
 !## get directory of REGIS files
 CALL WDIALOGGETSTRING(IDF_STRING1,REGISFOLDER)
 !## get directory of TOP files model
 CALL WDIALOGGETSTRING(IDF_STRING2,TOPFOLDER) 
 !## get directory of BOT files model
 CALL WDIALOGGETSTRING(IDF_STRING3,BOTFOLDER) 
 !## read formation name from grid 
 DO I=1,SIZE(IACTM)
  CALL WGRIDGETCELLCHECKBOX(IDF_GRID1,1,I,IACTM(I))
 ENDDO
 
 END SUBROUTINE GC_INIT_GET 
 
 !###======================================================================
 SUBROUTINE GC_INIT_PUT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB4)

 !## get amount of model layers
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,NLAYM) 
 CALL WDIALOGFIELDSTATE(IDF_INTEGER1,2)
  
 !## get directory of REGIS files
 CALL WDIALOGPUTSTRING(IDF_STRING1,REGISFOLDER)
 !## get directory of TOP files model
 CALL WDIALOGPUTSTRING(IDF_STRING2,TOPFOLDER) 
 !## get directory of BOT files model
 CALL WDIALOGPUTSTRING(IDF_STRING3,BOTFOLDER) 
 !## read formation name from grid 
 CALL WGRIDROWS(IDF_GRID1,NLAYM)
 DO I=1,SIZE(IACTM)
  CALL WGRIDLABELROW(IDF_GRID1,I,'Layer '//TRIM(ITOS(I)))
  CALL WGRIDPUTCELLCHECKBOX(IDF_GRID1,1,I,IACTM(I))
 ENDDO
 
 END SUBROUTINE GC_INIT_PUT
  
 !###======================================================================
 SUBROUTINE GC_INIT_PREPROCESSING_WRITE(FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU,IOS,I
 CHARACTER(LEN=256),INTENT(IN) :: FNAME
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE,DENYREAD',IOSTAT=IOS)
 IF(IOS.NE.0)RETURN
 
 WRITE(IU,'(A)') 'FUNCTION=GEOCONNECT'

 !## write common settings
 CALL GC_INIT_WRITE(IU,'')

 WRITE(IU,'(A6,I1)') 'IFLAG=',GC_IFLAG
 IF(IWINDOW.EQ.1)THEN
  WRITE(IU,'(A7,4(F10.2,1X))') 'WINDOW=',IDF%XMIN,IDF%XMAX,IDF%YMIN,IDF%YMAX
 ENDIF

 WRITE(IU,'(A,I10)') 'NFORM=',SIZE(IPFAC%FORM)
 DO I=1,SIZE(IPFAC%FORM)
  WRITE(IU,'(A10,1X,F10.2)') TRIM(IPFAC(I)%FORM),IPFAC(I)%FACT
 ENDDO
 !IF(TRIM(IPEST).NE.'')WRITE(IU,'(A6)') 'IPEST='//TRIM(IPEST)
 !IF(IPESTNR.NE.0)WRITE(IU,'(A8,I3)') 'IPESTNR=',IPESTNR
 WRITE(IU,'(A)') 'OUTPUTFOLDER='//TRIM(OUTPUTFOLDER)
 WRITE(IU,'(A,I1)') 'ISAVEK=',ISAVEK
 WRITE(IU,'(A,I1)') 'ISAVEC=',ISAVEC
! WRITE(IU,'(A)') 'TXTFILE='//TRIM(TXTFILE)
 
 CLOSE(IU)

 END SUBROUTINE GC_INIT_PREPROCESSING_WRITE

 !###======================================================================
 LOGICAL FUNCTION GC_INIT_PREPROCESSING_READ(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU !# Unit number of GEOCONNECT.ini
 INTEGER :: IOS,I,N
 CHARACTER(LEN=256) :: LINE
 
 GC_INIT_PREPROCESSING_READ=.FALSE.
 
 OUTPUTFOLDER=''
 ISAVEK=1
 ISAVEC=0 
 
 !## read common settings
 IF(.NOT.GC_INIT_READ(IU,''))RETURN

 IF(.NOT.UTL_READINITFILE('NFORM',LINE,IU,0))RETURN
 READ(LINE,*) N; WRITE(*,'(A,I10)') 'NFORM=',N
 ALLOCATE(IPFAC(N))
 DO I=1,SIZE(IPFAC%FORM)
  IF(.NOT.UTL_READINITFILE('FORM'//TRIM(ITOS(I)),LINE,IU,0))RETURN
  READ(LINE,*) IPFAC(I)%FORM,IPFAC(I)%FACT
  LINE='FORM'//TRIM(ITOS(I))//'='
  WRITE(IU,'(A,1X,A,1X,F10.2)') TRIM(LINE),TRIM(IPFAC(I)%FORM),IPFAC(I)%FACT
 ENDDO

 IF(UTL_READINITFILE('ISAVEK',LINE,IU,1))THEN
  READ(LINE,*) ISAVEK; WRITE(*,'(A7,I1)') 'ISAVEK=',ISAVEK
 ENDIF
 IF(UTL_READINITFILE('ISAVEC',LINE,IU,1))THEN
  READ(LINE,*) ISAVEC; WRITE(*,'(A7,I1)') 'ISAVEC=',ISAVEC
 ENDIF 

 IF(.NOT.UTL_READINITFILE('OUTPUTFOLDER',LINE,IU,0))RETURN
 READ(LINE,*) OUTPUTFOLDER; WRITE(*,'(A)') 'OUTPUTFOLDER='//TRIM(OUTPUTFOLDER)

! !## read name of *.txt-file 
! IF(.NOT.UTL_READINITFILE('TXTFILE',LINE,IU,0))RETURN
! READ(LINE,*) TXTFILE; WRITE(*,'(A)') 'TXTFILE='//TRIM(TXTFILE)
 
 GC_INIT_PREPROCESSING_READ=.TRUE.

 END FUNCTION GC_INIT_PREPROCESSING_READ
  
 !###======================================================================
 SUBROUTINE GC_INIT_PREPROCESSING_PUT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB2)

 !## put directory+name of outputfile
 CALL WDIALOGPUTSTRING(IDF_STRING1,TRIM(OUTPUTFOLDER)) 
 !## put Save option KHV-,KVV,KVA
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,ISAVEK) 
 !## put Save option KDW and VCW
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,ISAVEC) 
 
 END SUBROUTINE GC_INIT_PREPROCESSING_PUT 
 
 !###======================================================================
 LOGICAL FUNCTION GC_INIT_PREPROCESSING_GET()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 GC_INIT_PREPROCESSING_GET=.FALSE.
 
 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB2)
 
  !## get window
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IWINDOW)
 IF(IWINDOW.EQ.1)THEN
  IDF%NCOL=0; IDF%NROW=0
 ELSE
  CALL WDIALOGGETREAL(IDF_REAL1,IDF%XMIN); CALL WDIALOGGETREAL(IDF_REAL2,IDF%YMIN)
  CALL WDIALOGGETREAL(IDF_REAL3,IDF%XMAX); CALL WDIALOGGETREAL(IDF_REAL4,IDF%YMAX)
  CALL WDIALOGGETREAL(IDF_REAL5,IDF%DX); IDF%DY=IDF%DX
  IF(IDF%DX.LE.0.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should specify a CellSize of greater than 0.0.','Error')
   RETURN
  ENDIF
  CALL UTL_IDFSNAPTOGRID(IDF%XMIN,IDF%XMAX,IDF%YMIN,IDF%YMAX,IDF%DX,IDF%NCOL,IDF%NROW)
 ENDIF

 !## get directory+name of outputfile
 CALL WDIALOGGETSTRING(IDF_STRING1,OUTPUTFOLDER) 
 IF(TRIM(OUTPUTFOLDER).EQ.'')THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should specify an output folder.','Error')
  RETURN
 ENDIF
 !## get save option KHV-,KVV,KVA
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,ISAVEK)      
 !## get save option KDW and VCW
 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,ISAVEC)      
 
 !## read formation name from grid
 DO I=1,NLAYR 
!  CALL WGRIDGETCELLSTRING(IDF_GRID1,1,I,IPFAC(I)%FORM)
  !## read factor related to formation name from grid
  CALL WGRIDGETCELLREAL(IDF_GRID1,3,I,IPFAC(I)%FACT) 
 ENDDO
 
 !## set flag
 GC_IFLAG=2 

 GC_INIT_PREPROCESSING_GET=.TRUE.
 
 END FUNCTION GC_INIT_PREPROCESSING_GET 

 !###======================================================================
 SUBROUTINE GC_INIT_POSTPROCESSING_PUT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB3)

! !## put directory+name of outputfile
! CALL WDIALOGPUTSTRING(IDF_STRING1,TRIM(OUTPUTFOLDER)) 
! !## put Save option KHV-,KVV,KVA
! CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,ISAVEK) 
! !## put Save option KDW and VCW
! CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,ISAVEC) 
 
 END SUBROUTINE GC_INIT_POSTPROCESSING_PUT 
 
 !###======================================================================
 LOGICAL FUNCTION GC_INIT_POSTPROCESSING_GET()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 GC_INIT_POSTPROCESSING_GET=.FALSE.
 
 CALL IDFNULLIFY(IDF)

 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB3)
  
 !## get window
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IWINDOW)
 IF(IWINDOW.EQ.1)THEN
  IDF%NCOL=0; IDF%NROW=0
 ELSE
  CALL WDIALOGGETREAL(IDF_REAL1,IDF%XMIN); CALL WDIALOGGETREAL(IDF_REAL2,IDF%YMIN)
  CALL WDIALOGGETREAL(IDF_REAL3,IDF%XMAX); CALL WDIALOGGETREAL(IDF_REAL4,IDF%YMAX)
  CALL WDIALOGGETREAL(IDF_REAL5,IDF%DX); IDF%DY=IDF%DX
  IF(IDF%DX.LE.0.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should specify a CellSize of greater than 0.0.','Error')
   RETURN
  ENDIF
  CALL UTL_IDFSNAPTOGRID(IDF%XMIN,IDF%XMAX,IDF%YMIN,IDF%YMAX,IDF%DX,IDF%NCOL,IDF%NROW)
 ENDIF
 
 !## get dbase-directory+name of outputfile
 CALL WDIALOGGETSTRING(IDF_STRING1,DBASEFOLDER) 
 IF(TRIM(DBASEFOLDER).EQ.'')THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should specify a DBASE folder.','Error')
  RETURN
 ENDIF

 !## read formation name from grid
 DO I=1,NLAYR
  !## read factor related to formation name from grid
  CALL WGRIDGETCELLINTEGER(IDF_GRID1,3,I,IPFAC(I)%IGRP)
 ENDDO

 !## get aggregate option (1=model; 2=input; 3=ipf)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,IAGGR)
 SELECT CASE (IAGGR)
  !## model results
  CASE (1)
   !## get dbase-directory+name of outputfile
   CALL WDIALOGGETSTRING(IDF_STRING2,MODELFOLDER) 
   IF(TRIM(MODELFOLDER).EQ.'')THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should specify a model results folder.','Error')
    RETURN
   ENDIF
   CALL WDIALOGGETMENU(IDF_MENU1,MODELTYPE) 
  !## model input
  CASE (2)
   CALL WDIALOGGETMENU(IDF_MENU2,INPUTTYPE) 
  !## ipf-file
  CASE (3)
   !## get ipffile
   CALL WDIALOGGETSTRING(IDF_STRING3,IPFFILE) 
   IF(TRIM(IPFFILE).EQ.'')THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should specify an ipf-file.','Error')
    RETURN
   ENDIF
 END SELECT

 !## get aggregate option (1=model; 2=input; 3=ipf)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO6,IAGGR_DUPLICATES)
  
 !## get dbase-directory+name of outputfile
 CALL WDIALOGGETSTRING(IDF_STRING4,OUTPUTFOLDER) 
 IF(TRIM(OUTPUTFOLDER).EQ.'')THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should specify an output folder.','Error')
  RETURN
 ENDIF

 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,ISAVETB)
 
 !## set flag
 GC_IFLAG=3

 GC_INIT_POSTPROCESSING_GET=.TRUE.
 
 END FUNCTION GC_INIT_POSTPROCESSING_GET 


  
END MODULE MOD_GEOCONNECT_PAR