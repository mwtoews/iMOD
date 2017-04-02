!!  Copyright (C) Stichting Deltares, 2005-2017.
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
MODULE MOD_RO_SCEN

USE WINTERACTER
USE RESOURCE
USE MOD_PREF_PAR, ONLY : PREFVAL,PREF
USE MOD_OSD, ONLY : OSD_GETENV,OSD_OPEN,OSD_IOSTAT_MSG,ICF
USE MOD_UTL, ONLY : UTL_GETUNIT,RTOS,UTL_CAP,UTL_WSELECTFILE,UTL_WAITMESSAGE,UTL_CHECKNAME
USE IMODVAR
USE IMOD, ONLY : IDFINIT
USE ROVAR
USE MOD_IDF, ONLY : IDFREAD,IDFOPEN,IDFGETXYVAL,IDFWRITEDIM,IDFOBJ,IDFGETAGGREGATEDVAL,IDF_EXTENT,IDFNULLIFY,IDFDEALLOCATEX

CONTAINS

!###======================================================================
SUBROUTINE ROSCENMAIN(ITYPE,MESSAGE)
!###======================================================================
IMPLICIT NONE
TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
INTEGER,INTENT(IN) :: ITYPE

SELECT CASE(ITYPE)
 CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDF_CHECK1)
     CALL ROSCEN1FIELDS(IDF_MENU2)
    CASE (IDF_MENU2)
      CALL ROSCEN1FIELDS(IDF_MENU2)
      CALL ROSCENREINITPREFS()
    CASE (IDF_CHECK3)
      CALL ROSCEN2FIELDS()
   END SELECT

 CASE (TABCHANGED)
  SELECT CASE (MESSAGE%VALUE2)
   CASE (ID_DROSCENT1)

   CASE (ID_DROSCENT2,ID_DROSCENT3)
    CALL ROSCEN1FIELDS(MESSAGE%VALUE2)

   CASE (ID_DROSCENT4)
  END SELECT

 CASE(PUSHBUTTON)
  SELECT CASE (MESSAGE%VALUE1)	!INITIAL DIALOG

   CASE (ID_OPEN1,ID_OPEN2,ID_OPEN3,ID_OPEN4,ID_OPEN5,ID_OPEN6)
    CALL ROSCEN1SELECTFILE(MESSAGE)

   CASE (ID_HELP)
!    CALL IMODGETHELP('',1026)

   CASE (IDOK)
    CALL ROSCENOK()

   CASE (IDCANCEL)
    CALL ROSCENCLOSE()
    
  END SELECT
END SELECT

END SUBROUTINE

!###======================================================================
SUBROUTINE ROSCENINIT()
!###======================================================================
IMPLICIT NONE
INTEGER,PARAMETER :: MAXPREFRO=11
CHARACTER(LEN=256) :: RESULTDIR
INTEGER	:: NR,I,J,ISTATE,NRSEL
INTEGER,DIMENSION(MAXOPT) :: IOPTION
LOGICAL	:: LEX
INTEGER,DIMENSION(6) :: BTNFLDS,STRFLDS
CHARACTER(LEN=11),DIMENSION(MAXPREFRO) :: PREFS
DATA PREFS/'LANDUSE','HLPSOIL','NDT','NDT_LUT','ABIOT_LUT','RFCSOIL', &
           'RFC_LUT','HLP_DRY','HLP_WET','URBAN_RANGE','CROP_COSTS'/

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_ROTOOL,2).EQ.1)THEN
  CALL ROSCENCLOSE(); RETURN
 ENDIF

 CALL MAIN1INACTMODULE(ID_ROTOOL)
 !## other module no closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_ROTOOL,2,1)
 
!## Initialise dialog settings first.
CALL WINDOWSELECT(0)

!## place some bitmaps on the buttons
CALL WDIALOGLOAD(ID_DROSCEN,ID_DROSCEN)
CALL WDIALOGSELECT(ID_DROSCENT1)
CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONOPEN,1)

IOPTION=0; CALL WDIALOGPUTMENU(IDF_MENU2,RONAME,MAXOPT,IOPTION)

RESULTDIR = TRIM(PREFVAL(1))//'\TMP\'

CALL WDIALOGPUTSTRING(ID_FILE1,RESULTDIR)

!## Reinitialise SCENPREF and REFPREF type arrays, as well as LEGEND array
IF (ALLOCATED(SCENPREF)) DEALLOCATE(SCENPREF); ALLOCATE(SCENPREF(MAXOPT))
IF (ALLOCATED(REFPREF)) DEALLOCATE(REFPREF); ALLOCATE(REFPREF(MAXOPT))
LEGEND = ''

DO I=1,MAXOPT
 SCENPREF(I)%GHG  = ''; REFPREF(I)%GHG   = ''
 SCENPREF(I)%GLG  = ''; REFPREF(I)%GLG   = ''
 SCENPREF(I)%LUSE = ''; REFPREF(I)%LUSE  = ''
 SCENPREF(I)%SOIL = ''; REFPREF(I)%SOIL  = ''
 SCENPREF(I)%LUT1 = ''; REFPREF(I)%LUT1  = ''
 SCENPREF(I)%LUT2 = ''; REFPREF(I)%LUT2  = ''
 SCENPREF(I)%LUT3 = ''; REFPREF(I)%LUT3  = ''
ENDDO

!## POPULATE IACT (activated subtypes)
CALL WDIALOGGETMENU(IDF_MENU2,IOPTION)
NRSEL = 0
DO I=1,MAXOPT
 IF (IOPTION(I).EQ.1) THEN
  SCENPREF(I)%IACT = 1
  REFPREF(I)%IACT = 1
  NRSEL = NRSEL + 1
 ELSE
  SCENPREF(I)%IACT = 0
  REFPREF(I)%IACT = 0
 ENDIF
ENDDO

!## Assign bitmaps to buttons on ID_DROSCENT2 and ID_DROSCENT3
!## and reset them to the original (empty) situation
BTNFLDS = (/ID_OPEN1,ID_OPEN2,ID_OPEN3,ID_OPEN4,ID_OPEN5,ID_OPEN6/)
STRFLDS = (/ID_FILE1,ID_FILE2,ID_FILE3,ID_FILE4,ID_FILE5,ID_FILE6/)
DO I=1,2
 IF (I.EQ.1) CALL WDIALOGSELECT(ID_DROSCENT2)
 IF (I.EQ.2) CALL WDIALOGSELECT(ID_DROSCENT3)
 DO J=1,6
  IF (J.NE.6) THEN
   CALL WDIALOGPUTSTRING(STRFLDS(J),'')
   CALL WDIALOGPUTIMAGE(BTNFLDS(J),ID_ICONOPENIDF,1)
  ELSE
   CALL WDIALOGPUTSTRING(STRFLDS(J),'')
   CALL WDIALOGPUTIMAGE(BTNFLDS(J),ID_ICONOPEN,1)
  ENDIF
 ENDDO
ENDDO

CALL WDIALOGSELECT(ID_DROSCENT4)
CALL WDIALOGCLEARFIELD(IDF_GRID1)

!## resize grid (idf_grid1) with MAXPREF
CALL WGRIDROWS(IDF_GRID1,MAXPREFRO)

!## Store preferences for both the reference situation as the scenario situation in the
!## two arrays (SCENPREF and REFPREF)
NR=0
DO I=1,MAXPREFRO

 DO J=1,SIZE(PREF)
  IF(TRIM(UTL_CAP(PREF(J),'U')).EQ.TRIM(UTL_CAP(PREFS(I),'U')))EXIT
 ENDDO
 LEX=.TRUE.; IF(J.GT.SIZE(PREFVAL))LEX=.FALSE.
 IF(LEX)INQUIRE(FILE=PREFVAL(J),EXIST=LEX)
 
 IF(LEX)THEN
  CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,I,TRIM(PREF(J)))
  CALL WGRIDPUTCELLSTRING(IDF_GRID1,2,I,TRIM(PREFVAL(J)))

  NR=NR+1
!##AGRICULTURAL PREFERENCES
  IF(PREF(J).EQ.'LANDUSE') THEN
   REFPREF(1:4)%LUSE  = PREFVAL(J)	!AGRICULTURAL
   SCENPREF(1:4)%LUSE = PREFVAL(J)
   REFPREF(6)%LUSE    = PREFVAL(J)	!URBAN AREA
   SCENPREF(6)%LUSE   = PREFVAL(J)
  ENDIF
!##AGRICULTURAL PREFERENCES
  IF(PREF(J).EQ.'HLPSOIL') THEN
   REFPREF(1:4)%SOIL  = PREFVAL(J)
   SCENPREF(1:4)%SOIL = PREFVAL(J)
  ENDIF
  IF(PREF(J).EQ.'RFCSOIL') THEN
   REFPREF(5)%SOIL    = PREFVAL(J)
   SCENPREF(5)%SOIL   = PREFVAL(J)
  ENDIF
  IF(PREF(J).EQ.'HLP_DRY') THEN
   REFPREF(2)%LUT1    = PREFVAL(J)
   SCENPREF(2)%LUT1   = PREFVAL(J)
   REFPREF(4)%LUT1    = PREFVAL(J)
   SCENPREF(4)%LUT1   = PREFVAL(J)
  ENDIF
  IF(PREF(J).EQ.'HLP_WET') THEN
   REFPREF(1)%LUT1    = PREFVAL(J)
   SCENPREF(1)%LUT1   = PREFVAL(J)
   REFPREF(3)%LUT1    = PREFVAL(J)
   SCENPREF(3)%LUT1   = PREFVAL(J)
  ENDIF
  IF(PREF(J).EQ.'CROP_COSTS') THEN
   REFPREF(1:4)%LUT2= PREFVAL(J)
   SCENPREF(1:4)%LUT2= PREFVAL(J)
  ENDIF
!##NATURE PREFERENCES
  IF(PREF(J).EQ.'NDT') THEN
   REFPREF(5)%LUSE    = PREFVAL(J)
   SCENPREF(5)%LUSE   = PREFVAL(J)
  ENDIF
  IF(PREF(J).EQ.'ABIOT_LUT') THEN
   REFPREF(5)%LUT1    = PREFVAL(J)
   SCENPREF(5)%LUT1   = PREFVAL(J)
  ENDIF
  IF(PREF(J).EQ.'RFC_LUT') THEN
   REFPREF(5)%LUT2    = PREFVAL(J)
   SCENPREF(5)%LUT2   = PREFVAL(J)
  ENDIF
  IF(PREF(J).EQ.'NDT_LUT') THEN
   REFPREF(5)%LUT3    = PREFVAL(J)
   SCENPREF(5)%LUT3   = PREFVAL(J)
  ENDIF
!##URBAN PREFERENCES
  IF(PREF(J).EQ.'URBAN_RANGE') THEN
   REFPREF(6)%LUT1    = PREFVAL(J)
   SCENPREF(6)%LUT1   = PREFVAL(J)
  ENDIF

 ELSE
  CALL WMESSAGEBOX(OKOnly,ExclamationIcon,CommonOK,'File assigned to '//TRIM(PREF(J))// &
       ' not found. Please correct this in the preference file and start the RO Tool again.','iMOD ERROR')
  CALL ROSCENCLOSE()
  RETURN
 ENDIF
END DO

IF(NR.LT.MAXPREFRO)THEN
 CALL WMESSAGEBOX(OKONLY,InformationIcon,CommonOK,&
    'Not all preferences are found. Please check preference file and iMOD documentation (RO TOOL).',&
    'iMOD warning')
 CALL ROSCENCLOSE()
 RETURN
ENDIF

!Disable tabs DROSCENT2 and DROSCENT3
ISTATE = 0
CALL WDIALOGSELECT(ID_DROSCEN)
CALL WDIALOGTABSTATE(ID_TAB,ID_DROSCENT2,ISTATE)
CALL WDIALOGTABSTATE(ID_TAB,ID_DROSCENT3,ISTATE)
CALL WDIALOGLOAD(ID_DROSCEN,ID_DROSCEN)
CALL WDIALOGSHOW(-1,-1,0,2)

END SUBROUTINE ROSCENINIT

!###======================================================================
SUBROUTINE ROSCENCLOSE()
!###======================================================================
IMPLICIT NONE
INTEGER	:: I,J

!## Deallocate several arrays
IF (ALLOCATED(REFPREF)) DEALLOCATE(REFPREF)
IF (ALLOCATED(SCENPREF))DEALLOCATE(SCENPREF)
IF (ALLOCATED(IDFS))    DEALLOCATE(IDFS)

!## Close all files
IF (ALLOCATED(ROSCEN)) THEN
 DO I=1,5; DO J=1,2; IF (ROSCEN(J,I)%IU.GT.0)CLOSE(ROSCEN(J,I)%IU); ENDDO; ENDDO
 DEALLOCATE(ROSCEN)
ENDIF

CALL WDIALOGSELECT(ID_DROSCEN); CALL WDIALOGUNLOAD(ID_DROSCEN)
CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_ROTOOL,2,0)

END SUBROUTINE ROSCENCLOSE

!###======================================================================
SUBROUTINE ROSCEN1SELECTFILE(MESSAGE)
!###======================================================================
IMPLICIT NONE
TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
CHARACTER(LEN=256) :: IDFNAME,FILTERSTRING,WTITLE,IDFORG
INTEGER :: IFIELD, IFLAGS,CODE

IFLAGS = LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT+MUSTEXIST

CALL WDIALOGGETSTRING(IFIELD,IDFNAME)
CALL WDIALOGSELECT(MESSAGE%WIN)

IF (MESSAGE%VALUE1.EQ.ID_OPEN6) THEN	!LEGEND FILE
 FILTERSTRING = 'iMOD legend files|*.leg|'
 IDFNAME = TRIM(PREFVAL(1))//'\'//TRIM(OSD_GETENV('USERNAME'))//'\LEGEND\'
 WTITLE = 'Select iMOD legend files'
ELSE
 FILTERSTRING = 'iMOD files|*.idf|'
 WTITLE = 'Select iMOD files'
 IDFNAME = TRIM(PREFVAL(1))//'\'//TRIM(OSD_GETENV('USERNAME'))//'\TMP\'
 IDFORG = IDFNAME
ENDIF

SELECT CASE (MESSAGE%WIN)
 CASE (ID_DROSCENT1)
  IFLAGS = NONEXPATH+DIRCHANGE+DIRCREATE
  IDFNAME = TRIM(PREFVAL(5))//'\'//TRIM(OSD_GETENV('USERNAME'))//'\TMP\'
  CALL WSELECTDIR(IFLAGS,IDFNAME,'Select directory')
  IDFNAME = TRIM(IDFNAME)//'\'
 CASE (ID_DROSCENT2)
  IF(UTL_WSELECTFILE(TRIM(FILTERSTRING),IFLAGS,IDFNAME,TRIM(WTITLE)))THEN; ENDIF !,IFTYPE) WSELECTFILE(TRIM(FILTERSTRING),IFLAGS,IDFNAME,TRIM(WTITLE))
  CODE = 1 !scenario
 CASE (ID_DROSCENT3)
  CODE = 2 !reference
  IF(UTL_WSELECTFILE(TRIM(FILTERSTRING),IFLAGS,IDFNAME,TRIM(WTITLE)))THEN; ENDIF !,IFTYPE) WSELECTFILE(TRIM(FILTERSTRING),IFLAGS,IDFNAME,TRIM(WTITLE))
END SELECT

IF (WINFODIALOG(ExitButtonCommon)==CommonCancel) IDFNAME = ''

SELECT CASE (MESSAGE%VALUE1)
 CASE (ID_OPEN1)		!GHG
  IFIELD = ID_FILE1
  CALL ROSCENFILLPREFS(1,IDFNAME,CODE)
 CASE (ID_OPEN2)		!GLG
  IFIELD = ID_FILE2
  CALL ROSCENFILLPREFS(2,IDFNAME,CODE)
 CASE (ID_OPEN3)		!LUSE
  IFIELD = ID_FILE3
  CALL ROSCENFILLPREFS(3,IDFNAME,CODE)
 CASE (ID_OPEN4)		!NDT
  IFIELD = ID_FILE4
  CALL ROSCENFILLPREFS(4,IDFNAME,CODE)
 CASE (ID_OPEN5)		!outputfile (only available when 1 subtype is chosen)
  IFIELD = ID_FILE5
 CASE (ID_OPEN6)		!legendfile = optionial
  IFIELD = ID_FILE6
  LEGEND(CODE) = TRIM(IDFNAME)
END SELECT

CALL WDIALOGPUTSTRING(IFIELD,TRIM(IDFNAME))

END SUBROUTINE ROSCEN1SELECTFILE

!###======================================================================
SUBROUTINE ROSCENFILLPREFS(RCODE,IDFNAME,CODE)
!###======================================================================
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN)	:: IDFNAME
INTEGER,INTENT(IN) :: RCODE !1 = GHG, 2=GLG, 3=LUSE, 4=NDT
INTEGER,INTENT(IN) :: CODE	!1 = SCENARIO SITUATION, 2 = REFERENCE SITUATION
INTEGER :: I

SELECT CASE (CODE)
 CASE (1)	!## SCENARIO
  DO I=1,6
   IF (SCENPREF(I)%IACT.EQ.1) THEN
    IF (RCODE.EQ.1) THEN
     SCENPREF(I)%GHG = IDFNAME
    ENDIF
    IF (RCODE.EQ.2) THEN
     SCENPREF(I)%GLG = IDFNAME
    ENDIF
   ENDIF
  ENDDO
 CASE (2)	!## REFERENCE SITUATION
  DO I=1,6
   IF (REFPREF(I)%IACT.EQ.1) THEN
    IF (RCODE.EQ.1) THEN
     REFPREF(I)%GHG = IDFNAME
    ENDIF
    IF (RCODE.EQ.2) THEN
     REFPREF(I)%GLG = IDFNAME
    ENDIF
   ENDIF
  ENDDO
END SELECT

END SUBROUTINE

!###======================================================================
SUBROUTINE ROSCEN1FIELDS(IFLD)
!###======================================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: IFLD
INTEGER :: I,J
INTEGER,DIMENSION(MAXOPT) :: IOPTIONS
INTEGER :: NRSEL,ISTATE,JSTATE
CHARACTER(LEN=256) :: FNAME,RESULTDIR
LOGICAL :: LOGICALDR

CALL WDIALOGSELECT(ID_DROSCENT1)
CALL WDIALOGGETCHECKBOX(IDF_CHECK1,JSTATE)
CALL WDIALOGGETMENU(IDF_MENU2,IOPTIONS)
NRSEL = 0
LOGICALDR=.FALSE.
DO I=1,MAXOPT
 IF (IOPTIONS(I).EQ.1) THEN
  J=I
  NRSEL = NRSEL+1
  IF(I.EQ.2.OR.I.EQ.4) LOGICALDR=.TRUE.
 ENDIF
ENDDO

!## Switch on checkbox sprinkling on
IF(LOGICALDR) CALL WDIALOGFIELDSTATE(IDF_CHECK3,1)
IF(.NOT.LOGICALDR) CALL WDIALOGFIELDSTATE(IDF_CHECK3,0)

CALL WDIALOGGETSTRING(ID_FILE1,RESULTDIR)
ISTATE = 1
IF (NRSEL.GT.1) THEN
 !## DISABLE LEGEND CHOICE AND OUTPUTFILE CHOICE FOR TABS DROSCENT2 EN 3
 CALL WDIALOGSELECT(IFLD)

 CALL WDIALOGFIELDSTATE(ID_OPEN5,0)
 CALL WDIALOGFIELDSTATE(ID_OPEN6,0)
 CALL WDIALOGFIELDSTATE(ID_FILE5,0)
 CALL WDIALOGFIELDSTATE(ID_FILE6,0)
 FNAME = ''
 CALL WDIALOGPUTSTRING(ID_FILE5,FNAME)
ELSEIF (NRSEL.EQ.1) THEN
 CALL WDIALOGSELECT(IFLD)
 CALL WDIALOGFIELDSTATE(ID_OPEN5,1)
 CALL WDIALOGFIELDSTATE(ID_OPEN6,1)
 CALL WDIALOGFIELDSTATE(ID_FILE5,1)
 CALL WDIALOGFIELDSTATE(ID_FILE6,1)
! CALL WDIALOGGETSTRING(ID_FILE5,FNAME)
! IF (TRIM(FNAME).EQ.'') THEN
 IF (IFLD.EQ.ID_DROSCENT2) THEN
  FNAME = TRIM(RESULTDIR)//TRIM(RONAME(J))//'_SCEN.IDF'
 ELSEIF (IFLD.EQ.ID_DROSCENT3) THEN
  FNAME = TRIM(RESULTDIR)//TRIM(RONAME(J))//'_REF.IDF'
 ENDIF
 CALL WDIALOGPUTSTRING(ID_FILE5,FNAME)
! ENDIF
ELSEIF (NRSEL.EQ.0) THEN
 ISTATE = 0
ENDIF

CALL WDIALOGSELECT(ID_DROSCEN)
CALL WDIALOGTABSTATE(ID_TAB,ID_DROSCENT2,ISTATE)
IF (ISTATE.EQ.0) JSTATE = ISTATE
CALL WDIALOGTABSTATE(ID_TAB,ID_DROSCENT3,JSTATE)

END SUBROUTINE ROSCEN1FIELDS

!###======================================================================
SUBROUTINE ROSCENREINITPREFS()
!###======================================================================
IMPLICIT NONE
INTEGER  :: I,CNT
INTEGER,DIMENSION(MAXOPT) :: IOPTIONS

CALL WDIALOGSELECT(ID_DROSCENT1)
CALL WDIALOGGETMENU(IDF_MENU2,IOPTIONS)

CNT = 0
DO I=1,MAXOPT
 IF (IOPTIONS(I).EQ.1) THEN
  CNT = CNT+1
  SCENPREF(I)%IACT = 1
  REFPREF(I)%IACT = 1
 ELSE
  SCENPREF(I)%IACT = 0
  REFPREF(I)%IACT = 0
 ENDIF
ENDDO

IF (CNT.EQ.0) THEN
 CALL WDIALOGSELECT(ID_DROSCEN)
 CALL WDIALOGTABSTATE(ID_TAB,ID_DROSCENT2,0)
ENDIF

END SUBROUTINE ROSCENREINITPREFS

!###======================================================================
SUBROUTINE ROSCENOK()
!###======================================================================
IMPLICIT NONE
INTEGER	:: I,ISTATE,IOPTIONS(MAXOPT),NRSEL,L,WU,DU,CODE,RSTATE
CHARACTER(LEN=256) :: OUTPUTDIR,FNAME,OUTPUTIDF
LOGICAL	:: LOK
REAL,DIMENSION(2) :: AREAS
REAL :: SFACT

!## RDOELMIN only used in IRRO tool, not in RO tool
RDOELMIN = 0; AREAS    = 0
!## carry out check for existence of the directory in case user pasted a string
!## a check routine has to be build checking all directories and files for existence (if necessary)
!## Also double click events have to be catched.
CALL ROSCENCHECKOK(LOK); IF(.NOT.LOK)RETURN

!## read out the dialogs DROSCENT2 and DROSCENT3 and fill refpref and scenpref using
!## selection of subtypes on DROSCENT1 (these are already stored in previously mentioned arrays).
CALL WDIALOGLOAD(ID_DROSCEN,ID_DROSCEN); CALL WDIALOGSELECT(ID_DROSCENT1)

!## compare with reference situation or not
CALL WDIALOGGETCHECKBOX(IDF_CHECK1,ISTATE)
IF (ISTATE.EQ.0) CODE = 1
IF (ISTATE.EQ.1) CODE = 2

!## with or without sprinkling
CALL WDIALOGGETCHECKBOX(IDF_CHECK3,RSTATE)
IF (RSTATE.EQ.0) SFACT = 1.0
IF (RSTATE.EQ.1) SFACT = 0.2	!80% vermindering van droogschade als gevolg van beregening

!## Determine if there is more then one selection in idf_menu2
!## It is not possible to switch HLP tables between calculations of reference or scenario situation
CALL WDIALOGGETMENU(IDF_MENU2,IOPTIONS)
NRSEL = 0
DO I=1,MAXOPT
 IF(SCENPREF(I)%IACT.EQ.1) THEN
  NRSEL = NRSEL + 1
 END IF
ENDDO

CALL WINDOWOUTSTATUSBAR(4,'')
!## Retrieve outputdirectory from idf_file1
CALL WDIALOGGETSTRING(ID_FILE1,OUTPUTDIR)
!## check for existence of outputdir

IF(ALLOCATED(RESULTIDFS))DEALLOCATE(RESULTIDFS); ALLOCATE(RESULTIDFS(2,MAXOPT)); RESULTIDFS=''

!## allocate REFCOSTS, SCENCOST arrays to gather all cost variables
!## note: With respect to the original version of RO.f90 the choice is to create (dependend on the
!## users choice) 2 arrays, resp. an array for the scenario situation and one for the reference situation
!##  - 1. Doelrealisatie type /subtype (size is 6, no of subtypes)
!##  - 2. no of croptypes (size is maxhlpc in the original version, but has to be extended to maxhlpc+ndttypes+urbantypes)
!##  - 3. size is 4 holding resp.:
!## 		-> 1, total area for each crop (ha)
!## 		-> 2, maximum yield (euro/ha/yr)
!## 		-> 3, area with damage (ha)
!## 		-> 4, damage (euro/ha/yr)

DO I=1,MAXOPT
 IF (SCENPREF(I)%IACT.EQ.1) THEN
  !## Call subroutine to initialise ro array, determine dimensions of resultfile and create it
  CALL ROSCENINITROARRAY(I,CODE,LOK,NRSEL)
  IF (.NOT.LOK) THEN
   RETURN
  ENDIF

  !## Name of the export file
  FNAME=TRIM(OUTPUTDIR)//TRIM(RONAME(I))//'.CSV'

  IF (I.LE.4) THEN	!## AGRICULTURE
   IF (.NOT.ALLOCATED(LGNLUT)) CALL ROFILLLUTAGRICULTURE(SCENPREF(I)%LUT2)
   IF (ALLOCATED(ROSCENCOSTS)) DEALLOCATE(ROSCENCOSTS)
   ALLOCATE(ROSCENCOSTS(CODE,MAXHLPC,4)); ROSCENCOSTS=0.0

   WU=UTL_GETUNIT(); CALL OSD_OPEN(WU,FILE=SCENPREF(I)%LUT1,STATUS='OLD',ACTION='READ,DENYWRITE', &
      FORM='UNFORMATTED',ACCESS='DIRECT',RECL=1)

   !## Call subroutine to determine realisation map
   DO L=1,CODE
    SELECT CASE (I)
     CASE (1,3)	
      CALL ROSCENLB(I,WU,L,1,SFACT)
     CASE (2,4)	
      CALL ROSCENLB(I,WU,L,1,SFACT)
    END SELECT
    CALL IDFINIT(RESULTIDFS(L,I),LEGNAME=TRIM(LEGEND(L)),LPLOT=.TRUE.)
   ENDDO
   CLOSE(WU)

   !## CALL PROCEDURE TO CREATE MAP WITH DIFFERENCES
   IF (CODE.EQ.2) THEN
    OUTPUTIDF = TRIM(OUTPUTDIR)//TRIM(RONAME(I))//'_DIFF.idf'
    CALL ROSCENDIFF(OUTPUTIDF,I,AREAS)
   ENDIF

   CALL ROSCENREPORT(FNAME,CODE,I,AREAS)

  ELSEIF (I.EQ.5) THEN	!## NATURE
   !Fill lut arrays which are defined as global variables
   IF (.NOT.ALLOCATED(NDTLUT)) CALL ROSCENNDTREADLUT(SCENPREF(I)%LUT3)
   IF (.NOT.ALLOCATED(IGLD)) CALL ROARRNDT(SCENPREF(I)%LUT1,SCENPREF(I)%LUT2)
   IF (ALLOCATED(ROSCENCOSTS))DEALLOCATE(ROSCENCOSTS); ALLOCATE(ROSCENCOSTS(CODE,MAXINDT,4))
   ROSCENCOSTS = 0

   !## Call subroutine to determine realisation map
   !## In case of realisation NATURE this can be done by same procedure as for
   !## agriculature, at least in case of requesting base maps for values.
   !## the variable I takes care of the right switch in requested procedures.
   !## The procudure to determine the realisation values for NATURE use several
   !## array's declared in de imod.f90 rovar module
   DO L=1,CODE
    WU = 1 !## DUMMY VALUE, In case of nature no HELP table is necessary
    CALL ROSCENLB(I,WU,L,1,SFACT)
    !## Load the map in the view
    CALL IDFINIT(RESULTIDFS(L,I),TRIM(LEGEND(L)))
   ENDDO

   !## CALL PROCEDURE TO CREATE MAP WITH DIFFERENCES
   IF (CODE.EQ.2) THEN
    OUTPUTIDF = TRIM(OUTPUTDIR)//TRIM(RONAME(I))//'_DIFF.idf'
    CALL ROSCENDIFF(OUTPUTIDF,I,AREAS)
   ENDIF

   !## Create report
   CALL ROSCENREPORTNDT(FNAME,CODE,I,AREAS)

  ELSEIF (I.EQ.6) THEN	!## URBAN

   IF (ALLOCATED(ROSCENCOSTS))DEALLOCATE(ROSCENCOSTS); ALLOCATE(ROSCENCOSTS(CODE,3,1))
   ROSCENCOSTS = 0
   !## call subroutine to calculate the doelrealisatie
   DO L=1,CODE
    CALL ROSCENURBCALC(L)

    !## Load the map in the view
    CALL IDFINIT(RESULTIDFS(L,I),TRIM(LEGEND(L)))

   ENDDO

   !## CALL PROCEDURE TO CREATE MAP WITH DIFFERENCES
   IF (CODE.EQ.2) THEN
    OUTPUTIDF = TRIM(OUTPUTDIR)//TRIM(RONAME(I))//'_DIFF.idf'
    CALL ROSCENDIFF(OUTPUTIDF,I,AREAS)
   ENDIF

   CALL ROSCENREPORTURB(FNAME,CODE,AREAS)
  ENDIF
 ENDIF
ENDDO

!## At this stage at least 1 file is available and already put into the view
!## in this case only a table with the yield (in ha or euro, see functional design) for exact
!## list of demands

!## if code = 2 (determine doelrealisatie including a map with differences) then at least 2 files
!## are available and put into the view. In this case a difference file has to be created including
!## 3 tables with yields (reference situation, scenario situation, difference file).

END SUBROUTINE ROSCENOK

!###======================================================================
SUBROUTINE ROARRNDT(FNAME,FNAMERFC)
!###======================================================================
IMPLICIT NONE

CHARACTER(LEN=*),INTENT(IN)	:: FNAME,FNAMERFC
INTEGER		:: IOS, JU, K, INDT, IRFC

!##OPEN FNAME HOLDING FILE WITH BOUNDARY CONDITIONS FOR NATUURDOELTYPE
JU = UTL_GETUNIT(); OPEN(JU,FILE=FNAME,STATUS='OLD',ACTION='READ')

IF (ALLOCATED(IGLD)) DEALLOCATE(IGLD)
IF (ALLOCATED(IKWL)) DEALLOCATE(IKWL)
IF (ALLOCATED(INDR)) DEALLOCATE(INDR)
IF (ALLOCATED(GVGRVW)) DEALLOCATE(GVGRVW)
IF (ALLOCATED(GLGRVW)) DEALLOCATE(GLGRVW)
IF (ALLOCATED(DSTRVW)) DEALLOCATE(DSTRVW)
ALLOCATE(IGLD(MAXINDT))
ALLOCATE(IKWL(MAXINDT))
ALLOCATE(INDR(MAXINDT))
ALLOCATE(GVGRVW(MAXINDT,MXC))
ALLOCATE(GLGRVW(MAXINDT,MXC))
ALLOCATE(DSTRVW(MAXINDT,MXC))
IGLD = 0
IKWL = 0
INDR = 0
GVGRVW = 0
GLGRVW = 0
DSTRVW = 0

READ(JU,'(A)')  !Header
DO WHILE(.TRUE.)
  READ(JU,*,IOSTAT=IOS) INDT, &
      (GVGRVW(INDT,K),K=1,MXC), (GLGRVW(INDT,K),K=1,MXC), (DSTRVW(INDT,K),K=1,MXC), &
      IGLD(INDT), IKWL(INDT), INDR(INDT)
  IF(IOS.NE.0) EXIT
END DO
CLOSE(JU)

!## WHERE(GVGRVW>NODATA) GVGRVW=0.01*GVGRVW ! CONVERSION FROM CM --> M
!## WHERE(GLGRVW>NODATA) GLGRVW=0.01*GLGRVW ! CONVERSION FROM CM --> M
!## ALL VALUES ARE IN CM-MV

!## OPEN FILE WITH REPROFUNCTIES (B-M-C WAARDEN) AND STORE IN ARRAYS'
JU = UTL_GETUNIT(); OPEN(JU,FILE=FNAMERFC,STATUS='OLD',ACTION='READ')
IF (ALLOCATED(B)) DEALLOCATE(B); IF (ALLOCATED(M)) DEALLOCATE(M); IF (ALLOCATED(C)) DEALLOCATE(C)
ALLOCATE(B(MXRFC)); ALLOCATE(M(MXRFC)); ALLOCATE(C(MXRFC))
READ(JU,*) !header
DO WHILE(.TRUE.)
 READ(JU,*,IOSTAT=IOS) IRFC, B(IRFC),M(IRFC),C(IRFC)
 IF(IOS.NE.0) EXIT
END DO
CLOSE(JU)

END SUBROUTINE ROARRNDT

!###======================================================================
SUBROUTINE ROSCENURBCALC(CODE)
!###======================================================================
IMPLICIT NONE
INTEGER,INTENT(IN):: CODE
INTEGER :: ICOL, IROW, IREC
REAL :: YC, XC, RGHG, DR, LGNV,LRANGE,URANGE,PERC, AREA,DRMIN, DRMAX
REAL,DIMENSION(2) :: AREAS

!## call subroutine to readout lut and to gather lrange and urange
CALL ROSCENURBREADLUT(SCENPREF(6)%LUT1,LRANGE,URANGE)
IF (LRANGE.EQ.-9999.0) THEN
 RETURN
ENDIF
AREA = (ROSCEN(CODE,5)%DX*ROSCEN(CODE,5)%DY)/10000.0
AREAS = 0
DRMIN= 10.0E10
DRMAX=-10.0E10
YC=ROSCEN(CODE,5)%YMAX+(0.5*ROSCEN(CODE,5)%DY)
DO IROW=1,ROSCEN(CODE,5)%NROW
 YC=YC-ROSCEN(CODE,5)%DX
 XC=ROSCEN(CODE,5)%XMIN-(0.5*ROSCEN(CODE,5)%DX)
 DO ICOL=1,ROSCEN(CODE,5)%NCOL
  XC=XC+ROSCEN(CODE,5)%DX

  RGHG = ROSCEN(CODE,1)%NODATA
  LGNV = ROSCEN(CODE,3)%NODATA
  !## get proper ghg-value (from reference)
  RGHG =IDFGETXYVAL(ROSCEN(CODE,1),XC,YC)

  !## retrieve lgn value
  LGNV = IDFGETXYVAL(ROSCEN(CODE,3),XC,YC) 

  DR = ROSCEN(CODE,5)%NODATA
  IF ((LGNV.EQ.18.0.OR.LGNV.EQ.19.0).AND.RGHG.NE.ROSCEN(CODE,1)%NODATA) THEN
   IF (RGHG.LE.LRANGE) THEN
    DR = 100.0; ROSCENCOSTS(CODE,1,1) = ROSCENCOSTS(CODE,1,1)+AREA
   ELSEIF(RGHG.GT.LRANGE.AND.RGHG.LE.URANGE) THEN
    DR = 50.0; ROSCENCOSTS(CODE,2,1) = ROSCENCOSTS(CODE,2,1)+AREA
   ELSEIF (RGHG.GT.URANGE) THEN
    DR = 0.1; ROSCENCOSTS(CODE,3,1) = ROSCENCOSTS(CODE,3,1)+AREA
   ENDIF

   DRMIN = MIN(DRMIN,DR)
   DRMAX = MAX(DRMAX,DR)
  ENDIF
  
  IREC=ICF+12+(IROW-1)*ROSCEN(CODE,5)%NCOL+ICOL
  WRITE(ROSCEN(CODE,5)%IU,REC=IREC) DR		      
  PERC=100.0*REAL(IROW)/REAL(ROSCEN(CODE,5)%NROW)
  IF(MOD(PERC,1.0).EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Calculating '//TRIM(RESULTIDFS(CODE,6))//' , progress: '//TRIM(RTOS(PERC,'F',2))//'%')

 ENDDO
ENDDO

ROSCEN(CODE,5)%DMIN=DRMIN
ROSCEN(CODE,5)%DMAX=DRMAX

IF(IDFWRITEDIM(0,ROSCEN(CODE,5)))THEN; ENDIF

END SUBROUTINE ROSCENURBCALC

!###======================================================================
SUBROUTINE ROSCENURBREADLUT(LUT,LRANGE,URANGE)
!###======================================================================
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN)	:: LUT
LOGICAL :: LEX
REAL,INTENT(OUT) :: LRANGE,URANGE
INTEGER :: JU

!## READ URBAN_RANGE.LUT TO RETRIEVE RANGES VALUES ARE IN METERS BELOW SURFACE
LEX=.FALSE.; INQUIRE(FILE=LUT,EXIST=LEX)

IF (.NOT.LEX) THEN
 CALL WMESSAGEBOX(OKONLY,ExclamationIcon,CommonOK ,'File '//TRIM(LUT)//' not found.','iMOD error')
 LRANGE = -9999.
 RETURN
ENDIF
JU=UTL_GETUNIT(); OPEN(JU,FILE=LUT,STATUS='OLD',ACTION='READ')
READ(JU,*) !HEADER
READ(JU,*) LRANGE
READ(JU,*) URANGE
CLOSE(JU)

IF (LRANGE.GE.URANGE) THEN
 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Lower range value larger then upper range, please check '//TRIM(LUT),'iMOD ERROR')
 LRANGE = -9999.
ENDIF

END SUBROUTINE ROSCENURBREADLUT

!###======================================================================
SUBROUTINE ROSCENDIFF(OUTPUTDIR,IEFFECT,AREAS)
!###======================================================================
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN)	:: OUTPUTDIR
INTEGER,INTENT(IN) :: IEFFECT
REAL,INTENT(OUT) :: AREAS(2)
CHARACTER(LEN=256) :: FNAMES(2)
CHARACTER(LEN=80) :: STRING
INTEGER :: I,J,IROW,ICOL,IREC,IRAT1,IRAT
REAL :: DRMIN,DRMAX,INITD,IRD,E,YC,XC,AREA
LOGICAL :: LOKAY
TYPE(IDFOBJ),DIMENSION(:),ALLOCATABLE :: MATH

!allocate math array
IF(ALLOCATED(MATH))DEALLOCATE(MATH); ALLOCATE(MATH(3))

!array resultidf composed of scenario resp. reference result idfs
!Calculation of difference has to be reference result - scenario result
!or math(1) = scenario (resultidf(2) and math(2)= reference resultidf(1)
FNAMES(1) = RESULTIDFS(1,IEFFECT)  !scenario
FNAMES(2) = RESULTIDFS(2,IEFFECT)  !reference

DO I=1,2
 MATH(I)%FNAME = FNAMES(I); IF(IDFREAD(MATH(I),MATH(I)%FNAME,0))THEN; ENDIF
ENDDO

MATH(3)%FNAME = TRIM(OUTPUTDIR)
IF(IDFOPEN(MATH(3)%IU,MATH(3)%FNAME,'W',0))THEN; ENDIF 
MATH(3)%DX    = MATH(1)%DX
MATH(3)%DY    = MATH(1)%DY
MATH(3)%NODATA= MATH(1)%NODATA
MATH(3)%XMIN  = MATH(1)%XMIN
MATH(3)%XMAX  = MATH(1)%XMAX
MATH(3)%YMIN  = MATH(1)%YMIN
MATH(3)%YMAX  = MATH(1)%YMAX
MATH(3)%NCOL  =(MATH(3)%XMAX-MATH(3)%XMIN)/MATH(3)%DX
MATH(3)%NROW  =(MATH(3)%YMAX-MATH(3)%YMIN)/MATH(3)%DY
DRMIN= 10.0E10
DRMAX=-10.0E10

!## set area (ha)
AREA = (MATH(1)%DX*MATH(1)%DX)/10000.0
IRAT = 0
IRAT1 = IRAT
YC=MATH(3)%YMAX+(0.5*MATH(3)%DY)
DO IROW=1,MATH(3)%NROW
 YC=YC-MATH(3)%DX
 XC=MATH(3)%XMIN-(0.5*MATH(3)%DX)
 DO ICOL=1,MATH(3)%NCOL
  XC=XC+MATH(3)%DX
  !## retrieve doelrealisatie scenario
  IRD = IDFGETXYVAL(MATH(1),XC,YC)
  !## retrieve doelrealisatie reference
  INITD = IDFGETXYVAL(MATH(2),XC,YC) 
  IF(INITD.EQ.MATH(1)%NODATA.AND.IRD.EQ.MATH(2)%NODATA) THEN
   IRD = 0.0
   INITD = 0.0
  ELSEIF(INITD.EQ.MATH(1)%NODATA) THEN
   INITD = 0.0
  ELSEIF(IRD.EQ.MATH(2)%NODATA) THEN
   IRD = 0.0
  ENDIF

  E =INITD - IRD	!## reference - scenario
  SELECT CASE (IEFFECT)
   CASE (3,4,5)
    IF (E.GT.0.0) THEN
     AREAS(1) = AREAS(1)+AREA	!1 = VERBETERING AREAAL
    ELSEIF (E.LT.0.0) THEN
     AREAS(2) = AREAS(2)+AREA   !2 = VERSLECHTERING AREAAL
    ENDIF
   CASE (1,2)
    IF (E.LT.0.0) THEN
     AREAS(1) = AREAS(1)+AREA	!1 = VERBETERING AREAAL
    ELSEIF (E.GT.0.0) THEN
     AREAS(2) = AREAS(2)+AREA   !2 = VERSLECHTERING AREAAL
   ENDIF
  END SELECT

  IF(E.NE.0.0) THEN
   DRMIN=MIN(DRMIN,E)
   DRMAX=MAX(DRMAX,E)
  ELSE
   E = MATH(3)%NODATA
  ENDIF

  IREC=ICF+12+(IROW-1)*MATH(3)%NCOL+ICOL
  WRITE(MATH(3)%IU,REC=IREC) E
  CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW,MATH(3)%NROW,'Progress ')
  
 ENDDO
ENDDO

MATH(3)%DMIN=DRMIN
MATH(3)%DMAX=DRMAX

!## write dimensions
IF(IDFWRITEDIM(0,MATH(3)))THEN; ENDIF

!## Draw differences IDF without any legend
CALL IDFINIT(MATH(3)%FNAME,'')

DO J=1,3; CLOSE(MATH(J)%IU); END DO

IF(ALLOCATED(MATH))DEALLOCATE(MATH)

END SUBROUTINE ROSCENDIFF

!###======================================================================
SUBROUTINE ROSCENINITROARRAY(I,Y,LOK,NRSEL)
!###======================================================================
IMPLICIT NONE
INTEGER,INTENT(IN)	:: I,Y,NRSEL
LOGICAL,INTENT(OUT)	:: LOK
LOGICAL :: LEX
INTEGER :: J,K,X,IFLD,CODE,NR,NC
CHARACTER(LEN=256) :: FNGHG,FNGLG,FNLUSE,RESULTDIR,FNAME
CHARACTER(LEN=5) :: PREFIX
REAL :: MINX,MAXX,MINY,MAXY,CSA

!## SELECT DIALOG ID_DROSCEN
CALL WDIALOGLOAD(ID_DROSCEN)

!## SELECT TAB PAGE ID_DROSCENT1 AND GET RESULT DIRECTORY
CALL WDIALOGSELECT(ID_DROSCENT1)
CALL WDIALOGGETSTRING(ID_FILE1,RESULTDIR)

!## ALLOCATE IDFS USING MAXSIZE
IF (ALLOCATED(IDFS)) DEALLOCATE(IDFS); ALLOCATE(IDFS(2,5)); IDFS = ''

!## Allocate ro array roscen using maxsize. This array holds all filenames used in further
!## determination of the doelrealisatie per subtype (Per subtype this array will be initialised).
IF (ALLOCATED(ROSCEN)) DEALLOCATE(ROSCEN); ALLOCATE(ROSCEN(2,5))
DO J=1,2; DO K=1,5; CALL IDFNULLIFY(ROSCEN(J,K)); ENDDO; ENDDO

!## 1 for scenario situation and 2 for reference situation
!## And 5 for all subtypes. For every subtype the entire extent and all inputfiles (only geographic
!## maps are present (LGN, SOIL). This will be initialised completed for every subtype
!## by routine ROSCENINITROARRAY. The 5th place is exclusively meant for the outputfile and its
!## dimensions

DO CODE=1,Y
 IF (CODE.EQ.1) THEN
  CALL WDIALOGSELECT(ID_DROSCENT2)
  PREFIX = '_SCEN'
 ELSEIF(CODE.EQ.2) THEN
  CALL WDIALOGSELECT(ID_DROSCENT3)
  PREFIX = '_REFF'
 ENDIF

 !## if code = 1 then fname legend of scenario situation is retrieved
 CALL WDIALOGGETSTRING(ID_FILE6,FNAME)
 IF (TRIM(FNAME).NE.'')THEN 
  LEGEND(CODE) = FNAME
 ELSE
  LEGEND(CODE) = ''
 ENDIF

 !## Retrieve all files needed in the determination of doelrealisatie
 !## GHG filename
 CALL WDIALOGGETSTRING(ID_FILE1,FNGHG)
 IF (TRIM(FNGHG).EQ.'') THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Please enter a filename for GHG','iMOD message')
  LOK = .FALSE.
  RETURN
 ELSE
  INQUIRE(FILE=FNGHG,EXIST=LEX)
  IF (.NOT.LEX) THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'File '//TRIM(FNGHG)//' not found','iMOD error')
   LOK=.FALSE.
   RETURN
  ELSE
   IDFS(CODE,1) = FNGHG
  ENDIF
 ENDIF

 !## In case urban area is selected then no glg is needed,
 !## For coding reasons GLG is set to GHG.
 IF (I.EQ.6) THEN
   IDFS(CODE,2) = IDFS(CODE,1)
 ELSE
  !## GLG filename
  CALL WDIALOGGETSTRING(ID_FILE2,FNGLG)
  IF (TRIM(FNGLG).EQ.'') THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Please enter a filename for GLG','iMOD message')
   LOK = .FALSE.
   RETURN
  ELSE
   INQUIRE(FILE=FNGLG,EXIST=LEX)
   IF(.NOT.LEX) THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'File '//TRIM(FNGLG)//' not found','iMOD error')
    LOK=.FALSE.
    RETURN
   ELSE
    IDFS(CODE,2) = FNGLG
   ENDIF
  ENDIF
  !## SOIL Name according to preference file
  IDFS(CODE,4) = SCENPREF(I)%SOIL
 ENDIF

 !## retrieve landuse file
 IF (I.EQ.5) THEN     !## IN CASE NATURE THEN OTHER FILE
  IFLD = ID_FILE4
 ELSE
  IFLD = ID_FILE3
 ENDIF
 !## SET LANDUSE (I.E. LGN5 IN CASE AGRICULTURE(I=1-4)OR URBAN (I=6) AND NDT IN CASE NATURE (I=5)
 CALL WDIALOGGETSTRING(IFLD,FNLUSE)
 IF (TRIM(FNLUSE).NE.'') THEN
  INQUIRE(FILE=FNLUSE,EXIST=LEX)
  IF(.NOT.LEX) THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'File '//TRIM(FNLUSE)//' not found','iMOD error')
   LOK=.FALSE.
   RETURN
  ELSE
   IDFS(CODE,3) = FNLUSE
  ENDIF
 ELSE
  IDFS(CODE,3) = SCENPREF(I)%LUSE
 ENDIF

 !## Construct filename of the resultfile
 IF (NRSEL.EQ.1) THEN
  CALL WDIALOGGETSTRING(ID_FILE5,FNAME)
  IF (TRIM(FNAME).EQ.'') THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No filename given, iMOD is bailing out','iMOD error')
    LOK = .FALSE.
    RETURN
  ELSE
   CALL UTL_CHECKNAME(FNAME,'idf')
   RESULTIDFS(CODE,I) = TRIM(FNAME)
   LOK = .TRUE.
  ENDIF
 ELSE
  RESULTIDFS(CODE,I) = TRIM(RESULTDIR)//TRIM(RONAME(I))//PREFIX//'.IDF'
 ENDIF

 !## Retrieve dimensions of all idfs, in case urban then only first 3 files are filled, of which only first
 !## and third is necessary. The second is a dummy idf.
 SELECT CASE (I)
  CASE (6)
   J = 3
  CASE DEFAULT
   J = 4
 END SELECT

 CALL ROCALCEXTENT((/IDFS(CODE,1),IDFS(CODE,2)/),2,MINX,MAXX,MINY,MAXY,CSA,NR,NC)

 DO X=1,J
  IF(IDFREAD(ROSCEN(CODE,X),IDFS(CODE,X),0))THEN; ENDIF
 ENDDO
 ROSCEN(CODE,5)%XMIN=MINX
 ROSCEN(CODE,5)%XMAX=MAXX
 ROSCEN(CODE,5)%YMIN=MINY
 ROSCEN(CODE,5)%YMAX=MAXY
 ROSCEN(CODE,5)%DX = CSA
 ROSCEN(CODE,5)%DY = CSA
 ROSCEN(CODE,5)%NCOL=NC
 ROSCEN(CODE,5)%NROW=NR
 ROSCEN(CODE,5)%NODATA=-9999.
 
 IF(.NOT.IDFOPEN(ROSCEN(CODE,5)%IU,RESULTIDFS(CODE,I),'WO',0,0))THEN; LOK = .FALSE. ; RETURN; ENDIF

ENDDO

END SUBROUTINE ROSCENINITROARRAY

!###======================================================================
SUBROUTINE ROFILLLUTAGRICULTURE(LUT)
!Fills lut for determining doelrealisatie agriculture
!Desrcription of the arrays that are used:
!- CCOSTS Array is used to ..?
!- TARR array contains descriptions of the landuse
!- LGNLUT array contains lut describing relation between
!  LGN5 values and HELP Crop values
!###======================================================================
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) 		:: LUT
!CHARACTER(LEN=50),DIMENSION(MAXHLPC)	:: TARR
INTEGER					:: JU, I,IOS,HCRP,CODELGN

!Array with descriptions
IF (ALLOCATED(DAGR)) DEALLOCATE(DAGR)
ALLOCATE(DAGR(MAXHLPC))
DAGR = ''

!Allocate and fill string array (only descriptions of the HLP croptypes)
!and fill ccost table with costs (Euro/ha/yr)
IF (ALLOCATED(LGNLUT)) DEALLOCATE(LGNLUT)
ALLOCATE(LGNLUT(MAXHLPC,2))
LGNLUT = 0

!Cost variables and variables for visualisation of results
IF (ALLOCATED(CCOSTS)) DEALLOCATE(CCOSTS); ALLOCATE(CCOSTS(MAXHLPC,2)); CCOSTS = 0

JU = UTL_GETUNIT()
OPEN(JU,FILE=LUT,STATUS='OLD')
READ(JU,'(A)')
I = 0
DO WHILE(.TRUE.)
 READ(JU,'(I2,I11,A33,F10.2,F14.2)',IOSTAT=IOS) HCRP,CODELGN,DAGR(HCRP),CCOSTS(HCRP,1),CCOSTS(HCRP,2)
 IF(IOS.NE.0) EXIT
 I = I + 1
 LGNLUT(I,1) = CODELGN
 LGNLUT(I,2) = HCRP
END DO
CLOSE(JU)

END SUBROUTINE ROFILLLUTAGRICULTURE

!###======================================================================
SUBROUTINE ROSCENLB(ST,JU,CODE,CALC,SFACT)
!Input is only RESULTIDF. This file is taken up in RO (5)
!Input:
! ST   = subtype (i.e. natschade landbouw (4 typen) en natuur)
! JU   = unit number of HELP TABLE
! CODE = indicates whether calculations are performed for scenario (1) or
!        reference situation (2)
! CALC = Indicates the type of doelrealisatie determination for nature (0 is average, 1 is the product
!        of GLGDOEL, GVGDOEL and DSTDOEL
! SFACT= Sprinkling factor (currently set to 80 % reduce of droogschade)

!Input for calculations
! - GLG (meter - surface level)
! - GHG (meter - surface level)
! - LGN (LGN 5 values)
! - HLPSOIL (reclassified soilmap using database with 6110 1:50.000 soil \
!            map values). Reclassified map is converted to raster.
! - JU (HELP lookuptable with for each combination of SOIL, LGN, GHG and GLG
!   a damage value).
!###======================================================================
IMPLICIT NONE
INTEGER,INTENT(IN):: ST,JU,CODE,CALC	
REAL,INTENT(IN)	:: SFACT				
INTEGER :: METEOF,ICOL, IROW, I,IREC,NRW,NCOL,IRAT,IRAT1,AS 
REAL :: YC, XC, RGLG, RGHG, DR, CODELGN
REAL :: RS, RC, AREA,RATIO,CS,CS1
REAL :: DRMIN, DRMAX
REAL :: GEMDOEL, GLGDOEL, DSTDOEL, GVGDOEL, KWLDOEL,NDT
REAL :: LIMVMIN,LIMMIN, LIMMAX, LIMVMAX,LIMVAL(4)
REAL,ALLOCATABLE,DIMENSION(:) :: VALS

SELECT CASE(ST)
 CASE(4)
  METEOF = 1.1
 CASE DEFAULT
  METEOF = 1.0
END SELECT

!## set drmin and drmax
DRMIN= 10.0E10
DRMAX=-10.0E10
AREA = (ROSCEN(CODE,5)%DX*ROSCEN(CODE,5)%DY)/10000.0	!area (ha) for each cell

!ROSCEN is an type array which contains dimensions of 5 idfs which are in the following order:
! 1 - GHG idf
! 2 - GLG idf
! 3 - LUSE idf (differs in case calculation doelrealisatie nature is carried out)
! 4 - SOIL idf (differs in case calculation doelrealisatie nature is carried out)
! 5 - result idf
!FOR EVERY CELL DETERMINE VALUE, IN CASE ALL EXTENTS ARE SIMILAR
YC=ROSCEN(CODE,5)%YMAX+(0.5*ROSCEN(CODE,5)%DY)
NRW = ROSCEN(CODE,5)%NROW
NCOL = ROSCEN(CODE,5)%NCOL

RATIO=1.0
DO I=1,4
 CS   =ROSCEN(CODE,5)%DX
 CS1  =ROSCEN(CODE,I)%DX
 RATIO=MAX(RATIO,CS/CS1)
ENDDO
AS=NINT(RATIO**2) 		
IF(ALLOCATED(VALS))DEALLOCATE(VALS); ALLOCATE(VALS(AS))

DO IROW=1,NRW
 YC=YC-ROSCEN(CODE,5)%DX
 XC=ROSCEN(CODE,5)%XMIN-(0.5*ROSCEN(CODE,5)%DX)
 DO ICOL=1,NCOL
  XC=XC+ROSCEN(CODE,5)%DX
  RGHG  = ROSCEN(CODE,1)%NODATA
  RGLG  = ROSCEN(CODE,2)%NODATA

  !## get proper ghg-value (from reference)
  RGHG=IDFGETAGGREGATEDVAL(ROSCEN(CODE,1),XC,YC,ROSCEN(CODE,5)%DX,ROSCEN(CODE,1)%DX,VALS,AS,2) 
  !## get proper glg-value (from reference)
  RGLG=IDFGETAGGREGATEDVAL(ROSCEN(CODE,2),XC,YC,ROSCEN(CODE,5)%DX,ROSCEN(CODE,2)%DX,VALS,AS,2) 
  !## retrieve lgn value and use lgnlut to determine help croptype
  RC=IDFGETAGGREGATEDVAL(ROSCEN(CODE,3),XC,YC,ROSCEN(CODE,5)%DX,ROSCEN(CODE,3)%DX,VALS,AS,1) 
  !## retrieve soil (help soil code (range 1-74)) or RFC Soils
  RS=IDFGETAGGREGATEDVAL(ROSCEN(CODE,4),XC,YC,ROSCEN(CODE,5)%DX,ROSCEN(CODE,4)%DX,VALS,AS,1) 

  DR=ROSCEN(CODE,5)%NODATA
  IF(INT(RC).NE.INT(ROSCEN(CODE,3)%NODATA).AND.&
     INT(RS).NE.INT(ROSCEN(CODE,4)%NODATA).AND.&
     INT(RS).GT.0.AND.INT(RC).GT.0)THEN

   IF (ST.LE.4) THEN  !## Only in case of agriculture
    CODELGN =ROSCEN(CODE,3)%NODATA
    DO I = 1,MAXHLPC
     IF (LGNLUT(I,1).EQ.RC) THEN
      CODELGN = REAL(LGNLUT(I,2)); EXIT
     ENDIF
    ENDDO

    !## Total areal for each crop type within total extent in case a calculation can be made
    IF(CODELGN.NE.ROSCEN(CODE,3)%NODATA.AND.CODELGN.GT.0.0) &
      ROSCENCOSTS(CODE,INT(CODELGN),1)=ROSCENCOSTS(CODE,INT(CODELGN),1)+AREA

   ENDIF
   
   !## retrieve damage for initial GLG, GHG
   IF(INT(RGLG).NE.INT(ROSCEN(CODE,2)%NODATA).AND.&
      INT(RGHG).NE.INT(ROSCEN(CODE,1)%NODATA)) THEN

     SELECT CASE (ST)
      CASE (1,2,3,4)	!AGRICULTURE
       !## unit conversion from M-MV to CM-MV
       RGLG=RGLG*100.0
       RGHG=RGHG*100.0
       IF (INT(CODELGN).GE.1.AND.INT(CODELGN).LE.14) THEN

         DR = HLPQUERY(JU,RGHG,RGLG,RS,CODELGN,ST)*METEOF
         IF (DR.GE.0.0) THEN
          IF (ST.EQ.2.OR.ST.EQ.4) THEN	
           DR = DR*SFACT
          ENDIF

          !## In case doelrealisatie is requested
          IF(ST.EQ.3.OR.ST.EQ.4) THEN
           DR=100.0-DR   

           !## Actual YIELD (in euro/yr)
           ROSCENCOSTS(CODE,INT(CODELGN),4) = ROSCENCOSTS(CODE,INT(CODELGN),4)+ &
                      ((DR/100) * CCOSTS(INT(CODELGN),1) * AREA * CCOSTS(INT(CODELGN),2))
          ELSE
           !## Actual YIELD (in euro/yr)
           ROSCENCOSTS(CODE,INT(CODELGN),4) = ROSCENCOSTS(CODE,INT(CODELGN),4)+ &
                      (((100.0-DR)/100) * CCOSTS(INT(CODELGN),1) * AREA * CCOSTS(INT(CODELGN),2))
          ENDIF


          !## areaal wat voldoet aan de norm in case rdoelmin.GE.0. IRRO tool
	      IF (DR.GE.REAL(RDOELMIN)) THEN
           ROSCENCOSTS(CODE,INT(CODELGN),2) = ROSCENCOSTS(CODE,INT(CODELGN),2)+AREA !AREA DAT VOLDOET AAN NORM
          ENDIF

          !## mean doelrealisatie RO TOOL
          ROSCENCOSTS(CODE,INT(CODELGN),3) = (ROSCENCOSTS(CODE,INT(CODELGN),3) + DR)/2

         ENDIF
       ELSE
         DR = ROSCEN(CODE,5)%NODATA
       ENDIF

      CASE (5)		!NATURE
       DR = ROSCEN(CODE,5)%NODATA

       IF (RC.GT.0.0.AND.RS.GT.0.0) THEN

        !## GHG en GLG in meters.
        CALL NDTQUERY(RGHG,RGLG,RS,RC,&
                 DR, GLGDOEL, DSTDOEL, GVGDOEL, KWLDOEL, &
                 LIMVMIN,LIMMIN,LIMVMAX,LIMMAX,LIMVAL,CALC)

	  !## Convert found ndt type to aggregated ndttype (if applicable, see ndt.lut,
      !## if ndt <> ndtaggr (2 column) then if loop below is used to find aggregated ndt, if
	  !## ndt = ndtaggr then no aggregation takes place
	  IF (NDTLUT(INT(RC)).NE.0) THEN
         NDT=REAL(NDTLUT(INT(RC)))

         !## Total areal for each crop type within total extent
         ROSCENCOSTS(CODE,INT(NDT),1) = ROSCENCOSTS(CODE,INT(NDT),1)+ AREA

         IF (DR.GE.0.0) THEN
        !## note for ro tool rdoelmin is set to 0
 	    !## Area for which damage amount is calculated
          if (DR.GE.RDOELMIN) ROSCENCOSTS(CODE,INT(NDT),2) = ROSCENCOSTS(CODE,INT(NDT),2)+ AREA
          ROSCENCOSTS(CODE,INT(NDT),3) = (ROSCENCOSTS(CODE,INT(NDT),3)+ DR)/2 
          ROSCENCOSTS(CODE,INT(NDT),4) = ROSCENCOSTS(CODE,INT(NDT),4)+ 1 
         ENDIF
        ENDIF 
       ENDIF
     END SELECT

     IF (DR.GE.0.0) THEN
      DRMIN = MIN(DRMIN,DR)
      DRMAX = MAX(DRMAX,DR)
     ELSE
      DR = ROSCEN(CODE,5)%NODATA
     ENDIF
   END IF
  ENDIF

  IREC=ICF+12+(IROW-1)* NCOL+ICOL
  WRITE(ROSCEN(CODE,5)%IU,REC=IREC) DR		      
  CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW,NRW,'Calculating '//TRIM(RESULTIDFS(CODE,ST))//' , progress')

 END DO
END DO

ROSCEN(CODE,5)%DMIN=DRMIN
ROSCEN(CODE,5)%DMAX=DRMAX

IF(IDFWRITEDIM(0,ROSCEN(CODE,5)))THEN; ENDIF

CLOSE(ROSCEN(CODE,5)%IU)

IF(ALLOCATED(VALS))DEALLOCATE(VALS); ALLOCATE(VALS(AS))

END SUBROUTINE ROSCENLB

!###======================================================================
SUBROUTINE ROSCENNDTREADLUT(LUT)
!###======================================================================
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN)	:: LUT  
INTEGER :: IU,NDT,IOS

IF (ALLOCATED(DAGR)) DEALLOCATE(DAGR); ALLOCATE(DAGR(MAXINDT)); DAGR= ''

IF (ALLOCATED(NDTLUT)) DEALLOCATE(NDTLUT)
ALLOCATE(NDTLUT(MAXINDT))
NDTLUT = 0

!## Allocate and fill string array (only descriptions of the NDT croptypes)
IU = UTL_GETUNIT()
OPEN(IU,FILE=LUT,STATUS='OLD')
READ(IU,'(A)')	
DO WHILE(.TRUE.)
  READ(IU,*,IOSTAT=IOS) NDT, NDTLUT(NDT),DAGR(NDT)
  IF(IOS.NE.0) EXIT
END DO
CLOSE(IU)

END SUBROUTINE ROSCENNDTREADLUT

!###======================================================================
SUBROUTINE ROSCENCHECKOK(LOK)
!###======================================================================
IMPLICIT NONE
LOGICAL,INTENT(OUT)	:: LOK
INTEGER :: I,NRSEL,TABS(2),JSTATE,IOPTIONS(MAXOPT),OPTS
CHARACTER(LEN=256) :: FNAME
CHARACTER(LEN=15) :: SUBMSG

LOK = .TRUE.

CALL WDIALOGSELECT(ID_DROSCENT1)
CALL WDIALOGGETCHECKBOX(IDF_CHECK1,JSTATE)
CALL WDIALOGGETMENU(IDF_MENU2,IOPTIONS)
NRSEL = 0
DO I=1,MAXOPT
 IF (IOPTIONS(I).EQ.1) THEN
  NRSEL = NRSEL+1
 ENDIF
ENDDO

IF (NRSEL.EQ.0) THEN
 LOK = .FALSE.
 RETURN
ELSE
 TABS(1) = ID_DROSCENT2
 TABS(2) = ID_DROSCENT3
 IF (JSTATE.EQ.0) OPTS=1
 IF (JSTATE.EQ.1) OPTS=2
 DO I=1,OPTS

  SELECT CASE (I)
   CASE (1)
    SUBMSG= 'scenario'
   CASE (2)
    SUBMSG= 'reference'
  END SELECT

  CALL WDIALOGSELECT(TABS(I))
  CALL WDIALOGGETSTRING(ID_FILE1,FNAME)
  IF (FNAME.EQ.'') THEN
   LOK=.FALSE.
   CALL WMESSAGEBOX(OKOnly,StopIcon,CommonOK,'Please select GHG file for '//TRIM(SUBMSG)//' situation.','iMOD message')
   RETURN
  ENDIF

  CALL WDIALOGGETSTRING(ID_FILE2,FNAME)
  IF (FNAME.EQ.'') THEN
    LOK=.FALSE.
    CALL WMESSAGEBOX(OKOnly,StopIcon,CommonOK,'Please select GLG file for '//TRIM(SUBMSG)//' situation.','iMOD message')
    RETURN
  ENDIF
 ENDDO
ENDIF

END SUBROUTINE ROSCENCHECKOK

!###======================================================================
SUBROUTINE ROSCENREPORT(FNAME,CODE,ST,AREAS)
!###======================================================================
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN)	:: FNAME
INTEGER,INTENT(IN) :: CODE,ST
REAL,INTENT(IN) :: AREAS(2)
CHARACTER(LEN=500) :: ASTRING,CLSTRING,MSG
CHARACTER(LEN=75),DIMENSION(:),ALLOCATABLE :: COLUMNTITLES
CHARACTER(LEN=50) :: RECVAL
INTEGER :: IU,I,NROWS,J,NT,NR,T,X,IOS
REAL :: AVAL
REAL,DIMENSION(4) :: SUMC

TYPE DIFARRAY
 CHARACTER(LEN=50)	:: LUSE
 REAL 			:: TA,PY,CA,DD
END TYPE DIFARRAY
TYPE(DIFARRAY),DIMENSION(:,:),ALLOCATABLE :: DIFS

!## In case code is 1:
!## - one table with area under consideration, potential yield, area for which damage is queried and costs
!## In case code is 2:
!## same as for code 1 completed with values for reference situation and difference. So 3 tables in stead of 1

IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='REPLACE',IOSTAT=IOS,ACTION='WRITE')
IF(IOS.NE.0) THEN
 CALL OSD_IOSTAT_MSG(IOS,MSG)
 CALL WMESSAGEBOX(OKOnly,ExclamationIcon,CommonOK,'Following error occurred: '//TRIM(MSG),'iMOD Error Message')
 RETURN
ENDIF

IF (ALLOCATED(COLUMNTITLES)) DEALLOCATE(COLUMNTITLES)
IF (ALLOCATED(DIFS)) DEALLOCATE(DIFS)
NT = 5

SELECT CASE (ST)
!## ST 1 = NATSCHADE
!## ST 2 = DROOGTESCHADE
!## ST 3 = DOELREALISATIE LANDBOUW NAT
!## ST 4 = DOELREALISATIE LANDBOUW DROOG
 CASE (1,2,3,4)
  !## agriculture
  NROWS = MAXHLPC
  ALLOCATE(COLUMNTITLES(NT))
  COLUMNTITLES(1) = 'gewastype'
  COLUMNTITLES(2) = 'totaal oppervlak (ha)'
  COLUMNTITLES(5) = 'actuele opbrengst (kEuro)'
  IF (ST.EQ.3.OR.ST.EQ.4) THEN
   COLUMNTITLES(3) = 'areaal waarvoor doelrealisatie is berekend (ha)'
   COLUMNTITLES(4) = 'gemiddelde doelrealisatie (%)'
  ELSE
   COLUMNTITLES(3) = 'areaal waarvoor schade percentage is berekend (ha)'
   COLUMNTITLES(4) = 'gemiddelde schade (%)'
  ENDIF
  ALLOCATE(DIFS(2,NROWS))
  DIFS(1:2,1:NROWS)%LUSE = ''
  DIFS(1:2,1:NROWS)%TA = 0.
END SELECT

!## write used files in first part of the file

!## Create columnstring
DO I=1,NT
 IF (I.EQ.1) THEN
  CLSTRING = TRIM(COLUMNTITLES(I))
 ELSE
  CLSTRING = TRIM(CLSTRING)//','//TRIM(COLUMNTITLES(I))
 ENDIF
ENDDO

WRITE(IU,*) 'Overzicht uitkomsten ro tool voor bepaling van '//TRIM(RONAME(ST))
DO J=1,CODE
 IF (J.EQ.1) WRITE(IU,'(/A)') 'Scenario situatie'
 IF (J.EQ.2) WRITE(IU,'(/A)') 'Referentie situatie'
 WRITE(IU,'(/A)') 'Parameter'//',Bestandsnaam'
 WRITE(IU,'(A)') ' - GHG'//','//TRIM(IDFS(J,1))
 WRITE(IU,'(A)') ' - GLG'//','//TRIM(IDFS(J,2))
 WRITE(IU,'(A)') ' - LANDUSE'//','//TRIM(IDFS(J,3))
 WRITE(IU,'(A)') ' - SOIL'//','//TRIM(IDFS(J,4))
 WRITE(IU,'(/A/)') TRIM(CLSTRING)
 SUMC = 0
 DO I=1,NROWS
  ASTRING = TRIM(DAGR(I))
  IF(ROSCENCOSTS(J,I,1).GT.0.0) THEN
   DIFS(J,I)%LUSE = TRIM(DAGR(I))
   DO NR=1,4
    AVAL = ROSCENCOSTS(J,I,NR)
    IF (NR.EQ.1) THEN
     DIFS(J,I)%TA = AVAL  !## TOTAL AREA
     SUMC(NR) = SUMC(NR)+AVAL
    ENDIF
    IF (NR.EQ.4) THEN
     DIFS(J,I)%PY = AVAL  !## actual yield
     SUMC(NR) = SUMC(NR)+AVAL
    ENDIF
    IF (NR.EQ.2) THEN
     DIFS(J,I)%CA = AVAL  !## areaal waarvoor schade is berekend
     SUMC(NR) = SUMC(NR)+AVAL
    ENDIF
    IF (NR.EQ.3) THEN
     DIFS(J,I)%DD = AVAL  !## average damage
     SUMC(NR) = (SUMC(NR)+AVAL)/2
    ENDIF
    CALL IREALTOSTRING(AVAL,RECVAL,'(F15.2)')
    ASTRING = TRIM(ASTRING)//','//TRIM(RECVAL)
   ENDDO
   WRITE(IU,'(A)') TRIM(ASTRING)
  ENDIF
 ENDDO
 !## write totals for each column
 WRITE(IU,*) ''
 ASTRING = 'TOTAAL'
 DO T=1,4
  CALL IREALTOSTRING(SUMC(T),RECVAL,'(F15.2)')
  ASTRING = TRIM(ASTRING)//','//TRIM(RECVAL)
 ENDDO
 WRITE(IU,'(A/)') TRIM(ASTRING)
ENDDO

SUMC = 0	!## RESET SUMC TO 0
IF (CODE.EQ.2) THEN
 SELECT CASE (ST)
  CASE (1,2,3,4)
   !## agriculture
   NROWS = MAXHLPC
   NT = 4 !5
   DEALLOCATE(COLUMNTITLES)
   ALLOCATE(COLUMNTITLES(NT))
   COLUMNTITLES(1) = 'Gewastype'
   COLUMNTITLES(2) = 'Totaal oppervlak (ha)'
   COLUMNTITLES(4) = 'actuele opbrengst (kEuro)'
   IF (ST.EQ.2.OR.ST.EQ.4) THEN
    COLUMNTITLES(3) = 'toe-/afname areaal met doelrealisatie (ha)'
   ELSE
    COLUMNTITLES(3) = 'areaal waarvoor schade percentage is berekend (ha)'
   ENDIF
 END SELECT

 !## Create columnstring
 DO I=1,NT
  IF (I.EQ.1) THEN
   CLSTRING = TRIM(COLUMNTITLES(I))
  ELSE
   CLSTRING = TRIM(CLSTRING)//','//TRIM(COLUMNTITLES(I))
  ENDIF
 ENDDO

 WRITE(IU,'(/A/)') 'Verschil'
 WRITE(IU,'(A)') TRIM(CLSTRING)
 DO X=1,NROWS
  IF (TRIM(DIFS(1,X)%LUSE).GT.'') THEN
   ASTRING = TRIM(DIFS(1,X)%LUSE)
   DO NR=1,3
    IF (NR.EQ.1) THEN
     AVAL = DIFS(1,X)%TA
     SUMC(NR) = SUMC(NR)+AVAL
    ENDIF
    IF (NR.EQ.2) THEN
     AVAL = DIFS(2,X)%CA - DIFS(1,X)%CA
     SUMC(NR) = SUMC(NR)+AVAL
    ENDIF
    IF (NR.EQ.3) THEN
     AVAL = DIFS(2,X)%DD - DIFS(1,X)%DD
     SUMC(NR) = SUMC(NR)+AVAL
    ENDIF
    CALL IREALTOSTRING(AVAL,RECVAL,'(F15.2)')
    ASTRING = TRIM(ASTRING)//','//TRIM(RECVAL)
   ENDDO
   WRITE(IU,'(A)') TRIM(ASTRING)
  ENDIF
 ENDDO

 !## WRITE TOTALS
 ASTRING = 'Totaal'
 DO T=1,3
  CALL IREALTOSTRING(SUMC(T),RECVAL,'(F15.2)')
  ASTRING = TRIM(ASTRING)//','//TRIM(RECVAL)
 ENDDO
 WRITE(IU,'(/A/)') TRIM(ASTRING)

 !## write down total decline/increase (not crop specified)
 WRITE(IU,*) ''
 WRITE(IU,'(A25,F10.2)') 'Areaal afname (ha) :,    ',areas(1)
 WRITE(IU,'(A25,F10.2)') 'Areaal toename (ha) :,   ',areas(2)
ENDIF
CLOSE(IU)

IF (ALLOCATED(COLUMNTITLES)) DEALLOCATE(COLUMNTITLES)
IF (ALLOCATED(DIFS)) DEALLOCATE(DIFS)

CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Table written to '//TRIM(FNAME),'iMOD Information')

END SUBROUTINE ROSCENREPORT

!###======================================================================
SUBROUTINE ROSCENREPORTNDT(FNAME,CODE,ST,AREAS)
!###======================================================================
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN)	:: FNAME
INTEGER,INTENT(IN) :: CODE,ST
REAL,INTENT(IN) :: AREAS(2)
CHARACTER(LEN=500) :: ASTRING,CLSTRING,MSG
CHARACTER(LEN=75),ALLOCATABLE :: COLUMNTITLES(:)
CHARACTER(LEN=50) :: RECVAL,TXT
INTEGER :: IU,I,J,NT,NR,T,X,IOS
REAL :: AVAL,SUMC(4)

TYPE DIFARRAY
 CHARACTER(LEN=50)	:: LUSE
 REAL :: TA,PY,CA,DD
END TYPE DIFARRAY
TYPE(DIFARRAY),DIMENSION(:,:),ALLOCATABLE :: DIFS

!In case code is 1:
!- one table with area under consideration, potential yield, area for which damage is queried and costs
!In case code is 2:
!same as for code 1 completed with values for reference situation and difference. So 3 tables in stead of 1

!Create file to write to
!CALL WSELECTFILE('Comma Seperated File (*.csv)|*.csv|',SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,&
!     FNAME,'Save as')
!IF(WINFODIALOG(4).NE.1)RETURN
IU = UTL_GETUNIT()
OPEN(IU,FILE=FNAME,STATUS='REPLACE',IOSTAT=IOS)
IF(IOS.NE.0) THEN
 CALL OSD_IOSTAT_MSG(IOS,MSG)
 CALL WMESSAGEBOX(OKOnly,ExclamationIcon,CommonOK,'Following error occurred: '//TRIM(MSG),'iMOD Error Message')
 RETURN
ENDIF

IF (ALLOCATED(COLUMNTITLES)) DEALLOCATE(COLUMNTITLES)
IF (ALLOCATED(DIFS)) DEALLOCATE(DIFS)
NT = 5

IF (ST.EQ.5) THEN  !nature
  ALLOCATE(COLUMNTITLES(NT))
  COLUMNTITLES(1) = 'NDT type'
  COLUMNTITLES(2) = 'Totaal areaal (ha)'
  COLUMNTITLES(3) = 'Areaal met doelrealisatie >= 0'
  COLUMNTITLES(4) = 'Gemiddelde doelrealisatie'
  COLUMNTITLES(5) = 'Aantal cellen'
  ALLOCATE(DIFS(2,MAXINDT))
ELSE
 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Wrong type of doelrealisatie.','iMOD error message')
 RETURN
ENDIF

!Create columnstring
DO I=1,NT
 IF (I.EQ.1) THEN
  CLSTRING = TRIM(COLUMNTITLES(I))
 ELSE
  CLSTRING = TRIM(CLSTRING)//','//TRIM(COLUMNTITLES(I))
 ENDIF
ENDDO


WRITE(IU,'(A)') 'Overzicht uitkomsten ro tool voor bepaling van '//TRIM(RONAME(ST))
DO J=1,CODE
 IF (J.EQ.1) WRITE(IU,'(/A)') 'Scenario situatie'
 IF (J.EQ.2) WRITE(IU,'(/A)') 'Referentie situatie'
 WRITE(IU,'(/A)') 'Parameter'//',Bestandsnaam'
 WRITE(IU,'(A)') ' - GHG'//','//TRIM(IDFS(J,1))
 WRITE(IU,'(A)') ' - GLG'//','//TRIM(IDFS(J,2))
 WRITE(IU,'(A)') ' - NDT'//','//TRIM(IDFS(J,3))
 WRITE(IU,'(A/)') ' - RFCSOIL'//','//TRIM(IDFS(J,4))
 WRITE(IU,'(A/)') TRIM(CLSTRING)

 SUMC = 0
 DO I=1,MAXINDT
  IF(ROSCENCOSTS(J,I,1).GT.0.01) THEN
   DO X=1,MAXINDT
    IF (NDTLUT(X).EQ.I) THEN
      TXT = DAGR(X)
    ENDIF
   ENDDO
   DIFS(J,I)%LUSE = TRIM(ADJUSTL(TXT))
   ASTRING=TRIM(ADJUSTL(TXT))

   DO NR=1,4
    AVAL = ROSCENCOSTS(J,I,NR)
    IF (NR.EQ.1) THEN
     DIFS(J,I)%TA = AVAL	!totaal areaal
     SUMC(NR) = SUMC(NR)+AVAL
    ENDIF
    IF (NR.EQ.2) THEN
     DIFS(J,I)%PY = AVAL	!areaal waarvoor doelrealisatie berekend kan worden
     SUMC(NR) = SUMC(NR)+AVAL
    ENDIF
    IF (NR.EQ.3) THEN
     DIFS(J,I)%CA = AVAL	!gemiddelde doelrealisatie
     SUMC(NR) = (SUMC(NR)+AVAL)/2
    ENDIF
    IF (NR.EQ.4) THEN
     DIFS(J,I)%DD = AVAL	!aantal cellen
     SUMC(NR) = SUMC(NR)+AVAL
    ENDIF

    CALL IREALTOSTRING(AVAL,RECVAL,'(F15.2)')
    ASTRING = TRIM(ASTRING)//','//TRIM(RECVAL)
   ENDDO
   WRITE(IU,'(A)') TRIM(ASTRING)
  ENDIF
 ENDDO
 WRITE(IU,*) ''
 ASTRING = 'TOTAAL'
 DO T=1,4
  IF (T.NE.3) THEN
   CALL IREALTOSTRING(SUMC(T),RECVAL,'(F15.2)')
  ELSE
   RECVAL = '-'
  ENDIF
  ASTRING = TRIM(ASTRING)//','//TRIM(RECVAL)
 ENDDO
 WRITE(IU,'(A/)') TRIM(ASTRING)
ENDDO

!## RESET TO 0
SUMC = 0
IF (CODE.EQ.2) THEN
 !## nature
 NT = 3
 DEALLOCATE(COLUMNTITLES)
 ALLOCATE(COLUMNTITLES(NT))
 COLUMNTITLES = ''
 COLUMNTITLES(1) = 'Natuur Doel Type'
 COLUMNTITLES(2) = 'Totaal oppervlak (ha)'
 COLUMNTITLES(3) = 'Toe-/afname gemiddelde doelrealisatie (%)'

 !## Create columnstring
 DO I=1,NT
  IF (I.EQ.1) THEN
   CLSTRING = TRIM(COLUMNTITLES(I))
  ELSE
   CLSTRING = TRIM(CLSTRING)//','//TRIM(COLUMNTITLES(I))
  ENDIF
 ENDDO
 WRITE(IU,'(/A)') 'Verschil'
 WRITE(IU,'(/A)') TRIM(CLSTRING)
 DO X=1,MAXINDT
  IF (DIFS(1,X)%TA.GT.0.01) THEN
   ASTRING = TRIM(DIFS(1,X)%LUSE)
   DO NR=1,2
    !## TOTAL AREA
    IF (NR.EQ.1) THEN
     AVAL = DIFS(1,X)%TA		!dif Totaal areaal
     SUMC(NR) = SUMC(NR)+AVAL
    ENDIF
    IF (NR.EQ.2) THEN			!dif gemiddelde doelrealisatie
     AVAL = DIFS(2,X)%CA-DIFS(1,X)%CA
     SUMC(NR) = SUMC(NR)+AVAL
    ENDIF

    CALL IREALTOSTRING(AVAL,RECVAL,'(F15.2)')
    ASTRING = TRIM(ASTRING)//','//TRIM(RECVAL)
   ENDDO
   WRITE(IU,'(A)') TRIM(ASTRING)
  ENDIF
 ENDDO

 !## WRITE TOTALS
 WRITE(IU,*) ''
 ASTRING = 'Totaal'
 DO T=1,2
  CALL IREALTOSTRING(SUMC(T),RECVAL,'(F15.2)')
  ASTRING = TRIM(ASTRING)//','//TRIM(RECVAL)
 ENDDO
 WRITE(IU,'(/A/)') TRIM(ASTRING)

 !## write down total decline/increase (not crop specified)
 WRITE(IU,'(/A25,F10.2)') 'Areaal afname (ha) :,    ',areas(1)
 WRITE(IU,'(A25,F10.2)') 'Areaal toename (ha) :,   ',areas(2)
ENDIF

CLOSE(IU)

IF (ALLOCATED(COLUMNTITLES)) DEALLOCATE(COLUMNTITLES)
IF (ALLOCATED(DIFS)) DEALLOCATE(DIFS)

CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Table written to '//TRIM(FNAME),'iMOD Information')

END SUBROUTINE ROSCENREPORTNDT

!###======================================================================
SUBROUTINE ROSCENREPORTURB(FNAME,CODE,AREAS)
!###======================================================================
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN)	:: FNAME
INTEGER,INTENT(IN) :: CODE
REAL,INTENT(IN),DIMENSION(2) :: AREAS
INTEGER :: MAXURB,I,NROWS,NT,IU,J
REAL :: AVAL
REAL, ALLOCATABLE :: DIFS(:,:)
CHARACTER(LEN=500) :: ASTRING,RECVAL

IU = UTL_GETUNIT()
OPEN(IU,FILE=FNAME,STATUS='REPLACE')
WRITE(IU,'(A)') 'Overzicht berekening doelrealisatie stedelijk gebied'

IF (ALLOCATED(DIFS)) DEALLOCATE(DIFS)
MAXURB = 3
ALLOCATE(DIFS(2,MAXURB))

DO I =1,CODE
 WRITE(IU,*) ''
 SELECT CASE (I)
  CASE (1)
   WRITE(IU,'(A)') 'Scenario situatie'
  CASE (2)
   WRITE(IU,'(A)') 'Referentie situatie'
 END SELECT
 ASTRING = 'Klasse,Totaal areaal (ha)'
 WRITE(IU,'(A)') TRIM(ASTRING)
 DO J=1,MAXURB
  IF (J.EQ.1) ASTRING = 'Grote kans op schade'
  IF (J.EQ.2) ASTRING = 'Matige kans op schade'
  IF (J.EQ.3) ASTRING = 'Kleine kans op schade'
  AVAL = ROSCENCOSTS(I,J,1)
  DIFS(I,J) = AVAL
  CALL IREALTOSTRING(AVAL,RECVAL,'(F15.2)')

  ASTRING = TRIM(ASTRING)//','//TRIM(RECVAL)
  WRITE(IU,'(A)') TRIM(ASTRING)
 ENDDO
 WRITE(IU,*) ''
ENDDO

IF (CODE.EQ.2) THEN
 WRITE(IU,'(A)') 'Verschillen tussen referentie en initiele situatie'
 WRITE(IU,'(A)') 'Klasse, verschil in areaal (ha)'
 DO J=1,MAXURB
  IF (J.EQ.1) ASTRING = 'Grote kans op schade'
  IF (J.EQ.2) ASTRING = 'Matige kans op schade'
  IF (J.EQ.3) ASTRING = 'Weinig kans op schade'
  AVAL = DIFS(2,J) - DIFS(1,J)
  CALL IREALTOSTRING(AVAL,RECVAL,'(F15.2)')
  ASTRING = TRIM(ASTRING)//','//TRIM(RECVAL)
  WRITE(IU,'(A)') TRIM(ASTRING)
 ENDDO

 !write down total decline/increase (not crop specified)
 WRITE(IU,*) ''
 WRITE(IU,'(A25,F15.2)') 'Areaal afname (ha) :,    ',areas(1)
 WRITE(IU,'(A25,F15.2)') 'Areaal toename (ha) :,   ',areas(2)
ENDIF
CLOSE(IU)

IF (ALLOCATED(DIFS)) DEALLOCATE(DIFS)
CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Table written to '//TRIM(FNAME),'iMOD Information')

END SUBROUTINE ROSCENREPORTURB

!###======================================================================
SUBROUTINE ROSCEN2FIELDS()
!###======================================================================
IMPLICIT NONE
INTEGER	:: RSTATE
CHARACTER(LEN=50) :: MSG

CALL WDIALOGSELECT(ID_DROSCENT1)
CALL WDIALOGGETCHECKBOX(IDF_CHECK3,RSTATE)
IF (RSTATE.EQ.0) THEN
 MSG = 'Sprinkling off'
! SFACT = 1.
ELSEIF (RSTATE.EQ.1) THEN
 MSG = 'Sprinkling on'
! SFACT = .2			!80% VERMINDERING VAN DROOGSCHADE
ENDIF
CALL WDIALOGPUTSTRING(IDF_CHECK3,MSG)

END SUBROUTINE ROSCEN2FIELDS

!###======================================================================
REAL FUNCTION HLPQUERY(JU,RGHG,RGLG,RS,RC,IEFFECT)
!###======================================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: JU,IEFFECT
REAL,INTENT(INOUT):: RGHG,RGLG
REAL,INTENT(IN)	:: RC,RS
REAL :: D
INTEGER :: C, S, GLG, GHG, IGLG, IGHG, DBT,DGHG,DGLG, ISOIL
INTEGER :: JGHG,JGLG,AREC,BREC,DMN,I,J
INTEGER(KIND=1) :: IX					   !values are in Signed integers
INTEGER(KIND=1),DIMENSION(4) :: JX
INTEGER,PARAMETER :: MGHG=0,MGLG=20,MBT=1          !minimal values
INTEGER,PARAMETER :: NGHG=200,NGLG=320,NBT=72      !maximum values

C = INT(RC)
SELECT CASE (C)
 CASE (1:14)
  CALL CHECKHELPVALUES(RGHG,RGLG,IEFFECT,DMN)

  GHG = NINT(RGHG)
  GLG = NINT(RGLG)
  S   = INT(RS)

  !## Try to find a valid value whithin 3 steps. Because. the helptables are rasters
  !## and the checkhelpvalue routine checks using a line (points of the original helpfile) it
  !## is possible that the returned value gives a -1, even if there should be a value. In that case it
  !## is tried three times and then it quits with a -1 (= nodata)
  DO I=1,3

   !## Version 2 differs from initial version in that it has no applicable domain control
   !## three domains are recognize (actually 5), see documentation "hoofdrappport help 2005.doc"
   !## and PPT BC HELP waternood 2005 v2 met domeinextrapolatie 3 april 2004.ppt van Jan van Bakel
   !## with several suggestions for improvement to determine damage outside the domain.
   !## domain f --> (GHG >=90 and GLG < 200) and (GLG < 320) USE IGHG

   !## retrieving array at specified position GLG, GHG, CROP, SOIL
   DBT =NBT-MBT+1
   DGHG=NGHG-MGHG+1
   DGLG=NGLG-MGLG+1

   ISOIL=S-MBT+1
   IGHG =GHG-MGHG+1
   IGLG =GLG-MGLG+1

   AREC=(C-1)*(DBT*DGHG*DGLG)+  &
        (ISOIL-1)*(DGHG*DGLG)+  &
        (IGHG-1)*(DGLG)+        &
         IGLG

   !## devide to be record length of 4 (IVF consequences)
   IF(ICF.EQ.1)THEN
    !## include one record to correct for record one
    AREC=AREC+1
    BREC=AREC/4; IF(MOD(AREC,4).NE.0)BREC=BREC+1
    !## read out percentage damage dry
    READ(JU,REC=BREC) (JX(J),J=1,4)
    J=AREC-((BREC-1)*4)
    IX=JX(J)
   ELSE
    READ(JU,REC=AREC) IX
   ENDIF

   IF (IX.GT.0) EXIT
   
   IF (IX.EQ.-1) THEN
    SELECT CASE (DMN)
     CASE(1)
      GLG = GLG - 1
     CASE(2)
      GLG = GLG + 1
    END SELECT
   ENDIF
  ENDDO

  D=REAL(IX)
CASE DEFAULT
  D = -9999.0
END SELECT

HLPQUERY = D

END FUNCTION HLPQUERY

!###======================================================================
SUBROUTINE NDTQUERY(RGHG ,RGLG, RS, RC, & !, RKWL, &
                    GEMDOEL, GLGDOEL, DSTDOEL, GVGDOEL, KWLDOEL, &
                    LIMVMIN,LIMMIN,LIMVMAX,LIMMAX,LIMVAL,CALC)
!###======================================================================
! - GVG 		M-MW
! - GLG           M-MV
! - RFC waarde (uit gemodificeerde bodemkaart) = rs
! - natuurdoeltype = rc
! - kennistabel (bestand) (zijn in ROCALCLB al gevuld)
! Uitvoer voor iedere cel:
! - GVGDoelrealisatie
!Droogtestress wordt bepaald dmv zgn. Gompertz-curve, zie o.a. AlterraRapport367 (Runhaar en Jansen)
!###======================================================================
IMPLICIT NONE
!1 arrays met switches voor bepalende factor (wel of niet kwelafhankelijk, droogtestress en ..)
!2 natuurdoeltype en bodem reprofunctienr.
!3 constanten
!4 arrays met abiotische randvoorwaarden per natuurdoeltype
!5 variabelen
!6 arrays met variabelen per reprofunctie
REAL,INTENT(IN) :: RGHG,RGLG,RC,RS !,RKWL                                        !2
INTEGER,INTENT(IN) :: CALC
REAL,INTENT(OUT) :: GEMDOEL, GLGDOEL, DSTDOEL, GVGDOEL, KWLDOEL
REAL,INTENT(OUT) :: LIMVMIN,LIMMIN, LIMMAX, LIMVMAX, LIMVAL(4)
REAL :: GVGMOD, GLGMOD, KWLFLX, GLGDSTMOD
REAL :: CELVAL, DSTMOD, NOEMER,NODATA
INTEGER :: RFC, NDT, II

! CONVERT PASSED VAR'S TO LOCAL VAR'S
NODATA  = -9999.
GEMDOEL = NODATA
GLGMOD  = RGLG*100  !conversion to cm-mv ghn30082007
!note: all values are in m, so 0.05 is in m, as mentioned in various reports (of which some use
!      0.054 in stead of 0.5 see HELP2005, WATERNOOD values in m-mv
GVGMOD  = .05+0.8*RGHG+0.2*RGLG
GVGMOD  = GVGMOD*100 !convert all values to cm GHN30082007
RFC     = NINT(RS)	!INT vervangen door NINT 11122007
NDT     = NINT(RC)	!INT vervangen door NINT 11122007
LIMVMIN = REAL(RDOELMIN)
LIMVMAX = REAL(RDOELMIN)
!KWLFLX   = RKWL      !for now (12-05-2007 in unkwown unit)

IF (ALLOCATED(CELRVW)) DEALLOCATE(CELRVW)
ALLOCATE(CELRVW(MXC))

GLGDSTMOD = 0
IF (NDT > 0) THEN ! natuurdoeltype bekend
  !## Evaluatie GVG, GLG/droogte-stress en kwel
  IF(INDR(NDT) >= 1) THEN ! natte natuur
      !## GVG evaluatie
    IF(GVGMOD.NE.NODATA) then ! gvg bekend
      GVGDOEL=0.
	CELVAL=GVGMOD
	CELRVW(1:MXC)=GVGRVW(NDT,1:MXC)
	IF(CELRVW(1).NE.NODATA.AND.CELRVW(4).NE.NODATA) THEN		! tweezijdig begrensd
	  IF(CELVAL<=CELRVW(1)) THEN
	    GVGDOEL=-2. 						! gvg te nat, score 0% (-2)
	  ELSEIF(CELVAL>=CELRVW(4)) THEN
	    GVGDOEL=-3. 						! gvg te droog, score 0% (-3)
	  ELSEIF(CELVAL>=CELRVW(2).AND.CELVAL<=CELRVW(3)) THEN
	    GVGDOEL=100. 						! score 100%
	  ELSEIF(CELVAL>CELRVW(1).AND.CELVAL<CELRVW(2)) THEN
	    GVGDOEL=100.*((CELVAL-CELRVW(1))/(CELRVW(2)-CELRVW(1))) 	! score tussen 0 en 100%
	  ELSEIF(CELVAL>CELRVW(3).AND.CELVAL<CELRVW(4)) THEN
	    GVGDOEL=100.*((CELVAL-CELRVW(4))/(CELRVW(3)-CELRVW(4))) 	! score tussen 100 en 0%
	  END IF
	ELSEIF(CELRVW(1).NE.NODATA) THEN				! links begrensd
	  IF(CELVAL<=CELRVW(1)) THEN
	    GVGDOEL=-2. 						! gvg te nat, score 0% (-2)
	  ELSEIF(CELVAL>=CELRVW(2)) THEN
	    GVGDOEL=100. 						! score 100%
	  ELSEIF(CELVAL>CELRVW(1).AND.CELVAL<CELRVW(2)) THEN
	    GVGDOEL=100.*((CELVAL-CELRVW(1))/(CELRVW(2)-CELRVW(1))) 	! score tussen 0 en 100%
	  END IF
	ELSEIF(CELRVW(4).NE.NODATA) THEN				! rechts begrensd
	  IF(CELVAL>=CELRVW(4)) THEN
	    GVGDOEL=-3. 						! gvg te droog, score 0% (-3)
	  ELSEIF(CELVAL<=CELRVW(3)) THEN
	    GVGDOEL=100. 						! score 100%
	  ELSEIF(CELVAL>CELRVW(3).AND.CELVAL<CELRVW(4)) THEN
	    GVGDOEL=100.*((CELVAL-CELRVW(4))/(CELRVW(3)-CELRVW(4))) 	! score tussen 100 en 0%
	  END IF
	ELSE 								! grondwater onafhankelijk (onbegrensd)
	  GVGDOEL=101. 							! score n.v.t. (101)
	END IF
    ENDIF !GVG Evaluatie

    !## GLG EVALUATIE
    IF(GLGMOD.NE.NODATA) THEN ! glg bekend
      ! sectie evalueer op glg
	IF(IGLD(NDT)==1.) THEN
	  GLGDOEL=0.
	  CELVAL=GLGMOD
	  CELRVW(1:MXC)=GLGRVW(NDT,1:MXC)
	  IF(CELRVW(1)>NODATA.AND.CELRVW(4)>NODATA) THEN 		! tweezijdig begrensd
	    IF(CELVAL<=CELRVW(1)) THEN
	     GLGDOEL=-2. 						! glg te nat, score 0% (-2)
	    ELSEIF(CELVAL>=CELRVW(4)) THEN
	     GLGDOEL=-3. 						! glg te droog, score 0% (-3)
	    ELSEIF(CELVAL>=CELRVW(2).AND.CELVAL<=CELRVW(3)) THEN
	     GLGDOEL=100. 						! score 100%
	    ELSEIF(CELVAL>CELRVW(1).AND.CELVAL<CELRVW(2)) THEN
	     GLGDOEL=100.*((CELVAL-CELRVW(1))/(CELRVW(2)-CELRVW(1))) 	! score tussen 0 en 100%
	    ELSEIF(CELVAL>CELRVW(3).AND.CELVAL<CELRVW(4)) THEN
	     GLGDOEL=100.*((CELVAL-CELRVW(4))/(CELRVW(3)-CELRVW(4))) 	! score tussen 100 en 0%
	    END IF
	  ELSEIF(CELRVW(1)>NODATA) THEN 				! links begrensd
	    IF(CELVAL<=CELRVW(1)) THEN
	     GLGDOEL=-2. 						! glg te nat, score 0% (-2)
	    ELSEIF(CELVAL>=CELRVW(2)) THEN
	     GLGDOEL=100. 						! score 100%
	    ELSEIF(CELVAL>CELRVW(1).AND.CELVAL<CELRVW(2)) THEN
	     GLGDOEL=100.*((CELVAL-CELRVW(1))/(CELRVW(2)-CELRVW(1))) 	! score tussen 0 en 100%
	    END IF
	  ELSEIF(CELRVW(4)>NODATA) THEN 				! rechts begrensd
	    IF(CELVAL>=CELRVW(4)) THEN
	     GLGDOEL=-3. 						! glg te droog, score 0% (-3)
	    ELSEIF(CELVAL<=CELRVW(3)) THEN
	     GLGDOEL=100. 						! score 100%
	    ELSEIF(CELVAL>CELRVW(3).AND.CELVAL<CELRVW(4)) THEN
	     GLGDOEL=100.*((CELVAL-CELRVW(4))/(CELRVW(3)-CELRVW(4))) 	! score tussen 100 en 0%
	    END IF
	  ELSE 								! grondwater onafhankelijk (onbegrensd)
	    GLGDOEL=101. 						! score n.v.t. (101)
	  END IF
	  IF(CELVAL<0) THEN 								! inundatie
	    IF(CELRVW(2)>NODATA.AND.CELRVW(2)<=0) GLGDOEL=102. 		! inundatie gewenst (102)
	    IF(CELRVW(2)>0) GLGDOEL=103. 						! inundatie niet gewenst (103)
	  END IF
        GLGDSTMOD = GLGDOEL

      !## sectie evaulueren op droogtestress
	ELSEIF(IGLD(NDT)==2..AND.(RFC>0.AND.RFC<=99)) THEN		! evalueer op droogte-stress mbv zgn. Gompertz-curve

	 IF(RFC==99.OR.(RFC>=10.AND.RFC<=12)) THEN
	   DSTDOEL=100. 						! deze bodems nooit vochttekort, score 100%
	   DSTMOD=0.
	 ELSE
	  DSTMOD=C(RFC)*EXP(-EXP(-B(RFC)*(GLGMOD-M(RFC))))          ! langjarig gemiddeld aantal dagen met droogtestress
	  DSTDOEL=0.
	  CELVAL=DSTMOD
	  CELRVW(1:MXC)=DSTRVW(NDT,1:MXC)
	  IF(CELRVW(1)>NODATA.AND.CELRVW(4)>NODATA) THEN 		! tweezijdig begrensd
	    IF(CELVAL<=CELRVW(1)) THEN
	      DSTDOEL=-2. 						! dst te nat, score 0% (-2)
	    ELSEIF(CELVAL>=CELRVW(4)) THEN
	      DSTDOEL=-3. 						! dst te droog, score 0% (-3)
	    ELSEIF(CELVAL>=CELRVW(2).AND.CELVAL<=CELRVW(3)) THEN
	      DSTDOEL=100. 						! score 100%
	    ELSEIF(CELVAL>CELRVW(1).AND.CELVAL<CELRVW(2)) THEN
	      DSTDOEL=100.*((CELVAL-CELRVW(1))/(CELRVW(2)-CELRVW(1))) 	! score tussen 0 en 100%
	    ELSEIF(CELVAL>CELRVW(3).AND.CELVAL<CELRVW(4)) THEN
	      DSTDOEL=100.*((CELVAL-CELRVW(4))/(CELRVW(3)-CELRVW(4))) 	! score tussen 100 en 0%
	    END IF

	  ELSEIF(CELRVW(1)>NODATA) THEN 				! links begrensd
	    IF(CELVAL<=CELRVW(1)) THEN
	      DSTDOEL=-2. 						! dst te nat, score 0% (-2)
	    ELSEIF(CELVAL>=CELRVW(2)) THEN
	      DSTDOEL=100. 						! score 100%
	    ELSEIF(CELVAL>CELRVW(1).AND.CELVAL<CELRVW(2)) THEN
	      DSTDOEL=100.*((CELVAL-CELRVW(1))/(CELRVW(2)-CELRVW(1))) 	! score tussen 0 en 100%
	    END IF
	  ELSEIF(CELRVW(4)>NODATA) THEN 				! rechts begrensd
	    IF(CELVAL>=CELRVW(4)) THEN
	      DSTDOEL=-3. 						! dst te droog, score 0% (-3)
	    ELSEIF(CELVAL<=CELRVW(3)) THEN
	      DSTDOEL=100. 						! score 100%
	    ELSEIF(CELVAL>CELRVW(3).AND.CELVAL<CELRVW(4)) THEN
	      DSTDOEL=100.*((CELVAL-CELRVW(4))/(CELRVW(3)-CELRVW(4))) 	! score tussen 100 en 0%
	    END IF
	  ELSE	 							! grondwater onafhankelijk (onbegrensd)
	    DSTDOEL=101.	 					! score n.v.t. (101)
	  END IF
         GLGDSTMOD = DSTDOEL
        END IF
	END IF ! evalueer op glg of droogte-stress
    END IF ! glg bekend

    ! Sectie Evaluatie kwel, vanaf 16-03-2007 door GHN en MKR als ratio van
    ! berekende kwelflux en gewenste kwelflux beschouwd
    ! verbeteringen zouden kunnen zijn als tijdsduur van de kwel in de wortelzone beschikbaar is
    KWLDOEL = 0
!    IF(IKWL(NDT)==1) THEN 						! kwelafhankelijke natuur (is ook altijd natte natuur)
!      IF (KWLFLX.NE.NODATA) THEN
!         KWLDOEL = KWLFLX/FLXWNS
!         IF (KWLDOEL>1) THEN
!            KWLDOEL = 100
!         ELSEIF (KWLDOEL==0) THEN
!            KWLDOEL = 0
!         ELSE
!            KWLDOEL = KWLDOEL*100
!         END IF
!	END IF ! ghg2 en gewenste ghg2 bekend
!    END IF ! kwelafhankelijke natuur

    !doelrealisaties > 100% wordt op 100% gezet; < 0% op 0%
    !indien nodig kan hierboven in het script met deze waarden iets gedaan worden
    !101=grondwater onafhankelijk; 102=inundatie gewenst; 103=inundatie niet gewenst
    !-2=te nat; -3=te droog
    IF(GVGDOEL>100) GVGDOEL=100.
    IF(GVGDOEL<0.AND.GVGDOEL>NODATA) GVGDOEL=0.
    IF(GLGDOEL>100) GLGDOEL=100.
    IF(GLGDOEL<0.AND.GLGDOEL>NODATA) GLGDOEL=0.
    IF(DSTDOEL>100) DSTDOEL=100.
    IF(DSTDOEL<0.AND.DSTDOEL>NODATA) DSTDOEL=0.
    IF(GLGDSTMOD.GT.100) GLGDSTMOD=100.
    IF(GLGDSTMOD<0.AND.GLGDSTMOD>NODATA) GLGDSTMOD=0.
    IF(KWLDOEL>100) KWLDOEL=100.
    IF(KWLDOEL<0.AND.KWLDOEL>NODATA) KWLDOEL=0.

    SELECT CASE (CALC)
     CASE(0)	!Average doelrealisatie is caluclated
      !bereken gemiddelde doelrealisatie en limiterende doelrealisatie(s)
      GEMDOEL=0.
      NOEMER=0.
      DO II=1,4
       LIMVAL(II)=1000.
      END DO
      IF(GVGDOEL>NODATA) THEN
       LIMVAL(1)=GVGDOEL
       GEMDOEL=GEMDOEL+GVGDOEL
       NOEMER=NOEMER+1.
       IF (GVGDOEL<LIMVMIN) THEN       !GHN
          LIMVMIN = GVGDOEL
          LIMMIN = 1
       END IF
       IF (GVGDOEL>LIMVMAX) THEN
          LIMVMAX = GVGDOEL
          LIMMAX = 1
       END IF
      END IF
      IF(GLGDOEL>NODATA) THEN
       LIMVAL(2)=GLGDOEL
       GEMDOEL=GEMDOEL+GLGDOEL
       NOEMER=NOEMER+1.
       IF (GLGDOEL<LIMVMIN) THEN       !GHN
          LIMVMIN = GLGDOEL
          LIMMIN = 2
       END IF
       IF (GLGDOEL>LIMVMAX) THEN
          LIMVMAX = GLGDOEL
          LIMMAX = 2
       END IF
       GLGDSTMOD = GLGDOEL
      END IF

      IF(DSTDOEL>NODATA) THEN
       LIMVAL(3)=DSTDOEL
       GEMDOEL=GEMDOEL+DSTDOEL
       NOEMER=NOEMER+1.
       IF (DSTDOEL<LIMVMIN) THEN       !GHN
          LIMVMIN = DSTDOEL
          LIMMIN = 3
       END IF
       IF (DSTDOEL>LIMVMAX) THEN
          LIMVMAX = DSTDOEL
          LIMMAX = 3
       END IF
       GLGDSTMOD = DSTDOEL
      END IF
!     IF(KWLDOEL>NODATA) THEN
!       LIMVAL(4)=KWLDOEL
!       GEMDOEL=GEMDOEL+KWLDOEL
!       NOEMER=NOEMER+1.
!       IF (KWLDOEL<LIMVMIN) THEN       !GHN
!          LIMVMIN = KWLDOEL
!          LIMMIN = 4
!       END IF
!       IF (KWLDOEL>LIMVMAX) THEN
!          LIMVMAX = KWLDOEL
!          LIMMAX = 4
!       END IF
!     END IF
      IF(NOEMER>0) THEN
       GEMDOEL=GEMDOEL/NOEMER
      ELSE
       GEMDOEL=NODATA
      END IF
    CASE (1)
     IF((GLGDSTMOD>0.).AND.(GVGDOEL>0.)) THEN
      GEMDOEL = ((GVGDOEL/100) * (GLGDSTMOD/100))*100
     ELSEIF ((GVGDOEL.EQ.0.).OR.(GLGDSTMOD.EQ.0.)) THEN
      GEMDOEL = 0.
     ELSE
      GEMDOEL = NODATA
     ENDIF
    END SELECT
  END IF ! natte natuur
ELSE
 GEMDOEL = NODATA
END IF ! natuurdoeltype bekend

NOEMER =-9999.
GVGMOD =-9999.
GVGDOEL=-9999.
GLGMOD =-9999.
GLGDOEL=-9999.
DSTDOEL=-9999.
KWLDOEL=-9999.
GLGDSTMOD=-9999.

END SUBROUTINE NDTQUERY

!###======================================================================
SUBROUTINE CHECKHELPVALUES(GHG,GLG,IEFFECT,DMN)
!###======================================================================
IMPLICIT NONE
REAL,PARAMETER :: GHGMX=200.0,GHGA2=105.0,GHGD=90.0
REAL,PARAMETER :: GLGA2=145.0,GLGA3=250.0,GLGMX=320.0,GLGD=250.0,GLGMIN=20.0
REAL,INTENT(INOUT) :: GHG,GLG
INTEGER,INTENT(OUT)	:: DMN
INTEGER,INTENT(IN):: IEFFECT
REAL :: GHGA1,RICO,B,GLGA1,MODGHG,MODGLG
REAL,DIMENSION(4) :: AGHG,AGLG
INTEGER :: INS, N

SELECT CASE (IEFFECT)
 CASE (1,3)     !NATSCHADE STEUNPUNT ZIE TECHNISCHE HELPDOCUMENTATIE
  GHGA1 = 40.
  GLGA1 = 70.
 CASE (2,4)     !DROOGSCHADE STEUNPUNT ZIE TECHNISCHE HELPDOCUMENTATIE
  GHGA1 = 45.
  GLGA1 = 75.
END SELECT

MODGHG = 0.
MODGLG = 0.
DMN = 0
!Check if glg, ghg values are less then 0
IF (GHG.LT.0.) GHG = 0.0
IF (GLG.LT.GLGMIN) GLG = GLGMIN

!DOMAIN A1
IF ((GHG.GE.0.0).AND.(GHG.LE.GHGA1).AND.(GLG.LE.GLGA1)) THEN
 N = 4
 AGHG(1) = 0.
 AGLG(1) = 0.
 AGHG(2) = 0.
 AGLG(2) = GLGMIN
 AGHG(3) = GHGA1
 AGLG(3) = GLGA1
 AGHG(4) = 40.
 AGLG(4) = 0.

 !DETERMINE VALUE OF GLG ON LINE 0,0 - 40/45,75
 IF (IGRINSIDEPOLYGON(AGHG,AGLG,N,GHG,GLG)) THEN
  DMN=1
  RICO = (GLGA1-GLGMIN)/(GHGA1-0.)
  B = GLGA1-GHGA1*RICO
  GLG = GHG*RICO+B
  IF (MOD(GLG,1.).LT.0.5) GLG = GLG+1.
  IF (MOD(GHG,1.).GT.0.5) GHG = GHG-1.
  RETURN
 ENDIF
ENDIF

!DOMAIN A2
IF ((GHG.GT.GHGA1).AND.(GHG.LE.GHGA2).AND.(GLG.LE.GLGA2)) THEN
 N = 4
 !SET AGHG AND AGLG AND CALL PNPOLY
 AGHG(1) = GHGA1
 AGLG(1) = 0.
 AGHG(2) = GHGA1
 AGLG(2) = GLGA1
 AGHG(3) = GHGA2
 AGLG(3) = GLGA2
 AGHG(4) = GHGA2
 AGLG(4) = 0.

 !DETERMINE VALUE OF GLG ON LINE 40/45,75 - 105,145
 IF (IGRINSIDEPOLYGON(AGHG,AGLG,N,GHG,GLG)) THEN
  RICO = (GLGA2-GLGA1)/(GHGA2-GHGA1)
  B = GLGA2-GHGA2*RICO
  GLG = GHG*RICO+B
  DMN=1
  IF (MOD(GHG,1.).GT.0.5) GHG = GHG-1.
  IF (MOD(GLG,1.).LT.0.5) GLG = GLG+1.
  RETURN
 ENDIF
ENDIF

!DOMAIN A3
IF ((GHG.GT.GHGA2).AND.(GHG.LE.GHGMX).AND.(GLG.LE.GLGA3)) THEN
 N = 4
 !SET AGHG AND AGLG AND CALL PNPOLY
 AGHG(1) = GHGA2
 AGLG(1) = 0.
 AGHG(2) = GHGA2
 AGLG(2) = GLGA2
 AGHG(3) = GHGMX
 AGLG(3) = GLGA3
 AGHG(4) = GHGMX
 AGLG(4) = 0.

 IF (IGRINSIDEPOLYGON(AGHG,AGLG,N,GHG,GLG)) THEN
  RICO = (GLGA3-GLGA2)/(GHGMX-GHGA2)
  B = GLGA3-GHGMX*RICO
  GLG = GHG*RICO+B
  DMN=1
  IF (MOD(GHG,1.).GT.0.5) GHG = GHG-1.
  IF (MOD(GLG,1.).LT.0.5) GLG = GLG+1.
  RETURN
 ENDIF
ENDIF

!DOMAIN B
IF ((GHG.GT.GHGMX).AND.(GLG.LE.GLGA3)) THEN
 DMN = 0
 GHG = GHGMX
 GLG = GLGA3
 RETURN
ENDIF

!DOMAIN C
IF ((GHG.GT.GHGMX).AND.(GLG.LE.GLG).AND.(GLG.GT.GLGD)) THEN
 DMN = 0
 GLG = GLGD
 GHG = GHGMX
 RETURN
ENDIF

!DOMAIN E
IF ((GHG.GT.0.0).AND.(GHG.LE.GHGD).AND.(GLG.GT.GLGD).AND.(GLG.LE.GLGMX)) THEN
 DMN=2
 N = 3
 !SET AGHG AND AGLG AND CALL PNPOLY
 AGHG(1) = 0.
 AGLG(1) = GLGD
 AGHG(2) = 0.
 AGLG(2) = GLGMX
 AGHG(3) = GHGD
 AGLG(3) = GLGMX
 IF (IGRINSIDEPOLYGON(AGHG,AGLG,N,GHG,GLG)) THEN
  RICO = (GLGMX-GLGD)/GHGD
  B = GLGMX-GHGD*RICO
  GLG = GHG*RICO+B
  IF (MOD(GHG,1.).LT.0.5) GHG = GHG+1.
  IF (MOD(GLG,1.).GT.0.5) GLG = GLG-1.
  RETURN
 ENDIF
ENDIF

!DOMAIN D
IF ((GHG.GT.0.0).AND.(GHG.LE.GHGD).AND.(GLG.GT.GLGMX)) THEN
 DMN=2
 !GHG VALUE DOES NOT CHANGE
 GLG = GLGD+GHG*((GLGMX-GLGD)/GHGD)
 IF (MOD(GHG,1.).LT.0.5) GHG = GHG+1.
 IF (MOD(GLG,1.).GT.0.5) GLG = GLG-1.
 RETURN
ENDIF

!DOMAIN F
IF ((GHG.GT.GHGD).AND.(GHG.LE.GHGMX).AND.(GLG.GT.GLGMX)) THEN
 DMN = 0
 !GHG VALUE DOES NOT CHANGE
 GLG = GLGMX
 RETURN
ENDIF

!DOMAIN G
IF ((GHG.GT.GHGMX).AND.(GLG.GT.GLGMX)) THEN
 DMN = 0
 GLG = GLGMX
 GHG = GHGMX
 RETURN
!APPARANTLY INSIDE POLY X
ELSE
 DMN = 0
 GLG = GLG
 GHG = GHG
 RETURN
END IF

END SUBROUTINE CHECKHELPVALUES

!###======================================================================
SUBROUTINE ROCALCEXTENT(TMPIDFS,N,MINX,MAXX,MINY,MAXY,CSA,NR,NC)
!determines the extent of the idf's used within RO calculations.
!this depends on the maximum extent of the two calculated base raster
!(GLG and GHG), the effect can be zero, so the maximum dimensions of both
!is calculated and returned.
!###======================================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: N
CHARACTER(LEN=*),DIMENSION(N),INTENT(IN) :: TMPIDFS
REAL,INTENT(OUT) :: MINX,MAXX,MINY,MAXY,CSA
INTEGER,INTENT(OUT)	:: NR, NC
INTEGER :: I
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF1
TYPE(IDFOBJ) :: IDF2

ALLOCATE(IDF1(N)); CALL IDFNULLIFY(IDF2)
DO I=1,N
 CALL IDFNULLIFY(IDF1(I))
 IF(.NOT.IDFREAD(IDF1(I),TMPIDFS(I),0,1))THEN; ENDIF
ENDDO

IF(IDF_EXTENT(N,IDF1,IDF2,2))THEN; ENDIF

DO I=1,N
 CALL IDFDEALLOCATEX(IDF1(I)); CLOSE(IDF1(I)%IU); IDF1(I)%IU=0
ENDDO
MINX=IDF2%XMIN
MINY=IDF2%YMIN
MAXX=IDF2%XMAX
MAXY=IDF2%YMAX
CSA=IDF2%DX
NC=IDF2%NCOL
NR=IDF2%NROW

DEALLOCATE(IDF1)

END SUBROUTINE ROCALCEXTENT

END MODULE MOD_RO_SCEN