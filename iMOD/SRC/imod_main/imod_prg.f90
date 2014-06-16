!!  Copyright (C) Stichting Deltares, 2005-2014.
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

!###======================================================================
PROGRAM IMODPRG
!###======================================================================
USE WINTERACTER
USE RESOURCE
USE MODPLOT
USE IMODVAR
USE MOD_POLYGON_PAR, ONLY : ISHPEDIT,ICRD,CRDITYPE
USE MOD_IR, ONLY : IR1MENU,IR1MAIN,IR1CLOSE
USE MOD_IR_PAR, ONLY : IRWIN
USE MOD_RO_SCEN, ONLY : ROSCENMAIN
USE PMANAGER_MOD, ONLY : PMANAGERINIT,PMANAGERMAIN,PMANAGERRUN,PMANAGERSHOW
USE MOD_MANAGER, ONLY : MANAGERMAIN,MANAGERINIT
USE MOD_PREF, ONLY : PREFINIT,PREFCOLOURSINIT,PREFVAL,PREFREAD,PREFUPDATE
USE MOD_UTL, ONLY : INFOUNITS,UTL_CAP,UTL_CREATEDIR
USE MOD_START, ONLY : START_MAIN
USE MOD_IDFEDIT, ONLY : IDFEDITCALCMAIN,IDFEDITMAIN
USE MOD_EXTRACTIPF, ONLY : EXTRACTIPF1MAIN
USE MOD_TOOLS, ONLY : TOOLS1MAIN
USE MOD_SCEN, ONLY : SCEN1MAIN
USE MOD_CREATEIDF, ONLY : CREATEIDF1MAIN
USE MOD_PLINES, ONLY : PLINES1MAIN
USE MOD_SPOINTS,ONLY : STARTP1MAIN
USE MOD_LEGEND, ONLY : LEGINIT
USE MOD_MODEL, ONLY : MODEL1MAIN
USE MOD_SCENTOOL, ONLY : ST1MAIN,ST1PROPMAIN
USE MOD_IPF_PAR, ONLY : NLITHO,BH
USE MOD_IPFANALYSE, ONLY : IPFANALYSE_INIT_GRAPHVARIABLES
USE MOD_TSTAT, ONLY : TSTAT1MAIN
USE MOD_SOLID, ONLY : SOLIDMAIN
USE MOD_OSD, ONLY : OSD_GETARG,OSD_GETNARG
USE MOD_ABOUT, ONLY : IMODSTARTSCREEN,IMODAGREEMENT
USE MOD_BATCH, ONLY : IMODBATCH
USE MOD_QUICKOPEN, ONLY : IDFQUICKOPEN_MAIN
USE MOD_ISG, ONLY : ISGEDITMAIN
USE MOD_ISG_ADJ, ONLY : ISGADJUSTMAIN
USE MOD_CREATEGEN, ONLY : CREATEGEN1MAIN
USE MOD_CREATEIPF, ONLY : CREATEIPF1MAIN
USE MOD_INTERSECT, ONLY : INTERSECT_NULLIFY
USE MOD_KRIGING, ONLY : KRIGING_UNITTEST
USE MOD_BATCH, ONLY : CREATEIMODBATCHMAIN
USE IMOD
USE MOD_SOBEK, ONLY : ISOBEK
USE MOD_GEN2GEN_PUZZLE, ONLY : GENFNAME,PUZZLEMAIN
USE IMODCONFIG
USE MOD_UTM, ONLY : UTM_MAIN
USE MOD_GOOGLE, ONLY : GOOGLE_MAIN

IMPLICIT NONE
TYPE(WIN_MESSAGE)  :: MESSAGE
INTEGER :: ITYPE,IERROR,I,NARG
CHARACTER(LEN=256) :: ARGSTRING,LEGNAME

CALL WINITIALISE(' ') 

IF(IMODBATCH())STOP

!## get username and status and initialise window
CALL WINDOWOPEN(FLAGS=SYSMENUON+MINBUTTON+MAXBUTTON+STATUSBAR+MAXWINDOW, &
                TITLE='iMOD [Version '//TRIM(RVERSION)//'; Configuration '//TRIM(CCONFIG)//']')
CALL WINDOWSTATUSBARPARTS(4,(/2000,2000,750,-1/),(/1,1,1,1/))

CALL IMODSTARTSCREEN()

!## initialize preferences
CALL PREFINIT(IERROR)

IERROR=0; CALL IMODAGREEMENT(IERROR)
IF(IERROR.NE.1)THEN
 CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'Can not start iMOD unless you agree with the user agreement','Error')
 CALL WINDOWCLOSE(); STOP
ENDIF

!## nullify pointer
CALL INTERSECT_NULLIFY()
!## allocate memory fo graph-variables ipf plotting
CALL IPFANALYSE_INIT_GRAPHVARIABLES()
!##24-bits colour application
CALL IGRCOLOURMODEL(24)
!##load datamanager in memory
CALL MANAGERINIT()
!##load project-datamanager in memory
CALL PMANAGERINIT()
!##load legend in memory
CALL LEGINIT()
!##initialize iMOD
CALL IMODINIT()
!##initialize colours
CALL PREFCOLOURSINIT(.TRUE.)
!## no colour read
NLITHO=0
!## initiate white colors
BH%LITHOCLR=WRGB(255,255,255)
!## remove backslash in labeling on default
IBACKSLASH=1; ILABELNAME=0
!## set ability to import sobek model
CALL WMENUSETSTATE(ID_IMPORTSOBEK,1,ISOBEK)
!## initiate idproc for help-manual
IDPROC=0

LEGNAME=''; CALL OSD_GETNARG(NARG)
IF(NARG.GT.0)THEN
 DO I=1,NARG
  CALL OSD_GETARG(I,ARGSTRING)
  IF(INDEX(UTL_CAP(ARGSTRING,'U'),'.IMF').GT.0)THEN
   IMFFNAME=ARGSTRING
   CALL IMODLOADIMF()
  ELSEIF(INDEX(UTL_CAP(ARGSTRING,'U'),'.IDF').GT.0.OR. &
         INDEX(UTL_CAP(ARGSTRING,'U'),'.IPF').GT.0.OR. &
         INDEX(UTL_CAP(ARGSTRING,'U'),'.IFF').GT.0.OR. &
         INDEX(UTL_CAP(ARGSTRING,'U'),'.ISG').GT.0.OR. &
         INDEX(UTL_CAP(ARGSTRING,'U'),'.GEN').GT.0.OR. &
         INDEX(UTL_CAP(ARGSTRING,'U'),'.ASC').GT.0.OR. &
         INDEX(UTL_CAP(ARGSTRING,'U'),'.MDF').GT.0)THEN
   CALL IDFINIT(ARGSTRING,LEGNAME=LEGNAME,LPLOT=.TRUE.); LEGNAME=''
  ELSEIF(INDEX(UTL_CAP(ARGSTRING,'U'),'.LEG').GT.0)THEN
   LEGNAME=ARGSTRING
  ELSEIF(INDEX(UTL_CAP(ARGSTRING,'U'),'.PRF').GT.0)THEN
   CALL PREFREAD(ARGSTRING)
   CALL PREFUPDATE()
  ELSEIF(INDEX(UTL_CAP(ARGSTRING,'U'),'.RUN').GT.0)THEN
   IF(PMANAGERRUN(ID_OPENRUN,ARGSTRING))THEN
    CALL PMANAGERSHOW(1); CALL IDFPLOT(1)
    CALL UTL_CREATEDIR(TRIM(PREFVAL(1))//'\RUNFILES')
    CALL IOSCOPYFILE(ARGSTRING,TRIM(PREFVAL(1))//'\RUNFILES\'//TRIM(ARGSTRING(INDEX(ARGSTRING,'\',.TRUE.):)))
   ENDIF
  ELSE
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not recognize given argument'//CHAR(13)//TRIM(ARGSTRING),'Error')
   STOP
  ENDIF
 END DO
ENDIF

IF(NARG.EQ.0)THEN
 !## get username and status and initialise window
 CALL START_MAIN('IMF',IERROR); IF(IERROR.EQ.0)THEN; CALL WINDOWCLOSE(); STOP; ENDIF
 CALL IDFPLOT(1)
ENDIF

CALL WCURSORSHAPE(CURARROW)
!##for plotting purposes of shapes in idfedit
ICRD    =0
CRDITYPE=0
IDOWN   =0
DOWNX   =0.0
DOWNY   =0.0
ISHPEDIT=1   ! ## shapes are editable

DO

 CALL WMESSAGE(ITYPE,MESSAGE)

 SELECT CASE(MESSAGE%WIN)

  !## messages from child-windows/ir window
  CASE(0:20)
   IF(MESSAGE%WIN.NE.0.AND.MESSAGE%WIN.EQ.IRWIN)THEN
    CALL IR1MENU(ITYPE,MESSAGE)
   ELSE
    CALL IMOD1MENU(ITYPE,MESSAGE)
   ENDIF

  !## Message from manager Menu
  CASE(ID_DMANAGER,ID_DMANAGERTAB1,ID_DMANAGERTAB2,ID_DMANAGERTAB3,ID_DMANAGERTAB4)
   CALL MANAGERMAIN(ITYPE,MESSAGE)

  !## Message from project manager Menu
  CASE(ID_DPMANAGER)
   CALL PMANAGERMAIN(ITYPE,MESSAGE)

  !## Message from ir-manager MainMenu
  CASE(ID_DIR_PM,ID_DIR_PMTAB1,ID_DIR_PMTAB1TAB1,ID_DIR_PMTAB1TAB2, &
                 ID_DIR_PMTAB2,ID_DIR_PMTAB2TAB1,ID_DIR_PMTAB2TAB2,ID_DIR_PMTAB2TAB3,ID_DIR_PMTAB2TAB4, &
                 ID_DIR_PMTAB3,ID_DIR_PMTAB3TAB1,ID_DIR_PMTAB3TAB2)
   CALL IR1MAIN(ITYPE,MESSAGE)

  !## Message from model-manager Menu
  CASE(ID_DMODEL,ID_DMDLTAB1,ID_DMDLTAB2,ID_DMDLTAB3,ID_DMDLTAB4) 
   CALL MODEL1MAIN(ITYPE,MESSAGE)

  !## Message from scenario-manager Menu
  CASE(ID_DSCENARIO,ID_DSCEN1,ID_DSCEN2)
   CALL SCEN1MAIN(ITYPE,MESSAGE)

  !## Message from scenario-tool Menu
  CASE(ID_DSCENTOOL,ID_DSCENTOOLTAB1,ID_DSCENTOOLTAB2,ID_DSCENTOOLTAB3,ID_DSCENTOOLTAB4,ID_DSCENTOOLTAB5)
   CALL ST1MAIN(ITYPE,MESSAGE)
   
  !## Message from scenario-tool WellMenu
  CASE(ID_DSCENTOOL_PROP,ID_DSCENTOOL_PROPTAB1,ID_DSCENTOOL_PROPTAB2,ID_DSCENTOOL_PROPTAB3)
   CALL ST1PROPMAIN(ITYPE,MESSAGE)

  !## Message from tools menu for waterbalance/gxg
  CASE(ID_TOOLS,ID_TOOLSTAB1,ID_TOOLSTAB2,ID_TOOLSTAB3,ID_TOOLSTAB4)
   CALL TOOLS1MAIN(ITYPE,MESSAGE)

  !## Message from imodbatch
  CASE(ID_DBATCH)
   CALL CREATEIMODBATCHMAIN(ITYPE,MESSAGE)

  !## Message from tvariant statistics
  CASE(ID_DTSTAT)
   CALL TSTAT1MAIN(ITYPE,MESSAGE)

  !## Message from idfedit Menu
  CASE(ID_DIDFEDIT,ID_DIDFEDITTAB1,ID_DIDFEDITTAB2)
   CALL IDFEDITMAIN(ITYPE,MESSAGE)

  !## Message from solid Menu
  CASE(ID_DSOLID,ID_DSOLIDTAB1,ID_DSOLIDTAB2,ID_DSOLIDTAB3)
   CALL SOLIDMAIN(ITYPE,MESSAGE)

  !## Message from isgedit Menu
  CASE(ID_DISGEDIT,ID_DISGEDITTAB1)
   CALL ISGEDITMAIN(ITYPE,MESSAGE)
  CASE(ID_DISGEDITTAB2,ID_DISGEDITTAB3,ID_DISGEDITTAB4,ID_DISGEDITTAB5,ID_DISGEDITTAB6,ID_DISGEDITTAB7)
   CALL ISGADJUSTMAIN(ITYPE,MESSAGE)

  !## Message from idfeditcalc Menu
  CASE(ID_DIDFEDITCALC)
   CALL IDFEDITCALCMAIN(ITYPE,MESSAGE)

  !## Message from pathlines Menu
  CASE(ID_DSPOINTS,ID_DSPTAB1,ID_DSPTAB2)
   CALL STARTP1MAIN(ITYPE,MESSAGE)

  !## Message from pathlines Menu
  CASE(ID_DPATHLINES,ID_DPATHTAB1,ID_DPATHTAB2,ID_DPATHTAB3,ID_DPATHTAB4,ID_DPATHTAB5)
   CALL PLINES1MAIN(ITYPE,MESSAGE)

  !## Message from extractipf
  CASE(ID_DEXTRACT)
   IF(WMENUGETSTATE(ID_EXTRACTIPF,2).EQ.1)CALL EXTRACTIPF1MAIN(ITYPE,MESSAGE)

! Message from RO
!  CASE (ID_DRO)
!   CALL RO1MAIN(ITYPE,MESSAGE)
! Message from ROSCEN
  CASE (ID_DROSCEN,ID_DROSCENT1,ID_DROSCENT2,ID_DROSCENT3,ID_DROSCENT4)
   CALL ROSCENMAIN(ITYPE,MESSAGE)

  !## Message from CREATEGEN
  CASE (ID_DCREATEGEN)
   CALL CREATEGEN1MAIN(ITYPE,MESSAGE)
  !## Message from CREATEIPF
  CASE (ID_DCREATEIPF)
   CALL CREATEIPF1MAIN(ITYPE,MESSAGE)
  !## Message from createidf
  CASE(ID_DCREATEIDF,ID_DCREATEIDFTAB1,ID_DCREATEIDFTAB2,ID_DCREATEIDFTAB3)
   CALL CREATEIDF1MAIN(ITYPE,MESSAGE)

  !## Message from quickopen
  CASE(ID_DQUICKOPEN)
   CALL IDFQUICKOPEN_MAIN(ITYPE,MESSAGE)

 END SELECT
ENDDO

STOP
END PROGRAM IMODPRG