!!  Copyright (C) Stichting Deltares, 2005-2018.
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
!###======================================================================
PROGRAM IMODPRG
!###======================================================================
USE WINTERACTER
USE RESOURCE
USE MODPLOT
USE IMODVAR
USE MOD_IDFPLOT
USE MOD_POLYGON_PAR
USE MOD_POLYGON_UTL, ONLY : POLYGON1INIT
USE MOD_IR, ONLY : IR1MENU,IR1MAIN,IR1CLOSE
USE MOD_IR_PAR, ONLY : IRWIN
USE MOD_RO_SCEN, ONLY : ROSCENMAIN
USE MOD_PMANAGER, ONLY : PMANAGER_UTL_INIT,PMANAGERMAIN,PMANAGERRUN,PMANAGER_UTL_SHOW,PMANAGER_UTL_UPDATE,PMANAGERFIELDS
USE MOD_MANAGER, ONLY : MANAGERMAIN,MANAGER_UTL_INIT
USE MOD_MANAGER_UTL
USE MOD_PREF, ONLY : PREFINIT,PREFCOLOURSINIT,PREFVAL,PREFREAD,PREFUPDATE
USE MOD_UTL, ONLY : UTL_CAP,UTL_CREATEDIR,JD,UTL_IDATETOJDATE,ITOS,UTL_IMODVERSION
USE MOD_START, ONLY : START_MAIN
USE MOD_IDFEDIT, ONLY : IDFEDITCALCMAIN,IDFEDITMAIN
USE MOD_EXTRACTIPF, ONLY : EXTRACTIPF1MAIN
USE MOD_TOOLS, ONLY : TOOLS_MAIN
USE MOD_CREATEIDF, ONLY : CREATEIDF1MAIN
USE MOD_PLINES, ONLY : PLINES1MAIN
USE MOD_SPOINTS,ONLY : STARTP1_MAIN
USE MOD_MODEL, ONLY : MODEL1MAIN
USE MOD_SCENTOOL, ONLY : ST1MAIN,ST1PROPMAIN
USE MOD_IPF_PAR, ONLY : NLITHO,BH
USE MOD_IPFANALYSE, ONLY : IPFANALYSE_INIT_GRAPHVARIABLES
USE MOD_SOLID, ONLY : SOLID_MAIN
USE MOD_OSD, ONLY : OSD_GETARG,OSD_GETNARG
USE MOD_ABOUT, ONLY : IMOD_STARTSCREEN,IMOD_AGREEMENT
USE MOD_QUICKOPEN, ONLY : IDFQUICKOPEN_MAIN
USE MOD_ISG, ONLY : ISGEDITMAIN,ISGLEGENDMAIN
USE MOD_ISG_ADJ, ONLY : ISGADJUSTMAIN
USE MOD_CREATEGEN, ONLY : CREATEGEN1MAIN
USE MOD_CREATEIPF, ONLY : CREATEIPF1MAIN
USE MOD_INTERSECT, ONLY : INTERSECT_NULLIFY
USE MOD_KRIGING, ONLY : KRIGING_UNITTEST
USE MOD_BATCH, ONLY : IMODBATCH
USE MOD_BATCH_MAIN, ONLY : CREATEIMODBATCHMAIN
USE MOD_SUBSURFEX, ONLY : SUBSURFEXMAIN
USE MOD_3D, ONLY : IMOD3D_MAIN_MESSAGES,IMOD3D_RENDER
USE MOD_3D_DISPLAY, ONLY : IMOD3D_DISPLAY
USE MOD_3D_PAR, ONLY : IRENDER_3D !,D4ITIME
USE MOD_PLINES_PAR, ONLY : PL
USE MOD_PLINES_TRACE, ONLY : TRACE_3D_COMPUTE,TRACE_3D_COMPUTE_STOP
USE MOD_KRIGING, ONLY : KRIGING_UNITTEST
USE MOD_DEMO
USE MOD_GEOCONNECT, ONLY: GC_MAIN
USE MODPLOT
USE MOD_SOBEK, ONLY : ISOBEK
USE MOD_CONFIG
USE MOD_RESIDUALPLOT
USE MOD_MOVIE, ONLY : MOVIE_PLAY_MAIN
USE MOD_MAIN_UTL
USE MOD_GRAPH, ONLY : GRAPH_MAIN,GRAPH_DEALLOCATE
USE MOD_IPEST_ANALYSER, ONLY : IPEST_ANALYSE_MAIN
USE MOD_MAIN
USE MOD_WBAL_ANALYSE, ONLY : WBAL_ANALYSE_MAIN,WBAL_ANALYSE_TAB1,WBAL_ANALYSE_TAB2,WBAL_ANALYSE_TAB3,WBAL_ANALYSE_TAB4,WBAL_ANALYSE_TAB5
USE MOD_MSPINSPECTOR, ONLY : MSPINSPECTOR_MAIN
USE MOD_UZFANALYSER, ONLY : UZFANALYSER_MAIN
USE, INTRINSIC :: IEEE_EXCEPTIONS
IMPLICIT NONE
TYPE(WIN_MESSAGE) :: MESSAGE
INTEGER :: ITYPE,IERROR,I,NARG
CHARACTER(LEN=256) :: ARGSTRING,LEGNAME
INTEGER :: IY,IM,ID,IEXIT
REAL(KIND=DP_KIND) :: MOUSEX,MOUSEY

!CALL UTL_EQUALNAMES_UNITTEST()

!## set offset used in iMOD
OFFSETX=0.0D0
OFFSETY=0.0D0

#if (defined(DEBUG))
CALL IEEE_SET_HALTING_MODE (IEEE_DIVIDE_BY_ZERO, .TRUE.)
!## all kind of errors if reals are under- of overflow - particle tracking
!CALL IEEE_SET_HALTING_MODE (IEEE_OVERFLOW, .TRUE.)
!CALL IEEE_SET_HALTING_MODE (IEEE_UNDERFLOW, .TRUE.)
#endif

!## default settings for idfgetvalue
PLACES=15; DECPLACES=3; IFORM=1

CALL WINITIALISE()

!## get executable name
CALL GETARG(0,EXENAME); EXEPATH=TRIM(EXENAME(:INDEX(EXENAME,'\',.TRUE.)-1))
IF(TRIM(EXEPATH).EQ.'')EXEPATH='.'

IF(LEXPDATE)THEN
 CALL IOSDATE(IY,IM,ID)
 IF(JD(IY,IM,ID).GT.UTL_IDATETOJDATE(EXPDATE))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'This iMOD version is restricted for usage until '//TRIM(ITOS(EXPDATE)), &
     'Version exceeds expire date.')
  STOP
 ENDIF
ENDIF

CALL WBITMAPALLOC(40)

IF(IMODBATCH())STOP

!## get username and status and initialise window
CALL WINDOWOPEN(FLAGS=SYSMENUON+MINBUTTON+MAXBUTTON+STATUSBAR+MAXWINDOW,TITLE=TRIM(UTL_IMODVERSION()))
CALL WINDOWSTATUSBARPARTS(4,(/2000,2000,750,-1/),(/1,1,1,1/))
CALL WMENU(ID_MAINMENU1,0)
CALL WMENUTOOLBAR(ID_TOOLBAR1,0,1)

CALL IDEBUGLEVEL(DBGSILENT)
#if (defined(DEBUG))
 CALL IDEBUGLEVEL(DBGMSGBOX)
 CALL WMENUSETSTATE(ID_IDEBUGLEVEL0,2,0)
 CALL WMENUSETSTATE(ID_IDEBUGLEVEL4,2,1)
#endif
!## store debug level
ICDEBUGLEVEL=WINFOERROR(DEBUGLEVEL)

!## initialize preferences
CALL PREFINIT()

IERROR=0; CALL IMOD_AGREEMENT(IERROR)
IF(IERROR.NE.1)THEN
 IF(LBETA)THEN
  CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'Cannot start Beta-iMOD because you are not authorized in writing for Beta-iMOD','Error')
 ELSE
  CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'Cannot start iMOD unless you accept the iMOD Software License Agreement','Error')
 ENDIF
 CALL WINDOWCLOSE(); STOP
ENDIF

CALL IMOD_STARTSCREEN()

!## allocate zoom settings
NULLIFY(ZM%ZOOMXY,ZM%ZOOMXY_BU); ALLOCATE(ZM%ZOOMXY(10,4)); ZM%NZOOM=0; ZM%IZOOM=0

!## 24-bits colour application
CALL IGRCOLOURMODEL(24)
!## initialize colours
CALL PREFCOLOURSINIT(.TRUE.)
!## nullify pointer
CALL INTERSECT_NULLIFY()
!## allocate memory fo graph-variables ipf plotting
CALL IPFANALYSE_INIT_GRAPHVARIABLES()
!## load datamanager in memory
CALL MANAGER_UTL_INIT()
!## load project-datamanager in memory
CALL PMANAGER_UTL_INIT(); CALL PMANAGER_UTL_UPDATE(0,0,0); CALL PMANAGERFIELDS()
!## initialize iMOD
CALL IMODINIT()
!## initialize preferences
CALL PREFINIT()
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

IMOD_IUNITS=1  !## units are meters

!## initialize demo-mode
DEMO%IDEMO=0

!## initialize save directory location of open/save windows
SAVEDIR=TRIM(PREFVAL(1))//'\'

LEGNAME=''; CALL OSD_GETNARG(NARG)
IF(NARG.GT.0)THEN
 DO I=1,NARG
  CALL OSD_GETARG(I,ARGSTRING)
  IF(INDEX(UTL_CAP(ARGSTRING,'U'),'.IMF').GT.0)THEN
   IMFFNAME=ARGSTRING; CALL MAIN_UTL_LOAD_IMF(); CALL MAIN_UTL_LOAD()
   CALL IDFPLOTFAST(1)
   !## set variable on 1 from initial demo-mode, causes iMOD to stop whenever tool terminated
   IF(DEMO%IDEMO.NE.0)DEMO%INIT=1
  ELSEIF(INDEX(UTL_CAP(ARGSTRING,'U'),'.IDF').GT.0.OR. &
         INDEX(UTL_CAP(ARGSTRING,'U'),'.IPF').GT.0.OR. &
         INDEX(UTL_CAP(ARGSTRING,'U'),'.IFF').GT.0.OR. &
         INDEX(UTL_CAP(ARGSTRING,'U'),'.ISG').GT.0.OR. &
         INDEX(UTL_CAP(ARGSTRING,'U'),'.GEN').GT.0.OR. &
         INDEX(UTL_CAP(ARGSTRING,'U'),'.ASC').GT.0.OR. &
         INDEX(UTL_CAP(ARGSTRING,'U'),'.MAP').GT.0.OR. &
         INDEX(UTL_CAP(ARGSTRING,'U'),'.ARR').GT.0.OR. &
         INDEX(UTL_CAP(ARGSTRING,'U'),'.MDF').GT.0)THEN
   CALL MANAGER_UTL_ADDFILE(ARGSTRING,LEGNAME=LEGNAME); LEGNAME=''
   CALL IDFZOOM(ID_ZOOMFULLMAP,0.0D0,0.0D0,0)
   CALL IDFPLOTFAST(1)
  ELSEIF(INDEX(UTL_CAP(ARGSTRING,'U'),'.LEG').GT.0)THEN
   LEGNAME=ARGSTRING
  ELSEIF(INDEX(UTL_CAP(ARGSTRING,'U'),'.PRF').GT.0)THEN
   CALL PREFREAD(ARGSTRING)
   CALL PREFUPDATE()
  ELSEIF(INDEX(UTL_CAP(ARGSTRING,'U'),'.RUN').GT.0)THEN
   IF(PMANAGERRUN(ID_OPENRUN,ARGSTRING,0))THEN
    CALL PMANAGER_UTL_SHOW(1); CALL IDFPLOT(1)
    CALL UTL_CREATEDIR(TRIM(PREFVAL(1))//'\RUNFILES')
    CALL IOSCOPYFILE(ARGSTRING,TRIM(PREFVAL(1))//'\RUNFILES\'//TRIM(ARGSTRING(INDEX(ARGSTRING,'\',.TRUE.):)))
   ENDIF
  ELSE
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot recognize given argument'//CHAR(13)//TRIM(ARGSTRING),'Error')
   STOP
  ENDIF
 END DO
ENDIF

!## 3D demo, setup 3-D screen only
IF(DEMO%IDEMO.EQ.2)THEN
 CALL WINDOWSELECT(0); CALL WINDOWSIZEPOS(ISTATE=WINHIDDEN)
ENDIF

IF(NARG.EQ.0)THEN
 !## get username and status and initialise window
 CALL START_MAIN(IERROR); IF(IERROR.EQ.0)THEN; CALL WINDOWCLOSE(); STOP; ENDIF
 CALL IDFPLOT(1)
ENDIF

CALL WCURSORSHAPE(CURARROW)
!## for plotting purposes of shapes in idfedit
ICRD    =0
CRDITYPE=0
IDOWN   =0
DOWNX   =0.0D0
DOWNY   =0.0D0
ISHPEDIT=1    !## shapes are editable
PL%IRUN=0     !## no particle tracking active
IRENDER_3D=0  !## 0=3D INACTIVE; -1=3D ACTIVE, 1=RENDERING ACTIVE
GKEYPRESSED=0 !## nothing pressed

!## store first zoom-extent
ZM%NZOOM=1; ZM%IZOOM=1
ZM%ZOOMXY(ZM%NZOOM,1)=MPW%XMIN
ZM%ZOOMXY(ZM%NZOOM,2)=MPW%YMIN
ZM%ZOOMXY(ZM%NZOOM,3)=MPW%XMAX
ZM%ZOOMXY(ZM%NZOOM,4)=MPW%YMAX

CALL DEMO_MAIN()  !## calls demo-version iMOD directly after initialisation of iMOD manager and plotting available files
IF(DEMO%ISAVEBMP.EQ.1)THEN; CALL WINDOWCLOSE(); STOP; ENDIF

CALL WMENUSETSTATE(ID_GEOCONNECT,1,1)

!## initiate timers
CALL MAIN_UTL_TIMERS()

DO

 IF(PL%IRUN.EQ.1.OR.ABS(IRENDER_3D).EQ.1)THEN
  !## simulation busy
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
 ELSE
  !## wait for message
  CALL WMESSAGE(ITYPE,MESSAGE)
 ENDIF

 !## shift mouse coordinates
 MOUSEX=DBLE(MESSAGE%GX)+OFFSETX
 MOUSEY=DBLE(MESSAGE%GY)+OFFSETY

 SELECT CASE (ITYPE)
  CASE (KEYDOWN)
   SELECT CASE (MESSAGE%VALUE1)
    !## space key
    CASE (32)
     IRENDER_3D=-1*IRENDER_3D
   END SELECT
 END SELECT

 SELECT CASE(MESSAGE%WIN)

  !## Messages from child-windows/ir window
  CASE(0:20)
   IF(MESSAGE%WIN.NE.0.AND.MESSAGE%WIN.EQ.IRWIN)THEN
    CALL IR1MENU(ITYPE,MESSAGE)
   ELSEIF(MESSAGE%WIN.EQ.2)THEN
    CALL IMOD3D_MAIN_MESSAGES(ITYPE,MESSAGE)
   ELSE
    CALL IMOD1MENU(ITYPE,MESSAGE,MOUSEX,MOUSEY)
   ENDIF

  !## Message from manager Menu
  CASE(ID_DMANAGER,ID_DMANAGERTAB1,ID_DMANAGERTAB2,ID_DMANAGERTAB3,ID_DMANAGERTAB4)
   CALL MANAGERMAIN(ITYPE,MESSAGE)

  !## Message from project manager Menub
  CASE(ID_DPMANAGER)
   CALL PMANAGERMAIN(ITYPE,MESSAGE)

  !## Message from ir-manager MainMenu
  CASE(ID_DIR_PM,ID_DIR_PMTAB1,ID_DIR_PMTAB1TAB1,ID_DIR_PMTAB1TAB2, &
                 ID_DIR_PMTAB2,ID_DIR_PMTAB2TAB1,ID_DIR_PMTAB2TAB2,ID_DIR_PMTAB2TAB3,ID_DIR_PMTAB2TAB4, &
                 ID_DIR_PMTAB3,ID_DIR_PMTAB3TAB1,ID_DIR_PMTAB3TAB2)
   CALL IR1MAIN(ITYPE,MESSAGE)

  !## Message from model-manager Menu
  CASE(ID_DMODEL,ID_DMDLTAB1,ID_DMDLTAB2,ID_DMDLTAB3,ID_DMDLTAB4,ID_DMDLTAB6)
   CALL MODEL1MAIN(ITYPE,MESSAGE)

  !## Message from scenario-tool Menu
  CASE(ID_DSCENTOOL,ID_DSCENTOOLTAB1,ID_DSCENTOOLTAB2,ID_DSCENTOOLTAB3,ID_DSCENTOOLTAB4,ID_DSCENTOOLTAB5)
   CALL ST1MAIN(ITYPE,MESSAGE)

  !## Message from scenario-tool WellMenu
  CASE(ID_DSCENTOOL_PROP,ID_DSCENTOOL_PROPTAB1,ID_DSCENTOOL_PROPTAB2,ID_DSCENTOOL_PROPTAB3)
   CALL ST1PROPMAIN(ITYPE,MESSAGE)

  !## Message from tools menu for waterbalance/gxg
  CASE(ID_TOOLS,ID_TOOLSTAB1,ID_TOOLSTAB2,ID_TOOLSTAB3,ID_TOOLSTAB4)
   CALL TOOLS_MAIN(ITYPE,MESSAGE)

  !## Message from imodbatch
  CASE(ID_DBATCH)
   CALL CREATEIMODBATCHMAIN(ITYPE,MESSAGE)

  !## Message from subsurface explorer Menu
  CASE(ID_DSUBSURFEX)
   CALL SUBSURFEXMAIN(ITYPE,MESSAGE)

  !## Message from movie Menu
  CASE(ID_DMOVIE)
   CALL MOVIE_PLAY_MAIN(ITYPE,MESSAGE)

  !## Message from idfedit Menu
  CASE(ID_DIDFEDIT,ID_DIDFEDITTAB1,ID_DIDFEDITTAB2)
   CALL IDFEDITMAIN(ITYPE,MESSAGE)

  !## Message from solid Menu
  CASE(ID_DSOLID,ID_DSOLIDTAB1,ID_DSOLIDTAB2,ID_DSOLIDTAB3)
   CALL SOLID_MAIN(ITYPE,MESSAGE)

  !## Message from 3D Settings Menu
  CASE(ID_D3DSETTINGS,ID_D3DSETTINGS_TAB1,ID_D3DSETTINGS_TAB2,ID_D3DSETTINGS_TAB3,ID_D3DSETTINGS_TAB4,ID_D3DSETTINGS_TAB5, &
       ID_D3DSETTINGS_TAB6,ID_D3DSETTINGS_TAB7,ID_D3DSETTINGS_TAB8,ID_D3DSETTINGS_TAB9,ID_D3DSETTINGS_TAB9_TAB1,           &
       ID_D3DSETTINGS_TAB9_TAB2,ID_D3DSETTINGS_FENCES)
   CALL IMOD3D_MAIN_MESSAGES(ITYPE,MESSAGE)

  !## Message from wbal_analyse
  CASE(ID_DWBAL_ANALYSE)
   CALL WBAL_ANALYSE_MAIN(ITYPE,MESSAGE)
  CASE(ID_DWBAL_ANALYSE_TAB1)
   CALL WBAL_ANALYSE_TAB1(ITYPE,MESSAGE)
  CASE(ID_DWBAL_ANALYSE_TAB2)
   CALL WBAL_ANALYSE_TAB2(ITYPE,MESSAGE)
  CASE(ID_DWBAL_ANALYSE_TAB3)
   CALL WBAL_ANALYSE_TAB3(ITYPE,MESSAGE)
  CASE(ID_DWBAL_ANALYSE_TAB4)
   CALL WBAL_ANALYSE_TAB4(ITYPE,MESSAGE)
  CASE(ID_DWBAL_ANALYSE_TAB5)
   CALL WBAL_ANALYSE_TAB5(ITYPE,MESSAGE)

  !## Message from isgedit Menu
  CASE(ID_DISGEDIT,ID_DISGEDITTAB1)
   CALL ISGEDITMAIN(ITYPE,MESSAGE)
  CASE(ID_DISGEDITTAB2,ID_DISGEDITTAB3,ID_DISGEDITTAB4,ID_DISGEDITTAB5,ID_DISGEDITTAB6,ID_DISGEDITTAB7)
   CALL ISGADJUSTMAIN(ITYPE,MESSAGE)

  !## Message from idfeditcalc Menu
  CASE(ID_DIDFEDITCALC)
   CALL IDFEDITCALCMAIN(ITYPE,MESSAGE)

  !## Message from idfedit legend Menu
  CASE(ID_DISGEDITLEGEND)
   CALL ISGLEGENDMAIN(ITYPE,MESSAGE)

  !## Message from pathlines Menu
  CASE(ID_DSPOINTS,ID_DSPTAB1,ID_DSPTAB2)
   CALL STARTP1_MAIN(ITYPE,MESSAGE)

  !## Message from pathlines Menu
  CASE(ID_DPATHLINES,ID_DPATHTAB1,ID_DPATHTAB2,ID_DPATHTAB3,ID_DPATHTAB4,ID_DPATHTAB5)
   CALL PLINES1MAIN(ITYPE,MESSAGE)

  !## Message from extractipf
  CASE(ID_DEXTRACT)
   IF(WMENUGETSTATE(ID_EXTRACTIPF,2).EQ.1)CALL EXTRACTIPF1MAIN(ITYPE,MESSAGE)

  !## Message from roscen
  CASE (ID_DROSCEN,ID_DROSCENT1,ID_DROSCENT2,ID_DROSCENT3,ID_DROSCENT4)
   CALL ROSCENMAIN(ITYPE,MESSAGE)

  !## Message from creategen
  CASE (ID_DCREATEGEN)
   CALL CREATEGEN1MAIN(ITYPE,MESSAGE)

  !## Message from createipf
  CASE (ID_DCREATEIPF)
   CALL CREATEIPF1MAIN(ITYPE,MESSAGE)

  !## Message from createidf
  CASE(ID_DCREATEIDF,ID_DCREATEIDFTAB1,ID_DCREATEIDFTAB2,ID_DCREATEIDFTAB3)
   CALL CREATEIDF1MAIN(ITYPE,MESSAGE)

  !## Message from GeoConnect
  CASE(ID_DGEOCONNECT,ID_DGEOCONNECT_TAB1,ID_DGEOCONNECT_TAB2,ID_DGEOCONNECT_TAB3,ID_DGEOCONNECT_TAB4)
   CALL GC_MAIN(ITYPE,MESSAGE)
  
  !## Message from quickopen
  CASE(ID_DQUICKOPEN)
   CALL IDFQUICKOPEN_MAIN(ITYPE,MESSAGE)

  !## Message from ipest analyser
  CASE (ID_DIPESTANALYSE)
   CALL IPEST_ANALYSE_MAIN(ITYPE,MESSAGE)

  !## Message from metaswap analyser
  CASE (ID_DMSPANALYSER,ID_DMSPANALYSER_TAB1,ID_DMSPANALYSER_TAB2,ID_DMSPANALYSER_TAB3,ID_DMSPANALYSER_TAB4,ID_DMSPANALYSER_TAB5)
   CALL MSPINSPECTOR_MAIN(ITYPE,MESSAGE)

  !## Message from ipest analyser
  CASE (ID_DUZFANALYSER)
   CALL UZFANALYSER_MAIN(ITYPE,MESSAGE)

  !## Message from ipest analyser
  CASE (ID_DSCENTOOL_FIGURE)
   CALL GRAPH_MAIN(ITYPE,MESSAGE,IEXIT=IEXIT)
   IF(IEXIT.EQ.1)CALL GRAPH_DEALLOCATE()
   
 END SELECT

! IF(D4ITIME.NE.0)CALL IMOD3D_DISPLAY(1)

 !## do a particle tracking - if exists
 IF(PL%IRUN.EQ.1)THEN
  !## if not .true., than all particles finished
  IF(.NOT.TRACE_3D_COMPUTE())THEN
   CALL TRACE_3D_COMPUTE_STOP()
  ELSE
   CALL IMOD3D_DISPLAY(1)
  ENDIF
 ENDIF
 IF(IRENDER_3D.EQ.1)CALL IMOD3D_RENDER()

ENDDO

STOP
END PROGRAM IMODPRG
