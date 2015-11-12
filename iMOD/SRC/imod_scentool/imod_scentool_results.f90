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
MODULE MOD_SCENTOOL_RESULTS

USE WINTERACTER
USE RESOURCE
USE MOD_COLOURS
USE MOD_SCENTOOL_PAR
USE MOD_SCENTOOL_WELLS, ONLY : ST1SIMBOXWELLS,ST1CREATEIPF
USE MOD_SCENTOOL_UTL, ONLY : ST1FILLRESULTS,ST1FIELDS
USE MOD_UTL, ONLY : UTL_HIDESHOWDIALOG,UTL_CREATEDIR,UTL_MESSAGEHANDLE,UTL_GETUNIT,ITOS,RTOS,UTL_JDATETOIDATE,UTL_DEL1TREE, &
                    UTL_IDATETOJDATE,UTL_FILLDATESDIALOG,UTL_DIRINFO,UTL_IDFGETDATE
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFDEALLOCATE,IDFDEALLOCATEX,IDFNULLIFY,IDFREAD,IDFGETVAL,IDFIROWICOL,IDFWRITE
USE MOD_MODEL, ONLY : MODEL1READRUNFILE,MODEL1WRITERUNFILE,MODEL1DEALLOCATE,MODEL1START,MODEL1CHECKRUNFILE
USE MOD_MDL_PAR, ONLY : NLMDL,ILMDL,IAMDL,SIMBOX,MAXSIMCSIZE,SIMCSIZE,NMULT,NSCL,IBUFFER,NLAY, &
  NMP_ACT,MDL_IPOS,MDLKEYWS
USE MOD_OSD, ONLY : OSD_OPEN
USE DATEVAR
USE MOD_PROFILE_UTL, ONLY : GRAPH,GRAPHNAMES,PROFILE_PLOTGRAPH,PROFILE_DEALLGRAPH,PROFILE_ALLGRAPH
USE MOD_QUICKOPEN, ONLY : IDFQUICKOPEN_INIT

TYPE(IDFOBJ),PRIVATE,ALLOCATABLE,DIMENSION(:) :: IDF
INTEGER,PRIVATE :: MINDATE,MAXDATE
INTEGER,PRIVATE,ALLOCATABLE,DIMENSION(:) :: LID
INTEGER,PRIVATE,ALLOCATABLE,DIMENSION(:) :: IOKAY
INTEGER,DIMENSION(:),ALLOCATABLE,PRIVATE :: ILIST,JLIST
CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:),PRIVATE :: IDFNAMES
REAL,ALLOCATABLE,DIMENSION(:) :: TOP,BOT,L

CONTAINS

 !###======================================================================
 SUBROUTINE STRES1MAIN()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ITYPE
 TYPE(WIN_MESSAGE) :: MESSAGE

 CALL WDIALOGLOAD(ID_DSCENTOOL_RESULTS,ID_DSCENTOOL_RESULTS)
 
 !## get simulation periods (well systems/observation wells)
 IF(.NOT.STRES1SIMTIME())THEN
  IF(STRES1CLOSE())RETURN
 ENDIF
 
 CALL UTL_HIDESHOWDIALOG(ID_DSCENTOOL,0)

 CALL WDIALOGSELECT(ID_DSCENTOOL_RESULTS)
 CALL WDIALOGPUTIMAGE(ID_INFO,ID_ICONINFO)
 CALL WDIALOGPUTIMAGE(ID_CHECKRUN,ID_ICONOKAY)
 
 !## date section
 CALL WDIALOGFIELDSTATE(IDF_INTEGER2,2)
 CALL WDIALOGFIELDSTATE(IDF_INTEGER3,2) 
 CALL WDIALOGFIELDSTATE(IDF_INTEGER4,2)
 CALL WDIALOGFIELDSTATE(IDF_INTEGER5,2) 
 CALL WDIALOGFIELDSTATE(IDF_MENU3,2)
 CALL WDIALOGFIELDSTATE(IDF_MENU4,2) 
 CALL WDIALOGPUTMENU(IDF_MENU3,CDATE,12,1)
 CALL WDIALOGPUTMENU(IDF_MENU4,CDATE,12,12)
 CALL UTL_FILLDATESDIALOG(ID_DSCENTOOL_RESULTS,IDF_INTEGER2,IDF_MENU3,IDF_INTEGER3,UTL_JDATETOIDATE(MINDATE)) !## mindate
 CALL UTL_FILLDATESDIALOG(ID_DSCENTOOL_RESULTS,IDF_INTEGER4,IDF_MENU4,IDF_INTEGER5,UTL_JDATETOIDATE(MAXDATE)) !## maxdate

 CALL STRES1PUTFIELDS()
 CALL STRES1FIELDS()

 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)

  SELECT CASE (ITYPE)
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_REAL1,IDF_REAL2,IDF_REAL3)
      CALL STRES1FIELDS()
     CASE (IDF_INTEGER2,IDF_INTEGER3,IDF_INTEGER4,IDF_INTEGER5,IDF_MENU3,IDF_MENU4)
      !CALL UTL_FILLDATES() <--- bij datum aanpassing
     CASE (IDF_RADIO1,IDF_RADIO2,IDF_CHECK1,IDF_CHECK2)
     !## get simulation periods (well systems/observation wells)
     IF(.NOT.STRES1SIMTIME())THEN
      IF(STRES1CLOSE())EXIT
     ENDIF
    END SELECT
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     !## runfile info
     CASE (ID_INFO,ID_CHECKRUN)
      CALL STRES1RUNINFO(MESSAGE%VALUE1)
     !## start simulation
     CASE (IDOK)
      IF(STRES1START())THEN
       IF(STRES1CLOSE())EXIT
      ENDIF
     !## quit
     CASE (IDCANCEL)
      IF(STRES1CLOSE())EXIT
    END SELECT
  END SELECT

 ENDDO

 CALL UTL_HIDESHOWDIALOG(ID_DSCENTOOL,2)
 CALL ST1FILLRESULTS()
 CALL STRES1TITLE()
 CALL ST1FIELDS()

 END SUBROUTINE STRES1MAIN

 !###======================================================================
 SUBROUTINE STRES1TITLE()
 !###======================================================================
 IMPLICIT NONE
 
 CALL WDIALOGSELECT(ID_DSCENTOOLTAB5)
 CALL WDIALOGTITLE('Results ('//TRIM(ITOS(NRES))//')')
 IF(NRES.EQ.0)THEN
  CALL WDIALOGCLEARFIELD(IDF_MENU1)
 ELSE
  CALL WDIALOGPUTMENU(IDF_MENU1,RES%CNAME,NRES,RES%IRES)
 ENDIF
 
 END SUBROUTINE STRES1TITLE

 !###======================================================================
 SUBROUTINE STRES1TSERIE_INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,NG,ILAY,NO,NR,IL,IROW,ICOL,IOBS
 REAL :: Z1,Z2,ZCOR,ML
   
 CALL UTL_MESSAGEHANDLE(0)

 NO=0 !## no.observations
 NR=0 !## no.modelresults
 DO I=1,NOBS; NO=NO+(OBS(I)%NZ-1); ENDDO !## maximum observations
 DO I=1,NRES; IF(RES(I)%IRES.EQ.1)NR=NR+1; ENDDO

 ALLOCATE(TOP(STNLAY),BOT(STNLAY),L(STNLAY))
 CALL PROFILE_ALLGRAPH(NR+1,NOBS)
 GRAPH%LEGTXT=''

 !## generate rx/ry values for each observations
 DO NG=1,NOBS
  
  !## name of the observation
  GRAPHNAMES(NG)=TRIM(ADJUSTL(OBS(NG)%CNAME))  !'Observation '//TRIM(ITOS(NG))
  
  IF(NG.EQ.1)THEN
   DO ILAY=1,STNLAY
    IF(.NOT.IDFREAD(TOPIDF(ILAY),TOPIDF(ILAY)%FNAME,0))THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not read '//TRIM(TOPIDF(ILAY)%FNAME),'Error')
     EXIT
    ENDIF
    IF(.NOT.IDFREAD(BOTIDF(ILAY),BOTIDF(ILAY)%FNAME,0))THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not read '//TRIM(BOTIDF(ILAY)%FNAME),'Error')
     EXIT
    ENDIF
   ENDDO
  ENDIF

  !## only one location
  IOBS=1
  !## get top/bottoms
  DO ILAY=1,STNLAY
   TOP(ILAY)=TOPIDF(ILAY)%NODATA
   BOT(ILAY)=BOTIDF(ILAY)%NODATA
   CALL IDFIROWICOL(TOPIDF(ILAY),IROW,ICOL,OBS(NG)%LOC(IOBS)%X,OBS(NG)%LOC(IOBS)%Y)
   IF(IROW.NE.0.AND.ICOL.NE.0)TOP(ILAY)=IDFGETVAL(TOPIDF(ILAY),IROW,ICOL)
   CALL IDFIROWICOL(BOTIDF(ILAY),IROW,ICOL,OBS(NG)%LOC(IOBS)%X,OBS(NG)%LOC(IOBS)%Y)
   IF(IROW.NE.0.AND.ICOL.NE.0)BOT(ILAY)=IDFGETVAL(BOTIDF(ILAY),IROW,ICOL)
  ENDDO

  ZCOR=0.0
  !## translate z from surfacelevel to msl
  IF(OBS(NG)%ILOCT.EQ.2)ZCOR=TOP(1)

  !## fit current position (compute length of well inside each modellayer)
  DO ILAY=1,STNLAY
   Z1=0.0
   Z2=0.0
   L(ILAY)=0.0
   IF(TOP(ILAY).NE.TOPIDF(ILAY)%NODATA.AND.BOT(ILAY).NE.BOTIDF(ILAY)%NODATA)THEN
    Z1=MIN(TOP(ILAY),ZCOR+OBS(NG)%LOC(IOBS)%Z1)
    Z2=MAX(BOT(ILAY),ZCOR+OBS(NG)%LOC(IOBS)%Z2)
    L(ILAY)=MAX(0.0,Z1-Z2)
   ENDIF
  END DO

  !## observation not in a single modellayer, terminate
  IF(SUM(L).EQ.0.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Observation screen is zero!','Error')
   EXIT
  ENDIF
  
  !## assign observation to modellayer that occupied most of it
  ML=0.0
  IL=0
  DO ILAY=1,STNLAY
   IF(L(ILAY).GT.ML)THEN
    ML=L(ILAY)
    IL=ILAY
   ENDIF
  ENDDO

  !## fill in observation
  ALLOCATE(GRAPH(1,NG)%RX(OBS(NG)%NZ-1),GRAPH(1,NG)%RY(OBS(NG)%NZ-1))
  !## observation
  DO J=1,OBS(NG)%NZ-1
   GRAPH(1,NG)%RX(J)=OBS(NG)%Z(J)%IDATE
   GRAPH(1,NG)%RY(J)=ZCOR+OBS(NG)%Z(J)%MEASURE
  ENDDO
  GRAPH(1,NG)%NP=OBS(NG)%NZ-1
  !## lines
  GRAPH(1,NG)%GTYPE=2
  GRAPH(1,NG)%LEGTXT=TRIM(ADJUSTL(OBS(NG)%CNAME))
  GRAPH(1,NG)%ICLR  =ICOLOR(1)  
  
  !## get result for current observation point
  IF(.NOT.STRES1TSERIE(OBS(NG)%LOC(IOBS)%X,OBS(NG)%LOC(IOBS)%Y,-999.99,NG,IL))EXIT
 
 ENDDO

 CALL UTL_MESSAGEHANDLE(1)

 !## go to the plot-mode
 IF(NG.GT.NOBS)CALL PROFILE_PLOTGRAPH('Date','Heads (m+MSL)',.TRUE.)

 CALL STRES1TSERIE_DEAL()
 
 END SUBROUTINE STRES1TSERIE_INIT

 !###======================================================================
 SUBROUTINE STRES1MAPS()
 !###======================================================================
 IMPLICIT NONE

 CALL IDFQUICKOPEN_INIT(1,(/'SCENTOOL'/))
 CALL WDIALOGSELECT(ID_DQUICKOPEN)
! CALL WDIALOGPUTOPTION(IDF_MENU1,3) !## select scentool

 END SUBROUTINE STRES1MAPS

 !###======================================================================
 LOGICAL FUNCTION STRES1TSERIE(XC,YC,NODATA,NG,ILAY)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: XC,YC,NODATA
 INTEGER,INTENT(IN) :: ILAY,NG
 INTEGER :: I,J,N,IROW,ICOL,NR
 
 STRES1TSERIE=.FALSE.
 
 !## dimension idf
 ALLOCATE(IDF(1)); CALL IDFNULLIFY(IDF(1))

 CALL IOSDIRENTRYTYPE('F')
  
 !## head steady-state hernoemen naar datum (start datum!!!!)

 !## count number of files
 NR=0
 DO I=1,NRES
  IF(RES(I)%IRES.EQ.1)THEN
   NR=NR+1
   !## get number of files
   IF(.NOT.STRES1TSERIE_GETNOFILES(TRIM(RES(I)%CNAME),N,TRIM(ITOS(ILAY))))RETURN
   ALLOCATE(IDFNAMES(N))
   CALL UTL_DIRINFO(SCFFNAME(:INDEX(SCFFNAME,'.',.TRUE.)-1)//'\'//TRIM(RES(I)%CNAME)//'\head', &
    'head_*_l'//TRIM(ITOS(ILAY))//'.idf',IDFNAMES,N,'F') 

   ALLOCATE(GRAPH(1+NR,NG)%RX(N),GRAPH(1+NR,NG)%RY(N))

   DO J=1,N
    IF(.NOT.IDFREAD(IDF(1),SCFFNAME(:INDEX(SCFFNAME,'.',.TRUE.)-1)//'\'// &
           TRIM(RES(I)%CNAME)//'\head\'//TRIM(IDFNAMES(J)),0))RETURN
    CALL IDFIROWICOL(IDF(1),IROW,ICOL,XC,YC)
    IF(ICOL.NE.0.AND.IROW.NE.0)THEN
     GRAPH(1+NR,NG)%RY(J)=IDFGETVAL(IDF(1),IROW,ICOL)
    ENDIF
    GRAPH(1+NR,NG)%RX(J)=REAL(IDF(1)%JD)
    CLOSE(IDF(1)%IU)
   ENDDO
   GRAPH(1+NR,NG)%GTYPE =2
   GRAPH(1+NR,NG)%NP    =N
   GRAPH(1+NR,NG)%LEGTXT=TRIM(ADJUSTL(RES(I)%CNAME))//'(ilay='//TRIM(ITOS(ILAY))//')'
   GRAPH(1+NR,NG)%ICLR  =ICOLOR(1+NR)

   DEALLOCATE(IDFNAMES)

  ENDIF
 ENDDO
 CALL IDFDEALLOCATE(IDF,SIZE(IDF))
 DEALLOCATE(IDF)
 
 STRES1TSERIE=.TRUE.
 
 END FUNCTION STRES1TSERIE
 
 !###======================================================================
 LOGICAL FUNCTION STRES1TSERIE_GETNOFILES(DIRNAME,N,CL)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIRNAME,CL
 INTEGER,INTENT(OUT) :: N

 STRES1TSERIE_GETNOFILES=.FALSE.

 !## fill dialog with information for selected idf's
 N=0
 CALL IOSDIRCOUNT(SCFFNAME(:INDEX(SCFFNAME,'.',.TRUE.)-1)//'\'//DIRNAME//'\head', &  !## folder
   'head_*_l'//CL//'.idf',N)
 IF(N.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No HEAD results found for modellayer '//CL//CHAR(13)// &
    'in results version '//DIRNAME//CHAR(13)//'for scenario project'//CHAR(13)// &
    SCFFNAME(:INDEX(SCFFNAME,'.',.TRUE.)-1),'Warning')
  RETURN
 ENDIF

 STRES1TSERIE_GETNOFILES=.TRUE.

 END FUNCTION STRES1TSERIE_GETNOFILES

 !###======================================================================
 SUBROUTINE STRES1TSERIE_DEAL()
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: LEX
 INTEGER :: I

 DO I=1,STNLAY
  IF(TOPIDF(I)%IU.GT.0)THEN
   INQUIRE(FILE=TOPIDF(I)%FNAME,OPENED=LEX)
   IF(LEX)CLOSE(TOPIDF(I)%IU)
   TOPIDF(I)%IU=0
  ENDIF
  IF(BOTIDF(I)%IU.GT.0)THEN
   INQUIRE(FILE=BOTIDF(I)%FNAME,OPENED=LEX)
   IF(LEX)CLOSE(BOTIDF(I)%IU)
   BOTIDF(I)%IU=0
  ENDIF
 ENDDO

 IF(ALLOCATED(IDF))THEN
  CALL IDFDEALLOCATEX(IDF(1))
  INQUIRE(UNIT=IDF(1)%IU,OPENED=LEX)
  IF(LEX)CLOSE(IDF(1)%IU)
  DEALLOCATE(IDF)
 ENDIF

 IF(ALLOCATED(IDFNAMES))DEALLOCATE(IDFNAMES) 
 IF(ALLOCATED(TOP))DEALLOCATE(TOP)
 IF(ALLOCATED(BOT))DEALLOCATE(BOT)
 IF(ALLOCATED(L))DEALLOCATE(L)

 !## deallocate graph object
 CALL PROFILE_DEALLGRAPH()

! IF(ALLOCATED(ISE))DEALLOCATE(ISE)
! IF(ALLOCATED(RX))DEALLOCATE(RX)
! IF(ALLOCATED(RY))DEALLOCATE(RY)
  
 END SUBROUTINE STRES1TSERIE_DEAL
 
 !###======================================================================
 SUBROUTINE STRES1RUNINFO(CODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: CODE
 LOGICAL :: LEX
 INTEGER :: I,IWIN

 ALLOCATE(ILIST(NSCNCONF))
 !## get selected runfiles
 CALL WDIALOGGETMENU(IDF_MENU1,ILIST)
 DO I=1,SIZE(ILIST); IF(ILIST(I).EQ.1)EXIT; ENDDO
 IWIN=SIZE(ILIST)
 DEALLOCATE(ILIST)

 IF(I.GT.IWIN)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Select at least on Model Configuration','Error')
  RETURN
 ENDIF

 INQUIRE(FILE=CONF(I)%RUNF,EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open runfile:'//CHAR(13)// &
   TRIM(CONF(I)%RUNF),'Error')
  RETURN
 ENDIF

 IF(CODE.EQ.ID_INFO)THEN
  IWIN=0
  CALL WINDOWOPENCHILD(IWIN,FLAGS=SYSMENUON+MAXBUTTON,WIDTH=1000,HEIGHT=500)
  CALL WINDOWSELECT(IWIN)
  CALL WEDITFILE(CONF(I)%RUNF,ITYPE=MODAL,IDMENU=0,IFONT=COURIERNEW,ISIZE=10)
 ELSEIF(CODE.EQ.ID_CHECKRUN)THEN
  CALL MODEL1CHECKRUNFILE(CONF(I)%RUNF)
 ENDIF

 END SUBROUTINE STRES1RUNINFO

 !###======================================================================
 LOGICAL FUNCTION STRES1START()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,&
            IMODFLOW,& !# type of modflow program
            IVERSION
 CHARACTER(LEN=256) :: DIR

 STRES1START=.FALSE.

 !## are you sure?
 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to continue ?','Question')
 IF(WINFODIALOG(4).NE.1)RETURN

 ALLOCATE(IOKAY(NSCNCONF),ILIST(NSCNCONF),JLIST(MXRES))

 !## construct result map (name of the scf-filename):
 CALL UTL_CREATEDIR(SCFFNAME(:INDEX(SCFFNAME,'.',.TRUE.)-1))

 !## get selected runfiles
 CALL WDIALOGGETMENU(IDF_MENU1,ILIST)
 !## get selected result-types
 CALL WDIALOGGETMENU(IDF_MENU2,JLIST)
 !## get version number
 CALL WDIALOGGETINTEGER(IDF_INTEGER1,IVERSION)

 IF((SUM(ILIST).EQ.0.OR.SUM(JLIST).EQ.0).OR.JLIST(1).EQ.0)THEN
  DEALLOCATE(ILIST,JLIST,SIMDELT,SIMJDATE)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should select at least one Simulation Configuration'//CHAR(13)// &
   'and at least one Simulation Result (phreatic heads)','Error')
  RETURN
 ENDIF
 
 CALL UTL_MESSAGEHANDLE(0)

 IOKAY=ILIST
 DO I=1,NSCNCONF
  !## current configuration selected to be computed
  IF(ILIST(I).EQ.1)THEN

   !## construct result map:
   DIR=SCFFNAME(:INDEX(SCFFNAME,'.',.TRUE.)-1)//'\V'//TRIM(ITOS(IVERSION))//'_'//TRIM(CONF(I)%RUNNAME)
   CALL UTL_CREATEDIR(DIR)

   !## read runfile
   IF(.NOT.MODEL1READRUNFILE(CONF(I)%RUNF))EXIT

   !## get simbox
   IF(.NOT.STRES1SIMBOX())RETURN

   !## create ipf for wel-system
   IF(.NOT.ST1CREATEIPF(NLAY,DIR))EXIT !IU))EXIT
   !## add all other modifications here
!   IF(.NOT.)EXIT

   !## nothing to be saved - initially
   NLMDL=0
   ILMDL=0

   !## save heads/budgets for all layers - in case flowlines (options 2 & 3)
   IF(JLIST(3).EQ.1.OR.JLIST(4).EQ.1)THEN
    DO J=3,6
     NLMDL(J)=NLAY
     ILMDL(J,1:NLAY)=(/(K,K=1,NLAY)/) !## heads,(frf/fff),flf,sto
    END DO
   !## save heads for all modellayer (cause observations need resuls for different modellayers)
   ELSE
    NLMDL(3)=NLAY
    ILMDL(3,1:NLAY)=(/(K,K=1,NLAY)/)   !## heads
   ENDIF

   IBUFFER=0 !## not saving results in buffer
   !## rewrite runfile
   IF(.NOT.MODEL1WRITERUNFILE(CONF(I)%RUNF,DIR,1,SIMNPER,ITOS(UTL_JDATETOIDATE(SIMJDATE(0)))))EXIT
   !## add scenario files for next stressperiods
   IF(.NOT.STRES1APPENDRUNFILE(DIR))EXIT

   !## start simulation
   CALL MODEL1START(CONF(I)%RUNF,DIR,1)

   CALL MODEL1DEALLOCATE()
   !## successfull completed
   IOKAY(I)=0

  ENDIF
 END DO

 IF(SUM(IOKAY).EQ.0)STRES1START=.TRUE.

 !## compute drawdown
 IF(JLIST(2).EQ.1)CALL STRES1DRAWDOWN(DIR)

 CALL STRES1DEALLOCATE()
 CALL UTL_MESSAGEHANDLE(1)

 END FUNCTION STRES1START

 !###======================================================================
 SUBROUTINE STRES1DRAWDOWN(DIR)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: IPER,ILAY,I

 CALL UTL_CREATEDIR(TRIM(DIR)//'\drawdown')

 ALLOCATE(DDNIDF(2))
 DO I=1,SIZE(DDNIDF); CALL IDFNULLIFY(DDNIDF(I)); ENDDO

 DO ILAY=1,NLAY
  !## open
  IF(.NOT.IDFREAD(DDNIDF(1),TRIM(DIR)//'\head\head_'//TRIM(ITOS(UTL_JDATETOIDATE(SIMJDATE(0))))// &
           '_l'//TRIM(ITOS(ILAY))//'.idf',1))RETURN
  DO IPER=1,SIMNPER
   IF(.NOT.IDFREAD(DDNIDF(2),TRIM(DIR)//'\head\head_'//TRIM(ITOS(UTL_JDATETOIDATE(SIMJDATE(IPER))))// &
            '_l'//TRIM(ITOS(ILAY))//'.idf',1))RETURN
   DDNIDF(2)%X=DDNIDF(2)%X-DDNIDF(1)%X
   IF(.NOT.IDFWRITE(DDNIDF(2),TRIM(DIR)//'\drawdown\drawdown_'//TRIM(ITOS(UTL_JDATETOIDATE(SIMJDATE(IPER))))// &
            '_l'//TRIM(ITOS(ILAY))//'.idf',1))RETURN  !## no question
  END DO
 ENDDO

 END SUBROUTINE STRES1DRAWDOWN

 !###======================================================================
 SUBROUTINE STRES1DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(ILIST))   DEALLOCATE(ILIST)
 IF(ALLOCATED(JLIST))   DEALLOCATE(JLIST)
 IF(ALLOCATED(IOKAY))   DEALLOCATE(IOKAY)
 IF(ALLOCATED(SIMDELT)) DEALLOCATE(SIMDELT)
 IF(ALLOCATED(SIMJDATE))DEALLOCATE(SIMJDATE)
 IF(ALLOCATED(LID))     DEALLOCATE(LID) 
 IF(ALLOCATED(DDNIDF))THEN
  CALL IDFDEALLOCATE(DDNIDF,SIZE(DDNIDF))
  DEALLOCATE(DDNIDF)
 ENDIF
 
 CALL MODEL1DEALLOCATE()

 END SUBROUTINE STRES1DEALLOCATE
 
 !###======================================================================
 LOGICAL FUNCTION STRES1SIMTIME()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,ID,TWEL,TOBS,IFIN
 REAL :: SUMQ

 STRES1SIMTIME=.FALSE.

 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,TWEL) !## use daily/stress timesteps
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,TOBS)    !## usage of timesteps in observation
 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,IFIN)    !## add final (infinite) solution
 
 SIMNPER=0

 !## find minimal data (julian date)
 MINDATE=UTL_IDATETOJDATE(21000101)
 MAXDATE=UTL_IDATETOJDATE(18000101)
 DO I=1,NWEL; MINDATE=MIN(MINDATE,WEL(I)%Q(1)%IDATE); ENDDO
 DO I=1,NWEL; MAXDATE=MAX(MAXDATE,WEL(I)%Q( WEL(I)%NQ )%IDATE); ENDDO

 IF(MAXDATE-MINDATE+1.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not determine duration of simulation.'//CHAR(13)// &
   'Probably you do not have entered any wells strenghts.','Error')
  RETURN
 ENDIF

 IF(ALLOCATED(LID))DEALLOCATE(LID)
 ALLOCATE(LID(MAXDATE-MINDATE+1+IFIN))
 LID=0
 !## last timestep is infinite timestep
 IF(IFIN.EQ.1)LID(IFIN)=1
 
 !## fill in all dates
 SUMQ=0.0
 DO I=1,NWEL
  DO J=1,WEL(I)%NQ
   ID     =WEL(I)%Q(J)%IDATE-MINDATE+1
   LID(ID)=1
   !## do not enter the last one!
   IF(J.LT.WEL(I)%NQ)SUMQ=SUMQ+ABS(WEL(I)%Q(J)%QRATE)
  END DO
 ENDDO

 IF(SUMQ.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not determine any rates for wells and/or well systems.'//CHAR(13)// &
   'Probably you do not have entered any wells strenghts.','Error')
  RETURN
 ENDIF

 !## fill in all observation dates (if they fit in the simulation period)
 IF(TOBS.EQ.1)THEN
  DO I=1,NOBS
   DO J=1,OBS(I)%NZ-1 !## skip last (is nothing!)
    ID     =OBS(I)%Z(J)%IDATE-MINDATE+1
    IF(ID.LT.SIZE(LID).AND.ID.GT.0)LID(ID)=1
   END DO
  ENDDO
 ENDIF
 
 !## OVERRULE whenever twel.eq.1: activate all days in between start/end period
 IF(TWEL.EQ.1)LID=1

 SIMNPER=0
 DO I=1,SIZE(LID); IF(LID(I).EQ.1)SIMNPER=SIMNPER+1; END DO
 IF(ALLOCATED(SIMDELT)) DEALLOCATE(SIMDELT)
 IF(ALLOCATED(SIMJDATE))DEALLOCATE(SIMJDATE)
 ALLOCATE(SIMDELT(SIMNPER),SIMJDATE(0:SIMNPER))
 SIMNPER=0
 DO I=1,SIZE(LID) 
  IF(LID(I).EQ.1)THEN
   SIMNPER=SIMNPER+1
   SIMDELT(SIMNPER)=REAL(I)-1
  ENDIF
 END DO

 !## compute timestep lengths
 DO I=SIZE(SIMDELT),2,-1; SIMDELT(I)=SIMDELT(I)-SIMDELT(I-1); ENDDO
 !## assume timestep length of first to be equal to the second (does not matter for computation, only for visualisation)
 SIMDELT(1)=SIMDELT(2)
 SIMJDATE(0)=MINDATE-SIMDELT(1)
 DO I=1,SIZE(SIMDELT); SIMJDATE(I)=SIMJDATE(I-1)+INT(SIMDELT(I)); ENDDO

 !## enter last timestep to be (semi) STEADY-STATE, otherwise wells are inactive!
 IF(IFIN.EQ.1)SIMDELT(SIZE(SIMDELT))=365.0 
 
 DEALLOCATE(LID)

 IF(IFIN.EQ.0)THEN
  CALL WDIALOGPUTSTRING(IDF_LABEL12, &
    '>>> Current Simulation consists out of (1) an initial steady state simulation (2) and simulation of '// &
    TRIM(ITOS(SIMNPER))//' intermediate stressperiods <<<')
 ELSE
  CALL WDIALOGPUTSTRING(IDF_LABEL12, &
    '>>> Current Simulation consists out of (1) an initial steady state simulation (2) and simulation of '// &
    TRIM(ITOS(SIMNPER-1))//' intermediate stressperiods and (3) a final steady state simulation <<<')
 ENDIF 
 
 STRES1SIMTIME=.TRUE.

 END FUNCTION STRES1SIMTIME

 !###======================================================================
 LOGICAL FUNCTION STRES1APPENDRUNFILE(RUN1)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: RUN1 
 INTEGER :: IU,JU,KU,IOS,IPER,I,K,NP,MP
 CHARACTER(LEN=256) :: LINE

 !## append scenario to runfile
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(RUN1)//'\IMODFLOW.RUN',STATUS='OLD',ACTION='WRITE', &
       POSITION='APPEND',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not open/add to '//CHAR(13)// &
   TRIM(RUN1)//'\IMODFLOW.RUN','Error')
  RETURN
 ENDIF

 DO IPER=1,SIMNPER

  !## write date/timestep
  LINE=TRIM(ITOS(IPER+1))//','//TRIM(RTOS(SIMDELT(IPER),'F',1))//','//&
       TRIM(ITOS(UTL_JDATETOIDATE(SIMJDATE(IPER))))//',-1'  !## simulation delt
  WRITE(IU,'(A)') TRIM(LINE)
  
  !## add packages --- only for those that are perturbed
  DO I=1,NMP_ACT
   K=MDL_IPOS(I)
   SELECT CASE (K)
    !## only packages
    CASE (15,16,17,18,19,20,21,22,23)
     JU=UTL_GETUNIT()
     CALL OSD_OPEN(JU,FILE=TRIM(RUN1)//'\scn_'//TRIM(MDLKEYWS(K))//'.RUN',STATUS='OLD', &
        ACTION='READ,DENYWRITE',IOSTAT=IOS)
     !## add scenario
     IF(IOS.EQ.0)THEN  
      KU=UTL_GETUNIT()
      CALL OSD_OPEN(KU,FILE=TRIM(RUN1)//'\org_'//TRIM(MDLKEYWS(K))//'.RUN',STATUS='OLD', &
         ACTION='READ,DENYWRITE',IOSTAT=IOS)
      IF(IOS.NE.0)THEN
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not read '//TRIM(RUN1)//'\org_'// &
         TRIM(MDLKEYWS(K))//'.RUN','Error')
       RETURN
      ENDIF
      READ(JU,*) NP
      READ(KU,*) MP
      LINE=TRIM(ITOS(NP+MP))//',('//TRIM(MDLKEYWS(K))//')'
      WRITE(IU,'(A)') TRIM(LINE)
      !## get original package info
      DO
       READ(KU,'(A256)',IOSTAT=IOS) LINE
       IF(IOS.NE.0)EXIT
       WRITE(IU,'(A)') TRIM(LINE)
      ENDDO
      DO
       READ(JU,'(A256)',IOSTAT=IOS) LINE
       IF(IOS.NE.0)EXIT
       WRITE(IU,'(A)') TRIM(LINE)
      ENDDO
      CLOSE(KU)
      CLOSE(JU)
     ELSE
      !## reuse same package from previous timestep
      WRITE(IU,'(A)') '-1,('//TRIM(MDLKEYWS(K))//')'
     ENDIF
   END SELECT
  ENDDO    

 END DO
 
 CLOSE(IU)
 
 STRES1APPENDRUNFILE=.TRUE.
 
 END FUNCTION STRES1APPENDRUNFILE
 
 !###======================================================================
 LOGICAL FUNCTION STRES1SIMBOX()
 !###======================================================================
 IMPLICIT NONE
 REAL :: X

 STRES1SIMBOX=.FALSE.

 SIMBOX(1)= 10.0E10 !## xmin
 SIMBOX(2)= 10.0E10 !## ymin
 SIMBOX(3)=-10.0E10 !## xmax
 SIMBOX(4)=-10.0E10 !## ymax

 CALL ST1SIMBOXWELLS(SIMBOX(1),SIMBOX(2),SIMBOX(3),SIMBOX(4))

 !## nothing found
 IF(SIMBOX(3).LT.SIMBOX(1).OR.SIMBOX(4).LT.SIMBOX(2))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not determine size of the model!'//CHAR(13)// &
   'Probably something wrong in the coordinates of the individual wells','Error')
  RETURN
 ENDIF

 !## read cellsize
 CALL WDIALOGGETREAL(IDF_REAL1,SIMCSIZE)
 CALL WDIALOGGETREAL(IDF_REAL2,MAXSIMCSIZE)
 CALL WDIALOGGETREAL(IDF_REAL3,X)
 SIMBOX(1)=SIMBOX(1)-X
 SIMBOX(2)=SIMBOX(2)-X
 SIMBOX(3)=SIMBOX(3)+X
 SIMBOX(4)=SIMBOX(4)+X

 !## define network in area of interest
 NMULT=1
 IF(SIMCSIZE.NE.MAXSIMCSIZE)NSCL=2

 STRES1SIMBOX=.TRUE.

 END FUNCTION STRES1SIMBOX

 !###======================================================================
 SUBROUTINE STRES1DELETE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WDIALOGSELECT(ID_DSCENTOOLTAB5)
 CALL WDIALOGGETMENU(IDF_MENU1,RES%IRES)

 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to delete simulation for all selected results?','Question')
 IF(WINFODIALOG(4).NE.1)RETURN
 
 DO I=1,SIZE(RES)
  IF(RES(I)%IRES.EQ.1)THEN
   CALL UTL_DEL1TREE(SCFFNAME(:INDEX(SCFFNAME,'.',.TRUE.)-1)//'\'//TRIM(RES(I)%CNAME))
  ENDIF
 ENDDO
 
 CALL ST1FILLRESULTS()
 CALL STRES1TITLE()
 CALL ST1FIELDS()

 END SUBROUTINE STRES1DELETE

 !###======================================================================
 SUBROUTINE STRES1PUTFIELDS()
 !###======================================================================
 IMPLICIT NONE

 ALLOCATE(ILIST(MAX(MXRES,NSCNCONF)))
 ILIST=0
 ILIST(1)=1
 CALL WDIALOGPUTMENU(IDF_MENU1,CONF%RUNNAME,NSCNCONF,ILIST(1:NSCNCONF))
 CALL WDIALOGPUTMENU(IDF_MENU2,RESLIST,MXRES,ILIST(1:MXRES))
 DEALLOCATE(ILIST)

 CALL WDIALOGFIELDOPTIONS(IDF_REAL1,EDITFIELDCHANGED,1)
 CALL WDIALOGFIELDOPTIONS(IDF_REAL2,EDITFIELDCHANGED,1)
 CALL WDIALOGPUTREAL(IDF_REAL1,25.0,'(F7.2)')
 CALL WDIALOGPUTREAL(IDF_REAL2,25.0,'(F7.2)')
 CALL WDIALOGPUTREAL(IDF_REAL3,1500.0,'(F7.2)')

 !## version number
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,1)

 END SUBROUTINE STRES1PUTFIELDS

 !###======================================================================
 LOGICAL FUNCTION STRES1GETFIELDS()
 !###======================================================================
 IMPLICIT NONE

 STRES1GETFIELDS=.FALSE.

 STRES1GETFIELDS=.TRUE.

 END FUNCTION STRES1GETFIELDS

 !###======================================================================
 SUBROUTINE STRES1FIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IACT
 REAL :: X1,X2

 CALL WDIALOGUNDEFINED(RVALUE=NODATAGRID)
 CALL WDIALOGSELECT(ID_DSCENTOOL_RESULTS)

 !## test rest of fields to be filled in completely
 IACT=1
 CALL WDIALOGGETREAL(IDF_REAL1,X1)
 IF(X1.LE.0.0)IACT=0
 !## cellsize
 CALL WDIALOGGETREAL(IDF_REAL2,X2)
 IF(X2.LE.0.0)IACT=0
 !## cellsize in buffer should be larger than inside area of interest
 IF(X1.GT.X2)IACT=0

 !## extra scenario-buffersize
 CALL WDIALOGGETREAL(IDF_REAL3,X1)
 IF(X1.LT.0.0)IACT=0

 CALL WDIALOGFIELDSTATE(IDOK,IACT)

 END SUBROUTINE STRES1FIELDS

 !###======================================================================
 LOGICAL FUNCTION STRES1CLOSE()
 !###======================================================================
 IMPLICIT NONE

 STRES1CLOSE=.FALSE.
 
 CALL STRES1DEALLOCATE()

! CALL WMESSAGEBOX(YESNOCANCEL,QUESTIONICON,COMMONNO,'Do you want to save any adjustments ?','Question')
! !## save and quit
! IF(WINFODIALOG(4).EQ.1)THEN
!  IF(.NOT.STRES1GETFIELDS())RETURN   !## get data from grid, if not correct, return - not closing!
! ELSE
!  !## not saving
!  IF(IOPT.EQ.ID_ADD)IWEL=IWEL-1
! ENDIF
! !## not canceling
! IF(WINFODIALOG(4).EQ.0)RETURN

! CALL WDIALOGSELECT(ID_DSCENTOOL_PROP)
 CALL WDIALOGUNLOAD()

 STRES1CLOSE=.TRUE.

 END FUNCTION STRES1CLOSE

END MODULE MOD_SCENTOOL_RESULTS