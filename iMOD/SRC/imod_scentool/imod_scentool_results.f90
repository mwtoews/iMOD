!!  Copyright (C) Stichting Deltares, 2005-2019.
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
MODULE MOD_SCENTOOL_RESULTS

USE WINTERACTER
USE RESOURCE
USE MOD_PREF_PAR
USE MOD_COLOURS
USE MOD_SCENTOOL_PAR
USE MOD_SCENTOOL_UTL, ONLY : ST1FILLRESULTS,ST1FIELDS,ST1SIMBOXWELLS,ST1CREATEIPF,ST1CLOSEFILES,ST1ADDWELLSTOPRJ,ST1OPENFILES
USE MOD_UTL, ONLY : UTL_HIDESHOWDIALOG,UTL_CREATEDIR,UTL_MESSAGEHANDLE,UTL_GETUNIT,ITOS,RTOS,UTL_JDATETOIDATE,UTL_DEL1TREE, &
                    UTL_IDATETOJDATE,UTL_FILLDATESDIALOG,UTL_DIRINFO,UTL_IDFGETDATE,JDATETOGDATE
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFDEALLOCATE,IDFDEALLOCATEX,IDFNULLIFY,IDFREAD,IDFGETVAL,IDFIROWICOL,IDFWRITE
USE MOD_MODEL, ONLY : MODEL1CHECKRUNFILE
USE MOD_PMANAGER, ONLY : PMANAGERPRJ,PMANAGERRUN  
USE MOD_PMANAGER_UTL, ONLY : PMANAGER_DEALLOCATE,PMANAGER_DEALLOCATE_PEST,PMANAGER_GETNFILES

USE MOD_PMANAGER_PAR, ONLY : PBMAN,TOPICS,TTOP,TBOT,TBND,TSHD,TKDW,TKHV,TKVA,TVCW,TKVV,TSTO,TSPY

USE MOD_OSD, ONLY : OSD_OPEN
USE DATEVAR
USE MOD_GRAPH, ONLY : GRAPH,GRAPH_MAIN,GRAPH_INIT,GRAPH_ALLOCATE,GRAPHDIM,GRAPH_DEALLOCATE
USE MOD_QUICKOPEN, ONLY : IDFQUICKOPEN_INIT

TYPE(IDFOBJ),PRIVATE,ALLOCATABLE,DIMENSION(:) :: IDF
INTEGER,PRIVATE :: MINDATE,MAXDATE
INTEGER,PRIVATE,ALLOCATABLE,DIMENSION(:) :: LID
INTEGER,PRIVATE,ALLOCATABLE,DIMENSION(:) :: IOKAY
INTEGER,DIMENSION(:),ALLOCATABLE,PRIVATE :: ILIST,JLIST
CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:),PRIVATE :: IDFNAMES
REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: TOP,BOT,L

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
 INTEGER :: I,J,NG,ILAY,NO,NR,IL,IROW,ICOL,IOBS,NLAY,ITYPE,IEXIT
 REAL(KIND=DP_KIND) :: Z1,Z2,ZCOR,ML
 TYPE(WIN_MESSAGE) :: MESSAGE

 !## set i=1 to use the first configuration
 I=1

 !## read prjfile
 IF(.NOT.PMANAGERPRJ(ID_OPEN,CONF(I)%PRJF,0,0))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading project file '//TRIM(CONF(I)%PRJF),'Error')
  RETURN
 ENDIF

 !## get number of model layers
 CALL PMANAGER_GETNFILES((/TTOP,TBOT,TBND,TSHD,TKDW,TKHV,TKVA,TVCW,TKVV,TSTO,TSPY/),NLAY)

 CALL UTL_MESSAGEHANDLE(0)

 NO=0 !## no.observations
 NR=0 !## no.modelresults
 DO I=1,NOBS; NO=NO+(OBS(I)%NZ-1); ENDDO !## maximum observations
 DO I=1,NRES; IF(RES(I)%IRES.EQ.1)NR=NR+1; ENDDO

 ALLOCATE(TOP(NLAY),BOT(NLAY),L(NLAY))
 CALL GRAPH_ALLOCATE(NR+1,NOBS)
 GRAPH%LEGTXT=''
 
 !## open and read files
 IF(.NOT.ST1OPENFILES(NLAY))RETURN

 !## generate rx/ry values for each observations
 DO NG=1,NOBS
  
  !## name of the observation
  GRAPHDIM(NG)%GRAPHNAMES='' 
  GRAPHDIM(NG)%IGROUP=1
  
  !## only one location
  IOBS=1
  !## get top/bottoms
  DO ILAY=1,NLAY
   TOP(ILAY)=TOPIDF(ILAY)%NODATA
   BOT(ILAY)=BOTIDF(ILAY)%NODATA
   CALL IDFIROWICOL(TOPIDF(ILAY),IROW,ICOL,OBS(NG)%LOC(IOBS)%X,OBS(NG)%LOC(IOBS)%Y)
   IF(IROW.NE.0.AND.ICOL.NE.0)TOP(ILAY)=IDFGETVAL(TOPIDF(ILAY),IROW,ICOL)
   CALL IDFIROWICOL(BOTIDF(ILAY),IROW,ICOL,OBS(NG)%LOC(IOBS)%X,OBS(NG)%LOC(IOBS)%Y)
   IF(IROW.NE.0.AND.ICOL.NE.0)BOT(ILAY)=IDFGETVAL(BOTIDF(ILAY),IROW,ICOL)
  ENDDO

  ZCOR=0.0D0
  !## translate z from surfacelevel to msl
  IF(OBS(NG)%ILOCT.EQ.2)ZCOR=TOP(1)

  !## fit current position (compute length of well inside each modellayer)
  DO ILAY=1,NLAY
   Z1=0.0D0
   Z2=0.0D0
   L(ILAY)=0.0D0
   IF(TOP(ILAY).NE.TOPIDF(ILAY)%NODATA.AND.BOT(ILAY).NE.BOTIDF(ILAY)%NODATA)THEN
    Z1=MIN(TOP(ILAY),ZCOR+OBS(NG)%LOC(IOBS)%Z1)
    Z2=MAX(BOT(ILAY),ZCOR+OBS(NG)%LOC(IOBS)%Z2)
    L(ILAY)=MAX(0.0D0,Z1-Z2)
   ENDIF
  END DO

  !## observation not in a single modellayer, terminate
  IF(SUM(L).EQ.0.0D0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Observation screen is zero!','Error')
   EXIT
  ENDIF
  
  !## assign observation to modellayer that occupied most of it
  ML=0.0D0
  IL=0
  DO ILAY=1,NLAY
   IF(L(ILAY).GT.ML)THEN
    ML=L(ILAY)
    IL=ILAY
   ENDIF
  ENDDO

  !## fill in observation
  ALLOCATE(GRAPH(1,NG)%RX(OBS(NG)%NZ),GRAPH(1,NG)%RY(OBS(NG)%NZ))
  !## observation
  DO J=1,OBS(NG)%NZ !-1
   GRAPH(1,NG)%RX(J)=OBS(NG)%Z(J)%IDATE
   GRAPH(1,NG)%RY(J)=ZCOR+OBS(NG)%Z(J)%MEASURE
  ENDDO
  GRAPH(1,NG)%NP=OBS(NG)%NZ !-1

  !## lines
  GRAPH(1,NG)%GTYPE =2
  GRAPH(1,NG)%LEGTXT=TRIM(ADJUSTL(OBS(NG)%CNAME))
  GRAPH(1,NG)%ICLR  =ICOLOR(1)  
  
  !## get result for current observation point
  IF(.NOT.STRES1TSERIE(OBS(NG)%LOC(IOBS)%X,OBS(NG)%LOC(IOBS)%Y,NG,IL))EXIT
 
 ENDDO

 CALL UTL_MESSAGEHANDLE(1)
 GRAPHDIM(1)%IFIXX=0; GRAPHDIM(1)%IFIXY=0; GRAPHDIM(1)%XTITLE='Date'; GRAPHDIM(1)%YTITLE='Heads (m+MSL)'; GRAPHDIM(1)%LDATE=.TRUE.; GRAPHDIM(1)%TEXTSIZE=5.0D0
 !## go to the plot-mode
 IF(NG.GT.NOBS)THEN
  CALL GRAPH_INIT(3)
  DO
   CALL WMESSAGE(ITYPE,MESSAGE)
   CALL GRAPH_MAIN(ITYPE,MESSAGE,IEXIT=IEXIT)
   IF(IEXIT.EQ.1)EXIT
  ENDDO
 ENDIF
 
 CALL STRES1TSERIE_DEAL()
 
 END SUBROUTINE STRES1TSERIE_INIT

 !###======================================================================
 SUBROUTINE STRES1MAPS()
 !###======================================================================
 IMPLICIT NONE

 CALL IDFQUICKOPEN_INIT(1,(/'PUMPINGTOOL'/))
 CALL WDIALOGSELECT(ID_DQUICKOPEN)
! CALL WDIALOGPUTOPTION(IDF_MENU1,3) !## select scentool

 END SUBROUTINE STRES1MAPS

 !###======================================================================
 LOGICAL FUNCTION STRES1TSERIE(XC,YC,NG,ILAY)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: XC,YC
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
    'head_????????_l'//TRIM(ITOS(ILAY))//'.idf',IDFNAMES,N,'F') 

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
    'in results version '//DIRNAME//CHAR(13)//'for pumping project'//CHAR(13)// &
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

 CALL ST1CLOSEFILES()

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
 CALL GRAPH_DEALLOCATE()

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

 INQUIRE(FILE=CONF(I)%PRJF,EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open runfile:'//CHAR(13)// &
   TRIM(CONF(I)%PRJF),'Error')
  RETURN
 ENDIF

 IF(CODE.EQ.ID_INFO)THEN
  IWIN=0
  CALL WINDOWOPENCHILD(IWIN,FLAGS=SYSMENUON+MAXBUTTON,WIDTH=1000,HEIGHT=500)
  CALL WINDOWSELECT(IWIN)
  CALL WEDITFILE(CONF(I)%PRJF,ITYPE=MODAL,IDMENU=0,IFONT=COURIERNEW,ISIZE=10)
 ELSEIF(CODE.EQ.ID_CHECKRUN)THEN
  CALL MODEL1CHECKRUNFILE(CONF(I)%PRJF)
 ENDIF

 END SUBROUTINE STRES1RUNINFO

 !###======================================================================
 LOGICAL FUNCTION STRES1START()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IVERSION,NLAY,JU
 CHARACTER(LEN=256) :: DIR
 LOGICAL :: LEX
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: TOP,BOT,KHV,L,Q
 INTEGER,ALLOCATABLE,DIMENSION(:) :: LAYQ,IU
 
 STRES1START=.FALSE.

 IF(TRIM(PREFVAL(8)).EQ.'')THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to specify the keyword MODFLOW in the IMOD_INIT.PRF.','Error')
  RETURN
 ENDIF
 
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

 PBMAN%IFORMAT=2; PBMAN%ISOLVE=1; PBMAN%SSYSTEM=0; PBMAN%MINKD=0.0D0; PBMAN%MINC=0.0D0
 PBMAN%NSTEP=1; PBMAN%NMULT=1.0D0; PBMAN%ISTEADY=0; PBMAN%MODFLOW=TRIM(PREFVAL(8))
 PBMAN%ICONCHK=0; PBMAN%IPEST=0; PBMAN%ISAVEENDDATE=0; PBMAN%IPESTP=0
 PBMAN%ISS=1; PBMAN%IFVDL=0; PBMAN%BNDFILE=''; PBMAN%IPKS=0; PBMAN%ICHKCHD=0
 PBMAN%IDOUBLE=0; PBMAN%ICONSISTENCY=1; PBMAN%MINTHICKNESS=0.1D0
 PBMAN%INT=1

 !## save all heads
 ALLOCATE(PBMAN%ISAVE(TSHD)%ILAY(1)); PBMAN%ISAVE(TSHD)%ILAY(1)=-1
 ALLOCATE(PBMAN%UNCONFINED(1)); PBMAN%UNCONFINED(1)=0

 IOKAY=ILIST
 DO I=1,NSCNCONF
  !## current configuration selected to be computed
  IF(ILIST(I).EQ.1)THEN

   !## construct result map:
   DIR=SCFFNAME(:INDEX(SCFFNAME,'.',.TRUE.)-1)//'\V'//TRIM(ITOS(IVERSION))//'_'//TRIM(CONF(I)%PRJNAME)
   CALL UTL_CREATEDIR(DIR)

   !## read prjfile
   IF(.NOT.PMANAGERPRJ(ID_OPEN,CONF(I)%PRJF,0,0))THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading project file '//TRIM(CONF(I)%PRJF),'Error')
    EXIT
   ENDIF

   CALL UTL_MESSAGEHANDLE(0)

   !## get number of model layers
   CALL PMANAGER_GETNFILES((/TTOP,TBOT,TBND,TSHD,TKDW,TKHV,TKVA,TVCW,TKVV,TSTO,TSPY/),NLAY)

   !## get simbox
   IF(.NOT.STRES1SIMBOX())RETURN

   !## create ipf for wel-system
   ALLOCATE(TOP(NLAY),BOT(NLAY),L(NLAY),KHV(NLAY),Q(NLAY),LAYQ(NLAY),IU(NLAY+1))

   LEX=ST1CREATEIPF(NLAY,IU,TOP,BOT,L,KHV,Q,LAYQ,DIR)
   IF(LEX)LEX=ST1ADDWELLSTOPRJ(DIR,NLAY)
    
   CALL ST1CLOSEFILES(IU=IU); DEALLOCATE(IU,TOP,BOT,L,KHV,Q,LAYQ)
 
   !## add all other modifications here
   IF(.NOT.LEX)EXIT

   !## create timfile
   PBMAN%TIMFNAME=TRIM(DIR)//'\MODEL.TIM'
   JU=UTL_GETUNIT(); OPEN(JU,FILE=PBMAN%TIMFNAME,STATUS='UNKNOWN',ACTION='WRITE')
   DO J=0,SIMNPER
    WRITE(JU,'(A8,A8)') JDATETOGDATE(SIMJDATE(J),DTYPE=2),'000000,1'
   ENDDO
   CLOSE(JU)

   !## start simulation
   IF(PMANAGERPRJ(ID_SAVE   ,TRIM(DIR)//'\MODEL.PRJ',1,0))THEN; ENDIF

   CALL UTL_MESSAGEHANDLE(0)

   PBMAN%OUTPUT=TRIM(DIR); IF(PMANAGERRUN(ID_SAVERUN,TRIM(DIR)//'\MODEL.NAM',1))IOKAY(I)=0

   DO J=1,SIZE(TOPICS); CALL PMANAGER_DEALLOCATE(J); ENDDO; CALL PMANAGER_DEALLOCATE_PEST()

   !## unsuccessfull completed
   IF(IOKAY(I).NE.0)EXIT

  ENDIF
 END DO

 IF(SUM(IOKAY).EQ.0)STRES1START=.TRUE.

 !## compute drawdown
 IF(JLIST(2).EQ.1)CALL STRES1DRAWDOWN(DIR,NLAY)

 IF(STRES1START)CALL STRES1DEALLOCATE()
 CALL UTL_MESSAGEHANDLE(1)
 CALL WDIALOGSELECT(ID_DSCENTOOL_RESULTS)
 
 END FUNCTION STRES1START

 !###======================================================================
 SUBROUTINE STRES1DRAWDOWN(DIR,NLAY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NLAY
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: IPER,ILAY,I

 CALL UTL_CREATEDIR(TRIM(DIR)//'\DRAWDOWN')

 ALLOCATE(DDNIDF(2)); DO I=1,SIZE(DDNIDF); CALL IDFNULLIFY(DDNIDF(I)); ENDDO

 DO ILAY=1,NLAY
  !## open
  IF(.NOT.IDFREAD(DDNIDF(1),TRIM(DIR)//'\HEAD\HEAD_'//TRIM(ITOS(UTL_JDATETOIDATE(SIMJDATE(0))))// &
           '_L'//TRIM(ITOS(ILAY))//'.IDF',1))RETURN
  DO IPER=1,SIMNPER-1
   IF(.NOT.IDFREAD(DDNIDF(2),TRIM(DIR)//'\HEAD\HEAD_'//TRIM(ITOS(UTL_JDATETOIDATE(SIMJDATE(IPER))))// &
            '_L'//TRIM(ITOS(ILAY))//'.IDF',1))RETURN
   DDNIDF(2)%X=DDNIDF(2)%X-DDNIDF(1)%X
   IF(.NOT.IDFWRITE(DDNIDF(2),TRIM(DIR)//'\DRAWDOWN\DRAWDOWN_'//TRIM(ITOS(UTL_JDATETOIDATE(SIMJDATE(IPER))))// &
            '_L'//TRIM(ITOS(ILAY))//'.IDF',1))RETURN  !## no question
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
 
 END SUBROUTINE STRES1DEALLOCATE
 
 !###======================================================================
 LOGICAL FUNCTION STRES1SIMTIME()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,ID,TWEL,TOBS,IFIN
 REAL(KIND=DP_KIND) :: SUMQ

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
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot determine duration of simulation.'//CHAR(13)// &
   'Probably you do not have entered any wells strenghts.','Error')
  RETURN
 ENDIF

 IF(ALLOCATED(LID))DEALLOCATE(LID)
 ALLOCATE(LID(MAXDATE-MINDATE+1+IFIN))
 LID=0
 !## last timestep is infinite timestep
 IF(IFIN.EQ.1)LID(IFIN)=1
 
 !## fill in all dates
 SUMQ=0.0D0
 DO I=1,NWEL
  DO J=1,WEL(I)%NQ
   ID     =WEL(I)%Q(J)%IDATE-MINDATE+1
   LID(ID)=1
   !## do not enter the last one!
   IF(J.LT.WEL(I)%NQ)SUMQ=SUMQ+ABS(WEL(I)%Q(J)%QRATE)
  END DO
 ENDDO

 IF(SUMQ.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot determine any rates for wells and/or well systems.'//CHAR(13)// &
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

 !## always apply an initial steady-state solution
 SIMNPER=1
 DO I=1,SIZE(LID); IF(LID(I).EQ.1)SIMNPER=SIMNPER+1; END DO
 IF(ALLOCATED(SIMDELT)) DEALLOCATE(SIMDELT)
 IF(ALLOCATED(SIMJDATE))DEALLOCATE(SIMJDATE)
 ALLOCATE(SIMDELT(SIMNPER),SIMJDATE(0:SIMNPER))
 SIMNPER=1
 !## initial steady-state solution
 SIMDELT(SIMNPER)=0
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
 IF(IFIN.EQ.1)SIMDELT(SIZE(SIMDELT))=365.0D0 
 
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
 LOGICAL FUNCTION STRES1SIMBOX()
 !###======================================================================
 IMPLICIT NONE

 STRES1SIMBOX=.FALSE.

 PBMAN%XMIN= 10.0D10 !## xmin
 PBMAN%YMIN= 10.0D10 !## ymin
 PBMAN%XMAX=-10.0D10 !## xmax
 PBMAN%YMAX=-10.0D10 !## ymax
 
 PBMAN%IWINDOW=2; CALL ST1SIMBOXWELLS(PBMAN%XMIN,PBMAN%YMIN,PBMAN%XMAX,PBMAN%YMAX)

 !## nothing found
 IF(PBMAN%XMAX.LT.PBMAN%XMIN.OR.PBMAN%YMAX.LT.PBMAN%YMIN)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot determine size of the model!'//CHAR(13)// &
   'Probably something wrong in the coordinates of the individual wells.','Error')
  RETURN
 ENDIF

 !## read cellsize
 CALL WDIALOGSELECT(ID_DSCENTOOL_RESULTS)
 CALL WDIALOGGETDOUBLE(IDF_REAL1,PBMAN%CELLSIZE)
 CALL WDIALOGGETDOUBLE(IDF_REAL2,PBMAN%BUFFERCS)
 CALL WDIALOGGETDOUBLE(IDF_REAL3,PBMAN%BUFFER)
! PBMAN%XMIN=PBMAN%XMIN-PBMAN%BUFFER
! PBMAN%YMIN=PBMAN%YMIN-PBMAN%BUFFER
! PBMAN%XMAX=PBMAN%XMAX+PBMAN%BUFFER
! PBMAN%YMAX=PBMAN%YMAX+PBMAN%BUFFER

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
   IF(.NOT.UTL_DEL1TREE(SCFFNAME(:INDEX(SCFFNAME,'.',.TRUE.)-1)//'\'//TRIM(RES(I)%CNAME)))THEN
!## error occured
   ENDIF
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
 ILIST=0; ILIST(1)=1
 CALL WDIALOGPUTMENU(IDF_MENU1,CONF%PRJNAME,NSCNCONF,ILIST(1:NSCNCONF))
 CALL WDIALOGPUTMENU(IDF_MENU2,RESLIST,MXRES,ILIST(1:MXRES))
 DEALLOCATE(ILIST)

 CALL WDIALOGFIELDOPTIONS(IDF_REAL1,EDITFIELDCHANGED,1)
 CALL WDIALOGFIELDOPTIONS(IDF_REAL2,EDITFIELDCHANGED,1)
 CALL WDIALOGPUTDOUBLE(IDF_REAL1,25.0D0,'(F7.2)')
 CALL WDIALOGPUTDOUBLE(IDF_REAL2,25.0D0,'(F7.2)')
 CALL WDIALOGPUTDOUBLE(IDF_REAL3,1500.0D0,'(F7.2)')

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
 REAL(KIND=DP_KIND) :: X1,X2

 CALL WDIALOGUNDEFINED(DVALUE=NODATAGRID)
 CALL WDIALOGSELECT(ID_DSCENTOOL_RESULTS)

 !## test rest of fields to be filled in completely
 IACT=1
 CALL WDIALOGGETDOUBLE(IDF_REAL1,X1)
 IF(X1.LE.0.0D0)IACT=0
 !## cellsize
 CALL WDIALOGGETDOUBLE(IDF_REAL2,X2)
 IF(X2.LE.0.0D0)IACT=0
 !## cellsize in buffer should be larger than inside area of interest
 IF(X1.GT.X2)IACT=0

 !## extra scenario-buffersize
 CALL WDIALOGGETDOUBLE(IDF_REAL3,X1)
 IF(X1.LT.0.0D0)IACT=0

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