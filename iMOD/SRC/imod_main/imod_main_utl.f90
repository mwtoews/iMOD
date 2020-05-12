!!  Copyright (C) Stichting Deltares, 2005-2020.
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
MODULE MOD_MAIN_UTL

USE WINTERACTER
USE RESOURCE
USE MOD_UTL
USE MOD_IPF_PAR, ONLY : NLITHO,BH,GRAPHLINESSCALE,SCALED
USE BMPVAR
USE MOD_GENPLOT_PAR
USE MOD_PLUGIN_PAR
USE MOD_DEMO_PAR
USE MOD_POLYGON_UTL
USE MOD_IR_UTL, ONLY : IR1CLOSE
USE MOD_TOOLS_UTL, ONLY : TOOLS_CLOSE
USE MOD_MODEL_UTL, ONLY : MODEL1CLOSE
USE MOD_ISG_UTL, ONLY : ISGEDITCLOSE
USE MOD_PROFILE_UTL, ONLY : PROFILE_CLOSE
USE MOD_IDFEDIT_UTL, ONLY : IDFEDITCLOSE
USE MOD_CREATE_UTL, ONLY : CREATEGEN1CLOSE,CREATEIDF1CLOSE,CREATEIPF1CLOSE
USE MOD_SPOINTS_UTL, ONLY : STARTP1_UTL_CLOSE
USE MOD_EXTRACTIPF_UTL, ONLY : EXTRACTIPF1CLOSE
USE MOD_IDFTIMESERIE_UTL, ONLY : IDFTIMESERIE_CLOSE
USE MOD_IPFGETVALUE_UTL, ONLY : IPFGETVALUE_CLOSE
USE MOD_MANAGER_UTL, ONLY : MANAGER_UTL_CLOSE
USE MOD_SOLID_UTL, ONLY : SOLID_CLOSE
USE MOD_PLUGIN 
USE MOD_GENPLOT
USE MOD_SCENTOOL_UTL, ONLY : ST1CLOSE
USE MOD_MSPINSPECTOR_UTL, ONLY : MSPINSPECTOR_CLOSE
USE MOD_UZFANALYSER_UTL, ONLY : UZFANALYSER_CLOSE

CONTAINS

 !###======================================================================
 SUBROUTINE MAIN_UTL_INACTMODULE(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID

 IF(ID.NE.ID_RUNMODEL.AND.WMENUGETSTATE(ID_RUNMODEL,2).EQ.1)          CALL MODEL1CLOSE()
 IF(ID.NE.ID_IRDATABASE.AND.WMENUGETSTATE(ID_IRDATABASE,2).EQ.1)      CALL IR1CLOSE(1)
 IF(ID.NE.ID_IDFEDIT.AND.WMENUGETSTATE(ID_IDFEDIT,2).EQ.1)            CALL IDFEDITCLOSE()
 IF(ID.NE.ID_ISGEDIT.AND.WMENUGETSTATE(ID_ISGEDIT,2).EQ.1)            CALL ISGEDITCLOSE(1)
 IF(ID.NE.ID_SPOINTS.AND.WMENUGETSTATE(ID_SPOINTS,2).EQ.1)            CALL STARTP1_UTL_CLOSE(1)
 IF(ID.NE.ID_WBAL_GENERATE.AND.WMENUGETSTATE(ID_WBAL_GENERATE,2).EQ.1)CALL TOOLS_CLOSE()
 IF(ID.NE.ID_GXG.AND.WMENUGETSTATE(ID_GXG,2).EQ.1)                    CALL TOOLS_CLOSE()
 IF(ID.NE.ID_MEAN.AND.WMENUGETSTATE(ID_MEAN,2).EQ.1)                  CALL TOOLS_CLOSE()
 IF(ID.NE.ID_TS.AND.WMENUGETSTATE(ID_TS,2).EQ.1)                      CALL TOOLS_CLOSE()
 IF(ID.NE.ID_CREATEGEN.AND.WMENUGETSTATE(ID_CREATEGEN,2).EQ.1)        CALL CREATEGEN1CLOSE()
 IF(ID.NE.ID_CREATEIPF.AND.WMENUGETSTATE(ID_CREATEIPF,2).EQ.1)        CALL CREATEIPF1CLOSE()
 IF(ID.NE.ID_CREATEIDF_IPF.AND.WMENUGETSTATE(ID_CREATEIDF_IPF,2).EQ.1)CALL CREATEIDF1CLOSE()
 IF(ID.NE.ID_CREATEIDF_GEN.AND.WMENUGETSTATE(ID_CREATEIDF_GEN,2).EQ.1)CALL CREATEIDF1CLOSE()
 IF(ID.NE.ID_CREATEIDF_IFF.AND.WMENUGETSTATE(ID_CREATEIDF_IFF,2).EQ.1)CALL CREATEIDF1CLOSE()
 IF(ID.NE.ID_EXTRACTIPF.AND.WMENUGETSTATE(ID_EXTRACTIPF,2).EQ.1)      CALL EXTRACTIPF1CLOSE()
 IF(ID.NE.ID_PROFILE.AND.WMENUGETSTATE(ID_PROFILE,2).EQ.1)            CALL PROFILE_CLOSE()
 IF(ID.NE.ID_TIMESERIES.AND.WMENUGETSTATE(ID_TIMESERIES,2).EQ.1)      CALL IDFTIMESERIE_CLOSE()
 IF(ID.NE.ID_MANAGER.AND.WMENUGETSTATE(ID_MANAGER,2).EQ.1)            CALL MANAGER_UTL_CLOSE()
 IF(ID.NE.ID_ANALYSEIPF.AND.WMENUGETSTATE(ID_ANALYSEIPF,2).EQ.1)      CALL IPFGETVALUE_CLOSE()
 IF(ID.NE.ID_SCENTOOL.AND.WMENUGETSTATE(ID_SCENTOOL,2).EQ.1)          CALL ST1CLOSE(1)
 IF(ID.NE.ID_MSPANALYSER.AND.WMENUGETSTATE(ID_MSPANALYSER,2).EQ.1)    CALL MSPINSPECTOR_CLOSE()
 IF(ID.NE.ID_UZFANALYSER.AND.WMENUGETSTATE(ID_UZFANALYSER,2).EQ.1)    CALL UZFANALYSER_CLOSE()

 !## close only if not equal profile-tool/3d tool
 IF(ID.NE.ID_PROFILE.AND.&
    ID.NE.ID_3DTOOL.AND.&
    ID.NE.ID_SOLIDS.AND.WMENUGETSTATE(ID_SOLIDS,2).EQ.1)CALL SOLID_CLOSE()

 CALL MAIN_UTL_TIMERS()
!IF(IDIAGERROR.EQ.0)CALL WMENUSETSTATE(ID,2,1)

 END SUBROUTINE MAIN_UTL_INACTMODULE

 !###======================================================================
 SUBROUTINE MAIN_UTL_TIMERS()
 !###======================================================================
 IMPLICIT NONE
 
 !## (re)set AutoSave timer 
 CALL WMESSAGETIMER(60*1000,ID=1,IREPEAT=1)  !## 1 minute, autosave
 !## (re)set plugin timer
 CALL PLUGIN_SETTIMER()                

 END SUBROUTINE MAIN_UTL_TIMERS

!###======================================================================
SUBROUTINE MAIN_UTL_LOAD_SAVE_IMF(ID,IOKAY)
!###======================================================================
IMPLICIT NONE
INTEGER,INTENT(OUT) :: IOKAY
INTEGER,INTENT(IN) :: ID
INTEGER :: IU
CHARACTER(LEN=256) :: FNAME

IOKAY=0
IU=UTL_GETUNIT()

SELECT CASE (ID)

 CASE (ID_SAVE,ID_SAVEAS)

  IF(IMFFNAME.EQ.''.OR.ID.EQ.ID_SAVEAS)THEN
   FNAME=TRIM(PREFVAL(1))//'\IMFILES\*.imf'
   IF(.NOT.UTL_WSELECTFILE('iMOD Project (*.imf)|*.imf|',SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,&
        FNAME,'Select iMOD Project'))RETURN
   CALL WINDOWSELECT(MPW%IWIN)
   IMFFNAME=FNAME
   CALL WINDOWTITLE('Current iMOD Project: '//TRIM(IMFFNAME))
  ENDIF
  CALL MAIN_UTL_SAVE_IMF(IMFFNAME,1)
  CALL WINDOWSELECT(0)
  CALL WMENUSETSTATE(ID_SAVE,1,1)

 CASE(ID_OPEN)

  FNAME=TRIM(PREFVAL(1))//'\IMFILES\*.imf'
  IF(.NOT.UTL_WSELECTFILE('iMOD Project (*.imf)|*.imf|',LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT+MUSTEXIST,&
       FNAME,'Select iMOD Project'))RETURN
  IMFFNAME=FNAME

  CALL MAIN_UTL_LOAD_IMF()
  CALL MAIN_UTL_LOAD()

END SELECT

CLOSE(IU)
IOKAY=1

END SUBROUTINE MAIN_UTL_LOAD_SAVE_IMF

!###======================================================================
SUBROUTINE MAIN_UTL_LOAD_IMF()
!###======================================================================
IMPLICIT NONE
INTEGER,PARAMETER :: MAXMPWKEYS=21
INTEGER,PARAMETER :: MAXMPKEYS =36
INTEGER,PARAMETER :: MAXGENKEYS=10
INTEGER,PARAMETER :: MAXBMPKEYS=10
INTEGER,PARAMETER :: MAXPLKEYS=2
INTEGER,PARAMETER :: MAXDEMOKEYS=1
INTEGER :: IU,I,J,K,IOS,IVALUE,IPLOT,TI,N
REAL(KIND=DP_KIND) :: RVALUE
LOGICAL :: LEX,LVALUE
CHARACTER(LEN=10) :: STRING
CHARACTER(LEN=8) :: CKEY
CHARACTER(LEN=12) :: FRM
CHARACTER(LEN=300) :: LINE
CHARACTER(LEN=256) :: CVALUE
CHARACTER(LEN=8),DIMENSION(MAXMPWKEYS) :: MPWKEYS
CHARACTER(LEN=8),DIMENSION(MAXMPKEYS)  :: MPKEYS
CHARACTER(LEN=8),DIMENSION(MAXGENKEYS) :: GENKEYS
CHARACTER(LEN=8),DIMENSION(MAXBMPKEYS) :: BMPKEYS
CHARACTER(LEN=8),DIMENSION(MAXPLKEYS) :: PLKEYS
CHARACTER(LEN=8),DIMENSION(MAXDEMOKEYS) :: DEMOKEYS
DATA MPWKEYS/'NACT=','XMIN=','XMAX=','YMIN=','YMAX=','ITRANSP=','IASAVE=','SCLBRX1=','SCLBRY1=','SCLBRX2=','SCLBRY2=', &
            'AXESX1=','AXESY1=','AXESX2=','AXESY2=','LEGEX1=','LEGEY1=','LEGEX2=','LEGEY2=','ITRBITMAP=','ICTONE='/
DATA MPKEYS/'IDFNAME=','ALIAS=','ISEL=','IDFI=','IEQ=','IDFKIND=','SYMBOL=',      &    !1-7
            'FADEOUT=','UNITS=','SCOLOR=','PRFTYPE=','NCLR=','THICKNS=','XCOL=',  &    !8-14
            'YCOL=','ZCOL=','HCOL=','IAXES=','ILEG=','Z2COL=','IATTRIB=','NLIDF=',&    !15-22
            'TSIZE=','ISCREEN=','ASSCOL1=','ASSCOL2=','ILEG=','ILEGDLF=','GLSCALE=',&  !23-29
            'SCALED=','BORECK1=','BORECK2=','BORECLS=','HCOL_M=','IARROW=','TFORMAT='/ !30-36
DATA GENKEYS/'GENNAME=','ISEL=','ITYPE=','SYMBOL=','THICKNS=','RGB=','ILABELS=','IFILL=','TXTSIZE=','TFORMAT='/
DATA BMPKEYS/'BMPNAME=','ITYPE=','NCOL=','NROW=','XMIN=','YMIN=','XMAX=','YMAX=','DX=','DY='/
DATA PLKEYS/'PLUGIN1=','PLUGIN2='/
DATA DEMOKEYS/'TOPIC='/

INQUIRE(FILE=IMFFNAME,EXIST=LEX)
IF(.NOT.LEX)THEN
 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot find '//TRIM(IMFFNAME),'Error')
 RETURN
ENDIF

CALL WINDOWSELECT(0)

MP%IACT =.FALSE.
MP%ISEL =.FALSE.
GEN%IACT=.FALSE.
GEN%ISEL=.FALSE.
BMP%IACT =0

INQUIRE(FILE=IMFFNAME,UNFORMATTED=STRING)
IF(TRIM(STRING).EQ.'YES')THEN
 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'This version of iMOD cannot read old version of IMF files (v1.10) ','Error')
 RETURN
ENDIF

IU=UTL_GETUNIT()
CALL OSD_OPEN(IU,FILE=IMFFNAME,STATUS='OLD',FORM='FORMATTED',ACTION='READ,DENYWRITE')
READ(IU,*,IOSTAT=IOS) ; IF(IOS.NE.0)RETURN
READ(IU,*,IOSTAT=IOS) ; IF(IOS.NE.0)RETURN

!## read header
DO
 READ(IU,'(A256)',IOSTAT=IOS) LINE
 IF(IOS.NE.0)EXIT

 !## stop header
 IF(LINE(1:10).EQ.'==========')EXIT
 READ(LINE,'(A8)') CKEY
 CKEY=UTL_CAP(CKEY,'U')
 !## find keyword
 DO I=1,MAXMPWKEYS
  IF(TRIM(CKEY).EQ.MPWKEYS(I))EXIT
 END DO
 IF(I.LE.MAXMPWKEYS)THEN
  SELECT CASE (I)
   !## integer
   CASE (1,6,7,20,21)
    READ(LINE,'(8X,I10)')   IVALUE
   !## real
   CASE (2:5,8:19)
    READ(LINE,'(8X,F15.0)') RVALUE
  END SELECT
  SELECT CASE (I)
   CASE (1)
    MPW%NACT=IVALUE
   CASE (2)
    MPW%XMIN=RVALUE
    OFFSETX=MPW%XMIN
   CASE (3)
    MPW%XMAX=RVALUE
   CASE (4)
    MPW%YMIN=RVALUE
    OFFSETY=MPW%YMIN
   CASE (5)
    MPW%YMAX=RVALUE
   CASE (6) !## transparancy
    CALL WMENUSETSTATE(ID_TRANSPARANTIDF,2,IVALUE)
   CASE (7) !## autosave
    IF(IVALUE.EQ.0)THEN
     CALL WMENUSETSTATE(ID_AUTOSAVE,2,0)
     CALL WMENUSETSTRING(ID_AUTOSAVE,'AutoSave Off')
     CALL WMESSAGETIMER(0,1)
    ELSE
     CALL WMENUSETSTATE(ID_AUTOSAVE,2,1)
     CALL WMENUSETSTRING(ID_AUTOSAVE,'AutoSave On (1 minute)')
     CALL WMESSAGETIMER(60*1000,1,IREPEAT=1)  !minutes
    ENDIF
   CASE (8)
    SB_XP1=RVALUE
   CASE (9)
    SB_YP1=RVALUE
   CASE (10)
    SB_XP2=RVALUE
   CASE (11)
    SB_YP2=RVALUE
   CASE (12)
    AX_XP1=RVALUE
   CASE (13)
    AX_YP1=RVALUE
   CASE (14)
    AX_XP2=RVALUE
   CASE (15)
    AX_YP2=RVALUE
   CASE (16)
    LG_XP1=RVALUE
   CASE (17)
    LG_YP1=RVALUE
   CASE (18)
    LG_XP2=RVALUE
   CASE (19)
    LG_YP2=RVALUE
   CASE (20)
    MPW%ITRBITMAP=IVALUE
   CASE (21)
    MPW%ICTONE=IVALUE
  END SELECT
 ENDIF
END DO

DO IPLOT=1,MXMPLOT

 READ(IU,'(A256)',IOSTAT=IOS) LINE !reading ++++++-en
 IF(IOS.NE.0)EXIT

 !## continue with overlays
 IF(LINE(1:10).EQ.'//////////')EXIT

 !## read INFORMATION for each plot
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  !## stop INFORMATION for particular plot
  IF(LINE(1:10).EQ.'++++++++++')EXIT

  !## reading legend
  IF(LINE(1:10).EQ.'----------')THEN
   READ(IU,*)  !## legend
   READ(IU,*)  !## ----------

   !## try to read legend-header-text
   READ(IU,'(A256)') LINE
   READ(LINE,'(A8)') CKEY
   CKEY=UTL_CAP(CKEY,'U') 
   IF(TRIM(CKEY).EQ.'HEDTXT=')THEN
    READ(LINE,'(8X,A)') MP(IPLOT)%LEG%HEDTXT
    READ(IU,'(26X,F15.0)') MP(IPLOT)%LEG%CLASS(0)
   ELSE
    READ(LINE,'(26X,F15.0)') MP(IPLOT)%LEG%CLASS(0)
   ENDIF

   DO J=1,MP(IPLOT)%LEG%NCLR
    READ(IU,'(A256)',IOSTAT=IOS) LINE
    READ(LINE,'(8X,I10,8X,F15.0,8X,A32)',IOSTAT=IOS) MP(IPLOT)%LEG%RGB(J),MP(IPLOT)%LEG%CLASS(J),MP(IPLOT)%LEG%LEGTXT(J)
    !## old legend format
    IF(IOS.NE.0)READ(LINE,'(8X,I10,8X,F10.0,8X,A32)',IOSTAT=IOS) MP(IPLOT)%LEG%RGB(J),MP(IPLOT)%LEG%CLASS(J),MP(IPLOT)%LEG%LEGTXT(J)
   END DO
   K=0; DO J=1,MXCGRAD
    READ(IU,'(8X,2I10)',IOSTAT=IOS) MP(IPLOT)%LEG%CGRAD(J),MP(IPLOT)%LEG%ICLRGRAD(J)
    IF(IOS.NE.0)K=K+1
   END DO
   IF(K.NE.0)THEN
    DO I=1,MXCGRAD; MP(IPLOT)%LEG%ICLRGRAD(I)=MP(IPLOT)%LEG%RGB(CLRGIVEN(I)); ENDDO
   ENDIF
   
  ENDIF

  MP(IPLOT)%IACT=.TRUE.

  READ(LINE,'(A8)') CKEY
  CKEY=UTL_CAP(CKEY,'U')

  !## find keyword
  DO I=1,MAXMPKEYS
   IF(TRIM(CKEY).EQ.MPKEYS(I))EXIT
  END DO
  IF(I.LE.MAXMPKEYS)THEN
   SELECT CASE (I)
    !## character
    CASE (1:2,18,30,33,36)
     READ(LINE,'(8X,A256)')  CVALUE
    !## integer
    CASE (4:17,19:29,31:32,34,35)
     READ(LINE,'(8X,I10)')   IVALUE
    !## real
    CASE (0)
     READ(LINE,'(8X,F15.0)') RVALUE
    !## logical
    CASE (3)
     READ(LINE,'(8X,L10)')   LVALUE
    CASE DEFAULT
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD read unknown keyword '//TRIM(CKEY)//CHAR(13)//'This keyword is discarded.','Error')    
     CYCLE
   END SELECT
   SELECT CASE (I)
    CASE (1)
     CALL UTL_RELPATHNAME(IMFFNAME,CVALUE,MP(IPLOT)%IDFNAME)
    CASE (2)
     MP(IPLOT)%ALIAS=CVALUE
    CASE (3)
     MP(IPLOT)%ISEL=LVALUE
    CASE (4)
     MP(IPLOT)%IDFI=IVALUE
    CASE (5)
     MP(IPLOT)%IEQ=IVALUE
    CASE (6)
     MP(IPLOT)%IDFKIND=IVALUE
    CASE (7)
     MP(IPLOT)%SYMBOL=IVALUE
    CASE (8)
     MP(IPLOT)%FADEOUT=IVALUE
    CASE (9)
     MP(IPLOT)%UNITS=IVALUE
    CASE (10)
     MP(IPLOT)%SCOLOR=IVALUE
    CASE (11)
     MP(IPLOT)%PRFTYPE=IVALUE
    CASE (12)
     MP(IPLOT)%LEG%NCLR=IVALUE
    CASE (13)
     MP(IPLOT)%THICKNESS=IVALUE
    CASE (14)
     MP(IPLOT)%XCOL=IVALUE
    CASE (15)
     MP(IPLOT)%YCOL=IVALUE
    CASE (16)
     MP(IPLOT)%ZCOL=IVALUE
    CASE (17)
     MP(IPLOT)%HCOL=IVALUE
    CASE (18)
     !## backward compatibility - number of iaxes increased
     N=LEN_TRIM(CVALUE); WRITE(FRM,'(A1,I3.3,A3)') '(',N,'I1)'
     MP(IPLOT)%IAXES=1; READ(CVALUE,FRM) MP(IPLOT)%IAXES(1:N)  
     !## first two are always plotted on first y-axes
     MP(IPLOT)%IAXES(1:2)=1
     DO J=3,SIZE(MP(IPLOT)%IAXES)
      MP(IPLOT)%IAXES(J)=MAX(1,MP(IPLOT)%IAXES(J))
     END DO
    CASE (19)
     MP(IPLOT)%ILEG=IVALUE
    CASE (20)
     MP(IPLOT)%Z2COL=IVALUE
    CASE (21)
     MP(IPLOT)%IATTRIB=IVALUE
    CASE (22)
     MP(IPLOT)%NLIDF=IVALUE
    CASE (23)
     MP(IPLOT)%TSIZE=IVALUE
    CASE (24)
     MP(IPLOT)%ISCREEN=IVALUE     
    CASE (25)
     MP(IPLOT)%ASSCOL1=IVALUE     
    CASE (26)
     MP(IPLOT)%ASSCOL2=IVALUE     
    CASE (27)
     MP(IPLOT)%ILEG=IVALUE     
    CASE (28)
     MP(IPLOT)%ILEGDLF=IVALUE   
    CASE (29)
     GRAPHLINESSCALE=IVALUE
    CASE (30)
     !## backward compatibility - number of iaxes increased
     N=LEN_TRIM(CVALUE); N=N/4 
     WRITE(FRM,'(A1,I3.3,A5)') '(',N,'F4.1)'
     SCALED=1.0D0; READ(CVALUE,FRM) SCALED(1:N)
    CASE (31)
     MP(IPLOT)%GPERC1=IVALUE
    CASE (32)
     MP(IPLOT)%GPERC2=IVALUE
    CASE (33)
     N=LEN_TRIM(CVALUE);N=N/3; WRITE(FRM,'(A1,I3.3,A3)') '(',N,'I3)'
     MP(IPLOT)%ICPERC=1.0D0; READ(CVALUE,FRM) MP(IPLOT)%ICPERC  
    CASE (34)
     MP(IPLOT)%HCOL_METHOD=IVALUE
    CASE (35)
     MP(IPLOT)%IARROW=IVALUE
    CASE (36)
     MP(IPLOT)%TFORMAT=CVALUE
   END SELECT
  ENDIF
 ENDDO
END DO

DO IPLOT=1,MXGEN

 READ(IU,*,IOSTAT=IOS) LINE !reading ++++++-en
 IF(IOS.NE.0)EXIT

 !## continue with bmps
 IF(LINE(1:10).EQ.'[[[[[[[[[[')EXIT

 !## read INFORMATION for each plot
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  !## stop INFORMATION for particular plot
  IF(LINE(1:10).EQ.'++++++++++')EXIT

  GEN(IPLOT)%IACT=.TRUE.

  READ(LINE,'(A8)') CKEY
  CKEY=UTL_CAP(CKEY,'U') !IUPPERCASE(CKEY)

  !## find keyword
  DO I=1,MAXGENKEYS
   IF(TRIM(CKEY).EQ.GENKEYS(I))EXIT
  END DO
  IF(I.LE.MAXGENKEYS)THEN
   SELECT CASE (I)
    !## character
    CASE (1,10)
     READ(LINE,'(8X,A256)')  CVALUE
    !## integer
    CASE (3:9)
     READ(LINE,'(8X,I10)')   IVALUE
    !## real
    CASE (0)
     READ(LINE,'(8X,F10.2)') RVALUE
    !## logical
    CASE (2)
     READ(LINE,'(8X,L10)')   LVALUE
   END SELECT
   SELECT CASE (I)
    CASE (1)
     CALL UTL_RELPATHNAME(IMFFNAME,CVALUE,GEN(IPLOT)%GENFNAME)
    CASE (2)
     GEN(IPLOT)%ISEL=LVALUE
    CASE (3)
     GEN(IPLOT)%ITYPE=IVALUE
    CASE (4)
     GEN(IPLOT)%SYMBOL=IVALUE
    CASE (5)
     GEN(IPLOT)%THICKNESS=IVALUE
    CASE (6)
     GEN(IPLOT)%RGB=IVALUE
    CASE (7)
     GEN(IPLOT)%ILABELS=IVALUE
    CASE (8)
     GEN(IPLOT)%IFILL=IVALUE
    CASE (9)
     GEN(IPLOT)%TSIZE=IVALUE
    CASE (10)
     GEN(IPLOT)%TFORMAT=CVALUE
   END SELECT
  ENDIF
 ENDDO
END DO

NBMP=0
BMP%IACT=0
BMP%BMPFNAME=''
BMP%IBITMAP=0

DO IPLOT=1,MXBMP

 READ(IU,*,IOSTAT=IOS) LINE !reading ++++++-en
 IF(IOS.NE.0)EXIT
 
 !## continue with dlf legends
 IF(LINE(1:10).EQ.'$$$$$$$$$$')EXIT
 
 !## read INFORMATION for each plot
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  !## stop INFORMATION for particular plot
  IF(LINE(1:10).EQ.'++++++++++')EXIT

  BMP(IPLOT)%IACT=1

  READ(LINE,'(A8)') CKEY
  CKEY=UTL_CAP(CKEY,'U')

  !## find keyword
  DO I=1,MAXBMPKEYS
   IF(TRIM(CKEY).EQ.BMPKEYS(I))EXIT
  END DO
  IF(I.LE.MAXBMPKEYS)THEN
   SELECT CASE (I)
    !## character
    CASE (1)
     READ(LINE,'(8X,A256)')  CVALUE
    !## integer
    CASE (2:4)
     READ(LINE,'(8X,I10)')   IVALUE
    !## real
    CASE (5:9)
     READ(LINE,'(8X,F15.0)') RVALUE
    !## logical
    CASE (0)
     READ(LINE,'(8X,L10)')   LVALUE
   END SELECT
   SELECT CASE (I)
    CASE (1)
     CALL UTL_RELPATHNAME(IMFFNAME,CVALUE,BMP(IPLOT)%BMPFNAME)
    CASE (2)
     BMP(IPLOT)%ITYPE=IVALUE
    CASE (3)
     BMP(IPLOT)%NCOL=IVALUE
    CASE (4)
     BMP(IPLOT)%NROW=IVALUE
    CASE (5)
     BMP(IPLOT)%XMIN=RVALUE
    CASE (6)
     BMP(IPLOT)%YMIN=RVALUE
    CASE (7)
     BMP(IPLOT)%XMAX=RVALUE
    CASE (8)
     BMP(IPLOT)%YMAX=RVALUE
    CASE (9)
     BMP(IPLOT)%DX=RVALUE
    CASE (10)
     BMP(IPLOT)%DY=RVALUE
   END SELECT
  ENDIF
 ENDDO
 NBMP=NBMP+1
END DO

IF(NBMP.GT.0)THEN
 CALL WMENUSETSTATE(ID_TOPOGRAPHY,2,1)
ELSE
 CALL WMENUSETSTATE(ID_TOPOGRAPHY,2,0)
ENDIF

!DO IPLOT=1,SIZE(BMP)
! INQUIRE(FILE=BMP(IPLOT)%BMPFNAME,EXIST=LEX)
! IF(.NOT.LEX)THEN
!  BMP(IPLOT)%IACT=0
!  BMP(IPLOT)%BMPFNAME=''
!  BMP(IPLOT)%IBITMAP=0
! ENDIF
!ENDDO

DO I=1,SIZE(NLITHO)

 READ(IU,*,IOSTAT=IOS) LINE !reading ++++++-en
 IF(IOS.NE.0)EXIT

 !## continue with plugins
 IF(LINE(1:10).EQ.'**********')EXIT
 
 !## read INFORMATION for each dlf legend
 J=0
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  !## stop INFORMATION for particular dlf
  IF(LINE(1:10).EQ.'++++++++++')EXIT

  J=J+1
  READ(LINE,*) BH(I,J)%LITHOCLR,BH(I,J)%LITHOWIDTH,BH(I,J)%LITHO,BH(I,J)%LITHOTXT

 ENDDO
 NLITHO(I)=J

END DO

!## read in plug-ins from .imf file into Plugin-manager en Plugin-menu
DO
 READ(IU,'(A256)',IOSTAT=IOS) LINE !## reading ******-en
 IF(IOS.NE.0)EXIT
 
 IF(LINE(1:10).EQ.'**********')CYCLE
 IF(LINE(1:10).EQ.'DDDDDDDDDD')EXIT
 
 READ(LINE,'(3X,I2)') TI
 CALL WMENUSETSTATE(ID_TI1,2,0)
 CALL WMENUSETSTATE(ID_TI2,2,0)
 CALL WMENUSETSTATE(ID_TI3,2,0)
 CALL WMENUSETSTATE(ID_TI4,2,0)
 SELECT CASE (TI)
  CASE (60);    CALL WMENUSETSTATE(ID_TI1,2,1)
  CASE (30);    CALL WMENUSETSTATE(ID_TI2,2,1)
  CASE (15);    CALL WMENUSETSTATE(ID_TI3,2,1)
  CASE (1);     CALL WMENUSETSTATE(ID_TI4,2,1)
  CASE DEFAULT; CALL WMENUSETSTATE(ID_TI1,2,1)
 END SELECT

 DO 
  READ(LINE,'(A)') CKEY
  CKEY=UTL_CAP(CKEY,'U')

  DO I=1,MAXPLKEYS
   IF(TRIM(CKEY).EQ.PLKEYS(I))EXIT
  END DO

  IF(I.LE.MAXPLKEYS)THEN
    
   IF(I.EQ.1)THEN
    CALL PLUGIN_LOAD(PI1,27,LINE,IU)
   ELSE
    CALL PLUGIN_LOAD(PI2,28,LINE,IU)
   ENDIF

  ENDIF

  READ(IU,'(A256)',IOSTAT=IOS) LINE !reading ******-en
  IF(IOS.NE.0)EXIT
  IF(LINE(1:10).EQ.'**********')EXIT

 ENDDO
 
ENDDO

!## read demo keyword if available
DO
 READ(IU,'(A256)',IOSTAT=IOS) LINE !## reading ******-en
 IF(IOS.NE.0)EXIT
 
 IF(LINE(1:10).EQ.'DDDDDDDDDD')EXIT

 !## read demo keyword if available
 DO 
  READ(LINE,'(A)') CKEY
  CKEY=UTL_CAP(CKEY,'U')
   
  DO I=1,MAXDEMOKEYS
   IF(TRIM(CKEY).EQ.TRIM(DEMOKEYS(I)))EXIT
  END DO
   
  IF(I.LE.MAXDEMOKEYS)THEN
   READ(LINE,'(8X,A52)') DEMO%TDNAME
   DEMO%TDNAME=ADJUSTL(DEMO%TDNAME)
   
   IF(TRIM(UTL_CAP(DEMO%TDNAME,'U')).EQ.'CROSS')THEN   !## cross-section Demo
    
    DEMO%IDEMO=1
    READ(IU,*) DEMO%NXY
   
    IF(ASSOCIATED(DEMO%X))DEALLOCATE(DEMO%X); ALLOCATE(DEMO%X(DEMO%NXY))       
    IF(ASSOCIATED(DEMO%Y))DEALLOCATE(DEMO%Y); ALLOCATE(DEMO%Y(DEMO%NXY))       
    IF(ASSOCIATED(DEMO%L))DEALLOCATE(DEMO%L); ALLOCATE(DEMO%L(DEMO%NXY))       
   
    DO I=1,DEMO%NXY
     READ(IU,*,IOSTAT=IOS) DEMO%X(I),DEMO%Y(I),DEMO%L(I)
     IF(IOS.NE.0)EXIT
    ENDDO      
    
    READ(IU,'(8X,I1)') DEMO%IBLOCKLINES
    READ(IU,'(8X,I1)') DEMO%IBLOCKFILLS
    
   ELSEIF(TRIM(UTL_CAP(DEMO%TDNAME,'U')).EQ.'3D')THEN   !## 3D-tool Demo
    DEMO%IDEMO=2
    READ(IU,'(8X,I1)') DEMO%CONFLAG !## reads configuration type in which 3D-model needs to be plotted (1 t/m 7)
    READ(IU,'(8X,I1)') DEMO%IFILL   !## reads display type in which 3D-model needs to be plotted (1 t/m 3)
    READ(IU,'(8X,I1)') DEMO%ACCFLAG !## reads accuracy type (sets resolution of plotted 3D grid)
   ENDIF 
   
   READ(IU,'(8X,I1)') DEMO%ISAVEBMP

  ENDIF
 
  READ(IU,'(A256)',IOSTAT=IOS) LINE !## reading DDDDDDDDDDD-en
  IF(IOS.NE.0)EXIT
  IF(LINE(1:10).EQ.'DDDDDDDDDD')EXIT

 ENDDO
ENDDO

CLOSE(IU)

IF(PLUGIN_INITMENU_FILL())THEN; ENDIF
CALL PLUGIN_SETTIMER()

 END SUBROUTINE MAIN_UTL_LOAD_IMF
 
!###======================================================================
SUBROUTINE MAIN_UTL_LOAD()
!###======================================================================
IMPLICIT NONE
INTEGER :: I,J,IPLOT,JPLOT,II,JJ
LOGICAL :: LEX,L2
CHARACTER(LEN=1000) :: LINE

CALL WINDOWSELECT(MPW%IWIN)
PLOTNAME='iMOD-Map Configuration : '//TRIM(IMFFNAME)
CALL WINDOWTITLE(PLOTNAME)

!## overrule scherms settings
!## root window - size for the bitmap
MPW%DIX    =WINFOWINDOW(WINDOWWIDTH) 
MPW%DIY    =WINFOWINDOW(WINDOWHEIGHT)

!## compute start pixels such that mid of plot is in screen centre
MPW%IX=(MPW%DIX/2)-(WINFOWINDOW(WINDOWWIDTH)/2)
MPW%IY=(MPW%DIY/2)-(WINFOWINDOW(WINDOWHEIGHT)/2)

MPW%IBITMAP=0

LINE='LIST OF MISSING FILES IS:'//TRIM(NEWLINE)
L2=.TRUE.

!## update statistics
IPLOT=0
J    =0
DO JPLOT=1,MXMPLOT
 IF(MP(JPLOT)%IACT)THEN
  INQUIRE(FILE=MP(JPLOT)%IDFNAME,EXIST=LEX)
  IF(LEX)THEN

   IPLOT=IPLOT+1
   IF(IPLOT.NE.JPLOT)THEN
    MP(IPLOT)=MP(JPLOT)
    MP(JPLOT)%IACT=.FALSE.
    MP(JPLOT)%ISEL=.FALSE.
   ENDIF

   I=INDEXNOCASE(MP(IPLOT)%IDFNAME,'.',.TRUE.)+1
   MP(IPLOT)%IDFNAME=UTL_CAP(MP(IPLOT)%IDFNAME,'U')
   IF(MP(IPLOT)%IDFNAME(I:I+2).EQ.'IDF')MP(IPLOT)%IPLOT=1 !## IDF
   IF(MP(IPLOT)%IDFNAME(I:I+2).EQ.'IPF')MP(IPLOT)%IPLOT=2 !## IPF
   IF(MP(IPLOT)%IDFNAME(I:I+2).EQ.'IFF')MP(IPLOT)%IPLOT=3 !## IFF
   IF(MP(IPLOT)%IDFNAME(I:I+2).EQ.'ISG')MP(IPLOT)%IPLOT=4 !## ISG
   IF(MP(IPLOT)%IDFNAME(I:I+2).EQ.'MDF')MP(IPLOT)%IPLOT=5 !## MDF
   IF(MP(IPLOT)%IDFNAME(I:I+2).EQ.'GEN')MP(IPLOT)%IPLOT=6 !## GEN
   IF(MP(IPLOT)%IDFNAME(I:I+2).EQ.'UDF')MP(IPLOT)%IPLOT=7 !## UDF
   MP(IPLOT)%ISCREEN=MAX(MP(IPLOT)%ISCREEN,0)   !## nul is all screens (handy for IPF files)
   SELECT CASE (MP(IPLOT)%IPLOT)
    CASE (1)
     IF(.NOT.IDFREAD(MP(IPLOT)%IDF,MP(IPLOT)%IDFNAME,0))MP(IPLOT)%IPLOT=0
     IF(MP(IPLOT)%IDF%IU.GT.0)CLOSE(MP(IPLOT)%IDF%IU)
     IF(MP(IPLOT)%IDFKIND.EQ.0)CALL UTL_READARRAY((/1,0,0/),3,MP(IPLOT)%IDFKIND)
    !## ipf
    CASE (2)
     IF(MP(IPLOT)%PRFTYPE.LT.0)MP(IPLOT)%PRFTYPE=0  !non-active
     IF(MP(IPLOT)%PRFTYPE.GT.0)MP(IPLOT)%PRFTYPE=1  !active
     MP(IPLOT)%IDFKIND=0                       !type of plotting for associate file
     MP(IPLOT)%IDFI=MAX(0,MP(IPLOT)%IDFI)      !sight (m)
     MP(IPLOT)%SCOLOR=MAX(0,MP(IPLOT)%SCOLOR)  !no colouring, attribute colouring
     MP(IPLOT)%TSIZE =MAX(1,MP(IPLOT)%TSIZE) !## size of legend in nbetween 1 and 10
     IF(TRIM(MP(IPLOT)%TFORMAT).EQ.'')MP(IPLOT)%TFORMAT='F10.2' !## size of legend in nbetween 1 and 10
     MP(IPLOT)%ILEGDLF=MAX(1,MIN(10,MP(IPLOT)%ILEGDLF)) !## number of legend in between 1 and 10
     IF(MP(IPLOT)%ASSCOL1.LE.0)MP(IPLOT)%ASSCOL1=2 !## borehole plotting
     MP(IPLOT)%ASSCOL2=0 !## borehole plotting
     MP(IPLOT)%PCOL=0                           !column for plotting ipf labels
    !## iff
    CASE (3)
     IF(MP(IPLOT)%PRFTYPE.LT.0)MP(IPLOT)%PRFTYPE=0
     IF(MP(IPLOT)%PRFTYPE.GT.0)MP(IPLOT)%PRFTYPE=1
     MP(IPLOT)%IDFI=MAX(0,MP(IPLOT)%IDFI)      !sight (m)
     MP(IPLOT)%SCOLOR=MAX(0,MP(IPLOT)%SCOLOR)  !no colouring, attribute colouring
     MP(IPLOT)%IDFKIND=0    !nog vrij te gebruiken
     IF(MP(IPLOT)%IEQ.LT.0)MP(IPLOT)%IEQ=0     !no value plotted
     MP(IPLOT)%UNITS  =0    !nog vrij te gebruiken
    !## isg
    CASE (4)
    !## mdf
    CASE (5)
     MP(IPLOT)%NLIDF=MAX(1,MP(IPLOT)%NLIDF) !## default take the first to be plotted
    !## gen
    CASE (6)
     MP(IPLOT)%TSIZE=MIN(MAX(MP(IPLOT)%TSIZE,1),25)
     IF(TRIM(MP(IPLOT)%TFORMAT).EQ.'')MP(IPLOT)%TFORMAT='F10.2' !## size of legend in nbetween 1 and 10
   END SELECT
  ELSE
   J=J+1
   MP(JPLOT)%IACT=.FALSE.
   MP(JPLOT)%ISEL=.FALSE.
   IF(L2)THEN
    II=LEN_TRIM(LINE)
    JJ=LEN_TRIM(MP(JPLOT)%IDFNAME)
    IF(II+JJ.GT.LEN(LINE)-20)THEN
     LINE=TRIM(LINE)//TRIM(NEWLINE)//'... and more ...'; L2=.FALSE.
    ELSE
     LINE=TRIM(LINE)//TRIM(NEWLINE)//TRIM(MP(JPLOT)%IDFNAME)
    ENDIF
   ENDIF
  ENDIF
 ENDIF
ENDDO

IF(J.GT.0)CALL WMESSAGEBOX(OKONLY,COMMONOK,INFORMATIONICON,'iMOD removed '//TRIM(ITOS(J))// &
          ' files from the iMOD-manager that did not exist anymore'//NEWLINE//TRIM(LINE),'Info')

CALL MANAGER_UTL_FILL()
CALL MANAGER_UTL_UPDATE()
CALL GEN_FILL()
CALL GEN_UPDATE()
CALL MANAGER_UTL_SHOW()

CALL WINDOWSELECT(0)
CALL WMENUSETSTATE(ID_SAVE,1,1)

END SUBROUTINE MAIN_UTL_LOAD

 !###======================================================================
 SUBROUTINE MAIN_UTL_SAVE_IMF(FNAME,IERROR)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(IN) :: IERROR
 CHARACTER(LEN=256) :: LINE
 INTEGER :: IU,I,J,K,IOS,TI,N
 CHARACTER(LEN=12) :: FRM

CALL WINDOWSELECT(0)

IU=UTL_GETUNIT()
OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE',IOSTAT=IOS)
IF(IOS.NE.0)THEN
 IF(IERROR.EQ.1)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot write to file:'//CHAR(13)//TRIM(FNAME),'Warning')
 RETURN
ENDIF

WRITE(IU,'(A/)') 'IMOD META-FILE '//TRIM(UTL_IMODVERSION())

WRITE(IU,'(A8,I10)')   'NACT=   ',MPW%NACT
WRITE(IU,'(A8,F15.3)') 'XMIN=   ',MPW%XMIN
WRITE(IU,'(A8,F15.3)') 'XMAX=   ',MPW%XMAX
WRITE(IU,'(A8,F15.3)') 'YMIN=   ',MPW%YMIN
WRITE(IU,'(A8,F15.3)') 'YMAX=   ',MPW%YMAX
WRITE(IU,'(A8,I10)')   'ITRANSP=',WMENUGETSTATE(ID_TRANSPARANTIDF,2)
WRITE(IU,'(A8,I10)')   'IASAVE= ',WMENUGETSTATE(ID_AUTOSAVE,2)
WRITE(IU,'(A8,I10)')   'ITRBITM=',MPW%ITRBITMAP
WRITE(IU,'(A8,I10)')   'ICTONE= ',MPW%ICTONE
WRITE(IU,'(A8,E15.7)') 'SCLBRX1=',SB_XP1
WRITE(IU,'(A8,E15.7)') 'SCLBRY1=',SB_YP1
WRITE(IU,'(A8,E15.7)') 'SCLBRX2=',SB_XP2
WRITE(IU,'(A8,E15.7)') 'SCLBRY2=',SB_YP2
WRITE(IU,'(A8,E15.7)') 'AXESX1= ',AX_XP1
WRITE(IU,'(A8,E15.7)') 'AXESY1= ',AX_YP1
WRITE(IU,'(A8,E15.7)') 'AXESX2= ',AX_XP2
WRITE(IU,'(A8,E15.7)') 'AXESY2= ',AX_YP2
WRITE(IU,'(A8,E15.7)') 'LEGEX1= ',LG_XP1
WRITE(IU,'(A8,E15.7)') 'LEGEY1= ',LG_YP1
WRITE(IU,'(A8,E15.7)') 'LEGEX2= ',LG_XP2
WRITE(IU,'(A8,E15.7)') 'LEGEY2= ',LG_YP2

WRITE(IU,'(50A1)') ('=',I=1,50)

DO I=1,MXMPLOT
 IF(MP(I)%IACT)THEN
  WRITE(IU,'(50A1)') ('+',K=1,50)
  WRITE(IU,'(A8,A)')   'IDFNAME=',TRIM(MP(I)%IDFNAME)
  WRITE(IU,'(A8,A)')   'ALIAS=  ',TRIM(MP(I)%ALIAS)
  WRITE(IU,'(A8,L10)') 'ISEL=   ',MP(I)%ISEL
  WRITE(IU,'(A8,I10)') 'SCOLOR= ',MP(I)%SCOLOR
  WRITE(IU,'(A8,I10)') 'THICKNS=',MP(I)%THICKNESS
  !## ipf/iff-settings
  IF(MP(I)%IPLOT.EQ.2.OR.MP(I)%IPLOT.EQ.3.OR.MP(I)%IPLOT.EQ.6)THEN
   WRITE(IU,'(A8,I10)') 'ILEG=   ',MP(I)%ILEG
  ENDIF
  !## ipf-settings
  IF(MP(I)%IPLOT.EQ.2)THEN
   WRITE(IU,'(A8,I10)')  'XCOL=   ',MP(I)%XCOL
   WRITE(IU,'(A8,I10)')  'YCOL=   ',MP(I)%YCOL
   WRITE(IU,'(A8,I10)')  'ZCOL=   ',MP(I)%ZCOL
   WRITE(IU,'(A8,I10)')  'Z2COL=  ',MP(I)%Z2COL
   WRITE(IU,'(A8,I10)')  'HCOL=   ',MP(I)%HCOL
   WRITE(IU,'(A8,I10)')  'HCOL_M= ',MP(I)%HCOL_METHOD
   N=SIZE(MP(I)%IAXES)
   WRITE(FRM,'(A4,I3.3,A3)') '(A8,',N,'I1)'
   WRITE(IU,FRM) 'IAXES=  ',MP(I)%IAXES
   WRITE(IU,'(A8,I10)')  'TSIZE=  ',MP(I)%TSIZE
   WRITE(IU,'(A8,A12)')  'TFORMAT=',MP(I)%TFORMAT
   WRITE(IU,'(A8,I10)')  'ASSCOL1=',MP(I)%ASSCOL1
   WRITE(IU,'(A8,I10)')  'ASSCOL2=',MP(I)%ASSCOL2
   WRITE(IU,'(A8,I10)')  'ILEGDLF=',MP(I)%ILEGDLF
   WRITE(IU,'(A8,I10)')  'GLSCALE=',GRAPHLINESSCALE
   N=SIZE(SCALED)
   WRITE(FRM,'(A4,I3.3,A5)') '(A8,',N,'F4.1)'
   WRITE(IU,FRM) 'SCALED= ',SCALED(1:N)  
   WRITE(IU,'(A8,I10)')  'BORECK1=',MP(I)%GPERC1
   WRITE(IU,'(A8,I10)')  'BORECK2=',MP(I)%GPERC2
   N=SIZE(MP(I)%ICPERC)
   WRITE(FRM,'(A4,I3.3,A3)') '(A8,',N,'I3)'
   WRITE(IU,FRM)  'BORECLS=',MP(I)%ICPERC
  ENDIF
  !## gen-settings
  IF(MP(I)%IPLOT.EQ.6)THEN
   WRITE(IU,'(A8,I10)')  'TSIZE=  ',MP(I)%TSIZE
   WRITE(IU,'(A8,A12)')  'TFORMAT=',MP(I)%TFORMAT
  ENDIF
  !## iff arrows
  IF(MP(I)%IPLOT.EQ.6)THEN
   WRITE(IU,'(A8,10I1)') 'IARROW= ',MP(I)%IARROW
  ENDIF
  !## mdf-settings
  IF(MP(I)%IPLOT.EQ.5)THEN
   WRITE(IU,'(A8,10I1)') 'NLIDF=  ',MP(I)%NLIDF
  ENDIF
  IF(MP(I)%IPLOT.NE.4)THEN !## ne isg
   WRITE(IU,'(A8,I10)') 'IATTRIB=',MP(I)%IATTRIB
   WRITE(IU,'(A8,I10)') 'IDFI=   ',MP(I)%IDFI
   WRITE(IU,'(A8,I10)') 'IEQ=    ',MP(I)%IEQ
   WRITE(IU,'(A8,I10)') 'IDFKIND=',MP(I)%IDFKIND
   WRITE(IU,'(A8,I10)') 'SYMBOL= ',MP(I)%SYMBOL
   WRITE(IU,'(A8,I10)') 'FADEOUT=',MP(I)%FADEOUT
   WRITE(IU,'(A8,I10)') 'UNITS=  ',MP(I)%UNITS
   WRITE(IU,'(A8,I10)') 'PRFTYPE=',MP(I)%PRFTYPE
   WRITE(IU,'(A8,I10)') 'ISCREEN=',MP(I)%ISCREEN
  ENDIF
  WRITE(IU,'(A8,I10)') 'ILEG=   ',MP(I)%ILEG
  WRITE(IU,'(A8,I10)') 'NCLR=   ',MP(I)%LEG%NCLR
  WRITE(IU,'(50A1)') ('-',K=1,50)
  WRITE(IU,'(A)') 'LEGEND DEFINITION'
  WRITE(IU,'(50A1)') ('-',K=1,50)
  WRITE(IU,'(A8,A)') 'HEDTXT= ',TRIM(MP(I)%LEG%HEDTXT)
  WRITE(IU,'(18X,A8,G15.7)') 'CLASS=  ',MP(I)%LEG%CLASS(0)
  DO J=1,MP(I)%LEG%NCLR
   WRITE(IU,'(A8,I10,A8,G15.7,A8,A)') 'RGB=    ',MP(I)%LEG%RGB(J),'CLASS=  ',MP(I)%LEG%CLASS(J),'LEGTXT= ',TRIM(MP(I)%LEG%LEGTXT(J))
  END DO
  DO J=1,MXCGRAD
   WRITE(IU,'(A8,2I10)') 'CGRAD=  ',MP(I)%LEG%CGRAD(J),MP(I)%LEG%ICLRGRAD(J)
  END DO
  WRITE(IU,'(50A1)') ('+',K=1,50)
 ENDIF
END DO

WRITE(IU,'(50A1)') ('/',K=1,50)

DO I=1,MXGEN
 IF(GEN(I)%IACT)THEN
  WRITE(IU,'(50A1)') ('+',K=1,50)
  WRITE(IU,'(A8,A)')   'GENNAME=',TRIM(GEN(I)%GENFNAME)
  WRITE(IU,'(A8,L10)') 'ISEL=   ',GEN(I)%ISEL
  WRITE(IU,'(A8,I10)') 'ITYPE=  ',GEN(I)%ITYPE
  WRITE(IU,'(A8,I10)') 'SYMBOL= ',GEN(I)%SYMBOL
  WRITE(IU,'(A8,I10)') 'THICKNS=',GEN(I)%THICKNESS
  WRITE(IU,'(A8,I10)') 'ILABELS=',GEN(I)%ILABELS
  WRITE(IU,'(A8,I10)') 'IFILL='  ,GEN(I)%IFILL
  WRITE(IU,'(A8,I10)') 'TXTSIZE=',GEN(I)%TSIZE
  WRITE(IU,'(A8,A12)') 'TFORMAT=',GEN(I)%TFORMAT
  WRITE(IU,'(A8,I10)') 'RGB=    ',GEN(I)%RGB
  WRITE(IU,'(50A1)') ('+',K=1,50)
 ENDIF
ENDDO

WRITE(IU,'(50A1)') ('[',K=1,50)

DO I=1,MXBMP
 IF(BMP(I)%IACT.EQ.1)THEN
  WRITE(IU,'(50A1)') ('+',K=1,50)
  WRITE(IU,'(A8,A)')     'BMPNAME=',TRIM(BMP(I)%BMPFNAME)
  WRITE(IU,'(A8,I10)')   'ITYPE=  ',BMP(I)%ITYPE
  WRITE(IU,'(A8,I10)')   'NCOL=   ',BMP(I)%NCOL
  WRITE(IU,'(A8,I10)')   'NROW=   ',BMP(I)%NROW
  WRITE(IU,'(A8,F15.3)') 'XMIN=   ',BMP(I)%XMIN
  WRITE(IU,'(A8,F15.3)') 'YMIN=   ',BMP(I)%YMIN
  WRITE(IU,'(A8,F15.3)') 'XMAX=   ',BMP(I)%XMAX
  WRITE(IU,'(A8,F15.3)') 'YMAX=   ',BMP(I)%YMAX
  WRITE(IU,'(A8,F15.3)') 'DX=     ',BMP(I)%DX
  WRITE(IU,'(A8,F15.3)') 'DY=     ',BMP(I)%DY
  WRITE(IU,'(50A1)') ('+',K=1,50)
 ENDIF
ENDDO

!## write dlf legends

WRITE(IU,'(50A1)') ('$',K=1,50)

DO I=1,SIZE(NLITHO)
 WRITE(IU,'(50A1)') ('+',K=1,50)
 DO J=1,NLITHO(I)
  LINE=TRIM(ITOS(BH(I,J)%LITHOCLR))//','//TRIM(RTOS(BH(I,J)%LITHOWIDTH,'F',2))//',"'//TRIM(BH(I,J)%LITHO)//'","'//TRIM(BH(I,J)%LITHOTXT)//'"'
  WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
 END DO
 WRITE(IU,'(50A1)') ('+',K=1,50)
ENDDO

!## save settings for plugins
WRITE(IU,'(50A1)') ('*',K=1,50)

CALL WINDOWSELECT(0)
IF(WMENUGETSTATE(ID_TI1,2).EQ.1)TI=60
IF(WMENUGETSTATE(ID_TI2,2).EQ.1)TI=30
IF(WMENUGETSTATE(ID_TI3,2).EQ.1)TI=15
IF(WMENUGETSTATE(ID_TI4,2).EQ.1)TI=1

IF(SIZE(PI1).GE.1)THEN !If at least 1 plugin available call to plugin is performed
 WRITE(IU,'(A3,I2)') 'TI=',TI !chosen and saved check interval (in seconds) for plugin when running
 WRITE(IU,'(A8,I2)') 'PLUGIN1=',SIZE(PI1)
 WRITE(IU,'(A)') TRIM(PREFVAL(27))
 DO J=1,SIZE(PI1,1)
  WRITE(IU,'(A,I1)') '"'//TRIM(PI1(J)%PNAME)//'",',PI1(J)%IACT
 ENDDO
 WRITE(IU,'(50A1)') ('*',K=1,50)
ENDIF

WRITE(IU,'(50A1)') ('*',K=1,50)

!## if at least 1 plugin available call to plugin is performed
IF(SIZE(PI2).GE.1)THEN 
 WRITE(IU,'(A3,I2)') 'TI=',TI !chosen and saved check interval (in seconds) for plugin when running
 WRITE(IU,'(A8,I2)') 'PLUGIN2=',SIZE(PI2)
 WRITE(IU,'(A)') TRIM(PREFVAL(28))
 DO J=1,SIZE(PI2,1)
  WRITE(IU,'(A,I1)') '"'//TRIM(PI2(J)%PNAME)//'",',(PI2(J)%IACT)
 ENDDO
 WRITE(IU,'(50A1)') ('*',K=1,50)
ENDIF

!## only for demo-version -> if idemo>0
WRITE(IU,'(50A1)') ('D',K=1,50)
IF(DEMO%IDEMO.EQ.1)THEN
 WRITE(IU,'(A8,A)') 'TOPIC=  ','CROSS'!#=DEMO%TDNAME
 WRITE(IU,'(I10)') DEMO%NXY
 DO I=1,DEMO%NXY 
  WRITE(IU,*) DEMO%X(I),',',DEMO%Y(I),',"'//TRIM(DEMO%L(I))//'"'
 ENDDO
 WRITE(IU,'(A8,I1)') 'IBLINES=',DEMO%IBLOCKLINES
 WRITE(IU,'(A8,I1)') 'IBFILLS=',DEMO%IBLOCKFILLS
ELSEIF(DEMO%IDEMO.EQ.2)THEN
 WRITE(IU,'(A8,A)')  'TOPIC=  ','3D'!#=DEMO%TDNAME
 WRITE(IU,'(A8,I1)') 'CONFIG= ', DEMO%CONFLAG
 WRITE(IU,'(A8,I1)') 'IFILL=  ', DEMO%IFILL
 WRITE(IU,'(A8,I1)') 'IACC=   ',DEMO%ACCFLAG
ENDIF
WRITE(IU,'(A8,I1)')  'ISAVE=  ',DEMO%ISAVEBMP
WRITE(IU,'(50A1)') ('D',K=1,50)

CLOSE(IU)

END SUBROUTINE MAIN_UTL_SAVE_IMF

END MODULE MOD_MAIN_UTL