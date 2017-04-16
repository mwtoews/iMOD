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
MODULE MOD_WBAL_ANALYSE

USE WINTERACTER
USE RESOURCE
USE MOD_COLOURS, ONLY : ICOLOR,COLOUR_RANDOM
USE IMODVAR, ONLY : IDIAGERROR,TP
USE MOD_IDF, ONLY : IDFALLOCATEX,IDFALLOCATESXY,IDFNULLIFY,IDFDEALLOCATEX,IDFFILLSXSY
USE MOD_WBAL_PAR
USE MOD_WBAL_GRAPHICS, ONLY : DRAWBAL
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_CAP,UTL_IDATETOJDATE,UTL_JDATETOIDATE,NV,NL,ITOS,IDATETOGDATE,UTL_GDATE,UTL_GETUNIQUE_CHAR, &
            UTL_WSELECTFILE
 !,RTOS,ITOS,JD,UTL_SUBST,,UTL_CLOSEUNITS, &
!     UTL_INSIDEPOLYGON,UTL_CREATEDIR,UTL_DIRINFO_POINTER,JDATETOFDATE,, &
!     NV,NL,MAXLEN,UTL_MESSAGEHANDLE,
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_GRAPH, ONLY : GRAPH,GRAPHNAMES,GRAPH_DEALLOCATE,GRAPH_ALLOCATE,GRAPH_PLOT

CONTAINS

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_MAIN(ITYPE,MESSAGE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 
 CALL WDIALOGSELECT(MESSAGE%WIN) 
  
 SELECT CASE (ITYPE)
 
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDCANCEL)
     CALL WBAL_ANALYSE_CLOSE()
    CASE (IDHELP)
    !## save configuration
    CASE (IDSAVE)
   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT
 END SELECT
     
 END SUBROUTINE WBAL_ANALYSE_MAIN 

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_TAB1(ITYPE,MESSAGE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER :: I
 
 CALL WDIALOGSELECT(MESSAGE%WIN)
 
 SELECT CASE (ITYPE)
 
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    !## open configuration
    CASE (ID_OPEN1)
!     CALL WBAL_ANALYSE_MAIN_INI(MESSAGE%VALUE1)

    !## open csv
    CASE (ID_OPEN2)
     IF(.NOT.WBAL_ANALYSE_READCSV())CALL WBAL_ANALYSE_DEALLOCATE()
   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
    CASE (IDF_RADIO1,IDF_RADIO2)
     CALL WDIALOGGETRADIOBUTTON(MESSAGE%VALUE2,I)
     IF(I.EQ.1)THEN
      CALL WDIALOGFIELDSTATE(ID_OPEN1,1)
      CALL WDIALOGFIELDSTATE(ID_OPEN2,0)
     ELSE
      CALL WDIALOGFIELDSTATE(ID_OPEN1,0)
      CALL WDIALOGFIELDSTATE(ID_OPEN2,1)
     ENDIF
   END SELECT
 END SELECT
     
 END SUBROUTINE WBAL_ANALYSE_TAB1

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_TAB2(ITYPE,MESSAGE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER :: IROW,ICOL,IRGB
 
 CALL WDIALOGSELECT(MESSAGE%WIN)
 
 SELECT CASE (ITYPE)
 
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDCANCEL)
    CASE (IDHELP)
    CASE (IDOK)
   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
    CASE (IDF_GRID1)
     CALL WGRIDPOS(MESSAGE%Y,ICOL,IROW)
     IF(ICOL.EQ.3.AND.MESSAGE%Y.NE.MESSAGE%X)THEN
      CALL WGRIDGETCELLINTEGER(IDF_GRID1,3,IROW,IRGB)
      CALL WSELECTCOLOUR(IRGB)
      IF(WINFODIALOG(4).EQ.1)THEN
       CALL WGRIDPUTCELLINTEGER(IDF_GRID1,3,IROW,IRGB)
       CALL WGRIDCOLOURCELL(    IDF_GRID1,3,IROW,IRGB,IRGB)
       BUDGET(IROW)%ICLR=IRGB
      ENDIF
      CALL WGRIDSETCELL(IDF_GRID1,1,IROW)
     ENDIF
   END SELECT
 END SELECT
     
 END SUBROUTINE WBAL_ANALYSE_TAB2

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_TAB3(ITYPE,MESSAGE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE

 CALL WDIALOGSELECT(MESSAGE%WIN)
 
 SELECT CASE (ITYPE)
 
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDCANCEL)
    CASE (IDHELP)
    CASE (IDOK)
   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT
 END SELECT
     
 END SUBROUTINE WBAL_ANALYSE_TAB3

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_TAB4(ITYPE,MESSAGE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE

 CALL WDIALOGSELECT(MESSAGE%WIN)
 
 SELECT CASE (ITYPE)
 
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (ID_GRAPHICS,ID_PREVIEW)
     CALL WBAL_ANALYSE_PLOT(MESSAGE%VALUE1)

   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT
 END SELECT
     
 END SUBROUTINE WBAL_ANALYSE_TAB4

 !###======================================================================
 LOGICAL FUNCTION WBAL_ANALYSE_READCSV() 
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=512) :: LINE
 CHARACTER(LEN=256) :: CSVFNAME
 CHARACTER(LEN=52) :: TXT
 INTEGER :: I,J,K,II,IU,IOS,SKIPLINES,CFN_N_ELEM,ICOL,IROW,IBAL

 WBAL_ANALYSE_READCSV=.TRUE.
 
 CSVFNAME='d:\iMOD-Gebruikersdag\IMOD_USER\MODELS\WERKHOVEN\WBALANCE.CSV'

! CSVFNAME=''
! IF(.NOT.UTL_WSELECTFILE('Load Comma Separated File (*.csv)|*.csv|',&
!                  LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,CSVFNAME,&
!                  'Load Comma Separated File (*.csv)'))RETURN

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=CSVFNAME,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot OPEN/READ file called:'//CHAR(13)//&
    TRIM(CSVFNAME),'Error')
  RETURN
 ENDIF

 !## define size of header
 SKIPLINES=0; DO 
  READ(IU,'(A512)') LINE; IF(UTL_CAP(LINE(1:4),'U').EQ.'DATE')EXIT; SKIPLINES=SKIPLINES+1
 ENDDO

 !## number of budgetterms
 NV=CFN_N_ELEM(',;',2,LINE); NV=NV-3
 
 !## allocate object, gwbal(1) is original gwbal(2) is selection to be processed as plot/graph
 CALL WBAL_ANALYSE_DEALLOCATE(); ALLOCATE(GWBAL(2))

 WBAL_ANALYSE_READCSV=.FALSE.

 NBUDGET=NV
 !## read labels of budgetterms
 ALLOCATE(GWBAL(1)%TXT(NV),GWBAL(2)%TXT(NV))
 READ(LINE,*) TXT,TXT,TXT,(GWBAL(1)%TXT(I),I=1,NV)
 DO I=1,NV; GWBAL(2)%TXT(I)=GWBAL(1)%TXT(I); ENDDO
 
 ALLOCATE(BUDGET(NV/2))
 J=0; K=0; DO I=1,NV,2
  J=J+1; K=K+1
  II=INDEX(GWBAL(1)%TXT(I),'_',.TRUE.)
  IF(II.GT.0)THEN
   BUDGET(J)%FLUXTERM=GWBAL(1)%TXT(I)(:II-1)
  ELSE
   BUDGET(J)%FLUXTERM=GWBAL(1)%TXT(I)
  ENDIF
  !## get existing waterbalance term and number
  IBAL=WBAL_ANALYSE_GETBALANCETERM(BUDGET(J)%FLUXTERM)
  IF(IBAL.GT.0)THEN
   BUDGET(J)%LABEL=UTL_CAP(TP(IBAL)%ALIAS,'L')
   BUDGET(J)%LABEL(1:1)=UTL_CAP(BUDGET(J)%LABEL(1:1),'U')
  ELSE
   BUDGET(J)%LABEL=GWBAL(1)%TXT(I)
  ENDIF
  BUDGET(J)%ICLR=COLOUR_RANDOM()
  BUDGET(J)%IGROUP=J
  BUDGET(J)%IACT=1
 ENDDO
 
 !## read to end of file
 NL=-1
 DO
  READ(IU,*,IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  IF(TRIM(LINE).EQ.'--------------------------------------------------')EXIT
  NL=NL+1
 ENDDO
  
 !## allocate memory
 ALLOCATE(GWBAL(1)%CDATE(NL),GWBAL(1)%Q(NV,NL),GWBAL(1)%CLAY(NL),GWBAL(1)%CZONE(NL))

 REWIND(IU)
 
 !## skip header
 DO I=1,SKIPLINES+2; READ(IU,'(A)') LINE; ENDDO
 
 !## read data entire data from csv
 DO I=1,NL
  READ(IU,*) GWBAL(1)%CDATE(I),GWBAL(1)%CLAY(I),GWBAL(1)%CZONE(I),(GWBAL(1)%Q(J,I),J=1,NV)
 ENDDO

 !## try to read in zone-idf
 CALL IDFNULLIFY(IDFP)

 !## look for network dimensions
 DO
  READ(IU,'(A52)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  IF(INDEX(LINE,'DIMENSIONS').GT.0)THEN
   READ(IU,*) IDFP%NCOL
   READ(IU,*) IDFP%NROW
   IF(.NOT.IDFALLOCATEX(IDFP))RETURN
   READ(IU,*) IDFP%XMIN
   READ(IU,*) IDFP%YMIN
   READ(IU,*) IDFP%XMAX
   READ(IU,*) IDFP%YMAX
   READ(IU,*) IDFP%NODATA
   READ(IU,*) IDFP%IEQ
   IF(IDFP%IEQ.EQ.0)THEN
    READ(IU,*) IDFP%DX
    READ(IU,*) IDFP%DY
    !## fill in sx/sy for plottig purposes
    IF(.NOT.IDFFILLSXSY(IDFP))RETURN
   ELSE
    IF(.NOT.IDFALLOCATESXY(IDFP))RETURN
    DO ICOL=0,IDFP%NCOL; READ(IU,*) IDFP%SX(ICOL); ENDDO
    DO IROW=0,IDFP%NROW; READ(IU,*) IDFP%SY(IROW); ENDDO 
   ENDIF
   IDFP%ITB=0; IDFP%IXV=0
   DO IROW=1,IDFP%NROW; READ(IU,*) (IDFP%X(ICOL,IROW),ICOL=1,IDFP%NCOL); ENDDO
   EXIT
  ENDIF
 ENDDO
 
 CLOSE(IU)
 
 ALLOCATE(CILAY(NL),CIZONE(NL),CIDATE(NL))
 CILAY =GWBAL(1)%CLAY
 CIZONE=GWBAL(1)%CZONE
 CIDATE=GWBAL(1)%CDATE

 !## find how many dates
 CALL UTL_GETUNIQUE_CHAR(CIDATE,NL,NDATE)
 !## find how many layers
 CALL UTL_GETUNIQUE_CHAR(CILAY,NL,NLAY)
 !## find how many zones
 CALL UTL_GETUNIQUE_CHAR(CIZONE,NL,NZONE)
 
 !## print summary of csv on tab1
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB1)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,NBUDGET) !## unique budget terms
 CALL WDIALOGPUTINTEGER(IDF_INTEGER2,NDATE)   !## unique period
 CALL WDIALOGPUTINTEGER(IDF_INTEGER3,NLAY)    !## unique layers
 CALL WDIALOGPUTINTEGER(IDF_INTEGER4,NZONE)   !## unique zones
 CALL WDIALOGPUTSTRING(IDF_STRING1,CSVFNAME)  !## name of the csv
 
 !## allocate menufield list indices
 ALLOCATE(LIDATE(NDATE),LIZONE(NZONE),CLRIZONE(NZONE),LILAY(NLAY))
 !## none selected
 LIDATE=0; LIZONE=0; LILAY=0

 IF(.NOT.WBAL_ANALYSE_FILLGRID())RETURN

 !## generate polygon colours
 DO I=1,NZONE; CLRIZONE(I)=COLOUR_RANDOM(); ENDDO
 
 !## fill number of dates
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB2)
 CALL WDIALOGPUTMENU(IDF_MENU1,CIDATE,NDATE,LIDATE)

 !## fill number of layers/zones
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB3)
 CALL WDIALOGPUTMENU(IDF_MENU1,CILAY,NLAY,LILAY)
 CALL WDIALOGPUTMENU(IDF_MENU2,CIZONE,NZONE,LIZONE)

 !## outgrey tabstates
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DWBAL_ANALYSE_TAB2,1)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DWBAL_ANALYSE_TAB3,1)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DWBAL_ANALYSE_TAB4,1)

 WBAL_ANALYSE_READCSV=.TRUE.

 END FUNCTION WBAL_ANALYSE_READCSV

 !###======================================================================
 INTEGER FUNCTION WBAL_ANALYSE_GETBALANCETERM(TXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: TXT
 INTEGER :: I
  
 WBAL_ANALYSE_GETBALANCETERM=0
  
 DO I=1,SIZE(TP)
  IF(TRIM(UTL_CAP(TXT,'U')).EQ.TRIM(UTL_CAP(TP(I)%ACRNM,'U')))EXIT
 ENDDO
 IF(I.LE.SIZE(TP))WBAL_ANALYSE_GETBALANCETERM=I

!  !## find the appropriate sequence number of the graphics
!  SELECT CASE (TXT(4:6))
!   CASE ('DRN'); IQ=1
!   CASE ('OLF'); IQ=2
!   CASE ('RIV'); IQ=3
!   CASE ('GHB'); IQ=4
!   CASE ('ISG'); IQ=5
!   CASE ('WEL'); IQ=6
!   !CASE ('REG'); IQ=1
!   CASE ('BND'); IQ=8
!   CASE ('FLF'); IQ=9
!   CASE (''); IQ=8
!!     Q(01,1)=QDRN_IN   Q(01,1)=QDRN_OUT    Q(13,1)=QCAP_IN   Q(13,1)=QCAP_OUT    
!!     Q(02,1)=QOLF_IN   Q(02,1)=QOLF_OUT    Q(14,1)=QETACT_IN Q(14,1)=QETACT_OUT  
!!     Q(03,1)=QRIV_IN   Q(03,1)=QRIV_OUT    Q(15,1)=QPM_IN    Q(15,1)=QPM_OUT     
!!     Q(04,1)=QGHB_IN   Q(04,1)=QGHB_OUT    Q(16,1)=QPMGW_IN  Q(16,1)=QPMGW_OUT   
!!     Q(05,1)=QISG_IN   Q(05,1)=QISG_OUT    Q(17,1)=QPMSW_IN  Q(17,1)=QPMSW_OUT   
!!     Q(06,1)=QWEL_IN   Q(06,1)=QWEL_OUT    Q(18,1)=QSTO_IN   Q(18,1)=QSTO_OUT    
!!     Q(07,1)=QREG_IN   Q(07,1)=QREG_OUT    Q(19,1)=QDECSTO_INQ(19,1)=QDECSTO_OUT 
!!     Q(08,1)=QCNH_IN   Q(08,1)=QCNH_OUT    Q(20,1)=QQSPGW_IN Q(20,1)=QQSPGW_OUT  
!!     Q(09,1)=QFLF1_IN  Q(09,1)=QFLF1_OUT   Q(21,1)=QQCOR_IN  Q(21,1)=QQCOR_OUT   
!!     Q(10,1)=QFLF2_IN  Q(10,1)=QFLF2_OUT   Q(22,1)=QQDR_IN   Q(22,1)=QQDR_OUT    
!!     Q(11,1)=QRCH_IN   Q(11,1)=QRCH_OUT    Q(23,1)=QQRUN_IN  Q(23,1)=QQRUN_OUT   
!!     Q(12,1)=QEVT_IN   Q(12,1)=QEVT_OUT    Q(24,1)=QMODF_IN  Q(24,1)=QMODF_OUT   

 END FUNCTION WBAL_ANALYSE_GETBALANCETERM
 
 !###======================================================================
 LOGICAL FUNCTION WBAL_ANALYSE_FILLGRID()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,NROW
  
 WBAL_ANALYSE_FILLGRID=.FALSE.
  
 !## fill grid
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB2)
 NROW=WINFOGRID(IDF_GRID1,GRIDROWSMAX)
 IF(SIZE(BUDGET).GT.NROW)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can store maximal number of '//TRIM(ITOS(NROW))//' waterbalance terms.'//CHAR(13)// &
    'The number of waterbalance terms in te selected csv-file is '//TRIM(ITOS(SIZE(BUDGET))),'Error')
  RETURN
 ENDIF
 CALL WGRIDROWS(IDF_GRID1,SIZE(BUDGET))
 DO I=1,SIZE(BUDGET)
  CALL WGRIDPUTCELLCHECKBOX(IDF_GRID1,1,I,BUDGET(I)%IACT)
  CALL WGRIDPUTCELLSTRING(  IDF_GRID1,2,I,BUDGET(I)%FLUXTERM)
  CALL WGRIDPUTCELLINTEGER( IDF_GRID1,3,I,BUDGET(I)%ICLR)
  CALL WGRIDCOLOURCELL(     IDF_GRID1,3,I,BUDGET(I)%ICLR,BUDGET(I)%ICLR)
  CALL WGRIDPUTCELLSTRING(  IDF_GRID1,4,I,BUDGET(I)%LABEL)
  CALL WGRIDPUTCELLINTEGER( IDF_GRID1,5,I,BUDGET(I)%IGROUP)
 ENDDO
   
 WBAL_ANALYSE_FILLGRID=.TRUE.
 
 END FUNCTION  WBAL_ANALYSE_FILLGRID

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_PLOT(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 
 !## get the appropriate selection
 IF(.NOT.WBAL_ANAYSE_PREPARE())RETURN

 IF(ID.EQ.ID_GRAPHICS)THEN
  CALL WBAL_ANALYSE_PLOTIMAGE()
 ELSEIF(ID.EQ.ID_PREVIEW)THEN
  IF(WBAL_ANALYSE_PLOTGRAPH(1,1))THEN; ENDIF
 ENDIF
 
!     IY1=19630101
!     IY2=20140101
!     IPERIOD=1
!     ITIME=0
!     CALL WDIALOGGETMENU(IDF_MENU1,IBLANK)
!     CALL WBAL_ANAYSE_PREPARE(IBLANK,ITIME,IPERIOD,IY1,IY2) 
!     IF(WBAL_ANALYSE_PLOTGRAPH(NLAY,NZONE))THEN; ENDIF
!     DEALLOCATE(GWBAL(2)%CDATE,GWBAL(2)%Q,GWBAL(2)%CLAY,GWBAL(2)%CZONE)
!     CALL WDIALOGSELECT(ID_WBALMAIN)

 END SUBROUTINE WBAL_ANALYSE_PLOT

 !###======================================================================
 LOGICAL FUNCTION WBAL_ANAYSE_PREPARE() 
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: NQ,NT,I,J,K,II
! REAL :: A
 
 WBAL_ANAYSE_PREPARE=.FALSE.
 
! SELECT CASE (ITIME)
!  !## as is
!  CASE (0)
!   ALLOCATE(GWBAL(2)%CDATE(NL),GWBAL(2)%Q(NV,NL),GWBAL(2)%CLAY(NL),GWBAL(2)%CZONE(NL))
!   DO I=1,NL; GWBAL(2)%CDATE(I)=GWBAL(1)%CDATE(I); ENDDO
!   DO I=1,NL; GWBAL(2)%CLAY(I) =GWBAL(1)%CLAY(I); ENDDO
!   DO I=1,NL; GWBAL(2)%CZONE(I)=GWBAL(1)%CZONE(I); ENDDO
!   DO I=1,NL; DO J=1,NV; GWBAL(2)%Q(J,I)=GWBAL(1)%Q(J,I); ENDDO; ENDDO
!   NLL=NL
!  !## total per period (m3/day)
!  CASE (1)
!
!   JD1=UTL_IDATETOJDATE(IY1)
!   JD2=UTL_IDATETOJDATE(IY2)
!   JDP=0
!
!   !## number of periods
!   SELECT CASE (IPERIOD)
!    CASE (1)  !## year
!     CALL UTL_GDATE(JD1,IY1,IM1,ID1)
!     CALL UTL_GDATE(JD2,IY2,IM2,ID2)
!     NLL=IY2-IY1+1
!    CASE (2)  !## month
!     CALL UTL_GDATE(JD1,IY1,IM1,ID1)
!     CALL UTL_GDATE(JD2,IY2,IM2,ID2)
!     NLL=(IY2-IY1+1)*12
!   END SELECT
!
!   NLL=NLL*NZONE*NLAY    

!## make selection

   !## number of budgets
   NQ=SIZE(GWBAL(1)%Q,1)
   !## number of periods
   NT=SIZE(GWBAL(1)%Q,2)
   
   ALLOCATE(GWBAL(2)%CDATE(NT),GWBAL(2)%Q(NQ,NT),GWBAL(2)%CLAY(NT),GWBAL(2)%CZONE(NT), &
     GWBAL(2)%TXT(NQ),GWBAL(2)%ICLR(NQ),GWBAL(2)%FLX(NQ))

!   GWBAL(2)%CDATE=0; GWBAL(2)%Q=0.0; GWBAL(2)%CLAY=0; GWBAL(2)%CZONE=0

!  CHARACTER(LEN=52) :: LABEL,FLUXTERM
!  INTEGER :: ICLR,IACT,IGROUP
! END TYPE WBUDGETOBJ
! TYPE(WBUDGETOBJ),ALLOCATABLE,DIMENSION(:) :: BUDGET

   !## fill in label/color etc.
   J=0; K=0; DO I=1,SIZE(GWBAL(1)%Q,1)
!# NOG IETS MET GROUP DOEN
    IF(MOD(I,2).NE.0)K=K+1
    IF(BUDGET(K)%IACT.EQ.0)CYCLE
    J=J+1
    GWBAL(2)%ICLR(J)=BUDGET(K)%ICLR
    GWBAL(2)%TXT(J) =BUDGET(K)%LABEL
    GWBAL(2)%FLX(J) =BUDGET(K)%FLUXTERM
   ENDDO
   
   J=0; DO I=1,NL
!# correct layer/zon/date
!LILAY(),LIZONE(),LIDATE()
!    IF()THEN
     J=J+1
     GWBAL(2)%CDATE(J)=GWBAL(1)%CDATE(I)
     II=0; DO K=1,NQ
!      IF()THEN
       II=II+1
       GWBAL(2)%Q(II,J)=GWBAL(1)%Q(K,I)
!      ENDIF
     ENDDO
     GWBAL(2)%CLAY(J) =GWBAL(1)%CLAY(I)
     GWBAL(2)%CZONE(J)=GWBAL(1)%CZONE(I)
!    ENDIF
   ENDDO
   
!   I=0
!   K=0
!   DO 
!    I=I+1
!    IF(I.GT.SIZE(GWBAL(1)%CDATE))EXIT
!     
!    !## get year - julian dates !!!
!    J=UTL_IDATETOJDATE(GWBAL(1)%CDATE(I))
!
!    IF(J.LT.JD1)CYCLE    !## searching for startdate
!    IF(J.GT.JD2)EXIT     !## finished
!    IF(J.GT.JDP)THEN     !## goto the next period
!
!     IF(JDP.GT.0)THEN
!      JD1=JDP
!      K=K+NLAY*NZONE
!     ENDIF
!      
!     CALL IDATETOGDATE(GWBAL(1)%CDATE(I),IY1,IM1,ID1)
!     SELECT CASE (IPERIOD)
!      CASE (1)  !## year
!       JDP=JD1+365; IF(WDATELEAPYEAR(IY1))JDP=JDP+1
!      CASE (2)  !## month
!       JDP=JD1+WDATEDAYSINMONTH(IY1,IM1)-1
!     END SELECT
!
!    ENDIF
!
!    !## sum for all ilay/izone
!    I=I-1
!    DO J=1,NLAY*NZONE
!     I=I+1
!
!     GWBAL(2)%CDATE(K+J)=UTL_JDATETOIDATE(JD1)
!     GWBAL(2)%CLAY(K+J) =GWBAL(1)%CLAY(I)
!     GWBAL(2)%CZONE(K+J)=GWBAL(1)%CZONE(I)
!
!     DO JJ=1,NV
!      GWBAL(2)%Q(JJ,K+J)=GWBAL(2)%Q(JJ,K+J)+GWBAL(1)%Q(JJ,I)
!     ENDDO
!    ENDDO
!     
!   ENDDO   
! END SELECT
!
! !## remove topics if needed
! DO J=1,NV
!  IF(IBLANK(J).EQ.0)THEN
!   DO I=1,NLL; GWBAL(2)%Q(J,I)=0.0; ENDDO
!  ENDIF
!  IF(.FALSE.)THEN
!   A=121647.0*250.0*250.0
!   A=1000.0/A
!  ELSE
!   A=1.0
!  ENDIF
!  DO I=1,NLL; GWBAL(2)%Q(J,I)=GWBAL(2)%Q(J,I)*A; ENDDO
! ENDDO
!
!! !## sum 1-4 and 5-8
!! DO I=1,NLL
! ! DO J=1,4
! !  GWBAL(2)%Q(10,I)=GWBAL(2)%Q(10,I)+GWBAL(2)%Q(J,I)
! ! ENDDO
! ! DO J=6,8
! !  GWBAL(2)%Q(11,I)=GWBAL(2)%Q(11,I)+GWBAL(2)%Q(J,I)
! ! ENDDO
! !ENDDO
! 
 WBAL_ANAYSE_PREPARE=.TRUE.

 END FUNCTION WBAL_ANAYSE_PREPARE

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_PLOTIMAGE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: NXPIX=1000, NYPIX=1200 !## resolution dx,dy
 INTEGER,PARAMETER :: NFLX=24  !## number of zones in current csv file
 REAL,PARAMETER :: CS=0.009 !## charactersize
 CHARACTER(LEN=256) :: PNGNAME
 LOGICAL :: LOCAL,PERC
 INTEGER :: IPOL,IOS,I
 REAL,DIMENSION(:,:), ALLOCATABLE :: Q,QSUBREGIO
 CHARACTER(LEN=10),DIMENSION(:),ALLOCATABLE :: QTXT
 INTEGER,DIMENSION(:),ALLOCATABLE :: IPLG
 
!QTXT(01)='Q-drn  '             bdgdrn  
!QTXT(02)='Q-olf  '             bdgolf  
!QTXT(03)='Q-riv  '             bdgriv  
!QTXT(04)='Q-ghb  '             bdgghb  
!QTXT(05)='Q-isg  '             bdgisg  
!QTXT(06)='Q-wel  '             bdgwel  
!QTXT(07)='Q-reg  '             bdgfrf en bdgfff
!QTXT(08)='Q-cnh  '             bdgbnd
!QTXT(09)='Q-ftf  '             bdgflf
!QTXT(10)='Q-flf  '             bdgflf
!QTXT(11)='Q-rch  '             bdgrch
!QTXT(12)='Q-evt  '             bdgevt
!QTXT(13)='Q-cap   '            bdgcap
!QTXT(14)='Q-etact '            bdgETact  
!QTXT(15)='Q-pm    '            bdgpm   
!QTXT(16)='Q-pmgw  '            bdgpmgw 
!QTXT(17)='Q-pmsw  '            bdgpmsw 
!QTXT(18)='Q-sto   '            bdgsto
!QTXT(19)='Q-decsto'            bdgdecStot
!QTXT(20)='Q-spgw  '            bdgqspgw
!QTXT(21)='Q-cor   '            msw_qsimcorrmf
!QTXT(22)='Q-qdr   '            bdgdrn    
!QTXT(23)='Q-qrun  '             ????   weet ik nog niet precies, bdgqrun
!QTXT(24)='Q-modf  '            bdgqmodf  

 !!     Q(01,1)=QDRN_IN   Q(01,1)=QDRN_OUT    Q(13,1)=QCAP_IN   Q(13,1)=QCAP_OUT    
!!     Q(02,1)=QOLF_IN   Q(02,1)=QOLF_OUT    Q(14,1)=QETACT_IN Q(14,1)=QETACT_OUT  
!!     Q(03,1)=QRIV_IN   Q(03,1)=QRIV_OUT    Q(15,1)=QPM_IN    Q(15,1)=QPM_OUT     
!!     Q(04,1)=QGHB_IN   Q(04,1)=QGHB_OUT    Q(16,1)=QPMGW_IN  Q(16,1)=QPMGW_OUT   
!!     Q(05,1)=QISG_IN   Q(05,1)=QISG_OUT    Q(17,1)=QPMSW_IN  Q(17,1)=QPMSW_OUT   
!!     Q(06,1)=QWEL_IN   Q(06,1)=QWEL_OUT    Q(18,1)=QSTO_IN   Q(18,1)=QSTO_OUT    
!!     Q(07,1)=QREG_IN   Q(07,1)=QREG_OUT    Q(19,1)=QDECSTO_INQ(19,1)=QDECSTO_OUT 
!!     Q(08,1)=QCNH_IN   Q(08,1)=QCNH_OUT    Q(20,1)=QQSPGW_IN Q(20,1)=QQSPGW_OUT  
!!     Q(09,1)=QFLF1_IN  Q(09,1)=QFLF1_OUT   Q(21,1)=QQCOR_IN  Q(21,1)=QQCOR_OUT   
!!     Q(10,1)=QFLF2_IN  Q(10,1)=QFLF2_OUT   Q(22,1)=QQDR_IN   Q(22,1)=QQDR_OUT    
!!     Q(11,1)=QRCH_IN   Q(11,1)=QRCH_OUT    Q(23,1)=QQRUN_IN  Q(23,1)=QQRUN_OUT   
!!     Q(12,1)=QEVT_IN   Q(12,1)=QEVT_OUT    Q(24,1)=QMODF_IN  Q(24,1)=QMODF_OUT   
  
 !## zone numbers
 ALLOCATE(IPLG(SIZE(CIZONE))); IPLG=0
 DO I=1,SIZE(CIZONE)
  READ(CIZONE(I),*,IOSTAT=IOS) IPLG(I)
  IF(IOS.NE.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot convert zone '//TRIM(CIZONE(I))//CHAR(13)// &
    'into a integer value.','Error')
   DEALLOCATE(IPLG); RETURN  
  ENDIF
 ENDDO
  
 !## selected polygon
 IPOL=1  !lizone()

 ALLOCATE(QSUBREGIO(SIZE(LIZONE),2),Q(NFLX,2),QTXT(NFLX))

 qtxt='dds'
 call random_number(q)
 qsubregio=0.
 
 PNGNAME='D:\TEXT.PNG'

 !## plot local window of selected polygon ipol
 LOCAL=.false.
 
 !## percentiles
 PERC=.FALSE.
 
 IF(.NOT.DRAWBAL(Q,QTXT,NXPIX,NYPIX,CS,IPOL,SIZE(LIZONE),QSUBREGIO,PERC,CLRIZONE,IPLG,PNGNAME,IDFP,LOCAL,'GRAPHTITLE'))THEN
 ENDIF

 DEALLOCATE(QSUBREGIO,Q,QTXT,IPLG)
 
 END SUBROUTINE WBAL_ANALYSE_PLOTIMAGE

 !###======================================================================
 LOGICAL FUNCTION WBAL_ANALYSE_PLOTGRAPH(NL,NZ)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NL,NZ !## number of layers (nl); number of zones (nz)
 INTEGER,PARAMETER :: MXFLUX=11
 INTEGER :: NQ,NB,I,J,K,II,JJ,III,KK,KKK,IDATE,IX,IPOS,ITYPE,IOS
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: IBAR
 REAL :: X
 REAL,ALLOCATABLE,DIMENSION(:,:,:) :: XG
 INTEGER,DIMENSION(0:MXFLUX) :: ICOLOR
! CHARACTER(LEN=20),DIMENSION(MXFLUX) :: CFLUX
! DATA CFLUX/'CONSTANT HEAD','FLUX LOWER FACE','FLUX UPPER FACE','FLUX FRONT FACE','FLUX RIGHT FACE','STORAGE', &
!            'EVAPOTRANSPIRATION','OVERLAND FLOW','RECHARGE','SEGMENTS','WELLS'/

 WBAL_ANALYSE_PLOTGRAPH=.FALSE.

! !## positive

!CLRIZONE(I)

! ICOLOR(0) =WRGB(225,225,225)! -- unknown grey
! ICOLOR(1) =WRGB(0  ,0  ,64 ) !## constant head
! ICOLOR(2) =WRGB(0  ,0  ,255) !## flf
! ICOLOR(3) =WRGB(128,128,255) !## fuf
! ICOLOR(4) =WRGB(0  ,128,64 ) !## fff
! ICOLOR(5) =WRGB(0  ,255,0  ) !## frf
! ICOLOR(6) =WRGB(0  ,255,255) !## sto
! ICOLOR(7) =WRGB(128,255,128) !## evt
! ICOLOR(8) =WRGB(255,0  ,0  ) !## olf
! ICOLOR(9) =WRGB(255,128,64 ) !## rch
! ICOLOR(10)=WRGB(128,0  ,0  ) !## isg
! ICOLOR(11)=WRGB(255,255,0  ) !## wel
! ICOLOR(12)=WRGB(255,255,128) 
! ICOLOR(13)=WRGB(255,128,255) 
! ICOLOR(14)=WRGB(128,0  ,255) 

 IF(ALLOCATED(GRAPH))CALL GRAPH_DEALLOCATE()
 
 !## if four columns per category
 !ITYPE=2
 !## if two columns per category
 ITYPE=1
 
 !## number of budgets
 NQ= SIZE(GWBAL(2)%Q,1)
 !## number of bars
 NB= NQ/(2*ITYPE) 

 !## select bars to be plotted
 ALLOCATE(IBAR(NB,2)); IBAR=0
 J=0; K=1
 DO I=1,NB
  J=J+1
  IBAR(J,1)=K 
  IBAR(J,2)=IBAR(J,1)+1
  K=K+2*ITYPE 
 ENDDO

 !## check whether these are all non-zero per category
 DO I=1,NB/ITYPE 
  DO J=1,2
   II=IBAR(I,J)
   IX=0  
   DO K=1,NL*NZ
    X=GWBAL(2)%Q(II,K)
    IF(X.NE.0.0)IX=IX+1
   ENDDO
   IF(IX.EQ.0)IBAR(I,J)=-IBAR(I,J)
  ENDDO
 ENDDO

 IX=0; DO I=1,NB/ITYPE; DO J=1,2; IF(IBAR(I,J).GT.0)IX=IX+1; ENDDO; ENDDO; NB=IX
 
 CALL GRAPH_ALLOCATE(NB,NZ)

 DO I=1,NB; DO J=1,NZ
   ALLOCATE(GRAPH(I,J)%RX(NL),GRAPH(I,J)%RY(NL))
 ENDDO; ENDDO
 
 K=0
 DO I=1,NL
  DO J=1,NZ
   K=K+1
   GRAPHNAMES(K)='Layer '//TRIM(GWBAL(2)%CLAY(I))//'; Zone Number '//TRIM(GWBAL(2)%CZONE(J))
  ENDDO
 ENDDO

 !## keep record of minimal and maximal values
 ALLOCATE(XG(NL,NZ,2)); XG=0.0

 !## fill in legend
 !## read each group
 DO J=1,NZ
  III=0
  !## read in/out volumes
  DO JJ=1,2
   !## read each active bar
   DO I=1,SIZE(IBAR,1)
    !## skip empty bars
    II=IBAR(I,JJ); IF(II.LE.0)CYCLE
    III=III+1
    GRAPH(III,J)%NP=NL
    !## columns (histogram)
    GRAPH(III,J)%GTYPE =1
    GRAPH(III,J)%LEGTXT=UTL_CAP(GWBAL(2)%TXT(II),'U')
    GRAPH(III,J)%ICLR  =GWBAL(2)%ICLR(II)
   ENDDO
  ENDDO
 ENDDO

 !## read each timestep
 KK=0
 DO K=1,NL
  !## read each group
  DO J=1,NZ
   III=0
   KK=KK+1
   !## read in/out volumes
   DO JJ=1,2
    !## read each active bar
    DO I=1,SIZE(IBAR,1)
     !## skip empty bars
     II=IBAR(I,JJ); IF(II.LE.0)CYCLE
     III=III+1

!## doe iets met jaartallen ...

     !## always first column
     READ(GWBAL(2)%CDATE(KK),*,IOSTAT=IOS) IDATE
     IF(IOS.NE.0)THEN
      GRAPH(III,J)%RX(K)=0.0
     ELSE
      GRAPH(III,J)%RX(K)=REAL(UTL_IDATETOJDATE(IDATE))
     ENDIF

     !## get balance value as summed value (stacked)
     X=GWBAL(2)%Q(II,KK)
     KKK=1; IF(X.LT.0.0)KKK=2
     XG(K,J,KKK)=XG(K,J,KKK)+X
     X=XG(K,J,KKK)
     GRAPH(III,J)%RY(K)=X

    ENDDO

   ENDDO
  ENDDO
 ENDDO
 
 DEALLOCATE(XG)
  
!TYPE GRAPHOBJ
! REAL,POINTER,DIMENSION(:) :: RX,RY !## x and y values
! INTEGER :: GTYPE !## graph type 1=solid 2=lines 3=histogram
! INTEGER :: NP  !## no. points
! CHARACTER(LEN=50) :: LEGTXT !## legend text
! INTEGER :: ICLR
!END TYPE GRAPHOBJ
!TYPE(GRAPHOBJ),DIMENSION(:,:),ALLOCATABLE :: GRAPH

! CALL WINDOWOPEN(FLAGS=SYSMENUON+HIDEWINDOW+STATUSBAR)
! CALL WINDOWSTATUSBARPARTS(4,(/2000,2000,750,-1/),(/1,1,1,1/))
! CALL IGRCOLOURMODEL(24)
! CALL IMODINITMESSAGE()
! CALL UTL_MESSAGEHANDLE(1)
 
 !## plot graph(s)
! IF()THEN
  !## no dates
  CALL GRAPH_PLOT('Time','Volumes (m3/d)',.FALSE.)
! ELSE
!  CALL GRAPH_PLOT('Time','Volumes (m3/d)',.TRUE.)
! ENDIF
 
 !## clean up, deallocate
 IF(ALLOCATED(GRAPH))CALL GRAPH_DEALLOCATE()

 WBAL_ANALYSE_PLOTGRAPH=.TRUE.

 END FUNCTION WBAL_ANALYSE_PLOTGRAPH

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_INIT() 
 !###======================================================================
 IMPLICIT NONE 

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_WBAL_ANALYSE,2).EQ.1)THEN
  CALL WBAL_ANALYSE_CLOSE()
  RETURN
 ENDIF

 CALL MAIN1INACTMODULE(ID_WBAL_ANALYSE)

 !## other module no closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_WBAL_ANALYSE,2,1)

 !## fill in dialog
 CALL WDIALOGLOAD(ID_DWBAL_ANALYSE,ID_DWBAL_ANALYSE)
! CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN,1)
! CALL WDIALOGPUTIMAGE(ID_SAVEAS,ID_ICONSAVEAS,1)

 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB1)
 CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONOPEN,1)
 CALL WDIALOGPUTIMAGE(ID_OPEN2,ID_ICONOPEN,1)
 
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB2)
 CALL WDIALOGPUTIMAGE(ID_PLUS1,ID_ICONPLUS,1)
 CALL WDIALOGPUTIMAGE(ID_PLUS2,ID_ICONPLUS,1)
 CALL WDIALOGPUTIMAGE(ID_MIN1,ID_ICONMIN,1)
 CALL WDIALOGPUTIMAGE(ID_MIN2,ID_ICONMIN,1)

 !## outgrey tabs
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DWBAL_ANALYSE_TAB2,0)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DWBAL_ANALYSE_TAB3,0)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DWBAL_ANALYSE_TAB4,0)

 CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE WBAL_ANALYSE_INIT

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 IF(ALLOCATED(GWBAL))THEN
  DO I=1,SIZE(GWBAL)
   IF(ASSOCIATED(GWBAL(I)%CDATE))DEALLOCATE(GWBAL(I)%CDATE)
   IF(ASSOCIATED(GWBAL(I)%CLAY)) DEALLOCATE(GWBAL(I)%CLAY)
   IF(ASSOCIATED(GWBAL(I)%CZONE))DEALLOCATE(GWBAL(I)%CZONE)
   IF(ASSOCIATED(GWBAL(I)%Q))    DEALLOCATE(GWBAL(I)%Q)
   IF(ASSOCIATED(GWBAL(I)%TXT))  DEALLOCATE(GWBAL(I)%TXT)
   IF(ASSOCIATED(GWBAL(I)%FLX))  DEALLOCATE(GWBAL(I)%FLX)
   IF(ASSOCIATED(GWBAL(I)%ICLR)) DEALLOCATE(GWBAL(I)%ICLR)
  ENDDO
  DEALLOCATE(GWBAL)
 ENDIF
 
 IF(ALLOCATED(BUDGET))DEALLOCATE(BUDGET)
 
 IF(ALLOCATED(CILAY))   DEALLOCATE(CILAY)
 IF(ALLOCATED(CIZONE))  DEALLOCATE(CIZONE)
 IF(ALLOCATED(CIDATE))  DEALLOCATE(CIDATE)
 IF(ALLOCATED(LILAY))   DEALLOCATE(LILAY)
 IF(ALLOCATED(LIZONE))  DEALLOCATE(LIZONE)
 IF(ALLOCATED(CLRIZONE))DEALLOCATE(CLRIZONE)
 IF(ALLOCATED(LIDATE))  DEALLOCATE(LIDATE)

 CALL IDFDEALLOCATEX(IDFP)
 
 END SUBROUTINE WBAL_ANALYSE_DEALLOCATE

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_CLOSE()
 !###======================================================================
 IMPLICIT NONE
 
 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_WBAL_ANALYSE,2,0)

 CALL WBAL_ANALYSE_DEALLOCATE()
 
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE)
 CALL WDIALOGUNLOAD()

 END SUBROUTINE WBAL_ANALYSE_CLOSE

END MODULE MOD_WBAL_ANALYSE