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
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_CAP,UTL_IDATETOJDATE,UTL_JDATETOIDATE,ITOS,IDATETOGDATE,UTL_GDATE,UTL_GETUNIQUE_CHAR, &
            UTL_WSELECTFILE,RTOS
 !,RTOS,ITOS,JD,UTL_SUBST,,UTL_CLOSEUNITS, &
!     UTL_INSIDEPOLYGON,UTL_CREATEDIR,UTL_DIRINFO_POINTER,JDATETOFDATE,, &
!     NV,NL,MAXLEN,UTL_MESSAGEHANDLE,
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_GRAPH, ONLY : GRAPH,GRAPHNAMES,GRAPH_DEALLOCATE,GRAPH_ALLOCATE,GRAPH_PLOT,GRAPHDIM

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
 INTEGER :: NV,NL
 
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

 NBUDGET=NV/2
 !## read labels of budgetterms
 ALLOCATE(GWBAL(1)%TXT(NV),GWBAL(2)%TXT(NV))
 READ(LINE,*) TXT,TXT,TXT,(GWBAL(1)%TXT(I),I=1,NV)
 DO I=1,NV; GWBAL(2)%TXT(I)=GWBAL(1)%TXT(I); ENDDO
 
 ALLOCATE(BUDGET(NBUDGET))
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
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,NBUDGET)  !## unique budget terms
 CALL WDIALOGPUTINTEGER(IDF_INTEGER2,NDATE)    !## unique period
 CALL WDIALOGPUTINTEGER(IDF_INTEGER3,NLAY)     !## unique layers
 CALL WDIALOGPUTINTEGER(IDF_INTEGER4,NZONE)    !## unique zones
 NRECORDS=NL; CALL WDIALOGPUTINTEGER(IDF_INTEGER5,NRECORDS) !## unique zones
 CALL WDIALOGPUTSTRING(IDF_STRING1,CSVFNAME)   !## name of the csv
 
 !## allocate menufield list indices
 ALLOCATE(LIDATE(NDATE),LIZONE(NZONE),CLRIZONE(NZONE),LILAY(NLAY))
 !## none selected
 LIDATE=0; LIZONE=0; LILAY=0; CLRIZONE=0

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
 INTEGER :: IOPT
 CHARACTER(LEN=256) :: CSVFNAME
 
 !## get the appropriate selection
 IF(.NOT.WBAL_ANAYSE_PREPARE())RETURN

 !## get option from the window to determine what to do
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IOPT)
 SELECT CASE (IOPT)
  !## timeseries
  CASE (1)
   CALL GRAPH_PLOT('Time','Volumes (m3/d)',.FALSE.)

! IF(ID.EQ.ID_GRAPHICS)THEN
! ELSEIF(ID.EQ.ID_PREVIEW)THEN
!  CALL GRAPH_PLOT('Time','Volumes (m3/d)',.FALSE.)
! ENDIF

  !# # graph
  CASE (2)
   CALL WBAL_ANALYSE_PLOTIMAGE()

  !## save to csv
  CASE (3)
   IF(WBAL_ANALYSE_EXPORTCSV(''))THEN; ENDIF
  !## table
  CASE (4)
   CSVFNAME='D:\DUMMY.CSV'
   IF(WBAL_ANALYSE_EXPORTCSV(CSVFNAME))THEN
    IF(WBAL_ANALYSE_TABLE(CSVFNAME))THEN; ENDIF
   ENDIF
  CASE (5)
 END SELECT
 
 !## clean up, deallocate
 IF(ALLOCATED(GRAPH))CALL GRAPH_DEALLOCATE()

 END SUBROUTINE WBAL_ANALYSE_PLOT

 !###======================================================================
 LOGICAL FUNCTION WBAL_ANAYSE_PREPARE() 
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,IL,IZ,ID,IG,IQIN,IQOU,JQIN,JQOU,IQ,JQ,IOS,IDATE,ITTYPE
 REAL :: QIN,QOU
  
 WBAL_ANAYSE_PREPARE=.FALSE.
 
 IF(ALLOCATED(GRAPH))CALL GRAPH_DEALLOCATE()

 lilay=1
 lizone=1
 lidate=1
 
 !## make selection
 MLAY=0;    DO I=1,NLAY;    IF(LILAY(I).EQ.1)      MLAY=MLAY+1;       ENDDO
 MZONE=0;   DO I=1,NZONE;   IF(LIZONE(I).EQ.1)     MZONE=MZONE+1;     ENDDO
 MDATE=0;   DO I=1,NDATE;   IF(LIDATE(I).EQ.1)     MDATE=MDATE+1;     ENDDO
 MBUDGET=0; DO I=1,NBUDGET; IF(BUDGET(I)%IACT.EQ.1)MBUDGET=MBUDGET+2; ENDDO
  
 !## total groups in graph
 MGROUP=MZONE*MLAY

 IF(MGROUP.EQ.0.OR.MBUDGET.EQ.0.OR.MDATE.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'There is nothing selected to generate a water balance for'//CHAR(13)// &
    ' Go to the tabs BUDGETTERMS and AGGREGATION to make an appropriate selection,','Information')
  RETURN
 ENDIF
 
 !## allocate graph memory
 CALL GRAPH_ALLOCATE(MBUDGET,MGROUP)
 DO I=1,MBUDGET; DO J=1,MGROUP
  ALLOCATE(GRAPH(I,J)%RX(MDATE),GRAPH(I,J)%RY(MDATE))
  GRAPH(I,J)%RX=0.0; GRAPH(I,J)%RY=0.0; GRAPH(I,J)%NP=MDATE
 ENDDO; ENDDO

 !## all histograms
 GRAPH%GTYPE =1

 ALLOCATE(CMDATE(MDATE),CMLAY(MLAY),CMZONE(MZONE)); CMDATE=''; CMLAY=''; CMZONE=''

 J=0; DO I=1,NLAY; IF(LILAY(I).EQ.1)THEN
   J=J+1; CMLAY(J)=CILAY(I)
 ENDIF; ENDDO
 J=0; DO I=1,NZONE; IF(LIZONE(I).EQ.1)THEN
   J=J+1; CMZONE(J)=CIZONE(I)
 ENDIF; ENDDO
 ITTYPE=0; J=0; DO I=1,NDATE; IF(LIDATE(I).EQ.1)THEN
   J=J+1; CMDATE(J)=CIDATE(I)
   !## check whether all dates are convertable
   IF(ITTYPE.EQ.0)THEN
    READ(CIDATE(I),*,IOSTAT=IOS) IDATE
    IF(IOS.NE.0)ITTYPE=1
   ENDIF
 ENDIF; ENDDO

 !## add custom predefined axes titles
 IF(ITTYPE.EQ.1)THEN
  ALLOCATE(GRAPHDIM%XTXT(MDATE)); GRAPHDIM%XTXT=''
 ENDIF
 
 !## assign label name for each group (= zone and layer)
 K=0; DO I=1,NLAY; DO J=1,NZONE
  IF(LILAY(I).EQ.0)CYCLE
  IF(LIZONE(J).EQ.0)CYCLE
  K=K+1; GRAPHNAMES(K)='Layer '//TRIM(CILAY(I))//'; Zone Number '//TRIM(CIZONE(J))
 ENDDO; ENDDO

 !## gather main information
 JQ=0; DO IQ=1,NBUDGET
  IF(BUDGET(IQ)%IACT.EQ.0)CYCLE

  JQ=JQ+1
  IG=0; DO I=1,NLAY; DO J=1,NZONE
   IF(LILAY(I).EQ.0)CYCLE
   IF(LIZONE(J).EQ.0)CYCLE
  
   IG=IG+1
  
   JQIN=(JQ-1)*2+1
   JQOU= JQIN+1
  
   GRAPH(JQIN,IG)%LEGTXT=TRIM(UTL_CAP(BUDGET(IQ)%LABEL,'U'))//'_in'
   GRAPH(JQOU,IG)%LEGTXT=TRIM(UTL_CAP(BUDGET(IQ)%LABEL,'U'))//'_out'
   GRAPH(JQIN,IG)%ICLR  =BUDGET(IQ)%ICLR
   GRAPH(JQOU,IG)%ICLR  =BUDGET(IQ)%ICLR
   GRAPH(JQIN,IG)%CTYPE =TRIM(UTL_CAP(BUDGET(IQ)%FLUXTERM,'U'))//'_in'
   GRAPH(JQOU,IG)%CTYPE =TRIM(UTL_CAP(BUDGET(IQ)%FLUXTERM,'U'))//'_in'
  
  ENDDO; ENDDO
 ENDDO

 !## gather data
 DO I=1,NRECORDS
  
  !## apprppriate item
  IF(.NOT.WBAL_ANALYSE_SELECT(GWBAL(1)%CLAY(I),GWBAL(1)%CZONE(I),GWBAL(1)%CDATE(I),IL,IZ,ID))CYCLE

  !## get appropriate group number
  IG=(IZ-1)*MZONE+IL

  JQIN=-1; JQOU=0; DO IQ=1,NBUDGET
   IF(BUDGET(IQ)%IACT.EQ.0)CYCLE

   JQIN= JQIN+2
   JQOU= JQOU+2
   IQIN=(IQ-1)*2+1
   IQOU= IQIN+1

   !## get balance value as summed value (stacked)
   QIN=GWBAL(1)%Q(IQIN,I)
   QOU=GWBAL(1)%Q(IQOU,I)

   !## add to existing fluxes
   GRAPH(JQIN,IG)%RY(ID)=GRAPH(JQIN,IG)%RY(ID)+QIN
   GRAPH(JQOU,IG)%RY(ID)=GRAPH(JQOU,IG)%RY(ID)+QOU

   !## apply axes titles predefined
   IF(ITTYPE.EQ.0)THEN
    READ(GWBAL(1)%CDATE(I),*,IOSTAT=IOS) IDATE
    GRAPH(JQIN,IG)%RX(ID)=REAL(UTL_IDATETOJDATE(IDATE))
    GRAPH(JQOU,IG)%RX(ID)=REAL(UTL_IDATETOJDATE(IDATE))
   ELSE
    GRAPH(JQIN,IG)%RX(ID)=REAL(ID)
    GRAPH(JQOU,IG)%RX(ID)=REAL(ID)
    GRAPHDIM%XTXT(ID)=GWBAL(1)%CDATE(I)
   ENDIF

  ENDDO
 ENDDO
 
 DEALLOCATE(CMDATE,CMLAY,CMZONE)
 
 WBAL_ANAYSE_PREPARE=.TRUE.

 END FUNCTION WBAL_ANAYSE_PREPARE

 !###======================================================================
 LOGICAL FUNCTION WBAL_ANALYSE_SELECT(CL,CZ,CD,IL,IZ,ID)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: CL,CZ,CD
 INTEGER,INTENT(OUT) :: IL,IZ,ID
  
 WBAL_ANALYSE_SELECT=.TRUE.
 
 DO ID=1,MDATE
  !## date selected
  IF(TRIM(CMDATE(ID)).EQ.TRIM(CD))THEN
   DO IL=1,NLAY
    !## layer selected
    IF(TRIM(CMLAY(IL)).EQ.TRIM(CL))THEN
     DO IZ=1,NZONE
      !## zone selected
      IF(TRIM(CMZONE(IZ)).EQ.TRIM(CZ))RETURN
     ENDDO
    ENDIF
   ENDDO
  ENDIF
 ENDDO
 
 WBAL_ANALYSE_SELECT=.FALSE.
 
 END FUNCTION WBAL_ANALYSE_SELECT

 !###======================================================================
 LOGICAL FUNCTION WBAL_ANALYSE_TABLE(CSVFNAME)   
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: CSVFNAME
 INTEGER :: IGRIDWIN

!
!      PROGRAM GRIDWIN
!!
!      USE WINTERACTER
!      USE RESID
!!
!      IMPLICIT NONE
!!
!      INTEGER            :: IGRIDWIN
!      INTEGER            :: ITYPE
!      TYPE (WIN_MESSAGE) :: MESSAGE
!      CHARACTER(LEN=260) :: FILENAME = "" ! Initial filename
!!
!!  Intialise Winteracter and open a hidden root window. We require only the
!!  grid window and this must be a child window.
!!
!      CALL WInitialise()
!      CALL WindowOpen(HideWindow)
!!
!!  Check command line for a filename.
!!
!      CALL IOsArgument(1,FILENAME)
!!
!!  Open child window to be converted into a grid window.
!!  The flags specified for this window control certain aspects of the grid,
!!  such as the presence of a Status Bar. Opening the window initially hidden
!!  gives a smoother effect if the window is resized to limit it to the size
!!  of the grid when WGridEdit is called.
!!
!      CALL WindowOpenChild(IGRIDWIN,                            &
!                           FLAGS=SysMenuOn+MinButton+MaxButton+ &
!                                 StatusBar+HideWindow,          &
!                           TITLE='Winteracter grid window')
!!
!!  Load dialog containing template grid used to determine column layout, etc.
!!  of grid window.
!!
!      CALL WDialogLoad(IDD_DIALOG001)
!!
!!  Convert child window into a grid window.
!!  Append a menu to the grid window's build in menu.
!!  Load specified file. If FILENAME is blank then any contents from the
!!  template grid (in this case none) are used.
!!
!      CALL WGridEdit(IDF_GRID1,FILENAME,Modeless,IDM_MENU1)
!!
!!  Dialog containing template grid can now be unloaded.
!!
!      CALL WDialogUnload()
!!
!!  Main message loop.
!!
!      DO
!        CALL WMessage(ITYPE,MESSAGE)
!        SELECT CASE(ITYPE)
!          CASE (CloseRequest)
!!
!!  Close request. This is also reported if the Exit option is chosen from the
!!  grid window's File menu. Any prompts to save or abandon changes will have
!!  been displayed and confirmed before this message is reported,
!!
!            EXIT
!          CASE (MenuSelect)
!!
!!  Menu selection. These report the selection of options on the grid window's
!!  in built menu which are not reported by any other mechanism. For example the
!!  options on the Search menu are not reported since the result of these is
!!  already detectable via the FieldChanged message. Built in options are
!!  reported for information only - the corresponding action has already been
!!  performed.
!!
!!  Menu selections are also reported for additional options added in the
!!  WGridEdit call. In this case we will use this mechanism to display an
!!  About box.
!!
!            IF (MESSAGE%VALUE1 == ID_HELP_ABOUT) THEN
!                CALL WDialogLoad(IDD_ABOUT)
!                CALL WDialogShow(ITYPE=Modal)
!                CALL WDialogUnload()
!            END IF
!        END SELECT
!      END DO
!!
!!  Close windows and exit
!!
!      CALL WindowClose()
!!
!      STOP
!      END PROGRAM GRIDWIN
 
! CALL WINDOWOPENCHILD(IGRIDWIN,FLAGS=HIDEWINDOW, &
!                      TITLE='EXAMPLE GRID WINDOW')
! CALL WGRIDEDIT(CSVFNAME,IDF_GRID1,MODAL)

 END FUNCTION WBAL_ANALYSE_TABLE

 !###======================================================================
 LOGICAL FUNCTION WBAL_ANALYSE_EXPORTCSV(CSVFNAME)   
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: CSVFNAME
 CHARACTER(LEN=256) :: FNAME
 CHARACTER(LEN=16) :: CDATE
 INTEGER :: IU,IOS,IG,I,J
  
 WBAL_ANALYSE_EXPORTCSV=.FALSE.
 
 IF(LEN_TRIM(CSVFNAME).EQ.0)THEN
  FNAME=''
  IF(.NOT.UTL_WSELECTFILE('Save Comma Separated File (*.csv)|*.csv|',&
                   SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                   'Save Comma Separated File (*.csv)'))RETURN
 ELSE
  FNAME=CSVFNAME
 ENDIF
 
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot CREATE file called:'//CHAR(13)//&
    TRIM(FNAME),'Error')
  RETURN
 ENDIF

 DO IG=1,MGROUP 
  WRITE(IU,'(A)') GRAPHNAMES(IG)
  WRITE(IU,'(/999A14/)') 'Period',(','//TRIM(GRAPH(J,IG)%LEGTXT),J=1,MBUDGET)
  DO I=1,MDATE
   IF(ASSOCIATED(GRAPHDIM%XTXT))THEN
    CDATE=GRAPHDIM%XTXT(I)
   ELSE
    WRITE(CDATE,*) UTL_JDATETOIDATE(INT(GRAPH(J,IG)%RX(I)))
   ENDIF
   WRITE(IU,'(999A14)') TRIM(CDATE),(','//TRIM(RTOS(REAL(GRAPH(J,IG)%RY(I)),'G',7)),J=1,MBUDGET)
  ENDDO
 ENDDO

 CLOSE(IU)
 
 WBAL_ANALYSE_EXPORTCSV=.TRUE.
  
 END FUNCTION WBAL_ANALYSE_EXPORTCSV
 
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

 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB4)
 CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONOPEN,1)

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
!   IF(ASSOCIATED(GWBAL(I)%FLX))  DEALLOCATE(GWBAL(I)%FLX)
!   IF(ASSOCIATED(GWBAL(I)%ICLR)) DEALLOCATE(GWBAL(I)%ICLR)
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