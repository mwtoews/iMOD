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
USE IMODVAR, ONLY : IDIAGERROR,TP,ICPL
USE MOD_IDF, ONLY : IDFALLOCATEX,IDFALLOCATESXY,IDFNULLIFY,IDFDEALLOCATEX,IDFFILLSXSY,UTL_READ_FREE
USE MOD_WBAL_PAR
USE MOD_WBAL_GRAPHICS, ONLY : DRAWBAL
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_CAP,UTL_IDATETOJDATE,UTL_JDATETOIDATE,ITOS,IDATETOGDATE,UTL_GDATE,UTL_GETUNIQUE_CHAR, &
            UTL_WSELECTFILE,RTOS,UTL_DEBUGLEVEL,UTL_GETUNIQUE_INT
!USE MOD_POLINT, ONLY : POL1LOCATEINT
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
     IF(.NOT.WBAL_ANALYSE_READCSV('g:\IMOD-MODELS\IBRAHYM_V2.0\DBASE_DLD\IMOD_USER\MODELS\IBRAHYM_V2.0.5_23\test_ns.csv'))CALL WBAL_ANALYSE_DEALLOCATE()
!     IF(.NOT.WBAL_ANALYSE_READCSV(''))CALL WBAL_ANALYSE_DEALLOCATE()
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
 INTEGER :: IROW,ICOL,IRGB,I,J,IOS,IDATE
 
 CALL WDIALOGSELECT(MESSAGE%WIN)
 
 SELECT CASE (ITYPE)
 
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDF_SELECTALL)
     BUDGET%IACT=1; CALL WGRIDPUTCHECKBOX(IDF_GRID1,1,BUDGET%IACT,SIZE(BUDGET))
    CASE (IDF_DESELECTALL)
     BUDGET%IACT=0; CALL WGRIDPUTCHECKBOX(IDF_GRID1,1,BUDGET%IACT,SIZE(BUDGET))
    CASE (IDF_SELECTALLDATES)
     LIDATE=1; CALL WDIALOGPUTMENU(IDF_MENU1,CIDATE,NDATE,LIDATE)
! CALL WDIALOGPUTMENU(IDF_MENU1,CIDATE,NDATE,LIDATE)
    CASE (IDF_DESELECTALLDATES)
     LIDATE=0; CALL WDIALOGPUTMENU(IDF_MENU1,CIDATE,NDATE,LIDATE)
!    CASE (IDCANCEL)
!    CASE (IDHELP)
!    CASE (IDOK)
   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
    !## select period - check whether aggregration in time is possible - only for true dates
    CASE (IDF_MENU1)
     CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB2)
     CALL WDIALOGGETMENU(IDF_MENU1,LIDATE)
     DO I=1,NDATE
      IF(LIDATE(I).EQ.0)CYCLE
      READ(CIDATE(I),*,IOSTAT=IOS) IDATE
      IF(IOS.NE.0)EXIT
     ENDDO
     J=1; IF(I.LE.NDATE)J=0
     CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB3)
     CALL WDIALOGFIELDSTATE(IDF_RADIO5 ,J)
     CALL WDIALOGFIELDSTATE(IDF_RADIO6 ,J)
     CALL WDIALOGFIELDSTATE(IDF_RADIO7 ,J)
     CALL WDIALOGFIELDSTATE(IDF_RADIO8 ,J)
     CALL WDIALOGFIELDSTATE(IDF_RADIO9 ,J)
     CALL WDIALOGFIELDSTATE(IDF_RADIO10,J)
     CALL WDIALOGFIELDSTATE(IDF_RADIO11,J)
     CALL WDIALOGFIELDSTATE(IDF_RADIO12,J)
     
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
    CASE (IDF_SELECTALLLAYERS)
     LILAY=1; CALL WDIALOGPUTMENU(IDF_MENU1,CILAY,NLAY,LILAY)
    CASE (IDF_DESELECTALLLAYERS)
     LILAY=0; CALL WDIALOGPUTMENU(IDF_MENU1,CILAY,NLAY,LILAY)
    CASE (IDF_SELECTALLZONES)
     LIZONE=1; CALL WDIALOGPUTMENU(IDF_MENU2,CIZONE,NZONE,LIZONE)
    CASE (IDF_DESELECTALLZONES)
     LIZONE=0; CALL WDIALOGPUTMENU(IDF_MENU2,CIZONE,NZONE,LIZONE)
!    CASE (IDCANCEL)
!    CASE (IDHELP)
!    CASE (IDOK)
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
 INTEGER :: I,J,K,L
 
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
    CASE (IDF_RADIO1,IDF_RADIO2,IDF_RADIO3,IDF_RADIO4,IDF_RADIO5)
     CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
     SELECT CASE (I)
      !## timeseries
      CASE (1); J=1; K=1; L=0
      !## graphics
      CASE (2); J=1; K=1; L=1
      !## table
      CASE (3); J=1; K=0; L=0
      !## export csv
      CASE (4); J=0; K=1; L=0
      !## idf
      CASE (5); J=0; K=1; L=0
     END SELECT
     CALL WDIALOGFIELDSTATE(ID_PREVIEW,J)
     CALL WDIALOGFIELDSTATE(ID_GRAPHICS,K)
     CALL WDIALOGFIELDSTATE(IDF_CHECK1,L)
     CALL WDIALOGFIELDSTATE(IDF_CHECK2,L)
   END SELECT
 END SELECT
     
 END SUBROUTINE WBAL_ANALYSE_TAB4

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_TAB5(ITYPE,MESSAGE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 
 CALL WDIALOGSELECT(MESSAGE%WIN)
 
 SELECT CASE (ITYPE)
 
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
    CASE (IDF_MENU1)
     CALL WBAL_ANALYSE_TABLE_FILL()
   END SELECT
 END SELECT
     
 END SUBROUTINE WBAL_ANALYSE_TAB5

 !###======================================================================
 LOGICAL FUNCTION WBAL_ANALYSE_READCSV(FNAME) 
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=512) :: LINE
 CHARACTER(LEN=256) :: CSVFNAME
 CHARACTER(LEN=52) :: TXT
 INTEGER :: I,J,K,II,IU,IOS,SKIPLINES,CFN_N_ELEM,IBAL
 INTEGER :: NV,NL
 
 WBAL_ANALYSE_READCSV=.TRUE.
 
 IF(LEN_TRIM(FNAME).EQ.0)THEN
  CSVFNAME=''
  IF(.NOT.UTL_WSELECTFILE('Load Comma Separated File (*.csv)|*.csv|',&
                   LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,CSVFNAME,&
                   'Load Comma Separated File (*.csv)'))RETURN
 ELSE
  CSVFNAME=FNAME !'d:\iMOD-Gebruikersdag\IMOD_USER\MODELS\WERKHOVEN\WBALANCE.CSV'
 ENDIF
 
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
 NV=CFN_N_ELEM(',;',2,LINE); NV=NV-4
 
 !## allocate object, gwbal(1) is original gwbal(2) is selection to be processed as plot/graph
 CALL WBAL_ANALYSE_DEALLOCATE(); ALLOCATE(GWBAL(2))

 WBAL_ANALYSE_READCSV=.FALSE.

 NBUDGET=NV/2
 !## read labels of budgetterms
 ALLOCATE(GWBAL(1)%TXT(NV),GWBAL(2)%TXT(NV))
 READ(LINE,*) TXT,TXT,TXT,TXT,(GWBAL(1)%TXT(I),I=1,NV)
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
!   !## plotgroup
!   BUDGET(J)%IPLTGRP=WBAL_ANALYSE_GETPLTIGRP(BUDGET(IBAL)%ACRN)
  ELSE
!   IF(GWBAL(1)%TXT(I).EQ.'BDGFUF')IBAL=10
!## insert appropriate budget number for fuf, ...
   BUDGET(J)%LABEL=GWBAL(1)%TXT(I)
!   BUDGET(J)%IBAL=0 !IBAL
  ENDIF
!  BUDGET(J)%FILE=GWBAL(1)%TXT(I)
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
 ALLOCATE(GWBAL(1)%CDATE(NL),GWBAL(1)%Q(NV,NL),GWBAL(1)%CLAY(NL),GWBAL(1)%CZONE(NL),GWBAL(1)%AREA(NL))

 REWIND(IU)
 
 !## skip header
 DO I=1,SKIPLINES+2; READ(IU,'(A)') LINE; ENDDO
 
 !## read data entire data from csv
 DO I=1,NL
  READ(IU,*) GWBAL(1)%CDATE(I),GWBAL(1)%CLAY(I),GWBAL(1)%CZONE(I),GWBAL(1)%AREA(I),(GWBAL(1)%Q(J,I),J=1,NV)
 ENDDO

 !## try to read in zone-idf
 CALL IDFNULLIFY(IDFP)

 !## look for network dimensions
 DO I=1,3; READ(IU,*); ENDDO
 IF(.NOT.UTL_READ_FREE(IU,IDFP,'T'))RETURN
 !## fill in sx,sy variables
 IF(.NOT.IDFFILLSXSY(IDFP))RETURN
 
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
 INTEGER :: I,J
  
 WBAL_ANALYSE_GETBALANCETERM=0
  
 J=INDEX(TXT,'_SYS'); IF(J.EQ.0)J=LEN_TRIM(TXT)+1; J=J-1
 
 DO I=1,SIZE(TP)
  IF(TRIM(UTL_CAP(TXT(1:J),'U')).EQ.TRIM(UTL_CAP(TP(I)%ACRNM,'U')))EXIT
 ENDDO
 IF(I.LE.SIZE(TP))WBAL_ANALYSE_GETBALANCETERM=I

 END FUNCTION WBAL_ANALYSE_GETBALANCETERM

 !###======================================================================
 INTEGER FUNCTION WBAL_ANALYSE_GETQCATEGORY(FLUXTERM)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FLUXTERM
 CHARACTER(LEN=52) :: FTERM
 INTEGER :: J
  
 WBAL_ANALYSE_GETQCATEGORY=0
  
 FTERM=UTL_CAP(FLUXTERM,'U')
 J=INDEX(FTERM,'_SYS'); IF(J.EQ.0)J=LEN_TRIM(FTERM)+1; J=J-1
 !## overrule for connected flows
 IF(INDEX(FTERM,'BDGFCF').GT.0)J=6
 
 SELECT CASE (FTERM(1:J))
  CASE ('BDGDRN');    WBAL_ANALYSE_GETQCATEGORY= 1
  CASE ('BDGOLF');    WBAL_ANALYSE_GETQCATEGORY= 2
  CASE ('BDGRIV');    WBAL_ANALYSE_GETQCATEGORY= 3
  CASE ('BDGGHB');    WBAL_ANALYSE_GETQCATEGORY= 4
  CASE ('BDGISG');    WBAL_ANALYSE_GETQCATEGORY= 5
  CASE ('BDGWEL');    WBAL_ANALYSE_GETQCATEGORY= 6
  CASE ('BDGFRF', &
        'BDGFFF');    WBAL_ANALYSE_GETQCATEGORY= 7
  CASE ('BDGBND');    WBAL_ANALYSE_GETQCATEGORY= 8
  CASE ('BDGFLF');    WBAL_ANALYSE_GETQCATEGORY= 9
  CASE ('BDGFTF');    WBAL_ANALYSE_GETQCATEGORY=10
  CASE ('BDGRCH');    WBAL_ANALYSE_GETQCATEGORY=11
  CASE ('BDGEVT');    WBAL_ANALYSE_GETQCATEGORY=12
  CASE ('BDGCAP');    WBAL_ANALYSE_GETQCATEGORY=13
  CASE ('BDGETACT');  WBAL_ANALYSE_GETQCATEGORY=14
  CASE ('BDGPM');     WBAL_ANALYSE_GETQCATEGORY=15
  CASE ('BDGPSGW');   WBAL_ANALYSE_GETQCATEGORY=16
  CASE ('BDGPSSW');   WBAL_ANALYSE_GETQCATEGORY=17
  CASE ('BDGSTO');    WBAL_ANALYSE_GETQCATEGORY=18
  CASE ('BDGDECSTO'); WBAL_ANALYSE_GETQCATEGORY=19
  CASE ('BDGQSPGW');  WBAL_ANALYSE_GETQCATEGORY=20
  CASE ('BDGQCOR');   WBAL_ANALYSE_GETQCATEGORY=21
  CASE ('BDGQDR');    WBAL_ANALYSE_GETQCATEGORY=22
  CASE ('BDGQRUN');   WBAL_ANALYSE_GETQCATEGORY=23
  CASE ('BDGQMODF');  WBAL_ANALYSE_GETQCATEGORY=24
  CASE ('BDGFCF');    WBAL_ANALYSE_GETQCATEGORY=-1

 END SELECT

! DATA ICPL   /08, & !## 01 drn
!              12, & !## 02 olf
!              09, & !## 03 riv
!              11, & !## 04 ghb
!              14, & !## 05 isg
!              07, & !## 06 wel
!              00, & !## 07 reg ---?
!              02, & !## 08 chh
!              03, & !## 09 flf 
!              00, & !## 10 fuf ---?
!              13, & !## 11 rch
!              10, & !## 12 evt
!              15, & !## 13 cap
!              13, & !## 14 etact
!              00, & !## 15 pm ---?
!              24, & !## 16 pmgw (bdgpsgw)
!              35, & !## 17 pmsw (bdgpssw)
!              06, & !## 18 sto
!              34, & !## 19 decsto
!              30, & !## 20 bdgqspgw
!              00, & !## 21 qcor 
!              00, & !## 22 qdr (bdgqdr)
!              00, & !## 23 qrun 
!              00/ !## 24 qmodf

 END FUNCTION WBAL_ANALYSE_GETQCATEGORY

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
 INTEGER :: IOPT,IG,I1,I2
 
 !## get option from the window to determine what to do
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB4)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IOPT)

 !## get the appropriate selection
 IF(.NOT.WBAL_ANAYSE_PREPARE(IOPT))RETURN

 SELECT CASE (IOPT)
  !## timeseries
  CASE (1)
!   CALL GRAPH_PLOT('Time','Volumes (m3/d)',LDATE) !.TRUE.)
   CALL GRAPH_PLOT('Time','Volumes (m3/d)',.FALSE.)

! IF(ID.EQ.ID_GRAPHICS)THEN
! ELSEIF(ID.EQ.ID_PREVIEW)THEN
!  CALL GRAPH_PLOT('Time','Volumes (m3/d)',.FALSE.)
! ENDIF

  !## graph
  CASE (2)
   CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB4)
   CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I1)
   CALL WDIALOGGETCHECKBOX(IDF_CHECK2,I2)
   DO IG=1,SIZE(GRAPHNAMES)
    CALL WBAL_ANALYSE_PLOTIMAGE(ID,IG,I1,I2)
   ENDDO
  !## table
  CASE (3)
   IF(WBAL_ANALYSE_TABLE())THEN
    CALL WDIALOGSELECT(ID_DWBAL_ANALYSE)
    CALL WDIALOGSETTAB(IDF_TAB1,ID_DWBAL_ANALYSE_TAB5)
   ENDIF

  !## save to csv
  CASE (4)
   IF(WBAL_ANALYSE_EXPORTCSV(''))THEN; ENDIF

  CASE (5)
 END SELECT
 
! !## clean up, deallocate
! IF(ALLOCATED(GRAPH))CALL GRAPH_DEALLOCATE()

 END SUBROUTINE WBAL_ANALYSE_PLOT

 !###======================================================================
 LOGICAL FUNCTION WBAL_ANAYSE_PREPARE(IOPT) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPT
 INTEGER :: I,J,K,IL,IZ,ID,IG,IQIN,IQOU,JQIN,JQOU,IQ,IIQ,JQ,IOS,IDATE,ITTYPE,IHIT, &
     IAG,NXG,IY1,IM1,ID1,IY2,IM2,ID2,IPOS,SDATE,IB,LSUM,ZSUM,N,INET,IUNIT,NUQ,PQ
 REAL,POINTER,DIMENSION(:) :: DT
 CHARACTER(LEN=16),POINTER,DIMENSION(:) :: XTXT=>NULL()
 CHARACTER(LEN=16) :: TXTL,TXTZ
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IDATES,SQ,UQ,OSQ,CLILAY,CLIZONE
 REAL :: QIN,QOU
  
 WBAL_ANAYSE_PREPARE=.FALSE.
 
 IF(ALLOCATED(GRAPH))CALL GRAPH_DEALLOCATE()

 !## get selected dates
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB2)
 CALL WDIALOGGETMENU(IDF_MENU1,LIDATE)
 !## select m3/d or mmd
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IUNIT)
 
 !## get selected budgets
 DO I=1,SIZE(BUDGET)
  CALL WGRIDGETCELLCHECKBOX(IDF_GRID1,1,I,BUDGET(I)%IACT)
  CALL WGRIDGETCELLSTRING(  IDF_GRID1,2,I,BUDGET(I)%FLUXTERM)
  CALL WGRIDGETCELLINTEGER( IDF_GRID1,3,I,BUDGET(I)%ICLR)
  CALL WGRIDGETCELLSTRING(  IDF_GRID1,4,I,BUDGET(I)%LABEL)
  CALL WGRIDGETCELLINTEGER( IDF_GRID1,5,I,BUDGET(I)%IGROUP)
 ENDDO

 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB3)
 !## get selected layers
 CALL WDIALOGGETMENU(IDF_MENU1,LILAY)
 !## sum layers
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,LSUM); LSUM=LSUM-1
 !## get selected zones
 CALL WDIALOGGETMENU(IDF_MENU2,LIZONE)
 !## sum zones
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,ZSUM); ZSUM=ZSUM-1
 !## check whether selectable
 IAG=0; IF(WINFODIALOGFIELD(IDF_RADIO5,FIELDSTATE).EQ.1)CALL WDIALOGGETRADIOBUTTON(IDF_RADIO5,IAG)
 !## apply net fluxes
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,INET)
! !## net balance terms if selected = inet=0 otherwise inet=1
! INET=ABS(INET-1)
 
 !## make selection
 MLAY=0;    DO I=1,NLAY;    IF(LILAY(I).EQ.1)      MLAY=MLAY+1;       ENDDO
 MZONE=0;   DO I=1,NZONE;   IF(LIZONE(I).EQ.1)     MZONE=MZONE+1;     ENDDO
 MDATE=0;   DO I=1,NDATE;   IF(LIDATE(I).EQ.1)     MDATE=MDATE+1;     ENDDO
 MBUDGET=0; DO I=1,NBUDGET; IF(BUDGET(I)%IACT.EQ.1)MBUDGET=MBUDGET+1; ENDDO
 
 !## get appropriate sort
 ALLOCATE(SQ(MBUDGET),UQ(MBUDGET),OSQ(MBUDGET))
 IQ=0; DO I=1,MBUDGET
  IF(BUDGET(I)%IACT.EQ.0)CYCLE
  IQ=IQ+1
  !## do not use group number for the graphical representation
  IF(IOPT.EQ.2)THEN
   UQ(IQ)=IQ !BUDGET(I)%IGROUP
  ELSE
   UQ(IQ)=BUDGET(I)%IGROUP
  ENDIF
 ENDDO
 SQ=UQ
 
 !## get number of unique budget terms (groups)
 CALL UTL_GETUNIQUE_INT(UQ,MBUDGET,NUQ,-9999)

 !## it is not allowed to aggregrate budgetterms for graphical representations
 IF(IOPT.EQ.2)THEN
  IF(NUQ.NE.MBUDGET)THEN
   DEALLOCATE(SQ,UQ,OSQ)
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'It is not allowed to aggregrate budget terms for the graphical representation','Information')
   RETURN
  ENDIF
 ENDIF
 
 !## get appropriate sort order osq
 CALL WSORT(SQ,1,NBUDGET,IORDER=OSQ)

 !## number of budget in the graphs (unique groups times 2 - in- and outflow)
 IF(INET.EQ.0)THEN
  MBUDGET=NUQ*2
 ELSE
  !## net fluxes
  MBUDGET=NUQ
 ENDIF
 
 !## total groups in potential
 MGROUP=MZONE*MLAY

 IF(MGROUP.EQ.0.OR.MBUDGET.EQ.0.OR.MDATE.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'There is nothing selected to generate a water balance for.'//CHAR(13)// &
    'Go to the tabs BUDGET TERMS and AGGREGATION to make an appropriate selection.','Information')
  RETURN
 ENDIF
 
 !## allocate memory for selected items
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

 !## get number of years, months, decades (mindate/maxdate)
 IF(ITTYPE.EQ.0)THEN
  
  READ(CMDATE(1),*)            IDATE; CALL IDATETOGDATE(IDATE,IY1,IM1,ID1)
  READ(CMDATE(SIZE(CMDATE)),*) IDATE; CALL IDATETOGDATE(IDATE,IY2,IM2,ID2)
  
  !## define nxg, dt, sdate en xtxt for aggregration
  SELECT CASE (IAG)
   !## all
   CASE (1); NXG=WBAL_ANAYSE_PREPARE_GETALL(IY1,IM1,ID1,IY2,IM2,ID2,SDATE,DT,XTXT)
   !## year
   CASE (2); NXG=WBAL_ANAYSE_PREPARE_GETYEAR(IM1,ID1,IY1,IY2,SDATE,DT,XTXT)
   !## months
   CASE (3); NXG=WBAL_ANAYSE_PREPARE_GETMONTHS(IY1,IM1,IY2,IM2,SDATE,DT,XTXT)
   !## hydrologic seasons
   CASE (4); NXG=WBAL_ANAYSE_PREPARE_GETHSEASON(IY1,IM1,IY2,IM2,SDATE,DT,XTXT)
   !## decade
   CASE (5); NXG=WBAL_ANAYSE_PREPARE_GETDECADES(IY1,IM1,ID1,IY2,IM2,ID2,SDATE,DT,XTXT)
   !## hydrological year
   CASE (6); NXG=WBAL_ANAYSE_PREPARE_GETHYEAR(IY1,IM1,IY2,IM2,SDATE,DT,XTXT)
   !## quarters
   CASE (7); NXG=WBAL_ANAYSE_PREPARE_GETQUARTERS(IY1,IM1,IY2,IM2,SDATE,DT,XTXT)
   !## none - true dates in the result array
   CASE (8); NXG=WBAL_ANAYSE_PREPARE_GETSELDATES(CMDATE,SDATE,DT,XTXT) 
  END SELECT
  
  ALLOCATE(IDATES(NXG+1)); IDATES=0

 ELSE

  !## steady-state (or equivalent defined by periods)
  NXG=J; ALLOCATE(IDATES(NXG)); IDATES=0
  ALLOCATE(DT(NXG),XTXT(NXG))
  DO I=1,NXG; DT(I)=1.0; XTXT(I)=CMDATE(I); ENDDO
  
 ENDIF
 
 !## if zones need to be summed, overrule number of zones
 I=MZONE; IF(ZSUM.EQ.1)I=1
 !## if layers need to be summed, overrule number of layers
 J=MLAY; IF(LSUM.EQ.1)J=1
 !## total groups in graph
 MGROUP=I*J 

 !## allocate graph memory
 CALL GRAPH_ALLOCATE(MBUDGET,MGROUP)
 DO I=1,MBUDGET; DO J=1,MGROUP
  ALLOCATE(GRAPH(I,J)%RX(NXG),GRAPH(I,J)%RY(NXG))
  GRAPH(I,J)%RX=0.0; GRAPH(I,J)%RY=0.0; GRAPH(I,J)%NP=NXG; GRAPH(I,J)%CTYPE=''; GRAPH(I,J)%ICLR=0
 ENDDO; ENDDO

 !## add custom predefined axes titles
 GRAPHDIM%IFIXX=0
 GRAPHDIM%IFIXY=0
 ALLOCATE(GRAPHDIM%XTXT(NXG),GRAPHDIM%XPOS(NXG)); GRAPHDIM%XTXT=''; GRAPHDIM%XPOS=0.0; GRAPHDIM%IFIXX=1
 
 !### use dt to create rdates array
 CALL WBAL_ANALYSE_PREPARERDATES(SDATE,DT,XTXT,IDATES)
 
 !## set axes dimensions
 GRAPHDIM%XINT=1.0; GRAPHDIM%XMIN=GRAPHDIM%XPOS(1)-1.0; GRAPHDIM%XMAX=GRAPHDIM%XPOS(NXG)+1.0

 !## remove memory of dt
 IF(ASSOCIATED(DT))  DEALLOCATE(DT)
 IF(ASSOCIATED(XTXT))DEALLOCATE(XTXT)

 !## all stacked histograms
 GRAPH%GTYPE =3

 !## copy selected layers/zones for summing purposes
 ALLOCATE(CLILAY(NLAY));   DO I=1,NLAY;  CLILAY(I) =LILAY(I);  ENDDO
 ALLOCATE(CLIZONE(NZONE)); DO I=1,NZONE; CLIZONE(I)=LIZONE(I); ENDDO
 !## turn layers/zones and active only one for summing purposes
 IF(LSUM.EQ.1)THEN; CLILAY=0;  CLILAY(1)=1;  ENDIF
 IF(ZSUM.EQ.1)THEN; CLIZONE=0; CLIZONE(1)=1; ENDIF

 !## assign label name for each group (= zone and layer)
 K=0; DO I=1,NLAY; DO J=1,NZONE
  IF(CLILAY(I).EQ.0)CYCLE
  IF(CLIZONE(J).EQ.0)CYCLE
  K=K+1
  TXTL='Layer '//TRIM(CILAY(I)); IF(LSUM.EQ.1)TXTL='Layer [sum]'
  TXTZ='Zone '//TRIM(CIZONE(J)); IF(ZSUM.EQ.1)TXTZ='Zone [sum]'
  GRAPHNAMES(K)=TRIM(TXTL)//'; '//TRIM(TXTZ) 
 ENDDO; ENDDO
 
 !## gather main information
 JQ=0; DO IIQ=1,SIZE(OSQ) !MBUDGET !NBUDGET
 
  !## function of group number and therefore defines sort-order
  IQ=OSQ(IIQ) !; IF(BUDGET(IQ)%IACT.EQ.0)CYCLE
  !## previous budget terms
  IF(IIQ.GT.1)THEN
   PQ=OSQ(IIQ-1); IF(BUDGET(PQ)%IGROUP.NE.BUDGET(IQ)%IGROUP)JQ=JQ+1
  ELSE
   !## increase only whenever different group starts
   JQ=JQ+1
  ENDIF
  
  IG=0; DO I=1,NLAY; DO J=1,NZONE
   IF(CLILAY(I).EQ.0)CYCLE
   IF(CLIZONE(J).EQ.0)CYCLE
  
   IG=IG+1
  
   IF(INET.EQ.0)THEN
    JQIN=(JQ-1)*2+1
    JQOU= JQIN+1
   ELSE
    JQIN=(JQ-1)+1   
    JQOU= JQIN
   ENDIF
   
   !## plot legend one-by-one
   GRAPH(JQOU,IG)%LEGTXT='' 
   GRAPH(JQIN,IG)%LEGTXT=TRIM(UTL_CAP(BUDGET(IQ)%LABEL,'U'))

   GRAPH(JQOU,IG)%ICLR  =BUDGET(IQ)%ICLR
   GRAPH(JQIN,IG)%ICLR  =BUDGET(IQ)%ICLR

   IF(TRIM(GRAPH(JQIN,IG)%CTYPE).EQ.'')THEN
    GRAPH(JQOU,IG)%CTYPE =TRIM(UTL_CAP(BUDGET(IQ)%FLUXTERM,'U'))//'_out'
    GRAPH(JQIN,IG)%CTYPE =TRIM(UTL_CAP(BUDGET(IQ)%FLUXTERM,'U'))//'_in'
   ELSE
    GRAPH(JQOU,IG)%CTYPE ='group flux out'
    GRAPH(JQIN,IG)%CTYPE ='group flux in'
   ENDIF
     
  ENDDO; ENDDO
 ENDDO

 !## fill in in advance the horizontal position
 DO I=1,SIZE(GRAPH,1); DO J=1,SIZE(GRAPH,2)
  DO IPOS=1,SIZE(GRAPH(I,J)%RX)
   GRAPH(I,J)%RX(IPOS)=REAL(IPOS)
   GRAPH(I,J)%RX(IPOS)=REAL(IPOS)
  ENDDO
 ENDDO; ENDDO
 
 !## gather data
 IHIT=0; DO I=1,NRECORDS
  
  !## appropriate item
  IF(.NOT.WBAL_ANALYSE_SELECT(GWBAL(1)%CLAY(I),GWBAL(1)%CZONE(I),GWBAL(1)%CDATE(I),IL,IZ,ID,IAG,IPOS,IDATES))CYCLE

!  !## if sum overwrite ID to be one all the times
!  IF()THEN
!   ID=1
!  ENDIF
  
  !## sum for layers
  N=MLAY; IF(LSUM.EQ.1)IL=1; N=1
  !## sum for zones
  IF(ZSUM.EQ.1)IZ=1

  !## get appropriate group number
  IG=(IZ-1)*N+IL

  JQIN=-1; JQOU=0; JQ=0
  DO IIQ=1,SIZE(OSQ) !MBUDGET !NBUDGET

   !## function of group number and therefore defines sort-order
   IQ=OSQ(IIQ) !; IF(BUDGET(IQ)%IACT.EQ.0)CYCLE
   !## previous budget terms
   IF(IIQ.GT.1)THEN
    PQ=OSQ(IIQ-1); IF(BUDGET(PQ)%IGROUP.NE.BUDGET(IQ)%IGROUP)JQ=JQ+1
   ELSE
    !## increase only whenever different group starts
    JQ=JQ+1
   ENDIF

   IHIT=IHIT+1
   
   IF(INET.EQ.0)THEN
    JQIN=(JQ-1)*2+1
    JQOU= JQIN+1
   ELSE
    JQIN=(JQ-1)+1
    JQOU= JQIN
   ENDIF

   IQIN=(IQ-1)*2+1
   IQOU= IQIN+1
   
   !## get balance value as summed value (stacked)
   QIN=GWBAL(1)%Q(IQIN,I)
   QOU=GWBAL(1)%Q(IQOU,I)
  
   !## add to existing fluxes - ID is timestep
   GRAPH(JQIN,IG)%RY(IPOS)=GRAPH(JQIN,IG)%RY(IPOS)+QIN
   GRAPH(JQOU,IG)%RY(IPOS)=GRAPH(JQOU,IG)%RY(IPOS)+QOU

  ENDDO
 ENDDO
 
 DEALLOCATE(CMDATE,CMLAY,CMZONE,IDATES,SQ,UQ,OSQ,CLILAY,CLIZONE)
 
 IF(IOPT.EQ.1)THEN
  !## make stacked-histograms
  DO ID=1,NXG
   DO IG=1,MGROUP; DO IB=3,MBUDGET,2
    !## in
    GRAPH(IB  ,IG)%RY(ID)=GRAPH(IB  ,IG)%RY(ID)+GRAPH(IB-2,IG)%RY(ID)
    !## out
    GRAPH(IB+1,IG)%RY(ID)=GRAPH(IB+1,IG)%RY(ID)+GRAPH(IB-1,IG)%RY(ID)
   ENDDO; ENDDO
  ENDDO
 ENDIF
 
 IF(IHIT.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'There remains nothing to generate a water balance for.'//CHAR(13)// &
    'Go to the tabs BUDGET TERMS and AGGREGATION to make an appropriate selection.','Information')
  RETURN
 ENDIF
 
 WBAL_ANAYSE_PREPARE=.TRUE.

 END FUNCTION WBAL_ANAYSE_PREPARE

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_PREPARERDATES(SDATE,DT,XTXT,IDATES)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: SDATE
 INTEGER,INTENT(OUT),DIMENSION(:) :: IDATES
 REAL,POINTER,INTENT(IN),DIMENSION(:) :: DT
 CHARACTER(LEN=*),INTENT(OUT),DIMENSION(:),POINTER :: XTXT
 INTEGER :: I,J

 IDATES(1)=SDATE
 DO I=1,SIZE(DT)
  J=UTL_IDATETOJDATE(IDATES(I))+DT(I)
  IDATES(I+1)=UTL_JDATETOIDATE(J)
  GRAPHDIM%XTXT(I)=XTXT(I)
  GRAPHDIM%XPOS(I)=REAL(I) 
 ENDDO

 END SUBROUTINE WBAL_ANALYSE_PREPARERDATES

 !###======================================================================
 INTEGER FUNCTION WBAL_ANAYSE_PREPARE_GETSELDATES(CMDATE,SDATE,DT,XTXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: CMDATE
 REAL,INTENT(OUT),DIMENSION(:),POINTER :: DT
 CHARACTER(LEN=*),INTENT(OUT),DIMENSION(:),POINTER :: XTXT
 INTEGER,INTENT(OUT) :: SDATE
 INTEGER :: I,ND,IDATE,JDATE
 
 !## set start date
 READ(CMDATE(1),*) SDATE

 ND=SIZE(CMDATE); ALLOCATE(DT(ND),XTXT(ND)); DT=0.0; XTXT=''
 
 ND=0; DO I=1,SIZE(CMDATE)
  ND=ND+1
  READ(CMDATE(I),*) IDATE
  JDATE=IDATE; IF(I.LT.SIZE(CMDATE))READ(CMDATE(I+1),*) JDATE
  DT(I)  =(UTL_IDATETOJDATE(JDATE)-UTL_IDATETOJDATE(IDATE))+1.0
  XTXT(I)=TRIM(ITOS(IDATE))
 ENDDO
 
 WBAL_ANAYSE_PREPARE_GETSELDATES=ND
 
 END FUNCTION WBAL_ANAYSE_PREPARE_GETSELDATES

 !###======================================================================
 INTEGER FUNCTION WBAL_ANAYSE_PREPARE_GETALL(IY1,IM1,ID1,IY2,IM2,ID2,SDATE,DT,XTXT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IY1,IM1,ID1,IY2,IM2,ID2
 INTEGER,INTENT(OUT) :: SDATE
 REAL,INTENT(OUT),DIMENSION(:),POINTER :: DT
 CHARACTER(LEN=*),INTENT(OUT),DIMENSION(:),POINTER :: XTXT
 INTEGER :: EDATE
 
 !## set start date
 SDATE=IY1*10000+IM1*100+ID1
 !## set end date
 EDATE=IY2*10000+IM2*100+ID2
 
 ALLOCATE(DT(1),XTXT(1)); DT=0.0; XTXT=TRIM(ITOS(SDATE))//'-'//TRIM(ITOS(EDATE))
 DT(1)=(UTL_IDATETOJDATE(EDATE)-UTL_IDATETOJDATE(SDATE))+1.0
 
 WBAL_ANAYSE_PREPARE_GETALL=1
 
 END FUNCTION WBAL_ANAYSE_PREPARE_GETALL
 
 !###======================================================================
 INTEGER FUNCTION WBAL_ANAYSE_PREPARE_GETYEAR(IM1,ID1,IY1,IY2,SDATE,DT,XTXT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IY1,IY2,IM1,ID1
 INTEGER,INTENT(OUT) :: SDATE
 REAL,INTENT(OUT),DIMENSION(:),POINTER :: DT
 CHARACTER(LEN=*),INTENT(OUT),DIMENSION(:),POINTER :: XTXT
 INTEGER :: I,NY,IY

 !## set start date
 SDATE=IY1*10000+IM1*100+ID1

 !## get number of months
 NY=(IY2-IY1)+1

 !## define dt pointer
 ALLOCATE(DT(NY),XTXT(NY)); XTXT=''; DT=0.0
 IY=IY1; DO I=1,NY
  DT(I)=365.0; IF(WDATELEAPYEAR(IY))DT(I)=DT(I)+1.0
  XTXT(I)=TRIM(ITOS(IY)) !//'0000'
  IY=IY+1
 ENDDO
 
 WBAL_ANAYSE_PREPARE_GETYEAR=NY
 
 END FUNCTION WBAL_ANAYSE_PREPARE_GETYEAR

 !###======================================================================
 INTEGER FUNCTION WBAL_ANAYSE_PREPARE_GETHYEAR(IY1,IM1,IY2,IM2,SDATE,DT,XTXT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IY1,IM1,IY2,IM2
 INTEGER,INTENT(OUT) :: SDATE
 REAL,INTENT(OUT),DIMENSION(:),POINTER :: DT
 CHARACTER(LEN=*),INTENT(OUT),DIMENSION(:),POINTER :: XTXT
 INTEGER :: I,J,ND,IY,IM,ID,IDATE,EDATE,IYY,IMM

 !## four seasons 1/12 - 28/2
 !##              1/3  - 31/5
 !##              1/6  - 31/8
 !##              1/9  - 30/11

 !## set start date - clipped to first of month
 ID=1; IY=IY1
 IF(IM1.EQ.12.OR.IM1.LE.2)THEN
  IM=12; IF(IM1.LE.2)IY=IY-1
 ELSEIF(IM1.GE.3.AND.IM1.LE.5)THEN
  IM=3
 ELSEIF(IM1.GE.6.AND.IM1.LE.8)THEN
  IM=6
 ELSE
  IM=9
 ENDIF
 SDATE=IY*10000+IM*100+ID

 IYY=IY
 IMM=IM
 
 !## set end date - rounded to end of month
 IY=IY2
 IF(IM2.EQ.12.OR.IM2.LE.2)THEN
  IM=2; IF(IM2.EQ.12)IY=IY+1
 ELSEIF(IM2.GE.3.AND.IM2.LE.5)THEN
  IM=5
 ELSEIF(IM2.GE.6.AND.IM2.LE.8)THEN
  IM=8
 ELSE
  IM=11
 ENDIF
 ID=WDATEDAYSINMONTH(IY,IM); EDATE=IY*10000+IM*100+ID
 
 DO I=1,2

  IM=IMM; IY=IYY; ND=0
  DO
   ND=ND+1
   DO J=1,6
    IF(I.EQ.2)THEN
     DT(ND)=DT(ND)+WDATEDAYSINMONTH(IY,IM)
     IF(J.EQ.1)XTXT(ND)=TRIM(ITOS(IY))//'-'//TRIM(ITOS(IM))
    ENDIF
    IM=IM+1
    IF(IM.GT.12)THEN; IM=1; IY=IY+1; ENDIF
   ENDDO

   !## current date
   ID=WDATEDAYSINMONTH(IY,IM); IDATE=IY*10000+IM*100+ID
   
   !## stop
   IF(IDATE.GT.EDATE)EXIT

  ENDDO

  !## define dt pointer
  IF(I.EQ.1)THEN; ALLOCATE(DT(ND),XTXT(ND)); XTXT=''; DT=0.0; ENDIF
 
 ENDDO
 
 WBAL_ANAYSE_PREPARE_GETHYEAR=ND
 
 END FUNCTION WBAL_ANAYSE_PREPARE_GETHYEAR

 !###======================================================================
 INTEGER FUNCTION WBAL_ANAYSE_PREPARE_GETHSEASON(IY1,IM1,IY2,IM2,SDATE,DT,XTXT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IY1,IM1,IY2,IM2
 INTEGER,INTENT(OUT) :: SDATE
 REAL,INTENT(OUT),DIMENSION(:),POINTER :: DT
 CHARACTER(LEN=*),INTENT(OUT),DIMENSION(:),POINTER :: XTXT
 INTEGER :: I,J,ND,IY,IM,ID,IDATE,EDATE,IYY,IMM

 !## two seasons 1/04 - 30/9
 !##             1/10 - 31/3

 !## set start date - clipped to first of month
 ID=1; IY=IY1
 IF(IM1.GE.4.AND.IM1.LE.9)THEN
  IM=4
 ELSE
  IM=10; IF(IM1.LE.3)IY=IY-1
 ENDIF
 SDATE=IY*10000+IM*100+ID
 
 IYY=IY
 IMM=IM
 
 !## set end date - rounded to end of month
 IY=IY2
 IF(IM2.LE.9.AND.IM2.GE.4)THEN
  IM=9
 ELSE
  IM=3; IF(IM2.GE.10.AND.IM2.LE.12)IY=IY+1
 ENDIF
 ID=WDATEDAYSINMONTH(IY,IM); EDATE=IY*10000+IM*100+ID

 DO I=1,2

  IM=IMM; IY=IYY; ND=0
  DO
   ND=ND+1
   DO J=1,6
    IF(I.EQ.2)THEN
     DT(ND)=DT(ND)+WDATEDAYSINMONTH(IY,IM)
     IF(J.EQ.1)XTXT(ND)=TRIM(ITOS(IY))//'-'//TRIM(ITOS(IM))
    ENDIF
    IM=IM+1
    IF(IM.GT.12)THEN; IM=1; IY=IY+1; ENDIF
   ENDDO

   !## current date
   ID=WDATEDAYSINMONTH(IY,IM); IDATE=IY*10000+IM*100+ID
   
   !## stop
   IF(IDATE.GT.EDATE)EXIT

  ENDDO

  !## define dt pointer
  IF(I.EQ.1)THEN; ALLOCATE(DT(ND),XTXT(ND)); XTXT=''; DT=0.0; ENDIF
 
 ENDDO
 
 WBAL_ANAYSE_PREPARE_GETHSEASON=ND
 
 END FUNCTION WBAL_ANAYSE_PREPARE_GETHSEASON

 !###======================================================================
 INTEGER FUNCTION WBAL_ANAYSE_PREPARE_GETQUARTERS(IY1,IM1,IY2,IM2,SDATE,DT,XTXT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IY1,IM1,IY2,IM2
 INTEGER,INTENT(OUT) :: SDATE
 REAL,INTENT(OUT),DIMENSION(:),POINTER :: DT
 CHARACTER(LEN=*),INTENT(OUT),DIMENSION(:),POINTER :: XTXT
 INTEGER :: I,J,ND,IY,IM,ID,IDATE,EDATE,IMM,IYY

 !## set start date - clipped to first of month
 ID=1; IF(IM1.LE.3)THEN
  IM=1
 ELSEIF(IM1.LE.6)THEN
  IM=4
 ELSEIF(IM1.LE.9)THEN
  IM=7
 ELSE
  IM=10
 ENDIF
 
 SDATE=IY1*10000+IM*100+ID
 
 IMM=IM
 IYY=IY1
 
 !## set end date - rounded to end of month
 IF(IM2.LE.3)THEN
  IM=3
 ELSEIF(IM2.LE.6)THEN
  IM=6
 ELSEIF(IM2.LE.9)THEN
  IM=9
 ELSE
  IM=12
 ENDIF
 ID=WDATEDAYSINMONTH(IY,IM); EDATE=IY2*10000+IM*100+ID
 
 DO I=1,2

  IM=IMM; IY=IYY; ND=0
  DO
   ND=ND+1
   DO J=1,3
    IF(I.EQ.2)THEN
     DT(ND)=DT(ND)+WDATEDAYSINMONTH(IY,IM)
     IF(J.EQ.1)XTXT(ND)=TRIM(ITOS(IY))//'-'//TRIM(ITOS(IM))
    ENDIF
    IM=IM+1
    IF(IM.GT.12)THEN; IM=1; IY=IY+1; ENDIF
   ENDDO

   !## current date
   ID=WDATEDAYSINMONTH(IY,IM); IDATE=IY*10000+IM*100+ID
   
   !## stop
   IF(IDATE.GT.EDATE)EXIT

  ENDDO

  !## define dt pointer
  IF(I.EQ.1)THEN; ALLOCATE(DT(ND),XTXT(ND)); XTXT=''; DT=0.0; ENDIF
 
 ENDDO
 
 WBAL_ANAYSE_PREPARE_GETQUARTERS=ND
 
 END FUNCTION WBAL_ANAYSE_PREPARE_GETQUARTERS
 
 !###======================================================================
 INTEGER FUNCTION WBAL_ANAYSE_PREPARE_GETDECADES(IY1,IM1,ID1,IY2,IM2,ID2,SDATE,DT,XTXT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IY1,IM1,ID1,IY2,IM2,ID2
 INTEGER,INTENT(OUT) :: SDATE
 REAL,INTENT(OUT),DIMENSION(:),POINTER :: DT
 CHARACTER(LEN=*),INTENT(OUT),DIMENSION(:),POINTER :: XTXT
 INTEGER :: I,ND,IY,IM,ID,IDD,JD,IDATE,EDATE
 
 !## get an approprate start date
 IF(ID1.LT.10)THEN
  ID=1
 ELSEIF(ID1.LT.20)THEN
  ID=11
 ELSE
  ID=21
 ENDIF
 IDD=ID
 
 !## set start date
 SDATE=IY1*10000+IM1*100+ID

 !## get an approprate start date
 IF(ID2.LT.10)THEN
  ID=10
 ELSEIF(ID2.LT.20)THEN
  ID=20
 ELSE
  ID=WDATEDAYSINMONTH(IY,IM)
 ENDIF

 !## set end date
 EDATE=IY2*10000+IM2*100+ID
 
 DO I=1,2

  IY=IY1; IM=IM1; ID=IDD; ND=0
  DO

   JD=10; IF(ID.GT.20)JD=WDATEDAYSINMONTH(IY,IM)-20

   ND=ND+1
   IF(I.EQ.2)THEN
    DT(ND)=REAL(JD)
    XTXT(ND)=TRIM(ITOS(IY))//'-'//TRIM(ITOS(IM))//'-'//TRIM(ITOS(ID))
   ENDIF

   ID=ID+JD
   IF(ID.GE.WDATEDAYSINMONTH(IY,IM))THEN
    IM=IM+1; ID=1; IF(IM.GT.12)THEN; IM=1; IY=IY+1; ENDIF
   ENDIF
   
   !## current date
   IDATE=IY*10000+IM*100+ID
   
   !## stop
   IF(IDATE.GT.EDATE)EXIT
  ENDDO

  !## define dt pointer
  IF(I.EQ.1)THEN; ALLOCATE(DT(ND),XTXT(ND)); DT=0.0; XTXT=''; ENDIF
 
 ENDDO
 
 WBAL_ANAYSE_PREPARE_GETDECADES=ND
 
 END FUNCTION WBAL_ANAYSE_PREPARE_GETDECADES

 !###======================================================================
 INTEGER FUNCTION WBAL_ANAYSE_PREPARE_GETMONTHS(IY1,IM1,IY2,IM2,SDATE,DT,XTXT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IY1,IM1,IY2,IM2
 INTEGER,INTENT(OUT) :: SDATE
 REAL,INTENT(OUT),DIMENSION(:),POINTER :: DT
 CHARACTER(LEN=*),INTENT(OUT),DIMENSION(:),POINTER :: XTXT
 INTEGER :: NM,IM,IY,I
 
 !## get number of months
 IF(IY1.EQ.IY2)THEN
  NM=(IM2-IM1)+1
 ELSE
  NM=(12-IM1)+1
  NM= NM+12*MAX(0,(IY2-IY1-1))
  NM= NM+IM2
 ENDIF
  
 !## set start date of time-series
 SDATE=IY1*10000+IM1*100+1

 !## define dt pointer
 ALLOCATE(DT(NM),XTXT(NM)); XTXT=''; DT=0.0
 IM=IM1; IY=IY1
 DO I=1,NM
  DT(I)=WDATEDAYSINMONTH(IY,IM)
  XTXT(I)=TRIM(ITOS(IY))//'-'//TRIM(ITOS(IM))
  IM=IM+1; IF(IM.GT.12)THEN; IM=1; IY=IY+1; ENDIF
 ENDDO
  
 WBAL_ANAYSE_PREPARE_GETMONTHS=NM
 
 END FUNCTION WBAL_ANAYSE_PREPARE_GETMONTHS

 !###======================================================================
 LOGICAL FUNCTION WBAL_ANALYSE_SELECT(CL,CZ,CD,IL,IZ,ID,IAG,IPOS,IDATES)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: CL,CZ,CD
 INTEGER,INTENT(IN),DIMENSION(:) :: IDATES
 INTEGER,INTENT(IN) :: IAG
 INTEGER,INTENT(OUT) :: IL,IZ,ID,IPOS
 INTEGER :: IDATE
  
 WBAL_ANALYSE_SELECT=.TRUE.
 
 DO ID=1,SIZE(CMDATE)
  !## date selected
  IF(TRIM(CMDATE(ID)).EQ.TRIM(CD))THEN
   DO IL=1,SIZE(CMLAY)
    !## layer selected
    IF(TRIM(CMLAY(IL)).EQ.TRIM(CL))THEN
     DO IZ=1,SIZE(CMZONE)
      !## zone selected
      IF(TRIM(CMZONE(IZ)).EQ.TRIM(CZ))THEN
       SELECT CASE (IAG)
        !## all
        CASE (1:7)
         READ(CMDATE(ID),*) IDATE
         !## get location in dates
         DO IPOS=1,SIZE(IDATES)-1
          IF(IDATE.GE.IDATES(IPOS).AND.IDATE.LT.IDATES(IPOS+1))EXIT
         ENDDO
         RETURN
        CASE (0,8)
         IPOS=ID; RETURN
       END SELECT
      ENDIF
     ENDDO
    ENDIF
   ENDDO
  ENDIF
 ENDDO
 
 WBAL_ANALYSE_SELECT=.FALSE.
 
 END FUNCTION WBAL_ANALYSE_SELECT

 !###======================================================================
 LOGICAL FUNCTION WBAL_ANALYSE_TABLE()   
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,NROW
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IC
 CHARACTER(LEN=52) :: CDATE
 
 WBAL_ANALYSE_TABLE=.FALSE.
 
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB5)
 NROW=WINFOGRID(IDF_GRID1,GRIDROWSMAX)
 
 IF(MDATE.GT.NROW)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can display only the first '//TRIM(ITOS(NROW))//' records of'//CHAR(13)// &
    'the current set of '//TRIM(ITOS(MDATE))//' existing records in the current selection.','Warning')
 ENDIF
 
 ALLOCATE(IC(MBUDGET)); IC=1
 CALL WGRIDCOLUMNS(IDF_GRID1,MBUDGET,IC)
 DEALLOCATE(IC)
 
 !## set number of rows
 CALL WGRIDROWS(IDF_GRID1,MDATE)
  
 !## set column labels
 DO I=1,MBUDGET
  CALL WGRIDLABELCOLUMN(IDF_GRID1,I,TRIM(GRAPH(I,1)%LEGTXT))
 ENDDO
 
 !## set row labels
 DO I=1,MDATE
  !## apply axes titles predefined
  IF(ASSOCIATED(GRAPHDIM%XTXT))THEN
   CDATE=GRAPHDIM%XTXT(I)
  ELSE
   WRITE(CDATE,*) UTL_JDATETOIDATE(INT(GRAPH(1,1)%RX(I)))
  ENDIF
  CALL WGRIDLABELROW(IDF_GRID1,I,TRIM(CDATE))
 ENDDO
 
 IF(MGROUP.EQ.1)THEN
  CALL WDIALOGFIELDSTATE(IDF_MENU1,2)
 ELSE
  CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
 ENDIF
 CALL WDIALOGPUTMENU(IDF_MENU1,GRAPHNAMES,MGROUP,1)
 
 CALL WBAL_ANALYSE_TABLE_FILL()
 
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DWBAL_ANALYSE_TAB5,1)

 WBAL_ANALYSE_TABLE=.TRUE.
 
 END FUNCTION WBAL_ANALYSE_TABLE

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_TABLE_FILL()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IG
 
 CALL WDIALOGGETMENU(IDF_MENU1,IG)

 DO I=1,MBUDGET
  CALL WGRIDPUTREAL(IDF_GRID1,I,GRAPH(I,IG)%RY,MDATE,'(G15.7)')
 ENDDO

 END SUBROUTINE WBAL_ANALYSE_TABLE_FILL

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
 
 CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Succesfully saved the current waterbalance to:'//CHAR(13)// &
  TRIM(FNAME),'Information')
  
 WBAL_ANALYSE_EXPORTCSV=.TRUE.
  
 END FUNCTION WBAL_ANALYSE_EXPORTCSV
 
 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_PLOTIMAGE(ID,IG,IP1,IP2)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID,IG,IP1,IP2
 INTEGER,PARAMETER :: NXPIX=1000, NYPIX=1200 !## resolution dx,dy
 INTEGER,PARAMETER :: NFLX=24  !## number of zones in current csv file
 REAL,DIMENSION(NFLX,2) :: Q
 CHARACTER(LEN=10),DIMENSION(NFLX) :: QTXT
 REAL,PARAMETER :: CS=0.009 !## charactersize
 CHARACTER(LEN=256) :: PNGNAME
 LOGICAL :: LOCAL,LPERC
 INTEGER :: IOS,I,II,J,IBITMAP,IWINDOW,I1,I2,IB,IT,NCF
 REAL,DIMENSION(:,:), ALLOCATABLE :: QSUBREGIO 
 INTEGER,DIMENSION(:),ALLOCATABLE :: IPLG,IPOL
 DATA QTXT/'Q-drn   ','Q-olf   ','Q-riv   ','Q-ghb   ','Q-isg   ', &
           'Q-wel   ','Q-reg   ','Q-cnh   ','Q-ftf   ','Q-flf   ', &
           'Q-rch   ','Q-evt   ','Q-cap   ','Q-etact ','Q-pm    ', &
           'Q-pmgw  ','Q-pmsw  ','Q-sto   ','Q-decsto','Q-spgw  ', &
           'Q-cor   ','Q-qdr   ','Q-qrun  ','Q-modf  '/
 
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

 DO II=1,2
  NCF=0; DO IB=1,NBUDGET
   !## get appropriate polygon numbers that are currently selected
   J=WBAL_ANALYSE_GETQCATEGORY(BUDGET(IB)%FLUXTERM)
   IF(J.NE.-1)CYCLE !; I=INDEX(BUDGET(IB)%FLUXTERM,'_')
   NCF=NCF+1; IF(II.EQ.2)READ(BUDGET(IB)%FLUXTERM(7:),*,IOSTAT=IOS) IPOL(NCF)
   IF(IOS.NE.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot convert zone from '//TRIM(BUDGET(IB)%FLUXTERM)//CHAR(13)// &
     'into a integer value.','Error')
    DEALLOCATE(IPLG); RETURN  
   ENDIF
  ENDDO
  !## get number of active zones (non-zero budget term) 
  IF(II.EQ.1)THEN 
   ALLOCATE(IPOL(NCF)); IPOL=0
  ENDIF
 ENDDO

 ALLOCATE(QSUBREGIO(NCF,2))

 !## generate image for all timeseries (if available)
 DO IT=1,SIZE(GRAPH(1,1)%RY)

  NCF=0; I1=-1; I2=0; I=0
  DO IB=1,MBUDGET,2
   I=I+1
   I1=I1+2; I2=I2+2
   !## get appropriate balancenumber
   J=WBAL_ANALYSE_GETQCATEGORY(BUDGET(I)%FLUXTERM)
   !## connected flow - need to saved further
   IF(J.EQ.-1)THEN
    !## count for number of connected flow
    NCF=NCF+1
    CYCLE
   ENDIF
   
   !## cannot get right number
   IF(J.EQ.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot find category for '//TRIM(BUDGET(I)%FLUXTERM),'Error')
    CYCLE
   ENDIF

   !## sum volumes positive/negative separately
   Q(J,1)=Q(J,1)+GRAPH(I1,IG)%RY(IT)
   Q(J,2)=Q(J,2)+GRAPH(I2,IG)%RY(IT)

  ENDDO

  !## get regio fluxes
  QSUBREGIO=0.0; NCF=0; I1=-1; I2=0; I=0
  DO IB=1,MBUDGET,2
   I1=I1+2; I2=I2+2; I=I+1
   !## get appropriate polygon numbers that are currently selected
   J=WBAL_ANALYSE_GETQCATEGORY(BUDGET(I)%FLUXTERM)
   IF(J.NE.-1)CYCLE
   NCF=NCF+1
   QSUBREGIO(NCF,1)=GRAPH(I1,IG)%RY(IT)
   QSUBREGIO(NCF,2)=GRAPH(I2,IG)%RY(IT)
  ENDDO
  
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
 
  PNGNAME='.\TEXT.PNG'

  !## plot local window of selected polygon ipol
  LOCAL=.FALSE.; IF(IP1.EQ.1)LOCAL=.TRUE.
   !## percentiles
  LPERC=.FALSE.; IF(IP2.EQ.1)LPERC=.TRUE.
 
  CALL UTL_DEBUGLEVEL(0)
  IF(.NOT.DRAWBAL(Q,QTXT,NXPIX,NYPIX,CS,IPOL(1),SIZE(LIZONE),QSUBREGIO,LPERC,CLRIZONE,IPLG,IDFP,LOCAL,'GRAPHTITLE',IBITMAP))THEN
  ENDIF
  CALL UTL_DEBUGLEVEL(1)

  !## display image in viewer
  IF(ID.EQ.ID_PREVIEW)THEN

!  !## read settings from bitmap
!  CALL IGRFILEINFO(PNGNAME,INFO,6)
!  IW=INFO(2); IH=INFO(3)
!  CALL WBITMAPSTRETCHMODE(STRETCHDEFAULT)
!  IBITMAP=0; CALL WBITMAPLOAD(IBITMAP,PNGNAME,1)

   CALL WINDOWOPENCHILD(IWINDOW,FLAGS=SYSMENUON+FIXEDSIZEWIN+ALWAYSONTOP+HIDEWINDOW,TITLE='VIEWING: '//TRIM(PNGNAME))
   CALL IGRSELECT(DRAWWIN)
   CALL WBITMAPVIEW(IBITMAP,0,0,MODAL,KEYSCROLL+DRAGSCROLL)
   CALL WBITMAPDESTROY(IBITMAP)

  ELSE

   I=INFOERROR(1)
   CALL WBITMAPSAVE(IBITMAP,PNGNAME)
   I=INFOERROR(1)
   IF(I.NE.0)THEN 
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot SAVE the requested bitmap file called:'//CHAR(13)//&
     TRIM(PNGNAME),'Error')
   ELSE
    CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'iMOD SAVED the requested bitmap file called:'//CHAR(13)//&
     TRIM(PNGNAME)//CHAR(13)//'successfully.','Information')
   ENDIF
  ENDIF
 
 ENDDO
 
 DEALLOCATE(IPOL,QSUBREGIO,IPLG)

 END SUBROUTINE WBAL_ANALYSE_PLOTIMAGE

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_INIT(FNAME) 
 !###======================================================================
 IMPLICIT NONE 
 CHARACTER(LEN=*),INTENT(IN) :: FNAME

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

 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB3)
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
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DWBAL_ANALYSE_TAB5,0)

 IF(LEN_TRIM(FNAME).GT.0)THEN
  IF(WBAL_ANALYSE_READCSV(FNAME))THEN; ENDIF
 ENDIF
 
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
   IF(ASSOCIATED(GWBAL(I)%AREA)) DEALLOCATE(GWBAL(I)%AREA)
   IF(ASSOCIATED(GWBAL(I)%CZONE))DEALLOCATE(GWBAL(I)%CZONE)
   IF(ASSOCIATED(GWBAL(I)%Q))    DEALLOCATE(GWBAL(I)%Q)
   IF(ASSOCIATED(GWBAL(I)%TXT))  DEALLOCATE(GWBAL(I)%TXT)
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

 CALL GRAPH_DEALLOCATE()
 CALL WBAL_ANALYSE_DEALLOCATE()
 
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE)
 CALL WDIALOGUNLOAD()

 END SUBROUTINE WBAL_ANALYSE_CLOSE

END MODULE MOD_WBAL_ANALYSE