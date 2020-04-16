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
MODULE MOD_WBAL_UTL

USE WINTERACTER
USE RESOURCE
USE MOD_COLOURS, ONLY : COLOUR_RANDOM
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_UTL, ONLY : UTL_WSELECTFILE,UTL_GETUNIT,UTL_READINITFILE,ITOS,UTL_READPOINTER
USE MOD_WBAL_PAR

CHARACTER(LEN=100*256) :: LINE

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION WBAL_ANALYSE_SAVECONFIG(INIFNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: INIFNAME
 CHARACTER(LEN=256) :: FNAME,DIR
 INTEGER :: IU,IOS,I,IUNIT,LSUM,ZSUM,IAG,INET,IOPT
 
 WBAL_ANALYSE_SAVECONFIG=.FALSE.
 
 IF(LEN_TRIM(INIFNAME).EQ.0)THEN
  FNAME=''
  IF(.NOT.UTL_WSELECTFILE('Save iMOD Batch File (*.ini)|*.ini|',&
                   SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                   'Save iMOD Batch File (*.ini)'))RETURN
 ELSE
  FNAME=INIFNAME
 ENDIF
 
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot CREATE file called:'//CHAR(13)//&
    TRIM(FNAME),'Error')
  RETURN
 ENDIF
 
 !## get the settings from the dialogs
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB2)
 DO I=1,SIZE(BUDGET)
  CALL WGRIDGETCELLCHECKBOX(IDF_GRID1,1,I,BUDGET(I)%IACT)
  CALL WGRIDGETCELLSTRING(  IDF_GRID1,2,I,BUDGET(I)%FLUXTERM)
  CALL WGRIDGETCELLINTEGER( IDF_GRID1,3,I,BUDGET(I)%ICLR)
  CALL WGRIDGETCELLSTRING(  IDF_GRID1,4,I,BUDGET(I)%LABEL)
  CALL WGRIDGETCELLINTEGER( IDF_GRID1,5,I,BUDGET(I)%IGROUP)
 ENDDO
 CALL WDIALOGGETMENU(IDF_MENU1,LIDATE)
 !## select m3/d or mm/d
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IUNIT); IUNIT=IUNIT-1
 
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

 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB4)
 !## get output folder
 CALL WDIALOGGETSTRING(IDF_STRING1,DIR)
 !## get output configuration
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IOPT)
 
 !## write content of the ini-file
 WRITE(IU,'(A)') 'FUNCTION=WBALANCE'
 WRITE(IU,'(A)') 'CSVFNAME="'//TRIM(CSVFNAME)//'"'

 WRITE(IU,'(A,999(I1  ,A1))') 'BDGIACT=',(BUDGET(I)%IACT,','  ,I=1,SIZE(BUDGET)-1),BUDGET(I)%IACT
 WRITE(IU,'(A,999(I2.2,A1))') 'BDGIGRP=',(BUDGET(I)%IGROUP,',',I=1,SIZE(BUDGET)-1),BUDGET(I)%IGROUP
 WRITE(IU,'(A,999(I8.8,A1))') 'BDGICLR=',(BUDGET(I)%ICLR  ,',',I=1,SIZE(BUDGET)-1),BUDGET(I)%ICLR

 WRITE(IU,'(A,I1)') 'IAVG='   ,IAG
 WRITE(IU,'(A,I1)') 'NETFLUX=',INET
 WRITE(IU,'(A,I1)') 'LSUM='   ,LSUM
 WRITE(IU,'(A,I1)') 'ZSUM='   ,ZSUM
 WRITE(IU,'(A,I1)') 'IUNIT='  ,IUNIT
 WRITE(IU,'(A,I1)') 'IOPT='   ,IOPT
 
 !## skip if all selected
 IF(SUM(LIZONE).LT.SIZE(LIZONE))THEN
  LINE=''; DO I=1,NZONE; IF(LIZONE(I).EQ.1)LINE=TRIM(LINE)//TRIM(CIZONE(I))//','; ENDDO
  IF(LINE.NE.'')WRITE(IU,'(A)') 'ZONES='//LINE(:LEN_TRIM(LINE)-1)
 ENDIF
 !## skip if all selected
 IF(SUM(LILAY).LT.SIZE(LILAY))THEN
  LINE=''; DO I=1,NLAY; IF(LILAY(I).EQ.1)LINE=TRIM(LINE)//TRIM(CILAY(I))//','; ENDDO
  IF(LINE.NE.'')WRITE(IU,'(A)') 'LAYERS='//LINE(:LEN_TRIM(LINE)-1)
 ENDIF
 !## skip if all selected
 IF(SUM(LIDATE).LT.SIZE(LIDATE))THEN
  LINE=''; DO I=1,NDATE; IF(LIDATE(I).EQ.1)LINE=TRIM(LINE)//TRIM(CIDATE(I))//','; ENDDO
  IF(LINE.NE.'')WRITE(IU,'(A)') 'DATES='//LINE(:LEN_TRIM(LINE)-1)
 ENDIF
 
 WRITE(IU,'(A)') 'DIR='//TRIM(DIR)

 CLOSE(IU)

 CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Succesfully saved the current configuration to:'//CHAR(13)// &
  TRIM(FNAME),'Information')
 
 WBAL_ANALYSE_SAVECONFIG=.TRUE.
 
 END FUNCTION WBAL_ANALYSE_SAVECONFIG

 !###======================================================================
 LOGICAL FUNCTION WBAL_ANALYSE_READCONFIG(IU,IBATCH,IOPT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,IBATCH
 INTEGER,INTENT(OUT) :: IOPT
 INTEGER :: I,J,K,N,IAG,INET,LSUM,ZSUM,IUNIT
 CHARACTER(LEN=256) :: DIR
 INTEGER,POINTER,DIMENSION(:) :: ITMP
 
 WBAL_ANALYSE_READCONFIG=.FALSE.
 
 !## read active budget terms
 BUDGET%IACT=1; N=0; IF(.NOT.UTL_READPOINTER(IU,N,ITMP,'BDGIACT',1,IECHO=0))RETURN
 DO I=1,N; BUDGET(I)%IACT=ITMP(I); ENDDO
 !## read budget groups
 DO I=1,SIZE(BUDGET); BUDGET(I)%IGROUP=I; ENDDO; N=0; IF(.NOT.UTL_READPOINTER(IU,N,ITMP,'BDGIGRP',1,IECHO=0))RETURN
 DO I=1,N; BUDGET(I)%IGROUP=ITMP(I); ENDDO
 !## read budget colours
 DO I=1,SIZE(BUDGET); BUDGET(I)%ICLR=COLOUR_RANDOM(); ENDDO; N=0; IF(.NOT.UTL_READPOINTER(IU,N,ITMP,'BDGICLR',1,IECHO=0))RETURN
 DO I=1,N; BUDGET(I)%ICLR=ITMP(I); ENDDO

 IAG=8; IF(UTL_READINITFILE('IAVG',LINE,IU,1))READ(LINE,*) IAG
 IF(IBATCH.EQ.1)WRITE(*,'(A)') 'IAVG='//TRIM(ITOS(IAG))

 INET=0; IF(UTL_READINITFILE('NETFLUX',LINE,IU,1))READ(LINE,*) INET
 IF(IBATCH.EQ.1)WRITE(*,'(A)') 'NETFLUX='//TRIM(ITOS(INET))

 LSUM=0; IF(UTL_READINITFILE('LSUM',LINE,IU,1))READ(LINE,*) LSUM
 IF(IBATCH.EQ.1)WRITE(*,'(A)') 'LSUM='//TRIM(ITOS(LSUM))

 ZSUM=0; IF(UTL_READINITFILE('ZSUM',LINE,IU,1))READ(LINE,*) ZSUM
 IF(IBATCH.EQ.1)WRITE(*,'(A)') 'ZSUM='//TRIM(ITOS(ZSUM))

 IUNIT=0; IF(UTL_READINITFILE('IUNIT',LINE,IU,1))READ(LINE,*) IUNIT
 IF(IBATCH.EQ.1)WRITE(*,'(A)') 'IUNIT='//TRIM(ITOS(IUNIT))
 
 IOPT=1; IF(UTL_READINITFILE('IOPT',LINE,IU,1))READ(LINE,*) IOPT
 IF(IBATCH.EQ.1)WRITE(*,'(A)') 'IOPT='//TRIM(ITOS(IOPT))

 IF(.NOT.UTL_READINITFILE('DIR',LINE,IU,0))RETURN
 READ(LINE,*) DIR; IF(IBATCH.EQ.1)WRITE(*,'(A)') 'DIR='//TRIM(DIR)

 !## read selected zones - default none selected
 LIZONE=0; N=0; IF(.NOT.UTL_READPOINTER(IU,N,ITMP,'ZONES',1,IECHO=0))RETURN
 IF(N.EQ.0)THEN
  LIZONE=1
 ELSE
  DO I=1,N; 
   DO J=1,NZONE; READ(CIZONE(J),*) K; IF(K.EQ.ITMP(I))EXIT; ENDDO
   IF(J.GT.NZONE)THEN
    IF(IBATCH.EQ.0)THEN
      WRITE(*,'(A,I10,A)') 'iMOD cannot find zone ',K,' in given csv file'
    ELSE
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot find zone '//TRIM(ITOS(K))//' in given csv file','Error')
    ENDIF
    RETURN
   ENDIF
   !## turn current zone active
   LIZONE(J)=1
  ENDDO
 ENDIF
 
 !## read selected layers - default none selected
 LILAY=0; N=0; IF(.NOT.UTL_READPOINTER(IU,N,ITMP,'LAYERS',1,IECHO=0))RETURN
 IF(N.EQ.0)THEN
  LILAY=1
 ELSE
  DO I=1,N; 
   DO J=1,NLAY; READ(CILAY(J),*) K; IF(K.EQ.ITMP(I))EXIT; ENDDO
   IF(J.GT.NLAY)THEN
    IF(IBATCH.EQ.0)THEN
     WRITE(*,'(A,I10,A)') 'iMOD cannot find layer ',K,' in given csv file'
    ELSE
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot find layer '//TRIM(ITOS(K))//' in given csv file','Error')
    ENDIF
    RETURN
   ENDIF
   !## turn current zone active
   LILAY(J)=1
  ENDDO
 ENDIF
 
 !## read selected periods - default none selected
 LIDATE=0; N=0; IF(.NOT.UTL_READPOINTER(IU,N,ITMP,'DATES',1,IECHO=0))RETURN
 IF(N.EQ.0)THEN
  LIDATE=1
 ELSE
  DO I=1,N; 
   DO J=1,NDATE; READ(CIDATE(J),*) K; IF(K.EQ.ITMP(I))EXIT; ENDDO
   IF(J.GT.NDATE)THEN
    IF(IBATCH.EQ.0)THEN
     WRITE(*,'(A,I10,A)') 'iMOD cannot find date ',K,' in given csv file'
    ELSE
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot find date '//TRIM(ITOS(K))//' in given csv file','Error')
    ENDIF
    RETURN
   ENDIF
   !## turn current zone active
   LIDATE(J)=1
  ENDDO
 ENDIF
 
 IF(ASSOCIATED(ITMP))DEALLOCATE(ITMP)

 !## fill number of dates
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB2)

 !## set the settings in the dialogs
 DO I=1,SIZE(BUDGET)
  CALL WGRIDPUTCELLCHECKBOX(IDF_GRID1,1,I,BUDGET(I)%IACT)
  CALL WGRIDPUTCELLSTRING(  IDF_GRID1,2,I,BUDGET(I)%FLUXTERM)
  CALL WGRIDPUTCELLINTEGER( IDF_GRID1,3,I,BUDGET(I)%ICLR)
  CALL WGRIDCOLOURCELL(     IDF_GRID1,3,I,BUDGET(I)%ICLR,BUDGET(I)%ICLR)
  CALL WGRIDPUTCELLSTRING(  IDF_GRID1,4,I,BUDGET(I)%LABEL)
  CALL WGRIDPUTCELLINTEGER( IDF_GRID1,5,I,BUDGET(I)%IGROUP)
 ENDDO

 CALL WDIALOGPUTMENU(IDF_MENU1,CIDATE,NDATE,LIDATE)
 !## change units to mm/d
 IF(IUNIT.EQ.0)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO1)
 IF(IUNIT.EQ.1)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO2)

 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB3)
 !## fill number of layers/zones
 CALL WDIALOGPUTMENU(IDF_MENU1,CILAY,NLAY,LILAY)
 CALL WDIALOGPUTMENU(IDF_MENU2,CIZONE,NZONE,LIZONE)
 !## sum layers
 IF(LSUM.EQ.0)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO3)
 IF(LSUM.EQ.1)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO4)
 !## sum zones
 IF(ZSUM.EQ.0)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO1)
 IF(ZSUM.EQ.1)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO2)
 !## set type of aggregation
 IF(IAG.EQ.1)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO5)
 IF(IAG.EQ.2)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO6)
 IF(IAG.EQ.3)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO7)
 IF(IAG.EQ.4)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO8)
 IF(IAG.EQ.5)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO9)
 IF(IAG.EQ.6)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO10)
 IF(IAG.EQ.7)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO11)
 IF(IAG.EQ.8)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO12)
 !## apply net fluxes
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,INET)

 !## set units
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB2)
 IF(IUNIT.EQ.0)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO1)
 IF(IUNIT.EQ.1)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO2)

 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB4)
 !## set type of aggregation
 IF(IOPT.EQ.1)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO1)
 IF(IOPT.EQ.2)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO2)
 IF(IOPT.EQ.3)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO4)
 IF(IOPT.EQ.4)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO3)
 IF(IOPT.EQ.5)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO5)
 !## set outputfolder
 CALL WDIALOGPUTSTRING(IDF_STRING1,TRIM(DIR))

 WBAL_ANALYSE_READCONFIG=.TRUE.

 END FUNCTION WBAL_ANALYSE_READCONFIG

END MODULE MOD_WBAL_UTL