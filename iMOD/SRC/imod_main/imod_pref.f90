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
MODULE MOD_PREF

USE WINTERACTER
USE RESOURCE
USE MOD_DBL
USE MOD_POLYGON_PAR
USE MOD_COLOURS
USE MODPLOT
USE MOD_UTL, ONLY : ITOS,UTL_GETUNIT,UTL_CREATEDIR,UTL_IMODFILLMENU,UTL_WSELECTFILE,UTL_RELPATHNAME,UTL_GETHELP,UTL_SUBST,UTL_CAP
USE MOD_PREF_PAR
USE MOD_ISG_PAR, ONLY : ICLRSC,ICLRSD,ICLRSP,ICLRND,ICLRST,ICLRQH,ICLRSF,ICLRCO
USE MOD_OSD, ONLY : OSD_OPEN,OSD_GETENV
USE MOD_MANAGER_UTL, ONLY : MANAGER_UTL_UPDATE
USE MOD_PLUGIN, ONLY: PLUGIN_INITMENU,PLUGIN_INITMENU_FILL

INTEGER,DIMENSION(MXCGRAD) :: ID,ID2
DATA    (ID(ICGRAD),ICGRAD=1,MXCGRAD) /IDF_INTEGER4,IDF_INTEGER5,IDF_INTEGER6,IDF_INTEGER7, &
                                       IDF_INTEGER8,IDF_INTEGER9,IDF_INTEGER10/
DATA    (ID2(ICGRAD),ICGRAD=1,MXCGRAD) /IDF_CHECK1,IDF_CHECK2,IDF_CHECK3,IDF_CHECK4, &
                                        IDF_CHECK5,IDF_CHECK6,IDF_CHECK7/

CONTAINS

 !###======================================================================
 SUBROUTINE PREFMAIN()
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I,J,IRGB
 CHARACTER(LEN=3),DIMENSION(:),ALLOCATABLE :: STRING
 CHARACTER(LEN=50) :: PRFFILE

 CALL WDIALOGLOAD(ID_DPREF)
 CALL WDIALOGSELECT(ID_DPREFTAB1)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN)
 CALL WDIALOGPUTIMAGE(ID_RIGHT,ID_ICONRIGHT)
 CALL UTL_IMODFILLMENU(IDF_MENU2,TRIM(PREFDIR),'*.PRF','F',I,0,0)
 I=MIN(I,1)
 CALL WDIALOGFIELDSTATE(ID_RIGHT,I)
 CALL WDIALOGSELECT(ID_DPREFTAB2)
 CALL WDIALOGPUTIMAGE(ID_PROPERTIES,ID_ICONPROPERTIES)
 IF(ALLOCATED(STRING))DEALLOCATE(STRING)
 ALLOCATE(STRING(MAXCOLOUR))
 DO I=1,MAXCOLOUR
  STRING(I)=ITOS(I)
 END DO
 CALL WDIALOGPUTMENU(IDF_MENU1,STRING,MAXCOLOUR,1)
 DEALLOCATE(STRING)

 !## include default colourset icons
 CALL WDIALOGPUTIMAGE(ID_FLIP,ID_ICONFLIPCOLOUR_90,1)
 
 DO I=1,SIZE(ID)
  CALL WDIALOGPUTINTEGER(ID(I),WRGB(CLR(I,1),CLR(I,2),CLR(I,3)))
 ENDDO
 
 CALL WDIALOGSELECT(ID_DPREFTAB1)
 CALL WDIALOGPUTMENU(IDF_MENU1,PREF,MAXPREF,1)

 CALL WDIALOGSELECT(ID_DPREFTAB3)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,MXMPLOT)
 CALL WDIALOGFIELDSTATE(IDF_INTEGER1,2)

 CALL WDIALOGPUTINTEGER(IDF_INTEGER2,MAXSHAPES)
! CALL WDIALOGPUTINTEGER(IDF_INTEGER3,MAXSHPCRD)
 CALL WDIALOGSPINNERSTEP(IDF_INTEGER2,10,100)
! CALL WDIALOGSPINNERSTEP(IDF_INTEGER3,10,100)

 CALL PREFUPDATE()
 CALL PREFFIELDS()
 CALL WDIALOGSELECT(ID_DPREF)
 CALL WDIALOGSHOW(-1,-1,1,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)

  SELECT CASE (MESSAGE%WIN)

   CASE (ID_DPREF)
    SELECT CASE (ITYPE)
     CASE (TABCHANGED)
      SELECT CASE (MESSAGE%VALUE2)
       CASE (ID_DPREFTAB2)
        CALL PREF_CLR_REFRESH()
      END SELECT
     CASE (PUSHBUTTON)
      SELECT CASE (MESSAGE%VALUE1)
       CASE (IDCANCEL)
        EXIT
       CASE (IDHELP)
        CALL UTL_GETHELP('2.3','GS.Preferences')
      END SELECT
    END SELECT

   CASE (ID_DPREFTAB1)
    SELECT CASE (ITYPE)
     CASE (FIELDCHANGED)
      SELECT CASE (MESSAGE%VALUE2)
       CASE (IDF_MENU1)
        CALL PREFUPDATE()
       CASE (IDF_MENU2)

      END SELECT

     CASE (PUSHBUTTON)
      SELECT CASE (MESSAGE%VALUE1)
       CASE (ID_RIGHT)
        CALL WDIALOGSELECT(ID_DPREFTAB1)
        CALL WDIALOGGETMENU(IDF_MENU2,I,PRFFILE)
        CALL PREFREAD(TRIM(PREFDIR)//'\'//TRIM(PRFFILE))
        CALL PREFUPDATE()
       CASE (ID_OPEN)
        CALL PREFREAD('')
        CALL PREFUPDATE()
      END SELECT
    END SELECT

   CASE (ID_DPREFTAB2)   
    SELECT CASE (ITYPE)
     CASE(EXPOSE)
      CALL PREF_CLR_REFRESH_DEFAULT()
     CASE (FIELDCHANGED)
      SELECT CASE (MESSAGE%VALUE1)
       CASE (IDF_MENU1)
        CALL PREFFIELDS()
      END SELECT
      SELECT CASE (MESSAGE%VALUE2)
       CASE (IDF_INTEGER4,IDF_INTEGER5,IDF_INTEGER6,IDF_INTEGER7,IDF_INTEGER8,IDF_INTEGER9,IDF_INTEGER10)
        CALL WDIALOGGETINTEGER(MESSAGE%VALUE2,IRGB)
        CALL WSELECTCOLOUR(IRGB)
        IF(WINFODIALOG(4).EQ.1)THEN
         CALL WDIALOGPUTINTEGER(MESSAGE%VALUE2,IRGB)         
         CALL PREF_CLR_REFRESH()
        ENDIF
      END SELECT
     CASE (PUSHBUTTON)
      SELECT CASE (MESSAGE%VALUE1)
       !## read in new default colours
       CASE (ID_OPEN)
        CALL PREFOPENCOLOURS('',.TRUE.) 
        DO I=1,SIZE(ID); CALL WDIALOGPUTINTEGER(ID(I),WRGB(CLR(I,1),CLR(I,2),CLR(I,3))); ENDDO
        CALL PREF_CLR_REFRESH()
        CALL PREFFIELDS()       
       CASE (ID_SAVEAS,ID_SAVE)
        CALL PREFSAVECOLOURS('')
       CASE (ID_PROPERTIES)
        CALL WDIALOGGETMENU(IDF_MENU1,I)
        J=ICOLOR(I)
        CALL WSELECTCOLOUR(J)
        IF(WINFODIALOG(4).EQ.1)ICOLOR(I)=J
        CALL PREFFIELDS()
       CASE(ID_FLIP)
        CALL PREF_CLR_FLIP()
       CASE(ID_DEFAULT)
        CALL COLOUR_INIT()
        DO I=1,SIZE(ID); CALL WDIALOGPUTINTEGER(ID(I),WRGB(CLR(I,1),CLR(I,2),CLR(I,3))); ENDDO
        CALL PREF_CLR_REFRESH_DEFAULT()
      END SELECT
    END SELECT

  END SELECT
 ENDDO

 CALL WDIALOGSELECT(ID_DPREFTAB3)
 CALL WDIALOGGETINTEGER(IDF_INTEGER2,MAXSHAPES)

 CALL WDIALOGSELECT(ID_DPREF); CALL WDIALOGUNLOAD()

 !## update settings iMOD main window
 CALL IGRSELECT(DRAWWIN,MPW%IWIN)
 CALL DBL_IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX,IOFFSET=1)

 END SUBROUTINE PREFMAIN

 !###======================================================================
 SUBROUTINE PREFSAVECOLOURS(FNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=256) :: CLRFNAME
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IRGB
 INTEGER :: I,IU

 IF(LEN(TRIM(FNAME)).EQ.0)THEN
  IF(.NOT.UTL_WSELECTFILE('iMOD Preferences Colours (*.clr)|*.clr|',&
       SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,CLRFNAME,'Save iMOD Preferences Colours'))RETURN
 ELSE
  CLRFNAME=FNAME
 ENDIF
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=CLRFNAME,STATUS='UNKNOWN',ACTION='WRITE,DENYREAD')
 ALLOCATE(IRGB(3))
 WRITE(IU,*) '!## no,red,green,blue'
 DO I=1,MAXCOLOUR
  CALL WRGBSPLIT(ICOLOR(I),IRGB(1),IRGB(2),IRGB(3))
  WRITE(IU,*) I,IRGB(1),IRGB(2),IRGB(3)
 END DO
 DEALLOCATE(IRGB)
 !## save default legend colors
 WRITE(IU,*) '!##Default legend colours'
 DO I=1,MXCGRAD
  WRITE(IU,*) I,CLR(I,1),CLR(I,2),CLR(I,3)
 ENDDO
 CLOSE(IU)

 END SUBROUTINE PREFSAVECOLOURS

 !###======================================================================
 SUBROUTINE PREFOPENCOLOURS(FNAME,LERROR)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 LOGICAL,INTENT(IN) :: LERROR
 CHARACTER(LEN=256) :: CLRFNAME
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IRGB
 INTEGER :: I,J,K,IU,IOS
 
 IF(LEN(TRIM(FNAME)).EQ.0)THEN
  CLRFNAME='' 
  IF(.NOT.UTL_WSELECTFILE('iMOD Preferences Colours (*.clr)|*.clr|',&
       LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT+MUSTEXIST,CLRFNAME,'Load iMOD Preferences Colours'))RETURN
 ELSE
  CLRFNAME=FNAME
 ENDIF
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=CLRFNAME,STATUS='OLD',ACTION='READ,DENYWRITE')
 ALLOCATE(IRGB(3))
 DO K=1,2
  READ(IU,*)
  MAXCOLOUR=0
  DO
   READ(IU,*,IOSTAT=IOS) J,IRGB(1),IRGB(2),IRGB(3)
   IF(IOS.NE.0)EXIT
   MAXCOLOUR=MAXCOLOUR+1
   IF(K.EQ.2)ICOLOR(MAXCOLOUR)=WRGB(IRGB(1),IRGB(2),IRGB(3))
  ENDDO
  IF(K.EQ.1)THEN
   ALLOCATE(ICOLOR(MAXCOLOUR))
   REWIND(IU)
  ENDIF
 END DO
 DEALLOCATE(IRGB)

 BACKSPACE(IU)
 READ(IU,*,IOSTAT=IOS)
 IF(IOS.EQ.0)THEN
  !## try to read in legend-colours
  DO I=1,MXCGRAD
   READ(IU,*,IOSTAT=IOS) J,CLR(I,1),CLR(I,2),CLR(I,3)
   IF(IOS.NE.0)EXIT 
  END DO
 ELSE
  IF(LERROR)CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Current *.CLR does not contain extra colour information for legends.'//CHAR(13)// &
      'Legend colour remain unchanged.','Information')
 ENDIF
 CLOSE(IU)
 
 END SUBROUTINE PREFOPENCOLOURS

 !###======================================================================
 SUBROUTINE PREFCOLOURSINIT(LSAVE)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL,INTENT(IN) :: LSAVE
 INTEGER :: I
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: IRGB
 LOGICAL :: LEX
 
 CALL COLOUR_INIT()
 
 LEX=.FALSE.
 IF(LSAVE)INQUIRE(FILE=TRIM(PREFVAL(1))//'\IMOD_INIT.CLR',EXIST=LEX)

 IF(.NOT.LEX)THEN
  ALLOCATE(IRGB(3))
  CALL IGRPALETTEINIT()
  MAXCOLOUR=50; ALLOCATE(ICOLOR(MAXCOLOUR))
  DO I=1,MAXCOLOUR; ICOLOR(I)=COLOUR_RANDOM(); END DO
  DEALLOCATE(IRGB)
  IF(LSAVE)CALL PREFSAVECOLOURS(TRIM(PREFVAL(1))//'\IMOD_INIT.CLR')
 ELSE
  CALL PREFOPENCOLOURS(TRIM(PREFVAL(1))//'\IMOD_INIT.CLR',.FALSE.)
 ENDIF

 !## initialize colours for segment-package
 ICLRSC=WRGB(0,255,0)
 ICLRSD=WRGB(0,255,255)
 ICLRSP=WRGB(255,0,0)
 ICLRND=WRGB(0,0,255)
 ICLRST=WRGB(255,0,255)
 ICLRQH=WRGB(255,255,0)
 ICLRSF=WRGB(0,0,0)       !## black
 ICLRCO=WRGB(192,192,192) !## grey

 END SUBROUTINE PREFCOLOURSINIT

 !###====================================================================
 SUBROUTINE PREF_CLR_REFRESH()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,IRGB
 
 CALL WDIALOGSELECT(ID_DPREFTAB2)
 
 !## get colors on dialog
 DO I=1,MXCGRAD
  CALL WDIALOGGETINTEGER(ID(I),IRGB)
  CALL WRGBSPLIT(IRGB,CLR(I,1),CLR(I,2),CLR(I,3))
  CALL WDIALOGCOLOUR(ID(I),IRGB,IRGB)
 END DO

 CALL PREF_CLR_UPDATE()
   
 END SUBROUTINE PREF_CLR_REFRESH
 
 !###====================================================================
 SUBROUTINE PREF_CLR_UPDATE()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IRED,IGRN,IBLU,N
 REAL(KIND=DP_KIND) :: DY,Y,DRED,DGRN,DBLU

 CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
 CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
 CALL DBL_IGRUNITS(0.0D0,0.0D0,1.0D0,1.0D0)
 CALL IGRPLOTMODE(MODECOPY)
 CALL IGRFILLPATTERN(SOLID)
 CALL IGRLINEWIDTH(1)
 CALL IGRLINETYPE(SOLIDLINE)
 
 N=256/(MXCGRAD-1)
 DY=1.0D0/(N*(MXCGRAD-1))
 Y=0.0D0
 
 DO I=2,MXCGRAD
  DRED=REAL(CLR(I,1)-CLR(I-1,1))
  DGRN=REAL(CLR(I,2)-CLR(I-1,2))
  DBLU=REAL(CLR(I,3)-CLR(I-1,3))
 
  DRED=DRED/REAL(N)
  DGRN=DGRN/REAL(N)
  DBLU=DBLU/REAL(N)
 
  DO J=1,N
   IRED=CLR(I-1,1)+DRED*J
   IGRN=CLR(I-1,2)+DGRN*J
   IBLU=CLR(I-1,3)+DBLU*J
   CALL IGRCOLOURN(WRGB(IRED,IGRN,IBLU))
   CALL DBL_IGRRECTANGLE(0.0D0+Y+DY,0.0D0,0.0D0+Y,1.0D0)
   Y=Y+DY
  ENDDO

 END DO
 
 END SUBROUTINE PREF_CLR_UPDATE
 
 !###====================================================================
 SUBROUTINE PREF_CLR_FLIP()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,J
 INTEGER,DIMENSION(MXCGRAD,3) :: ICLR
 
 J=MXCGRAD
 DO I=1,MXCGRAD
  ICLR(I,1)=CLR(I,1)
  ICLR(I,2)=CLR(I,2)
  ICLR(I,3)=CLR(I,3)
 END DO
  
 J=MXCGRAD
 DO I=1,MXCGRAD
  CLR(I,1)=ICLR(J,1)
  CLR(I,2)=ICLR(J,2)
  CLR(I,3)=ICLR(J,3)
  J=J-1
 ENDDO

 CALL PREF_CLR_REFRESH_DEFAULT()
 
 DO I=1,SIZE(ID)
  CALL WDIALOGPUTINTEGER(ID(I),WRGB(CLR(I,1),CLR(I,2),CLR(I,3)))
 ENDDO
          
 END SUBROUTINE PREF_CLR_FLIP
 
 !###====================================================================
 SUBROUTINE PREF_CLR_REFRESH_DEFAULT()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,IRGB
 
 !## read current color settings on preference-dialog
 CALL WDIALOGSELECT(ID_DPREFTAB2)
 
 !## get colors on dialog
 DO I=1,MXCGRAD
  IRGB= WRGB(CLR(I,1),CLR(I,2),CLR(I,3))
  CALL WDIALOGCOLOUR(ID(I),IRGB,IRGB)
 END DO
  
 CALL PREF_CLR_UPDATE()
  
 END SUBROUTINE PREF_CLR_REFRESH_DEFAULT
 
 !###======================================================================
 SUBROUTINE PREFUPDATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DPREFTAB1)
 CALL WDIALOGGETMENU(IDF_MENU1,I)
 IF(TRIM(PREFVAL(I)).EQ.'')THEN
  CALL WDIALOGCOLOUR(IDF_LABEL1,WRGB(250,0,0),-1)
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'NOT a parameter asigned to this keyword, add in *.prf-file!')
 ELSE
  CALL WDIALOGCOLOUR(IDF_LABEL1,-1,-1)
  CALL WDIALOGPUTSTRING(IDF_LABEL1,TRIM(PREFVAL(I)))
 ENDIF

 END SUBROUTINE PREFUPDATE

 !###======================================================================
 SUBROUTINE PREFFIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IRED,IGREEN,IBLUE,I

 CALL WDIALOGSELECT(ID_DPREFTAB2)
 CALL WDIALOGGETMENU(IDF_MENU1,I)
 CALL WDIALOGCOLOUR(IDF_LABEL9,RGBBACK=ICOLOR(I))
 CALL WRGBSPLIT(ICOLOR(I),IRED,IGREEN,IBLUE)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,IRED)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER2,IGREEN)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER3,IBLUE)

 END SUBROUTINE PREFFIELDS

 !###======================================================================
 SUBROUTINE PREFREAD(FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS,I,IU,J,IOR
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=256) :: DIRNAME,PRFNAME,LINE
 CHARACTER(LEN=25) :: CCTYPE
 LOGICAL :: LEX
 
 IF(LEN(TRIM(FNAME)).EQ.0)THEN
  PRFNAME=''
  IF(.NOT.UTL_WSELECTFILE('iMOD Preferences (*.prf)|*.prf|',&
       LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,PRFNAME,&
       'Load iMOD Preferences'))RETURN
 ELSE
  PRFNAME=FNAME
 ENDIF

 PREFVAL=''
 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_SHOWNARROW,1,0)

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=PRFNAME,ACTION='READ,DENYWRITE',FORM='FORMATTED',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot read '//TRIM(PRFNAME),'Error')
  RETURN
 ENDIF

 IOR=0
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  IF(LEN_TRIM(LINE).EQ.0)CYCLE
  LINE=ADJUSTL(LINE)
  READ(LINE,*) CCTYPE
  IF(LEN_TRIM(CCTYPE).EQ.0)EXIT
  IF(IOS.NE.0)EXIT
  I=LEN_TRIM(CCTYPE)
  READ(LINE(I+1:),'(A256)') DIRNAME
  !## remove quotes
  DO
   I=INDEX(DIRNAME,CHAR(34)); IF(I.EQ.0)EXIT; IF(I.GT.0)DIRNAME(I:I)=' '
  ENDDO
  !## remove quotes
  DO
   I=INDEX(DIRNAME,CHAR(39)); IF(I.EQ.0)EXIT; IF(I.GT.0)DIRNAME(I:I)=' '
  ENDDO
  DIRNAME=ADJUSTL(DIRNAME)
  
  CCTYPE=UTL_CAP(CCTYPE,'U')
  DIRNAME=UTL_CAP(DIRNAME,'U')
  !## get drive
  I  =INDEX(DIRNAME,':')
  IF(I.GT.0)THEN
   I=ICHAR(DIRNAME(:I))
   IF(INFOOPSYSTEM(I).EQ.DRIVEUNKNOWN)DIRNAME=''
  ENDIF

  !## check any usage of %%-variables
  IF(INDEX(DIRNAME,'%').GT.0)THEN
   DO
    I=INDEX(DIRNAME,'%'); IF(I.EQ.0)EXIT
    I=I+1; J=INDEX(DIRNAME(I:),'%'); IF(J.EQ.0)EXIT
    J=J-1; J=I+J-1; LINE=OSD_GETENV(DIRNAME(I:J))
    IF(LEN_TRIM(LINE).EQ.0)THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot read the system variable %'//DIRNAME(I:J)//'%'//CHAR(13)//CHAR(13)// &
       'The PRF has not been read completely','Error')
     EXIT
    ENDIF
    DIRNAME=UTL_SUBST(DIRNAME,'%'//DIRNAME(I:J)//'%',TRIM(LINE))
   ENDDO
  ENDIF

  !## assign to right preference variable
  DO I=1,MAXPREF
   IF(TRIM(CCTYPE).EQ.TRIM(PREF(I)))THEN
    PREFVAL(I)=DIRNAME
    EXIT
   ENDIF
  END DO

  SELECT CASE (I)
   !## user[1]
   CASE(1)
    CALL UTL_RELPATHNAME(PREFDIR,DIRNAME,PREFVAL(I))
    J=LEN_TRIM(PREFVAL(I))
    IF(INDEX(PREFVAL(I),'\',.TRUE.).EQ.J)PREFVAL(I)(J:J)=' '
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I)))
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I))//'\LEGEND')
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I))//'\TMP')
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I))//'\RUNFILES')
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I))//'\IMFILES')
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I))//'\SHAPES')
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I))//'\SETTINGS')
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I))//'\IMODBATCH')
   !## vector[3],dbase[5]
   CASE(3,5)
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I)))
   !## top25[2],help[4],modflow[8],irdbase[6],tags[7]
   CASE(2,4,6,7,8)
   CASE(10)
   !## northarrow
   CASE(11)
    INQUIRE(FILE=PREFVAL(I),EXIST=LEX)    
    IF(LEX)CALL WMENUSETSTATE(ID_SHOWNARROW,1,1)
   !## ro-tool
   CASE(14:24)
    IOR=IOR+1
   !## plugin tool
   CASE(27,28)
   !## remove last backslash in preference directory if available 
    J=LEN_TRIM(PREFVAL(I)); IF(INDEX(PREFVAL(I),'\',.TRUE.).EQ.J)PREFVAL(I)(J:J)=' '
  END SELECT
 END DO

 CLOSE(IU)
 
 !## initiate main menu for plugins
 CALL PLUGIN_INITMENU()
 
 IF(IOR.NE.11)CALL WMENUSETSTATE(ID_ROTOOL,1,0)
 IF(IOR.EQ.11)CALL WMENUSETSTATE(ID_ROTOOL,1,1)
 
 END SUBROUTINE PREFREAD

 !###======================================================================
 SUBROUTINE PREFINIT()
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: LEX
 INTEGER :: IU
 
 CALL IOSDIRNAME(PREFDIR)
 INQUIRE(FILE=TRIM(PREFDIR)//'\IMOD_INIT.PRF',EXIST=LEX)
 !## create dummy imod_init.prf
 IF(.NOT.LEX)THEN
  CALL WMESSAGEBOX(YESNO,COMMONNO,QUESTIONICON,'Cannot find '//TRIM(PREFDIR)//'\IMOD_INIT.PRF'//CHAR(13)// &
   'If you agree iMOD will create this file and add the keyword'//CHAR(13)//&
   'USER='//TRIM(PREFDIR)//'\IMOD_USER'//CHAR(13)//'After that iMOD can start!','Error')
  IF(WINFODIALOG(4).NE.1)THEN
   CALL WINDOWCLOSE(); STOP
  ENDIF
  IU=UTL_GETUNIT()
  CALL OSD_OPEN(IU,FILE=TRIM(PREFDIR)//'\IMOD_INIT.PRF',STATUS='UNKNOWN')
  WRITE(IU,'(A)') 'USER "'//TRIM(PREFDIR)//'\IMOD_USER"'
  CLOSE(IU) 
 ENDIF

 CALL PREFREAD(TRIM(PREFDIR)//'\IMOD_INIT.PRF')
 !## not user keyword assigned
 IF(PREFVAL(1).EQ.'')RETURN

 !## initial values
 MAXSHAPES=400  !400
! MAXSHPCRD=500  !0

 END SUBROUTINE PREFINIT

END MODULE MOD_PREF
