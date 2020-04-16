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
MODULE MOD_MOVIE

USE WINTERACTER
USE RESOURCE
USE MOD_DBL
USE MODPLOT, ONLY : MPW
USE MOD_IDFPLOT
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_LEGEND_UTL, ONLY : LEG_WRITE
USE MODPLOT, ONLY : MP
USE MOD_MANAGER_UTL, ONLY : MANAGER_UTL_DELETE
USE MOD_UTL, ONLY : ITOS,UTL_IDFGETDATE,UTL_IDATETOJDATE,UTL_CREATEDIR,UTL_IMODFILLMENU,JDATETOGDATE,UTL_DEL1TREE,  &
      UTL_DEBUGLEVEL,UTL_GETUNIT,UTL_CLOSEUNITS,RTOS,ITIMETOCDATE,ITIMETOGDATE,UTL_GETUNIQUE_DINT,&
      YMDHMSTOITIME
USE MOD_OSD, ONLY : OSD_OPEN
USE IMODVAR, ONLY : DP_KIND,SP_KIND,IDIAGERROR
USE MOD_IDFTIMESERIE_UTL, ONLY : IDFTIMESERIE_DATES
USE MOD_IDFTIMESERIE_PAR, ONLY : NFILES,LISTFILES 
USE MOD_MOVIE_PAR

CONTAINS

 !###======================================================================
 SUBROUTINE MOVIE_CREATE_INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: N,IPLOT,I,J,K,NPER,NF,NP,NS,ISEC,IEXT,IW,IH,NU,IYR,IMH,IDY,IHR,IMT,ISC,IDATE,ITYPE
 TYPE(WIN_MESSAGE) :: MESSAGE
 REAL(KIND=DP_KIND) :: RAT,THEIGHT,TWIDTH,RFRAME
 LOGICAL :: L3
 CHARACTER(LEN=52) :: BGTEXT
 CHARACTER(LEN=256) :: DIR
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: BMPOUTNAME

 !## check whether there is ffmeg available
 L3=.FALSE.; IF(LEN_TRIM(PREFVAL(29)).NE.'')THEN
  INQUIRE(FILE=PREFVAL(29),EXIST=L3)
 ENDIF 
 IF(.NOT.L3)THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'iMOD cannot find the following executable'//CHAR(13)//TRIM(PREFVAL(29))// &
    'This is necessary to create a movie file out of the generated images.'//CHAR(13)// &
    'Do you want to continue nevertherless, only images will be created.','Question')
  IF(WINFODIALOG(4).NE.1)THEN
   CALL MOVIE_CREATE_CLOSE(); RETURN
  ENDIF
 ENDIF

 N=0; DO IPLOT=1,SIZE(MP)
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)N=N+1
 ENDDO
 
 ALLOCATE(NFILES(N));   NFILES=0
 ALLOCATE(OFNAMES(N));  OFNAMES=''
 ALLOCATE(IMANAGER(N)); IMANAGER=0 

 !## save old filename to be used to set back at the end
 N=0; DO IPLOT=1,SIZE(MP)
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)THEN
   N=N+1; OFNAMES(N)=MP(IPLOT)%IDFNAME; IMANAGER(N)=IPLOT
  ENDIF
 ENDDO

 !## get number of dates - start the dialog
 IF(.NOT.IDFTIMESERIE_DATES(2,DIR=DIR,ISEC=ISEC,IEXT=IEXT,RFRAME=RFRAME))THEN
  CALL MOVIE_CREATE_CLOSE(); RETURN
 ENDIF

 DIR=TRIM(PREFVAL(1))//'\MOVIES\'//TRIM(DIR)

 !## allready exists folder - empty it
 IF(IOSDIREXISTS(TRIM(DIR)))THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Output folder [.\MOVIES\'//TRIM(DIR)//'] allready exist, do you want to overwrite the entire content ?','Question')
  IF(WINFODIALOG(4).NE.1)THEN
   CALL MOVIE_CREATE_CLOSE(); RETURN
  ENDIF
  IF(.NOT.UTL_DEL1TREE(TRIM(DIR)))THEN
   CALL MOVIE_CREATE_CLOSE(); RETURN
  ENDIF
 ENDIF
 
 !## create output folder
 CALL UTL_CREATEDIR(TRIM(DIR)) 

 NF=SIZE(NFILES); NP=MAXVAL(NFILES); NS=SUM(NFILES)
 ALLOCATE(LDATES(NS));    LDATES=0
 ALLOCATE(FDATES(NP,NF)); FDATES=0

 !## get all the available data
 NPER=0
 DO I=1,NF
  DO J=1,NFILES(I)
   IDATE=UTL_IDFGETDATE(LISTFILES(J,I),IYR=IYR,IMH=IMH,IDY=IDY,IHR=IHR,IMT=IMT,ISC=ISC)
   IF(IDATE.NE.0)THEN
    FDATES(J,I)=YMDHMSTOITIME(IYR,IMH,IDY,IHR,IMT,ISC)
    NPER=NPER+1
    LDATES(NPER)=FDATES(J,I)
   ENDIF   
  ENDDO
 ENDDO
  
 !## sort all dates
 !## get number unique dates
 CALL UTL_GETUNIQUE_DINT(LDATES,NPER,NU,0); NPER=NU

 !## message/question
 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to continue to process '//TRIM(ITOS(NPER))//' images ?','Question')
 IF(WINFODIALOG(4).NE.1)THEN; CALL MOVIE_CREATE_CLOSE(); RETURN; ENDIF

 ALLOCATE(BMPOUTNAME(NPER),PLOTNAMES(SIZE(NFILES)),IFILES(SIZE(NFILES))); PLOTNAMES=''; IFILES=1

 !## initialise filename(s) that are time-constant
 DO I=1,NF
  IF(FDATES(1,I).EQ.0)PLOTNAMES(I)=LISTFILES(1,I)
 ENDDO
 
 CALL WDIALOGLOAD(ID_DIRPROGRESS,ID_DIRPROGRESS)
 CALL WDIALOGPUTSTRING(IDF_GROUP1,'Progress Movie Creation - creating '//TRIM(ITOS(NPER))//' images')
 CALL WDIALOGRANGEPROGRESSBAR(IDF_PROGRESS1,0,NPER)
 CALL WDIALOGPUTPROGRESSBAR(IDF_PROGRESS1,0,ABSOLUTE)
 CALL UTL_DIALOGSHOW(-1,-1,0,3)

 !## create drawing per timestep
 DO K=1,NPER
  
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  IF(ITYPE.EQ.PUSHBUTTON)THEN
   IF(MESSAGE%VALUE1.EQ.IDCANCEL)THEN
    CALL WMESSAGEBOX(YESNOCANCEL,QUESTIONICON,COMMONNO,'Are you sure to terminate the process of generating images for the movie ?','Question')
    !## yes, terminate
    IF(WINFODIALOG(4).EQ.1)THEN; L3=.FALSE.; EXIT; ENDIF
   ENDIF
  ENDIF

  !## add timestamp
  CALL ITIMETOCDATE(LDATES(K),BGTEXT)

  CALL WDIALOGSELECT(ID_DIRPROGRESS)
  CALL WDIALOGPUTPROGRESSBAR(IDF_PROGRESS1,K,ABSOLUTE)
  CALL WDIALOGPUTSTRING(IDF_LABEL2,'Date: '//TRIM(BGTEXT))

  !## get file that needs to be plotted for this timestep
  DO I=1,NF
   DO  
    !## do not update filename for plotting
    IF(FDATES(IFILES(I),I).GT.LDATES(K))EXIT
    !## take the next, next time
    PLOTNAMES(I)=LISTFILES(IFILES(I),I)
    IFILES(I)   =IFILES(I)+1
    EXIT
   ENDDO
  ENDDO

  !## plot files - use legend assigned to file
  DO I=1,NF
   !## skip this file
   IF(PLOTNAMES(I).EQ.'')CYCLE

   CALL WDIALOGSELECT(ID_DIRPROGRESS)
   CALL WDIALOGPUTSTRING(IDF_LABEL3,'File: '//TRIM(PLOTNAMES(I)))

   MP(IMANAGER(I))%IDFNAME=PLOTNAMES(I)

  ENDDO

  !## redraw
  CALL IDFPLOT(1)

  !## print date stamp in image
  IW =WINFOBITMAP(MPW%IBITMAP,BITMAPWIDTH)
  IH =WINFOBITMAP(MPW%IBITMAP,BITMAPHEIGHT)
  RAT=REAL(IW)/REAL(IH)

  CALL IGRSELECT(DRAWBITMAP,MPW%IBITMAP)
  CALL IGRPLOTMODE(MODECOPY)

  CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
  CALL DBL_IGRUNITS(0.0D0,0.0D0,REAL(IW,8),REAL(IH,8))
  
  CALL IGRCOLOURN(WRGB(0,0,0))
  
  !## textsize in graphical dimensions
  THEIGHT=0.05D0
  TWIDTH =THEIGHT/(0.03333D0/0.01333D0)/RAT
  CALL DBL_WGRTEXTFONT(IFAMILY=FFHELVETICA,TWIDTH=TWIDTH,THEIGHT=THEIGHT,ISTYLE=0)
  CALL DBL_WGRTEXTORIENTATION(IALIGN=ALIGNLEFT,ANGLE=0.0D0)
  IW=0.1D0*IW; IH=IH*0.1D0
  
  TWIDTH  = WGRTEXTLENGTH('Date '//TRIM(BGTEXT))*WINFOGRREAL(GRAPHICSCHWIDTH)
  THEIGHT = WINFOGRREAL(GRAPHICSCHHEIGHT)/2.0D0
  CALL IGRFILLPATTERN(SOLID)
  CALL IGRCOLOURN(WRGB(255,255,255))
  CALL DBL_IGRRECTANGLE(REAL(IW,8),REAL(IH,8)-THEIGHT,REAL(IW,8)+TWIDTH,REAL(IH,8)+THEIGHT)
  CALL IGRCOLOURN(WRGB(0,0,0))
  CALL DBL_WGRTEXTSTRING(REAL(IW,8),REAL(IH,8),'Date '//TRIM(BGTEXT))
  
  !## save bitmaps
  WRITE(BMPOUTNAME(K),'(A,I5.5,A)') TRIM(DIR)//'\IMAGE',K,'.'//TRIM(EXT(IEXT))

  CALL WDIALOGSELECT(ID_DIRPROGRESS)
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'Saving: '//TRIM(BMPOUTNAME(K)))

  I=WINFOERROR(1)
  CALL WBITMAPSAVE(MPW%IBITMAP,BMPOUTNAME(K))
  I=WINFOERROR(1)
  IF(I.NE.0)THEN
   CALL WMESSAGEBOX(YESNO,EXCLAMATIONICON,COMMONOK,'iMOD cannot save the current image.'//CHAR(13)//'Probably there is not enough space on the disc.','Error')
   L3=.FALSE.; EXIT
  ENDIF

 ENDDO

 CALL WDIALOGSELECT(ID_DIRPROGRESS)
 CALL WDIALOGUNLOAD()

 !## create the mpeg
 IF(L3)THEN; IF(.NOT.MOVIE_CREATE_MPEG(BMPOUTNAME,DIR,ISEC,RFRAME,EXT(IEXT)))K=0; ENDIF
 
 CALL MOVIE_CREATE_CLOSE()
 
 IF(K.GT.NPER)THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONOK,'Succesfully saved '//TRIM(ITOS(NPER))//' images.'//CHAR(13)// &
    'Do you want to start movie-player ?','Question')
  IF(WINFODIALOG(4).EQ.1)CALL MOVIE_PLAY_INIT()
 ENDIF
 
 END SUBROUTINE MOVIE_CREATE_INIT
 
 !###======================================================================
 LOGICAL FUNCTION MOVIE_CREATE_MPEG(BMPOUTNAME,DIR,ISEC,RFRAME,EXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,EXT
 INTEGER,INTENT(IN) :: ISEC
 REAL(KIND=DP_KIND),INTENT(IN) :: RFRAME
 CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: BMPOUTNAME
 INTEGER :: I,IU,IOS
 CHARACTER(LEN=256) :: EXESTRING,CURDIR
 REAL(KIND=DP_KIND) :: RFRAMERATE
 
 MOVIE_CREATE_MPEG=.FALSE.

 !## simulate batch-file, inclusive pause statement.
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(DIR)//'\FFMPEG.BAT',STATUS='REPLACE',ACTION='WRITE,DENYREAD',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot create the file:'//CHAR(13)// &
   TRIM(DIR)//'\FFMPEG.BAT'//CHAR(13)//'This file is needed to create the movie file','Error')
  RETURN
 ENDIF

 IF(RFRAME.EQ.0.0D0)THEN
  RFRAMERATE=REAL(SIZE(BMPOUTNAME))/REAL(ISEC)
  RFRAMERATE=MAX(0.01D0,RFRAMERATE)
 ELSE
  RFRAMERATE=RFRAME
 ENDIF
 
 !## define executable string
 EXESTRING='"'//TRIM(PREFVAL(29))//'" -start_number 1 -framerate '//TRIM(RTOS(RFRAMERATE,'F',2))//' -i "image%%05d.'//TRIM(EXT)//'" -vb 20M imod.avi'
 WRITE(IU,'(A)') TRIM(EXESTRING)
 CLOSE(IU)
 
 CALL IOSDIRNAME(CURDIR); CALL IOSDIRCHANGE(DIR)
 I=WINFOERROR(1)
 EXESTRING=TRIM(DIR)//'\FFMPEG.BAT'
 CALL IOSCOMMAND(TRIM(EXESTRING))
 I=WINFOERROR(1)
 IF(I.NE.0)THEN
  CALL WINFOERRORMESSAGE(I,EXESTRING)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error creating the movie file:'//CHAR(13)// &
     TRIM(DIR)//'\imod.avi'//CHAR(13)//'Error message:'//CHAR(13)//TRIM(EXESTRING),'Error')
 ENDIF
 
 CALL IOSDIRCHANGE(CURDIR)

 MOVIE_CREATE_MPEG=.TRUE.
  
 END FUNCTION MOVIE_CREATE_MPEG

 !###======================================================================
 SUBROUTINE MOVIE_CREATE_CLOSE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT,N
 
 !## restore old filename at the end
 IF(ALLOCATED(OFNAMES))THEN 
  N=0; DO IPLOT=1,SIZE(MP)
   IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)THEN
    N=N+1; MP(IPLOT)%IDFNAME=OFNAMES(N)
   ENDIF
  ENDDO
 ENDIF
 
 IF(ALLOCATED(IMANAGER)) DEALLOCATE(IMANAGER)
 IF(ALLOCATED(NFILES))   DEALLOCATE(NFILES)
 IF(ALLOCATED(IFILES))   DEALLOCATE(IFILES)
 IF(ALLOCATED(PLOTNAMES))DEALLOCATE(PLOTNAMES)
 IF(ALLOCATED(LISTFILES))DEALLOCATE(LISTFILES)
 IF(ALLOCATED(LDATES))   DEALLOCATE(LDATES)
 IF(ALLOCATED(FDATES))   DEALLOCATE(FDATES)
 IF(ALLOCATED(OFNAMES))  DEALLOCATE(OFNAMES)
 
 END SUBROUTINE MOVIE_CREATE_CLOSE
 
 !###======================================================================
 SUBROUTINE MOVIE_PLAY_MAIN(ITYPE,MESSAGE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 
 CALL WDIALOGSELECT(MESSAGE%WIN) 
  
 SELECT CASE (ITYPE)
 
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (ID_PLAY)
     CALL MOVIE_PLAY_START(0) 
    CASE (IDCANCEL)
     CALL MOVIE_PLAY_CLOSE()
    CASE (IDHELP)
    !## save configuration
   END SELECT
   
  CASE (FIELDCHANGED)
   IF(MESSAGE%VALUE1.EQ.MESSAGE%VALUE2)THEN
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_MENU1,IDF_MENU4)
      CALL MOVIE_PLAY_ITEMS()
      CALL MOVIE_PLAY_START(1) 
     CASE (IDF_MENU2)
      CALL MOVIE_PLAY_START(1)
    END SELECT
   ENDIF
  CASE (RESIZE,EXPOSE)
   CALL MOVIE_PLAY_START(1)
  
 END SELECT
     
 END SUBROUTINE MOVIE_PLAY_MAIN 
 
 !###======================================================================
 SUBROUTINE MOVIE_PLAY_INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: N

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_MOVIE_PLAY,2).EQ.1)THEN
  CALL MOVIE_PLAY_CLOSE(); RETURN
 ENDIF

 !## fill in dialog
 CALL WDIALOGLOAD(ID_DMOVIE,ID_DMOVIE)

 !## fill in available extents
 CALL WDIALOGPUTMENU(IDF_MENU4,EXT,SIZE(EXT),1)
 !## create folder if not yet existing
 CALL UTL_CREATEDIR(TRIM(PREFVAL(1))//'\MOVIES')
 !## fill menu with existing movies
 CALL UTL_IMODFILLMENU(IDF_MENU1,TRIM(PREFVAL(1))//'\MOVIES','*','D',N,0,0)

 !## nothing available
 IF(N.LE.0)THEN
  CALL WDIALOGUNLOAD()
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'There are no folders within the movie folder:'//CHAR(13)// &
   TRIM(PREFVAL(1))//'\MOVIES'//CHAR(13)//'You need to create a movie first.','Warning')
  RETURN
 ENDIF
 
 CALL MOVIE_PLAY_ITEMS()
 
 !## display dialog
 CALL UTL_DIALOGSHOW(-1,-1,0,2)

 CALL WMENUSETSTATE(ID_MOVIE_PLAY,2,1)

 END SUBROUTINE MOVIE_PLAY_INIT

 !###======================================================================
 SUBROUTINE MOVIE_PLAY_ITEMS()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=52) :: FNAME
 INTEGER :: I,J,K,N
 LOGICAL :: LEX

 !## get selected dirname
 CALL WDIALOGGETMENU(IDF_MENU1,I,FNAME)
 !## get tye of file to be displayed
 CALL WDIALOGGETMENU(IDF_MENU4,I)
 !## fill menu with existing movies
 CALL UTL_IMODFILLMENU(IDF_MENU2,TRIM(PREFVAL(1))//'\MOVIES\'//TRIM(FNAME),'*.'//TRIM(EXT(I)),'F',N,0,0,CORDER='D')
 !## outgrey
 SELECT CASE (I)
  !## bmp,png,jpg,pcx
  CASE (1:4); J=0
  !## avi
  CASE (5)
   !## check whether a movie-player is associated to an avi
   J=0
   DO K=30,31
    IF(LEN_TRIM(PREFVAL(K)).NE.'')THEN
     INQUIRE(FILE=PREFVAL(K),EXIST=LEX); IF(LEX)J=1
    ENDIF
   ENDDO
 END SELECT
 CALL WDIALOGFIELDSTATE(ID_PLAY,J)

 END SUBROUTINE MOVIE_PLAY_ITEMS

 !###======================================================================
 SUBROUTINE MOVIE_PLAY_CLOSE()
 !###======================================================================
 IMPLICIT NONE

 CALL WDIALOGSELECT(ID_DMOVIE)
 CALL WDIALOGUNLOAD()
 
 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_MOVIE_PLAY,2,0)

 END SUBROUTINE MOVIE_PLAY_CLOSE

 !###======================================================================
 SUBROUTINE MOVIE_PLAY_START(IFC)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFC
 INTEGER :: IFLAGS,DTYPE,DID,DD,IOPT,I,J,K,IHANDLE,IW,IH
 CHARACTER(LEN=256) :: MOVIEFILE,STRING1
 CHARACTER(LEN=52) :: DIR,FNAME
 LOGICAL :: LEX
 
 DTYPE=WINFODRAWABLE(DRAWABLETYPE)
 DID  =WINFODRAWABLE(DRAWABLEID)
 DD   =WINFODRAWABLE(DRAWABLEDIALOG)

 CALL WDIALOGSELECT(ID_DMOVIE)

 !## get tye of file to be displayed
 CALL WDIALOGGETMENU(IDF_MENU4,IOPT)

 !## get selected dirname
 CALL WDIALOGGETMENU(IDF_MENU1,I,DIR)
 CALL UTL_DEBUGLEVEL(0); CALL WDIALOGGETMENU(IDF_MENU2,I,FNAME); CALL UTL_DEBUGLEVEL(1)
 
 !## do not start a movie directly
 IF(IOPT.EQ.5.AND.IFC.EQ.1)I=-1

 IF(I.LE.0)THEN

  CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
  CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
  CALL IGRAREACLEAR()
  RETURN

 ELSE

  MOVIEFILE=TRIM(PREFVAL(1))//'\MOVIES\'//TRIM(DIR)//'\'//TRIM(FNAME)

  SELECT CASE (TRIM(EXT(IOPT)))
   !## display bitmaps
   CASE ('BMP','PNG','JPG','PCX')

    IF(WINFOBITMAP(0,BITMAPFREE).GT.0)THEN
     
     I=WINFOERROR(1)
     IHANDLE=0; CALL WBITMAPLOAD(IHANDLE,TRIM(MOVIEFILE))
     I=WINFOERROR(1)
     IF(I.NE.0)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot read/load image:'//CHAR(13)//TRIM(MOVIEFILE)//CHAR(13)// &
       'ERROR code = '//TRIM(ITOS(I)),'Error')
      IF(IHANDLE.NE.0)CALL WBITMAPDESTROY(IHANDLE); RETURN 
     ENDIF

     IW=WINFOBITMAP(IHANDLE,BITMAPWIDTH)
     IH=WINFOBITMAP(IHANDLE,BITMAPHEIGHT)
    
     CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
     CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
     CALL IGRAREACLEAR()
     CALL MOVIE_PLAY_START_SETAREA(IW,IH)
     CALL WBITMAPSTRETCHMODE(STRETCHHALFTONE)
     CALL WBITMAPPUT(IHANDLE,METHOD=2,ISTRETCH=1)
     CALL WBITMAPDESTROY(IHANDLE)
   
    ENDIF
   
   !## display movies
   CASE ('AVI')

    !## get the player - last overrules first
    !## first is ffmpeg
    !## last is vlcplayer
    K=0; DO J=30,31
     IF(LEN_TRIM(PREFVAL(J)).NE.'')THEN
      INQUIRE(FILE=PREFVAL(J),EXIST=LEX); IF(LEX)K=J
     ENDIF
    ENDDO

    !## create string
    SELECT CASE (K)
     !## ffplay
     CASE (30)
      STRING1=TRIM(PREFVAL(K))//' '//TRIM(MOVIEFILE) !test.avi
     !## vlcplayer
     CASE (31)
      STRING1=TRIM(PREFVAL(K))//' '//TRIM(MOVIEFILE) !test.avi
!      STRING1=REM "c:\Program Files (x86)\VideoLAN\VLC\vlc.exe" test.avi
    END SELECT
    
    IFLAGS=0
    !## hidden window
    IFLAGS=IFLAGS+PROCSILENT
    !## I=0: show execution window
    !## I=1: hide execution window
    IFLAGS=IFLAGS+PROCBLOCKED

    CALL IOSCOMMAND(STRING1,IFLAGS) !,0) !,PID)
    
!    CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
!    IFLAGS=MOVIEDRAWABLE+MOVIEASYNC
!    !## clear error
!    I=WINFOERROR(LASTERROR); CALL UTL_DEBUGLEVEL(0)
!    CALL WPLAYMOVIE(MOVIEFILE,IFLAGS) !,ISTART,IEND)
!    CALL UTL_DEBUGLEVEL(0)
!    !## get error
!    I=WINFOERROR(LASTERROR)
!    !## operating system error
!    IF(I.EQ.ERROSCOMMAND)THEN
!     CALL WINFOERRORMESSAGE(I,STRING1,MSGTYPEWINT)
!     I=WINFOERROR(OSERRORCODE)
!     IF(I.NE.0)THEN
!      CALL WINFOERRORMESSAGE(I,STRING2,MSGTYPEOS)
!      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error occured, error message received is:'//CHAR(13)//TRIM(STRING1)//CHAR(13)//TRIM(STRING2),'Error')
!     ENDIF
!    ELSEIF(I.NE.0)THEN
!     CALL WINFOERRORMESSAGE(I,STRING1,MSGTYPEWINT)
!     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error occured, error message received is:'//CHAR(13)//TRIM(STRING1),'Error')
!    ENDIF
! 
  END SELECT
 
 ENDIF
 
 SELECT CASE (DTYPE)
  CASE(1); CALL IGRSELECT(DRAWWIN   ,DID)
  CASE(2); CALL IGRSELECT(DRAWBITMAP,DID)
  CASE(3); CALL WDIALOGSELECT(DD); CALL IGRSELECT(DRAWFIELD ,DID)
 END SELECT
 
 END SUBROUTINE MOVIE_PLAY_START

 !###====================================================================
 SUBROUTINE MOVIE_PLAY_START_SETAREA(IWB,IHB)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IWB,IHB
 REAL(KIND=DP_KIND) :: R1,R2,F
 INTEGER :: IW,IH
 REAL(KIND=DP_KIND),DIMENSION(4) :: AREA
 
 IW=WINFODIALOGFIELD(IDF_PICTURE1,FIELDWIDTH)
 IH=WINFODIALOGFIELD(IDF_PICTURE1,FIELDHEIGHT)

 R1=REAL(IW) /REAL(IH)
 R2=REAL(IWB)/REAL(IHB)

 AREA(1)=0.0D0; AREA(2)=0.0D0; AREA(3)=1.0D0; AREA(4)=1.0D0

 !## bitmap is wider than window - adjust y
 IF(R2.GT.R1)THEN
  F      = REAL(IHB)/(REAL(IWB)/REAL(IW))
  F      = F/REAL(IH)
  F      =(1.0D0-F)/2.0D0
  AREA(2)= F       !ymin
  AREA(4)= 1.0D0-F   !ymax
 !## bitmap is smaller than window - adjust x
 ELSE
  F      = REAL(IWB)/(REAL(IHB)/REAL(IH))
  F      = F/REAL(IW)
  F      =(1.0D0-F)/2.0D0
  AREA(1)= F       !xmin
  AREA(3)= 1.0D0-F   !xmax
 ENDIF

 CALL DBL_IGRAREA(AREA(1),AREA(2),AREA(3),AREA(4))

 END SUBROUTINE MOVIE_PLAY_START_SETAREA

END MODULE MOD_MOVIE