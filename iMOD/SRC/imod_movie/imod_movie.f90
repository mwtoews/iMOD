MODULE MOD_MOVIE

USE WINTERACTER
USE RESOURCE
USE IMOD, ONLY : IDFINIT
USE MODPLOT, ONLY : MPW
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_LEGEND_UTL, ONLY : LEG_WRITE
USE MODPLOT, ONLY : MP
USE MOD_MANAGER, ONLY : MANAGERDELETE
USE MOD_UTL, ONLY : ITOS,UTL_IDFGETDATE,UTL_IDATETOJDATE,UTL_CREATEDIR,UTL_IMODFILLMENU,JDATETOGDATE,UTL_DEL1TREE,UTL_DEBUGLEVEL,UTL_GETUNIT
USE MOD_OSD, ONLY : OSD_OPEN
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_IDFTIMESERIE_UTL, ONLY : IDFTIMESERIE_DATES
USE MOD_IDFTIMESERIE_PAR, ONLY : NFILES,LISTFILES,MINDATE,MAXDATE
USE MOD_MOVIE_PAR

CONTAINS

 !###======================================================================
 SUBROUTINE MOVIE_CREATE_INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: N,IPLOT,I,J,K,NPER,NF,NP,NS,I1,I2,ISEC,IEXT,IW,IH
 REAL :: RAT,THEIGHT,TWIDTH
 LOGICAL :: L1,L2,L3
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
 
 ALLOCATE(NFILES(N)); NFILES=0
 
 !## get number of dates - start the dialog
 IF(.NOT.IDFTIMESERIE_DATES(2,DIR=DIR,ISEC=ISEC,IEXT=IEXT))THEN
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

 !## message/question
 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to continue to process '//TRIM(ITOS(MAXVAL(NFILES)))//' images ?','Question')
 IF(WINFODIALOG(4).NE.1)THEN; CALL MOVIE_CREATE_CLOSE(); RETURN; ENDIF
 
 NF=SIZE(NFILES); NP=MAXVAL(NFILES); NS=SUM(NFILES)
 ALLOCATE(LDATES(NS));    LDATES=0
 ALLOCATE(FDATES(NP,NF)); FDATES=0
 ALLOCATE(IDFSTYLE(NF));  IDFSTYLE=0 

 N=0; DO IPLOT=1,SIZE(MP)
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)THEN
   N=N+1
   !## save legend
   CALL LEG_WRITE(MP(IPLOT)%LEG,TRIM(PREFVAL(1))//'\tmp\movie_legend_file'//TRIM(ITOS(N))//'.leg')
   IDFSTYLE(N)=MP(IPLOT)%IDFKIND
  ENDIF
 ENDDO

 !## get all the available data
 NPER=0
 DO I=1,NF
  DO J=1,NFILES(I)
   FDATES(J,I)=UTL_IDFGETDATE(LISTFILES(J,I)) 
   IF(FDATES(J,I).NE.0)THEN
    FDATES(J,I)=UTL_IDATETOJDATE(FDATES(J,I))
    NPER=NPER+1
    LDATES(NPER)=FDATES(J,I)
   ENDIF   
  ENDDO
 ENDDO
  
 !## sort all dates
 CALL WSORT(LDATES,1,NPER)
 
 ALLOCATE(BMPOUTNAME(NPER),PLOTNAMES(SIZE(NFILES)),IFILES(SIZE(NFILES))); PLOTNAMES=''; IFILES=1
 
 !## initialise filename that are time-constant
 DO I=1,NF
  IF(FDATES(1,I).EQ.0)PLOTNAMES(I)=LISTFILES(1,I)
 ENDDO
 
 CALL WDIALOGLOAD(ID_DIRPROGRESS,ID_DIRPROGRESS)
 CALL WDIALOGPUTSTRING(IDF_GROUP1,'Progress Movie Creation - creating '//TRIM(ITOS(NPER))//' images')
 CALL WDIALOGRANGEPROGRESSBAR(IDF_PROGRESS1,0,NPER)
 CALL WDIALOGPUTPROGRESSBAR(IDF_PROGRESS1,0,ABSOLUTE)
 CALL WDIALOGSHOW(-1,-1,0,3)

 !## create drawing per timestep
 DO K=1,NPER
  
  !## add timestamp
  BGTEXT=JDATETOGDATE(LDATES(K),DTYPE=0)

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

  !## get the first and last plotnames()
  DO I=1,NF;    IF(PLOTNAMES(I).NE.'')THEN; I1=I; EXIT; ENDIF; ENDDO
  DO I=NF,1,-1; IF(PLOTNAMES(I).NE.'')THEN; I2=I; EXIT; ENDIF; ENDDO

  !## plot files - use legend assigned to file
  DO I=1,NF
   !## skip this file
   IF(PLOTNAMES(I).EQ.'')CYCLE

   CALL WDIALOGSELECT(ID_DIRPROGRESS)
   CALL WDIALOGPUTSTRING(IDF_LABEL3,'File: '//TRIM(PLOTNAMES(I)))

   L1=.FALSE.; IF(I.EQ.I2)L1=.TRUE.
   L2=.FALSE.; IF(I.EQ.I1)L2=.TRUE.

   CALL IDFINIT(PLOTNAMES(I),LEGNAME=TRIM(PREFVAL(1))//'\tmp\movie_legend_file'//TRIM(ITOS(N))//'.leg', & 
        ISTYLE=IDFSTYLE(I),LPLOT=L1,LDEACTIVATE=L2)

  ENDDO

  !## print date stamp in image
  IW =WINFOBITMAP(MPW%IBITMAP,BITMAPWIDTH)
  IH =WINFOBITMAP(MPW%IBITMAP,BITMAPHEIGHT)
  RAT=REAL(IW)/REAL(IH)

  CALL IGRSELECT(DRAWBITMAP,MPW%IBITMAP)
  CALL IGRPLOTMODE(MODECOPY)

  CALL IGRAREA(0.0,0.0,1.0,1.0)
  CALL IGRUNITS(0.0,0.0,REAL(IW),REAL(IH))
  
  CALL IGRCOLOURN(WRGB(0,0,0))
!  CALL IGRFILLPATTERN(OUTLINE)
!  CALL IGRRECTANGLE(100.0,100.0,400.0,400.0)
  
  !## textsize in graphical dimensions
  THEIGHT=0.05
  TWIDTH =THEIGHT/(0.03333/0.01333)/RAT
  CALL WGRTEXTFONT(FFHELVETICA,WIDTH=TWIDTH,HEIGHT=THEIGHT,ISTYLE=0)
  CALL WGRTEXTORIENTATION(IALIGN=ALIGNLEFT,ANGLE=0.0)
  IW=0.1*IW; IH=IH*0.1
  
  TWIDTH  = WGRTEXTLENGTH('Date '//TRIM(BGTEXT))*WINFOGRREAL(GRAPHICSCHWIDTH)
  THEIGHT = WINFOGRREAL(GRAPHICSCHHEIGHT)/2.0
  CALL IGRFILLPATTERN(SOLID)
  CALL IGRCOLOURN(WRGB(255,255,255))
  CALL IGRRECTANGLE(REAL(IW),REAL(IH)-THEIGHT,REAL(IW)+TWIDTH,REAL(IH)+THEIGHT)
  CALL IGRCOLOURN(WRGB(0,0,0))
  CALL WGRTEXTSTRING(REAL(IW),REAL(IH),'Date '//TRIM(BGTEXT))
  
  !## save bitmaps
  WRITE(BMPOUTNAME(K),'(A,I3.3,A)') TRIM(DIR)//'\IMAGE',K,'.'//TRIM(EXT(IEXT))
!  WRITE(BMPOUTNAME(K),'(A,I3.3,A)') TRIM(PREFVAL(1))//'\MOVIES\'//TRIM(DIR)//'\IMAGE',K,'.'//TRIM(EXT(IEXT))

  CALL WDIALOGSELECT(ID_DIRPROGRESS)
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'Saving: '//TRIM(BMPOUTNAME(K)))

  I=WINFOERROR(1)
  CALL WBITMAPSAVE(MPW%IBITMAP,BMPOUTNAME(K))
  I=WINFOERROR(1)
  IF(I.NE.0)THEN
   CALL WMESSAGEBOX(YESNO,EXCLAMATIONICON,COMMONOK,'iMOD cannot save the current image.'//CHAR(13)//'Probably there is not enough space on the disc.','Error')
   L3=.FALSE.; EXIT
  ENDIF
  
  !## remove selected files from manager - leave last entry
  IF(K.NE.NPER)CALL MANAGERDELETE(0)

 ENDDO

 CALL WDIALOGSELECT(ID_DIRPROGRESS)
 CALL WDIALOGUNLOAD()

 !## create the mpeg
 IF(L3)THEN; IF(.NOT.MOVIE_CREATE_MPEG(BMPOUTNAME,DIR,ISEC,EXT(IEXT)))K=0; ENDIF
 
 CALL MOVIE_CREATE_CLOSE()
 
 IF(K.GT.NPER)THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONOK,'Succesfully saved '//TRIM(ITOS(NPER))//' images.'//CHAR(13)// &
    'Do you want to start movie-player ?','Question')
  IF(WINFODIALOG(4).EQ.1)CALL MOVIE_PLAY_INIT()
 ENDIF
 
 END SUBROUTINE MOVIE_CREATE_INIT
 
 !###======================================================================
 LOGICAL FUNCTION MOVIE_CREATE_MPEG(BMPOUTNAME,DIR,ISEC,EXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,EXT
 INTEGER,INTENT(IN) :: ISEC
 CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: BMPOUTNAME
 INTEGER :: I,IFRAMERATE,IU,IOS
 CHARACTER(LEN=256) :: EXESTRING,CURDIR

!d:\OSS\THIRD_PARTY_SOFTWARE\ffmpeg-3.3.1-win64-static\bin\ffmpeg.exe  -start_number 1 -framerate 12 -i "image%%03d.png" -crf 25 test.avi
 MOVIE_CREATE_MPEG=.FALSE.

 !## simulate batch-file, inclusive pause statement.
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(DIR)//'\FFMPEG.BAT',STATUS='REPLACE',ACTION='WRITE,DENYREAD',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot create the file:'//CHAR(13)// &
   TRIM(DIR)//'\FFMPEG.BAT'//CHAR(13)//'This file is needed to create the movie file','Error')
  RETURN
 ENDIF

!## padding
!ffmpeg -i vid.mp4 -framerate 1 img_%03d.png -filter_complex overlay output.mp4

 IFRAMERATE=SIZE(BMPOUTNAME)/ISEC
 IFRAMERATE=MAX(1,IFRAMERATE)

 !## define executable string
 EXESTRING=TRIM(PREFVAL(29))//' -start_number 1 -framerate '//TRIM(ITOS(IFRAMERATE))//' -i "image%%03d.'//TRIM(EXT)//'" imod.avi'
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

!ffmpeg -i vid.mp4 -framerate 1 img_%d.png -filter_complex overlay output.mp4

 MOVIE_CREATE_MPEG=.TRUE.
  
 END FUNCTION MOVIE_CREATE_MPEG

 !###======================================================================
 SUBROUTINE MOVIE_CREATE_CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(IDFSTYLE)) DEALLOCATE(IDFSTYLE)
 IF(ALLOCATED(NFILES))   DEALLOCATE(NFILES)
 IF(ALLOCATED(IFILES))   DEALLOCATE(IFILES)
 IF(ALLOCATED(PLOTNAMES))DEALLOCATE(PLOTNAMES)
 IF(ALLOCATED(LISTFILES))DEALLOCATE(LISTFILES)
 IF(ALLOCATED(LDATES))   DEALLOCATE(LDATES)
 IF(ALLOCATED(FDATES))   DEALLOCATE(FDATES)
  
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
 CALL WDIALOGSHOW(-1,-1,0,2)

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
  !## bmp,jpg,pcx
  CASE (1:3); J=0
  !## avi
  CASE (4)
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
 INTEGER :: ISTART,IEND,IFLAGS,DTYPE,DID,DD,IOPT,I,J,K,IHANDLE,N,IW,IH
 CHARACTER(LEN=256) :: MOVIEFILE,STRING1,STRING2
 CHARACTER(LEN=52) :: DIR,FNAME
 INTEGER,DIMENSION(3) :: INFO
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
 IF(IOPT.EQ.4.AND.IFC.EQ.1)I=-1

 IF(I.LE.0)THEN

  CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
  CALL IGRAREA(0.0,0.0,1.0,1.0)
  CALL IGRAREACLEAR()
  RETURN

 ELSE

  MOVIEFILE=TRIM(PREFVAL(1))//'\MOVIES\'//TRIM(DIR)//'\'//TRIM(FNAME)

  SELECT CASE (TRIM(EXT(IOPT)))
   !## display bitmaps
   CASE ('BMP','JPG','PCX')

    IF(WINFOBITMAP(0,BITMAPFREE).GT.0)THEN

     CALL IGRFILEINFO(MOVIEFILE,INFO,3)
     IW=INFO(2); IH=INFO(3)
 
     IHANDLE=0; CALL WBITMAPCREATE(IHANDLE,IW,IH) 
     IF(IHANDLE.EQ.0)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot create memory for image:'//CHAR(13)//TRIM(MOVIEFILE)//CHAR(13)// &
       'Probably the image is too large.'//CHAR(13)//'Width is '//TRIM(ITOS(IW))//' and heigth is '//TRIM(ITOS(IH)),'Error')
      RETURN
     ENDIF 

     I=WINFOERROR(1)
     CALL WBITMAPLOAD(IHANDLE,MOVIEFILE,1)
     I=WINFOERROR(1)
     IF(I.NE.0)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot read/load image:'//CHAR(13)//TRIM(MOVIEFILE),'Error')
      IF(IHANDLE.NE.0)CALL WBITMAPDESTROY(IHANDLE); IHANDLE=0; RETURN 
     ENDIF
  
     CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
     CALL IGRAREA(0.0,0.0,1.0,1.0)
     CALL IGRAREACLEAR()
     CALL MOVIE_PLAY_START_SETAREA(IW,IH)
     CALL WBITMAPSTRETCHMODE(STRETCHHALFTONE)
     CALL WBITMAPPUT(IHANDLE,METHOD=2,ISTRETCH=1)
     CALL WBITMAPDESTROY(IHANDLE); IHANDLE=0
   
    ENDIF
   
   !## display movies
   CASE ('AVI')

    !## get the player - last overrules first
    !## firs is ffmpeg
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
 REAL :: R1,R2,F
 INTEGER :: IW,IH
 REAL,DIMENSION(4) :: AREA
 
 IW=WINFODIALOGFIELD(IDF_PICTURE1,FIELDWIDTH)
 IH=WINFODIALOGFIELD(IDF_PICTURE1,FIELDHEIGHT)

 R1=REAL(IW) /REAL(IH)
 R2=REAL(IWB)/REAL(IHB)

 AREA(1)=0.0; AREA(2)=0.0; AREA(3)=1.0; AREA(4)=1.0

 !## bitmap is wider than window - adjust y
 IF(R2.GT.R1)THEN
  F      = REAL(IHB)/(REAL(IWB)/REAL(IW))
  F      = F/REAL(IH)
  F      =(1.0-F)/2.0
  AREA(2)= F       !ymin
  AREA(4)= 1.0-F   !ymax
 !## bitmap is smaller than window - adjust x
 ELSE
  F      = REAL(IWB)/(REAL(IHB)/REAL(IH))
  F      = F/REAL(IW)
  F      =(1.0-F)/2.0
  AREA(1)= F       !xmin
  AREA(3)= 1.0-F   !xmax
 ENDIF

 CALL IGRAREA(AREA(1),AREA(2),AREA(3),AREA(4))

 END SUBROUTINE MOVIE_PLAY_START_SETAREA

END MODULE MOD_MOVIE