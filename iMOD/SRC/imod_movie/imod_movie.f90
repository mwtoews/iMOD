MODULE MOD_MOVIE

USE WINTERACTER
USE RESOURCE
USE IMOD, ONLY : IDFINIT
USE MODPLOT, ONLY : MPW
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_LEGEND_UTL, ONLY : LEG_WRITE
USE MODPLOT, ONLY : MP
USE MOD_MANAGER, ONLY : MANAGERDELETE
USE MOD_UTL, ONLY : ITOS,UTL_IDFGETDATE,UTL_IDATETOJDATE,UTL_CREATEDIR,UTL_IMODFILLMENU
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_IDFTIMESERIE_UTL, ONLY : IDFTIMESERIE_DATES
USE MOD_IDFTIMESERIE_PAR, ONLY : NFILES,LISTFILES,MINDATE,MAXDATE
USE MOD_MOVIE_PAR

CONTAINS

 !###======================================================================
 SUBROUTINE MOVIE_CREATE_INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: N,IPLOT,I,J,K,NPER,NF,NP,NS,I1,I2,ISEC
 LOGICAL :: L1,L2
 CHARACTER(LEN=256) :: DIR
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: BMPOUTNAME

 N=0; DO IPLOT=1,SIZE(MP)
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)N=N+1
 ENDDO
 
 ALLOCATE(NFILES(N)); NFILES=0
 
 !## get number of dates - start the dialog
 IF(.NOT.IDFTIMESERIE_DATES(2,DIR=DIR,ISEC=ISEC))THEN
  CALL MOVIE_CREATE_CLOSE(); RETURN
 ENDIF

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
 
 ALLOCATE(BMPOUTNAME(NPER),PLOTNAMES(SIZE(NFILES)),IFILES(SIZE(NFILES))); PLOTNAMES=''; IFILES=0
 
 !## initialise filename that are time-constant
 DO I=1,NF
  IF(FDATES(1,I).EQ.0)PLOTNAMES(I)=LISTFILES(1,I)
 ENDDO
 
 !## create output folder
 CALL UTL_CREATEDIR(TRIM(PREFVAL(1))//'\MOVIES\'//TRIM(DIR)) 

 !## create drawing per timestep
 DO K=1,NPER
  
  !## get file that needs to be plotted for this timestep
  DO I=1,NF
   DO  
    !## do not update filename for plotting
    IF(FDATES(IFILES(I),I).LT.LDATES(K))EXIT
    !## take the next
    IFILES(I)   =IFILES(I)+1
    PLOTNAMES(I)=LISTFILES(IFILES(I),I)
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

   L1=.FALSE.; IF(I.EQ.I2)L1=.TRUE.
   L2=.FALSE.; IF(I.EQ.I1)L2=.TRUE.

   CALL IDFINIT(PLOTNAMES(I),LEGNAME=TRIM(PREFVAL(1))//'\tmp\movie_legend_file'//TRIM(ITOS(N))//'.leg',ISTYLE=IDFSTYLE(I),LPLOT=L1,LDEACTIVATE=L2)

  ENDDO

  !## save bitmaps
  BMPOUTNAME(K)=TRIM(PREFVAL(1))//'\MOVIES\'//TRIM(DIR)//'MOVIE_FRAME'//TRIM(ITOS(K))//'_'//TRIM(ITOS(LDATES(K)))//'.PNG'
  CALL WBITMAPSAVE(MPW%IBITMAP,BMPOUTNAME(K))

  !## remove selected files from manager
  CALL MANAGERDELETE(0)

 ENDDO

! !## create the mpeg
! CALL MOVIE_CREATE_MPEG(BMPOUTNAME,DIR,ISEC)

 CALL MOVIE_CREATE_CLOSE()
 
 END SUBROUTINE MOVIE_CREATE_INIT
 
 !###======================================================================
 SUBROUTINE MOVIE_CREATE_MPEG(BMPOUTNAME,DIR,ISEC)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER,INTENT(IN) :: ISEC
 CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: BMPOUTNAME
 INTEGER :: I

!## padding
!ffmpeg -i vid.mp4 -framerate 1 img_%03d.png -filter_complex overlay output.mp4

!## no padding
!ffmpeg -i vid.mp4 -framerate 1 img_%d.png -filter_complex overlay output.mp4
 
 DO I=1,SIZE(BMPOUTNAME)

 ENDDO
 
 END SUBROUTINE MOVIE_CREATE_MPEG

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
    CASE (IDCANCEL)
     CALL MOVIE_PLAY_CLOSE()
    CASE (IDHELP)
    !## save configuration
   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
    CASE (IDF_MENU1)
     CALL MOVIE_PLAY_ITEMS()
    CASE (IDF_MENU2)
     CALL MOVIE_PLAY_START()
    CASE (IDF_RADIO1,IDF_RADIO2)
     CALL MOVIE_PLAY_ITEMS()
   END SELECT
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
 INTEGER :: I,N
 
 !## get selected dirname
 CALL WDIALOGGETMENU(IDF_MENU1,I,FNAME)
 !## get tye of file to be displayed
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
 !## fill menu with existing movies
 CALL UTL_IMODFILLMENU(IDF_MENU2,TRIM(PREFVAL(1))//'\MOVIES\'//TRIM(FNAME),'*.'//TRIM(EXT(I)),'F',N,0,0,CORDER='D')

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
 SUBROUTINE MOVIE_PLAY_START()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ISTART,IEND,IFLAGS,DTYPE,DID,DD,IOPT,I,IHANDLE,N,IW,IH
 CHARACTER(LEN=256) :: MOVIEFILE
 CHARACTER(LEN=52) :: DIR,FNAME
 INTEGER,DIMENSION(3) :: INFO
! LOGICAL :: LEX
  
 DTYPE=WINFODRAWABLE(DRAWABLETYPE)
 DID  =WINFODRAWABLE(DRAWABLEID)
 DD   =WINFODRAWABLE(DRAWABLEDIALOG)

 CALL WDIALOGSELECT(ID_DMOVIE)

 !## get selected dirname
 CALL WDIALOGGETMENU(IDF_MENU1,I,DIR)
 CALL WDIALOGGETMENU(IDF_MENU2,I,FNAME)
 !## get tye of file to be displayed
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IOPT)

 MOVIEFILE=TRIM(PREFVAL(1))//'\MOVIES\'//TRIM(DIR)//'\'//TRIM(FNAME)

 IF(IOPT.EQ.1)THEN

  CALL IGRFILEINFO(MOVIEFILE,INFO,3)
  IW=INFO(2)
  IH=INFO(3)
!  INQUIRE(FILE=MOVIEFILE,OPENED=LEX)

  N=WINFOBITMAP(0,BITMAPFREE) !>0) THEN  
  
  IHANDLE=0; CALL WBITMAPCREATE(IHANDLE,IW,IH) 
  IF(IHANDLE.EQ.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot create memory for image:'//CHAR(13)//TRIM(MOVIEFILE)//CHAR(13)// &
    'Probably the image is too large.'//CHAR(13)//'Width is '//TRIM(ITOS(IW))//' and heigth is '//TRIM(ITOS(IH)),'Error')
   RETURN
  ENDIF

!  CALL WBITMAPSTRETCHMODE(STRETCHHALFTONE)
  I=WINFOERROR(1)
  CALL WBITMAPLOAD(IHANDLE,MOVIEFILE,1)
  I=WINFOERROR(1)
  IF(I.NE.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot read/load image:'//CHAR(13)//TRIM(MOVIEFILE),'Error')
   IF(IHANDLE.NE.0)CALL WBITMAPDESTROY(IHANDLE); IHANDLE=0; RETURN 
  ENDIF
!  CALL WBITMAPSTRETCHMODE(0)
!  CALL WBITMAPLOAD(IHANDLE,MOVIEFILE) 

  CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
  CALL IGRAREA(0.0,0.0,1.0,1.0)
  CALL WBITMAPPUT(IHANDLE,METHOD=0,ISTRETCH=1) !,IX1,IY1,IX2,IY2

  CALL WBITMAPDESTROY(IHANDLE); IHANDLE=0

 ELSEIF(IOPT.EQ.2)THEN

  CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
  IFLAGS=MOVIEDRAWABLE+MOVIEASYNC
  CALL WPLAYMOVIE(MOVIEFILE,IFLAGS) !,ISTART,IEND)
!
!CHARACTER MOVIEFILE Name of movie file to play 
!INTEGER, OPTIONAL IFLAGS How to play movie. Sum of: 
!  MovieFullScreen (1) Play back full screen (default:current window) 
!  MovieASync (2) Play asynchronously (default:synchronous) 
!  MovieDrawable (4) Play back to current drawable (window or dialog field) 
!INTEGER, OPTIONAL ISTART Frame to play from 
!INTEGER, OPTIONAL IEND Frame to play up to 
 
 ENDIF
 
 SELECT CASE (DTYPE)
  CASE(1); CALL IGRSELECT(DRAWWIN   ,DID)
  CASE(2); CALL IGRSELECT(DRAWBITMAP,DID)
  CASE(3); CALL WDIALOGSELECT(DD); CALL IGRSELECT(DRAWFIELD ,DID)
 END SELECT
 
 END SUBROUTINE MOVIE_PLAY_START

END MODULE MOD_MOVIE