MODULE MOD_MOVIE

USE WINTERACTER
USE RESOURCE
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_LEGEND_UTL, ONLY : LEG_WRITE
USE MODPLOT, ONLY : MP
USE MOD_MANAGER, ONLY : MANAGERDELETE
USE MOD_UTL, ONLY : ITOS,UTL_IDFGETDATE,UTL_IDATETOJDATE,UTL_CREATEDIR
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_IDFTIMESERIE_UTL, ONLY : IDFTIMESERIE_DATES
USE MOD_IDFTIMESERIE_PAR, ONLY : NFILES,LISTFILES,MINDATE,MAXDATE
USE MOD_MOVIE_PAR

CONTAINS

 !###======================================================================
 SUBROUTINE MOVIE_MAIN(ITYPE,MESSAGE) 
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
    CASE (IDOK)
   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT
 END SELECT
     
 END SUBROUTINE MOVIE_MAIN 

 !###======================================================================
 SUBROUTINE MOVIE_CREATE_INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: N,IPLOT,I,J,K,NPER,NF,NP,NS,I1,I2,ISEC
 LOGICAL :: L1,L2
 CHARACTER(LEN=256) :: DIR
  
 N=0; DO IPLOT=1,SIZE(MP)
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)N=N+1
 ENDDO
 
 ALLOCATE(NFILES(N)); NFILES=0
 
 !## get number of dates - start the dialog
 IF(.NOT.IDFTIMESERIE_DATES(2,DIR,ISEC))THEN
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
 
 ALLOCATE(PLOTNAMES(SIZE(NFILES)),IFILES(SIZE(NFILES))); PLOTNAMES=''; IFILES=1
 
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

!## drop down menu voor folders in de movie-map
  !## save bitmaps

!  CALL WBITMAPSAVE(MPW%IBITMAP,BMPOUTNAME)

  !## remove selected files from manager
  CALL MANAGERDELETE(0)

 ENDDO
   
 CALL MOVIE_CREATE_CLOSE()
 
 END SUBROUTINE MOVIE_CREATE_INIT
 
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
 SUBROUTINE MOVIE_PLAY_INIT()
 !###======================================================================
 IMPLICIT NONE
 
 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_MOVIE_PLAY,2).EQ.1)THEN
  CALL MOVIE_PLAY_CLOSE(); RETURN
 ENDIF

 CALL WMENUSETSTATE(ID_MOVIE_PLAY,2,1)

 !## fill in dialog
! CALL WDIALOGLOAD(ID_DTIMESERIES,ID_DTIMESERIES)

! CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN,1)
! CALL WDIALOGPUTIMAGE(ID_SAVEAS,ID_ICONSAVEAS,1)

! CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE MOVIE_PLAY_INIT

 !###======================================================================
 SUBROUTINE MOVIE_PLAY_CLOSE()
 !###======================================================================
 IMPLICIT NONE

! CALL WDIALOGSELECT()
! CALL WDIALOGUNLOAD()
 
 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_MOVIE_PLAY,2,0)

 END SUBROUTINE MOVIE_PLAY_CLOSE

END MODULE MOD_MOVIE