MODULE MOD_MOVIE

USE WINTERACTER
USE RESOURCE
USE MODPLOT, ONLY : MP
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_IDFTIMESERIE_UTL, ONLY : IDFTIMESERIE_DATES
USE MOD_IDFTIMESERIE_PAR, ONLY : NFILES,MINDATE,MAXDATE,DSKIP,IDFNAMES
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
 INTEGER :: N,IPLOT
! REAL :: DAYFRACTION
 
 N=0; DO IPLOT=1,SIZE(MP); IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)N=N+1; ENDDO
 ALLOCATE(NFILES(N)); NFILES=0
 
 !## get number of dates - start the dialog
 IF(.NOT.IDFTIMESERIE_DATES())RETURN

! IDFNAMES()

! !## get them all and read properties!
! NIDF=0
! DO IPLOT=1,SIZE(MP)
!  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)THEN
!
!   NIDF=NIDF+1   
!   IDFDATE=UTL_IDFGETDATE(MP(IPLOT)%IDFNAME,DAYFRACTION)
! 
!   IF(IDFDATE.NE.0)THEN
!    WRITE(CDATES,'(I8)') IDFDATE
!    I=INDEX(MP(IPLOT)%IDFNAME,'\',.TRUE.)
!    J=INDEX(MP(IPLOT)%IDFNAME,CDATES,.TRUE.)
!    !## get root
!    ROOT=MP(IPLOT)%IDFNAME(I+1:J-1)
!    !## get them all
!    N=SIZE(IDFNAMES)
!    IF(DAYFRACTION.LT.0.0)THEN
!     CALL UTL_DIRINFO(MP(IPLOT)%IDFNAME(:I-1),TRIM(ROOT)//'????????'//TRIM(MP(IPLOT)%IDFNAME(J+8:)),IDFNAMES,N,'F') 
!    ELSE
!     CALL UTL_DIRINFO(MP(IPLOT)%IDFNAME(:I-1),TRIM(ROOT)//'??????????????'//TRIM(MP(IPLOT)%IDFNAME(J+14:)),IDFNAMES,N,'F') 
!    ENDIF
!    !## read each file
!!    K=0
!    DO J=1,N,DSKIP
!     IDFDATE=UTL_IDFGETDATE(IDFNAMES(J)) 
!     IDFDATE=UTL_IDATETOJDATE(IDFDATE)
!     !## proper idf name and date is inside mindate/maxdate
!     IF(IDFDATE.NE.0.AND.(IDFDATE.GE.MINDATE.AND.IDFDATE.LE.MAXDATE))THEN
!!      K=K+1
!!      CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(IDFNAMES(J))//'...')
!!      IDFNAMES(J)=MP(IPLOT)%IDFNAME(:I-1)//'\'//TRIM(IDFNAMES(J))
!!      IF(.NOT.IDFREAD(IDF(NIDF,K),IDFNAMES(J),0))RETURN
!     ENDIF
!    ENDDO
!    NFILES(NIDF)=K
!   ELSE
!!    K=1
!!    CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(MP(IPLOT)%IDFNAME)//'...')
!!    IF(.NOT.IDFREAD(IDF(NIDF,K),MP(IPLOT)%IDFNAME,0))RETURN
!!    NFILES(NIDF)=K
!   ENDIF
!  ENDIF
! ENDDO
 
 DEALLOCATE(NFILES)
 
 END SUBROUTINE MOVIE_CREATE_INIT
 
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