MODULE MOD_MOVIE

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : IDIAGERROR

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
     CALL MOVIE_CLOSE()
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
 SUBROUTINE MOVIE_INIT()
 !###======================================================================
 IMPLICIT NONE

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_MOVIE,2).EQ.1)THEN
  CALL MOVIE_CLOSE()
  RETURN
 ENDIF

 CALL MAIN1INACTMODULE(ID_MOVIE)

 !## other module no closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_MOVIE,2,1)

! !## fill in dialog
! CALL WDIALOGLOAD(ID_DWBAL_ANALYSE,ID_DWBAL_ANALYSE)
! CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN,1)
! CALL WDIALOGPUTIMAGE(ID_SAVEAS,ID_ICONSAVEAS,1)

! CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE MOVIE_INIT
 
 !###======================================================================
 SUBROUTINE MOVIE_CLOSE()
 !###======================================================================
 IMPLICIT NONE

 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_MOVIE,2,0)

 END SUBROUTINE MOVIE_CLOSE

END MODULE MOD_MOVIE