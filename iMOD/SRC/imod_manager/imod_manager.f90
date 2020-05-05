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
MODULE MOD_MANAGER

USE WINTERACTER
USE RESOURCE
USE MOD_COLOURS
USE MODPLOT
USE MOD_IDFPLOT
USE IMODVAR, ONLY : DP_KIND,SP_KIND
USE BMPVAR
USE MOD_GENPLOT, ONLY : GEN_UPDATE,GEN_INIT,GEN_DELETE,GEN_MOVE,GEN_FILL
USE MOD_SETTINGS, ONLY : SETTINGS_GENSYMBOLS
USE MOD_UTL, ONLY : ITOS,UTL_GETUNIT,UTL_GETRELEVANTDIR,UTL_WSELECTFILE,UTL_MESSAGEHANDLE,UTL_CAP,UTL_EQUALNAMES,UTL_GETHELP
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_IPF_PAR, ONLY : IPF
USE MOD_IDF, ONLY : IDFDEALLOCATEX,IDFDEALLOCATE,IDFREAD,IDF_EXTENT,IDFEQUAL,IDFGETVAL,IDFGETXYVAL,IDFNULLIFY,IDFGETLOC,IDFCOPY
USE MOD_LEGPLOT, ONLY : LEGPLOT_PLOT_SHOW
USE MOD_LEGEND, ONLY : LEG_MAIN,LEG_ALLOCATE
USE MOD_IDFGETVALUE, ONLY : IDFGETVALUE_MAIN
USE MOD_INFO, ONLY : INFOMAIN
USE MOD_TAGS, ONLY : TAGUPDATE,TAGOPEN,TAGDELETE,TAGNEW,TAGUPDATEFIELD
USE MOD_MATH, ONLY : MATH1MAIN
USE MOD_TOPO, ONLY : TOPO1UPDATEMANAGER
USE MOD_OSD
USE MOD_MANAGER_UTL

CONTAINS

 !###======================================================================
 SUBROUTINE MANAGERMAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE

 SELECT CASE(MESSAGE%WIN)

  CASE (ID_DMANAGER)

   SELECT CASE (ITYPE)

    CASE(TABCHANGED)

     SELECT CASE (MESSAGE%VALUE2)  !new tab
      !## tags
      CASE (ID_DMANAGERTAB3)
       CALL TAGUPDATE()
      !## legend
      CASE (ID_DMANAGERTAB4)
       CALL LEGPLOT_PLOT_SHOW()

     END SELECT

    CASE(PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)

      CASE(IDCANCEL)
       CALL MANAGER_UTL_CLOSE()

      CASE(IDHELP)
       CALL UTL_GETHELP('3.3.4','VMO.iMODMan')

     END SELECT
   END SELECT

  !## idf,ipf,iff etc. files
  CASE (ID_DMANAGERTAB1)
   CALL MANAGERTAB1(ITYPE,MESSAGE)

  !## overlays
  CASE(ID_DMANAGERTAB2)
   CALL MANAGERTAB2(ITYPE,MESSAGE)

  !## comments
  CASE(ID_DMANAGERTAB3)
   CALL MANAGERTAB3(ITYPE,MESSAGE)

  !## legend
  CASE(ID_DMANAGERTAB4)
   CALL MANAGERTAB4(ITYPE,MESSAGE)

 END SELECT

 END SUBROUTINE MANAGERMAIN

 !###======================================================================
 SUBROUTINE MANAGERTAB1(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 INTEGER :: I,N

 SELECT CASE (ITYPE)

  CASE(FIELDCHANGED)

   IF(MESSAGE%VALUE1.EQ.MESSAGE%VALUE2.AND. &
      MESSAGE%VALUE2.EQ.ID_DMTABMENU)       &
    CALL MANAGER_UTL_UPDATE()

  CASE(PUSHBUTTON)

   SELECT CASE (MESSAGE%VALUE1)

    CASE(ID_MOVEUP,ID_MOVEDOWN)
     CALL MANAGERMOVE(MESSAGE%VALUE1)
    CASE(ID_OPEN)
     CALL MANAGER_UTL_ADDFILE()
     N=0; DO I=1,MXMPLOT; IF(MP(I)%IACT)N=N+1; ENDDO; IF(N.EQ.1)CALL IDFZOOM(ID_ZOOMFULLMAP,0.0D0,0.0D0,0)
     CALL IDFPLOTFAST(1)
    CASE (ID_DRAW)
     !##  refresh means re-read from disc, reset ipffname
     IF(ALLOCATED(IPF))IPF%FNAME=''
     CALL IDFPLOTFAST(1)
     CALL MANAGER_UTL_UPDATE()
    CASE(ID_INFO)
     CALL INFOMAIN()
     CALL MANAGER_UTL_FILL()
    CASE(ID_IDFVALUE)
     CALL IDFGETVALUE_MAIN()
     CALL MANAGER_UTL_UPDATE()
    CASE (ID_LEGEND)
     IF(LEG_MAIN(0))CALL IDFPLOTFAST(1)
    CASE(ID_MATH)
     CALL MATH1MAIN()
    CASE(ID_DELETE)
     CALL MANAGER_UTL_DELETE(IQ=1)
     CALL IDFPLOTFAST(1)
    CASE (ID_PROPERTIES)
     CALL MANAGERPROPERTIES()
    CASE (ID_FIND)
     CALL MANAGERFIND()
   END SELECT
 END SELECT

 END SUBROUTINE MANAGERTAB1

 !###======================================================================
 SUBROUTINE MANAGERTAB2(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN)           :: ITYPE

 SELECT CASE (ITYPE)

  CASE (FIELDCHANGED)

   IF(MESSAGE%VALUE1.EQ.MESSAGE%VALUE2.AND. &
      MESSAGE%VALUE2.EQ.ID_DMTABMENU)       &
    CALL GEN_UPDATE()

  CASE(PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    !## draw vecs by idfplot()
    CASE(ID_OPEN)
     CALL GEN_INIT()
     CALL IDFPLOTFAST(1)
    !## delete gen
    CASE(ID_DELETE)
     CALL GEN_DELETE()
     CALL IDFPLOTFAST(0)
    !## move
    CASE(ID_MOVEUP,ID_MOVEDOWN)
     CALL GEN_MOVE(MESSAGE%VALUE1)
    !## draw gen's etc.
    CASE (ID_DRAW)
     CALL IDFPLOTFAST(0)
    !## colouring/linetype
    CASE (ID_LEGEND)
     CALL SETTINGS_GENSYMBOLS()
     CALL IDFPLOTFAST(0)

   END SELECT
 END SELECT

 END SUBROUTINE MANAGERTAB2

 !###======================================================================
 SUBROUTINE MANAGERTAB3(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE

 SELECT CASE (ITYPE)

  CASE(FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
    !## tag selected
    CASE (ID_TAGOWNER)
     CALL TAGUPDATE()
     CALL TAGUPDATEFIELD()
    CASE (ID_DMTABMENU)
     CALL TAGUPDATEFIELD()
   END SELECT

  CASE(PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    !## draw tag
    CASE (ID_DRAW)
     CALL IDFPLOT(1)
    !## create tag
    CASE(ID_NEW)
     CALL TAGNEW()
     CALL TAGUPDATEFIELD()
     CALL IDFPLOT(1)
    !## view tag
    CASE(ID_OPEN)
     CALL TAGOPEN()
    !## delete tag
    CASE(ID_DELETE)
     CALL TAGDELETE()
     CALL TAGUPDATEFIELD()
     CALL IDFPLOT(1)

   END SELECT

 END SELECT

 END SUBROUTINE MANAGERTAB3

 !###======================================================================
 SUBROUTINE MANAGERTAB4(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN)           :: ITYPE

 SELECT CASE (ITYPE)

  CASE(FIELDCHANGED)

  CASE(PUSHBUTTON)

   SELECT CASE (MESSAGE%VALUE1)

    CASE (ID_LEGEND)
     IF(LEG_MAIN(0))THEN
      CALL LEGPLOT_PLOT_SHOW()
      CALL IDFPLOTFAST(1)
     ENDIF
   END SELECT
 END SELECT
 
 END SUBROUTINE MANAGERTAB4

 !###====================================================================
 SUBROUTINE MANAGERFIND()
 !###====================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I,ICASE,IALIAS
 CHARACTER(LEN=256) :: STRING,FNAME

 CALL WDIALOGLOAD(ID_DMANAGERFIND,ID_DMANAGERFIND)
 CALL UTL_DIALOGSHOW(-1,-1,0,3)

 DO

  CALL WMESSAGE(ITYPE,MESSAGE)

  SELECT CASE (ITYPE)
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE1)
    END SELECT
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDCANCEL)
      EXIT
     CASE (IDOK)
      CALL WDIALOGGETSTRING(IDF_STRING1,STRING)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,ICASE)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK2,IALIAS)
      IF(TRIM(STRING).EQ.'')THEN
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to specify a search-string first.','Information')
      ELSE
       EXIT
      ENDIF
     CASE (IDHELP)
    END SELECT
  END SELECT
 ENDDO

 CALL WDIALOGSELECT(ID_DMANAGERFIND); CALL WDIALOGUNLOAD()

 IF(MESSAGE%VALUE1.EQ.IDOK)THEN
  !## not case-sensitive
  IF(ICASE.EQ.0)STRING=UTL_CAP(STRING,'U')
  MP%ISEL=.FALSE.
  DO I=1,SIZE(MP)
   IF(.NOT.MP(I)%IACT)CYCLE
   IF(IALIAS.EQ.1)THEN
    FNAME=MP(I)%ALIAS
   ELSE
    FNAME=MP(I)%IDFNAME
   ENDIF
   !## not case-sensitive
!   IF(ICASE.EQ.0)FNAME=UTL_CAP(FNAME,'U')
   MP(I)%ISEL=UTL_EQUALNAMES(TRIM(STRING),TRIM(FNAME),ICAP=ICASE)
  ENDDO
 ENDIF
 
 CALL MANAGER_UTL_FILL()
 CALL MANAGER_UTL_UPDATE()

 END SUBROUTINE MANAGERFIND
 
 !###======================================================================
 SUBROUTINE MANAGERMOVE(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER :: IPLOT

 !## make sure all associated idf's re deallocated
 DO IPLOT=1,MXMPLOT
  CALL IDFDEALLOCATEX(MP(IPLOT)%IDF)
 ENDDO
 
 IF(ID.EQ.ID_MOVEUP)THEN

  DO IPLOT=1,MXMPLOT
   IF(MP(IPLOT)%ISEL)THEN
    MP(MXMPLOT)=MP(IPLOT-1)
    MP(IPLOT-1)=MP(IPLOT)
    MP(IPLOT)  =MP(MXMPLOT)
   ENDIF
  ENDDO

 ELSEIF(ID.EQ.ID_MOVEDOWN)THEN

  DO IPLOT=MXMPLOT-1,1,-1
   IF(MP(IPLOT)%ISEL)THEN
    MP(MXMPLOT)=MP(IPLOT+1)
    MP(IPLOT+1)=MP(IPLOT)
    MP(IPLOT)  =MP(MXMPLOT)
   ENDIF
  ENDDO

 ENDIF
 
 MP(MXMPLOT)%IACT=.FALSE.
 MP(MXMPLOT)%ISEL=.FALSE.

 !##  fill manager
 CALL MANAGER_UTL_FILL()
 CALL MANAGER_UTL_UPDATE()

 END SUBROUTINE MANAGERMOVE
 
 !###======================================================================
 SUBROUTINE MANAGERSORT_ALPHA(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER :: IPLOT,N,J
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IX,JX
 CHARACTER(LEN=52),DIMENSION(:),ALLOCATABLE :: CN
 
 N=0; DO IPLOT=1,MXMPLOT; IF(MP(IPLOT)%ISEL)N=N+1; ENDDO
 ALLOCATE(IX(N),JX(N),CN(N)); IX=0; JX=0; CN=''
 N=0; DO IPLOT=1,MXMPLOT; IF(MP(IPLOT)%ISEL)THEN; N=N+1; CN(N)=MP(IPLOT)%ALIAS; ENDIF; ENDDO

 !## get right iplot number
 N=0; DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL)THEN
   !## get right iplot number
   N=N+1; IX(N)=IPLOT; JX(N)=N
  ENDIF
 ENDDO

 IF(ID.EQ.ID_SORTALPHA_ZA)CALL WSORT(CN,1,N,IFLAGS=SORTDESCEND,IORDER=JX)
 IF(ID.EQ.ID_SORTALPHA_AZ)CALL WSORT(CN,1,N,                   IORDER=JX)

 !## process ordering in correct order
 N=0; DO IPLOT=1,MXMPLOT-1
  IF(.NOT.MP(IPLOT)%ISEL)CYCLE
  N=N+1
  !## skip as no shift need to be applied
  IF(IPLOT.EQ.IX(JX(N)))CYCLE

  !## remove current iplot
  MP(MXMPLOT)=MP(IPLOT)   
  !## copy right one
  MP(IPLOT)  =MP(IX(JX(N)))
  !## switch
  MP(IX(JX(N)))  =MP(MXMPLOT)
  !## adjust list of sort
  DO J=1,SIZE(IX)
   IF(IX(JX(J)).EQ.IPLOT)IX(JX(J))=IX(JX(N))
  ENDDO
 ENDDO
 
 MP(MXMPLOT)%IACT=.FALSE.
 MP(MXMPLOT)%ISEL=.FALSE.

 DEALLOCATE(IX,JX,CN)
 
 !##  fill manager
 CALL MANAGER_UTL_FILL()
 CALL MANAGER_UTL_UPDATE()

 END SUBROUTINE MANAGERSORT_ALPHA 
 
 !###======================================================================
 SUBROUTINE MANAGERSORT_KEYWORD()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT,JPLOT,N,I,J,ILAY,NKEY
 CHARACTER(LEN=52),DIMENSION(:),ALLOCATABLE :: CN
 CHARACTER(LEN=52) :: TCN
 CHARACTER(LEN=52),DIMENSION(4) :: CKEY
 INTEGER,DIMENSION(:),ALLOCATABLE :: ILS
 INTEGER :: ITYPE
 TYPE(WIN_MESSAGE) :: MESSAGE
 
 CALL WDIALOGLOAD(ID_DMANAGERSORT,ID_DMANAGERSORT)
 CALL UTL_DIALOGSHOW(-1,-1,0,3)
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDCANCEL,IDOK)
      EXIT
    END SELECT
  END SELECT
 ENDDO
 
 CALL WDIALOGGETSTRING(IDF_STRING1,CKEY(1))
 CALL WDIALOGGETSTRING(IDF_STRING2,CKEY(2))
 CALL WDIALOGGETSTRING(IDF_STRING3,CKEY(3))
 CALL WDIALOGGETSTRING(IDF_STRING4,CKEY(4))
 CALL WDIALOGUNLOAD()

 IF(MESSAGE%VALUE1.EQ.IDCANCEL)RETURN
 
 NKEY=0
 DO I=1,SIZE(CKEY)
  IF(TRIM(CKEY(I)).NE.'')THEN
   NKEY=NKEY+1
   CKEY(NKEY)=CKEY(I)
  ENDIF
 ENDDO

 N=0; DO IPLOT=1,MXMPLOT; IF(MP(IPLOT)%ISEL)N=N+1; ENDDO
 ALLOCATE(CN(N)); CN=''
 N=0; DO IPLOT=1,MXMPLOT; IF(MP(IPLOT)%ISEL)THEN; N=N+1; CN(N)=UTL_CAP(MP(IPLOT)%ALIAS,'U'); ENDIF; ENDDO
 ALLOCATE(ILS(SIZE(CN)))
 
 !## find number of layers and then fill in
 CALL UTL_IDFGETLAYERS(CN,SIZE(CN),ILS); DEALLOCATE(CN)

 !## start from first selected file
 DO IPLOT=1,MXMPLOT; IF(MP(IPLOT)%ISEL)EXIT; ENDDO; IPLOT=IPLOT-1

 !## proces number of unique layers
 DO I=1,SIZE(ILS)
  !## stop when ilay is zero found
  IF(ILS(I).LE.0)EXIT

  ILAY =I !ILS(I)   

  DO J=1,NKEY

   IPLOT=IPLOT+1

   !## find correct file and interchange
   DO JPLOT=1,MXMPLOT
    IF(.NOT.MP(JPLOT)%ISEL)CYCLE
    !## check wether current location suits keyword already
    TCN=UTL_CAP(MP(JPLOT)%ALIAS,'U')
    IF(INDEX(TRIM(TCN),TRIM(CKEY(J))//TRIM(ITOS(ILAY))//'.IDF').GT.0)EXIT
   ENDDO
   
   IF(JPLOT.GT.MXMPLOT)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot complete the sort action as it'//CHAR(13)// &
     'cannot find the keyword: '//TRIM(CKEY(J))//TRIM(ITOS(ILAY)),'Warning')
    EXIT
   ENDIF
   
   IF(IPLOT.NE.JPLOT)THEN
    !## remove current iplot
    MP(MXMPLOT)=MP(IPLOT)   
    !## copy right one
    MP(IPLOT)  =MP(JPLOT)
    !## switch
    MP(JPLOT)  =MP(MXMPLOT)
   ENDIF
        
  ENDDO
  IF(JPLOT.GT.MXMPLOT)EXIT
 ENDDO
 
 MP(MXMPLOT)%IACT=.FALSE.
 MP(MXMPLOT)%ISEL=.FALSE.

 !##  fill manager
 CALL MANAGER_UTL_FILL()
 CALL MANAGER_UTL_UPDATE()

 END SUBROUTINE MANAGERSORT_KEYWORD 
 
 !###======================================================================
 SUBROUTINE MANAGERPROPERTIES()
 !###======================================================================
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE

 CALL WDIALOGSELECT(ID_DMANAGERPROPERTIES)
 CALL UTL_DIALOGSHOW(-1,-1,-1,2)
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDOK,IDCANCEL)
      EXIT
     CASE (IDHELP)
       CALL UTL_GETHELP('3.3.5','VMO.iMODManProp')
    END SELECT
  END SELECT
 ENDDO
 CALL WDIALOGHIDE()
 CALL MANAGER_UTL_FILL()
 CALL GEN_FILL()

 END SUBROUTINE MANAGERPROPERTIES

END MODULE MOD_MANAGER
