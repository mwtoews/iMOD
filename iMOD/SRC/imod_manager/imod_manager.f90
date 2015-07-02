!!  Copyright (C) Stichting Deltares, 2005-2014.
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

USE MODPLOT
USE IMODVAR
USE BMPVAR
USE MOD_GENPLOT, ONLY : TOPOGENUPDATE,TOPOGENINIT,TOPOGENDELETE,TOPOGENMOVE,TOPOGENFILL
USE MOD_SETTINGS, ONLY : SETTINGS_GENSYMBOLS
USE MOD_UTL, ONLY : ITOS,UTL_GETUNIT,UTL_GETRELEVANTDIR,UTL_WSELECTFILE,UTL_MESSAGEHANDLE
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_IPF_PAR, ONLY : IPF
USE MOD_IDF, ONLY : IDFDEALLOCATEX,IDFDEALLOCATE,IDFREAD,IDF_EXTENT,IDFEQUAL,IDFGETVAL,IDFGETXYVAL,IDFNULLIFY,IDFGETLOC,IDFCOPY
USE MOD_LEGPLOT, ONLY : LEGSHOW
USE MOD_LEGEND, ONLY : LEGMAIN
USE MOD_IDFGETVALUE, ONLY : IDFGETVALUE_MAIN
USE MOD_INFO, ONLY : INFOMAIN
USE MOD_TAGS, ONLY : TAGUPDATE,TAGOPEN,TAGDELETE,TAGNEW,TAGDRAW,TAGUPDATEFIELD
USE MOD_MATH, ONLY : MATH1MAIN
USE MOD_MDF, ONLY : WRITEMDF,READMDF,MDF,MDFDEALLOCATE,MDFALLOCATE
USE MOD_NC2IDF, ONLY : INETCDF
USE MOD_TOPO, ONLY : TOPO1UPDATEMANAGER
USE IMOD

CONTAINS

 !###======================================================================
 SUBROUTINE MANAGERMAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 INTEGER :: ITAB

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
       CALL LEGSHOW()

     END SELECT

    CASE(PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)

      CASE(IDCANCEL)
       CALL MANAGERCLOSE()

      CASE(IDHELP)
       CALL IMODGETHELP('3.3.4','iMOD Manager')

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

 SELECT CASE (ITYPE)

  CASE(FIELDCHANGED)

   IF(MESSAGE%VALUE1.EQ.MESSAGE%VALUE2.AND. &
      MESSAGE%VALUE2.EQ.ID_DMTABMENU)       &
    CALL MANAGERUPDATE()

  CASE(PUSHBUTTON)

   SELECT CASE (MESSAGE%VALUE1)

    CASE(ID_MOVEUP,ID_MOVEDOWN)
     CALL MANAGERMOVE(MESSAGE%VALUE1)
    CASE(ID_OPEN)
     CALL IDFINIT()
    CASE (ID_DRAW)
     !##  refresh means re-read from disc, reset ipffname
     IF(ALLOCATED(IPF))IPF%FNAME=''
     CALL IDFPLOTFAST(1)
     CALL MANAGERUPDATE()
    CASE(ID_INFO)
     CALL INFOMAIN()
     CALL MANAGERFILL()
    CASE(ID_IDFVALUE)
     CALL IDFGETVALUE_MAIN()
     CALL MANAGERUPDATE()
    CASE (ID_LEGEND)
     CALL LEGMAIN(0)
    CASE(ID_MATH)
     CALL MATH1MAIN()
    CASE(ID_DELETE)
     CALL MANAGERDELETE(IQ=1)
     !CALL IDFPLOTFAST(1)
    CASE (ID_PROPERTIES)
     CALL MANAGERPROPERTIES()
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
    CALL TOPOGENUPDATE()

  CASE(PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    !## draw vecs by idfplot()
    CASE(ID_OPEN)
     CALL TOPOGENINIT()
    !## delete gen
    CASE(ID_DELETE)
     CALL TOPOGENDELETE()
     CALL IDFPLOTFAST(0)
    !## move
    CASE(ID_MOVEUP,ID_MOVEDOWN)
     CALL TOPOGENMOVE(MESSAGE%VALUE1)
    !## draw gen's etc.
    CASE (ID_DRAW)
     CALL IDFPLOTFAST(0)
    !## colouring/linetype
    CASE (ID_LEGEND)
     CALL SETTINGS_GENSYMBOLS()

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
     CALL LEGMAIN(0)
     CALL LEGSHOW()

   END SELECT
 END SELECT

 END SUBROUTINE MANAGERTAB4

 !###======================================================================
 SUBROUTINE MANAGERMOVE(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER :: IPLOT

 !## make sure all associated idf's re deallocated
 DO IPLOT=1,MXMPLOT
!  IF(MP(IPLOT)%ISEL)
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
 CALL MANAGERFILL()
 CALL MANAGERUPDATE()

 END SUBROUTINE MANAGERMOVE

 !###======================================================================
 SUBROUTINE MANAGERSORT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT,N,IROW,ICOL,I,J,K,M
 REAL :: XC,YC
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF,IDFM
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IX,JX
 REAL,ALLOCATABLE,DIMENSION(:) :: X
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: IORDER
 LOGICAL :: LEQUAL

 CALL UTL_MESSAGEHANDLE(0)

 !## make sure all associated idf's re deallocated
 DO IPLOT=1,MXMPLOT; CALL IDFDEALLOCATEX(MP(IPLOT)%IDF); ENDDO
 
 N=0; DO IPLOT=1,MXMPLOT; IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)N=N+1; ENDDO
 ALLOCATE(IDF(N),IDFM(1),X(N),IX(N),JX(N),IORDER(N,N))
 IX=0; JX=0; IORDER=0
 DO I=1,SIZE(IDF);  CALL IDFNULLIFY(IDF(I));  ENDDO
 DO I=1,SIZE(IDFM); CALL IDFNULLIFY(IDFM(I)); ENDDO
  
 N=0; DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)THEN
   N=N+1; IF(.NOT.IDFREAD(IDF(N),MP(IPLOT)%IDFNAME,0))EXIT
  ENDIF
 ENDDO

 !## check whether files are equal, speeds up process
 DO I=2,N; IF(.NOT.IDFEQUAL(IDF(1),IDF(I),0))EXIT; ENDDO
 LEQUAL=.FALSE.; IF(I.GT.N)LEQUAL=.TRUE. 
 
 IORDER=0
 
 !## process idf files
 IF(LEQUAL)THEN
  CALL IDFCOPY(IDF(1),IDFM(1))
IRLOOP1: DO IROW=1,IDFM(1)%NROW; DO ICOL=1,IDFM(1)%NCOL
   DO I=1,N; IX(I)=REAL(I); X(I)=IDFGETVAL(IDF(I),IROW,ICOL); ENDDO
   IF(MANAGERSORT_PROCESS(IDF,IX,X,IORDER,IROW,ICOL))EXIT IRLOOP1
  ENDDO; ENDDO IRLOOP1
 ELSE
  !## minimum overlapping size
  IF(.NOT.IDF_EXTENT(N,IDF,IDFM(1),2))RETURN
IRLOOP2: DO IROW=1,IDFM(1)%NROW; DO ICOL=1,IDFM(1)%NCOL
   CALL IDFGETLOC(IDFM(1),IROW,ICOL,XC,YC)
   DO I=1,N; IX(I)=REAL(I); X(I)=IDFGETXYVAL(IDF(1),XC,YC); ENDDO
   IF(MANAGERSORT_PROCESS(IDF,IX,X,IORDER,IROW,ICOL))EXIT IRLOOP2
  ENDDO; ENDDO IRLOOP2
 ENDIF
  
! do i=1,n
!  write(*,'(99I10)') i,(iorder(i,j),j=1,n)
! enddo

 !## get the biggest value to be the order number
 IX=0.0
 DO I=1,N
  M=0
  DO J=1,N
   IF(IORDER(I,J).GT.M)THEN
    M=IORDER(I,J)
    IX(I)=REAL(J)
   ENDIF
  ENDDO
 ENDDO

! do i=1,n
!  write(*,'(2I10)') i,INT(IX(I))
! enddo
   
 !## get right order
 I=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)THEN
   I=I+1
   !## get right iplot number
   DO J=1,N
    IF(IX(J).EQ.I)JX(I)=IPLOT
   ENDDO
  ENDIF
 ENDDO

! do i=1,n
!  write(*,'(3I10)') i,IX(I),JX(I)
! enddo
 
 !## process ordering in correct order
 DO I=1,N
  DO J=1,N
   IF(IX(J).EQ.I)THEN

    !## don't interchange those idf's since it remains on the same position
    IF(JX(I).EQ.JX(J))CYCLE
    
    IPLOT=JX(I)
    !## remove current iplot
    MP(MXMPLOT)=MP(IPLOT)   
    !## copy right one
    MP(IPLOT)  =MP(JX(J))
    !## switch
    MP(JX(J))  =MP(MXMPLOT)

    K    =IX(I)
    IX(I)=IX(J)
    IX(J)=K

   ENDIF
  ENDDO
 ENDDO
 
 MP(MXMPLOT)%IACT=.FALSE.
 MP(MXMPLOT)%ISEL=.FALSE.

 CALL IDFDEALLOCATE(IDF,SIZE(IDF));  DEALLOCATE(IDF)
 CALL IDFDEALLOCATE(IDFM,SIZE(IDFM)); DEALLOCATE(IDFM)
 DEALLOCATE(X,IX,JX,IORDER)
 
 CALL UTL_MESSAGEHANDLE(1)

 !##  fill manager
 CALL MANAGERFILL()
 CALL MANAGERUPDATE()

 END SUBROUTINE MANAGERSORT
 
 !###======================================================================
 SUBROUTINE MANAGERSORT_ALPHA(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER :: IPLOT,N,I,J,K
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IX,JX
 CHARACTER(LEN=52),DIMENSION(:),ALLOCATABLE :: CN
 
 N=0; DO IPLOT=1,MXMPLOT; IF(MP(IPLOT)%ISEL)N=N+1; ENDDO
 ALLOCATE(IX(N),JX(N),CN(N)); IX=0; JX=0; CN=''
 N=0; DO IPLOT=1,MXMPLOT; IF(MP(IPLOT)%ISEL)THEN; N=N+1; CN(N)=MP(IPLOT)%ALIAS; ENDIF; ENDDO
 
 IF(ID.EQ.ID_SORTALPHA_ZA)CALL WSORT(CN,1,N,IFLAGS=SORTDESCEND,IORDER=IX)
 IF(ID.EQ.ID_SORTALPHA_AZ)CALL WSORT(CN,1,N,                   IORDER=IX)

 !## get right order
 I=0; DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL)THEN
   !## get right iplot number
   I=I+1; DO J=1,N; IF(IX(J).EQ.I)JX(I)=IPLOT; ENDDO
  ENDIF
 ENDDO
   
 !## process ordering in correct order
 DO I=1,N
  DO J=1,N
   IF(IX(J).EQ.I)THEN

    !## don't interchange those idf's since it remains on the same position
    IF(JX(I).EQ.JX(J))CYCLE
    
    IPLOT=JX(I)
    !## remove current iplot
    MP(MXMPLOT)=MP(IPLOT)   
    !## copy right one
    MP(IPLOT)  =MP(JX(J))
    !## switch
    MP(JX(J))  =MP(MXMPLOT)

    K    =IX(I)
    IX(I)=IX(J)
    IX(J)=K

   ENDIF
  ENDDO
 ENDDO
 
 MP(MXMPLOT)%IACT=.FALSE.
 MP(MXMPLOT)%ISEL=.FALSE.

 DEALLOCATE(IX,JX,CN)
 
 !##  fill manager
 CALL MANAGERFILL()
 CALL MANAGERUPDATE()

 END SUBROUTINE MANAGERSORT_ALPHA 

 !###======================================================================
 LOGICAL FUNCTION MANAGERSORT_PROCESS(IDF,IX,X,IORDER,IROW,ICOL)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: IDF
 INTEGER,INTENT(IN) :: IROW,ICOL
 INTEGER,INTENT(INOUT),DIMENSION(:) :: IX
 INTEGER,INTENT(INOUT),DIMENSION(:,:) :: IORDER
 REAL,INTENT(INOUT),DIMENSION(:) :: X
 INTEGER :: I,J,N
 
 MANAGERSORT_PROCESS=.TRUE.
 
 N=SIZE(IX)
 
 DO I=1,N; IF(X(I).EQ.IDF(I)%NODATA)X(I)=-999999.99; ENDDO
 !## sort for altitude/value
 CALL WSORT(X,1,N,SORTDESCEND,IX) 
 !## reset values for nodata
 DO I=1,N; IF(X(I).EQ.-999999.99)IX(I)=0.0; ENDDO   
   
 !## set all values to nodata for values "too much" equal to previous values
 J=1; DO I=2,N
  IF(ABS(X(I)-X(J)).LT.0.1)THEN
   IX(I)=0.0; IX(J)=0
  ELSE
   J=I
  ENDIF
 ENDDO   
   
 !## collect sorts per idf-file
 J=0; DO I=1,N
  IF(INT(IX(I)).EQ.0)CYCLE
  J=J+1
  IORDER(INT(IX(I)),I)=IORDER(INT(IX(I)),I)+1 
 ENDDO

 !## all filed in, process can be terminated
 IF(J.EQ.N)THEN
  !# clean progress up to now
  IORDER=0
  DO I=1,N; IORDER(INT(IX(I)),I)=IORDER(INT(IX(I)),I)+1; ENDDO
 ENDIF
  
 MANAGERSORT_PROCESS=.FALSE.
 
 END FUNCTION MANAGERSORT_PROCESS
 
 !###======================================================================
 SUBROUTINE MANAGERDELETE(IQ)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN),OPTIONAL :: IQ
 INTEGER :: IPLOT,JPLOT
 INTEGER :: I

 I=1
 IF(PRESENT(IQ))I=IQ
 IF(I.EQ.1)THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to remove the selected files from the iMOD manager?','Question')
  IF(WINFODIALOG(4).NE.1)RETURN
 ENDIF

 !## delete all selections
 IPLOT=1
 DO WHILE(IPLOT.LE.MXMPLOT)
  !## remove plot
  IF(MP(IPLOT)%ISEL)THEN
   DO JPLOT=IPLOT,MXMPLOT-1
    MP(JPLOT)     =MP(JPLOT+1)
    DRWLIST(JPLOT)=DRWLIST(JPLOT+1)
    ACTLIST(JPLOT)=ACTLIST(JPLOT+1)
    !## deallocate idf if existing after removal
    CALL IDFDEALLOCATEX(MP(JPLOT)%IDF)
   END DO
   DRWLIST(MXMPLOT)=0
   ACTLIST(MXMPLOT)=0
   MP(MXMPLOT)%ISEL=.FALSE.
   MP(MXMPLOT)%IACT=.FALSE.
   !## deallocate idf if existing after removal
   CALL IDFDEALLOCATEX(MP(MXMPLOT)%IDF)
  ELSE
   IPLOT=IPLOT+1
  ENDIF
 END DO

 CALL MANAGERFILL()
 CALL MANAGERUPDATE()

 END SUBROUTINE MANAGERDELETE

 !###======================================================================
 LOGICAL FUNCTION MANAGERGROUP(INFILE,LPLOT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: INFILE
 LOGICAL,INTENT(IN),OPTIONAL :: LPLOT
 INTEGER :: IPLOT,N
 CHARACTER(LEN=256) :: FNAME
 LOGICAL :: LPLT
 
 MANAGERGROUP=.FALSE.
 
 N=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)N=N+1
 ENDDO
 
 IF(N.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Select more than one IDF file in the iMOD Manager to group.','Error')
  RETURN
 ENDIF
 
 IF(PRESENT(INFILE))THEN
  FNAME=INFILE
 ELSE
  IF(.NOT.UTL_WSELECTFILE('iMOD Multi Data File (*.mdf)|*.mdf|',         &
                   SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME, &
                   'Save Multi Data File (*.mdf)'))RETURN
 ENDIF
 
 CALL MDFDEALLOCATE(); CALL MDFALLOCATE()

 N=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)THEN
   N=N+1
   MDF(N)%FNAME     =MP(IPLOT)%IDFNAME
   MDF(N)%ALIAS     =MP(IPLOT)%ALIAS
   MDF(N)%SCOLOR    =MP(IPLOT)%SCOLOR
   MDF(N)%PRFTYPE   =MP(IPLOT)%PRFTYPE
   MDF(N)%LEG%NCLR  =MP(IPLOT)%LEG%NCLR
   MDF(N)%LEG%CGRAD =MP(IPLOT)%LEG%CGRAD
   MDF(N)%LEG%CLASS =MP(IPLOT)%LEG%CLASS
   MDF(N)%LEG%LEGTXT=MP(IPLOT)%LEG%LEGTXT
   MDF(N)%LEG%RGB   =MP(IPLOT)%LEG%RGB
  ENDIF
 ENDDO

 IF(.NOT.WRITEMDF(FNAME,N))THEN
  CALL MDFDEALLOCATE()
  RETURN
 ENDIF
 
 !## delete all idf from manager
 CALL MANAGERDELETE(IQ=0)
 !## add mdf to manager
 LPLT=.TRUE.
 IF(PRESENT(LPLOT))LPLT=LPLOT
 CALL IDFINIT(FNAME,LPLOT=LPLT)

 CALL MDFDEALLOCATE()
 
 MANAGERGROUP=.TRUE.
 
 END FUNCTION MANAGERGROUP

 !###======================================================================
 SUBROUTINE MANAGERUNGROUP()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT,I,J,M,N
 CHARACTER(LEN=256),DIMENSION(:),ALLOCATABLE :: CFNAME

 !## store mdf-files
 M=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.5)THEN
   M=M+1
  ENDIF
 ENDDO
 ALLOCATE(CFNAME(M))
 M=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.5)THEN
   M        =M+1
   CFNAME(M)=MP(IPLOT)%IDFNAME
  ENDIF
 ENDDO

 !## number of mdf files to be ungrouped
 DO I=1,M
  DO IPLOT=1,MXMPLOT
   !## found current mdf to be ungrouped
   IF(MP(IPLOT)%IPLOT.EQ.5.AND.MP(IPLOT)%IDFNAME.EQ.CFNAME(I))EXIT
  ENDDO
  IF(IPLOT.LE.MXMPLOT)THEN
   MP%ISEL       =.FALSE.
   MP(IPLOT)%ISEL=.TRUE.
   !## delete mdf from manager
   CALL MANAGERDELETE(IQ=0)

   IF(READMDF(CFNAME(I),N))THEN
    DO J=1,N
     !## read *.mdf file, only to get selected idf to be plotted
     CALL IDFINIT(MDF(J)%FNAME,LPLOT=.FALSE.)
     DO IPLOT=1,MXMPLOT
      IF(MP(IPLOT)%ISEL)THEN
       MP(IPLOT)%ALIAS     =MDF(J)%ALIAS
       MP(IPLOT)%SCOLOR    =MDF(J)%SCOLOR
       MP(IPLOT)%PRFTYPE   =MDF(J)%PRFTYPE
       MP(IPLOT)%LEG%NCLR  =MDF(J)%LEG%NCLR
       MP(IPLOT)%LEG%CGRAD =MDF(J)%LEG%CGRAD
       MP(IPLOT)%LEG%CLASS =MDF(J)%LEG%CLASS
       MP(IPLOT)%LEG%LEGTXT=MDF(J)%LEG%LEGTXT
       MP(IPLOT)%LEG%RGB   =MDF(J)%LEG%RGB
      ENDIF
     END DO
    ENDDO
    CALL MDFDEALLOCATE()
   ENDIF
  ENDIF
 ENDDO

 DEALLOCATE(CFNAME)

 END SUBROUTINE MANAGERUNGROUP

 !###======================================================================
 SUBROUTINE MANAGERFILL()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IPLOT,IOPT
 CHARACTER(LEN=256),DIMENSION(MXMPLOT) :: ACTIDF   !##which idf is active in manager

 CALL WDIALOGSELECT(ID_DMANAGERPROPERTIES)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IOPT)

 ACTIDF  =''
 ACTLIST =0
 MPW%NACT=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%IACT)THEN
   MPW%NACT=MPW%NACT+1
   I=INDEXNOCASE(MP(IPLOT)%IDFNAME,'\',.TRUE.)+1
   SELECT CASE (IOPT)
    CASE (1)
     ACTIDF(MPW%NACT)=MP(IPLOT)%ALIAS
    CASE (2)
     ACTIDF(MPW%NACT)=(MP(IPLOT)%ALIAS)//'     ('//MP(IPLOT)%IDFNAME(:I-2)//')'
    CASE (3,4)
     ACTIDF(MPW%NACT)=MP(IPLOT)%IDFNAME(:I-1)//TRIM(MP(IPLOT)%ALIAS)
   END SELECT
   IF(MP(IPLOT)%ISEL)ACTLIST(MPW%NACT)=1
  ENDIF
 ENDDO

 IF(IOPT.EQ.4)CALL UTL_GETRELEVANTDIR(ACTIDF,MPW%NACT)

 CALL WDIALOGSELECT(ID_DMANAGERTAB1)
 IF(MPW%NACT.GT.0)THEN
  IF(WINFODIALOGFIELD(ID_DMTABMENU,FIELDSTATE).NE.1)CALL WDIALOGFIELDSTATE(ID_DMTABMENU,1)
  CALL WDIALOGPUTMENU(ID_DMTABMENU,ACTIDF,MPW%NACT,ACTLIST)
 ELSE
  CALL WDIALOGCLEARFIELD(ID_DMTABMENU)
  IF(WINFODIALOGFIELD(ID_DMTABMENU,FIELDSTATE).NE.2)CALL WDIALOGFIELDSTATE(ID_DMTABMENU,2)
 ENDIF

 END SUBROUTINE MANAGERFILL

 !###======================================================================
 SUBROUTINE MANAGERUPDATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT,I,J,NIPF,NIDF,NIFF,NISG,NMDF,NGEN

 CALL WDIALOGSELECT(ID_DMANAGERTAB1)

 MP%ISEL=.FALSE.
 ACTLIST=0
 IF(MPW%NACT.GT.0)THEN
  CALL WDIALOGGETMENU(ID_DMTABMENU,ACTLIST)
  DO IPLOT=1,MXMPLOT; IF(ACTLIST(IPLOT).EQ.1)MP(IPLOT)%ISEL=.TRUE.; END DO
 ENDIF

 NIPF=0  !IPF SELECTED
 NIDF=0  !IDF SELECTED
 NIFF=0  !IFF SELECTED
 NISG=0  !ISG SELECTED
 NMDF=0  !MDF SELECTED
 NGEN=0  !GEN SELECTED
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL)THEN
   IF(MP(IPLOT)%IPLOT.EQ.1)NIDF=NIDF+1
   IF(MP(IPLOT)%IPLOT.EQ.2)NIPF=NIPF+1
   IF(MP(IPLOT)%IPLOT.EQ.3)NIFF=NIFF+1
   IF(MP(IPLOT)%IPLOT.EQ.4)NISG=NISG+1
   IF(MP(IPLOT)%IPLOT.EQ.5)NMDF=NMDF+1
   IF(MP(IPLOT)%IPLOT.EQ.6)NGEN=NGEN+1
  ENDIF
 ENDDO

 CALL WINDOWSELECT(0)

 !## zoomprevious and zoomnext settings
 I=0; IF(ZM%IZOOM.GT.1)I=1
 CALL WMENUSETSTATE(ID_ZOOMPREVIOUS,1,I)
 I=0; IF(ZM%IZOOM.LT.ZM%NZOOM)I=1
 CALL WMENUSETSTATE(ID_ZOOMNEXT,1,I)

 !## always off
 CALL WMENUSETSTATE(ID_SFIGURE,1,0)

 !## nothing active
 I=1
 IF(MPW%NACT.EQ.0)I=0

 IF(WMENUGETSTATE(ID_ACCURACY,1).NE.I)CALL WMENUSETSTATE(ID_ACCURACY,1,I)
 IF(WINFODIALOGFIELD(ID_DRAW,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_DRAW,I)
 IF(WMENUGETSTATE(ID_IMODINFO,1).NE.I)CALL WMENUSETSTATE(ID_IMODINFO,1,I)
 IF(WINFODIALOGFIELD(ID_INFO,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_INFO,I)

 !## nothing selected
 I=1
 IF(SUM(ACTLIST).EQ.0)I=0
 IF(WINFODIALOGFIELD(ID_DELETE,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_DELETE,I)

 I=0; IF(NIDF.GT.1)I=1; CALL WMENUSETSTATE(ID_SORT,1,I)

 !## at least one selected
 I=0
 IF(SUM(ACTLIST).GT.0)I=1
 !## not sustained whenever nod idf,ipf,iff,mdf is selected
 IF(NIDF.EQ.0.AND.NIPF.EQ.0.AND.NIFF.EQ.0.AND.NMDF.EQ.0)I=0
 IF(WMENUGETSTATE(ID_PROFILE,1).NE.I)CALL WMENUSETSTATE(ID_PROFILE,1,I)
 IF(WMENUGETSTATE(ID_TIMESERIES,1).NE.I)CALL WMENUSETSTATE(ID_TIMESERIES,1,I)
 IF(WMENUGETSTATE(ID_MOVIE,1).NE.I)CALL WMENUSETSTATE(ID_MOVIE,1,I)
 !## not sustained to have more than one mdf in the 3d-tool
 IF(NMDF.GT.1)I=0
 IF(WMENUGETSTATE(ID_3DTOOL,1).NE.I)CALL WMENUSETSTATE(ID_3DTOOL,1,I)

 !## legend status
 I=0
 IF(SUM(ACTLIST).GT.0)I=1
 J=0
 IF(NIDF.GT.0)J=J+1
 IF(NIPF.GT.0)J=J+1
 IF(NISG.GT.0)J=J+1
 IF(NIFF.GT.0)J=J+1
 IF(NMDF.GT.0)J=J+1
 IF(NGEN.GT.0)J=J+1
 !## not one single extension selected
 IF(J.NE.1)I=0

 IF(NISG.GT.0)I=0 !## no legend for isg files
 IF(NMDF.GT.1)I=0 !## no legend for mdf files
 !## only if %ileg=1 in case iff,ipf and gen files
 IF(NGEN.GT.0.OR.NIPF.GT.0.OR.NIFF.GT.0)THEN
  DO IPLOT=1,MXMPLOT; IF(ACTLIST(IPLOT).EQ.1)EXIT; END DO
  IF(MP(IPLOT)%ILEG.EQ.0)I=0
 ENDIF
 IF(WMENUGETSTATE(ID_LEGCD,1).NE.I)CALL WMENUSETSTATE(ID_LEGCD,1,I)

 !## not for ipf,iff,gen's
 J=I
 IF(NIPF+NIFF+NGEN.GT.0)J=0
 IF(WMENUGETSTATE(ID_LEGED,1).NE.J)CALL WMENUSETSTATE(ID_LEGED,1,J)
 IF(WMENUGETSTATE(ID_TDLL,1).NE.J)CALL WMENUSETSTATE(ID_TDLL,1,J) !## total domain

 IF(WMENUGETSTATE(ID_CDLL,1).NE.I)CALL WMENUSETSTATE(ID_CDLL,1,I) !## current domain
 IF(WMENUGETSTATE(ID_PLOTLEGEND,1).NE.I)CALL WMENUSETSTATE(ID_PLOTLEGEND,1,I)
 IF(I.EQ.1)THEN
  IF(WMENUGETSTATE(ID_PLOTLEGEND,2).EQ.1)CALL WMENUSETSTATE(ID_LEGENDCOLUMNS,1,I)
 ELSE
  IF(WMENUGETSTATE(ID_LEGENDCOLUMNS,1).NE.I)CALL WMENUSETSTATE(ID_LEGENDCOLUMNS,1,I)
 ENDIF

 IF(NIDF+NIPF+NISG+NIFF+NMDF+NGEN.NE.1)I=0
 IF(WMENUGETSTATE(ID_ADJUSTLEGEND,1).NE.I)CALL WMENUSETSTATE(ID_ADJUSTLEGEND,1,I)
 IF(WINFODIALOGFIELD(ID_LEGEND,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_LEGEND,I)

 I=1
 IF(NIDF.EQ.0.AND.NMDF.EQ.0)I=0 !## only for idf and mdf
 IF(WMENUGETSTATE(ID_CDUV,1).NE.I) CALL WMENUSETSTATE(ID_CDUV,1,I)  !## unique legend CURRENT domain
 IF(WMENUGETSTATE(ID_TDUV,1).NE.I) CALL WMENUSETSTATE(ID_TDUV,1,I)  !## unique legend TOTAL domain
 IF(WMENUGETSTATE(ID_CDLNL,1).NE.I)CALL WMENUSETSTATE(ID_CDLNL,1,I) !## percentile legend CURRENT domain
 IF(WMENUGETSTATE(ID_TDLNL,1).NE.I)CALL WMENUSETSTATE(ID_TDLNL,1,I) !## percentile legend TOTAL domain

 !## more than one selected
 I=0
 IF(SUM(ACTLIST).GT.1)I=1
 IF(NISG.GT.0)I=0
 IF(WMENUGETSTATE(ID_SYNLEGENDS,1).NE.I)CALL WMENUSETSTATE(ID_SYNLEGENDS,1,I)

 !## movement of idf in datamanager
 I=0
 J=0
 IF(SUM(ACTLIST).GT.0)THEN
  IF(MPW%NACT.GT.1.AND.MPW%NACT.LT.MXMPLOT)THEN
   IF(.NOT.MP(1)%ISEL)       I=1
   IF(.NOT.MP(MPW%NACT)%ISEL)J=1
  ENDIF
 ENDIF
 IF(WINFODIALOGFIELD(ID_MOVEUP,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_MOVEUP,I)
 IF(WINFODIALOGFIELD(ID_MOVEDOWN,FIELDSTATE).NE.J)CALL WDIALOGFIELDSTATE(ID_MOVEDOWN,J)

 !## update topography
 CALL TOPO1UPDATEMANAGER() 

 I=1
 IF(MPW%NACT.GE.MXMPLOT)I=0
 IF(WMENUGETSTATE(ID_OPENIDF,1).NE.I)CALL WMENUSETSTATE(ID_OPENIDF,1,I)

 CALL WDIALOGSELECT(ID_DMANAGERTAB1)
 CALL WDIALOGFIELDSTATE(ID_OPEN,I)

 !##  settings regarding IDF's
 I=0
 IF(NIDF.GT.0.OR.NMDF.GT.0)I=1
 IF(WMENUGETSTATE(ID_IDFMAPVALUES,1).NE.I)CALL WMENUSETSTATE(ID_IDFMAPVALUES,1,I)
 IF(WMENUGETSTATE(ID_IDFOPTIONS,1).NE.I)CALL WMENUSETSTATE(ID_IDFOPTIONS,1,I)
 IF(WMENUGETSTATE(ID_IDFEDIT,1).NE.I)CALL WMENUSETSTATE(ID_IDFEDIT,1,I)
 IF(WMENUGETSTATE(ID_MAPEXPORT,1).NE.I)CALL WMENUSETSTATE(ID_MAPEXPORT,1,I)
 IF(WINFODIALOGFIELD(ID_IDFVALUE,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_IDFVALUE,I)

 IF(WMENUGETSTATE(ID_NETCDFFORMAT,1).NE.INETCDF)CALL WMENUSETSTATE(ID_NETCDFFORMAT,1,INETCDF)
 
 !## no export to given extent (yet!)
 IF(WMENUGETSTATE(ID_MAPEXPORT3,1).NE.0)  CALL WMENUSETSTATE(ID_MAPEXPORT3,1,0)
 IF(WMENUGETSTATE(ID_MAPEXPORTNC3,1).NE.0)CALL WMENUSETSTATE(ID_MAPEXPORTNC3,1,0)

 I=0
 IF(NIDF.GT.1.AND.NIPF.EQ.0.AND.NISG.EQ.0.AND.NIFF.EQ.0.AND.NMDF.EQ.0)I=1!THEN
 IF(WMENUGETSTATE(ID_IDFGROUP,1).NE.I)CALL WMENUSETSTATE(ID_IDFGROUP,1,I)
 I=0
 IF(NMDF.GT.0.AND.NIPF.EQ.0.AND.NISG.EQ.0.AND.NIFF.EQ.0.AND.NIDF.EQ.0)I=1!THEN
 IF(WMENUGETSTATE(ID_IDFUNGROUP,1).NE.I)CALL WMENUSETSTATE(ID_IDFUNGROUP,1,I)

 !##  settings regarding IPF's
 I=0
 IF(NIPF.GT.0.AND.NIFF.EQ.0.AND.NISG.EQ.0)I=1
 IF(WMENUGETSTATE(ID_IPFOPTIONS,1).NE.I)CALL WMENUSETSTATE(ID_IPFOPTIONS,1,I)
 IF(WMENUGETSTATE(ID_ANALYSEIPF,1).NE.I) CALL WMENUSETSTATE(ID_ANALYSEIPF,1,I)
 I=0
 IF(NIPF.EQ.1)I=1
 IF(WMENUGETSTATE(ID_EXTRACTIPF,1).NE.I) CALL WMENUSETSTATE(ID_EXTRACTIPF,1,I)
 I=0
 IF(NIPF.EQ.1.AND.NIDF.EQ.0.AND.NIFF.EQ.0.AND.NISG.EQ.0.AND.NMDF.EQ.0)I=1
 IF(WMENUGETSTATE(ID_IPFCONFIGURE,1).NE.I) CALL WMENUSETSTATE(ID_IPFCONFIGURE,1,I)

 !##  settings regarding IFF's
 I=0
 IF(NIFF.GT.0.AND.NIPF.EQ.0.AND.NISG.EQ.0)I=1
 IF(WMENUGETSTATE(ID_IFFOPTIONS,1).NE.I)CALL WMENUSETSTATE(ID_IFFOPTIONS,1,I)
 I=0
 IF(NIFF.EQ.1.AND.NIDF.EQ.0.AND.NIPF.EQ.0.AND.NISG.EQ.0.AND.NMDF.EQ.0)I=1
 IF(WMENUGETSTATE(ID_IFFCONFIGURE,I).NE.1)CALL WMENUSETSTATE(ID_IFFCONFIGURE,1,I)

 !##  settings regarding ISG's
 I=0
 IF(NISG.GT.0.AND.NIFF.EQ.0.AND.NIPF.EQ.0)I=1
 IF(WMENUGETSTATE(ID_ISGOPTIONS,1).NE.I)CALL WMENUSETSTATE(ID_ISGOPTIONS,1,I)
 IF(WMENUGETSTATE(ID_ISGSHOW,1).NE.I)CALL WMENUSETSTATE(ID_ISGSHOW,1,I)
! IF(NISG.GT.1)I=0
 IF(WMENUGETSTATE(ID_ISGEDIT,I).NE.1)CALL WMENUSETSTATE(ID_ISGEDIT,1,I)

 !##  settings regarding GEN's
 I=0
 IF(NGEN.GT.0)I=1
 IF(WMENUGETSTATE(ID_GENOPTIONS,1).NE.I)CALL WMENUSETSTATE(ID_GENOPTIONS,1,I)
 IF(WMENUGETSTATE(ID_GENCONFIGURE,1).NE.I)CALL WMENUSETSTATE(ID_GENCONFIGURE,1,I)
 IF(WMENUGETSTATE(ID_EXTRACTGEN,1).NE.I) CALL WMENUSETSTATE(ID_EXTRACTGEN,1,I)

 I=1
 !## no legend available whenever more than one file or no map is selected
 IF(SUM(ACTLIST).NE.1)I=0
 !## no legend available whenever isg is active
 IF(NISG.GT.0)I=0
 CALL WDIALOGSELECT(ID_DMANAGER)
 !## grey out tab legend
 CALL WDIALOGTABSTATE(ID_DMTAB,ID_DMANAGERTAB4,I)
 !## no comments whenever more than one file is selected AND no tags keyword (TAGS) available
 IF(PREFVAL(7).EQ.'')I=0
 CALL WDIALOGTABSTATE(ID_DMTAB,ID_DMANAGERTAB3,I)

 END SUBROUTINE MANAGERUPDATE

 !###======================================================================
 SUBROUTINE MANAGERINIT()
 !###======================================================================
 IMPLICIT NONE

 CALL WDIALOGLOAD(ID_DMANAGER)

 CALL WDIALOGSELECT(ID_DMANAGERTAB1)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPENIDF,1)
 CALL WDIALOGPUTIMAGE(ID_INFO,ID_ICONINFO,1)
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE,1)
 CALL WDIALOGPUTIMAGE(ID_DRAW,ID_ICONREDRAW,1)
 CALL WDIALOGPUTIMAGE(ID_IDFVALUE,ID_ICONSELECTPOINT,1)
 CALL WDIALOGPUTIMAGE(ID_LEGEND,ID_ICONLEGEND,1)
 CALL WDIALOGPUTIMAGE(ID_MOVEUP,ID_ICONMOVEUP,1)
 CALL WDIALOGPUTIMAGE(ID_MOVEDOWN,ID_ICONMOVEDOWN,1)
 CALL WDIALOGPUTIMAGE(ID_SORT,ID_ICONSORT,1)
 CALL WDIALOGPUTIMAGE(ID_MATH,ID_ICONCALC,1)
 CALL WDIALOGPUTIMAGE(ID_PROPERTIES,ID_ICONPROPERTIES,1)

 CALL WDIALOGSELECT(ID_DMANAGERTAB2)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPENIDF,1)
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE,1)
 CALL WDIALOGPUTIMAGE(ID_DRAW,ID_ICONREDRAW,1)
 CALL WDIALOGPUTIMAGE(ID_MOVEUP,ID_ICONMOVEUP,1)
 CALL WDIALOGPUTIMAGE(ID_MOVEDOWN,ID_ICONMOVEDOWN,1)
 CALL WDIALOGPUTIMAGE(ID_LEGEND,ID_ICONLEGEND,1)

 CALL WDIALOGSELECT(ID_DMANAGERTAB3)
 CALL WDIALOGPUTIMAGE(ID_NEW,ID_ICONNEW,1)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONINFO,1)
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE,1)
 CALL WDIALOGPUTIMAGE(ID_DRAW,ID_ICONREDRAW,1)
 CALL WDIALOGPUTIMAGE(ID_TAGOWNER,ID_ICONTAGOWNER,1)

 CALL WDIALOGSELECT(ID_DMANAGERTAB4)
 CALL WDIALOGPUTIMAGE(ID_LEGEND,ID_ICONLEGEND,1)

 !## nullify pointer in mp() object
! DO I=1,MXMPLOT; NULLIFY(MP(I)%LIDF); END DO

 CALL WDIALOGLOAD(ID_DMANAGERPROPERTIES,ID_DMANAGERPROPERTIES)

 END SUBROUTINE MANAGERINIT

 !###======================================================================
 SUBROUTINE MANAGERPROPERTIES()
 !###======================================================================
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE

 CALL WDIALOGSELECT(ID_DMANAGERPROPERTIES)
 CALL WDIALOGSHOW(0,0,-1,2)
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDOK,IDCANCEL)
      EXIT
     CASE (IDHELP)
       CALL IMODGETHELP('3.3.5','iMOD Manager Properties')
    END SELECT
  END SELECT
 ENDDO
 CALL WDIALOGHIDE()
 CALL MANAGERFILL()
 CALL TOPOGENFILL()

 END SUBROUTINE MANAGERPROPERTIES

 !###======================================================================
 SUBROUTINE MANAGERSHOW()
 !###======================================================================
 IMPLICIT NONE

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_MANAGER,2).EQ.1)THEN
  CALL MANAGERCLOSE()
  RETURN
 ENDIF
 CALL WMENUSETSTATE(ID_MANAGER,2,1)

 CALL WDIALOGSELECT(ID_DMANAGER)
 CALL WDIALOGSHOW(0,65,0,2)
 CALL MANAGERUPDATE()

 END SUBROUTINE MANAGERSHOW

 !###======================================================================
 SUBROUTINE MANAGERCLOSE()
 !###======================================================================
 IMPLICIT NONE

 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_MANAGER,2,0)

 CALL WDIALOGSELECT(ID_DMANAGER)
 CALL WDIALOGHIDE()

 CALL WINDOWSELECT(MPW%IWIN)

 END SUBROUTINE MANAGERCLOSE

END MODULE MOD_MANAGER
