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
MODULE MOD_3D_QUERY

USE WINTERACTER
USE RESOURCE
USE MODPLOT
USE MOD_3D_PAR
USE MOD_QUERY_PAR
USE MOD_IPF_PAR, ONLY : ASSF,IPF,MAXATTRIB
USE MOD_IDF, ONLY : IDFIROWICOL,IDFGETVAL,IDFREAD,IDFDEALLOCATE,IDFNULLIFY
USE MOD_UTL, ONLY : UTL_CAP,UTL_WSELECTFILE,UTL_GETUNIT,ITOS

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION IMOD3D_IPF_QUERY_INIT(IIPF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIPF
 INTEGER :: N,M
 INTEGER,DIMENSION(4) :: BHTYPE
 
 IMOD3D_IPF_QUERY_INIT=.FALSE.

 IF(IPFPLOT(IIPF)%IPLOTACOL.EQ.1)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You cannot perform a query on an IPF without associated files turned on.','Error')
  RETURN
 ENDIF
 
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,IPFPLOT(IIPF)%ISELECT(1))
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,IPFPLOT(IIPF)%ISELECT(2))
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK3,IPFPLOT(IIPF)%ISELECT(3))
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK5,IPFPLOT(IIPF)%ISELECT(4))
 CALL WDIALOGPUTDOUBLE(IDF_REAL1,IPFPLOT(IIPF)%RSELECT(1),'(F15.3)')
 CALL WDIALOGPUTDOUBLE(IDF_REAL2,IPFPLOT(IIPF)%RSELECT(2),'(F15.3)')
 CALL WDIALOGPUTDOUBLE(IDF_REAL3,IPFPLOT(IIPF)%RSELECT(3),'(F15.3)')
 CALL WDIALOGFIELDSTATE(IDF_REAL1,IPFPLOT(IIPF)%ISELECT(1))
 CALL WDIALOGFIELDSTATE(IDF_REAL2,IPFPLOT(IIPF)%ISELECT(2))
 CALL WDIALOGFIELDSTATE(IDF_REAL3,IPFPLOT(IIPF)%ISELECT(3))
 CALL WDIALOGFIELDSTATE(IDF_STRING1,IPFPLOT(IIPF)%ISELECT(4))
 CALL WDIALOGFIELDSTATE(ID_OPEN,IPFPLOT(IIPF)%ISELECT(4))
 CALL WDIALOGPUTIMAGE(IDF_COLOUR,ID_ICONLEGEND,1)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN,1)

 IF(IPFPLOT(IIPF)%EXCLCOLOUR.LT.WRGB(0,0,0).OR.IPFPLOT(IIPF)%EXCLCOLOUR.GT.WRGB(255,255,255))THEN
  !## default exclude colour is white
  IPFPLOT(IIPF)%EXCLCOLOUR=WRGB(255,255,255)
 ENDIF
 CALL WDIALOGCOLOUR(IDF_LABEL6,IPFPLOT(IIPF)%EXCLCOLOUR,IPFPLOT(IIPF)%EXCLCOLOUR)
  
 IPFPLOT(IIPF)%IEXCLUDE=MAX(1,MIN(2,IPFPLOT(IIPF)%IEXCLUDE))
 IF(IPFPLOT(IIPF)%IEXCLUDE.EQ.1)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO1)
 N=WINFOGRID(IDF_GRID1,GRIDROWSMAX); ALLOCATE(ISEL(N)); ISEL=1

 !## aparte functie maken
 M=IMOD3D_IPF_QUERY_NUMBEROFIDFS(1)
 IF(M.GT.0)THEN
  CALL WGRIDPUTMENU(IDF_GRID1,6,ALIAS,M,ISEL,N)
  CALL WGRIDPUTMENU(IDF_GRID1,7,ALIAS,M,ISEL,N)
 ENDIF

 IF(.NOT.IMOD3D_IPF_QUERY_NUMBEROFLABELS(IIPF,BHTYPE))RETURN
 !## set initially the z-coordinate for 1d/3d boreholes
 IF(BHTYPE(2).GT.0)THEN
  ISEL=1
 ELSEIF(BHTYPE(4).GT.0)THEN
  ISEL=MIN(3,SIZE(UQLABELS))
 ENDIF
 CALL WGRIDPUTMENU(IDF_GRID1,3,UQLABELS,SIZE(UQLABELS),ISEL,N)
 
 IPFPLOT(IIPF)%QYFFNAME=''

 IMOD3D_IPF_QUERY_INIT=.TRUE.

 END FUNCTION IMOD3D_IPF_QUERY_INIT

 !###======================================================================
 LOGICAL FUNCTION IMOD3D_IPF_QUERY_NUMBEROFLABELS(IIPF,BHTYPE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIPF
 INTEGER,INTENT(OUT),DIMENSION(4) :: BHTYPE
 INTEGER :: I,II,J,K,NUA
 
 IMOD3D_IPF_QUERY_NUMBEROFLABELS=.FALSE.
 
 IF(ASSOCIATED(UQLABELS))DEALLOCATE(UQLABELS); NUA=0; BHTYPE=0
 
 !## get number of unique labels in current selected ipf
 DO I=1,NIPFLIST
  !## part of selected ipf
  IF(IIPF.NE.IPFDLIST(1,I))CYCLE

  BHTYPE(ASSF(I)%ITOPIC)=BHTYPE(ASSF(I)%ITOPIC)+1

  DO J=1,ASSF(I)%NCASS
   !## check if it is still available
   DO K=1,NUA
    IF(TRIM(UTL_CAP(ASSF(I)%ATTRIB(J),'U')).EQ.TRIM(UQLABELS(K)))EXIT
   ENDDO
   IF(K.GT.NUA)THEN
    NUA=NUA+1
    IF(ASSOCIATED(UQLABELS))THEN
     IF(SIZE(UQLABELS).LT.NUA)THEN
      ALLOCATE(UQLABELS_BU(NUA))
      DO II=1,SIZE(UQLABELS); UQLABELS_BU(II)=UQLABELS(II); ENDDO
      DEALLOCATE(UQLABELS); UQLABELS=>UQLABELS_BU
     ENDIF
    ENDIF
    IF(.NOT.ASSOCIATED(UQLABELS))ALLOCATE(UQLABELS(NUA))
    UQLABELS(NUA)=UTL_CAP(ASSF(I)%ATTRIB(J),'U')
   ENDIF
  ENDDO
 ENDDO

 IF(ASSOCIATED(UQLABELS))IMOD3D_IPF_QUERY_NUMBEROFLABELS=.TRUE.

 END FUNCTION IMOD3D_IPF_QUERY_NUMBEROFLABELS

 !###======================================================================
 INTEGER FUNCTION IMOD3D_IPF_QUERY_NUMBEROFIDFS(N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER :: I,M
  
 IMOD3D_IPF_QUERY_NUMBEROFIDFS=0; IF(N.LE.0)RETURN
 
 M=0; DO I=1,MXMPLOT; IF(MP(I)%IPLOT.EQ.1)M=M+1; ENDDO
 IF(M.GT.0)THEN
  IF(ALLOCATED(IDFS))DEALLOCATE(IDFS)
  IF(ALLOCATED(ALIAS))DEALLOCATE(ALIAS)
  ALLOCATE(IDFS(M),ALIAS(M)); IDFS=''; ALIAS=''
  M=0; DO I=1,MXMPLOT
   IF(MP(I)%IPLOT.EQ.1)THEN
    M=M+1; IDFS(M)=MP(I)%IDFNAME; ALIAS(M)=MP(I)%ALIAS
   ENDIF
  ENDDO
 ENDIF
 
 IMOD3D_IPF_QUERY_NUMBEROFIDFS=M
 
 END FUNCTION IMOD3D_IPF_QUERY_NUMBEROFIDFS

 !###======================================================================
 LOGICAL FUNCTION IMOD3D_IPF_QUERY_FIELDS(MESSAGE,IIPF)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: IIPF
 INTEGER :: IROW,ICOL
 
 IMOD3D_IPF_QUERY_FIELDS=.FALSE.

 !## check is iact is 1, then delete option aanzetten
! CALL WGRIDPOS(MESSAGE%X,ICOL,IROW) !## from
 CALL WGRIDPOS(MESSAGE%Y,ICOL,IROW) !## to
 !## check is in between degrey idf2
 SELECT CASE (ICOL)
  !## iact
  CASE (1)
   CALL WGRIDGETCELLCHECKBOX(IDF_GRID1,ICOL,IROW,IPFPLOT(IIPF)%QUERY(IROW)%IACT)
   IF(SUM(IPFPLOT(IIPF)%QUERY%IACT).EQ.0)THEN
    CALL WDIALOGFIELDSTATE(ID_DELETECLAUSE,0)
   ELSE
    CALL WDIALOGFIELDSTATE(ID_DELETECLAUSE,1)
   ENDIF
  !## operator
  CASE (4)
   CALL WGRIDGETCELLMENU(IDF_GRID1,ICOL,IROW,IPFPLOT(IIPF)%QUERY(IROW)%OPER)
   IF(IPFPLOT(IIPF)%QUERY(IROW)%OPER.EQ.4)THEN
    CALL WGRIDSTATECELL(     IDF_GRID1,7,IROW,1)
    CALL WGRIDPUTCELLOPTION( IDF_GRID1,7,IROW,IPFPLOT(IIPF)%QUERY(IROW)%INAME2)
   ELSE
    CALL WGRIDSTATECELL(     IDF_GRID1,7,IROW,0)
   ENDIF
   !## use arithmetic operator 
   IF(IPFPLOT(IIPF)%QUERY(IROW)%OPER.LE.4)THEN
    CALL WGRIDSTATECELL(     IDF_GRID1,5,IROW,0)
    CALL WGRIDSTATECELL(     IDF_GRID1,6,IROW,1)
   ELSE
    CALL WGRIDSTATECELL(     IDF_GRID1,5,IROW,1)
    CALL WGRIDSTATECELL(     IDF_GRID1,6,IROW,0)
   ENDIF
 END SELECT
 
 IMOD3D_IPF_QUERY_FIELDS=.TRUE.
 
 END FUNCTION IMOD3D_IPF_QUERY_FIELDS
 
 !###======================================================================
 LOGICAL FUNCTION IMOD3D_IPF_QUERY_FILL(IIPF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIPF
 INTEGER :: I,N
 
 IMOD3D_IPF_QUERY_FILL=.FALSE.

 CALL WGRIDCLEAR(IDF_GRID1)

 I=1; IF(.NOT.ALLOCATED(IDFS))I=0
 CALL WDIALOGFIELDSTATE(ID_NEW,I)
 CALL WDIALOGFIELDSTATE(ID_LOAD,I)
 IF(I.EQ.0)THEN
  CALL WDIALOGPUTSTRING(IDF_LABEL5,'This functionality is only available whenever at least a single IDF file has been loaded in the iMOD Manager')
 ELSE
  CALL WDIALOGPUTSTRING(IDF_LABEL5,'Start or Load a Query to Display the Local Query Table')
 ENDIF
 
 IF(IPFPLOT(IIPF)%NQUERY.GT.0.AND.IPFPLOT(IIPF)%NQUERY.LT.SIZE(IPFPLOT(IIPF)%QUERY))THEN
  IF(TRIM(IPFPLOT(IIPF)%QYFFNAME).EQ.'')THEN
   CALL WDIALOGFIELDSTATE(ID_SAVE,0)
  ELSE
   CALL WDIALOGFIELDSTATE(ID_SAVE,1)
  ENDIF
  CALL WDIALOGFIELDSTATE(ID_NEWCLAUSE,1)
  CALL WDIALOGFIELDSTATE(ID_SAVEAS,1)
 ELSE
  CALL WDIALOGFIELDSTATE(ID_NEWCLAUSE,0)
  CALL WDIALOGFIELDSTATE(ID_SAVEAS,0)
  CALL WDIALOGFIELDSTATE(ID_SAVE,0)
 ENDIF

 N=0;
 DO I=1,IPFPLOT(IIPF)%NQUERY
  IF(IPFPLOT(IIPF)%QUERY(I)%IACT.GT.0)N=N+1
 ENDDO
 I=MIN(1,N); CALL WDIALOGFIELDSTATE(ID_DELETECLAUSE,I)

 IF(IPFPLOT(IIPF)%NQUERY.LE.0)THEN
  CALL WDIALOGFIELDSTATE(IDF_GRID1,3)
  CALL WDIALOGFIELDSTATE(IDF_LABEL5,1)
  CALL WDIALOGFIELDSTATE(IDF_LABEL4,2)
  CALL WDIALOGFIELDSTATE(IDF_RADIO1,2)
  CALL WDIALOGFIELDSTATE(IDF_RADIO2,2)
  CALL WDIALOGFIELDSTATE(ID_DELETE,0)
  CALL WDIALOGFIELDSTATE(IDF_CHECK4,0)
  CALL WDIALOGFIELDSTATE(IDF_LABEL6,0)
  CALL WDIALOGFIELDSTATE(IDF_COLOUR,0)

 ELSE
 
  CALL WDIALOGFIELDSTATE(ID_DELETE,1)
  CALL WDIALOGFIELDSTATE(IDF_LABEL5,3)
  CALL WDIALOGFIELDSTATE(IDF_GRID1,1)
  CALL WDIALOGFIELDSTATE(IDF_LABEL4,1)
  CALL WDIALOGFIELDSTATE(IDF_RADIO1,1)
  CALL WDIALOGFIELDSTATE(IDF_RADIO2,1)
  CALL WGRIDROWS(IDF_GRID1,IPFPLOT(IIPF)%NQUERY)
  !## fill in query table
  DO I=1,IPFPLOT(IIPF)%NQUERY
   CALL WGRIDPUTCELLCHECKBOX(IDF_GRID1,1,I,IPFPLOT(IIPF)%QUERY(I)%IACT)
   CALL WGRIDPUTCELLOPTION(  IDF_GRID1,2,I,IPFPLOT(IIPF)%QUERY(I)%ANDOR)
   CALL WGRIDPUTCELLOPTION(  IDF_GRID1,3,I,IPFPLOT(IIPF)%QUERY(I)%IFIELD)
   CALL WGRIDPUTCELLOPTION(  IDF_GRID1,4,I,IPFPLOT(IIPF)%QUERY(I)%OPER)
   CALL WGRIDPUTCELLSTRING(  IDF_GRID1,5,I,IPFPLOT(IIPF)%QUERY(I)%CVALUE)
   CALL WGRIDPUTCELLOPTION(  IDF_GRID1,6,I,IPFPLOT(IIPF)%QUERY(I)%INAME1)
   IF(IPFPLOT(IIPF)%QUERY(I)%OPER.EQ.4)THEN
    CALL WGRIDSTATECELL(     IDF_GRID1,7,I,0)
    CALL WGRIDPUTCELLOPTION( IDF_GRID1,7,I,IPFPLOT(IIPF)%QUERY(I)%INAME2)
   ELSE
    CALL WGRIDSTATECELL(     IDF_GRID1,7,I,0)
   ENDIF
   !## use arithmetic operator 
   IF(IPFPLOT(IIPF)%QUERY(I)%OPER.LE.4)THEN
    CALL WGRIDSTATECELL(     IDF_GRID1,5,I,0)
    CALL WGRIDSTATECELL(     IDF_GRID1,6,I,1)
   ELSE
    CALL WGRIDSTATECELL(     IDF_GRID1,5,I,1)
    CALL WGRIDSTATECELL(     IDF_GRID1,6,I,0)
   ENDIF
  ENDDO

  IF(IPFPLOT(IIPF)%IEXCLUDE.EQ.2)THEN
   CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO2)
   CALL WDIALOGFIELDSTATE(IDF_CHECK4,1)
  ELSE
   CALL WDIALOGFIELDSTATE(IDF_CHECK4,0)
  ENDIF
  CALL WDIALOGPUTCHECKBOX(IDF_CHECK4,IPFPLOT(IIPF)%IEXCLCOLOUR)
  I=0; IF(IPFPLOT(IIPF)%IEXCLCOLOUR.EQ.1)I=1
  CALL WDIALOGFIELDSTATE(IDF_LABEL6,I); CALL WDIALOGFIELDSTATE(IDF_COLOUR,I)
 
 ENDIF
 
 IMOD3D_IPF_QUERY_FILL=.TRUE.

 END FUNCTION  IMOD3D_IPF_QUERY_FILL

 !###======================================================================
 LOGICAL FUNCTION IMOD3D_IPF_QUERY_READ(IIPF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIPF
 INTEGER :: I
 
 IMOD3D_IPF_QUERY_READ=.FALSE.

 DO I=1,IPFPLOT(IIPF)%NQUERY
  CALL WGRIDGETCELLCHECKBOX(IDF_GRID1,1,I,IPFPLOT(IIPF)%QUERY(I)%IACT)
  CALL WGRIDGETCELLMENU(    IDF_GRID1,2,I,IPFPLOT(IIPF)%QUERY(I)%ANDOR)
  CALL WGRIDGETCELLMENU(    IDF_GRID1,3,I,IPFPLOT(IIPF)%QUERY(I)%IFIELD)
  CALL WGRIDGETCELLMENU(    IDF_GRID1,4,I,IPFPLOT(IIPF)%QUERY(I)%OPER)
  CALL WGRIDGETCELLSTRING(  IDF_GRID1,5,I,IPFPLOT(IIPF)%QUERY(I)%CVALUE)
  CALL WGRIDGETCELLMENU(    IDF_GRID1,6,I,IPFPLOT(IIPF)%QUERY(I)%INAME1)
  IPFPLOT(IIPF)%QUERY(I)%FNAME1=IDFS(IPFPLOT(IIPF)%QUERY(I)%INAME1)
  IF(IPFPLOT(IIPF)%QUERY(I)%OPER.EQ.4)THEN
   CALL WGRIDGETCELLMENU(   IDF_GRID1,7,I,IPFPLOT(IIPF)%QUERY(I)%INAME2)
   IPFPLOT(IIPF)%QUERY(I)%FNAME2=IDFS(IPFPLOT(IIPF)%QUERY(I)%INAME2)
  ELSE
   IPFPLOT(IIPF)%QUERY(I)%FNAME2=''
  ENDIF
 ENDDO

 IMOD3D_IPF_QUERY_READ=.TRUE.

 END FUNCTION IMOD3D_IPF_QUERY_READ
 
 !###======================================================================
 LOGICAL FUNCTION IMOD3D_IPF_QUERY_NEWCLAUSE(IIPF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIPF
 INTEGER :: I,N
 
 IMOD3D_IPF_QUERY_NEWCLAUSE=.FALSE.

 !## if nothing selected, add new clause at the bottom
 N=0; DO I=1,IPFPLOT(IIPF)%NQUERY
  IF(IPFPLOT(IIPF)%QUERY(I)%IACT.GT.0)N=I
 ENDDO
 IF(N.GT.0)THEN
  !## insert row
  DO I=IPFPLOT(IIPF)%NQUERY+1,N+2,-1
   IPFPLOT(IIPF)%QUERY(I)%IACT  =IPFPLOT(IIPF)%QUERY(I-1)%IACT
   IPFPLOT(IIPF)%QUERY(I)%ANDOR =IPFPLOT(IIPF)%QUERY(I-1)%ANDOR
   IPFPLOT(IIPF)%QUERY(I)%IFIELD=IPFPLOT(IIPF)%QUERY(I-1)%IFIELD
   IPFPLOT(IIPF)%QUERY(I)%OPER  =IPFPLOT(IIPF)%QUERY(I-1)%OPER
   IPFPLOT(IIPF)%QUERY(I)%CVALUE=IPFPLOT(IIPF)%QUERY(I-1)%CVALUE
   IPFPLOT(IIPF)%QUERY(I)%INAME1=IPFPLOT(IIPF)%QUERY(I-1)%INAME1
   IPFPLOT(IIPF)%QUERY(I)%INAME2=IPFPLOT(IIPF)%QUERY(I-1)%INAME2
  ENDDO 
  IPFPLOT(IIPF)%QUERY(I)%IACT  =0
  IPFPLOT(IIPF)%QUERY(I)%ANDOR =1
  IPFPLOT(IIPF)%QUERY(I)%IFIELD=1 !## z
  IPFPLOT(IIPF)%QUERY(I)%OPER  =1
  IPFPLOT(IIPF)%QUERY(I)%CVALUE=''
  IPFPLOT(IIPF)%QUERY(I)%INAME1=1
  IPFPLOT(IIPF)%QUERY(I)%INAME2=1
 ENDIF

 IPFPLOT(IIPF)%NQUERY=IPFPLOT(IIPF)%NQUERY+1

 IMOD3D_IPF_QUERY_NEWCLAUSE=IMOD3D_IPF_QUERY_FILL(IIPF)
 
 END FUNCTION IMOD3D_IPF_QUERY_NEWCLAUSE

 !###======================================================================
 LOGICAL FUNCTION IMOD3D_IPF_QUERY_DELETECLAUSE(IIPF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIPF
 INTEGER :: I,J,N
 
 IMOD3D_IPF_QUERY_DELETECLAUSE=.FALSE.

 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to delete the selected clauses ?','Question')
 IF(WINFODIALOG(4).NE.1)RETURN

 !## delete until nothing found
 DO
  N=0; DO I=1,IPFPLOT(IIPF)%NQUERY
   IF(IPFPLOT(IIPF)%QUERY(I)%IACT.EQ.0)CYCLE
  
   !## delete row
   DO J=I,IPFPLOT(IIPF)%NQUERY-1
    IPFPLOT(IIPF)%QUERY(J)%IACT  =IPFPLOT(IIPF)%QUERY(J+1)%IACT
    IPFPLOT(IIPF)%QUERY(J)%ANDOR =IPFPLOT(IIPF)%QUERY(J+1)%ANDOR
    IPFPLOT(IIPF)%QUERY(J)%IFIELD=IPFPLOT(IIPF)%QUERY(J+1)%IFIELD
    IPFPLOT(IIPF)%QUERY(J)%OPER  =IPFPLOT(IIPF)%QUERY(J+1)%OPER
    IPFPLOT(IIPF)%QUERY(J)%CVALUE=IPFPLOT(IIPF)%QUERY(J+1)%CVALUE
    IPFPLOT(IIPF)%QUERY(J)%INAME1=IPFPLOT(IIPF)%QUERY(J+1)%INAME1
    IPFPLOT(IIPF)%QUERY(J)%INAME2=IPFPLOT(IIPF)%QUERY(J+1)%INAME2
   ENDDO 
   IPFPLOT(IIPF)%QUERY(J)%IACT  =0
   IPFPLOT(IIPF)%QUERY(J)%ANDOR =1
   IPFPLOT(IIPF)%QUERY(J)%IFIELD=1 !## z
   IPFPLOT(IIPF)%QUERY(J)%OPER  =1
   IPFPLOT(IIPF)%QUERY(J)%CVALUE=''
   IPFPLOT(IIPF)%QUERY(J)%INAME1=1
   IPFPLOT(IIPF)%QUERY(J)%INAME2=1
   N=N+1
   EXIT

  ENDDO
  IF(N.EQ.0)EXIT
 ENDDO

 IPFPLOT(IIPF)%NQUERY=IPFPLOT(IIPF)%NQUERY-1

 IMOD3D_IPF_QUERY_DELETECLAUSE=IMOD3D_IPF_QUERY_FILL(IIPF)
 
 END FUNCTION IMOD3D_IPF_QUERY_DELETECLAUSE

 !###======================================================================
 LOGICAL FUNCTION IMOD3D_IPF_QUERY_NEW(IIPF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIPF
 INTEGER :: I

 IMOD3D_IPF_QUERY_NEW=.FALSE.

 IF(IPFPLOT(IIPF)%NQUERY.GT.0)THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to reset the query ?','Question')
  IF(WINFODIALOG(4).NE.1)RETURN
 ENDIF
 
 !## reset all
 DO I=1,SIZE(IPFPLOT(IIPF)%QUERY)
  IPFPLOT(IIPF)%QUERY(I)%IACT  =0
  IPFPLOT(IIPF)%QUERY(I)%ANDOR =1
  IPFPLOT(IIPF)%QUERY(I)%IFIELD=1
  IPFPLOT(IIPF)%QUERY(I)%OPER  =1
  IPFPLOT(IIPF)%QUERY(I)%CVALUE=''
  IPFPLOT(IIPF)%QUERY(I)%INAME1=1
  IPFPLOT(IIPF)%QUERY(I)%INAME2=1
 ENDDO
 
 IPFPLOT(IIPF)%NQUERY=1
 IMOD3D_IPF_QUERY_NEW=IMOD3D_IPF_QUERY_FILL(IIPF)
 
 END FUNCTION IMOD3D_IPF_QUERY_NEW

 !###======================================================================
 LOGICAL FUNCTION IMOD3D_IPF_QUERY_LOAD(IIPF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIPF
 INTEGER :: IU,I,J,IOS
 CHARACTER(LEN=256) :: CL
 CHARACTER(LEN=52) :: CERROR
 
 IMOD3D_IPF_QUERY_LOAD=.FALSE.

 !## select query file
 IF(.NOT.UTL_WSELECTFILE('Load Query File (*.qyf)|*.qyf|',LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,&
      IPFPLOT(IIPF)%QYFFNAME,'Load Query File (*.qyf)'))RETURN
 !## clean existing query
 IF(.NOT.IMOD3D_IPF_QUERY_DELETE(IIPF))RETURN
 IU=UTL_GETUNIT(); OPEN(IU,FILE=IPFPLOT(IIPF)%QYFFNAME,STATUS='OLD',ACTION='READ')
  
 CERROR=''

 READ(IU,*,IOSTAT=IOS) IPFPLOT(IIPF)%NQUERY
 IF(IOS.NE.0)THEN
  CERROR='Cannot read NQUERY in file'
 ELSE
  !## read
  DO I=1,IPFPLOT(IIPF)%NQUERY

   READ(IU,*,IOSTAT=IOS) IPFPLOT(IIPF)%QUERY(I)%IACT
   IF(IOS.NE.0)THEN; CERROR='Cannot read IACT for query '//TRIM(ITOS(I))//' in file.'; EXIT; ENDIF

   READ(IU,*,IOSTAT=IOS) IPFPLOT(IIPF)%QUERY(I)%ANDOR
   IF(IOS.NE.0)THEN; CERROR='Cannot read ANDOR for query '//TRIM(ITOS(I))//' in file.'; EXIT; ENDIF

   READ(IU,*,IOSTAT=IOS)  CL; CL=UTL_CAP(CL,'U')
   IF(IOS.NE.0)THEN; CERROR='Cannot read LABEL for query '//TRIM(ITOS(I))//' in file.'; EXIT; ENDIF

   DO J=1,SIZE(UQLABELS); IF(TRIM(UTL_CAP(UQLABELS(J),'U')).EQ.TRIM(CL))EXIT; ENDDO
   IF(J.GT.SIZE(UQLABELS))THEN; CERROR='Cannot match labels in curent IPF file.'; EXIT; ENDIF
   IPFPLOT(IIPF)%QUERY(I)%IFIELD=J

   READ(IU,*,IOSTAT=IOS) IPFPLOT(IIPF)%QUERY(I)%OPER
   IF(IOS.NE.0)THEN; CERROR='Cannot read OPER for query '//TRIM(ITOS(I))//' in file.'; EXIT; ENDIF

   READ(IU,*,IOSTAT=IOS) IPFPLOT(IIPF)%QUERY(I)%CVALUE
   IF(IOS.NE.0)THEN; CERROR='Cannot read CVALUE for query '//TRIM(ITOS(I))//' in file.'; EXIT; ENDIF

   READ(IU,*,IOSTAT=IOS) IPFPLOT(IIPF)%QUERY(I)%FNAME1
   IPFPLOT(IIPF)%QUERY(I)%FNAME1=UTL_CAP(IPFPLOT(IIPF)%QUERY(I)%FNAME1,'U')
   IF(IOS.NE.0)THEN; CERROR='Cannot read FNAME1 for query '//TRIM(ITOS(I))//' in file.'; EXIT; ENDIF
   DO J=1,SIZE(IDFS); IF(TRIM(UTL_CAP(IDFS(J),'U')).EQ.TRIM(IPFPLOT(IIPF)%QUERY(I)%FNAME1))EXIT; ENDDO
   IF(J.GT.SIZE(IDFS))THEN; CERROR='Cannot match '//TRIM(IPFPLOT(IIPF)%QUERY(I)%FNAME1)//' in curent IDF list.'; EXIT; ENDIF
   IPFPLOT(IIPF)%QUERY(I)%INAME1=J
   
   IF(IPFPLOT(IIPF)%QUERY(I)%OPER.EQ.4)THEN
    READ(IU,*,IOSTAT=IOS) IPFPLOT(IIPF)%QUERY(I)%FNAME2 
    IPFPLOT(IIPF)%QUERY(I)%FNAME2=UTL_CAP(IPFPLOT(IIPF)%QUERY(I)%FNAME2,'U')
    IF(IOS.NE.0)THEN; CERROR='Cannot read FNAME2 for query '//TRIM(ITOS(I))//' in file.'; EXIT; ENDIF
    DO J=1,SIZE(IDFS); IF(TRIM(UTL_CAP(IDFS(J),'U')).EQ.TRIM(IPFPLOT(IIPF)%QUERY(I)%FNAME2))EXIT; ENDDO
    IF(J.GT.SIZE(IDFS))THEN; CERROR='Cannot match '//TRIM(IPFPLOT(IIPF)%QUERY(I)%FNAME2)//' in curent IDF list.'; EXIT; ENDIF
    IPFPLOT(IIPF)%QUERY(I)%INAME2=J
   ELSE
    IPFPLOT(IIPF)%QUERY(I)%INAME2=1
   ENDIF
   
  ENDDO
 ENDIF
 
 CLOSE(IU)

 IF(TRIM(CERROR).NE.'')THEN
  IPFPLOT(IIPF)%NQUERY=0
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(CERROR),'Error')
 ELSE
  !## store results in dialog
  IMOD3D_IPF_QUERY_LOAD=IMOD3D_IPF_QUERY_FILL(IIPF)
 ENDIF

 END FUNCTION IMOD3D_IPF_QUERY_LOAD

 !###======================================================================
 LOGICAL FUNCTION IMOD3D_IPF_QUERY_SAVE(IIPF,ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIPF,ID
 INTEGER :: IU,I,J
 
 IMOD3D_IPF_QUERY_SAVE=.FALSE.

 !## read input first prior to saving
 IF(.NOT.IMOD3D_IPF_QUERY_READ(IIPF))RETURN

 !## select query file
 IF((ID.EQ.ID_SAVE.AND.TRIM(IPFPLOT(IIPF)%QYFFNAME).EQ.'').OR. &
     ID.EQ.ID_SAVEAS)THEN
  IF(.NOT.UTL_WSELECTFILE('Save Query File (*.qyf)|*.qyf|',SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,&
       IPFPLOT(IIPF)%QYFFNAME,'Save Query File (*.qyf)'))RETURN
 ENDIF
 
 IU=UTL_GETUNIT(); OPEN(IU,FILE=IPFPLOT(IIPF)%QYFFNAME,STATUS='UNKNOWN',ACTION='WRITE')
 
 WRITE(IU,'(1X,A)') TRIM(ITOS(IPFPLOT(IIPF)%NQUERY))
 
 !## write all
 DO I=1,IPFPLOT(IIPF)%NQUERY
  WRITE(IU,'(1X,A)') TRIM(ITOS(IPFPLOT(IIPF)%QUERY(I)%IACT))
  WRITE(IU,'(1X,A)') TRIM(ITOS(IPFPLOT(IIPF)%QUERY(I)%ANDOR))
  J=IPFPLOT(IIPF)%QUERY(I)%IFIELD
  WRITE(IU,'(1X,A)') CHAR(39)//TRIM(UQLABELS(J))//CHAR(39)
  WRITE(IU,'(1X,A)') TRIM(ITOS(IPFPLOT(IIPF)%QUERY(I)%OPER))
  WRITE(IU,'(1X,A)') CHAR(39)//TRIM(IPFPLOT(IIPF)%QUERY(I)%CVALUE)//CHAR(39)
  WRITE(IU,'(1X,A)') CHAR(39)//TRIM(IPFPLOT(IIPF)%QUERY(I)%FNAME1)//CHAR(39)
  IF(IPFPLOT(IIPF)%QUERY(I)%OPER.EQ.4)WRITE(IU,'(1X,A) ') CHAR(39)//TRIM(IPFPLOT(IIPF)%QUERY(I)%FNAME2)//CHAR(39)
 ENDDO
 CLOSE(IU)
  
 IMOD3D_IPF_QUERY_SAVE=.TRUE.

 END FUNCTION IMOD3D_IPF_QUERY_SAVE

 !###======================================================================
 LOGICAL FUNCTION IMOD3D_IPF_QUERY_DELETE(IIPF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIPF
 INTEGER :: I

 IMOD3D_IPF_QUERY_DELETE=.FALSE.

 IF(IPFPLOT(IIPF)%NQUERY.GT.0)THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to delete the current query?','Question')
  IF(WINFODIALOG(4).NE.1)RETURN
 ENDIF
 
 !## reset all
 DO I=1,SIZE(IPFPLOT(IIPF)%QUERY)
  IPFPLOT(IIPF)%QUERY(I)%IACT  =0
  IPFPLOT(IIPF)%QUERY(I)%ANDOR =1
  IPFPLOT(IIPF)%QUERY(I)%IFIELD=1
  IPFPLOT(IIPF)%QUERY(I)%OPER  =1
  IPFPLOT(IIPF)%QUERY(I)%CVALUE=''
  IPFPLOT(IIPF)%QUERY(I)%INAME1=1
  IPFPLOT(IIPF)%QUERY(I)%INAME2=1
 ENDDO
 
 IPFPLOT(IIPF)%NQUERY=0
 IMOD3D_IPF_QUERY_DELETE=IMOD3D_IPF_QUERY_FILL(IIPF)
 
 END FUNCTION IMOD3D_IPF_QUERY_DELETE
 
 !###======================================================================
 LOGICAL FUNCTION IMOD3D_IPF_QUERY_EVALUATE_READIDF(IIPF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIPF
 INTEGER :: I,II,J 
 
 IMOD3D_IPF_QUERY_EVALUATE_READIDF=.TRUE.; IF(IPFPLOT(IIPF)%NQUERY.LE.0)RETURN

 IMOD3D_IPF_QUERY_EVALUATE_READIDF=.FALSE.

 !## open idf-files
 ALLOCATE(IDFQ(SIZE(IDFS))); DO I=1,SIZE(IDFQ); CALL IDFNULLIFY(IDFQ(I)); ENDDO
 !## open selected ones only
ILOOP: DO I=1,SIZE(IDFS)
  DO II=1,2
   !## selected idf file
   IF(II.EQ.1)THEN
    J=IPFPLOT(IIPF)%QUERY(I)%INAME1
   ELSE
    J=0; IF(IPFPLOT(IIPF)%QUERY(I)%OPER.EQ.4)J=IPFPLOT(IIPF)%QUERY(I)%INAME2
    IF(J.EQ.0)CYCLE
   ENDIF
   !## already opened
   IF(IDFQ(J)%IU.NE.0)CYCLE
   IF(.NOT.IDFREAD(IDFQ(J),IDFS(J),0))THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot perform query for wells and discards them.','Error')
    RETURN
   ENDIF
  ENDDO
 ENDDO ILOOP
 
 !## allocate memory for queries
 ALLOCATE(LEXQ(IPFPLOT(IIPF)%NQUERY),LEXT(IPFPLOT(IIPF)%NQUERY))

 IMOD3D_IPF_QUERY_EVALUATE_READIDF=.TRUE.

 END FUNCTION IMOD3D_IPF_QUERY_EVALUATE_READIDF
 
 !###======================================================================
 INTEGER FUNCTION IMOD3D_IPF_QUERY_EVALUATE(NPNT,IALL,IIPF,ISEL) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIPF,ISEL,IALL
 INTEGER,INTENT(INOUT) :: NPNT
 INTEGER :: IROW,ICOL,I,II,III,J,JJ,IPLOTTYPE,IOS
 LOGICAL :: L
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: XV
 REAL(KIND=DP_KIND) :: RXV
 CHARACTER(LEN=MAXATTRIB),ALLOCATABLE,DIMENSION(:) :: CV
 CHARACTER(LEN=MAXATTRIB) :: RCV
 INTEGER,ALLOCATABLE,DIMENSION(:) :: TV
 REAL(KIND=DP_KIND),DIMENSION(2) :: ZINT
 INTEGER,DIMENSION(2) :: LINT
 LOGICAL,PARAMETER :: LTRUE=.TRUE.
 LOGICAL,PARAMETER :: LFALSE=.FALSE.

 !## not proven initially
 IMOD3D_IPF_QUERY_EVALUATE=0

 IPLOTTYPE=ASSF(ISEL)%ITOPIC

 !## set intervals in the well logs
 IF(IALL.EQ.2)THEN
  III=0
  DO
   !## all points processed
   III=III+1; IF(III.EQ.NPNT)EXIT   

   !## change coordinates and elevation of borehole and assign exclude colour (white)
   DO I=1,IPFPLOT(IIPF)%NQUERY

    !## get value for interface
    DO II=1,2
     IF(II.EQ.1)THEN
      J=IPFPLOT(IIPF)%QUERY(I)%INAME1
     ELSE
      J=0; IF(IPFPLOT(IIPF)%QUERY(I)%OPER.EQ.4)J=IPFPLOT(IIPF)%QUERY(I)%INAME2
      !## skip this, not clause for in-between
      IF(J.EQ.0)CYCLE
     ENDIF
     CALL IDFIROWICOL(IDFQ(J),IROW,ICOL,XBH(III+1),YBH(III+1))
     IF(IROW.GE.1.AND.IROW.LE.IDFQ(J)%NROW.AND.ICOL.GE.1.AND.ICOL.LE.IDFQ(J)%NCOL)THEN
      ZINT(II)=IDFGETVAL(IDFQ(J),IROW,ICOL)
      !## not possible to sample - skip this one
      IF(ZINT(II).EQ.IDFQ(J)%NODATA)CYCLE
     ELSE
      !## not possible to sample - skip this one
      CYCLE
     ENDIF

     !## next interface is not above - so correct and add an additional interface
     IF(ZBH(III).GT.ZINT(II).AND.ZBH(III+1).LT.ZINT(II))THEN
      CALL IMOD3D_IPF_QUERY_EVALUATE_COR(NPNT,III,III+1,ZINT(II))
      !## make sure to process two interfaces
      III=III+1
     ENDIF

    ENDDO   
   ENDDO

  ENDDO
 ENDIF
 
 !## store entered values
 I=IPFPLOT(IIPF)%NQUERY; ALLOCATE(CV(I),TV(I),XV(I)); CV=''; TV=0; XV=0.0D0
 DO I=1,IPFPLOT(IIPF)%NQUERY
  IF(IPFPLOT(IIPF)%QUERY(I)%OPER.GE.5)THEN
   CV(I)=UTL_CAP(IPFPLOT(IIPF)%QUERY(I)%CVALUE,'U')
   !## try to read it as a number
   READ(CV(I),*,IOSTAT=IOS) XV(I); IF(IOS.EQ.0)TV(I)=1
  ENDIF
 ENDDO
 
 !## all false until proven otherwise
 LEXQ=.FALSE.; LEXT=.FALSE.

 !## evaluate well - read in again levels ...
 DO III=1,NPNT

  !## set false for individual segments
  IF(IALL.EQ.2)LEXQ=.FALSE.
  !## reset variable tv in case where incorrect values are read
  TV=ABS(TV)
  
  !## go through all queries
  DO I=1,IPFPLOT(IIPF)%NQUERY

   !## get value for interface
   LINT=0
   DO II=1,2
    IF(II.EQ.1)THEN
     J=IPFPLOT(IIPF)%QUERY(I)%INAME1
    ELSE
     J=0; IF(IPFPLOT(IIPF)%QUERY(I)%OPER.EQ.4)J=IPFPLOT(IIPF)%QUERY(I)%INAME2
     !## skip this, not clause for in-between
     IF(J.EQ.0)CYCLE
    ENDIF
    CALL IDFIROWICOL(IDFQ(J),IROW,ICOL,XBH(III),YBH(III))
    IF(IROW.GE.1.AND.IROW.LE.IDFQ(J)%NROW.AND.ICOL.GE.1.AND.ICOL.LE.IDFQ(J)%NCOL)THEN
     ZINT(II)=IDFGETVAL(IDFQ(J),IROW,ICOL); IF(ZINT(II).EQ.IDFQ(J)%NODATA)CYCLE
     LINT(II)=1
    ELSE
     !## not possible to sample
     CYCLE
    ENDIF
   ENDDO
 
   SELECT CASE (IPFPLOT(IIPF)%QUERY(I)%OPER)
    !## intersect
    CASE (1)
     IF(III.GT.1)THEN
      IF(LINT(1).EQ.1.AND.ZBH(III-1).GE.ZINT(1).AND.ZBH(III).LE.ZINT(1))LEXQ(I)=.TRUE.
     ENDIF
    !## above
    CASE (2)
     IF(LINT(1).EQ.1.AND.ZBH(III).GE.ZINT(1))LEXQ(I)=.TRUE.
    !## below
    CASE (3)
     IF(LINT(1).EQ.1.AND.ZBH(III).LT.ZINT(1))LEXQ(I)=.TRUE.
    !## in between
    CASE (4)
     IF(LINT(1).EQ.1.AND.LINT(2).EQ.1.AND.ZBH(III).LT.ZINT(1).AND.ZBH(III).GE.ZINT(2))LEXQ(I)=.TRUE.
    !## rest of operators work as logicals
    CASE DEFAULT
     IF(IALL.EQ.1)THEN
      !## vertical borehole
      IF(ASSF(ISEL)%ITOPIC.EQ.2)THEN
       !## read z-coordinate
       IF(IPFPLOT(IIPF)%QUERY(I)%IFIELD.EQ.1)THEN
        READ(ASSF(ISEL)%Z(III),*,IOSTAT=IOS) RXV
        !## skip as values cannot be read
        IF(IOS.NE.0)TV(I)=-1*TV(I)
       ELSE
        !## value to be examined
        JJ=IPFPLOT(IIPF)%QUERY(I)%IFIELD-1
        IF(TV(I).EQ.0)THEN
         RCV=UTL_CAP(ASSF(ISEL)%L(JJ,III),'U')
        ELSE
         READ(ASSF(ISEL)%L(JJ,III),*,IOSTAT=IOS) RXV
         !## skip as values cannot be read
         IF(IOS.NE.0)TV(I)=-1*TV(I)
        ENDIF
       ENDIF
      !## deviated well
      ELSEIF(ASSF(ISEL)%ITOPIC.EQ.4)THEN
       !## read dx-coordinate
       IF(IPFPLOT(IIPF)%QUERY(I)%IFIELD.EQ.1)THEN
        READ(ASSF(ISEL)%DX(III),*,IOSTAT=IOS) RXV
        !## skip as values cannot be read
        IF(IOS.NE.0)TV(I)=-1*TV(I)
       ELSEIF(IPFPLOT(IIPF)%QUERY(I)%IFIELD.EQ.2)THEN
        READ(ASSF(ISEL)%DY(III),*,IOSTAT=IOS) RXV
        !## skip as values cannot be read
        IF(IOS.NE.0)TV(I)=-1*TV(I)
       ELSEIF(IPFPLOT(IIPF)%QUERY(I)%IFIELD.EQ.3)THEN
        READ(ASSF(ISEL)%Z(III),*,IOSTAT=IOS) RXV
        !## skip as values cannot be read
        IF(IOS.NE.0)TV(I)=-1*TV(I)
       ELSE
        JJ=IPFPLOT(IIPF)%QUERY(I)%IFIELD-3
        !## value to be examined
        IF(TV(I).EQ.0)THEN
         RCV=UTL_CAP(ASSF(ISEL)%L(JJ,III),'U')
        ELSE
         READ(ASSF(ISEL)%L(JJ,III),*,IOSTAT=IOS) RXV
         !## skip as values cannot be read
         IF(IOS.NE.0)TV(I)=-1*TV(I)
        ENDIF
       ENDIF
      ENDIF
     ELSE
      !## value to be examined
      IF(TV(I).EQ.0)THEN
       RCV=UTL_CAP(LBH(III),'U')
      ELSE
       READ(LBH(III),*,IOSTAT=IOS) RXV
       !## skip as values cannot be read
       IF(IOS.NE.0)TV(I)=-1*TV(I)
      ENDIF
     ENDIF
   END SELECT
   
   IF(TV(I).GE.0)THEN
    SELECT CASE (IPFPLOT(IIPF)%QUERY(I)%OPER)
     !## =
     CASE (5)
      IF(TV(I).EQ.0)THEN
       IF(RCV.EQ.CV(I))LEXQ(I)=.TRUE.
      ELSEIF(TV(I).EQ.1)THEN
       IF(RXV.EQ.XV(I))LEXQ(I)=.TRUE.
      ENDIF
     !## >=
     CASE (6)
      IF(TV(I).EQ.0)THEN
       IF(RCV.GE.CV(I))LEXQ(I)=.TRUE.
      ELSEIF(TV(I).EQ.1)THEN
       IF(RXV.GE.XV(I))LEXQ(I)=.TRUE.
      ENDIF
     !## <=
     CASE (7)
      IF(TV(I).EQ.0)THEN
       IF(RCV.LE.CV(I))LEXQ(I)=.TRUE.
      ELSEIF(TV(I).EQ.1)THEN
       IF(RXV.LE.XV(I))LEXQ(I)=.TRUE.
      ENDIF
     !## <>
     CASE (8)
      IF(TV(I).EQ.0)THEN
       IF(RCV.NE.CV(I))LEXQ(I)=.TRUE.
      ELSEIF(TV(I).EQ.1)THEN
       IF(RXV.NE.XV(I))LEXQ(I)=.TRUE.
      ENDIF
     !## >
     CASE (9)
      IF(TV(I).EQ.0)THEN
       IF(RCV.GT.CV(I))LEXQ(I)=.TRUE.
      ELSEIF(TV(I).EQ.1)THEN
       IF(RXV.GT.XV(I))LEXQ(I)=.TRUE.
      ENDIF
     !## <
     CASE (10)
      IF(TV(I).EQ.0)THEN
       IF(RCV.LT.CV(I))LEXQ(I)=.TRUE.
      ELSEIF(TV(I).EQ.1)THEN
       IF(RXV.LT.XV(I))LEXQ(I)=.TRUE.
      ENDIF
    END SELECT
   ENDIF
   
  ENDDO  !DO I=1,IPFPLOT(IIPF)%NQUERY
  
! A	        B	    A.AND.B	 A.OR.B	 A.EQV.B  A.NEQV.B
!.TRUE.	    .TRUE.	.TRUE.	 .TRUE.	 .TRUE.	  .FALSE.
!.TRUE.	    .FALSE.	.FALSE.	 .TRUE.	 .FALSE.  .TRUE.
!.FALSE.	.TRUE.	.FALSE.	 .TRUE.	 .FALSE.  .TRUE.
!.FALSE.	.FALSE.	.FALSE.	 .FALSE. .TRUE.	  .FALSE.

  !## not in selection blank out
  IF(IALL.EQ.2)THEN

   !## evaluate lexq() icm and/or
   L=LEXQ(1)
   DO I=2,IPFPLOT(IIPF)%NQUERY
    !## and
    IF(IPFPLOT(IIPF)%QUERY(I)%ANDOR.EQ.1)THEN 
     L=L.AND.LEXQ(I) 
    !## or
    ELSEIF(IPFPLOT(IIPF)%QUERY(I)%ANDOR.EQ.2)THEN
     L=L.OR.LEXQ(I) 
    !## and not
    ELSEIF(IPFPLOT(IIPF)%QUERY(I)%ANDOR.EQ.3)THEN
     L=L.AND..NOT.LEXQ(I)
    ENDIF 
   ENDDO

   IF(.NOT.L)THEN
    !## apply exclude colour
    IF(IPFPLOT(IIPF)%IEXCLCOLOUR.EQ.1)THEN
     CBH(III)=IPFPLOT(IIPF)%EXCLCOLOUR
!     IF(III.GT.1)THEN
!      CBH(III-1)=IPFPLOT(IIPF)%EXCLCOLOUR
!     ENDIF
    !## negative colour as signal to skip parts for drawing
    ELSE
!     IF(III.GT.1)THEN
      CBH(III)=-1*IPFPLOT(IIPF)%EXCLCOLOUR
      RBH(III)=0.0D0
!      CBH(III-1)=-1*IPFPLOT(IIPF)%EXCLCOLOUR
!      RBH(III-1)=0.0D0
!     ENDIF
    ENDIF
   ENDIF
  
  ELSE

   DO I=1,IPFPLOT(IIPF)%NQUERY
    LEXT(I)=LEXT(I).OR.LEXQ(I)
   ENDDO

  ENDIF

 ENDDO  !DO III=1,NPNT
 
 IF(IALL.EQ.1)THEN
 
  !## evaluate lexq() icm and/or
  L=LEXT(1)
  DO I=2,IPFPLOT(IIPF)%NQUERY
   !## and
   IF(IPFPLOT(IIPF)%QUERY(I)%ANDOR.EQ.1)THEN 
    L=L.AND.LEXT(I) 
   !## or
   ELSEIF(IPFPLOT(IIPF)%QUERY(I)%ANDOR.EQ.2)THEN
    L=L.OR.LEXT(I) 
   !## and not
   ELSEIF(IPFPLOT(IIPF)%QUERY(I)%ANDOR.EQ.3)THEN
    L=L.AND..NOT.LEXT(I)
   ENDIF 
  ENDDO
 
 ENDIF
  
 DEALLOCATE(CV,TV,XV); IF(IALL.EQ.2.OR.L)IMOD3D_IPF_QUERY_EVALUATE=1
 
 END FUNCTION IMOD3D_IPF_QUERY_EVALUATE
 
 !###======================================================================
 SUBROUTINE IMOD3D_IPF_QUERY_EVALUATE_COR(NPNT,IPOS1,IPOS2,Z)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: NPNT
 INTEGER,INTENT(IN) :: IPOS1,IPOS2
 REAL(KIND=DP_KIND),INTENT(IN) :: Z
 REAL(KIND=DP_KIND) :: DX,DY,DZ,F
 
 DX=XBH(IPOS2)-XBH(IPOS1)
 DY=YBH(IPOS2)-YBH(IPOS1)
 DZ=ZBH(IPOS2)-ZBH(IPOS1)
 !## z-difference
 F=(Z-ZBH(IPOS1))/DZ
 
 CALL MOD3D_IPF_QUERY_INSERT_INTERFACE(NPNT,IPOS2)

 !## insert new interface at the edge
 XBH(IPOS2)=XBH(IPOS1)+F*DX
 YBH(IPOS2)=YBH(IPOS1)+F*DY
 ZBH(IPOS2)=ZBH(IPOS1)+F*DZ
 !## these stay the same
! CBH(IPOS2)=CBH(IPOS2)
! RBH(IPOS2)=RBH(IPOS2)
 
 END SUBROUTINE IMOD3D_IPF_QUERY_EVALUATE_COR
 
 !###======================================================================
 SUBROUTINE MOD3D_IPF_QUERY_INSERT_INTERFACE(NPNT,IPOS)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: NPNT
 INTEGER,INTENT(IN) :: IPOS
 INTEGER :: I
 
 NPNT=NPNT+1; CALL IMOD3D_IPF_QUERY_RESIZE_VECTOR(NPNT)
 DO I=NPNT,IPOS+1,-1
  XBH(I)=XBH(I-1)
  YBH(I)=YBH(I-1)
  ZBH(I)=ZBH(I-1)
  CBH(I)=CBH(I-1)
  RBH(I)=RBH(I-1)
  LBH(I)=LBH(I-1)
 ENDDO
 
 END SUBROUTINE MOD3D_IPF_QUERY_INSERT_INTERFACE
 
 !###======================================================================
 SUBROUTINE IMOD3D_IPF_QUERY_RESIZE_VECTOR(N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER :: MM,NN,I
 
 IF(N.LE.SIZE(XBH))RETURN
 
 NN=SIZE(XBH); MM=N
 
 ALLOCATE(XBH_DUMMY(MM),YBH_DUMMY(MM),ZBH_DUMMY(MM),CBH_DUMMY(MM),RBH_DUMMY(MM),LBH_DUMMY(MM))

 DO I=1,NN
  XBH_DUMMY(I)=XBH(I)
  YBH_DUMMY(I)=YBH(I)
  ZBH_DUMMY(I)=ZBH(I)
  CBH_DUMMY(I)=CBH(I)
  RBH_DUMMY(I)=RBH(I)
  LBH_DUMMY(I)=LBH(I)
 ENDDO
 
 DEALLOCATE(XBH,YBH,ZBH,CBH,RBH,LBH)
 
 XBH=>XBH_DUMMY
 YBH=>YBH_DUMMY
 ZBH=>ZBH_DUMMY
 CBH=>CBH_DUMMY
 RBH=>RBH_DUMMY
 LBH=>LBH_DUMMY
 
 END SUBROUTINE IMOD3D_IPF_QUERY_RESIZE_VECTOR
 
 !###======================================================================
 SUBROUTINE IMOD3D_IPF_QUERY_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(ISEL))DEALLOCATE(ISEL) 
 IF(ALLOCATED(IDFS))DEALLOCATE(IDFS) 
 IF(ALLOCATED(ALIAS))DEALLOCATE(ALIAS) 
 IF(ALLOCATED(LEXQ))DEALLOCATE(LEXQ)
 IF(ALLOCATED(LEXT))DEALLOCATE(LEXT)
 IF(ALLOCATED(IDFQ))THEN; CALL IDFDEALLOCATE(IDFQ,SIZE(IDFQ)); DEALLOCATE(IDFQ); ENDIF
 IF(ASSOCIATED(UQLABELS))DEALLOCATE(UQLABELS)

 END SUBROUTINE IMOD3D_IPF_QUERY_DEALLOCATE

END MODULE MOD_3D_QUERY