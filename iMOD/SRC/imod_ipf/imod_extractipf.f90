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
MODULE MOD_EXTRACTIPF

USE WINTERACTER
USE RESOURCE
USE IMODHELP
USE MOD_PREF_PAR, ONLY : PREFVAL
USE IMODVAR, ONLY : IDIAGERROR
USE MODPLOT
USE MOD_IPF_PAR, ONLY : IPF,NIPF
USE MOD_IPFGETVALUE, ONLY : IPFGETVALUE_SELECTLOGICAL,IPFGETVALUE_SELECTLOGICALFIELDS
USE MOD_IPF, ONLY : IPFDEALLOCATE,IPFINIT,IPFREAD
USE MOD_UTL, ONLY : ITOS,UTL_GETUNIT,UTL_WAITMESSAGE,UTL_CREATEDIR,UTL_WSELECTFILE,UTL_INSIDEPOLYGON
USE MOD_POLYGON, ONLY : POLYGON1MAIN
USE MOD_POLYGON_PAR
USE MOD_POLYGON_UTL, ONLY : POLYGON1INIT,POLYGON1CLOSE,POLYGON1FIELDS,POLYGON1IMAGES
USE MOD_POLYGON_DRAW, ONLY : POLYGON1DRAWSHAPE
USE IMOD
USE MOD_OSD, ONLY : OSD_OPEN

INTEGER,PRIVATE :: NSEL

CONTAINS

 !###======================================================================
 SUBROUTINE EXTRACTIPF1MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE

 CALL WDIALOGSELECT(MESSAGE%WIN)

 IACTSHAPES=(/3,1,1,1,3,3/)
 CALL POLYGON1MAIN(ITYPE,MESSAGE)
 CALL EXTRACTIPF1FIELDS()

 SELECT CASE (MESSAGE%WIN)

  CASE (ID_DEXTRACT)
   SELECT CASE(ITYPE)
    CASE (FIELDCHANGED)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (IDF_MENU1)
       CALL POLYGON1FIELDS(ID_DEXTRACT)
       CALL EXTRACTIPF1FIELDS()
      CASE (IDF_CHECK1,IDF_CHECK2)
       CALL EXTRACTIPF1FIELDS()
     END SELECT

    CASE(PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (IDHELP)
       CALL IMODGETHELP('4.2.4','IPF Extract') ! ID 1028
      CASE (IDCANCEL)
       CALL EXTRACTIPF1CLOSE()
      CASE (ID_GET)
       !## fill in ip pointer to become one! --- not taken into account any polygon ...
       CALL IPFGETVALUE_SELECTLOGICAL(1)
       !## adjust selected pointer for polygon(s)
       CALL IPFGETVALUE_SELECTPOLYGON()
       CALL IDFPLOT(1)

      CASE (ID_CLEAR)
       CALL EXTRACTIPF1CLEAR()
      CASE (IDOK)
       IF(EXTRACTIPF1APPLY())CALL EXTRACTIPF1CLOSE()

     END SELECT
   END SELECT

 END SELECT

 END SUBROUTINE EXTRACTIPF1MAIN

 !###======================================================================
 SUBROUTINE EXTRACTIPF1CLEAR()
 !###======================================================================
 IMPLICIT NONE

 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to clear the selection ?','Question')
 IF(WINFODIALOG(4).NE.1)RETURN

 IPF(1)%IP=0
 NSEL=0!SUM(IPF(1)%IP)
 CALL EXTRACTIPF1FIELDS()
 CALL IDFPLOT(1)

 END SUBROUTINE EXTRACTIPF1CLEAR

 !###======================================================================
 LOGICAL FUNCTION EXTRACTIPF1APPLY()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256) :: IPFNAME,DIR1,DIR2,FNAME1,FNAME2,LINE
 CHARACTER(LEN=500) :: STRING
 INTEGER :: IU,JU,I,J,IOS,N,IRAT,IRAT1

 EXTRACTIPF1APPLY=.FALSE.

 IPFNAME=''
 IF(.NOT.UTL_WSELECTFILE('iMOD Point File (*.ipf)|*.ipf|', &
                  SAVEDIALOG+PROMPTON+NONEXPATH+DIRCHANGE+APPENDEXT, &
                  IPFNAME,'Specify New Filename'))RETURN

 JU=UTL_GETUNIT()
 CALL OSD_OPEN(JU,FILE=TRIM(PREFVAL(1))//'\TMP\COPY.BAT',STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not create '//TRIM(PREFVAL(1))//'\TMP\COPY.BAT','Error')
  RETURN
 ENDIF

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=IPFNAME,STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.EQ.0)THEN

  CALL WCURSORSHAPE(CURHOURGLASS)

  N=NSEL 

  WRITE(IU,*) N 
  WRITE(IU,*) IPF(1)%NCOL
  DO I=1,IPF(1)%NCOL
   WRITE(IU,'(A)')  TRIM(IPF(1)%ATTRIB(I))
  END DO
  WRITE(IU,'(I3,A)') IPF(1)%ACOL,','//TRIM(IPF(1)%FEXT)

  I   =INDEXNOCASE(IPF(1)%FNAME,'\',.TRUE.)
  DIR1=IPF(1)%FNAME(1:I-1)
  I   =INDEXNOCASE(IPFNAME,'\',.TRUE.)
  DIR2=IPFNAME(1:I-1)

  IRAT=0
  DO I=1,IPF(1)%NROW
   IF(IPF(1)%IP(I).EQ.INT(5,1))THEN

    IF(IPF(1)%ACOL.NE.0)THEN

     FNAME1=TRIM(DIR1)//'\'//TRIM(IPF(1)%INFO(IPF(1)%ACOL,I))//'.'//TRIM(ADJUSTL(IPF(1)%FEXT))
     FNAME2=TRIM(DIR2)//'\'//TRIM(IPF(1)%INFO(IPF(1)%ACOL,I))//'.'//TRIM(ADJUSTL(IPF(1)%FEXT))
     J=INDEX(FNAME2,'\',.TRUE.)
     IF(J.NE.0)CALL UTL_CREATEDIR(FNAME2(:J-1))

     STRING='copy '//TRIM(FNAME1)//' '//TRIM(FNAME2)
     WRITE(JU,'(A)') TRIM(STRING)

    ENDIF

    LINE='"'//TRIM(IPF(1)%INFO(1,I))//'"'
    DO J=2,IPF(1)%NCOL
     LINE=TRIM(LINE)//',"'//TRIM(IPF(1)%INFO(J,I))//'"'
    END DO
    WRITE(IU,'(A)') TRIM(LINE)

   ENDIF
   CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,IPF(1)%NROW,'Writing results ...')
  ENDDO
  CLOSE(JU)
  CLOSE(IU)

  IF(IPF(1)%ACOL.NE.0)THEN

   CALL WINDOWOUTSTATUSBAR(4,'Copy selected associated files, can take a while ...')
   !## start copying
#if (defined(WINTERACTER9))
   CALL IOSCOMMAND(TRIM(PREFVAL(1))//'\TMP\COPY.BAT',PROCSILENT+PROCBLOCKED+PROCCMDPROC)
#endif
#if (defined(WINTERACTER8))
   CALL IOSCOMMAND(TRIM(PREFVAL(1))//'\TMP\COPY.BAT',PROCSILENT+PROCBLOCKED)
#endif
   CALL WINDOWOUTSTATUSBAR(4,'')

  ENDIF

  CALL WCURSORSHAPE(CURARROW)

  IPF(1)%IP=0
  NSEL=0!SUM(IPF(1)%IP)
  CALL IDFINIT(IDFNAMEGIVEN=IPFNAME,LPLOT=.FALSE.)
  EXTRACTIPF1APPLY=.TRUE.
 ELSE
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD could not open file '//TRIM(IPFNAME),'Error')
 ENDIF
 CALL WCURSORSHAPE(CURARROW)

 END FUNCTION EXTRACTIPF1APPLY

 !###======================================================================
 SUBROUTINE IPFGETVALUE_SELECTPOLYGON()
 !###======================================================================
 IMPLICIT NONE
 REAL :: X,Y
 INTEGER :: I,J,NCRD

 NCRD=0
 DO J=1,SHPNO
  IF(SHPIACT(J).EQ.1)NCRD=MAX(NCRD,SHPNCRD(J))
 ENDDO
 
 DO I=1,IPF(1)%NROW
  IF(IPF(1)%IP(I).EQ.INT(1,1))THEN
   X=IPF(1)%XYZ(1,I)
   Y=IPF(1)%XYZ(2,I)
   IF(NCRD.GT.0)THEN
    DO J=1,SHPNO
     IF(SHPNCRD(J).GT.0)THEN
      !## remove selection outside polygon(s)
      IF(UTL_INSIDEPOLYGON(X,Y,SHPXC(:,J),SHPYC(:,J),SHPNCRD(J)).EQ.1)THEN
       IPF(1)%IP(I)=INT(6,1)
      ENDIF
     ENDIF
    ENDDO
   ELSE
    IPF(1)%IP(I)=INT(6,1)
   ENDIF
   IPF(1)%IP(I)=IPF(1)%IP(I)-1
  ENDIF
 ENDDO
 NSEL=SUM(INT(IPF(1)%IP))/5
  
 CALL EXTRACTIPF1FIELDS()

 END SUBROUTINE IPFGETVALUE_SELECTPOLYGON

 !###======================================================================
 SUBROUTINE EXTRACTIPF1FIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 I=NSEL
 CALL WDIALOGSELECT(ID_DEXTRACT)
 CALL WDIALOGPUTSTRING(IDF_LABEL1,TRIM(ITOS(I))//' points selected')
 I=MIN(I,1)
 CALL WDIALOGFIELDSTATE(ID_CLEAR,I)
 CALL WDIALOGFIELDSTATE(IDOK,I)

 END SUBROUTINE EXTRACTIPF1FIELDS

 !###======================================================================
 SUBROUTINE EXTRACTIPF1INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT

 !## if selected IPF, use that one!
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.2)THEN
   !## allocate memory for ipf-plotting, they will be read in memory and drawn from that
   CALL IPFINIT()
   IF(.NOT.IPFREAD(IPLOT,1))THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not read '//TRIM(MP(IPLOT)%IDFNAME),'Error')
    RETURN
   ENDIF
   EXIT
  ENDIF
 END DO

 IF(IPLOT.GT.MXMPLOT)RETURN

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_EXTRACTIPF,2).EQ.1)THEN
  CALL EXTRACTIPF1CLOSE()
  RETURN
 ENDIF

 CALL MAIN1INACTMODULE(ID_EXTRACTIPF)

 !other module no closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_EXTRACTIPF,2,1)

 NSEL=0
 CALL WDIALOGLOAD(ID_DIPFINFOFIND,ID_DIPFINFOFIND)
 CALL WDIALOGPUTMENU(IDF_MENU1,IPF%ALIAS,NIPF,1)
 CALL WDIALOGPUTMENU(IDF_MENU2,IPF(1)%ATTRIB,IPF(1)%NCOL,1)

 CALL IPFGETVALUE_SELECTLOGICALFIELDS()

 CALL WDIALOGLOAD(ID_DEXTRACT,ID_DEXTRACT)
 CALL POLYGON1IMAGES(ID_DEXTRACT)
 CALL WDIALOGPUTSTRING(IDF_LABEL2,'IPF: '//IPF(1)%ALIAS)

 CALL POLYGON1INIT()
 CALL POLYGON1FIELDS(ID_DEXTRACT)

 CALL WDIALOGSELECT(ID_DEXTRACT)
 CALL WDIALOGSHOW(-0,100,0,2)

 END SUBROUTINE EXTRACTIPF1INIT

 !###======================================================================
 SUBROUTINE EXTRACTIPF1CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IDIAGERROR=1

 IPF(1)%IP=0
 NSEL=0!SUM(IPF(1)%IP)
 CALL IPFDEALLOCATE()

 CALL POLYGON1DRAWSHAPE(1,SHPNO)
 CALL POLYGON1CLOSE()

 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_EXTRACTIPF,2,0)

 CALL WDIALOGSELECT(ID_DEXTRACT)
 CALL WDIALOGUNLOAD()
 CALL WDIALOGSELECT(ID_DIPFINFOFIND)
 CALL WDIALOGUNLOAD()

 IDIAGERROR=0

 CALL IDFPLOT(1)

 END SUBROUTINE EXTRACTIPF1CLOSE

END MODULE MOD_EXTRACTIPF

