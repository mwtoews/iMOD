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
MODULE MOD_CREATEGEN

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_POLYGON_PAR, ONLY : SHPNO,SHPFILE,IACTSHAPES
USE MOD_POLYGON_DRAW, ONLY : POLYGON1DRAWSHAPE
USE MOD_POLYGON_UTL, ONLY : POLYGON1CLOSE,POLYGON1INIT,POLYGON1IMAGES,POLYGON1FIELDS,POLYGON1SAVELOADSHAPE
USE MOD_POLYGON, ONLY : POLYGON1MAIN
USE MOD_GENPLOT, ONLY : GENDATAGRID
USE MOD_UTL, ONLY : NV,NL,IVAR,VAR
USE MOD_OSD, ONLY : OSD_GETENV

CHARACTER(LEN=256),PRIVATE :: GENFNAME

CONTAINS

 !###======================================================================
 SUBROUTINE CREATEGEN1MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN)           :: ITYPE
 INTEGER :: MSHPNO
 
 !## check polygon actions
 IACTSHAPES=(/3,1,1,3,1,3/)
 IF(GENFNAME.EQ.'')THEN
  CALL POLYGON1MAIN(ITYPE,MESSAGE,IDAT=1)
 ELSE
  CALL POLYGON1MAIN(ITYPE,MESSAGE,IDAT=1,GENFNAME=GENFNAME)
 ENDIF
 
 CALL WDIALOGSELECT(MESSAGE%WIN)
 CALL WDIALOGFIELDSTATE(ID_INFO,MIN(1,SHPNO))
 CALL WDIALOGFIELDSTATE(ID_SAVE,MIN(1,SHPNO))
 
 SELECT CASE(ITYPE)

  CASE(FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT

  CASE(PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (ID_INFO)
     !## read/show current data from memory!     
     CALL GENDATAGRID('') 
    CASE (IDHELP)
     CALL IMODGETHELP('3.2.2','EMO.CreateGEN')
    CASE (IDCANCEL)
     CALL CREATEGEN1CLOSE()
   END SELECT

 END SELECT

 END SUBROUTINE

 !###======================================================================
 SUBROUTINE CREATEGEN1INIT()
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: LEX
 
 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_CREATEGEN,2).EQ.1)THEN
  CALL CREATEGEN1CLOSE()
  RETURN
 ENDIF

 CALL MAIN1INACTMODULE(ID_CREATEGEN)

 !## other module no closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_CREATEGEN,2,1)

 CALL WDIALOGLOAD(ID_DCREATEGEN,ID_DCREATEGEN)
 CALL POLYGON1INIT()
 CALL POLYGON1IMAGES(ID_DCREATEGEN)
 CALL POLYGON1FIELDS(ID_DCREATEGEN)
 CALL WDIALOGPUTIMAGE(ID_INFO,ID_ICONINFO)
 CALL WDIALOGPUTIMAGE(ID_SAVE,ID_ICONSAVE)
 CALL WDIALOGFIELDSTATE(ID_INFO,0)
 CALL WDIALOGFIELDSTATE(ID_SAVE,0)
 GENFNAME=''
 
 CALL WDIALOGSELECT(ID_DCREATEGEN); CALL WDIALOGSHOW(-0,100,0,2)

 END SUBROUTINE CREATEGEN1INIT

 !###======================================================================
 SUBROUTINE CREATEGEN1CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IDIAGERROR=1

 CALL POLYGON1DRAWSHAPE(1,SHPNO); CALL POLYGON1CLOSE()
 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_CREATEGEN,2,0)
 CALL WDIALOGSELECT(ID_DCREATEGEN); CALL WDIALOGUNLOAD()

 IDIAGERROR=0

 END SUBROUTINE CREATEGEN1CLOSE

END MODULE MOD_CREATEGEN
