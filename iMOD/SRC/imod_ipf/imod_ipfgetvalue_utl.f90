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
MODULE MOD_IPFGETVALUE_UTL

USE WINTERACTER
USE RESOURCE
USE MODPLOT
USE MOD_UTL, ONLY : UTL_CLOSEUNITS
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_POLYGON_DRAW
USE MOD_POLYGON_UTL
USE MOD_POLYGON_PAR
USE MOD_IPF_PAR

CONTAINS

 !###======================================================================
 SUBROUTINE IPFGETVALUE_CLOSE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IIPF,IPLOT

 !## close all files
 CALL UTL_CLOSEUNITS()

 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_ANALYSEIPF,2,0)

 CALL WDIALOGSELECT(ID_DIPFINFO); CALL WDIALOGUNLOAD()

 CALL WDIALOGSELECT(ID_DIPFINFOFIND); CALL WDIALOGUNLOAD()

 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_SFIGURE,1,0)
 CALL WINDOWOUTSTATUSBAR(2,'')
 CALL WINDOWOUTSTATUSBAR(3,'')
 CALL WINDOWOUTSTATUSBAR(4,'')

 CALL IPFGETVALUE_MENUFIELDS(1)

 IIPF=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.2)THEN
   IIPF=IIPF+1
   MP(IPLOT)%SYMBOL=IPF(IIPF)%SYMBOL
   MP(IPLOT)%THICKNESS=IPF(IIPF)%THICKNESS
   MP(IPLOT)%ILEGDLF=IPF(IIPF)%ILEGDLF
   IF(ASSOCIATED(IPF(IIPF)%IP))IPF(IIPF)%IP=0
  ENDIF
 END DO

! CALL IDFPLOTFAST(0)

 END SUBROUTINE IPFGETVALUE_CLOSE
 
 !###======================================================================
 SUBROUTINE IPFGETVALUE_MENUFIELDS(J)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: J
 INTEGER,PARAMETER :: MAXID=18
 INTEGER :: I
 INTEGER,DIMENSION(MAXID) :: ID
 DATA                (ID(I),I=1,MAXID) /ID_NEW,ID_OPEN,ID_SAVE,ID_SAVEAS,ID_FILE,ID_PREFERENCES, &
                                     ID_MANAGER,ID_MAP,ID_TOOLBOX,ID_OPENIDF,ID_PROFILE,      &
                                     ID_IRDATABASE,ID_VIEW,ID_EDIT,ID_IMODINFO,&
                                     ID_TIMESERIES,ID_3DTOOL,ID_MOVIE/

 CALL WINDOWSELECT(0)
 DO I=1,MAXID
  IF(WMENUGETSTATE(ID(I),1).NE.J)CALL WMENUSETSTATE(ID(I),1,J)
 END DO

 END SUBROUTINE IPFGETVALUE_MENUFIELDS
 
END MODULE MOD_IPFGETVALUE_UTL
