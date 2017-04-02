!!  Copyright (C) Stichting Deltares, 2005-2017.
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
MODULE MOD_SCEN_FIELDS

USE WINTERACTER
USE RESOURCE
USE MOD_POLYGON_PAR
USE MOD_SCEN_PAR
USE MOD_UTL, ONLY : UTL_CAP

CONTAINS

 !###======================================================================
 SUBROUTINE SCEN1FIELDS1()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,ITAB

 CALL WDIALOGSELECT(ID_DSCEN1)

 I=0
 IF(SHPNO.GT.0)THEN
  SHPIACT=0
  CALL WDIALOGGETMENU(IDF_MENU1,SHPIACT)
  IF(SUM(SHPIACT).EQ.1)I=1
 ENDIF
! CALL WDIALOGFIELDSTATE(ID_ZOOMSELECT,MIN(1,SUM(SHPIACT)))
 
 CALL WDIALOGSELECT(ID_DSCENARIO)
 IF(I.EQ.0)THEN
  CALL WDIALOGGETTAB(ID_DTAB,ITAB)
  IF(ITAB.EQ.ID_DSCEN2)CALL WDIALOGSETTAB(ID_DTAB,ID_DSCEN1)
 ENDIF
 CALL WDIALOGTABSTATE(ID_DTAB,ID_DSCEN2,I)

 !## refresh current polygon information
 CALL SCEN1ADDDEL(0,'')

 END SUBROUTINE SCEN1FIELDS1

 !###======================================================================
 SUBROUTINE SCEN1FIELDS2()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DSCEN2)

 I=0
 IF(NSDFNAME(SHPI).GT.0)CALL WDIALOGGETMENU(IDF_MENU1,I)
 IF(I.GT.0)I=1
 IF(I.LE.0)I=0
 CALL WDIALOGFIELDSTATE(ID_DELETE,I)
 CALL WDIALOGFIELDSTATE(ID_INFO,I)
 I=1
 IF(NSDFNAME(SHPI).GE.MAXSDF)I=0
 CALL WDIALOGFIELDSTATE(ID_OPEN,I)
 CALL WDIALOGFIELDSTATE(ID_NEW,I)

 END SUBROUTINE SCEN1FIELDS2

 !###======================================================================
 SUBROUTINE SCEN1ADDDEL(CODE,FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: CODE !0=refresh;1=add;2=delete
 INTEGER :: J,K
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=256) :: FNAME2

 CALL WDIALOGSELECT(ID_DSCEN1)
 CALL WDIALOGGETMENU(IDF_MENU1,SHPIACT)
 DO SHPI=1,SHPNO
  IF(SHPIACT(SHPI).EQ.1)EXIT
 END DO

 CALL WDIALOGSELECT(ID_DSCEN2)

 SELECT CASE (CODE)

  !## refresh from tab1
  CASE (0)

   DO K=1,NSDFNAME(SHPI)
    CALL IUPPERCASE(SDFNAME(SHPI,K))
   END DO

  !## add
  CASE (1)

   IF(TRIM(FNAME).EQ.'')THEN
    FNAME2=TRIM(SCNDIR)//'\*.sdf'
    CALL WSELECTFILE('iMOD Scenario Definition File (*.sdf)|*.sdf|',  &
         LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME2,&
         'Load iMOD Scenario File')
    IF(WINFODIALOG(4).NE.1)RETURN
   ELSE
    FNAME2=FNAME
   ENDIF
   DO K=1,NSDFNAME(SHPI)
    IF(TRIM(UTL_CAP(SDFNAME(SHPI,K),'U')).EQ.TRIM(UTL_CAP(FNAME2,'U')))EXIT
   END DO
   IF(K.GT.NSDFNAME(SHPI))THEN
    NSDFNAME(SHPI)=NSDFNAME(SHPI)+1
    SDFNAME(SHPI,NSDFNAME(SHPI))=FNAME2
   ENDIF

  !## delete
  CASE (2)

   CALL WDIALOGGETMENU(IDF_MENU1,J,FNAME2)
   NSDFNAME(SHPI)=NSDFNAME(SHPI)-1
   DO K=J,NSDFNAME(SHPI)
    SDFNAME(SHPI,K)=SDFNAME(SHPI,K+1)
   END DO

 END SELECT

 IF(NSDFNAME(SHPI).GT.0)THEN
  CALL WDIALOGPUTMENU(IDF_MENU1,SDFNAME(SHPI,:),NSDFNAME(SHPI),NSDFNAME(SHPI))
 ELSE
  CALL WDIALOGCLEARFIELD(IDF_MENU1)
 ENDIF

 CALL SCEN1FIELDS2()

 END SUBROUTINE SCEN1ADDDEL

END MODULE MOD_SCEN_FIELDS

