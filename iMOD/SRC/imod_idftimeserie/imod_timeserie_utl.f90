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

MODULE MOD_IDFTIMESERIE_UTL

USE WINTERACTER
USE RESOURCE
USE MOD_UTL, ONLY : JD,JDATETOIDATE,UTL_FILLDATES,IDATETOGDATE

CONTAINS

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_GETMINMAXX(XMIN,XMAX,XINT,IFX,IDURATION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDURATION
 INTEGER,INTENT(OUT) :: IFX
 REAL,INTENT(OUT) :: XMIN,XMAX,XINT
 INTEGER :: IY,IM,ID

 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IFX)
 IF(IFX.EQ.0)RETURN

 IF(IDURATION.EQ.0)THEN

  !## get axes information on dialog
  CALL WDIALOGGETINTEGER(IDF_INTEGER1,ID)
  CALL WDIALOGGETMENU(IDF_MENU1,IM)
  CALL WDIALOGGETINTEGER(IDF_INTEGER2,IY)

  XMIN=REAL(JD(IY,IM,ID))
  CALL WDIALOGGETINTEGER(IDF_INTEGER3,ID)
  CALL WDIALOGGETMENU(IDF_MENU2,IM)
  CALL WDIALOGGETINTEGER(IDF_INTEGER4,IY)

  XMAX=REAL(JD(IY,IM,ID))
  CALL WDIALOGGETREAL(IDF_REAL3,XINT)

 ELSE

  CALL WDIALOGGETREAL(IDF_REAL6,XMIN)
  CALL WDIALOGGETREAL(IDF_REAL5,XMAX)
  CALL WDIALOGGETREAL(IDF_REAL7,XINT)

 ENDIF

 XINT=MAX(XINT,(XMAX-XMIN)/100.0)

 END SUBROUTINE IDFTIMESERIE_GETMINMAXX

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_GETMINMAXY(YMIN,YMAX,YINT,IFY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IFY
 REAL,INTENT(OUT) :: YMIN,YMAX,YINT

 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,IFY)
 IF(IFY.EQ.0)RETURN

 !## get axes information on dialog
 CALL WDIALOGGETREAL(IDF_REAL1,YMIN)
 CALL WDIALOGGETREAL(IDF_REAL2,YMAX)
 CALL WDIALOGGETREAL(IDF_REAL4,YINT)

 IF(YINT.LE.0.0) YINT=(YMAX-YMIN)/10.0

 END SUBROUTINE IDFTIMESERIE_GETMINMAXY

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_GETMINMAXY2(Y2MIN,Y2MAX,Y2INT,IFY2)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IFY2
 REAL,INTENT(OUT) :: Y2MIN,Y2MAX,Y2INT

 CALL WDIALOGGETCHECKBOX(IDF_CHECK5,IFY2)
 IF(IFY2.EQ.0)RETURN

 !## get axes information on dialog
 CALL WDIALOGGETREAL(IDF_REAL10,Y2MIN)
 CALL WDIALOGGETREAL(IDF_REAL9 ,Y2MAX)
 CALL WDIALOGGETREAL(IDF_REAL8 ,Y2INT)

 IF(Y2INT.LE.0.0)  Y2INT=(Y2MAX-Y2MIN)/10.0

 END SUBROUTINE IDFTIMESERIE_GETMINMAXY2

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_PUTMINMAXX(XMIN,XMAX,XINT,IDURATION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDURATION
 REAL,INTENT(IN) :: XMIN,XMAX,XINT
 INTEGER :: IY,IM,ID,IDATE,IFX,NDAY

 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IFX); IF(IFX.EQ.1)RETURN

 IF(IDURATION.EQ.0)THEN !## put axes information on dialog

  IDATE=JDATETOIDATE(INT(XMIN))
  CALL IDATETOGDATE(IDATE,IY,IM,ID)
  CALL WDIALOGPUTOPTION(IDF_MENU1,IM)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER2,IY)

  NDAY=WDATEDAYSINMONTH(IY,IM)
  CALL WDIALOGRANGEINTEGER(IDF_INTEGER1,1,NDAY)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER1,MIN(ID,NDAY))

  IDATE=JDATETOIDATE(INT(XMAX))
  CALL IDATETOGDATE(IDATE,IY,IM,ID)
  CALL WDIALOGPUTOPTION(IDF_MENU2,IM)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER4,IY)

  NDAY=WDATEDAYSINMONTH(IY,IM)
  CALL WDIALOGRANGEINTEGER(IDF_INTEGER3,1,NDAY)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER3,MIN(ID,NDAY))
  CALL WDIALOGPUTREAL(IDF_REAL3,XINT,'(F10.2)')

 ELSE

  CALL WDIALOGPUTREAL(IDF_REAL6,XMIN)
  CALL WDIALOGPUTREAL(IDF_REAL5,XMAX)
  CALL WDIALOGPUTREAL(IDF_REAL7,XINT)

 ENDIF

 END SUBROUTINE IDFTIMESERIE_PUTMINMAXX

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_PUTMINMAXY(YMIN,YMAX,YINT)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: YMIN,YMAX,YINT
 INTEGER :: IFY

 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,IFY); IF(IFY.EQ.1)RETURN

 CALL WDIALOGPUTREAL(IDF_REAL1,YMIN)
 CALL WDIALOGPUTREAL(IDF_REAL2,YMAX)
 CALL WDIALOGPUTREAL(IDF_REAL4,YINT,'(F10.2)')

 END SUBROUTINE IDFTIMESERIE_PUTMINMAXY

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_PUTMINMAXY2(Y2MIN,Y2MAX,Y2INT)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: Y2MIN,Y2MAX,Y2INT
 INTEGER :: IFY2

 CALL WDIALOGGETCHECKBOX(IDF_CHECK5,IFY2); IF(IFY2.EQ.1)RETURN

 CALL WDIALOGPUTREAL(IDF_REAL10,Y2MIN)
 CALL WDIALOGPUTREAL(IDF_REAL9 ,Y2MAX)
 CALL WDIALOGPUTREAL(IDF_REAL8 ,Y2INT,'(F10.2)')

 END SUBROUTINE IDFTIMESERIE_PUTMINMAXY2

 !###======================================================================
 SUBROUTINE IDFTIMESERIE_FIELDS(IDURATION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDURATION
 INTEGER :: I,J !,JULD1,JULD2
 INTEGER,DIMENSION(26) :: ID
 DATA ID/IDF_INTEGER1,IDF_INTEGER2,IDF_INTEGER3,IDF_INTEGER4, &
         IDF_MENU1,IDF_MENU2,IDF_LABEL1,IDF_LABEL2,IDF_LABEL5,IDF_LABEL6,IDF_REAL3, &   !## x-as
         IDF_LABEL3,IDF_LABEL4,IDF_REAL1,IDF_REAL2,IDF_LABEL7,IDF_REAL4,IDF_LABEL15,IDF_STRING2,    &   !## y-as
         IDF_LABEL9,IDF_LABEL10,IDF_LABEL11,IDF_LABEL12,IDF_REAL5,IDF_REAL6,IDF_REAL7/  !## x-as (perc)

! CALL WDIALOGSELECT(ID_DTIMESERIESTAB2) <-- wordt ook vanuit ipfgetvalue aangeroepen
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
 IF(IDURATION.EQ.0)THEN
  DO J=1,11; CALL WDIALOGFIELDSTATE(ID(J),I); END DO
 ELSE
  DO J=20,26; CALL WDIALOGFIELDSTATE(ID(J),I); END DO
 ENDIF

 !## y-as
 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,I)
 DO J=12,19; CALL WDIALOGFIELDSTATE(ID(J),I); END DO

 !## from date
 CALL UTL_FILLDATES(IDF_INTEGER2,IDF_MENU1,IDF_INTEGER1) 
 !## to date
 CALL UTL_FILLDATES(IDF_INTEGER4,IDF_MENU2,IDF_INTEGER3) 

 END SUBROUTINE IDFTIMESERIE_FIELDS

END MODULE MOD_IDFTIMESERIE_UTL

