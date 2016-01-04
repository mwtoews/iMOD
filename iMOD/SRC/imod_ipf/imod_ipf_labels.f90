!!  Copyright (C) Stichting Deltares, 2005-2016.
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
MODULE MOD_IPF_LABEL

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : IBACKSLASH,ILABELNAME
USE MODPLOT, ONLY : MP
USE MOD_UTL, ONLY : UTL_FILLARRAY,UTL_READARRAY
USE MOD_IPF_PAR
USE MOD_3D_PAR, ONLY : IPFPLOT

CONTAINS

 !###======================================================================
 SUBROUTINE IMOD3D_LABELS(IIPF,IPLOT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIPF,IPLOT
 INTEGER :: ITYPE,I
 INTEGER,DIMENSION(:),ALLOCATABLE :: ILIST
 TYPE(WIN_MESSAGE) :: MESSAGE

 IF(.NOT.ALLOCATED(IPF))CALL IDFPLOTFAST(1) 

 CALL WDIALOGLOAD(ID_DIPFLABELS,ID_DIPFLABELS)

 IF(ALLOCATED(IPFPLOT))THEN
  CALL WDIALOGPUTCHECKBOX(IDF_CHECK4,IPFPLOT(IIPF)%IFANCY)
  CALL WDIALOGPUTCHECKBOX(IDF_CHECK3,IPFPLOT(IIPF)%ISHADE)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER1,IPFPLOT(IIPF)%ISUB)
  CALL WDIALOGPUTREAL(IDF_REAL1,IPFPLOT(IIPF)%RADIUS,'(F8.2)')
  CALL WDIALOGPUTINTEGER(IDF_INTEGER2,IPFPLOT(IIPF)%ASSCOL1)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER3,IPFPLOT(IIPF)%ASSCOL2)
  CALL WDIALOGPUTOPTION(IDF_MENU3,IPFPLOT(IIPF)%ISTYLE)
 ELSE
  CALL WDIALOGFIELDSTATE(IDF_LABEL3,0)
  CALL WDIALOGFIELDSTATE(IDF_LABEL4,0)
  CALL WDIALOGFIELDSTATE(IDF_LABEL7,0)
  CALL WDIALOGFIELDSTATE(IDF_CHECK3,0)
  CALL WDIALOGFIELDSTATE(IDF_CHECK4,0)
  CALL WDIALOGFIELDSTATE(IDF_MENU3,0)
  CALL WDIALOGFIELDSTATE(IDF_INTEGER1,0)
  CALL WDIALOGFIELDSTATE(IDF_REAL1,0)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER2,MP(IPLOT)%ASSCOL1)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER3,MP(IPLOT)%ASSCOL2)
 ENDIF
 
 ALLOCATE(ILIST(IPF(IIPF)%NCOL))

 ILIST=0
 IF(ABS(MP(IPLOT)%IEQ).GT.0)THEN
  CALL UTL_FILLARRAY(ILIST,IPF(IIPF)%NCOL,ABS(MP(IPLOT)%IEQ))
 ENDIF
 CALL WDIALOGPUTMENU(IDF_MENU1,IPF(IIPF)%ATTRIB,IPF(IIPF)%NCOL,ILIST)   !## plot
 IF(MP(IPLOT)%IEQ.LT.0)CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,0)  !## use colouring for labels
 IF(MP(IPLOT)%IEQ.GE.0)CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,1)  !## use colouring for labels
 CALL WDIALOGPUTOPTION(IDF_MENU2,MP(IPLOT)%TSIZE)  !## textsize
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,IBACKSLASH)
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK5,ILABELNAME)

 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_CHECK4)
      CALL IMOD3D_LABELS_FIELDS()
    END SELECT
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDHELP)
       CALL IMODGETHELP('4.2.2','MMO.IPO.IPFLab')     
     CASE (IDOK)
      CALL WDIALOGGETMENU(IDF_MENU1,ILIST)
      CALL UTL_READARRAY(ILIST,IPF(IIPF)%NCOL,MP(IPLOT)%IEQ)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)  !## use colouring for labels
      IF(I.EQ.0)MP(IPLOT)%IEQ=-1*MP(IPLOT)%IEQ
      CALL WDIALOGGETMENU(IDF_MENU2,MP(IPLOT)%TSIZE)  !## second z
      CALL WDIALOGGETCHECKBOX(IDF_CHECK2,IBACKSLASH)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK5,ILABELNAME)
      IF(ALLOCATED(IPFPLOT))THEN
       CALL WDIALOGGETCHECKBOX(IDF_CHECK4,IPFPLOT(IIPF)%IFANCY)
       CALL WDIALOGGETCHECKBOX(IDF_CHECK3,IPFPLOT(IIPF)%ISHADE)
       CALL WDIALOGGETINTEGER(IDF_INTEGER1,IPFPLOT(IIPF)%ISUB)
       CALL WDIALOGGETREAL(IDF_REAL1,IPFPLOT(IIPF)%RADIUS)
       CALL WDIALOGGETINTEGER(IDF_INTEGER2,IPFPLOT(IIPF)%ASSCOL1)
       CALL WDIALOGGETINTEGER(IDF_INTEGER3,IPFPLOT(IIPF)%ASSCOL2)
       CALL WDIALOGGETMENU(IDF_MENU3,IPFPLOT(IIPF)%ISTYLE)
      ENDIF
      CALL WDIALOGGETINTEGER(IDF_INTEGER2,MP(IPLOT)%ASSCOL1)
      CALL WDIALOGGETINTEGER(IDF_INTEGER3,MP(IPLOT)%ASSCOL2)
      EXIT
     CASE (IDCANCEL)
      EXIT
    END SELECT
  END SELECT
 ENDDO

 CALL WDIALOGUNLOAD()
 DEALLOCATE(ILIST)

 END SUBROUTINE IMOD3D_LABELS

 !###======================================================================
 SUBROUTINE IMOD3D_LABELS_FIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WDIALOGGETCHECKBOX(IDF_CHECK4,I)
 CALL WDIALOGFIELDSTATE(IDF_LABEL3,I)
 CALL WDIALOGFIELDSTATE(IDF_LABEL4,I)
 CALL WDIALOGFIELDSTATE(IDF_LABEL7,I)
 CALL WDIALOGFIELDSTATE(IDF_REAL1,I)
 CALL WDIALOGFIELDSTATE(IDF_INTEGER1,I)
 CALL WDIALOGFIELDSTATE(IDF_CHECK3,I)     
 CALL WDIALOGFIELDSTATE(IDF_MENU3,I)     

 END SUBROUTINE IMOD3D_LABELS_FIELDS
 
END MODULE MOD_IPF_LABEL

