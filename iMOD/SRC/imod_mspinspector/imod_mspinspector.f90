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
MODULE MOD_MSPINSPECTOR

USE WINTERACTER
USE RESOURCE
USE MOD_MSPINSPECTOR_PAR
USE MOD_MSPINSPECTOR_UTL
USE MOD_MAIN_UTL, ONLY : MAIN_UTL_INACTMODULE

CONTAINS

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(INOUT) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 
 SELECT CASE (ITYPE)

  !## case field changed
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
  
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDHELP)
    CASE (IDCANCEL)
     CALL MSPINSPECTOR_CLOSE()
   END SELECT
 END SELECT

 END SUBROUTINE MSPINSPECTOR_MAIN

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_OPENFILES()
 !###======================================================================
 IMPLICIT NONE

 END SUBROUTINE MSPINSPECTOR_OPENFILES

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_PLOTLOC() !X,Y,ICODE)
 !###======================================================================
 IMPLICIT NONE
! INTEGER,INTENT(IN) :: ICODE
! REAL(KIND=DP_KIND),INTENT(IN) :: X,Y
! INTEGER :: I,IROW,ICOL,N

 !!## nothing to show
 !IF(ISHOW.EQ.0)RETURN
 !IF(ISHOW.EQ.1)N=1
 !IF(ISHOW.EQ.2)N=NIDFS
 !
 !!## remove all rectangles
 !IF(ICODE.EQ.0)THEN
 ! DO I=1,N; CALL UTL_PLOTLOCATIONIDF(IDF(I),IPOSIDF(1,I),IPOSIDF(2,I)); ENDDO
 !ELSE
 ! DO I=1,N 
 !  IF(IDF(I)%XMIN.LE.X.AND.IDF(I)%XMAX.GE.X.AND. &
 !     IDF(I)%YMIN.LE.Y.AND.IDF(I)%YMAX.GE.Y)THEN
 !   CALL IGRCOLOURN(UTL_INVERSECOLOUR(ICOLORIDF(I))) 
 !   CALL IDFIROWICOL(IDF(I),IROW,ICOL,X,Y)
 !   IF(IROW.NE.IPOSIDF(1,I).OR.ICOL.NE.IPOSIDF(2,I))THEN
 !    !## remove it, previous one
 !    CALL UTL_PLOTLOCATIONIDF(IDF(I),IPOSIDF(1,I),IPOSIDF(2,I))
 !    !## plot new one
 !    IF(ICODE.EQ.1)CALL UTL_PLOTLOCATIONIDF(IDF(I),IROW,ICOL)
 !    IPOSIDF(1,I)=IROW
 !    IPOSIDF(2,I)=ICOL
 !   ENDIF
 !  ENDIF
 ! ENDDO
 !ENDIF

 END SUBROUTINE MSPINSPECTOR_PLOTLOC

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_FIELDS()
 !###======================================================================
 IMPLICIT NONE

 END SUBROUTINE MSPINSPECTOR_FIELDS

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_INIT()
 !###======================================================================
 IMPLICIT NONE

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_MSPANALYSER,2).EQ.1)THEN
  CALL MSPINSPECTOR_CLOSE(); RETURN
 ENDIF

 CALL MAIN_UTL_INACTMODULE(ID_MSPANALYSER)

 !## other module not closed, no approvement given to start this functionality
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_MSPANALYSER,2,1)

 CALL WDIALOGLOAD(ID_DMSPANALYSER,ID_DMSPANALYSER)
 CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE MSPINSPECTOR_INIT
   
END MODULE MOD_MSPINSPECTOR
