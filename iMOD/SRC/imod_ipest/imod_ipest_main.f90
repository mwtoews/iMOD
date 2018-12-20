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
MODULE MOD_IPEST_ANALYSER

USE IMODVAR, ONLY : DP_KIND,SP_KIND
USE WINTERACTER
USE RESOURCE

CONTAINS

 !###======================================================================
 SUBROUTINE IPEST_ANALYSE_MAIN(ITYPE,MESSAGE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 
 CALL WDIALOGSELECT(MESSAGE%WIN) 
  
 SELECT CASE (ITYPE)
 
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDCANCEL)
     CALL IPEST_ANALYSE_CLOSE()
    CASE (IDHELP)
   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT
 END SELECT
     
 END SUBROUTINE IPEST_ANALYSE_MAIN 
 
 !###======================================================================
 SUBROUTINE IPEST_ANALYSE_INIT() 
 !###======================================================================
 IMPLICIT NONE 
 
 CALL WINDOWSELECT(0); IF(WMENUGETSTATE(ID_IPESTANALYSER,2).EQ.1)THEN
  CALL IPEST_ANALYSE_CLOSE(); RETURN
 ENDIF

 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_IPESTANALYSER,2,1)
 !## fill in dialog
 CALL WDIALOGLOAD(ID_DIPESTANALYSE,ID_DIPESTANALYSE)

! CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONOPEN,1)
 CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE IPEST_ANALYSE_INIT

  !###======================================================================
 SUBROUTINE IPEST_ANALYSE_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 
 END SUBROUTINE IPEST_ANALYSE_DEALLOCATE

 !###======================================================================
 SUBROUTINE IPEST_ANALYSE_CLOSE()
 !###======================================================================
 IMPLICIT NONE
 
 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_IPESTANALYSER,2,0)
 CALL IPEST_ANALYSE_DEALLOCATE() 
 CALL WDIALOGSELECT(ID_DIPESTANALYSE); CALL WDIALOGUNLOAD()

 END SUBROUTINE IPEST_ANALYSE_CLOSE
 
END MODULE MOD_IPEST_ANALYSER
