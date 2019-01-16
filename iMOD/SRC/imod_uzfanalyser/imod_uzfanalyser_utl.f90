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
MODULE MOD_UZFANALYSER_UTL

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_UZFANALYSER_PAR
USE MOD_GRAPH, ONLY : GRAPHUNITS,GRAPHAREA

CONTAINS

 !###======================================================================
 SUBROUTINE UZFANALYSE_ALLOCATE(NPER,NZ)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NPER,NZ
 INTEGER :: I
 
 CALL UZFANALYSE_DEALLOCATE()
 ALLOCATE(MC(NPER),CPER(NPER))
 DO I=1,NPER
  NULLIFY(MC(I)%Z); NULLIFY(MC(I)%M)
  ALLOCATE(MC(I)%Z(NZ),MC(I)%M(NZ))
 ENDDO

 ALLOCATE(GRAPHUNITS(6,1),GRAPHAREA(4,1))
 GRAPHUNITS(1,1)=0.0D0; GRAPHUNITS(2,1)=0.0D0
 GRAPHUNITS(3,1)=1.0D0; GRAPHUNITS(4,1)=1.0D0
 GRAPHUNITS(5,1)=0.0D0; GRAPHUNITS(6,1)=1.0D0
 GRAPHAREA(1,1) =0.0D0; GRAPHAREA(2,1) =0.0D0
 GRAPHAREA(3,1) =1.0D0; GRAPHAREA(4,1) =1.0D0
 
 END SUBROUTINE UZFANALYSE_ALLOCATE
 
 !###======================================================================
 SUBROUTINE UZFANALYSE_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 IF(ALLOCATED(MC))THEN
  DO I=1,SIZE(MC)
   IF(ASSOCIATED(MC(I)%Z))DEALLOCATE(MC(I)%Z) 
   IF(ASSOCIATED(MC(I)%M))DEALLOCATE(MC(I)%M) 
  ENDDO
  DEALLOCATE(MC)
 ENDIF
 IF(ALLOCATED(CPER))DEALLOCATE(CPER)
 IF(ALLOCATED(GRAPHUNITS))DEALLOCATE(GRAPHUNITS)
 IF(ALLOCATED(GRAPHAREA))DEALLOCATE(GRAPHAREA)
 
 END SUBROUTINE UZFANALYSE_DEALLOCATE 
 
 !###======================================================================
 SUBROUTINE UZFANALYSER_CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IDIAGERROR=1
 
 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_UZFANALYSER,2,0)

 CALL UZFANALYSE_DEALLOCATE()
 CALL WDIALOGSELECT(ID_DUZFANALYSER); CALL WDIALOGUNLOAD()

 IDIAGERROR=0

 END SUBROUTINE UZFANALYSER_CLOSE
  
END MODULE MOD_UZFANALYSER_UTL
