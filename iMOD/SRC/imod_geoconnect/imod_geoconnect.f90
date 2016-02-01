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

MODULE MOD_GEOCONNECT

USE WINTERACTER
USE RESOURCE

CONTAINS

 !###======================================================================
 SUBROUTINE GC_PRE(IOPT)
 !###======================================================================
 !# subroutine to manage all preprocessing steps
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPT
 
 IF(IOPT.EQ.1)THEN !# iMODbatch
  CALL GC_READ_IPEST(FNAME,NR) !#FNAME=iPEST-CSV file (stored in "IPEST"-variable and read in GC_READ_PRE), #NR= number of block to be used from iPEST-file (stored in "IPESTNO"-variable and read in GC_READ_PRE).
  CALL GC_PRE_COMPUTE()
 ELSEIF(IOPT.EQ.0)THEN !# iMOD GUI
  !# use CALL GC_READ_IPEST(FNAME,NR) related to button 'iPEST' on dialog
  !# use CALL GC_PRE_COMPUTE() related to button 'Apply' on dialog
 ENDIF
 
 END SUBROUTINE GC_PRE

 !###======================================================================
 SUBROUTINE GC_POST(IOPT)
 !###======================================================================
 !# subroutine to manage all postprocessing steps
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPT
 
 IF(IOPT.EQ.1)THEN !# iMODbatch
  CALL GC_POST_COMPUTE()
 ELSEIF(IOPT.EQ.0)THEN !# iMOD GUI
  !# use CALL GC_POST_COMPUTE() related to button 'Apply' on dialog
 ENDIF
 
 END SUBROUTINE GC_POST

 !###======================================================================
 SUBROUTINE GC_READ_IPEST(FNAME,NR)
 !###======================================================================
 !# subroutine to read IPEST factors from file
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(IN) :: NR
 

 END SUBROUTINE GC_READ_IPEST

 !###======================================================================
 SUBROUTINE GC_PRE_COMPUTE()
 !###======================================================================
 !# subroutine to compute K-values for preprocessing purposes 
 IMPLICIT NONE

 

 END SUBROUTINE GC_PRE_COMPUTE

 !###======================================================================
 SUBROUTINE GC_POST_COMPUTE()
 !###======================================================================
 !# subroutine to compute K-values for postprocessing purposes
 IMPLICIT NONE

 

 END SUBROUTINE GC_POST_COMPUTE

END MODULE MOD_GEOCONNECT