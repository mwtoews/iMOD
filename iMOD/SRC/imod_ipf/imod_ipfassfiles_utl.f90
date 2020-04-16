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
MODULE MOD_IPFASSFILE_UTL

USE MOD_IPF_PAR

CONTAINS

 !###===============================================================================
 SUBROUTINE IPFCLOSEASSFILE()
 !###===============================================================================
 IMPLICIT NONE
 INTEGER :: I

 IF(.NOT.ALLOCATED(ASSF))RETURN

 DO I=1,SIZE(ASSF)

  IF(ASSOCIATED(ASSF(I)%ATTRIB)) DEALLOCATE(ASSF(I)%ATTRIB);  NULLIFY(ASSF(I)%ATTRIB)
  IF(ASSOCIATED(ASSF(I)%NODATA)) DEALLOCATE(ASSF(I)%NODATA);  NULLIFY(ASSF(I)%NODATA)
  IF(ASSOCIATED(ASSF(I)%MEASURE))DEALLOCATE(ASSF(I)%MEASURE); NULLIFY(ASSF(I)%MEASURE)
  IF(ASSOCIATED(ASSF(I)%IDATE))  DEALLOCATE(ASSF(I)%IDATE);   NULLIFY(ASSF(I)%IDATE)
  IF(ASSOCIATED(ASSF(I)%L))      DEALLOCATE(ASSF(I)%L);       NULLIFY(ASSF(I)%L)
  IF(ASSOCIATED(ASSF(I)%Z))      DEALLOCATE(ASSF(I)%Z);       NULLIFY(ASSF(I)%Z)
  IF(ASSOCIATED(ASSF(I)%DX))     DEALLOCATE(ASSF(I)%DX);      NULLIFY(ASSF(I)%DX)
  IF(ASSOCIATED(ASSF(I)%DY))     DEALLOCATE(ASSF(I)%DY);      NULLIFY(ASSF(I)%DY)

 END DO
 
 DEALLOCATE(ASSF)

 END SUBROUTINE IPFCLOSEASSFILE

 !###===============================================================================
 SUBROUTINE IPFASSFILEALLOCATE(N)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER :: I
 
 IF(ALLOCATED(ASSF))RETURN
 ALLOCATE(ASSF(N))
 
 DO I=1,SIZE(ASSF)

  NULLIFY(ASSF(I)%ATTRIB)
  NULLIFY(ASSF(I)%NODATA)
  NULLIFY(ASSF(I)%MEASURE)
  NULLIFY(ASSF(I)%IDATE)
  NULLIFY(ASSF(I)%L)
  NULLIFY(ASSF(I)%Z)
  NULLIFY(ASSF(I)%DX)
  NULLIFY(ASSF(I)%DY)

 END DO

 END SUBROUTINE IPFASSFILEALLOCATE

END MODULE MOD_IPFASSFILE_UTL