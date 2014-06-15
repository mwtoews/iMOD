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

MODULE MOD_GENPLOT_PAR

INTEGER,PARAMETER         :: MXGEN  =150
TYPE GENOBJ
 LOGICAL                  :: IACT                      !plot type,1=grid,2=ipf,3=iff
 LOGICAL                  :: ISEL                      !plot selected
 INTEGER                  :: ITYPE                     !plot type,1=grid,2=ipf,3=iff
 INTEGER                  :: SYMBOL                    !no.
! INTEGER                  :: FILL                      !0/1
 INTEGER                  :: THICKNESS                 !dikte
 REAL                     :: XMIN,YMIN,XMAX,YMAX       !coordinates of gen
 INTEGER                  :: RGB                       !color
 CHARACTER(LEN=256)       :: GENFNAME                  !name of idf/ipf-file
END TYPE GENOBJ
TYPE(GENOBJ),DIMENSION(MXGEN) :: GEN
CHARACTER(LEN=256),DIMENSION(MXGEN) :: ACTGEN   !##which gen is active in manager
INTEGER,DIMENSION(MXGEN) :: ACTLISTGEN  !##which gen is selected
INTEGER :: NGEN

REAL,POINTER,DIMENSION(:) :: X,Y,XDUMMY,YDUMMY

END MODULE MOD_GENPLOT_PAR
