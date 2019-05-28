!!  Copyright (C) Stichting Deltares, 2005-2019.
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
MODULE MOD_IDFGETVALUE_PAR

USE IMODVAR, ONLY : DP_KIND,SP_KIND
USE MOD_IDF_PAR
USE RESOURCE

INTEGER,PARAMETER :: MAXPOL=50
INTEGER :: NIDFS
TYPE(IDFOBJ),DIMENSION(:),ALLOCATABLE :: IDF
CHARACTER(LEN=256),DIMENSION(:),ALLOCATABLE :: SNAMES,ALIAS
REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: XPOL,YPOL
INTEGER,ALLOCATABLE,DIMENSION(:) :: ICOLORIDF,IUNITS
INTEGER,ALLOCATABLE,DIMENSION(:,:) :: IPOSIDF
INTEGER :: ISHOW

INTEGER,PARAMETER :: MXID=19
INTEGER,DIMENSION(MXID) :: ID,IACT
DATA (ID(I),I=1,MXID) /ID_NEW,ID_OPEN,ID_SAVE,ID_SAVEAS,ID_COPY,ID_MANAGER,ID_OPENIDF, &
                       ID_IRDATABASE,ID_PROFILE,ID_IMODINFO,ID_TIMESERIES, &
                       ID_FILE,ID_EDIT,ID_VIEW,ID_TOOLBOX,ID_HELPMAIN,ID_3DTOOL,ID_MAP,ID_MOVIE/
                       
END MODULE MOD_IDFGETVALUE_PAR