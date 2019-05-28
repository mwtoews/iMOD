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
MODULE MOD_CONFIG

#if (defined(X64DEBUG))
CHARACTER(LEN=20),PARAMETER :: CCONFIG='X64 Debug'
#endif
#if (defined(X64RELEASE))
CHARACTER(LEN=20),PARAMETER :: CCONFIG='X64 Optimized'
#endif
#if (defined(X32DEBUG))
CHARACTER(LEN=20),PARAMETER :: CCONFIG='X32 Debug'
#endif
#if (defined(X32RELEASE))
CHARACTER(LEN=20),PARAMETER :: CCONFIG='X32 Optimized'
#endif

END MODULE MOD_CONFIG
