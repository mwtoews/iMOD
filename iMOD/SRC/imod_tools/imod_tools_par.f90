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
MODULE MOD_TOOLS_PAR

INTEGER :: TOOLSNTOPIC,TOOLSNDIR,TOOLSID
CHARACTER(LEN=256) :: TOOLSBROWSENAME,TOOLSDIR,ADIR,FNAME,LINE
CHARACTER(LEN=3),ALLOCATABLE,DIMENSION(:) :: CLAY
CHARACTER(LEN=4),ALLOCATABLE,DIMENSION(:) :: CYEAR
INTEGER,ALLOCATABLE,DIMENSION(:) :: ILAY,IYEAR
INTEGER,ALLOCATABLE,DIMENSION(:) :: COPYSHPIACT
INTEGER,POINTER,DIMENSION(:,:) :: IPERIOD
CHARACTER(LEN=3) :: DUMCLAY  !## cause lahey90 will not take this
CHARACTER(LEN=4) :: DUMCYEAR !## cause lahey90 will not take this
INTEGER(KIND=2),ALLOCATABLE,DIMENSION(:) :: IPLIST

END MODULE MOD_TOOLS_PAR