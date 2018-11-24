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
MODULE MOD_MOVIE_PAR

INTEGER,ALLOCATABLE,DIMENSION(:) :: IMANAGER
INTEGER,ALLOCATABLE,DIMENSION(:) :: IFILES
INTEGER(KIND=8),ALLOCATABLE,DIMENSION(:) :: LDATES
INTEGER(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: FDATES
CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: PLOTNAMES,OFNAMES

CHARACTER(LEN=4),DIMENSION(5) :: EXT
DATA EXT/'BMP ','PNG','JPG ','PCX ','AVI '/ !,'MPG'/

END MODULE MOD_MOVIE_PAR