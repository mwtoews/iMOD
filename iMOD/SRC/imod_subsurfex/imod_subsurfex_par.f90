!!  Copyright (C) Stichting Deltares, 2005-2017.
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
MODULE MOD_SUBSURFEX_PAR
 
 INTEGER :: CLICKS,POLYGONNR
 INTEGER :: CLEAR
 INTEGER :: NROFCELLS
 INTEGER :: NPARAM
 INTEGER,ALLOCATABLE,DIMENSION(:) :: CELLS 
 INTEGER,ALLOCATABLE,DIMENSION(:) :: DATASELECTION
 
 REAL :: GRIDSIZE,INIXMIN,INIYMIN,INIXMAX,INIYMAX
 REAL :: XMIN,YMIN,XMAX,YMAX,OLDX,OLDY,NEWX,NEWY,FIRSTX,FIRSTY
 REAL,ALLOCATABLE,DIMENSION(:) :: VERTICESX,VERTICESY 
 
 CHARACTER(LEN=50) :: PROJECT 
 CHARACTER(LEN=52),ALLOCATABLE,DIMENSION(:) :: PARAM,HEADER
 CHARACTER(LEN=256),POINTER,DIMENSION(:) :: FOLDERNAMES
 CHARACTER(LEN=256),POINTER,DIMENSION(:) :: PROJECTNAMES 

 LOGICAL :: MOVE,DRAW
 
END MODULE MOD_SUBSURFEX_PAR
