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
!!
MODULE MOD_PLUGIN_PAR

USE RESOURCE
IMPLICIT NONE

TYPE PIOBJ
 CHARACTER(LEN=52) :: PNAME
 INTEGER :: IACT,ID
END TYPE PIOBJ

TYPE(PIOBJ),POINTER,DIMENSION(:) :: PI1,PI2

INTEGER,DIMENSION(10) :: MENUID
DATA MENUID/ID_PLUGIN1,ID_PLUGIN2,ID_PLUGIN3,ID_PLUGIN4,ID_PLUGIN5, &
            ID_PLUGIN6,ID_PLUGIN7,ID_PLUGIN8,ID_PLUGIN9,ID_PLUGIN10/

END MODULE MOD_PLUGIN_PAR