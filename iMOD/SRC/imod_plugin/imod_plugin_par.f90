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
!!
MODULE MOD_PLUGIN_PAR

USE RESOURCE
IMPLICIT NONE

CHARACTER(LEN=256) :: HELP

!#Be aware that you update PLUGIN_CHECK with the content of PIOBJ!!
TYPE PIOBJ
 CHARACTER(LEN=52) :: PNAME !## Name of the plugin
 INTEGER :: IACT !##activates plugin in plugin-manager if iact=1
 INTEGER :: ID !## menu-id of plugin menu
 INTEGER :: IFLAG  !## if nowait-command IFLAG=1, if wait-command IFLAG=2
 CHARACTER(LEN=256) :: BACK !##checks output-file: PLUG-IN.OUT
 INTEGER,DIMENSION(2) :: IDPROC !## proces id-number related to the plugin executable
END TYPE PIOBJ

TYPE(PIOBJ),POINTER,DIMENSION(:) :: PI1,PI2

INTEGER,DIMENSION(10) :: MENUID
DATA MENUID/ID_PLUGIN1,ID_PLUGIN2,ID_PLUGIN3,ID_PLUGIN4,ID_PLUGIN5, &
            ID_PLUGIN6,ID_PLUGIN7,ID_PLUGIN8,ID_PLUGIN9,ID_PLUGIN10/

END MODULE MOD_PLUGIN_PAR