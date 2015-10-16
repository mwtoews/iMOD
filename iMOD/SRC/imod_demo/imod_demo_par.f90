!!  Copyright (C) Stichting Deltares, 2005-2015.
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
MODULE MOD_DEMO_PAR

USE RESOURCE
IMPLICIT NONE

TYPE DEMOBJ
 CHARACTER(LEN=52) :: TDNAME !## demo type name
 REAL,POINTER,DIMENSION(:) :: X,Y !#coordinates of the edges of the profile
 INTEGER :: NXY !#Number of cross-section points
 INTEGER :: IDEMO !#Type of demo (e.g. Cross-section=1, 3D-tool=2)
 INTEGER :: CONFLAG !#Configuration flag
 INTEGER :: IFILL !#Display flag (1=solid,2=wireframe,3=solid+wireframe)
 INTEGER :: ACCFLAG !#Accuracy flag
 INTEGER :: IBLOCKLINES !# show cross-section with blocklines
 INTEGER :: IBLOCKFILLS !# show cross-section with filled blocks
END TYPE DEMOBJ

TYPE(DEMOBJ) :: DEMO

END MODULE MOD_DEMO_PAR