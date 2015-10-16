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
MODULE MOD_COLOURS

USE MODPLOT

INTEGER,PARAMETER :: MAXCOLOUR=50
INTEGER,DIMENSION(MAXCOLOUR) :: ICOLOR

INTEGER,DIMENSION(MXCGRAD,3) :: CLR
DATA                  ((CLR(I,J),J=1,3),I=1,MXCGRAD) /&
                                   64,0,0,     & !## brown
                                   255,0,0,    & !## red
                                   255,0,255  ,& !## purple
                                   0,0,255    ,& !## blue
                                   0,255,0    ,& !## green
                                   255,255,0  ,& !## yellow
                                   128,255,255/  !## cyan

END MODULE MOD_COLOURS

