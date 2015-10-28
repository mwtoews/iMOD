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
USE WINTERACTER

INTEGER,PARAMETER :: MAXCOLOUR=50
INTEGER,DIMENSION(MAXCOLOUR) :: ICOLOR
INTEGER,DIMENSION(MXCGRAD,3),SAVE :: CLR

CONTAINS

 !###====================================================================
 SUBROUTINE COLOUR_INIT()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I
 INTEGER,DIMENSION(MXCGRAD) :: IRGB
 
 IRGB(1)=WRGB(64,0,0); CALL WRGBSPLIT(IRGB(1),CLR(1,1),CLR(1,2),CLR(1,3)) !## brown
 IRGB(2)=WRGB(255,0,0); CALL WRGBSPLIT(IRGB(2),CLR(2,1),CLR(2,2),CLR(2,3)) !## red
 IRGB(3)=WRGB(255,0,255); CALL WRGBSPLIT(IRGB(3),CLR(3,1),CLR(3,2),CLR(3,3)) !## purple
 IRGB(4)=WRGB(0,0,255); CALL WRGBSPLIT(IRGB(4),CLR(4,1),CLR(4,2),CLR(4,3)) !## blue
 IRGB(5)=WRGB(0,255,0); CALL WRGBSPLIT(IRGB(5),CLR(5,1),CLR(5,2),CLR(5,3)) !## green
 IRGB(6)=WRGB(255,255,0); CALL WRGBSPLIT(IRGB(6),CLR(6,1),CLR(6,2),CLR(6,3)) !## yellow
 IRGB(7)=WRGB(128,255,255); CALL WRGBSPLIT(IRGB(7),CLR(7,1),CLR(7,2),CLR(7,3)) !## cyan

END SUBROUTINE COLOUR_INIT

END MODULE MOD_COLOURS

