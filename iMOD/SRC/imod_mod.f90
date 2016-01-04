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
MODULE IMOD

 IMPLICIT NONE

 INTERFACE

 SUBROUTINE IDFINIT(IDFNAMEGIVEN,LEGNAME,LPLOT,ISTYLE,LDEACTIVATE,IPFICOL,ILABELS,IPFASSFILES)
  IMPLICIT NONE
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: IDFNAMEGIVEN 
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: LEGNAME
  LOGICAL,INTENT(IN),OPTIONAL :: LPLOT,LDEACTIVATE
  INTEGER,INTENT(IN),OPTIONAL :: ISTYLE
  INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: IPFASSFILES
  INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: ILABELS
  INTEGER,DIMENSION(3),OPTIONAL,INTENT(IN) :: IPFICOL
 END SUBROUTINE

 LOGICAL FUNCTION IDFDRAW(IDF,LEG,UNITS,IP,XMIN,YMIN,XMAX,YMAX,THICKNESS,LPLOT,UMIN,UMAX)
  USE MODPLOT, ONLY : LEGENDOBJ
  USE MOD_IDF_PAR, ONLY : IDFOBJ
  IMPLICIT NONE
  TYPE(IDFOBJ),INTENT(INOUT) :: IDF
  TYPE(LEGENDOBJ),INTENT(INOUT) :: LEG
  REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
  INTEGER,INTENT(IN),DIMENSION(3) :: IP
  INTEGER,INTENT(IN) :: UNITS,THICKNESS
  LOGICAL,INTENT(IN) :: LPLOT
  REAL,INTENT(OUT),OPTIONAL :: UMIN,UMAX
 END FUNCTION IDFDRAW

 END INTERFACE

END MODULE
