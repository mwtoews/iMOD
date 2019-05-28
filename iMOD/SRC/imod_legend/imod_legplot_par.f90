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
MODULE MOD_LEGPLOT_PAR

USE MODPLOT
USE RESOURCE

USE IMODVAR, ONLY : DP_KIND,SP_KIND
INTEGER :: IDLEGDIALOG,ISHAPECOLOR 
REAL(KIND=DP_KIND),PARAMETER :: LEGTSIZE=7.0D0

INTEGER,DIMENSION(MXCGRAD) :: ID1,ID2,ID3

DATA    (ID1(ICGRAD),ICGRAD=1,MXCGRAD) /IDF_REAL1,IDF_REAL2,IDF_REAL3,IDF_REAL4, &
                                        IDF_REAL5,IDF_REAL6,IDF_REAL7/
DATA    (ID2(ICGRAD),ICGRAD=1,MXCGRAD) /IDF_CHECK1,IDF_CHECK2,IDF_CHECK3,IDF_CHECK4, &
                                        IDF_CHECK5,IDF_CHECK6,IDF_CHECK7/
DATA    (ID3(ICGRAD),ICGRAD=1,MXCGRAD) /IDF_INTEGER1,IDF_INTEGER2,IDF_INTEGER3,IDF_INTEGER4, &
                                        IDF_INTEGER5,IDF_INTEGER6,IDF_INTEGER7/

INTEGER,DIMENSION(7) :: IPOS

END MODULE MOD_LEGPLOT_PAR

