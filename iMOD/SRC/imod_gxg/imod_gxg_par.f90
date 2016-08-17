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

MODULE MOD_GXG_PAR

 INTEGER :: IBATCH,ISEL
 CHARACTER(LEN=256) :: GXG_MVIDFNAME,GXG_RESDIR,GXG_IDFNAME,GXG_GENFNAME
 INTEGER :: GXG_NYEAR,GXG_SYEAR,GXG_EYEAR,GXG_NLAYER,GXG_STARTMONTH,GXG_HGLG3
 INTEGER,POINTER,DIMENSION(:,:) :: GXG_IPERIOD
 INTEGER,POINTER,DIMENSION(:) :: GXG_IYEAR,GXG_ILAYER

END MODULE MOD_GXG_PAR