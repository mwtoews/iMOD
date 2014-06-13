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

MODULE IMOD_IDF_PAR

TYPE IDFOBJ
 INTEGER :: IU
 INTEGER :: NCOL
 INTEGER :: NROW
 INTEGER :: IEQ     !=0:equi =1:non-equi
 INTEGER :: ITB     !=0: =1:usage of top/bot information
 INTEGER :: IXV     !=0:storage in x =1:storage in v
 INTEGER :: JD      !=julian date (if neccessary)
 INTEGER :: ILAY    !=ilay of idf (if neccessary)
 INTEGER :: UNITS   !=units
 REAL :: XMIN,YMIN,XMAX,YMAX
 REAL :: DX,DY        !equi.distance if ieq=0
 REAL :: TOP,BOT      !top and bot information
 REAL :: NODATA,DMIN,DMAX
 REAL,DIMENSION(:),POINTER :: SX   !x.coord. network
 REAL,DIMENSION(:),POINTER :: SY   !y.coord. network
 REAL,DIMENSION(:,:),POINTER :: X  !idfvalues in matrix
 REAL,DIMENSION(:),POINTER :: V    !idfvalues in vector
 INTEGER(KIND=2),DIMENSION(:,:),POINTER :: YSEL    !idfvalues in vector, irow/icol
 CHARACTER(LEN=4),DIMENSION(:),POINTER :: COMMENT    !comments
 INTEGER :: NTHREAD
 CHARACTER(LEN=256) :: FNAME  ! name of the idf
END TYPE IDFOBJ

END MODULE IMOD_IDF_PAR
