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
MODULE BMPVAR

INTEGER,PARAMETER :: MXBMP=100           !## max. bitmaps
TYPE BMPOBJ
 REAL :: XMIN,YMIN,XMAX,YMAX,DX,DY       !## coordinates within mplot-box (m)
 INTEGER :: NCOL,NROW,ITYPE,NCLR,COMPR,CDEPT
 INTEGER :: IBITMAP                      !## bitmap handle
 INTEGER :: IACT
! INTEGER :: IX1,IX2,IY1,IY2
 CHARACTER(LEN=256) :: BMPFNAME          !## name of idf/ipf-file
END TYPE BMPOBJ
TYPE(BMPOBJ),DIMENSION(MXBMP) :: BMP
INTEGER :: NBMP

END MODULE

