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

MODULE MOD_AHNFILTER_PAR

 INTEGER        :: NSCRIT     !## surface yes/no (cells!)
 REAL           :: LOCCRIT    !## no local depression/upconing whenever max-min>loccrit (m)
 REAL           :: XCRIT      !## building/strong edge (m)
 INTEGER        :: DPW        !## size window upconing
 REAL           :: DP1        !## depression percentile
 REAL           :: DP2        !## upconing percentile
 REAL           :: CORXCRIT   !## max. change to become surf.level (m)
 INTEGER        :: CORCRIT    !## max. number of cells corrected during interpolation outer loop (-)
 REAL           :: INTXCRIT   !## stop criterion max. change interpolation (m)
 REAL           :: BUFFER     !## buffer aanbrengen
 
 INTEGER :: NAHN       !## number of ahn files
 INTEGER :: INTNODATA !## ignore nodata
 INTEGER :: IWINDOW    !## ignore specified window
 INTEGER :: IAGGREGATEY  !##     aggregarate y pointer to increase area to be flat
 REAL :: XMIN,YMIN,XMAX,YMAX
 
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: AHN
 CHARACTER(LEN=256) :: OUTFILE
 
END MODULE MOD_AHNFILTER_PAR

