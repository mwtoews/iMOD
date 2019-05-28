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

MODULE MOD_AHNFILTER_PAR

 USE IMODVAR, ONLY : DP_KIND,SP_KIND
 USE MOD_IDF_PAR
 
 INTEGER :: NSCRIT     !## surface yes/no (cells!)
 REAL(KIND=DP_KIND) :: LOCCRIT    !## no local depression/upconing whenever max-min>loccrit (m)
 REAL(KIND=DP_KIND) :: XCRIT      !## building/strong edge (m)
 INTEGER :: DPW        !## size window upconing
 REAL(KIND=DP_KIND) :: DP1        !## depression percentile
 REAL(KIND=DP_KIND) :: DP2        !## upconing percentile
 REAL(KIND=DP_KIND) :: CORXCRIT   !## max. change to become surf.level (m)
 INTEGER :: CORCRIT    !## max. number of cells corrected during interpolation outer loop (-)
 REAL(KIND=DP_KIND) :: INTXCRIT   !## stop criterion max. change interpolation (m)
 REAL(KIND=DP_KIND) :: BUFFER     !## buffer aanbrengen
 
 INTEGER :: NAHN       !## number of ahn files
 INTEGER :: INTNODATA !## ignore nodata
 INTEGER :: AHN_IWINDOW    !## ignore specified window
 INTEGER :: IAGGREGATEY  !##     aggregarate y pointer to increase area to be flat
 REAL(KIND=DP_KIND) :: AHN_XMIN,AHN_YMIN,AHN_XMAX,AHN_YMAX
 
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: AHN
 CHARACTER(LEN=256) :: OUTFILE
 
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDFAHN
 TYPE(IDFOBJ) :: IDFX  !## original data
 TYPE(IDFOBJ) :: IDFY  !## result data
 INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:) :: Y   !## pointer to monitor previous position(s)
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: AHN_XP   !## percentile window (local upconing)
 INTEGER(KIND=1),POINTER,DIMENSION(:) :: ISPEC
 INTEGER(KIND=2),POINTER,DIMENSION(:,:) :: THREAD,ISCRIT !,YSEL

 REAL(KIND=DP_KIND) :: DX,DY
 REAL(KIND=DP_KIND),PARAMETER :: AHN_NODATA=-9999.99  
 INTEGER,PARAMETER :: IUPC    =-3 !upconing
 INTEGER,PARAMETER :: IDEP    =-2 !depression
 INTEGER,PARAMETER :: INODATA =-1 !nodata
 INTEGER,PARAMETER :: IINI    = 0 !initial
 INTEGER,PARAMETER :: IPOTSURF= 1 !potential surface
 INTEGER,PARAMETER :: ISURF   = 2 !surface

END MODULE MOD_AHNFILTER_PAR

