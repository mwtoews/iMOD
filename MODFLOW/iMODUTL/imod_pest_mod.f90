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

!###====================================================================
MODULE PESTVAR
!###====================================================================
INTEGER,PARAMETER :: MXPTYPE=19
REAL,PARAMETER :: XPBND=0.01 !## boundary percentage
CHARACTER(LEN=2),DIMENSION(MXPTYPE) :: PTYPE
INTEGER :: PEST_NITER,PEST_ITER,PEST_ILNSRCH,PEST_IGRAD,PEST_NOBS,PEST_NPERIOD,PEST_NBATCH,PEST_ISCALING, &
   PEST_KTYPE,PEST_IREGULARISATION
REAL,DIMENSION(2) :: PEST_ITARGET
INTEGER(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: PEST_IPERIOD
LOGICAL :: LGRAD,LLNSRCH,LSENS
REAL :: PEST_JSTOP,PEST_SENSITIVITY,PEST_PADJ,PEST_DRES
REAL(KIND=8) :: TJOBJ,TJ,PJ    !## objective function value, plausibility value (pj)
TYPE BATCHOBJ
 CHARACTER(LEN=256) :: BATCHFILE,OUTFILE
 REAL :: FRACTION
END TYPE BATCHOBJ
TYPE(BATCHOBJ),ALLOCATABLE,DIMENSION(:) :: PEST_IBATCH
TYPE PARAOBJ
 INTEGER :: IACT            !## active (yes=1;no=0)
 CHARACTER(LEN=2) :: PTYPE  !## parameter type
 CHARACTER(LEN=256) :: EXBATFILE  !## external batch file
 REAL :: MIN,MAX,INI,FADJ   !## min,max,initial value of parameter
 DOUBLE PRECISION :: TJOBJ  !## objective function value
 REAL,DIMENSION(2) :: ALPHA !## alpha value
 REAL,POINTER,DIMENSION(:) :: ALPHA_HISTORY
 REAL,POINTER,DIMENSION(:) :: ALPHA_ERROR_VARIANCE
 REAL :: DELTA              !## delta alpha
 INTEGER :: ILS             !## ilayer/isystem
 INTEGER :: IZONE           !## zone number
 INTEGER :: ZTYPE           !## zone type (0) = grid, type (1) = ppoint
 INTEGER :: NODES           !## zone number
 INTEGER :: IBND            !## type of parameter limitation (-1=minbound,1=maxbound)
 INTEGER :: IGROUP          !## group to which parameters belongs
 LOGICAL :: LOG             !## logical to determine whether parameter is lognormal
 INTEGER(KIND=2),POINTER,DIMENSION(:) :: IROW,ICOL  !## node number of zone for parameter param(i) --- pointer to IZONE
 REAL,POINTER,DIMENSION(:,:) :: XY  !## xy location for pilot-points
 REAL,POINTER,DIMENSION(:) :: X  !## copy of variable to be adjusted
 REAL,POINTER,DIMENSION(:) :: F  !## fraction of variable to be adjusted
END TYPE PARAOBJ
TYPE(PARAOBJ),ALLOCATABLE,DIMENSION(:) :: PARAM
TYPE ZONEOBJ
 REAL,POINTER,DIMENSION(:,:) :: X,XY
 INTEGER,POINTER,DIMENSION(:) :: IZ
 INTEGER :: ZTYPE  !## ztype=0 idf, ztype=1 ipf (ppoint)
END TYPE ZONEOBJ
TYPE(ZONEOBJ),ALLOCATABLE,DIMENSION(:) :: ZONE
INTEGER :: IUPESTOUT,IUPESTRESIDUAL,IUPESTPROGRESS,IUPESTEFFICIENCY,IUPESTSENSITIVITY,IUPESTRUNFILE
TYPE MSROBJ
 CHARACTER(LEN=32),POINTER,DIMENSION(:) :: CLABEL,CLABEL_DUMMY
 INTEGER,POINTER,DIMENSION(:) :: L,L_DUMMY
 REAL,POINTER,DIMENSION(:) :: X,X_DUMMY
 REAL,POINTER,DIMENSION(:) :: Y,Y_DUMMY
 REAL,POINTER,DIMENSION(:) :: W,W_DUMMY
 REAL,POINTER,DIMENSION(:,:) :: DH,DH_DUMMY 
END TYPE MSROBJ
TYPE(MSROBJ) :: MSR
REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: JQJ
REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: JQR,U
REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: QPP
REAL(KIND=8) :: MARQUARDT
REAL,PARAMETER :: NUGGET=0.0, SILL=100.0

DATA PTYPE/'KD',& ! 1 transmissivity
           'KH',& ! 2 horizontal permeability
           'KV',& ! 3 vertical   permeability
           'VC',& ! 4 vertical   resistance
           'SC',& ! 5 storage    coefficient
           'RC',& ! 6 river      conductance
           'RI',& ! 7 river      infiltration
           'DC',& ! 8 drainage   conductance
           'IC',& ! 9 isg        conductance
           'II',& !10 isg        infiltration
           'AF',& !11 ani        factor
           'AA',& !12 ani        angle
           'VA',& !13 kva        vertical anisotropy
           'HF',& !14 hfb        horizontal barrier resistance
           'MS',& !15 metaswap theta (soil moisture)
           'MC',& !16 metaswap conductivity
           'RE',& !17 recharge
           'EX',& !18 external distribution
           'EP'/  !19 

END MODULE PESTVAR

