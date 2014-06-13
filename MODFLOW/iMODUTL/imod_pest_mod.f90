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
INTEGER,PARAMETER :: MXPTYPE=16
REAL,PARAMETER :: XPBND=0.01 !## boundary percentage
CHARACTER(LEN=2),DIMENSION(MXPTYPE) :: PTYPE
INTEGER :: PEST_NITER,PEST_ITER,PEST_ILNSRCH,PEST_IGRAD,PEST_NOBS,PEST_NPERIOD,PEST_NBATCH,PEST_ISCALING
REAL,DIMENSION(2) :: PEST_ITARGET
INTEGER,ALLOCATABLE,DIMENSION(:,:) :: PEST_IPERIOD
LOGICAL :: LGRAD,LLNSRCH,LSENS
REAL :: PEST_JSTOP,PEST_SENSITIVITY,PEST_PADJ
DOUBLE PRECISION :: TJOBJ,TJ    !## objective function value
TYPE BATCHOBJ
 CHARACTER(LEN=256) :: BATCHFILE,OUTFILE
 REAL :: FRACTION
END TYPE BATCHOBJ
TYPE(BATCHOBJ),ALLOCATABLE,DIMENSION(:) :: PEST_IBATCH
TYPE PARAOBJ
 INTEGER :: IACT            !## active (yes=1;no=0)
 CHARACTER(LEN=2) :: PTYPE  !## parameter type
 REAL :: MIN,MAX,INI,FADJ   !## min,max,initial value of parameter
 DOUBLE PRECISION :: TJOBJ  !## objective function value
 REAL,DIMENSION(2) :: ALPHA !## alpha value
 REAL,POINTER,DIMENSION(:) :: ALPHA_HISTORY
 REAL :: DELTA              !## delta alpha
 INTEGER :: ILS             !## ilayer/isystem
 INTEGER :: IZONE           !## zone number
 INTEGER :: NODES           !## zone number
 INTEGER :: IBND            !## type of parameter limitation (-1=minbound,1=maxbound)
 INTEGER :: IGROUP          !## group to which parameters belongs
 INTEGER(KIND=2),POINTER,DIMENSION(:) :: IROW,ICOL  !## node number of zone for parameter param(i) --- pointer to IZONE
 REAL,POINTER,DIMENSION(:) :: X  !## copy of variable to be adjusted
 REAL,POINTER,DIMENSION(:) :: F  !## fraction of variable to be adjusted
END TYPE PARAOBJ
TYPE(PARAOBJ),ALLOCATABLE,DIMENSION(:) :: PARAM
TYPE ZONEOBJ
 INTEGER(KIND=2),POINTER,DIMENSION(:) :: IROW,ICOL
END TYPE ZONEOBJ
TYPE(ZONEOBJ),ALLOCATABLE,DIMENSION(:) :: ZONE
REAL,ALLOCATABLE,DIMENSION(:,:,:) :: BUFPST
INTEGER :: IUPESTOUT,IUPESTRESIDUAL,IUPESTPROGRESS
REAL,POINTER,DIMENSION(:) :: W,W_DUMMY
REAL,POINTER,DIMENSION(:,:) :: DH,DH_DUMMY
REAL,ALLOCATABLE,DIMENSION(:,:) :: JQJ
REAL,ALLOCATABLE,DIMENSION(:) :: JQR,U
REAL :: MARQUARDT

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
           'MS',& !14 metaswap theta (soil moisture)
           'MC'/  !15 metaswap conductivity

END MODULE PESTVAR