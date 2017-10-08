!!  Copyright (C) Stichting Deltares, 2005-2017.
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
MODULE MOD_MDL_PAR

CHARACTER(LEN=256) :: RUNFILE,RESDIR,CLIPDIR

INTEGER,PARAMETER :: MXMDL=26 
INTEGER,PARAMETER :: MAXCSIZES=100

INTEGER :: NRUNFILES,NSCENFILES,NSDFFILES,NRESULTDIR,NSCENARIOS,&
!## settings in runfiles:
           NLAY,    &  !## number of layers
           MXITER,  &  !## maximum number of iterations
           NITER,   &  !## maximum number of inner iterations
           IIPF,    &  !## usage of ipf
           IDBG,    &  !## idebug configuration
           IEXPORT, &  !## export setting
           IPOSWEL, &  !## positioning of wells
           ISCEN,   &  !## usage of scenarios
           MXNLAY,  &  !## maximum number of modellayers
           NPER,    &  !## number of stressperiods
           ISS,     &  !## usage of storage coefficient
           NSCL,    &  !## griddimension
           IFTEST,  &  !## testing runfile
           ICONCHK, &  !## adjustment of drainage/overland to rivers
           NMULT,   &  !## number of simulation boxes
           IBUFFER, &  !## save result with/without buffer
           MAXICVNG,&  !## max. inner convergences
           IBUDGET, &  !## save individual budget-terms
           IDAMPING,&  !## use if damping
           NPCOND,  &  !## preconditioning
           IUNCONF, &  !## unconfined
           IFVDL,   &  !## formulea of de lange
           IARMSWP, &  !## artificial recharge metaswap
           IMERGE,  &  !## PKS-package: option idf-merge output files
           PARTOPT, &  !## PKS-package: methode of subdomain partition
           NMAXCORES, &!## amount of system cores available for parallel simulation
           NCORES      !## amount of cores selected for model simulation 
REAL :: MDLBUFFER,  &  !## buffersize
        SIMCSIZE,   &  !## cellsize
        MAXSIMCSIZE,&  !## max.cellsize
        HCLOSE,     &  !## closure criterion heads
        RCLOSE,     &  !## closure criterion budgets
        RELAX,      &  !## relaxation parameter
        MAXWBAL,    &  !## 0.01 %
        MINKD,      &  !## minimal kD
        MINC           !## minimal c-value
        !DELTCNVG
REAL,DIMENSION(4) :: SIMBOX  !## chosen size of model-simulation
REAL,DIMENSION(4) :: MODBOX  !## bigest size of model-simulation
INTEGER :: IDRAW             !## idraw=0 after idfplot() idraw=1 remove drawn lines
INTEGER,DIMENSION(:),ALLOCATABLE :: NLMDL, & !## number of modellayers for output
                                    IAMDL    !## activeness of module/package
INTEGER,DIMENSION(:,:),ALLOCATABLE :: ILMDL  !## modellayers for output
CHARACTER(LEN=10),PARAMETER :: REPLACESTRING='$DBASE$'
CHARACTER(LEN=256) :: SCENFNAME,BNDFNAME,MRGFNAME !## scenario filename
CHARACTER(LEN=256),DIMENSION(:),POINTER :: SDFFNAME,SDFFNAME_DUMMY !## sdf path+filename
CHARACTER(LEN=50),DIMENSION(:),ALLOCATABLE :: SDFFNAME_SHORT !## sdf filename

CHARACTER(LEN=25),DIMENSION(MXMDL) :: MDLALIAS !## Alias of acronym
CHARACTER(LEN=3) ,DIMENSION(MXMDL) :: MDLKEYWS !## keywords
INTEGER,DIMENSION(MXMDL) :: MDL_IPOS !## position
CHARACTER(LEN=25),DIMENSION(MXMDL) :: MDLALIAS_ACT !## active modules/packages
INTEGER :: NMP_ACT  !## number of modules/packages active

DATA MDLALIAS/'SIMGRO','BOUNDARY','GROUNDWATERHEAD','FLUX FRONT/RIGHT FACE','FLUX LOWER FACE','STORAGE', &
              'PURGED WATER TABLE','ANISOTROPY','HORIZ.FLOW BARRIER', &
              'TOP','BOT','CONCENTRATION','FLUX FRONT/RIGHT FACE (K)','FLUX LOWER FACE (K)', &
              'VERTICAL ANISOTROPY','WELLS','DRAINAGE','RIVERS','EVAPOTRANSPIRATION','GENERAL HEAD BOUNDARY',&
              'RECHARGE','OVERLAND FLOW','CONSTANT HEAD','SEGMENT RIVERS','INTERBED STORAGE','PARAMETER ESTIMATION'/
DATA MDLKEYWS/'CAP','BND','SHD','KDW','VCW','STO', &     !  1- 6
              'PWT','ANI','HFB','TOP','BOT','CON', &     !  7-12
              'KHV','KVV','KVA','WEL','DRN','RIV', &     ! 13-18
              'EVT','GHB','RCH','OLF','CHD','ISG', &     ! 19-24
              'IBS','PST'/                               ! 15-26

END MODULE MOD_MDL_PAR

