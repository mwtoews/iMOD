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
!!
MODULE IMODVAR

INTEGER,DIMENSION(2) :: IDPROC

REAL,PARAMETER :: PI=ATAN(1.0)*4.0  !##value pi

CHARACTER(LEN=20),PARAMETER            :: RVERSION='3.0.2'
CHARACTER(LEN=256)                     :: IMFFNAME         !##name of drawing file
CHARACTER(LEN=256)                     :: OPENDIR   !remember saved location
CHARACTER(LEN=256)                     :: SAVEDIR   !remember opened location
REAL                                   :: MASKXMIN,MASKXMAX,MASKYMIN,MASKYMAX
REAL :: DOWNX,DOWNY
INTEGER :: IDOWN
INTEGER :: IW,IH,IBITMAP,DX,DY,PX,PY
INTEGER :: IDIAGERROR
INTEGER :: IBACKSLASH  !##  label trimmen achter backslash
INTEGER :: ILABELNAME  !##  plot of labels optional

INTEGER,PARAMETER :: MXTP=34  !## no of TYPE
INTEGER,PARAMETER :: MXSYS=10
TYPE TPOBJ
 CHARACTER(LEN=15) :: ACRNM  !## acronym waterbalance budget
 CHARACTER(LEN=50) :: ALIAS  !## alias of acronym
 INTEGER :: IACT
 INTEGER,DIMENSION(:),POINTER :: ISYS  !## system numbers  *_sys{i}.idf
 INTEGER :: NSYS !## number of systems in waterbalance
END TYPE TPOBJ
TYPE(TPOBJ),DIMENSION(MXTP)   :: TP
DATA TP%ACRNM/'HEAD  ','BDGBND'       ,'BDGFLF'  ,'BDGFRF' ,'BDGFFF'    ,'BDGSTO' ,'BDGWEL' ,'BDGDRN' ,'BDGRIV'  ,'BDGEVT'  , &
              'BDGGHB','BDGOLF'       ,'BDGRCH'  ,'BDGISG' ,'BDGCAP'    ,'BDGDS ' ,'BDGPM ' ,'BDGPS ' ,'BDGEVA'  ,'BDGQRUN' , &
              'PWTHEAD','GWL'         ,'BDGETACT','BDGPSGW','MSW_EBSPOT','MSW_EIC','MSW_EPD','MSW_ESP','MSW_TPOT','BDGQSPGW', &
              'MSW_EBS','MSW_QMODFBOT','MSW_QMR' ,'BDGDECSTOT'/
DATA TP%ALIAS/'GROUNDWATERHEAD','CONSTANT HEAD','FLUX LOWER FACE','FLUX RIGHT FACE','FLUX FRONT FACE','STORAGE',   &
              'WELLS','DRAINAGE','RIVERS','EVAPOTRANSPIRATION','GENERAL HEAD BOUNDARY','OVERLAND FLOW','RECHARGE', &
              'SEGMENTS','CAPSIM','DECREASE WATER ST.ROOTZ.','MEASURED PRECIPITATION','SPRINKLING PRECIPITATION', &
              'NET EVAPORATION WATER','RUNOFF','PURGE WATER TABLE HEAD','GROUNDWATERLEVEL',&
              'TOTAL ACTUAL TRANSPIRATION',&
              'SPRINKLING PRECIPITATION, FROM GROUNDWATER',&
              'POTENTIAL EVAPORATION BARE SOIL',&
              'EVAPORATION INTERCEPTION WATER', &
              'EVAPORATION PONDING WATER',&
              'EVAPORATION SPRINKLING WATER',&
              'POTENTIAL TRANSPIRATION VEGETATION',&
              'GROUNDWATER EXTRACTION FOR SPRINKLING',&
              'EVAPORATION BARE SOIL',&
              'UPWARD SEEPAGE OF MODFLOW CELL',&
              'FLOW THROUGH BOTTOM OF BOX1, ROOT ZONE', &
              'DECREASE STORAGE'/

END MODULE IMODVAR

