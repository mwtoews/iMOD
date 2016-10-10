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
MODULE IMODVAR

INTEGER,DIMENSION(2) :: IDPROC

REAL,PARAMETER :: PI=ATAN(1.0)*4.0  !## value pi

LOGICAL :: LBETA=.FALSE.       !## if TRUE: Show question whether it is allowed to use Beta-version or not
LOGICAL :: LEXPDATE=.TRUE.     !## if TRUE: activate expire date
INTEGER :: EXPDATE=20161231    !## expire data, after this date the iMOD-beta version cannot be used.
INTEGER,SAVE :: ICDEBUGLEVEL   !## applied current debuglevel

CHARACTER(LEN=30),PARAMETER :: RVERSION='V3_4'       !## release message - only with single subnummers
CHARACTER(LEN=30),PARAMETER :: BVERSION='Beta'       !## banner message !!!
CHARACTER(LEN=32) :: LICFILE='I_accepted_'//TRIM(RVERSION)//'.txt'
CHARACTER(LEN=256) :: IMFFNAME         !## name of drawing file
CHARACTER(LEN=256) :: EXENAME,EXEPATH
CHARACTER(LEN=256) :: SAVEDIR          !## remember opened location
REAL :: MASKXMIN,MASKXMAX,MASKYMIN,MASKYMAX
REAL :: DOWNX,DOWNY

INTEGER :: PLACES,DECPLACES,IFORM !## idfgetvalue variables to put on map-menu

INTEGER :: IDOWN
INTEGER :: IW,IH,IBITMAP,DX,DY,PX,PY
INTEGER :: IDIAGERROR
INTEGER :: IBACKSLASH  !##  label trimmen achter backslash
INTEGER :: ILABELNAME  !##  plot of labels optional

INTEGER,PARAMETER :: MXTP=36  !## no of TYPE
INTEGER,PARAMETER :: MXSYS=10
TYPE TPOBJ
 CHARACTER(LEN=15) :: ACRNM  !## acronym waterbalance budget
 CHARACTER(LEN=50) :: ALIAS  !## alias of acronym
 CHARACTER(LEN=5) :: UNIT   !## unit of the budget element
 INTEGER :: IACT
 INTEGER,DIMENSION(:),POINTER :: ISYS  !## system numbers  *_sys{i}.idf
 INTEGER :: NSYS !## number of systems in waterbalance
END TYPE TPOBJ
TYPE(TPOBJ),DIMENSION(MXTP)   :: TP
DATA TP%ACRNM/'HEAD','BDGBND','BDGFLF','BDGFRF','BDGFFF',&
              'BDGSTO','BDGWEL','BDGDRN','BDGRIV','BDGEVT',&
              'BDGGHB','BDGOLF','BDGRCH','BDGISG','BDGCAP',&
              'BDGDS','BDGPM','BDGPS','BDGEVA','BDGQRUN',&
              'PWTHEAD','GWL','BDGETACT','BDGPSGW','MSW_EBSPOT',&
              'MSW_EIC','MSW_EPD','MSW_ESP','MSW_TPOT','BDGQSPGW',&
              'MSW_EBS','MSW_QMODFBOT','MSW_QMR','BDGDECSTOT','BDGPSSW',&
              'MSW_TACT'/

DATA TP%UNIT/ '    m',' m3/d',' m3/d',' m3/d',' m3/d',&
              ' m3/d',' m3/d',' m3/d',' m3/d',' m3/d',&
              ' m3/d',' m3/d',' m3/d',' m3/d',' m3/d',&
              ' m3/d','  m/d',' m3/d',' m3/d','  m/d',&
              '    m','    m','m3/m2','m3/m2','m3/m2',&
              'm3/m2','m3/m2','m3/m2','m3/m2','m3/m2',&
              'm3/m2','m3/m2','m3/m2','m3/m2','m3/m2',&
              'm3/m2'/

DATA TP%ALIAS/'GROUNDWATERHEAD','CONSTANT_HEAD','FLUX_LOWER_FACE','FLUX_RIGHT_FACE','FLUX_FRONT_FACE',&
              'STORAGE','WELLS','DRAINAGE','RIVERS','EVAPOTRANSPIRATION',&
              'GENERAL_HEAD_BOUNDARY','OVERLAND_FLOW','RECHARGE','SEGMENTS','CAPSIM',&
              'DECREASE_WATER_ST_ROOTZONE','MEASURED_PRECIPITATION','SPRINKLING_PRECIPITATION','NET_EVAPORATION_WATER','RUNOFF',&
              'PURGE_WATER_TABLE_HEAD','GROUNDWATERLEVEL','TOTAL_ACTUAL_TRANSPIRATION','SPRINKLING_PRECIPITATION_FROM_GROUNDWATER','POTENTIAL_EVAPORATION_BARE_SOIL',&
              'EVAPORATION_INTERCEPTION_WATER','EVAPORATION_PONDING_WATER','EVAPORATION_SPRINKLING_WATER','POTENTIAL_TRANSPIRATION_VEGETATION','GROUNDWATER_EXTRACTION_FOR_SPRINKLING',&
              'EVAPORATION_BARE_SOIL','UPWARD_SEEPAGE_OF_MODFLOW_CELL','FLOW_THROUGH_BOTTOM_OF_BOX1_ROOT_ZONE','DECREASE_STORAGE','SPRINKLING_PRECIPITATION_FROM_SURFACEWATER',&
              'ACTUAL_TRANSPIRATION_VEGETATION'/

END MODULE IMODVAR

