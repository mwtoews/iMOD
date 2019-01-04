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

USE WINTERACTER

!## font
INTEGER,PARAMETER :: TFONT=FFHELVETICA

!## specify kind for double-precision real with 15 digits and a point ranging up to exponent 307
INTEGER,PARAMETER :: DP_KIND=SELECTED_REAL_KIND(15,307) 
!## specify kind for single-precision real with 6  digits and a point ranging up to exponent 37
INTEGER,PARAMETER :: SP_KIND=SELECTED_REAL_KIND(6 ,37 ) 

INTEGER,DIMENSION(2) :: IDPROC

REAL(KIND=DP_KIND),PARAMETER :: PI=ATAN(1.0D0)*4.0D0  !## value pi

LOGICAL :: LBETA=.TRUE.           !## if TRUE: Show question whether it is allowed to use Beta-version or not
LOGICAL :: LBETA_QUESTION=.FALSE. !## overrule the question at startup
LOGICAL :: LEXPDATE=.TRUE.        !## if TRUE: activate expire date
INTEGER :: EXPDATE=20190201       !## expire data, after this date the iMOD-beta version cannot be used.
INTEGER,SAVE :: ICDEBUGLEVEL      !## applied current debuglevel

INTEGER,SAVE :: GKEYPRESSED    !## keypressed (cntr/shift)
INTEGER,SAVE :: IMOD_IUNITS
CHARACTER(LEN=2),DIMENSION(2) :: IMOD_CUNITS
DATA IMOD_CUNITS/'m ','ft'/
CHARACTER(LEN=10),PARAMETER :: REPLACESTRING='$DBASE$'

CHARACTER(LEN=30),PARAMETER :: RVERSION    ='V4_4'      !## release message - used for license
CHARACTER(LEN=30),PARAMETER :: RVERSION_EXE='V4_4'      !## release message - only with single subnummers
CHARACTER(LEN=50),PARAMETER :: BVERSION='Beta Build Intel v2019.1.144 [18-12-2018 11:47]'       !## banner message !!!
CHARACTER(LEN=32) :: LICFILE='I_accepted_'//TRIM(RVERSION)//'.txt'
CHARACTER(LEN=256) :: IMFFNAME         !## name of drawing file
CHARACTER(LEN=256) :: EXENAME,EXEPATH
CHARACTER(LEN=256) :: SAVEDIR          !## remember opened location
REAL(KIND=DP_KIND) :: MASKXMIN,MASKXMAX,MASKYMIN,MASKYMAX
REAL(KIND=DP_KIND) :: DOWNX,DOWNY
REAL(KIND=DP_KIND) :: OFFSETX,OFFSETY

INTEGER :: PLACES,DECPLACES,IFORM !## idfgetvalue variables to put on map-menu

INTEGER :: IDOWN !## mouse button pressed
INTEGER :: IW,IH,IBITMAP,DX,DY,PX,PY
INTEGER :: IDIAGERROR
INTEGER :: IBACKSLASH  !##  label trimmen achter backslash
INTEGER :: ILABELNAME  !##  plot of labels optional

INTEGER,PARAMETER :: MXBG=24
INTEGER,PARAMETER :: MXTP=38
!INTEGER,PARAMETER :: MXSYS=10
INTEGER,DIMENSION(MXBG) :: ICPL
TYPE TPOBJ
 CHARACTER(LEN=15) :: ACRNM  !## acronym waterbalance budget
 CHARACTER(LEN=50) :: ALIAS  !## alias of acronym
 CHARACTER(LEN=5) :: UNIT   !## unit of the budget element
 INTEGER :: IACT
 INTEGER :: MODFLOWMETASWAP !## code whether part of modflow or part of metaswap
 INTEGER,DIMENSION(:),POINTER :: ISYS  !## system numbers  *_sys{i}.idf
 INTEGER :: NSYS !## number of systems in waterbalance
END TYPE TPOBJ
TYPE(TPOBJ),DIMENSION(MXTP)   :: TP

DATA TP%UNIT/ '    m',' m3/d',' m3/d',' m3/d',' m3/d',' m3/d',&
              ' m3/d',' m3/d',' m3/d',' m3/d',' m3/d',&
              ' m3/d',' m3/d',' m3/d',' m3/d',' m3/d',&
              ' m3/d','  m/d',' m3/d',' m3/d','  m/d',&
              '    m','    m','m3/m2','m3/m2','m3/m2',&
              'm3/m2','m3/m2','m3/m2','m3/m2','m3/m2',&
              'm3/m2','m3/m2','m3/m2','m3/m2','m3/m2',&
              'm3/m2','m3/m2'/

DATA TP%ALIAS/'GROUNDWATERHEAD','CONSTANT_HEAD','FLUX_LOWER_FACE','FLUX_UPPER_FACE','FLUX_RIGHT_FACE','FLUX_FRONT_FACE',&
              'STORAGE','WELLS','DRAINAGE','RIVERS','EVAPOTRANSPIRATION',&
              'GENERAL_HEAD_BOUNDARY','OVERLAND_FLOW','RECHARGE','SEGMENTS','CAPSIM',&
              'DECREASE_WATER_ST_ROOTZONE','MEASURED_PRECIPITATION','SPRINKLING_PRECIPITATION','NET_EVAPORATION_WATER','RUNOFF',&
              'PURGE_WATER_TABLE_HEAD','GROUNDWATERLEVEL','TOTAL_ACTUAL_TRANSPIRATION','SPRINKLING_PRECIPITATION_FROM_GROUNDWATER', &
              'POTENTIAL_EVAPORATION_BARE_SOIL',&
              'EVAPORATION_INTERCEPTION_WATER','EVAPORATION_PONDING_WATER','EVAPORATION_SPRINKLING_WATER','POTENTIAL_TRANSPIRATION_VEGETATION', &
              'GROUNDWATER_EXTRACTION_FOR_SPRINKLING',&
              'EVAPORATION_BARE_SOIL','UPWARD_SEEPAGE_OF_MODFLOW_CELL','FLOW_THROUGH_BOTTOM_OF_BOX1_ROOT_ZONE','DECREASE_STORAGE', &
              'SPRINKLING_PRECIPITATION_FROM_SURFACEWATER',&
              'ACTUAL_TRANSPIRATION_VEGETATION','CORRECTION TERM OF REALIGNMENT OF IMODFLOW'/

DATA TP%ACRNM/'HEAD'  , 'BDGBND'       ,'BDGFLF'  ,'BDGFTF'    ,'BDGFRF'    ,'BDGFFF',    &     !01-05
              'BDGSTO', 'BDGWEL'       ,'BDGDRN'  ,'BDGRIV'    ,'BDGEVT',    &     !06-10
              'BDGGHB', 'BDGOLF'       ,'BDGRCH'  ,'BDGISG'    ,'BDGCAP',    &     !11-15
              'BDGDS' , 'BDGPM'        ,'BDGPS'   ,'BDGEVA'    ,'BDGQRUN',   &     !16-20
              'PWTHEAD','GWL'          ,'BDGETACT','BDGPSGW'   ,'MSW_EBSPOT',&     !21-25
              'MSW_EIC' ,'MSW_EPD'     ,'MSW_ESP' ,'MSW_TPOT'  ,'BDGQSPGW',  &     !26-30
              'MSW_EBS' ,'MSW_QMODFBOT','MSW_QMR' ,'BDGDECSTOT','BDGPSSW',   &     !31-35
              'MSW_TACT','BDGQMODF'/                                               !36-37

DATA TP%MODFLOWMETASWAP/0,1,1,1,1,1, &
                        1,1,1,1,1, &
                        1,1,1,1,0, &
                        0,2,0,0,2, &
                        0,0,2,2,0, &
                        0,0,0,0,2, &
                        0,2,2,2,2, &
                        0,2/

END MODULE IMODVAR

