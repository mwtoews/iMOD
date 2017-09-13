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
MODULE IMODVAR

INTEGER,DIMENSION(2) :: IDPROC

REAL,PARAMETER :: PI=ATAN(1.0)*4.0  !## value pi

LOGICAL :: LBETA=.FALSE.       !## if TRUE: Show question whether it is allowed to use Beta-version or not
LOGICAL :: LEXPDATE=.TRUE.     !## if TRUE: activate expire date
INTEGER :: EXPDATE=20171015    !## expire data, after this date the iMOD-beta version cannot be used.
INTEGER,SAVE :: ICDEBUGLEVEL   !## applied current debuglevel

INTEGER,SAVE :: GKEYPRESSED    !## keypressed (cntr/shift)
INTEGER,SAVE :: IMOD_IUNITS
CHARACTER(LEN=2),DIMENSION(2) :: IMOD_CUNITS
DATA IMOD_CUNITS/'m ','ft'/

CHARACTER(LEN=30),PARAMETER :: RVERSION='V4_1'       !## release message - only with single subnummers
CHARACTER(LEN=30),PARAMETER :: BVERSION='Beta'       !## banner message !!!
CHARACTER(LEN=32) :: LICFILE='I_accepted_'//TRIM(RVERSION)//'.txt'
CHARACTER(LEN=256) :: IMFFNAME         !## name of drawing file
CHARACTER(LEN=256) :: EXENAME,EXEPATH
CHARACTER(LEN=256) :: SAVEDIR          !## remember opened location
REAL :: MASKXMIN,MASKXMAX,MASKYMIN,MASKYMAX
REAL :: DOWNX,DOWNY

INTEGER :: PLACES,DECPLACES,IFORM !## idfgetvalue variables to put on map-menu

INTEGER :: IDOWN !## mouse button pressed
INTEGER :: IW,IH,IBITMAP,DX,DY,PX,PY
INTEGER :: IDIAGERROR
INTEGER :: IBACKSLASH  !##  label trimmen achter backslash
INTEGER :: ILABELNAME  !##  plot of labels optional

INTEGER,PARAMETER :: MXBG=24
INTEGER,PARAMETER :: MXTP=38
INTEGER,PARAMETER :: MXSYS=10
INTEGER,DIMENSION(MXBG) :: ICPL
TYPE TPOBJ
 CHARACTER(LEN=15) :: ACRNM  !## acronym waterbalance budget
 CHARACTER(LEN=50) :: ALIAS  !## alias of acronym
 CHARACTER(LEN=5) :: UNIT   !## unit of the budget element
 INTEGER :: IACT
! INTEGER :: ICPL
 INTEGER :: MODFLOWMETASWAP !## code whether part of modflow or part of metaswap
 INTEGER,DIMENSION(:),POINTER :: ISYS  !## system numbers  *_sys{i}.idf
 INTEGER :: NSYS !## number of systems in waterbalance
END TYPE TPOBJ
TYPE(TPOBJ),DIMENSION(MXTP)   :: TP
!DATA TP%ACRNM/'HEAD','BDGBND','BDGFLF','BDGFRF','BDGFFF',&
!              'BDGSTO','BDGWEL','BDGDRN','BDGRIV','BDGEVT',&
!              'BDGGHB','BDGOLF','BDGRCH','BDGISG','BDGCAP',&
!              'BDGDS','BDGPM','BDGPS','BDGEVA','BDGQRUN',&
!              'PWTHEAD','GWL','BDGETACT','BDGPSGW','MSW_EBSPOT',&
!              'MSW_EIC','MSW_EPD','MSW_ESP','MSW_TPOT','BDGQSPGW',&
!              'MSW_EBS','MSW_QMODFBOT','MSW_QMR','BDGDECSTOT','BDGPSSW',&
!              'MSW_TACT','BDGQMODF'/

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

!ETACT x BDGETACT
!PM x BDGPM
!PMGW x BDGPSGW
!PMSW x BDGPSSW
!DECSTO x BDGDECSTOT
!QSPGW x BDGQSPGW
!QCOR x BDGQMODF
!QDR x MSW_QMR
!QRUN x BDGQRUN
!QMODF x BDGQMODF

! !## couple table for graph function in waterbalance analyse
! DATA ICPL   /08, & !## 01 drn
!              12, & !## 02 olf
!              09, & !## 03 riv
!              11, & !## 04 ghb
!              14, & !## 05 isg
!              07, & !## 06 wel
!              00, & !## 07 reg ---?
!              02, & !## 08 chh
!              03, & !## 09 flf 
!              00, & !## 10 fuf ---?
!              13, & !## 11 rch
!              10, & !## 12 evt
!              15, & !## 13 cap
!              13, & !## 14 etact
!              00, & !## 15 pm ---?
!              24, & !## 16 pmgw (bdgpsgw)
!              35, & !## 17 pmsw (bdgpssw)
!              06, & !## 18 sto
!              34, & !## 19 decsto
!              30, & !## 20 bdgqspgw
!              00, & !## 21 qcor 
!              00, & !## 22 qdr (bdgqdr)
!              00, & !## 23 qrun 
!              00/ !## 24 qmodf

!bdgdecstot
!bdgetact
!bdgpm
!bdgpsgw
!bdgpssw
!bdgqrun
!bdgqdr
!bdgqspgw
!bdgqmodf
!msw_qsimcorrmf
              
!!     Q(01,1)=QDRN_IN   Q(01,1)=QDRN_OUT    Q(13,1)=QCAP_IN   Q(13,1)=QCAP_OUT    
!!     Q(02,1)=QOLF_IN   Q(02,1)=QOLF_OUT    Q(14,1)=QETACT_IN Q(14,1)=QETACT_OUT  
!!     Q(03,1)=QRIV_IN   Q(03,1)=QRIV_OUT    Q(15,1)=QPM_IN    Q(15,1)=QPM_OUT     
!!     Q(04,1)=QGHB_IN   Q(04,1)=QGHB_OUT    Q(16,1)=QPMGW_IN  Q(16,1)=QPMGW_OUT   
!!     Q(05,1)=QISG_IN   Q(05,1)=QISG_OUT    Q(17,1)=QPMSW_IN  Q(17,1)=QPMSW_OUT   
!!     Q(06,1)=QWEL_IN   Q(06,1)=QWEL_OUT    Q(18,1)=QSTO_IN   Q(18,1)=QSTO_OUT    
!!     Q(07,1)=QREG_IN   Q(07,1)=QREG_OUT    Q(19,1)=QDECSTO_INQ(19,1)=QDECSTO_OUT 
!!     Q(08,1)=QCNH_IN   Q(08,1)=QCNH_OUT    Q(20,1)=QQSPGW_IN Q(20,1)=QQSPGW_OUT  
!!     Q(09,1)=QFLF1_IN  Q(09,1)=QFLF1_OUT   Q(21,1)=QQCOR_IN  Q(21,1)=QQCOR_OUT   
!!     Q(10,1)=QFLF2_IN  Q(10,1)=QFLF2_OUT   Q(22,1)=QQDR_IN   Q(22,1)=QQDR_OUT    
!!     Q(11,1)=QRCH_IN   Q(11,1)=QRCH_OUT    Q(23,1)=QQRUN_IN  Q(23,1)=QQRUN_OUT   
!!     Q(12,1)=QEVT_IN   Q(12,1)=QEVT_OUT    Q(24,1)=QMODF_IN  Q(24,1)=QMODF_OUT   


END MODULE IMODVAR

