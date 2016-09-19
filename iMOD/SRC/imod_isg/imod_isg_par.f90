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
MODULE MOD_ISG_PAR

USE MOD_IDF_PAR, ONLY : IDFOBJ

INTEGER,PARAMETER :: MAXLEN  =32
INTEGER,PARAMETER :: MAXFILES=10 !## number of files for ISG-"family"
INTEGER,PARAMETER :: MAXITEMS=12
INTEGER,SAVE :: ICLRSC,ICLRSD,ICLRSP,ICLRND,ICLRST,ICLRQH,ICLRSG,ICLRSF
INTEGER,SAVE :: DIMISG,NISG,ISFR
INTEGER,SAVE :: DIMISP,NISP
INTEGER,SAVE :: DIMISC,DIMDATISC,NDISC,NISC,SELISC
INTEGER,SAVE :: DIMISD,DIMDATISD,NDISD,NISD,SELISD
INTEGER,SAVE :: DIMIST,DIMDATIST,NDIST,NIST,SELIST
INTEGER,SAVE :: DIMISQ,DIMDATISQ,NDISQ,NISQ,SELISQ
INTEGER,SAVE :: ISELISG,IISGPLOT
INTEGER,SAVE :: ISDMAXROW,ISCMAXROW,ISPMAXROW,ISTMAXROW,ISQMAXROW
INTEGER,SAVE :: ISGSHAPES
INTEGER,SAVE :: ICROSS_PNTR,ICROSS_ZVAL,ICROSS_HPNT,ICROSS_CPNT

INTEGER,ALLOCATABLE,DIMENSION(:) :: NISGF,NISFR,OFFSD,OFFSC,OFFST,OFFSQ,OFFISG,OFFISD,OFFISC,OFFIST,OFFISQ

INTEGER,DIMENSION(:,:),ALLOCATABLE :: ISGIU
CHARACTER(LEN=256),SAVE :: ISGFNAME

!## type for coordinates along the segment
TYPE ISPOBJ
 REAL :: X,Y
END TYPE ISPOBJ
TYPE(ISPOBJ),ALLOCATABLE,DIMENSION(:) :: ISP,ISP2,DUMISP
INTEGER :: TISP

!## main type for isg files
TYPE ISGOBJ
 CHARACTER(LEN=50) :: SNAME
 INTEGER :: ICLC,ICRS,ISEG,ISTW,IQHR
 INTEGER :: NCLC,NCRS,NSEG,NSTW,NQHR
 INTEGER :: ILIST
END TYPE ISGOBJ
TYPE(ISGOBJ),ALLOCATABLE,DIMENSION(:) :: ISG,DUMISG

!## general type for all
TYPE DATOBJ1
 INTEGER :: N,IREF
 REAL :: DIST
 CHARACTER(LEN=MAXLEN) :: CNAME
END TYPE DATOBJ1
TYPE(DATOBJ1),ALLOCATABLE,DIMENSION(:) :: ISD,ISC,IST,ISQ,DUMISD,DUMISC,DUMIST,DUMISQ

!## type for calculation points
TYPE ISDOBJ
 INTEGER :: IDATE  !## date in yyyymmdd
 INTEGER :: ID_STW    !## influenced by structure #
 REAL :: WLVL         !## waterlevel
 REAL :: RESIS        !## resistance
 REAL :: INFF         !## infiltration factor
 REAL :: WL_STW       !## influenced waterlevel

 INTEGER :: ITIME  !## time in hhmmss
 REAL :: BTML      !## bottom level
 REAL :: THCK      !## thickness of riverbed
 REAL :: HCND      !## permeability of riverbed
 REAL :: WIDTH     !## average width of stream channel
 REAL :: DEPTH     !## average depth of stream channel
 INTEGER :: DWNS   !## downstream segment number
 INTEGER :: UPSG   !## upstream segment number
 INTEGER :: ICLC   !## calculation option
 INTEGER :: IPRI   !## diversion option
 REAL :: QFLW      !## streamflow entering segment
 REAL :: QROF      !## runoff 

END TYPE ISDOBJ
TYPE(ISDOBJ),ALLOCATABLE,DIMENSION(:) :: DATISD,DUMDATISD,ISGEDITISD
TYPE(ISDOBJ),ALLOCATABLE,DIMENSION(:,:) :: DATISD2
INTEGER,DIMENSION(:),ALLOCATABLE :: TISD

!## type for cross-sections
TYPE ISCOBJ
 REAL :: DISTANCE,BOTTOM,KM,ZP
END TYPE ISCOBJ
TYPE(ISCOBJ),ALLOCATABLE,DIMENSION(:) :: DATISC,DUMDATISC,ISGEDITISC
TYPE(ISCOBJ),ALLOCATABLE,DIMENSION(:,:) :: DATISC2
INTEGER,DIMENSION(:),ALLOCATABLE :: TISC,ISCN

!## type for weirs
TYPE ISTOBJ
 INTEGER :: IDATE
 REAL :: WLVL_UP,WLVL_DOWN
END TYPE ISTOBJ
TYPE(ISTOBJ),ALLOCATABLE,DIMENSION(:) :: DATIST,DUMDATIST,ISGEDITIST
TYPE(ISTOBJ),ALLOCATABLE,DIMENSION(:,:) :: DATIST2
INTEGER,DIMENSION(:),ALLOCATABLE :: TIST

!## type for discharge-water level relationships
TYPE ISQOBJ
 REAL :: QZ,HZ,QW,HW
END TYPE ISQOBJ
TYPE(ISQOBJ),ALLOCATABLE,DIMENSION(:) :: DATISQ,DUMDATISQ,ISGEDITISQ
TYPE(ISQOBJ),ALLOCATABLE,DIMENSION(:,:) :: DATISQ2
INTEGER,DIMENSION(:),ALLOCATABLE :: TISQ

CHARACTER(LEN=24),DIMENSION(MAXITEMS) :: FNAME
REAL,DIMENSION(2,MAXITEMS) :: ISGVALUE

CHARACTER(LEN=4),DIMENSION(MAXFILES) :: EXT
CHARACTER(LEN=50),DIMENSION(MAXFILES) :: TFORM
INTEGER,DIMENSION(MAXFILES) :: RECLEN

DATA FNAME/'COND','STAGE','BOTTOM','INFFCT','TOTAL_LENGTH','MEAN_WPERIMETER','MEAN_WWIDTH','RESISTANCE','EROSION','EFFECT','CUR_ID','NEX_ID'/
DATA EXT/'ISG ','ISP ','ISD1','ISD2','ISC1','ISC2','IST1','IST2','ISQ1','ISQ2'/
DATA RECLEN/0,8,44,20,44,12,44,12,44,16/
DATA TFORM/'FORMATTED  ','UNFORMATTED','UNFORMATTED','UNFORMATTED', &
           'UNFORMATTED','UNFORMATTED','UNFORMATTED','UNFORMATTED'  , &
           'UNFORMATTED','UNFORMATTED'/

CHARACTER(LEN=10),DIMENSION(5) :: TATTRIB1
CHARACTER(LEN=10),DIMENSION(14) :: TATTRIB2
INTEGER,DIMENSION(5) :: CTATTRIB1
INTEGER,DIMENSION(14) :: CTATTRIB2

!## items for isd2-file for 1) riv approach and 2) sfr approach
DATA TATTRIB1/'Date','Waterlevel','Bottomlevel','Resistance','Infilt.fct'/
DATA TATTRIB2/'Date','Time','Waterlevel','Bottomlvl','Thickness','Conduct.','AvgWidth','Avg.Depth','UpSeg','DownSeg','CalcOpt', &
              'DivOpt','QInFlow','QRunoff'/
DATA CTATTRIB1/1,2,2,2,2/   !## 1=integer,2=real,3=menu
DATA CTATTRIB2/1,1,2,2,2,2,2,2,1,1,3,3,2,2/

TYPE ISGTYPE
 INTEGER :: ISOURCE,IACT
 TYPE(IDFOBJ),DIMENSION(4) :: IDF                       !idf-structure
 INTEGER,DIMENSION(4) :: IATTRIB,IOP 
 REAL,DIMENSION(4) :: VALUE 
 CHARACTER(LEN=256),DIMENSION(4) :: IDFNAME
 CHARACTER(LEN=256) :: FNAME
 CHARACTER(LEN=50) :: WC
END TYPE ISGTYPE
TYPE(ISGTYPE),ALLOCATABLE,DIMENSION(:) :: ISGADJ
INTEGER,ALLOCATABLE,DIMENSION(:) :: ISEGMENTS,IDATES
CHARACTER(LEN=8),ALLOCATABLE,DIMENSION(:) :: CDATES
REAL,SAVE :: ISGX,ISGY
REAL,ALLOCATABLE,DIMENSION(:) :: XPOL,YPOL   
INTEGER,SAVE :: NXY,IULOG
INTEGER,DIMENSION(4) :: NRECORDS

REAL :: XMIN,YMIN,XMAX,YMAX          !## area to be gridded (x1,y1,x2,y2)'
INTEGER :: ISS                       !## (1) mean over all periods, (2) mean over given period'
INTEGER :: SDATE,EDATE,DDATE         !## startdate,enddate,ddate (yyyymmdd,yyyymmdd,dd)'
INTEGER(KIND=8) :: STIME,ETIME,DTIME !## starttime,endtime (yyyymmddmmhhss,yyyymmddmmhhss)'
INTEGER :: IDIM                      !## (0) give area (2) entire domain of isg (3) selected isg'
REAL :: CS                           !## cellsize'
REAL :: MINDEPTH                     !## minimal waterdepth for computing conductances (m)'
REAL :: WDEPTH                       !## waterdepth only used in combination with isimgro>0'
INTEGER :: ICDIST                    !## (0) do not compute effect of weirs (1) do compute effect of weirs'
INTEGER :: ISIMGRO                   !## ISIMGRO'
INTEGER :: IEXPORT                   !## (0) idf (1) modflow river file
CHARACTER(LEN=256) :: ROOT           !## resultmap'
CHARACTER(LEN=52) :: POSTFIX         !## POSTFIX {POSTFIX}_stage.idf etc.'
REAL :: NODATA                       !## nodatavalue in ISG
INTEGER,DIMENSION(12) :: ISAVE       !## array to specify the attributes to be saved
REAL :: MAXWIDTH                     !## 3 maximum widht for computing rivier-width (in case cross-sections are rubbish)
INTEGER :: IAVERAGE                  !## (1) mean (2) median value

!## applicable for svat export to swnr_svat_drng.inp
CHARACTER(LEN=256) :: THIESSENFNAME,AHNFNAME,SEGMENTCSVFNAME,SVAT2SWNR_DRNG
CHARACTER(LEN=30),ALLOCATABLE,DIMENSION(:) :: CSOBEK
INTEGER,ALLOCATABLE,DIMENSION(:) :: SWNR
INTEGER :: NSVATS,NSWNR,SYSID

END MODULE MOD_ISG_PAR

