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
USE MODPLOT, ONLY : LEGENDOBJ

INTEGER,DIMENSION(:),ALLOCATABLE :: IACTSTREAM,ISTR !## active yes/no (1/0) of stream in model domain

INTEGER,PARAMETER :: IRDFLG=-1
!If IRDFLG <= 0, input data for this stress period will be printed. If IRDFLG > 0, then input data for this stress period will not be printed.
!If the absolute value of IRDFLG = 2, The elevation of top of streambed of diversion segments (canals/laterals) follows the slope of ground surface at a depth defined by the interpolation of:
!(1) the difference between the ground surface elevation and the elevation of the upstream end of a diversion segment, as specified in the SFR input file; and
!(2) the difference between the ground surface elevation and the elevation of the downstream end of a diversion segment, as specified in the SFR input file.
!Note limitation: IRDFLG = 2 cannot be chosen if the number of the diversion segment is equal to the total number of segments.

INTEGER,PARAMETER :: IPTFLG=0
 !An integer value for printing streamflow-routing results during this stress period. If IPTFLG = 0, or whenever the variable
 !ICBCFL or 'Save Budget' is specified in Output Control, the results for specified time steps during this stress period will
 !be printed. If IPTFLG > 0, then the results during this stress period will not be printed.

INTEGER,PARAMETER :: MAXLEN  =32
INTEGER,PARAMETER :: MAXFILES=10 !## number of files for ISG-"family"
INTEGER,PARAMETER :: MAXITEMS=12
INTEGER,SAVE :: ICLRSC,ICLRSD,ICLRSP,ICLRND,ICLRST,ICLRQH,ICLRSG,ICLRSF,ICLRCO
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
 INTEGER :: ILIST !## new selected list
 INTEGER :: JLIST !## old selected list
END TYPE ISGOBJ
TYPE(ISGOBJ),ALLOCATABLE,DIMENSION(:) :: ISG,DUMISG

TYPE(LEGENDOBJ),ALLOCATABLE,DIMENSION(:) :: ISGLEG

!## general type for all
TYPE DATOBJ1
 INTEGER :: N,IREF
 REAL :: DIST
 CHARACTER(LEN=MAXLEN) :: CNAME
END TYPE DATOBJ1
TYPE(DATOBJ1),ALLOCATABLE,DIMENSION(:) :: ISD,ISC,IST,ISQ,DUMISD,DUMISC,DUMIST,DUMISQ

!## type for calculation points
TYPE ISDOBJ
 !## isfr=0
 INTEGER :: IDATE  !## date in yyyymmdd
 INTEGER :: ID_STW    !## influenced by structure #
 REAL :: RESIS        !## resistance
 REAL :: INFF         !## infiltration factor
 REAL :: WL_STW       !## influenced waterlevel
 !## isfr=0/1
 REAL :: WLVL         !## average waterlevel
 !## isfr=1
 CHARACTER(LEN=8) :: CTIME !## time in hhmmss
 REAL :: BTML      !## averagebottom level
 REAL :: WIDTH     !## average width
 REAL :: THCK      !## thickness of riverbed
 REAL :: HCND      !## permeability of riverbed
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
 REAL :: DISTANCE,BOTTOM,MRC,ZP
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
 REAL :: Q,W,D,F
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

CHARACTER(LEN=12),DIMENSION(5) :: TATTRIB1
CHARACTER(LEN=12),DIMENSION(13) :: TATTRIB2
INTEGER,DIMENSION(5) :: CTATTRIB1
INTEGER,DIMENSION(13) :: CTATTRIB2

!## items for isd2-file for 1) riv approach and 2) sfr approach
DATA TATTRIB1/'Date','Water level','Bottom level','Resistance','Inf.factor'/
DATA TATTRIB2/'Date','Time','Water level','Bottom level','Stream Width','Bed Thickn.','Bed Perm.','Iup Seg','Idown Seg','Calc Opt', &
              'Div Opt','Q Flow','Q Runoff'/
DATA CTATTRIB1/1,2,2,2,2/   !## 1=integer,2=real,3=menu
DATA CTATTRIB2/1,4,2,2,2,2,2,1,1,3,3,2,2/

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

TYPE GRIDISGOBJ
 REAL :: XMIN,YMIN,XMAX,YMAX          !## area to be gridded (x1,y1,x2,y2)'
 INTEGER :: ISTEADY                   !## (1) mean over all periods, (2) mean over given period'
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
 CHARACTER(LEN=30),POINTER,DIMENSION(:) :: CSOBEK
 INTEGER,POINTER,DIMENSION(:) :: SWNR
 INTEGER :: NSVATS,NSWNR,SYSID
END TYPE GRIDISGOBJ

END MODULE MOD_ISG_PAR

