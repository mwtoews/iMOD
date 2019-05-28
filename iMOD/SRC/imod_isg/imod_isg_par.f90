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
!!
MODULE MOD_ISG_PAR

USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MODPLOT, ONLY : LEGENDOBJ
USE IMODVAR, ONLY : DP_KIND,SP_KIND

INTEGER :: ICF=1 
REAL(KIND=SP_KIND),DIMENSION(9) :: X_SP
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

INTEGER,SAVE :: ISGDOUBLE

INTEGER,PARAMETER :: MAXLENISG  =32
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
 REAL(KIND=DP_KIND) :: X,Y
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
CHARACTER(LEN=MAXLENISG),DIMENSION(:),ALLOCATABLE :: ISDLABELS

TYPE(LEGENDOBJ),ALLOCATABLE,DIMENSION(:) :: ISGLEG

!## general type for all
TYPE DATOBJ1
 INTEGER :: N,IREF
 REAL(KIND=DP_KIND) :: DIST
 CHARACTER(LEN=MAXLENISG) :: CNAME
END TYPE DATOBJ1
TYPE(DATOBJ1),ALLOCATABLE,DIMENSION(:) :: ISD,ISC,IST,ISQ,DUMISD,DUMISC,DUMIST,DUMISQ

!## type for calculation points
TYPE ISDOBJSEG
INTEGER :: ISEG !## segment number
INTEGER :: ICLC !## calc number
END TYPE ISDOBJSEG
TYPE(ISDOBJSEG),DIMENSION(:),ALLOCATABLE :: ISGEDITISD_SEG

!## type for calculation points
TYPE ISDOBJ
 !## isfr=0
 INTEGER :: IDATE  !## date in yyyymmdd
 INTEGER :: ID_STW    !## influenced by structure #
 REAL(KIND=DP_KIND) :: RESIS        !## resistance
 REAL(KIND=DP_KIND) :: INFF         !## infiltration factor
 REAL(KIND=DP_KIND) :: WL_STW       !## influenced waterlevel
 !## isfr=0/1
 REAL(KIND=DP_KIND) :: WLVL         !## average waterlevel
 REAL(KIND=DP_KIND) :: BTML      !## averagebottom level
 !## isfr=1
 CHARACTER(LEN=8) :: CTIME !## time in hhmmss
 REAL(KIND=DP_KIND) :: WIDTH     !## average width
 REAL(KIND=DP_KIND) :: THCK      !## thickness of riverbed
 REAL(KIND=DP_KIND) :: HCND      !## permeability of riverbed
 INTEGER :: DWNS   !## downstream segment number
 INTEGER :: UPSG   !## upstream segment number
 INTEGER :: ICLC   !## calculation option
 INTEGER :: IPRI   !## diversion option
 REAL(KIND=DP_KIND) :: QFLW      !## streamflow entering segment
 REAL(KIND=DP_KIND) :: QROF      !## runoff 
 REAL(KIND=DP_KIND) :: PPTSW     !## precipitation
 REAL(KIND=DP_KIND) :: ETSW      !## evaporation
END TYPE ISDOBJ
TYPE(ISDOBJ),ALLOCATABLE,DIMENSION(:) :: DATISD,DUMDATISD,ISGEDITISD
TYPE(ISDOBJ),ALLOCATABLE,DIMENSION(:,:) :: DATISD2
INTEGER,DIMENSION(:),ALLOCATABLE :: TISD

!## type for cross-sections
TYPE ISCOBJ
 REAL(KIND=DP_KIND) :: DISTANCE,BOTTOM,MRC,ZP
END TYPE ISCOBJ
TYPE(ISCOBJ),ALLOCATABLE,DIMENSION(:) :: DATISC,DUMDATISC,ISGEDITISC
TYPE(ISCOBJ),ALLOCATABLE,DIMENSION(:,:) :: DATISC2
INTEGER,DIMENSION(:),ALLOCATABLE :: TISC,ISCN

!## type for weirs
TYPE ISTOBJ
 INTEGER :: IDATE
 REAL(KIND=DP_KIND) :: WLVL_UP,WLVL_DOWN
 CHARACTER(LEN=8) :: CTIME !## time in hhmmss
END TYPE ISTOBJ
TYPE(ISTOBJ),ALLOCATABLE,DIMENSION(:) :: DATIST,DUMDATIST,ISGEDITIST
TYPE(ISTOBJ),ALLOCATABLE,DIMENSION(:,:) :: DATIST2
INTEGER,DIMENSION(:),ALLOCATABLE :: TIST

!## type for discharge-water level relationships
TYPE ISQOBJ
 REAL(KIND=DP_KIND) :: Q,W,D,F
END TYPE ISQOBJ
TYPE(ISQOBJ),ALLOCATABLE,DIMENSION(:) :: DATISQ,DUMDATISQ,ISGEDITISQ
TYPE(ISQOBJ),ALLOCATABLE,DIMENSION(:,:) :: DATISQ2
INTEGER,DIMENSION(:),ALLOCATABLE :: TISQ

CHARACTER(LEN=24),DIMENSION(MAXITEMS) :: FNAME
REAL(KIND=DP_KIND),DIMENSION(2,MAXITEMS) :: ISGVALUE

CHARACTER(LEN=4),DIMENSION(MAXFILES) :: EXT
CHARACTER(LEN=50),DIMENSION(MAXFILES) :: TFORM
INTEGER,DIMENSION(MAXFILES) :: RECLEN,RECLND

DATA FNAME/'COND','STAGE','BOTTOM','INFFCT','TOTAL_LENGTH','MEAN_WPERIMETER','MEAN_WWIDTH','RESISTANCE','EROSION','EFFECT','CUR_ID','NEX_ID'/
DATA EXT/'ISG ','ISP ','ISD1','ISD2','ISC1','ISC2','IST1','IST2','ISQ1','ISQ2'/
!## single precision ISG
DATA RECLEN/0, 8,44,20,44,12,44,12,44,16/
!## double precision ISG
DATA RECLND/0,16,48,44,48,24,48,28,48,32/

DATA TFORM/'FORMATTED  ','UNFORMATTED','UNFORMATTED','UNFORMATTED', &
           'UNFORMATTED','UNFORMATTED','UNFORMATTED','UNFORMATTED'  , &
           'UNFORMATTED','UNFORMATTED'/

CHARACTER(LEN=24),DIMENSION(5) :: TATTRIB1
CHARACTER(LEN=24),DIMENSION(6) :: TATTRIB3
CHARACTER(LEN=24),DIMENSION(15) :: TATTRIB2
INTEGER,DIMENSION(5) :: CTATTRIB1
INTEGER,DIMENSION(15) :: CTATTRIB2
INTEGER,DIMENSION(6) :: CTATTRIB3

!## items for isd2-file for 1) riv approach and 2) sfr approach
DATA TATTRIB1/'Date','Water level (m+MSL)','Bottom level (m+MSL)','Resistance (d)','Inf.factor (-)'/
DATA TATTRIB2/'Date','Time','Water level (m+MSL)','Bottom level (m+MSL)','Stream Width (m)','Bed Thickn. (m)','Bed Perm. (m/d)', &
    'Iup Seg (-)','Idown Seg (-)','Calc Opt (-)','Div Opt (-)','Q Flow (m3/s)','Q Runoff (m3/s)','PPTSW (mm/d)','ETSW (mm/d)'/
DATA TATTRIB3/'Date','Time','Water level (m+MSL)','Bottom level (m+MSL)','Resistance (d)','Inf.factor (-)'/
DATA CTATTRIB1/1,2,2,2,2/     !## 1=integer,2=real,3=menu,4=character
DATA CTATTRIB2/1,4,2,2,2,2,2,1,1,3,3,2,2,2,2/
DATA CTATTRIB3/1,4,2,2,2,2/   !## 1=integer,2=real,3=menu,4=character

TYPE ISGTYPE
 INTEGER :: ISOURCE,IACT
 TYPE(IDFOBJ),DIMENSION(4) :: IDF                       !idf-structure
 INTEGER,DIMENSION(4) :: IATTRIB,IOP,IACTATTRIB
 REAL(KIND=DP_KIND),DIMENSION(4) :: VALUE 
 CHARACTER(LEN=256),DIMENSION(4) :: IDFNAME
 CHARACTER(LEN=256) :: FNAME
 CHARACTER(LEN=50) :: WC
 CHARACTER(LEN=52),DIMENSION(4) :: ATTRIB
END TYPE ISGTYPE
TYPE(ISGTYPE),ALLOCATABLE,DIMENSION(:) :: ISGADJ
INTEGER,ALLOCATABLE,DIMENSION(:) :: ISEGMENTS,IDATES
CHARACTER(LEN=8),ALLOCATABLE,DIMENSION(:) :: CDATES
REAL(KIND=DP_KIND),SAVE :: ISGX,ISGY
!REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: XPOL,YPOL   
INTEGER,SAVE :: NXY,IULOG
INTEGER,DIMENSION(4) :: NRECORDS

TYPE GRIDISGOBJ
 REAL(KIND=DP_KIND) :: XMIN,YMIN,XMAX,YMAX          !## area to be gridded (x1,y1,x2,y2)'
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: DELR=>NULL(),DELC=>NULL()  !## cellsizes
 INTEGER :: NCOL,NROW                 !## number of columns/row for griddin non-equidistantial
 INTEGER :: ISTEADY                   !## (1) mean over all periods, (2) mean over given period'
 INTEGER :: SDATE,EDATE,DDATE         !## startdate,enddate,ddate (yyyymmdd,yyyymmdd,dd)'
 INTEGER(KIND=8) :: STIME,ETIME,DTIME !## starttime,endtime (yyyymmddmmhhss,yyyymmddmmhhss)'
 INTEGER :: IDIM                      !## (0) give area (2) entire domain of isg (3) selected isg'
 REAL(KIND=DP_KIND) :: CS                           !## cellsize'
 REAL(KIND=DP_KIND) :: MINDEPTH                     !## minimal waterdepth for computing conductances (m)'
 REAL(KIND=DP_KIND) :: WDEPTH                       !## waterdepth only used in combination with isimgro>0'
 INTEGER :: ICDIST                    !## (0) do not compute effect of weirs (1) do compute effect of weirs'
 INTEGER :: ISIMGRO                   !## ISIMGRO'
 INTEGER :: IEXPORT                   !## (0) idf (1) modflow river file
 CHARACTER(LEN=256) :: ROOT           !## resultmap'
 CHARACTER(LEN=52) :: POSTFIX         !## POSTFIX {POSTFIX}_stage.idf etc.'
 REAL(KIND=DP_KIND) :: NODATA                       !## nodatavalue in ISG
 INTEGER,DIMENSION(12) :: ISAVE       !## array to specify the attributes to be saved
 REAL(KIND=DP_KIND) :: MAXWIDTH                     !## 3 maximum widht for computing rivier-width (in case cross-sections are rubbish)
 INTEGER :: IAVERAGE                  !## (1) mean (2) median value
 !## applicable for svat export to swnr_svat_drng.inp
 CHARACTER(LEN=256) :: THIESSENFNAME,AHNFNAME,SEGMENTCSVFNAME,SVAT2SWNR_DRNG
 CHARACTER(LEN=30),POINTER,DIMENSION(:) :: CSOBEK
 INTEGER,POINTER,DIMENSION(:) :: SWNR
 INTEGER :: NSVATS,NSWNR,SYSID
END TYPE GRIDISGOBJ

END MODULE MOD_ISG_PAR

