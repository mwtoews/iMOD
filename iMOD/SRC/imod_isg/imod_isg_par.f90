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
MODULE MOD_ISG_PAR

USE MOD_IDF_PAR, ONLY : IDFOBJ

INTEGER,PARAMETER :: MAXLEN=32
INTEGER,PARAMETER :: MAXFILES=10
INTEGER,PARAMETER :: MAXITEMS=12
INTEGER,SAVE :: ICLRSC,ICLRSD,ICLRSP,ICLRND,ICLRST,ICLRQH,ICLRSG
INTEGER,SAVE :: DIMISG,NISG
INTEGER,SAVE :: DIMISP,NISP
INTEGER,SAVE :: DIMISC,DIMDATISC,NDISC,NISC,SELISC
INTEGER,SAVE :: DIMISD,DIMDATISD,NDISD,NISD,SELISD
INTEGER,SAVE :: DIMIST,DIMDATIST,NDIST,NIST,SELIST
INTEGER,SAVE :: DIMISQ,DIMDATISQ,NDISQ,NISQ,SELISQ
INTEGER,SAVE :: ISELISG,IISGPLOT
INTEGER,SAVE :: ISDMAXROW,ISCMAXROW,ISPMAXROW,ISTMAXROW,ISQMAXROW
INTEGER,SAVE :: ISGSHAPES
INTEGER,SAVE :: ICROSS_PNTR,ICROSS_ZVAL

INTEGER,DIMENSION(:,:),ALLOCATABLE :: ISGIU
INTEGER,SAVE :: NISGFILES
CHARACTER(LEN=256),SAVE :: ISGFNAME

TYPE ISPOBJ
 REAL :: X,Y
END TYPE ISPOBJ
TYPE(ISPOBJ),ALLOCATABLE,DIMENSION(:) :: ISP,ISP2,DUMISP
INTEGER :: TISP

TYPE ISGOBJ
 CHARACTER(LEN=50) :: SNAME
 INTEGER :: ICLC,ICRS,ISEG,ISTW,IQHR,ILIST
 INTEGER :: NCLC,NCRS,NSEG,NSTW,NQHR
END TYPE ISGOBJ
TYPE(ISGOBJ),ALLOCATABLE,DIMENSION(:) :: ISG,DUMISG

TYPE DATOBJ1
 INTEGER :: N,IREF
 REAL :: DIST
 CHARACTER(LEN=MAXLEN) :: CNAME
END TYPE DATOBJ1
TYPE(DATOBJ1),ALLOCATABLE,DIMENSION(:) :: ISD,ISC,IST,ISQ,DUMISD,DUMISC,DUMIST,DUMISQ

TYPE ISDOBJ
 INTEGER :: IDATE
 INTEGER :: ID_STW  !influenced by structure #
 REAL :: WLVL,BTML,RESIS,INFF
 REAL :: WL_STW       !influenced waterlevel
END TYPE ISDOBJ
TYPE(ISDOBJ),ALLOCATABLE,DIMENSION(:) :: DATISD,DUMDATISD,ISGEDITISD
TYPE(ISDOBJ),ALLOCATABLE,DIMENSION(:,:) :: DATISD2
INTEGER,DIMENSION(:),ALLOCATABLE :: TISD

TYPE ISCOBJ
 REAL :: DISTANCE,BOTTOM,KM
END TYPE ISCOBJ
TYPE(ISCOBJ),ALLOCATABLE,DIMENSION(:) :: DATISC,DUMDATISC,ISGEDITISC
TYPE(ISCOBJ),ALLOCATABLE,DIMENSION(:,:) :: DATISC2
INTEGER,DIMENSION(:),ALLOCATABLE :: TISC,ISCN

TYPE ISTOBJ
 INTEGER :: IDATE
 REAL :: WLVL_UP,WLVL_DOWN
END TYPE ISTOBJ
TYPE(ISTOBJ),ALLOCATABLE,DIMENSION(:) :: DATIST,DUMDATIST,ISGEDITIST
TYPE(ISTOBJ),ALLOCATABLE,DIMENSION(:,:) :: DATIST2
INTEGER,DIMENSION(:),ALLOCATABLE :: TIST

TYPE ISQOBJ
 REAL :: QZ,HZ,QW,HW
END TYPE ISQOBJ
TYPE(ISQOBJ),ALLOCATABLE,DIMENSION(:) :: DATISQ,DUMDATISQ,ISGEDITISQ
TYPE(ISQOBJ),ALLOCATABLE,DIMENSION(:,:) :: DATISQ2
INTEGER,DIMENSION(:),ALLOCATABLE :: TISQ

CHARACTER(LEN=24),DIMENSION(MAXITEMS) :: FNAME
REAL,DIMENSION(2,MAXITEMS) :: VALUE

CHARACTER(LEN=4),DIMENSION(MAXFILES) :: EXT
CHARACTER(LEN=50),DIMENSION(MAXFILES) :: TFORM
INTEGER,DIMENSION(MAXFILES) :: RECLEN

DATA FNAME/'COND','STAGE','BOTTOM','INFFCT','TOTAL_LENGTH','MEAN_WPERIMETER','MEAN_WWIDTH','RESISTANCE','EROSION','EFFECT','CUR_ID','NEX_ID'/
DATA EXT/'ISG ','ISP ','ISD1','ISD2','ISC1','ISC2','IST1','IST2','ISQ1','ISQ2'/
DATA RECLEN/0,8,44,20,44,12,44,12,44,16/
DATA TFORM/'FORMATTED  ','UNFORMATTED','UNFORMATTED','UNFORMATTED', &
           'UNFORMATTED','UNFORMATTED','UNFORMATTED','UNFORMATTED'  , &
           'UNFORMATTED','UNFORMATTED'/

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

REAL :: XMIN,YMIN,XMAX,YMAX !## area to be gridded (x1,y1,x2,y2)'
INTEGER :: ISS !## (1) mean over all periods, (2) mean over given period'
INTEGER :: SDATE,EDATE,DDATE !## startdate,enddate,ddate (yyyymmdd,yyyymmdd,dd)'
INTEGER :: IDIM !## (0) give area (2) entire domain of isg (3) selected isg'
REAL :: CS !## cellsize'
REAL :: MINDEPTH !## minimal waterdepth for computing conductances (m)'
REAL :: WDEPTH !## waterdepth only used in combination with isimgro>0'
INTEGER :: ICDIST !## (0) do not compute effect of weirs (1) do compute effect of weirs'
INTEGER :: ISIMGRO !## ISIMGRO'
CHARACTER(LEN=256) :: ROOT  !## resultmap'
CHARACTER(LEN=52) :: POSTFIX  !## POSTFIX {POSTFIX}_stage.idf etc.'
REAL :: NODATA  !## nodatavalue in ISG
INTEGER,DIMENSION(12) :: ISAVE
REAL :: MAXWIDTH !#3 maximum widht for computing rivier-width (in case cross-sections are rubbish)
INTEGER :: IAVERAGE !## (1) mean (2) median value

END MODULE MOD_ISG_PAR

