!!  Copyright (C) Stichting Deltares, 2005-2018.
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
MODULE MOD_IR_PAR

USE WINTERACTER
USE RESOURCE
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE IMODVAR, ONLY : DP_KIND,SP_KIND

INTEGER,PARAMETER :: MAXLENIR=30
CHARACTER(LEN=256) :: PRJFNAME
CHARACTER(LEN=MAXLENIR),DIMENSION(3) :: CTREE
CHARACTER(LEN=256) :: DIRNAME
INTEGER,ALLOCATABLE,DIMENSION(:) :: IFIXED  !## variable fixed 0=no;1=yes
REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: IMP        !## impulse strength

INTEGER :: ICLRMEASURE,ICLRTARGET !## default colours for polygons
INTEGER,PARAMETER :: TFONT=FFHELVETICA
INTEGER,PARAMETER :: MAXTARGET=50,MAXMEASURE=50,MAXRESULT=50

INTEGER,DIMENSION(3) :: TABID,BUTID,ACTID

INTEGER :: IRWIN

INTEGER(KIND=1),DIMENSION(:),ALLOCATABLE :: ISELGEN
REAL(KIND=DP_KIND) :: XCMOUSE,YCMOUSE

!## coefficients for inverse IR computation
REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:,:,:,:,:) :: COEF

TYPE IDFTYPE
 CHARACTER(LEN=256) :: IDFFILE
 CHARACTER(LEN=50) :: IDFNAME
 TYPE(IDFOBJ) :: IDF !idf object
 REAL(KIND=DP_KIND) :: IMP         !impulse strength for current idf
END TYPE IDFTYPE
TYPE(IDFTYPE),ALLOCATABLE,DIMENSION(:) :: IDF,IDFP
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: EFFECT

TYPE PERTYPE
 CHARACTER(LEN=50) :: NAMEPER
 INTEGER           :: IPERPER
END TYPE PERTYPE
INTEGER :: NPER
TYPE(PERTYPE),ALLOCATABLE,DIMENSION(:) :: PER
INTEGER,ALLOCATABLE,DIMENSION(:) :: IPR

TYPE RESTYPE
 CHARACTER(LEN=256) :: LEGRES
 CHARACTER(LEN=50)  :: NAMERES,DIRRES
 INTEGER            :: ILAYRES
 INTEGER            :: ITYPERES  !0=m, 1=m3/d
END TYPE RESTYPE
INTEGER :: NRES
TYPE(RESTYPE),ALLOCATABLE,DIMENSION(:) :: RES
INTEGER,ALLOCATABLE,DIMENSION(:) :: IRS

CHARACTER(LEN=100),DIMENSION(:),ALLOCATABLE :: PERRES  !## combined name for "per" and "res"

TYPE IRTYPE
 CHARACTER(LEN=50) :: NAMEIR
 CHARACTER(LEN=256) :: DIRIR,IDFIR,SDFIR,SDFBM
 REAL(KIND=DP_KIND) :: MINIR,MAXIR
 INTEGER :: TYPEIR  !0=REAL(KIND=DP_KIND),1=binair
 !## neccessary for linear-programming
 INTEGER :: ISEL    !selected in ir-inverse mode
 REAL(KIND=DP_KIND) :: ULIMP      !upper limit
 REAL(KIND=DP_KIND) :: LLIMP      !lower limit
 REAL(KIND=DP_KIND) :: IMP        !fixed limit
 INTEGER :: IFIXED  !fixed
END TYPE IRTYPE
TYPE(IRTYPE),DIMENSION(:),ALLOCATABLE :: IR
INTEGER :: NIR
CHARACTER(LEN=256) :: MAINRESDIR,RESDIR,QUARTERRUNFILE,BASISRUNFILE,TARGETLEG

TYPE BCTYPE
 INTEGER,DIMENSION(:),POINTER :: IXY              !## number of coordinates
 REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: X,Y                 !## coordinates
 REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: XMIN,XMAX,YMIN,YMAX !## min/max of polygon
 INTEGER :: NCRD,NGEN
 INTEGER :: ITYPE  !## 0=gen,1=ipf
 CHARACTER(LEN=256) :: BCNAME
END TYPE BCTYPE
TYPE(BCTYPE),ALLOCATABLE,DIMENSION(:) :: BC
INTEGER :: NBC 

REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: XPOL,YPOL !## used in ir1highlightgen to highlight selected polygons

!##-------------
!## definition of TARGET object and sub-objects
!##-------------

TYPE SUB_TARGET
 INTEGER :: INEWP      !period
 INTEGER :: INEWT      !topic
 REAL(KIND=DP_KIND) :: LOWER      !lower limit
 REAL(KIND=DP_KIND) :: UPPER      !upper limit
 REAL(KIND=DP_KIND) :: MEAN !FLUXSUM    !mean values!sum of fluxes
 INTEGER :: POLSIZE    !size of polygon in cells (used in IR1FIELDS_TAB3_CALCDIFF)
END TYPE SUB_TARGET

TYPE SUB_TARGET_POLYGON
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: X,Y      !x-y coordinates
 INTEGER :: NCRD       !number of coordinates
 INTEGER :: ITYPE      !polygon type
 INTEGER :: IACT       !polygon selected
 INTEGER :: ICLR       !colour of polygon
 INTEGER :: WIDTH      !width of line
 CHARACTER(LEN=25) :: POLNAME   !name of polygon
! INTEGER :: EFFECT     !effectiveness
 INTEGER :: NDEF       !number of definitions
 TYPE(SUB_TARGET),POINTER,DIMENSION(:) :: DEF   !definition object
END TYPE SUB_TARGET_POLYGON

TYPE TYPE_TARGET
 INTEGER :: TARGET_ID                   !id of current target in treefield
 INTEGER :: IDPOS                       !id of mother id
 CHARACTER(LEN=MAXLENIR) :: CNAME         !name of the target
 INTEGER :: NPOL
 TYPE(SUB_TARGET_POLYGON),POINTER,DIMENSION(:) :: POL
END TYPE TYPE_TARGET

TYPE(TYPE_TARGET),ALLOCATABLE,DIMENSION(:),SAVE :: TTREE
INTEGER :: MAXDEF  !maximum number of definitions in application, depends on gridsize in resource.rc
INTEGER :: NTARGET !number of targets currently available

!##-------------
!## definition of MEASURE object and sub-objects
!##-------------

TYPE SUB_MEASURE
 REAL(KIND=DP_KIND) :: IMP    !impulse strength
 REAL(KIND=DP_KIND) :: FT_IMP !full-throttle strength
 INTEGER :: IMES   !number of the measurement
END TYPE SUB_MEASURE

TYPE OPTIMIZETYPE
 INTEGER :: ISEL    !selected in ir-inverse mode
 REAL(KIND=DP_KIND) :: ULIMP      !upper limit
 REAL(KIND=DP_KIND) :: LLIMP      !lower limit
 REAL(KIND=DP_KIND) :: IMP        !fixed limit
 INTEGER :: IFIXED  !fixed
END TYPE OPTIMIZETYPE

TYPE SUB_MEASURE_POLYGON
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: X,Y      !x-y coordinates
 INTEGER :: NCRD       !number of coordinates
 INTEGER :: ITYPE      !polygon type
 INTEGER :: IACT       !polygon selected
 INTEGER :: ICLR       !colour of polygon
 INTEGER :: WIDTH      !width of line
 CHARACTER(LEN=25) :: POLNAME   !name of polygon
 INTEGER :: NMES       !number of measures
 TYPE(SUB_MEASURE),POINTER,DIMENSION(:) :: MES   !measure object
END TYPE SUB_MEASURE_POLYGON

TYPE TYPE_MEASURE
 INTEGER :: MEASURE_ID                  !id of current measure in treefield
 INTEGER :: IDPOS                       !id of mother id
 CHARACTER(LEN=MAXLENIR) :: CNAME         !name of the measure
 INTEGER :: NPOL
 TYPE(SUB_MEASURE_POLYGON),POINTER,DIMENSION(:) :: POL
 INTEGER :: NOPT
 TYPE(OPTIMIZETYPE),POINTER,DIMENSION(:) :: OPT
END TYPE TYPE_MEASURE

TYPE(TYPE_MEASURE),ALLOCATABLE,DIMENSION(:),SAVE :: MTREE
INTEGER :: MAXMES   !maximum number of measures in application, depends on gridsize in resource.rc
INTEGER :: NMEASURE !number of measures currently available

!##-------------
!## definition of RESULTS object and sub-objects
!##-------------

TYPE TYPE_RESULT
 INTEGER :: RESULT_ID                   !reference to measure id in treefield
 INTEGER :: IDPOS                       !id of mother id
 INTEGER :: IMENU                       !selected menu-item
 CHARACTER(LEN=MAXLENIR) :: CNAME         !name of the result
END TYPE TYPE_RESULT
TYPE(TYPE_RESULT),ALLOCATABLE,DIMENSION(:),SAVE :: RTREE
INTEGER :: NRESULT

DATA TABID/ID_DIR_PMTAB1,ID_DIR_PMTAB2,ID_DIR_PMTAB3/
DATA BUTID/ID_NEWTARGET ,ID_NEWMEASURE,ID_NEWRESULTS/

INTEGER :: ICUR_IFIELD,ICUR_ITREE !## previous tree-field
INTEGER :: NLIST
CHARACTER(LEN=256) :: IDFNAME
CHARACTER(LEN=256),DIMENSION(:),ALLOCATABLE :: IDFRESLIST
INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:) :: IP
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDFDIFF

END MODULE MOD_IR_PAR
