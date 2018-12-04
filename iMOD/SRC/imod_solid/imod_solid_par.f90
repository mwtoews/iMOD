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
MODULE MOD_SOLID_PAR

USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MODPLOT, ONLY : BITMAPOBJ
USE IMODVAR, ONLY : DP_KIND,SP_KIND

INTEGER :: IBATCH,IWINDOW 
CHARACTER(LEN=256),SAVE :: SOLFILE,OUTPUTFOLDER,REGISTOP,REGISBOT,REGISKHV,REGISKVV

TYPE GENOBJ
 INTEGER :: ILAY
 REAL(KIND=DP_KIND) :: FCT
 CHARACTER(LEN=256) :: FNAME
END TYPE GENOBJ
TYPE(GENOBJ),ALLOCATABLE,DIMENSION(:) :: GENSOL
INTEGER :: NGENSOL

TYPE IPFPOBJ
 INTEGER :: ILAY
 CHARACTER(LEN=256) :: FNAME
END TYPE IPFPOBJ
TYPE(IPFPOBJ),ALLOCATABLE,DIMENSION(:) :: IPFP
INTEGER :: NIPFP

INTEGER,PARAMETER :: MXPX=500    !## max. 500 point in graph

TYPE SPFOBJ_PROF
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: PX !## cross-section position
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: PZ !## cross-section value
 INTEGER :: NPOS                 !## number of points on cross-section
 INTEGER :: ICLR                 !## colour of the cross-section
 INTEGER :: IWIDTH               !## width of the cross-section
 INTEGER :: IACTIVE              !## line active or not in interpolation
 CHARACTER(LEN=52) :: LNAME      !## line label
END TYPE SPFOBJ_PROF

TYPE SPFOBJ
 CHARACTER(LEN=256) :: FNAME       !## name of spf
 INTEGER :: NXY                    !## number of coordinates
 REAL(KIND=DP_KIND) :: TX                        !## length of cross-section
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: X,Y  !## coordinates of cross-section
 TYPE(SPFOBJ_PROF),POINTER,DIMENSION(:) :: PROF
 TYPE(BITMAPOBJ) :: PBITMAP
END TYPE SPFOBJ
TYPE(SPFOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: SPF  

!## to be used for consensus modelling, storage of (supplementary) solids
TYPE SOLIDOBJ
 INTEGER :: NINT
 CHARACTER(LEN=256) :: SNAME
 CHARACTER(LEN=256),POINTER,DIMENSION(:) :: INTNAME
 INTEGER,POINTER,DIMENSION(:) :: INTCLR,ICLC,ICHECK
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: XRESOLUTION
END TYPE SOLIDOBJ
TYPE(SOLIDOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: SLD

INTEGER,SAVE :: NBOREHOLES   !## number of boreholes
CHARACTER(LEN=256),DIMENSION(:),ALLOCATABLE :: FBOREHOLES

TYPE MASKOBJ
 CHARACTER(LEN=256) :: FNAME
 CHARACTER(LEN=52) :: ALIAS
 TYPE(IDFOBJ) :: IDF
END TYPE MASKOBJ
TYPE(MASKOBJ),ALLOCATABLE,DIMENSION(:) :: MASK

REAL(KIND=DP_KIND),POINTER,DIMENSION(:),SAVE :: PX,PY,PZ

INTEGER,DIMENSION(:),ALLOCATABLE ,SAVE:: SOLID_ILIST
REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE ,SAVE:: DZ
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: TB,SOLIDF,TOPIDF,BOTIDF,CIDF,KDHIDF,KDVIDF,CFRACIDF,KDFRACIDF,KH,KV,FFRAC,CFRAC
TYPE(IDFOBJ),SAVE :: MDLIDF
CHARACTER(LEN=256),SAVE :: FNAME
INTEGER,SAVE :: NSPF,ISPF
INTEGER,SAVE :: NMASK
INTEGER,SAVE :: IMASK
INTEGER,SAVE :: IBNDCHK
INTEGER,SAVE :: SOL_IINT_IDF
INTEGER,SAVE :: NTBSOL !## number of solids (tops/bottoms)

INTEGER,ALLOCATABLE,DIMENSION(:) :: ISEL_IDF,ICHECK_IDF,IACT,ICHECK,ICLEAN,IEXIST
REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: GCODE,XEXCLUDE,DTOL,TOP,BOT,XRESOLUTION
REAL(KIND=DP_KIND),SAVE :: MINC

TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: KHG,KVG,IBG,SHG,TPM,BTM,KHM,KAM,KVM,SHM,IBM
INTEGER,SAVE :: NLAYG,NLAYM
CHARACTER(LEN=256) :: RESULTFOLDER
INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:) :: IBND

INTEGER :: MXITER1=5000 !## outer (linear system)
INTEGER :: MXITER2=20   !## inner (linear system)
INTEGER :: IDAMPING=0   !## use adaptive damping
REAL(KIND=DP_KIND) :: HCLOSE=0.01D0    !## m
REAL(KIND=DP_KIND) :: RCLOSE=10000.0D0  !## m3/dag
REAL(KIND=DP_KIND) :: RELAX=0.98D0      !##
REAL(KIND=DP_KIND) :: FMIDELEV=1.0D0    !## average in between aquifers
REAL(KIND=DP_KIND) :: FTIGHT=100.0D0    !## how tight works itight=2
INTEGER :: ITIGHT=1     !## using constant head boundary condition
REAL(KIND=DP_KIND),PARAMETER :: HNOFLOW=-9999.0D0
INTEGER :: MICNVG=25

!## kriging settings per interface
TYPE KRIGINGOBJ
 INTEGER :: MAXPNT,KTYPE,PNTSEARCH,COINCIDENT,IQUADRANT
 REAL(KIND=DP_KIND) :: RANGE,SILL,NUGGET,COINCIDENTDIST
END TYPE KRIGINGOBJ
TYPE(KRIGINGOBJ),ALLOCATABLE,DIMENSION(:) :: KSETTINGS

CHARACTER(LEN=256),DIMENSION(:),POINTER :: REGISFILES_TOP,REGISFILES_BOT,REGISFILES_KHV,REGISFILES_KVV

INTEGER :: NROW,NCOL,NLAY

TYPE PCGOBJ
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:,:) :: RHS,CC,CR,CV,P,V,SS,CD,HCOF,HOLD,COND
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:,:) :: HNEW
 INTEGER,POINTER,DIMENSION(:,:) :: IB
END TYPE PCGOBJ
TYPE(PCGOBJ),ALLOCATABLE,DIMENSION(:) :: PCG

LOGICAL :: LSPLINE=.TRUE.
REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: XD,YD,ZD,XDDUMMY,YDDUMMY,ZDDUMMY

END MODULE MOD_SOLID_PAR
