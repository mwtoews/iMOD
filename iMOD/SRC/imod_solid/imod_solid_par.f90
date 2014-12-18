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
MODULE MOD_SOLID_PAR

USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_PROF_PAR, ONLY : BITMAPOBJ

INTEGER :: IBATCH,IMIDELEV,IWINDOW 
CHARACTER(LEN=256),SAVE :: SOLFILE,OUTPUTFOLDER,REGISTOP,REGISBOT,REGISKHV,REGISKVV

INTEGER,PARAMETER :: MXPX=500    !## max. 500 point in graph

TYPE SPFOBJ_PROF
 REAL,POINTER,DIMENSION(:) :: PX !## cross-section position
 REAL,POINTER,DIMENSION(:) :: PZ !## cross-section value
 INTEGER :: NPOS                 !## number of points on cross-section
 INTEGER :: ICLR                 !## colour of the cross-section
 INTEGER :: IWIDTH               !## width of the cross-section
 CHARACTER(LEN=52) :: LNAME      !## line label
END TYPE SPFOBJ_PROF

TYPE SPFOBJ
 CHARACTER(LEN=256) :: FNAME       !## name of spf
 INTEGER :: NXY                    !## number of coordinates
 REAL :: TX                        !## length of cross-section
 REAL,POINTER,DIMENSION(:) :: X,Y  !## coordinates of cross-section
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
 REAL,POINTER,DIMENSION(:) :: XRESOLUTION
END TYPE SOLIDOBJ
TYPE(SOLIDOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: SLD

INTEGER,SAVE :: NBOREHOLES   !## nuber of boreholes
CHARACTER(LEN=256),DIMENSION(:),ALLOCATABLE :: FBOREHOLES

TYPE MASKOBJ
 CHARACTER(LEN=256) :: FNAME
 CHARACTER(LEN=52) :: ALIAS
 TYPE(IDFOBJ) :: IDF
END TYPE MASKOBJ
TYPE(MASKOBJ),ALLOCATABLE,DIMENSION(:) :: MASK

REAL,POINTER,DIMENSION(:),SAVE :: PX,PY,PZ

INTEGER,DIMENSION(:),ALLOCATABLE ,SAVE:: ILIST
REAL,DIMENSION(:),ALLOCATABLE ,SAVE:: DZ
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: TB,SOLIDF,TOPIDF,BOTIDF,CIDF,KDHIDF,KDVIDF,CFRACIDF,KDFRACIDF,KH,KV,FFRAC,CFRAC
TYPE(IDFOBJ),SAVE :: MDLIDF
CHARACTER(LEN=256),SAVE :: FNAME
INTEGER,SAVE :: NSPF,ISPF
INTEGER,SAVE :: NMASK
INTEGER,SAVE :: IMASK
INTEGER,SAVE :: NTBSOL !## number of solids (tops/bottoms)

INTEGER,ALLOCATABLE,DIMENSION(:) :: ISEL_IDF,ICHECK_IDF,IACT,ICHECK,ICLEAN,IEXIST
REAL,ALLOCATABLE,DIMENSION(:) :: GCODE,XEXCLUDE,DTOL,TOP,BOT,XRESOLUTION
REAL,SAVE :: MINC

TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: KHG,KVG,IBG,SHG,TPM,BTM,KHM,KAM,KVM,SHM,IBM
INTEGER,SAVE :: NLAYG,NLAYM
CHARACTER(LEN=256) :: RESULTFOLDER
INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:) :: IBND

INTEGER :: MXITER1=5000 !## outer (linear system)
INTEGER :: MXITER2=20   !## inner (linear system)
INTEGER :: IDAMPING=1   !## use adaptive damping
REAL :: HCLOSE=0.001    !## m
REAL :: RCLOSE=10000.0  !## m3/dag
REAL :: RELAX=1.0      !##
INTEGER :: ITIGHT=1     !## using constant head boundary condition
REAL,PARAMETER :: HNOFLOW=-9999.0
INTEGER :: MICNVG=25

END MODULE MOD_SOLID_PAR
