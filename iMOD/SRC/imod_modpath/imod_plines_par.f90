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
MODULE MOD_PLINES_PAR

USE MOD_IDF_PAR, ONLY : IDFOBJ
USE OPENGL
USE IMODVAR, ONLY : DP_KIND,SP_KIND

INTEGER,PARAMETER :: MAXTYPE=3
INTEGER,ALLOCATABLE,DIMENSION(:) :: PLNPER,PLNLAY
INTEGER,ALLOCATABLE,DIMENSION(:,:) :: PLIPER
CHARACTER(LEN=6),DIMENSION(MAXTYPE) :: CTYPE
DATA CTYPE/'BDGFRF','BDGFFF','BDGFLF'/ !,'BDGSTO'/

CHARACTER(LEN=256) :: PLDIRNAME,PLBROWSENAME,MDLDIRNAME,PLSAVEDIRNAME  ! PLDIRNAME: folder containing MODEL folder(s), PLBROWSENAME: MODEL folder, MDLDIRNAMER: user given MODEL folder, PLSAVEDIRNAME: location to save results (*.RUN, *.IFF and *.IPF)
INTEGER :: PLNDIR

TYPE IPFOBJ
 REAL(KIND=DP_KIND) :: X,Y
 INTEGER :: ILAY,INQ,IROW,ICOL
 CHARACTER(LEN=52) :: LABEL,UNQLABEL
END TYPE IPFOBJ
TYPE(IPFOBJ),POINTER,DIMENSION(:) :: IPF
INTEGER :: NUNQ

CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: IBIDF,PORIDF,TBIDF

CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:,:,:) :: HFFNAME  !## BDGFRF,BDGFFF,BDGFLF,BDGSTO
CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:,:) :: ITBFNAME   !## IBOUND,TOP,BOT,POR_AQF,POR_AQT
INTEGER,SAVE :: NPER,NLAY,ISS,IREV,ISNK,ISTOPCRIT,JD0,JD1,JD2,ISTO 
INTEGER,DIMENSION(2) :: IMODE
INTEGER,ALLOCATABLE,DIMENSION(:,:) :: IUOUT
REAL(KIND=DP_KIND) :: FRAC,TMAX 
CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: SPFNAME,IFFFNAME
INTEGER :: NSPFNAME,ISPFNAME
INTEGER :: TPART
INTEGER :: NCP1,NRP1,NLP1,NLPOR
REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: DELR,DELC,LDELR,LDELC,DELX,DELY
REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:,:) :: ZTOP,ZBOT,POR,QX,QY,QZ,QS,QSS
INTEGER,ALLOCATABLE,DIMENSION(:,:,:) :: IBOUND
INTEGER,ALLOCATABLE,DIMENSION(:) :: NCON
INTEGER,SAVE :: NSPG    !## number of startpoint groups

TYPE SPOBJ
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:)    :: XLC   ,YLC   ,ZLC   ,ZLL   ,TOT   
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:)    :: XLC_BU,YLC_BU,ZLC_BU,ZLL_BU,TOT_BU
 INTEGER,POINTER,DIMENSION(:) :: ILC   ,JLC   ,KLC   ,STM
 INTEGER,POINTER,DIMENSION(:) :: ILC_BU,JLC_BU,KLC_BU,STM_BU
 INTEGER,POINTER,DIMENSION(:) :: MXL   ,MXL_BU          !## max. modellayer
 INTEGER :: NPART  !## number particles per group
 INTEGER :: ICLR   !## particle colour
 INTEGER :: IACT   !## particle active (0=no; 1=yes)
 INTEGER :: IREV   !## direction (0=forward; 1=backward)
 INTEGER :: IRSTRT !## random start in time
 REAL(KIND=DP_KIND) :: SPWIDTH   !## plot size startpoint (0=no, 1.0D0,2,3,4,5.0 = size)
 REAL(KIND=DP_KIND) :: PWIDTH    !## plot size particle (0=no, 1.0D0,2,3,4,5.0 = size)
END TYPE SPOBJ
TYPE(SPOBJ),DIMENSION(:),ALLOCATABLE :: SP,SPR

TYPE PLOBJ
 REAL(KIND=DP_KIND) :: TDEL,TCUR,TMAX
 INTEGER :: IREV,NPART,IPLOTSP,SPCOLOR,ITYPE
 INTEGER :: NPER,NTIME,NTREP
 INTEGER :: IRUN=0
END TYPE PLOBJ
TYPE(PLOBJ) :: PL

TYPE(IDFOBJ) :: IDF  !## model dimensions

INTEGER(KIND=1),DIMENSION(:),ALLOCATABLE :: IVISIT
INTEGER,DIMENSION(:),ALLOCATABLE :: LVISIT
INTEGER :: NVISIT

END MODULE MOD_PLINES_PAR
