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

MODULE MOD_RF2MF

IMPLICIT NONE

CHARACTER(LEN=10),PARAMETER :: RVERSION='v3.3'
REAL,PARAMETER :: IDFTINY=0.001 !1.0         !needed to compute ir1,ir2,ic1,ic2 properly

INTEGER,PARAMETER :: MXMOD =21        !MAX. MODFLOW MODULES
INTEGER,PARAMETER :: MXPCK =9         !MAX. MODFLOW PACKAGES

INTEGER,PARAMETER :: PCAP=1
INTEGER,PARAMETER :: PBND=2
INTEGER,PARAMETER :: PSHD=3
INTEGER,PARAMETER :: PKDW=4
INTEGER,PARAMETER :: PVCW=5
INTEGER,PARAMETER :: PSTO=6
INTEGER,PARAMETER :: PPWT=7
INTEGER,PARAMETER :: PANI=8
INTEGER,PARAMETER :: PHFB=9
INTEGER,PARAMETER :: PTOP=10
INTEGER,PARAMETER :: PBOT=11
INTEGER,PARAMETER :: PCON=12
INTEGER,PARAMETER :: PKHV=13
INTEGER,PARAMETER :: PKVV=14
INTEGER,PARAMETER :: PIBS=15
INTEGER,PARAMETER :: PPST=16
INTEGER,PARAMETER :: PKVA=17
INTEGER,PARAMETER :: PSFT=18
INTEGER,PARAMETER :: PCPP=19
INTEGER,PARAMETER :: PSSC=20
INTEGER,PARAMETER :: PSCR=21

INTEGER,PARAMETER :: PWEL=1
INTEGER,PARAMETER :: PDRN=2
INTEGER,PARAMETER :: PRIV=3
INTEGER,PARAMETER :: PEVT=4
INTEGER,PARAMETER :: PGHB=5
INTEGER,PARAMETER :: PRCH=6
INTEGER,PARAMETER :: POLF=7
INTEGER,PARAMETER :: PCHD=8
INTEGER,PARAMETER :: PISG=9

!## global variables
INTEGER,DIMENSION(:,:),ALLOCATABLE,SAVE :: MODSAVE  !## module/package save
INTEGER,DIMENSION(:,:),ALLOCATABLE,SAVE :: PCKSAVE  !## module/package save
INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: MPCK       !## package activated
INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: JACT,NMOD,MMOD,MDIM,PDIM !,MAXPCK
INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: RFMOD      !## module  exists in runfile
INTEGER,DIMENSION(:),ALLOCATABLE,SAVE :: RFPCK      !## package exists in runfile
CHARACTER(LEN=3),DIMENSION(:),ALLOCATABLE,SAVE :: CPCK  !##
CHARACTER(LEN=3),DIMENSION(:),ALLOCATABLE,SAVE :: CMOD  !##
CHARACTER(LEN=10),DIMENSION(:),ALLOCATABLE,SAVE :: OMOD !##
CHARACTER(LEN=10),DIMENSION(:),ALLOCATABLE,SAVE :: OPCK !##
CHARACTER(LEN=24),DIMENSION(:),ALLOCATABLE,SAVE :: TXTPCK !##
CHARACTER(LEN=24),DIMENSION(:),ALLOCATABLE,SAVE :: TXTMOD !##

 REAL,SAVE :: MAXSIMCSIZE                              !## maximal # cells in the end
 REAL,SAVE :: DELT,SIMCSIZE
 REAL,SAVE,DIMENSION(4) :: LAMBDA

 INTEGER,SAVE :: IURUN,IU

 INTEGER,SAVE :: ISAVE,KPER,IFTEST, &
            NMULT,IIDEBUG,IEXPORT,IPOSWEL,ISCEN,IACT,MXNLAY,ICONCHK,&
            NLINES,IMULT,IBDG
 INTEGER,SAVE :: IUNCONF,IFVDL,IARMWP
 INTEGER,SAVE :: ISS       !## 1=STEADYSTATE 2=TRANSIENT
 INTEGER,SAVE :: SDATE     !## 0=no use of starting date otherwise starting date as yyyymmdd
 INTEGER,SAVE :: NSCL,ISCL !## 1=NORMAL      2=IR
 CHARACTER(LEN=20),SAVE :: CDATE
 CHARACTER(LEN=256),SAVE :: LINE,RESULTDIR,RESULTRESDIR !,FNAME
 REAL,DIMENSION(4),SAVE :: USEBOX,SIMBOX,SAVEBOX
 INTEGER,DIMENSION(4),SAVE :: IFULL
 LOGICAL :: LEX
 LOGICAL :: LQD  !## equidistantial grid
! CHARACTER(LEN=8),DIMENSION(2),SAVE :: IRDIRNAMES
! DATA IRDIRNAMES/'default ','scenario'/
 REAL :: WBALERROR

INTEGER,SAVE :: MXCNVG
REAL,SAVE :: MAXWBALERROR

CHARACTER(LEN=256) :: SCENFNAME

REAL,ALLOCATABLE,DIMENSION(:),SAVE :: DELR,DELC

REAL, SAVE :: MINKD, MINC

END MODULE MOD_RF2MF