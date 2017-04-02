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
MODULE MOD_IMPORT_PAR

USE MOD_IDF_PAR, ONLY : IDFOBJ

TYPE(IDFOBJ) :: IDF
TYPE(IDFOBJ),DIMENSION(:),ALLOCATABLE :: BAS,CHD,H,K,TOP,BOT,VA,PSC,KD,C,S,VK

!## parameters
INTEGER,PARAMETER :: MAXIUNIT=27     !max. # files
LOGICAL :: FREEFORMATTED

INTEGER,DIMENSION(:),ALLOCATABLE :: DATAI
CHARACTER(LEN=256),DIMENSION(:),ALLOCATABLE :: DATAF

!## global variables
CHARACTER(LEN=256) :: DIR_DBS,RUNFILE,FNAME_MDL

INTEGER :: IVERSION,SDATE
CHARACTER(LEN=14) :: LONGDATE1,LONGDATE2
INTEGER :: MVERSION
REAL :: XMIN,YMIN,YMAX
REAL :: FT,FL !## transformation factors itminu en ilmuni
INTEGER :: ISUMPCK
INTEGER :: IRIV5 !## 5th column of river
INTEGER :: IBATCH

!## local module variables
CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: FNAME
INTEGER,DIMENSION(:),ALLOCATABLE :: LAYCON,LAYCBD,LAYWT,LTHUF
INTEGER,DIMENSION(:),ALLOCATABLE :: LAYTYPE,LAYAVG,CHANI,LAYVKA,LAYWET
INTEGER,DIMENSION(:),ALLOCATABLE :: IU,IUNIT
REAL,DIMENSION(:),ALLOCATABLE :: DELR,DELC,TRPY
REAL :: HNOFLOW,HCLOSE,RCLOSE,RELAX
INTEGER,SAVE :: NLAY,ITMUNI,IURUN,MAXITER,NITER,IWDFLG
INTEGER,DIMENSION(:),ALLOCATABLE :: NSTP
REAL,DIMENSION(:),ALLOCATABLE :: TPER
REAL,DIMENSION(:),ALLOCATABLE :: DTPER,TSMULT
CHARACTER(LEN=3),DIMENSION(0:MAXIUNIT) :: EXT
INTEGER,DIMENSION(0:MAXIUNIT) :: IFUNIT
INTEGER :: ILAY,IOS,NRCHOP,NEVTOP,MXWELL,MXDRAIN,MXRIVER,MXGHB,MXCHD,MAXHFB,MXSTREAM,IPER
LOGICAL :: LEX

DATA EXT/'BAS','BCF','WEL','DRN','RIV','EVT','XXX', &   !0 -6
         'GHB','RCH','SIP','XXX','SOR','OCD','PCG', &   !7 -13
         'DIS','HFB','LPF','CHD','   ','MOC','TOP', &   !14-20
         'BOT','PTH','ANI','HUF','SCR','HDS','STR'/     !20-27

INTEGER,PARAMETER ::  IBAS=0
INTEGER,PARAMETER ::  IBCF=1
INTEGER,PARAMETER ::  IWEL=2
INTEGER,PARAMETER ::  IDRN=3
INTEGER,PARAMETER ::  IRIV=4
INTEGER,PARAMETER ::  IEVT=5
 !XXX=
INTEGER,PARAMETER ::  IGHB=7
INTEGER,PARAMETER ::  IRCH=8
INTEGER,PARAMETER ::  ISIP=9
 !XXX=
INTEGER,PARAMETER ::  ISOR=11
INTEGER,PARAMETER ::  IOCD=12
INTEGER,PARAMETER ::  IPCG=13

INTEGER,PARAMETER ::  IDIS=14
INTEGER,PARAMETER ::  IHFB=15
INTEGER,PARAMETER ::  ILPF=16
INTEGER,PARAMETER ::  ICHD=17

INTEGER,PARAMETER ::  IMOC=19
INTEGER,PARAMETER ::  ITOP=20
INTEGER,PARAMETER ::  IBOT=21
INTEGER,PARAMETER ::  IPTH=22
INTEGER,PARAMETER ::  IANI=23
  
INTEGER,PARAMETER ::  IHUF=24
INTEGER,PARAMETER ::  ISCR=25
INTEGER,PARAMETER ::  IHDS=26

INTEGER,PARAMETER ::  ISTR=27

END MODULE MOD_IMPORT_PAR
