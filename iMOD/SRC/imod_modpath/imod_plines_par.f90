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
MODULE MOD_PLINES_PAR

USE MOD_IDF_PAR, ONLY : IDFOBJ

INTEGER,PARAMETER :: MAXTYPE=3
INTEGER,ALLOCATABLE,DIMENSION(:) :: PLNPER,NL
INTEGER,ALLOCATABLE,DIMENSION(:,:) :: PLIPER
CHARACTER(LEN=6),DIMENSION(MAXTYPE) :: CTYPE
DATA CTYPE/'BDGFRF','BDGFFF','BDGFLF'/

CHARACTER(LEN=256) :: PLDIRNAME,PLBROWSENAME,MDLDIRNAME !,IPFFNAME
INTEGER :: PLNDIR

TYPE IPFOBJ
 REAL :: X,Y
 INTEGER :: ILAY,INQ,IROW,ICOL
 CHARACTER(LEN=52) :: LABEL,UNQLABEL
END TYPE IPFOBJ
TYPE(IPFOBJ),POINTER,DIMENSION(:) :: IPF
INTEGER :: NUNQ

CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: IBIDF,PORIDF,TBIDF

CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:,:,:) :: HFFNAME  !BDGFRF,BDGFFF,BDGFLF,BDGSTO
CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:,:) :: ITBFNAME   !IBOUND,TOP,BOT,POR_AQF,POR_AQT
REAL,ALLOCATABLE,DIMENSION(:) :: X
REAL,ALLOCATABLE,DIMENSION(:,:) :: XP
INTEGER,ALLOCATABLE,DIMENSION(:) :: Y,YP,SLAY,MAXILAY
INTEGER :: NPER,NLAY,ISS,IREV,ISNK,ISTOPCRIT,JD0,JD1,JD2,IULOG
INTEGER,DIMENSION(2) :: IMODE
REAL :: FRAC,TMAX
CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: SPFNAME,IFFFNAME
INTEGER :: NSPFNAME,ISPFNAME
INTEGER :: NPART,TPART
INTEGER :: NCP1,NRP1,NLP1,NLPOR
REAL,ALLOCATABLE,DIMENSION(:,:,:) :: ZTOP,ZBOT,POR,QX,QY,QZ,BUFF,QSS
REAL,ALLOCATABLE,DIMENSION(:) :: DELR,DELC,LDELR,LDELC,DELX,DELY
REAL,ALLOCATABLE,DIMENSION(:,:) :: XLC,YLC,ZLC,ZLL,TOT
INTEGER,ALLOCATABLE,DIMENSION(:,:,:) :: IBOUND
INTEGER,ALLOCATABLE,DIMENSION(:) :: NCON,KLC,JLC,ILC

TYPE(IDFOBJ) :: IDF  !## model dimensions

END MODULE MOD_PLINES_PAR
