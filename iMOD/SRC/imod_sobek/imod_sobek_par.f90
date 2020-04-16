!!  Copyright (C) Stichting Deltares, 2005-2020.
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
MODULE MOD_SOBEK_PAR

 USE DIO_PLT_RW
 USE IMODVAR, ONLY : SP_KIND,DP_KIND
 
 INTEGER,PARAMETER :: MAXLEN=32
 INTEGER,PARAMETER :: MAXPROF=500,MAXCPNT=10000
 INTEGER,PARAMETER :: ITP=1,ICR=2,ICP=3,IGR=4,IAT=5,IEF=6,IFR=7,      &
                      ISG=8,ISD1=9,ISP=10,ISC1=11,ISD2=12,ISC2=13,    &
                      ISQ1=14,ISQ2=15,IST=16,IST1=17,IST2=18, &
                      IHIS=19,SHIS=20,IOUT=21 
                      
 CHARACTER(LEN=256) :: SOBEKDIR,CALCPNTHISNAME,STRUCHISNAME,ISGNAME
 INTEGER :: IBATCH

 INTEGER,PARAMETER :: NIU=21
 CHARACTER(LEN=256),DIMENSION(NIU) :: FNAME
 CHARACTER(LEN=256) :: LINE
 INTEGER,DIMENSION(NIU) :: IOS,IU
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: X,Y

 INTEGER :: NNODE,NNDLK,NBRCH,NCPNT,NCALCP,NCROS,NDAT,NDEF,NBDFR,NSTUW,NQHREL
 INTEGER,DIMENSION(NIU) :: IL
 DATA IL/ITP,ICR,ICP,IGR,IAT,IEF,IFR,IST,0,0,0,0,0,0,0,0,0,0,0,0,0/

 TYPE TP1OBJ
  REAL(KIND=DP_KIND) :: PX,PY
  CHARACTER(LEN=MAXLEN) :: ID
 END TYPE TP1OBJ
 TYPE(TP1OBJ),ALLOCATABLE,DIMENSION(:) :: TP1  !TP-POINTS

 TYPE TP2OBJ
  REAL(KIND=DP_KIND) :: AL
  CHARACTER(LEN=MAXLEN) :: ID,CBN,CEN
 END TYPE TP2OBJ
 TYPE(TP2OBJ),ALLOCATABLE,DIMENSION(:) :: TP2  !TP-SEGMENTS

 TYPE TP3OBJ
  REAL(KIND=DP_KIND) :: PX,PY,LC
  CHARACTER(LEN=MAXLEN) :: ID,CI
 END TYPE TP3OBJ
 TYPE(TP3OBJ),ALLOCATABLE,DIMENSION(:) :: TP3  !NLD-POINTS

 TYPE CPOBJ
  REAL(KIND=DP_KIND) :: ANGLE,SLEN
  CHARACTER(LEN=MAXLEN) :: ID
 END TYPE CPOBJ
 TYPE(CPOBJ),ALLOCATABLE,DIMENSION(:) :: CP    !CP

 TYPE CROBJ
  REAL(KIND=DP_KIND) :: LC
  CHARACTER(LEN=MAXLEN) :: ID,CI
 END TYPE CROBJ
 TYPE(CROBJ),ALLOCATABLE,DIMENSION(:) :: CR    !CR

 TYPE GROBJ
  REAL(KIND=DP_KIND) ::  LC !gr gr = grid table, with the distance of every grid point from the beginning of the branch.
  CHARACTER(LEN=MAXLEN) :: ID !id=id of the gridpoint;
  CHARACTER(LEN=MAXLEN) :: CI !ci=carrier id;
 END TYPE GROBJ
 TYPE(GROBJ),ALLOCATABLE,DIMENSION(:) :: GR    !GR

 TYPE QHOBJ
  REAL(KIND=DP_KIND) ::  LC !gr gr = grid table, with the distance of every grid point from the beginning of the branch.
  CHARACTER(LEN=MAXLEN) :: ID !id=id of the gridpoint;
  CHARACTER(LEN=MAXLEN) :: CI !ci=carrier id;
 END TYPE QHOBJ
 TYPE(QHOBJ),ALLOCATABLE,DIMENSION(:) :: QH    !QH

 TYPE STOBJ
  REAL(KIND=DP_KIND) ::  LC !st st = stuwen table, with the distance of every weir from the beginning of the branch.
  CHARACTER(LEN=MAXLEN) :: ID !id=id of the weir;
  CHARACTER(LEN=MAXLEN) :: CI !ci=carrier id;
 END TYPE STOBJ
 TYPE(STOBJ),ALLOCATABLE,DIMENSION(:) :: ST    !ST

 TYPE PDATOBJ
  CHARACTER(LEN=MAXLEN) :: ID,DI
  REAL(KIND=DP_KIND) :: RL
 END TYPE PDATOBJ
 TYPE(PDATOBJ),ALLOCATABLE,DIMENSION(:) :: PDAT !PROFILE.DAT

 TYPE PDEFOBJ
  INTEGER :: PTYPE,NNXY
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: XPROF,YPROF
  CHARACTER(LEN=MAXLEN) :: ID
 END TYPE PDEFOBJ
 TYPE(PDEFOBJ),ALLOCATABLE,DIMENSION(:) :: PDEF !PROFILE.DEF

 TYPE BDFROBJ
  CHARACTER(LEN=MAXLEN) :: ID,CI
  INTEGER :: MF
  REAL(KIND=DP_KIND) :: MRC
 END TYPE BDFROBJ
 TYPE(BDFROBJ),ALLOCATABLE,DIMENSION(:) :: BDFR !FRICTION.DAT

 TYPE(DIOPLTTYPE) :: HISDATASET,SHISDATASET
 INTEGER,DIMENSION(:),ALLOCATABLE :: TIMINDICES
 REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: JULIANTIMES
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:,:) :: SELECTEDVALUES
 INTEGER :: DIMTIMES,NTIMES

END MODULE MOD_SOBEK_PAR
