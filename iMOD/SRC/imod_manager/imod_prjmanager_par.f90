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
MODULE MOD_PMANAGER_PAR

 INTEGER,PARAMETER :: MAXLEN      =52
 INTEGER,PARAMETER :: MAXTOPICS   =29
 INTEGER,PARAMETER :: MAXSUBTOPICS=6
 INTEGER,PARAMETER :: MAXPERIODS  =10

 TYPE PERIODOBJ
  CHARACTER(LEN=MAXLEN) :: NAME
  INTEGER,DIMENSION(2) :: IDY,IMH,IYR
 END TYPE PERIODOBJ
 TYPE(PERIODOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: PERIOD
 INTEGER :: NPERIOD
 
 TYPE SIMOBJ
  INTEGER :: ISAVE
  CHARACTER(LEN=MAXLEN) :: CDATE
  REAL :: DELT
  INTEGER :: ISUM
 END TYPE SIMOBJ
 TYPE(SIMOBJ),ALLOCATABLE,DIMENSION(:) :: SIM
  
 TYPE STRESSOBJ
  CHARACTER(LEN=MAXLEN) :: CDATE                                !date string of each time step
  TYPE(FILESOBJ),POINTER,DIMENSION(:,:) :: FILES,FILES_TMP => NULL()     !stress information for current defined timestep
                                                                !(I,:) = subtopic i
                                                                !(:,I) = system i
 END TYPE STRESSOBJ
 TYPE(STRESSOBJ),ALLOCATABLE,DIMENSION(:) :: STRESS

 TYPE FILESOBJ
  INTEGER :: IACT                                               !active in runfile
  CHARACTER(LEN=256) :: FNAME                                   !name of current file, could be a constant too!
  CHARACTER(LEN=MAXLEN) :: ALIAS                                !alias name of current file, could be a constant too!
  INTEGER :: ID                                                 !id in treeview field
  INTEGER :: ILAY                                     !layer of current file
  INTEGER :: ICNST                                    !to be constant yes (1) or no (2)
  REAL :: FCT                                      !factor
  REAL :: IMP                                      !impulse
  REAL :: CNST                                      !constant value
 END TYPE FILESOBJ

 TYPE TOPICSOBJ
  INTEGER :: ID                                                 !id of main topics
  INTEGER,POINTER,DIMENSION(:) :: IDT                           !id of each time step for current topic
  INTEGER,POINTER,DIMENSION(:,:) :: ISD                         !id of main subtopics for each timestep
  INTEGER :: IACT                                               !active in runfile
  CHARACTER(LEN=MAXLEN) :: TNAME                                !name of topic
  CHARACTER(LEN=MAXLEN),DIMENSION(MAXSUBTOPICS) :: SNAME        !name of subtopics
  INTEGER :: NSUBTOPICS                                         !number of subtopics
  LOGICAL :: TIMDEP                                             !timedependent module/package
  TYPE(STRESSOBJ),POINTER,DIMENSION(:) :: STRESS,STRESS_TMP => NULL()     !files 
 END TYPE TOPICSOBJ

 TYPE(TOPICSOBJ),DIMENSION(MAXTOPICS) :: TOPICS

 CHARACTER(LEN=3),DIMENSION(MAXTOPICS) :: CMOD
 CHARACTER(LEN=MAXLEN),ALLOCATABLE,DIMENSION(:) :: MENUNAMES
 
 DATA CMOD/'CAP','TOP','BOT','BND','SHD','KDW','KHV','KVA','VCW','KVV', & ! 1-10
           'STO','SSC','PWT','ANI','HFB','IBS','SFT','CPP','CON','PST', & !11-20
           'WEL','DRN','RIV','EVT','GHB','RCH','OLF','CHD','ISG'/         !21-29
 
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IDT
 
 TYPE PRJOBJ
  INTEGER :: ILAY,ICNST,IACT
  REAL :: FCT,IMP,CNST
  CHARACTER(LEN=256) :: FNAME
 END TYPE PRJOBJ
 TYPE(PRJOBJ),ALLOCATABLE,DIMENSION(:) :: PRJ

 INTEGER :: NLAY,NPER,MXNLAY,IUNCONF,ISTEADY,IFORMAT,ISUBMODEL
 REAL :: MINKD,MINC
 LOGICAL :: LBCF,LLPF,LPCG,LPCGN,LSIP,LRCH,LEVT,LDRN,LRIV,LGHB,LOLF,LCHD,LWEL,LISG

 INTEGER,SAVE :: MXITER=500
 INTEGER,SAVE :: ITER1=20
 INTEGER,SAVE :: IIDEBUG=0
 REAL,SAVE :: HCLOSE=0.001
 REAL,SAVE :: RCLOSE=0.1
 REAL,SAVE :: RELAX=0.98
 INTEGER,SAVE :: NPCOND=1
 CHARACTER(LEN=4),DIMENSION(0:1) :: FEXT
 DATA FEXT/'.ARR','.ASC'/
 REAL,DIMENSION(5) :: SUBMODEL
 
END MODULE MOD_PMANAGER_PAR