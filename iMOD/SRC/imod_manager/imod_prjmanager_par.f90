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
 INTEGER,PARAMETER :: MAXSUBTOPICS=24
 INTEGER,PARAMETER :: MAXPERIODS  =10

 TYPE PERIODOBJ
  CHARACTER(LEN=MAXLEN) :: NAME
  INTEGER,DIMENSION(2) :: IYR
  INTEGER :: IDY,IMH,IHR,IMT,ISC
 END TYPE PERIODOBJ
 TYPE(PERIODOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: PERIOD
 INTEGER :: NPERIOD
 
 TYPE SIMOBJ
  INTEGER :: ISAVE
  CHARACTER(LEN=MAXLEN) :: CDATE
  REAL :: DELT
  INTEGER :: DDAY,DSEC
  INTEGER :: ISUM
  INTEGER :: IDY,IMH,IYR
  INTEGER :: IHR,IMT,ISC
 END TYPE SIMOBJ
 TYPE(SIMOBJ),POINTER,DIMENSION(:) :: SIM,SIM_C,SIM_C2
  
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

 TYPE STRESSOBJ
  CHARACTER(LEN=MAXLEN) :: CDATE                                !date string of each time step
  INTEGER :: IYR,IMH,IDY                                        !years,months ,days
  INTEGER :: IHR,IMT,ISC                                        !hours,minutes,seconds
  TYPE(FILESOBJ),POINTER,DIMENSION(:,:) :: FILES,FILES_TMP => NULL()     !stress information for current defined timestep
                                                                !(I,:) = subtopic i
                                                                !(:,I) = system i
  CHARACTER(LEN=256),POINTER,DIMENSION(:) :: INPFILES => NULL()  !stores the inp files for metaswap
 END TYPE STRESSOBJ
 TYPE(STRESSOBJ),ALLOCATABLE,DIMENSION(:) :: STRESS

 TYPE PSTOBJ
  INTEGER :: PE_MXITER=10
  INTEGER :: PE_NPERIOD=0
  INTEGER :: PE_NBATCH=0
  INTEGER :: PE_SCALING=1
  INTEGER :: PE_KTYPE=1
  REAL,DIMENSION(2) :: PE_TARGET=(/1.0,0.0/)
  REAL :: PE_STOP=10.0
  REAL :: PE_SENS=0.5
  REAL :: PE_PADJ=0.05
  REAL :: PE_DRES=0.01
  INTEGER,DIMENSION(:),POINTER :: S_PERIOD,S_PERIOD_BU,E_PERIOD,E_PERIOD_BU => NULL()
  REAL,DIMENSION(:),POINTER :: B_FRACTION,B_FRACTION_BU => NULL()
  CHARACTER(LEN=256),DIMENSION(:),POINTER :: B_BATCHFILE,B_BATCHFILE_BU,B_OUTFILE,B_OUTFILE_BU => NULL()
  TYPE(PSTPARAMOBJ),POINTER,DIMENSION(:) :: PARAM,PARAM_BU => NULL()
  CHARACTER(LEN=256),POINTER,DIMENSION(:) :: IDFFILES,IDFFILES_BU => NULL()  !## stores the idf files for zones
 END TYPE PSTOBJ

 TYPE PSTPARAMOBJ
  INTEGER :: PACT=1
  INTEGER :: PILS=1
  INTEGER :: PIZONE=1
  INTEGER :: PIGROUP=1
  INTEGER :: PLOG=1
  INTEGER :: IPARAM=1
  CHARACTER(LEN=3) :: PPARAM
  REAL :: PINI=1.0
  REAL :: PDELTA=1.1
  REAL :: PMIN=0.01
  REAL :: PMAX=100.0
  REAL :: PINCREASE=2.0
 END TYPE PSTPARAMOBJ

 TYPE(PSTOBJ) :: PEST
 
 TYPE TOPICSOBJ
  INTEGER :: ID                                                 !id of main topics
  INTEGER,POINTER,DIMENSION(:) :: IDT                           !id of each time step for current topic
  INTEGER,POINTER,DIMENSION(:,:) :: ISD                         !id of main subtopics for each timestep
  INTEGER :: IACT                                               !active in projectmanager
  INTEGER :: IACT_MODEL                                         !active in runfile/model
  CHARACTER(LEN=MAXLEN) :: TNAME                                !name of topic
  CHARACTER(LEN=MAXLEN),DIMENSION(MAXSUBTOPICS) :: SNAME        !name of subtopics
  INTEGER :: NSUBTOPICS                                         !number of subtopics
  LOGICAL :: TIMDEP                                             !timedependent module/package
  TYPE(STRESSOBJ),POINTER,DIMENSION(:) :: STRESS,STRESS_TMP => NULL()     !files 
 END TYPE TOPICSOBJ

 TYPE(TOPICSOBJ),DIMENSION(MAXTOPICS) :: TOPICS

 CHARACTER(LEN=3),DIMENSION(MAXTOPICS) :: CMOD
 CHARACTER(LEN=MAXLEN),ALLOCATABLE,DIMENSION(:) :: MENUNAMES
 
 DATA CMOD/'MSP','TOP','BOT','BND','SHD','KDW','KHV','KVA','VCW','KVV', & ! 1-10
           'STO','SSC','PWT','ANI','HFB','IBS','SFT','CPP','CON','PST', & !11-20
           'WEL','DRN','RIV','EVT','GHB','RCH','OLF','CHD','ISG'/         !21-29
 
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IDT
   
 TYPE PRJOBJ
  INTEGER :: ILAY,ICNST,IACT
  REAL :: FCT,IMP,CNST
  CHARACTER(LEN=256) :: FNAME
 END TYPE PRJOBJ
 TYPE(PRJOBJ),ALLOCATABLE,DIMENSION(:) :: PRJ

 INTEGER :: NLAY,MXNLAY,IUNCONF,IFORMAT,ISUBMODEL,NPER
 INTEGER,DIMENSION(4) :: IFULL
 LOGICAL :: LBCF,LLPF,LPCG,LPCGN,LSIP,LRCH,LEVT,LDRN,LRIV,LGHB,LOLF,LCHD,LWEL,LISG,LPWT,LHFB,LMSP,LQBD

 INTEGER,SAVE :: MXITER=500
 INTEGER,SAVE :: ITER1=20
 INTEGER,SAVE :: IIDEBUG=0
 REAL,SAVE :: HCLOSE=0.001
 REAL,SAVE :: RCLOSE=0.1
 REAL,SAVE :: RELAX=0.98
 INTEGER,SAVE :: NPCOND=1
 REAL,SAVE :: MINTHICKNESS=0.01
 CHARACTER(LEN=52),SAVE :: MODELNAME='Model'
 CHARACTER(LEN=4),DIMENSION(0:1) :: FEXT
 DATA FEXT/'.ARR','.ASC'/
 REAL,DIMENSION(5) :: SUBMODEL
 
 CHARACTER(LEN=8),DIMENSION(9 ) :: TMENU1
 CHARACTER(LEN=8),DIMENSION(10) :: TMENU2
 DATA TMENU1/'Hourly  ','Daily   ','Weekly  ','Decade  ', &
             '14/28   ','Monthly ','Yearly  ','Packages','Custom  '/
 DATA TMENU2/'Hourly  ','Daily   ','Weekly  ','Decade  ', &
             '14/28   ','Monthly ','Yearly  ','Packages','Inherit','Custom  '/
 
END MODULE MOD_PMANAGER_PAR