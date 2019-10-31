!!  Copyright (C) Stichting Deltares, 2005-2019.
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
 
 USE MOD_IDF_PAR, ONLY : IDFOBJ
 USE IMODVAR, ONLY : DP_KIND,SP_KIND
 
 INTEGER,PARAMETER :: MAXTOPICS=42
 INTEGER,PARAMETER :: TBND= 1
 INTEGER,PARAMETER :: TTOP= 2
 INTEGER,PARAMETER :: TBOT= 3
 INTEGER,PARAMETER :: TKHV= 4
 INTEGER,PARAMETER :: TKVA= 5
 INTEGER,PARAMETER :: TKDW= 6
 INTEGER,PARAMETER :: TKVV= 7
 INTEGER,PARAMETER :: TVCW= 8
 INTEGER,PARAMETER :: TSHD= 9
 INTEGER,PARAMETER :: TSTO=10
 INTEGER,PARAMETER :: TSPY=11
 INTEGER,PARAMETER :: TPOR=12
 INTEGER,PARAMETER :: TANI=13
 INTEGER,PARAMETER :: THFB=14
 INTEGER,PARAMETER :: TIBS=15
 INTEGER,PARAMETER :: TPWT=16
 INTEGER,PARAMETER :: TSFT=17
 INTEGER,PARAMETER :: TCAP=18
 INTEGER,PARAMETER :: TUZF=19
 INTEGER,PARAMETER :: TRCH=20
 INTEGER,PARAMETER :: TEVT=21
 INTEGER,PARAMETER :: TDRN=22
 INTEGER,PARAMETER :: TOLF=23
 INTEGER,PARAMETER :: TRIV=24
 INTEGER,PARAMETER :: TISG=25
 INTEGER,PARAMETER :: TSFR=26
 INTEGER,PARAMETER :: TLAK=27
 INTEGER,PARAMETER :: TWEL=28
 INTEGER,PARAMETER :: TMNW=29
 INTEGER,PARAMETER :: TGHB=30
 INTEGER,PARAMETER :: TCHD=31
 INTEGER,PARAMETER :: TFHB=32
 INTEGER,PARAMETER :: TPST=33
 INTEGER,PARAMETER :: TOBS=34
 INTEGER,PARAMETER :: TPCG=35
 INTEGER,PARAMETER :: TGCG=36 !## mt3d/seawat
 INTEGER,PARAMETER :: TBTN=37 !## mt3d/seawat
 INTEGER,PARAMETER :: TADV=38 !## mt3d/seawat
 INTEGER,PARAMETER :: TDSP=39 !## mt3d/seawat
 INTEGER,PARAMETER :: TRCT=40 !## mt3d/seawat
 INTEGER,PARAMETER :: TUDR=41 !## mt3d/seawat
 INTEGER,PARAMETER :: TVDF=42 !## mt3d/seawat
 
 INTEGER,PARAMETER :: MAXLENPRJ   =52
 INTEGER,PARAMETER :: MAXSUBTOPICS=24
 INTEGER,PARAMETER :: MAXPERIODS  =10
 INTEGER,PARAMETER :: MAXSPECIES  =10
 INTEGER,PARAMETER :: MAXPARAM    =27
 REAL(KIND=DP_KIND),PARAMETER :: HNOFLOW=HUGE(1.0)
 
 INTEGER :: ITABVIEW
 
 REAL(KIND=DP_KIND),PARAMETER :: COLF=1.0D0     !## resistance of overland flow package
 REAL(KIND=DP_KIND),PARAMETER :: MINKH=0.0D0    !## minimal k-value to assign wells to modellayers

 TYPE LAYOBJ
  INTEGER,DIMENSION(:),POINTER :: ILAY
 END TYPE LAYOBJ
 
 ! parameters for iMOD WQ
 TYPE WQOBJ
  INTEGER :: ISS
 END TYPE WQOBJ
     
 TYPE PMANBATCH
  INTEGER(KIND=8) :: SDATE,EDATE
  INTEGER :: ITT,IDT,ISS,IFORMAT,IWINDOW,ISOLVE,NSTEP,SSYSTEM,IPEST,IPESTP,ISTEADY,ICONCHK,ISAVEENDDATE,IFVDL, &
   IPKS,ICHKCHD,IDOUBLE,ICONSISTENCY, &
   DWEL,DISG,DSFR,DMMFILE,NSUBMODEL,ISUBMODEL,NLINESEARCH,NCPU,CMDHIDE,PTEST,NSWAIT,PDEBUG,MC,IGENMF6
  REAL(KIND=DP_KIND) :: XMIN,YMIN,XMAX,YMAX,CELLSIZE,BUFFER,NMULT,MINKD,MINC,BUFFERCS,MINTHICKNESS,EIGV
  !## isave options
  TYPE(LAYOBJ),DIMENSION(MAXTOPICS) :: ISAVE 
  !## interpolation options
  INTEGER,DIMENSION(MAXTOPICS) :: INT
  INTEGER,POINTER,DIMENSION(:) :: UNCONFINED
  INTEGER,POINTER,DIMENSION(:) :: ILAY
  CHARACTER(LEN=256) :: TIMFNAME,MODFLOW,BNDFILE,OUTPUT,GENFNAME
  CHARACTER(LEN=6) :: RUNTYPE
  TYPE(WQOBJ) :: WQ     ! parameters for iMOD WQ
 END TYPE PMANBATCH
 TYPE(PMANBATCH) :: PBMAN
 
 TYPE PERIODOBJ
  CHARACTER(LEN=MAXLENPRJ) :: NAME
  INTEGER :: IYR,IDY,IMH,IHR,IMT,ISC
 END TYPE PERIODOBJ
 TYPE(PERIODOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: PERIOD
 INTEGER :: NPERIOD
 
 TYPE SPECIESOBJ
  CHARACTER(LEN=MAXLENPRJ) :: NAME
 END TYPE SPECIESOBJ
 TYPE(SPECIESOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: SPECIES
 INTEGER :: NSPECIES

 !## For each simulation step date-time info and MODFLOW flags
 TYPE SIMOBJ
  INTEGER :: ISAVE
  CHARACTER(LEN=MAXLENPRJ) :: CDATE
  REAL(KIND=DP_KIND) :: DELT
  INTEGER :: DDAY,DSEC
  INTEGER :: ISUM
  INTEGER :: NSTP  !## number of timesteps
  REAL(KIND=DP_KIND) :: TMULT !## multiplication of timesteps
  INTEGER :: IDY,IMH,IYR
  INTEGER :: IHR,IMT,ISC
 END TYPE SIMOBJ
 TYPE(SIMOBJ),POINTER,DIMENSION(:) :: SIM,SIM_C,SIM_C2
  
 TYPE FILESOBJ
  INTEGER :: IACT                                  !## active in runfile
  CHARACTER(LEN=256) :: FNAME                      !## name of current file, could be a constant too!
  CHARACTER(LEN=MAXLENPRJ) :: ALIAS                !## alias name of current file, could be a constant too!
  INTEGER :: ID                                    !## id in treeview field
  INTEGER :: ILAY                                  !## layer of current file
  INTEGER :: ICNST                                 !## to be constant (1) or an idf file (2), or inherent (0)
  REAL(KIND=DP_KIND) :: FCT                        !## factor
  REAL(KIND=DP_KIND) :: IMP                        !## impulse
  REAL(KIND=DP_KIND) :: CNST                       !## constant value
 END TYPE FILESOBJ

 TYPE STRESSOBJ
  CHARACTER(LEN=MAXLENPRJ) :: CDATE                             !date string of each time step
  INTEGER :: IYR,IMH,IDY                                        !years,months ,days
  INTEGER :: IHR,IMT,ISC                                        !hours,minutes,seconds
  TYPE(FILESOBJ),POINTER,DIMENSION(:,:) :: FILES,FILES_TMP => NULL()     !stress information for current defined timestep
                                                                !(I,:) = subtopic i
                                                                !(:,I) = system i
  CHARACTER(LEN=256),POINTER,DIMENSION(:) :: INPFILES => NULL()  !stores the inp files for metaswap
 END TYPE STRESSOBJ
 TYPE(STRESSOBJ),ALLOCATABLE,DIMENSION(:) :: STRESS  !## only in PMANAGER_GETIPER, temp to fill TOPICS(ITOPIC)%STRESS
 
 TYPE SIPOBJ
  INTEGER :: NOUTER=150
  REAL(KIND=DP_KIND) :: HCLOSE=1.0D-4
  REAL(KIND=DP_KIND) :: RELAX=0.98D0
 END TYPE SIPOBJ
 TYPE(SIPOBJ) :: SIP

 TYPE PCGOBJ
  INTEGER :: NOUTER=150
  INTEGER :: NINNER=30
  INTEGER :: NPCOND=1
  INTEGER :: IPRPCG=1
  INTEGER :: MUTPCG=1
  REAL(KIND=DP_KIND) :: HCLOSE=1.0D-4
  REAL(KIND=DP_KIND) :: RCLOSE=1.0D-1
  REAL(KIND=DP_KIND) :: RELAX=0.98D0
  REAL(KIND=DP_KIND) :: QERROR=0.1D0
  INTEGER :: IQERROR=0
  REAL(KIND=DP_KIND) :: DAMPPCG=1.0D0
  REAL(KIND=DP_KIND) :: DAMPPCGT=1.0D0
  INTEGER :: IMERGE=0
  INTEGER :: PARTOPT=0
  INTEGER :: NCORES=1
  CHARACTER(LEN=256) :: MRGFNAME=''
 END TYPE PCGOBJ
 TYPE(PCGOBJ) :: PCG

 !## mt3d/seawat
 TYPE BTNOBJ
  INTEGER :: MUT=1,NCOMP, MCOMP, MIXELM
  REAL(KIND=DP_KIND) :: HCLOSE=1.0D-4, THKMIN, CINACT
 END TYPE BTNOBJ
 TYPE(BTNOBJ) :: BTN

 TYPE GCGOBJ
  INTEGER :: MUT=1
  REAL(KIND=DP_KIND) :: HCLOSE=1.0D-4
 END TYPE GCGOBJ
 TYPE(GCGOBJ) :: GCG
 
 TYPE RCTOBJ
  INTEGER :: MUT=1
  REAL(KIND=DP_KIND) :: HCLOSE=1.0D-4
 END TYPE RCTOBJ
 TYPE(RCTOBJ) :: RCT
  
 TYPE ADVOBJ
  INTEGER :: MUT=1
  REAL(KIND=DP_KIND) :: HCLOSE=1.0D-4
 END TYPE ADVOBJ
 TYPE(ADVOBJ) :: ADV
  
 TYPE VDFOBJ
  INTEGER :: MUT=1
  REAL(KIND=DP_KIND) :: HCLOSE=1.0D-4
 END TYPE VDFOBJ
 TYPE(VDFOBJ) :: VDF
  
 !## pest settings
 TYPE PSTMEASURE
  CHARACTER(LEN=256) :: IPFNAME
  INTEGER :: IPFTYPE,IXCOL,IYCOL,ILCOL,IMCOL,IVCOL
 END TYPE PSTMEASURE

 TYPE PSTPARAMOBJ
  INTEGER :: PACT=1
  INTEGER :: PILS=1
  INTEGER :: PIZONE=1
  INTEGER :: PIGROUP=1
  INTEGER :: PLOG=1
  INTEGER :: CONVERTLOG=0
  INTEGER :: IPARAM=1
  INTEGER :: IBND
  CHARACTER(LEN=3) :: PPARAM
  CHARACTER(LEN=15) :: ACRONYM
  REAL(KIND=DP_KIND) :: PINI=1.0D0
  REAL(KIND=DP_KIND) :: PPRIOR=1.0D0
  REAL(KIND=DP_KIND),DIMENSION(2) :: ALPHA=1.0D0
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: ALPHA_HISTORY
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: ALPHA_ERROR_VARIANCE
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: GALPHA     !## collection of alphas per gradient simulation
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: LALPHA     !## collection of alphas per linesearch simulation
  REAL(KIND=DP_KIND) :: PDELTA=1.1D0
  REAL(KIND=DP_KIND) :: PMIN=0.01D0
  REAL(KIND=DP_KIND) :: PMAX=100.0D0
  REAL(KIND=DP_KIND) :: PINCREASE=2.0D0
 END TYPE PSTPARAMOBJ

 TYPE PSTOBJ
  INTEGER :: PE_MXITER=10
  INTEGER :: PE_SCALING=1
  INTEGER :: PE_KTYPE=1
  INTEGER :: PE_REGULARISATION=1
  REAL(KIND=DP_KIND),DIMENSION(2) :: PE_TARGET=(/1.0D0,0.0D0/)
  REAL(KIND=DP_KIND) :: PE_STOP=0.0D0
  REAL(KIND=DP_KIND) :: PE_SENS=0.0D0
  REAL(KIND=DP_KIND) :: PE_PADJ=0.0D0
  REAL(KIND=DP_KIND) :: PE_DRES=0.0D0
  REAL(KIND=DP_KIND) :: PE_KRANGE=0.0D0
  REAL(KIND=DP_KIND) :: PE_REGFACTOR=1.0D0
  TYPE(PSTMEASURE),POINTER,DIMENSION(:) :: MEASURES,MEASURES_BU
  INTEGER :: IIPF
  CHARACTER(LEN=14),DIMENSION(:),POINTER :: S_PERIOD,S_PERIOD_BU,E_PERIOD,E_PERIOD_BU => NULL()
  REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: B_FRACTION,B_FRACTION_BU => NULL()
  CHARACTER(LEN=256) :: PPBNDIDF
  CHARACTER(LEN=256),DIMENSION(:),POINTER :: B_BATCHFILE,B_BATCHFILE_BU,B_OUTFILE,B_OUTFILE_BU => NULL()
  TYPE(PSTPARAMOBJ),POINTER,DIMENSION(:) :: PARAM,PARAM_BU => NULL()
  CHARACTER(LEN=256),POINTER,DIMENSION(:) :: IDFFILES,IDFFILES_BU => NULL()  !## stores the idf files for zones
 END TYPE PSTOBJ

 TYPE(PSTOBJ) :: PEST
 
 !## Complete set of all files in time for all packages
 TYPE TOPICSOBJ
  INTEGER :: ID                                                 !id of main topics
  INTEGER,POINTER,DIMENSION(:) :: IDT                           !id of each time step for current topic
  INTEGER,POINTER,DIMENSION(:,:) :: ISD                         !id of main subtopics for each timestep
  INTEGER :: IACT                                               !active in projectmanager (only 1 at the same time)
  INTEGER :: IACT_MODEL                                         !active in runfile/model 
  CHARACTER(LEN=MAXLENPRJ) :: TNAME                             !name of topic
  CHARACTER(LEN=MAXLENPRJ),DIMENSION(MAXSUBTOPICS) :: SNAME     !name of subtopics
  INTEGER :: NSUBTOPICS                                         !number of subtopics
  LOGICAL :: TIMDEP                                             !timedependent module/package
  LOGICAL :: LSPECIES                                           !whether species can be defined
  CHARACTER(LEN=3) :: CMOD                                      !short name (3 characters) of the topic
  TYPE(STRESSOBJ),POINTER,DIMENSION(:) :: STRESS,STRESS_TMP => NULL()     !files 
 END TYPE TOPICSOBJ
 TYPE(TOPICSOBJ),DIMENSION(MAXTOPICS) :: TOPICS

 CHARACTER(LEN=MAXLENPRJ),ALLOCATABLE,DIMENSION(:) :: MENUNAMES
 CHARACTER(LEN=3),DIMENSION(MAXPARAM) :: PARAM
 DATA PARAM/'KD','KH','KV','VC','SC','RC','RI','DC','IC','II','AF','AA','VA','HF','MS','MC','RE','EX','EP', &
    'QR','GC','SY','RL','RB','IL','IB','DL'/

 INTEGER,ALLOCATABLE,DIMENSION(:) :: IDT,LAYCON
   
 TYPE PRJOBJ
  INTEGER :: ILAY,ICNST,IACT,INHERENT
  REAL(KIND=DP_KIND) :: FCT,IMP,CNST
  CHARACTER(LEN=256) :: FNAME
 END TYPE PRJOBJ
 TYPE(PRJOBJ),POINTER,DIMENSION(:) :: PRJ,PRJ_TMP

 INTEGER :: PRJNLAY,PRJMXNLAY,ISUBMODEL,PRJNPER 
 INTEGER,DIMENSION(4) :: IFULL
 LOGICAL :: LBCF,LLPF,LNPF,LPCG,LRCH,LEVT,LDRN,LRIV,LGHB,LOLF, &   ! Logical for all (composed) packages indicating if the package COMPLETE/Active
            LCHD,LWEL,LISG,LPWT,LHFB,LMSP,LQBD,LSFR,LFHB,LLAK, &   ! composed is based on items in MAXTOPICS list e.g. LBCF checks for existing TKDW and TVCW
            LMNW,LUZF,LPST,LANI,LSFT,LPKS,LBAS, &
            LGCG,LRCT,LADV,LVDF,LBTN,LDIS,LDSP,LSSM,LUDR,LFTL ! added for iMOD WQ
 INTEGER :: IHEDUN,IBCFCB,IRCHCB,IEVTCB,IDRNCB,IRIVCB,IGHBCB,ICHDCB,IWELCB,ISFRCB,IFHBCB,ISFRCB2,IFHBUN,ILAKCB,IUZFCB1,IWL2CB
 
 REAL(KIND=DP_KIND),DIMENSION(:,:),ALLOCATABLE :: FHBHED,FHBFLW
 REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: FHBNBDTIM
 
 TYPE(IDFOBJ) :: PRJIDF
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: BND,TOP,BOT,SHD,LAK,KHV,KVV,KVA,STO,SPY,KDW,VCW,LBD,LCD,PCK,ANA,ANF,SFT

 INTEGER,DIMENSION(:),ALLOCATABLE :: ULAKES,DULAKES,NP_IPER
 INTEGER :: NLAKES

 CHARACTER(LEN=52),SAVE :: MODELNAME='Model'
 REAL(KIND=DP_KIND),DIMENSION(7) :: SUBMODEL
 
 CHARACTER(LEN=8),DIMENSION(9) :: TMENU1
 CHARACTER(LEN=8),DIMENSION(10) :: TMENU2
 DATA TMENU1/'Minutes ','Hourly  ','Daily   ','Weekly  ','Decade  ', &
             '14/28   ','Monthly ','Yearly  ','Packages'/ !,'Custom  '/
 DATA TMENU2/'Minutes ','Hourly  ','Daily   ','Weekly  ','Decade  ', &
             '14/28   ','Monthly ','Yearly  ','Packages','All     '/
 
   INTEGER,ALLOCATABLE,DIMENSION(:) :: PRJILIST
 TYPE(PRJOBJ),POINTER,DIMENSION(:) :: FNAMES,FNAMES_BU
 
 INTEGER :: NMAXCORES
 
 TYPE SIMGRO_OBJ
  INTEGER :: IBOUND   !boundary condition
  INTEGER :: LGN      !landuse
  INTEGER :: METEO    !meteo-station
  INTEGER :: BER_LAAG !artificial recharge layer
  INTEGER :: BEREGEN  !artificial recharge
  INTEGER :: BODEM    !soil type
  REAL(KIND=DP_KIND) :: BEREGEN_Q  !artificial recharge strength
  REAL(KIND=DP_KIND) :: NOPP     !wetted-surface
  REAL(KIND=DP_KIND) :: SOPP     !urban-surface
  REAL(KIND=DP_KIND) :: RZ       !rootzone
  REAL(KIND=DP_KIND) :: MV       !surface-level
  REAL(KIND=DP_KIND) :: PWT_LEVEL !level for PWT (optional)
  REAL(KIND=DP_KIND) :: COND      !conductivity
  REAL(KIND=DP_KIND) :: MOISTURE  !moisture
  REAL(KIND=DP_KIND) :: VXMU_SOPP !micro-storage capacity, sill of the runoff relationship
  REAL(KIND=DP_KIND) :: VXMU_ROPP !micro-storage capacity, sill of the runoff relationship
  REAL(KIND=DP_KIND) :: CRUNOFF_SOPP !runoff resistance (days)
  REAL(KIND=DP_KIND) :: CRUNOFF_ROPP !runoff resistance (days)
  REAL(KIND=DP_KIND) :: CRUNON_SOPP !runon resistance (days)
  REAL(KIND=DP_KIND) :: CRUNON_ROPP !runon resistance (days)
  REAL(KIND=DP_KIND) :: QINFBASIC_SOPP !infiltratie cap.
  REAL(KIND=DP_KIND) :: QINFBASIC_ROPP
 END TYPE SIMGRO_OBJ
 
 TYPE(SIMGRO_OBJ),ALLOCATABLE,DIMENSION(:,:) :: SIMGRO         

 INTEGER :: INDSB     !unit number for svat2swnr_roff.inp
 INTEGER :: IAREA     !unit number for area_msw.inp
 INTEGER :: ISELSVAT  !unit number for sel_svat_bda.inp
 INTEGER :: ISCAP     !unit number for scap_msw.inp
 INTEGER :: IGWMP     !unit number for gwmp_msw.inp
 INTEGER :: IMODSIM   !unit number for mod-sim.txt
 INTEGER :: IDXC      !unit number for modflow.dxc
 INTEGER :: IINFI     !unit number for infi_svat.inp
 INTEGER :: IIDF      !unit number for idf_svat.inp
 INTEGER :: IUSCL   
 INTEGER :: IDFM_MSWP
 INTEGER :: IMSWP_DFM
 INTEGER :: IARMWP

 LOGICAL,PARAMETER :: LFREEFORMAT=.TRUE.  !## use true free-format
 CHARACTER(LEN=1024) :: LINE
 LOGICAL :: LYESNO

 TYPE RUNFILEWQ_OBJ  ! For re-use of parameter information in iMOD WQ input files 
  CHARACTER(LEN=256) :: DELR_C, DELR_R, LAYCON_L
  INTEGER :: NLAY
 END TYPE RUNFILEWQ_OBJ
 
 TYPE(RUNFILEWQ_OBJ) :: WQFILE         

END MODULE MOD_PMANAGER_PAR