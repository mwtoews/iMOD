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

 CHARACTER(LEN=24),DIMENSION(5) :: CSOLVER=['MODFLOW2005-RUN',',MODFLOW2005-NAM','MODFLOW6','IMOD-WQ-SEAWAT','IMOD-WQ-MT3D']

 INTEGER,PARAMETER :: MAXTOPICS=53
 INTEGER,PARAMETER :: TBND= 1
 INTEGER,PARAMETER :: TTOP= 2
 INTEGER,PARAMETER :: TBOT= 3
 INTEGER,PARAMETER :: TTHK= 4
 INTEGER,PARAMETER :: TKHV= 5
 INTEGER,PARAMETER :: TKVA= 6
 INTEGER,PARAMETER :: TKDW= 7
 INTEGER,PARAMETER :: TKVV= 8
 INTEGER,PARAMETER :: TVCW= 9
 INTEGER,PARAMETER :: TSHD=10
 INTEGER,PARAMETER :: TSTO=11
 INTEGER,PARAMETER :: TSPY=12
 INTEGER,PARAMETER :: TPOR=13
 INTEGER,PARAMETER :: TANI=14
 INTEGER,PARAMETER :: THFB=15
 INTEGER,PARAMETER :: TIBS=16
 INTEGER,PARAMETER :: TPWT=17
 INTEGER,PARAMETER :: TSFT=18
 INTEGER,PARAMETER :: TCAP=19
 INTEGER,PARAMETER :: TUZF=20
 INTEGER,PARAMETER :: TRCH=21
 INTEGER,PARAMETER :: TEVT=22
 INTEGER,PARAMETER :: TDRN=23
 INTEGER,PARAMETER :: TOLF=24
 INTEGER,PARAMETER :: TRIV=25
 INTEGER,PARAMETER :: TISG=26
 INTEGER,PARAMETER :: TSFR=27
 INTEGER,PARAMETER :: TLAK=28
 INTEGER,PARAMETER :: TWEL=29
 INTEGER,PARAMETER :: TMNW=30
 INTEGER,PARAMETER :: TGHB=31
 INTEGER,PARAMETER :: TCHD=32
 INTEGER,PARAMETER :: TFHB=33
 INTEGER,PARAMETER :: TPST=34
 INTEGER,PARAMETER :: TIES=35 !## iterative ensemble smoother
 INTEGER,PARAMETER :: TOBS=36
 INTEGER,PARAMETER :: TPCG=37
 INTEGER,PARAMETER :: TGCG=38 !## mt3d/seawat
 INTEGER,PARAMETER :: TVDF=39 !## mt3d/seawat - parameters vdf
 INTEGER,PARAMETER :: TFDE=40 !## mt3d/seawat - fluid density
 INTEGER,PARAMETER :: TCBI=41 !## mt3d/seawat - concentration boundary indicator
 INTEGER,PARAMETER :: TSCO=42 !## mt3d/seawat - start concentration per layer and species
 INTEGER,PARAMETER :: TDSP=43 !## mt3d/seawat - dispersiviteit
 INTEGER,PARAMETER :: TTVC=44 !## mt3d/seawat - time varying concentration
 INTEGER,PARAMETER :: THOB=45 !## mt3d/seawat - bulk density of the porous medium
 INTEGER,PARAMETER :: TPID=46 !## mt3d/seawat - porosity of the immobile domain
 INTEGER,PARAMETER :: TICS=47 !## mt3d/seawat - initial concentration of the sorbed or immobile liquid phase
 INTEGER,PARAMETER :: TFSC=48 !## mt3d/seawat - first sorption constant
 INTEGER,PARAMETER :: TSSC=49 !## mt3d/seawat - second sorption constant
 INTEGER,PARAMETER :: TFOD=50 !## mt3d/seawat - first order rate reaction for the dissolved phase
 INTEGER,PARAMETER :: TFOS=51 !## mt3d/seawat - second order rate reaction for the sorbed phase
 INTEGER,PARAMETER :: TRCT=52 !## mt3d/seawat - rct
 INTEGER,PARAMETER :: TCON=53 !## modflow2005 - concentration
 
 TYPE MTOBJ
  INTEGER,POINTER,DIMENSION(:) :: T=>NULL()     !## fixed: packages available for selected Model
  INTEGER,POINTER,DIMENSION(:) :: IACT=>NULL()  !## variable: packages (de)activated by the user 
  CHARACTER(LEN=24) :: MCNAME           !## model configuration name
 END TYPE MTOBJ
 TYPE(MTOBJ),DIMENSION(6) :: MC     ! Model Configuration - list of topics for each Model (modflow2005, modflow6,seawat,mt3d,modpath)
 
 INTEGER,PARAMETER :: MAXLENPRJ   =52
 INTEGER,PARAMETER :: MAXSUBTOPICS=24
 INTEGER,PARAMETER :: MAXPERIODS  =10
 INTEGER,PARAMETER :: MAXSPECIES  =10
 INTEGER,PARAMETER :: MAXPARAM    =28
 REAL(KIND=DP_KIND),PARAMETER :: HNOFLOW=HUGE(1.0)
 
 REAL(KIND=DP_KIND),PARAMETER :: COLF=1.0D0     !## resistance of overland flow package
 REAL(KIND=DP_KIND),PARAMETER :: MINKH=0.0D0    !## minimal k-value to assign wells to modellayers

 TYPE LAYOBJ
  INTEGER,DIMENSION(:),POINTER :: ILAY=>NULL()
 END TYPE LAYOBJ

 !## parameter for BTN
 TYPE BTNOBJ
  CHARACTER(LEN=1) :: TUNIT='D'   !## fixed
  CHARACTER(LEN=1) :: LUNIT='M'   !## fixed
  CHARACTER(LEN=1) :: MUNIT='K'   !## fixed
  INTEGER :: IFMTCN=0   !## fixed
  INTEGER :: IFMTNP=0   !## fixed
  INTEGER :: IFMTRF=0   !## fixed
  INTEGER :: IFMTDP=0   !## fixed
  INTEGER :: NPRMAS=1   !## fixed
  INTEGER :: NPROBS=1
  LOGICAL :: SAVUCN=.TRUE. !## fixed
  LOGICAL :: CHKMAS=.TRUE. !## fixed
  REAL(KIND=DP_KIND) :: CINACT=-9999.0D0  !## fixed
  REAL(KIND=DP_KIND) :: THKMIN=1.0D-02    !## fixed
 END TYPE BTNOBJ
 
 !## parameter for adv
 TYPE ADVOBJ
  INTEGER :: MIXELM=-1  !## from a menu, start -1
  INTEGER :: MXPART=0   !## fixed
  INTEGER :: NADVFD=0   !## from a menu, start 0
  !## hmoc/moc/mmoc options
  INTEGER :: ITRACK   !## fixed
  INTEGER :: NPLANE   !## fixed
  INTEGER :: NPL      !## fixed
  INTEGER :: NPH      !## fixed
  INTEGER :: NPMIN    !## fixed
  INTEGER :: NPMAX    !## fixed
  INTEGER :: INTERP   !## fixed
  INTEGER :: NLSINK   !## fixed
  INTEGER :: NPSINK   !## fixed
  REAL(KIND=DP_KIND) :: DCHMOC !## fixed
  REAL(KIND=DP_KIND) :: WD     !## fixed
  REAL(KIND=DP_KIND) :: DCEPS  !## fixed
  REAL(KIND=DP_KIND) :: PERCEL=1.0D0 
 END TYPE ADVOBJ
 
 !## parameter for ssm
 TYPE SSMOBJ
  INTEGER :: MXSS
 END TYPE SSMOBJ

 !## parameter for vdf
 TYPE VDFOBJ
  INTEGER :: MTDNCONC=1   !## fixed
  INTEGER :: MFNADVFD=1   !## fixed
  INTEGER :: NSWTCPL=1    !## fixed
  INTEGER :: IWTABLE=0    !## fixed
  INTEGER :: INDENSE_P=1  !## fixed
  REAL(KIND=DP_KIND) :: DENSEMIN=0.0D0
  REAL(KIND=DP_KIND) :: DENSEMAX=0.0D0
  REAL(KIND=DP_KIND) :: DNSCRIT=0.0D0    !## fixed
  REAL(KIND=DP_KIND) :: DENSEREF=1000.0D0
  REAL(KIND=DP_KIND) :: DENSESLP=0.7143D0
  REAL(KIND=DP_KIND) :: FIRSTDT=1.0D-03  !## fixed
 END TYPE VDFOBJ

 !## parameter for gcg
 TYPE GCGOBJ
  INTEGER :: MXITER=1000
  INTEGER :: ITER1=30
  INTEGER :: ISOLVE=3    !## from a menu, start 1
  INTEGER :: NCRS=0      !## from a menu, start 0
  INTEGER :: IPRGCG=0
  REAL(KIND=DP_KIND) :: ACCL=1.0D0
  REAL(KIND=DP_KIND) :: CCLOSE=1.0D-06
 END TYPE GCGOBJ

 !## parameter for rct
 TYPE RCTOBJ
  INTEGER :: ISOTHM=1   !## from a menu, start 1
  INTEGER :: IREACT=1   !## from a menu, start 0
  INTEGER :: IGETSC=0
  INTEGER :: IRCTOP=2 !# fixed
 END TYPE RCTOBJ
 CHARACTER(LEN=47),DIMENSION(7) :: ISOTHM_STR=['0 (non)', &
       '1 (linear)', &
       '2 (Freundlich)', &
       '3 (Langmuir)', &
       '4 (onequilibrium)', &
       '5 (dual-domain mass transfer, without sorption)', &
       '6 (dual-domain mass transfer, with sorption)']

 !## parameters for iMOD WQ from projectfile
 TYPE WQOBJ
  TYPE(VDFOBJ) :: VDF
  TYPE(GCGOBJ) :: GCG
  TYPE(RCTOBJ) :: RCT
 END TYPE WQOBJ
 TYPE(WQOBJ) :: WQ     !## parameters for iMOD WQ. Given with the Projectmanager or read from the PRJ file 

 TYPE SM_OBJ
  INTEGER,POINTER,DIMENSION(:) :: ILAY=>NULL()  !## number of layers per submodel
  TYPE(IDFOBJ),POINTER,DIMENSION(:) :: IDF=>NULL()
  TYPE(IDFOBJ),POINTER,DIMENSION(:) :: CON=>NULL()
 END TYPE SM_OBJ
 
  !## from imodbatch ini file
 TYPE PMANBATCH
  INTEGER(KIND=DP_KIND) :: SDATE,EDATE
  INTEGER :: ITT,IDT,ISS,IFORMAT,IWINDOW,ISOLVE,NSTEP,SSYSTEM,IPEST,IPESTP,ISTEADY,ICONCHK,ISAVEENDDATE,IFVDL, &
             IPKS,ICHKCHD,IDOUBLE,ICONSISTENCY,IIES, &
             DWEL,DISG,DSFR,DMMFILE,NSUBMODEL,ISUBMODEL,NLINESEARCH,NCPU,CMDHIDE,NSWAIT,IGENMF6
  REAL(KIND=DP_KIND) :: XMIN,YMIN,XMAX,YMAX,CELLSIZE,BUFFER,NMULT,MINKD,MINC,BUFFERCS,MINTHICKNESS,EIGV
  INTEGER :: SMTYPE
  !## Seawat/mt3d
  TYPE(BTNOBJ) :: BTN
  TYPE(ADVOBJ) :: ADV
  TYPE(SSMOBJ) :: SSM
  
  !## isave options
  TYPE(LAYOBJ),DIMENSION(MAXTOPICS) :: ISAVE 
  !## interpolation options
  INTEGER,DIMENSION(MAXTOPICS) :: INT
  INTEGER,POINTER,DIMENSION(:) :: UNCONFINED=>NULL()
  INTEGER,POINTER,DIMENSION(:) :: ILAY=>NULL()       !## number of layer for current submodel in the pre-processing
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: LAMBDA_TEST=>NULL()
  TYPE(SM_OBJ),POINTER,DIMENSION(:) :: SM=>NULL()
  CHARACTER(LEN=256) :: TIMFNAME,MODFLOW,MODFLOW6,IMOD_WQ,BNDFILE,GENFNAME,RUNFILE,PRJFILE
  CHARACTER(LEN=256) :: MODELNAME     ! name of model without root
  CHARACTER(LEN=256) :: OUTPUT        ! Resultdir full name, e.g. ...\IMOD_USER\MODELS\MODELNAME 
  CHARACTER(LEN=256) :: FLOW_RESULT_DIR ! for MT3D, location of the resultfolder containing a FLOW field

 END TYPE PMANBATCH
 TYPE(PMANBATCH) :: PBMAN  !## Object contains all input/keyword from the batch function RUNFILE. Data is used for writing different type of RUN files
                           !## in GUI modus: PBMAN is filled from the Simulation Manager (sub)windows. 

 TYPE PERIODOBJ
  CHARACTER(LEN=MAXLENPRJ) :: NAME
  INTEGER :: IYR,IDY,IMH,IHR,IMT,ISC
 END TYPE PERIODOBJ
 TYPE(PERIODOBJ),DIMENSION(MAXPERIODS),SAVE :: PERIOD
 INTEGER :: NPERIOD
 
 TYPE SPECIESOBJ
  CHARACTER(LEN=MAXLENPRJ) :: NAME
  INTEGER :: IMOBILE   !## flag indicating mobility of a species (1: mobile, 2: immobile). Related to BTN key 'MCOMP'
 END TYPE SPECIESOBJ
 TYPE(SPECIESOBJ),DIMENSION(MAXSPECIES),SAVE :: SPECIES
 INTEGER :: NSPECIES
 
 !## For each simulation step date-time info and MODFLOW flags
 TYPE SIMOBJ
  INTEGER :: ISAVE                   !## save results
  CHARACTER(LEN=MAXLENPRJ) :: CDATE  !## date string
  REAL(KIND=DP_KIND) :: DELT         !## delta time timestep
  INTEGER :: DSEC                    !## number of second in between stresses
  INTEGER :: DDAY                    !## number of days in between stresses
  INTEGER :: ISUM                    !## sum fluxes
  INTEGER :: NSTP                    !## number of timesteps
  REAL(KIND=DP_KIND) :: TMULT        !## multiplication of timesteps
  REAL(KIND=DP_KIND) :: TTSMULT      !## transport step multiplier (only for fully implicit finite-difference method)
  REAL(KIND=DP_KIND) :: TTSMAX       !## maximum transport steps step size (only for fully implicit finite-difference method)
  REAL(KIND=DP_KIND) :: DT0          !## Transport time size
  INTEGER :: MXSTRN                  !## maximum transport steps allowed in one flow time-step
  INTEGER :: IDY,IMH,IYR             !## day,month,year
  INTEGER :: IHR,IMT,ISC             !## hour,minute,second
 END TYPE SIMOBJ
 TYPE(SIMOBJ),POINTER,DIMENSION(:) :: SIM=>NULL(),SIM_C=>NULL(),SIM_C2=>NULL()
  
 TYPE FILESOBJ
  INTEGER :: IACT                                  !## active in runfile
  CHARACTER(LEN=256) :: FNAME                      !## name of current file, could be a constant too!
  CHARACTER(LEN=MAXLENPRJ) :: ALIAS                !## alias name of current file, could be a constant too!
  INTEGER :: ID                                    !## id in treeview field
  INTEGER :: ILAY=0                                !## layer of current file
  INTEGER :: ICNST=1                               !## to be constant (1) or an idf file (2), or inherent (0)
  REAL(KIND=DP_KIND) :: FCT=1.0D0                  !## factor
  REAL(KIND=DP_KIND) :: IMP=0.0D0                  !## impulse
  REAL(KIND=DP_KIND) :: CNST=1.0D0                 !## constant value
 END TYPE FILESOBJ

 TYPE STRESSOBJ
  CHARACTER(LEN=MAXLENPRJ) :: CDATE                             !date string of each time step
  INTEGER :: IYR,IMH,IDY                                        !years,months ,days
  INTEGER :: IHR,IMT,ISC                                        !hours,minutes,seconds
  TYPE(FILESOBJ),POINTER,DIMENSION(:,:) :: FILES=>NULL(),&
                                       FILES_TMP=>NULL()      !stress information for current defined timestep
                                                                !(I,:) = subtopic i
                                                                !(:,I) = system i
  CHARACTER(LEN=256),POINTER,DIMENSION(:) :: INPFILES=>NULL()  !stores the inp files for metaswap
 END TYPE STRESSOBJ
 TYPE(STRESSOBJ),ALLOCATABLE,DIMENSION(:) :: STRESS  !## only in PMANAGER_GETIPER, temp to fill TOPICS(ITOPIC)%STRESS
 
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
  
 !## pest settings
 TYPE PSTMEASURE
  CHARACTER(LEN=256) :: IPFNAME
  INTEGER :: IPFTYPE,IXCOL,IYCOL,ILCOL,IMCOL,IVCOL,IDCOL
 END TYPE PSTMEASURE

 TYPE PSTPARAMOBJ
  INTEGER :: PACT=1
  INTEGER :: PILS=1
  INTEGER :: PIZONE=1
  INTEGER :: PIGROUP=1
  INTEGER :: PLOG=1
!  INTEGER :: CONVERTLOG=0
  INTEGER :: IPARAM=1
  INTEGER :: IBND
  CHARACTER(LEN=2) :: PPARAM
  CHARACTER(LEN=15) :: ACRONYM
  REAL(KIND=DP_KIND) :: PINI=1.0D0
  REAL(KIND=DP_KIND) :: PPRIOR=1.0D0
  REAL(KIND=DP_KIND),DIMENSION(2) :: ALPHA=1.0D0
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: ALPHA_HISTORY=>NULL()
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: GALPHA=>NULL()     !## collection of alphas per gradient simulation
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: LALPHA=>NULL()     !## collection of alphas per linesearch simulation
  REAL(KIND=DP_KIND) :: ALPHA_ERROR_STDEV
  REAL(KIND=DP_KIND) :: PDELTA=1.1D0
  REAL(KIND=DP_KIND) :: PMIN=0.01D0
  REAL(KIND=DP_KIND) :: PMAX=100.0D0
  REAL(KIND=DP_KIND) :: PINCREASE=2.0D0
  INTEGER,POINTER,DIMENSION(:) :: ILS 
  CHARACTER(LEN=256) :: ICOVFNAME
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:,:) :: COV,INVCOV,ISQRTCOV
  CHARACTER(LEN=256),POINTER,DIMENSION(:) :: REALSFNAME
  REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: MEAN,STD !## computed mean/var for each ensemble (or stdev?)
  REAL(KIND=DP_KIND) :: PARRANGE,PARMEAN,PARSTD !## computed mean/var for each ensemble (or stdev?)
 END TYPE PSTPARAMOBJ

 TYPE PSTOBJ
  INTEGER :: PE_MXITER=10
  INTEGER :: PE_SCALING=1
  INTEGER :: PE_KTYPE=1
  INTEGER :: PE_REGULARISATION=1
  INTEGER :: NREALS  !## number of realisations
  INTEGER :: ICREATEENSEMBLES  !## create realisations
  REAL(KIND=DP_KIND),DIMENSION(2) :: PE_TARGET=(/1.0D0,0.0D0/)
  REAL(KIND=DP_KIND) :: PE_STOP=0.0D0
  REAL(KIND=DP_KIND) :: PE_SENS=0.0D0
  REAL(KIND=DP_KIND) :: PE_PADJ=0.0D0
  REAL(KIND=DP_KIND) :: PE_DRES=0.0D0
  REAL(KIND=DP_KIND) :: PE_KRANGE=0.0D0
  REAL(KIND=DP_KIND) :: PE_VAR=0.0D0
  TYPE(PSTMEASURE),POINTER,DIMENSION(:) :: MEASURES=>NULL(),MEASURES_BU=>NULL()
  INTEGER :: IIPF
  CHARACTER(LEN=14),DIMENSION(:),POINTER :: S_PERIOD=>NULL(),S_PERIOD_BU=>NULL(), &
                                            E_PERIOD=>NULL(),E_PERIOD_BU=>NULL()
  REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: B_FRACTION=>NULL(),B_FRACTION_BU=>NULL()
  CHARACTER(LEN=256) :: PPBNDIDF
  CHARACTER(LEN=256),DIMENSION(:),POINTER :: B_BATCHFILE=>NULL(),B_BATCHFILE_BU=>NULL(), &
                                             B_OUTFILE=>NULL(),B_OUTFILE_BU=>NULL()
  TYPE(PSTPARAMOBJ),POINTER,DIMENSION(:) :: PARAM=>NULL(),PARAM_BU=>NULL()
  CHARACTER(LEN=256),POINTER,DIMENSION(:) :: IDFFILES=>NULL(),IDFFILES_BU=>NULL()  !## stores the idf files for zones
 END TYPE PSTOBJ

 TYPE(PSTOBJ) :: PEST
 
 !## Complete set of all files in time for all packages
 TYPE TOPICSOBJ
  LOGICAL :: DEFINED                                            !logical whether it is defined
  INTEGER :: NLAY                                               !maximum # continuous layers from 1 downwards
  INTEGER :: ID                                                 !id of main topics
  INTEGER,POINTER,DIMENSION(:) :: IDT=>NULL()                   !id of each time step for current topic
  INTEGER,POINTER,DIMENSION(:,:) :: ISD=>NULL()                 !id of main subtopics for each timestep
  INTEGER :: IACT                                               !active in projectmanager (only 1 at the same time)
  INTEGER :: IACT_MODEL                                         !active in runfile/model 
  CHARACTER(LEN=MAXLENPRJ) :: TNAME                             !name of topic
  CHARACTER(LEN=MAXLENPRJ),DIMENSION(MAXSUBTOPICS) :: SNAME     !name of subtopics
  INTEGER :: NSUBTOPICS                                         !number of subtopics
  LOGICAL :: TIMDEP                                             !timedependent module/package
  LOGICAL :: LSPECIES                                           !whether species can be defined
  CHARACTER(LEN=3) :: CMOD                                      !short name (3 characters) of the topic
  TYPE(STRESSOBJ),POINTER,DIMENSION(:) :: STRESS=>NULL(),STRESS_TMP=>NULL()     !files 
 END TYPE TOPICSOBJ
 TYPE(TOPICSOBJ),DIMENSION(MAXTOPICS) :: TOPICS

 CHARACTER(LEN=MAXLENPRJ),ALLOCATABLE,DIMENSION(:) :: MENUNAMES
 CHARACTER(LEN=3),DIMENSION(MAXPARAM) :: PARAM
 DATA PARAM/'KD','KH','KV','VC','SC','RC','RI','DC','IC','II','AF','AA','VA','HF','MS','MC','RE','EX','EP', &
    'QR','GC','SY','RL','RB','IL','IB','DL','MQ'/

 INTEGER,ALLOCATABLE,DIMENSION(:) :: IDT,LAYCON
   
 TYPE PRJOBJ
  INTEGER :: ILAY,ICNST,IACT,INHERENT
  REAL(KIND=DP_KIND) :: FCT,IMP,CNST
  CHARACTER(LEN=256) :: FNAME
 END TYPE PRJOBJ
 TYPE(PRJOBJ),POINTER,DIMENSION(:) :: PRJ=>NULL(),PRJ_TMP=>NULL()

 INTEGER :: PRJNLAY,PRJMXNLAY,ISUBMODEL,PRJNPER 
 INTEGER,DIMENSION(4) :: IFULL
 LOGICAL :: LQBD !## if true apply quasi 3D
 LOGICAL :: LBCF !## if true use bcf
 LOGICAL :: LLPF !## if true use lpf
 LOGICAL :: LNPF !## if true use npf for mf6
 LOGICAL :: LPKS !## if true use pks
 INTEGER :: IHEDUN,IBCFCB,IRCHCB,IEVTCB,IDRNCB,IRIVCB,IGHBCB,ICHDCB,IWELCB,ISFRCB,IFHBCB,ISFRCB2,IFHBUN,ILAKCB,IUZFCB1,IWL2CB
 
 REAL(KIND=DP_KIND),DIMENSION(:,:),ALLOCATABLE :: FHBHED,FHBFLW
 REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: FHBNBDTIM
 
 TYPE(IDFOBJ) :: PRJIDF ! IDF based on definition in variable SUBMODEL. Actual model dimensions (including buffer)
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: BND,TOP,BOT,SHD,LAK,KHV,KVV,KVA,STO,SPY,KDW,VCW,LBD,LCD,PCK,ANA,ANF,SFT,CON

 INTEGER,DIMENSION(:),ALLOCATABLE :: ULAKES,DULAKES,NP_IPER
 INTEGER :: NLAKES

! CHARACTER(LEN=52),SAVE :: MODELNAME='Model'
 REAL(KIND=DP_KIND),DIMENSION(7) :: SUBMODEL ! xmin, ymin, xmax, ymax, cell size, buffer, max buffer celsize. Result from Tab2 
 
 CHARACTER(LEN=8),DIMENSION(9) :: TMENU1
 CHARACTER(LEN=8),DIMENSION(10) :: TMENU2
 DATA TMENU1/'Minutes ','Hourly  ','Daily   ','Weekly  ','Decade  ', &
             '14/28   ','Monthly ','Yearly  ','Packages'/ !,'Custom  '/
 DATA TMENU2/'Minutes ','Hourly  ','Daily   ','Weekly  ','Decade  ', &
             '14/28   ','Monthly ','Yearly  ','Packages','All     '/
 
 INTEGER,ALLOCATABLE,DIMENSION(:) :: PRJILIST
 TYPE(PRJOBJ),POINTER,DIMENSION(:) :: FNAMES=>NULL(),FNAMES_BU=>NULL()
 
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

END MODULE MOD_PMANAGER_PAR