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

program driver

! modules
use driver_module
use IMOD_UTL, only : imod_utl_capf,luse_runfile
use imod_idf_par, only : idfobj
use m_main_info
use m_vcl, only: targ
use imod_utl, only: imod_utl_closeunits, imod_utl_has_ext, imod_utl_printtext,imod_utl_getunit, imod_utl_getslash
use pks_imod_utl, only: pks_imod_utl_idfmerge_init, pks_imod_utl_write_idfmergefile, pks_imod_utl_idfmerge ! PKS
use mod_pest, only: pest1_meteo_metaswap, pest1alpha_metaswap, pest1appendlogfile, pestnext, pestdumpfct, PEST1INIT, PEST1CLOSELOGFILES
use PESTVAR, only : IUPESTOUT
use pks_imod_utl, only: pks_imod_utl_iarmwp_xch_read
use, intrinsic :: ieee_exceptions
implicit none

! general
 logical :: mf_steadystate
 integer :: mf_ngrid, mf_igrid, iout, mf_minid, mf_maxid

! iPEST with MetaSWAP
 integer :: mf_nrow, mf_ncol, mf_nlay

! parameters
 integer, parameter :: nsubmax = 1000

! functions
 integer   osd_open2,cfn_length
 integer   cfn_idx_get_i
 double precision cfn_mjd_nodata
 logical pks7mpimasterwrite

! local variables
 integer   deltats,ios,iteration,lunc,lunsts,lc,xchinit,exitcode,ivcl,jvcl,iarg,jarg,itermozart,narg

 logical :: ok
 logical    endOfSimulation
 logical    stsave,strestore,mozstsave
 character  del*1,cont*1,comm*1

 character (len=1024) :: root, wd, modwd1, modwd2, simwd1, simwd2, mozwd, infile, rfopt, wddum, pfile, rfroot
 integer, allocatable, dimension(:) :: hrivsys, wrivsys
 integer :: nhrivsys, nwrivsys
 integer :: privsys, srivsys, trivsys, bdrnsys, odrnsys
 integer, dimension(1) :: m
 character (len=1)    :: cdum
 character (len=1024) :: record, modrecord, sobrecord, mozrecord, tranrecord
 character (len=32)   :: compcls,time_string
 character (len=1024) :: compfile
 logical  :: usemodflow,usemetaswap,usetransol,usemozart,usests,usestsmodflow
 logical  :: converged,convergedMF2005,convergedMetaSwap,convergedMozart,convergedPest
 logical :: doTimeLoop
 integer  :: retValMF2005

 double precision :: currentTime,currentTimeMF2005,currentTimeMozart, &
                     endOfCurrentTimeStep,endOfCurrentTimeStepMF2005, &
                     nodataTime,BeginOfNextTimeStepMF2005,&
                     endOfCurrentTimeStepMozart
 double precision :: dtMetaSwap            ! variable for metaswap
 double precision :: dtMozart
 integer          :: iterMetaSwap          ! variable for metaswap
 double precision :: stsTestTime

 integer                         :: stsFtest,stsBtest,stsItest,i,j,n
 double precision, allocatable   :: stsTtest(:)

 character tekens(1)*1

 integer tsc ! debug variable
 integer :: date, hour, minute, second, inoc

 logical :: lrunfile, lnamfile, llpf, lipest, lpwt, lss, lrf, psolved
 
 logical :: lwstdo, lpks, lidfmerge
 character(len=1024) :: idfmergefile
 
 integer :: isub, nsub, nnsub
 character(len=50), dimension(nsubmax) :: submstr=''

 real :: hnoflo
 type(idfobj) :: idf

 character(len=1024) :: str
 character(len=256) :: tssavepath ,savepdir
 
 real, dimension(4) :: imodusebox
 
 character(len=1) :: slash
 
! debug
 integer lswid, js, je, k, ilay, irow, icol, lun, cfn_getlun, ncvgerr

! program section
! ------------------------------------------------------------------------------

CALL IEEE_SET_HALTING_MODE (IEEE_DIVIDE_BY_ZERO, .TRUE.)
CALL IEEE_SET_HALTING_MODE (IEEE_OVERFLOW,       .TRUE.)

call pks7mpiini1(lwstdo) ! PKS
call pks7mpiactive(lpks) ! PKS

! Evaluate iMOD license
if(pks7mpimasterwrite()) call imod_license()
call pks7mpibarrier() ! PKS

! ... init
 exitcode    = 0            ! when 0: ok
 lunc        = 9            ! unit number for components file
 lunsts      = 8            ! <=0: sts2init() will assign a free unit number
                            !  >0: unit number will be used
 comm        = '!'          ! comment sign for components input file
 cont        = char(92)     ! continuation mark for components input file records
 nodataTime  = cfn_mjd_nodata()

 nsub = 0
 NCVGERR = 0

! ... process initialisation
! ... init components
!     ios: 0: OK
!         -1: commandline argument not found
!        <-1: ERROR
 ! define virtual commandline
 call cfn_vcl_set(' ',ivcl)
 
 ! get the root name
 call osd_getcwd(root)
 modwd1  = root
 simwd1  = root
 mozwd   = root

 ! get slash
 call imod_utl_getslash(slash) 
 
 ! run-file
 lrunfile    = .false.
 lnamfile    = .false.
 usemodflow  = .false.
 usemetaswap = .false.
 lipest      = .false.
 lidfmerge   = .false. 
 call cfn_vcl_narg(ivcl,narg)
 if (narg.eq.0) then
  call imod_utl_printtext(' ',3)
  call imod_utl_printtext('Driver program valid arguments:',3)
  call imod_utl_printtext(' 1: <iMOD run-file> <optional subdomain number>',3)
  call imod_utl_printtext(' 2: <MODFLOW-2005 nam-file>',3)
  call imod_utl_printtext(' 3: -components <components steering file>',3)
  call imod_utl_printtext(' 4: -pksmergeidf <PKS merge IDF file>',3)
  call imod_utl_printtext(' 5: <MODFLOW-2005 nam-file> -ipest <MODFLOW-2005 pst-file>',3)
  call pks7mpifinalize()! PKS
  call exit(0)
 end if
 
 ! option for merging PKS output IDF files
 call cfn_vcl_fndc(ivcl,iarg,'-pksmergeidf',.true.,idfmergefile,1)
 if (iarg.gt.0) then 
    call pks_imod_utl_idfmerge(idfmergefile) 
    stop   
 end if
 
 call cfn_vcl_arg(ivcl,1,infile,n)
 if (imod_utl_has_ext(infile,'nam')) lnamfile= .true.
 if (imod_utl_has_ext(infile,'run')) lrunfile= .true.
 luse_runfile=lrunfile
 if (lrunfile .or. lnamfile) then ! run-file or nam-file
    usemodflow=.true.
    modrecord=''
    if (lnamfile) write(modrecord,'(a,1x,3a)') '-namfile','"',trim(infile),'"'
    if (lrunfile) then
       write(modrecord,'(a,1x,3a)') '-runfile','"',trim(infile),'"'
       if (narg.eq.3) then ! runfile options
          call cfn_vcl_arg(ivcl,2,rfopt,n)
          write(modrecord,'(a,1x,a,1x,a)') trim(modrecord),'-rfopt',trim(rfopt)
          call cfn_vcl_arg(ivcl,3,rfopt,n)
          write(modrecord,'(a,1x,a)') trim(modrecord),trim(rfopt)
       end if
    end if
    wd = ''
    call rf2mf_prg(lrunfile,lipest,lidfmerge,modrecord,usemetaswap,submstr,nsub,nsubmax,wd,imodusebox,rfroot)
    modwd1 = trim(wd)
    call osd_s_filename(modwd1)
    if (usemetaswap) then
      simwd1 = trim(wd)
    end if
 end if

 usests        = .false.
 usestsmodflow = .false.
  
! !## peter remove this
! usests        = .true.
! usestsmodflow = .true.
! !## peter remove this

 usetransol    = .false.
 usemozart     = .false.
 ! get explicit arguments
 call cfn_vcl_fndc(ivcl,iarg,'-components',.true.,compfile,1)
 if (iarg.gt.0) then

    ! open file and initialise components
    ios=osd_open2(lunc,0,compfile,'readonly,shared')
    if (ios.eq.0) then

       ! read all component information and initialise all components
       call cfn_getrec(lunc,record,comm,cont)
       do while (cfn_length(record).gt.0)
          ! extract the first argument of variable record,
          ! leave the rest of the record for initialisation of the component
          tekens(1) = ' '
          call cfn_par_ext(compcls,record,tekens,1,del)
          call cfn_token(compcls,'tu')
          lc=cfn_length(compcls)

          select case( compcls(1:lc) )

          case( 'MODFLOW' )
                modrecord = record
                usemodflow=.true.
                ! set virtual command line
                call cfn_vcl_set(modrecord,jvcl)
                ! set working directory
                call cfn_vcl_fndc(jvcl,jarg,'-wd',.true.,wd,1)
                if (jarg.gt.0) modwd1 = trim(modwd1)//wd
                call osd_chdir(modwd1)
                ! check if state save is enabled
                call cfn_vcl_fnd(jvcl,jarg,'-sts',.true.)
                if (jarg.gt.0) then
                   usestsmodflow = .true.
!                   call sts2init(usestsmodflow,lunsts)
                end if
                ! convert iMOD run-file to MODFLOW
                call rf2mf_prg(lrf,lipest,lidfmerge,modrecord,usemetaswap,submstr,nsub,nsubmax,modwd1,imodusebox,rfroot)
                if (lrf) then
                   modwd1 = trim(modwd1)//slash//'mf2005_tmp'
                   call osd_s_filename(modwd1)
                end if
#ifdef INCLUDE_METASWAP
             case( 'METASWAP' )
                usemetaswap=.true.
                ! set working directory
                call cfn_vcl_set(record,jvcl)
                call cfn_vcl_fndc(jvcl,jarg,'-wd',.true.,wd,1)
                if (jarg.gt.0) simwd1 = trim(simwd1)//wd
             case( 'TRANSOL' )
                privsys = 0
                srivsys = 0
                trivsys = 0
                bdrnsys = 0
                odrnsys = 0
                tranrecord=record
                usetransol=.true.
                call cfn_vcl_set(tranrecord,jvcl)
                call cfn_vcl_fndi(jvcl,jarg,'-priv*sys',.true.,m,1)
                if (m(1).gt.0) privsys = m(1)
                call cfn_vcl_fndi(jvcl,jarg,'-sriv*sys',.true.,m,1)
                if (m(1).gt.0) srivsys = m(1)
                call cfn_vcl_fndi(jvcl,jarg,'-triv*sys',.true.,m,1)
                if (m(1).gt.0) trivsys = m(1)
                call cfn_vcl_fndi(jvcl,jarg,'-bdrn*sys',.true.,m,1)
                if (m(1).gt.0) bdrnsys = m(1)
                call cfn_vcl_fndi(jvcl,jarg,'-odrn*sys',.true.,m,1)
                if (m(1).gt.0) odrnsys = m(1)
#endif
#ifdef INCLUDE_MOZART
             case( 'MOZART' )
                mozrecord=record
                usemozart=.true.
                ! set working directory
                call cfn_vcl_set(mozrecord,jvcl)
                call cfn_vcl_fndc(jvcl,jarg,'-wd',.true.,wd,1)
                if (jarg.gt.0) mozwd = trim(mozwd)//wd
                ! get the river subsystems for coupling
                call cfn_vcl_fndi(jvcl,jarg,'-nhriv*sys',.true.,m,1)
                if (m(1).gt.0) then
                   nhrivsys = m(1)
                   allocate(hrivsys(nhrivsys))
                else
                   call driverChk(.false.,'Error: -nhriv*sys not found.')
                end if
                call cfn_vcl_fndi(jvcl,jarg,'-hriv*sys',.true.,hrivsys,nhrivsys)
                call cfn_vcl_fndi(jvcl,jarg,'-nwriv*sys',.true.,m,1)
                if (m(1).gt.0) then
                   nwrivsys = m(1)
                   allocate(wrivsys(nwrivsys))
                else
                   call driverChk(.false.,'-nwriv*sys not found.')
                end if
                call cfn_vcl_fndi(jvcl,jarg,'-wriv*sys',.true.,wrivsys,nwrivsys)
             case( 'STS' )
                usests = .true.
#endif
             case default
                ! ERROR, component unknown
                if (pks7mpimasterwrite()) write(*,*) ' Warning: component ',compcls(1:cfn_length(compcls)),' unknown.'

          end select

          ! get next component
          call cfn_getrec(lunc,record,comm,cont)
       enddo

       ! close components file
       close(lunc)
    else
       call driverChk(.false.,'Could not open component description file '//compfile(1:cfn_length(compfile)))
       call pks7mpifinalize()! PKS
       call exit(1)
    endif
 endif

 ! check if user has specified -ipest
 if(lnamfile)then
  lipest = .false.
  call cfn_vcl_fndc(ivcl,iarg,'-ipest',.true.,pfile,1)
  if (iarg.gt.0) then
   lipest=.true.  
  endif
 endif
 
 rt = driverGetRunType(usemodflow,usemetaswap,usetransol,usemozart,usests)
 
 str = 'Running mode: '//trim(rtdesc(rt))
 if(lpks) str = trim(str)//' (Parallel Krylov Solver activated)'
 if (pks7mpimasterwrite()) write(*,'(79(''*''),/,(1x,a),/,79(''*''))') trim(str)
 
! check state-save options
 if (usests.and..not.usestsmodflow) call driverChk(.false.,'MODFLOW state-save not activated.')
 if (.not.usests.and.usestsmodflow) call driverChk(.false.,'MODFLOW state-save activated.')

 nnsub = max(1,nsub)

! ######################################### SUBMODEL LOOP #########################################
 submodelloop: do isub = 1,nnsub
! ######################################### PEST LOOP #########################################
 call pks_imod_utl_idfmerge_init() ! PKS
 convergedPest = .false.
 pestloop: do while (.not.convergedPest)

 if (lrunfile) then
    if (nsub.gt.0) then
       write(modwd2,'(5a)') trim(modwd1),slash,trim(submstr(isub)),slash,'mf2005_tmp'
       if (usemetaswap) then
          write(simwd2,'(5a)') trim(simwd1),slash,trim(submstr(isub)),slash,'metaswap'
       end if
       write(*,'(50(''+''),/,1x,a,1x,a,1x,a,i3.3,a,i3.3,a,/,50(''+''))') 'Computing for submodel:',trim(submstr(isub)),'(',isub,'/',nsub,')'
    else
       write(modwd2,'(3a)') trim(modwd1),slash,'mf2005_tmp'
       if (usemetaswap) then
          write(simwd2,'(3a)') trim(simwd1),slash,'metaswap'
       end if
    end if
 else
    modwd2 = modwd1
    simwd2 = simwd1
 end if

 ! PKS IARMWP option
 call pks_imod_utl_iarmwp_xch_read(modwd2)

 timestep    = 1
 stsave      = .false.
 strestore   = .false.

 ! init return values
 retValMF2005      = 0

 ! time values
 currentTime       = nodataTime
 currentTimeMF2005 = 0.d0

 ! convergence
 convergedMF2005   = .true.
 convergedMetaSwap = .true.
 convergedMozart   = .true.
 endOfSimulation   = .false.

 ! init all components TODO!!!!!!!
 if (usemodflow) then
    call osd_chdir(modwd2)

    if (usestsmodflow) call sts2init(usestsmodflow,lunsts)
    call mf2005_initComponent(modrecord,retValMF2005)
    !## get pest variables
    call mf2005_GetSavePath_TS(tssavepath,savepdir)
    IF(lipest)THEN
     if(lnamfile)CALL PEST1INIT(1,pfile,IUPESTOUT,tssavepath,savepdir,idf,0) !'CODE OF HET UIT RUN- OF NAMFILE KOMT')
    ENDIF
    ! get number of grids
    ok = mf2005_PutNumberOfGrids(mf_ngrid); call driverChk(ok,'mf2005_PutNumberOfGrids')
    if (mf_ngrid.ne.1) call driverChk(.false.,'More than one MODFLOW grids is not supported.'); mf_igrid = 1
    if (lipest) then
       ok = mf2005_GetPestFlag(lipest); call driverChk(ok,'mf2005_GetPestFlag')
    end if
 end if
 if (usemetaswap) then
    call osd_chdir(simwd2)
    if (lipest) then
       mf_nrow = 0; mf_ncol = 0; lpwt = .false.
       ok = mf2005_PutGridDimensions(mf_igrid, mf_nrow, mf_ncol, mf_nlay); call driverChk(ok,'mf2005_PutGridDimensions')
       ok = mf2005_PutPWTActive(mf_igrid, lpwt); call driverChk(ok,'mf2005_PutPWTActive')
       call pest1_meteo_metaswap()
       call pest1alpha_metaswap(mf_nrow,mf_ncol,lpwt)
    end if
    call MetaSwap_initComponent()
 end if
 if (usetransol) call TRANSOL_initComponent()
 if (usemozart) then
    call osd_chdir(mozwd)
    call mozart_initComponent(mozrecord)
    ok = mf2005_GetDxcRoot(rfroot)
 end if

 ! append the PEST log-file
 if (lipest) then
    call pest1appendlogfile(tssavepath)
    call pest1log()
    CALL PEST1CLOSELOGFILES()
 end if

 ! check if MODFLOW is activated
 if (retValMF2005.ne.0) call driverChk(.false.,'MODFLOW initialization failed.')

! get starting date for this simulation
 call mf2005_getCurrentTime(currentTimeMF2005,retValMF2005)
 currentTime = currentTimeMF2005

! start sts
 if (usestsmodflow) call sts2start(currentTime)

! read data for entire simulation
 call osd_chdir(modwd2)
 call mf2005_initSimulation(currentTime,retValMF2005)
 if (retValMF2005.ne.0) exitcode = -12
 ! Coupling MODFLOW-MetaSWAP phase 1
 if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
    ok = mf2005_PutModSimNumberOfIDs(mf_igrid, mf_minid, mf_maxid, XchModSimModNID); call driverChk(ok,'mf2005_PutModSimNumberOfIDs') ! MODFLOW puts the number of MOD-SIM IDs
    call driverXchInitModSim1() ! allocate and init
    ok = mf2005_PutModSimIDs(mf_igrid,XchModSimModIds); call driverChk(ok,'mf2005_PutModSimIDs')       ! MODFLOW puts the MOD-SIM exchange IDs
    ok = mf2005_PutModSimCells(mf_igrid,XchModSimModCells); call driverChk(ok,'mf2005_PutModSimCells') ! MODFLOW puts the MOD-SIM cells
    call pks7mpimssetids(XchModSimModIds,XchModSimModNID,mf_minid, mf_maxid) ! PKS
    call osd_chdir(simwd2)
    call metaswap_initSimulation(currentTime)
 end if
! #### BEGIN EXCHANGE: BeforeInitSimulationMozart #############################

 ok = mf2005_PutLPFActive(mf_igrid, llpf); call driverChk(ok,'mf2005_PutLPFActive') ! get flag if BCF is used or LPF
 ok = mf2005_PutSimulationType(mf_igrid, lss); call driverChk(ok,'mf2005_PutSimulationType')  ! get flag if simulation is steady-state or transient
 ok = mf2005_PutHeadNoFlo(mf_igrid, hnoflo);  call driverChk(ok,'mf2005_PutHeadNoFlo')  ! get hnoflo

 ! append the PEST log-file
 IF (LIPEST) THEN
    CALL MF2005_RETURNIOUT(IOUT)
    CALL PESTDUMPFCT(TSSAVEPATH,IOUT,IDF)
 ENDIF

 !#### TIMESERIES ####
 call osd_chdir(root)
 call tserie1init1(lipest,lss,hnoflo)
 ok = mf2005_TimeserieInit(mf_igrid); call driverChk(ok,'mf2005_TimeserieInit')

 if (rt.eq.rtmodsimtranmoz) then
    call osd_chdir(mozwd)
    call mozart_initSimulation()
    convergedMozart = .false.
    endOfSimulation = .false.
    call mozart_prepareTimeStep(endOfSimulation,convergedMozart,currentTime) ! read signal file
 end if

! #### BEGIN EXCHANGE: BeforeTimestep #########################################
 if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
    ok = mf2005_PutSimulationType(mf_igrid, mf_steadystate); call driverChk(ok,'mf2005_PutSimulationType') ! MODFLOW puts the simulation type (transient/steady-state)
    if (mf_steadystate) call driverChk(ok,'MODFLOW may not be steady-state')
 end if
 ! Coupling MODFLOW-MetaSWAP phase 2
 if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
    ok = metaswap_PutModSimNumberOfIDs(XchModSimSimNID); call driverChk(ok,'metaswap_PutModSimNumberOfIDs')       ! MetaSWAP puts the number of MOD-SIM IDs
    call driverXchInitModSim2() ! allocate and init
    ok = metaswap_PutModSimIDs(XchModSimSimIds); call driverChk(ok,'metaswap_PutModSimIDs')            ! get exchange id's
 end if
 ! Coupling MODFLOW-MOZART/LSW, MODFLOW-MOZART/PV, MetaSWAP-MOZART/LSW
 if (rt.eq.rtmodsimtranmoz) then
    ! Main program puts the river systems to skip in the coupling
    ok = mf2005_PutModMozRiversToSkip(mf_igrid,nhrivsys,hrivsys); call driverChk(ok,'mf2005_PutModMozRiversToSkip')     ! Main program puts the river systems to skip in the coupling
    ok = mozart_PutModMozNumberOfIDs(XchMozNID); call driverChk(ok,'mozart_PutModMozNumberOfIDs')                       ! MOZART *puts* the number of LSW IDs
    ok = mozart_PutModMozPVNumberOfIDs(XchMozPVNID); call driverChk(ok,'mozart_PutModMozPVNumberOfIDs')                 ! MOZART *puts* the number of PV IDs
    ok = mf2005_PutModMozNumberOfIDs(mf_igrid, XchModMozModNID); call driverChk(ok,'mf2005_PutModMozNumberOfIDs')       ! MODFLOW *puts* the number of LSW IDs
    ok = mf2005_PutModMozPVNumberOfIDs(mf_igrid, XchModMozPVModNID); call driverChk(ok,'mf2005_PutModMozPVNumberOfIDs') ! MODFLOW *puts* the number of PV IDs
    ok = metaswap_PutModMozNumberOfIDs(XchSimMozSimNID); call driverChk(ok,'metaswap_PutModMozNumberOfIDs')             ! MetaSWAP *puts* the number of LSW IDs

    call driverXchInitModSimTranMoz() ! allocate and init

    ok = mozart_PutModMozIDs(XchMozIds,XchMozNID); call driverChk(ok,'mozart_PutModMozIDs')             ! MOZART *puts* the LSW IDs
    ok = mozart_PutModMozPVIDs(XchMozPVIds,XchMozPVNID); call driverChk(ok,'mozart_PutModMozPVIDs')     ! MOZART *puts* the PV IDs
    ok = mf2005_PutModMozIDs(mf_igrid, XchModMozModIds); call driverChk(ok,'mf2005_PutModMozIDs')       ! MODFLOW *puts* the LSW IDs
    ok = mf2005_PutModMozPVIDs(mf_igrid, XchModMozPVModIds); call driverChk(ok,'mf2005_PutModMozPVIDs') ! MODFLOW *puts* the PV IDs
    ok = mf2005_PutModMozCells(mf_igrid,XchModMozModCells); call driverChk(ok,'mf2005_PutModMozCells')  ! MODFLOW puts the MOD-MOZ cells
    ok = metaswap_PutModMozIDs(XchSimMozSimIds); call driverChk(ok,'metaswap_PutModMozIDs')             ! MetaSWAP *puts* the LSW IDs
 end if

 ! Create mappings
 if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) call driverXchIniMapModSim()
 if (rt.eq.rtmodsimtranmoz) then
    call driverXchIniMapModMoz()
    call driverXchIniMapModMozPV()
    call driverXchIniMapSimMoz()
    call driverXchIniMapModTran()
    call driverXchIniMapTranMoz()
 end if

! ###### END EXCHANGE: BeforeTimestep #########################################

! ... timestep
 timestep = 0

 tsc=0 ! debug: TimeStepCycle

 stsave    = .false.
 mozstsave = .false.
 strestore = .false.

 do while (.not.endOfSimulation .and. exitcode.eq.0) ! MOZART-loop

    iterMozart = 0; convergedMozart = .false.
    do while ((.not.endOfSimulation .and. exitcode.eq.0) .and. .not.convergedMozart)
       if (rt.eq.rtmodsimtranmoz) then
          iterMozart = iterMozart + 1
          if (iterMozart.eq.1) mozstsave = .true.
          call mozart_getCurrentTime(currentTimeMozart) ! get the current time
          currentTime = currentTimeMozart
          call mozart_getEndOfCurrentTimeStep(endOfCurrentTimeStepMozart) ! get the end of the currentMozart time step
          call osd_chdir(mozwd)
          call mozart_initTimeStep(iterMozart)  ! initialize time step

!##### BEGIN EXCHANGE: BeginOfMozartTimestep ##################################

          ok = mozart_PutLSWLevels(XchModMozMozLevels,mv); call DriverChk(ok,'mozart_PutLSWLevels') ! MOZART puts the LSW levels
          ok = mozart_PutPVLevels(XchModMozPVMozPVLevels,mv); call DriverChk(ok,'mozart_PutPVLevels') ! MOZART puts the PV levels
          ok = mozart_PutLSWFractions(XchSimMozMozFractions,mv); call DriverChk(ok,'mozart_PutLSWFractions') ! MOZART puts the LSW fractions
          ok = mf2005_GetLSWLevels(mf_igrid,XchModMozMozLevels,&
                                   XchModMozModNID,XchMoz2ModIdx,XchMoz2ModOff,mv); call DriverChk(ok,'mf2005_GetLSWLevels') ! MODFLOW gets LSW levels
          XchModMozMozLevels = mv
          ok = mf2005_GetPVLevels(mf_igrid,XchModMozPVMozPVLevels,&
                                  XchModMozPVModNID,XchMozPV2ModIdx,XchMozPV2ModOff,mv); call DriverChk(ok,'mf2005_GetPVLevels') ! MODFLOW gets PV levels
          XchModMozPVMozPVLevels = mv
          ok = metaswap_GetFractions(XchSimMozMozFractions,XchSimMozSimNID,XchMoz2SimIdx,XchMoz2ModOff,mv); call DriverChk(ok,'metaswap_GetFractions') ! MetaSWAP gets the LSW fractions
          XchSimMozMozFractions = mv
!####### END EXCHANGE: BeginOfMozartTimestep ##################################
       end if

       doTimeLoop = .true.
       do while(doTimeLoop .and. exitcode.eq.0)
          tsc=tsc+1
          timestep = timestep + 1

          call cfn_mjd2datehms(currentTime,date,hour,minute,second)

!what about steady-state solutions, currenttime.eq.0
!          if(issflg(kkper).eq.1)then 
!           write(*,'(5x,a,1x,a)')&
!              'Timestep     :','steady-state'
!          else
!           write(*,'(5x,a,1x,i5,1x,a,1x,i8,3(a,i2.2))')&
!              'Timestep     :',tsc,':',date,' ',abs(hour),':',minute,':',second
!          endif
          ! one timestep for each cycle

!##### BEGIN EXCHANGE: BeginOfTimestep ########################################
          if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
             ok = mf2005_PutHeads(mf_igrid,XchModSimModCells,XchModSimModNID,XchModSimModHeads,mv); call DriverChk(ok,'mf2005_PutHeads') ! Put groundwater heads
             ok = metaswap_GetHeads(XchModSimModHeads,XchModSimSimNID,XchMod2SimIdx,XchMod2SimOff,mv); call DriverChk(ok,'metaswap_GetHeads') ! Get groundwater heads
             XchModSimModHeads = mv
          end if
!####### END EXCHANGE: BeginOfTimestep ########################################

          ! perform state save, phase 1 (before reading and writing data)
          if (usests) then
             if (mozstsave) stsave = .true.
             call osd_chdir(modwd2)
             call GWF2BAS7_GETOC(inoc)
             if (usestsmodflow) call sts2saverestore(currentTime,stsave,strestore,1,inoc)
             if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
                call osd_chdir(simwd2)
                call MetaSWAP_saveRestore(stsave,strestore)
                !call TRANSOL_saveRestore(stsave,strestore)
             end if
          end if

         ! ... init timestep
          call mf2005_prepareTimestep(currentTime,stsave,retValMF2005)
          call osd_chdir(modwd2)
          call mf2005_initTimeStep(currentTime,retValMF2005)
          if (retValMF2005.ne.0) exitcode = -15

          ! get end of current timestep
          !!!!!!!!!!!!!!!! Only modflow information is used !!!!!!!!!!!!!!!!!!!!!!!
          call mf2005_getEndOfCurrentTimeStep(endOfCurrentTimeStepMF2005,retValMF2005)
          if (retValMF2005.ne.0) exitcode = -17
          endOfCurrentTimeStep = endOfCurrentTimeStepMF2005

          ! extra check
          if (endOfCurrentTimeStep.eq.nodataTime) call driverChk(.false.,'endOfCurrentTimeStep = nodataTime')

          ! read timestep data
          if (exitcode.eq.0) then
             if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
                dtMetaSwap = endOfCurrentTimeStep - currentTime
                call osd_chdir(simwd2)
                call MetaSwap_initTimestep(dtMetaSwap)
                iterMetaSwap = 0
             end if
          end if

          if (pks7mpimasterwrite()) call mf2005_writeTimeStep(tsc,date,hour,minute,second)
          
          ! one timestep for each cycle

! ... iteration
          converged =.false.
          do while (.not. converged .and. exitcode.eq.0)

             if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
                call MetaSwap_prepareIter(iterMetaSwap)  ! only action: iter=iter+1
                call MetaSwap_performIter(iterMetaSwap)
!##### BEGIN EXCHANGE: AfterSolveMetaSwap #####################################
                ok = metaswap_PutModSimUnsaturatedZoneFlux(XchModSimSimUnsZFlux,mv); call DriverChk(ok,'metaswap_PutUnsaturatedZoneFlux') ! Put unsaturated zone flux
                ok = mf2005_GetUnsaturatedZoneFlux(mf_igrid,&
                        XchModSimModNID,XchModSimSimUnsZFlux,XchSim2ModIdx,XchSim2ModOff,mv); call DriverChk(ok,'mf2005_GetUnsaturatedZoneFlux') ! Get unsaturated zone flux
                XchModSimSimUnsZFlux = mv
                if (.not.mf_steadystate) then
                   ok = metaswap_PutStorageFactor(XchModSimSimStrFct,mv); call DriverChk(ok,'metaswap_PutStorageFactor') ! Put storage factor
                   if (.not.llpf) then ! BCF
                      ok = mf2005_GetStorageFactor(mf_igrid,XchModSimSimStrFct,&
                                                  XchModSimModNID,XchSim2ModIdx,XchSim2ModOff,mv); call DriverChk(ok,'mf2005_GetStorageFactor') ! Get storage factor
                   else ! LPF
                      ok = mf2005_GetStorageFactorLPF(mf_igrid,XchModSimSimStrFct,&
                                                  XchModSimModNID,XchSim2ModIdx,XchSim2ModOff,mv); call DriverChk(ok,'mf2005_GetStorageFactorLPF') ! Get storage factor
                   end if
                   XchModSimSimStrFct = mv
                end if
             end if
!####### END EXCHANGE: AfterSolveMetaSwap #####################################
             call mf2005_performIter(retValMF2005,psolved)
             if (retValMF2005.ne.0) exitcode = -26

!##### BEGIN EXCHANGE: AfterSolve #############################################
             if (exitcode.eq.0) then
                if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
                   ok = mf2005_PutHeads(mf_igrid,XchModSimModCells,XchModSimModNID,XchModSimModHeads,mv); call DriverChk(ok,'mf2005_PutHeads') ! MODFLOW put groundwater heads
                   ok = metaswap_GetHeads(XchModSimModHeads,XchModSimSimNID,XchMod2SimIdx,XchMod2SimOff,mv); call DriverChk(ok,'metaswap_GetHeads') ! MetaSWAP gets the groundwater heads
                   XchModSimModHeads = mv
                end if
             end if
!####### END EXCHANGE: AfterSolve #############################################

             ! finish iteration
             if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz)&
             call MetaSwap_finishIter(iterMetaSwap,convergedMetaSwap)
             call mf2005_finishIter(convergedMF2005,retValMF2005)
             if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
                if (pks7mpimasterwrite()) write(*,*) ' convergence MODFLOW - MetaSWAP: ',convergedMF2005, convergedMetaSwap
             end if   

             ! get next iteration information
             converged = .true.
             converged = converged .and. convergedMF2005
             if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz)&
                converged = converged .and. convergedMetaSwap

             if (retValMF2005.ne.0    ) exitcode = -261
          enddo
          if (.not.converged) then
             if (pks7mpimasterwrite()) write(*,*) ' Model did not converge!',exitcode
          end if   

! ... write results
          if (converged .and. exitcode.eq.0) then
             call osd_chdir(modwd2)
             call mf2005_finishTimestep(retValMF2005,psolved,ncvgerr)
             if (retValMF2005.ne.0) exitcode = -31

!##### BEGIN EXCHANGE: AfterFinishTimeStepMODFLOW #############################
             if (rt.eq.rtmodsimtranmoz) then
                !##### MODFLOW-TRANSOL interface !#####
                
                ! River stages P, S, T (privsys, srivsys, trivsys)
                ok = mf2005_PutRiverStageSubsys(mf_igrid,XchModTranModRiverStage,XchModTranModCells,XchModTranModNID,mv,privsys); call DriverChk(ok,'mf2005_PutRiverStageSubsys') ! MODFLOW puts the river stage for TRANSOL [m]: P
                ok = TRANSOL_GetSeepageRiverDrain(XchModTranModRiverStage,XchModTranModNID,XchMod2TranIdx,XchMod2TranOff,mv,'srivp'); call DriverChk(ok,'TRANSOL_GetSeepageRiverDrain') ! TRANSOL gets the river Stage: P
                XchModTranModRiverStage = mv
                ok = mf2005_PutRiverStageSubsys(mf_igrid,XchModTranModRiverStage,XchModTranModCells,XchModTranModNID,mv,srivsys); call DriverChk(ok,'mf2005_PutRiverStageSubsys') ! MODFLOW puts the river stage for TRANSOL [m]: S
                ok = TRANSOL_GetSeepageRiverDrain(XchModTranModRiverStage,XchModTranModNID,XchMod2TranIdx,XchMod2TranOff,mv,'srivs'); call DriverChk(ok,'TRANSOL_GetSeepageRiverDrain') ! TRANSOL gets the river Stage: S
                XchModTranModRiverStage = mv
                ok = mf2005_PutRiverStageSubsys(mf_igrid,XchModTranModRiverStage,XchModTranModCells,XchModTranModNID,mv,trivsys); call DriverChk(ok,'mf2005_PutRiverStageSubsys') ! MODFLOW puts the river stage for TRANSOL [m]: T
                ok = TRANSOL_GetSeepageRiverDrain(XchModTranModRiverStage,XchModTranModNID,XchMod2TranIdx,XchMod2TranOff,mv,'srivt'); call DriverChk(ok,'TRANSOL_GetSeepageRiverDrain') ! TRANSOL gets the river Stage: T
                XchModTranModRiverStage = mv                
                
                ! Seepage flux
                ok = mf2005_PutSeepageFlux(mf_igrid,XchModTranModSeepageFlux,XchModTranModCells,XchModTranModNID,mv,.true.); call DriverChk(ok,'mf2005_PutSeepageFlux') ! MODFLOW puts the seepage flux for TRANSOL
                ok = TRANSOL_GetSeepageRiverDrain(XchModTranModSeepageFlux,XchModTranModNID,XchMod2TranIdx,XchMod2TranOff,mv,'qseep'); call DriverChk(ok,'TRANSOL_GetSeepageRiverDrain') ! TRANSOL gets the seepage flux
                XchModTranModSeepageFlux = mv
                
                ! River fluxes P, S, T (privsys, srivsys, trivsys)
                ok = mf2005_PutRiverFluxSubsys(mf_igrid,XchModTranModRiverFlux,XchModTranModCells,XchModTranModNID,mv,.true.,privsys); call DriverChk(ok,'mf2005_PutRiverFluxSubsys') ! MODFLOW puts the river flux for TRANSOL [m]: P
                ok = TRANSOL_GetSeepageRiverDrain(XchModTranModRiverFlux,XchModTranModNID,XchMod2TranIdx,XchMod2TranOff,mv,'qrivp'); call DriverChk(ok,'TRANSOL_GetSeepageRiverDrain') ! TRANSOL gets the river flux: P
                XchModTranModRiverFlux = mv
                ok = mf2005_PutRiverFluxSubsys(mf_igrid,XchModTranModRiverFlux,XchModTranModCells,XchModTranModNID,mv,.true.,srivsys); call DriverChk(ok,'mf2005_PutRiverFluxSubsys') ! MODFLOW puts the river flux for TRANSOL [m]: S
                ok = TRANSOL_GetSeepageRiverDrain(XchModTranModRiverFlux,XchModTranModNID,XchMod2TranIdx,XchMod2TranOff,mv,'qrivs'); call DriverChk(ok,'TRANSOL_GetSeepageRiverDrain') ! TRANSOL gets the river flux: S
                XchModTranModRiverFlux = mv
                ok = mf2005_PutRiverFluxSubsys(mf_igrid,XchModTranModRiverFlux,XchModTranModCells,XchModTranModNID,mv,.true.,trivsys); call DriverChk(ok,'mf2005_PutRiverFluxSubsys') ! MODFLOW puts the river flux for TRANSOL [m]: T
                ok = TRANSOL_GetSeepageRiverDrain(XchModTranModRiverFlux,XchModTranModNID,XchMod2TranIdx,XchMod2TranOff,mv,'qrivt'); call DriverChk(ok,'TRANSOL_GetSeepageRiverDrain') ! TRANSOL gets the river flux: T
                XchModTranModRiverFlux = mv
                
                ! Drain fluxes B(uis) and O(overland flow) (bdrnsys, odrnsys)
                ok = mf2005_PutDrainFluxSubsys(mf_igrid,XchModTranModDrainFlux,XchModTranModCells,XchModTranModNID,mv,.true.,bdrnsys); call DriverChk(ok,'mf2005_PutDrainFluxSubsys') ! MODFLOW puts the drain flux for TRANSOL [m]: b
                ok = TRANSOL_GetSeepageRiverDrain(XchModTranModDrainFlux,XchModTranModNID,XchMod2TranIdx,XchMod2TranOff,mv,'qdrnb'); call DriverChk(ok,'TRANSOL_GetSeepageRiverDrain') ! TRANSOL gets the drain flux
                XchModTranModDrainFlux = mv
                ok = mf2005_PutDrainFluxSubsys(mf_igrid,XchModTranModDrainFlux,XchModTranModCells,XchModTranModNID,mv,.true.,odrnsys); call DriverChk(ok,'mf2005_PutDrainFluxSubsys') ! MODFLOW puts the drain flux for TRANSOL [m]: b
                ok = TRANSOL_GetSeepageRiverDrain(XchModTranModDrainFlux,XchModTranModNID,XchMod2TranIdx,XchMod2TranOff,mv,'qdrno'); call DriverChk(ok,'TRANSOL_GetSeepageRiverDrain') ! TRANSOL gets the drain flux
                XchModTranModDrainFlux = mv
                
                ! !##### MODFLOW-MOZART interface !#####
                ok = mf2005_PutRiverFlux(mf_igrid,XchModMozModRiverFlux,XchModMozModCells,XchModMozModNID,mv,&
                                         nhrivsys,hrivsys,nwrivsys,wrivsys,.false.,.false.); call DriverChk(ok,'mf2005_PutRiverFlux') ! MODFLOW puts the river flux for MOZART: skip H and W [m3]
                ok = mf2005_PutRiverFlux(mf_igrid,XchModMozModRiverFluxWells,XchModMozModCells,XchModMozModNID,mv,&
                                         nhrivsys,hrivsys,nwrivsys,wrivsys,.false.,.true.); call DriverChk(ok,'mf2005_PutRiverFlux') ! MODFLOW puts the river flux for MOZART ("wellen"): skip H; only W; [m3]
                ok = mf2005_PutDrainFlux(mf_igrid,XchModMozModDrainFlux,XchModMozModCells,XchModMozModNID,mv,.false.); call DriverChk(ok,'mf2005_PutDrainFlux') ! MODFLOW puts the drain flux for MOZART; don't skip; all layers; [m3]
                ok = mf2005_PutSaltFlux(mf_igrid,XchModMozModSalt,XchModMozModCells,XchModMozModNID,mv,nwrivsys,wrivsys); call DriverChk(ok,'mf2005_PutSaltFlux') ! MODFLOW puts the salt flux for MOZART
                ok = MOZART_GetRiverDrainFlux(XchModMozModRiverFlux,XchMozNID,XchMod2MozIdx,XchMod2MozOff,mv,1); call DriverChk(ok,'MOZART_GetRiverDrainFlux') ! MOZART gets the river flux
                XchModMozModRiverFlux = mv
                ok = MOZART_GetRiverDrainFlux(XchModMozModRiverFluxWells,XchMozNID,XchMod2MozIdx,XchMod2MozOff,mv,2); call DriverChk(ok,'MOZART_GetRiverDrainFlux') ! MOZART gets the river flux (wells)
                XchModMozModRiverFluxWells = mv
                ok = MOZART_GetRiverDrainFlux(XchModMozModDrainFlux,XchMozNID,XchMod2MozIdx,XchMod2MozOff,mv,1); call DriverChk(ok,'MOZART_GetRiverDrainFlux') ! MOZART gets the drain flux
                XchModMozModDrainFlux = mv
                ok = MOZART_GetSalt(XchModMozModSalt,XchMozNID,XchMod2MozIdx,XchMod2MozOff,mv,2); call DriverChk(ok,'MOZART_GetSalt') ! MOZART get the salt flux
                XchModMozModSalt = mv
             end if
!##### BEGIN EXCHANGE: AfterFinishTimeStepMODFLOW #############################

             if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
                call osd_chdir(simwd2)
                call MetaSwap_finishTimestep(currentTime)
             end if

             !#### TIMESERIES #####
             call osd_chdir(root)  
             ok = mf2005_TimeserieGetHead(mf_igrid); call DriverChk(ok,'mf2005_TimeserieGetHead')
             call gwf2getcurrentdate(mf_igrid,time_string)  !,issflg(kkper)
             call tserie1init2(lipest,lss,hnoflo,tssavepath,submstr(isub))
             call tserie1write(0,lss,currentTime,dble(hnoflo),usests,tssavepath,submstr(isub),time_string) !,tssavepath)

             !#### TIMESERIES #####

          endif

          if (exitcode.eq.0) then
             call GWF2BAS7_GETOC(inoc)
             ! perform state save, phase 2 (after reading and writing data)
             if (usestsmodflow) call sts2saverestore(currentTime,stsave,strestore,2,inoc)
             if (mozstsave) then
                stsave    = .false.
                strestore = .false.
                mozstsave = .false.
             end if
          endif

! ... next timestep
          if (exitcode.ne.0) then
             ! ERROR, do not continue
             endOfSimulation=.true.
          else

             ! get new time information
             ! Let Modflow be leading. don't ask the other components
             call mf2005_getBeginOfNextTimeStep(BeginOfNextTimeStepMF2005,retValMF2005)
             ! find errors
             if (retValMF2005.eq.-1) then
                ! end of simulation for Modflow
                endOfSimulation=.true.
                retValMF2005   =0
             endif
             if (retValMF2005.ne.0) exitcode = -31
             if (exitcode.ne.0)     endOfSimulation=.true.
             ! find start time of next timestep
             !   this may be a time in the 'future', the 'past' or the 'current' time
             ! nodata value of time-value means: this was the last timestep for this component
             if (exitcode.eq.0) then

                currentTime=nodataTime

                ! check for each component (ok, only modflow available now)
                if (BeginOfNextTimeStepMF2005.ne.nodataTime) then
                   if (currentTime.eq.nodataTime) then
                      currentTime=BeginOfNextTimeStepMF2005
                   else
                      currentTime=min(currentTime,BeginOfNextTimeStepMF2005)
                   endif
                endif

                if (currentTime.eq.nodataTime.and.rt.ne.rtmod) endOfSimulation=.true.
!                if (currentTime.eq.nodataTime) endOfSimulation=.true.
             endif
          endif

          if (rt.eq.rtmodsimtranmoz) then
             doTimeLoop = currenttime.lt.endOfCurrentTimeStepMozart .and. .not.endOfSimulation
          else
             doTimeLoop = .not.endOfSimulation
          end if
       enddo ! time loop

!##### BEGIN EXCHANGE: BeforeTRANSOLIteration #################################
       if (rt.eq.rtmodsimtranmoz .and. exitcode.eq.0) then
          ok = mozart_PutLSWSalt(XchTranMozMozSalt,mv); call DriverChk(ok,'Error mozart_PutLSWSalt') ! MOZART puts the LSW salt concentrations
          ok = TRANSOL_GetSalt(XchTranMozMozSalt,&
                               XchSimMozSimNID,XchMoz2SimIdx,XchMoz2ModOff,mv); call DriverChk(ok,'Error TRANSOL_GetSalt') ! TRANSOL gets the LSW salt concentrations
          XchTranMozMozSalt = mv
       end if

!####### END EXCHANGE: BeforeTRANSOLIteration #################################

       if (rt.eq.rtmodsimtranmoz .and. exitcode.eq.0) then
          call osd_chdir(simwd2)
          call TRANSOL_performIter()
       end if
!##### BEGIN EXCHANGE: AfterMozartIteration ###################################
       if (rt.eq.rtmodsimtranmoz .and. exitcode.eq.0) then
          ok = MetaSWAP_PutCumSWSprinklingDemandFluxes(XchSimMozSimCuSWSprinklingFlux,mv); call DriverChk(ok,'MetaSWAP_PutCumSWSprinklingDemandFluxes') ! MetaSWAP puts the cumulative surface water sprinkling demand fluxes
          ok = MetaSWAP_PutCumRunonFluxes(XchSimMozSimCuRunonFlux,mv); call DriverChk(ok,'MetaSWAP_PutCumRunonFluxes') ! MetaSWAP puts the cumulative runon fluxes
          ok = TRANSOL_PutSalt(XchTranMozTranSalt,mv); call DriverChk(ok,'TRANSOL_PutSalt') ! TRANSOL puts the concentrations
          ok = MOZART_GetCumSWSprinklingDemandFluxes(XchSimMozSimCuSWSprinklingFlux,&
                                                  XchMozNID,XchSim2MozIdx,XchSim2MozOff,mv); call DriverChk(ok,'MOZART_GetCumSWSprinklingDemandFluxes') ! MOZART gets the surface water sprinkling demand fluxes
          XchSimMozSimCuSWSprinklingFlux = mv
          ok = MOZART_GetCumRunonFluxes(XchSimMozSimCuRunonFlux,&
                                        XchMozNID,XchSim2MozIdx,XchSim2MozOff,mv); call DriverChk(ok,'MOZART_GetCumRunonFluxes') ! MOZART gets the cumulative runon fluxes
          ok = MOZART_GetSalt(XchTranMozTranSalt,&
                              XchMozNID,XchTran2MozIdx,XchTran2MozOff,mv,1); call DriverChk(ok,'MOZART_GetSalt') ! MOZART gets the concentrations
          XchTranMozTranSalt = mv
       end if
!####### END EXCHANGE: AfterMozartIteration ###################################

       if (rt.eq.rtmodsimtranmoz .and. exitcode.eq.0) then
          call osd_chdir(simwd2)
          call TRANSOL_finishTimeStep(currentTime)
          dtMozart = endOfCurrentTimeStepMozart - currentTimeMozart
          call osd_chdir(mozwd)
          call mozart_finishTimeStep(iterMozart,dtMozart,usetransol)
          call mozart_prepareTimeStep(endOfSimulation,convergedMozart,currentTime) ! read signal file
       end if

       ! make sure MODFLOW-MetaSWAP terminates normally
       if (rt.eq.rtmodsim) then
          convergedMozart = .true.
       end if
    end do ! Mozart loop
 end do

 !## deallocate memory
 call pest1_deallocate()

 ! ... end
 call osd_chdir(modwd2)
 if (exitcode.eq.0)  call mf2005_finishSimulation(retValMF2005)
 if (retValMF2005.ne.0 ) exitcode = -61
 if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
    call osd_chdir(simwd2)
    call MetaSWAP_finishSimulation()
 end if
!#### TIMESERIES #####
 call osd_chdir(root)
 call tserie1write(1,lss,currentTime,dble(hnoflo),usests,tssavepath,submstr(isub),time_string)
 call tserie1close()
!#### TIMESERIES #####

! list routine timer info
 call cfn_rtt_list(0)

! Deallocate coupling arrays
 call driverXchDeallocate()

! exit
 if (exitcode.ne.0) then
    if (pks7mpimasterwrite()) write(unit=*,fmt=*) 'ERROR, exit code: ',exitcode
    call pks7mpifinalize()! PKS 
    call exit(10)
 endif

!C10-----END OF PROGRAM.
      IF(NCVGERR.GT.0) THEN
        if (pks7mpimasterwrite()) WRITE(*,*) 'FAILED TO MEET SOLVER CONVERGENCE CRITERIA ',NCVGERR,' TIME(S)'
      END IF

! next pest iteration
 call imod_utl_closeunits()
 if (.not.lipest) then
    convergedPest=.true.
 else
  convergedPest=pestnext(lss,tssavepath,idf)
 end if
! call imod_utl_closeunits()

 end do pestloop

! PKS IDF merge
 call osd_chdir(root)
 call pks_imod_utl_write_idfmergefile(modwd2,idfmergefile)
 if (lidfmerge.and.lpks) call pks_imod_utl_idfmerge(idfmergefile)

 end do submodelloop

! timing output
call osd_chdir(root)
call timing_stat() 
 
call pks7mpifinalize()! PKS 
 
end program

