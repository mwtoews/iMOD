!subroutine mf_init(config_file,nnsub,stsOverRule)
!!DEC$ ATTRIBUTES DLLEXPORT :: mf_init
! use mf_module
! use m_main_info
! use m_vcl, only: targ
! use imod_utl, only: imod_utl_closeunits, imod_utl_has_ext, imod_utl_printtext
! use mod_pest, only: pest1_meteo_metaswap, pest1alpha_metaswap, pest1appendlogfile, pestnext
! 
! implicit none   
! 
! ! arguments
! character(len=*), intent(in) :: config_file ! nam-file, components-file or run file
! integer, intent(out) :: nnsub
! logical, intent(in) :: stsOverRule
!    
!! functions
! integer   osd_open2,cfn_length
! integer   cfn_idx_get_i
! double precision cfn_mjd_nodata
!
!! local variables
!
! character  del*1,cont*1,comm*1
!
! integer, dimension(1) :: m
! character (len=1)    :: cdum
! character (len=32)   :: compcls
! character (len=1024) :: compfile
!
! integer                         :: stsFtest,stsBtest,stsItest,i,j,n
! double precision, allocatable   :: stsTtest(:)
!
! character tekens(1)*1
!
!! debug
! integer lswid, js, je, k, ilay, irow, icol, lun, cfn_getlun
!
!! program section
!! ------------------------------------------------------------------------------
!
! call imod_utl_printtext('====================================================================',0)
! call imod_utl_printtext('You may use this compiled version of the iMOD-software if you are '  ,0)
! call imod_utl_printtext('entitled to this use under a iMOD software license agreement for the',0)
! call imod_utl_printtext('iMOD software executables with Deltares or with a party entitled by' ,0)
! call imod_utl_printtext('Deltares to provide sublicenses for the iMOD-software executables.'  ,0)
! call imod_utl_printtext('Otherwise use of this compiled version of the iMOD-software is'      ,0)
! call imod_utl_printtext('prohibited and illegal. If you are not allowed under a Deltares iMOD',0)
! call imod_utl_printtext('license agreement to use the iMOD-software executables, you may find',0)
! call imod_utl_printtext('a solution in compiling the open source version of the iMOD-software',0)
! call imod_utl_printtext('into an executable yourself (see oss.deltares.nl), or apply for a'   ,0)
! call imod_utl_printtext('Deltares iMOD license agreement by sending an email to'              ,0)
! call imod_utl_printtext(''                                                                    ,0)
! call imod_utl_printtext('Version 3.00.01, 18/09/14'                                           ,0)
! call imod_utl_printtext('====================================================================',0)
!
!! ... init
! exitcode    = 0            ! when 0: ok
! lunc        = 9            ! unit number for components file
! lunsts      = 8            ! <=0: sts2init() will assign a free unit number
!                            !  >0: unit number will be used
! comm        = '!'          ! comment sign for components input file
! cont        = char(92)     ! continuation mark for components input file records
! nodataTime  = cfn_mjd_nodata()
!
! nsub = 0
!
!! ... process initialisation
!! ... init components
!!     ios: 0: OK
!!         -1: commandline argument not found
!!        <-1: ERROR
! ! define virtual commandline
! call cfn_vcl_set(' ',ivcl)
!
! ! get the root name
! call osd_getcwd(root)
! modwd1  = root
! simwd1  = root
! mozwd   = root
!
! ! run-file
! lrunfile = .false.
! lnamfile = .false.
! usemodflow    = .false.
! usemetaswap   = .false.
! lipest = .false.
!! call cfn_vcl_narg(ivcl,narg)
!! if (narg.eq.0) then
!!  call imod_utl_printtext(' ',0)
!!  call imod_utl_printtext('Driver program valid arguments:',0)
!!  call imod_utl_printtext(' 1: <iMOD run-file> <optional subdomain number>',0)
!!  call imod_utl_printtext(' 2: <MODFLOW-2005 nam-file>',0)
!!  call imod_utl_printtext(' 3: -components <components steering file>',0)
!!  call exit(0)
!! end if
!! call cfn_vcl_arg(ivcl,1,infile,n)
! infile = config_file 
! if (imod_utl_has_ext(infile,'nam')) lnamfile= .true.
! if (imod_utl_has_ext(infile,'run')) lrunfile= .true.
! if (lrunfile .or. lnamfile) then ! run-file or nam-file
!    usemodflow=.true.
!    modrecord=''
!    if (lnamfile) write(modrecord,'(a,1x,a)') '-namfile',trim(infile)
!    if (lrunfile) then
!       write(modrecord,'(a,1x,a)') '-runfile',trim(infile)
!       if (narg.eq.3) then ! runfile options
!          call cfn_vcl_arg(ivcl,2,rfopt,n)
!          write(modrecord,'(a,1x,a,1x,a)') trim(modrecord),'-rfopt',trim(rfopt)
!          call cfn_vcl_arg(ivcl,3,rfopt,n)
!          write(modrecord,'(a,1x,a)') trim(modrecord),trim(rfopt)
!       end if
!    end if
!    wd = ''
!    call rf2mf_prg(lrunfile,lipest,modrecord,usemetaswap,submstr,nsub,nsubmax,wd)
!    modwd1 = trim(wd)
!    call osd_s_filename(modwd1)
!    if (usemetaswap) then
!      simwd1 = trim(wd)
!    end if
! end if
!
! usests        = .false.
! usestsmodflow = .false.
! usetransol    = .false.
! usemozart     = .false.
! useribasim    = .false.
! ! get explicit arguments
! call cfn_vcl_fndc(ivcl,iarg,'-components',.true.,compfile,1)
! if (iarg.gt.0) then
!
!    ! open file and initialise components
!    ios=osd_open2(lunc,0,compfile,'readonly')
!    if (ios.eq.0) then
!
!       ! read all component information and initialise all components
!       call cfn_getrec(lunc,record,comm,cont)
!       do while (cfn_length(record).gt.0)
!          ! extract the first argument of variable record,
!          ! leave the rest of the record for initialisation of the component
!          tekens(1) = ' '
!          call cfn_par_ext(compcls,record,tekens,1,del)
!          call cfn_token(compcls,'tu')
!          lc=cfn_length(compcls)
!
!          select case( compcls(1:lc) )
!
!          case( 'MODFLOW' )
!                modrecord = record
!                usemodflow=.true.
!                ! set virtual command line
!                call cfn_vcl_set(modrecord,jvcl)
!                ! set working directory
!                call cfn_vcl_fndc(jvcl,jarg,'-wd',.true.,wd,1)
!                if (jarg.gt.0) modwd1 = trim(modwd1)//wd
!                call osd_chdir(modwd1)
!                ! check if state save is enabled
!                call cfn_vcl_fnd(jvcl,jarg,'-sts',.true.)
!                if (jarg.gt.0) then
!                   usestsmodflow = .true.
!!                   call sts2init(usestsmodflow,lunsts)
!                end if
!                ! convert iMOD run-file to MODFLOW
!                call rf2mf_prg(lrf,lipest,modrecord,usemetaswap,submstr,nsub,nsubmax,modwd1)
!                if (lrf) then
!                   modwd1 = trim(modwd1)//'\mf2005_tmp'
!                   call osd_s_filename(modwd1)
!                end if
!#ifdef INCLUDE_METASWAP
!             case( 'METASWAP' )
!                usemetaswap=.true.
!                ! set working directory
!                call cfn_vcl_set(record,jvcl)
!                call cfn_vcl_fndc(jvcl,jarg,'-wd',.true.,wd,1)
!                if (jarg.gt.0) simwd1 = trim(simwd1)//wd
!             case( 'TRANSOL' )
!                usetransol=.true.
!#endif
!#ifdef INCLUDE_MOZART
!             case( 'MOZART' )
!                mozrecord=record
!                usemozart=.true.
!                ! set working directory
!                call cfn_vcl_set(mozrecord,jvcl)
!                call cfn_vcl_fndc(jvcl,jarg,'-wd',.true.,wd,1)
!                if (jarg.gt.0) mozwd = trim(mozwd)//wd
!                ! get the river subsystems for coupling
!                call cfn_vcl_fndi(jvcl,jarg,'-nhriv*sys',.true.,m,1)
!                if (m(1).gt.0) then
!                   nhrivsys = m(1)
!                   allocate(hrivsys(nhrivsys))
!                else
!                   call driverChk(.false.,'Error: -nhriv*sys not found.')
!                end if
!                call cfn_vcl_fndi(jvcl,jarg,'-hriv*sys',.true.,hrivsys,nhrivsys)
!                call cfn_vcl_fndi(jvcl,jarg,'-nwriv*sys',.true.,m,1)
!                if (m(1).gt.0) then
!                   nwrivsys = m(1)
!                   allocate(wrivsys(nwrivsys))
!                else
!                   call driverChk(.false.,'-nwriv*sys not found.')
!                end if
!                call cfn_vcl_fndi(jvcl,jarg,'-wriv*sys',.true.,wrivsys,nwrivsys)
!             case( 'STS' )
!                usests = .true.
!#endif
!             case default
!                ! ERROR, component unknown
!                write(*,*) ' Warning: component ',compcls(1:cfn_length(compcls)),' unknown.'
!
!          end select
!
!          ! get next component
!          call cfn_getrec(lunc,record,comm,cont)
!       enddo
!
!       ! close components file
!       close(lunc)
!    else
!       call driverChk(.false.,'Could not open component description file '//compfile(1:cfn_length(compfile)))
!       call exit(1)
!    endif
! endif
!
! rt = driverGetRunType(usemodflow,usemetaswap,usetransol,usemozart,usests,useribasim)
! write(*,'(50(''*''),/,2(1x,a),/,50(''*''))') 'Running mode:', trim(rtdesc(rt))
!
!! check state-save options
! if (usests.and..not.usestsmodflow) call driverChk(.false.,'MODFLOW state-save not activated.')
! if (.not.usests.and.usestsmodflow) call driverChk(.false.,'MODFLOW state-save activated.')
!
! nnsub = max(1,nsub)   
! 
! if (stsOverRule) then
!    usests = .true.
!    usestsmodflow = .true.
! end if    
!    
!end subroutine mf_init
!
!subroutine mf_get_current_time(timeValue,retVal)
!!DEC$ ATTRIBUTES DLLEXPORT :: mf_get_current_time
!    ! arguments
!    double precision, intent(out)   :: timeValue      !> start time of current time step
!    integer         , intent(out)   :: retVal         !> return code   0: OK
!    call mf2005_getCurrentTime(timeValue,retVal)
!end subroutine mf_get_current_time
!
!subroutine mf_init_iter(stssaveOverRule)
!!DEC$ ATTRIBUTES DLLEXPORT :: mf_init_iter
! use mf_module
!
! implicit none
! 
! logical, intent(in) :: stssaveOverRule
! 
! integer :: idum1, idum2 !!!!!!!!!!!! WORKAROUND !!!!!!!!!!!!!!
!    
! tsc=tsc+1
! timestep = timestep + 1
!
! call cfn_mjd2datehms(currentTime,date,hour,minute,second)
!
!!what about steady-state solutions, currenttime.eq.0
! write(*,'(5x,a,1x,i5,1x,a,1x,i8,3(a,i2.2))')&
!    'Timestep     ',tsc,':',date,' ',abs(hour),':',minute,':',second
!
! ! one timestep for each cycle
!
!!##### BEGIN EXCHANGE: BeginOfTimestep ########################################
! if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
!    ok = mf2005_PutHeads(mf_igrid,XchModSimModCells,XchModSimModNID,XchModSimModHeads,mv); call DriverChk(ok,'mf2005_PutHeads') ! Put groundwater heads
!    ok = metaswap_GetHeads(XchModSimModHeads,XchModSimModNID,XchMod2SimIdx,XchMod2SimOff,mv); call DriverChk(ok,'metaswap_GetHeads') ! Get groundwater heads
!    XchModSimModHeads = mv
! end if
!!####### END EXCHANGE: BeginOfTimestep ########################################
!
! ! perform state save, phase 1 (before reading and writing data)
! if (usests) then
!    if (stssaveOverRule) stsave= .true.
!    if (mozstsave) stsave = .true.
!    call osd_chdir(modwd2)
!    if (usestsmodflow) call sts2saverestore(currentTime,stsave,strestore,1)
!    if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
!       call osd_chdir(simwd2)
!       call MetaSWAP_saveRestore(stsave,strestore)
!       !call TRANSOL_saveRestore(stsave,strestore)
!    end if
! end if
!
!!  ... init timestep
! call mf2005_prepareTimestep(currentTime,stsave,retValMF2005,idum1,idum2)
! call osd_chdir(modwd2)
! call mf2005_initTimeStep(currentTime,retValMF2005)
! if (retValMF2005.ne.0) exitcode = -15
!
! ! get end of current timestep
! !!!!!!!!!!!!!!!! Only modflow information is used !!!!!!!!!!!!!!!!!!!!!!!
! call mf2005_getEndOfCurrentTimeStep(endOfCurrentTimeStepMF2005,retValMF2005)
! if (retValMF2005.ne.0) exitcode = -17
! endOfCurrentTimeStep = endOfCurrentTimeStepMF2005
!
! ! extra check
! if (endOfCurrentTimeStep.eq.nodataTime) call driverChk(.false.,'endOfCurrentTimeStep = nodataTime')
!
! ! read timestep data
! if (exitcode.eq.0) then
!    if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
!       dtMetaSwap = endOfCurrentTimeStep - currentTime
!       call osd_chdir(simwd2)
!       call MetaSwap_initTimestep(dtMetaSwap)
!       iterMetaSwap = 0
!    end if
! end if
!
!end subroutine mf_init_iter         
!              
!subroutine mf_iter(converged)
!!DEC$ ATTRIBUTES DLLEXPORT :: mf_iter
! use mf_module             
!           
! implicit none
! 
! logical, intent(inout) :: converged     
!    
! if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
!    call MetaSwap_prepareIter(iterMetaSwap)  ! only action: iter=iter+1
!    call MetaSwap_performIter(iterMetaSwap)
!!##### BEGIN EXCHANGE: AfterSolveMetaSwap #####################################
!    ok = metaswap_PutModSimUnsaturatedZoneFlux(XchModSimSimUnsZFlux,mv); call DriverChk(ok,'metaswap_PutUnsaturatedZoneFlux') ! Put unsaturated zone flux
!    ok = mf2005_GetUnsaturatedZoneFlux(mf_igrid,&
!            XchModSimModNID,XchModSimSimUnsZFlux,XchSim2ModIdx,XchSim2ModOff,mv); call DriverChk(ok,'mf2005_GetUnsaturatedZoneFlux') ! Get unsaturated zone flux
!    XchModSimSimUnsZFlux = mv
!    if (.not.mf_steadystate) then
!       ok = metaswap_PutStorageFactor(XchModSimSimStrFct,mv); call DriverChk(ok,'metaswap_PutStorageFactor') ! Put storage factor
!       if (.not.llpf) then ! BCF
!          ok = mf2005_GetStorageFactor(mf_igrid,XchModSimSimStrFct,&
!                                      XchModSimModNID,XchSim2ModIdx,XchSim2ModOff,mv); call DriverChk(ok,'mf2005_GetStorageFactor') ! Get storage factor
!       else ! LPF
!          ok = mf2005_GetStorageFactorLPF(mf_igrid,XchModSimSimStrFct,&
!                                      XchModSimModNID,XchSim2ModIdx,XchSim2ModOff,mv); call DriverChk(ok,'mf2005_GetStorageFactorLPF') ! Get storage factor
!       end if
!       XchModSimSimStrFct = mv
!    end if
! end if
!!####### END EXCHANGE: AfterSolveMetaSwap #####################################
! call mf2005_performIter(retValMF2005)
! if (retValMF2005.ne.0) exitcode = -26
!
!!##### BEGIN EXCHANGE: AfterSolve #############################################
! if (exitcode.eq.0) then
!    if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
!       ok = mf2005_PutHeads(mf_igrid,XchModSimModCells,XchModSimModNID,XchModSimModHeads,mv); call DriverChk(ok,'mf2005_PutHeads') ! MODFLOW put groundwater heads
!       ok = metaswap_GetHeads(XchModSimModHeads,XchModSimModNID,XchMod2SimIdx,XchMod2SimOff,mv); call DriverChk(ok,'metaswap_GetHeads') ! MetaSWAP gets the groundwater heads
!       XchModSimModHeads = mv
!    end if
! end if
!!####### END EXCHANGE: AfterSolve #############################################
!
! ! finish iteration
! if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz)&
! call MetaSwap_finishIter(iterMetaSwap,convergedMetaSwap)
! call mf2005_finishIter(convergedMF2005,retValMF2005)
! if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz)&
!    write(*,*) ' convergence MODFLOW - MetaSWAP: ',convergedMF2005, convergedMetaSwap
!
! ! get next iteration information
! converged = .true.
! converged = converged .and. convergedMF2005
! if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz)&
!    converged = converged .and. convergedMetaSwap
!
! if (retValMF2005.ne.0    ) exitcode = -261
! 
! if (exitcode.ne.0) converged = .true.
!            
!end subroutine mf_iter         
!          
!subroutine mf_finish_timestep(converged,endOfSimulation,doTimeLoop)
!!DEC$ ATTRIBUTES DLLEXPORT :: mf_finish_timestep
! use mf_module                   
!       
! implicit none
! 
! logical, intent(in) :: converged
! logical, intent(inout) :: endOfSimulation,doTimeLoop 
! 
! if (.not.converged) write(*,*) ' Model did not converge!',exitcode
!
!! ... write results
! if (converged .and. exitcode.eq.0) then
!    call osd_chdir(modwd2)
!    call mf2005_finishTimestep(retValMF2005)
!    if (retValMF2005.ne.0) exitcode = -31
!
!!##### BEGIN EXCHANGE: AfterFinishTimeStepMODFLOW #############################
!    if (rt.eq.rtmodsimtranmoz) then
!
!       ok = mf2005_PutSeepageFlux(mf_igrid,XchModTranModSeepageFlux,XchModTranModCells,XchModTranModNID,mv,.true.); call DriverChk(ok,'mf2005_PutSeepageFlux') ! MODFLOW puts the seepage flux for TRANSOL
!       ok = mf2005_PutRiverFlux(mf_igrid,XchModTranModRiverFlux,XchModTranModCells,XchModTranModNID,mv,&
!                                nhrivsys,hrivsys,nwrivsys,wrivsys,.true.,.false.); call DriverChk(ok,'mf2005_PutRiverFlux') ! MODFLOW puts the river flux for TRANSOL: skip H and W [m]
!       ok = mf2005_PutRiverFlux(mf_igrid,XchModMozModRiverFlux,XchModMozModCells,XchModMozModNID,mv,&
!                                nhrivsys,hrivsys,nwrivsys,wrivsys,.false.,.false.); call DriverChk(ok,'mf2005_PutRiverFlux') ! MODFLOW puts the river flux for MOZART: skip H and W [m3]
!       ok = mf2005_PutRiverFlux(mf_igrid,XchModMozModRiverFluxWells,XchModMozModCells,XchModMozModNID,mv,&
!                                nhrivsys,hrivsys,nwrivsys,wrivsys,.false.,.true.); call DriverChk(ok,'mf2005_PutRiverFlux') ! MODFLOW puts the river flux for MOZART ("wellen"): skip H; only W; [m3]
!       ok = mf2005_PutDrainFlux(mf_igrid,XchModTranModDrainFlux,XchModTranModCells,XchModTranModNID,mv,.true.); call DriverChk(ok,'mf2005_PutDrainFlux') ! MODFLOW puts the drain flux for TRANSOL: don't skip; all layers; [m]
!       ok = mf2005_PutDrainFlux(mf_igrid,XchModMozModDrainFlux,XchModMozModCells,XchModMozModNID,mv,.false.); call DriverChk(ok,'mf2005_PutDrainFlux') ! MODFLOW puts the drain flux for MOZART; don't skip; all layers; [m3]
!       ok = mf2005_PutSaltFlux(mf_igrid,XchModMozModSalt,XchModMozModCells,XchModMozModNID,mv,nwrivsys,wrivsys); call DriverChk(ok,'mf2005_PutSaltFlux') ! MODFLOW puts the salt flux for MOZART
!       ok = TRANSOL_GetSeepageRiverDrainFlux(XchModTranModSeepageFlux,XchModTranModNID,XchMod2TranIdx,XchMod2TranOff,mv,'see'); call DriverChk(ok,'TRANSOL_GetSeepageRiverDrainFlux') ! TRANSOL gets the seepage flux
!       XchModTranModSeepageFlux = mv
!       ok = TRANSOL_GetSeepageRiverDrainFlux(XchModTranModRiverFlux,XchModTranModNID,XchMod2TranIdx,XchMod2TranOff,mv,'riv'); call DriverChk(ok,'TRANSOL_GetSeepageRiverDrainFlux') ! TRANSOL gets the river flux
!       ok = TRANSOL_GetSeepageRiverDrainFlux(XchModTranModDrainFlux,XchModTranModNID,XchMod2TranIdx,XchMod2TranOff,mv,'drn'); call DriverChk(ok,'TRANSOL_GetSeepageRiverDrainFlux') ! TRANSOL gets the drain flux
!       XchModTranModDrainFlux = mv
!       ok = MOZART_GetRiverDrainFlux(XchModMozModRiverFlux,XchMozNID,XchMod2MozIdx,XchMod2MozOff,mv,1); call DriverChk(ok,'MOZART_GetRiverDrainFlux') ! MOZART gets the river flux
!       XchModMozModRiverFlux = mv
!       ok = MOZART_GetRiverDrainFlux(XchModMozModRiverFluxWells,XchMozNID,XchMod2MozIdx,XchMod2MozOff,mv,2); call DriverChk(ok,'MOZART_GetRiverDrainFlux') ! MOZART gets the river flux (wells)
!       XchModMozModRiverFluxWells = mv
!       ok = MOZART_GetRiverDrainFlux(XchModMozModDrainFlux,XchMozNID,XchMod2MozIdx,XchMod2MozOff,mv,1); call DriverChk(ok,'MOZART_GetRiverDrainFlux') ! MOZART gets the drain flux
!       XchModMozModDrainFlux = mv
!       ok = MOZART_GetSalt(XchModMozModSalt,XchMozNID,XchMod2MozIdx,XchMod2MozOff,mv,2); call DriverChk(ok,'MOZART_GetSalt') ! MOZART get the salt flux
!       XchModMozModSalt = mv
!    end if
!!##### BEGIN EXCHANGE: AfterFinishTimeStepMODFLOW #############################
!    if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
!       call osd_chdir(simwd2)
!       call MetaSwap_finishTimestep(currentTime)
!    end if
!
!    !#### TIMESERIES #####
!    ok = mf2005_TimeserieGetHead(mf_igrid); call DriverChk(ok,'mf2005_TimeserieGetHead')
!    call tserie1write(0,lss,currentTime,hnoflo,usests)
!    !#### TIMESERIES #####
!
! endif
!
! if (exitcode.eq.0) then
!    ! perform state save, phase 2 (after reading and writing data)
!    if (usestsmodflow) call sts2saverestore(currentTime,stsave,strestore,2)
!    if (mozstsave) then
!       stsave    = .false.
!       strestore = .false.
!       mozstsave = .false.
!    end if
! endif
!
!! ... next timestep
! if (exitcode.ne.0) then
!    ! ERROR, do not continue
!    endOfSimulation=.true.
! else
!
!    ! get new time information
!    ! Let Modflow be leading. don't ask the other components
!    call mf2005_getBeginOfNextTimeStep(BeginOfNextTimeStepMF2005,retValMF2005)
!    ! find errors
!    if (retValMF2005.eq.-1) then
!       ! end of simulation for Modflow
!       endOfSimulation=.true.
!       retValMF2005   =0
!    endif
!    if (retValMF2005.ne.0) exitcode = -31
!    if (exitcode.ne.0)     endOfSimulation=.true.
!    ! find start time of next timestep
!    !   this may be a time in the 'future', the 'past' or the 'current' time
!    ! nodata value of time-value means: this was the last timestep for this component
!    if (exitcode.eq.0) then
!
!       currentTime=nodataTime
!
!       ! check for each component (ok, only modflow available now)
!       if (BeginOfNextTimeStepMF2005.ne.nodataTime) then
!          if (currentTime.eq.nodataTime) then
!             currentTime=BeginOfNextTimeStepMF2005
!          else
!             currentTime=min(currentTime,BeginOfNextTimeStepMF2005)
!          endif
!       endif
!
!       if (currentTime.eq.nodataTime.and.rt.ne.rtmod) endOfSimulation=.true.
!!       if (currentTime.eq.nodataTime) endOfSimulation=.true.
!    endif
! endif
!
! if (rt.eq.rtmodsimtranmoz) then
!    doTimeLoop = currenttime.lt.endOfCurrentTimeStepMozart .and. .not.endOfSimulation
! else
!    doTimeLoop = .not.endOfSimulation
! end if
!
! if (exitcode.ne.0) doTimeLoop = .false.
!   
!end subroutine mf_finish_timestep
!
!subroutine mf_init_simulation(endOfSimulation,convergedMozart,start_time)
! !DEC$ ATTRIBUTES DLLEXPORT :: mf_init_simulation
! use mf_module
! use mod_pest, only: pest1_meteo_metaswap, pest1alpha_metaswap, pest1appendlogfile
! implicit none
! 
! logical, intent(inout) :: endOfSimulation,convergedMozart
! double precision, intent(in) :: start_time
! 
!if (lrunfile) then
!    if (nsub.gt.0) then
!       write(modwd2,'(4a)') trim(modwd1),'\',trim(submstr(isub)),'\modflow\mf2005_tmp'
!       if (usemetaswap) then
!          write(simwd2,'(4a)') trim(simwd1),'\',trim(submstr(isub)),'\metaswap'
!       end if
!       write(*,'(50(''+''),/,1x,a,1x,a,1x,a,i3.3,a,i3.3,a,/,50(''+''))') 'Computing for submodel:',trim(submstr(isub)),'(',isub,'/',nsub,')'
!    else
!       write(modwd2,'(2a)') trim(modwd1),'\modflow\mf2005_tmp'
!       if (usemetaswap) then
!          write(simwd2,'(2a)') trim(simwd1),'\metaswap'
!       end if
!    end if
! else
!    modwd2 = modwd1
!    simwd2 = simwd1
! end if
!
! timestep    = 1
! stsave      = .false.
! strestore   = .false.
!
! ! init return values
! retValMF2005      = 0
!
! ! time values
! currentTime       = nodataTime
! currentTimeMF2005 = 0.d0
!
! ! convergence
! convergedMF2005   = .true.
! convergedMetaSwap = .true.
!
! ! init all components TODO!!!!!!!
! if (usemodflow) then
!    call osd_chdir(modwd2)
!    if (usestsmodflow) call sts2init(usestsmodflow,lunsts)
!    call mf2005_initComponent(modrecord,retValMF2005)
!    ! get number of grids
!    ok = mf2005_PutNumberOfGrids(mf_ngrid); call driverChk(ok,'mf2005_PutNumberOfGrids')
!    if (mf_ngrid.ne.1) call driverChk(.false.,'More than one MODFLOW grids is not supported.'); mf_igrid = 1
!    if (lipest) then
!       ok = mf2005_GetPestFlag(lipest); call driverChk(ok,'mf2005_GetPestFlag')
!    end if
! end if
! if (usemetaswap) then
!    call osd_chdir(simwd2)
!    if (lipest) then
!       mf_nrow = 0; mf_ncol = 0; lpwt = .false.
!       ok = mf2005_PutGridDimensions(mf_igrid, mf_nrow, mf_ncol, mf_nlay); call driverChk(ok,'mf2005_PutGridDimensions')
!       ok = mf2005_PutPWTActive(mf_igrid, lpwt); call driverChk(ok,'mf2005_PutPWTActive')
!       call pest1_meteo_metaswap()
!       call pest1alpha_metaswap(mf_nrow,mf_ncol,lpwt)
!    end if
!    call MetaSwap_initComponent()
! end if
! if (usetransol) call TRANSOL_initComponent()
! if (usemozart) then
!    call osd_chdir(mozwd)
!    call mozart_initComponent(mozrecord)
! end if
!
! ! append the PEST log-file
! if (lipest) then
!    call pest1appendlogfile(modwd1)
!    call pest1log()
!    CALL PEST1CLOSELOGFILES()
! end if
!
! ! check if MODFLOW is activated
! if (retValMF2005.ne.0) call driverChk(.false.,'MODFLOW initialization failed.')
! 
! write(*,*) '--> start_time=',start_time
! if (start_time > -0.01d0) then
!    ! outside world provides starting date/time
!    call mf2005_setCurrentTime(start_time,retValMF2005)
!    currentTime = start_time
! else
!    ! get starting date/time from modflow
!    call mf2005_getCurrentTime(currentTimeMF2005,retValMF2005)
!    currentTime = currentTimeMF2005
! endif
!
!! start sts
! if (usestsmodflow) call sts2start(currentTime)
!
!! read data for entire simulation
! if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
!    call osd_chdir(simwd2)
!    call metaswap_initSimulation(currentTime)
! end if
! call osd_chdir(modwd2)
! call mf2005_initSimulation(currentTime,retValMF2005)
! if (retValMF2005.ne.0) then
!    exitcode = -12
!    return
! endif
! 
!! #### BEGIN EXCHANGE: BeforeInitSimulationMozart #############################
!
! ok = mf2005_PutLPFActive(mf_igrid, llpf); call driverChk(ok,'mf2005_PutLPFActive') ! get flag if BCF is used or LPF
! ok = mf2005_PutSimulationType(mf_igrid, lss); call driverChk(ok,'mf2005_PutSimulationType')  ! get flag if simulation is steady-state or transient
! ok = mf2005_PutHeadNoFlo(mf_igrid, hnoflo);  call driverChk(ok,'mf2005_PutHeadNoFlo')  ! get hnoflo
!
! !#### TIMESERIES ####
! call tserie1init1(lipest,lss,hnoflo)
! ok = mf2005_TimeserieInit(mf_igrid); call driverChk(ok,'mf2005_TimeserieInit')
! call tserie1init2(lipest,lss,hnoflo)
!
! if (rt.eq.rtmodsimtranmoz) then
!    call osd_chdir(mozwd)
!    call mozart_initSimulation()
!    convergedMozart = .false.
!    endOfSimulation = .false.
!    call mozart_prepareTimeStep(endOfSimulation,convergedMozart,currentTime) ! read signal file
! end if
!
!! #### BEGIN EXCHANGE: BeforeTimestep #########################################
! if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
!    ok = mf2005_PutSimulationType(mf_igrid, mf_steadystate); call driverChk(ok,'mf2005_PutSimulationType') ! MODFLOW puts the simulation type (transient/steady-state)
!    if (mf_steadystate) call driverChk(ok,'MODFLOW may not be steady-state')
! end if
! ! Coupling MODFLOW-MetaSWAP
! if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
!    ok = mf2005_PutModSimNumberOfIDs(mf_igrid, XchModSimModNID); call driverChk(ok,'mf2005_PutModSimNumberOfIDs') ! MODFLOW puts the number of MOD-SIM IDs
!    ok = metaswap_PutModSimNumberOfIDs(XchModSimSimNID); call driverChk(ok,'metaswap_PutModSimNumberOfIDs')       ! MetaSWAP puts the number of MOD-SIM IDs
!    call driverXchInitModSim() ! allocate and init
!
!    ok = mf2005_PutModSimIDs(mf_igrid,XchModSimModIds); call driverChk(ok,'mf2005_PutModSimIDs')       ! MODFLOW puts the MOD-SIM exchange IDs
!    ok = mf2005_PutModSimCells(mf_igrid,XchModSimModCells); call driverChk(ok,'mf2005_PutModSimCells') ! MODFLOW puts the MOD-SIM cells
!    ok = metaswap_PutModSimIDs(XchModSimSimIds); call driverChk(ok,'metaswap_PutModSimIDs')            ! get exchange id's
! end if
! ! Coupling MODFLOW-MOZART/LSW, MODFLOW-MOZART/PV, MetaSWAP-MOZART/LSW
! if (rt.eq.rtmodsimtranmoz) then
!    ! Main program puts the river systems to skip in the coupling
!    ok = mf2005_PutModMozRiversToSkip(mf_igrid,nhrivsys,hrivsys); call driverChk(ok,'mf2005_PutModMozRiversToSkip')     ! Main program puts the river systems to skip in the coupling
!    ok = mozart_PutModMozNumberOfIDs(XchMozNID); call driverChk(ok,'mozart_PutModMozNumberOfIDs')                       ! MOZART *puts* the number of LSW IDs
!    ok = mozart_PutModMozPVNumberOfIDs(XchMozPVNID); call driverChk(ok,'mozart_PutModMozPVNumberOfIDs')                 ! MOZART *puts* the number of PV IDs
!    ok = mf2005_PutModMozNumberOfIDs(mf_igrid, XchModMozModNID); call driverChk(ok,'mf2005_PutModMozNumberOfIDs')       ! MODFLOW *puts* the number of LSW IDs
!    ok = mf2005_PutModMozPVNumberOfIDs(mf_igrid, XchModMozPVModNID); call driverChk(ok,'mf2005_PutModMozPVNumberOfIDs') ! MODFLOW *puts* the number of PV IDs
!    ok = metaswap_PutModMozNumberOfIDs(XchSimMozSimNID); call driverChk(ok,'metaswap_PutModMozNumberOfIDs')             ! MetaSWAP *puts* the number of LSW IDs
!
!    call driverXchInitModSimTranMoz() ! allocate and init
!
!    ok = mozart_PutModMozIDs(XchMozIds,XchMozNID); call driverChk(ok,'mozart_PutModMozIDs')             ! MOZART *puts* the LSW IDs
!    ok = mozart_PutModMozPVIDs(XchMozPVIds,XchMozPVNID); call driverChk(ok,'mozart_PutModMozPVIDs')     ! MOZART *puts* the PV IDs
!    ok = mf2005_PutModMozIDs(mf_igrid, XchModMozModIds); call driverChk(ok,'mf2005_PutModMozIDs')       ! MODFLOW *puts* the LSW IDs
!    ok = mf2005_PutModMozPVIDs(mf_igrid, XchModMozPVModIds); call driverChk(ok,'mf2005_PutModMozPVIDs') ! MODFLOW *puts* the PV IDs
!    ok = mf2005_PutModMozCells(mf_igrid,XchModMozModCells); call driverChk(ok,'mf2005_PutModMozCells')  ! MODFLOW puts the MOD-MOZ cells
!    ok = metaswap_PutModMozIDs(XchSimMozSimIds); call driverChk(ok,'metaswap_PutModMozIDs')             ! MetaSWAP *puts* the LSW IDs
! end if
!
! ! Create mappings
! if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) call driverXchIniMapModSim()
! if (rt.eq.rtmodsimtranmoz) then
!    call driverXchIniMapModMoz()
!    call driverXchIniMapModMozPV()
!    call driverXchIniMapSimMoz()
!    call driverXchIniMapModTran()
!    call driverXchIniMapTranMoz()
! end if
!
!! ###### END EXCHANGE: BeforeTimestep #########################################
!
!! ... timestep
! timestep = 0
!
! tsc=0 ! debug: TimeStepCycle
!
! stsave    = .false.
! mozstsave = .false.
! strestore = .false.
! 
!end subroutine mf_init_simulation
!
!subroutine mf_init_mozart
! !DEC$ ATTRIBUTES DLLEXPORT :: mf_init_mozart
! use mf_module
! implicit none
! iterMozart = 0.
!end subroutine mf_init_mozart
!
!subroutine mf_init_timestep
! !DEC$ ATTRIBUTES DLLEXPORT :: mf_init_timestep
! use mf_module
! 
! implicit none
! 
! if (rt.eq.rtmodsimtranmoz) then
!    iterMozart = iterMozart + 1
!    if (iterMozart.eq.1) mozstsave = .true.
!    call mozart_getCurrentTime(currentTimeMozart) ! get the current time
!    currentTime = currentTimeMozart
!    call mozart_getEndOfCurrentTimeStep(endOfCurrentTimeStepMozart) ! get the end of the currentMozart time step
!    call osd_chdir(mozwd)
!    call mozart_initTimeStep(iterMozart)  ! initialize time step
!
!!BEGIN EXCHANGE: BeginOfMozartTimestep ##################################
!
!    ok = mozart_PutLSWLevels(XchModMozMozLevels,mv); call DriverChk(ok,'mozart_PutLSWLevels') ! MOZART puts the LSW levels
!    ok = mozart_PutPVLevels(XchModMozPVMozPVLevels,mv); call DriverChk(ok,'mozart_PutPVLevels') ! MOZART puts the PV levels
!    ok = mozart_PutLSWFractions(XchSimMozMozFractions,mv); call DriverChk(ok,'mozart_PutLSWFractions') ! MOZART puts the LSW fractions
!    ok = mf2005_GetLSWLevels(mf_igrid,XchModMozMozLevels,&
!                             XchModMozModNID,XchMoz2ModIdx,XchMoz2ModOff,mv); call DriverChk(ok,'mf2005_GetLSWLevels') ! MODFLOW gets LSW levels
!    XchModMozMozLevels = mv
!    ok = mf2005_GetPVLevels(mf_igrid,XchModMozPVMozPVLevels,&
!                            XchModMozPVModNID,XchMozPV2ModIdx,XchMozPV2ModOff,mv); call DriverChk(ok,'mf2005_GetPVLevels') ! MODFLOW gets PV levels
!    XchModMozPVMozPVLevels = mv
!    ok = metaswap_GetFractions(XchSimMozMozFractions,XchSimMozSimNID,XchMoz2SimIdx,XchMoz2ModOff,mv); call DriverChk(ok,'metaswap_GetFractions') ! MetaSWAP gets the LSW fractions
!    XchSimMozMozFractions = mv
!!# END EXCHANGE: BeginOfMozartTimestep ##################################
! end if
!  
!end subroutine mf_init_timestep
!
!subroutine mf_finish_simulation(endOfSimulation,convergedMozart)
! !DEC$ ATTRIBUTES DLLEXPORT :: mf_finish_simulation
! use mf_module
! 
! implicit none
!
! logical, intent(out) :: endOfSimulation,convergedMozart
!
!!##### BEGIN EXCHANGE: BeforeTRANSOLIteration #################################
! if (rt.eq.rtmodsimtranmoz .and. exitcode.eq.0) then
!    ok = mozart_PutLSWSalt(XchTranMozMozSalt,mv); call DriverChk(ok,'Error mozart_PutLSWSalt') ! MOZART puts the LSW salt concentrations
!    ok = TRANSOL_GetSalt(XchTranMozMozSalt,&
!                         XchSimMozSimNID,XchMoz2SimIdx,XchMoz2ModOff,mv); call DriverChk(ok,'Error TRANSOL_GetSalt') ! TRANSOL gets the LSW salt concentrations
!    XchTranMozMozSalt = mv
! end if
!
!!####### END EXCHANGE: BeforeTRANSOLIteration #################################
!
! if (rt.eq.rtmodsimtranmoz .and. exitcode.eq.0) then
!    call TRANSOL_performIter()
! end if
!!##### BEGIN EXCHANGE: AfterMozartIteration ###################################
! if (rt.eq.rtmodsimtranmoz .and. exitcode.eq.0) then
!    ok = MetaSWAP_PutCumSWSprinklingDemandFluxes(XchSimMozSimCuSWSprinklingFlux,mv); call DriverChk(ok,'MetaSWAP_PutCumSWSprinklingDemandFluxes') ! MetaSWAP puts the cumulative surface water sprinkling demand fluxes
!    ok = MetaSWAP_PutCumRunonFluxes(XchSimMozSimCuRunonFlux,mv); call DriverChk(ok,'MetaSWAP_PutCumRunonFluxes') ! MetaSWAP puts the cumulative runon fluxes
!    ok = TRANSOL_PutSalt(XchTranMozTranSalt,mv); call DriverChk(ok,'TRANSOL_PutSalt') ! TRANSOL puts the concentrations
!    ok = MOZART_GetCumSWSprinklingDemandFluxes(XchSimMozSimCuSWSprinklingFlux,&
!                                            XchMozNID,XchSim2MozIdx,XchSim2MozOff,mv); call DriverChk(ok,'MOZART_GetCumSWSprinklingDemandFluxes') ! MOZART gets the surface water sprinkling demand fluxes
!    XchSimMozSimCuSWSprinklingFlux = mv
!    ok = MOZART_GetCumRunonFluxes(XchSimMozSimCuRunonFlux,&
!                                  XchMozNID,XchSim2MozIdx,XchSim2MozOff,mv); call DriverChk(ok,'MOZART_GetCumRunonFluxes') ! MOZART gets the cumulative runon fluxes
!    ok = MOZART_GetSalt(XchTranMozTranSalt,&
!                        XchMozNID,XchTran2MozIdx,XchTran2MozOff,mv,1); call DriverChk(ok,'MOZART_GetSalt') ! MOZART gets the concentrations
!    XchTranMozTranSalt = mv
! end if
!!####### END EXCHANGE: AfterMozartIteration ###################################
!
! if (rt.eq.rtmodsimtranmoz .and. exitcode.eq.0) then
!    call TRANSOL_finishTimeStep(currentTime)
!    dtMozart = endOfCurrentTimeStepMozart - currentTimeMozart
!    call osd_chdir(mozwd)
!    call mozart_finishTimeStep(iterMozart,dtMozart,usetransol)
!    call mozart_prepareTimeStep(endOfSimulation,convergedMozart,currentTime) ! read signal file
! end if
!
! ! make sure MODFLOW-MetaSWAP terminates normally
! if (rt.eq.rtmodsim) then
!    convergedMozart = .true.
! end if
! 
! if (exitcode.ne.0) endOfSimulation = .True.
! 
!end subroutine mf_finish_simulation
!
!subroutine mf_finish_pest(convergedPest)
! !DEC$ ATTRIBUTES DLLEXPORT :: mf_finish_pest
! use mf_module
! use imod_utl, only: imod_utl_closeunits
! use mod_pest, only: pestnext
! 
! implicit none
! 
! logical, intent(inout) :: convergedPest
! 
!! ... end
! call osd_chdir(modwd2)
! if (exitcode.eq.0)  call mf2005_finishSimulation(retValMF2005)
! if (retValMF2005.ne.0 ) exitcode = -61
! if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
!    call osd_chdir(simwd2)
!    call MetaSWAP_finishSimulation()
! end if
!!#### TIMESERIES #####
! call tserie1write(1,lss,currentTime,hnoflo,usests)
! call tserie1close()
!!#### TIMESERIES #####
!
!! list routine timer info
! call cfn_rtt_list(0)
!
!! Deallocate coupling arrays
! call driverXchDeallocate()
!
!! exit
! if (exitcode.ne.0) then
!    write(unit=*,fmt=*) 'ERROR, exit code: ',exitcode
!    call exit(10)
! endif
!
!! next pest iteration
! call imod_utl_closeunits()
! if (.not.lipest) then
!    convergedPest=.true.
! else
!  convergedPest=pestnext(lss,modwd1)
! end if
! 
!end subroutine mf_finish_pest
!
!subroutine mf_set_timestep_done(done_with_current_timestep, Reset_time)
!!DEC$ ATTRIBUTES DLLEXPORT :: mf_set_timestep_done
! use mf_module
! implicit none
! double precision Reset_Time,LastSavedTime
! logical done_with_current_timestep
! if (.not. done_with_current_timestep) then
!     ! jump back to begin of time step
!     currenttime = Reset_time
! else
!     ! time step done
! endif
!end subroutine mf_set_timestep_done

!use driver_module 

!!!
!!! DLL-methods for coupling with WFLOW
!!!

function mf_init_no_sub_pest_moz(config_file_name, eOS, startTime,stsOverRule) result(ret_val)
!DEC$ ATTRIBUTES DLLEXPORT :: mf_init_no_sub_pest_moz
 use driver_module
 implicit none
 integer              :: ret_val
 character(len=*)     :: config_file_name
 logical, intent(out) :: eOS
 logical, intent(in) :: stsOverRule
 double precision, intent(in) :: startTime
 logical              :: dummyConvergedMozart
 integer :: dummynnsub
 logical :: rcmd_line

 rcmd_line = .false. 
 call driver_init(config_file_name,dummynnsub,rcmd_line,stsOverRule)
 if (exitcode == 0) then
     eOS   = .false.
     call driver_init_simulation(eOS,dummyConvergedMozart,startTime)
 endif
 ret_val = exitcode

end function  mf_init_no_sub_pest_moz

subroutine mf_update_no_sub_pest_moz(eOS, dTL, sOR) !# endOfSimulation=eOS, doTimeLoop=dTL, stssaveOverRule=sOR
!DEC$ ATTRIBUTES DLLEXPORT :: mf_update_no_sub_pest_moz
 use driver_module
 implicit none
 logical, intent(inout) :: eOS
 logical, intent(inout) :: dTL
 logical, intent(in) :: sOR
 
 logical :: convergedIter
 call driver_init_iter(sOR)
 convergedIter = .false.
 do while(.not.convergedIter) ! Picard iter loop
    call driver_iter(convergedIter)    
 end do
 call driver_finish_timestep(convergedIter,eOS,dTL)
end subroutine mf_update_no_sub_pest_moz

subroutine mf_finish_no_sub_pest_moz()
!DEC$ ATTRIBUTES DLLEXPORT :: mf_finish_no_sub_pest_moz
 use driver_module
 implicit none
 logical :: dummyConvergedPest
 call driver_finish_pest(dummyConvergedPest)
end subroutine mf_finish_no_sub_pest_moz

subroutine mf_get_grid_sizes(nlay, nrow, ncol)
!DEC$ ATTRIBUTES DLLEXPORT :: mf_get_grid_sizes
use driver_module
 implicit none
 integer, intent(out) :: nlay, nrow, ncol
 integer :: igrid = 1
 ok = mf2005_PutGridDimensions(igrid,nrow,ncol,nlay)  
end subroutine mf_get_grid_sizes
    
subroutine mf_set_recharge(recharge, ncol, nrow)
!DEC$ ATTRIBUTES DLLEXPORT :: mf_set_recharge
use driver_module
  implicit none
  integer, intent(in)                        :: ncol, nrow
  real   , intent(in), dimension(ncol, nrow) :: recharge
  integer :: igrid = 1
  ok = mf2005_GetRecharge(recharge,ncol,nrow,igrid)
end subroutine mf_set_recharge
    
subroutine mf_get_heads(head,ncol,nrow)
!DEC$ ATTRIBUTES DLLEXPORT :: mf_get_heads
  use driver_module 
  implicit none
  integer, intent(in)                                 :: ncol,nrow
  double precision, intent(out), dimension(ncol,nrow) :: head

  integer :: iilay = 1
  integer :: iigrid = 1
  
  ok = mf2005_PutHeadsForLayer(head,ncol,nrow,iilay,iigrid)
  
end subroutine mf_get_heads


subroutine get_currenttime(current_time)
!DEC$ ATTRIBUTES DLLEXPORT :: get_currenttime
use driver_module
    implicit none
    double precision current_time
    current_time = currenttime
end subroutine  get_currenttime

function mf_external_timestep_reached(external_end_of_timestep) result(external_timestep_reached)
 !DEC$ ATTRIBUTES DLLEXPORT :: mf_external_timestep_reached
     use driver_module
     implicit none
 
     logical external_timestep_reached
     double precision external_end_of_timestep
 
     if(currenttime.ge.external_end_of_timestep)then
         external_timestep_reached = .true.
     else
         external_timestep_reached = .false.
     endif
end function mf_external_timestep_reached

subroutine modflow_dll_set_timestep_done(done_with_current_timestep, Reset_time)
!DEC$ ATTRIBUTES DLLEXPORT :: modflow_dll_set_timestep_done
    use driver_module
    implicit none
    integer nribasimtimestep
    double precision Reset_Time,LastSavedTime
    logical done_with_current_timestep
    if (.not. done_with_current_timestep) then
        ! jump back to begin of time step
        currenttime = Reset_time
        call set_last_saved_time(Reset_time)
    else
        ! time step done
    endif
    
 end subroutine modflow_dll_set_timestep_done
 
 subroutine set_last_saved_time(LastSavedTime)
 !DEC$ ATTRIBUTES DLLEXPORT :: modflow_dll_set_timestep_done
     use m_sts2
     implicit none
     double precision LastSavedTime
 
     currenttime = LastSavedTime
 !    Last_Saved_Time = currenttime
 end subroutine
 
 
 !======================
 ! BMI
 !======================
 
 module mf_bmi_module
    use iso_c_binding

    ! NOTE: these 'MAX'-sizes must be the same as those in
    !       https://github.com/openearth/bmi-python/blob/master/bmi/wrapper.py
    integer(c_int)            :: MAXDIMS = 6
    integer(c_int), parameter :: MAXSTRINGLEN = 1024
    
    ! various variables for calling modflow dll methods
    double precision            :: mf_start_time = -0.1d+0 ! <-0.01 = take starttime from model 
    logical                     :: endOfSimulation = .false.
    integer                     :: mf_ncol, mf_nrow, mf_nper ! TODO: use mf-dll-method
    double precision, &
          dimension(:,:), pointer :: mf_layer1_head
    real, dimension(:,:), pointer :: mf_recharge
    double precision :: mf_end_time
    double precision, dimension(:), pointer :: mf_delt

 contains
    
    pure function char_array_to_string(char_array, length)
        integer(c_int), intent(in) :: length
        character(c_char),intent(in) :: char_array(length)
        character(len=length) :: char_array_to_string
        integer :: i
        do i = 1, length
           char_array_to_string(i:i) = char_array(i)
        enddo
    end function char_array_to_string
    
    integer(c_int) pure function strlen(char_array)
        character(c_char), intent(in) :: char_array(MAXSTRINGLEN)
        integer :: inull, i
        strlen = 0
        do i = 1, size(char_array)
           if (char_array(i) .eq. C_NULL_CHAR) then
              strlen = i-1
              exit
           end if
        end do
    end function strlen

    pure function string_to_char_array(string) result(char_array)
      ! pass only trimmed strings to this one
      character(len=*), intent(in) :: string
      character(kind=c_char,len=1) :: char_array(MAXSTRINGLEN)
      integer :: i
      do i = 1, len(string)
         char_array(i) = string(i:i)
      enddo
      char_array(len(string)+1) = C_NULL_CHAR
    end function string_to_char_array

end module mf_bmi_module
 
   !> The initialize() function accepts a string argument that
  !! gives the name (and path) of its "main input file", called
  !! a configuration file. This function should perform all tasks
  !! that are to take place before entering the model's time loop.
  integer(c_int) function initialize(c_config_file) result(c_iresult) bind(C, name="initialize")
    !DEC$ ATTRIBUTES DLLEXPORT :: initialize
    
    use mf_bmi_module
    use global
    use rf2mf_module, only: starttime

    ! arguments
    character(kind=c_char),intent(in)    :: c_config_file(MAXSTRINGLEN)
    character(len=strlen(c_config_file)) :: config_file

    ! parameters
    integer, parameter :: igrid = 1
    
    ! locals
    integer                      :: ret_val
    character(len=MAXSTRINGLEN)  :: config_file_name
    logical                      :: enableStateSave
    integer                      :: retValMF2005
    double precision             :: tstart, factor
    integer                      :: iper, year, month, day, date
    integer                      :: mf_nsp
    real                         :: mf_tsmlt, mf_perlen
 
    ! get the config file name name
    config_file = char_array_to_string(c_config_file, strlen(c_config_file))

    ! Now we can initialize with the config_file
    
    endOfSimulation = .false.
    enableStateSave = .true.
    
    c_iresult = mf_init_no_sub_pest_moz (config_file, endOfSimulation, mf_start_time, enableStateSave)
    if (c_iresult == 0) then
        call mf2005_getCurrentTime(mf_start_time, ret_val)
        c_iresult = ret_val
    endif
    
    if (c_iresult == 0) then
        mf_ncol = ncol
        mf_nrow = nrow
        allocate(mf_layer1_head(ncol, nrow))
        allocate(mf_recharge(ncol, nrow))
    endif
    
    ! determine starttime/endtime
    write(*,*) 'starttime:',starttime
    read(starttime(1:4),*) year
    read(starttime(5:6),*) month
    read(starttime(7:8),*) day
    date = year*10000 + month*100 + day
    call cfn_datehms2mjd(date,0,0,0,tstart)
    factor = 1.d0
    if (itmuni.eq.1) factor = 1/86400.d0 ! seconds
    if (itmuni.eq.2) factor = 1/1440.d0  ! minutes
    if (itmuni.eq.3) factor = 1/24.d0    ! hours    

    ok = mf2005_GetDis(nper,perlen,nstp,tsmult)
    allocate(mf_delt(nper))
    one=1.
    mf_end_time = tstart  
    mf_nper = nper
    do kper = 1, nper 
       mf_perlen  = perlen(kper)
       mf_nsp    = nstp(kper)
       mf_tsmlt  = tsmult(kper)  
       mf_delt(kper) = mf_perlen/dble(mf_nsp)
       if(mf_tsmlt.ne.one)then
          mf_delt(kper)=mf_perlen*(one-mf_tsmlt)/(one-mf_tsmlt)**mf_nsp
       end if    
       mf_end_time = mf_end_time+mf_delt(kper)*dble(factor) ! [d]    
    end do

  end function initialize

  subroutine get_start_time(t) bind(C, name="get_start_time")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time

    use mf_bmi_module

    real(c_double), intent(out) :: t
    t = mf_start_time
  end subroutine get_start_time

  subroutine get_end_time(t) bind(C, name="get_end_time")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time

    use mf_bmi_module

    real(c_double), intent(out) :: t
    t = mf_end_time
  end subroutine get_end_time

  subroutine get_time_step(dt) bind(C, name="get_time_step")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_time_step

    use mf_bmi_module

    real(c_double), intent(out) :: dt

    dt = -1d+0  ! TODO: implement
    
  end subroutine get_time_step

  subroutine get_current_time(t) bind(C, name="get_current_time")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time

    use mf_bmi_module

    ! arguments
    real(c_double), intent(out) :: t

    ! locals
    double precision :: time
    integer :: ret_val

    call mf2005_getCurrentTime(time, ret_val)
    t = time
    c_iresult = ret_val

  end subroutine get_current_time

  subroutine get_time_units(c_unit)  bind(C, name="get_time_units")

    use mf_bmi_module

    ! arguments
    character(kind=c_char), intent(out) :: c_unit(MAXSTRINGLEN)
    
    ! locals
    character(len=MAXSTRINGLEN) :: unit

    unit = 'Modified Julian Date'
    
    c_unit = string_to_char_array(trim(unit))
  
  end subroutine get_time_units

  integer(c_int) function update(dt) result(c_iresult) bind(C, name="update")
    !DEC$ ATTRIBUTES DLLEXPORT :: update

    use mf_bmi_module
    use iso_c_binding

    real(c_double),intent(in) :: dt

    logical :: stssave
    logical :: doTimeLoop
    integer :: ret_val
    double precision :: tt,endTime,currentTime,nextTime
    
    stssave=.true.    
    call mf2005_getCurrentTime(currentTime, ret_val)
    endTime = currentTime + dt
    nextTime = currentTime
    do while (nextTime < endTime)
        call mf_update_no_sub_pest_moz(endOfSimulation, doTimeLoop, stssave) 
        call mf2005_getBeginOfNextTimeStep(nextTime,ret_val) 
    end do
        
    c_iresult = 0

  end function update
  
  subroutine update_until(c_time) bind(C, name="update_until")

    use mf_bmi_module

    real(c_double),intent(in) :: c_time

    ! locals
    logical :: stssave
    logical :: doTimeLoop
    logical, external :: mf_external_timestep_reached
    double precision :: time

    time = c_time

    do while (.not. mf_external_timestep_reached(time) .and. .not. endOfSimulation)  
        stssave=.true.
        call mf_update_no_sub_pest_moz(endOfSimulation, doTimeLoop, stssave) 
    enddo

  end subroutine update_until

  subroutine get_var_type(c_var_name, c_type)  bind(C, name="get_var_type")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_var_type

    use mf_bmi_module

    character(kind=c_char), intent(in) :: c_var_name(*)
    character(kind=c_char), intent(out) :: c_type(MAXSTRINGLEN)
    character(len=MAXSTRINGLEN) :: type_name, var_name

    ! Use one of the following types
    ! BMI datatype        C datatype        NumPy datatype
    ! BMI_STRING          char*             S<
    ! BMI_INT             int               int16
    ! BMI_LONG            long int          int32
    ! BMI_FLOAT           float             float32
    ! BMI_DOUBLE          double            float64
    var_name = char_array_to_string(c_var_name, strlen(c_var_name))

    select case(var_name)
    case("head","delt")
        type_name = "double"
    case("recharge")
        type_name = "float"
    end select

    c_type = string_to_char_array(trim(type_name))

  end subroutine get_var_type

subroutine get_var(c_var_name, x) bind(C, name="get_var")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var
  ! Return a pointer to the variable

  use mf_bmi_module

  character(kind=c_char), intent(in) :: c_var_name(*)
  type(c_ptr), intent(inout) :: x
  integer(c_int), target, allocatable, save :: xi(:,:)

  integer :: ilay = 1
  integer :: igrid = 1
  logical :: ret_val

  ! The fortran name of the attribute name
  character(len=strlen(c_var_name)) :: var_name
  ! Store the name
  var_name = char_array_to_string(c_var_name, strlen(c_var_name))

  select case(var_name)
  case("head")
     ret_val = mf2005_PutHeadsForLayer(mf_layer1_head, mf_ncol, mf_nrow, ilay, igrid)
     x = c_loc(mf_layer1_head)
  case("delt")
     x = c_loc(mf_delt) 
  end select

end subroutine get_var

subroutine set_var(c_var_name, xptr) bind(C, name="set_var")
  !DEC$ ATTRIBUTES DLLEXPORT :: set_var
  ! Return a pointer to the variable

  use mf_bmi_module

  character(kind=c_char), intent(in) :: c_var_name(*)
  type(c_ptr), value, intent(in) :: xptr

  real(c_float), pointer :: x_2d_float_ptr(:,:)

  integer :: igrid = 1
  logical :: ret_val

  ! The fortran name of the attribute name
  character(len=strlen(c_var_name)) :: var_name
  ! Store the name
  var_name = char_array_to_string(c_var_name, strlen(c_var_name))

  select case(var_name)
  case("recharge")
     call c_f_pointer(xptr, x_2d_float_ptr, shape(mf_recharge))
     mf_recharge(:,:) = x_2d_float_ptr   
     ret_val = mf2005_GetRecharge(mf_recharge, mf_ncol, mf_nrow, igrid)
     write(*,*) 'Done setting recharge.'
  end select

end subroutine set_var

  subroutine get_var_rank(c_var_name, rank) bind(C, name="get_var_rank")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_var_rank

    use mf_bmi_module

    use iso_c_binding, only: c_int, c_char
    character(kind=c_char), intent(in) :: c_var_name(*)
    integer(c_int), intent(out) :: rank

    ! The fortran name of the attribute name
    character(len=strlen(c_var_name)) :: var_name
    ! Store the name
    var_name = char_array_to_string(c_var_name, strlen(c_var_name))

    select case(var_name)
    case("delt")
       rank = 1 
    case default    
       rank = 2
    end select
    
  end subroutine get_var_rank
  
  subroutine get_var_shape(c_var_name, shape) bind(C, name="get_var_shape")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_var_shape

    use mf_bmi_module

    use iso_c_binding, only: c_int, c_char, c_loc

    character(kind=c_char), intent(in) :: c_var_name(*)
    integer(c_int), intent(inout) :: shape(MAXDIMS)
    
    character(len=strlen(c_var_name)) :: var_name

    var_name = char_array_to_string(c_var_name, strlen(c_var_name))
    shape = (/0, 0, 0, 0, 0, 0/)

    !shape(1) = mf_ncol
    !shape(2) = mf_nrow
    select case(var_name)
    case("delt")
       shape(1) = mf_nper
    case default    
       shape(1) = mf_nrow
       shape(2) = mf_ncol
    end select
    
  end subroutine get_var_shape
    
  integer(c_int) function finalize() result(c_iresult) bind(C, name="finalize")
    !DEC$ ATTRIBUTES DLLEXPORT :: finalize

    use mf_bmi_module

    call mf_finish_no_sub_pest_moz()
    c_iresult = 0
  end function finalize

