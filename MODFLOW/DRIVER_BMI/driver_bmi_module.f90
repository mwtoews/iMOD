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

!module m_main_info
!   integer,save          :: timestep          !> timestep number
!end module m_main_info

!subroutine main_info_timestep(ts)
!   use m_main_info
!   implicit none
!   integer  , intent(out) :: ts          !> timestep
!   ts=timestep
!   return
!end


module mf_module

implicit none

 logical :: ok

 integer, save :: mf_ngrid, mf_igrid
 !logical, save :: endOfSimulation = .false.
 logical, save :: stsave,strestore,mozstsave
 integer, save ::  deltats,ios,iteration,lunc,lunsts,lc,xchinit,exitcode,ivcl,jvcl,iarg,jarg,itermozart,narg
 double precision, save :: currentTime,currentTimeMF2005,currentTimeMozart, &
                     endOfCurrentTimeStep,endOfCurrentTimeStepMF2005, &
                     nodataTime,BeginOfNextTimeStepMF2005,&
                     endOfCurrentTimeStepMozart
 logical, save  :: usemodflow,usemetaswap,usetransol,usemozart,usests,usestsmodflow,useribasim
 character (len=1024), save :: root, wd, modwd1, modwd2, simwd1, simwd2, mozwd, infile, rfopt, wddum
 integer, save :: retValMF2005
 integer, save :: tsc ! debug variable
 logical, save :: lss
 real, save :: hnoflo

 integer, save :: isub, nsub
 
 logical, save :: convergedMF2005,convergedMetaSwap !,convergedMozart,convergedPest
 logical, save :: mf_steadystate
 logical, save :: lrunfile, lnamfile, llpf, lipest, lpwt, lrf
 
 character (len=1024), save :: record, modrecord, sobrecord, mozrecord
 
 integer, save :: date, hour, minute, second
 
 integer, allocatable, dimension(:), save :: hrivsys, wrivsys
 integer, save :: nhrivsys, nwrivsys
 integer, save :: timestep
 
 double precision :: dtMetaSwap            ! variable for metaswap
 double precision :: dtMozart
 integer          :: iterMetaSwap          ! variable for metaswap
 double precision :: stsTestTime
 
 ! iPEST with MetaSWAP
 integer, save :: mf_nrow, mf_ncol, mf_nlay

! parameters
 real, parameter :: mv = -99999.
 integer, parameter :: nsubmax = 1000
 character(len=50), dimension(nsubmax), save :: submstr

! ##############################################################################
! MODFLOW-2005
! ##############################################################################
! functions
 logical, external :: mf2005_PutSimulationType,&      ! general
            mf2005_PutGridDimensions,&      ! general
            mf2005_PutNumberOfGrids,&       ! general
            mf2005_PutLPFActive,&           ! general
            mf2005_PutHeadNoFlo,&           ! general
            mf2005_GetPestFlag,&            ! pest
            mf2005_PutPWTActive,&           ! pest
            mf2005_TimeserieInit,&          ! timeseries
            mf2005_TimeserieGetHead,&       ! timeseries
            mf2005_PutModSimNumberOfIDs,&   ! mod-sim coupling
            mf2005_PutModSimIDs,&           ! mod-sim coupling
            mf2005_PutModSimCells,&         ! mod-sim coupling
            mf2005_PutHeads,&               ! mod-sim coupling
            mf2005_GetUnsaturatedZoneFlux,& ! mod-sim coupling
            mf2005_GetStorageFactor,&       ! mod-sim coupling
            mf2005_GetStorageFactorLPF,&    ! mod-sim coupling
            mf2005_PutSeepageFlux,&         ! mod-tran coupling
            mf2005_PutRiverFlux,&           ! mod-tran and mod-moz coupling
            mf2005_PutDrainFlux,&           ! mod-tran and mod-moz coupling
            mf2005_PutSaltFlux,&            ! mod-moz
            mf2005_PutModMozNumberOfIDs,&   ! mod-moz
            mf2005_PutModMozIDs,&           ! mod-moz
            mf2005_PutModMozCells,&         ! mod-moz
            mf2005_GetLSWLevels,&           ! mod-moz
            mf2005_PutModMozRiversToSkip,&  ! mod-moz
            mf2005_PutModMozPVNumberOfIDs,& ! mod-mozpv
            mf2005_PutModMozPVIDs,&         ! mod-mozpv
            mf2005_GetPVLevels,&            ! mod-mozpv
            mf2005_GetQdemand,&             ! mod-ribasim coupling
            mf2005_PutQrealized,&           ! mod-ribasim coupling
            mf2005_PutHeadsForLayer,&       ! mod-wflow coupling
            mf2005_GetRecharge,&            ! mod-wflow coupling 
            mf2005_GetDis                   ! mod-wflow coupling 

 ! general
integer, dimension(:,:), allocatable :: XchModSimModCells, XchModMozModCells, XchModTranModCells

 ! MODFLOW - MetaSWAP coupling
 integer :: XchModSimModNID
 integer, dimension(:), allocatable :: XchModSimModIds, XchSim2ModIdx, XchSim2ModOff
 real, dimension(:), allocatable :: XchModSimModHeads

 ! MODFLOW - MOZART (LSW) coupling
 integer :: XchModMozModNID
 integer, dimension(:), allocatable :: XchModMozModIds, XchMoz2ModIdx, XchMoz2ModOff
 real, dimension(:), allocatable :: XchModMozModRiverFlux, XchModMozModRiverFluxWells,&
                                    XchModMozModDrainFlux, XchModMozModSalt

 ! MODFLOW - MOZART PV coupling
 integer :: XchModMozPVModNID
 integer, dimension(:), allocatable :: XchModMozPVModIds, XchMozPV2ModIdx, XchMozPV2ModOff

 ! MODFLOW - TRANSOL coupling
 integer :: XchModTranModNID
 integer, dimension(:), allocatable :: XchModTranModIds, XchTran2ModIdx, XchTran2ModOff
 real, dimension(:), allocatable :: XchModTranModSeepageFlux,&
                                    XchModTranModRiverFlux,&
                                    XchModTranModDrainFlux
 
! ##############################################################################
! MetaSWAP
! ##############################################################################
 ! functions
 logical, external :: metaswap_PutModSimNumberOfIDs,&   ! mod-sim coupling
            metaswap_PutModSimIDs,&                     ! mod-sim coupling
            metaswap_PutModSimUnsaturatedZoneFlux,&     ! mod-sim coupling
            metaswap_PutStorageFactor,&                 ! mod-sim coupling
            metaswap_GetHeads,&                         ! mod-sim coupling
            metaswap_PutModMozNumberOfIDs,&             ! sim-moz coupling
            metaswap_PutModMozIDs,&                     ! sim-moz coupling
            metaswap_PutCumSWSprinklingDemandFluxes,&   ! sim-moz coupling
            metaswap_PutCumRunonFluxes,&                ! sim-moz coupling
            metaswap_GetFractions                       ! sim-moz coupling
 
 ! MODFLOW - MetaSWAP coupling
 integer :: XchModSimSimNID
 integer, dimension(:), allocatable :: XchModSimSimIds, XchMod2SimIdx, XchMod2SimOff
 real, dimension(:), allocatable :: XchModSimSimUnsZFlux, XchModSimSimStrFct

 ! MetaSWAP - MOZART coupling
 integer :: XchSimMozSimNID
 integer, dimension(:), allocatable :: XchSimMozSimIds, XchMoz2SimIdx, XchMoz2SimOff
 real, dimension(:), allocatable :: XchSimMozSimCuSWSprinklingFlux, XchSimMozSimCuRunonFlux

! ##############################################################################
! TRANSOL
! ##############################################################################
! functions
 logical, external :: TRANSOL_GetSeepageRiverDrainFlux,& ! mod-tran coupling
            TRANSOL_PutSalt,&                  ! tran-moz coupling
            TRANSOL_GetSalt                    ! tran-moz coupling

 ! MODFLOW - TRANSOL coupling
 integer :: XchModTranTranNID
 integer, dimension(:), allocatable :: XchModTranTranIds, XchMod2TranIdx, XchMod2TranOff

 ! TRANSOL - MOZART coupling
 integer :: XchTranMozTranNID
 integer, dimension(:), allocatable :: XchTranMozTranIds, XchMoz2TranIdx, XchMoz2TranOff
 real, dimension(:), allocatable :: XchTranMozTranSalt

! ##############################################################################
! MOZART
! ##############################################################################
! functions
 logical, external :: mozart_PutModMozNumberOfIDs,& ! mod-moz coupling
            mozart_PutModMozIDs,&                   ! mod-moz coupling
            mozart_PutLSWLevels,&                   ! mod-moz coupling
            mozart_GetRiverDrainFlux,&              ! mod-moz coupling
            mozart_PutModMozPVNumberOfIDs,&         ! mod-mozpv coupling
            mozart_PutModMozPVIDs,&                 ! mod-mozpv coupling
            mozart_PutPVLevels,&                    ! mod-mozpv coupling
            mozart_PutLSWFractions,&                ! sim-moz coupling
            mozart_GetCumSWSprinklingDemandFluxes,& ! sim-moz coupling
            mozart_GetCumRunonFluxes,&              ! sim-moz coupling
            mozart_PutLSWSalt,&                     ! tran-moz coupling
            mozart_GetSalt                          ! tran-moz coupling

 ! general
 integer :: XchMozNID, XchMozPVNID
 integer, dimension(:), allocatable :: XchMozIds, XchMozPVIds

 ! MODFLOW - MOZART coupling
 integer, dimension(:), allocatable :: XchMod2MozIdx, XchMod2MozOff
 real, dimension(:), allocatable :: XchModMozMozLevels

 ! MODFLOW - MOZART PV coupling
 integer, dimension(:), allocatable :: XchMod2MozPVIdx, XchMod2MozPVOff
 real, dimension(:), allocatable :: XchModMozPVMozPVLevels

 ! MetaSWAP - MOZART coupling
 integer, dimension(:), allocatable :: Xchsim2MozIdx, XchSim2MozOff
 real, dimension(:), allocatable :: XchSimMozMozFractions

 ! TRANSOL - MOZART coupling
 integer, dimension(:), allocatable :: XchTran2MozIdx, XchTran2MozOff
 real, dimension(:), allocatable :: XchTranMozMozSalt
 
! ##############################################################################

 ! parameters
 integer :: rt = -1
 integer, parameter :: rtmod           = 1 ! MODFLOW only
 integer, parameter :: rtmodsim        = 2 ! MODFLOW-MetaSWAP
 integer, parameter :: rtmodsimtranmoz = 3 ! MODFLOW-MetaSWAP-Mozart
 integer, parameter :: rtmodriba       = 4 ! MODFLOW-Ribasim

 character(len=31), dimension(4) :: rtdesc
 data rtdesc/'MODFLOW                        ',&
             'MODFLOW-MetaSWAP               ',&
             'MODFLOW-MetaSWAP-Transol-Mozart',&
             'MODFLOW-Ribasim                '/

contains

! ------------------------------------------------------------------------------
recursive subroutine qsort(a,ai)

  integer, intent(in out) :: a(:), ai(:)
  integer :: split

  if(size(a) > 1) then
     call partition(a, ai, split)
     call qsort(a(:split-1),ai(:split-1))
     call qsort(a(split:),ai(split:))
  end if

end subroutine qsort

! ------------------------------------------------------------------------------
subroutine partition(a, ai, marker)

  integer, intent(in out) :: a(:), ai(:)
  integer, intent(out) :: marker
  integer :: left, right, pivot, temp

  pivot = (a(1) + a(size(a))) / 2  ! average of first and last elements to prevent quadratic
  left = 0                         ! behavior with sorted or reverse sorted data
  right = size(a) + 1

  do while (left < right)
     right = right - 1
     do while (a(right) > pivot)
        right = right-1
     end do
     left = left + 1
     do while (a(left) < pivot)
        left = left + 1
     end do
     if (left < right) then
        temp = a(left)
        a(left) = a(right)
        a(right) = temp

        temp = ai(left)
        ai(left) = ai(right)
        ai(right) = temp
     end if
  end do

  if (left == right) then
     marker = left + 1
  else
     marker = left
  end if

end subroutine partition

! ------------------------------------------------------------------------------
!subroutine findid(ids,nid,idval,i,i1,i2)
!
!! arguments
!integer, intent(in) :: nid
!integer, dimension(:), intent(in) :: ids(nid)
!integer, intent(in) :: idval
!integer, intent(inout) :: i
!integer, intent(out) :: i1, i2
!
!! locals
!integer :: is
!logical :: l1, l2, fnd
!!.......................................................................
!
!is = i
!i1 = -1
!i2 = -1
!if (i.gt.nid) return
!
!l1  = .false.
!l2  = .false.
!fnd = .false.
!do while(.true.)
!   if (is.gt.nid) exit
!   if (l1 .and. l2) exit
!   if (ids(is).eq.idval) fnd = .true.
!   if (fnd .and. .not.l1) then
!      l1 = .true.
!      i1 = is
!   end if
!   if (is.lt.nid) then
!      if (l1 .and. ids(is+1).ne.idval) then
!         l2 = .true.
!         i2 = is
!      end if
!   end if
!   is = is + 1
!end do
!if (l1 .and. .not.l2) then
!   if (is-1.eq.nid) then
!      i2 = nid
!      l2 = .true.
!   end if
!end if
!
!if (.not.l1 .or. .not.l2) then
!   i1 = -1
!   i2 = -1
!else
!   i = is
!end if
!
!end subroutine

! ------------------------------------------------------------------------------

!> Get the index array for mapping ID1 --> ID2
!subroutine mapIds(idx1,nidx1,off1,id1,nid1,id2,nid2)
!
!! arguments
!integer, intent(in)                    :: nid1 ! number of indices
!integer, intent(in)                    :: nid2 ! number of indices
!integer, intent(in)                    :: nidx1 ! length of idx1
!integer, dimension(nid1),  intent(in)  :: id1 ! indices
!integer, dimension(nid2),  intent(in)  :: id2 ! indices
!integer, dimension(nidx1), intent(out) :: idx1 ! idexes
!integer, dimension(nid1),  intent(out) :: off1 ! off-sets
!
!! locals
!type tMap
!   integer :: n = 0
!   integer :: i = -1
!   integer, dimension(:), pointer :: iarr
!end type tMap
!type(tMap), dimension(:), allocatable :: mapidx
!integer :: i, j, k, n, m, n1, n2, idval, i1s, i1e, i2s, i2e
!integer, dimension(:), allocatable :: id1s, id2s, id1si, id2si, itmp
!!.......................................................................
!
!! allocate temporary data
!allocate(id1s(nid1), id2s(nid2), id1si(nid1), id2si(nid2))
!allocate(mapidx(nid1), itmp(nid2))
!
!! sort the indices (quick sort)
!id1s = id1
!id2s = id2
!do i = 1, nid1
!   id1si(i) = i
!end do
!do i = 1, nid2
!   id2si(i) = i
!end do
!call qsort(id1s,id1si)
!call qsort(id2s,id2si)
!
!! store mapping indices
!n1 = 1
!n2 = 1
!itmp = 0
!do while(.true.)
!   if (n1.gt.nid1 .or. n2.gt.nid2) exit
!   idval = id1s(n1)
!   call findid(id1s,nid1,idval,n1,i1s,i1e)
!   call findid(id2s,nid2,idval,n2,i2s,i2e)
!   if (i2s.gt.0 .and. i2e.gt.0) then
!      do i = i1s, i1e
!         j = id1si(i)
!         m = i2e-i2s+1
!         mapidx(j)%n = m
!         if (m.eq.1) then
!            mapidx(j)%i = id2si(i2s)
!         else
!            allocate(mapidx(j)%iarr(m))
!            do k = i2s, i2e
!               mapidx(j)%iarr(k-i2s+1) = id2si(k)
!            end do
!            ! sort (this is not really necessary)
!            call qsort(mapidx(j)%iarr, itmp(1:m))
!         end if
!      end do
!   end if
!end do
!
!! create index array and offset array
!k = 0
!off1 = 0
!do i = 1, nid1
!   m = mapidx(i)%n
!   if (m.eq.1) then
!      k = k + 1
!      idx1(k) = mapidx(i)%i
!   else if (m.gt.1) then
!      do j = 1, m
!         k = k + 1
!         idx1(k) = mapidx(i)%iarr(j)
!      end do
!   end if
!   if (i.eq.1) then
!      off1(i) = m
!   else
!      off1(i) = off1(i-1) + m
!   end if
!end do
!
!! deallocate
!do i = 1, nid1
!   m = mapidx(i)%n
!   if (m.gt.1) deallocate(mapidx(i)%iarr)
!end do
!deallocate(mapidx)
!deallocate(id1s, id2s, id1si, id2si, itmp)
!
!end subroutine mapIds

! ------------------------------------------------------------------------------

!subroutine driverXchInitModSim()
!
!   ! allocate the MOD-SIM exchange arrays
!   allocate(XchModSimModIds(XchModSimModNID))
!   allocate(XchModSimModHeads(XchModSimModNID))
!   XchModSimModHeads = mv
!   allocate(XchModSimModCells(3,XchModSimModNID))
!
!   ! allocate exchange arrays
!   allocate(XchModSimSimIds(XchModSimSimNID),&
!            XchModSimSimUnsZFlux(XchModSimSimNID),&
!            XchModSimSimStrFct(XchModSimSimNID))
!   XchModSimSimUnsZFlux = mv
!   XchModSimSimStrFct = mv
!
!end subroutine driverXchInitModSim

!subroutine driverXchInitModSimTranMoz()
!
!   ! allocate arrays to store the IDs
!   allocate(XchMozIds(XchMozNID))
!   allocate(XchMozPVIds(XchMozPVNID))
!   allocate(XchModMozModIds(XchModMozModNID))
!   allocate(XchModMozPVModIds(XchModMozPVModNID))
!   allocate(XchSimMozSimIds(XchSimMozSimNID))
!   allocate(XchModMozModCells(3,XchModMozModNID))
!
!   ! allocate the exchange data arrays
!   allocate(XchModMozMozLevels(XchMozNID))
!   allocate(XchModMozPVMozPVLevels(XchMozPVNID))
!   allocate(XchSimMozMozFractions(XchMozNID))
!   allocate(XchModMozModRiverFlux(XchModMozModNID))
!   allocate(XchModMozModRiverFluxWells(XchModMozModNID))
!   allocate(XchModMozModDrainFlux(XchModMozModNID))
!   allocate(XchModMozModSalt(XchModMozModNID))
!   allocate(XchSimMozSimCuSWSprinklingFlux(XchSimMozSimNID))
!   allocate(XchSimMozSimCuRunonFlux(XchSimMozSimNID))
!   XchModMozMozLevels = mv
!   XchModMozPVMozPVLevels = mv
!   XchSimMozMozFractions = mv
!   XchModMozModRiverFlux = mv
!   XchModMozModRiverFluxWells = mv
!   XchModMozModDrainFlux = mv
!   XchModMozModSalt = mv
!   XchSimMozSimCuSWSprinklingFlux = mv
!   XchSimMozSimCuRunonFlux = mv
!   ! Transol (temporary)
!   XchModTranModNID  = XchModSimModNID
!   XchTranMozTranNID = XchSimMozSimNID
!   allocate(XchModTranModCells(3,XchModTranModNID))
!   allocate(XchModTranModSeepageFlux(XchModTranModNID))
!   allocate(XchModTranModRiverFlux(XchModTranModNID))
!   allocate(XchModTranModDrainFlux(XchModTranModNID))
!   allocate(XchTranMozMozSalt(XchMozNID))
!   allocate(XchTranMozTranSalt(XchTranMozTranNID))
!   XchModTranModSeepageFlux = mv
!   XchModTranModRiverFlux = mv
!   XchModTranModDrainFlux = mv
!   XchTranMozMozSalt = mv
!   XchTranMozTranSalt = mv
!   XchModTranModCells = XchModSimModCells
!
!end subroutine driverXchInitModSimTranMoz

!subroutine driverXchIniMapModSim()
!
!   integer :: n
!
!   ! allocate arrays to map MetaSWAP to MODFLOW IDs
!   n = max(XchModSimSimNID,XchModSimModNID)
!   allocate(XchSim2ModIdx(n), XchSim2ModOff(XchModSimModNID))
!   allocate(XchMod2SimIdx(n), XchMod2SimOff(XchModSimSimNID))
!   ! get mapping MODFLOW -> MetaSWAP IDs
!   write(*,*) 'Constructing coupling tables: MODFLOW      --> MetaSWAP'
!   call mapIds(XchMod2SimIdx,n,XchMod2SimOff,XchModSimSimIds,XchModSimSimNID,&
!                                           XchModSimModIds,XchModSimModNID)
!   ! get mapping MetaSWAP -> MODFLOW IDs
!   write(*,*) 'Constructing coupling tables: MetaSWAP     --> MODFLOW'
!   call mapIds(XchSim2ModIdx,n,XchSim2ModOff,XchModSimModIds,XchModSimModNID,&
!                                           XchModSimSimIds,XchModSimSimNID)
!
!end subroutine driverXchIniMapModSim

!subroutine driverXchIniMapModMoz()
!
!   integer :: n
!
!   ! MODFLOW - MOZART coupling (LSW)
!   n = max(XchMozNID,XchModMozModNID)
!   allocate(XchMoz2ModIdx(n), XchMoz2ModOff(XchModMozModNID))
!   allocate(XchMod2MozIdx(n), XchMod2MozOff(XchMozNID))
!   ! mapping MODFLOW --> MOZART (LSW)
!   write(*,*) 'Constructing coupling tables: MODFLOW      --> MOZART (LSW)'
!   call mapIds(XchMod2MozIdx,n,XchMod2MozOff,XchMozIds,      XchMozNID,&
!                                               XchModMozModIds,XchModMozModNID)
!   ! mapping MOZART (LSW) --> MODFLOW
!   write(*,*) 'Constructing coupling tables: MOZART (LSW) --> MODFLOW'
!   call mapIds(XchMoz2ModIdx,n,XchMoz2ModOff,XchModMozModIds,XchModMozModNID,&
!                                               XchMozIds,      XchMozNID)
!end subroutine driverXchIniMapModMoz

!subroutine driverXchIniMapModMozPV()
!
!   integer :: n
!
!   ! MODFLOW - MOZART coupling (PV)
!   n = max(XchMozPVNID,XchModMozPVModNID)
!   allocate(XchMozPV2ModIdx(n), XchMozPV2ModOff(XchModMozPVModNID))
!   allocate(XchMod2MozPVIdx(n), XchMod2MozPVOff(XchMozPVNID))
!   ! mapping MODFLOW --> MOZART (PV)
!   write(*,*) 'Constructing coupling tables: MODFLOW      --> MOZART (PV)'
!   call mapIds(XchMod2MozPVIdx,n,XchMod2MozPVOff,XchMozPVIds,      XchMozPVNID,&
!                                                 XchModMozPVModIds,XchModMozPVModNID)
!   ! mapping MOZART (PV) --> MODFLOW
!   write(*,*) 'Constructing coupling tables: MOZART (PV)  --> MODFLOW'
!   call mapIds(XchMozPV2ModIdx,n,XchMozPV2ModOff,XchModMozPVModIds,XchModMozPVModNID,&
!                                                  XchMozPVIds,      XchMozPVNID)
!end subroutine driverXchIniMapModMozPV

!subroutine driverXchIniMapSimMoz()
!
!   integer :: n
!
!   ! MetaSwap - MOZART coupling
!   n = max(XchMozNID,XchSimMozSimNID)
!   allocate(XchMoz2SimIdx(n), XchMoz2SimOff(XchSimMozSimNID))
!   allocate(XchSim2MozIdx(n), XchSim2MozOff(XchMozNID))
!   ! mapping MetaSWAP --> MOZART
!   write(*,*) 'Constructing coupling tables: MetaSWAP     --> MOZART'
!   call mapIds(XchSim2MozIdx,n,XchSim2MozOff,XchMozIds,      XchMozNID,&
!                                               XchSimMozSimIds,XchSimMozSimNID)
!   ! mapping MOZART  --> MetaSWAP
!   write(*,*) 'Constructing coupling tables: MOZART       --> MetaSWAP'
!   call mapIds(XchMoz2SimIdx,n,XchMoz2SimOff,XchSimMozSimIds,XchSimMozSimNID,&
!                                               XchMozIds,      XchMozNID)
!end subroutine driverXchIniMapSimMoz

!subroutine driverXchIniMapModTran()
!
!   integer :: n
!
!   ! MODFLOW - TRANSOL coupling (for now: using MODFLOW-MetaSWAP coupling)
!   XchModTranTranNID = XchModSimSimNID
!   n = max(XchModTranTranNID,XchModTranModNID)
!   allocate(XchTran2ModIdx(n), XchTran2ModOff(XchModTranModNID))
!   allocate(XchMod2TranIdx(n), XchMod2TranOff(XchModTranTranNID))
!   XchTran2ModIdx = XchSim2ModIdx
!   XchTran2ModOff = XchSim2ModOff
!   XchMod2TranIdx = XchMod2SimIdx
!   XchMod2TranOff = XchMod2SimOff
!
!end subroutine driverXchIniMapModTran

!subroutine driverXchIniMapTranMoz()
!
!   integer :: n
!
!   ! TRANSOL - MOZART coupling (for now: using MetaSWAP-Mozart coupling)
!   n = max(XchMozNID,XchTranMozTranNID)
!   allocate(XchMoz2TranIdx(n), XchMoz2TranOff(XchSimMozSimNID))
!   allocate(XchTran2MozIdx(n), XchTran2MozOff(XchMozNID))
!   XchMoz2TranIdx = XchMoz2SimIdx
!   XchMoz2TranOff = XchMoz2SimOff
!   XchTran2MozIdx = XchSim2MozIdx
!   XchTran2MozOff = XchSim2MozOff
!
!end subroutine driverXchIniMapTranMoz

!subroutine driverXchDeallocate()
!
!!##### BEGIN EXCHANGE: De-allocate ############################################
!if (allocated(XchModSimModCells             )) deallocate(XchModSimModCells             )
!if (allocated(XchModMozModCells             )) deallocate(XchModMozModCells             )
!if (allocated(XchModTranModCells            )) deallocate(XchModTranModCells            )
!if (allocated(XchModSimModIds               )) deallocate(XchModSimModIds               )
!if (allocated(XchSim2ModIdx                 )) deallocate(XchSim2ModIdx                 )
!if (allocated(XchSim2ModOff                 )) deallocate(XchSim2ModOff                 )
!if (allocated(XchModSimModHeads             )) deallocate(XchModSimModHeads             )
!if (allocated(XchModSimModIds               )) deallocate(XchModSimModIds               )
!if (allocated(XchSim2ModIdx                 )) deallocate(XchSim2ModIdx                 )
!if (allocated(XchSim2ModOff                 )) deallocate(XchSim2ModOff                 )
!if (allocated(XchModMozModRiverFlux         )) deallocate(XchModMozModRiverFlux         )
!if (allocated(XchModMozModRiverFluxWells    )) deallocate(XchModMozModRiverFluxWells    )
!if (allocated(XchModMozModDrainFlux         )) deallocate(XchModMozModDrainFlux         )
!if (allocated(XchModMozModSalt              )) deallocate(XchModMozModSalt              )
!if (allocated(XchModMozPVModIds             )) deallocate(XchModMozPVModIds             )
!if (allocated(XchMozPV2ModIdx               )) deallocate(XchMozPV2ModIdx               )
!if (allocated(XchMozPV2ModOff               )) deallocate(XchMozPV2ModOff               )
!if (allocated(XchModTranModIds              )) deallocate(XchModTranModIds              )
!if (allocated(XchTran2ModIdx                )) deallocate(XchTran2ModIdx                )
!if (allocated(XchTran2ModOff                )) deallocate(XchTran2ModOff                )
!if (allocated(XchModTranModSeepageFlux      )) deallocate(XchModTranModSeepageFlux      )
!if (allocated(XchModTranModRiverFlux        )) deallocate(XchModTranModRiverFlux        )
!if (allocated(XchModTranModDrainFlux        )) deallocate(XchModTranModDrainFlux        )
!if (allocated(XchModSimSimIds               )) deallocate(XchModSimSimIds               )
!if (allocated(XchMod2SimIdx                 )) deallocate(XchMod2SimIdx                 )
!if (allocated(XchMod2SimOff                 )) deallocate(XchMod2SimOff                 )
!if (allocated(XchModSimSimUnsZFlux          )) deallocate(XchModSimSimUnsZFlux          )
!if (allocated(XchModSimSimStrFct            )) deallocate(XchModSimSimStrFct            )
!if (allocated(XchSimMozSimIds               )) deallocate(XchSimMozSimIds               )
!if (allocated(XchMoz2SimIdx                 )) deallocate(XchMoz2SimIdx                 )
!if (allocated(XchMoz2SimOff                 )) deallocate(XchMoz2SimOff                 )
!if (allocated(XchSimMozSimCuSWSprinklingFlux)) deallocate(XchSimMozSimCuSWSprinklingFlux)
!if (allocated(XchSimMozSimCuRunonFlux       )) deallocate(XchSimMozSimCuRunonFlux       )
!if (allocated(XchModTranTranIds             )) deallocate(XchModTranTranIds             )
!if (allocated(XchMod2TranIdx                )) deallocate(XchMod2TranIdx                )
!if (allocated(XchMod2TranOff                )) deallocate(XchMod2TranOff                )
!if (allocated(XchTranMozTranIds             )) deallocate(XchTranMozTranIds             )
!if (allocated(XchMoz2TranIdx                )) deallocate(XchMoz2TranIdx                )
!if (allocated(XchMoz2TranOff                )) deallocate(XchMoz2TranOff                )
!if (allocated(XchTranMozTranSalt            )) deallocate(XchTranMozTranSalt            )
!if (allocated(XchMozIds                     )) deallocate(XchMozIds                     )
!if (allocated(XchMozPVIds                   )) deallocate(XchMozPVIds                   )
!if (allocated(XchMod2MozIdx                 )) deallocate(XchMod2MozIdx                 )
!if (allocated(XchMod2MozOff                 )) deallocate(XchMod2MozOff                 )
!if (allocated(XchModMozMozLevels            )) deallocate(XchModMozMozLevels            )
!if (allocated(XchMod2MozPVIdx               )) deallocate(XchMod2MozPVIdx               )
!if (allocated(XchMod2MozPVOff               )) deallocate(XchMod2MozPVOff               )
!if (allocated(XchModMozPVMozPVLevels        )) deallocate(XchModMozPVMozPVLevels        )
!if (allocated(Xchsim2MozIdx                 )) deallocate(Xchsim2MozIdx                 )
!if (allocated(XchSim2MozOff                 )) deallocate(XchSim2MozOff                 )
!if (allocated(XchSimMozMozFractions         )) deallocate(XchSimMozMozFractions         )
!if (allocated(XchTran2MozIdx                )) deallocate(XchTran2MozIdx                )
!if (allocated(XchTran2MozOff                )) deallocate(XchTran2MozOff                )
!if (allocated(XchTranMozMozSalt             )) deallocate(XchTranMozMozSalt             )
!
!end subroutine driverXchDeallocate

! ------------------------------------------------------------------------------
!subroutine driverChk(ok,mes)
!
!logical, intent(in) :: ok
!character(Len=*), intent(in) :: mes
!
!if (.not. ok) then
!    write(*,*) 'Error: ',trim(mes)
!    stop 1
!end if
!
!end subroutine driverChk

! ------------------------------------------------------------------------------

!integer function driverGetRunType(usemodflow,usemetaswap,usetransol,usemozart,usests,useribasim)
!
!logical, intent(in) :: usemodflow
!logical, intent(in) :: usemetaswap
!logical, intent(in) :: usetransol
!logical, intent(in) :: usemozart
!logical, intent(in) :: usests
!logical, intent(in) :: useribasim
!
!integer :: rt
!if (usemodflow .and. .not.usemetaswap .and. .not.usetransol .and.&
!  .not.usemozart) rt = rtmod
!if (usemodflow .and.      usemetaswap .and. .not.usetransol .and.&
!  .not.usemozart) rt = rtmodsim
!if (usemodflow .and.      usemetaswap .and.      usetransol .and.&
!  usemozart .and. usests) rt = rtmodsimtranmoz
!if (usemodflow .and.      useribasim .and. .not.usetransol .and.&
!  .not.usemozart) rt = rtmodriba
!if (rt.le.0) call driverChk(.false.,'Invalid run combination of components')
!
!driverGetRunType = rt
!
!end function driverGetRunType

end module mf_module