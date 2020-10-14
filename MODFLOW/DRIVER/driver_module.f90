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

module m_main_info
   integer,save          :: timestep          !> timestep number
end module m_main_info

subroutine main_info_timestep(ts)
   use m_main_info
   implicit none
   integer  , intent(out) :: ts          !> timestep
   ts=timestep
   return
end


module driver_module
use imod_utl, only: imod_utl_qksort3
implicit none

real, parameter :: mv = -99999.

! ##############################################################################
! MODFLOW-2005
! ##############################################################################
! functions
!            mf2005_PutSimulationType      ! general
!            mf2005_PutGridDimensions      ! general
!            mf2005_PutNumberOfGrids       ! general
!            mf2005_PutLPFActive           ! general
!            mf2005_PutHeadNoFlo           ! general
!            mf2005_GetPestFlag            ! pest
!            mf2005_PutPWTActive           ! pest
!            mf2005_TimeserieInit          ! timeseries
!            mf2005_TimeserieGetHead       ! timeseries
!            mf2005_PutModSimNumberOfIDs   ! mod-sim coupling
!            mf2005_PutModSimIDs           ! mod-sim coupling
!            mf2005_PutModSimCells         ! mod-sim coupling
!            mf2005_PutHeads               ! mod-sim coupling
!            mf2005_GetUnsaturatedZoneFlux ! mod-sim coupling
!            mf2005_GetStorageFactor       ! mod-sim coupling
!            mf2005_GetStorageFactorLPF    ! mod-sim coupling
!            mf2005_PutSeepageFlux         ! mod-tran coupling
!            mf2005_PutRiverFlux           ! mod-tran and mod-moz coupling
!            mf2005_PutRiverFluxSubsys     ! mod-tran coupling
!            mf2005_PutDrainFlux           ! mod-tran and mod-moz coupling
!            mf2005_PutDrainFluxSubsys     ! mod-tran coupling
!            mf2005_PutSaltFlux            ! mod-moz
!            mf2005_PutModMozNumberOfIDs   ! mod-moz
!            mf2005_PutModMozIDs           ! mod-moz
!            mf2005_PutModMozCells         ! mod-moz
!            mf2005_GetLSWLevels           ! mod-moz
!            mf2005_PutModMozRiversToSkip  ! mod-moz
!            mf2005_GetDxcRoot             ! mod-moz
!            mf2005_PutModMozPVNumberOfIDs ! mod-mozpv
!            mf2005_PutModMozPVIDs         ! mod-mozpv
!            mf2005_GetPVLevels            ! mod-mozpv

interface
   logical function mf2005_PutNumberOfGrids(nGrids)
      integer, intent(out) :: nGrids
   end function
end interface  
interface
   logical function mf2005_PutGridDimensions(igrid,nRows,nColumns,nLayers)
      integer, intent(in)  :: igrid
      integer, intent(out) :: nRows
      integer, intent(out) :: nColumns
     integer, intent(out) :: nLayers
   end function
end interface
interface
   logical function mf2005_PutModSimNumberOfIDs(igrid, mini, maxi, nxch)
      integer, intent(in) :: igrid
      integer, intent(out) :: mini
      integer, intent(out) :: maxi
      integer, intent(out) :: nxch
   end function
end interface
interface
   logical function mf2005_PutModSimIDs(igrid,ids)
      integer, intent(in) :: igrid
      integer, dimension(*), intent(out) :: ids
   end function
end interface
interface
   logical function mf2005_PutModSimCells(igrid,cells)
      integer, intent(in) :: igrid
      integer, dimension(3,*), intent(out) :: cells
   end function
end interface
interface
   logical function mf2005_PutModMozCells(igrid,cells)
      integer, intent(in) :: igrid
      integer, dimension(3,*), intent(out) :: cells
   end function
end interface
interface
   logical function mf2005_PutSimulationType(igrid, lss)
      integer, intent(in) :: igrid
      logical, intent(out) :: lss
   end function
end interface
interface
   logical function mf2005_PutLPFActive(igrid, llpf)
      integer, intent(in) :: igrid
      logical, intent(out) :: llpf
   end function
end interface   
interface
   logical function mf2005_PutPWTActive(igrid, lpwt)
      integer, intent(in) :: igrid
      logical, intent(out) :: lpwt
   end function
end interface   
interface
   logical function mf2005_PutHeadNoFlo(igrid, h)
      integer, intent(in) :: igrid
      real, intent(out) :: h
   end function
end interface
interface
  logical function mf2005_PutHeads(igrid,iliric,n,head,mv)
      integer, intent(in) :: igrid, n
      integer, dimension(3,n), intent(in) :: iliric
      real, intent(in) :: mv
      real, dimension(n), intent(out) :: head
   end function
end interface
interface
   logical function mf2005_GetUnsaturatedZoneFlux(igrid,nid,unsflux,xchIdx,xchOff,mv)
      integer, intent(in)                    :: igrid
      integer, intent(in)                    :: nid
      real, dimension(*), intent(inout)      :: unsflux
      integer, dimension(*), intent(in)      :: xchIdx
      integer, dimension(nid), intent(in)    :: xchOff
      real, intent(in)                       :: mv
   end function
end interface
interface
   logical function mf2005_GetStorageFactorLPF(igrid,strfct,nid,xchIdx,xchOff,mv)
      integer, intent(in)                 :: igrid
      integer, intent(in)                 :: nid
      real, dimension(*), intent(inout)   :: strfct
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mv
   end function
end interface
interface
   logical function mf2005_GetStorageFactor(igrid,strfct,nid,xchIdx,xchOff,mv)
      integer, intent(in)                 :: igrid
      integer, intent(in)                 :: nid
      real, dimension(*), intent(inout)   :: strfct
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mv
   end function
end interface
interface
   logical function mf2005_PutSeepageFlux(igrid,xchSeepage,xchCells,nxch,mv,mflag)
      integer, intent(in)                    :: igrid
      integer, intent(in)                    :: nxch
      integer, dimension(3,nxch), intent(in) :: xchCells
      real, dimension(nxch), intent(out)     :: xchSeepage
      real, intent(in)                       :: mv
      logical, intent(in)                    :: mflag
   end function
end interface
interface
   logical function mf2005_PutSeepageSalt(igrid,xchSalt,xchCells,nxch,mv,mflag)
      integer, intent(in)                    :: igrid
      integer, intent(in)                    :: nxch
      integer, dimension(3,nxch), intent(in) :: xchCells
      real, dimension(nxch), intent(out)     :: xchSalt
      real, intent(in)                       :: mv
      logical, intent(in)                    :: mflag
   end function
end interface
interface
   logical function mf2005_PutRiverFlux(igrid,xchRivFlux,&
                      xchCells,nxch,mv,&
                      nhrivsys,hrivsys,nwrivsys,wrivsys,&
                      mflag,wells)
      integer, intent(in)                         :: igrid
      integer, intent(in)                         :: nxch
      integer, dimension(3,nxch), intent(in)      :: xchCells
      real, dimension(nxch), intent(out)          :: xchRivFlux
      real, intent(in)                            :: mv
      integer, intent(in)                         :: nhrivsys
      integer, dimension(nhrivsys), intent(in) :: hrivsys
      integer, intent(in)                         :: nwrivsys
      integer, dimension(nwrivsys), intent(in) :: wrivsys
      logical, intent(in)                         :: mflag
      logical, intent(in)                         :: wells
   end function
end interface
interface
   logical function mf2005_PutRiverFluxSubsys(igrid,xchRivFlux,&
                      xchCells,nxch,mv,&
                      mflag,isubsys)
      integer, intent(in)                         :: igrid
      integer, intent(in)                         :: nxch
      integer, dimension(3,nxch), intent(in)      :: xchCells
      real, dimension(nxch), intent(out)          :: xchRivFlux
      real, intent(in)                            :: mv
      logical, intent(in)                         :: mflag
      integer, intent(in)                         :: isubsys
   end function
end interface     
interface
   logical function mf2005_PutRiverStageSubsys(igrid,xchRivStage,&
                      xchCells,nxch,mv,isubsys)
      integer, intent(in)                         :: igrid
      integer, intent(in)                         :: nxch
      integer, dimension(3,nxch), intent(in)      :: xchCells
      real, dimension(nxch), intent(out)          :: xchRivStage
      real, intent(in)                            :: mv
      integer, intent(in)                         :: isubsys
   end function   
end interface
interface                    
   logical function mf2005_PutSaltFlux(igrid,xchRivFlux,&
                          xchCells,nxch,mv,nwrivsys,wrivsys)
      integer, intent(in)                      :: igrid
      integer, intent(in)                      :: nxch
      integer, dimension(3,nxch), intent(in)   :: xchCells
      real, dimension(nxch), intent(out)       :: xchRivFlux
      real, intent(in)                         :: mv
      integer, intent(in)                      :: nwrivsys
      integer, dimension(nwrivsys), intent(in) :: wrivsys
   end function
end interface
interface
   logical function mf2005_PutSaltFluxSeepage(igrid,xchFlux,xchCells,nxch,mv)
      integer, intent(in)                      :: igrid
      integer, intent(in)                      :: nxch
      integer, dimension(3,nxch), intent(in)   :: xchCells
      real, dimension(nxch), intent(out)       :: xchFlux
      real, intent(in)                         :: mv
   end function
end interface
interface
   logical function mf2005_PutDrainFlux(igrid,xchDrnFlux,&
                        xchCells,nxch,mv,mflag)
      integer, intent(in)                    :: igrid
      integer, intent(in)                    :: nxch
      integer, dimension(3,nxch), intent(in) :: xchCells
      real, dimension(nxch), intent(out)     :: xchDrnFlux
      real, intent(in)                       :: mv
      logical, intent(in)                    :: mflag
   end function
end interface
interface
   logical function mf2005_PutDrainFluxSubsys(igrid,xchDrnFlux,&
                           xchCells,nxch,mv,mflag,isubsys)
      integer, intent(in)                    :: igrid
      integer, intent(in)                    :: nxch
      integer, dimension(3,nxch), intent(in) :: xchCells
      real, dimension(nxch), intent(out)     :: xchDrnFlux
      real, intent(in)                       :: mv
      logical, intent(in)                    :: mflag
      integer, intent(in)                    :: isubsys
   end function
end interface
interface
   logical function mf2005_PutModMozRiversToSkip(igrid,nhriv,hriv)
      integer, intent(in) :: igrid
      integer, intent(in) :: nhriv
      integer, dimension(nhriv), intent(in) :: hriv
   end function
end interface
interface
   logical function mf2005_GetDxcRoot(rfroot)
      character(len=*), intent(in) :: rfroot
   end function
end interface
interface
   logical function mf2005_PutModMozNumberOfIDs(igrid, nxch)
      integer, intent(in) :: igrid
      integer, intent(out) :: nxch
   end function
end interface
interface
   logical function mf2005_PutModMozPVNumberOfIDs(igrid, nxch)
      integer, intent(in) :: igrid
      integer, intent(out) :: nxch
   end function
end interface
interface
   logical function mf2005_PutModMozIDs(igrid,ids)
      integer, intent(in) :: igrid
      integer, dimension(*), intent(out) :: ids
   end function
end interface
interface
   logical function mf2005_PutModMozPVIDs(igrid,ids)
      integer, intent(in) :: igrid
      integer, dimension(*), intent(out) :: ids
   end function
end interface
interface
   logical function mf2005_GetLSWLevels(igrid,levels,nid,xchIdx,xchOff,mv)
      integer, intent(in)                    :: igrid
      integer, intent(in)                    :: nid
      real, dimension(*), intent(inout)      :: levels
      integer, dimension(*), intent(in)      :: xchIdx
      integer, dimension(nid), intent(in)    :: xchOff
      real, intent(in)                       :: mv
   end function
end interface
interface
   logical function mf2005_GetPVLevels(igrid,levels,nid,xchIdx,xchOff,mv)
      integer, intent(in)                    :: igrid
      integer, intent(in)                    :: nid
      real, dimension(*), intent(inout)      :: levels
      integer, dimension(*), intent(in)      :: xchIdx
      integer, dimension(nid), intent(in)    :: xchOff
      real, intent(in)                       :: mv
   end function
end interface
interface
   logical function mf2005_TimeserieInit(igrid)
      integer, intent(in) :: igrid
   end function
end interface
interface
   logical function mf2005_TimeserieGetHead(igrid)
      integer, intent(in) :: igrid
   end function
end interface
interface
  logical function mf2005_GetPestFlag(flag)
      logical, intent(in) :: flag
   end function
end interface 
 
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
                                    XchModTranModDrainFlux,&
                                    XchModTranModRiverStage
! ##############################################################################
! MetaSWAP
! ##############################################################################
 ! functions
 !           metaswap_PutModSimNumberOfIDs             ! mod-sim coupling
 !           metaswap_PutModSimIDs                     ! mod-sim coupling
 !           metaswap_PutModSimUnsaturatedZoneFlux     ! mod-sim coupling
 !           metaswap_PutStorageFactor                 ! mod-sim coupling
 !           metaswap_GetHeads                         ! mod-sim coupling
 !           metaswap_PutModMozNumberOfIDs             ! sim-moz coupling
 !           metaswap_PutModMozIDs                     ! sim-moz coupling
 !           metaswap_PutCumSWSprinklingDemandFluxes   ! sim-moz coupling
 !           metaswap_PutCumRunonFluxes                ! sim-moz coupling
 !           metaswap_GetFractions                     ! sim-moz coupling
interface
   logical function metaswap_PutModSimNumberOfIDs(nxch)
      integer, intent(out) :: nxch
   end function
end interface
interface
   logical function metaswap_PutModSimIDs(ids)
      logical :: retval
      integer, dimension(*), intent(out) :: ids
   end function
end interface
interface
   logical function metaswap_GetHeads(gwheads,nid,xchIdx,xchOff,mv)
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: gwheads
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mv
   end function
end interface
interface
   logical function metaswap_PutModSimUnsaturatedZoneFlux(uszflux,mv)
      real, dimension(*), intent(out) :: uszflux
      real, intent(in)                :: mv ! not used yet 
   end function
end interface
interface
   logical function metaswap_PutSimMozUnsaturatedZoneFlux(uszflux,mv)
      real, dimension(*), intent(out) :: uszflux
      real, intent(in)                :: mv ! not used yet 
   end function      
end interface     
interface
   logical function metaswap_PutStorageFactor(strfct,mv)
      real, dimension(*), intent(out) :: strfct 
      real, intent(in)                :: mv ! not used yet 
   end function      
end interface
interface
   logical function metaswap_PutModMozNumberOfIDs(nid)
      integer, intent(out) :: nid
   end function
end interface
interface
   logical function metaswap_PutModMozIDs(ids)
      integer, dimension(*), intent(out) :: ids
   end function      
end interface
interface
   logical function MetaSWAP_PutCumSWSprinklingDemandFluxes(sprflux,mvin)
      real, dimension(*), intent(out) :: sprflux
      real, intent(in) :: mvin
   end function      
end interface
interface
   logical function MetaSWAP_PutCumRunonFluxes(runonflux,mvin)
      real, dimension(*), intent(out) :: runonflux
      real, intent(in) :: mvin
   end function      
end interface
interface
   logical function metaswap_GetFractions(fractions,nid,xchIdx,xchOff,mvin)
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: fractions
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin
   end function      
end interface
 
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
!            TRANSOL_GetSeepageRiverDrain ! mod-tran coupling
!            TRANSOL_PutSalt              ! tran-moz coupling
!            TRANSOL_GetSalt              ! tran-moz coupling

interface
   logical function TRANSOL_GetSeepageRiverDrain(flux,nid,&
                                             xchIdx,xchOff,mvin,act)
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: flux ! m
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin
      character(len=3), intent(in)        :: act              
   end function
end interface
interface                                             
   logical function TRANSOL_GetSalt(flux,nid,&
                                 xchIdx,xchOff,mvin)
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: flux ! kg/m3
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin
   end function
end interface
interface
   logical function TRANSOL_PutSalt(salt,mvin)
      real, dimension(*), intent(out) :: salt
      real, intent(in) :: mvin
   end function      
end interface

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
!            mozart_PutModMozNumberOfIDs           ! mod-moz coupling
!            mozart_PutModMozIDs                   ! mod-moz coupling
!            mozart_PutLSWLevels                   ! mod-moz coupling
!            mozart_GetRiverDrainFlux              ! mod-moz coupling
!            mozart_PutModMozPVNumberOfIDs         ! mod-mozpv coupling
!            mozart_PutModMozPVIDs                 ! mod-mozpv coupling
!            mozart_PutPVLevels                    ! mod-mozpv coupling
!            mozart_PutLSWFractions                ! sim-moz coupling
!            mozart_GetCumSWSprinklingDemandFluxes ! sim-moz coupling
!            mozart_GetCumRunonFluxes              ! sim-moz coupling
!            mozart_PutLSWSalt                     ! tran-moz coupling
!            mozart_GetSalt                        ! tran-moz coupling
interface
   logical function mozart_PutModMozNumberOfIDs(nid)
      integer, intent(out) :: nid
   end function
end interface
interface
   logical function mozart_PutModMozPVNumberOfIDs(nid)
      integer, intent(out) :: nid
   end function
end interface
interface
   logical function mozart_PutModMozIDs(ids,nid)
      integer, intent(in) :: nid
      integer, dimension(nid), intent(out) :: ids
   end function
end interface
interface
   logical function mozart_PutModMozPVIDs(ids,nid)
      integer, intent(in) :: nid
      integer, dimension(nid), intent(out) :: ids
   end function
end interface
interface
   logical function mozart_PutLSWLevels(levels,mvin)
      real, dimension(*), intent(out) :: levels
      real, intent(in) :: mvin
   end function
end interface
interface
   logical function mozart_PutPVLevels(levels,mvin)
      real, dimension(*), intent(out) :: levels
      real, intent(in) :: mvin
   end function
end interface
interface
   logical function mozart_PutLSWFractions(fractions,mvin)
      real, dimension(*), intent(out) :: fractions
      real, intent(in) :: mvin
   end function
end interface
interface
   logical function mozart_PutLSWSalt(salt,mvin)
      real, dimension(*), intent(out) :: salt
      real, intent(in) :: mvin
   end function
end interface
interface
   logical function MOZART_GetSalt(salt,nid,xchIdx,xchOff,mvin,iact)
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: salt
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin
      integer, intent(in)                 :: iact
   end function
end interface   
interface
   logical function mozart_GetSeepageFlux(flux,nid,xchIdx,xchOff,mvin)
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: flux
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin
    end function
end interface
interface
   logical function MOZART_GetUnsaturatedZoneFlux(flux,nid,xchIdx,xchOff,mvin)
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: flux
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin
   end function
end interface
interface
   logical function mozart_GetRiverDrainFlux(flux,nid,xchIdx,xchOff,mvin,iact)
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: flux
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin
      integer, intent(in)                 :: iact
   end function
end interface
interface
   logical function MOZART_GetCumSWSprinklingDemandFluxes(sprflux,nid,xchIdx,xchOff,mvin)
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: sprflux
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin
   end function
end interface
interface
   logical function MOZART_GetCumRunonFluxes(runonflux,nid,xchIdx,xchOff,mvin)
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: runonflux
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin
   end function
end interface
interface
   subroutine MOZART_GetCurrentTime(t)
      double precision, intent(out) :: t
   end subroutine 
end interface
 
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

 character(len=31), dimension(3) :: rtdesc
 data rtdesc/'MODFLOW                        ',&
             'MODFLOW-MetaSWAP               ',&
             'MODFLOW-MetaSWAP-Transol-Mozart'/

contains

! ------------------------------------------------------------------------------
subroutine findid(ids,nid,idval,i,i1,i2)

! arguments
integer, intent(in) :: nid
integer, dimension(:), intent(in) :: ids(nid)
integer, intent(in) :: idval
integer, intent(inout) :: i
integer, intent(out) :: i1, i2

! locals
integer :: is
logical :: l1, l2, fnd
!.......................................................................

is = i
i1 = -1
i2 = -1
if (i.gt.nid) return

l1  = .false.
l2  = .false.
fnd = .false.
do while(.true.)
   if (is.gt.nid) exit
   if (l1 .and. l2) exit
   if (ids(is).eq.idval) fnd = .true.
   if (fnd .and. .not.l1) then
      l1 = .true.
      i1 = is
   end if
   if (is.lt.nid) then
      if (l1 .and. ids(is+1).ne.idval) then
         l2 = .true.
         i2 = is
      end if
   end if
   is = is + 1
end do
if (l1 .and. .not.l2) then
   if (is-1.eq.nid) then
      i2 = nid
      l2 = .true.
   end if
end if

if (.not.l1 .or. .not.l2) then
   i1 = -1
   i2 = -1
else
   i = is
end if

end subroutine

! ------------------------------------------------------------------------------

!> Get the index array for mapping ID1 --> ID2
subroutine mapIds(idx1,nidx1,off1,id1,nid1,id2,nid2)

! arguments
integer, intent(in)                :: nid1 ! number of indices
integer, intent(in)                :: nid2 ! number of indices
integer, intent(in)                :: nidx1 ! length of idx1
integer, dimension(*), intent(in)  :: id1 ! indices
integer, dimension(*), intent(in)  :: id2 ! indices
integer, dimension(*), intent(out) :: idx1 ! idexes
integer, dimension(*), intent(out) :: off1 ! off-sets

! locals
type tMap
   integer :: n = 0
   integer :: i = -1
   integer, dimension(:), pointer :: iarr
end type tMap
type(tMap), dimension(:), allocatable :: mapidx
integer :: i, j, k, n, m, n1, n2, idval, i1s, i1e, i2s, i2e
integer :: nb1, nb2, nb3, nb4, nb5, nb6
integer, dimension(:), allocatable :: id1s, id2s, id1si, id2si, itmp
!.......................................................................

! allocate temporary data
n = max(nid1,1)
m = max(nid2,1)
nb1 = n; nb2 = m; nb3 = n; nb4 = m; nb5 = n; nb6 = m
allocate(id1s(n), id2s(m), id1si(n), id2si(m))
allocate(mapidx(n), itmp(m))

! sort the indices (quick sort)
do i = 1, nid1
   id1s(i) = id1(i)
end do
do i = 1, nid2
   id2s(i) = id2(i)
end do
do i = 1, nid1
   id1si(i) = i
end do
do i = 1, nid2
   id2si(i) = i
end do
call imod_utl_qksort3(id1s,id1si)
call imod_utl_qksort3(id2s,id2si)

! store mapping indices
n1 = 1
n2 = 1
itmp = 0
do while(.true.)
   if (n1.gt.nid1 .or. n2.gt.nid2) exit
   call checkbound(n1,nb1,'1')
   idval = id1s(n1)
   call findid(id1s,nid1,idval,n1,i1s,i1e)
   call findid(id2s,nid2,idval,n2,i2s,i2e)
   if (i2s.gt.0 .and. i2e.gt.0) then
      do i = i1s, i1e
         j = id1si(i)
         m = i2e-i2s+1
         call checkbound(j,nb5,'2')
         mapidx(j)%n = m
         if (m.eq.1) then
            call checkbound(i2s,nb4,'3')
            mapidx(j)%i = id2si(i2s)
         else
            allocate(mapidx(j)%iarr(m))
            do k = i2s, i2e
               call checkbound(k,nb4,'4')
               mapidx(j)%iarr(k-i2s+1) = id2si(k)
            end do
            ! sort (this is not really necessary)
            call checkbound(m,nb6,'5')
            call imod_utl_qksort3(mapidx(j)%iarr, itmp(1:m))
         end if
      end do
   end if
end do

! create index array and offset array
do i = 1, nid1
   off1(i) = 0
end do   
k = 0
do i = 1, nid1
   m = mapidx(i)%n
   if (m.eq.1) then
      k = k + 1
      call checkbound(k,nidx1,'6')
      call checkbound(i,nb3,'7')
      idx1(k) = mapidx(i)%i
   else if (m.gt.1) then
      do j = 1, m
         k = k + 1
         call checkbound(k,nidx1,'8')
         call checkbound(i,nb3,'9') 
         idx1(k) = mapidx(i)%iarr(j)
      end do
   end if
   if (i.eq.1) then
      off1(i) = m
   else
      off1(i) = off1(i-1) + m
   end if
end do

! deallocate
do i = 1, nid1
   call checkbound(i,nb3,'10') 
   m = mapidx(i)%n
   if (m.gt.1) deallocate(mapidx(i)%iarr)
end do
deallocate(mapidx)
deallocate(id1s)
deallocate(id2s)
deallocate(id1si)
deallocate(id2si)
deallocate(itmp)

end subroutine mapIds

subroutine checkbound(i,ib,msg)
implicit none
integer, intent(in) :: i,ib
character(len=*), intent(in) :: msg

if(i.gt.ib.or.i.le.0)then
   write(*,*) 'Program error: '//trim(msg), i, ib 
   call ustop(' ')
end if   

end subroutine

! ------------------------------------------------------------------------------

subroutine driverXchInitModSim1()
  
   integer :: n

   n = max(XchModSimModNID,1)
   
   ! allocate the MOD-SIM exchange arrays
   allocate(XchModSimModIds(n))
   allocate(XchModSimModCells(3,n))

end subroutine driverXchInitModSim1

subroutine driverXchInitModSim2()

   integer :: n, m

   n = max(XchModSimModNID,1)
   m = max(XchModSimSimNID,1)
   
   ! allocate the MOD-SIM exchange arrays
   allocate(XchModSimModHeads(n))
   XchModSimModHeads = mv

   ! allocate exchange arrays
   allocate(XchModSimSimIds(m),&
            XchModSimSimUnsZFlux(m),&
            XchModSimSimStrFct(m))
   XchModSimSimUnsZFlux = mv
   XchModSimSimStrFct = mv

end subroutine driverXchInitModSim2

subroutine driverXchInitModSimTranMoz()

   ! allocate arrays to store the IDs
   allocate(XchMozIds(XchMozNID))
   allocate(XchMozPVIds(XchMozPVNID))
   allocate(XchModMozModIds(XchModMozModNID))
   allocate(XchModMozPVModIds(max(XchModMozPVModNID,1)))
   allocate(XchSimMozSimIds(XchSimMozSimNID))
   allocate(XchModMozModCells(3,XchModMozModNID))

   ! allocate the exchange data arrays
   allocate(XchModMozMozLevels(XchMozNID))
   allocate(XchModMozPVMozPVLevels(XchMozPVNID))
   allocate(XchSimMozMozFractions(XchMozNID))
   allocate(XchModMozModRiverFlux(XchModMozModNID))
   allocate(XchModMozModRiverFluxWells(XchModMozModNID))
   allocate(XchModMozModDrainFlux(XchModMozModNID))
   allocate(XchModMozModSalt(XchModMozModNID))
   allocate(XchSimMozSimCuSWSprinklingFlux(XchSimMozSimNID))
   allocate(XchSimMozSimCuRunonFlux(XchSimMozSimNID))
   XchModMozMozLevels = mv
   XchModMozPVMozPVLevels = mv
   XchSimMozMozFractions = mv
   XchModMozModRiverFlux = mv
   XchModMozModRiverFluxWells = mv
   XchModMozModDrainFlux = mv
   XchModMozModSalt = mv
   XchSimMozSimCuSWSprinklingFlux = mv
   XchSimMozSimCuRunonFlux = mv
   ! Transol (temporary)
   XchModTranModNID  = XchModSimModNID
   XchTranMozTranNID = XchSimMozSimNID
   allocate(XchModTranModCells(3,XchModTranModNID))
   allocate(XchModTranModSeepageFlux(XchModTranModNID))
   allocate(XchModTranModRiverFlux(XchModTranModNID))
   allocate(XchModTranModRiverStage(XchModTranModNID))
   allocate(XchModTranModDrainFlux(XchModTranModNID))
   allocate(XchTranMozMozSalt(XchMozNID))
   allocate(XchTranMozTranSalt(XchTranMozTranNID))
   XchModTranModSeepageFlux = mv
   XchModTranModRiverFlux = mv
   XchModTranModRiverStage = mv
   XchModTranModDrainFlux = mv
   XchTranMozMozSalt = mv
   XchTranMozTranSalt = mv
   XchModTranModCells = XchModSimModCells

end subroutine driverXchInitModSimTranMoz

subroutine driverXchIniMapModSim()
   use pksmpi_mod,only:myrank
   integer :: n, n1, n2, i
   logical :: pks7mpimasterwrite

   !if(myrank.eq.0)then
   !   write(*,*) 'myrank=0:' 
   !   write(*,*) '# modflow ID=',XchModSimModNID
   !   do i = 1, XchModSimModNID
   !      write(*,*)i,'-->',XchModSimModIds(i)
   !   end do
   !   write(*,*) '# metaswap ID=',XchModSimSimNID
   !   do i = 1, XchModSimSimNID
   !      write(*,*)i,'-->',XchModSimSimIds(i)
   !   end do
   !end if
   !call pks7mpibarrier()
   !if(myrank.eq.1)then
   !   write(*,*) 'myrank=1:' 
   !   write(*,*) '# modflow ID=',XchModSimModNID
   !   do i = 1, XchModSimModNID
   !      write(*,*)i,'-->',XchModSimModIds(i)
   !   end do
   !   write(*,*) '# metaswap ID=',XchModSimSimNID
   !   do i = 1, XchModSimSimNID
   !      write(*,*)i,'-->',XchModSimSimIds(i)
   !   end do
   !end if   
   
   ! allocate arrays to map MetaSWAP to MODFLOW IDs
   n1 = max(XchModSimModNID,1)
   n2 = max(XchModSimSimNID,1)
   n = max(n1,n2)
   n = max(n,1)
!   allocate(XchSim2ModIdx(n), XchSim2ModOff(n1))
!   allocate(XchMod2SimIdx(n), XchMod2SimOff(n2))
   allocate(XchSim2ModIdx(n), XchSim2ModOff(n))
   allocate(XchMod2SimIdx(n), XchMod2SimOff(n))
   
   ! get mapping MODFLOW -> MetaSWAP IDs
   if (pks7mpimasterwrite()) write(*,*) 'Constructing coupling tables: MODFLOW      --> MetaSWAP'
   call mapIds(XchMod2SimIdx,n,XchMod2SimOff,XchModSimSimIds,XchModSimSimNID,&
                                           XchModSimModIds,XchModSimModNID)
   ! get mapping MetaSWAP -> MODFLOW IDs
   if (pks7mpimasterwrite()) write(*,*) 'Constructing coupling tables: MetaSWAP     --> MODFLOW'
   call mapIds(XchSim2ModIdx,n,XchSim2ModOff,XchModSimModIds,XchModSimModNID,&
                                           XchModSimSimIds,XchModSimSimNID)
 
   !call pks7mpibarrier()
   !call pks7mpiwrpfinalize()
   !write(*,*),'@@@@@@@ stopping!'
   !stop
   
end subroutine driverXchIniMapModSim

subroutine driverXchIniMapModMoz()

   integer :: n
   logical :: pks7mpimasterwrite

   ! MODFLOW - MOZART coupling (LSW)
   n = max(XchMozNID,XchModMozModNID)
   allocate(XchMoz2ModIdx(n), XchMoz2ModOff(XchModMozModNID))
   allocate(XchMod2MozIdx(n), XchMod2MozOff(XchMozNID))
   ! mapping MODFLOW --> MOZART (LSW)
   if (pks7mpimasterwrite()) write(*,*) 'Constructing coupling tables: MODFLOW      --> MOZART (LSW)'
   call mapIds(XchMod2MozIdx,n,XchMod2MozOff,XchMozIds,      XchMozNID,&
                                               XchModMozModIds,XchModMozModNID)
   ! mapping MOZART (LSW) --> MODFLOW
   if (pks7mpimasterwrite()) write(*,*) 'Constructing coupling tables: MOZART (LSW) --> MODFLOW'
   call mapIds(XchMoz2ModIdx,n,XchMoz2ModOff,XchModMozModIds,XchModMozModNID,&
                                               XchMozIds,      XchMozNID)
end subroutine driverXchIniMapModMoz

subroutine driverXchIniMapModMozPV()

   integer :: n
   logical :: pks7mpimasterwrite

   ! MODFLOW - MOZART coupling (PV)
   n = max(XchMozPVNID,XchModMozPVModNID); n = max(n,1)
   allocate(XchMozPV2ModIdx(n), XchMozPV2ModOff(XchModMozPVModNID))
   allocate(XchMod2MozPVIdx(n), XchMod2MozPVOff(XchMozPVNID))
   ! mapping MODFLOW --> MOZART (PV)
   if (pks7mpimasterwrite()) write(*,*) 'Constructing coupling tables: MODFLOW      --> MOZART (PV)'
   call mapIds(XchMod2MozPVIdx,n,XchMod2MozPVOff,XchMozPVIds,      XchMozPVNID,&
                                                 XchModMozPVModIds,XchModMozPVModNID)
   ! mapping MOZART (PV) --> MODFLOW
   if (pks7mpimasterwrite()) write(*,*) 'Constructing coupling tables: MOZART (PV)  --> MODFLOW'
   call mapIds(XchMozPV2ModIdx,n,XchMozPV2ModOff,XchModMozPVModIds,XchModMozPVModNID,&
                                                  XchMozPVIds,      XchMozPVNID)
end subroutine driverXchIniMapModMozPV

subroutine driverXchIniMapSimMoz()

   integer :: n
   logical :: pks7mpimasterwrite

   ! MetaSwap - MOZART coupling
   n = max(XchMozNID,XchSimMozSimNID)
   allocate(XchMoz2SimIdx(n), XchMoz2SimOff(XchSimMozSimNID))
   allocate(XchSim2MozIdx(n), XchSim2MozOff(XchMozNID))
   ! mapping MetaSWAP --> MOZART
   if (pks7mpimasterwrite()) write(*,*) 'Constructing coupling tables: MetaSWAP     --> MOZART'
   call mapIds(XchSim2MozIdx,n,XchSim2MozOff,XchMozIds,      XchMozNID,&
                                               XchSimMozSimIds,XchSimMozSimNID)
   ! mapping MOZART  --> MetaSWAP
   if (pks7mpimasterwrite()) write(*,*) 'Constructing coupling tables: MOZART       --> MetaSWAP'
   call mapIds(XchMoz2SimIdx,n,XchMoz2SimOff,XchSimMozSimIds,XchSimMozSimNID,&
                                               XchMozIds,      XchMozNID)
end subroutine driverXchIniMapSimMoz

subroutine driverXchIniMapModTran()

   integer :: n

   ! MODFLOW - TRANSOL coupling (for now: using MODFLOW-MetaSWAP coupling)
   XchModTranTranNID = XchModSimSimNID
   n = max(XchModTranTranNID,XchModTranModNID)
   allocate(XchTran2ModIdx(n), XchTran2ModOff(XchModTranModNID))
   allocate(XchMod2TranIdx(n), XchMod2TranOff(XchModTranTranNID))
   XchTran2ModIdx = XchSim2ModIdx
   XchTran2ModOff = XchSim2ModOff
   XchMod2TranIdx = XchMod2SimIdx
   XchMod2TranOff = XchMod2SimOff

end subroutine driverXchIniMapModTran

subroutine driverXchIniMapTranMoz()

   integer :: n

   ! TRANSOL - MOZART coupling (for now: using MetaSWAP-Mozart coupling)
   n = max(XchMozNID,XchTranMozTranNID)
   allocate(XchMoz2TranIdx(n), XchMoz2TranOff(XchSimMozSimNID))
   allocate(XchTran2MozIdx(n), XchTran2MozOff(XchMozNID))
   XchMoz2TranIdx = XchMoz2SimIdx
   XchMoz2TranOff = XchMoz2SimOff
   XchTran2MozIdx = XchSim2MozIdx
   XchTran2MozOff = XchSim2MozOff

end subroutine driverXchIniMapTranMoz

subroutine driverXchDeallocate()

!##### BEGIN EXCHANGE: De-allocate ############################################
if (allocated(XchModSimModCells             )) deallocate(XchModSimModCells             )
if (allocated(XchModMozModCells             )) deallocate(XchModMozModCells             )
if (allocated(XchModTranModCells            )) deallocate(XchModTranModCells            )
if (allocated(XchModSimModIds               )) deallocate(XchModSimModIds               )
if (allocated(XchSim2ModIdx                 )) deallocate(XchSim2ModIdx                 )
if (allocated(XchSim2ModOff                 )) deallocate(XchSim2ModOff                 )
if (allocated(XchModSimModHeads             )) deallocate(XchModSimModHeads             )
if (allocated(XchModSimModIds               )) deallocate(XchModSimModIds               )
if (allocated(XchSim2ModIdx                 )) deallocate(XchSim2ModIdx                 )
if (allocated(XchSim2ModOff                 )) deallocate(XchSim2ModOff                 )
if (allocated(XchModMozModRiverFlux         )) deallocate(XchModMozModRiverFlux         )
if (allocated(XchModMozModRiverFluxWells    )) deallocate(XchModMozModRiverFluxWells    )
if (allocated(XchModMozModDrainFlux         )) deallocate(XchModMozModDrainFlux         )
if (allocated(XchModMozModSalt              )) deallocate(XchModMozModSalt              )
if (allocated(XchModMozPVModIds             )) deallocate(XchModMozPVModIds             )
if (allocated(XchMozPV2ModIdx               )) deallocate(XchMozPV2ModIdx               )
if (allocated(XchMozPV2ModOff               )) deallocate(XchMozPV2ModOff               )
if (allocated(XchModTranModIds              )) deallocate(XchModTranModIds              )
if (allocated(XchTran2ModIdx                )) deallocate(XchTran2ModIdx                )
if (allocated(XchTran2ModOff                )) deallocate(XchTran2ModOff                )
if (allocated(XchModTranModSeepageFlux      )) deallocate(XchModTranModSeepageFlux      )
if (allocated(XchModTranModRiverFlux        )) deallocate(XchModTranModRiverFlux        )
if (allocated(XchModTranModRiverStage       )) deallocate(XchModTranModRiverStage       )
if (allocated(XchModTranModDrainFlux        )) deallocate(XchModTranModDrainFlux        )
if (allocated(XchModSimSimIds               )) deallocate(XchModSimSimIds               )
if (allocated(XchMod2SimIdx                 )) deallocate(XchMod2SimIdx                 )
if (allocated(XchMod2SimOff                 )) deallocate(XchMod2SimOff                 )
if (allocated(XchModSimSimUnsZFlux          )) deallocate(XchModSimSimUnsZFlux          )
if (allocated(XchModSimSimStrFct            )) deallocate(XchModSimSimStrFct            )
if (allocated(XchSimMozSimIds               )) deallocate(XchSimMozSimIds               )
if (allocated(XchMoz2SimIdx                 )) deallocate(XchMoz2SimIdx                 )
if (allocated(XchMoz2SimOff                 )) deallocate(XchMoz2SimOff                 )
if (allocated(XchSimMozSimCuSWSprinklingFlux)) deallocate(XchSimMozSimCuSWSprinklingFlux)
if (allocated(XchSimMozSimCuRunonFlux       )) deallocate(XchSimMozSimCuRunonFlux       )
if (allocated(XchModTranTranIds             )) deallocate(XchModTranTranIds             )
if (allocated(XchMod2TranIdx                )) deallocate(XchMod2TranIdx                )
if (allocated(XchMod2TranOff                )) deallocate(XchMod2TranOff                )
if (allocated(XchTranMozTranIds             )) deallocate(XchTranMozTranIds             )
if (allocated(XchMoz2TranIdx                )) deallocate(XchMoz2TranIdx                )
if (allocated(XchMoz2TranOff                )) deallocate(XchMoz2TranOff                )
if (allocated(XchTranMozTranSalt            )) deallocate(XchTranMozTranSalt            )
if (allocated(XchMozIds                     )) deallocate(XchMozIds                     )
if (allocated(XchMozPVIds                   )) deallocate(XchMozPVIds                   )
if (allocated(XchMod2MozIdx                 )) deallocate(XchMod2MozIdx                 )
if (allocated(XchMod2MozOff                 )) deallocate(XchMod2MozOff                 )
if (allocated(XchModMozMozLevels            )) deallocate(XchModMozMozLevels            )
if (allocated(XchMod2MozPVIdx               )) deallocate(XchMod2MozPVIdx               )
if (allocated(XchMod2MozPVOff               )) deallocate(XchMod2MozPVOff               )
if (allocated(XchModMozPVMozPVLevels        )) deallocate(XchModMozPVMozPVLevels        )
if (allocated(Xchsim2MozIdx                 )) deallocate(Xchsim2MozIdx                 )
if (allocated(XchSim2MozOff                 )) deallocate(XchSim2MozOff                 )
if (allocated(XchSimMozMozFractions         )) deallocate(XchSimMozMozFractions         )
if (allocated(XchTran2MozIdx                )) deallocate(XchTran2MozIdx                )
if (allocated(XchTran2MozOff                )) deallocate(XchTran2MozOff                )
if (allocated(XchTranMozMozSalt             )) deallocate(XchTranMozMozSalt             )

end subroutine driverXchDeallocate

! ------------------------------------------------------------------------------
subroutine driverChk(ok,mes)

logical, intent(in) :: ok
character(Len=*), intent(in) :: mes

if (.not. ok) then
    write(*,*) 'Error: ',trim(mes)
    stop 1
end if

end subroutine driverChk

! ------------------------------------------------------------------------------

integer function driverGetRunType(usemodflow,usemetaswap,usetransol,usemozart,usests)

logical, intent(in) :: usemodflow
logical, intent(in) :: usemetaswap
logical, intent(in) :: usetransol
logical, intent(in) :: usemozart
logical, intent(in) :: usests

integer :: rt

rt = 0
if (usemodflow .and. .not.usemetaswap .and. .not.usetransol .and.&
  .not.usemozart) rt = rtmod
if (usemodflow .and.      usemetaswap .and. .not.usetransol .and.&
  .not.usemozart) rt = rtmodsim
if (usemodflow .and.      usemetaswap .and.      usetransol .and.&
  usemozart .and. usests) rt = rtmodsimtranmoz
if (rt.le.0) call driverChk(.false.,'Invalid run combination of components')

driverGetRunType = rt

end function driverGetRunType

! ------------------------------------------------------------------------------

subroutine imod_license()

use imod_utl, only: imod_utl_getunit, imod_utl_getdir, imod_utl_s_cap,&
                    imod_utl_printtext, imod_utl_openasc, nlic, lic, nhdr, hdr, licfile

implicit none

! locals
character(len=1024) :: dir, fname, datetime
character(len=1024) :: key
logical :: lex, lagree
integer :: i, iu
integer, dimension(8) :: iedt

call getarg(0,dir) ! get full path of the executable
call imod_utl_getdir(dir) ! get the directory (last character is a slash)
write(fname,'(2a)') trim(dir), trim(licfile)
inquire(file=fname,exist=lex) ! check if file exists
lagree=.false.
if (.not.lex) then
   ! write license to standard output
   do i = 1, size(lic)
      call imod_utl_printtext(trim(lic(i)),0)
   end do
   call imod_utl_printtext('',0) 
   call imod_utl_printtext('I accept the iMOD License (please enter "Y" or "N" and hit the Enter-key):',0)
   do while(.true.)
      read(*,*) key     
      call imod_utl_s_cap(key,'l')
      select case(key)
      case('y')
         call date_and_time(values=iedt)
         write(datetime,10)(iedt(i),i=3,1,-1),(iedt(i),i=5,7) ! (yyyy/mm/dd hh:mm:ss)
         lagree = .true.
         exit
      case('n')
         call imod_utl_printtext('I do NOT accept the iMOD License. Exiting program.',0)
         stop 1
      case default
         call imod_utl_printtext('Invalid input, please try again.',0)
         call imod_utl_printtext('I accept the iMOD License (please enter "Y" or "N" and hit the Enter-key):',0)    
      end select   
   end do
end if

! If agreed, then write license file
if (lagree) then 
   call imod_utl_printtext('Writing license agreement file ('//trim(licfile)//')...',0)
   iu=imod_utl_getunit()
   call imod_utl_openasc(iu,fname,'w')
   WRITE(IU,'( A )') 'You accepted the term and conditions of the iMOD Software License Agreement on '//TRIM(datetime)
   call imod_writelicense(iu)
!   do i = 1, size(lic)
!      call imod_utl_printtext(trim(lic(i)),-2,iu)
!   end do
!   call imod_utl_printtext('',-1,iu)
!   call imod_utl_printtext('I accepted the terms and conditions of the iMOD Software License Agreement on:',-2,iu)
!   call imod_utl_printtext('',-2,iu)
!   call imod_utl_printtext(trim(datetime),-2,iu)
   close(iu)
end if

do i = 1, size(hdr)
   call imod_utl_printtext(trim(hdr(i)),0)
end do

10 format(i2.2,'/',i2.2,'/',i4,1x,i2.2,':',i2.2,':',i2.2)

end subroutine imod_license

 !###====================================================================
 subroutine imod_writelicense(iu)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: iu
 INTEGER,PARAMETER :: STRLEN=20000 
 CHARACTER(LEN=2),PARAMETER :: NEWLINE=CHAR(13)//CHAR(10)
 CHARACTER(LEN=:),ALLOCATABLE :: STR
 
 ALLOCATE(CHARACTER(LEN=STRLEN) :: STR)

 STR='iMOD Software License Agreement'//NEWLINE// &
 NEWLINE// &
 'This is a license for the iMOD-executables, distributed from the Stichti'// &
 'ng Deltares website. If you require a printed version of this iMOD Softw'// &
 'are License Agreement, e.g. prior to accepting these terms and condition'// &
 's, please print the pdf-file ‘iMOD_Software_License_Agreement_V5_2.pdf’. You '// &
 'should have received a copy of this pdf-file; if not, see '//NEWLINE// &
 'http://oss.deltares.nl/web/iMOD/iMOD_software_license_agreement.'//NEWLINE// &
 NEWLINE// &
 'Read this software license agreement'//NEWLINE// &
 NEWLINE// &
 'General'//NEWLINE// &
 NEWLINE// &
 'This is a legal agreement between the prospective user, either an indivi'// &
 'dual or an entity, (hereafter: “Licensee”) and Stichting Deltares (herea'// &
 'fter “Deltares”) to license to Licensee the computer program called iMOD'// &
 ' as described below under “DESCRIPTION OF iMOD” (hereafter “iMOD”). '//NEWLINE// &
 'By marking the “Yes, I Accept”-checkbox in the iMOD-GUI or entering “Y” '// &
 'or “y” when prompted in iMODFLOW:'//NEWLINE// &
 NEWLINE// &
 '1. You expressly declare being authorized to act on behalf of Licensee f'// &
 'or the purposes of accepting this software license agreement (hereafter '// &
 '“License Agreement”);'//NEWLINE// &
 '2. Licensee expressly accepts this License Agreement and accepts to be l'// &
 'egally bound by the terms and conditions contained therein. '//NEWLINE// &
 NEWLINE// &
 'If you are not authorized to act on behalf of Licensee to agree upon thi'// &
 's License Agreement, please do not mark the “Yes, I accept”-checkbox in '// &
 'the iMOD-GUI and do not enter “Y” or “y” when prompted from iMODFLOW and '// &
 'exit the iMOD-program by clicking the “Cancel” checkbox in the iMOD-GUI '// &
 'and exit the iMOD-GUI and do not enter “Y” or “y” when prompted in iMODFL'// &
 'OW and exit iMODFLOW by hitting the enter-key. Furthermore do not mark th'// &
 'e “Yes, I Accept”-checkbox in the iMOD-GUI or enter “Y” or “y” when prom'// &
 'pted in iMODFLOW and end iMOD if Licensee does not agree with the License'// &
 ' Agreement. '//NEWLINE// &
 NEWLINE// &
 'WHEREAS:'//NEWLINE// &
 'Deltares owns the intellectual property of the computer program develope'// &
 'd by Deltares, including documentation, as described below under DESCRIP'// &
 'TION OF iMOD, hereinafter referred to as “iMOD”;'//NEWLINE// &
 'Licensee wishes to acquire a non-exclusive and non-transferable license,'// &
 ' without the right of sub-licensing, to use iMOD within Licensee‘s organ'// &
 'isation;'//NEWLINE// &
 'Deltares grants Licensee an iMOD-license on the following conditions.'//NEWLINE// &
 NEWLINE// &
 'AGREE AS FOLLOWS:'//NEWLINE// &
 NEWLINE// &
 'Article 1 License'//NEWLINE// &
 NEWLINE// &
 'Deltares grants Licensee a conditional, non-exclusive, non-transferable '// &
 'license without the right to sublicense or modify, to use iMOD within th'// &
 'e organisation of Licensee. This License Agreement is provided solely un'// &
 'der the condition that Licensee complies with the provisions of this Lic'// &
 'ense Agreement. '//NEWLINE// &
 NEWLINE// &
 'Article 2 Use of iMOD (including the documentation) '//NEWLINE// &
 NEWLINE// &
 '1. Licensee shall only be authorised to use iMOD within its own organisa'// &
 'tion and for its own use. Licensee shall not be permitted to make any ot'// &
 'her use of iMOD or to make available or grant access to iMOD to any thir'// &
 'd party.'//NEWLINE// &
 '2. Licensee shall not be authorised to modify and/or adjust iMOD and/or '// &
 'to (otherwise) carry out alterations to it and/or to integrate iMOD in o'// &
 'ther software, unless and only in so far as Licensee has obtained expres'// &
 's written permission to that effect in advance from Deltares. iMOD may –'// &
 ' furthermore – only be used on the hardware platform & operating system '// &
 'and the system software as defined below under OTHER REQUIREMENTS FOR TH'// &
 'E USE OF iMOD or on alternatives for the hardware platform & operating s'// &
 'ystem and/or the System Software that have been approved by Deltares in '// &
 'writing.'//NEWLINE// &
 '3. Licensee shall not be authorised to (have others) copy iMOD in any ma'// &
 'nner whatsoever or to (have others) multiply it (in any other way), exce'// &
 'pt for backup purposes. '//NEWLINE// &
 NEWLINE// &
 'Article 3 Intellectual Property Rights, Ownership '//NEWLINE// &
 NEWLINE// &
 '1. All rights, including the intellectual property rights, to iMOD and d'// &
 'ocumentation are owned by Deltares. Licensee acknowledges that this Lice'// &
 'nse Agreement does not provide Licensee with any rights or ownership to '// &
 'iMOD or documentation, including any rights to the intellectual property'// &
 '.'//NEWLINE// &
 '2. All changes to iMOD developed (or to be developed in the future) by D'// &
 'eltares will remain the intellectual property of Deltares. In so far as '// &
 'Licensee obtains any intellectual property to these features or function'// &
 'alities (other than the right to use such changes under this license), L'// &
 'icensee will transfer all intellectual property concerning the above-men'// &
 'tioned feature(s) and/or functionalities to Deltares.'//NEWLINE// &
 '3. All developments to iMOD are always intended to be distributed by Del'// &
 'tares to all licensees. Deltares shall not bind herself in any contract '// &
 'or whatsoever to limit the distribution of new developments of iMOD.'//NEWLINE// &
 '4. Deltares represents and warrants that to the best of its knowledge iM'// &
 'OD does not infringe on third party intellectual property rights.'//NEWLINE// &
 NEWLINE// &
 'Article 4 Confidentiality'//NEWLINE// &
 NEWLINE// &
 '1. Licensee shall keep confidential iMOD which Licensee has obtained and'// &
 '/or obtains, in any manner, from Deltares under or in connection with th'// &
 'e License Agreement. '//NEWLINE// &
 'This obligation shall at any rate include: '//NEWLINE// &
 'a. '//NEWLINE// &
 'Treating of iMOD confidentially; '//NEWLINE// &
 'b.'//NEWLINE// &
 'releasing iMOD solely to those employees of Licensees under  the conditi'// &
 'ons of this License Agreement who require access to iMOD, whereby Licens'// &
 'ee will oblige these employees of the same confidentiality as Licensee;'//NEWLINE// &
 'c.'//NEWLINE// &
 'The non-disclosure of information and/or data related to the License Agr'// &
 'eement to third parties and/or refraining from making such information a'// &
 'nd/or data public in any other way without the prior express and written'// &
 ' consent of Deltares, to be obtained for each separate event. '//NEWLINE// &
 'd.'//NEWLINE// &
 'Using information and/or data obtained solely for the purposes for which'// &
 ' they were obtained.'//NEWLINE// &
 NEWLINE// &
 '2. Licensee‘s obligation of confidentiality referred to in Article 4.1 s'// &
 'hall not apply to information and/or data that were already at Licensee‘'// &
 's free disposal, or were part of the public domain, or were already incl'// &
 'uded in generally accessible literature at the time when they were obtai'// &
 'ned by Licensee, or that were obtained by Licensee from a third party or'// &
 ' third parties who was or were free to disclose the relevant information'// &
 ' and/or data and who had not obtained the information and/or data from D'// &
 'eltares.'//NEWLINE// &
 NEWLINE// &
 'Article 5  No guarantee, no warrantee '//NEWLINE// &
 NEWLINE// &
 'Deltares has developed, or produced, as the case may be, iMOD to the bes'// &
 't of its abilities and in accordance with the state of art. However, Del'// &
 'tares does not give any guarantee or warrantee with respect to iMOD or i'// &
 'ts functioning and the contents thereof and/or the results obtained or t'// &
 'o be obtained with iMOD, or the correctness or quality thereof.  '//NEWLINE// &
 NEWLINE// &
 'Article 6  Duration, Termination '//NEWLINE// &
 NEWLINE// &
 '1. This License Agreement is concluded for an indefinite period, subject'// &
 ' to termination in accordance with the provisions of article 6.2 and 6.3'// &
 '. Except based on these provisions, parties are not allowed to terminate'// &
 ' the License Agreement. '//NEWLINE// &
 '2. Without prejudice to their rights to receive compensation, parties ar'// &
 'e entitled to terminate the License Agreement  in writing with immediate'// &
 ' effect, without judicial intervention being required, if the other part'// &
 'y fails to comply, or to comply timely or fully, with its obligations un'// &
 'der the License Agreement, provided that the defaulting party shall be g'// &
 'ranted one month’s time to comply with its obligations as yet, in which '// &
 'event the License Agreement shall continue to be in force.'//NEWLINE// &
 '3. Deltares shall be entitled to terminate the License Agreement forthwi'// &
 'th in writing with immediate effect, without judicial intervention being'// &
 ' required, if Licensee is adjudged bankrupt, is granted a moratorium on '// &
 'payments, is dissolved or liquidated, or if (an) application(s) to this '// &
 'end  has (have) been filled.'//NEWLINE// &
 '4. In the event of termination of the License Agreement, Licensee shall '// &
 'immediately uninstall and remove iMOD from its system(s). '//NEWLINE// &
 '5. The following provisions shall remain in full force after termination'// &
 ' of the License Agreement as set forth in this Article: Article 3.2, Art'// &
 'icle 4 and Article 7. '//NEWLINE// &
 NEWLINE// &
 'Article 7  Liability'//NEWLINE// &
 NEWLINE// &
 '1. Licensee agrees that Deltares (including its personnel and non-employ'// &
 'ees who (have) undertake(n) activities for Deltares) shall not be respon'// &
 'sible to Licensee for any loss-of-profit, direct, indirect, incidental, '// &
 'special or consequential damages arising out of the License Agreement or'// &
 ' the installation or the use of iMOD, except for damages caused by wilfu'// &
 'l act/conduct or gross negligence of Deltares or its personnel.'//NEWLINE// &
 '2. Licensee shall indemnify, hold harmless and defend Deltares against a'// &
 'ny action brought by a third party against Deltares to the extent that s'// &
 'uch a claim is connected to the use of iMOD by Licensee and/or third par'// &
 'ties at whose disposal the Licensee has placed iMOD in accordance with t'// &
 'his License Agreement and/or these results or to whom he has otherwise m'// &
 'ade them known the results, including use of the results of use by Licen'// &
 'see and/or third parties.'//NEWLINE// &
 NEWLINE// &
 'Article 8  Other provisions'//NEWLINE// &
 NEWLINE// &
 '1. Licensee is not allowed to assign any rights and/or obligations under'// &
 ' the License Agreement, entirely or in part, to third parties without th'// &
 'e prior written consent of Deltares.'//NEWLINE// &
 '2. Any disputes arising from the License Agreement or from agreements ar'// &
 'ising therefrom, shall be submitted solely to the competent court of The'// &
 ' Hague.'//NEWLINE// &
 '3. This License Agreement and all the agreements arising therefrom are g'// &
 'overned exclusively by Netherlands law.'//NEWLINE// &
 NEWLINE// &
 'DESCRIPTION OF iMOD'//NEWLINE// &
 NEWLINE// &
 'This iMOD Software License Agreement contains the following executables (<xx>: bugfix-versionnumber):'// &
 NEWLINE// &
 NEWLINE// &
 '- The iMOD Graphical User Interface (iMOD-GUI): iMOD_V5_2_X32R.exe and i'// &
 'MOD_V5_2_X64R.exe:'//NEWLINE// &
 'A computer program to perform a variety of graphical visualizations of M'// &
 'odel Configurations and/or (in)directly related geographical information'// &
 '. The iMOD GUI itself if fully written in Fortran9x and compiled by the '// &
 'Intel Visual Fortran Compiler Professional v11.1.0D054 in conjunction with'// &
 ' Winteracter 10 (Interactive Software Services Ltd (ISS)).'//NEWLINE// &
 NEWLINE// &
 '- The MODFLOW computational core (iMODFLOW):'//NEWLINE// &
 'iMODFLOW_V5_2_X32R.exe and'//NEWLINE// &
 'iMODFLOW_V5_2_METASWAP_SVN1644_X64R.exe:'//NEWLINE// &
 NEWLINE// &
 'iMODFLOW is partly based on the USGS MODFLOW2005 s'// &
 'ource code; for iMOD the USGS MODFLOW2005 source code has been expanded '// &
 'and extensively modified by Stichting Deltares. '//NEWLINE// &
 NEWLINE// &
 'The original USGS MODFLOW source code can be downloaded from the USGS we'// &
 'bsite http://www.usgs.gov/. The original MODFLOW2005 source code incorpo'// &
 'rated in the Deltares-executables is covered by the USGS Software User R'// &
 'ights Notice; you should have received a copy of this notice along with '// &
 'this program. If not, see http://water.usgs.gov/software/help/notice.'//NEWLINE// &
 NEWLINE// &
 'The X64-bit iMODFLOW-executable includes the MetaSWAP-module '// &
 'SVN version number 1644, part of SIMGRO V8_0_1_0 as described '// &
 'in the SIMGRO-release notes ftp://ftp.wur.nl/simgro/doc/Change_log/ Release_Notes_'// &
 'MetaSWAP_V8_0_1_0.pdf. '// &
 'MetaSWAP has been developed by WENR - Wageningen UR. For more '// &
 'info on MetaSWAP, see the iMOD user manual, Annex 1. For more info on WENR'// &
 ' – Wageningen UR, see https://www.wur.nl/nl/Onderzoek-Resultaten/Onderzoeks'// &
 'instituten/Environmental-Research.htm.'//NEWLINE// &
 NEWLINE// &
 'iMOD user manual'//NEWLINE// &
 NEWLINE// &
 'A pdf-file of the latest version of the iMOD-user manual, downloadable f'// &
 'rom oss.deltares.nl/web/iMOD/user-manual.'//NEWLINE// &
 NEWLINE// &
 'NETCDF'//NEWLINE// &
 NEWLINE// &
 'iMOD makes use of functions available in the NetCDF library of www.unida'// &
 'ta.ucar.edu. The file ‘NetCDF.dll’ is redistributed together with the iM'// &
 'OD-executables. The NetCDF.dll-file can be used under the conditions as '// &
 'described in http://www.unidata.ucar.edu/software/netcdf/copyright.html.'// &
 NEWLINE// &
 NEWLINE// &
 'OTHER REQUIREMENTS FOR THE USE OF iMOD'//NEWLINE// &
 NEWLINE// &
 NEWLINE// &
 'HARDWARE PLATFORM & OPERATING SYSTEM '//NEWLINE// &
 NEWLINE// &
 'iMOD works on IBM-compatible personal computers equipped with at least:'//NEWLINE// &
 '1. a Pentium or compatible processor;'//NEWLINE// &
 '2. 512 MB internal memory (2045MB recommended);'//NEWLINE// &
 '3. 100 MB available on the hard disk (10GB is recommended in case large '// &
 'model simulations need to be carried out);'//NEWLINE// &
 '4. A graphics adapter with 32 MB video memory and screen resolution of 8'// &
 '00-600 (256MB video memory and a screen resolution of 1024x768 is recomm'// &
 'end). Moreover, an graphical card that supports OpenGL (OpenGL is a trad'// &
 'emark of Silicon Graphics Inc.), such as an ATI Radeon HD or NVIDIA grap'// &
 'hical card is necessary to use the 3D rendering.'//NEWLINE// &
 NEWLINE// &
 'Please note: it is permitted to install the Model System on a different '// &
 'Hardware Platform as long as it is a computer similar to the above-menti'// &
 'oned computer. The transfer of the Model System to a dissimilar computer'// &
 ' may endanger the working of the Model System and require adjustments in'// &
 ' the Configuration. '//NEWLINE// &
 NEWLINE// &
 'iMOD-GUI is available for a 32- and 64-bit system and runs on the following '// &
 'platforms: Windows XP / Server 2003 / Vista Business / Vista Ultimate / Server 2008 / 7'// &
 '; the 64-bit version of the iMODFLOW-executable includes, the 32-bit '// &
 'version does not include MetaSWAP.'//NEWLINE// &
 NEWLINE// &
 'SYSTEM SOFTWARE'//NEWLINE// &
 NEWLINE// &
 'Adobe Acrobat is a family of application software developed by Adobe Sys'// &
 'tems to view, create, manipulate, print and manage files in Portable Doc'// &
 'ument Format (PDF). All members of the family, except Adobe Reader (form'// &
 'erly Acrobat Reader), are commercial software; Adobe Reader however, is '// &
 'available as freeware and can be downloaded from Adobe‘s web site. Adobe'// &
 ' Reader enables users to view and print PDF files but has negligible PDF'// &
 ' creation capabilities. Acrobat and Reader are widely used as a way to p'// &
 'resent information with a fixed layout similar to a paper publication.'//NEWLINE// &
 NEWLINE// &
 NEWLINE// &
 'Stichting Deltares'//NEWLINE// &
 'Boussinesqweg 1'//NEWLINE// &
 'P.O. Box 177'//NEWLINE// &
 '2600 MH Delft, The Netherlands'//NEWLINE// &
 'Tel:	 +31 (0) 88 335 82 73'//NEWLINE// &
 'Fax:	 +31 (0) 88 355 85 82'//NEWLINE// &
 'e-mail: info@deltares.nl'//NEWLINE// &
 'web:    www.deltares.com'//NEWLINE// &
 'Chamber of Commerce no. 41146461'

  WRITE(IU,'(A)') TRIM(STR)
 
  DEALLOCATE(STR)
  
 end subroutine imod_writelicense

end module driver_module
