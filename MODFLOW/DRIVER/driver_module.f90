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
   logical function mf2005_PutModSimNumberOfIDs(igrid, nxch)
      integer, intent(in) :: igrid
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
integer, intent(in)                    :: nid1 ! number of indices
integer, intent(in)                    :: nid2 ! number of indices
integer, intent(in)                    :: nidx1 ! length of idx1
integer, dimension(nid1),  intent(in)  :: id1 ! indices
integer, dimension(nid2),  intent(in)  :: id2 ! indices
integer, dimension(nidx1), intent(out) :: idx1 ! idexes
integer, dimension(nid1),  intent(out) :: off1 ! off-sets

! locals
type tMap
   integer :: n = 0
   integer :: i = -1
   integer, dimension(:), pointer :: iarr
end type tMap
type(tMap), dimension(:), allocatable :: mapidx
integer :: i, j, k, n, m, n1, n2, idval, i1s, i1e, i2s, i2e
integer, dimension(:), allocatable :: id1s, id2s, id1si, id2si, itmp
!.......................................................................

! allocate temporary data
allocate(id1s(nid1), id2s(nid2), id1si(nid1), id2si(nid2))
allocate(mapidx(nid1), itmp(nid2))

! sort the indices (quick sort)
id1s = id1
id2s = id2
do i = 1, nid1
   id1si(i) = i
end do
do i = 1, nid2
   id2si(i) = i
end do
call qsort(id1s,id1si)
call qsort(id2s,id2si)

! store mapping indices
n1 = 1
n2 = 1
itmp = 0
do while(.true.)
   if (n1.gt.nid1 .or. n2.gt.nid2) exit
   idval = id1s(n1)
   call findid(id1s,nid1,idval,n1,i1s,i1e)
   call findid(id2s,nid2,idval,n2,i2s,i2e)
   if (i2s.gt.0 .and. i2e.gt.0) then
      do i = i1s, i1e
         j = id1si(i)
         m = i2e-i2s+1
         mapidx(j)%n = m
         if (m.eq.1) then
            mapidx(j)%i = id2si(i2s)
         else
            allocate(mapidx(j)%iarr(m))
            do k = i2s, i2e
               mapidx(j)%iarr(k-i2s+1) = id2si(k)
            end do
            ! sort (this is not really necessary)
            call qsort(mapidx(j)%iarr, itmp(1:m))
         end if
      end do
   end if
end do

! create index array and offset array
k = 0
off1 = 0
do i = 1, nid1
   m = mapidx(i)%n
   if (m.eq.1) then
      k = k + 1
      idx1(k) = mapidx(i)%i
   else if (m.gt.1) then
      do j = 1, m
         k = k + 1
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
   m = mapidx(i)%n
   if (m.gt.1) deallocate(mapidx(i)%iarr)
end do
deallocate(mapidx)
deallocate(id1s, id2s, id1si, id2si, itmp)

end subroutine mapIds

! ------------------------------------------------------------------------------

subroutine driverXchInitModSim()

   ! allocate the MOD-SIM exchange arrays
   allocate(XchModSimModIds(XchModSimModNID))
   allocate(XchModSimModHeads(XchModSimModNID))
   XchModSimModHeads = mv
   allocate(XchModSimModCells(3,XchModSimModNID))

   ! allocate exchange arrays
   allocate(XchModSimSimIds(XchModSimSimNID),&
            XchModSimSimUnsZFlux(XchModSimSimNID),&
            XchModSimSimStrFct(XchModSimSimNID))
   XchModSimSimUnsZFlux = mv
   XchModSimSimStrFct = mv

end subroutine driverXchInitModSim

subroutine driverXchInitModSimTranMoz()

   ! allocate arrays to store the IDs
   allocate(XchMozIds(XchMozNID))
   allocate(XchMozPVIds(XchMozPVNID))
   allocate(XchModMozModIds(XchModMozModNID))
   allocate(XchModMozPVModIds(XchModMozPVModNID))
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

   integer :: n

   ! allocate arrays to map MetaSWAP to MODFLOW IDs
   n = max(XchModSimSimNID,XchModSimModNID)
   allocate(XchSim2ModIdx(n), XchSim2ModOff(XchModSimModNID))
   allocate(XchMod2SimIdx(n), XchMod2SimOff(XchModSimSimNID))
   ! get mapping MODFLOW -> MetaSWAP IDs
   write(*,*) 'Constructing coupling tables: MODFLOW      --> MetaSWAP'
   call mapIds(XchMod2SimIdx,n,XchMod2SimOff,XchModSimSimIds,XchModSimSimNID,&
                                           XchModSimModIds,XchModSimModNID)
   ! get mapping MetaSWAP -> MODFLOW IDs
   write(*,*) 'Constructing coupling tables: MetaSWAP     --> MODFLOW'
   call mapIds(XchSim2ModIdx,n,XchSim2ModOff,XchModSimModIds,XchModSimModNID,&
                                           XchModSimSimIds,XchModSimSimNID)

end subroutine driverXchIniMapModSim

subroutine driverXchIniMapModMoz()

   integer :: n

   ! MODFLOW - MOZART coupling (LSW)
   n = max(XchMozNID,XchModMozModNID)
   allocate(XchMoz2ModIdx(n), XchMoz2ModOff(XchModMozModNID))
   allocate(XchMod2MozIdx(n), XchMod2MozOff(XchMozNID))
   ! mapping MODFLOW --> MOZART (LSW)
   write(*,*) 'Constructing coupling tables: MODFLOW      --> MOZART (LSW)'
   call mapIds(XchMod2MozIdx,n,XchMod2MozOff,XchMozIds,      XchMozNID,&
                                               XchModMozModIds,XchModMozModNID)
   ! mapping MOZART (LSW) --> MODFLOW
   write(*,*) 'Constructing coupling tables: MOZART (LSW) --> MODFLOW'
   call mapIds(XchMoz2ModIdx,n,XchMoz2ModOff,XchModMozModIds,XchModMozModNID,&
                                               XchMozIds,      XchMozNID)
end subroutine driverXchIniMapModMoz

subroutine driverXchIniMapModMozPV()

   integer :: n

   ! MODFLOW - MOZART coupling (PV)
   n = max(XchMozPVNID,XchModMozPVModNID)
   allocate(XchMozPV2ModIdx(n), XchMozPV2ModOff(XchModMozPVModNID))
   allocate(XchMod2MozPVIdx(n), XchMod2MozPVOff(XchMozPVNID))
   ! mapping MODFLOW --> MOZART (PV)
   write(*,*) 'Constructing coupling tables: MODFLOW      --> MOZART (PV)'
   call mapIds(XchMod2MozPVIdx,n,XchMod2MozPVOff,XchMozPVIds,      XchMozPVNID,&
                                                 XchModMozPVModIds,XchModMozPVModNID)
   ! mapping MOZART (PV) --> MODFLOW
   write(*,*) 'Constructing coupling tables: MOZART (PV)  --> MODFLOW'
   call mapIds(XchMozPV2ModIdx,n,XchMozPV2ModOff,XchModMozPVModIds,XchModMozPVModNID,&
                                                  XchMozPVIds,      XchMozPVNID)
end subroutine driverXchIniMapModMozPV

subroutine driverXchIniMapSimMoz()

   integer :: n

   ! MetaSwap - MOZART coupling
   n = max(XchMozNID,XchSimMozSimNID)
   allocate(XchMoz2SimIdx(n), XchMoz2SimOff(XchSimMozSimNID))
   allocate(XchSim2MozIdx(n), XchSim2MozOff(XchMozNID))
   ! mapping MetaSWAP --> MOZART
   write(*,*) 'Constructing coupling tables: MetaSWAP     --> MOZART'
   call mapIds(XchSim2MozIdx,n,XchSim2MozOff,XchMozIds,      XchMozNID,&
                                               XchSimMozSimIds,XchSimMozSimNID)
   ! mapping MOZART  --> MetaSWAP
   write(*,*) 'Constructing coupling tables: MOZART       --> MetaSWAP'
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
                    imod_utl_printtext, imod_utl_openasc

implicit none

! parameters
integer, parameter :: nlines = 15
character(len=1024), dimension(nlines) :: lic
data lic/'====================================================================',& !01
         'You may use this compiled version of the iMOD-software if you are '  ,& !02
         'entitled to this use under a iMOD software license agreement for the',& !03
         'iMOD software executables with Deltares or with a party entitled by' ,& !04
         'Deltares to provide sublicenses for the iMOD-software executables.'  ,& !05
         'Otherwise use of this compiled version of the iMOD-software is'      ,& !06
         'prohibited and illegal. If you are not allowed under a Deltares iMOD',& !07
         'license agreement to use the iMOD-software executables, you may find',& !08
         'a solution in compiling the open source version of the iMOD-software',& !09
         'into an executable yourself (see oss.deltares.nl), or apply for a'   ,& !10
         'Deltares iMOD license agreement by sending an email to'              ,& !11
         '   sales@deltares.nl. '                                              ,& !12
         ''                                                                    ,& !13 
         'Version 3.01.00, 01/07/15'                                           ,& !14
         '===================================================================='/  !15

! locals
character(len=1024) :: dir, licfile, agreestr
character(len=1024) :: key
logical :: lex, lagree
integer :: i, iu
integer, dimension(8) :: iedt

! write license to standard output
do i = 1, size(lic)
   call imod_utl_printtext(trim(lic(i)),0)
end do

call getarg(0,dir) ! get full path of the executable
call imod_utl_getdir(dir) ! get the directory (last character is a slash)
write(licfile,'(2a)') trim(dir),'license_agreement.txt'
inquire(file=licfile,exist=lex) ! check if file exists
if (.not.lex) then
   call imod_utl_printtext('Do you agree on using iMOD under the conditions stated in the',0) 
   call imod_utl_printtext('iMOD licence agreement that can be found in <XXXXXXXXX>?',0)
   call imod_utl_printtext('',0)
   call imod_utl_printtext('Please enter (Yes/No) followed by (Enter).',0)
   lagree = .false.
   do while(.true.)
      read(*,*) key     
      call imod_utl_s_cap(key,'l')
      select case(key)
      case('y','yes')
         call date_and_time(values=iedt)
         write(agreestr,10)(iedt(i),i=3,1,-1),(iedt(i),i=5,7) ! (yyyy/mm/dd hh:mm:ss)
         lagree = .true.
         exit
      case('n','no')
         call imod_utl_printtext('You have not agreed on the iMOD license, exiting program.',0)
         stop 1
      case default
         call imod_utl_printtext('Invalid input, please enter (Yes/No) followed by (Enter).',0)
      end select   
   end do
end if    

! If agreed, then write license file
if (lagree) then 
   call imod_utl_printtext('Writing license agreement:',0)
   call imod_utl_printtext('',0)
   iu=imod_utl_getunit()
   call imod_utl_openasc(iu,licfile,'w')    
   call imod_utl_printtext(trim(agreestr),-1,iu)
   call imod_utl_printtext('',-1,iu)
   do i = 1, size(lic)
      call imod_utl_printtext(trim(lic(i)),-1,iu)
   end do
   close(iu)
end if

10 format('Accepted on:',1x,i2.2,'/',i2.2,'/',i4,1x,i2,':',i2.2,':',i2.2)

end subroutine imod_license

end module driver_module
