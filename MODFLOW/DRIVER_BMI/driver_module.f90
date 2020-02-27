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

use IMOD_UTL, only : imod_utl_capf
use imod_idf_par, only : idfobj
use m_main_info
use m_vcl, only: targ
use imod_utl, only: imod_utl_closeunits, imod_utl_has_ext, imod_utl_printtext,imod_utl_getunit, imod_utl_getslash
use pks_imod_utl, only: pks_imod_utl_idfmerge_init, pks_imod_utl_write_idfmergefile, pks_imod_utl_idfmerge ! PKS
use mod_pest, only: pest1_meteo_metaswap, pest1alpha_metaswap, pest1appendlogfile, pestnext, pestdumpfct, PEST1INIT, PEST1CLOSELOGFILES
use PESTVAR, only : IUPESTOUT
use pks_imod_utl, only: pks_imod_utl_iarmwp_xch_read
use rf2mf_module, only: dis

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
interface
  logical function mf2005_GetRecharge(recharge,ncol,nrow,igrid)
      integer, intent(in) :: ncol,nrow,igrid
      real, dimension(ncol,nrow), intent(in) :: recharge
   end function
end interface
interface
  logical function mf2005_PutHeadsForLayer(head,ncol,nrow,ilay,igrid)
      integer, intent(in) :: ncol,nrow,ilay,igrid
      double precision, dimension(ncol,nrow), intent(out) :: head
   end function
end interface
interface
  logical function mf2005_GetDis(disnper,disperlen,disnstp,distsmult)
      integer, intent(out) :: disnper
      real,dimension(*),intent(out) :: disperlen
      integer,dimension(*),intent(out) :: disnstp
      real,dimension(*),intent(out) :: distsmult      
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
      real, dimension(*), intent(inout)   :: gwheads
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(*), intent(in)   :: xchOff
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
      character(len=5), intent(in)        :: act              
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

!##############################################################################
! general
 logical :: mf_steadystate
 integer :: mf_ngrid, mf_igrid, iout, mf_minid, mf_maxid

! iPEST with MetaSWAP
 integer :: mf_nrow, mf_ncol, mf_nlay

! parameters
 integer, parameter :: nsubmax = 1000

! functions
 integer, external :: osd_open2,cfn_length
 integer   cfn_idx_get_i
 double precision, external :: cfn_mjd_nodata
 logical, external :: pks7mpimasterwrite

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
 integer :: date, hour, minute, second

 logical :: lrunfile, lnamfile, llpf, lipest, lpwt, lss, lrf, psolved
 
 logical :: lwstdo, lpks, lidfmerge
 character(len=1024) :: idfmergefile
 
 integer :: isub, nsub !, nnsub
 character(len=50), dimension(nsubmax) :: submstr=''

 real :: hnoflo
 type(idfobj) :: idf

 character(len=1024) :: str
 
 real, dimension(4) :: imodusebox
 
 character(len=1) :: slash
 
! debug
 integer lswid, js, je, k, ilay, irow, icol, lun, cfn_getlun, ncvgerr

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
   do i = 1, size(lic)
      call imod_utl_printtext(trim(lic(i)),-2,iu)
   end do
   call imod_utl_printtext('',-1,iu)
   call imod_utl_printtext('I accepted the terms and conditions of the iMOD Software License Agreement on:',-2,iu)
   call imod_utl_printtext('',-2,iu)
   call imod_utl_printtext(trim(datetime),-2,iu)
   close(iu)
end if

do i = 1, size(hdr)
   call imod_utl_printtext(trim(hdr(i)),0)
end do

10 format(i2.2,'/',i2.2,'/',i4,1x,i2.2,':',i2.2,':',i2.2)

end subroutine imod_license

! ------------------------------------------------------------------------------
subroutine driver_init(config_file,nnsub,rcmd_line,stsOverRule)
!DEC$ ATTRIBUTES DLLEXPORT :: driver_init
 implicit none
! arguments
 character(len=*), intent(in) :: config_file ! nam-file, components-file or run file
 integer, intent(out) :: nnsub
 logical, intent(in), optional :: stsOverRule
 logical, intent(in) :: rcmd_line
  
 call pks7mpiini1(lwstdo) ! PKS
 call pks7mpiactive(lpks) ! PKS

! Evaluate iMOD license
 if(pks7mpimasterwrite()) call imod_license()
 call pks7mpibarrier() ! PKS
 
!... init
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

 if (rcmd_line) then
  call cfn_vcl_arg(ivcl,1,config_file,n)
 end if

 if (imod_utl_has_ext(config_file,'nam')) lnamfile= .true.
 if (imod_utl_has_ext(config_file,'run')) lrunfile= .true.
 if (lrunfile .or. lnamfile) then ! run-file or nam-file
    usemodflow=.true.
    modrecord=''
    if (lnamfile) write(modrecord,'(a,1x,3a)') '-namfile','"',trim(config_file),'"'
    if (lrunfile) then
       write(modrecord,'(a,1x,3a)') '-runfile','"',trim(config_file),'"'
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
   CALL PEST1INIT(1,pfile,IUPESTOUT,modwd1,idf,0) !'CODE OF HET UIT RUN- OF NAMFILE KOMT')
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

end subroutine driver_init

subroutine driver_init_simulation(eOS,cMZ,start_time)
 !DEC$ ATTRIBUTES DLLEXPORT :: driver_init_simulation
 use mod_pest, only: pest1_meteo_metaswap, pest1alpha_metaswap, pest1appendlogfile
 implicit none
 
 logical, intent(inout) :: eOS,cMZ
 double precision, intent(in),optional :: start_time

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
 cMZ   = .true.
 eOS   = .false.

 ! init all components TODO!!!!!!!
 if (usemodflow) then
    call osd_chdir(modwd2)
    if (usestsmodflow) call sts2init(usestsmodflow,lunsts)
    call mf2005_initComponent(modrecord,retValMF2005)
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
    call pest1appendlogfile(modwd1)
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
 if (lipest) then
    call mf2005_returnIOUT(iout)
    CALL PESTDUMPFCT(modwd1,iout)
 endif

 !#### TIMESERIES ####
 call osd_chdir(root)
 call tserie1init1(lipest,lss,hnoflo)
 ok = mf2005_TimeserieInit(mf_igrid); call driverChk(ok,'mf2005_TimeserieInit')
 call tserie1init2(lipest,lss,hnoflo,modwd1,submstr(isub))

 if (rt.eq.rtmodsimtranmoz) then
    call osd_chdir(mozwd)
    call mozart_initSimulation()
    cMZ = .false.
    eOS = .false.
    call mozart_prepareTimeStep(eOS,cMZ,currentTime) ! read signal file
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
 
end subroutine driver_init_simulation

subroutine driver_init_mozart
 !DEC$ ATTRIBUTES DLLEXPORT :: driver_init_mozart
 implicit none
 iterMozart = 0; convergedMozart = .false.
end subroutine driver_init_mozart

subroutine driver_init_timestep
 !DEC$ ATTRIBUTES DLLEXPORT :: driver_init_timestep
 implicit none

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
 
end subroutine driver_init_timestep

subroutine driver_init_iter(stssaveOverRule)
!DEC$ ATTRIBUTES DLLEXPORT :: driver_init_iter
 implicit none
 
 logical, intent(in) :: stssaveOverRule
       
 tsc=tsc+1
 timestep = timestep + 1

 call cfn_mjd2datehms(currentTime,date,hour,minute,second)

!what about steady-state solutions, currenttime.eq.0
! if(issflg(kkper).eq.1)then 
!   write(*,'(5x,a,1x,a)')&
!        'Timestep     :','steady-state'
! else
!   write(*,'(5x,a,1x,i5,1x,a,1x,i8,3(a,i2.2))')&
!       'Timestep     :',tsc,':',date,' ',abs(hour),':',minute,':',second
! endif
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
    if (usestsmodflow) call sts2saverestore(currentTime,stsave,strestore,1)
    if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz) then
       call osd_chdir(simwd2)
       call MetaSWAP_saveRestore(stsave,strestore)
       !call TRANSOL_saveRestore(stsave,strestore)
    end if
 end if

 !... init timestep
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


end subroutine driver_init_iter

subroutine driver_iter(convergedin)
!DEC$ ATTRIBUTES DLLEXPORT :: driver_iter
 implicit none
 logical, intent(inout) :: convergedin

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
    convergedin = .true.
    convergedin = convergedin .and. convergedMF2005
    if (rt.eq.rtmodsim .or. rt.eq.rtmodsimtranmoz)&
        convergedin = convergedin .and. convergedMetaSwap

    if (retValMF2005.ne.0) exitcode = -261
 
end subroutine driver_iter

subroutine driver_finish_timestep(convergedin,eOS,dTL)
!DEC$ ATTRIBUTES DLLEXPORT :: driver_finish_timestep
 implicit none
 
 logical, intent(in) :: convergedin
 logical, intent(inout) :: eOS,dTL
 
 if (.not.convergedin) then
    if (pks7mpimasterwrite()) write(*,*) ' Model did not converge!',exitcode
 end if   

! ... write results
 if (convergedin .and. exitcode.eq.0) then
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
    call tserie1write(0,lss,currentTime,hnoflo,usests,modwd1,submstr(isub),time_string)

    !#### TIMESERIES #####
 endif
 if (exitcode.eq.0) then
    ! perform state save, phase 2 (after reading and writing data)
    if (usestsmodflow) call sts2saverestore(currentTime,stsave,strestore,2)
    if (mozstsave) then
        stsave    = .false.
        strestore = .false.
        mozstsave = .false.
    end if
 endif

! ... next timestep
 if (exitcode.ne.0) then
    ! ERROR, do not continue
    eOS=.true.
 else
    ! get new time information
    ! Let Modflow be leading. don't ask the other components
    call mf2005_getBeginOfNextTimeStep(BeginOfNextTimeStepMF2005,retValMF2005)
    ! find errors
    if (retValMF2005.eq.-1) then
    ! end of simulation for Modflow
        eOS=.true.
        retValMF2005   =0
    endif
    if (retValMF2005.ne.0) exitcode = -31
    if (exitcode.ne.0)     eOS=.true.
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

        if (currentTime.eq.nodataTime.and.rt.ne.rtmod) eOS=.true.
!           if (currentTime.eq.nodataTime) eOS=.true.
    endif

    if (rt.eq.rtmodsimtranmoz) then
        dTL = currenttime.lt.endOfCurrentTimeStepMozart .and. .not.eOS
    else
        dTL = .not.eOS
    endif

 endif

 if (exitcode.ne.0) dTL = .false.

 end subroutine driver_finish_timestep
!
subroutine driver_finish_simulation(eOS,cMz)
 !DEC$ ATTRIBUTES DLLEXPORT :: driver_finish_simulation
 implicit none
 logical, intent(out) :: eOS,cMz

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
    call mozart_prepareTimeStep(eOS,cMZ,currentTime) ! read signal file
 end if

 ! make sure MODFLOW-MetaSWAP terminates normally
 if (rt.eq.rtmodsim) then
    cMZ = .true.
 end if
  
 if (exitcode.ne.0) eOS = .True.

end subroutine driver_finish_simulation

subroutine driver_finish_pest(cPst)
 !DEC$ ATTRIBUTES DLLEXPORT :: driver_finish_pest
 use imod_utl, only: imod_utl_closeunits
 use mod_pest, only: pestnext
 implicit none
 logical, intent(inout) :: cPst

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
 call tserie1write(1,lss,currentTime,hnoflo,usests,modwd1,submstr(isub),time_string)
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
    cPst=.true.
 else
  cPst=pestnext(lss,modwd1)
 end if
! call imod_utl_closeunits()

end subroutine driver_finish_pest

end module driver_module
