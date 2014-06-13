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

 !> description
 !! module for Data eXChange between components
 module gwfdxcmodule

    ! declaration section
    ! ------------------------------------------------------------------------------
    logical, save   :: notransol = .false.

    ! nodata value used by dxc
    real, parameter  :: dxcmv=-9.99e28

    integer, parameter     ::  nsid=256         !> size of schemID

    integer, parameter :: maxnhrivsys = 100

    ! data exchange elements projected to the local 3D modflow grid
    integer, pointer, save :: mxdxc             !> maximum number of data exchange elements stored
    integer, pointer, save :: ndxc              !> number of data exchange elements stored
    integer, pointer, save :: dxcid(:)          !> element identification number
    integer, pointer, save :: dxcic(:)          !> local grid column number
    integer, pointer, save :: dxcir(:)          !> local grid row    number
    integer, pointer, save :: dxcil(:)          !> local grid layer  number

    real   , pointer, save :: dxchead(:)        !> head           data to be exchanged: GWMHeads
    real   , pointer, save :: dxcuzflux(:)      !> UnsatZoneFlux  data to be exchanged: UnsaturatedZoneFlux
    real   , pointer, save :: dxcsf(:)          !> storage factor data to be exchanged: StorageFactor

    character (len=nsid), pointer, save :: schemIDdxc !> schematisation ID

    ! mozart
    integer, pointer, save :: maxlsw
    integer, pointer, save :: ndxclsw
    integer, pointer, save :: ndxcpv
    integer, pointer, save :: dxciclsw(:)
    integer, pointer, save :: dxcirlsw(:)
    integer, pointer, save :: dxcidlsw(:)
    real, pointer, save    :: dxcowclsw(:)
    integer, pointer, save :: dxcicpv(:)
    integer, pointer, save :: dxcirpv(:)
    integer, pointer, save :: dxcidpv(:)
    real, pointer, save    :: dxclevlsw(:)
    real, pointer, save    :: dxclevpv(:)
    real, pointer, save    :: levels(:,:)
    integer, pointer, save :: nhrivsys
    integer, pointer, save :: hrivsys(:)
    real, pointer,dimension(:,:),save ::seepageconc

    type gwfdxctype
       integer, pointer :: mxdxc
       integer, pointer :: ndxc
       integer, pointer :: dxcid(:)
       integer, pointer :: dxcic(:)
       integer, pointer :: dxcir(:)
       integer, pointer :: dxcil(:)

       real   , pointer :: dxchead(:)
       real   , pointer :: dxcuzflux(:)
       real   , pointer :: dxcsf(:)

       character (len=nsid), pointer :: schemIDdxc

       ! mozart
       integer, pointer :: maxlsw
       integer, pointer :: ndxclsw
       integer, pointer :: ndxcpv
       integer, pointer :: dxciclsw(:)
       integer, pointer :: dxcirlsw(:)
       integer, pointer :: dxcidlsw(:)
       real, pointer    :: dxcowclsw(:)
       integer, pointer :: dxcicpv(:)
       integer, pointer :: dxcirpv(:)
       integer, pointer :: dxcidpv(:)
       real, pointer    :: dxclevlsw(:)
       real, pointer    :: dxclevpv(:)
       real, pointer    :: levels(:,:)
       integer, pointer :: nhrivsys
       integer, pointer :: hrivsys(:)
       real, pointer,dimension(:,:) ::seepageconc
    end type

    type(gwfdxctype), save:: gwfdxcdat(10)

 end module gwfdxcmodule
