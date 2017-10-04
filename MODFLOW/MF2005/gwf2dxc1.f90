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

!> description
!! allocate arrays and read data
subroutine gwf2dxc1ar(in,igrid)

! declaration section
! ------------------------------------------------------------------------------
 USE GLOBAL,      ONLY:IOUT,NCOL,NROW
 use gwfdxcmodule
 use imod_utl, only: imod_utl_rel_to_abs, imod_utl_qksort3
 use pks_iarmwp, only: liarmwp, nrxp, xp
 
 implicit none


! arguments
 integer  , intent(in)     :: in          !> unit number of input file
 integer  , intent(in)     :: igrid       !> grid number


! local variables
 integer    :: i,n,ilay,irow,icol,id

 real       :: usf

 logical    :: used

 character (len=256) :: line

 real       :: r
 integer    :: lloc,istart,istop

 character(len=256) :: fname, label
 logical :: mozartcoupling
 integer :: lun, cfn_getlun, ios, lswid, pvid
 real :: owc
 logical :: lused, lfirst
 double precision :: mask ! PKS
 integer :: ixp, ok1, ok2, j, nn, nid, ndum, iact ! PKS
 integer :: cfn_unique_i, cfn_idx_get_i ! PKS
 integer, dimension(:), allocatable :: iwrk1, iwrk2, iwrk3, iwrk4

! program section
! ------------------------------------------------------------------------------
! set pointer
 call sgwf2bas7pnt(igrid)

! package id
 write(iout,'(/,9x,a,i4)') ' DXC -- Data eXChange PACKAGE, VERSION 1'// &
                   ', 29 june 2011 INPUT READ FROM UNIT',in

! allocate scalars
 allocate(mxdxc,ndxc,minid,maxid,idxccb)

! read maximum number of data exchange elements and the Schematisation ID
 read(in,'(a)') line
 lloc=1
 ! get maximum number of data exchange elements
 call urword(line,lloc,istart,istop,2,mxdxc,r,iout,in)
 ! get schematisation id
 call urword(line,lloc,istart,istop,2,idxccb,r,iout,in)

! allocate arrays

! Add dummy ID's for iarmwp = 1: count
 if (liarmwp) then ! PKS
    ndum = 0  ! PKS 
    do ixp = 1, nrxp ! PKS
       ndum = ndum + xp(ixp)%nid ! PKS
    end do ! PKS
    mxdxc = mxdxc + ndum ! PKS
 end if ! PKS
 
 allocate(dxcid(mxdxc),dxcic(mxdxc),dxcir(mxdxc),dxcil(mxdxc))

! read data
 read(in,*) ndxc

 n=0
 lfirst = .true.
 do i=1,ndxc
    ! read one line and convert indices to local grid when necessary
    read(in,*) ilay,irow,icol,id
    if (lfirst) then
       minid = id
       maxid = id
       lfirst = .false.       
    end if
    minid = min(minid,id)
    maxid = max(maxid,id) 
    call pks7mpitrn(icol,irow,ilay,lused) ! PKS
    if(.not.lused) cycle ! PKS
    call pks7mpimask( mask, icol, irow, ilay ) ! PKS
    if (mask.lt.0.5d0) id = -id ! PKS
    if (n.lt.mxdxc) then
       n=n+1
       dxcic(n)=icol
       dxcir(n)=irow
       dxcil(n)=ilay
       dxcid(n)=id
    else
       write(iout,'(a)') ' ERROR, Not enough elements allocated for DXC package.'
       call exit(11)
    endif
 enddo

 ! store number of read elements
 ndxc=n
 
! PKS: Add dummy ID's for iarmwp = 1: add
 if (liarmwp) then ! PKS
    allocate(iwrk1(max(1,ndxc)),iwrk2(max(1,ndxc)))
    do i = 1, ndxc
       iwrk1(i) = abs(dxcid(i))
       iwrk2(i) = i
    end do
    call imod_utl_qksort3(iwrk1,iwrk2)
    ! count interface nodes 
    nn = 0 
    do ixp = 1, nrxp
       nn = nn + xp(ixp)%nid
    end do
    allocate(iwrk3(max(1,nn)),iwrk4(max(1,nn)))
    ! fill with interface nodes
    nn = 0
    do ixp = 1, nrxp
       do i = 1, xp(ixp)%nid
          id = xp(ixp)%id(i)
          nn = nn + 1
          iwrk3(nn) = id
          iwrk4(nn) = nn
       end do
    end do
    ! get unique interface nodes
    if (nn.gt.0) then 
       call imod_utl_qksort3(iwrk3,iwrk4)
       nid = cfn_unique_i(iwrk3,nn,0)
    end if
    iwrk4 = 0
    do ixp = 1, nrxp
       do i = 1, xp(ixp)%nid
          id = xp(ixp)%id(i)
          ok1 = cfn_idx_get_i(id,iwrk1,ndxc,j)
          if(ok1.ne.0) then ! index found -- do nothing
             xp(ixp)%idx(i) = -iwrk2(j)
          else
             ok2 = cfn_idx_get_i(id,iwrk3,nid,j)
             if (iwrk4(j).eq.0) then ! first unique index, so add 
                n = n + 1
                dxcic(n)=0
                dxcir(n)=0
                dxcil(n)=0
                dxcid(n)=id
             end if
             iwrk4(j) = 1
             xp(ixp)%idx(i) = n
          end if
       end do 
    end do
    ndxc=n
    deallocate(iwrk1,iwrk2,iwrk3,iwrk4)
 end if !PKS

! allocate arrays for parameter values
! at least 1 element
 n=max(ndxc,1)
 allocate(dxchead(n),dxcuzflux(n),dxcsf(n))

! read mozart coupling table
 allocate(maxlsw,ndxclsw,ndxcpv)
 maxlsw  = 1
 ndxclsw = 0
 ndxcpv  = 0
 mozartcoupling = .false.
 read(unit=in,fmt='(a)',iostat=ios) line
 if (ios.eq.0) then
    call cfn_token(line ,'tcu')
    if (index(line,'MOZART').gt.0) then
       mozartcoupling = .true.
       read(in,*) maxlsw
    else
       maxlsw = 1
    end if
 end if

 allocate(dxciclsw(maxlsw), dxcirlsw(maxlsw), dxcidlsw(maxlsw), dxcowclsw(maxlsw))
 allocate(dxcicpv(maxlsw), dxcirpv(maxlsw), dxcidpv(maxlsw))
 allocate(dxclevlsw(maxlsw), dxclevpv(maxlsw))
 allocate(nhrivsys, hrivsys(maxnhrivsys))
 allocate(seepageconc(ncol,nrow))
 nhrivsys = 0
 dxclevlsw = dxcmv
 dxclevpv = dxcmv

 if (mozartcoupling) then
    read(in,'(a)') fname
    call imod_utl_rel_to_abs(dxcroot,fname)
    lun = cfn_getlun(10,99)
    open(unit=lun,file=fname)
    read(lun,'(a)') line
    do while(.true.)
       read(unit=lun,fmt=*,iostat=ios) icol, irow, lswid, pvid, owc
       if (ios.ne.0) exit
       call pks7mpitrn(icol,irow,1,lused) ! PKS
       if(.not.lused) cycle ! PKS      
       ndxclsw = ndxclsw + 1
       dxciclsw(ndxclsw) = icol
       dxcirlsw(ndxclsw) = irow
       dxcidlsw(ndxclsw) = lswid
       dxcowclsw(ndxclsw) = owc
       if (pvid.gt.0) then
          ndxcpv = ndxcpv + 1
          dxcicpv(ndxcpv) = icol
          dxcirpv(ndxcpv) = irow
          dxcidpv(ndxcpv) = pvid
       end if
    end do
    close(lun)
    allocate(levels(ncol,nrow))
 end if

!7------SAVE POINTERS TO DATA AND RETURN.
 CALL SGWF2dxc1PSV(IGRID)

 read(unit=in,fmt='(a)',iostat=ios) line
 if (ios.eq.0) then
    call cfn_token(line ,'tcu')
    if (index(line,'NOTRANSOL').gt.0) then
       notransol = .true.
       label = 'concentration'
       call u2drel(seepageconc,label,nrow,ncol,1,in,iout)
       call sgwf2bcf7psv(igrid)
    end if
 end if

! end of program
 return
end

subroutine gwf2dxc1rp(igrid)
! modules
use gwfdxcmodule
use gwfrivmodule, only: rivr, nriver, nrivvl, irivsubsys
use gwfdrnmodule, only: drai, ndrain, ndrnvl, idrnsubsys, drnlev

implicit none

! arguments
integer, intent(in) :: igrid

! locals
logical :: lskip
integer :: i, j, icol, irow, n
real :: lev, rbot, elev

! set pointers
call sgwf2dxc1pnt(igrid)
call sgwf2riv7pnt(igrid)
call sgwf2drn7pnt(igrid)

if (ndxclsw.eq.0 .and. ndxcpv.eq.0) return
if (nhrivsys.eq.0) then
   write(*,*) 'Warning, no rivers defined to skip.'
end if

! fill levels
levels = dxcmv
! LSW
do i = 1, ndxclsw
   icol = dxciclsw(i)
   irow = dxcirlsw(i)
   lev  = dxclevlsw(i)
   levels(icol,irow) = lev
end do
! PV
do i = 1, ndxcpv
   icol = dxcicpv(i)
   irow = dxcirpv(i)
   lev  = dxclevpv(i)
   levels(icol,irow) = lev
end do
! OWC
do i = 1, ndxclsw
   icol = dxciclsw(i)
   irow = dxcirlsw(i)
   lev = levels(icol,irow)
   if (lev.ne.dxcmv) then
      levels(icol,irow) = lev + dxcowclsw(i)
   end if
end do

! correct rivers
if (associated(nriver)) then
   if (irivsubsys.eq.0) then
      write(*,*) 'Error: no river subsystems found.'
      call ustop(' ')
   end if
   do i = 1, nriver
      lskip = .false.
      do j = 1, nhrivsys
         if (int(rivr(irivsubsys,i)).eq.hrivsys(j)) lskip = .true.
      end do
      if (.not.lskip) then
         irow = rivr(2,i)
         icol = rivr(3,i)
         rbot = rivr(6,i)
         lev = levels(icol,irow)
         if (lev.ne.dxcmv) then
            rivr(4,i) = max(lev, rbot)
         end if
      end if
   end do
   call sgwf2riv7psv(igrid)
else
   write(*,*) 'Error: no rivers set.'
   call ustop(' ')
end if

! correct drains
if (associated(ndrain)) then
   do i = 1, ndrain
      irow = drai(2,i)
      icol = drai(3,i)
      elev = drnlev(i) ! get original drain levels
      lev = levels(icol,irow)
      if (lev.ne.dxcmv) then
          drai(4,i) = max(lev, elev)
      end if
   end do
   call sgwf2drn7psv(igrid)
else
   write(*,*) 'Error: no drains set.'
   call ustop(' ')
end if

end subroutine

!> description
!! perform formulate for DXC Flux
subroutine gwf2dxc1fm(igrid)

! declaration section
! ------------------------------------------------------------------------------
 use global,       only:ibound,rhs
 use gwfdxcmodule

 implicit none


! arguments
 integer  , intent(in)     :: igrid       !> grid number


! local variables
 integer   i,icol,irow,ilay

 real      q


! program section
! ------------------------------------------------------------------------------

! set pointers
 call sgwf2bas7pnt(igrid)
 call sgwf2dxc1pnt(igrid)

! perform formulate
 do i=1,ndxc
    icol=dxcic(i)
    irow=dxcir(i)
    ilay=dxcil(i)
    if(ilay.eq.0) cycle ! PKS
    q   =dxcuzflux(i)
    if (q.ne.dxcmv) then
       ! not a missing value
       if (ibound(icol,irow,ilay).gt.0) then
          ! Active Cell
          ! add flux to the RHS
          rhs(icol,irow,ilay)=rhs(icol,irow,ilay)-q
       endif
    endif
 enddo

! save pointers
 call sgwf2bas7psv(igrid)

! end of program
 return
end

! ******************************************************************************

!> description
!! calculate budget terms for DXC fluxes
subroutine gwf2dxc1bd(KSTP,KPER,igrid)

! declaration section
! ------------------------------------------------------------------------------
 USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,BUFF
 USE GWFBASMODULE,ONLY:MSUM,ICBCFL,IAUXSV,DELT,PERTIM,TOTIM,VBVL,VBNM
 use gwfdxcmodule

 implicit none


! arguments
 integer  , intent(in)     :: kstp        !> time step number
 integer  , intent(in)     :: kper        !> stress period number
 integer  , intent(in)     :: igrid       !> grid number


! local variables
 integer   i,icol,irow,ilay
 real      rin,rout,q,zero

 integer   ibd,ibdlbl

 CHARACTER (len=16)  :: TEXT
 DOUBLE PRECISION    ::  RATIN,RATOUT

 DATA TEXT /'      DXC FLUXES'/


! program section
! ------------------------------------------------------------------------------


! set pointers
 call sgwf2dxc1pnt(igrid)


!C1------CLEAR RATIN AND RATOUT ACCUMULATORS, AND SET CELL-BY-CELL
!C1------BUDGET FLAG.
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
      IBD=0
      IF(IdxcCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IdxcCB.GT.0) IBD=ICBCFL
      IBDLBL=0

!C3------CLEAR THE BUFFER.
 DO ilay=1,NLAY
    DO irow=1,NROW
       DO icol=1,NCOL
          BUFF(ICol,IRow,ILay)=ZERO
       enddo
    enddo
 enddo

 do i=1,ndxc
    icol=dxcic(i)
    irow=dxcir(i)
    ilay=dxcil(i)
    if (ilay.eq.0) cycle ! PKS
    q   =dxcuzflux(i)
    if (q.ne.dxcmv) then
       ! not a missing value
       if (ibound(icol,irow,ilay).gt.0) then
          ! Active Cell
          ! add flux to the RHS
          buff(icol,irow,ilay)=buff(icol,irow,ilay)+q
          if (q.ge.zero) then
             ratin =ratin  + q
          else
             ratout=ratout - q
          endif
       endif
    endif
 enddo


!C6------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A 3-D ARRAY,
!C6------CALL UBUDSV TO SAVE THEM.
 IF (IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IdxcCB,BUFF,NCOL,NROW,NLAY,IOUT)

!C7------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
 RIN=RATIN
 ROUT=RATOUT
 VBVL(3,MSUM)=RIN
 VBVL(4,MSUM)=ROUT
 VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
 VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
 VBNM(MSUM)=TEXT

!C8------INCREMENT BUDGET TERM COUNTER(MSUM).
 MSUM=MSUM+1


! end of program
 return
end

! ******************************************************************************

!> description
!! save pointers
subroutine sgwf2dxc1psv(igrid)

! declaration section
! ------------------------------------------------------------------------------
  use gwfdxcmodule

 implicit none


! arguments
 integer  , intent(in)     :: igrid       !> grid number


! program section
! ------------------------------------------------------------------------------

 gwfdxcdat(igrid)%mxdxc     => mxdxc
 gwfdxcdat(igrid)%idxccb    => idxccb
 gwfdxcdat(igrid)%ndxc      => ndxc
 gwfdxcdat(igrid)%dxcid     => dxcid
 gwfdxcdat(igrid)%dxcic     => dxcic
 gwfdxcdat(igrid)%dxcir     => dxcir
 gwfdxcdat(igrid)%dxcil     => dxcil
 gwfdxcdat(igrid)%minid     => minid
 gwfdxcdat(igrid)%maxid     => maxid
 gwfdxcdat(igrid)%dxchead   => dxchead
 gwfdxcdat(igrid)%dxcuzflux => dxcuzflux
 gwfdxcdat(igrid)%dxcsf     => dxcsf

 gwfdxcdat(igrid)%maxlsw    => maxlsw
 gwfdxcdat(igrid)%ndxclsw   => ndxclsw
 gwfdxcdat(igrid)%ndxcpv    => ndxcpv
 gwfdxcdat(igrid)%dxciclsw  => dxciclsw
 gwfdxcdat(igrid)%dxcirlsw  => dxcirlsw
 gwfdxcdat(igrid)%dxcidlsw  => dxcidlsw
 gwfdxcdat(igrid)%dxcowclsw => dxcowclsw
 gwfdxcdat(igrid)%dxcicpv   => dxcicpv
 gwfdxcdat(igrid)%dxcirpv   => dxcirpv
 gwfdxcdat(igrid)%dxcidpv   => dxcidpv
 gwfdxcdat(igrid)%dxclevlsw => dxclevlsw
 gwfdxcdat(igrid)%dxclevpv  => dxclevpv
 gwfdxcdat(igrid)%nhrivsys  => nhrivsys
 gwfdxcdat(igrid)%hrivsys   => hrivsys
 gwfdxcdat(igrid)%seepageconc => seepageconc

! end of program
 return
end

! ******************************************************************************

!> description
!! restore pointers
subroutine sgwf2dxc1pnt(igrid)

! declaration section
! ------------------------------------------------------------------------------
  use gwfdxcmodule

 implicit none


! arguments
 integer  , intent(in)     :: igrid       !> grid number


! program section
! ------------------------------------------------------------------------------

 mxdxc     => gwfdxcdat(igrid)%mxdxc
 idxccb    => gwfdxcdat(igrid)%idxccb
 ndxc      => gwfdxcdat(igrid)%ndxc
 dxcid     => gwfdxcdat(igrid)%dxcid
 dxcic     => gwfdxcdat(igrid)%dxcic
 dxcir     => gwfdxcdat(igrid)%dxcir
 dxcil     => gwfdxcdat(igrid)%dxcil
 minid     => gwfdxcdat(igrid)%minid
 maxid     => gwfdxcdat(igrid)%maxid
 dxchead   => gwfdxcdat(igrid)%dxchead
 dxcuzflux => gwfdxcdat(igrid)%dxcuzflux
 dxcsf     => gwfdxcdat(igrid)%dxcsf

 maxlsw    => gwfdxcdat(igrid)%maxlsw
 ndxclsw   => gwfdxcdat(igrid)%ndxclsw
 ndxcpv    => gwfdxcdat(igrid)%ndxcpv
 dxciclsw  => gwfdxcdat(igrid)%dxciclsw
 dxcirlsw  => gwfdxcdat(igrid)%dxcirlsw
 dxcidlsw  => gwfdxcdat(igrid)%dxcidlsw
 dxcowclsw => gwfdxcdat(igrid)%dxcowclsw
 dxcicpv   => gwfdxcdat(igrid)%dxcicpv
 dxcirpv   => gwfdxcdat(igrid)%dxcirpv
 dxcidpv   => gwfdxcdat(igrid)%dxcidpv
 dxclevlsw => gwfdxcdat(igrid)%dxclevlsw
 dxclevpv  => gwfdxcdat(igrid)%dxclevpv
 nhrivsys  => gwfdxcdat(igrid)%nhrivsys
 hrivsys   => gwfdxcdat(igrid)%hrivsys
 seepageconc => gwfdxcdat(igrid)%seepageconc

! end of program
 return
end

! ******************************************************************************

!> description
!! deallocate pointers
subroutine sgwf2dxc1da(igrid)

! declaration section
! ------------------------------------------------------------------------------
  use gwfdxcmodule

 implicit none


! arguments
 integer  , intent(in)     :: igrid       !> grid number


! program section
! ------------------------------------------------------------------------------

 call sgwf2dxc1pnt(igrid)
 deallocate(mxdxc)
 deallocate(idxccb)
 deallocate(ndxc)
 deallocate(dxcid)
 deallocate(dxcic)
 deallocate(dxcir)
 deallocate(dxcil)
 deallocate(minid)
 deallocate(maxid)
 deallocate(dxchead)
 deallocate(dxcuzflux)
 deallocate(dxcsf)

 deallocate(maxlsw)
 deallocate(ndxclsw)
 deallocate(ndxcpv)
 deallocate(dxciclsw)
 deallocate(dxcirlsw)
 deallocate(dxcidlsw)
 deallocate(dxcowclsw)
 deallocate(dxcicpv)
 deallocate(dxcirpv)
 deallocate(dxcidpv)
 deallocate(dxclevlsw)
 deallocate(dxclevpv)
 deallocate(nhrivsys)
 deallocate(hrivsys)
 deallocate(seepageconc)

! end of program
 return
end
