c   Copyright (C) Stichting Deltares, 2005-2014.
c
c   This file is part of iMOD.
c
c   This program is free software: you can redistribute it and/or modify
c   it under the terms of the GNU General Public License as published by
c   the Free Software Foundation, either version 3 of the License, or
c   (at your option) any later version.
c
c   This program is distributed in the hope that it will be useful,
c   but WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU General Public License for more details.
c
c   You should have received a copy of the GNU General Public License
c   along with this program.  If not, see <http://www.gnu.org/licenses/>.
c
c   Contact: imod.support@deltares.nl
c   Stichting Deltares
c   P.O. Box 177
c   2600 MH Delft, The Netherlands.

      module rdgcd_interface

      interface

      subroutine rdgcd(nlist,rlist,rlisttmp,lstbeg,ldim,mxlist,ial,in,
     1                 iout,label,caux,ncaux,naux,ncol,nrow,nlay)
c description:
c ------------------------------------------------------------------------------
c read data block of Grid Column Data type
c

c declaration section
c ------------------------------------------------------------------------------
      implicit none

c arguments
      integer, intent(inout) :: nlist
      integer, intent(in) :: ldim
      integer, intent(inout) :: mxlist
      real, dimension(:,:), pointer :: rlist
      real, dimension(:,:), pointer :: rlisttmp
      integer, intent(in) :: lstbeg
      integer, intent(in) :: ial
      integer, intent(in) :: in
      integer, intent(in) :: iout
      character(len=*), dimension(*), intent(in) :: label
      integer, intent(in) :: ncaux
      character(len=16), dimension(ncaux), intent(in) :: caux
      integer, intent(in) :: naux
      integer, intent(in) :: ncol
      integer, intent(in) :: nrow
      integer, intent(in) :: nlay

      end subroutine

      end interface

      end module

      module gcdmodule
      implicit none

      integer, save :: isub, icolumn, nread2ipf


      type listType
         integer :: nlist = 0
         real, dimension(:,:), allocatable :: list
      end type listType
      type(listType), dimension(:,:), allocatable, save :: ipflist
      type(listType), dimension(:), allocatable, save   :: isglist
      real, dimension(:,:), allocatable, save :: isglist2

      end module

      subroutine rdgcd(nlist,rlist,rlisttmp,lstbeg,ldim,mxlist,ial,in,
     1                 iout,label,caux,ncaux,naux,ncol,nrow,nlay)
c description:
c ------------------------------------------------------------------------------
c read file of GCD type
c

c declaration section
c ------------------------------------------------------------------------------
      use gcdmodule
      use idfmodule
      use rdrsmodule, only: nodata

      implicit none

c arguments
      integer, intent(inout) :: nlist
      integer, intent(in) :: ldim
      integer, intent(inout) :: mxlist
      real, dimension(:,:), pointer :: rlist
      real, dimension(:,:), pointer :: rlisttmp
      integer, intent(in) :: lstbeg
      integer, intent(in) :: ial
      integer, intent(in) :: in
      integer, intent(in) :: iout
!      character(len=*), dimension(*), intent(in) :: label
      character(len=*), intent(in) :: label
      integer, intent(in) :: ncaux
      character(len=16), dimension(ncaux), intent(in) :: caux
      integer, intent(in) :: naux
      integer, intent(in) :: ncol
      integer, intent(in) :: nrow
      integer, intent(in) :: nlay

c functions
      integer :: cfn_length

c local variables
      logical :: storeisub, valid, found, ipfonly
      integer :: ii, jj, kk, nread1, nread2, nisub,
     1           icol, irow, ic, ir, jsub, iact, jact, nalloc
      integer, dimension(:), allocatable :: ilay
      real, dimension(:,:,:,:), allocatable :: data
      real :: rval
      real :: tlp(nlay)
      logical :: isgfile
      integer :: lloc, istart, istop, i, n, nisg, il
      real :: r, q, z1, z2, c
      character(len=200) :: line
      character(len=256) :: str, fname, tmplabel
      integer, dimension(10) :: jjj
      data jjj/2,1,3,4,5,6,7,8,9,10/

c program section
c ------------------------------------------------------------------------------
c
c set pointers
      call sgwf2bas7pnt(1)

      nread2=ldim-ial
      nread1=nread2-naux
      nread1 = nread1 - 3
      nread2 = nread2 - 3

c read number of subsystems
      read(in,*) nisub

      storeisub = .false.
      if (nisub.gt.0) then
         do ii = 1, naux
            if (caux(ii).eq.'ISUB            ') then
               storeisub = .true.
            end if
         end do
      else
         nisub = 1
      end if

c allocate
      allocate(data(ncol,nrow,nisub,nread2))
      allocate(ipflist(nisub,nread2))
      allocate(isglist(nisub))
      allocate(ilay(nisub))

c read data and count nlist

      nlist = 0
      data = nodata

c        iact = 1: read and count
c        iact = 2: store in rlist
      do iact = 1, 2

         ii = lstbeg-1

c           loop over the number of subsystems
         do isub = 1, nisub

            if (iact.eq.1) then
c
c                 layer applied for this subsystem; subsystem index
               read(in,'(a)') str
               read(str,*) ilay(isub), jsub
               call cfn_s_lowcase(str)
               read(in,'(a)') line
               backspace(in)

c                  check if file is of type isg
               isgfile = .false.
               lloc=1
               call urword(line,lloc,istart,istop,1,i,r,iout,in)
               if(line(istart:istop).eq.'OPEN/CLOSE') then
                  call urword(line,lloc,istart,istop,0,n,r,iout,in)
                  fname = line(istart:istop)
                  call cfn_s_lowcase(fname)
                  n = cfn_length(fname)
                  i = index(fname,'.isg',back=.true.)
                  if (i.gt.0) then
                     i = n-i
                     if (i.eq.3) isgfile = .true.
                  end if
                  fname = line(istart:istop)
               end if
            end if
            if (isgfile) call isginit()
            if (iact.eq.1) then
               isglist(isub)%nlist = -1
               if (isgfile) then
                  if (nread2.ne.5) then
                     write(*,*) 'ERROR. ISG only to be used for river.'
                     call ustop(' ')
                  end if
                  write(*,*) 'reading ',trim(fname)
                  nalloc = 1
                  allocate(isglist(isub)%list(nalloc,10))
                  call pck1rpisg(isglist(isub)%list,nalloc,
     1               isglist(isub)%nlist,fname,ilay(isub),0)
                  deallocate(isglist(isub)%list)
                  nalloc = 2*isglist(isub)%nlist+nrow*ncol
                  allocate(isglist(isub)%list(nalloc,10))
                  call pck1rpisg(isglist(isub)%list,nalloc,
     1               isglist(isub)%nlist,fname,ilay(isub),1)
                  read(in,'(a)') line
               else
c                    read basic fields
                  kk = ilay(isub)
                  do jj = 1, nread1
                     icolumn = jj
                     write(tmplabel,'(a,1x,i1)') trim(label), jj
                     call u2drel(data(:,:,isub,jj),tmplabel,
     1                        nrow,ncol,kk,in,iout)
                     ! jj = 1 is always the conductance!
                     if (jj.eq.1) then
                        usexmask = .true.
                        if (.not.allocated(xmask))
     1                      allocate(xmask(ncol,nrow))
                        xmask = nodata
                        do irow = 1, nrow
                           do icol = 1, ncol
                             if (data(icol,irow,isub,jj).ne.nodata) then
                               xmask(icol,irow) = -12345.
                             end if
                           end do
                        end do
                     end if
                  end do

c                    read auxiliary fields
                  do jj = 1, naux
                     if (caux(jj).ne.'ISUB            ') then
                        icolumn = nread1+jj
                        write(tmplabel,'(a,1x,i1)') trim(label),
     1                     jj+nread1
                        call u2drel(data(:,:,isub,nread1+jj),tmplabel,
     1                              nrow,ncol,kk,in,iout)
                     else
                        if (.not.storeisub) then
                           write(iout,*) 'ERROR. AUX ISUB'
                           call ustop(' ')
                        else
                           data(:,:,isub,nread1+jj) = jsub
                        end if
                     end if
                  end do ! naux
                  ! do not use mask anymore
                  usexmask = .false.
               end if
            end if ! iact

c              check if subsystem is defined by ipf's only
            ipfonly = .true.
            do jj = 1, nread2
               if (ipflist(isub,jj)%nlist.eq.0) ipfonly = .false.
            end do
            if (ipfonly) then
               if (ilay(isub).le.0) then ! automatically assign
                  do kk = 1, ipflist(isub,1)%nlist
                     irow = int(ipflist(isub,1)%list(1,kk))
                     icol = int(ipflist(isub,1)%list(2,kk))
                     q  = ipflist(isub,1)%list(3,kk)
                     z1 = ipflist(isub,1)%list(4,kk)
                     z2 = ipflist(isub,1)%list(5,kk)
                     call assign_layer(tlp,irow,icol,z1,z2)
                     do il = 1, nlay
                        if (tlp(il).gt.0.) then
                           ii = ii + 1
                           if (iact.eq.2) then
                              rlisttmp(1,ii) = il
                              rlisttmp(2,ii) = irow
                              rlisttmp(3,ii) = icol
                              rlisttmp(4,ii) = q*tlp(il)
                              do jj = 2, nread2
                                 rlisttmp(3+jj,ii) =
     1                              ipflist(isub,jj)%list(3,kk)
                              end do
                           end if
                        end if
                     end do
                  end do
               else
c                    assume all ipfs are consistent
                  do kk = 1, ipflist(isub,1)%nlist
                     ii = ii + 1
                     if (iact.eq.2) then
                        rlisttmp(1,ii) = ilay(isub)
                        rlisttmp(2,ii) = int(ipflist(isub,1)%list(1,kk))
                        rlisttmp(3,ii) = int(ipflist(isub,1)%list(2,kk))
                        do jj = 1, nread2
                           rlisttmp(3+jj,ii) =
     1                        ipflist(isub,jj)%list(3,kk)
                        end do
                     end if
                  end do
               end if

c              check if subsystem is of type ISG
            else if (isglist(isub)%nlist.ge.0) then
               if (allocated(isglist2)) deallocate(isglist2)
               do jact = 1, 2
                  nisg = 0
                  if (ilay(isub).le.0) then ! automatically assign
                     do kk = 1, isglist(isub)%nlist
                        irow = int(isglist(isub)%list(kk,2))
                        icol = int(isglist(isub)%list(kk,3))
                        z1   = isglist(isub)%list(kk,4)
                        q    = isglist(isub)%list(kk,5)
                        z2   = isglist(isub)%list(kk,6)
                        if (z1.eq.z2) then
                           z1 = z1 + 0.05
                           z2 = z2 - 0.05
                        end if
                        call assign_layer(tlp,irow,icol,z1,z2)
                        do il = 1, nlay
                           if (tlp(il).gt.0.) then
                              nisg = nisg + 1
                              if (jact.eq.2) then
                                 do jj = 1, 10
                                    isglist2(nisg,jj) =
     1                                 isglist(isub)%list(kk,jj)
                                 end do
                                 isglist2(nisg,1) = il
                                 isglist2(nisg,5) = q*tlp(il)
                              end if
                           end if
                        end do
                      end do
                   else
                      nisg = isglist(isub)%nlist
                      if (jact.eq.2) isglist2 = isglist(isub)%list
                   end if
                   if (jact.eq.1) then
                      if (allocated(isglist2)) deallocate(isglist2)
                      allocate(isglist2(max(nisg,1),10))
                   end if
               end do

               call pck3rpisg(isglist2,nisg)

               if (iact.eq.1) then
                  ii = ii + nisg
               else
                  do jj = 1, nisg
                     ii = ii + 1
                     do kk = 1, 7
                        rlisttmp(kk,ii) = isglist2(jj,kk)
                     end do
                     if (storeisub) then
                        rlisttmp(8,ii) = -jsub ! for ISG label subnumber negative!
                     end if
                  end do
               end if

            else ! IDF only

               do irow = 1, nrow
                  do icol = 1, ncol
                     valid = .true.
                     do jj = 1, nread2
                        if (data(icol,irow,isub,jj).eq.nodata)
     1                     valid = .false.
                     end do
                     if (valid) then
                        if (ilay(isub).le.0) then ! automatically assign
                           c = data(icol,irow,isub,1)
                           z1 = data(icol,irow,isub,2)
                           if (nread1.gt.2) then
                              z2 = data(icol,irow,isub,3)
                           else
                              z2 = z1
                           end if
                           if (z1.eq.z2) then
                              z1 = z1 + 0.05
                              z2 = z2 - 0.05
                           end if
                           call assign_layer(tlp,irow,icol,z1,z2)
                           do il = 1, nlay
                              if (tlp(il).gt.0.) then
                                 ii = ii + 1
                                 if (iact.eq.2) then
                                    rlisttmp(1,ii) = il
                                    rlisttmp(2,ii) = irow
                                    rlisttmp(3,ii) = icol
                                    do jj = 1, nread2
                                        rlisttmp(3+jj,ii) =
     1                                    data(icol,irow,isub,jjj(jj))
                                    end do
                                    rlisttmp(3+jjj(1),ii) = c*tlp(il)
                                 end if
                              end if
                           end do
                        else
                           ii = ii + 1
                           if (iact.eq.2) then
                              rlisttmp(1,ii) = ilay(isub)
                              rlisttmp(2,ii) = irow
                              rlisttmp(3,ii) = icol
                              do jj = 1, nread2
                                 rlisttmp(3+jj,ii) =
     1                             data(icol,irow,isub,jjj(jj))
                              end do
                           end if
                        end if
                     end if
                  end do
               end do
            end if

            if (isgfile) call isgfinalize()

         end do ! isub

         if (iact.eq.1) then
            nlist = ii-lstbeg+1
            allocate(rlisttmp(ldim,max(nlist,1)))
            if (nlist.gt.mxlist) then
               mxlist = nlist
               deallocate(rlist)
               allocate(rlist(ldim,mxlist))
            end if
         end if ! iact = 1
      end do ! iact

c deallocate data
      deallocate(data)
      do isub = 1, nisub
         do jj = 1, nread2
            if (ipflist(isub,jj)%nlist.gt.0)
     1         deallocate(ipflist(isub,jj)%list)
         end do
      end do
      deallocate(ipflist)
      deallocate(isglist)
      if (allocated(isglist2)) deallocate(isglist2)
      deallocate(ilay)

c end of program
      return
      end


