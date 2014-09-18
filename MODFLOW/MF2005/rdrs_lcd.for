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

      module lcdmodule

      use global, only: ncol, nrow

      implicit none

      logical, save :: lcdinit = .true.
      logical, save :: lqd
      real, save :: simcsize, xmin, ymin, xmax, ymax

      integer, dimension(:), allocatable, save :: genip
      integer, dimension(:,:), allocatable, save :: genpos

      end module lcdmodule

      module rdlcd_interface

      implicit none

      interface

      subroutine rdlcd(nlist,rlist,rlisttmp,lstbeg,ldim,mxlist,in,
     1                 iout,label,ncol,nrow,nlay)

      implicit none

c arguments
      integer, intent(inout) :: nlist
      integer, intent(in) :: ldim
      integer, intent(inout) :: mxlist
      real, dimension(:,:), pointer :: rlist
      real, dimension(:,:), pointer :: rlisttmp
      integer, intent(in) :: lstbeg
      integer, intent(in) :: in
      integer, intent(in) :: iout
      character(len=*), dimension(*), intent(in) :: label
      integer, intent(in) :: ncol
      integer, intent(in) :: nrow
      integer, intent(in) :: nlay

      end subroutine

      end interface

      end module rdlcd_interface

      subroutine rdlcd(nlist,rlist,rlisttmp,lstbeg,ldim,mxlist,in,
     1                 iout,label,ncol,nrow,nlay)

c description:
c ------------------------------------------------------------------------------
c read data block of Line Column Data type.
c

c declaration section
c ------------------------------------------------------------------------------
      use imod_utl, only: imod_utl_openasc
      use gwfmetmodule, only: cdelr,cdelc
      use lcdmodule, only: genip, genpos

      implicit none

c arguments
      integer, intent(inout) :: nlist
      integer, intent(in) :: ldim
      integer, intent(inout) :: mxlist
      real, dimension(:,:), pointer :: rlist
      real, dimension(:,:), pointer :: rlisttmp
      integer, intent(in) :: lstbeg
      integer, intent(in) :: in
      integer, intent(in) :: iout
      character(len=*), dimension(*), intent(in) :: label
      integer, intent(in) :: ncol
      integer, intent(in) :: nrow
      integer, intent(in) :: nlay

c local variables
      integer :: icol, irow, jcol, jrow, igen, iact, ngen, ii, kk, n
      integer ::  il, is, ie, iline, jline, nline
      integer :: ilay
      real :: factor
      real, dimension(:,:), allocatable :: tmp
      integer(kind=1), dimension(:,:,:), allocatable :: ipc
      logical, dimension(:), allocatable :: writegen
      character(len=1024) :: fname
      integer :: lun
      character(len=1024) :: str

c parameters
      character(len=24) :: aname(1)
      data aname(1) /'                     LCD'/
      integer,parameter :: ineighbours=2
     
c program section
c ------------------------------------------------------------------------------
c init
      call initlcd()

c read number of hfb layers
      read(in,*) ngen

c allocate ipc
      allocate(tmp(ncol,nrow))
      allocate(ipc(0:ncol+2,0:nrow+2,0:2))
      allocate(writegen(max(1,ngen))); writegen = .false.

c count number of hfb and fill
      do iact = 1, 2
         if (iact == 2) then
            do while(.true.)
               backspace(in)
               read(in,'(a)') str
               str = adjustl(str)
               call upcase(str)
               if (str(1:3).eq.'LCD') then
                  exit
               else
                  backspace(in)
               end if
            end do
            read(in,*) ngen
         end if
         ii = lstbeg-1
         do igen = 1, ngen
            read(in,*) ilay, factor
            kk = ilay
            call u2drel(tmp,aname(1),
     1                  nrow,ncol,kk,in,iout) ! fill genpos list

            nline = size(genip)-1

            if (writegen(igen).and.iact.eq.2) then
               write(fname,'(a,i2.2,a,i2.2,a)')
     1            'hfb_',igen,'_l',ilay,'.gen'
               call imod_utl_openasc(lun,fname,'w')
               n = 0
            end if

            do iline = 1, nline ! iline
               
            ipc = int(0,1)
            is = genip(iline-1)+1; ie = genip(iline)
            jcol = genpos(is,1); jrow = genpos(is,2);
            do il = is, ie
               writegen(igen) = .true.
               icol = genpos(il,1)
               irow = genpos(il,2)
               jline = genpos(il,3)
               if (iline.ne.jline) then
                  write(*,*) 'ERROR, something went wrong in rdlcd'
                  call ustop(' ')
               end if          
               ipc(icol,irow,0) = int(1,1)
               
               if(icol.gt.jcol)then
                  ipc(jcol,jrow,2)=int(1,1);     jcol=jcol+1
               elseif(icol.lt.jcol)then
                  ipc(jcol-1,jrow,2)=int(1,1);   jcol=jcol-1   
               elseif(irow.gt.jrow)then
                  ipc(jcol-1,jrow+1,1)=int(1,1); jrow=jrow+1
               elseif(irow.lt.jrow)then
                  ipc(jcol-1,jrow,1)=int(1,1);   jrow=jrow-1
               endif
            end do
            
            do irow = 1, nrow
               do icol = 1, ncol
                  !## place horizontal wall
                  if (irow.lt.nrow) then
                     if(ipc(icol,irow,2).eq.int(1,1)) then
                        ii = ii + 1
                        if (iact.eq.2) then
                           rlisttmp(1,ii) = ilay
                           rlisttmp(2,ii) = irow
                           rlisttmp(3,ii) = icol
                           rlisttmp(4,ii) = irow+1
                           rlisttmp(5,ii) = icol
                           rlisttmp(6,ii) = factor
                           rlisttmp(7,ii) = 0.0
                           if (writegen(igen)) then
                              n=n+1
                              write(lun,'(2i10)') n,igen
                              write(lun,'(2(f10.2,a1))') cdelr(icol-1),
     1                           ',',cdelc(irow)
                              write(lun,'(2(f10.2,a1))') cdelr(icol),
     1                           ',',cdelc(irow)
                              write(lun,'(a)') 'end'
                           end if
                        end if
                     end if
                  end if
                  !## place vertical wall
                  if (icol.lt.ncol) then
                     if(ipc(icol,irow,1).eq.int(1,1)) then
                        ii = ii + 1
                        if (iact.eq.2) then
                            
                           rlisttmp(1,ii) = ilay
                           rlisttmp(2,ii) = irow
                           rlisttmp(3,ii) = icol
                           rlisttmp(4,ii) = irow
                           rlisttmp(5,ii) = icol+1
                           rlisttmp(6,ii) = factor
                           rlisttmp(7,ii) = 0.0
                           if (writegen(igen)) then
                              n=n+1
                              write(lun,'(2i10)') n,igen
                              write(lun,'(2(f10.2,a1))') cdelr(icol),
     1                           ',',cdelc(irow-1)
                              write(lun,'(2(f10.2,a1))') cdelr(icol),
     1                           ',',cdelc(irow)
                              write(lun,'(a)') 'end'
                           end if
                        end if
                     end if
                  end if
               end do ! icol
            end do ! irow

            end do ! iline

            if (writegen(igen).and.iact.eq.2) close(lun)

         end do ! igen

         nlist = ii-lstbeg+1
         if (iact.eq.1) then
            if (.not.associated(rlisttmp)) then
               allocate(rlisttmp(ldim,2*nlist))
            end if
            if (nlist.gt.mxlist) then
               mxlist = 2*nlist
               deallocate(rlist)
               allocate(rlist(ldim,mxlist))
            end if
         end if ! iact = 1
      end do ! iact

c deallocate ipc
      if (allocated(ipc)) deallocate(ipc)
      if (allocated(tmp)) deallocate(tmp)
      if (allocated(writegen)) deallocate(writegen)

c end of program
      return
      end

      subroutine initlcd()
c description:
c ------------------------------------------------------------------------------
c read file of LCD type
c

c declaration section
c ------------------------------------------------------------------------------
      use lcdmodule
      use global, only: iunit, delc, delr
      use gwfmetmodule
      use m_mf2005_iu, only: iumet

c arguments

c local variables
      integer :: icol, irow

c parameters

c program section
c ------------------------------------------------------------------------------
      if (lcdinit) then
         lcdinit = .false.
      else
         return
      end if

c check if metadata package is activated
      if (IUNIT(IUMET).le.0) then
         write(*,*) 'Error: lcd file, please activate met package.'
         call ustop(' ')
      end if
      if (.not.associated(coord_xll) .or.
     &    .not.associated(coord_yll) ) then
         write(*,*) 'Error: lcd file can only be used with xll and yll'
         call ustop(' ')
      end if

c check if grid is uniform
      lqd = .true.
      if ((maxval(delr).ne.minval(delr)).or.
     &    (maxval(delc).ne.minval(delc))) lqd = .false.
      if (lqd) then
         simcsize = delr(1)
      else
         write(*,*) 'Error: non-uniform grids not yet supported.'
         call ustop(' ')
      end if

      if (lqd) then
         xmin=cdelr(0)
         ymax=cdelc(0)
         xmax=xmin+(simcsize*ncol)
         ymin=ymax-(simcsize*nrow)
      endif

c end of program
      return
      end