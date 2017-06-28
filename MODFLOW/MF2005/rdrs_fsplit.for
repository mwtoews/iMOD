c   Copyright (C) Stichting Deltares, 2005-2017.
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

      module fsplitmodule                                                ! DLT

      implicit none

      integer, parameter :: maxfiles = 100
      integer, parameter :: splitsize = 1
      integer, parameter :: splitidf  = 2
      integer, parameter :: splitnc   = 3

      integer, save, pointer                          :: nfsplit
      integer, save, dimension(:), pointer            :: foiu
      character(len=300), save, dimension(:), pointer :: fofname
      character(len=20), save, dimension(:), pointer  :: fofmtarg
      character(len=20), save, dimension(:), pointer  :: foaccarg
      character(len=7), save, dimension(:), pointer   :: fofilstat
      character(len=20), save, dimension(:), pointer  :: fofilact
      real, save, dimension(:), pointer               :: fomaxfs
      integer, save, dimension(:), pointer            :: fosubnr
      integer, save, dimension(:), pointer            :: fotype
      integer, save, dimension(:), pointer            :: foocnlay ! output control
      integer, save, dimension(:,:), pointer          :: fooclay  ! output control

      type fsplittype
         integer, pointer                          :: nfsplit
         integer, dimension(:), pointer            :: foiu
         character(len=300), dimension(:), pointer :: fofname
         character(len=20), dimension(:), pointer  :: fofmtarg
         character(len=20), dimension(:), pointer  :: foaccarg
         character(len=7), dimension(:), pointer   :: fofilstat
         character(len=20), dimension(:), pointer  :: fofilact
         real, dimension(:), pointer               :: fomaxfs
         integer, dimension(:), pointer            :: fosubnr
         integer, dimension(:), pointer            :: fotype
         integer, dimension(:), pointer            :: foocnlay ! output control
         integer, dimension(:,:), pointer          :: fooclay  ! output control
      end type fsplittype

      type(fsplittype), save:: fsplitdat(10)
      end module fsplitmodule

      subroutine fsplitnull(igrid)
c modules
      use fsplitmodule

      implicit none

c arguments
      integer, intent(in) :: igrid

      call fsplitpnt(igrid)
      nfsplit   => null()
      foiu      => null()
      fofname   => null()
      fofmtarg  => null()
      foaccarg  => null()
      fofilstat => null()
      fofilact  => null()
      fomaxfs   => null()
      fosubnr   => null()
      fotype    => null()
      foocnlay  => null()
      fooclay   => null()
      call fsplitpsv(igrid)

      end subroutine

      subroutine fsplitinit(iu,fname,fmtarg,accarg,filstat,filact,
     1                     maxfs,type)                                  ! DLT

c modules
      use fsplitmodule

      implicit none

c arguments
      integer, intent(in)               :: iu
      character(len=300), intent(inout) :: fname
      character(len=20), intent(in)     :: fmtarg
      character(len=20), intent(in)     :: accarg
      character(len=7), intent(in)      :: filstat
      character(len=20), intent(in)     :: filact
      real, intent(in)                  :: maxfs
      character(len=*), intent(in)      :: type

c local variables
      character(len=300) :: ofname

c program section
c ------------------------------------------------------------------------------

      if (.not.associated(nfsplit)) then ! initialize
         allocate(nfsplit)
         nfsplit = 0
         allocate(foiu(maxfiles),
     1            fofname(maxfiles),
     1            fofmtarg(maxfiles),
     1            foaccarg(maxfiles),
     1            fofilstat(maxfiles),
     1            fofilact(maxfiles),
     1            fomaxfs(maxfiles),
     1            fosubnr(maxfiles),
     1            fotype(maxfiles))
         allocate(foocnlay(maxfiles))
         foocnlay(:) = 0
         allocate(fooclay(maxfiles,1))
      end if
      nfsplit = nfsplit + 1

      foiu(nfsplit)      = iu
      fofname(nfsplit)   = fname
      fofmtarg(nfsplit)  = fmtarg
      foaccarg(nfsplit)  = accarg
      fofilstat(nfsplit) = filstat
      fofilact(nfsplit)  = filact
      fomaxfs(nfsplit)   = maxfs
      fosubnr(nfsplit)   = 1
      if (type.eq.'size') then
         fotype(nfsplit) = splitsize
         write(ofname,'(2a,i3.3)') trim(fname),'.',fosubnr(nfsplit)
         fname = ofname
      end if
      if (type.eq.'idf') then
         fotype(nfsplit) = splitidf
      end if
      if (type.eq.'nc') then
         fotype(nfsplit) = splitnc
      end if

c end of program
      return
      end

      subroutine splitfiles(iout,igrid)                                 ! DLT

c modules
      use fsplitmodule

      implicit none

c arguments
      integer, intent(in) :: iout
      integer, intent(in) :: igrid

c local variables
      character(len=300) :: fname
      logical :: opened
      integer :: isub, lun, ifstat(13)

c program section
c ------------------------------------------------------------------------------

      call fsplitpnt(igrid)

      if (.not.associated(nfsplit)) return

      do isub = 1, nfsplit
         ! test or file is opened, if not: no need to re-open it
         if (fotype(isub).eq.splitsize) then
            lun = foiu(isub)
            inquire (unit=lun,opened=opened)
            if (opened) then

               ! check file size
               call osd_fstat(lun,ifstat)

               if (ifstat(8).gt.fomaxfs(isub)) then
                  ! close file and reopen it
                  close(lun)
                  ! create new file name
                  fosubnr(isub) = fosubnr(isub) + 1
                  write(fname,'(2a,i3.3)') trim(fofname(isub)),
     1               '.', fosubnr(isub)
                  ! open new file
                  if (lun.eq.iout) then
                     open(unit=lun,file=fname,
     1                  form  =fofmtarg(isub),
     1                  access=foaccarg(isub),
     1                  status=fofilstat(isub),err=2000)
                  else
                     open(unit=lun,file=fname,
     1                  form  =fofmtarg(isub),
     1                  access=foaccarg(isub),
     1                  status=fofilstat(isub),
     1                  action=fofilact(isub),err=2000)
                  end if
               end if

            end if ! opened
         end if
      end do ! isub

      call fsplitpsv(igrid)

c end of program
      return

 2000 CONTINUE
      WRITE(*,2010)fname,lun,fofilstat(isub),
     1   fofmtarg(isub),foaccarg(isub),
     1   fofilact(isub)
      WRITE(iout,2010)fname,lun,fofilstat(isub),
     1   fofmtarg(isub),foaccarg(isub),
     1   fofilact(isub)
 2010 FORMAT(/,1X,'*** ERROR OPENING FILE "',A,'" ON UNIT ',I5,/,
     &7X,'SPECIFIED FILE STATUS: ',A,/
     &7X,'SPECIFIED FILE FORMAT: ',A,/
     &7X,'SPECIFIED FILE ACCESS: ',A,/
     &7X,'SPECIFIED FILE ACTION: ',A,/
     &2X,'-- STOP EXECUTION (FSPLITINIT)')
      CALL USTOP(' ')

      end

      subroutine fsplitpnt(igrid)
      use fsplitmodule
      implicit none
      integer igrid

      if (.not.associated(fsplitdat(igrid)%nfsplit)) return

      nfsplit   => fsplitdat(igrid)%nfsplit
      foiu      => fsplitdat(igrid)%foiu
      fofname   => fsplitdat(igrid)%fofname
      fofmtarg  => fsplitdat(igrid)%fofmtarg
      foaccarg  => fsplitdat(igrid)%foaccarg
      fofilstat => fsplitdat(igrid)%fofilstat
      fofilact  => fsplitdat(igrid)%fofilact
      fomaxfs   => fsplitdat(igrid)%fomaxfs
      fosubnr   => fsplitdat(igrid)%fosubnr
      fotype    => fsplitdat(igrid)%fotype
      foocnlay  => fsplitdat(igrid)%foocnlay
      fooclay   => fsplitdat(igrid)%fooclay
C
      return
      end
      subroutine fsplitpsv(igrid)
      use fsplitmodule
      implicit none
      integer igrid
C
      if (.not.associated(nfsplit)) return

      fsplitdat(igrid)%nfsplit   => nfsplit
      fsplitdat(igrid)%foiu      => foiu
      fsplitdat(igrid)%fofname   => fofname
      fsplitdat(igrid)%fofmtarg  => fofmtarg
      fsplitdat(igrid)%foaccarg  => foaccarg
      fsplitdat(igrid)%fofilstat => fofilstat
      fsplitdat(igrid)%fofilact  => fofilact
      fsplitdat(igrid)%fomaxfs   => fomaxfs
      fsplitdat(igrid)%fosubnr   => fosubnr
      fsplitdat(igrid)%fotype    => fotype
      fsplitdat(igrid)%foocnlay  => foocnlay
      fsplitdat(igrid)%fooclay   => fooclay

      return
      end


      subroutine splitbudget(ipos,line,lloc,nlay,iout,label,inoc,iact)  ! DLT

c modules
      use gwfbasmodule, only: ihedun
      use fsplitmodule

      implicit none

c arguments
      integer, intent(in)          :: ipos
      character(len=*),intent(in)  :: line
      integer, intent(in)          :: lloc
      integer, intent(in)          :: nlay
      integer, intent(in)          :: iout
      character(len=*), intent(in) :: label
      integer, intent(in)          :: inoc
      integer, intent(in)          :: iact

c local variables
      integer :: istart, istop, l, nset, i, iiu
      real :: r

      integer, parameter :: igrid = 1 ! temporary

c program section
c ------------------------------------------------------------------------------
c read unit number
      if (iact.eq.1) then ! save budget
         call urword(line,lloc,istart,istop,2,l,r,-1,inoc)
      else
         call sgwf2bas7pnt(igrid)
         if (.not.associated(nfsplit)) return
         l = ihedun
      end if

c return in case no layers are specified
      if (l.le.0) return

c find index for unit number
      call splitgetiuidx(l,iiu)
      if (iiu.le.0) return

c set pointers
      call fsplitpnt(igrid)

c set number of layers
      if (sum(foocnlay).eq.0) then
         if (associated(fooclay)) then
            deallocate(fooclay)
            allocate(fooclay(nfsplit,nlay))
            fooclay = 0
         else
            write(iout,*) 'ERROR. Allocating fooclay'
            call ustop(' ')
         end if
      end if
      foocnlay(iiu) = nlay

c read layers
10    call urword(line,lloc,istart,istop,2,l,r,-1,inoc)
      nset = 0
      if(l.gt.0 .and. l.le.nlay) then
         nset=nset+1
         fooclay(iiu,l) = 1
         go to 10
      end if

c store pointers
      call fsplitpsv(igrid)

c end of program
      return
      end

      subroutine splitgetiuidx(iu,iuidx)                                ! DLT

c modules
      use fsplitmodule, only: nfsplit, foiu

      implicit none

c arguments
      integer, intent(in) :: iu
      integer, intent(out) :: iuidx

c local variables
      integer :: i

c program section
c ------------------------------------------------------------------------------
      iuidx = 0
      do i = 1, nfsplit
         if (foiu(i).eq.iu) then
            iuidx = i
            exit
         end if
      end do
      if (iuidx.eq.0) then
         write(*,*) 'WARNING. SAVE BUDGET unit number not found.'
      end if

c end of program
      return
      end



