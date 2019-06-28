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

module pks_iarmwp
implicit none

logical :: liarmwp = .false. 

logical :: lfirst = .true.

type xpType
   integer :: xprnk = -1
   integer :: xpbuf = 0
   integer :: nid   = 0
   integer, pointer, dimension(:)   :: idx => null()
   integer, pointer, dimension(:)   :: id  => null()
   integer, pointer, dimension(:,:) :: lrc => null()    
end type xpType

! serial
type xpaType
   integer                             :: nrxp = 0 
   integer, pointer, dimension(:)      :: xpflg => null() 
   type(xpType), pointer, dimension(:) :: xp => null()
end type xpaType
type(xpaType), pointer, dimension(:) :: xpa => null()

! parallel
integer :: nrxp = 0
type(xpType), pointer, dimension(:) :: xp => null()

! buffer
integer :: lenb
real, dimension(:), allocatable :: rb, sb

save

end module pks_iarmwp
    
module pks_imod_utl
use imod_idf
implicit none

contains
 
!###====================================================================
 subroutine pks_imod_utl_idfmerge_init()
!###====================================================================
 ! locals
 logical :: pks7mpimasterwrite, lpks
 !......................................................................
 call pks7mpiactive(lpks)
 if (lpks) then
    if (pks7mpimasterwrite()) lidfout = .true.
 end if   

 end subroutine pks_imod_utl_idfmerge_init
 
!###====================================================================
 subroutine pks_imod_utl_write_idfmergefile(wd,fname)
!###====================================================================
 ! modules
 use pksmpi_mod
 ! arguments
 character(len=*), intent(in) :: wd
 character(len=*), intent(out) :: fname
 ! locals
 logical :: lpks
 integer :: i, j, iu
 character(len=1024) :: s
 character(len=1024), dimension(10) :: sa
 !......................................................................
 
 call pks7mpiactive(lpks)
 if (.not.lpks) return
 if (.not.lidfout) return 
 
 write(fname,'(2a)') trim(wd), '\pksidfout.txt' 
 call osd_s_filename(fname)
 
 iu = 0
 call imod_utl_openasc(iu,fname,'w')

 write(s,*) nrproc
 write(iu,'(a)') trim(adjustl(s))
 
 do i = 1, nidfout
    s = idfout(i) 
    call imod_utl_swapslash(s)
    write(iu,'(a)') trim(s)
 end do
 close(iu)
 
 lidfout = .false.
 nidfout = 0  
 if(allocated(idfout)) deallocate(idfout)
 
 return
 end subroutine pks_imod_utl_write_idfmergefile 
  
!###====================================================================
 subroutine pks_imod_utl_idfmerge(fname)
!###====================================================================
 ! arguments
 character(len=*), intent(in) :: fname
 ! functions
 logical :: pks7mpimasterwrite
 ! locals
 integer :: iu
 logical :: lpks, lex
 type(idfobj) :: gidf, lidf
 integer :: ios, gncol, gnrow, nrproc, iproc, ic1, ic2, ir1, ir2, ip0, i, j
 real :: xmin, xmax, ymin, ymax, dx, dy
 character(len=1024) :: s, fp0, f
 real :: nodata
 character(len=1) :: slash 
 !......................................................................

 call pks7mpibarrier()
 if (pks7mpimasterwrite()) then
    call imod_utl_printtext('Start merging PKS output IDF files',0)
    
    inquire(file=fname,exist=lex)
    if(.not.lex) call imod_utl_printtext('File '//trim(fname)//' does not exist',2)

    
    iu = getunit()
    open(unit=iu,file=fname,status='old')
    read(iu,*,iostat=ios) nrproc
    if(ios.ne.0) call imod_utl_printtext('Error reading '//trim(fname),2)
    
    call idfdeallocatex(gidf)
    call idfdeallocatex(lidf)
    
    ! determine xmin, ymin, xmax, ymax
    xmin = huge(xmin); ymin = huge(ymin); xmax = -huge(xmax); ymax = -huge(ymax)  
    do while(.true.) 
       read(iu,'(a)',iostat=ios) s  
       if (ios.ne.0) exit
       if(len_trim(s).eq.0) cycle
       if(s(1:1).eq.'#') cycle
       fp0 = s
       ip0 = index(fp0,'_p000',back=.true.)
       if(ip0.le.0) call imod_utl_printtext('Error reading '//trim(fp0),2)
       
       do iproc = 1, nrproc
          f = fp0
          write(s,'(a,i3.3)') '_p',iproc-1
          f(ip0:ip0+len_trim(s)-1) = trim(s)
          ! check is file exist
          inquire(file=f,exist=lex)
          if (.not.lex) then
             call imod_utl_printtext(' Warning, could not find '//trim(f),0)
             cycle
          end if     
          ! read the IDF and delete when done
          close(lidf%iu); call idfdeallocatex(lidf)    
          if (.not.idfread(lidf,f,0)) call imod_utl_printtext('Error reading '//trim(f),2) 
          xmin = min(xmin,lidf%xmin); ymin = min(ymin,lidf%ymin); xmax = max(xmax,lidf%xmax); ymax = max(ymax,lidf%ymax)
       end do   
    end do   
    rewind(iu); read(iu,*) nrproc 
    
    do while(.true.) 
       read(iu,'(a)',iostat=ios) s  
       if (ios.ne.0) exit
       if(len_trim(s).eq.0) cycle
       if(s(1:1).eq.'#') cycle
       fp0 = s
       inquire(file=fp0,exist=lex)
       if (.not.lex) then
          call imod_utl_printtext(' Warning, could not find '//trim(fp0),0)
          cycle
       end if
       ip0 = index(fp0,'_p000',back=.true.)
       if(ip0.le.0) call imod_utl_printtext('Error reading '//trim(fp0),2)
       
       ! read the idf for p000 and delete when done
       close(lidf%iu); call idfdeallocatex(lidf)    
       if (.not.idfread(lidf,fp0,0)) call imod_utl_printtext('Error reading '//trim(fp0),2)
       ! checks
       if (lidf%ieq.ne.0) call imod_utl_printtext('Merging non-equidistant IDFs is not supported '//trim(fp0),2)
       dx = lidf%dx; dy = lidf%dy
       ! initialize global idf
       gidf%dx      = dx
       gidf%dy      = dy
       gidf%nodata  = lidf%nodata    
       gidf%comment = lidf%comment 
       gncol = (xmax-xmin)/dx; gnrow = (ymax-ymin)/dy
       gidf%ncol    = gncol
       gidf%nrow    = gnrow
       gidf%xmin    = xmin
       gidf%xmax    = xmax 
       gidf%ymin    = ymin
       gidf%ymax    = ymax
       
       !## set precision definition
       gidf%itype   = lidf%itype
       
       if(.not.associated(gidf%x)) allocate(gidf%x(gncol,gnrow))
       ! assign nodata
       nodata = gidf%nodata
       j = index(fp0,'bdg',back=.true.)
       if (j.le.0) j = index(fp0,'BDG',back=.true.)
       if (j.gt.0) then ! found the key bdg
          call imod_utl_getslash(slash)
          i = index(fp0,slash,back=.true.)
          if (i.gt.0) then ! slash in name
             if (i.le.j) nodata = 0.    
          else ! no slash in name
             nodata = 0.
          end if
       end if   
       gidf%x = nodata
       
       ! fill data
       do iproc = 1, nrproc
          f = fp0
          write(s,'(a,i3.3)') '_p',iproc-1
          f(ip0:ip0+len_trim(s)-1) = trim(s)
          ! check is file exist
          inquire(file=f,exist=lex)
          if (.not.lex) then
             call imod_utl_printtext(' Warning, could not find '//trim(f),0)
             cycle
          end if     
          ! read the IDF and delete when done
          close(lidf%iu); call idfdeallocatex(lidf)    
          if (.not.idfread(lidf,f,2)) call imod_utl_printtext('Error reading '//trim(f),2)   
          ! checks
          if (lidf%ieq.ne.0) call imod_utl_printtext('Merging non-equidistant IDFs is not supported '//trim(f),2)
          if (lidf%dx.ne.dx.or.lidf%dy.ne.dy) call imod_utl_printtext('Merging IDFs with different cell size is not supported '//trim(f),2)
          ! fill
          ic1 = (lidf%xmin-xmin)/dx+1; ir1 = (ymax-lidf%ymax)/dy+1
          ic2 = ic1+lidf%ncol-1; ir2 = ir1+lidf%nrow-1 
          gidf%x(ic1:ic2,ir1:ir2) = lidf%x
       end do    
       close(lidf%iu); call idfdeallocatex(lidf)
       
       ! write the global IDF
       f = fp0(1:ip0-1)//'.idf'
       call imod_utl_printtext('Writing '//trim(f)//'...',0)
       if (.not.idfwrite(gidf,f,0)) call imod_utl_printtext('Could not write '//trim(f),2)
    end do
    
    ! close file
    close(gidf%iu); call idfdeallocatex(gidf)
    close(iu,status='delete')
    
    ! cleanup
    call idfdeallocatex(gidf)
    
    call imod_utl_printtext('Done merging PKS output IDF files',0)
 end if
 call pks7mpibarrier() 

 return
 end subroutine pks_imod_utl_idfmerge 
 
!###====================================================================
 subroutine pks_imod_utl_iarmwp_xch_init(iarmwp)
!###====================================================================
! modules
 use pks_iarmwp
! arguments
 integer, intent(in) :: iarmwp
! locals
 logical :: lpks
!......................................................................
 call pks7mpiactive(lpks)
 if (.not.lpks) return
 
 if (iarmwp.eq.1) then
    liarmwp = .true.
 end if
 
 return   
 end subroutine pks_imod_utl_iarmwp_xch_init
 
 !###====================================================================
 subroutine pks_imod_utl_iarmwp_xch_disable()
!###====================================================================
! modules
 use pks_iarmwp, only: liarmwp
!......................................................................
 
 liarmwp = .false.
 
 return   
 end subroutine pks_imod_utl_iarmwp_xch_disable
 
!###====================================================================
 subroutine pks_imod_utl_iarmwp_xch_store(msir,msic,mfil,mfir,mfic,dxcid,ncol,nrow,nlay,iact)
!###====================================================================
 ! modules
 use rf2mf_module, only: pks
 use pks_iarmwp
 use pksmpi_mod, only: nrproc  
 ! arguments
 integer, intent(in) :: msir, msic, mfil, mfir, mfic, ncol, nrow, nlay, iact
 integer, dimension(ncol,nrow,nlay), intent(inout) :: dxcid 
 ! locals
 logical :: lpks, pmsovl(nrproc), pmfovl(nrproc)
 integer :: iproc, jproc, pmsnovl, pmfnovl, i, n, mfid
 integer :: ic1, ic2, ir1, ir2 ! non-overlapping
 integer :: jc1, jc2, jr1, jr2 ! overlapping
 !......................................................................
 call pks7mpiactive(lpks)
 if (.not.lpks) return
 
 if (.not.associated(xpa)) then
     allocate(xpa(nrproc)) !A1
     do iproc = 1, nrproc
        allocate(xpa(iproc)%xpflg(nrproc)) !A2
        xpa(iproc)%xpflg = 0
        allocate(xpa(iproc)%xp(nrproc)) ! A3
     end do
 end if
 
 if (iact.eq.2.and.lfirst) then
    lfirst = .false.
     do iproc = 1, nrproc
        do jproc = 1, nrproc 
           n = xpa(iproc)%xp(jproc)%nid
           if(n.gt.0) then
              allocate(xpa(iproc)%xp(jproc)%id(n))
           end if
           xpa(iproc)%xp(jproc)%nid = 0
        end do
        xpa(iproc)%xpflg = 0
     end do
 end if    
 
 pmsovl = .false.; pmfovl = .false.
 pmsnovl = -1; pmfnovl = -1
 do iproc = 1, nrproc
    ic1 = pks%partminmax(iproc,1); ic2 = pks%partminmax(iproc,2) ! non overlapping 
    ir1 = pks%partminmax(iproc,3); ir2 = pks%partminmax(iproc,4) ! non overlapping 
    jc1 = max(1,ic1-1); jc2 = min(ncol,ic2+1)                    ! overlapping
    jr1 = max(1,ir1-1); jr2 = min(nrow,ir2+1)                    ! overlapping  
     
    if (msic.ge.jc1 .and. msic.le.jc2 .and. &                    ! overlapping
        msir.ge.jr1 .and. msir.le.jr2) pmsovl(iproc) = .true.

    if (mfic.ge.jc1 .and. mfic.le.jc2 .and. &                    ! overlapping
        mfir.ge.jr1 .and. mfir.le.jr2) pmfovl(iproc) = .true.
    
    if (msic.ge.ic1 .and. msic.le.ic2 .and. &                    ! non overlapping
        msir.ge.ir1 .and. msir.le.ir2) pmsnovl = iproc

    if (mfic.ge.ic1 .and. mfic.le.ic2 .and. &                    ! non overlapping
        mfir.ge.ir1 .and. mfir.le.ir2) pmfnovl = iproc
 end do
 if(pmfnovl.le.0.and.pmsnovl.gt.0) then
    write(*,*) 'Warning IARMWP=1, MODFLOW cell does not belong to any partition!',mfic,mfir
    return
 end if
 if(pmfnovl.gt.0.and.pmsnovl.le.0.) then
    write(*,*) 'Warning IARMWP=1, MetaSWAP svat does not belong to any partition!',msic,msir
 end if
 if(pmfnovl.le.0.or.pmsnovl.le.0.) then
    return
 end if

 mfid = abs(dxcid(mfic,mfir,mfil))
 do iproc = 1, nrproc
    jproc = -1 
    if (     pmsovl(iproc).and.     pmfovl(iproc)) cycle
    if (.not.pmsovl(iproc).and..not.pmfovl(iproc)) cycle
    ! svat not in overlapping parition & cell     in overlapping parition
    if (.not.pmsovl(iproc).and.     pmfovl(iproc)) then
       jproc = pmsnovl         
    end if    
    ! svat     in overlapping parition & cell not in overlapping parition
    if (     pmsovl(iproc).and..not.pmfovl(iproc)) then
       jproc = pmfnovl
    end if
    if (jproc.le.0) then
       write(*,*) 'Program error, pks_imod_utl_iarmwp_xch_store',pmsnovl,pmfnovl,msic,msir,mfic,mfir    
       stop 
    end if
    if(iproc.eq.jproc) cycle
    xpa(iproc)%xpflg(jproc) = 1
    xpa(iproc)%xp(jproc)%nid = xpa(iproc)%xp(jproc)%nid + 1 
    if (iact.eq.2) then
       i = xpa(iproc)%xp(jproc)%nid  
       xpa(iproc)%xp(jproc)%id(i) = mfid    
    end if
       
    xpa(jproc)%xpflg(iproc) = 1
    xpa(jproc)%xp(iproc)%nid = xpa(jproc)%xp(iproc)%nid + 1 
    if (iact.eq.2) then
       i = xpa(jproc)%xp(iproc)%nid  
       xpa(jproc)%xp(iproc)%id(i) = mfid    
    end if
    
!    if(iact.eq.2) then
!       dxcid(mfic,mfir,mfil) = -abs(dxcid(mfic,mfir,mfil))    
!    end if
    
 end do
 
 return   
 end subroutine pks_imod_utl_iarmwp_xch_store

!###====================================================================
 subroutine pks_imod_utl_iarmwp_xch_write(dxcid,ncol,nrow,nlay,ndxc,modwd)
!###====================================================================
 ! modules
 use pks_iarmwp
 use pksmpi_mod, only: nrproc
 use imod_utl, only: imod_utl_openasc
 ! arguments
 integer, intent(in) :: ncol, nrow, nlay, ndxc
 integer, dimension(ncol,nrow,nlay), intent(in) :: dxcid
 character(len=*), intent(in) :: modwd
 ! locals
 logical :: lpks
 integer :: iproc, jproc, id, n, i, j, lun, ilay, irow, icol
 character(len=1000) :: fname, s
 character(len=100), dimension(4) :: sa
 integer, dimension(:,:), allocatable :: iwrk
 ! functions
 integer :: cfn_unique_i
 !......................................................................
 call pks7mpiactive(lpks)
 if (.not.lpks) return

 allocate(iwrk(3,ndxc))
 do ilay = 1, nlay
    do irow = 1, nrow
       do icol = 1, ncol
          id = abs(dxcid(icol,irow,ilay))
          if (id.ne.0) then
             if (id.gt.ndxc) then
                write(*,*) 'Program error, stopping'
                stop
             end if 
             iwrk(1,id) = ilay 
             iwrk(2,id) = irow 
             iwrk(3,id) = icol 
          end if
       end do
    end do
 end do
 
 do iproc = 1, nrproc
    !write(*,'(a,i3.3,a)') '========= iproc = ',iproc,':' 
    do jproc = 1, nrproc
       if (xpa(iproc)%xpflg(jproc).eq.1) then
          xpa(iproc)%nrxp = xpa(iproc)%nrxp + 1    
       end if 
    end do
    !write(*,*) 'nxp = ',xp(iproc)%nrxp 
    do jproc = 1, nrproc
       if (xpa(iproc)%xpflg(jproc).eq.1) then
          n = cfn_unique_i(xpa(iproc)%xp(jproc)%id,xpa(iproc)%xp(jproc)%nid,0) 
          xpa(iproc)%xp(jproc)%nid = n 
          !write(*,*) jproc,':', xp(iproc)%xp(jproc)%nid
          !do i = 1, xp(iproc)%xp(jproc)%nid
          !   write(*,*) '-->',xp(iproc)%xp(jproc)%id(i)
          !end do
       end if 
    end do
    
    lun = 0
    write(fname,'(2a,i3.3)') trim(modwd),'pks_xch_iarmwp.p', iproc-1 
    call osd_s_filename(fname)
    write(*,'(a,1x,2a)') 'Writing',trim(fname),'...'
    call imod_utl_openasc(lun,fname,'w')
    write(s,*) xpa(iproc)%nrxp
    write(lun,'(a)') trim(adjustl(s))
    do jproc = 1, nrproc
       if (xpa(iproc)%xpflg(jproc).eq.1) then
          write(sa(1),*) jproc-1
          write(sa(2),*) xpa(iproc)%xp(jproc)%nid
          write(lun,'(1(a,1x),a)')(trim(adjustl(sa(j))),j=1,2)
          do i = 1, xpa(iproc)%xp(jproc)%nid
             id = xpa(iproc)%xp(jproc)%id(i)
             write(sa(1),*) id
             write(sa(2),*) iwrk(1,id)
             write(sa(3),*) iwrk(2,id)
             write(sa(4),*) iwrk(3,id)
             write(lun,'(3(a,1x),a)')(trim(adjustl(sa(j))),j=1,4)
          end do
       end if
    end do
    close(lun)
 end do

 ! cleanup
 do iproc = 1, nrproc
    if (associated(xpa(iproc)%xpflg)) deallocate(xpa(iproc)%xpflg)
    do jproc = 1, nrproc
       if (associated(xpa(iproc)%xp(jproc)%id)) &
          deallocate(xpa(iproc)%xp(jproc)%id)    
    end do
 end do
 deallocate(xpa)
 deallocate(iwrk)
 
 return   
 end subroutine pks_imod_utl_iarmwp_xch_write
  
!###====================================================================
 subroutine pks_imod_utl_iarmwp_xch_read(modwd)
!###====================================================================
 ! modules
 use pks_iarmwp
 use pksmpi_mod, only: myrank
 ! arguments
 character(len=*), intent(in) :: modwd
 ! locals
 character(len=1000) :: fname
 logical :: lpks
 integer :: lun, ixp, i, xprnk, nid, id, il, ir, ic, n
!......................................................................
 call pks7mpiactive(lpks)
 if (.not.lpks) return
 if (.not.liarmwp) return
 
 lun = 0
 write(fname,'(2a,i3.3)') trim(modwd),'\pks_xch_iarmwp.p', myrank 
 call osd_s_filename(fname)
 write(*,'(a,1x,2a)') 'Reading',trim(fname),'...'
 call imod_utl_openasc(lun,fname,'r')
 
 read(lun,*) nrxp
 allocate(xp(max(nrxp,1)))
 
 n = 0
 do ixp = 1, nrxp     
    read(lun,*) xprnk, nid  
    n = n + nid
    xp(ixp)%xprnk = xprnk; xp(ixp)%nid = nid
    allocate(xp(ixp)%idx(nid))
    allocate(xp(ixp)%id(nid))
    allocate(xp(ixp)%lrc(3,nid))
    do i = 1, nid 
       read(lun,*) id, il, ir, ic
       xp(ixp)%id(i)    = id
       xp(ixp)%lrc(1,i) = il 
       xp(ixp)%lrc(2,i) = ir 
       xp(ixp)%lrc(3,i) = ic 
    end do 
 end do
 close(lun)
 
 lenb = max(n,1)
 allocate(sb(lenb),rb(lenb))
 
 return   
 end subroutine pks_imod_utl_iarmwp_xch_read 
 
!###====================================================================
subroutine pks_imod_utl_iarmwp_xch(x,xcht)
!###====================================================================
! modules
 use pks_iarmwp
 use pksmpi_mod, only: myrank
 use pksmpiwrp_mod

! arguments
 real, dimension(*), intent(inout) :: x
 character(len=1), intent(in) :: xcht
! locals
 logical :: lpks
 integer :: ixp, bufptr, buflen, i, j, nrxp2
!......................................................................
 
 call pks7mpiactive(lpks)
 if (.not.lpks) return

 bufptr = 1
 nrxp2 = 0
 do ixp = 1, nrxp ! loop over exchange partners
    nrxp2 = nrxp2 + 1 
    xp(ixp)%xpbuf = bufptr
    buflen = xp(ixp)%nid
    call pks7mpiwrpirecvr( rb(bufptr),&
                           buflen,&
                           xp(ixp)%xprnk,&
                           0,&
                           pks7mpiwrpcomm_world,&
                           rreq(nrxp2) )
    bufptr = bufptr + buflen
 end do ! ixp

 ! check
 bufptr = bufptr - 1
 if (bufptr.gt.lenb) then
    write(*,*) 'Program error pks_imod_utl_iarmwp_xch: lenb!'
    call pksstop(' ')
 end if

 ! pack
 bufptr = 1
 do ixp = 1, nrxp
    do i = 1, xp(ixp)%nid
       j = xp(ixp)%idx(i) 
       if (j.eq.0) then
          write(*,*) 'Program error pks_imod_utl_iarmwp_xch: packing!',&
                      myrank, ixp, nrxp, j
          write(*,*) 'index ',j
          call pksstop(' ')
       end if
       sb(bufptr) = x(abs(j))
       if (xcht.eq.'q') then
          ! write(*,*) '@@@ PCK Q:', myrank, j, xp(ixp)%id(i), sb(bufptr)    
       end if    
      bufptr = bufptr + 1
    end do
 end do ! ixp
 
 ! check
 bufptr = bufptr - 1
 if (bufptr.gt.lenb) then
    write(*,*) 'Program error pks_imod_utl_iarmwp_xch: lenb!'
    call pksstop(' ')
 end if
      
 bufptr = 1

 nrxp2 = 0
 do ixp = 1, nrxp
    nrxp2 = nrxp2 + 1
    buflen = xp(ixp)%nid
    call pks7mpiwrpisendr( sb(bufptr),&
                           buflen,&
                           xp(ixp)%xprnk,&
                           0,&
                           pks7mpiwrpcomm_world,&
                           sreq(nrxp2) )
    bufptr = bufptr + buflen
 end do ! ixp

 ! check
 bufptr = bufptr - 1
 if (bufptr.gt.lenb) then
    write(*,*) 'Program error pks_imod_utl_iarmwp_xch: lenb!'
    call pksstop(' ')
 end if

 call pks7mpiwrpwaitall( nrxp2, rreq )

 ! unpacking
 do ixp = 1, nrxp
    bufptr = xp(ixp)%xpbuf
    do i = 1, xp(ixp)%nid
       j = xp(ixp)%idx(i)
       if (j.eq.0) then
          write(*,*) 'Program error pks_imod_utl_iarmwp_xch: unpacking!',&
                      myrank, ixp, nrxp, j
          call pksstop(' ')
       end if
       if (xcht.eq.'q') then
          x(abs(j)) = x(abs(j)) + rb(bufptr)
          ! write(*,*) '@@@ UPCK Q:', myrank, j, xp(ixp)%id(i), rb(bufptr), x(abs(j))
       end if   
       if (xcht.eq.'h') then
          if (j.gt.0) then ! I do not have this cell
             x(j) = rb(bufptr)   
             ! write(*,*) '@@@ UPCK H:', myrank, j, xp(ixp)%id(i), rb(bufptr)
          end if  
       end if      
       bufptr = bufptr + 1
    end do
 end do ! ixp
      
 ! check
 bufptr = bufptr - 1
 if (bufptr.gt.lenb) then
    write(*,*) 'Program error pks_imod_utl_iarmwp_xch: lenb!'
    call pksstop(' ')
 end if

 call pks7mpiwrpwaitall( nrxp2, sreq ) 
 
 return
 end subroutine pks_imod_utl_iarmwp_xch
 
 end module pks_imod_utl
    
    