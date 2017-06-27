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

module timing_module

implicit none

logical, save :: ltiming = .false.

integer, parameter :: mxnprim = 10
integer, parameter :: mxnsec  = 20
integer, parameter :: mxnproc = 1000
integer, parameter :: mxstrlen = 1000
 
type tsecType
   character(len=mxstrlen) :: name
   integer, dimension(8) :: ibeg
   logical :: lstart = .false.
   logical :: lsum   = .true.
   real :: t = 0. 
   real, dimension(mxnproc) :: tp = 0. 
end type tsecType

integer :: nprim = 0
type tprimType
   integer :: nsec = 0    
   character(len=mxstrlen) :: name
   type(tsecType), dimension(mxnsec) :: tsec
   real :: t = 0.
   real, dimension(mxnproc) :: tp = 0. 
   logical :: lstart = .false.
end type tprimType

type(tprimType), dimension(mxnprim) :: tprim

end module timing_module

! =============================================================================
subroutine timing_findps(p,s,i,j)
! =============================================================================

! modules
use timing_module

! arguments
character(len=*), intent(in) :: p, s
integer, intent(out) :: i, j

! locals
integer :: ip, is
!.......................................................................

! first, finds keys
i = 0; j = 0
do ip = 1, nprim
   if (trim(tprim(ip)%name)==trim(p)) then    
      i = ip
      do is = 1, tprim(ip)%nsec  
         if (trim(tprim(ip)%tsec(is)%name)==trim(s)) then    
            j = is
         end if    
      end do
   end if
end do

if (i==0) then
   nprim = nprim + 1
   if(nprim.gt.mxnprim)then
      write(*,*) 'Error timing, increase mxnprim'
      stop 1
   end if
   i = nprim
   tprim(i)%name = trim(p)
end if   
if (j==0) then
   tprim(i)%nsec = tprim(i)%nsec + 1
   if(tprim(i)%nsec.gt.mxnsec)then
      write(*,*) 'Error timing, increase mxnsec'
      stop 1
   end if
   j = tprim(i)%nsec
   tprim(i)%tsec(j)%name = trim(s)
end if

end subroutine

! =============================================================================
subroutine timing_tic(pin,sin)
! =============================================================================
! modules
use timing_module

! arguments
character(len=*), intent(in) :: pin, sin

! locals
character(len=mxstrlen) :: p, s, s2
integer :: ip, is, i, j
!.......................................................................

if (.not.ltiming) return

p = trim(pin)
s = trim(sin)
call cfn_s_lowcase(p)
call cfn_s_lowcase(s)

if (p(1:3).eq.'mpi') then 
   if(index(s,'_barrier').le.0)then 
      s2 = trim(s)//'_barrier'
   else
      s2 = trim(s)
   end if   
   call timing_mpi(p,s2)
end if   

call timing_tic2(p,s)

end subroutine

! =============================================================================
subroutine timing_mpi(pin,sin)
! =============================================================================
! modules
use timing_module
use pksmpi_mod,only:myrank

implicit none

! arguments
character(len=*), intent(in) :: pin, sin
!.......................................................................

!return
call timing_tic2(pin,sin)
call pks7mpibarrier()
call timing_toc(pin,sin)

end subroutine

! =============================================================================
subroutine timing_tic2(pin,sin)
! =============================================================================
! modules
use timing_module

! arguments
character(len=*), intent(in) :: pin, sin

! locals
logical :: lstart
character(len=mxstrlen) :: p, s
integer :: ip, is, i, j
!.......................................................................

if (.not.ltiming) return

p = trim(pin)
s = trim(sin)
call cfn_s_lowcase(p)
call cfn_s_lowcase(s)

call timing_findps(p,s,i,j)

if (tprim(i)%tsec(j)%lstart) then
   write(*,*) 'Error, subroutine timing_tic called twice for '//trim(p)//','//trim(s)
   stop 1
end if

lstart = .false.
do ip = 1, nprim
   if (tprim(ip)%lstart) lstart = .true.
end do
if(lstart)then
   tprim(i)%tsec(j)%lsum = .false.
else
   tprim(i)%lstart = .true.
end if    

tprim(i)%tsec(j)%lstart = .true.
call timing_date(tprim(i)%tsec(j)%ibeg)

end subroutine

! =============================================================================
subroutine timing_toc(pin,sin)
! =============================================================================
! modules
use timing_module

implicit none

! arguments
character(len=*), intent(in) :: pin, sin

! locals
character(len=mxstrlen) :: p, s
integer :: ip, is, i, j
integer, dimension(8) :: iend
!.......................................................................

if (.not.ltiming) return

p = trim(pin)
s = trim(sin)
call cfn_s_lowcase(p)
call cfn_s_lowcase(s)

call timing_findps(p,s,i,j)

if (.not.tprim(i)%tsec(j)%lstart) then
   write(*,*) 'Error, subroutine timing_toc for '//trim(p)//','//trim(s)
   stop 1
end if

call timing_date(iend)
call timing_add(tprim(i)%tsec(j)%ibeg,iend,tprim(i)%tsec(j)%t)
tprim(i)%tsec(j)%lstart = .false.
tprim(i)%lstart = .false.
end subroutine

! =============================================================================
subroutine timing_stat()
! =============================================================================
! modules
use timing_module
use pksmpi_mod, only: myrank

implicit none

! locals
integer :: ip, is, n, i, j, lun, ios, m
integer, dimension(8) :: ibeg
real :: t
real, dimension(:), allocatable :: tt
character(len=mxstrlen) :: s, so, csvfile, fmtstr, pstr
character(len=mxstrlen), dimension(:), allocatable :: sa
! PKS
integer :: nrproc, iproc, gscnt
integer, dimension(:), allocatable :: grcnt, offsets
real, dimension(:), allocatable :: sbuf, rbuf
!.......................................................................

if (.not.ltiming) return

! exchange in case of parallel computing
call pks7mpigetnrproc(nrproc)
if (nrproc.gt.1) then
   ! count and allocate
   allocate(grcnt(nrproc),offsets(nrproc))
   n = 0
   do ip = 1, nprim
      n = n + tprim(ip)%nsec    
   end do 
   gscnt = n
   grcnt(1:nrproc) = n
   n = max(1,n)
   allocate(sbuf(n),rbuf(nrproc*n))
   ! pack
   n = 0
   do ip = 1, nprim
      do is = 1, tprim(ip)%nsec
         n = n + 1
         t = tprim(ip)%tsec(is)%t
         sbuf(n) = t
      end do
   end do    
 
   ! offsets
   offsets(1) = 0
   do iproc = 1, nrproc-1
      offsets(iproc+1) = offsets(iproc) + grcnt(iproc)
   end do
   
   call pks7mpiwrpgathervr( sbuf, gscnt, rbuf, grcnt, offsets, 0 )   
   
   if (myrank.eq.0) then ! unpack      
      n =  offsets(2) 
      do iproc = 2, nrproc  
         do ip = 1, nprim
            do is = 1, tprim(ip)%nsec
               n = n + 1
               t = rbuf(n)
               tprim(ip)%tsec(is)%tp(iproc) = t
            end do
         end do    
      end do
      deallocate(sbuf,rbuf,grcnt,offsets) 
   else
      deallocate(sbuf,rbuf,grcnt,offsets) 
      return
   end if    
end if

! determine subtotals
m = 10+4*nrproc
allocate(tt(nrproc),sa(m))
tt = 0.
do ip = 1, nprim
   do is = 1, tprim(ip)%nsec
      if (tprim(ip)%tsec(is)%lsum) then
         t = tprim(ip)%tsec(is)%t
         tprim(ip)%t = tprim(ip)%t + t ! set
         tt(1) = tt(1) + t
         do iproc = 2, nrproc
           t = tprim(ip)%tsec(is)%tp(iproc)
           tprim(ip)%tp(iproc) = tprim(ip)%tp(iproc) + t ! set
           tt(iproc) = tt(iproc) + t
         end do
      end if
   end do 
end do

! write
call timing_date(ibeg)
write(sa(1),*) 'timing_'
if(nrproc.gt.1)then
   write(sa(1),'(2a,i3.3,a)') trim(sa(1)),'np',nrproc,'_'    
end if
write(sa(2),'(i4,a)')   ibeg(1),'-'
write(sa(3),'(i2.2,a)') ibeg(2),'-'
write(sa(4),'(i2.2,a)') ibeg(3),'_'
write(sa(5),'(i2.2,a)') ibeg(5),'-'
write(sa(6),'(i2.2,a)') ibeg(6),'-'
write(sa(7),'(i2.2)') ibeg(7)
write(sa(8),*) '.csv'
write(csvfile,'(8a)')(trim(adjustl(sa(i))),i=1,8)
write(*,'(1x,a,1x,2a)') 'Writing timing statistics to',trim(csvfile),'...'
lun = 99
open(unit=lun,file=csvfile,status='replace',iostat=ios) 

s = 'Primary,Secundary,Subtotal'
do iproc = 1, nrproc
   write(pstr,'(i3.3)') iproc 
   s = trim(s)//',Time'//trim(adjustl(pstr))
end do    
write(lun,'(a)') trim(s)

do ip = 1, nprim
   do is = 1, tprim(ip)%nsec
      t = tprim(ip)%tsec(is)%t
!      if (t.eq.0.) cycle
      ! fill
      j = 0
      j = j + 1; write(sa(j),'(a)') tprim(ip)%name; 
      j = j + 1; write(sa(j),'(a)') ','  
      j = j + 1; write(sa(j),'(a)') tprim(ip)%tsec(is)%name  
      j = j + 1; write(sa(j),'(a)') ','  
      j = j + 1; write(sa(j),*)     tprim(ip)%tsec(is)%lsum 
      j = j + 1; write(sa(j),'(a)') ',' 
      j = j + 1; write(sa(j),*)     tprim(ip)%tsec(is)%t 
      do iproc = 2, nrproc
         j = j + 1; write(sa(j),'(a)') ',' 
         j = j + 1; write(sa(j),*) tprim(ip)%tsec(is)%tp(iproc) 
      end do
      write(fmtstr,'(a,i3,a)') '(',j,'a)'
      write(lun,fmtstr)(trim(adjustl(sa(i))),i=1,j)
   end do 
   j = 0
   j = j + 1; write(sa(j),'(a)') tprim(ip)%name  
   j = j + 1; write(sa(j),'(a)') ',' 
   j = j + 1; write(sa(j),'(a)') 'Subtotal'
   j = j + 1; write(sa(j),'(a)') ','  
   j = j + 1; write(sa(j),'(a)') ','  
   j = j + 1; write(sa(j),*)     tprim(ip)%t  
   do iproc = 2, nrproc
      j = j + 1; write(sa(j),'(a)') ',' 
      j = j + 1; write(sa(j),*) tprim(ip)%tp(iproc) 
   end do
   write(fmtstr,'(a,i3,a)') '(',j,'a)'
   write(lun,fmtstr)(trim(adjustl(sa(i))),i=1,j)
end do
j = 0
j = j + 1; write(sa(j),'(a)') 'Total:'  
j = j + 1; write(sa(j),'(a)') ','   
j = j + 1; write(sa(j),'(a)') ','  
do iproc = 1, nrproc
   j = j + 1; write(sa(j),'(a)') ',' 
   j = j + 1; write(sa(j),*) tt(iproc); 
end do    
write(fmtstr,'(a,i3,a)') '(',j,'a)'
write(lun,fmtstr)(trim(adjustl(sa(i))),i=1,j)

close(lun)

! initialize timer
do ip = 1, nprim
   tprim(ip)%t = 0.
   do iproc = 2, nrproc
      tprim(ip)%tp(iproc) = 0.
   end do    
   do is = 1, tprim(ip)%nsec
      do iproc = 2, nrproc
         tprim(ip)%tsec(is)%tp(iproc)  = 0.
      end do    
   end do   
end do

deallocate(tt,sa)

end subroutine

! =============================================================================
subroutine timing_fmt(s,elsec)
! =============================================================================

! use modules
use timing_module  

implicit none

! arguments
character(len=*), intent(out) :: s
real, intent(in) :: elsec

! locals
integer, parameter :: NSPD = 86400
integer, dimension(12) :: idpm(12)
data idpm/31,28,31,30,31,30,31,31,30,31,30,31/ ! days per month
integer :: ndays, nhours, nmins, nsecs, msecs 
real :: rsecs, nrsecs
!.......................................................................

s = ''
            NDAYS = ELSEC/NSPD
            RSECS = MOD(ELSEC,86400.0)
            NHOURS = RSECS/3600.0
            RSECS = MOD(RSECS,3600.0)
            NMINS = RSECS/60.0
            RSECS = MOD(RSECS,60.0)
            NSECS = RSECS
            RSECS = MOD(RSECS,1.0)
            MSECS = NINT(RSECS*1000.0)
            NRSECS = NSECS
            IF (RSECS.GE.0.5) NRSECS=NRSECS+1   
            IF (NDAYS.GT.0) THEN
               WRITE(s,1010) NDAYS,NHOURS,NMINS,NRSECS
 1010          FORMAT(I3,' Days, ',I2,' Hours, ',I2,' Minutes, ',I2,' Seconds')
            ELSEIF (NHOURS.GT.0) THEN
               WRITE(s,1020) NHOURS,NMINS,NRSECS
 1020          FORMAT(I2,' Hours, ',I2,' Minutes, ',I2,' Seconds')
            ELSEIF (NMINS.GT.0) THEN
               WRITE(s,1030) NMINS,NSECS,MSECS
 1030          FORMAT(I2,' Minutes, ',I2,'.',I3.3,' Seconds')
            ELSE
               WRITE(s,1040) NSECS,MSECS
 1040          FORMAT(I2,'.',I3.3,' Seconds')
            ENDIF
            
end subroutine            

! =============================================================================
subroutine timing_pause(s)
! =============================================================================
      
implicit none

! arguments
character(len=*), intent(in) :: s

!.......................................................................

return
write(*,'(a,1x,a)') 'Pausing message:',trim(s)
pause
      
end subroutine

      subroutine timing_date(idt)
      use pksmpitim_mod, only: ltiming
      implicit none
!        Arguments      
      integer, dimension(8), intent(out) :: idt
     
      if (.not.ltiming) return
      
      call date_and_time(values=idt)      
      
      end subroutine
      
      subroutine timing_add(ibdt,iedt,val)
      use pksmpitim_mod, only: ltiming
      implicit none
!        Arguments      
      integer, dimension(8), intent(in) :: ibdt, iedt
      real, intent(inout) :: val
!        Locals
      integer, parameter :: NSPD = 86400
      integer, dimension(12) :: idpm(12)
      data idpm/31,28,31,30,31,30,31,31,30,31,30,31/ ! days per month
      integer :: ndays, nhours, nmins, nsecs, msecs 
      integer :: leap, mb, mc, me, mm, m, nm, ibd, ied
      real :: elsec, rsecs

      if (.not.ltiming) return
      
!     Calculate elapsed time in days and seconds
      NDAYS=0
      LEAP=0
      IF (MOD(IEDT(1),4).EQ.0) LEAP = 1
      IBD = IBDT(3)            ! BEGIN DAY
      IED = IEDT(3)            ! END DAY
!     FIND DAYS
      IF (IBDT(2).NE.IEDT(2)) THEN
!       MONTHS DIFFER
        MB = IBDT(2)             ! BEGIN MONTH
        ME = IEDT(2)             ! END MONTH
        NM = ME-MB+1             ! NUMBER OF MONTHS TO LOOK AT
        IF (MB.GT.ME) NM = NM+12
        MC=MB-1
        DO 10 M=1,NM
          MC=MC+1                ! MC IS CURRENT MONTH
          IF (MC.EQ.13) MC = 1
          IF (MC.EQ.MB) THEN
            NDAYS = NDAYS+IDPM(MC)-IBD
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ELSEIF (MC.EQ.ME) THEN
            NDAYS = NDAYS+IED
          ELSE
            NDAYS = NDAYS+IDPM(MC)
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ENDIF
   10   CONTINUE
      ELSEIF (IBD.LT.IED) THEN
!       START AND END IN SAME MONTH, ONLY ACCOUNT FOR DAYS
        NDAYS = IED-IBD
      ENDIF
      ELSEC=NDAYS*NSPD
!
!     ADD OR SUBTRACT SECONDS
      ELSEC = ELSEC+(IEDT(5)-IBDT(5))*3600.0
      ELSEC = ELSEC+(IEDT(6)-IBDT(6))*60.0
      ELSEC = ELSEC+(IEDT(7)-IBDT(7))
      ELSEC = ELSEC+(IEDT(8)-IBDT(8))*0.001      
      
      val = val + elsec
      
      end subroutine