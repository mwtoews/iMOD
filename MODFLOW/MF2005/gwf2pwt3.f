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

      module getfrombcflpf

      contains

      subroutine getbcfmincminkd(imkd,imc,mkd,mc)
      use gwfbcfmodule, only: iminkd,iminc,minkd,minc
      implicit none
      integer, pointer, intent(inout) :: imkd, imc
      real, pointer, intent(inout) :: mkd, mc

      imkd => iminkd
      imc  => iminc
      mkd  => minkd
      mc   => minc
      end subroutine getbcfmincminkd

      subroutine getbcfsc1(s1)
      use gwfbcfmodule, only: sc1
      implicit none
      real, pointer, dimension(:,:,:), intent(inout) :: s1
      s1 => sc1
      end subroutine getbcfsc1

      subroutine getlpfmincminkd(imkd,imc,mkd,mc)
      use gwflpfmodule, only: iminkd,iminc,minkd,minc
      implicit none
      integer, pointer, intent(out) :: imkd, imc
      real, pointer, intent(out) :: mkd, mc

      imkd => iminkd
      imc  => iminc
      mkd  => minkd
      mc   => minc
      end subroutine getlpfmincminkd

      subroutine getlpfsc1(s1)
      use gwflpfmodule, only: sc1
      implicit none
      real, pointer, dimension(:,:,:), intent(inout) :: s1
      s1 => sc1
      end subroutine getlpfsc1

      subroutine getbcflayavg(lavg)
      use gwfbcfmodule, only: layavg
      implicit none
      integer, pointer, dimension(:), intent(inout) :: lavg
      lavg => layavg
      end subroutine

      subroutine getlpflayavg(lavg)
      use gwflpfmodule, only: layavg
      implicit none
      integer, pointer, dimension(:), intent(inout) :: lavg
      lavg => layavg
      end subroutine

      end module getfrombcflpf

      module gwfpwtmodule
         implicit none

         integer, parameter :: ihnew = 2 ! 1=USE HNEW 2=USE HOLD

         integer,parameter :: ipilay = 1       ! (p) layer
         integer,parameter :: ipirow = 2       ! (p) row
         integer,parameter :: ipicol = 3       ! (p) column
         integer,parameter :: ipsc2  = 4       ! (p) freatic storage coefficient of lower layer
         integer,parameter :: ipbot  = 5       ! (p) bottom of aquifer
         integer,parameter :: iptop2 = 6       ! (p) top of lower layer (leest dikte)
         integer,parameter :: ipthkaf= 7       ! (p) thickness of aquifer
         integer,parameter :: ipvcont= 8       ! (p) vcont of pwt-element (leest c)
         integer,parameter :: ipsc1  = 9       ! (p) backup for storage

         real,save,dimension(:,:,:),pointer :: pwt_kd
         integer,save,pointer               :: npwt
         real,save,dimension(:,:),pointer   :: pwt
         type gwfpwttype
            real,dimension(:,:,:),pointer :: pwt_kd
            integer,pointer               :: npwt
            real,dimension(:,:),pointer   :: pwt
         end type
         type(gwfpwttype), save:: gwfpwtdat(10)
      end module

      subroutine gwf2pwt3da(igrid)
c deallocate purged water table memory
      use gwfpwtmodule
      implicit none
      integer, intent(in) :: igrid

      call sgwf2pwt3pnt(igrid)

      deallocate(pwt_kd)
      deallocate(npwt)
      deallocate(pwt)

      end subroutine

      subroutine sgwf2pwt3pnt(igrid)
c  change purged water table data to a different grid.
      use gwfpwtmodule
      implicit none
      integer, intent(in) :: igrid

      pwt_kd => gwfpwtdat(igrid)%pwt_kd
      npwt   => gwfpwtdat(igrid)%npwt
      pwt    => gwfpwtdat(igrid)%pwt

      end subroutine

      subroutine sgwf2pwt3psv(igrid)
C save purged water table data for a grid.
      use gwfpwtmodule
      implicit none
      integer, intent(in) :: igrid

      gwfpwtdat(igrid)%pwt_kd => pwt_kd
      gwfpwtdat(igrid)%npwt   => npwt
      gwfpwtdat(igrid)%pwt    => pwt

      end subroutine

      subroutine gwf2pwt3ar(inpwt,inbcf,igrid,iout)
      use getfrombcflpf
      use global, only: kdsv, ibound
      use gwfpwtmodule
      use global, only: nlay,nrow,ncol,delr,delc
      use rdrsmodule, only: nodata

      implicit none

c arguments
      integer, intent(in) :: inpwt            ! package input file
      integer, intent(in) :: inbcf
      integer, intent(in) :: igrid            ! grid number
      integer, intent(in) :: iout

c parameters
      real, parameter :: tiny = 1.0e-20

c locals
      character(len=24) :: aname(6)
      data aname(1) /'pwt model layer         '/
      data aname(2) /'pwt storage top aquifer '/
      data aname(3) /'pwt top aquitard        '/
      data aname(4) /'pwt thick. aquitard     '/
      data aname(5) /'pwt thick. top aquifer  '/
      data aname(6) /'pwt resis. aquitard     '/

      integer :: i, ip, iact, ilay, irow, icol
      integer, pointer :: iminkd => null(), iminc => null()
      real :: rval, dxy
      real, pointer :: minkd => null(), minc => null()
      real, dimension(:,:,:), pointer :: sc1 => null()
c ------------------------------------------------------------------------------

c allocate
      allocate(npwt)
      allocate(pwt_kd(ncol,nrow,nlay))

c read the PWT data
      do i = 1, 6
         call u2drel(pwt_kd(1,1,1),aname(i),nrow,ncol,1,inpwt,iout)
         if (i.eq.1) then
            do iact = 1, 2
               npwt = 0
               do irow = 1, nrow
                  do icol = 1, ncol
                     rval = pwt_kd(icol,irow,1)
                     if (rval.ne.nodata) then
                         ilay = int(rval)
                         if (ilay.gt.0.and.ilay.le.nlay)then
                          if(ibound(icol,irow,ilay).gt.0) then
                           npwt = npwt + 1
                           if (iact.eq.2) then
                            pwt(1,npwt) = ilay
                            pwt(2,npwt) = irow
                            pwt(3,npwt) = icol
                           end if
                          endif
                         end if
                     end if
                  end do
               end do
               if (iact.eq.1) then
                  allocate(pwt(9,max(1,npwt)))
               end if
            end do
         else
            do ip = 1, npwt
               irow = pwt(2,ip)
               icol = pwt(3,ip)
               rval = pwt_kd(icol,irow,1)
               if (rval.eq.nodata) then
                  write(*,*) 'Error PWT: reading nodata for ',
     1                    trim(aname(i)),' (icol,irow) =',icol,',',irow
                  call ustop(' ')
               else
                   pwt(i+2,ip) = rval
               end if
            end do
         end if
      end do

c init the PWT data
      if (inbcf.gt.0) then
         call sgwf2bcf7pnt(igrid)
         call getbcfmincminkd(iminkd,iminc,minkd,minc)
         call getbcfsc1(sc1)
      else
         call sgwf2lpf7pnt(igrid)
         call getlpfmincminkd(iminkd,iminc,minkd,minc)
         call getlpfsc1(sc1)
      end if
      
      do ilay=1,nlay; do irow=1,nrow; do icol=1,ncol
       pwt_kd(icol,irow,ilay) = kdsv(icol,irow,ilay)
      enddo; enddo; enddo
      do ip = 1, npwt
         ilay = pwt(ipilay,ip)
         irow = pwt(ipirow,ip)
         icol = pwt(ipicol,ip)
         if (iminc.eq.1) then
            pwt(ipvcont,ip) = max(minc,pwt(ipvcont,ip))
         end if
         dxy = delr(icol)*delc(irow)
         pwt(ipvcont,ip) = (1.0/(pwt(ipvcont,ip)+tiny))*dxy ! c--> conductance
         pwt(ipsc2,ip) = pwt(ipsc2,ip)*dxy                  ! muliply psc2 by cell area
         pwt(ipsc1,ip) = sc1(icol,irow,ilay+1)              ! backup sc for ilay+1
         if (ilay.gt.1) then
            pwt_kd(icol,irow,ilay)=sum(pwt_kd(icol,irow,1:ilay))
            if (iminkd.eq.1) pwt_kd(icol,irow,1:ilay-1)=minkd
         end if
      end do

c save data
      call sgwf2pwt3psv(igrid)

      end subroutine

      subroutine gwf2pwt3fm(kiter,kper,igrid,inbcf)
      use getfrombcflpf
      use gwfpwtmodule
      use global, only: nlay,nrow,ncol,delr,delc,cc,cr,cv,
     1                  ibound,hnew,hold,issflg,rhs
      use gwfbcfmodule, only: laycon
      use gwflpfmodule, only: laytyp

      implicit none

c arguments
      integer, intent(in) :: kiter
      integer, intent(in) :: kper
      integer, intent(in) :: igrid            ! grid number
      integer, intent(in) :: inbcf

c parameters
      real, parameter :: tiny = 1.0e-20

c locals
      integer :: iss, ip, ilay, irow, icol, i, ir, ic, ii
      integer, dimension(:), pointer :: layavg => null()
      real :: h, h1, t, fct, q1, q2
      real, dimension(:,:,:), pointer :: sc1 => null()
      
      integer,dimension(3,4) :: rci
      data rci/-1, 0,-1,  !## row/col/cc (north)
     1    0, 1, 2,        !## row/col/cr (east)
     1    1, 0, 1,        !## row/col/cc (south)
     1    0,-1,-2/        !## row/col/cr (west)

c ------------------------------------------------------------------------------
      
c check if PWT can be used in combination with BCF and LPF
      if (inbcf.gt.0) then
         call sgwf2bcf7pnt(igrid)
         call getbcflayavg(layavg)
         if (sum(laycon).ne.0) then
            write(*,*) 'Error, PWT package can only be used',
     1         ' with confined layers.'
            call ustop(' ')
         end if
      else
         call sgwf2lpf7pnt(igrid)
         call getlpflayavg(layavg)
         if (sum(laytyp).ne.0) then
            write(*,*) 'Error, PWT package can only be used',
     1         ' with confined layers.'
            call ustop(' ')
         end if
      end if
      if (sum(layavg).ne.0) then
         write(*,*) 'Error, PWT package can only be used',
     1      ' with horizontal harmonic weighed transmissivities.'
         call ustop(' ')
      end if

c get iss for this stress period
      iss=issflg(kper)

c return if steady-state
      if (iss.ne.0) return

c init the PWT data
      if (inbcf.gt.0) then
         call sgwf2bcf7pnt(igrid)
         call getbcfsc1(sc1)
      else
         call sgwf2lpf7pnt(igrid)
         call getlpfsc1(sc1)
      end if

      ! reset parameters
      do ip = 1, npwt
         ilay = pwt(ipilay,ip)
         irow = pwt(ipirow,ip)
         icol = pwt(ipicol,ip)
         if (ibound(icol,irow,ilay)  .gt.0 .and.
     1       ibound(icol,irow,ilay+1).gt.0) then
            sc1(icol,irow,ilay+1)= pwt(ipsc1,ip)
            cv(icol,irow,ilay)   = pwt(ipvcont,ip)
         end if   
      end do
      do icol=1,ncol; do irow=1,nrow; do ilay=1,nlay
       cc(icol,irow,ilay) = pwt_kd(icol,irow,ilay)
      enddo; enddo; enddo
      
      !## store top of pwt
      rhs=-9999.0
  
      do ip = 1, npwt
         ilay = pwt(ipilay,ip)
         irow = pwt(ipirow,ip)
         icol = pwt(ipicol,ip)

         if (ibound(icol,irow,ilay)  .gt.0 .and.
     1       ibound(icol,irow,ilay+1).gt.0) then
         
            !## store top of pwt layer
            rhs(icol,irow,ilay)=pwt(ipbot,ip) !xpwt(i,ipbot)
   
            !## storage below pwt layer
            t =  pwt(ipbot,ip)- pwt(iptop2,ip)
            if(ihnew.eq.1)then
               h =real(hnew(icol,irow,ilay))
               h1=real(hnew(icol,irow,ilay+1))
            elseif(ihnew.eq.2)then
               h =real(hold(icol,irow,ilay))
               h1=real(hold(icol,irow,ilay+1))
            endif
            if(h1.lt.t)sc1(icol,irow,ilay+1)=pwt(ipsc2,ip) !## conductance
   
            !## adjust transmissivity
            fct=(h-pwt(ipbot,ip))/pwt(ipthkaf,ip)
            cc(icol,irow,ilay)=max(0.01,cc(icol,irow,ilay)*fct)
   
            !## head below pwt is lower than pwt layer
            if(h1.lt.pwt(ipbot,ip))then
               !## force cv to yield hnew>=bot()
               q1=max(0.0,h-pwt(ipbot,ip))*cv(icol,irow,ilay)
               !## computed by ax=b
               q2=(h-h1)*cv(icol,irow,ilay)
               !## correction to vertical conductance (smaller)
               fct=0.0; if(q2.gt.0.0) fct=q1/q2 !(q2+tiny)
               fct=max(tiny,fct)
               cv(icol,irow,ilay)=fct*cv(icol,irow,ilay)
            endif
         end if
      end do
      
      ! compute transmissivities using harmonic mean
      do ilay = 1, nlay
         !## no usage of minkd for pwt - isotropic
         call sgwf2bcf7c(ilay,0,0.0,1.0) !iminkd,minkd) !,cc,cr)
      end do

      !## correct harmonic conductances whenever next cells are dry (below top pwt)
      do ilay=1,nlay
       do irow=1,nrow
        do icol=1,ncol
         !## found pwt top
         if(rhs(icol,irow,ilay).eq.-9999.0)cycle
         do i=1,4
          ir=min(nrow,max(1,irow+rci(1,i)))
          ic=min(ncol,max(1,icol+rci(2,i)))
          ii=     rci(3,i)
          if(rhs(ic,ir,ilay).ne.-9999.0)cycle
          if(ihnew.eq.1)then
           h1=hnew(ic,ir,ilay)
          elseif(ihnew.eq.2)then
           h1=hold(ic,ir,ilay)
          endif
          if(h1.lt.rhs(icol,irow,ilay))then
           select case (ii)
            case (-1) !## north
             cc(icol,irow-1,ilay)=0.0
            case (2) !## east
             cr(icol,irow,ilay)=0.0
            case (1) !## south
             cc(icol,irow,ilay)=0.0
            case (-2) !## west
             cr(icol-1,irow,ilay)=0.0
           end select
          endif
         enddo
        enddo
       enddo
      enddo

      !## clear rhs
      rhs=0.0
      
      ! extra checks conform imod
      do ilay=1,nlay
         do irow=1,nrow-1
            do icol=1,ncol-1
               if(ibound(icol,irow,ilay).eq.0)
     1            cr(icol,irow,ilay)=0.0
               if(ibound(icol+1,irow,ilay).eq.0)
     1            cr(icol,irow,ilay)=0.0
               if(ibound(icol,irow,ilay).eq.0)
     1            cc(icol,irow,ilay)=0.0
               if(ibound(icol,irow+1,ilay).eq.0)
     1            cc(icol,irow,ilay)=0.0
            enddo
         enddo
      enddo
      
      do ilay=1,nlay
         do irow=1,nrow; cr(ncol,irow,ilay)=0.0; enddo
         do icol=1,ncol; cc(icol,nrow,ilay)=0.0; enddo
      enddo
c      cr(ncol,1:nrow,1:nlay)=0.0
c      cc(1:ncol,nrow,1:nlay)=0.0

      ! save pointers
      call sgwf2bas7psv(igrid)

      end subroutine

