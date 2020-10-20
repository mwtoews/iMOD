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

      module gwfanimodule
         integer, save, dimension(:), pointer  :: indx      ! (4)
         real, save, dimension(:,:,:), pointer :: anifactor ! (ncol,nrow,nlay)
         real, save, dimension(:,:,:), pointer :: aniangle  ! (ncol,nrow,nlay)
         real, save, dimension(:,:,:), pointer :: dcc       ! (ncol,nrow,nlay)
         real, save, dimension(:,:,:), pointer :: dcr       ! (ncol,nrow,nlay)
         real, save, dimension(:,:,:), pointer :: dcu       ! (ncol,nrow,nlay)
         real, save, dimension(:,:,:), pointer :: dcd       ! (ncol,nrow,nlay)
         real, save, dimension(:,:,:), pointer :: kxx       ! (ncol,nrow,nlay)
         real, save, dimension(:,:,:), pointer :: kyy       ! (ncol,nrow,nlay)
         real, save, dimension(:,:,:), pointer :: kxy       ! (ncol,nrow,nlay)
         real, save, dimension(:,:,:), pointer :: diag      ! (ncol,nrow,nlay)
         real, save, dimension(:,:,:), pointer :: dfu       ! (ncol,nrow,nlay)
         real, save, dimension(:,:,:), pointer :: dfd       ! (ncol,nrow,nlay)
         real, save, dimension(:,:,:), pointer :: dff       ! (ncol,nrow,nlay)
         real, save, dimension(:,:,:), pointer :: dfr       ! (ncol,nrow,nlay)
         real, save, dimension(:,:),   pointer :: a         ! (4,4)
         real, save, dimension(:,:),   pointer :: b         ! (4,4)
         real, save, dimension(:),     pointer :: c         ! (ncol*nrow*16)

         type gwfanitype
            integer, dimension(:), pointer  :: indx      ! (4)
            real, dimension(:,:,:), pointer :: anifactor ! (ncol,nrow,nlay)
            real, dimension(:,:,:), pointer :: aniangle  ! (ncol,nrow,nlay)
            real, dimension(:,:,:), pointer :: dcc       ! (ncol,nrow,nlay)
            real, dimension(:,:,:), pointer :: dcr       ! (ncol,nrow,nlay)
            real, dimension(:,:,:), pointer :: dcu       ! (ncol,nrow,nlay)
            real, dimension(:,:,:), pointer :: dcd       ! (ncol,nrow,nlay)
            real, dimension(:,:,:), pointer :: kxx       ! (ncol,nrow,nlay)
            real, dimension(:,:,:), pointer :: kyy       ! (ncol,nrow,nlay)
            real, dimension(:,:,:), pointer :: kxy       ! (ncol,nrow,nlay)
            real, dimension(:,:,:), pointer :: diag      ! (ncol,nrow,nlay)
            real, dimension(:,:,:), pointer :: dfu       ! (ncol,nrow,nlay)
            real, dimension(:,:,:), pointer :: dfd       ! (ncol,nrow,nlay)
            real, dimension(:,:,:), pointer :: dff       ! (ncol,nrow,nlay)
            real, dimension(:,:,:), pointer :: dfr       ! (ncol,nrow,nlay)
            real, dimension(:,:),   pointer :: a         ! (4,4)
            real, dimension(:,:),   pointer :: b         ! (4,4)
            real, dimension(:),     pointer :: c         ! (ncol*nrow*16)
         end type
         type(gwfanitype), save:: gwfanidat(10)

      end module gwfanimodule

      subroutine gwf2ani3da(igrid)
      use gwfanimodule
      implicit none
      integer, intent(in) :: igrid
C
      call sgwf2ani3pnt(igrid)
      deallocate(indx)
      deallocate(anifactor)
      deallocate(aniangle)
      deallocate(dcc)
      deallocate(dcr)
      deallocate(dcu)
      deallocate(dcd)
      deallocate(kxx)
      deallocate(kyy)
      deallocate(kxy)
      deallocate(diag)
      deallocate(dfu)
      deallocate(dfd)
      deallocate(dff)
      deallocate(dfr)
      deallocate(a)
      deallocate(b)
      deallocate(c)
C
      return
      end

      subroutine sgwf2ani3pnt(igrid)
      use gwfanimodule
      implicit none
      integer, intent(in) :: igrid
C
      indx      => gwfanidat(igrid)%indx
      anifactor => gwfanidat(igrid)%anifactor
      aniangle  => gwfanidat(igrid)%aniangle
      dcc       => gwfanidat(igrid)%dcc
      dcr       => gwfanidat(igrid)%dcr
      dcu       => gwfanidat(igrid)%dcu
      dcd       => gwfanidat(igrid)%dcd
      kxx       => gwfanidat(igrid)%kxx
      kyy       => gwfanidat(igrid)%kyy
      kxy       => gwfanidat(igrid)%kxy
      diag      => gwfanidat(igrid)%diag
      dfu       => gwfanidat(igrid)%dfu
      dfd       => gwfanidat(igrid)%dfd
      dff       => gwfanidat(igrid)%dff
      dfr       => gwfanidat(igrid)%dfr
      a         => gwfanidat(igrid)%a
      b         => gwfanidat(igrid)%b
      c         => gwfanidat(igrid)%c
C
      return
      end

      subroutine sgwf2ani3psv(igrid)
      use gwfanimodule
      implicit none
      integer, intent(in) :: igrid
C
      gwfanidat(igrid)%indx      => indx
      gwfanidat(igrid)%anifactor => anifactor
      gwfanidat(igrid)%aniangle  => aniangle
      gwfanidat(igrid)%dcc       => dcc
      gwfanidat(igrid)%dcr       => dcr
      gwfanidat(igrid)%dcu       => dcu
      gwfanidat(igrid)%dcd       => dcd
      gwfanidat(igrid)%kxx       => kxx
      gwfanidat(igrid)%kyy       => kyy
      gwfanidat(igrid)%kxy       => kxy
      gwfanidat(igrid)%diag      => diag
      gwfanidat(igrid)%dfu       => dfu
      gwfanidat(igrid)%dfd       => dfd
      gwfanidat(igrid)%dff       => dff
      gwfanidat(igrid)%dfr       => dfr
      gwfanidat(igrid)%a         => a
      gwfanidat(igrid)%b         => b
      gwfanidat(igrid)%c         => c
C
      return
      end

      subroutine gwf2ani3ar(inani,inpwt,inhfb,igrid)
c description:
c ------------------------------------------------------------------------------
c Allocate and read for ani package.
c

c declaration section
c ------------------------------------------------------------------------------
c modules
      use global, only: iout, ncol, nrow, nlay, ibound, delr, delc, cc,
     1                  kdsv, cr, cv, hcof, rhs, buff
      use gwfanimodule
      use gwfpwtmodule, only: npwt,pwt,ipilay,ipirow,ipicol
      use rdrsmodule, only: nodata
      USE GWFHFBMODULE,ONLY:NHFB,HFB
      implicit none

c arguments
      integer, intent(in) :: inani
      integer, intent(in) :: inpwt
      integer, intent(in) :: inhfb
      integer, intent(in) :: igrid

c        locals
      integer :: j, i, k, kk, ilay, irow, icol, n, ip, ii, i1, i2, j1,
     1 j2, icol1, icol2, irow1, irow2
      character(len=1024) :: str

c program section
c ------------------------------------------------------------------------------

      call sgwf2bas7pnt(igrid)

      allocate(indx(4))
      allocate(anifactor(ncol,nrow,nlay))
      allocate(aniangle (ncol,nrow,nlay))
      allocate(dcc      (ncol,nrow,nlay)); dcc = 0.
      allocate(dcr      (ncol,nrow,nlay)); dcr = 0.
      allocate(dcu      (ncol,nrow,nlay)); dcu = 0.
      allocate(dcd      (ncol,nrow,nlay)); dcd = 0.
      allocate(kxx      (ncol,nrow,nlay)); kxx = 0.
      allocate(kyy      (ncol,nrow,nlay)); kyy = 0.
      allocate(kxy      (ncol,nrow,nlay)); kxy = 0.
      allocate(diag     (ncol,nrow,nlay)); diag = 0. 
      allocate(dfu      (ncol,nrow,nlay)); dfu = 0.
      allocate(dfd      (ncol,nrow,nlay)); dfd = 0.
      allocate(dff      (ncol,nrow,nlay)); dff = 0.
      allocate(dfr      (ncol,nrow,nlay)); dfr = 0.
      allocate(a(4,4)); a = 0.
      allocate(b(4,4)); b = 0.
      allocate(c(ncol*nrow*16)); c = 0.

C4------READ ANISOTROPY-FACTOR EN -HOEK
      DO 300 K=1,NLAY
       KK=K
       nodata = 1.
       CALL U2DREL(ANIFACTOR(1,1,k),'anisotropy factor       ',
     1             NROW,NCOL,KK,INANI,IOUT)
       nodata = 0.
       CALL U2DREL(aniangle(1,1,k)  ,'anisotropy angle        ',
     1             NROW,NCOL,KK,INANI,IOUT)
       do j=1,nrow
          do i=1,ncol
             if (anifactor(i,j,k).gt.1.)then
                write(str,*) 'Anisotropyfactor in MODFLOW for',
     1              ' variable anisotropy should be smaller than 1.0'
                write(str,*) trim(str),' icol,irow,ilay,anifactor: ',
     1              i,j,k,anifactor(i,j,k)
                write(iout,*) trim(str)
                write(*,*) trim(str)
                stop
             else if (anifactor(i,j,k).lt.0.)then
                write(str,*) 'Anisotropyfactor in MODFLOW for',
     1              ' variable anisotropy should be larger than 0.'
                write(str,*) trim(str), ' icol,irow,ilay,anifactor: ',
     1              i,j,k,anifactor(i,j,k)
                write(iout,*) trim(str)
                write(*,*) trim(str)
                stop
             end if
          end do
       end do
  300 CONTINUE
      nodata = -9999.

      call pest1alpha_grid('AF',anifactor,nrow,ncol,nlay,iout)               ! IPEST
      call pest1alpha_grid('AA',aniangle,nrow,ncol,nlay,iout,anifactor)      ! IPEST

! removed anisotrophy in case for perched water table cells
      if (inpwt.gt.0) then
         call sgwf2pwt3pnt(igrid)
         buff = 0.0D0
         do ip = 2, npwt
            ilay = pwt(ipilay,ip)
            irow = pwt(ipirow,ip)
            icol = pwt(ipicol,ip)

            do i = max(1,irow-1), min(nrow,irow+1)
               do j = max(1,icol-1), min(ncol,icol+1)
                  do k = 1, ilay
                     buff(j,i,k) = 1.0D0
                  end do
               end do
            end do
         end do
         
         n = 0
         do k = 1, nlay
            do j=1,nrow
               do i=1,ncol
                  if (buff(i,j,k).gt.0.0D0) then
                     if(anifactor(i,j,k).ne.1.0)then
                      anifactor(i,j,k) = 1.0D0
                      n = n + 1
                     endif
                  end if
               end do
            end do
         end do
         write(iout,*) 'ANI check for PWT:',n,'cells removed'
         write(*,*)    'ANI check for PWT:',n,'cells removed'
      end if

! removed anisotrophy in case for fault
      if (inhfb.gt.0) then
       call SGWF2HFB7PNT(IGRID) 
       n=0
       DO II=1,NHFB
        K = HFB(1,II)
        I1 = HFB(2,II)  !row
        J1 = HFB(3,II)  !col
        I2 = HFB(4,II)  !row
        J2 = HFB(5,II)  !col

        IF (I1.EQ.I2) THEN
         icol1=min(j1,j2); icol2=max(j1,j2)
         irow1=i1; irow2=i2        
        endif
        IF (j1.EQ.j2) THEN
         irow1=min(i1,i2); irow2=max(i1,i2)
         icol1=j1; icol2=j2
        endif 
        
        do irow=max(1,irow1-1),min(nrow,irow2+1)
         do icol=max(1,icol1-1),min(ncol,icol2+1)
          if(anifactor(icol,irow,k).lt.1.0)then
           n=n+1; anifactor(icol,irow,k)=1.0
          endif
         enddo
        enddo

!C6------IF I1=I2, MODIFY HORIZONTAL BRANCH CONDUCTANCES ALONG ROW
!C6------DIRECTION.
!        IF (I1.EQ.I2) THEN
!         if(anifactor(J1,I1,k).lt.1.0)then
!          n=n+1; anifactor(J1,I1,k)=1.0
!          write(*,*) j1,i1,k
!         endif
!         if(anifactor(J2,I1,k).lt.1.0)then
!          n=n+1; anifactor(J2,I1,k)=1.0
!          write(*,*) j2,i1,k
!         endif
!        endif
!C6------IF I1=I2, MODIFY HORIZONTAL BRANCH CONDUCTANCES ALONG COLUMN
!C6------DIRECTION.
!        IF (J1.EQ.J2) THEN
!         if(anifactor(J1,I1,k).lt.1.0)then
!          n=n+1; anifactor(J1,I1,k)=1.0
!          write(*,*) j1,i1,k
!         endif
!         if(anifactor(J1,I2,k).lt.1.0)then
!          n=n+1; anifactor(J1,I2,k)=1.0
!          write(*,*) j1,i2,k
!         endif
!        endif
       enddo
       write(iout,*) 'ANI check for HFB:',n,'cells transfered into isotr
     1opic conditions'
       write(*,*)    'ANI check for HFB:',n,'cells transfered into isotr
     1opic conditions'
      endif

      call translatekxx(ncol,nrow,nlay,ibound,kdsv,anifactor,
     1                  aniangle,kxx,kyy,kxy)

      call scl1ten(dcu,dcd,dcc,
     1             dcr,kxx,kyy,kxy,ibound,
     1             delr,delc,ncol,nrow,nlay,
     1             a,b,c,indx)

      call scl1fmd(cc,cr,cv,ibound,dcu,
     1             dcd,dcc,dcr,hcof,rhs,
     1             diag,ncol,nrow,nlay,anifactor)

      ! save pointers to data and return.
      call sgwf2ani3psv(igrid)
          
      return
      end

      subroutine gwf2ani3fm(igrid)
c description:
c ------------------------------------------------------------------------------
c Wrapper routine around scl1fmt.
c

c declaration section
c ------------------------------------------------------------------------------
c modules
      use global, only: rhs, ibound, hnew, ncol, nrow, nlay, hcof
      use gwfanimodule

      implicit none
      character(len=52) :: fname
      
c arguments
      integer, intent(in) :: igrid

c program section
c ------------------------------------------------------------------------------

      ! set pointers for this igrid
      call sgwf2ani3pnt(igrid)
      call sgwf2bas7pnt(igrid)

!      write(fname,'(a,i2.2,a)') 'd:\tmp',1,'.txt'
!      open(99,file=fname,status='unknown',action='write')
!      write(99,*) dcu
!      write(99,*) dcd
!      write(99,*) dcc
!      write(99,*) dcr
!      write(99,*) diag
!      close(99)

      call scl1fmt(dcu,dcd,dcc,dcr,diag,rhs,ibound,hnew,ncol,nrow,
     1             nlay,hcof,anifactor)
      
      call sgwf2bas7psv(igrid)

      return
      end

      subroutine vdf2ani3fm(igrid)
c description:
c ------------------------------------------------------------------------------
c Wrapper routine around scl1fmt.
c

c declaration section
c ------------------------------------------------------------------------------
c modules
      use global, only: rhs, ibound, hnew, ncol, nrow, nlay, hcof
      use gwfanimodule

      implicit none

c arguments
      integer, intent(in) :: igrid

c program section
c ------------------------------------------------------------------------------

      ! set pointers for this igrid
      call sgwf2ani3pnt(igrid)
      call sgwf2bas7pnt(igrid)

      call vdf1fmt(dcu,dcd,dcc,dcr,diag,rhs,ibound,hnew,ncol,nrow,
     1             nlay,hcof,anifactor)
      
      call sgwf2bas7psv(igrid)

      return
      end

      subroutine gwf2ani7bd(igrid,idir)
c description:
c ------------------------------------------------------------------------------
c Wrapper routine around ANI7FM.
c

c declaration section
c ------------------------------------------------------------------------------
c modules
      use global, only: ncol, nrow, nlay, buff, ibound, hnew
      use gwfanimodule

      implicit none

c arguments
      integer, intent(in) :: igrid,idir

c program section
c ------------------------------------------------------------------------------

      ! set pointers for this igrid
      call sgwf2ani3pnt(igrid)

      call ani5fm(dfu,ncol,nrow,nlay,dcu,dcd,dcc,dcr,diag,ibound,hnew,1)
      call ani5fm(dfd,ncol,nrow,nlay,dcu,dcd,dcc,dcr,diag,ibound,hnew,2)
      call ani5fm(dff,ncol,nrow,nlay,dcu,dcd,dcc,dcr,diag,ibound,hnew,3)
      call ani5fm(dfr,ncol,nrow,nlay,dcu,dcd,dcc,dcr,diag,ibound,hnew,4)

      call ani7fm(buff,dfu,dfd,dff,dfr,ncol,nrow,nlay,idir,anifactor)

      return
      end subroutine gwf2ani7bd

!##====================
      subroutine gwf2ani8bd_chd(igrid)
!##====================
      use global, only: ncol, nrow, nlay, buff, ibound, hnew
      use gwfanimodule

      implicit none

c arguments
      integer, intent(in) :: igrid
      integer :: irow, icol, ilay

c program section
c ------------------------------------------------------------------------------

      ! set pointers for this igrid
      call sgwf2ani3pnt(igrid)

      call ani8fm(dfu,ncol,nrow,nlay,dcu,dcd,dcc,dcr,diag,ibound,hnew,1)
      call ani8fm(dfd,ncol,nrow,nlay,dcu,dcd,dcc,dcr,diag,ibound,hnew,2)
      call ani8fm(dff,ncol,nrow,nlay,dcu,dcd,dcc,dcr,diag,ibound,hnew,3)
      call ani8fm(dfr,ncol,nrow,nlay,dcu,dcd,dcc,dcr,diag,ibound,hnew,4)
   
      !## total flux caused by anisotropy and constant heads
      do ilay=1,nlay
       do irow=1,nrow
        do icol=1,ncol
         buff(icol,irow,ilay)=dfu(icol,irow,ilay)+dfd(icol,irow,ilay)+
     1                        dff(icol,irow,ilay)+dfr(icol,irow,ilay)
         buff(icol,irow,ilay)=-1.0D0*buff(icol,irow,ilay)
        enddo
       enddo
      enddo
      
      return
      end subroutine gwf2ani8bd_chd

      !###====================================================================
      subroutine ani8fm(x,ncol,nrow,nlay,dcu,dcd,dcc,dcr,diag,
     1                 ibound,hnew,i)
      !###====================================================================
      implicit none

      integer, intent(in) :: ncol,nrow,nlay
      real, dimension(ncol,nrow,nlay), intent(out) :: x
      real, dimension(ncol,nrow,nlay), intent(in) :: dcu
      real, dimension(ncol,nrow,nlay), intent(in) :: dcc
      real, dimension(ncol,nrow,nlay), intent(in) :: dcd
      real, dimension(ncol,nrow,nlay), intent(in) :: dcr
      real, dimension(ncol,nrow,nlay), intent(in) :: diag
      integer, dimension(ncol,nrow,nlay), intent(in) :: ibound
      double precision, dimension(ncol,nrow,nlay), intent(in) :: hnew
      integer, intent(in) :: i

      real,parameter :: ccrit=10.0e-03
      integer:: ilay,irow,icol
      real :: sumc

      do ilay=1,nlay
       do irow=1,nrow
        do icol=1,ncol
         x(icol,irow,ilay) = 0.
        enddo
       enddo
      enddo
      
      do ilay=1,nlay
       do irow=1,nrow
        do icol=1,ncol

        !## constant head
         if(ibound(icol,irow,ilay).lt.0)then
          sumc=abs(dcu(icol,irow,ilay))+
     1         abs(dcd(icol,irow,ilay))+
     1         abs(dcc(icol,irow,ilay))+
     1         abs(dcr(icol,irow,ilay))+
     1         abs(diag(icol,irow,ilay))
          if(sumc.gt.ccrit)then
            !## connection towards right direction
            if(icol.lt.ncol)then
             if(ibound(icol+1,irow,ilay).gt.0)then
             if(i.eq.4)then
              x(icol,irow,ilay)=x(icol,irow,ilay)+
     1        dcr(icol,irow,ilay)*
     1         (hnew(icol+1,irow,ilay)-hnew(icol,irow,ilay))
             end if
            end if
            if(irow.lt.nrow)then
             !## diagonal
             if(ibound(icol+1,irow+1,ilay).gt.0)then
              if(i.eq.2)then
               x(icol,irow,ilay)=x(icol,irow,ilay)+dcd(icol,irow,ilay)*
     1          (hnew(icol+1,irow+1,ilay)-hnew(icol,irow,ilay))
              endif
             endif
            end if
            if(irow.gt.1)then
             if(ibound(icol+1,irow-1,ilay).gt.0)then
              if(i.eq.1)then
               x(icol,irow,ilay)=x(icol,irow,ilay)+dcu(icol,irow,ilay)*
     1          (hnew(icol+1,irow-1,ilay)-hnew(icol,irow,ilay))
              endif
             endif
            endif
           endif
           !## connection towards bottom direction
           if(irow.lt.nrow)then
            if(ibound(icol,irow+1,ilay).gt.0)then
             if(i.eq.3)then
              x(icol,irow,ilay)=x(icol,irow,ilay)+
     1         dcc(icol,irow,ilay)*
     1         (hnew(icol,irow+1,ilay)-hnew(icol,irow,ilay))
             endif
            endif
           endif
          endif
         endif

!      IF(IBOUND(ICOL+1,IROW,ILAY).LT.0)THEN
!       RHS(ICOL+1,IROW,ILAY)=RHS(ICOL+1,IROW,ILAY)+ANI(IPCK,4)*(HNEW(ICOL,IROW,ILAY)-HNEW(ICOL+1,IROW,ILAY))
!      ENDIF
   
        !## constant head
         if(ibound(icol,irow,ilay).gt.0)then
          sumc=abs(dcu(icol,irow,ilay))+
     1         abs(dcd(icol,irow,ilay))+
     1         abs(dcc(icol,irow,ilay))+
     1         abs(dcr(icol,irow,ilay))+
     1         abs(diag(icol,irow,ilay))
          if(sumc.gt.ccrit)then
            !## connection towards right direction
            if(icol.lt.ncol)then
             if(ibound(icol+1,irow,ilay).lt.0)then
             if(i.eq.4)then
              x(icol+1,irow,ilay)=x(icol+1,irow,ilay)+
     1        dcr(icol,irow,ilay)*
     1         (hnew(icol,irow,ilay)-hnew(icol+1,irow,ilay))
             end if
            end if
            if(irow.lt.nrow)then
             !## diagonal
             if(ibound(icol+1,irow+1,ilay).lt.0)then
              if(i.eq.2)then
               x(icol+1,irow+1,ilay)=x(icol+1,irow+1,ilay)+
     1           dcd(icol,irow,ilay)*
     1          (hnew(icol,irow,ilay)-hnew(icol+1,irow+1,ilay))
              endif
             endif
            end if
            if(irow.gt.1)then
             if(ibound(icol+1,irow-1,ilay).lt.0)then
              if(i.eq.1)then
               x(icol+1,irow-1,ilay)=x(icol+1,irow-1,ilay)+
     1           dcu(icol,irow,ilay)*
     1          (hnew(icol,irow,ilay)-hnew(icol+1,irow-1,ilay))
              endif
             endif
            endif
           endif
           !## connection towards bottom direction
           if(irow.lt.nrow)then
            if(ibound(icol,irow+1,ilay).lt.0)then
             if(i.eq.3)then
              x(icol,irow+1,ilay)=x(icol,irow+1,ilay)+
     1         dcc(icol,irow,ilay)*
     1         (hnew(icol,irow,ilay)-hnew(icol,irow+1,ilay))
             endif
            endif
           endif
          endif
         endif

        end do
       end do
      end do

      return
      end subroutine

!###====================================================================
      subroutine scl1ten(dcu,dcd,dcc,dcr,kxx,kyy,kxy,ibound,
     1                   delr,delc,ncol,nrow,nlay,a,b,c,indx)
!###====================================================================
      use imod_utl, only: imod_utl_ludecomp, imod_utl_lubacksub
      implicit none
      integer ncol,nrow,ilay,nlay,ic1,ic2,ir1,ir2

      integer ibound(ncol,nrow,nlay)
      real c(ncol*nrow*16)
      real a(4,4),b(4,4)
      integer indx(4)
      real dcc(ncol,nrow,nlay),dcr(ncol,nrow,nlay),
     %     dcu(ncol,nrow,nlay),dcd(ncol,nrow,nlay)
      real kxx(ncol,nrow,nlay),kyy(ncol,nrow,nlay),
     %     kxy(ncol,nrow,nlay)
      real(kind=8) delr(ncol)
      real(kind=8) delc(nrow)
      integer irow,icol,i,j,ii,ising,col,row,lay
      real detk,t1,t2

!##initialise variables
      do lay=1,nlay
       do row=1,nrow
        do col=1,ncol
         dcc(col,row,lay)=0.0
         dcr(col,row,lay)=0.0
         dcd(col,row,lay)=0.0
         dcu(col,row,lay)=0.0
        end do
       end do
      end do

      do ilay=1,nlay
!
!######apply weigthing
       do irow=1,nrow
        do icol=1,ncol

         !## set dummy values for inactive cells
         if(ibound(icol,irow,ilay).eq.0)then

          kxx(icol,irow,ilay)=1.0
          kyy(icol,irow,ilay)=1.0
          kxy(icol,irow,ilay)=0.0

         else

!########perform scalings
          if(kxx(icol,irow,ilay).ne.0.0.and.
     1       kyy(icol,irow,ilay).ne.0.0)then
           detk=(kxx(icol,irow,ilay)*kyy(icol,irow,ilay))-
     1           kxy(icol,irow,ilay)**2.0
           kxx(icol,irow,ilay)=kxx(icol,irow,ilay)*
     1           (delc(irow)/delr(icol))/detk
           kyy(icol,irow,ilay)=kyy(icol,irow,ilay)*
     1           (delr(icol)/delc(irow))/detk
           kxy(icol,irow,ilay)=kxy(icol,irow,ilay)/detk
          endif
         
         endif
        enddo 
       enddo  

       do irow=2,nrow
        do icol=1,ncol-1
      
!########construct A
         do i=1,4
          do j=1,4
           a(j,i)=0.0
          end do
         end do 
!########fill main diagonal of dual-mesh - control volume
         a(1,1)= kyy(icol,irow,ilay)+kyy(icol+1,irow,ilay)     ! ten1(2,2)+ten2(2,2);
         a(2,2)= kxx(icol+1,irow,ilay)+kxx(icol+1,irow-1,ilay) ! ten2(1,1)+ten3(1,1);
         a(3,3)= kyy(icol+1,irow-1,ilay)+kyy(icol,irow-1,ilay) ! ten3(2,2)+ten4(2,2);
         a(4,4)= kxx(icol,irow,ilay)+kxx(icol,irow-1,ilay)     ! ten1(1,1)+ten4(1,1);
!########fill off-diagonals of dual mesh - control volume
         a(1,2)=-kxy(icol+1,irow,ilay)                    ! -ten2(1,2);
         a(2,1)= a(1,2)
         a(1,4)=-kxy(icol,irow,ilay)                      ! -ten1(1,2);
         a(4,1)= a(1,4)
         a(2,3)=-kxy(icol+1,irow-1,ilay)                  ! -ten3(1,2);
         a(3,2)= a(2,3)
         a(3,4)=-kxy(icol,irow-1,ilay)                    ! -ten4(1,2);
         a(4,3)= a(3,4)

!########put proper matrix elements for missing control-volume elements
         do i=1,4
          if(a(i,1)+a(i,2)+a(i,3)+a(i,4).eq.0.0)a(i,i)=1.0
         enddo
!########perform lu-decomposition
         call imod_utl_ludecomp(a,indx,4,ising)
!########compute inverse of A -> B
         do i=1,4
          do j=1,4
           b(j,i)=0.0
          end do
         end do

         do i=1,4
          b(i,i)=1.0
         enddo
         do i=1,4
          call imod_utl_lubacksub(a,indx,b(1,i),4)
         enddo

!########reshufle inverse-matrix
         do i=1,4
          do j=1,4
           a(j,i)=0.0
          end do
         end do

         do i=1,4
          a(i,1)=-b(i,1)-b(i,4)
          a(i,2)= b(i,1)-b(i,2)
          a(i,3)= b(i,2)+b(i,3)
          a(i,4)=-b(i,3)+b(i,4)
         enddo

!########store coeffcients in vector c
         ii=((irow-1)*ncol+icol-1)*16
         do i=1,4
          do j=1,4
           ii=ii+1
           c(ii)=a(i,j)
          enddo
         enddo

        enddo
       enddo

!######compute cr - conductance in horizontal direction
       do irow=1,nrow
        do icol=1,ncol-1
         if(irow.gt.1)then
          ii=((irow-1)*ncol+icol-1)*16
          i=ii+2
          dcr(icol,irow,ilay)=dcr(icol,irow,ilay)+c(i)    ! q_1 (right - main term)
          i=ii+14
          dcr(icol,irow,ilay)=dcr(icol,irow,ilay)+c(i)    ! q_4 (above - diagonal term)
         endif
         if(irow.lt.nrow)then
          ii=((irow)*ncol+icol-1)*16
          i=ii+11
          dcr(icol,irow,ilay)=dcr(icol,irow,ilay)+c(i)    ! q_3 (right - main term)
          i=ii+15
          dcr(icol,irow,ilay)=dcr(icol,irow,ilay)-c(i)    ! q_4 (below - diagonal term)
         endif

        enddo
       enddo

!######compute cc - conductance in vertical direction
       do icol=1,ncol
        do irow=1,nrow-1

         if(icol.gt.1)then
          ii=((irow)*ncol+icol-2)*16
          i=ii+6
          dcc(icol,irow,ilay)=dcc(icol,irow,ilay)-c(i)    ! q_3 (left - main term)
          i=ii+10
          dcc(icol,irow,ilay)=dcc(icol,irow,ilay)-c(i)    ! q_2 (below - diagonal term)
         endif
         ii=((irow)*ncol+icol-1)*16
         i=ii+13
         dcc(icol,irow,ilay)=dcc(icol,irow,ilay)-c(i)     ! q_3 (right - main term)
         i=ii+9
         dcc(icol,irow,ilay)=dcc(icol,irow,ilay)+c(i)     ! q_4 (below - diagonal term)

        enddo
       enddo

!######compute cu - conductance in upper-right diagonal direction
       do icol=1,ncol
        do irow=2,nrow

         ii=((irow-1)*ncol+icol-1)*16
         i=ii+3
         dcu(icol,irow,ilay)=dcu(icol,irow,ilay)+c(i)    ! q_1 (right/up right - diagonal term)
         i=ii+15
         dcu(icol,irow,ilay)=dcu(icol,irow,ilay)+c(i)    ! q_1 (right/up up - diagonal term)

        enddo
       enddo

!######compute cd - conductance in lower-right diagonal direction
       do icol=1,ncol
        do irow=1,nrow-1

         ii=((irow)*ncol+icol-1)*16
         i=ii+10
         dcd(icol,irow,ilay)=dcd(icol,irow,ilay)+c(i)    ! q_1 (right/down right - diagonal term)
         i=ii+14
         dcd(icol,irow,ilay)=dcd(icol,irow,ilay)-c(i)    ! q_1 (right/down down - diagonal term)

        enddo
       enddo

!######correct boundary-edges - only if tensor available
       do icol=1,ncol-1
        dcr(icol,1,ilay)   =dcr(icol,1,ilay)*2.0
        dcr(icol,nrow,ilay)=dcr(icol,nrow,ilay)*2.0
       enddo

!######correct boundary-edges - only if tensor available
       do irow=1,nrow-1
        dcc(1,irow,ilay)   =dcc(1,irow,ilay)*2.0
        dcc(ncol,irow,ilay)=dcc(ncol,irow,ilay)*2.0
       enddo

      enddo

      do ilay=1,nlay
       do irow=1,nrow
        do icol=1,ncol
         !## remove anisotropy in case kxx.eq.0.0
         if(ibound(icol,irow,ilay).eq.0.or.
     1     kxx(icol,irow,ilay).eq.0.0)then
          dcc(icol,irow,ilay)=0.0
          dcr(icol,irow,ilay)=0.0
          dcd(icol,irow,ilay)=0.0
          dcu(icol,irow,ilay)=0.0
         else
          if(icol.lt.ncol)then
           if(ibound(icol+1,irow,ilay).eq.0.or.
     1      kxx(icol+1,irow,ilay).eq.0.0)dcr(icol,irow,ilay)=0.0
           if(irow.lt.nrow)then
            if(ibound(icol+1,irow+1,ilay).eq.0.or.
     1      kxx(icol+1,irow+1,ilay).eq.0.0)dcd(icol,irow,ilay)=0.0
           endif
           if(irow.gt.1)then
            if(ibound(icol+1,irow-1,ilay).eq.0.or.
     1       kxx(icol+1,irow-1,ilay).eq.0.0)dcu(icol,irow,ilay)=0.0
           endif
          endif
          if(irow.lt.nrow)then
           if(ibound(icol,irow+1,ilay).eq.0.or.
     1      kxx(icol,irow+1,ilay).eq.0.0)dcc(icol,irow,ilay)=0.0
          endif
         endif
        end do
       end do
      end do

      !## array-cleaning
      do ilay=1,nlay
       do irow=1,nrow
        do icol=1,ncol
         ic2=min(icol+1,ncol); ir2=min(irow+1,nrow)
         ic1=max(icol-1,1);    ir1=max(irow-1,1)       
         if(ibound(icol,irow,ilay).lt.0)then
          if(ibound(ic2,irow,ilay).lt.0)dcr(icol,irow,ilay)=0.0
          if(ibound(icol,ir2,ilay).lt.0)dcc(icol,irow,ilay)=0.0
          if(ibound(ic2,ir2,ilay).lt.0) dcd(icol,irow,ilay)=0.0
          if(irow.gt.1)then
           if(ibound(ic2,ir1,ilay).lt.0)dcu(icol,irow,ilay)=0.0
          endif
         endif
        enddo
       enddo
      enddo

      !## array-cleaning
      do ilay=1,nlay
       do irow=1,nrow; dcr(ncol,irow,ilay)=0.0; enddo
       do icol=1,ncol; dcc(icol,nrow,ilay)=0.0; enddo
       do icol=1,ncol; dcd(icol,nrow,ilay)=0.0; enddo
       do icol=1,ncol; dcu(icol,1,ilay)   =0.0; enddo
      enddo

      return
      end
      

      
!###====================================================================
      subroutine scl1fmt(dcu,dcd,dcc,dcr,diag,rhs,ibound,hnew,ncol,nrow,
     1                   nlay,hcof,anifactor)
!###====================================================================
      implicit none
      integer ncol,nrow,nlay
      integer ibound(ncol,nrow,nlay)
      real dcu(ncol,nrow,nlay),dcd(ncol,nrow,nlay),
     1     dcc(ncol,nrow,nlay),dcr(ncol,nrow,nlay),
     1     diag(ncol,nrow,nlay)
      real(kind=8) hnew(ncol,nrow,nlay)
      real             rhs(ncol,nrow,nlay)
      real hcof(ncol,nrow,nlay)
      real anifactor(ncol,nrow,nlay)
      integer    ilay, irow,icol,ic1,ic2,ir1,ir2
      real     q,fod

      do ilay=1,nlay

!######compute diagonal right-down-flow
       do irow=1,nrow
        do icol=1,ncol

         ic2=min(icol+1,ncol); ir2=min(irow+1,nrow)
         ic1=max(icol-1,1);    ir1=max(irow-1,1)       
         if ((anifactor(icol,irow,ilay).lt.1.).or. !P
     1       (anifactor(icol,ir1,ilay).lt.1.).or. !N
     2       (anifactor(icol,ir2,ilay).lt.1.).or. !S
     3       (anifactor(ic2,irow,ilay).lt.1.).or. !E
     4       (anifactor(ic1,irow,ilay).lt.1.)) then !W
         
         if(ibound(icol,irow,ilay).ne.0)then

          ic2=icol+1; ir2=irow+1; ic1=icol-1; ir1=irow-1

          !## connection towards x-direction
          fod=0.0
          if(icol.lt.ncol)then
           if(ibound(ic2,irow,ilay).ne.0)then
            fod=fod-dcr(icol,irow,ilay)*hnew(ic2,irow,ilay)
            rhs(ic2,irow,ilay)=rhs(ic2,irow,ilay)-
     1       dcr(icol,irow,ilay)*hnew(icol,irow,ilay)
           endif
           if(irow.lt.nrow)then
            if(ibound(ic2,ir2,ilay).ne.0)then
             fod=fod-dcd(icol,irow,ilay)*hnew(ic2,ir2,ilay)
             rhs(ic2,ir2,ilay)=rhs(ic2,ir2,ilay)-
     1        dcd(icol,irow,ilay)*hnew(icol,irow,ilay)
            endif
           endif
           if(irow.gt.1)then
            if(ibound(ic2,ir1,ilay).ne.0)then
             fod=fod-dcu(icol,irow,ilay)*hnew(ic2,ir1,ilay)
             rhs(ic2,ir1,ilay)=rhs(ic2,ir1,ilay)-
     1        dcu(icol,irow,ilay)*hnew(icol,irow,ilay)
            endif
           endif
          endif
          !## connection towards y-direction
          if(irow.lt.nrow)then
           if(ibound(icol,ir2,ilay).ne.0)then
            fod=fod-dcc(icol,irow,ilay)*hnew(icol,ir2,ilay)
            rhs(icol,ir2,ilay)=rhs(icol,ir2,ilay)-
     1       dcc(icol,irow,ilay)*hnew(icol,irow,ilay)
           endif
          endif
!
!!#########get final flux for tensor-computation purposes
          q=fod-diag(icol,irow,ilay)*hnew(icol,irow,ilay)
          rhs(icol,irow,ilay)=rhs(icol,irow,ilay)+q
         endif
         
         end if
         
        enddo
       enddo
      enddo

      return
      end

!###====================================================================
      subroutine vdf1fmt(dcu,dcd,dcc,dcr,diag,rhs,ibound,hnew,ncol,nrow,
     1                   nlay,hcof,anifactor)
!###====================================================================
      USE VDFMODULE,    ONLY:DENSEREF,PS
      implicit none
      integer ncol,nrow,nlay
      integer ibound(ncol,nrow,nlay)
      real dcu(ncol,nrow,nlay),dcd(ncol,nrow,nlay),
     1     dcc(ncol,nrow,nlay),dcr(ncol,nrow,nlay),
     1     diag(ncol,nrow,nlay)
      real(kind=8) hnew(ncol,nrow,nlay)
      real             rhs(ncol,nrow,nlay)
      real hcof(ncol,nrow,nlay)
      real anifactor(ncol,nrow,nlay)
      integer    ilay, irow,icol,ic1,ic2,ir1,ir2
      real     q,fod

      do ilay=1,nlay

!######compute diagonal right-down-flow
       do irow=1,nrow
        do icol=1,ncol

         ic2=min(icol+1,ncol); ir2=min(irow+1,nrow)
         ic1=max(icol-1,1);    ir1=max(irow-1,1)       
         if ((anifactor(icol,irow,ilay).lt.1.).or. !P
     1       (anifactor(icol,ir1,ilay).lt.1.).or. !N
     2       (anifactor(icol,ir2,ilay).lt.1.).or. !S
     3       (anifactor(ic2,irow,ilay).lt.1.).or. !E
     4       (anifactor(ic1,irow,ilay).lt.1.)) then !W
         
         if(ibound(icol,irow,ilay).ne.0)then

          ic2=icol+1; ir2=irow+1; ic1=icol-1; ir1=irow-1

          !## connection towards x-direction
          fod=0.0
          if(icol.lt.ncol)then
           if(ibound(ic2,irow,ilay).ne.0)then
            fod=fod-dcr(icol,irow,ilay)*hnew(ic2,irow,ilay)
            rhs(ic2,irow,ilay)=rhs(ic2,irow,ilay)-
     1       dcr(icol,irow,ilay)*hnew(icol,irow,ilay)*denseref
           endif
           if(irow.lt.nrow)then
            if(ibound(ic2,ir2,ilay).ne.0)then
             fod=fod-dcd(icol,irow,ilay)*hnew(ic2,ir2,ilay)
             rhs(ic2,ir2,ilay)=rhs(ic2,ir2,ilay)-
     1        dcd(icol,irow,ilay)*hnew(icol,irow,ilay)*denseref
            endif
           endif
           if(irow.gt.1)then
            if(ibound(ic2,ir1,ilay).ne.0)then
             fod=fod-dcu(icol,irow,ilay)*hnew(ic2,ir1,ilay)
             rhs(ic2,ir1,ilay)=rhs(ic2,ir1,ilay)-
     1        dcu(icol,irow,ilay)*hnew(icol,irow,ilay)*denseref
            endif
           endif
          endif
          !## connection towards y-direction
          if(irow.lt.nrow)then
           if(ibound(icol,ir2,ilay).ne.0)then
            fod=fod-dcc(icol,irow,ilay)*hnew(icol,ir2,ilay)
            rhs(icol,ir2,ilay)=rhs(icol,ir2,ilay)-
     1       dcc(icol,irow,ilay)*hnew(icol,irow,ilay)*denseref
           endif
          endif
!
!!#########get final flux for tensor-computation purposes
          q=fod-diag(icol,irow,ilay)*hnew(icol,irow,ilay)
          rhs(icol,irow,ilay)=rhs(icol,irow,ilay)+q*denseref
         endif
         
         end if
         
        enddo
       enddo
      enddo

      return
      end

!###====================================================================
      subroutine ani5fm(x,ncol,nrow,nlay,dcu,dcd,dcc,dcr,diag,
     1                 ibound,hnew,i)
!###====================================================================
      implicit none

      integer, intent(in) :: ncol,nrow,nlay
      real, dimension(ncol,nrow,nlay), intent(out) :: x
      real, dimension(ncol,nrow,nlay), intent(in) :: dcu
      real, dimension(ncol,nrow,nlay), intent(in) :: dcc
      real, dimension(ncol,nrow,nlay), intent(in) :: dcd
      real, dimension(ncol,nrow,nlay), intent(in) :: dcr
      real, dimension(ncol,nrow,nlay), intent(in) :: diag
      integer, dimension(ncol,nrow,nlay), intent(in) :: ibound
      double precision, dimension(ncol,nrow,nlay), intent(in) :: hnew
      integer, intent(in) :: i

      real,parameter :: ccrit=10.0e-03
      integer:: ilay,irow,icol
      real :: sumc

      x = 0.

      do ilay=1,nlay
       do irow=1,nrow
        do icol=1,ncol
         if(ibound(icol,irow,ilay).ne.0)then
          sumc=abs(dcu(icol,irow,ilay))+
     1         abs(dcd(icol,irow,ilay))+
     1         abs(dcc(icol,irow,ilay))+
     1         abs(dcr(icol,irow,ilay))+
     1         abs(diag(icol,irow,ilay))
          if(sumc.gt.ccrit)then
            !## connection towards right direction
            if(icol.lt.ncol)then
             if(ibound(icol+1,irow,ilay).ne.0)then
             if(i.eq.4)then
              x(icol,irow,ilay)=x(icol,irow,ilay)+
     1        dcr(icol,irow,ilay)*
     1         (hnew(icol+1,irow,ilay)-hnew(icol,irow,ilay))
             end if
            end if
            if(irow.lt.nrow)then
             !## diagonal
             if(ibound(icol+1,irow+1,ilay).ne.0)then
              if(i.eq.2)then
               x(icol,irow,ilay)=x(icol,irow,ilay)+dcd(icol,irow,ilay)*
     1          (hnew(icol+1,irow+1,ilay)-hnew(icol,irow,ilay))
              endif
             endif
            end if
            if(irow.gt.1)then
             if(ibound(icol+1,irow-1,ilay).ne.0)then
              if(i.eq.1)then
               x(icol,irow,ilay)=x(icol,irow,ilay)+dcu(icol,irow,ilay)*
     1          (hnew(icol+1,irow-1,ilay)-hnew(icol,irow,ilay))
              endif
             endif
            endif
           endif
           !## connection towards bottom direction
           if(irow.lt.nrow)then
            if(ibound(icol,irow+1,ilay).ne.0)then
             if(i.eq.3)then
              x(icol,irow,ilay)=x(icol,irow,ilay)+
     1         dcc(icol,irow,ilay)*
     1         (hnew(icol,irow+1,ilay)-hnew(icol,irow,ilay))
             endif
            endif
           endif
          endif
         endif
        end do
       end do
      end do

      return
      end subroutine

      !###====================================================================
      subroutine ani7fm(buff,dfu,dfd,dff,dfr,ncol,nrow,nlay,idir,
     1anifactor)
      !###====================================================================
      implicit none
      integer,intent(in) :: ncol,nrow,nlay,idir
      real(kind=8),dimension(ncol,nrow,nlay),intent(inout) :: buff
      real,dimension(ncol,nrow,nlay),intent(in) :: dfu,dfd,dff,dfr,
     1anifactor
      integer :: irow,icol,ilay,ic1,ic2,ir1,ir2
      real :: q1,q2,q3,q4,q5

      do ilay=1,nlay
       do irow=1,nrow
        do icol=1,ncol

         ic2=min(icol+1,ncol); ir2=min(irow+1,nrow)
         ic1=max(icol-1,1);    ir1=max(irow-1,1)       
         if ((anifactor(icol,irow,ilay).lt.1.).or. !P
     1       (anifactor(icol,ir1,ilay).lt.1.).or. !N
     2       (anifactor(icol,ir2,ilay).lt.1.).or. !S
     3       (anifactor(ic2,irow,ilay).lt.1.).or. !E
     4       (anifactor(ic1,irow,ilay).lt.1.)) then !W

          if(idir.eq.1)then
           q1=0.5*dfu(icol,irow,ilay)
           q2=0.0; if(irow.gt.1)q2=0.5*dfd(icol,irow-1,ilay)
           q3=dfr(icol,irow,ilay)
           q4=0.0; if(irow.lt.nrow)q4=0.5*dfu(icol,irow+1,ilay)
           q5=0.5*dfd(icol,irow,ilay)
           buff(icol,irow,ilay)=buff(icol,irow,ilay)-(q1+q2+q3+q4+q5)
          endif

          if(idir.eq.2)then
           q1=0.0
           if(icol.gt.1.and.irow.lt.nrow)q1=-0.5*dfu(icol-1,irow+1,ilay)
           q2=0.0
           if(icol.gt.1)q2=0.5*dfd(icol-1,irow,ilay)
           q3=dff(icol,irow,ilay)
           q4=0.5*dfd(icol,irow,ilay)
           q5=0.0
           if(irow.lt.nrow)q5=-0.5*dfu(icol,irow+1,ilay)
           buff(icol,irow,ilay)=buff(icol,irow,ilay)-(q1+q2+q3+q4+q5)
          endif
         
         endif
         
        enddo
       enddo
      enddo

      return
      end subroutine

      subroutine translatekxx(ncol,nrow,nlay,ibound,kd,fct,angle,
     %    kxx,kyy,kxy)

      implicit none
      integer ncol,nrow,nlay,ilay,irow,icol
      integer ibound(ncol,nrow,nlay)
      real,intent(in) :: kd(ncol,nrow,nlay),fct(ncol,nrow,nlay), 
     & angle(ncol,nrow,nlay)
      real  k1,k2,kxx(ncol,nrow,nlay),kyy(ncol,nrow,nlay),
     %      kxy(ncol,nrow,nlay),phi

      do ilay=1,nlay
       do irow=1,nrow
        do icol=1,ncol
         !## set dummy values for inactive cells - isotropic
         if(ibound(icol,irow,ilay).eq.0)then
          kxx(icol,irow,ilay)=1.0
          kyy(icol,irow,ilay)=1.0
          kxy(icol,irow,ilay)=0.0
         else
          !## overrule angle in case factor is 1.0 - irrelevant in that case
          if(fct(icol,irow,ilay).eq.1.0)then !eq.1.0)then
!           angle(icol,irow,ilay)=0.0
           kxx(icol,irow,ilay)=kd(icol,irow,ilay) !0.0
           kyy(icol,irow,ilay)=kd(icol,irow,ilay) !0.0
           kxy(icol,irow,ilay)=0.0          
          else
           phi=((360.-angle(icol,irow,ilay))*2.0*3.14159)/360.0
           k1 =kd(icol,irow,ilay)*fct(icol,irow,ilay)
           k2 =kd(icol,irow,ilay)
           kxx(icol,irow,ilay)=k1*cos(phi)**2.0+k2*sin(phi)**2.0
           kxy(icol,irow,ilay)=(k1-k2)*cos(phi)*sin(phi)
           kyy(icol,irow,ilay)=k1*sin(phi)**2.0+k2*cos(phi)**2.0
         endif
        endif
        end do
       end do 
      end do

      return
      end

!###====================================================================
      subroutine scl1fmd(cc,cr,cv,ibound,dcu,dcd,dcc,dcr,hcof,
     1           rhs,diag,ncol,nrow,nlay,anifactor)
!###====================================================================
      use gwfbasmodule,only:hnoflo
      use global,only:hnew
      implicit none
      integer ncol,nrow,nlay
      integer ibound(ncol,nrow,nlay)
      real dcu(ncol,nrow,nlay),dcd(ncol,nrow,nlay),
     %    cc(ncol,nrow,nlay),cr(ncol,nrow,nlay),cv(ncol,nrow,nlay)
      real dcc(ncol,nrow,nlay),dcr(ncol,nrow,nlay)
      real hcof(ncol,nrow,nlay),diag(ncol,nrow,nlay)
c      double precision rhs(ncol,nrow,nlay)
      real rhs(ncol,nrow,nlay)
      real anifactor(ncol,nrow,nlay)
      integer ilay,irow,icol
      real      ccrit,sumc
      parameter (ccrit=10.0e-3)
      integer :: ic1,ic2,ir1,ir2,il1,il2
      integer, dimension(:,:,:), allocatable :: iwrk

      do ilay=1,nlay
       do irow=1,nrow
        do icol=1,ncol
         rhs(icol,irow,ilay) =0.0
         hcof(icol,irow,ilay)=0.0
         diag(icol,irow,ilay)=0.0
        end do
       end do
      end do

!#####cleaning cc/cr/cv for anisotropical issues
      do ilay=1,nlay
       do irow=1,nrow
        do icol=1,ncol
         ic2=min(icol+1,ncol); ir2=min(irow+1,nrow)
         ic1=max(icol-1,1);    ir1=max(irow-1,1)       
         if(ibound(icol,irow,ilay).eq.0)then
          cr(icol,irow,ilay)=0.0
          cc(icol,irow,ilay)=0.0
          if(ilay.lt.nlay)cv(icol,irow,ilay)=0.0
         else
          if(icol.lt.ncol)then
           if(ibound(ic2,irow,ilay).eq.0)cr(icol,irow,ilay)=0.0
          endif
          if(irow.lt.nrow)then
           if(ibound(icol,ir2,ilay).eq.0)cc(icol,irow,ilay)=0.0
          endif
          if(ilay.lt.nlay)then
           if(ibound(icol,irow,ilay+1).eq.0)cv(icol,irow,ilay)=0.0
          endif
         endif
         if(ibound(icol,irow,ilay).lt.0)then
          if(icol.lt.ncol)then
           if(ibound(ic2,irow,ilay).lt.0)cr(icol,irow,ilay)=0.0
          endif
          if(irow.lt.nrow)then
           if(ibound(icol,ir2,ilay).lt.0)cc(icol,irow,ilay)=0.0
          endif
          if(ilay.lt.nlay)then
           if(ibound(icol,irow,ilay+1).lt.0)cv(icol,irow,ilay)=0.0
          endif
         endif
        enddo
       enddo
      enddo
      
!#####cleaning for constant head cells that are only connected to other constant head/inactive cells    
      allocate(iwrk(ncol,nrow,nlay))
      iwrk = ibound
      do ilay=1,nlay
       do irow=1,nrow
        do icol=1,ncol
         ic1=max(icol-1,1); ic2=min(icol+1,ncol)
         ir1=max(irow-1,1); ir2=min(irow+1,nrow)
         il1=max(ilay-1,1); il2=min(ilay+1,nlay)
         if(iwrk(icol,irow,ilay).lt.0)then         
          if((iwrk(icol,ir1,ilay).le.0).and. !N
     1       (iwrk(icol,ir2,ilay).le.0).and. !S
     1       (iwrk(ic1,irow,ilay).le.0).and. !W
     1       (iwrk(ic2,irow,ilay).le.0).and. !E
     1       (iwrk(icol,irow,il1).le.0).and. !T
     1       (iwrk(icol,irow,il2).le.0))then !B
            ibound(icol,irow,ilay) = 0
            hnew(icol,irow,ilay) = hnoflo
           end if
         end if    
        end do    
       end do    
      enddo
      deallocate(iwrk)
      
!#####construct main diagonal 5 and 9-point stencil
      do ilay=1,nlay
       do irow=1,nrow
        do icol=1,ncol
         if(ibound(icol,irow,ilay).ne.0)then
         ic2=min(icol+1,ncol); ir2=min(irow+1,nrow)
         ic1=max(icol-1,1);    ir1=max(irow-1,1)       
         if ((anifactor(icol,irow,ilay).lt.1.).or. !P
     1       (anifactor(icol,ir1,ilay).lt.1.).or. !N
     2       (anifactor(icol,ir2,ilay).lt.1.).or. !S
     3       (anifactor(ic2,irow,ilay).lt.1.).or. !E
     4       (anifactor(ic1,irow,ilay).lt.1.)) then !W

!#########5-point stencil

!#########right
          if(icol.lt.ncol) then
            if(ibound(icol+1,irow,ilay).ne.0)
     1     rhs(icol,irow,ilay)=rhs(icol,irow,ilay)-cr(icol,irow,ilay)
          endif
!#########left
          if(icol.gt.1) then
            if(ibound(icol-1,irow,ilay).ne.0)
     1     rhs(icol,irow,ilay)=rhs(icol,irow,ilay)-cr(icol-1,irow,ilay)
          endif
!#########back
          if(irow.lt.nrow) then
            if(ibound(icol,irow+1,ilay).ne.0)
     1     rhs(icol,irow,ilay)=rhs(icol,irow,ilay)-cc(icol,irow,ilay)
          endif
!#########front
          if(irow.gt.1) then
            if(ibound(icol,irow-1,ilay).ne.0)
     1     rhs(icol,irow,ilay)=rhs(icol,irow,ilay)-cc(icol,irow-1,ilay)
          endif
!#########top
          if(ilay.lt.nlay) then
            if(ibound(icol,irow,ilay+1).ne.0)
     1     rhs(icol,irow,ilay)=rhs(icol,irow,ilay)-cv(icol,irow,ilay)
          endif
!#########bottom
          if(ilay.gt.1) then
            if (ibound(icol,irow,ilay-1).ne.0)
     1     rhs(icol,irow,ilay)=rhs(icol,irow,ilay)-cv(icol,irow,ilay-1)
          endif

!#########9-point stencil

!#########right
          if(icol.lt.ncol) then
            if(ibound(icol+1,irow,ilay).ne.0)
     1     hcof(icol,irow,ilay)=hcof(icol,irow,ilay)-dcr(icol,irow,ilay)
          endif
!#########left
          if(icol.gt.1) then
            if(ibound(icol-1,irow,ilay).ne.0)
     1   hcof(icol,irow,ilay)=hcof(icol,irow,ilay)-dcr(icol-1,irow,ilay)
          endif
!#########back
          if(irow.lt.nrow) then
            if(ibound(icol,irow+1,ilay).ne.0)
     1    hcof(icol,irow,ilay)=hcof(icol,irow,ilay)-dcc(icol,irow,ilay)
          endif
!#########front
          if(irow.gt.1) then
            if(ibound(icol,irow-1,ilay).ne.0)
     1   hcof(icol,irow,ilay)=hcof(icol,irow,ilay)-dcc(icol,irow-1,ilay)
          endif
!#########top
          if(ilay.lt.nlay) then
            if(ibound(icol,irow,ilay+1).ne.0)
     1    hcof(icol,irow,ilay)=hcof(icol,irow,ilay)-cv(icol,irow,ilay)
          endif
!#########bottom
          if(ilay.gt.1) then
            if(ibound(icol,irow,ilay-1).ne.0)
     1    hcof(icol,irow,ilay)=hcof(icol,irow,ilay)-cv(icol,irow,ilay-1)
          endif

!#########diag. right upper
          if(icol.lt.ncol.and.irow.gt.1)then
           if(ibound(icol+1,irow-1,ilay).ne.0)hcof(icol,irow,ilay)=
     1        hcof(icol,irow,ilay)-dcu(icol,irow,ilay)
          endif
!#########diag. right down
          if(icol.lt.ncol.and.irow.lt.nrow)then
           if(ibound(icol+1,irow+1,ilay).ne.0)hcof(icol,irow,ilay)=
     1        hcof(icol,irow,ilay)-dcd(icol,irow,ilay)
          endif
!#########diag. left upper
          if(icol.gt.1.and.irow.gt.1)then
           if(ibound(icol-1,irow-1,ilay).ne.0)hcof(icol,irow,ilay)=
     1        hcof(icol,irow,ilay)-dcd(icol-1,irow-1,ilay)
          endif
!#########diag. left down
          if(icol.gt.1.and.irow.lt.nrow)then
           if(ibound(icol-1,irow+1,ilay).ne.0)hcof(icol,irow,ilay)=
     1        hcof(icol,irow,ilay)-dcu(icol-1,irow+1,ilay)
          endif

         endif
         endif
        end do
       end do
      end do

      do ilay=1,nlay
       do irow=1,nrow
        do icol=1,ncol
         if(ibound(icol,irow,ilay).ne.0)then
          ic2=min(icol+1,ncol); ir2=min(irow+1,nrow)
          ic1=max(icol-1,1);    ir1=max(irow-1,1)       
          if ((anifactor(icol,irow,ilay).lt.1.).or. !P
     1        (anifactor(icol,ir1,ilay).lt.1.).or. !N
     2        (anifactor(icol,ir2,ilay).lt.1.).or. !S
     3        (anifactor(ic2,irow,ilay).lt.1.).or. !E
     4        (anifactor(ic1,irow,ilay).lt.1.)) then !W
           diag(icol,irow,ilay)=hcof(icol,irow,ilay)-rhs(icol,irow,ilay)
          endif
         endif
        end do
       end do
      end do

!#####construct correction matrix a(9-5)=a(9)-a(5) for dcc,dcc and the main diagonal diag
      do ilay=1,nlay
       do irow=1,nrow
        do icol=1,ncol
         if(ibound(icol,irow,ilay).ne.0)then
          ic2=min(icol+1,ncol); ir2=min(irow+1,nrow)
          ic1=max(icol-1,1);    ir1=max(irow-1,1)       
          if ((anifactor(icol,irow,ilay).lt.1.).or. !P
     1        (anifactor(icol,ir1,ilay).lt.1.).or. !N
     2        (anifactor(icol,ir2,ilay).lt.1.).or. !S
     3        (anifactor(ic2,irow,ilay).lt.1.).or. !E
     4        (anifactor(ic1,irow,ilay).lt.1.)) then !W

           if(irow.lt.nrow)then
            if(ibound(icol,irow+1,ilay).ne.0)then
             dcc(icol,irow,ilay) =dcc(icol,irow,ilay)-cc(icol,irow,ilay)
            endif 
           endif
           if(icol.lt.ncol)then
            if(ibound(icol+1,irow,ilay).ne.0)then
             dcr(icol,irow,ilay) =dcr(icol,irow,ilay)-cr(icol,irow,ilay)
            endif
           endif
          else
           dcc(icol,irow,ilay) =0.0
           dcr(icol,irow,ilay) =0.0
          endif
         endif
        end do
       end do
      end do
      
      return
      end
