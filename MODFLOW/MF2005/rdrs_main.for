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

      function rdrs_main(file,array,larray,ndimi,type,iout,
     1                        iupsc,idosc,ilay)

c description:
c ------------------------------------------------------------------------------
c MODFLOW-2005 wrapper around rdrs_data to read data from a file.
c

c declaration section
c ------------------------------------------------------------------------------
      use rdrsmodule
      use global, only: ncol, nrow
      use imod_utl, only: imod_utl_has_ext

      implicit none

c function declaration
      integer   rdrs_main    ! return value: 0: OK
                                  !             <>0: ERROR


c arguments
c      file  : I: file naam
c      array : O: data read
c      larray: I: maximum number of elements in array
c      ndimi : I: maximum number of dimensions
c              O: number of dimension of data read
c      nodata: I: no data value
c      type  : I: data type as expected for array: 'R'eal 'I'nteger
c                 If necessary, conversion will be applied

      integer :: larray          ! (I)
      integer :: ndimi           ! (I)
      integer :: iout            ! (I)

      integer :: array(larray)   ! (O)

      character :: file*(*)        ! (I)
      character :: type*(*)        ! (I)
      
      integer,intent(in) :: ilay
      
      integer :: iupsc ! (I)
      integer :: idosc ! (I)

c local variables
      integer, parameter :: iipf = 1
      integer, parameter :: iidf = 2
      integer, parameter :: igen = 3

      integer :: ndim, dims(ndimi), ios, filetype

c functions
      integer :: cfn_length, rdrs_rddata_ipf,
     1           imod_idfread, rdrs_rddata_gen


c include files


c program section
c ------------------------------------------------------------------------------
c set nodata
      ndim = ndimi

c check for ipf (u2drel only)
      if (imod_utl_has_ext(file,'ipf')) then
         filetype = iipf
         ios = rdrs_rddata_ipf(file,iout,ilay)
      else if (imod_utl_has_ext(file,'idf')) then
         filetype = iidf
         if (type(1:1).eq.'i') then
            if (.not.allocated(rwrk)) then
               allocate(rwrk(ncol,nrow))
            else
                if (size(rwrk,1).lt.ncol .or.
     1              size(rwrk,2).lt.nrow) then
                   deallocate(rwrk)
                   allocate(rwrk(ncol,nrow))
                end if
            end if
            ios = imod_idfread(file,array,rwrk,nodata,type(1:1),
     1            iupsc,idosc)
         else
            if (.not.allocated(iwrk)) then
               allocate(iwrk(ncol,nrow))
            else
                if (size(iwrk,1).lt.ncol .or.
     1             size(iwrk,2).lt.nrow) then
                   deallocate(iwrk)
                   allocate(iwrk(ncol,nrow))
                end if
            end if
            ios = imod_idfread(file,iwrk,array,nodata,type(1:1),
     1            iupsc,idosc)
         end if
      else if (imod_utl_has_ext(file,'gen')) then
         filetype = igen
         ios = rdrs_rddata_gen(file,iout,ilay)
      else
         rdrs_main = -1
         return
      end if

      if (ios.ne.0) call ustop(' Stopped in routine rdrs_main')

c assign result flag
      rdrs_main = ios

c echo file type to list file
      select case(filetype)
         case(iidf)
            write(iout,*) ' FILE IDENTIFIED AS IDF'
         case(iipf)
            write(iout,*) ' FILE IDENTIFIED AS IPF'
         case(igen)
            write(iout,*) ' FILE IDENTIFIED AS GEN'
      end select

c end of program
      return
      end

      function rdrs_rddata_ipf(file,iout,ilay)

c description:
c ------------------------------------------------------------------------------
c Read IPF file.
c

c declaration section
c ------------------------------------------------------------------------------
      use global, only: ncol, nrow, delr, delc, iunit, issflg
      use m_mf2005_main, only: kper
      use gwfbasmodule, only: delt
      use gcdmodule
      use gwfmetmodule, only: coord_xll, coord_yll, time_ostring, ieq,
     1                        cdelr, cdelc, iss
      use imod_utl
      use m_mf2005_iu, only: iumet

      implicit none

c function declaration
      integer   rdrs_rddata_ipf   ! return value: ios
      
c arguments
      character file*(*)      ! (I)
      integer   iout          ! (O)
      integer,intent(in) :: ilay
      
c local variables
      character(len=8) :: cdate
      character(len=300) :: line, txtfile, root, name
      integer :: i, ret, ios, lun, nlist, icol, irow, ii, nc, iext,
     1   sdate, edate, ttime, idate
      integer(kind=8) :: stime, etime
      real    :: x, y
      real :: q, tf, bf  !, dimension(1)
      logical :: found
      character(len=3) :: ext
      character(len=50), allocatable, dimension(:) :: string
!      real, dimension(:), allocatable :: qsort

c functions
      integer   cfn_getlun,
     1          osd_open2,
     2          osd_fsplit

c include files

c program section
c ------------------------------------------------------------------------------
c
c check if MET package is activated
      if (IUNIT(IUMET).le.0) then
         rdrs_rddata_ipf = -20
         return
      else
         if (.not.associated(coord_xll) .or.
     1       .not.associated(coord_yll)) then
            rdrs_rddata_ipf = -10
            return
         end if
      end if

      ret = 0

c open file
      lun=cfn_getlun(10,99)
      if (lun.gt.0) then

         ios=osd_open2(lun,0,file,'old,formatted,readonly,shared')

         if (ios.ne.0) then
               ! ERROR opening file
               write(*,*) ' ERROR. opening file. ',
     1              'I/O status: ',ios
               ret = -11
         endif
      else
            ! ERROR no lun found
         write(*,*) ' ERROR. no free lun found for opening file.'
         ret = -12
      end if

      if (ret.ne.0) then
          rdrs_rddata_ipf = ret
          return
      end if

c read data
      read(lun,*) nlist
      read(lun,*) nc
      do i = 1, nc
         read(lun,'(a)') line
      end do
      read(lun,*) iext, ext

c overrule iext when current timestep is steady-state
!      if(iss.eq.1)iext=0
      if(delt.eq.0.0)iext=0

c determine root and date
      if (iext.ne.0) then
         call osd_s_filename(file)
         ios=osd_fsplit(file,root,name)
         if (ios.ne.0) then
            write(*,*) 'ERROR. File splitting ipf.'
            call ustop(' ')
         end if
         iss = 0
         if (issflg(kper).eq.1) then ! steady-state
            iss = 1
         else if (issflg(kper).eq.0) then ! transient
            iss = 2
         end if

c ### work-around Julian date ###
         read(time_ostring(1:8),*) idate
         sdate=imod_utl_idatetojdate(idate)
c ### work-around Julian date ###
c         sdate = time_cjd
         ttime = int(delt)
         if (iss==1) ttime = 1
         edate = sdate + ttime ! days only

         stime=sdate
         etime=edate
         IF(STIME.LT.100000000)STIME=STIME*1000000
         IF(ETIME.LT.100000000)ETIME=ETIME*1000000

       end if

c allocate
      if(allocated(string)) deallocate(string)
      allocate(string(nc))
      allocate(ipflist(isub,icolumn)%list(nc,nlist))
      ipflist(isub,icolumn)%nlist = nlist

      ii = 0
      do i = 1, nlist
         read(lun,'(a300)') line
         read(line,*,iostat=ios) (string(icol),icol=1,nc)
         if(ios.ne.0) then
            write(*,*) 'ERROR. Reading ipf '//trim(file)
            write(*,'(a)') trim(line)
            call ustop(' ')
         end if

         ! read x, y
         read(string(1),*) x
         read(string(2),*) y

         call imod_utl_geticir(x,y,icol,irow,ncol,nrow,cdelr,cdelc,
     1                         delr(1),ieq)
         found = .true.
         if (icol.lt.1.or.icol.gt.ncol) found = .false.
         if (irow.lt.1.or.irow.gt.nrow) found = .false.

         if (found) then
            if (ilay.eq.0)then !nc.ge.5) then
               read(string(4),*,iostat=ios) tf !(1)
               if(ios.ne.0) then
                write(*,*) 'ERROR. Reading ipf '//trim(file)
                write(*,'(a)') trim(string(4))
                call ustop(' ')
               end if
               read(string(5),*,iostat=ios) bf !(1)
               if(ios.ne.0) then
                write(*,*) 'ERROR. Reading ipf '//trim(file)
                write(*,'(a)') trim(string(5))
                call ustop(' ')
               end if
            end if
            if (iext.eq.0) then
               read(string(3),*) q !(1)
            else
               txtfile = trim(root)//trim(string(iext))//'.'//trim(ext)
               call imod_utl_readipf(stime,etime,q,txtfile,iss)
!               call imod_utl_readipf(sdate,edate,q,txtfile,iss)
            end if
       
            ii = ii + 1
            ipflist(isub,icolumn)%list(1,ii) = real(irow)
            ipflist(isub,icolumn)%list(2,ii) = real(icol)
            ipflist(isub,icolumn)%list(3,ii) = q !(1)
!            if (nc.ge.5) then
            if (ilay.eq.0)then 
               ipflist(isub,icolumn)%list(4,ii) = tf !(1)
               ipflist(isub,icolumn)%list(5,ii) = bf !(1)
            end if
         end if ! found
      end do
      ipflist(isub,icolumn)%nlist = ii

      if (nlist.gt.ipflist(isub,icolumn)%nlist) then
         write (*,*) 'Items skipped for ',trim(file),
     1    nlist-ipflist(isub,icolumn)%nlist
      end if

c close file
      close(lun)

c assign function value
      rdrs_rddata_ipf = ret

c deallocate
!      if(allocated(qsort))  deallocate(qsort)
      if(allocated(string)) deallocate(string)

c end of program
      return
      end

      function imod_idfread(file,iarray,rarray,nodata,type,
     1                      iupsc,idosc)
c description:
c ------------------------------------------------------------------------------
c read idf file and scale is necessary
c
c declaration section
c ------------------------------------------------------------------------------
c modules
      use idfmodule
      use global, only: delr, delc, ncol, nrow
      use gwfmetmodule
      use pksmpi_mod, only: nrproc, myrank
      
      implicit none

c arguments
      character(len=*), intent(in) :: file
      integer, dimension(ncol,nrow), intent(inout) :: iarray
      real, dimension(ncol,nrow), intent(inout)    :: rarray
      real, intent(in) :: nodata
      character(len=1) :: type
      integer, intent(in) :: iupsc
      integer, intent(in) :: idosc

c function declaration
      integer :: imod_idfread

c local variables
      logical :: lop
      integer :: icol, irow, ieqd, i, j
      real :: xmin, ymin, xmax, ymax, val
      character(len=1000) :: fname
      logical :: lnodata, lex
      character(len=10) :: partstr
 
c program section
c ------------------------------------------------------------------------------

c check if coordinates are specified
      if (.not.associated(coord_xll) .or.
     &    .not.associated(coord_yll) ) then
         write(*,*) 'Error: idf can only be used with xll and yll'
         imod_idfread = 1
         return
      end if

c read idf header
      if (.not.idfread(idfc,file,0)) then
c read the entire IDF
!      if (.not.idfread(idfc,file,1)) then
         if (idfc%iu.gt.0) close(idfc%iu)
         imod_idfread = 1
         return
      end if

c map on cell centroids
      call idfnullify(idfm)
      ieqd=0
      do i=2,ncol; if(delr(i).ne.delr(i-1))exit; enddo
      if(i.le.ncol)ieqd=1
      if(ieqd.eq.0)then
      idfm%dx=delr(1)
      idfm%dy=delc(1)
      idfm%ncol=ncol
      idfm%nrow=nrow
       idfm%ieq=int(0,1)
      else
       idfm%ncol=ncol; idfm%nrow=nrow; idfm%ieq=int(1,1)
       IF(.NOT.IDFALLOCATESXY(IDFM))
     1    stop 'cannot allocate memory for sx and sy vectors'
       idfm%sx(0)=coord_xll
       do i=1,ncol
        idfm%sx(i)=idfm%sx(i-1)+delr(i)
       enddo
       idfm%sy(0)=coord_yur
       do i=1,nrow
        idfm%sy(i)=idfm%sy(i-1)-delc(i)
       enddo
      endif
      idfm%xmin = coord_xll
      idfm%xmax = coord_xur
      idfm%ymin = coord_yll
      idfm%ymax = coord_yur
      idfm%nodata = nodata
      if (usexmask) then
        IF(.NOT.IDFALLOCATEX(IDFM))
     1   stop 'cannote allocate memory for x array' 
        !  !allocate(idfm%x(ncol,nrow))
         idfm%x = xmask
         do irow = 1, nrow
            do icol = 1, ncol
               if (idfm%x(icol,irow)==idfc%nodata) then
                  idfm%x(icol,irow)=nodata
               end if
            end do
         end do
      end if

c read idf and scale
      if (.not.idfreadscale(idfc,idfm,iupsc,idosc)) then
         if (idfc%iu.gt.0) close(idfc%iu)
         imod_idfread = 1
         return
      end if

c deallocate child idf and close
      call idfdeallocatex(idfc)
      if (idfc%iu.gt.0) then
         inquire(unit=idfc%iu,opened=lop); if(lop)close(idfc%iu)
      endif

c set array
      lnodata = .false.
      do irow = 1, nrow
         do icol = 1, ncol
            val = idfm%x(icol,irow)
c change data in case nodata value is used
c            if (val.eq.idfm%nodata) val = nodata
            if (val.eq.idfc%nodata) then
               val = nodata
               lnodata = .true.
            end if   
            if (type.eq.'i') then
               iarray(icol,irow) = int(val)
            else
               rarray(icol,irow) = val
            end if
         end do
      end do

c check for nodata
      if (nrproc.gt.1) then
         if (lnodata) then
c            write(*,*) 'Warning, nodata found in ',trim(file), myrank
         end if    
      end if   

c debug
      if(associated(write_debug_idf))then
         i = index(file,'/',back=.true.)
         if (i.le.0) then
           i = index(file,'\',back=.true.)
         end if
         i = max(i,1)
         j = index(file,'.',back=.true.)
         fname = trim(debugdir)//file(i:j-1)
         call pks7mpipartstr(partstr)
         fname = trim(fname)//trim(partstr)//'.idf'
         inquire(file=fname,exist=lex)
         if (lex) then
            write(*,'(1x,a,1x,a,1x,a)') 'Warning: ',trim(fname),
     1       ' already exists!'
         end if 
         write(*,'(1x,a,1x,a,1x,a)') 'Writing',trim(fname),'...'
         if (.not.idfwrite(idfm,fname,0)) then
            imod_idfread = 1
            return
         end if          
      end if
      
c deallocate mother idf
      call idfdeallocatex(idfm)

c assign function value
      imod_idfread = 0

c end of program
      return
      end

      function rdrs_rddata_gen(file,iout,ilay)
c description:
c ------------------------------------------------------------------------------
c Read GEN file.
c

c declaration section
c ------------------------------------------------------------------------------
      use lcdmodule
      use imod_utl, only: imod_utl_intersect_equi,
     1                    imod_utl_intersect_nonequi,
     1                    xa,ya,fa,ln,ca,ra,intersect_deallocate
      use gwfmetmodule, only: cdelr, cdelc
      implicit none

c function declaration
      integer   rdrs_rddata_gen  ! return value: ios

c arguments
      character(len=*) :: file
      integer, intent(in) :: iout,ilay

c local variables
      character(len=256) :: line
      logical :: ok
      integer :: ret, lun, ios, mx, id, nid,
     1           i, ii, n, nn, m, l, icol, irow, iact
      real    :: x1, y1, x2, y2, xx1, yy1, xx2, yy2, tl, zl, z1, z2, 
     1           dz !zz1, zz2


c functions
      integer   cfn_getlun,
     1          osd_open2

c include files


c program section
c ------------------------------------------------------------------------------
c
c check for lcd file
      ret = 0
      if (lcdinit) then
         ret = -20
         rdrs_rddata_gen = ret
         return
      end if

c open file
      lun=cfn_getlun(10,99)
      if (lun.gt.0) then

         ios=osd_open2(lun,0,file,'old,formatted,readonly,shared')

         if (ios.ne.0) then
               ! ERROR opening file
               write(*,*) ' ERROR. opening file. ',
     1              'I/O status: ',ios
               ret = -14
         endif
      else
            ! ERROR no lun found
         write(*,*) ' ERROR. no free lun found for opening file.'
         ret = -15
      end if

      if (ret.ne.0) then
          rdrs_rddata_gen = ret
          return
      end if

c init
      mx=lncol*lnrow
!      if(allocated(xa))deallocate(xa)
!      if(allocated(ya))deallocate(ya)
!      if(allocated(ln))deallocate(ln)
!      if(allocated(fa))deallocate(fa)
!      allocate(xa(mx),ya(mx),ln(mx),fa(mx))
      if (allocated(genpos))deallocate(genpos)
      if (allocated(genip))deallocate(genip)

c read file
      do iact = 1, 2
      m = 0; nid = 0
      do while(.true.)
         ii=0
         read(lun,'(a)') line
         call cfn_s_trim(line)
         call cfn_s_lowcase(line)
         ios=0
         if(index(line,'end').gt.0)exit
         read(line,*,iostat=ios) id
         if(ios.ne.0)exit
         nid = nid + 1
         if (iact.eq.2) genip(nid) = genip(nid-1)
         do while (.true.)
            read(lun,'(a)') line
            call cfn_s_trim(line)
            call cfn_s_lowcase(line)
            ios=0
            if(index(line,'end').gt.0)exit

            IF(ilay.EQ.0)THEN
             READ(LINE,*,IOSTAT=IOS) xx2,yy2,Z2; IF(IOS.NE.0)EXIT
            ELSE
             READ(LINE,*,IOSTAT=IOS) xx2,yy2; IF(IOS.NE.0)EXIT
            ENDIF
!            read(line,*,iostat=ios) xx2,yy2
            if(ios.ne.0)exit

            if(ii.gt.0)then

!######intersect line
               x1 = xx1
               y1 = yy1
               x2 = xx2
               y2 = yy2
!               IF(ilay.EQ.0)THEN; ZZ1=Z1; ZZ2=Z2; ENDIF
               if(lqd)then
                call imod_utl_intersect_equi(xmin,xmax,ymin,ymax,
     1                    simcsize,simcsize,x1,x2,y1,y2,n,.true.)
               else
                call imod_utl_intersect_nonequi(cdelr,cdelc,lnrow,lncol,
     1                    x1,x2,y1,y2,n,.true.)
               endif

               IF(ilay.EQ.0)THEN
                !## skip, probably a perfect-vertical segment
                TL=0.0; DZ=0.0
                IF(SUM(LN(1:N)).LE.0.0)THEN
                 N=0
                ELSE
                 DZ=(Z2-Z1)/SUM(LN(1:N))
                ENDIF
               ENDIF

!######fill result array
               nn = 0
               do l = 1, n
                  icol=int(ca(l))
                  irow=int(ra(l))

                  if(icol.ge.1.and.irow.ge.1.and.
     1                icol.le.lncol.and.irow.le.lnrow) then
                     m = m + 1; nn = nn + 1
                     if (iact.eq.2) then
                        genpos(m,1) = icol
                        genpos(m,2) = irow
                        genpos(m,3) = nid
                        genpos(m,4) = int(fa(l))
                        
                        IF(ilay.EQ.0)THEN
                         IF(L.EQ.1)THEN
                          TL=0.5*LN(L)
                         ELSE
                          TL=TL+0.5*LN(L-1)+0.5*LN(L)
                         ENDIF
                         ZL=Z1+(TL*DZ); genpos(m,5)=ZL*100.0
                        ENDIF

                     end if
                  end if
               enddo
               if (iact.eq.2) then
                  genip(nid) = genip(nid)+nn
               end if
            endif
            ii=ii+1
            xx1=xx2
            yy1=yy2
            IF(ilay.EQ.0)Z1=Z2
         enddo
      if (ios.ne.0) then
         write(*,*) 'ERROR. Reading ',trim(file)
         ret = -17
         exit
      end if

      end do

      if (iact.eq.1) then
         rewind lun
         if (m.gt.0) allocate(genpos(m,5))
         allocate(genip(0:nid))
         genip = 0
      else
         close(lun)
      end if

      end do ! iact

c close file
      close(lun)

c clear memory
      call intersect_deallocate()
c assign function value
      rdrs_rddata_gen = ret

c end of program
      return
      end

      subroutine assign_layer(tlp,irow,icol,z1,z2)
c modules
      use global, only: nlay, nrow, ncol, delr, delc, cv, hcof, cc, cr,
     1 kdsv
      use global, only: ibound, botm, lbotm
      use gwfmetmodule, only: coord_xll, coord_yll, time_cstring, ieq
      use imod_utl, only: IMOD_UTL_ST1CREATEIPF_GETTLP

      implicit none
c arguments
      real, dimension(nlay), intent(out) :: tlp
      integer, intent(in) :: irow, icol
      real, intent(in) :: z1, z2
c parameters
      real, parameter :: maxc = 1000000.0
      real, parameter :: minkh = 0.0
      real, parameter :: tiny = 1.0e-20
      integer, parameter :: iclay = 1
c locals
      logical :: found
      integer :: ilay
      real :: dz
      real, dimension(nlay) :: kh, c, tp, bt

c init
      tlp = 0.

c init
       kh = 0.; c = 0.; tp = 0.; bt = 0.
       do ilay = 1, nlay
          tp(ilay) = botm(icol,irow,lbotm(ilay)-1)
          bt(ilay) = botm(icol,irow,lbotm(ilay))
          if (ibound(icol,irow,ilay).gt.0) then
             if (ilay.lt.nlay) then
              c(ilay)=1.0/(cv(icol,irow,ilay)/(delr(icol)*delc(irow)))
             end if
             kh(ilay)=kdsv(icol,irow,ilay)
             kh(ilay)=(cc(icol,irow,ilay)+cr(icol,irow,ilay))/2.0
             tp(ilay) = botm(icol,irow,lbotm(ilay)-1)
             bt(ilay) = botm(icol,irow,lbotm(ilay))
             dz = tp(ilay)-bt(ilay)
             if (dz.gt.0.0) then
                kh(ilay) = kh(ilay)/(dz+tiny)
             else
                kh(ilay) = 0.0
             end if
          end if
       end do

       call IMOD_UTL_ST1CREATEIPF_GETTLP(nlay,tlp,kh,c,tp,bt,z1,z2,
     1                              maxc,minkh,iclay,'error-txt')

c## nothing in model, whenever system on top of model, put them in first modellayer
      if(sum(tlp).le.0)then
         tlp=0; if(z2.ge.tp(1))tlp(1)=1.0
      endif

c## still nothing in model, check whether in clay layer
      if(sum(tlp).le.0)then
         tlp=0
         do ilay=1,nlay-1
            if(bt(ilay).ge.z1.and.tp(ilay+1).le.z2)tlp(ilay)=1.0
         enddo
      endif

      end subroutine
