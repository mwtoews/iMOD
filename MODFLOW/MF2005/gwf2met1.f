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

      subroutine gwf2met1da(igrid)
c deallocate MET memory
      use gwfmetmodule
      use idfmodule, only: xmask

      implicit none
      integer, intent(in) :: igrid

      call sgwf2met1pnt(igrid)

      if (associated(runcomment))         deallocate(runcomment)
      if (associated(coord_descr))        deallocate(coord_descr)
      if (associated(time_syear))         deallocate(time_syear)
      if (associated(time_smonth))        deallocate(time_smonth)
      if (associated(time_sday))          deallocate(time_sday)
      if (associated(time_shour))         deallocate(time_shour)
      if (associated(time_sminute))       deallocate(time_sminute)
      if (associated(time_ssecond))       deallocate(time_ssecond)
      if (associated(time_sjd))           deallocate(time_sjd)
      if (associated(time_cjd))           deallocate(time_cjd)
      if (associated(time_cstring))       deallocate(time_cstring)
      if (associated(time_ostring))       deallocate(time_ostring)
      if (associated(coord_xll))          deallocate(coord_xll)
      if (associated(coord_yll))          deallocate(coord_yll)
      if (associated(gcoord_xll))         deallocate(gcoord_xll)
      if (associated(gcoord_yll))         deallocate(gcoord_yll)
      if (associated(gcoord_xur))         deallocate(gcoord_xur)
      if (associated(gcoord_yur))         deallocate(gcoord_yur)
      if (associated(coord_xur))          deallocate(coord_xur)
      if (associated(coord_yur))          deallocate(coord_yur)
      if (associated(coord_xll_nb))       deallocate(coord_xll_nb)
      if (associated(coord_yll_nb))       deallocate(coord_yll_nb)
      if (associated(coord_xur_nb))       deallocate(coord_xur_nb)
      if (associated(coord_yur_nb))       deallocate(coord_yur_nb)
      if (associated(iss))                deallocate(iss)
      if (associated(ieq))                deallocate(ieq)
      if (associated(resultdir))          deallocate(resultdir)
      if (associated(debugdir))           deallocate(debugdir)
      if (associated(ibound_fixed_west))  deallocate(ibound_fixed_west)
      if (associated(ibound_fixed_east))  deallocate(ibound_fixed_east)
      if (associated(ibound_fixed_north)) deallocate(ibound_fixed_north)
      if (associated(ibound_fixed_south)) deallocate(ibound_fixed_south)
      if (associated(cdelr))              deallocate(cdelr)
      if (associated(cdelc))              deallocate(cdelc)
      if (associated(save_no_buf))        deallocate(save_no_buf)
      if (associated(write_debug_idf))    deallocate(write_debug_idf)
      if (associated(idate_save))         deallocate(idate_save)

      if (igrid.eq.1) then
         if (allocated(xmask)) deallocate(xmask)
      end if

      return
      end

      subroutine sgwf2met1pnt(igrid)
C change meta data to a different grid.
      use gwfmetmodule
      implicit none
      integer, intent(in) :: igrid

      runcomment   => gwfmetdat(igrid)%runcomment
      coord_descr  => gwfmetdat(igrid)%coord_descr

      idate_save   => gwfmetdat(igrid)%idate_save

      time_syear   => gwfmetdat(igrid)%time_syear
      time_smonth  => gwfmetdat(igrid)%time_smonth
      time_sday    => gwfmetdat(igrid)%time_sday
      time_shour   => gwfmetdat(igrid)%time_shour
      time_sminute => gwfmetdat(igrid)%time_sminute
      time_ssecond => gwfmetdat(igrid)%time_ssecond
      time_sjd     => gwfmetdat(igrid)%time_sjd
      time_cjd     => gwfmetdat(igrid)%time_cjd
      time_cstring => gwfmetdat(igrid)%time_cstring
      time_ostring => gwfmetdat(igrid)%time_ostring
      coord_xll    => gwfmetdat(igrid)%coord_xll
      coord_yll    => gwfmetdat(igrid)%coord_yll
      coord_xur    => gwfmetdat(igrid)%coord_xur
      coord_yur    => gwfmetdat(igrid)%coord_yur
      gcoord_xll   => gwfmetdat(igrid)%gcoord_xll
      gcoord_yll   => gwfmetdat(igrid)%gcoord_yll
      gcoord_xur   => gwfmetdat(igrid)%gcoord_xur
      gcoord_yur   => gwfmetdat(igrid)%gcoord_yur
      coord_xll_nb => gwfmetdat(igrid)%coord_xll_nb
      coord_yll_nb => gwfmetdat(igrid)%coord_yll_nb
      coord_xur_nb => gwfmetdat(igrid)%coord_xur_nb
      coord_yur_nb => gwfmetdat(igrid)%coord_yur_nb
      iss          => gwfmetdat(igrid)%iss
      ieq          => gwfmetdat(igrid)%ieq
      resultdir    => gwfmetdat(igrid)%resultdir
      debugdir     => gwfmetdat(igrid)%debugdir
      ibound_fixed_west  => gwfmetdat(igrid)%ibound_fixed_west
      ibound_fixed_east  => gwfmetdat(igrid)%ibound_fixed_east
      ibound_fixed_north => gwfmetdat(igrid)%ibound_fixed_north
      ibound_fixed_south => gwfmetdat(igrid)%ibound_fixed_south
      cdelr => gwfmetdat(igrid)%cdelr
      cdelc => gwfmetdat(igrid)%cdelc
      save_no_buf => gwfmetdat(igrid)%save_no_buf
      write_debug_idf => gwfmetdat(igrid)%write_debug_idf
      idate_save => gwfmetdat(igrid)%idate_save

      return
      end

      subroutine sgwf2met1psv(igrid)
C save meta data for a grid.
      use gwfmetmodule
      implicit none
      integer, intent(in) :: igrid

      gwfmetdat(igrid)%runcomment   => runcomment
      gwfmetdat(igrid)%coord_descr  => coord_descr

      gwfmetdat(igrid)%idate_save   => idate_save

      gwfmetdat(igrid)%time_syear   => time_syear
      gwfmetdat(igrid)%time_smonth  => time_smonth
      gwfmetdat(igrid)%time_sday    => time_sday
      gwfmetdat(igrid)%time_shour   => time_shour
      gwfmetdat(igrid)%time_sminute => time_sminute
      gwfmetdat(igrid)%time_ssecond => time_ssecond
      gwfmetdat(igrid)%time_sjd     => time_sjd
      gwfmetdat(igrid)%time_cjd     => time_cjd
      gwfmetdat(igrid)%time_cstring => time_cstring
      gwfmetdat(igrid)%time_ostring => time_ostring
      gwfmetdat(igrid)%coord_xll    => coord_xll
      gwfmetdat(igrid)%coord_yll    => coord_yll
      gwfmetdat(igrid)%coord_xur    => coord_xur
      gwfmetdat(igrid)%coord_yur    => coord_yur
      gwfmetdat(igrid)%gcoord_xll   => gcoord_xll
      gwfmetdat(igrid)%gcoord_yll   => gcoord_yll
      gwfmetdat(igrid)%gcoord_xur   => gcoord_xur
      gwfmetdat(igrid)%gcoord_yur   => gcoord_yur
      gwfmetdat(igrid)%coord_xll_nb => coord_xll_nb
      gwfmetdat(igrid)%coord_yll_nb => coord_yll_nb
      gwfmetdat(igrid)%coord_xur_nb => coord_xur_nb
      gwfmetdat(igrid)%coord_yur_nb => coord_yur_nb
      gwfmetdat(igrid)%iss          => iss
      gwfmetdat(igrid)%ieq          => ieq
      gwfmetdat(igrid)%resultdir    => resultdir
      gwfmetdat(igrid)%debugdir     => debugdir
      gwfmetdat(igrid)%ibound_fixed_west  => ibound_fixed_west
      gwfmetdat(igrid)%ibound_fixed_east  => ibound_fixed_east
      gwfmetdat(igrid)%ibound_fixed_north => ibound_fixed_north
      gwfmetdat(igrid)%ibound_fixed_south => ibound_fixed_south
      gwfmetdat(igrid)%cdelr => cdelr
      gwfmetdat(igrid)%cdelc => cdelc
      gwfmetdat(igrid)%save_no_buf => save_no_buf
      gwfmetdat(igrid)%write_debug_idf => write_debug_idf

      return
      end

      subroutine gwf2getcurrentdate(igrid,cdate) !,issflg,cdate)
      use gwfmetmodule

      USE IMOD_UTL, ONLY : IMOD_UTL_IDATETOJDATE,imod_utl_printtext
      use m_mf2005_main, only : kkper
      use global, only : issflg
      implicit none
      integer,intent(in) :: igrid !,issflg
      character(len=*),intent(out) :: cdate
      integer :: ios,idate
      
c body
      call sgwf2met1pnt(igrid)

      if (issflg(kkper).eq.0 .and. associated(time_ostring)) then ! TR
       if(idate_save.eq.0)then
        cdate=time_ostring
       elseif(idate_save.eq.1)then
        cdate=time_cstring
       endif
       cdate=adjustl(cdate)
   
       read(cdate,'(i8)',iostat=ios) idate
       IF(IOS.EQ.0)then
        IDATE=IMOD_UTL_IDATETOJDATE(IDATE)
       else
        call imod_utl_printtext('Error cannot read date '//TRIM(CDATE),2
     1)
       endif
       
      else ! SS
  
       cdate='steady-state'
  
      end if

      call sgwf2met1psv(igrid)
      
      end subroutine gwf2getcurrentdate
      
      subroutine gwf2met1ar(inmet,igrid,iout)

c description:
c ------------------------------------------------------------------------------
c allocate storage for METadata package.
c

c declaration section
c ------------------------------------------------------------------------------
c modules
      use gwfmetmodule

c implicit none statement
      implicit none

c arguments
      integer, intent(in) :: inmet            ! package input file
      integer, intent(in) :: igrid            ! grid number
      integer, intent(in) :: iout

c local variables
      integer :: ios, lloc, istart, istop, n, icol, irow
      real :: r
      character(len=300) :: line
      logical :: eol
      integer :: date, year, month, day, hour, minute, second

c functions
      character(len=300) :: cfn_trim

c include files

c program section
c ------------------------------------------------------------------------------

c nullify
      runcomment   => null()
      coord_descr  => null()

      idate_save   => null()

      time_syear   => null()
      time_smonth  => null()
      time_sday    => null()
      time_shour   => null()
      time_sminute => null()
      time_ssecond => null()
      coord_xll    => null()
      coord_yll    => null()
      coord_xur    => null()
      coord_yur    => null()
      gcoord_xll   => null()
      gcoord_yll   => null()
      gcoord_xur   => null()
      gcoord_yur   => null()
      coord_xll_nb => null()
      coord_yll_nb => null()
      coord_xur_nb => null()
      coord_yur_nb => null()
      resultdir    => null()
      debugdir     => null()
      iss          => null()
      allocate(ieq)
      ibound_fixed_west  => null()
      ibound_fixed_east  => null()
      ibound_fixed_north => null()
      ibound_fixed_south => null()
      cdelr => null()
      cdelc => null()
      save_no_buf => null()
      write_debug_idf => null()


c read options
C2------READ A LINE; IGNORE BLANK LINES AND PRINT COMMENT LINES.
      ios = 0
      do while (ios.eq.0)
         read(unit=inmet,fmt='(a)',iostat=ios) line
         if (ios.ne.0) exit
         if (ios.eq.0 .and. line.ne.' ' .and. line(1:1).ne.'#') then
            lloc=1
            r = 0.
            call urword(line,lloc,istart,istop,1,n,r,iout,inmet)
            if (line(istart:istop).eq.'COMMENT') then
               if (.not.associated(runcomment)) allocate(runcomment)
               runcomment = cfn_trim(line(lloc:))
            end if
            if (line(istart:istop).eq.'COORD_XLL') then
               if (.not.associated(coord_xll)) allocate(coord_xll)
               if (.not.associated(gcoord_xll)) allocate(gcoord_xll)
               read(line(lloc:),*) coord_xll
            end if
            if (line(istart:istop).eq.'COORD_YLL') then
               if (.not.associated(coord_yll)) allocate(coord_yll)
               if (.not.associated(gcoord_yll)) allocate(gcoord_yll)
               read(line(lloc:),*) coord_yll
            end if
            if (line(istart:istop).eq.'COORD_XUR') then
               if (.not.associated(coord_xur)) allocate(coord_xur)
               if (.not.associated(gcoord_xur)) allocate(gcoord_xur)
               read(line(lloc:),*) coord_xur
            end if
            if (line(istart:istop).eq.'COORD_YUR') then
               if (.not.associated(coord_yur)) allocate(coord_yur)
               if (.not.associated(gcoord_yur)) allocate(gcoord_yur)
               read(line(lloc:),*) coord_yur
            end if
           if (line(istart:istop).eq.'COORD_XLL_NB') then
               if (.not.associated(coord_xll_nb)) allocate(coord_xll_nb)
               read(line(lloc:),*) coord_xll_nb
            end if
            if (line(istart:istop).eq.'COORD_YLL_NB') then
               if (.not.associated(coord_yll_nb)) allocate(coord_yll_nb)
               read(line(lloc:),*) coord_yll_nb
            end if
            if (line(istart:istop).eq.'COORD_XUR_NB') then
               if (.not.associated(coord_xur_nb)) allocate(coord_xur_nb)
               read(line(lloc:),*) coord_xur_nb
            end if
            if (line(istart:istop).eq.'COORD_YUR_NB') then
               if (.not.associated(coord_yur_nb)) allocate(coord_yur_nb)
               read(line(lloc:),*) coord_yur_nb
            end if
            if (line(istart:istop).eq.'COORD_DESCR') then
               if (.not.associated(coord_descr)) allocate(coord_descr)
               coord_descr = cfn_trim(line(lloc:))
            end if
            if (line(istart:istop).eq.'RESULTDIR') then
               if (.not.associated(resultdir)) allocate(resultdir)
               read(line(lloc:),*) resultdir
            end if
            if (line(istart:istop).eq.'IBOUND_FIXED_WEST') then
               if (.not.associated(ibound_fixed_west))
     1            allocate(ibound_fixed_west)
               ibound_fixed_west = .true.
            end if
            if (line(istart:istop).eq.'IBOUND_FIXED_EAST') then
               if (.not.associated(ibound_fixed_east))
     1            allocate(ibound_fixed_east)
               ibound_fixed_east = .true.
            end if
            if (line(istart:istop).eq.'IBOUND_FIXED_NORTH') then
               if (.not.associated(ibound_fixed_north))
     1            allocate(ibound_fixed_north)
               ibound_fixed_north = .true.
            end if
            if (line(istart:istop).eq.'IBOUND_FIXED_SOUTH') then
               if (.not.associated(ibound_fixed_south))
     1            allocate(ibound_fixed_south)
               ibound_fixed_south = .true.
            end if
            if (line(istart:istop).eq.'SAVE_NO_BUF') then
               if (.not.associated(save_no_buf))
     1            allocate(save_no_buf)
               save_no_buf = .true.
            end if

            if (line(istart:istop).eq.'IDATE_SAVE') then
               if (.not.associated(idate_save))
     1            allocate(idate_save)
               read(line(lloc:),*) idate_save
            end if

            if (line(istart:istop).eq.'WRITE_DEBUG_IDF') then
               if (.not.associated(write_debug_idf))
     1            allocate(write_debug_idf)
               if (.not.associated(debugdir)) allocate(debugdir)
               write_debug_idf = .true.
               read(line(lloc:),*) debugdir
            end if
            eol = .false.
            if (line(istart:istop).eq.'STARTTIME') then
               do while(.not.eol)
                  call urword(line,lloc,istart,istop,1,n,r,iout,inmet)
                  if (istart.gt.len_trim(line)) eol = .true.
                  if (line(istart:istop).eq.'YEAR') then
                     if (.not.associated(time_syear))
     1                  allocate(time_syear)
                     read(line(lloc:),*) time_syear
                  end if
                  if (line(istart:istop).eq.'MONTH') then
                     if (.not.associated(time_smonth))
     1                  allocate(time_smonth)
                     read(line(lloc:),*) time_smonth
                  end if
                  if (line(istart:istop).eq.'DAY') then
                     if (.not.associated(time_sday))
     1                  allocate(time_sday)
                     read(line(lloc:),*) time_sday
                  end if
                  if (line(istart:istop).eq.'HOUR') then
                     if (.not.associated(time_shour))
     1                  allocate(time_shour)
                     read(line(lloc:),*) time_shour
                  end if
                  if (line(istart:istop).eq.'MINUTE') then
                     if (.not.associated(time_sminute))
     1                  allocate(time_sminute)
                     read(line(lloc:),*) time_sminute
                  end if
                  if (line(istart:istop).eq.'SECOND') then
                     if (.not.associated(time_ssecond))
     1                  allocate(time_ssecond)
                     read(line(lloc:),*) time_ssecond
                  end if
               end do
            end if
         end if
      end do

c set default for idate_save     
      if (.not.associated(idate_save)) then     
          allocate(idate_save)
          idate_save = 0
      end if    
            
c determine Julian Date of starting time
      if (associated(time_syear).and.
     1    associated(time_smonth).and.
     1    associated(time_sday)) then
          year  = time_syear
          month = time_smonth
          day   = time_sday
          date = year*10000 + month*100 + day
          hour   = 0
          minute = 0
          second = 0
          if (associated(time_shour))   hour   = time_shour
          if (associated(time_sminute)) minute = time_sminute
          if (associated(time_ssecond)) second = time_ssecond
          allocate(time_sjd)
          call cfn_datehms2mjd(date,hour,minute,second,time_sjd)
          if (.not.associated(time_cstring)) allocate(time_cstring)
          if (.not.associated(time_ostring)) allocate(time_ostring)
          write(time_cstring,'(i8)') date
      end if

c determine Julian Date of current time
      if (associated(time_sjd)) then
         allocate(time_cjd)
         time_cjd = time_sjd
      end if

c save data
      call sgwf2met1psv(igrid)

c end of program
      return
      end

      subroutine gwf2met1iss(igrid)

c modules
      use global, only: nper, issflg
      use gwfmetmodule

      implicit none

c arguments
      integer, intent(in) :: igrid

c locals
      integer :: iper

c body
      call sgwf2met1pnt(igrid)

c find out whether model is steady state or not
      if (.not.associated(iss))then
       allocate(iss)
       iss = 1 ! SS
       do iper = 1, nper
          if (issflg(iper).eq.0) iss = 0 ! TR
       enddo
      endif

      call sgwf2met1psv(igrid)

      end subroutine

      subroutine gwf2met1extent(igrid,ncol,nrow,delr,delc)

c description:
c ------------------------------------------------------------------------------
c compute coord_xur and coord_yur.
c

c declaration section
c ------------------------------------------------------------------------------
c modules
      use gwfmetmodule

c implicit none statement
      implicit none

c arguments
      integer, intent(in) :: igrid
      integer, intent(in) :: ncol
      integer, intent(in) :: nrow
      real, dimension(ncol), intent(in) :: delr
      real, dimension(nrow), intent(in) :: delc

c local variables
      integer :: icol, irow
      real :: cs

c functions

c include files

c program section
c ------------------------------------------------------------------------------
      call sgwf2met1pnt(igrid)

      if (.not.associated(coord_xur))  allocate(coord_xur)
      if (.not.associated(coord_yur))  allocate(coord_yur)
      if (.not.associated(ieq)) allocate(ieq)

c check for equidistant grid
      if ((maxval(delr).eq.minval(delr)).and.
     1    (maxval(delc).eq.minval(delc))) then
         ieq = 0 ! default equidistant
      else
         ieq = 1
      end if   
    
c determine coord_xur
      if (ieq.eq.0) then
         coord_xur = coord_xll + ncol*delr(1)
      else   
         coord_xur = coord_xll
         do icol = 1, ncol
            coord_xur = coord_xur + delr(icol)
         end do
      end if
         
c determine coord_yur
      if (ieq.eq.0) then
         coord_yur = coord_yll + nrow*delc(1) 
      else   
         coord_yur = coord_yll
         do irow = nrow, 1, -1
            coord_yur = coord_yur + delc(irow)
         end do
      end if   

c determine cumulative delr and delc
      if (associated(coord_xll).and.associated(coord_yur)) then
         allocate(cdelr(0:ncol),cdelc(0:nrow))
         if (ieq.eq.0) then
            cs = maxval(delr)
            cdelr(0) = coord_xll
            do icol = 1, ncol
              cdelr(icol) = coord_xll + real(icol)*cs
            end do
            cs = maxval(delc)
            cdelc(0) = coord_yur
            do irow = 1, nrow
              cdelc(irow) = coord_yur - real(irow)*cs
            end do
         else
            cdelr(0)=coord_xll
            cdelc(0)=coord_yur
            do icol = 1, ncol
              cdelr(icol) = cdelr(icol-1) + delr(icol)
            end do
            do irow = 1, nrow
              cdelc(irow) = cdelc(irow-1) - delc(irow)
            end do
         end if
      end if

c save data
      call sgwf2met1psv(igrid)

      return
      end

      subroutine gwf2met1st(kkper,igrid)

c description:
c ------------------------------------------------------------------------------
c Setup time for METadata package.
c

c declaration section
c ------------------------------------------------------------------------------
c modules
      use global, only: itmuni, iout, ISSFLG
      use gwfbasmodule, only: delt
      use gwfmetmodule
      use m_mf2005_main, only: timesteptime ! workaround

c implicit none statement
      implicit none

c arguments
      integer, intent(in) :: igrid,kkper            ! grid number


c local variables
      integer :: date, hour, minute, second
      double precision :: factor

c functions
      double precision :: sutl_getLengthTotalStressPeriod !sutl_getTimeStepLength      

c include files

c program section
c ------------------------------------------------------------------------------
      call sgwf2met1pnt(igrid)

      if (.not.associated(time_sjd)) return

      factor = 1.d0
      if (itmuni.eq.1) factor = 1/86400.d0 ! seconds
      if (itmuni.eq.2) factor = 1/1440.d0  ! minutes
      if (itmuni.eq.3) factor = 1/24.d0    ! hours
      if (itmuni.eq.5) then
         write(iout,*)
     1     ' ERROR. ITMUNI = 5 is not allowed using MET1 package.'
         call ustop(' ')
      end if

c update current time
      call sgwf2ins1pnt(igrid)
      if(issflg(kkper).eq.0)then
       time_cjd = timesteptime+sutl_getLengthTotalStressPeriod(igrid)
       time_ostring = time_cstring
       call cfn_mjd2datehms(time_cjd,date,hour,minute,second)
       write(time_cstring,'(i8,3i2.2)') date,hour,minute,second
      endif
      call sgwf2met1psv(igrid)

c end of program
      return
      end

      !!###====================================================================
      !SUBROUTINE met1_ITIMETOGDATE(ITIME,IH,IM,IS)
      !!###====================================================================
      ! IMPLICIT NONE
      ! INTEGER(KIND=8),INTENT(IN) :: ITIME
      ! INTEGeR,INTENT(OUT) :: IH,IM,IS
      !
      !IH =      ITIME         / 3600
      !IM = MOD( ITIME, 3600 ) / 60
      !IS = MOD( ITIME, 60 ) 
      !
      ! END SUBROUTINE met1_ITIMETOGDATE
      
      subroutine met1ubudsv(text,ibdchn,buff,ncol,nrow,
     1                      nlay,iout,retflag)

c description:
c ------------------------------------------------------------------------------
c Write idf files for budgets.
c


c declaration section
c ------------------------------------------------------------------------------
c modules
      use global, only: iunit, ibound
      use gwfbasmodule, only: HNOFLO
      use fsplitmodule
      use m_mf2005_iu, only: iumet
      implicit none

c arguments
      character(len=16), intent(in) :: text
      integer, intent(in) :: ibdchn
      integer, intent(in) :: ncol, nrow, nlay
      real,dimension(ncol,nrow,nlay) :: buff
      integer, intent(in) :: iout
      logical, intent(out) :: retflag

c local variables
      logical :: writefile
      character(len=1024) :: fname
      integer :: ilay, type, isplit, iuidx, i, j
      real(kind=8) :: nodata

c functions
      character(len=1024) :: met1fname

c parameters
      character(len=16), parameter :: frftxt = 'FLOW RIGHT FACE '
      character(len=16), parameter :: ffftxt = 'FLOW FRONT FACE '
      character(len=16), parameter :: flftxt = 'FLOW LOWER FACE '

c program section
c ------------------------------------------------------------------------------

c check if type of file
      call met1getfiletype(ibdchn,type,isplit)
      if (type.eq.0) then
         retflag = .false.
         return
      end if

c check if MET package is activated
      if (IUNIT(IUMET).le.0) then
         if (type.eq.splitidf) then
           write(iout,*)
     1     'ERROR. For using DATA(BINARYIDF) enable MET1 package.'
         end if
         if (type.eq.splitnc) then
           write(iout,*)
     1     'ERROR. For using DATA(BINARYNC) enable MET1 package.'
         end if
         call ustop(' ')
      end if

c swap sign for bcf fluxes
      if (index(text,trim(frftxt)).gt.0 .or.
     1    index(text,trim(ffftxt)).gt.0 .or.
     1    index(text,trim(flftxt)).gt.0) then
           write(iout,*)
     1     'INFO. Sign of cbc-flux ',trim(text),' is swapped!'
         buff = -buff
      end if

c loop over the layers
      do ilay = 1, nlay

c     clean for nodata
      DO I=1,NROW; DO J=1,NCOL
       IF(IBOUND(J,I,ilay).EQ.0) buff(J,I,ilay)=hnoflo !0. ! CHECK WITH PETER
      enddo; enddo

         if (type.eq.splitidf) then
            fname = met1fname(isplit,text,ilay,'idf')
!            write(*,*) trim(fname)
            writefile = .true.
            ! check SAVE BUDGET layers
            call splitgetiuidx(ibdchn,iuidx)
            if (iuidx.gt.0) then
!            write(*,*) fooclay(iuidx,ilay)
               if (fooclay(iuidx,ilay).eq.0) writefile = .false.
            end if
            if (writefile) then
               nodata = HNOFLO !0.
               call met1wrtidf(fname,buff(:,:,ilay),ncol,nrow,
     1                         nodata,iout,4)
            end if
         end if
      end do

      retflag = .true.

c end of program
      return
      end

      subroutine met1ulasav(text,ichn,buff,ncol,nrow,ilay,retflag)

c description:
c ------------------------------------------------------------------------------
c Write idf files for ulasav.
c


c declaration section
c ------------------------------------------------------------------------------
c modules
      use global, only: iunit, iout
      use gwfbasmodule, only: hnoflo
      use fsplitmodule
      use m_mf2005_iu, only: iumet

      implicit none

c arguments
      character(len=16), intent(in) :: text
      integer, intent(in)           :: ichn
      integer, intent(in)           :: ncol, nrow
      real, dimension(ncol,nrow)    :: buff
      logical, intent(out)          :: retflag

c local variables
      character(len=1024) :: fname
      integer :: ilay, type, isplit, iuidx
      real :: nodata

c functions
      character(len=1024) :: met1fname

c program section
c ------------------------------------------------------------------------------

c check if type of file
      call met1getfiletype(ichn,type,isplit)
      if (type.eq.0) then
         retflag = .false.
         return
      end if

c check if MET package is activated
      if (IUNIT(IUMET).le.0) then
         if (type.eq.splitidf) then
           write(iout,*)
     1     'ERROR. For using DATA(BINARYIDF) enable MET1 package.'
         end if
         if (type.eq.splitnc) then
           write(iout,*)
     1     'ERROR. For using DATA(BINARYNC) enable MET1 package.'
         end if
         call ustop(' ')
      end if

c check SAVE BUDGET layers
      call splitgetiuidx(ichn,iuidx)
      if (iuidx.gt.0) then
         if (fooclay(iuidx,ilay).eq.0)then
           retflag = .true.
           return
         endif
      end if

c loop over the layers
      nodata = hnoflo
      if (type.eq.splitidf) then
         fname = met1fname(isplit,text,ilay,'idf')
         call met1wrtidf(fname,buff,ncol,nrow,nodata,iout)
      end if
      if (type.eq.splitnc) then
         fname = met1fname(isplit,text,ilay,'idf')
         call met1wrtidf(fname,buff,ncol,nrow,nodata,iout)
         call met1wrtnc(fname)
      end if

      retflag = .true.

c end of program
      return
      end

      subroutine met1getfiletype(iu,type,isplit)
c declaration section
c ------------------------------------------------------------------------------
c modules
      use fsplitmodule

      implicit none

c arguments
      integer, intent(in)  :: iu
      integer, intent(out) :: type
      integer, intent(out) :: isplit

c local variables
      integer :: i

c program section
c ------------------------------------------------------------------------------
      isplit = 0
      type   = 0

      if (.not.associated(nfsplit) ) return

      do i = 1, nfsplit
        if (foiu(i).eq.iu) then
           select case( fotype(i))
              case(splitidf)
                 isplit = i
                 type   = splitidf
              case(splitnc)
                 isplit = i
                 type   = splitnc
           end select
        end if
      end do

c end of program
      return
      end

      subroutine met1wrtidf(fname,buff,ncol,nrow,nodata,iout)
c description:
c ------------------------------------------------------------------------------
c Write idf file.
c

c declaration section
c ------------------------------------------------------------------------------
c modules
      use global, only: delr, delc
      use fsplitmodule
      use gwfmetmodule
      use imod_idf
      use imod_utl, only: imod_utl_pol1located

      implicit none

c arguments
      character(len=*), intent(in) :: fname
      integer, intent(in) :: ncol, nrow
      real(kind=8), dimension(ncol,nrow) :: buff
      real(kind=8), intent(in) :: nodata
      integer, intent(in) :: iout

c parameters
      real(kind=8),parameter :: tiny=0.001D0 !1.0         !needed to compute ir1,ir2,ic1,ic2 properly

c local variables
      integer :: i, ilay, iok
      integer, dimension(2) :: dims
      real(kind=8), dimension(2) :: lcorner
      real(kind=8), dimension(ncol+nrow) :: dgrd
      logical :: lok, leq
      integer :: ic1, ic2, ir1, ir2, sncol, snrow

c functions
c      integer :: idfx_wrfile

c program section
c ------------------------------------------------------------------------------

      leq = .true.
      if (minval(delr).ne.maxval(delr)) leq = .false.
      if (minval(delc).ne.maxval(delc)) leq = .false.

c write IDF-file
      iok = 0
      if (associated(save_no_buf).and.
     1    associated(coord_xll_nb).and.associated(coord_yll_nb).and.
     1    associated(coord_xur_nb).and.associated(coord_yur_nb)) then

         call imod_utl_pol1located(cdelr,ncol+1,
     1        coord_xll_nb+tiny,ic1)
         call imod_utl_pol1located(cdelr,ncol+1,
     1        coord_xur_nb-tiny,ic2)
         call imod_utl_pol1located(cdelc,nrow+1,
     1        coord_yur_nb-tiny,ir1)
         call imod_utl_pol1located(cdelc,nrow+1,
     1        coord_yll_nb+tiny,ir2)

         !#check to make sure dimensions are within bounds!
         ic1  = max(1,ic1); ic2  = min(ic2,ncol)
         ir1  = max(1,ir1); ir2  = min(ir2,nrow)
         sncol=(ic2-ic1)+1; snrow=(ir2-ir1)+1

         if (leq) then
            lok = idfwrite_wrapper(sncol,snrow,buff(ic1:ic2,ir1:ir2),
     1                            (/delr(1)/),(/delc(1)/),
     1                            coord_xll_nb,coord_yll_nb,
     1                            nodata,'',fname)
         else
            lok = idfwrite_wrapper(sncol,snrow,buff(ic1:ic2,ir1:ir2),
     1                            delr(ic1:ic2),delc(ir1:ir2),
     1                            coord_xll_nb,coord_yll_nb,
     1                            nodata,'',fname)
         end if
      else
         if (leq) then
            lok = idfwrite_wrapper(ncol,nrow,buff,
     1                             (/delr(1)/),(/delc(1)/),
     1                             coord_xll,coord_yll,nodata,'',fname)
         else
            lok = idfwrite_wrapper(ncol,nrow,buff,delr,delc,
     1                             coord_xll,coord_yll,nodata,'',fname)
         end if
      end if

c check if everything went right
      if (iok.gt.0 .or. .not.lok) then
         write(iout,*) 'ERROR. Writing IDF file'
         call ustop(' ')
      end if

c end of program
      return
      end
      
      subroutine met1wrtnc(fname)
c description:
c ------------------------------------------------------------------------------
c Write netcdf file.
c

c declaration section
c ------------------------------------------------------------------------------
c modules
#if (defined(IFORTNC))
      use mod_nc2idf
#endif
      implicit none

c arguments
      character(len=1024), intent(inout) :: fname

c local variables
      integer :: lun, ios

c functions
      integer :: cfn_getlun

c program section
c ------------------------------------------------------------------------------

c convert idf file
#if (defined(IFORTNC))
      call nc2idf_exportnc(fname)
#else
      return
#endif

c open file and delete idf
      lun=cfn_getlun(10,99)
      open(unit=lun,file=fname,status='UNKNOWN')
      close(lun,status='DELETE')

c end of program
      return
      end

      function met1fname(isplit,text,ilay,ext)
c description:
c ------------------------------------------------------------------------------
c Write idf file.
c

c declaration section
c ------------------------------------------------------------------------------
c modules
      use fsplitmodule
      use gwfmetmodule

      USE GLOBAL,ONLY : ISSFLG
      use m_mf2005_main, only : kper
      use gwfrivmodule, only: nrivsubsys
      use gwfdrnmodule, only: ndrnsubsys

      implicit none

c function declratation
      character(len=1024) :: met1fname

c arguments
      integer, intent(in) :: isplit
      character(len=16), intent(in) :: text
      integer, intent(in) :: ilay
      character(len=*) :: ext

c local variables
      integer :: i
      character(len=16) :: txt
      character(len=10) :: fmt
      character(len=300) :: prefix
      character(len=300) :: tmp
      character(len=1024) :: fname
      character(len=300) :: root
      character(len=52) :: cdate_string
      integer :: isub
      character(len=10) :: partstr

c parameters
      CHARACTER(LEN=16), PARAMETER :: RIVTXT = 'RIV LEAKAGE     '
      CHARACTER(LEN=16), PARAMETER :: DRNTXT = '       DRAINS   '

      CHARACTER(LEN=16), PARAMETER :: STOTXT = '         STORAGE'
      CHARACTER(LEN=16), PARAMETER :: BNDTXT = '   CONSTANT HEAD'
      CHARACTER(LEN=16), PARAMETER :: FRFTXT = 'FLOW RIGHT FACE '
      CHARACTER(LEN=16), PARAMETER :: FFFTXT = 'FLOW FRONT FACE '
      CHARACTER(LEN=16), PARAMETER :: FLFTXT = 'FLOW LOWER FACE '

!      DATA textinf/'    UZF INFILTR.'/
!      DATA textinf2/'SFR-DIV. INFLTR.'/
!      DATA textrch/'    UZF RECHARGE'/
!      DATA textet/'           GW ET'/
!      DATA textexfl/' SURFACE LEAKAGE'/
!      DATA uzinftxt/'    INFILTRATION'/
!      DATA uzsttext/'  STORAGE CHANGE'/
!      DATA uzettext/'          UZF ET'/

      CHARACTER(LEN=16), PARAMETER :: INFTXT = '    UZF INFILTR.'
      CHARACTER(LEN=16), PARAMETER :: RCHTXT = '    UZF RECHARGE'
      CHARACTER(LEN=16), PARAMETER :: GETTXT = '           GW ET'
      CHARACTER(LEN=16), PARAMETER :: EXFTXT = ' SURFACE LEAKAGE'
      CHARACTER(LEN=16), PARAMETER :: UETTXT = '          UZF ET'
      CHARACTER(LEN=16), PARAMETER :: STRTXT = 'SFR-DIV. INFLTR.'
!      DATA uzinftxt/'    INFILTRATION'/
!      DATA uzsttext/'  STORAGE CHANGE'/

!      DATA textinf/'    UZF INFILTR.'/
!      DATA textinf2/'SFR-DIV. INFLTR.'/
!      DATA textrch/'    UZF RECHARGE'/
!      DATA textet/'           GW ET'/
!      DATA textexfl/' SURFACE LEAKAGE'/
!      DATA uzinftxt/'    INFILTRATION'/
!      DATA uzsttext/'  STORAGE CHANGE'/
!      DATA uzettext/'          UZF ET'/   
      
      !UZFINF BDGGRC BDGGET UZFRUN UZFET
      
      CHARACTER(LEN=16), PARAMETER :: IBSTXT = 'INTERBED STORAGE'
      CHARACTER(LEN=16), PARAMETER :: SUBTXT = '      SUBSIDENCE'
      CHARACTER(LEN=16), PARAMETER :: LCPTXT = 'LAYER COMPACTION'
      CHARACTER(LEN=16), PARAMETER :: SCPTXT = 'SYSTM COMPACTION'
      CHARACTER(LEN=16), PARAMETER :: ZDPTXT = '  Z DISPLACEMENT'
      CHARACTER(LEN=16), PARAMETER :: PSTTXT = 'PRECONSOL STRESS'
      CHARACTER(LEN=16), PARAMETER :: DPSTXT = 'CHANGE IN PCSTRS'
      CHARACTER(LEN=16), PARAMETER :: GSTTXT = 'GEOSTATIC STRESS'
      CHARACTER(LEN=16), PARAMETER :: CGTTXT = 'CHANGE IN G-STRS'
      CHARACTER(LEN=16), PARAMETER :: ESTTXT = 'EFFECTIVE STRESS'
      CHARACTER(LEN=16), PARAMETER :: DESTXT = 'CHANGE IN EFF-ST'
      CHARACTER(LEN=16), PARAMETER :: VRATXT = '      VOID RATIO'
      CHARACTER(LEN=16), PARAMETER :: THITXT = '       THICKNESS'
      CHARACTER(LEN=16), PARAMETER :: CELTXT = 'CENTER ELEVATION'

c funtions
      logical :: done, rivflg, isgflg, olfflg, drnflg, ldrn, lriv
      integer :: cfn_length,ios,ihms

c program section
c ------------------------------------------------------------------------------

      call sgwf2met1pnt(1) !igrid)

c convert TEXT
      txt = TEXT
      call cfn_s_trim(txt)
      call cfn_s_lowcase(txt)
      do i = 1, cfn_length(txt)
         if (txt(i:i).eq.' ') txt(i:i) = '_'
      end do

      done = .false.

      prefix = fofname(isplit)
      if (cfn_length(prefix).eq.0) then
         prefix = txt(1:cfn_length(txt))
         done = .true.
      end if

c check for bcf fluxen
      if(.not.done)then

       isub = 0
       if (index(text,stotxt).gt.0) isub = 1
       if (index(text,bndtxt).gt.0) isub = 2
       if (index(text,frftxt).gt.0) isub = 3
       if (index(text,ffftxt).gt.0) isub = 4
       if (index(text,flftxt).gt.0) isub = 5
       if (isub.gt.0 .and. .not.done) then
          read(prefix,*)(tmp,i=1,isub)
          prefix = tmp
          done = .true.
       end if

      endif

c check for uzf fluxen
      if(.not.done)then

       isub = 0
       if (index(text,inftxt).gt.0) isub = 1
       if (index(text,rchtxt).gt.0) isub = 2
       if (index(text,gettxt).gt.0) isub = 3
       if (index(text,exftxt).gt.0) isub = 4
       if (index(text,uettxt).gt.0) isub = 5
       if (index(text,strtxt).gt.0) isub = 6
       if (isub.gt.0 .and. .not.done) then
          read(prefix,*)(tmp,i=1,isub)
          prefix = tmp
          done = .true.
       end if

      endif

c check for scr terms
      if(.not.done)then

       isub = 0
       if (index(text,ibstxt).gt.0) isub = 1
       if (index(text,subtxt).gt.0) isub = 2
       if (index(text,lcptxt).gt.0) isub = 3
       if (index(text,scptxt).gt.0) isub = 4
       if (index(text,zdptxt).gt.0) isub = 5
       if (index(text,psttxt).gt.0) isub = 6
       if (index(text,dpstxt).gt.0) isub = 7
       if (index(text,gsttxt).gt.0) isub = 8
       if (index(text,cgttxt).gt.0) isub = 9
       if (index(text,esttxt).gt.0) isub = 10
       if (index(text,destxt).gt.0) isub = 11
       if (index(text,vratxt).gt.0) isub = 12
       if (index(text,thitxt).gt.0) isub = 13
       if (index(text,celtxt).gt.0) isub = 14
       if (isub.gt.0 .and. .not.done) then
          read(prefix,*)(tmp,i=1,isub)
          prefix = tmp
          done = .true.
       end if

      endif     
      
c assemble root
      root = ''
      if (associated(resultdir)) then
         root = resultdir(1:cfn_length(resultdir))//
     1      '\'//prefix(1:cfn_length(prefix)) //'\'
         call osd_s_filename(root)
      end if

c check for subsystem
      lriv = .false.; rivflg = .false.; isgflg = .false.
      ldrn = .false.; drnflg = .false.; olfflg = .false.
      
      i = index(text,trim(rivtxt))
      if (i.gt.0)then
       lriv=.true. !rivflg = .true.
      endif
      i = index(text,trim(drntxt))
      if(i.gt.0)then
       ldrn=.true.
      endif
      
      if(lriv.or.ldrn)then
         if (cfn_length(text(14:)).gt.0) then
            read(text(14:),*) isub
        if (isub.lt.0)then
         if(lriv) isgflg = .true.
         if(ldrn) olfflg = .true.
            isub = abs(isub)
        else
         if(lriv) rivflg = .true.
         if(ldrn) drnflg = .true.
        end if
       end if
       if(lriv.and.nrivsubsys.eq.1)then
        isub = 0
       end if   
       if(ldrn.and.ndrnsubsys.eq.1)then
        isub = 0
       end if      
         ! for isg, riv --> isg
       if (isgflg) then
             i = index(prefix,'riv')
             if (i.gt.0) prefix(i:i+2) = 'isg'
       end if
       ! for olf, drn --> olf
       if (olfflg) then
        i = index(prefix,'drn')
        if (i.gt.0) prefix(i:i+2) = 'olf'
       end if
c assemble root
             root = ''
             if (associated(resultdir)) then
                root = resultdir(1:cfn_length(resultdir))//
     1             '\'//prefix(1:cfn_length(prefix)) //'\'
                call osd_s_filename(root)
             end if
       if(isub.gt.0)then
        if (isub.lt.10) then
         fmt = '(2a,i1)'
        else
         fmt = '(2a,i2)'
        end if
         write(prefix,fmt) prefix(1:cfn_length(prefix)),'_sys', isub
      end if
      end if

c create output file name
      if (ilay.lt.10) then
         fmt = 'i1,3a)'
      else if (ilay.lt.100) then
         fmt = 'i2,3a)'
      else
         fmt = 'i3,3a)'
      end if
    
      call pks7mpipartstr(partstr) 

      if (issflg(kper).eq.0 .and. associated(time_ostring)) then ! TR
         fmt = '(5a,'//fmt
!         write(*,*) idate_save
         if(idate_save.eq.0)then
          cdate_string=time_ostring
          !## trim last zero is all zero
          read(time_ostring(9:14),*,iostat=ios) ihms
          if(ios.eq.0)then
           if(ihms.eq.0)cdate_string=time_ostring(1:8)
          endif
          write(fname,fmt) root(1:cfn_length(root)),
     1                       prefix(1:cfn_length(prefix)),'_',
     1                       cdate_string(1:cfn_length(cdate_string)),
     1                       '_l', ilay, partstr(1:cfn_length(partstr)),
     1                       '.',ext(1:cfn_length(ext))
         elseif(idate_save.eq.1)then
          cdate_string=time_cstring
          !## trim last zero is all zero
          read(time_cstring(9:14),*,iostat=ios) ihms
          if(ios.eq.0)then
           if(ihms.eq.0)cdate_string=time_cstring(1:8)
          endif
          write(fname,fmt) root(1:cfn_length(root)),
     1                       prefix(1:cfn_length(prefix)),'_',
     1                       cdate_string(1:cfn_length(cdate_string)),
     1                      '_l', ilay, partstr(1:cfn_length(partstr)),
     1                      '.',ext(1:cfn_length(ext))
!         write(fname,fmt) root(1:cfn_length(root)),
!     1                       prefix(1:cfn_length(prefix)),'_',
!     1                       time_cstring(1:cfn_length(time_cstring)),
!     1                      '_l', ilay, partstr(1:cfn_length(partstr)),
!     1                      '.',ext(1:cfn_length(ext))
         endif
      else ! SS
         fmt = '(5a,'//fmt
         write(fname,fmt) root(1:cfn_length(root)),
     1                    prefix(1:cfn_length(prefix)),'_',
     1                    'steady-state',
     1                    '_l', ilay, partstr(1:cfn_length(partstr)),
     1                    '.',ext(1:cfn_length(ext))
      end if

c assign result
      met1fname = fname

c end of program
      return
      end

      subroutine gwf2met1ibound(ibound,ncol,nrow,nlay,igrid)

c description:
c ------------------------------------------------------------------------------
c Adjust the ibound from free to fixed for a subdomain.
c

c declaration section
c ------------------------------------------------------------------------------
c modules
      use gwfmetmodule
      use pksmpi_mod, only: nrproc, myrank
      
c implicit none statement
      implicit none

c arguments
      integer,intent(in) :: ncol,nrow,nlay,igrid
      integer, dimension(ncol,nrow,nlay), intent(inout) :: ibound

c local variables
      integer :: icol, irow, ilay
      double precision :: mask
      logical :: ln, ls, le, lw
      
c functions

c include files

c program section
c ------------------------------------------------------------------------------
      call sgwf2met1pnt(igrid)

c west
      lw = .false.
      if (associated(ibound_fixed_west)) then
         if (ibound_fixed_west) then
            icol = 1
            do ilay = 1, nlay
               do irow = 1, nrow
                 if (ibound(icol,irow,ilay).gt.0) then
                    call pks7mpimaskbound( mask,icol,irow,ilay,         ! PKS
     1                 ncol,nrow,nlay )                                 ! PKS
                    if (mask.gt.-0.5d0) then ! skip band nodes
                       ibound(icol,irow,ilay) = -1
                       lw = .true.
                    end if   
                 end if   
               end do
            end do
         end if
      end if

c east
      le = .false.
      if (associated(ibound_fixed_east)) then
         if (ibound_fixed_east) then
            icol = ncol
            do ilay = 1, nlay
               do irow = 1, nrow
                 if (ibound(icol,irow,ilay).gt.0) then
                    call pks7mpimaskbound( mask,icol,irow,ilay,         ! PKS
     1                 ncol,nrow,nlay )                                 ! PKS
                    if (mask.gt.-0.5d0) then ! skip band nodes
                       ibound(icol,irow,ilay) = -1
                       le = .true.       
                    end if   
                 end if   
               end do
            end do
         end if
      end if

c north
      ln = .false.
      if (associated(ibound_fixed_north)) then
         if (ibound_fixed_north) then
            irow = 1
            do ilay = 1, nlay
               do icol = 1, ncol
                 if (ibound(icol,irow,ilay).gt.0) then
                    call pks7mpimaskbound( mask,icol,irow,ilay,         ! PKS
     1                 ncol,nrow,nlay )                                 ! PKS
                    if (mask.gt.-0.5d0) then ! skip band nodes
                       ibound(icol,irow,ilay) = -1
                       ln = .true.
                    end if   
                 end if   
               end do
            end do
         end if
      end if

c south
      ls = .false.
      if (associated(ibound_fixed_south)) then
         if (ibound_fixed_south) then
            irow = nrow
            do ilay = 1, nlay
               do icol = 1, ncol
                 if (ibound(icol,irow,ilay).gt.0) then
                    call pks7mpimaskbound( mask,icol,irow,ilay,         ! PKS
     1                 ncol,nrow,nlay )                                 ! PKS
                    if (mask.gt.-0.5d0) then ! skip band nodes
                       ibound(icol,irow,ilay) = -1
                       ls = .true.
                    end if   
                 end if   
               end do
            end do
         end if
      end if

      if (nrproc.gt.1) then
         if (ln) then
            write(*,*) 'Setting ibound=-1 for NORTH boundary',myrank
         end if   
         if (ls) then
            write(*,*) 'Setting ibound=-1 for SOUTH boundary',myrank
         end if   
         if (le) then
            write(*,*) 'Setting ibound=-1 for  EAST boundary',myrank
         end if   
         if (lw) then
            write(*,*) 'Setting ibound=-1 for  WEST boundary',myrank
         end if   
      end if
      
      return
      end
      
      subroutine gwf2met1pks(igrid,nlay,nrow,ncol)
c modules
      use gwfmetmodule, only: coord_xll, coord_yll,
     1                        coord_xur, coord_yur,     
     1                        gcoord_xll, gcoord_yll,
     1                        gcoord_xur, gcoord_yur,
     1                        coord_xll_nb, coord_yll_nb,
     1                        coord_xur_nb, coord_yur_nb, ieq
      
      use pksmpi_mod, only: myproc, mpptyp, mppser, mppini1,
     1                      iovl, inovl, gnrow, gncol, gdelr, gdelc, 
     2                      proc_ncol, proc_nrow,
     3                      proc_icolmin, proc_icolmax,
     4                      proc_irowmin, proc_irowmax
      
c implicit none statement
      implicit none
 
c arguments
      integer,intent(in) :: igrid
      integer,intent(inout) :: nlay, nrow, ncol

c local variables
      logical :: leq
      integer :: ic1, ic2, ir1, ir2, ic, ir
      real :: xll, yll, xur, yur

c program section
c ------------------------------------------------------------------------------
      
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

c checks
      if (gnrow.eq.0 .or. gncol.eq.0) then
         write(*,*) 'Program error 1 initializing PKS package for MPI' 
         call pks7mpiwrpfinalize()
         call ustop(' ')
      end if
      if (gnrow.ne.nrow .or. gncol.ne.ncol) then
         write(*,*) 'Program error 2 initializing PKS package for MPI' 
         call pks7mpiwrpfinalize()
         call ustop(' ')
      end if   
      !write(*,*) 'gdelr gdelc=',gdelr,gdelc
      if (.not.allocated(gdelr) .or. .not.allocated(gdelc)) then
         write(*,*) 'Program error 3 initializing PKS package for MPI' 
         call pks7mpiwrpfinalize()
         call ustop(' ')
      end if
      
c set pointer      
      call sgwf2met1pnt(igrid)
      
c set grid      
      ncol = proc_ncol(myproc,iovl)
      nrow = proc_nrow(myproc,iovl)

c check equidistant grid      
      leq = .true.
      if (minval(gdelr).ne.maxval(gdelr)) leq = .false.
      if (minval(gdelc).ne.maxval(gdelc)) leq = .false.
      ieq = 0
      if(.not.leq) ieq = 1
      
c save     
      gcoord_xll = coord_xll      
      gcoord_yll = coord_yll   
      
c coordinates overlapping
      ic1=proc_icolmin(myproc,iovl)
      ic2=proc_icolmax(myproc,iovl)
      ir1=proc_irowmin(myproc,iovl)
      ir2=proc_irowmax(myproc,iovl)
            
c...     local xll overlapping   
      coord_xll = gcoord_xll
      if (leq) then
         coord_xll = coord_xll + (ic1-1)*gdelr(1)    
      else
         do ic=1,ic1-1
            coord_xll = coord_xll + gdelr(ic)     
         end do    
      end if    
c...     local yll overlapping   
      coord_yll = gcoord_yll
      if (leq) then
         coord_yll = coord_yll + (gnrow-ir2)*gdelc(1)    
      else
         do ir=gnrow,ir2+1,-1
            coord_yll = coord_yll + gdelc(ir)     
         end do    
      end if    
      
c coordinates non-overlapping     
      ic1=proc_icolmin(myproc,inovl)
      ic2=proc_icolmax(myproc,inovl)
      ir1=proc_irowmin(myproc,inovl)
      ir2=proc_irowmax(myproc,inovl)

c...     local xll non-overlapping   
      xll = gcoord_xll
      if (leq) then
         xll = xll + (ic1-1)*gdelr(1)    
      else
         do ic=1,ic1-1
            xll = xll + gdelr(ic)     
         end do    
      end if    
c...     local yll non-overlapping   
      yll = gcoord_yll
      if (leq) then
         yll = yll + (gnrow-ir2)*gdelc(1)    
      else
         do ir=gnrow,ir2+1,-1
            yll = yll + gdelc(ir)     
         end do    
      end if                
c...     local xur non-overlapping
      xur = xll
      if (.not.associated(gcoord_xur)) allocate(gcoord_xur)
      gcoord_xur = gcoord_xll 
      if (leq) then
         xur = xur + proc_ncol(myproc,inovl)*gdelc(1)    
         gcoord_xur = gcoord_xur + gncol*gdelc(1)
      else
         do ic=ic1,ic2
            xur = xur + gdelr(ic)
         end do
         do ic=1,gnrow
            gcoord_xur = gcoord_xur + gdelr(ic)
         end do
      end if
c...     local yur non-overlapping
      yur = yll
      if (.not.associated(gcoord_yur)) allocate(gcoord_yur)
      gcoord_yur = gcoord_yll
      if (leq) then
         yur = yur + proc_nrow(myproc,inovl)*gdelc(1)    
         gcoord_yur = gcoord_yur + gnrow*gdelc(1)     
      else
         do ir=ir2,ir2,-1
            yur = yur + gdelc(ir)     
         end do
         do ir=gnrow,1,-1
            gcoord_yur = gcoord_yur + gdelc(ir)     
         end do
      end if   
      
      if (associated(coord_xll_nb)) coord_xll_nb = max(xll,coord_xll_nb)
      if (associated(coord_yll_nb)) coord_yll_nb = max(yll,coord_yll_nb)
      if (associated(coord_xur_nb)) coord_xur_nb = min(xur,coord_xur_nb)
      if (associated(coord_yur_nb)) coord_yur_nb = min(yur,coord_yur_nb)
      
c save data
      call sgwf2met1psv(igrid)

c      write(*,*) '==>myrank',myrank,ncol,nrow,nlay,coord_xll,coord_yll
c      call pks7mpiwrpfinalize()
c      call ustop(' ')
      
      end subroutine
      