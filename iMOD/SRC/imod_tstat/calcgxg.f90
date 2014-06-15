!!  Copyright (C) Stichting Deltares, 2005-2014.
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

!> Calculate GxG from time series
!! result: GHG, GLG, nYear
!!
!! method:
!!    - get GH3 and GL3
!!    - choose 24 measurements to calculate GH3 and GL3, take one mesaurement
!!      in each period of 1/24 year
subroutine calcgxg(x,y,nval,gxgdat,result)


! declaration section
! ------------------------------------------------------------------------------

 implicit none


! arguments
 integer  , intent(in)     :: nval              !> number of values in ts

 real     , intent(in)     :: x(nval),y(nval)   !> x,y values

 integer  , intent(in)     :: gxgdat(3)         !> start/end date (yyyymmdd), min # meas for Gx3

 real     , intent(out)    :: result(3)         !> GHG,GLG,nyear


! local variables
 integer, parameter :: np=24    ! number of periods in one year to find measurements for Gx3

 integer   sdate,edate,ngx3,nyear,ibeg,iend,i,j,jb,je,js,nmin

 real      gx3(np),sday,eday,bper,eper,gh3,gl3,d,ds,cper

 integer   measdate(np)

! functions
 integer   cfn_dat2cen,cfn_fidxlow_r,cfn_fidxupp_r

! data
 data measdate/0114,0128,0214,0228,0314,0328,0414,0428,0514,0528,0614,0628,  &
               0714,0728,0814,0828,0914,0928,1014,1028,1114,1128,1214,1228/

! program section
! ------------------------------------------------------------------------------


 if(nval.le.0)then
  result(1)=0.0             ! GHG
  result(2)=0.0             ! GLG
  result(3)=0               ! nyear
  return
 endif
 
! get start data
 sdate=gxgdat(1)
 edate=gxgdat(2)
 nmin =gxgdat(3)
 nmin = max(3,nmin)

 gh3=0.0
 gl3=0.0

 nyear=0
 do while (sdate.lt.edate)
 
    ! end date of current year
    sday=cfn_dat2cen(sdate)
    sdate=sdate+10000         ! end of this year
    eday=cfn_dat2cen(sdate)

    ! find start and end position for current year
    ibeg=cfn_fidxlow_r(sday,x,nval)
    iend=cfn_fidxupp_r(eday,x,nval)

    eper=sday
    ngx3=0
    do i=np,1,-1
       bper=eper                           ! begin of period to find one value for Gx3
       cper=eday-(eday-sday)*(np-i)/np     ! center date for search measurements
       !cnpr=eday-(eday-sday)*(np-i+1)/np   ! center date of next search period
       eper=bper+(eday-bper)/i             ! end   of period to find one value for Gx3
       cper=(eper-bper)/2.                 ! center date for search measurements

       ! change center date (cper) to 14th or 28th of a month
       

       ! get interval to process for this period
       jb=cfn_fidxlow_r(bper,x,nval)
       je=cfn_fidxupp_r(eper,x,nval)

       js=0
       ds=9.e30
       if (je.ge.jb .and. jb.gt.0) then
          ! some measurements found
          
          ! check or x(je)<eper
          if (.not. x(je).lt.eper) je=je-1

          ! find the measurements closesed to cper
          do j=jb,je
             d=abs(x(j)-cper)
             if (d.lt.ds) then
                ! this measurement is closer to cper then the former one
                js=j
                d =ds
             endif
          enddo
       endif

       ! measurement found? store it
       if (js.gt.0) then
          ngx3=ngx3+1
          gx3(ngx3)=y(js)
       endif

    enddo

    ! calc gx3 (at least nmin measurements)
    if (ngx3.ge.nmin) then
       call sortem(1,ngx3,gx3,0,(/0.0/),(/0.0/),(/0.0/),(/0.0/),(/0.0/),(/0.0/),(/0.0/))
       ! add values to gx3
       do i=1,3
          gl3=gl3+gx3(i)
       enddo
       do i=ngx3-2,ngx3
          gh3=gh3+gx3(i)
       enddo
       ! increase nyear
       nyear=nyear+1
    endif

 enddo


! calculate result
 if (nyear.gt.0) then
    result(1)=gh3/(nyear*3)   ! GHG
    result(2)=gl3/(nyear*3)   ! GLG
    result(3)=nyear           ! nyear
 else
    result(1)=0.0             ! GHG
    result(2)=0.0             ! GLG
    result(3)=0               ! nyear
 endif


! end of program
 return
end
