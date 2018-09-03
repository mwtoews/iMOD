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
!> description
!! ...
subroutine findoffset(x1,y1,n1,x2,y2,n2,xt,yt1,yt2,nt,  &
                      slen,wfc,off,ostep,mv,perresult,offresult)


! declaration section
! ------------------------------------------------------------------------------

 implicit none


! arguments
 integer  , intent(in)     :: n1,n2           !> size of time series

 REAL(KIND=DP_KIND)     , intent(in)     :: x1(n1),y1(n1)   !> time series 1
 REAL(KIND=DP_KIND)     , intent(in)     :: x2(n2),y2(n2)   !> time series 2
 REAL(KIND=DP_KIND)     , intent(in)     :: perresult(4,2)  !> xtop,ytop,xbot,ybot for each time series
 REAL(KIND=DP_KIND)     , intent(out)    :: offresult(4)    !> varmovav,crsmovav,movav1,movav2
 REAL(KIND=DP_KIND)     , intent(in)     :: slen            !> search length
 REAL(KIND=DP_KIND)     , intent(in)     :: wfc             !> weight factor coefficient
 integer  , intent(in)     :: nt              !> length of temp arrays
 REAL(KIND=DP_KIND)     , intent(out)    :: xt(nt),yt1(nt),yt2(nt) !> temporary time series
 REAL(KIND=DP_KIND)     , intent(in)     :: off             !> start value to create timeseries of movav
 REAL(KIND=DP_KIND)     , intent(in)     :: ostep           !> step size for creating timeseries
 REAL(KIND=DP_KIND)     , intent(in)     :: mv              !> missing value for y()

! local variables
 integer   n,i,ibeg,iend

 REAL(KIND=DP_KIND)      xbot1,ybot1,xbot2,ybot2,xv

 REAL(KIND=DP_KIND) val1,val2,vald,cov,var1,var2,vard,gem1,gem2,gemd,corr


! functions
 integer   cfn_fidxlow_r,cfn_fidxupp_r

 REAL(KIND=DP_KIND)      weightedvalue


! program section
! ------------------------------------------------------------------------------

! get results of period calculations
 xbot1=perresult(3,1)
 ybot1=perresult(4,1)
 xbot2=perresult(3,2)
 ybot2=perresult(4,2)

! check for missing values of perresult
 if (xbot1.ne.mv .and. ybot1.ne.mv .and. xbot2.ne.mv .and. ybot2.ne.mv) then

   ! create a date series
    xt(1)=off
    do i=2,nt
       xt(i)=xt(i-1)+ostep
    enddo


   ! find moving average series
   !    series 1
    do i=1,nt
       xv=xt(i)+xbot1
       ibeg=cfn_fidxlow_r(xv-slen,x1,n1)
       iend=cfn_fidxupp_r(xv+slen,x1,n1)
       if (ibeg.ge.1 .and. iend.ge.ibeg) then
          yt1(i)=weightedvalue(x1,y1,ibeg,iend,xv,slen,wfc)
       else
          yt1(i)=mv
       endif
    enddo
   !    series 2
    do i=1,nt
       xv=xt(i)+xbot2
       ibeg=cfn_fidxlow_r(xv-slen,x2,n2)
       iend=cfn_fidxupp_r(xv+slen,x2,n2)
       if (ibeg.ge.1 .and. iend.ge.ibeg) then
          yt2(i)=weightedvalue(x2,y2,ibeg,iend,xv,slen,wfc)
       else
          yt2(i)=mv
       endif
    enddo


   ! calc cross correlation between two movav series
    cov =0.0D0
    var1=0.0D0
    var2=0.0D0
    vard=0.0D0
    gem1=0.0D0
    gem2=0.0D0
    gemd=0.0D0
    n   =0
    do i=1,nt
       val1 = yt1(i)
       val2 = yt2(i)
       if (val1.ne.mv .and. val2.ne.mv) then
          n=n+1
          vald = val1 - val2
          cov  = cov  + val1*val2
          var1 = var1 + val1*val1
          var2 = var2 + val2*val2
          vard = vard + vald*vald
          gem1 = gem1 + val1
          gem2 = gem2 + val2
          gemd = gemd + vald
       endif
    enddo
    if (n.gt.0) then
       cov  = cov /n
       var1 = var1/n
       var2 = var2/n
       vard = vard/n
       gem1 = gem1/n
       gem2 = gem2/n
       gemd = gemd/n
       var1 = var1 - gem1*gem1
       var2 = var2 - gem2*gem2
       vard = vard - gemd*gemd
       cov  = cov  - gem1*gem2
       corr = cov / sqrt(var1*var2)
    else
       cov  = mv
       var1 = mv
       var2 = mv
       vard = mv
       gem1 = mv
       gem2 = mv
       gemd = mv
       var1 = mv
       var2 = mv
       vard = mv
       cov  = mv
       corr = mv
    endif


   ! set values
    offresult(1)=vard       ! varmovav
    offresult(2)=corr       ! crsmovav
    offresult(3)=yt1(1)     ! movav1
    offresult(4)=yt2(1)     ! movav2
 else
    offresult(1)=mv         ! varmovav
    offresult(2)=mv         ! crsmovav
    offresult(3)=mv         ! movav1
    offresult(4)=mv         ! movav2
 endif
 


! end of program
 return
end
