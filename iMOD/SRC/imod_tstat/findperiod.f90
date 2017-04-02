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

subroutine findperiod(x,y,nval,minx,maxx,slen,wfc,xstep,mv,result)

! description:
! ------------------------------------------------------------------------------
! find the (sine)period of a graph at a certain interval
! 

! declaration section
! ------------------------------------------------------------------------------

 implicit none


! arguments
 integer  , intent(in)     :: nval              ! number of values in ts

 real     , intent(in)     :: x(nval),y(nval)   ! graph
 real     , intent(in)     :: mv                ! missing value for y()

 real     , intent(in)     :: minx,maxx         ! window where to find the top and bottom value
 real     , intent(in)     :: slen              ! search length
 real     , intent(in)     :: wfc               ! weight factor coefficient
 real     , intent(in)     :: xstep             ! step for x-values to find a top or bottom
                                                ! between two measurements witch are more then 
                                                ! xstep appart

 real     , intent(out)    :: result(4)         ! xtop,ytop,xbot,ybot


! local variables
 integer   i,j,ib,ie,itop,ibot,ibeg,iend,nstep

 real      vtop,vbot,v,xv,xtop,xbot,xb,xe


! functions
 real      weightedvalue

 integer   cfn_fidxlow_r,cfn_fidxupp_r


! program section
! ------------------------------------------------------------------------------

! init
 xtop=mv
 vtop=mv
 xbot=mv
 vbot=mv


! find interval positions
 ib=cfn_fidxlow_r(minx,x,nval)
 ie=cfn_fidxupp_r(maxx,x,nval)
 if (ib.gt.0) then
    itop=0
    ibot=0
    
    do i=ib,ie
       xv=x(i)
       ibeg=cfn_fidxlow_r(xv-slen,x,nval)
       iend=cfn_fidxupp_r(xv+slen,x,nval)
       if (ibeg.gt.0 .and. iend.gt.0) then
          v=weightedvalue(x,y,ibeg,iend,xv,slen,wfc)
          if (i.gt.ib) then
             ! not the first value
                if (v.gt.vtop) then
                ! new top value
                vtop=v
                itop=i
             else if (v.lt.vbot) then
                ! new bot value
                vbot=v
                ibot=i
             endif
          else
             ! firts value
             vtop=v
             itop=i
             vbot=v
             ibot=i
          endif
       endif
    enddo
    
    if (itop.gt.0) then
       xtop=x(itop)
       xbot=x(ibot)

      ! find a top/bottom between x(itop-1)...x(itop+1)  and x(ibot-1)...x(ibot+1)
      if (xstep.gt.0.0) then
       do j=1,2
          if (j.eq.1) then
             ! search arround top
             i=itop
          else
             ! search arround bot
             i=ibot
          endif
          xb=x(i)
          xe=xb
          if (i.gt.ib) xb=x(i-1)
          if (i.lt.ie) xe=x(i+1)
          ! number of steps
          nstep=int((xe-xb)/xstep)
          xv=xb
          do i=1,nstep
             ibeg=cfn_fidxlow_r(xv-slen,x,nval)
             iend=cfn_fidxupp_r(xv+slen,x,nval)
             if (ibeg.gt.0 .and. iend.gt.0) then
                v=weightedvalue(x,y,ibeg,iend,xv,slen,wfc)
                if (v.gt.vtop) then
                   ! new top value
                   xtop=xv
                   vtop=v
                else if (v.lt.vbot) then
                   ! new bot value
                   xbot=xv
                   vbot=v
                endif
             endif
             ! next xv
             xv=xv+xstep
          enddo
       enddo
      endif

    endif

    ! result
    result(1)=xtop
    result(2)=vtop
    result(3)=xbot
    result(4)=vbot
 endif

! end of program
 return
end

! ******************************************************************************

function weightedvalue(x,y,ibeg,iend,xv,slen,wfc)

! description:
! ------------------------------------------------------------------------------
! calculate a weighted average from a graph
! weigth function: wf = 1 / exp(wfc*(d-0.5*slen)/slen)
!                     = exp(-wfc*(d-0.5*slen)/slen)
!                     = exp(f1*d+f2)
!      f1=-wfc/slen
!      f2=-wfc*(-0.5*slen)/slen=wfc*0.5


! declaration section
! ------------------------------------------------------------------------------

 implicit none


! function declaration
 real      weightedvalue   ! return value: weighted value
                          !               :


! arguments
 integer  , intent(in)     :: ibeg          ! 
 integer  , intent(in)     :: iend          ! 

 real     , intent(in)     :: x(iend),y(iend)   ! graph

 real     , intent(in)     :: xv                ! centre x-value
 real     , intent(in)     :: slen              ! search length
 real     , intent(in)     :: wfc               ! weight factor coefficient

! local variables
 integer   i

 double precision f1,f2,sumval,sumwf,d,wf

! functions


! program section
! ------------------------------------------------------------------------------

! get coefficient for wf-function: wf = exp(f1*d+f2)
 f1=-1.*wfc/slen
 f2=0.5*wfc

! go
 sumval=0.d0
 sumwf =0.d0
 do i=ibeg,iend
    d =abs(xv-x(i))
    wf=exp(f1*d+f2)
    sumval=sumval+wf*y(i)
    sumwf =sumwf +wf
 enddo

! assign functionvalue
 weightedvalue = sumval/sumwf


! end of program
 return
end
