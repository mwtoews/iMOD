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
subroutine tsstat(lcor,nlag,ncol,minlag,maxlag,       &
                  lper,begdat,enddat,slen,wfc,xstep,  &
                  loff,noff,off,ostep,                &
                  lgxg,datgxg,                        &
                  result,mv,exitcode)


! declaration section
! ------------------------------------------------------------------------------
 use m_cross

 implicit none


! arguments
 integer  , intent(out)    :: exitcode       !> exit status

 real     , intent(out)    :: result(*)      !> result array
                                             !! content depends on active statistics
 real     , intent(in)     :: mv             !> nodata value


 logical  , intent(in)     :: lcor           !> .true. calculate correltations switched on
 integer  , intent(in)     :: nlag,ncol      !> 
 real     , intent(in)     :: minlag,maxlag  !> 

 logical  , intent(in)     :: lper           !> .true. calculate period        switched on
 real     , intent(in)     :: begdat,enddat,slen,wfc,xstep  !> 

 logical  , intent(in)     :: loff           !> .true. calculate offset        switched on
 integer  , intent(in)     :: noff           !> 
 real     , intent(in)     :: off,ostep      !> 

 logical  , intent(in)     :: lgxg           !> .true. calculate GxG           switched on
 integer  , intent(in)     :: datgxg(3)      !> 


! local variables
 integer   ires,ipres


! program section
! ------------------------------------------------------------------------------

! init
 exitcode = 0
 ires=1    ! first free position in result array


! set pointers
 n1=>ser(1)%n
 x1=>ser(1)%x
 y1=>ser(1)%y

 n2=>ser(2)%n
 x2=>ser(2)%x
 y2=>ser(2)%y


! only continue when both time series contain values
 if (n1.le.0 .or. n2.le.0) then
    write(*,*) ' WARNING: no measurements available: '
 else

! allocate temporary arrys
    call createtmp(nlag,ncol,noff)


! calculate correlation
   if (lcor) then
      call tscorr2(x1,y1,tsw1,n1,x2,y2,tsw2,n2,result(ires),tsw4,nlag,ncol,minlag,maxlag,mv,.true.,.true.)
      ires=ires+nlag*ncol
   endif


! calculate period
   if (lper) then
      ipres=ires   ! save position for calculate offset
      call findperiod(x1,y1,n1,begdat,enddat,slen,wfc,xstep,mv,result(ires))
      ires=ires+4
      call findperiod(x2,y2,n2,begdat,enddat,slen,wfc,xstep,mv,result(ires))
      ires=ires+4
   endif


! calculate offset
   if (loff) then
      call findoffset(x1,y1,n1,x2,y2,n2,           &
                     tsw1,tsw2,tsw3,               &
                     noff,slen,wfc,off,ostep,mv,   &
                     result(ipres),                &
                     result(ires))
      ires=ires+4
   endif


! calculate GxG
   if (lgxg) then
      call calcgxg(x1,y1,n1,datgxg,result(ires))
      ires=ires+3
      call calcgxg(x2,y2,n2,datgxg,result(ires))
      ires=ires+3
   endif

 endif



! end of program
 return
end
