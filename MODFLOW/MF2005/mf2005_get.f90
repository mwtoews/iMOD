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
!! get time information from MF2005
!!    mf2005_getCurrentTime             start time of current time step
!!    mf2005_getEndOfCurrentTimeStep    end   time of current time step
!!    mf2005_getBeginOfNextTimeStep     start time of next    time step
!!    mf2005_getEndTime                 (not available yet)
!! WARNING: these routines only give the information of igrid=1

subroutine mf2005_getCurrentTime(timeValue,retVal)

! declaration section
! ------------------------------------------------------------------------------
 use m_mf2005_main , only: mi
 implicit none


! arguments
 double precision, intent(out)   :: timeValue      !> start time of current time step
 integer         , intent(out)   :: retVal         !> return code   0: OK


! local variables
 integer          :: igrid


! program section
! ------------------------------------------------------------------------------

! init
 retVal = 0


! set time value
 igrid = 1
 timeValue = mi(igrid)%timesteptime


! end of program
 return
end

! ******************************************************************************

!> get end time of current time step
subroutine mf2005_getEndOfCurrentTimeStep(timeValue,retVal)

! declaration section
! ------------------------------------------------------------------------------
 use m_mf2005_main , only: mi
 implicit none


! arguments
 double precision, intent(out)   :: timeValue      !> end time of current time step
 integer         , intent(out)   :: retVal         !> return code   0: OK


! local variables
 integer          :: igrid


! functione
 double precision :: sutl_getTimeStepLength

! program section
! ------------------------------------------------------------------------------

! init
 retVal = 0


! set current time
 igrid=1
 timeValue = mi(igrid)%timesteptime + sutl_getTimeStepLength(igrid)


! end of program
 return
end

! ******************************************************************************

!> get begin time of next time step
!! When the calculations for the current time step are not finished yet the next
!! time step is the same as the current time step. So the start time of the current
!! time step is returned. Otherwise the start time of the next time step.
subroutine mf2005_getBeginOfNextTimeStep(timeValue,retVal)

! declaration section
! ------------------------------------------------------------------------------
 use m_mf2005_main , only: mi
 implicit none


! arguments
 double precision, intent(out)   :: timeValue      !> begin time of next time step
 integer         , intent(out)   :: retVal         !> return code   0: OK


! local variables
 integer          :: igrid
 double precision :: dt

! functions
 double precision :: sutl_getTimeStepLength
 double precision :: cfn_mjd_nodata
 logical :: mf2005_lastTimeStep

! program section
! ------------------------------------------------------------------------------

! init
 retVal = 0


! set current time
 igrid=1
 if (mi(igrid)%timeStepCalculated) then
    dt=sutl_getTimeStepLength(igrid)
    if (dt.lt.0.d0) then
       ! End of simulation reached --- can be steady-state for default modflow!!!!
       timeValue = cfn_mjd_nodata()
    else
       ! Current time step has been calculated satisfactorily
       timeValue = mi(igrid)%timesteptime + sutl_getTimeStepLength(igrid)
    endif
 else
    ! Current time step has to be calculated at least once more
    timeValue = mi(igrid)%timesteptime
 endif

 ! check if this was the last time step
 if (mf2005_lastTimeStep(igrid)) then
    retVal = -1
 end if

 return
end

