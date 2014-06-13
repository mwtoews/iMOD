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

subroutine mf2005_args(record,lfname,igrid,exitcode)

! description:
! ------------------------------------------------------------------------------
! process arguments record and create model instance
! record may contain:
!   - namefile

! declaration section
! ------------------------------------------------------------------------------
 use m_mf2005_main, only: fname,nsol,iouts,           &
                          kper,kkper,kstp,kkstp,kiter,kkiter, &
                          icnvg,ilmtfmt,issmt3d,iumt3d,       &
                          ninstance,timesteptime,initTimeStep,&
                          timeStepCalculated,solverConverged
 implicit none


! arguments
 character (len=*), intent(in)  :: record      ! arguments
 logical          , intent(out) :: lfname      ! .true. file name has to be collected by GETNAMFIL
 integer          , intent(out) :: igrid       ! model instance grid number
 integer          , intent(out) :: exitcode    ! exit code:  0: OK

! local variables
 integer   ivcl,iarg,larg,nmem

 logical   lexist

 integer          :: date,h,m,s
 double precision :: starttime

! functions
 integer   cfn_length


! program section
! ------------------------------------------------------------------------------

! init
! starttime = 0.0d0
 exitcode  = 0


! process arguments

 ! define virtual commandline
 call cfn_vcl_set(record,ivcl)

 ! ***************************
 ! * get explicite arguments *
 ! ***************************
 ! start date/time
 !call cfn_vcl_fndd(ivcl,iarg,'-start*time',.true.,starttime,1)
 !if (iarg.gt.0) then
 !   ! starttime given, convert to mjd
 !   date=int(starttime)
 !   starttime=starttime-date
 !
 !   starttime=starttime*24.d0
 !   h   =int(starttime)
 !   starttime=starttime-h
 !
 !   starttime=starttime*60.d0
 !   m   =int(starttime)
 !   starttime=starttime-m
 !
 !   starttime=starttime*60.d0
 !   s   =int(starttime)
 !   starttime=starttime-s
 !
 !   call cfn_datehms2mjd(date,h,m,s,starttime)
 !endif
 ! state-save option
 call cfn_vcl_fndi(ivcl,iarg,'-mem*ory',.true.,nmem,1)
 if (iarg.gt.0) then
    call mf2005_stsinit(nmem)
 endif


 ! ***************************
 ! * get implicit  arguments *
 ! ***************************
! record

 if (exitcode.eq.0) then
    call cfn_vcl_arg(ivcl,-1,fname,larg)

!  write(*,*) trim(record)
!  write(*,*) trim(fname)
!pause
    if (larg.gt.0) then
       ! filename found
       lfname = .false.
       ! check existence of filename
       inquire(file=fname,exist=lexist)
       if (.not.lexist) then
          ! ERROR, given file doesn't exist
          write(*,*) ' ERROR, given namefile does not exist: ',fname(1:cfn_length(fname))
          !call exit(3)
          exitcode = 3
       else
          ! OK
          lfname=.false.
       endif
    else
       ! no filename given
       lfname = .true.
    endif
 endif


! create model instance
 if (exitcode.eq.0) then
    ! check size of instance data structure
    if (ninstance.ge.10) then
      ! ERROR
       write(*,*) ' ERROR, trying to create too many instances of mf2005'
       !call exit(3)
       exitcode = 3
    endif
 endif


 if (exitcode.eq.0) then

!    ninstance=ninstance+1
    igrid=ninstance

    ! allocate all variables of data structure mi()
    allocate(nsol,iouts,kper,kkper,kstp,kkstp,kiter,kkiter,icnvg,ilmtfmt,issmt3d,iumt3d)
    allocate(timesteptime,initTimeStep,timeStepCalculated,solverConverged)

   ! init values
   ! timesteptime=starttime   ! this is an extension to standard Modflow, so it's initialised here

   ! save mi
    call sgwf2ins1psv(igrid)

 endif

end

subroutine mf2005_args_time(record,igrid)

! description:
! ------------------------------------------------------------------------------
! process arguments record and create model instance
! record may contain:
!   - namefile

! declaration section
! ------------------------------------------------------------------------------
 use m_mf2005_main, only: timesteptime
 use gwfmetmodule, only: time_cstring
 use global, only: iunit
 use m_mf2005_iu, only: iumet

 implicit none

! arguments
 character (len=*), intent(in)  :: record      ! arguments
 integer          , intent(out) :: igrid       ! model instance grid number

! local variables
 integer   ivcl,iarg,larg

 logical   lexist

 integer          :: date,h,m,s
 double precision :: starttime

! functions
 integer   cfn_length


! program section
! ------------------------------------------------------------------------------

! init
 starttime = 0.0d0

! process arguments

 ! define virtual commandline
 call cfn_vcl_set(record,ivcl)

 ! ***************************
 ! * get explicite arguments *
 ! ***************************
 ! start date/time
 call cfn_vcl_fndd(ivcl,iarg,'-start*time',.true.,starttime,1)
 if (iarg.le.0) then
    call sgwf2bas7pnt(igrid) ! set pointers
    if(IUNIT(IUMET).gt.0) then ! try the met-file
        if (associated(time_cstring)) then
            read(time_cstring,*) starttime
        else
            starttime = 20000101.0d0
        end if
    else ! set default starttime
       starttime = 20000101.0d0
    end if
 end if

 if (starttime.gt.0.0d0) then
    ! starttime given, convert to mjd
    date=int(starttime)
    starttime=starttime-date

    starttime=starttime*24.d0
    h   =int(starttime)
    starttime=starttime-h

    starttime=starttime*60.d0
    m   =int(starttime)
    starttime=starttime-m

    starttime=starttime*60.d0
    s   =int(starttime)
    starttime=starttime-s

    call cfn_datehms2mjd(date,h,m,s,starttime)

    ! save mi
    call sgwf2ins1pnt(igrid)
    timesteptime=starttime   ! this is an extension to standard Modflow, so it's initialised here
    call sgwf2ins1psv(igrid)
 end if

! end of program
 return
end

