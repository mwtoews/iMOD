c   Copyright (C) Stichting Deltares, 2005-2014.
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

      module m_modsave2005
         USE GLOBAL,        ONLY: IBOUND,HNEW,hold,ncol,nrow,nlay,
     1                            tsmult,nstp,nper,niunit
         use GWFBASMODULE,  only: vbvl,delt,pertim,totim
         use m_mf2005_main, only: kper,kkper,kstp,kkstp
         !
     1   !                  tsmult
         ! module for Modflow state save into memory
         implicit none

         logical, save  :: saveinmem=.false.    ! .true.  save state in memory
                                                ! .false. save state on file
         integer, save  :: mxsave               ! number of states able to be saved
         integer, save  :: nsaved               ! number of states saved yet


         type stsmemmodflow
            ! define modflow state save
            integer                   :: kper,kstp,nper,nstp
            real                      :: tsmult,delt,totim,pertim
c            integer                   :: timestep
            double precision          :: timevalue
            double precision, pointer :: hnew(:)
            integer         , pointer :: ibound(:)
            real            , pointer :: vbvl(:)
         end type

         type pstsmemmodflow
            type(stsmemmodflow), pointer :: pmem
         end type
         type (pstsmemmodflow), pointer, save  :: stsmem(:)       ! size: mxsave

      end module m_modsave2005


      subroutine mf2005_stsinit(nmem)

c description:
c ------------------------------------------------------------------------------
c save state for Modflow
c

c declaration section
c ------------------------------------------------------------------------------
      use m_modsave2005

      implicit none


c arguments
      integer, intent(in)  :: nmem            ! maximum number of states to be saved


c local variables


c program section
c ------------------------------------------------------------------------------

c set saveinmem
      if (nmem.gt.0) then
         ! state save will be done in memeory
         saveinmem=.true.
         mxsave   =nmem
      else
         saveinmem=.false.
      endif


c define structures
      if (saveinmem) then

         ! check for memory leak
         if (associated(stsmem)) then
            ! ERROR
            write(*,*) ' ERROR, state save memory for Modflow'//
     1                 ' allready defined!'
            call exit(15)
         endif

         ! allocate structure
         allocate(stsmem(nmem))

         ! set number of saved states
         nsaved=0

      endif


c end of program
      return
      end

c ******************************************************************************

      subroutine mf2005_statesave(timevalue)

c description:
c ------------------------------------------------------------------------------
c save state for Modflow
c

c declaration section
c ------------------------------------------------------------------------------
      use m_modsave2005

      implicit none


c arguments
      double precision, intent(in)  :: timevalue   ! current time value
c      integer, intent(in)  :: timestep        ! current timestep

c local variables
      integer   lun         ! logical unit number
      integer   n,i,mkper
      type(stsmemmodflow), pointer :: pm

c functions
      double precision cfn_mjd_delta


c program section
c ------------------------------------------------------------------------------

c save data
      n=ncol*nrow*nlay
      if (saveinmem) then

         ! remove all timesteps with pm%timevalue >= timevalue
         i=nsaved
         do while (i.gt.0)
            pm=>stsmem(i)%pmem
            if (cfn_mjd_delta(pm%timevalue,timevalue).ge.0.d0) then
               ! remove this one
               deallocate(pm%hnew,pm%ibound,pm%vbvl)
               deallocate(stsmem(i)%pmem)
               nsaved=nsaved-1
            else
               ! no more timesteps to be checked
               i=0
            endif
            ! next
            i=i-1
         enddo

         ! free position left to store data?
         if (nsaved.ge.mxsave) then
            ! no, remove first one
            i=1
            pm=>stsmem(i)%pmem
            deallocate(pm%hnew,pm%ibound,pm%vbvl)
            deallocate(stsmem(i)%pmem)
            nsaved=nsaved-1
            ! move rest
            do i=1,nsaved
               stsmem(i)%pmem=>stsmem(i+1)%pmem
            enddo
         endif

         ! allocate
         nsaved=nsaved+1
         allocate(stsmem(nsaved)%pmem)
         pm=>stsmem(nsaved)%pmem
         allocate(pm%hnew(n),pm%ibound(n),pm%vbvl(4*niunit))

         ! store
         pm%kper     = kper
         pm%kstp     = kstp
         pm%nper     = nper
         mkper       = max(1,kper)
         pm%nstp     = nstp(mkper)
         pm%tsmult   = tsmult(mkper)
         pm%delt     = delt
         pm%totim    = totim
         pm%pertim   = pertim
c         pm%timestep = timestep
         pm%timevalue= timevalue
         call cfn_cp_d2d(hnew  ,pm%hnew  ,n)
         call cfn_cp_i2i(ibound,pm%ibound,n)
         call cfn_cp_r2r(vbvl  ,pm%vbvl  ,4*niunit)

      else
         call sts2getlun(lun)
         write(lun) kper,kstp,nper,nstp,tsmult,delt,totim,pertim,
     1              timevalue
         call utl_write_unf_dble(lun,hnew  ,n)
         call utl_write_unf_int (lun,ibound,n)
         call utl_write_unf_real(lun,vbvl  ,4*niunit)


c intel compiler (windows) can not handle buffered I/O in combination with
c repositioning file pointers (fseek)
c Therefore this call flush is added
         call flush(lun)
      endif


c end of program
      return
      end

c ******************************************************************************

      subroutine mf2005_staterestore(timevalue)

c description:
c ------------------------------------------------------------------------------
c restore state for Modflow
c

c declaration section
c ------------------------------------------------------------------------------
      use m_modsave2005

      implicit none


c arguments
      double precision, intent(in)  :: timevalue   ! current time value
c      integer, intent(in)  :: timestep        ! current timestep

c local variables
      integer   lun         ! logical unit number
      integer   n,lts,i,its,mkper
      type(stsmemmodflow), pointer :: pm

      double precision ltv


c functions
      double precision cfn_mjd_delta


c program section
c ------------------------------------------------------------------------------

c restore data
      n=ncol*nrow*nlay
      if (saveinmem) then
         ! find timestep
         its=-1
         do i=1,nsaved
            pm=>stsmem(i)%pmem
            if (cfn_mjd_delta(pm%timevalue,timevalue).eq.0.d0) then
               its=i
               exit
            endif
         enddo
         ! check
         if (its.gt.0) then
            ! found
            pm=>stsmem(its)%pmem

            kper         = pm%kper
            kstp         = pm%kstp
            nper         = pm%nper
            mkper        = max(1,kper)
            nstp(mkper)  = pm%nstp
            tsmult(mkper)= pm%tsmult
            delt         = pm%delt
            totim        = pm%totim
            pertim       = pm%pertim
            ltv          = pm%timevalue
            call cfn_cp_d2d(pm%hnew  ,hnew  ,n)
            call cfn_cp_i2i(pm%ibound,ibound,n)
            call cfn_cp_r2r(pm%vbvl  ,vbvl  ,4*niunit)

         else
            ! not found
            write(*,*) ' ERROR, Modflow state restore from memory.'//
     1       ' timestep not found.'
            call exit(16)
         endif
      else
         ! get lun
         call sts2getlun(lun)
         read(lun) kper,kstp,nper,nstp,tsmult,delt,totim,pertim,ltv
         call utl_read_unf_dble(lun,hnew  ,n)
         call utl_read_unf_int (lun,ibound,n)
         call utl_read_unf_real(lun,vbvl  ,4*niunit)
      endif

      kkper=kper
      kkstp=kstp


c check timevalue
      if (cfn_mjd_delta(ltv,timevalue).ne.0.d0) then
         write(*,'(2(a,i8))')
     1              ' ERROR: restore state of modflow from wrong'//
     1              ' timestep: data from timevalue ',ltv,
     1              ', should be from timevalue ',timevalue
         call exit(11)
      endif


c copy hnew to hold
      call cfn_cp_d2r(hnew,hold,n)


c end of program
      return
      end


