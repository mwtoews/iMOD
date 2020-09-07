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
c
c state save for Modflow2005
c
c Method:
c  during forward run
c  ------------------
c   1: when initialising packages
c       - subscribe all input and output files for each package
c   2: before reading stress/timestep information
c       - save the file pointers of all input files
c       - save the file pointers of all output files
c       - save the timestep information kper,kstp,delt
c       - save HNEW, IBOUND, VBVL to file
c   3: when reading stress information
c       - save if new information is read or not for every input file
c         This has to be implemented in every RP-routine of every package
c  restoring a timestep
c  --------------------
c       - find the last timestep at or before the the timestep to rollback to
c         where data was available
c       - set the file pointers of the input files to these corresponding records
c       - call the stress data read routines
c       - restore the timestep information kper,kstp,delt
c       - restore HNEW, IBOUND, VBVL from file
c       - set the file pointers of the input  files to the wanted timestep
c       - set the file pointers of the output files to the wanted timestep
c       -
c how to use these routines
c -------------------------
c MAIN
c    sts2init(lsts:I,lun:IO)     initialise STS before components start opening files
c    sts2start(timevalue:I)      set currenttime for first timestep (necessary?????? or implement in other routines????)
c        nextts(timevalue:I,restore:O)
c        save(timevalue:I)
c        restore1(timevalue:I)
c        restore2(timevalue:I)
c    sts2saverestore(timevalue:I,stsave:I,strestore:I,phase:I)
c
c Packages
c    sts2subscribe(lun:I)
c    sts2unsubscribe(lun:I)
c    sts2data(lun:I)
c    sts2nodata(lun:I)

      module m_sts2

         ! description:
         ! ------------------------------------------------------------------------------
         ! State Save module
         !

         implicit none

         ! parameters to define filedat() array with
         ! REMARK: the order of the numbering of these parameters is important, the value itself isn't.
         !         In routine sts_setdata() the maximum value of the already defined
         !         value and the new value will be used. So, the highest value of these parameters
         !         has always precedence over the lower value.
         integer, parameter    :: p_dataunk=0      ! unknown whether data is avilable for this timestep
         integer, parameter    :: p_datano=1       ! NO data avilable for this timestep
         integer, parameter    :: p_data=2         ! data avilable for this timestep


         ! parameter to set restore2 or not
         integer, parameter    :: p_res2_yes=1     ! restore for unit has to be done in sts_restore2 too
         integer, parameter    :: p_res2_no =0     ! restore for unit has NOT to be done in sts_restore2


         logical, save         :: usests=.false.   ! .true.: sts-package is active

         integer, save         :: mxluns=0         ! maximum number of units allocated
         integer, save         :: nluns=0          ! number of units defined
         integer, save         :: lastilun=0       ! for fast search of unit number
         integer, save         :: nts =0           ! number of timesteps stored
         integer, save         :: mxts=0           ! maximum number af timesteps allocated
       !  integer, save         :: currentts        ! current timestep
         double precision, save:: currenttime      ! current time value
         logical, save         :: currentTimeStepCalculated=.false.
         integer, save         :: lastts=-1        ! last saved timestep
         logical, save         :: restorets        ! restore timestep

         integer, save         :: stslun=0         ! unit number to save state data to

         type sts_tsinfo
            ! time step information
            integer           :: kper ! not used
            integer           :: kstp ! not used
            real              :: delt ! not used
            double precision  :: timevalue       ! start time of current time step (MJD)
         end type

         type sts_luninfo
            ! information of unit
            ! size of arrays is: mxts
            integer           :: lun          ! unit number for which next information holds
            integer           :: res2         ! Value is set in sts_restore1 routine
                                              !    0: don't restore this unit in routine sts_restore2
                                              !    1: restore in restore2
            integer, pointer  :: filepsb(:)   ! file pointer for corresponding lun at
                                              ! the beginning of this timestep
                                              !   -1: not set yet
                                              !  >=0: set
            integer, pointer  :: filepse(:)   ! file pointer for corresponding lun at
                                              ! the end       of this timestep
                                              !   -1: not set yet
                                              !  >=0: set
            integer, pointer  :: filedat(:)   ! Type of data filepointer points to
                                              !    p_dataunk: unknown whether data is available
                                              !    p_datano : data of former timestep reused
                                              !    p_data   : new data available at this timestep
                                              ! This value is set by sts2data or sts2nodata
         end type

         type stsptsinfo
            ! pointer to type sts_tsinfo
            type(sts_tsinfo), pointer :: ptsinfo
         end type

         type stspluninfo
            ! pointer to type sts_luninfo
            type(sts_luninfo), pointer :: pluninfo
         end type

         type(stsptsinfo) , pointer, save  :: tsinfo(:)    ! size: mxts
         type(stspluninfo), pointer, save  :: luninfo(:)   ! size: mxluns

      end module m_sts2

c ******************************************************************************
      subroutine utl_write_unf_int(lun,data,ndata)

c description:
c ------------------------------------------------------------------------------
c WRITE UNFormatted INTeger array
c

c declaration section
c ------------------------------------------------------------------------------

      implicit none

c arguments
      integer, intent(in)   :: lun             ! logical unit number
      integer, intent(in)   :: ndata           ! number of data elements
      integer, intent(in)   :: data(ndata)     ! data

c local variables
      integer   i

c program section
c ------------------------------------------------------------------------------

c write
      write(lun) (data(i),i=1,ndata)


c end of program
      return
      end

c ******************************************************************************

      subroutine utl_write_unf_real(lun,data,ndata)

c description:
c ------------------------------------------------------------------------------
c WRITE UNFormatted REAL array
c

c declaration section
c ------------------------------------------------------------------------------

      implicit none

c arguments
      integer, intent(in)   :: lun             ! logical unit number
      integer, intent(in)   :: ndata           ! number of data elements
      real   , intent(in)   :: data(ndata)     ! data

c local variables
      integer   i

c program section
c ------------------------------------------------------------------------------

c write
      write(lun) (data(i),i=1,ndata)


c end of program
      return
      end

c ******************************************************************************

      subroutine utl_write_unf_dble(lun,data,ndata)

c description:
c ------------------------------------------------------------------------------
c WRITE UNFormatted DouBLE precision array
c

c declaration section
c ------------------------------------------------------------------------------

      implicit none

c arguments
      integer, intent(in)   :: lun             ! logical unit number
      integer, intent(in)   :: ndata           ! number of data elements
      double precision, intent(in)   :: data(ndata)     ! data

c local variables
      integer   i

c program section
c ------------------------------------------------------------------------------

c write
      write(lun) (data(i),i=1,ndata)


c end of program
      return
      end

c ******************************************************************************

      subroutine utl_read_unf_int(lun,data,ndata)

c description:
c ------------------------------------------------------------------------------
c READ UNFormatted INTeger array
c

c declaration section
c ------------------------------------------------------------------------------

      implicit none

c arguments
      integer, intent(in)   :: lun             ! logical unit number
      integer, intent(in)   :: ndata           ! number of data elements
      integer, intent(out)  :: data(ndata)     ! data

c local variables
      integer   i

c program section
c ------------------------------------------------------------------------------

c write
      read(lun) (data(i),i=1,ndata)


c end of program
      return
      end

c ******************************************************************************

      subroutine utl_read_unf_real(lun,data,ndata)

c description:
c ------------------------------------------------------------------------------
c READ UNFormatted REAL array
c

c declaration section
c ------------------------------------------------------------------------------

      implicit none

c arguments
      integer, intent(in)   :: lun             ! logical unit number
      integer, intent(in)   :: ndata           ! number of data elements
      real   , intent(out)  :: data(ndata)     ! data

c local variables
      integer   i

c program section
c ------------------------------------------------------------------------------

c write
      read(lun) (data(i),i=1,ndata)


c end of program
      return
      end

c ******************************************************************************

      subroutine utl_read_unf_dble(lun,data,ndata)

c description:
c ------------------------------------------------------------------------------
c READ UNFormatted DouBLE precision array
c

c declaration section
c ------------------------------------------------------------------------------

      implicit none

c arguments
      integer, intent(in)   :: lun             ! logical unit number
      integer, intent(in)   :: ndata           ! number of data elements
      double precision, intent(out)  :: data(ndata)     ! data

c local variables
      integer   i

c program section
c ------------------------------------------------------------------------------

c write
      read(lun) (data(i),i=1,ndata)


c end of program
      return
      end

c ******************************************************************************

      subroutine sts2init(lsts,lun)

c description:
c ------------------------------------------------------------------------------
c Init State Save
c

c declaration section
c ------------------------------------------------------------------------------
      use m_sts2
      implicit none


c arguments
      logical, intent(in)    :: lsts          ! .true. switch sts on
      integer, intent(in)    :: lun           ! unit number to store state save


c local variables
      integer   ios


c functions
      integer   osd_open2,cfn_getlun


c program section
c ------------------------------------------------------------------------------


c init
      if (lsts) then
         usests=.true.
         write(*,*) ' ***** State Save in effect. *****'
      else
         usests=.false.
         write(*,*) ' ***** State Save will not be used. *****'
      endif


c allocate
      if (usests) then
         ! init timestep
       !  currentts=0
         currenttime=0.0d0
         restorets=.false.
         stslun   =lun

         ! nullify pointer arrays
         nullify(tsinfo)
         nullify(luninfo)

         ! allocate timestep info pointer
         ! make a wild guess about the length
         ! if too short it can be redefined later
         !    mxts  =1000
         !    mxluns= 200
         call ssts2increaseStructures(1000,200)
c         nts=0


         ! unit number
         if (stslun.le.0) then
            stslun=cfn_getlun(10,99)
         endif

         ! open state save file
         ios=osd_open2(stslun,0,'StateSaveBin.dat','unformatted')
         call sts2subscribe(stslun)

      endif


c end of program
      return
      end

c ******************************************************************************

      subroutine sts2start(timevalue)

c description:
c ------------------------------------------------------------------------------
c State Save start saving unit information from:
c     sts_data()
c     sta_nodata()
c Internal Current timevalue is set to timevalue

c declaration section
c ------------------------------------------------------------------------------
      use m_sts2
      implicit none


c arguments
c      integer, intent(in)    :: timestep      ! timestep number to start with
      double precision, intent(in)  :: timevalue    ! start date/time of current time step


c program section
c ------------------------------------------------------------------------------


c check or sts is in effect
      if (.not. usests) return


c init timestep
      ! currentts=timestep
      currenttime=timevalue


c end of program
      return
      end

c ******************************************************************************

      subroutine ssts2nextts(timevalue)

c description:
c ------------------------------------------------------------------------------
c Init next timestep
c If timestep> currentts   then  continue run
c    timestep<=currentts         data of former timestep needs to be restored
c                                datainformation of units will be cleared

c declaration section
c ------------------------------------------------------------------------------
      use m_sts2
      implicit none


c arguments
      ! integer, intent(in)    :: timestep      ! timestep number to start with
      double precision, intent(in)  :: timevalue     ! start date/time of current time step


c local variables
      integer   ilun,its

      integer   timestep,currentts

      integer, pointer   :: filedat(:),filepsb(:),filepse(:)


c program section
c ------------------------------------------------------------------------------


c check or sts is in effect
      if (.not. usests) return


c find idices for time values
      call ssts2timeposition(timevalue  ,timestep, .false.)
      call ssts2timeposition(currenttime,currentts, .false.)

c check
c    timestep has to be older then or equal to currentts
c  and
c    timestep has to be older then or equal to last saved timestep
      if (timestep.le.0 .or. currentts.le.0) then
         restorets=.false.
      else
         if (timestep.lt.currentts) then
            restorets=.true.
         else if (timestep.eq.currentts.and.
     1            currentTimeStepCalculated) then
            restorets=.true.
         else
            restorets=.false.
         endif
      end if

      write(*,*)' sts2nextts: ',timevalue,currenttime,timestep,
     1                          currentts,restorets

      if (restorets) then
         ! restore timestep is wanted

         ! reset unit information from timestep+2...currentts
         do ilun=1,nluns
            filedat=>luninfo(ilun)%pluninfo%filedat
            filepsb=>luninfo(ilun)%pluninfo%filepsb
            filepse=>luninfo(ilun)%pluninfo%filepse
            do its=timestep+1,currentts
               filedat(its)=p_dataunk
               filepsb(its)=-1
               filepse(its)=-1
            enddo
         enddo

      endif
      write(*,*) ' ***** State Save nextts: ',restorets,currentts,
     1           timestep

c set currentts to the new value
      currentTimeStepCalculated=.false.

c end of program
      return
      end

c ******************************************************************************

      subroutine sts2subscribe(lun)

c description:
c ------------------------------------------------------------------------------
c subscribe a logical unit number to the State Save module
c This routine can be called by everyone who wants to subscribe a unit number

c declaration section
c ------------------------------------------------------------------------------
      use m_sts2

      implicit none


c arguments
      integer, intent(in)    :: lun         ! logical unit number to subscribe


c local variables
      integer   i,ilun

      integer, pointer   :: filedat(:),filepsb(:),filepse(:)


c program section
c ------------------------------------------------------------------------------


c check or sts is in effect
      if (.not. usests) return


c check or lun is already defined
      call ssts2getilun(lun,ilun)


c add when not already defined
      if (ilun.le.0) then

         ! array long enough?
         if (nluns.ge.mxluns) then
            ! increase number of units
            call ssts2increaseStructures(0,nluns+1)
         endif

         nluns=nluns+1
         ! allocate
         allocate(luninfo(nluns)%pluninfo)
         allocate(luninfo(nluns)%pluninfo%filepsb(mxts))
         allocate(luninfo(nluns)%pluninfo%filepse(mxts))
         allocate(luninfo(nluns)%pluninfo%filedat(mxts))
         ! assign
         luninfo(nluns)%pluninfo%lun=lun
         ! fill filepos with -1
         filedat=>luninfo(nluns)%pluninfo%filedat
         filepsb=>luninfo(nluns)%pluninfo%filepsb
         filepse=>luninfo(nluns)%pluninfo%filepse
         do i=1,mxts
            filedat(i)=p_dataunk
            filepsb(i)=-1
            filepse(i)=-1
         enddo

      write(*,*) ' ***** State Save subscribe  : ',lun
      endif


c end of program
      return
      end

c ******************************************************************************

      subroutine sts2unsubscribe(lun)

c description:
c ------------------------------------------------------------------------------
c unsubscribe a logical unit number from the State Save module
c This routine can be called by everyone who wants to unsubscribe a unit number

c declaration section
c ------------------------------------------------------------------------------
      use m_sts2

      implicit none


c arguments
      integer, intent(in)    :: lun         ! logical unit number to subscribe


c local variables
      integer   ilun


c program section
c ------------------------------------------------------------------------------


c check or sts is in effect
      if (.not. usests) return


c check or lun is already defined
      call ssts2getilun(lun,ilun)


c remove when defined
      if (ilun.gt.0) then

         ! deallocate
         deallocate(luninfo(ilun)%pluninfo%filedat)
         deallocate(luninfo(ilun)%pluninfo%filepsb)
         deallocate(luninfo(ilun)%pluninfo%filepse)
         deallocate(luninfo(ilun)%pluninfo)

         ! move last structure to empty position
         if (ilun.lt.nluns) then
            luninfo(ilun)%pluninfo => luninfo(nluns)%pluninfo
            nullify(luninfo(nluns)%pluninfo)
         endif

         ! decrease number of defined structures
         nluns=nluns-1

      write(*,*) ' ***** State Save unsubscribe: ',lun
      endif


c end of program
      return
      end

c ******************************************************************************

      subroutine ssts2save(timevalue,phase)

c description:
c ------------------------------------------------------------------------------
c save state for timestep
c - save file-pointers
c Check array sizes and re-allocate when necessary
c Only save when no restore has to be done

c declaration section
c ------------------------------------------------------------------------------
      use m_sts2

      implicit none


c arguments
c      integer, intent(in)  ::    ts         ! time step number
      double precision, intent(in)  :: timevalue     ! start date/time of current time step
      integer         , intent(in)  :: phase         ! 1: at begin of timestep loop
                                                     ! 2: at end   of timestep loop

c local variables
      integer   ilun

      integer   ts

      integer, pointer, save ::  filedat(:), filepsb(:), filepse(:)
      integer, pointer       ::  filepos(:)

      type(sts_luninfo), pointer :: pluninfo

c program section
c ------------------------------------------------------------------------------

c check or sts is in effect
      if (.not. usests) return


c find idices for time values
      call ssts2timeposition(timevalue,ts,.false.)

c if last saved timestep equal to ts, not necessary to save
      if (ts.eq.lastts .and. phase.eq.1) return

      call ssts2timeposition(timevalue,ts,.true.)

c check array sizes
      call ssts2increaseStructures(ts,0)


c save file pointers
      if (.not.restorets) then
         write(*,*) ' ***** State Save save: ',ts
         do ilun=1,nluns
            ! use pointer
            pluninfo => luninfo(ilun)%pluninfo
            filedat  => pluninfo%filedat
            filepsb  => pluninfo%filepsb
            filepse  => pluninfo%filepse

            ! set position depending on phase
            if (phase.eq.1) then
               filepos=>filepsb
            else
               filepos=>filepse
            endif

            ! save file position
            call osd_ftell(pluninfo%lun,filepos(ts))

            ! set filedat to unknown
            !filedat(ts)=p_dataunk
            write(*,*) '       lun: ',pluninfo%lun,filepos(ts)
         enddo

         ! save last save timestep
         lastts=ts

         ! save time value
         tsinfo(ts)%ptsinfo%timevalue=timevalue
         ! set new number of saved timesteps
         nts=max(nts,ts)

      endif

c end of program
      return
      end

c ******************************************************************************

      subroutine ssts2restore1(timevalue)

c description:
c ------------------------------------------------------------------------------
c restore timestep
c set file-pointers to the first timestep at or before ts where data was available
c

c declaration section
c ------------------------------------------------------------------------------
      use m_sts2

      implicit none


c arguments
      ! integer, intent(in)  ::    ts         ! time step number
      double precision, intent(in)  :: timevalue     ! start date/time of current time step


c local variables
      integer   ilun,its
      integer   ts

      integer, pointer ::  filedat(:), filepsb(:), filepse(:)

      type(sts_luninfo), pointer :: pluninfo

c program section
c ------------------------------------------------------------------------------

c check or sts is in effect
      if (.not. usests) return


c find idices for time values
      call ssts2timeposition(timevalue,ts,.true.)


c for each lun
      if (restorets) then
      write(*,*) ' ***** State Save restore1: ',ts
         do ilun=1,nluns
            ! use pointer
            pluninfo => luninfo(ilun)%pluninfo
            filedat  => pluninfo%filedat
            filepsb  => pluninfo%filepsb
            filepse  => pluninfo%filepse


            ! find position where data was found
            its=ts
            do while (its.gt.1 .and. filedat(its).ne.p_data)
               its=its-1
            enddo

            ! check or pinfo%filedat(its).eq.p_data
            if (filedat(its).ne.p_data) then
               ! see or timestep ts had some data to read
               if (filepsb(ts).ne.filepse(ts)) then
                  its=ts
               else
                  ! try again, look for a timestep where the fileposition changes
                  its=ts
                  do while (its.gt.1 .and. filepsb(its).eq.filepsb(ts))
                     its=its-1
                  enddo
               endif

            endif

            ! chek for res2 value
            if (its.eq.ts) then
               ! no restore needed in routine sts2restore2()
               pluninfo%res2=p_res2_no
            else
               pluninfo%res2=p_res2_yes
            endif

            ! restore file position
            if (filepsb(its).ge.0) then
               call osd_fseek(pluninfo%lun,filepsb(its),0)
               write(*,'(6x,a,6i8)') 'restore lun,psb,dat,res2,ts,its:',
     1                 pluninfo%lun,filepsb(its),filedat(its),
     1                                   pluninfo%res2,ts,its
            else
               call osd_fseek(pluninfo%lun,0,0)
            endif

            if (pluninfo%res2.ne.p_res2_yes) then
               ! reset data info to unknown for this timestep
               filedat(ts)=p_dataunk
            endif

         enddo
      endif


c end of program
      return
      end

c ******************************************************************************

      subroutine ssts2restore2(timevalue)

c description:
c ------------------------------------------------------------------------------
c set file-pointers to position AFTER reading the timestep ts data
c

c declaration section
c ------------------------------------------------------------------------------
      use m_sts2

      implicit none


c arguments
      ! integer, intent(in)  ::    ts         ! time step number
      double precision, intent(in)  :: timevalue     ! start date/time of current time step


c local variables
      integer   ilun
      integer   ts

      integer, pointer ::  filedat(:), filepsb(:), filepse(:)

      type(sts_luninfo), pointer :: pluninfo

      character filename*1024

c functions
      integer  cfn_length

c program section
c ------------------------------------------------------------------------------

c check or sts is in effect
      if (.not. usests) return


c find idices for time values
      call ssts2timeposition(timevalue,ts,.true.)


c for each lun
      if (restorets) then
      write(*,*) ' ***** State Save restore2: ',ts

         do ilun=1,nluns
            ! use pointer
            pluninfo => luninfo(ilun)%pluninfo
            filedat  => pluninfo%filedat
            filepsb  => pluninfo%filepsb
            filepse  => pluninfo%filepse

      write(*,*) pluninfo%lun,pluninfo%res2
            if (pluninfo%res2.eq.p_res2_yes) then
               ! restore file position
               if (filepse(ts).ge.0) then
                  call osd_fseek(pluninfo%lun,filepse(ts),0)
              !    write(*,*) '       lun: ',pinfo%lun,pinfo%filepos(ts)
               write(*,'(6x,a,5i8)') 'restore lun,pse,dat,res2,ts:',
     1                 pluninfo%lun,filepse(ts),filedat(ts),
     1                                   pluninfo%res2,ts
               else
                  inquire(unit=pluninfo%lun,name=filename)
                  write(*,'(3a,i4,/,9x,a,i10,/,9x,a)')
     1                ' WARNING sts_restore2: need to restore file ',
     1                filename(1:cfn_length(filename)),', unit ',
     1                                               pluninfo%lun,
     1                'at an unknown fileposition ',filepse(ts),
     1                'File pointer will be restored to position 0'
                  call osd_fseek(pluninfo%lun,0,0)
               !   call exit(13)
               endif
               ! reset res2
               pluninfo%res2=p_res2_no
            endif

         enddo
      endif

      ! reset restorets
      restorets=.false.


c end of program
      return
      end

c ******************************************************************************

      subroutine sts2saverestore(timevalue,stsave,strestore,phase)

c description:
c ------------------------------------------------------------------------------
c state save/restore
c - argument strestore has no function yet, may be removed later on
c - Depending on 'timevalue' this routines determines itself or a StateRestore
c   has to be done (When timevalue<=lastSavedTime)
c - when stsave==TRUE state save will be don, unless a stateRestore is executed

c declaration section
c ------------------------------------------------------------------------------
      use m_sts2

      implicit none


c arguments
      double precision, intent(in)  :: timevalue     ! start date/time of current time step
      logical, intent(in)     :: stsave        ! .true.  save    state
      logical, intent(out)    :: strestore     ! .true.  restore state
      integer, intent(in)     :: phase         ! 1: at begin of timestep loop
                                               ! 2: at end   of timestep loop

c local variables
c      integer :: timestep
      logical  restore


c program section
c ------------------------------------------------------------------------------

c check or sts is in effect
      if (.not. usests) return


c check or restore is necessary
      if (phase.eq.1) call ssts2nextts(timevalue)


c save if wanted
      write(*,*) ' ===== stsave   : ',stsave
      write(*,*) ' ===== restore  : ',restorets
      write(*,*) ' ===== timevalue: ',timevalue
      write(*,*) ' ===== phase    : ',phase
      if (stsave .and. .not.restorets) then
         if (phase.eq.1) currenttime = timevalue
         call ssts2save(timevalue,phase)
      end if

c restore
      if (restorets) then
         ! select phase
         if (phase.eq.1) then
            ! phase 1
            call ssts2restore1(timevalue)              ! restore timestep (1)
         else
            ! phase 2
            call ssts2restore2(timevalue)              ! restore timestep (2)
         endif
      endif

c set currentTimeStepCalculated to true
c phase 2 is at the end of the timestep loop
      if (phase.eq.2) then
         currentTimeStepCalculated=.true.
      endif


c set argument value
      strestore=restorets


c end of program
      return
      end

c ******************************************************************************

      subroutine sts2data(lun)

c description:
c ------------------------------------------------------------------------------
c save for unit lun for current timestep data has been read
c

c declaration section
c ------------------------------------------------------------------------------
      use m_sts2

      implicit none


c arguments
      integer, intent(in)    :: lun         ! logical unit number to subscribe


c program section
c ------------------------------------------------------------------------------


c check or sts is in effect
      if (.not. usests) return


c set value
      call ssts2setdata(lun,p_data)


c end of program
      return
      end


c ******************************************************************************

      subroutine sts2nodata(lun)

c description:
c ------------------------------------------------------------------------------
c save for unit lun for current timestep data has not been read
c

c declaration section
c ------------------------------------------------------------------------------
      use m_sts2

      implicit none


c arguments
      integer, intent(in)    :: lun         ! logical unit number to subscribe


c program section
c ------------------------------------------------------------------------------


c check or sts is in effect
      if (.not. usests) return


c set value
      call ssts2setdata(lun,p_datano)


c end of program
      return
      end

c ******************************************************************************

      subroutine ssts2setdata(lun,value)

c description:
c ------------------------------------------------------------------------------
c save for unit lun for current timestep data has been read or not
c

c declaration section
c ------------------------------------------------------------------------------
      use m_sts2

      implicit none


c arguments
      integer, intent(in)    :: lun         ! logical unit number to subscribe
      integer, intent(in)    :: value       ! value to set to filedat()


c local variables
      integer   ilun

      integer   currentts

      integer, pointer   :: filedat(:)
      type(sts_luninfo), pointer :: pluninfo


c program section
c ------------------------------------------------------------------------------


c only save data when currentts>0
      call ssts2timeposition(currenttime,currentts,.true.)
      if (currentts.le.0) return


c check or lun is defined
      call ssts2getilun(lun,ilun)


c set data
      if (ilun.gt.0) then

         ! increase strucure
         call ssts2increaseStructures(currentts,0)


         ! check res2,
         !     if res2 is set to p_res2_yes then do not change the
         !     value of filedat()
         !     reason: this unit number is in a restore state and gets data from a different
         !             timestep then the current timestep
         pluninfo => luninfo(ilun)%pluninfo
         filedat  => pluninfo%filedat
         if (pluninfo%res2.ne.p_res2_yes) then
            ! set data
            ! see m_sts2 for the defined order of 'value'
            filedat(currentts)=max(value,filedat(currentts))
         endif

      endif


c end of program
      return
      end

c ******************************************************************************

      subroutine ssts2increaseStructures(ts,nlun)

c description:
c ------------------------------------------------------------------------------
c increase arrays of lunifo and tsinfo structures to be able to hold timestep ts and
c number of nlun luns
c

c declaration section
c ------------------------------------------------------------------------------

      use m_sts2

      implicit none


c arguments
      integer, intent(in)     :: ts            ! minimum number of time steps in structures
      integer, intent(in)     :: nlun          ! minimum number of luns in structures


c local variables
      integer   nmxluns,nmxts,i,ilun,its

      integer, pointer, save ::  filepsb(:), filepse(:), filedat(:)
      integer, pointer       :: tfilepsb(:),tfilepse(:),tfiledat(:)

      type(sts_luninfo), pointer :: pluninfo
c      type(sts_tsinfo) , pointer :: ptsinfo

      type(stsptsinfo) , pointer, save  :: ttsinfo(:)    ! size: mxts
      type(stspluninfo), pointer, save  :: tluninfo(:)   ! size: mxluns


c program section
c ------------------------------------------------------------------------------


c calculate wanted sizes of arrays (at least 10 elements)
      nmxluns=max(10,nlun)
      nmxts  =max(10,ts  )
      if (nmxluns.gt.mxluns) nmxluns=max(nmxluns,int(1.5*mxluns))
      if (nmxts  .gt.mxts  ) nmxts  =max(nmxts  ,int(1.5*mxts  ))


c check number of units
      if (nmxluns.gt.mxluns) then
         ! increase luninfo()

         ! allocate temporary pointer
         allocate(tluninfo(nmxluns))

         ! if luninfo already allocated then move data
         if (associated(luninfo)) then
            do ilun=1,nluns
               tluninfo(ilun)%pluninfo => luninfo(ilun)%pluninfo
               nullify(luninfo(ilun)%pluninfo)
            enddo
            nullify(luninfo)
         endif

         ! associate luninfo to temporary pointer
         luninfo => tluninfo
         nullify(tluninfo)

         ! new number of mxluns
         mxluns=nmxluns
      endif


c check array size and increase when necessary
      if (nmxts.gt.mxts) then
         ! array too short, reallocate


         ! reallocate tsinfo
         allocate(ttsinfo(nmxts))

         ! if tsinfo already allocated then move data
         if (associated(tsinfo)) then
            do its=1,nts
               ttsinfo(its)%ptsinfo => tsinfo(its)%ptsinfo
               nullify(tsinfo(its)%ptsinfo)
            enddo
         endif
         do its=nts+1,nmxts
            allocate(ttsinfo(its)%ptsinfo)
         enddo

         ! associate tsinfo to temporary pointer
         tsinfo => ttsinfo
         nullify(ttsinfo)


         ! reallocate luninfo
         do ilun=1,nluns
            ! use pointer
            pluninfo => luninfo(ilun)%pluninfo

            ! allocate temp array
            allocate(filepsb(nmxts),filepse(nmxts),filedat(nmxts))

            ! copy data
            tfilepsb => pluninfo%filepsb
            tfilepse => pluninfo%filepse
            tfiledat => pluninfo%filedat
            do i=1,mxts
               filepsb(i)=tfilepsb(i)
               filepse(i)=tfilepse(i)
               filedat(i)=tfiledat(i)
            enddo
            ! fill new part with 'nodata'
            do i=mxts+1,nmxts
               filepsb(i)=-1
               filepse(i)=-1
               filedat(i)=p_dataunk
            enddo

            ! move arrays
            deallocate(pluninfo%filepsb)
            deallocate(pluninfo%filepse)
            deallocate(pluninfo%filedat)
            pluninfo%filepsb => filepsb
            pluninfo%filepse => filepse
            pluninfo%filedat => filedat
            nullify(filepsb)
            nullify(filepse)
            nullify(filedat)
         enddo

         ! assign new size
         mxts=nmxts
      endif


c end of program
      return
      end

c ******************************************************************************

      subroutine ssts2getilun(lun,ilun)

c description:
c ------------------------------------------------------------------------------
c find position in luninfo structure of lun
c

c declaration section
c ------------------------------------------------------------------------------

      use m_sts2

      implicit none


c arguments
      integer, intent(in)    :: lun         ! logical unit number to find
      integer, intent(out)   :: ilun        !  >0: position in luninfo structure
                                            ! <=0: lun not defined yet


c local variables
      integer   i


c program section
c ------------------------------------------------------------------------------

c find ilun
      ! first try last-ilun position
      ilun=lastilun
      ! fast search
      if (ilun.gt.0 .and. ilun.le.nluns) then
         if (luninfo(ilun)%pluninfo%lun.ne.lun) then
            ilun=0
         endif
      else
         ilun=0
      endif

      ! slow search
      if (ilun.le.0) then
         do i=1,nluns
            if (luninfo(i)%pluninfo%lun.eq.lun) then
               ilun=i
               exit
            endif
         enddo
      endif


c set lastilun
      lastilun=ilun


c end of program
      return
      end


c ******************************************************************************

      subroutine sts2dump(lun)

c description:
c ------------------------------------------------------------------------------
c write the contents of the data structure to standard output
c

c declaration section
c ------------------------------------------------------------------------------

      use m_sts2

      implicit none


c arguments
      integer, intent(in)     :: lun       ! <=0: all units are displayed
                                           !  >0: only unit lun will be displayed


c local variables
      integer   i,j,k,n

      integer   currentts

      type(sts_luninfo), pointer :: pluninfo

c program section
c ------------------------------------------------------------------------------

c check or sts is in effect
      if (.not. usests) return

c find idices for time values
      call ssts2timeposition(currenttime,currentts,.true.)


c dump
      do i=1,nluns
         pluninfo => luninfo(i)%pluninfo

         if (lun.le.0 .or. lun.eq.pluninfo%lun) then
            write(*,'(/,i4,'':'')') pluninfo%lun
            do j=1,currentts,10
               n=min(j+9,currentts)
               write(*,'(i5,a,i5)') j,'...',n
               write(*,'(''  dat'',10I8)') (pluninfo%filedat(k),k=j,n)
               write(*,'(''  psb'',10I8)') (pluninfo%filepsb(k),k=j,n)
               write(*,'(''  pse'',10I8)') (pluninfo%filepse(k),k=j,n)
            enddo
         endif

      enddo


c end of program
      return
      end

c ******************************************************************************

      subroutine sts2test(cla,stsave,strestore,timevalue,
     1                    ts1,ts2,tsstep,deltats,option)

c description:
c ------------------------------------------------------------------------------
c routine to test save/restore
c option 1: init test save/restore for timestep 1
c        2: determine action for NEXT timestep (timestep+1)

c declaration section
c ------------------------------------------------------------------------------

      implicit none


c arguments
      character (len=*), intent(in)    :: cla               ! command line argument of sts
      logical, intent(inout)           :: stsave            ! state save
      logical, intent(inout)           :: strestore         ! state restore
      double precision, intent(in)  :: timevalue     ! start date/time of current time step
      ! integer, intent(in)              :: timestep          ! current timestep
      integer, intent(inout)           :: ts1,ts2,tsstep    ! save/restore/step
      integer, intent(inout)           :: deltats           ! delta timestep for next timestep
      integer, intent(in)              :: option            ! option


c local variables
      integer         :: tsplus1,ios,timestep
      logical, save   :: usedummy=.true.


c program section
c ------------------------------------------------------------------------------

c check usage of routine
      if (.not.usedummy) return


c find idices for time values
      call ssts2timeposition(timevalue  ,timestep,.true.)


c execute action
      tsplus1=timestep+1
      select case( option )

         case( 1 )
            ! init test save/restore
            if (cla(1:5).eq.'test') then
               ! set parameters for test save/restore
               write(*,*) ' MESSAGE: STS-test switched on.'
               ! try to read data from cla
               read(cla(6:),*,iostat=ios) ts1,tsstep
               if (ios.ne.0) then
                  tsstep=5
                  ts1=1
               endif
               ts2=ts1
               if (timestep.eq.ts1) then
                  stsave   =.true.
                  ts2      =ts2+tsstep
               !   strestore=.false.
               endif
            else
               ! don't use this routine anymore
               usedummy=.false.
            endif

         case( 2 )
            ! check what to do
            if (timestep.eq.ts2) then
               ! when timestep==ts2: restore
               !stsave   =.false.
               strestore=.true.
               deltats  =min(deltats,ts1-timestep)  ! restore data at ts1
               ts1      =ts1+tsstep
            else
               if (tsplus1.eq.ts1) then
                  ! when tsplus1==ts1: save
                  stsave   =.true.
                  !strestore=.false.
                  ts2      =ts2+tsstep
               endif
            endif

         case default
            !
            write(*,*) ' ERROR: unknown option in sts_test: ',option
            call exit(10)

      end select
      write(*,'(a,2l4,5i8)')
     1 ' STS TEST: stsave,strestore,timestep,ts1,ts2,tsstep,deltats:',
     1             stsave,strestore,timestep,ts1,ts2,tsstep,deltats

c end of program
      return
      end

c ******************************************************************************

      subroutine ssts2timeposition(timevalue,timestep,new)

c description:
c ------------------------------------------------------------------------------
c

c declaration section
c ------------------------------------------------------------------------------
      use m_sts2

      implicit none


c arguments
      double precision, intent(in)  :: timevalue            !> time value
      integer, intent(out)          :: timestep             !> timestep index
                                                            !!    >0: index number
                                                            !!   <=0: not found

      logical, intent(in) :: new

c local variables
      integer i
      double precision d


c functions
      double precision cfn_mjd_delta


c program section
c ------------------------------------------------------------------------------


c find index number for timevalue
      timestep=0
      do i=1,nts
         d=cfn_mjd_delta(timevalue,tsinfo(i)%ptsinfo%timevalue)
         write(*,'(a,4(1x,f15.7))') 'd,timevalue,timevalue(),i',d,
     1timevalue,tsinfo(i)%ptsinfo%timevalue,i
         if (d.eq.0.d0) then
            timestep=i
            exit
         endif
      enddo
      ! may be a new number needed
      if (timestep.eq.0 .and. new) then
         if (nts.le.0) then
            ! first usage of these data structures
            timestep=1
         else
            if (timevalue.gt.tsinfo(nts)%ptsinfo%timevalue) then
               ! new index
               timestep=nts+1
            endif
         endif
      endif

c end of program
      return
      end

c ******************************************************************************

      subroutine sts2getlun(lun)

c description:
c ------------------------------------------------------------------------------
c get unit number for storage of state-save data as defined by sts-routines
c

c declaration section
c ------------------------------------------------------------------------------
      use m_sts2, only: usests,stslun

      implicit none


c arguments
      integer, intent(out)          :: lun             !> unit number


c program section
c ------------------------------------------------------------------------------

c get unit number
      if (usests) then
         lun=stslun
      else
         lun=-1
      endif


c end of program
      return
      end
