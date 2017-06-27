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

      !> Module for the coupling MODFLOW-MetaSWAP with MOZART.
      module mozartmod

      implicit none

      logical, save :: first = .true.

      character(len=100), save :: mzsignal         !< MOZART signal file
      character(len=100), save :: mfmssignal       !< MODFLOW-MetaSWAP signal file
      character(len=100), save :: mzexitsignal     !< MOZART signal file indicating MOZART has finished
      character(len=100), save :: lsw_file         !< File with LSW IDs
      character(len=100), save :: pv_file          !< File with PV IDs
      character(len=100), save :: mzlevels_file    !< MOZART levels file (in)
      character(len=100), save :: mzfractions_file !< MOZART fractions file (in)
      character(len=100), save :: mzsalt_file      !< MOZART salt file (in)
      character(len=100), save :: mms_dmnds_file   !< MOZART demand file (out)
      character(len=100), save :: mms_alloc_file   !< MOZART allocation file (out)

!...     Parameters
      integer, parameter :: tsleep  = 1 !< time in seconds to check coupling signal file
      integer, parameter :: ntry = 10

      double precision :: beginOfCurrentTimeStep, endOfCurrentTimeStep

      real, parameter :: mv = -1234.0 !< general missing value

      integer, parameter :: maxlswid = 1000000 !< maximum ID of the LSWs
      integer, save :: nlsw !< number of LSWs
      type tLSW
         integer :: lswid
         real :: lev
         real :: frc
         real :: salt
         real :: cuflroff
         real :: cuflron
         real :: cufldr
         real :: cufldrwells
         real :: cuflif
         real :: cuflifwells
         real :: cuflsp
         real :: cusalt
         real :: cusaltwells
         real :: cuseep
         real :: cuunszflux
      end type tLSW
      type (tLSW), dimension(:), allocatable, save :: lswdat !< Local Surface Water array
      integer, dimension(:), allocatable, save :: lsw

      integer, parameter :: maxpvid = 1000000 !< maximum ID of the LSWs
      integer, save :: npv !< number of LSWs
      type tPV
         integer :: pvid
         real :: lev
      end type tPV
      type (tPV), dimension(:), allocatable, save :: pvdat !< Local Surface Water array
      integer, dimension(:), allocatable :: pv

      real, parameter :: concrch = 20.

      real, dimension(:), allocatable :: lswsbuf, lswrbuf
      
      integer, parameter :: minlun = 10
      integer, parameter :: maxlun = 9999
      
      end module

! =============================================================================
     !> Initialize MOZART: read command line
      subroutine mozart_initComponent(cla_mozart)
! =============================================================================
!...     modules
      use mozartmod

      implicit none

!...     arguments
      character (len=*), intent(in) :: cla_mozart !< command line for MOZART

!...     functions
      integer :: cfn_n_elem

!...     local variables
      character(len=50), dimension(:), allocatable :: str
      integer :: iarg, nargs
!.......................................................................

      write(*,*) 'mz: *** initialization ***'
      write(*,*) 'mz: reading command line arguments'

      nargs = cfn_n_elem(' ',1,cla_mozart)

      if (nargs.eq.0) then
         write(*,*) 'mz: error, mozart commandline arguments not found!'
         call exit(1)
      end if
      allocate(str(nargs))
      read(cla_mozart,*)(str(iarg),iarg=1,nargs)

      read(str(1),*) mzsignal         ! read MOZART signal file name
      read(str(2),*) mfmssignal       ! read MODFLOW-MetaSWAP signal file name
      read(str(3),*) mzexitsignal     ! read MOZART signal file if finished
      read(str(4),*) lsw_file
      read(str(5),*) pv_file
      read(str(6),*) mzlevels_file    ! read MOZART levels file name
      read(str(7),*) mzfractions_file ! read MOZART fractions file name
      read(str(8),*) mzsalt_file      ! read MOZART salt file name
      read(str(9),*) mms_dmnds_file   ! read MOZART output demand file
      read(str(10),*) mms_alloc_file   ! read MOZART output alloc file
      deallocate(str)

      end subroutine

! =============================================================================
      !> Initialize MOZART: allocate arrays
      subroutine mozart_initSimulation()
! =============================================================================
!...     modules
      use mozartmod

      implicit none
!.......................................................................

      call timing_tic('MOZART','initSimulation')
      
!...     read file with LSW's
      call read_lsw()

!...     read file with PV
      call read_pv()

      call timing_toc('MOZART','initSimulation')

      end subroutine

      !> This subroutine reads the LSW's
      subroutine read_lsw()

      use mozartmod

      implicit none

!...     functions
      integer :: cfn_getlun

!...     local variables
      character(len=100) :: str
      logical :: lex
      integer :: lun, ios, i, n,lswid
!.......................................................................

      write(*,*) 'mz: reading LSW file (',trim(lsw_file),')'

!...     Open mftolsw.csv
      lun = cfn_getlun(minlun,maxlun)
      inquire(file=lsw_file,exist=lex)
      if (lex) then
         open(unit=lun,file=lsw_file,status='old',form='formatted',share='denynone')
      else
         write(*,*) 'mz: error, file ',trim(lsw_file),' does not exist'
         call exit(1)
      end if

!....    determine number of records (ncell)
      nlsw = 0
      do while (.true.)
         read(unit=lun,fmt=*,iostat=ios) lswid
         if (ios.ne.0) exit
         nlsw = nlsw + 1
      end do
      rewind(lun)

!...     allocate the lsw structure
      if (nlsw.gt.0) then
         allocate(lsw(nlsw),lswdat(maxlswid))
      else
         write(*,*) 'Error reading lsw.csv'; stop 1
      end if
      lswdat(:)%lswid = 0

      nlsw = 0
      do while (.true.)
         read(unit=lun,fmt=*,iostat=ios) lswid
         if (ios.ne.0) exit
         nlsw = nlsw + 1
         lsw(nlsw) = lswid
         lswdat(lswid)%lswid = lswid
      end do

      close(lun)

      end subroutine

      !> This subroutine reads the PV's
      subroutine read_pv()

      use mozartmod

      implicit none

!...     functions
      integer :: cfn_getlun

!...     local variables
      character(len=100) :: str
      logical :: lex
      integer :: lun, ios, i, n, pvid
!.......................................................................

      write(*,*) 'mz: reading PV file (',trim(pv_file),')'

!...     Open mftolsw.csv
      lun = cfn_getlun(minlun,maxlun)
      inquire(file=pv_file,exist=lex)
      if (lex) then
         open(unit=lun,file=pv_file,status='old',form='formatted',share='denynone')
      else
         write(*,*) 'mz: error, file ',trim(pv_file),' does not exist'
         call exit(1)
      end if

!....    determine number of records (ncell)
      npv = 0
      do while (.true.)
         read(unit=lun,fmt=*,iostat=ios) pvid
         if (ios.ne.0) exit
         npv = npv + 1
      end do
      rewind(lun)

!...     allocate the lsw structure
      if (npv.gt.0) then
         allocate(pv(npv), pvdat(maxpvid))
      else
         write(*,*) 'Error reading pv.csv'; stop 1
      end if
      pvdat(:)%pvid = 0

      npv = 0
      do while (.true.)
         read(unit=lun,fmt=*,iostat=ios) pvid
         if (ios.ne.0) exit
         npv = npv + 1
         pv(npv) = pvid
         pvdat(pvid)%pvid = pvid
      end do

      close(lun)

      end subroutine

! =============================================================================
      subroutine mozart_prepareTimeStep(endOfSimulation,convergedMozart,currentTime)
! =============================================================================
!...     modules
      use mozartmod
      use pksmpi_mod, only: myrank

      implicit none

!...     arguments
      logical, intent(inout) :: endOfSimulation
      logical, intent(out) :: convergedMozart
      double precision, intent(inout) :: currentTime

!...     functions
      integer :: cfn_getlun

!...     locals
      logical :: lcont, lexit
      character(len=1)  :: dora
      character(len=15) :: time
      integer :: lun, ios, date, year, month, day, hour, minute, second, itry
      double precision :: dt
!.......................................................................

      call timing_tic('MOZART','prepareTimeStep')

!...     wait until the MOZART signal file is found
      write(*,*) 'mz: waiting for signal from mozart ',mzsignal(1:len_trim(mzsignal)), ' or ',&
                                mzexitsignal(1:len_trim(mzexitsignal)),' to be found ...'
      do while(.true.)
         inquire(file=mzsignal,exist=lcont)
         inquire(file=mzexitsignal,exist=lexit)
         if (lcont.or.lexit) exit
         call sleep(tsleep)
      end do

      endOfSimulation = .false.
      if (lexit) then
         write(*,*) 'mz: found ',mzexitsignal(1:len_trim(mzexitsignal))
         write(*,*) 'mz: mozart finished simulation'
         endOfSimulation = .true.
         call timing_toc('MOZART','prepareTimeStep')
         return
      end if

 !...     open the MOZART signal file, read, and delete file
      write(*,*) 'mz: found ',mzsignal(1:len_trim(mzsignal)),' and reading'
      lun = cfn_getlun(minlun,maxlun)
      open(unit=lun,file=mzsignal,status='old',form='formatted',iostat=ios,share='denynone')
      if (ios.ne.0) then
         itry = 0
         do while(itry.le.ntry)
            write(*,*) 'Error opening file, retrying...'
            call sleep(tsleep)   
            open(unit=lun,file=mzsignal,status='old',form='formatted',iostat=ios,share='denynone')
            if (ios.eq.0) then
               exit
            else   
               itry = itry + 1
            end if   
         end do
      end if            
      if (ios.ne.0) then
         write(*,*) 'Error opening file'; stop 1
      end if

!...     read MOZART signal file
      read(lun,*) dora, time, dt
      write(*,*) 'mz: read from signal file: ', dora, time, dt

!...     delete signal file
      if (myrank.ne.0) then ! PKS
         close(lun)
      end if ! PKS                                       
      call pks7mpibarrier() ! PKS
      if (myrank.eq.0) then ! PKS
         close(lun,status='delete')
      end if ! PKS                                       

!...     read time information
      read(time,'(i4,2i2)') year, month, day
      date = year*10000 + month*100 + day
      hour   = 0
      minute = 0
      second = 0
      call cfn_datehms2mjd(date,hour,minute,second,beginOfCurrentTimeStep)
      endOfCurrentTimeStep = beginOfCurrentTimeStep + dt

      if (dora.eq.'d' .or. dora.eq.'D') then
         convergedMozart = .true.
      else
         convergedMozart = .false.
      end if
      if (first) then
         convergedMozart = .false.
         first = .false.
      end if
      
      call timing_toc('MOZART','prepareTimeStep')

      end subroutine

! =============================================================================
      subroutine mozart_initTimeStep(iter)
! =============================================================================
      use mozartmod, only: lswdat

      implicit none

!...     arguments
      integer, intent(in) :: iter
!.......................................................................

      call timing_tic('MOZART','initTimeStep')
      
!...     initalize the arrays
      call mozart_init_lsw()

!...     read MOZART levels
      call mozart_read_levels()

!...     read MOZART fractions
      if (iter.eq.2) then
         call mozart_read_fractions()
      else
         lswdat(:)%frc = -9999.0
      end if

!...     read MOZART salt file
      call mozart_read_salt()

      call timing_toc('MOZART','initTimeStep')
      
      end subroutine

       !> Read MOZART levels.
      subroutine mozart_read_levels()

      use mozartmod, only: mzlevels_file, lswdat, pvdat, maxlswid, maxpvid, mv, minlun, maxlun

      implicit none

!...     functions
      integer :: cfn_getlun

!...     local variables
      integer :: lun, lswid, pvid
      real    :: lev
!.......................................................................

      write(*,*) 'mz: reading mozart levels (',trim(mzlevels_file),')'

      lun = cfn_getlun(minlun,maxlun)
      open(unit=lun,file=mzlevels_file,status='old',form='formatted',share='denynone')

      lswdat(:)%lev = mv
      pvdat(:)%lev = mv
      read(lun,*)
      do while (.not. eof(lun))
         read(lun,*) lswid, pvid, lev
         if (lswid.le.0 .or. lswid.gt.maxlswid) then
            write(*,*) lswid, maxlswid
            write(*,*) 'Error reading mzlevels.mms'
            stop 1
         end if
         if (pvid.gt.maxpvid) then
            write(*,*) pvid, maxpvid
            write(*,*) 'Error reading mzlevels.mms'
            stop 1
         end if
         if (pvid.eq.0) then
            if (lswdat(lswid)%lswid.gt.0) lswdat(lswid)%lev = lev
         else
            if (pvdat(pvid)%pvid.gt.0) pvdat(pvid)%lev = lev
         end if
      end do
      close(lun)

      end subroutine

      !> Read MOZART fractions.
      subroutine mozart_read_fractions()

      use mozartmod, only: mzfractions_file, lswdat, maxlswid, mv, minlun, maxlun

      implicit none

!...     functions
      integer :: cfn_getlun

!...     local variables
      integer :: lun, lswid
      real    :: frc
!.......................................................................

      write(*,*) 'mz: reading mozart levels (',trim(mzfractions_file),')'

      lun = cfn_getlun(minlun,maxlun)
      open(unit=lun,file=mzfractions_file,status='old',form='formatted',share='denynone')

      ! read fractions
      lswdat(:)%frc = mv
      read(lun,*)
      do while (.not. eof(lun))
         read(lun,*) lswid, frc
         if (lswid.le.0 .or. lswid.gt.maxlswid) then
            write(*,*) 'Error reading mzfractions.mms'
            stop 1
         end if
         if (lswdat(lswid)%lswid.gt.0) lswdat(lswid)%frc = frc
      end do
      close(lun)

      end subroutine

       !> Read Salt.
      subroutine mozart_read_salt()

      use mozartmod, only: mzsalt_file, lswdat, maxlswid, mv, minlun, maxlun

      implicit none

!...     functions
      integer :: cfn_getlun

!...     local variables
      integer :: lun, lswid
      real    :: salt
!.......................................................................

      write(*,*) 'mz: reading mozart salt (',trim(mzsalt_file),')'

      lun = cfn_getlun(minlun,maxlun)
      open(unit=lun,file=mzsalt_file,status='old',form='formatted',share='denynone')

      ! read fractions
      lswdat(:)%salt = mv
      read(lun,*)
      do while (.not. eof(lun))
         read(lun,*) lswid, salt
         if (lswid.le.0 .or. lswid.gt.maxlswid) then
            write(*,*) 'Error reading mzsalt.mms'
            stop 1
         end if
         if (lswdat(lswid)%lswid.gt.0) lswdat(lswid)%salt = salt
      end do
      close(lun)

      end subroutine

      !> Initialize lsw data.
      subroutine mozart_init_lsw()
!...     modules
      use mozartmod, only: lswdat, mv
!.......................................................................

      lswdat(:)%lev         = mv
      lswdat(:)%frc         = mv
      lswdat(:)%salt        = mv
      lswdat(:)%cuflroff    = mv
      lswdat(:)%cuflron     = mv
      lswdat(:)%cufldr      = mv
      lswdat(:)%cufldrwells = mv
      lswdat(:)%cuflif      = mv
      lswdat(:)%cuflifwells = mv
      lswdat(:)%cuflsp      = mv
      lswdat(:)%cusalt      = mv
      lswdat(:)%cusaltwells = mv
      lswdat(:)%cuseep      = mv
      lswdat(:)%cuunszflux  = mv

      end subroutine

! =============================================================================
      subroutine mozart_finishTimeStep(iter,dt,usetransol)
! =============================================================================
!...     modules
      use pksmpi_mod, only: myrank

      implicit none

!...     arguments
      integer, intent(in) :: iter
      double precision, intent(in) :: dt
      logical, intent(in) :: usetransol
!.......................................................................

      call timing_tic('MOZART','finishTimeStep')
      
      call mozart_reduce_lsw()
      if (myrank.eq.0) then
         if (iter.eq.1) then ! demand phase
            call mozart_write_mms(dt,'d',usetransol)
         else if (iter.eq.2) then ! allocation phase
            call mozart_write_mms(dt,'a',usetransol)
         else
             write(*,*) 'Error mozart_finishTimeStep'
             call exit(1)
         end if
         ! write the signal file
         call mozart_write_sig()
      end if
      call pks7mpibarrier() ! PKS
  
      call timing_toc('MOZART','finishTimeStep')
     
      end subroutine

      !> Write MOZART signal file.
      subroutine mozart_write_sig()

      use mozartmod

      implicit none

!...     functions
      integer :: cfn_getlun

!...     local variables
      integer :: lun, ios
      character(len=100) :: tmpfile
!.......................................................................

!...     write (empty) MODFLOW-MetaSWAP file
      write(*,*) 'mz: writing signal file for mozart (',trim(mfmssignal),')'

      lun = cfn_getlun(minlun,maxlun)
      write(tmpfile,'(2a)') mfmssignal(1:len_trim(mfmssignal)), 'tmp'
      open(unit=lun,file=tmpfile,status='new',form='formatted',share='denyrd')
      close(lun)
      call osd_rename(tmpfile,mfmssignal,ios) ! rename file
      if (ios.ne.0) write(*,*) 'Error renaming file...'

      end subroutine

      !> Write MOZART file the demand phase.
      subroutine mozart_write_mms(dt,phase,usetransol)

      use mozartmod, only: mms_dmnds_file, mms_alloc_file, nlsw, lsw, lswdat, mv, concrch, minlun, maxlun

      implicit none

!...     arguments
      double precision, intent(in) :: dt
      character(len=1), intent(in) :: phase
      logical, intent(in) :: usetransol

!...     functions
      integer :: cfn_getlun

!...     local variables
      character(len=1) :: dora
      character(len=100) :: outfile
      integer :: lun, i, lswid
      real :: sc, cufldr, cuflif, cuflroff, cuflron, cuflsp, cusalt
      real :: cufldrwells, cuflifwells, cusaltwells, cuseep, cuunszflux
      real :: conc
!.......................................................................
      dora = phase
      call cfn_token(dora ,'u')
      if (dora.eq.'D') then
         outfile = mms_dmnds_file
      else if (dora.eq.'A') then
         outfile = mms_alloc_file
      else
         write(*,*) 'Error, mozart_write_mms'
         call exit(1)
      end if
      write(*,*) 'mz: phase ',dora
      write(*,*) 'mz: writing mozart file (',trim(outfile),')'

      lun = cfn_getlun(minlun,maxlun)
      open(unit=lun,file=outfile,status='unknown',form='formatted')

!...     write header
      if (dora.eq.'D') then
         write(lun,*) 'ixLSW,cufldr,cuflif,cufldr2,cuflif2,cuflroff,cuflron,cuflsp,cuNaCl,cuNaCl2'
      else
         write(lun,*) 'ixLSW,cufldr,cuflif,cufldr2,cuflif2,cuflroff,cuflron,cuNaCl,cuNaCl2'
      end if

!...     write LSW's
      sc = real(dt)*86400. ! days --> seconds
      do i = 1, nlsw
         lswid = lsw(i)
         if (lswdat(lswid)%lswid.le.0) cycle
         cufldr      = lswdat(lswid)%cufldr
         cufldrwells = lswdat(lswid)%cufldrwells
         cuflif      = lswdat(lswid)%cuflif
         cuflifwells = lswdat(lswid)%cuflifwells
         cuflroff    = lswdat(lswid)%cuflroff
         cuflron     = 0.0
         cuflsp      = lswdat(lswid)%cuflsp
         cusalt      = lswdat(lswid)%cusalt  ! = seepage salt if no transol
         cusaltwells = lswdat(lswid)%cusaltwells
         cuseep      = lswdat(lswid)%cuseep
         cuunszflux  = lswdat(lswid)%cuunszflux

         ! temporary for wells/salt
         if (cufldrwells.eq.mv) cufldrwells = 0.0
         if (cuflifwells.eq.mv) cuflifwells = 0.0
         if (cusaltwells.eq.mv) cusaltwells = 0.0

         if (.not.usetransol) then
            if (cuseep.eq.mv .and. cuunszflux.eq.mv) then
               cusalt = 0.
            else
               if (cusalt.eq.mv)     cusalt = 0.
               if (cuseep.eq.mv)     cuseep = 0.
               if (cuunszflux.eq.mv) cuunszflux = 0.
               cuseep = -cuseep
               conc = (cusalt + concrch* cuunszflux)/(cuseep + cuunszflux) ! ([g] + [g]/[m3] * [m3])/([m3] + [m3]) = [g]/[m3]
               if (cufldr.ne.mv) cusalt = -cufldr*conc
            end if
         end if

         ! check for missing value
         if (cufldr.eq.mv .or. cuflif.eq.mv .or. cuflroff.eq.mv .or. cusalt.eq.mv) then
            !write(*,*) 'Warning: skipping LSW number ',lswid
            cycle
         end if
         if (dora.eq.'D') then
            if (cuflsp.eq.mv) then
               write(*,*) 'Error, writing MOZART demand file.'
               call exit(1)
            end if
            write(lun,'(i10,9('','',e12.5))') lswid, -cufldr/sc,      -cuflif/sc,&
                                                     -cufldrwells/sc, -cuflifwells/sc,&
                                                     -cuflroff/sc,    -cuflron/sc, -cuflsp/sc,&
                                                     cusalt, -cusaltwells
         else
            write(lun,'(i10,8('','',e12.5))') lswid, -cufldr/sc,      -cuflif/sc,&
                                                     -cufldrwells/sc, -cuflifwells/sc,&
                                                     -cuflroff/sc,    -cuflron/sc,&
                                                     cusalt, -cusaltwells
         end if
      enddo

      close(lun)

      return
      end subroutine

      !> Put the number of LSW IDs
      logical function mozart_PutModMozNumberOfIDs(nid)
!...     modules
      use mozartmod, only: nlsw

      implicit none

!...     arguments
      integer, intent(out) :: nid

!...     locals
      logical :: ok
!.......................................................................

      ok = .true.
      nid = nlsw

      mozart_PutModMozNumberOfIDs = ok

      end function

      !> Put the number of PV IDs
      logical function mozart_PutModMozPVNumberOfIDs(nid)
!...     modules
      use mozartmod, only: npv

      implicit none

!...     arguments
      integer, intent(out) :: nid

!...     locals
      logical :: ok
!.......................................................................

      ok = .true.
      nid = npv

      mozart_PutModMozPVNumberOfIDs = ok

      end function

      !> Put LSW IDs
      logical function mozart_PutModMozIDs(ids,nid)
!...     modules
      use mozartmod, only: lsw

      implicit none

!...     arguments
      integer, intent(in) :: nid
      integer, dimension(nid), intent(out) :: ids

!...     locals
      logical :: ok
!.......................................................................

      ok = .true.
      ids = lsw

      mozart_PutModMozIDs = ok

      end function

      !> Put PV IDs
      logical function mozart_PutModMozPVIDs(ids,nid)
!...     modules
      use mozartmod, only: pv

      implicit none

!...     arguments
      integer, intent(in) :: nid
      integer, dimension(nid), intent(out) :: ids

!...     locals
      logical :: ok
!.......................................................................

      ok = .true.
      ids = pv

      mozart_PutModMozPVIDs = ok

      end function

      !> Put MOZART levels for LSWs
      logical function mozart_PutLSWLevels(levels,mvin)

      use mozartmod, only: lsw, lswdat, nlsw, mv

      implicit none

!...     arguments
      real, dimension(*), intent(out) :: levels
      real, intent(in) :: mvin

!...     locals
      logical :: ok
      integer :: i, lswid
      real :: lev
!......................................................................

      do i = 1, nlsw
          lswid = lsw(i)
          lev = mvin
          if (lswdat(lswid)%lswid.gt.0) then
             if (lswdat(lswid)%lev.ne.mv) then
                lev = lswdat(lswid)%lev
             end if
          end if
          levels(i) = lev
      end do
      ok = .true.
      mozart_PutLSWLevels = ok

      end function

      !> Put MOZART levels for LSWs
      logical function mozart_PutPVLevels(levels,mvin)

      use mozartmod, only: pv, pvdat, npv, mv

      implicit none

!...     arguments
      real, dimension(*), intent(out) :: levels
      real, intent(in) :: mvin

!...     locals
      logical :: ok
      integer :: i, pvid
      real :: lev
!......................................................................

      do i = 1, npv
          pvid = pv(i)
          lev = mvin
          if (pvdat(pvid)%pvid.gt.0) then
             if (pvdat(pvid)%lev.ne.mv) then
                lev = pvdat(pvid)%lev
             end if
          end if
          levels(i) = lev
      end do
      ok = .true.
      mozart_PutPVLevels = ok

      end function

      !> Put MOZART fractions for LSWs
      logical function mozart_PutLSWFractions(fractions,mvin)

      use mozartmod, only: lsw, lswdat, nlsw, mv

      implicit none

!...     arguments
      real, dimension(*), intent(out) :: fractions
      real, intent(in) :: mvin

!...     locals
      logical :: ok
      integer :: i, lswid
      real :: frc
!......................................................................

      do i = 1, nlsw
          lswid = lsw(i)
          frc = mvin
          if (lswdat(lswid)%lswid.gt.0) then
             if (lswdat(lswid)%frc.ne.mv) then
                frc = lswdat(lswid)%frc
             end if
          end if
          fractions(i) = frc
      end do
      ok = .true.
      mozart_PutLSWFractions = ok

      end function

      !> Put MOZART salt concentrations for LSWs
      logical function mozart_PutLSWSalt(salt,mvin)

      use mozartmod, only: lsw, lswdat, nlsw, mv

      implicit none

!...     arguments
      real, dimension(*), intent(out) :: salt
      real, intent(in) :: mvin

!...     locals
      logical :: ok
      integer :: i, lswid
      real :: cnc
!......................................................................

      do i = 1, nlsw
          lswid = lsw(i)
          cnc = mvin
          if (lswdat(lswid)%lswid.gt.0) then
             if (lswdat(lswid)%salt.ne.mv) then
                cnc = lswdat(lswid)%salt/1000. ! g/m3 --> kg/m3
             end if
          end if
          salt(i) = cnc
      end do
      ok = .true.
      mozart_PutLSWSalt = ok

      end function

      logical function MOZART_GetSalt(salt,nid,xchIdx,xchOff,mvin,iact)
!...     modules
      use mozartmod, only: mv, lsw, lswdat

      implicit none

!...     arguments
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: salt
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin
      integer, intent(in)                 :: iact

!...     locals
      logical :: ok
      integer :: i, j, js, je, k, lswid
      real :: sval
!.......................................................................
      ok = .true.
      if (iact.ne.1 .and. iact.ne.2 .and. iact.ne.3) then
         ok = .false.
         MOZART_GetSalt = ok
         return
      end if

      if (iact.eq.1) lswdat(:)%cusalt = mv

      js = 1
      do i = 1, nid
         lswid = lsw(i)
         je = xchOff(i)
         do j = js, je
            k = xchIdx(j)
            sval = salt(k)
            if (sval.ne.mvin) then
               if (iact.eq.1) then
                  if (lswdat(lswid)%cusalt.eq.mv) lswdat(lswid)%cusalt = 0.0
                  lswdat(lswid)%cusalt = lswdat(lswid)%cusalt + sval*1000. ! [g]
               else if (iact.eq.2) then
                  if (lswdat(lswid)%cusaltwells.eq.mv) lswdat(lswid)%cusaltwells = 0.0
                  lswdat(lswid)%cusaltwells = lswdat(lswid)%cusaltwells + sval ! [g]
               else if (iact.eq.3) then
                  if (lswdat(lswid)%cusalt.eq.mv) lswdat(lswid)%cusalt = 0.0
                  lswdat(lswid)%cusalt = lswdat(lswid)%cusalt + sval ! [g]
               end if
            end if
         end do
         js = je + 1
      end do

      MOZART_GetSalt = ok
      end function


     logical function mozart_GetSeepageFlux(flux,nid,xchIdx,xchOff,mvin)
!...     modules
      use mozartmod, only: mv, lsw, lswdat

      implicit none

!...     arguments
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: flux
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin

!...     locals
      logical :: ok
      integer :: i, j, js, je, k, lswid
      real :: fluxval
!.......................................................................
      ok = .true.

      js = 1
      do i = 1, nid
         lswid = lsw(i)
         je = xchOff(i)
         do j = js, je
            k = xchIdx(j)
            fluxval = flux(k)
            if (fluxval.ne.mvin) then
               if (fluxval.lt.0.) then
                  if (lswdat(lswid)%cuseep.eq.mv) lswdat(lswid)%cuseep = 0.0
                  lswdat(lswid)%cuseep = lswdat(lswid)%cuseep + fluxval
               end if
            end if
         end do
         js = je + 1
      end do

      mozart_GetSeepageFlux = ok
      end function


     logical function MOZART_GetUnsaturatedZoneFlux(flux,nid,xchIdx,xchOff,mvin)
!...     modules
      use mozartmod, only: mv, lsw, lswdat

      implicit none

!...     arguments
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: flux
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin

!...     locals
      logical :: ok
      integer :: i, j, js, je, k, lswid
      real :: fluxval
!.......................................................................
      ok = .true.

      js = 1
      do i = 1, nid
         lswid = lsw(i)
         je = xchOff(i)
         do j = js, je
            k = xchIdx(j)
            fluxval = flux(k)
            if (fluxval.ne.mvin) then
               if (fluxval.gt.0.) then
                  if (lswdat(lswid)%cuunszflux.eq.mv) lswdat(lswid)%cuunszflux = 0.0
                  lswdat(lswid)%cuunszflux = lswdat(lswid)%cuunszflux + fluxval
               end if
            end if
         end do
         js = je + 1
      end do

      MOZART_GetUnsaturatedZoneFlux = ok
      end function


      logical function mozart_GetRiverDrainFlux(flux,nid,xchIdx,xchOff,mvin,iact)
!...     modules
      use mozartmod, only: mv, lsw, lswdat

      implicit none

!...     arguments
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: flux
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin
      integer, intent(in)                 :: iact

!...     locals
      logical :: ok
      integer :: i, j, js, je, k, lswid
      real :: fluxval
!.......................................................................
      ok = .true.

      js = 1
      do i = 1, nid
         lswid = lsw(i)
         je = xchOff(i)
         do j = js, je
            k = xchIdx(j)
            fluxval = flux(k)
            if (fluxval.ne.mvin) then
               if (iact.eq.1) then
                  if (lswdat(lswid)%cufldr.eq.mv) lswdat(lswid)%cufldr = 0.0
                  if (lswdat(lswid)%cuflif.eq.mv) lswdat(lswid)%cuflif = 0.0
                  if (fluxval.lt.0.) then
                     lswdat(lswid)%cufldr = lswdat(lswid)%cufldr + fluxval
                  else
                     lswdat(lswid)%cuflif = lswdat(lswid)%cuflif + fluxval
                  end if
               else
                  if (lswdat(lswid)%cufldrwells.eq.mv) lswdat(lswid)%cufldrwells = 0.0
                  if (lswdat(lswid)%cuflifwells.eq.mv) lswdat(lswid)%cuflifwells = 0.0
                  if (fluxval.lt.0.) then
                     lswdat(lswid)%cufldrwells = lswdat(lswid)%cufldrwells + fluxval
                  else
                     lswdat(lswid)%cuflifwells = lswdat(lswid)%cuflifwells + fluxval
                  end if
               end if
            end if
         end do
         js = je + 1
      end do

      mozart_GetRiverDrainFlux = ok
      end function

      logical function MOZART_GetCumSWSprinklingDemandFluxes(sprflux,nid,xchIdx,xchOff,mvin)
!...     modules
      use mozartmod, only: mv, lsw, lswdat

      implicit none

!...     arguments
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: sprflux
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin

!...     locals
      logical :: ok
      integer :: i, j, js, je, k, lswid
      real :: flux
!.......................................................................
      ok = .true.

      lswdat(:)%cuflsp = mv

      js = 1
      do i = 1, nid
         lswid = lsw(i)
         je = xchOff(i)
         do j = js, je
            k = xchIdx(j)
            flux = sprflux(k)
            if (flux.ne.mvin) then
               if (lswdat(lswid)%cuflsp.eq.mv) lswdat(lswid)%cuflsp = 0.0
               lswdat(lswid)%cuflsp = lswdat(lswid)%cuflsp + flux
            end if
         end do
         js = je + 1
      end do

      MOZART_GetCumSWSprinklingDemandFluxes = ok
      end function

      logical function MOZART_GetCumRunonFluxes(runonflux,nid,xchIdx,xchOff,mvin)
!...     modules
      use mozartmod, only: mv, lsw, lswdat

      implicit none

!...     arguments
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: runonflux
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin

!...     locals
      logical :: ok
      integer :: i, j, js, je, k, lswid
      real :: flux, fluxron, fluxroff
!.......................................................................
      ok = .true.

      lswdat(:)%cuflroff = mv
      lswdat(:)%cuflron  = mv
      js = 1
      do i = 1, nid
         lswid = lsw(i)
         je = xchOff(i)
         do j = js, je
            k = xchIdx(j)
            flux = runonflux(k)
            if (flux.ne.mvin) then
               fluxron  = 0.0
               fluxroff = 0.0
               if (flux.lt.0.0) fluxroff = flux
               if (flux.gt.0.0) fluxron  = flux
               if (lswdat(lswid)%cuflroff.eq.mv) lswdat(lswid)%cuflroff = 0.0
               if (lswdat(lswid)%cuflron .eq.mv) lswdat(lswid)%cuflron  = 0.0
               lswdat(lswid)%cuflroff = lswdat(lswid)%cuflroff + fluxroff
               lswdat(lswid)%cuflron  = lswdat(lswid)%cuflron  + fluxron
            end if
         end do
         js = je + 1
      end do

      MOZART_GetCumRunonFluxes = ok
      end function

      subroutine MOZART_GetCurrentTime(t)
!...     modules
      use mozartmod, only: beginOfCurrentTimeStep

      implicit none

!...     arguments
      double precision, intent(out) :: t
!.......................................................................

      t = beginOfCurrentTimeStep

      end subroutine

      subroutine MOZART_GetEndOfCurrentTimeStep(t)
!...     modules
      use mozartmod, only: endOfCurrentTimeStep

      implicit none

!...     arguments
      double precision, intent(out) :: t
!.......................................................................

      t = endOfCurrentTimeStep

      end subroutine
      
! =============================================================================
      subroutine mozart_reduce_lsw()
! =============================================================================
      
      use mozartmod, only: nlsw, lsw, lswdat, mv, lswsbuf, lswrbuf
      use pksmpi_mod, only: myrank, mpptyp, mppser, mppini1
      
      implicit none

!...     locals
      integer, parameter :: nlswitem = 10
      integer :: n, i, j, jj, k, lswid
!.......................................................................
      
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return
      
      n = 2*nlsw*nlswitem
      if (.not.allocated(lswsbuf)) then
         allocate(lswsbuf(n))
      end if
      if (.not.allocated(lswrbuf)) then
         allocate(lswrbuf(n))
      end if
      lswsbuf = 0. 
      lswrbuf = 0. 
      
      ! pack phase 1: label missing value
      j = 0
      do i = 1, nlsw
         lswid = lsw(i)
         if (lswdat(lswid)%lswid.le.0) cycle
         call mozart_reduce_lsw_pck_mv(lswsbuf,j,mv,lswdat(lswid)%cufldr)      !01
         call mozart_reduce_lsw_pck_mv(lswsbuf,j,mv,lswdat(lswid)%cufldrwells) !02
         call mozart_reduce_lsw_pck_mv(lswsbuf,j,mv,lswdat(lswid)%cuflif)      !03
         call mozart_reduce_lsw_pck_mv(lswsbuf,j,mv,lswdat(lswid)%cuflifwells) !04
         call mozart_reduce_lsw_pck_mv(lswsbuf,j,mv,lswdat(lswid)%cuflroff)    !05
         call mozart_reduce_lsw_pck_mv(lswsbuf,j,mv,lswdat(lswid)%cuflsp)      !06
         call mozart_reduce_lsw_pck_mv(lswsbuf,j,mv,lswdat(lswid)%cusalt)      !07
         call mozart_reduce_lsw_pck_mv(lswsbuf,j,mv,lswdat(lswid)%cusaltwells) !08
         call mozart_reduce_lsw_pck_mv(lswsbuf,j,mv,lswdat(lswid)%cuseep)      !09
         call mozart_reduce_lsw_pck_mv(lswsbuf,j,mv,lswdat(lswid)%cuunszflux)  !10
      end do      
      jj = j
      ! pack phase 2: data
      do i = 1, nlsw
         lswid = lsw(i)
         if (lswdat(lswid)%lswid.le.0) cycle
         j = j + 1; lswsbuf(j) = lswdat(lswid)%cufldr      !01  
         j = j + 1; lswsbuf(j) = lswdat(lswid)%cufldrwells !02
         j = j + 1; lswsbuf(j) = lswdat(lswid)%cuflif      !03
         j = j + 1; lswsbuf(j) = lswdat(lswid)%cuflifwells !04
         j = j + 1; lswsbuf(j) = lswdat(lswid)%cuflroff    !05 
         j = j + 1; lswsbuf(j) = lswdat(lswid)%cuflsp      !06
         j = j + 1; lswsbuf(j) = lswdat(lswid)%cusalt      !07
         j = j + 1; lswsbuf(j) = lswdat(lswid)%cusaltwells !08
         j = j + 1; lswsbuf(j) = lswdat(lswid)%cuseep      !09
         j = j + 1; lswsbuf(j) = lswdat(lswid)%cuunszflux  !10
      end do
      n = j
      ! mv --> 0
      do j = jj + 1, n
         if (lswsbuf(j).eq.mv) lswsbuf(j) = 0.    
      end do

      ! Master process collects 
      call pks7mpiwrpreducersum( lswsbuf, lswrbuf, n, 0)
      
      ! Master process unpacks
      if (myrank.eq.0) then
         j = 0
         do i = 1, nlsw
            lswid = lsw(i)
            if (lswdat(lswid)%lswid.le.0) cycle
            call mozart_reduce_lsw_upck(lswrbuf,j,jj,mv,lswdat(lswid)%cufldr     ) !01
            call mozart_reduce_lsw_upck(lswrbuf,j,jj,mv,lswdat(lswid)%cufldrwells) !02
            call mozart_reduce_lsw_upck(lswrbuf,j,jj,mv,lswdat(lswid)%cuflif     ) !03
            call mozart_reduce_lsw_upck(lswrbuf,j,jj,mv,lswdat(lswid)%cuflifwells) !04
            call mozart_reduce_lsw_upck(lswrbuf,j,jj,mv,lswdat(lswid)%cuflroff   ) !05
            call mozart_reduce_lsw_upck(lswrbuf,j,jj,mv,lswdat(lswid)%cuflsp     ) !06
            call mozart_reduce_lsw_upck(lswrbuf,j,jj,mv,lswdat(lswid)%cusalt     ) !07
            call mozart_reduce_lsw_upck(lswrbuf,j,jj,mv,lswdat(lswid)%cusaltwells) !08
            call mozart_reduce_lsw_upck(lswrbuf,j,jj,mv,lswdat(lswid)%cuseep     ) !09
            call mozart_reduce_lsw_upck(lswrbuf,j,jj,mv,lswdat(lswid)%cuunszflux ) !10
         end do
      end if
         
      end subroutine   
      
! =============================================================================
      subroutine mozart_reduce_lsw_pck_mv(buf,j,mv,rval)
! =============================================================================
      
      implicit none

!...     arguments
      integer, intent(inout) :: j
      real, dimension(*), intent(inout) :: buf
      real, intent(in) :: mv
      real, intent(in) :: rval
!.......................................................................
          
      j = j + 1
      if(rval.eq.mv)then
         buf(j) = 0.
      else   
         buf(j) = 1.
      end if       
      
      end subroutine
 
! =============================================================================
      subroutine mozart_reduce_lsw_upck(buf,j,jj,mv,rval)
! =============================================================================
      
      implicit none

!...     arguments
      integer, intent(inout) :: j, jj
      real, dimension(*), intent(in) :: buf
      real, intent(in) :: mv
      real, intent(out) :: rval
!.......................................................................
          
      j  = j + 1
      jj = jj + 1
      if (buf(j).gt.0) then
         rval = buf(jj)    
      else
         rval = mv 
      end if
      
      end subroutine      