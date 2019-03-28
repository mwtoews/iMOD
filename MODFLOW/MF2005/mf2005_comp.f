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
c   iMod is partly based on the USGS MODFLOW2005 source code;
c   for iMOD the USGS MODFLOW2005 source code has been expanded
c   and extensively modified by Stichting Deltares.
c   The original USGS MODFLOW2005 source code can be downloaded from the USGS
c   website http://www.usgs.gov/. The original MODFLOW2005 code incorporated
c   in this file is covered by the USGS Software User Rights Notice;
c   you should have received a copy of this notice along with this program.
c   If not, see <http://water.usgs.gov/software/help/notice/>.

c component version of mf2005 main program
C     ******************************************************************
C     MAIN CODE FOR U.S. GEOLOGICAL SURVEY MODULAR MODEL -- MODFLOW-2005
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C1------USE package modules.
      module m_mf2005_main
      USE GLOBAL
      USE GWFBASMODULE
      USE GWFHUFMODULE, ONLY:IOHUFHDS,IOHUFFLWS
      USE GWFEVTMODULE, ONLY:NEVTOP
      USE GWFRCHMODULE, ONLY:NRCHOP
      USE GWFLAKMODULE, ONLY:NLAKESAR,THETA,STGOLD,STGNEW,VOL
      USE GWFUZFMODULE, ONLY: IUZFBND, FINF, VKS
      USE PCGMODULE
      USE PKSMODULE                                                     ! PKS
c      USE LMGMODULE
      USE SIPMODULE
      USE DE4MODULE
      USE GMGMODULE
      integer, private :: i     ! integer i is used in include file openspec.inc
      INCLUDE 'openspec.inc'
C
C-------ASSIGN VERSION NUMBER AND DATE
      CHARACTER*40 VERSION
      CHARACTER*10 MFVNAM
      PARAMETER (VERSION='1.8.00 12/18/2009')
      PARAMETER (MFVNAM='-2005')
C
      CHARACTER*80 HEADNG(2)
      CHARACTER*200 FNAME
      INTEGER IBDT(8)
      integer :: inunit
C
      CHARACTER*4 CUNIT(NIUNIT)
      DATA CUNIT/'BCF6', 'WEL ', 'DRN ', 'RIV ', 'EVT ', '    ', 'GHB ',  !  7
     &           'RCH ', 'SIP ', 'DE4 ', '    ', 'OC  ', 'PCG ', 'lmg ',  ! 14
     &           'gwt ', 'FHB ', 'RES ', 'STR ', 'IBS ', 'CHD ', 'HFB6',  ! 21
     &           'LAK ', 'LPF ', 'DIS ', '    ', 'PVAL', '    ', 'HOB ',  ! 28
     &           '    ', '    ', 'ZONE', 'MULT', 'DROB', 'RVOB', 'GBOB',  ! 35
     &           'STOB', 'HUF2', 'CHOB', 'ETS ', 'DRT ', '    ', 'GMG ',  ! 42
     &           'HYD ', 'SFR ', '    ', 'GAGE', 'LVDA', '    ', 'LMT6',  ! 49
     &           'MNW2', 'MNWI', 'MNW1', 'KDEP', 'SUB ', 'UZF ', 'gwm ',  ! 56
     &           'SWT ', 'cfp ', 'PWT ', '    ', '    ', 'SCR ', 'nrs ',  ! 63  ! DLT: SUB-Creep (62) added
     &           'DXC ', 'ANI ', 'PKS  ', '    ', 'MET ',                  ! 70
     &           32*'    '/        ! 64: Data eXChange


      integer, save, pointer :: nsol
      integer, save, pointer :: iouts
      integer, save, pointer :: kper,kkper,kstp,kkstp,kiter,kkiter
      integer, save, pointer :: icnvg
      integer, save, pointer :: ilmtfmt,issmt3d,iumt3d
      double precision, save, pointer :: timesteptime     ! absolute time value of the
                                                          ! start of the current time step
      logical, save, pointer :: initTimeStep              ! .true.  read timestep data
                                                          ! .false. don´t read anything
                                                          !         set  in *_prepareTimeStep()
                                                          !         used in *_initTimeStep()
      logical, save, pointer :: timeStepCalculated        ! .true.  timestep calculated and converged
                                                          ! .false. calculation of timestep not ready yet
      logical, save, pointer :: solverConverged           ! .true.  last _performIter has converged
      type mitype
         ! multi instance type
         ! modflow 2005 variables
         integer, pointer :: nsol
         integer, pointer :: iouts
         integer, pointer :: kper,kkper,kstp,kkstp,kiter,kkiter
         integer, pointer :: icnvg
         integer, pointer :: ilmtfmt,issmt3d,iumt3d
         double precision, pointer  :: timesteptime
         logical, pointer           :: initTimeStep
         logical, pointer           :: timeStepCalculated
         logical, pointer           :: solverConverged

      end type
      type(mitype), save :: mi(10)

      integer, save :: ninstance=1
!      integer, save :: ninstance=0

      end module m_mf2005_main
c ******************************************************************************
      subroutine mf2005_initComponent(record,retVal)
C     ------------------------------------------------------------------
C     create instance for mf2005 run, igrid is generated

      use m_mf2005_main
      use m_mf2005_iu
      use imod_utl, only : hdr, lic

      implicit none

c arguments
      character (len=*), intent(in)   :: record   ! initialisation record
                                                  ! if empty: the commandline arguments
                                                  !           will be used instead
      integer          , intent(out)  :: retVal   ! return value: 0=OK

c functions
      logical :: pks7mpimasterwrite
      
c local variables
      integer    :: maxunit,nc,i
      integer    :: igrid
      logical    :: lfname

c init
      call timing_tic('MODFLOW','INIT')                                 ! DLT
      retVal = 0
      !## write banner in commandtool
      do i = 1, size(hdr)
       if (pks7mpimasterwrite()) write(*,'(a)') trim(hdr(i))
      end do

C2------WRITE BANNER TO SCREEN AND DEFINE CONSTANTS.
      if (pks7mpimasterwrite()) WRITE (*,1) MFVNAM,VERSION
    1 FORMAT (/,34X,'MODFLOW',A,/,
     &4X,'U.S. GEOLOGICAL SURVEY MODULAR FINITE-DIFFERENCE',
     &' GROUND-WATER FLOW MODEL',/,29X,'Version ',A/)
      INUNIT = 99
c-------process argument record                                         ! DLT
      if (retVal.eq.0) then                                             ! DLT
         call mf2005_args(record,lfname,igrid,retVal)                   ! DLT
      endif                                                             ! DLT
      if (retVal.ne.0) return                                           ! DLT
C
C3------GET THE NAME OF THE NAME FILE
      if (lfname) then                                                  ! DLT
      CALL GETNAMFIL(FNAME)
      endif                                                             ! DLT
      MAXUNIT= INUNIT
C
C4------OPEN NAME FILE.
#ifdef __INTEL_COMPILER
      OPEN (UNIT=INUNIT,FILE=FNAME,STATUS='OLD',ACTION=ACTION(1),
     & SHARE='DENYNONE')                                                ! PKS
#else
      OPEN (UNIT=INUNIT,FILE=FNAME,STATUS='OLD',ACTION=ACTION(1))
#endif       
      NC=INDEX(FNAME,' ')
      if (pks7mpimasterwrite()) 
     &  WRITE(*,490)' Using NAME file: ',FNAME(1:NC)
  490 FORMAT(A,A)
C
C5------Get current date and time, assign to IBDT, and write to screen
      CALL DATE_AND_TIME(VALUES=IBDT)
      if (pks7mpimasterwrite())
     &  WRITE(*,2) (IBDT(I),I=1,3),(IBDT(I),I=5,7)
    2 FORMAT(1X,'Run start date and time (yyyy/mm/dd hh:mm:ss): ',
     &I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2,/)
C
C6------ALLOCATE AND READ (AR) PROCEDURE
cDLT      IGRID=1
      NSOL=1
      CALL GWF2BAS7AR(INUNIT,CUNIT,VERSION,24,31,32,MAXUNIT,IGRID,12,
     1                HEADNG,26,MFVNAM)

      IF(IUNIT(IUMNW2).GT.0 .AND. IUNIT(IUMNW1).GT.0) THEN
        WRITE(IOUT,'(1X,/,1X,A)')
     1  'MNW1 and MNW2 cannot both be active in the same simulation'
        CALL USTOP(' ')
      END IF

c-------process argument record for setting time                        ! DLT
      call mf2005_args_time(record,igrid)                               ! DLT

C  -----SAVE POINTERS TO DATA AND RETURN.
c      CALL SGWF2INS1PSV(IGRID)                                           ! DLT: instances
      call timing_toc('MODFLOW','INIT')                                 ! DLT
      return
      end
c ******************************************************************************
      subroutine mf2005_initSimulationWrp0()
      ! wrapper routine
      implicit none

      ! local variables
      integer :: retVal
      ! functions
      double precision :: currentTime
      ! ------------------------------
      call mf2005_getCurrentTime(currentTime,retVal)
      call mf2005_initSimulation(currentTime,retVal)
      return
      end
c =======
      subroutine mf2005_initSimulation(currentTime,retVal)

      use m_mf2005_main
      use m_mf2005_iu

      implicit none

c arguments
      double precision, intent(in)  :: currentTime
      integer         , intent(out) :: retVal


c local variable
      integer    :: igrid
      double precision, parameter   :: dnull=0.d0

c functions
      double precision  :: cfn_mjd_delta

      call timing_tic('MODFLOW','AR')                                   ! DLT
      
c init
      retVal=0

c program

      do igrid=1,ninstance                                              ! DLT: instances
      call sgwf2ins1pnt(igrid)                                           DLT: instances
      call SGWF2BAS7PNT(IGRID)                                          ! DLT: instances

         ! check current time value                                     ! DLT: components
         if (cfn_mjd_delta(currentTime,timesteptime).ne.dnull) then     ! DLT: components
            write(*,*)                                                  ! DLT: components
     1            ' ERROR, current time not equal to Modflow time: ',   ! DLT: components
     1                 currentTime,timesteptime                         ! DLT: components
            retVal=-1                                                   ! DLT: components
            return                                                      ! DLT: components
         endif                                                          ! DLT: components
         timeStepCalculated = .false.                                   ! DLT: components

      IF(IUNIT(IUBCF6).GT.0) CALL GWF2BCF7AR(IUNIT(IUBCF6),IGRID)
      IF(IUNIT(IULPF).GT.0) CALL GWF2LPF7AR(IUNIT(IULPF),IGRID)
c read pwt before ani for correction of ani
      if(IUNIT(IUPWT).gt.0) call gwf2pwt3ar(IUNIT(IUPWT),IUNIT(IUBCF6),
     1                           igrid,iout)                            ! PWT3
c read hfb before ani for corrections of ani
      IF(IUNIT(IUHFB6).GT.0) CALL GWF2HFB7AR(IUNIT(IUHFB6),IGRID)
      if(IUNIT(IUANI).gt.0) call gwf2ani3ar(IUNIT(IUANI),               ! ANI
     1                      IUNIT(IUPWT),IUNIT(IUHFB6),igrid)           ! ANI
      IF(IUNIT(IUHUF2).GT.0) CALL GWF2HUF7AR(IUNIT(IUHUF2),
     1                             IUNIT(IULVDA),IUNIT(IUKDEP),IGRID)
      IF(IUNIT(IUWEL).GT.0) CALL GWF2WEL7AR(IUNIT(IUWEL),IGRID)
      IF(IUNIT(IUDRN).GT.0) CALL GWF2DRN7AR(IUNIT(IUDRN),IGRID)
      IF(IUNIT(IURIV).GT.0) CALL GWF2RIV7AR(IUNIT(IURIV),IGRID)
      IF(IUNIT(IUEVT).GT.0) CALL GWF2EVT7AR(IUNIT(IUEVT),IGRID)
      IF(IUNIT(IUGHB).GT.0) CALL GWF2GHB7AR(IUNIT(IUGHB),IGRID)
      IF(IUNIT(IURCH).GT.0) CALL GWF2RCH7AR(IUNIT(IURCH),IGRID)
      IF(IUNIT(IUFHB).GT.0) CALL GWF2FHB7AR(IUNIT(IUFHB),IGRID)
      IF(IUNIT(IURES).GT.0) CALL GWF2RES7AR(IUNIT(IURES),IGRID)
      IF(IUNIT(IUSTR).GT.0) CALL GWF2STR7AR(IUNIT(IUSTR),IGRID)
      IF(IUNIT(IUIBS).GT.0) CALL GWF2IBS7AR(IUNIT(IUIBS),
     1                                      IUNIT(IUSUB),IGRID)
      IF(IUNIT(IUCHD).GT.0) CALL GWF2CHD7AR(IUNIT(IUCHD),IGRID)
!      IF(IUNIT(IUHFB6).GT.0) CALL GWF2HFB7AR(IUNIT(IUHFB6),IGRID)
      IF(IUNIT(IUSFR).GT.0) CALL GWF2SFR7AR(IUNIT(IUSFR),IUNIT(IUBCF6),
     1      IUNIT(IULPF), IUNIT(IUHUF2),IUNIT(IUGWT),NSOL,IOUTS,
     1      IUNIT(IUUZF),IGRID)
      IF(IUNIT(IUUZF).GT.0) CALL GWF2UZF1AR(IUNIT(IUUZF),IUNIT(IUBCF6),
     1                                IUNIT(IULPF),IUNIT(IUHUF2),IGRID)
      IF(IUNIT(IULAK).GT.0 .OR. IUNIT(IUSFR).GT.0) CALL GWF2LAK7AR(
     1             IUNIT(IULAK),IUNIT(IUSFR),IUNIT(IUGWT),IUNIT(IUUZF),
     1             NSOL,IGRID)
      IF(IUNIT(IUGAGE).GT.0) CALL GWF2GAG7AR(IUNIT(IUGAGE),IUNIT(IUSFR),
     1                                     IUNIT(IULAK),IGRID)
      IF(IUNIT(IUETS).GT.0) CALL GWF2ETS7AR(IUNIT(IUETS),IGRID)
      IF(IUNIT(IUDRT).GT.0) CALL GWF2DRT7AR(IUNIT(IUDRT),IGRID)
      IF(IUNIT(IUSUB).GT.0) CALL GWF2SUB7AR(IUNIT(IUSUB),IGRID)
      IF(IUNIT(IUSIP).GT.0) CALL SIP7AR(IUNIT(IUSIP),MXITER,IGRID)
      IF(IUNIT(IUDE4).GT.0) CALL DE47AR(IUNIT(IUDE4),MXITER,IGRID)
      IF(IUNIT(IUPCG).GT.0) CALL PCG7AR(IUNIT(IUPCG),MXITER,IGRID)
c      IF(IUNIT(IULMG).GT.0) CALL LMG7AR(IUNIT(IULMG),MXITER,IGRID)
c      IF(IUNIT(42).GT.0) CALL GMG7AR(IUNIT(42),MXITER,IGRID)
      IF(IUNIT(IUPKS).GT.0) THEN
        CALL PKS7AR(IUNIT(IUPKS),MXITER,IGRID)                          !PKS - JDH
        CALL PKS7MPIAR(IOUT,IBOUND,NODES)                               !JV
      ENDIF         
      IF(IUNIT(IUMNW2).GT.0) CALL GWF2MNW27AR(IUNIT(IUMNW2),IGRID)
      IF(IUNIT(IUMNWI).GT.0) CALL GWF2MNW2I7AR(IUNIT(IUMNWI),
     1                     IUNIT(IUMNW2),IGRID)
      IF(IUNIT(IUMNW1).GT.0) CALL GWF2MNW17AR(IUNIT(IUMNW1),
     1              IUNIT(IUSIP),IUNIT(IUDE4),0,IUNIT(IUPCG),
     2                     0,IUNIT(42),FNAME,IGRID)
      IF(IUNIT(IUSWT).GT.0) CALL GWF2SWT7AR(IUNIT(IUSWT),IGRID)
c SUB-Creep
      IF(IUNIT(IUSCR).GT.0) THEN
          CALL GWF2SCR7AR(IUNIT(IUSCR),IGRID)
          !calculate a priori time steps which will be later used by MODFLOW
          CALL GWF2SCR1TM(IGRID)
       END IF
C SUB-Creep end

      IF(IUNIT(IUHYD).GT.0) CALL GWF2HYD7BAS7AR(IUNIT(IUHYD),IGRID)
      IF(IUNIT(IUHYD).GT.0 .AND. IUNIT(IUIBS).GT.0)
     1                   CALL GWF2HYD7IBS7AR(IUNIT(IUHYD),IGRID)
      IF(IUNIT(IUHYD).GT.0 .AND. IUNIT(IUSUB).GT.0)
     1                   CALL GWF2HYD7SUB7AR(IUNIT(IUHYD),IGRID)
      IF(IUNIT(IUHYD).GT.0 .AND. IUNIT(IUSTR).GT.0)
     1                   CALL GWF2HYD7STR7AR(IUNIT(IUHYD),IGRID)
      IF(IUNIT(IUHYD).GT.0 .AND. IUNIT(IUSFR).GT.0)
     1                   CALL GWF2HYD7SFR7AR(IUNIT(IUHYD),IGRID)
      IF(IUNIT(IUDXC).GT.0) CALL gwf2dxc1AR(IUNIT(IUDXC),IGRID)         ! DLT: Data eXChance package
C
C  Observation allocate and read
      CALL OBS2BAS7AR(IUNIT(IUHOB),IGRID)
      IF(IUNIT(IUDROB).GT.0) CALL OBS2DRN7AR(IUNIT(IUDROB),
     1                                       IUNIT(IUDRN),IGRID)
      IF(IUNIT(IURVOB).GT.0) CALL OBS2RIV7AR(IUNIT(IURVOB),
     1                                       IUNIT(IURIV),IGRID)
      IF(IUNIT(IUGBOB).GT.0) CALL OBS2GHB7AR(IUNIT(IUGBOB),
     1                                       IUNIT(IUGHB),IGRID)
      IF(IUNIT(IUSTOB).GT.0) CALL OBS2STR7AR(IUNIT(IUSTOB),
     1                                       IUNIT(IUSTR),IGRID)
      IF(IUNIT(IUCHOB).GT.0) CALL OBS2CHD7AR(IUNIT(IUCHOB),IGRID)

      write(*,*) 'End MF Allocation'
      ! init timestep
      kper=0

      enddo                                                             ! DLT: instances
C
      call timing_toc('MODFLOW','AR')

      return
      end

c ******************************************************************************
      subroutine mf2005_writeTimeStep(tsc,date,hour,minute,second)
      use global, only : issflg
      use m_mf2005_main, only : kkper
      implicit none
      integer,intent(in) :: date,tsc,hour,minute,second
            
!what about steady-state solutions, currenttime.eq.0
          if(issflg(kkper).eq.1)then 
           write(*,'(5x,a,1x,a)')'Timestep     :','steady-state'
          else
           write(*,'(5x,a,1x,i5,1x,a,1x,i8,3(a,i2.2))')
     1'Timestep     :',tsc,':',date,' ',abs(hour),':',minute,':',second
          endif

      end subroutine mf2005_writeTimeStep

c ******************************************************************************
      subroutine mf2005_returnIOUT(jout)
      use global, only : iout
      implicit none
      integer,intent(out) :: jout
            
      jout=iout
      
      end subroutine mf2005_returnIOUT

c ******************************************************************************
      subroutine mf2005_prepareTimestepWrp0(deltats,endOfSimulation)
      ! wrapper routine
      ! this routine will always advance one timestep
      use m_mf2005_main, only: mi

      implicit none

      ! arguments
      integer, intent(inout)  :: deltats           ! set timstep increment
      logical, intent(inout)  :: endOfSimulation   ! set value to .true. if end of simulation is reached
                                                   ! otherwise DON'T change this vallue
      ! local variables
      integer           :: igrid
      integer           :: retVal
      logical           :: stateSave
      double precision  :: currentTime

      ! functions
      double precision  :: sutl_getTimeStepLength
      ! ------------------------------

      ! add timesteplength to current time
      ! use timestep of model(igrid=1)
      igrid=1
      currentTime=mi(igrid)%timesteptime
      currentTime=currentTime+sutl_getTimeStepLength(igrid)
      stateSave  =.false.
      call mf2005_prepareTimestep(currentTime,stateSave,retVal)

      ! check return value
      if (retVal.ne.0) then
         endOfSimulation=.true.
      else
         deltats=min(deltats,1)    ! advance maximum 1 timestep forward
      endif

      return
      end
c =======
      subroutine mf2005_prepareTimestep(currentTime,saveState,retVal)

      use m_mf2005_main
      use global, only : iout, issflg

      implicit none


c arguments
      double precision, intent(in)  :: currentTime
      logical         , intent(in)  :: saveState
      integer         , intent(out) :: retVal
c      integer, intent(inout)  :: deltats           ! set timstep increment
c      logical, intent(inout)  :: endOfSimulation   ! set value to .true. if end of simulation is reached
c                                                   ! otherwise DON'T change this vallue

c local variable
      integer           :: igrid
      logical           :: initstress,inittime
      double precision  :: dt

c functions
      double precision  :: cfn_mjd_delta

c ------------------------------------------------------------------------------

! init
      retVal=0


      do igrid=1,ninstance                                              ! DLT: instances
      call sgwf2ins1pnt(igrid)                                          ! DLT: instances
      call SGWF2BAS7PNT(IGRID)                                          ! DLT: instances

      ! find out whether the model has to advance to the next timestep  ! DLT: components
      dt = cfn_mjd_delta(currentTime,timesteptime)                      ! DLT: components
                                                                        ! DLT: components
      ! check restore timestep                                          ! DLT: components
      if (dt.lt.0.d0) then                                              ! DLT: components
         ! restore timestep                                             ! DLT: components
         write(*,'(a,f20.5)') '  RESTORE STATE: MF2005  ',currentTime
         call mf2005_staterestore(currentTime)                          ! DLT: components
      else if (dt.eq.0.d0 .and. timeStepCalculated) then                ! DLT: components
         ! calculate timestep again, restoreState needed                ! DLT: components
       !## only for transient timesteps
       if(issflg(kkper).eq.0)then
         write(*,'(a,f20.5)') '  RESTORE STATE: MF2005  ',currentTime
         call mf2005_staterestore(currentTime)                          ! DLT: components
       endif
      else                                                              ! DLT: components
         ! save timestep (only when no restore has to be done)          ! DLT: components
         if (saveState) then                                            ! DLT: components
      write(*,'(a,f20.5)') '  SAVE    STATE: MF2005  ',currentTime
            call mf2005_statesave(currentTime)                          ! DLT: components
         endif                                                          ! DLT: components
      endif                                                             ! DLT: components
                                                                        ! DLT: components
      initTimeStep=.true.                                               ! DLT: components

      ! set new time value
      timesteptime=currentTime

c get next timestep
      if (initTimeStep) then

         initstress=.false.
         if (kper.eq.0) then
            ! first timestep
            initstress=.true.
            inittime  =.true.
         else
            ! check end of stress period
            if (kstp.ge.nstp(kper)) then
               ! new stress period needed
               if (kper.ge.nper) then
                  ! this was the last stress period
                  initstress=.false.
                  inittime  =.false.
               else
                  ! new stress period, new timestep
                  initstress=.true.
                  inittime  =.true.
               endif
            else
               ! same stress period, new timestep
               initstress=.false.
               inittime  =.true.
            endif
         endif


c next stress period
         if (initstress) then
            kper =kper+1
            kkper=kper
            kstp=0
         endif


c next time step
         if (inittime) then
c         deltats=min(deltats,1)    ! advance maximum 1 timestep forward
            kstp =kstp+1
            kkstp=kstp
         endif


c find out or this is end of simulation
         if (.not. inittime) then
            ! endOfSimulation=.true.
            initTimeStep=.false.
            retVal=-1
         endif


c init solve
         kiter =0
         kkiter=kiter

         if (initTimeStep) then
            timeStepCalculated = .false.
            solverConverged    = .false.
         endif

      endif

      enddo                                                             ! DLT: instances
C
      return
      end
c ******************************************************************************
      subroutine mf2005_initTimeStepWrp0(restore)
      ! wrapper routine
      implicit none
c arguments
      logical, intent(in)     :: restore   ! force reading data

      ! local variables
      integer           :: retVal
      double precision  :: currentTime
      ! ------------------------------
      call mf2005_getCurrentTime(currentTime,retVal)
      call mf2005_initTimeStep(currentTime,retVal)
      return
      end
c =======
      subroutine mf2005_initTimeStep(currentTime,retVal)

      use m_mf2005_main
      use m_mf2005_iu

      implicit none


c arguments
      double precision, intent(in)  :: currentTime   ! not used at this moment
                                                     ! Necessary information comes from _prepareTimeStep
                                                     ! by using variable initTimeStep
      integer         , intent(out) :: retVal


c local variable
      integer    :: igrid
      logical    initstress,inittime


c ------------------------------------------------------------------------------

c init
      call timing_tic('MODFLOW','RP')                                   ! DLT

      retVal = 0


      do igrid=1,ninstance                                              ! DLT: instances
c      call sgwf2ins1pnt(igrid)                                                  ! DLT: instances

      call SGWF2BAS7PNT(IGRID)                                          ! DLT: instances

        KKPER = KPER
        kkstp = kstp                                                    ! DLT: instances

c check what to read
      if (kstp.eq.1) then
         initstress=.true.  .and. initTimeStep
         inittime  =.true.  .and. initTimeStep
      else
         initstress=.false. .and. initTimeStep
         inittime  =.true.  .and. initTimeStep
      endif


c next stress period
      if (initstress) then

C7------SIMULATE EACH STRESS PERIOD.
c      DO 100 KPER = 1, NPER
        CALL GWF2BAS7ST(KKPER,IGRID)
        IF(IUNIT(IUMET).GT.0) CALL GWF2MET1ST(KKPER,IGRID)                    ! MET
        IF(IUNIT(IUIBS).GT.0) CALL GWF2IBS7ST(KKPER,IGRID)
        IF(IUNIT(IUSUB).GT.0) CALL GWF2SUB7ST(KKPER,IGRID)
        IF(IUNIT(IUSWT).GT.0) CALL GWF2SWT7ST(KKPER,IGRID)
c SUB-Creep
        IF(IUNIT(IUSCR).GT.0) CALL GWF2SCR7ST(KKPER,IGRID)
c SUB-Creep end


C7B-----READ AND PREPARE INFORMATION FOR STRESS PERIOD.
C----------READ USING PACKAGE READ AND PREPARE MODULES.
        IF(IUNIT(IUWEL).GT.0) CALL GWF2WEL7RP(IUNIT(IUWEL),IGRID)
        IF(IUNIT(IURIV).GT.0) CALL GWF2RIV7RP(IUNIT(IURIV),IGRID)
        IF(IUNIT(IUDRN).GT.0) CALL GWF2DRN7RP(IUNIT(IUDRN),IGRID,
     1                                        IUNIT(IURIV))
        IF(IUNIT(IUEVT).GT.0) CALL GWF2EVT7RP(IUNIT(IUEVT),IGRID)
        IF(IUNIT(IUGHB).GT.0) CALL GWF2GHB7RP(IUNIT(IUGHB),IGRID)
        IF(IUNIT(IURCH).GT.0) CALL GWF2RCH7RP(IUNIT(IURCH),IGRID)
        IF(IUNIT(IURES).GT.0) CALL GWF2RES7RP(IUNIT(IURES),IGRID)
        IF(IUNIT(IUSTR).GT.0) CALL GWF2STR7RP(IUNIT(IUSTR),IGRID)
        IF(IUNIT(IUHYD).GT.0 .AND. IUNIT(IUSTR).GT.0)
     1                     CALL GWF2HYD7STR7RP(IUNIT(IUHYD),KKPER,IGRID)
        IF(IUNIT(IUCHD).GT.0) CALL GWF2CHD7RP(IUNIT(IUCHD),IGRID)

!        SUBROUTINE GWF2SFR7RP(In, Iunitgwt, Iunitlak, Kkper, Kkstp, Nsol,
!     +                      Iouts, Iunitbcf, Iunitlpf, Iunithuf, 
!     +                      Iunituzf, Igrid)
     
        IF(IUNIT(IUSFR).GT.0) CALL GWF2SFR7RP(IUNIT(IUSFR),IUNIT(IUGWT),
     1   IUNIT(IULAK),KKPER,KKSTP,NSOL,IOUTS,IUNIT(IUBCF6),IUNIT(IULPF),
     1   IUNIT(IUHUF2),IUNIT(IUUZF),IGRID)
        IF(IUNIT(IUHYD).GT.0 .AND. IUNIT(IUSFR).GT.0)
     1                     CALL GWF2HYD7SFR7RP(IUNIT(IUHYD),KKPER,IGRID)
        IF(IUNIT(IUUZF).GT.0) CALL GWF2UZF1RP(IUNIT(IUUZF),KKPER,
     1    iunit(iusfr),IGRID)

!        SUBROUTINE GWF2LAK7RP(IN,IUNITBCF,IUNITGWT,IUNITLPF,IUNITHUF,
!     +                      IUNITSFR,IUNITUZF,KKPER,NSOL,IOUTS,IGRID)

        IF(IUNIT(IULAK).GT.0) CALL GWF2LAK7RP(IUNIT(IULAK),
     1     IUNIT(IUBCF6),IUNIT(IUGWT),IUNIT(IULPF),IUNIT(IUHUF2),
     1     IUNIT(IUSFR),IUNIT(IUUZF),KKPER,NSOL,IOUTS,IGRID)
        IF(IUNIT(IUGAGE).GT.0.AND.KKPER.EQ.1)
     1     CALL GWF2GAG7RP(IUNIT(IUGWT),IUNIT(IULAK),IUNIT(IUUZF),NSOL,
     1     IGRID)
        IF(IUNIT(IUETS).GT.0) CALL GWF2ETS7RP(IUNIT(IUETS),IGRID)
        IF(IUNIT(IUDRT).GT.0) CALL GWF2DRT7RP(IUNIT(IUDRT),IGRID)
        IF(IUNIT(IUMNW2).GT.0) CALL GWF2MNW27RP(IUNIT(IUMNW2),KKPER,
     1                      IUNIT(IUSIP),
     1                     IUNIT(IUDE4),0,IUNIT(IUPCG),0,IUNIT(42),0,
     1                     IGRID)
        IF(IUNIT(IUMNWI).GT.0.AND.KKPER.EQ.1)
     1        CALL GWF2MNW2I7RP(IUNIT(IUMNWI),0,IGRID)
        IF(IUNIT(IUMNW1).GT.0) CALL GWF2MNW17RP(IUNIT(IUMNW1),
     1                           IUNIT(IUBCF6),
     1                           IUNIT(IULPF),IUNIT(IUHUF2),KKPER,IGRID)
        IF(IUNIT(IUDXC).GT.0) CALL gwf2dxc1rp(igrid)                       ! DLT: Data eXChance package

      endif  ! next stress period
      
c next time step
      if (inittime) then

C
C7C-----SIMULATE EACH TIME STEP.
c        DO 90 KSTP = 1, NSTP(KPER)
          KKSTP = KSTP
C
C7C1----CALCULATE TIME STEP LENGTH. SET HOLD=HNEW.
          CALL GWF2BAS7AD(KKPER,KKSTP,IGRID)
          IF(IUNIT(IUCHD).GT.0) CALL GWF2CHD7AD(KKPER,IGRID)
          IF(IUNIT(IUBCF6).GT.0) CALL GWF2BCF7AD(KKPER,IGRID)
          IF(IUNIT(IURES).GT.0) CALL GWF2RES7AD(KKSTP,KKPER,IGRID)
          IF(IUNIT(IULPF).GT.0) CALL GWF2LPF7AD(KKPER,IGRID)
          IF(IUNIT(IUHUF2).GT.0) CALL GWF2HUF7AD(KKPER,IGRID)
          IF(IUNIT(IUFHB).GT.0) CALL GWF2FHB7AD(IGRID)
          IF(IUNIT(IULAK).GT.0) CALL GWF2LAK7AD(KKPER,KKSTP,
     1                                    IUNIT(IUGWT),IGRID)
          IF(IUNIT(IUMNW2).GT.0) THEN
            IF (IUNIT(IUBCF6).GT.0) THEN
              CALL GWF2MNW27BCF(KPER,IGRID)
            ELSE IF (IUNIT(IULPF).GT.0) THEN
              CALL GWF2MNW27LPF(KPER,IGRID)
            ELSE IF(IUNIT(IUHUF2).GT.0) THEN
              CALL GWF2MNW27HUF(KPER,IGRID)
            ELSE
              WRITE(IOUT,1000)
 1000         FORMAT(/1X,
     &      '***ERROR: MNW2 PACKAGE DOES NOT SUPPORT',/,
     &      ' SELECTED FLOW PACKAGE',/,
     &      ' (MNW2 DOES FULLY SUPPORT BCF, LPF, AND HUF PACKAGES)',/,
     &      ' -- STOP EXECUTION')
              CALL USTOP('MNW2 error-flow package')
            END IF
            CALL GWF2MNW27AD(KKSTP,KKPER,IGRID)
          END IF
          IF(IUNIT(IUMNW1).GT.0) CALL GWF2MNW17AD(IUNIT(IUBCF6),
     1                             IUNIT(IULPF),IUNIT(IUHUF2),IGRID)
      endif

      enddo                                                                     ! DLT: instances
C
      call timing_toc('MODFLOW','RP')                                   ! DLT

      return
      end

c ******************************************************************************
      subroutine mf2005_prepareIter(retVal)

      implicit none

      ! arguments
      integer, intent(out)           :: retVal
      ! local variables
      integer iteration
      logical converged
      ! ------------------------------

!  Moved to performiter
!      call mf2005_prepareIterWrp0(Iteration,converged)
!      call mf2005_initIter(retVal)
      retVal = 0
      return
      end

c =====


      subroutine mf2005_prepareIterWrp0(Iteration,converged)
c get next iteration number
c 2011/06/16 renamed from mf2005_prepareIter

      use m_mf2005_main

      implicit none

c arguments
      integer, intent(out)   :: iteration   ! -1: convergence failed
                                            !  0: continue iterations
                                            !  1: solver converged
      logical, intent(in)    :: converged   ! .true.  solver converged
                                            ! .false. solver did not converge
      integer    :: igrid

c ------------------------------------------------------------------------------

      iteration=1 ! start value

      do igrid=1,ninstance                                              ! DLT: instances
      call sgwf2ins1pnt(igrid)                                          ! DLT: instances
      call SGWF2BAS7PNT(IGRID)                                          ! DLT: instances

      kiter =kiter+1
      kkiter=kiter
      if (kkiter.gt.mxiter) then
         ! convergence failed
         iteration=min(-1,iteration)
      else
         if (kkiter.eq.1) then
            ! first iteration
            iteration=min(0,iteration)
         else
            ! check convergence
            if (converged) then
               iteration=min(1,iteration)
            else
               iteration=min(0,iteration)
            endif
         endif
      endif

      enddo                                                             ! DLT: instances

      return
      end   ! subroutine mf2005_prepareIter

c ******************************************************************************
      subroutine mf2005_initIterWrp0()
      ! wrapper routine
      implicit none

      ! local variables
      integer           :: retVal
      ! ------------------------------

      call mf2005_initIter(retVal)
      return
      end

c =====

      subroutine mf2005_initIter(retVal)

      use m_mf2005_main
      use m_mf2005_iu

      implicit none

c arguments
      integer         , intent(out) :: retVal

c functions
      logical :: pks7mpimasterwrite
      
c local variable
      integer    :: igrid


c init
      call timing_tic('MODFLOW','FM')                                   ! DLT

      retVal = 0


      do igrid=1,ninstance                                              ! DLT: instances
      call sgwf2ins1pnt(igrid)                                          ! DLT: instances
      call SGWF2BAS7PNT(IGRID)                                          ! DLT: instances

C---------INDICATE IN PRINTOUT THAT SOLUTION IS FOR HEADS
          CALL UMESPR('SOLVING FOR HEAD',' ',IOUT)
          if (pks7mpimasterwrite()) WRITE(*,25)KPER,KSTP
   25     FORMAT(' Solving:  Stress period: ',i5,4x,
     &       'Time step: ',i5,4x,'Ground-Water Flow Eqn.')
C
C7C2----ITERATIVELY FORMULATE AND SOLVE THE FLOW EQUATIONS.
c          DO 30 KITER = 1, MXITER
            KKITER = KITER
C
C7C2A---FORMULATE THE FINITE DIFFERENCE EQUATIONS.
            CALL GWF2BAS7FM(IGRID)
            if(iunit(iupwt).gt.0) call gwf2pwt3fm(kkiter,kkper,igrid,
     1                                           iunit(iubcf6))         ! PWT3
            IF(IUNIT(IUBCF6).GT.0) CALL GWF2BCF7FM(KKITER,KKSTP,
     1                               KKPER,IGRID)
            IF(IUNIT(IULPF).GT.0) CALL GWF2LPF7FM(KKITER,
     1                             KKSTP,KKPER,IGRID)
            IF(IUNIT(IUHUF2).GT.0) CALL GWF2HUF7FM(KKITER,
     1                             KKSTP,KKPER,IUNIT(IULVDA),IGRID)
            IF(IUNIT(IUHFB6).GT.0) CALL GWF2HFB7FM(IGRID)
            IF(IUNIT(IUWEL).GT.0) CALL GWF2WEL7FM(IGRID)
            IF(IUNIT(IUDRN).GT.0) CALL GWF2DRN7FM(IGRID)
            IF(IUNIT(IURIV).GT.0) CALL GWF2RIV7FM(IGRID)
            IF(IUNIT(IUEVT).GT.0) THEN
              IF(IUNIT(IULAK).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
     1                                                    0,IGRID)
              CALL GWF2EVT7FM(IGRID)
              IF(IUNIT(IULAK).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
     1                                                    1,IGRID)
            END IF
            IF(IUNIT(IUGHB).GT.0) CALL GWF2GHB7FM(IGRID)
            if(IUNIT(IUDXC).gt.0) call gwf2dxc1fm(igrid)                ! DLT: DXC added
            IF(IUNIT(IURCH).GT.0) THEN
               IF(IUNIT(IULAK).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
     1                                                    0,IGRID)
               CALL GWF2RCH7FM(IGRID)
               IF(IUNIT(IULAK).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
     1                                                    1,IGRID)
            END IF
            IF(IUNIT(IUFHB).GT.0) CALL GWF2FHB7FM(IGRID)
            IF(IUNIT(IURES).GT.0) CALL GWF2RES7FM(IGRID)
            IF(IUNIT(IUSTR).GT.0) CALL GWF2STR7FM(IGRID)
            IF(IUNIT(IUIBS).GT.0) CALL GWF2IBS7FM(KKPER,IGRID)
            IF(IUNIT(IUETS).GT.0) CALL GWF2ETS7FM(IGRID)
            IF(IUNIT(IUDRT).GT.0) CALL GWF2DRT7FM(IGRID)
!                  SUBROUTINE GWF2UZF1FM(Kkper, Kkstp, Kkiter, Iunitsfr, Iunitlak, 
!     +                      Iunitcfp, Igrid)
            IF(IUNIT(IUUZF).GT.0) CALL GWF2UZF1FM(KKPER,KKSTP,KKITER,
     1                           IUNIT(IUSFR),IUNIT(IULAK),
     1                           IUNIT(IUCFP),IGRID)
!           SUBROUTINE GWF2SFR7FM(Kkiter, Kkper, Kkstp, Iunitlak, Iunitrch, 
!     +                      Iunituzf, Igrid)
            IF(IUNIT(IUSFR).GT.0) CALL GWF2SFR7FM(KKITER,KKPER,KKSTP,
     1        IUNIT(IULAK),IUNIT(IURCH),IUNIT(IUUZF),IGRID)
!           SUBROUTINE GWF2LAK7FM(KKITER,KKPER,KKSTP,IUNITSFR,IUNITUZF,IGRID)
            IF(IUNIT(IULAK).GT.0) CALL GWF2LAK7FM(KKITER,KKPER,KKSTP,
     1        IUNIT(IUSFR),IUNIT(IUUZF),IGRID)
            IF(IUNIT(IUMNW2).GT.0) THEN
              IF (IUNIT(IUBCF6).GT.0) THEN
                CALL GWF2MNW27BCF(KPER,IGRID)
              ELSE IF (IUNIT(IULPF).GT.0) THEN
                CALL GWF2MNW27LPF(KPER,IGRID)
              ELSE IF(IUNIT(IUHUF2).GT.0) THEN
                CALL GWF2MNW27HUF(KPER,IGRID)
              END IF
              CALL GWF2MNW27FM(KKITER,kkstp,kkper,IGRID)
            END IF
            IF(IUNIT(IUMNW1).GT.0) CALL GWF2MNW17FM(KKITER,
     1          IUNIT(IUBCF6),IUNIT(IULPF),IUNIT(IUHUF2),IGRID)
            IF(IUNIT(IUSUB).GT.0) CALL GWF2SUB7FM(KKPER,KKITER,
     1            IUNIT(IUSIP),IGRID)
            IF(IUNIT(IUSWT).GT.0) CALL GWF2SWT7FM(KKPER,IGRID)
c SUB-Creep
            IF(IUNIT(IUSCR).GT.0) CALL GWF2SCR7FM(KKPER,KSTP,IGRID)
c SUB-Creep

            if(IUNIT(IUANI).gt.0) call gwf2ani3fm(igrid)                ! ANI

      enddo                                                             ! DLT: instances
C
      call timing_toc('MODFLOW','FM')                                   ! DLT

      return
      end
c ******************************************************************************
      subroutine mf2005_performIter(retVal,psolved)
c 2011/06/16 renamed from mf2005_solve.

      use m_mf2005_main
      use m_mf2005_iu

      implicit none

c arguments
      integer, intent(out) :: retVal
      logical, intent(out) :: psolved
      
c local variables
      integer    :: ierr
      integer    :: igrid

      integer    :: iteration
      logical    :: converged
      
      integer :: i
      logical :: lss
c ------------------------------------------------------------------------------

c init
      retVal = 0

c     Moved from prepare iter (2 lines)
      call mf2005_prepareIterWrp0(Iteration,converged)
      if (iteration.lt.0.and..not.converged) then
         lss = .true.
         do i=1, nper
            if (issflg(i).eq.0) lss = .false.
         end do
         if (lss) then
            retval = -1
            return
         end if   
      end if 
      call mf2005_initIter(retVal)
c
      call timing_tic('MODFLOW','SOL')                                  ! DLT

      do igrid=1,ninstance                                                      ! DLT: instances
      call sgwf2ins1pnt(igrid)                                                  ! DLT: instances
      call SGWF2BAS7PNT(IGRID)                                                  ! DLT: instances

      kkiter=kiter                                                              ! DLT: instances
!      write(*,*) kiter
C7C2B---MAKE ONE CUT AT AN APPROXIMATE SOLUTION.
            IERR=0
            IF (IUNIT(IUSIP).GT.0) THEN
                   CALL SIP7PNT(IGRID)
                   CALL SIP7AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,EL,FL,GL,
     1               V,W,HDCG,LRCH,NPARM,KKITER,HCLOSE,ACCL,ICNVG,
     2               KKSTP,KKPER,IPCALC,IPRSIP,MXITER,NSTP(KKPER),
     3               NCOL,NROW,NLAY,NODES,IOUT,0,IERR)
            END IF
            IF (IUNIT(IUDE4).GT.0) THEN
                   CALL DE47PNT(IGRID)
                   CALL DE47AP(HNEW,IBOUND,AU,AL,IUPPNT,IEQPNT,D4B,MXUP,
     1               MXLOW,MXEQ,MXBW,CR,CC,CV,HCOF,RHS,ACCLDE4,KITER,
     2               ITMX,MXITER,NITERDE4,HCLOSEDE4,IPRD4,ICNVG,NCOL,
     3               NROW,NLAY,IOUT,LRCHDE4,HDCGDE4,IFREQ,KKSTP,KKPER,
     4               DELT,NSTP(KKPER),ID4DIR,ID4DIM,MUTD4,
     5               DELTL,NBWL,NUPL,NLOWL,NLOW,NEQ,NUP,NBW,IERR)
            END IF
            IF (IUNIT(IUPCG).GT.0) THEN
                   CALL PCG7PNT(IGRID)
                   CALL PCG7AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,VPCG,SS,
     1               P,CD,HCHG,LHCH,RCHG,LRCHPCG,KKITER,NITER,
     2               HCLOSEPCG,RCLOSEPCG,ICNVG,KKSTP,KKPER,IPRPCG,
     3               MXITER,ITER1,NPCOND,NBPOL,NSTP(KKPER),NCOL,NROW,
     4               NLAY,NODES,RELAXPCG,IOUT,MUTPCG,IT1,DAMPPCG,BUFF,
     5               HCSV,IERR,HPCG,DAMPPCGT,ISSFLG(KKPER),HDRY)
            END IF
c            IF (IUNIT(IULMG).GT.0) THEN
c              CALL LMG7PNT(IGRID)
c              CALL LMG7AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,A,IA,JA,U1,
c     1           FRHS,IG,ISIZ1,ISIZ2,ISIZ3,ISIZ4,KKITER,BCLOSE,DAMPLMG,
c     2           ICNVG,KKSTP,KKPER,MXITER,MXCYC,NCOL,NROW,NLAY,NODES,
c     3           HNOFLO,IOUT,IOUTAMG,ICG,IADAMPLMG,DUPLMG,DLOWLMG)
c            END IF
            IF (IUNIT(42).GT.0) THEN
                   CALL GMG7PNT(IGRID)
                   CALL GMG7AP(HNEW,RHS,CR,CC,CV,HCOF,HNOFLO,IBOUND,
     1                         IITER,MXITER,RCLOSEGMG,HCLOSEGMG,
     2                         KKITER,KKSTP,KKPER,NCOL,NROW,NLAY,ICNVG,
     3                         SITER,TSITER,DAMPGMG,IADAMPGMG,IOUTGMG,
     4                         IOUT,GMGID,
     5                         IUNITMHC,DUP,DLOW,CHGLIMIT,
     6                         BIGHEADCHG,HNEWLAST)
            ENDIF
           IF (IUNIT(IUPKS).GT.0) THEN
                   CALL PKS7PNT(IGRID)                
                   CALL PKS7AP(
     &                 HNEW,      IBOUND,      IACTCELL,  CR,           !004
     &                 CC,        CV,          HCOF,      RHS,          !008
     &                 ICNVG,     NSTP(KKPER), KKSTP,     KKPER,        !012
     &                 MXITER,    KKITER,      NCOL,      NROW,         !016
     &                 NLAY,      NODES,       HNOFLO,    IOUT,         !020
     &                 ISOLVER,   NPC,         NCORESM,   NCORESV,      !024
     &                 ICNVGOPT,  IEPFACT,     NRPROC,    INNERIT,      !028
     &                 NITERC,    NNZC,        NIAC,      NIABCGS,      !032
     &                 NGMRES,    NREST,       NIAPC,     NJAPC,        !036
     &                 NNZAPC,    NIAPCM,      NJAPCM,    NIWC,         !040
     &                 NWC,       HCLOSEPKS,   RCLOSEPKS, ISSFLG(KKPER),!044
     &                 NDAMPPKS,  DAMPPKS,     DAMPTPKS,  HPKS,         !048
     &                 NRESUP,    RELAXPKS,    IFILL,     NLEVELS,      !052
     &                 DROPTOL,   IPKSO,       IPKSI,     PKST,         !056
     &                 PKSPCU,    PKSPCA,      IPRPKS,    MUTPKS,       !060
     &                 IUNITPKS,  PKSCLEN,     PKSIT,     PKSHMXLOC,    !064
     &                 PKSHMX,    PKSRMXLOC,   PKSRMX,    PKSL2NORM,    !068
     &                 PKSRL2NORM,NODEC,       BC,        XC,           !072
     &                 AC,        IAC,         JAC,       IXMAP,        !076
     &                 IORD,      NIARO,       NNZRO,     LORDER,       !080
     &                 IORDER,    IARO,        JARO,      ARO,          !084
     &                 APC,       IAPC,        JAPC,      IAPCM,        !088
     &                 JAPCM,     IWC,         WC,        DC,           !092
     &                 ZC,        PC,          QC,        VC,           !096
     &                 DTILC,     PHATC,       DHATC,     CSG,          !100
     &                 SNG,       SG,          YG,        HG,           !104
     &                 VG,        ISCL,        SCL,       SCLI,         !108
     &                 A0,        IA0,         JA0,       EXPLINSYS)    !112
            END IF          
            IF(IERR.EQ.1) CALL USTOP(' ')

C
C7C2C---IF CONVERGENCE CRITERION HAS BEEN MET STOP ITERATING.
c            IF (ICNVG.EQ.1) GOTO 33
            if (icnvg.ne.1) then
               solverConverged=.false.
            else
               solverConverged=.true.
            endif
c  30      CONTINUE
c          KITER = MXITER

           !## could be solved based upon waterbalance error, maximum number of iterations finished
           psolved=.false.
           if(kiter.ge.mxiter)then
            psolved=.true.; solverconverged=.true.
           endif
           
           KITER=KKITER                                                 ! DLT: instances
      enddo                                                             ! DLT: instances
      call timing_toc('MODFLOW','SOL')                                  ! DLT
      return
      end
c ******************************************************************************
      subroutine mf2005_finishIter(converged,retVal)

      use m_mf2005_main

      implicit none

      ! arguments
      logical, intent(out)           :: converged        ! .false. not converged yet

      integer, intent(out)           :: retVal


c local variables
      integer    :: igrid
      ! ------------------------------

c init
      converged = .true.
      retVal    = 0


      do igrid=1,ninstance                                              ! DLT: instances
      call sgwf2ins1pnt(igrid)                                          ! DLT: instances
         converged=converged.and.solverConverged
      enddo

      return
      end
c ******************************************************************************

      subroutine mf2005_finishTimestepWrp0()
      ! wrapper routine
      implicit none

      ! local variables
      integer           :: retVal

      ! ------------------------------
      call mf2005_finishTimestep(retVal)
      return
      end

c =======
      subroutine mf2005_finishTimestep(retVal,psolved,ncvgerr)

      use m_mf2005_main
      use m_mf2005_iu

      implicit none
c arguments
      integer, intent(out)    :: retVal
      integer, intent(inout)  :: ncvgerr
      logical, intent(in)     :: psolved
      
c local variables
      integer  IBDRET,ic1,ic2,ir1,ir2,il1,il2,idir
      integer    :: igrid
      real :: budperc
c init
      call timing_tic('MODFLOW','OTBD')                                 ! DLT

      retVal=0

      do igrid=1,ninstance                                              ! DLT: instances
      call sgwf2ins1pnt(igrid)                                          ! DLT: instances
      call SGWF2BAS7PNT(IGRID)                                          ! DLT: instances

      timeStepCalculated=.true.                                         ! DLT: instances
C
c  33      CONTINUE
C
C7C3----DETERMINE WHICH OUTPUT IS NEEDED.
          CALL GWF2BAS7OC(KKSTP,KKPER,ICNVG,IUNIT(IUOC),IGRID)
C
C7C4----CALCULATE BUDGET TERMS. SAVE CELL-BY-CELL FLOW TERMS.
          MSUM = 1
          IF (IUNIT(IUBCF6).GT.0) THEN
            CALL GWF2BCF7BDS(KKSTP,KKPER,IGRID)
            CALL GWF2BCF7BDCH(KKSTP,KKPER,IGRID)
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 37 IDIR = 1, 3
              CALL GWF2BCF7BDADJ(KKSTP,KKPER,IDIR,IBDRET,
     1                          IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
   37       CONTINUE
          ENDIF
          IF(IUNIT(IULPF).GT.0) THEN
            CALL GWF2LPF7BDS(KKSTP,KKPER,IGRID)
            CALL GWF2LPF7BDCH(KKSTP,KKPER,IGRID)
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 157 IDIR=1,3
              CALL GWF2LPF7BDADJ(KKSTP,KKPER,IDIR,IBDRET,
     &                        IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
157         CONTINUE
          ENDIF
          IF(IUNIT(IUHUF2).GT.0) THEN
            CALL GWF2HUF7BDS(KKSTP,KKPER,IGRID)
            CALL GWF2HUF7BDCH(KKSTP,KKPER,IUNIT(IULVDA),IGRID)
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 159 IDIR=1,3
              CALL GWF2HUF7BDADJ(KKSTP,KKPER,IDIR,IBDRET,
     &                      IC1,IC2,IR1,IR2,IL1,IL2,IUNIT(IULVDA),IGRID)
159         CONTINUE
          ENDIF
          IF(IUNIT(IUWEL).GT.0) CALL GWF2WEL7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(IUDRN).GT.0) CALL GWF2DRN7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(IURIV).GT.0) CALL GWF2RIV7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(IUEVT).GT.0) THEN
             IF(IUNIT(IULAK).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
     1                                                     0,IGRID)
             CALL GWF2EVT7BD(KKSTP,KKPER,IGRID)
             IF(IUNIT(IULAK).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
     1                                                     1,IGRID)
          END IF
          IF(IUNIT(IUGHB).GT.0) CALL GWF2GHB7BD(KKSTP,KKPER,IGRID)
          if(IUNIT(IUDXC).gt.0) call gwf2dxc1bd(kkstp,kkper,igrid)
          IF(IUNIT(IURCH).GT.0) THEN
             IF(IUNIT(IULAK).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
     1                                                     0,IGRID)
             CALL GWF2RCH7BD(KKSTP,KKPER,IGRID)
             IF(IUNIT(IULAK).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
     1                                                     1,IGRID)
          END IF
          IF(IUNIT(IUFHB).GT.0) CALL GWF2FHB7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(IURES).GT.0) CALL GWF2RES7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(IUSTR).GT.0) CALL GWF2STR7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(IUIBS).GT.0) CALL GWF2IBS7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(IUETS).GT.0) CALL GWF2ETS7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(IUDRT).GT.0) CALL GWF2DRT7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(IUUZF).GT.0) CALL GWF2UZF1BD(KKSTP,KKPER,
     1                             IUNIT(IULAK),IUNIT(IUSFR),IGRID)
          IF(IUNIT(IUSFR).GT.0) CALL GWF2SFR7BD(KKSTP,KKPER,
     1                        IUNIT(IUGWT),IUNIT(IULAK),IUNIT(IUGAGE),
     1                        IUNIT(IUUZF),NSOL,IUNIT(IURCH),IGRID)

!SUBROUTINE GWF2SFR7BD(Kkstp, Kkper, Iunitgwt, Iunitlak, Iunitgage,
!     +                      Iunituzf, Nsol, Iunitrch, Igrid)  !cjm (added Iunitrch)     
     
      IF(IUNIT(IULAK).GT.0) CALL GWF2LAK7BD(KKSTP,KKPER,
     1                       IUNIT(IUGWT),IUNIT(IUGAGE),IUNIT(IUSFR),
     1                       IUNIT(IUUZF),NSOL,IGRID)
          IF(IUNIT(IUMNW2).GT.0) CALL GWF2MNW27BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(IUMNW1).GT.0) CALL GWF2MNW17BD(NSTP(KPER),KKSTP,
     1                      KKPER,IGRID)
          IF(IUNIT(IUSUB).GT.0) CALL GWF2SUB7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(IUSWT).GT.0) CALL GWF2SWT7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(IUSCR).GT.0) CALL GWF2SCR7BD(KKSTP,KKPER,IGRID)

CLMT
CLMT----CALL LINK-MT3DMS SUBROUTINES TO SAVE FLOW-TRANSPORT LINK FILE
CLMT----FOR USE BY MT3DMS FOR TRANSPORT SIMULATION
CLMT
          INCLUDE 'lmt7.inc'
CLMT
C
C  Observation and hydrograph simulated equivalents
          CALL OBS2BAS7SE(IUNIT(IUHOB),IGRID)
          IF(IUNIT(IUDROB).GT.0) CALL OBS2DRN7SE(IGRID)
          IF(IUNIT(IURVOB).GT.0) CALL OBS2RIV7SE(IGRID)
          IF(IUNIT(IUGBOB).GT.0) CALL OBS2GHB7SE(IGRID)
          IF(IUNIT(IUSTOB).GT.0) CALL OBS2STR7SE(IGRID)
          IF(IUNIT(IUCHOB).GT.0) CALL OBS2CHD7SE(KKPER,IGRID)
          IF(IUNIT(IUHYD).GT.0) CALL GWF2HYD7BAS7SE(1,IGRID)
          IF(IUNIT(IUHYD).GT.0 .AND. IUNIT(IUIBS).GT.0)
     1                              CALL GWF2HYD7IBS7SE(1,IGRID)
          IF(IUNIT(IUHYD).GT.0 .AND. IUNIT(IUSUB).GT.0)
     1                              CALL GWF2HYD7SUB7SE(1,IGRID)
          IF(IUNIT(IUHYD).GT.0 .AND. IUNIT(IUSTR).GT.0)
     1                              CALL GWF2HYD7STR7SE(1,IGRID)
          IF(IUNIT(IUHYD).GT.0 .AND. IUNIT(IUSFR).GT.0)
     1                              CALL GWF2HYD7SFR7SE(1,IGRID)
C
C7C5---PRINT AND/OR SAVE DATA.
          CALL GWF2BAS7OT(KKSTP,KKPER,ICNVG,1,IGRID,BUDPERC)
          IF(IUNIT(IUIBS).GT.0) CALL GWF2IBS7OT(KKSTP,KKPER,
     1                            IUNIT(IUIBS),IGRID)
          IF(IUNIT(IUHUF2).GT.0)THEN
            IF(IOHUFHDS .NE.0 .OR.IOHUFFLWS .NE.0)
     1         CALL GWF2HUF7OT(KKSTP,KKPER,ICNVG,1,IGRID)
          ENDIF
          IF(IUNIT(IUMNWI).NE.0) CALL GWF2MNW2I7OT(NSTP(KKPER),KKSTP,
     1                       KKPER,IGRID)
          IF(IUNIT(IUSUB).GT.0) CALL GWF2SUB7OT(KKSTP,KKPER,
     1                                IUNIT(IUSUB),IGRID)
          IF(IUNIT(IUSWT).GT.0) CALL GWF2SWT7OT(KKSTP,KKPER,IGRID)
c SUB-Creep
          IF(IUNIT(IUSCR).GT.0) CALL GWF2SCR7OT(KKSTP,KKPER,IGRID)
c SUB-Creep end
          IF(IUNIT(IUHYD).GT.0) CALL GWF2HYD7BAS7OT(KKSTP,KKPER,IGRID)
C
C7C6---JUMP TO END OF PROGRAM IF CONVERGENCE WAS NOT ACHIEVED.
c          IF(ICNVG.EQ.0) GO TO 110
          !## check waterbalance whether to continue yes/no
          if(psolved)then
            NCVGERR=NCVGERR+1
            WRITE(IOUT,87) BUDPERC
   87       FORMAT(1X,'FAILURE TO MEET SOLVER CONVERGENCE CRITERIA',/
     1       1X,'BUDGET PERCENT DISCREPANCY IS',F10.4)
            IF(ABS(BUDPERC).GT.STOPER) THEN
              WRITE(IOUT,*) 'STOPPING SIMULATION'
              stop
!              GO TO 110
            ELSE
              WRITE(IOUT,*) 'CONTINUING EXECUTION'
              ICNVG=1
            END IF
          END IF

          call splitfiles(iout,igrid)                                   ! DLT

C-----END OF TIME STEP (KSTP) AND STRESS PERIOD (KPER) LOOPS
c   90   CONTINUE
c  100 CONTINUE
      enddo                                                                     ! DLT: instances
      call timing_toc('MODFLOW','OTBD')                                 ! DLT
      
      return
      end
c ******************************************************************************
      subroutine mf2005_finishSimulationWrp0()
      ! wrapper routine
      implicit none

      ! local variables
      integer           :: retVal
      ! ------------------------------

      call mf2005_finishSimulation(retVal)
      return
      end

c =======
      subroutine mf2005_finishSimulation(retVal)

      use m_mf2005_main
      use m_mf2005_iu

      implicit none

c arguments
      integer, intent(out)    :: retVal

c functions
      logical :: pks7mpimasterwrite 
      
c local variable
      integer    :: igrid

c init
      retVal = 0


      do igrid=1,ninstance                                                      ! DLT: instances
      call sgwf2ins1pnt(igrid)                                                  ! DLT: instances
      call SGWF2BAS7PNT(IGRID)                                                  ! DLT: instances

C
C
      IF(IUNIT(IUMNW1).NE.0) CALL GWF2MNW17OT(IGRID)
C
C8------END OF SIMULATION
C-------SAVE RESTART RECORDS FOR SUB PACKAGE
  110 IF(IUNIT(IUSUB).GT.0) CALL GWF2SUB7SV(IGRID)
C
C  Observation output
      IF(IUNIT(IUHOB).GT.0) CALL OBS2BAS7OT(IUNIT(IUHOB),IGRID)
      IF(IUNIT(IUDROB).GT.0) CALL OBS2DRN7OT(IGRID)
      IF(IUNIT(IURVOB).GT.0) CALL OBS2RIV7OT(IGRID)
      IF(IUNIT(IUGBOB).GT.0) CALL OBS2GHB7OT(IGRID)
      IF(IUNIT(IUSTOB).GT.0) CALL OBS2STR7OT(IGRID)
      IF(IUNIT(IUCHOB).GT.0) CALL OBS2CHD7OT(IGRID)
      CALL GLO1BAS6ET(IOUT,IBDT,1)
C
C9------CLOSE FILES AND DEALLOCATE MEMORY.  GWF2BAS7DA MUST BE CALLED
C9------LAST BECAUSE IT DEALLOCATES IUNIT.
      CALL SGWF2BAS7PNT(IGRID)
      IF(IUNIT(IUBCF6).GT.0) CALL GWF2BCF7DA(IGRID)
      IF(IUNIT(IUWEL).GT.0) CALL GWF2WEL7DA(IGRID)
      IF(IUNIT(IUDRN).GT.0) CALL GWF2DRN7DA(IGRID)
      IF(IUNIT(IURIV).GT.0) CALL GWF2RIV7DA(IGRID)
      IF(IUNIT(IUEVT).GT.0) CALL GWF2EVT7DA(IGRID)
      IF(IUNIT(IUGHB).GT.0) CALL GWF2GHB7DA(IGRID)
      IF(IUNIT(IURCH).GT.0) CALL GWF2RCH7DA(IGRID)
      IF(IUNIT(IUSIP).GT.0) CALL SIP7DA(IGRID)
      IF(IUNIT(IUDE4).GT.0) CALL DE47DA(IGRID)
      IF(IUNIT(IUPCG).GT.0) CALL PCG7DA(IGRID)
      IF(IUNIT(IUPKS).GT.0) CALL PKS7DA(IGRID)                          !PKS
c      IF(IUNIT(IULMG).GT.0) CALL LMG7DA(IGRID)
      IF(IUNIT(IUFHB).GT.0) CALL GWF2FHB7DA(IGRID)
      IF(IUNIT(IURES).GT.0) CALL GWF2RES7DA(IGRID)
      IF(IUNIT(IUSTR).GT.0) CALL GWF2STR7DA(IGRID)
      IF(IUNIT(IUIBS).GT.0) CALL GWF2IBS7DA(IGRID)
      IF(IUNIT(IUCHD).GT.0) CALL GWF2CHD7DA(IGRID)
      IF(IUNIT(IUHFB6).GT.0) CALL GWF2HFB7DA(IGRID)
      IF(IUNIT(IULAK).GT.0 .OR. IUNIT(IUSFR).GT.0)
     1      CALL GWF2LAK7DA(IUNIT(IULAK),IGRID)
      IF(IUNIT(IULPF).GT.0) CALL GWF2LPF7DA(IGRID)
      IF(IUNIT(IUHUF2).GT.0) CALL GWF2HUF7DA(IGRID)
      IF(IUNIT(IUETS).GT.0) CALL GWF2ETS7DA(IGRID)
      IF(IUNIT(IUDRT).GT.0) CALL GWF2DRT7DA(IGRID)
      IF(IUNIT(42).GT.0) CALL GMG7DA(IGRID)
      IF(IUNIT(IUSFR).GT.0) CALL GWF2SFR7DA(IGRID)
      IF(IUNIT(IUGAGE).GT.0) CALL GWF2GAG7DA(IGRID)
      IF(IUNIT(IUMNW2).GT.0) CALL GWF2MNW27DA(IGRID)
      IF(IUNIT(IUMNWI).GT.0) CALL GWF2MNW2I7DA(IGRID)
      IF(IUNIT(IUMNW1).GT.0) CALL GWF2MNW17DA(IGRID)
      IF(IUNIT(IUSUB).GT.0) CALL GWF2SUB7DA(IGRID)
      IF(IUNIT(IUUZF).GT.0) CALL GWF2UZF1DA(IGRID)
      IF(IUNIT(IUSWT).GT.0) CALL GWF2SWT7DA(IGRID)
      IF(IUNIT(IUSCR).GT.0) CALL GWF2SCR7DA(IGRID)
      CALL OBS2BAS7DA(IUNIT(IUHOB),IGRID)
      IF(IUNIT(IUDROB).GT.0) CALL OBS2DRN7DA(IGRID)
      IF(IUNIT(IURVOB).GT.0) CALL OBS2RIV7DA(IGRID)
      IF(IUNIT(IUGBOB).GT.0) CALL OBS2GHB7DA(IGRID)
      IF(IUNIT(IUSTOB).GT.0) CALL OBS2STR7DA(IGRID)
      IF(IUNIT(IUCHOB).GT.0) CALL OBS2CHD7DA(IGRID)
      IF(IUNIT(IUHYD).GT.0) CALL GWF2HYD7DA(IGRID)
      if(IUNIT(IUANI).gt.0) call gwf2ani3da(igrid)                      ! ANI
      if(IUNIT(IUMET).gt.0) call gwf2met1da(igrid)                      ! MET

      CALL GWF2BAS7DA(IGRID)
C
C10-----END OF PROGRAM.
      IF(ICNVG.EQ.0) THEN
        if (pks7mpimasterwrite())
     &    WRITE(*,*) 'FAILED TO MEET SOLVER CONVERGENCE CRITERIA'
        retVal = -1
      ELSE
        if (pks7mpimasterwrite())
     &    WRITE(*,*) ' Normal termination of simulation'
      END IF

      call sgwf2ins1da(igrid)

c      CALL USTOP(' ')
C
      enddo                                                             ! DLT: instances
      return
      END
      SUBROUTINE GETNAMFIL(FNAME)
C     ******************************************************************
C     GET THE NAME OF THE NAME FILE
C     ******************************************************************
C        SPECIFICATIONS:
C
C     ------------------------------------------------------------------
      CHARACTER*(*) FNAME
      CHARACTER*200 COMLIN
      LOGICAL EXISTS
C     ------------------------------------------------------------------
C
C Get name file from command line or user interaction.
        FNAME=' '
        COMLIN=' '
C *** Subroutines GETARG and GETCL are extensions to Fortran 90/95 that
C *** allow a program to retrieve command-line arguments.  To enable
C *** Modflow-2000 to read the name of a Name file from the command
C *** line, either GETARG or GETCL must be called, but not both.  As
C *** distributed, the call to GETARG is uncommented.  For compilers
C *** that support GETCL but not GETARG, comment out the call to GETARG
C *** and uncomment the call to GETCL.  The calls to both GETARG and
C *** GETCL may be commented out for compilers that do not support
C *** either extension.
        CALL GETARG(1,COMLIN)
C        CALL GETCL(COMLIN)
        ICOL = 1
        IF(COMLIN.NE.' ') THEN
          FNAME=COMLIN
        ELSE
   15     WRITE (*,*) ' Enter the name of the NAME FILE: '
          READ (*,'(A)') FNAME
          CALL URWORD(FNAME,ICOL,ISTART,ISTOP,0,N,R,0,0)
          FNAME=FNAME(ISTART:ISTOP)
          IF (FNAME.EQ.' ') GOTO 15
        ENDIF
        INQUIRE (FILE=FNAME,EXIST=EXISTS)
        IF(.NOT.EXISTS) THEN
          NC=INDEX(FNAME,' ')
          FNAME(NC:NC+3)='.nam'
          INQUIRE (FILE=FNAME,EXIST=EXISTS)
          IF(.NOT.EXISTS) THEN
            WRITE (*,480) FNAME(1:NC-1),FNAME(1:NC+3)
  480       FORMAT(1X,'Can''t find name file ',A,' or ',A)
            CALL USTOP(' ')
          ENDIF
        ENDIF
C
      RETURN
      END
      SUBROUTINE GLO1BAS6ET(IOUT,IBDT,IPRTIM)
C     ******************************************************************
C     Get end time and calculate elapsed time
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IBDT(8), IEDT(8), IDPM(12)
      DATA IDPM/31,28,31,30,31,30,31,31,30,31,30,31/ ! Days per month
      DATA NSPD/86400/  ! Seconds per day
      logical :: pks7mpimasterwrite                                     ! PKS
C     ------------------------------------------------------------------
C
C     Get current date and time, assign to IEDT, and write.
      CALL DATE_AND_TIME(VALUES=IEDT)
      if (pks7mpimasterwrite()) 
     &WRITE(*,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
 1000 FORMAT(1X,'Run end date and time (yyyy/mm/dd hh:mm:ss): ',
     &I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2)
      IF(IPRTIM.GT.0) THEN
        WRITE(IOUT,'(1X)')
        WRITE(IOUT,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
      END IF
C
C     Calculate elapsed time in days and seconds
      NDAYS=0
      LEAP=0
      IF (MOD(IEDT(1),4).EQ.0) LEAP = 1
      IBD = IBDT(3)            ! BEGIN DAY
      IED = IEDT(3)            ! END DAY
C     FIND DAYS
      IF (IBDT(2).NE.IEDT(2)) THEN
C       MONTHS DIFFER
        MB = IBDT(2)             ! BEGIN MONTH
        ME = IEDT(2)             ! END MONTH
        NM = ME-MB+1             ! NUMBER OF MONTHS TO LOOK AT
        IF (MB.GT.ME) NM = NM+12
        MC=MB-1
        DO 10 M=1,NM
          MC=MC+1                ! MC IS CURRENT MONTH
          IF (MC.EQ.13) MC = 1
          IF (MC.EQ.MB) THEN
            NDAYS = NDAYS+IDPM(MC)-IBD
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ELSEIF (MC.EQ.ME) THEN
            NDAYS = NDAYS+IED
          ELSE
            NDAYS = NDAYS+IDPM(MC)
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ENDIF
   10   CONTINUE
      ELSEIF (IBD.LT.IED) THEN
C       START AND END IN SAME MONTH, ONLY ACCOUNT FOR DAYS
        NDAYS = IED-IBD
      ENDIF
      ELSEC=NDAYS*NSPD
C
C     ADD OR SUBTRACT SECONDS
      ELSEC = ELSEC+(IEDT(5)-IBDT(5))*3600.0
      ELSEC = ELSEC+(IEDT(6)-IBDT(6))*60.0
      ELSEC = ELSEC+(IEDT(7)-IBDT(7))
      ELSEC = ELSEC+(IEDT(8)-IBDT(8))*0.001
C
C     CONVERT SECONDS TO DAYS, HOURS, MINUTES, AND SECONDS
      NDAYS = ELSEC/NSPD
      RSECS = MOD(ELSEC,86400.0)
      NHOURS = RSECS/3600.0
      RSECS = MOD(RSECS,3600.0)
      NMINS = RSECS/60.0
      RSECS = MOD(RSECS,60.0)
      NSECS = RSECS
      RSECS = MOD(RSECS,1.0)
      MSECS = NINT(RSECS*1000.0)
      NRSECS = NSECS
      IF (RSECS.GE.0.5) NRSECS=NRSECS+1
C
C     Write elapsed time to screen
        IF (NDAYS.GT.0) THEN
          if (pks7mpimasterwrite()) 
     &      WRITE(*,1010) NDAYS,NHOURS,NMINS,NRSECS
 1010     FORMAT(1X,'Elapsed run time: ',I3,' Days, ',I2,' Hours, ',I2,
     &      ' Minutes, ',I2,' Seconds',/)
        ELSEIF (NHOURS.GT.0) THEN
          if (pks7mpimasterwrite()) WRITE(*,1020) NHOURS,NMINS,NRSECS
 1020     FORMAT(1X,'Elapsed run time: ',I2,' Hours, ',I2,
     &      ' Minutes, ',I2,' Seconds',/)
        ELSEIF (NMINS.GT.0) THEN
          if (pks7mpimasterwrite()) WRITE(*,1030) NMINS,NSECS,MSECS
 1030     FORMAT(1X,'Elapsed run time: ',I2,' Minutes, ',
     &      I2,'.',I3.3,' Seconds',/)
        ELSE
          if (pks7mpimasterwrite()) WRITE(*,1040) NSECS,MSECS
 1040     FORMAT(1X,'Elapsed run time: ',I2,'.',I3.3,' Seconds',/)
        ENDIF
C
C     Write times to file if requested
      IF(IPRTIM.GT.0) THEN
        IF (NDAYS.GT.0) THEN
          WRITE(IOUT,1010) NDAYS,NHOURS,NMINS,NRSECS
        ELSEIF (NHOURS.GT.0) THEN
          WRITE(IOUT,1020) NHOURS,NMINS,NRSECS
        ELSEIF (NMINS.GT.0) THEN
          WRITE(IOUT,1030) NMINS,NSECS,MSECS
        ELSE
          WRITE(IOUT,1040) NSECS,MSECS
        ENDIF
      ENDIF
C
      RETURN
      END

c ******************************************************************************

      function sutl_getTimeStepLength(igrid)

c description:
c ------------------------------------------------------------------------------
c get the timesteplength in days of the current timestep of model igrid
c

c declaration section
c ------------------------------------------------------------------------------
      use m_mf2005_main, only: GWFBASDAT,GLOBALDAT,mi
      use GLOBAL, only : ISSFLG

      implicit none

c function declaration
      double precision     :: sutl_getTimeStepLength   ! return value: >0: time step length in days
                                                       !              <=0: no more timesteps left

c arguments
      integer, intent(in)  :: igrid             !> grid number of modflow

c local variables
      double precision     :: dfact
      logical              :: lastStep
      integer              :: lkper,lkstp,gnper,gnstp,tkper

c program section
c ------------------------------------------------------------------------------

      ! check or any timesteps are left
      lastStep=.false.
      lkper=mi(igrid)%kper
      lkstp=mi(igrid)%kstp
      gnper=GLOBALDAT(igrid)%nper
      tkper=min(max(1,lkper),gnper)   ! to be sure stressperiod number in interval [1,nper]
      gnstp=GLOBALDAT(igrid)%nstp(tkper)
      if (lkper.gt.gnper                     ) lastStep=.true.
      if (lkper.eq.gnper .and. lkstp.gt.gnstp) lastStep=.true.

      if (lastStep) then
         ! no more timesteps left
         sutl_getTimeStepLength = -1.0d0
      else
         ! get conversion factor from model time to days
         select case( GLOBALDAT(igrid)%itmuni )
            case (1)      ! seconds
               dfact=1.0d0/86400.d0
            case (2)      ! minutes
               dfact=1.0d0/1440.d0
            case (3)      ! hours
               dfact=1.0d0/24.d0
            case (4)      ! days
               dfact=1.0d0
            case (5)      ! years
               dfact=365.d0
            case default  ! unknown
               dfact=1.0d0
         end select

         !## make sure delt is zero for steady-state
         if(ISSFLG(lkper).ne.0)dfact=0.0
         
         ! assign functionvalue
         sutl_getTimeStepLength = dfact*GWFBASDAT(igrid)%DELT
      endif

c end of program
      return
      end

c ******************************************************************************

      function sutl_getLengthTotalStressPeriod(igrid)

c description:
c ------------------------------------------------------------------------------
c get the timesteplength in days of the current timestep of model igrid
c

c declaration section
c ------------------------------------------------------------------------------
      use m_mf2005_main, only: GWFBASDAT,GLOBALDAT,mi

      implicit none

c function declaration
      double precision     :: sutl_getLengthTotalStressPeriod   ! return value: >0: time step length in days
                                                       !              <=0: no more timesteps left

c arguments
      integer, intent(in)  :: igrid             !> grid number of modflow

c local variables
      double precision     :: dfact
!      logical              :: lastStep
      integer              :: lkper !,lkstp,gnper,gnstp,tkper

c program section
c ------------------------------------------------------------------------------

!      ! check or any timesteps are left
!      lastStep=.false.
      lkper=mi(igrid)%kper
!      lkstp=mi(igrid)%kstp
!      gnper=GLOBALDAT(igrid)%nper
!      tkper=min(max(1,lkper),gnper)   ! to be sure stressperiod number in interval [1,nper]
!      gnstp=GLOBALDAT(igrid)%nstp(tkper)

         ! get conversion factor from model time to days
         select case( GLOBALDAT(igrid)%itmuni )
            case (1)      ! seconds
               dfact=1.0d0/86400.d0
            case (2)      ! minutes
               dfact=1.0d0/1440.d0
            case (3)      ! hours
               dfact=1.0d0/24.d0
            case (4)      ! days
               dfact=1.0d0
            case (5)      ! years
               dfact=365.d0
            case default  ! unknown
               dfact=1.0d0
         end select

         ! assign functionvalue
      sutl_getLengthTotalStressPeriod = 
     1    dfact*GLOBALDAT(igrid)%PERLEN(lkper)

c end of program
      return
      end

      logical function mf2005_lastTimeStep(igrid)
c description:
c ------------------------------------------------------------------------------
c check is this was the last timestep
c

c declaration section
c ------------------------------------------------------------------------------
      use m_mf2005_main, only: GWFBASDAT,GLOBALDAT,mi

      implicit none

c arguments
      integer, intent(in)  :: igrid             !> grid number of modflow

c local variables
      logical :: lastStep
      integer :: lkper,lkstp,gnper,gnstp,tkper

c program section
c ------------------------------------------------------------------------------
      lastStep=.false.
      lkper=mi(igrid)%kper
      lkstp=mi(igrid)%kstp
      gnper=GLOBALDAT(igrid)%nper
      tkper=min(max(1,lkper),gnper)   ! to be sure stressperiod number in interval [1,nper]
      gnstp=GLOBALDAT(igrid)%nstp(tkper)

      if (lkper.gt.gnper                     ) lastStep=.true.
      if (lkper.eq.gnper .and. lkstp.ge.gnstp) lastStep=.true.
      mf2005_lastTimeStep = lastStep

      return
      end

      !> Return number of MODFLOW grids.
      logical function mf2005_PutNumberOfGrids(nGrids)

!...     modules
      use m_mf2005_main, only: ninstance

      implicit none

!...    arguments
      integer, intent(out) :: nGrids

!...     locals
      logical :: ok
!.......................................................................

      nGrids = ninstance

      ok = .true.
      if (nGrids.le.0) ok = .false.
      mf2005_PutNumberOfGrids = ok

      end function

      !> return dimensions of the MODFLOW grid.
      logical function mf2005_PutGridDimensions(igrid,nRows,nColumns,
     1                                          nLayers)
!...     modules
      use global, only: ncol, nrow, nlay

      implicit none

!...     arguments
      integer, intent(in)  :: igrid
      integer, intent(out) :: nRows
      integer, intent(out) :: nColumns
      integer, intent(out) :: nLayers

!...     locals
      logical :: ok
!.......................................................................

!...     get pointers
      call sgwf2bas7pnt(igrid)

!...     checks
      ok = .true.
      if (.not.associated(nrow)) then
         ok = .false.
      else
         if (nrow.le.0) ok = .false.
      end if
      if (.not.associated(ncol)) then
         ok = .false.
      else
         if (ncol.le.0) ok = .false.
      end if
      if (.not.associated(nlay)) then
         ok = .false.
      else
         if (ncol.le.0) ok = .false.
      end if

      if (ok) then
         nRows    = nrow
         nColumns = ncol
         nLayers  = nlay
      end if

      mf2005_PutGridDimensions = ok

      end function

      !> Return number of exchange items (IDs).
      logical function mf2005_PutModSimNumberOfIDs(igrid,mini,
     1   maxi,nxch)
!...     modules
      use gwfdxcmodule, only: minid, maxid, ndxc

      implicit none

!...     arguments
      integer, intent(in) :: igrid
      integer, intent(out) :: mini
      integer, intent(out) :: maxi
      integer, intent(out) :: nxch

!...     locals
      logical :: ok
!.......................................................................

!...     get pointers
      call sgwf2dxc1pnt(igrid)

!...     check
      ok = .true.
      if (.not.associated(ndxc)) then
         ok = .false.
         mf2005_PutModSimNumberOfIDs = ok
         return
      end if
      mini = minid
      maxi = maxid
      nxch = ndxc

      !if (nxch.le.0) ok = .false.
      mf2005_PutModSimNumberOfIDs = ok

      end function

      !> Return MODFLOW exchange IDs.
      logical function mf2005_PutModSimIDs(igrid,ids)

!...     modules
      use gwfdxcmodule, only: dxcid, ndxc

      implicit none

!...     arguments
      integer, intent(in) :: igrid
      integer, dimension(*), intent(out) :: ids

!...     locals
      logical :: ok
      integer :: i
!.......................................................................

!...     set pointers
      call sgwf2dxc1pnt(igrid)

!...     check
      ok = .true.
      if (.not.associated(dxcid)) then
         ok = .false.
         mf2005_PutModSimIDs = ok
         return
      end if

!...     Copy the IDs
      do i = 1, ndxc
         ids(i) = dxcid(i)
      end do

      mf2005_PutModSimIDs = ok

      end function

      !> return exchange MODFLOW cells (layer, row, column)
      logical function mf2005_PutModSimCells(igrid,cells)

!...     modules
      use gwfdxcmodule, only: ndxc, dxcil, dxcir, dxcic

      implicit none

!...     arguments
      integer, intent(in) :: igrid
      integer, dimension(3,*), intent(out) :: cells

!...     locals
      logical :: ok
      integer :: i
!.......................................................................

!...     set pointers
      call sgwf2dxc1pnt(igrid)

!...     check
      ok = .true.
      if (.not.associated(ndxc))  ok = .false.
      if (.not.associated(dxcil)) ok = .false.
      if (.not.associated(dxcir)) ok = .false.
      if (.not.associated(dxcic)) ok = .false.
      if (.not.ok) then
         mf2005_PutModSimCells = ok
         return
      end if

!...     copy cells
      do i = 1, ndxc
         cells(1,i) = dxcil(i)
         cells(2,i) = dxcir(i)
         cells(3,i) = dxcic(i)
      end do

      mf2005_PutModSimCells = ok

      end function

      !> return exchange MODFLOW cells (layer, row, column)
      logical function mf2005_PutModMozCells(igrid,cells)

!...     modules
      use gwfdxcmodule, only: ndxclsw, dxcirlsw, dxciclsw

      implicit none

!...     arguments
      integer, intent(in) :: igrid
      integer, dimension(3,*), intent(out) :: cells

!...     locals
      logical :: ok
      integer :: i
!.......................................................................

!...     set pointers
      call sgwf2dxc1pnt(igrid)

!...     check
      ok = .true.
      if (.not.associated(ndxclsw))  ok = .false.
      if (.not.associated(dxcirlsw)) ok = .false.
      if (.not.associated(dxciclsw)) ok = .false.
      if (.not.ok) then
         mf2005_PutModMozCells = ok
         return
      end if

!...     copy cells
      do i = 1, ndxclsw
         cells(1,i) = 1
         cells(2,i) = dxcirlsw(i)
         cells(3,i) = dxciclsw(i)
      end do

      mf2005_PutModMozCells = ok

      end function

      !> Return steady-state flag.
      logical function mf2005_PutSimulationType(igrid, lss)

!...     modules
      use global, only: nper, issflg

      implicit none

!...     arguments
      integer, intent(in) :: igrid
      logical, intent(out) :: lss

!...     locals
      logical :: ok
      integer :: i
!.......................................................................

!...     get pointers
      call sgwf2bas7pnt(igrid)

!...     check
      ok = .true.
      if (.not.associated(nper)) then
         ok = .false.
      else
         if (nper.le.0) ok = .false.
      end if
      if (.not.associated(issflg)) ok = .false.
      if (.not.ok) then
         mf2005_PutSimulationType = ok
         return
      end if

!...     find out or model is steady state or not
      lss = .true.
      do i=1, nper
         if (issflg(i).eq.0) lss = .false.
      enddo

      mf2005_PutSimulationType = ok
      end function

      !> Return flag for LPF activation.
      logical function mf2005_PutLPFActive(igrid, llpf)

!...     modules
      use global, only: iunit
      use m_mf2005_iu, only: iulpf

      implicit none

!...     arguments
      integer, intent(in) :: igrid
      logical, intent(out) :: llpf

!...     locals
      logical :: ok
      integer :: i
!.......................................................................

!...     get pointers
      call sgwf2bas7pnt(igrid)

      llpf = .false.
      if(IUNIT(IULPF).gt.0) llpf = .true.

      ok = .true.
      mf2005_PutLPFActive = ok
      end function

      !> Return flag for LPF activation.
      logical function mf2005_PutPWTActive(igrid, lpwt)

!...     modules
      use global, only: iunit
      use m_mf2005_iu, only: iupwt

      implicit none

!...     arguments
      integer, intent(in) :: igrid
      logical, intent(out) :: lpwt

!...     locals
      logical :: ok
      integer :: i
!.......................................................................

!...     get pointers
      call sgwf2bas7pnt(igrid)

      lpwt = .false.
      if(iunit(iupwt).gt.0) lpwt = .true.

      ok = .true.
      mf2005_PutPWTActive = ok
      end function

      !> Return hnoflo
      logical function mf2005_PutHeadNoFlo(igrid, h)

!...     modules
      use gwfbasmodule, only: hnoflo

      implicit none

!...     arguments
      integer, intent(in) :: igrid
      real, intent(out) :: h

!...     locals
      logical :: ok
      integer :: i
!.......................................................................

!...     get pointers
      call sgwf2bas7pnt(igrid)

      h = hnoflo

      mf2005_PutHeadNoFlo = .true.

      end function

      logical function mf2005_PutHeads(igrid,iliric,n,head,mv)
!...     modules
      use global, only: ibound, hnew

      implicit none

!...     arguments
      integer, intent(in) :: igrid, n
      integer, dimension(3,*), intent(in) :: iliric
      real, intent(in) :: mv
      real, dimension(*), intent(out) :: head

!...     locals
      integer :: i, icol, irow, ilay

      ! get pointers
      call sgwf2bas7pnt(igrid)

      do i = 1, n
         ilay = iliric(1,i)
         if(ilay.eq.0) then
            head(i) = mv
            cycle
         end if   
         irow = iliric(2,i)
         icol = iliric(3,i)
         if (ibound(icol,irow,ilay).ne.0) then
            head(i) = hnew(icol,irow,ilay)
         else
            head(i) = mv
         endif
      end do

      mf2005_PutHeads = .true.
      end function

      logical function mf2005_GetUnsaturatedZoneFlux(igrid,
     1                                   nid,unsflux,xchIdx,xchOff,mv)
!...     modules
      use gwfbasmodule, only: delt
      use gwfdxcmodule, only: dxcuzflux
      use pks_imod_utl, only: pks_imod_utl_iarmwp_xch

      implicit none

      ! arguments
      integer, intent(in)                    :: igrid
      integer, intent(in)                    :: nid
      real, dimension(*), intent(inout)      :: unsflux
      integer, dimension(*), intent(in)      :: xchIdx
      integer, dimension(*), intent(in)      :: xchOff
      real, intent(in)                       :: mv

!...     locals
      integer :: i, j, js, je, k, ilay, irow, icol
      real :: q
!.......................................................................

      ! get pointers
      call sgwf2bas7pnt(igrid)
      call sgwf2dxc1pnt(igrid)

      dxcuzflux = 0.0
      js = 1
      do i = 1, nid         
         je = xchOff(i)
         if (je-js.gt.0) then
            !write(*,*) 'Warning: cell received > 1 uszflux'
         end if   
         do j = js, je
            k = xchIdx(j)
            if (k.le.0) then
               write(*,*) 'Error: coupling node index out of range'
               stop 1
            end if
            q = unsflux(k)
            if (q.ne.mv) then
               ! the values got are volumes, recalculate them to fluxes
               q = q/delt
            else
               q = 0.0
            end if
            dxcuzflux(i) = dxcuzflux(i) + q
         end do
         js = je + 1
      end do

      call pks_imod_utl_iarmwp_xch(dxcuzflux,'q')   
         
      ! save pointers
      call sgwf2dxc1psv(igrid)

      mf2005_GetUnsaturatedZoneFlux = .true.
      end function

      !> Set storage factor.
      logical function mf2005_GetStorageFactorLPF(igrid,strfct,
     1                                      nid,xchIdx,xchOff,mv)
!...     modules
      use gwflpfmodule, only: sc1

      use gwfdxcmodule, only: dxcic, dxcir, dxcil

      implicit none

!...     arguments
      integer, intent(in)                 :: igrid
      integer, intent(in)                 :: nid
      real, dimension(*), intent(inout)   :: strfct
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(*), intent(in)   :: xchOff
      real, intent(in)                    :: mv

!...     locals
      logical :: ok
      integer :: i, j, js, je, k, ilay, irow, icol
      real :: s
!.......................................................................

!...     get pointers
      call sgwf2lpf7pnt(igrid)
      call sgwf2dxc1pnt(igrid)

      ok = .true.
      js = 1
      do i = 1, nid
         ilay = dxcil(i)
         if(ilay.eq.0) cycle
         irow = dxcir(i)
         icol = dxcic(i)
         je = xchOff(i)
         if (je-js.gt.0) then 
            !write(*,*) 'Warning: cell received > 1 sf1'
         end if   
         s = mv
         do j = js, je
            k = xchIdx(j)
            if (k.le.0) then
               ok = .false.
               mf2005_GetStorageFactorLPF = ok
               return
            end if
            if (strfct(k).ne.mv) then
               if (s.eq.mv) s = 0.0
               s = s + strfct(k)
            end if
         end do
         if (s.ne.mv) then
            ! set storage factor
            sc1(icol,irow,ilay) = s
         endif
         js = je + 1
      end do

!...     pointer save
      call gwf2lpf7psv(igrid)

      mf2005_GetStorageFactorLPF = ok
      end function

      !> Set storage factor.
      logical function mf2005_GetStorageFactor(igrid,strfct,
     1                                      nid,xchIdx,xchOff,mv)
!...     modules
      use gwfbcfmodule, only: sc1
      use gwfdxcmodule, only: dxcic, dxcir, dxcil

      implicit none

!...     arguments
      integer, intent(in)                 :: igrid
      integer, intent(in)                 :: nid
      real, dimension(*), intent(inout)   :: strfct
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(*), intent(in)   :: xchOff
      real, intent(in)                    :: mv

!...     locals
      logical :: ok
      integer :: i, j, js, je, k, ilay, irow, icol
      real :: s
!.......................................................................

!...     get pointers
      call sgwf2bcf7pnt(igrid)
      call sgwf2dxc1pnt(igrid)

      ok = .true.
      js = 1
      do i = 1, nid
         ilay = dxcil(i)
         if(ilay.eq.0) cycle ! PKS
         irow = dxcir(i)
         icol = dxcic(i)
         je = xchOff(i)
         if (je-js.gt.0) then
            !write(*,*) 'Warning: cell received > 1 sf1'
         end if   
         s = mv
         do j = js, je
            k = xchIdx(j)
            if (k.le.0) then
               ok = .false.
               mf2005_GetStorageFactor = ok
               return
            end if
            if (strfct(k).ne.mv) then
               if (s.eq.mv) s = 0.0
               s = s + strfct(k)
            end if
         end do
         if (s.ne.mv) then
            ! set storage factor
            sc1(icol,irow,ilay) = s
         endif
         js = je + 1
      end do

!...     pointer save
      call sgwf2bcf7psv(igrid)

      mf2005_GetStorageFactor = ok
      end function

      logical function mf2005_PutSeepageFlux(igrid,xchSeepage,
     1                                   xchCells,nxch,mv,mflag)

      use global, only: delr, delc
      use gwfbcfmodule, only: seepage, seepagemv

      implicit none

!...     arguments
      integer, intent(in)                    :: igrid
      integer, intent(in)                    :: nxch
      integer, dimension(3,nxch), intent(in) :: xchCells
      real, dimension(nxch), intent(out)     :: xchSeepage
      real, intent(in)                       :: mv
      logical, intent(in)                    :: mflag

!...     locals
      logical :: ok
      integer :: i, irow, icol, ilay
      real :: s, area
!.......................................................................

!...     get pointers
      call sgwf2bas7pnt(igrid)
      call sgwf2bcf7pnt(igrid)

      do i = 1, nxch
         ilay = xchCells(1,i)
         irow = xchCells(2,i)
         icol = xchCells(3,i)
         ! only add for the first layer
         if (ilay.eq.1) then
            s = seepage(icol,irow)
            if (s.eq.seepagemv) then
               s = mv
            else
               area = delr(icol)*delc(irow)
               if (.not.mflag) area = 1.0
               s = s/area ! m3 --> m if mflag = .true.
            end if
         else
            s = mv
         end if
         xchSeepage(i) = s
      end do


      ok = .true.
      mf2005_PutSeepageFlux = ok
      end function

      logical function mf2005_PutSeepageSalt(igrid,xchSalt,
     1                                   xchCells,nxch,mv,mflag)

      use gwfbcfmodule, only: seepage, seepagemv
      use gwfdxcmodule, only: seepageconc

      implicit none

!...     arguments
      integer, intent(in)                    :: igrid
      integer, intent(in)                    :: nxch
      integer, dimension(3,nxch), intent(in) :: xchCells
      real, dimension(nxch), intent(out)     :: xchSalt
      real, intent(in)                       :: mv
      logical, intent(in)                    :: mflag

!...     locals
      logical :: ok
      integer :: i, irow, icol, ilay
      real :: seep, conc, flux, area
!.......................................................................

!...     get pointers
      call sgwf2bas7pnt(igrid)
      call sgwf2bcf7pnt(igrid)
      call sgwf2dxc1pnt(igrid)

      do i = 1, nxch
         ilay = xchCells(1,i)
         irow = xchCells(2,i)
         icol = xchCells(3,i)
         ! only add for the first layer
         if (ilay.eq.1) then
            seep = seepage(icol,irow)
            conc = seepageconc(icol,irow)
            flux = mv
            if (seep.ne.seepagemv .and. conc.gt.0.) then
               if (seep.lt.0.) flux = -seep*conc
            end if
         else
            flux = mv
         end if
         xchSalt(i) = flux
      end do

      ok = .true.
      mf2005_PutSeepageSalt = ok
      end function

      logical function mf2005_PutRiverFlux(igrid,xchRivFlux,
     1                   xchCells,nxch,mv,
     1                   nhrivsys,hrivsys,nwrivsys,wrivsys,
     1                   mflag,wells)

      use global, only: buff, delr, delc
      use gwfrivmodule, only: rivr, nriver, nrivvl,
     1                        irivsubsys, irivrconc

      implicit none

!...     arguments
      integer, intent(in)                         :: igrid
      integer, intent(in)                         :: nxch
      integer, dimension(3,nxch), intent(in)      :: xchCells
      real, dimension(nxch), intent(out)          :: xchRivFlux
      real, intent(in)                            :: mv
      integer, intent(in)                         :: nhrivsys
      integer, dimension(nhrivsys), intent(in) :: hrivsys
      integer, intent(in)                         :: nwrivsys
      integer, dimension(nwrivsys), intent(in) :: wrivsys
      logical, intent(in)                         :: mflag
      logical, intent(in)                         :: wells

!...     locals
      logical :: ok, add
      integer :: i, j, n, irow, icol, ilay, isys
      real :: flux, conc, area
      double precision :: mask                                          ! PKS
!.......................................................................

!...     get pointers
      call sgwf2bas7pnt(igrid)
      call sgwf2riv7pnt(igrid)

!...     get number of rivers
      if (associated(nriver)) then
         n = nriver
      else
         n = 0
      end if

!...     use the working buffer for temporary storage and initialize
      buff = mv
      do i = 1, n
         add = .true.
         ilay = rivr(1,i)
         irow = rivr(2,i)
         icol = rivr(3,i)
         flux = rivr(nrivvl,i)
         call pks7mpimask( mask, icol, irow, ilay )                     ! PKS
         if (mask.lt.0.5d0) flux = 0.                                   ! PKS
         conc = rivr(irivrconc,i)
         isys = rivr(irivsubsys,i)
         ! skip subsystem H
         do j = 1, nhrivsys
             if (isys.eq.hrivsys(j)) add = .false.
         end do
         if (.not.wells) then
            do j = 1, nwrivsys
               if (isys.eq.wrivsys(j)) add = .false.
            end do
         else
            do j = 1, nwrivsys
               if (isys.ne.wrivsys(j)) add = .false.
            end do
            if (add .and. conc.lt.0.0) add = .false.
         end if
         if (add) then
            if (buff(icol,irow,1).eq.mv) buff(icol,irow,1) = 0.0
            buff(icol,irow,1) = buff(icol,irow,1) + flux
         end if
      end do

!...     Copy to exchange array
      do i = 1, nxch
         ilay = xchCells(1,i)
         irow = xchCells(2,i)
         icol = xchCells(3,i)
         flux = buff(icol,irow,1)
         if (flux.ne.mv) then
            area = delr(icol)*delc(irow)
            if (.not.mflag) area = 1.0
            flux = flux/area ! m3 --> m if mflag = .true.
         end if
         xchRivFlux(i) = flux
      end do

      ok = .true.
      mf2005_PutRiverFlux = ok
      end function

      logical function mf2005_PutRiverFluxSubsys(igrid,xchRivFlux,
     1                   xchCells,nxch,mv,
     1                   mflag,isubsys)

      use global, only: buff, delr, delc
      use gwfrivmodule, only: rivr, nriver, nrivvl,
     1                        irivsubsys

      implicit none

!...     arguments
      integer, intent(in)                         :: igrid
      integer, intent(in)                         :: nxch
      integer, dimension(3,nxch), intent(in)      :: xchCells
      real, dimension(nxch), intent(out)          :: xchRivFlux
      real, intent(in)                            :: mv
      logical, intent(in)                         :: mflag
      integer, intent(in)                         :: isubsys

!...     locals
      logical :: ok, add
      integer :: i, j, n, irow, icol, ilay, isys
      real :: flux, area
!.......................................................................

      ok = .true.
      if (isubsys.le.0) then
          ok = .false.
          mf2005_PutRiverFluxSubsys = ok
          return
      end if
      
!...     get pointers
      call sgwf2bas7pnt(igrid)
      call sgwf2riv7pnt(igrid)

!...     get number of rivers
      if (associated(nriver)) then
         n = nriver
      else
         n = 0
      end if

!...     use the working buffer for temporary storage and initialize
      buff = mv
      do i = 1, n
         add = .false.
         ilay = rivr(1,i)
         irow = rivr(2,i)
         icol = rivr(3,i)
         flux = rivr(nrivvl,i)
         isys = int(rivr(irivsubsys,i))
         if (isys.eq.isubsys) add = .true.
         if (add) then
            if (buff(icol,irow,1).eq.mv) buff(icol,irow,1) = 0.0
            buff(icol,irow,1) = buff(icol,irow,1) + flux
         end if
      end do

!...     Copy to exchange array
      do i = 1, nxch
         ilay = xchCells(1,i)
         irow = xchCells(2,i)
         icol = xchCells(3,i)
         flux = buff(icol,irow,1)
         if (flux.ne.mv) then
            area = delr(icol)*delc(irow)
            if (.not.mflag) area = 1.0
            flux = flux/area ! m3 --> m if mflag = .true.
         end if
         xchRivFlux(i) = flux
      end do

      mf2005_PutRiverFluxSubsys = ok
      end function
      
      logical function mf2005_PutRiverStageSubsys(igrid,xchRivStage,
     1                   xchCells,nxch,mv,isubsys)

      use global, only: buff, delr, delc
      use gwfrivmodule, only: rivr, nriver, nrivvl,
     1                        irivsubsys

      implicit none

!...     arguments
      integer, intent(in)                         :: igrid
      integer, intent(in)                         :: nxch
      integer, dimension(3,nxch), intent(in)      :: xchCells
      real, dimension(nxch), intent(out)          :: xchRivStage
      real, intent(in)                            :: mv
      integer, intent(in)                         :: isubsys

!...     locals
      logical :: ok, add
      integer :: i, j, n, irow, icol, ilay, isys
      real :: stage
!.......................................................................
      ok = .true.
      if (isubsys.le.0) then
          ok = .false.
          mf2005_PutRiverStageSubsys = ok
          return
      end if    
      
!...     get pointers
      call sgwf2bas7pnt(igrid)
      call sgwf2riv7pnt(igrid)

!...     get number of rivers
      if (associated(nriver)) then
         n = nriver
      else
         n = 0
      end if

!...     use the working buffer for temporary storage and initialize
      buff = mv
      do i = 1, n
         add = .false.
         ilay = rivr(1,i)
         irow = rivr(2,i)
         icol = rivr(3,i)
         stage = rivr(4,i)
         isys = int(rivr(irivsubsys,i))
         if (isys.eq.isubsys) add = .true.
         if (add) then
            if (buff(icol,irow,1).eq.mv) then
               buff(icol,irow,1) = stage
            else
               ok = .false.
            end if    
         end if
      end do

!...     Copy to exchange array
      do i = 1, nxch
         ilay = xchCells(1,i)
         irow = xchCells(2,i)
         icol = xchCells(3,i)
         stage = buff(icol,irow,1)
         xchRivStage(i) = stage
      end do

      mf2005_PutRiverStageSubsys = ok
      end function      
      
      logical function mf2005_PutSaltFlux(igrid,xchRivFlux,
     1                       xchCells,nxch,mv,nwrivsys,wrivsys)

      use global, only: buff, delr, delc
      use gwfrivmodule, only: rivr, nriver, nrivvl,
     1   irivsubsys, irivrconc

      implicit none

!...     arguments
      integer, intent(in)                      :: igrid
      integer, intent(in)                      :: nxch
      integer, dimension(3,nxch), intent(in)   :: xchCells
      real, dimension(nxch), intent(out)       :: xchRivFlux
      real, intent(in)                         :: mv
      integer, intent(in)                      :: nwrivsys
      integer, dimension(nwrivsys), intent(in) :: wrivsys

!...     locals
      logical :: ok, add
      integer :: i, j, n, irow, icol, ilay
      real :: flux, conc, saltflux, area
      double precision :: mask                                          ! PKS
!.......................................................................

!...     get pointers
      call sgwf2bas7pnt(igrid)
      call sgwf2riv7pnt(igrid)

!...     get number of rivers
      if (associated(nriver)) then
         n = nriver
      else
         n = 0
      end if

!...     use the working buffer for temporary storage and initialize
      buff = mv
      do i = 1, n
         add = .false.
         if (irivsubsys.gt.0) then
            do j = 1, nwrivsys
               if (rivr(irivsubsys,i).eq.wrivsys(j)) add = .true.
            end do
         end if
         ilay = rivr(1,i)
         irow = rivr(2,i)
         icol = rivr(3,i)
         flux = rivr(nrivvl,i)
         call pks7mpimask( mask, icol, irow, ilay )                     ! PKS
         if (mask.lt.0.5d0) flux = 0.                                   ! PKS
         conc = rivr(irivrconc,i)
         saltflux = mv
         if (conc.gt.0.0) then ! work-around, to be changed for proper missing value
            if (flux.lt.0.0) then
               saltflux = flux*conc
            else
               saltflux = 0.0
            end if
         end if
         if (saltflux.eq.mv) add = .false.
         if (add) then
            if (buff(icol,irow,1).eq.mv) buff(icol,irow,1) = 0.0
            buff(icol,irow,1) = buff(icol,irow,1) + saltflux
         end if
      end do

!...     Copy to exchange array
      do i = 1, nxch
         ilay = xchCells(1,i)
         irow = xchCells(2,i)
         icol = xchCells(3,i)
         flux = buff(icol,irow,1)
         xchRivFlux(i) = flux
      end do

      ok = .true.
      mf2005_PutSaltFlux = ok
      end function

      logical function mf2005_PutSaltFluxSeepage(igrid,xchFlux,
     1                       xchCells,nxch,mv)

      use gwfbcfmodule, only: seepage, seepagemv
      use gwfdxcmodule, only: seepageconc

      implicit none

!...     arguments
      integer, intent(in)                      :: igrid
      integer, intent(in)                      :: nxch
      integer, dimension(3,nxch), intent(in)   :: xchCells
      real, dimension(nxch), intent(out)       :: xchFlux
      real, intent(in)                         :: mv

!...     locals
      logical :: ok, add
      integer :: i, j, n, irow, icol, ilay
      real :: flux, seep, conc
!.......................................................................

!...     get pointers
      call sgwf2bcf7pnt(igrid)
      call sgwf2dxc1pnt(igrid)

!...     determine seepage flux
      do i = 1, nxch
         ilay = xchCells(1,i)
         irow = xchCells(2,i)
         icol = xchCells(3,i)
         flux = mv
         seep = seepage(icol,irow)
         conc = seepageconc(icol,irow)
         if (seep.ne.seepagemv .and. conc > 0.) then
            flux = 0.
            if (seep < 0.) then
                flux = -seep*conc ! [m3]*[mg/l] = [m3]*[g/m3] = [g]
            end if
         end if
         xchFlux(i) = flux
      end do

      ok = .true.
      mf2005_PutSaltFluxSeepage = ok
      end function


      logical function mf2005_PutDrainFlux(igrid,xchDrnFlux,
     1                        xchCells,nxch,mv,mflag)

      use global, only: buff, delr, delc
      use gwfdrnmodule, only: drai, ndrain, ndrnvl, idrnsubsys

      implicit none

!...     arguments
      integer, intent(in)                    :: igrid
      integer, intent(in)                    :: nxch
      integer, dimension(3,nxch), intent(in) :: xchCells
      real, dimension(nxch), intent(out)     :: xchDrnFlux
      real, intent(in)                       :: mv
      logical, intent(in)                    :: mflag

!...     locals
      logical :: ok, add
      integer :: i, n, irow, icol, ilay
      real :: flux, area
      double precision :: mask                                          ! PKS
!.......................................................................
       
!...     get pointers
      call sgwf2bas7pnt(igrid)
      call sgwf2drn7pnt(igrid)

!...     get number of rivers
      if (associated(ndrain)) then
         n = ndrain
      else
         n = 0
      end if

!...     use the working buffer for temporary storage and initialize
      buff = mv
      do i = 1, n
         add = .true.
         ilay = drai(1,i)
         irow = drai(2,i)
         icol = drai(3,i)
         flux = drai(ndrnvl,i)
         call pks7mpimask( mask, icol, irow, ilay )                     ! PKS
         if (mask.lt.0.5d0) flux = 0.                                   ! PKS
         if (add) then
            if (buff(icol,irow,1).eq.mv) buff(icol,irow,1) = 0.0
            buff(icol,irow,1) = buff(icol,irow,1) + flux
         end if
      end do

!...     Copy to exchange array
      do i = 1, nxch
         ilay = xchCells(1,i)
         irow = xchCells(2,i)
         icol = xchCells(3,i)
         flux = buff(icol,irow,1)
         if (flux.ne.mv) then
            area = delr(icol)*delc(irow)
            if (.not.mflag) area = 1.0
            flux = flux/area ! m3 --> m if mflag = .true.
         end if
         xchDrnFlux(i) = flux
      end do

      ok = .true.
      mf2005_PutDrainFlux = ok
      end function

      logical function mf2005_PutDrainFluxSubsys(igrid,xchDrnFlux,
     1                        xchCells,nxch,mv,mflag,isubsys)

      use global, only: buff, delr, delc
      use gwfdrnmodule, only: drai, ndrain, ndrnvl, idrnsubsys

      implicit none

!...     arguments
      integer, intent(in)                    :: igrid
      integer, intent(in)                    :: nxch
      integer, dimension(3,nxch), intent(in) :: xchCells
      real, dimension(nxch), intent(out)     :: xchDrnFlux
      real, intent(in)                       :: mv
      logical, intent(in)                    :: mflag
      integer, intent(in)                    :: isubsys

!...     locals
      logical :: ok, add
      integer :: i, n, irow, icol, ilay, isys
      real :: flux, area
!.......................................................................

      ok = .true.
      if (isubsys.le.0) then
          ok = .false.
          mf2005_PutDrainFluxSubsys = ok
          return
      end if         
      
!...     get pointers
      call sgwf2bas7pnt(igrid)
      call sgwf2drn7pnt(igrid)

!...     get number of rivers
      if (associated(ndrain)) then
         n = ndrain
      else
         n = 0
      end if

!...     use the working buffer for temporary storage and initialize
      buff = mv
      do i = 1, n
         add = .false.
         ilay = drai(1,i)
         irow = drai(2,i)
         icol = drai(3,i)
         flux = drai(ndrnvl,i)
         isys = int(drai(idrnsubsys,i)) 
         if (isys.eq.isubsys) add = .true.
         if (add) then
            if (buff(icol,irow,1).eq.mv) buff(icol,irow,1) = 0.0
            buff(icol,irow,1) = buff(icol,irow,1) + flux
         end if
      end do

!...     Copy to exchange array
      do i = 1, nxch
         ilay = xchCells(1,i)
         irow = xchCells(2,i)
         icol = xchCells(3,i)
         flux = buff(icol,irow,1)
         if (flux.ne.mv) then
            area = delr(icol)*delc(irow)
            if (.not.mflag) area = 1.0
            flux = flux/area ! m3 --> m if mflag = .true.
         end if
         xchDrnFlux(i) = flux
      end do

      mf2005_PutDrainFluxSubsys = ok
      end function      
        
      !> Put the river subsystems to skip.
      logical function mf2005_PutModMozRiversToSkip(igrid,nhriv,
     1                                              hriv)
!...     modules
      use gwfdxcmodule, only: maxnhrivsys, nhrivsys, hrivsys

      implicit none

!...     arguments
      integer, intent(in) :: igrid
      integer, intent(in) :: nhriv
      integer, dimension(nhriv), intent(in) :: hriv

!...     locals
      logical :: ok
!.......................................................................

!...     get pointers
      call sgwf2dxc1pnt(igrid)

      ok = .true.
      if (nhriv.gt.maxnhrivsys) then
         ok = .false.
         mf2005_PutModMozRiversToSkip = ok
         return
      end if

      nhrivsys = nhriv
      hrivsys(1:nhrivsys) = hriv(1:nhrivsys)

!...     save pointers
      call sgwf2dxc1psv(igrid)

!...     return
      mf2005_PutModMozRiversToSkip = ok

      end function

      !> Return number of exchange items (LSW IDs).
      logical function mf2005_PutModMozNumberOfIDs(igrid, nxch)
!...     modules
      use gwfdxcmodule, only: ndxclsw

      implicit none

!...     arguments
      integer, intent(in) :: igrid
      integer, intent(out) :: nxch

!...     locals
      logical :: ok
!.......................................................................

!...     get pointers
      call sgwf2dxc1pnt(igrid)

!...     check
      ok = .true.
      if (.not.associated(ndxclsw)) then
         ok = .false.
         mf2005_PutModMozNumberOfIDs = ok
         return
      end if
      nxch = ndxclsw

      if (nxch.le.0) ok = .false.
      mf2005_PutModMozNumberOfIDs = ok

      end function

      !> Return number of exchange items (PV IDs).
      logical function mf2005_PutModMozPVNumberOfIDs(igrid, nxch)
!...     modules
      use gwfdxcmodule, only: ndxcpv

      implicit none

!...     arguments
      integer, intent(in) :: igrid
      integer, intent(out) :: nxch

!...     locals
      logical :: ok
!.......................................................................

!...     get pointers
      call sgwf2dxc1pnt(igrid)

!...     check
      ok = .true.
      if (.not.associated(ndxcpv)) then
         ok = .false.
         mf2005_PutModMozPVNumberOfIDs = ok
         return
      end if
      nxch = ndxcpv

c     if (nxch.le.0) ok = .false.
      mf2005_PutModMozPVNumberOfIDs = ok

      end function

      !> Return MODFLOW exchange IDs.
      logical function mf2005_PutModMozIDs(igrid,ids)

!...     modules
      use gwfdxcmodule, only: dxcidlsw, ndxclsw

      implicit none

!...     arguments
      integer, intent(in) :: igrid
      integer, dimension(*), intent(out) :: ids

!...     locals
      logical :: ok
      integer :: i
!.......................................................................

!...     set pointers
      call sgwf2dxc1pnt(igrid)

!...     check
      ok = .true.
      if (.not.associated(dxcidlsw)) then
         ok = .false.
         mf2005_PutModMozIDs = ok
         return
      end if

!...     Copy the IDs
      do i = 1, ndxclsw
         ids(i) = dxcidlsw(i)
      end do

      mf2005_PutModMozIDs = ok

      end function

      !> Return MODFLOW exchange IDs.
      logical function mf2005_PutModMozPVIDs(igrid,ids)

!...     modules
      use gwfdxcmodule, only: dxcidpv, ndxcpv

      implicit none

!...     arguments
      integer, intent(in) :: igrid
      integer, dimension(*), intent(out) :: ids

!...     locals
      logical :: ok
      integer :: i
!.......................................................................

!...     set pointers
      call sgwf2dxc1pnt(igrid)

!...     check
      ok = .true.
      if (.not.associated(dxcidpv)) then
         ok = .false.
         mf2005_PutModMozPVIDs = ok
         return
      end if

!...     Copy the IDs
      do i = 1, ndxcpv
         ids(i) = dxcidpv(i)
      end do

      mf2005_PutModMozPVIDs = ok

      end function

      logical function mf2005_GetLSWLevels(igrid,
     1                           levels,nid,xchIdx,xchOff,mv)
!...     modules
      use gwfdxcmodule, only: dxclevlsw, dxcmv

      implicit none

!...     arguments
      integer, intent(in)                    :: igrid
      integer, intent(in)                    :: nid
      real, dimension(*), intent(inout)      :: levels
      integer, dimension(*), intent(in)      :: xchIdx
      integer, dimension(nid), intent(in)    :: xchOff
      real, intent(in)                       :: mv

!...     locals
      integer :: i, j, js, je, k, ilay, irow, icol
      real :: lev
!.......................................................................

      ! get pointers
      call sgwf2dxc1pnt(igrid)

      dxclevlsw = dxcmv
      js = 1
      do i = 1, nid
         je = xchOff(i)
         if (je-js.gt.0) write(*,*) 'Warning: cell received > 1 uszflux'
         do j = js, je
            k = xchIdx(j)
            if (k.le.0) then
               write(*,*) 'Error: coupling node index out of range'
               stop 1
            end if
            lev = levels(k)
            if (lev.eq.mv) lev = dxcmv
            dxclevlsw(i) = lev
         end do
         js = je + 1
      end do

      ! save pointers
      call sgwf2dxc1psv(igrid)

      mf2005_GetLSWLevels = .true.
      end function


      logical function mf2005_GetPVLevels(igrid,
     1                           levels,nid,xchIdx,xchOff,mv)
!...     modules
      use gwfdxcmodule, only: dxclevpv, dxcmv

      implicit none

!...     arguments
      integer, intent(in)                    :: igrid
      integer, intent(in)                    :: nid
      real, dimension(*), intent(inout)      :: levels
      integer, dimension(*), intent(in)      :: xchIdx
      integer, dimension(nid), intent(in)    :: xchOff
      real, intent(in)                       :: mv

!...     locals
      integer :: i, j, js, je, k, ilay, irow, icol
      real :: lev
!.......................................................................

      ! set pointers
      call sgwf2dxc1pnt(igrid)

      dxclevpv = dxcmv
      js = 1
      do i = 1, nid
         je = xchOff(i)
         if (je-js.gt.0) then
            write(*,*) 'Warning: cell received > 1 levels'
         end if
         do j = js, je
            k = xchIdx(j)
            if (k.le.0) then
               write(*,*) 'Error: coupling node index out of range'
               stop 1
            end if
            lev = levels(k)
            if (lev.eq.mv) lev = dxcmv
            dxclevpv(i) = lev
         end do
         js = je + 1
      end do

      ! save pointers
      call sgwf2dxc1psv(igrid)

      mf2005_GetPVLevels = .true.
      end function

      logical function mf2005_TimeserieInit(igrid)
!...     modules
      use imod_utl, only: imod_utl_pol1located
      use global, only: iunit, nlay, nrow, ncol, ibound
      use gwfmetmodule, only: coord_xll_nb,coord_yll_nb,coord_xur_nb,
     1                        coord_yur_nb,cdelr,cdelc,resultdir
      use m_mf2005_iu
      use tsvar

      implicit none

!...     arguments
      integer, intent(in) :: igrid

!...     locals
      logical :: ok, valid
      integer :: jj, i, ilay, irow, icol
      real(kind=8) :: x, y, w
!.......................................................................

      ok = .true.
      if (iipf.eq.0) then
         mf2005_TimeserieInit = ok
         return
      end if

      ! set pointers
      call sgwf2bas7pnt(igrid)
      call sgwf2met1pnt(igrid)

      if (IUNIT(IUMET).le.0) then
         ok = .false.
         mf2005_TimeserieInit = ok
         return
      end if

      if (.not.associated(coord_xll_nb).or.
     1    .not.associated(coord_yll_nb).or.
     1    .not.associated(coord_yur_nb).or.
     1    .not.associated(coord_xur_nb).or.
     1    .not.associated(cdelr).or.
     1    .not.associated(cdelc)) then
         ok = .false.
         mf2005_TimeserieInit = ok
         return
      end if

      do jj = 1, abs(iipf)
        do i = 1, ts(jj)%nrowipf
           x = ts(jj)%stvalue(i)%x
           y = ts(jj)%stvalue(i)%y
           w = ts(jj)%stvalue(i)%w
           ilay = ts(jj)%stvalue(i)%ilay
           valid = .true.
!           if (x.le.coord_xll_nb .or. x.ge.coord_xur_nb) valid = .false.
!           if (y.le.coord_yll_nb .or. y.ge.coord_yur_nb) valid = .false.
           if (ilay.lt.1 .or. ilay.gt.nlay) valid = .false.
           if(w.eq.0.0)valid=.false.
           ts(jj)%stvalue(i)%valid = valid
           if (valid) then ! store indices
              icol = ts(jj)%stvalue(i)%icol
              irow = ts(jj)%stvalue(i)%irow
              if (icol.eq.0 .or. irow.eq.0) then
                 call imod_utl_pol1located(cdelr,ncol+1,x,icol)
                 call imod_utl_pol1located(cdelc,nrow+1,y,irow)
                 !## check if constant head or inactive cell - make measurment invalid
                 if(ibound(icol,irow,ilay).le.0)then
                  ts(jj)%stvalue(i)%valid = .false.
                 else
                 ts(jj)%stvalue(i)%icol = icol
                 ts(jj)%stvalue(i)%irow = irow
                 endif
              end if
           end if
        end do
      end do

      mf2005_TimeserieInit = ok
      end function

      subroutine mf2005_GetSavePath_TS(savepath)
!...     modules
      use gwfmetmodule
!...     arguments
      character(len=*),intent(out) :: savepath

      call sgwf2met1pnt(1) !igrid)
      if (associated(resultdir))savepath=resultdir
      
      end subroutine mf2005_GetSavePath_TS

      logical function mf2005_TimeserieGetHead(igrid)
!...     modules
      use global, only: hnew, iunit, nlay, nrow, ncol
      use gwfmetmodule
      use tsvar

      implicit none

!...     arguments
      integer, intent(in) :: igrid
      
!...     functions
      real :: mf2005_tserie1hmean

!...     locals
      logical :: ok
      integer :: jj, i, ilay, irow, icol
      real(kind=8) :: x, y
      real :: h
!.......................................................................

      ok = .true.

      ! set pointers
      call sgwf2bas7pnt(igrid)

      if (iipf.eq.0) then
         mf2005_TimeserieGetHead = ok
         return
      end if

      do jj = 1, abs(iipf)
        do i = 1, ts(jj)%nrowipf
           if (.not.ts(jj)%stvalue(i)%valid) cycle ! skip invalid data
           icol = ts(jj)%stvalue(i)%icol
           irow = ts(jj)%stvalue(i)%irow
           if (icol.eq.0 .or. irow.eq.0) then
              ok = .false.
              mf2005_TimeserieGetHead = ok
              return
           end if
           ilay = ts(jj)%stvalue(i)%ilay
           if(iipf.gt.0)then
              h=real(hnew(icol,irow,ilay))
           else
              x = ts(jj)%stvalue(i)%x
              y = ts(jj)%stvalue(i)%y
              !## interpolate heads
              h=mf2005_tserie1hmean(x,y,icol,irow,ilay)
           endif
           ts(jj)%stvalue(i)%c = h ! set computed value
        end do
      end do
      
      mf2005_TimeserieGetHead = ok
      end function

      !> Enable PEST for MODFLOW.
      logical function mf2005_GetPestFlag(flag)

!...     modules
      use global, only: lipest

      implicit none

!...    arguments
      logical, intent(in) :: flag
!.......................................................................

      lipest = flag
      mf2005_GetPestFlag = .true.

      end function
      
      !> Set the run-file root for DXC package
      logical function mf2005_GetDxcRoot(rfroot)

!...     modules
      use gwfdxcmodule, only: dxcroot

      implicit none

!...    arguments
      character(len=*), intent(in) :: rfroot

!...     locals
      logical :: ok
!.......................................................................

      ok = .true.
      dxcroot = trim(rfroot)
      mf2005_GetDxcRoot = ok

      end function
 
      logical function mf2005_PutHeadsForLayer(head,ncol,nrow,
     1                                         ilay,igrid)
!...     modules
      use global, only: hnew

      implicit none

!...     arguments
      integer, intent(in) :: ncol,nrow,ilay,igrid
      double precision, dimension(ncol,nrow), intent(out) :: head

!...     locals

      ! get pointers
      call sgwf2bas7pnt(igrid)

      head = hnew(:,:,ilay)

      mf2005_PutHeadsForLayer = .true.
      end function mf2005_PutHeadsForLayer
            
      logical function mf2005_GetRecharge(recharge,ncol,nrow,igrid)
!...     modules
      use global, only: iunit,delr,delc
      use m_mf2005_iu, only: iurch
      use gwfrchmodule, only: rechbuff

!...     arguments
      integer, intent(in) :: ncol,nrow,igrid
      real, dimension(ncol,nrow), intent(in) :: recharge
      
!...     locals
!.......................................................................
 
      ! check if package is active
      if (iunit(iurch).le.0) then
          mf2005_GetRecharge = .false.
          return
      end if    
      
      ! get pointers
      call sgwf2rch7pnt(igrid)
      
      ! allocate for the first time
      if (.not.associated(rechbuff)) then
          allocate(rechbuff(ncol,nrow))
      end if
      
      ! set and multipy with cell area
      do irow=1,nrow
         do icol=1,ncol
            rechbuff(icol,irow)=
     1         recharge(icol,irow)*delr(icol)*delc(irow)
         end do    
      end do       
      
      ! set pointers 
      call sgwf2rch7psv(igrid)
            
      mf2005_GetRecharge = .true.
      end function mf2005_GetRecharge

