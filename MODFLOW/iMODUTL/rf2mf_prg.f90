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

MODULE MOD_RF2MF_MAIN

USE IMOD_IDF, ONLY: IDFREAD,IDFOBJ
USE rf2mf_module
USE RF2MF_UTL
USE MOD_METASWAP
USE MOD_RF2MF_READ
USE MOD_RF2MF
USE IMOD_UTL
USE TSVAR
USE MOD_PEST, ONLY: PEST1INIT
use pestvar, ONLY : IUPESTOUT,IUPESTPROGRESS,IUPESTEFFICIENCY,IUPESTSENSITIVITY,IUPESTRUNFILE

IMPLICIT NONE

PRIVATE

logical, save :: savebuf = .false.
logical, save :: savenobuf = .false.

PUBLIC :: RF2MF

CONTAINS

!#####=================================================================
SUBROUTINE RF2MF(RUNFNAME,DXCFNAME,submstr,nsub,nsubmax,lipest,lidfmerge,rfroot,I1,I2)
!#####=================================================================
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN) :: RUNFNAME
CHARACTER(LEN=*),INTENT(INOUT) :: DXCFNAME
integer, intent(in) :: nsubmax
integer, intent(inout) :: nsub
logical, intent(inout) :: lipest
logical, intent(inout) :: lidfmerge
character(len=*),intent(out) :: rfroot

character(len=50), dimension(nsubmax), intent(out) :: submstr

INTEGER,OPTIONAL,INTENT(IN) :: I1,I2

CHARACTER(LEN=10) :: CHR
CHARACTER(LEN=256) :: FNAME

FNAME=RUNFNAME

!## operating system
CALL IMOD_UTL_OSSYSTEM()

!!## store current directory
!CALL OSD_GETCWD(ROOTMAIN)

CALL IMOD_UTL_PRINTTEXT('====================================================================',3)
CALL IMOD_UTL_PRINTTEXT('',3)
CALL IMOD_UTL_PRINTTEXT('        '//'iMODFLOW 2005 '//TRIM(RVERSION)//' ('//TRIM(OSN(OS))//')',3)
CALL IMOD_UTL_PRINTTEXT('',3)
CALL IMOD_UTL_PRINTTEXT('====================================================================',3)
CALL IMOD_UTL_PRINTTEXT('',3)
CALL IMOD_UTL_PRINTTEXT('Syntax: imodflow.exe arg1 arg2 arg3',3)
CALL IMOD_UTL_PRINTTEXT('',3)
CALL IMOD_UTL_PRINTTEXT('  arg1: runfile',3)
CALL IMOD_UTL_PRINTTEXT('  arg2: (optional) pause (0) or continue (1) after errors',3)
CALL IMOD_UTL_PRINTTEXT('  arg3: (optional) surpress IACT and simulate submodel NUMBER only',3)
CALL IMOD_UTL_PRINTTEXT('',3)
CALL IMOD_UTL_PRINTTEXT('  e.g.: imodflow.exe test.run',3)
CALL IMOD_UTL_PRINTTEXT('  e.g.: imodflow.exe test.run 1',3)
CALL IMOD_UTL_PRINTTEXT('  e.g.: imodflow.exe test.run 0 23',3)
CALL IMOD_UTL_PRINTTEXT('',3)
CALL IMOD_UTL_PRINTTEXT('  Note: testmodels will be created after supplying a non-existing ',3)
CALL IMOD_UTL_PRINTTEXT('        runfile called IMODFLOW.RUN',3)
CALL IMOD_UTL_PRINTTEXT('',3)
CALL IMOD_UTL_PRINTTEXT('====================================================================',3)
CALL IMOD_UTL_PRINTTEXT('',3)

CALL IMOD_UTL_STRING(FNAME)
CALL IMOD_UTL_FILENAME(FNAME)

IF(LEN_TRIM(FNAME).EQ.0)CALL IMOD_UTL_PRINTTEXT('Start code with *.exe *.run flags',-3)

IFLAG=0
IF(PRESENT(I1))THEN
 IF (I1.EQ.0) THEN
  CALL IMOD_UTL_PRINTTEXT('arg2 option pause (0) is not supported!',-3)
 END IF
 IFLAG(1)=I1 !## get second argument - pause
END IF
IF(PRESENT(I2))IFLAG(2)=I2 !## get third argument - submodel

INQUIRE(FILE=FNAME,EXIST=LEX)
IF(LEX)THEN
 CALL RF2MF_MODFLOW(FNAME,DXCFNAME,submstr,nsub,nsubmax,lipest,lidfmerge,rfroot)
ELSE
 CALL IMOD_UTL_PRINTTEXT('RUN-FILE '//TRIM(FNAME)//' does not exists!',-3)
ENDIF

END SUBROUTINE RF2MF

!#####=================================================================
SUBROUTINE RF2MF_MODFLOW(CL,DXCFILE,submstr,nsub,nsubmax,lipest,lidfmerge,rfroot)
!#####=================================================================
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(INOUT) :: CL
CHARACTER(LEN=*),INTENT(INOUT) :: DXCFILE
integer, intent(in) :: nsubmax
integer, intent(inout) :: nsub
character(len=50), dimension(nsubmax), intent(out) :: submstr
logical, intent(inout) :: lipest
logical, intent(inout) :: lidfmerge
character(len=*),intent(out) :: rfroot
type(idfobj) :: idf
LOGICAL :: LSTOP
integer :: i, iper, nsys, isys, ilay, ios, myrank
character(len=1) :: slash
character(len=1024) :: tmpdir

!#close all units
CALL IMOD_UTL_CLOSEUNITS()

!!#change directory to start position
!  CALL OSD_CHDIR(ROOTMAIN)

call imod_utl_getslash(slash)
i = index(cl,slash,.true.)
root%modelname=CL(i+1:INDEX(CL,'.',.TRUE.)-1)

!## get root for run-file
call osd_getcwd(root%runfileroot)
if (i.gt.0) then
   call osd_getcwd(tmpdir)
   call osd_chdir(cl(1:i))
   call osd_getcwd(root%runfileroot)
   call osd_chdir(tmpdir)
end if
rfroot = trim(root%runfileroot)

IMULT=0
nsub=0
!## loop over mult-boxes
MULTLOOP: DO

!## open runfile
IURUN=IMOD_UTL_GETUNIT()
OPEN(IURUN,FILE=CL,STATUS='OLD',ACTION='READ',SHARE='DENYNONE',IOSTAT=IOS)
IF(IOS.NE.0)THEN
 WRITE(*,'(A)') 'Can not find runfile ['//TRIM(CL)//']'
 PAUSE; STOP
ENDIF

!## read result-root
CALL RF2MF_DATASET1()
!## read main settings
CALL RF2MF_DATASET2()
!## allocate memory
CALL RF2MF_INIT_ALLOCATE()
!#read settings (nmult,idebug,imodflow etc.)
CALL RF2MF_DATASET3()
!#read solver settings
CALL RF2MF_DATASET4(lidfmerge)
!### PKS inititalization
call InitPks()
call pks7mpigetmyrank(myrank)
IF(.NOT.IMOD_UTL_DIREXIST(RESULTDIR))THEN
   IF(MYRANK.EQ.0)CALL IMOD_UTL_CREATEDIR(RESULTDIR)
END IF    
!#read box (imult)
CALL RF2MF_DATASET5()
IF(IMULT.GT.NMULT)EXIT
if (nmult.gt.1) then
   nsub = nsub + 1
   submstr(nsub)=root%submodel
end if

!## nothing to do, stop everything!
IF(IACT.NE.0)THEN
 !## read scenario filename
 CALL RF2MF_DATASET6()
 !## read empty string - header ACTIVE MODULES
 READ(IURUN,*)
 !## read active packages and save-settings
 CALL RF2MF_DATASET7(DXCFILE)
 !## check run-file if necessary
 CALL RF2MF_CHECKRUN()
 !## determine size of SIMBOX and adjust ncol/nrow
 CALL RF2MF_EXTENT(idf)
 !### PKS partitioning
 if(pks%active)call PartPks()
 !## read empty string - header MODULES FOR EACH LAYER
 READ(IURUN,*) LINE
 !## solve current model - lstop=.true.: quit ; lstop=.false. whenever effect 'bounds' to boundary!
 CALL RF2MF_MAIN(DXCFILE,lipest,idf)
ELSE
 CLOSE(IURUN)
 CLOSE(IUOUT)
END IF

!...     write package input files
if (myrank.eq.0) then ! master only
 call WriteDis()
 call WriteBas()
 call WriteBcf()
 call WritePwt()
 call WriteScr()
 call WriteMet()
 call WriteRiv()
 call WriteDrn()
 call WriteGhb()
 call WriteWel()
 call WriteAni()
 call WriteHfb()
 call WritePcg()
 call WritePks()
 call WriteRch()
 call WriteEvt()
 call WriteOc()
 call WriteChd()
 call WriteNam(dxcfile)
end if
call pks7mpibarrier() ! PKS

call AllocNam(idealloc)
call AllocDis(idealloc)
call AllocBas(idealloc)
call AllocBcf(idealloc)
call AllocMet(idealloc)
call AllocOc (idealloc)
if (mpck(priv).eq.1 .or. mpck(pisg).eq.1) call AllocRiv(idealloc,mmod(psft))
if (mpck(pdrn).eq.1 .or. mpck(polf).eq.1) call AllocDrn(idealloc,iconchk)
if (mpck(pghb).eq.1) call AllocGhb(idealloc)
if (mpck(pwel).eq.1) call AllocWel(idealloc)
if (mpck(pani).eq.1) call AllocAni(idealloc)
if (mpck(phfb).eq.1) call AllocHfb(idealloc)
if (mpck(prch).eq.1) call AllocRch(idealloc)
if (mpck(pevt).eq.1) call AllocEvt(idealloc)
if (mpck(pchd).eq.1) call AllocChd(idealloc)
if (mmod(pscr).eq.1) call AllocScr(idealloc)
if (mmod(ppwt).eq.1) call AllocPwt(idealloc)

if (mmod(pcap).eq.1.or.len_trim(dxcfile).gt.0) call AllocDxc(idealloc)

!## close all units
CALL imod_utl_closeunits()

IF(NMULT.LE.1)EXIT

ENDDO MULTLOOP

IUPESTOUT=0
IUPESTPROGRESS=0
IUPESTEFFICIENCY=0
IUPESTSENSITIVITY=0
IUPESTRUNFILE=0

END SUBROUTINE RF2MF_MODFLOW

!###====================================================================
SUBROUTINE RF2MF_MAIN(DXCFILE,lipest,idf)
!###====================================================================
use pks_imod_utl, only: pks_imod_utl_iarmwp_xch_disable ! PKS
IMPLICIT NONE
type(idfobj),intent(inout) :: idf
CHARACTER(LEN=*), INTENT(INOUT) :: DXCFILE
logical, intent(inout) :: lipest

INTEGER :: IPCK,I, myrank
logical :: lcap

!...     fill: met package
met%kws(imet_coord_xll)%type = imetr
met%kws(imet_coord_xll)%rval = simbox(1)
met%kws(imet_coord_yll)%type = imetr
met%kws(imet_coord_yll)%rval = simbox(2)

met%kws(imet_coord_xll_nb)%type = imetr
met%kws(imet_coord_xll_nb)%rval = usebox(1)
met%kws(imet_coord_yll_nb)%type = imetr
met%kws(imet_coord_yll_nb)%rval = usebox(2)
met%kws(imet_coord_xur_nb)%type = imetr
met%kws(imet_coord_xur_nb)%rval = usebox(3)
met%kws(imet_coord_yur_nb)%type = imetr
met%kws(imet_coord_yur_nb)%rval = usebox(4)

met%kws(imet_starttime)%type = imett
starttime = '20000101'
met%kws(imet_starttime)%time%year  = 2000
met%kws(imet_starttime)%time%month = 1
met%kws(imet_starttime)%time%day   = 1

!## read modules ...
lcap = .false.
DO
 !## determine whether to stop, if all active modules have been passed!
 IF(.NOT.RF2MF_UTL_READNLINES(0,IPCK))EXIT
 !## count for existence of module
 RFMOD(IPCK)=-1*RFMOD(IPCK)
 SELECT CASE (IPCK)
 CASE (PPST)
   IF(MMOD(PPST).GT.0)THEN
    lipest = .true.
    call pks7mpinotsupported('iPEST') ! PKS
    CALL PEST1INIT(0,'',0,rootres,idf=idf,nparam=NLINES)
   END IF
  CASE (PCAP)
!   !## read/prepare simgro-files (capsim/metaswap)
   lcap = .true.
   call pks7mpigetmyrank(myrank)
   CALL RF2MF_METASWAP(DXCFILE,idf)
   call pks7mpibarrier()
  CASE DEFAULT
   !## read/scale BASIC packages (ibound,shead,kd,c,s,top,bot)
   CALL RF2MF_READ1MAIN(IPCK,0)
 END SELECT
 !## evaluate modules to be processed
 DO I=1,MXMOD; IF(RFMOD(I).GT.0)EXIT; ENDDO
 !## all processed stop!
 IF(I.GT.MXMOD)THEN
     EXIT
 ENDIF
ENDDO
RFMOD=ABS(RFMOD)
dxc%fname = dxcfile
if(.not.lcap) call pks_imod_utl_iarmwp_xch_disable() ! PKS

!#read empty string from runfile
READ(IURUN,*)

DO KPER=1,NPER

 !## read stress information
 CALL RF2MF_PERIODDEFINITION()

 !## change number of packages in case concentrations are to be used (not all packages!)
 IF(MMOD(PCON).EQ.1 .AND. KPER.EQ.1)THEN
  DO IPCK=1,MXPCK
   SELECT CASE (IPCK)
    CASE (PGHB,PRIV,PDRN)
     IF(MPCK(IPCK).EQ.1)THEN
      PDIM(IPCK)=PDIM(IPCK)+1
      if (ipck.eq.pghb) then
       !ghb%gconc = .true. ! TO BE ENABLED LATER ON
       !ghb%sp(:)%gcd%ncolumns = 3 ! TO BE ENABLED LATER ON
      end if
      if (ipck.eq.priv) then
       riv%rconc = .true.
       riv%sp(:)%gcd%ncolumns = 5
      end if
      if (ipck.eq.pdrn) then
       !drn%dconc = .true. ! TO BE ENABLED LATER ON
       !drn%sp(:)%gcd%ncolumns = 3 ! TO BE ENABLED LATER ON
      end if
     END IF
   END SELECT
  ENDDO
 ENDIF
! !## add two columns in case top and bottom are used
! IF(MMOD(PTOP).EQ.1.AND.MMOD(PBOT).EQ.1)PDIM(PWEL)=PDIM(PWEL)+2

 DO
  !## do until time information is read
  IF(.NOT.RF2MF_UTL_READNLINES(1,IPCK))EXIT
  RFPCK(IPCK)=-1*RFPCK(IPCK)
  CALL RF2MF_READ1MAIN(IPCK,1)
  DO I=1,MXPCK; IF(RFPCK(I).GT.0)EXIT; ENDDO
  !## stop if everything read
  IF(I.GT.MXPCK)EXIT
 ENDDO
 RFPCK=ABS(RFPCK)

 ENDDO  !DO KPER=1,NPER

if (.not.savebuf.and.savenobuf) then
   met%kws(imet_save_no_buf)%type = imetc
end if

!met%kws(imet_idate_save)%type = imeti
!met%kws(imet_idate_save)%ival = 0

CALL IMOD_UTL_PRINTTEXT('  ',3)
CALL IMOD_UTL_PRINTTEXT('---------------------------------------------',3)
CALL IMOD_UTL_PRINTTEXT('  ',3)
CALL IMOD_UTL_PRINTTEXT('   Succesfully TRANSLATED iMODFLOW '//TRIM(RVERSION),3)
CALL IMOD_UTL_PRINTTEXT('  ',3)
CALL IMOD_UTL_PRINTTEXT('---------------------------------------------',3)

!## close all files ...
CALL IMOD_UTL_CLOSEUNITS()

END SUBROUTINE RF2MF_MAIN

!###====================================================================
SUBROUTINE RF2MF_PERIODDEFINITION()
!###====================================================================
IMPLICIT NONE
INTEGER :: I,IPER,IOS,IYR,IMH,IDY,IHR,IMT,ISC
CHARACTER(LEN=256) :: LINE

!## because not all packages are needed, read until correct time-info is found!
DO
 READ(IURUN,'(A256)',IOSTAT=IOS) LINE
 IF(IOS.LT.0)CALL IMOD_UTL_PRINTTEXT('Error reading/finding time heading stressperiod '//TRIM(IMOD_UTL_ITOS(KPER)),-3)
 READ(LINE,*,IOSTAT=IOS) IPER,DELT,CDATE,ISAVE,isumbudget
 if(ios.ne.0)then
  !## set isum flag by ibdg
  isumbudget=ibdg
  READ(LINE,*,IOSTAT=IOS) IPER,DELT,CDATE,ISAVE
 endif
 IF(IOS.EQ.0)THEN
  !## check appropriate stress-definition, CDATE must be character or integer
  IF(INDEX(CDATE,'.').EQ.0)EXIT
 ENDIF
ENDDO


!if(pks%active) isave = -abs(isave)
dis%sp(kper)%perlen=delt
dis%sp(kper)%nstp=1
dis%sp(kper)%tsmult=1.0
dis%sp(kper)%writeoc=.true.
dis%sp(kper)%sstr='SS'   !## steady
if (delt.gt.0.0) then
   dis%sp(kper)%sstr='TR'  !## transient
   !## take first transient period - first can be steady-state
   if (kper.eq.1.or. &
      (kper.eq.2.and.dis%sp(1)%sstr.eq.'SS'))then
      starttime = cdate(1:8)
      read(cdate(1:4),*) met%kws(imet_starttime)%time%year
      read(cdate(5:6),*) met%kws(imet_starttime)%time%month
      read(cdate(7:8),*) met%kws(imet_starttime)%time%day
   end if
end if
if (isave.eq.0) dis%sp(kper)%writeoc=.false.
if (isave.eq.-1) savenobuf = .true.
if (isave.eq.1)  savebuf   = .true.

!## overrule isave incl. buffer whenever nmult>1, merging does not work otherwise!
IF(NMULT.GT.1)ISAVE=ABS(ISAVE)
IF(NSCL .GE.3)ISAVE=ABS(ISAVE)
!## in debug mode write result for each timestep
IF(IIDEBUG.EQ.1)ISAVE=1

!## switch between steady-state and transient during simulation
IF(DELT.LT.0.0)CALL IMOD_UTL_PRINTTEXT('Error, delt.lt.0.0 for timestep '//TRIM(IMOD_UTL_ITOS(KPER)),-3)
IF(DELT.EQ.0.0)ISS=1
IF(DELT.GT.0.0)ISS=2

IF(ISS.EQ.2.AND.MMOD(PSTO).EQ.0)THEN
 CALL IMOD_UTL_PRINTTEXT('',3)
 CALL IMOD_UTL_PRINTTEXT('No storage loaded, therefore no transient simulation allowed.',3)
 CALL IMOD_UTL_PRINTTEXT('You should not specify delt='//TRIM(IMOD_UTL_RTOS(DELT,'F',2))//' for timestep '//TRIM(IMOD_UTL_ITOS(KPER)),3)
 CALL IMOD_UTL_PRINTTEXT('For steady-state simulation specify delt=0.0',3)
 CALL IMOD_UTL_PRINTTEXT('',-3)
ENDIF

IF(ISS.NE.2.AND.MMOD(PCAP).EQ.1)CALL IMOD_UTL_PRINTTEXT('SIMGRO: CAPSIM/MetaSwap not suitable for steady-state simulations!',-3)
IF(ISS.NE.2.AND.MMOD(PPWT).EQ.1)CALL IMOD_UTL_PRINTTEXT('PWT PACKAGE not suitable for steady-state simulations!',-3)

IF(SDATE.GT.0)THEN
 I=IMOD_UTL_IDATETOJDATE(SDATE)
 IF(I.LE.0)CALL IMOD_UTL_PRINTTEXT('Error, SDATE is out of real date notation for timestep '//TRIM(IMOD_UTL_ITOS(KPER)),-3)
 !## julian date, add current time-step length
 SDATE=I+INT(DELT)
 SDATE=IMOD_UTL_JDATETOIDATE(SDATE)
 CDATE=TRIM(IMOD_UTL_ITOS(SDATE))
ENDIF

CALL IMOD_UTL_S_CAP(CDATE,'U')
IF(IPER.NE.KPER)CALL IMOD_UTL_PRINTTEXT('Something probably wrong iper.ne.kper',3)

IF(ISS.EQ.1.AND.TRIM(CDATE).NE.'STEADY-STATE')THEN
 CALL IMOD_UTL_PRINTTEXT('',3)
 CALL IMOD_UTL_PRINTTEXT('For reasons of compatibility with iMOD it is adviseable to use the',3)
 CALL IMOD_UTL_PRINTTEXT('name STEADY-STATE as result name for steady-state simulations',3)
 CALL IMOD_UTL_PRINTTEXT('',3)
ENDIF

!## which box-size to be saved
SAVEBOX=SIMBOX
IF(ISAVE.EQ.-1)SAVEBOX=USEBOX

!## write current simulation period
CALL IMOD_UTL_PRINTTEXT('',3)
WRITE(LINE,'(52A1)') ('=',I=1,52)
CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),3)
IF(ISS.EQ.1)THEN
 CALL IMOD_UTL_PRINTTEXT('Start steady-state period',3)
ELSEIF(ISS.EQ.2)THEN
! READ(CDATE,*,IOSTAT=IOS) I
! READ(CDATE,*,IOSTAT=IOS) DDATE
! IF(IOS.EQ.0)THEN
!  IF(DDATE.LT.100000000)DDATE=DDATE*1000000
!  CALL IMOT_UTL_ITIMETOGDATE(DDATE,IYR,IMH,IDY,IHR,IMT,ISC)
!  I=IMOD_UTL_IDATETOJDATE(I)+INT(DELT)
!  I=IMOD_UTL_JDATETOIDATE(I)
!  CALL IMOD_UTL_PRINTTEXT('Starting Transient simulation for: '//TRIM(CDATE)//'-'//TRIM(IMOD_UTL_ITOS(I)),3)
! ELSE
! ENDIF
  CALL IMOD_UTL_PRINTTEXT('Starting Transient simulation for: '//TRIM(CDATE),3)
! ENDIF
ENDIF
WRITE(LINE,'(52A1)') ('=',I=1,52)
CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),3)

END SUBROUTINE RF2MF_PERIODDEFINITION

!#####=================================================================
SUBROUTINE RF2MF_DATASET1()
!#####=================================================================
IMPLICIT NONE

INTEGER :: IOS

READ(IURUN,'(A256)',IOSTAT=IOS) RESULTDIR
RESULTDIR = ADJUSTL(RESULTDIR)

!## read without quotes
IF(RESULTDIR(1:1).EQ.CHAR(34).OR.RESULTDIR(1:1).EQ.CHAR(39))THEN ! double quote: CHAR(34); single quote: CHAR(39)
 READ(RESULTDIR,*,IOSTAT=IOS) RESULTDIR
ENDIF 
CALL IMOD_UTL_STRING(RESULTDIR)
CALL IMOD_UTL_ABS_PATH(RESULTDIR)
IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('ERROR line 1:'//TRIM(RESULTDIR),-3)
CALL IMOD_UTL_FILENAME(RESULTDIR)

END SUBROUTINE RF2MF_DATASET1

!#####=================================================================
SUBROUTINE RF2MF_DATASET2()
!#####=================================================================
use pks_imod_utl, only: pks_imod_utl_iarmwp_xch_init ! PKS
IMPLICIT NONE
INTEGER :: JJ,JS,IOS
character(len=8) :: date

READ(IURUN,'(A256)') LINE
CALL IMOD_UTL_STRING(LINE)

READ(LINE,*,IOSTAT=IOS) NLAY,MXNLAY,NPER,SDATE,NSCL,IFTEST,ICONCHK,IIPF,IUNCONF,IFVDL,IARMWP
IF(IOS.NE.0)THEN
 IARMWP=0; READ(LINE,*,IOSTAT=IOS) NLAY,MXNLAY,NPER,SDATE,NSCL,IFTEST,ICONCHK,IIPF,IUNCONF,IFVDL
 IF(IOS.NE.0)THEN
  IFVDL=0; READ(LINE,*,IOSTAT=IOS) NLAY,MXNLAY,NPER,SDATE,NSCL,IFTEST,ICONCHK,IIPF,IUNCONF
  IF(IOS.NE.0)THEN
   IUNCONF=0; READ(LINE,*,IOSTAT=IOS) NLAY,MXNLAY,NPER,SDATE,NSCL,IFTEST,ICONCHK,IIPF
  ENDIF
 ENDIF
ENDIF

if (nscl.ne.1) then
 call pks7mpinotsupported('Non-uniform grid')
end if

call pks_imod_utl_iarmwp_xch_init(iarmwp) ! PKS

if (ifvdl.ne.0) then
    riv%ifvdl=.true.
end if

if(sdate.ne.1)sdate=0
met%kws(imet_idate_save)%type = imeti
met%kws(imet_idate_save)%ival = sdate

!if (sdate.gt.0) then
!   write(date,'(i8)') sdate
!   starttime = date(1:8)
!   read(date(1:4),*) met%kws(imet_starttime)%time%year
!   read(date(5:6),*) met%kws(imet_starttime)%time%month
!   read(date(7:8),*) met%kws(imet_starttime)%time%day
!end if
IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('ERROR line 2:'//TRIM(LINE),-3)
IF(NPER.EQ.0)CALL IMOD_UTL_PRINTTEXT('NPER.EQ.0',-3)
IF(NLAY.EQ.0.OR.NLAY.GT.MXNLAY)CALL IMOD_UTL_PRINTTEXT('NLAY.EQ.0.OR.NLAY.GT.MXNLAY',-3)
IF(SDATE.LT.0)CALL IMOD_UTL_PRINTTEXT('SDATE.LT.0',-3)
IF(SDATE.GT.0)THEN
 IF(IMOD_UTL_IDATETOJDATE(SDATE).EQ.0)CALL IMOD_UTL_PRINTTEXT('SDATE ['//TRIM(IMOD_UTL_ITOS(SDATE))//'] not valid/existing date',-3)
ENDIF

IF(NSCL.LT.0.OR.NSCL.GT.4)CALL IMOD_UTL_PRINTTEXT('NSCL.LT.0.OR.NSCL.GT.4',-3)

LQD=.TRUE.
IF(NSCL.EQ.0.OR.NSCL.EQ.2.OR.NSCL.EQ.4)LQD=.FALSE.

IF(IIPF.NE.0)THEN
 IF(ALLOCATED(TS))DEALLOCATE(TS)
 ALLOCATE(TS(ABS(IIPF)))
 DO JJ=1,ABS(IIPF)
  READ(IURUN,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('ERROR DataSet 3 (see manual):'//TRIM(LINE),-3)
  !## swap / -> \ in case of Linux (temporary)
  CALL IMOD_UTL_STRING(LINE); JS=OS; OS=1; CALL IMOD_UTL_SWAPSLASH(LINE)
  READ(LINE,*,IOSTAT=IOS) TS(JJ)%IPFNAME,TS(JJ)%IPFTYPE,TS(JJ)%IXCOL,TS(JJ)%IYCOL,TS(JJ)%ILCOL,TS(JJ)%IMCOL,TS(JJ)%IVCOL
  IF(IOS.NE.0)THEN
   TS(JJ)%IXCOL=1; TS(JJ)%IYCOL=2; TS(JJ)%ILCOL=3; TS(JJ)%IMCOL=0; TS(JJ)%IVCOL=0
   READ(LINE,*,IOSTAT=IOS) TS(JJ)%IPFNAME,TS(JJ)%IPFTYPE
  ENDIF
  IF(IOS.NE.0)THEN
   IF(NPER.EQ.1)TS(JJ)%IPFTYPE=1; IF(NPER.GT.1)TS(JJ)%IPFTYPE=2
   READ(LINE,*,IOSTAT=IOS) TS(JJ)%IPFNAME
  ENDIF
  !## swap back again
  !OS=JS;
  CALL IMOD_UTL_SWAPSLASH(TS(JJ)%IPFNAME)
  IF(NPER.EQ.1.AND. TS(JJ)%IPFTYPE.GE.2)CALL IMOD_UTL_PRINTTEXT('For steady-state simulation IPFTYPE(.)=1',-3)
  IF(NPER.GT.1.AND.(TS(JJ)%IPFTYPE.LT.2.OR.TS(JJ)%IPFTYPE.GT.3))CALL IMOD_UTL_PRINTTEXT('for transient simulations IPFTYPE(.)=2 or IPFTYPE(.)=3',-3)
  INQUIRE(FILE=TS(JJ)%IPFNAME,EXIST=LEX)
  CALL IMOD_UTL_PRINTTEXT('  - '//TRIM(TS(JJ)%IPFNAME(INDEX(TS(JJ)%IPFNAME,'\',.TRUE.)+1:)),3)
  IF(.NOT.LEX)CALL IMOD_UTL_PRINTTEXT('IPF-file does not exist',-3)
 ENDDO
ELSE
   DO JJ=1,ABS(IIPF)
     READ(IURUN,'(A256)',IOSTAT=IOS) LINE
    IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('ERROR DataSet 3 (see manual):'//TRIM(LINE),-3)
   END DO
END IF

END SUBROUTINE RF2MF_DATASET2

!#####=================================================================
SUBROUTINE RF2MF_DATASET3()
!#####=================================================================
IMPLICIT NONE

INTEGER :: IOS, N, MYRANK
CHARACTER(LEN=1) :: SLASH

READ(IURUN,'(A256)') LINE
CALL IMOD_UTL_STRING(LINE)
READ(LINE,*,IOSTAT=IOS) NMULT,IIDEBUG,IDOUBLE,IPOSWEL,ISCEN,IBDG,MINKD,MINC
if (ios.eq.0) then
  bcf%iminkd = 1
  bcf%iminc = 1
  bcf%minkd = minkd
  bcf%minc  = minc
endif
IF(IOS.NE.0)THEN
 READ(LINE,*,IOSTAT=IOS) NMULT,IIDEBUG,IDOUBLE,IPOSWEL,ISCEN,IBDG,MINKD
 if (ios.eq.0) then
   bcf%iminkd = 1
   bcf%minkd = minkd
 endif
 IF(IOS.NE.0)THEN
  READ(LINE,*,IOSTAT=IOS) NMULT,IIDEBUG,IDOUBLE,IPOSWEL,ISCEN,IBDG
  IF(IOS.NE.0)THEN
   IBDG=0
   READ(LINE,*,IOSTAT=IOS) NMULT,IIDEBUG,IDOUBLE,IPOSWEL,ISCEN
  ENDIF
 ENDIF
ENDIF
IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('ERROR DataSet 4 (see manual):'//TRIM(LINE),-3)

met%kws(imet_savedouble)%ival=idouble
met%kws(imet_savedouble)%type = imeti

!#overrule submodel whenever iflag(2)=active
IF(IFLAG(2).GT.0)THEN
 IF(IFLAG(2).GT.NMULT)CALL IMOD_UTL_PRINTTEXT('IFLAG(2).GT.NMULT',-3)
ENDIF

debugflag = iidebug
if(abs(debugflag)==1)then
   N = LEN_TRIM(RESULTDIR)
   IF (RESULTDIR(N:N).EQ.'/'.OR.RESULTDIR(N:N).EQ.'\') THEN
      SLASH =''
   ELSE
      CALL IMOD_UTL_GETSLASH(SLASH)
   END IF
   met%kws(imet_write_debug_idf)%type = imetc   
   MET%KWS(IMET_WRITE_DEBUG_IDF)%CVAL = TRIM(RESULTDIR)//SLASH//'debug'
   CALL PKS7MPIGETMYRANK(MYRANK)
   IF(.NOT.IMOD_UTL_DIREXIST(MET%KWS(IMET_WRITE_DEBUG_IDF)%CVAL))THEN
      IF(MYRANK.EQ.0)CALL IMOD_UTL_CREATEDIR(MET%KWS(IMET_WRITE_DEBUG_IDF)%CVAL)
   END IF
end if

IF(NMULT.GE.1.AND.NSCL.EQ.0)    CALL IMOD_UTL_PRINTTEXT('You can not combine nmult.ge.1. with given raster definition (nscl.eq.0)',-3)

!memory of submodels yet done!
IF(ALLOCATED(JACT))DEALLOCATE(JACT)
ALLOCATE(JACT(NMULT))
JACT=0

END SUBROUTINE RF2MF_DATASET3

!#####=================================================================
SUBROUTINE RF2MF_DATASET4(lidfmerge)
!#####=================================================================
IMPLICIT NONE

logical, intent(inout) :: lidfmerge

INTEGER :: IOS

! options to skip
INTEGER :: MXCNVG,IDELTCNVG,IDAMPING,I
REAL :: MAXWBALERROR

! options PKS
LOGICAL :: WRITESTO
INTEGER :: MXITER, NRPROC, idfmerge

READ(IURUN,'(A256)') LINE
CALL IMOD_UTL_STRING(LINE)
READ(LINE,*,IOSTAT=IOS) MXITER

IF(MXITER.LT.0) THEN ! PKS SOLVER OPTIONS
  idfmerge = 0   
  call AllocPks(ialloc)  
  READ(LINE,*,IOSTAT=IOS) pks%MXITER,pks%INNERIT,pks%HCLOSEPKS,pks%RCLOSEPKS,pks%RELAXPKS,pks%partopt,idfmerge,pks%pressakey
  IF(IOS.NE.0)THEN
    READ(LINE,*,IOSTAT=IOS) pks%MXITER,pks%INNERIT,pks%HCLOSEPKS,pks%RCLOSEPKS,pks%RELAXPKS,pks%partopt
    IF(IOS.NE.0)THEN
      READ(LINE,*,IOSTAT=IOS) pks%MXITER,pks%INNERIT,pks%HCLOSEPKS,pks%RCLOSEPKS,pks%RELAXPKS,pks%partopt
      IF(IOS.NE.0)THEN
        READ(LINE,*,IOSTAT=IOS) pks%MXITER,pks%INNERIT,pks%HCLOSEPKS,pks%RCLOSEPKS,pks%RELAXPKS
      END IF 
    END IF
  END IF
  pks%MXITER = abs(pks%MXITER)
  if (idfmerge.eq.1) lidfmerge = .true.
  IF(pks%partopt.ne.0 .and. pks%partopt.ne.1 .and. pks%partopt.ne.2)THEN
    CALL IMOD_UTL_PRINTTEXT('Dataset 5: PARTOPT must be 0, 1, or 2',-3)          
  ENDIF    
  if (pks%partopt.eq.1.or.pks%partopt.eq.2) then ! RCB load file
    if (pks%partopt.eq.1) then
       CALL IMOD_UTL_PRINTTEXT('Dataset 5: PARTOPT = 1 (RCB) activated',3)  
    else
       CALL IMOD_UTL_PRINTTEXT('Dataset 5: PARTOPT = 2 (RCB) activated',3)  
    end if        
    READ(IURUN,'(A256)') LINE; CALL IMOD_UTL_STRING(LINE)
    READ(LINE,'(A)',IOSTAT=IOS) pks%loadfile
    pks%loadfile = adjustl(pks%loadfile)
    CALL IMOD_UTL_FILENAME(pks%loadfile)
    !## read without quotes
    IF(pks%loadfile(1:1).EQ.CHAR(34).OR.pks%loadfile(1:1).EQ.CHAR(39))THEN ! double quote: CHAR(34); single quote: CHAR(39)
       READ(pks%loadfile,*) pks%loadfile
    ENDIF
    if (pks%partopt.eq.1) then
       IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Dataset 5: PARTOPT = 1 (RCB)',-3)
    else
       IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Dataset 5: PARTOPT = 2 (RCB)',-3)
    end if        
  end if
  if (pks%partopt.eq.3) then ! read partitions - NOT YET SUPPORTED!
    CALL IMOD_UTL_PRINTTEXT('Dataset 5: PARTOPT = 2 activated',3)  
    READ(IURUN,'(A256)') LINE; CALL IMOD_UTL_STRING(LINE)
    READ(LINE,*,IOSTAT=IOS) pks%nrproc 
    IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Dataset 5b: PARTOPT = 2 , could not read NRPROC',-3)
    IF(pks%nrproc.LE.0)CALL IMOD_UTL_PRINTTEXT('Dataset 5b: PARTOPT = 2 , invalid NRPROC',-3)
    allocate(pks%partminmax(pks%nrproc,4))
    DO i = 1, pks%nrproc  
      READ(IURUN,'(A256)') LINE; CALL IMOD_UTL_STRING(LINE)
      READ(LINE,*,IOSTAT=IOS) pks%partminmax(i,1), pks%partminmax(i,2),& 
                              pks%partminmax(i,3), pks%partminmax(i,4)  
      IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Dataset 5b: PARTOPT = 2, could not read partition bounds',-3)
    END DO      
  end if    
ELSE ! PCG SOLVER
  CALL PKS7MPIGETNRPROC(NRPROC)
  IF(NRPROC.GT.1)CALL IMOD_UTL_PRINTTEXT('Dataset 5: only PKS is supported in parallel mode',-3)
  call AllocPcg(ialloc)
  READ(LINE,*,IOSTAT=IOS) pcg%MXITER,pcg%ITER1,pcg%HCLOSE,pcg%RCLOSE,pcg%RELAX,pcg%NPCOND,MAXWBALERROR,MXCNVG,IDELTCNVG,IDAMPING
  IF(IOS.NE.0)THEN
   IDAMPING=0
   READ(LINE,*,IOSTAT=IOS) pcg%MXITER,pcg%ITER1,pcg%HCLOSE,pcg%RCLOSE,pcg%RELAX,pcg%NPCOND,MAXWBALERROR,MXCNVG,IDELTCNVG
   IF(IOS.NE.0)THEN
    IDELTCNVG=0
    READ(LINE,*,IOSTAT=IOS) pcg%MXITER,pcg%ITER1,pcg%HCLOSE,pcg%RCLOSE,pcg%RELAX,pcg%NPCOND,MAXWBALERROR,MXCNVG
    IF(IOS.NE.0)THEN
     MXCNVG=pcg%MXITER*pcg%ITER1
     READ(LINE,*,IOSTAT=IOS) pcg%MXITER,pcg%ITER1,pcg%HCLOSE,pcg%RCLOSE,pcg%RELAX,pcg%NPCOND,MAXWBALERROR
     IF(IOS.NE.0)THEN
      MAXWBALERROR=0.0
      READ(LINE,*,IOSTAT=IOS) pcg%MXITER,pcg%ITER1,pcg%HCLOSE,pcg%RCLOSE,pcg%RELAX,pcg%NPCOND
      IF(IOS.NE.0)THEN
       pcg%NPCOND=1
       READ(LINE,*,IOSTAT=IOS) pcg%MXITER,pcg%ITER1,pcg%HCLOSE,pcg%RCLOSE,pcg%RELAX
      ENDIF
     END IF
    ELSE
     CALL IMOD_UTL_PRINTTEXT('Dataset 5: ***warning*** option MXCNVG is not supported and will be ignored!',3)
    END IF
   ELSE
    CALL IMOD_UTL_PRINTTEXT('Dataset 5: ***warning*** option MXCNVG is not supported and will be ignored!',3)
    CALL IMOD_UTL_PRINTTEXT('Dataset 5: ***warning*** option IDELTCNVG is not supported and will be ignored!',3)
   END IF
  ELSE
   CALL IMOD_UTL_PRINTTEXT('Dataset 5: ***warning*** option MXCNVG is not supported and will be ignored!',3)
   CALL IMOD_UTL_PRINTTEXT('Dataset 5: ***warning*** option IDELTCNVG is not supported and will be ignored!',3)
   CALL IMOD_UTL_PRINTTEXT('Dataset 5: ***warning*** option IDAMPING is not supported and will be ignored!',3)
  END IF
  
  if (pcg%npcond.ne.1 .and.pcg%npcond.ne.2) then
   CALL IMOD_UTL_PRINTTEXT('Dataset 5: NPCOND must be 1 or 2',-3)
  end if
if(MAXWBALERROR.gt.0.0)then
 bas%options=trim(bas%options)//' STOPERROR '//TRIM(IMOD_UTL_RTOS(MAXWBALERROR,'G',5))
endif
  
  pcg%NBPOL=1;IF(pcg%NPCOND.EQ.2)pcg%NBPOL=2
END IF

IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('ERROR line 4:'//TRIM(LINE),-3)

END SUBROUTINE RF2MF_DATASET4

!#####=================================================================
SUBROUTINE RF2MF_DATASET5()
!#####=================================================================
IMPLICIT NONE
INTEGER :: I,IOS,n
REAL :: INCREASE
CHARACTER(LEN=50) :: CSUBMODEL
CHARACTER(LEN=1) :: SLASH

IF(NMULT.GT.1)THEN

 !## skip previous processed part of run-file
 DO I=1,IMULT; READ(IURUN,*); ENDDO

 !## search for active non-existing directory!
 DO IMULT=I,NMULT
  READ(IURUN,'(A256)') LINE
  CALL IMOD_UTL_STRING(LINE)
  IF(LQD)THEN
   READ(LINE,*,IOSTAT=IOS)  IACT,USEBOX(1:4),SIMCSIZE            ,LAMBDA(1),CSUBMODEL
  ELSE
   IF(NSCL.NE.0)THEN
    READ(LINE,*,IOSTAT=IOS) IACT,USEBOX(1:4),SIMCSIZE,MAXSIMCSIZE,LAMBDA(1),CSUBMODEL
   ENDIF
  ENDIF
  IF(IOS.NE.0)THEN
   IF(LQD)THEN
    READ(LINE,*,IOSTAT=IOS)  IACT,USEBOX(1:4),SIMCSIZE            ,LAMBDA(1)
   ELSE
    IF(NSCL.NE.0)THEN
     READ(LINE,*,IOSTAT=IOS) IACT,USEBOX(1:4),SIMCSIZE,MAXSIMCSIZE,LAMBDA(1)
    ENDIF
   ENDIF
   CSUBMODEL='submodel'//TRIM(IMOD_UTL_ITOS(IMULT))
  ELSE
   CSUBMODEL=TRIM(CSUBMODEL)
  ENDIF
  IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('ERROR line '//TRIM(IMOD_UTL_ITOS(5+IMULT))//': '//TRIM(LINE),-3)
  RESULTRESDIR=TRIM(RESULTDIR)//CHAR(92)//TRIM(CSUBMODEL)
  CALL IMOD_UTL_FILENAME(RESULTRESDIR)

  !## overrule submodel whenever iflag(2)=active
  IF(IFLAG(2).NE.0)THEN
   IACT=0
   IF(IFLAG(2).EQ.IMULT)IACT=1
  ENDIF

  !## do it no matter what!
  IF(IACT.EQ.1.AND.JACT(IMULT).EQ.0)THEN
   IF(.NOT.IMOD_UTL_DIREXIST(RESULTRESDIR))CALL IMOD_UTL_CREATEDIR(RESULTRESDIR)
   EXIT
  ENDIF
  !## do it if not exist an unique filename)
  IF(IACT.EQ.-1)THEN
   IF(.NOT.IMOD_UTL_DIREXIST(RESULTRESDIR))THEN
    CALL IMOD_UTL_CREATEDIR(RESULTRESDIR)
    IACT=1
    EXIT
   ELSE
    IACT=1
   ENDIF
  ENDIF

 ENDDO

 !## skip remaining part of run-file
 DO I=IMULT+1,NMULT; READ(IURUN,*); ENDDO

ELSE

 IF(NMULT.EQ.1)THEN
  READ(IURUN,'(A256)') LINE
  CALL IMOD_UTL_STRING(LINE)

  IF(LQD)THEN
   READ(LINE,*,IOSTAT=IOS) USEBOX(1:4),SIMCSIZE,LAMBDA(1)
  ELSE
   IF(NSCL.NE.0)THEN
    READ(LINE,*,IOSTAT=IOS) USEBOX(1:4),SIMCSIZE,MAXSIMCSIZE,LAMBDA(1)
   ENDIF
  ENDIF
  IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('ERROR line 5:'//TRIM(LINE),-3)
  IMULT    =1
 ELSEIF(NMULT.EQ.0)THEN
  IMULT    =0
 ENDIF
 RESULTRESDIR  =RESULTDIR
 CSUBMODEL=''
 IACT     =1

ENDIF

IF(NSCL.NE.0.AND.SIMCSIZE.LE.0.0)CALL IMOD_UTL_PRINTTEXT('ERROR Grid Size le 0.0',-3)

DO I=2,4; LAMBDA(I)=LAMBDA(1); ENDDO

IF(IMULT.GT.NMULT)IACT=0
IF(IACT.EQ.0)RETURN

IF(NMULT.GT.1)THEN
 CALL IMOD_UTL_PRINTTEXT('Computing box: '//TRIM(CSUBMODEL),3)
 CALL IMOD_UTL_PRINTTEXT('',3)
ENDIF
IF(NMULT.GT.1)JACT(IMULT)=IACT

N = LEN_TRIM(RESULTDIR)
IF (RESULTDIR(N:N).EQ.'/'.OR.RESULTDIR(N:N).EQ.'\') THEN
 SLASH =''
ELSE   
 CALL IMOD_UTL_GETSLASH(SLASH)
END IF
IF(TRIM(CSUBMODEL).EQ.'')THEN
 LINE=TRIM(RESULTDIR)//TRIM(SLASH)//'log_'//TRIM(RVERSION)//'.txt'
ELSE
 LINE=TRIM(RESULTDIR)//TRIM(SLASH)//'log_'//TRIM(CSUBMODEL)//'_'//TRIM(RVERSION)//'.txt'
ENDIF
n = len_trim(line) ! PKS
call pks7mpifname( line, n ) ! PKS
CALL IMOD_UTL_FILENAME(LINE)
IUOUT=IMOD_UTL_GETUNIT()
OPEN(IUOUT,FILE=LINE,STATUS='UNKNOWN',ACTION='WRITE')
IF(IUOUT.LE.0)CALL IMOD_UTL_PRINTTEXT('Cannot open output file '//TRIM(LINE),-3)
WRITE(IUOUT,'(A)') 'MODEL SIMULATION - SUMMARY'
WRITE(IUOUT,*)

root%submodel = csubmodel
root%resultdir = trim(resultdir)
rootres=resultdir
call imod_utl_abs_path(root%resultdir)
! strip last slash
n = len_trim(root%resultdir)
if (root%resultdir(n:n).eq.'/'.or.root%resultdir(n:n).eq.'\') then
   root%resultdir = root%resultdir(1:n-1)
end if
met%kws(imet_resultdir)%type = imetc
if (len_trim(csubmodel).gt.0) then
   met%kws(imet_resultdir)%cval = trim(root%resultdir)//'\'//trim(csubmodel)
else
   met%kws(imet_resultdir)%cval = trim(root%resultdir)
end if

END SUBROUTINE RF2MF_DATASET5

!#####=================================================================
SUBROUTINE RF2MF_DATASET6()
!#####=================================================================
IMPLICIT NONE

IF(ISCEN.EQ.0)RETURN

LINE=''
READ(IURUN,*) LINE
CALL IMOD_UTL_STRING(LINE)
CALL IMOD_UTL_FILENAME(LINE)
READ(LINE,'(A256)') SCENFNAME
INQUIRE(FILE=SCENFNAME,EXIST=LEX)
IF(.NOT.LEX)CALL IMOD_UTL_PRINTTEXT('Cannot find '//TRIM(SCENFNAME),-3)
WRITE(IUOUT,'(A)')
WRITE(IUOUT,'(A)') 'Using Scenario File: '
WRITE(IUOUT,'(A)') '  '//TRIM(SCENFNAME)

END SUBROUTINE RF2MF_DATASET6

!#####=================================================================
SUBROUTINE RF2MF_DATASET7(DXCFILE)
!#####=================================================================
IMPLICIT NONE
CHARACTER(LEN=*), INTENT(INOUT) :: DXCFILE
INTEGER :: I,J,IKEY,ilay,jlay,IOS
CHARACTER(LEN=1000) :: BIGLINE

!## initialise modules/packages
MMOD =0
MPCK =0
RFMOD=0
RFPCK=0
MODSAVE=0
PCKSAVE=0

!## read header of activated modules/packages
DO
 READ(IURUN,'(A1000)',IOSTAT=IOS) BIGLINE
 IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('ERROR line '//TRIM(BIGLINE),-3)
 CALL IMOD_UTL_STRING(BIGLINE)
 !## still within header-section containing module and package information
 IF(INDEX(BIGLINE,'(').EQ.0.OR.INDEX(BIGLINE,')').EQ.0)THEN
  !## finished reading modules/packages activation
  BACKSPACE(IURUN)
  EXIT
 ELSE
  !## find keyword:
  IKEY=RF2MF_UTL_FIND_KEYWORD(BIGLINE)
  !## module found
  IF(IKEY.GT.0)THEN
   RFMOD(IKEY)=RFMOD(IKEY)+1
   READ(BIGLINE,*,IOSTAT=IOS) MMOD(IKEY),MODSAVE(IKEY,0),(MODSAVE(IKEY,J),J=1,MODSAVE(IKEY,0))
  !## package found
  ELSE
   IKEY       =ABS(IKEY)
   RFPCK(IKEY)=RFPCK(IKEY)+1
   READ(BIGLINE,*,IOSTAT=IOS) MPCK(IKEY),PCKSAVE(IKEY,0),(PCKSAVE(IKEY,J),J=1,PCKSAVE(IKEY,0))
  ENDIF
  IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('ERROR line '//TRIM(BIGLINE),-3)
 ENDIF
END DO

call AllocNam(ialloc)
call AllocBas(ialloc)
call AllocBcf(ialloc)
call AllocMet(ialloc)
call AllocOc (ialloc)
if (mpck(priv).eq.1 .or. mpck(pisg).eq.1) call AllocRiv(ialloc,mmod(psft))
if (mpck(pdrn).eq.1 .or. mpck(polf).eq.1) call AllocDrn(ialloc,iconchk)
if (mpck(pghb).eq.1) call AllocGhb(ialloc)
if (mpck(pwel).eq.1) call AllocWel(ialloc)
if (mmod(pani).eq.1) call AllocAni(ialloc)
if (mmod(phfb).eq.1) call AllocHfb(ialloc)
if (mpck(prch).eq.1) call AllocRch(ialloc)
if (mpck(pevt).eq.1) call AllocEvt(ialloc)
if (mpck(pchd).eq.1) call AllocChd(ialloc)
if (mmod(pscr).eq.1) call AllocScr(ialloc)
if (mmod(ppwt).eq.1) call AllocPwt(ialloc)
if (mmod(pcap).eq.1.or.len_trim(dxcfile).gt.0) call AllocDxc(ialloc)

! set buget output with modsave
do ikey = 1, size(mmod)
   if (mmod(ikey).eq.0) cycle
   if(modsave(ikey,0).eq.1.and.modsave(ikey,1).eq.0)then
    modsave(ikey,0)=nlay
    do ilay=1,nlay; modsave(ikey,ilay)=ilay; enddo
   endif
   select case(ikey)
      case (pshd)
         oc%cbnlay = modsave(ikey,0)
         oc%cblay(1:modsave(ikey,0)) = modsave(ikey,1:modsave(ikey,0))
      case (psto, pbnd, pkdw, pvcw)
         if (ikey.eq.psto) trflag = .true.
        do ilay = 1, nlay
            if (bcf%cblay(ilay).eq.0 .and. modsave(ikey,ilay).gt.0) then
                bcf%cbnlay = bcf%cbnlay + 1
                bcf%cblay(ilay) = ilay
            end if
         end do
     case (pscr)
         scr%cbnlay = modsave(ikey,0)
         scr%cblay(1:modsave(ikey,0)) = modsave(ikey,1:modsave(ikey,0))
     case (pcap)
         dxc%cbnlay = modsave(ikey,0)
         dxc%cblay(1:modsave(ikey,0)) = modsave(ikey,1:modsave(ikey,0))
   end select
end do
! set budget output with modsave
do ikey = 1, size(mpck)
   if (mpck(ikey).eq.0) cycle
   if(pcksave(ikey,0).eq.1.and.pcksave(ikey,1).eq.0)then
    pcksave(ikey,0)=nlay
    do ilay=1,nlay; pcksave(ikey,ilay)=ilay; enddo
   endif
   select case(ikey)
      case (pwel)
         wel%cbnlay = pcksave(ikey,0)
         wel%cblay(1:pcksave(ikey,0)) = pcksave(ikey,1:pcksave(ikey,0))
      case (pdrn,polf)
         drn%cbnlay = pcksave(ikey,0)
         drn%cblay(1:pcksave(ikey,0)) = pcksave(ikey,1:pcksave(ikey,0))
      case (priv,pisg)
         riv%cbnlay = pcksave(ikey,0)
         riv%cblay(1:pcksave(ikey,0)) = pcksave(ikey,1:pcksave(ikey,0))
      case (pghb)
         ghb%cbnlay = pcksave(ikey,0)
         ghb%cblay(1:pcksave(ikey,0)) = pcksave(ikey,1:pcksave(ikey,0))
      case (prch)
         rch%cbnlay = pcksave(ikey,0)
         rch%cblay(1:pcksave(ikey,0)) = pcksave(ikey,1:pcksave(ikey,0))
   end select
end do

IF(SUM(MMOD).EQ.0) CALL IMOD_UTL_PRINTTEXT('Define KEYWORD between brackets, e.g. (bnd) otherwise iMODFLOW can not find them!',-3)
IF(MMOD(PBND).EQ.0)CALL IMOD_UTL_PRINTTEXT('Boundaries (bnd) are not defined in the module header',-3)
IF(MMOD(PSHD).EQ.0)CALL IMOD_UTL_PRINTTEXT('Starting Heads (shd) are not defined in the module header',-3)
IF(MMOD(PKHV).EQ.0.AND.MMOD(PKDW).EQ.0) &
   CALL IMOD_UTL_PRINTTEXT('Transmissivities (kds) or Horizontal Permeabilities (khv) are not defined in the module header',-3)
IF(NLAY.GT.1.AND.(MMOD(PKVV).EQ.0.AND.MMOD(PVCW).EQ.0)) &
   CALL IMOD_UTL_PRINTTEXT('Vertical Resistance (vcw) or Vertical Permeabilities (kvv) are not defined in the module header',-3)
IF(MMOD(PTOP)+MMOD(PBOT).EQ.1)CALL IMOD_UTL_PRINTTEXT('Top (top) and Bottom (bot) need to be defined both',-3)
IF(MMOD(PKHV).EQ.1.AND.MMOD(PTOP).EQ.0) &
 CALL IMOD_UTL_PRINTTEXT('Top (top) and Bottom (bot) needed for usage of Horizontal Permeabilities (khv)',-3)
IF(MMOD(PKVV).EQ.1.AND.MMOD(PTOP).EQ.0) &
 CALL IMOD_UTL_PRINTTEXT('Top (top) and Bottom (bot) needed for usage of Vertical Permeabilities (kvv)',-3)
IF(MMOD(PCAP).EQ.1.AND.MMOD(PSTO).EQ.0) &
 CALL IMOD_UTL_PRINTTEXT('You should specify/use the STORAGE module in combination with CapSim/MetaSwap',-3)
IF(NLAY.LE.1.AND.MMOD(PPWT).EQ.1)CALL IMOD_UTL_PRINTTEXT('PWT package not suitable for one layered model!',-3)
!IF(NLAY.EQ.1.AND.MMOD(PIBS).EQ.1)CALL IMOD_UTL_PRINTTEXT('IBS PACKAGE not suitable for one-layered model!',-3)

!## pwt activated? turn on switched output for pwt - option 3! and deactivate it whenever nscl=3/4 (ir)
IF(MMOD(PPWT).EQ.1)THEN
 DO I=1,MODSAVE(PSHD,0)
  !## output head layer #1 required
  IF(MODSAVE(PSHD,I).EQ.1)THEN
   DO J=1,MODSAVE(PPWT,0)
    IF(MODSAVE(PPWT,J).EQ.3)EXIT
   END DO
   !## turn PWT output option 3 on!
   IF(J.GT.MODSAVE(PPWT,0))THEN
    MODSAVE(PPWT,0)=MODSAVE(PPWT,0)+1
    MODSAVE(PPWT,MODSAVE(PPWT,0))=3
   ENDIF
  ENDIF
 END DO
ENDIF

CALL IMOD_UTL_PRINTTEXT('',3)
CALL IMOD_UTL_PRINTTEXT(' ACTIVE MODULES/PACKAGES',3)
CALL IMOD_UTL_PRINTTEXT('',3)
CALL IMOD_UTL_PRINTTEXT(' ===================================================================',3)
DO I=1,MXMOD
 IF(RFMOD(I).EQ.1)THEN
  IF(MMOD(I).EQ.1)WRITE(BIGLINE,'(1X,A8,I2,A13,A10,A)') 'Module  ',I,'   present ','  active  ',TRIM(TXTMOD(I))
  IF(MMOD(I).EQ.0)WRITE(BIGLINE,'(1X,A8,I2,A13,A10,A)') 'Module  ',I,'   present ',' inactive ',TRIM(TXTMOD(I))
 ELSE
  WRITE(BIGLINE,'(1X,A8,I2,A13,10X,A)') 'Module  ',I,' not present ',TRIM(TXTMOD(I))
 ENDIF
 CALL IMOD_UTL_PRINTTEXT(TRIM(BIGLINE),3)
END DO
DO I=1,MXPCK
 IF(RFPCK(I).EQ.1)THEN
  IF(MPCK(I).EQ.1)WRITE(BIGLINE,'(1X,A8,I2,A13,A10,A)') 'Package ',I,'   present ','  active  ',TRIM(TXTPCK(I))
  IF(MPCK(I).EQ.0)WRITE(BIGLINE,'(1X,A8,I2,A13,A10,A)') 'Package ',I,'   present ',' inactive ',TRIM(TXTPCK(I))
 ELSE
  WRITE(BIGLINE,'(1X,A8,I2,A13,10X,A)') 'Package ',I,' not present ',TRIM(TXTPCK(I))
 ENDIF
 CALL IMOD_UTL_PRINTTEXT(TRIM(BIGLINE),3)
END DO
CALL IMOD_UTL_PRINTTEXT(' ===================================================================',3)

END SUBROUTINE RF2MF_DATASET7

!#####=================================================================
SUBROUTINE RF2MF_INIT_ALLOCATE()
!#####=================================================================
IMPLICIT NONE
INTEGER :: I

!memory what to save
IF(ALLOCATED(MODSAVE))DEALLOCATE(MODSAVE)
IF(ALLOCATED(PCKSAVE))DEALLOCATE(PCKSAVE)
IF(ALLOCATED(TXTPCK))DEALLOCATE(TXTPCK)
IF(ALLOCATED(TXTMOD))DEALLOCATE(TXTMOD)
IF(ALLOCATED(RFPCK))DEALLOCATE(RFPCK)
IF(ALLOCATED(RFMOD))DEALLOCATE(RFMOD)
IF(ALLOCATED(MPCK))DEALLOCATE(MPCK)
IF(ALLOCATED(NMOD))DEALLOCATE(NMOD)
IF(ALLOCATED(MMOD))DEALLOCATE(MMOD)
IF(ALLOCATED(CPCK))DEALLOCATE(CPCK)
IF(ALLOCATED(CMOD))DEALLOCATE(CMOD)
IF(ALLOCATED(OPCK))DEALLOCATE(OPCK)
IF(ALLOCATED(OMOD))DEALLOCATE(OMOD)
IF(ALLOCATED(MDIM))DEALLOCATE(MDIM)
IF(ALLOCATED(PDIM))DEALLOCATE(PDIM)

ALLOCATE(MODSAVE(MXMOD,0:MAX(MXNLAY,3)))
ALLOCATE(PCKSAVE(MXPCK,0:MXNLAY))
ALLOCATE(MPCK(MXPCK))    !package activated
ALLOCATE(PDIM(MXPCK))    !package dimensions
ALLOCATE(NMOD(MXMOD))    !module activated
ALLOCATE(MMOD(MXMOD))    !module activated
ALLOCATE(MDIM(MXMOD))    !module dimensions
ALLOCATE(TXTMOD(MXMOD))
ALLOCATE(TXTPCK(MXPCK))
ALLOCATE(CMOD(MXMOD))
ALLOCATE(CPCK(MXPCK))
ALLOCATE(OMOD(MXMOD))
ALLOCATE(OPCK(MXPCK))
ALLOCATE(RFMOD(MXMOD))
ALLOCATE(RFPCK(MXPCK))

MPCK  =0
NMOD  =0
MMOD  =0

TXTMOD(PCAP) ='SIMGRO'
TXTMOD(PBND) ='IBOUND'
TXTMOD(PSHD) ='STARTING_HEAD'
TXTMOD(PKDW) ='TRANSMISSIVITIES'
TXTMOD(PVCW) ='VERTICAL_RESIST._VALUES'
TXTMOD(PSTO) ='STORAGE_COEFFICIENTS'
TXTMOD(PPWT) ='PERCHED_WATER_TABLE'
TXTMOD(PANI) ='ANISOTROPY'
TXTMOD(PHFB) ='HORIZONTAL_FLOW_BARRIER'
TXTMOD(PTOP) ='TOP_OF_AQUIFER'
TXTMOD(PBOT) ='BOTTOM_OF_AQUIFER'
TXTMOD(PCON) ='CONCENTRATION'
TXTMOD(PKHV) ='HORIZONTAL_K_VALUE'
TXTMOD(PKVV) ='VERTICAL_K_VALUE'
TXTMOD(PIBS) ='INTERBED_STORAGE'
TXTMOD(PPST) ='PARAMETER ESTIMATION'
TXTMOD(PKVA) ='VERTICAL ANISOTROPY'
TXTMOD(PSFT) ='STREAM FLOW THICKNESS'
TXTMOD(PCPP) ='COMMON POINTER MODULE'
TXTMOD(PSSC) ='STORAGE COEFFICIENT MODULE'
TXTMOD(PSCR) ='SUBSIDENCE_CREEP_PACKAGE'

TXTPCK(PWEL)='WELLS'
TXTPCK(PDRN)='DRAINS'
TXTPCK(PRIV)='RIVERS'
TXTPCK(PEVT)='EVAPOTRANSPIRATION'
TXTPCK(PGHB)='GENERAL_HEAD_BOUNDARY'
TXTPCK(PRCH)='RECHARGE'
TXTPCK(POLF)='OVERLANDFLOW'
TXTPCK(PCHD)='CONSTANT_HEAD'
TXTPCK(PISG)='SEGMENT_RIVER'

CMOD(PCAP)='CAP'
CMOD(PBND)='BND'
CMOD(PSHD)='SHD'
CMOD(PKDW)='KDW'
CMOD(PVCW)='VCW'
CMOD(PSTO)='STO'
CMOD(PPWT)='PWT'
CMOD(PANI)='ANI'
CMOD(PHFB)='HFB'
CMOD(PTOP)='TOP'
CMOD(PBOT)='BOT'
CMOD(PCON)='CON'
CMOD(PKHV)='KHV'
CMOD(PKVV)='KVV'
CMOD(PIBS)='IBS'
CMOD(PPST)='PST'
CMOD(PKVA)='KVA'
CMOD(PSFT)='SFT'
CMOD(PCPP)='CPP'
CMOD(PSSC)='SSC'
CMOD(PSCR)='SCR'

CPCK(PWEL)='WEL'
CPCK(PDRN)='DRN'
CPCK(PRIV)='RIV'
CPCK(PEVT)='EVT'
CPCK(PGHB)='GHB'
CPCK(PRCH)='RCH'
CPCK(POLF)='OLF'
CPCK(PCHD)='CHD'
CPCK(PISG)='ISG'

OMOD(PCAP)='bdgcap'
OMOD(PBND)='bdgbnd'
OMOD(PSHD)='head'
OMOD(PKDW)='bdgfrf'
OMOD(PVCW)='bdgflf'
OMOD(PSTO)='bdgsto'
OMOD(PPWT)='pwthead'
OMOD(PANI)='bdgani'
OMOD(PHFB)=''
OMOD(PTOP)=''
OMOD(PBOT)=''
OMOD(PCON)=''
OMOD(PKHV)='bdgfrf'
OMOD(PKVV)='bdgflf'
OMOD(PIBS)='bdgibs'
OMOD(PPST)='alpha'
OMOD(PKVA)=''
OMOD(PSFT)=''
OMOD(PCPP)=''
OMOD(PSSC)=''
OMOD(PSCR)='bdgscr'

OPCK(PWEL)='bdgwel'
OPCK(PDRN)='bdgdrn'
OPCK(PRIV)='bdgriv'
OPCK(PEVT)='bdgevt'
OPCK(PGHB)='bdgghb'
OPCK(PRCH)='bdgrch'
OPCK(POLF)='bdgolf'
OPCK(PCHD)='bdgchd'
OPCK(PISG)='bdgisg'

MDIM(PBND)=1   !BND (BOUNDARY)
MDIM(PSHD)=1   !SHD (SHEAD)
MDIM(PKDW)=1   !KDW (KD)
MDIM(PVCW)=1   !VCW (C)
MDIM(PSTO)=1   !STO (STORAGE)
IF(LPWT)THEN
 MDIM(PPWT)=6  !PWT (LAYER,STORAGE,TOP,THICKNESS_AQT,THICKNESS_AQF,SC1)
ELSE
 MDIM(PPWT)=22 !PWT (LAYER,STORAGE,TOP,THICKNESS_AQT,THICKNESS_AQF)
ENDIF
MDIM(PANI)=2   !ANI (FACTOR,HOEK)
MDIM(PHFB)=1   !HFB - ALEEN GEBRUIKT TIJDENS SKIPPEN IN RUNFILE
MDIM(PTOP)=1   !TOP (TOP OF AQUIFER)
MDIM(PBOT)=1   !BOT (BOT OF AQUIFER)
MDIM(PCON)=1   !CON (CONCENTRATION)
MDIM(PKHV)=1   !KHV (K-HORIZONTAL)
MDIM(PKVV)=1   !KVV (K-VERTINCAL)
MDIM(PIBS)=4   !HC,SCE,SCV,SUB
MDIM(PPST)=1   !1 zone per parameter gezet in
MDIM(PKVA)=1   !KVA (K VERTICAL ANISOTROPY)
MDIM(PSFT)=2   !idf for streamflow thickness and streamthickness permeability
MDIM(PCPP)=1   !IDF FOR POINTER
MDIM(PSSC)=1   !IDF FOR SC1 (UNCONFINED) EN SC2 (CONFINED)
MDIM(PSCR)=7   !IDF (GL0, SGM, SGS) THICKNESS, RRISOA, RRISOB, CAISOC, VOID, SUB, QLAYER, (PCSOFF)

PDIM(PWEL)=1 !??   !WEL (*.IPF)  Q [optional],Z1,Z2 [optional],CONCENTRATION
PDIM(PDRN)=2   !DRN (COND,ELEV,[optional]CONCENTRATION)
PDIM(PRIV)=4   !RIV (COND,STAGE,RBOT,FCT,[optional]CONCENTRATION)
PDIM(PEVT)=3   !EVT (E,SURF,EXDP)
PDIM(PGHB)=2   !GHB (COND,ELEV,[optional]CONCENTRATION)
PDIM(PRCH)=1   !RCH (MM/D)
PDIM(POLF)=1   !OLF (ELEV, bandwidth lowest in neighbouring cells) !, actual drain-level)
PDIM(PCHD)=1   !CHD (HEAD1,head2)
PDIM(PISG)=1   !ISG (*.ISG: COND,STAGE,RBOT,FCT)

END SUBROUTINE RF2MF_INIT_ALLOCATE

!###======================================================================
SUBROUTINE RF2MF_CHECKRUN()
!###======================================================================
IMPLICIT NONE
INTEGER :: I,J,K,LU,IOS
CHARACTER(LEN=4),DIMENSION(4) :: EXT
DATA EXT/'.IDF','.IPF','.GEN','.ISG'/
CHARACTER(LEN=256) :: FNAME

IF(IFTEST.EQ.0)RETURN

CALL IMOD_UTL_PRINTTEXT('',3)
CALL IMOD_UTL_PRINTTEXT('Checking Run-file ...',3)
CALL IMOD_UTL_PRINTTEXT('Results written in RF2MF_'//TRIM(RVERSION)//'.log',3)
CALL IMOD_UTL_PRINTTEXT('',3)

LU=IMOD_UTL_GETUNIT()
OPEN(LU,FILE='RF2MF_'//TRIM(RVERSION)//'.log',STATUS='UNKNOWN',ACTION='WRITE')
!## assign log file to the log file of the runfile
IUOUT=LU

WRITE(LU,'(A/)') 'Non-existing files:'
WRITE(LU,'(A10,1X,A)') 'Line','File'

K=0
DO
 READ(IURUN,'(A256)',IOSTAT=IOS) LINE
 IF(IOS.NE.0)EXIT
 CALL IMOD_UTL_STRING(LINE)
 CALL IMOD_UTL_S_CAP(LINE,'U')
 K=K+1
 !## try find extent
 DO I=1,SIZE(EXT)
  J=INDEX(LINE,EXT(I),.TRUE.)
  !## found extent
  IF(J.GT.0)THEN
   J=INDEX(LINE,',',.TRUE.)+1
   !## found comma
   IF(J.GT.0)THEN
    CALL IMOD_UTL_FILENAME(FNAME)
    FNAME=LINE(J:)
    INQUIRE(FILE=FNAME,EXIST=LEX)
    IF(.NOT.LEX)WRITE(LU,'(I10,1X,A)') K,TRIM(FNAME)
    EXIT
   ENDIF
  ENDIF
 ENDDO
ENDDO

CLOSE(IURUN)
CLOSE(LU)

CALL IMOD_UTL_PRINTTEXT('',3)
CALL IMOD_UTL_PRINTTEXT('Finished checking Run-file ...',3)
CALL EXIT(1)

END SUBROUTINE RF2MF_CHECKRUN

!###====================================================================
SUBROUTINE MODFLOW_IDF(LINE,FCT,IMP,ILAY,CONSTANTE,FNAME,IOS,IMODPCK,IPCK)
!###====================================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: IMODPCK,IPCK
CHARACTER(LEN=*),INTENT(INOUT) :: LINE
REAL,INTENT(OUT) :: CONSTANTE,FCT,IMP
INTEGER,INTENT(OUT) :: IOS,ILAY
CHARACTER(LEN=*),INTENT(OUT) :: FNAME
LOGICAL :: LEX

CALL IMOD_UTL_STRING(LINE)

IF(IMODPCK.EQ.0.AND.(IPCK.EQ.PCAP.OR.IPCK.EQ.PPWT))THEN
 READ(LINE,*,IOSTAT=IOS) FCT,IMP
 ILAY=1
ELSE
 READ(LINE,*,IOSTAT=IOS) ILAY,FCT,IMP
ENDIF
IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Error reading: ['//TRIM(LINE)//']',-3)
IF(ILAY.GT.MXNLAY)CALL IMOD_UTL_PRINTTEXT('Error reading ILAY='//TRIM(IMOD_UTL_ITOS(ILAY))//' that is larger than MXNLAY ('//TRIM(IMOD_UTL_ITOS(MXNLAY))//')',-3)

FNAME=IMOD_UTL_GETFNAME(LINE)

LEX=.TRUE.
IF(IMODPCK.EQ.0)THEN
 IF(MMOD(IPCK).EQ.0)LEX=.FALSE.
ELSE
 IF(MPCK(IPCK).EQ.0)LEX=.FALSE.
ENDIF
IF(.NOT.LEX)RETURN

LEX=.FALSE.
IF(ILAY.GE.1.AND.ILAY.LE.NLAY)LEX=.TRUE.
IF(ILAY.EQ.NLAY.AND.(IPCK.EQ.PVCW.OR.IPCK.EQ.PKVV))LEX=.FALSE.

CONSTANTE=IMOD_UTL_GETREAL(FNAME,IOS)
IF(IOS.EQ.0)THEN
 IF(LEX)THEN
  CALL IMOD_UTL_PRINTTEXT('Read Constant Value '//TRIM(IMOD_UTL_RTOS(CONSTANTE,'G',4)),3)
  CALL IMOD_UTL_PRINTTEXT(CHAR(9)//'* Modellayer: '//TRIM(IMOD_UTL_ITOS(ILAY))//'; Mult. Factor: '//TRIM(IMOD_UTL_RTOS(FCT,'G',4))// &
                          '; Addition: '//TRIM(IMOD_UTL_RTOS(IMP,'G',4)),3)
  CONSTANTE=CONSTANTE*FCT+IMP
  CALL IMOD_UTL_PRINTTEXT(CHAR(9)//'Constant Value becomes '//TRIM(IMOD_UTL_RTOS(CONSTANTE,'G',4)),3)
 ENDIF
ELSE
 CALL IMOD_UTL_FILENAME(FNAME)
 IF(LEX)THEN
  CALL IMOD_UTL_PRINTTEXT('Assigned '//TRIM(FNAME),3)
  CALL IMOD_UTL_PRINTTEXT(CHAR(9)//'* Modellayer: '//TRIM(IMOD_UTL_ITOS(ILAY))//'; Mult. Factor: '//TRIM(IMOD_UTL_RTOS(FCT,'G',4))// &
                          '; Addition: '//TRIM(IMOD_UTL_RTOS(IMP,'G',4)),3)
 ENDIF
ENDIF

!## check combination khv/kvv and top/bot
IF(IMODPCK.EQ.0)THEN
 IF(MMOD(PKHV).EQ.1)THEN
  IF(MMOD(PTOP).NE.1.OR.MMOD(PBOT).NE.1)CALL IMOD_UTL_PRINTTEXT('Horizontal K value needs usage TOP and BOT!',-3)
 ENDIF
 IF(MMOD(PKVV).EQ.1)THEN
  IF(MMOD(PTOP).NE.1.OR.MMOD(PBOT).NE.1)CALL IMOD_UTL_PRINTTEXT('Vertical K value needs usage TOP and BOT!',-3)
 ENDIF
ENDIF

IF(ILAY.LE.0)THEN
 IF(IMODPCK.EQ.0)THEN
  CALL IMOD_UTL_PRINTTEXT(TRIM(TXTMOD(IPCK))//' ilay less or equal zero!',-3)
 ELSE
  SELECT CASE (IPCK)
   CASE (PRCH)
    IF(ILAY.EQ.0)CALL IMOD_UTL_PRINTTEXT('Modellayer number equal to zero for '//TRIM(TXTPCK(IPCK))//' package!',-3)
   CASE DEFAULT
    IF(ILAY.EQ.0)THEN
     IF(MMOD(PTOP).NE.1.OR.MMOD(PBOT).NE.1)THEN
      CALL IMOD_UTL_PRINTTEXT('Usage of modellayers equal to zero only sustained in combination with both TOP and BOT!',-3)
     ENDIF
    ENDIF
  END SELECT
 ENDIF
ENDIF

IF(ILAY.LT.0)CALL IMOD_UTL_PRINTTEXT(CHAR(9)//'# Will be assigned to first active model layer',3)
IF(ILAY.EQ.0)CALL IMOD_UTL_PRINTTEXT(CHAR(9)//'# Layer will be computed by TOP and BOT data',3)

END SUBROUTINE MODFLOW_IDF

!#####=================================================================
SUBROUTINE RF2MF_WRITEBOX()
!#####=================================================================
IMPLICIT NONE
REAL(KIND=DP_KIND) :: AREA

CALL IMOD_UTL_PRINTTEXT('Given window of interest:' ,3)
WRITE(LINE,'(A,3(F15.3,A1))') ' Xmin - Xmax - Delta X (m):',USEBOX(1),' - ',USEBOX(3),' - ',USEBOX(3)-USEBOX(1)
CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),3)
WRITE(LINE,'(A,3(F15.3,A1))') ' Ymin - Ymax - Delta Y (m):',USEBOX(2),' - ',USEBOX(4),' - ',USEBOX(4)-USEBOX(2)
CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),3)
IF(NSCL.LE.2)WRITE(LINE,'(A,2(F15.3,A1))') ' Cellsize - Buffer (m)    :',SIMCSIZE,' - ',LAMBDA(1)
IF(NSCL.GE.3)WRITE(LINE,'(A,F10.2,A1,4F15.3)') ' Cellsize - Four Buffers (m)    :',SIMCSIZE,' - ',LAMBDA(1:4)
CALL IMOD_UTL_PRINTTEXT('Computed window of simulation:' ,3)
CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),3)
WRITE(LINE,'(A,3(F15.3,A1))') ' Xmin - Xmax - Delta X (m):',SIMBOX(1),' - ',SIMBOX(3),' - ',SIMBOX(3)-SIMBOX(1)
CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),3)
WRITE(LINE,'(A,3(F15.3,A1))') ' Ymin - Ymax - Delta Y (m):',SIMBOX(2),' - ',SIMBOX(4),' - ',SIMBOX(4)-SIMBOX(2)
CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),3)
AREA=(SIMBOX(3)-SIMBOX(1))*(SIMBOX(4)-SIMBOX(2))
WRITE(LINE,'(A,F15.3,A1)')    ' Total Area (km2)         :',AREA/1.0D6
CALL IMOD_UTL_PRINTTEXT(TRIM(LINE),3)

END SUBROUTINE RF2MF_WRITEBOX

!#####=================================================================
SUBROUTINE RF2MF_EXTENT(idf)
!#####=================================================================
IMPLICIT NONE
TYPE(IDFOBJ),INTENT(INOUT) :: IDF
CHARACTER(LEN=256) :: FNAME
INTEGER :: IOS 
REAL(KIND=8) :: XMAX,YMIN,XMIN,YMAX 
LOGICAL :: LEX

READ(IURUN,'(A256)') FNAME
FNAME = ADJUSTL(FNAME)
CALL IMOD_UTL_STRING(FNAME)
!## check if string corresponds to file
CALL IMOD_UTL_FILENAME(FNAME)
!## read without quotes
IF(FNAME(1:1).EQ.CHAR(34).OR.FNAME(1:1).EQ.CHAR(39))THEN ! double quote: CHAR(34); single quote: CHAR(39)
 READ(FNAME,*) FNAME
ENDIF
INQUIRE(FILE=FNAME,EXIST=LEX)

IF(LEX)THEN
 !## read without quotes
 IF(.NOT.IDFREAD(IDF,FNAME,0))CALL IMOD_UTL_PRINTTEXT('iMOD cannot read the file',2)
 XMIN=IDF%XMIN; XMAX=IDF%XMAX
 YMIN=IDF%YMIN; YMAX=IDF%YMAX
ELSE
 READ(FNAME,*,IOSTAT=IOS) XMIN,YMIN,XMAX,YMAX
 if(ios.ne.0)then
  CALL IMOD_UTL_PRINTTEXT('',3) 
  CALL IMOD_UTL_PRINTTEXT('Reading '//TRIM(FNAME),3)
  CALL IMOD_UTL_PRINTTEXT('from which no proper XMIN,YMIN,XMAX,YMAX can be read',3)
  CALL IMOD_UTL_PRINTTEXT('',2) 
 endif
 CALL IMOD_UTL_PRINTTEXT('',3); CALL IMOD_UTL_PRINTTEXT('Using XMIN,YMIN,XMAX,YMAX as entered',3)
 CALL IMOD_UTL_PRINTTEXT(' '//TRIM(FNAME),0)
ENDIF

!## take dimensions of idf/xy1,xy2
IF(NSCL.EQ.0)THEN

 IF(.NOT.LEX)CALL IMOD_UTL_PRINTTEXT('Cannot combine NSCL.eq.0 and reading x1,y1,x2,y2 for BNDFILE',2)
 
 CALL IMOD_UTL_PRINTTEXT('',3); CALL IMOD_UTL_PRINTTEXT('Using grid dimensions (read) specified by:',3)
 CALL IMOD_UTL_PRINTTEXT(' '//TRIM(FNAME),0)

 USEBOX(1)=idf%XMIN;   USEBOX(2)=idf%YMIN
 USEBOX(3)=idf%XMAX;   USEBOX(4)=idf%YMAX
 SIMBOX   =USEBOX; SIMCSIZE =idf%dx !CS
 !## change to nscl=1 for equidistantial cells
 IF(idf%IEQ.EQ.0)THEN; NSCL=1; LQD=.TRUE.; ENDIF
 XMIN=IDF%XMIN
 XMAX=IDF%XMAX
 YMIN=IDF%YMIN
 YMAX=IDF%YMAX
 NCOL=IDF%NCOL
 NROW=IDF%NROW
 
ELSE

 !## evaluate simulationbox first
 IF(XMIN.GT.USEBOX(1).OR.XMAX.LT.USEBOX(3).OR. &
    YMIN.GT.USEBOX(2).OR.YMAX.LT.USEBOX(4))THEN
  CALL IMOD_UTL_PRINTTEXT('',3)
  CALL IMOD_UTL_PRINTTEXT('Simulation window:',3)
  CALL IMOD_UTL_PRINTTEXT('',3)
  CALL IMOD_UTL_PRINTTEXT('xmin/xmax: '//TRIM(IMOD_UTL_DTOS(USEBOX(1),'F',2))//','//TRIM(IMOD_UTL_DTOS(USEBOX(3),'F',2)),3)
  CALL IMOD_UTL_PRINTTEXT('ymin/ymax: '//TRIM(IMOD_UTL_DTOS(USEBOX(2),'F',2))//','//TRIM(IMOD_UTL_DTOS(USEBOX(4),'F',2)),3)
  CALL IMOD_UTL_PRINTTEXT('',3)
  CALL IMOD_UTL_PRINTTEXT('is outside model domain!',3)
  CALL IMOD_UTL_PRINTTEXT('',3)
  CALL IMOD_UTL_PRINTTEXT('xmin/xmax: '//TRIM(IMOD_UTL_DTOS(XMIN,'F',2))//','//TRIM(IMOD_UTL_DTOS(XMAX,'F',2)),3)
  CALL IMOD_UTL_PRINTTEXT('ymin/ymax: '//TRIM(IMOD_UTL_DTOS(YMIN,'F',2))//','//TRIM(IMOD_UTL_DTOS(YMAX,'F',2)),3)
  CALL IMOD_UTL_PRINTTEXT('',-3)
 ENDIF

 !## correct use box first!
 USEBOX(1) =MAX(XMIN,USEBOX(1))
 USEBOX(2) =MAX(YMIN,USEBOX(2))
 USEBOX(3) =MIN(XMAX,USEBOX(3))
 USEBOX(4) =MIN(YMAX,USEBOX(4))

 SIMBOX=USEBOX

 SIMBOX(1) =MAX(XMIN,SIMBOX(1)-LAMBDA(1))
 SIMBOX(2) =MAX(YMIN,SIMBOX(2)-LAMBDA(2))
 SIMBOX(3) =MIN(XMAX,SIMBOX(3)+LAMBDA(3))
 SIMBOX(4) =MIN(YMAX,SIMBOX(4)+LAMBDA(4))

 CALL RF2MF_SNAPTOGRID(SIMBOX(1),SIMBOX(3),SIMBOX(2),SIMBOX(4),SIMCSIZE,NCOL,NROW)

 !## no IDF with raster definition
 IDF%IEQ=0
 IDF%DX=SIMCSIZE
 IDF%DY=SIMCSIZE
 IDF%NCOL=NCOL
 IDF%NROW=NROW
 IDF%XMIN = SIMBOX(1)
 IDF%YMIN = SIMBOX(2)
 IDF%XMAX = SIMBOX(3)
 IDF%YMAX = SIMBOX(4)

END IF   

IFULL=0
IF(XMIN.EQ.SIMBOX(1))IFULL(1)=1; IF(XMAX.EQ.SIMBOX(3))IFULL(3)=1
IF(YMIN.EQ.SIMBOX(2))IFULL(2)=1; IF(YMAX.EQ.SIMBOX(4))IFULL(4)=1

if (ifull(1).eq.0) met%kws(imet_ibound_fixed_west)%type = imetc
if (ifull(3).eq.0) met%kws(imet_ibound_fixed_east)%type = imetc
if (ifull(2).eq.0) met%kws(imet_ibound_fixed_south)%type = imetc
if (ifull(4).eq.0) met%kws(imet_ibound_fixed_north)%type = imetc

!## determine cellsizes delr/delc (in upscale mode)
CALL RF2MF_SCALE1DELRC(IDF)

CALL IMOD_UTL_PRINTTEXT('',3)
CALL IMOD_UTL_PRINTTEXT('Solving system (ncol x nrow x nlay): '// &
       TRIM(IMOD_UTL_ITOS(NCOL))//' x '//TRIM(IMOD_UTL_ITOS(NROW))//' x '//TRIM(IMOD_UTL_ITOS(NLAY)),3)
CALL IMOD_UTL_PRINTTEXT('',3)
CALL RF2MF_WRITEBOX()
IF(NSCL.GE.3)THEN
 CALL IMOD_UTL_PRINTTEXT('',-1)
 CALL IMOD_UTL_PRINTTEXT('Solving system (ncol x nrow x nlay): '// &
        TRIM(IMOD_UTL_ITOS(NCOL))//' x '//TRIM(IMOD_UTL_ITOS(NROW))//' x '//TRIM(IMOD_UTL_ITOS(NLAY)),-1)
 CALL IMOD_UTL_PRINTTEXT('',-1)
ENDIF

END SUBROUTINE RF2MF_EXTENT

!###======================================================================
SUBROUTINE RF2MF_SNAPTOGRID(MINX,MAXX,MINY,MAXY,CS,NCOL,NROW)
!###======================================================================
USE IMOD_UTL, ONLY : IMOD_UTL_EQUALS_DOUBLE
REAL(KIND=8),INTENT(INOUT) :: MINX,MAXX,MINY,MAXY
REAL(KIND=8),INTENT(IN)	:: CS
INTEGER,INTENT(OUT) :: NCOL,NROW
REAL(KIND=8) :: D

NCOL=(MAXX-MINX)/CS
NROW=(MAXY-MINY)/CS

IF(.NOT.IMOD_UTL_EQUALS_DOUBLE(MINX+REAL(NCOL,8)*CS,MAXX))THEN
 MAXX=MINX+NCOL*CS
ENDIF

IF(.NOT.IMOD_UTL_EQUALS_DOUBLE(MINY+REAL(NROW,8)*CS,MAXY))THEN
 MAXY=MINY+NROW*CS
ENDIF

RETURN
END SUBROUTINE RF2MF_SNAPTOGRID

!###====================================================================
SUBROUTINE RF2MF_SCALE1DELRC(IDF)
!###====================================================================
IMPLICIT NONE
TYPE(IDFOBJ),INTENT(INOUT) :: IDF
REAL,PARAMETER :: INC=1.0     !## minimal scaling in interest
DOUBLE PRECISION,PARAMETER :: FINCR=0.02D0    !0.02d0
REAL,PARAMETER ::    POWR     =0.3     !     !1.3
INTEGER :: NOMAXCELL   !## maximal # cells in the end
INTEGER,PARAMETER :: NOMINCELL=1       !## minimal # cells in the centre
LOGICAL,PARAMETER :: LCLIP    =.TRUE.  !
INTEGER :: I,ORGNROW,ORGNCOL,IC1,IC2,IR1,IR2,OC1,OC2,OR1,OR2,IREC
INTEGER,ALLOCATABLE,DIMENSION(:) :: PDELR,PDELC

IF(ALLOCATED(DELR))DEALLOCATE(DELR)
IF(ALLOCATED(DELC))DEALLOCATE(DELC)

IF(NSCL.EQ.1.OR.NSCL.EQ.3)THEN

!--- for metaswap ---
 ALLOCATE(DELR(0:NCOL),DELC(0:NROW))

 DELR(0)=SIMBOX(1)
 DELC(0)=SIMBOX(4)
 DO I=1,NCOL
  DELR(I)=SIMBOX(1)+REAL(I)*SIMCSIZE
 ENDDO
 DO I=1,NROW
  DELC(I)=SIMBOX(4)-REAL(I)*SIMCSIZE
 ENDDO

 dis%nrow = NROW
 dis%ncol = NCOL
 call AllocDis(ialloc)
 dis%delr=simcsize
 dis%delc=simcsize

ELSE

 !## take dimensions of idf
 IF(NSCL.EQ.0)THEN

  NCOL=IDF%NCOL
  NROW=IDF%NROW
  
  dis%nrow = NROW
  dis%ncol = NCOL
  call AllocDis(ialloc)

  !--- for metaswap ---
  ALLOCATE(DELR(0:NCOL),DELC(0:NROW))

  IF(IDF%IEQ.EQ.0)THEN
      
   DELR(0)=IDF%XMIN
   DO I=1,NCOL
    DELR(I)=DELR(I-1)+IDF%DX
   ENDDO
   DELC(0)=IDF%YMAX
   DO I=1,NROW
    DELC(I)=DELC(I-1)-IDF%DY
   ENDDO
   
  ELSE
  
   DO I=0,NCOL; DELR(I)=IDF%SX(I); ENDDO
   DO I=0,NROW; DELC(I)=IDF%SY(I); ENDDO
  
  ENDIF
  
  !--- for metaswap ---

  IF(IDF%IEQ.eq.0)THEN
   dis%delr(i)=IDF%DX
   dis%delr(i)=IDF%DY
  ELSE
   DO I=1,IDF%NCOL; dis%delr(i)=IDF%SX(I)-IDF%SX(I-1); ENDDO
   DO I=1,IDF%Nrow; dis%delc(i)=IDF%Sy(I-1)-IDF%Sy(I); ENDDO
  ENDIF

 ELSE

  NOMAXCELL=INT(MAXSIMCSIZE/SIMCSIZE)

  !## find mid icol
  IC1=INT((USEBOX(1)-SIMBOX(1))/SIMCSIZE)+1
  IC2=INT((USEBOX(3)-SIMBOX(1))/SIMCSIZE)+1
  IR1=INT((SIMBOX(4)-USEBOX(4))/SIMCSIZE)+1
  IR2=INT((SIMBOX(4)-USEBOX(2))/SIMCSIZE)+1

  ORGNCOL=NCOL
  ORGNROW=NROW

  ALLOCATE(PDELR(NCOL),PDELC(NROW))
  CALL MODELLHS1(PDELR,ORGNCOL,NCOL,IC1,IC2,OC1,OC2,INC,FINCR,POWR,NOMINCELL,NOMAXCELL,LCLIP)
  CALL MODELLHS1(PDELC,ORGNROW,NROW,IR1,IR2,OR1,OR2,INC,FINCR,POWR,NOMINCELL,NOMAXCELL,LCLIP)

  dis%nrow = NROW
  dis%ncol = NCOL
  call AllocDis(ialloc)
  
  IDF%NCOL=NCOL
  IDF%NROW=NROW
  
  CALL IMOD_UTL_PRINTTEXT('',3)
  CALL IMOD_UTL_PRINTTEXT('Scaling Results along COLUMN-direction:',3)
  CALL IMOD_UTL_PRINTTEXT('',3)
  CALL RF2MF_SCALE1RESULTS(PDELR,dis%delr,NCOL,ORGNCOL)

  CALL IMOD_UTL_PRINTTEXT('',3)
  CALL IMOD_UTL_PRINTTEXT('Scaling Results along ROW-direction:',3)
  CALL IMOD_UTL_PRINTTEXT('',3)
  CALL RF2MF_SCALE1RESULTS(PDELC,dis%delc,NROW,ORGNROW)

  !--- for metaswap ---
  ALLOCATE(DELR(0:NCOL),DELC(0:NROW))

  IDF%IEQ=0
  IF(MINVAL(DIS%DELR).NE.MAXVAL(DIS%DELR).OR. &
     MINVAL(DIS%DELC).NE.MAXVAL(DIS%DELC))IDF%IEQ=1
  
  DELR(0)=IDF%XMIN
  DO I=1,NCOL
   DELR(I)=DELR(I-1)+dis%delr(i)
  ENDDO
  DELC(0)=IDF%YMAX
  DO I=1,NROW
   DELC(I)=DELC(I-1)-dis%delc(i)
  ENDDO

  !## create non-equidistantial network - if needed
  IF(IDF%IEQ.EQ.1)THEN  
   ALLOCATE(IDF%SX(0:IDF%NCOL),IDF%SY(0:IDF%NROW))
   DO I=0,IDF%NCOL; IDF%SX(I)=DELR(I); ENDDO
   DO I=0,IDF%NROW; IDF%SY(I)=DELC(I); ENDDO
  ENDIF
 
 ENDIF
ENDIF

END SUBROUTINE RF2MF_SCALE1DELRC

!###====================================================================
SUBROUTINE RF2MF_SCALE1RESULTS(IX,DX,NX,NXORG)
!###====================================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: NX,NXORG
REAL,INTENT(OUT),DIMENSION(NX) :: DX
INTEGER,INTENT(IN),DIMENSION(NXORG) :: IX
INTEGER :: I,J,K

J=1
K=1
DO I=2,NXORG
 IF(IX(I).NE.IX(I-1))THEN
  DX(K)=SIMCSIZE*REAL(J)
  K=K+1
  J=1
 ELSE
  J=J+1
 ENDIF
END DO
DX(K)=SIMCSIZE*REAL(J)

I=1
DO K=2,NX
 IF(DX(K).NE.DX(K-1))THEN
  WRITE(LINE,'(I5,2(A3,F10.2))') I,' x ',DX(K-1),' = ',REAL(I)*DX(K-1)
  CALL IMOD_UTL_PRINTTEXT(' Cellsizes: '//TRIM(LINE),3)
  I=1
 ELSE
  I=I+1
 ENDIF
ENDDO
WRITE(LINE,'(I5,2(A3,F10.2))') I,' x ',DX(K-1),' = ',REAL(I)*DX(K-1)
CALL IMOD_UTL_PRINTTEXT(' Cellsizes: '//TRIM(LINE),3)

END SUBROUTINE RF2MF_SCALE1RESULTS

!#####=================================================================
SUBROUTINE RF2MF_SIMGRO()
!#####=================================================================
IMPLICIT NONE
INTEGER :: I

IF(MMOD(PCAP).EQ.1)THEN !.AND.IMODFLOW.EQ.0)THEN
! CALL METASWAP_MAIN()
ELSE
!#skip input from runfile
 DO I=1,NLINES; READ(IURUN,*); END DO
ENDIF

END SUBROUTINE RF2MF_SIMGRO

END MODULE  MOD_RF2MF_MAIN

!#####=================================================================
subroutine rf2mf_prg(lrunfile,lipest,lidfmerge,record,usemetaswap,submstr,nsub,nsubmax,wd,imodusebox,rfroot)
!#####=================================================================

! modules
use mod_rf2mf_main, only: rf2mf
use rf2mf_module, only: root, starttime, lmfroot, mfroot
USE imod_utl, only: imod_utl_s_cap, imod_utl_closeunits
use mod_rf2mf, only: usebox

implicit none

logical, intent(out) :: lrunfile
integer, intent(inout) :: nsub
integer, intent(in) :: nsubmax
character(len=*), intent(inout) :: record
character(len=50), dimension(nsubmax), intent(out) :: submstr
logical, intent(out) :: usemetaswap
character(len=*), intent(out) :: wd
logical, intent(inout) :: lipest
logical, intent(inout) :: lidfmerge
real, dimension(4), intent(out) :: imodusebox
character(len=*), intent(out) :: rfroot 

logical :: lnamfile, lrfopt
character(len=256) :: dxcfile, runfile, namfile, ext, cdum
integer :: i, n, ivcl, iarg, ios, lun, osd_open2
integer, dimension(2) :: rfopt
character(len=256) :: cwrk
logical :: pks7mpimasterwrite ! PKS
character(len=1) :: c1

call timing_tic('MODFLOW','RF')

lipest = .false.
lmfroot = .false.
if (len_trim(wd).gt.0) then
   lmfroot = .true.
   mfroot  = trim(wd)
else
   wd = ''
end if
root%modeldir = ''
dxcfile = ''; runfile = ''; namfile = ''

lrunfile = .false.; lnamfile = .false.

! define virtual commandline
call cfn_vcl_set(record,ivcl)

! check if user has specified -wd(workdirectory)
call cfn_vcl_fndc(ivcl,iarg,'-wd',.true.,root%modeldir,1)

! check if user has specified -run(file)
call cfn_vcl_fndc(ivcl,iarg,'-run*file',.true.,runfile,1)
if (iarg.gt.0) then
   lrunfile = .true.
   c1 = runfile(1:1)
   if (c1.eq.char(34).or.c1.eq.char(39)) then
      cwrk = record 
      i = index(cwrk,trim(runfile))
      cwrk = cwrk(i+1:)
      i = index(cwrk,c1)
      write(runfile,'(a)') cwrk(1:i-1)
   end if   
end if

! check if user has specified -runfileopt
lrfopt = .false.; rfopt = -1
call cfn_vcl_fndi(ivcl,iarg,'-rfopt',.true.,rfopt,2)
if (iarg.gt.0) lrfopt = .true.

! check if user has specified -nam(file)
call cfn_vcl_fndc(ivcl,iarg,'-nam*file',.true.,namfile,1)
if (iarg.gt.0) then
   lnamfile = .true.
   c1 = namfile(1:1) 
   if (c1.eq.char(34).or.c1.eq.char(39)) then
      cwrk = record 
      i = index(cwrk,trim(namfile))
      cwrk = cwrk(i+1:)
      i = index(cwrk,c1)
      write(namfile,'(a)') cwrk(1:i-1)
   end if   
end if

call cfn_vcl_fndc(ivcl,iarg,'-dxc*file',.true.,dxcfile,1)

if (lnamfile) then
   wd = ''
   record = trim(namfile)
   i = index(namfile,'\',.true.)
   if (i.gt.0) wd = namfile(1:i-1)
   i = index(namfile,'/',.true.)
   if (i.gt.0) wd = namfile(1:i-1)
   if (len_trim(wd).eq.0) then
       wd = '.\'
   end if
   return
else
   if (pks7mpimasterwrite()) write(*,*) 'running with imod run-file.'
   if (lrfopt) then
      call rf2mf(trim(runfile),dxcfile,submstr,nsub,nsubmax,lipest,lidfmerge,rfroot,rfopt(1),rfopt(2))
   else
      call rf2mf(trim(runfile),dxcfile,submstr,nsub,nsubmax,lipest,lidfmerge,rfroot)
   end if
   if(.not.lmfroot) wd = root%resultdir
   usemetaswap = .false.
   if (len_trim(dxcfile).gt.0) usemetaswap = .true.

!... change dir
   record = trim(root%modelname)//'.nam '//trim(record)

! check if user has specified -starttime
   call cfn_vcl_fndc(ivcl,iarg,'-start*time',.true.,cdum,1)
   if (iarg.le.0) then
      record = trim(record)//' -starttime '//starttime
   end if

    ! close all units
   call imod_utl_closeunits()
endif

imodusebox = usebox

call timing_toc('MODFLOW','RF')

end subroutine rf2mf_prg





