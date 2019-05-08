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

      !> Module to convert runfile --> modflow 2005
      module rf2mf_module

      use imod_utl, only: imod_utl_rel_to_abs

      implicit none

      private

      real,parameter :: hnoflo=huge(1.0)
      
!...     parameters
      integer, parameter :: ilst = 1
      integer, parameter :: idis = 2
      integer, parameter :: ibas = 3
      integer, parameter :: ibcf = 4
      integer, parameter :: ioc  = 5
      integer, parameter :: imet = 6
      integer, parameter :: iriv = 7
      integer, parameter :: idrn = 8
      integer, parameter :: ighb = 9
      integer, parameter :: iwel = 10
      integer, parameter :: iani = 11
      integer, parameter :: ihfb = 12
      integer, parameter :: ipcg = 13
      integer, parameter :: ipks = 14
      integer, parameter :: irch = 15
      integer, parameter :: ichd = 16
      integer, parameter :: ievt = 17
      integer, parameter :: iscr = 18
      integer, parameter :: ipwt = 19
      integer, parameter :: idxc = 20

      integer, parameter :: npck = idxc

      character(len=4), dimension(npck) :: pckftype
      data pckftype/&
      'list',&
      'dis ',&
      'bas6 ',&
      'bcf6',&
      'oc  ',&
      'met ',&
      'riv ',&
      'drn ',&
      'ghb ',&
      'wel ',&
      'ani ',&
      'hfb6',&
      'pcg ',&
      'pks ',&
      'rch ',&
      'chd ',&
      'evt ',&
      'scr ',&
      'pwt ',&
      'dxc '/

      integer, parameter :: ihead    = 1
      integer, parameter :: ibcfflux = 2
      integer, parameter :: irivflux = 3
      integer, parameter :: idrnflux = 4
      integer, parameter :: ighbflux = 5
      integer, parameter :: iwelflux = 6
      integer, parameter :: irchflux = 7
      integer, parameter :: ievtflux = 8
      integer, parameter :: iscrflux = 9
      integer, parameter, public :: idxcflux = 10

      integer, parameter :: ndat = idxcflux

      character(len=15), dimension(ndat) :: datftype
      data datftype/&
      'data(binaryidf)',&
      'data(binaryidf)',&
      'data(binaryidf)',&
      'data(binaryidf)',&
      'data(binaryidf)',&
      'data(binaryidf)',&
      'data(binaryidf)',&
      'data(binaryidf)',&
      'data(binaryidf)',&
      'data(binaryidf)'/

      integer, public, parameter :: iaru      = 0   ! not defined
      integer, public, parameter :: iarr      = 1   ! real
      integer, public, parameter :: iari      = 2   ! integer
      integer, parameter :: maxlen    = 256 ! maximum string length
      integer, public, parameter :: maxsubsys = 500 ! maximum number of subsystems (wells can be much)
      integer, public, parameter :: maxgen    = 10  ! maximum number of gen files
      integer, public, parameter :: maxcol    = 10  ! maximum number of stress package columns
      integer, public, parameter :: ialloc    = 1   ! allocate
      integer, public, parameter :: idealloc  = 2   ! de-allocate

      ! upscaling
      integer, parameter, public :: iusclnodata    = -1 ! no data
      integer, parameter, public :: iusclspec      =  1 ! special/ibound
      integer, parameter, public :: iusclarith     =  2 ! arithmetic
      integer, parameter, public :: iusclgeom      =  3 ! geometric
      integer, parameter, public :: iusclsumq      =  4 ! sum(Q)
      integer, parameter, public :: iusclsumcdr    =  5 ! sum(cond)*ratio
      integer, parameter, public :: iusclinvc      =  6 ! inverse (c)
      integer, parameter, public :: iusclmostfr    =  7 ! most freq. occ
      integer, parameter, public :: iusclsuminvcvr =  8 ! sum(1/c)*ratio
      integer, parameter, public :: iusclperc      =  9 ! percentile
      integer, parameter, public :: iusclarithnd   =  11 ! average including locations with nodata (rch/evt)

      ! downscaling
      integer, parameter, public :: idsclnodata    = -1 ! no data
      integer, parameter, public :: idsclnointp    =  0 ! no interpolation
      integer, parameter, public :: idsclintp      =  1 ! interpolation

      logical, save, public :: lmfroot = .false.
      character(len=maxlen), save, public :: mfroot = ''

!...     globals
      integer, public, save :: nlay, nrow, ncol
      integer, public, save :: nper
      integer, public, save :: dxciu
      logical, public, save :: trflag = .false.

      type tRoot
         character(len=50) :: submodel = ''
         character(len=maxlen) :: modeldir  = '..\modflow2005\'
         character(len=maxlen) :: modelname = 'modelname'
         character(len=maxlen) :: resultdir = '' ! filled by wrapper
         character(len=maxlen) :: runfileroot = '' ! filled by wrapper
      end type tRoot
      type(tRoot), public, save :: root

!...     general types
      type, public :: tArrayRead
         integer               :: type = iarr
         character(len=maxlen) :: keyword = 'open/close'
         character(len=maxlen) :: fname = ''
         character(len=maxlen) :: fmtin = '(free)'
         real(KIND=8)          :: cnstnt = 0.0d0
         real(KIND=8)          :: fct = 1.0d0
         real(KIND=8)          :: imp = 0.0d0
         real(KIND=8)          :: pow = 0.0d0
         character(len=10)     :: oper = ''
         integer               :: iprn = -1
         integer               :: ilay = 0
         integer               :: iuscl = -1
         integer               :: idscl = -1
      end type tArrayRead
      type tSubsys
         integer :: ilay   = -1
         integer :: isub   = -1
         real    :: factor = -1.
         real    :: imp    = -1.
         logical :: lisg   = .false.
         logical :: lolf   = .false.
         logical :: lriv   = .false.
         logical :: ldrn   = .false.
         logical :: active = .true.
!         type(tArrayRead), dimension(maxcol) :: data
         type(tArrayRead), dimension(:), pointer :: data
      end type tSubsys

      type tGcd
         integer :: ncolumns = -1
         integer :: nsubsys  = 0
!         type(tSubsys), dimension(maxsubsys) :: subsys
         type(tSubsys), dimension(:), pointer :: subsys => null()
      end type tGcd
      type tLcd
         integer :: ngen = 0
!         type(tSubsys), dimension(maxgen) :: gen
         type(tSubsys), dimension(:), pointer :: gen => null()
      end type tLcd
      type tSpGcdLcd
         logical :: reuse = .true.
         logical :: lriv = .false.
         logical :: lisg = .false.
         logical :: ldrn = .false.
         logical :: lolf = .false.
         integer :: np = 1
         logical :: usegcd = .true.
         logical :: uselcd = .false.
         type(tGcd) :: gcd
         type(tLcd) :: lcd
      end type tSpGcdLcd

!=======================================================================
      type tPackage
         logical                        :: active = .false.
         character(len=maxlen)          :: ftype
         integer                        :: nunit
         character(len=maxlen)          :: fname = ''
         character(len=maxlen)          :: fnamenoroot = ''
         integer                        :: cbnlay = 0
         integer, dimension(:), pointer :: cblay => null()
      end type tPackage
      type tNam
         character(len=maxlen) :: text = 'Name file'
         character(len=maxlen) :: fname
         type(tPackage), dimension(npck) :: package
         type(tPackage), dimension(ndat) :: data
      end type tNam
      type(tNam), public, save :: nam

!.......................................................................
      type tSp
         real             :: perlen = -1
         integer          :: nstp   = 1
         real             :: tsmult = 1.0
         character(len=2) :: SsTr   = ''
         logical          :: writeoc = .true.
      end type tSp
      type tDis
         character(len=maxlen) :: text = 'Discretization file'
         integer :: nlay = -1
         integer :: nrow = -1
         integer :: ncol = -1
         integer :: nper = -1
         integer :: itmuni = 4 ! days
         integer :: lenuni = 2 ! meters
         integer, dimension(:), pointer :: laycbd => null()
         real, dimension(:), pointer :: delr => null()
         real, dimension(:), pointer :: delc => null()
         logical :: settop = .false.
         logical :: setbot = .false.
         type(tArrayRead), dimension(:), pointer :: aquifertop => null()
         type(tArrayRead), dimension(:), pointer :: aquiferbot => null()
         type(tSp), dimension(:), pointer :: sp => null()
      end type tDis
      type(tDis), public, save :: dis

!.......................................................................
      type tBas
         character(len=maxlen) :: text = 'Basic Package file'
         character(len=maxlen) :: options = 'free'
         type(tArrayRead), dimension(:), pointer :: ibound => null()
         real :: hnoflo = hnoflo !huge(1.0) !-999.99
         type(tArrayRead), dimension(:), pointer :: strt => null()
      end type tBas
      type(tBas), public, save :: bas

!.......................................................................
      type tBcf
         character(len=maxlen) :: text   = 'Block-Centered Flow Package file'
         integer                        :: cbnlay = 0
         integer, dimension(:), pointer :: cblay => null()
         real                           :: hdry   = huge(1.0) !-9999.
         integer                        :: iwdflg = 0
         real                           :: wetfct = 0.
         integer                        :: iwetit = 0
         integer                        :: ihdwet = 0
         integer, dimension(:), pointer :: ltype => null()
         type(tArrayRead) :: trpy
         type(tArrayRead), dimension(:), pointer :: sf1 => null()
         type(tArrayRead), dimension(:), pointer :: tran => null()
         type(tArrayRead), dimension(:), pointer :: hhy => null()
         type(tArrayRead), dimension(:), pointer :: vhy => null()
         type(tArrayRead), dimension(:), pointer :: kva => null()
         type(tArrayRead), dimension(:), pointer :: vcont => null()
         type(tArrayRead), dimension(:), pointer :: sf2 => null()
         type(tArrayRead), dimension(:), pointer :: wetdry => null()
         logical :: llpf=.false.
         integer :: iminkd = 0
         integer :: iminc  = 0
         real :: minkd
         real :: minc
      end type tBcf
      type(tBcf), public, save :: bcf

!.......................................................................
      type tPwt
         character(len=maxlen) :: text = 'Perched Water Table Package file'
         type(tArrayRead), pointer :: ilay  => null()
         type(tArrayRead), pointer :: sc2   => null()
         type(tArrayRead), pointer :: bot   => null()
         type(tArrayRead), pointer :: top2  => null()
         type(tArrayRead), pointer :: thkaf => null()
         type(tArrayRead), pointer :: vcont => null()
      end type tPwt
      type(tPwt), public, save :: pwt

!.......................................................................
      type tOc
         character(len=maxlen) :: text   = 'Output Control Option'
         integer                        :: cbnlay = 0 ! heads
         integer, dimension(:), pointer :: cblay => null() ! heads
         integer                        :: ihedun
      end type tOc
      type(tOc), public :: oc

!.......................................................................
      integer, public, parameter :: imet_coord_xll          = 1
      integer, public, parameter :: imet_coord_yll          = 2
      integer, public, parameter :: imet_coord_xll_nb       = 3
      integer, public, parameter :: imet_coord_yll_nb       = 4
      integer, public, parameter :: imet_coord_xur_nb       = 5
      integer, public, parameter :: imet_coord_yur_nb       = 6
      integer, public, parameter :: imet_starttime          = 7
      integer, public, parameter :: imet_resultdir          = 8
      integer, public, parameter :: imet_ibound_fixed_west  = 9
      integer, public, parameter :: imet_ibound_fixed_east  = 10
      integer, public, parameter :: imet_ibound_fixed_north = 11
      integer, public, parameter :: imet_ibound_fixed_south = 12
      integer, public, parameter :: imet_save_no_buf        = 13
      integer, public, parameter :: imet_write_debug_idf    = 14
      integer, public, parameter :: imet_idate_save         = 15
      integer, public, parameter :: imet_savedouble         = 16

      integer, parameter :: nmetkws = imet_savedouble

      character(len=18), dimension(nmetkws) :: metkws
!...   12345678901234567890123
      data metkws/&
      'coord_xll         ',&
      'coord_yll         ',&
      'coord_xll_nb      ',&
      'coord_yll_nb      ',&
      'coord_xur_nb      ',&
      'coord_yur_nb      ',&
      'starttime         ',&
      'resultdir         ',&
      'ibound_fixed_west ',&
      'ibound_fixed_east ',&
      'ibound_fixed_north',&
      'ibound_fixed_south',&
      'save_no_buf       ',&
      'write_debug_idf   ',&     
      'idate_save        ',&
      'savedouble        '/

      integer, public, parameter :: imetu = 1
      integer, public, parameter :: imeti = 2
      integer, public, parameter :: imetr = 3
      integer, public, parameter :: imetc = 4
      integer, public, parameter :: imett = 5

      type tTime
         integer :: year   = -1
         integer :: month  = -1
         integer :: day    = -1
         integer :: hour   = -1
         integer :: minute = -1
         integer :: second = -1
!...                                      123456
         character(len=4) :: kw_year   = 'year'
         character(len=5) :: kw_month  = 'month'
         character(len=3) :: kw_day    = 'day'
         character(len=4) :: kw_hour   = 'hour'
         character(len=6) :: kw_minute = 'minute'
         character(len=6) :: kw_second = 'second'
      end type tTime
      type tKw
         character(len=maxlen) :: keyword
         integer :: type = imetu
         integer :: ival
         real    :: rval
         character(len=maxlen) :: cval = ''
         type(tTime) :: time
      end type tKw
      type tMet
!         integer, save :: nmult
         type(tKw), dimension(nmetkws) :: kws
      end type tMet
      type(tMet), public, save :: met

!.......................................................................
      type tRiv
         character(len=maxlen) :: text = 'River Package file'
         integer                        :: type = iriv
         integer                        :: mxactr = 1
         integer                        :: cbnlay = 0
         integer, dimension(:), pointer :: cblay => null()
         logical                        :: rfact = .true.
         logical                        :: rsubsys = .true.
         logical                        :: rconc = .false.
         logical                        :: ifvdl = .false.
         logical                        :: sft = .false.
         type(tArrayRead), pointer      :: sft1 => null()
         type(tArrayRead), pointer      :: sft2 => null()
         type(tSpGcdLcd), dimension(:), pointer :: sp => null()
      end type tRiv
      type(tRiv), public, save :: riv

!.......................................................................
      type tDrn
         character(len=maxlen) :: text = 'Drain Package file'
         integer                        :: type = idrn
         integer                        :: mxactr = 1
         integer                        :: cbnlay = 0
         integer, dimension(:), pointer :: cblay => null()
         logical                        :: dsubsys = .true.
         logical                        :: iconchk = .false.
         logical                        :: dconc = .false.
         type(tSpGcdLcd), dimension(:), pointer :: sp => null()
      end type tDrn
      type(tDrn), public, save :: drn

!.......................................................................
      type tGhb
         character(len=maxlen) :: text = 'General-Head Boundary Package file'
         integer                        :: type = ighb
         integer                        :: mxactr = 1
         integer                        :: cbnlay = 0
         integer, dimension(:), pointer :: cblay => null()
         logical                        :: gconc = .false.
         type(tSpGcdLcd), dimension(:), pointer :: sp => null()
      end type tGhb
      type(tGhb), public, save :: ghb

!.......................................................................
      type tWel
         character(len=maxlen) :: text = 'Well Package file'
         integer                        :: type = iwel
         integer                        :: mxactr = 1
         logical                        :: wsubsys = .false. !## needed for ipest - but not working right now
         integer                        :: cbnlay = 0
         integer, dimension(:), pointer :: cblay => null()
         type(tSpGcdLcd), dimension(:), pointer :: sp => null()
      end type tWel
      type(tWel), public, save :: wel

!.......................................................................
      type tChd
         character(len=maxlen) :: text = 'Time-Variant Specified Head package file'
         integer :: type = ichd
         integer :: mxactc = 1
         logical :: negbnd = .true.
         type(tSpGcdLcd), dimension(:), pointer :: sp => null()
      end type tChd
      type(tchd), public, save :: chd

!.......................................................................
      type tAni
         character(len=maxlen) :: text = 'Anisoptropy Package file'
         type(tArrayRead), dimension(:), pointer :: factor => null()
         type(tArrayRead), dimension(:), pointer :: angle => null()
      end type tAni
      type(tAni), public, save :: ani

!.......................................................................
      type tScr
         integer                        :: cbnlay = 0
         integer, dimension(:), pointer :: cblay => null()
         character(len=maxlen) :: text = 'Subsidence-Creep Package file'
         type(tArrayRead), dimension(:), pointer :: pcsoff => null()
         type(tArrayRead), dimension(:), pointer :: pcs => null()
         type(tArrayRead), dimension(:), pointer :: gl0 => null()
         type(tArrayRead), dimension(:), pointer :: sgm => null()
         type(tArrayRead), dimension(:), pointer :: sgs => null()
         type(tArrayRead), dimension(:), pointer :: thick => null()
         type(tArrayRead), dimension(:), pointer :: rrisoa => null()
         type(tArrayRead), dimension(:), pointer :: rrisob => null()
         type(tArrayRead), dimension(:), pointer :: caisoc => null()
         type(tArrayRead), dimension(:), pointer :: void => null()
         type(tArrayRead), dimension(:), pointer :: sub => null()
         integer :: istpc=0
         integer :: nsys=0
         integer :: ithk=0
         integer :: ivoid=0
         integer :: imethod=2
         real :: alpha=0.0
      end type tScr
      type(tScr), public, save :: scr

!.......................................................................
      type tHfb
         character(len=maxlen) :: text = 'Horizontal Flow Barrier Package file'
         integer :: type = ihfb
         integer :: nphfb  = 0
         integer :: mxfb   = 0
         integer :: nhfbnp = 1
         logical :: hfbfact  = .false.
         logical :: hfbresis = .true.
         type(tSpGcdLcd), dimension(:), pointer :: sp => null()
      end type tHfb
      type(tHfb), public, save :: hfb

!.......................................................................
      type tPcg
         character(len=maxlen) :: text = 'Preconditioned Conjugate-Gradient Package'
         integer :: mxiter = 100  ! maximum number of outer iterations
         integer :: iter1  = 20   ! maximum number of inner iterations
         integer :: npcond = 1    ! 1: modified incomplete Cholesky; 2: polynomial
         real    :: hclose = 1e-3 ! head change criterion
         real    :: rclose = 100. ! residual criteron
         real    :: relax  = 0.98 ! relaxation parameter
         real    :: nbpol  = 1.0  ! upperbound maximum eigen value
         integer :: iprpcg = 1    ! printout flag
         integer :: mutpcg = 0    ! print flag
         reaL    :: damp  = 1.    ! damping factor
      end type tPcg
      type(tPcg), public, save :: pcg
!.......................................................................
      type tPks
         logical :: active = .false. 
         character(len=maxlen) :: text = 'Parallel Krylov Solver Package'
         integer :: mxiter = 100     ! maximum number of outer iterations
         integer :: innerit = 20     ! maximum number of inner iterations
         integer :: isolver = 1      ! 1: PCG; 2: BiCGSTAB; 3: GMRES
         integer :: npc = 2          ! 2: incomplete LU
         real    :: hclosepks = 1e-3 ! head change criterion
         real    :: rclosepks = 100. ! residual criteron
         real    :: relaxpks  = 0.98 ! relaxation parameter
         integer :: iprpks = 1       ! printout flag
         integer :: mutpks = 0       ! print flag
         reaL    :: damp  = 1.       ! damping factor
         ! PKSMPI part
         integer :: nrproc = 0       ! 0 = serial; > 0 = parallel
         integer :: partopt = 0      ! 0 = uniform; 1 = RCB
         integer, dimension(:,:), allocatable :: partminmax ! ! ic1, ic2, ir1, ir2
         character(len=maxlen)                :: loadfile ! ! loadptr
         integer :: pressakey = 0 ! flag required for debugging with Visual Studio
      end type tPks
      type(tPks), public, save :: pks
!.......................................................................
      integer, parameter, public :: mxrech = 10
      type tSpRch
         logical :: reuse = .true.
         integer :: inrech = 0
         integer :: inirch = -1
         type(tArrayRead), dimension(mxrech) :: rech
         type(tArrayRead) :: irch
      end type tSpRch
      type tRch
         character(len=maxlen) :: text = 'Recharge Package file'
         integer                        :: type = irch
         integer                        :: nrchop = 1       ! 1: top grid layer; 2: layer variable IRCH; 3: highest active cell
         integer                        :: cbnlay = 0
         integer, dimension(:), pointer :: cblay => null()
         type(tSpRch), dimension(:), pointer :: sp => null()
      end type tRch
      type(tRch), public, save :: rch

!.......................................................................
      type tSpEvt
         logical :: reuse = .true.
         integer :: insurf = -1
         integer :: inevtr = -1
         integer :: inexdp = -1
         type(tArrayRead) :: surf ! elevation of the ET surface
         type(tArrayRead) :: evtr ! maximum ET flux
         type(tArrayRead) :: exdp ! ET extinction depth
      end type tSpEvt
      type tEvt
         character(len=maxlen) :: text = 'Evapotranspiration Package file'
         integer                        :: type = ievt
         integer                        :: nevtop = 1       ! 1: top grid layer; 2: vertical column specified by IEVT
         integer                        :: cbnlay = 0
         integer, dimension(:), pointer :: cblay => null()
         type(tSpEvt), dimension(:), pointer :: sp => null()
      end type tEvt
      type(tEvt), public, save :: evt

!.......................................................................
      type tDxc
         character(len=maxlen) :: text = 'DXC Package file'
         character(len=maxlen)          :: fname = ''
         integer                        :: cbnlay = 0
         integer, dimension(:), pointer :: cblay => null()
      end type tDxc
      type(tDxc), public, save :: dxc    
      
      character(len=8), public, save :: starttime
      integer, public, save :: debugflag = 0

      public :: AllocNam,AllocDis,AllocBas,AllocBcf,AllocMet,AllocOc, &
                AllocRiv,AllocDrn,AllocGhb,AllocWel,AllocAni,AllocHfb,AllocRch,AllocEvt,AllocChd,AllocPcg,AllocPks,AllocDxc,InitPks,PartPks,AllocScr,AllocPwt
      public :: WriteDis,WriteBas,WriteBcf,WriteMet,WriteOc,WriteRiv,&
                WriteDrn,WriteGhb,WriteWel,WriteAni,WriteHfb,WriteRch,WriteEvt,WriteChd,WritePcg,WritePks,WriteNam,WriteScr,WritePwt

      contains

!=======================================================================

      !> Allocate name package.
      subroutine AllocNam(iact)
      use imod_utl
      implicit none

!...     arguments
      integer, intent(in) :: iact ! dummy

!...     local variable
      logical :: sq, dq
      integer :: i, iu, n
      character(len=256) :: modeldir
!.......................................................................

!...     initialize
      if (iact == ialloc) then
         modeldir = imod_utl_getfname(root%modeldir)
         sq = .false.
         dq = .false.
         n = len_trim(modeldir)
         if (n==0) then
            modeldir = '.'
            n = 1
         end if
         if (modeldir(1:1).eq."'".and. modeldir(n:n).eq."'") sq = .true.
         if (modeldir(1:1).eq.'"'.and. modeldir(n:n).eq.'"') dq = .true.
         if (sq.or.dq) modeldir = modeldir(2:n-1)
         call osd_s_filename(modeldir)
!... change output
         root%modeldir = trim(modeldir)
         if(index(root%modeldir,'\',.true.).eq.len_trim(root%modeldir))root%modeldir(len_trim(root%modeldir):len_trim(root%modeldir))=' '
         if (len_trim(root%submodel).gt.0) then
            if (lmfroot) then
               nam%fname = trim(mfroot)//'\mf2005_tmp\'//trim(root%submodel)//'\'//trim(root%modelname)//'.nam'
            else
               nam%fname = trim(root%resultdir)//'\'//trim(root%submodel)//'\mf2005_tmp\'//trim(root%modelname)//'.nam'
            end if
         else
            if (lmfroot) then
               nam%fname = trim(mfroot)//'\mf2005_tmp\'//trim(root%modelname)//'.nam'
            else
               nam%fname = trim(root%resultdir)//'\mf2005_tmp\'//trim(root%modelname)//'.nam'
            end if
         end if
         call osd_s_filename(nam%fname)
         iu = 10
         do i = 1, npck
            nam%package(i)%ftype = pckftype(i)
            nam%package(i)%nunit = iu
            iu = iu + 1
            nam%package(i)%fnamenoroot = trim(root%modelname)//'.'//adjustl(trim(pckftype(i)))
            if (len_trim(root%submodel).gt.0) then
               if (lmfroot) then
                  nam%package(i)%fname = trim(mfroot)//'\mf2005_tmp\'//trim(root%submodel)//'/'//trim(nam%package(i)%fnamenoroot)
               else
                  nam%package(i)%fname = trim(root%resultdir)//'\'//trim(root%submodel)//'\mf2005_tmp\'//trim(nam%package(i)%fnamenoroot)
               end if
            else
               if (lmfroot) then
                  nam%package(i)%fname = trim(mfroot)//'\mf2005_tmp\'//trim(nam%package(i)%fnamenoroot)
               else
                  nam%package(i)%fname = trim(root%resultdir)//'\mf2005_tmp\'//trim(nam%package(i)%fnamenoroot)
               end if
            end if
            call osd_s_filename(nam%package(i)%fname)
         end do
         do i = 1, ndat
            nam%data(i)%ftype = datftype(i)
            nam%data(i)%nunit = iu
            iu = iu + 1
            nam%data(i)%fname = ''
            allocate(nam%data(i)%cblay(nlay))
         end do
         dxciu = iu

!...        enable list file
         nam%package(ilst)%active = .true.
      else
         do i = 1, ndat
            if (associated(nam%data(i)%cblay)) deallocate(nam%data(i)%cblay)
            nam%data(i)%active = .false.
            nam%data(i)%cbnlay = 0
         end do
      end if

      return
      end subroutine AllocNam

      !> Allocate discretization package.
      subroutine AllocDis(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact
!.......................................................................

      if (iact == ialloc) then

         nam%package(idis)%active = .true.

         allocate(dis%delr(ncol))
         allocate(dis%delc(nrow))
         allocate(dis%laycbd(nlay))
         allocate(dis%aquifertop(nlay))
         allocate(dis%aquiferbot(nlay))
         allocate(dis%sp(nper))

!...        set defaults
         dis%nlay = nlay
         dis%nrow = nrow
         dis%ncol = ncol
         dis%nper = nper
         dis%laycbd = 1
         dis%aquifertop(:)%keyword = 'constant'
         dis%aquifertop(:)%fct  = 0.
         dis%aquiferbot(:)%keyword = 'constant'
         dis%aquiferbot(:)%fct  = 0.
      else
         if (associated(dis%delr))       deallocate(dis%delr)
         if (associated(dis%delc))       deallocate(dis%delc)
         if (associated(dis%laycbd))     deallocate(dis%laycbd)
         if (associated(dis%aquifertop)) deallocate(dis%aquifertop)
         if (associated(dis%aquiferbot)) deallocate(dis%aquiferbot)
         if (associated(dis%sp))         deallocate(dis%sp)
      end if

      return
      end subroutine AllocDis

      !> Allocate basic package.
      subroutine AllocBas(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact
!.......................................................................

      if (iact == ialloc) then

         nam%package(ibas)%active = .true.

         allocate(bas%ibound(nlay))
         allocate(bas%strt(nlay))
      else
         if (associated(bas%ibound)) deallocate(bas%ibound)
         if (associated(bas%strt))   deallocate(bas%strt)
      end if

      return
      end subroutine AllocBas

      !> Allocate Block-Centered Flow package.
      subroutine AllocBcf(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact

!...     local variables
      integer :: ilay
!.......................................................................

      if (iact == ialloc) then

         nam%package(ibcf)%active = .true.

         allocate(bcf%ltype(nlay))
         allocate(bcf%sf1(nlay))
         allocate(bcf%tran(nlay))
         allocate(bcf%hhy(nlay))
         allocate(bcf%vhy(nlay-1))
         allocate(bcf%vcont(nlay-1))
         allocate(bcf%sf2(nlay))
         allocate(bcf%wetdry(nlay))
         allocate(bcf%cblay(nlay))
         allocate(bcf%kva(nlay))
         bcf%cblay = 0

!...        defaults
         bcf%ltype = 0 ! confined
         bcf%trpy%keyword = 'constant'
         bcf%trpy%cnstnt = 1.
         do ilay = 1, nlay - 1
            bcf%vcont(ilay)%oper = '^'
            bcf%vcont(ilay)%pow = -1.
         end do
         !## vertical anisotropy need to scaled numerically but entered to the lpf-package as its inverse
         !## so keep default values and inverse is computed after reading values.
!         do ilay = 1, nlay
!            bcf%kva(ilay)%oper = '^'
!            bcf%kva(ilay)%pow = -1.
!            bcf%kva(ilay)%keyword = 'constant'
!            bcf%kva(ilay)%cnstnt = 1.
!         end do
      else
         if (associated(bcf%ltype))  deallocate(bcf%ltype)
         if (associated(bcf%sf1))    deallocate(bcf%sf1)
         if (associated(bcf%tran))   deallocate(bcf%tran)
         if (associated(bcf%hhy))    deallocate(bcf%hhy)
         if (associated(bcf%vhy))    deallocate(bcf%vhy)
         if (associated(bcf%vcont))  deallocate(bcf%vcont)
         if (associated(bcf%sf2))    deallocate(bcf%sf2)
         if (associated(bcf%wetdry)) deallocate(bcf%wetdry)
         if (associated(bcf%cblay))  deallocate(bcf%cblay)
         bcf%cbnlay = 0
      end if

      return
      end subroutine AllocBcf

     !> Allocate perched water table package.
      subroutine AllocPwt(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact

!.......................................................................

      if (iact == ialloc) then

         nam%package(ipwt)%active = .true.

         allocate(pwt%ilay)
         allocate(pwt%sc2)
         allocate(pwt%bot)
         allocate(pwt%top2)
         allocate(pwt%thkaf)
         allocate(pwt%vcont)
      else
         if (associated(pwt%ilay))  deallocate(pwt%ilay)
         if (associated(pwt%sc2))   deallocate(pwt%sc2)
         if (associated(pwt%bot))   deallocate(pwt%bot)
         if (associated(pwt%top2))  deallocate(pwt%top2)
         if (associated(pwt%thkaf)) deallocate(pwt%thkaf)
         if (associated(pwt%vcont)) deallocate(pwt%vcont)
      end if

      return
      end subroutine AllocPwt

      !> Allocate meta data package.
      subroutine AllocMet(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact ! dummy

!...     locals
      integer :: ikey
!.......................................................................

      if (iact == ialloc) then

      nam%package(imet)%active = .true.

!...     fill with keywords
      do ikey = 1, nmetkws
         met%kws(ikey)%keyword = trim(metkws(ikey))
      end do

      else
         do ikey = 1, nmetkws
            met%kws(ikey)%type = imetu
            met%kws(ikey)%cval = ''
         end do
      end if

      return
      end subroutine AllocMet

      !> Allocate Output Control Option.
      subroutine AllocOc(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact ! dummy
!.......................................................................

!...     initialize
      if (iact == ialloc) then
         nam%package(ioc)%active = .true.
         allocate(oc%cblay(nlay))
      else
         deallocate(oc%cblay)
      end if
      oc%ihedun = nam%data(ihead)%nunit

      return
      end subroutine AllocOc

      !> Allocate River package.
      subroutine AllocRiv(iact,isft)

      implicit none

!...     arguments
      integer, intent(in) :: iact,isft
!.......................................................................
      if (iact == ialloc) then
         nam%package(iriv)%active = .true.
         allocate(riv%sp(nper))
         riv%sp(1:nper)%gcd%ncolumns = 4
         allocate(riv%cblay(nlay))
!         if (riv%ifvdl) then
         if (isft.eq.1) then
             allocate(riv%sft1,riv%sft2)
         end if
     else
         if (associated(riv%sp))    deallocate(riv%sp)
         if (associated(riv%cblay)) deallocate(riv%cblay)
      end if

      return
      end subroutine AllocRiv

      !> Allocate Drain package.
      subroutine AllocDrn(iact,iconchk)

      implicit none

!...     arguments
      integer, intent(in) :: iact
      integer, intent(in) :: iconchk
!.......................................................................

      if (iact == ialloc) then
         nam%package(idrn)%active = .true.
         allocate(drn%sp(nper))
         drn%iconchk = .false.
         drn%sp(1:nper)%gcd%ncolumns = 2
         if (iconchk.eq.1) then
            drn%iconchk = .true.
!            drn%sp(1:nper)%gcd%ncolumns = 3
!         else
!            drn%sp(1:nper)%gcd%ncolumns = 2
         end if
         allocate(drn%cblay(nlay))
      else
         if (associated(drn%sp))    deallocate(drn%sp)
         if (associated(drn%cblay)) deallocate(drn%cblay)

      end if

      return
      end subroutine AllocDrn

      !> Allocate General-Head boundary package.
      subroutine AllocGhb(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact
!.......................................................................

      if (iact == ialloc) then
         nam%package(ighb)%active = .true.
         allocate(ghb%sp(nper))
         ghb%sp(1:nper)%gcd%ncolumns = 2
         allocate(ghb%cblay(nlay))
      else
         if (associated(ghb%sp))    deallocate(ghb%sp)
         if (associated(ghb%cblay)) deallocate(ghb%cblay)
      end if

      return
      end subroutine AllocGhb

      !> Allocate Well package.
      subroutine AllocWel(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact
!.......................................................................

      if (iact == ialloc) then
         nam%package(iwel)%active = .true.
         allocate(wel%sp(nper))
         wel%sp(1:nper)%gcd%ncolumns = 1
         allocate(wel%cblay(nlay))
      else
         if (associated(wel%sp))    deallocate(wel%sp)
         if (associated(wel%cblay)) deallocate(wel%cblay)
      end if

      return
      end subroutine AllocWel

      !> Allocate anisotropy package.
      subroutine AllocAni(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact

!...     local variables
      integer :: ilay
!.......................................................................

      if (iact == ialloc) then

         nam%package(iani)%active = .true.

         allocate(ani%factor(nlay))
         allocate(ani%angle(nlay))

         ani%factor(1:nlay)%keyword='constant'
         ani%factor(1:nlay)%cnstnt = 1.
         ani%angle(1:nlay)%keyword='constant'
         ani%angle(1:nlay)%cnstnt = 0.
      else
         if (associated(ani%factor)) deallocate(ani%factor)
         if (associated(ani%angle))  deallocate(ani%angle)
      end if

      return
      end subroutine AllocAni

      !> Allocate subsidence-creep package.
      subroutine AllocScr(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact

!...     local variables
      integer :: ilay
!.......................................................................

      if (iact == ialloc) then

         nam%package(iscr)%active = .true.

         if(.not.associated(scr%gl0))then
          allocate(scr%gl0(1))
          allocate(scr%sgm(1))
          allocate(scr%sgs(1))
          allocate(scr%pcsoff(nlay))
          allocate(scr%pcs(nlay))
          scr%gl0(1)%keyword='constant'
          scr%gl0(1)%cnstnt= 1.
          scr%sgm(1)%keyword='constant'
          scr%sgm(1)%cnstnt= 1.
          scr%sgs(1)%keyword='constant'
          scr%sgs(1)%cnstnt= 1.
          allocate(scr%cblay(nlay))
          scr%cblay = 0
         else
          allocate(scr%thick(scr%nsys))
          allocate(scr%rrisoa(scr%nsys))
          allocate(scr%rrisob(scr%nsys))
          allocate(scr%caisoc(scr%nsys))
          allocate(scr%void(scr%nsys))
          allocate(scr%sub(scr%nsys))

          scr%thick(1:scr%nsys)%keyword='constant'
          scr%thick(1:scr%nsys)%cnstnt= 1.
          scr%rrisoa(1:scr%nsys)%keyword='constant'
          scr%rrisoa(1:scr%nsys)%cnstnt= 0.
          scr%rrisob(1:scr%nsys)%keyword='constant'
          scr%rrisob(1:scr%nsys)%cnstnt= 0.
          scr%caisoc(1:scr%nsys)%keyword='constant'
          scr%caisoc(1:scr%nsys)%cnstnt= 0.
          scr%void(1:scr%nsys)%keyword='constant'
          scr%void(1:scr%nsys)%cnstnt= 0.
          scr%sub(1:scr%nsys)%keyword='constant'
          scr%sub(1:scr%nsys)%cnstnt= 0.
         endif
      else
         if (associated(scr%pcsoff))deallocate(scr%pcsoff)
         if (associated(scr%pcs))   deallocate(scr%pcs)
         if (associated(scr%gl0))   deallocate(scr%gl0)
         if (associated(scr%sgm))   deallocate(scr%sgm)
         if (associated(scr%sgs))   deallocate(scr%sgs)
         if (associated(scr%thick)) deallocate(scr%thick)
         if (associated(scr%rrisoa))deallocate(scr%rrisoa)
         if (associated(scr%rrisob))deallocate(scr%rrisob)
         if (associated(scr%caisoc))deallocate(scr%caisoc)
         if (associated(scr%void))  deallocate(scr%void)
         if (associated(scr%sub))   deallocate(scr%sub)
      end if

      return
      end subroutine AllocScr

      !> Allocate Horizontal Flow Barrier package.
      subroutine AllocHfb(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact
!.......................................................................

      if (iact == ialloc) then
         nam%package(ihfb)%active = .true.
         allocate(hfb%sp(nper))
         hfb%sp(1:nper)%usegcd = .false.
         hfb%sp(1:nper)%uselcd = .true.
         hfb%sp(1:nper)%reuse  = .true.
      else
         if (associated(hfb%sp)) deallocate(hfb%sp)
      end if

      return
      end subroutine AllocHfb

      !> Allocate PCG package.
      subroutine AllocPcg(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact
!.......................................................................

      if (iact == ialloc) nam%package(ipcg)%active = .true.

      return
      end subroutine AllocPcg

      !> Allocate PKS package.
      subroutine AllocPks(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact
!.......................................................................

      if (iact == ialloc) then
         nam%package(ipks)%active = .true.
         pks%active = .true.
      end if         

      return
      end subroutine AllocPks

     !> Allocate DXC package.
      subroutine AllocDxc(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact
!.......................................................................

      if (iact == ialloc) then
         nam%package(idxc)%active = .true.
         allocate(dxc%cblay(nlay))
         dxc%cblay = 0         
      else
         if (associated(dxc%cblay)) deallocate(dxc%cblay)
      end if

      return
      end subroutine AllocDxc
      
     !> Initialization for he PKS package
      subroutine InitPks()

      implicit none
!.......................................................................
      
      if(.not.pks%active)return 
      
      call pks7mpigetnrproc(pks%nrproc)
      
      end subroutine InitPks
      
      !> Partitioning for he PKS package
      subroutine PartPks()
      
      use idfmodule
      use imod_idf, only: idfread, idfreadscale
      use mod_rf2mf, only: simcsize, simbox
      
      use pksmpi_mod, only: gncol,myrank

      implicit none

!...     locals      
      logical :: writesto !,le, lw, ls, ln
      integer :: nrproc, icol, irow, ic, ir
      real, dimension(:,:), allocatable :: loadptr
      real :: chdadd
!.......................................................................
      
      if(.not.pks%active)return 
      call pks7mpigetnrproc(nrproc)
      if(nrproc.le.1)return
      
      call pks7mpisetgnodes( ncol, nrow, nlay )
            
      if (pks%partopt.eq.0) then ! uniform
         pks%nrproc = nrproc
         allocate(pks%partminmax(pks%nrproc,4)) 
         call pks7mpipartdef( pks%partminmax(:,1), pks%partminmax(:,2),& 
                              pks%partminmax(:,3), pks%partminmax(:,4) )
      elseif(pks%partopt.eq.1.or.pks%partopt.eq.2) then ! RCB
         ! read and scale the loadpointer 
         pks%nrproc = nrproc
         allocate(pks%partminmax(pks%nrproc,4))
         if (.not.idfread(idfc,pks%loadfile,0)) CALL IMOD_UTL_PRINTTEXT('Data Set 5a: could not read LOADFILE',2)
         call idfnullify(idfm)
         idfm%ieq=0
         idfm%dx=simcsize
         idfm%dy=simcsize
         idfm%ncol=ncol
         idfm%nrow=nrow
         idfm%xmin = simbox(1)
         idfm%ymin = simbox(2)
         idfm%xmax = simbox(3)
         idfm%ymax = simbox(4)
         idfm%nodata = 0.
         if (.not.idfreadscale(idfc,idfm,4,idsclnointp)) CALL IMOD_UTL_PRINTTEXT('Data Set 5a: could not read LOADFILE',2)
         allocate(loadptr(ncol,nrow))
         loadptr = idfm%x
         ! assume ibound in case of partopt = 2
         if (pks%partopt.eq.2) then
            ! step 1: set load = 1 for active cells
            do irow = 1, nrow
               do icol = 1, ncol    
                  loadptr(icol,irow) = min(loadptr(icol,irow), 1.) 
               end do
            end do
            ! step 2: modify for constant head cells              
            chdadd = minval(loadptr)    
            if (chdadd.lt.0.) then ! constant head cells detected
               chdadd = chdadd - 1. ! set value that should be added (border)
               do irow = 1, nrow
                  do icol = 1, ncol                   
                     if (loadptr(icol,irow).ge.1.) then
                        if (icol.lt.ncol) then ! E
                           ic = icol + 1
                           ir = irow
                           if (loadptr(ic,ir).lt.0.) then
                              loadptr(ic,ir) = chdadd    
                           end if
                           if (irow.gt.1) then ! NE
                              ir = irow-1
                              if (loadptr(ic,ir).lt.0.) then
                                 loadptr(ic,ir) = chdadd    
                              end if
                           end if       
                           if (irow.lt.nrow) then ! SE
                              ir = irow+1
                              if (loadptr(ic,ir).lt.0.) then
                                 loadptr(ic,ir) = chdadd    
                              end if
                           end if       
                        end if    
                         
                        if (icol.gt.1) then ! W
                           ic = icol - 1
                           ir = irow
                           if (loadptr(ic,ir).lt.0.) then
                              loadptr(ic,ir) = chdadd    
                           end if
                           if (irow.gt.1) then ! NW
                              ir = irow-1
                              if (loadptr(ic,ir).lt.0.) then
                                 loadptr(ic,ir) = chdadd    
                              end if
                           end if       
                           if (irow.lt.nrow) then ! SW
                              ir = irow+1
                              if (loadptr(ic,ir).lt.0.) then
                                 loadptr(ic,ir) = chdadd    
                              end if
                           end if       
                        end if    
                         
                        if (irow.gt.1) then ! N
                           ic = icol
                           ir = irow-1
                           if (loadptr(ic,ir).lt.0.) then
                              loadptr(ic,ir) = chdadd    
                           end if
                        end if
                        
                        if (irow.lt.nrow) then ! S
                           ic = icol
                           ir = irow+1
                           if (loadptr(ic,ir).lt.0.) then
                              loadptr(ic,ir) = chdadd    
                           end if
                        end if
                     end if
                   end do
               end do
               ! remove constant head cells   
               do irow = 1, nrow
                  do icol = 1, ncol                   
                     if (loadptr(icol,irow).lt.0. .and. loadptr(icol,irow).gt.chdadd) then
                        loadptr(icol,irow) = 0.
                     end if 
                  end do      
               end do            
            end if
         end if
         call idfdeallocatex(idfc)
         call pks7mpipartrcb( pks%partminmax(:,1), pks%partminmax(:,2),& 
                              pks%partminmax(:,3), pks%partminmax(:,4),& 
                              ncol, nrow, loadptr, 0.)
         deallocate(loadptr)         
      end if    
      
      ! check number of processes
      if(nrproc.ne.pks%nrproc) call imod_utl_printtext('Data Set 5: non-matching number of processes',2) 
      
      end subroutine PartPks
      
      !> Allocate Recharge package.
      subroutine AllocRch(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact
!.......................................................................

      if (iact == ialloc) then
         nam%package(irch)%active = .true.
         allocate(rch%sp(nper))
         allocate(rch%cblay(nlay))
      else
         if (associated(rch%sp))    deallocate(rch%sp)
         if (associated(rch%cblay)) deallocate(rch%cblay)
      end if

      return
      end subroutine AllocRch

      !> Allocate Evapotranspiration package.
      subroutine AllocEvt(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact
!.......................................................................

      if (iact == ialloc) then
         nam%package(ievt)%active = .true.
         allocate(evt%sp(nper))
         evt%sp(1:nper)%evtr%fct = 0.001D0 ! iMOD in mm!
         allocate(evt%cblay(nlay))
      else
         if (associated(evt%sp))    deallocate(evt%sp)
         if (associated(evt%cblay)) deallocate(evt%cblay)
      end if

      return
      end subroutine AllocEvt

      !> Allocate Constant head package.
      subroutine AllocChd(iact)

      implicit none

!...     arguments
      integer, intent(in) :: iact
!.......................................................................

      if (iact == ialloc) then
         nam%package(ichd)%active = .true.
         allocate(chd%sp(nper))
         chd%sp(1:nper)%gcd%ncolumns = 3
      else
         if (associated(chd%sp)) deallocate(chd%sp)
      end if

      return
      end subroutine AllocChd

!=======================================================================

      !> Write name file.
      subroutine WriteNam(dxcfile)

      implicit none

!...     arguments
      character(len=*), intent(in) :: dxcfile

!...     locals
      integer :: cfn_getlun, lun, ipck, idat, i, lastlun
      character(len=maxlen) :: outstr,absfile
      character(len=maxlen), dimension(3) :: str
!.......................................................................

!...     open nam-file
      lun = cfn_getlun(10,99)
      call CreateDir(nam%fname)
      open(unit=lun,file=nam%fname,action='write') ! text

!...     write nam-file
      write(lun,'(a,1x,a)') '#', trim(nam%text) ! text
      write(lun,'(a)') '# packages:'
      do ipck = 1, npck
         if (nam%package(ipck)%active == .true.) then
             write(str(1),*) trim(nam%package(ipck)%ftype)
             write(str(2),*) nam%package(ipck)%nunit
             call osd_s_filename(nam%package(ipck)%fnamenoroot)
             if(ipck.ne.idxc)then
                write(str(3),*) "'"//trim(nam%package(ipck)%fnamenoroot)//"'"
             else
                write(str(3),*) "'"//trim(dxc%fname)//"'"
             end if                
             write(lun,'(3(a,1x))')(trim(adjustl(str(i))), i = 1, 3) ! ftype nunit fname
         end if
      end do
      write(lun,'(a)') '# data:'
      do idat = 1, ndat
         if (nam%data(idat)%active == .true.) then
             write(str(1),*) trim(nam%data(idat)%ftype)
             write(str(2),*) nam%data(idat)%nunit
             if (len_trim(nam%data(idat)%fname).gt.0) call osd_s_filename(nam%data(idat)%fname)
             write(str(3),*) "'"//trim(nam%data(idat)%fname)//"'"
             write(lun,'(3(a,1x))')(trim(adjustl(str(i))), i = 1, 3) ! ftype nunit fname
         end if
      end do

!...     close nam-file
      close(lun)

      return
      end subroutine WriteNam

      !> Write basic anisotropy file.
      subroutine WriteAni()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, i, ilay
!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(iani)%active) return

!...     set slashes
      call osd_s_filename(nam%package(iani)%fname)

!...     open ani-file
      call CreateDir(nam%package(iani)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(iani)%fname,action='write')

!...     write ani file
      do ilay = 1, nlay
         call WriteArrayRead(ani%factor(ilay),lun) ! factor
         call WriteArrayRead(ani%angle(ilay),lun) ! angle
      end do

!...     close ani file
      close(lun)

      return
      end subroutine WriteAni

      !> Write discretization file.
      subroutine WriteDis()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, i, ilay, irow, icol, iper
      character(len=maxlen) :: nstr
      character(len=maxlen), dimension(:), allocatable :: str
      character(len=20), dimension(:), allocatable :: strdelrc
!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(idis)%active) return

!...     set slashes
      call osd_s_filename(nam%package(idis)%fname)

      allocate(str(max(7,nlay)))
      allocate(strdelrc(max(ncol,nrow)))

!...     open dis-file
      call CreateDir(nam%package(idis)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(idis)%fname,action='write')

!...     write dis-file
      write(lun,'(a,1x,a)') '#', trim(dis%text) ! text
      write(str(1),*) dis%nlay
      write(str(2),*) dis%nrow
      write(str(3),*) dis%ncol
      write(str(4),*) dis%nper
      write(str(5),*) dis%itmuni
      write(str(6),*) dis%lenuni
      write(str(7),*) 'tbcheck'
      write(lun,'(7(a,1x))')(trim(adjustl(str(i))), i = 1, 7) ! nlay nrow ncol nper itmuni lenuni
      do ilay = 1, nlay
       if(ilay.eq.nlay)then
        dis%laycbd(ilay)=0
       else
        dis%laycbd(ilay)=1
       endif
      end do
      do ilay = 1, nlay
         write(str(ilay),*) dis%laycbd(ilay)
      end do
      write(nstr,*) nlay
      write(lun,'('//trim(nstr)//'(a,1x))')(trim(adjustl(str(i))), i = 1, nlay) ! laycbd
      if (minval(dis%delr) == maxval(dis%delr)) then
         write(strdelrc(1),*) 'constant'
         write(strdelrc(2),*) dis%delr(1)
         write(lun,'(2(a,1x))')(trim(adjustl(strdelrc(i))), i = 1, 2) ! delr
      else
         write(lun,'(A)') 'INTERNAL,1.0,(FREE),-1'
         do icol = 1, ncol
            write(strdelrc(icol),*) dis%delr(icol)
         end do
         write(nstr,*) ncol
         write(lun,'('//trim(nstr)//'(a,1x))')(trim(adjustl(strdelrc(i))), i = 1, ncol) ! delr
      end if
      if (minval(dis%delc) == maxval(dis%delc)) then
         write(strdelrc(1),*) 'constant'
         write(strdelrc(2),*) dis%delc(1)
         write(lun,'(2(a,1x))')(trim(adjustl(strdelrc(i))), i = 1, 2) ! delr
      else
         write(lun,'(A)') 'INTERNAL,1.0,(FREE),-1'
         do irow = 1, nrow
            write(strdelrc(irow),*) dis%delc(irow)
         end do
         write(nstr,*) nrow
         write(lun,'('//trim(nstr)//'(a,1x))')(trim(adjustl(strdelrc(i))), i = 1, nrow) ! delc
      end if

      do ilay = 1, nlay
         call WriteArrayRead(dis%aquifertop(ilay),lun) ! top
         call WriteArrayRead(dis%aquiferbot(ilay),lun) !bot
      end do
      do iper = 1, nper
         if (dis%sp(iper)%perlen < 0.0 .or. len(dis%sp(iper)%SsTr) < 2) then
            write(*,*) 'Error: incorrect stress period writing for DIS file'
            stop 1
         end if
         !## set perlen eq 1.0 for steady-state, otherwise crash in UZF package
         if(dis%sp(iper)%SsTr.eq.'SS')then
          write(str(1),'(G15.7)') 1.0 !dis%sp(iper)%perlen         
         else
          write(str(1),'(G15.7)') dis%sp(iper)%perlen
         endif
         write(str(2),'(I10)') dis%sp(iper)%nstp
         write(str(3),'(G15.7)') dis%sp(iper)%tsmult
         write(str(4),'(A)') dis%sp(iper)%SsTr
         write(lun,'(4(a,1x))')(trim(adjustl(str(i))), i = 1, 4) ! perlen nstp tsmult SS/Tr
      end do

!...     close dis-file
      close(lun)
      deallocate(str,strdelrc)

      return
      end subroutine WriteDis

      !> Write basic package file.
      subroutine WriteBas()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, i, ilay
      character(len=maxlen) :: str
!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(ibas)%active) return

!...     set slashes
      call osd_s_filename(nam%package(ibas)%fname)

!...     open bas-file
      call CreateDir(nam%package(ibas)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(ibas)%fname,action='write')

!...     write bas file
      write(lun,'(a,1x,a)') '#', trim(bas%text)
      write(lun,'(a)') trim(bas%options)
      do ilay = 1, nlay
         call WriteArrayRead(bas%ibound(ilay),lun) ! ibound
      end do
      write(str,*) bas%hnoflo ! hnoflo
      write(lun,*) trim(adjustl(str))
      do ilay = 1, nlay
         call WriteArrayRead(bas%strt(ilay),lun) ! strt
      end do

!...     close bas file
      close(lun)

      return
      end subroutine WriteBas

      !> Write Block-Centered Flow package.
      subroutine WriteBcf()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, i, ilay, laycon, luncb, n
      character(len=maxlen), dimension(10) :: str
      character(len=maxlen) :: nlaystr, fmtstr

!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(ibcf)%active) return

!...     change to lpf package whenever unconfined
      if(bcf%llpf)then
         nam%package(ibcf)%ftype = 'lpf'
         nam%package(ibcf)%fname=nam%package(ibcf)%fname(:index(nam%package(ibcf)%fname,'.',.true.))//'lpf'
         nam%package(ibcf)%fnamenoroot=nam%package(ibcf)%fnamenoroot(:index(nam%package(ibcf)%fnamenoroot,'.',.true.))//'lpf'
      endif

!...     set slashes
      call osd_s_filename(nam%package(ibcf)%fname)

!...     set oc data
      nam%data(ibcfflux)%fname  = 'bdgsto bdgbnd bdgfrf bdgfff bdgflf'
      nam%data(ibcfflux)%cbnlay = bcf%cbnlay
      nam%data(ibcfflux)%cblay  = bcf%cblay

!...     open bcf/lpf-file
      call CreateDir(nam%package(ibcf)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(ibcf)%fname,action='write')

!...   determine whether transient simulation using storage
      trflag=.false.
      do i=1,nper
       if(dis%sp(i)%SsTr.eq.'TR')trflag=.true.
      enddo
!...     write bcf/lpf file
      if (bcf%cbnlay.gt.0) then
         nam%data(ibcfflux)%active = .true.
         luncb = nam%data(ibcfflux)%nunit
      else
         luncb = 0
      end if
      write(str(1),*) luncb
      write(str(2),*) bcf%hdry
      if(bcf%llpf)then
         write(str(3),*) 0
         n = 3
         n = n + 1
         write(str(n),*) 'STORAGECOEFFICIENT'
         if (bcf%iminkd == 1) then
             n = n + 1
             write(str(n),*) 'MINKD',bcf%minkd
         end if
         if (bcf%iminc == 1) then
             n = n + 1
             write(str(n),*) 'MINC',bcf%minc
         end if
         write(fmtstr,'(a,i1,a)') '(',n,'(a,1x))'
         write(lun,trim(fmtstr))(trim(adjustl(str(i))), i = 1, n)
      else
         write(str(3),*) bcf%iwdflg
         write(str(4),*) bcf%wetfct
         write(str(5),*) bcf%iwetit
         write(str(6),*) bcf%ihdwet
         n = 6
         if (bcf%iminkd == 1) then
             n = n + 1
             write(str(n),*) 'MINKD',bcf%minkd
         end if
         if (bcf%iminc == 1) then
             n = n + 1
             write(str(n),*) 'MINC',bcf%minc
         end if
         write(fmtstr,'(a,i1,a)') '(',n,'(a,1x))'
         write(lun,trim(fmtstr))(trim(adjustl(str(i))), i = 1, n)
      endif
      write(nlaystr,*) nlay
      write(lun,'('//trim(nlaystr)//'i2)')(bcf%ltype(ilay), ilay = 1, nlay) ! laytype
      if(bcf%llpf)then
         write(lun,'('//trim(nlaystr)//'i2)')(0, ilay = 1, nlay) ! layavg
      endif
      if(bcf%llpf)then
         write(lun,'('//trim(nlaystr)//'f3.0)')(1.0, ilay = 1, nlay) ! chani
         write(lun,'('//trim(nlaystr)//'i2)')(1, ilay = 1, nlay) ! layvka (vertical permeability)
         write(lun,'('//trim(nlaystr)//'i2)')(0, ilay = 1, nlay) ! laywet (inactive)
!... skip wettable layers, we don't have those (wetting is also inappropriate to use)
         do ilay = 1, nlay
            laycon = bcf%ltype(ilay)
            call WriteArrayRead(bcf%hhy(ilay),lun) ! hy
            call WriteArrayRead(bcf%kva(ilay),lun) ! vertical anisotropy
            if (trflag) then
               call WriteArrayRead(bcf%sf1(ilay),lun) ! sf1
            if ((laycon == 2 .or. laycon == 3))&
               call WriteArrayRead(bcf%sf2(ilay),lun) ! sf2
            endif
            if (ilay < nlay) then
               call WriteArrayRead(bcf%vhy(ilay),lun) ! vertical anisotropy
            end if
            if (bcf%iwdflg /= 0 .and. (laycon == 1 .or. laycon == 3))&
               call WriteArrayRead(bcf%wetdry(ilay),lun) ! wetdry
         end do
      else
         call WriteArrayRead(bcf%trpy,lun) ! chani
         do ilay = 1, nlay
            laycon = bcf%ltype(ilay)
            if (trflag) call WriteArrayRead(bcf%sf1(ilay),lun) ! sf1
            if (laycon == 0 .or. laycon == 2)&
            call WriteArrayRead(bcf%tran(ilay),lun)  ! tran
            if (laycon == 1 .or. laycon == 3)&
               call WriteArrayRead(bcf%hhy(ilay),lun) ! hy
            if (ilay /= nlay) call WriteArrayRead(bcf%vcont(ilay),lun) ! vcont
            if (trflag .and. (laycon == 2 .or. laycon == 3))&
               call WriteArrayRead(bcf%sf2(ilay),lun) ! sf2
            if (bcf%iwdflg /= 0 .and. (laycon == 1 .or. laycon == 3))&
               call WriteArrayRead(bcf%wetdry(ilay),lun) ! wetdry
         end do
      endif

!...     close bcf/lpf file
      close(lun)

      return
      end subroutine WriteBcf


     !> Write perched water table file.
      subroutine WritePwt()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, i, ilay
!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(ipwt)%active) return

!...     set slashes
      call osd_s_filename(nam%package(ipwt)%fname)

!...     open ani-file
      call CreateDir(nam%package(ipwt)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(ipwt)%fname,action='write')

!...     write pwt file
      call WriteArrayRead(pwt%ilay,lun)
      call WriteArrayRead(pwt%sc2,lun)
      call WriteArrayRead(pwt%bot,lun)
      call WriteArrayRead(pwt%top2,lun)
      call WriteArrayRead(pwt%thkaf,lun)
      call WriteArrayRead(pwt%vcont,lun)

!...     close pwt file
      close(lun)

      return
      end subroutine WritePwt

      !> Write subsidence-creep file.
      subroutine WriteScr()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, i, ilay
      character(len=maxlen), dimension(50) :: str

!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(iscr)%active) return

!...     set slashes
      call osd_s_filename(nam%package(iscr)%fname)

!...     set oc data
      nam%data(iscrflux)%fname  = 'bdgibs,subsidence,layer_compaction,systm_compaction,z_displacement,preconsol_stress,change_in_pcstrs,'// &
          'geostatic_stress,change_in_g-strs,effective_stress,change_in_eff-st,void_ratio,thickness,center_elevation'
      nam%data(iscrflux)%cbnlay = scr%cbnlay
      nam%data(iscrflux)%cblay  = scr%cblay

!                 unit for saving subsidence is  70
!        unit for saving compaction by layer is  70
!       unit for saving compaction by system is  70
!      unit for saving vertical displacement is  70
!    unit for saving preconsolidation stress is  70
! unit for saving change in preconsol stress is  70
!           unit for saving geostatic stress is  70
! unit for saving change in geostatic stress is  70
!           unit for saving effective stress is  70
! unit for saving change in effective stress is  70
!                 unit for saving void ratio is  70
!                  unit for saving thickness is  70
!           unit for saving center elevation is  70

!...     open scr-file
      call CreateDir(nam%package(iscr)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(iscr)%fname,action='write')

      nam%data(iscrflux)%active = .true.

      write(str(1),*) 35
      write(str(2),*) 1  !## a single definition for saving, foor all stress-periods results (for now)
      write(str(3),*) scr%nsys
      write(str(4),*) 0
      write(str(5),*) scr%ithk
      write(str(6),*) scr%ivoid
      write(str(7),*) scr%imethod
      write(str(8),*) scr%istpc
      write(str(9),*) scr%alpha
      write(lun,'(9(a,1x))')(trim(adjustl(str(i))), i = 1, 9)

!...    get layers for current systems
      do ilay=1,scr%nsys
       write(str(ilay),*) scr%thick(ilay)%ilay
      enddo
      write(lun,'(99(a,1x))')(trim(adjustl(str(i))), i = 1, scr%nsys)

      write(str(1),*) -1
      write(str(2),*) 0
      write(str(3),*) -1
      write(str(4),*) 0
      write(str(5),*) -1
      write(str(6),*) 0
      write(str(7),*) -1
      write(str(8),*) 0
      write(lun,'(8(a,1x))')(trim(adjustl(str(i))), i = 1, 8)

!...     write scr file
      call WriteArrayRead(scr%gl0(1),lun) ! gl0
      call WriteArrayRead(scr%sgm(1),lun) ! sgm
      call WriteArrayRead(scr%sgs(1),lun) ! sgs
      do ilay = 1, scr%nsys
         call WriteArrayRead(scr%thick(ilay),lun)  ! thickness
         call WriteArrayRead(scr%rrisoa(ilay),lun) ! rrisoa
         call WriteArrayRead(scr%rrisob(ilay),lun) ! rrisob
         call WriteArrayRead(scr%caisoc(ilay),lun) ! caisoc
         call WriteArrayRead(scr%void(ilay),lun)   ! void
         call WriteArrayRead(scr%sub(ilay),lun)    ! sub
      end do
      do ilay = 1, nlay
         if(scr%istpc.ne.0)then
          call WriteArrayRead(scr%pcsoff(ilay),lun)  ! pcsoff
         else
          call WriteArrayRead(scr%pcs(ilay),lun)     ! pcs
         endif
      enddo

!...      write istwoc for formats for output, irrelavant icw IDF files
      do ilay=1,26,2; write(str(ilay),*) 0; write(str(ilay+1),*) 35; enddo
      write(lun,'(99(a,1x))')(trim(adjustl(str(i))), i = 1, 26)
!...      write starting stessperiod, ending stressperiod, starting timestep, ending timestep
      write(str(1),*) 1
      write(str(2),*) nper
      write(str(3),*) 1
      write(str(4),*) 1
!...      write print/save sequences
!subsidence 1 2
!compaction by layer 3 4
!compaction by interbed system 5 6
!vertical displacement 7 8
!preconsolidation stress 9 10
!change in preconsolidation stress 11 12
!geostatic stress 13 14
!change in geostatic stress 15 16
!effective stress 17 18
!change in effective stress 19 20
!void ratio 21 22
!compressible bed thickness 23 24
!layer-center elevation 25 26
      ilay=4
      do i=1,13
       ilay=ilay+1; write(str(ilay),*) 0
!...     save them all
       ilay=ilay+1; write(str(ilay),*) 1
      enddo
      write(lun,'(99(a,1x))')(trim(adjustl(str(i))), i = 1, 30)

!...     close scr file
      close(lun)

      return
      end subroutine WriteScr

      !> Write Meta Data package.
      subroutine WriteMet()

      implicit none

!...     locals
      logical :: writekey, dq, sq
      integer :: cfn_getlun, lun, ikey, n, i
      character(len=maxlen) :: cdum
      character(len=maxlen), dimension(15) :: str, nstr
      type(tKw) :: kwtmp
!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(imet)%active) return

!...     set slashes
      call osd_s_filename(nam%package(imet)%fname)

!...     open met-file
      call CreateDir(nam%package(imet)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(imet)%fname,action='write')

      do ikey = 1, nmetkws
         kwtmp = met%kws(ikey)
         writekey = .true.
         select case( kwtmp%type )
            case(imeti)
               write(str(1),*) kwtmp%ival
            case(imetr)
               write(str(1),'(F15.3)') kwtmp%rval
            case(imetc)
               cdum = trim(adjustl(kwtmp%cval))
               n = len_trim(cdum)
               sq = .false.
               dq = .false.
               if (n.gt.0) then
                  if (cdum(1:1).eq."'".and. cdum(n:n).eq."'") sq = .true.
                  if (cdum(1:1).eq.'"'.and. cdum(n:n).eq.'"') dq = .true.
               end if
               if (sq.or.dq) cdum = cdum(2:n-1)
               if (n.gt.0) then
                  str(1) = "'"//trim(cdum)//"'"
                  !...     set slashes
                  call osd_s_filename(str(1))
               else
                  str(1) = ''
               end if
            case(imett)
               n = 1
               if (kwtmp%time%year.gt.0) then
                  n = n + 1
                  write(str(n),*) kwtmp%time%kw_year
                  n = n + 1
                  write(str(n),*) kwtmp%time%year
               end if
               if (kwtmp%time%month.gt.0) then
                  n = n + 1
                  write(str(n),*) kwtmp%time%kw_month
                  n = n + 1
                  write(str(n),*) kwtmp%time%month
               end if
               if (kwtmp%time%day.gt.0) then
                  n = n + 1
                  write(str(n),*) kwtmp%time%kw_day
                  n = n + 1
                  write(str(n),*) kwtmp%time%day
               end if
               write(nstr,*) n
               write(str(1), '(10(a,:,1x))')(trim(adjustl(str(i))), i = 2, n)
            case default
               writekey = .false.
         end select
         if (writekey) then
            write(lun,'(a,1x,a)') trim(adjustl(kwtmp%keyword)), trim(adjustl(str(1)))
         end if
      end do
!
!...     close met file
      close(lun)

      return
      end subroutine WriteMet

      !> Write output control option
      subroutine WriteOc()

      implicit none

!...     locals
      integer :: lun, cfn_getlun, iper, ikstp, idat, ilay, n, i
      character(len=maxlen) :: nstr
      character(len=maxlen), dimension(:), allocatable :: str
!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(ioc)%active) return

!...     set slashes
      call osd_s_filename(nam%package(ioc)%fname)

!...     open oc-file
      call CreateDir(nam%package(ioc)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(ioc)%fname,action='write')

!...     write oc-file
      allocate(str(nlay+2))
      if (oc%cbnlay.gt.0) then
         write(str(1),*) oc%ihedun
         write(lun,'(a,1x,a)') 'head save unit', trim(adjustl(str(1)))
      end if

      do iper = 1, nper
         if (dis%sp(iper)%writeoc) then
            do ikstp = 1, dis%sp(iper)%nstp
                write(str(1),*) iper
                write(str(2),*) ikstp
                write(lun,'(4(a,1x))') 'period', trim(adjustl(str(1))),&
                                       'step', trim(adjustl(str(2)))
                !... heads
                if (oc%cbnlay.gt.0) then
                   write(str(1),'(a)') 'save head'
                   n = 1
                   do ilay = 1, oc%cbnlay
                      write(str(1+ilay),*) oc%cblay(ilay)
                      n = n + 1
                   end do
                   write(nstr,*) n
                   write(lun,'('//trim(nstr)//'(a,1x))')(trim(adjustl(str(i))), i = 1, n)
                end if

                ! budgets
                do idat = 1, ndat
                   if (nam%data(idat)%cbnlay.gt.0) then
                      write(str(1),'(a)') 'save budget'
                      write(str(2),*) nam%data(idat)%nunit
                      n = 2
                      do ilay = 1, nam%data(idat)%cbnlay
                         write(str(2+ilay),*) nam%data(idat)%cblay(ilay)
                         n = n + 1
                      end do
                      write(nstr,*) n
                      write(lun,'('//trim(nstr)//'(a,1x))')(trim(adjustl(str(i))), i = 1, n)
                   end if
                end do
            end do
         end if
      end do

!...     close oc-file
      close(lun)

!...     set for name file
      if (oc%cbnlay.gt.0) nam%data(ihead)%active = .true.

      deallocate(str)
      
      return
      end subroutine WriteOc

      !> Write River package.
      subroutine WriteRiv()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, iper, jper, kper, isub, n, i, luncb, isys, nsys, iact
      character(len=maxlen) :: nstr
      character(len=maxlen), dimension(20) :: str
      type(tsubsys), dimension(:), pointer :: subsys => null()
!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(iriv)%active) return

!...     set slashes
      call osd_s_filename(nam%package(iriv)%fname)

!...     write oc-data
      nam%data(irivflux)%fname  = 'bdgriv'
!      do iper = 1, nper
!       if (riv%sp(iper)%lriv)then
!        nam%data(irivflux)%fname  = trim(nam%data(irivflux)%fname)//' bdgriv'; exit
!       endif
!      enddo
!      do iper = 1, nper
!       if (riv%sp(iper)%lisg)then
!        nam%data(irivflux)%fname  = trim(nam%data(irivflux)%fname)//' bdgisg'; exit
!       endif
!      enddo
!      nam%data(irivflux)%fname=adjustl(nam%data(irivflux)%fname)
      nam%data(irivflux)%cbnlay = riv%cbnlay
      nam%data(irivflux)%cblay  = riv%cblay

!...     open riv-file
      call CreateDir(nam%package(iriv)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(iriv)%fname,action='write')

!...     write riv file
      write(lun,'(a,1x,a)') '#', trim(riv%text)

!...     write mxactr and irivcb
      n = 1
      write(str(n),*) riv%mxactr
      if (riv%cbnlay.gt.0) then
         nam%data(irivflux)%active = .true.
         luncb = nam%data(irivflux)%nunit
      else
         luncb = 0
      end if
      n = n + 1
      write(str(n),*) luncb
      if (debugflag.le.0) then
         n = n + 1
         write(str(n),*) 'noprint'
      end if
      if (riv%ifvdl) then
         n = n + 1
         write(str(n),*) 'ifvdl'
      end if
      if (riv%sft) then
         n = n + 1
         write(str(n),*) 'sft'
      end if
      if (riv%rfact) then
         n = n + 1
         write(str(n),*) 'aux rfct'
      end if
      if (riv%rconc) then
         n = n + 1
         write(str(n),*) 'aux rcnc'
      end if
      if (riv%rsubsys) then
         n = n + 1
         write(str(n),*) 'aux isub'
      end if
      if (riv%rfact) then
         n = n + 1
         write(str(n),*) 'rfact rfct'
      end if
      if (riv%rconc) then
         n = n + 1
         write(str(n),*) 'rconc rcnc'
      end if
      if (riv%rsubsys) then
         n = n + 1
         write(str(n),*) 'rsubsys isub'
      end if
      write(nstr,*) n
      write(lun,'('//trim(nstr)//'(a,1x))')(trim(adjustl(str(i))), i = 1, n)

      if (riv%sft) then
         call WriteArrayRead(riv%sft1,lun) ! D
         call WriteArrayRead(riv%sft2,lun) ! kh
      end if

      ! merge river and isg data
      do iper = 1, nper
         if (.not.riv%sp(iper)%reuse) then
            if (riv%sp(iper)%lriv .and. .not.riv%sp(iper)%lisg) then
               kper = 0
               do jper = iper, 1, -1 ! find most recent isg
                  if (riv%sp(jper)%lisg) then
                     kper = jper; exit
                  end if
               end do
               if (kper.gt.0) then
                  do iact = 1, 2
                     nsys = riv%sp(iper)%gcd%nsubsys
                     if (iact.eq.2) then
                        do i=1,nsys; subsys(i) = riv%sp(iper)%gcd%subsys(i); enddo
                     end if
                     do isys = 1, riv%sp(kper)%gcd%nsubsys
                        if (riv%sp(kper)%gcd%subsys(isys)%lisg) then ! find isg
                           nsys = nsys + 1
                           if (iact.eq.2) then
                              subsys(nsys) = riv%sp(kper)%gcd%subsys(isys)
                           end if
                        end if
                     end do
                     if (iact.eq.1) then
                        allocate(subsys(nsys))
                     else
                        riv%sp(iper)%gcd%nsubsys = nsys
                        do i=1,nsys; riv%sp(iper)%gcd%subsys(i) = subsys(i); enddo
                        deallocate(subsys)
                     end if
                  end do
               end if
            else if (.not.riv%sp(iper)%lriv .and. riv%sp(iper)%lisg) then
               kper = 0
               do jper = iper, 1, -1 ! find most recent riv
                  if (riv%sp(jper)%lriv) then
                     kper = jper; exit
                  end if
               end do
               if (kper.gt.0) then
                  do iact = 1, 2
                     nsys = riv%sp(iper)%gcd%nsubsys
                     if (iact.eq.2) then
                        do i=1,nsys; subsys(i) = riv%sp(iper)%gcd%subsys(i); enddo
                     end if
                     do isys = 1, riv%sp(kper)%gcd%nsubsys
                        if (riv%sp(kper)%gcd%subsys(isys)%lriv) then ! find riv
                           nsys = nsys + 1
                           if (iact.eq.2) then
                              subsys(nsys) = riv%sp(kper)%gcd%subsys(isys)
                           end if
                        end if
                     end do
                     if (iact.eq.1) then
                        allocate(subsys(nsys))
                     else
                        riv%sp(iper)%gcd%nsubsys = nsys
                        do i=1,nsys; riv%sp(iper)%gcd%subsys(i) = subsys(i); enddo
                        deallocate(subsys)
                     end if
                  end do
               end if
            else
            end if
         end if
      end do

      call writeSPckSPer(lun, nper, riv%sp, pckftype(riv%type))

!...     close riv file
      close(lun)

      return
      end subroutine WriteRiv

      !> Write Drain package.
      subroutine WriteDrn()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, iper, isub, n, i, luncb, kper, jper, iact, nsys, isys
      character(len=maxlen) :: nstr
      character(len=maxlen), dimension(10) :: str
      type(tsubsys), dimension(:), pointer :: subsys => null()

!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(idrn)%active) return

!...     set slashes
      call osd_s_filename(nam%package(idrn)%fname)

!...     write oc-data
      nam%data(idrnflux)%fname  = 'bdgdrn'
      nam%data(idrnflux)%cbnlay = drn%cbnlay
      nam%data(idrnflux)%cblay  = drn%cblay

!...     open drn-file
      call CreateDir(nam%package(idrn)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(idrn)%fname,action='write')

!...     write drn file
      write(lun,'(a,1x,a)') '#', trim(drn%text)

!...     write mxactr and idrncb
      n = 1
      write(str(n),*) drn%mxactr
      if (drn%cbnlay.gt.0) then
         nam%data(idrnflux)%active = .true.
         luncb = nam%data(idrnflux)%nunit
      else
         luncb = 0
      end if
      n = n + 1
      write(str(n),*) luncb
      if (debugflag.le.0) then
         n = n + 1
         write(str(n),*) 'noprint'
      end if
      if (drn%dsubsys) then
         n = n + 1
         write(str(n),*) 'aux isub'
      end if
!      if (drn%iconchk) then
!         n = n + 1
!         write(str(n),*) 'aux ic'
!      end if
      if (drn%dsubsys) then
         n = n + 1
         write(str(n),*) 'dsubsys isub'
      end if
      if (drn%iconchk) then
         n = n + 1
         write(str(n),*) 'iconchk' ! ic'
      end if

      write(nstr,*) n
      write(lun,'('//trim(nstr)//'(a,1x))')(trim(adjustl(str(i))), i = 1, n)

      ! merge drn and olf data
      do iper = 1, nper
         if (.not.drn%sp(iper)%reuse) then
            if (drn%sp(iper)%ldrn .and. .not.drn%sp(iper)%lolf) then
               kper = 0
               do jper = iper, 1, -1 ! find most recent olf
                  if (drn%sp(jper)%lolf) then
                     kper = jper; exit
                  end if
               end do
               if (kper.gt.0) then
                  do iact = 1, 2
                     nsys = drn%sp(iper)%gcd%nsubsys
                     if (iact.eq.2) then
                        do i=1,nsys; subsys(i) = drn%sp(iper)%gcd%subsys(i); enddo
                     end if
                     do isys = 1, drn%sp(kper)%gcd%nsubsys
                        if (drn%sp(kper)%gcd%subsys(isys)%lolf) then ! find olf
                           nsys = nsys + 1
                           if (iact.eq.2) then
                              subsys(nsys) = drn%sp(kper)%gcd%subsys(isys)
                           end if
                        end if
                     end do
                     if (iact.eq.1) then
                        allocate(subsys(nsys))
                     else
                        drn%sp(iper)%gcd%nsubsys = nsys
                        do i=1,nsys; drn%sp(iper)%gcd%subsys(i) = subsys(i); enddo
                        deallocate(subsys)
                     end if
                  end do
               end if
            else if (.not.drn%sp(iper)%ldrn .and. drn%sp(iper)%lolf) then
               kper = 0
               do jper = iper, 1, -1 ! find most recent drn
                  if (drn%sp(jper)%ldrn) then
                     kper = jper; exit
                  end if
               end do
               if (kper.gt.0) then
                  do iact = 1, 2
                     nsys = drn%sp(iper)%gcd%nsubsys
                     if (iact.eq.2) then
                        do i=1,nsys; subsys(i) = drn%sp(iper)%gcd%subsys(i); enddo
                     end if
                     do isys = 1, drn%sp(kper)%gcd%nsubsys
                        if (drn%sp(kper)%gcd%subsys(isys)%ldrn) then ! find drn
                           nsys = nsys + 1
                           if (iact.eq.2) then
                              subsys(nsys) = drn%sp(kper)%gcd%subsys(isys)
                           end if
                        end if
                     end do
                     if (iact.eq.1) then
                        allocate(subsys(nsys))
                     else
                        drn%sp(iper)%gcd%nsubsys = nsys
                        do i=1,nsys; drn%sp(iper)%gcd%subsys(i) = subsys(i); enddo
                        deallocate(subsys)
                     end if
                  end do
               end if
            end if
         end if
      end do

      call writeSPckSPer(lun, nper, drn%sp, pckftype(drn%type))

!...     close drn file
      close(lun)

      return
      end subroutine WriteDrn

      !> Write General-Head Boundary package.
      subroutine WriteGhb()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, iper, isub, n, i, luncb
      character(len=maxlen) :: nstr
      character(len=maxlen), dimension(3) :: str
!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(ighb)%active) return

!...     set slashes
      call osd_s_filename(nam%package(ighb)%fname)

!...     write oc-data
      nam%data(ighbflux)%fname  = 'bdgghb'
      nam%data(ighbflux)%cbnlay = ghb%cbnlay
      nam%data(ighbflux)%cblay  = ghb%cblay

!...     open ghb-file
      call CreateDir(nam%package(ighb)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(ighb)%fname,action='write')

!...     write ghb file
      write(lun,'(a,1x,a)') '#', trim(ghb%text)

!...     write mxactr and ighbcb
      n = 1
      write(str(n),*) ghb%mxactr
      if (drn%cbnlay.gt.0) then
         nam%data(ighbflux)%active = .true.
         luncb = nam%data(ighbflux)%nunit
      else
         luncb = 0
      end if
      n = n + 1
      write(str(n),*) luncb
      if (debugflag.le.0) then
         n = n + 1
         write(str(n),*) 'noprint'
      end if
      write(nstr,*) n
      write(lun,'('//trim(nstr)//'(a,1x))')(trim(adjustl(str(i))), i = 1, n)

      call writeSPckSPer(lun, nper, ghb%sp, pckftype(ghb%type))

!...     close ghb file
      close(lun)

      return
      end subroutine WriteGhb

      !> Write Well package.
      subroutine WriteWel()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, iper, isub, n, i, luncb
      character(len=maxlen) :: nstr
      character(len=maxlen), dimension(4) :: str
!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(iwel)%active) return

!...     set slashes
      call osd_s_filename(nam%package(iwel)%fname)

!...     write oc-data
      nam%data(iwelflux)%fname  = 'bdgwel'
      nam%data(iwelflux)%cbnlay = wel%cbnlay
      nam%data(iwelflux)%cblay  = wel%cblay

!...     open wel-file
      call CreateDir(nam%package(iwel)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(iwel)%fname,action='write')

!...     write wel file
      write(lun,'(a,1x,a)') '#', trim(wel%text)

!...     write mxactr and iwelcb
      n = 1
      write(str(n),*) wel%mxactr
      if (wel%cbnlay.gt.0) then
         nam%data(iwelflux)%active = .true.
         luncb = nam%data(iwelflux)%nunit
      else
         luncb = 0
      end if
      n = n + 1
      write(str(n),*) luncb
      if (debugflag.le.0) then
         n = n + 1
         write(str(n),*) 'noprint'
      end if
      if (wel%wsubsys) then
         n = n + 1
         write(str(n),*) 'wsubsys isub'
      end if

      write(nstr,*) n
      write(lun,'('//trim(nstr)//'(a,1x))')(trim(adjustl(str(i))), i = 1, n)

      call writeSPckSPer(lun, nper, wel%sp, pckftype(wel%type))

!...     close wel file
      close(lun)

      return
      end subroutine WriteWel

      !> Write Horizonal Flow Barrier package.
      subroutine WriteHfb()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, iper, isub, n, i, luncb
      character(len=maxlen) :: nstr
      character(len=maxlen), dimension(7) :: str
!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(ihfb)%active) return

!...     set slashes
      call osd_s_filename(nam%package(ihfb)%fname)

!...     open hfb-file
      call CreateDir(nam%package(ihfb)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(ihfb)%fname,action='write')

!...     write hfb file
      write(lun,'(a,1x,a)') '#', trim(hfb%text)

!...     write nphfb, mxfb, nhfbnp
      n = 1
      write(str(n),*) hfb%nphfb
      n = n + 1
      write(str(n),*) hfb%mxfb
      n = n + 1
      write(str(n),*) hfb%nhfbnp
      if (debugflag.le.0) then
         n = n + 1
         write(str(n),*) 'noprint'
      end if
      if(dis%settop .and. dis%setbot) then
         n = n + 1
         write(str(n),*) 'hfbresis'
      else
         n = n + 1
         write(str(n),*) 'hfbfact'
      end if

      write(nstr,*) n
      write(lun,'('//trim(nstr)//'(a,1x))')(trim(adjustl(str(i))), i = 1, n)

      call writeSPckSPer(lun, 1, hfb%sp, pckftype(hfb%type))
!      call writeSPckSPer(lun, nper, hfb%sp, pckftype(hfb%type))

!...     close hfb file
      close(lun)

      return
      end subroutine WriteHfb

      !> Write Preconditioned Conjugate-Gradient package.
      subroutine WritePcg()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, i, ilay, laycon
      character(len=maxlen), dimension(7) :: str
      character(len=maxlen) :: nlaystr
!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(ipcg)%active) return

!...     set slashes
      call osd_s_filename(nam%package(ipcg)%fname)

!...     open pcg-file
      call CreateDir(nam%package(ipcg)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(ipcg)%fname,action='write')

!...     write pcg file
      write(lun,'(a,1x,a)') '#', trim(pcg%text)
      write(str(1),*) pcg%mxiter
      write(str(2),*) pcg%iter1
      write(str(3),*) pcg%npcond
      write(lun,'(3(a,1x))')(trim(adjustl(str(i))), i = 1, 3) ! mxiter iter1 npcond
      write(str(1),*) pcg%hclose
      write(str(2),*) pcg%rclose
      write(str(3),*) pcg%relax
      write(str(4),*) pcg%nbpol
      write(str(5),*) pcg%iprpcg
      write(str(6),*) pcg%mutpcg
      write(str(7),*) pcg%damp
      write(lun,'(7(a,1x))')(trim(adjustl(str(i))), i = 1, 7) ! hclose rclose relax nbpol iprpcg mutpcg damp

!...     close pcg file
      close(lun)

      return
      end subroutine WritePcg

      !> Write Parallel Krylov Solver package.
      subroutine WritePks()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, i, iproc
      character(len=maxlen), dimension(:), allocatable :: str
      character(len=maxlen) :: nstr
!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(ipks)%active) return

      allocate(str(4))
      
!...     set slashes
      call osd_s_filename(nam%package(ipks)%fname)

!...     open pks-file
      call CreateDir(nam%package(ipks)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(ipks)%fname,action='write')

!...     write pks file
      write(lun,'(a,1x,a)') '#', trim(pks%text)
      write(str(1),'(a)') 'isolver'; write(str(2),*) pks%isolver 
      write(lun,'(a,1x,a)')(trim(adjustl(str(i))),i=1,2) 
      write(str(1),'(a)') 'npc';     write(str(2),*) pks%npc
      write(lun,'(a,1x,a)')(trim(adjustl(str(i))),i=1,2) 
      write(str(1),'(a)') 'hclosepks'; write(str(2),*) pks%hclosepks 
      write(lun,'(a,1x,a)')(trim(adjustl(str(i))),i=1,2) 
      write(str(1),'(a)') 'rclosepks'; write(str(2),*) pks%rclosepks 
      write(lun,'(a,1x,a)')(trim(adjustl(str(i))),i=1,2) 
      write(str(1),'(a)') 'mxiter'; write(str(2),*) pks%mxiter
      write(lun,'(a,1x,a)')(trim(adjustl(str(i))),i=1,2) 
      write(str(1),'(a)') 'innerit'; write(str(2),*) pks%innerit
      write(lun,'(a,1x,a)')(trim(adjustl(str(i))),i=1,2) 
      write(str(1),'(a)') 'relax'; write(str(2),*) pks%relaxpks 
      write(lun,'(a,1x,a)')(trim(adjustl(str(i))),i=1,2) 
      if (pks%nrproc > 1) then
         write(str(1),'(a)') 'partopt'; write(str(2),*) 3 
         write(lun,'(a,1x,a)')(trim(adjustl(str(i))),i=1,2)
         write(lun,'(a)') 'partdata'
         write(str(1),*) pks%nrproc
         write(lun,'(a)') trim(adjustl(str(1)))
         do iproc = 1, pks%nrproc
            write(str(1),*) pks%partminmax(iproc,1)  
            write(str(2),*) pks%partminmax(iproc,2)  
            write(str(3),*) pks%partminmax(iproc,3)  
            write(str(4),*) pks%partminmax(iproc,4)  
            write(lun,'(3(a,1x),1x,a)')(trim(adjustl(str(i))),i=1,4) 
         end do
         write(str(1),*) dis%nrow
         write(lun,'(a,1x,a)') 'gnrow', trim(adjustl(str(1)))    
         write(str(1),*) dis%ncol
         write(lun,'(a,1x,a)') 'gncol', trim(adjustl(str(1)))  
         write(lun,'(a)') 'gdelr' 
         if (minval(dis%delr) == maxval(dis%delr)) then
             write(str(1),*) dis%delr(1)
             write(lun,'(a)') trim(adjustl(str(1)))  
         else
             deallocate(str)
             allocate(str(dis%ncol))
             do i = 1, dis%ncol
                write(str(i),*) dis%delr(i)    
             end do
             write(nstr,*) dis%ncol
             write(lun,'('//trim(nstr)//'(a,1x))')(trim(adjustl(str(i))), i = 1, dis%ncol)
         end if
         write(lun,'(a)') 'gdelc' 
         if (minval(dis%delc) == maxval(dis%delc)) then
             write(str(1),*) dis%delc(1)
             write(lun,'(a)') trim(adjustl(str(1)))  
         else
             deallocate(str)
             allocate(str(dis%nrow))
             do i = 1, dis%nrow
                write(str(i),*) dis%delc(i)    
             end do
             write(nstr,*) dis%nrow
             write(lun,'('//trim(nstr)//'(a,1x))')(trim(adjustl(str(i))), i = 1, dis%nrow)
         end if
      end if      
      if (pks%pressakey.eq.1) then
         write(lun,'(a)') 'pressakey'
      end if       
      write(lun,'(a)') 'end'

!...     close pks file
      close(lun)

      return
      end subroutine WritePks
      
       !> Write Recharge package.
      subroutine WriteRch()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, iper, n, i, luncb
      character(len=maxlen), dimension(3) :: str
!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(irch)%active) return

!...     set slashes
      call osd_s_filename(nam%package(irch)%fname)

!...     write oc-data
      nam%data(irchflux)%fname  = 'bdgrch'
      nam%data(irchflux)%cbnlay = rch%cbnlay
      nam%data(irchflux)%cblay  = rch%cblay

!...     open rch-file
      call CreateDir(nam%package(irch)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(irch)%fname,action='write')

!...     write rch file
      write(lun,'(a,1x,a)') '#', trim(rch%text)

!...     write nrchop and irchcb
      write(str(1),*) rch%nrchop
      if (rch%cbnlay.gt.0) then
         nam%data(irchflux)%active = .true.
         luncb = nam%data(irchflux)%nunit
      else
         luncb = 0
      end if
      write(str(2),*) luncb
      write(str(3),'(a)') 'addrech'
      write(lun,'(3(a,1x))')(trim(adjustl(str(i))), i = 1, 3)

      do iper = 1, nper
         if (rch%sp(iper)%reuse) then
            if (iper == 1) then
               write(*,'(3(a,1x))') 'Error: please specify first stress period for rch package.'
               stop 1
            end if
            write(lun,'(a)') '-1'
         else
            write(str(1),*) rch%sp(iper)%inrech
            write(str(2),*) rch%sp(iper)%inirch
            write(lun,'(2(a,1x))')(trim(adjustl(str(i))), i = 1, 2)
            if (rch%sp(iper)%inrech.gt.0) then
               do i = 1, rch%sp(iper)%inrech
                  call WriteArrayRead(rch%sp(iper)%rech(i),lun)
               end do
            elseif (rch%sp(iper)%inrech.eq.0) then
             write(str(1),*) 'constant'
             write(str(2),*) 0.0
             write(lun,'(2(a,1x))')(trim(adjustl(str(i))), i = 1, 2)
            end if
            if (rch%nrchop == 2 .and. rch%sp(iper)%inirch >= 0) then
               call WriteArrayRead(rch%sp(iper)%irch,lun)
            end if
         end if
      end do

!...     close rch file
      close(lun)

      return
      end subroutine WriteRch

       !> Write Evapotranspiration package.
      subroutine WriteEvt()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, iper, n, i, luncb
      character(len=maxlen), dimension(3) :: str
!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(ievt)%active) return

!...     set slashes
      call osd_s_filename(nam%package(ievt)%fname)

!...     write oc-data
      nam%data(ievtflux)%fname  = 'bdgevt'
      nam%data(ievtflux)%cbnlay = evt%cbnlay
      nam%data(ievtflux)%cblay  = evt%cblay

!...     open rch-file
      call CreateDir(nam%package(ievt)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(ievt)%fname,action='write')

!...     write rch file
      write(lun,'(a,1x,a)') '#', trim(evt%text)

!...     write nrchop and irchcb
      write(str(1),*) evt%nevtop
      if (evt%cbnlay.gt.0) then
         nam%data(ievtflux)%active = .true.
         luncb = nam%data(ievtflux)%nunit
      else
         luncb = 0
      end if
      write(str(2),*) luncb
      write(lun,'(2(a,1x))')(trim(adjustl(str(i))), i = 1, 2)

      do iper = 1, nper
         if (evt%sp(iper)%reuse) then
            if (iper == 1) then
               write(*,'(3(a,1x))') 'Error: please specify first stress period for evt package.'
               stop 1
            end if
            write(lun,'(a)') '-1 -1 -1'
         else
            write(str(1),*) evt%sp(iper)%insurf
            write(str(2),*) evt%sp(iper)%inevtr
            write(str(3),*) evt%sp(iper)%inexdp
            write(lun,'(3(a,1x))')(trim(adjustl(str(i))), i = 1, 3)
            if(evt%sp(iper)%insurf.ne.0.and.evt%sp(iper)%inevtr.ne.0.and.evt%sp(iper)%inexdp.ne.0)then
            if (evt%sp(iper)%insurf >= 0) then
               call WriteArrayRead(evt%sp(iper)%surf,lun)
            end if
            if (evt%sp(iper)%inevtr >= 0) then
               call WriteArrayRead(evt%sp(iper)%evtr,lun)
            end if
            if (evt%sp(iper)%inexdp >= 0) then
               call WriteArrayRead(evt%sp(iper)%exdp,lun)
            end if
            else
             write(str(1),*) 'constant'
             write(str(2),*) 0.0
             write(lun,'(2(a,1x))')(trim(adjustl(str(i))), i = 1, 2)
             write(lun,'(2(a,1x))')(trim(adjustl(str(i))), i = 1, 2)
             write(lun,'(2(a,1x))')(trim(adjustl(str(i))), i = 1, 2)
            endif
         end if
      end do

!...     close evt file
      close(lun)

      return
      end subroutine WriteEvt

      !> Write Constant Head package.
      subroutine WriteChd()

      implicit none

!...     locals
      integer :: cfn_getlun, lun, iper, isub, jsub, n, i, luncb, ilay, jlay, flag(nlay)
      character(len=maxlen) :: nstr
      character(len=maxlen), dimension(4) :: str
      logical :: linterp, lfound
      type(tArrayRead), pointer :: dataptr
!.......................................................................

!...     return in case package is not active
      if (.not.nam%package(ichd)%active) return

!...     set slashes
      call osd_s_filename(nam%package(ichd)%fname)

!...     open chd-file
      call CreateDir(nam%package(ichd)%fname)
      lun = cfn_getlun(10,99)
      open(unit=lun,file=nam%package(ichd)%fname,action='write')

!...     write chd file
      write(lun,'(a,1x,a)') '#', trim(chd%text)

!...    check for factor usage
      linterp = .false.
      do iper = 1, nper
         if (.not.chd%sp(iper)%reuse) then
            n = chd%sp(iper)%gcd%nsubsys
            if (n.gt.nlay) linterp = .true.
         end if   
      end do
      if (linterp) then
         do iper = 1, nper
            if (.not.chd%sp(iper)%reuse) then
               n = chd%sp(iper)%gcd%nsubsys
               if (n.gt.nlay) then
                  flag = 0
                  do isub = 1, n
                     ilay = chd%sp(iper)%gcd%subsys(isub)%ilay
                     if (flag(ilay).eq.0) then
                        lfound = .false.
                        do jsub = 1, n
                           if (jsub.ne.isub .and. chd%sp(iper)%gcd%subsys(jsub)%ilay.eq.ilay) then
                              chd%sp(iper)%gcd%subsys(jsub)%active = .false. 
                              dataptr => chd%sp(iper)%gcd%subsys(jsub)%data(1)
                              lfound = .true.; exit
                           end if
                        end do
                        if (.not.lfound) then
                           chd%sp(iper)%gcd%subsys(isub)%data(2)%keyword='constant'
                           chd%sp(iper)%gcd%subsys(isub)%data(2)%cnstnt=0.0
                        else
                           chd%sp(iper)%gcd%subsys(isub)%data(2) = dataptr
                           flag(ilay) = 1
                        end if   
                     end if   
                  end do
               else
                  do isub = 1, n
                     chd%sp(iper)%gcd%subsys(isub)%data(2)%keyword='constant'
                     chd%sp(iper)%gcd%subsys(isub)%data(2)%cnstnt=0.0
                  end do                   
               end if    
           end if    
         end do    
      end if
         
!...     write mxactc
      n = 1
      write(str(n),*) chd%mxactc
      if (debugflag.le.0) then
         n = n + 1
         write(str(n),*) 'noprint'
      end if   
      if (chd%negbnd) then
         n = n + 1
         write(str(n),*) 'negbnd'
      end if
      if(linterp) then
         n = n + 1
         write(str(n),*) 'interp'
      end if          
      write(nstr,*) n
      write(lun,'('//trim(nstr)//'(a,1x))')(trim(adjustl(str(i))), i = 1, n)
      call writeSPckSPer(lun, nper, chd%sp, pckftype(chd%type))

!...     close chd file
      close(lun)

      return
      end subroutine WriteChd

      !> Write Stress Package Stress Period input
      subroutine writeSPckSPer(lun, nper, sp, pckname)

      implicit none

!...     arguments
      integer, intent(in) :: lun
      integer, intent(in) :: nper
      type(tSpGcdLcd), dimension(nper), intent(in) :: sp
      character(len=4) :: pckname

!...     locals
      integer :: iper
      character(len=4) :: ext
      character(len=maxlen) :: str
      type(tArrayRead) :: arr
      character(len=maxlen) :: fname
!.......................................................................

      do iper = 1, nper
         if (sp(iper)%reuse) then
            if (iper == 1) then
               write(*,'(3(a,1x))') 'Error: please specify first stress period for', trim(pckname), 'package.'
               stop 1
            end if
            write(lun,'(a,1x,i5.5)') '-1',iper
         else
            if (sp(iper)%usegcd) then
               if(sp(iper)%gcd%nsubsys.gt.0)then
               write(str,*) sp(iper)%np
               write(lun,'(a)') trim(adjustl(str))
               write(lun,'(a,1x,i5.5)') 'gcd',iper
               call WriteGcd(sp(iper)%gcd, lun)
               else
                write(lun,'(a)') '0'
               endif
            else if (sp(iper)%uselcd) then
               write(lun,'(a,1x,i5.5)') 'lcd',iper
               call WriteLcd(sp(iper)%lcd, lun)
            end if
         end if
      end do

      return
      end subroutine writeSPckSPer

      !> Write array file.
      subroutine WriteArrayRead(arr, lun)

      implicit none

!...     arguments
      type(tArrayRead), intent(in) :: arr
      integer, intent(in) :: lun

!...     functions
      integer :: cfn_length

!...     locals
      logical :: lfct, limp, lpow
      integer :: i
      real(KIND=8) :: val
      character(len=20) :: keyword
      character(len=maxlen), dimension(8) :: str
      character(len=maxlen) :: cnstnt, fstr
!.......................................................................

      lfct = .false.; limp = .false.; lpow = .false.
      if (arr%fct/= 1.0D0) lfct = .true.
      if (arr%imp/= 0.0D0) limp = .true.
      if (arr%pow/= 0.0D0) lpow = .true.

      keyword = adjustl(arr%keyword)
      cnstnt = ''
      select case (keyword)
         case ('open/close')
            fstr = arr%fname
            if (cfn_length(fstr) == 0) then
               return
            end if
            !...     set slashes
            call osd_s_filename(fstr)
            cnstnt='fct='//trim(cnstnt)
            call AppendVal(cnstnt,arr%fct,arr%type)
            if (limp) then
             cnstnt=trim(cnstnt)//'_imp='
             call AppendVal(cnstnt,arr%imp,arr%type)
!               if (arr%imp.gt.0.) then
!                  cnstnt = trim(cnstnt)//'+'
!                  call AppendVal(cnstnt,arr%imp,arr%type)
!               else
!                  cnstnt = trim(cnstnt)//'-'
!                  call AppendVal(cnstnt,-arr%imp,arr%type)
!               end if
            end if
            if (lpow) then
             cnstnt = trim(cnstnt)//'_pow='
             call AppendVal(cnstnt,arr%pow,arr%type)
            end if
            !cnstnt = trim(cnstnt)//trim(arr%oper)
            ! replace relative dot for one level deeper
            call imod_utl_rel_to_abs(root%runfileroot,fstr)
            ! write string
            write(str(1),*) trim(keyword)
            write(str(2),*) "'"//trim(fstr)//"'"
            write(str(3),*) trim(cnstnt)
            write(str(4),*) trim(arr%fmtin)
            write(str(5),*) arr%iprn
            write(str(6),*) 'scaling'
            write(str(7),*) arr%iuscl
            write(str(8),*) arr%idscl
            write(lun,'(8(a,1x))')(trim(adjustl(str(i))), i = 1, 8)
         case ('constant')
            val = arr%cnstnt
            if (lfct) val = val*arr%fct
            if (limp) val = val + arr%imp
            if (lpow) val = val ** arr%pow
            call AppendVal(cnstnt,val,arr%type)
            !cnstnt = trim(cnstnt)//trim(arr%oper)
            ! write string
            write(str(1),*) trim(keyword)
            write(str(2),*) trim(cnstnt)
            write(lun,'(2(a,1x))')(trim(adjustl(str(i))), i = 1, 2)
         case default
             write(*,*) 'Error: unsupported keyword for writing array.'
             stop 1
      end select

      return
      end subroutine WriteArrayRead

      subroutine AppendVal(str,val,typ)
      implicit none

!...     arguments
      character(len=*), intent(inout) :: str
      real(kind=8), intent(in) :: val
      integer, intent(in) :: typ

!...     locals
      character(len=maxlen) :: valstr
!.......................................................................

      valstr = ''
      if (typ == iarr) then
         write(valstr,*) val
      else
         write(valstr,*) int(val)
      end if
      str = trim(str)//trim(adjustl(valstr))

      end subroutine AppendVal

      subroutine WriteGcd(gcd, lun)

      implicit none

!...     arguments
      type(tGcd), intent(in) :: gcd
      integer, intent(in) :: lun

!...     functions
      integer :: cfn_getlun

!...     locals
      integer :: isub, jsub, icol, i, nsubsys
      character(len=maxlen), dimension(5) :: str
      logical :: lrenum
!.......................................................................

!...     open gcd file
      nsubsys = 0; lrenum = .false.
      do isub = 1, gcd%nsubsys
         if (gcd%subsys(isub)%active) nsubsys = nsubsys + 1
      end do  
      if (gcd%nsubsys.ne.nsubsys) lrenum = .true.
      
      write(str(1),*) nsubsys
      write(lun,'(a)') trim(adjustl(str(1)))
      jsub = 0
      do isub = 1, gcd%nsubsys
         if (.not.gcd%subsys(isub)%active) cycle     
         jsub = jsub + 1
         write(str(1),*) gcd%subsys(isub)%ilay
         if (lrenum) then
            write(str(2),*) jsub
         else   
            write(str(2),*) gcd%subsys(isub)%isub
         end if   
         write(lun,'(2(a,1x))')(trim(adjustl(str(i))), i = 1, 2)
         do icol = 1, gcd%ncolumns
            call WriteArrayRead(gcd%subsys(isub)%data(icol),lun) ! column data
         end do ! icol
      end do ! isub

      return
      end subroutine WriteGcd

      subroutine WriteLcd(lcd, lun)

      implicit none

!...     arguments
      type(tLcd), intent(in) :: lcd
      integer, intent(in) :: lun

!...     functions
      integer :: cfn_getlun

!...     locals
      integer :: igen, icol, i
      character(len=maxlen), dimension(2) :: str
!.......................................................................

      write(str(1),*) lcd%ngen
      write(lun,'(a)') trim(adjustl(str(1)))
      do igen = 1, lcd%ngen
         write(str(1),*) lcd%gen(igen)%ilay
         write(str(2),*) lcd%gen(igen)%factor
         write(lun,'(2(a,1x))')(trim(adjustl(str(i))), i = 1, 2)
         call WriteArrayRead(lcd%gen(igen)%data(1),lun) ! column data
      end do ! igen

      return
      end subroutine WriteLcd

      !> Create directory wrapper routine.
      subroutine CreateDir(file)
      implicit none

 !...     arguments
      character(len=*), intent(in) :: file

!...     functions
      integer :: osd_fsplit

!...     locals
      integer :: ios
      character(len=300) :: dir, name
!.......................................................................

      ios = osd_fsplit(file,dir,name)
      if (ios.ne.0) then
         write(*,*) 'ERROR. File splitting ipf.'
         stop 1
      end if
      call osd_mkdir(dir,ios)

!      if (ios.ne.0) then
!         write(*,*) 'ERROR. Creating directory.'
!         stop 1
!      end if

      end subroutine CreateDir

      end module rf2mf_module