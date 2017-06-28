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

      module gwfscrmodule
        integer, save,pointer                       ::nstptotscr  !> Total number of time steps (this is the sum of all time steps in all stress periods
                                                                  !> excluding steady state; if exist)
        integer, save,pointer                       :: iscrcb     !> iscrcb is a flag and unit number to store cell-by-cell flow terms
        integer, save,pointer                       :: iscroc     !> iscroc is number of repetition of format lines to be used to store
                                                                  !> subsidence, compaction, vertical displacement, preconsoloidation
                                                                  !> stress, change in change in preconsolidation stress, geostatic stress,
                                                                  !> change in geostatic stress, effective stress, change in effective stress,
                                                                  !> void ratio, thickness of interbeds, and layer-center elevation.
        integer, save,pointer                       :: nsystm     !> nsystm is the number of systems of interbeds
        integer, save,pointer                       :: ithk       !> ithk is a flag to determine how thicknesses of compressible sediments
                                                                  !> vary in  response to changes in saturated thickness.
        integer, save,pointer                       :: ivoid      !> ivoid is a flag to determine how void ratios of compressible sediments vary in
                                                                  !> response to changes in saturated thickness.
        integer, save,pointer                       :: istpcs     !> istpcs is a flag to determine how initial preconsolidation stress will be obtained.
!        integer, save,pointer                       :: isincr     !> isincr is a flag to indicate incremental run or not.
        integer, save,pointer                       :: imethod    !> imethod is a flag to determine which method to use in calculating settlement (subsidence).
                                                                  !> = 1 isotache
                                                                  !> = 2 bjerrum
        real,    save,pointer                       :: alpha      !> relaxation factor to calculate flux due to subsidence
        integer, save,pointer                       :: izcfl      !> a flag to specify whether or not initial calculated values of layer-center elevation will
                                                                  !> be printed
        integer, save,pointer                       :: izcfm      !> a code for the format in which layer-center elevation will be printed.
        integer, save,pointer                       :: iglfl      !> a flag to specify whether or not initial calculated values of geostatic stress will be printed.
        integer, save,pointer                       :: iglfm      !> a code for the format in which geostatic stress will be printed.
        integer, save,pointer                       :: iestfl     !> a code for the format in which effective stress will be printed.
        integer, save,pointer                       :: iestfm     !> a code for the format in which effective stress will be printed.
        integer, save,pointer                       :: ipcsfl     !> a flag to specify whether or not initial calculated values of preconsolidation stress will
                                                                  !> be printed.
        integer, save,pointer                       :: ipcsfm     !> a code for the format in which preconsolidation stress will be printed.
        integer, save,pointer                       :: nobssub    !> number of observation subsidence points
c
        integer, save,    dimension(:),     pointer :: iswocf     !> formats of output control items (13 in total)
        integer, save,    dimension(:),     pointer :: iswocu     !> a flag and unit number for output control items
        integer, save,    dimension(:),     pointer :: ifl2       !> used to control printing and saving of information generated by the package during program
                                                                  !> execution.
        logical, save,    dimension(:),     pointer :: oclay2     !> a flag to indicate to sum compaction in each layer in the buff array for saving or printing
                                                                  !> compaction or vertical displacement by modellayer.
        integer, save,    dimension(:),     pointer :: lnwt       !> a one-dimensional array specifying the model-layer assignments for each system of interbeds.
                                                                  !> The array has NSYSTM values.
        integer, save,    dimension(:),     pointer :: ntssm2     !> number of time steps before current stress time step? nper
        logical, save,    dimension(:,:),   pointer :: ocflg2     !> output control flags
        real,    save,    dimension(:,:),   pointer :: sgs        !> specific gravity of saturated sediments
        real,    save,    dimension(:,:),   pointer :: sgm        !> specific gravity of moist sediments

        real,    save,    dimension(:,:,:), pointer :: pcsoff     !> offset of initial preconsolidation stress from initial effective stress (overburden pressure)
        real,    save,    dimension(:,:,:), pointer :: thick      !> thickness of interbeds in saturated interval
        real,    save,    dimension(:,:,:), pointer :: isoarr     !> recompression index
        real,    save,    dimension(:,:,:), pointer :: isobcr     !> compression index
        real,    save,    dimension(:,:,:), pointer :: isocca     !> secondary compression index
        real,    save,    dimension(:,:,:,:), pointer :: subtime  !> compaction
c
        !real,    save,    dimension(:),     pointer :: dblold     !> comaction of a single cell of all times
        real,    save,    dimension(:,:,:), pointer :: sub        !> !sub will be used to calculate total subsidence
                                                                  !> for all systems of all cells and times: sub(j,i,isys,it)
        real,    save,    dimension(:,:,:), pointer :: void       !> void ratio
        real,    save,    dimension(:,:,:), pointer :: est        !> effective stress
        real,    save,    dimension(:,:,:), pointer :: estold     !> effective stress for previous time step
        real,    save,    dimension(:,:,:), pointer :: gl         !> geostatic stress
        real,    save,    dimension(:,:,:), pointer :: zc         !> layer center
        real,    save,    dimension(:,:,:), pointer :: pcs0       !> initial preconsolidation stress
        real,    save,    dimension(:,:,:), pointer :: pcs        !> preconsolidation stress
        real,    save,    dimension(:,:,:), pointer :: pcsold     !> old (previous time step) preconsolidation stress
        real*8,  save,    dimension(:,:,:), pointer :: oIntegralEs !> old integral part of the secondary strains
        real*8,  save,    dimension(:,:,:), pointer :: nIntegralEs !> new integral part of the secondary strains
        real*8,  save,    dimension(:,:,:), pointer :: epTotal    !> cumulative primary strain
        real,    save,    dimension(:,:,:), pointer :: gl0        !> initial geostatic stress
        real,    save,    dimension(:,:,:), pointer :: est0       !> initial effective stress
c
c calculation times for all time steps and per stress period
        real,    save,    dimension(:),     pointer :: totimscr   !> total elapsed time since simulation started
        real,    save,    dimension(:),     pointer :: pertimscr  !> elapsed time within a stress period
        real,    save,    dimension(:),     pointer :: deltallscr !> time steps lengths (excluding steady state period)
        integer, save,    dimension(:,:),   pointer :: iddeltscr  !> this is to map between mf (stress period, timestep) and scr time indices
        real,  save,    dimension(:,:,:,:), pointer :: qscr       !> the volumetric rate of flow to or from compressible interbeds due to compacation
c
c subsidence observation points: for now they are used for tracing
        integer, save,    dimension(:),     pointer :: irowobssub !> Rows IDs for land subsidence observations
        integer, save,    dimension(:),     pointer :: icolobssub !> Columns IDS for land subsidence observations
C
C Type definition
c
        type gwfscrtype

          integer,pointer                           :: nstptotscr
          integer,pointer                           :: iscrcb
          integer,pointer                           :: iscroc
          integer,pointer                           :: nsystm
          integer,pointer                           :: ithk
          integer,pointer                           :: ivoid
          integer,pointer                           :: istpcs
!          integer,pointer                           :: isincr
          integer,pointer                           :: imethod
          real,pointer                              :: alpha
          integer,pointer                           :: izcfl
          integer,pointer                           :: izcfm
          integer,pointer                           :: iglfl
          integer,pointer                           :: iglfm
          integer,pointer                           :: iestfl
          integer,pointer                           :: iestfm
          integer,pointer                           :: ipcsfl
          integer,pointer                           :: ipcsfm
c
          integer,    dimension(:),     pointer     :: iswocf
          integer,    dimension(:),     pointer     :: iswocu
          integer,    dimension(:),     pointer     :: ifl2
          logical,    dimension(:),     pointer     :: oclay2
          integer,    dimension(:),     pointer     :: lnwt
          integer,    dimension(:),     pointer     :: ntssm2
          logical,    dimension(:,:),   pointer     :: ocflg2
          real,       dimension(:,:),   pointer     :: sgs
          real,       dimension(:,:),   pointer     :: sgm
          real,       dimension(:,:,:), pointer     :: pcs
          real,       dimension(:,:,:), pointer     :: pcsoff
          real,       dimension(:,:,:), pointer     :: thick
          real,       dimension(:,:,:), pointer     :: isoarr !ce
          real,       dimension(:,:,:), pointer     :: isobcr !ci
          real,       dimension(:,:,:), pointer     :: isocca
!sub will be used to calculate total subsidence for all systems of all cells and times: sub(j,i,isys)
          real,       dimension(:,:,:), pointer     :: sub
          real,       dimension(:,:,:), pointer     :: void
          real,       dimension(:,:,:), pointer     :: est
          real,       dimension(:,:,:), pointer     :: estold
          real,       dimension(:,:,:), pointer     :: gl
          real,       dimension(:,:,:), pointer     :: zc
          real,       dimension(:,:,:), pointer     :: pcs0
          real,       dimension(:,:,:), pointer     :: pcsold
          real*8,     dimension(:,:,:), pointer     :: oIntegralEs
          real*8,     dimension(:,:,:), pointer     :: nIntegralEs
          real*8,     dimension(:,:,:), pointer     :: epTotal
          real,       dimension(:,:,:), pointer     :: gl0
          real,       dimension(:,:,:), pointer     :: est0
!sub will be used to calculate total subsidence for all systems of all cells and times: sub(j,i,isys,it)
          real,       dimension(:,:,:,:), pointer   :: subtime
c comaction of a single cell of all times
!          real,       dimension(:), pointer         :: dblold
c
          real,       dimension(:),       pointer   :: totimscr
          real,       dimension(:),       pointer   :: pertimscr
          real,       dimension(:),       pointer   :: deltallscr
          integer,    dimension(:,:),     pointer   :: iddeltscr
          real*8,     dimension(:,:,:,:), pointer   :: qscr !(j,i,kq,it)

          integer,                        pointer   :: nobssub
          integer,    dimension(:),       pointer   :: irowobssub
          integer,    dimension(:),       pointer   :: icolobssub
        end type gwfscrtype

        type(gwfscrtype), save                      :: gwfscrdat(10)
      end module gwfscrmodule
c
c subr 1
      subroutine gwf2scr7ar(in,igrid)
c     ******************************************************************
c     allocate array storage for subsidence-creep and read and
c     prepare data
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      use global,      only: ncol,nrow,nlay,nper,issflg,nstp,laycbd,
     1                       ibound,hnew,botm,buff,delr,delc,iout,ncnfbd

      use gwfscrmodule,only: iscrcb,iscroc,ithk,ivoid,nsystm,
     &                       ntssm2,istpcs,lnwt,thick,isoarr,isobcr,
     &                       isocca,sub,void,pcs,pcs0,pcsoff,est,est0,
     &                       estold,gl,gl0,zc,sgm,sgs,ocflg2,
     &                       oclay2,izcfl,izcfm,iglfl,iglfm,iestfl,
     &                       iestfm,ipcsfl,ipcsfm, !istfl,istfm,
     &                       iswocf,iswocu,ifl2,totimscr,nstptotscr,
     &                       deltallscr,pertimscr,iddeltscr,qscr,
     &                       subtime,imethod,alpha, !isincr,
     &                       nobssub,icolobssub,irowobssub,
     &                       pcsold,nIntegralEs,oIntegralEs,epTotal

      implicit none
      integer                                 :: in
      integer                                 :: igrid
      integer                                 :: nstpt
      integer                                 :: ns
      integer                                 :: iloc,lloc
      integer                                 :: istart
      integer                                 :: istop
      integer                                 :: i,j,k
      integer                                 :: ic,ir
      integer                                 :: jc
      integer                                 :: kk
      integer                                 :: ncolin
      integer                                 :: is,it
      integer                                 :: nij
      integer                                 :: kq
      integer                                 :: n
      integer                                 :: isp1,isp2
      integer                                 :: jts1,jts2
      integer                                 :: j1,j2
      integer                                 :: iocr,noclin
      real                                    :: bt,tp,thick1
      real                                    :: r

      character*200                           :: line
      character*24                            :: aname(17),tmpnam
      character*16                            :: text(4)
c
      data aname /' silt and clay thickness',
     1    'elastic specific storage',
     1    'inelas. specific storage',
     1    '              void ratio',
     1    '       preconsol. stress',
     1    '        geostatic stress',
     1    '   elev. of layer center',
     1    '     starting compaction',
     1    '   elev. of land surface',
     1    '  moist specific gravity',
     1    '   sat. specific gravity',
     1    ' iso recompression index',
     1    '   iso compression index',
     1    '   iso sec. compr. index',
     1    '  bjerrum recompr. index',
     1    '    bjerrum compr. index',
     1    'bjerrum sec. compr.index'/

!      data aname(1) /'   silt and clay thickness'/
!      data aname(2) /'  elastic specific storage --- not used'/
!      data aname(3) /'  inelas. specific storage --- not used'/
!      data aname(4) /'                void ratio'/
!      data aname(5) /'       preconsol.   stress'/
!      data aname(6) /'          geostatic stress'/
!      data aname(7) /'     elev. of layer center'/
!      data aname(8) /'       starting compaction'/
!      data aname(9) /'     elev. of land surface'/
!      data aname(10)/'    moist specific gravity'/
!      data aname(11)/'     sat. specific gravity'/
!
!      data aname(12)/'   iso recompression index'/
!      data aname(13)/'  iso    compression index'/
!      data aname(14)/'iso sec. compression index'/
!      data aname(15)/'      bjerrum recompression index'/
!      data aname(16)/'        bjerrum compression index'/
!      data aname(17)/'   bjerrum sec. compression index'/

      data text(1) /'center elevation'/,
     &     text(2) /'geostatic stress'/,
     &     text(3) /'effective stress'/,
     &     text(4) /'preconsol stress'/
c     ------------------------------------------------------------------
      allocate(iscrcb,iscroc,nsystm,nobssub,ithk,ivoid,istpcs,izcfl,
     &         izcfm,iglfl,iglfm,iestfl,iestfm,ipcsfl,ipcsfm, !istfl,istfm,isincr,
     &         imethod,alpha)
      allocate(iswocf(13),iswocu(13),ifl2(26))
c
c1------identify package.
      write(iout,1)in
    1 format('scr -- subsidence for creep package, version 1,',
     1     ' 05/26/06',' input read from unit',i3)
c
c2------check to see that subsidence option is appropriate
c2------if inappropriate print a message & stop the simulation.
c2------also, sum to get the total number of time steps in the
c2------simulation.
c
      nstpt=0
      do ns=1,nper
         nstpt=nstpt+nstp(ns)
         if(issflg(ns).ne.0.and.ns.gt.1) then
            write(iout,10)
   10       format(1x,'subsidence cannot be used in simulations',
     &      ' in which stress periods other than the ',/,1x,
     &      ' first are steady-state. simulation aborted.')
            call ustop(' ')
         endif
      enddo
c
c3------check that there are no quasi-3d confining beds
      if(ncnfbd.gt.0) then
         write(iout,45)
   45    format(' stopping -- quasi-3d confining beds cannot ',/,
     &     'be used with scr')
         call ustop(' ')
      endif
c
c ------allocate space for array ntssm2, which will contain the total
c ------number of time steps prior to the current time step.
      allocate(ntssm2(nper))
c
c4------read flag for storing cell-by-cell storage changes and
c4------flag for printing and storing compaction, subsidence, and
c4------critical head arrays.
      call urdcom(in,iout,line)
c     read(in,'(a)') line
      lloc=1
      call urword(line,lloc,istart,istop,2,iscrcb,r,iout,in)
      call urword(line,lloc,istart,istop,2,iscroc,r,iout,in)              !rw change from isuboc
      call urword(line,lloc,istart,istop,2,nsystm,r,iout,in)
      call urword(line,lloc,istart,istop,2,nobssub,r,iout,in)
c      call urword(line,lloc,istart,istop,2,igl,r,iout,in)
      call urword(line,lloc,istart,istop,2,ithk,r,iout,in)
      call urword(line,lloc,istart,istop,2,ivoid,r,iout,in)
      call urword(line,lloc,istart,istop,2,imethod,r,iout,in)              !sl added flag
      call urword(line,lloc,istart,istop,2,istpcs,r,iout,in)              !sl added flag
c
!      call urword(line,lloc,istart,istop,2,isincr,r,iout,in)
c
      call urword(line,lloc,istart,istop,3,n,alpha,iout,in)
      if (alpha.lt.0 .or. alpha.gt.1) then
         write(iout,104) alpha
         call ustop(' ')
  104    format(1x,' alpha should be between 0 and 1; alpha = ',
     &          f10.4)
      endif

c4------read flag for storing cell-by-cell storage changes and
c4------flag for printing and storing compaction, subsidence, and
c4------critical head arrays.
c
c5------if cell-by-cell terms to be saved then print unit number.
      if(iscrcb.gt.0) write(iout,105) iscrcb
  105 format(1x,'cell-by-cell flow terms will be saved on unit',i3)
c
c5a-----if output control for printing arrays is selected print message.
      if(iscroc.gt.0) write(iout,106) iscroc
  106 format(1x,i4,' output control records for sub-cr package will',
     1 ' be read.')
c5a-----if output control for printing arrays is selected print message.
c      if(iscroc.gt.0) write(iout,107) iscroc
c  107 format(1x,i4,'output control records for scr package will be ',
c     1 'read.')
c5b-----print number of interbed systems
      write(iout,50) nsystm
   50 format(/,'     number of systems of interbeds for wt subsidence:',
     1 i3)
c
c5c-----print message on how geostatic load is treated.              !rw remove igl logic
c      write(iout,107)
c 107  format(1x,'geostatic load for ibs3 package will be ',
c    1 'read with u2drel.')
c
c6b-----print a message on how thickness is treated
      if(ithk.le.0) then
         write(iout,111)
  111    format(1x,'thickness of interbeds for sub-cr package will ',
     &     'be treated as a constant.')
c
      else
         write(iout,112)
  112    format(1x,'thickness of interbeds for sub-cr package will ',
     &    'be treated as a function of saturated thickness.')
      endif
c
c
c6b-----print a message on how void ratio is treated
      if(ivoid.le.0) then
         write(iout,114)
  114    format(1x,'void ratio for sub-cr package will be ',
     &    'treated as a constant.')
c
      else
         write(iout,115)
  115    format(1x,'void ratio for sub-cr package will be ',
     1    'treated as a variable.')
      endif
c
c6b-----print a message on the method being selected to calculate
c       settlement (subsidence)
      if (imethod.eq.1) then ! isotache
         write(iout,1151) imethod
 1151    format(1x,'imethod= ',i3,', the isotache method is selected.')
      elseif (imethod.eq.2) then ! bjerrum
         write(iout,1152) imethod
 1152    format(1x,'imethod= ',i3, ', the bjerrum method is selected.')
      else
         write(iout,1153)
 1153    format(1x,'imethod= ',i3,', method is not known -- stop.')
         stop
      endif
c6b-----print a message on how initial preconsolidation stress is
c       treated
      if(istpcs.ne.0) then !this corresponds to pop parameter in msettle
         write(iout,126)
  126    format(1x,'arrays of offset values will be read and added',
     &    ' to initial',/,' effective stress to get initial ',
     &    'preconsolidation stress.')
      else
         write(iout,127)
  127    format(1x,'arrays of preconsolidation stress will be read.')
      endif
c
c print alph
      write(iout,1260) alpha
 1260 format(1x,'alpha = ',f10.4)
c
c6b-----print a message on how preconsolidation stress is treated
c       over time.
!      if(isincr.ne.0) then
!         write(iout,1261)
! 1261    format(1x,'incremental run: preconsolidation and initial',
!     &    ' stress updated every time step.')
!      else
!         write(iout,1263)
! 1263    format(1x,'preconsolidation and initial stress willl be kept',
!     &    ' at their initial values.')
!      endif
c
c6_-----print a message on how recompression and compression
c
c ------abort if no layers are specified for interbed storage
      if(nsystm.lt.1) then
         write(iout,60)
   60    format(1x,'no layers with interbed storage ',
     1    'were specified in input.',/,1x,'simulation aborted.')
         call ustop(' ')
      endif
c ------read in model layer numbers for each system of interbeds,
c ------for layers without delay.
      allocate(lnwt(nsystm))
      write(iout,116) nsystm
  116 format(/,' model layer assignments for each of',i3,' sub-cr',
     1   ' systems of interbeds:')
      call urdcom(in,iout,line)
      read(line,*) (lnwt(n),n=1,nsystm)
      write(iout,117) (lnwt(n),n=1,nsystm)
  117 format(1x,25i4)
c
c ------read in observation points irow and icol. this will be used for tracing
c
      allocate(irowobssub(nobssub))
      allocate(icolobssub(nobssub))
      write(iout,216) nobssub
  216 format(/,' icol and irow for each subsidence observation pnts: ',
     &  i3,' sub-cr',' systems of interbeds:')

      if(nobssub.gt.0)then
       call urdcom(in,iout,line)
       read(line,*) (icolobssub(n), irowobssub(n),n=1,nobssub)
       write(iout,217) (icolobssub(n),n=1,nobssub)
       write(iout,217) (irowobssub(n),n=1,nobssub)
  217  format(1x,25i4)
      endif

      do n=1,nsystm
         if(lnwt(n).ge.1.and.lnwt(n).le.nlay) cycle
         write(iout,118)
  118    format(/,' improper layer assignment for sub-cr system of ',
     &    'interbeds.',/,' aborting...')
         call ustop(' ')
      enddo
c
      do k=1,nlay
c ------make sure there are no quasi-3d confining layers
         if(laycbd(k).ne.0) then
            write(iout,121)
  121       format(' sub-cr cannot be used in conjunction with ',
     &       'quasi-3d confining units.',/,' aborting...')
            call ustop(' ')
         endif
      enddo
c
c7------check to see that there are no zero or negative layer
c7------thicknesses
      do kq=1,nsystm
         k=lnwt(kq)
         do i=1,nrow
            do j=1,ncol
               if(ibound(j,i,k).le.0) cycle
               tp=botm(j,i,k-1)
               bt=botm(j,i,k)
               thick1=tp-bt
               if(thick1.le.0.0) then
                  write(iout,44) i,j,k
   44             format(' stopping-- zero or negative layer ',/,
     &             ' thickness found at (row, column, layer):',3i5)
                  write(iout,*) ' check layer elevation arrays in',
     &             ' dis input.'
                  call ustop(' ')
               endif
            enddo
         enddo
      enddo
c
c     calculate a priori at what times modflow will do calculation.
c     this is required to calculate effect (settlement) at a future
c     time steps due a change in effective stresses at current time step.
c
      allocate(nstptotscr)
      if (issflg(1).ne.0) then !only first stress period is allowed as steady state
         nstptotscr=nstp(1)
      else
         nstptotscr=0
      endif
c
      do i = 2, nper
         nstptotscr=nstptotscr+nstp(i)
      enddo
c
c8------allocate space for the arrays.
      allocate(thick(ncol,nrow,nsystm))
      allocate(isoarr(ncol,nrow,nsystm))
      allocate(isobcr(ncol,nrow,nsystm))
      allocate(isocca(ncol,nrow,nsystm))
c
      allocate(nIntegralEs(ncol,nrow,nsystm))
      allocate(oIntegralEs(ncol,nrow,nsystm))
      allocate(epTotal(ncol,nrow,nsystm))
c
      allocate(sub(ncol,nrow,nsystm))

      allocate(void(ncol,nrow,nsystm))
      allocate(pcs(ncol,nrow,nlay))
      allocate(pcs0(ncol,nrow,nlay))
      allocate(pcsold(ncol,nrow,nlay))
c
      if(istpcs.ne.0) then                              !sl added option
         allocate(pcsoff(ncol,nrow,nlay))
      else
         allocate(pcsoff(1,1,1))
      endif

      allocate(est(ncol,nrow,nlay))
      allocate(est0(ncol,nrow,nlay))
      allocate(estold(ncol,nrow,nlay))                     !estold
      allocate(gl(ncol,nrow,0:nlay))                       !modify for gl above layer 1
      allocate(gl0(ncol,nrow,0:nlay))
      allocate(zc(ncol,nrow,nlay))
      allocate(sgm(ncol,nrow))
      allocate(sgs(ncol,nrow))
      allocate(ocflg2(26,nstpt))
      allocate(oclay2(nlay))

c     now allocate memory for totimscr and pertimscr
      allocate(totimscr(nstptotscr))
      allocate(deltallscr(nstptotscr))
      allocate(pertimscr(nper))
      allocate(iddeltscr(nper,maxval(nstp)))


      allocate(subtime(ncol,nrow,nsystm,nstptotscr))
!      allocate(dblold(nstptotscr))
c     allocate memory for the array that will hold flux released due to
c     settlement.
      do ic=1,ncol
         do ir=1,nrow
            do is=1,nsystm
               do it=1,nstptotscr
                  subtime(ic,ir,is,it)=0
               enddo
            enddo
         enddo
      enddo

      do ic=1,ncol
         do ir=1,nrow
            do is=1,nsystm
               nIntegralEs(ic,ir,is)=0.d0
               oIntegralEs(ic,ir,is)=0.d0
               epTotal(ic,ir,is)=0.d0
            enddo
         enddo
      enddo
c
      allocate(qscr(ncol,nrow,nsystm,nstptotscr))
      do ic=1,ncol
         do ir=1,nrow
            do is=1,nsystm
               do it=1,nstptotscr
                  qscr(ic,ir,is,it)=0
               enddo
            enddo
         enddo
      enddo
c
c     read interbed storage data
c
c
c ------initialize arrays
      nij=nrow*ncol
      do n=1,nlay
         do i=1,nrow
            do j=1,ncol
               gl(j,i,n)=0.0
               est(j,i,n)=0.0
               estold(j,i,n)=0.0
               zc(j,i,n)=0.0
            enddo
         enddo
      enddo
c ------read flags and formats for printing calculated arrays
      call urdcom(in,iout,line)
c      read(in,'(a)') line
      lloc=1
      call urword(line,lloc,istart,istop,2,izcfl,r,iout,in)
      call urword(line,lloc,istart,istop,2,izcfm,r,iout,in)
      call urword(line,lloc,istart,istop,2,iglfl,r,iout,in)
      call urword(line,lloc,istart,istop,2,iglfm,r,iout,in)
      call urword(line,lloc,istart,istop,2,iestfl,r,iout,in)
      call urword(line,lloc,istart,istop,2,iestfm,r,iout,in)
      call urword(line,lloc,istart,istop,2,ipcsfl,r,iout,in)              !sl new flag
      call urword(line,lloc,istart,istop,2,ipcsfm,r,iout,in)              !sl new fmt
!      call urword(line,lloc,istart,istop,2,istfl,r,iout,in)              !sl new flag
!      call urword(line,lloc,istart,istop,2,istfm,r,iout,in)              !sl new fmt
c
c1------read in arrays with one value for all layers with interbed storage
      call u2drel(gl(:,:,0),aname(6),nrow,ncol,1,in,iout)    !change zls read to gl read
      call u2drel( sgm,aname(10),nrow,ncol,1,in,iout)
      call u2drel( sgs,aname(11),nrow,ncol,1,in,iout)
c3------read in arrays for each layer with interbed storage
      do kq=1,nsystm
         k=lnwt(kq)
         call u2drel(thick(:,:,kq),aname(1),nrow,ncol,k,in,iout)

         if (imethod.eq.1) then
            tmpnam=aname(12)
         elseif (imethod.eq.2) then
           tmpnam=aname(15)
         endif

         call u2drel(   isoarr(:,:,kq),tmpnam,nrow,ncol,k,in,iout)

         if (imethod.eq.1) then
           tmpnam=aname(13)
         elseif (imethod.eq.2) then
           tmpnam=aname(16)
         endif
c
         call u2drel(   isobcr(:,:,kq),tmpnam,nrow,ncol,k,in,iout)

         if (imethod.eq.1) then
           tmpnam=aname(14)
         elseif (imethod.eq.2) then
           tmpnam=aname(17)
        endif
c
         call u2drel(   isocca(:,:,kq),tmpnam,nrow,ncol,k,in,iout)

         call u2drel( void(:,:,kq),aname(4),nrow,ncol,k,in,iout)
         call u2drel(  sub(:,:,kq),aname(8),nrow,ncol,k,in,iout)
      enddo
c
      do k=1,nlay
         if(istpcs.ne.0) then
            call u2drel(  pcsoff(:,:,k),aname(5),nrow,ncol,k,in,iout)  !remove gl reads
         else
            call u2drel(  pcs(:,:,k),aname(5),nrow,ncol,k,in,iout)
         endif
      enddo
c
c     if the first stress period is steady state, delay
c     calculation of initial layer center, geostatic stress, effective
c     stress, and preconsolidation stress until after first stress
c     period is complete.
      if(issflg(1).ne.0) then
         write(iout,12)
   12    format(' calculated arrays for subcr will be printed after
     &  initial steady-state stress period.')
      else
c ------compute layer centers for all layers
         call sscr7z(ibound,hnew,botm,zc,nrow,ncol,nlay)
c
c ------compute starting geostatic stress and effective stress
c       if(igl.ne.0) then
         call sscr7g(ibound,hnew,botm,gl,
     &            sgm,sgs,nrow,ncol,nlay)
c       endif
c ------compute effective stress
         call sscr7e(ibound,hnew,botm,gl,est,nrow,ncol,nlay,iout)
c
c ------loop through all cells
         do k=1,nlay
            do ir=1,nrow
               do jc=1,ncol
                  if(istpcs.ne.0) pcs(jc,ir,k)=0.0    !pop; initialize so later it will contain initial stress + pcsoff
                  if(ibound(jc,ir,k).le.0) cycle                               !sl changed eq to le
c ------compute starting preconsolidation stress from offset
c ------values and starting effective stress values.
                  if(istpcs.ne.0) then
                     pcs(jc,ir,k)=est(jc,ir,k)+pcsoff(jc,ir,k)                     !sl added option
                  else
c ------make sure that starting preconsolidation stress values
c ------are consistant with starting effective stress values.
                     if (pcs(jc,ir,k).lt.est(jc,ir,k))
     &                      pcs(jc,ir,k)=est(jc,ir,k)
                  endif
c ------set effective stress for previous step.
                  estold(jc,ir,k)=est(jc,ir,k)
               enddo
            enddo
         enddo
c
c ------set initial values of effective stress, preconsolidation
c ------stress and geostatic stress
         do k=1,nlay
            do ir=1,nrow
               do jc=1,ncol
                  est0(jc,ir,k)=est(jc,ir,k)
                  pcs0(jc,ir,k)=pcs(jc,ir,k)
                  gl0(jc,ir,k)=gl(jc,ir,k)
               enddo
            enddo
         enddo
c
c ------print calculated arrays if flags are set
         do k=1,nlay
            kk=k
            if(izcfl.gt.0) then
               write(iout,222)
  222          format(/,' the following is a calculated ',
     &          ' (or recalculated) sub-cr array at the start of the ',
     &          'simulation:')
               if(izcfm.lt.0) call ulaprs(zc(:,:,kk),text(1),1,1,ncol,
     &          nrow,kk,-izcfm,iout)
               if(izcfm.ge.0) call ulaprw(zc(:,:,kk),text(1),1,1,ncol,
     &           nrow,kk,izcfm,iout)
            endif
         enddo
c
         do k=1,nlay
            kk=k
            if(iglfl.gt.0) then
               write(iout,222)
               if(iglfm.lt.0) call ulaprs(gl(:,:,kk),text(2),1,1,
     &           ncol,nrow,kk,-iglfm,iout)
               if(iglfm.ge.0) call ulaprw(gl(:,:,kk),text(2),1,1,
     1           ncol,nrow,kk,iglfm,iout)
            endif
         enddo
c
         do k=1,nlay
            kk=k
            if(iestfl.gt.0) then
               write(iout,222)
               if(iestfm.lt.0) call ulaprs(est(:,:,kk),text(3),1,1,
     &           ncol,nrow,kk,-iestfm,iout)
               if(iestfm.ge.0) call ulaprw(est(:,:,kk),text(3),1,1,
     &           ncol,nrow,kk,iestfm,iout)
            endif
         enddo
c
         do k=1,nlay
            kk=k
            if(ipcsfl.gt.0) then
               write(iout,222)
               if(ipcsfm.lt.0) call ulaprs(pcs(:,:,kk),text(4),1,1,
     &            ncol,nrow,kk,-ipcsfm,iout)
               if(ipcsfm.ge.0) call ulaprw(pcs(:,:,kk),text(4),1,1,
     &            ncol,nrow,kk,ipcsfm,iout)
            endif
         enddo
c
      endif !if(issflg(1).ne.0)               !sl end actions 1st period not ss
c
c ------initialize and read output flags.
c ------set all flags for output control to "false".
      do i=1,nstpt
         do n=1,26
            ocflg2(n,i)=.false.
         enddo
      enddo
c
c5------read formats and unit numbers output flags.
      if(iscroc.gt.0) then
         call urdcom(in,iout,line)
c         read(in,'(a)') line
         lloc=1
         do n=1,13
            call urword(line,lloc,istart,istop,2,iswocf(n),r,iout,in)
            call urword(line,lloc,istart,istop,2,iswocu(n),r,iout,in)
         enddo
         write(iout,310) (iswocf(n),iswocu(n),n=1,13)
  310    format(/,'             subsidence print format is number',i4/
     &            '                 unit for saving subsidence is',i4/
     &            '    compaction by layer print format is number',i4/
     &            '        unit for saving compaction by layer is',i4/
     &            '   compaction by system print format is number',i4/
     &            '       unit for saving compaction by system is',i4/
     &            '  vertical displacement print format is number',i4/
     &            '      unit for saving vertical displacement is',i4/
     &            'preconsolidation stress print format is number',i4/
     &            '    unit for saving preconsolidation stress is',i4/
     &            'change in precon stress print format is number',i4/
     &            ' unit for saving change in preconsol stress is',i4/
     &            '       geostatic stress print format is number',i4/
     &            '           unit for saving geostatic stress is',i4/
     &            'chnge in geostatic strs print format is number',i4/
     &            ' unit for saving change in geostatic stress is',i4/
     &            '       effective stress print format is number',i4/
     &            '           unit for saving effective stress is',i4/
     &            '  change in eff. stress print format is number',i4/
     &            ' unit for saving change in effective stress is',i4/
     &            '             void ratio print format is number',i4/
     &            '                 unit for saving void ratio is',i4/
     &            '              thickness print format is number',i4/
     &            '                  unit for saving thickness is',i4/
     &            '       center elevation print format is number',i4/
     &            '           unit for saving center elevation is',i4)
c
         ntssm2(1)=0
         if(nper.gt.1) then
            do n=2,nper
               ntssm2(n)=ntssm2(n-1)+nstp(n-1)
            enddo
         endif
         iocr=0
         do noclin=1,iscroc
            call urdcom(in,iout,line)
c            read(in,'(a)',end=500) line
            iocr=iocr+1
            lloc=1
            call urword(line,lloc,istart,istop,2,isp1,r,iout,in)
            call urword(line,lloc,istart,istop,2,isp2,r,iout,in)
            call urword(line,lloc,istart,istop,2,jts1,r,iout,in)
            call urword(line,lloc,istart,istop,2,jts2,r,iout,in)
            do n=1,26
               call urword(line,lloc,istart,istop,2,ifl2(n),r,iout,in)
            enddo
c
            if(isp1.lt.1) isp1=1
            if(isp1.gt.nper) isp1=nper
            if(isp2.lt.1) isp2=1
            if(isp2.gt.nper) isp2=nper
            if(isp1.gt.isp2) isp1=isp2
            do i=isp1,isp2
               j1=jts1
               j2=jts2
               if(j1.lt.1) j1=1
               if(j1.gt.nstp(i)) j1=nstp(i)
               if(j2.lt.1) j2=1
               if(j2.gt.nstp(i)) j2=nstp(i)
               if(j1.gt.j2) j1=j2
               do j=j1,j2
                  iloc=ntssm2(i)+j
                  do n=1,26
                     if(ifl2(n).gt.0) ocflg2(n,iloc)=.true.
                     if(ifl2(n).eq.0) ocflg2(n,iloc)=.false.
                  enddo
               enddo
            enddo
         enddo
      endif  !if(iscroc.gt.0)
c
      go to 200
  500 write(iout,502) iocr,iscroc
  502 format(1x,'only ',i4,' out of ',i4,' output control records ',
     &  'for sub-cr were found.',/,1x,'simulation aborted.')
      call ustop(' ')
c
c6------return
  200 call sgwf2scr7psv(igrid)
c
      return
      end subroutine gwf2scr7ar
c
c subr 2
      subroutine gwf2scr7st(kper,igrid)
c     ******************************************************************
c        calculate layer centers, geostatic stress, effective
c     stress, and preconsolidation stress after an initial
c     steady-state stress period
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      use global,      only: ibound,hnew,botm,buff,delr,delc,issflg,
     1                       nrow,ncol,nlay,nper,iout

      use gwfscrmodule,only: zc,gl,sgm,sgs,est,istpcs,pcs,pcsoff,
     &                       estold,nsystm,lnwt,void,isoarr,isobcr,
     &                       isocca,est0,pcs0,gl0,izcfl,izcfm,iglfl,
     &                       iglfm,iestfl,iestfm,ipcsfl,ipcsfm,
     &                       nstptotscr, !isincr, !,istfl,istfm
     &                       pcsold, nIntegralEs, oIntegralEs,eptotal

      implicit none
      integer                                 :: kper
      integer                                 :: igrid
      integer                                 :: k,ir,jc
      integer                                 :: kk
      character(16)                           :: text(8)

      data text(1) /'center elevation'/,
     &     text(2) /'geostatic stress'/,
     &     text(3) /'effective stress'/,
     &     text(4) /'preconsol stress'/
c
c     ------------------------------------------------------------------
      call sgwf2scr7pnt(igrid)
c
c1------return if this is not the second stress period or if the first
c1------stress period was transient.
      if(kper.ne.2) return
      if(issflg(1).eq.0) return
c ------compute layer centers for all layers
      call sscr7z(ibound,hnew,botm,zc,nrow,ncol,nlay)
c
c ------compute starting geostatic stress and effective stress (mahmoud: at bottom of layers)
      call sscr7g(ibound,hnew,botm,gl,
     &            sgm,sgs,nrow,ncol,nlay)
c ------compute effective stress
      call sscr7e(ibound,hnew,botm,gl,est,nrow,ncol,nlay,iout)
c
c ------loop through all cells
      do k=1,nlay
         do ir=1,nrow
            do jc=1,ncol
              if(istpcs.ne.0) pcs(jc,ir,k)=0.0
              if(ibound(jc,ir,k).le.0) cycle                               !sl changed eq to le
c ------compute starting preconsolidation stress from offset
c ------values and starting effective stress values.
              if(istpcs.ne.0) then
                 pcs(jc,ir,k)=est(jc,ir,k)+pcsoff(jc,ir,k)                     !sl added option
              else
c ------make sure that starting preconsolidation stress values
c ------are consistant with starting effective stress values.
                 if (pcs(jc,ir,k).lt.est(jc,ir,k))
     &                       pcs(jc,ir,k)=est(jc,ir,k)
              endif
c ------set effective stress for previous step.
              estold(jc,ir,k)=est(jc,ir,k)
            enddo
         enddo
      enddo
c
c ------set initial values of effective stress, preconsolidation
c ------stress and geostatic stress
      do k=1,nlay
         do ir=1,nrow
            do jc=1,ncol
               est0(jc,ir,k)=est(jc,ir,k)
               pcs0(jc,ir,k)=pcs(jc,ir,k)
               gl0(jc,ir,k)=gl(jc,ir,k)
               pcsold(jc,ir,k)=pcs(jc,ir,k)
            enddo
         enddo
      enddo
c
c ------print calculated arrays if flags are set
      do k=1,nlay
         kk=k
         if(izcfl.gt.0) then
            write(iout,222)
  222       format(/,' the following is a calculated (or recalculated)',
     &    ' sub-cr array after the initial steady-state stress period:')
            if(izcfm.lt.0) call ulaprs(zc(:,:,kk),text(1),1,1,ncol,
     &         nrow,kk,-izcfm,iout)
            if(izcfm.ge.0) call ulaprw(zc(:,:,kk),text(1),1,1,ncol,
     &           nrow,kk,izcfm,iout)
         endif
      enddo
c
      do k=1,nlay
        kk=k
        if(iglfl.gt.0) then
          write(iout,222)
          if(iglfm.lt.0) call ulaprs(gl(:,:,kk),text(2),1,1,
     1          ncol,nrow,kk,-iglfm,iout)
          if(iglfm.ge.0) call ulaprw(gl(:,:,kk),text(2),1,1,
     1           ncol,nrow,kk,iglfm,iout)
        endif
      enddo
c
      do k=1,nlay
        kk=k
        if(iestfl.gt.0) then
          write(iout,222)
          if(iestfm.lt.0) call ulaprs(est(:,:,kk),text(3),1,1,
     1           ncol,nrow,kk,-iestfm,iout)
          if(iestfm.ge.0) call ulaprw(est(:,:,kk),text(3),1,1,
     1           ncol,nrow,kk,iestfm,iout)
        endif
      enddo
c
      do k=1,nlay
        kk=k
        if(ipcsfl.gt.0) then
          write(iout,222)
          if(ipcsfm.lt.0) call ulaprs(pcs(:,:,kk),text(4),1,1,
     1           ncol,nrow,kk,-ipcsfm,iout)
          if(ipcsfm.ge.0) call ulaprw(pcs(:,:,kk),text(4),1,1,
     1           ncol,nrow,kk,ipcsfm,iout)
        endif
      enddo
c
c4-----return.
      return
      end subroutine gwf2scr7st
c
c subr 3m
      subroutine gwf2scr7fm(kper,kstp,igrid)
c     ******************************************************************
c        calculate settlement and add interbed storage to rhs
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      use global,      only: rhs,ibound,hnew,hold,botm,ncol,nrow, !hcof,
     &                       nlay,issflg,iout,buff,delr,delc
      use gwfbasmodule,only: delt
c
      use gwfscrmodule,only: nsystm,thick,isoarr,isobcr,isocca,
     &                       void,pcs,gl,zc,est,estold,sgm,sgs,ithk,
     &                       lnwt,nstptotscr,iddeltscr,subtime,qscr, !isincr,istfl,
     &                       est0,pcs0,sub,totimscr,imethod,alpha
c
      implicit none
      integer                                 :: igrid,kper,kstp
      integer                                 :: kq,k,i,j
      real*8                                  :: rho1,rho2
      real*8                                  :: gln,zcn
      real*8                                  :: nPS,oPS
      real*8                                  :: oS,nS
      real*8                                  :: esto
      real*8                                  :: dbl
      real*8                                  :: dblq
      real*8                                  :: ep
      real*8                                  :: es
      real*8                                  :: dblinc
      real*8                                  :: dbl1
      real*8                                  :: nIntegral
      integer                                 :: kk,ik
      real*8                                  :: strg
      real                                    :: th
      real                                    :: tled
c
c     ------------------------------------------------------------------
      call sgwf2scr7pnt(igrid)
c
c0------skip calculations if this is a steady-state stress period.
      if(issflg(kper).eq.1) return
c
c1------initialize
       tled=1./delt
c
c ------update layer centers for layers
      call sscr7z(ibound,hnew,botm,zc,nrow,ncol,nlay)
c
c4------update geostatic stress
      call sscr7g(ibound,hnew,botm,gl,
     &            sgm,sgs,nrow,ncol,nlay)
c
      kk=iddeltscr(kper,kstp)
c2------add contributions from storage changes to rhs
c2------for each system of interbeds
      do kq=1,nsystm
        k=lnwt(kq)
        do i=1,nrow
          do j=1,ncol
            if(ibound(j,i,k).le.0) cycle
c
            call gwf2scr7csub(j,i,k,kq,kper,kstp,th,nS,oS,nPS,oPS,
     &               nIntegral,ep,es,dblq,dbl,strg)
c4------add appropriate terms to rhs; i.e. q from interbed

            rhs(j,i,k)=rhs(j,i,k)-qscr(j,i,kq,kk) !dblq/thick1*delr(j)*delc(i)*tled*
!     &                                       void(j,i,kq)
c
c calculate equivalent inelastic (virgin ) and elastic skeletal specific storage values.

          enddo !do j=1,ncol
        enddo !do i=1,nrow
      enddo !do kq=1,nsystem
c
c5------return
      return
      end subroutine gwf2scr7fm
c
c subr 4m
      subroutine gwf2scr7bd(kstp,kper,igrid)
c     ******************************************************************
c     calculate volumetric budget for interbed storage
c     ******************************************************************
c
c     specifications:
c     ------------------------------------------------------------------
      use global,      only: buff,ibound,hnew,hold,botm,delr,delc,
     &                       ncol,nrow,nlay,issflg,iout
      use gwfbasmodule,only: delt,vbvl,vbnm,msum,icbcfl
      use gwfscrmodule,only: iscrcb,nsystm,thick,isoarr,
     &                       isobcr,isocca,sub,
     &                       ivoid,void,pcs,gl,zc,est,estold,sgm,sgs,
     &                       ithk,lnwt,subtime,qscr,nstptotscr,
     &                       iddeltscr,est0,pcs0,totimscr, !isincr,
     &                       imethod,alpha,pcsold,nobssub,icolobssub,
     &                       irowobssub,oIntegralEs,nIntegralEs,eptotal
      implicit none
      integer                                       :: kstp
      integer                                       :: kper
      integer                                       :: igrid
      integer                                       :: ibd              ! cell-by-cell flow term flag
      integer                                       :: il,ir,ic
      integer                                       :: kq
      integer                                       :: k,i,j
      integer                                       :: iobs

      real                                          :: stoin            ! cell-by-cell flow term inflow
      real                                          :: stout            ! cell-by-cell flow term outflow
      real                                          :: tled
      real*8                                        :: strg
      real*8                                        :: delb
      real*8                                        :: strain
      real*8                                        :: ep
      real*8                                        :: es
      character *128                                :: fname
      integer                                       :: iutrace

      character*16 text
      double precision gln,zcn,oPS,nPS,oS,nS,esto,
     &        dblq,dblinc,dbl1,dbl
      real *8 nIntegral
      real th
      integer ik,kk
c
      data text /'INTERBED STORAGE'/
c     ------------------------------------------------------------------
      call sgwf2scr7pnt(igrid)
c
      iutrace=13
      if (kstp.eq.1 .and. kper.eq.1) then
        fname='trace.dat'
        open(unit=iutrace, file = fname, access = 'sequential',
     &       status = 'unknown')
        write(iutrace, 114)
      endif
c1------initialize cell-by-cell flow term flag (ibd) and
c1------accumulators (stoin and stout).
      ibd=0
      stoin=0.
      stout=0.
c
c2------test to see if cell-by-cell flow terms are needed.
      if(icbcfl.ne.0  .and. iscrcb.gt.0 ) ibd=1
c
c ------if this is a steady-state stress period, skip calculations
      tled=0.0
      if(issflg(kper).eq.1) go to 111
c
      ik=0
      if(issflg(1).eq.1) then !only the first stress is allowed to bd ss
         ik=0
      endif
c3------cell-by-cell flow terms are needed set ibd and clear buffer.
      do il=1,nlay
        do ir=1,nrow
          do ic=1,ncol
            buff(ic,ir,il)=0.
          enddo
        enddo
      enddo
      tled=1./delt
      dbl=0

      kk=iddeltscr(kper,kstp)

      do kq=1,nsystm
        k=lnwt(kq)
c
c4------run through every cell in the grid with interbed storage.
        do i=1,nrow
          do j=1,ncol
c
c5------calculate flow from storage (variable head cells only)
            if(ibound(j,i,k).le.0) cycle
c
            call gwf2scr7csub(j,i,k,kq,kper,kstp,th,nS,oS,nPS,oPS,
     &                 nIntegral,ep,es,dblq,dbl,strg)
c
c7------calculate volume change in interbed storage for time step.
!            strg=dblq*delr(j)*delc(i) *porosity !void(j,i,kq)
c
c8------accumulate subsidence associated with change in storage
            subtime(j,i,kq,kk)=dbl !subtime(j,i,kq,kk-1)+dbl

            delb=dblq

            sub(j,i,kq)=subtime(j,i,kq,kk) !dbl(kk) !sub(j,i,kq)+dblq !delb
c
c tracing only: comment after that
            do iobs=1, nobssub
              if (j.eq. icolobssub(iobs) .and. i.eq.irowobssub(iobs))
     &                                                            then
                write(iutrace, 115) j,i,kq,totimscr(kk),th,
     &                            hold(j,i,k),hnew(j,i,k),nS,oS,
     &                            nPS,sub(j,i,kq),dblq,strg
              endif
            enddo

c ------update void ratio and thickness arrays
            if(ivoid.gt.0) then
              if(thick(j,i,kq).gt.0.0) then
                strain=-dblq/thick(j,i,kq) !-delb/thick(j,i,kq)
              else
                strain=0.0
              endif
              void(j,i,kq)=strain+void(j,i,kq)*(strain+1.)
              thick(j,i,kq)=thick(j,i,kq)*(strain+1.)
            endif
c
c9------if c-b-c flow terms are to be saved then add rate to buffer.
            if(ibd.eq.1) buff(j,i,k)=buff(j,i,k)+strg*tled
c
c10-----see if flow is into or out of storage.
            if(strg.lt.0.0) then
              stout=stout-strg
            else
              stoin=stoin+strg
            endif
c------update integral part of the secondary strain
            oIntegralEs(j,i,kq)=nIntegralEs(j,i,kq)

            epTotal(j,i,kq)=epTotal(j,i,kq)+ep
          enddo
        enddo
      enddo
c
c11-----if c-b-c flow terms will be saved call ubudsv to record them.
  111 if(ibd.eq.1) call ubudsv(kstp,kper,text,iscrcb,buff,ncol,nrow,
     1                          nlay,iout)
c
c12-----move rates,volumes & labels into arrays for printing.
      vbvl(3,msum)=stoin*tled
      vbvl(4,msum)=stout*tled
      vbvl(1,msum)=vbvl(1,msum)+stoin
      vbvl(2,msum)=vbvl(2,msum)+stout
      vbnm(msum)=text
c
c13-----increment budget term counter
      msum=msum+1
c
c ------update layer centers for layers with bottom specified
      call sscr7z(ibound,hnew,botm,zc,nrow,ncol,nlay)
c
c4------update geostatic stress and effective stress
       call sscr7g(ibound,hnew,botm,gl,
     1            sgm,sgs,nrow,ncol,nlay)
      call sscr7e(ibound,hnew,botm,gl,est,nrow,ncol,nlay,iout)
c
c14-----update preconsolidation head and old effective stress arrays
      kk=iddeltscr(kper,kstp)
      do k=1,nlay
        do i=1,nrow
          do j=1,ncol
            pcsold(j,i,k) = pcs(j,i,k)
            if(ibound(j,i,k).le.0) cycle
            if(est(j,i,k).gt.pcs(j,i,k)) pcs(j,i,k)=est(j,i,k)
            estold(j,i,k)=est(j,i,k)
          enddo
        enddo
      enddo
c
      if (kk.eq.nstptotscr) close(iutrace)
 114  format(' col',1x,' row',1x,'system',1x,'        time',1x,
     + '   thickness', 1x,
     + '        hold', 1x,'        hnew', 1x, '          nS', 1x,
     + '          oS', 1x, '         pcs', 1x, '         sub', 1x,
     + '        dblq',1x,'        strg')
 115  format(i4,1x,i4,1x,i6,1x,e12.5,1x,e12.5,1x,e12.5,1x,e12.5,1x,
     +       e12.5,1x,e12.5,
     +       1x,e12.5,1x,e12.5,1x,e12.5,1x,e12.5)
c15-----return
      return
      end subroutine gwf2scr7bd
c
c***********************************************************************
      subroutine gwf2scr7csub(jcol,irow,klayer,kqsys,kper,kstp,
     &                        th,nS,oS,nPS,oPS,nIntegral,ep,es,
     &                        dblq,dbl,strg)
      use global,      only: hnew,hold,botm,delr,delc,
     1                       ncol,nrow,nlay,issflg,iout
      use gwfbasmodule,only: delt
      use gwfscrmodule,only: iscrcb,nsystm,thick,isoarr,
     &                       isobcr,isocca,
     &                       ivoid,void,pcs,gl,zc,est,estold,sgm,sgs,
     &                       ithk,lnwt,subtime,qscr,nstptotscr, !dblold,
     &                       iddeltscr,est0,pcs0,totimscr, pcsold, !isincr,
     &                       imethod,alpha,oIntegralEs,epTotal

      implicit none
      real                                       :: hhnew
      real                                       :: tfact
      real                                       :: tp,bt,topnew
      real                                       :: thick1,th
      real                                       :: arr,bcr,cca
      real                                       :: thm
      real                                       :: dt
      real                                       :: porosity
      real                                       :: tled
      real*8                                     :: strg
      real                                       :: dtarray(nstptotscr)
      integer                                    :: kk
      integer                                    :: jcol,irow,klayer
      integer                                    :: kqsys
      integer                                    :: kper,kstp
      integer                                    :: isunloading
      integer                                    :: it
      integer                                    :: nt
      integer                                    :: itt
      real*8                                     :: gln
      real*8                                     :: zcn  !estn, esto, estn1, !pctmp,estn1,estn,esto,
      real*8                                     :: dblq
      real*8                                     :: dblinc
      real*8                                     :: dbl1
      real*8                                     :: subt(nstptotscr)
      real*8                                     :: dbl
      real*8                                     :: esold
      real*8                                     :: tnew,told
      real*8                                     :: opcs
      real*8                                     :: ep,es
      real*8                                     :: nIntegral,oIntegral
      real*8                                     :: oTime,nTime
      real*8                                     :: oS,nS
      real*8                                     :: oPS,nPS
      real*8                                     :: epT

c3------determine storage capacities for cell at start and end of step
      hhnew=hnew(jcol,irow,klayer)
c
      tled=1./delt
c3a-----find thickness of interbeds in saturated interval
      tfact=1
      if(ithk.gt.0) then
        tp=botm(jcol,irow,klayer-1)
        bt=botm(jcol,irow,klayer)
        topnew=hhnew
        thick1=tp-bt
c3b-----first find top of saturated thickness
        if(topnew.gt.tp) topnew=tp
c3c-----compute correction factor as ratio of current to past saturated
c3c-----thickness
        tfact=(topnew-bt)/thick1
      endif
c
      gln=gl(jcol,irow,klayer)
      zcn=botm(jcol,irow,klayer)
      nS=gln-hnew(jcol,irow,klayer)+zcn
      oS=estold(jcol,irow,klayer)
      th=tfact*thick(jcol,irow,kqsys)
c
c ------calculate settlement using isotache/bjerrum
      if (imethod.eq.1) then !isotache
        arr=isoarr(jcol, irow, kqsys)
        bcr=isobcr(jcol, irow, kqsys)
        cca=isocca(jcol, irow, kqsys)
      elseif (imethod.eq.2) then
        arr=isoarr(jcol, irow, kqsys)/(1+void(jcol,irow,kqsys))
        bcr=isobcr(jcol, irow, kqsys)/(1+void(jcol,irow,kqsys))
        cca=isocca(jcol, irow, kqsys)
      endif

      kk=iddeltscr(kper,kstp)
      itt=0
      do it=1,nstptotscr !kk,nstptotscr
        itt=itt+1
        if (it.eq.1) then
          dtarray(itt)=totimscr(it)
        else
          dtarray(itt)=totimscr(it)-totimscr(it-1) !totimscr(kk-1)
        endif
      enddo
      nt=nstptotscr-kk+1
c
      if (imethod.eq.1) then
        !oS=est0(jcol,irow,klayer)
        !oPS=pcs0(jcol,irow,klayer)
        oS=estold(jcol,irow,klayer)
        oPS=pcsold(jcol,irow,klayer)
        nPS=pcs(jcol,irow,klayer)
        oIntegral = oIntegralEs(jcol,irow,kqsys)
        nTime=totimscr(kk)
        oTime=totimscr(kk-1)
        call isotache(ep,es,nIntegral,oIntegral,oS,nS,oPS,nPS,
     &                    arr,bcr,cca,oTime,nTime)
        epT=eptotal(jcol,irow,kqsys)+ep
      elseif (imethod.eq.2) then
        !oS=est0(jcol,irow,klayer)
        !oPS=pcs0(jcol,irow,klayer)
        oS=estold(jcol,irow,klayer)
        oPS=pcsold(jcol,irow,klayer)
        nPS=pcs(jcol,irow,klayer)
        oIntegral = oIntegralEs(jcol,irow,kqsys)
        nTime=totimscr(kk)
        oTime=totimscr(kk-1)
        call bjerrum(ep,es,nIntegral,oIntegral,oS,nS,oPS,nPS,
     &                    arr,bcr,cca,oTime,nTime)
        epT=eptotal(jcol,irow,kqsys)+ep
      endif
      dbl=(epT+es)*th
      dbl=th-th*exp(-dbl/th);
c
      do it=1,nstptotscr
        subt(it)=subtime(jcol,irow,kqsys,it)
      enddo

      subt(kk)=dbl !(it) !dbl(it-kk+1) !+subt(it)
c
      if (kk.gt.2) then
        dblq=(subt(kk)-subt(kk-1))*alpha+
     +             (subt(kk-1)-subt(kk-2))*(1-alpha)
      elseif (kk.eq.2) then
        dblq=subt(kk)-subt(kk-1)
      else
        dblq=0
      endif

      !dblq=0    ! be careful ::: to be removed later
c
c q=area of the finite difference cell * dbl / model time step
      porosity=void(jcol,irow,kqsys)/(void(jcol,irow,kqsys)+1.0)
      qscr(jcol,irow,kqsys,kk)=dblq*tled*delr(jcol)*delc(irow) *porosity !should we multiply by void???

      strg=dblq*delr(jcol)*delc(irow) *porosity !void(j,i,kq)

      return
      end subroutine gwf2scr7csub
c***********************************************************************
c subr 5
      subroutine gwf2scr7ot(kstp,kper,igrid)
c     ******************************************************************
c     print and store subsidence, compaction and critical head.
c     ******************************************************************
c
c     specifications:
c     ------------------------------------------------------------------
      use global,      only: ncol,nrow,nlay,nstp,buff,iout
      use gwfbasmodule,only: pertim,totim
      use gwfscrmodule,only: nsystm,thick,sub,void,pcs,gl,zc,est,lnwt,
     &                       ocflg2,iswocf,iswocu,oclay2,ntssm2,pcs0,
     &                       gl0,est0
c
      implicit none
c
      integer                                       :: kstp
      integer                                       :: kper
      integer                                       :: igrid
      integer                                       :: nnstp
      integer                                       :: il,ir,ic
      integer                                       :: kq
      integer                                       :: nl,nll,nl1
      integer                                       :: k,i,j
      integer                                       :: kl,kkl,kl1

      character*16 text
      dimension text(13)
      DATA TEXT(1)  /'      SUBSIDENCE'/,
     2     TEXT(2)  /'LAYER COMPACTION'/,
     3     TEXT(3)  /'SYSTM COMPACTION'/,
     3     TEXT(4)  /'  Z DISPLACEMENT'/,
     3     TEXT(5)  /'PRECONSOL STRESS'/,
     3     TEXT(6)  /'CHANGE IN PCSTRS'/,
     4     TEXT(7)  /'GEOSTATIC STRESS'/,
     4     TEXT(8)  /'CHANGE IN G-STRS'/,
     5     TEXT(9)  /'EFFECTIVE STRESS'/,
     5     TEXT(10) /'CHANGE IN EFF-ST'/,
     6     TEXT(11) /'      VOID RATIO'/,
     7     TEXT(12) /'       THICKNESS'/,
     8     TEXT(13) /'CENTER ELEVATION'/
c     ------------------------------------------------------------------
      call sgwf2scr7pnt(igrid)
c
c1------initialize time step pointer to retrieve flags for printing and
c1------saving arrays.
      nnstp=ntssm2(kper)+kstp
c
c3------print and store subsidence, first, clear out buff.
      if(ocflg2(1,nnstp).or.ocflg2(2,nnstp)) then
         do i=1,nrow
            do j=1,ncol
               buff(j,i,1)=0.
            enddo
         enddo
c
c4------sum compaction in all layers to get subsidence.
         kq=0
         do kq=1,nsystm
            do i=1,nrow
               do j=1,ncol
                  buff(j,i,1)=buff(j,i,1)+sub(j,i,kq)
               enddo
            enddo
         enddo
c
c5-------print subsidence.
         if(ocflg2(1,nnstp)) then
            write(iout,'(2a)') ' the following subsidence array is the',
     &       ' sum of compaction values for all systems of interbeds:'
            if(iswocf(1).lt.0) call ulaprs(buff,text(1),kstp,kper,ncol,
     &            nrow,1,-iswocf(1),iout)
            if(iswocf(1).ge.0) call ulaprw(buff,text(1),kstp,kper,ncol,
     &             nrow,1,iswocf(1),iout)
         endif
c
c6-------store subsidence.
         if(ocflg2(2,nnstp)) then
            call ulasav(buff,text(1),kstp,kper,pertim,totim,ncol,nrow,1,
     &              iswocu(1))
         endif
c
c7------print and store compaction for each system of interbeds.
         if(ocflg2(5,nnstp).or.ocflg2(6,nnstp)) then
            do kq=1,nsystm
               k=lnwt(kq)
               if(ocflg2(5,nnstp)) then
                  write(iout,76) kq
  76              format(/,1x,' system',i4,' of subwt interbeds:')
                  if(iswocf(3).lt.0)
     &               call ulaprs(sub(:,:,kq),text(3),kstp,kper,
     &                           ncol,nrow,k,-iswocf(3),iout)
                     if(iswocf(3).ge.0) call ulaprw(sub(:,:,kq),text(3),
     1                  kstp,kper,ncol,nrow,k,iswocf(3),iout)
               endif
               if(ocflg2(6,nnstp)) then
                  call ulasav(sub(:,:,kq),text(3),kstp,kper,pertim,
     &              totim,ncol,nrow,kq,iswocu(3))
               endif
            enddo
         endif
      endif
c
c ------sum compaction in each layer in the buff array for saving
c ------or printing compaction or vertical displacement by model
c ------layer. first, clear out buff.
      if(ocflg2(3,nnstp).or.ocflg2(4,nnstp).or.
     &   ocflg2(7,nnstp).or.ocflg2(8,nnstp)) then
         do nl=1,nlay
            oclay2(nl)=.false.
         enddo

         do k=1,nlay
            do i=1,nrow
               do j=1,ncol
                  buff(j,i,k)=0.
               enddo
            enddo
         enddo

c -------sum compaction in all model layers.
         do kq=1,nsystm
            k=lnwt(kq)
            oclay2(k)=.true.
            do i=1,nrow
               do j=1,ncol
                  buff(j,i,k)=buff(j,i,k)+sub(j,i,kq)
               enddo
            enddo
         enddo
c
c -------print compaction by layer.
         if(ocflg2(3,nnstp)) then
            do kl=1,nlay
               if(.not.oclay2(kl)) cycle
               kkl=kl
               if(iswocf(2).lt.0) call ulaprs(buff(:,:,kkl),text(2),
     &           kstp,kper,ncol,nrow,kkl,-iswocf(2),iout)
               if(iswocf(2).ge.0) call ulaprw(buff(:,:,kkl),text(2),
     &            kstp,kper,ncol,nrow,kkl,iswocf(2),iout)
            enddo
         endif
c
c -------store compaction by layer.
         if(ocflg2(4,nnstp)) then
            do kl=1,nlay
               if(.not.oclay2(kl)) cycle                       !sl consider removing this
               kkl=kl
               call ulasav(buff(:,:,kkl),text(2),kstp,kper,pertim,totim,
     &             ncol,nrow,kkl,iswocu(2))
            enddo
         endif
c
c ------calculate vertical displacement.
         if(ocflg2(7,nnstp).or.ocflg2(8,nnstp)) then
            nl1=nlay-1
            if(nlay.gt.1) then
               do kl=nl1,1,-1
                  kl1=kl+1
                  do i=1,nrow
                     do j=1,ncol
                        buff(j,i,kl)=buff(j,i,kl)+buff(j,i,kl1)
                     enddo
                  enddo
               enddo
            endif
c ------print vertical displacement for all model layers.
            if(ocflg2(7,nnstp)) then
               do kl=1,nlay
                  kkl=kl
                  if(iswocf(4).lt.0) call ulaprs(buff(:,:,kkl),text(4),
     &             kstp,kper,ncol,nrow,kkl,-iswocf(4),iout)
                  if(iswocf(4).ge.0) call ulaprw(buff(:,:,kkl),text(4),
     &             kstp,kper,ncol,nrow,kkl,iswocf(4),iout)
               enddo
            endif
c
c ------save vertical displacement for all model layers.
            if(ocflg2(8,nnstp)) then
               do kl=1,nlay
                  kkl=kl
                  call ulasav(buff(:,:,kkl),text(4),kstp,kper,pertim,
     &              totim,ncol,nrow,kkl,iswocu(4))
               enddo
            endif
         endif
      endif
c
c ------print and save precocsolidation stress
      if(ocflg2(9,nnstp).or.ocflg2(10,nnstp)) then
         do kl=1,nlay
            kkl=kl
            if(ocflg2(9,nnstp)) then
               if(iswocf(5).lt.0) call ulaprs(pcs(:,:,kkl),text(5),kstp,
     &          kper,ncol,nrow,kkl,-iswocf(5),iout)
               if(iswocf(5).ge.0) call ulaprw(pcs(:,:,kkl),text(5),kstp,
     &          kper,ncol,nrow,kkl,iswocf(5),iout)
            endif
            if(ocflg2(10,nnstp)) then
               call ulasav(pcs(:,:,kkl),text(5),kstp,kper,pertim,totim,
     &          ncol,nrow,kkl,iswocu(5))
            endif
         enddo
      endif !if(ocflg2(1,nnstp).or.ocflg2(2,nnstp))
c
c ------print and save change in precocsolidation stress
      if(ocflg2(11,nnstp).or.ocflg2(12,nnstp)) then
         do k=1,nlay
            do i=1,nrow
               do j=1,ncol
                  buff(j,i,k)=pcs(j,i,k)-pcs0(j,i,k)
               enddo
            enddo
         enddo

         do kl=1,nlay
            kkl=kl
            if(ocflg2(11,nnstp)) then
               if(iswocf(6).lt.0) call ulaprs(buff(:,:,kkl),text(6),
     &           kstp,kper,ncol,nrow,kkl,-iswocf(6),iout)
               if(iswocf(6).ge.0) call ulaprw(buff(:,:,kkl),text(6),
     &           kstp,kper,ncol, nrow,kkl,iswocf(6),iout)
            endif
            if(ocflg2(12,nnstp)) then
               call ulasav(buff(:,:,kkl),text(6),kstp,kper,pertim,
     &            totim,ncol,nrow,kkl,iswocu(6))
            endif
         enddo
      endif !if(ocflg2(11,nnstp).or.ocflg2(12,nnstp))
c ------print and save geostatic stress
      if(ocflg2(13,nnstp).or.ocflg2(14,nnstp)) then
         do kl=1,nlay
            kkl=kl
            if(ocflg2(13,nnstp)) then
               if(iswocf(7).lt.0) call ulaprs(gl(:,:,kkl),text(7),kstp,
     &           kper,ncol,nrow,kkl,-iswocf(7),iout)
               if(iswocf(7).ge.0) call ulaprw(gl(:,:,kkl),text(7),kstp,
     &           kper,ncol, nrow,kkl,iswocf(7),iout)
            endif
            if(ocflg2(14,nnstp)) then
               call ulasav(gl(:,:,kkl),text(7),kstp,kper,pertim,totim,
     &            ncol,nrow,kkl,iswocu(7))
            endif
         enddo
      endif  !if(ocflg2(13,nnstp).or.ocflg2(14,nnstp))
c
c ------print and save change in geostatic stress
      if(ocflg2(15,nnstp).or.ocflg2(16,nnstp)) then
         do k=1,nlay
            do i=1,nrow
               do j=1,ncol
                  buff(j,i,k)=gl(j,i,k)-gl0(j,i,k)
               enddo
            enddo
         enddo

         do kl=1,nlay
            kkl=kl
            if(ocflg2(15,nnstp)) then
               if(iswocf(8).lt.0) call ulaprs(buff(:,:,kkl),text(8),
     &           kstp,kper,ncol,nrow,kkl,-iswocf(8),iout)
               if(iswocf(8).ge.0) call ulaprw(buff(:,:,kkl),text(8),
     &           kstp,kper,ncol, nrow,kkl,iswocf(8),iout)
            endif

            if(ocflg2(16,nnstp)) then
               call ulasav(buff(:,:,kkl),text(8),kstp,kper,pertim,totim,
     &           ncol,nrow,kkl,iswocu(8))
            endif
         enddo
      endif !if(ocflg2(15,nnstp).or.ocflg2(16,nnstp))
c ------print and save effective stress
      if(ocflg2(17,nnstp).or.ocflg2(18,nnstp)) then
         do kl=1,nlay
            kkl=kl
            if(ocflg2(17,nnstp)) then
               if(iswocf(9).lt.0) call ulaprs(est(:,:,kkl),text(9),kstp,
     &          kper,ncol,nrow,kkl,-iswocf(9),iout)
               if(iswocf(9).ge.0) call ulaprw(est(:,:,kkl),text(9),kstp,
     &          kper,ncol, nrow,kkl,iswocf(9),iout)
            endif
            if(ocflg2(18,nnstp)) then
               call ulasav(est(:,:,kkl),text(9),kstp,kper,pertim,totim,
     &          ncol,nrow,kkl,iswocu(9))
            endif
         enddo
      endif !if(ocflg2(17,nnstp).or.ocflg2(18,nnstp))
c
c ------print and save change in effective stress
      if(ocflg2(19,nnstp).or.ocflg2(20,nnstp)) then
         do k=1,nlay
            do i=1,nrow
               do j=1,ncol
                  buff(j,i,k)=est(j,i,k)-est0(j,i,k)
               enddo
            enddo
         enddo
c
         do kl=1,nlay
            kkl=kl
            if(ocflg2(19,nnstp)) then
               if(iswocf(10).lt.0) call ulaprs(buff(:,:,kkl),text(10),
     &          kstp,kper,ncol,nrow,kkl,-iswocf(10),iout)
               if(iswocf(10).ge.0) call ulaprw(buff(:,:,kkl),text(10),
     &          kstp,kper,ncol, nrow,kkl,iswocf(10),iout)
            endif
c
            if(ocflg2(20,nnstp)) then
               call ulasav(buff(:,:,kkl),text(10),kstp,kper,pertim,
     &           totim,ncol,nrow,kkl,iswocu(10))
            endif
         enddo
      endif !if(ocflg2(19,nnstp).or.ocflg2(20,nnstp))
c
c7------print and store void ratio for each system of interbeds.
      if(ocflg2(21,nnstp).or.ocflg2(22,nnstp)) then
         do kq=1,nsystm
            k=lnwt(kq)
            if(ocflg2(21,nnstp)) then
               write(iout,76) kq
               if(iswocf(11).lt.0) call ulaprs(void(:,:,kq),text(11),
     &            kstp,kper,ncol,nrow,k,-iswocf(11),iout)
               if(iswocf(11).ge.0) call ulaprw(void(:,:,kq),text(11),
     &            kstp,kper,ncol,nrow,k,iswocf(11),iout)
            endif
c
            if(ocflg2(22,nnstp)) then
               call ulasav(void(:,:,kq),text(11),kstp,kper,pertim,totim,
     &              ncol,nrow,kq,iswocu(11))
            endif
         enddo
      endif !if(ocflg2(21,nnstp).or.ocflg2(22,nnstp))
c
c7------print and store thickness for each system of interbeds.
      if(ocflg2(23,nnstp).or.ocflg2(24,nnstp)) then
         do kq=1,nsystm
            k=lnwt(kq)
            if(ocflg2(23,nnstp)) then
               write(iout,76) kq
               if(iswocf(12).lt.0) call ulaprs(thick(:,:,kq),text(12),
     &            kstp,kper,ncol,nrow,k,-iswocf(12),iout)
               if(iswocf(12).ge.0) call ulaprw(thick(:,:,kq),text(12),
     &            kstp,kper,ncol,nrow,k,iswocf(12),iout)
            endif
            if(ocflg2(24,nnstp)) then
               call ulasav(thick(:,:,kq),text(12),kstp,kper,pertim,
     &              totim,ncol,nrow,kq,iswocu(12))
            endif
         enddo
      endif !if(ocflg2(23,nnstp).or.ocflg2(24,nnstp))
c
c.... zelevation follows:
c7------print and store layer-center elevation for each layer.
      if(ocflg2(25,nnstp).or.ocflg2(26,nnstp)) then
         do kl=1,nlay
            kkl=kl
            if(ocflg2(25,nnstp)) then
               if(iswocf(13).lt.0) call ulaprs(zc(:,:,kkl),text(13),
     &            kstp,kper,ncol,nrow,kkl,-iswocf(13),iout)
               if(iswocf(13).ge.0) call ulaprw(zc(:,:,kkl),text(13),
     &            kstp,kper,ncol,nrow,kkl,iswocf(13),iout)
            endif
c
            if(ocflg2(26,nnstp)) then
               call ulasav(zc(:,:,kkl),text(13),kstp,kper,pertim,totim,
     &              ncol,nrow,kkl,iswocu(13))
            endif
         enddo
      endif !if(ocflg2(25,nnstp).or.ocflg2(26,nnstp))
c
c  -----return
  900 return
      end subroutine gwf2scr7ot
c-----------------------------------------------------------------------
c
c subr 6
      subroutine sscr7z(ibound,hnew,botm,zc,nrow,ncol,nlay)
c     ******************************************************************
c     compute layer center elevation
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      implicit none
      integer                                 :: nrow,ncol,nlay
      real*8                                  :: hnew(ncol,nrow,nlay)
      logical                                 :: ttop,tbot
      integer                                 :: ibound(ncol,nrow,nlay)
      real                                    :: botm(ncol,nrow,0:nlay)
      real                                    :: zc(ncol,nrow,nlay)
      real                                    :: hhnew
      real                                    :: zb
      real                                    :: zt
      integer                                 :: k,ir,ic
c
      do k=1,nlay
         do ir=1,nrow
            do ic=1,ncol
               hhnew=hnew(ic,ir,k)
               zb=botm(ic,ir,k)
               zt=botm(ic,ir,k-1)
               if(ibound(ic,ir,k).eq.0) then
                  zc(ic,ir,k)=(zt+zb)*0.5
                  cycle
               endif
c
c ------compute center elevation as midpoint between bottom and
c ------head elevation
               if(hhnew.lt.zt.and.hhnew.gt.zb) then
c ------wt in cell
                  zc(ic,ir,k)=(hhnew+zb)*0.5
                  cycle
               else
c ------wt is above or below cell
                  zc(ic,ir,k)=(zt+zb)*0.5
               endif
            enddo
         enddo
      enddo
c
c ------return
      return
      end subroutine sscr7z
c
c----------------------------------------------------------------------
c subr 7
      subroutine sscr7g(ibound,hnew,botm,gl,sgm,sgs,nrow,ncol,nlay)
c     ******************************************************************
c     compute geostatic stress
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      implicit none
      integer                                 :: nrow,ncol,nlay
      real*8                                  :: hnew(ncol,nrow,nlay)
      real                                    :: gl(ncol,nrow,0:nlay)
      real                                    :: sgm(ncol,nrow)
      real                                    :: sgs(ncol,nrow)
      integer                                 :: ibound(ncol,nrow,nlay)
      real                                    :: botm(ncol,nrow,0:nlay)
c
      integer                                 :: ir,jc,k
      integer                                 :: isat
      real                                    :: h
c
      do ir=1,nrow
         do jc=1,ncol
c if a vertical column of cells contains no active cells (ibound=0), it
c is out of the flow domain and we do not need to worry about geostatic
c stress. however, cells with ibound=0 above an active cell must be considered
c as "unsaturated." this could be cells for which ibound was originally
c zero and cells that went dry during the simulation i don't think it matters
c for our calculations here because modflow updates ibound when cells saturate
c or desaturate. cells with ibound=0 below active cells likely are out of the
c aquifer. note that when ibound=0, hnew is not a meaningful number for
c calculations. constant head cells have an ibound < 0. i guess we should
c consider these as saturated with the constant-head value indicating the
c elevation of the water level. this would be the same treatment as
c ibound > 0.
            do k=1,nlay
               if(ibound(jc,ir,k).ne.0) then
                  h=hnew(jc,ir,k)
                   isat=1
               else
                  h=0.0
                  isat=0
               endif
c     if cell fully unsaturated
c bhl      if (h.lt.botm(jc,ir,k).or.isat.eq.0) then
               if (h.le.botm(jc,ir,k).or.isat.eq.0) then
                  gl(jc,ir,k)=gl(jc,ir,k-1)+
     &               (botm(jc,ir,k-1)-botm(jc,ir,k))*sgm(jc,ir)
                  cycle
               endif
c
c     if cell fully saturated
c bhl      if (h.gt.botm(jc,ir,k-1)) then
               if (h.ge.botm(jc,ir,k-1)) then
                  gl(jc,ir,k)=gl(jc,ir,k-1)+
     &              (botm(jc,ir,k-1)-botm(jc,ir,k))*sgs(jc,ir)
                  cycle
               endif
c
c     if cell partially saturated
               if (h.lt.botm(jc,ir,k-1).and.h.gt.botm(jc,ir,k)) then
                  gl(jc,ir,k)=gl(jc,ir,k-1)+
     &              (botm(jc,ir,k-1)-h)*sgm(jc,ir)+
     &              (h-botm(jc,ir,k))*sgs(jc,ir)
                  cycle
               endif
            enddo
         enddo
      enddo
c
c ------return
      return
      end subroutine sscr7g
c
c-----------------------------------------------------------------------
c subr 8
      subroutine sscr7e(ibound,hnew,botm,gl,est,nrow,ncol,nlay,iout)
c     ******************************************************************
c     compute effective stress
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      implicit none
      integer                                 :: nrow,ncol,nlay
      integer                                 :: iout
      real*8                                  :: hnew(ncol,nrow,nlay)
      real                                    :: est(ncol,nrow,nlay)
      real                                    :: gl(ncol,nrow,0:nlay)
      integer                                 :: ibound(ncol,nrow,nlay)
      real                                    :: botm(ncol,nrow,0:nlay)
c
      integer                                 :: k,ir,ic
      real                                    :: hhnew
c
      do k=1,nlay
         do ir=1,nrow
            do ic=1,ncol
               est(ic,ir,k)=0.0
               if(ibound(ic,ir,k).eq.0) cycle
!               if (hnew(ic,ir,k).lt.gl(ic,ir,k)) then
               hhnew=hnew(ic,ir,k)
!              else
!                  hhnew=gl(ic,ir,k)
!              endif
                est(ic,ir,k)=gl(ic,ir,k)-hhnew+botm(ic,ir,k)
                if(est(ic,ir,k).lt.0.0) then
!         write(*,*) ibound(ic,ir,k),hnew(ic,ir,k),ic,ir,k
                   write(iout,5) ir,ic,k
    5              format(' negative effective stress value at ',
     &                '(row,col,lay): ',3i5,/,'   aborting...')
                   call ustop('')
                endif
!      if (est(ic,ir,k).lt.0.0) then
!        est(ic,ir,k)=1e-10
!      endif
             enddo
         enddo
      enddo
c
c ------return
      return
      end subroutine sscr7e
c
c----------------------------------------------------------------------
c subr 9
      subroutine gwf2scr7da(igrid)
c  deallocate scr memory
      use gwfscrmodule

      implicit none
      integer                                 :: igrid

      call sgwf2scr7pnt(igrid)

      deallocate(iscrcb,iscroc,nsystm,ithk,ivoid,istpcs, !isincr,
     &     izcfl,izcfm,iglfl,iglfm,iestfl,iestfm,ipcsfl,ipcsfm,alpha) !,
!     &     istfl,istfm)

      deallocate(iswocf,iswocu,ifl2)

      deallocate(oclay2,lnwt,ntssm2,ocflg2,sgs,sgm,pcs,pcsold,pcsoff,
     &  thick,isoarr,isobcr,isocca,sub,void,est,estold,gl,zc,pcs0,gl0,
     &  est0)

      deallocate(nIntegralEs,oIntegralEs)
      deallocate(epTotal)
      deallocate(nobssub,irowobssub,icolobssub)
c
      return
      end subroutine gwf2scr7da
c
c----------------------------------------------------------------------
c subr 10
      subroutine sgwf2scr7psv(igrid)
c  save scr data for a  grid.
      use gwfscrmodule
c
      implicit none

      integer                                 :: igrid
c
      gwfscrdat(igrid)%iscrcb=>iscrcb
      gwfscrdat(igrid)%iscroc=>iscroc
      gwfscrdat(igrid)%nsystm=>nsystm
      gwfscrdat(igrid)%ithk=>ithk
      gwfscrdat(igrid)%ivoid=>ivoid
      gwfscrdat(igrid)%istpcs=>istpcs
!      gwfscrdat(igrid)%isincr=>isincr
      gwfscrdat(igrid)%alpha=>alpha

      gwfscrdat(igrid)%izcfl=>izcfl
      gwfscrdat(igrid)%izcfm=>izcfm
      gwfscrdat(igrid)%iglfl=>iglfl
      gwfscrdat(igrid)%iglfm=>iglfm
      gwfscrdat(igrid)%iestfl=>iestfl
      gwfscrdat(igrid)%iestfm=>iestfm
      gwfscrdat(igrid)%ipcsfl=>ipcsfl
      gwfscrdat(igrid)%ipcsfm=>ipcsfm
      gwfscrdat(igrid)%iswocf=>iswocf
      gwfscrdat(igrid)%iswocu=>iswocu
      gwfscrdat(igrid)%ifl2=>ifl2
      gwfscrdat(igrid)%oclay2=>oclay2
      gwfscrdat(igrid)%lnwt=>lnwt
      gwfscrdat(igrid)%ntssm2=>ntssm2
      gwfscrdat(igrid)%ocflg2=>ocflg2
      gwfscrdat(igrid)%sgs=>sgs
      gwfscrdat(igrid)%sgm=>sgm
      gwfscrdat(igrid)%pcs=>pcs
      gwfscrdat(igrid)%pcsold=>pcsold
      gwfscrdat(igrid)%pcsoff=>pcsoff
      gwfscrdat(igrid)%thick=>thick

      gwfscrdat(igrid)%nIntegralEs=>nIntegralEs
      gwfscrdat(igrid)%oIntegralEs=>oIntegralEs
      gwfscrdat(igrid)%epTotal=>epTotal

      gwfscrdat(igrid)%isoarr=>isoarr
      gwfscrdat(igrid)%isobcr=>isobcr
      gwfscrdat(igrid)%isocca=>isocca
      gwfscrdat(igrid)%subtime=>subtime

      gwfscrdat(igrid)%sub=>sub
      gwfscrdat(igrid)%void=>void
      gwfscrdat(igrid)%est=>est
      gwfscrdat(igrid)%estold=>estold
      gwfscrdat(igrid)%gl=>gl
      gwfscrdat(igrid)%zc=>zc
      gwfscrdat(igrid)%pcs0=>pcs0
      gwfscrdat(igrid)%gl0=>gl0
      gwfscrdat(igrid)%est0=>est0

      gwfscrdat(igrid)%nobssub=>nobssub
      gwfscrdat(igrid)%irowobssub=>irowobssub
      gwfscrdat(igrid)%icolobssub=>icolobssub
c
      return
      end subroutine sgwf2scr7psv
c
c----------------------------------------------------------------------
c subr 11
      subroutine sgwf2scr7pnt(igrid)
c  change scr data to a different grid.
      use gwfscrmodule
c
      iscrcb=>gwfscrdat(igrid)%iscrcb
      iscroc=>gwfscrdat(igrid)%iscroc
      nsystm=>gwfscrdat(igrid)%nsystm
      ithk=>gwfscrdat(igrid)%ithk
      ivoid=>gwfscrdat(igrid)%ivoid
      istpcs=>gwfscrdat(igrid)%istpcs
      !isincr=>gwfscrdat(igrid)%isincr
      alpha=>gwfscrdat(igrid)%alpha

      izcfl=>gwfscrdat(igrid)%izcfl
      izcfm=>gwfscrdat(igrid)%izcfm
      iglfl=>gwfscrdat(igrid)%iglfl
      iglfm=>gwfscrdat(igrid)%iglfm
      iestfl=>gwfscrdat(igrid)%iestfl
      iestfm=>gwfscrdat(igrid)%iestfm
      ipcsfl=>gwfscrdat(igrid)%ipcsfl
      ipcsfm=>gwfscrdat(igrid)%ipcsfm
      iswocf=>gwfscrdat(igrid)%iswocf
      iswocu=>gwfscrdat(igrid)%iswocu
      ifl2=>gwfscrdat(igrid)%ifl2
      oclay2=>gwfscrdat(igrid)%oclay2
      lnwt=>gwfscrdat(igrid)%lnwt
      ntssm2=>gwfscrdat(igrid)%ntssm2
      ocflg2=>gwfscrdat(igrid)%ocflg2
      sgs=>gwfscrdat(igrid)%sgs
      sgm=>gwfscrdat(igrid)%sgm
      pcs=>gwfscrdat(igrid)%pcs
      pcsold=>gwfscrdat(igrid)%pcsold
      pcsoff=>gwfscrdat(igrid)%pcsoff
      thick=>gwfscrdat(igrid)%thick

      oIntegralEs=>gwfscrdat(igrid)%oIntegralEs
      nIntegralEs=>gwfscrdat(igrid)%nIntegralEs
      epTotal=>gwfscrdat(igrid)%epTotal

      isoarr=>gwfscrdat(igrid)%isoarr
      isobcr=>gwfscrdat(igrid)%isobcr
      isocca=>gwfscrdat(igrid)%isocca
      subtime=>gwfscrdat(igrid)%subtime

      sub=>gwfscrdat(igrid)%sub
      void=>gwfscrdat(igrid)%void
      est=>gwfscrdat(igrid)%est
      estold=>gwfscrdat(igrid)%estold
      gl=>gwfscrdat(igrid)%gl
      zc=>gwfscrdat(igrid)%zc
      pcs0=>gwfscrdat(igrid)%pcs0

      gl0=>gwfscrdat(igrid)%gl0
      est0=>gwfscrdat(igrid)%est0

      nobssub=>gwfscrdat(igrid)%nobssub
      irowobssub=>gwfscrdat(igrid)%irowobssub
      icolobssub=>gwfscrdat(igrid)%icolobssub
c
      return
      end subroutine sgwf2scr7pnt
c
c----------------------------------------------------------------------
c since we need to know in advance at what times modflow will do calculation
c this routine calculate an array of time steps
c it is basically the routine gwf2bas7ad with few modifications.
c
      subroutine gwf2scr1tm(igrid)
c     ******************************************************************
c     calculate modflow time steps
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      use global,       only: perlen,nstp,tsmult,nper,issflg
      use gwfscrmodule, only: nstptotscr,totimscr,pertimscr,deltallscr,
     &                        iddeltscr
c     ------------------------------------------------------------------
c
      implicit none
      real delt, totim, one, deltscr(nstptotscr)
      integer igrid,kper, kstp,it, kstart
c     ------------------------------------------------------------------
      !call sgwf2swt1pnt(igrid)
c
      it=0
      kstart=1
      do kper = 1, nper
c ------assume time step multiplier is equal to one.
        delt=perlen(kper)/float(nstp(kper))
c
c ------if time step multiplier is not one then calculate first
c ------term of geometric progression.
        one=1.
        if(tsmult(kper).ne.one)
     1      delt=perlen(kper)*(one-tsmult(kper))/
     2          (one-tsmult(kper)**nstp(kper))
c
c3------print the length of the first time step.
!      write (iout,9) delt
!    9 format(1x,/28x,'initial time step size =',g15.7)
c
c4------initialize pertim (elapsed time within stress period).
        pertimscr(kper)=0.
c
        do kstp = 1, nstp(kper)
          it=it+1
c1------if not first time step then calculate time step length.
          if(kstp.ne.1) then
            delt=tsmult(kper)*delt
          endif
          if (issflg(kper).ne.0) then !only first stress period is allowed to br ss
            delt=0
          endif
          deltallscr(it)=delt
          iddeltscr(kper,kstp)=it !this is to map between mf and scr time indices
c
c2------accumulate elapsed time in simulation(totim) and in this
c2------stress period(pertim).
          deltscr(it)=delt
          if (it.eq.1) then
            totimscr(it)=delt
          else
            totimscr(it)=totimscr(it-1)+delt
          endif
          pertimscr(kper)=pertimscr(kper)+delt
        enddo

      enddo !do kper
c
c
c4------return
      return
      end subroutine gwf2scr1tm
c
c----------------------------------------------------------------------
c this subroutine implement the isotache method.
c
      subroutine isotache(ep,es,nIntegral,oIntegral,oS,nS,oPS,nPS,
     &                    a,b,c,oTime,nTime)
      implicit none
      real*8                                        :: ep         !> Primary strain
      real*8                                        :: es         !> secondary strain
      real*8                                        :: nIntegral  !> new integral part
      real*8                                        :: oIntegral  !> old integral part
      real*8                                        :: oS         !> Effective stress at previous time step
      real*8                                        :: nS         !> Effective stress at current time step
      real*8                                        :: oPS        !> Preconsolidation stress at previous time step
      real*8                                        :: nPS        !> Preconsolidation stress at current time step
      real                                          :: a         !> Re-compression ratio
      real                                          :: b         !> Compression ratio
      real                                          :: c         !> Secondary Compression index

      real*8                                        :: oTime      !> time at the previous time step
      real*8                                        :: nTime      !> time at the curremt time step

      if (b < a) then
        write(*,*) "b < a; a is set equal to b"
        a=b
      endif

      if (c < 0.000001) then
        es=0.d0
        nIntegral=0.d0
      else
        call SecondaryStrainIsotache(es,nIntegral,oIntegral,
     &                               oS,oPS,nS,nPS,a,b,c,nTime-oTime)
      endif

      call primaryStrainIsotache(ep,oS,nS,nPS,a,b)

      return
      end subroutine isotache
c

c
c----------------------------------------------------------------------
c
      subroutine PrimaryStrainIsotache(ep,oS,nS,pS,a,b)
      implicit none
      real*8                                        :: ep         !> Primary strain
      real*8                                        :: oS         !> Effective stress at previous time step
      real*8                                        :: nS         !> Effective stress at current time step
      real*8                                        :: pS         !> Preconsolidation stress at current time step
      real                                          :: a          !> Re-compression ratio
      real                                          :: b          !> Compression ratio

      ep=0.d0
      if (oS < nS .and. nS < pS) then
         ep=a*log(nS/oS)
      elseif (pS < nS .and. nS > oS) then
         ep=a* log(pS/oS) + b*log(nS/pS)
      elseif (nS < oS) then
        !Swelling: in this case pS should be >= nS
         ep=a*log(nS/oS)
      endif

      return
      end subroutine PrimaryStrainIsotache
c
c----------------------------------------------------------------------
      subroutine SecondaryStrainIsotache(es,nIntegral,oIntegral,oS,oPS,
     &                            nS,nPS,a,b,c,dt)
      implicit none
      real*8                                        :: es         !> secondary strain
      real*8                                        :: nIntegral  !> new integral part

      real*8                                        :: oIntegral  !> old integral part
      real*8                                        :: oS         !> Effective stress at previous time step
      real*8                                        :: nS         !> Effective stress at current time step
      real*8                                        :: oPS        !> Preconsolidation stress at previous time step
      real*8                                        :: nPS        !> Preconsolidation stress at current time step
      real                                          :: a          !> Re-compression ratio
      real                                          :: b          !> Compression ratio
      real                                          :: c         !> Secondary Compression index
      real*8                                        :: dt         !> time step length

      real*8                                        :: cons
      real*8                                        :: nTocr
      real*8                                        :: nTerm

      cons=(b-a)/c

      nTocr=nS/nPs
      if (nTocr > 1) then
        nTocr = 1
      endif
      nTerm=nTocr**cons

      nIntegral=oIntegral+dt*nTerm
      es = c * log(1.d0 + nIntegral)

      return
      end subroutine SecondaryStrainIsotache

c----------------------------------------------------------------------
      subroutine bjerrum(ep,es,nIntegral,oIntegral,oS,nS,oPS,nPS,
     &                    RR,CR,Ca,oTime,nTime)
      implicit none
      real*8                                        :: ep         !> Primary strain
      real*8                                        :: es         !> secondary strain
      real*8                                        :: nIntegral  !> new integral part
      real*8                                        :: oIntegral  !> old integral part
      real*8                                        :: oS         !> Effective stress at previous time step
      real*8                                        :: nS         !> Effective stress at current time step
      real*8                                        :: oPS        !> Preconsolidation stress at previous time step
      real*8                                        :: nPS        !> Preconsolidation stress at current time step
      real                                          :: RR         !> Re-compression ratio
      real                                          :: CR         !> Compression ratio
      real                                          :: Ca         !> Secondary Compression index

      real*8                                        :: oTime      !> time at the previous time step
      real*8                                        :: nTime      !> time at the curremt time step

      if (CR < RR) then
        write(*,*) "CR < RR; RR is set equal to CR"
        RR=CR
      endif

      if (Ca < 0.000001) then
        es=0.d0
        nIntegral=0.d0
      else
        call SecondaryStrainBjerrum(es,nIntegral,oIntegral,
     &                               oS,oPS,nS,nPS,RR,CR,Ca,nTime-oTime)
      endif

      call primaryStrainBjerrum(ep,oS,nS,nPS,RR,CR)

      return
      end subroutine bjerrum
c
c----------------------------------------------------------------------
c
      subroutine PrimaryStrainBjerrum(ep,oS,nS,pS,RR,CR)
      implicit none
      real*8                                        :: ep         !> Primary strain
      real*8                                        :: oS         !> Effective stress at previous time step
      real*8                                        :: nS         !> Effective stress at current time step
      real*8                                        :: pS         !> Preconsolidation stress at current time step
      real                                          :: RR         !> Re-compression ratio
      real                                          :: CR         !> Compression ratio

      ep=0
      if (oS < nS .and. nS < pS) then
        ep=RR*log10(nS/oS)
      elseif (pS < nS .and. nS > oS) then
        ep=RR*log10(pS/oS)+CR*log10(nS/pS)
      elseif (nS < oS) then
        !Swelling: in this case pS should be >= nS
        ep=RR*log10(nS/oS)
      endif

      return
      end subroutine PrimaryStrainBjerrum
c
c----------------------------------------------------------------------
      subroutine SecondaryStrainBjerrum(es,nIntegral,oIntegral,oS,oPS,
     &                            nS,nPS,RR,CR,Ca,dt)
      implicit none
      real*8                                        :: es         !> secondary strain
      real*8                                        :: nIntegral  !> new integral part

      real*8                                        :: oIntegral  !> old integral part
      real*8                                        :: oS         !> Effective stress at previous time step
      real*8                                        :: nS         !> Effective stress at current time step
      real*8                                        :: oPS        !> Preconsolidation stress at previous time step
      real*8                                        :: nPS        !> Preconsolidation stress at current time step
      real                                          :: RR         !> Re-compression ratio
      real                                          :: CR         !> Compression ratio
      real                                          :: Ca         !> Secondary Compression index
      real*8                                        :: dt         !> time step length

      real*8                                        :: cons
      real*8                                        :: nTocr
      real*8                                        :: nTerm

      cons=(CR-RR)/Ca

      nTocr=nS/nPs
      if (nTocr > 1) then
        nTocr = 1
      endif
      nTerm=nTocr**cons

      nIntegral=oIntegral+dt*nTerm
      es = Ca * log10(1.d0 + nIntegral)

      return
      end subroutine SecondaryStrainBjerrum
c----------------------------------------------------------------------
c

