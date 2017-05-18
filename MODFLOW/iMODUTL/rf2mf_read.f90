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

MODULE MOD_RF2MF_READ

USE IMOD_UTL, ONLY : IMOD_UTL_ITOS,IMOD_UTL_RTOS,IMOD_UTL_GETFNAME,IMOD_UTL_GETREAL,IMOD_UTL_PRINTTEXT, &
                          IMOD_UTL_STRING,IMOD_UTL_FILENAME,IUOUT
USE rf2mf_module, ONLY : nlay,bas,bcf,pwt,dis,wel,drn,riv,ghb,hfb,ani,rch,mxrech,evt,chd,iarr,iari,nper,&
                         oc,maxsubsys,maxgen,maxcol,scr,ialloc,allocscr,&
                         iusclnodata,iusclspec,iusclarith,iusclgeom,iusclsumq,iusclsumcdr,iusclinvc,iusclmostfr,iusclsuminvcvr,iusclperc,&
                         idsclnodata,idsclnointp,idsclintp

USE MOD_RF2MF

INTEGER :: IOS

PRIVATE

PUBLIC :: RF2MF_READ1MAIN
PUBLIC :: RF2MF_READ1MAIN_system
PUBLIC :: RF2MF_READ_IDF

CONTAINS

 !###====================================================================
 SUBROUTINE RF2MF_READ1MAIN(IPCK,IMODPCK)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPCK,IMODPCK
 INTEGER :: ILAY,IROW,ICOL,IT,NTOPICS,isub,nsys,msys,jlay,iper
 REAL :: CONSTANTE,FCT,IMP,NODATA
 LOGICAL :: LLAY
 INTEGER,DIMENSION(5) :: ITP
 CHARACTER(LEN=256) :: FNAME
 DATA ITP/1,2,3,4,5/
 integer :: scltype, ismooth,isys

 IF(IMODPCK.EQ.0)NTOPICS=MDIM(IPCK)
 IF(IMODPCK.EQ.1)THEN
  NTOPICS=PDIM(IPCK)
  !## reset reuse variable on default on .false.
  SELECT CASE (IPCK)
   CASE (PWEL)     !## (PWEL) well
    if(associated(wel%sp))wel%sp(kper)%reuse = nlines.lt.0 
   CASE (PDRN)     !## (PDRN) drainage
    if(associated(drn%sp))then
     !## only apply reuse whenever lolf defined as reuse
     if(nlines.ge.0)then
      drn%sp(kper)%ldrn=.true.
      if (.not.associated(drn%sp(kper)%gcd%subsys)) allocate(drn%sp(kper)%gcd%subsys(maxsubsys))
     endif
     if(drn%sp(kper)%lolf)then
      if(drn%sp(kper)%reuse.and.nlines.ge.0)drn%sp(kper)%reuse=.false.
     else
      drn%sp(kper)%reuse = nlines.lt.0 
     endif
    endif
   CASE (PRIV)     !## (PRIV) rivers
    if(associated(riv%sp))then
     if(nlines.ge.0)then
      riv%sp(kper)%lriv=.true.
      if (.not.associated(riv%sp(kper)%gcd%subsys)) allocate(riv%sp(kper)%gcd%subsys(maxsubsys))
     endif
     !## only apply reuse whenever lolf defined as reuse
     if(riv%sp(kper)%lisg)then
      if(riv%sp(kper)%reuse.and.nlines.ge.0)riv%sp(kper)%reuse=.false.
     else
      riv%sp(kper)%reuse = nlines.lt.0 
     endif
    endif
   CASE (PEVT)     !## (PEVT) evapotranspiration
    if(associated(evt%sp))evt%sp(kper)%reuse = nlines.lt.0 
   CASE (PGHB)     !## (PGHB) general head boundary
    if(associated(ghb%sp))ghb%sp(kper)%reuse = nlines.lt.0 
   CASE (PRCH)     !## (PRCH) recharge
    if(associated(rch%sp))rch%sp(kper)%reuse = nlines.lt.0 
   CASE (POLF)     !## (POLF) overlandflow
    if(associated(drn%sp))then
     if(nlines.ge.0)then
      drn%sp(kper)%lolf=.true.
      if (.not.associated(drn%sp(kper)%gcd%subsys)) allocate(drn%sp(kper)%gcd%subsys(maxsubsys))
     endif
     !## only apply reuse whenever ldrn defined as reuse
     if(drn%sp(kper)%ldrn)then
      if(drn%sp(kper)%reuse.and.nlines.ge.0)drn%sp(kper)%reuse=.false.
     else
      drn%sp(kper)%reuse = nlines.lt.0 
     endif
    endif
   CASE (PCHD)     !## (PCHD) constant head
    if(associated(chd%sp))chd%sp(kper)%reuse = nlines.lt.0 
   CASE (PISG)     !## (PISG) riversegments
    if(associated(riv%sp))then
     if(nlines.ge.0)then
      riv%sp(kper)%lisg=.true.
      if (.not.associated(riv%sp(kper)%gcd%subsys)) allocate(riv%sp(kper)%gcd%subsys(maxsubsys))
     endif
     !## only apply reuse whenever lolf defined as reuse
     if(riv%sp(kper)%lriv)then
      if(riv%sp(kper)%reuse.and.nlines.ge.0)riv%sp(kper)%reuse=.false.
     else
      riv%sp(kper)%reuse = nlines.lt.0 
     endif
    endif
  END SELECT
 ENDIF
 
 !## read three extra idf files for scr module
 IF(IMODPCK.EQ.0.AND.IPCK.EQ.PSCR.AND.MMOD(PSCR).EQ.1)THEN
  scr%nsys=nlines; call AllocScr(ialloc)
  DO IT=1,3
   READ(IURUN,'(A256)',IOSTAT=IOS) LINE
   IF(IOS.NE.0)THEN
    CALL IMOD_UTL_PRINTTEXT('Error reading '//TRIM(LINE),0,IUOUT)
    CALL IMOD_UTL_PRINTTEXT('Busy processing module: '//TRIM(CMOD(IPCK)),2,IUOUT)
   ENDIF
   CALL RF2MF_READ_IDF(LINE,FCT,IMP,ILAY,CONSTANTE,FNAME,IOS,IMODPCK,PCAP) !,IPCK) --- because i need no layer (is trick)
   IF(IT.EQ.1)call RF2MF_READ1MAIN_system(scr%gl0(1),ios,ilay,fct,imp,simcsize,iarr,fname,iusclarith,idsclintp)
   IF(IT.EQ.2)call RF2MF_READ1MAIN_system(scr%sgm(1),ios,ilay,fct,imp,simcsize,iarr,fname,iusclarith,idsclintp)
   IF(IT.EQ.3)call RF2MF_READ1MAIN_system(scr%sgs(1),ios,ilay,fct,imp,simcsize,iarr,fname,iusclarith,idsclintp)
  ENDDO
 ENDIF

 DO IT=1,NTOPICS
  nsys=0

  DO isub=1,NLINES

   READ(IURUN,'(A256)',IOSTAT=IOS) LINE
   IF(IOS.NE.0)THEN
    CALL IMOD_UTL_PRINTTEXT('Error reading '//TRIM(LINE),0,IUOUT)
    IF(IMODPCK.EQ.0)CALL IMOD_UTL_PRINTTEXT('Busy processing module: '//TRIM(CMOD(IPCK)),2,IUOUT)
    IF(IMODPCK.EQ.1)CALL IMOD_UTL_PRINTTEXT('Busy processing package: '//TRIM(CPCK(IPCK)),2,IUOUT)
   ENDIF

   LLAY=.FALSE.
   CALL RF2MF_READ_IDF(LINE,FCT,IMP,ILAY,CONSTANTE,FNAME,IOS,IMODPCK,IPCK)

   IF(IMODPCK.EQ.0)THEN
    IF(IPCK.EQ.PHFB)THEN
     IF(ILAY.GE.0.AND.ILAY.LT.NLAY)LLAY=.TRUE.
    ELSE
     IF(ILAY.GE.1.AND.ILAY.LT.NLAY)LLAY=.TRUE.
    ENDIF
    IF((IPCK.NE.PVCW.OR.IPCK.NE.PKVV).AND.ILAY.EQ.NLAY)LLAY=.TRUE.
    IF((IPCK.EQ.PVCW.OR.IPCK.EQ.PKVV).AND.ILAY.EQ.NLAY)LLAY=.FALSE.
    IF(MMOD(IPCK).EQ.0)LLAY=.FALSE.    !## not active in header
   ELSEIF(IMODPCK.EQ.1)THEN
    IF(ILAY.GE.0.AND.ILAY.LE.NLAY)LLAY=.TRUE.
    IF(MPCK(IPCK).EQ.0)LLAY=.FALSE.    !## not active in header
   ENDIF

   IF(LLAY)THEN

    IF(IMODPCK.EQ.0)THEN
     SELECT CASE (IPCK)
      CASE (PCAP)     !## (PCAP) capsim
      CASE (PBND)     !## (PBND) boundary
       call RF2MF_READ1MAIN_system(bas%ibound(ilay),ios,ilay,fct,imp,constante,iari,fname,iusclspec,idsclnointp)
      CASE (PSHD)     !## (PSHD) starting heads
       call RF2MF_READ1MAIN_system(bas%strt(ilay),ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
      CASE (PKDW)     !## (PKDW) horizontal conductance
       call RF2MF_READ1MAIN_system(bcf%tran(ilay),ios,ilay,fct,imp,constante,iarr,fname,iusclgeom,idsclintp)
      CASE (PVCW)     !## (PVCS) vertical conductance
       call RF2MF_READ1MAIN_system(bcf%vcont(ilay),ios,ilay,fct,imp,constante,iarr,fname,iusclinvc,idsclintp)
      CASE (PSTO)     !## (PSTO) storage coefficient
       call RF2MF_READ1MAIN_system(bcf%sf1(ilay),ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
      CASE (PPWT)     !## (PPWT) purge water table
       if(it.eq.1)call RF2MF_READ1MAIN_system(pwt%ilay,ios,ilay,fct,imp,constante,iarr,fname,iusclmostfr,idsclnointp)
       if(it.eq.2)call RF2MF_READ1MAIN_system(pwt%sc2,ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
       if(it.eq.3)call RF2MF_READ1MAIN_system(pwt%bot,ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
       if(it.eq.4)call RF2MF_READ1MAIN_system(pwt%top2,ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
       if(it.eq.5)call RF2MF_READ1MAIN_system(pwt%thkaf,ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
       if(it.eq.6)call RF2MF_READ1MAIN_system(pwt%vcont,ios,ilay,fct,imp,constante,iarr,fname,iusclinvc,idsclintp)
      CASE (PANI)     !## (PANI) anisotropy
       if(it.eq.1)call RF2MF_READ1MAIN_system(ani%factor(ilay),ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
       if(it.eq.2)call RF2MF_READ1MAIN_system(ani%angle(ilay),ios,ilay,fct,imp,constante,iarr,fname,iusclmostfr,idsclnointp)
      CASE (PHFB)     !## (PHFB) horizontal flow barrier
       hfb%sp(1:nper)%reuse = .false.
       hfb%sp(1:nper)%lcd%ngen = hfb%sp(1:nper)%lcd%ngen+1
       do iper = 1, nper
        if (.not.associated(hfb%sp(iper)%lcd%gen)) allocate(hfb%sp(iper)%lcd%gen(maxsubsys))
        if (.not.associated(hfb%sp(iper)%lcd%gen(isub)%data)) allocate(hfb%sp(iper)%lcd%gen(isub)%data(1))
        hfb%sp(iper)%lcd%gen(isub)%ilay   = ilay
        hfb%sp(iper)%lcd%gen(isub)%factor = fct+imp
        hfb%sp(iper)%lcd%gen(isub)%data(it)%fname = trim(fname)
       end do
      CASE (PTOP)     !## (PTOP) top
       dis%settop = .true.
       call RF2MF_READ1MAIN_system(dis%aquifertop(ilay),ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
      CASE (PBOT)     !## (PBOT) bot
       dis%setbot = .true.
       call RF2MF_READ1MAIN_system(dis%aquiferbot(ilay),ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
      CASE (PCON)     !## (PCON) con
      CASE (PKHV)     !## (PKHV) horizontal.k.value
       bcf%llpf = .true. ! lpf needed
       call RF2MF_READ1MAIN_system(bcf%hhy(ilay),ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
      CASE (PKVV)     !## (PKVV) vertical.k.value
       bcf%llpf = .true. ! lpf needed
       call RF2MF_READ1MAIN_system(bcf%vhy(ilay),ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
      CASE (PKVA)     !## (PKVA) horizontal vertical anisotropy (3d model)
       bcf%llpf = .true. ! lpf needed
       call RF2MF_READ1MAIN_system(bcf%kva(ilay),ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
      CASE (PIBS)     !## (PIBS) interbed storage
      CASE (PSCR)     !## (PSCR) subsidence with creep (MBakr)
       if(it.eq.1)call RF2MF_READ1MAIN_system(scr%thick(ilay),ios,ilay,fct,imp,simcsize,iarr,fname,iusclarith,idsclintp)
       if(it.eq.2)call RF2MF_READ1MAIN_system(scr%rrisoa(ilay),ios,ilay,fct,imp,simcsize,iarr,fname,iusclarith,idsclintp)
       if(it.eq.3)call RF2MF_READ1MAIN_system(scr%rrisob(ilay),ios,ilay,fct,imp,simcsize,iarr,fname,iusclarith,idsclintp)
       if(it.eq.4)call RF2MF_READ1MAIN_system(scr%caisoc(ilay),ios,ilay,fct,imp,simcsize,iarr,fname,iusclarith,idsclintp)
       if(it.eq.5)call RF2MF_READ1MAIN_system(scr%void(ilay),ios,ilay,fct,imp,simcsize,iarr,fname,iusclarith,idsclintp)
       if(it.eq.6)call RF2MF_READ1MAIN_system(scr%sub(ilay),ios,ilay,fct,imp,simcsize,iarr,fname,iusclmostfr,idsclintp)
      CASE (PSFT)
       riv%sft=.true.
       if(isub.eq.1) call RF2MF_READ1MAIN_system(riv%sft1,ios,ilay,fct,imp,simcsize,iarr,fname,iusclarith,idsclintp)
       if(isub.eq.2) call RF2MF_READ1MAIN_system(riv%sft2,ios,ilay,fct,imp,simcsize,iarr,fname,iusclarith,idsclintp)
     END SELECT
    ELSEIF(IMODPCK.EQ.1)THEN
     nsys=nsys+1

     SELECT CASE (IPCK)
      CASE (PWEL)     !## (PWEL) well
       wel%sp(kper)%reuse = .false.
       wel%sp(kper)%gcd%nsubsys = nsys
       if (.not.associated(wel%sp(kper)%gcd%subsys)) allocate(wel%sp(kper)%gcd%subsys(maxsubsys))
       wel%sp(kper)%gcd%subsys(nsys)%ilay = ilay
!       wel%sp(kper)%gcd%subsys(nsys)%factor = fct
       isys=isub; if(isumbudget.eq.1)isys=1
       wel%sp(kper)%gcd%subsys(nsys)%isub = isys
       if (.not.associated(wel%sp(kper)%gcd%subsys(nsys)%data)) allocate(wel%sp(kper)%gcd%subsys(nsys)%data(1))
       call RF2MF_READ1MAIN_system(wel%sp(kper)%gcd%subsys(nsys)%data(it),ios,ilay,fct,imp,constante,iarr,fname,iusclnodata,idsclnodata)
      CASE (PDRN)     !## (PDRN) drainage
       drn%sp(kper)%ldrn  = .true.
       drn%sp(kper)%reuse = .false.
       drn%sp(kper)%gcd%nsubsys = nsys
       if (.not. associated(drn%sp(kper)%gcd%subsys)) allocate(drn%sp(kper)%gcd%subsys(maxsubsys))
       drn%sp(kper)%gcd%subsys(nsys)%ilay = ilay
!       drn%sp(kper)%gcd%subsys(nsys)%factor = fct
       isys=isub; if(isumbudget.eq.1)isys=1
       drn%sp(kper)%gcd%subsys(nsys)%isub = isys
       drn%sp(kper)%gcd%subsys(nsys)%ldrn = .true.
       if (.not.associated(drn%sp(kper)%gcd%subsys(nsys)%data)) allocate(drn%sp(kper)%gcd%subsys(nsys)%data(3))
       if (itp(it).eq.1) then
        scltype=iusclsumcdr
        ismooth=idsclnointp
       else
        scltype=iusclarith
        ismooth=idsclnointp
       end if
       call RF2MF_READ1MAIN_system(drn%sp(kper)%gcd%subsys(nsys)%data(itp(it)),ios,ilay,fct,imp,constante,iarr,fname,scltype,ismooth)
       if (itp(it).eq.2 .and. iconchk.eq.1) then
         call RF2MF_READ1MAIN_system(drn%sp(kper)%gcd%subsys(nsys)%data(3),0,ilay,fct,imp,1.,iarr,fname,iusclmostfr,idsclnointp)
       end if
      CASE (PRIV)     !## (PRIV) rivers
       riv%sp(kper)%lriv  = .true.
       riv%sp(kper)%reuse = .false.
       riv%sp(kper)%gcd%nsubsys = nsys
       if (.not.associated(riv%sp(kper)%gcd%subsys)) allocate(riv%sp(kper)%gcd%subsys(maxsubsys))
       riv%sp(kper)%gcd%subsys(nsys)%ilay = ilay
!       riv%sp(kper)%gcd%subsys(nsys)%factor = fct
       isys=isub; if(isumbudget.eq.1)isys=1
       riv%sp(kper)%gcd%subsys(nsys)%isub = isys
       riv%sp(kper)%gcd%subsys(nsys)%lriv = .true.
       if (.not.associated(riv%sp(kper)%gcd%subsys(nsys)%data)) allocate(riv%sp(kper)%gcd%subsys(nsys)%data(5))
       if (itp(it).eq.1) then
        scltype=iusclsumcdr
        ismooth=idsclnointp
       else
        scltype=iusclarith
        ismooth=idsclnointp
       end if
       call RF2MF_READ1MAIN_system(riv%sp(kper)%gcd%subsys(nsys)%data(itp(it)),ios,ilay,fct,imp,constante,iarr,fname,scltype,ismooth)
      CASE (PEVT)     !## (PEVT) evapotranspiration
       evt%sp(kper)%reuse = .false.
       evt%sp(kper)%insurf = 1
       evt%sp(kper)%inevtr = 1
       evt%sp(kper)%inexdp = 1
       if (it.eq.1) call RF2MF_READ1MAIN_system(evt%sp(kper)%evtr,ios,ilay,fct,imp,0.001*constante,iarr,fname,iusclarith,idsclintp)
       if (it.eq.2) call RF2MF_READ1MAIN_system(evt%sp(kper)%surf,ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclnointp)
       if (it.eq.3) call RF2MF_READ1MAIN_system(evt%sp(kper)%exdp,ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclnointp)
      CASE (PGHB)     !## (PGHB) general head boundary
       ghb%sp(kper)%reuse = .false.
       ghb%sp(kper)%gcd%nsubsys = nsys
       if (.not.associated(ghb%sp(kper)%gcd%subsys)) allocate(ghb%sp(kper)%gcd%subsys(maxsubsys))
       ghb%sp(kper)%gcd%subsys(isub)%ilay = ilay
!       ghb%sp(kper)%gcd%subsys(nsys)%factor = fct
       isys=isub; if(isumbudget.eq.1)isys=1
       ghb%sp(kper)%gcd%subsys(isub)%isub = isys
       if (.not.associated(ghb%sp(kper)%gcd%subsys(nsys)%data)) allocate(ghb%sp(kper)%gcd%subsys(nsys)%data(3))
       if (itp(it).eq.1) then
        scltype=iusclsumcdr
        ismooth=idsclnointp
       else
        scltype=iusclarith
        ismooth=idsclnointp
       end if
       call RF2MF_READ1MAIN_system(ghb%sp(kper)%gcd%subsys(nsys)%data(itp(it)),ios,ilay,fct,imp,constante,iarr,fname,scltype,ismooth)
      CASE (PRCH)     !## (PRCH) recharge
       rch%sp(kper)%reuse = .false.
       rch%sp(kper)%inrech = rch%sp(kper)%inrech + 1
       if (rch%sp(kper)%inrech.gt.mxrech) call imod_utl_printtext('Error, increase mxrech',2)
       call RF2MF_READ1MAIN_system(rch%sp(kper)%rech(isub),ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
       rch%sp(kper)%rech(isub)%fct = rch%sp(kper)%rech(isub)%fct * 0.001
      CASE (POLF)     !## (POLF) overlandflow
       drn%sp(kper)%lolf  = .true.
       drn%sp(kper)%reuse = .false.
       msys = drn%sp(kper)%gcd%nsubsys+nsys
       drn%sp(kper)%gcd%nsubsys = msys
       drn%sp(kper)%gcd%subsys(msys)%lolf = .true.
       if (.not.associated(drn%sp(kper)%gcd%subsys)) allocate(drn%sp(kper)%gcd%subsys(maxsubsys))
       drn%sp(kper)%gcd%subsys(msys)%ilay =  ilay
       isys=isub; if(isumbudget.eq.1)isys=1
       drn%sp(kper)%gcd%subsys(msys)%isub = -isys !drn%sp(kper)%gcd%subsys(nsys)%isub + 1
       if (.not.associated(drn%sp(kper)%gcd%subsys(msys)%data)) allocate(drn%sp(kper)%gcd%subsys(msys)%data(3))
       call RF2MF_READ1MAIN_system(drn%sp(kper)%gcd%subsys(msys)%data(2),ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
       drn%sp(kper)%gcd%subsys(msys)%data(1)%keyword = 'constant'
       constante = simcsize*simcsize
       call RF2MF_READ1MAIN_system(drn%sp(kper)%gcd%subsys(msys)%data(1),0,ilay,fct,imp,constante,iarr,fname,iusclnodata,idsclnodata)
       if (iconchk.eq.1) then
         call RF2MF_READ1MAIN_system(drn%sp(kper)%gcd%subsys(msys)%data(3),0,ilay,fct,imp,1.,iarr,fname,iusclmostfr,idsclnointp)
       end if
      CASE (PCHD)     !## (PCHD) constant head
       chd%sp(kper)%reuse = .false.
       chd%sp(kper)%gcd%nsubsys = nsys
       if (.not.associated(chd%sp(kper)%gcd%subsys)) allocate(chd%sp(kper)%gcd%subsys(maxsubsys))
       chd%sp(kper)%gcd%subsys(nsys)%ilay = ilay
       isys=isub; if(isumbudget.eq.1)isys=1
       chd%sp(kper)%gcd%subsys(nsys)%isub = isys
       if (.not.associated(chd%sp(kper)%gcd%subsys(nsys)%data)) allocate(chd%sp(kper)%gcd%subsys(nsys)%data(3))
       call RF2MF_READ1MAIN_system(chd%sp(kper)%gcd%subsys(nsys)%data(1),ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
       call RF2MF_READ1MAIN_system(chd%sp(kper)%gcd%subsys(nsys)%data(2),ios,ilay,fct,imp,constante,iarr,fname,iusclarith,idsclintp)
      CASE (PISG)     !## (PISG) riversegments
       riv%sp(kper)%lisg  = .true.
       fct = 1.; imp = 0.
       riv%sp(kper)%reuse = .false.
       msys = riv%sp(kper)%gcd%nsubsys+1
       riv%sp(kper)%gcd%nsubsys = msys
       if (.not.associated(riv%sp(kper)%gcd%subsys)) allocate(riv%sp(kper)%gcd%subsys(maxsubsys))
       riv%sp(kper)%gcd%subsys(msys)%ilay =  ilay
       isys=isub; if(isumbudget.eq.1)isys=1
       riv%sp(kper)%gcd%subsys(msys)%isub = -isys
       riv%sp(kper)%gcd%subsys(msys)%lisg = .true.
       if (.not.associated(riv%sp(kper)%gcd%subsys(msys)%data)) allocate(riv%sp(kper)%gcd%subsys(msys)%data(4))
       call RF2MF_READ1MAIN_system(riv%sp(kper)%gcd%subsys(msys)%data(itp(it)),ios,ilay,fct,imp,constante,iarr,fname,iusclnodata,idsclnodata)
     END SELECT
    END IF

   ENDIF
  ENDDO
 ENDDO

 !## read NLAY extra idf files for scr module
 IF(IMODPCK.EQ.0.AND.IPCK.EQ.PSCR.AND.MMOD(PSCR).EQ.1)THEN
  DO IT=1,NLAY
   READ(IURUN,'(A256)',IOSTAT=IOS) LINE
   IF(IOS.NE.0)THEN
    CALL IMOD_UTL_PRINTTEXT('Error reading '//TRIM(LINE),0,IUOUT)
    CALL IMOD_UTL_PRINTTEXT('Busy processing module: '//TRIM(CMOD(IPCK)),2,IUOUT)
   ENDIF
   CALL RF2MF_READ_IDF(LINE,FCT,IMP,ILAY,CONSTANTE,FNAME,IOS,IMODPCK,IPCK)
   if(scr%istpc.ne.0)then
    call RF2MF_READ1MAIN_system(scr%PCSOFF(ilay),ios,ilay,fct,imp,simcsize,iarr,fname,iusclarith,idsclintp)
   else
    call RF2MF_READ1MAIN_system(scr%PCS(ilay),ios,ilay,fct,imp,simcsize,iarr,fname,iusclarith,idsclintp)
   endif
  ENDDO
 ENDIF

 END SUBROUTINE RF2MF_READ1MAIN

 !###====================================================================
 subroutine RF2MF_READ1MAIN_system(array,ios,ilay,fct,imp,constante,itype,fname,iuscl,idscl)
 !###====================================================================
 use rf2mf_module, only : tArrayRead
 implicit none
 character(len=*),intent(in) :: fname
 integer,intent(in) :: ios,itype,ilay,iuscl,idscl
 real,intent(in) :: fct,imp,constante
 type(tArrayRead),intent(inout) :: array

 array%fct = fct
 array%imp = imp
 if(ios.eq.0)then
  array%keyword='constant'
  array%cnstnt = constante
 else
  array%keyword='open/close'
  array%fname = trim(fname)
 endif
 array%ilay  = ilay
 array%type  = itype
 array%iuscl = iuscl
 array%idscl = idscl

 end subroutine RF2MF_READ1MAIN_system

 !###====================================================================
 SUBROUTINE RF2MF_READ_IDF(LINE,FCT,IMP,ILAY,CONSTANTE,FNAME,IOS,IMODPCK,IPCK)
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
 ELSE IF (IMODPCK.EQ.1 .AND.IPCK.EQ.PISG) THEN
  FCT = 1.0
  IMP = 1.0
  READ(LINE,*,IOSTAT=IOS) ILAY
 ELSE
  READ(LINE,*,IOSTAT=IOS) ILAY,FCT,IMP
 ENDIF
 IF(IOS.NE.0)THEN
     CALL IMOD_UTL_PRINTTEXT('Expected to read more on line: ['//TRIM(LINE)//']',2,IUOUT)
 END IF
 IF(ILAY.GT.MXNLAY)CALL IMOD_UTL_PRINTTEXT('Error reading ILAY='//TRIM(IMOD_UTL_ITOS(ILAY))// &
        ' that is larger than MXNLAY ('//TRIM(IMOD_UTL_ITOS(MXNLAY))//')',2,IUOUT)

 FNAME=IMOD_UTL_GETFNAME(LINE)

 LEX=.TRUE.
 IF(IMODPCK.EQ.0)THEN
  IF(MMOD(IPCK).EQ.0)LEX=.FALSE.
 ELSE
  IF(MPCK(IPCK).EQ.0)LEX=.FALSE.
 ENDIF
 IF(.NOT.LEX)RETURN

 LEX=.FALSE.
 IF(IMODPCK.EQ.0)THEN
  IF(ILAY.GE.1.AND.ILAY.LE.NLAY)LEX=.TRUE.
  IF(IPCK.EQ.PHFB.AND.ILAY.EQ.0)LEX=.TRUE.
  IF(ILAY.EQ.NLAY.AND.(IPCK.EQ.PVCW.OR.IPCK.EQ.PKVV))LEX=.FALSE.
 ELSEIF(IMODPCK.EQ.1)THEN
  IF(ILAY.GE.-1.AND.ILAY.LE.NLAY)LEX=.TRUE.
 ENDIF

 CONSTANTE=IMOD_UTL_GETREAL(FNAME,IOS)
 IF(IOS.EQ.0)THEN
  IF(LEX)THEN
   CALL IMOD_UTL_PRINTTEXT('Read Constant Value '//TRIM(IMOD_UTL_RTOS(CONSTANTE,'G',4)),0,IUOUT)
   CALL IMOD_UTL_PRINTTEXT(CHAR(9)//'* Modellayer: '//TRIM(IMOD_UTL_ITOS(ILAY))// &
                           '; Mult. Factor: '//TRIM(IMOD_UTL_RTOS(FCT,'G',4))// &
                           '; Addition: '//TRIM(IMOD_UTL_RTOS(IMP,'G',4)),0,IUOUT)
   CONSTANTE=CONSTANTE*FCT+IMP
   CALL IMOD_UTL_PRINTTEXT(CHAR(9)//'Constant Value becomes '//TRIM(IMOD_UTL_RTOS(CONSTANTE,'G',4)),0,IUOUT)
  ENDIF
 ELSE
  CALL IMOD_UTL_FILENAME(FNAME)
  IF(LEX)THEN
   CALL IMOD_UTL_PRINTTEXT('Assigned '//TRIM(FNAME),0,IUOUT)
   CALL IMOD_UTL_PRINTTEXT(CHAR(9)//'* Modellayer: '//TRIM(IMOD_UTL_ITOS(ILAY))// &
                          '; Mult. Factor: '//TRIM(IMOD_UTL_RTOS(FCT,'G',4))// &
                           '; Addition: '//TRIM(IMOD_UTL_RTOS(IMP,'G',4)),0,IUOUT)
  ENDIF
 ENDIF

 !## check combination khv/kvv and top/bot
 IF(IMODPCK.EQ.0)THEN
  IF(MMOD(PKHV).EQ.1)THEN
   IF(MMOD(PTOP).NE.1.OR.MMOD(PBOT).NE.1)CALL IMOD_UTL_PRINTTEXT('Horizontal K value needs usage TOP and BOT!',2,IUOUT)
  ENDIF
  IF(MMOD(PKVV).EQ.1)THEN
   IF(MMOD(PTOP).NE.1.OR.MMOD(PBOT).NE.1)CALL IMOD_UTL_PRINTTEXT('Vertical K value needs usage TOP and BOT!',2,IUOUT)
  ENDIF
 ENDIF

 IF(ILAY.LE.0)THEN
  IF(IMODPCK.EQ.0)THEN
   IF(IPCK.NE.PHFB)CALL IMOD_UTL_PRINTTEXT(TRIM(TXTMOD(IPCK))//' ilay less or equal zero!',2,IUOUT)
  ELSE
   SELECT CASE (IPCK)
    CASE (PRCH)
     IF(ILAY.EQ.0)CALL IMOD_UTL_PRINTTEXT('Modellayer number equal to zero for '//TRIM(TXTPCK(IPCK))//' package!',2,IUOUT)
    CASE DEFAULT
     IF(ILAY.EQ.0)THEN
      IF(MMOD(PTOP).NE.1.OR.MMOD(PBOT).NE.1)THEN
       CALL IMOD_UTL_PRINTTEXT('Usage of modellayers equal to zero only sustained in combination with both TOP and BOT!',2,IUOUT)
      ENDIF
     ENDIF
   END SELECT
  ENDIF
 ENDIF

 IF(ILAY.LT.0)CALL IMOD_UTL_PRINTTEXT(CHAR(9)//'# Will be assigned to first active model layer',0,IUOUT)
 IF(ILAY.EQ.0)CALL IMOD_UTL_PRINTTEXT(CHAR(9)//'# Layer will be computed by TOP and BOT data',0,IUOUT)

 END SUBROUTINE RF2MF_READ_IDF

END MODULE MOD_RF2MF_READ