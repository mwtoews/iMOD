!*------------------------------------------------------------------------------
!* FILE:
!*    METASWAP_COMP.FOR             
!*  
!* COPYRIGHT: 2011
!*    Alterra
!* 
!*    This PROGRAM, or parts thereof, may not be reproduced,
!*    modified or transferred to third parties without the
!*    written permission of Alterra
!* 
!*------------------------------------------------------------------------------
!* PROGRAM:
!* 
!* SYNOPSIS:
!* 
!* DESCRIPTION:
!*    See manual
!* 
!* 
!-------------------------------------------------------------------------------
!  
      SUBROUTINE   MetaSWAP_initComponent
!
#if(defined(INCLUDE_METASWAP))
      USE          SIMVAR
#endif            
      IMPLICIT     NONE
!
!     Set flag for use of MetaSWAP as component
#if(defined(INCLUDE_METASWAP))   
      MetaSWAP_comp = .TRUE.        
#else
      write(*,*) 'Error: MetaSWAP_initComponent'; stop 1
#endif      
      END
!
!-------------------------------------------------------------------------------
      SUBROUTINE MetaSWAP_initSimulation(time)
!
      IMPLICIT     NONE

      REAL*8    time  ! input
      INTEGER*4 iact
      INTEGER*4 iter
      REAL*8    dt
      LOGICAL*4 cvg   ! output
      LOGICAL*4 ready ! output
      INTEGER*4 error
!
      call timing_tic('METASWAP','initSimulation')
      
!     Read data for setting up simulation, check that time is within meteo period
      iact = 0
      iter = 0
#if(defined(INCLUDE_METASWAP))   
      CALL SIMGRO(iact,iter,time,dt,cvg,ready,error)
      if (error .ne. 0) call kill_process(error)
#else
      write(*,*) 'Error: MetaSWAP_initSimulation'; stop 1
#endif
!
      call timing_toc('METASWAP','initSimulation')

      END
!
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
!
!     Data exchange:
!     - GET initial heads from MODFLOW
!
!
!------------------------------------------------------------------------------
      SUBROUTINE MetaSWAP_prepareTimestep(dt)
!
      IMPLICIT     NONE

      INTEGER*4 iact
      INTEGER*4 iter
      REAL*8    time
      REAL*8    dt 
      LOGICAL*4 ready
!
!     No actions
!       
      END
!
!------------------------------------------------------------------------------
      LOGICAL FUNCTION MetaSWAP_getEndofCurrentTimestep(time_arg)
!
      IMPLICIT     NONE

      INTEGER*4 iact
      INTEGER*4 iter
      REAL*8    time_arg, time
      REAL*8    dt 
      LOGICAL*4 cvg   ! output
      LOGICAL*4 ready
      INTEGER*4 error
!
!     Determine the end-time of meteo data
      iact = 10
      iter = 0
#if(defined(INCLUDE_METASWAP))   
      CALL SIMGRO(iact,iter,time,dt,cvg,ready,error)        
      if (error .ne. 0) call kill_process(error)
#else
      write(*,*) 'Error: MetaSWAP_getEndofCurrentTimestep'; stop 1
#endif
      time_arg = time
      MetaSWAP_getEndofCurrentTimestep = .TRUE.
!       
      END
!
!------------------------------------------------------------------------------
      SUBROUTINE MetaSWAP_initTimestep(dt)
!
      IMPLICIT     NONE

      INTEGER*4 iact
      INTEGER*4 iter
      REAL*8    time
      REAL*8    dt     ! input
      LOGICAL*4 cvg   ! output
      LOGICAL*4 ready
      INTEGER*4 error
!
!     If this is the first time step:
!     - process initial heads from MODFLOW, do the MetaSWAP inits
!     - save the initial SIMGRO state
!
!     Do the simulation for the fast processes: evapotranspiration, runoff

      call timing_tic('METASWAP','initTimestep')

      iact = 1
      iter = 0      
#ifdef INCLUDE_METASWAP
      CALL SIMGRO(iact,iter,time,dt,cvg,ready,error)        
      if (error .ne. 0) call kill_process(error)
#else
      write(*,*) 'Error: MetaSWAP_initTimestep'; stop 1
#endif      
!       
      call timing_toc('METASWAP','initTimestep')
      
      END
!
!------------------------------------------------------------------------------
      SUBROUTINE MetaSWAP_prepareIter(iter)
!
      IMPLICIT     NONE

      INTEGER*4 iact
      INTEGER*4 iter
      REAL*8    time
      REAL*8    dt
      LOGICAL*4 cvg   ! output
      LOGICAL*4 ready
!
!     No actions needed
!       
#ifdef EXCLUDE_METASWAP
      write(*,*) 'Error: MetaSWAP_prepareIter'; stop 1
#endif      
      iter=iter+1    ! AL 2011-07-05
      RETURN
      END
!
!------------------------------------------------------------------------------
      SUBROUTINE MetaSWAP_performIter(iter)
!
      IMPLICIT     NONE

      INTEGER*4 iact
      INTEGER*4 iter  ! input
      REAL*8    time
      REAL*8    dt
      LOGICAL*4 cvg   ! output
      LOGICAL*4 ready
      INTEGER*4 error
!
!     Update the unsaturated zone
!     Compute recharges and storage coefficients for MODFLOW
      call timing_tic('METASWAP','performIter')

      iact = 2
#if(defined(INCLUDE_METASWAP))
      CALL SIMGRO(iact,iter,time,dt,cvg,ready,error)   
      if (error .ne. 0) call kill_process(error)
#else
      write(*,*) 'Error: MetaSWAP_performIter'; stop 1
#endif
!       
      call timing_toc('METASWAP','performIter')

      RETURN
      END
!
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
!
!     Data exchange:
!     - PUT recharges to MODFLOW
!     - PUT storage coefficients to MODFLOW
!
!------------------------------------------------------------------------------

!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
!
!     Data exchange:
!     - GET heads from MODFLOW
!
!------------------------------------------------------------------------------
      SUBROUTINE MetaSWAP_finishIter(iter,ready)
!
      IMPLICIT     NONE

      INTEGER*4 iact
      INTEGER*4 iter  
      REAL*8    time
      REAL*8    dt
      LOGICAL*4 cvg   ! output
      LOGICAL*4 ready
      INTEGER*4 error
!
!     Process MODFLOW heads for update of:
!     - MODFLOW net flux as seen by MetaSWAP
!     - MetaSWAP groundwater level
      call timing_tic('METASWAP','finishIter')

      iact = 3
#if(defined(INCLUDE_METASWAP))
      CALL SIMGRO(iact,iter,time,dt,cvg,ready,error)        
      if (error .ne. 0) call kill_process(error)
#else
      write(*,*) 'Error: MetaSWAP_finishIter'; stop 1
#endif
!       
      call timing_toc('METASWAP','finishIter')
      RETURN
      END
!
!------------------------------------------------------------------------------
      SUBROUTINE MetaSWAP_finishTimestep(time)
!
      IMPLICIT     NONE

      INTEGER*4 iact
      INTEGER*4 iter
      REAL*8    time  ! input for synchro check
      REAL*8    dt
      LOGICAL*4 cvg   ! output
      LOGICAL*4 ready
      INTEGER*4 error
!
!     Finalize unsaturated zone
!     Budget calculations
!     Write data to file
      call timing_tic('METASWAP','finishTimestep')
      iact = 4
#if(defined(INCLUDE_METASWAP))
      CALL SIMGRO(iact,iter,time,dt,cvg,ready,error)
      if (error .ne. 0) call kill_process(error)
#else
      write(*,*) 'Error: MetaSWAP_finishTimestep'; stop 1
#endif
!       
      call timing_toc('METASWAP','finishTimestep')
      RETURN
      END
!
!------------------------------------------------------------------------------
      SUBROUTINE MetaSWAP_finishSimulation()
!
      IMPLICIT     NONE

      INTEGER*4 iact
      INTEGER*4 iter
      REAL*8    time 
      REAL*8    dt
      LOGICAL*4 cvg   ! output
      LOGICAL*4 ready
      INTEGER*4 error
!
!     Finalize simulation
      call timing_tic('METASWAP','finishSimulation')
      iact = 5
#if(defined(INCLUDE_METASWAP))
      CALL SIMGRO(iact,iter,time,dt,cvg,ready,error)
      if (error .ne. 0) call kill_process(error)
#else
      write(*,*) 'Error: MetaSWAP_finishSimulation'; stop 1
#endif
!       
      call timing_toc('METASWAP','finishSimulation')
      RETURN
      END
!
!------------------------------------------------------------------------------
      SUBROUTINE MetaSWAP_saveState()
!
      IMPLICIT     NONE
!
      INTEGER*4    error
       
#if(defined(INCLUDE_METASWAP))
      CALL SIMGRO_save(error)
      if (error .ne. 0) call kill_process(error)
#else
      write(*,*) 'Error: MetaSWAP_saveState'; stop 1
#endif
!
      RETURN
      END
!
!------------------------------------------------------------------------------
      SUBROUTINE MetaSWAP_restoreState()
!
      IMPLICIT     NONE

      INTEGER*4 iact
      INTEGER*4 iter
      REAL*8    time 
      REAL*8    dt
      LOGICAL*4 cvg   ! output
      LOGICAL*4 ready
      INTEGER*4 error
!
      iact = -1

#if(defined(INCLUDE_METASWAP))
      CALL SIMGRO(iact,iter,time,dt,cvg,ready,error)
      if (error .ne. 0) call kill_process(error)
#else
      write(*,*) 'Error: MetaSWAP_restoreState'; stop 1
#endif
!
      RETURN
      END
!------------------------------------------------------------------------------
      SUBROUTINE MetaSWAP_saveRestore(stsave,strestore)
!
      IMPLICIT     NONE

      logical, intent(in)  :: stsave
      logical, intent(in)  :: strestore

!      INTEGER*4 iact
!      INTEGER*4 iter
!      REAL*8    time 
!      REAL*8    dt
!      LOGICAL*4 status
!
      call timing_tic('METASWAP','saveRestore')
#if(defined(INCLUDE_METASWAP))
       if (strestore) then
          write(*,*) ' RESTORE STATE: MetaSwap'
          call MetaSWAP_restoreState()
       else if (stsave) then
          write(*,*) ' SAVE    STATE: MetaSwap'
           call MetaSWAP_saveState()
       endif
#else
      write(*,*) 'Error: MetaSWAP_saveRestore'; stop 1
#endif
!
      call timing_toc('METASWAP','saveRestore')
      RETURN
      END

      !> Return the number of exchange items (IDs).
      logical function metaswap_PutModSimNumberOfIDs(nxch)
!...     modules
#if(defined(INCLUDE_METASWAP))
      use simvar, only: mfi ! integer*4
#endif
      implicit none

!...     arguments
      integer, intent(out) :: nxch

!...     locals
      logical :: retval
!.......................................................................
#if(defined(INCLUDE_METASWAP))
      nxch = size(mfi)
#else
      write(*,*) 'Error: metaswap_PutModSimNumberOfIDs'; stop 1
      nxch=0
#endif
      !if (nxch.le.0) then
      !  retval = .false.      
      !else
         retval = .true.
      !end if
      metaswap_PutModSimNumberOfIDs = retval
      
      end function

      ! Put exchange IDs.
      logical function metaswap_PutModSimIDs(ids)
!...     modules      
#if(defined(INCLUDE_METASWAP))
      use simvar, only: mfi     ! integer*4
#endif
      implicit none

!...     arguments
      logical :: retval
      integer, dimension(*), intent(out) :: ids
      
!...     locals      
      integer :: i
!.......................................................................
 
#if(defined(INCLUDE_METASWAP))
      do i =1, size(mfi)
         ids(i) = mfi(i)
      end do
#else      
      write(*,*) 'Error: metaswap_PutModSimIDs'; stop 1
      ids(1)=0
#endif
      retval = .true.
      metaswap_PutModSimIDs = retval
      
      end function

      !> Get MODFLOW heads.
      logical function metaswap_GetHeads(gwheads,nid,
     1                                       xchIdx,xchOff,mv)
!...     modules  
#if(defined(INCLUDE_METASWAP))
      use simvar, only: hgwmod, mfi
      use pks_imod_utl, only: pks_imod_utl_iarmwp_xch ! PKS
#endif
      implicit none

!...     arguments    
      integer, intent(in)                 :: nid
      real, dimension(*), intent(inout)   :: gwheads
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(*), intent(in)   :: xchOff
      real, intent(in)                    :: mv
      
!...     locals
      logical :: ok
      integer :: i, j, js, je, k, n
!.......................................................................

#if(defined(INCLUDE_METASWAP))
      n = size(mfi)
      if (n.ne.nid) then
         ok = .false.      
      else
         ok = .true.
      end if
      call pks_imod_utl_iarmwp_xch(gwheads,'h')                         ! PKS
      js = 1
      do i = 1, nid
         ! get index in receive array
         je = xchOff(i)
         if (je-js.gt.0) then
            write(*,*) 'Warning: svat received >1 heads'
         end if   
         do j = js, je
            k = xchIdx(j)
            if (k.le.0) then
               ok = .false.
               metaswap_GetHeads = ok
               return         
            end if
            hgwmod(i) = gwheads(k) 
         end do
         js = je + 1
      end do
#else
      write(*,*) 'Error: metaswap_GetHeads'; stop 1
#endif

      metaswap_GetHeads = ok
      end function
  
      !> Put unsaturated zone flux.    
      logical function metaswap_PutModSimUnsaturatedZoneFlux(uszflux,mv)
!...     modules    
#if(defined(INCLUDE_METASWAP))
      use pksmpi_mod,only:myrank
      use simvar, only: vsim, mfi, ki
#endif
      implicit none

!...     arguments
      real, dimension(*), intent(out) :: uszflux
      real, intent(in)                :: mv ! not used yet 

!...     locals 
      logical ok     
      integer :: i, n
      real :: rmask ! PKS
!.......................................................................

#if(defined(INCLUDE_METASWAP))
      n = size(mfi)
      
!...     checks: to do      
      ok = .true.

!...      copy unsaturated zone flux            
      do i = 1, size(mfi)
         uszflux(i) = vsim(i)
         call pks7mpimsiarmwpmask(mfi(i),ki(i),rmask) ! PKS 
         uszflux(i) = rmask*uszflux(i) ! PKS
      end do
#else
      write(*,*) 'Error: metaswap_PutModSimUnsaturatedZoneFlux'; stop 1
      uszflux(1)=1
#endif
      metaswap_PutModSimUnsaturatedZoneFlux = ok
      end function
      
      !> Put unsaturated zone flux.    
      logical function metaswap_PutSimMozUnsaturatedZoneFlux(uszflux,mv)
!...     modules
#if(defined(INCLUDE_METASWAP))
      use simvar, only: vsim, nuomilsw, klswi
#endif
      implicit none

!...     arguments
      real, dimension(*), intent(out) :: uszflux
      real, intent(in)                :: mv ! not used yet 

!...     locals 
      logical ok     
      integer :: i, n
!.......................................................................
      
!...     checks: to do      
      ok = .true.

!...      copy unsaturated zone flux     
#if(defined(INCLUDE_METASWAP))
      do i = 1, nuomilsw
         uszflux(i) = vsim(klswi(i))
      end do        
#else
      write(*,*) 'Error: metaswap_PutSimMozUnsaturatedZoneFlux'; stop 1
      uszflux(1)=0
#endif
      metaswap_PutSimMozUnsaturatedZoneFlux = ok
      end function      
      
      logical function metaswap_PutStorageFactor(strfct,mv)
!...     modules      
#if(defined(INCLUDE_METASWAP))
      use simvar, only: sc1sim, mfi
#endif
      implicit none

!...     arguments
      real, dimension(*), intent(out) :: strfct 
      real, intent(in)                :: mv ! not used yet 

!...     locals 
      logical :: ok     
      integer :: i, n
!.......................................................................

!...     checks: to do     
      ok = .true.

#if(defined(INCLUDE_METASWAP))
      n = size(mfi)
      do i = 1, size(mfi)
         strfct(i) = sc1sim(i)
      end do        
#else
      write(*,*) 'Error: metaswap_PutStorageFactor'; stop 1
      strfct(1)=0
#endif
      metaswap_PutStorageFactor = ok
      end function      
      
      logical function metaswap_PutModMozNumberOfIDs(nid)
!...     modules      
#if(defined(INCLUDE_METASWAP))
      use simvar, only: nuomilsw
#endif
      implicit none

!...     arguments
      integer, intent(out) :: nid

!...     locals 
      logical :: ok           
!.......................................................................
      
#if(defined(INCLUDE_METASWAP))
      if (nuomilsw.le.0) then
         ok = .false.
         metaswap_PutModMozNumberOfIDs = ok
         return
      end if     
      nid = nuomilsw
#else
      write(*,*) 'Error: metaswap_PutModMozNumberOfIDs'; stop 1
      nid=0
#endif

      ok = .true.
      metaswap_PutModMozNumberOfIDs = ok
      end function
      
      logical function metaswap_PutModMozIDs(ids)
!...     modules      
#if(defined(INCLUDE_METASWAP))
      use simvar, only: nuomilsw, lswi
#endif
      implicit none

!...     arguments
      integer, dimension(*), intent(out) :: ids

!...     locals 
      logical :: ok
      integer :: i           
!.......................................................................

#if(defined(INCLUDE_METASWAP))
      do i = 1, nuomilsw
         ids(i) = lswi(i)      
      end do
#else
      write(*,*) 'Error: metaswap_PutModMozIDs'; stop 1
      ids(1)=0
#endif
      ok = .true.
      metaswap_PutModMozIDs = ok
      end function      
      
      logical function MetaSWAP_PutCumSWSprinklingDemandFluxes(sprflux,
     1                                                         mvin)
!...     modules      
#if(defined(INCLUDE_METASWAP))
      use simvar, only: nuomilsw, cupsswdemm3i 
#endif
      implicit none

!...     arguments
      real, dimension(*), intent(out) :: sprflux
      real, intent(in) :: mvin

!...     parameters
      real, parameter :: mv = -9999. ! In the future this should be defined by MetaSWAP !

!...     locals 
      logical :: ok 
      integer :: i     
      real :: flux     
!.......................................................................
 
      ok = .true.

#if(defined(INCLUDE_METASWAP))
      if (nuomilsw.le.0) ok = .false.
      do i = 1, nuomilsw
         flux = cupsswdemm3i(i)
         if (flux.eq.mv) flux = mvin
         sprflux(i) = flux
      end do
#else
      write(*,*) 'Error: MetaSWAP_PutCumSWSprinklingDemandFluxes'
      stop 1
      sprflux(1)=0.
#endif      
      
      MetaSWAP_PutCumSWSprinklingDemandFluxes = ok
      end function      
      
      logical function MetaSWAP_PutCumRunonFluxes(runonflux,mvin)
!...     modules      
#if(defined(INCLUDE_METASWAP))
      use simvar, only: nuomilsw, cuqrunm3i 
#endif
      implicit none

!...     arguments
      real, dimension(*), intent(out) :: runonflux
      real, intent(in) :: mvin

!...     parameters
      real, parameter :: mv = -9999.! In the future this should be defined by MetaSWAP !

!...     locals 
      logical :: ok 
      integer :: i     
      real :: flux     
!.......................................................................
      ok = .true.

#if(defined(INCLUDE_METASWAP))      
      if (nuomilsw.le.0) ok = .false.
      do i = 1, nuomilsw
         flux = cuqrunm3i(i)
         if (flux.eq.mv) flux = mvin
         runonflux(i) = flux
      end do
#else
      write(*,*) 'Error: MetaSWAP_PutCumRunonFluxes'; stop 1
      runonflux(1)=0. 
#endif
      
      MetaSWAP_PutCumRunonFluxes = ok
      end function      
      
      !> Get MOZART fractions.
      logical function metaswap_GetFractions(fractions,nid,
     1                                       xchIdx,xchOff,mvin)
!...     modules    
#if(defined(INCLUDE_METASWAP))      
      use simvar, only: frpsswi, lswi
#endif
      implicit none

!...     arguments    
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: fractions
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin
      
!...     parameters
      real, parameter :: mv = -9999. ! MetaSWAP missing value: in future this should be set from module 
      
!...     locals
      logical :: ok
      integer :: i, j, js, je, k, n
      real :: frc
!.......................................................................

#if(defined(INCLUDE_METASWAP))      
      n = size(lswi)
      if (n.ne.nid) then
         ok = .false.      
      else
         ok = .true.
      end if
      js = 1
      do i = 1, nid
         ! get index in receive array
         je = xchOff(i)
         if (je-js.gt.0)
     1      write(*,*) 'Warning: svat received >1 fractions'
         do j = js, je
            k = xchIdx(j)
            if (k.le.0) then
               ok = .false.
               metaswap_GetFractions = ok
               return         
            end if
            frc = fractions(k)
            if (frc.eq.mvin) frc = mv
            frpsswi(i) = frc
         end do
         js = je + 1
      end do
#else
      write(*,*) 'Error: metaswap_GetFractions'; stop 1
#endif

      metaswap_GetFractions = ok
      end function      
            