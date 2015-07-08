!*----------------------------------------------------------------------
!* FILE:
!*    TRANSOL_COMP.FOR             
!*  
!* COPYRIGHT: 2011
!*    Alterra
!* 
!*    This PROGRAM, or parts thereof, may not be reproduced,
!*    modified or transferred to third parties without the
!*    written permission of Alterra
!* 
!*---------------------------------------------------------------------------
!* PROGRAM:
!* 
!* SYNOPSIS:
!* 
!* DESCRIPTION:
!*    See manual
!* 
!* 
!* CODE:
!*    Standard fortran-77 programme, with USE and f90 dynamic memory allocation
!* 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  
      SUBROUTINE TRANSOL_initComponent()
!
#ifdef INCLUDE_METASWAP 
      USE        SIMVAR
#endif      
      IMPLICIT   NONE
!
!     Set flag for TRANSOL as component
#ifdef INCLUDE_METASWAP 
      TRANSOL_comp = .TRUE. 
#else   
      write(*,*) 'Error: TRANSOL_initComponent'; stop 1
#endif      
      RETURN
      END
!
!------------------------------------------------------------------------------
      SUBROUTINE TRANSOL_initSimulation(time)
!
      IMPLICIT   NONE

      INTEGER*4  iact
      INTEGER*4  iter
      INTEGER*4  iter_transol
      REAL*8     time          ! input
      REAL*8     dt
      LOGICAL    conv
!
!     This action is performed "on the fly" in the execution of the first time step of MetaSWAP
!       
#ifdef EXCLUDE_TRANSOL 
      write(*,*) 'Error: TRANSOL_initSimulation'; stop 1
#endif      
      RETURN
      END
!
!------------------------------------------------------------------------------
      SUBROUTINE TRANSOL_prepareTimestep(dt)
!
      IMPLICIT   NONE

      INTEGER*4 iact
      INTEGER*4 iter
      INTEGER*4 iter_transol
      REAL*8    time
      REAL*8    dt           ! input
      LOGICAL*4 conv
!
!     No actions needed
!       
#ifdef EXCLUDE_TRANSOL 
      write(*,*) 'Error: TRANSOL_prepareTimestep'; stop 1
#endif      
      RETURN
      END
!
!
!------------------------------------------------------------------------------
      SUBROUTINE TRANSOL_initTimestep(dt)
!
      IMPLICIT   NONE

      INTEGER*4 iact
      INTEGER*4 iter
      INTEGER*4 iter_transol
      REAL*8    time
      REAL*8    dt           ! input
      LOGICAL*4 conv
!
!     No actions needed
! 
#ifdef EXCLUDE_TRANSOL 
      write(*,*) 'Error: TRANSOL_initTimestep'; stop 1
#endif      
      RETURN
      END
!
!------------------------------------------------------------------------------
      SUBROUTINE TRANSOL_prepareIter(iter_transol)
!
      IMPLICIT   NONE

      INTEGER*4 iact
      INTEGER*4 iter          
      INTEGER*4 iter_transol  ! input
      REAL*8    time
      REAL*8    dt
      LOGICAL*4 conv
!
!     Restore the SIMGRO state if this is the second iteration of TRANSOL
#ifdef INCLUDE_METASWAP 
      IF (iter_transol .GE. 2) THEN
        iact = -1
        CALL SIMGRO(iact,iter,time,dt,conv)        
      ENDIF
#else
      write(*,*) 'Error: TRANSOL_prepareIter'; stop 1
#endif
!       
      RETURN
      END
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
!
!     Data exchange WATER QUANTITY for Mozart/TRANSOL time step:
!     - GET  qdr_m(1) = curidr(icol,irow)/areamfcell
!     - GET  qif_m(1) = curiif(icol,irow)/areamfcell
!     - GET  qbot     = cuseep(icol,irow)/areamfcell
!
!     These data should now be supplied by MODFLOW for the entries of the mapping
!     table mod2svat.inp, just like other info from MODFLOW (heads)
!  
!     Data exchange WATER QUALITY for Mozart/TRANSOL time step:
!     - GET data as now done in  subroutine mozart_forward_transol(use_mozart)
!
!------------------------------------------------------------------------------
      SUBROUTINE TRANSOL_performIter()
!
      IMPLICIT   NONE

      INTEGER*4 iact
      INTEGER*4 iter
      REAL*8    time
      REAL*8    dt
      LOGICAL*4 conv
!
!     Downscaled moisture profiles
!     TRANSOL concentrations
      iact = 41
#ifdef INCLUDE_METASWAP 
      CALL SIMGRO(iact,iter,time,dt,conv)
#else
      write(*,*) 'Error: TRANSOL_performIter'; stop 1
#endif
!       
      RETURN
      END
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
!
!     Data exchange WATER QUALITY for Mozart/TRANSOL iteration:
!     - PUT salt concentrations and balance terms to MOZART     must now go online
!
!------------------------------------------------------------------------------
      SUBROUTINE TRANSOL_finishIter(iter_transol,conv)
!
      IMPLICIT   NONE

      INTEGER*4 iact
      INTEGER*4 iter   
      INTEGER*4 iter_transol  ! input
      REAL*8    time
      REAL*8    dt
      LOGICAL*4 conv   
!
!     TRANSOL does not monitor convergence (at least, not yet)
      conv = .TRUE.
! 
#ifdef EXCLUDE_TRANSOL 
      write(*,*) 'Error: TRANSOL_finishIter'; stop 1
#endif      
      RETURN
      END
!
!------------------------------------------------------------------------------
      SUBROUTINE TRANSOL_finishTimestep(time)
!
      IMPLICIT   NONE

      INTEGER*4 iact
      INTEGER*4 iter
      REAL*8    time  ! input, used for synchro check
      REAL*8    dt
      LOGICAL*4 conv
!
!     Write data to file
      iact = 42
#ifdef INCLUDE_METASWAP 
      CALL SIMGRO(iact,iter,time,dt,conv)        
#else      
      write(*,*) 'Error: TRANSOL_finishTimestep'; stop 1
#endif    
!
      RETURN
      END
!
!------------------------------------------------------------------------------
      SUBROUTINE TRANSOL_finishSimulation()
!
      IMPLICIT   NONE

      INTEGER*4 iact
      INTEGER*4 iter
      REAL*8    time 
      REAL*8    dt
      LOGICAL*4 conv
!
!     Finalize is triggered by MetaSWAP
!  
#ifdef EXCLUDE_TRANSOL 
      write(*,*) 'Error: TRANSOL_finishSimulation'; stop 1
#endif      
      RETURN
      END

!
!------------------------------------------------------------------------------
      SUBROUTINE TRANSOL_saveState()
!
!     Done bij MetaSWAP (temprorary arrangement)
!
#ifdef EXCLUDE_TRANSOL 
      write(*,*) 'Error: TRANSOL_saveState'; stop 1
#endif      
      RETURN
      END
!
      !> Get MODFLOW fluxes/stages.
      logical function TRANSOL_GetSeepageRiverDrain(val,nid,
     1                                          xchIdx,xchOff,mvin,act)
!...     modules  
#ifdef INCLUDE_METASWAP 
      use simvar, only: qseepmodi, qrivmodi, qdrnmodi, mfi
#endif
      implicit none

!...     arguments    
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: val ! m
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin
      character(len=5), intent(in)        :: act              
            
!...     locals
      character(len=10) :: action
      logical :: ok
      integer :: i, j, js, je, k
      real :: fval
!.......................................................................
#ifdef INCLUDE_METASWAP 
      action = act
      call cfn_token(action ,'tu')
      ok = .true.
      
      js = 1
      do i = 1, nid
         je = xchOff(i)
         do j = js, je
            k = xchIdx(j)            
            if (k.le.0) then
               write(*,*) 'Error: coupling node index out of range'
               ok = .false.
               TRANSOL_GetSeepageRiverDrain = ok
               return
            end if
            fval = val(k)
            select case(action)
            case('QSEEP')    
               if (fval.eq.mvin) fval = 0.0
               qseepmodi(i) = fval
            case('QRIVP')
               if (fval.eq.mvin) fval = 0.0
               qrivmodi(i) = fval ! MODIFY: PUT IN CORRESPONDING VARIABLE
            case('QRIVS')
               if (fval.eq.mvin) fval = 0.0
               qrivmodi(i) = fval ! MODIFY: PUT IN CORRESPONDING VARIABLE
            case('QRIVT')
               if (fval.eq.mvin) fval = 0.0
               qrivmodi(i) = fval ! MODIFY: PUT IN CORRESPONDING VARIABLE
            case('QDRNB')                
               if (fval.eq.mvin) fval = 0.0
               qdrnmodi(i) = fval ! ! MODIFY: PUT IN CORRESPONDING VARIABLE
            case('QDRNO')                
               if (fval.eq.mvin) fval = 0.0
               qdrnmodi(i) = fval ! ! MODIFY: PUT IN CORRESPONDING VARIABLE
            case('SRIVP')
               if (fval.eq.mvin) fval = -9999.
               !srivmodi(i) = fval ! MODIFY: PUT IN CORRESPONDING VARIABLE
            case('SRIVS')
               if (fval.eq.mvin) fval = -9999.
               !srivmodi(i) = fval ! MODIFY: PUT IN CORRESPONDING VARIABLE
            case('SRIVT')
               if (fval.eq.mvin) fval = -9999.
               !srivmodi(i) = fval ! MODIFY: PUT IN CORRESPONDING VARIABLE
            case default
               ok = .false.
            end select
         end do 
         js = je + 1     
      end do
#else
      write(*,*) 'Error: TRANSOL_GetSeepageRiverDrain'; stop 1
#endif
      TRANSOL_GetSeepageRiverDrain = ok
      end function

      !> Get MODFLOW fluxes.
      logical function TRANSOL_GetSalt(flux,nid,
     1                                 xchIdx,xchOff,mvin)
!...     modules 
#ifdef INCLUDE_METASWAP 
      use solute2lsw, only: coliin_i
#endif
      implicit none

!...     arguments    
      integer, intent(in)                 :: nid
      real, dimension(*), intent(in)      :: flux ! kg/m3
      integer, dimension(*), intent(in)   :: xchIdx
      integer, dimension(nid), intent(in) :: xchOff
      real, intent(in)                    :: mvin
      
!...     parameters
      real, parameter :: mv = -9999. ! In future to be replace by missing value from TRANSOL !
      
!...     locals
      logical :: ok
      integer :: i, j, js, je, k
      real :: fval
!.......................................................................

      ok = .true.
      
#ifdef INCLUDE_METASWAP 
      js = 1
      do i = 1, nid
         je = xchOff(i)
         do j = js, je
            k = xchIdx(j)            
            if (k.le.0) then
               write(*,*) 'Error: coupling node index out of range'
               ok = .false.
               TRANSOL_GetSalt = ok
               return
            end if
            fval = flux(k)
            if (fval.eq.mvin) fval = mv
            coliin_i(i) = fval
         end do 
         js = je + 1     
      end do
#else
      write(*,*) 'Error: TRANSOL_GetSalt'; stop 1
#endif
      TRANSOL_GetSalt = ok
      end function

      logical function TRANSOL_PutSalt(salt,mvin)
!...     modules   
#ifdef INCLUDE_METASWAP 
      use solute2lsw, only: amdrrukg_i, nuomilsw_pmsw
#endif
      implicit none

!...     arguments
      real, dimension(*), intent(out) :: salt
      real, intent(in) :: mvin

!...     parameters
      real, parameter :: mv = -9999. ! In the future this should be defined by MetaSWAP !

!...     locals 
      logical :: ok 
      integer :: i     
      real :: svalkg     
!.......................................................................
      ok = .true.
      
#ifdef INCLUDE_METASWAP 
      if (nuomilsw_pmsw.le.0) ok = .false.
  
      do i = 1, nuomilsw_pmsw
         svalkg = amdrrukg_i(i)
         if (svalkg.eq.mv) svalkg = mvin
         salt(i) = svalkg
      end do
#else
      write(*,*) 'Error: TRANSOL_GetSalt'; stop 1
      salt(1)=0.
#endif
      TRANSOL_PutSalt = ok
      end function      
            



 




