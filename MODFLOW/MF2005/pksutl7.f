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

!----------------------------------------------------------------------- 
      subroutine ivperm (n, ix, perm) 
      integer n, perm(n) 
      integer ix(n)
!-----------------------------------------------------------------------
! this subroutine performs an in-place permutation of a integer vector
! ix according to the permutation array perm(*), i.e., on return, 
! the vector ix satisfies,
!
!	ix(perm(j)) :== ix(j), j=1,2,.., n
!
!-----------------------------------------------------------------------
! on entry:
!---------
! n 	  = length of vector x.
! perm 	= integer array of length n containing the permutation  array.
! ix	  = input vector
!
! on return:
!---------- 
! ix	  = vector x permuted according to ix(perm(*)) :=  ix(*)
!
!----------------------------------------------------------------------c
!           Y. Saad, Sep. 21 1989                                      c
!----------------------------------------------------------------------c
! local variables 
      integer itmp, itmp1
!
      init      = 1
      itmp      = ix(init)
      ii        = perm(init)
      perm(init)= -perm(init)
      k         = 0
!     
! loop
! 
 6    k = k+1
!
! save the chased element --
! 
      itmp1     = ix(ii) 
      ix(ii)    = itmp
      next      = perm(ii) 
      if (next .lt. 0 ) goto 65
!     
! test for end 
!
      if (k .gt. n) goto 101
      itmp      = itmp1
      perm(ii)  = - perm(ii)
      ii        = next 
!
! end loop 
!
      goto 6
!
! reinitilaize cycle --
!
 65   init       = init+1
      if (init .gt. n) goto 101
      if (perm(init) .lt. 0) goto 65
      itmp       = ix(init)
      ii         = perm(init)
      perm(init) =-perm(init)
      goto 6
!     
 101  continue
      do 200 j=1, n
         perm(j) = -perm(j)
 200  continue 
!     
      return
!-------------------end-of-ivperm--------------------------------------- 
!-----------------------------------------------------------------------
      end
      
      
      SUBROUTINE EXPORT_SYSTEM(NIAC,NNZC,BC,XC,AC,IAC,JAC,IBOUND,
     1    APREF,XPREF,BPREF,FM,KPER,KSTP,KITER)
      
      IMPLICIT NONE
      
C        ARGUMENTS
      INTEGER, INTENT(IN)                          :: NIAC
      INTEGER, INTENT(IN)                          :: NNZC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(IN) :: BC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(IN) :: XC
      DOUBLEPRECISION, DIMENSION(NNZC), INTENT(IN) :: AC
      INTEGER, DIMENSION(NIAC+1), INTENT(IN)       :: IAC
      INTEGER, DIMENSION(NNZC), INTENT(IN)         :: JAC
      INTEGER, DIMENSION(NIAC), INTENT(IN)         :: IBOUND
      CHARACTER(LEN=*), INTENT(IN)                 :: APREF
      CHARACTER(LEN=*), INTENT(IN)                 :: XPREF
      CHARACTER(LEN=*), INTENT(IN)                 :: BPREF
      CHARACTER(LEN=*), INTENT(IN)                 :: FM
      INTEGER, INTENT(IN)                          :: KPER
      INTEGER, INTENT(IN)                          :: KSTP
      INTEGER, INTENT(IN)                          :: KITER
 
C        LOCALS
      INTEGER, PARAMETER               :: LUN = 99
      CHARACTER(LEN=100)               :: FNAME, S
      CHARACTER(LEN=100), DIMENSION(3) :: SARR
      DOUBLE PRECISION                 :: V
      INTEGER :: N, I0, I1, I, J, JCOL
C     ------------------------------------------------------------------

C        AC
      IF (LEN_TRIM(APREF) > 0) THEN
         WRITE(FNAME,'(2A,3(I3.3,A),A)') TRIM(APREF),
     1    '_',KPER,'_',KSTP,'_',KITER,'.',TRIM(FM)    
         OPEN(UNIT=LUN,FILE=TRIM(FNAME),FORM='FORMATTED',
     1      ACCESS='SEQUENTIAL',STATUS='UNKNOWN',ACTION='WRITE')       
         DO N = 1, NIAC
            I0 = IAC(N)
            I1 = IAC(N+1) - 1
            DO J = I0, I1
               JCOL = JAC(J) 
               WRITE(SARR(1),*) N
               WRITE(SARR(2),*) JCOL
               V = AC(J)
               IF (IBOUND(JCOL).LT.0.AND.J.EQ.I0) THEN
                  V = -V
               END IF
               WRITE(SARR(3),*) V
               WRITE(SARR(1),'(2A)') TRIM(ADJUSTL(SARR(1))),','
               WRITE(SARR(2),'(2A)') TRIM(ADJUSTL(SARR(2))),','
               WRITE(S,'(3A)')(TRIM(ADJUSTL(SARR(I))),I=1,3)    
               WRITE(LUN,'(A)') TRIM(S)            
            END DO
         END DO
         CLOSE(LUN)
      END IF
      
C        XC
      IF (LEN_TRIM(XPREF) > 0) THEN
         WRITE(FNAME,'(2A,3(I3.3,A),A)') TRIM(XPREF),
     1    '_',KPER,'_',KSTP,'_',KITER,'.',TRIM(FM) 
         OPEN(UNIT=LUN,FILE=TRIM(FNAME),FORM='FORMATTED',
     1      ACCESS='SEQUENTIAL',STATUS='UNKNOWN',ACTION='WRITE')  
         DO N = 1, NIAC
           WRITE(S,*) XC(N)
           WRITE(LUN,'(A)') TRIM(ADJUSTL(S))           
         END DO                 
         CLOSE(LUN)
      END IF
      
C        BC
      IF (LEN_TRIM(BPREF) > 0) THEN
         WRITE(FNAME,'(2A,3(I3.3,A),A)') TRIM(BPREF),
     1    '_',KPER,'_',KSTP,'_',KITER,'.',TRIM(FM)
         OPEN(UNIT=LUN,FILE=TRIM(FNAME),FORM='FORMATTED',
     1      ACCESS='SEQUENTIAL',STATUS='UNKNOWN',ACTION='WRITE')       
         DO N = 1, NIAC
           V = BC(N)
           IF (IBOUND(N).LT.0) THEN
              V = -V
           END IF
           WRITE(S,*) V
           WRITE(LUN,'(A)') TRIM(ADJUSTL(S))           
         END DO
         CLOSE(LUN)
      END IF
       
      END SUBROUTINE EXPORT_SYSTEM
      
      SUBROUTINE EXPORT_SYSTEM_OLD(NIAC,NNZC,NODES,IXMAP,BC,XC,AC,
     1    IAC,JAC,IBOUND,APREF,XPREF,BPREF,MPREF,FM,KPER,KSTP,KITER,
     2    IOPT)
      
      IMPLICIT NONE
      
C        ARGUMENTS
      INTEGER, INTENT(IN)                          :: NIAC
      INTEGER, INTENT(IN)                          :: NNZC
      INTEGER, INTENT(IN)                          :: NODES
      INTEGER, DIMENSION(NIAC), INTENT(IN)         :: IXMAP
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(IN) :: BC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(IN) :: XC
      DOUBLEPRECISION, DIMENSION(NNZC), INTENT(IN) :: AC
      INTEGER, DIMENSION(NIAC+1), INTENT(IN)       :: IAC
      INTEGER, DIMENSION(NNZC), INTENT(IN)         :: JAC
      INTEGER, DIMENSION(NODES), INTENT(IN)        :: IBOUND
      CHARACTER(LEN=*), INTENT(IN)                 :: APREF
      CHARACTER(LEN=*), INTENT(IN)                 :: XPREF
      CHARACTER(LEN=*), INTENT(IN)                 :: BPREF
      CHARACTER(LEN=*), INTENT(IN)                 :: MPREF
      CHARACTER(LEN=*), INTENT(IN)                 :: FM
      INTEGER, INTENT(IN)                          :: KPER
      INTEGER, INTENT(IN)                          :: KSTP
      INTEGER, INTENT(IN)                          :: KITER
      INTEGER, INTENT(IN)                          :: IOPT
 
C        LOCALS
      INTEGER, PARAMETER               :: LUN = 99
      CHARACTER(LEN=100)               :: FNAME, S
      CHARACTER(LEN=100), DIMENSION(3) :: SARR
      DOUBLE PRECISION                 :: V
      INTEGER :: N, I0, I1, I, J, JCOL, IB
      DOUBLE PRECISION, PARAMETER      :: VMAX = 1E30   
      DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: BCT
      LOGICAL :: LFH
      
C     ------------------------------------------------------------------
 
C        COPY BC
      ALLOCATE(BCT(NIAC))
      DO N = 1, NIAC
         BCT(N) = BC(N)
      END DO    
C
C        AC
      IF (LEN_TRIM(APREF) > 0) THEN
         WRITE(FNAME,'(2A,3(I3.3,A),A)') TRIM(APREF),
     1    '_',KPER,'_',KSTP,'_',KITER,'.',TRIM(FM)    
         OPEN(UNIT=LUN,FILE=TRIM(FNAME),FORM='FORMATTED',
     1      ACCESS='SEQUENTIAL',STATUS='UNKNOWN',ACTION='WRITE')       
         DO N = 1, NIAC
            I0 = IAC(N)
            I1 = IAC(N+1) - 1
            LFH = .FALSE.
            DO J = I0, I1
               JCOL = JAC(J) 
               WRITE(SARR(1),*) N
               WRITE(SARR(2),*) JCOL
               V = AC(J)
               !IF (IBOUND(JCOL).LT.0.AND.J.EQ.I0) THEN
               !   V = -V
               !END IF
               IF (IOPT.EQ.1) THEN
                  IB = IBOUND(JCOL)
               ELSE
                  IB = IBOUND(IXMAP(JCOL))
               END IF
               
               IF (IB.LT.0) THEN
                  IF(J.EQ.I0) THEN
                     V = 1.0
                     BCT(N) = XC(N)
                     LFH = .TRUE.
                  ELSE
                     IF (.NOT.LFH) THEN 
                        BCT(N) = BCT(N) - V*XC(JCOL)
                        CYCLE 
                     END IF 
                  END IF   
               END IF
               IF (LFH.AND.J.NE.I0) CYCLE
               WRITE(SARR(3),*) V
               WRITE(SARR(1),'(2A)') TRIM(ADJUSTL(SARR(1))),','
               WRITE(SARR(2),'(2A)') TRIM(ADJUSTL(SARR(2))),','
               WRITE(S,'(3A)')(TRIM(ADJUSTL(SARR(I))),I=1,3)    
               WRITE(LUN,'(A)') TRIM(S)            
            END DO
         END DO
         CLOSE(LUN)
      END IF
      
C        XC
      IF (LEN_TRIM(XPREF) > 0) THEN
         WRITE(FNAME,'(2A,3(I3.3,A),A)') TRIM(XPREF),
     1    '_',KPER,'_',KSTP,'_',KITER,'.',TRIM(FM) 
         OPEN(UNIT=LUN,FILE=TRIM(FNAME),FORM='FORMATTED',
     1      ACCESS='SEQUENTIAL',STATUS='UNKNOWN',ACTION='WRITE')  
         DO N = 1, NIAC
           WRITE(S,*) XC(N)
           WRITE(LUN,'(A)') TRIM(ADJUSTL(S))           
         END DO                 
         CLOSE(LUN)
      END IF
      
C        BC
      IF (LEN_TRIM(BPREF) > 0) THEN
         WRITE(FNAME,'(2A,3(I3.3,A),A)') TRIM(BPREF),
     1    '_',KPER,'_',KSTP,'_',KITER,'.',TRIM(FM)
         OPEN(UNIT=LUN,FILE=TRIM(FNAME),FORM='FORMATTED',
     1      ACCESS='SEQUENTIAL',STATUS='UNKNOWN',ACTION='WRITE')       
         DO N = 1, NIAC
           V = BCT(N)
           !IF (IBOUND(N).LT.0) THEN
           !!   V = -V
           !    V = VMAX*XC(N)
           !END IF
           WRITE(S,*) V
           WRITE(LUN,'(A)') TRIM(ADJUSTL(S))           
         END DO
         CLOSE(LUN)
      END IF
       
C        IXMAP
      IF (LEN_TRIM(MPREF) > 0) THEN
         WRITE(FNAME,'(2A,3(I3.3,A),A)') TRIM(MPREF),
     1    '_',KPER,'_',KSTP,'_',KITER,'.',TRIM(FM)
         OPEN(UNIT=LUN,FILE=TRIM(FNAME),FORM='FORMATTED',
     1      ACCESS='SEQUENTIAL',STATUS='UNKNOWN',ACTION='WRITE')       
         DO N = 1, NIAC
           WRITE(S,*) IXMAP(N)
           WRITE(LUN,'(A)') TRIM(ADJUSTL(S))           
         END DO
         CLOSE(LUN)
      END IF
      
      END SUBROUTINE EXPORT_SYSTEM_OLD
                   
      subroutine timing_date_and_time(idt)
      use pksmpitim_mod, only: ltiming
      implicit none
c        Arguments      
      integer, dimension(8), intent(out) :: idt
     
      if (.not.ltiming) return
      
      call date_and_time(values=idt)      
      
      end subroutine
      
      subroutine timing_add_sec(ibdt,iedt,val)
      use pksmpitim_mod, only: ltiming
      implicit none
c        Arguments      
      integer, dimension(8), intent(in) :: ibdt, iedt
      real, intent(inout) :: val
c        Locals
      integer, parameter :: NSPD = 86400
      integer, dimension(12) :: idpm(12)
      data idpm/31,28,31,30,31,30,31,31,30,31,30,31/ ! days per month
      integer :: ndays, nhours, nmins, nsecs, msecs 
      integer :: leap, mb, mc, me, mm, m, nm, ibd, ied
      real :: elsec, rsecs

      if (.not.ltiming) return
      
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
      
      val = val + elsec
      
      end subroutine
        
      subroutine timing_write()
      
      use pksmpitim_mod, only: ltiming,ntim,timsec,timlabel,
     &                       itimmet1,itimmet2,itimmpil,itimmpig,itimtot
      
      implicit none
c        arguments
c        functions      
      logical :: pks7mpimasterwrite
c        locals
      integer, parameter :: NSPD = 86400
      integer :: i, ndays, nhours, nmins, nsecs, nrsecs, msecs, nrproc
      real :: elsec, rsecs
      logical :: writesto
      logical :: lskip
      
      if (.not.ltiming) return
      
c        mpi
      call pks7mpigetnrproc(nrproc)
      call pks7mpilgxchtiming(timsec,ntim)
      writesto = pks7mpimasterwrite()
      if (writesto) then       
         timsec(ntim) = 0.0 
         do i = 1, ntim
            lskip = .false. 
            if (i.eq.itimmet1) lskip = .true.
            if (i.eq.itimmet2) lskip = .true.
            if (i.eq.itimmpil) lskip = .true.
            if (i.eq.itimmpig) lskip = .true.
            if (i.eq.itimtot)  lskip = .true.
            if (.not.lskip) timsec(itimtot) = timsec(itimtot)+timsec(i) 
         end do
         write(*,'(1x,a)') 'Timing results (t/nproc):'
         do i = 1, ntim
            elsec = timsec(i)
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
            if (i.eq.ntim) then
               WRITE(*,1000) timlabel(i),elsec
 1000          FORMAT(2X,a,': ',f15.3,' Seconds')
               cycle
            end if
            IF (RSECS.GE.0.5) NRSECS=NRSECS+1   
            IF (NDAYS.GT.0) THEN
               WRITE(*,1010) timlabel(i),NDAYS,NHOURS,NMINS,NRSECS
 1010          FORMAT(2X,a,': ',I3,' Days, ',I2,' Hours, ',I2,
     &         ' Minutes, ',I2,' Seconds')
            ELSEIF (NHOURS.GT.0) THEN
               WRITE(*,1020) timlabel(i),NHOURS,NMINS,NRSECS
 1020          FORMAT(2X,a,': ',I2,' Hours, ',I2,
     &            ' Minutes, ',I2,' Seconds')
            ELSEIF (NMINS.GT.0) THEN
               WRITE(*,1030) timlabel(i),NMINS,NSECS,MSECS
 1030          FORMAT(2X,a,': ',I2,' Minutes, ',
     &            I2,'.',I3.3,' Seconds')
            ELSE
               WRITE(*,1040) timlabel(i),NSECS,MSECS
 1040          FORMAT(2X,a,': ',I2,'.',I3.3,' Seconds')
            ENDIF
         end do
      
      end if
      if (nrproc.gt.1) call pks7mpibarrier()
      
      end subroutine