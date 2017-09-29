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

      MODULE PARAMMODULE
C  Data definitions for Named Parameters
C  Explicitly declare all variables to enable subroutines that include
C  this file to use the IMPLICIT NONE statement.
        PARAMETER (MXPAR=300000,MXCLST=1000000,MXINST=50000)
        INTEGER,SAVE,POINTER ::ICLSUM,IPSUM,INAMLOC,NMLTAR,NZONAR,NPVAL
        REAL,          SAVE,    DIMENSION(:),    POINTER ::B
        INTEGER,       SAVE,    DIMENSION(:),    POINTER ::IACTIVE
        INTEGER,       SAVE,    DIMENSION(:,:),  POINTER ::IPLOC
        INTEGER,       SAVE,    DIMENSION(:,:),  POINTER ::IPCLST
        INTEGER,       SAVE,    DIMENSION(:,:,:),POINTER ::IZON
        REAL,          SAVE,    DIMENSION(:,:,:),POINTER ::RMLT
        CHARACTER(LEN=10),SAVE, DIMENSION(:),    POINTER ::PARNAM
        CHARACTER(LEN=4), SAVE, DIMENSION(:),    POINTER ::PARTYP
        CHARACTER(LEN=10),SAVE, DIMENSION(:),    POINTER ::ZONNAM
        CHARACTER(LEN=10),SAVE, DIMENSION(:),    POINTER ::MLTNAM
        CHARACTER(LEN=10),SAVE, DIMENSION(:),    POINTER ::INAME
      TYPE PARAMTYPE
        INTEGER,POINTER  ::ICLSUM,IPSUM,INAMLOC,NMLTAR,NZONAR,NPVAL
        REAL,              DIMENSION(:),    POINTER ::B
        INTEGER,           DIMENSION(:),    POINTER ::IACTIVE
        INTEGER,           DIMENSION(:,:),  POINTER ::IPLOC
        INTEGER,           DIMENSION(:,:),  POINTER ::IPCLST
        INTEGER,           DIMENSION(:,:,:),POINTER ::IZON
        REAL,              DIMENSION(:,:,:),POINTER ::RMLT
        CHARACTER(LEN=10), DIMENSION(:),    POINTER ::PARNAM
        CHARACTER(LEN=4),  DIMENSION(:),    POINTER ::PARTYP
        CHARACTER(LEN=10), DIMENSION(:),    POINTER ::ZONNAM
        CHARACTER(LEN=10), DIMENSION(:),    POINTER ::MLTNAM
        CHARACTER(LEN=10), DIMENSION(:),    POINTER ::INAME
      END TYPE
      TYPE(PARAMTYPE), SAVE  ::PARAMDAT(10)
      END MODULE PARAMMODULE
      MODULE GWFBASMODULE
        INTEGER, SAVE, POINTER  ::MSUM
        INTEGER, SAVE, POINTER  ::IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN
        INTEGER, SAVE, POINTER  ::LBHDSV,LBDDSV,LBBOSV
        INTEGER, SAVE, POINTER  ::IBUDFL,ICBCFL,IHDDFL,IAUXSV,IBDOPT
        INTEGER, SAVE, POINTER  ::IPRTIM,IPEROC,ITSOC,ICHFLG
        INTEGER, SAVE, POINTER  ::IDDREF,IDDREFNEW
        REAL,    SAVE, POINTER  ::DELT,PERTIM,TOTIM,HNOFLO,HDRY,STOPER
        CHARACTER(LEN=20), SAVE, POINTER   ::CHEDFM,CDDNFM,CBOUFM
        INTEGER,           SAVE, DIMENSION(:,:), POINTER ::IOFLG
        REAL,              SAVE, DIMENSION(:,:), POINTER ::VBVL
        CHARACTER(LEN=16), SAVE, DIMENSION(:),   POINTER ::VBNM
      TYPE GWFBASTYPE
        INTEGER, POINTER  ::MSUM
        INTEGER, POINTER  ::IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN
        INTEGER, POINTER  ::LBHDSV,LBDDSV,LBBOSV
        INTEGER, POINTER  ::IBUDFL,ICBCFL,IHDDFL,IAUXSV,IBDOPT
        INTEGER, POINTER  ::IPRTIM,IPEROC,ITSOC,ICHFLG
        INTEGER, POINTER  ::IDDREF,IDDREFNEW
        REAL,    POINTER  ::DELT,PERTIM,TOTIM,HNOFLO,HDRY,STOPER
        CHARACTER(LEN=20), POINTER   ::CHEDFM,CDDNFM,CBOUFM
        INTEGER,           DIMENSION(:,:), POINTER ::IOFLG
        REAL,              DIMENSION(:,:), POINTER ::VBVL
        CHARACTER(LEN=16), DIMENSION(:),   POINTER ::VBNM
      END TYPE
      TYPE(GWFBASTYPE), SAVE  ::GWFBASDAT(10)
      END MODULE GWFBASMODULE


      SUBROUTINE GWF2BAS7AR(INUNIT,CUNIT,VERSION,IUDIS,IUZON,IUMLT,
     2              MAXUNIT,IGRID,IUOC,HEADNG,IUPVAL,MFVNAM)
C     ******************************************************************
C     Allocate and Read for GWF Basic Package
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,
     1                     LENUNI,IXSEC,ITRSS,INBAS,IFREFM,NODES,IOUT,
     2                     MXITER,IUNIT,NIUNIT,HNEW,LBOTM,LAYCBD,LAYHDT,
     3                     LAYHDS,PERLEN,NSTP,TSMULT,ISSFLG,DELR,DELC,
     4                     BOTM,HOLD,IBOUND,CR,CC,CV,HCOF,RHS,BUFF,STRT,
     5                     DDREF,
     7                     kdsv,                                         ! ANIPWT
     8                     IACTCELL                                      ! PKS   
      USE PARAMMODULE,ONLY:MXPAR,MXCLST,MXINST,ICLSUM,IPSUM,
     1                     INAMLOC,NMLTAR,NZONAR,NPVAL,
     2                     B,IACTIVE,IPLOC,IPCLST,PARNAM,PARTYP,
     3                     ZONNAM,MLTNAM,INAME
      USE GWFBASMODULE,ONLY:MSUM,IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN,
     1                      LBHDSV,LBDDSV,LBBOSV,IBUDFL,ICBCFL,IHDDFL,
     2                      IAUXSV,IBDOPT,IPRTIM,IPEROC,ITSOC,ICHFLG,
     3                      IDDREF,IDDREFNEW,DELT,PERTIM,TOTIM,HNOFLO,
     4                      HDRY,STOPER,CHEDFM,CDDNFM,CBOUFM,VBVL,VBNM
      use m_mf2005_iu, only: iuani, iumet, iupwt, iusfr, iulak, 
     1                      IUMNW1, IUMNW2, IUMNWI                      ! PKS
C
      CHARACTER*4 CUNIT(NIUNIT)
      CHARACTER*(*) VERSION
      CHARACTER*80 HEADNG(2)
      CHARACTER*(*) MFVNAM
      CHARACTER*200 LINE
C
      DOUBLE PRECISION HNF
      CHARACTER*24 ANAME(2)
      DATA ANAME(1) /'          BOUNDARY ARRAY'/
      DATA ANAME(2) /'            INITIAL HEAD'/
C     ------------------------------------------------------------------
C1------Allocate scalar variables, which makes it possible for multiple
C1------grids to be defined.
      ALLOCATE(NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,LENUNI,ITRSS)
      ALLOCATE(IXSEC,INBAS,IFREFM,NODES,IOUT,MXITER)
      MXITER=1
      ALLOCATE(IUNIT(NIUNIT))
C
      ALLOCATE(ICLSUM,IPSUM,INAMLOC,NMLTAR,NZONAR,NPVAL)
      ALLOCATE (B(MXPAR))
      ALLOCATE (IACTIVE(MXPAR))
      ALLOCATE (IPLOC(4,MXPAR))
      ALLOCATE (IPCLST(14,MXCLST))
      ALLOCATE (PARNAM(MXPAR))
      ALLOCATE (PARTYP(MXPAR))
      ALLOCATE (INAME(MXINST))
C
      ALLOCATE(MSUM,IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN,LBHDSV,LBDDSV,
     1         LBBOSV)
      ALLOCATE(IBUDFL,ICBCFL,IHDDFL,IAUXSV,IBDOPT,IPRTIM,IPEROC,ITSOC,
     1         ICHFLG,IDDREF,IDDREFNEW)
      ALLOCATE(DELT,PERTIM,TOTIM,HNOFLO,HDRY,STOPER)
      ALLOCATE(CHEDFM,CDDNFM,CBOUFM)
      HDRY=1.E30
      STOPER=0.0
C
C2------Open all files in name file.
      call fsplitnull(igrid)                                            ! DLT
      CALL SGWF2BAS7OPEN(INUNIT,IOUT,IUNIT,CUNIT,NIUNIT,
     &                 VERSION,INBAS,MAXUNIT,MFVNAM)
      call fsplitpsv(igrid)                                             ! DLT
C
C-------Check for not-supported packages
      IF(IUNIT(IUSFR).GT.0) CALL PKS7MPINOTSUPPORTED('SFR package')     ! PKS
      IF(IUNIT(IULAK).GT.0) CALL PKS7MPINOTSUPPORTED('LAK package')     ! PKS
      IF(IUNIT(IUMNW1).GT.0) CALL PKS7MPINOTSUPPORTED('MNW1 package')   ! PKS
      IF(IUNIT(IUMNW2).GT.0) CALL PKS7MPINOTSUPPORTED('MNW2 package')   ! PKS
      IF(IUNIT(IUMNWI).GT.0) CALL PKS7MPINOTSUPPORTED('MNWI package')   ! PKS
C
C3------PRINT A MESSAGE IDENTIFYING THE BASIC PACKAGE.
      WRITE(IOUT,1)INBAS
    1 FORMAT(1X,/1X,'BAS -- BASIC PACKAGE, VERSION 7, 5/2/2005',
     2' INPUT READ FROM UNIT ',I4)
C
C4------Initialize parameter definition variables.
      IPSUM=0
      ICLSUM=0
      INAMLOC=1
      DO 10 N=1,MXPAR
        PARNAM(N)=' '
        PARTYP(N)=' '
        IPLOC(1,N)=0
        IPLOC(2,N)=0
        IACTIVE(N)=0
   10 CONTINUE
C
      if(IUNIT(IUMET).gt.0) call gwf2met1ar(IUNIT(IUMET),igrid,iout)    ! MET
C5------Allocate and read discretization data.
      CALL SGWF2BAS7ARDIS(IUDIS,IOUT,igrid)
      NODES=NCOL*NROW*NLAY
C
C6------Allocate space for global arrays except discretization data.
      ALLOCATE (HNEW(NCOL,NROW,NLAY))
      ALLOCATE (HOLD(NCOL,NROW,NLAY))
      ALLOCATE (IBOUND(NCOL,NROW,NLAY))
      ALLOCATE (IACTCELL(NCOL,NROW,NLAY))                               ! PKS
      ALLOCATE (CR(NCOL,NROW,NLAY)); CR = 0.
      ALLOCATE (CC(NCOL,NROW,NLAY)); CC = 0.
      if (IUNIT(IUANI).gt.0.or.IUNIT(IUPWT).gt.0) then                  ! ANIPWT
         allocate(kdsv(ncol,nrow,nlay))                                 ! ANIPWT
      else                                                              ! ANIPWT
         allocate(kdsv(1,1,1))                                          ! ANIPWT
      end if                                                            ! ANIPWT
      ALLOCATE (CV(NCOL,NROW,NLAY)); CV = 0.
      ALLOCATE (HCOF(NCOL,NROW,NLAY)); HCOF = 0.
      ALLOCATE (RHS(NCOL,NROW,NLAY)); RHS = 0.      
      ALLOCATE (BUFF(NCOL,NROW,NLAY))
      ALLOCATE (STRT(NCOL,NROW,NLAY))
      DDREF=>STRT
      ALLOCATE (LAYHDT(NLAY))
      ALLOCATE (LAYHDS(NLAY))
C
C7------Initialize head-dependent thickness indicator to code that
C7------indicates layer is undefined.
      DO 100 I=1,NLAY
        LAYHDT(I)=-1
        LAYHDS(I)=-1
  100 CONTINUE
      WRITE(IOUT,'(//)')
C
C8------Read BAS Package file.
C8A-----READ AND PRINT COMMENTS.  SAVE THE FIRST TWO COMMENTS IN HEADNG.
      HEADNG(1)=' '
      HEADNG(2)=' '
      WRITE(IOUT,*)
      READ(INBAS,'(A)') LINE
      IF(LINE(1:1).NE.'#') GO TO 20
      HEADNG(1)=LINE(1:80)
      WRITE(IOUT,'(1X,A)') HEADNG(1)
      READ(INBAS,'(A)') LINE
      IF(LINE(1:1).NE.'#') GO TO 20
      HEADNG(2)=LINE(1:80)
      WRITE(IOUT,'(1X,A)') HEADNG(2)
      CALL URDCOM(INBAS,IOUT,LINE)
C
C8B-----LOOK FOR OPTIONS IN THE FIRST ITEM AFTER THE HEADING.
   20 IXSEC=0
      ICHFLG=0
      IFREFM=0
      IPRTIM=0
      STOPER=0.0
      LLOC=1
   25 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INBAS)
      IF(LINE(ISTART:ISTOP).EQ.'XSECTION') THEN
         IXSEC=1
      ELSE IF(LINE(ISTART:ISTOP).EQ.'CHTOCH') THEN
         ICHFLG=1
      ELSE IF(LINE(ISTART:ISTOP).EQ.'FREE') THEN
         IFREFM=1
         WRITE(IOUT,26)
   26    FORMAT (1X,'THE FREE FORMAT OPTION HAS BEEN SELECTED')
      ELSEIF(LINE(ISTART:ISTOP).EQ.'PRINTTIME') THEN
         IPRTIM=1
         WRITE(IOUT,7)
    7    FORMAT(1X,'THE PRINTTIME OPTION HAS BEEN SELECTED')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'STOPERROR') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,STOPER,IOUT,INBAS)
         WRITE(IOUT,8) STOPER
    8    FORMAT(1X,'When solver convergence criteria are not met,',/
     1    1X,'execution will continue unless the budget percent',/
     2    1X,'discrepancy is greater than:',F10.4)
      END IF
      IF(LLOC.LT.200) GO TO 25
C
C8C-----PRINT A MESSAGE SHOWING OPTIONS.
      IF(IXSEC.NE.0) WRITE(IOUT,61)
   61 FORMAT(1X,'CROSS SECTION OPTION IS SPECIFIED')
      IF(ICHFLG.NE.0) WRITE(IOUT,62)
   62 FORMAT(1X,'CALCULATE FLOW BETWEEN ADJACENT CONSTANT-HEAD CELLS')
C
C8D-----INITIALIZE TOTAL ELAPSED TIME COUNTER STORAGE ARRAY COUNTER
C8D-----AND CALCULATE NUMBER OF CELLS.
      TOTIM=0.
C
C8E-----READ BOUNDARY ARRAY(IBOUND).
      IF(IXSEC.EQ.0) THEN
         DO 280 K=1,NLAY
         KK=K
         CALL U2DINT(IBOUND(:,:,KK),ANAME(1),NROW,NCOL,KK,INBAS,IOUT)
  280    CONTINUE
      ELSE
         CALL U2DINT(IBOUND(:,:,1),ANAME(1),NLAY,NCOL,-1,INBAS,IOUT)
      END IF
      if(IUNIT(IUMET).gt.0)
     1   call gwf2met1ibound(ibound,ncol,nrow,nlay,igrid)               ! MET
C
C-------SET IACTCELL
      DO K=1,NLAY                                                       ! PKS
        DO I=1,NROW                                                     ! PKS
          DO J=1,NCOL                                                   ! PKS
            IF (IBOUND(J,I,K).GT.0) THEN                                ! PKS
              IACTCELL(J,I,K) = 1                                       ! PKS
            ELSE                                                        ! PKS
              IACTCELL(J,I,K) = 0                                       ! PKS
            END IF                                                      ! PKS
          END DO                                                        ! PKS
        END DO                                                          ! PKS
      END DO                                                            ! PKS
C      
C8F-----READ AND PRINT HEAD VALUE TO BE PRINTED FOR NO-FLOW CELLS.
      IF(IFREFM.EQ.0) THEN
         READ(INBAS,'(F10.0)') HNOFLO
      ELSE
         READ(INBAS,*) HNOFLO
      END IF
      HNF=HNOFLO
      WRITE(IOUT,3) HNOFLO
    3 FORMAT(1X,/1X,'AQUIFER HEAD WILL BE SET TO ',1PG12.5,
     1       ' AT ALL NO-FLOW NODES (IBOUND=0).')
C
C8G-----READ INITIAL HEADS.
      IF(IXSEC.EQ.0) THEN
         DO 300 K=1,NLAY
         KK=K
         CALL U2DREL(STRT(:,:,KK),ANAME(2),NROW,NCOL,KK,INBAS,IOUT)
  300    CONTINUE
      ELSE
         CALL U2DREL(STRT(:,:,1),ANAME(2),NLAY,NCOL,-1,INBAS,IOUT)
      END IF
      !## apply consistency check constant head and top/bot
      do i=1,nrow; do j=1,ncol; do k=1,nlay
       if(ibound(j,i,k).lt.0)then

        !## in current model layer
        kkk=k*2-1
        if(strt(j,i,k).gt.botm(j,i,kkk))cycle

        !## constant head cell dry - becomes active node - shift to an appropriate model layer where the head is in
        do kk=k,nlay
         kkk=kk*2-1
         if(strt(j,i,k).le.botm(j,i,kkk))then
          ibound(j,i,kk)=1
          strt(j,i,kk)=strt(j,i,k)
         else
          ibound(j,i,kk)=-99
          strt(j,i,kk)=strt(j,i,k)
          exit
         endif
        enddo
       endif
      enddo; enddo; enddo

      !## cleaning for constant head cells that are only connected to other constant head/inactive cells    
      do k=1,nlay; do i=1,nrow; do j=1,ncol
       ic1=max(j-1,1); ic2=min(j+1,ncol)
       ir1=max(i-1,1); ir2=min(i+1,nrow)
       il1=max(k-1,1); il2=min(k+1,nlay)
       if(ibound(j,i,k).lt.0)then 
        if(ibound(j,ir1,k).le.0.and.ibound(j,ir2,k).le.0.and.
     1     ibound(ic1,i,k).le.0.and.ibound(ic2,i,k).le.0.and.
     1     ibound(j,i,il1).le.0.and.ibound(j,i,il2).le.0)then 
         ibound(j,i,k)=0
        end if
       end if
      enddo; enddo; enddo

      !## clean corners
      do k=1,nlay; ibound(1   ,1   ,k)=0; enddo
      do k=1,nlay; ibound(ncol,1   ,k)=0; enddo
      do k=1,nlay; ibound(1   ,nrow,k)=0; enddo
      do k=1,nlay; ibound(ncol,nrow,k)=0; enddo
C
C9------COPY INITIAL HEADS FROM STRT TO HNEW.
      DO 400 K=1,NLAY
      DO 400 I=1,NROW
      DO 400 J=1,NCOL
      HNEW(J,I,K)=STRT(J,I,K)
      IF(IBOUND(J,I,K).EQ.0) HNEW(J,I,K)=HNF
  400 CONTINUE
C
C10-----SET UP OUTPUT CONTROL.
      CALL SGWF2BAS7I(NLAY,IUNIT(IUOC),IOUT,IFREFM,NIUNIT)
C
C11-----INITIALIZE VOLUMETRIC BUDGET ACCUMULATORS TO ZERO.
  590 ZERO=0.
      DO 600 I=1,NIUNIT
      DO 600 J=1,4
      VBVL(J,I)=ZERO
  600 CONTINUE
C
C12-----Allocate and read Zone and Multiplier arrays
      CALL SGWF2BAS7ARMZ(IUNIT(IUZON),IUNIT(IUMLT))
C
C13-----READ PARAMETER VALUES FILE.
      CALL SGWF2BAS7ARPVAL(IUPVAL)
C
C14-----SAVE POINTERS TO DATA AND RETURN.
      CALL SGWF2BAS7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2BAS7ST(KPER,IGRID)
C     ******************************************************************
C     SETUP TIME VARIABLES FOR NEW TIME PERIOD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,PERLEN,NSTP,TSMULT
      USE GWFBASMODULE,ONLY:DELT,PERTIM
C     ------------------------------------------------------------------
      CALL SGWF2BAS7PNT(IGRID)
C
C1------WRITE STRESS PERIOD INFORMATION
      WRITE (IOUT,1) KPER,PERLEN(KPER),NSTP(KPER),TSMULT(KPER)
    1 FORMAT('1',/28X,'STRESS PERIOD NO. ',I4,', LENGTH =',G15.7,/
     1            28X,47('-'),//
     2            30X,'NUMBER OF TIME STEPS =',I6,//
     3            31X,'MULTIPLIER FOR DELT =',F10.3)

      iss=1; if(perlen(kper).eq.0.0)iss=0
C
C2------CALCULATE THE LENGTH OF THE FIRST TIME STEP.
C
C2A-----ASSUME TIME STEP MULTIPLIER IS EQUAL TO ONE.
      DELT=PERLEN(KPER)/FLOAT(NSTP(KPER))
C
C2B-----IF TIME STEP MULTIPLIER IS NOT ONE THEN CALCULATE FIRST
C2B-----TERM OF GEOMETRIC PROGRESSION.
      ONE=1.
      IF(TSMULT(KPER).NE.ONE)
     1    DELT=PERLEN(KPER)*(ONE-TSMULT(KPER))/
     2        (ONE-TSMULT(KPER)**NSTP(KPER))
C
C3------PRINT THE LENGTH OF THE FIRST TIME STEP.
      WRITE (IOUT,9) DELT
    9 FORMAT(1X,/28X,'INITIAL TIME STEP SIZE =',G15.7)
C
C4------INITIALIZE PERTIM (ELAPSED TIME WITHIN STRESS PERIOD).
      PERTIM=0.
C
C5------CHECK THAT ALL PARAMETERS IN PARAMETER VALUE FILE HAVE BEEN DEFINED.
      IF(KPER.GT.1) CALL SGWF2BAS7STPVAL()
C
C6------RETURN
      RETURN
      END
      SUBROUTINE GWF2BAS7AD(KPER,KSTP,IGRID)
C     ******************************************************************
C     ADVANCE TO NEXT TIME STEP
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,TSMULT,HNEW,HOLD
      USE GWFBASMODULE,ONLY:DELT,TOTIM,PERTIM
C     ------------------------------------------------------------------
      CALL SGWF2BAS7PNT(IGRID)
C
C1------IF NOT FIRST TIME STEP THEN CALCULATE TIME STEP LENGTH.
      IF(KSTP.NE.1) DELT=TSMULT(KPER)*DELT
C
C2------ACCUMULATE ELAPSED TIME IN SIMULATION(TOTIM) AND IN THIS
C2------STRESS PERIOD(PERTIM).
      TOTIM=TOTIM+DELT
      PERTIM=PERTIM+DELT
C
C3------COPY HNEW TO HOLD.
      DO 10 K=1,NLAY
      DO 10 I=1,NROW
      DO 10 J=1,NCOL
   10 HOLD(J,I,K)=HNEW(J,I,K)
C
C4------RETURN
      RETURN
      END
      SUBROUTINE GWF2BAS7FM(IGRID)
C     ******************************************************************
C     SET HCOF=RHS=0.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,ONLY:HCOF,RHS
C     ------------------------------------------------------------------
      CALL SGWF2BAS7PNT(IGRID)
C
C1------FOR EACH CELL INITIALIZE HCOF AND RHS ACCUMULATORS.
      ZERO=0.
      HCOF=ZERO
      RHS=ZERO
C
C2------RETURN
      RETURN
      END
      SUBROUTINE GWF2BAS7OC(KSTP,KPER,ICNVG,INOC,IGRID)
C     ******************************************************************
C     OUTPUT CONTROLLER FOR HEAD, DRAWDOWN, AND BUDGET
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NLAY,NSTP,IXSEC,IFREFM
      USE GWFBASMODULE,ONLY:IHDDFL,IBUDFL,ICBCFL,IPEROC,ITSOC,IBDOPT,
     1                      IOFLG
C
C     ------------------------------------------------------------------
      CALL SGWF2BAS7PNT(IGRID)
C
C1------TEST UNIT NUMBER (INOC (INOC=IUNIT(IUOC))) TO SEE IF
C1------OUTPUT CONTROL IS ACTIVE.  IF NOT, SET DEFAULTS AND RETURN.
      IF(INOC.LE.0) THEN
         IHDDFL=0
         IF(ICNVG.EQ.0 .OR. KSTP.EQ.NSTP(KPER))IHDDFL=1
         IBUDFL=0
         IF(ICNVG.EQ.0 .OR. KSTP.EQ.NSTP(KPER))IBUDFL=1
         ICBCFL=0
         GO TO 1000
      END IF
C
C2------OUTPUT CONTROL IS ACTIVE.  IF IPEROC >= 0, READ OUTPUT FLAGS
C2------USING ALPHABETIC INPUT STRUCTURE.
      IF(IPEROC.GE.0) THEN
         CALL SGWF2BAS7N(KPER,KSTP,INOC,IOUT,NLAY)
         GO TO 600
      END IF
C
C3------READ AND PRINT OUTPUT FLAGS AND CODE FOR DEFINING IOFLG USING
C3------THE ORIGINAL NUMERIC INPUT STRUCTURE.
      IF(IFREFM.EQ.0) THEN
         READ(INOC,'(4I10)') INCODE,IHDDFL,IBUDFL,ICBCFL
      ELSE
         READ(INOC,*) INCODE,IHDDFL,IBUDFL,ICBCFL
      END IF
      WRITE(IOUT,3) IHDDFL,IBUDFL,ICBCFL
    3 FORMAT(1X,/1X,'HEAD/DRAWDOWN PRINTOUT FLAG =',I2,
     1    5X,'TOTAL BUDGET PRINTOUT FLAG =',I2,
     2   /1X,'CELL-BY-CELL FLOW TERM FLAG =',I2)
      IF(ICBCFL.NE.0) ICBCFL=IBDOPT
C
C4------DECODE INCODE TO DETERMINE HOW TO SET FLAGS IN IOFLG.
      IF(INCODE.LT.0) THEN
C
C5------INCODE <0, USE IOFLG FROM LAST TIME STEP.
        WRITE(IOUT,101)
  101   FORMAT(1X,'REUSING PREVIOUS VALUES OF IOFLG')
      ELSE IF(INCODE.EQ.0) THEN
C
C6------INCODE=0, READ IOFLG FOR LAYER 1 AND ASSIGN SAME TO ALL LAYERS
        IF(IFREFM.EQ.0) THEN
           READ(INOC,'(4I10)') (IOFLG(1,M),M=1,4)
        ELSE
           READ(INOC,*) (IOFLG(1,M),M=1,4)
        END IF
        IOFLG(1,5)=0
        DO 210 K=1,NLAY
        IOFLG(K,1)=IOFLG(1,1)
        IOFLG(K,2)=IOFLG(1,2)
        IOFLG(K,3)=IOFLG(1,3)
        IOFLG(K,4)=IOFLG(1,4)
        IOFLG(K,5)=IOFLG(1,5)
  210   CONTINUE
        WRITE(IOUT,211) (IOFLG(1,M),M=1,4)
  211   FORMAT(1X,/1X,'OUTPUT FLAGS FOR ALL LAYERS ARE THE SAME:'/
     1     1X,'  HEAD    DRAWDOWN  HEAD  DRAWDOWN'/
     2     1X,'PRINTOUT  PRINTOUT  SAVE    SAVE'/
     3     1X,34('-')/1X,I5,I10,I8,I8)
      ELSE
C
C7------INCODE>0, READ IOFLG IN ENTIRETY -- IF CROSS SECTION, READ ONLY
C7------ONE VALUE.
        IF(IXSEC.EQ.0) THEN
           DO 301 K=1,NLAY
           IF(IFREFM.EQ.0) THEN
              READ(INOC,'(4I10)') (IOFLG(K,M),M=1,4)
           ELSE
              READ(INOC,*) (IOFLG(K,M),M=1,4)
           END IF
           IOFLG(K,5)=0
  301      CONTINUE
           WRITE(IOUT,302) 'OUTPUT FLAGS FOR EACH LAYER:','LAYER'
  302      FORMAT(1X,/1X,A,/
     1     1X,'         HEAD    DRAWDOWN  HEAD  DRAWDOWN'/
     2     1X,A,'  PRINTOUT  PRINTOUT  SAVE    SAVE'/
     3     1X,41('-'))
           WRITE(IOUT,303) (K,(IOFLG(K,M),M=1,4),K=1,NLAY)
  303      FORMAT(1X,I4,I8,I10,I8,I8)
        ELSE
           IF(IFREFM.EQ.0) THEN
              READ(INOC,'(4I10)') (IOFLG(1,M),M=1,4)
           ELSE
              READ(INOC,*) (IOFLG(1,M),M=1,4)
           END IF
           WRITE(IOUT,302) 'OUTPUT FLAGS FOR CROSS SECTION:','     '
           WRITE(IOUT,304) (IOFLG(1,M),M=1,4)
  304      FORMAT(1X,I12,I10,I8,I8)
        END IF
      END IF
C
C8------THE LAST STEP IN A STRESS PERIOD AND STEPS WHERE ITERATIVE
C8------PROCEDURE FAILED TO CONVERGE GET A VOLUMETRIC BUDGET.
  600 IF(ICNVG.EQ.0 .OR. KSTP.EQ.NSTP(KPER)) IBUDFL=1
C
C9------RETURN
 1000 RETURN
C
      END
      SUBROUTINE GWF2BAS7OT(KSTP,KPER,ICNVG,ISA,IGRID,BUDPERC)
C     ******************************************************************
C     OUTPUT TIME, VOLUMETRIC BUDGET, HEAD, AND DRAWDOWN
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:ITMUNI,IOUT,NCOL,NROW,NLAY,HNEW,STRT,DDREF
      USE GWFBASMODULE,ONLY:DELT,PERTIM,TOTIM,IHDDFL,IBUDFL,
     1                      MSUM,VBVL,VBNM,IDDREF
C     ------------------------------------------------------------------
C
      CALL SGWF2BAS7PNT(IGRID)
C
C1------CLEAR PRINTOUT FLAG (IPFLG)
      IPFLG=0
      BUDPERC=1.E30
C
C
      IF(ISA.EQ.0) THEN
         WRITE(IOUT,9) KSTP,KPER
    9    FORMAT(1X,/11X,'NO FLOW EQUATION TO SOLVE IN TIME STEP',I3,
     1      ' OF STRESS PERIOD',I3,/1X,'ALL HEADS ARE 0.0')
         IPFLG=1
      END IF
C
C2------IF ITERATIVE PROCEDURE FAILED TO CONVERGE PRINT MESSAGE
      IF(ICNVG.EQ.0) THEN
         WRITE(IOUT,17) KSTP,KPER
   17    FORMAT(1X,/11X,
     1 '****FAILED TO MEET SOLVER CONVERGENCE CRITERIA IN TIME STEP',
     2   I3,' OF STRESS PERIOD ',I4,'****')
         IPFLG=1
      END IF
C
C3------IF HEAD AND DRAWDOWN FLAG (IHDDFL) IS SET WRITE HEAD,
C3------DRAWDOWN, AND IBOUND IN ACCORDANCE WITH FLAGS IN IOFLG.
      IF(IHDDFL.EQ.0) GO TO 100
C
      CALL SGWF2BAS7H(KSTP,KPER,IPFLG,ISA)
      CALL SGWF2BAS7D(KSTP,KPER,IPFLG,ISA)
      CALL SGWF2BAS7IB(KSTP,KPER)
C
  100 CONTINUE

C4------PRINT TOTAL BUDGET IF REQUESTED
      IF(IBUDFL.EQ.0) GO TO 120
      CALL SGWF2BAS7V(MSUM,VBNM,VBVL,KSTP,KPER,IOUT,BUDPERC)
      IPFLG=1
C
C5------END PRINTOUT WITH TIME SUMMARY AND FORM FEED IF ANY PRINTOUT
C5------WILL BE PRODUCED.
  120 IF(IDDREF.NE.0) THEN
         IF(ASSOCIATED(DDREF,STRT)) THEN
            ALLOCATE(DDREF(NCOL,NROW,NLAY))
            CALL SGWF2BAS7PSV(IGRID)
         END IF
         DDREF=HNEW
         WRITE(IOUT,99)
   99    FORMAT(1X,'Drawdown Reference has been reset to the',
     1               ' end of this time step')
         IDDREF=0
      END IF
      IF(IPFLG.EQ.0) RETURN
      CALL SGWF2BAS7T(KSTP,KPER,DELT,PERTIM,TOTIM,ITMUNI,IOUT)
      WRITE(IOUT,101)
  101 FORMAT('1')
C
C6------RETURN
      RETURN
      END
      SUBROUTINE SGWF2BAS7ARDIS(IUDIS,IOUT,igrid)
C     *****************************************************************
C     ALLOCATE AND READ DIS DATA
C     *****************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,
     1                     LENUNI,IUNIT,LBOTM,LAYCBD,ITRSS,
     3                     PERLEN,NSTP,TSMULT,ISSFLG,DELR,DELC,BOTM
      use m_mf2005_iu, only: iumet, iupks
C
      CHARACTER*200 LINE
      CHARACTER*24 ANAME(5)
      DATA ANAME(1) /'                    DELR'/
      DATA ANAME(2) /'                    DELC'/
      DATA ANAME(3) /'TOP ELEVATION OF LAYER 1'/
      DATA ANAME(4) /'  MODEL LAYER BOTTOM EL.'/
      DATA ANAME(5) /'BOT. EL. OF QUASI-3D BED'/
      LOGICAL LTBCHECK                                                  ! DLT
      REAL T, B
C     ------------------------------------------------------------------
C
C1------Check for existence of discretization file
      INDIS=IUNIT(IUDIS)
      IF(INDIS.LE.0) THEN
         WRITE(IOUT,*) ' DIS file must be specified for MODFLOW to run'
         CALL USTOP(' ')
      END IF
      WRITE(IOUT,11) INDIS
   11 FORMAT(1X,/1X,'DISCRETIZATION INPUT DATA READ FROM UNIT ',I4)
C
C
C2------Read comments and the first line following the comments.
      CALL URDCOM(INDIS,IOUT,LINE)
C
C3------Get the number of layers, rows, columns, stress periods,
C3------ITMUNI, and LENUNI from the line.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NLAY,R,IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NROW,R,IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCOL,R,IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPER,R,IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMUNI,R,IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,LENUNI,R,IOUT,INDIS)
C      
      CALL PKS7MPIPART(NCOL, NROW, NLAY, IUNIT(IUPKS), IOUT)            ! PKS
      CALL GWF2MET1PKS(IGRID,NLAY,NROW,NCOL)                            ! PKS
      CALL PKS7MPILXCHINI()                                             ! PKS
C
C read options
      LTBCHECK = .FALSE.                                                ! DLT
   12 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)                 ! DLT
      IF(LINE(ISTART:ISTOP).EQ.'TBCHECK') THEN                          ! DLT
         LTBCHECK=.TRUE.                                                ! DLT
      ENDIF                                                             ! DLT
      IF(LLOC.LT.200) GO TO 12                                          ! DLT

C4------PRINT # OF LAYERS, ROWS, COLUMNS AND STRESS PERIODS.
      WRITE(IOUT,15) NLAY,NROW,NCOL
   15 FORMAT(1X,I4,' LAYERS',I10,' ROWS',I10,' COLUMNS')
      WRITE(IOUT,20) NPER
   20 FORMAT(1X,I4,' STRESS PERIOD(S) IN SIMULATION')
C
C5------SELECT AND PRINT A MESSAGE SHOWING TIME UNIT.
      IF(ITMUNI.LT.0 .OR. ITMUNI.GT.5) ITMUNI=0
      IF(ITMUNI.EQ.0) THEN
         WRITE(IOUT,30)
   30    FORMAT(1X,'MODEL TIME UNIT IS UNDEFINED')
      ELSE IF(ITMUNI.EQ.1) THEN
         WRITE(IOUT,40)
   40    FORMAT(1X,'MODEL TIME UNIT IS SECONDS')
      ELSE IF(ITMUNI.EQ.2) THEN
         WRITE(IOUT,50)
   50    FORMAT(1X,'MODEL TIME UNIT IS MINUTES')
      ELSE IF(ITMUNI.EQ.3) THEN
         WRITE(IOUT,60)
   60    FORMAT(1X,'MODEL TIME UNIT IS HOURS')
      ELSE IF(ITMUNI.EQ.4) THEN
         WRITE(IOUT,70)
   70    FORMAT(1X,'MODEL TIME UNIT IS DAYS')
      ELSE
         WRITE(IOUT,80)
   80    FORMAT(1X,'MODEL TIME UNIT IS YEARS')
      END IF
C
C6------SELECT AND PRINT A MESSAGE SHOWING LENGTH UNIT.
      IF(LENUNI.LT.0 .OR. LENUNI.GT.3) LENUNI=0
      IF(LENUNI.EQ.0) THEN
         WRITE(IOUT,90)
   90    FORMAT(1X,'MODEL LENGTH UNIT IS UNDEFINED')
      ELSE IF(LENUNI.EQ.1) THEN
         WRITE(IOUT,91)
   91    FORMAT(1X,'MODEL LENGTH UNIT IS FEET')
      ELSE IF(LENUNI.EQ.2) THEN
         WRITE(IOUT,93)
   93    FORMAT(1X,'MODEL LENGTH UNIT IS METERS')
      ELSE IF(LENUNI.EQ.3) THEN
         WRITE(IOUT,95)
   95    FORMAT(1X,'MODEL LENGTH UNIT IS CENTIMETERS')
      END IF
C
C7------ALLOCATE LAYER FLAGS.
      ALLOCATE(LBOTM(NLAY))
      ALLOCATE(LAYCBD(NLAY))
C
C8------Read confining bed information
      READ(INDIS,*) (LAYCBD(K),K=1,NLAY)
      LAYCBD(NLAY)=0
      WRITE(IOUT,*) ' Confining bed flag for each layer:'
      WRITE(IOUT,'(20I4)') (LAYCBD(K),K=1,NLAY)
C
C9------Count confining beds, setup the pointer to each layer's
C9------bottom array (LBOTM), and setup LAYCBD to be the confining
C9------bed number for each layer.
      NCNFBD=0
      DO 100 K=1,NLAY
      LBOTM(K)=K+NCNFBD
      IF(LAYCBD(K).NE.0) THEN
         NCNFBD=NCNFBD+1
         LAYCBD(K)=NCNFBD
      END IF
  100 CONTINUE
      NBOTM=NLAY+NCNFBD
C
C10-----Allocate space for discretization arrays
C10-----Note that NBOTM+1 arrays are allocated for BOTM
C10-----because BOTM(J,I,0) contains the top elevation of layer 1.
      ALLOCATE (DELR(NCOL))
      ALLOCATE (DELC(NROW))
      ALLOCATE (BOTM(NCOL,NROW,0:NBOTM))
      ALLOCATE (PERLEN(NPER),NSTP(NPER),TSMULT(NPER),ISSFLG(NPER))
C
C11-----Read the DELR and DELC arrays.
      CALL PKS7MPIGETNRPROC(NRPROC)                                     ! PKS
      IF (NRPROC.GT.1)THEN                                              ! PKS
         CALL PKS7MPISETDELRDELC(DELR,DELC,NCOL,NROW,INDIS)             ! PKS
      ELSE                                                              ! PKS
         CALL U1DREL(DELR,ANAME(1),NCOL,INDIS,IOUT)
         CALL U1DREL(DELC,ANAME(2),NROW,INDIS,IOUT)
      END IF                                                            ! PKS
         
      if(IUNIT(IUMET).gt.0)
     1   call gwf2met1extent(igrid,ncol,nrow,delr,delc)
C
C12-----Read the top elevation of layer 1.
      CALL U2DREL(BOTM(:,:,0),ANAME(3),NROW,NCOL,0,INDIS,IOUT)
C
C13-----Read the bottom elevations.
      DO 120 K=1,NLAY
      KK=K
      CALL U2DREL(BOTM(:,:,LBOTM(K)),ANAME(4),NROW,NCOL,KK,INDIS,IOUT)
      IF(LAYCBD(K).NE.0) CALL U2DREL(BOTM(:,:,LBOTM(K)+1),ANAME(5),
     1          NROW,NCOL,KK,INDIS,IOUT)
  120 CONTINUE
C
      if (ltbcheck) then                                                ! DLT
         n = 0                                                          ! DLT
         do k = 1, nbotm                                                ! DLT
            do i = 1, nrow                                              ! DLT
               do j = 1, ncol                                           ! DLT
                  t = botm(j,i,k-1)                                     ! DLT
                  b = botm(j,i,k)                                       ! DLT
                  if (t.lt.b) then                                      ! DLT
                     n = n + 1                                          ! DLT
                     botm(j,i,k) = min(b,t)                             ! DLT
                     if (k.lt.nbotm) then                               ! DLT
                        t = botm(j,i,k+1)                               ! DLT
                        botm(j,i,k+1) = min(b,t)                        ! DLT
                     end if                                             ! DLT
                  end if                                                ! DLT
               end do                                                   ! DLT
            end do                                                      ! DLT
         end do                                                         ! DLT
         if (n.gt.0) then                                               ! DLT
            write(*,*) 'Top/bot consistency check applied:',n,          ! DLT
     1         'cells adjusted'                                         ! DLT
         end if                                                         ! DLT
      end if                                                            ! DLT

C14-----READ AND WRITE LENGTH OF STRESS PERIOD, NUMBER OF TIME STEPS,
C14-----TIME STEP MULTIPLIER, AND STEADY-STATE FLAG..
      WRITE(IOUT,161)
  161 FORMAT(1X,//1X,'STRESS PERIOD     LENGTH       TIME STEPS',
     1            '     MULTIPLIER FOR DELT    SS FLAG',/1X,76('-'))
      ISS=0
      ITR=0
      DO 200 N=1,NPER
      READ(INDIS,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,PERLEN(N),IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSTP(N),R,IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TSMULT(N),IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,INDIS)
      IF (LINE(ISTART:ISTOP).EQ.'TR') THEN
         ISSFLG(N)=0
         ITR=1
      ELSE IF (LINE(ISTART:ISTOP).EQ.'SS') THEN
         ISSFLG(N)=1
         ISS=1
      ELSE
         WRITE(IOUT,162)
  162    FORMAT(' SSFLAG MUST BE EITHER "SS" OR "TR"',
     1      ' -- STOP EXECUTION (SGWF2BAS7ARDIS)')
         CALL USTOP(' ')
      END IF
      WRITE (IOUT,163) N,PERLEN(N),NSTP(N),TSMULT(N),LINE(ISTART:ISTOP)
!      write(*,163) N,PERLEN(N),NSTP(N),TSMULT(N),LINE(ISTART:ISTOP)
!      write(*,*) iss

  163 FORMAT(1X,I8,1PG21.7,I7,0PF25.3,A11)
C
C15-----STOP IF NSTP LE 0, PERLEN EQ 0 FOR TRANSIENT STRESS PERIODS,
C15-----TSMULT LE 0, OR PERLEN LT 0..
      IF(NSTP(N).LE.0) THEN
         WRITE(IOUT,164)
  164    FORMAT(1X,/1X,
     1  'THERE MUST BE AT LEAST ONE TIME STEP IN EVERY STRESS PERIOD')
         CALL USTOP(' ')
      END IF
      ZERO=0.
      IF(PERLEN(N).EQ.ZERO .AND. ISSFLG(N).EQ.0) THEN
         WRITE(IOUT,165)
  165    FORMAT(1X,/1X,
     1  'PERLEN MUST NOT BE 0.0 FOR TRANSIENT STRESS PERIODS')
         CALL USTOP(' ')
      END IF
      IF(TSMULT(N).LE.ZERO) THEN
         WRITE(IOUT,170)
  170    FORMAT(1X,/1X,'TSMULT MUST BE GREATER THAN 0.0')
         CALL USTOP(' ')
      END IF
      IF(PERLEN(N).LT.ZERO) THEN
         WRITE(IOUT,175)
  175    FORMAT(1X,/1X,
     1  'PERLEN CANNOT BE LESS THAN 0.0 FOR ANY STRESS PERIOD')
         CALL USTOP(' ')
      END IF
  200 CONTINUE
C
      if(IUNIT(IUMET).gt.0) call gwf2met1iss(igrid)                     ! MET
C
C16-----Assign ITRSS.
      IF(ISS.EQ.0 .AND. ITR.NE.0) THEN
         ITRSS=1
         WRITE(IOUT,270)
  270    FORMAT(/,1X,'TRANSIENT SIMULATION')
      ELSE IF(ISS.NE.0 .AND. ITR.EQ.0) THEN
         ITRSS=0
         WRITE(IOUT,275)
  275    FORMAT(/,1X,'STEADY-STATE SIMULATION')
      ELSE
         ITRSS=-1
         WRITE(IOUT,280)
  280    FORMAT(/,1X,'COMBINED STEADY-STATE AND TRANSIENT SIMULATION')
      END IF
C
C17-----RETURN.
      RETURN
      END
      SUBROUTINE SGWF2BAS7D(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     CALCULATE, PRINT, AND SAVE DRAWDOWNS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,HNEW,BUFF,IBOUND,
     1                      DDREF,IOUT
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IDDNFM,IDDNUN,LBDDSV,
     2                      CDDNFM,IOFLG
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION SSTRT
C
      DATA TEXT /'        DRAWDOWN'/
C     ------------------------------------------------------------------
C
C1------FOR EACH LAYER CALCULATE DRAWDOWN IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C2------IS DRAWDOWN NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC.NE.0) KL=1
      IF(IOFLG(KL,2).EQ.0 .AND. IOFLG(KL,4).EQ.0) GO TO 59
C
C4------CALCULATE DRAWDOWN FOR THE LAYER.
      DO 58 I=1,NROW
      DO 58 J=1,NCOL
      BUFF(J,I,K)=HNEW(J,I,K)
      SSTRT=DDREF(J,I,K)
      IF(IBOUND(J,I,K).NE.0) BUFF(J,I,K)=SSTRT-HNEW(J,I,K)
   58 CONTINUE
   59 CONTINUE
C
C5------FOR EACH LAYER: DETERMINE IF DRAWDOWN SHOULD BE PRINTED.
C5------IF SO THEN CALL ULAPRS OR ULAPRW TO PRINT DRAWDOWN.
      IF(ISA.NE.0) THEN
         IF(IXSEC.EQ.0) THEN
           DO 69 K=1,NLAY
           KK=K
           IF(IOFLG(K,2).EQ.0) GO TO 69
           IF(IDDNFM.LT.0) CALL ULAPRS(BUFF(:,:,K),TEXT,KSTP,KPER,
     1                  NCOL,NROW,KK,-IDDNFM,IOUT)
           IF(IDDNFM.GE.0) CALL ULAPRW(BUFF(:,:,K),TEXT,KSTP,KPER,
     1                  NCOL,NROW,KK,IDDNFM,IOUT)
           IPFLG=1
   69      CONTINUE
C
C5A-----PRINT DRAWDOWN FOR CROSS SECTION.
         ELSE
           IF(IOFLG(1,2).NE.0) THEN
             IF(IDDNFM.LT.0) CALL ULAPRS(BUFF,TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,-IDDNFM,IOUT)
             IF(IDDNFM.GE.0) CALL ULAPRW(BUFF,TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,IDDNFM,IOUT)
             IPFLG=1
           END IF
         END IF
      END IF
C
C6------FOR EACH LAYER: DETERMINE IF DRAWDOWN SHOULD BE SAVED.
C6------IF SO THEN CALL A ULASAV OR ULASV2 TO RECORD DRAWDOWN.
      IFIRST=1
      IF(IDDNUN.LE.0) GO TO 80
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,4).EQ.0) GO TO 79
        IF(IFIRST.EQ.1) WRITE(IOUT,74) IDDNUN,KSTP,KPER
   74   FORMAT(1X,/1X,'DRAWDOWN WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
        IFIRST=0
        IF(CDDNFM.EQ.' ') THEN
           CALL ULASAV(BUFF(:,:,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IDDNUN)
        ELSE
           CALL ULASV2(BUFF(:,:,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IDDNUN,CDDNFM,LBDDSV,IBOUND(:,:,K))
        END IF
   79   CONTINUE
C
C6A-----SAVE DRAWDOWN FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,4).NE.0) THEN
          WRITE(IOUT,74) IDDNUN,KSTP,KPER
          IF(CDDNFM.EQ.' ') THEN
             CALL ULASAV(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NLAY,-1,IDDNUN)
          ELSE
             CALL ULASV2(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,IDDNUN,CDDNFM,LBDDSV,IBOUND)
          END IF
        END IF
      END IF
C
C7------RETURN.
   80 RETURN
      END
      SUBROUTINE SGWF2BAS7H(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     PRINT AND RECORD HEADS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,HNEW,BUFF,
     1                      IBOUND,IOUT
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IHEDFM,IHEDUN,LBHDSV,
     2                      CHEDFM,IOFLG
C
      CHARACTER*16 TEXT
      DATA TEXT /'            HEAD'/
C     ------------------------------------------------------------------
C
C1------FOR EACH LAYER MOVE HNEW TO BUFF IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C2------IS HEAD NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC.NE.0) KL=1
      IF(IOFLG(KL,1).EQ.0 .AND. IOFLG(KL,3).EQ.0) GO TO 59
C
C3------MOVE HNEW TO BUFF FOR THE LAYER.
      DO 58 I=1,NROW
      DO 58 J=1,NCOL
      BUFF(J,I,K)=HNEW(J,I,K)
   58 CONTINUE
   59 CONTINUE
C
C4------FOR EACH LAYER: DETERMINE IF HEAD SHOULD BE PRINTED.
C4------IF SO THEN CALL ULAPRS OR ULAPRW TO PRINT HEAD.
      IF(ISA.NE.0) THEN
         IF(IXSEC.EQ.0) THEN
           DO 69 K=1,NLAY
           KK=K
           IF(IOFLG(K,1).EQ.0) GO TO 69
           IF(IHEDFM.LT.0) CALL ULAPRS(BUFF(:,:,K),TEXT,KSTP,KPER,
     1               NCOL,NROW,KK,-IHEDFM,IOUT)
           IF(IHEDFM.GE.0) CALL ULAPRW(BUFF(:,:,K),TEXT,KSTP,KPER,
     1               NCOL,NROW,KK,IHEDFM,IOUT)
           IPFLG=1
   69      CONTINUE
C
C4A-----PRINT HEAD FOR CROSS SECTION.
         ELSE
           IF(IOFLG(1,1).NE.0) THEN
             IF(IHEDFM.LT.0) CALL ULAPRS(BUFF,TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,-IHEDFM,IOUT)
             IF(IHEDFM.GE.0) CALL ULAPRW(BUFF,TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,IHEDFM,IOUT)
             IPFLG=1
           END IF
         END IF
      END IF
C
C5------FOR EACH LAYER: DETERMINE IF HEAD SHOULD BE SAVED ON DISK.
C5------IF SO THEN CALL ULASAV OR ULASV2 TO SAVE HEAD.
      IFIRST=1
      IF(IHEDUN.LE.0) GO TO 80
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,3).EQ.0) GO TO 79
        IF(IFIRST.EQ.1) WRITE(IOUT,74) IHEDUN,KSTP,KPER
   74   FORMAT(1X,/1X,'HEAD WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
        IFIRST=0
        IF(CHEDFM.EQ.' ') THEN
           CALL ULASAV(BUFF(:,:,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IHEDUN)
        ELSE
           CALL ULASV2(BUFF(:,:,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IHEDUN,CHEDFM,LBHDSV,IBOUND(:,:,K))
        END IF
   79   CONTINUE
C
C5A-----SAVE HEAD FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,3).NE.0) THEN
          WRITE(IOUT,74) IHEDUN,KSTP,KPER
          IF(CHEDFM.EQ.' ') THEN
             CALL ULASAV(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NLAY,-1,IHEDUN)
          ELSE
             CALL ULASV2(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,IHEDUN,CHEDFM,LBHDSV,IBOUND)
          END IF
        END IF
      END IF
C
C6------RETURN.
   80 RETURN
      END
      SUBROUTINE SGWF2BAS7IB(KSTP,KPER)
C     ******************************************************************
C     RECORD IBOUND
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,IBOUND,IOUT
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IBOUUN,LBBOSV,CBOUFM,IOFLG
C
      CHARACTER*16 TEXT
      DATA TEXT /'          IBOUND'/
C     ------------------------------------------------------------------
      IF(IBOUUN.LE.0) RETURN
C
C5------FOR EACH LAYER: SAVE IBOUND WHEN REQUESTED.
      IFIRST=1
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,5).EQ.0) GO TO 79
        IF(IFIRST.EQ.1) WRITE(IOUT,74) IBOUUN,KSTP,KPER
   74   FORMAT(1X,/1X,'IBOUND WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
        IFIRST=0
        CALL ULASV3(IBOUND(:,:,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IBOUUN,CBOUFM,LBBOSV)
   79   CONTINUE
C
C5A-----SAVE IBOUND FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,5).NE.0) THEN
          WRITE(IOUT,74) IBOUUN,KSTP,KPER
          CALL ULASV3(IBOUND,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,IBOUUN,CBOUFM,LBBOSV)
        END IF
      END IF
C
C6------RETURN.
      RETURN
      END
      SUBROUTINE SGWF2BAS7I(NLAY,INOC,IOUT,IFREFM,NIUNIT)
C     ******************************************************************
C     SET UP OUTPUT CONTROL.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBASMODULE, ONLY: IHEDFM,IDDNFM,IHEDUN,IDDNUN,IPEROC,ITSOC,
     1                        CHEDFM,CDDNFM,IBDOPT,LBHDSV,LBDDSV,
     2                        IBOUUN,LBBOSV,CBOUFM,IAUXSV,IOFLG,
     3                        VBVL,VBNM,IDDREF,IDDREFNEW
      CHARACTER*5120 LINE
!      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1-----ALLOCATE SPACE FOR IOFLG, VBVL, AND VBNM ARRAYS.
      ALLOCATE (IOFLG(NLAY,5))
      ALLOCATE (VBVL(4,NIUNIT))
      ALLOCATE (VBNM(NIUNIT))
      IDDREF=0
      IDDREFNEW=0
C
C1------ASSIGN DEFAULT VALUES.
      CHEDFM=' '
      CDDNFM=' '
      CBOUFM='(20I4)'
      IHEDFM=0
      IDDNFM=0
      IHEDUN=0
      IDDNUN=0
      IBOUUN=0
      IBDOPT=1
      LBHDSV=0
      LBDDSV=0
      LBBOSV=0
      IAUXSV=0
C
C2------TEST OUTPUT CONTROL INPUT UNIT TO SEE IF OUTPUT CONTROL IS
C2------ACTIVE.
      IF(INOC.LE.0) THEN
C
C2A-----OUTPUT CONTROL IS INACTIVE. PRINT A MESSAGE LISTING DEFAULTS.
         WRITE(IOUT, 41)
   41    FORMAT(1X,/1X,'DEFAULT OUTPUT CONTROL',/1X,
     1   'THE FOLLOWING OUTPUT COMES AT THE END OF EACH STRESS PERIOD:')
         WRITE(IOUT, 42)
   42    FORMAT(1X,'TOTAL VOLUMETRIC BUDGET')
         WRITE(IOUT, 43)
   43    FORMAT(1X,10X,'HEAD')
C
C2B-----SET DEFAULT FLAGS IN IOFLG SO THAT HEAD IS PRINTED FOR
C2B-----EVERY LAYER.
         DO 80 K=1,NLAY
         IOFLG(K,1)=1
         IOFLG(K,2)=0
         IOFLG(K,3)=0
         IOFLG(K,4)=0
         IOFLG(K,5)=0
   80    CONTINUE
         GO TO 1000
      END IF
C
C3------OUTPUT CONTROL IS ACTIVE.  READ FIRST RECORD AND DECODE FIRST
C3------WORD.  MUST USE URWORD IN CASE FIRST WORD IS ALPHABETIC.
      CALL URDCOM(INOC,IOUT,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
C
C4------TEST FOR NUMERIC OUTPUT CONTROL.  FIRST WORD WILL NOT BE
C4------"PERIOD", "HEAD", "DRAWDOWN", OR "COMPACT".
      IF(LINE(ISTART:ISTOP).NE.'PERIOD' .AND. LINE(ISTART:ISTOP).NE.
     1     'HEAD' .AND. LINE(ISTART:ISTOP).NE.'DRAWDOWN' .AND.
     2     LINE(ISTART:ISTOP).NE.'COMPACT' .AND.
     3     LINE(ISTART:ISTOP).NE.'IBOUND') THEN
C4A-----NUMERIC OUTPUT CONTROL.  DECODE THE INITIAL RECORD ACCORDINGLY.
         WRITE(IOUT,102)
  102    FORMAT(1X,/1X,'OUTPUT CONTROL IS SPECIFIED EVERY TIME STEP')
         IF(IFREFM.EQ.0) THEN
            READ(LINE,'(4I10)') IHEDFM,IDDNFM,IHEDUN,IDDNUN
         ELSE
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDFM,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNFM,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDUN,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNUN,R,IOUT,INOC)
         END IF
         WRITE(IOUT,103) IHEDFM,IDDNFM
  103    FORMAT(1X,'HEAD PRINT FORMAT CODE IS',I4,
     1     '    DRAWDOWN PRINT FORMAT CODE IS',I4)
         WRITE(IOUT,104) IHEDUN,IDDNUN
  104    FORMAT(1X,'HEADS WILL BE SAVED ON UNIT ',I4,
     1     '    DRAWDOWNS WILL BE SAVED ON UNIT ',I4)
         IPEROC=-1
         ITSOC=-1
      ELSE
C4B-----ALPHABETIC OUTPUT CONTROL.  CALL MODULE TO READ INITIAL RECORDS.
         CALL SGWF2BAS7J(INOC,IOUT,LINE,LLOC,ISTART,ISTOP)
      END IF
C
C5------RETURN.
 1000 RETURN
      END
      SUBROUTINE SGWF2BAS7J(INOC,IOUT,LINE,LLOC,ISTART,ISTOP)
C     ******************************************************************
C     READ INITIAL ALPHABETIC OUTPUT CONTROL RECORDS.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBASMODULE, ONLY: IHEDFM,IDDNFM,IHEDUN,IDDNUN,IPEROC,ITSOC,
     1                        CHEDFM,CDDNFM,IBDOPT,LBHDSV,LBDDSV,
     2                        IBOUUN,LBBOSV,CBOUFM,IAUXSV,IDDREFNEW
C
      CHARACTER*(*) LINE
!      CHARACTER*1000 LINE
C     ------------------------------------------------------------------
C
C1------ALPHABETIC OUTPUT CONTROL.  WRITE MESSAGE AND SET INITIAL VALUES
C1------FOR IPEROC AND ITSOC.
      WRITE(IOUT,91)
   91 FORMAT(1X,/1X,'OUTPUT CONTROL IS SPECIFIED ONLY AT TIME STEPS',
     1    ' FOR WHICH OUTPUT IS DESIRED')
      IPEROC=9999
      ITSOC=9999
C
C2------LOOK FOR ALPHABETIC WORDS:
C2A-----LOOK FOR "PERIOD", WHICH INDICATES THE END OF INITIAL OUTPUT
C2A-----CONTROL DATA.  IF FOUND, DECODE THE PERIOD NUMBER AND TIME
C2A-----STEP NUMBER FOR LATER USE.
  100 IF(LINE(ISTART:ISTOP).EQ.'PERIOD') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPEROC,R,IOUT,INOC)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).NE.'STEP') GO TO 2000
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITSOC,R,IOUT,INOC)
         WRITE(IOUT,101) IHEDFM,IDDNFM
  101    FORMAT(1X,'HEAD PRINT FORMAT CODE IS',I4,
     1        '    DRAWDOWN PRINT FORMAT CODE IS',I4)
         WRITE(IOUT,102) IHEDUN,IDDNUN
  102    FORMAT(1X,'HEADS WILL BE SAVED ON UNIT ',I4,
     1        '    DRAWDOWNS WILL BE SAVED ON UNIT ',I4)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'DDREFERENCE') THEN
           IDDREFNEW=1
         ELSE
           IDDREFNEW=0
         END IF
         GO TO 1000
C
C2B-----LOOK FOR "HEAD PRINT ..." AND "HEAD SAVE ...".  IF
C2B-----FOUND, SET APPROPRIATE FLAGS.
      ELSE IF(LINE(ISTART:ISTOP).EQ.'HEAD') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).NE.'FORMAT') GO TO 2000
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDFM,R,IOUT,INOC)
         ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDUN,R,IOUT,
     1            INOC)
            ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
               CHEDFM=LINE(ISTART:ISTOP)
               WRITE(IOUT,103) CHEDFM
  103          FORMAT(1X,'HEADS WILL BE SAVED WITH FORMAT: ',A)
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
               IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                  LBHDSV=1
                  WRITE(IOUT,104)
  104             FORMAT(1X,'SAVED HEADS WILL BE LABELED')
               END IF
            ELSE
               GO TO 2000
            END IF
         ELSE
            GO TO 2000
         END IF
C
C2C-----LOOK FOR "DRAWDOWN PRINT ..." AND "DRAWDOWN SAVE ...".
C2C-----IF FOUND, SET APPROPRIATE FLAGS
      ELSE IF(LINE(ISTART:ISTOP).EQ.'DRAWDOWN') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).NE.'FORMAT') GO TO 2000
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNFM,R,IOUT,INOC)
         ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNUN,R,IOUT,
     1                   INOC)
            ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
               CDDNFM=LINE(ISTART:ISTOP)
               WRITE(IOUT,105) CDDNFM
  105          FORMAT(1X,'DRAWDOWN WILL BE SAVED WITH FORMAT: ',A)
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
               IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                  LBDDSV=1
                  WRITE(IOUT,106)
  106             FORMAT(1X,'SAVED DRAWDOWN WILL BE LABELED')
               END IF
            ELSE
               GO TO 2000
            END IF
         ELSE
            GO TO 2000
         END IF
C
C2D-----LOOK FOR "COMPACT BUDGET FILES" -- "COMPACT" IS SUFFICIENT.
C2D-----IF FOUND, SET APPROPRIATE FLAG.
      ELSE IF(LINE(ISTART:ISTOP).EQ.'COMPACT') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'BUDGET') THEN
            IBDOPT=2
            WRITE(IOUT,107)
  107       FORMAT(1X,
     1      'COMPACT CELL-BY-CELL BUDGET FILES WILL BE WRITTEN')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1         LINE(ISTART:ISTOP).EQ.'AUX') THEN
               IAUXSV=1
               WRITE(IOUT,108)
  108          FORMAT(1X,
     1     'AUXILIARY DATA WILL BE SAVED IN CELL-BY-CELL BUDGET FILES')
            END IF
         ELSE
            GO TO 2000
         END IF
C
C2E-----LOOK FOR  "IBOUND SAVE ...".  IF FOUND, SET APPROPRIATE FLAGS.
      ELSE IF(LINE(ISTART:ISTOP).EQ.'IBOUND') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IBOUUN,R,IOUT,
     1            INOC)
               WRITE(IOUT,111) IBOUUN
  111          FORMAT(1X,'IBOUND WILL BE SAVED ON UNIT ',I4)
            ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
               CBOUFM=LINE(ISTART:ISTOP)
               WRITE(IOUT,112) CBOUFM
  112          FORMAT(1X,'IBOUND WILL BE SAVED WITH FORMAT: ',A)
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
               IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                  LBBOSV=1
                  WRITE(IOUT,109)
  109             FORMAT(1X,'SAVED IBOUND WILL BE LABELED')
               END IF
            ELSE
               GO TO 2000
            END IF
         ELSE
            GO TO 2000
         END IF
C
C2F-----ERROR IF UNRECOGNIZED WORD.
      ELSE
         GO TO 2000
      END IF
C
C3------FINISHED READING A RECORD.  READ NEXT RECORD, IGNORING BLANK
C3------LINES.  GO BACK AND DECODE IT.
  110 READ(INOC,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GO TO 110
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
      GO TO 100
C
C4------RETURN.
 1000 RETURN
C
C5------ERROR DECODING INPUT DATA.
 2000 WRITE(IOUT,2001) LINE
 2001 FORMAT(1X,/1X,'ERROR READING OUTPUT CONTROL INPUT DATA:'/1X,A80)
      CALL USTOP(' ')
      END
      SUBROUTINE SGWF2BAS7T(KSTP,KPER,DELT,PERTIM,TOTIM,ITMUNI,IOUT)
C     ******************************************************************
C     PRINT SIMULATION TIME
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
      WRITE(IOUT,199) KSTP,KPER
  199 FORMAT(1X,///10X,'TIME SUMMARY AT END OF TIME STEP ',I3,
     1     ' IN STRESS PERIOD ',I4)
C
C1------USE TIME UNIT INDICATOR TO GET FACTOR TO CONVERT TO SECONDS.
      ZERO=0.
      CNV=ZERO
      IF(ITMUNI.EQ.1) CNV=1.
      IF(ITMUNI.EQ.2) CNV=60.
      IF(ITMUNI.EQ.3) CNV=3600.
      IF(ITMUNI.EQ.4) CNV=86400.
      IF(ITMUNI.EQ.5) CNV=31557600.
C
C2------IF FACTOR=0 THEN TIME UNITS ARE NON-STANDARD.
      IF(CNV.NE.ZERO) GO TO 100
C
C2A-----PRINT TIMES IN NON-STANDARD TIME UNITS.
      WRITE(IOUT,301) DELT,PERTIM,TOTIM
  301 FORMAT(21X,'     TIME STEP LENGTH =',G15.6/
     1       21X,'   STRESS PERIOD TIME =',G15.6/
     2       21X,'TOTAL SIMULATION TIME =',G15.6)
C
C2B-----RETURN
      RETURN
C
C3------CALCULATE LENGTH OF TIME STEP & ELAPSED TIMES IN SECONDS.
  100 DELSEC=CNV*DELT
      TOTSEC=CNV*TOTIM
      PERSEC=CNV*PERTIM
C
C4------CALCULATE TIMES IN MINUTES,HOURS,DAYS AND YEARS.
      SIXTY=60.
      HRDAY=24.
      DAYYR=365.25
      DELMN=DELSEC/SIXTY
      DELHR=DELMN/SIXTY
      DELDY=DELHR/HRDAY
      DELYR=DELDY/DAYYR
      TOTMN=TOTSEC/SIXTY
      TOTHR=TOTMN/SIXTY
      TOTDY=TOTHR/HRDAY
      TOTYR=TOTDY/DAYYR
      PERMN=PERSEC/SIXTY
      PERHR=PERMN/SIXTY
      PERDY=PERHR/HRDAY
      PERYR=PERDY/DAYYR
C
C5------PRINT TIME STEP LENGTH AND ELAPSED TIMES IN ALL TIME UNITS.
      WRITE(IOUT,200)
  200 FORMAT(19X,' SECONDS     MINUTES      HOURS',7X,
     1    'DAYS        YEARS'/20X,59('-'))
      WRITE (IOUT,201) DELSEC,DELMN,DELHR,DELDY,DELYR
  201 FORMAT(1X,'  TIME STEP LENGTH',1P,5G12.5)
      WRITE(IOUT,202) PERSEC,PERMN,PERHR,PERDY,PERYR
  202 FORMAT(1X,'STRESS PERIOD TIME',1P,5G12.5)
      WRITE(IOUT,203) TOTSEC,TOTMN,TOTHR,TOTDY,TOTYR
  203 FORMAT(1X,'        TOTAL TIME',1P,5G12.5)
C
C6------RETURN
      RETURN
      END
      SUBROUTINE SGWF2BAS7V(MSUM,VBNM,VBVL,KSTP,KPER,IOUT,BUDPERC)
C     ******************************************************************
C     PRINT VOLUMETRIC BUDGET
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*16 VBNM(MSUM)
      DIMENSION VBVL(4,MSUM)
      CHARACTER*17 VAL1,VAL2
C     ------------------------------------------------------------------
C
C1------DETERMINE NUMBER OF INDIVIDUAL BUDGET ENTRIES.
      BUDPERC=0.
      MSUM1=MSUM-1
      IF(MSUM1.LE.0) RETURN
C
C2------CLEAR RATE AND VOLUME ACCUMULATORS.
      ZERO=0.
      TWO=2.
      HUND=100.
      BIGVL1=9.99999E11
      BIGVL2=9.99999E10
      SMALL=0.1
      TOTRIN=ZERO
      TOTROT=ZERO
      TOTVIN=ZERO
      TOTVOT=ZERO
C
C3------ADD RATES AND VOLUMES (IN AND OUT) TO ACCUMULATORS.
      DO 100 L=1,MSUM1
      TOTRIN=TOTRIN+VBVL(3,L)
      TOTROT=TOTROT+VBVL(4,L)
      TOTVIN=TOTVIN+VBVL(1,L)
      TOTVOT=TOTVOT+VBVL(2,L)
  100 CONTINUE
C
C4------PRINT TIME STEP NUMBER AND STRESS PERIOD NUMBER.
      WRITE(IOUT,260) KSTP,KPER
      WRITE(IOUT,265)
C
C5------PRINT INDIVIDUAL INFLOW RATES AND VOLUMES AND THEIR TOTALS.
      DO 200 L=1,MSUM1
      IF(VBVL(1,L).NE.ZERO .AND.
     1       (VBVL(1,L).GE.BIGVL1 .OR. VBVL(1,L).LT.SMALL)) THEN
         WRITE(VAL1,'(1PE17.4)') VBVL(1,L)
      ELSE
         WRITE(VAL1,'(F17.4)') VBVL(1,L)
      END IF
      IF(VBVL(3,L).NE.ZERO .AND.
     1       (VBVL(3,L).GE.BIGVL1 .OR. VBVL(3,L).LT.SMALL)) THEN
         WRITE(VAL2,'(1PE17.4)') VBVL(3,L)
      ELSE
         WRITE(VAL2,'(F17.4)') VBVL(3,L)
      END IF
      WRITE(IOUT,275) VBNM(L),VAL1,VBNM(L),VAL2
  200 CONTINUE
      IF(TOTVIN.NE.ZERO .AND.
     1      (TOTVIN.GE.BIGVL1 .OR. TOTVIN.LT.SMALL)) THEN
         WRITE(VAL1,'(1PE17.4)') TOTVIN
      ELSE
         WRITE(VAL1,'(F17.4)') TOTVIN
      END IF
      IF(TOTRIN.NE.ZERO .AND.
     1      (TOTRIN.GE.BIGVL1 .OR. TOTRIN.LT.SMALL)) THEN
         WRITE(VAL2,'(1PE17.4)') TOTRIN
      ELSE
         WRITE(VAL2,'(F17.4)') TOTRIN
      END IF
      WRITE(IOUT,286) VAL1,VAL2
C
C6------PRINT INDIVIDUAL OUTFLOW RATES AND VOLUMES AND THEIR TOTALS.
      WRITE(IOUT,287)
      DO 250 L=1,MSUM1
      IF(VBVL(2,L).NE.ZERO .AND.
     1       (VBVL(2,L).GE.BIGVL1 .OR. VBVL(2,L).LT.SMALL)) THEN
         WRITE(VAL1,'(1PE17.4)') VBVL(2,L)
      ELSE
         WRITE(VAL1,'(F17.4)') VBVL(2,L)
      END IF
      IF(VBVL(4,L).NE.ZERO .AND.
     1       (VBVL(4,L).GE.BIGVL1 .OR. VBVL(4,L).LT.SMALL)) THEN
         WRITE(VAL2,'(1PE17.4)') VBVL(4,L)
      ELSE
         WRITE(VAL2,'(F17.4)') VBVL(4,L)
      END IF
      WRITE(IOUT,275) VBNM(L),VAL1,VBNM(L),VAL2
  250 CONTINUE
      IF(TOTVOT.NE.ZERO .AND.
     1      (TOTVOT.GE.BIGVL1 .OR. TOTVOT.LT.SMALL)) THEN
         WRITE(VAL1,'(1PE17.4)') TOTVOT
      ELSE
         WRITE(VAL1,'(F17.4)') TOTVOT
      END IF
      IF(TOTROT.NE.ZERO .AND.
     1      (TOTROT.GE.BIGVL1 .OR. TOTROT.LT.SMALL)) THEN
         WRITE(VAL2,'(1PE17.4)') TOTROT
      ELSE
         WRITE(VAL2,'(F17.4)') TOTROT
      END IF
      WRITE(IOUT,298) VAL1,VAL2
C
C7------CALCULATE THE DIFFERENCE BETWEEN INFLOW AND OUTFLOW.
C
C7A-----CALCULATE DIFFERENCE BETWEEN RATE IN AND RATE OUT.
      DIFFR=TOTRIN-TOTROT
      ADIFFR=ABS(DIFFR)
C
C7B-----CALCULATE PERCENT DIFFERENCE BETWEEN RATE IN AND RATE OUT.
      PDIFFR=ZERO
      AVGRAT=(TOTRIN+TOTROT)/TWO
      IF(AVGRAT.NE.ZERO) PDIFFR=HUND*DIFFR/AVGRAT
      BUDPERC=PDIFFR
C
C7C-----CALCULATE DIFFERENCE BETWEEN VOLUME IN AND VOLUME OUT.
      DIFFV=TOTVIN-TOTVOT
      ADIFFV=ABS(DIFFV)
C
C7D-----GET PERCENT DIFFERENCE BETWEEN VOLUME IN AND VOLUME OUT.
      PDIFFV=ZERO
      AVGVOL=(TOTVIN+TOTVOT)/TWO
      IF(AVGVOL.NE.ZERO) PDIFFV=HUND*DIFFV/AVGVOL
C
C8------PRINT DIFFERENCES AND PERCENT DIFFERENCES BETWEEN INPUT
C8------AND OUTPUT RATES AND VOLUMES.
      IF(ADIFFV.NE.ZERO .AND.
     1      (ADIFFV.GE.BIGVL2 .OR. ADIFFV.LT.SMALL)) THEN
         WRITE(VAL1,'(1PE17.4)') DIFFV
      ELSE
         WRITE(VAL1,'(F17.4)') DIFFV
      END IF
      IF(ADIFFR.NE.ZERO .AND.
     1      (ADIFFR.GE.BIGVL2 .OR. ADIFFR.LT.SMALL)) THEN
         WRITE(VAL2,'(1PE17.4)') DIFFR
      ELSE
         WRITE(VAL2,'(F17.4)') DIFFR
      END IF
      WRITE(IOUT,299) VAL1,VAL2
      WRITE(IOUT,300) PDIFFV,PDIFFR
C
C9------RETURN.
      RETURN
C
C    ---FORMATS
C
  260 FORMAT('1',/2X,'VOLUMETRIC BUDGET FOR ENTIRE MODEL AT END OF'
     1,' TIME STEP',I3,' IN STRESS PERIOD',I4/2X,78('-'))
  265 FORMAT(1X,/5X,'CUMULATIVE VOLUMES',6X,'L**3',7X
     1,'RATES FOR THIS TIME STEP',6X,'L**3/T'/5X,18('-'),17X,24('-')
     2//11X,'IN:',38X,'IN:'/11X,'---',38X,'---')
  275 FORMAT(1X,3X,A16,' =',A17,6X,A16,' =',A17)
  286 FORMAT(1X,/12X,'TOTAL IN =',A,14X,'TOTAL IN =',A)
  287 FORMAT(1X,/10X,'OUT:',37X,'OUT:'/10X,4('-'),37X,4('-'))
  298 FORMAT(1X,/11X,'TOTAL OUT =',A,13X,'TOTAL OUT =',A)
  299 FORMAT(1X,/12X,'IN - OUT =',A,14X,'IN - OUT =',A)
  300 FORMAT(1X,/1X,'PERCENT DISCREPANCY =',F15.2
     1,5X,'PERCENT DISCREPANCY =',F15.2,///)
C
      END
      SUBROUTINE SGWF2BAS7N(KPER,KSTP,INOC,IOUT,NLAY)
C     ******************************************************************
C     SET OUTPUT FLAGS USING ALPHABETIC OUTPUT CONTROL INPUT STRUCTURE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBASMODULE, ONLY: IOFLG,IHDDFL,IBUDFL,ICBCFL,IPEROC,
     1                        ITSOC,IBDOPT,IDDREF,IDDREFNEW
C
      CHARACTER*5120 LINE
!      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------ERROR IF OUTPUT CONTROL TIME STEP PRECEDES CURRENT SIMULATION
C1------TIME STEP.
      IF((IPEROC.LT.KPER).OR.(IPEROC.EQ.KPER .AND. ITSOC.LT.KSTP)) THEN
         WRITE(IOUT,5) IPEROC,ITSOC,KPER,KSTP
    5    FORMAT(1X,/1X,'OUTPUT CONTROL WAS SPECIFIED FOR A NONEXISTENT',
     1   ' TIME STEP',/
     2   1X,'OR OUTPUT CONTROL DATA ARE NOT ENTERED IN ASCENDING ORDER',
     3   /1X,'OUTPUT CONTROL STRESS PERIOD ',I4,'   TIME STEP ',I3,/
     4   1X,'MODEL STRESS PERIOD ',I4,'   TIME STEP ',I3,/
     5   1X,'APPLYING THE SPECIFIED OUTPUT CONTROL TO THE CURRENT TIME',
     6   ' STEP')
         IPEROC=KPER
         ITSOC=KSTP
      END IF
C
C2------CLEAR I/O FLAGS.
      IHDDFL=0
      IBUDFL=0
      ICBCFL=0
      DO 10 I=1,5
      DO 10 K=1,NLAY
      IOFLG(K,I)=0
10    CONTINUE
C
C3------IF OUTPUT CONTROL TIME STEP DOES NOT MATCH SIMULATION TIME STEP,
C3------WRITE MESSAGE THAT THERE IS NO OUTPUT CONTROL THIS TIME STEP,
C3------AND RETURN.
      IF(IPEROC.NE.KPER .OR. ITSOC.NE.KSTP) THEN
         WRITE(IOUT,11) KPER,KSTP
11       FORMAT(1X,/1X,'NO OUTPUT CONTROL FOR STRESS PERIOD ',I4,
     1              '   TIME STEP ',I3)
         RETURN
      END IF
C
C4------OUTPUT CONTROL TIME STEP MATCHES SIMULATION TIME STEP.
      IDDREF=IDDREFNEW
      WRITE(IOUT,12) IPEROC,ITSOC
12    FORMAT(1X,/1X,'OUTPUT CONTROL FOR STRESS PERIOD ',I4,
     1              '   TIME STEP ',I3)
      IF(IDDREFNEW.NE.0) WRITE(IOUT,52)
   52      FORMAT(1X,'Drawdown Reference will be reset at the',
     1               ' end of this time step')
C
C4A-----OUTPUT CONTROL MATCHES SIMULATION TIME.  READ NEXT OUTPUT
C4A-----RECORD; SKIP ANY BLANK LINES.
50    READ(INOC,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GO TO 50
C
C4A1----LOOK FOR "PERIOD", WHICH TERMINATES OUTPUT CONTROL FOR CURRENT
C4A1----TIME STEP.  IF FOUND, DECODE TIME STEP FOR NEXT OUTPUT.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
      IF(LINE(ISTART:ISTOP).EQ.'PERIOD') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPEROC,R,IOUT,INOC)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).NE.'STEP') GO TO 2000
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITSOC,R,IOUT,INOC)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'DDREFERENCE') THEN
           IDDREFNEW=1
         ELSE
           IDDREFNEW=0
         END IF
         RETURN
C
C4A2----LOOK FOR "PRINT", WHICH MAY REFER TO "BUDGET", "HEAD", OR
C4A2----"DRAWDOWN".
      ELSE IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'BUDGET') THEN
            WRITE(IOUT,53)
53          FORMAT(4X,'PRINT BUDGET')
            IBUDFL=1
         ELSE IF(LINE(ISTART:ISTOP).EQ.'HEAD') THEN
            CALL SGWF2BAS7L(1,LINE,LLOC,IOFLG,NLAY,IOUT,'PRINT HEAD',
     1              INOC)
            IHDDFL=1
         ELSE IF(LINE(ISTART:ISTOP).EQ.'DRAWDOWN') THEN
            CALL SGWF2BAS7L(2,LINE,LLOC,IOFLG,NLAY,IOUT,
     1              'PRINT DRAWDOWN',INOC)
            IHDDFL=1
         ELSE
            GO TO 2000
         END IF
C
C4A3----LOOK FOR "SAVE", WHICH MAY REFER TO "BUDGET", "HEAD",
C4A3----"DRAWDOWN", OR "IBOUND".
      ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'BUDGET') THEN
            call splitbudget(ipos,line,lloc,nlay,iout,label,inoc,1)     ! DLT
            WRITE(IOUT,57)
57          FORMAT(4X,'SAVE BUDGET')
            ICBCFL=IBDOPT
         ELSE IF(LINE(ISTART:ISTOP).EQ.'HEAD') THEN
            call splitbudget(ipos,line,lloc,nlay,iout,label,inoc,2)     ! DLT
            CALL SGWF2BAS7L(3,LINE,LLOC,IOFLG,NLAY,IOUT,'SAVE HEAD',
     &                      INOC)
            IHDDFL=1
         ELSE IF(LINE(ISTART:ISTOP).EQ.'DRAWDOWN') THEN
            CALL SGWF2BAS7L(4,LINE,LLOC,IOFLG,NLAY,IOUT,'SAVE DRAWDOWN',
     1          INOC)
            IHDDFL=1
         ELSE IF(LINE(ISTART:ISTOP).EQ.'IBOUND') THEN
            CALL SGWF2BAS7L(5,LINE,LLOC,IOFLG,NLAY,IOUT,'SAVE IBOUND',
     1                     INOC)
            IHDDFL=1
         ELSE
            GO TO 2000
         END IF
C
C4A4----WHEN NO KNOWN ALPHABETIC WORDS ARE FOUND, THERE IS AN ERROR.
      ELSE
         GO TO 2000
C
C4B-----AFTER SUCCESSFULLY DECODING ONE RECORD, READ ANOTHER.
      END IF
      GO TO 50
C
C5------END OF FILE WHILE READING AN OUTPUT CONTROL RECORD, SO THERE
C5------WILL BE NO FURTHER OUTPUT.  SET IPEROC AND ITSOC HIGH ENOUGH
C5------THAT THE MODEL TIME WILL NEVER MATCH THEM.
1000  IPEROC=9999
      ITSOC=9999
      RETURN
C
C6------ERROR DECODING ALPHABETIC INPUT STRUCTURE.
2000  WRITE(IOUT,2001) LINE
2001  FORMAT(1X,/1X,'ERROR READING OUTPUT CONTROL INPUT DATA:'/1X,A80)
      CALL USTOP(' ')
      END
      SUBROUTINE SGWF2BAS7L(IPOS,LINE,LLOC,IOFLG,NLAY,IOUT,LABEL,INOC)
C     ******************************************************************
C     WHEN USING ALPHABETIC OUTPUT CONTROL, DECODE LAYER
C     NUMBERS FOR PRINTING OR SAVING HEAD OR DRAWDOWN
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION IOFLG(NLAY,5)
      CHARACTER(LEN=*) LINE
      CHARACTER*(*) LABEL
      DIMENSION LAYER(999)
C     ------------------------------------------------------------------
C
C1------INITIALIZE COUNTER FOR NUMBER OF LAYERS FOR WHICH OUTPUT IS
C1------SPECIFIED.
      NSET=0
C
C2------CHECK FOR A VALID LAYER NUMBER.  WHEN FOUND, SET FLAG AND
C2------REPEAT.
10    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,L,R,-1,INOC)
      IF(L.GT.0 .AND. L.LE.NLAY) THEN
         NSET=NSET+1
         LAYER(NSET)=L
         IOFLG(L,IPOS)=1
         GO TO 10
      END IF
C
C3------DONE CHECKING FOR LAYER NUMBERS.  IF NO LAYER NUMBERS WERE
C3------FOUND, SET FLAGS FOR ALL LAYERS.
      IF(NSET.EQ.0) THEN
         DO 110 K=1,NLAY
         IOFLG(K,IPOS)=1
110      CONTINUE
         WRITE(IOUT,111) LABEL
111      FORMAT(4X,A,' FOR ALL LAYERS')
C
C4------IF ONE OR MORE LAYER NUMBERS WERE FOUND, PRINT THE NUMBERS.
      ELSE
         WRITE(IOUT,112) LABEL,(LAYER(M),M=1,NSET)
112      FORMAT(4X,A,' FOR LAYERS:',(1X,15I3))
      END IF
C
C5------RETURN.
      RETURN
      END
      SUBROUTINE SGWF2BAS7OPEN(INUNIT,IOUT,IUNIT,CUNIT,
     1              NIUNIT,VERSION,INBAS,MAXUNIT,MFVNAM)
C     ******************************************************************
C     OPEN FILES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      use imod_utl, only : hdr, lic
      INCLUDE 'openspec.inc'
      DIMENSION IUNIT(NIUNIT)
      CHARACTER*4 CUNIT(NIUNIT)
      CHARACTER*7 FILSTAT
      CHARACTER*20 FILACT, FMTARG, ACCARG
      CHARACTER*(*) VERSION,MFVNAM
      CHARACTER*40 SPACES
      CHARACTER*300 LINE, FNAME, tmpname
      CHARACTER*20 FILTYP
      LOGICAL LOP
      CHARACTER*300 filename                                            ! DLT
      logical split                                                     ! DLT
      real    maxfs                                                     ! DLT
C     ---------------------------------------------------------------
C
C1------INITIALIZE CONSTANTS.
      INBAS=0
      NFILE=0
      IOUT=0
      DO 5 I=1,NIUNIT
      IUNIT(I)=0
5     CONTINUE
      SPACES=' '
      LENVER=LEN_TRIM(VERSION)
      INDENT=40-(LENVER+8)/2
C
C2------READ A LINE; IGNORE BLANK LINES AND PRINT COMMENT LINES.
10    READ(INUNIT,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GO TO 10
      IF(LINE(1:1).EQ.'#') THEN
        IF(NFILE.NE.0 .AND. IOUT.NE.0) WRITE(IOUT,'(A)') LINE
        GO TO 10
      END IF
C
C3------DECODE THE FILE TYPE, UNIT NUMBER, AND NAME.
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,IOUT,INUNIT)
      FILTYP=LINE(ITYP1:ITYP2)
      if (filtyp.eq.'SPLITFILE') then                                   ! DLT
         split = .true.                                                 ! DLT
         CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,IOUT,INUNIT)           ! DLT
         FILTYP=LINE(ITYP1:ITYP2)                                       ! DLT
         read(filtyp,*) maxfs                                           ! DLT
         CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,IOUT,INUNIT)           ! DLT
         FILTYP=LINE(ITYP1:ITYP2)                                       ! DLT
      else                                                              ! DLT
         split = .false.                                                ! DLT
      end if                                                            ! DLT
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INUNIT)
      CALL URWORD(LINE,LLOC,INAM1,INAM2,0,N,R,IOUT,INUNIT)
      IFLEN=INAM2-INAM1+1
      FNAME(1:IFLEN)=LINE(INAM1:INAM2)
      INQUIRE(UNIT=IU,OPENED=LOP)

      IF(LOP) THEN
         IF(IOUT.EQ.0) THEN
            WRITE(*,11) FNAME(1:IFLEN),IU
   11       FORMAT(1X,/1X,'CANNOT OPEN ',A,' ON UNIT',I4,
     1              ' BECAUSE UNIT IS ALREADY BEING USED')
         ELSE
            WRITE(IOUT,11) FNAME(1:IFLEN),IU
         END IF
         CALL USTOP(' ')
      END IF
C
C4------KEEP TRACK OF LARGEST UNIT NUMBER
      IF (IU.GT.MAXUNIT) MAXUNIT = IU
C
C5------SET DEFAULT FILE ATTRIBUTES.
      FMTARG='FORMATTED'
      ACCARG='SEQUENTIAL'
      FILSTAT='UNKNOWN'
      FILACT=' '
C
C6------SPECIAL CHECK FOR 1ST FILE.
      IF(NFILE.EQ.0) THEN
        IF(FILTYP.EQ.'LIST') THEN
          filename = FNAME(1:IFLEN)                                     ! DLT
          if (split) then                                               ! DLT
            fmtarg = 'FORMATTED'                                        ! DLT
            accarg = 'SEQUENTIAL'                                       ! DLT
            filstat = 'REPLACE'                                         ! DLT
            filact = ' '                                                ! DLT
            call fsplitinit(IU,filename,fmtarg,                         ! DLT
     1                     accarg,filstat,filact,maxfs,'size')          ! DLT
          end if                                                        ! DLT
          IOUT=IU
          CALL PKS7MPIFNAME(filename,IFLEN)                             ! PKS
          OPEN(UNIT=IU,FILE=filename,STATUS='REPLACE',
     1          FORM='FORMATTED',ACCESS='SEQUENTIAL')
          call sts2subscribe(IU)                                        ! DLT: subscribe unit number for save/restore

          !## write banner in logfile
          do i = 1, size(hdr)
           write(iout,'(a)') trim(hdr(i))
          end do

          WRITE(IOUT,60) MFVNAM,SPACES(1:INDENT),VERSION(1:LENVER)
60        FORMAT(34X,'MODFLOW',A,/,
     &             6X,'U.S. GEOLOGICAL SURVEY MODULAR',
     &             ' FINITE-DIFFERENCE GROUND-WATER FLOW MODEL',/,
     &             A,'VERSION ',A,/)
          WRITE(IOUT,78) FNAME(1:IFLEN),IOUT
78        FORMAT(1X,'LIST FILE: ',A,/25X,'UNIT ',I4)
        ELSE
          WRITE(*,*)
     1       ' FIRST ENTRY IN NAME FILE MUST BE "LIST".'
          CALL USTOP(' ')
        END IF
C  Get next file name
        NFILE=1
        GO TO 10
      END IF
C
C8------CHECK FOR "BAS" FILE TYPE.
      IF(FILTYP.EQ.'BAS6') THEN
         INBAS=IU
         FILSTAT='OLD    '
         FILACT=ACTION(1)
C
C9------CHECK FOR "UNFORMATTED" FILE TYPE.
      ELSE IF(FILTYP.EQ.'DATA(BINARY)' .OR.
     1        FILTYP.EQ.'DATAGLO(BINARY)'.OR.                           ! DLT
     1        filtyp.eq.'DATA(BINARYIDF)'.OR.                           ! DLT
     1        filtyp.eq.'DATA(BINARYNC)') THEN                          ! DLT
         FMTARG=FORM
         ACCARG=ACCESS
C
C10-----CHECK FOR "FORMATTED" FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'DATA' .OR.
     1        LINE(ITYP1:ITYP2).EQ.'DATAGLO') THEN
         FMTARG='FORMATTED'
         ACCARG='SEQUENTIAL'
C
C11-----CHECK FOR MAJOR OPTIONS.
      ELSE
        DO 20 I=1,NIUNIT
           IF(LINE(ITYP1:ITYP2).EQ.CUNIT(I)) THEN
              IUNIT(I)=IU
              FILSTAT='OLD    '
              FILACT=ACTION(1)
              GO TO 30
           END IF
20      CONTINUE
        WRITE(IOUT,21) LINE(ITYP1:ITYP2)
21      FORMAT(1X,'ILLEGAL FILE TYPE IN NAME FILE: ',A)
        CALL USTOP(' ')
30      CONTINUE
      END IF
C
C12-----FOR DATA FILES, CHECK FOR "REPLACE" OR "OLD" OPTION
      IF (FILSTAT.EQ.'UNKNOWN') THEN
        CALL URWORD(LINE,LLOC,IOPT1,IOPT2,1,N,R,IOUT,INUNIT)
        IF (LINE(IOPT1:IOPT2).EQ.'REPLACE' .OR.
     &      LINE(IOPT1:IOPT2).EQ.'OLD')
     &      FILSTAT = LINE(IOPT1:IOPT2)
      ENDIF
      IF (FILACT.EQ.' ') FILACT=ACTION(2)
C
      filename = FNAME(1:IFLEN)                                         ! DLT
      if (split.and.filtyp(1:4).eq.'DATA') then                         ! DLT
         call fsplitinit(IU,filename,FMTARG,                            ! DLT
     1                  ACCARG,FILSTAT,FILACT,maxfs,'size')             ! DLT
      end if                                                            ! DLT
      if (filtyp.eq.'DATA(BINARYIDF)') then                             ! DLT
         call fsplitinit(IU,filename,FMTARG,                            ! DLT
     1                  ACCARG,FILSTAT,FILACT,0.,'idf')                 ! DLT
         nfile=nfile+1                                                  ! DLT
         go to 10                                                       ! DLT
      end if                                                            ! DLT
      if (filtyp.eq.'DATA(BINARYNC)') then                              ! DLT
         call fsplitinit(IU,filename,FMTARG,                            ! DLT
     1                  ACCARG,FILSTAT,FILACT,0.,'nc')                  ! DLT
         nfile=nfile+1                                                  ! DLT
         go to 10                                                       ! DLT
      end if                                                            ! DLT

C13-----WRITE THE FILE NAME AND OPEN IT.
      WRITE(IOUT,50) filename,
     1     LINE(ITYP1:ITYP2),IU,FILSTAT,FMTARG,ACCARG
50    FORMAT(1X,/1X,'OPENING ',A,/
     &  1X,'FILE TYPE:',A,'   UNIT ',I4,3X,'STATUS:',A,/
     &  1X,'FORMAT:',A,3X,'ACCESS:',A)
#ifdef __INTEL_COMPILER
      OPEN(UNIT=IU,FILE=filename,FORM=FMTARG,                           ! PKS
     1      ACCESS=ACCARG,STATUS=FILSTAT,ACTION=FILACT,                 ! PKS
     2      SHARE='DENYNONE',ERR=2000)                                  ! PKS
#else
      OPEN(UNIT=IU,FILE=filename,FORM=FMTARG,
     1      ACCESS=ACCARG,STATUS=FILSTAT,ACTION=FILACT,ERR=2000)
#endif      
          call sts2subscribe(IU)                                        ! DLT: subscribe unit number for save/restore
      NFILE=NFILE+1
      IF (FILTYP.EQ.'PKS') CALL PKS7MPIINI2( IU, IOUT )                 ! PKS
      GO TO 10
C
C14-----END OF NAME FILE.  RETURN PROVIDED THAT LISTING FILE AND BAS
C14-----FILES HAVE BEEN OPENED.
1000  IF(NFILE.EQ.0) THEN
         WRITE(*,*) ' NAME FILE IS EMPTY.'
         CALL USTOP(' ')
      ELSE IF(INBAS.EQ.0) THEN
         WRITE(IOUT,*) ' BAS PACKAGE FILE HAS NOT BEEN OPENED.'
         CALL USTOP(' ')
      END IF
      CLOSE (UNIT=INUNIT)
C
      RETURN
C
C15-----ERROR OPENING FILE.
 2000 CONTINUE
      WRITE(*,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
      WRITE(IOUT,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
 2010 FORMAT(/,1X,'*** ERROR OPENING FILE "',A,'" ON UNIT ',I5,/,
     &7X,'SPECIFIED FILE STATUS: ',A,/
     &7X,'SPECIFIED FILE FORMAT: ',A,/
     &7X,'SPECIFIED FILE ACCESS: ',A,/
     &7X,'SPECIFIED FILE ACTION: ',A,/
     &2X,'-- STOP EXECUTION (SGWF2BAS7OPEN)')
      CALL USTOP(' ')
C
      END
      SUBROUTINE SGWF2BAS7ARMZ(INZONE,INMULT)
C     ******************************************************************
C     ALLOCATE AND READ MULTIPLIER AND ZONE ARRAYS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:NCOL,NROW,IOUT
      USE PARAMMODULE,ONLY:NZONAR,NMLTAR,ZONNAM,MLTNAM,IZON,RMLT
C
      CHARACTER*20 RW
      CHARACTER*1 COP
      CHARACTER*24 ANAME
      CHARACTER*10 CTMP1,CTMP2
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------Read Number of Zone Arrays if Zone Option is active.
      NZONAR=0
      IF(INZONE.GT.0) THEN
         WRITE(IOUT,1) INZONE
    1    FORMAT(1X,/1X,'ZONE OPTION, INPUT READ FROM UNIT ',I4)
         CALL URDCOM(INZONE,IOUT,LINE)
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NZONAR,R,IOUT,INZONE)
         WRITE(IOUT,2) NZONAR
    2    FORMAT(1X,I5,' ZONE ARRAYS')
         IF(NZONAR.LT.0) NZONAR=0
      END IF
C
C2------Allocate memory for zone arrays.  Allocate one array element if
C2------there are no zone arrays.
      IF(NZONAR.GT.0) THEN
        ALLOCATE (ZONNAM(NZONAR))
        ALLOCATE (IZON(NCOL,NROW,NZONAR))
      ELSE
        ALLOCATE (ZONNAM(1))
        ALLOCATE (IZON(1,1,1))
      ENDIF
C
C3------Read Number of Multiplier Arrays if Multiplier Option is active.
      NMLTAR=0
      IF(INMULT.GT.0) THEN
         WRITE(IOUT,11) INMULT
   11    FORMAT(1X,/1X,'MULTIPLIER OPTION, INPUT READ FROM UNIT ',I4)
         CALL URDCOM(INMULT,IOUT,LINE)
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NMLTAR,R,IOUT,INMULT)
         WRITE(IOUT,12) NMLTAR
   12    FORMAT(1X,I3,' MULTIPLIER ARRAYS')
         IF(NMLTAR.LT.0) NMLTAR=0
      END IF
C
C4------Allocate memory for multiplier arrays.  Allocate one array element if
C4------there are no multiplier arrays.
      IF(NMLTAR.GT.0) THEN
        ALLOCATE (MLTNAM(NMLTAR))
        ALLOCATE (RMLT(NCOL,NROW,NMLTAR))
      ELSE
        ALLOCATE (MLTNAM(1))
        ALLOCATE (RMLT(1,1,1))
      ENDIF
C
C5------Initialize names of zones, multipliers, and parameters.
      IF(NZONAR.GT.0) THEN
        DO 10 I=1,NZONAR
        ZONNAM(I)=' '
10      CONTINUE
      END IF
      IF(NMLTAR.GT.0) THEN
        DO 20 I=1,NMLTAR
        MLTNAM(I)=' '
20      CONTINUE
      END IF
C
C6------Define the multiplier arrays.
      IF(NMLTAR.GT.0) THEN
        DO 2000 M=1,NMLTAR
C
C6A-----Read a line describing a multiplier array.
          READ (INMULT,'(A)') LINE
C
C6B-----Get the name of the new array
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
C
C6C-----Add new multiplier name into list.
          MLTNAM(M)=LINE(ISTART:ISTOP)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INMULT)
          IF(LINE(ISTART:ISTOP).NE.'FUNCTION') THEN
C
C6D-----Define array using array reader.
             ANAME=' MULT. ARRAY: '//MLTNAM(M)
             CALL U2DREL(RMLT(:,:,M),ANAME,NROW,NCOL,0,INMULT,IOUT)
          ELSE
C
C6E-----Define array as aritmetic combination of other multiplier arrays.
C6E-----Start by initializing the array to 0.
             WRITE(IOUT,30) MLTNAM(M)
   30        FORMAT(1X,/1X,'Calculated multiplier array: ',A)
             DO 40 I=1,NROW
             DO 40 J=1,NCOL
             RMLT(J,I,M)=0.
   40        CONTINUE
C
C6E1----Get the names of the multipliers and the operands.
             READ (INMULT,'(A)') LINE
             LLOC=1
             NOP=0
C
C6E2----Get the operator.
   45        IF(NOP.EQ.0) THEN
C
C6E2A---No operator is specified before the first operand -- define it to be " "
                COP=' '
             ELSE
C
C6E2B---Get the operator that precedes each operand after the first operand.
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
                IF(LINE(ISTART:ISTOP).EQ.'+' .OR.
     1             LINE(ISTART:ISTOP).EQ.'-' .OR.
     2             LINE(ISTART:ISTOP).EQ.'*' .OR.
     3             LINE(ISTART:ISTOP).EQ.'/') THEN
                   COP=LINE(ISTART:ISTOP)
                ELSE
                   GO TO 1000
                END IF
             END IF
             NOP=NOP+1
C
C6E3----Get the operand.
             CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
             WRITE(IOUT,47 ) COP,LINE(ISTART:ISTOP)
   47        FORMAT(1X,'                        ',A,' ARRAY ',A)
C
C6E4----Lookup the operand in the list of existing multipliers
             DO 50 MM=1,M
               CTMP1=MLTNAM(MM)
               CALL UPCASE(CTMP1)
               CTMP2=LINE(ISTART:ISTOP)
               CALL UPCASE(CTMP2)
               IF(CTMP1.EQ.CTMP2) GO TO 60
   50        CONTINUE
             WRITE(IOUT,51) LINE(ISTART:ISTOP)
   51        FORMAT(1X,
     1        'ARRAY OPERAND HAS NOT BEEN PREVIOUSLY DEFINED:',A)
             CALL USTOP(' ')
C
C6E5----Apply the + operator.
   60        IF(COP.EQ.'+' .OR. COP.EQ.' ') THEN
                DO 100 I = 1, NROW
                DO 100 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)+ RMLT(J,I,MM)
  100           CONTINUE
             ELSE IF(COP.EQ.'-') THEN
                DO 200 I = 1, NROW
                DO 200 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)- RMLT(J,I,MM)
  200           CONTINUE
             ELSE IF(COP.EQ.'*') THEN
                DO 300 I = 1, NROW
                DO 300 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)* RMLT(J,I,MM)
  300           CONTINUE
             ELSE
                DO 400 I = 1, NROW
                DO 400 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)/ RMLT(J,I,MM)
  400           CONTINUE
             END IF
C
C6E6----Get the next operator.
             GO TO 45
C
C6E7-----Done defining the array.  Get the print code and print the array.
1000          IPRN=0
              L=20-ISTOP+ISTART
              IF(L.GT.1)  THEN
                 RW=' '
                 RW(L:20)=LINE(ISTART:ISTOP)
                 READ(RW,'(I20)',ERR=1200) IPRN
              END IF
 1200         IF(IPRN.GE.0) THEN
                 ANAME=' MULT. ARRAY: '//MLTNAM(M)
                 CALL ULAPRWC(RMLT(:,:,M),NCOL,NROW,0,IOUT,IPRN,
     1                 ANAME)
              END IF
          END IF
 2000   CONTINUE
      ENDIF
C
C7------Read the zone array names and arrays
      IF(NZONAR.GT.0) THEN
         DO 3000 NZ=1,NZONAR
         READ(INZONE,'(A)') ZONNAM(NZ)
         CALL U2DINT(IZON(:,:,NZ),'  ZONE ARRAY: '//ZONNAM(NZ),
     1            NROW,NCOL,0,INZONE,IOUT)
 3000    CONTINUE
      END IF
C
C8------Return.
      RETURN
      END
      SUBROUTINE SGWF2BAS7ARPVAL(IUPVAL)
C     ******************************************************************
C     READ PARAMETER INPUT FILE
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,    ONLY: IOUT,IUNIT
      USE PARAMMODULE, ONLY:MXPAR,IPSUM,PARNAM,B,NPVAL
C
      CHARACTER*10 PNI, PNJ
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------CHECK TO SEE IF THE PARAMETER FILE WAS DECLARED IN THE NAME FILE.
      IU=IUNIT(IUPVAL)
      IF(IU.LE.0) THEN
         NPVAL=0
         RETURN
      END IF
C
C2------INITIALIZE VARIABLES
      IERR = 0
      NPE = 0
C
C3------IDENTIFY PARAMETER VALUE OPTION.
      WRITE (IOUT,12) IU
   12 FORMAT (1X,/,1X,
     1  'PARAMETER VALUE INPUT FILE,  INPUT READ FROM UNIT ',I4)
C
C4------READ & PRINT NUMBER OF PARAMETER VALUES.
      CALL URDCOM(IU,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPVAL,DUM,IOUT,IU)
      WRITE (IOUT,14) NPVAL
   14 FORMAT (1X,/,1X,'NUMBER OF PARAMETER VALUES TO BE READ FROM',
     1               ' PARAMETER VALUE FILE:',I5)
      IF (NPVAL.LE.0) THEN
        WRITE (IOUT,16)
   16   FORMAT(1X,'NPVAL IN PARAMETER INPUT FILE MUST BE',
     1         ' > 0 -- STOP EXECUTION')
        CALL USTOP(' ')
      ENDIF
      IPSUM=NPVAL
C
C5-----DEACTIVATE OPTION IF THERE ARE NO PARAMETERS IN FILE.
      IF(NPVAL.LE.0) THEN
         WRITE(IOUT,*) ' NPVAL in parameter file is 0,',
     1            ' so ignoring the parameter file'
        CLOSE(UNIT=IU)
        IU=0
        RETURN
      END IF
C
C6------STOP IF THERE ARE MORE THAN THE MAXIMUM NUMBER OF PARAMETERS.
      IF(NPVAL.GT.MXPAR) THEN
         WRITE(IOUT,*) ' PARAMETER FILE CONTAINS',NPVAL,
     1     ' VALUES, BUT THE MAXIMUM NUMBER OF PARAMETERS IS',MXPAR
         CALL USTOP(' ')
      END IF
C
C7------WRITE A HEADING FOR THE LIST OF PARAMETERS.
      WRITE (IOUT,520)
  520 FORMAT (/,' INFORMATION ON PARAMETERS LISTED IN PARAMETER FILE',/,
     &             13X,'  VALUE IN',/,
     &   '    NAME     PARAMETER FILE',/,
     &   ' ----------  --------------')
C
C8-----READ AND WRITE PARAMETER NAMES AND VALUES.
      DO 70 I=1,NPVAL
        READ(IU,*,ERR=80) PARNAM(I),B(I)
        WRITE(IOUT,570) PARNAM(I),B(I)
  570   FORMAT(1X,A10,2X,G12.5)
C
C8A-----CHECK FOR DUPLICATE PARAMETER NAME FOR ALL BUT THE FIRST PARAMETER.
        IF (I.GT.1) THEN
          PNI=PARNAM(I)
          CALL UPCASE(PNI)
          IM1 = I-1
          DO 60 J=1,IM1
            PNJ=PARNAM(J)
            CALL UPCASE(PNJ)
            IF (PNI.EQ.PNJ) THEN
              WRITE(IOUT,500) PARNAM(I)
  500         FORMAT (' PARAMETER "',A10,
     &        '" IS LISTED MORE THAN ONCE IN PARAMETER FILE',/,
     &        ' -- STOP EXECUTION')
                IERR = 1
            ENDIF
   60     CONTINUE
        ENDIF
   70 CONTINUE
C
C9------WRITE A MESSAGE EXPLAINING THAT THE PARAMETER VALUES REPLACE THE
C9------VALUES FROM PACKAGE INPUT FILES..
      WRITE (IOUT,620)
  620 FORMAT(1X,77('-'))
      WRITE (IOUT,630)
  630 FORMAT(' FOR THE PARAMETERS LISTED IN THE TABLE ABOVE,',
     &       ' PARAMETER VALUES IN INDIVIDUAL',/,
     &       ' PACKAGE INPUT FILES ARE REPLACED BY THE VALUES FROM',
     &       ' THE PARAMETER INPUT FILE.')
C
C10-----STOP IF THERE WERE DUPLICATE NAMES.
      IF (IERR.GT.0) THEN
        WRITE(IOUT,680)
  680 FORMAT(/,
     &' ERROR FOUND IN PARAMETER INPUT FILE.  SEARCH ABOVE',/,
     &' FOR "STOP EXECUTION"')
         CALL USTOP(' ')
      ENDIF
C
C11-----CLOSE FILE AND RETURN.
      CLOSE(UNIT=IU)
      RETURN
C
C
   80 WRITE(IOUT,590)
  590 FORMAT(1X,/,1X,
     1  'ERROR ENCOUNTERED IN READING PARAMETER INPUT FILE',/,
     2       ' -- STOP EXECUTION')
      CALL USTOP(' ')
C
      END
      SUBROUTINE SGWF2BAS7STPVAL()
C     ******************************************************************
C     CHECK THAT PARAMETER DEFINITIONS ARE COMPLETE.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY: IOUT
      USE PARAMMODULE, ONLY:NPVAL,PARTYP,PARNAM
C     ------------------------------------------------------------------
      IF(NPVAL.LE.0) RETURN
      IERR=0
C
Cx------CHECK THAT ALL PARAMETERS IN PARAMETER INPUT FILE HAVE BEEN DEFINED.
      DO 90 IP=1,NPVAL
        IF (PARTYP(IP).EQ.' ') THEN
          IERR = 1
          WRITE(IOUT,110) PARNAM(IP)
  110     FORMAT(1X,/,1X,'PARAMETER "',A10,
     1      '" IN PARAMETER INPUT FILE HAS NOT BEEN DEFINED',/,
     2           ' -- STOP EXECUTION')
        ENDIF
   90 CONTINUE
C
      IF(IERR.NE.0) CALL USTOP(' ')
C
Cx------RETURN.
      RETURN
      END
      SUBROUTINE GWF2BAS7DA(IGRID)
C  DEALLOCATE GLOBAL DATA
      USE GLOBAL
      USE PARAMMODULE
      USE GWFBASMODULE
C
        DEALLOCATE(GLOBALDAT(IGRID)%NCOL)
        DEALLOCATE(GLOBALDAT(IGRID)%NROW)
        DEALLOCATE(GLOBALDAT(IGRID)%NLAY)
        DEALLOCATE(GLOBALDAT(IGRID)%NPER)
        DEALLOCATE(GLOBALDAT(IGRID)%NBOTM)
        DEALLOCATE(GLOBALDAT(IGRID)%NCNFBD)
        DEALLOCATE(GLOBALDAT(IGRID)%ITMUNI)
        DEALLOCATE(GLOBALDAT(IGRID)%LENUNI)
        DEALLOCATE(GLOBALDAT(IGRID)%IXSEC)
        DEALLOCATE(GLOBALDAT(IGRID)%ITRSS)
        DEALLOCATE(GLOBALDAT(IGRID)%INBAS)
        DEALLOCATE(GLOBALDAT(IGRID)%IFREFM)
        DEALLOCATE(GLOBALDAT(IGRID)%NODES)
        close(GLOBALDAT(IGRID)%IOUT)
        DEALLOCATE(GLOBALDAT(IGRID)%IOUT)
        DEALLOCATE(GLOBALDAT(IGRID)%MXITER)
C
        DEALLOCATE(GLOBALDAT(IGRID)%IUNIT)
        DEALLOCATE(GLOBALDAT(IGRID)%LAYCBD)
        DEALLOCATE(GLOBALDAT(IGRID)%LAYHDT)
        DEALLOCATE(GLOBALDAT(IGRID)%LAYHDS)
        DEALLOCATE(GLOBALDAT(IGRID)%NSTP)
        DEALLOCATE(GLOBALDAT(IGRID)%TSMULT)
        DEALLOCATE(GLOBALDAT(IGRID)%ISSFLG)
        DEALLOCATE(GLOBALDAT(IGRID)%DELR)
        DEALLOCATE(GLOBALDAT(IGRID)%DELC)
        DEALLOCATE(GLOBALDAT(IGRID)%BOTM)
        DEALLOCATE(GLOBALDAT(IGRID)%LBOTM)
        DEALLOCATE(GLOBALDAT(IGRID)%HNEW)
        DEALLOCATE(GLOBALDAT(IGRID)%HOLD)
        DEALLOCATE(GLOBALDAT(IGRID)%IBOUND)
        DEALLOCATE(GLOBALDAT(IGRID)%CR)
        DEALLOCATE(GLOBALDAT(IGRID)%CC)
        deallocate(globaldat(igrid)%kdsv)                               ! ANIPWT
        DEALLOCATE(GLOBALDAT(IGRID)%CV)
        DEALLOCATE(GLOBALDAT(IGRID)%HCOF)
        DEALLOCATE(GLOBALDAT(IGRID)%RHS)
        DEALLOCATE(GLOBALDAT(IGRID)%BUFF)
        DEALLOCATE(GLOBALDAT(IGRID)%STRT)
        IF(.NOT.ASSOCIATED(DDREF,STRT))
     1           DEALLOCATE(GLOBALDAT(IGRID)%DDREF)
        IF(.NOT.ASSOCIATED(IACTCELL))                                   ! PKS
     1           DEALLOCATE(GLOBALDAT(IGRID)%IACTCELL)                  ! PKS
C
        DEALLOCATE(ICLSUM,IPSUM,INAMLOC,NMLTAR,NZONAR,NPVAL)
        DEALLOCATE (PARAMDAT(IGRID)%B)
        DEALLOCATE (PARAMDAT(IGRID)%IACTIVE)
        DEALLOCATE (PARAMDAT(IGRID)%IPLOC)
        DEALLOCATE (PARAMDAT(IGRID)%IPCLST)
        DEALLOCATE (PARAMDAT(IGRID)%PARNAM)
        DEALLOCATE (PARAMDAT(IGRID)%PARTYP)
        DEALLOCATE (PARAMDAT(IGRID)%ZONNAM)
        DEALLOCATE (PARAMDAT(IGRID)%MLTNAM)
        DEALLOCATE (PARAMDAT(IGRID)%INAME)
        DEALLOCATE (PARAMDAT(IGRID)%RMLT)
        DEALLOCATE (PARAMDAT(IGRID)%IZON)
C
        DEALLOCATE(GWFBASDAT(IGRID)%MSUM)
        DEALLOCATE(GWFBASDAT(IGRID)%IHEDFM)
        DEALLOCATE(GWFBASDAT(IGRID)%IHEDUN)
        DEALLOCATE(GWFBASDAT(IGRID)%IDDNFM)
        DEALLOCATE(GWFBASDAT(IGRID)%IDDNUN)
        DEALLOCATE(GWFBASDAT(IGRID)%IBOUUN)
        DEALLOCATE(GWFBASDAT(IGRID)%LBHDSV)
        DEALLOCATE(GWFBASDAT(IGRID)%LBDDSV)
        DEALLOCATE(GWFBASDAT(IGRID)%LBBOSV)
        DEALLOCATE(GWFBASDAT(IGRID)%IBUDFL)
        DEALLOCATE(GWFBASDAT(IGRID)%ICBCFL)
        DEALLOCATE(GWFBASDAT(IGRID)%IHDDFL)
        DEALLOCATE(GWFBASDAT(IGRID)%IAUXSV)
        DEALLOCATE(GWFBASDAT(IGRID)%IBDOPT)
        DEALLOCATE(GWFBASDAT(IGRID)%IPRTIM)
        DEALLOCATE(GWFBASDAT(IGRID)%IPEROC)
        DEALLOCATE(GWFBASDAT(IGRID)%ITSOC)
        DEALLOCATE(GWFBASDAT(IGRID)%ICHFLG)
        DEALLOCATE(GWFBASDAT(IGRID)%IDDREF)
        DEALLOCATE(GWFBASDAT(IGRID)%IDDREFNEW)
        DEALLOCATE(GWFBASDAT(IGRID)%DELT)
        DEALLOCATE(GWFBASDAT(IGRID)%PERTIM)
        DEALLOCATE(GWFBASDAT(IGRID)%TOTIM)
        DEALLOCATE(GWFBASDAT(IGRID)%HNOFLO)
        DEALLOCATE(GWFBASDAT(IGRID)%HDRY)
        DEALLOCATE(GWFBASDAT(IGRID)%STOPER)
        DEALLOCATE(GWFBASDAT(IGRID)%CHEDFM)
        DEALLOCATE(GWFBASDAT(IGRID)%CDDNFM)
        DEALLOCATE(GWFBASDAT(IGRID)%CBOUFM)
C
        DEALLOCATE(GWFBASDAT(IGRID)%IOFLG)
        DEALLOCATE(GWFBASDAT(IGRID)%VBVL)
        DEALLOCATE(GWFBASDAT(IGRID)%VBNM)
C
      RETURN
      END
      SUBROUTINE SGWF2BAS7PNT(IGRID)
C  Change global data to a different grid.
      USE GLOBAL
      USE PARAMMODULE
      USE GWFBASMODULE
C
        NCOL=>GLOBALDAT(IGRID)%NCOL
        NROW=>GLOBALDAT(IGRID)%NROW
        NLAY=>GLOBALDAT(IGRID)%NLAY
        NPER=>GLOBALDAT(IGRID)%NPER
        NBOTM=>GLOBALDAT(IGRID)%NBOTM
        NCNFBD=>GLOBALDAT(IGRID)%NCNFBD
        ITMUNI=>GLOBALDAT(IGRID)%ITMUNI
        LENUNI=>GLOBALDAT(IGRID)%LENUNI
        IXSEC=>GLOBALDAT(IGRID)%IXSEC
        ITRSS=>GLOBALDAT(IGRID)%ITRSS
        INBAS=>GLOBALDAT(IGRID)%INBAS
        IFREFM=>GLOBALDAT(IGRID)%IFREFM
        NODES=>GLOBALDAT(IGRID)%NODES
        IOUT=>GLOBALDAT(IGRID)%IOUT
        MXITER=>GLOBALDAT(IGRID)%MXITER
C
        IUNIT=>GLOBALDAT(IGRID)%IUNIT
        LAYCBD=>GLOBALDAT(IGRID)%LAYCBD
        LAYHDT=>GLOBALDAT(IGRID)%LAYHDT
        LAYHDS=>GLOBALDAT(IGRID)%LAYHDS
        PERLEN=>GLOBALDAT(IGRID)%PERLEN
        NSTP=>GLOBALDAT(IGRID)%NSTP
        TSMULT=>GLOBALDAT(IGRID)%TSMULT
        ISSFLG=>GLOBALDAT(IGRID)%ISSFLG
        DELR=>GLOBALDAT(IGRID)%DELR
        DELC=>GLOBALDAT(IGRID)%DELC
        BOTM=>GLOBALDAT(IGRID)%BOTM
        LBOTM=>GLOBALDAT(IGRID)%LBOTM
        HNEW=>GLOBALDAT(IGRID)%HNEW
        HOLD=>GLOBALDAT(IGRID)%HOLD
        IBOUND=>GLOBALDAT(IGRID)%IBOUND
        CR=>GLOBALDAT(IGRID)%CR
        CC=>GLOBALDAT(IGRID)%CC
        kdsv=>globaldat(igrid)%kdsv                                     ! ANIPWT
        CV=>GLOBALDAT(IGRID)%CV
        HCOF=>GLOBALDAT(IGRID)%HCOF
        RHS=>GLOBALDAT(IGRID)%RHS
        BUFF=>GLOBALDAT(IGRID)%BUFF
        STRT=>GLOBALDAT(IGRID)%STRT
        DDREF=>GLOBALDAT(IGRID)%DDREF
        IACTCELL=>GLOBALDAT(IGRID)%IACTCELL
C
        ICLSUM=>PARAMDAT(IGRID)%ICLSUM
        IPSUM=>PARAMDAT(IGRID)%IPSUM
        INAMLOC=>PARAMDAT(IGRID)%INAMLOC
        NMLTAR=>PARAMDAT(IGRID)%NMLTAR
        NZONAR=>PARAMDAT(IGRID)%NZONAR
        NPVAL=>PARAMDAT(IGRID)%NPVAL
C
        B=>PARAMDAT(IGRID)%B
        IACTIVE=>PARAMDAT(IGRID)%IACTIVE
        IPLOC=>PARAMDAT(IGRID)%IPLOC
        IPCLST=>PARAMDAT(IGRID)%IPCLST
        IZON=>PARAMDAT(IGRID)%IZON
        RMLT=>PARAMDAT(IGRID)%RMLT
        PARNAM=>PARAMDAT(IGRID)%PARNAM
        PARTYP=>PARAMDAT(IGRID)%PARTYP
        ZONNAM=>PARAMDAT(IGRID)%ZONNAM
        MLTNAM=>PARAMDAT(IGRID)%MLTNAM
        INAME=>PARAMDAT(IGRID)%INAME
C
        MSUM=>GWFBASDAT(IGRID)%MSUM
        IHEDFM=>GWFBASDAT(IGRID)%IHEDFM
        IHEDUN=>GWFBASDAT(IGRID)%IHEDUN
        IDDNFM=>GWFBASDAT(IGRID)%IDDNFM
        IDDNUN=>GWFBASDAT(IGRID)%IDDNUN
        IBOUUN=>GWFBASDAT(IGRID)%IBOUUN
        LBHDSV=>GWFBASDAT(IGRID)%LBHDSV
        LBDDSV=>GWFBASDAT(IGRID)%LBDDSV
        LBBOSV=>GWFBASDAT(IGRID)%LBBOSV
        IBUDFL=>GWFBASDAT(IGRID)%IBUDFL
        ICBCFL=>GWFBASDAT(IGRID)%ICBCFL
        IHDDFL=>GWFBASDAT(IGRID)%IHDDFL
        IAUXSV=>GWFBASDAT(IGRID)%IAUXSV
        IBDOPT=>GWFBASDAT(IGRID)%IBDOPT
        IPRTIM=>GWFBASDAT(IGRID)%IPRTIM
        IPEROC=>GWFBASDAT(IGRID)%IPEROC
        ITSOC=>GWFBASDAT(IGRID)%ITSOC
        ICHFLG=>GWFBASDAT(IGRID)%ICHFLG
        IDDREF=>GWFBASDAT(IGRID)%IDDREF
        IDDREFNEW=>GWFBASDAT(IGRID)%IDDREFNEW
        DELT=>GWFBASDAT(IGRID)%DELT
        PERTIM=>GWFBASDAT(IGRID)%PERTIM
        TOTIM=>GWFBASDAT(IGRID)%TOTIM
        HNOFLO=>GWFBASDAT(IGRID)%HNOFLO
        HDRY=>GWFBASDAT(IGRID)%HDRY
        STOPER=>GWFBASDAT(IGRID)%STOPER
        CHEDFM=>GWFBASDAT(IGRID)%CHEDFM
        CDDNFM=>GWFBASDAT(IGRID)%CDDNFM
        CBOUFM=>GWFBASDAT(IGRID)%CBOUFM
C
        IOFLG=>GWFBASDAT(IGRID)%IOFLG
        VBVL=>GWFBASDAT(IGRID)%VBVL
        VBNM=>GWFBASDAT(IGRID)%VBNM
C
      RETURN
      END
      SUBROUTINE SGWF2BAS7PSV(IGRID)
C  Save global data for a grid.
      USE GLOBAL
      USE PARAMMODULE
      USE GWFBASMODULE
C
        GLOBALDAT(IGRID)%NCOL=>NCOL
        GLOBALDAT(IGRID)%NROW=>NROW
        GLOBALDAT(IGRID)%NLAY=>NLAY
        GLOBALDAT(IGRID)%NPER=>NPER
        GLOBALDAT(IGRID)%NBOTM=>NBOTM
        GLOBALDAT(IGRID)%NCNFBD=>NCNFBD
        GLOBALDAT(IGRID)%ITMUNI=>ITMUNI
        GLOBALDAT(IGRID)%LENUNI=>LENUNI
        GLOBALDAT(IGRID)%IXSEC=>IXSEC
        GLOBALDAT(IGRID)%ITRSS=>ITRSS
        GLOBALDAT(IGRID)%INBAS=>INBAS
        GLOBALDAT(IGRID)%IFREFM=>IFREFM
        GLOBALDAT(IGRID)%NODES=>NODES
        GLOBALDAT(IGRID)%IOUT=>IOUT
        GLOBALDAT(IGRID)%MXITER=>MXITER
C
        GLOBALDAT(IGRID)%IUNIT=>IUNIT
        GLOBALDAT(IGRID)%LAYCBD=>LAYCBD
        GLOBALDAT(IGRID)%LAYHDT=>LAYHDT
        GLOBALDAT(IGRID)%LAYHDS=>LAYHDS
        GLOBALDAT(IGRID)%PERLEN=>PERLEN
        GLOBALDAT(IGRID)%NSTP=>NSTP
        GLOBALDAT(IGRID)%TSMULT=>TSMULT
        GLOBALDAT(IGRID)%ISSFLG=>ISSFLG
        GLOBALDAT(IGRID)%DELR=>DELR
        GLOBALDAT(IGRID)%DELC=>DELC
        GLOBALDAT(IGRID)%BOTM=>BOTM
        GLOBALDAT(IGRID)%LBOTM=>LBOTM
        GLOBALDAT(IGRID)%HNEW=>HNEW
        GLOBALDAT(IGRID)%HOLD=>HOLD
        GLOBALDAT(IGRID)%IBOUND=>IBOUND
        GLOBALDAT(IGRID)%CR=>CR
        GLOBALDAT(IGRID)%CC=>CC
        globaldat(igrid)%kdsv=>kdsv                                     ! ANIPWT
        GLOBALDAT(IGRID)%CV=>CV
        GLOBALDAT(IGRID)%HCOF=>HCOF
        GLOBALDAT(IGRID)%RHS=>RHS
        GLOBALDAT(IGRID)%BUFF=>BUFF
        GLOBALDAT(IGRID)%STRT=>STRT
        GLOBALDAT(IGRID)%DDREF=>DDREF
        GLOBALDAT(IGRID)%IACTCELL=>IACTCELL
C
        PARAMDAT(IGRID)%ICLSUM=>ICLSUM
        PARAMDAT(IGRID)%IPSUM=>IPSUM
        PARAMDAT(IGRID)%INAMLOC=>INAMLOC
        PARAMDAT(IGRID)%NMLTAR=>NMLTAR
        PARAMDAT(IGRID)%NZONAR=>NZONAR
        PARAMDAT(IGRID)%NPVAL=>NPVAL
C
        PARAMDAT(IGRID)%B=>B
        PARAMDAT(IGRID)%IACTIVE=>IACTIVE
        PARAMDAT(IGRID)%IPLOC=>IPLOC
        PARAMDAT(IGRID)%IPCLST=>IPCLST
        PARAMDAT(IGRID)%IZON=>IZON
        PARAMDAT(IGRID)%RMLT=>RMLT
        PARAMDAT(IGRID)%PARNAM=>PARNAM
        PARAMDAT(IGRID)%PARTYP=>PARTYP
        PARAMDAT(IGRID)%ZONNAM=>ZONNAM
        PARAMDAT(IGRID)%MLTNAM=>MLTNAM
        PARAMDAT(IGRID)%INAME=>INAME
C
        GWFBASDAT(IGRID)%MSUM=>MSUM
        GWFBASDAT(IGRID)%IHEDFM=>IHEDFM
        GWFBASDAT(IGRID)%IHEDUN=>IHEDUN
        GWFBASDAT(IGRID)%IDDNFM=>IDDNFM
        GWFBASDAT(IGRID)%IDDNUN=>IDDNUN
        GWFBASDAT(IGRID)%IBOUUN=>IBOUUN
        GWFBASDAT(IGRID)%LBHDSV=>LBHDSV
        GWFBASDAT(IGRID)%LBDDSV=>LBDDSV
        GWFBASDAT(IGRID)%LBBOSV=>LBBOSV
        GWFBASDAT(IGRID)%IBUDFL=>IBUDFL
        GWFBASDAT(IGRID)%ICBCFL=>ICBCFL
        GWFBASDAT(IGRID)%IHDDFL=>IHDDFL
        GWFBASDAT(IGRID)%IAUXSV=>IAUXSV
        GWFBASDAT(IGRID)%IBDOPT=>IBDOPT
        GWFBASDAT(IGRID)%IPRTIM=>IPRTIM
        GWFBASDAT(IGRID)%IPEROC=>IPEROC
        GWFBASDAT(IGRID)%ITSOC=>ITSOC
        GWFBASDAT(IGRID)%ICHFLG=>ICHFLG
        GWFBASDAT(IGRID)%IDDREF=>IDDREF
        GWFBASDAT(IGRID)%IDDREFNEW=>IDDREFNEW
        GWFBASDAT(IGRID)%DELT=>DELT
        GWFBASDAT(IGRID)%PERTIM=>PERTIM
        GWFBASDAT(IGRID)%TOTIM=>TOTIM
        GWFBASDAT(IGRID)%HNOFLO=>HNOFLO
        GWFBASDAT(IGRID)%HDRY=>HDRY
        GWFBASDAT(IGRID)%STOPER=>STOPER        
        GWFBASDAT(IGRID)%CHEDFM=>CHEDFM
        GWFBASDAT(IGRID)%CDDNFM=>CDDNFM
        GWFBASDAT(IGRID)%CBOUFM=>CBOUFM
C
        GWFBASDAT(IGRID)%IOFLG=>IOFLG
        GWFBASDAT(IGRID)%VBVL=>VBVL
        GWFBASDAT(IGRID)%VBNM=>VBNM
C
      RETURN
      END
