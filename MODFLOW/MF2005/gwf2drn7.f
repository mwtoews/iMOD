c   Copyright (C) Stichting Deltares, 2005-2014.
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

      MODULE GWFDRNMODULE
        INTEGER,SAVE,POINTER  ::NDRAIN,MXDRN,NDRNVL,IDRNCB,IPRDRN
        INTEGER,SAVE,POINTER  ::NPDRN,IDRNPB,NNPDRN
        CHARACTER(LEN=16),SAVE, DIMENSION(:),   POINTER     ::DRNAUX
        REAL,             SAVE, DIMENSION(:,:), POINTER     ::DRAI
        real, save, dimension(:), pointer :: drnlev                     ! NHI
        integer,save,pointer  ::idrnsubsys,ndrnsubsys                   ! dsubsys
        integer,save,dimension(:),pointer :: drnsubsidx                 ! dsubsys
        integer, save, pointer :: iiconchk                              ! iconchk
        real, save, dimension(:,:,:), pointer :: wiconchk               ! iconchk
        integer, save, dimension(:,:,:), pointer :: w2iconchk           ! iconchk
        real, parameter :: iconchknodata = -9999.                       ! iconchk
      TYPE GWFDRNTYPE
        INTEGER,POINTER  ::NDRAIN,MXDRN,NDRNVL,IDRNCB,IPRDRN
        INTEGER,POINTER  ::NPDRN,IDRNPB,NNPDRN
        CHARACTER(LEN=16), DIMENSION(:),   POINTER     ::DRNAUX
        REAL,              DIMENSION(:,:), POINTER     ::DRAI
        real, dimension(:), pointer :: drnlev                           ! NHI
        integer,pointer  ::idrnsubsys,ndrnsubsys                        ! dsubsys
        integer,dimension(:),pointer :: drnsubsidx                      ! dsubsys
        integer,pointer :: iiconchk                                     ! iconchk
        real, dimension(:,:,:), pointer :: wiconchk                     ! iconchk
        integer,dimension(:,:,:), pointer :: w2iconchk                  ! iconchk
      END TYPE
      TYPE(GWFDRNTYPE), SAVE:: GWFDRNDAT(10)
      END MODULE GWFDRNMODULE



      SUBROUTINE GWF2DRN7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR DRAINS AND READ PARAMETER DEFINITIONS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      use ulstrd_inferface                                              ! GCD
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM
      USE GWFDRNMODULE, ONLY:NDRAIN,MXDRN,NDRNVL,IDRNCB,IPRDRN,NPDRN,
     1                       IDRNPB,NNPDRN,DRNAUX,DRAI
     1                      ,idrnsubsys,                                ! dsubsys
     1                       drnlev,                                    ! NHI
     1                       iiconchk,wiconchk,w2iconchk,iconchknodata  ! iconchk
      CHARACTER*200 LINE
      character     drnsubsys*16                                        ! dsubsys
      character     drniconchk*16                                       ! iconchk
C     ------------------------------------------------------------------
      ALLOCATE(NDRAIN,MXDRN,NDRNVL,IDRNCB,IPRDRN)
      ALLOCATE(NPDRN,IDRNPB,NNPDRN)
      allocate(idrnsubsys)                                              ! dsubsys
      allocate(iiconchk)                                                ! iconchk
C
C1------IDENTIFY PACKAGE AND INITIALIZE NDRAIN.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'DRN -- DRAIN PACKAGE, VERSION 7, 5/2/2005',
     1' INPUT READ FROM UNIT ',I4)
      NDRAIN=0
      NNPDRN=0
      idrnsubsys=0                                                      ! dsubsys
      iiconchk=0                                                        ! iconchk
C
C2------READ MAXIMUM NUMBER OF DRAINS AND UNIT OR FLAG FOR
C2------CELL-BY-CELL FLOW TERMS.
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPDRN,MXPD)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') MXACTD,IDRNCB
         LLOC=21
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTD,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDRNCB,R,IOUT,IN)
      END IF
      WRITE(IOUT,3) MXACTD
    3 FORMAT(1X,'MAXIMUM OF ',I6,' ACTIVE DRAINS AT ONE TIME')
      IF(IDRNCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
         IF(IDRNCB.GT.0) WRITE(IOUT,8) IDRNCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
C
C3------READ AUXILIARY VARIABLES AND CBC ALLOCATION OPTION.
      ALLOCATE (DRNAUX(20))
      NAUX=0
      IPRDRN=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1        LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.20) THEN
            NAUX=NAUX+1
            DRNAUX(NAUX)=LINE(ISTART:ISTOP)
            WRITE(IOUT,12) DRNAUX(NAUX)
   12       FORMAT(1X,'AUXILIARY DRAIN VARIABLE: ',A)
         END IF
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,13)
   13    FORMAT(1X,'LISTS OF DRAIN CELLS WILL NOT BE PRINTED')
         IPRDRN = 0
         GO TO 10
      else if(line(istart:istop).eq.'DSUBSYS') then                     ! dsubsys
         call urword(line,lloc,istart,istop,1,n,r,iout,in)              ! dsubsys
         drnsubsys=line(istart:istop)                                   ! dsubsys
         idrnsubsys=999                                                 ! dsubsys
         go to 10                                                       ! dsubsys
      else if(line(istart:istop).eq.'ICONCHK') then                     ! iconchk
         call urword(line,lloc,istart,istop,1,n,r,iout,in)              ! iconchk
         drniconchk=line(istart:istop)                                  ! iconchk
         iiconchk=999                                                   ! iconchk
         go to 10                                                       ! iconchk
      END IF

c check or DSUBSYS has been defined and AUX variabel exist              ! dsubsys
      if (idrnsubsys.gt.0) then                                         ! dsubsys
         idrnsubsys=0                                                   ! dsubsys
         do i=1,naux                                                    ! dsubsys
            if (drnsubsys.eq.drnaux(i)) then                            ! dsubsys
               idrnsubsys=i                                             ! dsubsys
            endif                                                       ! dsubsys
         enddo                                                          ! dsubsys
         if (idrnsubsys.eq.0) then                                      ! dsubsys
            ! ERROR defined variable not found                          ! dsubsys
            write(iout,'(1x,3a)') 'ERROR DSUBSYS variable ',drnsubsys,  ! dsubsys
     1                     ' not defined as an auxiliary variable.'     ! dsubsys
            call ustop(' ')                                             ! dsubsys
         else                                                           ! dsubsys
            ! dsubsys found                                             ! dsubsys
            write(iout,'(1x,3a)') 'DSUBSYS variabel ',drnsubsys,        ! dsubsys
     1                    ' used for sub-system indices.'               ! dsubsys
            ! isubsys gets the column number of drnR in which the       ! dsubsys
            ! sub system indices                                        ! dsubsys
            idrnsubsys=idrnsubsys+5                                     ! dsubsys
         endif                                                          ! dsubsys
      endif                                                             ! dsubsys
C
      if (iiconchk.gt.0) then                                           ! iconchk
         do i=1,naux                                                    ! iconchk
            if (drniconchk.eq.drnaux(i)) then                           ! iconchk
               iiconchk=i+5                                             ! iconchk
            endif                                                       ! iconchk
         enddo                                                          ! iconchk
         allocate(wiconchk(ncol,nrow,nlay))                             ! iconchk
         allocate(w2iconchk(ncol,nrow,nlay))                            ! iconchk
         wiconchk = iconchknodata                                       ! iconchk
         w2iconchk = 1                                                  ! iconchk
      else                                                              ! iconchk
         allocate(wiconchk(1,1,1))                                      ! iconchk
         allocate(w2iconchk(1,1,1))                                     ! iconchk
      end if
C
C3A-----THERE ARE FIVE INPUT DATA VALUES PLUS ONE LOCATION FOR
C3A-----CELL-BY-CELL FLOW.
      NDRNVL=6+NAUX
C
C4------ALLOCATE SPACE FOR DRAIN ARRAYs.
      IDRNPB=MXACTD+1
      MXDRN=MXACTD+MXPD
      ALLOCATE (DRAI(NDRNVL,MXDRN))
      nullify(drnlev)                                                   ! NHI
C
C5------READ NAMED PARAMETERS.
      WRITE(IOUT,1000) NPDRN
 1000 FORMAT(1X,//1X,I5,' Drain parameters')
      IF(NPDRN.GT.0) THEN
        LSTSUM=IDRNPB
        DO 120 K=1,NPDRN
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXDRN,IN,IOUT,IP,'DRN','DRN',1,
     &                   NUMINST)
          NLST=LSTSUM-LSTBEG
          IF(NUMINST.EQ.0) THEN
C5A-----READ PARAMETER WITHOUT INSTANCES
            CALL ULSTRD(NLST,DRAI,LSTBEG,NDRNVL,MXDRN,1,IN,IOUT,
     &      'DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  STRESS FACTOR',
     &        DRNAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,5,5,IPRDRN)
          ELSE
C5B-----READ INSTANCES
            NINLST=NLST/NUMINST
            DO 110 I=1,NUMINST
            CALL UINSRP(I,IN,IOUT,IP,IPRDRN)
            CALL ULSTRD(NINLST,DRAI,LSTBEG,NDRNVL,MXDRN,1,IN,IOUT,
     &      'DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  STRESS FACTOR',
     &        DRNAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,5,5,IPRDRN)
            LSTBEG=LSTBEG+NINLST
  110       CONTINUE
          END IF
  120   CONTINUE
      END IF
C
C6------RETURN
      CALL SGWF2DRN7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2DRN7RP(IN,IGRID,inriv)
C     ******************************************************************
C     READ DRAIN HEAD, CONDUCTANCE AND BOTTOM ELEVATION
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      use ulstrd_inferface                                              ! GCD
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM
      USE GWFDRNMODULE, ONLY:NDRAIN,MXDRN,NDRNVL,IPRDRN,NPDRN,
     1                       IDRNPB,NNPDRN,DRNAUX,DRAI
     1                       ,idrnsubsys,ndrnsubsys,drnsubsidx,         ! dsubsys
     1                       drnlev,                                    ! NHI
     1                       iiconchk,wiconchk,w2iconchk,iconchknodata  ! iconchk
      USE GWFRIVMODULE, ONLY: lreuse, NRIVER, RIVR                      ! iconchk
C     ------------------------------------------------------------------
      CALL SGWF2DRN7PNT(IGRID)
C
C1------READ ITMP (NUMBER OF DRAINS OR FLAG TO REUSE DATA) AND
C1------NUMBER OF PARAMETERS.
      IF(NPDRN.GT.0) THEN
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(2I10)') ITMP,NP
         ELSE
            READ(IN,*) ITMP,NP
         END IF
      ELSE
         NP=0
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(I10)') ITMP
         ELSE
            READ(IN,*) ITMP
         END IF
      END IF
C
C------CALCULATE SOME CONSTANTS
      NAUX=NDRNVL-6
      IOUTU = IOUT
      IF(IPRDRN.EQ.0) IOUTU=-IOUT
C
C2------DETERMINE THE NUMBER OF NON-PARAMETER DRAINS.
      IF(ITMP.LT.0) THEN
         call sts2nodata(in)                                             ! STS
         WRITE(IOUT,7)
    7    FORMAT(1X,/1X,
     1        'REUSING NON-PARAMETER DRAINS FROM LAST STRESS PERIOD')
      ELSE
         call sts2data(in)                                               ! STS
         NNPDRN=ITMP
      END IF
C
C3------IF THERE ARE NEW NON-PARAMETER DRAINS, READ THEM.
      MXACTD=IDRNPB-1
      IF(ITMP.GT.0) THEN
         IF(NNPDRN.GT.MXACTD) THEN
            WRITE(IOUT,99) NNPDRN,MXACTD
   99       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE DRAINS (',I6,
     1                     ') IS GREATER THAN MXACTD(',I6,')')
            CALL USTOP(' ')
         END IF
         CALL ULSTRD(NNPDRN,DRAI,1,NDRNVL,MXDRN,1,IN,IOUT,
     1     'DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  CONDUCTANCE',
     2     DRNAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,5,5,IPRDRN)
         call pest1alpha_list('DC',nnpdrn,drai,ndrnvl,mxdrn,            ! IPEST
     1                        idrnsubsys)                               ! IPEST
      END IF
      NDRAIN=NNPDRN
C
C1C-----IF THERE ARE ACTIVE DRN PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('DRN')
      IF(NP.GT.0) THEN
         NREAD=NDRNVL-1
         DO 30 N=1,NP
         CALL UPARLSTSUB(IN,'DRN',IOUTU,'DRN',DRAI,NDRNVL,MXDRN,NREAD,
     1                MXACTD,NDRAIN,5,5,
     2     'DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  CONDUCTANCE',
     3            DRNAUX,20,NAUX)
   30    CONTINUE
      END IF
C
      if(.not.associated(drnlev))then                                   ! NHI
       allocate(drnlev(ndrain))                                         ! NHI
      else                                                              ! NHI
       if(size(drnlev).le.ndrain)then                                   ! NHI
        deallocate(drnlev); allocate(drnlev(ndrain))                    ! NHI
       endif                                                            ! NHI
      endif                                                             ! NHI
      do i = 1, ndrain ! save original drain levels                     ! NHI
         drnlev(i) = drai(4,i)                                          ! NHI
      end do                                                            ! NHI

C3------PRINT NUMBER OF DRAINS IN CURRENT STRESS PERIOD.
      WRITE (IOUT,101) NDRAIN
  101 FORMAT(1X,/1X,I6,' DRAINS')
C
      ! create subsystem index
      if (associated(ndrnsubsys)) deallocate(ndrnsubsys)                ! dsubsys
      allocate(ndrnsubsys)                                              ! dsubsys
      call usubscnt(drai,ndrnvl,nnpdrn,idrnsubsys,ndrnsubsys)           ! dsubsys
      if (associated(drnsubsidx)) deallocate(drnsubsidx)                ! dsubsys
      allocate(drnsubsidx(ndrnsubsys))                                  ! dsubsys
      call usubsidx(drai,ndrnvl,nnpdrn,idrnsubsys,drnsubsidx,           ! dsubsys
     1              ndrnsubsys)                                         ! dsubsys

      if (iiconchk.gt.0) then                                           ! iconchk
         if (inriv.gt.0) then                                           ! iconchk
            CALL SGWF2RIV7PNT(IGRID)                                    ! iconchk
            if (.not.lreuse.or.itmp.gt.0) then                          ! iconchk
               wiconchk = iconchknodata                                 ! iconchk
               do l = 1, nriver                                         ! iconchk
                  IL=RIVR(1,L)                                          ! iconchk
                  IR=RIVR(2,L)                                          ! iconchk
                  IC=RIVR(3,L)                                          ! iconchk
                  if (wiconchk(ic,ir,il).eq.iconchknodata) then         ! iconchk
                     wiconchk(ic,ir,il) = rivr(4,l)                     ! iconchk
                  else                                                  ! iconchk
                    wiconchk(ic,ir,il) =                                ! iconchk
     1                  max(wiconchk(ic,ir,il),rivr(4,l))               ! iconchk
                  end if                                                ! iconchk
               end do                                                   ! iconchk
            end if                                                      ! iconchk
         end if                                                         ! iconchk
         if (itmp.gt.0) then                                            ! iconchk
            w2iconchk = 1                                               ! iconchk
            do l = 1, ndrain                                            ! iconchk
               il = drai(1,l)                                           ! iconchk
               ir = drai(2,l)                                           ! iconchk
               ic = drai(3,l)                                           ! iconchk
               if (wiconchk(ic,ir,il).ne.iconchknodata) then            ! iconchk
                  if (drai(4,l).lt.wiconchk(ic,ir,il)) ! river stage above maximum drain level
     1               w2iconchk(ic,ir,il)=0                              ! iconchk
               end if                                                   ! iconchk
            end do                                                      ! iconchk
         end if                                                         ! iconchk

      end if                                                            ! iconchk

C7------SAVE POINTERS TO DATA AND RETURN.
      CALL SGWF2DRN7PSV(IGRID)                                          ! dsubsys

C8------RETURN.
  260 RETURN
      END
      SUBROUTINE GWF2DRN7FM(IGRID)
C     ******************************************************************
C     ADD DRAIN FLOW TO SOURCE TERM
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IBOUND,HNEW,RHS,HCOF
      USE GWFDRNMODULE, ONLY:NDRAIN,DRAI,
     1                       iiconchk, wiconchk,                        ! iconchk
     1                       w2iconchk, iconchknodata                   ! iconchk
      USE GWFRIVMODULE, ONLY:NRIVER,RIVR
C
      DOUBLE PRECISION EEL
C     ------------------------------------------------------------------
      CALL SGWF2DRN7PNT(IGRID)
C
C1------IF NDRAIN<=0 THERE ARE NO DRAINS. RETURN.
      IF(NDRAIN.LE.0) RETURN
C
C2------PROCESS EACH CELL IN THE DRAIN LIST.
      DO 100 L=1,NDRAIN
C
C3------GET COLUMN, ROW AND LAYER OF CELL CONTAINING DRAIN.
      IL=DRAI(1,L)
      IR=DRAI(2,L)
      IC=DRAI(3,L)
C
      if (iiconchk.gt.0) then                                           ! iconchk
         if (w2iconchk(ic,ir,il).eq.0.and.drai(iiconchk,l).gt.0.) then  ! iconchk
            goto 100                                                    ! iconchk
         end if                                                         ! iconchk
      end if                                                            ! iconchk
C
C4-------IF THE CELL IS EXTERNAL SKIP IT.
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 100
C
C5-------IF THE CELL IS INTERNAL GET THE DRAIN DATA.
      EL=DRAI(4,L)
      EEL=EL
C
C6------IF HEAD IS LOWER THAN DRAIN THEN SKIP THIS CELL.
      IF(HNEW(IC,IR,IL).LE.EEL) GO TO 100
C
C7------HEAD IS HIGHER THAN DRAIN. ADD TERMS TO RHS AND HCOF.
      C=DRAI(5,L)
      HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-C
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-C*EL
  100 CONTINUE
C
C8------RETURN.
      RETURN
      END
      SUBROUTINE GWF2DRN7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR DRAINS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,HNEW,BUFF
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,IAUXSV,DELT,PERTIM,TOTIM,
     1                      VBVL,VBNM
      USE GWFDRNMODULE,ONLY:NDRAIN,IDRNCB,DRAI,NDRNVL,DRNAUX
     1                      ,ndrnsubsys,drnsubsidx,idrnsubsys,          ! dsubsys
     1                       iiconchk, w2iconchk                        ! iconchk
C
      CHARACTER*16 TEXT
      character*16 htxt                                                 ! dsubsys
      DOUBLE PRECISION HHNEW,EEL,CCDRN,CEL,RATOUT,QQ
C
      DATA TEXT /'          DRAINS'/
      DATA htxt /'       DRAINS   '/                                    ! dsubsys
      integer  isub,lbeg,lend                                           ! dsubsys
C     ------------------------------------------------------------------
      CALL SGWF2DRN7PNT(IGRID)

C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C1------ACCUMULATOR (RATOUT).
      ZERO=0.
      RATOUT=ZERO
      IBD=0
      IF(IDRNCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IDRNCB.GT.0) IBD=ICBCFL
      IBDLBL=0
C
C4------IF THERE ARE NO DRAINS THEN DO NOT ACCUMULATE DRAIN FLOW.
      IF(NDRAIN.LE.0) GO TO 200
C
c loop for subsystems
      lend=0                                                            ! dsubsys
      do isub=1,ndrnsubsys                                              ! dsubsys

C3------CLEAR THE BUFFER.
         DO 50 IL=1,NLAY
         DO 50 IR=1,NROW
         DO 50 IC=1,NCOL
         BUFF(IC,IR,IL)=ZERO
50       CONTINUE

         lbeg=lend+1                                                    ! dsubsys
         lend=drnsubsidx(isub)                                          ! dsubsys

         ! create text
         if (idrnsubsys.gt.0.and.ndrnsubsys.gt.1) then                  ! dsubsys
            ! add number of subsystem to text
            write(htxt(15:16),'(i2)') int(drai(idrnsubsys,lbeg))        ! dsubsys
         else                                                           ! dsubsys
            ! use standard text
            htxt=text                                                   ! dsubsys
         endif                                                          ! dsubsys

C2------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD.EQ.2) THEN
         NAUX=NDRNVL-6
         IF(IAUXSV.EQ.0) NAUX=0
         CALL UBDSV4(KSTP,KPER,htxt,NAUX,DRNAUX,IDRNCB,NCOL,NROW,NLAY,
     1          NDRAIN,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C5------LOOP THROUGH EACH DRAIN CALCULATING FLOW.
c      DO 100 L=1,NDRAIN
      DO 100 L=lbeg,lend                                                ! dsubsys
C
C5A-----GET LAYER, ROW & COLUMN OF CELL CONTAINING REACH.
      IL=DRAI(1,L)
      IR=DRAI(2,L)
      IC=DRAI(3,L)
C
      if (iiconchk.gt.0) then                                           ! iconchk
         if (w2iconchk(ic,ir,il).eq.0.and.drai(iiconchk,l).gt.0.) then  ! iconchk
            goto 100                                                    ! iconchk
         end if                                                         ! iconchk
      end if                                                            ! iconchk
C
      Q=ZERO
C
C5B-----IF CELL IS NO-FLOW OR CONSTANT-HEAD, IGNORE IT.
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 99
C
C5C-----GET DRAIN PARAMETERS FROM DRAIN LIST.
      EL=DRAI(4,L)
      EEL=EL
      C=DRAI(5,L)
      HHNEW=HNEW(IC,IR,IL)
C
C5D-----IF HEAD HIGHER THAN DRAIN, CALCULATE Q=C*(EL-HHNEW).
C5D-----SUBTRACT Q FROM RATOUT.
      IF(HHNEW.GT.EEL) THEN
         CCDRN=C
         CEL=C*EL
         QQ=CEL - CCDRN*HHNEW
         Q=QQ
         RATOUT=RATOUT-QQ
      END IF
C
C5E-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IDRNCB<0).
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) WRITE(IOUT,61) TEXT,KPER,KSTP
   61    FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
         WRITE(IOUT,62) L,IL,IR,IC,Q
   62    FORMAT(1X,'DRAIN ',I6,'   LAYER ',I3,'   ROW ',I5,'   COL ',I5,
     1       '   RATE ',1PG15.6)
         IBDLBL=1
      END IF
C
C5F-----ADD Q TO BUFFER.
      BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+Q
C
C5G-----IF SAVING CELL-BY-CELL FLOWS IN A LIST, WRITE FLOW.  ALSO
C5G-----COPY FLOW TO DRAI.
   99 IF(IBD.EQ.2) CALL UBDSVB(IDRNCB,NCOL,NROW,IC,IR,IL,Q,
     1                  DRAI(:,L),NDRNVL,NAUX,6,IBOUND,NLAY)
      DRAI(NDRNVL,L)=Q
  100 CONTINUE
C
C6------IF CELL-BY-CELL FLOW WILL BE SAVED AS A 3-D ARRAY,
C6------CALL UBUDSV TO SAVE THEM.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,htxt,IDRNCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C
      enddo ! isub-loop                                                 ! rsubsys

C7------MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  200 ROUT=RATOUT
      VBVL(3,MSUM)=ZERO
      VBVL(4,MSUM)=ROUT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C8------INCREMENT BUDGET TERM COUNTER.
      MSUM=MSUM+1
C
C9------RETURN.
      RETURN
      END
      SUBROUTINE GWF2DRN7DA(IGRID)
C  Deallocate DRN MEMORY
      USE GWFDRNMODULE
C
        CALL SGWF2DRN7PNT(IGRID)
        DEALLOCATE(NDRAIN)
        DEALLOCATE(MXDRN)
        DEALLOCATE(NDRNVL)
        DEALLOCATE(IDRNCB)
        DEALLOCATE(IPRDRN)
        DEALLOCATE(NPDRN)
        DEALLOCATE(IDRNPB)
        DEALLOCATE(NNPDRN)
        DEALLOCATE(DRNAUX)
        DEALLOCATE(DRAI)
        if (associated(drnlev)) deallocate(drnlev)                      ! NHI
        deallocate(idrnsubsys)                                          ! dsubsys
        deallocate(ndrnsubsys)                                          ! dsubsys
        deallocate(drnsubsidx)                                          ! dsubsys
        deallocate(iiconchk)                                            ! iconchk
        deallocate(wiconchk)                                            ! iconchk
        deallocate(w2iconchk)                                           ! iconchk
C
      RETURN
      END
      SUBROUTINE SGWF2DRN7PNT(IGRID)
C  Change DRN data to a different grid.
      USE GWFDRNMODULE
C
        NDRAIN=>GWFDRNDAT(IGRID)%NDRAIN
        MXDRN=>GWFDRNDAT(IGRID)%MXDRN
        NDRNVL=>GWFDRNDAT(IGRID)%NDRNVL
        IDRNCB=>GWFDRNDAT(IGRID)%IDRNCB
        IPRDRN=>GWFDRNDAT(IGRID)%IPRDRN
        NPDRN=>GWFDRNDAT(IGRID)%NPDRN
        IDRNPB=>GWFDRNDAT(IGRID)%IDRNPB
        NNPDRN=>GWFDRNDAT(IGRID)%NNPDRN
        DRNAUX=>GWFDRNDAT(IGRID)%DRNAUX
        DRAI=>GWFDRNDAT(IGRID)%DRAI
        drnlev=>gwfdrndat(igrid)%drnlev                                 ! NHI
        idrnsubsys=>gwfdrndat(igrid)%idrnsubsys                         ! dsubsys
        ndrnsubsys=>gwfdrndat(igrid)%ndrnsubsys                         ! dsubsys
        drnsubsidx=>gwfdrndat(igrid)%drnsubsidx                         ! dsubsys
        iiconchk=>gwfdrndat(igrid)%iiconchk                             ! iconchk
        wiconchk=>gwfdrndat(igrid)%wiconchk                             ! iconchk
        w2iconchk=>gwfdrndat(igrid)%w2iconchk                           ! iconchk
C
      RETURN
      END
      SUBROUTINE SGWF2DRN7PSV(IGRID)
C  Save DRN data for a grid.
      USE GWFDRNMODULE
C
        GWFDRNDAT(IGRID)%NDRAIN=>NDRAIN
        GWFDRNDAT(IGRID)%MXDRN=>MXDRN
        GWFDRNDAT(IGRID)%NDRNVL=>NDRNVL
        GWFDRNDAT(IGRID)%IDRNCB=>IDRNCB
        GWFDRNDAT(IGRID)%IPRDRN=>IPRDRN
        GWFDRNDAT(IGRID)%NPDRN=>NPDRN
        GWFDRNDAT(IGRID)%IDRNPB=>IDRNPB
        GWFDRNDAT(IGRID)%NNPDRN=>NNPDRN
        GWFDRNDAT(IGRID)%DRNAUX=>DRNAUX
        GWFDRNDAT(IGRID)%DRAI=>DRAI
        gwfdrndat(igrid)%drnlev=>drnlev                                 ! NHI
        gwfdrndat(igrid)%idrnsubsys=>idrnsubsys                         ! dsubsys
        gwfdrndat(igrid)%ndrnsubsys=>ndrnsubsys                         ! dsubsys
        gwfdrndat(igrid)%drnsubsidx=>drnsubsidx                         ! dsubsys
        gwfdrndat(igrid)%iiconchk=>iiconchk                             ! iconchk
        gwfdrndat(igrid)%wiconchk=>wiconchk                             ! iconchk
        gwfdrndat(igrid)%w2iconchk=>w2iconchk                           ! iconchk
C
      RETURN
      END
