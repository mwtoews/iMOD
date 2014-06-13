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

      MODULE GWFRIVMODULE
        INTEGER,SAVE,POINTER  ::NRIVER,MXRIVR,NRIVVL,IRIVCB,IPRRIV
        INTEGER,SAVE,POINTER  ::NPRIV,IRIVPB,NNPRIV
        integer,save,pointer  :: ifvdl,isft                             ! ifvdl
        real,dimension(:,:,:),save,pointer :: sft                       ! ifvdl
        CHARACTER(LEN=16),SAVE, DIMENSION(:),   POINTER     ::RIVAUX
        REAL,             SAVE, DIMENSION(:,:), POINTER     ::RIVR
        INTEGER,SAVE,POINTER  ::IRIVRFACT                               ! RFACT
        integer,save,pointer  ::irivsubsys,nrivsubsys                   ! rsubsys
        integer,save,pointer  ::irivrconc                               ! rconc
        integer,save,dimension(:),pointer :: rivsubsidx                 ! rsubsys
        logical,save,pointer :: lreuse                                  ! iconchk
      TYPE GWFRIVTYPE
        INTEGER,POINTER  ::NRIVER,MXRIVR,NRIVVL,IRIVCB,IPRRIV
        INTEGER,POINTER  ::NPRIV,IRIVPB,NNPRIV
        integer,pointer  :: ifvdl,isft                                  ! ifvdl
        real,dimension(:,:,:),pointer :: sft                            ! ifvdl
        CHARACTER(LEN=16), DIMENSION(:),   POINTER     ::RIVAUX
        REAL,              DIMENSION(:,:), POINTER     ::RIVR
        INTEGER,POINTER  ::IRIVRFACT                                    ! RFACT
        integer,pointer  ::irivsubsys,nrivsubsys                        ! rsubsys
        integer,pointer  ::irivrconc                                    ! rconc
        integer,dimension(:),pointer :: rivsubsidx                      ! rsubsys
        logical,pointer :: lreuse                                       ! iconchk
      END TYPE
      TYPE(GWFRIVTYPE), SAVE:: GWFRIVDAT(10)
      END MODULE GWFRIVMODULE


      SUBROUTINE GWF2RIV7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR RIVERS AND READ PARAMETER DEFINITIONS.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      use ulstrd_inferface                                              ! GCD
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM
      USE GWFRIVMODULE, ONLY:NRIVER,MXRIVR,NRIVVL,IRIVCB,IPRRIV,NPRIV,
     1                       IRIVPB,NNPRIV,RIVAUX,RIVR
     1                      ,IRIVRFACT                                  ! RFACT
     1                      ,irivsubsys                                 ! rsubsys
     1                      ,irivrconc                                  ! rconc
     1                      ,lreuse                                     ! iconchk
     1                      ,ifvdl,isft,sft                             ! ifvdl
C
      CHARACTER*200 LINE
      character     RIVRFACT*16                                         ! RFACT
      character     rivsubsys*16                                        ! rsubsys
      character     rivrconc*16                                         ! rconc
C     ------------------------------------------------------------------
C
C1------Allocate scalar variables, which makes it possible for multiple
C1------grids to be defined.
      ALLOCATE(NRIVER,MXRIVR,NRIVVL,IRIVCB,IPRRIV,NPRIV,IRIVPB,NNPRIV)
      allocate(IRIVRFACT)                                               ! RFACT
      allocate(irivsubsys)                                              ! rsubsys
      allocate(irivrconc)                                               ! rconc
      allocate(lreuse)                                                  ! iconchk
      allocate(ifvdl)                                                   ! ifvdl
      allocate(isft)                                                    ! ifvdl
C
C2------IDENTIFY PACKAGE AND INITIALIZE NRIVER AND NNPRIV.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'RIV -- RIVER PACKAGE, VERSION 7, 5/2/2005',
     1' INPUT READ FROM UNIT ',I4)
      write(iout,'(10x,a)') 'TNO version v7r1, 28 apr 2009'             ! RFACT
      NRIVER=0
      NNPRIV=0
      IRIVRFACT=0                                                       ! RFACT
      irivsubsys=0                                                      ! rsubsys
      irivrconc=0                                                       ! rconc
      lreuse=.false.                                                    ! iconchk
      ifvdl=0                                                           ! ifvdl
      isft=0                                                            ! ifvdl
C
C3------READ MAXIMUM NUMBER OF RIVER REACHES AND UNIT OR FLAG FOR
C3------CELL-BY-CELL FLOW TERMS.
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPRIV,MXPR)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') MXACTR,IRIVCB
         LLOC=21
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTR,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRIVCB,R,IOUT,IN)
      END IF
      WRITE(IOUT,3) MXACTR
    3 FORMAT(1X,'MAXIMUM OF ',I6,' ACTIVE RIVER REACHES AT ONE TIME')
      IF(IRIVCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
      IF(IRIVCB.GT.0) WRITE(IOUT,8) IRIVCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
C
C4------READ AUXILIARY VARIABLES AND PRINT OPTION.
      ALLOCATE (RIVAUX(20))
      NAUX=0
      IPRRIV=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1        LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.20) THEN
            NAUX=NAUX+1
            RIVAUX(NAUX)=LINE(ISTART:ISTOP)
            WRITE(IOUT,12) RIVAUX(NAUX)
   12       FORMAT(1X,'AUXILIARY RIVER VARIABLE: ',A)
         END IF
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,13)
   13    FORMAT(1X,'LISTS OF RIVER CELLS WILL NOT BE PRINTED')
         IPRRIV = 0
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'RFACT') THEN                       ! RFACT
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)              ! RFACT
         RIVRFACT=LINE(ISTART:ISTOP)                                    ! RFACT
         IRIVRFACT=999                                                  ! RFACT
         GO TO 10                                                       ! RFACT
      else if(line(istart:istop).eq.'RSUBSYS') then                     ! rsubsys
         call urword(line,lloc,istart,istop,1,n,r,iout,in)              ! rsubsys
         rivsubsys=line(istart:istop)                                   ! rsubsys
         irivsubsys=999                                                 ! rsubsys
         go to 10                                                       ! rsubsys
      else if(line(istart:istop).eq.'RCONC') then                       ! rconc
         call urword(line,lloc,istart,istop,1,n,r,iout,in)              ! rconc
         rivrconc=line(istart:istop)                                    ! rconc
         irivrconc=999                                                  ! rconc
         go to 10                                                       ! rconc
      ELSE IF(LINE(ISTART:ISTOP).EQ.'IFVDL') THEN                       ! ifvdl
         ifvdl=1                                                        ! ifvdl
         go to 10                                                       ! ifvdl
      ELSE IF(LINE(ISTART:ISTOP).EQ.'SFT') THEN                         ! ifvdl
         isft=1                                                         ! ifvdl
         go to 10                                                       ! ifvdl
      END IF                                                            ! ifvdl
c                                                                       ! RFACT
c check or RFACT has been defined and AUX variabel exist                ! RFACT
      if (irivrfact.gt.0) then                                          ! RFACT
         irivrfact=0                                                    ! RFACT
         do i=1,naux                                                    ! RFACT
            if (rivrfact.eq.rivaux(i)) then                             ! RFACT
               irivrfact=i                                              ! RFACT
            endif                                                       ! RFACT
         enddo                                                          ! RFACT
         if (irivrfact.eq.0) then                                       ! RFACT
            ! ERROR defined variable not found                          ! RFACT
            write(iout,'(1x,3a)') 'ERROR RFACT variable ',rivrfact,     ! RFACT
     1                     ' not defined as an auxiliary variable.'     ! RFACT
            call ustop(' ')                                             ! RFACT
         else                                                           ! RFACT
            ! RFACT found                                               ! RFACT
            write(iout,'(1x,3a)') 'RFACT variabel ',rivrfact,           ! RFACT
     1                    ' used as infiltration factor.'               ! RFACT
            ! irivrfact gets the column number of RIVR in which the     ! RFACT
            ! infiltration factor is stored                             ! RFACT
            irivrfact=irivrfact+6                                       ! RFACT
         endif                                                          ! RFACT
      endif                                                             ! RFACT
C
c check or RSUBSYS has been defined and AUX variabel exist              ! rsubsys
      if (irivsubsys.gt.0) then                                         ! rsubsys
         irivsubsys=0                                                   ! rsubsys
         do i=1,naux                                                    ! rsubsys
            if (rivsubsys.eq.rivaux(i)) then                            ! rsubsys
               irivsubsys=i                                             ! rsubsys
            endif                                                       ! rsubsys
         enddo                                                          ! rsubsys
         if (irivsubsys.eq.0) then                                      ! rsubsys
            ! ERROR defined variable not found                          ! rsubsys
            write(iout,'(1x,3a)') 'ERROR RSUBSYS variable ',rivsubsys,  ! rsubsys
     1                     ' not defined as an auxiliary variable.'     ! rsubsys
            call ustop(' ')                                             ! rsubsys
         else                                                           ! rsubsys
            ! rsubsys found                                             ! rsubsys
            write(iout,'(1x,3a)') 'RSUBSYS variabel ',rivsubsys,        ! rsubsys
     1                    ' used for sub-system indices.'               ! rsubsys
            ! isubsys gets the column number of RIVR in which the       ! rsubsys
            ! sub system indices                                        ! rsubsys
            irivsubsys=irivsubsys+6                                     ! rsubsys
         endif                                                          ! rsubsys
      endif                                                             ! rsubsys

c check or RCONC has been defined and AUX variabel exist                ! rconc
      if (irivrconc.gt.0) then                                          ! rconc
         irivrconc=0                                                    ! rconc
         do i=1,naux                                                    ! rconc
            if (rivrconc.eq.rivaux(i)) then                             ! rconc
               irivrconc=i                                              ! rconc
            endif                                                       ! rconc
         enddo                                                          ! rconc
         if (irivrconc.eq.0) then                                       ! rconc
            ! ERROR defined variable not found                          ! rconc
            write(iout,'(1x,3a)') 'ERROR rconc variable ',rivrconc,     ! rconc
     1                     ' not defined as an auxiliary variable.'     ! rconc
            call ustop(' ')                                             ! rconc
         else                                                           ! rconc
            ! rconc found                                               ! rconc
            write(iout,'(1x,3a)') 'rconc variabel ',rivrconc,           ! rconc
     1                    ' used as chloride concentration.'            ! rconc
            ! irivrconc gets the column number of RIVR in which the     ! rconc
            ! infiltration factor is stored                             ! rconc
            irivrconc=irivrconc+6                                       ! rconc
         endif                                                          ! rconc
      endif                                                             ! rconc
C
      if (isft.eq.1) then                                               ! ifvdl
         allocate(sft(ncol,nrow,2))                                     ! ifvdl
         call u2drel(sft(1,1,1),'sft stream flow thick.  ',             ! ifvdl
     1             nrow,ncol,1,in,iout)                                 ! ifvdl
         call u2drel(sft(1,1,2),'sft permeability        ',             ! ifvdl
     1             nrow,ncol,1,in,iout)                                 ! ifvdl
      else                                                              ! ifvdl
         allocate(sft(1,1,1))                                           ! ifvdl
      end if                                                            ! ifvdl

C5------ALLOCATE SPACE FOR RIVER ARRAYS.
C5------FOR EACH REACH, THERE ARE SIX INPUT DATA VALUES PLUS ONE
C5------LOCATION FOR CELL-BY-CELL FLOW.
      NRIVVL=7+NAUX
      IRIVPB=MXACTR+1
      MXRIVR=MXACTR+MXPR
      ALLOCATE (RIVR(NRIVVL,MXRIVR))
C
C6------READ NAMED PARAMETERS.
      WRITE(IOUT,99) NPRIV
   99 FORMAT(1X,//1X,I5,' River parameters')
      IF(NPRIV.GT.0) THEN
        LSTSUM=IRIVPB
        DO 120 K=1,NPRIV
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXRIVR,IN,IOUT,IP,'RIV','RIV',1,
     &                   NUMINST)
          NLST=LSTSUM-LSTBEG
          IF (NUMINST.EQ.0) THEN
C6A-----READ PARAMETER WITHOUT INSTANCES
            CALL ULSTRD(NLST,RIVR,LSTBEG,NRIVVL,MXRIVR,1,IN,
     &            IOUT,'REACH NO.  LAYER   ROW   COL'//
     &            '     STAGE    STRESS FACTOR     BOTTOM EL.',
     &            RIVAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,5,5,IPRRIV)
          ELSE
C6B-----READ INSTANCES
            NINLST = NLST/NUMINST
            DO 110 I=1,NUMINST
            CALL UINSRP(I,IN,IOUT,IP,IPRRIV)
            CALL ULSTRD(NINLST,RIVR,LSTBEG,NRIVVL,MXRIVR,1,IN,
     &            IOUT,'REACH NO.  LAYER   ROW   COL'//
     &            '     STAGE    STRESS FACTOR     BOTTOM EL.',
     &            RIVAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,5,5,IPRRIV)
            LSTBEG=LSTBEG+NINLST
  110       CONTINUE
          END IF
  120   CONTINUE
      END IF
C
C7------SAVE POINTERS TO DATA AND RETURN.
      CALL SGWF2RIV7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2RIV7RP(IN,IGRID)
C     ******************************************************************
C     READ RIVER HEAD, CONDUCTANCE AND BOTTOM ELEVATION
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      use ulstrd_inferface                                              ! GCD
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM
      USE GWFRIVMODULE, ONLY:NRIVER,MXRIVR,NRIVVL,IPRRIV,NPRIV,
     1                       IRIVPB,NNPRIV,RIVAUX,RIVR,
     1                       irivsubsys,nrivsubsys,rivsubsidx,          ! rsubsys
     1                       lreuse,                                    ! iconchk
     1                       irivrfact                                  ! DLT
C     ------------------------------------------------------------------
      CALL SGWF2RIV7PNT(IGRID)
C
C1------READ ITMP (NUMBER OF RIVER REACHES OR FLAG TO REUSE DATA) AND
C1------NUMBER OF PARAMETERS.
      IF(NPRIV.GT.0) THEN
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
      NAUX=NRIVVL-7
      IOUTU = IOUT
      IF (IPRRIV.EQ.0) IOUTU = -IOUT
C
C2------DETERMINE THE NUMBER OF NON-PARAMETER REACHES.
      IF(ITMP.LT.0) THEN
         WRITE(IOUT,7)
    7    FORMAT(1X,/1X,
     1   'REUSING NON-PARAMETER RIVER REACHES FROM LAST STRESS PERIOD')
         call sts2nodata(in)                                                 ! DLT: save/restore
      ELSE
         NNPRIV=ITMP
         call sts2data(in)                                                   ! DLT: save/restore
      END IF
C
C3------IF THERE ARE NEW NON-PARAMETER REACHES, READ THEM.
      MXACTR=IRIVPB-1
      lreuse = .true.                                                   ! iconchk
      IF(ITMP.GT.0) THEN
         lreuse = .false.                                               ! iconchk
         IF(NNPRIV.GT.MXACTR) THEN
            WRITE(IOUT,99) NNPRIV,MXACTR
   99       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE REACHES (',I6,
     1                     ') IS GREATER THAN MXACTR(',I6,')')
            CALL USTOP(' ')
         END IF
         CALL ULSTRD(NNPRIV,RIVR,1,NRIVVL,MXRIVR,1,IN,IOUT,
     1          'REACH NO.  LAYER   ROW   COL'//
     2          '     STAGE      CONDUCTANCE     BOTTOM EL.',
     3          RIVAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,5,5,IPRRIV)
         call pest1alpha_list('RC',nnpriv,rivr,nrivvl,mxrivr,           ! IPEST
     1                        irivsubsys)                               ! IPEST
         call pest1alpha_list('RI',nnpriv,rivr,nrivvl,mxrivr,           ! IPEST
     1                        irivsubsys,irivrfact)                     ! IPEST
         call pest1alpha_list('IC',nnpriv,rivr,nrivvl,mxrivr,           ! IPEST
     1                        irivsubsys)                               ! IPEST
         call pest1alpha_list('II',nnpriv,rivr,nrivvl,mxrivr,           ! IPEST
     1                        irivsubsys,irivrfact)                     ! IPEST
      END IF
      NRIVER=NNPRIV
C
C1C-----IF THERE ARE ACTIVE RIV PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('RIV')
      IF(NP.GT.0) THEN
         NREAD=NRIVVL-1
         DO 30 N=1,NP
         CALL UPARLSTSUB(IN,'RIV',IOUTU,'RIV',RIVR,NRIVVL,MXRIVR,NREAD,
     1                MXACTR,NRIVER,5,5,
     2   'REACH NO.  LAYER   ROW   COL'//
     3   '     STAGE      CONDUCTANCE     BOTTOM EL.',RIVAUX,20,NAUX)
   30    CONTINUE
      END IF
C
C3------PRINT NUMBER OF REACHES IN CURRENT STRESS PERIOD.
      WRITE (IOUT,101) NRIVER
  101 FORMAT(1X,/1X,I6,' RIVER REACHES')
C
      ! create subsystem index
      if (associated(nrivsubsys)) deallocate(nrivsubsys)                ! rsubsys
      allocate(nrivsubsys)                                              ! rsubsys
      call usubscnt(rivr,nrivvl,nnpriv,irivsubsys,nrivsubsys)           ! rsubsys
      if (associated(rivsubsidx)) deallocate(rivsubsidx)                ! rsubsys
      allocate(rivsubsidx(nrivsubsys))                                  ! rsubsys
      call usubsidx(rivr,nrivvl,nnpriv,irivsubsys,rivsubsidx,           ! rsubsys
     1              nrivsubsys)                                         ! rsubsys
C7------SAVE POINTERS TO DATA AND RETURN.
      CALL SGWF2RIV7PSV(IGRID)                                          ! rsubsys
C8------RETURN.
  260 RETURN
      END
      SUBROUTINE GWF2RIV7FM(IGRID)
C     ******************************************************************
C     ADD RIVER TERMS TO RHS AND HCOF
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IBOUND,HNEW,RHS,HCOF
      USE GWFRIVMODULE, ONLY:NRIVER,RIVR
     1                      ,IRIVRFACT                                   ! RFACT
C     ------------------------------------------------------------------
      CALL SGWF2RIV7PNT(IGRID)
C
C1------IF NRIVER<=0 THERE ARE NO RIVERS. RETURN.
      IF(NRIVER.LE.0)RETURN
C
C2------PROCESS EACH CELL IN THE RIVER LIST.
      DO 100 L=1,NRIVER
C
C3------GET COLUMN, ROW, AND LAYER OF CELL CONTAINING REACH.
      IL=RIVR(1,L)
      IR=RIVR(2,L)
      IC=RIVR(3,L)
C
C4------IF THE CELL IS EXTERNAL SKIP IT.
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 100
C
C5------SINCE THE CELL IS INTERNAL GET THE RIVER DATA.
      HRIV=RIVR(4,L)
      CRIV=RIVR(5,L)
      RBOT=RIVR(6,L)
      RRBOT=RBOT
c ------APPLY INFILTRATION FACTOR                                        ! RFACT
      if (IRIVRFACT.gt.0) then                                           ! RFACT
         if (HNEW(IC,IR,IL).LE.HRIV) then                                ! RFACT
            ! situation with infiltration, apply infiltration factor     ! RFACT
            CRIV=CRIV*RIVR(IRIVRFACT,L)                                  ! RFACT
         endif                                                           ! RFACT
      endif                                                              ! RFACT
C
C6------COMPARE AQUIFER HEAD TO BOTTOM OF STREAM BED.
      IF(HNEW(IC,IR,IL).LE.RRBOT)GO TO 96
C
C7------SINCE HEAD>BOTTOM ADD TERMS TO RHS AND HCOF.
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-CRIV*HRIV
      HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-CRIV
      GO TO 100
C
C8------SINCE HEAD<BOTTOM ADD TERM ONLY TO RHS.
   96 RHS(IC,IR,IL)=RHS(IC,IR,IL)-CRIV*(HRIV-RBOT)
  100 CONTINUE
C
C9------RETURN
      RETURN
      END
      SUBROUTINE GWF2RIV7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR RIVERS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,HNEW,BUFF
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,IAUXSV,DELT,PERTIM,TOTIM,
     1                      VBVL,VBNM
      USE GWFRIVMODULE,ONLY:NRIVER,IRIVCB,RIVR,NRIVVL,RIVAUX
     1                      ,IRIVRFACT                                  ! RFACT
     1                      ,nrivsubsys,rivsubsidx,irivsubsys           ! rsubsys
C
      DOUBLE PRECISION HHNEW,CHRIV,RRBOT,CCRIV,RATIN,RATOUT,RRATE
      CHARACTER*16 TEXT
      character*16 htxt                                                 ! rsubsys
      DATA TEXT /'   RIVER LEAKAGE'/
      DATA htxt /'RIV LEAKAGE     '/                                    ! rsubsys
      integer  isub,lbeg,lend                                           ! rsubsys
C     ------------------------------------------------------------------
      CALL SGWF2RIV7PNT(IGRID)
C
C
C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C1------ACCUMULATORS (RATIN AND RATOUT).
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
      IBD=0
      IF(IRIVCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IRIVCB.GT.0) IBD=ICBCFL
      IBDLBL=0
C
C4------IF NO REACHES, SKIP FLOW CALCULATIONS.
      IF(NRIVER.EQ.0)GO TO 200

c loop for subsystems
      lend=0                                                            ! rsubsys
      do isub=1,nrivsubsys                                              ! rsubsys

C3------CLEAR THE BUFFER.
         DO 50 IL=1,NLAY
         DO 50 IR=1,NROW
         DO 50 IC=1,NCOL
         BUFF(IC,IR,IL)=ZERO
50       CONTINUE

         lbeg=lend+1                                                    ! rsubsys
         lend=rivsubsidx(isub)                                          ! rsubsys

         ! create text
         if (irivsubsys.gt.0) then                                      ! rsubsys
            ! add number of subsystem to text
            write(htxt(14:16),'(i3)') int(rivr(irivsubsys,lbeg))        ! rsubsys
         else                                                           ! rsubsys
            ! use standard text
            htxt=text                                                   ! rsubsys
         endif                                                          ! rsubsys

C2------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD.EQ.2) THEN
         NAUX=NRIVVL-7
         IF(IAUXSV.EQ.0) NAUX=0
         CALL UBDSV4(KSTP,KPER,htxt,NAUX,RIVAUX,IRIVCB,NCOL,NROW,NLAY,
     1          NRIVER,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C5------LOOP THROUGH EACH RIVER REACH CALCULATING FLOW.
c      DO 100 L=1,NRIVER
      DO 100 L=lbeg,lend                                                ! rsubsys
C
C5A-----GET LAYER, ROW & COLUMN OF CELL CONTAINING REACH.
      IL=RIVR(1,L)
      IR=RIVR(2,L)
      IC=RIVR(3,L)
      RATE=ZERO
C
C5B-----IF CELL IS NO-FLOW OR CONSTANT-HEAD MOVE ON TO NEXT REACH.
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 99
C
C5C-----GET RIVER PARAMETERS FROM RIVER LIST.
      HRIV=RIVR(4,L)
      CRIV=RIVR(5,L)
      RBOT=RIVR(6,L)
      RRBOT=RBOT
      HHNEW=HNEW(IC,IR,IL)
c ------APPLY INFILTRATION FACTOR                                        ! RFACT
      if (IRIVRFACT.gt.0) then                                           ! RFACT
         if (HNEW(IC,IR,IL).LE.HRIV) then                                ! RFACT
            ! situation with infiltration, apply infiltration factor     ! RFACT
            CRIV=CRIV*RIVR(IRIVRFACT,L)                                  ! RFACT
         endif                                                           ! RFACT
      endif                                                              ! RFACT
C
C5D-----COMPARE HEAD IN AQUIFER TO BOTTOM OF RIVERBED.
      IF(HHNEW.GT.RRBOT) THEN
C
C5E-----AQUIFER HEAD > BOTTOM THEN RATE=CRIV*(HRIV-HNEW).
         CCRIV=CRIV
         CHRIV=CRIV*HRIV
         RRATE=CHRIV - CCRIV*HHNEW
         RATE=RRATE
C
C5F-----AQUIFER HEAD < BOTTOM THEN RATE=CRIV*(HRIV-RBOT).
      ELSE
         RATE=CRIV*(HRIV-RBOT)
         RRATE=RATE
      END IF
C
C5G-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IRIVCB<0).
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) WRITE(IOUT,61) TEXT,KPER,KSTP
   61    FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
         WRITE(IOUT,62) L,IL,IR,IC,RATE
   62    FORMAT(1X,'REACH ',I6,'   LAYER ',I3,'   ROW ',I5,'   COL ',I5,
     1       '   RATE',1PG15.6)
         IBDLBL=1
      END IF
C
C5H------ADD RATE TO BUFFER.
      BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+RATE
C
C5I-----SEE IF FLOW IS INTO AQUIFER OR INTO RIVER.
      IF(RATE.LT.ZERO) THEN
C
C5J-----AQUIFER IS DISCHARGING TO RIVER SUBTRACT RATE FROM RATOUT.
        RATOUT=RATOUT-RRATE
      ELSE
C
C5K-----AQUIFER IS RECHARGED FROM RIVER; ADD RATE TO RATIN.
        RATIN=RATIN+RRATE
      END IF
C
C5L-----IF SAVING CELL-BY-CELL FLOWS IN A LIST, WRITE FLOW.  ALSO
C5L-----COPY FLOW TO RIVR.
   99 IF(IBD.EQ.2) CALL UBDSVB(IRIVCB,NCOL,NROW,IC,IR,IL,RATE,
     1                  RIVR(:,L),NRIVVL,NAUX,7,IBOUND,NLAY)
      RIVR(NRIVVL,L)=RATE
  100 CONTINUE
C
C6------IF CELL-BY-CELL FLOW WILL BE SAVED AS A 3-D ARRAY,
C6------CALL UBUDSV TO SAVE THEM.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,htxt,IRIVCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C
      enddo ! isub-loop                                                 ! rsubsys

C7------MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  200 RIN=RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(4,MSUM)=ROUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C8------INCREMENT BUDGET TERM COUNTER.
      MSUM=MSUM+1
C
C9------RETURN.
      RETURN
      END
      SUBROUTINE GWF2RIV7DA(IGRID)
C  Deallocate RIV MEMORY
      USE GWFRIVMODULE
C
        CALL SGWF2RIV7PNT(IGRID)
        DEALLOCATE(NRIVER)
        DEALLOCATE(MXRIVR)
        DEALLOCATE(NRIVVL)
        DEALLOCATE(IRIVCB)
        DEALLOCATE(IPRRIV)
        DEALLOCATE(NPRIV)
        DEALLOCATE(IRIVPB)
        DEALLOCATE(NNPRIV)
        DEALLOCATE(RIVAUX)
        DEALLOCATE(RIVR)
        DEALLOCATE(IRIVRFACT)                                           ! RFACT
        deallocate(irivsubsys)                                          ! rsubsys
        deallocate(nrivsubsys)                                          ! rsubsys
        deallocate(rivsubsidx)                                          ! rsubsys
        deallocate(irivrconc)                                           ! rconc
        deallocate(lreuse)
        deallocate(ifvdl)                                               ! ifvdl
        deallocate(isft)                                                ! ifvdl
        deallocate(sft)                                                 ! ifvdl
C
      RETURN
      END
      SUBROUTINE SGWF2RIV7PNT(IGRID)
C  Change river data to a different grid.
      USE GWFRIVMODULE
C
        NRIVER=>GWFRIVDAT(IGRID)%NRIVER
        MXRIVR=>GWFRIVDAT(IGRID)%MXRIVR
        NRIVVL=>GWFRIVDAT(IGRID)%NRIVVL
        IRIVCB=>GWFRIVDAT(IGRID)%IRIVCB
        IPRRIV=>GWFRIVDAT(IGRID)%IPRRIV
        NPRIV=>GWFRIVDAT(IGRID)%NPRIV
        IRIVPB=>GWFRIVDAT(IGRID)%IRIVPB
        NNPRIV=>GWFRIVDAT(IGRID)%NNPRIV
        RIVAUX=>GWFRIVDAT(IGRID)%RIVAUX
        RIVR=>GWFRIVDAT(IGRID)%RIVR
        IRIVRFACT=>GWFRIVDAT(IGRID)%IRIVRFACT                           ! RFACT
        irivsubsys=>gwfrivdat(igrid)%irivsubsys                         ! rsubsys
        nrivsubsys=>gwfrivdat(igrid)%nrivsubsys                         ! rsubsys
        rivsubsidx=>gwfrivdat(igrid)%rivsubsidx                         ! rsubsys
        irivrconc=>gwfrivdat(igrid)%irivrconc                           ! rconc
        lreuse=>gwfrivdat(igrid)%lreuse                                 ! iconchk
        ifvdl=>gwfrivdat(igrid)%ifvdl                                   ! ifvdl
        isft=>gwfrivdat(igrid)%isft                                     ! ifvdl
        sft=>gwfrivdat(igrid)%sft                                       ! ifvdl
C
      RETURN
      END
      SUBROUTINE SGWF2RIV7PSV(IGRID)
C  Save river data for a grid.
      USE GWFRIVMODULE
C
        GWFRIVDAT(IGRID)%NRIVER=>NRIVER
        GWFRIVDAT(IGRID)%MXRIVR=>MXRIVR
        GWFRIVDAT(IGRID)%NRIVVL=>NRIVVL
        GWFRIVDAT(IGRID)%IRIVCB=>IRIVCB
        GWFRIVDAT(IGRID)%IPRRIV=>IPRRIV
        GWFRIVDAT(IGRID)%NPRIV=>NPRIV
        GWFRIVDAT(IGRID)%IRIVPB=>IRIVPB
        GWFRIVDAT(IGRID)%NNPRIV=>NNPRIV
        GWFRIVDAT(IGRID)%RIVAUX=>RIVAUX
        GWFRIVDAT(IGRID)%RIVR=>RIVR
        GWFRIVDAT(IGRID)%IRIVRFACT=>IRIVRFACT                           ! RFACT
        gwfrivdat(igrid)%irivsubsys=>irivsubsys                         ! rsubsys
        gwfrivdat(igrid)%nrivsubsys=>nrivsubsys                         ! rsubsys
        gwfrivdat(igrid)%rivsubsidx=>rivsubsidx                         ! rsubsys
        gwfrivdat(igrid)%irivrconc=>irivrconc                           ! rconc
        gwfrivdat(igrid)%lreuse=>lreuse                                 ! iconchk
        gwfrivdat(igrid)%ifvdl=>ifvdl                                   ! ifvdl
        gwfrivdat(igrid)%isft=>isft                                     ! ifvdl
        gwfrivdat(igrid)%sft=>sft                                       ! ifvdl
C
      RETURN
      END
