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
      SUBROUTINE VDF2RIV7FM(IGRID)
C     ******************************************************************
C     ADD RIVER TERMS TO RHS AND HCOF
C--C--SEAWAT: MODIFIED TO USE A VARIABLE DENSITY FORM OF DARCY'S LAW
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IBOUND,HNEW,RHS,HCOF
      USE GWFRIVMODULE, ONLY:NRIVER,RIVR
     1                      ,IRIVRFACT,RIVAUX                           ! RFACT
      USE VDFMODULE,   ONLY: DENSEREF,PS,ELEV
C
C--SEAWAT: ADD HRIV
      DOUBLE PRECISION RRBOT,HRIV
C--SEAWAT: NEED AUXILIARY VARIABLES
!      COMMON /RIVCOM/RIVAUX(5)
!      CHARACTER*16 RIVAUX      
C     ------------------------------------------------------------------
      CALL SGWF2RIV7PNT(IGRID)
C
C1------IF NRIVER<=0 THERE ARE NO RIVERS. RETURN.
      IF(NRIVER.LE.0)RETURN
C      
C--SEAWAT: FIND RBDTHK AND RIVDEN IF EXIST AS AUX VARIABLES
      LOCRIVDEN=0
      DO I=1,5
        IF(RIVAUX(I).EQ.'RIVDEN') LOCRIVDEN=I+6
      ENDDO   
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
        RBDTHK=ABS(RBOT-ELEV(IC,IR,IL))
        RIVDENS=PS(IC,IR,IL)
        IF(LOCRIVDEN.GT.0) RIVDENS=RIVR(LOCRIVDEN,L)
        HRIV=FEHEAD(HRIV,RIVDENS,RBOT+RBDTHK)
        HHNEW=HNEW(IC,IR,IL)
C--SEAWAT: CALCULATE FRESHWATER HEAD AT RBOT USING HEAD IN MODEL CELL
        HFRBOT=HHNEW+(PS(IC,IR,IL)-DENSEREF)/DENSEREF*
     &      (ELEV(IC,IR,IL)-RBOT)
C
C6------COMPARE AQUIFER HEAD TO BOTTOM OF STREAM BED.
C--SEAWAT: USE SALTHEAD FOR THIS COMPARISON
        IF(SALTHEAD(HNEW(IC,IR,IL),PS(IC,IR,IL),ELEV(IC,IR,IL))
     +           .LE.RRBOT) GO TO 96
        RHOAVG=(RIVDENS+PS(IC,IR,IL))/2
        DIRECT=-((HRIV-HFRBOT)+(PS(IC,IR,IL)-DENSEREF)/
     +            DENSEREF*RBDTHK)
C--SEAWAT: DIRECT IS POSITIVE, FLOW IS UP INTO RIVER
        IF (DIRECT.GT.0.) RIVDENS=PS(IC,IR,IL)
C7------SINCE HEAD>BOTTOM ADD TERMS TO RHS AND HCOF.
C--SEAWAT: USE VARIABLE DENSITY FORM OF DARCY'S LAW, CONSERVE MASS
        RHS(IC,IR,IL)=RHS(IC,IR,IL)-CRIV*RIVDENS*
     &                (HRIV-(PS(IC,IR,IL)-DENSEREF)/DENSEREF*
     &                  (ELEV(IC,IR,IL)-RBOT)+(RHOAVG-DENSEREF)/
     +                  DENSEREF*RBDTHK)
        HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-CRIV*RIVDENS
        GO TO 100
C8------SINCE HEAD<BOTTOM ADD TERM ONLY TO RHS.
C--SEAWAT: USE VARIABLE DENSITY FORM OF DARCY'S LAW, CONSERVE MASS
   96   RHS(IC,IR,IL)=RHS(IC,IR,IL)-CRIV*RIVDENS*
     &                  (HRIV-RBOT+(RIVDENS-DENSEREF)/DENSEREF*RBDTHK)
  100 CONTINUE
C
C9------RETURN
      RETURN
      END
      
      SUBROUTINE VDF2RIV7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR RIVERS
C--SEAWAT: USE VARIABLE-DENSITY EQUATIONS
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
      USE VDFMODULE,   ONLY:DENSEREF,PS,ELEV
C--SEAWAT: ADD HRIV
      DOUBLE PRECISION HHNEW,CHRIV,RRBOT,CCRIV,RATIN,RATOUT,RRATE,HRIV
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
C
C--SEAWAT: FIND AUX VARIABLES FOR RIVDEN IF EXISTS
      LOCRIVDEN=0
      DO I=1,5
        IF(RIVAUX(I).EQ.'RIVDEN') LOCRIVDEN=I+6
      ENDDO

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
        RBDTHK=ABS(RBOT-ELEV(IC,IR,IL))
        RIVDENS=PS(IC,IR,IL)
        IF(LOCRIVDEN.GT.0) RIVDENS=RIVR(LOCRIVDEN,L)
        HRIV=FEHEAD(HRIV,RIVDENS,RBOT+RBDTHK)
        HHNEW=HNEW(IC,IR,IL)
        HTEMP=SALTHEAD(HHNEW,PS(IC,IR,IL),ELEV(IC,IR,IL))
        HFRBOT=HHNEW+(PS(IC,IR,IL)-DENSEREF)/DENSEREF*
     &         (ELEV(IC,IR,IL)-RBOT)

c ------APPLY INFILTRATION FACTOR                                        ! RFACT
      if (IRIVRFACT.gt.0) then                                           ! RFACT
         if (HTEMP.LE.HRIV) then                                         ! RFACT
            ! situation with infiltration, apply infiltration factor     ! RFACT
            CRIV=CRIV*RIVR(IRIVRFACT,L)                                  ! RFACT
         endif                                                           ! RFACT
      endif                                                              ! RFACT
C
C--SEAWAT: COMPARISON DONE WITH SALTHEAD
C5D-----COMPARE HEAD IN AQUIFER TO BOTTOM OF RIVERBED.
        IF(HTEMP.GT.RRBOT) THEN
            RHOAVG=(RIVDENS+PS(IC,IR,IL))/2
            DIRECT=-(HRIV-HFRBOT+(RHOAVG-DENSEREF)/DENSEREF*RBDTHK)
C--SEAWAT:  DIRECT IS POSITIVE, FLOW IS UP INTO RIVER
            IF (DIRECT.GT.0.) RIVDENS=PS(IC,IR,IL)
            RATE=CRIV*RIVDENS*(HRIV-HFRBOT+(RHOAVG-DENSEREF)/
     &           DENSEREF*RBDTHK)
            RRATE=RATE
        ENDIF
C5F-----AQUIFER HEAD < BOTTOM THEN RATE=CRIV*(HRIV-RBOT).
        IF(HTEMP.LE.RRBOT) THEN
            RATE=CRIV*RIVDENS*(HRIV-RBOT+(RIVDENS-DENSEREF)/
     +           DENSEREF*RBDTHK)
            RRATE=RATE
        ENDIF
C
C5G-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IRIVCB<0).
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) WRITE(IOUT,61) TEXT,KPER,KSTP
   61    FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
C--SEAWAT: WRITE VOLUMETRIC FLUX
            WRITE(IOUT,62) L,IL,IR,IC,RATE/RIVDENS
   62       FORMAT(1X,'REACH',I4,'   LAYER',I3,'   ROW',I5,'   COL',
     +             I5,'   RATE',1PG15.6)
            IBDLBL=1
        END IF
C
C5H------ADD RATE TO BUFFER.
C--SEAWAT: CONVERT RATE TO VOLUMETRIC FLUX FOR CBB OUTPUT 
C--SEAWAT: RRATE REMAINS MASS FLUX
        RATE=RATE/RIVDENS

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
      SUBROUTINE VDF2RIV7DA(IGRID)
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
        if (associated(nrivsubsys)) deallocate(nrivsubsys)              ! rsubsys
        if (associated(rivsubsidx)) deallocate(rivsubsidx)              ! rsubsys
        deallocate(irivrconc)                                           ! rconc
        deallocate(ifvdl)                                               ! ifvdl
        deallocate(isft)                                                ! ifvdl
        deallocate(sft)                                                 ! ifvdl
C
      RETURN
      END
      SUBROUTINE SVDF2RIV7PNT(IGRID)
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
        ifvdl=>gwfrivdat(igrid)%ifvdl                                   ! ifvdl
        isft=>gwfrivdat(igrid)%isft                                     ! ifvdl
        sft=>gwfrivdat(igrid)%sft                                       ! ifvdl
C
      RETURN
      END
      SUBROUTINE SVDF2RIV7PSV(IGRID)
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
        gwfrivdat(igrid)%ifvdl=>ifvdl                                   ! ifvdl
        gwfrivdat(igrid)%isft=>isft                                     ! ifvdl
        gwfrivdat(igrid)%sft=>sft                                       ! ifvdl
C
      RETURN
      END
