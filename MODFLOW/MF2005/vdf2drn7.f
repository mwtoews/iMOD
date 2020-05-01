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

      SUBROUTINE VDF2DRN7FM(IGRID)
C     ******************************************************************
C     ADD DRAIN FLOW TO SOURCE TERM
C--SEAWAT: MODIFIED FOR VD FLOW
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFMODULE,    ONLY:DENSEREF,PS,ELEV
      USE GLOBAL,       ONLY:IBOUND,HNEW,RHS,HCOF
      USE GWFDRNMODULE, ONLY:NDRAIN,DRAI,
     1                       iiconchk
      USE GWFRIVMODULE, ONLY:NRIVER,RIVR
C
      DOUBLE PRECISION EEL
C--SEAWAT: ADD EL
      DOUBLE PRECISION EL,HHNEW
C--SEAWAT: INCLUDE AUXILIARY VARIABLES
      COMMON /DRNCOM/DRNAUX(5)
      CHARACTER*16 DRNAUX
C     ------------------------------------------------------------------
      CALL SGWF2DRN7PNT(IGRID)
C
C1------IF NDRAIN<=0 THERE ARE NO DRAINS. RETURN.
      IF(NDRAIN.LE.0) RETURN
C
C--SEAWAT: GET ZDRN IF AUX IS SPECIFIED
        LOCZDRN=0
        DO I=1,5
            IF(DRNAUX(I).EQ.'DRNBELEV') LOCZDRN=I+5
        ENDDO
C2------PROCESS EACH CELL IN THE DRAIN LIST.
      DO 100 L=1,NDRAIN
C
C3------GET COLUMN, ROW AND LAYER OF CELL CONTAINING DRAIN.
      IL=DRAI(1,L)
      IR=DRAI(2,L)
      IC=DRAI(3,L)
C
      if (iiconchk.gt.0) then                                           ! iconchk
       if(drai(5,l).le.0.0)goto 100 
      end if                                                            ! iconchk
C
C4-------IF THE CELL IS EXTERNAL SKIP IT.
        IF(IBOUND(IC,IR,IL).LE.0) GO TO 100
C
C5-------IF THE CELL IS INTERNAL GET THE DRAIN DATA.
        EL=DRAI(4,L)
        ZDRN=ELEV(IC,IR,IL)
        IF(LOCZDRN.GT.0) ZDRN=DRAI(LOCZDRN,L)
C--SEAWAT: MAKE EEL EQUIVALENT FRESHWATER
        EEL=FEHEAD(EL,PS(IC,IR,IL),ZDRN)
C
C6------IF HEAD IS LOWER THAN DRAIN THEN SKIP THIS CELL.
C--SEAWAT:USE SALTHEAD COMPARISONS
        IF(SALTHEAD(HNEW(IC,IR,IL),PS(IC,IR,IL),ELEV(IC,IR,IL))
     +               .LE.EL) GO TO 100
C
C7------HEAD IS HIGHER THAN DRAIN. ADD TERMS TO RHS AND HCOF.
C--SEAWAT: USE VD FORM,CONSERVE MASS
        C=DRAI(5,L)
        HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-C*PS(IC,IR,IL)
        RHS(IC,IR,IL)=RHS(IC,IR,IL)-C*PS(IC,IR,IL)*
     &                  (EEL-(PS(IC,IR,IL)-DENSEREF)/DENSEREF*
     &                  (ELEV(IC,IR,IL)-ZDRN))

  100 CONTINUE
C
C8------RETURN.
      RETURN
      END
      SUBROUTINE VDF2DRN7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR DRAINS
C-----SEAWAT: ADJUSTED FOR DENSITY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,HNEW,BUFF
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,IAUXSV,DELT,PERTIM,TOTIM,
     1                      VBVL,VBNM
      USE GWFDRNMODULE,ONLY:NDRAIN,IDRNCB,DRAI,NDRNVL,DRNAUX
     1                      ,ndrnsubsys,drnsubsidx,idrnsubsys,          ! dsubsys
     1                       iiconchk
      USE VDFMODULE,   ONLY: DENSEREF,PS,ELEV
    
C
      CHARACTER*16 TEXT
      character*16 htxt                                                 ! dsubsys
C--SEAWAT: ADD EL      
      DOUBLE PRECISION HHNEW,EEL,CCDRN,CEL,RATOUT,QQ,EL
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
         if (idrnsubsys.gt.0) then                                      ! dsubsys
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
         if(drai(5,l).le.0.0)goto 100
      end if                                                            ! iconchk
C
      Q=ZERO
C
C5B-----IF CELL IS NO-FLOW OR CONSTANT-HEAD, IGNORE IT.
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 99
C
C5C-----GET DRAIN PARAMETERS FROM DRAIN LIST.
      EL=DRAI(4,L)
      ZDRN=ELEV(IC,IR,IL)
      EEL=FEHEAD(EL,PS(IC,IR,IL),ZDRN)
      C=DRAI(5,L)
C     HHNEW=HNEW(IC,IR,IL)
C--SEAWAT: USE SALTWATER HEAD FOR TURNING DRAIN ON

C5D-----IF SALTHEAD HIGHER THAN DRAIN, CALCULATE Q=C*(EL-HHNEW).
C5D-----SUBTRACT Q FROM RATOUT.
        IF(SALTHEAD(HNEW(IC,IR,IL),PS(IC,IR,IL),ELEV(IC,IR,IL))
     +                .GT.EL) THEN
            CCDRN=C
C--SEAWAT: VD FORM, CONSERVE MASS
            QQ=PS(IC,IR,IL)*CCDRN*(EEL-HNEW(IC,IR,IL)-
     &           (PS(IC,IR,IL)-DENSEREF)/DENSEREF*(ELEV(IC,IR,IL)-ZDRN))
            Q=QQ
            RATOUT=RATOUT-QQ
        END IF

C
C5E-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IDRNCB<0).
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) WRITE(IOUT,61) TEXT,KPER,KSTP
   61    FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
C--SEAWAT: WRITE VOLUMETRIC FLUX
            WRITE(IOUT,62) L,IL,IR,IC,Q/PS(IC,IR,IL)
   62       FORMAT(1X,'DRAIN',I4,'   LAYER',I3,'   ROW',I5,'   COL',
     1             I5,'   RATE',1PG15.6)
            IBDLBL=1
      END IF
C
C5F-----ADD Q TO BUFFER.
C--SEAWAT: SAVE AS VOLUMETRIC FLUX
      Q=Q/PS(IC,IR,IL)
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
      SUBROUTINE VDF2DRN7DA(IGRID)
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
        if (associated(ndrnsubsys)) deallocate(ndrnsubsys)              ! dsubsys
        if (associated(drnsubsidx)) deallocate(drnsubsidx)              ! dsubsys
        deallocate(iiconchk)                                            ! iconchk
C
      RETURN
      END
      SUBROUTINE SVDF2DRN7PNT(IGRID)
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
C
      RETURN
      END
      SUBROUTINE SVDF2DRN7PSV(IGRID)
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
C
      RETURN
      END
