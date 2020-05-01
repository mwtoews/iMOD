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

      SUBROUTINE VDF2RCH7FM(IGRID)
C     ******************************************************************
C     SUBTRACT RECHARGE FROM RHS
C--SEAWAT: REFORMULATED FOR VD FLOW 
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,RHS
      USE GWFRCHMODULE,ONLY:NRCHOP,RECH,IRCH
      USE VDFMODULE,   ONLY:DENSEREF,PS    
C     ------------------------------------------------------------------
C
C1------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2RCH7PNT(IGRID)
C
C2------DETERMINE WHICH RECHARGE OPTION.
      IF(NRCHOP.EQ.1) THEN
C
C3------NRCHOP IS 1, SO RECHARGE IS IN TOP LAYER. LAYER INDEX IS 1.
        DO 10 IR=1,NROW
        DO 10 IC=1,NCOL
C
C3A-----IF CELL IS VARIABLE HEAD, SUBTRACT RECHARGE RATE FROM
C3A-----RIGHT-HAND-SIDE.
        IF(IBOUND(IC,IR,1).GT.0) THEN
         DENSE=DENSEREF
         IF(RECH(IC,IR).LT.0) DENSE=PS(IC,IR,1)
C
C1B-----SUBTRACT RECHARGE RATE FROM RIGHT-HAND-SIDE.
C--SEAWAT: CONSERVE MASS
C        RHS(IC,IR,1)=RHS(IC,IR,1)-RECH(IC,IR)
         RHS(IC,IR,1)=RHS(IC,IR,1)-RECH(IC,IR)*DENSE
        ENDIF
   10   CONTINUE
      ELSE IF(NRCHOP.EQ.2) THEN
C
C4------NRCHOP IS 2, SO RECHARGE IS INTO LAYER IN INDICATOR ARRAY
        DO 20 IR=1,NROW
        DO 20 IC=1,NCOL
        IL=IRCH(IC,IR)
C
C4A-----IF THE CELL IS VARIABLE HEAD, SUBTRACT RECHARGE FROM
C4A-----RIGHT-HAND-SIDE.
        IF(IL.EQ.0) GO TO 20
        IF(IBOUND(IC,IR,IL).GT.0)THEN
         DENSE=DENSEREF
         IF(RECH(IC,IR).LT.0) DENSE=PS(IC,IR,IL)
C
C2C-----SUBTRACT RECHARGE FROM RIGHT-HAND-SIDE.
C--SEAWAT:CONSERVE MASS
C        RHS(IC,IR,IL)=RHS(IC,IR,IL)-RECH(IC,IR)
         RHS(IC,IR,IL)=RHS(IC,IR,IL)-RECH(IC,IR)*DENSE
        ENDIF
   20   CONTINUE
      ELSE
C
C5------NRCHOP IS 3, RECHARGE IS INTO HIGHEST VARIABLE-HEAD CELL, EXCEPT
C5------CANNOT PASS THROUGH CONSTANT HEAD NODE
        DO 30 IR=1,NROW
        DO 30 IC=1,NCOL
        DO 28 IL=1,NLAY
C
C5A-----IF CELL IS CONSTANT HEAD MOVE ON TO NEXT HORIZONTAL LOCATION.
        IF(IBOUND(IC,IR,IL).LT.0) GO TO 30
C
C5B-----IF THE CELL IS VARIABLE HEAD, SUBTRACT RECHARGE FROM
C5B-----RIGHT-HAND-SIDE AND MOVE TO NEXT HORIZONTAL LOCATION.
        IF(IBOUND(IC,IR,IL).GT.0) THEN
          DENSE=DENSEREF
          IF(RECH(IC,IR).LT.0) DENSE=PS(IC,IR,IL)
C
C3C-----SUBTRACT RECHARGE FROM RIGHT-HAND-SIDE.
C--SEAWAT:CONSERVE MASS
C         RHS(IC,IR,IL)=RHS(IC,IR,IL)-RECH(IC,IR)
          RHS(IC,IR,IL)=RHS(IC,IR,IL)-RECH(IC,IR)*DENSE
          GO TO 30
        END IF
   28   CONTINUE
   30   CONTINUE
      END IF
C
C6------RETURN
      RETURN
      END
      SUBROUTINE VDF2RCH7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR RECHARGE
C--SEAWAT: USE VARIABLE-DENSITY EQUATIONS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,BUFF
      USE GWFBASMODULE,ONLY:MSUM,VBVL,VBNM,ICBCFL,DELT,PERTIM,TOTIM
      USE GWFRCHMODULE,ONLY:NRCHOP,IRCHCB,RECH,IRCH
      USE VDFMODULE,   ONLY: DENSEREF,PS      
C
      DOUBLE PRECISION RATIN,RATOUT,QQ
      CHARACTER*16 TEXT
      DATA TEXT /'        RECHARGE'/
C     ------------------------------------------------------------------
C
C1------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2RCH7PNT(IGRID)
C
C2------CLEAR THE RATE ACCUMULATORS.
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
C
C3------CLEAR THE BUFFER & SET FLAG FOR SAVING CELL-BY-CELL FLOW TERMS.
      DO 2 IL=1,NLAY
      DO 2 IR=1,NROW
      DO 2 IC=1,NCOL
      BUFF(IC,IR,IL)=ZERO
    2 CONTINUE
      IBD=0
      IF(IRCHCB.GT.0) IBD=ICBCFL
C
C4------DETERMINE THE RECHARGE OPTION.
      IF(NRCHOP.EQ.1) THEN
C
C5------NRCHOP=1, SO RECH GOES INTO LAYER 1. PROCESS EACH HORIZONTAL
C5------CELL LOCATION.
        DO 10 IR=1,NROW
        DO 10 IC=1,NCOL
C
C5A-----IF CELL IS VARIABLE HEAD, THEN DO BUDGET FOR IT.
        IF(IBOUND(IC,IR,1).GT.0) THEN
         DENSE=DENSEREF
         IF(RECH(IC,IR).LT.0) DENSE=PS(IC,IR,1)

C--SEAWAT:CONSERVE MASS
C        Q=RECH(IC,IR)
         Q=RECH(IC,IR)*DENSE

         QQ=Q
C
C5B-----ADD RECH TO BUFF.
C--SEAWAT:CONVERT TO VOLUMETRIC FLUX FOR OUTPUT
C        BUFF(IC,IR,1)=Q
         BUFF(IC,IR,1)=Q/DENSE

C
C5C-----IF RECH POSITIVE ADD IT TO RATIN, ELSE ADD IT TO RATOUT.
         IF(Q.GE.ZERO) THEN
            RATIN=RATIN+QQ
         ELSE
            RATOUT=RATOUT-QQ
         END IF
        END IF
   10   CONTINUE
      ELSE IF(NRCHOP.EQ.2) THEN
C
C6------NRCHOP=2, RECH IS IN LAYER SPECIFIED IN INDICATOR ARRAY(IRCH).
C6------PROCESS EACH HORIZONTAL CELL LOCATION.
        DO 20 IR=1,NROW
        DO 20 IC=1,NCOL
C
C6A-----GET LAYER INDEX FROM INDICATOR ARRAY(IRCH).
        IL=IRCH(IC,IR)
C
C6B-----IF CELL IS VARIABLE HEAD, THEN DO BUDGET FOR IT.
        IF(IL.EQ.0) GO TO 20
        IF(IBOUND(IC,IR,IL).GT.0) THEN
         DENSE=DENSEREF
         IF(RECH(IC,IR).LT.0) DENSE=PS(IC,IR,IL)
C--SEAWAT:CONSERVE MASS
C        Q=RECH(IC,IR)
         Q=RECH(IC,IR)*DENSE
         QQ=Q
C
C6C-----ADD RECHARGE TO BUFF.
C--SEAWAT: CONVERT TO VOLUMETRIC FLUX 
C        BUFF(IC,IR,IL)=Q
         BUFF(IC,IR,IL)=Q/DENSE

C
C6D-----IF RECHARGE IS POSITIVE ADD TO RATIN, ELSE ADD IT TO RATOUT.
          IF(Q.GE.ZERO) THEN
            RATIN=RATIN+QQ
          ELSE
            RATOUT=RATOUT-QQ
          END IF
        END IF
   20   CONTINUE
      ELSE
C
C7------NRCHOP=3; RECHARGE IS INTO HIGHEST CELL IN A VERTICAL COLUMN
C7------THAT IS NOT NO FLOW.  PROCESS EACH HORIZONTAL CELL LOCATION.
        DO 30 IR=1,NROW
        DO 29 IC=1,NCOL
C
C7A-----INITIALIZE IRCH TO 1, AND LOOP THROUGH CELLS IN A VERTICAL
C7A-----COLUMN TO FIND WHERE TO PLACE RECHARGE.
        IRCH(IC,IR)=1
        DO 28 IL=1,NLAY
C
C7B-----IF CELL IS CONSTANT HEAD, MOVE ON TO NEXT HORIZONTAL LOCATION.
        IF(IBOUND(IC,IR,IL).LT.0) GO TO 29
C
C7C-----IF CELL IS VARIABLE HEAD, THEN DO BUDGET FOR IT.
        IF (IBOUND(IC,IR,IL).GT.0) THEN
C--SEAWAT: SET DENSE
            DENSE=DENSEREF
            IF(RECH(IC,IR).LT.0) DENSE=PS(IC,IR,IL)
C--SEAWAT:CONSERVE MASS
C           Q=RECH(IC,IR)
            Q=RECH(IC,IR)*DENSE
            QQ=Q
C--SEAWAT:CONVERT TO VOLUMETRIC FLUX
C           BUFF(IC,IR,IL)=Q
            BUFF(IC,IR,IL)=Q/DENSE

            IRCH(IC,IR)=IL
C
C7E-----IF RECH IS POSITIVE ADD IT TO RATIN, ELSE ADD IT TO RATOUT.
          IF(Q.GE.ZERO) THEN
            RATIN=RATIN+QQ
          ELSE
            RATOUT=RATOUT-QQ
          END IF
          GO TO 29
        END IF
28      CONTINUE
29      CONTINUE
30      CONTINUE
C
      END IF
C
C8------IF CELL-BY-CELL FLOW TERMS SHOULD BE SAVED, CALL APPROPRIATE
C8------UTILITY MODULE TO WRITE THEM.
100   IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IRCHCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV3(KSTP,KPER,TEXT,IRCHCB,BUFF,IRCH,NRCHOP,
     1                   NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
C
C9------MOVE TOTAL RECHARGE RATE INTO VBVL FOR PRINTING BY BAS1OT.
      ROUT=RATOUT
      RIN=RATIN
      VBVL(4,MSUM)=ROUT
      VBVL(3,MSUM)=RIN
C
C10-----ADD RECHARGE FOR TIME STEP TO RECHARGE ACCUMULATOR IN VBVL.
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
C
C11-----MOVE BUDGET TERM LABELS TO VBNM FOR PRINT BY MODULE BAS_OT.
      VBNM(MSUM)=TEXT
C
C12-----INCREMENT BUDGET TERM COUNTER.
      MSUM=MSUM+1
C
C13-----RETURN
      RETURN
      END
      SUBROUTINE VDF2RCH7DA(IGRID)
C  Deallocate RCH DATA
      USE GWFRCHMODULE
C
        DEALLOCATE(GWFRCHDAT(IGRID)%NRCHOP)
        DEALLOCATE(GWFRCHDAT(IGRID)%IRCHCB)
        DEALLOCATE(GWFRCHDAT(IGRID)%NPRCH)
        DEALLOCATE(GWFRCHDAT(IGRID)%IRCHPF)
        DEALLOCATE(GWFRCHDAT(IGRID)%RECH)
        if(associated(gwfrchdat(igrid)%rechbuff))                       ! DLT
     1     deallocate(gwfrchdat(igrid)%rechbuff)                        ! DLT
        DEALLOCATE(GWFRCHDAT(IGRID)%IRCH)
        DEALLOCATE(GWFRCHDAT(IGRID)%IADDRECH)                           ! DLT
C
      RETURN
      END
      SUBROUTINE SVDF2RCH7PNT(IGRID)
C  Set RCH pointers for grid.
      USE GWFRCHMODULE
C
        NRCHOP=>GWFRCHDAT(IGRID)%NRCHOP
        IRCHCB=>GWFRCHDAT(IGRID)%IRCHCB
        NPRCH=>GWFRCHDAT(IGRID)%NPRCH
        IRCHPF=>GWFRCHDAT(IGRID)%IRCHPF
        RECH=>GWFRCHDAT(IGRID)%RECH
        RECHBUFF=>GWFRCHDAT(IGRID)%RECHBUFF                             ! DLT
        IRCH=>GWFRCHDAT(IGRID)%IRCH
        IADDRECH=>GWFRCHDAT(IGRID)%IADDRECH                             ! DLT
C
      RETURN
      END
      SUBROUTINE SVDF2RCH7PSV(IGRID)
C  Save RCH pointers for grid.
      USE GWFRCHMODULE
C
        GWFRCHDAT(IGRID)%NRCHOP=>NRCHOP
        GWFRCHDAT(IGRID)%IRCHCB=>IRCHCB
        GWFRCHDAT(IGRID)%NPRCH=>NPRCH
        GWFRCHDAT(IGRID)%IRCHPF=>IRCHPF
        GWFRCHDAT(IGRID)%RECH=>RECH
        GWFRCHDAT(IGRID)%RECHBUFF=>RECHBUFF                             ! DLT
        GWFRCHDAT(IGRID)%IRCH=>IRCH
        GWFRCHDAT(IGRID)%IADDRECH=>IADDRECH                             ! DLT
C
      RETURN
      END
