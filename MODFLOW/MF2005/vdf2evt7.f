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

      SUBROUTINE VDF2EVT7FM(IGRID)
C     ******************************************************************
C     ADD EVAPOTRANSPIRATION TO RHS AND HCOF
C--SEAWAT: INCLUDES VARIABLE-DENSITY MODIFICATIONS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,RHS,HCOF
      USE GWFEVTMODULE,ONLY:NEVTOP,EVTR,EXDP,SURF,IEVT
      USE VDFMODULE,   ONLY: DENSEREF,PS,ELEV
C
      DOUBLE PRECISION HH,SS,XX,DD 
C     ------------------------------------------------------------------
C
C1------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2EVT7PNT(IGRID)
C
C2------PROCESS EACH HORIZONTAL CELL LOCATION
      DO 10 IR=1,NROW
      DO 10 IC=1,NCOL
C
C3------SET THE LAYER INDEX -- FOR OPTION 1, THE LAYER IS 1;
C3------FOR OPTION 2, THE LAYER IS SPECIFIED IN IEVT.
      IF(NEVTOP.EQ.1) THEN
         IL=1
      ELSE IF(NEVTOP.EQ.2) THEN
         IL=IEVT(IC,IR)
         IF(IL.EQ.0) GO TO 10  ! ERB 1/11/07
      ELSE
C
C4------FOR OPTION 3, FIND UPPERMOST ACTIVE CELL.
         DO 3 IL=1,NLAY
        IF(IBOUND(IC,IR,IL).NE.0) GO TO 4
    3    CONTINUE
         IL=1
      END IF
C
C5------IF THE CELL IS EXTERNAL IGNORE IT.
    4 IF(IBOUND(IC,IR,IL).LE.0)GO TO 10
C--SEAWAT: DETERMINE DENSE VALUE OF WITHDRAWN ET FLUID
      DENSE=DENSEREF
      C=EVTR(IC,IR)
      S=SURF(IC,IR)
      SS=S
C      HH=HNEW(IC,IR,IL)
      HH=SALTHEAD(HNEW(IC,IR,IL),PS(IC,IR,IL),ELEV(IC,IR,IL))
C
C6------IF AQUIFER HEAD IS GREATER THAN OR EQUAL TO SURF, ET IS CONSTANT
      IF(HH.LT.SS) GO TO 5
C
C6A-----SUBTRACT -EVTR FROM RHS
C--SEAWAT: CONSERVE MASS
C      RHS(IC,IR,IL)=RHS(IC,IR,IL) + C
      RHS(IC,IR,IL)=RHS(IC,IR,IL) + C*DENSE

      GO TO 10
C
C7------IF DEPTH TO WATER>=EXTINCTION DEPTH THEN ET IS 0
    5 DD=SS-HH
      X=EXDP(IC,IR)
      XX=X
      IF(DD.GE.XX)GO TO 10
C
C8------LINEAR RANGE. ADD ET TERMS TO BOTH RHS AND HCOF.
C--SEAWAT: USE REFORMULATED EQUATION, CONSERVE MASS
C     RHS(IC,IR,IL)=RHS(IC,IR,IL)+C-C*S/X
C     HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-C/X
      RHS(IC,IR,IL)=RHS(IC,IR,IL)+DENSE*C-DENSE*C*S/X
      RHS(IC,IR,IL)=RHS(IC,IR,IL)+DENSE*C/X*(PS(IC,IR,IL)-DENSEREF)/
     +              PS(IC,IR,IL)*ELEV(IC,IR,IL)
      HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-DENSE*DENSEREF/PS(IC,IR,IL)*C/X
   10 CONTINUE
C
C9------RETURN
      RETURN
      END
      
      SUBROUTINE VDF2EVT7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR EVAPOTRANSPIRATION
C--SEAWAT: ADJUSTED FOR DENSITY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,HNEW,BUFF
      USE GWFBASMODULE,ONLY:MSUM,VBVL,VBNM,ICBCFL,DELT,PERTIM,TOTIM
      USE GWFEVTMODULE,ONLY:NEVTOP,IEVTCB,EVTR,EXDP,SURF,IEVT
      USE VDFMODULE,   ONLY:DENSEREF,PS,ELEV      
C
      DOUBLE PRECISION RATOUT,QQ,HH,SS,DD,XX,HHCOF,RRHS

      CHARACTER*16 TEXT
      DATA TEXT /'              ET'/
C     ------------------------------------------------------------------
C
C1------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2EVT7PNT(IGRID)
C
C2------CLEAR THE RATE ACCUMULATOR.
      ZERO=0.
      RATOUT=ZERO
C
C3------CLEAR THE BUFFER & SET CELL-BY-CELL BUDGET SAVE FLAG (IBD).
      DO 2 IL=1,NLAY
      DO 2 IR=1,NROW
      DO 2 IC=1,NCOL
      BUFF(IC,IR,IL)=ZERO
    2 CONTINUE
      IBD=0
      IF(IEVTCB.GT.0) IBD=ICBCFL
C
C4------PROCESS EACH HORIZONTAL CELL LOCATION.
      DO 10 IR=1,NROW
      DO 10 IC=1,NCOL
C
C5------SET THE LAYER INDEX -- FOR OPTION 1, THE LAYER IS 1;
C5------FOR OPTION 2, THE LAYER IS SPECIFIED IN IEVT.
      IF(NEVTOP.EQ.1) THEN
         IL=1
      ELSE IF(NEVTOP.EQ.2) THEN
         IL=IEVT(IC,IR)
         IF(IL.EQ.0) GO TO 10
      ELSE
C
C6------FOR OPTION 3, FIND UPPERMOST NON-DRY CELL.
         DO 3 IL=1,NLAY
         IF(IBOUND(IC,IR,IL).NE.0) GO TO 4
    3    CONTINUE
         IL=1
    4    IEVT(IC,IR)=IL
      END IF
C
C7------IF CELL IS EXTERNAL THEN IGNORE IT.
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 10
C--SEAWAT: DETERMINE DENSE VALUE OF WITHDRAWN ET FLUID
      DENSE=DENSEREF    
      C=EVTR(IC,IR)
      S=SURF(IC,IR)
      SS=S
C      HH=HNEW(IC,IR,IL)
      HH=SALTHEAD(HNEW(IC,IR,IL),PS(IC,IR,IL),ELEV(IC,IR,IL))

C
C8------IF AQUIFER HEAD => SURF,SET Q=MAX ET RATE.
      IF(HH.LT.SS) GO TO 7
C--SEAWAT:CONSERVE MASS
C      QQ=-C
      QQ=-C*DENSE
      GO TO 9
C
C9------IF DEPTH=>EXTINCTION DEPTH, ET IS 0.
    7 X=EXDP(IC,IR)
      XX=X
      DD=SS-HH
      IF(DD.GE.XX)GO TO 10
C
C10-----LINEAR RANGE. Q= -HNEW*EVTR/EXDP -EVTR + EVTR*SURF/EXDP.
      HHCOF=-C/X
      RRHS=(C*S/X)-C
C--SEAWAT:CONSERVE MASS
C      QQ=HH*HHCOF+RRHS
      QQ=(HH*HHCOF+RRHS)*DENSE
C
C11-----ACCUMULATE TOTAL FLOW RATE.
    9 Q=QQ
      RATOUT=RATOUT-QQ
C
C12-----ADD Q TO BUFFER.
C--SEAWAT:STORE AS VOLUME
C      BUFF(IC,IR,IL)=Q
      BUFF(IC,IR,IL)=Q/DENSE
   10 CONTINUE
C
C13-----IF CELL-BY-CELL FLOW TO BE SAVED, CALL APPROPRIATE UTILITY
C13-----MODULE SAVE THEM.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IEVTCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV3(KSTP,KPER,TEXT,IEVTCB,BUFF,IEVT,NEVTOP,
     1                   NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
C
C14-----MOVE TOTAL ET RATE INTO VBVL FOR PRINTING BY BAS1OT.
      ROUT=RATOUT
      VBVL(3,MSUM)=ZERO
      VBVL(4,MSUM)=ROUT
C
C15-----ADD ET(ET_RATE TIMES STEP LENGTH) TO VBVL.
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
C
C16-----MOVE BUDGET TERM LABELS TO VBNM FOR PRINT BY MODULE BAS1OT.
      VBNM(MSUM)=TEXT
C
C17-----INCREMENT BUDGET TERM COUNTER.
      MSUM=MSUM+1
C
C18-----RETURN.
      RETURN
      END
      SUBROUTINE VDF2EVT7DA(IGRID)
C  Deallocate EVT MEMORY
      USE GWFEVTMODULE
C
        DEALLOCATE(GWFEVTDAT(IGRID)%NEVTOP)
        DEALLOCATE(GWFEVTDAT(IGRID)%IEVTCB)
        DEALLOCATE(GWFEVTDAT(IGRID)%NPEVT)
        DEALLOCATE(GWFEVTDAT(IGRID)%IEVTPF)
        DEALLOCATE(GWFEVTDAT(IGRID)%EVTR)
        DEALLOCATE(GWFEVTDAT(IGRID)%EXDP)
        DEALLOCATE(GWFEVTDAT(IGRID)%SURF)
        DEALLOCATE(GWFEVTDAT(IGRID)%IEVT)
C
      RETURN
      END
      SUBROUTINE SVDF2EVT7PNT(IGRID)
C  Set pointers to EVT data for grid.
      USE GWFEVTMODULE
C
        NEVTOP=>GWFEVTDAT(IGRID)%NEVTOP
        IEVTCB=>GWFEVTDAT(IGRID)%IEVTCB
        NPEVT=>GWFEVTDAT(IGRID)%NPEVT
        IEVTPF=>GWFEVTDAT(IGRID)%IEVTPF
        EVTR=>GWFEVTDAT(IGRID)%EVTR
        EXDP=>GWFEVTDAT(IGRID)%EXDP
        SURF=>GWFEVTDAT(IGRID)%SURF
        IEVT=>GWFEVTDAT(IGRID)%IEVT
C
      RETURN
      END
      SUBROUTINE SVDF2EVT7PSV(IGRID)
C  Save pointers to EVT data for grid.
      USE GWFEVTMODULE
C
        GWFEVTDAT(IGRID)%NEVTOP=>NEVTOP
        GWFEVTDAT(IGRID)%IEVTCB=>IEVTCB
        GWFEVTDAT(IGRID)%NPEVT=>NPEVT
        GWFEVTDAT(IGRID)%IEVTPF=>IEVTPF
        GWFEVTDAT(IGRID)%EVTR=>EVTR
        GWFEVTDAT(IGRID)%EXDP=>EXDP
        GWFEVTDAT(IGRID)%SURF=>SURF
        GWFEVTDAT(IGRID)%IEVT=>IEVT
C
      RETURN
      END
