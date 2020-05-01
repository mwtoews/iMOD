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

      SUBROUTINE VDF2GHB7FM(IGRID)
C     ******************************************************************
C     ADD GHB TERMS TO RHS AND HCOF
C--SEAWAT: USE VARIABLE-DENSITY FORM OF DARCY'S LAW
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IBOUND,RHS,HCOF,HNEW
      USE GWFGHBMODULE, ONLY:NBOUND,BNDS,GHBAUX
      USE VDFMODULE,   ONLY: DENSEREF,PS,ELEV      
C--SEAWAT: MAKE HB, AND HHB DOUBLE PRECISION SO FEHEAD CAN BE USED
      DOUBLE PRECISION HB
!      COMMON /GHBCOM/GHBAUX(5)
!      CHARACTER*16 GHBAUX      
C     ------------------------------------------------------------------
      CALL SGWF2GHB7PNT(IGRID)
C
C0------IF NBOUND<=0 THEN THERE ARE NO GENERAL HEAD BOUNDS. RETURN.
      IF(NBOUND.LE.0) RETURN
C
C1--SEAWAT: FIND GHBELEV AND GHBDENS IF EXIST AS AUX VARIABLES
      LOCGHBDENS=0
      DO I=1,5
        IF(GHBAUX(I).EQ.'GHBDEN') LOCGHBDENS=I+5
      ENDDO 
C
C2------PROCESS EACH ENTRY IN THE GENERAL HEAD BOUND LIST (BNDS).
      DO 100 L=1,NBOUND
C
C3------GET COLUMN, ROW AND LAYER OF CELL CONTAINING BOUNDARY.
        IL=BNDS(1,L)
        IR=BNDS(2,L)
        IC=BNDS(3,L)
C
C4------IF THE CELL IS EXTERNAL THEN SKIP IT.
        IF(IBOUND(IC,IR,IL).LE.0) GO TO 100
C
C5------SINCE THE CELL IS INTERNAL GET THE BOUNDARY DATA.
        HB=BNDS(4,L)
        C=BNDS(5,L)
C--SEAWAT: SET GHBELEV
        GHBELEV=ELEV(IC,IR,IL)
C--SEAWAT: SET GHBDENS 
        GHBDENS=PS(IC,IR,IL)
        IF(LOCGHBDENS.GT.0) GHBDENS=BNDS(LOCGHBDENS,L)
C--SEAWAT: CONVERT HB TO FRESHWATER EQUIVALENT
        HB=FEHEAD(HB,GHBDENS,GHBELEV)
C--SEAWAT: SET AVERAGE DENSITY, AND FIND DIRECTION OF FLOW
        RHOAVG=(GHBDENS+PS(IC,IR,IL))/2
        DIRECT=HB-HNEW(IC,IR,IL)+
     &           (RHOAVG-DENSEREF)/DENSEREF*(GHBELEV-ELEV(IC,IR,IL))
C--SEAWAT: IF DIRECT IS POSITIVE, FLOW IS FROM GHB AND INTO MODEL CELL
        IF(DIRECT.LT.0.) GHBDENS=PS(IC,IR,IL)
C
C6------ADD TERMS TO RHS AND HCOF.
        HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-C*GHBDENS
C--SEAWAT: USE VD FORM
        RHS(IC,IR,IL)=RHS(IC,IR,IL)-C*GHBDENS*
     &                  (HB+(RHOAVG-DENSEREF)/DENSEREF*
     &                  (GHBELEV-ELEV(IC,IR,IL)))
C      RHS(IC,IR,IL)=RHS(IC,IR,IL)-C*HB
  100 CONTINUE
C
C7------RETURN.
      RETURN
      END
      SUBROUTINE VDF2GHB7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR GHB
C--SEAWAT: USE VARIABLE-DENSITY FORM OF DARCY'S LAW
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,HNEW,BUFF
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,IAUXSV,DELT,PERTIM,TOTIM,
     1                      VBVL,VBNM
      USE GWFGHBMODULE,ONLY:NBOUND,IGHBCB,BNDS,NGHBVL,GHBAUX
      USE VDFMODULE,   ONLY:DENSEREF,PS,ELEV      
C
C--SEAWAT: MAKE HB, AND DOUBLE PRECISION SO FEHEAD CAN BE USED
      DOUBLE PRECISION CCGHB,CHB,RATIN,RATOUT,RRATE,HB
      CHARACTER*16 TEXT
      DATA TEXT /' HEAD DEP BOUNDS'/
C     ------------------------------------------------------------------
      CALL SGWF2GHB7PNT(IGRID)
C
C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C1------ACCUMULATORS (RATIN AND RATOUT).
      ZERO=0.
      RATOUT=ZERO
      RATIN=ZERO
      IBD=0
      IF(IGHBCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IGHBCB.GT.0) IBD=ICBCFL
      IBDLBL=0
C
C2------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD.EQ.2) THEN
         NAUX=NGHBVL-6
         IF(IAUXSV.EQ.0) NAUX=0
         CALL UBDSV4(KSTP,KPER,TEXT,NAUX,GHBAUX,IGHBCB,NCOL,NROW,NLAY,
     1          NBOUND,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C3------CLEAR THE BUFFER.
      DO 50 IL=1,NLAY
      DO 50 IR=1,NROW
      DO 50 IC=1,NCOL
      BUFF(IC,IR,IL)=ZERO
50    CONTINUE
C
C4------IF NO BOUNDARIES, SKIP FLOW CALCULATIONS.
      IF(NBOUND.EQ.0) GO TO 200
      
C--SEAWAT: FIND GHBELEV AND GHBDENS IF EXIST AS AUX VARIABLES
      LOCGHBDENS=0
      DO I=1,5
        IF(GHBAUX(I).EQ.'GHBDENS') LOCGHBDENS=I+5
      ENDDO
C
      
C
C5------LOOP THROUGH EACH BOUNDARY CALCULATING FLOW.
      DO 100 L=1,NBOUND
C
C5A-----GET LAYER, ROW AND COLUMN OF EACH GENERAL HEAD BOUNDARY.
      IL=BNDS(1,L)
      IR=BNDS(2,L)
      IC=BNDS(3,L)
      RATE=ZERO
C
C5B-----IF CELL IS NO-FLOW OR CONSTANT-HEAD, THEN IGNORE IT.
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 99
C
C5C-----GET PARAMETERS FROM BOUNDARY LIST.
      HB=BNDS(4,L)
      HHNEW=HNEW(IC,IR,IL)      
      C=BNDS(5,L)
      CCGHB=C
      GHBELEV=ELEV(IC,IR,IL)
      GHBDENS=PS(IC,IR,IL)
      IF(LOCGHBDENS.GT.0) GHBDENS=BNDS(LOCGHBDENS,L)
C--SEAWAT: CONVERT HB TO FRESHWATER EQUIVALENT
      HB=FEHEAD(HB,GHBDENS,GHBELEV)
      RHOAVG=(PS(IC,IR,IL)+GHBDENS)/2
      DIRECT=HB-HHNEW+(RHOAVG-DENSEREF)/DENSEREF*
     &(GHBELEV-ELEV(IC,IR,IL))
C--SEAWAT: IF DIRECT IS POSITIVE, FLOW IS FROM GHB AND INTO MODEL CELL
      IF(DIRECT.LT.0.) GHBDENS=PS(IC,IR,IL)
      RATE=C*GHBDENS*(HB-HHNEW+(RHOAVG-DENSEREF)/DENSEREF*
     &(GHBELEV-ELEV(IC,IR,IL)))
      RRATE=RATE

C5E-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IGHBCB<0).
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) WRITE(IOUT,61) TEXT,KPER,KSTP
   61    FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
C--SEAWAT: WRITE VOLUMETRIC RATE
            WRITE(IOUT,62) L,IL,IR,IC,RATE/GHBDENS
   62       FORMAT(1X,'BOUNDARY',I4,'   LAYER',I3,'   ROW',I5,
     1               '   COL',I5,'   RATE',1PG15.6)
         IBDLBL=1
      END IF
C
C5F-----ADD RATE TO BUFFER.
C--SEAWAT: CONVERT BACK TO VOLUMETRIC FLUX
C--SEAWAT: RRATE REMAINS MASS FLUX
      RATE=RATE/GHBDENS
      BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+RATE
C
C5G-----SEE IF FLOW IS INTO AQUIFER OR OUT OF AQUIFER.
      IF(RATE.LT.ZERO) THEN
C
C5H------FLOW IS OUT OF AQUIFER SUBTRACT RATE FROM RATOUT.
        RATOUT=RATOUT-RRATE
      ELSE
C
C5I-----FLOW IS INTO AQIFER; ADD RATE TO RATIN.
        RATIN=RATIN+RRATE
      END IF
C
C5J-----IF SAVING CELL-BY-CELL FLOWS IN LIST, WRITE FLOW.  ALSO
C5J-----FLOW TO BNDS.
   99 IF(IBD.EQ.2) CALL UBDSVB(IGHBCB,NCOL,NROW,IC,IR,IL,RATE,
     1                  BNDS(:,L),NGHBVL,NAUX,6,IBOUND,NLAY)
      BNDS(NGHBVL,L)=RATE
  100 CONTINUE
C
C6------IF CELL-BY-CELL TERMS WILL BE SAVED AS A 3-D ARRAY, THEN CALL
C6------UTILITY MODULE UBUDSV TO SAVE THEM.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IGHBCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C
C7------MOVE RATES, VOLUMES AND LABELS INTO ARRAYS FOR PRINTING.
  200 RIN=RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(4,MSUM)=ROUT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C8------INCREMENT THE BUDGET TERM COUNTER.
      MSUM=MSUM+1
C
C9------RETURN.
      RETURN
      END
      SUBROUTINE VDF2GHB7DA(IGRID)
C  Deallocate GHB MEMORY
      USE GWFGHBMODULE
C
        CALL SGWF2GHB7PNT(IGRID)
        DEALLOCATE(NBOUND)
        DEALLOCATE(MXBND)
        DEALLOCATE(NGHBVL)
        DEALLOCATE(IGHBCB)
        DEALLOCATE(IPRGHB)
        DEALLOCATE(NPGHB)
        DEALLOCATE(IGHBPB)
        DEALLOCATE(NNPGHB)
        DEALLOCATE(GHBAUX)
        DEALLOCATE(BNDS)
C
      RETURN
      END
      SUBROUTINE SVDF2GHB7PNT(IGRID)
C  Change GHB data to a different grid.
      USE GWFGHBMODULE
C
        NBOUND=>GWFGHBDAT(IGRID)%NBOUND
        MXBND=>GWFGHBDAT(IGRID)%MXBND
        NGHBVL=>GWFGHBDAT(IGRID)%NGHBVL
        IGHBCB=>GWFGHBDAT(IGRID)%IGHBCB
        IPRGHB=>GWFGHBDAT(IGRID)%IPRGHB
        NPGHB=>GWFGHBDAT(IGRID)%NPGHB
        IGHBPB=>GWFGHBDAT(IGRID)%IGHBPB
        NNPGHB=>GWFGHBDAT(IGRID)%NNPGHB
        GHBAUX=>GWFGHBDAT(IGRID)%GHBAUX
        BNDS=>GWFGHBDAT(IGRID)%BNDS
C
      RETURN
      END
      SUBROUTINE SVDF2GHB7PSV(IGRID)
C  Save GHB data for a grid.
      USE GWFGHBMODULE
C
        GWFGHBDAT(IGRID)%NBOUND=>NBOUND
        GWFGHBDAT(IGRID)%MXBND=>MXBND
        GWFGHBDAT(IGRID)%NGHBVL=>NGHBVL
        GWFGHBDAT(IGRID)%IGHBCB=>IGHBCB
        GWFGHBDAT(IGRID)%IPRGHB=>IPRGHB
        GWFGHBDAT(IGRID)%NPGHB=>NPGHB
        GWFGHBDAT(IGRID)%IGHBPB=>IGHBPB
        GWFGHBDAT(IGRID)%NNPGHB=>NNPGHB
        GWFGHBDAT(IGRID)%GHBAUX=>GHBAUX
        GWFGHBDAT(IGRID)%BNDS=>BNDS
C
      RETURN
      END
