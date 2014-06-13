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

      MODULE GWFEVTMODULE
        INTEGER, SAVE, POINTER                 ::NEVTOP,IEVTCB
        INTEGER, SAVE, POINTER                 ::NPEVT,IEVTPF
        REAL,    SAVE,   DIMENSION(:,:),  POINTER      ::EVTR
        REAL,    SAVE,   DIMENSION(:,:),  POINTER      ::EXDP
        REAL,    SAVE,   DIMENSION(:,:),  POINTER      ::SURF
        INTEGER, SAVE,   DIMENSION(:,:),  POINTER      ::IEVT
      TYPE GWFEVTTYPE
        INTEGER,  POINTER                 ::NEVTOP,IEVTCB
        INTEGER,  POINTER                 ::NPEVT,IEVTPF
        REAL,       DIMENSION(:,:),  POINTER      ::EVTR
        REAL,       DIMENSION(:,:),  POINTER      ::EXDP
        REAL,       DIMENSION(:,:),  POINTER      ::SURF
        INTEGER,    DIMENSION(:,:),  POINTER      ::IEVT
      END TYPE
      TYPE(GWFEVTTYPE), SAVE ::GWFEVTDAT(10)
      END MODULE GWFEVTMODULE



      SUBROUTINE GWF2EVT7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR EVAPOTRANSPIRATION
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,IFREFM
      USE GWFEVTMODULE,ONLY:NEVTOP,IEVTCB,NPEVT,IEVTPF,EVTR,EXDP,SURF,
     1                      IEVT
C
      CHARACTER*200 LINE
      CHARACTER*4 PTYP
C     ------------------------------------------------------------------
C
C1------ALLOCATE SCALAR VARIABLES.
      ALLOCATE(NEVTOP,IEVTCB)
      ALLOCATE(NPEVT,IEVTPF)
C
C2------IDENTIFY PACKAGE.
      IEVTPF=0
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'EVT -- EVAPOTRANSPIRATION PACKAGE, VERSION 7,',
     1     ' 5/2/2005',/,9X,'INPUT READ FROM UNIT ',I4)
C
C3------READ ET OPTION (NEVTOP) AND UNIT OR FLAG FOR CELL-BY-CELL FLOW
C3------TERMS (IEVTCB).
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARARRAL(IN,IOUT,LINE,NPEVT)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') NEVTOP,IEVTCB
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NEVTOP,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IEVTCB,R,IOUT,IN)
      END IF
C
C4------CHECK TO SEE THAT ET OPTION IS LEGAL.
      IF(NEVTOP.GE.1.AND.NEVTOP.LE.3)GO TO 200
C
C4A-----OPTION IS ILLEGAL -- PRINT A MESSAGE & ABORT SIMULATION.
      WRITE(IOUT,8) NEVTOP
    8 FORMAT(1X,'ILLEGAL ET OPTION CODE (NEVTOP = ',I5,
     1       ') -- SIMULATION ABORTING')
      CALL USTOP(' ')
C
C5------OPTION IS LEGAL -- PRINT THE OPTION CODE.
  200 IF(NEVTOP.EQ.1) WRITE(IOUT,201)
  201 FORMAT(1X,'OPTION 1 -- EVAPOTRANSPIRATION FROM TOP LAYER')
      IF(NEVTOP.EQ.2) WRITE(IOUT,202)
  202 FORMAT(1X,'OPTION 2 -- EVAPOTRANSPIRATION FROM ONE SPECIFIED',
     1   ' NODE IN EACH VERTICAL COLUMN')
      IF(NEVTOP.EQ.3) WRITE(IOUT,203)
  203 FORMAT(1X,'OPTION 3 -- EVAPOTRANSPIRATION FROM HIGHEST ACTIVE',
     1   ' NODE IN EACH VERTICAL COLUMN')
C
C6------IF CELL-BY-CELL FLOWS ARE TO BE SAVED, THEN PRINT UNIT NUMBER.
      IF(IEVTCB.GT.0) WRITE(IOUT,204) IEVTCB
  204 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
C
C7------ALLOCATE SPACE FOR THE ARRAYS EVTR, EXDP, SURF, AND IEVT.
      ALLOCATE (EVTR(NCOL,NROW))
      ALLOCATE (EXDP(NCOL,NROW))
      ALLOCATE (SURF(NCOL,NROW))
      ALLOCATE (IEVT(NCOL,NROW))
C
C8------READ NAMED PARAMETERS
      WRITE(IOUT,5) NPEVT
    5 FORMAT(1X,//1X,I5,' Evapotranspiration parameters')
      IF(NPEVT.GT.0) THEN
         DO 20 K=1,NPEVT
         CALL UPARARRRP(IN,IOUT,N,0,PTYP,1,1,0)
         IF(PTYP.NE.'EVT') THEN
            WRITE(IOUT,7)
    7       FORMAT(1X,'Parameter type must be EVT')
            CALL USTOP(' ')
         END IF
   20    CONTINUE
      END IF
C
C9------RETURN
      CALL SGWF2EVT7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2EVT7RP(IN,IGRID)
C     ******************************************************************
C     READ EVAPOTRANSPIRATION DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,DELR,DELC,IFREFM
      USE GWFEVTMODULE,ONLY:NEVTOP,NPEVT,IEVTPF,EVTR,EXDP,SURF,IEVT
C
      CHARACTER*24 ANAME(4)
C
      DATA ANAME(1) /'          ET LAYER INDEX'/
      DATA ANAME(2) /'              ET SURFACE'/
      DATA ANAME(3) /' EVAPOTRANSPIRATION RATE'/
      DATA ANAME(4) /'        EXTINCTION DEPTH'/
C     ------------------------------------------------------------------
C
C1------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2EVT7PNT(IGRID)
C
C2------READ FLAGS SHOWING WHETHER DATA IS TO BE REUSED.
      IF(NEVTOP.EQ.2) THEN
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(4I10)') INSURF,INEVTR,INEXDP,INIEVT
         ELSE
            READ(IN,*) INSURF,INEVTR,INEXDP,INIEVT
         END IF
      ELSE
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(3I10)') INSURF,INEVTR,INEXDP
         ELSE
            READ(IN,*) INSURF,INEVTR,INEXDP
         END IF
      END IF
C
C3------TEST INSURF TO SEE WHERE SURFACE ELEVATION COMES FROM.
      IF(INSURF.LT.0) THEN
C
C3A------INSURF<0, SO REUSE SURFACE ARRAY FROM LAST STRESS PERIOD.
        call sts2nodata(in)                                              ! STS
        WRITE(IOUT,3)
    3   FORMAT(1X,/1X,'REUSING SURF FROM LAST STRESS PERIOD')
      ELSE
C
C3B------INSURF=>0, SO CALL MODULE U2DREL TO READ SURFACE.
        call sts2data(in)                                                ! STS
        CALL U2DREL(SURF,ANAME(2),NROW,NCOL,0,IN,IOUT)
      END IF
C
C4------TEST INEVTR TO SEE WHERE MAX ET RATE (EVTR) COMES FROM.
      IF(INEVTR.LT.0) THEN
C
C4A-----INEVTR<0, SO REUSE EVTR FROM LAST STRESS PERIOD.
        call sts2nodata(in)                                              ! STS
        WRITE(IOUT,4)
    4   FORMAT(1X,/1X,'REUSING EVTR FROM LAST STRESS PERIOD')
      ELSE
C
C4B-----INEVTR=>0, SO READ MAX ET RATE.
        IF(NPEVT.EQ.0) THEN
C
C4B1----THERE ARE NO PARAMETERS,SO READ EVTR USING U2DREL
          call sts2data(in)                                              ! STS
          CALL U2DREL(EVTR,ANAME(3),NROW,NCOL,0,IN,IOUT)
        ELSE
C
C4B2----DEFINE EVTR USING PARAMETERS. INEVTR IS THE NUMBER OF
C4B2----PARAMETERS TO USE THIS STRESS PERIOD.
          CALL PRESET('EVT')
          WRITE(IOUT,33)
   33     FORMAT(1X,///1X,
     1      'EVTR array defined by the following parameters:')
          IF (INEVTR.EQ.0) THEN
            WRITE(IOUT,34)
   34       FORMAT(' ERROR: When parameters are defined for the EVT',
     &     ' Package, at least one parameter',/,' must be specified',
     &     ' each stress period -- STOP EXECUTION (GWF2EVT7RPSS)')
            CALL USTOP(' ')
          END IF
          CALL UPARARRSUB2(EVTR,NCOL,NROW,0,INEVTR,IN,IOUT,'EVT',
     1        ANAME(3),'EVT',IEVTPF)
        END IF
C
C5------MULTIPLY MAX ET RATE BY CELL AREA TO GET VOLUMETRIC RATE
        DO 40 IR=1,NROW
        DO 40 IC=1,NCOL
        EVTR(IC,IR)=EVTR(IC,IR)*DELR(IC)*DELC(IR)
   40   CONTINUE
      END IF
C
C6------TEST INEXDP TO SEE WHERE EXTINCTION DEPTH COMES FROM
      IF(INEXDP.LT.0) THEN
C
C6A------IF INEXDP<0 REUSE EXTINCTION DEPTH FROM LAST STRESS PERIOD
        call sts2nodata(in)                                              ! STS
        WRITE(IOUT,5)
    5   FORMAT(1X,/1X,'REUSING EXDP FROM LAST STRESS PERIOD')
      ELSE
C
C6B------IF INEXDP=>0 CALL MODULE U2DREL TO READ EXTINCTION DEPTH
        call sts2data(in)                                               ! STS
        CALL U2DREL(EXDP,ANAME(4),NROW,NCOL,0,IN,IOUT)
      END IF
C
C7------IF OPTION(NEVTOP) IS 2 THEN WE NEED AN INDICATOR ARRAY.  TEST
C7------INIEVT TO SEE HOW TO DEFINE IEVT.
      IF(NEVTOP.EQ.2) THEN
        IF(INIEVT.LT.0) THEN
C
C7A------IF INIEVT<0 THEN REUSE LAYER INDICATOR ARRAY.
          call sts2nodata(in)                                            ! STS
          WRITE(IOUT,2)
    2     FORMAT(1X,/1X,'REUSING IEVT FROM LAST STRESS PERIOD')
        ELSE
C
C7B------IF INIEVT=>0 THEN CALL MODULE U2DINT TO READ INDICATOR ARRAY.
          call sts2data(in)                                              ! STS
   49     CALL U2DINT(IEVT,ANAME(1),NROW,NCOL,0,IN,IOUT)
          DO 57 IR=1,NROW
          DO 57 IC=1,NCOL
          IF(IEVT(IC,IR).LT.1 .OR. IEVT(IC,IR).GT.NLAY) THEN
            WRITE(IOUT,56) IC,IR,IEVT(IC,IR)
   56       FORMAT(1X,/1X,'INVALID LAYER NUMBER IN IEVT FOR COLUMN',I4,
     1           '  ROW',I4,'  :',I4)
            CALL USTOP(' ')
          END IF
   57     CONTINUE
        END IF
      END IF
C
C8------RETURN
      RETURN
      END
      SUBROUTINE GWF2EVT7FM(IGRID)
C     ******************************************************************
C     ADD EVAPOTRANSPIRATION TO RHS AND HCOF
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,RHS,HCOF
      USE GWFEVTMODULE,ONLY:NEVTOP,EVTR,EXDP,SURF,IEVT
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
         IF(IL.EQ.0) GO TO 10
      ELSE
C
C4------FOR OPTION 3, FIND UPPERMOST ACTIVE CELL.
         DO 3 IL=1,NLAY
         IF(IBOUND(IC,IR,IL).NE.0) GO TO 4
    3    CONTINUE
         IL=1
      END IF
C
C5------IF THE CELL IS NOT VARIABLE HEAD, IGNORE IT.  IF CELL IS
C5------VARIABLE HEAD, GET DATA NEEDED TO COMPUTE FLOW TERMS.
    4 IF(IBOUND(IC,IR,IL).LE.0)GO TO 10
      C=EVTR(IC,IR)
      S=SURF(IC,IR)
      SS=S
      HH=HNEW(IC,IR,IL)
C
C6------IF AQUIFER HEAD IS GREATER THAN OR EQUAL TO SURF, ET IS CONSTANT
      IF(HH.LT.SS) GO TO 5
C
C6A-----HEAD IS GREATER THAN OR EQUAL TO SURF.  ADD EVTR TO RHS
      RHS(IC,IR,IL)=RHS(IC,IR,IL) + C
      GO TO 10
C
C7------IF DEPTH TO WATER>=EXTINCTION DEPTH, THEN ET IS 0.
    5 DD=SS-HH
      X=EXDP(IC,IR)
      XX=X
      IF(DD.GE.XX)GO TO 10
C
C8------LINEAR RANGE. ADD ET TERMS TO BOTH RHS AND HCOF.
      RHS(IC,IR,IL)=RHS(IC,IR,IL)+C-C*S/X
      HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-C/X
   10 CONTINUE
C
C9------RETURN
      RETURN
      END
      SUBROUTINE GWF2EVT7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR EVAPOTRANSPIRATION
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,HNEW,BUFF
      USE GWFBASMODULE,ONLY:MSUM,VBVL,VBNM,ICBCFL,DELT,PERTIM,TOTIM
      USE GWFEVTMODULE,ONLY:NEVTOP,IEVTCB,EVTR,EXDP,SURF,IEVT
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
      C=EVTR(IC,IR)
      S=SURF(IC,IR)
      SS=S
      HH=HNEW(IC,IR,IL)
C
C8------IF AQUIFER HEAD => SURF,SET Q=MAX ET RATE.
      IF(HH.LT.SS) GO TO 7
      QQ=-C
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
      QQ=HH*HHCOF+RRHS
C
C11-----ACCUMULATE TOTAL FLOW RATE.
    9 Q=QQ
      RATOUT=RATOUT-QQ
C
C12-----ADD Q TO BUFFER.
      BUFF(IC,IR,IL)=Q
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
      SUBROUTINE GWF2EVT7DA(IGRID)
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
      SUBROUTINE SGWF2EVT7PNT(IGRID)
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
      SUBROUTINE SGWF2EVT7PSV(IGRID)
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
