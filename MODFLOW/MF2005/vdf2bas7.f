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


      SUBROUTINE VDF2BAS7OT(KSTP,KPER,ICNVG,ISA,IGRID,BUDPERC)
C     ******************************************************************
C     OUTPUT TIME, MASS BUDGET, HEAD, AND DRAWDOWN
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:ITMUNI,IOUT,NCOL,NROW,NLAY,STRT,DDREF
      USE GWFBASMODULE,ONLY:DELT,PERTIM,TOTIM,IHDDFL,IBUDFL,
     1                      MSUM,VBVL,VBNM,IDDREF
      USE VDFMODULE,   ONLY:HSALT
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
      CALL SVDF2BAS7H(KSTP,KPER,IPFLG,ISA)
      CALL SvDF2BAS7D(KSTP,KPER,IPFLG,ISA)
      CALL SGWF2BAS7IB(KSTP,KPER)
C
  100 CONTINUE

C4------PRINT TOTAL BUDGET IF REQUESTED
      IF(IBUDFL.EQ.0) GO TO 120
      CALL SVDF2BAS7V(MSUM,VBNM,VBVL,KSTP,KPER,IOUT,BUDPERC)
      IPFLG=1
C
C5------END PRINTOUT WITH TIME SUMMARY AND FORM FEED IF ANY PRINTOUT
C5------WILL BE PRODUCED.
  120 IF(IDDREF.NE.0) THEN
         IF(ASSOCIATED(DDREF,STRT)) THEN
            ALLOCATE(DDREF(NCOL,NROW,NLAY))
            CALL SGWF2BAS7PSV(IGRID)
         END IF
         DDREF=HSALT
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
      
      SUBROUTINE SVDF2BAS7H(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     PRINT AND RECORD HEADS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,BUFF,
     1                      IBOUND,IOUT
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IHEDFM,IHEDUN,LBHDSV,
     2                      CHEDFM,IOFLG
      USE VDFMODULE,   ONLY:HSALT
C
      CHARACTER*16 TEXT
      DATA TEXT /'            HEAD'/
C     ------------------------------------------------------------------
C
C1------FOR EACH LAYER MOVE HSALT TO BUFF IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C2------IS HEAD NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC.NE.0) KL=1
      IF(IOFLG(KL,1).EQ.0 .AND. IOFLG(KL,3).EQ.0) GO TO 59
C
C3------MOVE HSALT TO BUFF FOR THE LAYER.
      DO 58 I=1,NROW
      DO 58 J=1,NCOL
      BUFF(J,I,K)=HSALT(J,I,K)
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
      
      SUBROUTINE SVDF2BAS7D(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     CALCULATE, PRINT, AND SAVE DRAWDOWNS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,BUFF,IBOUND,
     1                      DDREF,IOUT
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IDDNFM,IDDNUN,LBDDSV,
     2                      CDDNFM,IOFLG
      USE VDFMODULE,   ONLY:HSALT     
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
      BUFF(J,I,K)=HSALT(J,I,K)
      SSTRT=DDREF(J,I,K)
      IF(IBOUND(J,I,K).NE.0) BUFF(J,I,K)=SSTRT-HSALT(J,I,K)
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
      
      SUBROUTINE SVDF2BAS7V(MSUM,VBNM,VBVL,KSTP,KPER,IOUT,BUDPERC)
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
C--SEAWAT: TITLES CHANGED TO MASS HERE
  260 FORMAT('1',/2X,'MASS BUDGET FOR ENTIRE MODEL AT END OF'
     1,' TIME STEP',I3,' IN STRESS PERIOD',I4/2X,78('-'))
  265 FORMAT(1X,/5X,'CUMULATIVE MASS',6X,'M',7X
     1,'RATES FOR THIS TIME STEP',6X,'M/T'/5X,18('-'),17X,24('-')
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