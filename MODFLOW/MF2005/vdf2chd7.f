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

      
       SUBROUTINE VDF2CHD7AD(KPER,IGRID)
C     ******************************************************************
C     COMPUTE HEAD FOR TIME STEP AT EACH TIME-VARIANT SPECIFIED HEAD
C     CELL.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,HNEW,HOLD,PERLEN,NCOL,NROW,NLAY
      USE GWFBASMODULE,ONLY:PERTIM
      USE GWFCHDMODULE,ONLY:NCHDS,CHDS
      USE VDFMODULE,   ONLY: PS,ELEV
C
      DOUBLE PRECISION DZERO,HB
      INTEGER HBCOUNT(NCOL,NROW,NLAY)                                   !JVA, ADOPTED FROM iMOD-WQ
C     ------------------------------------------------------------------
      CALL SGWF2CHD7PNT(IGRID)
      DZERO=0.
C
C1------IF NCHDS<=0 THEN THERE ARE NO TIME VARIANT SPECIFIED-HEAD CELLS.
C1------RETURN.
      IF(NCHDS.LE.0) RETURN
C
C2------INITIALIZE HNEW TO 0 AT SPECIFIED-HEAD CELLS.
      DO 50 L=1,NCHDS
      IL=CHDS(1,L)
      IR=CHDS(2,L)
      IC=CHDS(3,L)
      HNEW(IC,IR,IL)=DZERO
   50 CONTINUE
C
C3------COMPUTE PROPORTION OF STRESS PERIOD TO CENTER OF THIS TIME STEP
      IF (PERLEN(KPER).EQ.0.0) THEN
        FRAC=1.0
      ELSE
        FRAC=PERTIM/PERLEN(KPER)
      ENDIF
C
      HBCOUNT=0
C4-----CODE ADAPTED TP PREVENT SIMPLE Adding up if more contributions are in the same cell !JVA 
C     FIRST ONLY COPY HNEW TO HOLD AND COUNT CONTRIBUTIONS PER CELL      
      DO 51 L=1,NCHDS                                                   !JVA, ADOPTED FROM iMOD-WQ
C     GET COLUMN, ROW AND LAYER OF CELL CONTAINING BOUNDARY
      IL=CHDS(1,L)                                                      !JVA, ADOPTED FROM iMOD-WQ
      IR=CHDS(2,L)                                                      !JVA, ADOPTED FROM iMOD-WQ
      IC=CHDS(3,L)                                                      !JVA, ADOPTED FROM iMOD-WQ
      HBCOUNT(IC,IR,IL)=HBCOUNT(IC,IR,IL)+1                             !JVA, ADOPTED FROM iMOD-WQ
51    CONTINUE                                                          !JVA, ADOPTED FROM iMOD-WQ

C5------PROCESS EACH ENTRY IN THE SPECIFIED-HEAD CELL LIST (CHDS)
      DO 100 L=1,NCHDS
C
C6------GET COLUMN, ROW AND LAYER OF CELL CONTAINING BOUNDARY
      IL=CHDS(1,L)
      IR=CHDS(2,L)
      IC=CHDS(3,L)     
C
      IF (PERLEN(KPER).EQ.0.0 .AND. CHDS(4,L).NE.CHDS(5,L)) THEN
        WRITE(IOUT,200)IL,IR,IC
 200    FORMAT(/,' ***WARNING***  FOR CHD CELL (',I3,',',I5,',',I5,
     &'), START HEAD AND END HEAD DIFFER',/,
     &' FOR A STRESS PERIOD OF ZERO LENGTH --',/,
     &' USING ENDING HEAD AS CONSTANT HEAD',
     &' (GWF2CHD7AD)',/)
      ENDIF
C7------COMPUTE HEAD AT CELL BY LINEAR INTERPOLATION.
      HB=CHDS(4,L)+(CHDS(5,L)-CHDS(4,L))*FRAC
C--SEAWAT: SET DENSE = PS FOR DEFAULT OPTION
      DENSE=PS(IC,IR,IL)
C--SEAWAT: CONVERT HB TO EQUIVALENT FRESHWATER HEAD
      HB=FEHEAD(HB,DENSE,ELEV(IC,IR,IL))      
C
C8------UPDATE THE APPROPRIATE HNEW VALUE
      HNEW(IC,IR,IL)=HNEW(IC,IR,IL)+HB/HBCOUNT(IC,IR,IL)                !JVA, ADOPTED FROM iMOD-WQ
      HOLD(IC,IR,IL)=HNEW(IC,IR,IL)
      
  100 CONTINUE
C
C9------RETURN
      RETURN
      END