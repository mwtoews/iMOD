!!  Copyright (C) Stichting Deltares, 2005-2018.
!!
!!  This file is part of iMOD.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License as published by
!!  the Free Software Foundation, either version 3 of the License, or
!!  (at your option) any later version.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!!
!!  Contact: imod.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands.
!!
!!  iMOD is partly based on the USGS MODFLOW2005 source code;
!!  for iMOD the USGS MODFLOW2005 source code has been expanded
!!  and extensively modified by Stichting Deltares.
!!  The original USGS MODFLOW2005 source code can be downloaded from the USGS 
!!  website http://www.usgs.gov/. The original USGS MODFLOW2005 code incorporated 
!!  in this file is covered by the USGS Software User Rights Notice;
!!  you should have received a copy of this notice along with this program. 
!!  If not, see <http://water.usgs.gov/software/help/notice/>.
!!
MODULE MOD_PCG

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : DP_KIND,SP_KIND
USE MOD_UTL, ONLY : ITOS,RTOS

INTEGER,PARAMETER,PRIVATE :: NPCOND =1
INTEGER,PARAMETER,PRIVATE :: IPCGCD =0
INTEGER,PARAMETER,PRIVATE :: NBPOL  =0
INTEGER,PARAMETER,PRIVATE :: MUTPCG =1
INTEGER,PARAMETER,PRIVATE :: IU=0
INTEGER,PARAMETER,PRIVATE :: KPER=1
REAL(KIND=DP_KIND),PARAMETER,PRIVATE :: DELT=1.0D0

CONTAINS

 !###====================================================================
 SUBROUTINE PCGSETTINGS(NOUTER,NINNER,HCLOSE,RCLOSE,ITIGHT,MICNVG,RELAX,IDAMPING,FTIGHT,IBREAK)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(INOUT) :: HCLOSE,RCLOSE,RELAX,FTIGHT
 INTEGER,INTENT(INOUT),OPTIONAL :: IBREAK
 INTEGER,INTENT(INOUT) :: NOUTER,NINNER,ITIGHT,MICNVG,IDAMPING
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I
 
 CALL WDIALOGLOAD(ID_DPCGSOLVER,ID_DPCGSOLVER)
 
 !## pcg solver options
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,NOUTER)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER2,NINNER)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER3,MICNVG)
 CALL WDIALOGPUTDOUBLE(IDF_REAL1,HCLOSE,'(E10.3)') 
 CALL WDIALOGPUTDOUBLE(IDF_REAL2,RCLOSE,'(E10.3)')  
 CALL WDIALOGPUTDOUBLE(IDF_REAL3,RELAX,'(F15.3)')  
 CALL WDIALOGPUTDOUBLE(IDF_REAL4,FTIGHT,'(F15.3)')  
 IF(ITIGHT.EQ.1)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO1)
 IF(ITIGHT.EQ.2)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO2)
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,IDAMPING)
 CALL WDIALOGFIELDSTATE(IDCANCEL,3) 
 IF(PRESENT(IBREAK))THEN; IBREAK=0; CALL WDIALOGFIELDSTATE(IDCANCEL,1); ENDIF

 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
 I=I-1; CALL WDIALOGFIELDSTATE(IDF_REAL4,I)
 
 CALL WDIALOGSHOW(-1,-1,0,3)
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE(FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_RADIO1,IDF_RADIO2)
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
      I=I-1; CALL WDIALOGFIELDSTATE(IDF_REAL4,I)
    END SELECT

   CASE(PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDOK)
      CALL WDIALOGGETINTEGER(IDF_INTEGER1,NOUTER)
      CALL WDIALOGGETINTEGER(IDF_INTEGER2,NINNER)
      CALL WDIALOGGETINTEGER(IDF_INTEGER3,MICNVG)
      CALL WDIALOGGETDOUBLE(IDF_REAL1,HCLOSE) 
      CALL WDIALOGGETDOUBLE(IDF_REAL2,RCLOSE)  
      CALL WDIALOGGETDOUBLE(IDF_REAL3,RELAX)  
      CALL WDIALOGGETDOUBLE(IDF_REAL4,FTIGHT)  
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,ITIGHT)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IDAMPING)
      EXIT
     CASE (IDCANCEL)
      EXIT
      IF(PRESENT(IBREAK))IBREAK=1
    END SELECT
  END SELECT
 
 ENDDO

 CALL WDIALOGUNLOAD()
 
 END SUBROUTINE PCGSETTINGS

 !###====================================================================
 SUBROUTINE PCG2AP(NODES,NROW,NCOL,NLAY,IBOUND,CR,CC,CV,HCOF,RHS,V,SS,P, &
                   CD,HNEW,MXITER,ITER1,KITER,NITER,ICNVG,HCLOSE,RCLOSE,IECHO,NICNVG,RELAX,HCHG,RCHG)
 !###====================================================================
 IMPLICIT NONE

 REAL(KIND=DP_KIND), PARAMETER :: DZERO=0.0D0
 REAL(KIND=DP_KIND), PARAMETER :: DONE=1.0D0

 INTEGER,INTENT(INOUT) :: NITER,ICNVG
 INTEGER,INTENT(IN) :: NODES,NLAY,NROW,NCOL,MXITER,KITER,ITER1
 REAL(KIND=DP_KIND),DIMENSION(NODES),INTENT(INOUT) :: HNEW
 INTEGER,DIMENSION(NODES),INTENT(IN) :: IBOUND
 REAL(KIND=DP_KIND),DIMENSION(NODES),INTENT(INOUT)  :: CR,CC,HCOF,RHS
 REAL(KIND=DP_KIND),DIMENSION(NODES),INTENT(INOUT)  :: V,SS,P,CD
 REAL(KIND=DP_KIND),DIMENSION(MAX(1,NODES-(NROW*NCOL))),INTENT(INOUT) :: CV
 
 REAL(KIND=DP_KIND),INTENT(IN) :: HCLOSE,RCLOSE,RELAX
 INTEGER,INTENT(IN) :: IECHO
 INTEGER,INTENT(OUT) :: NICNVG
 REAL(KIND=DP_KIND),INTENT(OUT) :: HCHG,RCHG
 
 REAL(KIND=DP_KIND) :: CD1,T,BIGH,BIGR,BPOLY,STEPL
 INTEGER :: IP,NRC,NORM,N,ITYPE,I,J,K,NRN,NRL,NCN,NCL,NLN,NLL,NCF,NCD,NRB,NRH, &
         NLS,NLZ,IITER,IICNVG,IC,IR,IL,II,JJ,KK,NC,NR,NL,IH,JH,KH,NH,JR,KR
 REAL(KIND=DP_KIND) :: HHCOF,RRHS,C0,C1,C2
 REAL(KIND=DP_KIND) :: Z,B,D,E,F,H,S,ALPHA
 REAL(KIND=DP_KIND) :: ZHNEW,BHNEW,DHNEW,FHNEW,HHNEW,SHNEW
 REAL(KIND=DP_KIND) :: SRNEW,SROLD,SSCR,SSCC,SSCV,VCC,VCR,VCV
 REAL(KIND=DP_KIND) :: CDCC,CDCR,CDCV
 REAL(KIND=DP_KIND) :: PN,VN,HCHGN,RCHGN,PAP
 REAL(KIND=DP_KIND) :: FCC,FCR,FCV,FV

 INTEGER,DIMENSION(3)  :: LHCH,LRCH
! REAL(KIND=DP_KIND) :: RCHG

 KR=0
 IR=0
 JR=0
 KH=0
 IH=0
 JH=0

 IP=0
 IF(KITER.EQ.1)THEN
  LHCH=0
  LRCH=0
  RCHG=0.
  HCHG=0.
 ENDIF

 !C-------ASSIGN VALUES TO FIELDS THAT ARE CONSTANT DURING AN ITERATION
 NRC=NROW*NCOL
 !C-------INITIALIZE VARIABLES USED TO CALCULATE ITERATION PARAMETERS
 SRNEW=DZERO
 BPOLY=0.
! IF(NPCOND.NE.1) RELAX=1.
 NORM=0
 IF(NPCOND.EQ.2)NORM=1
 !C-------INITIALIZE VARIABLE USED TO TEST FOR NEGATIVE CHOLESKY DIAGONAL
 CD1=0.
 !C------CLEAR PCG WORK ARRAYS.

 DO 100 N=1,NODES
  SS(N)=0.
  P(N)=0.
 100 V(N)=0.

 ITYPE=0
 IF(NPCOND.EQ.1)THEN
   ITYPE=0
   IF(IPCGCD.EQ.1.AND.(IU.EQ.0.OR.IP.GT.0.OR.KPER.GT.1))THEN
     IF(STEPL.EQ.DELT)ITYPE=1
     STEPL=DELT
   ENDIF
   IF(ITYPE.EQ.0)THEN
    DO 105 N=1,NODES
 105     CD(N)=0.
   ENDIF
 ENDIF

 !C------CALCULATE THE RESIDUAL. IF NORM=1, CALCULATE THE DIAGONALS OF
 !C------THE A MATRIX,AND STORE THEM IN HCOF.
 DO 115 K=1,NLAY
 DO 115 I=1,NROW
 DO 115 J=1,NCOL

 !C-------CALCULATE 1 DIMENSIONAL SUBSCRIPT OF CURRENT CELL AND
 !C-------SKIP CALCULATIONS IF CELL IS INACTIVE
 N=J+(I-1)*NCOL+(K-1)*NRC
 IF(IBOUND(N).EQ.0)THEN
   CC(N)=0.
   CR(N)=0.
   IF(N.LE.(NODES-NRC))CV(N)=0.
   IF(N.GE.2)CR(N-1)=0.
   IF(N.GE.NCOL+1)CC(N-NCOL)=0.
   IF(N.LE.(NODES-NRC).AND.N.GE.NRC+1)CV(N-NRC)=0.
   HCOF(N)=0.
   RHS(N)=0.
   GO TO 115
 ENDIF

 !C-------CALCULATE 1 DIMENSIONAL SUBSCRIPTS FOR LOCATING THE 6
 !C-------SURROUNDING CELLS
 NRN=N+NCOL
 NRL=N-NCOL
 NCN=N+1
 NCL=N-1
 NLN=N+NRC
 NLL=N-NRC

 !C-------CALCULATE 1 DIMENSIONAL SUBSCRIPTS FOR CONDUCTANCE TO THE 6
 !C-------SURROUNDING CELLS.
 NCF=N
 NCD=N-1
 NRB=N-NCOL
 NRH=N
 NLS=N
 NLZ=N-NRC

 !C-----GET CONDUCTANCES TO NEIGHBORING CELLS
 !C-------NEIGHBOR IS 1 ROW BACK
 B=DZERO
 BHNEW=DZERO
 IF(I.NE.1)THEN
   B=CC(NRB)
   BHNEW=B*(HNEW(NRL)-HNEW(N))
 ENDIF

 !C-------NEIGHBOR IS 1 ROW AHEAD
 H=DZERO
 HHNEW=DZERO
 IF(I.NE.NROW)THEN
   H=CC(NRH)
   HHNEW=H*(HNEW(NRN)-HNEW(N))
 ENDIF

 !C-------NEIGHBOR IS 1 COLUMN BACK
 D=DZERO
 DHNEW=DZERO
 IF(J.NE.1)THEN
   D=CR(NCD)
   DHNEW=D*(HNEW(NCL)-HNEW(N))
 ENDIF

 !C-------NEIGHBOR IS 1 COLUMN AHEAD
 F=DZERO
 FHNEW=DZERO
 IF(J.NE.NCOL)THEN
   F=CR(NCF)
   FHNEW=F*(HNEW(NCN)-HNEW(N))
 ENDIF

 !C-------NEIGHBOR IS 1 LAYER BEHIND
 Z=DZERO
 ZHNEW=DZERO
 IF(K.NE.1)THEN
   Z=CV(NLZ)
   ZHNEW=Z*(HNEW(NLL)-HNEW(N))
 ENDIF

 !C-------NEIGHBOR IS 1 LAYER AHEAD
 S=DZERO
 SHNEW=DZERO
 IF(K.NE.NLAY)THEN
   S=CV(NLS)
   SHNEW=S*(HNEW(NLN)-HNEW(N))
 ENDIF

 IF(I.EQ.NROW)CC(N)=0.
 IF(J.EQ.NCOL)CR(N)=0.

 !C-------CALCULATE THE RESIDUAL AND STORE IT IN RHS.  TO SCALE A,
 !C-------CALCULATE THE DIAGONAL OF THE A MATRIX, AND STORE IT IN HCOF.
 E=-Z-B-D-F-H-S
 RRHS=RHS(N)
 HHCOF=HNEW(N)*HCOF(N)
 RHS(N)=RRHS-ZHNEW-BHNEW-DHNEW-HHCOF-FHNEW-HHNEW-SHNEW
 IF(NORM.EQ.1)HCOF(N)=HCOF(N)+E
 IF(IBOUND(N).LT.0)RHS(N)=0.
 115 CONTINUE

  !C-------SCALE CC,CR,CV,RHS AND HNEW IF NORM=1.
 IF(NORM.EQ.1)THEN
   DO 120 K=1,NLAY
   DO 120 I=1,NROW
   DO 120 J=1,NCOL
     N=J+(I-1)*NCOL+(K-1)*NRC
     IF(IBOUND(N).EQ.0)GO TO 120
     HHCOF=SQRT(-HCOF(N))
     IF(N.LE.(NODES-NCOL).AND.CC(N).GT.0.)CC(N)=CC(N)/(HHCOF*(SQRT(-HCOF(N+NCOL))))
     IF(CR(N).GT.0.)CR(N)=CR(N)/(HHCOF*(SQRT(-HCOF(N+1))))
     IF(N.LE.(NODES-NRC))THEN
      IF(CV(N).GT.0.)CV(N)=CV(N)/(HHCOF*(SQRT(-HCOF(N+NRC))))
     ENDIF
     HNEW(N)=HNEW(N)*HHCOF
     RHS(N)=RHS(N)/HHCOF
 120   CONTINUE
 ENDIF

 !C-------CALCULATE PARAMETER B OF THE POLYNOMIAL PRECONDITIONING METHOD
 IF(NPCOND.NE.2)GO TO 152
 IF(NBPOL.EQ.2)THEN
   BPOLY=2.
   GO TO 151
 ENDIF
 DO 150 K=1,NLAY
 DO 150 I=1,NROW
 DO 150 J=1,NCOL

 N=J+(I-1)*NCOL+(K-1)*NRC
 IF(IBOUND(N).LE.0)GO TO 150

 NCF=N
 NCD=N-1
 NRB=N-NCOL
 NRH=N
 NLS=N
 NLZ=N-NRC

 B=DZERO
 IF(I.NE.1)B=CC(NRB)
 H=DZERO
 IF(I.NE.NROW)H=CC(NRH)
 D=DZERO
 IF(J.NE.1)D=CR(NCD)
 F=DZERO
 IF(J.NE.NCOL)F=CR(NCF)
 Z=DZERO
 IF(K.NE.1)Z=CV(NLZ)
 S=DZERO
 IF(K.NE.NLAY)S=CV(NLS)

 !C-------NOTE : ABS. VAL. OF THE DIAG. OF THE SCALED A MATRIX IS 1.
 HHCOF=HCOF(N)
 IF(NORM.EQ.1)HHCOF=DONE
 T=DABS(Z)+DABS(B)+DABS(D)+ABS(HHCOF)+DABS(F)+DABS(H)+DABS(S)
 IF(T.GT.BPOLY)BPOLY=T
   150 CONTINUE
   151 CONTINUE

 !C-------CALCULATE ITERATION PARAMETERS FOR POLYNOMIAL PRECONDITIONING
 !C-------METHOD FOR A NEGATIVE DEFINITE MATRIX.
   C0=(15./32.)*(BPOLY**3)
   C1=(27./16.)*(BPOLY**2)
   C2=(9./4.)*BPOLY
   152 CONTINUE

 !C-------START INTERNAL ITERATIONS
 IITER=0
 IF(KITER.EQ.1)NITER=0
 ICNVG=0
 IICNVG=0
 153   CONTINUE
 IITER=IITER+1
 NITER=NITER+1

 !C-------INITIALIZE VARIABLES THAT TRACK MAXIMUM HEAD CHANGE AND RESIDUAL
 !C-------VALUE DURING EACH ITERATIONS
 BIGH=0.
 BIGR=0.

 !C-------CHECK NPCOND FOR PRECONDITIONING TYPE AND EXECUTE PROPER CODE
 IF(NPCOND.EQ.2)GO TO 165

 !C-------CHOLESKY PRECONDITIONING

 !C-------STEP THROUGH CELLS TO CALCULATE THE DIAGONAL OF THE CHOLESKY
 !C-------MATRIX(FIRST INTERNAL ITERATION ONLY) AND THE INTERMEDIATE
 !C-------SOLUTION.  STORE THEM IN CD AND V, RESPECTIVELY.
 DO 155 K=1,NLAY
 DO 155 I=1,NROW
 DO 155 J=1,NCOL

 N=J+(I-1)*NCOL+(K-1)*NRC

 IF(IBOUND(N).LE.0)GO TO 155

 !C-------CALCULATE V
 H=DZERO
 VCC=DZERO
 IC=N-NCOL
 IF(I.NE.1)THEN
   H=CC(IC)
   IF(CD(IC).NE.0.)VCC=H*V(IC)/CD(IC)
 ENDIF

 F=DZERO
 VCR=DZERO
 IR=N-1
 IF(J.NE.1)THEN
   F=CR(IR)
   IF(CD(IR).NE.0.)VCR=F*V(IR)/CD(IR)
 ENDIF

 S=DZERO
 VCV=DZERO
 IL=N-NRC
 IF(K.NE.1)THEN
   S=CV(IL)
   IF(CD(IL).NE.0.)VCV=S*V(IL)/CD(IL)
 ENDIF
 V(N)=RHS(N)-VCR-VCC-VCV

 !C-------CALCULATE CD - FIRST INTERNAL ITERATION ONLY
 IF(IITER.EQ.1.AND.ITYPE.EQ.0)THEN
  CDCR=DZERO
  CDCC=DZERO
  CDCV=DZERO
  FCC=DZERO
  FCR=DZERO
  FCV=DZERO
  IF(IR.GT.0)THEN
   IF(CD(IR).NE.0.)CDCR=(F**2.0D0)/CD(IR)
  ENDIF
  IF(IC.GT.0)THEN
   IF(CD(IC).NE.0.)CDCC=(H**2.0D0)/CD(IC)
  ENDIF
  IF(IL.GT.0)THEN
   IF(CD(IL).NE.0.)CDCV=(S**2.0D0)/CD(IL)
  ENDIF

  IF(NPCOND.EQ.1)THEN
   IF(IR.GT.0)THEN
    IF(K.NE.NLAY)FV=CV(IR)
 !   IF(NLAY.NE.1)FV=CV(IR)
 !   FV=CV(IR)
    IF(K.EQ.NLAY.AND.((J+I).GT.1))FV=DZERO
    IF(CD(IR).NE.0.)FCR=(F/CD(IR))*(CC(IR)+FV)
   ENDIF
   IF(IC.GT.0)THEN
    IF(K.NE.NLAY)FV=CV(IC)
 !   IF(NLAY.NE.1)FV=CV(IC)
 !   FV=CV(IC)
    IF(K.EQ.NLAY.AND.(I.GT.1))FV=DZERO
    IF(CD(IC).NE.0.)FCC=(H/CD(IC))*(CR(IC)+FV)
   ENDIF
   IF(IL.GT.0)THEN
    IF(CD(IL).NE.0.)FCV=(S/CD(IL))*(CR(IL)+CC(IL))
   ENDIF
  ENDIF

  IF(NORM.EQ.0)THEN
   B=DZERO
   H=DZERO
   D=DZERO
   F=DZERO
   Z=DZERO
   S=DZERO
   IF(I.NE.1)B=CC(IC)
   IF(I.NE.NROW)H=CC(N)
   IF(J.NE.1)D=CR(IR)
   IF(J.NE.NCOL)F=CR(N)
   IF(K.NE.1)Z=CV(IL)
   IF(K.NE.NLAY)S=CV(N)
   HHCOF=HCOF(N)-Z-B-D-F-H-S
  ENDIF
  IF(NORM.EQ.1)HHCOF=-DONE
  CD(N)=HHCOF-CDCR-CDCC-CDCV-RELAX*(FCR+FCC+FCV)
  IF(CD1.EQ.0.0D0.AND.CD(N).NE.0.0D0)CD1=CD(N)
  IF(CD(N)*CD1.LT.0.0D0)THEN
   IF(NLAY.EQ.1)WRITE(*,'(2i5,5E15.8)')N,IBOUND(N),HNEW(N),cc(N),cr(N),rhs(N),hcof(N)
   IF(NLAY.GT.1)WRITE(*,'(2i5,6E15.8)')N,IBOUND(N),HNEW(N),cc(N),cr(N),CV(N),rhs(N),hcof(N)
   WRITE(*,*) 'CHOLESKY DIAGONAL LESS THAN ZERO'
   WRITE(*,*) 'EXECUTION TERMINATED(MATRIX NOT DIAG. DOMINANT)'
   PAUSE 
   STOP
  ENDIF
 ENDIF

 155 CONTINUE

 !C-------STEP THROUGH EACH CELL AND SOLVE FOR S OF THE CONJUGATE
 !C-------GRADIENT ALGORITHM BY BACK SUBSTITUTION. STORE RESULT IN SS.
 DO 160 KK=NLAY,1,-1
 DO 160 II=NROW,1,-1
 DO 160 JJ=NCOL,1,-1

 N=JJ+(II-1)*NCOL+(KK-1)*NRC
 IF(IBOUND(N).LE.0)GO TO 160

 NC=N+1
 NR=N+NCOL
 NL=N+NRC

 !C-------BACK SUBSTITUTE, STORING RESULT IN ARRAY SS
 SSCR=DZERO
 SSCC=DZERO
 SSCV=DZERO
 IF(JJ.NE.NCOL)SSCR=CR(N)*SS(NC)/CD(N)
 IF(II.NE.NROW)SSCC=CC(N)*SS(NR)/CD(N)
 IF(KK.NE.NLAY)SSCV=CV(N)*SS(NL)/CD(N)
 VN=V(N)/CD(N)
 SS(N)=VN-SSCR-SSCC-SSCV
 160 CONTINUE
 !C-------SKIP OVER OTHER PRECONDITIONING TYPES
 GO TO 199
 165 CONTINUE

 !C-------POLYNOMIAL PRECONDITIONING
 DO 170 N=1,NODES
 V(N)=RHS(N)
 170 CONTINUE
 CALL SPCG2E(IBOUND,RHS,HCOF,CR,CC,CV,V,SS,C2,NORM,NCOL,NROW,NLAY,NODES)
 CALL SPCG2E(IBOUND,RHS,HCOF,CR,CC,CV,SS,V,C1,NORM,NCOL,NROW,NLAY,NODES)
 CALL SPCG2E(IBOUND,RHS,HCOF,CR,CC,CV,V,SS,C0,NORM,NCOL,NROW,NLAY,NODES)
 199 CONTINUE

 !C-------CALCULATE P OF THE CONJUGATE GRADIENT ALGORITHM
 SROLD=SRNEW
 SRNEW=DZERO
 DO 200 N=1,NODES
   IF(IBOUND(N).LE.0)GO TO 200
   SRNEW=SRNEW+SS(N)*RHS(N)
 200 CONTINUE

 IF(IITER.EQ.1)THEN
    DO 205 N=1,NODES
 205    P(N)=SS(N)
 ELSE
    DO 210 N=1,NODES
 210    P(N)=SS(N)+(SRNEW/SROLD)*P(N)
 ENDIF

 !C-------CALCULATE ALPHA OF THE CONJUGATE GRADIENT ROUTINE.
 !C-------FOR THE DENOMINATOR OF ALPHA, MULTIPLY THE MATRIX A BY THE
 !C-------VECTOR P, AND STORE IN V; THEN MULTIPLY P BY V.  STORE IN PAP.
 PAP=DZERO
 DO 290 K=1,NLAY
 DO 290 I=1,NROW
 DO 290 J=1,NCOL

 N=J+(I-1)*NCOL+(K-1)*NRC
 V(N)=0.
 IF(IBOUND(N).LE.0)GO TO 290

 NRN=N+NCOL
 NRL=N-NCOL
 NCN=N+1
 NCL=N-1
 NLN=N+NRC
 NLL=N-NRC

 NCF=N
 NCD=NCL
 NRB=NRL
 NRH=N
 NLS=N
 NLZ=NLL

 B=DZERO
 IF(I.NE.1)B=CC(NRB)
 H=DZERO
 IF(I.NE.NROW)H=CC(NRH)
 D=DZERO
 IF(J.NE.1)D=CR(NCD)
 F=DZERO
 IF(J.NE.NCOL)F=CR(NCF)
 Z=DZERO
 IF(K.NE.1)Z=CV(NLZ)
 S=DZERO
 IF(K.NE.NLAY)S=CV(NLS)

 IF(NORM.EQ.0)PN=P(N)
 IF(NORM.EQ.1)PN=DZERO
 BHNEW=DZERO
 HHNEW=DZERO
 DHNEW=DZERO
 FHNEW=DZERO
 ZHNEW=DZERO
 SHNEW=DZERO
 IF(NRL.GT.0)BHNEW=B*(P(NRL)-PN)
 IF(NRN.LE.NODES)HHNEW=H*(P(NRN)-PN)
 IF(NCL.GT.0)DHNEW=D*(P(NCL)-PN)
 IF(NCN.LE.NODES)FHNEW=F*(P(NCN)-PN)
 IF(NLL.GT.0)ZHNEW=Z*(P(NLL)-PN)
 IF(NLN.LE.NODES)SHNEW=S*(P(NLN)-PN)

 !C-------CALCULATE THE PRODUCT OF MATRIX A AND VECTOR P AND STORE
 !C-------RESULT IN V.
 PN=HCOF(N)*P(N)
 IF(NORM.EQ.1)PN=-P(N)
 VN=ZHNEW+BHNEW+DHNEW+PN+FHNEW+HHNEW+SHNEW
 V(N)=VN
 PAP=PAP+P(N)*VN
 290 CONTINUE

 !C-------CALCULATE ALPHA
 ALPHA=SRNEW/PAP

 !C-------CALCULATE NEW HEADS AND RESIDUALS, AND SAVE THE LARGEST
 !C-------CHANGE IN HEAD AND THE LARGEST VALUE OF THE RESIDUAL.
 DO 300 K=1,NLAY
 DO 300 I=1,NROW
 DO 300 J=1,NCOL

 N=J+(I-1)*NCOL+(K-1)*NRC
 IF(IBOUND(N).LE.0)GO TO 300

 !C-------HEAD
 HCHGN=ALPHA*P(N)
 IF(DABS(HCHGN).GT.ABS(BIGH))THEN
     BIGH=HCHGN
     IH=I
     JH=J
 !write(*,*) 'k=',k
     KH=K
     NH=N
 ENDIF
 HNEW(N)=HNEW(N)+HCHGN

 !C--------RESIDUAL(V IS THE PRODUCT OF MATRIX A AND VECTOR P)
 RCHGN=-ALPHA*V(N)
 RHS(N)=RHS(N)+RCHGN
 IF(ABS(RHS(N)).GT.ABS(BIGR))THEN
     BIGR=RHS(N)
     IR=I
     JR=J
     KR=K
     NR=N
 ENDIF
 300 CONTINUE

 !C-------UNSCALE LARGEST CHANGE IN HEAD AND LARGEST RESIDUAL, AND
 !C-------CHECK THE CONVERGENCE CRITERION
  IF(NORM.EQ.1)THEN
     BIGH=BIGH/SQRT(-HCOF(NH))
     BIGR=BIGR*SQRT(-HCOF(NR))
  ENDIF

 !#external criteria
  IF(MXITER.EQ.1)THEN
    IF(ABS(BIGH).LE.HCLOSE.AND.ABS(BIGR).LE.RCLOSE)ICNVG=1
  ELSE
    IF(IITER.EQ.1.AND.ABS(BIGH).LE.HCLOSE.AND.ABS(BIGR).LE.RCLOSE)ICNVG=1
  ENDIF
 !#internal criteria
  IF(ABS(BIGH).LE.HCLOSE.AND.ABS(BIGR).LE.RCLOSE)IICNVG=1

 !C-------STORE THE LARGEST UNSCALED HEAD CHANGE AND RESIDUAL VALUE
 !C-------(THIS ITERATION) AND THEIR LOCATIONS.
 II=NITER
 HCHG=BIGH

 LHCH(1)=KH
 LHCH(2)=IH
 LHCH(3)=JH
 RCHG=BIGR
 LRCH(1)=KR
 LRCH(2)=IR
 LRCH(3)=JR

 IF(IECHO.NE.-2)CALL PCG1CVG(LHCH,LRCH,HCHG,RCHG,NITER,IECHO,KITER,NICNVG,RELAX)

 !C-------GO TO NEXT INTERNAL ITERATION IF CONVERGENCE HAS NOT BEEN
 !C-------REACHED AND IITER IS LESS THAN ITER1
 IF(MXITER.EQ.1)THEN
   IF(ICNVG.EQ.0.AND.IITER.LT.ITER1)GO TO 153
 ELSE
   IF(IICNVG.EQ.0.AND.IITER.LT.ITER1)GO TO 153
 ENDIF

 !C-------UNSCALE CR,CC,CV AND HNEW
 IF(NORM.EQ.1)THEN
 DO 310 N=1,NODES
 IF(IBOUND(N).EQ.0)GO TO 310
 HHCOF=SQRT(-HCOF(N))
 IF(N.LE.(NODES-NCOL).AND.CC(N).GT.0.)CC(N)=CC(N)*(HHCOF*(SQRT(-HCOF(N+NCOL))))
 IF(N.LE.(NODES-1).AND.CR(N).GT.0.)CR(N)=CR(N)*(HHCOF*(SQRT(-HCOF(N+1))))
 !IF(N.LE.(NODES-NRC).AND.CV(N).GT.0.)CV(N)=CV(N)*(HHCOF*(SQRT(-HCOF(N+NRC))))
 IF(N.LE.(NODES-NRC))THEN
  IF(CV(N).GT.0.)CV(N)=CV(N)*(HHCOF*(SQRT(-HCOF(N+NRC))))
 ENDIF
 HNEW(N)=HNEW(N)/HHCOF
 310 CONTINUE
 ENDIF

 IF(ICNVG.EQ.0 .AND. KITER.NE.MXITER)GO TO 600
 IF(MUTPCG.NE.0)GO TO 600

 600 CONTINUE

 !## reset counter if internal solution has not been met!
 IF(IICNVG.EQ.0)THEN
  NICNVG=0
 ELSE
  !## count internal solutions
  NICNVG=NICNVG+1
 ENDIF

 CALL PCG1CVG(LHCH,LRCH,HCHG,RCHG,NITER,IECHO,KITER,NICNVG,RELAX)

 END SUBROUTINE PCG2AP

 !###====================================================================
 SUBROUTINE SPCG2E(IBOUND,RHS,HCOF,CR,CC,CV,VIN,VOUT,C,NORM,NCOL,NROW,NLAY,NODES)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NODES,NCOL,NROW,NLAY,NORM
 INTEGER:: I,J,K,NRC,N,NRN,NRL,NCN,NCL,NLN,NLL,NCF,NCD,NRB,NRH,NLS,NLZ
 REAL(KIND=DP_KIND) :: VN,CRHS,Z,B,D,F,H,S,ZV,BV,DV,FV,HV,SV,DZERO
 REAL(KIND=DP_KIND),INTENT(IN) :: C
 INTEGER,DIMENSION(NODES),INTENT(IN) :: IBOUND
 REAL(KIND=DP_KIND),DIMENSION(NODES),INTENT(IN) :: CR,CC
 REAL(KIND=DP_KIND),DIMENSION(NODES),INTENT(INOUT) :: RHS,VIN,VOUT,HCOF
 REAL(KIND=DP_KIND),DIMENSION(MAX(1,NODES-(NROW*NCOL))),INTENT(IN) :: CV

 DZERO=0.
 NRC=NROW*NCOL
 DO 290 K=1,NLAY
 DO 290 I=1,NROW
 DO 290 J=1,NCOL

 N=J+(I-1)*NCOL+(K-1)*NRC
 VOUT(N)=0.
 IF(IBOUND(N).LE.0)GO TO 290

 NRN=N+NCOL
 NRL=N-NCOL
 NCN=N+1
 NCL=N-1
 NLN=N+NRC
 NLL=N-NRC

 NCF=N
 NCD=NCL
 NRB=NRL
 NRH=N
 NLS=N
 NLZ=NLL

 B=DZERO
 BV=DZERO
 IF(I.NE.1.AND.IBOUND(NRL).GE.0)THEN
   B=CC(NRB)
   BV=B*VIN(NRL)
 ENDIF
 H=DZERO
 HV=DZERO
 IF(I.NE.NROW.AND.IBOUND(NRN).GE.0)THEN
   H=CC(NRH)
   HV=H*VIN(NRN)
 ENDIF
 D=DZERO
 DV=DZERO
 IF(J.NE.1.AND.IBOUND(NCL).GE.0)THEN
   D=CR(NCD)
   DV=D*VIN(NCL)
 ENDIF
 F=DZERO
 FV=DZERO
 IF(J.NE.NCOL.AND.IBOUND(NCN).GE.0)THEN
   F=CR(NCF)
   FV=F*VIN(NCN)
 ENDIF
 Z=DZERO
 ZV=DZERO
 IF(K.NE.1.AND.IBOUND(NLL).GE.0)THEN
   Z=CV(NLZ)
   ZV=Z*VIN(NLL)
 ENDIF
 S=DZERO
 SV=DZERO
 IF(K.NE.NLAY.AND.IBOUND(NLN).GE.0)THEN
   S=CV(NLS)
   SV=S*VIN(NLN)
 ENDIF

 !C-------CALCULATE THE PRODUCT OF MATRIX A AND VECTOR VIN AND STORE
 !C------ RESULT IN VOUT
 VN=HCOF(N)*VIN(N)
 IF(NORM.EQ.1)VN=-VIN(N)
 CRHS=C*RHS(N)
 VOUT(N)=CRHS+ZV+BV+DV+VN+FV+HV+SV
 290 CONTINUE

 END SUBROUTINE SPCG2E

 !###====================================================================
 SUBROUTINE PCG1CVG(LHCH,LRCH,HCHG,RCHG,NITER,IECHO,KITER,NICNVG,RELAX)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: HCHG,RCHG,RELAX
 INTEGER,INTENT(IN) :: NITER,IECHO,KITER,NICNVG
 INTEGER,INTENT(IN),DIMENSION(3)  :: LHCH,LRCH
 CHARACTER(LEN=20),DIMENSION(2) :: LRC
 CHARACTER(LEN=256) :: LINE

 LRC(1)='('//TRIM(ITOS(LHCH(1)))//'-'//TRIM(ITOS(LHCH(2)))//'-'//TRIM(ITOS(LHCH(3)))//')'
 LRC(2)='('//TRIM(ITOS(LRCH(1)))//'-'//TRIM(ITOS(LRCH(2)))//'-'//TRIM(ITOS(LRCH(3)))//')'

 WRITE(LINE,'(2E15.7,2(A,I4),A,I3,A,F5.3,A)') HCHG,RCHG,' m/m3 (inner: ',NITER,'; outer: ',KITER,'; nconv: ',NICNVG,'; relax: ',RELAX,')'
 IF(IECHO.EQ.1)CALL WINDOWOUTSTATUSBAR(4,'Solving Residual Change: '//TRIM(LINE))
 IF(IECHO.LT.0)WRITE(*,'(1X,A)') TRIM(LINE)

 END SUBROUTINE PCG1CVG

 !###====================================================================
 SUBROUTINE PCG_HFB_FM(IPC,CC,CR,NROW,NCOL,FCT)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL
 REAL(KIND=DP_KIND),INTENT(IN) :: FCT
 INTEGER(KIND=1),INTENT(IN),DIMENSION(NCOL,NROW,2) :: IPC
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(NCOL,NROW) :: CC,CR
 INTEGER :: IROW,ICOL

 DO IROW=1,NROW; DO ICOL=1,NCOL

  !## place vertical wall
  IF(IPC(ICOL,IROW,1).EQ.INT(1,1))THEN
   IF(ICOL.LT.NCOL)CR(ICOL,IROW)=FCT*CR(ICOL,IROW)
  ENDIF

  !## place horizontal wall
  IF(IPC(ICOL,IROW,2).EQ.INT(1,1))THEN
   IF(IROW.LT.NROW)CC(ICOL,IROW)=FCT*CC(ICOL,IROW)
  ENDIF
 
! !## place vertical wall
! IF(IPC(ICOL,IROW,1).EQ.INT(1,1).AND.ICOL.LT.NCOL)THEN
!  I=I+1
!  WRITE(JUHFB,'(2I10)') I,JLINE
!  WRITE(JUHFB,'(2(F10.2,A1))') DELR(ICOL),',',DELC(IROW-1)
!  WRITE(JUHFB,'(2(F10.2,A1))') DELR(ICOL),',',DELC(IROW)
!  WRITE(JUHFB,'(A)') 'END'
! ENDIF
! !## place horizontal wall
! IF(IPC(ICOL,IROW,2).EQ.INT(1,1).AND.IROW.LT.NROW)THEN
!  I=I+1
!  WRITE(JUHFB,'(2I10)') I,JLINE
!  WRITE(JUHFB,'(2(F10.2,A1))') DELR(ICOL-1),',',DELC(IROW)
!  WRITE(JUHFB,'(2(F10.2,A1))') DELR(ICOL  ),',',DELC(IROW)
!  WRITE(JUHFB,'(A)') 'END'
! ENDIF

 ENDDO; ENDDO
 
 END SUBROUTINE PCG_HFB_FM

END MODULE MOD_PCG

