!!  Copyright (C) Stichting Deltares, 2005-2019.
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
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.Q*
!!
!!  Contact: imod.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands.
!!
MODULE MOD_PMANAGER_WQ

USE WINTERACTER
USE RESOURCE
USE MOD_PMANAGER_PAR
USE MOD_PMANAGER_UTL
USE MOD_PMANAGER_MF2005, ONLY : PMANAGER_SAVEMF2005_SIM
!USE IMODVAR
!USE MOD_IDF
USE MOD_UTL
!USE MOD_IDF_PAR
!USE MOD_ISG_PAR
!USE MOD_ISG_GRID
!USE MOD_ISG_UTL
!USE MOD_POLINT
!USE MOD_QKSORT
!USE MOD_ASC2IDF_HFB
!USE MOD_ASC2IDF_PAR
!USE MOD_ASC2IDF_UTL
!USE MOD_OSD

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ(FNAME,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=52) :: CDATE1,CDATE2
 CHARACTER(LEN=256) :: BNDFNAME
 INTEGER(KIND=8) :: ITIME,JTIME
 INTEGER :: IU,I,J,K,IPER,KPER,N,NSCL
 LOGICAL :: LDAYS,LEX
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 CHARACTER(LEN=256) :: LINE

 PMANAGER_SAVERUNWQ=.FALSE.

 ! Frans: lijstje PBMAN uitbreiden met iMOD WQ items....
 !    er is bijvoorbeeld PBMAN%IWINDOW,PBMAN%IDOUBLE
 ! Frans: modelwindow: gegevens uit tabblad halen. Daar wordt berekendt welke minmx x y het wordt.

 !!## overrule ipst if not as keyword given
 !IF(IBATCH.EQ.1.AND.PBMAN%IPEST.EQ.0)TOPICS(TPST)%IACT_MODEL=0

 !## get active packages, set default values
 ! Frans: moet deze aangepast voor SEAWAT?
 IF(.NOT.PMANAGER_GETPACKAGES())RETURN

 !## time information
 ISS=0; DO KPER=1,PRJNPER; IF(SIM(KPER)%DELT.NE.0.0D0)ISS=1; ENDDO

 !## get area of simulation / allocate arrays
! IF(.NOT.PMANAGER_SAVEMF2005_SIM(ISS,IBATCH))RETURN

 !# Check if obligatory packages are active for MT3D and SEAWAT
 IF(.NOT.PMANAGER_SAVERUNWQ_CHK())RETURN

 !## remove last timestep sinces it is the final date
 IF(PRJNPER.GT.1)PRJNPER=PRJNPER-1

 !## Prepare result model file
 CALL UTL_CREATEDIR(FNAME(1:INDEX(FNAME,'\',.TRUE.)-1))
 IF(IBATCH.EQ.0)THEN
  INQUIRE(FILE=FNAME,EXIST=LEX)
  IF(LEX)THEN
   CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to overwrite'//CHAR(13)//TRIM(FNAME),'Question')
   IF(WINFODIALOG(4).NE.1)RETURN
  ENDIF
 ENDIF
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='REPLACE',ACTION='WRITE,DENYREAD',FORM='FORMATTED')
 IF(IU.EQ.0)RETURN

 WRITE(IU,'(A)') '############################################################################'
 WRITE(IU,'(A)') '# iMOD run-file for SEAWAT '
 WRITE(IU,'(A)') '############################################################################'

 !## write Modflow Packages
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTGEN(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTDIS(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTBAS6(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTBCF6(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTOC(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTLPF(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTRCH(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTDRN(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTRIV(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTGHB(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTWEL(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTCHD(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTPCG(IU))RETURN

 !## write MT3D/Seawat Packages
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTBTN(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTADV(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTDSP(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTGCG(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTSSM(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTVDF(IU))RETURN

 CLOSE(IU)

 PMANAGER_SAVERUNWQ=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTBTN(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC,I
 INTEGER :: ICOL,IROW,LCBD,KPER
 REAL(KIND=DP_KIND) :: DELX,DELY


 PMANAGER_SAVERUNWQ_WRTBTN=.FALSE.

 WRITE(IU,'(/A)') '[BTN] # MT3DMS Basic Transport Package'
 WRITE(IU,'(A)') 'HEADING1 = -'
 WRITE(IU,'(A)') 'HEADING2 = -'
 WRITE(IU,'(A)') 'NROW = '//TRIM(ITOS(PRJIDF%NROW))
 WRITE(IU,'(A)') 'NCOL = '//TRIM(ITOS(PRJIDF%NCOL))

 WQFILE%NLAY=0; DO I=1,SIZE(PBMAN%ILAY); IF(PBMAN%ILAY(I).EQ.1)WQFILE%NLAY=WQFILE%NLAY+1; ENDDO
 WRITE(IU,'(A)') 'NLAY = '//TRIM(ITOS(WQFILE%NLAY)) ! PRJNLAY))
 WRITE(IU,'(A)') 'NPER = '//TRIM(ITOS(PRJNPER))
 WRITE(IU,'(A)') 'NCOMP = '//TRIM(ITOS(NSPECIES))
 WRITE(IU,'(A)') 'MCOMP = '   ! frans, moet nog
 WRITE(IU,'(A)') 'TUNIT =  D'
 WRITE(IU,'(A)') 'LUNIT =  M'
 WRITE(IU,'(A)') 'MUNIT =  K'
 WRITE(IU,'(A)') '#TRNOP = '   !given in GEN file, not obligatory

 !## define LAYCON_L
 LCBD=-1
 DO ILAY=1,PRJNLAY
  IF(ILAY.LT.PRJNLAY)THEN
   !## quasi-3d scheme
   IF(LQBD)THEN
    LCBD=1
   !## 3d no quasi confining bed
   ELSE
    LCBD=0
   ENDIF
  ELSE
   !## lowest layer has never a quasi-confining bed
   LCBD=0
  ENDIF
  WRITE(IU,'(A)') 'LAYCON_L'//TRIM(ITOS(PRJNLAY))//' = '//TRIM(ITOS(LCBD))
 ENDDO

 IF(PRJIDF%IEQ.EQ.0)THEN
   WQFILE%DELR_C='  DELR_C? = '//TRIM(RTOS(PRJIDF%DX,'E',7))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(5)))  
   WRITE(IU,'(A)') TRIM(WQFILE%DELR_C)
 ELSE
  DO ICOL=1,PRJIDF%NCOL
   DELX=PRJIDF%SX(ICOL)-PRJIDF%SX(ICOL-1)
   WQFILE%DELR_C='  DELR_C'//TRIM(ITOS(ICOL))//' = '//TRIM(RTOS(DELX,'E',7))
   WRITE(IU,'(A)') TRIM(WQFILE%DELR_C)
  ENDDO
 ENDIF

 IF(PRJIDF%IEQ.EQ.0)THEN
   WQFILE%DELR_R=  '  DELR_R? = '//TRIM(RTOS(PRJIDF%DY,'E',7))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(5)))
   WRITE(IU,'(A)') TRIM(WQFILE%DELR_R)
 ELSE
  DO IROW=1,PRJIDF%NROW
   DELY=PRJIDF%SY(IROW-1)-PRJIDF%SY(IROW)
   WQFILE%DELR_R='  DELR_R'//TRIM(ITOS(IROW))//' = '//TRIM(RTOS(DELY,'E',7))
   WRITE(IU,'(A)') TRIM(WQFILE%DELR_R)
  ENDDO
 ENDIF
 
 DO ILAY=1,PRJNLAY
  !## quasi-3d scheme add top aquifer modellayer
  !IF(LQBD.OR.ILAY.EQ.1)THEN
  ITOPIC=TTOP
  IF(ILAY.EQ.1)THEN
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'HTOP',ITOPIC,1,1,ILAY))RETURN
  ENDIF
  
  ITOPIC=TBOT
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'BOTmoetDZofTHKworden_L?',ITOPIC,1,1,ILAY))RETURN
 ENDDO

 DO ILAY=1,PRJNLAY
  ITOPIC=TPOR
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'PRSITY_L?',ITOPIC,1,1,ILAY))RETURN
 ENDDO

 DO ILAY=1,PRJNLAY
!  ITOPIC=TBTN
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'ICBUND_L?',ITOPIC,1,1,ILAY))RETURN
 ENDDO

 DO ILAY=1,PRJNLAY
!  ITOPIC=TBTN
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'SCONC_T?_L?',ITOPIC,2,1,ILAY))RETURN
 ENDDO

 WRITE(IU,'(A)') 'CINACT = '
 WRITE(IU,'(A)') 'THKMIN = '
 WRITE(IU,'(A)') 'IFMTCN = '
 WRITE(IU,'(A)') 'IFMTNP = '
 WRITE(IU,'(A)') 'IFMTRF = '
 WRITE(IU,'(A)') 'IFMTDP = '
 WRITE(IU,'(A)') 'SAVUCN = '
 WRITE(IU,'(A)') 'NPRS = '
 WRITE(IU,'(A)') 'TIMPRS = '
 WRITE(IU,'(A)') 'NPROBS = '
 WRITE(IU,'(A)') 'OBS_L? = '
 WRITE(IU,'(A)') 'CHKMAS = '
 WRITE(IU,'(A)') 'NPRMAS = '
 WRITE(IU,'(A)') 'PERLEN_P? = '
 WRITE(IU,'(A)') 'NSTP_P? = '
 WRITE(IU,'(A)') 'TSMULT_P? = '
 WRITE(IU,'(A)') 'TSLNGH_P? = '
 WRITE(IU,'(A)') 'DT0_P? = '
 WRITE(IU,'(A)') 'MXSTRN_P? = '
 WRITE(IU,'(A)') 'TTSMULT_P? = '
 WRITE(IU,'(A)') 'TTSMAX_P? = '

 PMANAGER_SAVERUNWQ_WRTBTN=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTBTN

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTADV(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTADV=.FALSE.

 WRITE(IU,'(/A)') '[ADV] # MT3DMS ADVection package'

 PMANAGER_SAVERUNWQ_WRTADV=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTADV

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTDSP(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTDSP=.FALSE.

 WRITE(IU,'(/A)') '[DSP] #MT3DMS Dispersion Package'

 PMANAGER_SAVERUNWQ_WRTDSP=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTDSP

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTGCG(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTGCG=.FALSE.

 WRITE(IU,'(/A)') '[GCG] # MT3DMS Generalized Conjugate Gradient Solver Package'

 PMANAGER_SAVERUNWQ_WRTGCG=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTGCG

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTSSM(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTSSM=.FALSE.

 WRITE(IU,'(A)') '[SSM] # MT3DMS Sink Source Mixing Package'

 PMANAGER_SAVERUNWQ_WRTSSM=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTSSM

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTVDF(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTVDF=.FALSE.

 WRITE(IU,'(/A)') '[VDF] # Variable-Density Flow '

 PMANAGER_SAVERUNWQ_WRTVDF=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTVDF

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTPCG(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTPCG=.FALSE.

 WRITE(IU,'(/A)') '[PCG] # MODFLOW Preconditioned Conjugate-Gradient Package'

 PMANAGER_SAVERUNWQ_WRTPCG=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTPCG

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTCHD(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTCHD=.FALSE.

 WRITE(IU,'(/A)') '[CHD]'

 PMANAGER_SAVERUNWQ_WRTCHD=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTCHD

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTWEL(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTWEL=.FALSE.

 WRITE(IU,'(/A)') '[WEL]'

 PMANAGER_SAVERUNWQ_WRTWEL=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTWEL

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTGHB(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTGHB=.FALSE.

 WRITE(IU,'(/A)') '[GHB]'

 PMANAGER_SAVERUNWQ_WRTGHB=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTGHB

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTRIV(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTRIV=.FALSE.

 WRITE(IU,'(/A)') '[RIV]'

 PMANAGER_SAVERUNWQ_WRTRIV=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTRIV

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTDRN(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTDRN=.FALSE.

 WRITE(IU,'(/A)') '[DRN]'

 PMANAGER_SAVERUNWQ_WRTDRN=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTDRN

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTRCH(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTRCH=.FALSE.

 WRITE(IU,'(A)') '[RCH]'

 PMANAGER_SAVERUNWQ_WRTRCH=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTRCH

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTLPF(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTLPF=.FALSE.

 WRITE(IU,'(/A)') '[LPF]'

 PMANAGER_SAVERUNWQ_WRTLPF=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTLPF

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTOC(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTOC=.FALSE.

 WRITE(IU,'(/A)') '[OC] # Output Control option'

 PMANAGER_SAVERUNWQ_WRTOC=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTOC

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTBCF6(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTBCF6=.FALSE.

 WRITE(IU,'(/A)') '[BCF6] # BLOCK CENTRED FLOW'

 WRITE(IU,'(A)') 'IBCFCB = 0 '
 WRITE(IU,'(A)') 'HDRY = -9999 '
 WRITE(IU,'(A)') 'IWDFLG = 0 '
 WRITE(IU,'(A)') 'WETFCT = 1 '
 WRITE(IU,'(A)') 'IWETIT = 1 '
 WRITE(IU,'(A)') 'IHDWET = 0 '
 WRITE(IU,'(A)') 'LTYPE_L? = 0 '
 WRITE(IU,'(A)') 'TRPY_L? = 1 '
 WRITE(IU,'(A)') 'SF1_L? = - '
 WRITE(IU,'(A)') 'TRAN_L? = - '
 WRITE(IU,'(A)') 'HY_L? = - '
 WRITE(IU,'(A)') 'VCONT_L? = - '
 WRITE(IU,'(A)') 'SF2_L? = - '
 WRITE(IU,'(A)') 'WETDRY_L? = - '


 PMANAGER_SAVERUNWQ_WRTBCF6=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTBCF6

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTBAS6(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTBAS6=.FALSE.

 WRITE(IU,'(/A)') '[BAS6] # MODFLOW BASic Package'

 DO ILAY=1,PRJNLAY
  ITOPIC=TBND
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'IBOUND_test_L?',ITOPIC,1,1,ILAY))RETURN
 ENDDO

 WRITE(IU,'(A)') 'HNOFLO    = -9999.0            '

 DO ILAY=1,PRJNLAY
  ITOPIC=TSHD
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'STRT_test_L?',ITOPIC,1,1,ILAY))RETURN
 ENDDO

 PMANAGER_SAVERUNWQ_WRTBAS6=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTBAS6

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTDIS(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: I,ILAY,ICOL,IROW,LCBD,ITOPIC,KPER
 REAL(KIND=DP_KIND) :: DELX,DELY

 PMANAGER_SAVERUNWQ_WRTDIS=.FALSE.

 WRITE(IU,'(/A)') '[DIS] # MODFLOW DIScretization Package'

 WRITE(IU,'(A)') 'NLAY = '//TRIM(ITOS(WQFILE%NLAY)) 
 WRITE(IU,'(A)') 'NROW = '//TRIM(ITOS(PRJIDF%NROW))
 WRITE(IU,'(A)') 'NCOL = '//TRIM(ITOS(PRJIDF%NCOL))
 WRITE(IU,'(A)') 'NPER = '//TRIM(ITOS(PRJNPER))
 WRITE(IU,'(A)') 'ITMUNI = 4' ! default
 WRITE(IU,'(A)') 'LENUNI = 2' ! default

 !## define LAYCON_L
 LCBD=-1
 DO ILAY=1,PRJNLAY
  IF(ILAY.LT.PRJNLAY)THEN
   !## quasi-3d scheme
   IF(LQBD)THEN
    LCBD=1
   !## 3d no quasi confining bed
   ELSE
    LCBD=0
   ENDIF
  ELSE
   !## lowest layer has never a quasi-confining bed
   LCBD=0
  ENDIF
  WRITE(IU,'(A)') 'LAYCON_L'//TRIM(ITOS(PRJNLAY))//' = '//TRIM(ITOS(LCBD))
 ENDDO

 WRITE(IU,'(A)') TRIM(WQFILE%DELR_C)
 WRITE(IU,'(A)') TRIM(WQFILE%DELR_R)

 DO ILAY=1,PRJNLAY
  !## quasi-3d scheme add top aquifer modellayer
  !IF(LQBD.OR.ILAY.EQ.1)THEN
  ITOPIC=TTOP
  IF(ILAY.EQ.1)THEN
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'TOP',ITOPIC,1,1,ILAY))RETURN
  ENDIF
  
  ITOPIC=TBOT
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'BOT_L?',ITOPIC,1,1,ILAY))RETURN
 ENDDO

 IF(MINVAL(SIM(1:PRJNPER)%DELT).EQ.MAXVAL(SIM(1:PRJNPER)%DELT))THEN
   WRITE(IU,'(A)') 'PERLEN_P? = '//TRIM(UTL_REALTOSTRING(MAXVAL(SIM(:)%DELT)))
 ELSE
   DO KPER=1,PRJNPER
    WRITE(IU,'(A)') 'PERLEN_P'//TRIM(ITOS(KPER))//' = '//TRIM(UTL_REALTOSTRING(SIM(KPER)%DELT))
   ENDDO
 ENDIF

 IF(MINVAL(SIM(1:PRJNPER)%NSTP).EQ.MAXVAL(SIM(1:PRJNPER)%NSTP))THEN
   WRITE(IU,'(A)') 'NSTP_P? = '//TRIM(ITOS(MAXVAL(SIM(1:PRJNPER)%NSTP)))
 ELSE
   DO KPER=1,PRJNPER
    WRITE(IU,'(A)') 'NSTP_P'//TRIM(ITOS(KPER))//' = '//TRIM(ITOS(SIM(KPER)%NSTP))
   ENDDO
 ENDIF

 WRITE(IU,'(A)') 'TSMULT_P? = 1'  ! frans: aanpassen?
 WRITE(IU,'(A)') 'SSTR_P? = TR'   ! frans: aanpassen?

 PMANAGER_SAVERUNWQ_WRTDIS=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTDIS

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTGEN(IU)
 !####====================================================================
 IMPLICIT NONE
 !CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME
 INTEGER,INTENT(IN) :: IU
 CHARACTER(LEN=256) :: LINE
 INTEGER :: KPER,ITOPIC

 PMANAGER_SAVERUNWQ_WRTGEN=.FALSE.
 WRITE(IU,'(/A)') '[GEN] # GENeral settings'
 MODELNAME=MODELNAME(1:INDEX(MODELNAME,'.RUN',.TRUE.)-1)
 WRITE(IU,'(A)') 'MODELNAME = '//CHAR(39)//TRIM(MODELNAME)//CHAR(39)            
 WRITE(IU,'(A)') 'WRITEHELP     =   F'
 WRITE(IU,'(A)') 'ECHODEFAULTS  =   F'
 WRITE(IU,'(A)') 'RESULT_DIR    =   '//CHAR(39)//TRIM(PREFVAL(1))//'\MODELS\'//TRIM(MODELNAME)//CHAR(39)
 WRITE(IU,'(A)') 'IDFDEBUG      =   F'
 IF(PBMAN%IFORMAT.EQ.4) WRITE(IU,'(A)') 'RUNTYPE       = SEAWAT'
 IF(PBMAN%IFORMAT.EQ.5) WRITE(IU,'(A)') 'RUNTYPE       = MT3DMS'
 
 !## Select active packages
 LINE=''
 IF(PBMAN%IFORMAT.EQ.4) THEN    ! for Seawat
  DO ITOPIC=1,MAXTOPICS
   SELECT CASE (ITOPIC)  ! Frans: check of dit klopt
     CASE (TWEL, TDRN, TRIV, TGHB, TCHD, TRCH, TEVT, TPCG)
         IF(TOPICS(ITOPIC)%IACT_MODEL.GT.0) LINE=TRIM(LINE)//TRIM(TOPICS(ITOPIC)%CMOD)//','
     CASE (TBND)
         IF(TOPICS(ITOPIC)%IACT_MODEL.GT.0) LINE=TRIM(LINE)//'BAS6,'   
     CASE (TKHV, TKDW)
         IF(TOPICS(ITOPIC)%IACT_MODEL.GT.0) LINE=TRIM(LINE)//'BCF6,'   
   END SELECT
  ENDDO
 ENDIF
 IF(PBMAN%IFORMAT.EQ.5) THEN    ! for MT3D
  DO ITOPIC=1,MAXTOPICS
   SELECT CASE (ITOPIC)  
     CASE (TGCG,TVDF)  !(SSM, DSP, TUDR)
!     CASE (TBTN, TADV, TGCG, TRCT, TVDF)  !(SSM, DSP, TUDR)
         IF(TOPICS(ITOPIC)%IACT_MODEL.GT.0) LINE=TRIM(LINE)//TRIM(TOPICS(ITOPIC)%CMOD)//','   
   END SELECT
  ENDDO
 ENDIF 
 LINE=(LINE(1:INDEX(LINE,',',.TRUE.)-1))  ! remove last comma
 WRITE(IU,'(A)') 'PACKAGES      =   '//TRIM(LINE) 
 
 WRITE(IU,'(A)') 'COORD_XLL     =   '//TRIM(UTL_REALTOSTRING(PRJIDF%XMIN)) 
 WRITE(IU,'(A)') 'COORD_YLL     =   '//TRIM(UTL_REALTOSTRING(PRJIDF%YMIN)) 
 WRITE(IU,'(A)') 'COORD_XUR     =   '//TRIM(UTL_REALTOSTRING(PRJIDF%XMAX)) 
 WRITE(IU,'(A)') 'COORD_YUR     =   '//TRIM(UTL_REALTOSTRING(PRJIDF%YMAX)) 

 !## look for first
 DO KPER=1,PRJNPER; IF(SIM(KPER)%DELT.GT.0.0D0)EXIT; ENDDO

 WRITE(IU,'(A)') 'START_YEAR    =   '//TRIM(ITOS(SIM(KPER)%IYR))
 WRITE(IU,'(A)') 'START_MONTH   =   '//TRIM(ITOS(SIM(KPER)%IMH))
 WRITE(IU,'(A)') 'START_DAY     =   '//TRIM(ITOS(SIM(KPER)%IDY))
 WRITE(IU,'(A)') 'START_HOUR    =   1'
 WRITE(IU,'(A)') 'START_MINUTE  =   1'
 WRITE(IU,'(A)') 'START_SECOND  =   1'

 PMANAGER_SAVERUNWQ_WRTGEN=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTGEN

 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_CHK()
 !###======================================================================
 IMPLICIT NONE
 
 INTEGER :: ITOPIC

 PMANAGER_SAVERUNWQ_CHK=.FALSE.

 !DO ITOPIC=1,MAXTOPICS
 !  IF(PBMAN%IFORMAT.EQ.4) THEN    ! for Seawat
 !   SELECT CASE (ITOPIC)  
 !    CASE (TBTN, TADV, TGCG, TRCT, TVDF) 
 !        IF(TOPICS(ITOPIC)%IACT_MODEL.EQ.0) THEN
 !          CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'To run Seawat you need to activate package '//TRIM(TOPICS(ITOPIC)%TNAME) ,'Error')
 !          RETURN
 !        ENDIF
 !   END SELECT
 !  ENDIF ! seawat   
 !  IF(PBMAN%IFORMAT.EQ.5) THEN    ! for MT3D
 !   SELECT CASE (ITOPIC)  
 !    CASE (TBTN, TADV, TGCG, TRCT, TVDF, TSSM, TDSP, TUDR)
 !        IF(TOPICS(ITOPIC)%IACT_MODEL.EQ.0) THEN
 !          CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'To run MT3D you need to activate package '//TRIM(TOPICS(ITOPIC)%TNAME) ,'Error')
 !          RETURN
 !        ENDIF
 !   END SELECT
 !  ENDIF ! MT3D   
 !ENDDO

 PMANAGER_SAVERUNWQ_CHK=.TRUE. 

 END FUNCTION PMANAGER_SAVERUNWQ_CHK

 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_U2DREL(IU,KEYNAME,ITOPIC,IPER,ISUBT,ISYS)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: KEYNAME
 INTEGER,INTENT(IN) :: IU,ITOPIC,IPER,ISUBT,ISYS
 CHARACTER(LEN=256) :: FNAME,LINE,LINE2
 INTEGER :: ICNST, ILAY
 REAL(KIND=DP_KIND) :: FCT, IMP, CNST
 
 PMANAGER_SAVERUNWQ_U2DREL=.FALSE.

 !## substitute variables
 ICNST =TOPICS(ITOPIC)%STRESS(IPER)%FILES(ISUBT,ISYS)%ICNST 
 CNST  =TOPICS(ITOPIC)%STRESS(IPER)%FILES(ISUBT,ISYS)%CNST  
 FCT   =TOPICS(ITOPIC)%STRESS(IPER)%FILES(ISUBT,ISYS)%FCT   
 IMP   =TOPICS(ITOPIC)%STRESS(IPER)%FILES(ISUBT,ISYS)%IMP   
 FNAME =TOPICS(ITOPIC)%STRESS(IPER)%FILES(ISUBT,ISYS)%FNAME 
 ILAY  =TOPICS(ITOPIC)%STRESS(IPER)%FILES(ISUBT,ISYS)%ILAY
 
 ! Macro’s can be used for ranges layers (_L), rows (_R), columns (_C), stress periods (_P), species (_T) and sub-systems (_S)
 LINE= UTL_CAP(KEYNAME,'U')
 IF(IPER.GT.0) LINE=TRIM(UTL_SUBST(LINE,'_P?','_P'//TRIM(ITOS(IPER))))
 IF(ISYS.GT.0) LINE=TRIM(UTL_SUBST(LINE,'_S?','_S'//TRIM(ITOS(ISYS))))
 IF(ILAY.GT.0) LINE=TRIM(UTL_SUBST(LINE,'_L?','_L'//TRIM(ITOS(ILAY))))
 
 IF(ICNST.EQ.1)THEN ; LINE2=TRIM(UTL_REALTOSTRING(CNST)) ; ELSE ; LINE2=TRIM(FNAME) ; ENDIF
 IF(FCT.EQ.1.0.AND.IMP.EQ.0.0)THEN
   WRITE(IU,'(A)') TRIM(LINE)//' = '//TRIM(LINE2)
 ELSE  
   WRITE(IU,'(A)') TRIM(LINE)//' = '//TRIM(UTL_REALTOSTRING(FCT))//' * '// TRIM(LINE2)//' + '//TRIM(UTL_REALTOSTRING(IMP))
 ENDIF
 
         
 PMANAGER_SAVERUNWQ_U2DREL=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_U2DREL
 
END MODULE MOD_PMANAGER_WQ
