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
USE IMODVAR
!USE MOD_IDF
!USE MOD_UTL
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

 ! Frans: voorbeeld skippen bij foute invoer
 !IF(PCG%PARTOPT.EQ.3.AND.TRIM(PCG%MRGFNAME).EQ.'')THEN
 ! CLOSE(IU); CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to specify a pointer IDF-file when selecting the RCB partition method.','Error')
 ! RETURN
 !ENDIF

 !!## overrule ipst if not as keyword given
 !IF(IBATCH.EQ.1.AND.PBMAN%IPEST.EQ.0)TOPICS(TPST)%IACT_MODEL=0

 !## get active packages, set default values
 ! Frans: moet deze aangepast voor SEAWAT?
 IF(.NOT.PMANAGER_GETPACKAGES())RETURN

 !IF(.NOT.PMANAGER_SAVEMF2005_SIM(ISS,IBATCH))  ....

 !# Check if obligatory packages are active for MT3D and SEAWAT
 IF(.NOT.PMANAGER_SAVERUNWQ_CHK())RETURN

 !DO I=1,MAXTOPICS
 ! SELECT CASE (I)
 !  CASE (TFHB,TUZF,TMNW,TSFR,TLAK)
 !   IF(TOPICS(I)%IACT_MODEL.EQ.1)THEN
 !    CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'You cannot use the package '//TRIM(TOPICS(I)%TNAME)//CHAR(13)// &
 !     'to save for a RUN-file. Select the option MODFLOW2005 instead','Information')
 !    RETURN
 !   ENDIF
 ! END SELECT
 !ENDDO

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
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTBTN=.FALSE.

 WRITE(IU,'(/A)') '[BTN] # MT3DMS Basic Transport Package'

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
  IF(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%ICNST.EQ.1)THEN
    WRITE(IU,'(A)') 'IBOUND_L'//TRIM(ITOS(ILAY))//' = '//TRIM(UTL_REALTOSTRING(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%CNST))
  ELSE
    WRITE(IU,'(A)') 'IBOUND_L'//TRIM(ITOS(ILAY))//' = '//TRIM(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%FNAME)
  ENDIF
 ENDDO

 WRITE(IU,'(A)') 'HNOFLO    = -9999.0            '

 DO ILAY=1,PRJNLAY
  ITOPIC=TSHD
  IF(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%ICNST.EQ.1)THEN
    WRITE(IU,'(A)') 'STRT_L'//TRIM(ITOS(ILAY))//' = '//TRIM(UTL_REALTOSTRING(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%CNST))
  ELSE
    WRITE(IU,'(A)') 'STRT_L'//TRIM(ITOS(ILAY))//' = '//TRIM(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%FNAME)
  ENDIF
 ENDDO

 PMANAGER_SAVERUNWQ_WRTBAS6=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTBAS6

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTDIS(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: I,ILAY,ICOL,IROW,LCBD,N,ITOPIC,KPER
 REAL(KIND=DP_KIND) :: DELX,DELY

 PMANAGER_SAVERUNWQ_WRTDIS=.FALSE.

 WRITE(IU,'(/A)') '[DIS] # MODFLOW DIScretization Package'

 WRITE(IU,'(A)') 'MODELNAME = '//CHAR(39)//TRIM(MODELNAME)//CHAR(39)
 N=0; DO I=1,SIZE(PBMAN%ILAY); IF(PBMAN%ILAY(I).EQ.1)N=N+1; ENDDO
 WRITE(IU,'(A)') 'NLAY = '//TRIM(ITOS(N)) ! PRJNLAY))
 WRITE(IU,'(A)') 'NROW = '//TRIM(ITOS(PRJIDF%NROW))
 WRITE(IU,'(A)') 'NCOL = '//TRIM(ITOS(PRJIDF%NCOL))
 WRITE(IU,'(A)') 'NPER = '//TRIM(ITOS(PRJNPER))
 WRITE(IU,'(A)') 'ITMUNI = 4' ! default
 WRITE(IU,'(A)') 'LENUNI = 2' ! default
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
  WRITE(IU,'(A)') 'LAYCBD_L'//TRIM(ITOS(PRJNLAY))//' = '//TRIM(ITOS(LCBD))
 ENDDO

 IF(PRJIDF%IEQ.EQ.0)THEN
   WRITE(IU,'(A)') '  DELR_C? = '//TRIM(RTOS(PRJIDF%DX,'E',7))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(5)))
 ELSE
  DO ICOL=1,PRJIDF%NCOL
   DELX=PRJIDF%SX(ICOL)-PRJIDF%SX(ICOL-1)
   WRITE(IU,'(A)')'  DELR_C'//TRIM(ITOS(ICOL))//' = '//TRIM(RTOS(DELX,'E',7))
  ENDDO
 ENDIF
 IF(PRJIDF%IEQ.EQ.0)THEN
   WRITE(IU,'(A)') '  DELR_R? = '//TRIM(RTOS(PRJIDF%DY,'E',7))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(5)))
 ELSE
  DO IROW=1,PRJIDF%NROW
   DELY=PRJIDF%SY(IROW-1)-PRJIDF%SY(IROW)
   WRITE(IU,'(A)')'  DELR_R'//TRIM(ITOS(IROW))//' = '//TRIM(RTOS(DELY,'E',7))
  ENDDO
 ENDIF

 DO ILAY=1,PRJNLAY
  ITOPIC=TTOP
  !ITOPIC=2
  !## quasi-3d scheme add top aquifer modellayer
  !IF(LQBD.OR.ILAY.EQ.1)THEN
  IF(ILAY.EQ.1)THEN
    IF(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%ICNST.EQ.1)THEN
      WRITE(IU,'(A)') 'TOP = '//TRIM(UTL_REALTOSTRING(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%CNST))
    ELSE
      WRITE(IU,'(A)') 'TOP = '//TRIM(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%FNAME)
    ENDIF
  ENDIF
  ITOPIC=TBOT
  !ITOPIC=3
  IF(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%ICNST.EQ.1)THEN
    WRITE(IU,'(A)') 'BOT_L'//TRIM(ITOS(ILAY))//' = '//TRIM(UTL_REALTOSTRING(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%CNST))
  ELSE
    WRITE(IU,'(A)') 'BOT_L'//TRIM(ITOS(ILAY))//' = '//TRIM(TOPICS(ITOPIC)%STRESS(1)%FILES(1,ILAY)%FNAME)
  ENDIF
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
 INTEGER :: KPER

 PMANAGER_SAVERUNWQ_WRTGEN=.FALSE.
 WRITE(IU,'(/A)') '[GEN] # GENeral settings'
 WRITE(IU,'(A)') 'MODELNAME = '//CHAR(39)//TRIM(MODELNAME)//CHAR(39)
 WRITE(IU,'(A)') 'WRITEHELP     =   F'
 WRITE(IU,'(A)') 'ECHODEFAULTS  =   F'
 WRITE(IU,'(A)') 'RESULT_DIR    =   '//CHAR(39)//TRIM(PREFVAL(1))//'\MODELS\'//TRIM(MODELNAME)//CHAR(39)
 WRITE(IU,'(A)') 'IDFDEBUG      =   F'
 PBMAN%RUNTYPE='SEAWAT' ; IF(PBMAN%IFORMAT.EQ.5) PBMAN%RUNTYPE='MT3DMS'
 WRITE(IU,'(A)') 'RUNTYPE       =   '//PBMAN%RUNTYPE
 WRITE(IU,'(A)') 'PACKAGES      =   -'
 WRITE(IU,'(A)') 'COORD_XLL     =   '//TRIM(UTL_REALTOSTRING(PBMAN%XMIN))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(1)))   ! Frans vraag: of gebruiken SUBMODEL(x)
 WRITE(IU,'(A)') 'COORD_YLL     =   '//TRIM(UTL_REALTOSTRING(PBMAN%YMIN))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(2)))
 WRITE(IU,'(A)') 'COORD_XUR     =   '//TRIM(UTL_REALTOSTRING(PBMAN%XMAX))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(3)))
 WRITE(IU,'(A)') 'COORD_YUR     =   '//TRIM(UTL_REALTOSTRING(PBMAN%YMAX))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(4)))

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
 ! actie: nog opschonen
 IMPLICIT NONE
 !CHARACTER(LEN=*),INTENT(IN) :: FNAME
 !INTEGER,INTENT(IN) :: IBATCH
 !CHARACTER(LEN=52) :: CDATE1,CDATE2
 !CHARACTER(LEN=256) :: BNDFNAME
 !INTEGER(KIND=8) :: ITIME,JTIME
 !INTEGER :: IU,I,J,K,IPER,KPER,N,NSCL
 !LOGICAL :: LDAYS,LEX
 !TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 !CHARACTER(LEN=256) :: LINE

 PMANAGER_SAVERUNWQ_CHK=.FALSE.
! VDF, BTN, ADV ,DSP ,SSM, FTL, GCG, RCT, UDR
! RCT en UDR zijn reactie packages; die zijn wel echt optioneel. En per simulatie kan maar 1 van deze 2 gebruikt worden.
! dus als het makkelijker is zou je gewoon alle MT3DMS packages verplicht kunnen maken.

! BAS6, DIS, WEL, DRN, RIV, GHB, CHD, LPF, BCF6, RCH, EVT, OC, VDF, PCG, , BTN
! verplicht: BAS6, DIS, OC, PCG en  LPF of BCF6

 ! NPER > 0

 IF(.NOT.LPCG)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'It is compulsory to add a solver, e.g. PCG','Error')
  RETURN
 ENDIF

 PMANAGER_SAVERUNWQ_CHK=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_CHK


END MODULE MOD_PMANAGER_WQ
