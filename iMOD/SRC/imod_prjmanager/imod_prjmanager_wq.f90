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
 IF(.NOT.PMANAGER_GETPACKAGES(IBATCH))RETURN

 !## time information
 ISS=0; DO KPER=1,PRJNPER; IF(SIM(KPER)%DELT.NE.0.0D0)ISS=1; ENDDO
 !## remove last timestep sinces it is the final date 
 IF(PRJNPER.GT.1)PRJNPER=PRJNPER-1

 !# general preparations
 IF(.NOT.PMANAGER_SAVERUNWQ_INIT())RETURN

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

 !## write Start Packages
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTGEN(IU))RETURN

 !## write Modflow Packages
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
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTDIS(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTBTN(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTADV(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTDSP(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTGCG(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTSSM(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTVDF(IU))RETURN

 IF(.NOT.PMANAGER_SAVERUNWQ_WRTRCT(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTUDR(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTFTL(IU))RETURN


 CLOSE(IU)

 PMANAGER_SAVERUNWQ=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTBTN(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC,I
 INTEGER :: ICOL,IROW,LCBD,KPER,MCOMP

 PMANAGER_SAVERUNWQ_WRTBTN=.FALSE.

 WRITE(IU,'(/A)') '[BTN] # MT3DMS Basic Transport Package'
 WRITE(IU,'(A)') 'HEADING1 = -'
 WRITE(IU,'(A)') 'HEADING2 = -'

 !WQFILE%NLAY=0; DO I=1,SIZE(PBMAN%ILAY); IF(PBMAN%ILAY(I).EQ.1)WQFILE%NLAY=WQFILE%NLAY+1; ENDDO
! WRITE(IU,'(A)') 'NLAY = '//TRIM(ITOS(WQFILE%NLAY)) ! PRJNLAY))
 WRITE(IU,'(A)') 'NLAY = '//TRIM(ITOS(PRJNLAY))
 WRITE(IU,'(A)') 'NROW = '//TRIM(ITOS(PRJIDF%NROW))
 WRITE(IU,'(A)') 'NCOL = '//TRIM(ITOS(PRJIDF%NCOL))

 WRITE(IU,'(A)') 'NPER  = '//TRIM(ITOS(PRJNPER))
 WRITE(IU,'(A)') 'NCOMP = '//TRIM(ITOS(NSPECIES))
 MCOMP=0
 DO I=1,NSPECIES ; IF(SPECIES(I)%IMOBILE.EQ.2) MCOMP=MCOMP+1 ;  ENDDO
 WRITE(IU,'(A)') 'MCOMP = '//TRIM(ITOS(MCOMP))
 WRITE(IU,'(A)') 'TUNIT = '//TRIM(WQ%BTN%TUNIT)//' # default'  
 WRITE(IU,'(A)') 'LUNIT = '//TRIM(WQ%BTN%LUNIT)//' # default'  
 WRITE(IU,'(A)') 'MUNIT = '//TRIM(WQ%BTN%MUNIT)//' # default'  
 
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

 !# Write DELR_C and DELR_R
 WRITE(IU,'(A)') TRIM(WQFILE%DELR_C)
 WRITE(IU,'(A)') TRIM(WQFILE%DELR_R)

 DO ILAY=1,PRJNLAY
  !## quasi-3d scheme add top aquifer modellayer
  !IF(LQBD.OR.ILAY.EQ.1)THEN
  ITOPIC=TTOP
  IF(ILAY.EQ.1)THEN
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'HTOP',ITOPIC,0,1,0,ILAY,0))RETURN
  ENDIF
  
  ITOPIC=TTHK
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'DZ_L?',ITOPIC,0,1,0,ILAY,0))RETURN
 ENDDO

 DO ILAY=1,PRJNLAY
  ITOPIC=TPOR
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'PRSITY_L?',ITOPIC,0,1,0,ILAY,0))RETURN
 ENDDO

 DO ILAY=1,PRJNLAY
  ITOPIC=TCBI
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'ICBUND_L?',ITOPIC,0,1,0,ILAY,0))RETURN
 ENDDO

 DO ISPECIES=1,NSPECIES
  DO ILAY=1,PRJNLAY
   ITOPIC=TSCO
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'SCONC_T?_L?',ITOPIC,1,0,0,ILAY,ISPECIES))RETURN
  ENDDO
 ENDDO 

 WRITE(IU,'(A)') 'CINACT = '//TRIM(UTL_REALTOSTRING(WQ%BTN%CINACT))//' # default'
 WRITE(IU,'(A)') 'THKMIN = '//TRIM(UTL_REALTOSTRING(WQ%BTN%THKMIN))//' # default'
 WRITE(IU,'(A)') 'IFMTCN = '//TRIM(ITOS(WQ%BTN%IFMTCN))//' # default'
 WRITE(IU,'(A)') 'IFMTNP = '//TRIM(ITOS(WQ%BTN%IFMTNP))//' # default'
 WRITE(IU,'(A)') 'IFMTRF = '//TRIM(ITOS(WQ%BTN%IFMTRF))//' # default'
 WRITE(IU,'(A)') 'IFMTDP = '//TRIM(ITOS(WQ%BTN%IFMTDP))//' # default'
 WRITE(IU,'(A)') 'SAVUCN = '//TRIM(LTOS(WQ%BTN%SAVUCN,1))//' # default'
 WRITE(IU,'(A)') 'NPRS =   '//TRIM(ITOS(PBMAN%NPRS))
 WRITE(IU,'(A)') 'TIMPRS = '//TRIM(UTL_REALTOSTRING(PBMAN%TIMPRS))
 WRITE(IU,'(A)') 'NPROBS = '//TRIM(ITOS(PBMAN%NPROBS))
 DO ILAY=1,PRJNLAY
  ITOPIC=TOBS
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'OBS_L?',ITOPIC,0,1,0,ILAY,0))RETURN
 ENDDO
 WRITE(IU,'(A)') 'CHKMAS = '//TRIM(LTOS(WQ%BTN%CHKMAS,1))//' # default'
 WRITE(IU,'(A)') 'NPRMAS = '//TRIM(ITOS(PBMAN%NPRMAS))
 
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

 WRITE(IU,'(A)') 'TSMULT_P? = to do'
 WRITE(IU,'(A)') 'TSLNGH_P? = to do'
 WRITE(IU,'(A)') 'DT0_P? = to do'
 WRITE(IU,'(A)') 'MXSTRN_P? = to do'
 WRITE(IU,'(A)') 'TTSMULT_P? = to do'
 WRITE(IU,'(A)') 'TTSMAX_P? = to do'

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
 WRITE(IU,'(A)') 'MIXELM = '//TRIM(ITOS(WQ%ADV%MIXELM))
 WRITE(IU,'(A)') 'PERCEL = '//TRIM(UTL_REALTOSTRING(WQ%ADV%PERCEL))
 WRITE(IU,'(A)') 'MXPART = '//TRIM(ITOS(WQ%ADV%MXPART))
 WRITE(IU,'(A)') 'NADVFD = '//TRIM(ITOS(WQ%ADV%NADVFD))
 WRITE(IU,'(A)') '#ITRACK = '
 WRITE(IU,'(A)') '#WD = '
 WRITE(IU,'(A)') '#DCEPS = '
 WRITE(IU,'(A)') '#NPLANE = '
 WRITE(IU,'(A)') '#NPL = '
 WRITE(IU,'(A)') '#NPH = '
 WRITE(IU,'(A)') '#NPMIN = '
 WRITE(IU,'(A)') '#NPMAX = '
 WRITE(IU,'(A)') '#INTERP = '
 WRITE(IU,'(A)') '#NLSINK = '
 WRITE(IU,'(A)') '#NPSINK = '
 WRITE(IU,'(A)') '#DCHMOC = '
 
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
 DO ILAY=1,PRJNLAY
  ITOPIC=TDSP
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'TRPT_L?',ITOPIC,0,1,0,ILAY,0))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'TRPT_L?',ITOPIC,0,2,0,ILAY,0))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'TRPV_L?',ITOPIC,0,3,0,ILAY,0))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'DMCOEF_L?',ITOPIC,0,4,0,ILAY,0))RETURN
 ENDDO
 
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
 INTEGER :: ILAY,ITOPIC,ISPECIES,KPER

 PMANAGER_SAVERUNWQ_WRTSSM=.FALSE.

 WRITE(IU,'(A)') '[SSM] # MT3DMS Sink Source Mixing Package'
 WRITE(IU,'(A)') 'MXSS = '//TRIM(ITOS(PBMAN%MXSS))
 WRITE(IU,'(A)') 'CRCH_T?_P? = '
 WRITE(IU,'(A)') 'CEVT_T?_P? = '
 WRITE(IU,'(A)') 'CCHD_T?_P?_L? = '
 WRITE(IU,'(A)') 'CWEL_T?_P?_L? = '
 WRITE(IU,'(A)') 'CDRN_T?_P?_L? = '
 WRITE(IU,'(A)') 'CRIV_T?_P?_L? = '
 WRITE(IU,'(A)') 'CGHB_T?_P?_L? = '
 WRITE(IU,'(A)') 'CMAL_T?_P?_L? = '
 WRITE(IU,'(A)') 'CTVC_T?_P?_L? = '
 
 
 DO KPER=1,PRJNPER ; DO ISPECIES=1,NSPECIES
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CRCH_T?_P?',TRCH,KPER,0,0,ILAY,ISPECIES))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CEVT_T?_P?',TRCH,KPER,0,0,ILAY,ISPECIES))RETURN
  DO ILAY=1,PRJNLAY
    IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CCHD_T?_P?_L?',TCHD,KPER,0,0,ILAY,ISPECIES))RETURN
    !IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CWEL_T?_P?_L?',TCHD,KPER,0,0,ILAY,ISPECIES))RETURN
    !IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CDRN_T?_P?_L?',TCHD,KPER,0,0,ILAY,ISPECIES))RETURN
    !IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CRIV_T?_P?_L?',TCHD,KPER,0,0,ILAY,ISPECIES))RETURN
    !IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CGHB_T?_P?_L?',TCHD,KPER,0,0,ILAY,ISPECIES))RETURN
    !IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CTVC_T?_P?_L?',TCHD,KPER,0,0,ILAY,ISPECIES))RETURN
  ENDDO    
 ENDDO ; ENDDO

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
 WRITE(IU,'(A)') 'MTDNCONC = '
 WRITE(IU,'(A)') 'MFNADVFD = '
 WRITE(IU,'(A)') 'NSWTCPL = '
 WRITE(IU,'(A)') ' = '
 WRITE(IU,'(A)') ' = '
 WRITE(IU,'(A)') ' = '
 WRITE(IU,'(A)') 'IWTABLE = '
 WRITE(IU,'(A)') 'DENSEMIN = '
 WRITE(IU,'(A)') 'DENSEMAX = '
 WRITE(IU,'(A)') 'DNSCRIT = '
 WRITE(IU,'(A)') 'DENSEREF = '
 WRITE(IU,'(A)') 'DENSESLP = '
 WRITE(IU,'(A)') 'FIRSTDT = '
 WRITE(IU,'(A)') 'INDENSE_P? = '
 WRITE(IU,'(A)') 'DENSE_P?_L? = '

 PMANAGER_SAVERUNWQ_WRTVDF=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTVDF

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTRCT(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTRCT=.FALSE.

 WRITE(IU,'(/A)') '[RCT] # Chemical Reaction package'
 WRITE(IU,'(A)') 'ISOTHM = '//TRIM(ITOS(WQ%RCT%ISOTHM))
 WRITE(IU,'(A)') 'IREACT = '//TRIM(ITOS(WQ%RCT%IREACT))
 WRITE(IU,'(A)') 'IRCTOP = '//TRIM(ITOS(WQ%RCT%IRCTOP))//'# default'
 WRITE(IU,'(A)') 'IGETSC = '//TRIM(ITOS(WQ%RCT%IGETSC))
 WRITE(IU,'(A)') 'RHOB_L? = '
 WRITE(IU,'(A)') 'PRSITY2_L? = '
 WRITE(IU,'(A)') 'SRCONC_T?_L? = '
 WRITE(IU,'(A)') 'SP1_T?_L? = '
 WRITE(IU,'(A)') 'SP2_T?_L? = '
 WRITE(IU,'(A)') 'RC1_T?_L? = '
 WRITE(IU,'(A)') 'RC2_T?_L? = '


 PMANAGER_SAVERUNWQ_WRTRCT=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTRCT

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTUDR(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTUDR=.FALSE.

 WRITE(IU,'(/A)') '[UDF] # User Difined Reaction'

 PMANAGER_SAVERUNWQ_WRTUDR=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTUDR

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTFTL(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTFTL=.FALSE.

 WRITE(IU,'(/A)') '[FTL] # Flow Transport Link'

 PMANAGER_SAVERUNWQ_WRTFTL=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTFTL


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

 WRITE(IU,'(A)') 'IBCFCB = 0  # default'
 WRITE(IU,'(A)') 'HDRY = -9999  # default'
 WRITE(IU,'(A)') 'IWDFLG = 0  # default'
 WRITE(IU,'(A)') 'WETFCT = 1  # default'
 WRITE(IU,'(A)') 'IWETIT = 1  # default'
 WRITE(IU,'(A)') 'IHDWET = 0  # default'
 WRITE(IU,'(A)') 'LTYPE_L? = 0  # default'
 WRITE(IU,'(A)') 'TRPY_L? = 1 # default'
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
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'IBOUND_test_L?',ITOPIC,1,1,ILAY,ILAY,0))RETURN
 ENDDO

 WRITE(IU,'(A)') 'HNOFLO    = -9999.0  # default'

 DO ILAY=1,PRJNLAY
  ITOPIC=TSHD
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'STRT_test_L?',ITOPIC,1,1,ILAY,ILAY,0))RETURN
 ENDDO

 PMANAGER_SAVERUNWQ_WRTBAS6=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTBAS6

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTDIS(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: I,ILAY,ICOL,IROW,LCBD,ITOPIC,KPER

 PMANAGER_SAVERUNWQ_WRTDIS=.FALSE.

 WRITE(IU,'(/A)') '[DIS] # MODFLOW DIScretization Package'

! WRITE(IU,'(A)') 'NLAY = '//TRIM(ITOS(WQFILE%NLAY)) 
 WRITE(IU,'(A)') 'NLAY = '//TRIM(ITOS(PRJNLAY)) 
 WRITE(IU,'(A)') 'NROW = '//TRIM(ITOS(PRJIDF%NROW))
 WRITE(IU,'(A)') 'NCOL = '//TRIM(ITOS(PRJIDF%NCOL))
 WRITE(IU,'(A)') 'NPER = '//TRIM(ITOS(PRJNPER))
 WRITE(IU,'(A)') 'ITMUNI = 4 # default' 
 WRITE(IU,'(A)') 'LENUNI = 2 # default' 

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
 
 !# Write DELR_C and DELR_R
 WRITE(IU,'(A)') TRIM(WQFILE%DELR_C)
 WRITE(IU,'(A)') TRIM(WQFILE%DELR_R)

 DO ILAY=1,PRJNLAY
  !## quasi-3d scheme add top aquifer modellayer
  !IF(LQBD.OR.ILAY.EQ.1)THEN
  ITOPIC=TTOP
  IF(ILAY.EQ.1)THEN
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'TOP',ITOPIC,1,1,ILAY,ILAY,0))RETURN
  ENDIF
  
  ITOPIC=TBOT
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'BOT_L?',ITOPIC,1,1,ILAY,ILAY,0))RETURN
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
 IF(ISS.EQ.1) WRITE(IU,'(A)') 'SSTR_P? = TR'  
 IF(ISS.EQ.0) WRITE(IU,'(A)') 'SSTR_P? = SS'  

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
 WRITE(IU,'(A)') 'WRITEHELP     =   F # default'
 WRITE(IU,'(A)') 'ECHODEFAULTS  =   F # default'
 WRITE(IU,'(A)') 'RESULT_DIR    =   '//CHAR(39)//TRIM(PREFVAL(1))//'\MODELS\'//TRIM(MODELNAME)//CHAR(39)
 WRITE(IU,'(A)') 'IDFDEBUG      =   F # default'
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
 WRITE(IU,'(A)') 'START_HOUR    =   '//TRIM(ITOS(SIM(KPER)%IHR))
 WRITE(IU,'(A)') 'START_MINUTE  =   '//TRIM(ITOS(SIM(KPER)%IMT))
 WRITE(IU,'(A)') 'START_SECOND  =   '//TRIM(ITOS(SIM(KPER)%ISC))

 PMANAGER_SAVERUNWQ_WRTGEN=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTGEN

 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_U2DREL(IU,KEYNAME,ITOPIC,IPER,ISUBT,ISYS,ILAY,ISPEC)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: KEYNAME
 INTEGER,INTENT(IN) :: IU,ITOPIC,IPER,ISUBT,ISYS,ISPEC,ILAY
 CHARACTER(LEN=256) :: FNAME,LINE,LINE2
 INTEGER :: ICNST, JLAY,JSYS,CPER,JSUBT,NSYS
 REAL(KIND=DP_KIND) :: FCT, IMP, CNST
 INTEGER(KIND=8) :: ITIME,JTIME
 
 PMANAGER_SAVERUNWQ_U2DREL=.FALSE.

 !## Skip this topic in case it is not active
 IF(MC(PBMAN%IFORMAT)%IACT(ITOPIC).EQ.0) THEN
    PMANAGER_SAVERUNWQ_U2DREL=.TRUE. ;  RETURN ! 
 ENDIF   
 
 !## Find proper record for current IPER in Files Arry of actual Topic
 CPER=0
 IF(IPER.GT.0) CPER=PMANAGER_GETCURRENTIPER(IPER,ITOPIC,ITIME,JTIME) ; CPER=MAX(1,CPER)
 
 !## Correction in case ISUBT is incorrect. Species are considered additional/extra Subtopics in the object
 JSUBT=ISUBT ; IF(ISPEC.GT.0) JSUBT=TOPICS(ITOPIC)%NSUBTOPICS-NSPECIES

!## loop over systems to find 
 NSYS=SIZE(TOPICS(ITOPIC)%STRESS(CPER)%FILES,2)
 DO JSYS=1,NSYS

     !## substitute variables
     ICNST =TOPICS(ITOPIC)%STRESS(CPER)%FILES(JSUBT+ISPEC,JSYS)%ICNST 
     CNST  =TOPICS(ITOPIC)%STRESS(CPER)%FILES(JSUBT+ISPEC,JSYS)%CNST  
     FCT   =TOPICS(ITOPIC)%STRESS(CPER)%FILES(JSUBT+ISPEC,JSYS)%FCT   
     IMP   =TOPICS(ITOPIC)%STRESS(CPER)%FILES(JSUBT+ISPEC,JSYS)%IMP   
     FNAME =TOPICS(ITOPIC)%STRESS(CPER)%FILES(JSUBT+ISPEC,JSYS)%FNAME 
     JLAY  =TOPICS(ITOPIC)%STRESS(CPER)%FILES(JSUBT+ISPEC,JSYS)%ILAY
 
     IF(ILAY.NE.0.AND.ILAY.EQ.JLAY) EXIT ! found variable in case parameter input is layer based (in stead of system based, like TOP, BOT etc)
     IF(JSYS.NE.0.AND.JSYS.EQ.ISYS) EXIT ! found correct system 
     
 ENDDO     
 
 IF(JSYS.GT.NSYS) THEN
     PMANAGER_SAVERUNWQ_U2DREL=.TRUE. ;  RETURN ! no system found 
 ENDIF    
 
 ! Macro’s can be used for ranges layers (_L), rows (_R), columns (_C), stress periods (_P), species (_T) and sub-systems (_S)
 LINE=UTL_CAP(KEYNAME,'U')
 IF(JLAY.GT.0)  LINE=TRIM(UTL_SUBST(LINE,'_L?','_L'//TRIM(ITOS(JLAY))))
 IF(IPER.GT.0)  LINE=TRIM(UTL_SUBST(LINE,'_P?','_P'//TRIM(ITOS(IPER))))
 IF(ISPEC.GT.0) LINE=TRIM(UTL_SUBST(LINE,'_T?','_T'//TRIM(ITOS(ISPEC))))
 IF(ISYS.GT.0)  LINE=TRIM(UTL_SUBST(LINE,'_S?','_S'//TRIM(ITOS(ISYS))))
 
 IF(ICNST.EQ.1)THEN ; LINE2=TRIM(UTL_REALTOSTRING(CNST)) ; ELSE ; LINE2=TRIM(FNAME) ; ENDIF
 IF(FCT.EQ.1.0.AND.IMP.EQ.0.0)THEN
   WRITE(IU,'(A)') TRIM(LINE)//' = '//TRIM(LINE2)
 ELSE  
   WRITE(IU,'(A)') TRIM(LINE)//' = '//TRIM(UTL_REALTOSTRING(FCT))//' * '// TRIM(LINE2)//' + '//TRIM(UTL_REALTOSTRING(IMP))
 ENDIF
 
         
 PMANAGER_SAVERUNWQ_U2DREL=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_U2DREL
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ICOL, IROW
 REAL(KIND=DP_KIND) :: DELX, DELY
 
 PMANAGER_SAVERUNWQ_INIT=.FALSE.

 !## Create string for DELR_C and DELR_R  
 IF(PRJIDF%IEQ.EQ.0)THEN
   WQFILE%DELR_C='  DELR_C? = '//TRIM(RTOS(PRJIDF%DX,'E',7))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(5)))  
 ELSE
  DO ICOL=1,PRJIDF%NCOL
   DELX=PRJIDF%SX(ICOL)-PRJIDF%SX(ICOL-1)
   WQFILE%DELR_C='  DELR_C'//TRIM(ITOS(ICOL))//' = '//TRIM(RTOS(DELX,'E',7))
  ENDDO
 ENDIF

 IF(PRJIDF%IEQ.EQ.0)THEN
   WQFILE%DELR_R=  '  DELR_R? = '//TRIM(RTOS(PRJIDF%DY,'E',7))//' '//TRIM(UTL_REALTOSTRING(SUBMODEL(5)))
 ELSE
  DO IROW=1,PRJIDF%NROW
   DELY=PRJIDF%SY(IROW-1)-PRJIDF%SY(IROW)
   WQFILE%DELR_R='  DELR_R'//TRIM(ITOS(IROW))//' = '//TRIM(RTOS(DELY,'E',7))
  ENDDO
 ENDIF
 
 PMANAGER_SAVERUNWQ_INIT=.TRUE. 

 END FUNCTION PMANAGER_SAVERUNWQ_INIT

END MODULE MOD_PMANAGER_WQ
