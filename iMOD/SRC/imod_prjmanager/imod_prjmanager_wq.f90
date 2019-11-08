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
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!!
!!  Contact: imod.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands.
!!
MODULE MOD_PMANAGER_WQ

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : RVERSION
USE MOD_PMANAGER_PAR
USE MOD_PMANAGER_UTL
USE MOD_PMANAGER_MF2005, ONLY : PMANAGER_SAVEMF2005_SIM
USE MOD_UTL

INTEGER(KIND=8),PRIVATE :: ITIME,JTIME

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ(FNAME,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=52) :: CDATE1,CDATE2
 CHARACTER(LEN=256) :: BNDFNAME
 INTEGER :: IU,I,J,K,IPER,KPER,N,NSCL,ISS
 LOGICAL :: LDAYS,LEX
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 CHARACTER(LEN=256) :: LINE

 PMANAGER_SAVERUNWQ=.FALSE.

 !!## overrule ipst if not as keyword given
 !IF(IBATCH.EQ.1.AND.PBMAN%IPEST.EQ.0)TOPICS(TPST)%IACT_MODEL=0

 !## get active packages, set default values
 ! Frans: moet deze aangepast voor SEAWAT?
 IF(.NOT.PMANAGER_GETPACKAGES(IBATCH,1))RETURN

 !## Prepare result model file
 CALL UTL_CREATEDIR(FNAME(1:INDEX(FNAME,'\',.TRUE.)-1))
 IF(IBATCH.EQ.0)THEN
  INQUIRE(FILE=FNAME,EXIST=LEX)
  IF(LEX)THEN
   CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to overwrite'//CHAR(13)//TRIM(FNAME),'Question')
   IF(WINFODIALOG(4).NE.1)RETURN
  ENDIF
 ENDIF
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='REPLACE',ACTION='WRITE,DENYREAD',FORM='FORMATTED'); IF(IU.EQ.0)RETURN

 !## time information
 ISS=0; DO KPER=1,PRJNPER; IF(SIM(KPER)%DELT.NE.0.0D0)ISS=1; ENDDO
 !## remove last timestep sinces it is the final date 
 IF(PRJNPER.GT.1)PRJNPER=PRJNPER-1

 WRITE(IU,'(A)') '############################################################################'
 WRITE(IU,'(A)') '# iMOD run-file for SEAWAT '
 WRITE(IU,'(A)') '############################################################################'

 !## seawat selected
 IF(PBMAN%IFORMAT.EQ.4)THEN
  !## write Modflow Packages obligatory
  IF(.NOT.PMANAGER_SAVERUNWQ_WRTBAS6(IU))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_WRTDIS(IU))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_WRTBCF6(IU,ISS))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_WRTLPF(IU,ISS)) RETURN
 
  !## write Modflow Packages optional
  IF(.NOT.PMANAGER_SAVERUNWQ_WRTRCH(IU))RETURN  
  IF(.NOT.PMANAGER_SAVERUNWQ_WRTEVT(IU))RETURN  
  IF(.NOT.PMANAGER_SAVERUNWQ_WRTDRN(IU))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_WRTRIV(IU))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_WRTGHB(IU))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_WRTWEL(IU))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_WRTCHD(IU))RETURN

  IF(.NOT.PMANAGER_SAVERUNWQ_WRTPCG(IU))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_WRTOC(IU)) RETURN
 ELSE
  IF(.NOT.PMANAGER_SAVERUNWQ_WRTFTL(IU))RETURN
 ENDIF
 
 !## write MT3D/Seawat Packages obligatory
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTBTN(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTGCG(IU))RETURN

 !## write MT3D/Seawat Packages optional
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTADV(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTDSP(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTSSM(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTVDF(IU))RETURN

 IF(.NOT.PMANAGER_SAVERUNWQ_WRTRCT(IU))RETURN
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTUDR(IU))RETURN

 !## write Start Packages
 IF(.NOT.PMANAGER_SAVERUNWQ_WRTGEN(IU))RETURN

 CLOSE(IU)

 PMANAGER_SAVERUNWQ=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTBTN(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,I,ISPECIES
 INTEGER :: ICOL,IROW,LCBD,KPER,MCOMP

 PMANAGER_SAVERUNWQ_WRTBTN=.FALSE.

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[BTN] # MT3DMS Basic Transport Package'
 WRITE(IU,'(1X,A)') 'HEADING1 = Written by iMOD '//TRIM(RVERSION)
 WRITE(IU,'(1X,A)') 'HEADING2 = -'

 WRITE(IU,'(1X,A)') 'NLAY = '//TRIM(ITOS(PRJNLAY))
 WRITE(IU,'(1X,A)') 'NROW = '//TRIM(ITOS(PRJIDF%NROW))
 WRITE(IU,'(1X,A)') 'NCOL = '//TRIM(ITOS(PRJIDF%NCOL))
 WRITE(IU,'(1X,A)') 'NPER  = '//TRIM(ITOS(PRJNPER))

 WRITE(IU,'(1X,A)') 'NCOMP = '//TRIM(ITOS(NSPECIES))
 MCOMP=0; DO I=1,NSPECIES ; IF(SPECIES(I)%IMOBILE.EQ.2)MCOMP=MCOMP+1; ENDDO
 WRITE(IU,'(1X,A)') 'MCOMP = '//TRIM(ITOS(MCOMP))
 WRITE(IU,'(1X,A)') 'TUNIT = '//TRIM(WQ%BTN%TUNIT)//' # default'  
 WRITE(IU,'(1X,A)') 'LUNIT = '//TRIM(WQ%BTN%LUNIT)//' # default'  
 WRITE(IU,'(1X,A)') 'MUNIT = '//TRIM(WQ%BTN%MUNIT)//' # default'  
 
 !## define LAYCON_L    
 DO ILAY=1,PRJNLAY; WRITE(IU,'(1X,A)') 'LAYCON_L'//TRIM(ITOS(ILAY))//' = '//TRIM(ITOS(LAYCON(ILAY))); ENDDO

 IF(PRJIDF%IEQ.EQ.0)THEN
  WRITE(IU,'(1X,A)') 'DELR_C? = '//TRIM(RTOS(PRJIDF%DX,'F',3))
  WRITE(IU,'(1X,A)') 'DELC_R? = '//TRIM(RTOS(PRJIDF%DY,'F',3))
 ELSE
  DO ICOL=1,PRJIDF%NCOL; WRITE(IU,'(1X,A)') 'DELR_C'//TRIM(ITOS(ICOL))//' = '//TRIM(RTOS(PRJIDF%SX(ICOL)-PRJIDF%SX(ICOL-1),'F',3)); ENDDO
  DO IROW=1,PRJIDF%NROW; WRITE(IU,'(1X,A)') 'DELC_R'//TRIM(ITOS(IROW))//' = '//TRIM(RTOS(PRJIDF%SY(IROW-1)-PRJIDF%SY(IROW),'F',3)); ENDDO
 ENDIF

 DO ILAY=1,PRJNLAY
  !## quasi-3d scheme add top aquifer modellayer
  IF(ILAY.EQ.1)THEN
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'HTOP',TTOP,0,1,0,ILAY,0))RETURN
  ENDIF
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'DZ_L?',TTHK,0,1,0,ILAY,0))RETURN
 ENDDO

 DO ILAY=1,PRJNLAY
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'PRSITY_L?',TPOR,0,1,0,ILAY,0))RETURN
 ENDDO

 DO ILAY=1,PRJNLAY
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'ICBUND_L?',TCBI,0,1,0,ILAY,0))RETURN
 ENDDO

 DO ISPECIES=1,NSPECIES
  DO ILAY=1,PRJNLAY
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'SCONC_T?_L?',TSCO,1,0,0,ILAY,ISPECIES))RETURN
  ENDDO
 ENDDO 

 WRITE(IU,'(1X,A)') 'CINACT = '//TRIM(UTL_REALTOSTRING(WQ%BTN%CINACT))//' # default'
 WRITE(IU,'(1X,A)') 'THKMIN = '//TRIM(UTL_REALTOSTRING(WQ%BTN%THKMIN))//' # default'
 WRITE(IU,'(1X,A)') 'IFMTCN = '//TRIM(ITOS(WQ%BTN%IFMTCN))//' # default'
 WRITE(IU,'(1X,A)') 'IFMTNP = '//TRIM(ITOS(WQ%BTN%IFMTNP))//' # default'
 WRITE(IU,'(1X,A)') 'IFMTRF = '//TRIM(ITOS(WQ%BTN%IFMTRF))//' # default'
 WRITE(IU,'(1X,A)') 'IFMTDP = '//TRIM(ITOS(WQ%BTN%IFMTDP))//' # default'
 WRITE(IU,'(1X,A)') 'SAVUCN = '//TRIM(LTOS(WQ%BTN%SAVUCN,1))//' # default'
 WRITE(IU,'(1X,A)') 'NPRS =   '//TRIM(ITOS(PBMAN%NPRS))   ! frans, afleiden uit isave of zo uit tab 3, aantal te bewaren tijdstappen
 WRITE(IU,'(1X,A)') 'TIMPRS = '//TRIM(UTL_REALTOSTRING(PBMAN%TIMPRS)) ! regel met alle NPRS tijdstappen schrijven 
 WRITE(IU,'(1X,A)') 'NPROBS = '//TRIM(ITOS(PBMAN%NPROBS))
 DO ILAY=1,PRJNLAY
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'OBS_L?',TOBS,0,1,0,ILAY,0))RETURN
 ENDDO
 WRITE(IU,'(1X,A)') 'CHKMAS = '//TRIM(LTOS(WQ%BTN%CHKMAS,1))//' # default'
 WRITE(IU,'(1X,A)') 'NPRMAS = '//TRIM(ITOS(PBMAN%NPRMAS))
 
 CALL PMANAGER_SAVERUNWQ_WRT_SIMTIME(IU,.TRUE.)

 PMANAGER_SAVERUNWQ_WRTBTN=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTBTN

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTADV(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY

 PMANAGER_SAVERUNWQ_WRTADV=.FALSE.

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[ADV] # MT3DMS ADVection package'
 WQ%ADV%MIXELM=WQ%ADV%MIXELM-1 ; IF(WQ%ADV%MIXELM.EQ.4) WQ%ADV%MIXELM=-1
 WRITE(IU,'(1X,A)') 'MIXELM =  '//TRIM(ITOS(WQ%ADV%MIXELM))
 WRITE(IU,'(1X,A)') 'PERCEL =  '//TRIM(UTL_REALTOSTRING(WQ%ADV%PERCEL))
 WRITE(IU,'(1X,A)') '#MXPART = '//TRIM(ITOS(WQ%ADV%MXPART))
 WRITE(IU,'(1X,A)') 'NADVFD =  '//TRIM(ITOS(WQ%ADV%NADVFD))  
 WRITE(IU,'(1X,A)') '#ITRACK = '
 WRITE(IU,'(1X,A)') '#WD = '
 WRITE(IU,'(1X,A)') '#DCEPS = '
 WRITE(IU,'(1X,A)') '#NPLANE = '
 WRITE(IU,'(1X,A)') '#NPL = '
 WRITE(IU,'(1X,A)') '#NPH = '
 WRITE(IU,'(1X,A)') '#NPMIN = '
 WRITE(IU,'(1X,A)') '#NPMAX = '
 WRITE(IU,'(1X,A)') '#INTERP = '
 WRITE(IU,'(1X,A)') '#NLSINK = '
 WRITE(IU,'(1X,A)') '#NPSINK = '
 WRITE(IU,'(1X,A)') '#DCHMOC = '
 
 PMANAGER_SAVERUNWQ_WRTADV=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTADV

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTDSP(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY

 PMANAGER_SAVERUNWQ_WRTDSP=.FALSE.

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[DSP] #MT3DMS Dispersion Package'
 DO ILAY=1,PRJNLAY
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'TRPT_L?'  ,TDSP,0,1,0,ILAY,0))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'TRPT_L?'  ,TDSP,0,2,0,ILAY,0))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'TRPV_L?'  ,TDSP,0,3,0,ILAY,0))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'DMCOEF_L?',TDSP,0,4,0,ILAY,0))RETURN
 ENDDO
 
 PMANAGER_SAVERUNWQ_WRTDSP=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTDSP

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTGCG(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY

 PMANAGER_SAVERUNWQ_WRTGCG=.FALSE.

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[GCG] # MT3DMS Generalized Conjugate Gradient Solver Package'
 WRITE(IU,'(1X,A)') 'MXITER = '//TRIM(ITOS(WQ%GCG%MXITER))
 WRITE(IU,'(1X,A)') 'ITER1  = '//TRIM(ITOS(WQ%GCG%ITER1))
 WRITE(IU,'(1X,A)') 'ISOLVE = '//TRIM(ITOS(WQ%GCG%ISOLVE))
 WRITE(IU,'(1X,A)') 'NCRS   = '//TRIM(ITOS(WQ%GCG%NCRS))
 WRITE(IU,'(1X,A)') 'IPRGCG = '//TRIM(ITOS(WQ%GCG%IPRGCG))
 WRITE(IU,'(1X,A)') 'ACCL   = '//TRIM(RTOS(WQ%GCG%ACCL,'G',7))
 WRITE(IU,'(1X,A)') 'CCLOSE = '//TRIM(RTOS(WQ%GCG%CCLOSE,'G',7))
 
 PMANAGER_SAVERUNWQ_WRTGCG=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTGCG

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTSSM(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ISPECIES,KPER

 PMANAGER_SAVERUNWQ_WRTSSM=.FALSE.

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[SSM] # MT3DMS Sink Source Mixing Package'
 WRITE(IU,'(1X,A)') 'MXSS = '//TRIM(ITOS(PBMAN%MXSS))

 DO KPER=1,PRJNPER ; DO ISPECIES=1,NSPECIES
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CRCH_T?_P?',TRCH,KPER,0,0,ILAY,ISPECIES))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CEVT_T?_P?',TEVT,KPER,0,0,ILAY,ISPECIES))RETURN
  DO ILAY=1,PRJNLAY
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CCHD_T?_P?_L?',TCHD,KPER,0,0,ILAY,ISPECIES))RETURN
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CWEL_T?_P?_L?',TWEL,KPER,0,0,ILAY,ISPECIES))RETURN
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CDRN_T?_P?_L?',TDRN,KPER,0,0,ILAY,ISPECIES))RETURN
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CRIV_T?_P?_L?',TRIV,KPER,0,0,ILAY,ISPECIES))RETURN
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CGHB_T?_P?_L?',TGHB,KPER,0,0,ILAY,ISPECIES))RETURN
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'CTVC_T?_P?_L?',TTVC,KPER,0,0,ILAY,ISPECIES))RETURN
  ENDDO    
 ENDDO ; ENDDO

 WRITE(IU,'(1X,A)') '#CMAL_T?_P?_L? = 0  # default'

 PMANAGER_SAVERUNWQ_WRTSSM=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTSSM

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTVDF(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY

 PMANAGER_SAVERUNWQ_WRTVDF=.FALSE.

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[VDF] # Variable-Density Flow '
 WRITE(IU,'(1X,A)') 'MTDNCONC = '//TRIM(ITOS(WQ%VDF%MTDNCONC))//' # default'
 WRITE(IU,'(1X,A)') 'MFNADVFD = '//TRIM(ITOS(WQ%VDF%MFNADVFD))//' # default'
 WRITE(IU,'(1X,A)') 'NSWTCPL =  '//TRIM(ITOS(WQ%VDF%NSWTCPL))//' # default'
 WRITE(IU,'(1X,A)') 'IWTABLE =  '//TRIM(ITOS(WQ%VDF%IWTABLE))//' # default'
 WRITE(IU,'(1X,A)') 'DENSEMIN = '//TRIM(UTL_REALTOSTRING(WQ%VDF%DENSEMIN))
 WRITE(IU,'(1X,A)') 'DENSEMAX = '//TRIM(UTL_REALTOSTRING(WQ%VDF%DENSEMAX))
 WRITE(IU,'(1X,A)') '#DNSCRIT = only for NSWTCPL <> 1. Not supported'
 WRITE(IU,'(1X,A)') 'DENSEREF = '//TRIM(UTL_REALTOSTRING(WQ%VDF%DENSEREF))
 WRITE(IU,'(1X,A)') 'DENSESLP = '//TRIM(UTL_REALTOSTRING(WQ%VDF%DENSESLP))
 WRITE(IU,'(1X,A)') 'FIRSTDT =  '//TRIM(UTL_REALTOSTRING(WQ%VDF%FIRSTDT))//' # default'
 WRITE(IU,'(1X,A)') '#INDENSE_P? =  only for MTDNCONC = 1. Not supported'
 WRITE(IU,'(1X,A)') '#DENSE_P?_L? = only for MTDNCONC = 1. Not supported'

 PMANAGER_SAVERUNWQ_WRTVDF=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTVDF

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTRCT(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ISPECIES

 PMANAGER_SAVERUNWQ_WRTRCT=.FALSE.

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[RCT] # Chemical Reaction package'
 WRITE(IU,'(1X,A)') 'ISOTHM = '//TRIM(ITOS(WQ%RCT%ISOTHM))
 WRITE(IU,'(1X,A)') 'IREACT = '//TRIM(ITOS(WQ%RCT%IREACT))
 WRITE(IU,'(1X,A)') 'IRCTOP = '//TRIM(ITOS(WQ%RCT%IRCTOP))//'# default'
 WRITE(IU,'(1X,A)') 'IGETSC = '//TRIM(ITOS(WQ%RCT%IGETSC))

 DO ILAY=1,PRJNLAY
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'RHOB_L?'   ,THOB,0,0,0,ILAY,0))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'PRSITY2_L?',TPID,0,0,0,ILAY,0))RETURN
 ENDDO    

 DO ISPECIES=1,NSPECIES; DO ILAY=1,PRJNLAY
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'SRCONC_T?_L?',TICS,0,0,0,ILAY,ISPECIES))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'SP1_T?_L?'   ,TFSC,0,0,0,ILAY,ISPECIES))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'SP2_T?_L?'   ,TSSC,0,0,0,ILAY,ISPECIES))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'RC1_T?_L?'   ,TFOD,0,0,0,ILAY,ISPECIES))RETURN
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'RC2_T?_L?'   ,TFOS,0,0,0,ILAY,ISPECIES))RETURN
 ENDDO; ENDDO

 PMANAGER_SAVERUNWQ_WRTRCT=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTRCT

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTUDR(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY

 PMANAGER_SAVERUNWQ_WRTUDR=.FALSE.

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[UDF] # User Defined Reaction'

 PMANAGER_SAVERUNWQ_WRTUDR=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTUDR

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTFTL(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY

 PMANAGER_SAVERUNWQ_WRTFTL=.FALSE.

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[FTL] # Flow Transport Link'
 DO ILAY=1,PRJNLAY
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'IBOUND_L?',TBND,0,1,0,ILAY,0))RETURN
 ENDDO
 
 WRITE(IU,'(1X,A)') 'FLOW_RESULT_DIR = '//TRIM(PBMAN%FLOW_RESULT_DIR)
 WRITE(IU,'(1X,A)') 'FLOWTYPE = '
 WRITE(IU,'(1X,A)') 'TYPELABEL = steady-state # default'
 WRITE(IU,'(1X,A)') 'BDG = '
 WRITE(IU,'(1X,A)') 'FTLSOURCE = 1 # default'

 PMANAGER_SAVERUNWQ_WRTFTL=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTFTL

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTPCG(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY

 PMANAGER_SAVERUNWQ_WRTPCG=.FALSE.

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[PCG] # MODFLOW Preconditioned Conjugate-Gradient Package'
 
 WRITE(IU,'(1X,A)') 'MXITER = '//TRIM(ITOS(PCG%NOUTER))
 WRITE(IU,'(1X,A)') 'ITER1 =  '//TRIM(ITOS(PCG%NINNER))
 WRITE(IU,'(1X,A)') 'NPCOND = '//TRIM(ITOS(PCG%NPCOND))
 WRITE(IU,'(1X,A)') 'NBPOL = 2 '
 WRITE(IU,'(1X,A)') 'HCLOSE = '//TRIM(RTOS(PCG%HCLOSE,'G',7))
 WRITE(IU,'(1X,A)') 'RCLOSE = '//TRIM(RTOS(PCG%RCLOSE,'G',7))
 WRITE(IU,'(1X,A)') 'RELAX =  '//TRIM(RTOS(PCG%RELAX,'G',7))
 WRITE(IU,'(1X,A)') 'IPRPCG = '//TRIM(ITOS(PCG%IPRPCG))
 WRITE(IU,'(1X,A)') 'MUTPCG = '//TRIM(ITOS(PCG%MUTPCG))
 WRITE(IU,'(1X,A)') 'DAMP =  '//TRIM(RTOS(PCG%DAMPPCG,'G',7))
 
 PMANAGER_SAVERUNWQ_WRTPCG=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTPCG

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTCHD(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY

 PMANAGER_SAVERUNWQ_WRTCHD=.FALSE.
 
 !## skip this optional package if not selected
 IF(TOPICS(TCHD)%IACT_MODEL.NE.1)THEN; PMANAGER_SAVERUNWQ_WRTCHD=.TRUE. ; RETURN ; ENDIF

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[CHD]'

 !## write arrays
 CALL PMANAGER_SAVERUNWQ_WRTIDF(IU,TCHD,(/'SHEAD','EHEAD'/))

 PMANAGER_SAVERUNWQ_WRTCHD=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTCHD

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTWEL(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY

 PMANAGER_SAVERUNWQ_WRTWEL=.FALSE.
 
 !## skip this optional package if not selected
 IF(TOPICS(TWEL)%IACT_MODEL.NE.1)THEN; PMANAGER_SAVERUNWQ_WRTWEL=.TRUE. ; RETURN ; ENDIF

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[WEL]'

 WRITE(IU,'(1X,A)') 'MXACTW = 1000' 
 WRITE(IU,'(1X,A)') 'IWELCB = 0'
 WRITE(IU,'(1X,A)') 'OPTION = AUX NOPRINT'
 WRITE(IU,'(1X,A)') 'NODATA_P$ = -999'

 !## write arrays
 CALL PMANAGER_SAVERUNWQ_WRTIDF(IU,TWEL,(/'WEL'/))

 PMANAGER_SAVERUNWQ_WRTWEL=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTWEL

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTGHB(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY

 PMANAGER_SAVERUNWQ_WRTGHB=.FALSE.
 
 !## skip this optional package if not selected
 IF(TOPICS(TGHB)%IACT_MODEL.NE.1)THEN; PMANAGER_SAVERUNWQ_WRTGHB=.TRUE. ; RETURN ; ENDIF

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[GHB]'

 WRITE(IU,'(1X,A)') 'MXACTB = 1000' 
 WRITE(IU,'(1X,A)') 'IGHBCB = 0'
 WRITE(IU,'(1X,A)') 'OPTION = AUX GHBSSMDENSE NOPRINT'
 WRITE(IU,'(1X,A)') 'MAXGHBSYS = 1' !## nog in te vullen !!!

 !## write arrays
 CALL PMANAGER_SAVERUNWQ_WRTIDF(IU,TGHB,(/'BHEAD','COND','GHBSSMDENS'/))

 PMANAGER_SAVERUNWQ_WRTGHB=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTGHB

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTRIV(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY

 PMANAGER_SAVERUNWQ_WRTRIV=.FALSE.
 
 !## skip this optional package if not selected
 IF(TOPICS(TRIV)%IACT_MODEL.NE.1)THEN; PMANAGER_SAVERUNWQ_WRTRIV=.TRUE. ; RETURN ; ENDIF

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[RIV]'

 WRITE(IU,'(1X,A)') 'MXACTR = 1000' 
 WRITE(IU,'(1X,A)') 'IRIVCB = 0'
 WRITE(IU,'(1X,A)') 'OPTION = AUX RIVSSMDENSE NOPRINT'
 WRITE(IU,'(1X,A)') 'MAXRIVSYS = 1' !## nog in te vullen !!!
 
 !## write arrays
 CALL PMANAGER_SAVERUNWQ_WRTIDF(IU,TRIV,(/'STAGE','COND','RBOT','RIVSSMDENS'/))

 PMANAGER_SAVERUNWQ_WRTRIV=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTRIV

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTDRN(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: IPER,KPER,TPER,IP1,IP2
 
 PMANAGER_SAVERUNWQ_WRTDRN=.FALSE.
 
 !## skip this optional package if not selected
 IF(TOPICS(TDRN)%IACT_MODEL.NE.1)THEN; PMANAGER_SAVERUNWQ_WRTDRN=.TRUE. ; RETURN ; ENDIF

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[DRN]'

 WRITE(IU,'(1X,A)') 'MXACTD = 1000'
 WRITE(IU,'(1X,A)') 'IDRNCB = 0'
 WRITE(IU,'(1X,A)') 'MDRNSYS = 1' !## nog in te vullen !!!
! WRITE(IU,'(1X,A)') 'OPTION'

 !## write arrays
 CALL PMANAGER_SAVERUNWQ_WRTIDF(IU,TDRN,(/'ELEVATION','COND'/))

 PMANAGER_SAVERUNWQ_WRTDRN=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTDRN


 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTRCH(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY

 PMANAGER_SAVERUNWQ_WRTRCH=.FALSE.

 !## skip this optional package
 IF(TOPICS(TRCH)%IACT_MODEL.NE.1)THEN; PMANAGER_SAVERUNWQ_WRTRCH=.TRUE.; RETURN; ENDIF
 
 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[RCH]'

 WRITE(IU,'(1X,A)') 'NRCHOP = 3'
 WRITE(IU,'(1X,A)') 'IRCHCB = 0'
! WRITE(IU,'(1X,A)') 'INRECH_P$ = 0'
! WRITE(IU,'(1X,A)') 'INIRCH_P$ = 0'
! WRITE(IU,'(1X,A)') 'iRCH_P = '/

 CALL PMANAGER_SAVERUNWQ_WRTIDF(IU,TRCH,(/'RECH'/))
 
 PMANAGER_SAVERUNWQ_WRTRCH=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTRCH

  !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTEVT(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY

 PMANAGER_SAVERUNWQ_WRTEVT=.FALSE.

 !## skip this optional package if not selected
 IF(TOPICS(TEVT)%IACT_MODEL.NE.1)THEN; PMANAGER_SAVERUNWQ_WRTEVT=.TRUE. ; RETURN ; ENDIF
 
 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[TEVT]'

 WRITE(IU,'(1X,A)') 'NEVTOP = 1'
 WRITE(IU,'(1X,A)') 'IEVTCB = 0'
 WRITE(IU,'(1X,A)') 'INSURF_P$ = 1'
 WRITE(IU,'(1X,A)') 'INEVTR_P$ = 1'
 WRITE(IU,'(1X,A)') 'INEXPD_P$ = 1'
 WRITE(IU,'(1X,A)') 'INIEVT_P$ = 0'

 CALL PMANAGER_SAVERUNWQ_WRTIDF(IU,TEVT,(/'SURF','EVTR','EXDP'/))
 
 PMANAGER_SAVERUNWQ_WRTEVT=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTEVT
 
 !####====================================================================
 SUBROUTINE PMANAGER_SAVERUNWQ_WRTIDF(IU,ITOPIC,ATT)
 !####====================================================================
 INTEGER,INTENT(IN) :: ITOPIC,IU
 CHARACTER(LEN=*),DIMENSION(:) :: ATT
 INTEGER :: NSYS,NATT,ICNST,ILAY,IP1,IP2,KPER,TPER
 CHARACTER(LEN=256) :: FNAME,LINE
 CHARACTER(LEN=24) :: TXT 
 REAL(KIND=DP_KIND) :: IMP,FCT,CNST
 
 IP1=1; TPER=1; DO IPER=1,PRJNPER
  !## get appropriate stress-period to store in runfile   
  KPER=PMANAGER_GETCURRENTIPER(IPER,ITOPIC,ITIME,JTIME); IF(KPER.LE.0)CYCLE
  IP2=IPER-1

  NATT=SIZE(TOPICS(ITOPIC)%STRESS(KPER)%FILES,1) !-NSPECIES
  NSYS=SIZE(TOPICS(ITOPIC)%STRESS(KPER)%FILES,2)
  DO ISYS=1,NSYS
   DO IATT=1,NATT
    FCT  =TOPICS(ITOPIC)%STRESS(KPER)%FILES(IATT,ISYS)%FCT
    IMP  =TOPICS(ITOPIC)%STRESS(KPER)%FILES(IATT,ISYS)%IMP
    ILAY =TOPICS(ITOPIC)%STRESS(KPER)%FILES(IATT,ISYS)%ILAY
    ICNST=TOPICS(ITOPIC)%STRESS(KPER)%FILES(IATT,ISYS)%ICNST
    FNAME=TOPICS(ITOPIC)%STRESS(KPER)%FILES(IATT,ISYS)%FNAME

    TXT='_P'//TRIM(ITOS(IP1))//':'//TRIM(ITOS(IP2))//'_S'//TRIM(ITOS(ISYS))//'_L'//TRIM(ITOS(ILAY))   
   
    IF(ICNST.EQ.1)THEN; LINE=TRIM(UTL_REALTOSTRING(CNST)); ELSE; LINE=TRIM(FNAME) ; ENDIF  
    IF(FCT.EQ.1.0D0.AND.IMP.EQ.0.0D0)THEN
     WRITE(IU,'(1X,A)') TRIM(ATT(IATT))//TRIM(TXT)//' = '//TRIM(LINE)
    ELSE  
     WRITE(IU,'(1X,A)') TRIM(ATT(IATT))//TRIM(TXT)//' = '//TRIM(UTL_REALTOSTRING(FCT))//' * '// TRIM(LINE)//' + '//TRIM(UTL_REALTOSTRING(IMP))
    ENDIF
   ENDDO
  ENDDO
 
  TPER=KPER; IP1 =IP2

 ENDDO
 
 END SUBROUTINE PMANAGER_SAVERUNWQ_WRTIDF
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTLPF(IU,ISS)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,ISS
 INTEGER :: ILAY,IWETIT

 PMANAGER_SAVERUNWQ_WRTLPF=.FALSE.

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[LPF]'

 WRITE(IU,'(1X,A)') 'ILPFCB = 0  # default'
 WRITE(IU,'(1X,A)') 'HDRY = -9999  # default'
 WRITE(IU,'(1X,A)') 'NPLPF = 0  # default'
 DO ILAY=1,PRJNLAY; WRITE(IU,'(1X,A)') 'LAYTYPE_L'//TRIM(ITOS(ILAY))//' = '//TRIM(ITOS(LAYCON(ILAY))); ENDDO
 WRITE(IU,'(1X,A)') 'LAYAVG_L? = 0  # default'
 WRITE(IU,'(1X,A)') 'CHANI_L? = 1.0  # default'
 WRITE(IU,'(1X,A)') 'LAYVKA_L? = 1  # default'
 WRITE(IU,'(1X,A)') 'TRPY_L? = 1 # default'
 
 !## laywet code - if unconfined always use wetdry
 IWETIT=0; DO ILAY=1,PRJNLAY; IF(LAYCON(ILAY).EQ.2)IWETIT=1; ENDDO
 DO ILAY=1,PRJNLAY
  IF(LAYCON(ILAY).NE.2)WRITE(IU,'(1X,A)') 'LAYWET_L'//TRIM(ITOS(ILAY))//' = 0' 
  IF(LAYCON(ILAY).EQ.2)WRITE(IU,'(1X,A)') 'LAYWET_L'//TRIM(ITOS(ILAY))//' = 1' 
 ENDDO
 IF(IWETIT.EQ.1)THEN
  WRITE(IU,'(1X,A)') 'WETFCT = 0.1 # default'
  WRITE(IU,'(1X,A)') 'IWETIT = 1  # default'
  WRITE(IU,'(1X,A)') 'IHDWET = 0  # default'
  !## Threshold for wetdry always -0.1
  WRITE(IU,'(A)') 'WETDRY_L? = -0.1 '
 ENDIF
 WRITE(IU,'(1X,A)') 'HANI_L? = 1.0  # default'
 
 DO ILAY=1,PRJNLAY
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'HK_L?',TKHV,1,1,ILAY,ILAY,0))RETURN
 ENDDO 
 DO ILAY=1,PRJNLAY
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'VKA_L?',TKVA,1,1,ILAY,ILAY,0))RETURN
 ENDDO 
 DO ILAY=1,PRJNLAY-1
  !## vcw is in days --- vcont is on 1/day ...??
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'VKCB_L?',TVCW,1,1,ILAY,ILAY,0))RETURN
 ENDDO 
 !## transient - storage coefficient
 IF(ISS.EQ.1)THEN
  DO ILAY=1,PRJNLAY
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'SS_L?',TSTO,1,1,ILAY,ILAY,0))RETURN
  ENDDO 
  !## transient - storage coefficient
  DO ILAY=1,PRJNLAY
   IF(LAYCON(ILAY).NE.1)THEN
    IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'SY_L?',TSPY,1,1,ILAY,ILAY,0))RETURN
   ENDIF
  ENDDO 
 ENDIF

 PMANAGER_SAVERUNWQ_WRTLPF=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTLPF
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTOC(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY

 PMANAGER_SAVERUNWQ_WRTOC=.FALSE.

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[OC] # Output Control option'
 WRITE(IU,'(1X,A)') 'SAVEHEAD_P?_L? = '
 WRITE(IU,'(1X,A)') 'SAVEBUDGET_P?_L? = '
 WRITE(IU,'(1X,A)') 'SAVECONCLAYER_L? = '
 WRITE(IU,'(1X,A)') 'SAVEHEADTEC_P?_L? = '
 WRITE(IU,'(1X,A)') 'SAVECONCTEC_P?_L? = F # default'
 WRITE(IU,'(1X,A)') 'SAVEVXTEC_P?_L? =  F # default'
 WRITE(IU,'(1X,A)') 'SAVEVYTEC_P?_L? =  F # default'
 WRITE(IU,'(1X,A)') 'SAVEVZTEC_P?_L? =  F # default'
 WRITE(IU,'(1X,A)') 'TECFILE =  concvelo.tec # default'
 WRITE(IU,'(1X,A)') 'TECITMUN =  Y # default'
 WRITE(IU,'(1X,A)') 'SAVEHEADOUT_P?_L? =  F # default'
 WRITE(IU,'(1X,A)') 'SAVECONCOUT_P?_L? =  F # default'
 WRITE(IU,'(1X,A)') 'OUTFILE = concvelo.out # default'
 WRITE(IU,'(1X,A)') '#HEADCOLID = '
 WRITE(IU,'(1X,A)') '#CONCCOLID = '
 WRITE(IU,'(1X,A)') '#HEADCOLVAL = '
 WRITE(IU,'(1X,A)') '#CONCCOLVAL = '
 WRITE(IU,'(1X,A)') 'SAVEHEADVTK_P? =  F # default'
 WRITE(IU,'(1X,A)') 'SAVECONCVTK_P? =  F # default'
 WRITE(IU,'(1X,A)') 'SAVEVELOVTK_P? =  F # default'
 WRITE(IU,'(1X,A)') 'PVDFILE =  results.pvd # default'

 PMANAGER_SAVERUNWQ_WRTOC=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTOC

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTBCF6(IU,ISS)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,ISS
 INTEGER :: ILAY,IWETIT

 PMANAGER_SAVERUNWQ_WRTBCF6=.FALSE.

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[BCF6] # BLOCK CENTRED FLOW'

 WRITE(IU,'(1X,A)') 'IBCFCB = 0  # default'
 WRITE(IU,'(1X,A)') 'HDRY = -9999  # default'

 !## laywet code - if unconfined always use wetdry
 IWETIT=0; DO ILAY=1,PRJNLAY; IF(LAYCON(ILAY).EQ.2)IWETIT=1; ENDDO
 WRITE(IU,'(1X,A)') 'IWDFLG = '//TRIM(ITOS(IWETIT))
 IF(IWETIT.EQ.1)THEN
  WRITE(IU,'(1X,A)') 'WETFCT = 0.1 # default'
  WRITE(IU,'(1X,A)') 'IWETIT = 1  # default'
  WRITE(IU,'(1X,A)') 'IHDWET = 0  # default'
  !## Threshold for wetdry always -0.1
  WRITE(IU,'(A)') 'WETDRY_L? = -0.1 '
 ENDIF
 
 DO ILAY=1,PRJNLAY; WRITE(IU,'(1X,A)') 'LTYPE_L'//TRIM(ITOS(ILAY))//' = '//TRIM(ITOS(LAYCON(ILAY))); ENDDO
 WRITE(IU,'(1X,A)') 'TRPY_L? = 1 # default'
 
 IF(LBCF)THEN
  DO ILAY=1,PRJNLAY
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'TRAN_L?',TKDW,1,1,ILAY,ILAY,0))RETURN
  ENDDO 
 ELSE
  DO ILAY=1,PRJNLAY
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'HY_L?',TKHV,1,1,ILAY,ILAY,0))RETURN
  ENDDO 
 ENDIF
 DO ILAY=1,PRJNLAY-1
  !## vcw is in days --- vcont is on 1/day ...??
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'VCONT_L?',TVCW,1,1,ILAY,ILAY,0))RETURN
 ENDDO 
 !## transient - storage coefficient
 IF(ISS.EQ.1)THEN
  DO ILAY=1,PRJNLAY
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'SF1_L?',TSTO,1,1,ILAY,ILAY,0))RETURN
  ENDDO 
  !## transient - storage coefficient
  DO ILAY=1,PRJNLAY
   IF(LAYCON(ILAY).NE.1)THEN
    IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'SF2_L?',TSPY,1,1,ILAY,ILAY,0))RETURN
   ENDIF
  ENDDO 
 ENDIF
 
 PMANAGER_SAVERUNWQ_WRTBCF6=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTBCF6

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTBAS6(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: ILAY,ITOPIC

 PMANAGER_SAVERUNWQ_WRTBAS6=.FALSE.

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[BAS6] # MODFLOW BASic Package'

 DO ILAY=1,PRJNLAY
  ITOPIC=TBND; IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'IBOUND_test_L?',ITOPIC,1,1,ILAY,ILAY,0))RETURN
 ENDDO

 WRITE(IU,'(A)') 'HNOFLO    = -9999.0  # default'

 DO ILAY=1,PRJNLAY
  ITOPIC=TSHD; IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'STRT_test_L?',ITOPIC,1,1,ILAY,ILAY,0))RETURN
 ENDDO

 PMANAGER_SAVERUNWQ_WRTBAS6=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTBAS6

 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTDIS(IU)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: I,ILAY,ICOL,IROW,LCBD,KPER

 PMANAGER_SAVERUNWQ_WRTDIS=.FALSE.

 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[DIS] # MODFLOW DIScretization Package'

 WRITE(IU,'(A)') 'NLAY = '//TRIM(ITOS(PRJNLAY)) 
 WRITE(IU,'(A)') 'NROW = '//TRIM(ITOS(PRJIDF%NROW))
 WRITE(IU,'(A)') 'NCOL = '//TRIM(ITOS(PRJIDF%NCOL))
 WRITE(IU,'(A)') 'NPER = '//TRIM(ITOS(PRJNPER))
 WRITE(IU,'(A)') 'ITMUNI = 4 # default' 
 WRITE(IU,'(A)') 'LENUNI = 2 # default' 
 WRITE(IU,'(A)') 'LAYCBD_L? = 0 # only 3d supported' 
 
 IF(PRJIDF%IEQ.EQ.0)THEN
  WRITE(IU,'(A)') 'DELR_C? = '//TRIM(RTOS(PRJIDF%DX,'F',3))
  WRITE(IU,'(A)') 'DELC_R? = '//TRIM(RTOS(PRJIDF%DY,'F',3))
 ELSE
  DO ICOL=1,PRJIDF%NCOL; WRITE(IU,'(A)') 'DELR_C'//TRIM(ITOS(ICOL))//' = '//TRIM(RTOS(PRJIDF%SX(ICOL)-PRJIDF%SX(ICOL-1),'F',3)); ENDDO
  DO IROW=1,PRJIDF%NROW; WRITE(IU,'(A)') 'DELC_R'//TRIM(ITOS(IROW))//' = '//TRIM(RTOS(PRJIDF%SY(IROW-1)-PRJIDF%SY(IROW),'F',3)); ENDDO
 ENDIF

 DO ILAY=1,PRJNLAY
  IF(ILAY.EQ.1)THEN
   IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'TOP',TTOP,1,1,ILAY,ILAY,0))RETURN
  ENDIF
  IF(.NOT.PMANAGER_SAVERUNWQ_U2DREL(IU,'BOTM_L?',TBOT,1,1,ILAY,ILAY,0))RETURN
 ENDDO

 CALL PMANAGER_SAVERUNWQ_WRT_SIMTIME(IU,.FALSE.)
 
 PMANAGER_SAVERUNWQ_WRTDIS=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTDIS

 !####====================================================================
 SUBROUTINE PMANAGER_SAVERUNWQ_WRT_SIMTIME(IU,LEX)
 !####====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 LOGICAL,INTENT(IN) :: LEX
 INTEGER :: KPER,IT1,IT2,I1,I2
 REAL(KIND=DP_KIND) :: DT1,DT2

 I1=1; I2=I1; DO KPER=1,PRJNPER
  I2=I2+1; DT1=SIM(I1)%DELT; IF(DT1.EQ.0.0D0)DT1=1.0D0; DT2=SIM(I2)%DELT; IF(DT2.EQ.0.0D0)DT2=1.0D0
  IF(DT2.NE.DT1)THEN
   WRITE(IU,'(A)') 'PERLEN_P'//TRIM(ITOS(I1))//':'//TRIM(ITOS(I2-1))//' = '//TRIM(RTOS(SIM(I1)%DELT,'G',7))
   I1=I2
  ENDIF
 ENDDO

 I1=1; I2=I1; DO KPER=1,PRJNPER
  I2=I2+1; IT1=SIM(I1)%NSTP; IT2=SIM(I2)%NSTP
  IF(IT2.NE.IT1)THEN
   WRITE(IU,'(A)') 'NSTP_P'//TRIM(ITOS(I1))//':'//TRIM(ITOS(I2-1))//' = '//TRIM(ITOS(SIM(I1)%NSTP))
   I1=I2
  ENDIF
 ENDDO

 I1=1; I2=I1; DO KPER=1,PRJNPER
  I2=I2+1; DT1=SIM(I1)%TMULT; DT2=SIM(I2)%TMULT
  IF(DT2.NE.DT1)THEN
   WRITE(IU,'(A)') 'TSMULT_P'//TRIM(ITOS(I1))//':'//TRIM(ITOS(I2-1))//' = '//TRIM(RTOS(SIM(I1)%TMULT,'G',7))
   I1=I2
  ENDIF
 ENDDO

 I1=1; I2=I1; DO KPER=1,PRJNPER
  I2=I2+1; DT1=SIM(I1)%DELT; DT2=SIM(I2)%DELT
  IF(DT2.NE.DT1)THEN
   IF(DT1.EQ.0.0D0)WRITE(IU,'(A)') 'SSTR_P'//TRIM(ITOS(I1))//':'//TRIM(ITOS(I2-1))//' = TR'
   IF(DT1.GT.0.0D0)WRITE(IU,'(A)') 'SSTR_P'//TRIM(ITOS(I1))//':'//TRIM(ITOS(I2-1))//' = SS'
   I1=I2
  ENDIF
 ENDDO
 
 IF(LEX)THEN
 
  I1=1; I2=I1; DO KPER=1,PRJNPER
   I2=I2+1; IT1=SIM(I1)%DT0; IT2=SIM(I2)%DT0
   IF(IT2.NE.IT1)THEN
    WRITE(IU,'(A)') 'DT0_P'//TRIM(ITOS(I1))//':'//TRIM(ITOS(I2-1))//' = '//TRIM(RTOS(SIM(I1)%DT0,'G',7))
    I1=I2
   ENDIF
  ENDDO
  I1=1; I2=I1; DO KPER=1,PRJNPER
   I2=I2+1; IT1=SIM(I1)%TTSMULT; IT2=SIM(I2)%TTSMULT
   IF(IT2.NE.IT1)THEN
    WRITE(IU,'(A)') 'TTSMULT_P'//TRIM(ITOS(I1))//':'//TRIM(ITOS(I2-1))//' = '//TRIM(RTOS(SIM(I1)%TTSMULT,'G',7))
    I1=I2
   ENDIF
  ENDDO
  I1=1; I2=I1; DO KPER=1,PRJNPER
   I2=I2+1; IT1=SIM(I1)%TTSMAX; IT2=SIM(I2)%TTSMAX
   IF(IT2.NE.IT1)THEN
    WRITE(IU,'(A)') 'TTSMAX_P'//TRIM(ITOS(I1))//':'//TRIM(ITOS(I2-1))//' = '//TRIM(RTOS(SIM(I1)%TTSMAX,'G',7))
    I1=I2
   ENDIF
  ENDDO
  I1=1; I2=I1; DO KPER=1,PRJNPER
   I2=I2+1; IT1=SIM(I1)%MXSTRN; IT2=SIM(I2)%MXSTRN
   IF(IT2.NE.IT1)THEN
    WRITE(IU,'(A)') 'MXSTRN_P'//TRIM(ITOS(I1))//':'//TRIM(ITOS(I2-1))//' = '//TRIM(ITOS(SIM(I1)%MXSTRN))
    I1=I2
   ENDIF
  ENDDO
 
 ENDIF
 
 END SUBROUTINE PMANAGER_SAVERUNWQ_WRT_SIMTIME
 
 !####====================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_WRTGEN(IU)
 !####====================================================================
 IMPLICIT NONE
 !CHARACTER(LEN=*),INTENT(IN) :: DIR,DIRMNAME
 INTEGER,INTENT(IN) :: IU
 CHARACTER(LEN=256) :: LINE
 INTEGER :: KPER,ITOPIC

 PMANAGER_SAVERUNWQ_WRTGEN=.FALSE.
 WRITE(IU,'(/A)') '#-------------------------------------------'
 WRITE(IU,'(A)') '[GEN] # GENeral settings'
 MODELNAME=MODELNAME(1:INDEX(MODELNAME,'.RUN',.TRUE.)-1)
 WRITE(IU,'(A)') 'MODELNAME = '//CHAR(39)//TRIM(PBMAN%RESULT_DIR)//CHAR(39)            
 WRITE(IU,'(A)') 'WRITEHELP     =   F # default'
 WRITE(IU,'(A)') 'ECHODEFAULTS  =   F # default'
 WRITE(IU,'(A)') 'RESULT_DIR    =   '//CHAR(39)//TRIM(PBMAN%RESULT_DIR)//CHAR(39)
 WRITE(IU,'(A)') 'IDFDEBUG      =   F # default'
 IF(PBMAN%IFORMAT.EQ.4) WRITE(IU,'(A)') 'RUNTYPE       = SEAWAT'
 IF(PBMAN%IFORMAT.EQ.5) WRITE(IU,'(A)') 'RUNTYPE       = MT3DMS'
 
 !## Select active packages
 LINE=''
 IF(PBMAN%IFORMAT.EQ.4) THEN    ! for Seawat
  DO ITOPIC=1,MAXTOPICS
   SELECT CASE (ITOPIC)  !## Frans: check of dit klopt --> nee, anders: met MC( )%iact etc)
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
 
 WRITE(IU,'(A)') 'COORD_XLL     =   '//TRIM(RTOS(PRJIDF%XMIN,'F',3)) 
 WRITE(IU,'(A)') 'COORD_YLL     =   '//TRIM(RTOS(PRJIDF%YMIN,'F',3)) 
 WRITE(IU,'(A)') 'COORD_XUR     =   '//TRIM(RTOS(PRJIDF%XMAX,'F',3)) 
 WRITE(IU,'(A)') 'COORD_YUR     =   '//TRIM(RTOS(PRJIDF%YMAX,'F',3)) 

 !## look for first
 DO KPER=1,PRJNPER; IF(SIM(KPER)%DELT.GT.0.0D0)EXIT; ENDDO
     
 IF(KPER.GT.PRJNPER)THEN  ! Steady State?   Frans: check of dit de juiste aanpak is, beter verwijzen naar PBMAN parameter?? 
  WRITE(IU,'(A)') '# No timesteps defined'
  WRITE(IU,'(A)') 'START_YEAR    =   2000 # default'
  WRITE(IU,'(A)') 'START_MONTH   =      1 # default'
  WRITE(IU,'(A)') 'START_DAY     =      1 # default'  
 ELSE
  WRITE(IU,'(A)') 'START_YEAR    =   '//TRIM(ITOS(SIM(KPER)%IYR))
  WRITE(IU,'(A)') 'START_MONTH   =   '//TRIM(ITOS(SIM(KPER)%IMH))
  WRITE(IU,'(A)') 'START_DAY     =   '//TRIM(ITOS(SIM(KPER)%IDY))  
  WRITE(IU,'(A)') 'START_HOUR    =   '//TRIM(ITOS(SIM(KPER)%IHR))
  WRITE(IU,'(A)') 'START_MINUTE  =   '//TRIM(ITOS(SIM(KPER)%IMT))
  WRITE(IU,'(A)') 'START_SECOND  =   '//TRIM(ITOS(SIM(KPER)%ISC))
 ENDIF

 PMANAGER_SAVERUNWQ_WRTGEN=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_WRTGEN
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVERUNWQ_U2DREL(IU,KEYNAME,ITOPIC,IPER,ISUBT,ISYS,ILAY,ISPEC)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: KEYNAME
 INTEGER,INTENT(IN) :: IU,ITOPIC,IPER,ISUBT,ISYS,ISPEC,ILAY
 CHARACTER(LEN=256) :: FNAME,LINE,LINE2
 INTEGER :: ICNST, JLAY,JSYS,CPER,JSUBT,NSYS,I
 REAL(KIND=DP_KIND) :: FCT,IMP,CNST
 
 PMANAGER_SAVERUNWQ_U2DREL=.FALSE.
! WRITE(IU,'(A)') '                     # debug u2drel:'//TRIM(TOPICS(ITOPIC)%TNAME)

 !## Skip this topic in case it is not active
 IF(TOPICS(ITOPIC)%IACT_MODEL.EQ.0)THEN; PMANAGER_SAVERUNWQ_U2DREL=.TRUE.; RETURN; ENDIF
 
 !## find proper record for current iper in files array of actual topic
 CPER=0; IF(IPER.GT.0) CPER=PMANAGER_GETCURRENTIPER(IPER,ITOPIC,ITIME,JTIME); CPER=MAX(1,CPER)
 
 !## Correction in case ISUBT is incorrect. Species are considered additional/extra Subtopics in the object
 JSUBT=ISUBT; IF(ISPEC.GT.0) JSUBT=TOPICS(ITOPIC)%NSUBTOPICS-NSPECIES

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
 
 !## no system found 
 IF(JSYS.GT.NSYS)THEN; PMANAGER_SAVERUNWQ_U2DREL=.TRUE.; RETURN; ENDIF
 
 ! Macro�s can be used for ranges layers (_L), rows (_R), columns (_C), stress periods (_P), species (_T) and sub-systems (_S)
 LINE=UTL_CAP(KEYNAME,'U')
 IF(JLAY.GT.0) LINE=TRIM(UTL_SUBST(LINE,'_L?','_L'//TRIM(ITOS(JLAY))))
 IF(IPER.GT.0) LINE=TRIM(UTL_SUBST(LINE,'_P?','_P'//TRIM(ITOS(IPER))))
 IF(ISPEC.GT.0)LINE=TRIM(UTL_SUBST(LINE,'_T?','_T'//TRIM(ITOS(ISPEC))))
 IF(ISYS.GT.0) LINE=TRIM(UTL_SUBST(LINE,'_S?','_S'//TRIM(ITOS(ISYS))))
 
 IF(ICNST.EQ.1)THEN ; LINE2=TRIM(UTL_REALTOSTRING(CNST)) ; ELSE ; LINE2=TRIM(FNAME) ; ENDIF
 IF(FCT.EQ.1.0D0.AND.IMP.EQ.0.0D0)THEN
  WRITE(IU,'(1X,A)') TRIM(LINE)//' = '//TRIM(LINE2)
 ELSE  
  WRITE(IU,'(1X,A)') TRIM(LINE)//' = '//TRIM(UTL_REALTOSTRING(FCT))//' * '// TRIM(LINE2)//' + '//TRIM(UTL_REALTOSTRING(IMP))
 ENDIF
         
 PMANAGER_SAVERUNWQ_U2DREL=.TRUE.

 END FUNCTION PMANAGER_SAVERUNWQ_U2DREL

  !###======================================================================
 SUBROUTINE PMANAGER_SAVEGCG(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU

 WRITE(IU,'(1X,A)') 'MXITER='//TRIM(ITOS(WQ%GCG%MXITER))
 WRITE(IU,'(1X,A)') 'ITER1= '//TRIM(ITOS(WQ%GCG%ITER1))
 WRITE(IU,'(1X,A)') 'ISOLVE='//TRIM(ITOS(WQ%GCG%ISOLVE))
 WRITE(IU,'(1X,A)') 'NCRS=  '//TRIM(ITOS(WQ%GCG%NCRS))
 WRITE(IU,'(1X,A)') 'IPRGCG='//TRIM(ITOS(WQ%GCG%IPRGCG))
 WRITE(IU,'(1X,A)') 'ACCL=  '//TRIM(RTOS(WQ%GCG%ACCL,'G',7))
 WRITE(IU,'(1X,A)') 'CCLOSE='//TRIM(RTOS(WQ%GCG%CCLOSE,'G',7))
 
 END SUBROUTINE PMANAGER_SAVEGCG
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_LOADGCG(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU

 PMANAGER_LOADGCG=.FALSE.
 
 IF(.NOT.UTL_READINITFILE('MXITER',LINE,IU,0))RETURN; READ(LINE,*) WQ%GCG%MXITER
 IF(.NOT.UTL_READINITFILE('ITER1',LINE,IU,0))RETURN;  READ(LINE,*) WQ%GCG%ITER1
 IF(.NOT.UTL_READINITFILE('ISOLVE',LINE,IU,0))RETURN; READ(LINE,*) WQ%GCG%ISOLVE
 IF(.NOT.UTL_READINITFILE('NCRS',LINE,IU,0))RETURN;   READ(LINE,*) WQ%GCG%NCRS
 IF(.NOT.UTL_READINITFILE('IPRGCG',LINE,IU,0))RETURN; READ(LINE,*) WQ%GCG%IPRGCG
 IF(.NOT.UTL_READINITFILE('ACCL',LINE,IU,0))RETURN;   READ(LINE,*) WQ%GCG%ACCL
 IF(.NOT.UTL_READINITFILE('CCLOSE',LINE,IU,0))RETURN; READ(LINE,*) WQ%GCG%CCLOSE

 PMANAGER_LOADGCG=.TRUE. 

 END FUNCTION PMANAGER_LOADGCG

 !###======================================================================
 SUBROUTINE PMANAGER_SAVERCT(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU

 WRITE(IU,'(1X,A)') 'ISOTHM='//TRIM(ITOS(WQ%RCT%ISOTHM))
 WRITE(IU,'(1X,A)') 'IREACT='//TRIM(ITOS(WQ%RCT%IREACT))
 WRITE(IU,'(1X,A)') 'IGETSC='//TRIM(ITOS(WQ%RCT%IGETSC))

 END SUBROUTINE PMANAGER_SAVERCT
 
 !###======================================================================
 LOGICAL FUNCTION PMANAGER_LOADRCT(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU

 PMANAGER_LOADRCT=.FALSE.
 
 IF(.NOT.UTL_READINITFILE('ISOTHM',LINE,IU,0))RETURN; READ(LINE,*) WQ%RCT%ISOTHM
 IF(.NOT.UTL_READINITFILE('IREACT',LINE,IU,0))RETURN; READ(LINE,*) WQ%RCT%IREACT
 IF(.NOT.UTL_READINITFILE('IGETSC',LINE,IU,0))RETURN; READ(LINE,*) WQ%RCT%IGETSC

 PMANAGER_LOADRCT=.TRUE. 

 END FUNCTION PMANAGER_LOADRCT

END MODULE MOD_PMANAGER_WQ