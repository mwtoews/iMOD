MODULE MODULE_GEN_HFB

USE MOD_UTL, ONLY : UTL_GETUNIT !UTL_SWAPSLASH,UTL_STRING,UTL_CAP,UTL_FILENAME,
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_INTERSECT, ONLY : INTERSECT_EQUI,INTERSECT_DEALLOCATE
USE MOD_INTERSECT_PAR
USE MOD_IDF_PAR

INTEGER(KIND=2),POINTER,DIMENSION(:,:),PRIVATE :: HFBPOS,HFBPOS_BU

CONTAINS

 !###====================================================================
 SUBROUTINE GEN_HFB_RP(IDF,FNAME)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 REAL :: X1,Y1,X2,Y2,X,Y
 REAL :: XMIN,XMAX,YMIN,YMAX
 INTEGER :: IU,I,J,IOS,ID,N,IROW,ICOL

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,ACTION='READ',STATUS='OLD',FORM='FORMATTED')

  
! MX=1000; IF(ASSOCIATED(HFBPOS)) DEALLOCATE(HFBPOS)
!
! ALLOCATE(XA(MX),YA(MX),FA(MX),LN(MX))
! ALLOCATE(HFBPOS(500,4)) 
!
! NHFB=0
!
! ILINE=0
! 
! JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=FNAME,ACTION='READ',STATUS='OLD',FORM='FORMATTED')
! DO
!  ILINE=ILINE+1
!  II=0
!  READ(JU,'(A256)') LINE; CALL UTL_STRING(LINE); LINE=UTL_CAP(LINE,'U')
!  IOS=0
!  IF(INDEX(LINE,'END').GT.0)EXIT
!  READ(LINE,*,IOSTAT=IOS) ID; IF(IOS.NE.0)EXIT
!  DO
!   READ(JU,'(A256)') LINE; CALL UTL_STRING(LINE); LINE=UTL_CAP(LINE,'U')
!   IOS=0; IF(INDEX(LINE,'END').GT.0)EXIT
!   READ(LINE,*,IOSTAT=IOS) X2,Y2; IF(IOS.NE.0)EXIT 
!
!   IF(II.GT.0)THEN
!    !## intersect line
!    XX1=X1; YY1=Y1; XX2=X2; YY2=Y2
!
!    N=0
!    IF(IDF%IEQ.EQ.0)THEN
!     !## intersect line with rectangular-  regular-grid
!     CALL INTERSECT_EQUI(IDF%XMIN,IDF%XMAX,IDF%YMIN,IDF%YMAX,IDF%DX,XX1,XX2,YY1,YY2,N,.TRUE.,.TRUE.)
!    ELSE
!     !## intersect line with rectangular-irregular-grid
!     CALL INTERSECT_NONEQUI(IDF%SX,IDF%SY,IDF%NROW,IDF%NCOL,XX1,XX2,YY1,YY2,N,.TRUE.,.TRUE.)
!    ENDIF
!       
!    IF(NHFB+N.GT.SIZE(HFBPOS,1))THEN
!     ALLOCATE(HFBPOS_BU(NHFB*2,4))
!     DO J=1,NHFB
!      DO I=1,4; HFBPOS_BU(I,:)=HFBPOS(I,:); ENDDO
!     ENDDO
!     DEALLOCATE(HFBPOS)
!     HFBPOS=>HFBPOS_BU
!    ENDIF
!      
!    !## fill result array - check whether change in icol/irow is changed simultenously. This is signal that the line passes
!    !## through the centre which is not feasible, in that case, we add an extra node +1 for icol or irow
!    DO L=1,N
!     ICOL=INT(XA(L)); IROW=INT(YA(L)) 
!     IF(ICOL.GT.0.AND.IROW.GT.0.AND.ICOL.LE.IDF%NCOL.AND.IROW.LE.IDF%NROW)THEN
!      NHFB          =NHFB+1
!      HFBPOS(NHFB,1)=ICOL
!      HFBPOS(NHFB,2)=IROW
!      HFBPOS(NHFB,3)=ILINE
!      HFBPOS(NHFB,4)=INT(FA(L))
!     ENDIF
!    ENDDO
!   ENDIF
!   II=II+1; X1=X2; Y1=Y2
!  ENDDO
! ENDDO
!
! CALL INTERSECT_DEALLOCATE()

 END SUBROUTINE GEN_HFB_RP

!!###====================================================================
!SUBROUTINE HFB1CALC()
!!###====================================================================
!USE MOD_UTL, ONLY : PRINTTEXT,ITOS,GETRCL,UTL_SWAPSLASH,GETUNIT,OPENASC
!USE MODFLOW
!USE BASVAR, ONLY : NROW,NCOL 
!USE HFBVAR
!USE PCKVAR, ONLY : IANI,XANI
!USE GLBVAR, ONLY : SIMBOX,SIMCSIZE,NMOD,MMOD,PANI,PHFB,TXTMOD,LINE,NLAY,ROOTRES,IEXPORT
!IMPLICIT NONE
!INTEGER,PARAMETER :: INEIGHBOURS=2
!INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:,:) :: IPC
!INTEGER :: IROW,ICOL,ILAY,I,J,K,KK,IR,IC,IHFB,ILINE,JLINE,IR1,IC1,IR2,IC2,IP1,IP2,JCOL,JROW,ILOC,JLOC
!REAL :: FCT
!
!IF(MMOD(PHFB).EQ.0)RETURN
!IF(NMOD(PHFB).EQ.0)RETURN
!
!CALL PRINTTEXT('',0); CALL PRINTTEXT('Computing face-blocks due to Horizontal Flow Barrier (HFB) ...',0)
!
!IF(ALLOCATED(IUHFB))DEALLOCATE(IUHFB); IF(ALLOCATED(NHFB))DEALLOCATE(NHFB)
!ALLOCATE(IUHFB(0:NLAY*2),NHFB(NLAY)); NHFB=0; IUHFB=0
!DO ILAY=1,NLAY
! LINE=TRIM(ROOTRES)//CHAR(92)//'modflow.hfb_l'//TRIM(ITOS(ILAY))
! CALL UTL_SWAPSLASH(LINE); CALL OPENASC(IUHFB(ILAY),LINE,'W')
! LINE=TRIM(ROOTRES)//CHAR(92)//'modflow_hfb_l'//TRIM(ITOS(ILAY))//'.gen'
! CALL UTL_SWAPSLASH(LINE); CALL OPENASC(IUHFB(NLAY+ILAY),LINE,'W')
!ENDDO 
!
!IF(ALLOCATED(IPC))DEALLOCATE(IPC); ALLOCATE(IPC(NCOL,NROW,2))
!
!DO IHFB=1,SIZE(HFBFCT)
!
! !## fill in ipc
! IPC=INT(0,1)
! 
! !## startpoint
! J=HFBIP(IHFB-1)+1; JCOL=HFBPOS(J,1); JROW=HFBPOS(J,2); JLINE=HFBPOS(J,3) !; JLOC=HFBPOS(J,4)
! 
! !## process line
! DO J=HFBIP(IHFB-1)+2,HFBIP(IHFB)
!  ILINE=HFBPOS(J,3)
!  !## fill in ipc
!  IF(ILINE.EQ.JLINE)THEN
!   IC1=HFBPOS(J-1,1); IC2=HFBPOS(J  ,1)
!   IR1=HFBPOS(J-1,2); IR2=HFBPOS(J  ,2)
!   IP1=HFBPOS(J-1,4); IP2=HFBPOS(J  ,4)
!   CALL GEN_HFB_FACES(IC1,IC2,IR1,IR2,IP1,IP2,IPC,NROW,NCOL)
!  ENDIF
!  
!  !## write fault-faces
!  IF(ILINE.NE.JLINE.OR.J.EQ.HFBIP(IHFB))THEN
!   CALL HFB1EXPORT(IPC,NROW,NCOL,HFBFCT(IHFB),IUHFB(HFBILAY(IHFB)),IUHFB(NLAY+HFBILAY(IHFB)),NHFB(HFBILAY(IHFB)),HFBILAY(IHFB),JLINE)
!   !## reset for the next line   
!   IPC=INT(0,1); JLINE=HFBPOS(J,3)
!  ENDIF
!
! ENDDO
!ENDDO
!
!DO ILAY=0,NLAY*2; IF(IUHFB(ILAY).GT.0)CLOSE(IUHFB(ILAY)); ENDDO; IUHFB=0
!
!IF(ALLOCATED(HFBFCT)) DEALLOCATE(HFBFCT);  IF(ALLOCATED(HFBIMP))DEALLOCATE(HFBIMP)
!IF(ALLOCATED(HFBILAY))DEALLOCATE(HFBILAY); IF(ALLOCATED(HFBIP)) DEALLOCATE(HFBIP)
!IF(ASSOCIATED(HFBPOS))DEALLOCATE(HFBPOS)
!
!CALL PRINTTEXT('Finished computing face-blocks due to Horizontal Flow Barrier (HFB).',0); CALL PRINTTEXT('',0)
!
!!## remove anisotropy by hfb-elements - conflicts per modellayer
!IF(NMOD(PANI).GT.0)THEN 
! CALL PRINTTEXT('',0)
! CALL PRINTTEXT('Checking Anisotropy and Horizontal Flow Barrier (HFB) combinations.',0)
! CALL PRINTTEXT('Remove   Anisotropy for HFB occurences.',0)
! DO ILAY=1,NLAY
!  IPC=INT(0,1)
!  LINE=TRIM(ROOTRES)//CHAR(92)//'modflow.hfb_l'//TRIM(ITOS(ILAY))
!  CALL UTL_SWAPSLASH(LINE); CALL OPENASC(IUHFB(ILAY),LINE,'RW')
!  DO I=1,NHFB(ILAY)
!   READ(IUHFB(ILAY),'(4I10,F10.0,I10)') IR1,IC1,IR2,IC2,FCT,ILINE
!   IPC(IC1,IR1,1)=INT(1,1)
!   IF(IR1.EQ.IR2)IPC(IC2,IR1,1)=INT(1,1)
!   IF(IC1.EQ.IC2)IPC(IC1,IR2,1)=INT(1,1)
!  ENDDO
!  CLOSE(IUHFB(ILAY))
!     
!  KK=0; K=0
!  DO I=1,NMOD(PANI)
!   IF(XANI(I,1).LT.1.0)THEN
!    CALL GETRCL(IANI(I),NROW,NCOL,J,IROW,ICOL)
!    K=K+1
!    IF(IPC(ICOL,IROW,1).EQ.INT(1,1))THEN
!     XANI(I,1)=1.0 !## isotropic in case kd=absent
!     KK=KK+1
!    ENDIF
!   ENDIF 
!  ENDDO
!  IF(K.GT.0)CALL PRINTTEXT(TRIM(ITOS(KK))//' out of '//TRIM(ITOS(K))//' '//TRIM(TXTMOD(PANI))// &
!     ' elements removed, remain ('//TRIM(ITOS(K-J))//') for layer '//TRIM(ITOS(ILAY)),0)
! ENDDO
!
! CALL PRINTTEXT('',0); CALL PRINTTEXT('Finished Checking HFB-ANI Combinations',0)
!ENDIF
!
!IF(ALLOCATED(IPC))DEALLOCATE(IPC)
!
!RETURN
!END SUBROUTINE
!
!###====================================================================
SUBROUTINE GEN_HFB_FACES(IC1,IC2,IR1,IR2,IP1,IP2,IPC,NROW,NCOL)
!###====================================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: IC1,IC2,IR1,IR2,IP1,IP2
INTEGER,INTENT(IN) :: NROW,NCOL
INTEGER(KIND=1),INTENT(INOUT),DIMENSION(NCOL,NROW,2) :: IPC
INTEGER,DIMENSION(2) :: JPC,JPR,JC,JR,JP
INTEGER :: I,IC,IR

JC(1)=IC1; JC(2)=IC2
JR(1)=IR1; JR(2)=IR2
JP(1)=IP1; JP(2)=IP2

DO I=1,2
 IF(JP(I).EQ.2.OR.JP(I).EQ.3)JPC(I)=JC(I)
 IF(JP(I).EQ.1.OR.JP(I).EQ.4)JPC(I)=JC(I)-1
 IF(JP(I).EQ.1.OR.JP(I).EQ.2)JPR(I)=JR(I)-1
 IF(JP(I).EQ.3.OR.JP(I).EQ.4)JPR(I)=JR(I)
ENDDO

!## do nothing, is similar point
IF(JPR(1).EQ.JPR(2).AND.JPC(1).EQ.JPC(2))RETURN

!## do nothing whenever jpc.eq.0 or jpr.eq.0
IF(JPC(1).EQ.0.OR.JPC(2).EQ.0)RETURN
IF(JPR(1).EQ.0.OR.JPR(2).EQ.0)RETURN

!## horizontal fault ipc(,,1)=1
IF(JPR(1).EQ.JPR(2).AND.JPC(1).NE.JPC(2))THEN
 IC=MAX(JPC(1),JPC(2)); IR=JPR(1); IPC(IC,IR,2)=INT(1,1)
ENDIF
!## vertical fault ipc(,,2)=1
IF(JPC(1).EQ.JPC(2).AND.JPR(1).NE.JPR(2))THEN
 IC=JPC(1); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,1)=INT(1,1)
ENDIF
!## diagonal, add two faults
IF(JPR(1).NE.JPR(2).AND.JPC(1).NE.JPC(2))THEN
 !## goto to the west
 IF(JPC(1).GT.JPC(2))THEN
  !## goto to the north-west
  IF(JPR(1).GT.JPR(2))THEN
   IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,1)=INT(1,1) !## vertical
   IC=MAX(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,2)=INT(1,1) !## horizontal
  !## goto to the south-west
  ELSE
   IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,1)=INT(1,1) !## vertical
   IC=MAX(JPC(1),JPC(2)); IR=MIN(JPR(1),JPR(2)); IPC(IC,IR,2)=INT(1,1) !## horizontal
  ENDIF
 !## goto to the east
 ELSE
  !## goto to the north-east
  IF(JPR(1).GT.JPR(2))THEN
   IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,1)=INT(1,1) !## vertical
   IC=MAX(JPC(1),JPC(2)); IR=MIN(JPR(1),JPR(2)); IPC(IC,IR,2)=INT(1,1) !## horizontal   
  !## goto to the south-east
  ELSE
   IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,1)=INT(1,1) !## vertical
   IC=MAX(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,2)=INT(1,1) !## horizontal  
  ENDIF
 ENDIF
ENDIF

END SUBROUTINE GEN_HFB_FACES
!
!!###====================================================================
!SUBROUTINE HFB1FM()
!!###====================================================================
!USE MOD_UTL, ONLY : PRINTTEXT,ITOS,GETRCL,UTL_SWAPSLASH,GETUNIT,OPENASC
!USE BASVAR, ONLY : CC,CR,DELR,DELC,TOP,BOT,NCOL,NROW
!USE HFBVAR
!USE GLBVAR, ONLY : SIMBOX,SIMCSIZE,NMOD,MMOD,PANI,PHFB,PTOP,PBOT,TXTMOD,LINE, &
!   NLAY,ROOTRES,IEXPORT
!IMPLICIT NONE
!REAL :: FCT,DZ
!INTEGER :: ILAY,I,ILINE,IR1,IR2,IC1,IC2
!LOGICAL :: LTB
!
!IF(MMOD(PHFB).LE.0)RETURN; IF(NMOD(PHFB).LE.0)RETURN
!LTB=.FALSE.; IF(MMOD(PTOP).GT.0.AND.MMOD(PBOT).GT.0)LTB=.TRUE.
!
!CALL PRINTTEXT('Assigning face-blocks due to Horizontal Flow Barrier (HFB) ...',0)
!
!IF(IEXPORT.EQ.1.OR.IEXPORT.EQ.2)THEN
! LINE=TRIM(ROOTRES)//CHAR(92)//'modflow.hfb'
! CALL UTL_SWAPSLASH(LINE); CALL OPENASC(IUHFB(0),LINE,'W')
! WRITE(IUHFB(0),'(2I10)') SUM(NHFB),1
!ENDIF
!DO ILAY=1,NLAY
! LINE=TRIM(ROOTRES)//CHAR(92)//'modflow.hfb_l'//TRIM(ITOS(ILAY))
! CALL UTL_SWAPSLASH(LINE); CALL OPENASC(IUHFB(ILAY),LINE,'RW')
! IF(IEXPORT.EQ.1.OR.IEXPORT.EQ.2)WRITE(IUHFB(0),'(I10)') NHFB(ILAY)
! DO I=1,NHFB(ILAY)
! 
!  READ(IUHFB(ILAY),'(4I10,F10.0,I10)') IR1,IC1,IR2,IC2,FCT,ILINE
!  
!  !## outside model
!  IF(IC1.LE.0.OR.IC1.GT.NCOL.OR.IC2.LE.0.OR.IC2.GT.NCOL.OR. &
!     IR1.LE.0.OR.IR1.GT.NROW.OR.IR2.LE.0.OR.IR2.GT.NROW)CYCLE
!     
!  IF(IEXPORT.EQ.0.OR.IEXPORT.GE.3)THEN
!   !## fault in y-axes, change conductance in x-axes
!   IF(IR1.EQ.IR2)THEN
!    IF(LTB)THEN
!     IF(FCT.EQ.0.0)THEN
!      CR(IC1,IR1,ILAY)=0.0
!     ELSE
!      DZ=0.5*(TOP(IC1,IR1,ILAY)-BOT(IC1,IR1,ILAY))+ &
!         0.5*(TOP(IC2,IR1,ILAY)-BOT(IC2,IR1,ILAY))
!      DZ=MAX(0.0,DZ)
!      !## not to become more than original
!      CR(IC1,IR1,ILAY)=MIN(CR(IC1,IR1,ILAY),((DELC(IR1-1)-DELC(IR1))*DZ)/FCT)
!     ENDIF
!    ELSE
!     CR(IC1,IR1,ILAY)=FCT*CR(IC1,IR1,ILAY)
!    ENDIF
!   ENDIF
!   !## fault in x-axes, change conductance in y-axes 
!   IF(IC1.EQ.IC2)THEN
!    IF(LTB)THEN
!     IF(FCT.EQ.0.0)THEN
!      CC(IC1,IR1,ILAY)=0.0
!     ELSE
!      DZ=0.5*(TOP(IC1,IR1,ILAY)-BOT(IC1,IR1,ILAY))+ &
!         0.5*(TOP(IC1,IR2,ILAY)-BOT(IC1,IR2,ILAY))
!      DZ=MAX(0.0,DZ)
!      !## not to become more than original
!      CC(IC1,IR1,ILAY)=MIN(CC(IC1,IR1,ILAY),((DELR(IC1)-DELR(IC1-1))*DZ)/FCT)
!     ENDIF
!    ELSE
!     CC(IC1,IR1,ILAY)=FCT*CC(IC1,IR1,ILAY)
!    ENDIF
!   ENDIF
!  ENDIF
!  
!  IF(IEXPORT.EQ.1.OR.IEXPORT.EQ.2)WRITE(IUHFB(0),'(4I10,G10.4,I10)') IR1,IC1,IR2,IC2,FCT,ILINE
! ENDDO
! CLOSE(IUHFB(ILAY),STATUS='DELETE') 
!ENDDO
!
!!IF(IIDEBUG.EQ.1)THEN
!! IF(MMOD(PKDW).EQ.1)IPCK=PKDW
!! IF(MMOD(PKHV).EQ.1)IPCK=PKHV
!! CALL WRITE1DATA(CC,NROW,NCOL,NLAY,0.0,TRIM(ROOTRES)//CHAR(92)//'cc',1,'cc_hfb',IPCK,SIMBOX,DELR,DELC,0)
!! CALL WRITE1DATA(CR,NROW,NCOL,NLAY,0.0,TRIM(ROOTRES)//CHAR(92)//'cr',1,'cr_hfb',IPCK,SIMBOX,DELR,DELC,0)
!!ENDIF
!
!DEALLOCATE(IUHFB,NHFB)
!
!RETURN
!END SUBROUTINE
!
END MODULE MODULE_GEN_HFB
