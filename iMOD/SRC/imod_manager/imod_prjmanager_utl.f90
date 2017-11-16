MODULE MOD_PMANAGER_UTL

USE WINTERACTER
USE MOD_PMANAGER_PAR
USE MOD_IDF, ONLY : IDFNULLIFY,IDFDEALLOCATEX,IDFCOPY,IDFALLOCATESXY

CONTAINS

 !###====================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_COARSEGRID(IDF,X1,Y1,X2,Y2,BUFFERCS)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 REAL,INTENT(IN) :: BUFFERCS,X1,Y1,X2,Y2
 REAL,PARAMETER :: INC=1.0                    !## minimal scaling in interest
 DOUBLE PRECISION,PARAMETER :: FINCR=0.02D0
 REAL,PARAMETER :: POWR=0.3     !     
 INTEGER :: NOMAXCELL                         !## maximal # cells in the end
 INTEGER,PARAMETER :: NOMINCELL=1             !## minimal # cells in the centre
 LOGICAL,PARAMETER :: LCLIP=.TRUE.            !## along edge small cells
 INTEGER :: IC1,IC2,IR1,IR2,ORGNCOL,ORGNROW,OC1,OC2,OR1,OR2,I
 INTEGER,ALLOCATABLE,DIMENSION(:) :: PDELR,PDELC
 REAL,DIMENSION(:),ALLOCATABLE :: DX,DY
 
 PMANAGER_SAVEMF2005_COARSEGRID=.FALSE.
 
 NOMAXCELL=INT(BUFFERCS/IDF%DX)

! !## find mid icol
! IC1=INT((USEBOX(1)-SIMBOX(1))/SIMCSIZE)+1
! IC2=INT((USEBOX(3)-SIMBOX(1))/SIMCSIZE)+1
! IR1=INT((SIMBOX(4)-USEBOX(4))/SIMCSIZE)+1
! IR2=INT((SIMBOX(4)-USEBOX(2))/SIMCSIZE)+1

 IC1=INT((X1-IDF%XMIN)/IDF%DX)+1
 IC2=INT((X2-IDF%XMIN)/IDF%DX)+1
 IR1=INT((IDF%YMAX-Y1)/IDF%DY)+1
 IR2=INT((IDF%YMAX-Y2)/IDF%DY)+1

 ORGNCOL=IDF%NCOL
 ORGNROW=IDF%NROW

 ALLOCATE(PDELR(IDF%NCOL),PDELC(IDF%NROW))
 CALL MODELLHS1(PDELR,ORGNCOL,IDF%NCOL,IC1,IC2,OC1,OC2,INC,FINCR,POWR,NOMINCELL,NOMAXCELL,LCLIP)
 CALL MODELLHS1(PDELC,ORGNROW,IDF%NROW,IR1,IR2,OR1,OR2,INC,FINCR,POWR,NOMINCELL,NOMAXCELL,LCLIP)

! IDF%NROW=NROW
! IDF%NCOL=NCOL
 IDF%IEQ=1; IF(.NOT.IDFALLOCATESXY(IDF))RETURN
 
 ALLOCATE(DX(IDF%NCOL),DY(IDF%NROW))

 CALL PMANAGER_SAVEMF2005_COARSEGRID_RESULT(PDELR,DX,IDF%NCOL,ORGNCOL,IDF%DX)
 CALL PMANAGER_SAVEMF2005_COARSEGRID_RESULT(PDELC,DY,IDF%NROW,ORGNROW,IDF%DY)

 IDF%SX(0)=IDF%XMIN
 DO I=1,IDF%NCOL; IDF%SX(I)=IDF%SX(I-1)+DX(I); ENDDO
 IDF%SY(0)=IDF%YMAX
 DO I=1,IDF%NROW; IDF%SY(I)=IDF%SY(I-1)-DY(I); ENDDO

 DEALLOCATE(DX,DY)
 
 PMANAGER_SAVEMF2005_COARSEGRID=.TRUE.

 END FUNCTION PMANAGER_SAVEMF2005_COARSEGRID

 !###====================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_COARSEGRID_RESULT(IX,DX,NX,NXORG,SIMCSIZE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NX,NXORG
 REAL,INTENT(IN) :: SIMCSIZE
 REAL,INTENT(OUT),DIMENSION(NX) :: DX
 INTEGER,INTENT(IN),DIMENSION(NXORG) :: IX
 INTEGER :: I,J,K
 
 J=1
 K=1
 DO I=2,NXORG
  IF(IX(I).NE.IX(I-1))THEN
   DX(K)=SIMCSIZE*REAL(J)
   K=K+1
   J=1
  ELSE
   J=J+1
  ENDIF
 END DO
 DX(K)=SIMCSIZE*REAL(J)

 END SUBROUTINE PMANAGER_SAVEMF2005_COARSEGRID_RESULT

 !###======================================================================
 LOGICAL FUNCTION PMANAGER_SAVEMF2005_PCK_GETMINMAX(X,NCOL,NROW,XB,MINV,MAXV,IFBND,EXFNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: EXFNAME
 REAL,INTENT(IN),DIMENSION(NCOL,NROW) :: X,XB
 INTEGER,INTENT(IN) :: NROW,NCOL,IFBND
 INTEGER :: IROW,ICOL,I
 REAL,INTENT(OUT) :: MINV,MAXV

 PMANAGER_SAVEMF2005_PCK_GETMINMAX=.FALSE.

 MINV=HUGE(1.0); MAXV=-HUGE(1.0); I=0
 DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
  !## skip nodata points
  IF(X(ICOL,IROW).EQ.HNOFLOW)CYCLE
  !## check on active nodes only
  IF(IFBND.EQ.1)THEN
   IF(XB(ICOL,IROW).NE.0)THEN 
    MINV=MIN(MINV,X(ICOL,IROW))
    MAXV=MAX(MAXV,X(ICOL,IROW))
    I   =I+1
   ENDIF
  ELSE
   MINV=MIN(MINV,X(ICOL,IROW))
   MAXV=MAX(MAXV,X(ICOL,IROW))
   I   =I+1
  ENDIF
 ENDDO; ENDDO

 IF(I.LE.0)THEN
  MAXV=MINV
!  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot find any data for active cells'//CHAR(13)// &
!    TRIM(EXFNAME),'Error')
 ENDIF

 PMANAGER_SAVEMF2005_PCK_GETMINMAX=.TRUE.
 
 END FUNCTION PMANAGER_SAVEMF2005_PCK_GETMINMAX

 !###======================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_ALLOCATEPCK(N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER :: I
 
 IF(ALLOCATED(PCK))CALL PMANAGER_SAVEMF2005_DEALLOCATEPCK()

 ALLOCATE(PCK(N))
 DO I=1,N
  CALL IDFNULLIFY(PCK(I))
  CALL IDFCOPY(BND(1),PCK(I)) 
 ENDDO
 
 END SUBROUTINE PMANAGER_SAVEMF2005_ALLOCATEPCK

 !###======================================================================
 SUBROUTINE PMANAGER_SAVEMF2005_DEALLOCATEPCK()
 !###======================================================================
 IMPLICIT NONE
 INTEGER:: N,I
 
 IF(.NOT.ALLOCATED(PCK))RETURN
 
 N=SIZE(PCK)
 DO I=1,N; CALL IDFDEALLOCATEX(PCK(I)); ENDDO
 DEALLOCATE(PCK)
  
 END SUBROUTINE PMANAGER_SAVEMF2005_DEALLOCATEPCK

END MODULE MOD_PMANAGER_UTL