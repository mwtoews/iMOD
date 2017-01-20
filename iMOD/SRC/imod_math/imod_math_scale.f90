!!  Copyright (C) Stichting Deltares, 2005-2016.
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
MODULE MOD_MATH_SCALE

 USE WINTERACTER
 USE RESOURCE
 USE MOD_LUDCMP, ONLY : LUDCMP
 USE MOD_MATH_SCALE_PAR
 USE MOD_IDF, ONLY : IDFREAD,IDFALLOCATEX,IDFFILLCOMMENT,IDFWRITE,IDFGETVAL,IDFCOPY,IDFNULLIFY,IDFDEALLOCATE, &
 IDFGETXYVAL,IDFGETLOC,IDFREADSCALE
 USE MOD_UTL, ONLY : UTL_CHECKNAME,UTL_GETMED,NEWLINE,UTL_IDFSNAPTOGRID_LLC,UTL_MESSAGEHANDLE, &
                     UTL_WAITMESSAGE,UTL_DIRINFO,UTL_CAP,RTOS
 USE MOD_POLINT, ONLY : POL1LOCATE,POL1INTMAIN
 USE MODPLOT, ONLY : MPW
 USE MOD_PCG, ONLY : PCG2AP

 TYPE(IDFOBJ),DIMENSION(:),ALLOCATABLE,PRIVATE :: MATH,OUTF

 INTEGER,ALLOCATABLE,DIMENSION(:),PRIVATE :: IOS

 REAL,ALLOCATABLE,DIMENSION(:),PRIVATE :: DELR,DELC
 INTEGER,ALLOCATABLE,DIMENSION(:,:),PRIVATE :: PDELR,PDELC
 REAL,ALLOCATABLE,DIMENSION(:),PRIVATE :: FREQ
 REAL,ALLOCATABLE,DIMENSION(:,:),PRIVATE :: A !,L,U
 REAL,ALLOCATABLE,DIMENSION(:),PRIVATE :: B,DX,DY
 INTEGER,PRIVATE :: IULOG

 INTEGER,PRIVATE :: ISCALE

 INTEGER,PARAMETER,PRIVATE :: MXITER1=100 !## outer (linear system)
 INTEGER,PARAMETER,PRIVATE :: MXITER2=500 !## inner (linear system)
 REAL,PARAMETER,PRIVATE :: HCLOSE=0.001   !## m
 REAL,PARAMETER,PRIVATE :: RCLOSE=0.001   !## m3/dag
 REAL,PARAMETER,PRIVATE :: HNOFLOW=-999.99
 REAL,PRIVATE :: RELAX=1.0
 INTEGER,PARAMETER,PRIVATE :: IDAMPING=1

 REAL,ALLOCATABLE,DIMENSION(:,:,:),PRIVATE :: RHS,CC,CR,CV,P,V,SS,CD,HCOF
 REAL,ALLOCATABLE,DIMENSION(:),PRIVATE :: DZ,TZ
 DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:),PRIVATE :: HNEW
 INTEGER,ALLOCATABLE,DIMENSION(:,:,:),PRIVATE :: IB

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION MATH1SCALE(IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: I,J,N,NROW,NCOL
 LOGICAL :: LEX,LSCALE
 CHARACTER(LEN=256) :: ROOT,SCLTXT
 CHARACTER(LEN=52) :: WC
 REAL,ALLOCATABLE,DIMENSION(:) :: IX

 MATH1SCALE=.FALSE.

 !## 3d simulation --- get files
 IF(SCLTYPE_UP.EQ.14)THEN
  I=INDEX(IDFNAMES(1),'\',.TRUE.); ROOT=IDFNAMES(1)(:I-1); WC=TRIM(IDFNAMES(1)(I+1:))
  CALL IOSDIRENTRYTYPE('F'); CALL IOSDIRCOUNT(TRIM(ROOT),TRIM(WC),N)
  DEALLOCATE(IDFNAMES); ALLOCATE(IDFNAMES(N))
  CALL UTL_DIRINFO(TRIM(ROOT),TRIM(WC),IDFNAMES,N,'F')
  IF(N.EQ.0)THEN
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No files found in: '//TRIM(ROOT),'Warning')
   IF(IBATCH.EQ.1)WRITE(*,*) 'No files found in: '//TRIM(ROOT)
   RETURN
  ENDIF
  DO I=1,SIZE(IDFNAMES); IDFNAMES(I)=TRIM(ROOT)//'\'//TRIM(IDFNAMES(I)); ENDDO
 ENDIF

 !## check input files
 DO I=1,SIZE(IDFNAMES)
  INQUIRE(FILE=IDFNAMES(I),EXIST=LEX)
  IF(.NOT.LEX)THEN
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Input Map: '//TRIM(IDFNAMES(I))//CHAR(13)// &
    ' does not exists!','Warning')
   IF(IBATCH.EQ.1)WRITE(*,*) 'Input Map: '//TRIM(IDFNAMES(I))//' does not exists!'
   RETURN
  ENDIF
 ENDDO

 !## get outputnames
 CALL UTL_CHECKNAME(OUTNAMES(1),'IDF')
 I=INDEX(OUTNAMES(1),'.',.TRUE.)-1
 ROOT=OUTNAMES(1)(:I)
 SELECT CASE (SCLTYPE_UP)
  CASE (11,12,13)
   DEALLOCATE(OUTNAMES); ALLOCATE(OUTNAMES(2))
   OUTNAMES(1)=TRIM(ROOT)//'_maximal.idf'
   OUTNAMES(2)=TRIM(ROOT)//'_minimal.idf'
  CASE (14)
   DEALLOCATE(OUTNAMES); ALLOCATE(OUTNAMES(8)); OUTNAMES=''
   IF(DHX.NE.0.0)THEN
    OUTNAMES(1)=TRIM(ROOT)//'_KX_min.idf'
    OUTNAMES(4)=TRIM(ROOT)//'_KX_max.idf'
   ENDIF
   IF(DHZ.NE.0.0)THEN
    OUTNAMES(3)=TRIM(ROOT)//'_SUMVC.idf'
    OUTNAMES(6)=TRIM(ROOT)//'_SIMVC.idf'
   ENDIF
   IF(DHY.NE.0.0)THEN
    OUTNAMES(2)=TRIM(ROOT)//'_KY_min.idf'
    OUTNAMES(5)=TRIM(ROOT)//'_KY_max.idf'
   ENDIF
   OUTNAMES(7)=TRIM(ROOT)//'_bot.idf'
   OUTNAMES(8)=TRIM(ROOT)//'_top.idf'
 END SELECT

 !## check conflict input/output files
 DO I=1,SIZE(IDFNAMES)
  DO J=1,SIZE(OUTNAMES)
   IF(TRIM(UTL_CAP(IDFNAMES(I),'U')).EQ.TRIM(UTL_CAP(OUTNAMES(J),'U')))THEN
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Duplicate Filenames given!'//CHAR(13)//TRIM(IDFNAMES(I)),'Warning')
    IF(IBATCH.EQ.1)WRITE(*,*) 'Duplicate Filenames given: '//TRIM(IDFNAMES(I))
    RETURN
   ENDIF
  ENDDO
 ENDDO

 IF(IBATCH.EQ.0)THEN
  DO I=1,SIZE(OUTNAMES)
   INQUIRE(FILE=OUTNAMES(I),EXIST=LEX)
   IF(LEX)THEN
    CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Output Map: '//TRIM(OUTNAMES(I))//CHAR(13)// &
    'already exists, overwrite it?','Question')
    IF(WINFODIALOG(4).NE.1)RETURN
   ENDIF
  ENDDO
  CALL WINDOWSELECT(0)
  CALL WINDOWOUTSTATUSBAR(4,'Press Escape to stop operation: Progress 0%')
  CALL WMESSAGEENABLE(KEYDOWN,1)
 ENDIF

 !## allocate objects for all input files
 ALLOCATE(MATH(SIZE(IDFNAMES)))
 !## nullify pointers in object
 NCOL=0; NROW=0
 DO I=1,SIZE(MATH)
  CALL IDFNULLIFY(MATH(I))
  !## read idf
  IF(.NOT.IDFREAD(MATH(I),IDFNAMES(I),0))RETURN
  NCOL=NCOL+MATH(I)%NCOL
  NROW=NROW+MATH(I)%NROW
 ENDDO
 IF(NCOL/SIZE(MATH).NE.MATH(1)%NCOL.OR. &
    NROW/SIZE(MATH).NE.MATH(1)%NROW)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'ALL IDF-files should be equally in size/dimension','Warning')
  IF(IBATCH.EQ.1)WRITE(*,*) 'ALL IDF-files should be equally in size/dimension'
  RETURN
 ENDIF
 IF(SIZE(IDFNAMES).GT.1)THEN
  !## sort idf to fit top/bottom sequence
  ALLOCATE(IX(SIZE(IDFNAMES))) 
  DO I=1,SIZE(IDFNAMES); IX(I)=REAL(I); ENDDO
  CALL SORTEM(1,SIZE(IDFNAMES),MATH%TOP,1,IX,MATH%TOP,MATH%TOP,MATH%TOP,MATH%TOP,MATH%TOP,MATH%TOP)
  DO I=1,SIZE(IDFNAMES); CLOSE(MATH(I)%IU); ENDDO
  J=SIZE(MATH)+1
  DO I=1,SIZE(MATH)
   IF(MATH(I)%ITB.NE.1)THEN
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'File '//TRIM(MATH(I)%FNAME)//' does not contain TOP/BOTTOM information','Warning')
    IF(IBATCH.EQ.1)WRITE(*,*) 'File '//TRIM(MATH(I)%FNAME)//' does not contain TOP/BOTTOM information'
    RETURN
   ENDIF
   J=J-1
   IF(IBATCH.EQ.1)WRITE(*,*) 'File '//TRIM(MATH(I)%FNAME)//' will be used.'
   !## read idf
   IF(.NOT.IDFREAD(MATH(I),IDFNAMES(INT(IX(J))),0))RETURN
  ENDDO
  DEALLOCATE(IX)
 ENDIF

 !## allocate objects for all output files
 ALLOCATE(OUTF(SIZE(OUTNAMES)))
 DO I=1,SIZE(OUTF); CALL IDFNULLIFY(OUTF(I)); ENDDO

 !## determine size/parameters of result idf
 OUTF(1)%NODATA=MATH(1)%NODATA

 !## current zoom window
 IF(IIEXT.EQ.1)THEN
  I   =(MPW%XMIN-MATH(1)%XMIN)/MATH(1)%DX
  OUTF(1)%XMIN =MATH(1)%XMIN+I*MATH(1)%DX
  I   =(MPW%XMAX-MATH(1)%XMIN)/MATH(1)%DX
  OUTF(1)%XMAX =MATH(1)%XMIN+I*MATH(1)%DX
  I   =(MATH(1)%YMAX-MPW%YMIN)/MATH(1)%DY
  OUTF(1)%YMIN =MATH(1)%YMAX-I*MATH(1)%DY
  I   =(MATH(1)%YMAX-MPW%YMAX)/MATH(1)%DY
  OUTF(1)%YMAX =MATH(1)%YMAX-I*MATH(1)%DY
  OUTF(1)%XMIN=MAX(MATH(1)%XMIN,OUTF(1)%XMIN)
  OUTF(1)%XMAX=MIN(MATH(1)%XMAX,OUTF(1)%XMAX)
  OUTF(1)%YMIN=MAX(MATH(1)%YMIN,OUTF(1)%YMIN)
  OUTF(1)%YMAX=MIN(MATH(1)%YMAX,OUTF(1)%YMAX)
 !## first named selected idf's
 ELSEIF(IIEXT.EQ.2)THEN
  OUTF(1)%XMIN =MATH(1)%XMIN
  OUTF(1)%XMAX =MATH(1)%XMAX
  OUTF(1)%YMIN =MATH(1)%YMIN
  OUTF(1)%YMAX =MATH(1)%YMAX
 !## given coordinates
 ELSEIF(IIEXT.EQ.3)THEN
  OUTF(1)%XMIN =MPW%XMIN
  OUTF(1)%XMAX =MPW%XMAX
  OUTF(1)%YMIN =MPW%YMIN
  OUTF(1)%YMAX =MPW%YMAX
 ENDIF

 !## equidistantial idf
 OUTF(1)%IEQ=0; OUTF(1)%DX=SCLSIZE; OUTF(1)%DY=SCLSIZE

! CALL UTL_IDFSNAPTOGRID(OUTF(1)%XMIN,OUTF(1)%XMAX,OUTF(1)%YMIN,OUTF(1)%YMAX,OUTF(1)%DX,OUTF(1)%NCOL,OUTF(1)%NROW)
 CALL UTL_IDFSNAPTOGRID_LLC(OUTF(1)%XMIN,OUTF(1)%XMAX,OUTF(1)%YMIN,OUTF(1)%YMAX,OUTF(1)%DX,OUTF(1)%NCOL,OUTF(1)%NROW)

 IF(OUTF(1)%NCOL.LE.0.OR.OUTF(1)%NROW.LE.0)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Scaling is to big, yields no grid!','Warning')
  IF(IBATCH.EQ.1)WRITE(*,*) 'Scaling is to big, yields no grid!'
  RETURN
 ENDIF

 !## change nodata waarde in -999.99 for scaling 3d simulation
 IF(SCLTYPE_UP.EQ.12)THEN
  OUTF(1)%NODATA=-999.99
 ENDIF

 !## copy dimensions to other idf files
 DO I=2,SIZE(OUTF); CALL IDFCOPY(OUTF(1),OUTF(I)); ENDDO

 !## check whether map within current zoom-window
 IF((MPW%XMIN.LT.OUTF(1)%XMAX.AND.MPW%XMAX.GT.OUTF(1)%XMIN.AND.  &
     MPW%YMIN.LT.OUTF(1)%YMAX.AND.MPW%YMAX.GT.OUTF(1)%YMIN).OR.   &
     IIEXT.EQ.2)THEN

  !## should be able to store entire results
  DO I=1,SIZE(OUTF)
   IF(.NOT.IDFALLOCATEX(OUTF(I)))THEN
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot allocate enough memory','Warning')
    IF(IBATCH.EQ.1)WRITE(*,'(/1X,A,2I10/)') 'iMOD cannot allocate enough memory ',OUTF(1)%NCOL,OUTF(1)%NROW
    RETURN
   ENDIF
  ENDDO

   !## do the actual scaling
   SELECT CASE (SCLTYPE_UP)
    CASE (11:14)
     LSCALE=MATH1SCALECOMP(IBATCH)
    CASE DEFAULT
     LSCALE=IDFREADSCALE(MATH(1)%FNAME,OUTF(1),SCLTYPE_UP,SCLTYPE_DOWN,SFCT,0)
   END SELECT
!   IF(SCLTYPE_UP.GE.11)LSCALE=MATH1SCALECOMP(IBATCH)
!   IF(SCLTYPE_UP.LT.11)LSCALE=IDFREADSCALE(MATH(1)%FNAME,OUTF(1),SCLTYPE_UP,SCLTYPE_DOWN,SFCT,0)
   
   IF(LSCALE)THEN

   DO I=1,SIZE(OUTF) !3,6,3 !SIZE(OUTF)
    IF(TRIM(OUTNAMES(I)).NE.'')THEN
     SCLTXT=''
     IF(SCLTYPE_UP.GT.0)SCLTXT='Upscaling algorithm used: '//TRIM(MATH1GETSCALETXT(1,SCLTYPE_UP))//NEWLINE
     IF(SCLTYPE_DOWN.GT.0)SCLTXT=TRIM(SCLTXT)//'Downscaling algorithm used: '//TRIM(MATH1GETSCALETXT(-1,SCLTYPE_DOWN))//NEWLINE
     IF(SIZE(OUTF).EQ.1)THEN
      CALL IDFFILLCOMMENT(OUTF(I),'Units: Unknown'//NEWLINE//TRIM(SCLTXT)//'Source: '//TRIM(IDFNAMES(1)))
     ELSE
      CALL IDFFILLCOMMENT(OUTF(I),'Units: Unknown'//NEWLINE//TRIM(SCLTXT)//'Source: '//TRIM(ROOT)//'\'//TRIM(WC))    
     ENDIF
     IF(.NOT.IDFWRITE(OUTF(I),OUTNAMES(I),1))THEN
      !## error occured
      IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot write IDF '//TRIM(OUTNAMES(I)),'Error: Termination')
      IF(IBATCH.EQ.1)WRITE(*,*) 'Cannot write IDF '//TRIM(OUTNAMES(I))
      RETURN
     ENDIF
    ENDIF
   ENDDO

  ELSE
   DO I=1,SIZE(OUTF); CLOSE(OUTF(1)%IU,STATUS='DELETE'); ENDDO
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Proces terminated','Error')
   IF(IBATCH.EQ.1)WRITE(*,*) 'Proces terminated'
   RETURN
  ENDIF
 ELSE
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Selected Maps outside current window','Error: Termination')
  IF(IBATCH.EQ.1)WRITE(*,*) 'Selected Maps outside current window'
  RETURN
 ENDIF

 IF(IBATCH.EQ.0)THEN
  CALL WINDOWSELECT(0)
  CALL WINDOWOUTSTATUSBAR(4,'')
 ENDIF

 MATH1SCALE=.TRUE.

 END FUNCTION MATH1SCALE

 !###======================================================================
 SUBROUTINE MATH1SCALECLOSE(IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH

 IF(IBATCH.EQ.0)THEN
  CALL WINDOWSELECT(0)
  CALL WINDOWOUTSTATUSBAR(4,'')
 ENDIF

 !## module used outside imod
 IF(ALLOCATED(IDFNAMES))DEALLOCATE(IDFNAMES)
 IF(ALLOCATED(OUTNAMES))DEALLOCATE(OUTNAMES)
 IF(ALLOCATED(MATH))THEN
  CALL IDFDEALLOCATE(MATH,SIZE(MATH)); DEALLOCATE(MATH)
 ENDIF
 IF(ALLOCATED(OUTF))THEN
  CALL IDFDEALLOCATE(OUTF,SIZE(OUTF)); DEALLOCATE(OUTF)
 ENDIF

 IF(ALLOCATED(DELR))DEALLOCATE(DELR);   IF(ALLOCATED(DELC))DEALLOCATE(DELC)
 IF(ALLOCATED(PDELR))DEALLOCATE(PDELR); IF(ALLOCATED(PDELC))DEALLOCATE(PDELC)

 IF(ALLOCATED(P))   DEALLOCATE(P);   IF(ALLOCATED(V))   DEALLOCATE(V)
 IF(ALLOCATED(SS))  DEALLOCATE(SS);  IF(ALLOCATED(CD))  DEALLOCATE(CD)
 IF(ALLOCATED(RHS)) DEALLOCATE(RHS); IF(ALLOCATED(IB))  DEALLOCATE(IB)
 IF(ALLOCATED(CC))  DEALLOCATE(CC);  IF(ALLOCATED(CR))  DEALLOCATE(CR)
 IF(ALLOCATED(CV))  DEALLOCATE(CV);  IF(ALLOCATED(HNEW))DEALLOCATE(HNEW)
 IF(ALLOCATED(HCOF))DEALLOCATE(HCOF)
 
 IF(ALLOCATED(FREQ))DEALLOCATE(FREQ); IF(ALLOCATED(A))   DEALLOCATE(A)
 IF(ALLOCATED(B))   DEALLOCATE(B) !;    IF(ALLOCATED(L))   DEALLOCATE(L)
! IF(ALLOCATED(U))   DEALLOCATE(U)
 IF(ALLOCATED(DX))  DEALLOCATE(DX);   IF(ALLOCATED(DY))  DEALLOCATE(DY)
 IF(ALLOCATED(DZ))  DEALLOCATE(DZ)

 IF(ALLOCATED(IOS))DEALLOCATE(IOS)

 END SUBROUTINE MATH1SCALECLOSE

 !###====================================================================
 LOGICAL FUNCTION MATH1SCALECOMP(IBATCH)
 !###====================================================================
 IMPLICIT NONE
 ! 11 = darcian method (simulation)
 ! 12 = homogenization (simulation)
 ! 13 = global-local method
 ! 14 = 3d simulations
 TYPE (WIN_MESSAGE) :: MESSAGE
 INTEGER,INTENT(IN) :: IBATCH
 REAL :: RATIO
 REAL,ALLOCATABLE,DIMENSION(:) :: BVALUE,TVALUE
 INTEGER :: IROW,ICOL,ILAY,IR1,IR2,IC1,IC2,MXNCOL,MXNROW,ITYPE,IRAT,IRAT1
 INTEGER,DIMENSION(4) :: IOS

 MATH1SCALECOMP=.FALSE.

 ALLOCATE(DELC(0:OUTF(1)%NROW) ,STAT=IOS(1))
 ALLOCATE(DELR(0:OUTF(1)%NCOL) ,STAT=IOS(2))
 ALLOCATE(PDELC(2,OUTF(1)%NROW),STAT=IOS(3))
 ALLOCATE(PDELR(2,OUTF(1)%NCOL),STAT=IOS(4))
 IF(SUM(IOS).NE.0)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot allocate enough memory!','Error')
  IF(IBATCH.EQ.1)WRITE(*,*) 'Cannot allocate enough memory!'
  RETURN
 ENDIF

 !## construct delc/delr for output grid
 CALL MATH1SCALEDELRC()
 !## get pdelr/pdelc for grid#1 to be used for all grids
 CALL MATH1SCALEPDELRC(1)

 !## stores top- and bottom values for scaled parameters
 ALLOCATE(TVALUE(4),BVALUE(4))

 !## most-frequent,percentiles,darcian, homogenization, global-local, 3d-sim
 IF(SCLTYPE_UP.GE.7)THEN
  MXNCOL=0; DO ICOL=1,OUTF(1)%NCOL; MXNCOL=MAX(MXNCOL,PDELR(2,ICOL)-PDELR(1,ICOL)); END DO
  MXNCOL=MXNCOL+1
  MXNROW=0; DO IROW=1,OUTF(1)%NROW; MXNROW=MAX(MXNROW,PDELC(2,IROW)-PDELC(1,IROW)); END DO
  MXNROW=MXNROW+1

  SELECT CASE (SCLTYPE_UP)
   CASE (11,12,13)
    MXNROW=MXNROW+2
    MXNCOL=MXNCOL+2
    ALLOCATE(A(MXNROW*MXNCOL,MXNROW*MXNCOL),B(MXNROW*MXNCOL), &
!             L(MXNROW*MXNCOL,MXNROW*MXNCOL),U(MXNROW*MXNCOL,MXNROW*MXNCOL), &
             CR(MXNCOL,MXNROW,1),CC(MXNCOL,MXNROW,1),DX(MXNCOL),DY(MXNROW))
   CASE (14)
    MXNCOL=MXNCOL+2*IBUFFER
    MXNROW=MXNROW+2*IBUFFER
    !## allocate memory for 3d (pcg)-model
    IF(.NOT.MATH1_SIM_AL(MXNCOL,MXNROW,SIZE(IDFNAMES)+2))RETURN  !## nlay=idfnames+2 (top/bottom)
    !## compute thicknesses
    DZ=0.0; TZ=0.0
    DO ILAY=1,SIZE(IDFNAMES)
     DZ(ILAY+1)=MATH(ILAY)%TOP-MATH(ILAY)%BOT
     TZ(ILAY)  =MATH(ILAY)%TOP
     TZ(ILAY+1)=MATH(ILAY)%BOT
    ENDDO
  END SELECT 
 ENDIF

 IF(IBATCH.EQ.0)THEN
  CALL UTL_MESSAGEHANDLE(0)
  CALL WMESSAGEENABLE(KEYDOWN,1)
 ENDIF

 IRAT =0
 IRAT1=IRAT
 DO IROW=1,OUTF(1)%NROW

  IF(IBATCH.EQ.0)THEN
   CALL WMESSAGEPEEK(ITYPE, MESSAGE)
   IF(ITYPE.EQ.KEYDOWN.AND.MESSAGE%VALUE1.EQ.KEYESCAPE)RETURN
  ENDIF

  IR1=PDELC(1,IROW)-IBUFFER
  IR2=PDELC(2,IROW)+IBUFFER

  DO ICOL=1,OUTF(1)%NCOL
   IC1=PDELR(1,ICOL)-IBUFFER
   IC2=PDELR(2,ICOL)+IBUFFER

   CALL MATH1SCALEREADGETBLOCKVALUE(IR1,IR2,IC1,IC2,BVALUE,TVALUE)

   !## scale down for conductances only (riv,drn,ghb,scltype=5)
   IF((SCLTYPE_UP.EQ.5.OR.SCLTYPE_UP.EQ.6).AND.OUTF(1)%DX.LT.MATH(1)%DX)THEN
    RATIO=(OUTF(1)%DX**2.0)/(MATH(1)%DX**2.0)
    BVALUE(1)=BVALUE(1)/RATIO
   ENDIF
   SELECT CASE (SCLTYPE_UP)
    !## darcian simulation
    CASE (11,12,13)
     OUTF(1)%X(ICOL,IROW)=BVALUE(1)
     OUTF(2)%X(ICOL,IROW)=TVALUE(1)
    !## 3d simulation
    CASE (14)
     OUTF(1)%X(ICOL,IROW)=BVALUE(1) !## kx min
     OUTF(2)%X(ICOL,IROW)=BVALUE(2) !## ky min
     OUTF(3)%X(ICOL,IROW)=BVALUE(3) !## kz min (SUM)
     OUTF(7)%X(ICOL,IROW)=BVALUE(4) !## BOT
     OUTF(4)%X(ICOL,IROW)=TVALUE(1) !## kx max
     OUTF(5)%X(ICOL,IROW)=TVALUE(2) !## ky max
     OUTF(6)%X(ICOL,IROW)=TVALUE(3) !## kz max (SIM)
     OUTF(8)%X(ICOL,IROW)=TVALUE(4) !## TOP
    CASE DEFAULT
     OUTF(1)%X(ICOL,IROW)=BVALUE(1)
   END SELECT

  ENDDO

  IF(IBATCH.EQ.0)CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW,OUTF(1)%NROW,'Progress ')
  IF(IBATCH.EQ.1)WRITE(*,'(A,F10.2)') 'Progress ',REAL(IROW)/REAL(OUTF(1)%NROW)*100.0

 ENDDO

 !## smooth only if cs.gt.dx
 IF(SCLTYPE_DOWN.EQ.1)THEN 

  CALL MATH1SMOOTH(OUTF(1)%X,OUTF(1)%NCOL,OUTF(1)%NROW,DELR,DELC,OUTF(1)%XMIN,OUTF(1)%XMAX, &
       OUTF(1)%YMIN,OUTF(1)%YMAX,OUTF(1)%DX,MATH(1)%XMIN,MATH(1)%YMAX,  &
       MATH(1)%DX,IINT,OUTF(1)%NODATA)

 ENDIF

 MATH1SCALECOMP=.TRUE.

 IF(IBATCH.EQ.0)CALL UTL_MESSAGEHANDLE(1)

 END FUNCTION MATH1SCALECOMP

 !###====================================================================
 SUBROUTINE MATH1SCALEPDELRC(IGRID)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IGRID
 REAL :: DX,DY
 INTEGER :: I

 IF(MATH(IGRID)%IEQ.EQ.0)THEN

  !start/end column direction
  DO I=1,OUTF(1)%NCOL
   DX=(DELR(I-1)-MATH(IGRID)%XMIN)/MATH(IGRID)%DX
   PDELR(1,I)=INT(DX)+1  !van
   DX=(DELR(I)-MATH(IGRID)%XMIN)/MATH(IGRID)%DX
   PDELR(2,I)=INT(DX)+1  !tot/met
   DX=DELR(I)-MATH(IGRID)%XMIN
   IF(MOD(DX,MATH(IGRID)%DX).EQ.0.0)PDELR(2,I)=PDELR(2,I)-1
  ENDDO
  DO I=1,OUTF(1)%NROW
   DY=(MATH(IGRID)%YMAX-DELC(I-1))/MATH(IGRID)%DY
   PDELC(1,I)=INT(DY)+1
   DY=(MATH(IGRID)%YMAX-DELC(I))/MATH(IGRID)%DY
   PDELC(2,I)=INT(DY)+1
   DY=MATH(IGRID)%YMAX-DELC(I)
   IF(MOD(DY,MATH(IGRID)%DY).EQ.0.0)PDELC(2,I)=PDELC(2,I)-1
  ENDDO

 ELSEIF(MATH(IGRID)%IEQ.EQ.1)THEN

  !## start/end column direction
  DO I=1,OUTF(1)%NCOL
   CALL POL1LOCATE(MATH(IGRID)%SX,MATH(IGRID)%NCOL+1,REAL(DELR(I-1),8),PDELR(1,I))
   PDELR(1,I)=PDELR(1,I)+1
   CALL POL1LOCATE(MATH(IGRID)%SX,MATH(IGRID)%NCOL+1,REAL(DELR(I),8),PDELR(2,I))
  ENDDO
  DO I=1,OUTF(1)%NROW
   CALL POL1LOCATE(MATH(IGRID)%SY,MATH(IGRID)%NROW+1,REAL(DELC(I-1),8),PDELC(1,I))
   CALL POL1LOCATE(MATH(IGRID)%SY,MATH(IGRID)%NROW+1,REAL(DELC(I),8),PDELC(2,I))
   PDELC(2,I)=PDELC(2,I)-1
  ENDDO

 ENDIF

 END SUBROUTINE MATH1SCALEPDELRC

 !###====================================================================
 SUBROUTINE MATH1SCALEDELRC()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I

 DELR   =OUTF(1)%DX; DELC=OUTF(1)%DY
 DELR(0)=OUTF(1)%XMIN; DO I=1,OUTF(1)%NCOL; DELR(I)=DELR(I-1)+DELR(I); END DO
 DELC(0)=OUTF(1)%YMAX; DO I=1,OUTF(1)%NROW; DELC(I)=DELC(I-1)-DELC(I); END DO

 END SUBROUTINE MATH1SCALEDELRC

 !###====================================================================
 SUBROUTINE MATH1SCALEREADGETBLOCKVALUE(IR1,IR2,IC1,IC2,BVALUE,TVALUE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IR1,IR2,IC1,IC2
 REAL,DIMENSION(:),INTENT(OUT) :: BVALUE,TVALUE
 INTEGER :: I,IROW,ICOL,NAJ,IR,IC,IOFFSET,NROW,NCOL,NLAY,J,ITOP,IBOT
 REAL :: IDFVAL,NVALUE,XTOP,XBOT,X,Y,ZMID
 REAL,DIMENSION(1) :: XTEMP
 
 BVALUE =0.0 !## bot value
 TVALUE =0.0 !## top value
 NVALUE =0.0
 IOFFSET=0   !## darcian simulation uses border-ring
 SELECT CASE (SCLTYPE_UP)
  CASE (11,12,13)
   IOFFSET=1
  CASE (14)
   CC=0.0
   CR=0.0
   CV=0.0
   IB=0  !## initialize boundary window
 END SELECT

 IR=IOFFSET
 DO IROW=MAX(1,IR1),MIN(MATH(1)%NROW,IR2)
  IR=IR+1
  IC=IOFFSET
  DO ICOL=MAX(1,IC1),MIN(MATH(1)%NCOL,IC2)
   IC=IC+1

   IDFVAL=IDFGETVAL(MATH(1),IROW,ICOL)
   SELECT CASE (SCLTYPE_UP)
    !## darcian simulation
    CASE (11,12,13)
     A(IC,IR)=IDFVAL
     DX(IC)  =MATH(1)%DX 
     DY(IR)  =MATH(1)%DY 
     NVALUE  =NVALUE+1.0
    !## 3d simulation
    CASE (14)
     DO I=1,SIZE(MATH)
      IDFVAL=IDFGETVAL(MATH(I),IROW,ICOL)
      IF(IDFVAL.NE.MATH(I)%NODATA)THEN
       CC(IC,IR,1+I)=IDFVAL
       IB(IC,IR,1+I)=1
       NVALUE=NVALUE+1.0
      ENDIF
     ENDDO
     !## ib nul outside trimidf files
     IF(ITRIM(1).EQ.1.OR.ITRIM(2).EQ.1)THEN
      CALL IDFGETLOC(MATH(1),IROW,ICOL,X,Y)
      !## trim top
      IF(ITRIM(1).EQ.1)THEN
       XTOP=IDFGETXYVAL(TRIMIDF(1),X,Y)
       IF(XTOP.EQ.TRIMIDF(1)%NODATA)THEN
        DO I=1,SIZE(MATH); IB(IC,IR,1+I)=0; ENDDO
       ELSE
        DO I=1,SIZE(MATH)
         ZMID=(MATH(I)%TOP+MATH(I)%BOT)/2.0
         IF(ZMID.GT.XTOP)IB(IC,IR,1+I)=0
        ENDDO
       ENDIF
      ENDIF
      IF(ITRIM(2).EQ.1)THEN
       XBOT=IDFGETXYVAL(TRIMIDF(2),X,Y)
       IF(XBOT.EQ.TRIMIDF(2)%NODATA)THEN
        DO I=1,SIZE(MATH); IB(IC,IR,1+I)=0; ENDDO
       ELSE
        DO I=1,SIZE(MATH)
         ZMID=(MATH(I)%TOP+MATH(I)%BOT)/2.0
         IF(ZMID.LT.XBOT)IB(IC,IR,1+I)=0
        ENDDO
       ENDIF
      ENDIF          
     ENDIF

   END SELECT
  ENDDO
 ENDDO

 IF(SCLTYPE_UP.EQ.14)THEN
  !## get scaled cc for model area (i-1,i,i+1)   
  ITOP=0; DO I=1,SIZE(MATH); IF(IB(IC,IR,1+I).EQ.1)THEN
  IF(MATH1GETK_CUBE(I+1,NROW,NCOL))THEN; XTOP=MATH(I)%TOP; ITOP=I; EXIT; ENDIF
  ENDIF; ENDDO
  IBOT=0; DO I=SIZE(MATH),1,-1; IF(IB(IC,IR,1+I).EQ.1)THEN
  IF(MATH1GETK_CUBE(I+1,NROW,NCOL))THEN; XBOT=MATH(I)%BOT; IBOT=I; EXIT; ENDIF
  ENDIF; ENDDO
  IF(ITOP.EQ.0.AND.IBOT.EQ.0)THEN
   XTOP=MATH(1)%NODATA; XBOT=MATH(1)%NODATA
  ELSE
   IF(ITOP.GT.IBOT)THEN
    DO I=SIZE(MATH),1,-1; IF(IB(IC,IR,1+I).EQ.1)THEN; XBOT=MATH(I)%BOT; EXIT; ENDIF
    ENDDO; XTOP=XBOT
   ENDIF
  ENDIF
  TVALUE(4)=XTOP; BVALUE(4)=XBOT
 ENDIF
 
 IF(NVALUE.LE.0.0)THEN
  BVALUE(1)=MATH(1)%NODATA
  RETURN
 ENDIF

 SELECT CASE (SCLTYPE_UP)
  CASE (11,12)
    NROW=MIN(MATH(1)%NROW,IR2)-MAX(1,IR1)+3 !## inclusive bordercell
    NCOL=MIN(MATH(1)%NCOL,IC2)-MAX(1,IC1)+3 !## inclusive bordercell
   CALL MATH1DARCIAN(BVALUE(1),TVALUE(1),NCOL,NROW)
  CASE (13)
!   BVALUE(1)=MATH1GLOBALLOCAL()
  CASE (14)
   IF(SUM(IB).NE.0)THEN
    !## start simulation
    NCOL=SIZE(IB,1)  !## inclusive bordercell
    NROW=SIZE(IB,2)  !## inclusive bordercell
    NLAY=SIZE(IB,3)
    CALL MATH1_SIM(BVALUE,TVALUE,NCOL,NROW,NLAY) 
   ELSE
    BVALUE(1)=MATH(1)%NODATA
    TVALUE(1)=MATH(1)%NODATA
   ENDIF
 END SELECT

 END SUBROUTINE MATH1SCALEREADGETBLOCKVALUE

 !###====================================================================
 LOGICAL FUNCTION MATH1GETK_CUBE(IM,NROW,NCOL)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IM,NROW,NCOL
 INTEGER :: I,N,IROW,ICOL
 REAL :: C
 
 C=0.0; N=0
 DO IROW=1,SIZE(IB,2)
  DO ICOL=1,SIZE(IB,1)

   !## skip mid cells that have values above kmin
   IF(IB(ICOL,IROW,IM).GT.0.AND.CC(ICOL,IROW,IM).GT.KMIN)CYCLE

   DO I=IM-ILGROUP,IM+ILGROUP
    IF(IB(ICOL,IROW,I).GT.0)THEN
     C=C+CC(ICOL,IROW,I)
     N=N+1
    ENDIF
   ENDDO

  ENDDO
 ENDDO

 MATH1GETK_CUBE=.FALSE.
 IF(N.GT.0)THEN; IF(C/REAL(N).LT.KMIN)MATH1GETK_CUBE=.TRUE.; ENDIF
 
 END FUNCTION MATH1GETK_CUBE

 !###====================================================================
 SUBROUTINE MATH1_SIM(BVALUE,TVALUE,NCOL,NROW,NLAY)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW,NLAY
 REAL,INTENT(OUT),DIMENSION(:) :: BVALUE,TVALUE
 INTEGER :: ICNVG,ITER1,ITER2,IROW,ICOL,ILAY,NICNVG 
 REAL :: CVT,CT,DXY,NCV,HCHG,HCHGOLD,S,RCHG
 INTEGER :: MICNVG=25
 
 CALL MATH1_SIM_COND(NCOL,NROW,NLAY)
 
 NICNVG=0; RELAX=0.98
 DO ITER1=1,MXITER1
  RHS =0.0
  !## add well
  IF(QRATE.NE.0.0)THEN
   ICOL=(NCOL+1)/2
   IROW=(NROW+1)/2
   RHS(ICOL,IROW,NLAY)=QRATE
  ENDIF
  HCOF=0.0
  CALL PCG2AP(NROW*NCOL*NLAY,NROW,NCOL,NLAY,IB,CR,CC,CV,HCOF,RHS,V,SS,P, &
              CD,HNEW,MXITER1,MXITER2,ITER1,ITER2,ICNVG,HCLOSE,RCLOSE,-2,NICNVG,RELAX,HCHG,RCHG)
  !## convergence achieved
  IF(ICNVG.EQ.1)EXIT
  IF(NICNVG.GT.MICNVG)EXIT

  IF(IDAMPING.EQ.1)THEN
   IF(ITER1.EQ.1)HCHGOLD=HCHG
   S=HCHG/(RELAX*HCHGOLD)
   IF(S.LT.-1.0)THEN
    RELAX=1.0/(2.0*ABS(S))
   ELSE
    RELAX=(3.0+S)/(3.0+ABS(S))
   ENDIF
   HCHGOLD=HCHG
  ENDIF

 ENDDO
 
 CALL MATH1_SIM_POSTPROC(NCOL,NROW,NLAY,TVALUE,BVALUE)

 !## compute arithmetic values for resistance
 CT=0.0; NCV=0.0
 DO IROW=1,NROW
  DO ICOL=1,NCOL
   CVT=0.0
   DO ILAY=1,NLAY-1
!    !## skip outside top/bot
!    IF(TZ(ILAY).GT.TVALUE(4).OR.TZ(ILAY).LT.BVALUE(4))CYCLE
    !## skip nodata
    IF(CV(ICOL,IROW,ILAY).LE.0.0)CYCLE
    !# sum resistances
    CVT=CVT+1.0/(CV(ICOL,IROW,ILAY)/(MATH(1)%DX*MATH(1)%DY))
   ENDDO
   !## add as conductances
   IF(CVT.GT.0.0)THEN; CT=CT+1.0/CVT; NCV=NCV+1.0; ENDIF
  ENDDO
 ENDDO
 CT=1.0/CT/NCV

 !## copy values from simulation, since we do not scale for transmissivites
 BVALUE(1:2)=TVALUE(1:2)

 !## total resistance (days)
 BVALUE(3)=CT
 
 END SUBROUTINE MATH1_SIM

 !###====================================================================
 SUBROUTINE MATH1_SIM_POSTPROC(NCOL,NROW,NLAY,TVALUE,BVALUE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW,NLAY
 REAL,DIMENSION(:),INTENT(INOUT) :: TVALUE,BVALUE
 INTEGER :: IROW,ICOL,ILAY,I
 DOUBLE PRECISION,DIMENSION(3) :: TQ,DH
 DOUBLE PRECISION :: D,VCZ
 REAL :: KDX,KDY,TC,NH,SQ,DP,A,KV,SZ
 
 !## total thickness of volume
 SZ=SUM(DZ) 
 SZ=TVALUE(4)-BVALUE(4)
 !## area
 A=(REAL(NCOL)*MATH(1)%DX*REAL(NROW)*MATH(1)%DY)

 KDX=0.0; KDY=0.0
 DO ILAY=1,NLAY-1 !## nlay-1 cause last layer is thickness zero or extraction aquifer
  !## skip outside top/bot
  IF(TZ(ILAY).GT.TVALUE(4).OR.TZ(ILAY).LT.BVALUE(4))CYCLE

  TQ=0.0
  DH=0.0
  DO IROW=1,NROW
   DO ICOL=1,NCOL
    IF(IB(ICOL,IROW,ILAY).NE.0)THEN
     IF(ICOL.LT.NCOL)THEN
      IF(IB(ICOL+1,IROW,ILAY).NE.0)THEN
       D=ABS(HNEW(ICOL+1,IROW,ILAY)-HNEW(ICOL,IROW,ILAY))
       DH(1)=DH(1)+D
       TQ(1)=TQ(1)+(CR(ICOL,IROW,ILAY)*D)
      ENDIF
     ENDIF
     IF(IROW.LT.NROW)THEN
      IF(IB(ICOL,IROW+1,ILAY).NE.0)THEN
       D=ABS(HNEW(ICOL,IROW+1,ILAY)-HNEW(ICOL,IROW,ILAY))
       DH(2)=DH(2)+D
       TQ(2)=TQ(2)+(CC(ICOL,IROW,ILAY)*D)
      ENDIF
     ENDIF
    ENDIF
   ENDDO
  ENDDO
  !## mean conductances (horizontal)
  IF(DH(1).NE.0.0)KDX=KDX+TQ(1)/DH(1)  !## x-dir
  IF(DH(2).NE.0.0)KDY=KDY+TQ(2)/DH(2)  !## y-dir
 ENDDO

 !## transform transmissivities into k-values for x- and y-direction
 TVALUE(1)=KDX/SZ 
 TVALUE(2)=KDY/SZ 

 ILAY=NLAY

 !## get total q over last
 TQ(3)=0.0
 !## get mean head
 DH(3)=0.0
 NH   =0.0
 DO IROW=1,NROW
  DO ICOL=1,NCOL
   IF(IB(ICOL,IROW,ILAY-1).NE.0.AND.IB(ICOL,IROW,ILAY).NE.0)THEN
    D=ABS(HNEW(ICOL,IROW,ILAY)-HNEW(ICOL,IROW,ILAY-1))
    DH(3)= DH(3)+HNEW(ICOL,IROW,ILAY) 
    TQ(3)= TQ(3)+(CV(ICOL,IROW,ILAY-1)*D)
    NH   = NH+1.0
   ENDIF
  ENDDO
 ENDDO
 
 !## dz used --- m3/day -> m2/day
 IF(QRATE.EQ.0.0)THEN
  SQ=TQ(3)
  DP=DHZ
 !## qrate used
 ELSE
  SQ=ABS(QRATE)
  DP=DH(3)/NH
 ENDIF

 KV=SQ/((DP/SZ)*A)
 TC=SZ/KV
 
 !## storage resistance
 TVALUE(3)=TC

 END SUBROUTINE MATH1_SIM_POSTPROC

 !###====================================================================
 SUBROUTINE MATH1_SIM_COND(NCOL,NROW,NLAY)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW,NLAY
 INTEGER :: IROW,ICOL,ILAY,JLAY
 REAL :: T1,T2,DX,DY,D,C1,C2,H1,H2,DH

 !## fill in maximal k-value --- if not fully absent
 DO IROW=1,NROW
  DO ICOL=1,NCOL
   IF(SUM(IB(ICOL,IROW,1:NLAY)).NE.0)THEN
    DO ILAY=1,NLAY
     IF(IB(ICOL,IROW,ILAY).EQ.0)THEN
      IF(ILAY.EQ.1.OR.ILAY.EQ.NLAY)THEN
       IB(ICOL,IROW,ILAY)=-1
      ELSE
       IB(ICOL,IROW,ILAY)= 1
      ENDIF
      CC(ICOL,IROW,ILAY)=MAXK
     ENDIF
    ENDDO
   ELSE
    !## fully absent, fill in with high permeable material
    IF(ISURFWATER.EQ.1)THEN
     IB(ICOL,IROW,1:NLAY)=1
     CC(ICOL,IROW,1:NLAY)=MAXK
    ENDIF
   ENDIF
  ENDDO
 ENDDO

 HNEW=0.0
 HNEW(:,:,NLAY)=-DHZ

 CC(:,:,1)=CC(:,:,2)          !## first layer (artificial) equal to second      layer
 IF(QRATE.EQ.0.0)THEN
  CC(:,:,NLAY)=CC(:,:,NLAY-1) !## last layer  (artificial) equal to second-last layer
 ELSE
  CC(:,:,NLAY)=AQFR_KD        !## last layer  (artificial) equal to second-last layer
  IB(:,:,NLAY)=1              !## active again
  DZ(NLAY)=1.0
 ENDIF

 DO ILAY=1,NLAY
  DO IROW=1,NROW
   DY=MATH(1)%DY
   DO ICOL=1,NCOL
    DX=MATH(1)%DX

    !## compute vertical conductance
    IF(ILAY.LT.NLAY)THEN
     IF(IB(ICOL,IROW,ILAY).NE.0.AND.IB(ICOL,IROW,ILAY+1).NE.0)THEN
      C1=0.0; C2=0.0
      IF(CC(ICOL,IROW,ILAY).NE.0.0)  C1=(0.5*DZ(ILAY  ))/(CC(ICOL,IROW,ILAY)  *VER_FCT)
      IF(CC(ICOL,IROW,ILAY+1).NE.0.0)C2=(0.5*DZ(ILAY+1))/(CC(ICOL,IROW,ILAY+1)*VER_FCT)
      IF(C1+C2.NE.0.0)CV(ICOL,IROW,ILAY)=(1.0/(C1+C2))*(DX*DY)
     ENDIF
    ENDIF

    !## compute horizontal conductances
    T1=CC(ICOL,IROW,ILAY)*DZ(ILAY)*HOR_FCT
    IF(T1.EQ.0.0)THEN
     CR(ICOL,IROW,ILAY)=0.0
    ELSE
     IF(ICOL.NE.NCOL)THEN
      D =MATH(1)%DX
      T2=CC(ICOL+1,IROW,ILAY)*DZ(ILAY)*HOR_FCT
      CR(ICOL,IROW,ILAY)=2.0*T2*T1*DY/(T1*D+T2*DX)
     ENDIF
     IF(IROW.NE.NROW)THEN
      D =MATH(1)%DY
      T2=CC(ICOL,IROW+1,ILAY)*DZ(ILAY)*HOR_FCT
      CC(ICOL,IROW,ILAY)=2.0*T2*T1*DX/(T1*D+T2*DY)
     ENDIF
    ENDIF

   ENDDO
  ENDDO

  CR(NCOL,1:NROW,ILAY)=0.0 !## conductance over the east   of the model
  CC(1:NCOL,NROW,ILAY)=0.0 !## conductance over the bottom of the model

 ENDDO

 END SUBROUTINE MATH1_SIM_COND

 !###====================================================================
 LOGICAL FUNCTION MATH1_SIM_AL(NCOL,NROW,NLAY)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW,NLAY

 MATH1_SIM_AL=.FALSE.

 ALLOCATE(IOS(13)) 

 ALLOCATE(P(NCOL,NROW,NLAY)   ,STAT=IOS(1))
 ALLOCATE(V(NCOL,NROW,NLAY)   ,STAT=IOS(2))
 ALLOCATE(SS(NCOL,NROW,NLAY)  ,STAT=IOS(3))
 ALLOCATE(CD(NCOL,NROW,NLAY)  ,STAT=IOS(4))
 ALLOCATE(RHS(NCOL,NROW,NLAY) ,STAT=IOS(5))
 ALLOCATE(IB(NCOL,NROW,NLAY)  ,STAT=IOS(6))
 ALLOCATE(CC(NCOL,NROW,NLAY)  ,STAT=IOS(7))
 ALLOCATE(CR(NCOL,NROW,NLAY)  ,STAT=IOS(8))
 ALLOCATE(HNEW(NCOL,NROW,NLAY),STAT=IOS(9))
 ALLOCATE(HCOF(NCOL,NROW,NLAY),STAT=IOS(10))
 ALLOCATE(CV(NCOL,NROW,NLAY-1),STAT=IOS(11))
 ALLOCATE(DZ(NLAY)            ,STAT=IOS(12))
 ALLOCATE(TZ(NLAY)            ,STAT=IOS(13))

 IF(SUM(IOS).NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD Cannot allocate enough memory to solve this Problem!','Error')
  RETURN
 ENDIF

 MATH1_SIM_AL=.TRUE.

 END FUNCTION MATH1_SIM_AL

 !###====================================================================
 SUBROUTINE MATH1DARCIAN(BVALUE,TVALUE,NCOL,NROW)
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(OUT) :: BVALUE,TVALUE
 INTEGER,INTENT(IN) :: NCOL,NROW
 INTEGER :: I,K,N
 REAL,DIMENSION(2) :: XV,YV

 !## no scaling
 IF(NROW.EQ.3.AND.NCOL.EQ.3)THEN
  BVALUE=A(2,2)
  TVALUE=BVALUE
  RETURN
 ENDIF

 !## compute cc/cr terms and store them in a matrix
 CALL MATH1CCCR(MATH(1)%NODATA,NCOL,NROW)

 !## loop over direction of impulses
 DO I=1,2
  !## fill system a matrix
  CALL MATH1FILLA(NCOL,NROW)

  !## add extra boundary conditions for periodicity
  IF(SCLTYPE_UP.EQ.9)THEN
   !## fill right terms
   IF(I.EQ.1)CALL MATH1FILLB(NCOL,NROW,N=(/1.0,1.0/),S=(/0.0,0.0/))  !## gradient in x-direction
   IF(I.EQ.2)CALL MATH1FILLB(NCOL,NROW,W=(/1.0,1.0/),E=(/0.0,0.0/))  !## gradient in y-direction
  ELSEIF(SCLTYPE_UP.EQ.10)THEN
   CALL MATH1PERIODIC(I,NCOL,NROW)
  ENDIF

  N=NROW*NCOL
  CALL LUDCMP(A(1:N,1:N),B(1:N),N) !L(1:N,1:N),U(1:N,1:N),N) 
  CALL MATH1AVERAGE(XV(I),YV(I),NCOL,NROW) !## xdir;ydir
 ENDDO

 !## get minimal and maximal values
 BVALUE= 10.0E10
 TVALUE=-10.0E10
 K     =0
 DO I=1,2
  IF(XV(I).GE.0.0)THEN
   K     =1
   BVALUE=MIN(BVALUE,XV(I))
   TVALUE=MAX(TVALUE,XV(I))
  ENDIF
  IF(YV(I).GE.0.0)THEN
   K     =1
   BVALUE=MIN(BVALUE,YV(I))
   TVALUE=MAX(TVALUE,YV(I))
  ENDIF
 ENDDO
 BVALUE=REAL(K)*BVALUE
 TVALUE=REAL(K)*TVALUE

 END SUBROUTINE MATH1DARCIAN

 !###====================================================================
 REAL FUNCTION MATH1GLOBALLOCAL()
 !###====================================================================
 IMPLICIT NONE
 
 MATH1GLOBALLOCAL=.FALSE.
 MATH1GLOBALLOCAL=.TRUE.
 
 END FUNCTION MATH1GLOBALLOCAL

 !###====================================================================
 SUBROUTINE MATH1AVERAGE(XV,YV,NCOL,NROW)
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(INOUT) :: XV,YV
 INTEGER,INTENT(IN) :: NCOL,NROW
 INTEGER :: IROW,ICOL,I
 REAL :: DX,DY,QX,QY,TX,TY

 !## copy results to x-array
 I=0
 DO IROW=1,NROW
  DO ICOL=1,NCOL
   I=I+1
   A(ICOL,IROW)=B(I)
  ENDDO
 ENDDO

 !## compute fluxes/diff.heads
 TX=0.0
 TY=0.0
 QX=0.0
 QY=0.0
 DO IROW=1,NROW-1
  DO ICOL=1,NCOL-1
   IF(ICOL.LT.NCOL-1)THEN
    DX=ABS(A(ICOL+1,IROW)-A(ICOL,IROW))
    QX=QX+CR(ICOL,IROW,1)*DX
    TX=TX+DX
   ENDIF
   IF(IROW.LT.NROW-1)THEN
    DY=ABS(A(ICOL,IROW+1)-A(ICOL,IROW))
    QY=QY+CC(ICOL,IROW,1)*DY
    TY=TY+DY
   ENDIF
  ENDDO
 ENDDO

 !## nodata value, in case no gradient in x- and/or y-direction
 XV=-1
 YV=-1

 IF(TX.NE.0.0)XV=QX/TX
 IF(TY.NE.0.0)YV=QY/TY

 END SUBROUTINE MATH1AVERAGE

 !###====================================================================
 SUBROUTINE MATH1CCCR(NODATA,NCOL,NROW)
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: NODATA
 INTEGER,INTENT(IN) :: NCOL,NROW
 INTEGER :: I,IROW,ICOL
 REAL :: T1,T2

 !## borders thickness of zero
 DX(1)   =0.0
 DX(NCOL)=0.0
 DY(1)   =0.0
 DY(NROW)=0.0
 DO I=2,NCOL-1; A(I,1)   =A(I,2)     ; ENDDO
 DO I=2,NCOL-1; A(I,NROW)=A(I,NROW-1); ENDDO
 DO I=2,NROW-1; A(1,I)   =A(2,I)     ; ENDDO
 DO I=2,NROW-1; A(NCOL,I)=A(NCOL-1,I); ENDDO
 A(1,1)      =A(2,2)
 A(NCOL,1)   =A(NCOL-1,2)
 A(NCOL,NROW)=A(NCOL-1,NROW-1)
 A(1,NROW)   =A(2,NROW-1)

 DO IROW=1,NROW
  DO ICOL=1,NCOL
   T1=A(ICOL,IROW)
   IF(T1.EQ.0.0)THEN
    CR(ICOL,IROW,1)=0.0
   ELSE
    IF(ICOL.NE.NCOL)THEN
     T2=A(ICOL+1,IROW)
     CR(ICOL,IROW,1)=2.0*T2*T1*DY(IROW)/(T1*DX(ICOL+1)+T2*DX(ICOL))
    ENDIF
    IF(IROW.NE.NROW)THEN
     T2=A(ICOL,IROW+1)
     CC(ICOL,IROW,1)=2.0*T2*T1*DX(ICOL)/(T1*DY(IROW+1)+T2*DY(IROW))
    ENDIF
   ENDIF
  ENDDO
 ENDDO

 !## adjust boundary cc/cr conditions for nodata in x(.)
 DO IROW=1,NROW-1
  DO ICOL=1,NCOL-1
   IF(A(ICOL,IROW).EQ.NODATA)  CR(ICOL,IROW,1)=0.0
   IF(A(ICOL+1,IROW).EQ.NODATA)CR(ICOL,IROW,1)=0.0
   IF(A(ICOL,IROW).EQ.NODATA)  CC(ICOL,IROW,1)=0.0
   IF(A(ICOL,IROW+1).EQ.NODATA)CC(ICOL,IROW,1)=0.0
  ENDDO
 ENDDO

 CR(NCOL,1:NROW,1)=0.0 !## conductance over the  east of the model
 CC(NCOL,1:NROW,1)=0.0 !## conductance along the east edge
 CC(1   ,1:NROW,1)=0.0 !## conductance along the west edge

 CC(1:NCOL,NROW,1)=0.0 !## conductance over the  bottom of the model
 CR(1:NCOL,NROW,1)=0.0 !## conductance along the bottom edge
 CR(1:NCOL,   1,1)=0.0 !## conductance along the top    edge

 END SUBROUTINE MATH1CCCR

 !###====================================================================
 SUBROUTINE MATH1PERIODIC(IDIR,NCOL,NROW)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDIR,NCOL,NROW
 INTEGER :: IROW,ICOL,I,II,J,JJ

 !## initialize boundary conditions
 B=0.0

 !## set corners (dummies)
 A(1,1)                            =1.0
 A(NCOL,NCOL)                      =1.0
 A(NCOL*NROW,NCOL*NROW)            =1.0
 A((NROW-1)*NCOL+1,(NROW-1)*NCOL+1)=1.0

 !## west/east relationship
 DO IROW=2,NROW-1
  I =(IROW-1)*NCOL+1
  II= I+1
  J =(IROW-1)*NCOL+NCOL
  JJ= J-1
  A(I,:) = 0.0
  A(I,I) =-CR(1,IROW,1)
  A(I,II)= CR(1,IROW,1)
  A(I,J) =-CR(NCOL-1,IROW,1)
  A(I,JJ)= CR(NCOL-1,IROW,1)
  B(I)   = 0.0
  !## right-side - head definition
  A(J,:) = 0.0
  A(J,J) =-1.0
  A(J,I) = 1.0
  IF(IDIR.EQ.2)B(J)=1.0
 ENDDO

 !## top/bottom relationship
 DO ICOL=2,NCOL-1
  I = ICOL
  II= I+NCOL
  J =(NROW-1)*NCOL+ICOL
  JJ= J-NCOL
  A(I,:) = 0.0
  A(I,I) =-CC(ICOL,1,1)
  A(I,II)= CC(ICOL,1,1)
  A(I,J) =-CC(ICOL,NROW-1,1)
  A(I,JJ)= CC(ICOL,NROW-1,1)
  B(I)   = 0.0
  !## bottom-side - head definition
  A(J,:) = 0.0
  A(J,J) =-1.0
  A(J,I) = 1.0
  IF(IDIR.EQ.1)B(J)= 1.0
 ENDDO

 !## put one location on the boundary to fixate solution
 A(2,1:NROW*NCOL)=0.0
 A(2,2)          =1.0
 B(2)            =0.0

 END SUBROUTINE MATH1PERIODIC

 !###====================================================================
 SUBROUTINE MATH1FILLA(NCOL,NROW)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW
 INTEGER :: IROW,ICOL,I

 !## clean a
 A=0.0

 I=0
 DO IROW=1,NROW
  DO ICOL=1,NCOL
   I=I+1
   IF(ICOL.GT.1)THEN
    A(I,I-1)=CR(ICOL-1,IROW,1)
    A(I,I)  =A(I,I)-CR(ICOL-1,IROW,1)
   ENDIF
   IF(IROW.GT.1)THEN
    A(I,I-NCOL)=CC(ICOL,IROW-1,1)
    A(I,I)     =A(I,I)-CC(ICOL,IROW-1,1)
   ENDIF
   IF(ICOL.LT.NCOL)THEN
    A(I,I+1)=CR(ICOL,IROW,1)
    A(I,I)  =A(I,I)-CR(ICOL,IROW,1)
   ENDIF
   IF(IROW.LT.NROW)THEN
    A(I,I+NCOL)=CC(ICOL,IROW,1)
    A(I,I)     =A(I,I)-CC(ICOL,IROW,1)
   ENDIF
  ENDDO
 ENDDO

 !## initialise identity
 IF(MAXVAL(ABS(A(I,:))).EQ.0.0)A(I,I)=1.0

 END SUBROUTINE MATH1FILLA

 !###====================================================================
 SUBROUTINE MATH1FILLB(NCOL,NROW,N,S,W,E)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW
 REAL,INTENT(IN),DIMENSION(2),OPTIONAL :: N,S,W,E
 INTEGER :: I,ICOL,IROW
 REAL :: TX,TY,G

 TX=SUM(DX(1:NCOL))
 TY=SUM(DY(1:NROW))
 B =0.0
 !## put north boundary
 IF(PRESENT(N))THEN
  G=(N(2)-N(1))/TX
  I= 0
  DO ICOL=1,NCOL
   I     =I+1
   B(I)  =N(1)+SUM(DX(1:ICOL))*G
   A(I,:)=0.0
   A(I,I)=1.0
  ENDDO
 ENDIF

 !## put west boundary
 IF(PRESENT(W))THEN
  G=(W(2)-W(1))/TY
  I=-NCOL+1
  DO IROW=1,NROW
   I     =I+NCOL
   B(I)  =W(1)+SUM(DY(1:IROW))*G
   A(I,:)=0.0
   A(I,I)=1.0
  ENDDO
 ENDIF

 !## put south boundary
 IF(PRESENT(S))THEN
  G=(S(2)-S(1))/TX
  I =(NROW*NCOL)-NCOL
  DO ICOL=1,NCOL
   I     =I+1
   B(I)  =S(1)+SUM(DX(1:ICOL))*G
   A(I,:)=0.0
   A(I,I)=1.0
  ENDDO
 ENDIF

 !## put east boundary
 IF(PRESENT(E))THEN
  G=(E(2)-E(1))/TY
  I= 0
  DO IROW=1,NROW
   I     =I+NCOL
   B(I)  =E(1)+SUM(DY(1:IROW))*G
   A(I,:)=0.0
   A(I,I)=1.0
  ENDDO
 ENDIF

 END SUBROUTINE MATH1FILLB

 !###====================================================================
 SUBROUTINE MATH1SMOOTH(X,NCOL,NROW,DELR,DELC,XMIN,XMAX, &
         YMIN,YMAX,DX,XMINIDF,YMAXIDF,DXIDF,IINT,NODATA)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW,IINT
 REAL,INTENT(IN) :: XMIN,XMAX,YMIN,XMINIDF,YMAX,YMAXIDF,DX,DXIDF,NODATA
 REAL,DIMENSION(NCOL,NROW) :: X
 REAL,DIMENSION(0:NCOL) :: DELR
 REAL,DIMENSION(0:NROW) :: DELC
 INTEGER :: IC,IR,I,IROW,ICOL,NPC,NPR
 REAL :: XC,XX,YY,YC,XCIDF,YCIDF
 REAL,ALLOCATABLE,DIMENSION(:) :: X1A,X2A
 REAL,ALLOCATABLE,DIMENSION(:,:) :: Y2A

 IF(ALLOCATED(X1A)) DEALLOCATE(X1A)
 IF(ALLOCATED(X2A)) DEALLOCATE(X2A)
 IF(ALLOCATED(Y2A)) DEALLOCATE(Y2A)

 !## find correct mid of read idf
 XX=XMIN-XMINIDF
 I =0
 IF(MOD(XX,DXIDF).NE.0.0)I=1
 ICOL =INT((XMIN-XMINIDF)/DXIDF)+I
 XCIDF=XMINIDF+REAL(ICOL*DXIDF)+(DXIDF/2.0)

 YY=YMAXIDF-YMAX
 I =0
 IF(MOD(YY,DXIDF).NE.0.0)I=1
 IROW =INT((YMAXIDF-YMAX)/DXIDF)+I
 YCIDF=YMAXIDF-REAL(IROW*DXIDF)-(DXIDF/2.0)

 !## determine amount of fixed points
 NPR=1
 YC =YCIDF+DXIDF
 DO
  YC=YC-DXIDF
  IF(YC.LT.YMIN)EXIT
  NPR=NPR+1
 ENDDO

 NPC=1
 XC =XCIDF-DXIDF
 DO
  XC=XC+DXIDF
  IF(XC.GT.XMAX)EXIT
  NPC=NPC+1
 ENDDO

 NPR=NPR+1
 NPC=NPC+1

 !## assign one extra row/column for boundary
 ALLOCATE(X1A(NPC),X2A(NPR),Y2A(NPC,NPR))

 !store mids of cell in y2a
 !NPR=1
 !YC =YCIDF+DXIDF
 NPR=0
 YC =YCIDF+2*DXIDF
 DO
  YC=YC-DXIDF
  IF(YC.LT.YMIN-DXIDF)EXIT
 ! IF(YC.LT.YMIN)EXIT
  NPR=NPR+1

  YY=YMAX-YC
  I =0
  IF(MOD(YY,DX).NE.0.0)I=1
  IR=INT(YY/DX)+I

  IR=MIN(NROW,IR)
  IR=MAX(1,IR)

 ! XC =XCIDF-DXIDF
 ! NPC=1
  XC =XCIDF-2*DXIDF
  NPC=0
  DO
   XC=XC+DXIDF
   IF(XC.GT.XMAX+DXIDF)EXIT
 !  IF(XC.GT.XMAX)EXIT
   NPC=NPC+1

   XX=XC-XMIN
   I=0
   IF(MOD(XX,DX).NE.0.0)I=1
   IC=INT(XX/DX)+I

   IC=MIN(NCOL,IC)
   IC=MAX(1,IC)

   X1A(NPC)=XC
   X2A(NPR)=YC
   Y2A(NPC,NPR)=X(IC,IR)
  END DO
 ENDDO

 !NPR=NPR+1
 !NPC=NPC+1

 X1A(1)  =MAX(XMIN,X1A(2)-DXIDF)
 X1A(NPC)=MIN(XMAX,X1A(NPC-1)+DXIDF)
 X2A(1)  =MAX(YMIN,X2A(2)+DXIDF)
 X2A(NPR)=MIN(YMAX,X2A(NPR-1)-DXIDF)

 !Y2A(1,1:NPR)  =Y2A(2,1:NPR)
 !Y2A(NPC,1:NPR)=Y2A(NPC-1,1:NPR)
 !Y2A(1:NPC,1)  =Y2A(1:NPC,2)
 !Y2A(1:NPC,NPR)=Y2A(1:NPC,NPR-1)

 !#copy
 !DO I=1,NPC
 ! WRITE(*,*) I,NPR,X1A(I)
 !END DO
 !DO I=1,NPR
 ! WRITE(*,*) I,NPC,X2A(I)
 !END DO
 !DO I=1,NPR
 ! WRITE(*,*) (Y2A(J,I),J=1,NPC)
 !END DO

 CALL POL1INTMAIN(NCOL,NROW,NPC,NPR,X1A,X2A,Y2A,DELR,DELC,X,IINT,NODATA)

 IF(ALLOCATED(X1A)) DEALLOCATE(X1A)
 IF(ALLOCATED(X2A)) DEALLOCATE(X2A)
 IF(ALLOCATED(Y2A)) DEALLOCATE(Y2A)

 END SUBROUTINE MATH1SMOOTH

 !###====================================================================
 INTEGER FUNCTION MATH1GETISCALE(STRING)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: STRING
 INTEGER :: I,J
 REAL :: X

 I=INDEXNOCASE(STRING,'(',.TRUE.)+1
 J=INDEXNOCASE(STRING,'x',.TRUE.)-1
 X=0.0; IF(I.GT.0.AND.J.GE.I)READ(STRING(I:J),*) X
 MATH1GETISCALE=0
 IF(X.LT.1.0)MATH1GETISCALE=-1 !## downscaling
 IF(X.GT.1.0)MATH1GETISCALE= 1 !## upscaling
 IF(X.EQ.0.0)MATH1GETISCALE= 1 !## upscaling (ieq=1)
 
 END FUNCTION MATH1GETISCALE

!###======================================================================
 FUNCTION MATH1GETSCALETXT(ISCALE,I)
!###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I,ISCALE
 CHARACTER(LEN=256) :: MATH1GETSCALETXT

 ! 1 = SPECIAAL (IBOUNDARY)
 ! 2 = REKENKUNDIG (SHEAD/VCONT/S)
 ! 3 = GEOMETRISCH (KD)
 ! 4 = SUM(Q)
 ! 5 = SUM(COND)*RATIO (RIV/DRN/GHB CONDUCTANCE; RCH MM/DAY)
 ! 6 = INVERSE (C)
 ! 7 = MOST FREQUENT OCCURENCE
 ! 8 = SUM (1/c)*RATIO
 ! 9 = PERCENTILE 
 !10 = BLOCKVALUE

 !## upscaling
 IF(ISCALE.EQ.1)THEN
  SELECT CASE (I)
   CASE (1)
    MATH1GETSCALETXT='Rule: minus values above positive values above zero values.'
   CASE (2)
    MATH1GETSCALETXT='Rule: sum cell values x within a coarse cell, excluding the NodataValues, and devide them by the number of cells.'
   CASE (3)
    MATH1GETSCALETXT='Rule: take log()-function for cell values within a coarse cell, excluding the NodataValues, '// &
        'devide them by the number of cells and take the exp()-function.'
   CASE (4)
    MATH1GETSCALETXT='Rule: sum cell values, excluding the NodataValues.'
   CASE (5)
    MATH1GETSCALETXT='Rule: sum cell values, times ratio, excluding the NodataValues.'
   CASE (6)
    MATH1GETSCALETXT='Rule: take the inverse (1/x) of cell values x within a coarse cell, excluding the NodataValues, and devide '//&
    'them by the number of cells.'
   CASE (7)
    MATH1GETSCALETXT='Rule: take the value x that occures mostly within a coarse cell, excluding the NodataValues.'
   CASE (8)
    MATH1GETSCALETXT='Rule: take the inverse (1/x) of cell values x within a coarse cell, excluding the NodataValues, and devide '//&
    'them by the number of cells time ratio.'
   CASE (9)
    MATH1GETSCALETXT='Rule: take the value x that occures for a given percentage within a coarse cell, excluding the NodataValues.'
   CASE (10)
    MATH1GETSCALETXT='Rule: take the block value (centre)'
   CASE (11)
    MATH1GETSCALETXT='Rule: take the value x that occures after DARCIAN simulation of fine mesh with extent of coarse cell, '//&
    'excluding the NodataValues.'
   CASE (12)
    MATH1GETSCALETXT='Rule: take the value x that occures after DARCIAN simulation with PERIODIC BOUNDARIES of fine mesh with '//&
    'extent of coarse cell, excluding the NodataValues.'
   CASE (13)
    MATH1GETSCALETXT=''
   CASE (14)
    MATH1GETSCALETXT='Rule: take the value x that occures after DARCIAN simulation with REALISTIC BOUNDARIES of fine mesh with '//&
    'extent of coarse cell, excluding the NodataValues.'
  END SELECT
 !## downscaling
 ELSEIF(ISCALE.EQ.-1)THEN
  SELECT CASE (I)
   CASE (1)
    MATH1GETSCALETXT='Rule: produces a good guess for all finer grid cells as a linear interpolation based upon the coarse '//&
    'grid cells.'
   CASE (2)
    MATH1GETSCALETXT='Rule: assign the value of the coarse grid cell to all finer grid cells.'
  END SELECT
 ELSEIF(ISCALE.EQ.0)THEN
  MATH1GETSCALETXT='Rule: no scaling performed.'
 ENDIF

 END FUNCTION MATH1GETSCALETXT

 END MODULE MOD_MATH_SCALE
