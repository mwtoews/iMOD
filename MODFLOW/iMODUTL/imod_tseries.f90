!!  Copyright (C) Stichting Deltares, 2005-2017.
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

!####============================================================
MODULE TSVAR
!####============================================================
 INTEGER, SAVE :: NREC = 0
 INTEGER, SAVE :: IIPF = 0
 REAL(KIND=8), SAVE :: FWIIPF = 1.0D0 !## multiplication for weigths
 CHARACTER(LEN=256), SAVE :: ROOTRES = ''
 INTEGER :: NST,IUIPFTXT
 REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: TSNODATA,TSM
 CHARACTER(LEN=50),DIMENSION(:),ALLOCATABLE :: ATTRIB
 CHARACTER(LEN=31),DIMENSION(2) :: CIPFTYPE
 DATA CIPFTYPE/'X,Y,Layer,Measure,Weight       ','X,Y,Layer,IDentification,Weight'/ !,'X,Y,L,Z,W,T'/
 TYPE STOBJ
  LOGICAL :: VALID = .TRUE.
  REAL(KIND=8) :: X,Y,W,M,C ! X,Y,W(EIGHT),M(EASURED),C(COMPUTED)
  INTEGER :: ILAY
  INTEGER :: ICOL = 0
  INTEGER :: IROW = 0
  CHARACTER(LEN=52) :: ID
 END TYPE STOBJ
 TYPE TSOBJ
  INTEGER :: IPFTYPE,IXCOL,IYCOL,ILCOL,IMCOL,IVCOL
  INTEGER :: NROWIPF,NCOLIPF,IUIPF,IEXT
  CHARACTER(LEN=5) :: EXT
  CHARACTER(LEN=256) :: IPFNAME
  TYPE(STOBJ),POINTER,DIMENSION(:) :: STVALUE => NULL()
  LOGICAL :: DEL = .FALSE. ! PKS
 END TYPE TSOBJ
 TYPE(TSOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: TS
 
END MODULE

!###====================================================================
SUBROUTINE TSERIE1INIT1(LPEST,LSS,MV)
!###====================================================================
USE IMOD_UTL 
USE TSVAR
IMPLICIT NONE
! arguments
LOGICAL, INTENT(IN) :: LPEST
LOGICAL, INTENT(IN) :: LSS
REAL, INTENT(IN) :: MV
! locals
INTEGER :: IOS,I,II,III,J,JJ,N
CHARACTER(LEN=52),DIMENSION(:),ALLOCATABLE :: IPFSTRING
CHARACTER(LEN=512) :: LINE
CHARACTER(LEN=1000) :: BIGLINE

IF(IIPF.EQ.0)RETURN

NREC  =0

CALL IMOD_UTL_OSSYSTEM()

DO JJ=1,ABS(IIPF)

 CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT('Reading IPF for saving TIMESERIES',0)
 CALL IMOD_UTL_PRINTTEXT(' Reading:  '//TRIM(TS(JJ)%IPFNAME),0)
 CALL IMOD_UTL_PRINTTEXT(' IPF-type: ('//TRIM(IMOD_UTL_ITOS(TS(JJ)%IPFTYPE))//') '//TRIM(CIPFTYPE(TS(JJ)%IPFTYPE)),0)

 !## writing of steady-state/transient results in ipf-file
 CALL IMOD_UTL_OPENASC(TS(JJ)%IUIPF,TS(JJ)%IPFNAME,'R')
 IF(TS(JJ)%IUIPF.LE.0)THEN
  CALL IMOD_UTL_PRINTTEXT('Can not open '//TRIM(TS(JJ)%IPFNAME),0)
  CALL IMOD_UTL_PRINTTEXT('Could it be that this files has been double defined?',2)
 ENDIF
 READ(TS(JJ)%IUIPF,'(A256)') LINE; CALL IMOD_UTL_STRING(LINE); READ(LINE,*) TS(JJ)%NROWIPF
 READ(TS(JJ)%IUIPF,'(A256)') LINE; CALL IMOD_UTL_STRING(LINE); READ(LINE,*) TS(JJ)%NCOLIPF
 IF(TS(JJ)%IMCOL.GT.TS(JJ)%NCOLIPF)TS(JJ)%IMCOL=0
 IF(TS(JJ)%IVCOL.GT.TS(JJ)%NCOLIPF)TS(JJ)%IVCOL=0
 ALLOCATE(ATTRIB(TS(JJ)%NCOLIPF))
 CALL IMOD_UTL_PRINTTEXT('',0)
 CALL IMOD_UTL_PRINTTEXT('Read NLINES:      '//TRIM(IMOD_UTL_ITOS(TS(JJ)%NROWIPF)),0)
 CALL IMOD_UTL_PRINTTEXT('Read NATTRIBUTES: '//TRIM(IMOD_UTL_ITOS(TS(JJ)%NCOLIPF)),0)
 DO I=1,TS(JJ)%NCOLIPF
  READ(TS(JJ)%IUIPF,*) ATTRIB(I)
 ENDDO
 !## read iext,ext
 READ(TS(JJ)%IUIPF,*) TS(JJ)%IEXT,TS(JJ)%EXT
 !## overrule iext for steady-state simulations
 IF(LSS)TS(JJ)%IEXT=0
 CALL IMOD_UTL_PRINTTEXT('X-Coordinate  : '//TRIM(ATTRIB(TS(JJ)%IXCOL)),0)
 CALL IMOD_UTL_PRINTTEXT('Y-Coordinate  : '//TRIM(ATTRIB(TS(JJ)%IYCOL)),0)
 CALL IMOD_UTL_PRINTTEXT('Modellayer    : '//TRIM(ATTRIB(TS(JJ)%ILCOL)),0)
 IF(TS(JJ)%IEXT.EQ.0)THEN
  IF(TS(JJ)%IMCOL.NE.0)CALL IMOD_UTL_PRINTTEXT('Measurement   : '//TRIM(ATTRIB(TS(JJ)%IMCOL)),0)
 ELSE
  CALL IMOD_UTL_PRINTTEXT('MeasurementID : '//TRIM(ATTRIB(TS(JJ)%IEXT)),0)
 ENDIF
 IF(TS(JJ)%IVCOL.NE.0)CALL IMOD_UTL_PRINTTEXT('Variance      : '//TRIM(ATTRIB(ABS(TS(JJ)%IVCOL))),0)
 DEALLOCATE(ATTRIB)

 CALL IMOD_UTL_PRINTTEXT('',0)
 IF(.NOT.LSS)THEN
  IF(TS(JJ)%IPFTYPE.EQ.2)THEN
   IF(LPEST.AND.TS(JJ)%IEXT.EQ.0)THEN
    CALL IMOD_UTL_PRINTTEXT('Error occured in IPF, syntax is at least: '//TRIM(CIPFTYPE(TS(JJ)%IPFTYPE)),0)
    CALL IMOD_UTL_PRINTTEXT('IEXT should be assigned to any column',2)
   ENDIF
  ELSE
   CALL IMOD_UTL_PRINTTEXT('Error wrong ipftype given '//TRIM(IMOD_UTL_ITOS(TS(JJ)%IPFTYPE)),2)
  ENDIF
 ENDIF
ENDDO

II=0
DO JJ=1,ABS(IIPF)
 ALLOCATE(IPFSTRING(TS(JJ)%NCOLIPF))
 III=0
 ALLOCATE(TS(JJ)%STVALUE(MAX(1,TS(JJ)%NROWIPF)))
 DO I=1,TS(JJ)%NROWIPF
  READ(TS(JJ)%IUIPF,'(A512)') LINE
  CALL IMOD_UTL_STRING(LINE)
  READ(LINE,*,IOSTAT=IOS) (IPFSTRING(J),J=1,TS(JJ)%NCOLIPF)
  IF(IOS.NE.0.AND.LSS)CALL IMOD_UTL_PRINTTEXT('Error occured in IPF, syntax is at least: '//TRIM(CIPFTYPE(TS(JJ)%IPFTYPE)),2)
  IF(IOS.NE.0.AND.(.NOT.LSS))THEN
   CALL IMOD_UTL_PRINTTEXT('line '//TRIM(IMOD_UTL_ITOS(I))//','//TRIM(LINE),0)
   CALL IMOD_UTL_PRINTTEXT('Error occured in IPF, syntax is at least: '//TRIM(CIPFTYPE(TS(JJ)%IPFTYPE)),2)
  ENDIF
  II=II+1; III=III+1
  READ(IPFSTRING(TS(JJ)%IXCOL),*,IOSTAT=IOS) TS(JJ)%STVALUE(III)%X
  IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Missing proper column expressing the X-coordinate in timeserie IPF (record:'// &
    TRIM(IMOD_UTL_ITOS(I))//')',2)
  READ(IPFSTRING(TS(JJ)%IYCOL),*,IOSTAT=IOS) TS(JJ)%STVALUE(III)%Y
  IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Missing proper column expressing the Y-coordinate in timeserie IPF (record:'// &
    TRIM(IMOD_UTL_ITOS(I))//')',2)
  READ(IPFSTRING(TS(JJ)%ILCOL),*,IOSTAT=IOS) TS(JJ)%STVALUE(III)%ILAY
  IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Missing proper column expressing the LAYER identification in timeserie IPF (record:'// &
     TRIM(IMOD_UTL_ITOS(I))//')',2)
  !## read weighing
  TS(JJ)%STVALUE(III)%W=1.0
  IF(TS(JJ)%IVCOL.NE.0)THEN
   READ(IPFSTRING(ABS(TS(JJ)%IVCOL)),*,IOSTAT=IOS) TS(JJ)%STVALUE(III)%W
   IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Missing proper column expressing the WEIGHING in timeserie IPF (record:'// &
       TRIM(IMOD_UTL_ITOS(I))//')',2)
  ENDIF
  TS(JJ)%STVALUE(III)%M=MV
  TS(JJ)%STVALUE(III)%ID='Location_'//TRIM(IMOD_UTL_ITOS(II))
  IF(TS(JJ)%IMCOL.NE.0)THEN
   IF(TS(JJ)%IEXT.EQ.0)THEN
    !## steady-state
    IF(TS(JJ)%IPFTYPE.EQ.1)THEN
    READ(IPFSTRING(TS(JJ)%IMCOL),*,IOSTAT=IOS) TS(JJ)%STVALUE(III)%M
     IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Missing proper column expressing the MEASUREMENT in timeserie IPF (record:'// &
         TRIM(IMOD_UTL_ITOS(I))//')',2)
    !## transient
    ELSEIF(TS(JJ)%IPFTYPE.EQ.2)THEN
    READ(IPFSTRING(TS(JJ)%IMCOL),'(A52)',IOSTAT=IOS) TS(JJ)%STVALUE(III)%ID
     IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Missing proper column expressing the IDENTIFICATION in timeserie IPF (record:'// &
        TRIM(IMOD_UTL_ITOS(I))//')',2)
    ENDIF
   ELSEIF(TS(JJ)%IEXT.GT.0)THEN
    READ(IPFSTRING(TS(JJ)%IMCOL),'(A52)',IOSTAT=IOS) TS(JJ)%STVALUE(III)%ID
    IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Missing proper column expressing the IDENTIFICATION in timeserie IPF (record:'// &
       TRIM(IMOD_UTL_ITOS(I))//')',2)
   ENDIF
  ENDIF
 ENDDO
 TS(JJ)%NROWIPF=III
 IF(TS(JJ)%DEL)THEN ! PKS
  CALL IMOD_UTL_DELETE_BY_UNIT(TS(JJ)%IUIPF) ! PKS
 ELSE  ! PKS
  CLOSE(TS(JJ)%IUIPF);
 ENDIF
 DEALLOCATE(IPFSTRING)
END DO

RETURN
END SUBROUTINE

!###====================================================================
SUBROUTINE TSERIE1INIT2(LPEST,LSS,MV,root,subm)
!###====================================================================
USE IMOD_UTL 
USE TSVAR
IMPLICIT NONE
! arguments
LOGICAL, INTENT(IN) :: LPEST
LOGICAL, INTENT(IN) :: LSS
CHARACTER(LEN=*),intent(in) :: root,subm
REAL, INTENT(IN) :: MV
! locals
INTEGER :: IOS,I,II,III,J,JJ,N,NVALID,K,ichr
CHARACTER(LEN=52),DIMENSION(:),ALLOCATABLE :: IPFSTRING
CHARACTER(LEN=256) :: LINE
CHARACTER(LEN=1000) :: BIGLINE
LOGICAL :: LPKS ! PKS
CHARACTER(LEN=100) :: PS ! PKS

IF(IIPF.EQ.0)RETURN

IF(LPEST.EQ.1.AND.SUM(TS%NROWIPF).LE.0)CALL IMOD_UTL_PRINTTEXT('Should at least be some measurement points in area of interest!',2)

CALL PKS7MPIACTIVE(LPKS) ! PKS
CALL PKS7MPIPARTSTR(PS)  ! PKS

ichr=92; if(OS.eq.2)ichr=47

if(trim(subm).eq.'')then
 CALL IMOD_UTL_CREATEDIR(TRIM(root)//CHAR(ichr)//'timeseries')
else
 CALL IMOD_UTL_CREATEDIR(TRIM(root)//CHAR(ichr)//trim(subm)//char(ichr)//'timeseries')
endif

!## steady-state
IF(LSS)THEN

 DO JJ=1,ABS(IIPF)

  I=INDEX(TS(JJ)%IPFNAME,CHAR(92),.TRUE.)+1
  K = INDEX(TS(JJ)%IPFNAME,'.',.TRUE.)-1 ! PKS
  if(trim(subm).eq.'')then
   LINE=TRIM(ROOT)//CHAR(ichr)//'timeseries'//char(ichr)//TS(JJ)%IPFNAME(I:K)//TRIM(PS)//'.ipf'
  else
   LINE=TRIM(ROOT)//CHAR(ichr)//trim(subm)//char(ichr)//'timeseries'//char(ichr)//TS(JJ)%IPFNAME(I:K)//TRIM(PS)//'.ipf'
  endif
  CALL IMOD_UTL_SWAPSLASH(LINE)
  CALL IMOD_UTL_OPENASC(TS(JJ)%IUIPF,LINE,'W')
  ! check for valid data
  NVALID = 0
  DO I=1,TS(JJ)%NROWIPF
    IF (TS(JJ)%STVALUE(I)%VALID) NVALID = NVALID + 1
  END DO
  IF(LPKS.AND.NVALID.EQ.0) TS(JJ)%DEL = .TRUE. ! PKS
  WRITE(TS(JJ)%IUIPF,*) NVALID
  WRITE(TS(JJ)%IUIPF,*) 8
  WRITE(TS(JJ)%IUIPF,'(A)') 'X-COORDINATE'
  WRITE(TS(JJ)%IUIPF,'(A)') 'Y-COORDINATE'
  WRITE(TS(JJ)%IUIPF,'(A)') 'MODELLAYER'
  WRITE(TS(JJ)%IUIPF,'(A)') 'OBSERVATION'
  WRITE(TS(JJ)%IUIPF,'(A)') 'VARIANCE'
  WRITE(TS(JJ)%IUIPF,'(A)') 'COMPUTED_HEAD'
  WRITE(TS(JJ)%IUIPF,'(A)') 'DIFFERENCE'
  WRITE(TS(JJ)%IUIPF,'(A)') 'WEIGHED_DIFFERENCE'
  WRITE(TS(JJ)%IUIPF,'(A)') '0,TXT'
 ENDDO
 
!## transient
ELSE

 if(IUIPFTXT.eq.0)then
  if(trim(subm).eq.'')then
   LINE=TRIM(root)//CHAR(ichr)//'timeseries'//CHAR(ichr)//'timeseries_collect'//TRIM(PS)//'.txt'
  else
   LINE=TRIM(root)//CHAR(ichr)//trim(subm)//char(ichr)//'timeseries'//CHAR(ichr)//'timeseries_collect'//TRIM(PS)//'.txt'
  endif
  !## open txt file to collect all timeseries
  CALL IMOD_UTL_SWAPSLASH(LINE)
  IUIPFTXT=IMOD_UTL_GETUNIT()
  CALL IMOD_UTL_OPENASC(IUIPFTXT,LINE,'W')
 endif
 
 II=0
 DO JJ=1,ABS(IIPF)

  !## skip as file already closed again
  IF(TS(JJ)%IUIPF.EQ.0)CYCLE
  
  IF(OS.EQ.1)THEN
   I=INDEX(TS(JJ)%IPFNAME,CHAR(92),.TRUE.)+1
  ELSE
   I=INDEX(TS(JJ)%IPFNAME,CHAR(47),.TRUE.)+1
  ENDIF
  CALL PKS7MPIPARTSTR(PS) ! PKS
  K = INDEX(TS(JJ)%IPFNAME,'.',.TRUE.)-1 ! PKS
  if(trim(subm).eq.'')then
   LINE=TRIM(root)//CHAR(ichr)//'timeseries'//CHAR(ichr)//TS(JJ)%IPFNAME(I:K)//TRIM(PS)//'.ipf'
  else
   LINE=TRIM(root)//CHAR(ichr)//trim(subm)//char(ichr)//'timeseries'//CHAR(ichr)//TS(JJ)%IPFNAME(I:K)//TRIM(PS)//'.ipf'
  endif
  CALL IMOD_UTL_SWAPSLASH(LINE)
  TS(JJ)%IUIPF=IMOD_UTL_GETUNIT(); CALL IMOD_UTL_OPENASC(TS(JJ)%IUIPF,LINE,'W')

  ! check for valid data
  NVALID=0
  DO I=1,TS(JJ)%NROWIPF
   IF(TS(JJ)%STVALUE(I)%VALID)NVALID=NVALID+1
  END DO
  IF(LPKS.AND.NVALID.EQ.0) TS(JJ)%DEL = .TRUE. ! PKS
  CALL IMOD_UTL_PRINTTEXT('        * Assigned '//TRIM(IMOD_UTL_ITOS(NVALID))//' locations from total of '//TRIM(IMOD_UTL_ITOS(TS(JJ)%NROWIPF)),0)
!  CALL IMOD_UTL_PRINTTEXT('          >>> NO locations will be used that are WITHIN the buffer-zone <<<',0)

  !## write entire ipf again
  WRITE(TS(JJ)%IUIPF,*) NVALID
  WRITE(TS(JJ)%IUIPF,*) 5
  WRITE(TS(JJ)%IUIPF,'(A)') 'X-COORDINATE'
  WRITE(TS(JJ)%IUIPF,'(A)') 'Y-COORDINATE'
  WRITE(TS(JJ)%IUIPF,'(A)') 'MODELLAYER'
  WRITE(TS(JJ)%IUIPF,'(A)') 'IDENTIFICATION'
  WRITE(TS(JJ)%IUIPF,'(A)') 'VARIANCE'
  WRITE(TS(JJ)%IUIPF,'(A)') '4,TXT'

  DO I=1,TS(JJ)%NROWIPF
   IF(.NOT.TS(JJ)%STVALUE(I)%VALID) CYCLE  ! skip invalid data
   BIGLINE=TRIM(IMOD_UTL_DTOS(TS(JJ)%STVALUE(I)%X,'F',3))//','//TRIM(IMOD_UTL_DTOS(TS(JJ)%STVALUE(I)%Y,'F',3))//','// &
           TRIM(IMOD_UTL_ITOS(TS(JJ)%STVALUE(I)%ILAY))   //',"ipf'//TRIM(IMOD_UTL_ITOS(JJ))//'_'//TRIM(TS(JJ)%STVALUE(I)%ID)//'",'// &
           TRIM(IMOD_UTL_DTOS(TS(JJ)%STVALUE(I)%W,'G',7))
   WRITE(TS(JJ)%IUIPF,'(A)') TRIM(BIGLINE)
  END DO

  IF(TS(JJ)%DEL)THEN ! PKS
   CALL IMOD_UTL_DELETE_BY_UNIT(TS(JJ)%IUIPF) ! PKS
  ELSE  ! PKS
   CLOSE(TS(JJ)%IUIPF)
  ENDIF 
  TS(JJ)%IUIPF=0
 ENDDO
ENDIF

CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT(' Finished IPF for saving TIMESERIES',0)

RETURN
END SUBROUTINE

!###====================================================================
SUBROUTINE TSERIE1WRITE(ISIM,LSS,DDATE,MV,usests,root,subm,cdate)
!###====================================================================
USE IMOD_UTL
USE TSVAR
IMPLICIT NONE
! arguments
CHARACTER(LEN=*),intent(in) :: root,cdate,subm
INTEGER,INTENT(IN) :: ISIM
LOGICAL, INTENT(IN) :: LSS
DOUBLE PRECISION, INTENT(IN) :: DDATE
REAL(KIND=4), INTENT(IN) :: MV
LOGICAL, INTENT(IN) :: USESTS
! locals
INTEGER,ALLOCATABLE,DIMENSION(:) :: IUTXT,JUTXT
INTEGER(KIND=8),ALLOCATABLE, DIMENSION(:) :: TSDATE
REAL(KIND=8) :: DH1,DH2,H,X,Y,W,Z,M,HH,WW
INTEGER :: I,II,J,JJJ,ILAY,N,IDATE,JDATE,JJ,IU,KK,IOS,JOS,IREC,MPER
CHARACTER(LEN=52) :: CLABEL,CLDATE
INTEGER :: HOUR, MINUTE, SECOND,ICHR
INTEGER(KIND=8) :: DBLDATE
CHARACTER(LEN=256) :: LINE
CHARACTER(LEN=100) :: PS

IF(IIPF.EQ.0)RETURN

!## steady-state
IF(LSS)THEN

 IF(ISIM.EQ.1)THEN

  NREC = NREC + 1

  CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT('Writing Computed Heads to IPF file ...',0)

  DO JJ=1,ABS(IIPF)
   DO I=1,TS(JJ)%NROWIPF
    IF (.NOT.TS(JJ)%STVALUE(I)%VALID) CYCLE ! skip invalid data
    H = TS(JJ)%STVALUE(I)%C ! get computed value
    DH1=0.0; DH2=0.0
    !## compute difference whenever head is available and computed head not equal to no-flow value
    IF(TS(JJ)%STVALUE(I)%M.NE.MV.AND.H.NE.MV)THEN
     DH1=H-TS(JJ)%STVALUE(I)%M
    ELSE
     TS(JJ)%STVALUE(I)%W=0.0
    ENDIF
    W=TS(JJ)%STVALUE(I)%W
    IF(TS(JJ)%IVCOL.GT.0)THEN
     IF(W.GT.0.0)W=1.0/SQRT(W); W=MAX(0.0,W)
    ENDIF
    DH2=W*DH1

    LINE=TRIM(IMOD_UTL_DTOS(TS(JJ)%STVALUE(I)%X,'F',3))//','//TRIM(IMOD_UTL_DTOS(TS(JJ)%STVALUE(I)%Y,'F',3))//','// &
         TRIM(IMOD_UTL_ITOS(TS(JJ)%STVALUE(I)%ILAY))   //','//TRIM(IMOD_UTL_DTOS(TS(JJ)%STVALUE(I)%M,'F',3))//','// &
         TRIM(IMOD_UTL_DTOS(TS(JJ)%STVALUE(I)%W,'F',2))//','//TRIM(IMOD_UTL_DTOS(H,'G',7))//            ','// &
         TRIM(IMOD_UTL_DTOS(DH1,'G',7))          //','//TRIM(IMOD_UTL_DTOS(DH2,'G',7))
    WRITE(TS(JJ)%IUIPF,'(A)') TRIM(LINE)
   ENDDO
   IF(TS(JJ)%DEL)THEN ! PKS
    CALL IMOD_UTL_DELETE_BY_UNIT(TS(JJ)%IUIPF) ! PKS
   ELSE  ! PKS
    CLOSE(TS(JJ)%IUIPF)
   ENDIF
  ENDDO
 ENDIF

!## transient
ELSE

 !## simulation
 IF(ISIM.EQ.0)THEN
  
  CLDATE=ADJUSTL(CDATE)
  IF(CLDATE(9:14).EQ.'000000')THEN
   WRITE(IUIPFTXT,*) CLDATE(1:8)
  ELSE
   WRITE(IUIPFTXT,*) TRIM(CLDATE)
  ENDIF

  NREC = NREC + 1

  II=0
  DO JJ=1,ABS(IIPF)
   DO I=1,TS(JJ)%NROWIPF
    IF (.NOT.TS(JJ)%STVALUE(I)%VALID) CYCLE ! skip invalid data
    !## optional weigh factor
    W=1.0; W=TS(JJ)%STVALUE(I)%W;
    IF(TS(JJ)%IVCOL.GT.0)THEN
     IF(W.GT.0.0)W=1.0/SQRT(W)
    ENDIF
    W=MAX(0.0,W)
    H = TS(JJ)%STVALUE(I)%C ! get computed value
    WRITE(IUIPFTXT,*) JJ,I,H,W
   ENDDO
  ENDDO
  RETURN

 !## clean-up
 ELSE
  
  CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT(' Writing Timeseries to IPF file ...',0)
  CLOSE(IUIPFTXT); IUIPFTXT=0
  CALL PKS7MPIPARTSTR(PS) ! PKS
  ichr=92; if(OS.eq.2)ichr=47
  if(trim(subm).eq.'')then
   LINE=TRIM(root)//CHAR(ichr)//'timeseries'//CHAR(ichr)//'timeseries_collect'//TRIM(PS)//'.txt'
  else
   LINE=TRIM(root)//CHAR(ichr)//trim(subm)//char(ichr)//'timeseries'//CHAR(ichr)//'timeseries_collect'//TRIM(PS)//'.txt'
  endif
  CALL IMOD_UTL_SWAPSLASH(LINE)
  IU=IMOD_UTL_GETUNIT(); CALL IMOD_UTL_OPENASC(IU,LINE,'R')

  IF(ALLOCATED(IUTXT)) DEALLOCATE(IUTXT);  IF(ALLOCATED(JUTXT)) DEALLOCATE(JUTXT)
  IF(ALLOCATED(TSDATE))DEALLOCATE(TSDATE); IF(ALLOCATED(TSNODATA))DEALLOCATE(TSNODATA)
  IF(ALLOCATED(TSM))DEALLOCATE(TSM)

  I=SUM(TS%NROWIPF)
  ALLOCATE(IUTXT(I),JUTXT(I),TSDATE(I),TSNODATA(I),TSM(I))

  !## get number of stressperiods with a date
  MPER=0
  DO IREC=1,NREC
   READ(IU,*,IOSTAT=JOS) DBLDATE !IDATE
   !## skip this period since it is apparently not a date
   IF(JOS.EQ.0)MPER=MPER+1
   DO J=1,ABS(IIPF); DO I=1,TS(J)%NROWIPF
    IF(.NOT.TS(J)%STVALUE(I)%VALID)CYCLE; READ(IU,*)
   ENDDO; ENDDO
  ENDDO
  REWIND(IU)

  !## open and write header of txt files
  II=0; JJ=0; IUTXT=0
  DO J=1,ABS(IIPF)
   DO I=1,TS(J)%NROWIPF
    IF(.NOT.TS(J)%STVALUE(I)%VALID)CYCLE ! skip invalid data
    
    II=II+1

    !## apply appropriate slash
    IF(trim(subm).eq.'')THEN 
     LINE=TRIM(ROOT)//CHAR(ichr)//'timeseries'//CHAR(ichr)//'ipf'//TRIM(IMOD_UTL_ITOS(J))//'_'//TRIM(TS(J)%STVALUE(I)%ID)//'.txt'
    ELSE
     LINE=TRIM(ROOT)//CHAR(ichr)//trim(subm)//char(ichr)//'timeseries'//CHAR(ichr)//'ipf'//TRIM(IMOD_UTL_ITOS(J))//'_'//TRIM(TS(J)%STVALUE(I)%ID)//'.txt'
    ENDIF
     
    IF(II.GT.1)THEN
     IUTXT(II)=IMOD_UTL_GETUNIT(IUTXT(II-1))
    ELSE
     IUTXT(II)=IMOD_UTL_GETUNIT()
    ENDIF

    OPEN(IUTXT(II),FILE=LINE,FORM='FORMATTED',ACTION='WRITE',STATUS='UNKNOWN',IOSTAT=IOS) 
    IF(IOS.NE.0)THEN 
     !## maybe the directory doesn't exist - create it and try again
     
     JJJ=INDEX(LINE,CHAR(ichr),.TRUE.)
     IF(JJJ.GT.0)CALL IMOD_UTL_CREATEDIR(LINE(1:JJJ-1))

    OPEN(IUTXT(II),FILE=LINE,FORM='FORMATTED',ACTION='WRITE',STATUS='UNKNOWN',IOSTAT=IOS) 
     IF(IOS.NE.0)THEN
     CALL IMOD_UTL_PRINTTEXT(' Cannot create file '//TRIM(LINE),0)
      CALL IMOD_UTL_PRINTTEXT(' Probably not enough free unit numbers '//TRIM(IMOD_UTL_ITOS(IUTXT(II))),0)
      CALL IMOD_UTL_PRINTTEXT(' or duplicate file name and file is allready opened',0)
      CALL IMOD_UTL_PRINTTEXT('Stopped',2)
     ENDIF
    ENDIF

    !## write header and labels of txt file
    WRITE(IUTXT(II),'(I10)') MPER
    IF(TS(J)%IEXT.EQ.0)THEN
     WRITE(IUTXT(II),'(I1)') 2 
    ELSE
     WRITE(IUTXT(II),'(I1)') 3 
    ENDIF
    WRITE(IUTXT(II),'(A)') 'Date,-999'

    IF(TS(J)%IEXT.GT.0)THEN
     !## if iext.gt.0 read textfiles with measures
     JJJ=INDEX(TS(J)%IPFNAME,CHAR(ichr),.TRUE.)

     LINE=TS(J)%IPFNAME(:JJJ)//TRIM(TS(J)%STVALUE(I)%ID)//'.'//TRIM(TS(J)%EXT)
     JJ=JJ+1
     IF(JJ.GT.1)THEN
      JUTXT(JJ)=IMOD_UTL_GETUNIT(JUTXT(JJ-1))
     ELSE
      JUTXT(JJ)=IMOD_UTL_GETUNIT()
     ENDIF
     OPEN(JUTXT(JJ),FILE=LINE,FORM='FORMATTED',ACTION='READ',STATUS='OLD',SHARE='DENYNONE',IOSTAT=IOS) 

     READ(JUTXT(II),*)
     READ(JUTXT(II),*) N
     DO JJJ=1,2; READ(JUTXT(II),*) CLABEL,TSNODATA(II); ENDDO
     DO JJJ=2+1,N; READ(JUTXT(II),*) ; ENDDO
     WRITE(IUTXT(II),*) 'Measure,',TSNODATA(II) 
    ENDIF
    WRITE(IUTXT(II),*) 'Computed_Head,',MV 
   ENDDO
  ENDDO

  !## write summary into different *.txt files
  TSDATE=0D0
  DO IREC=1,NREC

   READ(IU,*,IOSTAT=JOS) DBLDATE !IDATE
   !## skip this period since it is apparently not a date
   IF(JOS.NE.0)THEN
    DO J=1,ABS(IIPF); DO I=1,TS(J)%NROWIPF; IF(.NOT.TS(J)%STVALUE(I)%VALID)CYCLE; READ(IU,*); ENDDO; ENDDO
    CYCLE
   ENDIF 
   !## make double precision dates
   IF(DBLDATE.LT.100000000)DBLDATE=DBLDATE*1000000

!   JDATE=IMOD_UTL_IDATETOJDATE(IDATE)

   II=0
   DO JJ=1,ABS(IIPF)
    DO I=1,TS(JJ)%NROWIPF
     IF(.NOT.TS(JJ)%STVALUE(I)%VALID)CYCLE
     II=II+1

     !## without impulse/default
     READ(IU,*,IOSTAT=JOS) KK,J,H,W
     IF(JOS.NE.0)CYCLE
     IF(KK.NE.JJ)CALL IMOD_UTL_PRINTTEXT(' Something goes wrong in reading summary timeseries on line 532 in imodflow_tseries.f90',2)

     IF(TS(JJ)%IEXT.GT.0)THEN
      IF(TSDATE(II).EQ.DBLDATE)THEN !JDATE)THEN
       M=TSM(II)
      !## try to read next date
      ELSEIF(TSDATE(II).LT.DBLDATE)THEN !JDATE)THEN
       !## read until current date is found
       DO
        READ(JUTXT(II),*,IOSTAT=IOS) TSDATE(II),TSM(II)
        IF(IOS.NE.0)EXIT
        IF(TSDATE(II).LT.100000000)TSDATE(II)=TSDATE(II)*1000000
!        TSDATE(II)=IMOD_UTL_IDATETOJDATE(TSDATE(II))
        IF(TSDATE(II).GE.DBLDATE)EXIT !JDATE)EXIT 
       ENDDO
       M=TSM(II)
       IF(TSDATE(II).NE.DBLDATE)M=TSNODATA(II) !JDATE)M=TSNODATA(II)
      ELSE
       M=TSNODATA(II)
      ENDIF
     ENDIF

     IF(TS(JJ)%IEXT.GT.0)THEN
      WRITE(IUTXT(II),*) DBLDATE,M,H !IDATE,M,H
     ELSE
      WRITE(IUTXT(II),*) DBLDATE,H   !IDATE,H
     ENDIF   

  ENDDO
   ENDDO
  ENDDO
  
  II=0; DO J=1,ABS(IIPF)
   DO I=1,TS(J)%NROWIPF; IF(.NOT.TS(J)%STVALUE(I)%VALID)CYCLE; II=II+1; CLOSE(IUTXT(II)); CLOSE(JUTXT(II)); ENDDO
  ENDDO
  
  IF(ALLOCATED(IUTXT)) DEALLOCATE(IUTXT);  IF(ALLOCATED(JUTXT)) DEALLOCATE(JUTXT)
  IF(ALLOCATED(TSDATE))DEALLOCATE(TSDATE); IF(ALLOCATED(TSNODATA))DEALLOCATE(TSNODATA)
  IF(ALLOCATED(TSM))DEALLOCATE(TSM)
 
  CALL IMOD_UTL_PRINTTEXT('',3); CALL IMOD_UTL_PRINTTEXT(' Finished Writing Timeseries to IPF file ...',0)
  ENDIF
 
 CLOSE(IU) 
 
ENDIF

RETURN
END SUBROUTINE

!###====================================================================
SUBROUTINE TSERIE1CLOSE()
!###====================================================================
USE TSVAR
USE IMOD_UTL, ONLY : IMOD_UTL_PRINTTEXT
IMPLICIT NONE
LOGICAL :: LEX
INTEGER :: I,II,JJ

IF(IIPF.EQ.0)RETURN

CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT(' Finished writing timeseries...',0)

!IF(ALLOCATED(IUTXT))THEN
! DO I=1,SIZE(IUTXT)
!  IF(IUTXT(I).GT.0)THEN
!   INQUIRE(UNIT=IUTXT(I),OPENED=LEX); IF(LEX)CLOSE(IUTXT(I))
!  ENDIF
! ENDDO
! DEALLOCATE(IUTXT)
!ENDIF

DO JJ = 1, ABS(IIPF)
  IF (ASSOCIATED(TS(JJ)%STVALUE)) DEALLOCATE(TS(JJ)%STVALUE)
END DO

RETURN
END SUBROUTINE