!!  Copyright (C) Stichting Deltares, 2005-2014.
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
 CHARACTER(LEN=256), SAVE :: ROOTRES = ''
 INTEGER :: NST,IUIPFTXT
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IUTXT,JUTXT,TSDATE
 REAL,ALLOCATABLE,DIMENSION(:) :: TSNODATA,TSM
 CHARACTER(LEN=50),DIMENSION(:),ALLOCATABLE :: ATTRIB
 CHARACTER(LEN=11),DIMENSION(3) :: CIPFTYPE
 DATA CIPFTYPE/'X,Y,L,Z,W','X,Y,L,ID,W','X,Y,L,Z,W,T'/
 TYPE STOBJ
  LOGICAL :: VALID = .TRUE.
  REAL :: X,Y,W,M,C ! X,Y,W(EIGHT),M(EASURED),C(COMPUTED)
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
 END TYPE TSOBJ
 TYPE(TSOBJ),ALLOCATABLE,DIMENSION(:),SAVE :: TS

END MODULE

!###====================================================================
SUBROUTINE TSERIE1INIT1(LPEST,LSS,MV)
!###====================================================================
USE IMOD_UTL, ONLY : IMOD_UTL_PRINTTEXT,IMOD_UTL_STRING,IMOD_UTL_ITOS,IMOD_UTL_RTOS,IMOD_UTL_OPENASC,IMOD_UTL_CAP,IMOD_UTL_CREATEDIR,IMOD_UTL_SWAPSLASH,IMOD_UTL_OSSYSTEM,OS
USE TSVAR
IMPLICIT NONE
! arguments
LOGICAL, INTENT(IN) :: LPEST
LOGICAL, INTENT(IN) :: LSS
REAL, INTENT(IN) :: MV
! locals
INTEGER :: IOS,I,II,III,J,JJ,N
CHARACTER(LEN=52),DIMENSION(:),ALLOCATABLE :: IPFSTRING
CHARACTER(LEN=256) :: LINE
CHARACTER(LEN=1000) :: BIGLINE

IF(IIPF.EQ.0)RETURN

NREC = 0

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
   IF(LPEST.AND.TS(JJ)%IEXT.EQ.0)THEN !NE.4)THEN
    CALL IMOD_UTL_PRINTTEXT('Error occured in IPF, syntax is at least: '//TRIM(CIPFTYPE(TS(JJ)%IPFTYPE)),0)
    CALL IMOD_UTL_PRINTTEXT('IEXT should be assigned to any column',2)
   ENDIF
  !## moments
  ELSEIF(TS(JJ)%IPFTYPE.EQ.3)THEN
   IF(TS(JJ)%IEXT.GT.0)THEN
    CALL IMOD_UTL_PRINTTEXT('Error occured in IPF, syntax is at least: '//TRIM(CIPFTYPE(TS(JJ)%IPFTYPE)),0)
    CALL IMOD_UTL_PRINTTEXT('IEXT should be ZERO',2)
   ENDIF
  ENDIF
 ENDIF
ENDDO

II=0
DO JJ=1,ABS(IIPF)
 ALLOCATE(IPFSTRING(TS(JJ)%NCOLIPF))
 III=0
 ALLOCATE(TS(JJ)%STVALUE(MAX(1,TS(JJ)%NROWIPF)))
 DO I=1,TS(JJ)%NROWIPF
  READ(TS(JJ)%IUIPF,'(A256)') LINE
  CALL IMOD_UTL_STRING(LINE)
  READ(LINE,*,IOSTAT=IOS) (IPFSTRING(J),J=1,TS(JJ)%NCOLIPF)
  IF(IOS.NE.0.AND.LSS)CALL IMOD_UTL_PRINTTEXT('Error occured in IPF, syntax is at least: '//TRIM(CIPFTYPE(TS(JJ)%IPFTYPE)),2)
  IF(IOS.NE.0.AND.(.NOT.LSS))THEN
   CALL IMOD_UTL_PRINTTEXT('line '//TRIM(IMOD_UTL_ITOS(I))//','//TRIM(LINE),0)
   CALL IMOD_UTL_PRINTTEXT('Error occured in IPF, syntax is at least: '//TRIM(CIPFTYPE(TS(JJ)%IPFTYPE)),2)
  ENDIF
  II=II+1; III=III+1
  READ(IPFSTRING(TS(JJ)%IXCOL),*,IOSTAT=IOS) TS(JJ)%STVALUE(III)%X
  IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Missing proper column expressing the X-coordinate in timeserie IPF (record:'//TRIM(IMOD_UTL_ITOS(I))//')',2)
  READ(IPFSTRING(TS(JJ)%IYCOL),*,IOSTAT=IOS) TS(JJ)%STVALUE(III)%Y
  IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Missing proper column expressing the Y-coordinate in timeserie IPF (record:'//TRIM(IMOD_UTL_ITOS(I))//')',2)
  READ(IPFSTRING(TS(JJ)%ILCOL),*,IOSTAT=IOS) TS(JJ)%STVALUE(III)%ILAY
  IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Missing proper column expressing the LAYER identification in timeserie IPF (record:'//TRIM(IMOD_UTL_ITOS(I))//')',2)
  !## read weighing
  TS(JJ)%STVALUE(III)%W=1.0
  IF(TS(JJ)%IVCOL.NE.0)THEN
   READ(IPFSTRING(ABS(TS(JJ)%IVCOL)),*,IOSTAT=IOS) TS(JJ)%STVALUE(III)%W
   IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Missing proper column expressing the WEIGHING in timeserie IPF (record:'//TRIM(IMOD_UTL_ITOS(I))//')',2)
  ENDIF
  TS(JJ)%STVALUE(III)%M=MV
  TS(JJ)%STVALUE(III)%ID='Location_'//TRIM(IMOD_UTL_ITOS(II))
  IF(TS(JJ)%IMCOL.NE.0)THEN
   IF(TS(JJ)%IEXT.EQ.0)THEN
    READ(IPFSTRING(TS(JJ)%IMCOL),*,IOSTAT=IOS) TS(JJ)%STVALUE(III)%M
    IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Missing proper column expressing the MEASUREMENT in timeserie IPF (record:'//TRIM(IMOD_UTL_ITOS(I))//')',2)
   ELSEIF(TS(JJ)%IEXT.GT.0)THEN
    READ(IPFSTRING(TS(JJ)%IMCOL),'(A52)',IOSTAT=IOS) TS(JJ)%STVALUE(III)%ID
    IF(IOS.NE.0)CALL IMOD_UTL_PRINTTEXT('Missing proper column expressing the IDENTIFICATION in timeserie IPF (record:'//TRIM(IMOD_UTL_ITOS(I))//')',2)
   ENDIF
  ENDIF
 ENDDO
 TS(JJ)%NROWIPF=III
 CLOSE(TS(JJ)%IUIPF); DEALLOCATE(IPFSTRING)
END DO

RETURN
END SUBROUTINE

!###====================================================================
SUBROUTINE TSERIE1INIT2(LPEST,LSS,MV,root)
!###====================================================================
USE IMOD_UTL, ONLY : IMOD_UTL_PRINTTEXT,IMOD_UTL_STRING,IMOD_UTL_ITOS,IMOD_UTL_RTOS,IMOD_UTL_OPENASC,IMOD_UTL_CAP,IMOD_UTL_CREATEDIR,IMOD_UTL_SWAPSLASH,OS
USE TSVAR
IMPLICIT NONE
! arguments
LOGICAL, INTENT(IN) :: LPEST
LOGICAL, INTENT(IN) :: LSS
CHARACTER(LEN=*),intent(in) :: root
REAL, INTENT(IN) :: MV
! locals
INTEGER :: IOS,I,II,III,J,JJ,N,NVALID
CHARACTER(LEN=52),DIMENSION(:),ALLOCATABLE :: IPFSTRING
CHARACTER(LEN=256) :: LINE
CHARACTER(LEN=1000) :: BIGLINE

IF(IIPF.EQ.0)RETURN

IF(LPEST.EQ.1.AND.SUM(TS%NROWIPF).LE.0)CALL IMOD_UTL_PRINTTEXT('Should at least be some measurement points in area of interest!',2)
!## steady-state
IF(LSS)THEN
 DO JJ=1,ABS(IIPF)
  I=INDEX(TS(JJ)%IPFNAME,CHAR(92),.TRUE.)+1
  LINE=TRIM(ROOT)//CHAR(92)//TS(JJ)%IPFNAME(I:)
!  LINE=TRIM(ROOTRES)//CHAR(92)//TS(JJ)%IPFNAME(I:)
  CALL IMOD_UTL_SWAPSLASH(LINE)
  CALL IMOD_UTL_OPENASC(TS(JJ)%IUIPF,LINE,'W')
  ! check for valid data
  NVALID = 0
  DO I=1,TS(JJ)%NROWIPF
    IF (TS(JJ)%STVALUE(I)%VALID) NVALID = NVALID + 1
  END DO
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

 CALL IMOD_UTL_CREATEDIR(TRIM(root)//CHAR(92)//'timeseries')
! CALL IMOD_UTL_CREATEDIR(TRIM(ROOTRES)//CHAR(92)//'timeseries')
 !## open txt file to collect all timeseries
! LINE=TRIM(ROOTRES)//CHAR(92)//'timeseries'//CHAR(92)//'timeseries_collect.txt'
 LINE=TRIM(root)//CHAR(92)//'timeseries'//CHAR(92)//'timeseries_collect.txt'
 CALL IMOD_UTL_SWAPSLASH(LINE); CALL IMOD_UTL_OPENASC(IUIPFTXT,LINE,'W')

 II=0
 DO JJ=1,ABS(IIPF)

  IF(OS.EQ.1)THEN
   I=INDEX(TS(JJ)%IPFNAME,CHAR(92),.TRUE.)+1
  ELSE
   I=INDEX(TS(JJ)%IPFNAME,CHAR(47),.TRUE.)+1
  ENDIF
  LINE=TRIM(root)//CHAR(92)//'timeseries'//CHAR(92)//TS(JJ)%IPFNAME(I:)
!  LINE=TRIM(ROOTRES)//CHAR(92)//'timeseries'//CHAR(92)//TS(JJ)%IPFNAME(I:)
  CALL IMOD_UTL_SWAPSLASH(LINE); CALL IMOD_UTL_OPENASC(TS(JJ)%IUIPF,LINE,'W')

  ! check for valid data
  NVALID = 0
  DO I=1,TS(JJ)%NROWIPF
    IF (TS(JJ)%STVALUE(I)%VALID) NVALID = NVALID + 1
  END DO
  CALL IMOD_UTL_PRINTTEXT('        * Assigned '//TRIM(IMOD_UTL_ITOS(NVALID))//' locations from total of '//TRIM(IMOD_UTL_ITOS(TS(JJ)%NROWIPF)),0)
  CALL IMOD_UTL_PRINTTEXT('          >>> NO locations will be used that are WITHIN the buffer-zone <<<',0)

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
   IF (.NOT.TS(JJ)%STVALUE(I)%VALID) CYCLE  ! skip invalid data
   BIGLINE=TRIM(IMOD_UTL_RTOS(TS(JJ)%STVALUE(I)%X,'F',3))//','//TRIM(IMOD_UTL_RTOS(TS(JJ)%STVALUE(I)%Y,'F',3))//','// &
           TRIM(IMOD_UTL_ITOS(TS(JJ)%STVALUE(I)%ILAY))   //',"'//TRIM(TS(JJ)%STVALUE(I)%ID)//         '",'// &
           TRIM(IMOD_UTL_RTOS(TS(JJ)%STVALUE(I)%W,'*',2))
   WRITE(TS(JJ)%IUIPF,'(A)') TRIM(BIGLINE)
  END DO

  CLOSE(TS(JJ)%IUIPF)

 ENDDO
ENDIF

CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT('Finished IPF for saving TIMESERIES',0)

RETURN
END SUBROUTINE

!###====================================================================
SUBROUTINE TSERIE1WRITE(ISIM,LSS,DDATE,MV,usests,root,issflg,time_cstring,time_ostring,idate_save)
!###====================================================================
USE IMOD_UTL, ONLY : IMOD_UTL_ITOS,IMOD_UTL_RTOS,IMOD_UTL_PRINTTEXT,IMOD_UTL_OPENASC,IMOD_UTL_SWAPSLASH,IMOD_UTL_CREATEDIR,OS,IMOD_UTL_IDATETOJDATE,IMOD_UTL_JDATETOIDATE
USE TSVAR
!USE GLOBAL,ONLY : ISSFLG
!use m_mf2005_main, only : kper
IMPLICIT NONE
! arguments
character(len=*),intent(in),pointer :: time_cstring,time_ostring
CHARACTER(LEN=*),intent(in) :: root
!integer,intent(in),dimension(:) :: issflg
INTEGER,INTENT(IN) :: ISIM,idate_save,issflg  !,kper
LOGICAL, INTENT(IN) :: LSS
DOUBLE PRECISION, INTENT(IN) :: DDATE
REAL, INTENT(IN) :: MV
LOGICAL, INTENT(IN) :: USESTS
! locals
REAL :: DH1,DH2,H,X,Y,W,Z,M,HH,WW
INTEGER :: I,II,J,JJJ,ILAY,N,IDATE,JDATE,JJ,IU,KK,IOS,JOS,IREC,MPER
CHARACTER(LEN=52) :: CLABEL,CLDATE
integer :: hour, minute, second
CHARACTER(LEN=256) :: LINE
character(len=52) :: cdate
!logical :: lfirst, lskip, lmatch
!integer :: firstdate, lastdate, mrec, iidate
!integer, dimension(:), allocatable :: ndate, mdate

IF(IIPF.EQ.0)RETURN

!## steady-state
IF(LSS)THEN

 IF(ISIM.EQ.1)THEN

  NREC = NREC + 1

  CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT('Writing Computing Heads to IPF file ...',0)

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

    LINE=TRIM(IMOD_UTL_RTOS(TS(JJ)%STVALUE(I)%X,'F',3))//','//TRIM(IMOD_UTL_RTOS(TS(JJ)%STVALUE(I)%Y,'F',3))//','// &
         TRIM(IMOD_UTL_ITOS(TS(JJ)%STVALUE(I)%ILAY))   //','//TRIM(IMOD_UTL_RTOS(TS(JJ)%STVALUE(I)%M,'F',3))//','// &
         TRIM(IMOD_UTL_RTOS(TS(JJ)%STVALUE(I)%W,'F',2))//','//TRIM(IMOD_UTL_RTOS(H,'G',7))//            ','// &
         TRIM(IMOD_UTL_RTOS(DH1,'G',7))          //','//TRIM(IMOD_UTL_RTOS(DH2,'G',7))
    WRITE(TS(JJ)%IUIPF,'(A)') TRIM(LINE)
   ENDDO
   CLOSE(TS(JJ)%IUIPF)
  ENDDO
 ENDIF

!## transient
ELSE

 !## simulation
 IF(ISIM.EQ.0)THEN

  if (issflg.eq.0 .and. associated(time_ostring)) then ! TR
   if(idate_save.eq.0)then
    cdate=time_ostring
   elseif(idate_save.eq.1)then
    cdate=time_cstring
   endif
   cdate=adjustl(cdate)
   
   read(cdate,'(i8)',iostat=ios) idate
   IF(IOS.EQ.0)THEN !CALL PRINTTEXT('iMOD can not create date for timeserie: '//TRIM(CDATE),2)
    IDATE=IMOD_UTL_IDATETOJDATE(IDATE)
   else
  
   ENDIF
   WRITE(IUIPFTXT,*) IDATE

  else ! SS
  
   cdate='steady-state'
   WRITE(IUIPFTXT,*) CDATE
  
  end if

  NREC = NREC + 1

!  call cfn_mjd2datehms(ddate,idate,hour,minute,second)
!  write(cdate,'(i8)',iostat=ios) idate
!  IF(IOS.EQ.0)THEN !CALL PRINTTEXT('iMOD can not create date for timeserie: '//TRIM(CDATE),2)
!   IDATE=IMOD_UTL_IDATETOJDATE(IDATE)
!  ENDIF

!  IF(IOS.EQ.0)WRITE(IUIPFTXT,*) IDATE
!  IF(IOS.NE.0)WRITE(IUIPFTXT,*) CDATE

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
  CLOSE(IUIPFTXT)

  LINE=TRIM(root)//CHAR(92)//'timeseries'//CHAR(92)//'timeseries_collect.txt'
  CALL IMOD_UTL_SWAPSLASH(LINE)
  CALL IMOD_UTL_OPENASC(IU,LINE,'R')

  IF(ALLOCATED(IUTXT)) DEALLOCATE(IUTXT);  IF(ALLOCATED(JUTXT)) DEALLOCATE(JUTXT)
  IF(ALLOCATED(TSDATE))DEALLOCATE(TSDATE); IF(ALLOCATED(TSNODATA))DEALLOCATE(TSNODATA)
  IF(ALLOCATED(TSM))DEALLOCATE(TSM)

  I=SUM(TS%NROWIPF)
  ALLOCATE(IUTXT(I),JUTXT(I),TSDATE(I),TSNODATA(I),TSM(I))

  !## get number of stressperiods with a date
  MPER=0
  DO irec=1,nrec !KPER=1,NPER
   READ(IU,*,IOSTAT=JOS) IDATE
   !## skip this period since it is apparently not a date
   IF(JOS.NE.0)THEN
    DO J=1,ABS(IIPF); DO I=1,TS(J)%NROWIPF
     IF(.NOT.TS(J)%STVALUE(I)%VALID)CYCLE; READ(IU,*)
    ENDDO; ENDDO
   ELSE
    MPER=MPER+1
   ENDIF
  ENDDO
  REWIND(IU)

  !## open and write header of txt files
  II=0; JJ=0
  DO J=1,ABS(IIPF)
   DO I=1,TS(J)%NROWIPF
    IF(.NOT.TS(J)%STVALUE(I)%VALID)CYCLE ! skip invalid data
    II=II+1

    WRITE(LINE,'(A,I3.3,A,A4)') TRIM(ROOT)//CHAR(92)//'timeseries'//CHAR(92)//'ipf',J,'_'//TRIM(TS(J)%STVALUE(I)%ID),'.TXT'
    !## id might contain backslash, create subfolder
    CALL IMOD_UTL_SWAPSLASH(LINE)

    IF(II.EQ.1)THEN
     IF(OS.EQ.1)THEN
      JJJ=INDEX(LINE,CHAR(92),.TRUE.)
     ELSE
      JJJ=INDEX(LINE,CHAR(47),.TRUE.)
     ENDIF
     IF(JJJ.GT.0)CALL IMOD_UTL_CREATEDIR(LINE(1:JJJ-1))
    ENDIF
    
    CALL IMOD_UTL_OPENASC(IUTXT(II),LINE,'W')
    IF(IUTXT(II).LE.0)THEN
     CALL IMOD_UTL_PRINTTEXT(' Cannot create file '//TRIM(LINE),0)
     CALL IMOD_UTL_PRINTTEXT(' Probably not enough free unit numbers',2)
    ENDIF

    WRITE(IUTXT(II),'(I10)') MPER 

    IF(TS(J)%IEXT.EQ.0)THEN
     WRITE(IUTXT(II),'(I1)') 2 
    ELSE
     WRITE(IUTXT(II),'(I1)') 3 
    ENDIF
    WRITE(IUTXT(II),'(A)') 'Date,-999'

    IF(TS(J)%IEXT.GT.0)THEN
     !## if iext.gt.0 read textfiles with measures
     IF(OS.EQ.1)THEN
      JJJ=INDEX(TS(J)%IPFNAME,CHAR(92),.TRUE.)
     ELSE
      JJJ=INDEX(TS(J)%IPFNAME,CHAR(47),.TRUE.)
     ENDIF

     LINE=TS(J)%IPFNAME(:JJJ)//TRIM(TS(J)%STVALUE(I)%ID)//'.'//TRIM(TS(J)%EXT)
     JJ=JJ+1 !; JUTXT(II)=IMOD_UTL_GETUNIT(JJ); OPEN(JUTXT(II),FILE=LINE,FORM='FORMATTED',ACTION='READ',STATUS='OLD')
     CALL IMOD_UTL_OPENASC(JUTXT(II),LINE,'R')
     
     READ(JUTXT(II),*)
     READ(JUTXT(II),*) N
     DO JJJ=1,2; READ(JUTXT(II),*) CLABEL,TSNODATA(II); ENDDO
     DO JJJ=2+1,N; READ(JUTXT(II),*) ; ENDDO
     WRITE(IUTXT(II),*) 'Measure,',TSNODATA(II) 
    ENDIF
    WRITE(IUTXT(II),*) 'Computed_Head,',MV !HNOFLOW 
   ENDDO
  ENDDO
    
  !## write summary into different *.txt files
  TSDATE=0
  DO irec=1,nrec !KPER=1,NPER

   READ(IU,*,IOSTAT=JOS) IDATE
   !## skip this period since it is apparently not a date
   IF(JOS.NE.0)THEN
    DO J=1,ABS(IIPF); DO I=1,TS(J)%NROWIPF; IF(.NOT.TS(J)%STVALUE(I)%VALID)CYCLE; READ(IU,*); ENDDO; ENDDO
    CYCLE
   ENDIF
   
   II=0
   DO JJ=1,ABS(IIPF)
    DO I=1,TS(JJ)%NROWIPF
     IF(.NOT.TS(J)%STVALUE(I)%VALID)CYCLE
     II=II+1
     
     !## without impulse/default
     READ(IU,*,IOSTAT=JOS) KK,J,H,W
     IF(JOS.NE.0)CYCLE
     IF(KK.NE.JJ)CALL IMOD_UTL_PRINTTEXT(' Something goes wrong in reading summary timeseries on line 367 in imodflow_tseries.f90',2)
     
     IF(TS(JJ)%IEXT.GT.0)THEN
      IF(TSDATE(II).EQ.IDATE)THEN
       M=TSM(II)
      !## try to read next date
      ELSEIF(TSDATE(II).LT.IDATE)THEN
       !## read until current date is found
       DO
        READ(JUTXT(II),*,IOSTAT=IOS) TSDATE(II),TSM(II)
        IF(IOS.NE.0)EXIT
        TSDATE(II)=IMOD_UTL_IDATETOJDATE(TSDATE(II))
        IF(TSDATE(II).GE.IDATE)EXIT
       ENDDO
       M=TSM(II)
       IF(TSDATE(II).NE.IDATE)M=TSNODATA(II)
      ELSE
       M=TSNODATA(II)
      ENDIF
     ENDIF
 
     IF(TS(JJ)%IEXT.GT.0)THEN
      WRITE(IUTXT(II),*) IMOD_UTL_JDATETOIDATE(IDATE),M,H
     ELSE
      WRITE(IUTXT(II),*) IMOD_UTL_JDATETOIDATE(IDATE),H
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
 ENDIF
 CLOSE(IU) 

!! Check for double data
!  if (usests) then
!     lfirst = .true.
!     firstdate = 0
!     lastdate  = 0
!     ! first determine the first and last date
!     do irec=1,nrec
!        read(iu,*,iostat=jos) idate
!        do jj=1,abs(iipf)
!          do i=1,ts(jj)%nrowipf
!             if (.not.ts(jj)%stvalue(i)%valid) cycle ! skip invalid data
!             read(iu,*)
!           enddo
!        enddo
!        if (jos.eq.0) then
!           if (lfirst) then
!              firstdate = idate
!              lfirst = .false.
!           end if
!           if (irec.eq.nrec) lastdate = idate
!        end if
!     end do
!     rewind iu
!     ! second, count the occurence of all dates
!     if (firstdate.gt.0 .and. lastdate.gt.0) then
!        allocate(ndate(lastdate-firstdate+1))
!        allocate(mdate(lastdate-firstdate+1))
!        ndate = 0; mdate = 0
!        do irec=1,nrec
!           read(iu,*,iostat=jos) idate
!           do jj=1,abs(iipf)
!             do i=1,ts(jj)%nrowipf
!                 if (.not.ts(jj)%stvalue(i)%valid) cycle ! skip invalid data
!                 read(iu,*)
!              enddo
!           enddo
!           if (jos.eq.0) then
!               ndate(idate-firstdate+1) = ndate(idate-firstdate+1) + 1
!           end if
!        end do
!        rewind iu
!     end if
!  end if
!
!  II=0
!  DO JJ=1,ABS(IIPF)
!   DO I=1,TS(JJ)%NROWIPF
!    IF (.NOT.TS(JJ)%STVALUE(I)%VALID) CYCLE ! skip invalid data
!    II=II+1
!    LINE=TRIM(TS(JJ)%STVALUE(I)%ID)
!    CALL IMOD_UTL_SWAPSLASH(LINE)
!    LINE=TRIM(ROOTRES)//CHAR(92)//'timeseries'//CHAR(92)//TRIM(LINE)//'.TXT'
!    !## id might contain backslash, create subfolder
!    CALL IMOD_UTL_SWAPSLASH(LINE)
!    IF(OS.EQ.1)THEN
!     J=INDEX(LINE,CHAR(92),.TRUE.)
!    ELSE
!     J=INDEX(LINE,CHAR(47),.TRUE.)
!    ENDIF
!
!    IF(J.GT.0)CALL IMOD_UTL_CREATEDIR(LINE(1:J-1))
!    CALL IMOD_UTL_OPENASC(IUTXT(II),LINE,'W')
!    IF(IUTXT(II).LE.0)THEN
!     CALL IMOD_UTL_PRINTTEXT('Can not create file '//TRIM(LINE),0)
!     CALL IMOD_UTL_PRINTTEXT('Probably not enough free unit numbers',2)
!    ENDIF
!    LINE=TRIM(IMOD_UTL_ITOS(NREC))
!    WRITE(IUTXT(II),'(A)') TRIM(LINE)
!    IF(TS(JJ)%IEXT.EQ.0)LINE=TRIM(IMOD_UTL_ITOS(2)); IF(TS(JJ)%IEXT.GT.0)LINE=TRIM(IMOD_UTL_ITOS(3))
!    WRITE(IUTXT(II),'(A)') TRIM(LINE)
!    WRITE(IUTXT(II),'(A)') 'Date,-999'
!
!    IF(TS(JJ)%IEXT.GT.0)THEN
!     !## if iext.gt.0 read textfiles with measures
!     IF(OS.EQ.1)THEN
!      J=INDEX(TS(JJ)%IPFNAME,CHAR(92),.TRUE.)
!     ELSE
!      J=INDEX(TS(JJ)%IPFNAME,CHAR(47),.TRUE.)
!     ENDIF
!     LINE=TS(JJ)%IPFNAME(:J)//TRIM(TS(JJ)%STVALUE(I)%ID)//'.'//TRIM(TS(JJ)%EXT)
!     CALL IMOD_UTL_SWAPSLASH(LINE)
!     CALL IMOD_UTL_OPENASC(JUTXT(II),LINE,'R')
!     IF(JUTXT(II).LE.0)CALL IMOD_UTL_PRINTTEXT('Can not open file '//TRIM(LINE),2)
!     READ(JUTXT(II),*)
!     READ(JUTXT(II),*) N
!!     IF(N.NE.2)CALL IMOD_UTL_PRINTTEXT('Number of columns in associated text file should be 2.',2)
!     DO J=1,2; READ(JUTXT(II),*) CLABEL,TSNODATA(II); ENDDO
!     DO J=2+1,N; READ(JUTXT(II),*) ; ENDDO
!     LINE='Measure,'//TRIM(IMOD_UTL_RTOS(TSNODATA(II),'G',7)); WRITE(IUTXT(II),'(A)') TRIM(LINE)
!    ENDIF
!    LINE='Computed_Head,'//TRIM(IMOD_UTL_RTOS(MV,'G',7)); WRITE(IUTXT(II),'(A)') TRIM(LINE)
!
!   ENDDO
!  ENDDO
!
!!  ## write summary into different *.txt files
!  TSDATE=0
!  lmatch = .true.
!  DO IREC=1,NREC
!
!   READ(IU,*,IOSTAT=JOS) IDATE
!!   ## skip this period since it is apparently not a date
!   lskip = .false.
!   if (jos.ne.0) lskip = .true.
!   if (usests.and..not.lskip) then
!      mdate(idate-firstdate+1) = mdate(idate-firstdate+1) + 1
!      if (lmatch) then
!         if (mdate(idate-firstdate+1).ne.ndate(idate-firstdate+1)) then
!            lmatch = .false.
!            iidate = idate
!         end if
!      else
!         if (iidate.eq.idate .and. mdate(idate-firstdate+1).eq.ndate(idate-firstdate+1)) then
!            lmatch = .true.
!         end if
!      end if
!      if(.not.lmatch) lskip = .true.
!   end if
!   IF(lskip)THEN
!    DO JJ=1,ABS(IIPF)
!       DO I=1,TS(JJ)%NROWIPF
!          IF (.NOT.TS(JJ)%STVALUE(I)%VALID) CYCLE ! skip invalid data
!          READ(IU,*)
!       ENDDO
!    ENDDO
!    CYCLE
!   ENDIF
!
!   II=0
!   DO JJ=1,ABS(IIPF)
!    DO I=1,TS(JJ)%NROWIPF
!     IF (.NOT.TS(JJ)%STVALUE(I)%VALID) CYCLE ! skip invalid data
!     II=II+1
!
!!     ## without impulse/default
!     READ(IU,'(A)') LINE
!     READ(LINE,*,IOSTAT=JOS) JDATE,KK,J,H,W
!     IF(JOS.NE.0) THEN
!        READ(LINE,*,IOSTAT=JOS) KK,J,H,W
!     END IF
!     IF(JOS.NE.0)CYCLE
!     IF(KK.NE.JJ)CALL IMOD_UTL_PRINTTEXT('Something goes wrong in reading summary timeseries online 340 in imod_tseries.f90',2)
!
!     IF(TS(JJ)%IEXT.GT.0)THEN
!      IF(TSDATE(II).EQ.IDATE)THEN
!       M=TSM(II)
!!      ## try to read next date
!      ELSEIF(TSDATE(II).LT.IDATE)THEN
!!       ## read until current date is found
!       DO
!        READ(JUTXT(II),*,IOSTAT=IOS) CLDATE,TSM(II)
!        IF(IOS.NE.0)EXIT
!        READ(CLDATE,'(I8)',IOSTAT=IOS) TSDATE(II)
!        IF(IOS.NE.0)EXIT
!        TSDATE(II)=IMOD_UTL_IDATETOJDATE(TSDATE(II))
!        IF(TSDATE(II).GE.IDATE)EXIT
!       ENDDO
!       M=TSM(II)
!       IF(TSDATE(II).NE.IDATE)M=TSNODATA(II)
!      ELSE
!       M=TSNODATA(II)
!      ENDIF
!     ENDIF
!
!     LINE=TRIM(IMOD_UTL_ITOS(IMOD_UTL_JDATETOIDATE(IDATE)))
!     IF(TS(JJ)%IEXT.GT.0)LINE=TRIM(LINE)//','//TRIM(IMOD_UTL_RTOS(M,'G',7))
!     LINE=TRIM(LINE)//','//TRIM(IMOD_UTL_RTOS(H,'G',7))
!     WRITE(IUTXT(II),'(A)') TRIM(LINE)
!    ENDDO
!   ENDDO
!  ENDDO
!  II=0
!  DO JJ=1,ABS(IIPF)
!   DO I=1,TS(JJ)%NROWIPF
!     IF (.NOT.TS(JJ)%STVALUE(I)%VALID) CYCLE ! skip invalid data
!     II=II+1
!     CLOSE(IUTXT(II))
!   ENDDO
!  ENDDO
!
!  IF(ALLOCATED(IUTXT)) DEALLOCATE(IUTXT);  IF(ALLOCATED(JUTXT)) DEALLOCATE(JUTXT)
!  IF(ALLOCATED(TSDATE))DEALLOCATE(TSDATE); IF(ALLOCATED(TSNODATA))DEALLOCATE(TSNODATA)
!  IF(ALLOCATED(TSM))DEALLOCATE(TSM)
! ENDIF
! CLOSE(IU) !,STATUS='DELETE')

ENDIF
!
!if (allocated(ndate)) deallocate(ndate)
!if (allocated(mdate)) deallocate(mdate)

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

CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT('Cleaning data for writing timeseries...',0)

IF(ALLOCATED(IUTXT))THEN
 DO I=1,SIZE(IUTXT) !TS(JJ)%NROWIPF
  IF(IUTXT(I).GT.0)THEN
   INQUIRE(UNIT=IUTXT(I),OPENED=LEX); IF(LEX)CLOSE(IUTXT(I))
  ENDIF
 ENDDO
 DEALLOCATE(IUTXT)
ENDIF

DO JJ = 1, ABS(IIPF)
  IF (ASSOCIATED(TS(JJ)%STVALUE)) DEALLOCATE(TS(JJ)%STVALUE)
END DO

RETURN
END SUBROUTINE