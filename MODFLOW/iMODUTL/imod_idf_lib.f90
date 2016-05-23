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

MODULE IMOD_IDF

USE IMOD_UTL
USE IMOD_IDF_PAR

CHARACTER(LEN=2),PARAMETER :: NEWLINE=CHAR(13)//CHAR(10)
REAL,PARAMETER,PRIVATE :: TINY=0.1

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION IDFREAD(IDF,IDFNAME,RDDATA)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(OUT) :: IDF
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 INTEGER,INTENT(IN),OPTIONAL :: RDDATA
 CHARACTER(LEN=2) :: TXT
 INTEGER :: IOPEN,IDATA

 IF(PRESENT(RDDATA))IDATA=RDDATA

 IDFREAD=.FALSE.

 TXT='RO'                   !## read only
 IF(IDATA.EQ.-1)TXT='RW'    !## read/write

 SELECT CASE (IDATA)
  !## direct access
  CASE (0,-1)
   IOPEN=0
  !# stream access
  CASE (1)
   IOPEN=1
 END SELECT

 !## open idf
 IF(IDFOPEN(IDF%IU,IDFNAME,TXT,IOPEN))THEN
  IF(IDATA.NE.1)IDF%IXV=0  !## initialize %ixv in case no data is read from idf
  CALL IDFNULLIFY(IDF)
  IF(IDFREADDIM(IOPEN,IDF))THEN
   IDF%FNAME=IDFNAME
   IF(IDFREADDATA(IOPEN,IDF))THEN
    !## get gregorian-date if possible
    IDF%JD=UTL_IDFGETDATE(IDFNAME)
    !## get julian-date if possible
    IF(IDF%JD.NE.0)THEN
     IDF%JD=IDATETOJDATE(IDF%JD)
    ENDIF
    CALL IDFGETILAY(IDF,IDFNAME)
    CALL IDFGETCOMMENT(IDF,IOPEN)  !!IDF%IADIT=0 !## nothing found
    IDFREAD=.TRUE.
   ELSE
    WRITE(*,*) 'Error occured reading DATA from IDF'//CHAR(13)//TRIM(IDFNAME)
   ENDIF
  ELSE
   WRITE(*,*) 'Error occured reading DIMENSIONS from IDF'//CHAR(13)//TRIM(IDFNAME)
  ENDIF
 ENDIF

 !## if stream access, close file
 IF(IOPEN.EQ.1)CLOSE(IDF%IU)

 END FUNCTION IDFREAD

 !###======================================================================
 LOGICAL FUNCTION ASCREAD(IDF,IDFNAME)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(OUT) :: IDF
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 CHARACTER(LEN=2) :: TXT
 INTEGER :: IOS

 ASCREAD=.FALSE.

 !## open idf
 IDF%IU=GETUNIT()
 OPEN(IDF%IU,FILE=IDFNAME,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  WRITE(*,*) 'Can not open '//TRIM(IDFNAME)
  RETURN
 ENDIF
 IDF%IXV=0  !## initialize %ixv in case no data is read from idf
 CALL IDFNULLIFY(IDF)
 IF(ASCREADDATA(IDF))THEN
  IDF%FNAME=IDFNAME
  !## get gregorian-date if possible
  IDF%JD=UTL_IDFGETDATE(IDFNAME)
  !## get julian-date if possible
  IF(IDF%JD.NE.0)THEN
   IDF%JD=IDATETOJDATE(IDF%JD)
  ENDIF
  CALL IDFGETILAY(IDF,IDFNAME)
  ASCREAD=.TRUE.
 ELSE
  WRITE(*,*) 'Error occured reading DATA from ASC'//CHAR(13)//TRIM(IDFNAME)
 ENDIF

 CLOSE(IDF%IU)

 END FUNCTION ASCREAD

 !###======================================================================
 LOGICAL FUNCTION IDFREADPART(IDF,XMIN,YMIN,XMAX,YMAX)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 INTEGER :: IR,IR1,IR2,IC,IC1,IC2,IROW,ICOL,NROW,NCOL

 IDFREADPART=.FALSE.

 !## get position to be read from idf inside current view-extent
 CALL IDFIROWICOL(IDF,IR1,IC1,XMIN,YMAX)
 CALL IDFIROWICOL(IDF,IR2,IC2,XMAX,YMIN)
 !## adjust ic1,ic2
 IF(IDF%XMIN.GE.XMIN)IC1=1
 IF(IDF%XMAX.LE.XMAX)IC2=IDF%NCOL
 !## adjust ir1,ir2
 IF(IDF%YMIN.GE.YMIN)IR2=IDF%NROW
 IF(IDF%YMAX.LE.YMAX)IR1=1

 IF(IR1.NE.0.AND.IR2.NE.0.AND.IC1.NE.0.AND.IC2.NE.0)THEN
  NROW=IR2-IR1+1
  NCOL=IC2-IC1+1
  ALLOCATE(IDF%X(NCOL,NROW))
  IROW=0
  DO IR=IR1,IR2
   IROW=IROW+1
   ICOL=0
   DO IC=IC1,IC2
    ICOL=ICOL+1
    IDF%X(ICOL,IROW)=IDFGETVAL(IDF,IR,IC)
   END DO
  END DO
  !## overrule current dimensions of idf()
  IDF%NROW=NROW
  IDF%NCOL=NCOL
  IF(IDF%IEQ.EQ.0)THEN
   IDF%XMAX=IDF%XMIN+ IC2   *IDF%DX
   IDF%XMIN=IDF%XMIN+(IC1-1)*IDF%DX
   IDF%YMIN=IDF%YMAX- IR2   *IDF%DY
   IDF%YMAX=IDF%YMAX-(IR1-1)*IDF%DY
  ELSE
   !## shift coordinates
   IDF%SX(0:IDF%NCOL)=IDF%SX(IC1-1:IC2)
   IDF%SY(0:IDF%NROW)=IDF%SY(IR1-1:IR2)
   IDF%XMIN=IDF%SX(0)
   IDF%XMAX=IDF%SX(IDF%NCOL)
   IDF%YMIN=IDF%SY(IDF%NROW)
   IDF%YMAX=IDF%SY(0)
  ENDIF

  IDFREADPART=.TRUE.

 ENDIF

 END FUNCTION IDFREADPART

 !###======================================================================
 LOGICAL FUNCTION IDFREADSCALE(IDFC,IDFM,SCLTYPE,ISMOOTH,PERC)
 ! IDFM = mother idf and will return values on grid defined by IDFM
 ! IDFC = child  idf and uses grid defined by IDFC to scale on IDFM
 ! scltype:
 ! 1 = SPECIAAL (IBOUNDARY)
 ! 2 = REKENKUNDIG (SHEAD/VCONT/S)
 ! 3 = GEOMETRISCH (KD)
 ! 4 = SUM(Q)
 ! 5 = SUM(COND)*RATIO (RIV/DRN/GHB CONDUCTANCE; RCH MM/DAY)
 ! 6 = INVERSE (C)
 ! 7 = MOST FREQUENT OCCURENCE
 ! 8 = SUM (1/c)*RATIO
 ! 9 = PERCENTILE
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDFC,IDFM
 INTEGER,INTENT(IN) :: SCLTYPE
 INTEGER,INTENT(IN) :: ISMOOTH
 REAL,INTENT(IN),OPTIONAL :: PERC
 INTEGER :: IRC,IRC1,IRC2,ICC,ICC1,ICC2,IRM,ICM,MXN,MXM,N,M,I,IINT,IDOWN
 REAL,ALLOCATABLE,DIMENSION(:) :: FREQ
 REAL :: SVALUE,SFCT,DXM,DYM,DXC,DYC
 logical :: flag1, flag2
 DOUBLE PRECISION :: XD1,XD2,YD1,YD2,TINY
 CHARACTER(LEN=256) :: IDFNAME

 !## check extent
 IF (IDFC%XMIN.GT.IDFM%XMIN.OR.IDFC%XMAX.LT.IDFM%XMAX.OR.IDFC%YMIN.GT.IDFM%YMIN.OR.IDFC%YMAX.LT.IDFM%YMAX) THEN
  INQUIRE(UNIT=IDFC%IU,NAME=IDFNAME)
  CALL IMOD_UTL_PRINTTEXT('=======================================',0)
  CALL IMOD_UTL_PRINTTEXT('Error!',0)
  CALL IMOD_UTL_PRINTTEXT('File: '//TRIM(IDFNAME),0)
  CALL IMOD_UTL_PRINTTEXT('Undersizes current model dimensions!',0)
  IF(IDFC%XMIN.GT.IDFM%XMIN)THEN
   CALL IMOD_UTL_PRINTTEXT('XMIN IDF '//TRIM(IMOD_UTL_RTOS(IDFC%XMIN,'F',2))//' > XMIN MODEL '//TRIM(IMOD_UTL_RTOS(IDFM%XMIN,'F',2)),0)
  ENDIF
  IF(IDFC%XMAX.LT.IDFM%XMAX)THEN
   CALL IMOD_UTL_PRINTTEXT('XMAX IDF '//TRIM(IMOD_UTL_RTOS(IDFC%XMAX,'F',2))//' < XMAX MODEL '//TRIM(IMOD_UTL_RTOS(IDFM%XMAX,'F',2)),0)
  ENDIF
  IF(IDFC%YMIN.GT.IDFM%YMIN)THEN
   CALL IMOD_UTL_PRINTTEXT('YMIN IDF '//TRIM(IMOD_UTL_RTOS(IDFC%YMIN,'F',2))//' > YMIN MODEL '//TRIM(IMOD_UTL_RTOS(IDFM%YMIN,'F',2)),0)
  ENDIF
  IF(IDFC%YMAX.LT.IDFM%YMAX)THEN
   CALL IMOD_UTL_PRINTTEXT('YMAX IDF '//TRIM(IMOD_UTL_RTOS(IDFC%YMAX,'F',2))//' < YMAX MODEL '//TRIM(IMOD_UTL_RTOS(IDFM%YMAX,'F',2)),0)
  ENDIF
  CALL IMOD_UTL_PRINTTEXT('=======================================',0)
  CALL IMOD_UTL_PRINTTEXT('Error',2)
 ENDIF

 !## check for valid scaling options
 SELECT CASE(SCLTYPE)
  CASE(1,2,3,4,5,6,7,8,9)
  CASE DEFAULT
   CALL IMOD_UTL_PRINTTEXT('Error!',0)
   CALL IMOD_UTL_PRINTTEXT('File: '//TRIM(IDFNAME),0)
   CALL IMOD_UTL_PRINTTEXT('Missing upscaling method',2)
 END SELECT
 SELECT CASE(ISMOOTH)
  CASE(0,1)
  CASE DEFAULT
   CALL IMOD_UTL_PRINTTEXT('Error!',0)
   CALL IMOD_UTL_PRINTTEXT('File: '//TRIM(IDFNAME),0)
   CALL IMOD_UTL_PRINTTEXT('Missing downscaling method',2)
 END SELECT

  flag2 = .false.

 IDFREADSCALE=.FALSE.

 SFCT=50.0
 IF(PRESENT(PERC))SFCT=PERC
 IINT=4
 IDOWN=0

 !## check whether MOTHER array is allocated, otherwise allocate it
 if (.not.associated(idfm%x)) then
  IF(.NOT.IDFALLOCATEX(IDFM))RETURN

  !## clean array
  IDFM%X=IDFC%NODATA
  flag1 = .false.
 else
  flag1 = .true.
 end if

 !## construct sx/sy arrays for child/mother (if not yet existing)
 IF(.NOT.IDFFILLSXSY(IDFC))RETURN
 IF(.NOT.IDFFILLSXSY(IDFM))RETURN

 !## most-frequent,percentiles
 MXN=1;MXM=1
 IF(SCLTYPE.EQ.7.OR.SCLTYPE.EQ.9)THEN
  MXN=0; DO I=1,IDFM%NCOL; N=(IDFM%SX(I)-IDFM%SX(I-1))/IDFC%DX; MXN=MAX(MXN,N); END DO
  MXN=MXN+2
  MXM=0; DO I=1,IDFM%NROW; M=(IDFM%SY(I-1)-IDFM%SY(I))/IDFC%DY; MXM=MAX(MXM,M); END DO
  MXM=MXM+2
 ENDIF
 ALLOCATE(FREQ(MXN*MXM))

 !## read/scale parameters
 DO IRM=1,IDFM%NROW

  !## get location to scale/cut data from IDFC
  TINY=MIN(1.0,0.01*(IDFM%SY(IRM-1)-IDFM%SY(IRM)))
  YD2=REAL(IDFM%SY(IRM-1),8)-TINY
  YD1=REAL(IDFM%SY(IRM  ),8)+TINY
  CALL IMOD_UTL_POL1LOCATED(IDFC%SY,IDFC%NROW+1,YD2,IRC1)
  CALL IMOD_UTL_POL1LOCATED(IDFC%SY,IDFC%NROW+1,YD1,IRC2)

  IF(IRC2.GE.IRC1.AND.IRC2.LE.IDFC%NROW.AND.IRC1.NE.0.AND.IRC2.NE.0)THEN

   DO ICM=1,IDFM%NCOL

    !## get location to scale/cut data from IDFC
    TINY=MIN(1.0,0.01*(IDFM%SX(ICM)-IDFM%SX(ICM-1)))
    XD1=REAL(IDFM%SX(ICM-1),8)+TINY
    XD2=REAL(IDFM%SX(ICM  ),8)-TINY
    CALL IMOD_UTL_POL1LOCATED(IDFC%SX,IDFC%NCOL+1,XD1,ICC1)
    CALL IMOD_UTL_POL1LOCATED(IDFC%SX,IDFC%NCOL+1,XD2,ICC2)
    IF(ICC2.GE.ICC1.AND.ICC2.LE.IDFC%NCOL.AND.ICC1.NE.0.AND.ICC2.NE.0)THEN

    DYM=IDFM%SY(IRM-1) -IDFM%SY(IRM)
    DYC=IDFC%SY(IRC1-1)-IDFC%SY(IRC2)

    !## get window to scale/cut data from IDFC

     if (flag1) then ! only read for indices specified with idfm%x
        if(idfm%x(icm,irm).ne.idfc%nodata) then
           flag2 = .true.
        else
           flag2 = .false.
        end if
     else ! read all data
        flag2 = .true.
     end if

     if (flag2) then

      DXM=IDFM%SX(ICM) -IDFM%SX(ICM-1)
      DXC=IDFC%SX(ICC2)-IDFC%SX(ICC1-1)

      CALL IDFGETBLOCKVALUE(IDFC,SCLTYPE,IRC1,IRC2,ICC1,ICC2,FREQ,SFCT,SVALUE)

      !## up- or downscaling?
      IF(DXC*DYC.GT.DXM*DYM)THEN
       IDOWN=1
       IF(SCLTYPE.EQ.5.AND.SVALUE.NE.IDFC%NODATA)SVALUE=SVALUE*(DXM*DYM)/(DXC*DYC)
      ENDIF

      IF(SVALUE.EQ.IDFC%NODATA) SVALUE = IDFM%NODATA
      IDFM%X(ICM,IRM)=SVALUE ! set value in mother

     end if ! flag2

    ENDIF
   ENDDO
  ENDIF
 END DO

 !## smooth only if cs.gt.dx
 IF(IDOWN.EQ.1.AND.ISMOOTH.EQ.1)CALL IDFSMOOTH(IDFC,IDFM,IINT)

 IDFREADSCALE=.TRUE.

 END FUNCTION IDFREADSCALE

 !###====================================================================
 LOGICAL FUNCTION IDFFILLSXSY(IDF)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: I

 IDFFILLSXSY=.TRUE.

 !## allready filled in
 IF(IDF%IEQ.EQ.1)RETURN

 IDFFILLSXSY=.FALSE.
 IF(.NOT.IDFALLOCATESXY(IDF))RETURN

 IDF%SX(0)=IDF%XMIN
 DO I=1,IDF%NCOL; IDF%SX(I)=IDF%SX(I-1)+IDF%DX; END DO
 IDF%SY(0)=IDF%YMAX
 DO I=1,IDF%NROW; IDF%SY(I)=IDF%SY(I-1)-IDF%DY; END DO

 IDFFILLSXSY=.TRUE.

 END FUNCTION IDFFILLSXSY

 !###====================================================================
 SUBROUTINE IDFGETBLOCKVALUE(IDF,SCLTYPE,IR1,IR2,IC1,IC2,FREQ,SFCT,SVALUE)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 REAL,INTENT(OUT),DIMENSION(:) :: FREQ
 REAL,INTENT(IN) :: SFCT
 INTEGER,INTENT(IN) :: IR1,IR2,IC1,IC2,SCLTYPE
 REAL,INTENT(OUT) :: SVALUE
 INTEGER :: I,IROW,ICOL,NAJ,IR,IC,NROW,NCOL,NLAY
 REAL :: IDFVAL,NVALUE,NFRAC
 REAL,DIMENSION(1) :: XTEMP

 SVALUE=0.0 !## scale value
 NVALUE=0.0
 SELECT CASE (SCLTYPE)
  CASE (7,9)
   FREQ=0.0
 END SELECT

 IR=0
 DO IROW=IR1,IR2
  IR=IR+1
  IC=0
  DO ICOL=IC1,IC2
   IC=IC+1

   IDFVAL=IDFGETVAL(IDF,IROW,ICOL)

   SELECT CASE (SCLTYPE)
    !## special for boundary purposes
    CASE (1)
     IF(IDFVAL.LT.0)SVALUE=IDFVAL
     IF(SVALUE.EQ.0.AND.IDFVAL.GT.0)SVALUE=IDFVAL
     NVALUE=NVALUE+1.0
    !## arithmetic mean (HEAD/SC); sum
    CASE (2,4,5)
     IF(IDFVAL.NE.IDF%NODATA)THEN
      SVALUE=SVALUE+IDFVAL
      NVALUE=NVALUE+1.0
     ENDIF
    !## geometric mean (KD)
    CASE (3)
     IF(IDFVAL.NE.IDF%NODATA.AND.IDFVAL.GT.0.0)THEN
      SVALUE=SVALUE+LOG(IDFVAL)
      NVALUE=NVALUE+1.0
     ENDIF
    !## sum, sum inverse
    CASE (6,8)
     IF(IDFVAL.NE.IDF%NODATA.AND.IDFVAL.NE.0.0)THEN
      SVALUE=SVALUE+(1.0/IDFVAL)
      NVALUE=NVALUE+1.0
     ENDIF
    !## most frequent occurence,percentile
    CASE (7,9)
     IF(IDFVAL.NE.IDF%NODATA)THEN
      NVALUE=NVALUE+1.0
      FREQ(INT(NVALUE))=IDFVAL
     ENDIF
    CASE DEFAULT
     WRITE(*,'(//A//)') 'Scaling not known for: '//TRIM(IDF%FNAME)
   END SELECT
  ENDDO
 ENDDO

 IF(NVALUE.LE.0.0)THEN
  SVALUE=IDF%NODATA
  RETURN
 ENDIF

 SELECT CASE (SCLTYPE)
  CASE (1,4)!## boundary, sum

  CASE (2)  !## arithmetic mean
   SVALUE=SVALUE/NVALUE
  CASE (3)  !## geometric
   SVALUE=EXP(SVALUE/NVALUE)
  CASE (6)  !## c-waarde reciprook opgeteld, terug naar gem. dagen
   SVALUE=1.0/(SVALUE/NVALUE)
  CASE (7)
   CALL IMOD_UTL_QKSORT(SIZE(FREQ),INT(NVALUE),FREQ)
   SVALUE=IMOD_UTL_GETMOSTFREQ(FREQ,SIZE(FREQ),INT(NVALUE))
   !## add fraction to the most frequent occurence
   NFRAC=NVALUE/REAL(((IR2-IR1)+1)*((IC2-IC1)+1))
   SVALUE=SVALUE+(1.0-NFRAC)
  CASE (8)  !## PWT c-waarde reciprook opgeteld, terug naar gem. dagen * fraction
   NFRAC=NVALUE/REAL(((IR2-IR1)+1)*((IC2-IC1)+1))
   SVALUE=1.0/((SVALUE*NFRAC)/NVALUE)
  CASE (9)  !## percentile
   CALL IMOD_UTL_GETMED(FREQ,SIZE(FREQ),IDF%NODATA,(/SFCT*100.0/),1,NAJ,XTEMP)
   SVALUE=XTEMP(1)
 END SELECT

 ! 1 = SPECIAAL (IBOUNDARY)
 ! 2 = REKENKUNDIG (SHEAD/VCONT/S)
 ! 3 = GEOMETRISCH (KD)
 ! 4 = SUM(Q)
 ! 5 = SUM(COND)*RATIO (RIV/DRN/GHB CONDUCTANCE; RCH MM/DAY)
 ! 6 = INVERSE (c)
 ! 7 = MOST FREQUENT OCCURENCE
 ! 8 = SUM (1/c)*RATIO
 ! 9 = PERCENTILE

 END SUBROUTINE IDFGETBLOCKVALUE

 !###====================================================================
 SUBROUTINE IDFSMOOTH(IDFC,IDFM,IINT)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDFC,IDFM
 INTEGER,INTENT(IN) :: IINT

 DOUBLE PRECISION, PARAMETER :: DTINY = 0.1D0

 INTEGER :: IC,IR,IC1,IC2,IR1,IR2,NPC,NPR,ICOL,IROW,I,J,IY,II,JJ
 REAL :: XC,XX,Y,YY,YC,XCIDF,YCIDF,XMID,YMID
 REAL,ALLOCATABLE,DIMENSION(:) :: X1A,X2A
 REAL,ALLOCATABLE,DIMENSION(:,:) :: Y2A

 IF(ALLOCATED(X1A)) DEALLOCATE(X1A)
 IF(ALLOCATED(X2A)) DEALLOCATE(X2A)
 IF(ALLOCATED(Y2A)) DEALLOCATE(Y2A)

 !## find number of x- y locations of grid to be interpolated
 CALL IMOD_UTL_POL1LOCATED(IDFC%SX,IDFC%NCOL+1,DBLE(IDFM%SX(0))+DTINY,IC1)
 CALL IMOD_UTL_POL1LOCATED(IDFC%SX,IDFC%NCOL+1,DBLE(IDFM%SX(IDFM%NCOL))-DTINY,IC2)
 CALL IMOD_UTL_POL1LOCATED(IDFC%SY,IDFC%NROW+1,DBLE(IDFM%SY(0))-DTINY,IR1)
 CALL IMOD_UTL_POL1LOCATED(IDFC%SY,IDFC%NROW+1,DBLE(IDFM%SY(IDFM%NROW))+DTINY,IR2)

 !## number of distinguished coordinates from child idf (coarser)
 NPC=(IC2-IC1)+1
 NPR=(IR2-IR1)+1
 !## add extra for boundary (north/west/east/south)
 NPR=NPR+2
 NPC=NPC+2

 !## assign one extra row/column for boundary
 ALLOCATE(X1A(NPC),X2A(NPR),Y2A(NPC,NPR))

 NPR=1
 DO IR=IR1,IR2  !## loop over row in coarser child
  NPC=1
  NPR=NPR+1
  YMID=(IDFC%SY(IR-1)+IDFC%SY(IR))/2.0
  X2A(NPR)=YMID
  DO IC=IC1,IC2 !## loop over col in coarser child idf
   NPC=NPC+1
   XMID=(IDFC%SX(IC-1)+IDFC%SX(IC))/2.0
   X1A(NPC)=XMID
   !## read value from idfm%x()
   CALL IDFIROWICOL(IDFM,IROW,ICOL,X1A(NPC),X2A(NPR))
   Y2A(NPC,NPR)=IDFM%X(ICOL,IROW)
  ENDDO
 ENDDO

 NPC=NPC+1
 NPR=NPR+1

 X1A(1)  =IDFC%SX(IC1-1)
 X1A(NPC)=IDFC%SX(IC2)
 X2A(1)  =IDFC%SY(IR1-1)
 X2A(NPR)=IDFC%SY(IR2)

 Y2A(1,:)  =Y2A(2,:)
 Y2A(NPC,:)=Y2A(NPC-1,:)
 Y2A(:,1)  =Y2A(:,2)
 Y2A(:,NPR)=Y2A(:,NPR-1)

 CALL IMOD_UTL_POL1INTMAIN(IDFM%NCOL,IDFM%NROW,NPC,NPR,X1A,X2A,Y2A,IDFM%SX,IDFM%SY,IDFM%X,IINT,IDFM%NODATA)

 IF(ALLOCATED(X1A))DEALLOCATE(X1A)
 IF(ALLOCATED(X2A))DEALLOCATE(X2A)
 IF(ALLOCATED(Y2A))DEALLOCATE(Y2A)

 END SUBROUTINE IDFSMOOTH

 !###======================================================================
 LOGICAL FUNCTION IDFWRITEPART(IDF1,IDF2)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF1,IDF2  !## write idf1 in idf2
 INTEGER :: IR,IR1,IR2,IC,IC1,IC2,IROW,ICOL,NROW,NCOL
 REAL :: X,Y

 IDFWRITEPART=.FALSE.

 !## get position to be write idf to
 IF(IDF1%IEQ.EQ.0)THEN
  X=IDF1%XMIN+IDF1%DX/2.0
  Y=IDF1%YMAX-IDF1%DY/2.0
 ELSE
  X=(IDF1%SX(0)+IDF1%SX(1))/2.0
  Y=(IDF1%SY(0)+IDF1%SY(1))/2.0
 ENDIF
 CALL IDFIROWICOL(IDF2,IR1,IC1,X,Y)!IDF1%XMIN,IDF1%YMAX)
 IF(IDF1%IEQ.EQ.0)THEN
  X=IDF1%XMAX-IDF1%DX/2.0
  Y=IDF1%YMIN+IDF1%DY/2.0
 ELSE
  X=(IDF1%SX(IDF1%NCOL)+IDF1%SX(IDF1%NCOL-1))/2.0
  Y=(IDF1%SY(IDF1%NROW)+IDF1%SY(IDF1%NROW-1))/2.0
 ENDIF
 CALL IDFIROWICOL(IDF2,IR2,IC2,X,Y)!IDF1%XMAX,IDF1%YMIN)

 IF(IR1.NE.0.AND.IR2.NE.0.AND.IC1.NE.0.AND.IC2.NE.0)THEN

  NROW=IR2-IR1+1
  NCOL=IC2-IC1+1

  !## wrong dimensions
  IF(NROW.NE.IDF1%NROW.OR.NCOL.NE.IDF1%NCOL)RETURN

  IROW=0
  DO IR=IR1,IR2
   IROW=IROW+1
   ICOL=0
   DO IC=IC1,IC2
    ICOL=ICOL+1
    CALL IDFPUTVAL(IDF2,IR,IC,IDF1%X(ICOL,IROW))
    IF(IDF1%X(ICOL,IROW).NE.IDF2%NODATA)THEN
     IDF2%DMIN=MIN(IDF1%X(ICOL,IROW),IDF2%DMIN)
     IDF2%DMAX=MAX(IDF1%X(ICOL,IROW),IDF2%DMAX)
    ENDIF
   END DO
  END DO

  WRITE(IDF2%IU,REC=7+ICF) IDF2%DMIN
  WRITE(IDF2%IU,REC=8+ICF) IDF2%DMAX

  IDFWRITEPART=.TRUE.

 ENDIF

 END FUNCTION IDFWRITEPART

 !###======================================================================
 LOGICAL FUNCTION IDFWRITE_WRAPPER(NCOL,NROW,X,DX,DY,XMIN,YMIN,NODATA,COMMENT,FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW
 REAL,INTENT(IN),DIMENSION(NCOL,NROW) :: X
 REAL,DIMENSION(:),INTENT(IN) :: DX,DY
 REAL,INTENT(IN) :: XMIN,YMIN,NODATA
 CHARACTER(LEN=*),INTENT(IN) :: FNAME,COMMENT
 TYPE(IDFOBJ) :: IDF
 INTEGER :: I
 LOGICAL :: LEX

 IDFWRITE_WRAPPER=.FALSE.

 CALL IDFNULLIFY(IDF)
 IDF%XMIN  =XMIN
 IDF%YMIN  =YMIN
 IDF%NCOL  =NCOL
 IDF%NROW  =NROW
 IDF%IXV   =0
 IDF%ITB   =0
 IDF%UNITS =0
 IDF%NODATA=NODATA

 IF(SIZE(DX).EQ.IDF%NCOL.AND.SIZE(DY).EQ.IDF%NROW)THEN
  IF(.NOT.IDFALLOCATESXY(IDF))RETURN
  IDF%IEQ =1
  IDF%XMAX=IDF%XMIN+SUM(DX)
  IDF%SX(0)=IDF%XMIN
  DO I=1,IDF%NCOL; IDF%SX(I)=IDF%SX(I-1)+DX(I); ENDDO
  IDF%YMAX =IDF%YMIN+SUM(DY)
  IDF%SY(0)=IDF%YMAX
  DO I=1,IDF%NROW; IDF%SY(I)=IDF%SY(I-1)-DY(I); ENDDO
 ELSEIF(SIZE(DX).EQ.1.AND.SIZE(DY).EQ.1)THEN
  IDF%IEQ =0
  IDF%DX  =DX(1)
  IDF%DY  =DY(1)
  IDF%XMAX=IDF%XMIN+REAL(IDF%NCOL)*DX(1)
  IDF%YMAX=IDF%YMIN+REAL(IDF%NROW)*DY(1)
 ELSE
  WRITE(*,*) 'ERROR, check array dx(.) and dy(.) to be consistent with ncol and nrow'
  RETURN
 ENDIF

 IF(.NOT.IDFALLOCATEX(IDF))RETURN
 IDF%X=X

 CALL IDFFILLCOMMENT(IDF,COMMENT)

 i = index(fname,char(47),.true.)
 if (i.le.0) i = index(fname,char(92),.true.)
 if(i.gt.0) call imod_utl_createdir(fname(:i))
 IDFWRITE_WRAPPER=IDFWRITE(IDF,FNAME,0)

 CALL IDFDEALLOCATEX(IDF)
 IF(IDF%IU.GT.0)THEN
  INQUIRE(UNIT=IDF%IU,OPENED=LEX); IF(LEX)CLOSE(IDF%IU)
 ENDIF

 END FUNCTION IDFWRITE_WRAPPER

 !###======================================================================
 LOGICAL FUNCTION IDFWRITE(IDF,IDFNAME,IQ)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IQ  !question to overwrite yes=1;no=0
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 INTEGER :: IDATA
 CHARACTER(LEN=2) :: TXT

 IDFWRITE=.FALSE.

 IDATA=1
 TXT  ='WO'  !## write only

 !##open idf
 IF(IDFOPEN(IDF%IU,IDFNAME,TXT,IDATA,IQUESTION=IQ))THEN
  IF(IDFWRITEDIM(IDATA,IDF).AND.IDFWRITEDATA(IDATA,IDF))THEN
   IDFWRITE=.TRUE.
   !## try to write comment, if available
   CALL IDFWRITECOMMENT(IDF,IDATA)
  ENDIF
 ENDIF

 IF(IDF%IU.NE.0)CLOSE(IDF%IU)
 IDF%IU=0

 END FUNCTION IDFWRITE

 !###====================================================================
 LOGICAL FUNCTION IDFWRITE_EQUI(IDF,IDFNAME)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 INTEGER :: SNCOL,SNROW,IROW,ICOL,IR,IC,NR,NC
 REAL :: X,CS,DX,DY

 IDFWRITE_EQUI=.FALSE.

 !## open idf
 IF(.NOT.IDFOPEN(IDF%IU,IDFNAME,'WO',1,0))RETURN

 CS=IDF%XMAX-IDF%XMIN
 DO ICOL=1,IDF%NCOL
  CS=MIN(CS,IDF%SX(ICOL)-IDF%SX(ICOL-1))
 END DO

 SNCOL=(IDF%XMAX-IDF%XMIN)/CS
 SNROW=(IDF%YMAX-IDF%YMIN)/CS

 IDF%DMIN= 10.0E10
 IDF%DMAX=-10.0E10
 DO IROW=1,IDF%NROW
  DO ICOL=1,IDF%NCOL
   IF(IDF%X(ICOL,IROW).NE.IDF%NODATA)THEN
    IDF%DMIN=MIN(IDF%DMIN,IDF%X(ICOL,IROW))
    IDF%DMAX=MAX(IDF%DMAX,IDF%X(ICOL,IROW))
   ENDIF
  END DO
 END DO

 WRITE(IDF%IU) 1271       !header: 1271
 WRITE(IDF%IU) SNCOL      !1
 WRITE(IDF%IU) SNROW      !2
 WRITE(IDF%IU) IDF%XMIN   !3
 WRITE(IDF%IU) IDF%XMAX   !4
 WRITE(IDF%IU) IDF%YMIN   !5
 WRITE(IDF%IU) IDF%YMAX   !6
 WRITE(IDF%IU) IDF%DMIN   !7
 WRITE(IDF%IU) IDF%DMAX   !8
 WRITE(IDF%IU) IDF%NODATA !9
 WRITE(IDF%IU) INT(0,1),INT(0,1),INT(IDF%UNITS,1),INT(0,1)          !10 - IEQ
 WRITE(IDF%IU) CS         !11
 WRITE(IDF%IU) CS         !12

 DO IROW=1,IDF%NROW
  DY=IDF%SY(IROW-1)-IDF%SY(IROW)
  NR=INT(DY/CS)
  DO IR=1,NR
   DO ICOL=1,IDF%NCOL
    DX=IDF%SX(ICOL)-IDF%SX(ICOL-1)
    NC=INT(DX/CS)
!    X =IDFTRANSFORM_B(IDF,IDF%X(ICOL,IROW),ICOL,IROW)
!    DO IC=1,NC
!     WRITE(IDF%IU) X
!    END DO
    WRITE(IDF%IU) (X,IC=1,NC)
   END DO
  END DO
 END DO

 CLOSE(IDF%IU)
 IDF%IU=0

 IDFWRITE_EQUI=.TRUE.

 END FUNCTION IDFWRITE_EQUI

 !###======================================================================
 LOGICAL FUNCTION IDFREADDATA(IDATA,IDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN)  :: IDATA
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: IROW,ICOL,I,IOS

 IDFREADDATA=.FALSE.

 IF(IDF%IU.LE.0)RETURN

 IF(IDATA.EQ.1)THEN
  IF(.NOT.IDFALLOCATEX(IDF))RETURN
  IF(IDF%IXV.EQ.0)THEN
   DO IROW=1,IDF%NROW
    DO ICOL=1,IDF%NCOL
     READ(IDF%IU,IOSTAT=IOS) IDF%X(ICOL,IROW)
     IF(IOS.NE.0)RETURN
     !## apply transformation
     IDF%X(ICOL,IROW)=IDFTRANSFORM_F(IDF,IDF%X(ICOL,IROW),ICOL,IROW)
    END DO
   END DO
  ELSEIF(IDF%IXV.EQ.1)THEN
   I=0
   DO IROW=1,IDF%NROW
    DO ICOL=1,IDF%NCOL
     I=I+1
     READ(IDF%IU,IOSTAT=IOS) IDF%V(I)
     IF(IOS.NE.0)RETURN
     !## apply transformation
     IDF%V(I)=IDFTRANSFORM_F(IDF,IDF%V(I),ICOL,IROW)
    END DO
   ENDDO
  ENDIF
  CLOSE(IDF%IU)
 ENDIF

 IDFREADDATA=.TRUE.

 END FUNCTION IDFREADDATA

 !###======================================================================
 SUBROUTINE IDFGETCOMMENT(IDF,IDATA)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: NP=1
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER,INTENT(IN) :: IDATA
 INTEGER :: IOS,IREC,N,I,IADIT
 INTEGER,DIMENSION(NP) :: IP

 !## read entire data-block
 IF(IDATA.EQ.1)THEN
  READ(IDF%IU,IOSTAT=IOS) IADIT
 ELSE
  IREC= 10 + ABS(IDF%IEQ-1) * 2 + IDF%IEQ * (IDF%NROW+IDF%NCOL) + IDF%ITB*2 + (IDF%NROW*IDF%NCOL) + 1
  READ(IDF%IU,REC=IREC+ICF,IOSTAT=IOS) IADIT
 ENDIF

 IF(IOS.NE.0)RETURN

 !# read binair number (e.g. 256) and returns array (/1,0,0,1,0,0,1/)
 CALL IMOD_UTL_FILLARRAY(IP,NP,IADIT)

 !## read comments
 IF(IP(1).EQ.1)THEN
  IF(IDATA.EQ.1)THEN
   READ(IDF%IU,IOSTAT=IOS) N
  ELSE
   IREC=IREC+1
   READ(IDF%IU,REC=IREC+ICF,IOSTAT=IOS) N
  ENDIF

  !## error in reading
  IF(IOS.NE.0)RETURN
  ALLOCATE(IDF%COMMENT(N))
  IF(IDATA.EQ.1)THEN
   READ(IDF%IU,IOSTAT=IOS) (IDF%COMMENT(I),I=1,N)
   IF(IOS.NE.0)RETURN
  ELSE
   DO I=1,N
    IREC=IREC+1
    READ(IDF%IU,REC=IREC+ICF,IOSTAT=IOS) IDF%COMMENT(I)
    IF(IOS.NE.0)RETURN
   ENDDO
  ENDIF
 ENDIF

 END SUBROUTINE IDFGETCOMMENT

 !###======================================================================
 SUBROUTINE IDFFILLCOMMENT(IDF,STRING)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 CHARACTER(LEN=*),INTENT(IN) :: STRING
 CHARACTER(LEN=50) :: DATESTRING
 CHARACTER(LEN=256) :: DEFAULT

 !## fill default values in the beginning
 CALL IDFFILLCOMMENT_DATETIME(DATEANDTIME=DATESTRING)
 DEFAULT='User :'//TRIM(IDFGETENV('USERNAME'))//NEWLINE// &
         'iMODFLOW Version 2005: '//NEWLINE// &
         'Creation Date: '//TRIM(DATESTRING)//NEWLINE
 CALL IDFFILLCOMMENT2(IDF,TRIM(DEFAULT)//STRING)

 END SUBROUTINE IDFFILLCOMMENT

!###======================================================================
 SUBROUTINE IDFFILLCOMMENT_DATETIME(DATEANDTIME) !,IDATE,ITIME)
 !###======================================================================
 IMPLICIT NONE
 !INTEGER,OPTIONAL,INTENT(OUT) :: IDATE,ITIME
 CHARACTER(LEN=*),INTENT(OUT) :: DATEANDTIME
 CHARACTER(LEN=50) :: CTIME
 INTEGER :: TIME

 !IF(PRESENT(IDATE))PAUSE
 !IF(PRESENT(ITIME))PAUSE
 !IF(PRESENT(DATEANDTIME))THEN
 DATEANDTIME=CTIME(TIME())
 DATEANDTIME=DATEANDTIME(1:LEN_TRIM(DATEANDTIME)-1) !## there is something "dirty" on the back of this
 !ENDIF

 END SUBROUTINE IDFFILLCOMMENT_DATETIME

 !###======================================================================
 FUNCTION IDFGETENV(IKEYW)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IKEYW
 CHARACTER(LEN=50) :: IDFGETENV

 CALL GETENV(IKEYW,IDFGETENV)

 END FUNCTION IDFGETENV

 !###======================================================================
 SUBROUTINE IDFFILLCOMMENT2(IDF,STRING)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 CHARACTER(LEN=*),INTENT(IN) :: STRING
 CHARACTER(LEN=256) :: DATESTRING
 CHARACTER(LEN=256) :: DEFAULT
 INTEGER :: I,N

 N=0
 IF(MOD(LEN(STRING),4).NE.0)N=1
 N=N+LEN(STRING)/4

 IF(ASSOCIATED(IDF%COMMENT))DEALLOCATE(IDF%COMMENT)
 ALLOCATE(IDF%COMMENT(N))

 DO I=1,N
  IDF%COMMENT(I)='    '
  IDF%COMMENT(I)=STRING(((I-1)*4)+1:MIN(LEN(STRING),I*4))
 ENDDO

 END SUBROUTINE IDFFILLCOMMENT2

 !###======================================================================
 SUBROUTINE IDFWRITECOMMENT(IDF,IDATA)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER,INTENT(IN) :: IDATA
 INTEGER :: IREC,I

 IF(.NOT.ASSOCIATED(IDF%COMMENT))RETURN

 !## direct mode
 IF(IDATA.EQ.0)THEN
  IREC=ICF + 10 + ABS(IDF%IEQ-1) * 2 + IDF%IEQ * (IDF%NROW+IDF%NCOL) + IDF%ITB*2 + (IDF%NROW*IDF%NCOL) + 1
  !## write additional information
  IREC=IREC+1
  WRITE(IDF%IU,REC=IREC) 1 !## iadit
  IREC=IREC+1
  WRITE(IDF%IU,REC=IREC) SIZE(IDF%COMMENT)
  DO I=1,SIZE(IDF%COMMENT)
   IREC=IREC+1
   WRITE(IDF%IU,REC=IREC) IDF%COMMENT(I)
  ENDDO
 !## stream-mode
 ELSE
  WRITE(IDF%IU) 1
  WRITE(IDF%IU) SIZE(IDF%COMMENT)
  DO I=1,SIZE(IDF%COMMENT); WRITE(IDF%IU) IDF%COMMENT(I); ENDDO
 ENDIF

 END SUBROUTINE IDFWRITECOMMENT

 !###======================================================================
 LOGICAL FUNCTION IDFWRITEDATA(IDATA,IDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN)  :: IDATA
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: IROW,ICOL,I
! REAL :: X

 IDFWRITEDATA=.FALSE.

 IF(IDF%IU.LE.0)RETURN

 IF(IDATA.EQ.0)RETURN

 IF(IDF%IXV.EQ.0)THEN
  WRITE(IDF%IU) ((IDF%X(ICOL,IROW),ICOL=1,IDF%NCOL),IROW=1,IDF%NROW)
!  DO IROW=1,IDF%NROW
!   DO ICOL=1,IDF%NCOL
!    !## apply transformation
!    X=IDFTRANSFORM_B(IDF,IDF%X(ICOL,IROW),ICOL,IROW)
!    WRITE(IDF%IU) X !IDF%X(ICOL,IROW)
!   END DO
!  END DO
 ELSEIF(IDF%IXV.EQ.1)THEN
  WRITE(IDF%IU) (IDF%V(I),I=1,IDF%NCOL*IDF%NROW)
!  I=0
!  DO IROW=1,IDF%NROW
!   DO ICOL=1,IDF%NCOL
!    I=I+1
!    !## apply transformation
!    X=IDFTRANSFORM_B(IDF,IDF%V(I),ICOL,IROW)
!    WRITE(IDF%IU) X!IDF%V(I)
!   ENDDO
!  END DO
 ENDIF

 IDFWRITEDATA=.TRUE.

 END FUNCTION IDFWRITEDATA

 !###======================================================================
 SUBROUTINE IDFGETLOC(IDF,IROW,ICOL,X,Y)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER,INTENT(IN) :: IROW,ICOL
 REAL,INTENT(OUT) :: X,Y

 IF(IDF%IEQ.EQ.0)THEN

  X=IDF%XMIN+((ICOL-1)*IDF%DX)+0.5*IDF%DX
  Y=IDF%YMAX-((IROW-1)*IDF%DY)-0.5*IDF%DY

 ELSEIF(IDF%IEQ.EQ.1)THEN

  X=(IDF%SX(ICOL-1)+IDF%SX(ICOL))/2.0
  Y=(IDF%SY(IROW-1)+IDF%SY(IROW))/2.0

 ENDIF

 END SUBROUTINE IDFGETLOC

 !###======================================================================
 SUBROUTINE IDFIROWICOL(IDF,IROW,ICOL,X,Y)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 REAL,INTENT(IN) :: X,Y
 INTEGER,INTENT(OUT) :: ICOL,IROW

 DOUBLE PRECISION,PARAMETER :: TINY=0.001D0
 DOUBLE PRECISION :: XD, YD

 XD = DBLE(X); YD = DBLE(Y)

 ICOL=0
 IROW=0

 IF(IDF%IEQ.EQ.0)THEN

  IF(XD+TINY.GT.IDF%XMIN.AND.XD-TINY.LT.IDF%XMAX)ICOL=INT((X-IDF%XMIN)/IDF%DX)+1
  IF(YD+TINY.GT.IDF%YMIN.AND.YD-TINY.LT.IDF%YMAX)IROW=INT((IDF%YMAX-Y)/IDF%DY)+1
  ICOL=MIN(ICOL,IDF%NCOL)
  IROW=MIN(IROW,IDF%NROW)

 ELSEIF(IDF%IEQ.EQ.1)THEN

  CALL IMOD_UTL_POL1LOCATED(IDF%SX,IDF%NCOL+1,XD,ICOL)
  CALL IMOD_UTL_POL1LOCATED(IDF%SY,IDF%NROW+1,YD,IROW)
  IF(ICOL.LT.0.OR.ICOL.GT.IDF%NCOL) ICOL=0
  IF(IROW.LT.0.OR.IROW.GT.IDF%NROW) IROW=0

 ENDIF

 END SUBROUTINE IDFIROWICOL

 !###======================================================================
 REAL FUNCTION IDFGETVAL(IDF,IROW,ICOL)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: IROW,ICOL
 INTEGER :: IREC

 IREC=ICF +10  +ABS(IDF%IEQ-1) *2    +IDF%IEQ*(IDF%NROW+IDF%NCOL) +IDF%ITB*2
 IREC=IREC+  ((IROW-1)*IDF%NCOL)+ICOL

 READ(IDF%IU,REC=IREC) IDFGETVAL
 IDFGETVAL=IDFTRANSFORM_F(IDF,IDFGETVAL,ICOL,IROW)

 END FUNCTION IDFGETVAL

 !###======================================================================
 INTEGER FUNCTION IDFGETVAL_CHECK(IDF,IROW,ICOL,IDFVAL)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 REAL,INTENT(OUT) :: IDFVAL
 INTEGER,INTENT(IN) :: IROW,ICOL
 INTEGER :: IREC

 IREC=ICF +10  +ABS(IDF%IEQ-1) *2    +IDF%IEQ*(IDF%NROW+IDF%NCOL) +IDF%ITB*2
 IREC=IREC+  ((IROW-1)*IDF%NCOL)+ICOL

 READ(IDF%IU,REC=IREC,IOSTAT=IDFGETVAL_CHECK) IDFVAL
 IDFVAL=IDFTRANSFORM_F(IDF,IDFVAL,ICOL,IROW)

 END FUNCTION IDFGETVAL_CHECK

 !###======================================================================
 REAL FUNCTION IDFTRANSFORM_F(IDF,X,ICOL,IROW)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: ICOL,IROW
 REAL,INTENT(INOUT) :: X

 IDFTRANSFORM_F=X

 IF(IDF%UNITS.EQ.0)RETURN  !## no transformation
 IF(X.EQ.IDF%NODATA)RETURN

 SELECT CASE (IDF%UNITS)
  CASE (1) !## m --- cm
   X=X*100.0
  CASE (2) !## cm --- m
   X=X/100.0
  CASE (3) !## m --- mm
   X=X*1000.0
  CASE (4) !## mm --- m
   X=X/1000.0
  CASE (5) !## m3/day --- mm/day
   X=(X*1000.0)/IDFGETAREA(IDF,ICOL,IROW)
  CASE (6) !## mm/day --- m3/day
   X=(X/1000.0)*IDFGETAREA(IDF,ICOL,IROW)
 END SELECT

 IDFTRANSFORM_F=X

 END FUNCTION IDFTRANSFORM_F

 !###======================================================================
 REAL FUNCTION IDFTRANSFORM_B(IDF,X,ICOL,IROW) !## reciprook
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: ICOL,IROW
 REAL,INTENT(INOUT) :: X

 IDFTRANSFORM_B=X

 IF(IDF%UNITS.EQ.0)RETURN
 IF(X.EQ.IDF%NODATA)RETURN

 SELECT CASE (IDF%UNITS)
  CASE (1) !## m --- cm
   X=X/100.0
  CASE (2) !## cm --- m
   X=X*100.0
  CASE (3) !## m --- mm
   X=X/1000.0
  CASE (4) !## mm --- m
   X=X*1000.0
  CASE (5) !## m3/day --- mm/day
   X=(X/1000.0)*IDFGETAREA(IDF,ICOL,IROW)
  CASE (6) !## mm/day --- m3/day
   X=(X*1000.0)/IDFGETAREA(IDF,ICOL,IROW)
 END SELECT

 IDFTRANSFORM_B=X

 END FUNCTION IDFTRANSFORM_B

 !###======================================================================
 REAL FUNCTION IDFGETAREA(IDF,ICOL,IROW)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: ICOL,IROW

 IF(IDF%IEQ.EQ.0)THEN
  IDFGETAREA=IDF%DX*IDF%DY
 ELSE
  IDFGETAREA=IDF%SY(IROW-1)-IDF%SY(IROW)* &
             IDF%SX(ICOL)  -IDF%SX(ICOL-1)
 ENDIF

 END FUNCTION IDFGETAREA

 !###======================================================================
 SUBROUTINE IDFPUTVAL(IDF,IROW,ICOL,IDFVALUE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 REAL,INTENT(IN) :: IDFVALUE
 INTEGER,INTENT(IN) :: IROW,ICOL
 INTEGER :: IREC

 IREC=ICF +10  +ABS(IDF%IEQ-1) *2    +IDF%IEQ*(IDF%NROW+IDF%NCOL) +IDF%ITB*2
 IREC=IREC+  ((IROW-1)*IDF%NCOL)+ICOL

 WRITE(IDF%IU,REC=IREC) IDFVALUE

 END SUBROUTINE IDFPUTVAL

 !###======================================================================
 LOGICAL FUNCTION IDFREADDIM(IDATA,IDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN)  :: IDATA
 TYPE(IDFOBJ),INTENT(OUT) :: IDF
 INTEGER :: I,IREC,IOS
 INTEGER(KIND=1) :: I1,I2,I3,I4

 IDFREADDIM=.FALSE.

 IF(IDF%IU.LE.0)RETURN

 IF(IDATA.EQ.0)THEN

  READ(IDF%IU,REC=1+ICF,IOSTAT=IOS)  IDF%NCOL
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,REC=2+ICF,IOSTAT=IOS)  IDF%NROW
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,REC=3+ICF,IOSTAT=IOS)  IDF%XMIN
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,REC=4+ICF,IOSTAT=IOS)  IDF%XMAX
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,REC=5+ICF,IOSTAT=IOS)  IDF%YMIN
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,REC=6+ICF,IOSTAT=IOS)  IDF%YMAX
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,REC=7+ICF,IOSTAT=IOS)  IDF%DMIN
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,REC=8+ICF,IOSTAT=IOS)  IDF%DMAX
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,REC=9+ICF,IOSTAT=IOS)  IDF%NODATA
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,REC=10+ICF,IOSTAT=IOS) IDF%IEQ
  IF(IOS.NE.0)RETURN
  IF(IDF%IEQ.NE.0.AND.IDF%IEQ.NE.1)THEN
   READ(IDF%IU,REC=10+ICF,IOSTAT=IOS) I1,I2,I3,I4
   IF(IOS.NE.0)RETURN
   IDF%IEQ  =INT(I1)
   IDF%ITB  =INT(I2)
   IDF%UNITS=INT(I3)
  ELSE
   IDF%ITB  =0
   IDF%UNITS=0
  ENDIF
  IF(IDF%IEQ.EQ.0)THEN
   READ(IDF%IU,REC=11+ICF,IOSTAT=IOS) IDF%DX
   IF(IOS.NE.0)RETURN
   READ(IDF%IU,REC=12+ICF,IOSTAT=IOS) IDF%DY
   IF(IOS.NE.0)RETURN
   IF(IDF%ITB.EQ.1)THEN
    READ(IDF%IU,REC=13+ICF,IOSTAT=IOS) IDF%TOP
    IF(IOS.NE.0)RETURN
    READ(IDF%IU,REC=14+ICF,IOSTAT=IOS) IDF%BOT
    IF(IOS.NE.0)RETURN
   ENDIF
  ELSE
   IF(IDF%ITB.EQ.1)THEN
    READ(IDF%IU,REC=11+ICF,IOSTAT=IOS) IDF%TOP
    IF(IOS.NE.0)RETURN
    READ(IDF%IU,REC=12+ICF,IOSTAT=IOS) IDF%BOT
    IF(IOS.NE.0)RETURN
   ENDIF
  ENDIF

 ELSEIF(IDATA.EQ.1)THEN

  READ(IDF%IU,IOSTAT=IOS) I   !header: 1271
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,IOSTAT=IOS) IDF%NCOL   !1
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,IOSTAT=IOS) IDF%NROW   !2
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,IOSTAT=IOS) IDF%XMIN   !3
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,IOSTAT=IOS) IDF%XMAX   !4
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,IOSTAT=IOS) IDF%YMIN   !5
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,IOSTAT=IOS) IDF%YMAX   !6
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,IOSTAT=IOS) IDF%DMIN   !7
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,IOSTAT=IOS) IDF%DMAX   !8
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,IOSTAT=IOS) IDF%NODATA !9
  IF(IOS.NE.0)RETURN
  READ(IDF%IU,IOSTAT=IOS) IDF%IEQ    !10
  IF(IOS.NE.0)RETURN
  IF(IDF%IEQ.EQ.256)THEN
   BACKSPACE(IDF%IU)
   READ(IDF%IU,IOSTAT=IOS) I1,I2,I3,I4
   IF(IOS.NE.0)RETURN
   IDF%IEQ  =INT(I1)
   IDF%ITB  =INT(I2)
   IDF%UNITS=INT(I3)
  ELSE
   IDF%ITB  =0
   IDF%UNITS=0
  ENDIF
  IF(IDF%IEQ.EQ.0)THEN
   READ(IDF%IU,IOSTAT=IOS) IDF%DX     !11
   IF(IOS.NE.0)RETURN
   READ(IDF%IU,IOSTAT=IOS) IDF%DY     !12
   IF(IOS.NE.0)RETURN
   IF(IDF%ITB.EQ.1)THEN
    READ(IDF%IU,IOSTAT=IOS) IDF%TOP    !13
    IF(IOS.NE.0)RETURN
    READ(IDF%IU,IOSTAT=IOS) IDF%BOT    !14
    IF(IOS.NE.0)RETURN
   ENDIF
  ELSE
   IF(IDF%ITB.EQ.1)THEN
    READ(IDF%IU,IOSTAT=IOS) IDF%TOP    !11
    IF(IOS.NE.0)RETURN
    READ(IDF%IU,IOSTAT=IOS) IDF%BOT    !12
    IF(IOS.NE.0)RETURN
   ENDIF
  ENDIF
 ENDIF

 !##non-equidistantial grid
 IF(IDF%IEQ.EQ.1)THEN

  CALL IDFDEALLOCATEX(IDF)

  IF(.NOT.IDFALLOCATESXY(IDF))RETURN

  IF(IDATA.EQ.0)THEN
   IREC=ICF+10+IDF%ITB*2
   DO I=1,IDF%NCOL
    IREC=IREC+1
    READ(IDF%IU,REC=IREC,IOSTAT=IOS) IDF%SX(I)
    IF(IOS.NE.0)RETURN
   END DO
   DO I=1,IDF%NROW
    IREC=IREC+1
    READ(IDF%IU,REC=IREC,IOSTAT=IOS) IDF%SY(I)
    IF(IOS.NE.0)RETURN
   END DO

  ELSEIF(IDATA.EQ.1)THEN

   DO I=1,IDF%NCOL
    READ(IDF%IU,IOSTAT=IOS) IDF%SX(I)
    IF(IOS.NE.0)RETURN
   END DO
   DO I=1,IDF%NROW
    READ(IDF%IU,IOSTAT=IOS) IDF%SY(I)
    IF(IOS.NE.0)RETURN
   END DO

  ENDIF

  !## minimal cell-sizes
  IDF%DX=MINVAL(IDF%SX(1:IDF%NCOL))
  IDF%DY=MINVAL(IDF%SY(1:IDF%NROW))

  IDF%SX(0)=IDF%XMIN
  DO I=1,IDF%NCOL
   IDF%SX(I)=IDF%SX(I-1)+IDF%SX(I)
  END DO
  IDF%SY(0)=IDF%YMAX
  DO I=1,IDF%NROW
   IDF%SY(I)=IDF%SY(I-1)-IDF%SY(I)
  END DO

 ENDIF

 IDFREADDIM=.TRUE.

 END FUNCTION IDFREADDIM

 !###======================================================================
 LOGICAL FUNCTION ASCREADDATA(IDF)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(OUT) :: IDF
 INTEGER :: I,ICOL,IROW
 INTEGER,DIMENSION(6) :: IOS
 CHARACTER(LEN=16),DIMENSION(6) :: TXT

 ASCREADDATA=.FALSE.

 IF(IDF%IU.LE.0)RETURN

 READ(IDF%IU,*,IOSTAT=IOS(1))   TXT(1),IDF%NCOL
 READ(IDF%IU,*,IOSTAT=IOS(2))   TXT(2),IDF%NROW

 READ(IDF%IU,*,IOSTAT=IOS(3))   TXT(3),IDF%XMIN
 !## xllcenter-xllcorner
 CALL IMOD_UTL_S_CAP(TXT(3),'U')

 READ(IDF%IU,*,IOSTAT=IOS(4))   TXT(4),IDF%YMIN
 !## recompute yllcenter-yllcorner
 CALL IMOD_UTL_S_CAP(TXT(4),'U')

 READ(IDF%IU,*,IOSTAT=IOS(5))   TXT(5),IDF%DX
 READ(IDF%IU,*,IOSTAT=IOS(6))   TXT(6),IDF%NODATA

 IF(SUM(IOS).NE.0)THEN
  WRITE(*,*) 'Error reading header of ascii file!'
  RETURN
 ENDIF

 IF(TRIM(TXT(3)).EQ.'XLLCENTER')IDF%XMIN=IDF%XMIN-(IDF%DX/2.0)
 IF(TRIM(TXT(4)).EQ.'YLLCENTER')IDF%YMIN=IDF%YMIN-(IDF%DX/2.0)
 IDF%YMAX=IDF%YMIN+IDF%NROW*IDF%DX
 IDF%XMAX=IDF%XMIN+IDF%NCOL*IDF%DX

 IDF%IEQ   =0
 IDF%DY    =IDF%DX
 IDF%IXV   =0
 IDF%ITB   =0

 IF(.NOT.IDFALLOCATEX(IDF))THEN
  WRITE(*,*) 'Error, iMOD can not allocate enough memory to read the ascii file!'
  RETURN
 ENDIF

 IOS=0
 READ(IDF%IU,*,IOSTAT=IOS(1)) ((IDF%X(ICOL,IROW),ICOL=1,IDF%NCOL),IROW=1,IDF%NROW)
 IF(IOS(1).NE.0)THEN
  WRITE(*,*) 'Error reading data block of ascii file!'
  RETURN
 ENDIF

 ASCREADDATA=.TRUE.

 END FUNCTION ASCREADDATA

 !###======================================================================
 LOGICAL FUNCTION IDFWRITEDIM(IDATA,IDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDATA
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: I,J,IREC

 IDFWRITEDIM=.FALSE.

 IF(IDF%IU.LE.0)RETURN

 IDF%DMIN= 10.0E10
 IDF%DMAX=-10.0E10
 IF(IDF%IXV.EQ.0.AND.ASSOCIATED(IDF%X))THEN
  DO I=1,IDF%NROW
   DO J=1,IDF%NCOL
    IF(IDF%X(J,I).NE.IDF%NODATA)THEN
     IDF%DMIN=MIN(IDF%DMIN,IDF%X(J,I))
     IDF%DMAX=MAX(IDF%DMAX,IDF%X(J,I))
    ENDIF
   END DO
  END DO
 ELSEIF(IDF%IXV.EQ.1.AND.ASSOCIATED(IDF%V))THEN
  DO I=1,IDF%NROW*IDF%NCOL
   IF(IDF%V(I).NE.IDF%NODATA)THEN
    IDF%DMIN=MIN(IDF%DMIN,IDF%V(I))
    IDF%DMAX=MAX(IDF%DMAX,IDF%V(I))
   ENDIF
  END DO
 ENDIF

 !## make sure
 IDF%ITB=MAX(MIN(IDF%ITB,1),0)
 IDF%IEQ=MAX(MIN(IDF%IEQ,1),0)

 IF(IDATA.EQ.0)THEN

  IF(ICF.EQ.1)WRITE(IDF%IU,REC=1) 1271
  WRITE(IDF%IU,REC=1+ICF)  IDF%NCOL
  WRITE(IDF%IU,REC=2+ICF)  IDF%NROW
  WRITE(IDF%IU,REC=3+ICF)  IDF%XMIN
  WRITE(IDF%IU,REC=4+ICF)  IDF%XMAX
  WRITE(IDF%IU,REC=5+ICF)  IDF%YMIN
  WRITE(IDF%IU,REC=6+ICF)  IDF%YMAX
  WRITE(IDF%IU,REC=7+ICF)  IDF%DMIN
  WRITE(IDF%IU,REC=8+ICF)  IDF%DMAX
  WRITE(IDF%IU,REC=9+ICF)  IDF%NODATA
  WRITE(IDF%IU,REC=10+ICF) INT(IDF%IEQ,1),INT(IDF%ITB,1),INT(IDF%UNITS,1),INT(0,1)
  !## equidistantial raster
  IF(IDF%IEQ.EQ.0)THEN
   WRITE(IDF%IU,REC=11+ICF) IDF%DX
   WRITE(IDF%IU,REC=12+ICF) IDF%DY
   IF(IDF%ITB.EQ.1)THEN
    WRITE(IDF%IU,REC=13+ICF) IDF%TOP
    WRITE(IDF%IU,REC=14+ICF) IDF%BOT
   ENDIF
  !## non-equidistantial raster
  ELSE
   IF(IDF%ITB.EQ.1)THEN
    WRITE(IDF%IU,REC=11+ICF) IDF%TOP
    WRITE(IDF%IU,REC=12+ICF) IDF%BOT
   ENDIF
  ENDIF

 ELSEIF(IDATA.EQ.1)THEN

  WRITE(IDF%IU) 1271       !header: 1271
  WRITE(IDF%IU) IDF%NCOL   !1
  WRITE(IDF%IU) IDF%NROW   !2
  WRITE(IDF%IU) IDF%XMIN   !3
  WRITE(IDF%IU) IDF%XMAX   !4
  WRITE(IDF%IU) IDF%YMIN   !5
  WRITE(IDF%IU) IDF%YMAX   !6
  WRITE(IDF%IU) IDF%DMIN   !7
  WRITE(IDF%IU) IDF%DMAX   !8
  WRITE(IDF%IU) IDF%NODATA !9
  WRITE(IDF%IU) INT(IDF%IEQ,1),INT(IDF%ITB,1),INT(IDF%UNITS,1),INT(0,1)
  IF(IDF%IEQ.EQ.0)THEN
   WRITE(IDF%IU) IDF%DX     !11
   WRITE(IDF%IU) IDF%DY     !12
   IF(IDF%ITB.EQ.1)THEN
    WRITE(IDF%IU) IDF%TOP     !13
    WRITE(IDF%IU) IDF%BOT     !14
   ENDIF
  ELSE
   IF(IDF%ITB.EQ.1)THEN
    WRITE(IDF%IU) IDF%TOP     !11
    WRITE(IDF%IU) IDF%BOT     !12
   ENDIF
  ENDIF

 ENDIF

 !## non-equidistantial grid
 IF(IDF%IEQ.EQ.1)THEN

  IF(IDATA.EQ.0)THEN

   IREC=ICF+10+IDF%ITB*2
   DO I=1,IDF%NCOL
    IREC=IREC+1
    WRITE(IDF%IU,REC=IREC) IDF%SX(I)-IDF%SX(I-1)
   END DO
   DO I=1,IDF%NROW
    IREC=IREC+1
    WRITE(IDF%IU,REC=IREC) IDF%SY(I-1)-IDF%SY(I)
   END DO

  ELSEIF(IDATA.EQ.1)THEN

   DO I=1,IDF%NCOL
    WRITE(IDF%IU) IDF%SX(I)-IDF%SX(I-1)
   END DO
   DO I=1,IDF%NROW
    WRITE(IDF%IU) IDF%SY(I-1)-IDF%SY(I)
   END DO

  ENDIF

 ENDIF

 IDFWRITEDIM=.TRUE.

 END FUNCTION IDFWRITEDIM

 !###======================================================================
 SUBROUTINE IDFCOPY(IDF1,IDF2)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF1
 TYPE(IDFOBJ),INTENT(OUT) :: IDF2

 IDF2%NCOL  =IDF1%NCOL
 IDF2%NROW  =IDF1%NROW
 IDF2%XMIN  =IDF1%XMIN
 IDF2%XMAX  =IDF1%XMAX
 IDF2%YMIN  =IDF1%YMIN
 IDF2%YMAX  =IDF1%YMAX
 IDF2%DX    =IDF1%DX
 IDF2%DY    =IDF1%DY
 IDF2%IXV   =IDF1%IXV
 IDF2%ITB   =IDF1%ITB
 IDF2%UNITS =IDF1%UNITS
 IDF2%IEQ   =IDF1%IEQ
 IDF2%NODATA=0.0
 IDF2%DMIN  =0.0
 IDF2%DMAX  =1.0
 IF(.NOT.IDFALLOCATESXY(IDF2))RETURN
 IF(IDF2%IEQ.EQ.1)THEN
  IDF2%SX=IDF1%SX
  IDF2%SY=IDF1%SY
 ENDIF
 !## allocate memory x/v/ysel/ithrd
 IF(.NOT.IDFALLOCATEX(IDF2))RETURN
 IF(IDF2%IXV.EQ.2)IDF2%NTHREAD=0

 END SUBROUTINE IDFCOPY

 !###======================================================================
 LOGICAL FUNCTION IDFALLOCATESXY(IDF)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER,DIMENSION(2) :: IOS

 IDFALLOCATESXY=.FALSE.

 IF(ASSOCIATED(IDF%SX))DEALLOCATE(IDF%SX)
 IF(ASSOCIATED(IDF%SY))DEALLOCATE(IDF%SY)
 NULLIFY(IDF%SX); NULLIFY(IDF%SY)

 ALLOCATE(IDF%SX(0:IDF%NCOL),STAT=IOS(1))
 ALLOCATE(IDF%SY(0:IDF%NROW),STAT=IOS(2))

 IF(SUM(IOS).NE.0)THEN
  WRITE(*,*) 'Can not allocate enough memory to store sx/sy data'
  RETURN
 ENDIF

 IDFALLOCATESXY=.TRUE.

 END FUNCTION IDFALLOCATESXY

 !###======================================================================
 LOGICAL FUNCTION IDFALLOCATEX(IDF)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: IOS

 IDFALLOCATEX=.FALSE.

 IF(IDF%IXV.EQ.0)THEN
  IF(ASSOCIATED(IDF%X))THEN
   IF(SIZE(IDF%X,1).NE.IDF%NCOL.OR.SIZE(IDF%X,2).EQ.IDF%NROW)THEN
    DEALLOCATE(IDF%X)
   ENDIF
  ENDIF
  IF(.NOT.ASSOCIATED(IDF%X))THEN
   NULLIFY(IDF%X)
   ALLOCATE(IDF%X(IDF%NCOL,IDF%NROW),STAT=IOS)
  ENDIF
 ELSEIF(IDF%IXV.EQ.1)THEN
  IF(ASSOCIATED(IDF%V))THEN
   IF(SIZE(IDF%V).NE.IDF%NCOL*IDF%NROW)THEN
    DEALLOCATE(IDF%V)
   ENDIF
  ENDIF
  IF(.NOT.ASSOCIATED(IDF%V))THEN
   NULLIFY(IDF%V)
   ALLOCATE(IDF%V(IDF%NCOL*IDF%NROW),STAT=IOS)
  ENDIF
 ELSEIF(IDF%IXV.EQ.2)THEN
  IF(ASSOCIATED(IDF%YSEL))THEN
   IF(SIZE(IDF%YSEL,1).NE.2.AND.SIZE(IDF%YSEL,2).NE.IDF%NCOL*IDF%NROW)THEN
    DEALLOCATE(IDF%YSEL)
   ENDIF
  ENDIF
  IF(.NOT.ASSOCIATED(IDF%YSEL))THEN
   NULLIFY(IDF%YSEL)
   ALLOCATE(IDF%YSEL(2,IDF%NCOL*IDF%NROW),STAT=IOS)
  ENDIF
 ELSE
  CALL IDFDEALLOCATEX(IDF)
  WRITE(*,*) 'Can not recognize ixv variable within IDF-object'
  RETURN
 ENDIF

 IF(IOS.NE.0)THEN
  CALL IDFDEALLOCATEX(IDF)
  WRITE(*,*) 'Can not allocate enough memory to store entire IDF (ncol='//TRIM(ITOS(IDF%NCOL))// &
    ';nrow='//TRIM(ITOS(IDF%NROW))
  RETURN
 ENDIF

 IDFALLOCATEX=.TRUE.

 END FUNCTION IDFALLOCATEX

 !###======================================================================
 SUBROUTINE IDFDEALLOCATE(IDF,NIDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NIDF
 TYPE(IDFOBJ),DIMENSION(NIDF),INTENT(INOUT) :: IDF
 INTEGER :: I
 LOGICAL :: LEX

 DO I=1,SIZE(IDF)
  CALL IDFDEALLOCATEX(IDF(I))
  IF(IDF(I)%IU.GT.0)THEN
   INQUIRE(UNIT=IDF(I)%IU,OPENED=LEX)
   IF(LEX)CLOSE(IDF(I)%IU)
   IDF(I)%IU=0
  ENDIF
 END DO

 END SUBROUTINE IDFDEALLOCATE

 !###======================================================================
 SUBROUTINE IDFDEALLOCATEX(IDF)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF

 !## deallocate sx
 IF(ASSOCIATED(IDF%SX))DEALLOCATE(IDF%SX)
 NULLIFY(IDF%SX)
 !## deallocate sy
 IF(ASSOCIATED(IDF%SY))DEALLOCATE(IDF%SY)
 NULLIFY(IDF%SY)
 !## deallocate x
 IF(ASSOCIATED(IDF%X))DEALLOCATE(IDF%X)
 NULLIFY(IDF%X)
 !## deallocate v
 IF(ASSOCIATED(IDF%V))DEALLOCATE(IDF%V)
 NULLIFY(IDF%V)
 !## deallocate ysel
 IF(ASSOCIATED(IDF%YSEL))DEALLOCATE(IDF%YSEL)
 NULLIFY(IDF%YSEL)
 !## deallocate comment
 IF(ASSOCIATED(IDF%COMMENT))DEALLOCATE(IDF%COMMENT)
 NULLIFY(IDF%COMMENT)

 END SUBROUTINE IDFDEALLOCATEX

 !###======================================================================
 SUBROUTINE IDFNULLIFY(IDF)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF

 NULLIFY(IDF%X)
 NULLIFY(IDF%V)
 NULLIFY(IDF%SX)
 NULLIFY(IDF%SY)
 NULLIFY(IDF%YSEL)
 NULLIFY(IDF%COMMENT)
 IDF%IXV  =0
 IDF%ITB  =0
 IDF%UNITS=0

 END SUBROUTINE IDFNULLIFY

 !###======================================================================
 SUBROUTINE IDFGETILAY(IDF,IDFNAME)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 INTEGER :: I,J,ILAY,IOS

 IDF%ILAY=0

 !## get layer
 I=INDEX(IMOD_UTL_CAPF(IDFNAME,'U'),'L',.TRUE.)+1
 J=INDEX(IMOD_UTL_CAPF(IDFNAME,'U'),'.IDF',.TRUE.)-1
 !## not proper file-name format
 IF(J.GE.I)THEN
  READ(IDFNAME(I:J),*,IOSTAT=IOS) ILAY
  IF(IOS.EQ.0)IDF%ILAY=ILAY
 ENDIF

 END SUBROUTINE IDFGETILAY

 !###======================================================================
 LOGICAL FUNCTION IDFOPEN(IU,IDFNAME,TSTAT,IDATA,IQUESTION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IU
 INTEGER,INTENT(IN) :: IDATA
 INTEGER,INTENT(IN),OPTIONAL :: IQUESTION
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME,TSTAT
 INTEGER :: RECLEN,I,IOS,IQ
 LOGICAL :: LEX,LOPEN
 CHARACTER(LEN=100) :: MSG
 CHARACTER(LEN=25) :: TACTION
 CHARACTER(LEN=1) :: YN

 IF(TSTAT(1:1).NE.'W'.AND.TSTAT(1:1).NE.'R')THEN
  WRITE(*,*) 'Opening Status [ '//TRIM(TSTAT)//' ] not recognized'
  RETURN
 ENDIF

 IDFOPEN=.FALSE.

 IF(PRESENT(IQUESTION))IQ=IQUESTION

 IU=0

 IF(LEN_TRIM(IDFNAME).EQ.0)THEN
  WRITE(*,*) 'No IDF filename given!'
  RETURN
 ENDIF

 IF(ICF.EQ.0)RECLEN=4 !## bytes
 IF(ICF.EQ.1)RECLEN=1 !## words

 INQUIRE(FILE=IDFNAME,EXIST=LOPEN)
 IF(LOPEN) THEN
  INQUIRE(FILE=IDFNAME,OPENED=LOPEN)
 ENDIF
 IF(LOPEN)THEN

  !## check opening action to be similar
  INQUIRE(FILE=IDFNAME,ACTION=TACTION)
  LEX=.FALSE.
  SELECT CASE (TRIM(TACTION))
   CASE ('READ')
    IF(TSTAT(1:1).EQ.'R')LEX=.TRUE.
   CASE ('WRITE')
    IF(TSTAT(1:1).EQ.'W')LEX=.TRUE.
   CASE ('READWRITE')
    IF(LEN_TRIM(TSTAT).EQ.2)LEX=.TRUE.
  END SELECT

  IF(LEX)THEN
   INQUIRE(FILE=IDFNAME,NUMBER=IU)
   IF(IDATA.EQ.0)THEN
    IDFOPEN=.TRUE.
    RETURN
   ELSEIF(IDATA.EQ.1)THEN
    CLOSE(IU)
   ENDIF
  ELSE
   CLOSE(IU)
  ENDIF
 ENDIF

 IF(TSTAT(1:1).EQ.'R')THEN  !## read
  INQUIRE(FILE=IDFNAME,EXIST=LEX)
  IF(.NOT.LEX)THEN
   WRITE(*,*) 'Can not find '//TRIM(IDFNAME)
   RETURN
  ENDIF
 ELSEIF(TSTAT(1:1).EQ.'W')THEN !## write
  INQUIRE(FILE=IDFNAME,EXIST=LEX)
  IF(IQ.EQ.1.AND.LEX)THEN
   WRITE(*,'(1X,A$)') TRIM(IDFNAME)//' exists, overwrite it?'
   READ(*,*) YN
   IF(YN.NE.'Y'.AND.YN.NE.'y')RETURN
  ENDIF
 ENDIF

 IU=GETUNIT()
 IF(TSTAT(1:1).EQ.'R')THEN
  IF(IDATA.EQ.1)THEN
   OPEN(IU,FILE=IDFNAME,STATUS='OLD',FORM='UNFORMATTED',ACCESS='STREAM', &
                 ACTION='READ',IOSTAT=IOS)
  ELSEIF(IDATA.EQ.0)THEN
   !## read only
   IF(LEN_TRIM(TSTAT).EQ.1)THEN
    OPEN(IU,FILE=IDFNAME,STATUS='OLD',FORM='UNFORMATTED',ACCESS='DIRECT', &
                  RECL=RECLEN,ACTION='READ',IOSTAT=IOS)
   ELSE
    !## read only
    IF(TSTAT.EQ.'RO')THEN
     OPEN(IU,FILE=IDFNAME,STATUS='OLD',FORM='UNFORMATTED',ACCESS='DIRECT', &
                   RECL=RECLEN,ACTION='READ',IOSTAT=IOS)
    !## read/write
    ELSEIF(TSTAT.EQ.'RW')THEN
     OPEN(IU,FILE=IDFNAME,STATUS='OLD',FORM='UNFORMATTED',ACCESS='DIRECT', &
                   RECL=RECLEN,ACTION='READWRITE',IOSTAT=IOS)
    ENDIF
   ENDIF
  ENDIF
 ELSEIF(TSTAT(1:1).EQ.'W')THEN
  IF(IDATA.EQ.1)THEN
   OPEN(IU,FILE=IDFNAME,STATUS='REPLACE',ACTION='WRITE',ACCESS='STREAM',IOSTAT=IOS,FORM='UNFORMATTED')
  ELSEIF(IDATA.EQ.0)THEN
   OPEN(IU,FILE=IDFNAME,STATUS='REPLACE',ACTION='READWRITE',ACCESS='DIRECT', &
                 RECL=RECLEN,IOSTAT=IOS,FORM='UNFORMATTED')
  ENDIF
  IF(IOS.NE.0)IU=0
 ENDIF

 IF(IOS.NE.0)THEN
!  CALL OSD_IOSTAT_MSG(IOS,MSG)
  I=INDEX(IDFNAME,'/')
  IF(I.NE.0)MSG=TRIM(MSG)//'"/" inside filename!'
  WRITE(*,*) 'Error opening '//TRIM(IDFNAME)//CHAR(13)//'Status: '//TRIM(MSG)
  RETURN
 ENDIF

 IDFOPEN=.TRUE.

 END FUNCTION IDFOPEN

END MODULE IMOD_IDF
