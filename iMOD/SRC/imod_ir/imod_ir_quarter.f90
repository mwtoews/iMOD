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
MODULE MOD_IR_QUARTER

USE WINTERACTER
USE RESOURCE
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_SUBST,CLOSEUNITS,ITOS,RTOS,UTL_CAP,UTL_MESSAGEHANDLE,UTL_CREATEDIR,UTL_IDFSNAPTOGRID
USE MOD_IR_PAR
USE MOD_IR_UTL, ONLY : IR1GETTREEVIEWID,IR1FIELDS_STRING,IR1GETTREEIDS
USE MOD_IR_CLC, ONLY : IR2GETEXTENSION
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_GXG_CLC, ONLY : GXG1COMPUTEGXG
USE MOD_OSD, ONLY : OSD_OPEN

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION IR1QUARTER_MAIN(IMDLTYPE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMDLTYPE
 INTEGER :: ITREE,IFIELD,IMEASURE,ITARGET
 CHARACTER(LEN=MAXLEN),DIMENSION(3) :: CTREE
 CHARACTER(LEN=256) :: OUTPUT
 LOGICAL :: LEX

 IR1QUARTER_MAIN=.FALSE.

 CALL UTL_MESSAGEHANDLE(0)

 IF(LEN_TRIM(PREFVAL(8)).EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should specify the keyword: MODFLOW in the used *.prf-file','Error')
  RETURN
 ENDIF
 INQUIRE(FILE=PREFVAL(8),EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'The specified modflow version '//TRIM(PREFVAL(8))//' does not exists','Error')
  RETURN
 ENDIF

 !## get level of treeview
 CALL IR1GETTREEVIEWID(ITREE,IFIELD)
 !## get name
 CALL IR1FIELDS_STRING(CTREE,ITREE,IFIELD)
 !## get imeasure
 CALL IR1GETTREEIDS(IFIELD,ITARGET,IMEASURE)

 OUTPUT=TRIM(RESDIR)//'\'//TRIM(ADJUSTL(CTREE(1)))//'\'//TRIM(ADJUSTL(CTREE(2)))//'\'//TRIM(ADJUSTL(CTREE(3)))
 IF(.NOT.IOSDIREXISTS(TRIM(OUTPUT)//'\simulation'))CALL UTL_CREATEDIR(TRIM(OUTPUT)//'\simulation')

 IR1QUARTER_MAIN=.TRUE.

 !## quarter model
 IF(IMDLTYPE.EQ.2)THEN
  !## create runfile quarter-model
  IF(.NOT.IR1QUARTER_RUNFILE(QUARTERRUNFILE,TRIM(OUTPUT)//'\simulation',IMEASURE))IR1QUARTER_MAIN=.FALSE.
 ELSEIF(IMDLTYPE.EQ.3)THEN
  !## create runfile daily-model
  IF(.NOT.IR1QUARTER_RUNFILE(BASISRUNFILE,TRIM(OUTPUT)//'\simulation',IMEASURE))IR1QUARTER_MAIN=.FALSE.
 ENDIF
 IF(IR1QUARTER_MAIN)THEN
  !## create scnfile/sdffiles
  IF(IR1QUARTER_SCNFILE(IFIELD,TRIM(OUTPUT)//'\simulation',IMEASURE,IMDLTYPE))THEN
   !## start simulation
   IF(.NOT.IR1QUARTER_RUNMODEL(TRIM(OUTPUT),IMDLTYPE))IR1QUARTER_MAIN=.FALSE.
  ENDIF
 ENDIF

 CALL CLOSEUNITS()
 CALL UTL_MESSAGEHANDLE(1)

 END FUNCTION IR1QUARTER_MAIN

 !###======================================================================
 LOGICAL FUNCTION IR1QUARTER_RUNMODEL(OUTPUT,IMDLTYPE)
 !###======================================================================
 USE MOD_GXG_PAR
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMDLTYPE
 CHARACTER(LEN=*),INTENT(IN) :: OUTPUT
 INTEGER :: IU,JU,IOS,IPER,IRES,I
 CHARACTER(LEN=256) :: FNAME1,FNAME2
 LOGICAL :: LEX

 IR1QUARTER_RUNMODEL=.FALSE.

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(OUTPUT)//'\simulation\run.bat',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not create run.bat','Error')
  RETURN
 ENDIF

 WRITE(IU,*) TRIM(PREFVAL(8))//' '//TRIM(OUTPUT)//'\simulation\imodflow.run'
 IF(IMDLTYPE.EQ.2)THEN
  !## write copy-action
  DO IPER=1,NPER
   DO IRES=1,NRES
    FNAME1=TRIM(OUTPUT)//'\simulation\model\ir_effect\'//TRIM(RES(IRES)%DIRRES)//'\'//TRIM(RES(IRES)%DIRRES)// &
           '_S'//TRIM(ITOS(PER(IPER)%IPERPER))//'L'//TRIM(ITOS(RES(IRES)%ILAYRES))//'.IDF'
    FNAME2=TRIM(OUTPUT)//'\'//TRIM(PER(IPER)%NAMEPER)//'_'//TRIM(RES(IRES)%NAMERES)//'.IDF'
    WRITE(IU,*) 'COPY "'//TRIM(FNAME1)//'" "'//TRIM(FNAME2)//'"'
   END DO
  END DO
 ELSEIF(IMDLTYPE.EQ.3)THEN
  !# write computing gxg
  !## gxg-batch-file
  GXG_NLAYER=1; ALLOCATE(GXG_ILAYER(GXG_NLAYER))
  ALLOCATE(GXG_IPERIOD(12,2)); GXG_IPERIOD=1
  GXG_RESDIR=TRIM(OUTPUT)//'\simulation\model\head'
  ISEL=1
 ENDIF

 CLOSE(IU)
 CALL IOSCOMMAND(TRIM(OUTPUT)//'\simulation\run.bat',PROCBLOCKED)
 !## compute gxg
 IF(IMDLTYPE.EQ.3)THEN
  IF(.NOT.GXG1COMPUTEGXG())THEN
  ENDIF
  DEALLOCATE(GXG_IPERIOD,GXG_ILAYER)
 ENDIF

 IR1QUARTER_RUNMODEL=.TRUE.

 END FUNCTION IR1QUARTER_RUNMODEL

 !###======================================================================
 LOGICAL FUNCTION IR1QUARTER_SCNFILE(IFIELD,OUTPUT,IMEASURE,IMDLTYPE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFIELD,IMEASURE,IMDLTYPE
 CHARACTER(LEN=*),INTENT(IN) :: OUTPUT
 LOGICAL :: LEX
 INTEGER :: IPOL,IMES,IIR
 REAL :: IMP
 CHARACTER(LEN=256) :: FNAME

 IR1QUARTER_SCNFILE=.FALSE.

 !## construct sdf-file for measures
 DO IPOL=1,MTREE(IMEASURE)%NPOL
  DO IMES=1,MTREE(IMEASURE)%POL(IPOL)%NMES
   IIR=MTREE(IMEASURE)%POL(IPOL)%MES(IMES)%IMES
   FNAME=TRIM(OUTPUT)//'\sdf_measure'//TRIM(ITOS(IIR))//'_polygon'//TRIM(ITOS(IPOL))//'.sdf'
   INQUIRE(FILE=FNAME,EXIST=LEX)
   !## does not exist yet, create it
   IF(.NOT.LEX)THEN
    IMP=MTREE(IMEASURE)%POL(IPOL)%MES(IMES)%IMP
    !## quarter model
    IF(IMDLTYPE.EQ.2)THEN
     IF(.NOT.IR1QUARTER_SDF(IR(IIR)%SDFIR,FNAME,IMP))RETURN
    !## basis model
    ELSEIF(IMDLTYPE.EQ.3)THEN
     IF(.NOT.IR1QUARTER_SDF(IR(IIR)%SDFBM,FNAME,IMP))RETURN
    ENDIF
   ENDIF
  END DO
 END DO

 IF(.NOT.IR1QUARTER_SCN(OUTPUT,IMEASURE))RETURN

 IR1QUARTER_SCNFILE=.TRUE.

 END FUNCTION IR1QUARTER_SCNFILE

 !###======================================================================
 LOGICAL FUNCTION IR1QUARTER_SCN(OUTPUT,IMEASURE)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: OUTPUT
 INTEGER,INTENT(IN) :: IMEASURE
 INTEGER :: IU,IOS,I,IMES,IPOL
 CHARACTER(LEN=256) :: LINE

 IR1QUARTER_SCN=.FALSE.

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(OUTPUT)//'\imodflow.scn',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not create scenariofile: '//TRIM(OUTPUT)//'\imodfow.scn','Error')
  RETURN
 ENDIF

 !## construct scn-file for measures
 DO IPOL=1,MTREE(IMEASURE)%NPOL
  WRITE(IU,'(50A1)') ('=',I=1,50)
  WRITE(IU,*) TRIM(MTREE(IMEASURE)%POL(IPOL)%POLNAME)
  WRITE(IU,'(50A1)') ('=',I=1,50)

  LINE=TRIM(ITOS(MTREE(IMEASURE)%POL(IPOL)%NMES))
  WRITE(IU,*) TRIM(LINE)
  DO IMES=1,MTREE(IMEASURE)%POL(IPOL)%NMES
   I=MTREE(IMEASURE)%POL(IPOL)%MES(IMES)%IMES
!   LINE=TRIM(OUTPUT)//'\sdf_measure'//TRIM(ITOS(I))//'.sdf'
   LINE=TRIM(OUTPUT)//'\sdf_measure'//TRIM(ITOS(I))//'_polygon'//TRIM(ITOS(IPOL))//'.sdf'
   WRITE(IU,*) TRIM(LINE)
  ENDDO
  LINE=TRIM(ITOS(MTREE(IMEASURE)%POL(IPOL)%NCRD))
  WRITE(IU,*) TRIM(LINE)
  DO I=1,MTREE(IMEASURE)%POL(IPOL)%NCRD
   LINE=TRIM(RTOS(MTREE(IMEASURE)%POL(IPOL)%X(I),'F',2))//','//TRIM(RTOS(MTREE(IMEASURE)%POL(IPOL)%Y(I),'F',2))
   WRITE(IU,*) TRIM(LINE)
  END DO
  WRITE(IU,'(50A1)') ('=',I=1,50)

 ENDDO

 CLOSE(IU)

 IR1QUARTER_SCN=.TRUE.

 END FUNCTION IR1QUARTER_SCN

 !###======================================================================
 LOGICAL FUNCTION IR1QUARTER_SDF(SDF_FROM,SDF_TO,IMP)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: SDF_TO,SDF_FROM
 REAL,INTENT(IN) :: IMP
 INTEGER :: IU,JU,IOS
 LOGICAL :: LEX
 CHARACTER(LEN=256) :: LINE

 IR1QUARTER_SDF=.FALSE.

 IU=UTL_GETUNIT()
 INQUIRE(FILE=SDF_FROM,EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not find scenario definition file: '//TRIM(SDF_FROM),'Error')
  RETURN
 ENDIF

 CALL OSD_OPEN(IU,FILE=SDF_FROM,ACTION='READ,DENYWRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open scenario definition file: '//TRIM(SDF_FROM),'Error')
  RETURN
 ENDIF
 JU=UTL_GETUNIT()
 CALL OSD_OPEN(JU,FILE=SDF_TO,ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not create scenario definition file: '//TRIM(SDF_TO),'Error')
  RETURN
 ENDIF

 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  IF(.NOT.IR1QUARTER_REPLACE(LINE,IMP))RETURN
  WRITE(JU,*) TRIM(LINE)
 ENDDO

 CLOSE(IU)
 CLOSE(JU)

 IR1QUARTER_SDF=.TRUE.

 END FUNCTION IR1QUARTER_SDF

 !###======================================================================
 LOGICAL FUNCTION IR1QUARTER_RUNFILE(RUNFILE,OUTPUT,IFIELD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFIELD
 CHARACTER(LEN=*),INTENT(IN) :: RUNFILE,OUTPUT
 INTEGER :: IU,JU,IOS,I,MXCRD
 REAL :: XMIN,YMIN,XMAX,YMAX,CS,MAXCS,BUFFER
 LOGICAL :: LEX
 CHARACTER(LEN=256) :: LINE

 CS    =25.0
 BUFFER=2500.0
 MAXCS =10*CS

 IR1QUARTER_RUNFILE=.FALSE.

 IU=UTL_GETUNIT()
 INQUIRE(FILE=RUNFILE,EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not find runfile: '//TRIM(RUNFILE),'Error')
  RETURN
 ENDIF

 CALL OSD_OPEN(IU,FILE=RUNFILE,ACTION='READ,DENYWRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open runfile: '//TRIM(RUNFILE),'Error')
  RETURN
 ENDIF
 JU=UTL_GETUNIT()
 CALL OSD_OPEN(JU,FILE=TRIM(OUTPUT)//'\imodflow.run',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not create runfile: '//TRIM(OUTPUT)//'\imodflow.run','Error')
  RETURN
 ENDIF

 !##activate all polygons
 DO I=1,MTREE(IFIELD)%NPOL; MTREE(IFIELD)%POL(I)%IACT=1; END DO
 !## get dimensions of impulse within each (selected) polygon
 IF(.NOT.IR2GETEXTENSION(2,IFIELD,XMIN,YMIN,XMAX,YMAX,MXCRD))RETURN
 CALL UTL_IDFSNAPTOGRID(XMIN,XMAX,YMIN,YMAX,CS,I,MXCRD)  !## i,mxcrd are dummy variables, nothing done with!

 READ(IU,*)
 WRITE(JU,*) TRIM(OUTPUT)//'\model'
 I=1
 DO
  I=I+1
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT

  !## replace coordinates (line6)
  IF(I.EQ.6)THEN

   LINE=TRIM(RTOS(XMIN,'F',2))//','//TRIM(RTOS(YMIN,'F',2))//','// &
        TRIM(RTOS(XMAX,'F',2))//','//TRIM(RTOS(YMAX,'F',2))//','// &
        TRIM(RTOS(CS,'F',2))//','//TRIM(RTOS(MAXCS,'F',2))//','//TRIM(RTOS(BUFFER,'F',2))
   WRITE(JU,*) TRIM(LINE)
  !## replace scenario-file (line7)
  ELSEIF(I.EQ.7)THEN
   WRITE(JU,*) TRIM(OUTPUT)//'\imodflow.scn'
  ELSE
   WRITE(JU,*) TRIM(LINE)
  ENDIF
 ENDDO

 CLOSE(IU)
 CLOSE(JU)

 IR1QUARTER_RUNFILE=.TRUE.

 END FUNCTION IR1QUARTER_RUNFILE

 !###======================================================================
 LOGICAL FUNCTION IR1QUARTER_REPLACE(LINE,IMP)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: LINE
 REAL,INTENT(IN) :: IMP
 INTEGER :: I,J,IOS
 REAL :: FCT

 IR1QUARTER_REPLACE=.FALSE.

 LINE=UTL_CAP(LINE,'U')
 I=INDEX(TRIM(LINE),'QS]',.TRUE.)
 !## nothing to adjust
 IF(I.EQ.0)THEN
  IR1QUARTER_REPLACE=.TRUE.
  RETURN
 ENDIF

 !## first position of keyword bracketing
 J=INDEX(TRIM(LINE),'[',.TRUE.)

 !## not a factor, replace string and return
 IF(J+1.EQ.I)THEN
! WRITE(*,*) 'not factor ...',IMP,TRIM(LINE)
  LINE=UTL_SUBST(LINE,'[QS]',TRIM(RTOS(IMP,'F',2)))
 ELSE
  !## get factor?
  READ(LINE(J+1:I-2),*,IOSTAT=IOS) FCT
!  WRITE(*,*) 'factor ...',FCT,TRIM(LINE),IOS
  !## error occured
  IF(IOS.NE.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not read impulse in key: '//CHAR(13)//TRIM(LINE),'Error')
   RETURN
  ENDIF
  !## get multiplyer
  SELECT CASE (LINE(I-1:I-1))
   CASE('*')
    FCT=FCT*IMP
   CASE('/')
    FCT=FCT/IMP
   CASE('+')
    FCT=FCT+IMP
   CASE('-')
    FCT=FCT-IMP
   CASE DEFAULT
  END SELECT
  LINE=UTL_SUBST(LINE,LINE(J:I+2),TRIM(RTOS(FCT,'F',2)))
 ENDIF

 IR1QUARTER_REPLACE=.TRUE.

 END FUNCTION IR1QUARTER_REPLACE

END MODULE MOD_IR_QUARTER
