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
MODULE MOD_TS_CLC

USE WINTERACTER
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_CAP,UTL_IDATETOJDATE,UTL_JDATETOIDATE,UTL_WAITMESSAGE,ITOS,RTOS,UTL_MESSAGEHANDLE,UTL_CREATEDIR
USE MOD_IDF, ONLY : IDFDEALLOCATE,IDFGETVAL,IDFREAD,IDFIROWICOL,IDFNULLIFY
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_TS_PAR

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION TS1COMPUTE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,IOS,IRAT,IRAT1,ITYPE !,DC
 TYPE(WIN_MESSAGE) :: MESSAGE
 REAL :: X,Y
 CHARACTER(LEN=MAXLEN) :: CDUM
 
 !## inquire whether org. ipf exists
 INQUIRE(FILE=IPFNAME1,EXIST=TS1COMPUTE)
 IF(.NOT.TS1COMPUTE)THEN
  IF(IBATCH.EQ.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'File: '//TRIM(IPFNAME1)//' not found.','Error')
  ELSEIF(IBATCH.EQ.1)THEN
   WRITE(*,*) 'File: '//TRIM(IPFNAME1)//' not found.'
  ENDIF
  RETURN
 ENDIF
 TS1COMPUTE=.FALSE.
 
 !## convert to julian date
 JD1 =UTL_IDATETOJDATE(JD1)
 !## convert to julian date
 JD2 =UTL_IDATETOJDATE(JD2)
 !## compute number of days in between
 NPER=JD2-JD1+1

 !## Initialize all global array's - using nper
 CALL TS_INIT()

 !## if not opened all existing idf's correctly
 IF(.NOT.TS_INIT_IDF())RETURN
 !## if not opened and read header of ipf
 IF(.NOT.TS_INIT_IPF())RETURN !DC))RETURN

 ALLOCATE(STRING(NCOL))

 !## process all the records in the ipf-files
 IRAT=0
 DO I=1,NROW

  CALL WMESSAGEPEEK(ITYPE,MESSAGE)

  READ(IU(1),*,IOSTAT=IOS) (STRING(J),J=1,NCOL)
  IF(IOS.NE.0) THEN
   CALL TS_ERROR_MESSAGE('Error occurred during read of file '//TRIM(IPFNAME1)//'.')
   RETURN
  ENDIF

  !## assuming these are the x/y coordinates
  CDUM=STRING(1)
  READ(CDUM,*,IOSTAT=IOS) X
  IF(IOS.NE.0)THEN
   CALL TS_ERROR_MESSAGE('Error reading X-coordinate in IROW '//TRIM(ITOS(I))//' in file '//TRIM(IPFNAME1)//'.')
   RETURN
  ENDIF
  CDUM=STRING(2)
  READ(CDUM,*,IOSTAT=IOS) Y
  IF(IOS.NE.0)THEN
   CALL TS_ERROR_MESSAGE('Error reading Y-coordinate in IROW '//TRIM(ITOS(I))//' in file '//TRIM(IPFNAME1)//'.')
   RETURN
  ENDIF

  IASSF=0
  IF(IEXT.GT.0)THEN 
   IASSF=1
   !## read associated file (could be spaces within) column
   CDUM='"'//TRIM(STRING(IEXT))//'"'
   READ(CDUM,*,IOSTAT=IOS) CTS
   IF(IOS.NE.0)THEN
    CALL TS_ERROR_MESSAGE('Error reading identification in IROW '//TRIM(ITOS(I))//' in file '//TRIM(IPFNAME1)//'.')
    RETURN
   ENDIF
  ENDIF

  !## read the timeseries, if available
  IF(.NOT.TS_READ_IPF())RETURN

  !## read the idfs
  IF(.NOT.TS_READ_IDF(X,Y))RETURN

  !## compute residual
  CALL TS_CALRESIDUALS()

  IF(LCOL.EQ.0)THEN
   CTS='ts_measure'//TRIM(ITOS(I))
   J=INDEX(IPFNAME2,'\',.TRUE.)+1
   K=INDEX(IPFNAME2,'.',.TRUE.)-1
   CTS=IPFNAME2(J:K)//'\'//TRIM(CTS)
  ELSEIF(LCOL.GT.0)THEN
   CTS=STRING(LCOL)  
  ENDIF
    
  !## writing COMMA DELIMITED file
  LINE=TRIM(STRING(1))
  DO J=2,NCOL; LINE=TRIM(LINE)//','//TRIM(STRING(J)); ENDDO
  
  IF(LCOL.EQ.0)LINE=TRIM(LINE)//',"'//TRIM(CTS)//'"' 
  WRITE(IU(2),*,IOSTAT=IOS) TRIM(LINE)
  IF(IOS.NE.0)THEN
   CALL TS_ERROR_MESSAGE('Error occurred during writing line '//TRIM(ITOS(I))//' to file '//TRIM(IPFNAME2)//'.')
   RETURN
  ENDIF

  !## write results
  LINE=IPFNAME2(:INDEX(IPFNAME2,'\',.TRUE.))//TRIM(CTS)//'.'//TRIM(CEXT)
  IF(.NOT.TS_WRITE(LINE)) RETURN

  IF(IBATCH.EQ.0)THEN
   CALL WINDOWSELECT(0)
   CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,NROW,'Progress Timeserie: ')
  ELSEIF(IBATCH.EQ.1)THEN
   WRITE(6,'(A,F10.2,A)') '+Progress Timeserie: ',REAL(100*I)/REAL(NROW),'%' !FR 20131007
  ENDIF

 ENDDO

 TS1COMPUTE=.TRUE.

 END FUNCTION TS1COMPUTE

 !###====================================================================
 LOGICAL FUNCTION TS_READ_IDF(X,Y)
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: X,Y
 INTEGER :: I,IROW,ICOL

 TS_READ_IDF = .FALSE.

 !## all dates are valid
 IPER=1

 OBS%CALC=NODATA

 !## loop over de data van start datum tot eind datum
 DO I=1,NPER
  !## check whether inside period and if idf is opened
  IF(IDF(I)%IU.NE.0) THEN
   CALL IDFIROWICOL(IDF(I),IROW,ICOL,X,Y)
   IF(IROW.NE.0.AND.ICOL.NE.0)OBS(I)%CALC=IDFGETVAL(IDF(I),IROW,ICOL)
  ENDIF
 ENDDO

 TS_READ_IDF = .TRUE.

 END FUNCTION TS_READ_IDF

 !###====================================================================
 LOGICAL FUNCTION TS_READ_IPF()
 !###====================================================================
 IMPLICIT NONE
 REAL :: OBSERVATION
 INTEGER :: IDATE,I,J,IOS,NCOLS,NDATES
 CHARACTER(LEN=MAXLEN) :: CDUM

 TS_READ_IPF = .FALSE.

 IF(IASSF.EQ.1)THEN

  IU(3)=UTL_GETUNIT()

  !## extentie is afhankelijk van de ipf, variabele CEXT dus.
  FNAME=IPFNAME1(:INDEX(IPFNAME1,'\',.TRUE.))//TRIM(CTS)//'.'//TRIM(CEXT)

  CALL OSD_OPEN(IU(3),FILE=FNAME,IOSTAT=IOS,ACTION='READ,DENYWRITE',FORM='FORMATTED')
  IF(IOS.NE.0)THEN
   CALL TS_ERROR_MESSAGE('Error while opening file '//TRIM(FNAME)//'. (1)')
   RETURN
  ENDIF

  !## header
  READ(IU(3),*,IOSTAT=IOS) NDATES
  IF(IOS.NE.0) THEN
   CALL TS_ERROR_MESSAGE('ERROR READING FILE, line 1 in '//TRIM(FNAME)//'. (2)')
   RETURN
  ENDIF
  READ(IU(3),*,IOSTAT=IOS) NCOLS
  IF(IOS.NE.0) THEN
   CALL TS_ERROR_MESSAGE('ERROR READING FILE, line 2 in '//TRIM(FNAME)//'. (2)')
   RETURN
  ENDIF

  !## right here the nodata values for date and observation are read
  !## first colum is date colum, second is the observation colum
  READ(IU(3),*,IOSTAT=IOS) CDUM,NODATA_DAT
  IF(IOS.NE.0) THEN
   CALL TS_ERROR_MESSAGE('ERROR READING FILE, line 3 in  '//TRIM(FNAME)//'. (2A)')
   RETURN
  ENDIF

  READ(IU(3),*,IOSTAT=IOS) CDUM,NODATA
  IF(IOS.NE.0) THEN
   CALL TS_ERROR_MESSAGE('ERROR READING FILE, line 4 in  '//TRIM(FNAME)//'. (2B)')
   RETURN
  ENDIF

  !## skip the rest
  DO I=1,NCOLS-2
   READ(IU(3),*,IOSTAT=IOS) !CDUM,RDUM
   IF(IOS.NE.0) THEN
    CALL TS_ERROR_MESSAGE('ERROR READING FILE, line >4 <ncols in  '//TRIM(FNAME)//'. (2C)')
    RETURN
   ENDIF
  ENDDO

  OBS%OBS=NODATA

  !## Read rest of the file within the range of dates
  DO I=1,NDATES
   READ(IU(3),*,IOSTAT=IOS) IDATE,OBSERVATION
   IF(IOS.NE.0)THEN
    CALL TS_ERROR_MESSAGE('Error reading file in dataline '//TRIM(ITOS(I))//' in file '//TRIM(FNAME)//'. (3)')
    RETURN
   ENDIF
   IDATE=UTL_IDATETOJDATE(IDATE)
   IF(IDATE.GE.JD1.AND.IDATE.LE.JD2) THEN
    J          =IDATE-JD1+1
    OBS(J)%OBS=OBSERVATION
   ENDIF
  ENDDO

  CLOSE(IU(3))

 ELSE
  NODATA_DAT=-999.0
  NODATA    =-999.00
  OBS%OBS   = NODATA
 ENDIF

 TS_READ_IPF = .TRUE.

 END FUNCTION TS_READ_IPF

 !###====================================================================
 LOGICAL FUNCTION TS_WRITE(FNAME)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: I,NRECS,IOS,NFIELDS
 CHARACTER(LEN=MAXLEN),DIMENSION(3) :: CF
 DATA CF/'Date','Calculated','Measure'/

 TS_WRITE = .FALSE.

 !## check if output directory exists or create it
 CALL UTL_CREATEDIR(FNAME(:INDEX(FNAME,'\',.TRUE.)-1))

 !## create the file
 IU(3) = UTL_GETUNIT()
 CALL OSD_OPEN(IU(3),FILE=FNAME,IOSTAT=IOS,STATUS='REPLACE',ACTION='WRITE')
 IF(IOS.NE.0) THEN
  CALL TS_ERROR_MESSAGE('Error while creating file '//TRIM(FNAME)//'. (1)')
  RETURN
 ENDIF

 !## count number of records to be written
 NRECS=0
 DO I=1,NPER
  IF(OBS(I)%OBS.NE.NODATA.OR.OBS(I)%CALC.NE.NODATA)NRECS=NRECS+1
 ENDDO
 !## write the header of the file. The header will be adjusted at the end of the process.
 WRITE(IU(3),'(I10)',IOSTAT=IOS) NRECS
 IF(IOS.NE.0) THEN
  CALL TS_ERROR_MESSAGE('Error while writing to file '//TRIM(FNAME)//'. (2)')
  RETURN
 ENDIF
 IF(IASSF.EQ.0)NFIELDS=2
 IF(IASSF.EQ.1)NFIELDS=3
 WRITE(IU(3),*) NFIELDS
 DO I=1,NFIELDS
  IF(I.EQ.1)LINE=TRIM(CF(I))//','//TRIM(RTOS(NODATA_DAT,'F',0))
  IF(I.NE.1)LINE=TRIM(CF(I))//','//TRIM(RTOS(NODATA,'F',2))
  WRITE(IU(3),*) TRIM(LINE)
 END DO

 DO I=1,NPER
  IF(OBS(I)%OBS.NE.NODATA.OR.OBS(I)%CALC.NE.NODATA)THEN
   LINE=TRIM(ITOS(OBS(I)%IDATE))//','//TRIM(RTOS(OBS(I)%CALC,'F',4))
   IF(IASSF.EQ.1)LINE=TRIM(LINE)//','//TRIM(RTOS(OBS(I)%OBS,'F',4)) 
   WRITE(IU(3),*) TRIM(LINE)
  ENDIF
 ENDDO
 CLOSE(IU(3))

 TS_WRITE = .TRUE.

 END FUNCTION TS_WRITE

 !###====================================================================
 LOGICAL FUNCTION TS_INIT_IPF()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: IOS,I,NC,CFN_N_ELEM
 CHARACTER(LEN=MAXLEN),ALLOCATABLE,DIMENSION(:) :: ATTRIB
 CHARACTER(LEN=256) :: LINE

 TS_INIT_IPF=.FALSE.

 !## read ipf
 IU(1)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(1),FILE=IPFNAME1,IOSTAT=IOS,ACTION='READ,DENYWRITE',FORM='FORMATTED')
 IF(IOS.NE.0) THEN
  CALL TS_ERROR_MESSAGE('File '//TRIM(IPFNAME1)//' can not be opened. (1)'); RETURN
 ENDIF

 !## create outputfile
 IU(2) = UTL_GETUNIT()
 CALL OSD_OPEN(IU(2),FILE=IPFNAME2,ACTION='WRITE',IOSTAT=IOS,STATUS='REPLACE')
 IF(IOS.NE.0) THEN
  CALL TS_ERROR_MESSAGE('File '//TRIM(IPFNAME2)//' can not be created.'); RETURN
 ENDIF

 !## read header
 READ(IU(1),'(A256)',IOSTAT=IOS) LINE
 
 READ(LINE,*,IOSTAT=IOS) NROW
 IF(IOS.EQ.0) THEN

  WRITE(IU(2),*,IOSTAT=IOS) NROW
  IF(IOS.NE.0) THEN
   CALL TS_ERROR_MESSAGE('Error occurred during writing to file '//TRIM(IPFNAME2)//'.'); RETURN
  ENDIF
  READ(IU(1),*,IOSTAT=IOS) NCOL
  IF(IOS.NE.0) THEN
   CALL TS_ERROR_MESSAGE('Error occurred during read of file '//TRIM(IPFNAME1)//'.'); RETURN
  ENDIF
  IF(LCOL.GT.NCOL)THEN
   CALL TS_ERROR_MESSAGE('LCOL should be maximal equal to NCOL of file '//TRIM(IPFNAME1)//'.'); RETURN
  ENDIF

  ALLOCATE(ATTRIB(NCOL+1))

  !## skip the fields
  DO I=1,NCOL
   READ(IU(1),*,IOSTAT=IOS) ATTRIB(I)
   IF(IOS.NE.0) THEN
    CALL TS_ERROR_MESSAGE('Error occurred during read of file '//TRIM(IPFNAME1)//'.'); RETURN
   ENDIF
  ENDDO

  !## read location of the id that points towards the textfiles
  READ(IU(1),*,IOSTAT=IOS) IEXT,CEXT
  IF(IOS.NE.0) THEN
   CALL TS_ERROR_MESSAGE('Error occurred during read of file '//TRIM(IPFNAME1)//'.'); RETURN
  ENDIF
  IF(IEXT.GT.0.AND.LCOL.EQ.0)LCOL=IEXT
  NC=NCOL
  IF(IEXT.EQ.0)THEN
   NC=NC+1
   ATTRIB(NC)='Identifier'
  ENDIF
 
  WRITE(IU(2),*,IOSTAT=IOS) NC
  IF(IOS.NE.0) THEN
   CALL TS_ERROR_MESSAGE('Error occurred during writing to file '//TRIM(IPFNAME2)//'.'); RETURN
  ENDIF

  DO I=1,NC
   WRITE(IU(2),*,IOSTAT=IOS) TRIM(ATTRIB(I))
   IF(IOS.NE.0) THEN
    CALL TS_ERROR_MESSAGE('Error occurred during writing to file '//TRIM(IPFNAME2)//'.'); RETURN
   ENDIF
  ENDDO

  IF(LCOL.EQ.0)THEN
   WRITE(IU(2),'(I5,A)',IOSTAT=IOS) NC,','//TRIM(CEXT)
  ELSE
   WRITE(IU(2),'(I5,A)',IOSTAT=IOS) LCOL,','//TRIM(CEXT)
  ENDIF
  IF(IOS.NE.0) THEN
   CALL TS_ERROR_MESSAGE('Error occurred during writing to file '//TRIM(IPFNAME2)//'.'); RETURN
  ENDIF
 
 !## ipf csv style
 ELSE
 
  NCOL=CFN_N_ELEM(' ,;',3,LINE)
  IF(LCOL.GT.NCOL)THEN
   CALL TS_ERROR_MESSAGE('LCOL should be maximal equal to NCOL of file '//TRIM(IPFNAME1)//'.'); RETURN
  ENDIF

  ALLOCATE(ATTRIB(NCOL+1))
  READ(LINE,*) (ATTRIB(I),I=1,NCOL)

  IEXT=0; CEXT='TXT'

  IF(IEXT.GT.0.AND.LCOL.EQ.0)LCOL=IEXT
  NC=NCOL
  IF(IEXT.EQ.0)THEN; NC=NC+1; ATTRIB(NC)='Identifier'; ENDIF
  
  NROW=0; DO; READ(IU(1),*,IOSTAT=IOS); IF(IOS.NE.0)EXIT; NROW=NROW+1; ENDDO
  REWIND(IU(1)); READ(IU(1),*)

  LINE='"'//TRIM(ATTRIB(1))//'"'
  DO I=2,NC; LINE=TRIM(LINE)//',"'//TRIM(ATTRIB(I))//'"'; ENDDO
  WRITE(IU(2),'(A)') TRIM(LINE)
   
 ENDIF
 
 DEALLOCATE(ATTRIB)

 TS_INIT_IPF=.TRUE.

 END FUNCTION TS_INIT_IPF

 !###====================================================================
 LOGICAL FUNCTION TS_INIT_IDF()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,IGD,NIDF
 LOGICAL :: LEX

 TS_INIT_IDF=.FALSE.

 IF(IBATCH.EQ.0)CALL WINDOWSELECT(0)

 !## no files are opened for each timestep
 IDF(1:NPER)%IU=0

 !## inquire/open all files that are within jd1 and jd2
 NIDF=0
 DO I=1,NPER
  !## gregorian date
  IGD=UTL_JDATETOIDATE(JD1+I-1)

  FNAME=TRIM(TSDIR)//'_'//TRIM(ITOS(IGD))//'_L'//TRIM(ITOS(TSILAY))//'.IDF'
  INQUIRE(FILE=FNAME,EXIST=LEX)
  IF(LEX)THEN
   IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Found '//TRIM(FNAME)//' ...')
   !## read idf header, open file
   IF(.NOT.IDFREAD(IDF(I),FNAME,0))THEN
    CALL TS_ERROR_MESSAGE('Error while reading file '//TRIM(FNAME))
    RETURN
   ENDIF
   NIDF=NIDF+1
  ENDIF

 ENDDO

 IF(NIDF.EQ.0)THEN
  CALL TS_ERROR_MESSAGE('No IDF found that is within the given timeselection.')
 ELSE
  TS_INIT_IDF=.TRUE.
 ENDIF
 IF(IBATCH.EQ.0)THEN
  CALL WINDOWSELECT(0)
  CALL WINDOWOUTSTATUSBAR(4,'Found '//TRIM(ITOS(NIDF))//' IDF"s within time constraint')
 ELSE
  WRITE(*,*) 'Found ',NIDF,' IDF"s within time constraint'
 ENDIF

 END FUNCTION TS_INIT_IDF

 !###====================================================================
 SUBROUTINE TS_CALRESIDUALS()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,N

 N=0; MEAN(1)=NODATA
 DO I=1,NPER
  IF(OBS(I)%CALC.NE.NODATA)THEN
   N=N+1
   IF(N.EQ.1)THEN
    MEAN(1)=OBS(I)%CALC
   ELSE
    MEAN(1)=MEAN(1)+OBS(I)%CALC
   ENDIF
  ENDIF
 ENDDO
 IF(N.GT.0)MEAN(1)=MEAN(1)/REAL(N)

 MEAN(3)=NODATA

 IF(IASSF.EQ.0)RETURN

 N=0; MEAN(2)=NODATA
 DO I=1,NPER
  IF(OBS(I)%OBS.NE.NODATA)THEN
   N=N+1
   IF(N.EQ.1)THEN
    MEAN(2)=OBS(I)%OBS
   ELSE
    MEAN(2)=MEAN(2)+OBS(I)%OBS
   ENDIF
  ENDIF
 ENDDO
 IF(N.GT.0)MEAN(2)=MEAN(2)/REAL(N)

 N=0; MEAN(3)=NODATA
 DO I=1,NPER
  IF(OBS(I)%OBS.NE.NODATA.AND.OBS(I)%CALC.NE.NODATA)THEN
   N=N+1
   IF(N.EQ.1)THEN
    MEAN(3)=OBS(I)%CALC-OBS(I)%OBS
   ELSE
    MEAN(3)=MEAN(3)+(OBS(I)%CALC-OBS(I)%OBS)
   ENDIF
  ENDIF
 ENDDO
 IF(N.GT.0)MEAN(3)=MEAN(3)/REAL(N)

 DO I=1,NPER
  OBS(I)%RES=NODATA
  IF(OBS(I)%OBS.NE.NODATA.AND.OBS(I)%CALC.NE.NODATA)THEN
   OBS(I)%RES=OBS(I)%OBS-OBS(I)%CALC
  ENDIF
 ENDDO

 END SUBROUTINE TS_CALRESIDUALS

 !###====================================================================
 SUBROUTINE TS_ERROR_MESSAGE(TXT)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN)   :: TXT

 IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(TXT),'Error')
 IF(IBATCH.EQ.1)WRITE(*,'(//1X,A/)') TRIM(TXT)

 END SUBROUTINE TS_ERROR_MESSAGE

 !###====================================================================
 SUBROUTINE TS_INIT()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL TS_END()
 ALLOCATE(IDF(NPER))
 DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO
 ALLOCATE(OBS(NPER))
 DO I=1,NPER
  OBS(I)%IDATE=UTL_JDATETOIDATE(JD1+I-1)
 END DO
 ALLOCATE(IPER(NPER))

 END SUBROUTINE TS_INIT

 !###====================================================================
 SUBROUTINE TS_END()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I

 DO I=1,SIZE(IU)
  IF(IU(I).NE.0)CLOSE(IU(I))
 ENDDO

 IF(ALLOCATED(IDF))THEN
  CALL IDFDEALLOCATE(IDF,SIZE(IDF))
  DEALLOCATE(IDF)
 ENDIF

 IF(ALLOCATED(IPER))  DEALLOCATE(IPER)
 IF(ALLOCATED(OBS))   DEALLOCATE(OBS)
 IF(ALLOCATED(STRING))DEALLOCATE(STRING)

 END SUBROUTINE TS_END

END MODULE MOD_TS_CLC
