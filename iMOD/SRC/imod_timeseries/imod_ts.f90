!!  Copyright (C) Stichting Deltares, 2005-2020.
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
MODULE MOD_TS_CLC

USE WINTERACTER
USE MOD_UTL
USE MOD_IDF
USE MOD_OSD
USE MOD_TS_PAR

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION TS1COMPUTE(IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: I,J,K,IOS,IRAT,IRAT1,ITYPE,NROW,NCOL
 TYPE(WIN_MESSAGE) :: MESSAGE
 REAL(KIND=DP_KIND) :: X,Y
 
 MSR%NPER=0
 OBS%NPER=0
 
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
 
 !## get them all
 IF(IOSDIREXISTS(TSDIR(:INDEX(TSDIR,'\',.TRUE.)-1)))THEN
  IF(.NOT.UTL_DIRINFO_POINTER(TSDIR(:INDEX(TSDIR,'\',.TRUE.)-1), &
                         TRIM(TSDIR(INDEX(TSDIR,'\',.TRUE.)+1:))//'_*_L'//TRIM(ITOS(TSILAY))//'.IDF',IDFNAMES,'F',CORDER='N'))RETURN
  IF(.NOT.ASSOCIATED(IDFNAMES))THEN
   CALL TS_ERROR_MESSAGE(IBATCH,'Error iMOD cannot find any appropriate files in '//TSDIR(:INDEX(TSDIR,'\',.TRUE.)-1)//CHAR(13)// &
    ' with wildcard '//TRIM(TSDIR(INDEX(TSDIR,'\',.TRUE.)+1:))//'_*_L'//TRIM(ITOS(TSILAY))//'.IDF'); RETURN
  ENDIF
 ELSE
  CALL TS_ERROR_MESSAGE(IBATCH,'Error iMOD cannot find folder '//TSDIR(:INDEX(TSDIR,'\',.TRUE.)-1)); RETURN
 ENDIF
 IF(SIZE(IDFNAMES).EQ.0)RETURN
 DO I=1,SIZE(IDFNAMES); IDFNAMES(I)=TSDIR(:INDEX(TSDIR,'\',.TRUE.)-1)//'\'//TRIM(IDFNAMES(I)); ENDDO

 !## initialize all global array's - using nper
 CALL TS_INIT()

 !## if not opened all existing idf's correctly
 IF(.NOT.TS_INIT_IDF(IBATCH))RETURN
 !## if not opened and read header of ipf
 IF(.NOT.TS_INIT_IPF(IBATCH,NROW,NCOL))RETURN

 ALLOCATE(STRING(NCOL))

 !## process all the records in the ipf-files
 IRAT=0
 DO I=1,NROW

  CALL WMESSAGEPEEK(ITYPE,MESSAGE)

  READ(IU(1),*,IOSTAT=IOS) (STRING(J),J=1,NCOL)
  IF(IOS.NE.0) THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error occurred during read of file '//TRIM(IPFNAME1)//'.'); RETURN
  ENDIF

  !## assuming these are the x/y coordinates
  CDUM=STRING(1);READ(CDUM,*,IOSTAT=IOS) X
  IF(IOS.NE.0)THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error reading X-coordinate in IROW '//TRIM(ITOS(I))//' in file '//TRIM(IPFNAME1)//'.'); RETURN; ENDIF
  CDUM=STRING(2); READ(CDUM,*,IOSTAT=IOS) Y
  IF(IOS.NE.0)THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error reading Y-coordinate in IROW '//TRIM(ITOS(I))//' in file '//TRIM(IPFNAME1)//'.'); RETURN; ENDIF

  IASSF=0
  IF(IEXT.GT.0)THEN 
   IASSF=1
   !## read associated file (could be spaces within) column
   CDUM='"'//TRIM(STRING(IEXT))//'"'; READ(CDUM,*,IOSTAT=IOS) CTS
   IF(IOS.NE.0)THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error reading identification in IROW '//TRIM(ITOS(I))//' in file '//TRIM(IPFNAME1)//'.'); RETURN; ENDIF
  ELSE
   IF(LCOL.EQ.0)THEN
    CTS='ts_measure'//TRIM(ITOS(I))
    J=INDEX(IPFNAME2,'\',.TRUE.)+1
    K=INDEX(IPFNAME2,'.',.TRUE.)-1
    CTS=IPFNAME2(J:K)//'\'//TRIM(CTS)
   ELSEIF(LCOL.GT.0)THEN
    CTS=STRING(LCOL)  
   ENDIF
  ENDIF

  !## read the timeseries, if available
  IF(.NOT.TS_READ_IPF(IBATCH))RETURN

  !## read the idfs
  IF(.NOT.TS_READ_IDF(X,Y))RETURN

  !## compute residual
  CALL TS_CALRESIDUALS()

  !## write results
  LINE=IPFNAME2(:INDEX(IPFNAME2,'\',.TRUE.))//TRIM(CTS)//'.'//TRIM(CEXT)
  IF(TS_WRITE(IBATCH,LINE,TSDIR(INDEX(TSDIR,'\',.TRUE.)+1:)))THEN

   !## writing comma delimited file
   LINE=TRIM(STRING(1))
   DO J=2,NCOL
    IF(LCOL.EQ.0)THEN
     LINE=TRIM(LINE)//','//TRIM(STRING(J))
    ELSE
     IF(J.EQ.IEXT)LINE=TRIM(LINE)//',"'//TRIM(STRING(J))//'"'
     IF(J.NE.IEXT)LINE=TRIM(LINE)//','//TRIM(STRING(J))
    ENDIF
   ENDDO
   IF(LCOL.EQ.0)LINE=TRIM(LINE)//',"'//TRIM(CTS)//'"' 

   WRITE(IU(2),'(A)',IOSTAT=IOS) TRIM(LINE)
   IF(IOS.NE.0)THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error occurred during writing line '//TRIM(ITOS(I))//' to file '//TRIM(IPFNAME2)//'.'); RETURN; ENDIF

  ENDIF
  
  IF(IBATCH.EQ.0)THEN
   CALL WINDOWSELECT(0)
   CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,NROW,'Progress Timeserie: ')
  ELSEIF(IBATCH.EQ.1)THEN
   WRITE(6,'(A,F10.2,A)') '+Progress Timeserie: ',REAL(100*I)/REAL(NROW),'%' 
  ENDIF

 ENDDO

 TS1COMPUTE=.TRUE.

 END FUNCTION TS1COMPUTE

 !###====================================================================
 LOGICAL FUNCTION TS_READ_IDF(X,Y)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y
 INTEGER :: I,IROW,ICOL

 TS_READ_IDF = .FALSE.

 MSR%OBS=MSR%NODATA

 !## loop over the data from start- to end date
 DO I=1,MSR%NPER
  !## check whether inside period and if idf is opened
  IF(IDF(I)%IU.NE.0) THEN
   CALL IDFIROWICOL(IDF(I),IROW,ICOL,X,Y)
   IF(IROW.NE.0.AND.ICOL.NE.0)MSR%OBS(I)=IDFGETVAL(IDF(I),IROW,ICOL)
  ENDIF
 ENDDO

 TS_READ_IDF = .TRUE.

 END FUNCTION TS_READ_IDF

 !###====================================================================
 LOGICAL FUNCTION TS_READ_IPF(IBATCH)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: OBSERVATION
 INTEGER :: I,J,K,IOS,NC,NDATES
 INTEGER(KIND=DP_KIND) :: IDATE
 
 TS_READ_IPF = .TRUE.

 IF(IASSF.EQ.0)RETURN

 TS_READ_IPF = .FALSE.

 IU(3)=UTL_GETUNIT()

 !## extentie is afhankelijk van de ipf, variabele cext dus.
 FNAME=IPFNAME1(:INDEX(IPFNAME1,'\',.TRUE.))//TRIM(CTS)//'.'//TRIM(CEXT)

 CALL OSD_OPEN(IU(3),FILE=FNAME,IOSTAT=IOS,ACTION='READ,DENYWRITE',FORM='FORMATTED')
 IF(IOS.NE.0)THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error while opening file '//TRIM(FNAME)//'. (1)'); RETURN; ENDIF

 !## header
 READ(IU(3),*,IOSTAT=IOS) NDATES
 IF(IOS.NE.0) THEN; CALL TS_ERROR_MESSAGE(IBATCH,'ERROR READING FILE, line 1 in '//TRIM(FNAME)//'. (2)'); RETURN; ENDIF
 READ(IU(3),*,IOSTAT=IOS) NC
 IF(IOS.NE.0) THEN; CALL TS_ERROR_MESSAGE(IBATCH,'ERROR READING FILE, line 2 in '//TRIM(FNAME)//'. (2)'); RETURN; ENDIF

 ALLOCATE(OBSERVATION(NC-1))

 !## right here the nodata values for date and observation are read
 !## first colum is date colum, second is the observation colum
 DO I=1,NC
  READ(IU(3),*,IOSTAT=IOS) CDUM,OBSERVATION(I)
  IF(IOS.NE.0) THEN; CALL TS_ERROR_MESSAGE(IBATCH,'ERROR READING FILE, line 3 in  '//TRIM(FNAME)//'. (2A)'); RETURN; ENDIF
 ENDDO
 OBS%NODATA=OBSERVATION(TXTCOL)

 ALLOCATE(OBS%OBS(NDATES),OBS%IDATE(NDATES)); OBS%OBS=OBS%NODATA
 
 !## Read rest of the file within the range of dates
 J=0; DO I=1,NDATES
  READ(IU(3),*,IOSTAT=IOS) IDATE,(OBSERVATION(K),K=1,NC-1)
  IF(IOS.NE.0)THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error reading file in dataline '//TRIM(ITOS(I))//' in file '//TRIM(FNAME)//'. (3)'); RETURN; ENDIF
  IF(IDATE.LE.99999999)THEN; IDATE=IDATE*1000000; ENDIF
  IF(IDATE.GE.SDATE.AND.IDATE.LE.EDATE) THEN
   J=J+1
   !## find correct location
   OBS%OBS(J)=OBSERVATION(TXTCOL-1)
   OBS%IDATE(J)=IDATE
  ENDIF
  OBS%NPER=J
 ENDDO

 CLOSE(IU(3))

 TS_READ_IPF = .TRUE.

 END FUNCTION TS_READ_IPF

 !###====================================================================
 LOGICAL FUNCTION TS_WRITE(IBATCH,FNAME,ANAME)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),INTENT(IN) :: FNAME,ANAME
 INTEGER :: I,J,NRECS,IOS,NFIELDS,N1,N2
 INTEGER(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: IDATES
 
 TS_WRITE = .FALSE.

 !## check if output directory exists or create it
 CALL UTL_CREATEDIR(FNAME(:INDEX(FNAME,'\',.TRUE.)-1))

 !## create the file
 IU(3)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(3),FILE=FNAME,IOSTAT=IOS,STATUS='REPLACE',ACTION='WRITE')
 IF(IOS.NE.0) THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error while creating file '//TRIM(FNAME)//'. (1)'); RETURN; ENDIF

 !## count number of records to be written
 IF(IASSF.EQ.0)THEN
  NRECS=MSR%NPER
 ELSE
  !## get unique combinations
  ALLOCATE(IDATES(MSR%NPER+OBS%NPER))
  DO I=1,MSR%NPER; IDATES(I)=MSR%IDATE(I); ENDDO
  I=I-1; DO J=1,OBS%NPER; I=I+1; IDATES(I)=OBS%IDATE(J); ENDDO
  CALL UTL_GETUNIQUE_DINT(IDATES,SIZE(IDATES),NRECS,INT(0,8))
 ENDIF
 
 !## write the header of the file. The header will be adjusted at the end of the process.
 WRITE(IU(3),'(I10)',IOSTAT=IOS) NRECS
 IF(IOS.NE.0) THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error while writing to file '//TRIM(FNAME)//'. (2)'); RETURN; ENDIF
 IF(IASSF.EQ.0)NFIELDS=2
 IF(IASSF.EQ.1)NFIELDS=3
 WRITE(IU(3),*) NFIELDS
 DO I=1,NFIELDS
  CF(3)=TRIM(ANAME) !'Value (-)'
!  CF(2)='Recharge_mm/d'
!  CF(2)='Precipitation_mm/d'
!  CF(2)='Evaporation_mm/d'
!  CF(2)='Flux_mm/d'
  SELECT CASE (I)
   CASE (1); LINE=TRIM(CF(I))//','//TRIM(RTOS(-999.0D0,'F',0))
   CASE (2); LINE=TRIM(CF(I))//','//TRIM(RTOS(OBS%NODATA,'G',8))
   CASE (3); LINE=TRIM(CF(I))//','//TRIM(RTOS(MSR%NODATA,'G',8))
  END SELECT
  WRITE(IU(3),*) TRIM(LINE)
 END DO

 N1=0; N2=0
 IF(IASSF.EQ.0)THEN
  DO I=1,MSR%NPER
   LINE=TRIM(ITOS_DBL(MSR%IDATE(I)))//','//TRIM(RTOS(MSR%OBS(I),'G',8))
   WRITE(IU(3),*) TRIM(LINE)
   N1=N1+1
  ENDDO
 ELSE
  DO I=1,NRECS
   LINE=TRIM(ITOS_DBL(IDATES(I)))
   DO J=1,OBS%NPER
    IF(OBS%IDATE(J).EQ.IDATES(I))THEN
     IF(OBS%OBS(J).NE.OBS%NODATA)THEN
      LINE=TRIM(LINE)//','//TRIM(RTOS(OBS%OBS(J),'G',8)); N2=N2+1; EXIT
     ENDIF
    ENDIF
   ENDDO
   !## nothing found
   IF(J.GT.OBS%NPER)LINE=TRIM(LINE)//','//TRIM(RTOS(OBS%NODATA,'G',8))
   DO J=1,MSR%NPER
    IF(MSR%IDATE(J).EQ.IDATES(I))THEN
     IF(MSR%OBS(J).NE.MSR%NODATA)THEN 
      LINE=TRIM(LINE)//','//TRIM(RTOS(MSR%OBS(J),'G',8)); N1=N1+1; EXIT
     ENDIF
    ENDIF
   ENDDO
   !## nothing found
   IF(J.GT.MSR%NPER)LINE=TRIM(LINE)//','//TRIM(RTOS(MSR%NODATA,'G',8))
   WRITE(IU(3),*) TRIM(LINE)
  ENDDO
 ENDIF
 
 IF(ALLOCATED(IDATES))DEALLOCATE(IDATES)

 CLOSE(IU(3))
 TS_WRITE = .TRUE.

 END FUNCTION TS_WRITE

 !###====================================================================
 LOGICAL FUNCTION TS_INIT_IPF(IBATCH,NROW,NCOL)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER,INTENT(OUT) :: NROW,NCOL
 INTEGER :: IOS,I,NC
 CHARACTER(LEN=256) :: LINE

 TS_INIT_IPF=.FALSE.

 !## read ipf
 IU(1)=UTL_GETUNIT(); CALL OSD_OPEN(IU(1),FILE=IPFNAME1,IOSTAT=IOS,ACTION='READ,DENYWRITE',FORM='FORMATTED')
 IF(IOS.NE.0) THEN; CALL TS_ERROR_MESSAGE(IBATCH,'File '//TRIM(IPFNAME1)//' cannot be opened. (1)'); RETURN; ENDIF

 !## create outputfile
 CALL UTL_CREATEDIR(IPFNAME2(:INDEX(IPFNAME2,'\',.TRUE.)-1))
 IU(2) = UTL_GETUNIT(); CALL OSD_OPEN(IU(2),FILE=IPFNAME2,ACTION='WRITE',IOSTAT=IOS,STATUS='REPLACE')
 IF(IOS.NE.0) THEN; CALL TS_ERROR_MESSAGE(IBATCH,'File '//TRIM(IPFNAME2)//' cannot be created.'); RETURN; ENDIF

 !## read header
 READ(IU(1),'(A256)',IOSTAT=IOS) LINE
 
 READ(LINE,*,IOSTAT=IOS) NROW
 IF(IOS.EQ.0) THEN

  WRITE(IU(2),*,IOSTAT=IOS) NROW
  IF(IOS.NE.0) THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error occurred during writing to file '//TRIM(IPFNAME2)//'.'); RETURN; ENDIF
  READ(IU(1),*,IOSTAT=IOS) NCOL
  IF(IOS.NE.0) THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error occurred during read of file '//TRIM(IPFNAME1)//'.'); RETURN; ENDIF
  IF(LCOL.GT.NCOL)THEN; CALL TS_ERROR_MESSAGE(IBATCH,'LCOL should be maximal equal to NCOL of file '//TRIM(IPFNAME1)//'.'); RETURN; ENDIF

  ALLOCATE(ATTRIB(NCOL+1))

  !## skip the fields
  DO I=1,NCOL
   READ(IU(1),*,IOSTAT=IOS) ATTRIB(I)
   IF(IOS.NE.0) THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error occurred during read of file '//TRIM(IPFNAME1)//'.'); RETURN; ENDIF
  ENDDO

  !## read location of the id that points towards the textfiles
  READ(IU(1),*,IOSTAT=IOS) IEXT,CEXT
  IF(IOS.NE.0) THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error occurred during read of file '//TRIM(IPFNAME1)//'.'); RETURN; ENDIF
  IF(IEXT.GT.0.AND.LCOL.EQ.0)LCOL=IEXT
  NC=NCOL; IF(IEXT.EQ.0)THEN; NC=NC+1; ATTRIB(NC)='Identifier'; ENDIF
 
  WRITE(IU(2),*,IOSTAT=IOS) NC
  IF(IOS.NE.0) THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error occurred during writing to file '//TRIM(IPFNAME2)//'.'); RETURN; ENDIF

  DO I=1,NC
   WRITE(IU(2),*,IOSTAT=IOS) TRIM(ATTRIB(I))
   IF(IOS.NE.0) THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error occurred during writing to file '//TRIM(IPFNAME2)//'.'); RETURN; ENDIF
  ENDDO

  IF(LCOL.EQ.0)THEN
   WRITE(IU(2),'(I5,A)',IOSTAT=IOS) NC,','//TRIM(CEXT)
  ELSE
   WRITE(IU(2),'(I5,A)',IOSTAT=IOS) LCOL,','//TRIM(CEXT)
  ENDIF
  IF(IOS.NE.0) THEN; CALL TS_ERROR_MESSAGE(IBATCH,'Error occurred during writing to file '//TRIM(IPFNAME2)//'.'); RETURN; ENDIF
 
 !## ipf csv style
 ELSE
 
  NCOL=UTL_COUNT_COLUMNS(LINE,' ,;')
  IF(LCOL.GT.NCOL)THEN; CALL TS_ERROR_MESSAGE(IBATCH,'LCOL should be maximal equal to NCOL of file '//TRIM(IPFNAME1)//'.'); RETURN; ENDIF

  ALLOCATE(ATTRIB(NCOL+1))
  READ(LINE,*) (ATTRIB(I),I=1,NCOL)

  IEXT=0; CEXT='TXT'

  IF(IEXT.GT.0.AND.LCOL.EQ.0)LCOL=IEXT
  IF(IEXT.EQ.0)THEN; NC=NCOL+1; ATTRIB(NC)='Identifier'; ENDIF
  
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
 LOGICAL FUNCTION TS_INIT_IDF(IBATCH)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: I,J
 INTEGER(KIND=DP_KIND) :: IDATE
 LOGICAL :: LEX

 TS_INIT_IDF=.FALSE.

 IF(IBATCH.EQ.0)CALL WINDOWSELECT(0)

 !## inquire/open all files that are within jd1 and jd2
 J=0; DO I=1,SIZE(IDFNAMES)

  FNAME=IDFNAMES(I)
  INQUIRE(FILE=FNAME,EXIST=LEX)
  IF(LEX)THEN
   IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Found '//TRIM(FNAME)//' ...')
   !## read idf header, open file
   IF(.NOT.IDFREAD(IDF(I),FNAME,0))THEN
    CALL TS_ERROR_MESSAGE(IBATCH,'Error while reading file '//TRIM(FNAME))
    RETURN
   ENDIF

   IDATE=YMDHMSTOITIME(IDF(I)%IYR,IDF(I)%IMH,IDF(I)%IDY,IDF(I)%IHR,IDF(I)%IMT,IDF(I)%ISC)
         
   IF(IDATE.GE.SDATE.AND.IDATE.LE.EDATE)THEN
    J=J+1; MSR%IDATE(J)=IDATE; MSR%NODATA=IDF(I)%NODATA; IDF(J)%IU=IDF(I)%IU
   ENDIF
   
  ENDIF

 ENDDO
 MSR%NPER=J
 
 IF(MSR%NPER.EQ.0)THEN
  CALL TS_ERROR_MESSAGE(IBATCH,'No IDF found that is within the given timeselection for the current SOURCEDIR folder.')
 ELSE
  TS_INIT_IDF=.TRUE.
 ENDIF
 IF(IBATCH.EQ.0)THEN
  CALL WINDOWSELECT(0)
  CALL WINDOWOUTSTATUSBAR(4,'Found '//TRIM(ITOS(MSR%NPER))//' IDF"s within time constraint')
 ELSE
  WRITE(*,*) 'Found ',MSR%NPER,' IDF"s within time constraint'
 ENDIF

 END FUNCTION TS_INIT_IDF

 !###====================================================================
 SUBROUTINE TS_CALRESIDUALS()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,J

 IF(OBS%NPER.EQ.0)RETURN

 MSR%RES=MSR%NODATA
 DO I=1,MSR%NPER
  
  DO J=1,OBS%NPER
   IF(MSR%IDATE(I).EQ.OBS%IDATE(J))THEN
    MSR%RES(I)=MSR%OBS(I)-OBS%OBS(J); EXIT
   ENDIF
  ENDDO

 ENDDO

 END SUBROUTINE TS_CALRESIDUALS

 !###====================================================================
 SUBROUTINE TS_ERROR_MESSAGE(IBATCH,TXT)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
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
 ALLOCATE(IDF(SIZE(IDFNAMES)))
 DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO
 ALLOCATE(MSR%OBS(SIZE(IDFNAMES)))
 ALLOCATE(MSR%RES(SIZE(IDFNAMES)))
 ALLOCATE(MSR%IDATE(SIZE(IDFNAMES)))

 END SUBROUTINE TS_INIT

 !###====================================================================
 SUBROUTINE TS_END()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I

 DO I=1,SIZE(IU); IF(IU(I).NE.0)CLOSE(IU(I)); ENDDO

 IF(ALLOCATED(IDF))THEN
  CALL IDFDEALLOCATE(IDF,SIZE(IDF)); DEALLOCATE(IDF)
 ENDIF

 IF(ASSOCIATED(MSR%OBS))  DEALLOCATE(MSR%OBS)
 IF(ASSOCIATED(OBS%OBS))  DEALLOCATE(OBS%OBS)
 IF(ASSOCIATED(MSR%RES))  DEALLOCATE(MSR%RES)
 IF(ASSOCIATED(OBS%RES))  DEALLOCATE(OBS%RES)
 IF(ASSOCIATED(MSR%IDATE))DEALLOCATE(MSR%IDATE)
 IF(ASSOCIATED(OBS%IDATE))DEALLOCATE(OBS%IDATE)
 IF(ALLOCATED(STRING))DEALLOCATE(STRING)

 END SUBROUTINE TS_END

END MODULE MOD_TS_CLC
