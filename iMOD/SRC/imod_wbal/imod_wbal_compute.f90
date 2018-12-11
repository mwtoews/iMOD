!!  Copyright (C) Stichting Deltares, 2005-2018.
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
MODULE MOD_WBAL_CLC

USE WINTERACTER
USE RESOURCE
USE MOD_COLOURS
USE MOD_POLYGON_PAR
USE MOD_PREF_PAR, ONLY : PREFVAL
USE IMODVAR, ONLY : DP_KIND,SP_KIND 
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF 
USE MOD_UTL
USE MOD_POLYGON_UTL, ONLY : POLYGON1SAVELOADSHAPE
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_WBAL_PAR
USE MOD_WBAL_ANALYSE, ONLY : WBAL_ANALYSE_GETBALANCETERM
USE MOD_MANAGER_UTL

CONTAINS
 
 !###======================================================================
 LOGICAL FUNCTION WBALCOMPUTE() 
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256) :: FNAME,ROOT
 CHARACTER(LEN=52) :: CDATE,EXT
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: I,J,ILAY,JLAY,IY,IM,ID,IU,ICSV,IBAL,ITYPE, &
   JSYS,NBDG,IBDG,NFILES,IFILES,NU,IROW,ICOL,IDATE,ITRY,MXSYS
 LOGICAL :: LEX,LIP
 INTEGER(KIND=DP_KIND) :: IDATEFULL
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: IDFNAMES
 INTEGER(KIND=DP_KIND),POINTER,DIMENSION(:) :: ITIME,ITIME_BU
 INTEGER(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: LDATES

 WBALCOMPUTE=.FALSE.
 
 IF(IBATCH.EQ.0)THEN
  CALL WDIALOGSELECT(ID_TOOLS)
  !## four processes for waterbalance computation
  CALL WDIALOGRANGEPROGRESSBAR(IDF_PROGRESS1,0,4)
  CALL WDIALOGPUTPROGRESSBAR(IDF_PROGRESS1,0,0)
  CALL WDIALOGRANGEPROGRESSBAR(IDF_PROGRESS2,0,1)
  CALL WDIALOGPUTPROGRESSBAR(IDF_PROGRESS2,0,0)
 ENDIF
 
 CALL IDFNULLIFY(WBALIDF); CALL IDFNULLIFY(IPIDF)
 
 IDF%IU =0
 !## entire area
 IF(WBAL_ISEL.EQ.1)THEN
  SHP%POL%IACT=0
 !## select all polygons
 ELSEIF(WBAL_ISEL.EQ.2)THEN  
  CALL POLYGON1SAVELOADSHAPE(ID_LOADSHAPE,WBAL_GENFNAME,'GEN')
  SHP%POL(1:SHP%NPOL)%IACT=1
 !## usage of idf
 ELSEIF(WBAL_ISEL.EQ.3)THEN
  IF(.NOT.IDFREAD(IDF,WBAL_IDFNAME,0))RETURN
 ENDIF

 I=INDEX(WBAL_OUTFNAME,'.',.TRUE.)
 EXT=WBAL_OUTFNAME(I+1:I+3)
 EXT=UTL_CAP(EXT,'U')
 SELECT CASE (TRIM(EXT))
  CASE ('TXT')
   ICSV=1
  CASE ('CSV')
   ICSV=2
  CASE ('IPF')
   ICSV=3
   IF(WBAL_NLAYER.GT.1)THEN
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You cannot create an IPF file format for more than one modellayer','Error')
    IF(IBATCH.EQ.1)WRITE(*,*) 'You cannot create an IPF file format for more than one modellayer'
    RETURN
   ENDIF
  CASE DEFAULT
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot associate file:'//CHAR(13)// &
   TRIM(WBAL_OUTFNAME),'Error')
   IF(IBATCH.EQ.1)WRITE(*,*) 'Cannot associate file '//TRIM(WBAL_OUTFNAME)
   RETURN
 END SELECT

 !## transient
 IF(WBAL_ISTEADY.EQ.0)THEN
  
  ALLOCATE(ITIME(1000)); ITIME=INT(0,8); NFILES=0
  
  !## get all unique files
  DO IBAL=1,SIZE(WCTP)
   DO JSYS=1,WCTP(IBAL)%NSYS

    ROOT=TRIM(WBAL_RESDIR)//'\'//TRIM(WCTP(IBAL)%BDGNAME)
   
    DO JLAY=1,WBAL_NLAYER
     ILAY=WBAL_ILAYER(JLAY)
     
     !## construct filename
     FNAME=TRIM(WCTP(IBAL)%BDGNAME)
     IF(ASSOCIATED(WCTP(IBAL)%ISYS))FNAME=TRIM(FNAME)//'_SYS'//TRIM(ITOS(WCTP(IBAL)%ISYS(JSYS)))
     FNAME=TRIM(FNAME)//'_*_L'//TRIM(ITOS(ILAY))//'.IDF'

     !## get them all
     IF(UTL_DIRINFO_POINTER(ROOT,FNAME,IDFNAMES,'F',CORDER='N'))THEN; ENDIF
     DO I=1,SIZE(IDFNAMES)
      IDATE=UTL_IDFGETDATE(IDFNAMES(I),IDATEFULL=IDATEFULL) 
      IF(IDATE.NE.0)THEN
       NFILES=NFILES+1
       IF(NFILES.GT.SIZE(ITIME))THEN
        ALLOCATE(ITIME_BU(SIZE(ITIME)+1000))
        DO J=1,SIZE(ITIME); ITIME_BU(J)=ITIME(J); ENDDO
        DEALLOCATE(ITIME); ITIME=>ITIME_BU
       ENDIF
       ITIME(NFILES)=IDATEFULL 
      ENDIF        
     ENDDO
     DEALLOCATE(IDFNAMES)
     
    ENDDO
   ENDDO
  ENDDO
 
  ALLOCATE(LDATES(NFILES)); DO I=1,NFILES; LDATES(I)=ITIME(I); ENDDO; DEALLOCATE(ITIME)
 
  !## get number unique dates
  CALL UTL_GETUNIQUE_DINT(LDATES,NFILES,NU,0); NFILES=NU

 !## steady-state
 ELSE
  
  NFILES=1; ALLOCATE(LDATES(NFILES)); LDATES=INT(0,8)
  
 ENDIF

 IF(IBATCH.EQ.0)CALL WDIALOGPUTPROGRESSBAR(IDF_PROGRESS1,1,1)

 !## create  waterbalance folder
 CALL UTL_CREATEDIR(WBAL_OUTFNAME(:INDEX(WBAL_OUTFNAME,'\',.TRUE.)-1))
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=WBAL_OUTFNAME,STATUS='UNKNOWN',ACTION='WRITE,DENYREAD',IOSTAT=I)
 IF(I.NE.0)THEN
  INQUIRE(UNIT=IU,OPENED=LEX)
  IF(LEX)CLOSE(IU)
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot create the file:'//CHAR(13)// &
   TRIM(WBAL_OUTFNAME)//CHAR(13)// &
   'it is probably already opened in another application','Error')
  IF(IBATCH.EQ.1)WRITE(*,*) 'Cannot create file : '//TRIM(WBAL_OUTFNAME)
  RETURN
 ENDIF

 IF(ICSV.NE.3)THEN
  CALL IOSDATE(IY,IM,ID)
  CALL WDATETOSTRING(CDATE,IY,IM,ID)
  WRITE(IU,'(A/)') 'Waterbalance file created at '//TRIM(CDATE)
 ENDIF

 IF(IBATCH.EQ.0)CALL WDIALOGPUTPROGRESSBAR(IDF_PROGRESS1,1,1)
 
 !## created pointer?
 LIP=.FALSE.

 IF(IBATCH.EQ.0)THEN
  !## maximum is 65535
  CALL WDIALOGRANGEPROGRESSBAR(IDF_PROGRESS2,0,NFILES)
  CALL WDIALOGPUTPROGRESSBAR(IDF_PROGRESS2,0,0)
 ENDIF
 
 !## get units
 WCTP%UNIT='m'
 DO I=1,SIZE(WCTP)
  J=WBAL_ANALYSE_GETBALANCETERM(UTL_CAP(WCTP(I)%BDGNAME,'U'))
  !## unknown budget term
  IF(J.EQ.0)CYCLE
  WCTP(I)%UNIT=TP(J)%UNIT
 !DO J=1,SIZE(CBDG)
 !  IF(INDEX(UTL_CAP(WCTP(I)%BDGNAME,'U'),'BDG'//CBDG(J)).GT.0)WCTP(I)%UNIT='m3/d'
 !ENDDO
 ENDDO
 
 MXSYS=0; DO IBAL=1,SIZE(WCTP)
  MXSYS=MAX(MXSYS,WCTP(IBAL)%NSYS)
 ENDDO

 !## process each date
 DO IFILES=1,NFILES 
 
  !## is current date within specified time window?
  IF(.NOT.WBALCORRECTPERIOD(LDATES(IFILES),CDATE))CYCLE
  
  IF(IBATCH.EQ.0)CALL WDIALOGPUTPROGRESSBAR(IDF_PROGRESS2,1,1)

  !## check each modellayer
  DO JLAY=1,WBAL_NLAYER

   !## current modellayer selected?, take next modellayer if not selecteD
   ILAY=WBAL_ILAYER(JLAY)
   
   IF(ALLOCATED(WBAL))THEN
    WBAL%QIN =0.0D0; WBAL%QOUT=0.0D0; WBAL%IACT=0
    WBEX%QIN =0.0D0; WBEX%QOUT=0.0D0; WBEX%IACT=0
   ENDIF
   
   !## process each waterbalance item
   DO IBAL=1,SIZE(WCTP)

    !## termination possibility
    IF(IBATCH.EQ.0)THEN
     CALL WMESSAGEPEEK(ITYPE,MESSAGE)
     IF(ITYPE.EQ.KEYDOWN.AND.MESSAGE%VALUE1.EQ.KEYESCAPE)RETURN
    ENDIF
     
    DO JSYS=1,WCTP(IBAL)%NSYS
    
     !## number of balance-terms, default=1, for flf only two
     NBDG=0
    
     !## include fuf from above layers, see if modellayer underneath is active too!
     IF(UTL_CAP(WCTP(IBAL)%BDGNAME,'U').EQ.'BDGFLF')NBDG=1

     DO IBDG=0,NBDG
     
      DO ITRY=1,2
      
       !## construct filename
       FNAME=TRIM(WBAL_RESDIR)//'\'//TRIM(WCTP(IBAL)%BDGNAME)//'\'//TRIM(WCTP(IBAL)%BDGNAME)

       !## add system name
       IF(ASSOCIATED(WCTP(IBAL)%ISYS))FNAME=TRIM(FNAME)//'_SYS'//TRIM(ITOS(WCTP(IBAL)%ISYS(JSYS)))

       !## contruct filename
       IF(WBAL_ISTEADY.EQ.1)THEN
        FNAME=TRIM(FNAME)//'_STEADY-STATE'
       ELSE
        FNAME=TRIM(FNAME)//'_'//TRIM(CDATE)
       ENDIF

       FNAME=TRIM(FNAME)//'_L'//TRIM(ITOS(ILAY-IBDG))//'.IDF'
      
       INQUIRE(FILE=FNAME,EXIST=LEX)
       IF(LEX)EXIT
       !## don't search again for steady-state
       IF(WBAL_ISTEADY.EQ.1)EXIT
       IF(CDATE(9:14).EQ.'000000')CDATE(9:14)=''             

      ENDDO
      IF(.NOT.LEX)CYCLE
     
      IF(.NOT.IDFREAD(WBALIDF,TRIM(FNAME),0))RETURN
      IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(FNAME)//'...')
      IF(IBATCH.EQ.1)WRITE(*,*) 'Reading '//TRIM(FNAME)//'...'
        
      !## create pointer - only once to be created
      IF(.NOT.LIP)THEN
       !## copy settings
       CALL WBALFILLPOINTER(WBAL_ISEL)  !## need because of iplist()-array
       !## if IPIDF%X eq 0, nothing to do!
       IF(SUM(IPIDF%X).EQ.0.0D0)THEN
        CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'There is no area defined to sum a waterbalance for, proces stopped.','Error')
        CALL WBALABORT(); RETURN
       ENDIF
       IF(ALLOCATED(WBAL))DEALLOCATE(WBAL); ALLOCATE(WBAL(0:MXTP,MXSYS,NIP))
       IF(ALLOCATED(WBEX))DEALLOCATE(WBEX); ALLOCATE(WBEX(NIP,NIP))
       WBAL%QIN =0.0D0; WBAL%QOUT=0.0D0; WBAL%IACT=0
       WBEX%QIN =0.0D0; WBEX%QOUT=0.0D0; WBEX%IACT=0
       !## don't allocate/read again
       LIP=.TRUE.
       IF(ICSV.EQ.2)CALL WBALCOMPUTECSV_INI(IU)
       IF(ICSV.EQ.3)THEN
        IF(.NOT.WBALCOMPUTEIPF_INI(IU))RETURN
       ENDIF
      ENDIF

      IF(IBDG.EQ.0)THEN
       WBAL(IBAL,JSYS,1:NIP)%FNAME=TRIM(FNAME(INDEX(FNAME,'\',.TRUE.)+1:))
       CALL WBAL1CALC(IBAL-IBDG,JSYS)
       CALL WBAL1CALC_EXCHANGE(IBAL-IBDG)
      ELSE
       !## bdgfuf (flow-upper-face)
       WBAL(0,JSYS,1:NIP)%FNAME=TRIM(FNAME(INDEX(FNAME,'\',.TRUE.)+1:))
       CALL WBAL1CALC(0,JSYS)
      ENDIF
       
      CALL IDFDEALLOCATEX(WBALIDF)
      CLOSE(WBALIDF%IU)
     ENDDO 
    ENDDO 
   ENDDO 
   
   !## write results
   IF(ALLOCATED(WBAL))THEN
    !## data found
    IF(SUM(WBAL%IACT).GT.0)THEN
     !## initialize waterbalance text
     IF(ICSV.EQ.1)THEN
      CALL WBALCOMPUTETXT_INI(IU,TRIM(CDATE),ILAY,JLAY.EQ.WBAL_NLAYER)
      CALL WBALCOMPUTETXT_WRITE(IU,ILAY) 
     ELSEIF(ICSV.EQ.2)THEN
      CALL WBALCOMPUTECSV_WRITE(IU,ILAY,CDATE)
     ELSEIF(ICSV.EQ.3)THEN
      IF(.NOT.WBALCOMPUTEIPF_WRITE(IU,ILAY,CDATE))RETURN
     ENDIF
    ENDIF

   ENDIF
  ENDDO 
 ENDDO 
 
 !## write area of waterbalance at the end of the file in zipformat for csv format only
 IF(ICSV.EQ.2)THEN
  WRITE(IU,'(50A1)') ('-',I=1,50)
  WRITE(IU,'(A)') 'Array of waterbalance area'
  WRITE(IU,'(50A1)') ('-',I=1,50)

  !## put original zone number in ipidf to write to the csv file
  DO ICOL=1,IPIDF%NCOL; DO IROW=1,IPIDF%NROW
   IF(IPIDF%X(ICOL,IROW).GT.0.0D0)THEN
    IPIDF%X(ICOL,IROW)=IPLIST(INT(IPIDF%X(ICOL,IROW)))
   ELSE
    IPIDF%X(ICOL,IROW)=IPIDF%NODATA 
   ENDIF
  ENDDO; ENDDO
  CALL IDFWRITEFREE(IU,IPIDF,1,'T','*') 
 ENDIF
 
 !## deallocate pointer idf
 IF(WBAL_ISEL.EQ.3)CALL IDFDEALLOCATEX(IDF)
 CALL IDFDEALLOCATEX(IPIDF)

 INQUIRE(UNIT=IU,OPENED=LEX); IF(LEX)CLOSE(IU)         
 CALL WBALABORT(); CALL WINDOWOUTSTATUSBAR(4,'')

 WBALCOMPUTE=.TRUE.

 END FUNCTION WBALCOMPUTE

 !###======================================================================
 LOGICAL FUNCTION WBALCORRECTPERIOD(LDATE,CDATE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER(KIND=DP_KIND),INTENT(IN) :: LDATE
 CHARACTER(LEN=*),INTENT(OUT) :: CDATE
 LOGICAL :: LEX
 INTEGER :: ID,IM,IY,K
 
 LEX=.TRUE.

 !## transient
 IF(WBAL_ISTEADY.EQ.0)THEN
  WRITE(CDATE,*) LDATE; CDATE=ADJUSTL(CDATE)
  LEX=LDATE.GE.WBAL_FYR.AND.LDATE.LE.WBAL_TYR
  !## lies within selected year
  IF(LEX.AND.WBAL_NYEAR.GT.0)THEN
   READ(CDATE,'(I4)') IY   
   DO K=1,WBAL_NYEAR; IF(IY.EQ.WBAL_IYEAR(K))EXIT; ENDDO
   LEX=K.LE.WBAL_NYEAR
  ENDIF
  !## check period: if nperiod.gt.0
  IF(LEX.AND.WBAL_NPERIOD.GT.0)THEN
   DO K=1,WBAL_NPERIOD,2
    IF(ID.GE.WBAL_IPERIOD(K,1).AND.ID.LE.WBAL_IPERIOD(K+1,1).AND. &
       IM.GE.WBAL_IPERIOD(K,2).AND.IM.LE.WBAL_IPERIOD(K+1,2))EXIT
   END DO
   LEX=K.LE.WBAL_NPERIOD
  ENDIF
 ELSE
  CDATE='STEADY-STATE'; LEX=.TRUE.
 ENDIF

 WBALCORRECTPERIOD=LEX

 END FUNCTION WBALCORRECTPERIOD

 !###======================================================================
 SUBROUTINE WBALCOMPUTETXT_INI(IU,CDATE,ILAY,LLAY)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL,INTENT(IN) :: LLAY
 INTEGER,INTENT(IN) :: ILAY,IU
 CHARACTER(LEN=*),INTENT(IN) :: CDATE
 INTEGER :: I

 IF(LLAY.OR.WBAL_WBEX.EQ.0)THEN
  WRITE(IU,'(122A1)') ('=',I=1,122)
  WRITE(IU,'(2A)')   'Period : ',CDATE
  IF(WBAL_WBEX.EQ.0)WRITE(IU,'(A,I3)') 'Layer  : ',ILAY
  WRITE(IU,'(122A1)') ('=',I=1,122)
  IF(WBAL_WBEX.EQ.0)THEN
   WRITE(IU,*)
   WRITE(IU,'(2X,A25,2A10,5A15)') 'Waterbalance budget','Q_in','Q_out','Q_in','Q_out','Q_in','Q_out','Area'
   WRITE(IU,'(27X,2A10,5A15)') '%','%','','','','','km2'
   WRITE(IU,*)
  ENDIF
 ENDIF
 
 END SUBROUTINE WBALCOMPUTETXT_INI

 !###======================================================================
 SUBROUTINE WBALCOMPUTETXT_WRITE(IU,ILAY) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,ILAY
 CHARACTER(LEN=256) :: ASTRING
 INTEGER :: JP,IBAL,I,JSYS,IIP,JJP
 REAL(KIND=DP_KIND) :: QIN,QOUT,QACT
 REAL(KIND=DP_KIND),DIMENSION(2) :: QTOT,QRAT
 CHARACTER(LEN=52) :: IDFFNAME
 
 IF(WBAL_WBEX.EQ.0)THEN
 
  DO JP=1,NIP
   WRITE(IU,'(A5,I10,12X,95A1)') 'Zone:',IPLIST(JP),('-',I=1,95)
   WRITE(IU,*)

   QACT=AREA(JP)

   !## get totals
   QTOT=0.0D0
   DO IBAL=1,SIZE(WCTP)
    DO JSYS=1,WCTP(IBAL)%NSYS
     IF(TRIM(WCTP(IBAL)%BDGNAME).EQ.'BDGFLF'.AND.ILAY.GT.1)THEN 
      QIN    =WBAL(0,JSYS,JP)%QIN
      QOUT   =WBAL(0,JSYS,JP)%QOUT
      QTOT(1)=QTOT(1)+QIN
      QTOT(2)=QTOT(2)+QOUT
     ENDIF
     QIN    =WBAL(IBAL,JSYS,JP)%QIN
     QOUT   =WBAL(IBAL,JSYS,JP)%QOUT
     QTOT(1)=QTOT(1)+QIN
     QTOT(2)=QTOT(2)+QOUT
    ENDDO
   ENDDO

   DO IBAL=1,SIZE(WCTP)
    DO JSYS=1,WCTP(IBAL)%NSYS
     IF(WBAL(0,JSYS,JP)%IACT.GT.0)THEN
      IF(TRIM(WCTP(IBAL)%BDGNAME).EQ.'BDGFLF'.AND.ILAY.GT.1)THEN 
       IDFFNAME=TRIM(WBAL(0,JSYS,JP)%FNAME)
       ASTRING='FLUX UPPER FACE '//TRIM(WCTP(IBAL)%UNIT)
       QIN    =WBAL(0,JSYS,JP)%QIN
       QOUT   =WBAL(0,JSYS,JP)%QOUT
       QRAT   =0.0D0
       IF(QTOT(1).NE.0.0D0)QRAT(1)=QIN /QTOT(1)*100.0D0
       IF(QTOT(2).NE.0.0D0)QRAT(2)=QOUT/QTOT(2)*100.0D0
       WRITE(IU,'(2X,A25,2F10.3,5E15.7,A)') TRIM(ASTRING),QRAT(1),QRAT(2),QIN,QOUT,QIN/QACT*1000.0D0,QOUT/QACT*1000.0D0, &
             QACT/1.0D6,' '//TRIM(IDFFNAME)
      ENDIF
     ENDIF
     IF(WBAL(IBAL,JSYS,JP)%IACT.GT.0)THEN
      IDFFNAME=TRIM(WBAL(IBAL,JSYS,JP)%FNAME)
      QIN    =WBAL(IBAL,JSYS,JP)%QIN
      QOUT   =WBAL(IBAL,JSYS,JP)%QOUT
      QRAT   =0.0D0
      IF(QTOT(1).NE.0.0D0)QRAT(1)=QIN /QTOT(1)*100.0D0
      IF(QTOT(2).NE.0.0D0)QRAT(2)=QOUT/QTOT(2)*100.0D0
      WRITE(IU,'(2X,A25,2F10.3,5E15.7,A)') TRIM(WCTP(IBAL)%BDGNAME)//' ('//TRIM(WCTP(IBAL)%UNIT)//')', &
            QRAT(1),QRAT(2),QIN,QOUT,QIN/QACT*1000.0D0,QOUT/QACT*1000.0D0,QACT/1.0D6,' '//TRIM(IDFFNAME)
     ENDIF
    ENDDO
   ENDDO 

   WRITE(IU,'(122A1)') ('-',I=1,122)
   WRITE(IU,'(2X,A25,20X,2E15.7,A15,E15.7)') 'TOTALS',QTOT(1),QTOT(2),'Error :',QTOT(1)+QTOT(2) !,' '//TRIM(WCTP(IBAL)%UNIT)
   WRITE(IU,*)
  ENDDO
  WRITE(IU,'(122A1)') ('=',I=1,122)
 
 !## write exchange between zones
 ELSE

  WRITE(IU,'(/A/)') 'Interchange fluxes '
  WRITE(IU,'(A5,1X,99(I12,1X))') 'Zone:',(IPLIST(JJP),JJP=1,NIP)
  WRITE(IU,'(999A1)') ('-',I=1,5+NIP*13)
  
  DO IIP=1,NIP

   DO JJP=1,NIP
!    TQEX(IIP,JJP)=WBEX(IIP,JJP)%QACT-WBEX(JJP,IIP)%QACT
   ENDDO
!   WRITE(IU,'(I5,1X,99(E12.5,1X))') IPLIST(IIP),(TQEX(IIP,JJP),JJP=1,NIP)
!   IF(LLAY)WRITE(IU,'(I5,1X,99(E12.5,1X))') IPLIST(IIP),(TQEX(IIP,JJP),JJP=1,NIP)

  ENDDO

  WRITE(IU,'(999A1)') ('-',I=1,5+NIP*13)
!  IF(LLAY)WRITE(IU,'(999A1)') ('-',I=1,5+NIP*13)

 ENDIF
  
 END SUBROUTINE WBALCOMPUTETXT_WRITE

 !###======================================================================
 SUBROUTINE WBALCOMPUTECSV_INI(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: IBAL,JSYS,IIP
 
 HSTRING = 'Date_Time,Layer,Zone,Area'
 DO IBAL=1,SIZE(WCTP)
  DO JSYS=1,WCTP(IBAL)%NSYS
   IF(.NOT.ASSOCIATED(WCTP(IBAL)%ISYS))THEN
    HSTRING=TRIM(HSTRING)//','//TRIM(WCTP(IBAL)%BDGNAME)//'_in,'// &
                                TRIM(WCTP(IBAL)%BDGNAME)//'_out'
    IF(TRIM(WCTP(IBAL)%BDGNAME).EQ.'BDGFLF')HSTRING=TRIM(HSTRING)//',bdgftf_in,bdgftf_out'
   ELSE
    HSTRING=TRIM(HSTRING)//','//TRIM(WCTP(IBAL)%BDGNAME)//'_sys'//TRIM(ITOS(WCTP(IBAL)%ISYS(JSYS)))//'_in,'// &
                                TRIM(WCTP(IBAL)%BDGNAME)//'_sys'//TRIM(ITOS(WCTP(IBAL)%ISYS(JSYS)))//'_out' 
   ENDIF
  ENDDO
 ENDDO

 IF(WBAL_WBEX.EQ.1)THEN
  DO IIP=1,NIP 
   WRITE(HSTRING,'(A)') TRIM(HSTRING)//',bdgfcf'//TRIM(ITOS(IPLIST(IIP)))//'_in'// &
                                       ',bdgfcf'//TRIM(ITOS(IPLIST(IIP)))//'_out'
  ENDDO
 ENDIF
 
 WRITE(IU,*)
 WRITE(IU,'(A)') TRIM(HSTRING)

 HSTRING = 'yyyymmddhhmmss,,,km2'
 DO IBAL=1,SIZE(WCTP)
  DO JSYS=1,WCTP(IBAL)%NSYS
   IF(.NOT.ASSOCIATED(WCTP(IBAL)%ISYS))THEN
    HSTRING=TRIM(HSTRING)//','//TRIM(WCTP(IBAL)%UNIT)//','//TRIM(WCTP(IBAL)%UNIT)
    IF(TRIM(WCTP(IBAL)%BDGNAME).EQ.'BDGFLF')HSTRING=TRIM(HSTRING)//','//TRIM(WCTP(IBAL)%UNIT)//','//TRIM(WCTP(IBAL)%UNIT)
   ELSE
    HSTRING=TRIM(HSTRING)//','//TRIM(WCTP(IBAL)%UNIT)//','//TRIM(WCTP(IBAL)%UNIT)
   ENDIF
  ENDDO
 ENDDO
 
 IF(WBAL_WBEX.EQ.1)THEN
  DO IIP=1,NIP
   WRITE(HSTRING,'(A)') TRIM(HSTRING)//',m3/d,m3/d'
  ENDDO
 ENDIF
 
 WRITE(IU,'(A)') TRIM(HSTRING)
  
 END SUBROUTINE WBALCOMPUTECSV_INI
 
 !###======================================================================
 SUBROUTINE WBALCOMPUTECSV_WRITE(IU,ILAY,CDATE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ILAY,IU
 CHARACTER(LEN=*),INTENT(IN) :: CDATE
 INTEGER :: JP,IBAL,JSYS,IIP
 REAL(KIND=DP_KIND) :: QIN,QOUT

 !## write waterbalance results
 DO JP=1,NIP
  HSTRING=TRIM(CDATE)//','//TRIM(ITOS(ILAY))
  HSTRING=TRIM(HSTRING)//','//TRIM(ITOS(INT(IPLIST(JP))))
  !## size of the zone
  HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(AREA(JP)/(1.0E6),'G',7))

  DO IBAL=1,SIZE(WCTP)
   DO JSYS=1,WCTP(IBAL)%NSYS
    QIN =WBAL(IBAL,JSYS,JP)%QIN
    QOUT=WBAL(IBAL,JSYS,JP)%QOUT
    HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QIN,'E',7))
    HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QOUT,'E',7))
    IF(TRIM(WCTP(IBAL)%BDGNAME).EQ.'BDGFLF')THEN
!    IF(IBAL.EQ.3)THEN
     IF(ILAY.GT.1)THEN
      !## upper face
      QIN =WBAL(0,JSYS,JP)%QIN
      QOUT=WBAL(0,JSYS,JP)%QOUT
      HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QIN,'E',7))
      HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QOUT,'E',7))
     ELSE
      HSTRING=TRIM(HSTRING)//',0.0D0,0.0D0' 
     ENDIF
    ENDIF
   ENDDO
  ENDDO

  IF(WBAL_WBEX.EQ.1)THEN
   DO IIP=1,NIP
    HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(WBEX(JP,IIP)%QIN ,'E',7))// &
                           ','//TRIM(RTOS(WBEX(JP,IIP)%QOUT,'E',7))
   ENDDO
  ENDIF
 
  WRITE(IU,'(A)') TRIM(HSTRING)

 ENDDO 
 
 END SUBROUTINE WBALCOMPUTECSV_WRITE

 !###======================================================================
 LOGICAL FUNCTION WBALCOMPUTEIPF_INI(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: IBAL,I,N,NN,JSYS
 REAL(KIND=DP_KIND) :: X,Y
 CHARACTER(LEN=256):: FNAME,LINE
  
 WBALCOMPUTEIPF_INI=.FALSE.
   
 IF(WBAL_ISTEADY.EQ.0)CALL UTL_CREATEDIR(WBAL_OUTFNAME(:INDEX(WBAL_OUTFNAME,'.',.TRUE.)-1))

 !## number of zones/points
 WRITE(IU,*) NIP
 IF(WBAL_ISTEADY.EQ.0)THEN     !## transient
  WRITE(IU,'(A)') '3'
  !## write labels  
  WRITE(IU,'(A)') 'X'
  WRITE(IU,'(A)') 'Y'
  WRITE(IU,'(A)') 'ZONE'
  WRITE(IU,'(A)') '3,TXT'

  ALLOCATE(IUTXT(NIP)); IUTXT=0
  NN=SIZE(WCTP)

  DO I=1,NIP
   IF(.NOT.WBALGETXYZONE(X,Y,I))RETURN 
   LINE=TRIM(WBAL_OUTFNAME(INDEX(WBAL_OUTFNAME,'\',.TRUE.)+1:))
   LINE=TRIM(LINE(:INDEX(LINE,'.',.TRUE.)-1))//'\'//TRIM(ITOS(INT(IPLIST(I))))

   WRITE(IU,*) X,Y,TRIM(LINE)

   FNAME=WBAL_OUTFNAME(:INDEX(WBAL_OUTFNAME,'.',.TRUE.)-1)//'\'//TRIM(ITOS(INT(IPLIST(I))))//'.TXT'
   IUTXT(I)=UTL_GETUNIT()
   CALL OSD_OPEN(IUTXT(I),FILE=FNAME,STATUS='UNKNOWN')
   WRITE(IUTXT(I),*) NPER
   WRITE(IUTXT(I),*) NN*3+1 !## in and out fluxes
   WRITE(IUTXT(I),'(A,G15.8)') 'Date,',HUGE(1.0D0)
   DO IBAL=1,SIZE(WCTP)
    DO JSYS=1,WCTP(IBAL)%NSYS
     IF(.NOT.ASSOCIATED(WCTP(IBAL)%ISYS))THEN
      WRITE(IUTXT(I),'(A,G15.8)') TRIM(WCTP(IBAL)%BDGNAME)//'_in'//TRIM(WCTP(IBAL)%UNIT),HUGE(1.0D0)
      WRITE(IUTXT(I),'(A,G15.8)') TRIM(WCTP(IBAL)%BDGNAME)//'_out'//TRIM(WCTP(IBAL)%UNIT),HUGE(1.0D0)
      WRITE(IUTXT(I),'(A,G15.8)') TRIM(WCTP(IBAL)%BDGNAME)//'_sum'//TRIM(WCTP(IBAL)%UNIT),HUGE(1.0D0)
     ELSE
      LINE=TRIM(WCTP(IBAL)%BDGNAME)//'sys'//TRIM(ITOS(WCTP(IBAL)%ISYS(JSYS)))//'_in'//TRIM(WCTP(IBAL)%UNIT)
      WRITE(IUTXT(I),'(A,G15.8)') TRIM(LINE)//',',HUGE(1.0D0)
      LINE=TRIM(WCTP(IBAL)%BDGNAME)//'_SYS'//TRIM(ITOS(WCTP(IBAL)%ISYS(JSYS)))//'_out'//TRIM(WCTP(IBAL)%UNIT)
      WRITE(IUTXT(I),'(A,G15.8)') TRIM(LINE)//',',HUGE(1.0D0)
      LINE=TRIM(WCTP(IBAL)%BDGNAME)//'_SYS'//TRIM(ITOS(WCTP(IBAL)%ISYS(JSYS)))//'_sum'//TRIM(WCTP(IBAL)%UNIT)
      WRITE(IUTXT(I),'(A,G15.8)') TRIM(LINE)//',',HUGE(1.0D0)
     ENDIF
    ENDDO
   ENDDO
  ENDDO
 
 ELSEIF(WBAL_ISTEADY.EQ.1)THEN !## steady-state
 
  N=SIZE(WCTP)
  WRITE(IU,*) N*3+3
  !## write labels  
  WRITE(IU,'(A)') 'X'
  WRITE(IU,'(A)') 'Y'
  WRITE(IU,'(A)') 'ZONE'
  DO IBAL=1,SIZE(WCTP)
   DO JSYS=1,WCTP(IBAL)%NSYS
    IF(.NOT.ASSOCIATED(WCTP(IBAL)%ISYS))THEN
     WRITE(IU,'(A)') TRIM(WCTP(IBAL)%BDGNAME)//'_in'//TRIM(WCTP(IBAL)%UNIT)
     WRITE(IU,'(A)') TRIM(WCTP(IBAL)%BDGNAME)//'_out'//TRIM(WCTP(IBAL)%UNIT)
     WRITE(IU,'(A)') TRIM(WCTP(IBAL)%BDGNAME)//'_sum'//TRIM(WCTP(IBAL)%UNIT)
    ELSE
     LINE=TRIM(WCTP(IBAL)%BDGNAME)//'_sys'//TRIM(ITOS(WCTP(IBAL)%ISYS(JSYS)))//'_in'//TRIM(WCTP(IBAL)%UNIT)
     WRITE(IU,'(A)') TRIM(LINE)
     LINE=TRIM(WCTP(IBAL)%BDGNAME)//'_sys'//TRIM(ITOS(WCTP(IBAL)%ISYS(JSYS)))//'_out'//TRIM(WCTP(IBAL)%UNIT)
     WRITE(IU,'(A)') TRIM(LINE)
     LINE=TRIM(WCTP(IBAL)%BDGNAME)//'_sys'//TRIM(ITOS(WCTP(IBAL)%ISYS(JSYS)))//'_sum'//TRIM(WCTP(IBAL)%UNIT)
     WRITE(IU,'(A)') TRIM(LINE)
    ENDIF
   ENDDO
  ENDDO
  WRITE(IU,'(A)') '0,txt'

 ENDIF
 
 WBALCOMPUTEIPF_INI=.TRUE.

 END FUNCTION WBALCOMPUTEIPF_INI
 
 !###======================================================================
 LOGICAL FUNCTION WBALGETXYZONE(X,Y,IZ)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IZ
 REAL(KIND=DP_KIND),INTENT(OUT) :: X,Y
 INTEGER :: IROW,ICOL,N
 REAL(KIND=DP_KIND) :: XZ,YZ
 
 WBALGETXYZONE=.FALSE.

 X=0.0D0; Y=0.0D0; N=0
 DO IROW=1,IPIDF%NROW
  DO ICOL=1,IPIDF%NCOL
   IF(IPIDF%X(ICOL,IROW).EQ.IZ)THEN
    CALL IDFGETLOC(IPIDF,IROW,ICOL,XZ,YZ)
    N=N+1
    X=X+XZ
    Y=Y+YZ
   ENDIF
  ENDDO
 ENDDO
 IF(N.EQ.0)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot find zone number '//TRIM(ITOS(IZ)),'Error')
  IF(IBATCH.EQ.1)WRITE(*,'(A)') 'iMOD cannot find zone number '//TRIM(ITOS(IZ))
  RETURN  
 ENDIF
 
 X=X/REAL(N)
 Y=Y/REAL(N)
  
 WBALGETXYZONE=.TRUE.

 END FUNCTION WBALGETXYZONE
 
 !###======================================================================
 LOGICAL FUNCTION WBALCOMPUTEIPF_WRITE(IU,ILAY,CDATE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ILAY,IU
 CHARACTER(LEN=*),INTENT(IN) :: CDATE
 INTEGER :: JP,IBAL,JSYS
 REAL(KIND=DP_KIND) :: QIN,QOUT,X,Y

 WBALCOMPUTEIPF_WRITE=.FALSE.

 DO JP=1,NIP

  IF(WBAL_ISTEADY.EQ.1)THEN
   IF(.NOT.WBALGETXYZONE(X,Y,JP))RETURN 
   HSTRING=TRIM(RTOS(X,'G',7))//','//TRIM(RTOS(Y,'G',7))//','//TRIM(ITOS(INT(IPLIST(JP))))
  ELSE
   HSTRING=TRIM(CDATE)
  ENDIF

  DO IBAL=1,SIZE(WCTP)
   DO JSYS=1,WCTP(IBAL)%NSYS
    QIN =WBAL(IBAL,JSYS,JP)%QIN
    QOUT=WBAL(IBAL,JSYS,JP)%QOUT
    HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QIN,'G',7))
    HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QOUT,'G',7))
    HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QIN+QOUT,'G',7))
    IF(TRIM(WCTP(IBAL)%BDGNAME).EQ.'BDGFLF')THEN
     IF(ILAY.GT.1)THEN
      !## upper face
      QIN =WBAL(0,JSYS,JP)%QIN
      QOUT=WBAL(0,JSYS,JP)%QOUT
      HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QIN,'G',7))
      HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QOUT,'G',7))
      HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QIN+QOUT,'G',7))
     ELSE
      HSTRING=TRIM(HSTRING)//',0.0D0,0.0D0,0.0D0'
     ENDIF
    ENDIF
   ENDDO
  ENDDO
  IF(WBAL_ISTEADY.EQ.1)THEN
   WRITE(IU,'(A)') TRIM(HSTRING)
  ELSE
   WRITE(IUTXT(JP),'(A)') TRIM(HSTRING)
  ENDIF
 ENDDO

 WBALCOMPUTEIPF_WRITE=.TRUE.

 END FUNCTION WBALCOMPUTEIPF_WRITE

 !###======================================================================
 SUBROUTINE WBALABORT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 IF(ASSOCIATED(WBAL_ILAYER))DEALLOCATE(WBAL_ILAYER)
 IF(ASSOCIATED(WBAL_IPERIOD))DEALLOCATE(WBAL_IPERIOD)
 IF(ASSOCIATED(WBAL_IYEAR))DEALLOCATE(WBAL_IYEAR)
 IF(ALLOCATED(IUTXT))DEALLOCATE(IUTXT)
 IF(ALLOCATED(WBAL))DEALLOCATE(WBAL)
 IF(ALLOCATED(WBEX))DEALLOCATE(WBEX)
 IF(ALLOCATED(IPLIST))DEALLOCATE(IPLIST)
 IF(ALLOCATED(AREA))DEALLOCATE(AREA)
 IF(ALLOCATED(WCTP))THEN
  DO I=1,SIZE(WCTP)
   IF(ASSOCIATED(WCTP(I)%ISYS))DEALLOCATE(WCTP(I)%ISYS)
  ENDDO
  DEALLOCATE(WCTP)
 ENDIF
 CALL IDFDEALLOCATEX(WBALIDF)
 CALL IDFDEALLOCATEX(IPIDF) 
 CALL UTL_CLOSEUNITS()
  
 END SUBROUTINE WBALABORT

 !###======================================================================
 SUBROUTINE WBALFILLPOINTER(WBAL_ISEL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: WBAL_ISEL
 INTEGER :: IC1,IC2,IR1,IR2,ICOL,IROW,I,J,K,SHPI
 REAL(KIND=DP_KIND) :: X1,X2,Y1,Y2,XVAL,YVAL,IDFVALUE

 !## copy settings
 CALL IDFCOPY(WBALIDF,IPIDF)
 IF(.NOT.IDFALLOCATEX(IPIDF))THEN; ENDIF
 
 !## entire area
 IF(WBAL_ISEL.EQ.1)THEN
 
  IF(ALLOCATED(IPLIST))DEALLOCATE(IPLIST) 
  ALLOCATE(IPLIST(1),AREA(1))
  IPIDF%X  =1.0D0
  NIP      =1
  IPLIST(1)=INT(1,2)

 !## polygons
 ELSEIF(WBAL_ISEL.EQ.2)THEN
 
  IF(ALLOCATED(IPLIST))DEALLOCATE(IPLIST)  
  ALLOCATE(IPLIST(SHP%NPOL),AREA(SHP%NPOL))
  IPIDF%X=0.0D0
  NIP    =0
  DO SHPI=1,SHP%NPOL
   IF(SHP%POL(SHPI)%IACT.EQ.1.AND.SHP%POL(SHPI)%N.GT.0)THEN
    NIP        =NIP+1
    IPLIST(NIP)=INT(SHPI,2)

    X1=MINVAL(SHP%POL(SHPI)%X(1:SHP%POL(SHPI)%N))
    X2=MAXVAL(SHP%POL(SHPI)%X(1:SHP%POL(SHPI)%N))
    Y1=MINVAL(SHP%POL(SHPI)%Y(1:SHP%POL(SHPI)%N))
    Y2=MAXVAL(SHP%POL(SHPI)%Y(1:SHP%POL(SHPI)%N))

    !## get ofset from xmin/ymin in the number of cells
    !## increase them in case of frf en fff computation
    CALL IDFIROWICOL(IPIDF,IR1,IC1,X1,Y2)
    CALL IDFIROWICOL(IPIDF,IR2,IC2,X2,Y1)
    
    IF(IC2.EQ.0)IC2=IPIDF%NCOL
    IF(IR2.EQ.0)IR2=IPIDF%NROW
     
    IC1=MAX(1,IC1)
    IC2=MIN(IC2,IPIDF%NCOL)
    IR1=MAX(1,IR1)
    IR2=MIN(IR2,IPIDF%NROW)

    DO IROW=IR1,IR2
     DO ICOL=IC1,IC2
      CALL IDFGETLOC(IPIDF,IROW,ICOL,XVAL,YVAL)
      IF(DBL_IGRINSIDEPOLYGON(XVAL,YVAL,SHP%POL(SHPI)%X,SHP%POL(SHPI)%Y,SHP%POL(SHPI)%N).EQ.1)THEN
       IF(IPIDF%X(ICOL,IROW).EQ.0.0D0)THEN
        IPIDF%X(ICOL,IROW)=REAL(SHPI)
       ELSE
        IPIDF%X(ICOL,IROW)=-1.0D0*IPIDF%X(ICOL,IROW)
       ENDIF
      ENDIF
     ENDDO
    ENDDO
   ENDIF
  ENDDO

 !## idf (get absolute values)
 ELSEIF(WBAL_ISEL.EQ.3)THEN

  IPIDF%X=IPIDF%NODATA
  DO IROW=1,IPIDF%NROW
   DO ICOL=1,IPIDF%NCOL
    !## get x/y coordinates
    CALL IDFGETLOC(IPIDF,IROW,ICOL,XVAL,YVAL)
    !## get irow/icol coordinates
    CALL IDFIROWICOL(IDF,IR1,IC1,XVAL,YVAL)
    !## given idf as pointer can have different dimensions/sizes
    IF((IC1.GT.0.AND.IC1.LE.IDF%NCOL).AND.(IR1.GT.0.AND.IR1.LE.IDF%NROW))THEN
     !## get idfvalue
     IDFVALUE=IDFGETVAL(IDF,IR1,IC1)
     IF(IDFVALUE.NE.IDF%NODATA)IPIDF%X(ICOL,IROW)=ABS(IDFVALUE)
    ENDIF
   ENDDO
  ENDDO
  CLOSE(IDF%IU)

  K=0
  DO IROW=1,IPIDF%NROW
   DO ICOL=1,IPIDF%NCOL
    IF(IPIDF%X(ICOL,IROW).NE.IPIDF%NODATA)THEN
     IF(K.EQ.0)THEN
      I=INT(IPIDF%X(ICOL,IROW))
      J=INT(IPIDF%X(ICOL,IROW))
     ELSE
      I=MIN(INT(IPIDF%X(ICOL,IROW)),I)
      J=MAX(INT(IPIDF%X(ICOL,IROW)),J)
     ENDIF
     K=K+1
    ENDIF
   ENDDO
  ENDDO

  IF(ALLOCATED(IPLIST))DEALLOCATE(IPLIST)  
  ALLOCATE(IPLIST(J-I+1),AREA(J-I+1))
  NIP=0
  DO IROW=1,IPIDF%NROW
   DO ICOL=1,IPIDF%NCOL
    IF(IPIDF%X(ICOL,IROW).NE.IPIDF%NODATA)THEN
     DO I=1,NIP
      IF(INT(IPIDF%X(ICOL,IROW),2).EQ.IPLIST(I))EXIT
     ENDDO
     IF(I.GT.NIP)THEN
      NIP        =NIP+1
      IPLIST(NIP)=INT(IPIDF%X(ICOL,IROW),2)
     ENDIF
    ENDIF
   ENDDO
  ENDDO

 ENDIF

 CALL IDFFILLCOMMENT(IPIDF,'Units: dimensionless')
 IF(.NOT.IDFWRITE(IPIDF,WBAL_OUTFNAME(:INDEX(WBAL_OUTFNAME,'.',.TRUE.)-1)//'_ZONES.IDF',1))THEN
 ENDIF

 !## make sure pointers are positive again, for polygons only
 IF(WBAL_ISEL.EQ.2)THEN
  DO IROW=1,IPIDF%NROW; DO ICOL=1,IPIDF%NCOL
   IPIDF%X(ICOL,IROW)=ABS(IPIDF%X(ICOL,IROW))
  ENDDO; ENDDO 
 ENDIF
 
 !## reshuffle pointer to fit iplist()
 AREA=0.0D0
 DO IROW=1,IPIDF%NROW
  DO ICOL=1,IPIDF%NCOL
   DO I=1,NIP
    IF(IPLIST(I).EQ.INT(IPIDF%X(ICOL,IROW),2))THEN
     IPIDF%X(ICOL,IROW)=REAL(I)
     AREA(I)=AREA(I)+IDFGETAREA(IPIDF,ICOL,IROW)
     EXIT
    ENDIF
   ENDDO
   !## reset nodata value to zero!
   IF(IPIDF%X(ICOL,IROW).EQ.IPIDF%NODATA)IPIDF%X(ICOL,IROW)=0.0D0
  ENDDO
 ENDDO
 
 END SUBROUTINE WBALFILLPOINTER

 !###======================================================================
 SUBROUTINE WBAL1CALC(IBAL,JSYS)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBAL,JSYS
 INTEGER :: ICOL,IROW,JP,JJP,JROW,JCOL
 REAL(KIND=DP_KIND) :: IDFVAL,FCT,X,Y
 LOGICAL :: LEX,LFRF,LFFF,LFUF

 WBAL(IBAL,JSYS,1:NIP)%QIN =0.0D0
 WBAL(IBAL,JSYS,1:NIP)%QOUT=0.0D0
 WBAL(IBAL,JSYS,1:NIP)%IACT=1

 LFUF=.FALSE.; LFRF=.FALSE.; LFFF=.FALSE.
 
 IF(IBAL.EQ.0)THEN
  LFUF=.TRUE.
 ELSE
  IF(TRIM(WCTP(IBAL)%BDGNAME).EQ.'BDGFRF')LFRF=.TRUE.
  IF(TRIM(WCTP(IBAL)%BDGNAME).EQ.'BDGFFF')LFFF=.TRUE.
 ENDIF
 
 DO IROW=1,IPIDF%NROW
  DO ICOL=1,IPIDF%NCOL

   FCT=1.0D0
   LEX=.FALSE.
   JP =INT(IPIDF%X(ICOL,IROW))
   JJP=0

   IF(LFRF)THEN
    IF(ICOL.LT.IPIDF%NCOL)THEN

     IF(JP.NE.0.AND.IPIDF%X(ICOL+1,IROW).NE.JP)THEN
      LEX=.TRUE.
      IF(IPIDF%X(ICOL+1,IROW).NE.0.0D0)JJP=INT(IPIDF%X(ICOL+1,IROW))
     ELSE
      JP=INT(IPIDF%X(ICOL+1,IROW))
      IF(JP.NE.0.AND.IPIDF%X(ICOL,IROW).NE.JP)THEN
       LEX=.TRUE.
       FCT=-1.0D0
      ENDIF
     ENDIF

    ENDIF

   !## fff - front face
   ELSEIF(LFFF)THEN
    IF(IROW.LT.IPIDF%NROW)THEN

     IF(JP.NE.0.AND.IPIDF%X(ICOL,IROW+1).NE.JP)THEN
      LEX=.TRUE.
      IF(IPIDF%X(ICOL,IROW+1).NE.0)JJP=INT(IPIDF%X(ICOL,IROW+1))
     ELSE
      JP=INT(IPIDF%X(ICOL,IROW+1))
      IF(JP.NE.0.AND.IPIDF%X(ICOL,IROW).NE.JP)THEN
       LEX=.TRUE.
       FCT=-1.0D0
      ENDIF
     ENDIF

    ENDIF

   !## flow top face
   ELSEIF(LFUF)THEN

    IF(JP.NE.0)LEX=.TRUE.
    FCT=-1.0D0
   
   !## rest
   ELSE

    IF(JP.NE.0)LEX=.TRUE.

   ENDIF

   IF(LEX)THEN

    !## get x/y coordinates based upon ipidf coordinates
    CALL IDFGETLOC(IPIDF,IROW,ICOL,X,Y)
    CALL IDFIROWICOL(WBALIDF,JROW,JCOL,X,Y)
    
    IF(JROW.GE.1.AND.JROW.LE.IPIDF%NROW.AND. &
       JCOL.GE.1.AND.JCOL.LE.IPIDF%NCOL)THEN

     IDFVAL=IDFGETVAL(WBALIDF,JROW,JCOL)
     IF(IDFVAL.EQ.WBALIDF%NODATA)IDFVAL=0.0D0
     IDFVAL=FCT*IDFVAL

     IF(IDFVAL.NE.0.0D0)THEN
      IF(IDFVAL.LT.0.0D0)WBAL(IBAL,JSYS,JP)%QOUT=WBAL(IBAL,JSYS,JP)%QOUT+IDFVAL
      IF(IDFVAL.GT.0.0D0)WBAL(IBAL,JSYS,JP)%QIN =WBAL(IBAL,JSYS,JP)%QIN +IDFVAL
      IF(JJP.NE.0)THEN
       IDFVAL=(-1.0D0*FCT)*IDFVAL
       IF(IDFVAL.LT.0.0D0)WBAL(IBAL,JSYS,JJP)%QOUT=WBAL(IBAL,JSYS,JJP)%QOUT+IDFVAL
       IF(IDFVAL.GT.0.0D0)WBAL(IBAL,JSYS,JJP)%QIN =WBAL(IBAL,JSYS,JJP)%QIN +IDFVAL
      ENDIF
     ENDIF
    ENDIF
    
   ENDIF
  ENDDO
 ENDDO

 END SUBROUTINE WBAL1CALC

 !###======================================================================
 SUBROUTINE WBAL1CALC_EXCHANGE(IBAL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBAL
 INTEGER :: ICOL,IROW,IIP,JJP,JROW,JCOL
 REAL(KIND=DP_KIND) :: IDFVAL,X,Y
 LOGICAL :: LEX,LFRF,LFFF

 LFRF=.FALSE.; IF(TRIM(WCTP(IBAL)%BDGNAME).EQ.'BDGFRF')LFRF=.TRUE.
 LFFF=.FALSE.; IF(TRIM(WCTP(IBAL)%BDGNAME).EQ.'BDGFFF')LFFF=.TRUE.

 IF(.NOT.LFRF.AND..NOT.LFFF)RETURN
 
 DO IROW=1,IPIDF%NROW
  DO ICOL=1,IPIDF%NCOL

   IIP=INT(IPIDF%X(ICOL,IROW)); IF(IIP.EQ.0)CYCLE
   LEX=.FALSE.
    
   IF(LFRF)THEN
   
    IF(ICOL.LT.IPIDF%NCOL)THEN
     JJP =INT(IPIDF%X(ICOL+1,IROW))
     !## different zone next to current zone
     IF(IIP.NE.JJP.AND.JJP.NE.0)LEX=.TRUE.
    ENDIF

   !## fff - front face
   ELSEIF(LFFF)THEN
   
    IF(IROW.LT.IPIDF%NROW)THEN
     JJP=INT(IPIDF%X(ICOL,IROW+1))
     IF(IIP.NE.JJP.AND.JJP.NE.0)LEX=.TRUE.
    ENDIF
   
   ENDIF

   IF(LEX)THEN

    !## get x/y coordinates based upon ipidf coordinates
    CALL IDFGETLOC(IPIDF,IROW,ICOL,X,Y)
    CALL IDFIROWICOL(WBALIDF,JROW,JCOL,X,Y)
    
    IF(JROW.GE.1.AND.JROW.LE.IPIDF%NROW.AND. &
       JCOL.GE.1.AND.JCOL.LE.IPIDF%NCOL)THEN
       
     IDFVAL=IDFGETVAL(WBALIDF,JROW,JCOL)
     IF(IDFVAL.EQ.WBALIDF%NODATA)IDFVAL=0.0D0

     IF(IDFVAL.GT.0.0D0)THEN
      WBEX(IIP,JJP)%QIN =WBEX(IIP,JJP)%QIN +IDFVAL
      WBEX(JJP,IIP)%QOUT=WBEX(JJP,IIP)%QOUT-IDFVAL
     ELSE
      WBEX(IIP,JJP)%QOUT=WBEX(IIP,JJP)%QOUT+IDFVAL
      WBEX(JJP,IIP)%QIN =WBEX(JJP,IIP)%QIN -IDFVAL
     ENDIF
     
    ENDIF
    
   ENDIF

  ENDDO
 ENDDO
  
 END SUBROUTINE WBAL1CALC_EXCHANGE

END MODULE MOD_WBAL_CLC

