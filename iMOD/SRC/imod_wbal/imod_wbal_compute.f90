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
!!
MODULE MOD_WBAL_CLC

USE WINTERACTER
USE RESOURCE, ONLY : ID_LOADSHAPE
USE MOD_COLOURS
USE MOD_POLYGON_PAR
USE MOD_PREF_PAR, ONLY : PREFVAL
USE IMODVAR, ONLY : MXTP,TP,MXSYS
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFALLOCATEX,IDFCOPY,IDFWRITE,IDFGETVAL,IDFDEALLOCATEX,IDFIROWICOL,IDFGETLOC, &
     IDFFILLCOMMENT,IDFNULLIFY,UTL_IDFGETDATE
USE MOD_UTL, ONLY : ITOS,UTL_GETUNIT,RTOS,UTL_CAP,JD,IDATETOJDATE,UTL_SUBST,GDATE,JDATETOIDATE,CLOSEUNITS, &
     UTL_INSIDEPOLYGON,UTL_CREATEDIR,GDATETOJDATE,UTL_DIRINFO_POINTER,JDATETOFDATE,IDATETOGDATE
USE MOD_POLYGON_UTL, ONLY : POLYGON1SAVELOADSHAPE
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_PROFILE_UTL, ONLY : GRAPH 
USE MOD_WBAL_PAR
USE IMOD, ONLY : IDFINIT

TYPE WBALTYPE
 REAL :: QIN,QOUT,QACT
 INTEGER :: IACT
 CHARACTER(LEN=52) :: FNAME
END TYPE WBALTYPE
TYPE(WBALTYPE),ALLOCATABLE,DIMENSION(:,:,:) :: WBAL
INTEGER(KIND=2),ALLOCATABLE,DIMENSION(:) :: IPLIST
INTEGER,ALLOCATABLE,DIMENSION(:) :: IUTXT
INTEGER(KIND=2) :: IP
INTEGER :: NIP,NWBAL,NPER
CHARACTER(LEN=2000),PRIVATE :: HSTRING

TYPE(IDFOBJ) :: IDF,WBALIDF,IPIDF

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION WBALCOMPUTE() 
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256) :: FNAME,ROOT
 CHARACTER(LEN=20) :: CDATE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: I,J,K,ILAY,JLAY,IY,IM,ID,IU,ICSV,IBAL,ITYPE, &
   IWIN,JSYS,NBDG,IBDG,IG,JG,NFILES,IFILES
 LOGICAL :: LEX,LIP
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: IDFNAMES
 
 WBALCOMPUTE=.FALSE.

 ILAY=WBAL_ILAYER(1)

 DO IBAL=1,MXTP  
  !## current balance term not active
  IF(TP(IBAL)%IACT.EQ.0)CYCLE
  DO JSYS=1,TP(IBAL)%NSYS

   ROOT=TRIM(WBAL_RESDIR)//'\'//TRIM(TP(IBAL)%ACRNM)

   !## transient
   IF(WBAL_ISTEADY.EQ.0)THEN

    FNAME=TRIM(TP(IBAL)%ACRNM)//'_????????'
    IF(TP(IBAL)%ISYS(JSYS).GT.0)FNAME=TRIM(FNAME)//'_SYS'//TRIM(ITOS(TP(IBAL)%ISYS(JSYS)))
    FNAME=TRIM(FNAME)//'_L'//TRIM(ITOS(ILAY))//'.IDF'
    CALL IOSDIRCOUNT(ROOT,FNAME,NFILES)

    !## try to read extent date notation - if nothing has been found
    IF(NFILES.EQ.0)THEN
     FNAME=TRIM(TP(IBAL)%ACRNM)//'_??????????????'
     IF(TP(IBAL)%ISYS(JSYS).GT.0)FNAME=TRIM(FNAME)//'_SYS'//TRIM(ITOS(TP(IBAL)%ISYS(JSYS)))
     FNAME=TRIM(FNAME)//'_L'//TRIM(ITOS(ILAY))//'.IDF'   
     CALL IOSDIRCOUNT(ROOT,FNAME,NFILES)
     IF(NFILES.EQ.0)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Nothing found','Error')
     ENDIF
    ENDIF

   !## steady-state
   ELSE

    FNAME=TRIM(TP(IBAL)%ACRNM)//'_STEADY-STATE'
    IF(TP(IBAL)%ISYS(JSYS).GT.0)FNAME=TRIM(FNAME)//'_SYS'//TRIM(ITOS(TP(IBAL)%ISYS(JSYS)))
    FNAME=TRIM(FNAME)//'_L'//TRIM(ITOS(ILAY))//'.IDF'
    CALL IOSDIRCOUNT(ROOT,FNAME,NFILES)

   ENDIF

   !## get list
   IF(.NOT.UTL_DIRINFO_POINTER(ROOT,FNAME,IDFNAMES,'F'))RETURN
   !## do not look any further
   IF(NFILES.GT.0)EXIT
   
  ENDDO
 ENDDO
 
! JD1=0; JD2=0
! !## transient
! IF(WBAL_ISTEADY.EQ.0)THEN
!  JD1=IDATETOJDATE(WBAL_FYR); JD2=IDATETOJDATE(WBAL_TYR)
! ENDIF
 
 CALL IDFNULLIFY(WBALIDF); CALL IDFNULLIFY(IPIDF)
 
 IDF%IU =0
 !## entire area
 IF(WBAL_ISEL.EQ.1)THEN
  SHPIACT=0
 !## select all polygons
 ELSEIF(WBAL_ISEL.EQ.2)THEN
  CALL POLYGON1SAVELOADSHAPE(ID_LOADSHAPE,0,WBAL_GENFNAME)
  SHPIACT(1:SHPNO)=1
 !## usage of idf
 ELSEIF(WBAL_ISEL.EQ.3)THEN
  IF(.NOT.IDFREAD(IDF,WBAL_IDFNAME,0))RETURN
 ENDIF

 I=INDEX(WBAL_OUTFNAME,'.',.TRUE.)
 SELECT CASE (WBAL_OUTFNAME(I+1:I+3))
  CASE ('TXT')
   ICSV=1
  CASE ('CSV')
   ICSV=2
  CASE ('IPF')
   ICSV=3
   IF(WBAL_NLAYER.GT.1)THEN
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You can not create an IPF file format for more than one modellayer','Error')
    IF(IBATCH.EQ.1)WRITE(*,*) 'You can not create an IPF file format for more than one modellayer'
    RETURN
   ENDIF
  CASE DEFAULT
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not associate file:'//CHAR(13)// &
   TRIM(WBAL_OUTFNAME),'Error')
   IF(IBATCH.EQ.1)WRITE(*,*) 'Can not associate file '//TRIM(WBAL_OUTFNAME)
   RETURN
 END SELECT

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=WBAL_OUTFNAME,STATUS='UNKNOWN',ACTION='WRITE,DENYREAD',IOSTAT=I)
 IF(I.NE.0)THEN
  INQUIRE(UNIT=IU,OPENED=LEX)
  IF(LEX)CLOSE(IU)
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not create the file:'//CHAR(13)// &
   TRIM(WBAL_OUTFNAME)//CHAR(13)// &
   'it is probably allready opened in another application','Error')
  IF(IBATCH.EQ.1)WRITE(*,*) 'Can not create file : '//TRIM(WBAL_OUTFNAME)
  RETURN
 ENDIF

 IF(ICSV.NE.3)THEN
  CALL IOSDATE(IY,IM,ID)
  CALL WDATETOSTRING(CDATE,IY,IM,ID)
  WRITE(IU,'(A/)') 'Waterbalance file created at '//CDATE
 
  IF(SUM(SHPIACT).EQ.0)THEN
   IF(IDF%IU.EQ.0)WRITE(IU,'(A)') 'Waterbalance for entire size of model.'
   IF(IDF%IU.GT.0)THEN
    WRITE(IU,'(A)') 'Waterbalance for values not equal to nodata value within:'
    WRITE(IU,'(A)') TRIM(WBAL_IDFNAME)
   ENDIF
  ELSE
   WRITE(IU,'(A)') 'Waterbalance for selected area given by polygon(s) read from :'
   WRITE(IU,'(A)') TRIM(WBAL_GENFNAME)
  ENDIF

  WRITE(IU,'(/A/)') 'Bear in mind that disclosure of the waterbalance might be caused by absent budget terms !'
 
 ENDIF
 
 IF(ICSV.EQ.2)CALL WBALCOMPUTECSV_INI(IU)
 
 !## created pointer?
 LIP=.FALSE.
 
 IG=0
 IF(IBATCH.EQ.0)THEN
  !## position in graph (pos=1 initial empty location)
  IF(WBAL_ISTEADY.EQ.1)THEN
   DO I=1,SIZE(GRAPH,1)
    DO J=1,SIZE(GRAPH,2)
     GRAPH(I,J)%RX(1)=0.0
     DO K=2,SIZE(GRAPH(I,J)%RX)
      IF(MOD(K,2).EQ.0)GRAPH(I,J)%RX(K)=GRAPH(I,J)%RX(K-1)+0.1
      IF(MOD(K,2).NE.0)GRAPH(I,J)%RX(K)=GRAPH(I,J)%RX(K-1)+0.5
     ENDDO
    ENDDO
   ENDDO
   IG=1
  ENDIF
 ENDIF

 NPER=0
 DO IFILES=1,NFILES
  IF(.NOT.WBALCORRECTPERIOD(IDFNAMES(IFILES),CDATE))CYCLE; NPER=NPER+1 
 ENDDO
 
 !## jg=ig for modellayer purposes
 JG=IG
 
 !## process each date
 DO IFILES=1,NFILES 
 
  !## is current date within specified time window?
  IF(.NOT.WBALCORRECTPERIOD(IDFNAMES(IFILES),CDATE))CYCLE
  
  !## check each modellayer
  DO JLAY=1,WBAL_NLAYER

   !## reset ig for modellayers
   IG=JG

   !## current modellayer selected?, take next modellayer if not selecteD
   ILAY=WBAL_ILAYER(JLAY)
   
   IF(ALLOCATED(WBAL))THEN
    WBAL%QIN =0.0
    WBAL%QOUT=0.0
    WBAL%QACT=0.0
    WBAL%IACT=0
   ENDIF

   !## process each waterbalance item
   DO IBAL=1,MXTP
   
    !## current balance term not active
    IF(TP(IBAL)%IACT.EQ.0)CYCLE

    !## termination possibility
    IF(IBATCH.EQ.0)THEN
     CALL WMESSAGEPEEK(ITYPE,MESSAGE)
     IF(ITYPE.EQ.KEYDOWN.AND.MESSAGE%VALUE1.EQ.KEYESCAPE)RETURN
    ENDIF
     
    DO JSYS=1,TP(IBAL)%NSYS
    
     !## number of balance-terms, default=1, for flf only two
     NBDG=0
    
     !## include fuf from above layers, see if modellayer underneath is active too!
     IF(IBAL.EQ.3.AND.ILAY.GT.1)NBDG=1 

     DO IBDG=0,NBDG
     
      FNAME=TRIM(WBAL_RESDIR)//'\'//TRIM(TP(IBAL)%ACRNM)//'\'//TRIM(TP(IBAL)%ACRNM)
      IF(TP(IBAL)%ISYS(JSYS).GT.0)THEN
       FNAME=TRIM(FNAME)//'_sys'//TRIM(ITOS(TP(IBAL)%ISYS(JSYS)))
      ENDIF

      !## contruct filename
      IF(WBAL_ISTEADY.EQ.1)THEN
       FNAME=TRIM(FNAME)//'_STEADY-STATE_L'//TRIM(ITOS(ILAY-IBDG))//'.IDF'
      ELSE
       FNAME=TRIM(FNAME)//'_'//TRIM(CDATE)//'_L'//TRIM(ITOS(ILAY-IBDG))//'.IDF'
      ENDIF
     
      INQUIRE(FILE=FNAME,EXIST=LEX)
      IF(.NOT.LEX)CYCLE
     
      IF(.NOT.IDFREAD(WBALIDF,TRIM(FNAME),0))RETURN
      IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(FNAME)//'...')
      IF(IBATCH.EQ.1)WRITE(*,*) 'Reading '//TRIM(FNAME)//'...'
        
      !## create pointer - only once to be created
      IF(.NOT.LIP)THEN
       !## copy settings
       CALL WBALFILLPOINTER(WBAL_ISEL)  !## need because of iplist()-array
       !## if IPIDF%X eq 0, nothing to do!
       IF(SUM(IPIDF%X).EQ.0.0)THEN
        CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'There is no area defined to sum a waterbalance for !','Error')
        CALL WBALABORT()
        RETURN
       ENDIF
       IF(ALLOCATED(WBAL))DEALLOCATE(WBAL)  
       ALLOCATE(WBAL(0:MXTP,MXSYS,NIP))
       WBAL%QIN =0.0
       WBAL%QOUT=0.0
       WBAL%QACT=0.0
       WBAL%IACT=0
       !## don't allocate/read again
       LIP=.TRUE.
       IF(ICSV.EQ.3)CALL WBALCOMPUTEIPF_INI(IU)
      ENDIF

      IF(IBDG.EQ.0)THEN
       WBAL(IBAL,JSYS,1:NIP)%FNAME=TRIM(FNAME(INDEX(FNAME,'\',.TRUE.)+1:))
       CALL WBAL1CALC(IBAL-IBDG,JSYS)
      ELSE
       !## bdgfuf (flow-upper-face)
       WBAL(0,JSYS,1:NIP)%FNAME=TRIM(FNAME(INDEX(FNAME,'\',.TRUE.)+1:))
       CALL WBAL1CALC(0,JSYS)
      ENDIF
       
      CALL IDFDEALLOCATEX(WBALIDF)
      CLOSE(WBALIDF%IU)
     ENDDO !## DO IBDG=1,NBDG
    ENDDO !## DO JSYS=1,TP(IBAL)%NSYS(IBAL)
   ENDDO !## DO IBAL=1,MXTP
   !## write results
   IF(ALLOCATED(WBAL))THEN
    !## data found
    IF(SUM(WBAL%IACT).GT.0)THEN
     !## initialize waterbalance text
     IF(ICSV.EQ.1)THEN
      CALL WBALCOMPUTETXT_INI(IU,TRIM(CDATE),ILAY)
      CALL WBALCOMPUTETXT_WRITE(IU,ILAY)
     ELSEIF(ICSV.EQ.2)THEN
      CALL WBALCOMPUTECSV_WRITE(IU,ILAY,CDATE)
     ELSEIF(ICSV.EQ.3)THEN
      CALL WBALCOMPUTEIPF_WRITE(IU,ILAY,CDATE)
     ENDIF
    ENDIF

!!## skip graph
!GOTO 10
!
!    !## fill in graph-object
!    IF(IBATCH.EQ.0)THEN
!     JBAL=0
!     DO IBAL=1,MXTP
!      IF(TP(IBAL)%IACT.EQ.0)CYCLE
!      IG  =IG+1
!      JBAL=JBAL+1
!!      !## transient, fill in previous value for current date
!!      IF(ISTEADY.EQ.0.AND.IG.GT.2)THEN
!!       GRAPH(JBAL,JLAY)%RX(IG)=REAL(JDATE)       
!!       GRAPH(JBAL,JLAY)%RX(IG)=QMAX
!!      ENDIF
!!      !## transient, use julian date
!!      IF(ISTEADY.EQ.0)THEN
!!       GRAPH(JBAL,JLAY)%RX(IG)=REAL(JDATE)
!!      ENDIF
!      QMIN=SUM(WBAL(IBAL,1:TP(IBAL)%NSYS,1:NIP)%QOUT) !## negative
!      QMAX=SUM(WBAL(IBAL,1:TP(IBAL)%NSYS,1:NIP)%QIN)  !## positive
!      GRAPH(JBAL,JLAY)%RY(IG)=QMIN
!      IF(WBAL_ISTEADY.EQ.1)THEN
!       GRAPH(JBAL,JLAY)%RY(IG+1)=QMAX
!!      ENDIF
!!      IF(ISTEADY.EQ.1)
!       IG=IG+1
!      ENDIF
!     ENDDO
!    ENDIF
!
!10 CONTINUE

   ENDIF
  ENDDO !## DO JLAY=1,NL
  JG=IG
 ENDDO !## DO JDATE=JD1,JD2
 
 !## deallocate pointer idf
 IF(WBAL_ISEL.EQ.3)CALL IDFDEALLOCATEX(IDF)
 CALL IDFDEALLOCATEX(IPIDF)

 IF(IBATCH.EQ.0)THEN
  INQUIRE(UNIT=IU,OPENED=LEX); IF(LEX)CLOSE(IU)         
  CALL WBALABORT(); CALL WINDOWOUTSTATUSBAR(4,'')
  IF(ICSV.EQ.3)THEN
   CALL IDFINIT(IDFNAMEGIVEN=WBAL_OUTFNAME,LPLOT=.TRUE.)
  ELSE
   IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=WBAL_OUTFNAME,STATUS='OLD',IOSTAT=I)
   IF(I.NE.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not view the created file : '//CHAR(13)// &
     TRIM(WBAL_OUTFNAME)//'.'//CHAR(13)//'It is probably opened allready in another application','Error')
   ELSE
    CLOSE(IU)
    CALL WINDOWOPENCHILD(IWIN,FLAGS=SYSMENUON+MAXWINDOW,WIDTH=1000,HEIGHT=500)
    CALL WINDOWSELECT(IWIN)
    CALL WEDITFILE(WBAL_OUTFNAME,ITYPE=MODAL,IDMENU=0, &
                   IFLAGS=NOTOOLBAR+VIEWONLY+WORDWRAP+NOFILENEWOPEN,&
                   IFONT=4,ISIZE=10)
   ENDIF
  ENDIF
 ELSE
  WRITE(*,'(/A)') 'Succesfully completed waterbalance, results written in:'
  WRITE(*,'(A/)') TRIM(WBAL_OUTFNAME)
 ENDIF

 WBALCOMPUTE=.TRUE.

 END FUNCTION WBALCOMPUTE

 !###======================================================================
 LOGICAL FUNCTION WBALCORRECTPERIOD(IDFNAME,CDATE)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 CHARACTER(LEN=*),INTENT(OUT) :: CDATE
 LOGICAL :: LEX
 INTEGER :: ID,IM,IY,K,JDATE
 REAL :: FRACTION
 
 LEX=.TRUE.
 !## transient
 IF(WBAL_ISTEADY.EQ.0)THEN
  JDATE=UTL_IDFGETDATE(IDFNAME,FRACTION)
  CALL IDATETOGDATE(JDATE,IY,IM,ID)
  JDATE=JD(IY,IM,ID)
  !CALL GDATE(JDATE,IY,IM,ID)
  !## lies within selected year
  IF(LEX.AND.WBAL_NYEAR.GT.0)THEN
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
  CDATE=JDATETOFDATE(FRACTION,JDATE,2)
!  CALL WDATETOSTRING(CDATE,IY,IM,ID)
 !## steady-state
 ELSE
  CDATE='STEADY-STATE'
 ENDIF

 WBALCORRECTPERIOD=LEX
   
!  !## take next period
!  IF(.NOT.LEX)CYCLE

 END FUNCTION WBALCORRECTPERIOD

 !###======================================================================
 SUBROUTINE WBALCOMPUTETXT_INI(IU,CDATE,ILAY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ILAY,IU
 CHARACTER(LEN=*),INTENT(IN) :: CDATE
 INTEGER :: I

 WRITE(IU,'(122A1)') ('=',I=1,122)
 WRITE(IU,'(2A)')   'Period : ',CDATE
 WRITE(IU,'(A,I3)') 'Layer  : ',ILAY
 WRITE(IU,'(122A1)') ('=',I=1,122)
 WRITE(IU,*)
 WRITE(IU,'(2X,A25,2A10,5A15)') 'Waterbalance budget','Q_in','Q_out','Q_in','Q_out','Q_in','Q_out','Area'
 WRITE(IU,'(27X,2A10,5A15)') '%','%','m3/d','m3/d','mm/d','mm/d','km2'
 WRITE(IU,*)

 END SUBROUTINE WBALCOMPUTETXT_INI

 !###======================================================================
 SUBROUTINE WBALCOMPUTETXT_WRITE(IU,ILAY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,ILAY
 CHARACTER(LEN=256) :: ASTRING
 INTEGER :: JP,IBAL,I,JSYS
 REAL :: QIN,QOUT,QACT
 REAL,DIMENSION(2) :: QTOT,QRAT
 CHARACTER(LEN=52) :: IDFFNAME
 
 DO JP=1,NIP
  WRITE(IU,'(A5,I10,12X,95A1)') 'Zone:',IPLIST(JP),('-',I=1,95)
  WRITE(IU,*)

  !## get totals
  QTOT=0.0
  DO IBAL=1,MXTP
   IF(TP(IBAL)%IACT.EQ.0)CYCLE
   DO JSYS=1,TP(IBAL)%NSYS
    IF(IBAL.EQ.3.AND.ILAY.GT.1)THEN
     IF(WBAL(0,JSYS,JP)%QACT.GT.0.0)THEN
      QIN    =WBAL(0,JSYS,JP)%QIN
      QOUT   =WBAL(0,JSYS,JP)%QOUT
      QTOT(1)=QTOT(1)+QIN
      QTOT(2)=QTOT(2)+QOUT
     ENDIF
    ENDIF
    QACT=WBAL(IBAL,JSYS,JP)%QACT
    IF(QACT.GT.0.0)THEN
     QIN    =WBAL(IBAL,JSYS,JP)%QIN
     QOUT   =WBAL(IBAL,JSYS,JP)%QOUT
     QTOT(1)=QTOT(1)+QIN
     QTOT(2)=QTOT(2)+QOUT
    ENDIF
   ENDDO
  ENDDO

  DO IBAL=1,MXTP
   IF(TP(IBAL)%IACT.EQ.0)CYCLE
   DO JSYS=1,TP(IBAL)%NSYS
    IF(WBAL(0,JSYS,JP)%IACT.GT.0)THEN
     IF(IBAL.EQ.3.AND.ILAY.GT.1)THEN
      IDFFNAME=TRIM(WBAL(0,JSYS,JP)%FNAME)
      ASTRING='FLUX UPPER FACE'
      IF(WBAL(0,JSYS,JP)%QACT.GT.0.0)THEN
       QIN    =WBAL(0,JSYS,JP)%QIN
       QOUT   =WBAL(0,JSYS,JP)%QOUT
       QACT   =WBAL(0,JSYS,JP)%QACT
       QRAT   =0.0
       IF(QTOT(1).NE.0.0)QRAT(1)=QIN /QTOT(1)*100.0
       IF(QTOT(2).NE.0.0)QRAT(2)=QOUT/QTOT(2)*100.0
       WRITE(IU,'(2X,A25,2F10.3,5E15.7,A)') TRIM(ASTRING),QRAT(1),QRAT(2),QIN,QOUT,QIN/QACT*1000.0,QOUT/QACT*1000.0, &
             QACT/1.0E6,' "'//TRIM(IDFFNAME)//'"'
      ELSE
       WRITE(IU,'(2X,A25,20X,4A15,E15.7,A)') TRIM(ASTRING),'---','---','---','---',0.0,' "'//TRIM(IDFFNAME)//'"'
      ENDIF
     ENDIF
    ENDIF
    IF(WBAL(IBAL,JSYS,JP)%IACT.GT.0)THEN
     IDFFNAME=TRIM(WBAL(IBAL,JSYS,JP)%FNAME)
     QACT=WBAL(IBAL,JSYS,JP)%QACT
     IF(QACT.GT.0.0)THEN
      QIN    =WBAL(IBAL,JSYS,JP)%QIN
      QOUT   =WBAL(IBAL,JSYS,JP)%QOUT
      QRAT   =0.0
      IF(QTOT(1).NE.0.0)QRAT(1)=QIN /QTOT(1)*100.0
      IF(QTOT(2).NE.0.0)QRAT(2)=QOUT/QTOT(2)*100.0
      WRITE(IU,'(2X,A25,2F10.3,5E15.7,A)') TRIM(TP(IBAL)%ALIAS),QRAT(1),QRAT(2),QIN,QOUT,QIN/QACT*1000.0,QOUT/QACT*1000.0, &
            QACT/1.0E6,' "'//TRIM(IDFFNAME)//'"'
     ELSE
      WRITE(IU,'(2X,A25,20X,4A15,E15.7,A)') TRIM(TP(IBAL)%ALIAS),'---','---','---','---',0.0,' "'//TRIM(IDFFNAME)//'"'
     ENDIF
    ENDIF
   ENDDO
  ENDDO

  WRITE(IU,'(122A1)') ('-',I=1,122)
  WRITE(IU,'(2X,A25,20X,2E15.7,A15,E15.7,A)') 'TOTALS',QTOT(1),QTOT(2),'Error :',QTOT(1)+QTOT(2),' (m3/d)'
  WRITE(IU,*)
 ENDDO
 WRITE(IU,'(122A1)') ('=',I=1,122)

 END SUBROUTINE WBALCOMPUTETXT_WRITE

 !###======================================================================
 SUBROUTINE WBALCOMPUTECSV_INI(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: IBAL,JSYS
 
 HSTRING = 'Date,Layer,Zone'
 DO IBAL=1,MXTP
  IF(TP(IBAL)%IACT.NE.1)CYCLE
  DO JSYS=1,TP(IBAL)%NSYS
   IF(TP(IBAL)%ISYS(JSYS).EQ.0)THEN
    HSTRING=TRIM(HSTRING)//',"'//TRIM(TP(IBAL)%ALIAS)//'_IN ","'// &
                                 TRIM(TP(IBAL)%ALIAS)//'_OUT","'//  &
                                 TRIM(TP(IBAL)%ALIAS)//'_IN ","'// &
                                 TRIM(TP(IBAL)%ALIAS)//'_OUT"'
    IF(IBAL.EQ.3)HSTRING=TRIM(HSTRING)//',"FLUX UPPER FACE_IN ","FLUX UPPER FACE_OUT ","FLUX UPPER FACE_IN ","FLUX UPPER FACE_OUT "'
   ELSE
    HSTRING=TRIM(HSTRING)//',"'//TRIM(TP(IBAL)%ALIAS)//'_SYS'//TRIM(ITOS(TP(IBAL)%ISYS(JSYS)))//'_IN ","'// &
                                 TRIM(TP(IBAL)%ALIAS)//'_SYS'//TRIM(ITOS(TP(IBAL)%ISYS(JSYS)))//'_OUT","'//  &
                                 TRIM(TP(IBAL)%ALIAS)//'_SYS'//TRIM(ITOS(TP(IBAL)%ISYS(JSYS)))//'_IN ","'// &
                                 TRIM(TP(IBAL)%ALIAS)//'_SYS'//TRIM(ITOS(TP(IBAL)%ISYS(JSYS)))//'_OUT"'
   ENDIF
  ENDDO
 ENDDO
 WRITE(IU,*)
 WRITE(IU,'(A)') TRIM(HSTRING)

 HSTRING = 'yyyymmdd,,'
 DO IBAL=1,MXTP
  IF(TP(IBAL)%IACT.NE.1)CYCLE
  DO JSYS=1,TP(IBAL)%NSYS
   IF(TP(IBAL)%ISYS(JSYS).EQ.0)THEN
    HSTRING=TRIM(HSTRING)//',"m3/d","m3/d","mm/d","mm/d"'
    IF(IBAL.EQ.3)HSTRING=TRIM(HSTRING)//',"m3/d","m3/d","mm/d","mm/d"'
   ELSE
    HSTRING=TRIM(HSTRING)//',"m3/d","m3/d","mm/d","mm/d"'
   ENDIF
  ENDDO
 ENDDO
 WRITE(IU,'(A)') TRIM(HSTRING)
 
 END SUBROUTINE WBALCOMPUTECSV_INI
 
 !###======================================================================
 SUBROUTINE WBALCOMPUTECSV_WRITE(IU,ILAY,CDATE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ILAY,IU
 CHARACTER(LEN=*),INTENT(IN) :: CDATE
 INTEGER :: JP,IBAL,JSYS
 REAL :: QIN,QOUT,QACT

 DO JP=1,NIP
  HSTRING=TRIM(CDATE)//','//TRIM(ITOS(ILAY))
  HSTRING=TRIM(HSTRING)//','//TRIM(ITOS(INT(IPLIST(JP))))
  DO IBAL=1,MXTP
   IF(TP(IBAL)%IACT.NE.1)CYCLE 
   DO JSYS=1,TP(IBAL)%NSYS
    QIN =WBAL(IBAL,JSYS,JP)%QIN
    QOUT=WBAL(IBAL,JSYS,JP)%QOUT
    QACT=WBAL(IBAL,JSYS,JP)%QACT; IF(QACT.EQ.0.0)QACT=1.0
    HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QIN,'E',7))
    HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QOUT,'E',7))
    HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QIN/QACT*1000.0,'E',7))
    HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QOUT/QACT*1000.0,'E',7))
    IF(IBAL.EQ.3)THEN
     IF(ILAY.GT.1)THEN
      !## upper face
      QIN =WBAL(0,JSYS,JP)%QIN
      QOUT=WBAL(0,JSYS,JP)%QOUT
      QACT=WBAL(0,JSYS,JP)%QACT; IF(QACT.EQ.0.0)QACT=1.0
      HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QIN,'E',7))
      HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QOUT,'E',7))
      HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QIN/QACT*1000.0,'E',7))
      HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QOUT/QACT*1000.0,'E',7))
     ELSE
      HSTRING=TRIM(HSTRING)//',0.0,0.0,0.0,0.0'
     ENDIF
    ENDIF
   ENDDO
  ENDDO
  WRITE(IU,'(A)') TRIM(HSTRING)
 ENDDO

 END SUBROUTINE WBALCOMPUTECSV_WRITE

 !###======================================================================
 SUBROUTINE WBALCOMPUTEIPF_INI(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: IBAL,I,N,NN,JSYS
 REAL :: X,Y
 CHARACTER(LEN=256):: FNAME
   
 IF(WBAL_ISTEADY.EQ.0)CALL UTL_CREATEDIR(WBAL_OUTFNAME(:INDEX(WBAL_OUTFNAME,'.',.TRUE.)-1))

 !## number of zones/points
 WRITE(IU,*) NIP
 IF(WBAL_ISTEADY.EQ.0)THEN     !## transient
  WRITE(IU,'(A)') '3'
  !## write labels  
  WRITE(IU,'(A)') 'XCOORD'
  WRITE(IU,'(A)') 'YCOORD'
  WRITE(IU,'(A)') 'ZONATION_NUMBER'
  WRITE(IU,'(A)') '3,TXT'

  ALLOCATE(IUTXT(NIP)); IUTXT=0
  NN=0; DO IBAL=1,MXTP; IF(TP(IBAL)%IACT.EQ.1)NN=NN+1; ENDDO
 
  DO I=1,NIP
   CALL WBALGETXYZONE(X,Y,INT(IPLIST(I)))
   WRITE(IU,*) X,Y,TRIM(WBAL_OUTFNAME(INDEX(WBAL_OUTFNAME,'.',.TRUE.)+1:))//'\zone_'//TRIM(ITOS(INT(IPLIST(I))))

   FNAME=WBAL_OUTFNAME(:INDEX(WBAL_OUTFNAME,'.',.TRUE.)-1)//'\zone_'//TRIM(ITOS(INT(IPLIST(I))))//'.txt'
   IUTXT(I)=UTL_GETUNIT()
   CALL OSD_OPEN(IUTXT(I),FILE=FNAME,STATUS='UNKNOWN')
   WRITE(IUTXT(I),*) NPER
   WRITE(IUTXT(I),*) NN*2+1 !## in and out fluxes
   WRITE(IUTXT(I),'(A,F10.1)') '"DATE"',-9999.0
   DO IBAL=1,MXTP
    IF(TP(IBAL)%IACT.NE.1)CYCLE
    DO JSYS=1,TP(IBAL)%NSYS
     IF(TP(IBAL)%ISYS(JSYS).EQ.0)THEN
      WRITE(IUTXT(I),'(A,F10.1)') '"'//TRIM(TP(IBAL)%ALIAS)//'_IN"',-9999.0
      WRITE(IUTXT(I),'(A,F10.1)') '"'//TRIM(TP(IBAL)%ALIAS)//'_OUT"',-9999.0
     ELSE
      WRITE(IUTXT(I),'(A,F10.1)') '"'//TRIM(TP(IBAL)%ALIAS)//'_SYS'//TRIM(ITOS(TP(IBAL)%ISYS(JSYS)))//'_IN"',-99999999.0
      WRITE(IUTXT(I),'(A,F10.1)') '"'//TRIM(TP(IBAL)%ALIAS)//'_SYS'//TRIM(ITOS(TP(IBAL)%ISYS(JSYS)))//'_OUT"',-99999999.0
     ENDIF
    ENDDO
   ENDDO
  ENDDO
 
 ELSEIF(WBAL_ISTEADY.EQ.1)THEN !## steady-state
 
  N=0; DO IBAL=1,MXTP
   IF(TP(IBAL)%IACT.NE.1)CYCLE; N=N+1
  ENDDO
  WRITE(IU,*) N*2+3
  !## write labels  
  WRITE(IU,'(A)') 'XCOORD'
  WRITE(IU,'(A)') 'YCOORD'
  WRITE(IU,'(A)') 'ZONATION_NUMBER'
  DO IBAL=1,MXTP
   IF(TP(IBAL)%IACT.NE.1)CYCLE
   DO JSYS=1,TP(IBAL)%NSYS
    IF(TP(IBAL)%ISYS(JSYS).EQ.0)THEN
     WRITE(IU,'(A)') '"'//TRIM(TP(IBAL)%ALIAS)//'_IN"'
     WRITE(IU,'(A)') '"'//TRIM(TP(IBAL)%ALIAS)//'_OUT"'
    ELSE
     WRITE(IU,'(A)') '"'//TRIM(TP(IBAL)%ALIAS)//'_SYS'//TRIM(ITOS(TP(IBAL)%ISYS(JSYS)))//'_IN"'
     WRITE(IU,'(A)') '"'//TRIM(TP(IBAL)%ALIAS)//'_SYS'//TRIM(ITOS(TP(IBAL)%ISYS(JSYS)))//'_OUT"'
    ENDIF
   ENDDO
  ENDDO
  WRITE(IU,'(A)') '0,TXT'

 ENDIF
 
 END SUBROUTINE WBALCOMPUTEIPF_INI
 
 !###======================================================================
 SUBROUTINE WBALGETXYZONE(X,Y,IZ)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IZ
 REAL,INTENT(OUT) :: X,Y
 INTEGER :: IROW,ICOL,N
 REAL :: XZ,YZ
 
 X=0.0; Y=0.0; N=0
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
 X=X/REAL(N)
 Y=Y/REAL(N)
  
 END SUBROUTINE WBALGETXYZONE
 
 !###======================================================================
 SUBROUTINE WBALCOMPUTEIPF_WRITE(IU,ILAY,CDATE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ILAY,IU
 CHARACTER(LEN=*),INTENT(IN) :: CDATE
 CHARACTER(LEN=12):: CCDATE
 INTEGER :: JP,IBAL,JSYS,JJ
 REAL :: QIN,QOUT,QACT,X,Y

 DO JP=1,NIP

  IF(WBAL_ISTEADY.EQ.1)THEN
   CALL WBALGETXYZONE(X,Y,INT(IPLIST(JP)))
   HSTRING=TRIM(RTOS(X,'F',2))//','//TRIM(RTOS(Y,'F',2))//','//TRIM(ITOS(INT(IPLIST(JP))))
  ELSE
   CCDATE=CDATE
   JJ=GDATETOJDATE(CCDATE)
   JJ=JDATETOIDATE(JJ)
   HSTRING=TRIM(ITOS(JJ)) 
  ENDIF

  DO IBAL=1,MXTP
   IF(TP(IBAL)%IACT.NE.1)CYCLE 
   DO JSYS=1,TP(IBAL)%NSYS
    QIN =WBAL(IBAL,JSYS,JP)%QIN
    QOUT=WBAL(IBAL,JSYS,JP)%QOUT
    QACT=WBAL(IBAL,JSYS,JP)%QACT; IF(QACT.EQ.0.0)QACT=1.0
    HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QIN,'E',7))
    HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QOUT,'E',7))
    IF(IBAL.EQ.3)THEN
     IF(ILAY.GT.1)THEN
      !## upper face
      QIN =WBAL(0,JSYS,JP)%QIN
      QOUT=WBAL(0,JSYS,JP)%QOUT
      QACT=WBAL(0,JSYS,JP)%QACT; IF(QACT.EQ.0.0)QACT=1.0
      HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QIN,'E',7))
      HSTRING=TRIM(HSTRING)//','//TRIM(RTOS(QOUT,'E',7))
     ELSE
      HSTRING=TRIM(HSTRING)//',0.0,0.0'
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

 END SUBROUTINE WBALCOMPUTEIPF_WRITE

 !###======================================================================
 SUBROUTINE WBALABORT()
 !###======================================================================
 IMPLICIT NONE

 IF(ASSOCIATED(WBAL_ILAYER))DEALLOCATE(WBAL_ILAYER)
 IF(ASSOCIATED(WBAL_IPERIOD))DEALLOCATE(WBAL_IPERIOD)
 IF(ASSOCIATED(WBAL_IYEAR))DEALLOCATE(WBAL_IYEAR)
 IF(ALLOCATED(IUTXT))DEALLOCATE(IUTXT)
 IF(ALLOCATED(WBAL))DEALLOCATE(WBAL)
 IF(ALLOCATED(IPLIST))DEALLOCATE(IPLIST)
 CALL IDFDEALLOCATEX(WBALIDF)
 CALL IDFDEALLOCATEX(IPIDF) 
 CALL CLOSEUNITS()
 
 END SUBROUTINE WBALABORT

 !###======================================================================
 SUBROUTINE WBALFILLPOINTER(WBAL_ISEL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: WBAL_ISEL
 INTEGER :: IC1,IC2,IR1,IR2,ICOL,IROW,I,J
 REAL :: X1,X2,Y1,Y2,XVAL,YVAL,IDFVALUE

 !## copy settings
 CALL IDFCOPY(WBALIDF,IPIDF)
 IF(.NOT.IDFALLOCATEX(IPIDF))THEN; ENDIF
 
 !## entire area
 IF(WBAL_ISEL.EQ.1)THEN
  IF(ALLOCATED(IPLIST))DEALLOCATE(IPLIST)  ! FR 20131025
  ALLOCATE(IPLIST(1))
  IPIDF%X  =1.0
  NIP      =1
  IPLIST(1)=INT(1,2)
 !## polygons
 ELSEIF(WBAL_ISEL.EQ.2)THEN
  IF(ALLOCATED(IPLIST))DEALLOCATE(IPLIST)  ! FR 20131025
  ALLOCATE(IPLIST(SHPNO))
  IPIDF%X=0.0
  NIP    =0
  DO SHPI=1,SHPNO
   IF(SHPIACT(SHPI).EQ.1.AND.SHPNCRD(SHPI).GT.0)THEN
    NIP        =NIP+1
    IPLIST(NIP)=INT(SHPI,2)

    X1=MINVAL(SHPXC(1:SHPNCRD(SHPI),SHPI))
    X2=MAXVAL(SHPXC(1:SHPNCRD(SHPI),SHPI))
    Y1=MINVAL(SHPYC(1:SHPNCRD(SHPI),SHPI))
    Y2=MAXVAL(SHPYC(1:SHPNCRD(SHPI),SHPI))

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
      IF(UTL_INSIDEPOLYGON(XVAL,YVAL,SHPXC(:,SHPI),SHPYC(:,SHPI),SHPNCRD(SHPI)).EQ.1)IPIDF%X(ICOL,IROW)=REAL(SHPI)!INT(SHPI,2)
     ENDDO
    ENDDO
   ENDIF
  ENDDO

 !## idf
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
     IF(IDFVALUE.NE.IPIDF%NODATA)IPIDF%X(ICOL,IROW)=IDFVALUE
    ENDIF
   ENDDO
  ENDDO
  CLOSE(IDF%IU)

  I=INT(MAXVAL(IPIDF%X))
  J=INT(MINVAL(IPIDF%X))
  DO IROW=1,IPIDF%NROW
   DO ICOL=1,IPIDF%NCOL
    IF(IPIDF%X(ICOL,IROW).NE.IPIDF%NODATA)THEN
     I=MIN(INT(IPIDF%X(ICOL,IROW)),I)
     J=MAX(INT(IPIDF%X(ICOL,IROW)),J)
    ENDIF
   ENDDO
  ENDDO

  IF(ALLOCATED(IPLIST))DEALLOCATE(IPLIST)  !FR 20131028
  ALLOCATE(IPLIST(J-I+1))
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
 IF(.NOT.IDFWRITE(IPIDF,TRIM(PREFVAL(1))//'\TMP\POINTER.IDF',1))THEN
 ENDIF

 !## reshuffle pointer to fit iplist()
 DO IROW=1,IPIDF%NROW
  DO ICOL=1,IPIDF%NCOL
   DO I=1,NIP
    IF(IPLIST(I).EQ.INT(IPIDF%X(ICOL,IROW),2))THEN
     IPIDF%X(ICOL,IROW)=REAL(I)
     EXIT
    ENDIF
   ENDDO
   !## reset nodata value to zero!
   IF(IPIDF%X(ICOL,IROW).EQ.IPIDF%NODATA)IPIDF%X(ICOL,IROW)=0.0
  ENDDO
 ENDDO
 
 END SUBROUTINE WBALFILLPOINTER

 !###======================================================================
 SUBROUTINE WBAL1CALC(IBAL,JSYS)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBAL,JSYS
 INTEGER :: ICOL,IROW,JP,JJP,JROW,JCOL
 REAL :: IDFVAL,FCT,DX,DY,X,Y
 LOGICAL :: LEX

 WBAL(IBAL,JSYS,1:NIP)%QIN =0.0
 WBAL(IBAL,JSYS,1:NIP)%QOUT=0.0
 WBAL(IBAL,JSYS,1:NIP)%QACT=0.0
 WBAL(IBAL,JSYS,1:NIP)%IACT=1

 DO IROW=1,IPIDF%NROW
  DO ICOL=1,IPIDF%NCOL

   FCT=1.0
   LEX=.FALSE.
   JP =INT(IPIDF%X(ICOL,IROW))
   JJP=0

   SELECT CASE (IBAL)
    !## frf - right face
    CASE (4)
     IF(ICOL.LT.IPIDF%NCOL)THEN

      IF(JP.NE.0.AND.IPIDF%X(ICOL+1,IROW).NE.JP)THEN
       LEX=.TRUE.
       IF(IPIDF%X(ICOL+1,IROW).NE.0.0)JJP=INT(IPIDF%X(ICOL+1,IROW))
      ELSE
       JP=INT(IPIDF%X(ICOL+1,IROW))
       IF(JP.NE.0.AND.IPIDF%X(ICOL,IROW).NE.JP)THEN
        LEX=.TRUE.
        FCT=-1.0
       ENDIF
      ENDIF

     ENDIF

    !## fff - front face
    CASE (5)
     IF(IROW.LT.IPIDF%NROW)THEN

      IF(JP.NE.0.AND.IPIDF%X(ICOL,IROW+1).NE.JP)THEN
       LEX=.TRUE.
       IF(IPIDF%X(ICOL,IROW+1).NE.0)JJP=INT(IPIDF%X(ICOL,IROW+1))
      ELSE
       JP=INT(IPIDF%X(ICOL,IROW+1))
       IF(JP.NE.0.AND.IPIDF%X(ICOL,IROW).NE.JP)THEN
        LEX=.TRUE.
        FCT=-1.0
       ENDIF
      ENDIF

     ENDIF

    !## flow top face
    CASE (0)
     IF(JP.NE.0)LEX=.TRUE.
     FCT=-1.0
    !## rest
    CASE DEFAULT
     IF(JP.NE.0)LEX=.TRUE.
   END SELECT

   IF(LEX)THEN

    !## get x/y coordinates based upon ipidf coordinates
    CALL IDFGETLOC(IPIDF,IROW,ICOL,X,Y)
    CALL IDFIROWICOL(WBALIDF,JROW,JCOL,X,Y)
    
    IF(JROW.GE.1.AND.JROW.LE.IPIDF%NROW.AND. &
       JCOL.GE.1.AND.JCOL.LE.IPIDF%NCOL)THEN
       
     !## total area
     IF(WBALIDF%IEQ.EQ.0)THEN
      DX=WBALIDF%DX
      DY=WBALIDF%DY
     ELSE
      DX=WBALIDF%SX(JCOL)  -WBALIDF%SX(JCOL-1)
      DY=WBALIDF%SY(JROW-1)-WBALIDF%SY(JROW)
     ENDIF

     WBAL(IBAL,JSYS,JP)%QACT=WBAL(IBAL,JSYS,JP)%QACT+(DX*DY)

     IDFVAL=IDFGETVAL(WBALIDF,JROW,JCOL)
     IF(IDFVAL.EQ.WBALIDF%NODATA)IDFVAL=0.0
     IDFVAL=FCT*IDFVAL

     IF(IDFVAL.NE.0.0)THEN
      IF(IDFVAL.LT.0.0)WBAL(IBAL,JSYS,JP)%QOUT=WBAL(IBAL,JSYS,JP)%QOUT+IDFVAL
      IF(IDFVAL.GT.0.0)WBAL(IBAL,JSYS,JP)%QIN =WBAL(IBAL,JSYS,JP)%QIN +IDFVAL
      IF(JJP.NE.0)THEN
       IDFVAL=(-1.0*FCT)*IDFVAL
       IF(IDFVAL.LT.0.0)WBAL(IBAL,JSYS,JJP)%QOUT=WBAL(IBAL,JSYS,JJP)%QOUT+IDFVAL
       IF(IDFVAL.GT.0.0)WBAL(IBAL,JSYS,JJP)%QIN =WBAL(IBAL,JSYS,JJP)%QIN +IDFVAL
      ENDIF
     ENDIF
    ENDIF
    
   ENDIF
  ENDDO
 ENDDO

 END SUBROUTINE WBAL1CALC

END MODULE MOD_WBAL_CLC

