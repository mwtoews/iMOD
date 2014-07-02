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
MODULE MOD_PLINES_TRACE

USE WINTERACTER
USE RESOURCE
USE MOD_ASC2IDF_PAR, ONLY : IDFFILE,IGRIDFUNC,CS,NODATA,XYZFNAMES,IXCOL,IYCOL,IZCOL
USE MOD_ASC2IDF, ONLY : ASC2IDF_TYPE3
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_UTL, ONLY : ITOS,UTL_GETUNIT,RTOS,JD,IDATETOJDATE,UTL_WAITMESSAGE,UTL_IDFGETDATE,UTL_CAP,UTL_GETUNIQUE_CHAR
USE MOD_IDF, ONLY : IDFREAD,IDFDEALLOCATEX,IDFALLOCATEX,IDFWRITE,IDFIROWICOL
USE MOD_PLINES_PAR
USE MOD_PLINES_READ, ONLY : TRACEREADBLOCK_R,TRACEREADBLOCK_I 
USE MOD_PLINES_SP, ONLY : TRACEPREPARESP,TRACEREADSP
USE MOD_PLINES_FLOLIN, ONLY : FLOLIN
USE MOD_OSD, ONLY : OSD_OPEN,OSD_IOSTAT_MSG

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION TRACEMAIN(RUNFILE,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: RUNFILE
 INTEGER,INTENT(IN) :: IBATCH

 TRACEMAIN=.FALSE.

 !## deallocate memory
 CALL TRACEDEALLOCATE()
 !## read runfile
 IF(TRACEREADRUNFILE(RUNFILE))THEN
  IF(TRACECALC(IBATCH))TRACEMAIN=.TRUE.
 ENDIF
 CALL TRACEDEALLOCATE()
  
 END FUNCTION TRACEMAIN

 !###======================================================================
 LOGICAL FUNCTION TRACECALC(IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=50) :: CPERIOD
 CHARACTER(LEN=1000) :: STRING
 REAL :: TIME,TTMAX,DT,MAXVELOCITY
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: I,J,ILAY,ITYPE,IPER,IPART,NCONS,IPERIOD,NIDSCH,IDSCH,IWIN,IP,IRAT,IRAT1,DPER,MPER,SPER
 LOGICAL :: LEX

 TRACECALC=.FALSE.

 IULOG=UTL_GETUNIT()
 IF(IBATCH.EQ.1)THEN
  CALL OSD_OPEN(IULOG,FILE='.\imodpath.log',STATUS='UNKNOWN')
 ELSE
  CALL OSD_OPEN(IULOG,FILE=TRIM(PREFVAL(1))//'\tmp\imodpath.log',STATUS='UNKNOWN')
 ENDIF

 !## take head for determination model-dimensions
 IF(.NOT.IDFREAD(IDF,HFFNAME(1,1,1),0))RETURN

 CLOSE(IDF%IU)
 !## allocate space
 CALL TRACEAL()
 !## create cell-sizes (cell-borders expressed in x,y-coordinates)
 CALL TRACEDELRC()
 !##read information-for particle tracking
 IF(.NOT.TRACEDATIN(NCONS,IBATCH))RETURN

 DO ISPFNAME=1,NSPFNAME

  !## read/process particles towards readable format
  IF(.NOT.TRACEPREPARESP(IBATCH))RETURN
  !## initialize outputfiles
  IF(.NOT.TRACEINITOUTFILES())RETURN
  !#allocate memory to store particles (spec. for transient sim.)
  CALL TRACEALPART()
  !## read particle in memory
  !## set initial time for each particle to zero
  CALL TRACEREADSP()

  TTMAX  =0.0
  IPERIOD=1
  IRAT   =0
  IRAT1  =IRAT
  MAXVELOCITY=0.0

  !## transient simulation
  IF(ISS.EQ.1)THEN
   !## forwards
   IF(IREV.EQ.0)THEN
    DO IPER=1,NPER
     IF(PLIPER(IPER,1).LE.JD0.AND.PLIPER(IPER+1,1).GT.JD0)EXIT
    ENDDO   
    TTMAX=PLIPER(IPER,1)-JD0
    WRITE(IULOG,*) 'Initial StartDate Offset:',TTMAX
    IPER=IPER-1; DPER=1; MPER=NPER; SPER=1
   !## backwards
   ELSEIF(IREV.EQ.1)THEN
    DO IPER=NPER,1,-1
     IF(PLIPER(IPER,1).LE.JD0.AND.PLIPER(IPER+1,1).GT.JD0)EXIT
    ENDDO   
    TTMAX=JD0-PLIPER(IPER+1,1)
    WRITE(IULOG,*) 'Initial StartDate Offset:',TTMAX
    IPER=IPER+1; DPER=-1; MPER=1; SPER=NPER
   ENDIF
   
  ENDIF
   
  DO

   !## transient simulation
   IF(ISS.EQ.1)THEN
    IPER=IPER+DPER   
    !## what to do after sequence has almost ended
    IF(IPER.EQ.MPER)THEN 
     LEX=.TRUE.; DT=PLIPER(IPER+1,1)-PLIPER(IPER,1)
     IF(ISTOPCRIT.EQ.3)THEN; TTMAX=TMAX; ELSE; TTMAX=TTMAX+DT; ENDIF
    ELSE
     !## within selected time-window
     LEX=.FALSE.
     !# added one day to pliper(nper+1,1)
     IF(PLIPER(IPER+1,1).LE.JD2+1.AND.PLIPER(IPER,1).GE.JD1)THEN
      !## length of stress-period (days)
      DT =MIN(JD2,PLIPER(IPER+1,1))-MAX(JD1,PLIPER(IPER,1))
      TTMAX=TTMAX+DT
      LEX  =.TRUE.
     ENDIF
    ENDIF

   !## steady-state simulation
   ELSE
    TTMAX=TMAX
    LEX=.TRUE.
    IPER=1
   ENDIF

   !## never exceeds given tmax
   TTMAX=MIN(TTMAX,TMAX)

   IF(LEX)THEN

    CALL WMESSAGEPEEK(ITYPE,MESSAGE)
    IF(ITYPE.EQ.KEYDOWN.AND.MESSAGE%VALUE1.EQ.KEYESCAPE)THEN
     CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to terminate current pathline computation?'//CHAR(13)// &
      'Though, current results upto now, are allready stored in an IFF-format','Question')
     IF(WINFODIALOG(4).EQ.1)EXIT
    ENDIF

    IF(ISS.EQ.0)WRITE(IULOG,'(1X,A,I10,A,F10.2,A)') 'Using following files for steady-state simulation'
    IF(ISS.EQ.1)WRITE(IULOG,'(1X,A,I10,A,F10.2,A)') 'Using following files for current stressperiod',IPER,' time upto: ',TTMAX,' days'
    DO ILAY=1,NLAY
     STRING=TRIM(ITOS(ILAY))
     J=1; IF(ILAY.EQ.NLAY)J=0
     DO I=1,2+J 
      IP=INDEX(HFFNAME(I,ILAY,IPER),'\',.TRUE.)+1
      STRING=TRIM(STRING)//','//TRIM(HFFNAME(I,ILAY,IPER)(IP:))
     END DO
     WRITE(IULOG,'(A)') TRIM(STRING)
    END DO
    WRITE(IULOG,*)

    !## read time-dependent data for particle tracking
    IF((ISS.EQ.0.AND.ISPFNAME.EQ.1).OR.ISS.EQ.1)THEN
     IF(.NOT.TRACEREADBUDGET(IPER,IBATCH))RETURN 
     !## backwards tracking
     IF(IREV.EQ.1)CALL TRACEIREV()
    ENDIF

    !## start particle loop
    IF(IBATCH.EQ.0)CALL WINDOWSELECT(0)

    IF(ISS.EQ.1)THEN
     CPERIOD=' [Duration '//TRIM(ITOS(INT(DT)))//' day; Period '//TRIM(ITOS(IPERIOD))//', max. '//TRIM(ITOS(INT(TTMAX)))//' days]'
     I=0
     DO IPART=1,NPART
      IF(KLC(IPART).NE.0)I=I+1
     ENDDO
     STRING='Still tracing '//TRIM(ITOS(I))//' out of '//TRIM(ITOS(NPART))//' particles for Stress '// &
                              TRIM(ITOS(IPER))//' out of '//TRIM(ITOS(NPER))//TRIM(CPERIOD)
     IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
     IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
    ELSE
     STRING='Tracing '//TRIM(ITOS(NPART))//' particles (Steady-state) ...'
     IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
     IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
    ENDIF

    NIDSCH=0
    DO IPART=1,NPART
     CALL WMESSAGEPEEK(ITYPE,MESSAGE)
     !## time in days!
     TIME=TOT(IPART,2)
     !## trace selected particle, NOT YET discharged!
     IF(KLC(IPART).NE.0)THEN
      CALL FLOLIN(IPART,IMODE(1),TIME,TTMAX,IDSCH,JLC(IPART),ILC(IPART),KLC(IPART),   &
                  XLC(IPART,2),YLC(IPART,2),ZLC(IPART,2),ZLL(IPART,2),IBOUND,ZBOT, &
                  ZTOP,LDELR,LDELC,QX,QY,QZ,QSS,POR,NCON,IDF%NCOL,IDF%NROW,NLAY,  &
                  NLPOR,IDF%NCOL*IDF%NROW*NLAY,NCP1,NRP1,NLP1,ISNK,IREV,FRAC,IMODE(1),   &
                  ISS,MAXVELOCITY,DELX,DELY,MAXILAY(IPART))
      !## time in days!
      TOT(IPART,2)=TIME !(days)
      IF(ISS.EQ.1)THEN
       !## end of current simulation
       IF(IDSCH.EQ.7)IDSCH=0
      ELSE
       !## end of simulation reached!
       IF(TOT(IPART,2).GE.TTMAX)IDSCH=7
      ENDIF
      !## particle discharged whenever idsch.ne.0
      IF(IDSCH.NE.0)THEN
       !## write endpoint information current particle that has stopped
       IF(IMODE(2).GT.0)CALL TRACECREATEIPF(IMODE(2),IDSCH,IPART)
       KLC(IPART)=0
      ELSE
       !## particle NOT discharged!
       NIDSCH=NIDSCH+1
      ENDIF
     ENDIF
     IF(IBATCH.EQ.0)CALL UTL_WAITMESSAGE(IRAT,IRAT1,IPART,NPART,'Busy tracking particle '//TRIM(ITOS(IPART))//' ')
     IF(IBATCH.EQ.1)WRITE(*,'(A,I10)') 'Busy tracking particle ',IPART
    ENDDO
    !## no particles left, stop tracking process!
    IF(NIDSCH.EQ.0)EXIT
   ENDIF

   !## stop while doing a steady-state simulation
   IF(ISS.EQ.0)EXIT 
   !## stop whenever tmax.eq.ttmax
   IF(TTMAX.GE.TMAX)EXIT
   !## determing stopcriterion whenever transient simulation concerned!
   IF(IPER.EQ.MPER)THEN !NPER)THEN
    !## stop after last period, assume last period duration is similar to previous one!
    !## continue until particle stops (given tmax, else tmax=10e30)
    IF(ISTOPCRIT.EQ.1.OR.ISTOPCRIT.EQ.3)EXIT
    !## repeat period again tmax
    IF(ISTOPCRIT.EQ.2)THEN
     IPER   =SPER+(-1.0*DPER)
     IPERIOD=IPERIOD+1
    ENDIF
   ENDIF

  ENDDO

  !## write remaining non-stopped particles for endpoints (IDSCH=0)
  IF(IMODE(2).GT.0)THEN
   IDSCH=0
   DO IPART=1,NPART; IF(KLC(IPART).NE.0)CALL TRACECREATEIPF(IMODE(2),IDSCH,IPART); ENDDO
  ENDIF

  IF(IMODE(1).GT.0)THEN; CLOSE(IMODE(1)); IMODE(1)=1; ENDIF
  IF(IMODE(2).GT.0)THEN; CLOSE(IMODE(2)); IMODE(2)=1; ENDIF
  
  STRING='Completed particle tracking. '// &
   'Results are stored within: '//CHAR(13)//TRIM(IFFFNAME(ISPFNAME))//CHAR(13)//'and added to the iMOD-manager.'//CHAR(13)//CHAR(13)// &
    TRIM(ITOS(NPART))//' particles were released out of '//TRIM(ITOS(TPART))//'. '//CHAR(13)//  &
   'Unreleased particles occured due to inactive/constant head boundary conditions'//CHAR(13)// &
   'and/or particles positioned above/beneath given thresshold.'//CHAR(13)//&
    TRIM(ITOS(NCONS))//' inconsequences were removed from top/bottom information!'//CHAR(13)//CHAR(13)// &
    'IMPORTANT: Maximum velocity that occured: '//TRIM(RTOS(MAXVELOCITY,'E',4))//' m/day'
  WRITE(IULOG,'(/A/)') TRIM(STRING)
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)THEN
   CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,TRIM(STRING),'Information')
!   CALL WINDOWOPENCHILD(IWIN,FLAGS=SYSMENUON,WIDTH=1000,HEIGHT=500)
!   CALL WINDOWSELECT(IWIN)
!   CALL WEDITFILE(TRIM(PREFVAL(1))//'\tmp\imodpath.log',ITYPE=MODAL,IDMENU=0, &
!                  IFLAGS=NOTOOLBAR+VIEWONLY+WORDWRAP+NOFILENEWOPEN+NOFILESAVEAS,&
!                  IFONT=4,ISIZE=10)
  ENDIF
 
  !## deallocate memory
  CALL TRACEDEALLOCATE()

 ENDDO
 
 CLOSE(IULOG)

 TRACECALC=.TRUE.

 END FUNCTION TRACECALC

 !###======================================================================
 SUBROUTINE TRACEPOSTPROCESSING(IFFFLOW,IPFFLOW,IDFFLOW,IPFFNAME,IPFICOL) 
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IFFFLOW,IPFFLOW,IDFFLOW,IPFFNAME
 INTEGER,INTENT(IN),DIMENSION(4) :: IPFICOL
 CHARACTER(LEN=50) :: ERRORMESSAGE
 CHARACTER(LEN=256) :: LINE
 TYPE IFFOBJ
  INTEGER :: ID,IL
  REAL :: X,Y,Z,T,V
 END TYPE IFFOBJ
 TYPE(IFFOBJ),DIMENSION(:),POINTER :: IFF,IFF_BU
 INTEGER :: IU,I,J,N,IOS,IDSCH,IROW,ICOL,IROW1,IROW2,ICOL1,ICOL2,NROW,NCOL
 REAL :: T,D,EPS
 INTEGER,DIMENSION(:,:),ALLOCATABLE :: JU
 INTEGER,DIMENSION(:,:),ALLOCATABLE :: NP
 REAL,DIMENSION(3,2) :: XYZ
 INTEGER,DIMENSION(3) :: ILAY
 INTEGER,DIMENSION(2) :: ID
  
 IF(.NOT.TRACEPOSTPROCESSING_INIT(IPFFNAME,IPFICOL))RETURN
 IMODE=0; IF(IFFFLOW.NE.'')IMODE(1)=1; IF(IPFFLOW.NE.'')IMODE(2)=1
 
 IF(.NOT.IDFREAD(IDF,IDFFLOW,0))RETURN 
 EPS=IDF%DX/10.0
 
 WRITE(*,'(/A/)') 'Busy with postprocessing particles ...'
 IF(IMODE(1).EQ.1)WRITE(*,'(1X,A)') 'Processing '//TRIM(IFFFLOW)
 IF(IMODE(2).EQ.1)WRITE(*,'(1X,A)') 'Processing '//TRIM(IPFFLOW)
 
 !## get ipf-information
 DO I=1,SIZE(IPF)
  CALL IDFIROWICOL(IDF,IPF(I)%IROW,IPF(I)%ICOL,IPF(I)%X,IPF(I)%Y)
 ENDDO

!write(*,*) nunq,imode

 !## open all files
 ALLOCATE(JU(NUNQ,2),NP(NUNQ,2)); JU=0
 DO I=1,NUNQ
  IF(IMODE(1).EQ.1)THEN
   JU(I,1)=UTL_GETUNIT()
   J=INDEX(IFFFLOW,'.',.TRUE.)-1; IF(J.LE.0)J=LEN_TRIM(IFFFLOW)
   CALL OSD_OPEN(JU(I,1),FILE=IFFFLOW(:J)//'_'//TRIM(IPF(I)%UNQLABEL)//'.iff',STATUS='REPLACE', &
                         ACTION='WRITE',IOSTAT=IOS,FORM='FORMATTED')
   CALL TRACEINITOUTFILES_IFF(JU(I,1))
  ENDIF
  IF(IMODE(2).EQ.1)THEN
   JU(I,2)=UTL_GETUNIT()
   J=INDEX(IPFFLOW,'.',.TRUE.)-1; IF(J.LE.0)J=LEN_TRIM(IPFFLOW)
   CALL OSD_OPEN(JU(I,2),FILE=IPFFLOW(:J)//'_'//TRIM(IPF(I)%UNQLABEL)//'_.ipf',STATUS='REPLACE', &
                         ACTION='WRITE',IOSTAT=IOS,FORM='FORMATTED')
   CALL TRACEINITOUTFILES_IPF(JU(I,2))
  ENDIF
 ENDDO

 NP=0; IU=UTL_GETUNIT()
 !## split pathlines to appropriate files
 IF(IMODE(1).EQ.1)THEN
  IMODE(1)=UTL_GETUNIT()
  CALL OSD_OPEN(IMODE(1),FILE=IFFFLOW,STATUS='OLD',ACTION='READ',IOSTAT=IOS,FORM='FORMATTED')
  DO I=1,8; READ(IMODE(1),*); ENDDO
  ALLOCATE(IFF(100))
  READ(IMODE(1),*,IOSTAT=IOS) IFF(1)%ID,IFF(1)%IL,IFF(1)%X,IFF(1)%Y,IFF(1)%Z,IFF(1)%T,IFF(1)%V 
  N=1; DO
   N=N+1
   IF(N.GT.SIZE(IFF))THEN
    IF(N.GT.10000)THEN
     WRITE(*,'(/A/)') '### Currently particle ',IFF(1)%ID,'probably went wrong, will be skipped ###'
     !## skip particle ... too big to process
     ALLOCATE(IFF(100))
     DO
      READ(IMODE(1),*) IFF(2)%ID,IFF(2)%IL,IFF(2)%X,IFF(2)%Y,IFF(2)%Z,IFF(2)%T,IFF(2)%V
      IF(IFF(2)%ID.NE.IFF(1)%ID)THEN; IFF(1)=IFF(2); N=2; EXIT; ENDIF
     ENDDO
    ELSE
     ALLOCATE(IFF_BU(N+100),STAT=IOS)
     IFF_BU(1:SIZE(IFF))=IFF; DEALLOCATE(IFF); IFF=>IFF_BU
    ENDIF
   ENDIF
   READ(IMODE(1),*,IOSTAT=IOS) IFF(N)%ID,IFF(N)%IL,IFF(N)%X,IFF(N)%Y,IFF(N)%Z,IFF(N)%T,IFF(N)%V 
   IF(IFF(1)%ID.NE.IFF(N)%ID.OR.IOS.NE.0)THEN

    !## get current irow/icol for LAST particle location
    CALL IDFIROWICOL(IDF,IROW1,ICOL1,IFF(N-1)%X-eps,IFF(N-1)%Y+eps)
    CALL IDFIROWICOL(IDF,IROW2,ICOL2,IFF(N-1)%X+eps,IFF(N-1)%Y-eps)
!WRITE(*,*) ICOL1,ICOL2,IROW1,IROW2

!    CALL IDFIROWICOL(IDF,IROW,ICOL,IFF(N-1)%X,IFF(N-1)%Y)

    !## see what ipf gets the data
IFFLOOP: DO I=1,SIZE(IPF)
     IF(IFF(N-1)%IL.EQ.IPF(I)%ILAY)THEN
      DO IROW=IROW1,IROW2; DO ICOL=ICOL1,ICOL2
       IF(IPF(I)%IROW.EQ.IROW.AND.IPF(I)%ICOL.EQ.ICOL)THEN
        NP(IPF(I)%INQ,1)=NP(IPF(I)%INQ,1)+1
        DO J=1,N-1
         LINE=TRIM(ITOS(IFF(J)%ID))  //','//TRIM(ITOS(IFF(J)%IL))//','// &
           TRIM(RTOS(IFF(J)%X,'F',2))//','//TRIM(RTOS(IFF(J)%Y,'F',2))//','//TRIM(RTOS(IFF(J)%Z,'F',2))//','// &
           TRIM(RTOS(IFF(J)%T,'F',2))//','//TRIM(RTOS(IFF(J)%V,'F',2))
         WRITE(JU(IPF(I)%INQ,1),'(A)') TRIM(LINE)
        ENDDO
        EXIT IFFLOOP
        ENDIF   
      ENDDO; ENDDO
     ENDIF
    ENDDO IFFLOOP
    IF(IOS.NE.0)EXIT
    IFF(1)=IFF(N); N=1
   ENDIF
  ENDDO
  DO I=1,NUNQ; IF(JU(I,1).NE.0)THEN
   IF(NP(I,1).EQ.0)THEN; CLOSE(JU(I,1),STATUS='DELETE'); JU(I,1)=0; ENDIF
   IF(NP(I,1).GT.0)CLOSE(JU(I,1))
  ENDIF; ENDDO
  CLOSE(IMODE(1)); IMODE(1)=1; DEALLOCATE(IFF)
  
 ENDIF
 !## open output channel endpoints
 IF(IMODE(2).EQ.1)THEN
  IMODE(2)=UTL_GETUNIT()
  CALL OSD_OPEN(IMODE(2),FILE=IPFFLOW,STATUS='OLD',ACTION='READ',IOSTAT=IOS,FORM='FORMATTED')
  READ(IMODE(2),*) NROW; READ(IMODE(2),*) NCOL
  DO I=1,NCOL+1; READ(IMODE(2),*); ENDDO
  !## split endpoint to appropriate files
  DO
   READ(IMODE(2),'(A256)',IOSTAT=IOS) LINE
   READ(LINE,*) (XYZ(I,1),I=1,3),ILAY(1),IROW,ICOL,(XYZ(I,2),I=1,3),ILAY(2),IROW,ICOL,ID(1),T,D,IDSCH,ILAY(3)
   !## get current irow/icol for model-idf
!   CALL IDFIROWICOL(IDF,IROW,ICOL,XYZ(1,2),XYZ(2,2))
   !## see what ipf gets the data
IPFLOOP: DO I=1,SIZE(IPF)
    IF(ILAY(2).EQ.IPF(I)%ILAY)THEN
     !## get current irow/icol for LAST particle location
     CALL IDFIROWICOL(IDF,IROW1,ICOL1,XYZ(1,2)-eps,XYZ(2,2)+eps)
     CALL IDFIROWICOL(IDF,IROW2,ICOL2,XYZ(1,2)+eps,XYZ(2,2)-eps)
     DO IROW=IROW1,IROW2; DO ICOL=ICOL1,ICOL2
      IF(IPF(I)%IROW.EQ.IROW.AND.IPF(I)%ICOL.EQ.ICOL)THEN
       NP(IPF(I)%INQ,2)=NP(IPF(I)%INQ,2)+1
       WRITE(JU(IPF(I)%INQ,2),'(A)') TRIM(LINE)
       EXIT IPFLOOP
      ENDIF
     ENDDO; ENDDO
    ENDIF
   ENDDO IPFLOOP
   IF(IOS.NE.0)EXIT
  ENDDO
  DO I=1,NUNQ; IF(JU(I,2).NE.0)THEN
   IF(NP(I,2).EQ.0)THEN; CLOSE(JU(I,2),STATUS='DELETE'); JU(I,2)=0; ENDIF
   IF(NP(I,2).GT.0)CLOSE(JU(I,2))
  ENDIF; ENDDO
  CLOSE(IMODE(2)); IMODE(2)=1
 ENDIF
 
 ALLOCATE(XYZFNAMES(1)); CS=IDF%DX; NODATA=0.0; IGRIDFUNC=3 !## mean
 IXCOL=1; IYCOL=2; IZCOL=10 !## cumtt
 
 !## construct header
 DO I=1,NUNQ
  WRITE(*,*) I,NP(I,1),NP(I,2)
  IF(IMODE(2).EQ.1)THEN
   IF(NP(I,2).GT.0)THEN
    JU(I,2)=UTL_GETUNIT()
    J=INDEX(IPFFLOW,'.',.TRUE.)-1; IF(J.LE.0)J=LEN_TRIM(IPFFLOW)
    CALL OSD_OPEN(JU(I,2),FILE=IPFFLOW(:J)//'_'//TRIM(IPF(I)%UNQLABEL)//'_.ipf',STATUS='OLD', &
                          ACTION='READ',IOSTAT=IOS,FORM='FORMATTED')
    JU(I,1)=UTL_GETUNIT()
    CALL OSD_OPEN(JU(I,1),FILE=IPFFLOW(:J)//'_'//TRIM(IPF(I)%UNQLABEL)//'.ipf',STATUS='REPLACE', &
                          ACTION='WRITE',IOSTAT=IOS,FORM='FORMATTED')  
    READ(JU(I,2),*); WRITE(JU(I,1),*) NP(I,2)
    DO
     READ(JU(I,2),'(A256)',IOSTAT=IOS) LINE
     IF(IOS.NE.0)EXIT; WRITE(JU(I,1),'(A)') TRIM(LINE)
    ENDDO
    CLOSE(JU(I,2),STATUS='DELETE'); CLOSE(JU(I,1))
    !## rasterize catchment area
    XYZFNAMES(1)=IPFFLOW(:J)//'_'//TRIM(IPF(I)%UNQLABEL)//'.ipf'
    IDFFILE=IPFFLOW(:J)//'_'//TRIM(IPF(I)%UNQLABEL)//'_catchment.idf'

    IF(.NOT.ASC2IDF_TYPE3(1,0.0,0.0,0.0,0.0))THEN
    ENDIF

   ENDIF
  ENDIF
 ENDDO
 
 END SUBROUTINE TRACEPOSTPROCESSING
 
 !###======================================================================
 LOGICAL FUNCTION TRACEPOSTPROCESSING_INIT(IPFFNAME,IPFICOL)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IPFFNAME
 INTEGER,INTENT(IN),DIMENSION(4) :: IPFICOL
 CHARACTER(LEN=52),ALLOCATABLE,DIMENSION(:) :: STRING
 INTEGER :: I,J,K,N,M,IOS,IU,NJ
 
 TRACEPOSTPROCESSING_INIT=.FALSE.
 
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=IPFFNAME,STATUS='OLD',FORM='FORMATTED',IOSTAT=IOS)
 READ(IU,*) M
 ALLOCATE(IPF(M))
 READ(IU,*) NJ; DO J=1,NJ+1; READ(IU,*); ENDDO
 ALLOCATE(STRING(NJ))
 DO J=1,M
  READ(IU,*) (STRING(K),K=1,NJ)
  READ(STRING(IPFICOL(1)),*) IPF(J)%X;     READ(STRING(IPFICOL(2)),*) IPF(J)%Y
  READ(STRING(IPFICOL(3)),*) IPF(J)%LABEL; READ(STRING(IPFICOL(4)),*) IPF(J)%ILAY
  IPF(J)%LABEL=UTL_CAP(IPF(J)%LABEL,'U')
 ENDDO
 DEALLOCATE(STRING); CLOSE(IU)

 !## get unique items
 IPF%UNQLABEL=IPF%LABEL
 CALL UTL_GETUNIQUE_CHAR(IPF%UNQLABEL,SIZE(IPF),NUNQ)
 IPF%INQ=0
 !## search for unique labels
 DO J=1,SIZE(IPF)
  DO I=1,NUNQ
   IF(IPF(J)%LABEL.EQ.IPF(I)%UNQLABEL)THEN; IPF(J)%INQ=I; EXIT; ENDIF
  ENDDO
 ENDDO
  
 !## found following unique labels
 WRITE(*,'(/A/)') 'Found following unique labels'
 DO I=1,NUNQ
  K=0; DO J=1,SIZE(IPF)
   IF(IPF(J)%INQ.EQ.I)K=K+1
  ENDDO
  WRITE(*,'(A,I10)') TRIM(IPF(I)%UNQLABEL),K
 ENDDO
 
 TRACEPOSTPROCESSING_INIT=.TRUE.
 
 END FUNCTION TRACEPOSTPROCESSING_INIT
 
 !###======================================================================
 LOGICAL FUNCTION TRACEINITOUTFILES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS,I
 CHARACTER(LEN=50) :: ERRORMESSAGE

 TRACEINITOUTFILES=.FALSE.

 I=INDEX(IFFFNAME(ISPFNAME),'.',.TRUE.)-1
 IF(I.LE.0)I=LEN_TRIM(IFFFNAME(ISPFNAME))
 !## open output channel pathlines
 IF(IMODE(1).GT.0)THEN
  IMODE(1)=UTL_GETUNIT()
  CALL OSD_OPEN(IMODE(1),FILE=IFFFNAME(ISPFNAME)(:I)//'.iff',STATUS='REPLACE',ACTION='WRITE',IOSTAT=IOS,FORM='FORMATTED')
  CALL TRACEINITOUTFILES_IFF(IMODE(1))
 ENDIF
 IF(IMODE(2).GT.0)THEN
  IMODE(2)=UTL_GETUNIT()
  CALL OSD_OPEN(IMODE(2),FILE=IFFFNAME(ISPFNAME)(:I)//'.ipf',STATUS='REPLACE',ACTION='WRITE',IOSTAT=IOS)
  CALL TRACEINITOUTFILES_IPF(IMODE(2))
 ENDIF
 IF(IOS.NE.0)THEN
  CALL OSD_IOSTAT_MSG(IOS,ERRORMESSAGE)
  IF(IMODE(1).GT.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK, &
                     TRIM(ERRORMESSAGE)//CHAR(13)//TRIM(IFFFNAME(ISPFNAME)),'Error')
  IF(IMODE(2).GT.1)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK, &
                     TRIM(ERRORMESSAGE)//CHAR(13)//TRIM(IFFFNAME(ISPFNAME)),'Error')
  RETURN
 ENDIF

 TRACEINITOUTFILES=.TRUE.

 END FUNCTION TRACEINITOUTFILES

 !###======================================================================
 SUBROUTINE TRACEINITOUTFILES_IFF(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU

 WRITE(IU,'(A)') '7'
 WRITE(IU,'(A)') 'PARTICLE_NUMBER'
 WRITE(IU,'(A)') 'ILAY'
 WRITE(IU,'(A)') 'XCRD.'
 WRITE(IU,'(A)') 'YCRD.'
 WRITE(IU,'(A)') 'ZCRD.'
 WRITE(IU,'(A)') 'TIME(YEARS)'
 WRITE(IU,'(A)') 'VELOCITY(M/DAY)'

 END SUBROUTINE TRACEINITOUTFILES_IFF

!###======================================================================
 SUBROUTINE TRACEINITOUTFILES_IPF(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU

 WRITE(IU,'(I10)') NPART
 WRITE(IU,'(A)') '17'
 WRITE(IU,'(A)') 'SP_XCRD.'
 WRITE(IU,'(A)') 'SP_YCRD.'
 WRITE(IU,'(A)') 'SP_ZCRD.'
 WRITE(IU,'(A)') 'START_ILAY'
 WRITE(IU,'(A)') 'START_IROW'
 WRITE(IU,'(A)') 'START_ICOL'
 WRITE(IU,'(A)') 'EP_XCRD.'
 WRITE(IU,'(A)') 'EP_YCRD.'
 WRITE(IU,'(A)') 'EP_ZCRD.'
 WRITE(IU,'(A)') 'END_ILAY'
 WRITE(IU,'(A)') 'END_IROW'
 WRITE(IU,'(A)') 'END_ICOL'
 WRITE(IU,'(A)') 'IDENT.NO.'
 WRITE(IU,'(A)') 'TIME(YEARS)'
 WRITE(IU,'(A)') 'DISTANCE'
 WRITE(IU,'(A)') 'CAPTURED_BY'
 WRITE(IU,'(A)') 'MAX_ILAY'
 WRITE(IU,'(A)') '0,TXT'

 END SUBROUTINE TRACEINITOUTFILES_IPF

 !###======================================================================
 LOGICAL FUNCTION TRACEREADRUNFILE(RUNFILE)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: RUNFILE
 CHARACTER(LEN=256) :: LINE
 INTEGER :: IU,JU,J,K,IDATE,IPER,ILAY,NJ,IOS,I,N,M
 LOGICAL :: LEX
 REAL :: X
 
 TRACEREADRUNFILE=.FALSE.

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=RUNFILE,STATUS='OLD',FORM='FORMATTED',IOSTAT=IOS)
 IF(.NOT.TRACECHECKRUN(IOS,'Can not open runfile:'//TRIM(RUNFILE)))RETURN
 !## optional output types
 READ(IU,*,IOSTAT=IOS) NLAY; IF(.NOT.TRACECHECKRUN(IOS,'NLAY'))RETURN
 READ(IU,*,IOSTAT=IOS) NPER; IF(.NOT.TRACECHECKRUN(IOS,'NPER'))RETURN

 IF(ALLOCATED(HFFNAME)) DEALLOCATE(HFFNAME)
 IF(ALLOCATED(ITBFNAME))DEALLOCATE(ITBFNAME)
 IF(ALLOCATED(SPFNAME)) DEALLOCATE(SPFNAME)
 IF(ALLOCATED(IFFFNAME)) DEALLOCATE(IFFFNAME)

 ALLOCATE(HFFNAME(3,NLAY,NPER))  !BDGFRF,BDGFFF,BDGFLF
 ALLOCATE(ITBFNAME(5,NLAY))      !IBOUND,TOP,BOT,POR_AQF,POR_AQT
 HFFNAME=''; ITBFNAME=''
 
 NSPFNAME=1; ALLOCATE(SPFNAME(NSPFNAME),IFFFNAME(NSPFNAME))
 
 READ(IU,'(A256)',IOSTAT=IOS) SPFNAME(1); IF(.NOT.TRACECHECKRUN(IOS,'SPFNAME'))RETURN
 !## try to read sequence number
 READ(SPFNAME(1),*,IOSTAT=IOS) NSPFNAME 
 IF(IOS.EQ.0)THEN
  DEALLOCATE(SPFNAME,IFFFNAME)
  ALLOCATE(SPFNAME(NSPFNAME),IFFFNAME(NSPFNAME))
  DO I=1,NSPFNAME
   READ(IU,*,IOSTAT=IOS) SPFNAME(I)
   IF(.NOT.TRACECHECKRUN(IOS,'SPFNAME('//TRIM(ITOS(I))//')'))RETURN
   READ(IU,*,IOSTAT=IOS) IFFFNAME(I)
   IF(.NOT.TRACECHECKRUN(IOS,'IFFFNAME('//TRIM(ITOS(I))//')'))RETURN
  ENDDO
 ELSE
  NSPFNAME=1; READ(SPFNAME(1),*,IOSTAT=IOS) SPFNAME(1)
  READ(IU,*,IOSTAT=IOS) IFFFNAME(1); IF(.NOT.TRACECHECKRUN(IOS,'IFFFNAME'))RETURN
 ENDIF
 READ(IU,*,IOSTAT=IOS) (IMODE(I),I=1,2); IF(.NOT.TRACECHECKRUN(IOS,'IMODE(.)'))RETURN
 READ(IU,*,IOSTAT=IOS) IREV ; IF(.NOT.TRACECHECKRUN(IOS,'IREV'))RETURN
 READ(IU,*,IOSTAT=IOS) ISNK;  IF(.NOT.TRACECHECKRUN(IOS,'ISNK'))RETURN
 READ(IU,*,IOSTAT=IOS) FRAC;  IF(.NOT.TRACECHECKRUN(IOS,'FRAC'))RETURN
 READ(IU,*,IOSTAT=IOS) ISTOPCRIT; IF(.NOT.TRACECHECKRUN(IOS,'ISTOPCRIT'))RETURN
 READ(IU,*,IOSTAT=IOS) TMAX;  IF(.NOT.TRACECHECKRUN(IOS,'TMAX'))RETURN
 JD0=0; JD1=0; JD2=0
 READ(IU,*,IOSTAT=IOS) IDATE; IF(.NOT.TRACECHECKRUN(IOS,'START DATE'))RETURN
 IF(NPER.GT.1)JD0=IDATETOJDATE(IDATE)
 READ(IU,*,IOSTAT=IOS) IDATE; IF(.NOT.TRACECHECKRUN(IOS,'START WINDOW'))RETURN
 IF(NPER.GT.1)JD1=IDATETOJDATE(IDATE)
 READ(IU,*,IOSTAT=IOS) IDATE; IF(.NOT.TRACECHECKRUN(IOS,'END WINDOW'))RETURN
 IF(NPER.GT.1)JD2=IDATETOJDATE(IDATE)

 !## ib,top,bot,por_aqf,por_aqt
 DO ILAY=1,NLAY
  DO J=1,5
   IF(J.NE.5.OR.ILAY.NE.NLAY)THEN
    READ(IU,*,IOSTAT=IOS) ITBFNAME(J,ILAY)
    IF(J.EQ.1)THEN
     IF(.NOT.TRACECHECKRUN(IOS,'IBOUND FOR LAYER '//TRIM(ITOS(ILAY))))RETURN
    ELSEIF(J.EQ.2)THEN
     IF(.NOT.TRACECHECKRUN(IOS,'TOP FOR LAYER '//TRIM(ITOS(ILAY))))RETURN
    ELSEIF(J.EQ.3)THEN
     IF(.NOT.TRACECHECKRUN(IOS,'BOT FOR LAYER '//TRIM(ITOS(ILAY))))RETURN
    ELSEIF(J.EQ.4)THEN
     IF(.NOT.TRACECHECKRUN(IOS,'POR AQUIFER FOR LAYER '//TRIM(ITOS(ILAY))))RETURN
    ELSEIF(J.EQ.5)THEN
     IF(.NOT.TRACECHECKRUN(IOS,'POR AQUITARD FOR LAYER '//TRIM(ITOS(ILAY))))RETURN
    ENDIF
    READ(ITBFNAME(J,ILAY),*,IOSTAT=IOS) X
    IF(IOS.NE.0)THEN
     INQUIRE(FILE=ITBFNAME(J,ILAY),EXIST=LEX)
     IF(.NOT.LEX)THEN
      IF(.NOT.TRACECHECKRUN(1,'File not found: '//TRIM(ITBFNAME(J,ILAY))))RETURN      
      RETURN
     ENDIF
     IOS=0
    ENDIF
    CALL IUPPERCASE(ITBFNAME(J,ILAY))
   ENDIF
  END DO
 END DO
 NJ=3
 ISS=0
 IF(NPER.GT.1)ISS=1
 !## frf,fff,flf
 DO IPER=1,NPER
  DO ILAY=1,NLAY
   DO J=1,NJ
    IF(J.NE.3.OR.ILAY.NE.NLAY)THEN
     READ(IU,*,IOSTAT=IOS) HFFNAME(J,ILAY,IPER)
     IF(J.EQ.1)THEN
      IF(.NOT.TRACECHECKRUN(IOS,'BDGFRF FOR LAYER '//TRIM(ITOS(ILAY))//' IPER '//TRIM(ITOS(IPER))))RETURN
     ELSEIF(J.EQ.2)THEN
      IF(.NOT.TRACECHECKRUN(IOS,'BDGFFF FOR LAYER '//TRIM(ITOS(ILAY))//' IPER '//TRIM(ITOS(IPER))))RETURN
     ELSEIF(J.EQ.3)THEN
      IF(.NOT.TRACECHECKRUN(IOS,'BDGFLF FOR LAYER '//TRIM(ITOS(ILAY))//' IPER '//TRIM(ITOS(IPER))))RETURN
     ENDIF
     INQUIRE(FILE=HFFNAME(J,ILAY,IPER),EXIST=LEX)
     IF(.NOT.LEX)THEN
      IF(.NOT.TRACECHECKRUN(1,'File not found: '//TRIM(HFFNAME(J,ILAY,IPER))))RETURN      
      RETURN
     ENDIF
     CALL IUPPERCASE(HFFNAME(J,ILAY,IPER))
    ENDIF
   END DO
  END DO
 END DO
 CLOSE(IU)

 !## get time data
 IF(ALLOCATED(PLIPER))DEALLOCATE(PLIPER)
 ALLOCATE(PLIPER(NPER+1,1)); PLIPER=0 
 DO IPER=1,NPER
  PLIPER(IPER,1)=IDATETOJDATE(UTL_IDFGETDATE(HFFNAME(1,1,IPER)))
 ENDDO
 CALL WSORT(PLIPER(:,1),1,NPER)
 !## artifically extent with one day
 PLIPER(NPER+1,1)=PLIPER(NPER,1)+(JD2-PLIPER(NPER,1)) !1
 
 TRACEREADRUNFILE=.TRUE.

 END FUNCTION TRACEREADRUNFILE

 !###======================================================================
 LOGICAL FUNCTION TRACECHECKRUN(IOS,TXT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOS
 CHARACTER(LEN=*),INTENT(IN) :: TXT

 TRACECHECKRUN=.TRUE.

 IF(IOS.EQ.0)RETURN
 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading parameter in runfile.'//CHAR(13)//TRIM(TXT),'Error')

 TRACECHECKRUN=.FALSE.

 END FUNCTION TRACECHECKRUN

 !###======================================================================
 SUBROUTINE TRACEDEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: LEX

 IF(ALLOCATED(XLC))DEALLOCATE(XLC); IF(ALLOCATED(YLC))DEALLOCATE(YLC)
 IF(ALLOCATED(ZLC))DEALLOCATE(ZLC); IF(ALLOCATED(ZLL))DEALLOCATE(ZLL)
 IF(ALLOCATED(TOT))DEALLOCATE(TOT); IF(ALLOCATED(KLC))DEALLOCATE(KLC)
 IF(ALLOCATED(JLC))DEALLOCATE(JLC); IF(ALLOCATED(ILC))DEALLOCATE(ILC)
 IF(ALLOCATED(SLAY))DEALLOCATE(SLAY)
 IF(ALLOCATED(MAXILAY))DEALLOCATE(MAXILAY)

 IF(ISPFNAME.NE.NSPFNAME)RETURN

 IF(ALLOCATED(ITBFNAME))DEALLOCATE(ITBFNAME)
 IF(ALLOCATED(HFFNAME))DEALLOCATE(HFFNAME)
 IF(ALLOCATED(XP))DEALLOCATE(XP); IF(ALLOCATED(YP))DEALLOCATE(YP)
 IF(ALLOCATED(X))DEALLOCATE(X); IF(ALLOCATED(Y))DEALLOCATE(Y)
 IF(ALLOCATED(QX))DEALLOCATE(QX); IF(ALLOCATED(QY))DEALLOCATE(QY)
 IF(ALLOCATED(QZ))DEALLOCATE(QZ); IF(ALLOCATED(POR))DEALLOCATE(POR)
 IF(ALLOCATED(DELR))DEALLOCATE(DELR); IF(ALLOCATED(DELC))DEALLOCATE(DELC)
 IF(ALLOCATED(DELX))DEALLOCATE(DELX); IF(ALLOCATED(DELY))DEALLOCATE(DELY)
 IF(ALLOCATED(LDELR))DEALLOCATE(LDELR); IF(ALLOCATED(LDELC))DEALLOCATE(LDELC)
 IF(ALLOCATED(ZBOT))DEALLOCATE(ZBOT); IF(ALLOCATED(ZTOP))DEALLOCATE(ZTOP)
 IF(ALLOCATED(BUFF))DEALLOCATE(BUFF); IF(ALLOCATED(QSS))DEALLOCATE(QSS)
 IF(ALLOCATED(IBOUND))DEALLOCATE(IBOUND); IF(ALLOCATED(NCON))DEALLOCATE(NCON)
 IF(ALLOCATED(PLIPER))DEALLOCATE(PLIPER)
 IF(ALLOCATED(SPFNAME))DEALLOCATE(SPFNAME)
 IF(ALLOCATED(IFFFNAME))DEALLOCATE(IFFFNAME)
 
 INQUIRE(UNIT=IULOG,OPENED=LEX); IF(LEX)CLOSE(IULOG)

 END SUBROUTINE TRACEDEALLOCATE

 !###======================================================================
 SUBROUTINE TRACECREATEIPF(IU,IDSCH,IPART)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDSCH,IPART,IU
 REAL :: DIST
 INTEGER :: IC1,IC2,IR1,IR2
 CHARACTER(LEN=256) :: LINE
 
 ! WRITE(IU,*) 'SP_XCRD.'
 ! WRITE(IU,*) 'SP_YCRD.'
 ! WRITE(IU,*) 'SP_ZCRD.'
 ! WRITE(IU,*) 'START_ILAY'
 ! WRITE(IU,*) 'START_IROW'
 ! WRITE(IU,*) 'START_ICOL'
 ! WRITE(IU,*) 'EP_XCRD.'
 ! WRITE(IU,*) 'EP_YCRD.'
 ! WRITE(IU,*) 'EP_ZCRD.'
 ! WRITE(IU,*) 'END_ILAY'
 ! WRITE(IU,*) 'END_IROW'
 ! WRITE(IU,*) 'END_ICOL'
 ! WRITE(IU,*) 'IDENT.NO.'
 ! WRITE(IU,*) 'TIME(YEARS)'
 ! WRITE(IU,*) 'DISTANCE'
 ! WRITE(IU,*) 'CAPTURED BY'
 ! WRITE(IU,*) 'MIN_ILAY'
 
 CALL IDFIROWICOL(IDF,IR1,IC1,XLC(IPART,1)+IDF%XMIN,YLC(IPART,1)+IDF%YMIN)
 CALL IDFIROWICOL(IDF,IR2,IC2,XLC(IPART,2)+IDF%XMIN,YLC(IPART,2)+IDF%YMIN)
 
 DIST=SQRT((XLC(IPART,1)-XLC(IPART,2))**2.0+ &
           (YLC(IPART,1)-YLC(IPART,2))**2.0+ &
           (ZLC(IPART,1)-ZLC(IPART,2))**2.0)

 ! LINE=TRIM(ADJUSTL(STRING(I,1)))
 ! DO J=2,NCOLIPF
 !  LINE=TRIM(LINE)//','//TRIM(ADJUSTL(STRING(I,J)))
 ! END DO
 ! WRITE(IU,*,IOSTAT=IOS) TRIM(LINE)!(TRIM((STRING(I,J))//','),J=1,NCOLIPF)

 LINE=TRIM(RTOS(XLC(IPART,1)+IDF%XMIN,'F',2))//','// &
      TRIM(RTOS(YLC(IPART,1)+IDF%YMIN,'F',2))//','// &
      TRIM(RTOS(ZLC(IPART,1),'F',3))//','// &
      TRIM(ITOS(SLAY(IPART)))//','// &      
      TRIM(ITOS(IR1))//','// &      
      TRIM(ITOS(IC1))//','// &      
      TRIM(RTOS(XLC(IPART,2)+IDF%XMIN,'F',2))//','// &
      TRIM(RTOS(YLC(IPART,2)+IDF%YMIN,'F',2))//','// &
      TRIM(RTOS(ZLC(IPART,2),'F',3))//','// &
      TRIM(ITOS(KLC(IPART)))//','// &      
      TRIM(ITOS(IR2))//','// &      
      TRIM(ITOS(IC2))//','// &      
      TRIM(ITOS(IPART))//','// &      
      TRIM(RTOS(TOT(IPART,2)/365.25,'E',5))//','// &
      TRIM(RTOS(DIST,'F',2))//','// &
      TRIM(ITOS(IDSCH))//','// &            
      TRIM(ITOS(MAXILAY(IPART)))
 WRITE(IU,'(A)') TRIM(LINE)

! WRITE(IU,'(2(3(E15.8,1X),I10,1X),I10,2(E15.8,1X),2I10)') &
!       XLC(IPART,1)+IDF%XMIN,YLC(IPART,1)+IDF%YMIN,ZLC(IPART,1),SLAY(IPART), &
!       XLC(IPART,2)+IDF%XMIN,YLC(IPART,2)+IDF%YMIN,ZLC(IPART,2),KLC(IPART), &
!       IPART,TOT(IPART,2)/365.25,DIST,IDSCH,MAXILAY(IPART)

 END SUBROUTINE TRACECREATEIPF

 !###======================================================================
 REAL FUNCTION TRACEGETV(XYZT,APOR)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: APOR
 REAL,INTENT(IN),DIMENSION(4,3) :: XYZT
 REAL :: DX,DY,DZ,DT,D

 DX=XYZT(1,2)-XYZT(1,1)
 DY=XYZT(2,2)-XYZT(2,1)
 DZ=XYZT(3,2)-XYZT(3,1)
 DT=XYZT(4,2)-XYZT(4,1)
 D =SQRT(DX**2.0+DY**2.0+DZ**2.0)
 !#m/year
 TRACEGETV=D/DT*APOR

 RETURN
 END FUNCTION

 !###======================================================================
 SUBROUTINE TRACEAL()
 !###======================================================================
 IMPLICIT NONE

 NCP1 =IDF%NCOL+1
 NRP1 =IDF%NROW+1
 NLP1 =NLAY+1
 !## porosity also for aquitards
 NLPOR=NLAY+NLAY-1

 ALLOCATE(QX(NCP1,IDF%NROW,NLAY))
 ALLOCATE(QY(IDF%NCOL,NRP1,NLAY))
 ALLOCATE(QZ(IDF%NCOL,IDF%NROW,NLP1))
 ALLOCATE(POR(IDF%NCOL,IDF%NROW,NLPOR))
 ALLOCATE(DELR(0:IDF%NCOL))
 ALLOCATE(DELC(0:IDF%NROW))
 ALLOCATE(LDELR(IDF%NCOL))
 ALLOCATE(LDELC(IDF%NROW))
 ALLOCATE(DELX(IDF%NCOL))
 ALLOCATE(DELY(IDF%NROW))
 ALLOCATE(ZBOT(IDF%NCOL,IDF%NROW,NLAY))
 ALLOCATE(ZTOP(IDF%NCOL,IDF%NROW,NLAY))
 ALLOCATE(BUFF(IDF%NCOL,IDF%NROW,NLAY))
 ALLOCATE(QSS(IDF%NCOL,IDF%NROW,NLAY))
 ALLOCATE(IBOUND(IDF%NCOL,IDF%NROW,NLAY))
 ALLOCATE(NCON(NLAY))

 RETURN
 END SUBROUTINE

 !###======================================================================
 SUBROUTINE TRACEALPART()
 !###======================================================================
 IMPLICIT NONE

 !## reals
 ALLOCATE(XLC(NPART,2))
 ALLOCATE(YLC(NPART,2))
 ALLOCATE(ZLC(NPART,2))
 ALLOCATE(ZLL(NPART,2))
 ALLOCATE(TOT(NPART,2))
 !## integers
 ALLOCATE(KLC(NPART))
 ALLOCATE(JLC(NPART))
 ALLOCATE(ILC(NPART))
 ALLOCATE(SLAY(NPART))
 ALLOCATE(MAXILAY(NPART))
 
 END SUBROUTINE

 !###======================================================================
 LOGICAL FUNCTION TRACEREADBUDGET(IPER,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: QCRIT=0.1 !10.0  !GROTER DAN DIT, POTENTIAL DISCARGE CELLS
 INTEGER,INTENT(IN) :: IBATCH,IPER
 CHARACTER(LEN=256) :: STRING
 INTEGER :: ILAY,IU,IROW,ICOL,ITYPE
 REAL :: QERROR
 TYPE(WIN_MESSAGE) :: MESSAGE

 TRACEREADBUDGET=.FALSE.

! if(.not.idfallocatex(idf))return

 !## read flux-right-face
 BUFF=0.0
 QX  =0.0
 DO ILAY=1,NLAY
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING='Reading BDGFRF L'//TRIM(ITOS(ILAY))//' '//TRIM(HFFNAME(1,ILAY,IPER))//'...'
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
  CALL TRACEREADBLOCK_R(BUFF(1,1,ILAY),IDF%NROW,IDF%NCOL,HFFNAME(1,ILAY,IPER),IU,4,DELC(0),DELR(0),0,QSS)
  IF(IU.LE.0)RETURN
  DO IROW=1,IDF%NROW
   DO ICOL=1,IDF%NCOL
    QX(ICOL+1,IROW,ILAY)=-BUFF(ICOL,IROW,ILAY)
   ENDDO
  ENDDO
!  idf%x=buff(:,:,ilay)
!  IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\qx_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!  endif
 ENDDO

 !## read flux-front-face
 BUFF=0.0
 QY  =0.0
 DO ILAY=1,NLAY
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING='Reading BDGFFF L'//TRIM(ITOS(ILAY))//' '//TRIM(HFFNAME(2,ILAY,IPER))//'...'
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
  CALL TRACEREADBLOCK_R(BUFF(1,1,ILAY),IDF%NROW,IDF%NCOL,HFFNAME(2,ILAY,IPER),IU,4,DELC(0),DELR(0),0,QSS)
  IF(IU.LE.0)RETURN
  DO ICOL=1,IDF%NCOL
   DO IROW=1,IDF%NROW
    QY(ICOL,IROW+1,ILAY)= BUFF(ICOL,IROW,ILAY)
   ENDDO
  ENDDO
!  idf%x=buff(:,:,ilay)
!  IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\qy_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!  endif
 ENDDO

 !## read flux-lower-face
 QZ  =0.0
 IF(NLAY.GT.1)THEN
  BUFF=0.0
  DO ILAY=1,NLAY-1
   CALL WMESSAGEPEEK(ITYPE,MESSAGE)
   STRING='Reading BDGFLF L'//TRIM(ITOS(ILAY))//' '//TRIM(HFFNAME(3,ILAY,IPER))//'...'
   IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
   IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
   CALL TRACEREADBLOCK_R(BUFF(1,1,ILAY),IDF%NROW,IDF%NCOL,HFFNAME(3,ILAY,IPER),IU,4,DELC(0),DELR(0),0,QSS)
   IF(IU.LE.0)RETURN
!   idf%x=buff(:,:,ilay)
!   IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\qz_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!   endif
  ENDDO
  DO IROW=1,IDF%NROW
   DO ICOL=1,IDF%NCOL
    DO ILAY=1,NLAY
     QZ(ICOL,IROW,ILAY+1)= BUFF(ICOL,IROW,ILAY) !standaard modflow does it wrong!
    ENDDO
   ENDDO
  ENDDO
 ENDIF

 !##initialize qss - as nett-term waterbalance! - wordt alleen gebruikt in combi. met frac!!!
 QSS=0.0
 DO ILAY=1,NLAY
  DO IROW=1,IDF%NROW
   DO ICOL=1,IDF%NCOL
    
    !## reset ibound 
    IF(IBOUND(ICOL,IROW,ILAY).EQ. 2000)IBOUND(ICOL,IROW,ILAY)=1
    IF(IBOUND(ICOL,IROW,ILAY).EQ.-2000)IBOUND(ICOL,IROW,ILAY)=1
    
    QERROR=QX(ICOL,IROW,ILAY)-QX(ICOL+1,IROW,ILAY)- &
           QY(ICOL,IROW,ILAY)+QY(ICOL,IROW+1,ILAY)- &
           QZ(ICOL,IROW,ILAY)+QZ(ICOL,IROW,ILAY+1)
    QSS(ICOL,IROW,ILAY)=-QERROR
 !##oorspronkelijk code flags all buff().lt.0 ! - NOODZAKELIJK OM PARTICLES TE LATEN STOPPEN!!!!
    IF(QSS(ICOL,IROW,ILAY).LT.-QCRIT.AND.IBOUND(ICOL,IROW,ILAY).NE.0)IBOUND(ICOL,IROW,ILAY)=-2000
    IF(QSS(ICOL,IROW,ILAY).GT. QCRIT.AND.IBOUND(ICOL,IROW,ILAY).NE.0)IBOUND(ICOL,IROW,ILAY)= 2000
 !   IF(QSS(ICOL,IROW,ILAY).LT.QCRIT.AND.ABS(IBOUND(ICOL,IROW,ILAY)).LT.1000)IBOUND(ICOL,IROW,ILAY)=1000*IBOUND(ICOL,IROW,ILAY)
   ENDDO
  ENDDO
!  idf%x=qss(:,:,ilay)
!  IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\qerror_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!  endif
 ENDDO
! deallocate(idf%x)
 
! CS=DELR(1)-DELR(0)
! DO ILAY=1,NLAY
!  CALL WRT2IDF(TRIM(PREFVAL(1))//'\tmp\qx_l'//TRIM(itos(ilay))//'.IDF',qx(1,1,ilay),IDF%NCol,IDF%NROW,0.0,cs,delr(0),delc(0))
!  CALL WRT2IDF(TRIM(PREFVAL(1))//'\tmp\qy_l'//TRIM(itos(ilay))//'.IDF',qy(1,1,ilay),IDF%NCOL,IDF%NRow,0.0,cs,delr(0),delc(0))
!  CALL WRT2IDF(TRIM(PREFVAL(1))//'\tmp\qerror_l'//TRIM(itos(ilay))//'.IDF',qss(1,1,ilay),IDF%NCOL,IDF%NROW,0.0,cs,delr(0),delc(0))
!  CALL WRT2IDF('qerror_l'//TRIM(itos(ilay))//'.IDF',qss(1,1,ilay),IDF%NCOL,IDF%NROW,0.0,cs,delr(0),delc(0))
! END DO

 TRACEREADBUDGET=.TRUE.

 END FUNCTION TRACEREADBUDGET

 !###======================================================================
 SUBROUTINE TRACEIREV()
 !###======================================================================
 IMPLICIT NONE

 QX =-1.0*QX
 QY =-1.0*QY
 QZ =-1.0*QZ

 END SUBROUTINE TRACEIREV

 !###======================================================================
 LOGICAL FUNCTION TRACEDATIN(NCONS,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: MINTHICKNESS=0.01 !## OTHERWISE TRACING IS PROBLEM, DIVIDING BY ZERO AND NO PARTICLE BEHAVIOR IN Z-DIRECTION
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER,INTENT(OUT) :: NCONS
 CHARACTER(LEN=256) :: STRING
 REAL :: APOR,XZTOP,XZBOT
 INTEGER :: ILAY,ICOL,IROW,IU,IOS,IBND,ITYPE
 LOGICAL :: LEX
 TYPE(WIN_MESSAGE) :: MESSAGE
 
 TRACEDATIN=.FALSE.

 !## all modellayers do have confining beds by default
 NCON=1

 IF(IBATCH.EQ.0)CALL WINDOWSELECT(0)

 !## read iboundary idf-files
 DO ILAY=1,NLAY
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING=ITBFNAME(1,ILAY)
  READ(STRING,*,IOSTAT=IOS) IBND
  IF(IOS.EQ.0)THEN
   IBOUND(1:IDF%NCOL,1:IDF%NROW,ILAY)=IBND
   STRING='Constant value ['//TRIM(ITOS(IBND))//'] for IBOUND L'//TRIM(ITOS(ILAY))
  ELSE
   STRING='Reading IBOUND L'//TRIM(ITOS(ILAY))//' '//TRIM(ITBFNAME(1,ILAY))//'...'
   LEX=.TRUE.
   IF(ILAY.GT.1)THEN
    LEX=.TRUE.
    IF(TRIM(ITBFNAME(1,ILAY)).EQ.TRIM(ITBFNAME(1,ILAY-1)))LEX=.FALSE.
   ENDIF
   IF(LEX)THEN
    CALL TRACEREADBLOCK_I(IBOUND(1,1,ILAY),IDF%NROW,IDF%NCOL,ITBFNAME(1,ILAY),IU,1,DELC,DELR)
    IF(IU.LE.0)RETURN
   ELSE
    STRING='Reusing '//TRIM(ITBFNAME(1,ILAY-1))
    IBOUND(:,:,ILAY)=IBOUND(:,:,ILAY-1)
   ENDIF
  ENDIF
  IF(IBATCH.EQ.1)WRITE(*,*) TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)

  DO IROW=1,IDF%NROW
   DO ICOL=1,IDF%NCOL

    IF(IBOUND(ICOL,IROW,ILAY).LT.0)THEN
     IBOUND(ICOL,IROW,ILAY)=-1
     !## POTENTIAL DISCHARGE CELLS ARE FLAGGED IN THE IBOUND ARRAY
     !## BY MULTIPLYING IBOUND VALUE BY 1000. THIS LOOP FLAGS CHANGING
     !## CELLS THAT HAVE NET DISCHARGE.
     IBOUND(ICOL,IROW,ILAY)=1000*IBOUND(ICOL,IROW,ILAY)
    ENDIF
    IF(IBOUND(ICOL,IROW,ILAY).GT.0)IBOUND(ICOL,IROW,ILAY)= 1
   END DO
  END DO
  !## FLAG SURROUNDING MODEL AS DISCHARGE AREA!!!!

 ENDDO

 !## read vertical coordinate data (TOP)
 DO ILAY=1,NLAY
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING=ITBFNAME(2,ILAY)
  READ(STRING,*,IOSTAT=IOS) XZTOP
  IF(IOS.EQ.0)THEN
   ZTOP(1:IDF%NCOL,1:IDF%NROW,ILAY)=XZTOP
   STRING='Constant value ['//TRIM(RTOS(XZTOP,'F',2))//'] for ZTOP L'//TRIM(ITOS(ILAY))
  ELSE
   STRING='Reading TOP L'//TRIM(ITOS(ILAY))//' '//TRIM(ITBFNAME(2,ILAY))//'...'
   !## non-smoothing in case of startingpoints will be not smoothed!
   CALL TRACEREADBLOCK_R(ZTOP(1,1,ILAY),IDF%NROW,IDF%NCOL,ITBFNAME(2,ILAY),IU,2,DELC,DELR,0,BUFF)
   IF(IU.LE.0)RETURN
  ENDIF
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
 ENDDO

 !## read vertical coordinate data (BOT)
 DO ILAY=1,NLAY
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING=ITBFNAME(3,ILAY)
  READ(STRING,*,IOSTAT=IOS) XZBOT
  IF(IOS.EQ.0)THEN
   ZBOT(1:IDF%NCOL,1:IDF%NROW,ILAY)=XZBOT
   STRING='Constant value ['//TRIM(RTOS(XZBOT,'F',2))//'] for ZBOT L'//TRIM(ITOS(ILAY))
  ELSE
   STRING='Reading BOT L'//TRIM(ITOS(ILAY))//' '//TRIM(ITBFNAME(3,ILAY))//'...'
   !## non-smoothing in case of startingpoints will be not smoothed!
   CALL TRACEREADBLOCK_R(ZBOT(1,1,ILAY),IDF%NROW,IDF%NCOL,ITBFNAME(3,ILAY),IU,2,DELC,DELR,0,BUFF)
   IF(IU.LE.0)RETURN
  ENDIF
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
 ENDDO

 !## check top/bot consequentie - minimal thickness=minthickness
 NCONS=0
 DO IROW=1,IDF%NROW
  DO ICOL=1,IDF%NCOL
   DO ILAY=1,NLAY
    IF(ZTOP(ICOL,IROW,ILAY).LT.ZBOT(ICOL,IROW,ILAY)+MINTHICKNESS)THEN
     NCONS=NCONS+1
     ZBOT(ICOL,IROW,ILAY)=ZTOP(ICOL,IROW,ILAY)-MINTHICKNESS
    ENDIF
    IF(ILAY.LT.NLAY)THEN
     IF(ZBOT(ICOL,IROW,ILAY).LT.ZTOP(ICOL,IROW,ILAY+1)+MINTHICKNESS)THEN
      NCONS=NCONS+1
      ZTOP(ICOL,IROW,ILAY+1)=ZBOT(ICOL,IROW,ILAY)-MINTHICKNESS
     ENDIF
    ENDIF
   ENDDO
  ENDDO
 ENDDO

 !## assign porosity
 DO ILAY=1,NLAY
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING=ITBFNAME(4,ILAY)
  READ(STRING,*,IOSTAT=IOS) APOR
  IF(IOS.EQ.0)THEN
   POR(1:IDF%NCOL,1:IDF%NROW,ILAY)=APOR
   STRING='Constant value ['//TRIM(RTOS(APOR,'F',2))//'] for POR_AQF L'//TRIM(ITOS(ILAY))
  ELSE
   STRING='Reading POR_AQF L'//TRIM(ITOS(ILAY))//' '//TRIM(ITBFNAME(4,ILAY))//'...'
   CALL TRACEREADBLOCK_R(POR(1,1,ILAY),IDF%NROW,IDF%NCOL,ITBFNAME(4,ILAY),IU,2,DELC,DELR,1,BUFF)
   IF(IU.LE.0)RETURN
  ENDIF
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
 ENDDO

 DO ILAY=1,NLAY-1
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING=ITBFNAME(5,ILAY)
  READ(STRING,*,IOSTAT=IOS) APOR
  IF(IOS.EQ.0)THEN
   POR(1:IDF%NCOL,1:IDF%NROW,NLAY+ILAY)=APOR
   STRING='Constant value ['//TRIM(RTOS(APOR,'F',2))//'] for POR_AQT L'//TRIM(ITOS(ILAY))
  ELSE
   STRING='Reading POR_AQT L'//TRIM(ITOS(ILAY))//' '//TRIM(ITBFNAME(5,ILAY))//'...'
   CALL TRACEREADBLOCK_R(POR(1,1,NLAY+ILAY),IDF%NROW,IDF%NCOL,ITBFNAME(5,ILAY),IU,2,DELC,DELR,1,BUFF)
   IF(IU.LE.0)RETURN
  ENDIF
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
 ENDDO

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'')

! IF(.not.IDFALLOCATEX(IDF))stop
! DO ILAY=1,NLAY
!  idf%x=REAL(IBOUND(:,:,ilay))
!  IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\ibound_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!  endif
! END DO
! DO ILAY=1,NLAY
!  idf%x=ZTOP(:,:,ilay)
!  IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\ztop_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!  endif
! END DO
! DO ILAY=1,NLAY
!  idf%x=Zbot(:,:,ilay)
!  IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\zBOT_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!  endif
! END DO
! DO ILAY=1,(NLAY*2)-1
!  idf%x=POR(:,:,ilay)
!  IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\por_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!  endif
! END DO

 TRACEDATIN=.TRUE.

 END FUNCTION TRACEDATIN

 !###====================================================================
 SUBROUTINE TRACEDELRC()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,II

 IF(IDF%IEQ.EQ.0)THEN

  !#fill global coordinates
  DELR(0)=0.0 !IDF%XMIN
  DO I=1,IDF%NCOL
   DELR(I)=REAL(I)*IDF%DX
!   DELR(I)=DELR(0)+REAL(I)*IDF%DX
!   DELR(I)=DELR(I-1)+IDF%DX
  END DO
!  DO I=1,IDF%NCOL
  DO I=0,IDF%NCOL
   DELR(I)=IDF%XMIN+DELR(I)
  ENDDO
  
  DELC(0)=0.0 !IDF%YMAX
  DO I=1,IDF%NROW
   DELC(I)=-REAL(I)*IDF%DY
!   DELC(I)=DELC(0)-REAL(I)*IDF%DY
!   write(*,*) delc(i),delc(0),real(i)*idf%dy
!   DELC(I)=DELC(I-1)-IDF%DY
  END DO
!  DO I=1,IDF%NROW
  DO I=0,IDF%NROW
   DELC(I)=IDF%YMAX+DELC(I)
  ENDDO
  
  !#fill local coordinates
!  LDELR(1)=IDF%DX
  DO I=1,IDF%NCOL
   LDELR(I)=REAL(I)*IDF%DX
!   LDELR(I)=LDELR(I-1)+IDF%DX
  END DO
!  LDELC(IDF%NROW)=IDF%DY
  II=0
  DO I=IDF%NROW,1,-1 
   II=II+1
   LDELC(I)=REAL(II)*IDF%DY
  ENDDO
!  DO I=2,IDF%NROW
!   II=IDF%NROW+1-I
!   LDELC(II)=LDELC(II+1)+IDF%DY
!  ENDDO

  DELX=IDF%DX
  DELY=IDF%DY

 ELSE

  DELR(0)=IDF%XMIN
  DO I=1,IDF%NCOL
   !## fill global coordinates
   DELR(I) =IDF%SX(I)
   !## delta x
   DELX(I) =DELR(I)-DELR(I-1)
   !## fill local coordinates
   LDELR(I)=DELR(I)-IDF%XMIN
  END DO
  DELC(0)=IDF%YMAX
  DO I=1,IDF%NROW
   !## fill global coordinates
   DELC(I) =IDF%SY(I)
   !## delta y
   DELY(I) =DELC(I-1)-DELC(I)
   !## fill local coordinates
   LDELC(I)=DELC(I-1)-IDF%YMIN
  END DO

 ENDIF
 
! write(*,*) idf%xmin,idf%xmax
! do i=1,idf%ncol
!  write(*,*) i,delr(i),ldelr(i)
! enddo
! write(*,*) idf%xmin,idf%xmax
! write(*,*) idf%ymin,idf%ymax
! do i=1,idf%nrow
!  write(*,*) i,delc(i),ldelc(i)
! enddo
! write(*,*) idf%ymin,idf%ymax
 
 END SUBROUTINE

END MODULE MOD_PLINES_TRACE

