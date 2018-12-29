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
MODULE MOD_IPEST_ANALYSER

USE IMODVAR, ONLY : DP_KIND,SP_KIND
USE MODPLOT, ONLY : MPW,MP,MXMPLOT
USE MOD_PREF_PAR, ONLY : PREFVAL
USE WINTERACTER
USE MOD_COLOURS
USE MOD_IPEST_PAR
USE LSQ
USE RESOURCE
USE MOD_LEGEND, ONLY : LEG_MAIN,LEG_PREDEFINED_WRITELEG,LEG_READ
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_GETUNIQUE_INT,ITOS,UTL_MESSAGEHANDLE,UTL_GOODNESS_OF_FIT,RTOS,UTL_GETMED,UTL_GETAXESCALES, &
             SXVALUE,SYVALUE,NSX,NSY,UTL_GETMED,UTL_STDEF,UTL_IDFGETCLASS,UTL_IDFCRDCOR,UTL_GETUNIQUE_DINT
USE MOD_DBL
USE MOD_GRAPH, ONLY : GRAPH_PLOTAXES,AXESOBJ,GRAPHUNITS,GRAPHAREA
USE MOD_POLINT, ONLY : POL1LOCATE

TYPE(AXESOBJ),PRIVATE :: AXES

CONTAINS

 !###======================================================================
 SUBROUTINE IPEST_ANALYSE_MAIN(ITYPE,MESSAGE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 
 CALL WDIALOGSELECT(MESSAGE%WIN) 
  
 SELECT CASE (ITYPE)
 
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDCANCEL)
     CALL IPEST_ANALYSE_CLOSE
    CASE (ID_OPEN)
     CALL UTL_MESSAGEHANDLE(0)
     CALL IPEST_ANALYSE_READLOG('d:\COMPILE\OSSDELTARES\iMOD\TUTORIALS\IMOD_USER\MODELS\TUT_PST_OPTIMIZE\pest')
!     CALL IPEST_ANALYSE_READLOG('d:\IMOD-MODELS\SWISS\CALIBRATION\pest')
     CALL UTL_MESSAGEHANDLE(1)
     CALL IPEST_ANALYSE_PLOTGRAPH(0)
    CASE (ID_LEGEND2)
     CALL LEG_MAIN(MXMPLOT)
     CALL IPEST_ANALYSE_PLOTGRAPH(IDF_PICTURE6)
     CASE (IDHELP)
   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
    CASE (IDF_MENU1,IDF_RADIO1,IDF_RADIO2,IDF_RADIO3,IDF_RADIO4,IDF_RADIO5,IDF_RADIO7,IDF_RADIO8,IDF_RADIO9,IDF_RADIO10,IDF_CHECK1, &
          IDF_CHECK2,IDF_CHECK3)
     IF(MESSAGE%VALUE1.EQ.MESSAGE%VALUE2)CALL IPEST_ANALYSE_PLOTGRAPH(MESSAGE%VALUE2)
   END SELECT
  CASE (RESIZE,EXPOSE)
   CALL IPEST_ANALYSE_PLOTGRAPH(0)
  
 END SELECT
     
 END SUBROUTINE IPEST_ANALYSE_MAIN 
 
 !###======================================================================
 SUBROUTINE IPEST_ANALYSE_READLOG(DIR) 
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: I,J,IU,NI,NP,NU,IOS,N,M,IACT,ISS,NPER
 CHARACTER(LEN=256) :: LINE
 INTEGER,DIMENSION(:),ALLOCATABLE :: IGROUP
 CHARACTER(LEN=15),DIMENSION(:),ALLOCATABLE :: ACRONYM
 CHARACTER(LEN=52) :: TXT
 CHARACTER(LEN=52),DIMENSION(:),ALLOCATABLE :: ITXT
 INTEGER(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: TMP
 
 !## find number of iterations and parameters
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(DIR)//'\log_pest_efficiency_mf2005.txt',STATUS='OLD',ACTION='READ')
 DO I=1,2; READ(IU,*,IOSTAT=IOS); IF(IOS.NE.0)THEN; CLOSE(IU); RETURN; ENDIF; ENDDO
 NI=0; DO; READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT; NI=NI+1; ENDDO
 
 CALL IPEST_ANALYSE_ALLOCATE(NI,0)

 REWIND(IU)
 DO I=1,2;  READ(IU,*,IOSTAT=IOS); IF(IOS.NE.0)THEN; CLOSE(IU); RETURN; ENDIF; ENDDO
 DO I=1,NI; READ(IU,*,IOSTAT=IOS) IPEST(I)%J; IF(IOS.NE.0)EXIT; ENDDO
 
 CLOSE(IU)

 !## find number of parameters
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(DIR)//'\log_pest_mf2005.txt',STATUS='OLD',ACTION='READ')
 !## find number of parameters
 DO I=1,2
  NP=0
  DO
   READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
   IF(INDEX(TRIM(LINE),'Parameters').GT.0)THEN
    READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
    DO
    READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
     IF(TRIM(LINE).EQ.'')EXIT
     READ(LINE,'(I2)') IACT
     IF(IACT.EQ.1)NP=NP+1
     IF(I.EQ.2)THEN
      READ(LINE,'(I2,98X,I10,20X,A15)') IACT,IGROUP(NP),ACRONYM(NP)
     ENDIF
    ENDDO
    EXIT
   ENDIF
  ENDDO
  IF(I.EQ.1)ALLOCATE(IGROUP(NP),ACRONYM(NP))
  REWIND(IU)
 ENDDO
 
 !## get unique set of groups
 CALL UTL_GETUNIQUE_INT(IGROUP,NP,NU,-999)
 CALL IPEST_ANALYSE_ALLOCATE(0,NU)

 !## set new list of parameters
 DO I=1,NU
  PARAM(I)%IGROUP=IGROUP(I)
  !## look for correct acronym
  DO J=1,NP
   IF(PARAM(I)%IGROUP.EQ.IGROUP(J))THEN
    PARAM(I)%ACRONYM=ACRONYM(J)
   ENDIF
  ENDDO
 ENDDO
 DEALLOCATE(IGROUP,ACRONYM)
 NP=NU
 
 !## continue reading
 NPER=2
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
  IF(INDEX(LINE,'*** Next Outer Iteration ***').GT.0)NPER=NPER+1

  IF(INDEX(LINE,'Upgrade Vector Parameter History:').GT.0)THEN
   READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
   
   !## how many iterations
   N=0; DO I=0,SIZE(IPEST)
    WRITE(TXT,'(A4,I3.3)') 'ITER',I
    IF(INDEX(LINE,TRIM(TXT)).GT.0)N=N+1
   ENDDO
   DO I=1,SIZE(PARAM)
    READ(IU,'(4X,A15,99F10.0)') IPEST(1)%CPARAM(I),(IPEST(J)%ALPHA(I),J=N,1,-1)
   ENDDO

  ENDIF

  IF(INDEX(LINE,'Confidence Limits (96%):').GT.0)THEN
   READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
   DO I=1,SIZE(PARAM)
    READ(IU,'(A256)') LINE
    READ(LINE,'(15X,F15.0,15X,F15.0)',IOSTAT=IOS) IPEST(NPER)%LOWER(I),IPEST(NPER)%UPPER(I)
    !## insensitive
    IF(IOS.NE.0)THEN
     IPEST(NPER)%LOWER(I)=IPEST(NPER-1)%LOWER(I)
     IPEST(NPER)%UPPER(I)=IPEST(NPER-1)%UPPER(I)
    ENDIF
   ENDDO
  ENDIF

!  IF(INDEX(LINE,'Parameter Correlation Matrix').GT.0)THEN
!   DO I=1,5; READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT; ENDDO
!   DO I=1,SIZE(PARAM)
!    READ(IU,'(A256)') LINE
!    READ(LINE,'(15X,F15.0,15X,F15.0)',IOSTAT=IOS) (IPEST(NPER)%CORRELATION(I,J),J=1,SIZE(PARAM)
!    !## insensitive
!    IF(IOS.NE.0)THEN
!     IPEST(NPER)%LOWER(I)=IPEST(NPER-1)%LOWER(I)
!     IPEST(NPER)%UPPER(I)=IPEST(NPER-1)%UPPER(I)
!    ENDIF
!   ENDDO
!  ENDIF
  
 ENDDO
 
 !## find number of parameters
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(DIR)//'\log_pest_sensitivity_mf2005.txt',STATUS='OLD',ACTION='READ')
 DO I=1,2; READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT; ENDDO
 DO I=1,SIZE(IPEST)-1
  READ(IU,'(10X,99F15.0)') (IPEST(I+1)%SENSI(J),J=1,NP)
 ENDDO
 CLOSE(IU)
 
 !## find residuals
 ISS=0
 DO I=1,2
  DO J=1,SIZE(IPEST) !-1
   NP=0
   IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(DIR)//'\log_pest_residual_'//TRIM(ITOS(J-1))//'mf2005.txt',STATUS='OLD',ACTION='READ')
   !# read number of ipf files
   DO !K=1,2
    READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
    !## steady-state
    IF(INDEX(LINE,'ILAY,            MSR,            MDL,').GT.0)THEN; ISS=1; LSS=.TRUE.; EXIT; ENDIF
    !## transient
    IF(INDEX(LINE,'ILAY,          WEIGH,            MSR,').GT.0)THEN; ISS=2; LSS=.FALSE.; EXIT; ENDIF
   ENDDO

   IF(ISS.EQ.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONNO,'Cannot determine in the log_pest_residual_*mf2005.txt files'//CHAR(13)// &
    'Whether they are configured for a steady or transient simulation','Error')
   ENDIF

   DO
    READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
    NP=NP+1
    IF(I.EQ.2)THEN
     IF(LSS)THEN
      READ(LINE,'(2(F15.0,1X),I10,1X,2(F15.0,1X),48X,F15.0)') IPEST(J)%MEASURE(NP)%X,IPEST(J)%MEASURE(NP)%Y,IPEST(J)%MEASURE(NP)%ILAY, &
                                           IPEST(J)%MEASURE(NP)%OBS,IPEST(J)%MEASURE(NP)%COM,IPEST(J)%MEASURE(NP)%W
      IPEST(J)%MEASURE(NP)%IDATE=0
     ELSE
      READ(LINE,'(2(F15.0,1X),I10,1X,3(F15.0,1X),5(16X),44X,I15)') IPEST(J)%MEASURE(NP)%X,IPEST(J)%MEASURE(NP)%Y,IPEST(J)%MEASURE(NP)%ILAY, &
                                           IPEST(J)%MEASURE(NP)%W,IPEST(J)%MEASURE(NP)%OBS,IPEST(J)%MEASURE(NP)%COM,IPEST(J)%MEASURE(NP)%IDATE
!                                           ,123x,i15)
     ENDIF
    ENDIF
   ENDDO
   CLOSE(IU)
   
   IF(I.EQ.1)THEN; IPEST(J)%NMEASURE=NP; ALLOCATE(IPEST(J)%MEASURE(NP)); ENDIF

  ENDDO
 ENDDO
 
 !## only entries of first cycle needed
 M=1
 !## get unique layers
 N=0; DO I=1,M; N=N+IPEST(I)%NMEASURE; ENDDO; ALLOCATE(TMP(N))
 N=0; DO I=1,M; DO J=1,IPEST(I)%NMEASURE; N=N+1; TMP(N)=IPEST(I)%MEASURE(J)%ILAY; ENDDO; ENDDO
 CALL UTL_GETUNIQUE_DINT(TMP,N,NU)
 ALLOCATE(CLAY(NU),ILAY(NU),LLAY(NU))
 DO I=1,NU; WRITE(CLAY(I),'(I10)') TMP(I); LLAY(I)=TMP(I); ENDDO; ILAY=1
 !## get unique periods
 N=0; DO I=1,M; DO J=1,IPEST(I)%NMEASURE; N=N+1; TMP(N)=IPEST(I)%MEASURE(J)%IDATE; ENDDO; ENDDO
 CALL UTL_GETUNIQUE_DINT(TMP,N,NU)
 ALLOCATE(CPERIOD(NU),IPERIOD(NU),LPERIOD(NU)); DO I=1,NU; WRITE(CPERIOD(I),'(I16)') TMP(I); LPERIOD(I)=TMP(I); ENDDO; IPERIOD=1

 CALL WDIALOGPUTMENU(IDF_MENU2,CPERIOD,SIZE(CPERIOD),IPERIOD)
 CALL WDIALOGPUTMENU(IDF_MENU3,CLAY   ,SIZE(CLAY)   ,ILAY)

 ALLOCATE(ITXT(SIZE(IPEST)))
 DO I=1,SIZE(IPEST)
  IF(I.EQ.1)THEN
   ITXT(I)='Initial'
  ELSE
   ITXT(I)='Cycle '//TRIM(ITOS(I-1))
  ENDIF
 ENDDO
 CALL WDIALOGPUTMENU(IDF_MENU1,ITXT,SIZE(IPEST),1)
 DEALLOCATE(ITXT)
 
 END SUBROUTINE IPEST_ANALYSE_READLOG
 
 !###======================================================================
 SUBROUTINE IPEST_ANALYSE_INIT() 
 !###======================================================================
 IMPLICIT NONE 
 INTEGER :: IOS
 
 CALL WINDOWSELECT(0); IF(WMENUGETSTATE(ID_IPESTANALYSER,2).EQ.1)THEN
  CALL IPEST_ANALYSE_CLOSE(); RETURN
 ENDIF

 IF(LEG_PREDEFINED_WRITELEG(4))CALL LEG_READ(MP(MXMPLOT)%LEG,TRIM(PREFVAL(1))//'\tmp\tmp.leg',IOS)
 
 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_IPESTANALYSER,2,1)
 !## fill in dialog
 CALL WDIALOGLOAD(ID_DIPESTANALYSE,ID_DIPESTANALYSE)

 CALL WDIALOGPUTIMAGE(ID_PROP1,ID_ICONPROPERTIES,1)
 CALL WDIALOGPUTIMAGE(ID_PROP2,ID_ICONPROPERTIES,1)
 CALL WDIALOGPUTIMAGE(ID_LEGEND1,ID_ICONLEGEND,1)
 CALL WDIALOGPUTIMAGE(ID_LEGEND2,ID_ICONLEGEND,1)
 CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE IPEST_ANALYSE_INIT

 !###====================================================================
 SUBROUTINE IPEST_ANALYSE_PLOTGRAPH(ID)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 REAL(KIND=DP_KIND) :: YMIN,YMAX,XMIN,XMAX,DX,DY
 INTEGER :: I
 CHARACTER(LEN=52) :: XTITLE,YTITLE
 INTEGER,DIMENSION(6) :: GID
 DATA GID/IDF_PICTURE1,IDF_PICTURE2,IDF_PICTURE3,IDF_PICTURE4,IDF_PICTURE5,IDF_PICTURE6/

 !## PLOT TRACKBAR ZORGT ERVOOR DAT ER MNDER TE ZIEN IS ...
 
 CALL WDIALOGSELECT(ID_DIPESTANALYSE)

 DO I=1,SIZE(GID)

  !## skip pictures
  IF(ID.NE.0)THEN
   SELECT CASE (ID)
    CASE (IDF_RADIO1,IDF_RADIO2,IDF_CHECK2)
     IF(I.NE.3)CYCLE
    CASE (IDF_RADIO3,IDF_RADIO4,IDF_CHECK1,IDF_CHECK3)
     IF(I.NE.2)CYCLE
    CASE (IDF_RADIO5,IDF_RADIO7)
     IF(I.NE.5)CYCLE
    CASE (IDF_RADIO8,IDF_RADIO9,IDF_RADIO10)
     IF(I.NE.1)CYCLE
    CASE (IDF_PICTURE6)
     IF(I.NE.6)CYCLE
   END SELECT
  ENDIF
  
  CALL IGRSELECT(3,GID(I)); CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0); CALL IGRAREACLEAR()
  CALL IGRFILLPATTERN(SOLID)

  XMIN= HUGE(1.0D0); YMIN=XMIN
  XMAX=-HUGE(1.0D0); YMAX=YMIN

  !## set dimensions
  SELECT CASE (I)
   !## j
   CASE (1); CALL IPEST_ANALYSE_DIMGRAPH1(XMIN,YMIN,XMAX,YMAX,XTITLE,YTITLE)
   !## alpha
   CASE (2); CALL IPEST_ANALYSE_DIMGRAPH2(XMIN,YMIN,XMAX,YMAX,XTITLE,YTITLE)
   !## sensitivity
   CASE (3); CALL IPEST_ANALYSE_DIMGRAPH3(XMIN,YMIN,XMAX,YMAX,XTITLE,YTITLE)
!   !## correlation
!   CASE (4);
   !## histogram/scatter
   CASE (5); IF(.NOT.IPEST_ANALYSE_DIMGRAPH5(XMIN,YMIN,XMAX,YMAX,XTITLE,YTITLE))CYCLE
   !## xy plot
   CASE (6); IF(.NOT.IPEST_ANALYSE_DIMGRAPH6(XMIN,YMIN,XMAX,YMAX,XTITLE,YTITLE))CYCLE
   CASE DEFAULT; CYCLE
  END SELECT
  
  DX=(XMAX-XMIN)/20.0D0; XMIN=XMIN-DX; XMAX=XMAX+DX 
  DY=(YMAX-YMIN)/20.0D0; YMIN=YMIN-DY; YMAX=YMAX+DY 

 !## set axes
  CALL IPEST_ANALYSE_PLOT_AXES(XMIN,YMIN,XMAX,YMAX,.FALSE.,XTITLE,YTITLE)

  !## plot figure
  SELECT CASE (I)
   CASE (1); CALL IPEST_ANALYSE_PLOTGRAPH1()
   CASE (2); CALL IPEST_ANALYSE_PLOTGRAPH2(YMIN,YMAX)
   CASE (3); CALL IPEST_ANALYSE_PLOTGRAPH3(YMIN,YMAX)
   !CASE (3); CALL IPEST_ANALYSE_PLOTGRAPH3()
   CASE (5); CALL IPEST_ANALYSE_PLOTGRAPH5(XMIN,YMIN,XMAX,YMAX)
   CASE (6); CALL IPEST_ANALYSE_PLOTGRAPH6()
  END SELECT

  CALL IPEST_ANALLYSE_PLOTIPOS(I,XMIN,YMIN,YMAX)

 ENDDO
 
 CALL IGRSELECT(DRAWWIN,MPW%IWIN); CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
 CALL DBL_IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX,IOFFSET=1)

 END SUBROUTINE IPEST_ANALYSE_PLOTGRAPH

 !###====================================================================
 SUBROUTINE IPEST_ANALYSE_DIMGRAPH1(XMIN,YMIN,XMAX,YMAX,XTITLE,YTITLE)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(INOUT) :: XMIN,YMIN,XMAX,YMAX 
 CHARACTER(LEN=*),INTENT(OUT) :: XTITLE,YTITLE
 INTEGER :: IPARAM,IPOS,I
 REAL(KIND=DP_KIND) :: D,T
 
 XTITLE='Iteration (#)' 
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO8,IPARAM)
 IF(IPARAM.EQ.1)THEN
  YTITLE='Objective Function Value (m2)'
 ELSEIF(IPARAM.EQ.2)THEN
  YTITLE='Average Absolute Objective Function Value (m)'
 ELSEIF(IPARAM.EQ.3)THEN
  YTITLE='Average Objective Function Value (m)'
 ENDIF

 XMIN=0.0D0; XMAX=SIZE(IPEST)-1
 
 IF(IPARAM.EQ.1)THEN
  YMIN=MINVAL(IPEST%J); YMAX=MAXVAL(IPEST%J)
 ELSE
  YMIN=HUGE(1.0D0); YMAX=-1.0D0*HUGE(1.0D0)
  DO IPOS=1,SIZE(IPEST)
   T=0.0D0
   DO I=1,IPEST(IPOS)%NMEASURE
    D=IPEST(IPOS)%MEASURE(I)%COM-IPEST(IPOS)%MEASURE(I)%OBS
    IF(IPARAM.EQ.2)D=ABS(D)
    T=T+D
   ENDDO
   T=T/DBLE(IPEST(IPOS)%NMEASURE)
   YMIN=MIN(YMIN,T); YMAX=MAX(YMAX,T)
   IPEST(IPOS)%T=T
  ENDDO
 ENDIF

 END SUBROUTINE IPEST_ANALYSE_DIMGRAPH1

 !###====================================================================
 SUBROUTINE IPEST_ANALYSE_DIMGRAPH2(XMIN,YMIN,XMAX,YMAX,XTITLE,YTITLE)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(INOUT) :: XMIN,YMIN,XMAX,YMAX 
 CHARACTER(LEN=*),INTENT(OUT) :: XTITLE,YTITLE
 INTEGER :: I,IPARAM,ICHECK,IPOS
 
 YTITLE='Multiplication Factor Alpha (-)'

 CALL WDIALOGGETMENU(IDF_MENU1,IPOS)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,IPARAM)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,ICHECK)
 
 XMIN=0.0D0
 IF(IPARAM.EQ.1)THEN
  XTITLE='Iteration (#)' 
  XMAX=SIZE(IPEST)-1
 ELSE
  XTITLE='Parameters (#)' 
  XMAX=SIZE(PARAM)+1
 ENDIF

 YMIN= HUGE(1.0D0); YMAX=-HUGE(1.0D0)
 IF(ICHECK.EQ.0.OR.IPARAM.EQ.1)THEN
  DO I=1,SIZE(IPEST)
   YMIN=MIN(YMIN,MINVAL(IPEST(I)%ALPHA))
   YMAX=MAX(YMAX,MAXVAL(IPEST(I)%ALPHA))
  ENDDO
 ELSE
  DO I=IPOS,IPOS
   YMIN=MIN(YMIN,MINVAL(IPEST(I)%ALPHA))
   YMAX=MAX(YMAX,MAXVAL(IPEST(I)%ALPHA))
   IF(IPARAM.EQ.2)THEN
    YMIN=MIN(YMIN,MINVAL(IPEST(I)%LOWER))
    YMAX=MAX(YMAX,MAXVAL(IPEST(I)%UPPER))
   ENDIF
  ENDDO
 ENDIF
 
 END SUBROUTINE IPEST_ANALYSE_DIMGRAPH2
 
 !###====================================================================
 SUBROUTINE IPEST_ANALYSE_DIMGRAPH3(XMIN,YMIN,XMAX,YMAX,XTITLE,YTITLE)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(INOUT) :: XMIN,YMIN,XMAX,YMAX 
 CHARACTER(LEN=*),INTENT(OUT) :: XTITLE,YTITLE
 INTEGER :: I,IPARAM
 
 YTITLE='Sensitivity (%)'

 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IPARAM)
 
 XMIN=0.0D0
 IF(IPARAM.EQ.1)THEN
  XTITLE='Iteration (#)' 
  XMAX=SIZE(IPEST)-1
 ELSE
  XTITLE='Parameters (#)' 
  XMAX=SIZE(PARAM)+1
 ENDIF

 YMIN= HUGE(1.0D0); YMAX=-HUGE(1.0D0)
 DO I=1,SIZE(IPEST)
  YMIN=MIN(YMIN,MINVAL(IPEST(I)%SENSI))
  YMAX=MAX(YMAX,MAXVAL(IPEST(I)%SENSI))
 ENDDO

 END SUBROUTINE IPEST_ANALYSE_DIMGRAPH3

 !###====================================================================
 LOGICAL FUNCTION IPEST_ANALYSE_DIMGRAPH5(XMIN,YMIN,XMAX,YMAX,XTITLE,YTITLE)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(INOUT) :: XMIN,YMIN,XMAX,YMAX 
 CHARACTER(LEN=*),INTENT(OUT) :: XTITLE,YTITLE
 INTEGER :: IPOS,IPARAM,N,J
 
 IPEST_ANALYSE_DIMGRAPH5=.FALSE.
 
 CALL WDIALOGGETMENU(IDF_MENU1,IPOS)
 IF(IPEST(IPOS)%NMEASURE.EQ.0)RETURN

 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO5,IPARAM)
 
 IF(IPARAM.EQ.1)THEN
  XTITLE='Measure (m+MSL)'
  YTITLE='Computed (m+MSL)'
 ELSE
  YTITLE='Difference'
  XTITLE='Frequency (%)'
 ENDIF
 
 IF(IPARAM.EQ.1)THEN
  XMIN=     HUGE(1.0D0); YMIN=     HUGE(1.0D0)
  XMAX=-1.0*HUGE(1.0D0); YMAX=-1.0*HUGE(1.0D0)
  DO IPOS=1,SIZE(IPEST)
   DO J=1,IPEST(IPOS)%NMEASURE
!    IF(.NOT.IPEST_ANALYSE_QUERY(IPEST(IPOS)%MEASURE(J)%ILAY,IPEST(IPOS)%MEASURE(J)%IDATE))CYCLE
    XMIN=MIN(XMIN,IPEST(IPOS)%MEASURE(J)%OBS); XMAX=MAX(XMAX,IPEST(IPOS)%MEASURE(J)%OBS)
    YMIN=MIN(YMIN,IPEST(IPOS)%MEASURE(J)%COM); YMAX=MAX(YMAX,IPEST(IPOS)%MEASURE(J)%COM)
   ENDDO
  ENDDO
  XMIN=MIN(XMIN,YMIN); XMAX=MIN(XMAX,YMAX); YMIN=XMIN; YMAX=XMAX
 ELSE
  !## fill it in categories
  CALL IPEST_ANALYSE_HISTCLASS(IPOS)
  N=SIZE(HCLASSES)
  XMIN=MINVAL(HCLASSES); XMAX=MAXVAL(HCLASSES)
  YMIN=0.0; YMAX=100.0D0
 ENDIF
 
 IPEST_ANALYSE_DIMGRAPH5=.TRUE.
 
 END FUNCTION IPEST_ANALYSE_DIMGRAPH5
 
 !###=================================== 
 SUBROUTINE IPEST_ANALYSE_HISTCLASS(IPOS)
 !###===================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPOS
 INTEGER :: I,J,N,NDIM
 REAL(KIND=DP_KIND) :: D,XMIN,YMIN,XMAX,YMAX
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: X
 REAL(KIND=DP_KIND),DIMENSION(2) :: PERC
 
 !## define histogram classes (default whenever none given)
 IF(.NOT.ASSOCIATED(HCLASSES))THEN
  
  N=0; DO I=1,SIZE(IPEST); N=N+IPEST(I)%NMEASURE; ENDDO; ALLOCATE(X(N)); X=0.0D0
  !## get an appropriate scale
  N=0; DO I=1,SIZE(IPEST)
   DO J=1,IPEST(I)%NMEASURE
    N=N+1
    X(N)=ABS(IPEST(I)%MEASURE(J)%OBS-IPEST(I)%MEASURE(J)%COM)
   ENDDO
  ENDDO
  CALL UTL_GETMED(X,SIZE(X),-999.0D0,(/10.0D0,90.0D0/),2,N,PERC)
  !## make classes
  XMIN=MINVAL(PERC); XMAX=MAXVAL(PERC)
  XMAX= MAX(ABS(XMIN),ABS(XMAX))
  XMIN=-1.0D0*XMAX
  YMIN=XMIN; YMAX=XMAX
  CALL UTL_GETAXESCALES(XMIN,YMIN,XMAX,YMAX)
  DEALLOCATE(X)
  NDIM=NSX
  ALLOCATE(HCLASSES(NDIM),XCLASSES(NDIM-1))
  DO I=1,NSX; HCLASSES(I)=SXVALUE(I); ENDDO
 ELSE
  NDIM=SIZE(HCLASSES)
 ENDIF

 !## count amount of points per class
 XCLASSES=0.0D0; N=0
 DO J=1,IPEST(IPOS)%NMEASURE
  D=IPEST(IPOS)%MEASURE(J)%OBS-IPEST(IPOS)%MEASURE(J)%COM
  IF(D.LT.HCLASSES(2))THEN
   XCLASSES(1)=XCLASSES(1)+1; N=N+1
  ELSEIF(D.GT.HCLASSES(NDIM-1))THEN
   XCLASSES(NDIM-1)=XCLASSES(NDIM-1)+1; N=N+1
  ELSE
   CALL POL1LOCATE(HCLASSES,SIZE(HCLASSES),D,I)
   !## add to the histogram class
   XCLASSES(I)=XCLASSES(I)+1; N=N+1
  ENDIF
 ENDDO
 
 !## get percentages
 DO I=1,SIZE(XCLASSES); XCLASSES(I)=100.0D0*(XCLASSES(I)/DBLE(N)); ENDDO
 
 END SUBROUTINE IPEST_ANALYSE_HISTCLASS
 
 !###====================================================================
 LOGICAL FUNCTION IPEST_ANALYSE_DIMGRAPH6(XMIN,YMIN,XMAX,YMAX,XTITLE,YTITLE)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(INOUT) :: XMIN,YMIN,XMAX,YMAX 
 CHARACTER(LEN=*),INTENT(OUT) :: XTITLE,YTITLE
 INTEGER :: IPOS,J
 REAL(KIND=DP_KIND) :: DX,DY
 
 IPEST_ANALYSE_DIMGRAPH6=.FALSE.
 
 YTITLE='Y-coordinate (m)'
 XTITLE='X-coordinate (m)'

 CALL WDIALOGGETMENU(IDF_MENU1,IPOS)
 IF(IPEST(IPOS)%NMEASURE.EQ.0)RETURN
 
 XMIN=     HUGE(1.0D0); YMIN=     HUGE(1.0D0)
 XMAX=-1.0*HUGE(1.0D0); YMAX=-1.0*HUGE(1.0D0)
 DO IPOS=1,SIZE(IPEST)
  DO J=1,IPEST(IPOS)%NMEASURE
   XMIN=MIN(XMIN,IPEST(IPOS)%MEASURE(J)%X); XMAX=MAX(XMAX,IPEST(IPOS)%MEASURE(J)%X)
   YMIN=MIN(YMIN,IPEST(IPOS)%MEASURE(J)%Y); YMAX=MAX(YMAX,IPEST(IPOS)%MEASURE(J)%Y)
  ENDDO
 ENDDO
 
 DX=WINFOGRREAL(GRAPHICSAREAMAXX)-WINFOGRREAL(GRAPHICSAREAMINX)
 DY=WINFOGRREAL(GRAPHICSAREAMAXY)-WINFOGRREAL(GRAPHICSAREAMINY)
 
 CALL UTL_IDFCRDCOR(XMIN,XMAX,YMIN,YMAX,DX,DY)
 
 IPEST_ANALYSE_DIMGRAPH6=.TRUE.
 
 END FUNCTION IPEST_ANALYSE_DIMGRAPH6
 
 !###====================================================================
 SUBROUTINE IPEST_ANALYSE_PLOTGRAPH1()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,IPARAM
 
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO8,IPARAM)
 
 CALL IGRCOLOURN(WRGB(255,0,0))
 IF(IPARAM.EQ.1)THEN
  DO I=1,SIZE(IPEST)
   IF(I.EQ.1)THEN
    CALL DBL_IGRMOVETO(REAL(I-1,8),IPEST(I)%J)
   ELSE
    CALL DBL_IGRLINETO(REAL(I-1,8),IPEST(I)%J)
   ENDIF
  ENDDO
 ELSE
  DO I=1,SIZE(IPEST)
   IF(I.EQ.1)THEN
    CALL DBL_IGRMOVETO(REAL(I-1,8),IPEST(I)%T)
   ELSE
    CALL DBL_IGRLINETO(REAL(I-1,8),IPEST(I)%T)
   ENDIF
  ENDDO
 ENDIF

 END SUBROUTINE IPEST_ANALYSE_PLOTGRAPH1
 
 !###====================================================================
 SUBROUTINE IPEST_ANALYSE_PLOTGRAPH2(YMIN,YMAX)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: YMIN,YMAX
 INTEGER :: I,J,K,IPARAM,IPOS,ICHECK,ILABEL
 REAL(KIND=DP_KIND) :: U,L,A,D,XS,DY,X1,Y1
 
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,IPARAM)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,ICHECK)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK3,ILABEL)
 
 IF(IPARAM.EQ.1)THEN
 
  K=0; DO J=1,SIZE(IPEST(1)%ALPHA)
   K=K+1; IF(K.GT.SIZE(ICOLOR))K=1
   CALL IGRCOLOURN(ICOLOR(K))
   DO I=1,SIZE(IPEST)
    IF(I.EQ.1)THEN
     CALL DBL_IGRMOVETO(DBLE(I-1),IPEST(I)%ALPHA(J))
    ELSE
     CALL DBL_IGRLINETO(DBLE(I-1),IPEST(I)%ALPHA(J))
    ENDIF
   ENDDO
  ENDDO
 
 ELSE

  XS=0.45D0
  CALL WDIALOGGETMENU(IDF_MENU1,IPOS)
  
  IF(ICHECK.EQ.0)THEN
   DY=(YMAX-YMIN)/10.0D0
   K=0; DO I=1,SIZE(IPEST(IPOS)%ALPHA)
    K=K+1; IF(K.GT.SIZE(ICOLOR))K=1
    CALL IGRCOLOURN(ICOLOR(K))
    CALL IGRFILLPATTERN(SOLID)
    CALL DBL_IGRRECTANGLE(DBLE(I)-XS,0.0D0,DBLE(I)+XS,IPEST(IPOS)%ALPHA(I))
    CALL IGRCOLOURN(WRGB(0,0,0))
    CALL IGRFILLPATTERN(OUTLINE)
    CALL DBL_IGRRECTANGLE(DBLE(I)-XS,0.0D0,DBLE(I)+XS,IPEST(IPOS)%ALPHA(I))

    IF(ILABEL.EQ.1)THEN
     X1=DBLE(I); Y1=IPEST(IPOS)%ALPHA(I)
     IF(Y1.GT.0.5D0*(YMAX+YMIN))THEN
      CALL DBL_WGRTEXTORIENTATION(ALIGNLEFT,ANGLE=-90.0D0) 
     ELSE
      CALL DBL_WGRTEXTORIENTATION(ALIGNRIGHT,ANGLE=-90.0D0) 
     ENDIF
     CALL DBL_WGRTEXTSTRING(X1,Y1,TRIM(IPEST(1)%CPARAM(I))) 
    ENDIF

   ENDDO
   CALL DBL_WGRTEXTORIENTATION(ALIGNLEFT,ANGLE=0.0D0) 

  ELSE
   
   XS=0.25D0
   
   K=0; DO I=1,SIZE(IPEST(IPOS)%ALPHA)
    K=K+1; IF(K.GT.SIZE(ICOLOR))K=1
    CALL IGRCOLOURN(ICOLOR(K))

    !## get 1*std error
    U=0.0D0; IF(IPEST(IPOS)%UPPER(I).GT.0.0D0)U=LOG(IPEST(IPOS)%UPPER(I))
    L=0.0D0; IF(IPEST(IPOS)%LOWER(I).GT.0.0D0)L=LOG(IPEST(IPOS)%LOWER(I))
    IF(U.EQ.0.0D0.OR.L.EQ.0.0D0)THEN
     CALL IGRCOLOURN(WRGB(0,0,0))
     CALL DBL_IGRJOIN(DBLE(I)-XS,IPEST(IPOS)%ALPHA(I),DBLE(I)+XS,IPEST(IPOS)%ALPHA(I))
    ELSE
     A=LOG(IPEST(IPOS)%ALPHA(I))
    
     !## inner upper border
     D=(U-A)/1.96D0; U=EXP(A+D)

     !## inner lower border
     D=(A-L)/1.96D0; L=EXP(A-D)

     CALL IGRFILLPATTERN(SOLID);   CALL DBL_IGRRECTANGLE(DBLE(I)-XS,L,DBLE(I)+XS,U)
     CALL IGRFILLPATTERN(OUTLINE); CALL DBL_IGRRECTANGLE(DBLE(I)-XS,L,DBLE(I)+XS,U)
     CALL IGRCOLOURN(WRGB(0,0,0))
     CALL DBL_IGRJOIN(DBLE(I)-XS,IPEST(IPOS)%ALPHA(I),DBLE(I)+XS,IPEST(IPOS)%ALPHA(I))
     CALL DBL_IGRJOIN(DBLE(I),IPEST(IPOS)%LOWER(I),DBLE(I),IPEST(IPOS)%UPPER(I))
     CALL DBL_IGRJOIN(DBLE(I)-XS,IPEST(IPOS)%LOWER(I),DBLE(I)+XS,IPEST(IPOS)%LOWER(I))
     CALL DBL_IGRJOIN(DBLE(I)-XS,IPEST(IPOS)%UPPER(I),DBLE(I)+XS,IPEST(IPOS)%UPPER(I))
    ENDIF

   ENDDO
  ENDIF
  
 ENDIF
 
 END SUBROUTINE IPEST_ANALYSE_PLOTGRAPH2

 !###====================================================================
 SUBROUTINE IPEST_ANALYSE_PLOTGRAPH3(YMIN,YMAX)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: YMIN,YMAX
 INTEGER :: I,J,K,IPARAM,IPOS,ILABEL
 REAL(KIND=DP_KIND) :: XS,X1,Y1
 
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IPARAM)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,ILABEL)

 IF(IPARAM.EQ.1)THEN

  K=0; DO J=1,SIZE(IPEST(1)%SENSI)
   K=K+1; IF(K.GT.SIZE(ICOLOR))K=1
   CALL IGRCOLOURN(ICOLOR(K))
   DO I=1,SIZE(IPEST)
    IF(I.EQ.1)THEN
     CALL DBL_IGRMOVETO(DBLE(I-1),IPEST(I)%SENSI(J)) 
    ELSE
     CALL DBL_IGRLINETO(DBLE(I-1),IPEST(I)%SENSI(J))
    ENDIF
   ENDDO
  ENDDO
 
 ELSE
 
  XS=0.45D0
  CALL WDIALOGGETMENU(IDF_MENU1,IPOS)
  K=0; DO I=1,SIZE(IPEST(IPOS)%SENSI)
   K=K+1; IF(K.GT.SIZE(ICOLOR))K=1
   CALL IGRCOLOURN(ICOLOR(K))
   CALL IGRFILLPATTERN(SOLID)
   CALL DBL_IGRRECTANGLE(DBLE(I)-XS,0.0D0,DBLE(I)+XS,IPEST(IPOS)%SENSI(I))
   CALL IGRCOLOURN(WRGB(0,0,0))
   CALL IGRFILLPATTERN(OUTLINE)
   CALL DBL_IGRRECTANGLE(DBLE(I)-XS,0.0D0,DBLE(I)+XS,IPEST(IPOS)%SENSI(I))

   IF(ILABEL.EQ.1)THEN
    X1=DBLE(I); Y1=IPEST(IPOS)%SENSI(I)
    IF(Y1.GT.0.5D0*(YMAX+YMIN))THEN
     CALL DBL_WGRTEXTORIENTATION(ALIGNLEFT,ANGLE=-90.0D0) 
    ELSE
     CALL DBL_WGRTEXTORIENTATION(ALIGNRIGHT,ANGLE=-90.0D0) 
    ENDIF
    CALL DBL_WGRTEXTSTRING(X1,Y1,TRIM(IPEST(1)%CPARAM(I))) 
   ENDIF

  ENDDO
 
 ENDIF
 
 END SUBROUTINE IPEST_ANALYSE_PLOTGRAPH3
 
 !###====================================================================
 SUBROUTINE IPEST_ANALYSE_PLOTGRAPH5(XMIN,YMIN,XMAX,YMAX)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 INTEGER :: IPOS,I,J,IPARAM,N,NPOP,DC1,DC2,HC1,HC2,MC,IC
 REAL(KIND=DP_KIND) :: X1,X2,GOF,X,Y,Y2,AVG,VAR,XT,MINX,MAXX,A,B,R2
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: D,OBS,COM
 REAL(KIND=DP_KIND),DIMENSION(3) :: XMED
 CHARACTER(LEN=256) :: LINE
 
 CALL WDIALOGGETMENU(IDF_MENU1,IPOS)
 IF(IPEST(IPOS)%NMEASURE.EQ.0)RETURN

 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO5,IPARAM)
 !## scatter
 IF(IPARAM.EQ.1)THEN

  CALL IGRCOLOURN(WRGB(0,0,0))
  DO I=1,IPEST(IPOS)%NMEASURE
   CALL DBL_IGRMARKER(IPEST(IPOS)%MEASURE(I)%OBS,IPEST(IPOS)%MEASURE(I)%COM,14)
  ENDDO
  CALL IGRCOLOURN(WRGB(50,50,50)); CALL IGRLINETYPE(DASHED)
  CALL DBL_IGRMOVETO(XMIN,XMIN); CALL DBL_IGRLINETO(XMAX,XMAX)
  CALL IGRLINETYPE(SOLIDLINE)
  
!!! PEST BOOK 5.3.2.1 Goodness of Fit
  DO I=1,2
   N=0
   DO J=1,IPEST(IPOS)%NMEASURE
    N=N+1
    IF(I.EQ.2)THEN
     OBS(N)=IPEST(IPOS)%MEASURE(J)%OBS
     COM(N)=IPEST(IPOS)%MEASURE(J)%COM
    ENDIF
   ENDDO
   IF(I.EQ.1)ALLOCATE(OBS(N),COM(N))
  ENDDO
  
  GOF=UTL_GOODNESS_OF_FIT(OBS,COM,N)
!  GOF=UTL_GOODNESS_OF_FIT(IPEST(IPOS)%MEASURE%OBS,IPEST(IPOS)%MEASURE%COM,IPEST(IPOS)%NMEASURE)
  
  CALL LINREGRESSION(N,OBS,COM,A,B,R2)
!  CALL LINREGRESSION(IPEST(IPOS)%NMEASURE,IPEST(IPOS)%MEASURE%OBS,IPEST(IPOS)%MEASURE%COM,A,B,R2)

  !## insert line of regression
  CALL IGRLINETYPE(DASHED)
  CALL IGRCOLOURN(WRGB(200,0,0))
  CALL DBL_IGRMOVETO(XMIN,XMIN*A+B) 
  CALL DBL_IGRLINETO(XMAX,XMAX*A+B)
  CALL IGRLINETYPE(SOLIDLINE)
  
  LINE='Goodness of Fit '//TRIM(RTOS(GOF*100.0D0,'G',7))//'%'//CHAR(13)//CHAR(10)// &
       'Regression Coefficient'//CHAR(32)//TRIM(RTOS(R2,'G',7))//CHAR(13)//CHAR(10)// &
       'y='//TRIM(RTOS(A,'G',7))//'x '//TRIM(RTOS(B,'G',7)) 
  CALL DBL_WGRTEXTORIENTATION(ALIGNLEFT) 
  CALL IGRCOLOURN(WRGB(0,0,0))
  X= XMIN+0.05D0*(XMAX-XMIN); Y2=YMAX-0.075D0*(YMAX-YMIN)
  X2=XMIN+0.75D0*(XMAX-XMIN);  Y=YMAX-0.65D0*(YMAX-YMIN)
  CALL DBL_WGRTEXTBLOCK(X,Y,X2,Y2,TRIM(LINE),IFLAGS=TBFONTSIZE) 

 !## histogram
 ELSE
 
  !## find switch in classe
  HC1=0; HC2=SIZE(HCLASSES)
  DO I=1,SIZE(HCLASSES)-1
   IF(HCLASSES(I).LT.0.0D0.AND.HCLASSES(I+1).GT.0.0D0)THEN; MC=I;        ENDIF
   IF(HCLASSES(I).GE.0.0D0)                           THEN; HC2=I; EXIT; ENDIF
   IF(HCLASSES(I).LE.0.0D0)                           THEN; HC1=I;       ENDIF
  ENDDO

  IF(HC1.GT.0)DC1=150/HC1
  IF(HC2.GT.0)DC2=150/HC2
  
  DO I=1,SIZE(HCLASSES)-1

   !## lower category
   IF(I.LT.MC)THEN
    IC=100+I*DC1
    CALL IGRCOLOURN(WRGB(0,0,IC))   
   !## higher category
   ELSEIF(I.GT.MC)THEN
    J=I-MC
    IC=250-J*DC2
    CALL IGRCOLOURN(WRGB(IC,0,0))      
   !## mid column
   ELSE
    CALL IGRCOLOURN(WRGB(225,225,225))
   ENDIF
   X1=HCLASSES(I); X2=HCLASSES(I+1)
   CALL IGRFILLPATTERN(SOLID)
   CALL DBL_IGRRECTANGLE(HCLASSES(I),0.0D0,HCLASSES(I+1),XCLASSES(I))
   CALL IGRFILLPATTERN(OUTLINE)
   CALL IGRCOLOURN(WRGB(0,0,0))
   CALL DBL_IGRRECTANGLE(HCLASSES(I),0.0D0,HCLASSES(I+1),XCLASSES(I))

  ENDDO
  CALL IGRFILLPATTERN(OUTLINE)

  !## write statistics
  N=IPEST(IPOS)%NMEASURE; ALLOCATE(D(N)); D=0.0D0
  N=0
  DO I=1,IPEST(IPOS)%NMEASURE
   N=N+1
   D(I)=IPEST(IPOS)%MEASURE(I)%COM-IPEST(IPOS)%MEASURE(I)%OBS 
  ENDDO
  AVG=SUM(D)/DBLE(SIZE(D)); MINX=MINVAL(D); MAXX=MAXVAL(D)
  CALL UTL_GETMED(D,SIZE(D),HUGE(1.0D0),(/10.0D0,50.0D0,90.0D0/),3,N,XMED)
  CALL UTL_STDEF(D,SIZE(D),HUGE(1.0D0),VAR,XT,NPOP)
  DEALLOCATE(D)
  
  X= XMIN+0.05D0*(XMAX-XMIN); Y2=YMAX-0.075D0*(YMAX-YMIN)
  X2=XMIN+0.75D0*(XMAX-XMIN);  Y=YMAX-0.5D0*(YMAX-YMIN)
  LINE='Average:    '//CHAR(32)//TRIM(RTOS(AVG,'G',7))    //CHAR(13)//CHAR(10)// &
       'St.Dev:     '//CHAR(32)//TRIM(RTOS(VAR,'G',7))    //CHAR(13)//CHAR(10)// &
       'Minimal:    '//CHAR(32)//TRIM(RTOS(MINX,'G',7))   //CHAR(13)//CHAR(10)// &
       '10 Percent: '//CHAR(32)//TRIM(RTOS(XMED(1),'G',7))//CHAR(13)//CHAR(10)// &
       '50 Percent: '//CHAR(32)//TRIM(RTOS(XMED(2),'G',7))//CHAR(13)//CHAR(10)// &
       '90 Percent: '//CHAR(32)//TRIM(RTOS(XMED(3),'G',7))//CHAR(13)//CHAR(10)// &
       'Maximal:    '//CHAR(32)//TRIM(RTOS(MAXX,'G',7)) 
  CALL DBL_WGRTEXTORIENTATION(ALIGNLEFT) 
  CALL IGRCOLOURN(WRGB(0,0,0))
  CALL DBL_WGRTEXTBLOCK(X,Y,X2,Y2,TRIM(LINE),IFLAGS=TBFONTSIZE) 

 ENDIF
 
 END SUBROUTINE IPEST_ANALYSE_PLOTGRAPH5

 !###====================================================================
 SUBROUTINE IPEST_ANALYSE_PLOTGRAPH6()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: IPOS,I,ICLR
 REAL(KIND=DP_KIND) :: D
 
 CALL WDIALOGGETMENU(IDF_MENU1,IPOS)
 IF(IPEST(IPOS)%NMEASURE.EQ.0)RETURN
 
 DO I=1,IPEST(IPOS)%NMEASURE
  D=IPEST(IPOS)%MEASURE(I)%COM-IPEST(IPOS)%MEASURE(I)%OBS
  ICLR=UTL_IDFGETCLASS(MP(MXMPLOT)%LEG,D)
  CALL IGRCOLOURN(ICLR)
  CALL DBL_IGRMARKER(IPEST(IPOS)%MEASURE(I)%X,IPEST(IPOS)%MEASURE(I)%Y,14)
 ENDDO

 END SUBROUTINE IPEST_ANALYSE_PLOTGRAPH6
 
 !###====================================================================
 SUBROUTINE IPEST_ANALLYSE_PLOTIPOS(IGRAPH,XMIN,YMIN,YMAX)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IGRAPH
 REAL(KIND=DP_KIND),INTENT(IN) :: XMIN,YMIN,YMAX
 INTEGER :: IPOS,IPARAM
 
 !## no plotting
 IF(IGRAPH.EQ.5)RETURN
 IF(IGRAPH.EQ.6)RETURN

 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,IPARAM)
 IF(IGRAPH.EQ.2.AND.IPARAM.EQ.2)RETURN

 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IPARAM)
 IF(IGRAPH.EQ.3.AND.IPARAM.EQ.2)RETURN
 
 !## get trackbar
 CALL WDIALOGGETMENU(IDF_MENU1,IPOS); IPOS=IPOS-1

 CALL IGRCOLOURN(WRGB(0,0,0))
 CALL DBL_IGRMOVETO(DBLE(IPOS),YMIN)
 CALL DBL_IGRLINETO(DBLE(IPOS),YMAX)
 CALL DBL_IGRLINETO(XMIN,YMAX)
 
 END SUBROUTINE IPEST_ANALLYSE_PLOTIPOS
 
 !###====================================================================
 SUBROUTINE IPEST_ANALYSE_PLOT_AXES(XMIN,YMIN,XMAX,YMAX,LDATE,XTITLE,YTITLE)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: XTITLE,YTITLE
 REAL(KIND=DP_KIND),INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 LOGICAL,INTENT(IN) :: LDATE
 
 AXES%XMIN  =XMIN
 AXES%XMAX  =XMAX
 IF(AXES%XMAX.LE.AXES%XMIN)THEN
  AXES%XMIN=AXES%XMIN-1.0D0
  AXES%XMAX=AXES%XMAX+1.0D0
 ENDIF
 AXES%YMIN  =YMIN
 AXES%YMAX  =YMAX
 IF(AXES%YMAX.LE.AXES%YMIN)THEN
  AXES%YMIN=AXES%YMIN-1.0D0
  AXES%YMAX=AXES%YMAX+1.0D0
 ENDIF
 AXES%IFIXX =0 
 AXES%IFIXY =0 
 AXES%IFIXY2=0 
 AXES%XINT  =1.0D0 
 AXES%YINT  =1.0D0 
 AXES%XOFFSET=0 
 AXES%LDATE  =LDATE
 AXES%XTITLE=TRIM(XTITLE)
 AXES%YTITLE=TRIM(YTITLE)
 AXES%IAXES=(/1,0/)       !## left/bottom axes only
 AXES%XFACTOR=1.0D0
 AXES%YFACTOR=1.0D0
 AXES%DXAXESL=40.0D0  !## 1/40.0D0 als rand
 AXES%DYAXESB=20.0D0
 AXES%DYAXEST=75.0D0
 AXES%DXAXESR=150.0D0
 AXES%TFONT=FFHELVETICA   !## text-font
 AXES%ICLRRASTER=WRGB(220,220,220)
 
 AXES%ICLRBACKGROUND=WRGB(123,152,168)

 !## plot axes and set units
 CALL GRAPH_PLOTAXES(AXES,1)

 END SUBROUTINE IPEST_ANALYSE_PLOT_AXES 
 
 !###======================================================================
 SUBROUTINE IPEST_ANALYSE_ALLOCATE(NI,NP)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NI,NP
 INTEGER :: I
 
 IF(NP.EQ.0)THEN
 
  CALL IPEST_ANALYSE_DEALLOCATE(); ALLOCATE(IPEST(NI)); IPEST%J=0.0D0
 
  ALLOCATE(GRAPHUNITS(6,1),GRAPHAREA(4,1))
  GRAPHUNITS(1,1)=0.0D0; GRAPHUNITS(2,1)=0.0D0
  GRAPHUNITS(3,1)=1.0D0; GRAPHUNITS(4,1)=1.0D0
  GRAPHUNITS(5,1)=0.0D0; GRAPHUNITS(6,1)=1.0D0
  GRAPHAREA(1,1) =0.0D0; GRAPHAREA(2,1) =0.0D0
  GRAPHAREA(3,1) =1.0D0; GRAPHAREA(4,1) =1.0D0

 ELSE

  DO I=1,SIZE(IPEST); ALLOCATE(IPEST(I)%ALPHA(NP));  IPEST(I)%ALPHA=0.0D0; ENDDO
  DO I=1,SIZE(IPEST); ALLOCATE(IPEST(I)%UPPER(NP));  IPEST(I)%UPPER=0.0D0; ENDDO
  DO I=1,SIZE(IPEST); ALLOCATE(IPEST(I)%LOWER(NP));  IPEST(I)%LOWER=0.0D0; ENDDO
  DO I=1,SIZE(IPEST); ALLOCATE(IPEST(I)%CPARAM(NP)); IPEST(I)%CPARAM=''; ENDDO
  DO I=1,SIZE(IPEST); ALLOCATE(IPEST(I)%SENSI(NP));  IPEST(I)%SENSI=0.0D0; ENDDO
  DO I=1,SIZE(IPEST);                                IPEST(I)%NMEASURE=0; ENDDO
  
  ALLOCATE(PARAM(NP))
  
 ENDIF
 
 END SUBROUTINE IPEST_ANALYSE_ALLOCATE

 !###======================================================================
 SUBROUTINE IPEST_ANALYSE_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 IF(ALLOCATED(IPEST))THEN
  DO I=1,SIZE(IPEST)
   IF(ASSOCIATED(IPEST(I)%ALPHA))  DEALLOCATE(IPEST(I)%ALPHA) 
   IF(ASSOCIATED(IPEST(I)%UPPER))  DEALLOCATE(IPEST(I)%UPPER) 
   IF(ASSOCIATED(IPEST(I)%LOWER))  DEALLOCATE(IPEST(I)%LOWER) 
   IF(ASSOCIATED(IPEST(I)%CPARAM)) DEALLOCATE(IPEST(I)%CPARAM) 
   IF(ASSOCIATED(IPEST(I)%SENSI))  DEALLOCATE(IPEST(I)%SENSI) 
   IF(ASSOCIATED(IPEST(I)%MEASURE))DEALLOCATE(IPEST(I)%MEASURE)
  ENDDO
  DEALLOCATE(IPEST)
 ENDIF
 IF(ALLOCATED(PARAM))DEALLOCATE(PARAM)
 
 IF(ALLOCATED(GRAPHUNITS))DEALLOCATE(GRAPHUNITS)
 IF(ALLOCATED(GRAPHAREA))DEALLOCATE(GRAPHAREA)
 IF(ASSOCIATED(HCLASSES))DEALLOCATE(HCLASSES)
 IF(ASSOCIATED(XCLASSES))DEALLOCATE(XCLASSES)
 IF(ALLOCATED(CPERIOD))DEALLOCATE(CPERIOD)
 IF(ALLOCATED(CLAY))DEALLOCATE(CLAY)
 IF(ALLOCATED(IPERIOD))DEALLOCATE(IPERIOD)
 IF(ALLOCATED(ILAY))DEALLOCATE(ILAY)
 IF(ALLOCATED(LPERIOD))DEALLOCATE(LPERIOD)
 IF(ALLOCATED(LLAY))DEALLOCATE(LLAY)
 
 END SUBROUTINE IPEST_ANALYSE_DEALLOCATE

 !###======================================================================
 SUBROUTINE IPEST_ANALYSE_CLOSE()
 !###======================================================================
 IMPLICIT NONE
 
 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_IPESTANALYSER,2,0)
 CALL IPEST_ANALYSE_DEALLOCATE() 
 CALL WDIALOGSELECT(ID_DIPESTANALYSE); CALL WDIALOGUNLOAD()

 END SUBROUTINE IPEST_ANALYSE_CLOSE
 
 !###======================================================================
 LOGICAL FUNCTION IPEST_ANALYSE_QUERY(JLAY,JDATE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: JLAY
 INTEGER(KIND=DP_KIND),INTENT(IN) :: JDATE
 INTEGER :: I
 
 IPEST_ANALYSE_QUERY=.FALSE.
 
 DO I=1,SIZE(LLAY)
  IF(JLAY.EQ.LLAY(I))EXIT
 ENDDO
 IF(I.GT.SIZE(LLAY))RETURN
 
 DO I=1,SIZE(LPERIOD)
  IF(JDATE.EQ.LPERIOD(I))EXIT
 ENDDO
 IF(I.GT.SIZE(LPERIOD))RETURN

 IPEST_ANALYSE_QUERY=.TRUE.
 
 END FUNCTION IPEST_ANALYSE_QUERY
 
 !###======================================================================
 SUBROUTINE IPEST_ANALYSE_SETTINGS() 
 !###======================================================================
 IMPLICIT NONE 
 INTEGER :: ITYPE,DID
 TYPE(WIN_MESSAGE) :: MESSAGE
 
 DID=WINFODIALOG(CURRENTDIALOG)
 
 !## fill in dialog
 CALL WDIALOGLOAD(ID_DIPESTANALYSE_SETTINGS,ID_DIPESTANALYSE_SETTINGS)

 CALL WDIALOGPUTMENU(IDF_MENU1,CPERIOD,SIZE(CPERIOD),IPERIOD)
 CALL WDIALOGPUTMENU(IDF_MENU2,CLAY   ,SIZE(CLAY)   ,ILAY)

 CALL WDIALOGSHOW(-1,-1,0,2)
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDCANCEL)
      EXIT
     CASE (IDHELP)
     CASE (IDOK)
      EXIT
    END SELECT
   CASE (FIELDCHANGED)
  END SELECT
 ENDDO

 IF(DID.NE.0)CALL WDIALOGSELECT(DID)
 
 END SUBROUTINE IPEST_ANALYSE_SETTINGS
 
END MODULE MOD_IPEST_ANALYSER
