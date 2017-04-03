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
!!
MODULE MOD_WBAL_ANALYSE

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_WBAL_PAR
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_CAP,UTL_IDATETOJDATE,UTL_JDATETOIDATE,NV,NL,ITOS,IDATETOGDATE,UTL_GDATE,UTL_GETUNIQUE_CHAR, &
            UTL_WSELECTFILE
 !,RTOS,ITOS,JD,UTL_SUBST,,UTL_CLOSEUNITS, &
!     UTL_INSIDEPOLYGON,UTL_CREATEDIR,UTL_DIRINFO_POINTER,JDATETOFDATE,, &
!     NV,NL,MAXLEN,UTL_MESSAGEHANDLE,
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_PROFILE_UTL, ONLY : GRAPH,GRAPHNAMES,PROFILE_DEALLGRAPH,PROFILE_ALLGRAPH,PROFILE_PLOTGRAPH

!integer,parameter :: ID_WBAL_ANALYSE=999

CONTAINS

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_MAIN(ITYPE,MESSAGE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 
 CALL WDIALOGSELECT(MESSAGE%WIN) 
  
 SELECT CASE (ITYPE)
 
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDCANCEL)
     CALL WBAL_ANALYSE_CLOSE()
    CASE (IDHELP)
    !## save configuration
    CASE (IDSAVE)
   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT
 END SELECT
     
 END SUBROUTINE WBAL_ANALYSE_MAIN 

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_TAB1(ITYPE,MESSAGE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER :: I
 
 CALL WDIALOGSELECT(MESSAGE%WIN)
 
 SELECT CASE (ITYPE)
 
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    !## open configuration
    CASE (ID_OPEN1)
!     CALL WBAL_ANALYSE_MAIN_INI(MESSAGE%VALUE1)

    !## open csv
    CASE (ID_OPEN2)
     CALL WBAL_ANALYSE_READCSV()
   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
    CASE (IDF_RADIO1,IDF_RADIO2)
     CALL WDIALOGGETRADIOBUTTON(MESSAGE%VALUE2,I)
     IF(I.EQ.1)THEN
      CALL WDIALOGFIELDSTATE(ID_OPEN1,1)
      CALL WDIALOGFIELDSTATE(ID_OPEN2,0)
     ELSE
      CALL WDIALOGFIELDSTATE(ID_OPEN1,0)
      CALL WDIALOGFIELDSTATE(ID_OPEN2,1)
     ENDIF
   END SELECT
 END SELECT
     
 END SUBROUTINE WBAL_ANALYSE_TAB1

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_TAB2(ITYPE,MESSAGE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE

 CALL WDIALOGSELECT(MESSAGE%WIN)
 
 SELECT CASE (ITYPE)
 
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDCANCEL)
    CASE (IDHELP)
    CASE (IDOK)
   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT
 END SELECT
     
 END SUBROUTINE WBAL_ANALYSE_TAB2

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_TAB3(ITYPE,MESSAGE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE

 CALL WDIALOGSELECT(MESSAGE%WIN)
 
 SELECT CASE (ITYPE)
 
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDCANCEL)
    CASE (IDHELP)
    CASE (IDOK)
   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT
 END SELECT
     
 END SUBROUTINE WBAL_ANALYSE_TAB3

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_TAB4(ITYPE,MESSAGE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE

 CALL WDIALOGSELECT(MESSAGE%WIN)
 
 SELECT CASE (ITYPE)
 
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDCANCEL)
    CASE (IDHELP)
    CASE (IDOK)

!     CALL WBAL_ANALYSE_TIMESERIES()

   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT
 END SELECT
     
 END SUBROUTINE WBAL_ANALYSE_TAB4

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_TIMESERIES() 
 !###======================================================================
 IMPLICIT NONE

!     IY1=19630101
!     IY2=20140101
!     IPERIOD=1
!     ITIME=0
!     CALL WDIALOGGETMENU(IDF_MENU1,IBLANK)
!     CALL WBAL_GRAPHPREPARE(IBLANK,ITIME,IPERIOD,IY1,IY2) 
!     IF(WBAL_GRAPHPLOT(NLAY,NZONE))THEN; ENDIF
!     DEALLOCATE(GWBAL(2)%CDATE,GWBAL(2)%Q,GWBAL(2)%CLAY,GWBAL(2)%CZONE)
!     CALL WDIALOGSELECT(ID_WBALMAIN)

 END SUBROUTINE WBAL_ANALYSE_TIMESERIES

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_READCSV() 
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=512) :: LINE
 CHARACTER(LEN=256) :: CSVFNAME
 CHARACTER(LEN=52) :: TXT
 INTEGER :: I,J,IU,NLAY,NZONE,NDATE,IOS,SKIPLINES,CFN_N_ELEM

 IF(.NOT.UTL_WSELECTFILE('Load Comma Separated File (*.csv)|*.csv|',&
                  LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,CSVFNAME,&
                  'Load Comma Separated File (*.csv)'))RETURN

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=CSVFNAME,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot OPEN/READ file called:'//CHAR(13)//&
    TRIM(CSVFNAME),'Error')
  RETURN
 ENDIF

 !## define size of header
 SKIPLINES=0; DO 
  READ(IU,'(A512)') LINE; IF(UTL_CAP(LINE(1:4),'U').EQ.'DATE')EXIT; SKIPLINES=SKIPLINES+1
 ENDDO

 !## number of budgetterms
 NV=CFN_N_ELEM(',;',2,LINE); NV=NV-3
 
 !## allocate object, gwbal(1) is original gwbal(2) is selection to be processed as plot/graph
 ALLOCATE(GWBAL(2))

 !## read labels of budgetterms
 ALLOCATE(GWBAL(1)%TXT(NV),GWBAL(2)%TXT(NV))
 READ(LINE,*) TXT,TXT,TXT,(GWBAL(1)%TXT(I),I=1,NV)
 DO I=1,NV; GWBAL(2)%TXT(I)=GWBAL(1)%TXT(I); ENDDO
 
 !## read to end of file
 NL=-1
 DO
  READ(IU,*,IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  NL=NL+1
 ENDDO
 
 !## allocate memory
 ALLOCATE(GWBAL(1)%CDATE(NL),GWBAL(1)%Q(NV,NL),GWBAL(1)%CLAY(NL),GWBAL(1)%CZONE(NL)) !,IBLANK(NV))

 REWIND(IU)
 
 !## skip header
 DO I=1,SKIPLINES+2; READ(IU,'(A)') LINE; ENDDO
 
 !## read data entire data from csv
 DO I=1,NL
  READ(IU,*) GWBAL(1)%CDATE(I),GWBAL(1)%CLAY(I),GWBAL(1)%CZONE(I),(GWBAL(1)%Q(J,I),J=1,NV)
 ENDDO
 CLOSE(IU)
 
 ALLOCATE(CILAY(NL),CIZONE(NL),CIDATE(NL))
 CILAY =GWBAL(1)%CLAY
 CIZONE=GWBAL(1)%CZONE
 CIDATE=GWBAL(1)%CDATE

 !## find how many dates
 CALL UTL_GETUNIQUE_CHAR(CIDATE,NL,NDATE)
 !## find how many layers
 CALL UTL_GETUNIQUE_CHAR(CILAY,NL,NLAY)
 !## find how many zones
 CALL UTL_GETUNIQUE_CHAR(CIZONE,NL,NZONE)
 
 !## print summary of csv on tab1
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,NV)    !## unique budget terms
 CALL WDIALOGPUTINTEGER(IDF_INTEGER2,NDATE) !## unique period
 CALL WDIALOGPUTINTEGER(IDF_INTEGER3,NLAY)  !## unique layers
 CALL WDIALOGPUTINTEGER(IDF_INTEGER4,NZONE) !## unique zones
 
 !# allocate menufield list indices
 ALLOCATE(LIDATE(NDATE),LIZONE(NZONE),LILAY(NLAY))
 !## none selected
 LIDATE=0; LIZONE=0; LILAY=0

 !## fill in in menu fields
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB1)

 !## fill number of dates
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB2)
 CALL WDIALOGPUTMENU(IDF_MENU1,CIDATE,NDATE,LIDATE)
 !## fill number of layers/zones
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB3)
 CALL WDIALOGPUTMENU(IDF_MENU1,CILAY,NLAY,LILAY)
 CALL WDIALOGPUTMENU(IDF_MENU2,CIZONE,NZONE,LIZONE)

 !## outgrey tabstates
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DWBAL_ANALYSE_TAB2,1)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DWBAL_ANALYSE_TAB3,1)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DWBAL_ANALYSE_TAB4,1)

 END SUBROUTINE WBAL_ANALYSE_READCSV

 !###======================================================================
 SUBROUTINE WBAL_GRAPHPREPARE(IBLANK,ITIME,IPERIOD,IY1,IY2) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITIME,IPERIOD
 INTEGER,INTENT(INOUT) :: IY1,IY2 
 INTEGER,INTENT(IN),DIMENSION(NV) :: IBLANK
! INTEGER :: I,J,K,JJ,NLAY,NZONE,NLL,JD1,JD2,JDP,IM1,IM2,ID1,ID2
! REAL :: A
 
! SELECT CASE (ITIME)
!  !## as is
!  CASE (0)
!   ALLOCATE(GWBAL(2)%CDATE(NL),GWBAL(2)%Q(NV,NL),GWBAL(2)%CLAY(NL),GWBAL(2)%CZONE(NL))
!   DO I=1,NL; GWBAL(2)%CDATE(I)=GWBAL(1)%CDATE(I); ENDDO
!   DO I=1,NL; GWBAL(2)%CLAY(I) =GWBAL(1)%CLAY(I); ENDDO
!   DO I=1,NL; GWBAL(2)%CZONE(I)=GWBAL(1)%CZONE(I); ENDDO
!   DO I=1,NL; DO J=1,NV; GWBAL(2)%Q(J,I)=GWBAL(1)%Q(J,I); ENDDO; ENDDO
!   NLL=NL
!  !## total per period (m3/day)
!  CASE (1)
!
!   JD1=UTL_IDATETOJDATE(IY1)
!   JD2=UTL_IDATETOJDATE(IY2)
!   JDP=0
!
!   !## number of periods
!   SELECT CASE (IPERIOD)
!    CASE (1)  !## year
!     CALL UTL_GDATE(JD1,IY1,IM1,ID1)
!     CALL UTL_GDATE(JD2,IY2,IM2,ID2)
!     NLL=IY2-IY1+1
!    CASE (2)  !## month
!     CALL UTL_GDATE(JD1,IY1,IM1,ID1)
!     CALL UTL_GDATE(JD2,IY2,IM2,ID2)
!     NLL=(IY2-IY1+1)*12
!   END SELECT
!
!   NLL=NLL*NZONE*NLAY    
!   ALLOCATE(GWBAL(2)%CDATE(NLL),GWBAL(2)%Q(NV,NLL),GWBAL(2)%CLAY(NLL),GWBAL(2)%CZONE(NLL))
!   GWBAL(2)%CDATE=0; GWBAL(2)%Q=0.0; GWBAL(2)%CLAY=0; GWBAL(2)%CZONE=0
!                        
!   I=0
!   K=0
!   DO 
!    I=I+1
!    IF(I.GT.SIZE(GWBAL(1)%CDATE))EXIT
!     
!    !## get year - julian dates !!!
!    J=UTL_IDATETOJDATE(GWBAL(1)%CDATE(I))
!
!    IF(J.LT.JD1)CYCLE    !## searching for startdate
!    IF(J.GT.JD2)EXIT     !## finished
!    IF(J.GT.JDP)THEN     !## goto the next period
!
!     IF(JDP.GT.0)THEN
!      JD1=JDP
!      K=K+NLAY*NZONE
!     ENDIF
!      
!     CALL IDATETOGDATE(GWBAL(1)%CDATE(I),IY1,IM1,ID1)
!     SELECT CASE (IPERIOD)
!      CASE (1)  !## year
!       JDP=JD1+365; IF(WDATELEAPYEAR(IY1))JDP=JDP+1
!      CASE (2)  !## month
!       JDP=JD1+WDATEDAYSINMONTH(IY1,IM1)-1
!     END SELECT
!
!    ENDIF
!
!    !## sum for all ilay/izone
!    I=I-1
!    DO J=1,NLAY*NZONE
!     I=I+1
!
!     GWBAL(2)%CDATE(K+J)=UTL_JDATETOIDATE(JD1)
!     GWBAL(2)%CLAY(K+J) =GWBAL(1)%CLAY(I)
!     GWBAL(2)%CZONE(K+J)=GWBAL(1)%CZONE(I)
!
!     DO JJ=1,NV
!      GWBAL(2)%Q(JJ,K+J)=GWBAL(2)%Q(JJ,K+J)+GWBAL(1)%Q(JJ,I)
!     ENDDO
!    ENDDO
!     
!   ENDDO   
! END SELECT
!
! !## remove topics if needed
! DO J=1,NV
!  IF(IBLANK(J).EQ.0)THEN
!   DO I=1,NLL; GWBAL(2)%Q(J,I)=0.0; ENDDO
!  ENDIF
!  IF(.FALSE.)THEN
!   A=121647.0*250.0*250.0
!   A=1000.0/A
!  ELSE
!   A=1.0
!  ENDIF
!  DO I=1,NLL; GWBAL(2)%Q(J,I)=GWBAL(2)%Q(J,I)*A; ENDDO
! ENDDO
!
!! !## sum 1-4 and 5-8
!! DO I=1,NLL
! ! DO J=1,4
! !  GWBAL(2)%Q(10,I)=GWBAL(2)%Q(10,I)+GWBAL(2)%Q(J,I)
! ! ENDDO
! ! DO J=6,8
! !  GWBAL(2)%Q(11,I)=GWBAL(2)%Q(11,I)+GWBAL(2)%Q(J,I)
! ! ENDDO
! !ENDDO
! 
 END SUBROUTINE WBAL_GRAPHPREPARE

 !###======================================================================
 LOGICAL FUNCTION WBAL_GRAPHPLOT(NLAY,NZONE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NLAY,NZONE
 INTEGER,PARAMETER :: MXFLUX=11
 INTEGER :: NG,NP,NB,I,J,K,II,JJ,III,KK,KKK,IDATE,IX,IPOS,ITYPE,NL
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: IBAR
 REAL :: X
 REAL,ALLOCATABLE,DIMENSION(:,:,:) :: XG
 INTEGER,DIMENSION(0:MXFLUX) :: ICOLOR
 CHARACTER(LEN=20),DIMENSION(MXFLUX) :: CFLUX
 DATA CFLUX/'CONSTANT HEAD','FLUX LOWER FACE','FLUX UPPER FACE','FLUX FRONT FACE','FLUX RIGHT FACE','STORAGE', &
            'EVAPOTRANSPIRATION','OVERLAND FLOW','RECHARGE','SEGMENTS','WELLS'/

 WBAL_GRAPHPLOT=.FALSE.

 !## positive
 ICOLOR(0) =WRGB(225,225,225)! -- unknown grey
 ICOLOR(1) =WRGB(0  ,0  ,64 ) !## constant head
 ICOLOR(2) =WRGB(0  ,0  ,255) !## flf
 ICOLOR(3) =WRGB(128,128,255) !## fuf
 ICOLOR(4) =WRGB(0  ,128,64 ) !## fff
 ICOLOR(5) =WRGB(0  ,255,0  ) !## frf
 ICOLOR(6) =WRGB(0  ,255,255) !## sto
 ICOLOR(7) =WRGB(128,255,128) !## evt
 ICOLOR(8) =WRGB(255,0  ,0  ) !## olf
 ICOLOR(9) =WRGB(255,128,64 ) !## rch
 ICOLOR(10)=WRGB(128,0  ,0  ) !## isg
 ICOLOR(11)=WRGB(255,255,0  ) !## wel
! ICOLOR(12)=WRGB(255,255,128) 
! ICOLOR(13)=WRGB(255,128,255) 
! ICOLOR(14)=WRGB(128,0  ,255) 

 IF(ALLOCATED(GRAPH))CALL PROFILE_DEALLGRAPH()
 
 !## if four columns per category
 !ITYPE=2
 !## if two columns per category
 ITYPE=1
 
 !## number of graphs
 NL= SIZE(GWBAL(2)%CDATE)
 NG= NZONE*NLAY      !## number of graphs
 NP=(NL/NG)          !## number of points
 NB=(NV)/(2*ITYPE)   !## number of bars

 !## select bars to be plotted
 ALLOCATE(IBAR(NB,2)); IBAR=0
 J=0; K=1
 DO I=1,NB
  J=J+1
  IBAR(J,1)=K 
  IBAR(J,2)=IBAR(J,1)+1
  K=K+2*ITYPE 
 ENDDO

 !## check whether these are all non-zero per category
 DO I=1,NB/ITYPE 
  DO J=1,2
   II=IBAR(I,J)
   IX=0  
   DO K=1,NP*NG
    X=GWBAL(2)%Q(II,K)
    IF(X.NE.0.0)IX=IX+1
   ENDDO
   IF(IX.EQ.0)IBAR(I,J)=-IBAR(I,J)
  ENDDO
 ENDDO

 IX=0; DO I=1,NB/ITYPE; DO J=1,2; IF(IBAR(I,J).GT.0)IX=IX+1; ENDDO; ENDDO; NB=IX
 
 CALL PROFILE_ALLGRAPH(NB,NG)

 DO I=1,NB
  DO J=1,NG
   ALLOCATE(GRAPH(I,J)%RX(NP),GRAPH(I,J)%RY(NP))
  ENDDO
 ENDDO
 
 K=0
 DO I=1,NLAY
  DO J=1,NZONE
   K=K+1
   GRAPHNAMES(K)='Layer '//TRIM(GWBAL(2)%CLAY(I))//'; Zone Number '//TRIM(GWBAL(2)%CZONE(J))
  ENDDO
 ENDDO

 !## keep record of minimal and maximal values
 ALLOCATE(XG(NP,NG,2)); XG=0.0

 !## fill in legend
 !## read each group
 DO J=1,NG
  III=0
  !## read in/out volumes
  DO JJ=1,2
   !## read each active bar
   DO I=1,SIZE(IBAR,1)
    !## skip empty bars
    II=IBAR(I,JJ); IF(II.LE.0)CYCLE
    III=III+1
    GRAPH(III,J)%NP=NP
    GRAPH(III,J)%GTYPE=1
    GRAPH(III,J)%LEGTXT=UTL_CAP(GWBAL(2)%TXT(II),'U')

    !## get color
    DO IPOS=1,MXFLUX
     IF(INDEX(GRAPH(III,J)%LEGTXT,TRIM(CFLUX(IPOS))//'_IN').GT.0)EXIT
     IF(INDEX(GRAPH(III,J)%LEGTXT,TRIM(CFLUX(IPOS))//'_OUT').GT.0)EXIT
    ENDDO
    IF(IPOS.GT.MXFLUX)IPOS=0
    GRAPH(III,J)%ICLR=ICOLOR(IPOS)
   ENDDO
  ENDDO
 ENDDO

 !## read each timestep
 KK=0
 DO K=1,NP
  !## read each group
  DO J=1,NG
   III=0
   KK=KK+1
   !## read in/out volumes
   DO JJ=1,2
    !## read each active bar
    DO I=1,SIZE(IBAR,1)
     !## skip empty bars
     II=IBAR(I,JJ); IF(II.LE.0)CYCLE
     III=III+1

     !## always first column
     READ(GWBAL(2)%CDATE(KK),*) IDATE
     GRAPH(III,J)%RX(K)=REAL(UTL_IDATETOJDATE(IDATE))
     !## get balance value
     X=GWBAL(2)%Q(II,KK)
     KKK=1; IF(X.LT.0.0)KKK=2
     XG(K,J,KKK)=XG(K,J,KKK)+X
     X=XG(K,J,KKK)
     GRAPH(III,J)%RY(K)=X

    ENDDO

   ENDDO
  ENDDO
 ENDDO
  
! CALL WINDOWOPEN(FLAGS=SYSMENUON+HIDEWINDOW+STATUSBAR)
! CALL WINDOWSTATUSBARPARTS(4,(/2000,2000,750,-1/),(/1,1,1,1/))
! CALL IGRCOLOURMODEL(24)
! CALL IMODINITMESSAGE()
! CALL UTL_MESSAGEHANDLE(1)
 
 !## plot graph(s)
! CALL PROFILE_PLOTGRAPH('Time','Volumes (mm/d)',.TRUE.)
 CALL PROFILE_PLOTGRAPH('Time','Volumes (m3/d)',.TRUE.)
 
 !## clean up, deallocate
 IF(ALLOCATED(GRAPH))CALL PROFILE_DEALLGRAPH()

 WBAL_GRAPHPLOT=.TRUE.

 END FUNCTION WBAL_GRAPHPLOT

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_INIT() 
 !###======================================================================
 IMPLICIT NONE 

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_WBAL_ANALYSE,2).EQ.1)THEN
  CALL WBAL_ANALYSE_CLOSE()
  RETURN
 ENDIF

 CALL MAIN1INACTMODULE(ID_WBAL_ANALYSE)

 !## other module no closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_WBAL_ANALYSE,2,1)

 !## fill in dialog
 CALL WDIALOGLOAD(ID_DWBAL_ANALYSE,ID_DWBAL_ANALYSE)
! CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN,1)
! CALL WDIALOGPUTIMAGE(ID_SAVEAS,ID_ICONSAVEAS,1)

 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE_TAB1)
 CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONOPEN,1)
 CALL WDIALOGPUTIMAGE(ID_OPEN2,ID_ICONOPEN,1)
 
 !## outgrey tabs
 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DWBAL_ANALYSE_TAB2,0)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DWBAL_ANALYSE_TAB3,0)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DWBAL_ANALYSE_TAB4,0)

 CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE WBAL_ANALYSE_INIT

 !###======================================================================
 SUBROUTINE WBAL_ANALYSE_CLOSE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_WBAL_ANALYSE,2,0)

 IF(ALLOCATED(GWBAL))THEN
  DO I=1,SIZE(GWBAL)
   IF(ASSOCIATED(GWBAL(I)%CDATE))DEALLOCATE(GWBAL(I)%CDATE)
   IF(ASSOCIATED(GWBAL(I)%CLAY)) DEALLOCATE(GWBAL(I)%CLAY)
   IF(ASSOCIATED(GWBAL(I)%CZONE))DEALLOCATE(GWBAL(I)%CZONE)
   IF(ASSOCIATED(GWBAL(I)%Q))    DEALLOCATE(GWBAL(I)%Q)
  ENDDO
  DEALLOCATE(GWBAL)
 ENDIF
 
 IF(ALLOCATED(CILAY)) DEALLOCATE(CILAY)
 IF(ALLOCATED(CIZONE))DEALLOCATE(CIZONE)
 IF(ALLOCATED(CIDATE))DEALLOCATE(CIDATE)
 IF(ALLOCATED(LILAY)) DEALLOCATE(LILAY)
 IF(ALLOCATED(LIZONE))DEALLOCATE(LIZONE)
 IF(ALLOCATED(LIDATE))DEALLOCATE(LIDATE)

 CALL WDIALOGSELECT(ID_DWBAL_ANALYSE)
 CALL WDIALOGUNLOAD()

 END SUBROUTINE WBAL_ANALYSE_CLOSE

END MODULE MOD_WBAL_ANALYSE