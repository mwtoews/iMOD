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
USE MODPLOT, ONLY : MPW
USE WINTERACTER
USE MOD_IPEST_PAR
USE RESOURCE
USE MOD_UTL, ONLY : UTL_GETUNIT
USE MOD_DBL
USE MOD_GRAPH, ONLY : GRAPH_PLOTAXES,AXESOBJ,GRAPHUNITS,GRAPHAREA

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
     CALL IPEST_ANALYSE_READLOG('d:\COMPILE\OSSDELTARES\iMOD\TUTORIALS\IMOD_USER\MODELS\TUT_PST_OPTIMIZE\pest')
     CALL IPEST_ANALYSE_PLOTGRAPH()
    CASE (IDHELP)
   END SELECT
   
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
   SELECT CASE (MESSAGE%VALUE2)
    CASE (IDF_TRACKBAR1)
     CALL IPEST_ANALYSE_PLOTGRAPH()
   END SELECT
  CASE (RESIZE,EXPOSE)
   CALL IPEST_ANALYSE_PLOTGRAPH()
  
 END SELECT
     
 END SUBROUTINE IPEST_ANALYSE_MAIN 
 
 !###======================================================================
 SUBROUTINE IPEST_ANALYSE_READLOG(DIR) 
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: I,J,IU,NI,NP,IOS,N
 CHARACTER(LEN=256) :: LINE
 CHARACTER(LEN=52) :: TXT
  
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
 NP=0
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
  IF(INDEX(TRIM(LINE),'Parameters').GT.0)THEN
   READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
   DO
    READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
    IF(TRIM(LINE).EQ.'')EXIT
    NP=NP+1
   ENDDO
   EXIT
  ENDIF
 ENDDO

 CALL IPEST_ANALYSE_ALLOCATE(0,NP)

 !## after '*** Next Outer Iteration ***' komen statistieken
 
 !## continue reading
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
  IF(INDEX(LINE,'Upgrade Vector Parameter History:').GT.0)THEN
   READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
   
   !## how many iterations
   N=0; DO I=0,NI
    WRITE(TXT,'(A4,I3.3)') 'ITER',I
    IF(INDEX(LINE,TRIM(TXT)).GT.0)N=N+1
   ENDDO
   DO I=1,NP
    READ(IU,'(4X,A15,99(F10.0))') IPEST(1)%CPARAM(I),(IPEST(J)%ALPHA(I),J=N,1,-1)
   ENDDO

  ENDIF   
 ENDDO
 
 !IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(DIR)//'\log_pest_runfile_mf2005.txt',STATUS='OLD',ACTION='READ')
 !DO I=1,4; READ(IU,*,IOSTAT=IOS); IF(IOS.NE.0)THEN; CLOSE(IU); RETURN; ENDIF; ENDDO
 !NP=0; DO; READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT; NP=NP+1; ENDDO
 !
 !CALL IPEST_ANALYSE_ALLOCATE(0,NP)
 !
 !REWIND(IU)
 ! 
 !READ(IU,*,IOSTAT=IOS) 
 !DO I=1,NI
 ! DO J=1,3; READ(IU,*,IOSTAT=IOS); IF(IOS.NE.0)THEN; CLOSE(IU); RETURN; ENDIF; ENDDO
 ! DO J=1,NP
 !  READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
 !  READ(LINE,'(I2,1X,A5,2(1X,I5),5(1X,F15.7),I10,L10,A10,A15)') PARAM(J)%IACT  ,PARAM(J)%PTYPE   ,PARAM(I)%ILS, &
 !                                                               PARAM(J)%IZONE ,IPEST(J)%ALPHA(I),PARAM(I)%DELTA,&
 !                                                               PARAM(J)%MIN   ,PARAM(J)%MAX     ,PARAM(I)%FADJ,&
 !                                                               PARAM(J)%IGROUP,PARAM(J)%LOG     ,PARAM(I)%NODES,&
 !                                                               PARAM(J)%ACRONYM
 ! ENDDO
 !ENDDO
 !CLOSE(IU)
 
 ! \log_pest_mf2005.txt
 !
 
! CLOSE(IU)
 
 CALL WDIALOGRANGETRACKBAR(IDF_TRACKBAR1,0,SIZE(IPEST)-1,1)

 END SUBROUTINE IPEST_ANALYSE_READLOG
 
 !###======================================================================
 SUBROUTINE IPEST_ANALYSE_INIT() 
 !###======================================================================
 IMPLICIT NONE 
 
 CALL WINDOWSELECT(0); IF(WMENUGETSTATE(ID_IPESTANALYSER,2).EQ.1)THEN
  CALL IPEST_ANALYSE_CLOSE(); RETURN
 ENDIF

 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_IPESTANALYSER,2,1)
 !## fill in dialog
 CALL WDIALOGLOAD(ID_DIPESTANALYSE,ID_DIPESTANALYSE)

! CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONOPEN,1)
 CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE IPEST_ANALYSE_INIT

 !###====================================================================
 SUBROUTINE IPEST_ANALYSE_PLOTGRAPH()
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND) :: YMIN,YMAX,XMIN,XMAX
 INTEGER :: I
 CHARACTER(LEN=52) :: XTITLE,YTITLE
 INTEGER,DIMENSION(6) :: GID
 DATA GID/IDF_PICTURE1,IDF_PICTURE2,IDF_PICTURE3,IDF_PICTURE4,IDF_PICTURE5,IDF_PICTURE6/

 !## PLOT TRACKBAR ZORGT ERVOOR DAT ER MNDER TE ZIEN IS ...
 
 CALL WDIALOGSELECT(ID_DIPESTANALYSE)

 DO I=1,SIZE(GID)
  CALL IGRSELECT(3,GID(I)); CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0); CALL IGRAREACLEAR()
  CALL IGRFILLPATTERN(SOLID)

  XMIN= HUGE(1.0D0); YMIN=XMIN
  XMAX=-HUGE(1.0D0); YMAX=YMIN

  !## set dimensions
  SELECT CASE (I)
   CASE (1); CALL IPEST_ANALYSE_DIMGRAPH1(XMIN,YMIN,XMAX,YMAX,XTITLE,YTITLE)
   CASE DEFAULT; CYCLE
  END SELECT
  
  !## set axes
  CALL IPEST_ANALYSE_PLOT_AXES(XMIN,YMIN,XMAX,YMAX,.FALSE.,XTITLE,YTITLE)

  !## plot figure
  SELECT CASE (I)
   CASE (1); CALL IPEST_ANALYSE_PLOTGRAPH1(XMIN,YMIN)
  END SELECT

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
 REAL(KIND=DP_KIND) :: DX,DY
 
 XTITLE='Iteration (#)' 
 YTITLE='Objective Function Value (m2)'
 
 XMIN=0.0D0; XMAX=SIZE(IPEST)-1
 YMIN=MINVAL(IPEST%J); YMAX=MAXVAL(IPEST%J)
 DX=(XMAX-XMIN)/20.0D0; XMIN=XMIN-DX; XMAX=XMAX+DX 
 DY=(YMAX-YMIN)/20.0D0; YMIN=YMIN-DY; YMAX=YMAX+DY 

 END SUBROUTINE IPEST_ANALYSE_DIMGRAPH1

 !###====================================================================
 SUBROUTINE IPEST_ANALYSE_PLOTGRAPH1(XMIN,YMIN)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: XMIN,YMIN
 INTEGER :: I,IPOS
 
 CALL IGRCOLOURN(WRGB(255,0,0))
 DO I=1,SIZE(IPEST)
  IF(I.EQ.1)THEN
   CALL DBL_IGRMOVETO(REAL(I-1,8),IPEST(I)%J)
  ELSE
   CALL DBL_IGRLINETO(REAL(I-1,8),IPEST(I)%J)
  ENDIF
 ENDDO
 
 !## get trackbar
 CALL WDIALOGGETTRACKBAR(IDF_TRACKBAR1,IPOS)
 
 CALL IGRCOLOURN(WRGB(0,0,0))
 CALL DBL_IGRMOVETO(DBLE(IPOS),YMIN)
 CALL DBL_IGRLINETO(DBLE(IPOS),IPEST(IPOS+1)%J)
 CALL DBL_IGRLINETO(XMIN,IPEST(IPOS+1)%J)
 
 END SUBROUTINE IPEST_ANALYSE_PLOTGRAPH1
 
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

  DO I=1,SIZE(IPEST); ALLOCATE(IPEST(I)%ALPHA(NP)); IPEST(I)%ALPHA=0.0D0; ENDDO
  DO I=1,SIZE(IPEST); ALLOCATE(IPEST(I)%CPARAM(NP)); IPEST(I)%CPARAM=''; ENDDO
 
 ENDIF
 
 END SUBROUTINE IPEST_ANALYSE_ALLOCATE

 !###======================================================================
 SUBROUTINE IPEST_ANALYSE_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 IF(ALLOCATED(IPEST))THEN
  DO I=1,SIZE(IPEST)
   IF(ASSOCIATED(IPEST(I)%ALPHA))DEALLOCATE(IPEST(I)%ALPHA) 
   IF(ASSOCIATED(IPEST(I)%CPARAM))DEALLOCATE(IPEST(I)%CPARAM) 
  ENDDO
  DEALLOCATE(IPEST)
 ENDIF
 
 IF(ALLOCATED(GRAPHUNITS))DEALLOCATE(GRAPHUNITS)
 IF(ALLOCATED(GRAPHAREA))DEALLOCATE(GRAPHAREA)

 END SUBROUTINE IPEST_ANALYSE_DEALLOCATE

 !###======================================================================
 SUBROUTINE IPEST_ANALYSE_CLOSE()
 !###======================================================================
 IMPLICIT NONE
 
 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_IPESTANALYSER,2,0)
 CALL IPEST_ANALYSE_DEALLOCATE() 
 CALL WDIALOGSELECT(ID_DIPESTANALYSE); CALL WDIALOGUNLOAD()

 END SUBROUTINE IPEST_ANALYSE_CLOSE
 
END MODULE MOD_IPEST_ANALYSER
