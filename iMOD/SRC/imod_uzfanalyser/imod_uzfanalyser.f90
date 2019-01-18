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
MODULE MOD_UZFANALYSER

USE WINTERACTER
USE RESOURCE
USE MODPLOT
USE MOD_DBL, ONLY : DBL_IGRUNITS,DBL_IGRAREA,DBL_IGRMOVETO,DBL_IGRLINETO
USE MOD_UZFANALYSER_PAR
USE MOD_UZFANALYSER_UTL
USE MOD_MAIN_UTL, ONLY : MAIN_UTL_INACTMODULE
USE MOD_UTL, ONLY : UTL_WSELECTFILE,UTL_GETUNIT
USE MOD_IPEST_ANALYSER, ONLY : IPEST_ANALYSE_PLOT_AXES
USE MOD_GRAPH, ONLY : GRAPH_PLOTAXES,AXESOBJ,GRAPHUNITS,GRAPHAREA

CONTAINS

 !###======================================================================
 SUBROUTINE UZFANALYSER_MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(INOUT) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 
 SELECT CASE (ITYPE)

  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (ID_OPEN)
     CALL UZFANALYSER_OPENFILE()
     CALL UZFANALYSE_EXTENT()
     CALL UZFANALYSE_PLOTGRAPH()
    CASE (IDHELP)
    CASE (IDCANCEL)
     CALL UZFANALYSER_CLOSE()
   END SELECT

  !## case field changed
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDF_MENU1)
     IF(MESSAGE%VALUE1.EQ.MESSAGE%VALUE2)CALL UZFANALYSE_PLOTGRAPH()
   END SELECT

  CASE (RESIZE,EXPOSE)
   CALL UZFANALYSE_PLOTGRAPH()
   
 END SELECT

 END SUBROUTINE UZFANALYSER_MAIN

 !###======================================================================
 SUBROUTINE UZFANALYSER_OPENFILE()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256) :: FNAME
 INTEGER :: I,J,IOS,IU,IPER,NPER,IZ,NZ
 
 IF(.NOT.UTL_WSELECTFILE('UZF Observation File (*.txt)|*.txt|',LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,&
      FNAME,'UZF Observation File (*.txt)'))RETURN
!FNAME='D:\IMOD-MODELS\TUAS\IMOD_USER\MODELS\VERSION_V8\CS_50\OBS1.TXT'
IU=UTL_GETUNIT()
 OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ')
 DO I=1,2
  DO J=1,3; READ(IU,*,IOSTAT=IOS); ENDDO
  !## set number of stress-periods
  IF(I.EQ.1)THEN
   NPER=0; NZ=1; DO
    READ(IU,'(9X,I5)',IOSTAT=IOS) J
    IF(IOS.NE.0)EXIT; IF(J.NE.0)THEN; NZ=1; NPER=NPER+1; ELSE; NZ=NZ+1; ENDIF
   ENDDO
   CALL UZFANALYSE_ALLOCATE(NPER,NZ)
  ELSE
   DO IPER=1,NPER
    IZ=1
    READ(IU,'(9X,I5,3X,5(1PE14.7,1X))',IOSTAT=IOS) MC(IPER)%ILAY,MC(IPER)%TIME,MC(IPER)%GWH,MC(IPER)%UZT,MC(IPER)%Z(IZ),MC(IPER)%M(IZ)
    WRITE(CPER(IPER),'(F15.7)') MC(IPER)%TIME
    DO IZ=2,NZ
     READ(IU,'(62X,2(1PE14.7,1X))',IOSTAT=IOS) MC(IPER)%Z(IZ),MC(IPER)%M(IZ)
    ENDDO
   ENDDO
  ENDIF
  REWIND(IU)
 ENDDO
 CLOSE(IU)
 
 DO IPER=1,NPER; MC(IPER)%Z=-1.0D0*MC(IPER)%Z; ENDDO
 
 CALL WDIALOGSELECT(ID_DUZFANALYSER)
 CALL WDIALOGPUTMENU(IDF_MENU1,CPER,SIZE(CPER),1) 
 CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
 
 END SUBROUTINE UZFANALYSER_OPENFILE

 !###======================================================================
 SUBROUTINE UZFANALYSE_EXTENT
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPER
 REAL(KIND=DP_KIND) :: DX,DY
 
 IF(WINFODIALOGFIELD(IDF_MENU1,FIELDSTATE).EQ.0)RETURN

 CALL WDIALOGSELECT(ID_DUZFANALYSER)
 
 CALL IGRSELECT(3,IDF_PICTURE1); CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0); CALL IGRAREACLEAR()
 CALL IGRFILLPATTERN(SOLID)
 
 XMIN= HUGE(1.0D0); YMIN=XMIN
 XMAX=-HUGE(1.0D0); YMAX=XMAX
 DO IPER=1,SIZE(MC)
  XMIN=MIN(XMIN,MINVAL(MC(IPER)%M))
  XMAX=MAX(XMAX,MAXVAL(MC(IPER)%M))
  YMIN=MIN(YMIN,MINVAL(MC(IPER)%Z))
  YMAX=MAX(YMAX,MAXVAL(MC(IPER)%Z))
 ENDDO
 DX=(XMAX-XMIN)/20.0D0; XMIN=XMIN-DX; XMAX=XMAX+DX 
 DY=(YMAX-YMIN)/20.0D0; YMIN=YMIN-DY; YMAX=YMAX+DY 

 END SUBROUTINE UZFANALYSE_EXTENT

 !###======================================================================
 SUBROUTINE UZFANALYSE_PLOTGRAPH()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IPER,IP1,IP2
 CHARACTER(LEN=52) :: XTITLE,YTITLE

 XTITLE='Moisture Content (%)'
 YTITLE='Depth (m)'

 CALL WDIALOGGETMENU(IDF_MENU1,IPER) 
 IP1=1 !IPER
 IP2=SIZE(MC) !IPER
 
 CALL IGRSELECT(3,IDF_PICTURE1); CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0); CALL IGRAREACLEAR()
 CALL IGRFILLPATTERN(SOLID)

 !## set axes
 CALL IPEST_ANALYSE_PLOT_AXES(XMIN,YMIN,XMAX,YMAX,.FALSE.,XTITLE,YTITLE,1)

 CALL IGRCOLOURN(WRGB(255,0,0))
 DO IPER=IP1,IP2
  DO I=1,SIZE(MC(IPER)%Z)
   IF(I.EQ.1)THEN
    CALL DBL_IGRMOVETO(MC(IPER)%M(I),MC(IPER)%Z(I))
   ELSE
    CALL DBL_IGRLINETO(MC(IPER)%M(I),MC(IPER)%Z(I))
   ENDIF
  ENDDO
 ENDDO
 
 CALL IGRSELECT(DRAWWIN,MPW%IWIN); CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
 CALL DBL_IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX,IOFFSET=1)

 END SUBROUTINE UZFANALYSE_PLOTGRAPH
 
 !###======================================================================
 SUBROUTINE UZFANALYSER_FIELDS()
 !###======================================================================
 IMPLICIT NONE


 END SUBROUTINE UZFANALYSER_FIELDS
 
 !###======================================================================
 SUBROUTINE UZFANALYSER_INIT()
 !###======================================================================
 IMPLICIT NONE

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_UZFANALYSER,2).EQ.1)THEN
  CALL UZFANALYSER_CLOSE(); RETURN
 ENDIF

 CALL MAIN_UTL_INACTMODULE(ID_MSPANALYSER)

 !## other module not closed, no approvement given to start this functionality
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_UZFANALYSER,2,1)

 CALL WDIALOGLOAD(ID_DUZFANALYSER,ID_DUZFANALYSER)
 CALL WDIALOGFIELDSTATE(IDF_MENU1,0)
 CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE UZFANALYSER_INIT
   
END MODULE MOD_UZFANALYSER
