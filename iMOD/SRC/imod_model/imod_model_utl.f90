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
MODULE MOD_MODEL_UTL

USE WINTERACTER
USE RESOURCE
USE MOD_DBL
USE IMODVAR, ONLY : DP_KIND,SP_KIND,IDIAGERROR
USE MOD_MODEL_PAR
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_INVERSECOLOUR,ITOS,UTL_PLOT1BITMAP,UTL_PLOT2BITMAP
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_COLOURS
USE MOD_IDF, ONLY : IDFDEALLOCATEX

CONTAINS

 !###======================================================================
 SUBROUTINE MODEL1CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IDIAGERROR=1

 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_RUNMODEL,2,0)

 CALL WDIALOGSELECT(ID_DMODEL)
 CALL WDIALOGUNLOAD()

 CALL MODEL1DEALLOCATE()

 !## deallocate x for idf (possible)
 CALL IDFDEALLOCATEX(IDF)

 IDIAGERROR=0

 END SUBROUTINE MODEL1CLOSE
 
  !###======================================================================
 SUBROUTINE MODEL1DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(IAMDL))DEALLOCATE(IAMDL)
 IF(ALLOCATED(NLMDL))DEALLOCATE(NLMDL)
 IF(ALLOCATED(ILMDL))DEALLOCATE(ILMDL)

 END SUBROUTINE MODEL1DEALLOCATE

 !###======================================================================
 SUBROUTINE MODEL1DRAW_SIMBOX()
 !###======================================================================
 IMPLICIT NONE

 IF(IDRAW.EQ.0)RETURN 

 CALL UTL_PLOT1BITMAP()
 CALL IGRPLOTMODE(MODEXOR)
 !## draw network
 CALL MODEL1PLOT_SIMBOX()
 CALL UTL_PLOT2BITMAP()

 CALL IGRPLOTMODE(MODECOPY)

 END SUBROUTINE MODEL1DRAW_SIMBOX

 !###======================================================================
 SUBROUTINE MODEL1PLOT_SIMBOX()
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND) :: X1,Y1,X2,Y2

 !## black - area of interest
 X1=SIMBOX(1)
 X2=SIMBOX(3)
 Y1=SIMBOX(2)
 Y2=SIMBOX(4)

 CALL IGRFILLPATTERN(HATCHED,DENSE1,DIAGUP)
 CALL IGRCOLOURN(UTL_INVERSECOLOUR(WRGB(0,0,0)))
 IF(ABS(X1-X2).GT.0.0D0.AND.ABS(Y2-Y1).GT.0.0D0)CALL DBL_IGRRECTANGLE(X1,Y1,X2,Y2,IOFFSET=1)
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRLINEWIDTH(5)
 IF(ABS(X1-X2).GT.0.0D0.AND.ABS(Y2-Y1).GT.0.0D0)CALL DBL_IGRRECTANGLE(X1,Y1,X2,Y2,IOFFSET=1)

 !## buffer - green
 X1=MAX(SIMBOX(1)-MDLBUFFER,MODBOX(1))
 X2=MIN(SIMBOX(3)+MDLBUFFER,MODBOX(3))
 Y1=MAX(SIMBOX(2)-MDLBUFFER,MODBOX(2))
 Y2=MIN(SIMBOX(4)+MDLBUFFER,MODBOX(4))
 CALL IGRCOLOURN(UTL_INVERSECOLOUR(WRGB(0,255,0)))
 IF(ABS(X1-X2).GT.0.0D0.AND.ABS(Y2-Y1).GT.0.0D0)CALL DBL_IGRRECTANGLE(X1,Y1,X2,Y2,IOFFSET=1)
 
 !## total model area
 X1=MODBOX(1)
 X2=MODBOX(3)
 Y1=MODBOX(2)
 Y2=MODBOX(4)
 CALL IGRCOLOURN(UTL_INVERSECOLOUR(WRGB(0,0,255)))
 IF(ABS(X1-X2).GT.0.0D0.AND.ABS(Y2-Y1).GT.0.0D0)CALL DBL_IGRRECTANGLE(X1,Y1,X2,Y2,IOFFSET=1)
 CALL IGRLINEWIDTH(1)
 CALL IGRFILLPATTERN(SOLID)

 END SUBROUTINE MODEL1PLOT_SIMBOX
 
 !###======================================================================
 SUBROUTINE MODEL1TABSTATES(LSTATE,LCHECK)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL,INTENT(IN) :: LSTATE,LCHECK
 INTEGER :: ISTATE

 ISTATE=0; IF(LSTATE)ISTATE=1

 CALL WDIALOGSELECT(ID_DMODEL)
 CALL WDIALOGTABSTATE(IDF_TAB,ID_DMDLTAB2,ISTATE)
 CALL WDIALOGTABSTATE(IDF_TAB,ID_DMDLTAB3,ISTATE)
 CALL WDIALOGTABSTATE(IDF_TAB,ID_DMDLTAB4,ISTATE)
 CALL WDIALOGTABSTATE(IDF_TAB,ID_DMDLTAB6,ISTATE)
 
 IF(LCHECK)THEN
  CALL WDIALOGSELECT(ID_DMDLTAB1)
  CALL WDIALOGFIELDSTATE(ID_CHECKRUNFILE,ISTATE)
 ENDIF
 
 END SUBROUTINE MODEL1TABSTATES

 !###======================================================================
 LOGICAL FUNCTION MODEL1READRUNFILEERROR(IOS,IU,TXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: TXT
 INTEGER,INTENT(IN) :: IOS,IU

 MODEL1READRUNFILEERROR=.TRUE.

 IF(IOS.EQ.0)RETURN

 MODEL1READRUNFILEERROR=.FALSE.

 IF(IU.NE.0)CLOSE(IU)
 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error occured reading selected run-file'//CHAR(13)// &
      TRIM(TXT)//CHAR(13)//CHAR(13)//'Check/adjust your runfile !','Error')
 IDRAW=0

 END FUNCTION MODEL1READRUNFILEERROR
  
END MODULE MOD_MODEL_UTL