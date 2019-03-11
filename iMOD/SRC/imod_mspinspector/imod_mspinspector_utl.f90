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
MODULE MOD_MSPINSPECTOR_UTL

USE WINTERACTER
USE RESOURCE
USE MOD_IDF, ONLY : IDFDEALLOCATEX,IDFGETEDGE
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_MSPINSPECTOR_PAR
USE MOD_DBL, ONLY : DBL_IGRRECTANGLE
USE MOD_UTL, ONLY : UTL_PLOT1BITMAP,UTL_PLOT2BITMAP,UTL_INVERSECOLOUR


CONTAINS
 
 !###======================================================================
 SUBROUTINE MSPINSPECTOR_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE

 IF(ASSOCIATED(DXC%INFO)) DEALLOCATE(DXC%INFO)
 IF(ASSOCIATED(DXC%LABEL))DEALLOCATE(DXC%LABEL)
 IF(ASSOCIATED(DXC%IACT)) DEALLOCATE(DXC%IACT)
 
 IF(ASSOCIATED(MODSVAT%INFO)) DEALLOCATE(MODSVAT%INFO)
 IF(ASSOCIATED(MODSVAT%LABEL))DEALLOCATE(MODSVAT%LABEL)
 IF(ASSOCIATED(MODSVAT%IACT)) DEALLOCATE(MODSVAT%IACT)

 IF(ASSOCIATED(IDFSVAT%INFO)) DEALLOCATE(IDFSVAT%INFO)
 IF(ASSOCIATED(IDFSVAT%LABEL))DEALLOCATE(IDFSVAT%LABEL)
 IF(ASSOCIATED(IDFSVAT%IACT)) DEALLOCATE(IDFSVAT%IACT)

 IF(ASSOCIATED(AREASVAT%INFO)) DEALLOCATE(AREASVAT%INFO)
 IF(ASSOCIATED(AREASVAT%LABEL))DEALLOCATE(AREASVAT%LABEL)
 IF(ASSOCIATED(AREASVAT%IACT)) DEALLOCATE(AREASVAT%IACT)
 
 IF(ASSOCIATED(INFISVAT%INFO)) DEALLOCATE(INFISVAT%INFO)
 IF(ASSOCIATED(INFISVAT%LABEL))DEALLOCATE(INFISVAT%LABEL)
 IF(ASSOCIATED(INFISVAT%IACT)) DEALLOCATE(INFISVAT%IACT)
 
 CALL IDFDEALLOCATEX(MSPIDF)
 
 END SUBROUTINE MSPINSPECTOR_DEALLOCATE

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_CLEANGRIDS()
 !###======================================================================
 IMPLICIT NONE

 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB2) ; CALL WGridClear(IDF_GRID1); CALL WGridClear(IDF_GRID2); CALL WGridClear(IDF_GRID4); CALL WGridClear(IDF_GRID5)
                                            CALL WGridClearCell(IDF_GRID3,2,1) ; CALL WGridClearCell(IDF_GRID3,3,1) 
 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB4) ; CALL WGridClear(IDF_GRID1); CALL WGridClear(IDF_GRID2);  
 
 !## remove drawn irrigation  rectangles
 CALL MSPINSPECTOR_CLEANSCAPPOINTER()     

 !## fill tabs 2 
 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB2)
 CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,1,'Rural')
 CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,2,'Irrigation')
 CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,3,'Urban')
 CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,4,'Nopp')
 
 CALL WGRIDPUTCELLSTRING(IDF_GRID3,1,1,'SVAT_ID')
 CALL WGRIDPUTCELLSTRING(IDF_GRID3,1,2,'Mouse')

 END SUBROUTINE MSPINSPECTOR_CLEANGRIDS

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_CLEANSCAPPOINTER()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ICOL,IROW,I
 REAL(KIND=DP_KIND) :: X1,Y1,X2,Y2

 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB4) 
 CALL IGRCOLOURN(UTL_INVERSECOLOUR(WRGB(0,0,255))) !## blue rectancle for irrigation cells
 CALL UTL_PLOT1BITMAP()
 
 I=1
 DO 
  IF(WInfoGridCell(IDF_GRID3,1,I,1).EQ.0)EXIT
  CALL WGridGetCellInteger(IDF_GRID3,1,I,IROW)
  CALL WGridGetCellInteger(IDF_GRID3,2,I,ICOL)
  IF(MSPIDF%X(ICOL,IROW).GT.0.)THEN
    CALL IDFGETEDGE(MSPIDF,IROW,ICOL,X1,Y1,X2,Y2)
    CALL DBL_IGRRECTANGLE(X1,Y1,X2,Y2,IOFFSET=1)
    MSPIDF%X(ICOL,IROW)=0.
  ENDIF 
  I=I+1
 ENDDO
 
 CALL UTL_PLOT2BITMAP()
 CALL WGRIDCLEAR(IDF_GRID3)
 
 END SUBROUTINE MSPINSPECTOR_CLEANSCAPPOINTER

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IDIAGERROR=1
 
 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_MSPANALYSER,2,0)

 CALL MSPINSPECTOR_DEALLOCATE()
 CALL WDIALOGSELECT(ID_DMSPANALYSER); CALL WDIALOGUNLOAD()

 IDIAGERROR=0

 END SUBROUTINE MSPINSPECTOR_CLOSE
  
END MODULE MOD_MSPINSPECTOR_UTL
