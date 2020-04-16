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
MODULE MOD_IR_PLOT

USE MOD_IR_PAR
!USE MOD_POLYGON_PAR
!USE MOD_IDFPLOT
USE MOD_UTL
USE IMODVAR

CONTAINS

 !###======================================================================
 SUBROUTINE IR1DRAWSHAPES(ISEQ)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ISEQ
 INTEGER :: N,IPOL,IFIELD1,IFIELD2!,ITREE1,ITREE2,ITARGET,IMEASUREI

 CALL UTL_PLOT1BITMAP()

 CALL IGRPLOTMODE(MODEXOR)
 CALL IGRLINEWIDTH(2)
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRFILLPATTERN(OUTLINE)

 IFIELD1=0
 IFIELD2=0

 !## redraw polygon
 IF(ISEQ.EQ.1)THEN
  !## result tab, draw target polygon to be editable - measure polygon is not!
  IF(ICUR_ITREE.EQ.1)IFIELD1=ICUR_IFIELD   !## previous is target - remove/draw it
  IF(ICUR_ITREE.EQ.2)IFIELD2=ICUR_IFIELD   !## previous is measure - remove/draw it
 !## draw non-selected (only used from imod_data.f90)
 ELSEIF(ISEQ.EQ.2)THEN
  !## result tab, draw target polygon to be not editable
  IF(ICUR_ITREE.EQ.1)IFIELD2=ICUR_IFIELD   !## previous is target - remove/draw it
  IF(ICUR_ITREE.EQ.2)IFIELD1=ICUR_IFIELD   !## previous is measure - remove/draw it
 ENDIF

 !## targets
 IF(IFIELD1.NE.0)THEN
  !## get selected ipol
  DO IPOL=1,TTREE(IFIELD1)%NPOL
   CALL IGRCOLOURN(UTL_INVERSECOLOUR(TTREE(IFIELD1)%POL(IPOL)%ICLR))
   N=TTREE(IFIELD1)%POL(IPOL)%NCRD
   CALL DBL_IGRPOLYGONCOMPLEX(TTREE(IFIELD1)%POL(IPOL)%X,TTREE(IFIELD1)%POL(IPOL)%Y,N)
  ENDDO
 ENDIF
 !## measure
 IF(IFIELD2.NE.0)THEN
  !## get selected ipol
  DO IPOL=1,MTREE(IFIELD2)%NPOL
   CALL IGRCOLOURN(UTL_INVERSECOLOUR(MTREE(IFIELD2)%POL(IPOL)%ICLR))
   N=MTREE(IFIELD2)%POL(IPOL)%NCRD
   CALL DBL_IGRPOLYGONCOMPLEX(MTREE(IFIELD2)%POL(IPOL)%X,MTREE(IFIELD2)%POL(IPOL)%Y,N)
  ENDDO
 ENDIF

 CALL UTL_PLOT2BITMAP()

 CALL IGRLINEWIDTH(1)
 CALL IGRPLOTMODE(MODECOPY)

 END SUBROUTINE IR1DRAWSHAPES

END MODULE MOD_IR_PLOT

