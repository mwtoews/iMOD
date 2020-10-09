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
MODULE MOD_ASC2IDF_UTL

USE MOD_ASC2IDF_PAR

CONTAINS

 !###======================================================================
 SUBROUTINE ASC2IDF_INT_NULLIFY()
 !###======================================================================
 IMPLICIT NONE
 
 NULLIFY(XP,YP,ZP,WP,FP)
  
 END SUBROUTINE ASC2IDF_INT_NULLIFY

 !###======================================================================
 SUBROUTINE ASC2IDF_INT_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 
 IF(ASSOCIATED(XP))DEALLOCATE(XP)
 IF(ASSOCIATED(YP))DEALLOCATE(YP) 
 IF(ASSOCIATED(ZP))DEALLOCATE(ZP) 
 IF(ASSOCIATED(WP))DEALLOCATE(WP) 
 IF(ASSOCIATED(FP))DEALLOCATE(FP) 
 IF(ASSOCIATED(PP))DEALLOCATE(PP) 
  
 END SUBROUTINE ASC2IDF_INT_DEALLOCATE
 
 !###======================================================================
 SUBROUTINE ASC2IDF_INT_RESIZEVECTORS(N,DN)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,DN
 INTEGER :: MM,NN,I
 
 !## make sure vector is equal to entered abs(n)
 IF(DN.EQ.0)THEN
  MM=N; NN=N; IF(NN.EQ.SIZE(XP))RETURN; NN=SIZE(XP)
 ELSE
  NN=0; IF(ASSOCIATED(XP))NN=SIZE(XP)
  IF(N.LE.NN)RETURN; MM=NN+DN
 ENDIF
 
 IF(MM.EQ.0)THEN; CALL ASC2IDF_INT_DEALLOCATE(); RETURN; ENDIF
 
 ALLOCATE(XP_DUMMY(MM),YP_DUMMY(MM),ZP_DUMMY(MM),WP_DUMMY(MM),FP_DUMMY(MM),PP_DUMMY(MM))
 
 XP_DUMMY=0.0D0
 YP_DUMMY=0.0D0
 ZP_DUMMY=0.0D0
 PP_DUMMY=0.0D0
 WP_DUMMY=0.0D0
 FP_DUMMY=0.0D0

 DO I=1,MIN(MM,NN)
  XP_DUMMY(I)=XP(I)
  YP_DUMMY(I)=YP(I)
  ZP_DUMMY(I)=ZP(I)
  PP_DUMMY(I)=PP(I)
  WP_DUMMY(I)=WP(I)
  FP_DUMMY(I)=FP(I)
 ENDDO
 
 CALL ASC2IDF_INT_DEALLOCATE()
 
 XP=>XP_DUMMY
 YP=>YP_DUMMY
 ZP=>ZP_DUMMY
 PP=>PP_DUMMY
 WP=>WP_DUMMY
 FP=>FP_DUMMY
 
 END SUBROUTINE ASC2IDF_INT_RESIZEVECTORS

END MODULE MOD_ASC2IDF_UTL
