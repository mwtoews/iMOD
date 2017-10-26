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
MODULE MOD_ASC2IDF_PAR
 USE MOD_IDF_PAR, ONLY : IDFOBJ
 
 INTEGER(KIND=8) :: I,N
 CHARACTER(LEN=256) :: IDFFILE,SOURCEDIR,TARGETDIR,STDEVIDF,GENFILE,BLNFILE
 INTEGER :: IGRIDFUNC,MINP,KTYPE,IXCOL,IYCOL,IZCOL,PNTSEARCH,LAGINTERVAL,ASSF_INDICATOR,ASSF_NTHRESHOLD, &
       ASSF_COLUMN,ASSF_STARTDATE,ASSF_ENDDATE,ASSF_DDATE,ILOG,COINCIDENT,IQUADRANT,ASSF_IDEPTH,IINT_IDF
 CHARACTER(LEN=1) :: ASSF_CDDATE
 REAL,ALLOCATABLE,DIMENSION(:) :: ASSF_K_THRESHOLD
 CHARACTER(LEN=52),ALLOCATABLE,DIMENSION(:) :: ASSF_THRESHOLD
 REAL :: CS,NODATA,PERCENTILE,RANGE,SILL,NUGGET,LAGDISTANCE,COINCIDENTDIST,ASSF_TOP,ASSF_BOT,ASSF_DZ
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: XYZFNAMES
 TYPE(IDFOBJ),DIMENSION(2) :: TRIMDEPTH_IDF
 TYPE(IDFOBJ),DIMENSION(3) :: ELLIPS_IDF
 TYPE(IDFOBJ),DIMENSION(:),ALLOCATABLE :: INT_IDF
 
 REAL,POINTER,DIMENSION(:) :: XP,XP_DUMMY,YP,YP_DUMMY,ZP,ZP_DUMMY,WP,WP_DUMMY,FP,FP_DUMMY
 
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
  NN=SIZE(XP); IF(N.LE.NN)RETURN; MM=NN+DN
 ENDIF
 
 ALLOCATE(XP_DUMMY(MM),YP_DUMMY(MM),ZP_DUMMY(MM),WP_DUMMY(MM),FP_DUMMY(MM))
 
 XP_DUMMY=0.0
 YP_DUMMY=0.0
 ZP_DUMMY=0.0
 WP_DUMMY=0.0
 FP_DUMMY=0.0

 DO I=1,MIN(MM,NN)
  XP_DUMMY(I)=XP(I)
  YP_DUMMY(I)=YP(I)
  ZP_DUMMY(I)=ZP(I)
  WP_DUMMY(I)=WP(I)
  FP_DUMMY(I)=FP(I)
 ENDDO
 
 DEALLOCATE(XP,YP,ZP,WP,FP)
 
 XP=>XP_DUMMY
 YP=>YP_DUMMY
 ZP=>ZP_DUMMY
 WP=>WP_DUMMY
 FP=>FP_DUMMY
 
 END SUBROUTINE ASC2IDF_INT_RESIZEVECTORS

END MODULE MOD_ASC2IDF_PAR

