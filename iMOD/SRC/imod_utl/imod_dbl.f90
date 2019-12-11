!!  Copyright (C) Stichting Deltares, 2005-2019.
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
MODULE MOD_DBL

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : DP_KIND,SP_KIND,OFFSETX,OFFSETY
USE MOD_POLYGON_PAR

CONTAINS

 !###====================================================================
 SUBROUTINE DBL_IPGXGETSCALE(SVALUE,NS,AXES)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: NS
 REAL(KIND=DP_KIND),DIMENSION(:),INTENT(OUT) :: SVALUE
 REAL(KIND=4),DIMENSION(:),ALLOCATABLE :: S
 CHARACTER(LEN=1),INTENT(IN) :: AXES
 INTEGER :: N,I
 CHARACTER(LEN=52) :: VAL
   
 N=SIZE(SVALUE); ALLOCATE(S(N))
 
 IF(AXES.EQ.'X'.OR.AXES.EQ.'x')THEN
  CALL IPGXGETSCALE(S,NS)
 ELSE
  CALL IPGYGETSCALE(S,NS)
 ENDIF
  
 DO I=1,NS
!  SVALUE(I)=DBLE(S(I))
  !## seven significant numbers as it is a single-precision
  WRITE(VAL,'(G15.6)') S(I)
  READ(VAL,*) SVALUE(I)
 ENDDO
 
 DEALLOCATE(S)
  
 END SUBROUTINE DBL_IPGXGETSCALE
 
 !###====================================================================
 SUBROUTINE DBL_IGRINTERSECTLINE(X1,Y1,X2,Y2,X3,Y3,X4,Y4,XINTER,YINTER,ISTATUS) 
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X1,Y1,X2,Y2,X3,Y3,X4,Y4
 REAL(KIND=DP_KIND),INTENT(OUT) :: XINTER,YINTER
 INTEGER,INTENT(INOUT) :: ISTATUS
 REAL(KIND=4) :: X,Y
 
 CALL IGRINTERSECTLINE(REAL(X1,4),REAL(Y1,4),REAL(X2,4),REAL(Y2,4), & 
                       REAL(X3,4),REAL(Y3,4),REAL(X4,4),REAL(Y4,4), &
                       X,Y,ISTATUS)
 XINTER=REAL(X,8)
 YINTER=REAL(Y,8)

 END SUBROUTINE DBL_IGRINTERSECTLINE
 
 !###====================================================================
 REAL(KIND=DP_KIND) FUNCTION DBL_IGRDISTANCELINE(XPOS1,YPOS1,XPOS2,YPOS2,XS1,YS1,IMETHOD) 
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: XPOS1,YPOS1,XPOS2,YPOS2,XS1,YS1
 INTEGER,INTENT(IN) :: IMETHOD

 DBL_IGRDISTANCELINE=IGRDISTANCELINE(REAL(XPOS1,4),REAL(YPOS1,4),REAL(XPOS2,4),REAL(YPOS2,4), &
          REAL(XS1,4),REAL(YS1,4),IMETHOD)

 END FUNCTION DBL_IGRDISTANCELINE
 
 !###====================================================================
 SUBROUTINE DBL_WGRTEXTORIENTATION(IALIGN,ANGLE,IDIR,NALIGN)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN),OPTIONAL :: IALIGN,IDIR,NALIGN
 REAL(KIND=DP_KIND),INTENT(IN),OPTIONAL :: ANGLE
 REAL :: AN
 INTEGER :: IA,ID,NA
 
 IA=0  ; IF(PRESENT(IALIGN))IA=IALIGN
 AN=0.0; IF(PRESENT(ANGLE)) AN=REAL(ANGLE,4)
 ID=0  ; IF(PRESENT(IDIR))  ID=IDIR
 NA=0  ; IF(PRESENT(NALIGN))NA=NALIGN
 
 CALL WGRTEXTORIENTATION(IALIGN=IA,ANGLE=AN,IDIR=ID,NALIGN=NA)
 
 END SUBROUTINE DBL_WGRTEXTORIENTATION
  
 !###====================================================================
 SUBROUTINE DBL_WGRTEXTBLOCK(XPOS1,YPOS1,XPOS2,YPOS2,TXT,SPACING,IFLAGS)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: XPOS1,YPOS1,XPOS2,YPOS2
 REAL(KIND=DP_KIND),INTENT(IN),OPTIONAL :: SPACING
 CHARACTER(LEN=*),INTENT(IN) :: TXT
 INTEGER,INTENT(IN),OPTIONAL :: IFLAGS
 REAL(KIND=4) :: SP
 INTEGER :: FG
  
 SP=1.0; IF(PRESENT(SPACING))SP=REAL(SPACING,4)
 FG=0;   IF(PRESENT(IFLAGS))FG=IFLAGS
 
 CALL WGRTEXTBLOCK(REAL(XPOS1,4),REAL(YPOS1,4),REAL(XPOS2,4),REAL(YPOS2,4),TRIM(TXT),SP,FG)

 END SUBROUTINE DBL_WGRTEXTBLOCK
 
 !###====================================================================
 SUBROUTINE DBL_IGRUNITSFROMPIXELS(IXPIXL,IYPIXL,XPOS,YPOS,IORIGIN,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(OUT) :: XPOS,YPOS
 INTEGER,INTENT(IN) :: IXPIXL,IYPIXL
 INTEGER,INTENT(IN),OPTIONAL :: IORIGIN
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 INTEGER :: IOR
 REAL(KIND=4) :: X,Y
 
 IOR=0; IF(PRESENT(IORIGIN))IOR=IORIGIN

 IF(PRESENT(IOFFSET))THEN
  CALL IGRUNITSFROMPIXELS(IXPIXL,IYPIXL,X,Y,IORIGIN=IOR)
  XPOS=DBLE(X)+OFFSETX
  YPOS=DBLE(Y)+OFFSETY
 ELSE
  CALL IGRUNITSFROMPIXELS(IXPIXL,IYPIXL,X,Y,IORIGIN=IOR)
  XPOS=DBLE(X)
  YPOS=DBLE(Y)
 ENDIF
 
 END SUBROUTINE DBL_IGRUNITSFROMPIXELS
 
 !###====================================================================
 SUBROUTINE DBL_IGRUNITSTOPIXELS(XPOS,YPOS,IXPIXL,IYPIXL,IORIGIN,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: XPOS,YPOS
 INTEGER,INTENT(OUT) :: IXPIXL,IYPIXL
 INTEGER,INTENT(IN),OPTIONAL :: IORIGIN
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 INTEGER :: IOR
 
 IOR=0; IF(PRESENT(IORIGIN))IOR=IORIGIN
  
 IF(PRESENT(IOFFSET))THEN
  CALL IGRUNITSTOPIXELS(REAL(XPOS-OFFSETX,4),REAL(YPOS-OFFSETY,4),IXPIXL,IYPIXL,IORIGIN=IOR)
 ELSE
  CALL IGRUNITSTOPIXELS(REAL(XPOS,4),REAL(YPOS,4),IXPIXL,IYPIXL,IORIGIN=IOR)
 ENDIF
 
 END SUBROUTINE DBL_IGRUNITSTOPIXELS

 !###====================================================================
 SUBROUTINE DBL_IGRARROWJOIN(X0,Y0,X1,Y1,IATYPE,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X0,Y0,X1,Y1
 INTEGER,INTENT(IN) :: IATYPE
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 
 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL IGRARROWJOIN(REAL(X0,4),REAL(Y0,4),REAL(X1,4),REAL(Y1,4),IATYPE)
  ELSE
   CALL IGRARROWJOIN(REAL(X0-OFFSETX,4),REAL(Y0-OFFSETY,4),REAL(X1-OFFSETX,4),REAL(Y1-OFFSETY,4),IATYPE)
  ENDIF
 ELSE
  CALL IGRARROWJOIN(REAL(X0,4),REAL(Y0,4),REAL(X1,4),REAL(Y1,4),IATYPE)
 ENDIF

 END SUBROUTINE DBL_IGRARROWJOIN
 
 !###====================================================================
 SUBROUTINE DBL_IGRAREA(X1,Y1,X2,Y2)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X1,Y1,X2,Y2
 REAL(KIND=SP_KIND) :: XS1,YS1,XS2,YS2
 
 XS1=MIN(MAX(0.0,DBLE(X1)),1.0)
 XS2=MIN(MAX(0.0,DBLE(X2)),1.0)
 YS1=MIN(MAX(0.0,DBLE(Y1)),1.0)
 YS2=MIN(MAX(0.0,DBLE(Y2)),1.0)

 CALL IGRAREA(XS1,YS1,XS2,YS2)
! CALL IGRAREA(REAL(X1,4),REAL(Y1,4),REAL(X2,4),REAL(Y2,4))

 END SUBROUTINE DBL_IGRAREA
 
 !###====================================================================
 SUBROUTINE DBL_IGRUNITS(X1,Y1,X2,Y2,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X1,Y1,X2,Y2
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 
 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL IGRUNITS(REAL(X1,4),REAL(Y1,4),REAL(X2,4),REAL(Y2,4))
  ELSE
   CALL IGRUNITS(REAL(X1-OFFSETX,4),REAL(Y1-OFFSETY,4), &
                 REAL(X2-OFFSETX,4),REAL(Y2-OFFSETY,4))
  ENDIF
 ELSE
  CALL IGRUNITS(REAL(X1,4),REAL(Y1,4),REAL(X2,4),REAL(Y2,4))
 ENDIF
 
 END SUBROUTINE DBL_IGRUNITS

 !###====================================================================
 SUBROUTINE DBL_IGRVIEWPORT(X1,Y1,X2,Y2)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X1,Y1,X2,Y2

 CALL IGRVIEWPORT(REAL(X1,4),REAL(Y1,4),REAL(X2,4),REAL(Y2,4))

 END SUBROUTINE DBL_IGRVIEWPORT

 !###====================================================================
 SUBROUTINE DBL_WGRTEXTINTEGER(X,Y,I,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y
 INTEGER,INTENT(IN) :: I
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 
 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL WGRTEXTINTEGER(REAL(X,4),REAL(Y,4),I)
  ELSE
   CALL WGRTEXTINTEGER(REAL(X-OFFSETX,4),REAL(Y-OFFSETY,4),I)
  ENDIF
 ELSE
  CALL WGRTEXTINTEGER(REAL(X,4),REAL(Y,4),I)
 ENDIF
 
 END SUBROUTINE DBL_WGRTEXTINTEGER

 !###====================================================================
 SUBROUTINE DBL_WGRTEXTREAL(X,Y,F,TXT,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y,F
 CHARACTER(LEN=*),INTENT(IN) :: TXT
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 
 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL WGRTEXTREAL(REAL(X,4),REAL(Y,4),REAL(F,4),TRIM(TXT))
  ELSE
   CALL WGRTEXTREAL(REAL(X-OFFSETX,4),REAL(Y-OFFSETY,4),REAL(F,4),TRIM(TXT))
  ENDIF
 ELSE
  CALL WGRTEXTREAL(REAL(X,4),REAL(Y,4),REAL(F,4),TRIM(TXT))
 ENDIF
 
 END SUBROUTINE DBL_WGRTEXTREAL

 !###====================================================================
 SUBROUTINE DBL_WGRTEXTFONT(IFAMILY,TWIDTH,THEIGHT,ISTYLE)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN),OPTIONAL :: TWIDTH,THEIGHT
 INTEGER,INTENT(IN),OPTIONAL :: ISTYLE
 INTEGER,INTENT(IN),OPTIONAL :: IFAMILY
 INTEGER :: IM,IS
 REAL(KIND=4) :: XW,XH
 
 IM=0;        IF(PRESENT(IFAMILY))IM=IFAMILY
 XW=0.013333; IF(PRESENT(TWIDTH)) XW=REAL(TWIDTH,4)
 XH=0.013333; IF(PRESENT(THEIGHT))XH=REAL(THEIGHT,4)
 IS=0;        IF(PRESENT(ISTYLE)) IS=ISTYLE
 
 CALL WGRTEXTFONT(IFAMILY=IM,WIDTH=XW,HEIGHT=XH,ISTYLE=IS)

 END SUBROUTINE DBL_WGRTEXTFONT

 !###====================================================================
 SUBROUTINE DBL_WGRTEXTSTRING(X,Y,TXT,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y
 CHARACTER(LEN=*),INTENT(IN) :: TXT
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET

 IF(LEN_TRIM(TXT).LE.0)RETURN

 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL WGRTEXTSTRING(REAL(X,4),REAL(Y,4),TRIM(TXT))
  ELSE
   CALL WGRTEXTSTRING(REAL(X-OFFSETX,4),REAL(Y-OFFSETY,4),TRIM(TXT))
  ENDIF
 ELSE
  CALL WGRTEXTSTRING(REAL(X,4),REAL(Y,4),TRIM(TXT))
 ENDIF
 
 END SUBROUTINE DBL_WGRTEXTSTRING

 !###====================================================================
 SUBROUTINE DBL_WGRTEXTDOUBLE(X,Y,XTXT,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y,XTXT
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET

 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL WGRTEXTDOUBLE(REAL(X,4),REAL(Y,4),XTXT)
  ELSE
   CALL WGRTEXTDOUBLE(REAL(X-OFFSETX,4),REAL(Y-OFFSETY,4),XTXT)
  ENDIF
 ELSE
  CALL WGRTEXTDOUBLE(REAL(X,4),REAL(Y,4),XTXT)
 ENDIF
 
 END SUBROUTINE DBL_WGRTEXTDOUBLE

 !###====================================================================
 SUBROUTINE DBL_IGRCIRCLE(X,Y,R,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y,R
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 
 IF(R.LT.0.0D0)RETURN

 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL IGRCIRCLE(REAL(X,4),REAL(Y,4),REAL(R,4))
  ELSE
   CALL IGRCIRCLE(REAL(X-OFFSETX,4),REAL(Y-OFFSETY,4),REAL(R,4))
  ENDIF
 ELSE
  CALL IGRCIRCLE(REAL(X,4),REAL(Y,4),REAL(R,4))
 ENDIF

 END SUBROUTINE DBL_IGRCIRCLE

 !###====================================================================
 SUBROUTINE DBL_IGRMARKER(X,Y,ISYMBOL,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y
 INTEGER,INTENT(IN) :: ISYMBOL
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 
 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL IGRMARKER(REAL(X,4),REAL(Y,4),ISYMBOL)
  ELSE
   CALL IGRMARKER(REAL(X-OFFSETX,4),REAL(Y-OFFSETY,4),ISYMBOL)
  ENDIF
 ELSE
  CALL IGRMARKER(REAL(X,4),REAL(Y,4),ISYMBOL)
 ENDIF
 
 END SUBROUTINE DBL_IGRMARKER

 !###====================================================================
 SUBROUTINE DBL_IGRLINETOREL(DX,DY,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: DX,DY
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 
 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL IGRLINETOREL(REAL(DX,4), REAL(DY,4))
  ELSE
   CALL IGRLINETOREL(REAL(DX-OFFSETX,4), REAL(DY-OFFSETY,4))
  ENDIF
 ELSE
  CALL IGRLINETOREL(REAL(DX,4), REAL(DY,4))
 ENDIF
 
 END SUBROUTINE DBL_IGRLINETOREL

 !###====================================================================
 SUBROUTINE DBL_IGRRECTANGLE(X1,Y1,X2,Y2,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X1,Y1,X2,Y2
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 
 !## skip rectangles with zero width/heigth to avoid error messages
 IF(X1.EQ.X2.OR.Y1.EQ.Y2)RETURN
 
 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL IGRRECTANGLE(REAL(X1,4),REAL(Y1,4),REAL(X2,4),REAL(Y2,4))
  ELSE
   CALL IGRRECTANGLE(REAL(X1-OFFSETX,4),REAL(Y1-OFFSETY,4),REAL(X2-OFFSETX,4),REAL(Y2-OFFSETY,4))
  ENDIF
 ELSE
  CALL IGRRECTANGLE(REAL(X1,4),REAL(Y1,4),REAL(X2,4),REAL(Y2,4))
 ENDIF
 
 END SUBROUTINE DBL_IGRRECTANGLE

 !###====================================================================
 INTEGER FUNCTION DBL_IGRINSIDESHAPE(X,Y,POL) !,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y
 TYPE(POLINDOBJ),INTENT(INOUT) :: POL
! INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 
 DBL_IGRINSIDESHAPE=0
 SELECT CASE (POL%ITYPE)
  CASE (ID_POLYGON)
   IF(DBL_IGRINSIDEPOLYGON(X,Y,POL%X,POL%Y,POL%N).EQ.1)DBL_IGRINSIDESHAPE=1
  CASE (ID_RECTANGLE)
   POL%XMIN=MIN(POL%X(1),POL%X(2)); POL%XMAX=MAX(POL%X(1),POL%X(2))
   POL%YMIN=MIN(POL%Y(1),POL%Y(2)); POL%YMAX=MAX(POL%Y(1),POL%Y(2))
   IF(X.GE.POL%XMIN.AND.X.LE.POL%XMAX.AND.Y.GE.POL%YMIN.AND.Y.LE.POL%YMAX)DBL_IGRINSIDESHAPE=1
  CASE (ID_CIRCLE)
   IF(DBL_IGRINSIDECIRCLE(X,Y,POL%X,POL%Y,POL%N))DBL_IGRINSIDESHAPE=1
 END SELECT
 
 END FUNCTION DBL_IGRINSIDESHAPE
 
 !###====================================================================
 INTEGER FUNCTION DBL_IGRINSIDEPOLYGON(PX,PY,XD,YD,ND) !,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ND
 REAL(KIND=DP_KIND),DIMENSION(ND),INTENT(IN) :: XD,YD
! INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 INTEGER :: I
 REAL(KIND=DP_KIND),INTENT(IN) :: PX,PY
 REAL :: PSX,PSY
 REAL,DIMENSION(:),ALLOCATABLE :: XS,YS
 
 DBL_IGRINSIDEPOLYGON=0
! IF(PRESENT(IOFFSET))THEN
!  IF(IOFFSET.EQ.0)THEN
!   !## this one does not perform well for multiply overlapping line definitions
!   IF(UTL_INSIDEPOLYGON2(PX,PY,XD,YD,ND).EQ.1)DBL_IGRINSIDEPOLYGON=1
!  ELSE
!   IF(UTL_INSIDEPOLYGON2(PX,PY,XD,YD,ND).EQ.1)DBL_IGRINSIDEPOLYGON=1
!   !## if present if performs better with lines that overlap multiply times
!   ALLOCATE(XS(ND-1),YS(ND-1))
!   DO I=1,ND-1
!    XS(I)=REAL(XD(I)-OFFSETX,4)
!    YS(I)=REAL(YD(I)-OFFSETY,4)
!    WRITE(*,*) I,XS(I),YS(I)
!   ENDDO
!   PSX=REAL(PX-OFFSETX,4); PSY=REAL(PY-OFFSETY,4)
!   WRITE(*,*) PSX,PSY
!   PAUSE
!   IF(UTL_INSIDEPOLYGON2(PX,PY,XD,YD,ND).EQ.1)DBL_IGRINSIDEPOLYGON=1
!   IF(IGRINSIDEPOLYGON(XS,YS,ND-1,PSX,PSY))DBL_IGRINSIDEPOLYGON=1
!   DEALLOCATE(XS,YS)
!  ENDIF
! ELSE
 !## this one does not perform well for multiply overlapping line definitions
 IF(UTL_INSIDEPOLYGON2(PX,PY,XD,YD,ND).EQ.1)DBL_IGRINSIDEPOLYGON=1
! ENDIF
 
 END FUNCTION DBL_IGRINSIDEPOLYGON

 !###====================================================================
 LOGICAL FUNCTION DBL_IGRINSIDECIRCLE(PX,PY,XD,YD,ND)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ND
 REAL(KIND=DP_KIND),DIMENSION(ND),INTENT(IN) :: XD,YD
 REAL(KIND=DP_KIND),INTENT(IN) :: PX,PY
 REAL(KIND=DP_KIND) :: R1,R2
 
 DBL_IGRINSIDECIRCLE=.FALSE.

 R1=(XD(2)-XD(1))**2.0D0+(YD(2)-YD(1))**2.0D0
 IF(R1.GT.0.0D0)R1=SQRT(R1)

 R2=(PX-XD(1))**2.0D0+(PY-YD(1))**2.0D0
 IF(R2.GT.0.0D0)R2=SQRT(R2)

 IF(R2.LE.R1)DBL_IGRINSIDECIRCLE=.TRUE.

 END FUNCTION DBL_IGRINSIDECIRCLE

!double sum_theta = 0`
!for( int i = 0 ; i < n_vtx ; i++ )
!{
!    int    j         = (i + 1) % n_vtx;
!    XYPT   p_i       = ngon[i];
!    XYPT   p_j       = ngon[j];
!    XYPT   vector_i  = p_i - p_o;                 // vector from point o to vertex i
!    XYPT   vector_j  = p_j - p_o;                 // vector from point o to vertex j
!    double cp        = vector_i.cross( vector_j ) // cross product
!    double dp        = vector_i.dot  ( vector_j ) // dot   product
!           sum_theta+= Atan2( cp, dp );
!
!}
!if( sum > pi ) return INSIDE;
!else           return OUTSIDE;

 !###======================================================================
 LOGICAL function UTL_INSIDEPOLYGON3 ( n, x, y, x0, y0 )
 !###======================================================================
 IMPLICIT NONE
 INTEGER ( KIND = 4 ) N
 LOGICAL B
 INTEGER ( KIND = 4 ) I
 INTEGER ( KIND = 4 ) IP1
 REAL ( KIND = 8 ) T
 REAL ( KIND = 8 ) X(N)
 REAL ( KIND = 8 ) X0
 REAL ( KIND = 8 ) Y(N)
 REAL ( KIND = 8 ) Y0
  
 B = .FALSE.

 DO I = 1, N
   IP1 = MOD ( I, N ) + 1
   IF ( Y(IP1) < Y0 .EQV. Y0 <= Y(I) ) THEN
     T = X0 - X(I) - ( Y0 - Y(I) ) * ( X(IP1) - X(I) ) / ( Y(IP1) - Y(I) )
     IF ( T < 0.0D+00 ) THEN
       B = .NOT. B
     END IF
   END IF
 END DO

 UTL_INSIDEPOLYGON3 = B

 END FUNCTION UTL_INSIDEPOLYGON3

 !###======================================================================
 INTEGER FUNCTION UTL_INSIDEPOLYGON2(PX,PY,XX,YY,N)
 !###======================================================================
 !
 !code extracted from the internet:
 !http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html#Fortran%20Code%20for%20the%20Point%20in%20Polygon%20Test
 !
 !    PURPOSE TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON
 !
 !    DESCRIPTION OF THE PARAMETERS
 !       PX      - X-COORDINATE OF POINT IN QUESTION.
 !       PY      - Y-COORDINATE OF POINT IN QUESTION.
 !       XX      - N LONG VECTOR CONTAINING X-COORDINATES OF
 !                 VERTICES OF POLYGON.
 !       YY      - N LONG VECTOR CONTAING Y-COORDINATES OF
 !                 VERTICES OF POLYGON.
 !       N       - NUMBER OF VERTICES IN THE POLYGON.
 !       UTL_INSIDEPOLYGON   - THE SIGNAL RETURNED:
 !                 -1 IF THE POINT IS OUTSIDE OF THE POLYGON,
 !                  0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,
 !                  1 IF THE POINT IS INSIDE OF THE POLYGON.

 !    REMARKS
 !       THE VERTICES MAY BE LISTED CLOCKWISE OR ANTICLOCKWISE.
 !       THE FIRST MAY OPTIONALLY BE REPEATED, IF SO N MAY
 !       OPTIONALLY BE INCREASED BY 1.
 !       THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING
 !       OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX
 !       OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING
 !       N, THESE FIRST VERTICES MUST BE COUNTED TWICE.
 !       INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.
 !       THE SIZE OF THE ARRAYS MUST BE INCREASED IF N > MAXDIM
 !       WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 7/70.
 !
 !
 !    METHOD
 !       A VERTICAL LINE IS DRAWN THRU THE POINT IN QUESTION. IF IT
 !       CROSSES THE POLYGON AN ODD NUMBER OF TIMES, THEN THE
 !       POINT IS INSIDE OF THE POLYGON.
 !
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),INTENT(IN) :: PX,PY
 REAL(KIND=DP_KIND),DIMENSION(N),INTENT(IN) :: XX,YY
 REAL(KIND=DP_KIND),DIMENSION(N) :: X,Y
 LOGICAL :: MX,MY,NX,NY
 INTEGER :: I,J

 DO I=1,N
  X(I)=XX(I)-PX
  Y(I)=YY(I)-PY
 ENDDO
 UTL_INSIDEPOLYGON2=-1
 DO I=1,N
  J =1+MOD(I,N)
  MX=X(I).GE.0.0D0
  NX=X(J).GE.0.0D0
  MY=Y(I).GE.0.0D0
  NY=Y(J).GE.0.0D0
  IF(.NOT.((MY.OR.NY).AND.(MX.OR.NX)).OR.(MX.AND.NX))THEN
  ELSE
   IF(.NOT.(MY.AND.NY.AND.(MX.OR.NX).AND..NOT.(MX.AND.NX)))THEN
    IF(X(J)-X(I).NE.0.0D0)THEN
     IF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I)).LT.0.0D0)THEN
     ELSEIF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I)).EQ.0.0D0)THEN
      UTL_INSIDEPOLYGON2=0
      EXIT 
     ELSEIF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I)).GT.0.0D0)THEN
      UTL_INSIDEPOLYGON2=-UTL_INSIDEPOLYGON2
     ENDIF
    ENDIF
   ELSE
    UTL_INSIDEPOLYGON2=-UTL_INSIDEPOLYGON2
   ENDIF
  ENDIF
 ENDDO

 END FUNCTION UTL_INSIDEPOLYGON2

 !###====================================================================
 SUBROUTINE DBL_IPGUNITS(X1,Y1,X2,Y2) !,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X1,Y1,X2,Y2
! INTEGER,INTENT(IN),OPTIONAL :: IOFFSET

! IF(PRESENT(IOFFSET))THEN
!  IF(IOFFSET.EQ.0)THEN
!   CALL IPGUNITS(REAL(X1,4),REAL(Y1,4),REAL(X2,4),REAL(Y2,4))
!  ELSE
!   CALL IPGUNITS(REAL(X1-XOFFSET,4),REAL(Y1-YOFFSET,4)-YOFFSET,REAL(X2-XOFFSET,4),REAL(Y2-YOFFSET,4))
!  ENDIF
! ELSE
 CALL IPGUNITS(REAL(X1,4),REAL(Y1,4),REAL(X2,4),REAL(Y2,4))
! ENDIF
 
 END SUBROUTINE DBL_IPGUNITS

 !###====================================================================
 SUBROUTINE DBL_IGRMOVETO(X,Y,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 
 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL IGRMOVETO(REAL(X,4),REAL(Y,4))
  ELSE
   CALL IGRMOVETO(REAL(X-OFFSETX,4),REAL(Y-OFFSETY,4))
  ENDIF
 ELSE
  CALL IGRMOVETO(REAL(X,4),REAL(Y,4))
 ENDIF
 
 END SUBROUTINE DBL_IGRMOVETO

 !###====================================================================
 SUBROUTINE DBL_IGRPOLYGONSIMPLE(X,Y,N,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),DIMENSION(N),INTENT(IN) :: X,Y
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 
 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL IGRPOLYGONSIMPLE(REAL(X,4),REAL(Y,4),N)
  ELSE
   CALL IGRPOLYGONSIMPLE(REAL(X-OFFSETX,4),REAL(Y-OFFSETY,4),N)
  ENDIF
 ELSE
  CALL IGRPOLYGONSIMPLE(REAL(X,4),REAL(Y,4),N)
 ENDIF
 
 END SUBROUTINE DBL_IGRPOLYGONSIMPLE

 !###====================================================================
 SUBROUTINE DBL_IGRPOLYGONCOMPLEX(X,Y,N,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),DIMENSION(N),INTENT(IN) :: X,Y
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 
 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL IGRPOLYGONCOMPLEX(REAL(X,4),REAL(Y,4),N)
  ELSE
   CALL IGRPOLYGONCOMPLEX(REAL(X-OFFSETX,4),REAL(Y-OFFSETY,4),N)
  ENDIF
 ELSE
  CALL IGRPOLYGONCOMPLEX(REAL(X,4),REAL(Y,4),N)
 ENDIF
 
 END SUBROUTINE DBL_IGRPOLYGONCOMPLEX

 !###====================================================================
 SUBROUTINE DBL_IGRPOLYLINE(X,Y,N,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),DIMENSION(N),INTENT(IN) :: X,Y
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 
 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL IGRPOLYLINE(REAL(X,4),REAL(Y,4),N)
  ELSE
   CALL IGRPOLYLINE(REAL(X-OFFSETX,4),REAL(Y-OFFSETY,4),N)
  ENDIF
 ELSE
  CALL IGRPOLYLINE(REAL(X,4),REAL(Y,4),N)
 ENDIF
 
 END SUBROUTINE DBL_IGRPOLYLINE

 !###====================================================================
 SUBROUTINE DBL_IGRLINETO(X,Y,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 
 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL IGRLINETO(REAL(X,4),REAL(Y,4))
  ELSE
   CALL IGRLINETO(REAL(X-OFFSETX,4),REAL(Y-OFFSETY,4))
  ENDIF
 ELSE
  CALL IGRLINETO(REAL(X,4),REAL(Y,4))
 ENDIF
 
 END SUBROUTINE DBL_IGRLINETO

 !###====================================================================
 SUBROUTINE DBL_IGRJOIN(X1,Y1,X2,Y2,IOFFSET)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X1,Y1,X2,Y2
 INTEGER,INTENT(IN),OPTIONAL :: IOFFSET
 
 IF(PRESENT(IOFFSET))THEN
  IF(IOFFSET.EQ.0)THEN
   CALL IGRJOIN(REAL(X1,4),REAL(Y1,4),REAL(X2,4),REAL(Y2,4))
  ELSE
   CALL IGRJOIN(REAL(X1-OFFSETX,4),REAL(Y1-OFFSETY,4), &
                REAL(X2-OFFSETX,4),REAL(Y2-OFFSETY,4))
  ENDIF
 ELSE
  CALL IGRJOIN(REAL(X1,4),REAL(Y1,4),REAL(X2,4),REAL(Y2,4))
 ENDIF
 
 END SUBROUTINE DBL_IGRJOIN

END MODULE MOD_DBL