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
MODULE MOD_3D_UTL

USE IMODVAR, ONLY : IDIAGERROR
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_UTL, ONLY : UTL_MESSAGEHANDLE3D,UTL_MESSAGEHANDLE,IDFPLOT2BITMAP,RTOS,UTL_IDFSNAPTOGRID,UTL_LOADIMAGE,ITOS
USE MOD_IDF, ONLY : IDFDEALLOCATE,IDFREAD,IDFREADSCALE_GETX,IDFREADPART,IDFCOPY,IDFIROWICOL,IDFNULLIFY,IDFEQUAL,IDFGETLOC,IDFGETEDGE
USE MOD_DEMO_PAR
USE MOD_3D_PAR
USE MOD_MDF, ONLY : MDFDEALLOCATE,READMDF,MDF,WRITEMDF
USE MOD_IPFASSFILE, ONLY : IPFCLOSEASSFILE
USE MODPLOT, ONLY : MP,MPW,MXMPLOT
USE MOD_IPFGETVALUE_COLOURS, ONLY : IPFGETVALUE_GETCOLOURS
USE MOD_PLINES_TRACE, ONLY : TRACE_3D_CLOSE
USE MOD_PLINES_PAR, ONLY : PL
USE MOD_PLUGIN_PAR
USE MOD_SOLID_UTL, ONLY : SOLIDDEALLOCATESPF

CONTAINS
 
 !###======================================================================
 SUBROUTINE IMOD3D_SETNORMALVECTOR(P1,P2,P3)
 !###======================================================================
 IMPLICIT NONE
 REAL(GLFLOAT),INTENT(IN),DIMENSION(3) :: P1,P2,P3
 REAL(GLFLOAT),DIMENSION(3) :: NV
 REAL(GLFLOAT) :: QX,QY,QZ,PX,PY,PZ

 CALL IMOD3D_ERROR('IMOD3D_SETNORMALVECTOR_BEGIN')

 QX=P2(1)-P1(1); QY=P2(2)-P1(2); QZ=P2(3)-P1(3)
 PX=P3(1)-P1(1); PY=P3(2)-P1(2); PZ=P3(3)-P1(3)

 NV(1)=PY*QZ-PZ*QY
 NV(2)=PZ*QX-PX*QZ
 NV(3)=PX*QY-PY*QX
 CALL GLNORMAL3F(NV(1),NV(2),NV(3))

 CALL IMOD3D_ERROR('IMOD3D_SETNORMALVECTOR_END')

 END SUBROUTINE IMOD3D_SETNORMALVECTOR 
 
 !###======================================================================
 SUBROUTINE IMOD3D_GETMATRICES(MODELMATRIX,PROJMATRIX,VIEWPORT)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=GLDOUBLE),DIMENSION(16) :: MODELMATRIX,PROJMATRIX
 INTEGER(KIND=GLINT),DIMENSION(4) :: VIEWPORT

 CALL GLGETDOUBLEV(GL_MODELVIEW_MATRIX,MODELMATRIX)
 CALL GLGETDOUBLEV(GL_PROJECTION_MATRIX,PROJMATRIX)
 CALL GLGETINTEGERV(GL_VIEWPORT,VIEWPORT)

 END SUBROUTINE IMOD3D_GETMATRICES
 
 !###======================================================================
 SUBROUTINE IMOD3D_MAPOBJTOWINDOW()
 !###======================================================================
 IMPLICIT NONE
 INTEGER(KIND=GLINT) :: GP
 REAL(KIND=GLDOUBLE) :: OBJ,OBJY,OBJZ
 REAL(KIND=GLDOUBLE) :: WINX,WINY,WINZ
 REAL(KIND=GLDOUBLE),DIMENSION(16) :: MODELMATRIX,PROJMATRIX
 INTEGER(KIND=GLINT),DIMENSION(4) :: VIEWPORT

 !## get model-matrices
 CALL IMOD3D_GETMATRICES(MODELMATRIX,PROJMATRIX,VIEWPORT)

 !## maps object coordinates to window coordinates
 GP=GLUPROJECT(OBJ,OBJY,OBJZ,MODELMATRIX,PROJMATRIX,VIEWPORT,WINX,WINY,WINZ)

!Maps object coordinates to window coordinates. 
!
!INTEGER(kind=Glint) FUNCTION gluProject(
!  REAL(kind=GLdouble)                :: objx
!  REAL(kind=GLdouble)                :: objy
!  REAL(kind=GLdouble)                :: objz
!  REAL(kind=GLdouble), DIMENSION(16) :: modelMatrix
!  REAL(kind=GLdouble), DIMENSION(16) :: projMatrix
!  INTEGER(kind=GLint), DIMENSION(4)  :: viewport
!  REAL(kind=GLdouble)                :: winx
!  REAL(kind=GLdouble)                :: winy
!  REAL(kind=GLdouble)                :: winz
!)

 END SUBROUTINE IMOD3D_MAPOBJTOWINDOW

 !###======================================================================
 SUBROUTINE IMOD3D_MAPWINDOWTOOBJ(MESSAGE,X,Y,Z)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 REAL,INTENT(OUT) :: X,Y,Z
 INTEGER(KIND=GLINT) :: IPOSX,IPOSY
 REAL(KIND=GLFLOAT),DIMENSION(1) :: IPOSZ
 INTEGER(KIND=GLINT) :: GUP
 INTEGER(KIND=GLSIZEI),PARAMETER :: NDX=1,NDY=1  !## selection window
 REAL(KIND=GLDOUBLE) :: OBJX,OBJY,OBJZ
 REAL(KIND=GLDOUBLE) :: WINX,WINY,WINZ,clip_z,world_z
 REAL(KIND=GLDOUBLE),DIMENSION(16) :: MODELMATRIX,PROJMATRIX
 INTEGER(KIND=GLINT),DIMENSION(4) :: VIEWPORT

 !## get model-matrices
 CALL IMOD3D_GETMATRICES(MODELMATRIX,PROJMATRIX,VIEWPORT)

 !## get xy coordinates
 IPOSX=MESSAGE%X
 IPOSY=MESSAGE%Y

 !## flip y-coordinate
 IPOSY=WINFODIALOGFIELD(IDF_PICTURE2,FIELDHEIGHT)-IPOSY+1

 !## get the window-Z, perpendicular to the screen, the depth-component (0-1)
 CALL GLREADPIXELS(IPOSX,IPOSY,NDX,NDY,GL_DEPTH_COMPONENT,GL_FLOAT,IPOSZ)

 WINX=IPOSX
 WINY=IPOSY
 WINZ=IPOSZ(1)

! clip_z = (winz - 0.5) * 2.0
! world_z = 2.0*zfar*znear/(clip_z*((zfar-znear)-(zfar+znear)))

 !## maps window coordinates to object coordinates
 GUP=GLUUNPROJECT(WINX,WINY,WINZ,MODELMATRIX,PROJMATRIX,VIEWPORT,OBJX,OBJY,OBJZ)

 X=OBJX
 Y=OBJY
 Z=OBJZ
 
! winz=znear+(zfar-znear)*winz
! CALL WINDOWOUTSTATUSBAR(1,TRIM(ITOS(INT(IPOSX)))//','//TRIM(ITOS(INT(IPOSY)))//','//TRIM(RTOS(REAL(world_z),'F',2)))
! WRITE(*,*) WINX,WINY,WINZ
! WRITE(*,*) OBJX,OBJY,OBJZ
 
 CALL WINDOWSELECT(IWIN)
 IF(Z.GE.BOT%Z.AND.Z.LE.TOP%Z)THEN
  !## update pointer
  CALL WINDOWOUTSTATUSBAR(2,'Current Mouse 3D Position ('//TRIM(RTOS(X,'F',2))//' , '//TRIM(RTOS(Y,'F',2))//' , '//TRIM(RTOS(Z,'F',2))//')')
  INDPOS%X=X; INDPOS%Y=Y; INDPOS%Z=Z; IVALIDCROSS=1
 ELSE
  CALL WINDOWOUTSTATUSBAR(2,'Current Mouse 3D Position (unknown)')
  INDPOS%X=0.0_GLDOUBLE; INDPOS%Y=0.0_GLDOUBLE; INDPOS%Z=0.0_GLDOUBLE; IVALIDCROSS=0
 ENDIF

 IF(IDRAWCROSS.EQ.1.AND.IVALIDCROSS.EQ.1)THEN
  XYZCROSS(NXYZCROSS)%X=INDPOS%X
  XYZCROSS(NXYZCROSS)%Y=INDPOS%Y
 ENDIF
 
 END SUBROUTINE IMOD3D_MAPWINDOWTOOBJ
 
 !###======================================================================
 LOGICAL FUNCTION IMOD3D_DRAWIDF_SIZE(IDFC,IDFM)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,DIMENSION(5) :: NRCMOTHER
 TYPE(IDFOBJ),INTENT(IN) :: IDFC
 TYPE(IDFOBJ),INTENT(OUT) :: IDFM
 INTEGER :: IR1,IR2,IC1,IC2,NCOL,NROW,N,M,ICOL,IROW,I
 REAL :: X1,X2,Y1,Y2
 
 IMOD3D_DRAWIDF_SIZE=.FALSE. 
  
 IDFM%XMIN=MAX(IDFC%XMIN,REAL(BOT%X)) 
 IDFM%XMAX=MIN(IDFC%XMAX,REAL(TOP%X)) 
 IDFM%YMIN=MAX(IDFC%YMIN,REAL(BOT%Y)) 
 IDFM%YMAX=MIN(IDFC%YMAX,REAL(TOP%Y)) 
 IDFM%IEQ=IDFC%IEQ;   IDFM%ITB=IDFC%ITB;   IDFM%NODATA=IDFC%NODATA
 IDFM%IU=0;           IDFM%JD=IDFC%JD;     IDFM%ILAY=IDFC%ILAY
 IDFM%DMIN=IDFC%DMIN; IDFM%DMAX=IDFC%DMAX; IDFM%FNAME=IDFC%FNAME
 IDFM%TOP=IDFC%TOP;   IDFM%BOT=IDFC%BOT
 
 !## get position to be read from idf inside current view-extent
 CALL IDFIROWICOL(IDFC,IR1,IC1,IDFM%XMIN,IDFM%YMAX)
 CALL IDFIROWICOL(IDFC,IR2,IC2,IDFM%XMAX,IDFM%YMIN)

 !## adjust ic1,ic2
 IF(IDFM%XMIN.LE.IDFC%XMIN)IC1=1; IF(IDFM%XMAX.GE.IDFC%XMAX)IC2=IDFC%NCOL
 !## adjust ir1,ir2
 IF(IDFM%YMIN.LE.IDFC%YMIN)IR2=IDFC%NROW; IF(IDFM%YMAX.GE.IDFC%YMAX)IR1=1

 CALL IDFGETEDGE(IDFC,IR1,IC1,X1,Y1,X2,Y2)
 IF(X1.LT.IDFM%XMIN)IDFM%XMIN=X2 
 IF(Y2.GT.IDFM%YMAX)IDFM%YMAX=Y1 
 CALL IDFGETEDGE(IDFC,IR2,IC2,X1,Y1,X2,Y2)
 IF(X2.GT.IDFM%XMAX)IDFM%XMAX=X1 
 IF(Y1.LT.IDFM%YMIN)IDFM%YMIN=Y2 

 !## return, don't use mother idf
 IF(IR1.EQ.0.OR.IR2.EQ.0.OR.IC1.EQ.0.OR.IC2.EQ.0)RETURN
 
 NROW=IR2-IR1+1; NCOL=IC2-IC1+1

 !## return (outside, don't use mother idf)
 IF(NROW.LE.IDFDATA(1).AND.NCOL.LE.IDFDATA(2))RETURN

 IF(IDFM%IEQ.EQ.0)THEN
  N=NCOL/IDFDATA(1); IF(MOD(NCOL,IDFDATA(1)).NE.0)N=N+1
  M=NROW/IDFDATA(2); IF(MOD(NROW,IDFDATA(2)).NE.0)M=M+1
  IDFM%DX=IDFC%DX*REAL(N); IDFM%DY=IDFM%DX
  CALL UTL_IDFSNAPTOGRID(IDFM%XMIN,IDFM%XMAX,IDFM%YMIN,IDFM%YMAX,IDFM%DX,IDFM%NCOL,IDFM%NROW)
 ELSE
  !## use readpart() in case non-equidistantial IDF files are concerned
  RETURN
!  IF(ASSOCIATED(IDFM%SX))DEALLOCATE(IDFM%SX)
!  IF(ASSOCIATED(IDFM%SY))DEALLOCATE(IDFM%SY)
!  ALLOCATE(IDFM%SX(0:NCOL),IDFM%SY(0:NROW))
!  ICOL=IC1-1; IDFM%SX(0)=IDFC%SX(ICOL)
!  DO I=1,NCOL
!   ICOL=ICOL+1; IDFM%SX(I)=IDFC%SX(ICOL)
!  ENDDO
!  IROW=IR1-1; IDFM%SY(0)=IDFC%SY(IROW)
!  DO I=1,NROW
!   IROW=IROW+1; IDFM%SY(I)=IDFC%SY(IROW)
!  ENDDO
!  IDFM%XMIN=IDFM%SX(0); IDFM%XMAX=IDFM%SX(NCOL)
!  IDFM%YMAX=IDFM%SY(0); IDFM%YMIN=IDFM%SY(NROW)
!  IDFM%NCOL=NCOL; IDFM%NROW=NROW
 ENDIF

 IMOD3D_DRAWIDF_SIZE=.TRUE.

 END FUNCTION IMOD3D_DRAWIDF_SIZE

 !###======================================================================
 SUBROUTINE IMOD3D_SETCOLOR(IRGB,IALPHA)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IRGB
 INTEGER,INTENT(IN),OPTIONAL :: IALPHA
 INTEGER :: IR,IG,IB
 REAL(KIND=GLFLOAT) :: XR,XG,XB,ALPHA

 CALL WRGBSPLIT(IRGB,IR,IG,IB)
 XR=REAL(IR)/255.0_GLFLOAT; XG=REAL(IG)/255.0_GLFLOAT; XB=REAL(IB)/255.0_GLFLOAT
 IF(PRESENT(IALPHA))THEN
  ALPHA=REAL(IALPHA)/100.0
  CALL GLCOLOR4F(XR,XG,XB,ALPHA)
 ELSE
  CALL GLCOLOR3F(XR,XG,XB)
 ENDIF
 
 END SUBROUTINE IMOD3D_SETCOLOR

 !###======================================================================
 SUBROUTINE IMOD3D_RETURNCOLOR(IRGB,GLCOLOR)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IRGB
 REAL(KIND=GLFLOAT),DIMENSION(3),INTENT(OUT) :: GLCOLOR
 INTEGER :: IR,IG,IB

 CALL WRGBSPLIT(IRGB,IR,IG,IB)
 GLCOLOR(1)=REAL(IR)/255.0_GLFLOAT
 GLCOLOR(2)=REAL(IG)/255.0_GLFLOAT
 GLCOLOR(3)=REAL(IB)/255.0_GLFLOAT

 END SUBROUTINE IMOD3D_RETURNCOLOR

 !###======================================================================
 SUBROUTINE IMOD3D_GETCOLOR(IRGB,GLCOLOR)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IRGB
 REAL(KIND=GLFLOAT),DIMENSION(3),INTENT(IN) :: GLCOLOR
 INTEGER :: IR,IG,IB

 IR  =INT(GLCOLOR(1)*255.0_GLFLOAT)
 IG  =INT(GLCOLOR(2)*255.0_GLFLOAT)
 IB  =INT(GLCOLOR(3)*255.0_GLFLOAT)
 IRGB=WRGB(IR,IG,IB)

 END SUBROUTINE IMOD3D_GETCOLOR
 
 !###======================================================================
 LOGICAL FUNCTION IMOD3D_BMP()
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=GLFLOAT) :: X1,X2,Y1,Y2,Z,ZF,XT1,XT2,YT1,YT2,Z1,Z2,Z3,Z4, &
         ZOFFSET,MPWDX,MPWDY !DX,DY,
 INTEGER :: I,IIDF,IROW,ICOL
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 
 IMOD3D_BMP=.FALSE.

 !## get display-list pointers
 IF(.NOT.ALLOCATED(BMPLISTINDEX))ALLOCATE(BMPLISTINDEX(1)); BMPLISTINDEX=0

 !## destroy current display list index
 IF(BMPLISTINDEX(1).NE.0)CALL GLDELETELISTS(BMPLISTINDEX(1),1_GLSIZEI)
 BMPLISTINDEX(1)=GLGENLISTS(1); CALL GLNEWLIST(BMPLISTINDEX(1),GL_COMPILE)

 !## fill display current bitmap
 IF(.NOT.IMOD3D_BMP_INIT())RETURN

 !## turns on texturing
 CALL GLENABLE(GL_TEXTURE_2D)
 !## repeating texture in both directions
 CALL GLTEXPARAMETERI(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S, GL_REPEAT)
 CALL GLTEXPARAMETERI(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T, GL_REPEAT)
 !## magnification and minification method
 CALL GLTEXPARAMETERI(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST)
 CALL GLTEXPARAMETERI(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST)
 !##
 CALL GLHINT(GL_PERSPECTIVE_CORRECTION_HINT,GL_FASTEST)

 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB5)
 
 !## plot on idf
 IF(IACTBITMAP.EQ.1)THEN

  CALL WDIALOGGETMENU(IDF_MENU1,IIDF); IIDF=IDFPLOT(IIDF)%DISP_ILIST 
  ALLOCATE(IDF(2)); DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO
  IF(IMOD3D_BMP_READIDF(IIDF,IDF,SIZE(IDF)))THEN; ENDIF
  IF(IMOD3D_CREATE_SXY(IDF(1),IIDF))THEN; ENDIF
  IF(IMOD3D_BLANKOUT(IDF(1)))THEN; ENDIF  
  
  ZOFFSET=(TOP%Z-BOT%Z)/1000_GLFLOAT
  
  MPWDX=(MPW%XMAX-MPW%XMIN)
  MPWDY=(MPW%YMAX-MPW%YMIN)
  
  !## planes
  IF(IDFPLOT(IIDF)%ICUBE.EQ.1)THEN
   DO IROW=1,IDF(1)%NROW-1; DO ICOL=1,IDF(1)%NCOL-1
    IF(IDF(1)%X(ICOL  ,IROW)  .NE.IDF(1)%NODATA.AND. &
       IDF(1)%X(ICOL+1,IROW)  .NE.IDF(1)%NODATA.AND. &
       IDF(1)%X(ICOL+1,IROW+1).NE.IDF(1)%NODATA.AND. &
       IDF(1)%X(ICOL  ,IROW+1).NE.IDF(1)%NODATA)THEN
     !## connect 2d texture to 3d object (in this case top of cube)
     X1=IDF(1)%SX(ICOL); X2=IDF(1)%SX(ICOL+1)
     Y1=IDF(1)%SY(IROW); Y2=IDF(1)%SY(IROW+1)

     XT1=X1
     XT2=X2
     YT1=Y1
     YT2=Y2

     XT1=(XT1-MPW%XMIN)/MPWDX
     XT2=(XT2-MPW%XMIN)/MPWDX
     YT1=(YT1-MPW%YMIN)/MPWDY
     YT2=(YT2-MPW%YMIN)/MPWDY

     Z1=IDF(1)%X(ICOL,IROW); Z2=IDF(1)%X(ICOL+1,IROW)
     Z3=IDF(1)%X(ICOL+1,IROW+1); Z4=IDF(1)%X(ICOL,IROW+1)
     CALL GLBEGIN(GL_QUADS)
      CALL GLTEXCOORD2F(XT1,YT1); CALL GLVERTEX3F(X1,Y1,Z1+ZOFFSET)
      CALL GLTEXCOORD2F(XT2,YT1); CALL GLVERTEX3F(X2,Y1,Z2+ZOFFSET)
      CALL GLTEXCOORD2F(XT2,YT2); CALL GLVERTEX3F(X2,Y2,Z3+ZOFFSET)
      CALL GLTEXCOORD2F(XT1,YT2); CALL GLVERTEX3F(X1,Y2,Z4+ZOFFSET)
     CALL GLEND()
    
    ENDIF  
   ENDDO; ENDDO  
  !## cubes
  ELSE
   DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL 
    IF(IDF(1)%X(ICOL,IROW).NE.IDF(1)%NODATA)THEN
     !## connect 2d texture to 3d object (in this case top of cube)
     X1=IDF(1)%SX(ICOL-1); X2=IDF(1)%SX(ICOL)
     Y1=IDF(1)%SY(IROW-1); Y2=IDF(1)%SY(IROW)

     XT1=X1
     XT2=X2
     YT1=Y1
     YT2=Y2

     XT1=(XT1-MPW%XMIN)/MPWDX
     XT2=(XT2-MPW%XMIN)/MPWDX
     YT1=(YT1-MPW%YMIN)/MPWDY
     YT2=(YT2-MPW%YMIN)/MPWDY

     Z =IDF(1)%X(ICOL,IROW)
     CALL GLBEGIN(GL_QUADS)
      CALL GLTEXCOORD2F(XT1,YT1); CALL GLVERTEX3F(X1,Y1,Z+ZOFFSET)
      CALL GLTEXCOORD2F(XT1,YT2); CALL GLVERTEX3F(X1,Y2,Z+ZOFFSET)
      CALL GLTEXCOORD2F(XT2,YT2); CALL GLVERTEX3F(X2,Y2,Z+ZOFFSET)
      CALL GLTEXCOORD2F(XT2,YT1); CALL GLVERTEX3F(X2,Y1,Z+ZOFFSET)
     CALL GLEND()
    ENDIF
   ENDDO; ENDDO   
  ENDIF

  CALL IDFDEALLOCATE(IDF,SIZE(IDF))
 
 !## plot horizontally
 ELSEIF(IACTBITMAP.EQ.2)THEN

  MPWDX=MPW%XMAX-MPW%XMIN
  MPWDY=MPW%YMAX-MPW%YMIN

  !## get position of bitmap
  CALL WDIALOGGETTRACKBAR(IDF_TRACKBAR1,I); ZF=REAL(I)

  X1=BOT%X
  X2=TOP%X
  Y1=BOT%Y
  Y2=TOP%Y
  Z =BOT%Z+(TOP%Z-BOT%Z)*((100.0-ZF)/100.0)

  MPWDX=(MPW%XMAX-MPW%XMIN)
  MPWDY=(MPW%YMAX-MPW%YMIN)

  XT1=(BOT%X-MPW%XMIN)/MPWDX
  XT2=(TOP%X-MPW%XMIN)/MPWDX
  YT1=(BOT%Y-MPW%YMIN)/MPWDY
  YT2=(TOP%Y-MPW%YMIN)/MPWDY

  !## connect 2d texture to 3d object (in this case top of cube)
  CALL GLBEGIN(GL_QUADS)
   CALL GLTEXCOORD2F(XT1,YT1); CALL GLVERTEX3F(X1,Y1,Z)
   CALL GLTEXCOORD2F(XT1,YT2); CALL GLVERTEX3F(X1,Y2,Z)
   CALL GLTEXCOORD2F(XT2,YT2); CALL GLVERTEX3F(X2,Y2,Z)
   CALL GLTEXCOORD2F(XT2,YT1); CALL GLVERTEX3F(X2,Y1,Z)
  CALL GLEND()
 ENDIF
 
 CALL GLDISABLE(GL_TEXTURE_2D)
 CALL GLENDLIST()

 IMOD3D_BMP=.TRUE.

 CALL IMOD3D_ERROR('IMOD3D_BMP')

 END FUNCTION IMOD3D_BMP

 !###======================================================================
 LOGICAL FUNCTION IMOD3D_CREATE_SXY(IDF,IIDF) 
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER,INTENT(IN) :: IIDF
 INTEGER :: IROW,ICOL
 REAL,ALLOCATABLE,DIMENSION(:) :: SX,SY
   
 IMOD3D_CREATE_SXY=.FALSE.
 
 !## get sx/sy array - in case ieq=0
 IF(IDF%IEQ.EQ.0)THEN
  IF(ASSOCIATED(IDF%SX))DEALLOCATE(IDF%SX)
  IF(ASSOCIATED(IDF%SY))DEALLOCATE(IDF%SY)
  ALLOCATE(IDF%SX(0:IDF%NCOL)); ALLOCATE(IDF%SY(0:IDF%NROW))
  IDF%SX(0)=IDF%XMIN; DO ICOL=1,IDF%NCOL; IDF%SX(ICOL)=IDF%SX(ICOL-1)+IDF%DX; ENDDO
  IDF%SY(0)=IDF%YMAX; DO IROW=1,IDF%NROW; IDF%SY(IROW)=IDF%SY(IROW-1)-IDF%DY; ENDDO
 ENDIF
 !## planes
 IF(IDFPLOT(IIDF)%ICUBE.EQ.1)THEN
  !## construct cell mids
  ALLOCATE(SX(IDF%NCOL),SY(IDF%NROW))
  DO ICOL=1,IDF%NCOL; SX(ICOL)=(IDF%SX(ICOL-1)+IDF%SX(ICOL))/2.0; ENDDO
  DO IROW=1,IDF%NROW; SY(IROW)=(IDF%SY(IROW-1)+IDF%SY(IROW))/2.0; ENDDO
  DO ICOL=1,IDF%NCOL; IDF%SX(ICOL)=SX(ICOL); ENDDO
  DO IROW=1,IDF%NROW; IDF%SY(IROW)=SY(IROW); ENDDO
  DEALLOCATE(SX,SY)
 ENDIF

 IMOD3D_CREATE_SXY=.TRUE.
 
 END FUNCTION IMOD3D_CREATE_SXY

 !###======================================================================
 LOGICAL FUNCTION IMOD3D_BLANKOUT(IDF)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: IROW,ICOL,JROW,JCOL
 REAL :: X,Y
 
 IMOD3D_BLANKOUT=.TRUE.; IF(.NOT.ALLOCATED(IDF_CC))RETURN
 IMOD3D_BLANKOUT=.FALSE.

 !## blank in case cookie-cutter is used
 IF(IDFEQUAL(IDF,IDF_CC(1),0))THEN
  DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
   IF(IDF_CC(1)%X(ICOL,IROW).EQ.IDF_CC(1)%NODATA)IDF%X(ICOL,IROW)=IDF%NODATA  
  ENDDO; ENDDO
 ELSE
  DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
   CALL IDFGETLOC(IDF,IROW,ICOL,X,Y)
   CALL IDFIROWICOL(IDF_CC(1),JROW,JCOL,X,Y)
   IF(JCOL.NE.0.AND.JROW.NE.0)THEN
    IF(IDF_CC(1)%X(JCOL,JROW).EQ.IDF_CC(1)%NODATA)IDF%X(ICOL,IROW)=IDF%NODATA     
   ENDIF
  ENDDO; ENDDO
 ENDIF
  
 IMOD3D_BLANKOUT=.TRUE.
 
 END FUNCTION IMOD3D_BLANKOUT
 
 !###======================================================================
 LOGICAL FUNCTION IMOD3D_BLANKOUT_XY(X,Y)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: X,Y
 INTEGER :: IROW,ICOL,JROW,JCOL
 
 IMOD3D_BLANKOUT_XY=.TRUE.; IF(.NOT.ALLOCATED(IDF_CC))RETURN
 IMOD3D_BLANKOUT_XY=.FALSE.

 CALL IDFIROWICOL(IDF_CC(1),IROW,ICOL,X,Y)
 IF(IDF_CC(1)%X(ICOL,IROW).NE.IDF_CC(1)%NODATA)IMOD3D_BLANKOUT_XY=.TRUE.
 
 END FUNCTION IMOD3D_BLANKOUT_XY

 !###======================================================================
 LOGICAL FUNCTION IMOD3D_BMP_READIDF(IIDF,IDF,N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIDF,N
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(N) :: IDF 
 
 IMOD3D_BMP_READIDF=.FALSE.
 
 !## create mother if number of columns/rows to large
 IF(.NOT.IDFREAD(IDF(2),IDFPLOT(IIDF)%FNAME,0))RETURN
 !## template idf will become idf(1) based upon original idf(4)
 IF(IMOD3D_DRAWIDF_SIZE(IDF(2),IDF(1)))THEN
  IF(.NOT.IDFREADSCALE_GETX(IDF(2),IDF(1),IDFDATA(3),1,0.0))RETURN   !# child,mother,blockvalue,percentile
 ELSE
  !## copy idf(5) to idf(1) to become the original
  CALL IDFCOPY(IDF(2),IDF(1)); IDF(1)%IU=IDF(2)%IU
  !## read part of idf(1)
  IF(.NOT.IDFREADPART(IDF(1),REAL(BOT%X)+IDF(1)%DX,REAL(BOT%Y)+IDF(1)%DY, &
                             REAL(TOP%X)-IDF(1)%DX,REAL(TOP%Y)-IDF(1)%DY))RETURN
 ENDIF
 
 IMOD3D_BMP_READIDF=.TRUE.
 
 END FUNCTION IMOD3D_BMP_READIDF

 !###======================================================================
 LOGICAL FUNCTION IMOD3D_BMP_INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER(GLSIZEI) :: IWIDTH,IHEIGHT
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IBMPDATA
 INTEGER :: I,J,IW,IH,IOS
 REAL(KIND=GLFLOAT),ALLOCATABLE,DIMENSION(:) :: FRGB

 IMOD3D_BMP_INIT=.TRUE.
 !## okay, already processed!
 IF(IREADBMP.EQ.1.AND.ISOLID_3D.EQ.0)RETURN
 
 IMOD3D_BMP_INIT=.FALSE.

 CALL WINDOWSELECT(IWIN); CALL WINDOWOUTSTATUSBAR(2,'Reading Bitmap ...')

 IWIDTH =WINFOBITMAP(MPW%IBITMAP,BITMAPWIDTH)
 IHEIGHT=WINFOBITMAP(MPW%IBITMAP,BITMAPHEIGHT)
 ALLOCATE(IBMPDATA(IWIDTH*IHEIGHT),STAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot allocate enough memory IBMPDATA() to store background image','Error'); RETURN
 ENDIF

 ALPHA=0.75_GLFLOAT
 IALPHA=1

 ALLOCATE(FRGB(IWIDTH*IHEIGHT*(3+IALPHA)),STAT=IOS)
 IF(IOS.NE.0)THEN
  DEALLOCATE(IBMPDATA)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot allocate enough memory FRGB() to store background image','Error'); RETURN
 ENDIF

 CALL WBITMAPSAVE(MPW%IBITMAP,TRIM(PREFVAL(1))//'\TMP\TMP.BMP') 
 IF(.NOT.UTL_LOADIMAGE(TRIM(PREFVAL(1))//'\TMP\TMP.BMP',SIZE(IBMPDATA),IBMPDATA,0))THEN 
  DEALLOCATE(IBMPDATA,FRGB); RETURN
 ENDIF
 
 !## draw pixels at the current rasterposition
 J=-2-IALPHA
 DO IH=IHEIGHT,1,-1
  DO IW=1,IWIDTH
   J=J+3+IALPHA
   I=(IH-1)*IWIDTH+IW
   CALL IMOD3D_RETURNCOLOR(IBMPDATA(I),FRGB(J))
   !## mask out white, to be translucent (make pure black=background)
   IF(IALPHA.EQ.1)THEN
    FRGB(J+3)=ALPHA !## alpha value
   ENDIF
  ENDDO
 ENDDO

 !## turns on texturing
 CALL GLENABLE(GL_TEXTURE_2D)
 !## sets the drawing mode to GL_DECAL so that the textured
 !## polygons are drawn using the colors from the texture map (rather than taking into account what color the polygons
 !## would have been drawn without the texture)
 CALL GLTEXENVI(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE)
 CALL GLCOLOR4F(1.0_GLFLOAT,1.0_GLFLOAT,1.0_GLFLOAT,1.0_GLFLOAT)

 !## it describes how the bitmap data is stored in computer memory
 CALL GLPIXELSTOREI(GL_UNPACK_ALIGNMENT,1)

 !## parameters indicate the size of the image, type of the image, location of the image, and other properties of it
 IF(IALPHA.EQ.0)THEN
  CALL GLTEXIMAGE2D(GL_TEXTURE_2D,0_GLINT,3_GLINT,IWIDTH,IHEIGHT,0_GLINT,GL_RGB ,GL_FLOAT,FRGB)
 ELSE
  CALL GLTEXIMAGE2D(GL_TEXTURE_2D,0_GLINT,GL_RGBA,IWIDTH,IHEIGHT,0_GLINT,GL_RGBA,GL_FLOAT,FRGB)
 ENDIF

 DEALLOCATE(IBMPDATA,FRGB)
 IREADBMP=1
 
 IMOD3D_BMP_INIT=.TRUE.

 CALL WINDOWOUTSTATUSBAR(2,'')

 CALL IMOD3D_ERROR('IMOD3D_BMP_INIT')

 END FUNCTION IMOD3D_BMP_INIT

 !###======================================================================
 SUBROUTINE IMOD3D_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(IPFLISTINDEX))DEALLOCATE(IPFLISTINDEX)
 IF(ALLOCATED(IDFLISTINDEX))DEALLOCATE(IDFLISTINDEX)
 IF(ALLOCATED(IFFLISTINDEX))DEALLOCATE(IFFLISTINDEX)
 IF(ALLOCATED(SOLLISTINDEX))DEALLOCATE(SOLLISTINDEX)
 IF(ALLOCATED(GENLISTINDEX))DEALLOCATE(GENLISTINDEX)
 IF(ALLOCATED(BMPLISTINDEX))DEALLOCATE(BMPLISTINDEX)
 IF(ALLOCATED(STPLISTINDEX))DEALLOCATE(STPLISTINDEX)
 IF(ALLOCATED(PLLISTINDEX))DEALLOCATE(PLLISTINDEX)
 IF(ALLOCATED(IPFDLIST))DEALLOCATE(IPFDLIST)
 IF(ALLOCATED(IDFPLOT))DEALLOCATE(IDFPLOT)
 IF(ALLOCATED(IPFPLOT))DEALLOCATE(IPFPLOT)
 IF(ALLOCATED(IFFPLOT))DEALLOCATE(IFFPLOT)
 IF(ALLOCATED(GENPLOT))DEALLOCATE(GENPLOT)
 IF(ALLOCATED(SOLPLOT))DEALLOCATE(SOLPLOT)
 IF(ALLOCATED(CLPPLOT))DEALLOCATE(CLPPLOT)
 IF(ALLOCATED(NANSTRING))DEALLOCATE(NANSTRING)

 !## deallocate assf(.)
 CALL IPFCLOSEASSFILE()
 
 IF(ASSOCIATED(XYZCROSS))DEALLOCATE(XYZCROSS); NXYZCROSS=0; IDRAWCROSS=0
 
 END SUBROUTINE IMOD3D_DEALLOCATE

 !###======================================================================
 SUBROUTINE IMOD3D_CLOSE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IPLOT,IIPF,ILEG
 INTEGER(KIND=GLSIZEI) :: N
 
 IDIAGERROR=1
 
 CALL WINDOWSELECT(IWIN)
 
 IF(NIDFLIST.GT.0)THEN
  DO I=1,SIZE(IDFLISTINDEX)
   IF(IDFLISTINDEX(I).NE.0)THEN
!   N=NIDFLIST
    CALL GLDELETELISTS(IDFLISTINDEX(I),1_GLSIZEI)
    CALL WINDOWOUTSTATUSBAR(2,'Clearing memory for IDF '//TRIM(ITOS(I))//'...')
   ENDIF
  ENDDO
 ENDIF
 IF(NIFFLIST.GT.0)THEN
  DO I=1,SIZE(IFFLISTINDEX)
   IF(IFFLISTINDEX(I).NE.0)THEN
!  N=NIFFLIST
    CALL GLDELETELISTS(IFFLISTINDEX(I),1_GLSIZEI)
    CALL WINDOWOUTSTATUSBAR(2,'Clearing memory for IFF '//TRIM(ITOS(I))//'...')
   ENDIF
  ENDDO
 ENDIF
 IF(NIPFLIST.GT.0)THEN
  DO I=1,SIZE(IPFLISTINDEX,1); DO J=1,SIZE(IPFLISTINDEX,2)
   IF(IPFLISTINDEX(I,J).NE.0)THEN
    N=NIPFLIST
    CALL GLDELETELISTS(IPFLISTINDEX(I,J),1_GLSIZEI)
    CALL WINDOWOUTSTATUSBAR(2,'Clearing memory for IPF ...') !'//TRIM(ITOS(I))//'...')
   ENDIF
  ENDDO; ENDDO
 ENDIF
 IF(NGENLIST.GT.0)THEN
  DO I=1,SIZE(GENLISTINDEX)
   IF(GENLISTINDEX(I).NE.0)THEN
!  N=NGENLIST
    CALL GLDELETELISTS(GENLISTINDEX(I),1_GLSIZEI)
    CALL WINDOWOUTSTATUSBAR(2,'Clearing memory for GEN '//TRIM(ITOS(I))//'...')  
   ENDIF
  ENDDO
 ENDIF
 IF(NSOLLIST.GT.0)THEN
  DO I=1,SIZE(SOLLISTINDEX,1); DO J=1,SIZE(SOLLISTINDEX,2)
   IF(SOLLISTINDEX(I,J).NE.0)THEN
    CALL GLDELETELISTS(SOLLISTINDEX(I,J),1_GLSIZEI)
    CALL WINDOWOUTSTATUSBAR(2,'Clearing memory for SOL '//TRIM(ITOS(I))//'...')
   ENDIF
  ENDDO; ENDDO
 ENDIF
 IF(ALLOCATED(BMPLISTINDEX))THEN
  DO I=1,SIZE(BMPLISTINDEX)
   IF(BMPLISTINDEX(I).NE.0)THEN
    CALL GLDELETELISTS(BMPLISTINDEX(I),1_GLSIZEI)
    CALL WINDOWOUTSTATUSBAR(2,'Clearing memory for BMP '//TRIM(ITOS(I))//'...')
   ENDIF
  ENDDO
 ENDIF
 IF(ALLOCATED(STPLISTINDEX))THEN
  DO I=1,SIZE(STPLISTINDEX,1)
   IF(STPLISTINDEX(I).NE.0)THEN
    CALL GLDELETELISTS(STPLISTINDEX(I),1_GLSIZEI)
    CALL WINDOWOUTSTATUSBAR(2,'Clearing memory for Startpoints '//TRIM(ITOS(I))//'...')
   ENDIF
  ENDDO
 ENDIF
 IF(ALLOCATED(PLLISTINDEX))THEN
  DO I=1,SIZE(PLLISTINDEX,1)
   DO J=1,SIZE(PLLISTINDEX,2)
    IF(PLLISTINDEX(I,J).NE.0)THEN
     CALL GLDELETELISTS(PLLISTINDEX(I,J),1_GLSIZEI)
     CALL WINDOWOUTSTATUSBAR(2,'Clearing memory for Pathines '//TRIM(ITOS(I))//'...')
    ENDIF
   ENDDO
  ENDDO
 ENDIF

 IF(LEGENDINDEX.NE.0)CALL GLDELETELISTS(LEGENDINDEX,1_GLSIZEI)
 DO I=0,SIZE(AXESINDEX)-1; IF(AXESINDEX(I).NE.0)CALL GLDELETELISTS(AXESINDEX(I),1_GLSIZEI); ENDDO
 IF(SPHEREINDEX.NE.0)CALL GLDELETELISTS(SPHEREINDEX,1_GLSIZEI); SPHEREINDEX=0

 IF(ALLOCATED(IDF_CC))THEN; CALL IDFDEALLOCATE(IDF_CC,SIZE(IDF_CC)); DEALLOCATE(IDF_CC); ENDIF
 
 !## clear buffers
 CALL GLCLEAR(IOR(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
 
 !## disables openGL 
 CALL WGLSELECT(0)

 !## save legend adjustments
 IF(ALLOCATED(IPFPLOT))THEN
  CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB2)
  CALL WDIALOGGETMENU(IDF_MENU1,IPFPLOT%ISEL)
  IF(SUM(IPFPLOT%ISEL).EQ.1)THEN
   CALL WDIALOGGETMENU(IDF_MENU2,ILEG)
   CALL IPFGETVALUE_GETCOLOURS(ID_D3DSETTINGS_TAB2,ILEG)
  ENDIF

  DO I=1,SIZE(IPFPLOT)
   IPLOT=IPFPLOT(I)%IPLOT
   MP(IPLOT)%ASSCOL1=IPFPLOT(I)%ASSCOL1
   MP(IPLOT)%ASSCOL2=IPFPLOT(I)%ASSCOL2
   MP(IPLOT)%ILEGDLF=IPFPLOT(I)%ILEGDLF
  ENDDO
 ENDIF
 
 !## copy adjusted value to mp() and mdf file(s)
 CALL IMOD3D_COPYINFO()

 !## free memory
 CALL IMOD3D_DEALLOCATE()
 
 !## deallocate memory used by fench-diagrams
 CALL SOLIDDEALLOCATESPF()

 IF(IWIN.NE.0)CALL WINDOWCLOSECHILD(IWIN); IWIN=0

 !## unload settings dialog
 CALL WDIALOGSELECT(ID_D3DSETTINGS_RENDER); CALL WDIALOGUNLOAD()

 CALL UTL_MESSAGEHANDLE3D(1)
 !## turn all iMOD messages on
 CALL UTL_MESSAGEHANDLE(1)
 
 !## deallocate mdf (if available)
 CALL MDFDEALLOCATE()

 !## reset to entire window
 CALL IDFPLOT2BITMAP()

 CALL MAIN_TIMERS()
 
 IDIAGERROR=0

 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_3DTOOL,2,0)
 
 !## close interactive particle tracking as well
 IF(WMENUGETSTATE(ID_INTERACTIVEPATHLINES,2).EQ.1)CALL TRACE_3D_CLOSE()
 !## turn pathline simulation off
 PL%IRUN=0
 !## turn 3D off
 IRENDER_3D=0
 
 !## if demo-active, close iMOD as well
 IF(DEMO%INIT.EQ.1)THEN; CALL WINDOWCLOSE(); STOP; ENDIF
 
 CALL WINDOWSELECT(0); CALL WINDOWSIZEPOS(ISTATE=WINMAXIMISED )
 
 END SUBROUTINE IMOD3D_CLOSE

 !###======================================================================
 SUBROUTINE IMOD3D_COPYINFO()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT,I,N

 !## fill dialog with information IDF
 !## open idf files (*.idf,*.mdf)
 NIDFLIST=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.(MP(IPLOT)%IPLOT.EQ.1.OR.MP(IPLOT)%IPLOT.EQ.5))THEN
   !## get idf for mdf file
   IF(MP(IPLOT)%IPLOT.EQ.5)THEN
    IF(READMDF(MP(IPLOT)%IDFNAME,N))THEN
     DO I=1,N
      NIDFLIST=NIDFLIST+1
      MDF(I)%LEG       =IDFPLOT(NIDFLIST)%LEG
      MDF(I)%SCOLOR    =IDFPLOT(NIDFLIST)%ICOLOR
     ENDDO
     IF(.NOT.WRITEMDF(MP(IPLOT)%IDFNAME,N))THEN
     ENDIF
     CALL MDFDEALLOCATE()
    ENDIF
   ELSE
    NIDFLIST=NIDFLIST+1
    MP(IPLOT)%SCOLOR =IDFPLOT(NIDFLIST)%ICOLOR
   ENDIF
  ENDIF
 ENDDO

 END SUBROUTINE IMOD3D_COPYINFO
 
 !###======================================================================
 SUBROUTINE IMOD3D_GET_HEADING_TILE(LENXY,LENXZ)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=GLDOUBLE),INTENT(OUT),OPTIONAL :: LENXY,LENXZ
 REAL(KIND=GLDOUBLE) :: DX,DY,DZ,DXY

 !## initiate view-directions
 DX=LOOKAT%X-LOOKFROM%X; DY=LOOKAT%Y-LOOKFROM%Y; DZ=LOOKAT%Z-LOOKFROM%Z
 IF(PRESENT(LENXY))LENXY=SQRT(DX**2.0+DY**2.0) 
 HEADING=ATAN2(DY,DX)
 DXY=SQRT(DX**2.0+DY**2.0)
 TILT=ATAN2(DZ,DXY)
 IF(PRESENT(LENXZ))LENXZ=SQRT(DXY**2.0+DZ**2.0)
 
 END SUBROUTINE IMOD3D_GET_HEADING_TILE
 
END MODULE MOD_3D_UTL

