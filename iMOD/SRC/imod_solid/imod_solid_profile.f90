!!  Copyright (C) Stichting Deltares, 2005-2014.
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
MODULE MOD_SOLID_PROFILE

USE WINTERACTER
USE RESOURCE
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_SOLID_PAR
USE MOD_IDF, ONLY : IDFREAD,IDFGETXYVAL
USE MOD_UTL, ONLY : UTL_CAP,ITOS,UTL_SETTEXTSIZE,INVERSECOLOUR,PEUCKER_SIMPLIFYLINE,UTL_INSIDEPOLYGON,UTL_FILLARRAY,UTL_EQUALS_REAL
USE MOD_SOLID_UTL, ONLY : SOLIDOPENSOL,GETSOLNAME
USE MOD_PROF_PAR, ONLY : NXY,XY,ISOLID,IBITMAP,IWINPROFILE,MXNIDF,PROFIDF,SERIE,XMIN,YMIN,XMAX,YMAX,PBITMAP
USE MOD_PROFILE_UTL, ONLY : GRAPHUNITS,GRAPHAREA,PROFILE_EXTENT_GRAPH,PROFILE_GETLOCATION
USE MOD_IPF_PAR, ONLY : NIPF,IPF
USE MOD_IPFASSFILE, ONLY : IPFCLOSEASSFILE,IPFDIMENSIONASSFILE,ASSF,IPFASSFILEALLOCATE 
USE MOD_IPF, ONLY : IPFPLOTLABEL
USE MOD_POLYGON_PAR

INTEGER :: ILOCK,ISSNAP,IFIND

CONTAINS

 !###======================================================================
 SUBROUTINE SOLIDPROFILEFIT(INEW)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: INEW !## 1=new, 0=adjust
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I,J,N,ICOL,IROW,IWINID
 LOGICAL :: LEX
 
 IF(ISPF.LE.0.AND.NXY.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to draw a cross-section first.','Error'); RETURN
 ENDIF
 
 CALL WDIALOGSELECT(ID_DSERIESTAB2)
 CALL WDIALOGGETMENU(IDF_MENU1,ISPF)
 
 !## copy settings for background-bitmap
 IF(ISPF.GT.0.AND.ISPF.LE.SIZE(SPF))THEN 
  !## get correct x- and y-coordinates
  CALL PROFILE_EXTENT_GRAPH(1)
  CALL IGRUNITSFROMPIXELS(PBITMAP%IX1,PBITMAP%IY1,PBITMAP%GX1,PBITMAP%GY1,IORIGIN=1)
  CALL IGRUNITSFROMPIXELS(PBITMAP%IX2,PBITMAP%IY2,PBITMAP%GX2,PBITMAP%GY2,IORIGIN=1)
  SPF(ISPF)%PBITMAP=PBITMAP
 ENDIF

 !## load solid-fit dialog
 CALL WDIALOGLOAD(ID_DSOLID_FIT,ID_DSOLID_FIT)
 
 IF(WINFOGRID(IDF_GRID1,GRIDROWSMAX).LT.NTBSOL+1)THEN
  CALL WDIALOGUNLOAD()
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not store '//TRIM(ITOS(N))//' rows in grid','Error')
  RETURN
 ENDIF
 
 IF(NSPF.GT.0)CALL WDIALOGPUTMENU(IDF_MENU1,SPF%FNAME,NSPF,ISPF)
 CALL WDIALOGFIELDSTATE(IDF_MENU1,INEW)
 
 N=NTBSOL+1; CALL WGRIDROWS(IDF_GRID1,N)
 ALLOCATE(ISEL_IDF(N),IACT(N),ICLEAN(N),XEXCLUDE(N),IEXIST(N),DTOL(N))

 !## 1 meter
 DTOL=1.0; XEXCLUDE=-999.99; IEXIST=1
 DO I=2,MIN(N,MXNIDF); XEXCLUDE(I)=PROFIDF(I-1)%IDF%NODATA; ENDDO
 IACT=1; ICLEAN=1; ISEL_IDF=1
 DO I=1,MIN(MXNIDF,N-1); ISEL_IDF(I+1)=I; ENDDO
 CALL WGRIDLABELROW(IDF_GRID1,1,'All SolidLayers')
 CALL WGRIDSTATECELL(IDF_GRID1,4,1,2)
 J=1
 DO I=1,NTBSOL
  IF(MOD(I,2).EQ.0)THEN
   CALL WGRIDLABELROW(IDF_GRID1,I+1,'('//TRIM(ITOS(I))//') Bottom Layer '//TRIM(ITOS(J)))
   J=J+1
  ENDIF
  IF(MOD(I,2).NE.0)CALL WGRIDLABELROW(IDF_GRID1,I+1,'('//TRIM(ITOS(I))//') Top Layer '//TRIM(ITOS(J)))
 ENDDO
 
 !## correct for npos, adjust mode
 IF(INEW.EQ.0)THEN
  DO I=1,NTBSOL
   IF(SPF(ISPF)%PROF(I)%NPOS.EQ.0)THEN; IEXIST(I+1)=0; IACT(I+1)=0; ENDIF
  ENDDO
 ENDIF
 
 !## no idf files active, deactivate "fit" column and "idf" column
 IF(MXNIDF.LE.0)THEN
  IACT=0
  CALL WGRIDSTATE(IDF_GRID1,2,0)
  CALL WGRIDSTATE(IDF_GRID1,4,0)
 ELSE
  CALL WGRIDPUTMENU(IDF_GRID1,4,PROFIDF%ALIAS,MXNIDF,ISEL_IDF,N)
 ENDIF 

 CALL WGRIDPUTCHECKBOX(IDF_GRID1,1,IEXIST,N) !## define
 CALL WGRIDPUTCHECKBOX(IDF_GRID1,2,IACT,N)   !## fit
 CALL WGRIDPUTCHECKBOX(IDF_GRID1,3,ICLEAN,N) !## clean first
 CALL WGRIDPUTREAL(IDF_GRID1,5,DTOL,N)
 CALL WGRIDPUTREAL(IDF_GRID1,6,XEXCLUDE,N)
 CALL WGRIDSETCELL(IDF_GRID1,1,2)
 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE(ITYPE)
   
   CASE(FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)

     CASE (IDF_GRID1)
      IF(MESSAGE%VALUE1.EQ.MESSAGE%VALUE2)THEN
      CALL WDIALOGSELECT(ID_DSOLID_FIT)
      
      CALL WGRIDPOS(MESSAGE%X,ICOL,IROW)
      IF(IROW.EQ.1)THEN
       IF(ICOL.EQ.5)THEN
        CALL WGRIDGETREAL(IDF_GRID1,ICOL,DTOL,N)    
        DTOL(2:N)=DTOL(1)
        CALL WGRIDPUTREAL(IDF_GRID1,ICOL,DTOL,N)
       ELSEIF(ICOL.EQ.6)THEN
        CALL WGRIDGETREAL(IDF_GRID1,ICOL,XEXCLUDE,N)    
        XEXCLUDE(2:N)=XEXCLUDE(1)
        CALL WGRIDPUTREAL(IDF_GRID1,ICOL,XEXCLUDE,N)
       ENDIF
      ENDIF

      CALL WGRIDPOS(MESSAGE%Y,ICOL,IROW)
      IF(IROW.EQ.1)THEN
       SELECT CASE (ICOL)
        CASE (1)
         CALL WGRIDGETCHECKBOX(IDF_GRID1,ICOL,IEXIST,N)
         IEXIST(2:N)=IEXIST(1)
         CALL WGRIDPUTCHECKBOX(IDF_GRID1,1,IEXIST,N)
         CALL WGRIDPUTCHECKBOX(IDF_GRID1,2,IEXIST,N)
         CALL WGRIDPUTCHECKBOX(IDF_GRID1,3,IEXIST,N)
        CASE (2) 
         CALL WGRIDGETCHECKBOX(IDF_GRID1,ICOL,IACT,N)
         IACT(2:N)=IACT(1)
         CALL WGRIDPUTCHECKBOX(IDF_GRID1,ICOL,IACT,N)
        CASE (3)
         CALL WGRIDGETCHECKBOX(IDF_GRID1,ICOL,ICLEAN,N)
         ICLEAN(2:N)=ICLEAN(1)
         CALL WGRIDPUTCHECKBOX(IDF_GRID1,ICOL,ICLEAN,N)
        CASE (4)
         CALL WGRIDGETMENU(IDF_GRID1,ICOL,ISEL_IDF,N)
         ISEL_IDF(2:N)=ISEL_IDF(1)
         CALL WGRIDPUTMENU(IDF_GRID1,ICOL,PROFIDF%ALIAS,MXNIDF,ISEL_IDF,N)
       END SELECT
      ELSE
       IF(ICOL.EQ.1)THEN
        CALL WGRIDGETCELLCHECKBOX(IDF_GRID1,1,IROW,I)
        CALL WGRIDPUTCELLCHECKBOX(IDF_GRID1,2,IROW,I)
        CALL WGRIDPUTCELLCHECKBOX(IDF_GRID1,3,IROW,I)
       ENDIF
      ENDIF
      ENDIF
    END SELECT

   CASE(PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDOK)
      CALL WGRIDGETCHECKBOX(IDF_GRID1,1,IEXIST,N)
      LEX=.TRUE.
      IF(INEW.EQ.1)LEX=SOLIDPROFILEADD() !## add line based on iexist()
      IF(LEX)THEN
       CALL WGRIDGETCHECKBOX(IDF_GRID1,2,IACT,N)
       IACT=IACT*IEXIST
       CALL WGRIDGETCHECKBOX(IDF_GRID1,3,ICLEAN,N)
       CALL WGRIDGETMENU(IDF_GRID1,4,ISEL_IDF,N)
       CALL WGRIDGETREAL(IDF_GRID1,5,DTOL,N)
       CALL WGRIDGETREAL(IDF_GRID1,6,XEXCLUDE,N)
       CALL SOLIDPROFILEFITDRILL_CALC()
       EXIT
      ENDIF
     CASE (IDCANCEL)
      EXIT
     CASE (IDHELP)
       CALL IMODGETHELP('5.4.2','TMO.ST.CrossSec')
    END SELECT
  END SELECT

 ENDDO

 DEALLOCATE(ISEL_IDF,IACT,DTOL,ICLEAN,XEXCLUDE,IEXIST)

 CALL WDIALOGSELECT(ID_DSOLID_FIT)
 CALL WDIALOGUNLOAD()
 ISOLID=1

 END SUBROUTINE SOLIDPROFILEFIT

 !###======================================================================
 SUBROUTINE SOLIDPROFILEFITDRILL_CALC()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IE,IS,IELEV,IIDF,N
 REAL :: DZ
 
 DO I=1,NTBSOL
  
  !## remove lines
  IF(IEXIST(I+1).EQ.0)THEN
   SPF(ISPF)%PROF(I)%NPOS=0
   SPF(ISPF)%PROF(I)%PX=0.0
   SPF(ISPF)%PROF(I)%PZ=0.0
   CYCLE
  ENDIF
    
  !## clean entire line
  IF(ICLEAN(I+1).EQ.1)THEN
   SPF(ISPF)%PROF(I)%NPOS=0
   SPF(ISPF)%PROF(I)%PX=0.0
   SPF(ISPF)%PROF(I)%PZ=0.0
  ENDIF

  !## perform fitting
  IF(IACT(I+1).EQ.1)THEN
   IIDF=ISEL_IDF(I+1)
   
   !## process each line segment in between nodata points (if they exist)  
   IS=0
   DO 
    IS=IS+1
    !## found start of connected piece of the line without nodata
    IF(SERIE(IIDF)%Y(IS).NE.XEXCLUDE(I+1).AND.SERIE(IIDF)%Y(IS).NE.PROFIDF(IIDF)%IDF%NODATA)THEN
     IE=IS
     DO 
      IE=IE+1
      !## found start of connected piece of the line without nodata         
      IF(IE.GE.SERIE(IIDF)%N.OR. &   !## final reached for line
         SERIE(IIDF)%Y(IE).EQ.XEXCLUDE(I+1).OR. &
         SERIE(IIDF)%Y(IE).EQ.PROFIDF(IIDF)%IDF%NODATA)THEN 
       !## not final of line --- do not use nodata point in that case
       IF(IE.LT.SERIE(IIDF)%N)IE=IE-1
       
       N=IE-IS+1
       ALLOCATE(GCODE(N))
       !## process line       
       CALL PEUCKER_SIMPLIFYLINE(SERIE(IIDF)%X(IS:),SERIE(IIDF)%Y(IS:),GCODE,N)

       IF(SPF(ISPF)%PROF(I)%NPOS.EQ.0)THEN
        !## reformulate begin
        SPF(ISPF)%PROF(I)%PX(1)=SERIE(IIDF)%X(IS)
        SPF(ISPF)%PROF(I)%PZ(1)=SERIE(IIDF)%Y(IS)
        !## reformulate end point
        SPF(ISPF)%PROF(I)%PX(2)=SERIE(IIDF)%X(IE)
        SPF(ISPF)%PROF(I)%PZ(2)=SERIE(IIDF)%Y(IE)
        SPF(ISPF)%PROF(I)%NPOS=2
       ELSE
        CALL SOLIDPROFILEPUTINTERSECTION(SERIE(IIDF)%X(IS),SERIE(IIDF)%Y(IS),I)
        CALL SOLIDPROFILEPUTINTERSECTION(SERIE(IIDF)%X(IE),SERIE(IIDF)%Y(IE),I)
       ENDIF

       !## add intermediate point, take into account maximum array-size
       DO J=2,MIN(N-1,SIZE(SPF(ISPF)%PROF(I)%PX))
        !## use point from Urs-Douglas-Peucker algorithm (greater then given tolerance)
        IF(GCODE(J).GT.DTOL(I+1))THEN
         !## put connection of current dril to 
         CALL SOLIDPROFILEPUTINTERSECTION(SERIE(IIDF)%X(IS+J-1),SERIE(IIDF)%Y(IS+J-1),I)
        ENDIF
       ENDDO
       DEALLOCATE(GCODE)

       IS=IE
       EXIT
      ENDIF
     ENDDO
    ENDIF
    IF(IS.GE.SERIE(IIDF)%N)EXIT
   ENDDO
   
  ELSE
   IF(ICLEAN(I+1).EQ.1)THEN
    !## mean vertical depth as first guess!
    DZ=(GRAPHUNITS(4,1)-GRAPHUNITS(2,1))/REAL(NTBSOL)
    SPF(ISPF)%PROF(I)%NPOS=2
    SPF(ISPF)%PROF(I)%PX(1)=0.0
    SPF(ISPF)%PROF(I)%PX(2)=SPF(ISPF)%TX
    SPF(ISPF)%PROF(I)%PZ(1)=GRAPHUNITS(4,1)-DZ*REAL(I-1) !(GRAPHUNITS(4,1)+GRAPHUNITS(2,1))/2.0
    SPF(ISPF)%PROF(I)%PZ(2)=SPF(ISPF)%PROF(I)%PZ(1)
   ENDIF
  ENDIF
 ENDDO
  
 END SUBROUTINE SOLIDPROFILEFITDRILL_CALC
 
 !###======================================================================
 SUBROUTINE SOLIDPROFILEFITDRILL()
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: MINDIST=1.0 !## minimum distance 10 meter
 INTEGER :: IIPF,I,NFOUND,IELEV,N
 REAL :: X,Y,Z,LENPROF
 CHARACTER(LEN=256) :: FNAME,DIR
 
 NFOUND=0
 
 !## allocate and nullify pointers
 CALL IPFASSFILEALLOCATE(1)

 !## reset entire cross-sectional lines
 DO IELEV=1,SIZE(SPF(ISPF)%PROF)
  N=SPF(ISPF)%PROF(IELEV)%NPOS
  SPF(ISPF)%PROF(IELEV)%NPOS=2
  SPF(ISPF)%PROF(IELEV)%PX(1)=0.0    
  SPF(ISPF)%PROF(IELEV)%PX(2)=SPF(ISPF)%PROF(IELEV)%PX(N)
 ENDDO

 LENPROF=SPF(ISPF)%PROF(1)%PX(2)

 DO IIPF=1,NIPF
  IF(IPF(IIPF)%ACOL.NE.0)THEN
   I  =INDEXNOCASE(IPF(IIPF)%FNAME,'\',.TRUE.)
   DIR=IPF(IIPF)%FNAME(1:I-1)
   DO I=1,IPF(IIPF)%NROW
    IF(IPF(IIPF)%IPOS(I).EQ.INT(1,1))THEN
     X=IPF(IIPF)%XYPOS(1,I) !## x in profile
     IF(X-LENPROF.LE.MINDIST)X=MIN(LENPROF,X)
     Y=IPF(IIPF)%XYPOS(2,I) !## top-z in profile
     FNAME=TRIM(DIR)//'\'//TRIM(IPF(IIPF)%INFO(IPF(IIPF)%ACOL,I))//'.'//TRIM(ADJUSTL(IPF(IIPF)%FEXT))
     !## read dimensions of associated file and read it!
     CALL IPFDIMENSIONASSFILE(1,FNAME,IPF(IIPF)%IAXES)
     ASSF(1)%ASSCOL1=IPF(IIPF)%ASSCOL1 !## column used with dlf
     ASSF(1)%ASSCOL2=IPF(IIPF)%ASSCOL2 !## on default not used --- border rings
     !## equal to drills --- activate current locations...     
     IF(ASSF(1)%ITOPIC.EQ.2)THEN
      DO IELEV=1,MIN(NTBSOL,ASSF(1)%NRASS)
       Z=ASSF(1)%Z(IELEV)      
       !## put connection of current dril to 
       CALL SOLIDPROFILEPUTINTERSECTION(X,Z,IELEV)
      ENDDO
     ENDIF
    ENDIF
   ENDDO
  ENDIF
 ENDDO

 CALL IPFCLOSEASSFILE()

 END SUBROUTINE SOLIDPROFILEFITDRILL
 
 !###======================================================================
 SUBROUTINE SOLIDPROFILEINTERSECT(DXB,DYB)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: DXB,DYB
 INTEGER :: I,J,K,JSPF,IS
 REAL,DIMENSION(2) :: X1,Y1,X2,Y2
 REAL :: XI,YI,DX,Z
 CHARACTER(LEN=52) :: ALIAS
 
 IF(ISPF.LE.0)RETURN
 
 !## intersect all lines for the current cross-section (ispf)
 DO I=1,SIZE(SPF(ISPF)%PROF)
  !## try to intersect first segment of line i from cross-section ispf
  DO J=2,SPF(ISPF)%NXY
   X1(1)=SPF(ISPF)%X(J-1)
   Y1(1)=SPF(ISPF)%Y(J-1)
   X1(2)=SPF(ISPF)%X(J)
   Y1(2)=SPF(ISPF)%Y(J)
   !## intersect with other segments ...
   DO JSPF=1,NSPF
    IF(JSPF.NE.ISPF)THEN
     DO K=2,SPF(JSPF)%NXY
      X2(1)=SPF(JSPF)%X(K-1)
      Y2(1)=SPF(JSPF)%Y(K-1)
      X2(2)=SPF(JSPF)%X(K)
      Y2(2)=SPF(JSPF)%Y(K)
      IF((X1(1).EQ.X2(1).AND.Y1(1).EQ.Y2(1)))THEN
       IS=5; XI=X1(1); YI=Y1(1)
      ELSEIF((X1(1).EQ.X2(2).AND.Y1(1).EQ.Y2(2)))THEN
       IS=5; XI=X1(1); YI=Y1(1)
      ELSEIF((X1(2).EQ.X2(1).AND.Y1(2).EQ.Y2(1)))THEN
       IS=5; XI=X1(2); YI=Y1(2)
      ELSEIF((X1(2).EQ.X2(2).AND.Y1(2).EQ.Y2(2)))THEN
       IS=5; XI=X1(2); YI=Y1(2)
      ELSE
       CALL IGRINTERSECTLINE(X1(1),Y1(1),X1(2),Y1(2),&
                             X2(1),Y2(1),X2(2),Y2(2), &
                             XI,YI,IS)
      ENDIF
      !## they intersect
      IF(IS.EQ.5)THEN
       !## get distance on other segment
       DX=SOLIDPROFILEDISTANCE(K,JSPF,XI,YI)
       !## get value
       IF(SOLIDPROFILEZVALUE(DX,JSPF,I,Z))THEN
        !## get distance on current segment
        DX=SOLIDPROFILEDISTANCE(J,ISPF,XI,YI)

        CALL IGRRECTANGLE(DX-(0.5*DXB),Z-(0.5*DYB),DX+(0.5*DXB),Z+(0.5*DYB))

        IF(ALLOCATED(PROFIDF))THEN   
         IF(I.LE.SIZE(PROFIDF))THEN        
          ALIAS=PROFIDF(I)%IDF%FNAME(INDEX(PROFIDF(I)%IDF%FNAME,'\',.TRUE.)+1:)
          CALL WGRTEXTSTRING(DX+DXB,Z,TRIM(SPF(JSPF)%FNAME)//';'//TRIM(ALIAS)) 
         ELSE
          CALL WGRTEXTSTRING(DX+DXB,Z,TRIM(SPF(JSPF)%FNAME)//';Elevation:'//TRIM(ITOS(I))) 
         ENDIF
        ELSE
         CALL WGRTEXTSTRING(DX+DXB,Z,TRIM(SPF(JSPF)%FNAME)//';Elevation:'//TRIM(ITOS(I))) 
        ENDIF
!       !## put intersection value to current isp-cross-section
!       CALL SOLIDPROFILEPUTINTERSECTION(DX,Z,I)
       ENDIF
       EXIT
      ENDIF   
     ENDDO
    ENDIF
   ENDDO
  ENDDO
 ENDDO

 END SUBROUTINE SOLIDPROFILEINTERSECT

 !###======================================================================
 SUBROUTINE SOLIDPROFILEPUTINTERSECTION(X,Z,IL)
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: MINDIST=1.0 !## minimum distance 10 meter
 REAL,INTENT(IN) :: X,Z
 INTEGER,INTENT(IN) :: IL
 INTEGER :: I,ICRD,N
 LOGICAL :: LEX
  
 N=SPF(ISPF)%PROF(IL)%NPOS
 
 !## add point, could be zero if nodes are removed
 LEX=.FALSE.
 IF(N.LE.1)THEN
  LEX=.TRUE.
 ELSE
  LEX=X.GT.SPF(ISPF)%PROF(IL)%PX(N)
 ENDIF

 !## add point at the end or beginning of the line
 IF(LEX)THEN !X.GT.SPF(ISPF)%PROF(IL)%PX(N))THEN
  N=N+1
  IF(N.EQ.1)THEN
   SPF(ISPF)%PROF(IL)%PX(N)=X
   SPF(ISPF)%PROF(IL)%PZ(N)=Z
   SPF(ISPF)%PROF(IL)%NPOS=N
  ELSE
   IF(X.LT.SPF(ISPF)%PROF(IL)%PX(N-1))THEN
    SPF(ISPF)%PROF(IL)%PX(N)=SPF(ISPF)%PROF(IL)%PX(N-1)
    SPF(ISPF)%PROF(IL)%PZ(N)=SPF(ISPF)%PROF(IL)%PZ(N-1)
    SPF(ISPF)%PROF(IL)%PX(N-1)=X
    SPF(ISPF)%PROF(IL)%PZ(N-1)=Z
   ELSE
    SPF(ISPF)%PROF(IL)%PX(N)=X
    SPF(ISPF)%PROF(IL)%PZ(N)=Z
   ENDIF
   SPF(ISPF)%PROF(IL)%NPOS=N
  ENDIF
  RETURN
 ENDIF
 
 !## get distance till now:
 DO I=2,SPF(ISPF)%PROF(IL)%NPOS
  !## split elements
  IF(SPF(ISPF)%PROF(IL)%PX(I).GE.X)THEN
   !## check whether point fits begin point
   IF(ABS(SPF(ISPF)%PROF(IL)%PX(I-1)-X).LE.MINDIST)THEN
    SPF(ISPF)%PROF(IL)%PZ(I-1)=Z 
    EXIT
   !## check whether point fits end point
   ELSEIF(ABS(SPF(ISPF)%PROF(IL)%PX(I)-X).LE.MINDIST)THEN
    SPF(ISPF)%PROF(IL)%PZ(I)=Z   
    EXIT
   !## fits in between, add node to it!
   ELSE 
    ICRD=I-1
    !## insert node at position icrd
    CALL SOLIDPROFILEINSERTNODE(ISPF,IL,ICRD,X,Z)
   ENDIF
   EXIT
  ENDIF
 ENDDO
   
 END SUBROUTINE SOLIDPROFILEPUTINTERSECTION

 !###======================================================================
 LOGICAL FUNCTION SOLIDPROFILEZVALUE(X,JSPF,IL,Z)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IL,JSPF
 REAL,INTENT(IN) :: X
 REAL,INTENT(OUT) :: Z
 INTEGER :: I
 REAL :: DX,DZ
 
 SOLIDPROFILEZVALUE=.FALSE.
 
 !## get distance till now:
 DO I=2,SPF(JSPF)%PROF(IL)%NPOS
  IF(SPF(JSPF)%PROF(IL)%PX(I)-X.GE.-0.01)THEN
   DZ=SPF(JSPF)%PROF(IL)%PZ(I)-SPF(JSPF)%PROF(IL)%PZ(I-1)
   DX=(X-SPF(JSPF)%PROF(IL)%PX(I-1)) / (SPF(JSPF)%PROF(IL)%PX(I)-SPF(JSPF)%PROF(IL)%PX(I-1))
   Z=SPF(JSPF)%PROF(IL)%PZ(I-1)+DX*DZ
   SOLIDPROFILEZVALUE=.TRUE.
   EXIT
  ENDIF
 ENDDO
   
 END FUNCTION SOLIDPROFILEZVALUE

 !###======================================================================
 REAL FUNCTION SOLIDPROFILEDISTANCE(I,JSPF,XI,YI)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I,JSPF
 REAL,INTENT(IN) :: XI,YI
 INTEGER :: II
 REAL :: TX,DX,DY
 
 !## get distance till now:
 TX=0.0
 DO II=2,I-1 !2
  DX=(SPF(JSPF)%X(II)-SPF(JSPF)%X(II-1))**2.0
  DY=(SPF(JSPF)%Y(II)-SPF(JSPF)%Y(II-1))**2.0
  IF(DX+DY.GT.0.0)TX=TX+SQRT(DX+DY)
 ENDDO
 !## add distance to intersection point
 DX=(XI-SPF(JSPF)%X(I-1))**2.0
 DY=(YI-SPF(JSPF)%Y(I-1))**2.0
 IF(DX+DY.GT.0.0)TX=TX+SQRT(DX+DY)     
 
 SOLIDPROFILEDISTANCE=TX
 
 END FUNCTION SOLIDPROFILEDISTANCE

 !###======================================================================
 SUBROUTINE SOLIDPROFILEDELETE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K
 
 CALL WDIALOGSELECT(ID_DSERIESTAB2)
 CALL WDIALOGGETMENU(IDF_MENU1,J)

 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'You are about to remove the selected Cross-Section: ['// &
   TRIM(SPF(J)%FNAME)//'] from the Solid.'//CHAR(13)// &
  'Be aware that the Cross-Section will be removed from the list and NOT from the Solid folder.'//CHAR(13)// &
  'Any recovery can take place, manually by editing the *.sol file.'//CHAR(13)//CHAR(13)// &
  'Are you sure to continue?','Question')
 IF(WINFODIALOG(4).NE.1)RETURN

 CALL SOLIDPROFILEFIELDS()

 !## shift data ...
 DO I=J,NSPF-1 

  !## deallocate memory
  IF(ASSOCIATED(SPF(I)%X))DEALLOCATE(SPF(I)%X)
  IF(ASSOCIATED(SPF(I)%Y))DEALLOCATE(SPF(I)%Y)
  SPF(I)%NXY=SPF(I+1)%NXY
  ALLOCATE(SPF(I)%X(SPF(I)%NXY),SPF(I)%Y(SPF(I)%NXY))
  !## copy coordinates
  SPF(I)%X=SPF(I+1)%X
  SPF(I)%Y=SPF(I+1)%Y
  SPF(I)%FNAME=SPF(I+1)%FNAME
  SPF(I)%TX=SPF(I+1)%TX

  !## copy information for cross-sections
  DO K=1,SIZE(SPF(I)%PROF)
   !## deallocate memory for cross-sections (not really neccessary since they all have the same dimension (for now))
   IF(SPF(I)%PROF(K)%NPOS.GT.0)THEN
    IF(ASSOCIATED(SPF(I)%PROF(K)%PX))DEALLOCATE(SPF(I)%PROF(K)%PX)
    IF(ASSOCIATED(SPF(I)%PROF(K)%PZ))DEALLOCATE(SPF(I)%PROF(K)%PZ)
   ENDIF
   SPF(I)%PROF(K)%NPOS  =SPF(I+1)%PROF(K)%NPOS
   ALLOCATE(SPF(I)%PROF(K)%PX(MXPX)) 
   ALLOCATE(SPF(I)%PROF(K)%PZ(MXPX)) 
   SPF(I)%PROF(K)%PX    =SPF(I+1)%PROF(K)%PX
   SPF(I)%PROF(K)%PZ    =SPF(I+1)%PROF(K)%PZ
   SPF(I)%PROF(K)%ICLR  =SPF(I+1)%PROF(K)%ICLR
   SPF(I)%PROF(K)%IWIDTH=SPF(I+1)%PROF(K)%IWIDTH
  ENDDO

 ENDDO

 !## free memory resource
 I=NSPF
 IF(ASSOCIATED(SPF(I)%X))DEALLOCATE(SPF(I)%X)
 IF(ASSOCIATED(SPF(I)%Y))DEALLOCATE(SPF(I)%Y)
 DO J=1,SIZE(SPF(I)%PROF)
  IF(SPF(I)%PROF(J)%NPOS.GT.0)THEN
   IF(ASSOCIATED(SPF(I)%PROF(J)%PX))DEALLOCATE(SPF(I)%PROF(J)%PX)
   IF(ASSOCIATED(SPF(I)%PROF(J)%PZ))DEALLOCATE(SPF(I)%PROF(J)%PZ)
  ENDIF
 ENDDO
 IF(ASSOCIATED(SPF(I)%PROF))DEALLOCATE(SPF(I)%PROF)
 SPF(I)%NXY=0
 SPF(I)%FNAME=''
 SPF(I)%TX=0.0
 NSPF=NSPF-1
 
 CALL WDIALOGSELECT(ID_DSERIESTAB2)
 CALL WDIALOGPUTMENU(IDF_MENU1,SPF%FNAME,NSPF,NSPF)
 CALL SOLIDPROFILEFIELDS()

 END SUBROUTINE SOLIDPROFILEDELETE

 !###======================================================================
 LOGICAL FUNCTION SOLIDPROFILEADD()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 REAL :: DZ
 REAL :: DX,DY,TX
  
 SOLIDPROFILEADD=.FALSE.
 
 CALL WDIALOGSELECT(ID_DSCENNAME)

 NSPF=NSPF+1
 CALL WDIALOGGETMENU(IDF_MENU1,I,SPF(NSPF)%FNAME)
 IF(LEN_TRIM(SPF(NSPF)%FNAME).EQ.0)THEN
  NSPF=NSPF-1
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Give a name for the cross-section.','Error')
  RETURN
 ENDIF
 I=INDEX(SPF(NSPF)%FNAME,'.',.TRUE.)
 IF(I.NE.0)SPF(NSPF)%FNAME=SPF(NSPF)%FNAME(:I-1)
 SPF(NSPF)%FNAME=TRIM(SPF(NSPF)%FNAME)//'.spf'

 !## see whether the name is unique?
 DO I=1,NSPF-1
  IF(TRIM(UTL_CAP(SPF(I)%FNAME,'U')).EQ.TRIM(UTL_CAP(SPF(NSPF)%FNAME,'U')))THEN
   NSPF=NSPF-1
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Name for the cross-section already exists.','Error')
   RETURN
  ENDIF
 END DO

 !## copy current coordinates of profile ...
 SPF(NSPF)%NXY=NXY
 ALLOCATE(SPF(NSPF)%X(NXY),SPF(NSPF)%Y(NXY))
 SPF(NSPF)%X(1:NXY)=XY(1,1:NXY)
 SPF(NSPF)%Y(1:NXY)=XY(2,1:NXY)
 !## get distance of cross-section
 TX=0.0
 DO I=1,NXY
  IF(I.GT.1)THEN
   DX=(SPF(NSPF)%X(I)-SPF(NSPF)%X(I-1))**2.0
   DY=(SPF(NSPF)%Y(I)-SPF(NSPF)%Y(I-1))**2.0
   IF(DX+DY.NE.0.0)TX=TX+SQRT(DX+DY)
  ENDIF
 ENDDO
 SPF(NSPF)%TX=TX
  
 !## allocate number of elevation (max. NTBSOL)
 ALLOCATE(SPF(NSPF)%PROF(NTBSOL))

 CALL WDIALOGSELECT(ID_DSERIESTAB2)
 CALL WDIALOGPUTMENU(IDF_MENU1,SPF%FNAME,NSPF,NSPF)
 CALL SOLIDPROFILEFIELDS()

 DZ=(GRAPHUNITS(4,1)-GRAPHUNITS(2,1))/REAL(NTBSOL)
 DO I=1,SIZE(SPF(NSPF)%PROF) 
  SPF(NSPF)%PROF(I)%NPOS=0
  ALLOCATE(SPF(NSPF)%PROF(I)%PX(MXPX))
  ALLOCATE(SPF(NSPF)%PROF(I)%PZ(MXPX))
  IF(IEXIST(I+1).EQ.1)THEN
   SPF(NSPF)%PROF(I)%NPOS=2
   SPF(NSPF)%PROF(I)%PX(1)=0.0    
   SPF(NSPF)%PROF(I)%PX(2)=TX
   !## mean vertical depth as first guess!
   SPF(NSPF)%PROF(I)%PZ(1)=GRAPHUNITS(4,1)-DZ*REAL(I-1) 
   SPF(NSPF)%PROF(I)%PZ(2)=SPF(NSPF)%PROF(I)%PZ(1)
  ENDIF
  IF(MOD(I,2).NE.0)SPF(NSPF)%PROF(I)%ICLR  =WRGB(255,0,0)
  IF(MOD(I,2).EQ.0)SPF(NSPF)%PROF(I)%ICLR  =WRGB(0,0,255)
!  IF(MOD(I,2).NE.0)
  SPF(NSPF)%PROF(I)%IWIDTH=2
  SPF(NSPF)%PROF(I)%LNAME=SLD(1)%INTNAME(I)(INDEX(SLD(1)%INTNAME(I),'\',.TRUE.)+1:INDEX(SLD(1)%INTNAME(I),'.',.TRUE.)-1)
!  IF(MOD(I,2).EQ.0)SPF(NSPF)%PROF(I)%IWIDTH=2
 ENDDO
 
 ISPF=NSPF
 !## turn off a bitmap for this new cross-section
 PBITMAP%IACT=0; CALL WMENUSETSTATE(ID_BITMAP,2,0)

 SOLIDPROFILEADD=.TRUE.
 
 END FUNCTION SOLIDPROFILEADD

 !###======================================================================
 SUBROUTINE SOLIDPROFILEDRAW(IS1,IS2,ICRD,ICURSOR,IWINID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IS1,IS2,ICRD,ICURSOR,IWINID
 INTEGER :: I,J,J1,J2
 REAL :: DX,DY

 !## nothing selected
 IF(ISPF.LE.0)RETURN
 !## nothing draw/adjusted before
 IF(IS1.EQ.0)RETURN

 !## select proper bitmap
 CALL IGRSELECT(DRAWBITMAP,IBITMAP(IWINID))
 !## change plotmode
 CALL IGRPLOTMODE(MODEXOR)
 !## set area/units
 CALL IGRAREA (GRAPHAREA (1,IWINID),GRAPHAREA(2,IWINID) ,GRAPHAREA(3,IWINID) ,GRAPHAREA(4,IWINID))
 CALL IGRUNITS(GRAPHUNITS(1,IWINID),GRAPHUNITS(2,IWINID),GRAPHUNITS(3,IWINID),GRAPHUNITS(4,IWINID))

 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRFILLPATTERN(OUTLINE)
 
 DX=(GRAPHUNITS(3,IWINID)-GRAPHUNITS(1,IWINID))/500.0
 DY=(GRAPHUNITS(4,IWINID)-GRAPHUNITS(2,IWINID))/500.0*WINFOGRREAL(GRAPHICSRATIO)
                  
 !## draw profile lines
 DO I=IS1,IS2 
  IF(SPF(ISPF)%PROF(I)%NPOS.LE.0)CYCLE
  CALL IGRLINEWIDTH(SPF(ISPF)%PROF(I)%IWIDTH)
  CALL IGRCOLOURN(INVERSECOLOUR(SPF(ISPF)%PROF(I)%ICLR))
  PX=>SPF(ISPF)%PROF(I)%PX
  PZ=>SPF(ISPF)%PROF(I)%PZ
  J1=2
  J2=SPF(ISPF)%PROF(I)%NPOS
  IF(ICRD.NE.0)THEN
   !## add point, remove current line only!
   IF(ICURSOR.EQ.2)THEN
    J1=MAX(J1,ICRD+1) 
    J2=MIN(J2,ICRD+1) 
   ELSE
    J1=MAX(J1,ICRD) 
    J2=MIN(J2,ICRD+1)  
   ENDIF
  ENDIF
  DO J=J1,J2 !2,SPF(ISPF)%PROF(I)%NPOS
   CALL IGRJOIN(PX(J-1),PZ(J-1),PX(J),PZ(J))
   CALL IGRRECTANGLE(PX(J-1)-DX,PZ(J-1)+DY,PX(J-1)+DX,PZ(J-1)-DY)
  ENDDO
  CALL IGRRECTANGLE(PX(J-1)-DX,PZ(J-1)+DY,PX(J-1)+DX,PZ(J-1)-DY)
  NULLIFY(PX,PZ)
 ENDDO
  
! !## putbitmap on entire screen or ratio
! CALL IGRSELECT(DRAWWIN,IWINPROFILE(IWINID))
! CALL WBITMAPPUT(IBITMAP(IWINID),0,1)
 
 CALL IGRCOLOURN(WRGB(0,0,0))
 CALL IGRLINEWIDTH(0) 
 CALL IGRPLOTMODE(MODEAND)

 END SUBROUTINE SOLIDPROFILEDRAW

 !###======================================================================
 SUBROUTINE SOLIDPROFILEDRAW_INTERSECTIONS(IWINID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IWINID
 REAL :: DX,DY

 IF(IFIND.EQ.0)RETURN
 
 CALL IGRPLOTMODE(MODECOPY) 
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRCOLOURN(WRGB(255,110,110))

 !## select proper bitmap
 CALL IGRSELECT(DRAWBITMAP,IBITMAP(IWINID))
 !## set area/units
 CALL IGRAREA (GRAPHAREA (1,IWINID),GRAPHAREA(2,IWINID) ,GRAPHAREA(3,IWINID) ,GRAPHAREA(4,IWINID))
 CALL IGRUNITS(GRAPHUNITS(1,IWINID),GRAPHUNITS(2,IWINID),GRAPHUNITS(3,IWINID),GRAPHUNITS(4,IWINID))
 
 DX=(GRAPHUNITS(3,IWINID)-GRAPHUNITS(1,IWINID))/250.0
 DY=(GRAPHUNITS(4,IWINID)-GRAPHUNITS(2,IWINID))/250.0*WINFOGRREAL(GRAPHICSRATIO)
 
! !## get current graph-dimensions
! X1 =INFOGRAPHICS(GRAPHICSUNITMINX); X2 =INFOGRAPHICS(GRAPHICSUNITMAXX)
! Y1 =INFOGRAPHICS(GRAPHICSUNITMINY); Y2 =INFOGRAPHICS(GRAPHICSUNITMAXY)
 !CALL WGRTEXTFONT(AXES%TFONT,WIDTH=AXES%CHW,HEIGHT=AXES%CHH,ISTYLE=0)
 CALL WGRTEXTORIENTATION(ALIGNLEFT,ANGLE=0.0)
 
 CALL SOLIDPROFILEINTERSECT(DX,DY)
 
 CALL IGRPLOTMODE(MODECOPY)
 
 END SUBROUTINE SOLIDPROFILEDRAW_INTERSECTIONS
 
 !###======================================================================
 SUBROUTINE SOLIDPROFILEDRAW_MASK()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IWINID,I,J,IIDF
 REAL :: XC,YC,X
 TYPE(IDFOBJ) :: IDF
 INTEGER,DIMENSION(6) :: IPRF
  
 !## nothing to do
 IF(NMASK.LE.0)RETURN
 IF(IMASK.EQ.0)RETURN
 
! CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not display mask yet!','Error')
 RETURN
 
 CALL IGRPLOTMODE(MODECOPY) !AND)
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRFILLPATTERN(SOLID)
 CALL IGRCOLOURN(WRGB(110,110,110))

! DO IWINID=1,SIZE(IWINPROFILE)

 !## only for mother screen
 IWINID=1

 !## select proper bitmap
 CALL IGRSELECT(DRAWBITMAP,IBITMAP(IWINID))
 !## set area/units
 CALL IGRAREA (GRAPHAREA (1,IWINID),GRAPHAREA(2,IWINID) ,GRAPHAREA(3,IWINID) ,GRAPHAREA(4,IWINID))
 CALL IGRUNITS(GRAPHUNITS(1,IWINID),GRAPHUNITS(2,IWINID),GRAPHUNITS(3,IWINID),GRAPHUNITS(4,IWINID))

 !## first of profidf() zijn target idf ... since they have the same resolution ... trust that!
 
 J=0
 DO IIDF=2,MXNIDF-1,2
  J=J+1
  !## plot area inside mask
  IF(PROFIDF(IIDF)%ISCREEN.EQ.1.AND.PROFIDF(IIDF+1)%ISCREEN.EQ.1)THEN
   DO I=1,SERIE(IIDF)%N
    CALL PROFILE_GETLOCATION(XC,YC,SERIE(IIDF)%X(I))
    !## point on profile inside mask
    IF(IDFGETXYVAL(MASK(J)%IDF,XC,YC).NE.MASK(J)%IDF%NODATA)THEN

     !## start rectangle
     IF(I.EQ.1)THEN
      IF(SERIE(IIDF)%Y(I).NE.PROFIDF(IIDF)  %IDF%NODATA.AND. &
         SERIE(IIDF)%Y(I).NE.PROFIDF(IIDF+1)%IDF%NODATA) &
       CALL IGRRECTANGLE( SERIE(IIDF)%X(I)                        ,SERIE(IIDF+1)%Y(I), &
                         (SERIE(IIDF)%X(I)+SERIE(IIDF)%X(I+1))/2.0,SERIE(IIDF)  %Y(I))   
     ELSEIF(I.EQ.SERIE(IIDF)%N)THEN 
      IF(SERIE(IIDF)%Y(I).NE.PROFIDF(IIDF)  %IDF%NODATA.AND. &
         SERIE(IIDF)%Y(I).NE.PROFIDF(IIDF+1)%IDF%NODATA) &
       CALL IGRRECTANGLE((SERIE(IIDF)%X(I)+SERIE(IIDF)%X(I-1))/2.0,SERIE(IIDF+1)%Y(I), &
                          SERIE(IIDF)%X(I)                        ,SERIE(IIDF)  %Y(I))   
     ELSE
      IF(SERIE(IIDF)  %Y(I)  .NE.PROFIDF(IIDF)  %IDF%NODATA.AND. &
         SERIE(IIDF)  %Y(I+1).NE.PROFIDF(IIDF)  %IDF%NODATA.AND. &
         SERIE(IIDF+1)%Y(I)  .NE.PROFIDF(IIDF+1)%IDF%NODATA.AND. &
         SERIE(IIDF+1)%Y(I+1).NE.PROFIDF(IIDF+1)%IDF%NODATA) &
       CALL IGRRECTANGLE((SERIE(IIDF)%X(I)+SERIE(IIDF)%X(I-1))/2.0,SERIE(IIDF+1)%Y(I), &
                         (SERIE(IIDF)%X(I)+SERIE(IIDF)%X(I+1))/2.0,SERIE(IIDF)  %Y(I))
     ENDIF
    
    ENDIF
   ENDDO
  ENDIF
  
!  !## colour cross-section grey
!  IF(NX.EQ.0)THEN
!   CALL IGRRECTANGLE(XMIN,YMIN,XMAX,YMAX)
!  !## colour those sections grey that are outside current polygon
!  ELSE
!   ION=JON
!   DO I=1,NX-1 
!    IF(ION.EQ.0)CALL IGRRECTANGLE(X(I),YMIN,X(I+1),YMAX)
!    ION=ABS(ION-1)
!   ENDDO
!  ENDIF
 
 ENDDO

! ENDDO 

 CALL IGRPLOTMODE(MODECOPY)
 
 END SUBROUTINE SOLIDPROFILEDRAW_MASK

 !###======================================================================
 SUBROUTINE SOLIDPROFILEDRAW_POLYGON()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,ISTATUS,NX,ION,JON,IWINID
 REAL :: XINTER,YINTER,D,TD
 REAL,DIMENSION(:),ALLOCATABLE :: X
  
 !## nothing to do
 IF(SHPNO.LE.0)RETURN
 IF(IMASK.EQ.0)RETURN
 
 ALLOCATE(X(100))

 !## intersect lines of polygon with lines of cross-section 
 NX=0
 TD=0.0
 D =0.0
 DO I=1,NXY-1
  TD=TD+D
  DO J=1,SHPNO
   !IF()THEN
   DO K=1,SHPNCRD(J)
    IF(K.EQ.SHPNCRD(J))THEN
     CALL IGRINTERSECTLINE(XY(1,I)   ,XY(2,I)   ,XY(1,I+1)   ,XY(2,I+1),    &  !## coordinates of cross-sectional line
                           SHPXC(K,J),SHPYC(K,J),SHPXC(1,J)  ,SHPYC(1,J),   &  !## coordinates of polygon
                           XINTER,YINTER,ISTATUS)
    ELSE
     CALL IGRINTERSECTLINE(XY(1,I)   ,XY(2,I)   ,XY(1,I+1)   ,XY(2,I+1),    & !## coordinates of cross-sectional line
                           SHPXC(K,J),SHPYC(K,J),SHPXC(K+1,J),SHPYC(K+1,J), & !## coordinates of polygon
                           XINTER,YINTER,ISTATUS)
    ENDIF
    IF(ISTATUS.EQ.5)THEN
     NX=NX+1
     X(NX)=TD+SQRT((XY(1,I)-XINTER)**2.0+(XY(2,I)-YINTER)**2.0)  
    ENDIF
   END DO
  ENDDO
  D=SQRT((XY(1,I)-XY(1,I+1))**2.0+(XY(2,I)-XY(2,I+1))**2.0)   
 ENDDO
 TD=TD+D
 
 !## sort distances
 NX=NX+1
 X(NX)=0.0
 NX=NX+1
 X(NX)=TD
 CALL SHELLSORT(NX,X)
 
 !## default outside unless otherwise
 JON=0
 DO J=1,SHPNO
  !## check FIRST point inside/outside polygon j
  ION=UTL_INSIDEPOLYGON(XY(1,1),XY(2,1),SHPXC(:,J),SHPYC(:,J),SHPNCRD(J))
  !## inside/on edge
  IF(ION.NE.-1)THEN
   JON=1
   EXIT
  ENDIF
 ENDDO

 CALL IGRPLOTMODE(MODEAND)
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRFILLPATTERN(SOLID)
 CALL IGRCOLOURN(WRGB(210,210,210))

 DO IWINID=1,SIZE(IWINPROFILE)

  !## select proper bitmap
  CALL IGRSELECT(DRAWBITMAP,IBITMAP(IWINID))
  !## set area/units
  CALL IGRAREA (GRAPHAREA (1,IWINID),GRAPHAREA(2,IWINID) ,GRAPHAREA(3,IWINID) ,GRAPHAREA(4,IWINID))
  CALL IGRUNITS(GRAPHUNITS(1,IWINID),GRAPHUNITS(2,IWINID),GRAPHUNITS(3,IWINID),GRAPHUNITS(4,IWINID))

  !## colour cross-section grey
  IF(NX.EQ.0)THEN
   CALL IGRRECTANGLE(XMIN,YMIN,XMAX,YMAX)
  !## colour those sections grey that are outside current polygon
  ELSE
   ION=JON
   DO I=1,NX-1 
    IF(ION.EQ.0)CALL IGRRECTANGLE(X(I),YMIN,X(I+1),YMAX)
    ION=ABS(ION-1)
   ENDDO
  ENDIF
 
 ENDDO
 
 CALL IGRPLOTMODE(MODECOPY)
 
 DEALLOCATE(X)

 END SUBROUTINE SOLIDPROFILEDRAW_POLYGON

 !###======================================================================
 SUBROUTINE SOLIDPLOTLOCATIONCROSSSECTIONS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J
 REAL :: TW,TH 
 CHARACTER(LEN=50) :: FNAME
  
 CALL UTL_SETTEXTSIZE(TW,TH,0.015) !,0.03)
 
 CALL IGRFILLPATTERN(SOLIDLINE)

 !## draw profile lines
 DO I=1,NSPF 
  IF(SPF(I)%NXY.GT.0)THEN
   PX=>SPF(I)%X
   PY=>SPF(I)%Y
   !## red line
   CALL IGRLINEWIDTH(3)
   CALL IGRCOLOURN(WRGB(255,255,255))
   DO J=2,SPF(I)%NXY
    CALL IGRJOIN(PX(J-1),PY(J-1),PX(J),PY(J))
   ENDDO
   CALL IGRLINEWIDTH(1)
   CALL IGRCOLOURN(WRGB(0,0,0))
   DO J=2,SPF(I)%NXY
    CALL IGRJOIN(PX(J-1),PY(J-1),PX(J),PY(J))
   ENDDO
   !## black label
   CALL IGRCOLOURN(WRGB(0,0,0))
   FNAME=SPF(I)%FNAME(:INDEX(SPF(I)%FNAME,'.',.TRUE.)-1)
   CALL IPFPLOTLABEL(PX(1),PY(1),(/TRIM(FNAME)/),(/1/),1,TW,TH,(/''/),.FALSE.,-1)
  ENDIF
  NULLIFY(PX,PY)
 ENDDO
 
 END SUBROUTINE SOLIDPLOTLOCATIONCROSSSECTIONS
 
 !###======================================================================
 SUBROUTINE SOLIDPROFILEMINMAX(IFIXX,IFIXY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFIXX,IFIXY
 INTEGER :: I,J
 
 IF(ISOLID.EQ.0)RETURN
 IF(ISPF.LE.0)RETURN
 
 DO I=1,SIZE(SPF(ISPF)%PROF) 
  IF(SPF(ISPF)%PROF(I)%NPOS.LE.0)CYCLE
  PX=>SPF(ISPF)%PROF(I)%PX
  PZ=>SPF(ISPF)%PROF(I)%PZ
  DO J=1,SPF(ISPF)%PROF(I)%NPOS
   IF(IFIXX.EQ.0)THEN
    XMIN=MIN(XMIN,PX(J))
    XMAX=MAX(XMAX,PX(J))
   ENDIF
   IF(IFIXY.EQ.0)THEN
    YMIN=MIN(YMIN,PZ(J))
    YMAX=MAX(YMAX,PZ(J))
   ENDIF
  ENDDO
  NULLIFY(PX,PZ)
 ENDDO

 END SUBROUTINE SOLIDPROFILEMINMAX
 
 !###======================================================================
 SUBROUTINE SOLIDPROFILEMOUSE(GX,GY,ICURSOR,ICRD,IELEV,IWINID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IWINID
 INTEGER,INTENT(OUT) :: ICURSOR,ICRD,IELEV
 INTEGER :: I,J
 REAL,INTENT(IN) :: GX,GY
 REAL :: DX,DY,DXX
 
 !## solid-modeling not activated
 IF(ISOLID.EQ.0)RETURN
 !## nothing selected
 IF(ISPF.LE.0)RETURN

 CALL WINDOWSELECT(IWINPROFILE(IWINID))
 CALL WINDOWOUTSTATUSBAR(2,'')

 ICURSOR=0
 IELEV  =0
 ICRD   =0
 DX     =(GRAPHUNITS(3,IWINID)-GRAPHUNITS(1,IWINID))/125.0
 DY     =(GRAPHUNITS(4,IWINID)-GRAPHUNITS(2,IWINID))/125.0

 !## move node
 DO I=SIZE(SPF(ISPF)%PROF),1,-1 ! 1,SIZE(SPF(ISPF)%PROF)
  IF(SPF(ISPF)%PROF(I)%NPOS.LE.0)CYCLE
  PX=>SPF(ISPF)%PROF(I)%PX
  PZ=>SPF(ISPF)%PROF(I)%PZ
  DO J=1,SPF(ISPF)%PROF(I)%NPOS
   IF(GX.GE.PX(J)-DX.AND.GX.LE.PX(J)+DX.AND. &
      GY.GE.PZ(J)-DY.AND.GY.LE.PZ(J)+DY)THEN
    ICURSOR=1
    IELEV=I
    ICRD =J  
    CALL WCURSORSHAPE(ID_CURSORADJUSTPOINT)
    IF(ALLOCATED(PROFIDF))THEN   
     IF(I.LE.SIZE(PROFIDF))THEN
      CALL WINDOWOUTSTATUSBAR(2,'Move Node '//TRIM(ITOS(ICRD))//' on Elevation '//TRIM(ITOS(IELEV))// &
       ';'//TRIM(PROFIDF(I)%IDF%FNAME))
     ELSE
      CALL WINDOWOUTSTATUSBAR(2,'Move Node '//TRIM(ITOS(ICRD))//' on Elevation '//TRIM(ITOS(IELEV)))
     ENDIF
    ELSE
     CALL WINDOWOUTSTATUSBAR(2,'Move Node '//TRIM(ITOS(ICRD))//' on Elevation '//TRIM(ITOS(IELEV)))
    ENDIF
    RETURN
   ENDIF
  ENDDO
  
  !## near the line
  !## only whenever points can be added still!
  IF(SPF(ISPF)%PROF(I)%NPOS.LT.SIZE(PX))THEN 
   DO J=1,SPF(ISPF)%PROF(I)%NPOS-1
    DXX=IGRDISTANCELINE(PX(J),PZ(J),PX(J+1),PZ(J+1),GX,GY,0)
    IF(DXX.GE.0.0.AND.DXX.LE.DY)THEN
     ICURSOR=2
     IELEV=I
     ICRD =J
     CALL WCURSORSHAPE(ID_CURSORADDPOINT)
     IF(ALLOCATED(PROFIDF))THEN   
      IF(I.LE.SIZE(PROFIDF))THEN
       CALL WINDOWOUTSTATUSBAR(2,'Add point on Line Elevation '//TRIM(ITOS(IELEV))// &
         ';'//TRIM(PROFIDF(I)%IDF%FNAME))  
      ELSE
       CALL WINDOWOUTSTATUSBAR(2,'Add point on Line Elevation '//TRIM(ITOS(IELEV)))  
      ENDIF
     ELSE
      CALL WINDOWOUTSTATUSBAR(2,'Add point on Line Elevation '//TRIM(ITOS(IELEV)))  
     ENDIF
     RETURN
    ENDIF
   ENDDO
  ENDIF
  
  NULLIFY(PX,PZ)
 ENDDO !ILOOP

 ICRD   =0
 ICURSOR=0
 CALL WCURSORSHAPE(CURARROW)
 CALL WINDOWOUTSTATUSBAR(2,'')

 END SUBROUTINE SOLIDPROFILEMOUSE

 !###======================================================================
 SUBROUTINE SOLIDPROFILESELECTNODES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: MAXPOL=50
 REAL :: XC1,YC1,XC2,YC2,X
 REAL,ALLOCATABLE,DIMENSION(:) :: XPOL,YPOL
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: SP
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IMENU
 CHARACTER(LEN=52),ALLOCATABLE,DIMENSION(:) :: CMENU
 INTEGER :: NPOL,ITYPE,IDOWN,IWINID,I,J,II,K,DID,ISEL
 TYPE(WIN_MESSAGE) :: MESSAGE
 LOGICAL :: LEX
  
 !## solid-modeling not activated
 IF(ISOLID.EQ.0)RETURN
 !## nothing selected
 IF(ISPF.LE.0)RETURN

 IWINID=1; CALL WINDOWSELECT(IWINPROFILE(IWINID)); CALL WINDOWOUTSTATUSBAR(2,'')

 !## draw polygon
 ALLOCATE(XPOL(MAXPOL),YPOL(MAXPOL)); NPOL=0
 CALL WCURSORSHAPE(ID_CURSORPOLYGON)

 CALL IGRPLOTMODE(MODEXOR); CALL IGRCOLOURN(WRGB(255,255,255)); CALL IGRLINETYPE(SOLIDLINE); CALL IGRLINEWIDTH(2)

 IDOWN=0; LEX=.FALSE.

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
    
   !## mouse-move
   CASE (MOUSEMOVE)

    XC2=MESSAGE%GX; YC2=MESSAGE%GY

    IF(NPOL.GT.0)THEN
     !## line drawn before, remove it
     IF(LEX)THEN
      XPOL(NPOL+1)=XC1; YPOL(NPOL+1)=YC1
      CALL IGRPOLYGONCOMPLEX(XPOL,YPOL,NPOL+1)
     ENDIF
     LEX=.TRUE.
     !## draw new line
     XPOL(NPOL+1)=XC2; YPOL(NPOL+1)=YC2
     CALL IGRPOLYGONCOMPLEX(XPOL,YPOL,NPOL+1)
     XC1=XC2; YC1=YC2
    ENDIF
    
   CASE (MOUSEBUTDOWN)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (1)  !## left button
      NPOL=NPOL+1; XPOL(NPOL)=XC2; YPOL(NPOL)=YC2
     CASE (3)  !## right button
      !## remove line
      IF(NPOL.GT.0)THEN
       IF(LEX)CALL IGRPOLYGONCOMPLEX(XPOL,YPOL,NPOL+1)
      ENDIF
      EXIT

    END SELECT

  END SELECT
 END DO

 CALL WCURSORSHAPE(CURARROW)
 CALL IGRPLOTMODE(MODECOPY); CALL IGRFILLPATTERN(OUTLINE); CALL IGRLINETYPE(SOLIDLINE); CALL IGRLINEWIDTH(1)

 DO II=1,2
  K=0
  !## select points
  DO I=1,SIZE(SPF(ISPF)%PROF)
   IF(SPF(ISPF)%PROF(I)%NPOS.LE.0)CYCLE
   PX=>SPF(ISPF)%PROF(I)%PX
   PZ=>SPF(ISPF)%PROF(I)%PZ
   DO J=1,SPF(ISPF)%PROF(I)%NPOS
    IF(UTL_INSIDEPOLYGON(PX(J),PZ(J),XPOL,YPOL,NPOL).EQ.1)THEN
     K=K+1
     !## store locations
     IF(II.EQ.2)THEN
      SP(K,1)=I  !## line
      SP(K,2)=J  !## node on line
     ENDIF
    ENDIF
   ENDDO
  ENDDO
  IF(II.EQ.2)EXIT
  ALLOCATE(SP(K,2)); SP=0
 ENDDO
 
 IF(ALLOCATED(XPOL))DEALLOCATE(XPOL); IF(ALLOCATED(YPOL))DEALLOCATE(YPOL)
 !## nothing selected
 IF(K.LE.0)RETURN
 
 DID=WINFODIALOG(CURRENTDIALOG)

 ALLOCATE(CMENU(SIZE(SPF(ISPF)%PROF)),IMENU(SIZE(SPF(ISPF)%PROF))); CMENU=''; IMENU=0
 DO I=1,SIZE(SPF(ISPF)%PROF); CMENU(I)='Interface '//TRIM(ITOS(I)); ENDDO
 
 CALL WDIALOGLOAD(ID_DSOLID_ADJUST,ID_DSOLID_ADJUST)

 CALL WDIALOGFIELDSTATE(IDF_REAL1,0)
 CALL WDIALOGPUTREAL(IDF_REAL1,1.0)

 DO I=1,K; IMENU(SP(I,1))=1; ENDDO
 CALL WDIALOGPUTMENU(IDF_MENU1,CMENU,SIZE(SPF(ISPF)%PROF),IMENU)
 CALL WDIALOGFIELDSTATE(IDF_MENU1,0)

 J=SP(1,1)
 CALL WDIALOGPUTMENU(IDF_MENU2,CMENU,SIZE(SPF(ISPF)%PROF),J)
 CALL WDIALOGFIELDSTATE(IDF_MENU2,0)

 CALL WDIALOGTITLE('Adjust Selected '//TRIM(ITOS(K))//' nodes')
 CALL WDIALOGPUTIMAGE(IDF_PICTURE1,ID_ICONRIGHT,1)
 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDCANCEL)
      EXIT
     CASE (IDHELP)
     CASE (IDOK)
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,ISEL)
      IF(ISEL.EQ.2)THEN
       CALL WDIALOGGETREAL(IDF_REAL1,X)
       EXIT
      ELSEIF(ISEL.EQ.3)THEN
       CALL WDIALOGGETMENU(IDF_MENU1,IMENU)
       CALL WDIALOGGETMENU(IDF_MENU2,J)
       IF(IMENU(J).EQ.1)THEN
        CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You cannot merge nodes from a line onto the identical line.','Error')
       ELSE
        IMENU(J)=-1; EXIT
       ENDIF
      ELSE
       EXIT
      ENDIF
    END SELECT
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_RADIO1,IDF_RADIO2,IDF_RADIO3)
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,ISEL)
      IF(ISEL.EQ.1)THEN; I=0; J=0
      ELSEIF(ISEL.EQ.2)THEN; I=1; J=0
      ELSE; I=0; J=1; ENDIF
      CALL WDIALOGFIELDSTATE(IDF_REAL1,I)
      CALL WDIALOGFIELDSTATE(IDF_MENU1,J)
      CALL WDIALOGFIELDSTATE(IDF_MENU2,J)
    END SELECT 
  END SELECT
 ENDDO

 CALL WDIALOGUNLOAD(); CALL WDIALOGSELECT(DID)
 IF(MESSAGE%VALUE1.EQ.IDCANCEL)RETURN

 SELECT CASE (ISEL)
  CASE (1) !## delete nodes
   CALL SOLIDPROFILEDELNODE(SP(:,2),SP(:,1))
  CASE (2) !## split level
   CALL SOLIDPROFILESPLITLINE(SP(:,2),SP(:,1),X)
  CASE (3) !## merge with level j
   CALL SOLIDPROFILEMERGELINE(SP(:,2),SP(:,1),IMENU)
 END SELECT

 IF(ALLOCATED(SP))   DEALLOCATE(SP)
 IF(ALLOCATED(CMENU))DEALLOCATE(CMENU)
 IF(ALLOCATED(IMENU))DEALLOCATE(IMENU)

 END SUBROUTINE SOLIDPROFILESELECTNODES
 
 !###======================================================================
 SUBROUTINE SOLIDPROFILEADJUST(XC,YC,ICURSOR,ICRD,IELEV,IWINID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IELEV,IWINID
 INTEGER,INTENT(INOUT) :: ICURSOR,ICRD
 REAL,INTENT(IN) :: XC,YC
 REAL :: GX,GY,X1,Y1,X2,Y2,X3,Y3,XINTER,YINTER,DX,DY
 INTEGER :: NCRD,ISTATUS,IL,I,JELEV,JCRD
 REAL,DIMENSION(2) :: D,MIND
 LOGICAL :: LINT,LSNP
 
 !## solid-modeling not activated
 IF(ISOLID.EQ.0)RETURN
 !## nothing selected
 IF(ISPF.LE.0)RETURN
 !## nothing to adjust
 IF(ICURSOR.EQ.0)RETURN
 
 DX=(GRAPHUNITS(3,IWINID)-GRAPHUNITS(1,IWINID))/200.0
 DY=(GRAPHUNITS(4,IWINID)-GRAPHUNITS(2,IWINID))/200.0

 GX=XC; GY=YC
 
 SELECT CASE (ICURSOR)
  !## move current point
  CASE (1)
   !## check for min./max. extinction of x-coordinate
   GX=MIN(SPF(ISPF)%TX,MAX(0.0,GX))
   NCRD=SPF(ISPF)%PROF(IELEV)%NPOS

   !## previous point not to pass
   X1=SPF(ISPF)%PROF(IELEV)%PX(MAX(1,ICRD-1))
   Y1=SPF(ISPF)%PROF(IELEV)%PZ(MAX(1,ICRD-1))
   !## next point not to pass
   X3=SPF(ISPF)%PROF(IELEV)%PX(MIN(NCRD,ICRD+1))
   Y3=SPF(ISPF)%PROF(IELEV)%PZ(MIN(NCRD,ICRD+1))

   !## check for previous point (if icrd.gt.1)
   IF(ICRD.GT.1)GX=MAX(GX,X1)
   !## check for previous point (if icrd.lt.ncrd)
   IF(ICRD.LT.NCRD)GX=MIN(GX,X3)
   
   !## current point
   X2=GX 
   Y2=GY 

   !## only whenever isnap.eq.1, snap to nearest point
   LSNP=.FALSE.
   IF(ISSNAP.EQ.1)THEN
    MIND =10.0E10
    JELEV=0
    JCRD =0
    !## check vertical position whether it yields any intersection
    DO IL=MAX(1,IELEV-1),MIN(NTBSOL,IELEV+1)
     IF(IL.EQ.IELEV)CYCLE
     IF(SPF(ISPF)%PROF(IL)%NPOS.LE.0)CYCLE
     !## check for nearest point above and beneath layer, intersections
     PX=>SPF(ISPF)%PROF(IL)%PX
     PZ=>SPF(ISPF)%PROF(IL)%PZ
     DO I=1,SPF(ISPF)%PROF(IL)%NPOS
      D(1)=ABS(X2-PX(I)); D(2)=ABS(Y2-PZ(I))
      IF(D(1).LT.DX.AND.D(2).LT.DY)THEN; MIND=D; JELEV=IL; JCRD=I; ENDIF
     ENDDO
    ENDDO
    !## point found in snap distance
    IF(MIND(1).LT.DX.AND.MIND(2).LT.DY)THEN
     LSNP=.TRUE.
     SPF(ISPF)%PROF(IELEV)%PX(ICRD)=SPF(ISPF)%PROF(JELEV)%PX(JCRD)
     SPF(ISPF)%PROF(IELEV)%PZ(ICRD)=SPF(ISPF)%PROF(JELEV)%PZ(JCRD)
    ENDIF
   ENDIF
   
   !## only whenever ilock.eq.1
   LINT=.FALSE.
   IF(ILOCK.EQ.1)THEN
    !## check vertical position whether it yields any intersection
ILLOOP: DO IL=MAX(1,IELEV-1),MIN(NTBSOL,IELEV+1)
     IF(IL.EQ.IELEV)CYCLE
     IF(SPF(ISPF)%PROF(IL)%NPOS.LE.0)CYCLE
     !## check for not overlapping above and beneath layer, intersections
     PX=>SPF(ISPF)%PROF(IL)%PX
     PZ=>SPF(ISPF)%PROF(IL)%PZ
     DO I=1,SPF(ISPF)%PROF(IL)%NPOS-1
      !## compute intersection previous segment
      CALL IGRINTERSECTLINE(X1,Y1,X2,Y2,PX(I),PZ(I),PX(I+1),PZ(I+1),XINTER,YINTER,ISTATUS)
      IF(ISTATUS.EQ.5)THEN
       LINT=.TRUE.
       EXIT ILLOOP
      ENDIF
      !## compute intersection --- next segment
      CALL IGRINTERSECTLINE(X2,Y2,X3,Y3,PX(I),PZ(I),PX(I+1),PZ(I+1),XINTER,YINTER,ISTATUS)
      IF(ISTATUS.EQ.5)THEN
       LINT=.TRUE.
       EXIT ILLOOP
      ENDIF
     ENDDO
    ENDDO ILLOOP
   
    !## no intersection found ... take adjusted point
    IF(.NOT.LINT)THEN
     SPF(ISPF)%PROF(IELEV)%PX(ICRD)=GX
     IF(IL.LT.IELEV)THEN
      SPF(ISPF)%PROF(IELEV)%PZ(ICRD)=GY+0.01
     ELSE
      SPF(ISPF)%PROF(IELEV)%PZ(ICRD)=GY-0.01
     ENDIF
    ENDIF
   
   ENDIF
   IF(.NOT.LSNP.AND..NOT.LINT)THEN 
    SPF(ISPF)%PROF(IELEV)%PX(ICRD)=GX   
    SPF(ISPF)%PROF(IELEV)%PZ(ICRD)=GY
   ENDIF
   
  !## insert node at position icrd
  CASE (2)
   CALL SOLIDPROFILEINSERTNODE(ISPF,IELEV,ICRD,GX,GY)
   !## change to move
   ICURSOR=1
   !## current node the one to be moved
   ICRD=ICRD+1
 END SELECT   
 
 END SUBROUTINE SOLIDPROFILEADJUST
 
 !###======================================================================
 SUBROUTINE SOLIDPROFILEINSERTNODE(JSPF,IELEV,ICRD,GX,GY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IELEV,JSPF,ICRD
 REAL,INTENT(IN) :: GX,GY
 INTEGER :: I,N
 
 N=SPF(JSPF)%PROF(IELEV)%NPOS
 
 IF(N.EQ.SIZE(SPF(JSPF)%PROF(IELEV)%PX))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not add another point,'//CHAR(13)// &
      'maximum of '//TRIM(ITOS(SIZE(SPF(JSPF)%PROF(IELEV)%PX)))//' points reached','Error')
  RETURN
 ENDIF
  
 !## shift coordinates backwards ...
 DO I=N+1,ICRD+2,-1
  SPF(JSPF)%PROF(IELEV)%PX(I)=SPF(JSPF)%PROF(IELEV)%PX(I-1)
  SPF(JSPF)%PROF(IELEV)%PZ(I)=SPF(JSPF)%PROF(IELEV)%PZ(I-1)
 ENDDO
 !## insert new coordinates
 SPF(JSPF)%PROF(IELEV)%PX(ICRD+1)=GX
 SPF(JSPF)%PROF(IELEV)%PZ(ICRD+1)=GY   
 !## add new coordinate
 SPF(JSPF)%PROF(IELEV)%NPOS=N+1
 
 END SUBROUTINE SOLIDPROFILEINSERTNODE

 !###======================================================================
 SUBROUTINE SOLIDPROFILEDELNODE(VCRD,VIELEV)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN),DIMENSION(:)  :: VCRD,VIELEV
 INTEGER,DIMENSION(:),ALLOCATABLE :: CCRD,CIELEV
 INTEGER :: I,J,ICRD,IELEV
 
 !## copy node numbers
 ALLOCATE(CCRD  (SIZE(VCRD)))  ; CCRD=VCRD
 ALLOCATE(CIELEV(SIZE(VIELEV))); CIELEV=VIELEV
 
 !## remove all coordinates for current ielev
 DO J=1,SIZE(VCRD)
  !## remove current node on ielev
  ICRD =CCRD(J)
  IELEV=CIELEV(J)
  DO I=ICRD,SPF(ISPF)%PROF(IELEV)%NPOS
   SPF(ISPF)%PROF(IELEV)%PX(I)=SPF(ISPF)%PROF(IELEV)%PX(I+1)
   SPF(ISPF)%PROF(IELEV)%PZ(I)=SPF(ISPF)%PROF(IELEV)%PZ(I+1)
  ENDDO
  SPF(ISPF)%PROF(IELEV)%NPOS=SPF(ISPF)%PROF(IELEV)%NPOS-1
  !## correct remaing nodes
  DO I=J+1,SIZE(VCRD)
   IF(IELEV.EQ.CIELEV(I).AND.ICRD.LT.CCRD(I))CCRD(I)=CCRD(I)-1
  ENDDO
 ENDDO
 
 DEALLOCATE(CCRD,CIELEV)
 
 END SUBROUTINE SOLIDPROFILEDELNODE

 !###======================================================================
 SUBROUTINE SOLIDPROFILESPLITLINE(VCRD,VIELEV,XDIST)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN),DIMENSION(:)  :: VCRD,VIELEV
 REAL,INTENT(IN) :: XDIST
 INTEGER :: I,J,ICRD,IELEV,JCRD,JELEV
 REAL :: X1,Z1,X2,Z2

 !## shift all lines equal with x-meter down
 DO I=1,SIZE(VCRD)

  !## remove current node on ielev
  ICRD =VCRD(I); IELEV=VIELEV(I)
  !## apply current location to see whether others are equal
  X1=SPF(ISPF)%PROF(IELEV)%PX(ICRD)
  Z1=SPF(ISPF)%PROF(IELEV)%PZ(ICRD)
  
  DO J=I,SIZE(VCRD)
   JCRD=VCRD(J); JELEV=VIELEV(J)
   !## potential correct selected node
   IF(JELEV.GT.IELEV)THEN
    X2=SPF(ISPF)%PROF(JELEV)%PX(JCRD)
    Z2=SPF(ISPF)%PROF(JELEV)%PZ(JCRD)
    !## same locations - apply offset
    IF(UTL_EQUALS_REAL(X1,X2).AND.UTL_EQUALS_REAL(Z1,Z2))THEN
     SPF(ISPF)%PROF(JELEV)%PZ(JCRD)=SPF(ISPF)%PROF(JELEV)%PZ(JCRD)-XDIST
    ENDIF
   ENDIF
  ENDDO

 ENDDO
 
 END SUBROUTINE SOLIDPROFILESPLITLINE

 !###======================================================================
 SUBROUTINE SOLIDPROFILEMERGELINE(VCRD,VIELEV,IMENU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN),DIMENSION(:) :: VCRD,VIELEV,IMENU
 INTEGER :: I,J,I1,I2,ICRD,IELEV,JCRD,JELEV
 REAL :: X1,Z1,X2,Z2

 !## remove all nodes in between line to be merged onto
 DO I=1,SIZE(IMENU)
  IF(IMENU(I).LE.0)CYCLE
  !## find start/end position of selected nodes on current line
  I1=0; I2=0
  DO J=1,SIZE(VCRD)
   IF(VIELEV(J).EQ.I.AND.I1.EQ.0)I1=J
   IF(VIELEV(J).GT.I)THEN; I2=J-1; EXIT; ENDIF
  ENDDO
  IF(I2.LE.0)I2=SIZE(VCRD)
  IF(I1.NE.0.AND.I2.NE.0)CALL SOLIDPROFILEDELNODE(VCRD(I1:I2),VIELEV(I1:I2))
 ENDDO
 
 !## find line to be merged upon
 DO I=1,SIZE(IMENU); IF(IMENU(I).LT.0)THEN; IELEV=I; EXIT; ENDIF; ENDDO

 !## find selected nodes on ielev
 I1=0; I2=0
 DO J=1,SIZE(VCRD)
  IF(VIELEV(J).EQ.IELEV.AND.I1.EQ.0)I1=J
  IF(VIELEV(J).GT.IELEV)THEN; I2=J-1; EXIT; ENDIF
 ENDDO
 IF(I2.LE.0)I2=SIZE(VCRD)
 !## no nodes found to be merged upon to
 IF(I1.EQ.0)RETURN
 
 !## copy coordinates from ielev to the others
 DO I=I1,I2

  ICRD=VCRD(I)
  X1=SPF(ISPF)%PROF(IELEV)%PX(ICRD)
  Z1=SPF(ISPF)%PROF(IELEV)%PZ(ICRD)
 
  DO JELEV=1,SIZE(IMENU)
   IF(IMENU(JELEV).LE.0)CYCLE
   CALL SOLIDPROFILEPUTINTERSECTION(X1,Z1,JELEV)
  ENDDO
  
 ENDDO
 
 END SUBROUTINE SOLIDPROFILEMERGELINE

 !###======================================================================
 SUBROUTINE SOLIDPROFILELINECOLOR(IELEV)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IELEV
 INTEGER :: IRGB

 IRGB=SPF(ISPF)%PROF(IELEV)%ICLR
 CALL WSELECTCOLOUR(IRGB)
 IF(WINFODIALOG(4).NE.1)RETURN
 SPF(ISPF)%PROF(IELEV)%ICLR=IRGB

 END SUBROUTINE SOLIDPROFILELINECOLOR

 !###======================================================================
 SUBROUTINE SOLIDPROFILELINETHICKNESS(ID,IELEV)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID,IELEV

 SELECT CASE(ID)
  CASE (ID_LTHICKNESS1)
   SPF(ISPF)%PROF(IELEV)%IWIDTH=1
  CASE (ID_LTHICKNESS2)
   SPF(ISPF)%PROF(IELEV)%IWIDTH=2
  CASE (ID_LTHICKNESS3)
   SPF(ISPF)%PROF(IELEV)%IWIDTH=3
 END SELECT

 END SUBROUTINE SOLIDPROFILELINETHICKNESS

 !###======================================================================
 LOGICAL FUNCTION SOLIDPROFILEINIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 SOLIDPROFILEINIT=.FALSE.
 
 CALL WDIALOGLOAD(ID_DSERIES,ID_DSERIES)
 CALL WDIALOGSELECT(ID_DSERIESTAB2) 
 CALL WDIALOGPUTIMAGE(ID_NEW,ID_ICONNEW,1)
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE,1)
 CALL WDIALOGPUTIMAGE(ID_FIT,ID_ICONFIT,1)
 CALL WDIALOGPUTIMAGE(ID_FITDRILL,ID_ICONFITDRILL,1)
 CALL WDIALOGPUTIMAGE(IDF_CHECK2,ID_ICONGLASSES,1)
 IF(NMASK.EQ.0.AND.SHPNO.EQ.0)THEN
  IMASK=0; CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,IMASK)
  CALL WDIALOGFIELDSTATE(IDF_CHECK2,0)
 ELSE
  IMASK=1; CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,IMASK)
 ENDIF
 CALL WDIALOGPUTIMAGE(IDF_CHECK1,ID_ICONLOCK_OPEN,1) 
 ILOCK=0; CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,ILOCK)  
 CALL WDIALOGPUTIMAGE(IDF_CHECK3,ID_ICONSNAPPEN,1)
 ISSNAP=1; CALL WDIALOGPUTCHECKBOX(IDF_CHECK3,ISSNAP)
 CALL WDIALOGPUTIMAGE(IDF_CHECK4,ID_ICONFIND,1)
 IFIND=0; CALL WDIALOGPUTCHECKBOX(IDF_CHECK4,IFIND)
 ISPF=0
 IF(NSPF.GT.0)THEN
  ISPF=1; CALL WDIALOGPUTMENU(IDF_MENU1,SPF%FNAME,NSPF,ISPF)
  !## copy background bitmap information
  PBITMAP=SPF(ISPF)%PBITMAP
 ENDIF
 CALL SOLIDPROFILEFIELDS()
 
 !## open masks
 DO I=1,NMASK
  IF(.NOT.IDFREAD(MASK(I)%IDF,MASK(I)%FNAME,0))RETURN
 ENDDO

 SOLIDPROFILEINIT=.TRUE.
 
 END FUNCTION SOLIDPROFILEINIT

 !###======================================================================
 SUBROUTINE SOLIDPROFILECLOSE()
 !###======================================================================
 IMPLICIT NONE
 
 !## read entire sol, incl. spf-files
 IF(.NOT.SOLIDOPENSOL('W',GETSOLNAME(),IQ=1,TXT='Cross-Sections'))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD could not save your cross-sections','Error')
  RETURN
 ENDIF
  
 END SUBROUTINE SOLIDPROFILECLOSE

 !###======================================================================
 SUBROUTINE SOLIDPROFILEFIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DSERIESTAB2)

 I=0
 IF(NSPF.GT.0)I=1
 CALL WDIALOGFIELDSTATE(IDF_MENU1,I)

 I=0
 IF(NSPF.GT.0)THEN
  CALL WDIALOGGETMENU(IDF_MENU1,I)
  I=MIN(I,1)
 ENDIF
 CALL WDIALOGFIELDSTATE(ID_DELETE,I)
 IF(NSPF.EQ.0)I=0
 CALL WDIALOGFIELDSTATE(ID_FIT,I)

 !## active fit-drilling
 I=1
 IF(NIPF.EQ.0.OR.NSPF.EQ.0)I=0
 CALL WDIALOGFIELDSTATE(ID_FITDRILL,I)
 
 !## active new cross-section
 I=1
 IF(NSPF.GE.SIZE(SPF))I=0
 CALL WDIALOGFIELDSTATE(ID_NEW,I)

 END SUBROUTINE SOLIDPROFILEFIELDS

END MODULE MOD_SOLID_PROFILE
