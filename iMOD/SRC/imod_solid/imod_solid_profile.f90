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
MODULE MOD_SOLID_PROFILE

USE WINTERACTER
USE RESOURCE
USE MOD_DBL
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_SOLID_PAR
USE MOD_IDF, ONLY : IDFREAD,IDFGETXYVAL
USE MOD_UTL, ONLY : UTL_CAP,ITOS,UTL_SETTEXTSIZE,UTL_INVERSECOLOUR,PEUCKER_SIMPLIFYLINE,DBL_IGRINSIDEPOLYGON, &
    UTL_FILLARRAY,UTL_EQUALS_REAL,UTL_DEBUGLEVEL,UTL_GETHELP,UTL_PLOTLABEL,UTL_DIALOGSHOW
USE MOD_SOLID_UTL, ONLY : SOLIDOPENSOL,GETSOLNAME
USE MOD_PROFILE_PAR, ONLY : NXY,XY,ISOLID,PRF_IBITMAP,IWINPROFILE,MXNIDF,PROFIDF,SERIE,XMIN,YMIN,XMAX,YMAX,PBITMAP
USE MOD_PROFILE_UTL, ONLY : GRAPHUNITS,GRAPHAREA,PROFILE_EXTENT_GRAPH,PROFILE_GETLOCATION
USE MOD_IPF_PAR, ONLY : NIPF,IPF
USE MOD_IPFASSFILE, ONLY : IPFCLOSEASSFILE,IPFDIMENSIONASSFILE,ASSF
USE MOD_IPFASSFILE_UTL 
USE MOD_POLYGON_PAR
USE MOD_POLINT, ONLY : SPLINE_AKIMA_MAIN
USE MOD_QKSORT

INTEGER :: ILOCK,ISSNAP,IFIND

CONTAINS

 !###======================================================================
 SUBROUTINE SOLID_PROFILEFIT(INEW)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: INEW !## 1=new, 0=adjust
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I,J,N,ICOL,IROW
 LOGICAL :: LEX
 
 IF(ISPF.LE.0.AND.NXY.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to draw a cross-section first.','Error'); RETURN
 ENDIF
 
 CALL WDIALOGSELECT(ID_DSERIESTAB2)
 CALL UTL_DEBUGLEVEL(0)
 CALL WDIALOGGETMENU(IDF_MENU1,ISPF)
 CALL UTL_DEBUGLEVEL(1)
 
 !## copy settings for background-bitmap
 IF(ISPF.GT.0.AND.ISPF.LE.SIZE(SPF))THEN 
  !## get correct x- and y-coordinates
  CALL PROFILE_EXTENT_GRAPH(1)
  CALL DBL_IGRUNITSFROMPIXELS(PBITMAP%IX1,PBITMAP%IY1,PBITMAP%GX1,PBITMAP%GY1,IORIGIN=1)
  CALL DBL_IGRUNITSFROMPIXELS(PBITMAP%IX2,PBITMAP%IY2,PBITMAP%GX2,PBITMAP%GY2,IORIGIN=1)
  SPF(ISPF)%PBITMAP=PBITMAP
 ENDIF

 !## load solid-fit dialog
 CALL WDIALOGLOAD(ID_DSOLID_FIT,ID_DSOLID_FIT)
 
 IF(WINFOGRID(IDF_GRID1,GRIDROWSMAX).LT.NTBSOL+1)THEN
  CALL WDIALOGUNLOAD()
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot store '//TRIM(ITOS(N))//' rows in grid','Error')
  RETURN
 ENDIF
 
 IF(NSPF.GT.0)CALL WDIALOGPUTMENU(IDF_MENU1,SPF%FNAME,NSPF,ISPF)
 CALL WDIALOGFIELDSTATE(IDF_MENU1,INEW)
 
 N=NTBSOL+1; CALL WGRIDROWS(IDF_GRID1,N)
 ALLOCATE(ISEL_IDF(N),IACT(N),ICLEAN(N),XEXCLUDE(N),IEXIST(N),DTOL(N))

 !## 1 meter
 DTOL=1.0D0; XEXCLUDE=-999.99; IEXIST=1
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
 CALL WGRIDPUTDOUBLE(IDF_GRID1,5,DTOL,N)
 CALL WGRIDPUTDOUBLE(IDF_GRID1,6,XEXCLUDE,N)
 CALL WGRIDSETCELL(IDF_GRID1,1,2)
 CALL UTL_DIALOGSHOW(-1,-1,0,3)

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
        CALL WGRIDGETDOUBLE(IDF_GRID1,ICOL,DTOL,N)    
        DTOL(2:N)=DTOL(1)
        CALL WGRIDPUTDOUBLE(IDF_GRID1,ICOL,DTOL,N)
       ELSEIF(ICOL.EQ.6)THEN
        CALL WGRIDGETDOUBLE(IDF_GRID1,ICOL,XEXCLUDE,N)    
        XEXCLUDE(2:N)=XEXCLUDE(1)
        CALL WGRIDPUTDOUBLE(IDF_GRID1,ICOL,XEXCLUDE,N)
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
      IF(INEW.EQ.1)LEX=SOLID_PROFILEADD() !## add line based on iexist()
      IF(LEX)THEN
       CALL WGRIDGETCHECKBOX(IDF_GRID1,2,IACT,N)
       IACT=IACT*IEXIST
       CALL WGRIDGETCHECKBOX(IDF_GRID1,3,ICLEAN,N)
       CALL WGRIDGETMENU(IDF_GRID1,4,ISEL_IDF,N)
       CALL WGRIDGETDOUBLE(IDF_GRID1,5,DTOL,N)
       CALL WGRIDGETDOUBLE(IDF_GRID1,6,XEXCLUDE,N)
       CALL SOLID_PROFILEFITDRILL_CALC()
       EXIT
      ENDIF
     CASE (IDCANCEL)
      EXIT
     CASE (IDHELP)
       CALL UTL_GETHELP('5.4.2','TMO.ST.CrossSec')
    END SELECT
  END SELECT

 ENDDO

 DEALLOCATE(ISEL_IDF,IACT,DTOL,ICLEAN,XEXCLUDE,IEXIST)

 CALL WDIALOGSELECT(ID_DSOLID_FIT)
 CALL WDIALOGUNLOAD()

 END SUBROUTINE SOLID_PROFILEFIT

 !###======================================================================
 SUBROUTINE SOLID_PROFILEFITDRILL_CALC()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IE,IS,IIDF,N,II
 REAL(KIND=DP_KIND) :: DZ
 
 DO I=1,NTBSOL
  
  !## remove lines
  IF(IEXIST(I+1).EQ.0)THEN
   SPF(ISPF)%PROF(I)%NPOS=0
   SPF(ISPF)%PROF(I)%PX=0.0D0
   SPF(ISPF)%PROF(I)%PZ=0.0D0
   SPF(ISPF)%PROF(I)%IT=0
   CYCLE
  ENDIF
    
  !## clean entire line
  IF(ICLEAN(I+1).EQ.1)THEN
   SPF(ISPF)%PROF(I)%NPOS=0
   SPF(ISPF)%PROF(I)%PX=0.0D0
   SPF(ISPF)%PROF(I)%PZ=0.0D0
   SPF(ISPF)%PROF(I)%IT=0
  ENDIF

  !## perform fitting
  IF(IACT(I+1).EQ.1)THEN
   IIDF=ISEL_IDF(I+1)
   
   !## process each line segment in between nodata points (if they exist)  
   IS=0; II=0
   DO 
    IS=IS+1
    !## found start of connected piece of the line without nodata
    IF(SERIE(IIDF)%Y(IS).NE.XEXCLUDE(I+1))THEN
     IE=IS
     DO 
      IE=IE+1
      !## found start of connected piece of the line without nodata         
      IF(IE.GE.SERIE(IIDF)%N.OR. &   !## final reached for line
         SERIE(IIDF)%Y(IE).EQ.XEXCLUDE(I+1))THEN
       !## not final of line --- do not use nodata point in that case
       IF(IE.LT.SERIE(IIDF)%N)IE=IE-1
       
       N=IE-IS+1
       
       ALLOCATE(GCODE(N))
       !## process line       
       CALL PEUCKER_SIMPLIFYLINE(SERIE(IIDF)%X(IS:),SERIE(IIDF)%Y(IS:),GCODE,N)
       !## never remove the first or last
       GCODE(1)=DTOL(I+1)+1.0D0
       GCODE(N)=DTOL(I+1)+1.0D0
       
       IF(ICLEAN(I+1).EQ.1)THEN
       
        !## start of line
        SPF(ISPF)%PROF(I)%IT(II+1)=-1
        !## add intermediate point, take into account maximum array-size
        DO J=1,N 
         !## use point from Urs-Douglas-Peucker algorithm (greater then given tolerance)
         IF(GCODE(J).GT.DTOL(I+1))THEN

          !## put connection of current drill to 
          II=II+1
          !## maximize the size of the vector - overwrite values
          II=MIN(II,SIZE(SPF(ISPF)%PROF(I)%PX))
          SPF(ISPF)%PROF(I)%PX(II)=SERIE(IIDF)%X(IS) 
          SPF(ISPF)%PROF(I)%PZ(II)=SERIE(IIDF)%Y(IS) 
          SPF(ISPF)%PROF(I)%NPOS=II !SPF(ISPF)%PROF(I)%NPOS+1

         ENDIF
         IS=IS+1
        ENDDO
        !## end of line
        SPF(ISPF)%PROF(I)%IT(II)= 1

       ELSE
       
        IF(SPF(ISPF)%PROF(I)%NPOS.EQ.0)THEN
         !## reformulate begin
         SPF(ISPF)%PROF(I)%PX(1)=SERIE(IIDF)%X(IS)
         SPF(ISPF)%PROF(I)%PZ(1)=SERIE(IIDF)%Y(IS)
         SPF(ISPF)%PROF(I)%IT(1)=-1
         !## reformulate end point
         SPF(ISPF)%PROF(I)%PX(2)=SERIE(IIDF)%X(IE)
         SPF(ISPF)%PROF(I)%PZ(2)=SERIE(IIDF)%Y(IE)
         SPF(ISPF)%PROF(I)%IT(2)= 1
         SPF(ISPF)%PROF(I)%NPOS=2
        ELSE
         CALL SOLID_PROFILEPUTINTERSECTION(SERIE(IIDF)%X(IS),SERIE(IIDF)%Y(IS),I)
         CALL SOLID_PROFILEPUTINTERSECTION(SERIE(IIDF)%X(IE),SERIE(IIDF)%Y(IE),I)
        ENDIF

        !## add intermediate point, take into account maximum array-size
        DO J=2,N 
         !## use point from Urs-Douglas-Peucker algorithm (greater then given tolerance)
         IF(GCODE(J).GT.DTOL(I+1))THEN
          !## cannot add more points          
          IF(SPF(ISPF)%PROF(I)%NPOS.GE.SIZE(SPF(ISPF)%PROF(I)%PX))EXIT
          !## put connection of current drill to 
          CALL SOLID_PROFILEPUTINTERSECTION(SERIE(IIDF)%X(IS+J-1),SERIE(IIDF)%Y(IS+J-1),I)
         ENDIF
        ENDDO
       
       ENDIF
       
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
    DZ=(GRAPHUNITS(4,1)-GRAPHUNITS(2,1))/REAL(NTBSOL,8)
    SPF(ISPF)%PROF(I)%NPOS=2
    SPF(ISPF)%PROF(I)%PX(1)=0.0D0
    SPF(ISPF)%PROF(I)%PX(2)=SPF(ISPF)%TX
    SPF(ISPF)%PROF(I)%IT(1)=-1
    SPF(ISPF)%PROF(I)%PZ(1)=GRAPHUNITS(4,1)-DZ*REAL(I-1,8)
    SPF(ISPF)%PROF(I)%PZ(2)=SPF(ISPF)%PROF(I)%PZ(1)
    SPF(ISPF)%PROF(I)%IT(2)= 1
   ENDIF

  ENDIF
 ENDDO
  
 END SUBROUTINE SOLID_PROFILEFITDRILL_CALC
 
 !###======================================================================
 SUBROUTINE SOLID_PROFILEFITDRILL()
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: MINDIST=1.0D0 !## minimum distance 1.0 meter
 INTEGER :: IIPF,I,IELEV !,N
 INTEGER,ALLOCATABLE,DIMENSION(:) :: NFOUND
 REAL(KIND=DP_KIND) :: X,Y,Z,LENPROF
 CHARACTER(LEN=256) :: FNAME,DIR
 
 !## save number of points found
 ALLOCATE(NFOUND(SIZE(SPF(ISPF)%PROF))); NFOUND=0
 
 !## allocate and nullify pointers
 CALL IPFASSFILEALLOCATE(1)

 !## reset entire cross-sectional lines
 DO IELEV=1,SIZE(SPF(ISPF)%PROF)
  SPF(ISPF)%PROF(IELEV)%NPOS=0
  SPF(ISPF)%PROF(IELEV)%NPOS=2
  SPF(ISPF)%PROF(IELEV)%PX(1)=0.0D0    
  SPF(ISPF)%PROF(IELEV)%PX(2)=SPF(ISPF)%TX
 ENDDO

 LENPROF=SPF(ISPF)%PROF(1)%PX(2)

 DO IIPF=1,NIPF
  IF(IPF(IIPF)%ACOL.NE.0)THEN
   I  =INDEXNOCASE(IPF(IIPF)%FNAME,'\',.TRUE.)
   DIR=IPF(IIPF)%FNAME(1:I-1)
   DO I=1,IPF(IIPF)%NROW
    !## borehole in cross-section
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
       !## skip if z.eq.nodata       
       IF(Z.EQ.ASSF(1)%NODATA(1))CYCLE
       NFOUND(IELEV)=NFOUND(IELEV)+1
       !## put connection of current drill to 
       CALL SOLID_PROFILEPUTINTERSECTION(X,Z,IELEV)
      ENDDO
     ENDIF
    ENDIF
   ENDDO
  ENDIF
 ENDDO

 !## reset entire cross-sectional lines(s) if nothing found here
 DO IELEV=1,SIZE(SPF(ISPF)%PROF)
  IF(NFOUND(IELEV).EQ.0)SPF(ISPF)%PROF(IELEV)%NPOS=0
 ENDDO

 CALL IPFCLOSEASSFILE()

 END SUBROUTINE SOLID_PROFILEFITDRILL
 
 !###======================================================================
 SUBROUTINE SOLID_PROFILEINTERSECT(DXB,DYB)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: DXB,DYB
 INTEGER :: I,J,K,JSPF,IS
 REAL(KIND=DP_KIND),DIMENSION(2) :: X1,Y1,X2,Y2
 REAL(KIND=DP_KIND) :: XI,YI,DX,Z
! CHARACTER(LEN=52) :: ALIAS
 
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
       CALL DBL_IGRINTERSECTLINE(X1(1),Y1(1),X1(2),Y1(2),&
                             X2(1),Y2(1),X2(2),Y2(2), &
                             XI,YI,IS)
      ENDIF
      !## they intersect
      IF(IS.EQ.5)THEN
       !## get distance on other segment
       DX=SOLID_PROFILEDISTANCE(K,JSPF,XI,YI)
       !## get value
       IF(SOLID_PROFILEZVALUE(DX,JSPF,I,Z))THEN
        !## get distance on current segment
        DX=SOLID_PROFILEDISTANCE(J,ISPF,XI,YI)

        CALL DBL_IGRRECTANGLE(DX-(0.5*DXB),Z-(0.5*DYB),DX+(0.5*DXB),Z+(0.5*DYB))
        CALL DBL_WGRTEXTSTRING(DX+DXB,Z,SPF(JSPF)%FNAME(:INDEX(SPF(JSPF)%FNAME,'.',.TRUE.)-1)//';Interface:'//TRIM(ITOS(I))) 

!        IF(ALLOCATED(PROFIDF))THEN   
!         IF(I.LE.SIZE(PROFIDF))THEN        
!          ALIAS=PROFIDF(I)%IDF%FNAME(INDEX(PROFIDF(I)%IDF%FNAME,'\',.TRUE.)+1:)
!          CALL DBL_WGRTEXTSTRING(DX+DXB,Z,SPF(JSPF)%FNAME(:INDEX(SPF(JSPF)%FNAME,'.',.TRUE.)-1)//';'//TRIM(ALIAS)) 
!         ELSE
!          CALL DBL_WGRTEXTSTRING(DX+DXB,Z,SPF(JSPF)%FNAME(:INDEX(SPF(JSPF)%FNAME,'.',.TRUE.)-1)//';Interface:'//TRIM(ITOS(I))) 
!         ENDIF
!        ELSE
!         CALL DBL_WGRTEXTSTRING(DX+DXB,Z,SPF(JSPF)%FNAME(:INDEX(SPF(JSPF)%FNAME,'.',.TRUE.)-1)//';Interface:'//TRIM(ITOS(I))) 
!        ENDIF

!       !## put intersection value to current isp-cross-section
!       CALL SOLID_PROFILEPUTINTERSECTION(DX,Z,I)
       ENDIF
       EXIT
      ENDIF   
     ENDDO
    ENDIF
   ENDDO
  ENDDO
 ENDDO

 END SUBROUTINE SOLID_PROFILEINTERSECT

 !###======================================================================
 SUBROUTINE SOLID_PROFILEPUTINTERSECTION(X,Z,IL)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: MINDIST=1.0D0 !## minimum distance 1 meter
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Z
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
 IF(LEX)THEN 
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
    CALL SOLID_PROFILEINSERTNODE(ISPF,IL,ICRD,X,Z)
   ENDIF
   EXIT
  ENDIF
 ENDDO
   
 END SUBROUTINE SOLID_PROFILEPUTINTERSECTION

 !###======================================================================
 LOGICAL FUNCTION SOLID_PROFILEZVALUE(X,JSPF,IL,Z)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IL,JSPF
 REAL(KIND=DP_KIND),INTENT(IN) :: X
 REAL(KIND=DP_KIND),INTENT(OUT) :: Z
 INTEGER :: I
 REAL(KIND=DP_KIND) :: DX,DZ
 
 SOLID_PROFILEZVALUE=.FALSE.
 
 !## get distance till now:
 DO I=2,SPF(JSPF)%PROF(IL)%NPOS
  IF(SPF(JSPF)%PROF(IL)%PX(I)-X.GE.-0.01D0)THEN
   DZ=SPF(JSPF)%PROF(IL)%PZ(I)-SPF(JSPF)%PROF(IL)%PZ(I-1)
   DX=(X-SPF(JSPF)%PROF(IL)%PX(I-1)) / (SPF(JSPF)%PROF(IL)%PX(I)-SPF(JSPF)%PROF(IL)%PX(I-1))
   Z=SPF(JSPF)%PROF(IL)%PZ(I-1)+DX*DZ
   SOLID_PROFILEZVALUE=.TRUE.
   EXIT
  ENDIF
 ENDDO
   
 END FUNCTION SOLID_PROFILEZVALUE

 !###======================================================================
 REAL(KIND=DP_KIND) FUNCTION SOLID_PROFILEDISTANCE(I,JSPF,XI,YI)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I,JSPF
 REAL(KIND=DP_KIND),INTENT(IN) :: XI,YI
 INTEGER :: II
 REAL(KIND=DP_KIND) :: TX,DX,DY
 
 !## get distance till now:
 TX=0.0D0
 DO II=2,I-1 !2
  DX=(SPF(JSPF)%X(II)-SPF(JSPF)%X(II-1))**2.0D0
  DY=(SPF(JSPF)%Y(II)-SPF(JSPF)%Y(II-1))**2.0D0
  IF(DX+DY.GT.0.0D0)TX=TX+SQRT(DX+DY)
 ENDDO
 !## add distance to intersection point
 DX=(XI-SPF(JSPF)%X(I-1))**2.0D0
 DY=(YI-SPF(JSPF)%Y(I-1))**2.0D0
 IF(DX+DY.GT.0.0D0)TX=TX+SQRT(DX+DY)     
 
 SOLID_PROFILEDISTANCE=TX
 
 END FUNCTION SOLID_PROFILEDISTANCE

 !###======================================================================
 LOGICAL FUNCTION SOLID_PROFILEDELETE(ID,N,ISEL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID,N
 INTEGER,DIMENSION(:),INTENT(IN) :: ISEL
 INTEGER :: I,II,J,JJ,K
 CHARACTER(LEN=256) :: LINE
 
 SOLID_PROFILEDELETE=.FALSE.
 
 LINE='['
 K=0; DO I=1,N 
  IF(ISEL(I).EQ.1)THEN
   IF(K.EQ.0)THEN
    LINE=TRIM(LINE)//TRIM(SPF(I)%FNAME); K=K+1
   ELSE
    LINE=TRIM(LINE)//CHAR(13)//TRIM(SPF(I)%FNAME)
   ENDIF
  ENDIF
 ENDDO
 LINE=TRIM(LINE)//']'

 IF(ID.EQ.ID_DSERIESTAB2)THEN 
  
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'You are about to remove the selected Cross-Section:' &
      //CHAR(13)//CHAR(13)//TRIM(LINE)//CHAR(13)//CHAR(13)//'from the Solid.'//CHAR(13)// &
   'Be aware that the Cross-Section will be removed from the list and NOT from the Solid folder.'//CHAR(13)// &
   'Any recovery can take place, manually by editing the *.sol file.'//CHAR(13)//CHAR(13)// &
   'Are you sure to continue?','Question')

 ELSEIF(ID.EQ.ID_D3DSETTINGS_TAB6)THEN

  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'You are about to remove the selected Cross-Section(s):' &
       //CHAR(13)//CHAR(13)//TRIM(LINE)//CHAR(13)//CHAR(13)// &
   'from the list. Be aware that the removed Cross-Section'//CHAR(13)// &
   'cannot be restored. Consider to save the'//CHAR(13)// &
   'cross-section prior to deletion.'//CHAR(13)//CHAR(13)// &
   'Are you sure to continue?','Question')

 ENDIF
 
 IF(WINFODIALOG(4).NE.1)RETURN

 IF(ID.EQ.ID_DSERIESTAB2)CALL SOLID_PROFILEFIELDS()

 I=0; DO II=1,N
 
  !## skip the ones that are selected
  IF(ISEL(II).EQ.1)CYCLE
  !## skip as this one does not to be deleted
  I=I+1; IF(I.EQ.II)CYCLE
  
  !## deallocate memory
  IF(ASSOCIATED(SPF(I)%X))DEALLOCATE(SPF(I)%X)
  IF(ASSOCIATED(SPF(I)%Y))DEALLOCATE(SPF(I)%Y)
  SPF(I)%NXY=SPF(II)%NXY; ALLOCATE(SPF(I)%X(SPF(I)%NXY),SPF(I)%Y(SPF(I)%NXY))
  SPF(I)%X=0.0D0; SPF(I)%Y=0.0D0
  !## copy coordinates
  DO JJ=1,SPF(I)%NXY
   SPF(I)%X(JJ)=SPF(II)%X(JJ); SPF(I)%Y(JJ)=SPF(II)%Y(JJ)
  ENDDO
  !## copy spf name
  SPF(I)%FNAME=SPF(II)%FNAME
  !## copy total distance
  SPF(I)%TX=SPF(II)%TX
  !## copy bitmap settings
  SPF(I)%PBITMAP=SPF(II)%PBITMAP

  !## copy information for cross-sections
  DO K=1,SIZE(SPF(I)%PROF)
   !## deallocate memory for cross-sections
   IF(ASSOCIATED(SPF(I)%PROF(K)%PX))DEALLOCATE(SPF(I)%PROF(K)%PX)
   IF(ASSOCIATED(SPF(I)%PROF(K)%PZ))DEALLOCATE(SPF(I)%PROF(K)%PZ)
   IF(ASSOCIATED(SPF(I)%PROF(K)%IT))DEALLOCATE(SPF(I)%PROF(K)%IT)
   SPF(I)%PROF(K)%NPOS=SPF(II)%PROF(K)%NPOS
   ALLOCATE(SPF(I)%PROF(K)%PX(MXPX)); SPF(I)%PROF(K)%PX=0.0D0
   ALLOCATE(SPF(I)%PROF(K)%PZ(MXPX)); SPF(I)%PROF(K)%PZ=0.0D0
   ALLOCATE(SPF(I)%PROF(K)%IT(MXPX)); SPF(I)%PROF(K)%IT=0
   DO JJ=1,SPF(I)%PROF(K)%NPOS
    SPF(I)%PROF(K)%PX(JJ)=SPF(II)%PROF(K)%PX(JJ)
    SPF(I)%PROF(K)%PZ(JJ)=SPF(II)%PROF(K)%PZ(JJ)
    SPF(I)%PROF(K)%IT(JJ)=SPF(II)%PROF(K)%IT(JJ)
   ENDDO
   SPF(I)%PROF(K)%ICLR  =SPF(II)%PROF(K)%ICLR
   SPF(I)%PROF(K)%IWIDTH=SPF(II)%PROF(K)%IWIDTH
  ENDDO 
 ENDDO

 !## free memory resource
 K=I+1; DO I=K,NSPF
  IF(ASSOCIATED(SPF(I)%X))DEALLOCATE(SPF(I)%X)
  IF(ASSOCIATED(SPF(I)%Y))DEALLOCATE(SPF(I)%Y)
  IF(ASSOCIATED(SPF(I)%ICOMBINE))DEALLOCATE(SPF(I)%ICOMBINE)
  DO J=1,SIZE(SPF(I)%PROF)
   IF(ASSOCIATED(SPF(I)%PROF(J)%PX))DEALLOCATE(SPF(I)%PROF(J)%PX)
   IF(ASSOCIATED(SPF(I)%PROF(J)%PZ))DEALLOCATE(SPF(I)%PROF(J)%PZ)
   IF(ASSOCIATED(SPF(I)%PROF(J)%IT))DEALLOCATE(SPF(I)%PROF(J)%IT)
  ENDDO
  IF(ASSOCIATED(SPF(I)%PROF))DEALLOCATE(SPF(I)%PROF)
  SPF(I)%NXY=0
  SPF(I)%FNAME=''
  SPF(I)%TX=0.0D0
  SPF(I)%PBITMAP%IACT=0
 ENDDO

 NSPF=K-1; NSPF=MAX(NSPF,0)
 
 CALL WDIALOGSELECT(ID)
 
 IF(ID.EQ.ID_DSERIESTAB2)THEN
  IF(NSPF.EQ.0)THEN
   CALL WDIALOGCLEARFIELD(IDF_MENU1)
  ELSE
   CALL WDIALOGPUTMENU(IDF_MENU1,SPF%FNAME,NSPF,NSPF)
  ENDIF
  CALL SOLID_PROFILEFIELDS()
 ENDIF
  
 SOLID_PROFILEDELETE=.TRUE.

 END FUNCTION SOLID_PROFILEDELETE

 !###======================================================================
 LOGICAL FUNCTION SOLID_PROFILEADD()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,DID
  
 SOLID_PROFILEADD=.FALSE.
 
 DID=WINFODIALOG(CURRENTDIALOG)

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

 CALL SOLID_PROFILEADD_SPFMEMORY(GRAPHUNITS(4,1),GRAPHUNITS(2,1))
 
 CALL WDIALOGSELECT(ID_DSERIESTAB2)
 CALL WDIALOGPUTMENU(IDF_MENU1,SPF%FNAME,NSPF,NSPF)
 CALL SOLID_PROFILEFIELDS()
 
 ISPF=NSPF
 !## turn off a bitmap for this new cross-section
 PBITMAP%IACT=0; 

 IF(DID.NE.0)CALL WDIALOGSELECT(DID)

 SOLID_PROFILEADD=.TRUE.
 
 END FUNCTION SOLID_PROFILEADD

 !###======================================================================
 SUBROUTINE SOLID_PROFILEADD_SPFMEMORY(Z2,Z1)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: Z1,Z2
 INTEGER :: I
 REAL(KIND=DP_KIND) :: DX,DY,DZ,TX
 
 !## copy current coordinates of profile ...
 SPF(NSPF)%NXY=NXY
 ALLOCATE(SPF(NSPF)%X(NXY),SPF(NSPF)%Y(NXY),SPF(NSPF)%ICOMBINE(NTBSOL,3))
 SPF(NSPF)%X(1:NXY)=XY(1,1:NXY)
 SPF(NSPF)%Y(1:NXY)=XY(2,1:NXY)
 SPF(NSPF)%ICOMBINE=0
 !## get distance of cross-section
 TX=0.0D0
 DO I=1,NXY
  IF(I.GT.1)THEN
   DX=(SPF(NSPF)%X(I)-SPF(NSPF)%X(I-1))**2.0D0
   DY=(SPF(NSPF)%Y(I)-SPF(NSPF)%Y(I-1))**2.0D0
   IF(DX+DY.NE.0.0D0)TX=TX+SQRT(DX+DY)
  ENDIF
 ENDDO
 SPF(NSPF)%TX=TX
  
 !## allocate number of elevation (max. NTBSOL)
 ALLOCATE(SPF(NSPF)%PROF(NTBSOL))

 DZ=(Z2-Z1)/REAL(NTBSOL,8)
 DO I=1,SIZE(SPF(NSPF)%PROF) 
  SPF(NSPF)%PROF(I)%NPOS=0
  ALLOCATE(SPF(NSPF)%PROF(I)%PX(MXPX))
  ALLOCATE(SPF(NSPF)%PROF(I)%PZ(MXPX))
  ALLOCATE(SPF(NSPF)%PROF(I)%IT(MXPX))
  IF(IEXIST(I+1).EQ.1)THEN
   SPF(NSPF)%PROF(I)%NPOS=2
   !## start of cross-section
   SPF(NSPF)%PROF(I)%IT(1)=-1
   SPF(NSPF)%PROF(I)%PX(1)=0.0D0    
   !## end of cross-section
   SPF(NSPF)%PROF(I)%IT(2)= 1
   SPF(NSPF)%PROF(I)%PX(2)=TX
   !## mean vertical depth as first guess!
   SPF(NSPF)%PROF(I)%PZ(1)=Z2-DZ*REAL(I-1,8) 
   SPF(NSPF)%PROF(I)%PZ(2)=SPF(NSPF)%PROF(I)%PZ(1)
  ENDIF
  IF(MOD(I,2).NE.0)SPF(NSPF)%PROF(I)%ICLR  =WRGB(255,0,0)
  IF(MOD(I,2).EQ.0)SPF(NSPF)%PROF(I)%ICLR  =WRGB(0,0,255)
  SPF(NSPF)%PROF(I)%IWIDTH=2
  IF(ALLOCATED(SLD))THEN
   IF(SIZE(SLD(1)%INTNAME).GE.I)THEN
    SPF(NSPF)%PROF(I)%LNAME=SLD(1)%INTNAME(I)(INDEX(SLD(1)%INTNAME(I),'\',.TRUE.)+1:INDEX(SLD(1)%INTNAME(I),'.',.TRUE.)-1)
   ELSE
    SPF(NSPF)%PROF(I)%LNAME='TEST'
   ENDIF
  ELSE
   SPF(NSPF)%PROF(I)%LNAME='TEST'
  ENDIF
  SPF(NSPF)%PROF(I)%IACTIVE=1
 ENDDO

 END SUBROUTINE SOLID_PROFILEADD_SPFMEMORY

 !###======================================================================
 SUBROUTINE SOLID_PROFILEDRAW(IS1,IS2,ICRD,ICURSOR,IWINID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IS1,IS2,ICRD,ICURSOR,IWINID
 INTEGER :: I,J,J1,J2,N,M,ISPLINE
 REAL(KIND=DP_KIND) :: DX,DY,X1,DXSPLINE
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: XI,ZI
 
 CALL WINDOWSELECT(0); ISPLINE=WMENUGETSTATE(ID_DRAWLINEASSPLINE,2)
 
 !## nothing selected
 IF(ISPF.LE.0)RETURN
 !## nothing draw/adjusted before
 IF(IS1.EQ.0)RETURN

 !## select proper bitmap
 CALL IGRSELECT(DRAWBITMAP,PRF_IBITMAP(IWINID))
 !## change plotmode
 CALL IGRPLOTMODE(MODEXOR)
 !## set area/units
 CALL DBL_IGRAREA (GRAPHAREA (1,IWINID),GRAPHAREA(2,IWINID) ,GRAPHAREA(3,IWINID) ,GRAPHAREA(4,IWINID))
 CALL DBL_IGRUNITS(GRAPHUNITS(1,IWINID),GRAPHUNITS(2,IWINID),GRAPHUNITS(3,IWINID),GRAPHUNITS(4,IWINID))

 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRFILLPATTERN(OUTLINE)
 
 DX=(GRAPHUNITS(3,IWINID)-GRAPHUNITS(1,IWINID))/500.0D0
 DY=(GRAPHUNITS(4,IWINID)-GRAPHUNITS(2,IWINID))/500.0D0*WINFOGRREAL(GRAPHICSRATIO)
                  
 !## draw profile lines
 DO I=IS1,IS2 
  IF(SPF(ISPF)%PROF(I)%NPOS.LE.0)CYCLE
  CALL IGRLINEWIDTH(SPF(ISPF)%PROF(I)%IWIDTH)
  CALL IGRCOLOURN(UTL_INVERSECOLOUR(SPF(ISPF)%PROF(I)%ICLR))

  !## skip lines that do not need to be processed
  IF(SPF(ISPF)%PROF(I)%IACTIVE.EQ.0)CALL IGRLINETYPE(DOTTED)

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

  IF(ISPLINE.EQ.0)THEN

   DO J=J1,J2 
    CALL DBL_IGRJOIN(PX(J-1),PZ(J-1),PX(J),PZ(J))
   ENDDO

  !## plot spline line optionally
  ELSEIF(ISPLINE.EQ.1)THEN
  
   !## create spline-points
   N=SPF(ISPF)%PROF(I)%NPOS
   !## total length of line
   DXSPLINE=SPF(ISPF)%PROF(I)%PX(N)-SPF(ISPF)%PROF(I)%PX(1)
   !## spline points maximal
   M=MAX(100,N); DXSPLINE=DXSPLINE/REAL(M)
   ALLOCATE(XI(M),ZI(M)); XI=0.0D0; ZI=0.0D0
   X1=SPF(ISPF)%PROF(I)%PX(1)-DXSPLINE
   DO J=1,M; X1=X1+DXSPLINE; XI(J)=X1; ENDDO   
   !## spline line
   CALL SPLINE_AKIMA_MAIN(SPF(ISPF)%PROF(I)%PX,SPF(ISPF)%PROF(I)%PZ,SPF(ISPF)%PROF(I)%NPOS,XI,ZI,SIZE(XI))  
!   CALL IGRLINEWIDTH(1); CALL IGRCOLOURN(WRGB(50,50,50))
   DO J=2,SIZE(XI); CALL DBL_IGRJOIN(XI(J-1),ZI(J-1),XI(J),ZI(J)); ENDDO
  ENDIF
  
  CALL IGRLINETYPE(SOLIDLINE)
  
  !## draw rectangles
  DO J=J1,J2 
   CALL DBL_IGRRECTANGLE(PX(J-1)-DX,PZ(J-1)+DY,PX(J-1)+DX,PZ(J-1)-DY)
  ENDDO
  CALL DBL_IGRRECTANGLE(PX(J-1)-DX,PZ(J-1)+DY,PX(J-1)+DX,PZ(J-1)-DY)

  NULLIFY(PX,PZ)

 ENDDO
   
 CALL IGRCOLOURN(WRGB(0,0,0)); CALL IGRLINEWIDTH(1); CALL IGRPLOTMODE(MODEAND)

 END SUBROUTINE SOLID_PROFILEDRAW

 !###======================================================================
 SUBROUTINE SOLID_PROFILEDRAW_INTERSECTIONS(IWINID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IWINID
 REAL(KIND=DP_KIND) :: DX,DY,TWIDTH,THEIGHT

 IF(IFIND.EQ.0)RETURN
 
 CALL IGRPLOTMODE(MODECOPY) 
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRCOLOURN(WRGB(255,110,110))

 !## select proper bitmap
 CALL IGRSELECT(DRAWBITMAP,PRF_IBITMAP(IWINID))
 !## set area/units
 CALL DBL_IGRAREA (GRAPHAREA (1,IWINID),GRAPHAREA(2,IWINID) ,GRAPHAREA(3,IWINID) ,GRAPHAREA(4,IWINID))
 CALL DBL_IGRUNITS(GRAPHUNITS(1,IWINID),GRAPHUNITS(2,IWINID),GRAPHUNITS(3,IWINID),GRAPHUNITS(4,IWINID))
 
 DX=(GRAPHUNITS(3,IWINID)-GRAPHUNITS(1,IWINID))/250.0D0
 DY=(GRAPHUNITS(4,IWINID)-GRAPHUNITS(2,IWINID))/250.0D0*WINFOGRREAL(GRAPHICSRATIO)
 
! !## get current graph-dimensions
! X1 =INFOGRAPHICS(GRAPHICSUNITMINX); X2 =INFOGRAPHICS(GRAPHICSUNITMAXX)
! Y1 =INFOGRAPHICS(GRAPHICSUNITMINY); Y2 =INFOGRAPHICS(GRAPHICSUNITMAXY)
 !CALL DBL_WGRTEXTFONT(AXES%TFONT,WIDTH=AXES%CHW,HEIGHT=AXES%CHH,ISTYLE=0)

 CALL UTL_SETTEXTSIZE(TWIDTH,THEIGHT,FCT=6.0D0)
! CALL DBL_WGRTEXTORIENTATION(ALIGNLEFT,ANGLE=0.0D0)

!## added
 CALL IGRCOLOURN(WRGB(0,0,0))
 CALL DBL_WGRTEXTFONT(IFAMILY=FFHELVETICA,TWIDTH=TWIDTH,THEIGHT=THEIGHT,ISTYLE=FSOPAQUE) 
 
 CALL SOLID_PROFILEINTERSECT(DX,DY)
 
 CALL DBL_WGRTEXTORIENTATION(ALIGNLEFT,ANGLE=0.0D0)
 CALL DBL_WGRTEXTFONT(IFAMILY=FFHELVETICA,ISTYLE=0)
 CALL IGRPLOTMODE(MODECOPY)
 
 END SUBROUTINE SOLID_PROFILEDRAW_INTERSECTIONS
 
 !###======================================================================
 SUBROUTINE SOLID_PROFILEDRAW_MASK()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IWINID,I,J,IIDF
 REAL(KIND=DP_KIND) :: XC,YC
  
 !## nothing to do
 IF(NMASK.LE.0)RETURN
 IF(IMASK.EQ.0)RETURN
 
! CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot display mask yet!','Error')
 RETURN
 
 CALL IGRPLOTMODE(MODECOPY) !AND)
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRFILLPATTERN(SOLID)
 CALL IGRCOLOURN(WRGB(110,110,110))

! DO IWINID=1,SIZE(IWINPROFILE)

 !## only for mother screen
 IWINID=1

 !## select proper bitmap
 CALL IGRSELECT(DRAWBITMAP,PRF_IBITMAP(IWINID))
 !## set area/units
 CALL DBL_IGRAREA (GRAPHAREA (1,IWINID),GRAPHAREA(2,IWINID) ,GRAPHAREA(3,IWINID) ,GRAPHAREA(4,IWINID))
 CALL DBL_IGRUNITS(GRAPHUNITS(1,IWINID),GRAPHUNITS(2,IWINID),GRAPHUNITS(3,IWINID),GRAPHUNITS(4,IWINID))

 !## first of profidf() zijn target idf ... since they have the same resolution ... trust that!
 
 J=0
 DO IIDF=2,MXNIDF-1,2
  J=J+1
  !## plot area inside mask
  IF(PROFIDF(IIDF)%ISCREEN.EQ.1.AND.PROFIDF(IIDF+1)%ISCREEN.EQ.1)THEN
   DO I=1,SERIE(IIDF)%N
    IF(PROFILE_GETLOCATION(XC,YC,SERIE(IIDF)%X(I)))THEN; ENDIF
    !## point on profile inside mask
    IF(IDFGETXYVAL(MASK(J)%IDF,XC,YC).NE.MASK(J)%IDF%NODATA)THEN

     !## start rectangle
     IF(I.EQ.1)THEN
      IF(SERIE(IIDF)%Y(I).NE.PROFIDF(IIDF)  %IDF%NODATA.AND. &
         SERIE(IIDF)%Y(I).NE.PROFIDF(IIDF+1)%IDF%NODATA) &
       CALL DBL_IGRRECTANGLE( SERIE(IIDF)%X(I)                        ,SERIE(IIDF+1)%Y(I), &
                         (SERIE(IIDF)%X(I)+SERIE(IIDF)%X(I+1))/2.0,SERIE(IIDF)  %Y(I))   
     ELSEIF(I.EQ.SERIE(IIDF)%N)THEN 
      IF(SERIE(IIDF)%Y(I).NE.PROFIDF(IIDF)  %IDF%NODATA.AND. &
         SERIE(IIDF)%Y(I).NE.PROFIDF(IIDF+1)%IDF%NODATA) &
       CALL DBL_IGRRECTANGLE((SERIE(IIDF)%X(I)+SERIE(IIDF)%X(I-1))/2.0,SERIE(IIDF+1)%Y(I), &
                          SERIE(IIDF)%X(I)                        ,SERIE(IIDF)  %Y(I))   
     ELSE
      IF(SERIE(IIDF)  %Y(I)  .NE.PROFIDF(IIDF)  %IDF%NODATA.AND. &
         SERIE(IIDF)  %Y(I+1).NE.PROFIDF(IIDF)  %IDF%NODATA.AND. &
         SERIE(IIDF+1)%Y(I)  .NE.PROFIDF(IIDF+1)%IDF%NODATA.AND. &
         SERIE(IIDF+1)%Y(I+1).NE.PROFIDF(IIDF+1)%IDF%NODATA) &
       CALL DBL_IGRRECTANGLE((SERIE(IIDF)%X(I)+SERIE(IIDF)%X(I-1))/2.0,SERIE(IIDF+1)%Y(I), &
                         (SERIE(IIDF)%X(I)+SERIE(IIDF)%X(I+1))/2.0,SERIE(IIDF)  %Y(I))
     ENDIF
    
    ENDIF
   ENDDO
  ENDIF
  
!  !## colour cross-section grey
!  IF(NX.EQ.0)THEN
!   CALL DBL_IGRRECTANGLE(XMIN,YMIN,XMAX,YMAX)
!  !## colour those sections grey that are outside current polygon
!  ELSE
!   ION=JON
!   DO I=1,NX-1 
!    IF(ION.EQ.0)CALL DBL_IGRRECTANGLE(X(I),YMIN,X(I+1),YMAX)
!    ION=ABS(ION-1)
!   ENDDO
!  ENDIF
 
 ENDDO

! ENDDO 

 CALL IGRPLOTMODE(MODECOPY)
 
 END SUBROUTINE SOLID_PROFILEDRAW_MASK

 !###======================================================================
 SUBROUTINE SOLID_PROFILEDRAW_POLYGON()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,ISTATUS,NX,ION,JON,IWINID
 REAL(KIND=DP_KIND) :: XINTER,YINTER,D,TD
 REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: X
  
 !## nothing to do
 IF(SHP%NPOL.LE.0)RETURN
 IF(IMASK.EQ.0)RETURN
 
 ALLOCATE(X(100))

 !## intersect lines of polygon with lines of cross-section 
 NX=0
 TD=0.0D0
 D =0.0D0
 DO I=1,NXY-1
  TD=TD+D
  DO J=1,SHP%NPOL
   DO K=1,SHP%POL(J)%N
    IF(K.EQ.SHP%POL(J)%N)THEN
     CALL DBL_IGRINTERSECTLINE(XY(1,I)   ,XY(2,I)   ,XY(1,I+1)   ,XY(2,I+1),    &  !## coordinates of cross-sectional line
                           SHP%POL(J)%X(K),SHP%POL(J)%Y(K),SHP%POL(J)%X(1)  ,SHP%POL(J)%Y(1),   &  !## coordinates of polygon
                           XINTER,YINTER,ISTATUS)
    ELSE
     CALL DBL_IGRINTERSECTLINE(XY(1,I)   ,XY(2,I)   ,XY(1,I+1)   ,XY(2,I+1),    & !## coordinates of cross-sectional line
                           SHP%POL(J)%X(K),SHP%POL(J)%Y(K),SHP%POL(J)%X(K+1),SHP%POL(J)%Y(K+1), & !## coordinates of polygon
                           XINTER,YINTER,ISTATUS)
    ENDIF
    IF(ISTATUS.EQ.5)THEN
     NX=NX+1
     X(NX)=TD+SQRT((XY(1,I)-XINTER)**2.0D0+(XY(2,I)-YINTER)**2.0D0)  
    ENDIF
   END DO
  ENDDO
  D=SQRT((XY(1,I)-XY(1,I+1))**2.0D0+(XY(2,I)-XY(2,I+1))**2.0D0)   
 ENDDO
 TD=TD+D
 
 !## sort distances
 NX=NX+1
 X(NX)=0.0D0
 NX=NX+1
 X(NX)=TD
 CALL QKSORT_SGL(NX,X)
 
 !## default outside unless otherwise
 JON=0
 DO J=1,SHP%NPOL
  !## check FIRST point inside/outside polygon j
  ION=DBL_IGRINSIDESHAPE(XY(1,1),XY(2,1),SHP%POL(J))
!  ION=DBL_IGRINSIDEPOLYGON(XY(1,1),XY(2,1),SHP%POL(J)%X,SHP%POL(J)%Y,SHP%POL(J)%N)
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
  CALL IGRSELECT(DRAWBITMAP,PRF_IBITMAP(IWINID))
  !## set area/units
  CALL DBL_IGRAREA (GRAPHAREA (1,IWINID),GRAPHAREA(2,IWINID) ,GRAPHAREA(3,IWINID) ,GRAPHAREA(4,IWINID))
  CALL DBL_IGRUNITS(GRAPHUNITS(1,IWINID),GRAPHUNITS(2,IWINID),GRAPHUNITS(3,IWINID),GRAPHUNITS(4,IWINID))

  !## colour cross-section grey
  IF(NX.EQ.0)THEN
   CALL DBL_IGRRECTANGLE(XMIN,YMIN,XMAX,YMAX)
  !## colour those sections grey that are outside current polygon
  ELSE
   ION=JON
   DO I=1,NX-1 
    IF(ION.EQ.0)CALL DBL_IGRRECTANGLE(X(I),YMIN,X(I+1),YMAX)
    ION=ABS(ION-1)
   ENDDO
  ENDIF
 
 ENDDO
 
 CALL IGRPLOTMODE(MODECOPY)
 
 DEALLOCATE(X)

 END SUBROUTINE SOLID_PROFILEDRAW_POLYGON

 !###======================================================================
 SUBROUTINE SOLID_PLOTLOCATION_CROSSSECTIONS()
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: RAD=360.0D0/(2.0D0*3.1415D0)
 INTEGER :: I,J,ILABEL
 REAL(KIND=DP_KIND) :: TW,TH,DX,DY,GANGLE,SLABEL
 CHARACTER(LEN=50) :: FNAME
  
 CALL WDIALOGSELECT(ID_DSOLIDTAB1)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,ILABEL)
 CALL WDIALOGGETDOUBLE(IDF_REAL1,SLABEL)

 CALL UTL_SETTEXTSIZE(TW,TH,FCT=SLABEL)
 
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
    CALL DBL_IGRJOIN(PX(J-1),PY(J-1),PX(J),PY(J),IOFFSET=1)
   ENDDO
   CALL IGRLINEWIDTH(1)
   CALL IGRCOLOURN(WRGB(0,0,0))
   DO J=2,SPF(I)%NXY
    CALL DBL_IGRJOIN(PX(J-1),PY(J-1),PX(J),PY(J),IOFFSET=1)
   ENDDO

   IF(ILABEL.EQ.1)THEN
    J=SPF(I)%NXY/2
    DX=PX(MIN(SPF(I)%NXY,J+1))-PX(MAX(1,J))
    DY=PY(MIN(SPF(I)%NXY,J+1))-PY(MAX(1,J))
    GANGLE=0.0D0; IF(DY.NE.0.0D0)GANGLE=ATAN2(DY,DX)*RAD
    DX=0.5D0*(PX(MIN(SPF(I)%NXY,J+1))+PX(MAX(1,J)))
    DY=0.5D0*(PY(MIN(SPF(I)%NXY,J+1))+PY(MAX(1,J)))
    !## black label
    CALL IGRCOLOURN(WRGB(0,0,0))
    FNAME=SPF(I)%FNAME(:INDEX(SPF(I)%FNAME,'.',.TRUE.)-1)
    CALL UTL_PLOTLABEL(DX,DY,(/TRIM(FNAME)/),(/1/),1,TW,TH,(/''/),.FALSE.,-1,ALIGNLEFT,GANGLE=GANGLE,CFORMAT='(F10.2)')
   ENDIF
   
  ENDIF
  NULLIFY(PX,PY)
 ENDDO
 
 END SUBROUTINE SOLID_PLOTLOCATION_CROSSSECTIONS
 
 !###======================================================================
 SUBROUTINE SOLID_PROFILEMINMAX(IFIXX,IFIXY)
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

 END SUBROUTINE SOLID_PROFILEMINMAX
 
 !###======================================================================
 SUBROUTINE SOLID_PROFILEMOUSE(GX,GY,ICURSOR,ICRD,IELEV,IWINID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IWINID
 INTEGER,INTENT(OUT) :: ICURSOR,ICRD,IELEV
 INTEGER :: I,J
 REAL(KIND=DP_KIND),INTENT(IN) :: GX,GY
 REAL(KIND=DP_KIND) :: DX,DY,DXX
 
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
 DO I=SIZE(SPF(ISPF)%PROF),1,-1 
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
    DXX=DBL_IGRDISTANCELINE(PX(J),PZ(J),PX(J+1),PZ(J+1),GX,GY,0)
    IF(DXX.GE.0.0D0.AND.DXX.LE.DY)THEN
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

 END SUBROUTINE SOLID_PROFILEMOUSE

 !###======================================================================
 SUBROUTINE SOLID_PROFILESELECTNODES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: MAXPOL=50
 REAL(KIND=DP_KIND) :: XC1,YC1,XC2,YC2,X
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: XPOL,YPOL
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

    XC2=DBLE(MESSAGE%GX); YC2=DBLE(MESSAGE%GY)

    IF(NPOL.GT.0)THEN
     !## line drawn before, remove it
     IF(LEX)THEN
      XPOL(NPOL+1)=XC1; YPOL(NPOL+1)=YC1
      CALL DBL_IGRPOLYGONCOMPLEX(XPOL,YPOL,NPOL+1)
     ENDIF
     LEX=.TRUE.
     !## draw new line
     XPOL(NPOL+1)=XC2; YPOL(NPOL+1)=YC2
     CALL DBL_IGRPOLYGONCOMPLEX(XPOL,YPOL,NPOL+1)
     XC1=XC2; YC1=YC2
    ENDIF
    
   CASE (MOUSEBUTDOWN)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (1)  !## left button
      NPOL=NPOL+1; XPOL(NPOL)=XC2; YPOL(NPOL)=YC2
     CASE (3)  !## right button
      !## remove line
      IF(NPOL.GT.0)THEN
       IF(LEX)CALL DBL_IGRPOLYGONCOMPLEX(XPOL,YPOL,NPOL+1)
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
    IF(DBL_IGRINSIDEPOLYGON(PX(J),PZ(J),XPOL,YPOL,NPOL).EQ.1)THEN
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
 CALL WDIALOGPUTDOUBLE(IDF_REAL1,1.0D0)

 DO I=1,K; IMENU(SP(I,1))=1; ENDDO
 CALL WDIALOGPUTMENU(IDF_MENU1,CMENU,SIZE(SPF(ISPF)%PROF),IMENU)
 CALL WDIALOGFIELDSTATE(IDF_MENU1,1)

 J=SP(1,1)
 CALL WDIALOGPUTMENU(IDF_MENU2,CMENU,SIZE(SPF(ISPF)%PROF),J)
 CALL WDIALOGFIELDSTATE(IDF_MENU2,0)

 CALL WDIALOGTITLE('Adjust Selected '//TRIM(ITOS(K))//' nodes')
 CALL UTL_DIALOGSHOW(-1,-1,0,3)

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
      SELECT CASE (ISEL)
       CASE (5)
        CALL WDIALOGGETDOUBLE(IDF_REAL1,X)
        CALL WDIALOGGETMENU(IDF_MENU2,IMENU)
        IF(SUM(IMENU).LT.2)THEN
         CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You have to select at least TWO lines.','Error')
        ELSE
         EXIT
        ENDIF
       CASE (4)
        CALL WDIALOGGETMENU(IDF_MENU1,IMENU)
        CALL WDIALOGGETMENU(IDF_MENU2,J)
        IF(IMENU(J).EQ.1)THEN
         CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You cannot merge nodes from a line onto the identical line.','Error')
        ELSE
         IMENU(J)=-1; EXIT
        ENDIF
       CASE (1,2,3)
        CALL WDIALOGGETMENU(IDF_MENU1,IMENU)
        IF(SUM(IMENU).EQ.0)THEN
         CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You have to select at least ONE line.','Error')
        ELSE
         EXIT
        ENDIF
       CASE DEFAULT
        EXIT
      END SELECT
    END SELECT
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_RADIO1,IDF_RADIO2,IDF_RADIO3,IDF_RADIO4,IDF_RADIO5)
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,ISEL)
      SELECT CASE (ISEL)
       CASE (4); I=0; J=1
       CASE (5); I=1; J=0
       CASE DEFAULT; I=0; J=0
      END SELECT
      CALL WDIALOGFIELDSTATE(IDF_REAL1,I)
      CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
      CALL WDIALOGFIELDSTATE(IDF_MENU2,J)
    END SELECT 
  END SELECT
 ENDDO

 CALL WDIALOGUNLOAD(); CALL WDIALOGSELECT(DID)
 IF(MESSAGE%VALUE1.EQ.IDCANCEL)RETURN

 SELECT CASE (ISEL)
  CASE (3) !## delete nodes
   CALL SOLID_PROFILEDELNODE(SP(:,2),SP(:,1),IMENU=IMENU)
  CASE (5) !## split level
   CALL SOLID_PROFILESPLITLINE(SP(:,2),SP(:,1),X,IMENU)
  CASE (4) !## merge with level j
   CALL SOLID_PROFILEMERGELINE(SP(:,2),SP(:,1),IMENU)
  CASE (1) !## activate level j
   DO I=1,SIZE(IMENU); SPF(ISPF)%PROF(I)%IACTIVE=1; ENDDO
  CASE (2) !## deactivate level j
   DO I=1,SIZE(IMENU); SPF(ISPF)%PROF(I)%IACTIVE=0; ENDDO
 END SELECT

 IF(ALLOCATED(SP))   DEALLOCATE(SP)
 IF(ALLOCATED(CMENU))DEALLOCATE(CMENU)
 IF(ALLOCATED(IMENU))DEALLOCATE(IMENU)

 END SUBROUTINE SOLID_PROFILESELECTNODES
 
 !###======================================================================
 SUBROUTINE SOLID_PROFILEADJUST(XC,YC,ICURSOR,ICRD,IELEV,IWINID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IELEV,IWINID
 INTEGER,INTENT(INOUT) :: ICURSOR,ICRD
 REAL(KIND=DP_KIND),INTENT(IN) :: XC,YC
 REAL(KIND=DP_KIND) :: GX,GY,X1,Y1,X2,Y2,X3,Y3,XINTER,YINTER,DX,DY
 INTEGER :: NCRD,ISTATUS,IL,I,JELEV,JCRD
 REAL(KIND=DP_KIND),DIMENSION(2) :: D,MIND
 LOGICAL :: LINT,LSNP
 
 !## solid-modeling not activated
 IF(ISOLID.EQ.0)RETURN
 !## nothing selected
 IF(ISPF.LE.0)RETURN
 !## nothing to adjust
 IF(ICURSOR.EQ.0)RETURN
 
 DX=(GRAPHUNITS(3,IWINID)-GRAPHUNITS(1,IWINID))/200.0D0
 DY=(GRAPHUNITS(4,IWINID)-GRAPHUNITS(2,IWINID))/200.0D0

 GX=XC; GY=YC
 
 SELECT CASE (ICURSOR)
  !## move current point
  CASE (1)
   !## check for min./max. extinction of x-coordinate
   GX=MIN(SPF(ISPF)%TX,MAX(0.0D0,GX))
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
    MIND =10.0D10
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
      CALL DBL_IGRINTERSECTLINE(X1,Y1,X2,Y2,PX(I),PZ(I),PX(I+1),PZ(I+1),XINTER,YINTER,ISTATUS)
      IF(ISTATUS.EQ.5)THEN
       LINT=.TRUE.
       EXIT ILLOOP
      ENDIF
      !## compute intersection --- next segment
      CALL DBL_IGRINTERSECTLINE(X2,Y2,X3,Y3,PX(I),PZ(I),PX(I+1),PZ(I+1),XINTER,YINTER,ISTATUS)
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
      SPF(ISPF)%PROF(IELEV)%PZ(ICRD)=GY+0.01D0
     ELSE
      SPF(ISPF)%PROF(IELEV)%PZ(ICRD)=GY-0.01D0
     ENDIF
    ENDIF
   
   ENDIF
   IF(.NOT.LSNP.AND..NOT.LINT)THEN 
    SPF(ISPF)%PROF(IELEV)%PX(ICRD)=GX   
    SPF(ISPF)%PROF(IELEV)%PZ(ICRD)=GY
   ENDIF
   
  !## insert node at position icrd
  CASE (2)
   CALL SOLID_PROFILEINSERTNODE(ISPF,IELEV,ICRD,GX,GY)
   !## change to move
   ICURSOR=1
   !## current node the one to be moved
   ICRD=ICRD+1
 END SELECT   
 
 END SUBROUTINE SOLID_PROFILEADJUST
 
 !###======================================================================
 SUBROUTINE SOLID_PROFILEINSERTNODE(JSPF,IELEV,ICRD,GX,GY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IELEV,JSPF,ICRD
 REAL(KIND=DP_KIND),INTENT(IN) :: GX,GY
 INTEGER :: I,N
 
 N=SPF(JSPF)%PROF(IELEV)%NPOS
 
 IF(N.EQ.SIZE(SPF(JSPF)%PROF(IELEV)%PX))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot add another point,'//CHAR(13)// &
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
 
 END SUBROUTINE SOLID_PROFILEINSERTNODE

 !###======================================================================
 SUBROUTINE SOLID_PROFILEDELNODE(VCRD,VIELEV,IMENU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN),DIMENSION(:)  :: VCRD,VIELEV
 INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: IMENU
 INTEGER,DIMENSION(:),ALLOCATABLE :: CCRD,CIELEV
 INTEGER :: I,J,ICRD,IELEV
 LOGICAL :: LEX
 
 !## copy node numbers
 ALLOCATE(CCRD  (SIZE(VCRD)))  ; CCRD=VCRD
 ALLOCATE(CIELEV(SIZE(VIELEV))); CIELEV=VIELEV
 
 !## remove all coordinates for current ielev
 DO J=1,SIZE(VCRD)
  !## remove current node on ielev
  ICRD =CCRD(J)
  IELEV=CIELEV(J)

  LEX=.TRUE.; IF(PRESENT(IMENU))THEN; IF(IMENU(IELEV).EQ.0)LEX=.FALSE.; ENDIF
  IF(.NOT.LEX)CYCLE
  
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
 
 END SUBROUTINE SOLID_PROFILEDELNODE

 !###======================================================================
 SUBROUTINE SOLID_PROFILESPLITLINE(VCRD,VIELEV,XDIST,IMENU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN),DIMENSION(:)  :: VCRD,VIELEV,IMENU
 REAL(KIND=DP_KIND),INTENT(IN) :: XDIST
 INTEGER :: I,J,ICRD,IELEV,JCRD,JELEV
 REAL(KIND=DP_KIND) :: X1,Z1,X2,Z2

 !## shift all lines equal with x-meter down
 DO I=1,SIZE(VCRD)

  !## remove current node on ielev
  ICRD =VCRD(I); IELEV=VIELEV(I)

  !## skip this line
  IF(IMENU(IELEV).EQ.0)CYCLE
  
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
 
 END SUBROUTINE SOLID_PROFILESPLITLINE

 !###======================================================================
 SUBROUTINE SOLID_PROFILEMERGELINE(VCRD,VIELEV,IMENU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN),DIMENSION(:) :: VCRD,VIELEV,IMENU
 INTEGER :: I,J,I1,I2,ICRD,IELEV,JELEV
 REAL(KIND=DP_KIND) :: X1,Z1

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
  IF(I1.NE.0.AND.I2.NE.0)CALL SOLID_PROFILEDELNODE(VCRD(I1:I2),VIELEV(I1:I2))
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
   CALL SOLID_PROFILEPUTINTERSECTION(X1,Z1,JELEV)
  ENDDO
  
 ENDDO
 
 END SUBROUTINE SOLID_PROFILEMERGELINE

 !###======================================================================
 SUBROUTINE SOLID_PROFILELINECOLOR(IELEV)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IELEV
 INTEGER :: IRGB

 IRGB=SPF(ISPF)%PROF(IELEV)%ICLR
 CALL WSELECTCOLOUR(IRGB)
 IF(WINFODIALOG(4).NE.1)RETURN
 SPF(ISPF)%PROF(IELEV)%ICLR=IRGB

 END SUBROUTINE SOLID_PROFILELINECOLOR

 !###======================================================================
 SUBROUTINE SOLID_PROFILELINETHICKNESS(ID,IELEV)
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

 END SUBROUTINE SOLID_PROFILELINETHICKNESS

 !###======================================================================
 LOGICAL FUNCTION SOLID_PROFILEINIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 SOLID_PROFILEINIT=.FALSE.
 
 CALL WDIALOGLOAD(ID_DSERIES,ID_DSERIES)
 CALL WDIALOGSELECT(ID_DSERIESTAB2) 
 CALL WDIALOGPUTIMAGE(ID_NEW,ID_ICONNEW,1)
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE,1)
 CALL WDIALOGPUTIMAGE(ID_FIT,ID_ICONFIT,1)
 CALL WDIALOGPUTIMAGE(ID_FITDRILL,ID_ICONFITDRILL,1)
 CALL WDIALOGPUTIMAGE(IDF_CHECK2,ID_ICONGLASSES,1)
 IF(NMASK.EQ.0.AND.SHP%NPOL.EQ.0)THEN
  IMASK=0; CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,IMASK)
  CALL WDIALOGFIELDSTATE(IDF_CHECK2,3)
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
 ELSE
  CALL WDIALOGSELECT(ID_DSERIES)
  CALL WDIALOGTABSTATE(ID_DSERIESTAB,ID_DSERIESTAB2,0)
 ENDIF
 CALL SOLID_PROFILEFIELDS()
 
 !## open masks
 DO I=1,NMASK
  IF(.NOT.IDFREAD(MASK(I)%IDF,MASK(I)%FNAME,0))RETURN
 ENDDO

 SOLID_PROFILEINIT=.TRUE.
 
 END FUNCTION SOLID_PROFILEINIT

 !###======================================================================
 SUBROUTINE SOLID_PROFILECLOSE()
 !###======================================================================
 IMPLICIT NONE
 
 !## read entire sol, incl. spf-files
 IF(.NOT.SOLIDOPENSOL('W',GETSOLNAME(),IQ=1,TXT='Cross-Sections'))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD could not save your cross-sections','Error')
  RETURN
 ENDIF
  
 END SUBROUTINE SOLID_PROFILECLOSE

 !###======================================================================
 SUBROUTINE SOLID_PROFILEFIELDS()
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

 END SUBROUTINE SOLID_PROFILEFIELDS

END MODULE MOD_SOLID_PROFILE
