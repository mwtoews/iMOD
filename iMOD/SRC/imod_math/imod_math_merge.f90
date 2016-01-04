!!  Copyright (C) Stichting Deltares, 2005-2016.
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
MODULE MOD_MATH_MERGE

 USE MOD_MATH_MERGE_PAR
 USE WINTERACTER
 USE MOD_IDF_PAR, ONLY : IDFOBJ
 USE MOD_IDF, ONLY : IDFGETLOC,IDFOPEN,IDFREAD,IDFWRITE,IDFGETVAL,IDFWRITEDIM,IDFNULLIFY,IDFPUTVAL,IDFIROWICOL, &
                     IDFDEALLOCATE 
 USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_IDFSNAPTOGRID,UTL_WAITMESSAGE,UTL_CREATEDIR 
 USE MODPLOT, ONLY : MPW
 TYPE(IDFOBJ),DIMENSION(:),ALLOCATABLE :: MATH

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION MATH1MERGE(IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: IROW,ICOL,IR,IC,I,JMASK,IRAT,IRAT1,NIDF
 REAL :: IDFVAL,TW,TD,MD,X,Y

 MATH1MERGE=.FALSE.

 NIDF=SIZE(IDFNAMES)

 ALLOCATE(MATH(ABS(IMASK-1):NIDF+1))
 DO I=1,SIZE(MATH); CALL IDFNULLIFY(MATH(I)); ENDDO
 
 !## use/read Mask-IDF
 IF(IMASK.EQ.1)THEN
  IF(.NOT.IDFREAD(MATH(0),MSKNAME,0))THEN
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not read Mask IDF '//CHAR(13)// &
     TRIM(MSKNAME),'Error')
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Can not read Mask IDF '//TRIM(MSKNAME)
   RETURN
  ENDIF
 ENDIF

 DO I=1,NIDF !## nidf is output filename
  IF(.NOT.IDFREAD(MATH(I),IDFNAMES(I),0,1))THEN
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not read IDF '//CHAR(13)// &
     TRIM(IDFNAMES(I)),'Error')
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Can not read IDF '//TRIM(IDFNAMES(I))
   MATH(I)%IU=0
  ENDIF
 ENDDO

 DO I=1,NIDF
  IF(MATH(I)%IU.GT.0)THEN
   MATH(NIDF+1)%DX  =MATH(I)%DX
   MATH(NIDF+1)%DY  =MATH(I)%DY
   MATH(NIDF+1)%IEQ =MATH(I)%IEQ
   MATH(NIDF+1)%ITB =MATH(I)%ITB
   IF(MATH(NIDF+1)%ITB.EQ.1)THEN
    MATH(NIDF+1)%TOP =MATH(I)%TOP
    MATH(NIDF+1)%BOT =MATH(I)%BOT
   ENDIF
   MATH(NIDF+1)%IXV =0
   MATH(NIDF+1)%NODATA=MATH(I)%NODATA
   EXIT
  ENDIF
 ENDDO
 
 !## it is not allowed to merge non-equidistant IDF files
 IF(MATH(NIDF+1)%IEQ.EQ.1)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You can not merge non-equidistantial IDF files','Warning')
  RETURN
 ENDIF
 
 !## zoom for all idfs
 IF(IEXT.EQ.2)THEN

  MATH(NIDF+1)%XMIN= 10.E10; MATH(NIDF+1)%YMIN= 10.E10
  MATH(NIDF+1)%XMAX=-10.E10; MATH(NIDF+1)%YMAX=-10.E10
  DO I=1,NIDF
   IF(MATH(I)%IU.GT.0)THEN
    MATH(NIDF+1)%XMIN=MIN( MATH(NIDF+1)%XMIN,MATH(I)%XMIN)
    MATH(NIDF+1)%XMAX=MAX( MATH(NIDF+1)%XMAX,MATH(I)%XMAX)
    MATH(NIDF+1)%YMIN=MIN( MATH(NIDF+1)%YMIN,MATH(I)%YMIN)
    MATH(NIDF+1)%YMAX=MAX( MATH(NIDF+1)%YMAX,MATH(I)%YMAX)
    MATH(NIDF+1)%NCOL=INT((MATH(NIDF+1)%XMAX-MATH(NIDF+1)%XMIN)/MATH(NIDF+1)%DX)
    MATH(NIDF+1)%NROW=INT((MATH(NIDF+1)%YMAX-MATH(NIDF+1)%YMIN)/MATH(NIDF+1)%DY)
   ENDIF
  ENDDO
  
 !## current zoom window
 ELSEIF(IEXT.EQ.1)THEN

  IF(MATH(NIDF+1)%IEQ.EQ.1)THEN
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not combine IEXT=2 and non-equidistantial IDF"s!','Error')
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Can not combine IEXT=2 and non-equidistantial IDF"s!'
   RETURN
  ENDIF
  MATH(NIDF+1)%XMIN=MPW%XMIN; MATH(NIDF+1)%XMAX=MPW%XMAX
  MATH(NIDF+1)%YMIN=MPW%YMIN; MATH(NIDF+1)%YMAX=MPW%YMAX
  CALL UTL_IDFSNAPTOGRID(MATH(NIDF+1)%XMIN,MATH(NIDF+1)%XMAX,MATH(NIDF+1)%YMIN,MATH(NIDF+1)%YMAX, &
                         MATH(NIDF+1)%DX,MATH(NIDF+1)%NCOL,MATH(NIDF+1)%NROW)
 ENDIF

 MATH(NIDF+1)%IU=UTL_GETUNIT()
 CALL UTL_CREATEDIR(OUTNAME(:INDEX(OUTNAME,'\',.TRUE.)-1))
 IF(.NOT.IDFOPEN(MATH(NIDF+1)%IU,OUTNAME,'WO',0,0))RETURN
 
 IF(IBATCH.EQ.1)WRITE(*,*)
 
 MATH(NIDF+1)%DMIN= 10.0E10
 MATH(NIDF+1)%DMAX=-10.0E10
 
 !## get merged values for constructed idf math(nidf)%idf
 IRAT1=0
 DO IROW=1,MATH(NIDF+1)%NROW
  DO ICOL=1,MATH(NIDF+1)%NCOL

   CALL IDFGETLOC(MATH(NIDF+1),IROW,ICOL,X,Y)

   JMASK=1
   IF(IMASK.EQ.1)THEN
    CALL IDFIROWICOL(MATH(0),IR,IC,X,Y)
    IF(IC.GE.1.AND.IC.LE.MATH(0)%NCOL.AND.IR.GE.1.AND.IR.LE.MATH(0)%NROW)THEN
     JMASK=0
     IF(IDFGETVAL(MATH(0),IR,IC).NE.MATH(0)%NODATA)JMASK=1
    ELSE
     JMASK=0
    ENDIF
   ENDIF
   
   TD=0.0
   TW=0.0

   IF(JMASK.EQ.1)THEN
   
    DO I=1,NIDF
     IF(MATH(I)%IU.GT.0)THEN
      IF(X.GE.MATH(I)%XMIN.AND.X.LE.MATH(I)%XMAX.AND. &
         Y.GE.MATH(I)%YMIN.AND.Y.LE.MATH(I)%YMAX)THEN
       !## get minimal distance to ANY cross-boundary
       MD=MATH(NIDF+1)%XMAX-MATH(NIDF+1)%XMIN
       MD=MIN(MD,X-MATH(I)%XMIN)
       MD=MIN(MD,MATH(I)%XMAX-X)
       MD=MIN(MD,Y-MATH(I)%YMIN)
       MD=MIN(MD,MATH(I)%YMAX-Y)
       CALL IDFIROWICOL(MATH(I),IR,IC,X,Y)
       IDFVAL=IDFGETVAL(MATH(I),IR,IC)
       IF(IDFVAL.NE.MATH(I)%NODATA)THEN
        TW=TW+MD
        TD=TD+IDFVAL*MD
       ENDIF
      ENDIF
     ENDIF
    ENDDO
   
   ENDIF
   
   IF(TW.NE.0.0)THEN
    IDFVAL=TD/TW
    MATH(NIDF+1)%DMIN=MIN(MATH(NIDF+1)%DMIN,IDFVAL)
    MATH(NIDF+1)%DMAX=MAX(MATH(NIDF+1)%DMAX,IDFVAL)
   ELSE
    IDFVAL=MATH(NIDF+1)%NODATA
   ENDIF
   CALL IDFPUTVAL(MATH(NIDF+1),IROW,ICOL,IDFVAL)
  
  ENDDO
  IF(IBATCH.EQ.0)CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW,MATH(NIDF+1)%NROW,'Progress ')
  IF(IBATCH.EQ.1)WRITE(6,'(A,F10.2)') '+Progress ',REAL(IROW)/REAL(MATH(NIDF+1)%NROW)*100.0
 ENDDO

 IF(.NOT.IDFWRITEDIM(0,MATH(NIDF+1)))RETURN
 CLOSE(MATH(NIDF+1)%IU)

 MATH1MERGE=.TRUE.

 END FUNCTION MATH1MERGE

 !###======================================================================
 SUBROUTINE MATH1MERGECLOSE(IBATCH) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH 

 IF(IBATCH.EQ.0)THEN; CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,''); ENDIF

 !## module used outside imod
 IF(ALLOCATED(IDFNAMES))DEALLOCATE(IDFNAMES)
 IF(ALLOCATED(MATH))THEN
  CALL IDFDEALLOCATE(MATH,SIZE(MATH))
  DEALLOCATE(MATH)
 ENDIF

 END SUBROUTINE MATH1MERGECLOSE
 
 END MODULE MOD_MATH_MERGE
