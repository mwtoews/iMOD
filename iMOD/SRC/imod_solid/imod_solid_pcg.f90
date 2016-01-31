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
MODULE MOD_SOLID_PCG

USE WINTERACTER
USE RESOURCE
USE MOD_PCG, ONLY : PCG2AP,PCGSETTINGS,PCG_HFB_FM
USE MOD_SOLID_PAR
USE MOD_UTL, ONLY : ITOS,UTL_INSIDEPOLYGON,NEWLINE,UTL_CREATEDIR,UTL_IDFSNAPTOGRID,UTL_SUBST,UTL_IMODVERSION
USE MOD_IDF, ONLY : IDFREAD,IDFEQUAL,IDFREADSCALE,IDFWRITE,IDFALLOCATEX,IDFCOPY,IDFIROWICOL,IDFGETLOC,IDFFILLCOMMENT
USE MOD_POLYGON_PAR
USE MOD_INTERSECT_PAR, ONLY : XA,YA,LN
USE MOD_SOLID_UTL
USE MOD_KRIGING, ONLY : KRIGINGSETTINGS,KRIGING_VARIOGRAM,KRIGING_MAIN
USE MOD_INTERSECT, ONLY : INTERSECT_EQUI,INTERSECT_NONEQUI,INTERSECT_DEALLOCATE 
USE MOD_POLINT, ONLY : SPLINE_MAIN,SPLINE_AKIMA_MAIN
USE MOD_ASC2IDF_HFB, ONLY : ASC2IDF_HFB
USE MOD_ASC2IDF_PAR, ONLY : ASC2IDF_INT_NULLIFY,ASC2IDF_INT_DEALLOCATE,XP,YP,ZP,WP

INTEGER,PRIVATE :: NROW,NCOL,NLAY

TYPE PCGOBJ
 REAL,POINTER,DIMENSION(:,:) :: RHS,CC,CR,CV,P,V,SS,CD,HCOF,HOLD,COND
 DOUBLE PRECISION,POINTER,DIMENSION(:,:) :: HNEW
 INTEGER,POINTER,DIMENSION(:,:) :: IB
END TYPE PCGOBJ
TYPE(PCGOBJ),ALLOCATABLE,DIMENSION(:) :: PCG

LOGICAL :: LSPLINE=.TRUE.
REAL,DIMENSION(:),POINTER,PRIVATE :: XD,YD,ZD,XDDUMMY,YDDUMMY,ZDDUMMY

CONTAINS

 !###======================================================================
 SUBROUTINE SOLID_CALC()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,ILAY,IROW,ICOL,IVERSION,IKRIGING,IEXPORTMODE
 LOGICAL :: LEX
 CHARACTER(LEN=256) :: FNAME,GENFNAME
 CHARACTER(LEN=10) :: TXT
 TYPE(IDFOBJ),DIMENSION(2) :: IDFK

 DO I=1,SIZE(IDFK); CALL IDFNULLIFY(IDFK(I)); ENDDO
 
 !## read all idf-files in to memory
 CALL SOLID_DEALLOCATE()
 FNAME=GETSOLNAME(); IF(.NOT.SOLIDOPENSOL('R',FNAME))RETURN
 
 !## get options for soluting the model
 LEX=SOLID_CALC_INIT(IVERSION,IKRIGING,IEXPORTMODE)
 IF(.NOT.SOLIDOPENSOL('W',GETSOLNAME(),IQ=0))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD could not save your calculation setting in the sol-file','Warning')
 ENDIF
 IF(.NOT.LEX)RETURN  

 !## result folder
 IF(IVERSION.EQ.0)THEN
  OUTPUTFOLDER=FNAME(:INDEX(FNAME,'\',.TRUE.)-1)
 ELSE
  OUTPUTFOLDER=FNAME(:INDEX(FNAME,'\',.TRUE.)-1)//'\version'//TRIM(ITOS(IVERSION))
 ENDIF
 
 !## read top/bottom idf files
 IF(.NOT.SOLID_READIDF())RETURN
 !## allocate memory
 IF(.NOT.SOLID_CALC_AL())RETURN
 
 ALLOCATE(ISEL_IDF(NLAY),ICHECK_IDF(NLAY))
 
 DO ILAY=1,NLAY
  ISEL_IDF(ILAY)  =SLD(1)%ICLC(ILAY)
  ICHECK_IDF(ILAY)=SLD(1)%ICHECK(ILAY)
 ENDDO
 
 !## process all idf's (starting heads) + crosssections (fixed heads/ghb-heads)
 IF(.NOT.SOLID_CALC_FILL_BND())RETURN
 
 CALL UTL_MESSAGEHANDLE(0)

 !## solve each system per modellayer
 DO ILAY=1,NLAY
  IF(ISEL_IDF(ILAY).EQ.0)CYCLE
  
  !## solve system
  CALL WINDOWOUTSTATUSBAR(3,'Comp. Elev.: '//TRIM(ITOS(ILAY)))

  CALL IDFCOPY(SOLIDF(ILAY),IDFK(1)); CALL IDFCOPY(SOLIDF(ILAY),IDFK(2))
  IF(IDFALLOCATEX(IDFK(1)).AND.IDFALLOCATEX(IDFK(2)))THEN
   IDFK(1)%X=IDFK(1)%NODATA; IDFK(2)%X=IDFK(2)%NODATA

   GENFNAME=''; DO I=1,SIZE(GEN)
    !## include proper layers only
    IF(GEN(I)%ILAY.EQ.ILAY)GENFNAME=GEN(I)%FNAME
   ENDDO

   !## get points from cross-sections
   CALL SOLID_GET_POINTS(ILAY)
   !## include fixed-boundaries
   CALL SOLID_GET_BND(ILAY)

   !## kriging
   IF(IKRIGING.EQ.1)THEN

    !## fill in known locations - to be skipped in kriging
    DO IROW=1,SOLIDF(ILAY)%NROW; DO ICOL=1,SOLIDF(ILAY)%NCOL
     IF(PCG(ILAY)%IB(ICOL,IROW).LT.0)IDFK(1)%X(ICOL,IROW)=PCG(ILAY)%HOLD(ICOL,IROW)
    ENDDO; ENDDO

    !## estimate kriging variable
    CALL KRIGING_MAIN(SIZE(XD),XD,YD,ZD,IDFK(1),IDFK(2),          &
                      KSETTINGS(ILAY)%MINP,KSETTINGS(ILAY)%RANGE, &
                      KSETTINGS(ILAY)%SILL,KSETTINGS(ILAY)%NUGGET,&
                      KSETTINGS(ILAY)%KTYPE,KSETTINGS(ILAY)%PNTSEARCH, &
                      0,0.0,KSETTINGS(ILAY)%IADJRANGE,KSETTINGS(ILAY)%IQUADRANT,IBATCH,GENFNAME)   

    !## copy solution into hold for active areas only
    DO IROW=1,SOLIDF(ILAY)%NROW; DO ICOL=1,SOLIDF(ILAY)%NCOL
     IF(PCG(ILAY)%IB(ICOL,IROW).GT.0)THEN
      PCG(ILAY)%HOLD(ICOL,IROW)=IDFK(1)%X(ICOL,IROW) 
     ELSE
      IDFK(2)%X(ICOL,IROW)=IDFK(2)%NODATA
     ENDIF
    ENDDO; ENDDO

    !## write variance of kriging interpolation
    SOLIDF(ILAY)%X=IDFK(2)%X; TXT='Standard Deviation'
    CALL IDFFILLCOMMENT(SOLIDF(ILAY),'Created by SolidTool'//NEWLINE//'Units: m'//NEWLINE//TRIM(TXT)//' '//TRIM(ITOS(ILAY)))
    SOLIDF(ILAY)%NODATA=IDFK(2)%NODATA
    FNAME=TRIM(OUTPUTFOLDER)//'\'//SOLIDF(ILAY)%FNAME(INDEX(SOLIDF(ILAY)%FNAME,'\',.TRUE.)+1:)
    FNAME=FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'_STDEV.IDF'
    CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(FNAME))
    IF(.NOT.IDFWRITE(SOLIDF(ILAY),FNAME,1))THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot write IDF file:'//CHAR(13)//TRIM(FNAME),'Error')
    ENDIF

   !## semivariogram
   ELSEIF(IKRIGING.EQ.2)THEN
    CALL KRIGING_VARIOGRAM(SIZE(XD),XD,YD,ZD,I,IDFK(1),IBATCH=0)
   !## export
   ELSEIF(IKRIGING.EQ.3)THEN
    CALL SOLID_EXPORT(ILAY,IEXPORTMODE)
   ENDIF

   !## clean memory
   DO I=1,SIZE(IDFK); CALL IDFDEALLOCATEX(IDFK(I)); ENDDO
  
  ELSE
   CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'iMOD can not allocate memory for storage of IDF-files.','Error')
   EXIT
  ENDIF

  IF(ASSOCIATED(XD))DEALLOCATE(XD); IF(ASSOCIATED(YD))DEALLOCATE(YD); IF(ASSOCIATED(ZD))DEALLOCATE(ZD)
    
 ENDDO

 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,''); CALL WINDOWOUTSTATUSBAR(3,'')

 !## write results
 IF(IKRIGING.EQ.1)THEN 

  !## make model consistent
  CALL SOLID_CONSISTENT()
  
  DO ILAY=1,NLAY
   SOLIDF(ILAY)%X=PCG(ILAY)%HOLD
   !## get solved interpolation field
   CLOSE(SOLIDF(ILAY)%IU)
   TXT='Interface'
   CALL IDFFILLCOMMENT(SOLIDF(ILAY),'Created by SolidTool'//NEWLINE//'Units: m+MSL'//NEWLINE// &
                              TRIM(TXT)//' '//TRIM(ITOS(ILAY)))
   SOLIDF(ILAY)%NODATA=HNOFLOW
   FNAME=TRIM(OUTPUTFOLDER)//'\'//TRIM(SOLIDF(ILAY)%FNAME(INDEX(SOLIDF(ILAY)%FNAME,'\',.TRUE.)+1:))
   IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(FNAME))
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Writing '//TRIM(FNAME)
   IF(.NOT.IDFWRITE(SOLIDF(ILAY),FNAME,1))THEN
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not write IDF file:'//CHAR(13)//TRIM(FNAME),'Error')
    IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Can not write IDF file:'//CHAR(13)//TRIM(FNAME)
    EXIT
   ENDIF
  END DO

  CALL WINDOWOUTSTATUSBAR(4,'')
 ENDIF

 !## free memory for pcg-computation
 CALL SOLID_CALC_DAL()
 
 CALL IDFDEALLOCATE(SOLIDF,SIZE(SOLIDF)); DEALLOCATE(SOLIDF,ISEL_IDF,ICHECK_IDF)
 CALL UTL_CLOSEUNITS()

 CALL UTL_MESSAGEHANDLE(1)
 IF(IKRIGING.EQ.1)THEN
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Successfully written new solid.','Information')
 ELSEIF(IKRIGING.EQ.3)THEN
  IF(IEXPORTMODE.EQ.1)CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Successfully exported solid as *.IPF files.','Information')
  IF(IEXPORTMODE.EQ.2)CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Successfully exported solid as *.GEO files.','Information')
 ENDIF
 
 END SUBROUTINE SOLID_CALC

 !###======================================================================
 SUBROUTINE SOLID_CALC_HYPO()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,ITER1,ITER2,ILAY,IROW,ICOL,ITYPE,IECHO,NICNVG
 INTEGER,ALLOCATABLE,DIMENSION(:) :: ICNVG
 REAL,ALLOCATABLE,DIMENSION(:) :: ARRAY_HCHG
 REAL :: C,HCHG,RCHG,HCHGOLD,S
 LOGICAL :: LEX
 CHARACTER(LEN=256) :: FNAME,LINE
 CHARACTER(LEN=10) :: TXT
 CHARACTER(LEN=1) :: YN
 TYPE(WIN_MESSAGE) :: MESSAGE
 TYPE(IDFOBJ),DIMENSION(2) :: IDFK
 INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:,:) :: IPC

 !## solid tool from iMOD
 IF(IBATCH.EQ.0)THEN; IECHO= 0; ENDIF
 !## solid tool from iMOD Batch
 IF(IBATCH.EQ.1)THEN; IECHO=-2; IDAMPING=0; RELAX=0.98; ENDIF

 DO I=1,SIZE(IDFK); CALL IDFNULLIFY(IDFK(I)); ENDDO
 
 NMASK=SLD(1)%NINT
 ALLOCATE(MASK(NMASK)); DO I=1,SIZE(MASK); CALL IDFNULLIFY(MASK(I)%IDF); ENDDO
 DO I=1,NMASK; MASK(I)%FNAME=TRIM(OUTPUTFOLDER)//'\MASK\MASK_L'//TRIM(ITOS(I))//'.IDF'; ENDDO
 
 !## read top/bottom idf files
 IF(.NOT.SOLID_READIDF())RETURN
 !## allocate memory
 IF(.NOT.SOLID_CALC_AL())RETURN
 
 ALLOCATE(ICNVG(NLAY),ARRAY_HCHG(NLAY),ISEL_IDF(NLAY),ICHECK_IDF(NLAY))
 IF(.NOT.ALLOCATED(DZ))THEN
  ALLOCATE(DZ(SLD(1)%NINT)); DZ=0.0
 ENDIF
 ARRAY_HCHG=0.0
 
 DO ILAY=1,NLAY
  ISEL_IDF(ILAY)  =SLD(1)%ICLC(ILAY)
  ICHECK_IDF(ILAY)=SLD(1)%ICHECK(ILAY)
 ENDDO
 !## check always from imodbatch, turn smoothing off
 IF(IBATCH.EQ.1)ICHECK_IDF=1
 
 !## process all idf's (starting heads) + crosssections (fixed heads/ghb-heads)
 IF(.NOT.SOLID_CALC_FILL())RETURN
 
 !## pcg-solving
 !## ibound=-1 actual borehole, ibound=-2 cross-section, ibound=3 ghb
 IF(ITIGHT.EQ.2)THEN
  DO ILAY=1,NLAY; DO IROW=1,SOLIDF(ILAY)%NROW; DO ICOL=1,SOLIDF(ILAY)%NCOL
   IF(PCG(ILAY)%IB(ICOL,IROW).EQ.-2)PCG(ILAY)%IB(ICOL,IROW)=3
  ENDDO; ENDDO; ENDDO 
 ENDIF
 
 !## do not compute hypothetical borders (lowest one)
 !## no cross-sections given
 IF(NSPF.EQ.0)THEN
  IF(SUM(ISEL_IDF).EQ.0)THEN
   ISEL_IDF=1; ISEL_IDF(1)=0; ISEL_IDF(NLAY)=0
   DO ILAY=1,NLAY-1,2; ISEL_IDF(ILAY)=0 ; ENDDO
  ENDIF
 ENDIF
 
 !## overall resistance
 C=100.0 !## day
 
 !## solve each system per modellayer
 DO ILAY=1,NLAY
  IF(ISEL_IDF(ILAY).EQ.1)THEN
  
   !## solve system
   WRITE(*,'(A,I10,A,F17.5)') 'Computing Elevation: ',ILAY, ' hclose=',HCLOSE

   !## fill top as constant head whenever ibound=2
   DO IROW=1,NROW; DO ICOL=1,NCOL
    IF(PCG(ILAY)%IB(ICOL,IROW).EQ.2)THEN
     PCG(ILAY)%IB(ICOL,IROW)=-2; PCG(ILAY)%HOLD(ICOL,IROW)=PCG(ILAY-1)%HOLD(ICOL,IROW)
    ENDIF
   ENDDO; ENDDO

   IF(MAXVAL(PCG(ILAY)%IB).GT.0)THEN

    !# pcg solving, add additional memory allocation
    IF(.NOT.SOLID_CALC_AL_PCG(SOLIDF(ILAY)%NROW,SOLIDF(ILAY)%NCOL))THEN; ENDIF
 
    !## reset isotropic
    PCG(1)%CR=(SOLIDF(ILAY)%DX*SOLIDF(ILAY)%DY)/C; PCG(1)%CC=(SOLIDF(ILAY)%DX*SOLIDF(ILAY)%DY)/C

    !## check for isolated active cell that can not be solved
    IF(IBNDCHK.EQ.1)CALL SOLID_CHECKIBND(ILAY,SOLIDF(ILAY)%NROW,SOLIDF(ILAY)%NCOL)

    !## include boundaries from genfiles
    IF(ALLOCATED(GEN))THEN
     !## compute block-faces
     ALLOCATE(IPC(SOLIDF(ILAY)%NCOL,SOLIDF(ILAY)%NROW,2)); IPC=INT(0,1)
     CALL ASC2IDF_INT_NULLIFY()
     ALLOCATE(XP(100),YP(100),ZP(100),WP(100))
     J=0; DO I=1,SIZE(GEN)
      !## include proper layers only
      IF(GEN(I)%ILAY.NE.ILAY)CYCLE
      J=J+1; 
      IF(LEN_TRIM(GEN(I)%FNAME).GT.0)CALL ASC2IDF_HFB(SOLIDF(ILAY),SOLIDF(ILAY)%NROW,SOLIDF(ILAY)%NCOL,IPC,GEN(I)%FNAME)
     ENDDO
     IF(J.GT.0)THEN
      LINE='Faults ('//TRIM(ITOS(J))//') are included'
      WRITE(*,'(A)') TRIM(LINE)
      CALL PCG_HFB_FM(IPC,PCG(1)%CC,PCG(1)%CR,SOLIDF(ILAY)%NROW,SOLIDF(ILAY)%NCOL,0.01)
     ENDIF
     DEALLOCATE(IPC); CALL ASC2IDF_INT_DEALLOCATE()
    ENDIF
    
    NICNVG=0; PCG(1)%HNEW=PCG(ILAY)%HOLD

    DO ITER1=1,MXITER1
     IF(IBATCH.EQ.0)CALL WMESSAGEPEEK(ITYPE,MESSAGE)
     CALL SOLID_CALC_CONSTRAINS(ILAY) 

     CALL PCG2AP(SOLIDF(ILAY)%NROW*SOLIDF(ILAY)%NCOL,SOLIDF(ILAY)%NROW,SOLIDF(ILAY)%NCOL,1,PCG(ILAY)%IB,PCG(1)%CR,PCG(1)%CC, &
                 PCG(1)%CV,PCG(1)%HCOF,PCG(1)%RHS,PCG(1)%V,PCG(1)%SS,PCG(1)%P,PCG(1)%CD,PCG(1)%HNEW,MXITER1,MXITER2,ITER1,   &
                 ITER2,ICNVG(ILAY),HCLOSE,RCLOSE,IECHO,NICNVG,RELAX,HCHG,RCHG)
     
     IF(IECHO.EQ.0)THEN
      WRITE(LINE,'(2E15.7,2(A,I4),A,I3,A,F5.3,A)') HCHG,RCHG,' m/m3 (inner: ',ITER1,'; outer: ',ITER2,'; nconv: ',NICNVG,'; relax: ',RELAX,')'
      CALL WINDOWOUTSTATUSBAR(4,'Solving Residual Change: '//TRIM(LINE))
     ENDIF
     
     !## store closure error
     ARRAY_HCHG(ILAY)=HCHG
      
     !## convergence achieved
     IF(ICNVG(ILAY).EQ.1)EXIT
     IF(MICNVG.GT.0.AND.NICNVG.GT.MICNVG)EXIT 

     IF(IDAMPING.EQ.1)THEN
      IF(ITER1.EQ.1)HCHGOLD=HCHG
      S=HCHG/(RELAX*HCHGOLD)
      IF(S.LT.-1.0)THEN
       RELAX=1.0/(2.0*ABS(S))
      ELSE
       RELAX=(3.0+S)/(3.0+ABS(S))
      ENDIF
      HCHGOLD=HCHG
     ENDIF

    ENDDO
    
    !## something went wrong, no convergence
    IF(ITER1.EQ.1.AND.ITER2.EQ.1)ICNVG(ILAY)=0
     
    !## copy solution into hold
    PCG(ILAY)%HOLD=PCG(1)%HNEW
    !## deallocate pcg memory     
    CALL SOLID_CALC_DAL_PCG()

   ENDIF  
   
  ELSE
   ICNVG(ILAY)      = 1
   ISEL_IDF(ILAY)   = 1
!   PCG(ILAY)%IB(:,:)=-3 !## whole modellayer inactive (constant head)
  ENDIF
 ENDDO

 LEX=.FALSE.
 IF(SUM(ICNVG).EQ.SUM(ISEL_IDF))THEN
  LEX=.TRUE.
 ELSE
  IF(IBATCH.EQ.0)THEN
   CALL UTL_MESSAGEHANDLE(1) 
   LINE='Closure Errors:'
   DO I=1,NLAY
    IF(ICNVG(I).EQ.0)LINE=TRIM(LINE)//CHAR(13)//'Interface '//TRIM(ITOS(I))//' error '//TRIM(RTOS(ARRAY_HCHG(I),'G',7))
   ENDDO
   CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Part of Solid did not converge.'//CHAR(13)//TRIM(LINE)//CHAR(13)// &
     'Do you want to save the results so far in:'//CHAR(13)// &
      '['//TRIM(FNAME)//']','Question')
   IF(WINFODIALOG(4).EQ.1)LEX=.TRUE.
  ELSEIF(IBATCH.EQ.1)THEN
   WRITE(*,'(A)') 'Part of Solid did not converge.'
   DO I=1,NLAY
    IF(ICNVG(I).EQ.0)WRITE(*,'(A,I3,A,G15.7)') 'Interface ',I,' NOT solved closure error ',ARRAY_HCHG(I)
    IF(ICNVG(I).EQ.1)WRITE(*,'(A,I3,A,G15.7)') 'Interface ',I,' solved closure error ',ARRAY_HCHG(I)
   ENDDO
   WRITE(*,'(A)') 'Do you want to save the results so far in:'
   WRITE(*,'(A$)') '['//TRIM(OUTPUTFOLDER)//'] (Y/N) ? '
   READ(*,*) YN; LEX=UTL_CAP(YN,'U').EQ.'Y'
  ENDIF
 ENDIF
 DEALLOCATE(ICNVG)
 
 IF(LEX)THEN 

  CALL SOLID_CONSISTENT()
  
  DO ILAY=1,NLAY
   SOLIDF(ILAY)%X=PCG(ILAY)%HOLD
   !## get solved interpolation field
   CLOSE(SOLIDF(ILAY)%IU)
   TXT='Interface'
   CALL IDFFILLCOMMENT(SOLIDF(ILAY),'Created by SolidTool'//NEWLINE//'Units: m+MSL'//NEWLINE// &
                              TRIM(TXT)//' '//TRIM(ITOS(ILAY)))
   SOLIDF(ILAY)%NODATA=HNOFLOW
   FNAME=TRIM(OUTPUTFOLDER)//'\'//TRIM(SOLIDF(ILAY)%FNAME(INDEX(SOLIDF(ILAY)%FNAME,'\',.TRUE.)+1:))
   WRITE(*,'(A)') 'Writing '//TRIM(FNAME)
   IF(.NOT.IDFWRITE(SOLIDF(ILAY),FNAME,1))THEN
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not write IDF file:'//CHAR(13)//TRIM(FNAME),'Error')
    IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Can not write IDF file:'//CHAR(13)//TRIM(FNAME)
    EXIT
   ENDIF
  END DO

 ENDIF

 !## free memory for pcg-computation
 CALL SOLID_CALC_DAL()
 
 CALL IDFDEALLOCATE(SOLIDF,SIZE(SOLIDF)); DEALLOCATE(SOLIDF,ISEL_IDF,ICHECK_IDF)
 CALL UTL_CLOSEUNITS()

 END SUBROUTINE SOLID_CALC_HYPO

 !###======================================================================
 SUBROUTINE SOLID_CONSISTENT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ILAY,IROW,ICOL,JROW,JCOL
 REAL :: X,Y,TOP
 
 DO ILAY=2,NLAY
  DO IROW=1,SOLIDF(ILAY)%NROW; DO ICOL=1,SOLIDF(ILAY)%NCOL
   !## get current x/y location
   CALL IDFGETLOC(SOLIDF(ILAY),IROW,ICOL,X,Y)
   CALL IDFIROWICOL(SOLIDF(ILAY-1),JROW,JCOL,X,Y)
   IF(JROW.NE.0.AND.JCOL.NE.0)THEN
    IF(PCG(ILAY)%IB(ICOL,IROW).EQ.2.AND.PCG(ILAY-1)%IB(JCOL,JROW).NE.0)PCG(ILAY)%HOLD(ICOL,IROW)=PCG(ILAY-1)%HOLD(JCOL,JROW)
   ENDIF
  ENDDO; ENDDO
 ENDDO  
             
 !## make sure lowest bot is lower than upper top
 IF(ICHECK_IDF(NLAY).EQ.1)THEN
  DO IROW=1,SOLIDF(NLAY)%NROW
   DO ICOL=1,SOLIDF(NLAY)%NCOL
    CALL IDFGETLOC(SOLIDF(NLAY),IROW,ICOL,X,Y)
    CALL IDFIROWICOL(SOLIDF(1),JROW,JCOL,X,Y)
    IF(JROW.NE.0.AND.JCOL.NE.0)THEN
     PCG(NLAY)%HOLD(ICOL,IROW)=MIN(PCG(1)%HOLD(JCOL,JROW),PCG(NLAY)%HOLD(ICOL,IROW))
    ENDIF
   ENDDO
  ENDDO
 ENDIF
  
 !## correct if layers overlap
 DO ILAY=2,NLAY-1
  !## check only whenever icheck is active
  IF(ICHECK_IDF(ILAY).EQ.0)CYCLE
  DO IROW=1,SOLIDF(ILAY)%NROW
   DO ICOL=1,SOLIDF(ILAY)%NCOL
    CALL IDFGETLOC(SOLIDF(ILAY),IROW,ICOL,X,Y)
    CALL IDFIROWICOL(SOLIDF(ILAY-1),JROW,JCOL,X,Y)
    IF(JROW.NE.0.AND.JCOL.NE.0)THEN   
     TOP=PCG(ILAY-1)%HOLD(JCOL,JROW)
     PCG(ILAY)%HOLD(ICOL,IROW)=MIN(PCG(ILAY)%HOLD(ICOL,IROW),TOP)
    ENDIF
    !## make sure all layers are higher than base
    IF(ICHECK_IDF(NLAY).EQ.1)THEN
     CALL IDFIROWICOL(SOLIDF(NLAY),JROW,JCOL,X,Y)
     IF(JROW.NE.0.AND.JCOL.NE.0)THEN
      PCG(ILAY)%HOLD(ICOL,IROW)=MAX(PCG(ILAY)%HOLD(ICOL,IROW),PCG(NLAY)%HOLD(JCOL,JROW))
     ENDIF
    ENDIF    
   ENDDO
  ENDDO
 ENDDO

 END SUBROUTINE SOLID_CONSISTENT

 !###======================================================================
 SUBROUTINE SOLID_GET_BND(ILAY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ILAY
 INTEGER :: IROW,ICOL,JROW,JCOL,I,J,N,M
 REAL :: X,Y
 INTEGER,DIMENSION(4) :: DC,DR
 DATA DC/0,1,0,-1/
 DATA DR/-1,0,1,0/
 
 !## see whether to attach above layers
 IF(ILAY.GT.1)THEN
  DO IROW=1,SOLIDF(ILAY)%NROW; DO ICOL=1,SOLIDF(ILAY)%NCOL
   !## get current x/y location
   CALL IDFGETLOC(SOLIDF(ILAY),IROW,ICOL,X,Y)
   CALL IDFIROWICOL(SOLIDF(ILAY-1),JROW,JCOL,X,Y)
   IF(JROW.NE.0.AND.JCOL.NE.0)THEN
    IF(PCG(ILAY)%IB(ICOL,IROW).EQ.2.AND.PCG(ILAY-1)%IB(JCOL,JROW).NE.0)THEN
     PCG(ILAY)%IB(ICOL,IROW)=-1
     PCG(ILAY)%HOLD(ICOL,IROW)=PCG(ILAY-1)%HOLD(JCOL,JROW)
    ENDIF
   ENDIF
  ENDDO; ENDDO
 ENDIF
 
 N=0
 DO I=1,2
  !## find outside points of known-locations
  DO IROW=1,SOLIDF(ILAY)%NROW; DO ICOL=1,SOLIDF(ILAY)%NCOL
   !## check current location to be part of the kriging data set
   IF(PCG(ILAY)%IB(ICOL,IROW).EQ.-1)THEN
    DO J=1,SIZE(DC)
     JROW=IROW+DR(J); JCOL=ICOL+DC(J)
     IF(JROW.GE.1.AND.JROW.LE.SOLIDF(ILAY)%NROW.AND. &
        JCOL.GE.1.AND.JCOL.LE.SOLIDF(ILAY)%NCOL)THEN
      IF(PCG(ILAY)%IB(JCOL,JROW).GT.0)THEN
       N=N+1
       IF(I.EQ.2)THEN
        CALL IDFGETLOC(SOLIDF(ILAY),IROW,ICOL,XD(N),YD(N))
        ZD(N)=PCG(ILAY)%HOLD(ICOL,IROW)
       ENDIF
       EXIT
      ENDIF
     ENDIF
    ENDDO
   ENDIF
  ENDDO; ENDDO
  IF(N.EQ.0)EXIT
  IF(I.EQ.1)THEN
   M=SIZE(XD); ALLOCATE(XDDUMMY(M+N),YDDUMMY(M+N),ZDDUMMY(M+N))
   DO J=1,M; XDDUMMY(J)=XD(J); YDDUMMY(J)=YD(J); ZDDUMMY(J)=ZD(J); ENDDO
   DEALLOCATE(XD,YD,ZD); XD=>XDDUMMY; YD=>YDDUMMY; ZD=>ZDDUMMY; N=M
  ENDIF
 ENDDO

 END SUBROUTINE SOLID_GET_BND

 !###======================================================================
 LOGICAL FUNCTION SOLID_READIDF()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 SOLID_READIDF=.FALSE.
 
 NTBSOL=SLD(1)%NINT; ALLOCATE(SOLIDF(NTBSOL))
 DO I=1,NTBSOL; CALL IDFNULLIFY(SOLIDF(I)); ENDDO

 !## try to read all idf's
 DO I=1,SLD(1)%NINT
  !## read entire IDF
  IF(.NOT.IDFREAD(SOLIDF(I),SLD(1)%INTNAME(I),0))RETURN !## read idf settings
 ENDDO

 IF(IBATCH.EQ.0)THEN; CALL UTL_MESSAGEHANDLE(0); CALL WINDOWSELECT(0); ENDIF

 IF(IBATCH.EQ.1)THEN
  !## no window specified, use minimum overlapping area of set of idf files
  IF(IWINDOW.EQ.0)THEN
   MDLIDF%XMIN=MAXVAL(SOLIDF%XMIN); MDLIDF%XMAX=MINVAL(SOLIDF%XMAX)  
   MDLIDF%YMIN=MAXVAL(SOLIDF%YMIN); MDLIDF%YMAX=MINVAL(SOLIDF%YMAX)  
   MDLIDF%DX=MIN(MINVAL(SOLIDF%DX),MINVAL(SOLIDF%DY)); MDLIDF%DY=MDLIDF%DX
  ENDIF
  CALL UTL_IDFSNAPTOGRID(MDLIDF%XMIN,MDLIDF%XMAX,MDLIDF%YMIN,MDLIDF%YMAX,MDLIDF%DX,MDLIDF%NCOL,MDLIDF%NROW)
  DO I=1,SLD(1)%NINT
   CALL IDFCOPY(MDLIDF,SOLIDF(I))
   IF(.NOT.IDFREADSCALE(SLD(1)%INTNAME(I),SOLIDF(I),2,1,0.0,0))RETURN !## scale use mean algorithm
  ENDDO
 ELSE
  DO I=1,SLD(1)%NINT
   IF(IWINDOW.EQ.1)THEN
    SOLIDF(I)%XMIN=MDLIDF%XMIN; SOLIDF(I)%XMAX=MDLIDF%XMAX
    SOLIDF(I)%YMIN=MDLIDF%YMIN; SOLIDF(I)%YMAX=MDLIDF%YMAX
   ENDIF
   SOLIDF(I)%DX=SLD(1)%XRESOLUTION(I); SOLIDF(I)%DY=SOLIDF(I)%DX
   CALL UTL_IDFSNAPTOGRID(SOLIDF(I)%XMIN,SOLIDF(I)%XMAX,SOLIDF(I)%YMIN,SOLIDF(I)%YMAX,SOLIDF(I)%DX,SOLIDF(I)%NCOL,SOLIDF(I)%NROW)
   !## scale use mean algorithm
   IF(.NOT.IDFREADSCALE(SLD(1)%INTNAME(I),SOLIDF(I),2,1,0.0,0))RETURN 
  ENDDO
 ENDIF
 
 NLAY=NTBSOL
 
 SOLID_READIDF=.TRUE.
 
 END FUNCTION SOLID_READIDF
 
 !###======================================================================
 LOGICAL FUNCTION SOLID_CALC_INIT(IVERSION,IKRIGING,IEXPORTMODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IVERSION,IKRIGING,IEXPORTMODE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I,N,ICOL,IROW
 TYPE(IDFOBJ) :: XIDF
 CHARACTER(LEN=256) :: LINE
 
 SOLID_CALC_INIT=.FALSE.

 !## load solid-fit dialog
 CALL WDIALOGLOAD(ID_DSOLID_CALC,ID_DSOLID_CALC)
 CALL WDIALOGTITLE('Compute Interfaces')
 
 IF(WINFOGRID(IDF_GRID1,GRIDROWSMAX).LT.NTBSOL+1)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not store '//TRIM(ITOS(N))//' rows in grid','Error')
  CALL WDIALOGUNLOAD(); RETURN  
 ENDIF

 IF(ALLOCATED(ISEL_IDF))DEALLOCATE(ISEL_IDF)
 IF(ALLOCATED(ICHECK))DEALLOCATE(ICHECK)
 IF(ALLOCATED(IACT))DEALLOCATE(IACT)
 IF(ALLOCATED(XRESOLUTION))DEALLOCATE(XRESOLUTION)
 N=NTBSOL+1; CALL WGRIDROWS(IDF_GRID1,N); ALLOCATE(ISEL_IDF(N),IACT(N),ICHECK(N),XRESOLUTION(N)) 

 IACT=1; ICHECK=1; ISEL_IDF=1
 DO I=1,NTBSOL; ISEL_IDF(I+1)=I; ENDDO
 CALL WGRIDLABELROW(IDF_GRID1,1,'All Interfaces')
 DO I=1,NTBSOL
  CALL WGRIDLABELROW(IDF_GRID1,I+1,'('//TRIM(ITOS(I))//') Interface '//TRIM(ITOS(I)))
  CALL WGRIDPUTCELLSTRING(IDF_GRID1,4,I+1,SLD(1)%INTNAME(I)(INDEX(SLD(1)%INTNAME(I),'\',.TRUE.)+1:))
  IF(.NOT.IDFREAD(XIDF,SLD(1)%INTNAME(I),0))THEN; ENDIF
  IF(SLD(1)%XRESOLUTION(I).LE.0.0)SLD(1)%XRESOLUTION(I)=XIDF%DX
  CLOSE(XIDF%IU); XIDF%IU=0
  IACT(I+1)=SLD(1)%ICLC(I)
  ICHECK(I+1)=SLD(1)%ICHECK(I)
  XRESOLUTION(I+1)=SLD(1)%XRESOLUTION(I)
 ENDDO
 XRESOLUTION(1)=XRESOLUTION(2)
 CALL WGRIDPUTCHECKBOX(IDF_GRID1,1,IACT,N)
 CALL WGRIDPUTCHECKBOX(IDF_GRID1,2,ICHECK,N)
 CALL WGRIDPUTREAL(IDF_GRID1,3,XRESOLUTION,N)
 
 LINE=TRIM(RTOS(MPW%XMIN,'F',2))//','//TRIM(RTOS(MPW%XMAX,'F',2))//','// &
      TRIM(RTOS(MPW%YMIN,'F',2))//','//TRIM(RTOS(MPW%YMAX,'F',2))
 CALL WDIALOGPUTSTRING(IDF_STRING1,TRIM(LINE))
 CALL WDIALOGFIELDSTATE(IDF_STRING1,0)
 
 CALL WGRIDSTATE(IDF_GRID1,4,2)
 CALL WDIALOGSHOW(-1,-1,0,3)
 
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE(ITYPE)

   CASE(FIELDCHANGED)

    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDF_GRID1)
      CALL WDIALOGSELECT(ID_DSOLID_FIT)
      !## report from cell
      CALL WGRIDPOS(MESSAGE%X,ICOL,IROW)
      IF(IROW.EQ.1)THEN
       SELECT CASE (ICOL)
        CASE (3)
         CALL WGRIDGETREAL(IDF_GRID1,3,XRESOLUTION,N)
         XRESOLUTION(2:N)=XRESOLUTION(1)
         CALL WGRIDPUTREAL(IDF_GRID1,3,XRESOLUTION,N)
       END SELECT
      ENDIF
    END SELECT
    
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_CHECK1)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
      CALL WDIALOGFIELDSTATE(IDF_STRING1,I)
     CASE (IDF_RADIO1,IDF_RADIO2)
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
      CALL WDIALOGFIELDSTATE(IDF_INTEGER1,I-1)
     CASE (IDF_GRID1)
      CALL WDIALOGSELECT(ID_DSOLID_FIT)

      !## report to cell
      CALL WGRIDPOS(MESSAGE%Y,ICOL,IROW)
      IF(IROW.EQ.1)THEN
       SELECT CASE (ICOL)
        CASE (1)
         CALL WGRIDGETCHECKBOX(IDF_GRID1,1,IACT,N)
         IACT(2:N)=IACT(1)
         CALL WGRIDPUTCHECKBOX(IDF_GRID1,1,IACT,N)
        CASE (2)
         CALL WGRIDGETCHECKBOX(IDF_GRID1,2,ICHECK,N)
         ICHECK(2:N)=ICHECK(1)
         CALL WGRIDPUTCHECKBOX(IDF_GRID1,2,ICHECK,N)
       END SELECT
      ENDIF
    END SELECT

   CASE(PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (ID_PROPERTIES_KRIGING)
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,IKRIGING)
      CALL WDIALOGGETINTEGER(IDF_INTEGER2,I)
      IF(I.EQ.0)I=1
      CALL KRIGINGSETTINGS(KSETTINGS(I)%MINP,KSETTINGS(I)%KTYPE,KSETTINGS(I)%RANGE,KSETTINGS(I)%SILL, &
                           KSETTINGS(I)%NUGGET,KSETTINGS(I)%PNTSEARCH,KSETTINGS(I)%IADJRANGE,KSETTINGS(I)%IQUADRANT)
      CALL WDIALOGSELECT(ID_DSOLID_CALC)
      CALL WDIALOGGETINTEGER(IDF_INTEGER2,I)
      IF(I.EQ.0)THEN; DO I=2,SIZE(KSETTINGS); KSETTINGS(I)=KSETTINGS(1); ENDDO; ENDIF
     !## start computing or get semivariogram
     CASE (IDCANCEL)
      IKRIGING=0; EXIT
     CASE (ID_SEMIVARIOGRAM)
      CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to compute Semivariogram(s) for the selected layers ? ','Question')
      IF(WINFODIALOG(4).EQ.1)THEN; SOLID_CALC_INIT=.TRUE.; IKRIGING=2; EXIT; ENDIF
     CASE (ID_EXPORT)
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO8,IEXPORTMODE)
      SOLID_CALC_INIT=.TRUE.; IKRIGING=3; EXIT
     CASE (IDOK)
      CALL WGRIDGETCHECKBOX(IDF_GRID1,1,IACT,N)
      CALL WGRIDGETCHECKBOX(IDF_GRID1,2,ICHECK,N)
      CALL WGRIDGETREAL(IDF_GRID1,3,XRESOLUTION,N)
      DO I=1,NTBSOL
       SLD(1)%ICLC(I)=IACT(I+1); SLD(1)%ICHECK(I)=ICHECK(I+1); SLD(1)%XRESOLUTION(I)=XRESOLUTION(I+1)
      ENDDO
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IWINDOW)
      IF(IWINDOW.EQ.1)THEN
       CALL WDIALOGGETSTRING(IDF_STRING1,LINE)
       READ(LINE,*) MDLIDF%XMIN,MDLIDF%XMAX,MDLIDF%YMIN,MDLIDF%YMAX
      ENDIF
      SOLID_CALC_INIT=.TRUE.; IKRIGING=1; EXIT
     CASE (IDHELP)
      CALL IMODGETHELP('5.4','TMO.SolTool')
    END SELECT
  END SELECT

 ENDDO

 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
 IVERSION=0; IF(I.EQ.2)CALL WDIALOGGETINTEGER(IDF_INTEGER1,IVERSION)

 DEALLOCATE(ISEL_IDF,IACT,ICHECK) 

 CALL WDIALOGUNLOAD()

 END FUNCTION SOLID_CALC_INIT
 
 !###======================================================================
 SUBROUTINE SOLID_PCGINT(XD,YD,ZD,ND,IERROR,IDF,IECHO,IDUPLICATE,HNOFLOW,CD,IPC,CC_GIVEN) 
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: FCT_CC_GIVEN=0.01
 INTEGER,INTENT(IN) :: ND,IECHO !## 1=WINDOWS -1=DOSBOX
 INTEGER,INTENT(IN),OPTIONAL :: IDUPLICATE
 REAL,OPTIONAL,INTENT(IN) :: HNOFLOW !## make areas inactive values in idf%x() equal to hoflow
 REAL,DIMENSION(:),POINTER :: XD,YD,ZD
 REAL,DIMENSION(:),POINTER,INTENT(IN),OPTIONAL :: CD
 INTEGER(KIND=1),DIMENSION(:,:,:),INTENT(IN),OPTIONAL :: IPC
 INTEGER,INTENT(OUT) :: IERROR
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 TYPE(IDFOBJ),OPTIONAL,INTENT(IN) :: CC_GIVEN
 LOGICAL :: LEX
 INTEGER :: I,ITER1,ITER2,IROW,ICOL,ICNVG,NICNVG,ITYPE
 TYPE(WIN_MESSAGE) :: MESSAGE
 REAL :: Z1,Z2,INIHEAD,C,HCHG,HCHGOLD,S,RCHG

 IERROR=1
 
 !## pcg can not solve perfect flat area, so idf%x=zd
 Z1=MINVAL(ZD(1:ND)); Z2=MAXVAL(ZD(1:ND))
 IF(Z1.EQ.Z2)THEN; IDF%X(:,:)=Z1; CALL UTL_MESSAGEHANDLE(1); IERROR=0; RETURN; ENDIF
 
 NROW=IDF%NROW; NCOL=IDF%NCOL; NLAY=1
 !## allocate memory
 IF(.NOT.SOLID_CALC_AL(NROW=NROW,NCOL=NCOL).OR. &
    .NOT.SOLID_CALC_AL_PCG(NROW,NCOL))THEN
  CALL SOLID_CALC_DAL()
  RETURN
 ENDIF

 !## adjust starting head to be mean of all fixed heads
 INIHEAD=(Z1+Z2)/2.0
 PCG(1)%HOLD=0.0; PCG(1)%IB=1
 !## isotroop
 IF(PRESENT(HNOFLOW))THEN
  DO IROW=1,NROW; DO ICOL=1,NCOL
   IF(IDF%X(ICOL,IROW).EQ.HNOFLOW)PCG(1)%IB(ICOL,IROW)=0
  ENDDO; ENDDO
 ENDIF
 
 PCG(1)%CR=1.0; PCG(1)%CC=1.0; PCG(1)%RHS=0.0; PCG(1)%HCOF=0.0; PCG(1)%SS=0.0
 
 !## apply given conductance
 IF(PRESENT(CC_GIVEN))THEN
  DO IROW=1,NROW; DO ICOL=1,NCOL
   IF(CC_GIVEN%X(ICOL,IROW).EQ.CC_GIVEN%NODATA)THEN
    PCG(1)%CR(ICOL,IROW)=FCT_CC_GIVEN*PCG(1)%CR(ICOL,IROW)
    PCG(1)%CC(ICOL,IROW)=FCT_CC_GIVEN*PCG(1)%CC(ICOL,IROW)
   ENDIF
  ENDDO; ENDDO
 ENDIF
 
 !## apply faults
 IF(PRESENT(IPC))THEN
  CALL PCG_HFB_FM(IPC,PCG(1)%CC,PCG(1)%CR,NROW,NCOL,0.01)
 ENDIF
 
 !## create boundary/fixed heads
 DO I=1,ND
  ICOL=INT(XD(I)); IROW=INT(YD(I))
  !### skip inactive
  IF(PCG(1)%IB(ICOL,IROW).EQ.0)CYCLE
  PCG(1)%HOLD(ICOL,IROW)=PCG(1)%HOLD(ICOL,IROW)+ZD(I)
  PCG(1)%RHS(ICOL,IROW) =PCG(1)%RHS(ICOL,IROW)+1.0
  IF(PRESENT(CD))THEN
   IF(CD(I).GT.0.0)PCG(1)%IB(ICOL,IROW)=-3
  ELSE
   PCG(1)%IB(ICOL,IROW)= -1
  ENDIF
 ENDDO
 !## compute mean/sum
 IF(PRESENT(IDUPLICATE))THEN
  IF(IDUPLICATE.EQ.1)PCG(1)%RHS=1.0
 ENDIF
 DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(PCG(1)%IB(ICOL,IROW).GT.0)THEN
   PCG(1)%HOLD(ICOL,IROW)=INIHEAD
  ELSEIF(PCG(1)%IB(ICOL,IROW).LT.0)THEN
   PCG(1)%HOLD(ICOL,IROW)=PCG(1)%HOLD(ICOL,IROW)/PCG(1)%RHS(ICOL,IROW)
  ENDIF
 ENDDO; ENDDO 
 PCG(1)%RHS=0.0
 
! IF(PRESENT(LBNDCHK))THEN
!  IF(LBNDCHK)THEN
!   !## change boundary condition
!   DO IROW=1,NROW; DO ICOL=1,NCOL
!    IF(PCG(1)%IB(ICOL,IROW).EQ.-3)PCG(1)%IB(ICOL,IROW)=0
!   ENDDO; ENDDO
!   CALL SOLID_CHECKIBND(1,NROW,NCOL)
!   IF(MAXVAL(PCG(1)%IB).EQ.0)THEN
!    WRITE(*,'(A)') 'Not possible to simulate model, no connection to boundary condition'
!    !## no boundary criterion condition met
!    IERROR=2; CALL SOLID_CALC_DAL(); RETURN
!   ENDIF
!   !## change boundary condition
!   DO IROW=1,NROW; DO ICOL=1,NCOL
!    IF(PCG(1)%SS(ICOL,IROW).LT.0)PCG(1)%IB(ICOL,IROW)=PCG(1)%SS(ICOL,IROW)
!   ENDDO; ENDDO
!  ENDIF
! ENDIF
 
 !## change boundary condition - if itight is 2
 IF(ITIGHT.EQ.2)THEN
  DO IROW=1,NROW; DO ICOL=1,NCOL
   IF(PCG(1)%IB(ICOL,IROW).EQ.-3)PCG(1)%IB(ICOL,IROW)=3.0
  ENDDO; ENDDO
 ENDIF

 !## solve each system per modellayer
 NICNVG=0; PCG(1)%HNEW=PCG(1)%HOLD
 
 DO ITER1=1,MXITER1 
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  !## change boundary condition
  IF(ITIGHT.EQ.2)THEN
   IF(PRESENT(CD))THEN
    DO I=1,ND
     ICOL=INT(XD(I)); IROW=INT(YD(I)); C=CD(I)
     PCG(1)%RHS(ICOL,IROW) =-C*PCG(1)%HOLD(ICOL,IROW)
     PCG(1)%HCOF(ICOL,IROW)=-C
    ENDDO
   ELSE
    C=0.1
    DO I=1,ND
     ICOL=INT(XD(I)); IROW=INT(YD(I))
     PCG(1)%RHS(ICOL,IROW) =-C*PCG(1)%HOLD(ICOL,IROW)
     PCG(1)%HCOF(ICOL,IROW)=-C
    ENDDO
   ENDIF
  ENDIF

  CALL PCG2AP(NROW*NCOL,NROW,NCOL,1,PCG(1)%IB,PCG(1)%CR,PCG(1)%CC,PCG(1)%CV,PCG(1)%HCOF,PCG(1)%RHS,PCG(1)%V, &
              PCG(1)%SS,PCG(1)%P,PCG(1)%CD,PCG(1)%HNEW,MXITER1,MXITER2,ITER1,ITER2,ICNVG,HCLOSE,RCLOSE,IECHO,&
              NICNVG,RELAX,HCHG,RCHG) 

  !## convergence achieved
  IF(ICNVG.EQ.1)EXIT
  IF(MICNVG.GT.0.AND.NICNVG.GT.MICNVG)EXIT

  IF(IDAMPING.EQ.1)THEN
   IF(ITER1.EQ.1)HCHGOLD=HCHG
   S=HCHG/(RELAX*HCHGOLD)
   IF(S.LT.-1.0)THEN
    RELAX=1.0/(2.0*ABS(S))
   ELSE
    RELAX=(3.0+S)/(3.0+ABS(S))
   ENDIF
   HCHGOLD=HCHG
  ENDIF

 ENDDO
 PCG(1)%HOLD=REAL(PCG(1)%HNEW)
 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,''); CALL WINDOWOUTSTATUSBAR(3,'')

 LEX=.TRUE.
 IF(ICNVG.NE.1)THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Interpolation did not converge.'//CHAR(13)// &
    'Hclosure ='//TRIM(RTOS(HCHG,'E',7))//CHAR(13)//'Do you want to save the results so far in:'//CHAR(13)// &
     '['//TRIM(FNAME)//']','Question')
  IF(WINFODIALOG(4).EQ.1)LEX=.TRUE.
 ENDIF

 IF(LEX)IDF%X(:,:)=PCG(1)%HOLD

 !## clean for inactive cells
 IF(PRESENT(HNOFLOW))THEN
  DO IROW=1,NROW; DO ICOL=1,NCOL
   IF(PCG(1)%IB(ICOL,IROW).EQ.0)IDF%X(ICOL,IROW)=HNOFLOW
  ENDDO; ENDDO
 ENDIF
 
 !## free memory for pcg-computation
 CALL SOLID_CALC_DAL()

 CALL UTL_MESSAGEHANDLE(1)
 IERROR=0

 END SUBROUTINE SOLID_PCGINT
 
 !###======================================================================
 SUBROUTINE SOLID_CALC_CONSTRAINS(JLAY) 
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: C=0.1   !## day resistance
 INTEGER,INTENT(IN) :: JLAY
 INTEGER :: IROW,ICOL,ILAY,IL1,IL2,JROW,JCOL,ITOP,IBOT
 REAL :: TOP,BOT,D,DH,DSYS,COND,X,Y
 
 PCG(1)%RHS=0.0; PCG(1)%HCOF=0.0
 COND=(SOLIDF(JLAY)%DX*SOLIDF(JLAY)%DY)/C
 
 !## make sure interfaces do not cross with top- and bottom interfaces
 IF(IINT_IDF.EQ.1)THEN 
 
  !## fill dh with minimal thickness for aquifer

  !## fill top constrains (drain, all above available layers, filled in yet!)
  DO IROW=1,SOLIDF(JLAY)%NROW; DO ICOL=1,SOLIDF(JLAY)%NCOL

   IF(PCG(JLAY)%IB(ICOL,IROW).GT.0)THEN

    !## get current x/y location
    CALL IDFGETLOC(SOLIDF(JLAY),IROW,ICOL,X,Y)

    !## find top aquifer 
    ITOP=0
    DO IL1=JLAY-1,1,-1
     CALL IDFIROWICOL(SOLIDF(IL1),JROW,JCOL,X,Y)
     IF(JROW.NE.0.AND.JCOL.NE.0)THEN
      IF(PCG(IL1)%IB(JCOL,JROW).LT.0)THEN
       TOP=PCG(IL1)%HOLD(JCOL,JROW); ITOP=1; EXIT
      ENDIF
     ENDIF
    ENDDO
    !## use last one - surfacelevel
    IL1=MAX(1,IL1)
    IF(ITOP.EQ.0.AND.JLAY.GT.1)THEN; TOP=PCG(IL1)%HOLD(JCOL,JROW); ITOP=1; ENDIF
    
    !## find bot aquifer 
    IBOT=0
    DO IL2=JLAY+1,NLAY
     CALL IDFIROWICOL(SOLIDF(IL2),JROW,JCOL,X,Y)
     IF(JROW.NE.0.AND.JCOL.NE.0)THEN
      IF(PCG(IL2)%IB(JCOL,JROW).LT.0)THEN
       BOT=PCG(IL2)%HOLD(JCOL,JROW); IBOT=1; EXIT
      ENDIF
     ENDIF
    ENDDO
    !## use last one - base
    IL2=MIN(NLAY,IL2)
    IF(IBOT.EQ.0.AND.JLAY.LT.NLAY)THEN; BOT=PCG(IL2)%HOLD(JCOL,JROW); IBOT=1; ENDIF
        
    !## top/bottom found
    IF(ITOP.EQ.1.AND.IBOT.EQ.1)THEN
    
     !## mean thickness available
     DSYS=(TOP-BOT)/REAL(IL2-IL1-1)
     !## can not be negative
     DSYS=MAX(0.0,DSYS)
   
     !## add drain(s) if head above level above layer
     DO ILAY=JLAY-1,1,-1
      DH=MIN(DZ(ILAY),DSYS)
      !## skip inactive cells
      CALL IDFIROWICOL(SOLIDF(ILAY),JROW,JCOL,X,Y)
      IF(PCG(ILAY)%IB(JCOL,JROW).EQ.0)CYCLE
      IF(PCG(1)%HNEW(ICOL,IROW).GE.(PCG(ILAY)%HOLD(JCOL,JROW)-DH))THEN
       !## create drain
       PCG(1)%HCOF(ICOL,IROW)=PCG(1)%HCOF(ICOL,IROW)-COND
       PCG(1)%RHS(ICOL,IROW) =PCG(1)%RHS(ICOL,IROW) -COND*(PCG(ILAY)%HOLD(JCOL,JROW)-DH)
      ENDIF
     ENDDO
   
     !## fill bottom constrains (river, all below available layers with constant heads)
     !## excluding the base (nlay)
     DO ILAY=JLAY+1,NLAY-1
      !## skip inactive cells
      CALL IDFIROWICOL(SOLIDF(ILAY),JROW,JCOL,X,Y)
      !## add river if head below level below and constant head value available (clay)
      IF(PCG(ILAY)%IB(JCOL,JROW).LT.0)THEN 
       DH=MIN(DZ(ILAY),DSYS)
       IF(PCG(1)%HNEW(ICOL,IROW).LE.(PCG(ILAY)%HOLD(JCOL,JROW)+DH))THEN
        !## create river
        PCG(1)%HCOF(ICOL,IROW)=PCG(1)%HCOF(ICOL,IROW)-COND
        PCG(1)%RHS(ICOL,IROW) =PCG(1)%RHS(ICOL,IROW) -COND*(PCG(ILAY)%HOLD(JCOL,JROW)+DH)
       ENDIF
      ENDIF
     ENDDO
     
     IF(FMIDELEV.GT.0.0)THEN
      D=DSYS*REAL(JLAY-IL1)
      !## create ghb as estimate
      PCG(1)%HCOF(ICOL,IROW)=PCG(1)%HCOF(ICOL,IROW)-(FMIDELEV*COND/10000.0)
      PCG(1)%RHS(ICOL,IROW) =PCG(1)%RHS(ICOL,IROW) -(FMIDELEV*COND/10000.0)*(TOP-D)
     ENDIF
     
    ENDIF
   ENDIF
  ENDDO; ENDDO
 
 ENDIF
 
 !## cross-section in between looser
 IF(ITIGHT.EQ.2)THEN
  DO IROW=1,SOLIDF(JLAY)%NROW; DO ICOL=1,SOLIDF(JLAY)%NCOL
   IF(PCG(JLAY)%IB(ICOL,IROW).EQ.3)THEN 
    !## create ghb as estimate
    COND=FTIGHT*PCG(JLAY)%COND(ICOL,IROW) 
    PCG(1)%HCOF(ICOL,IROW)=PCG(1)%HCOF(ICOL,IROW)-COND
    PCG(1)%RHS(ICOL,IROW) =PCG(1)%RHS(ICOL,IROW) -COND*PCG(JLAY)%HOLD(ICOL,IROW) 
   ENDIF
  ENDDO; ENDDO
 ENDIF
 
 IF(MINVAL(PCG(1)%IB).EQ.0.AND.MINVAL(PCG(1)%HCOF).EQ.0.0.AND.MAXVAL(PCG(1)%HCOF).EQ.0)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Probably this interface will not compute due to missing boundary conditions','Error')
  IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Probably this interface will not compute due to missing boundary conditions'
 ENDIF

 END SUBROUTINE SOLID_CALC_CONSTRAINS

 !###======================================================================
 LOGICAL FUNCTION SOLID_CALC_FILL_BND()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ICOL,IROW,ILAY,I,N,IR1,IR2,IC1,IC2
 REAL :: X1,X2,Y1,Y2
   
 SOLID_CALC_FILL_BND=.FALSE.

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Creating Interpolation Matrix ...')
 IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Creating Interpolation Matrix ...'
 
 !## process boundary
 IF(SHPNO.EQ.0.AND.NMASK.EQ.0)THEN
  DO I=1,SIZE(PCG); PCG(I)%IB=1; ENDDO    !## all active
 !## include boundary, mark them as inactive ...
 ELSE
  !## all constant head --- nothing to do with!
  DO I=1,SIZE(PCG); PCG(I)%IB=1; ENDDO

  !## include shapes
  IF(SHPNO.GT.0)THEN
   !## all inactive (constant head)
   DO I=1,SIZE(PCG); PCG(I)%IB=-1; ENDDO
   N=MAXVAL(SHPNCRD)
   DO ILAY=1,NLAY
    DO I=1,SHPNO
    
     X1=MINVAL(SHPXC(1:SHPNCRD(I),I)); X2=MAXVAL(SHPXC(1:SHPNCRD(I),I))
     Y1=MINVAL(SHPYC(1:SHPNCRD(I),I)); Y2=MAXVAL(SHPYC(1:SHPNCRD(I),I))
     !## get ofset from xmin/ymin in the number of cells
     CALL IDFIROWICOL(SOLIDF(ILAY),IR1,IC1,X1,Y2)
     CALL IDFIROWICOL(SOLIDF(ILAY),IR2,IC2,X2,Y1)
     IF(IC2.EQ.0)IC2=SOLIDF(ILAY)%NCOL; IF(IR2.EQ.0)IR2=SOLIDF(ILAY)%NROW
     
     DO IROW=MAX(1,IR1),MIN(IR2,SOLIDF(ILAY)%NROW)
      DO ICOL=MAX(1,IC1),MIN(IC2,SOLIDF(ILAY)%NCOL)
       CALL IDFGETLOC(SOLIDF(ILAY),IROW,ICOL,X1,Y1)
       !## inside polygon --- thus editable
       IF(UTL_INSIDEPOLYGON(X1,Y1,SHPXC(:,I),SHPYC(:,I),SHPNCRD(I)).EQ.1)PCG(ILAY)%IB(ICOL,IROW)=1
      ENDDO
     ENDDO
    
    ENDDO
   ENDDO
  ENDIF

  !## include masks (ibound)
  IF(NMASK.GT.0)THEN
   DO ILAY=1,NMASK
    CALL IDFCOPY(SOLIDF(ILAY),MASK(ILAY)%IDF)
    IF(.NOT.IDFREADSCALE(MASK(ILAY)%FNAME,MASK(ILAY)%IDF,1,1,0.0,0))RETURN
    !## mask equal to solidf()  
    IF(.NOT.IDFEQUAL(MASK(ILAY)%IDF,SOLIDF(ILAY),1))RETURN
    DO IROW=1,SOLIDF(ILAY)%NROW; DO ICOL=1,SOLIDF(ILAY)%NCOL
     !## values in between -1,0,2
     !## -2 = constant head on cross-section - to be used for tight/loose option
     !## -1 = constant precomputed value
     !##  0 = inactive does not influence results
     !##  1 = active will be computed
     !##  2 = active will be computed - forced to be attached to the upper layer, so acts a -1 to the upper layer
     PCG(ILAY)%IB(ICOL,IROW)=MAX(MIN(2,INT(MASK(ILAY)%IDF%X(ICOL,IROW))),-1)
    ENDDO; ENDDO
    CALL IDFDEALLOCATEX(MASK(ILAY)%IDF)
   ENDDO
  ENDIF
 ENDIF

 !## process all idf's (starting heads) + crosssections (fixed heads/ghb-heads)
 DO ILAY=1,NLAY
  DO IROW=1,SOLIDF(ILAY)%NROW; DO ICOL=1,SOLIDF(ILAY)%NCOL
   !## starting heads --- top/bot sequence
   PCG(ILAY)%HOLD(ICOL,IROW)=SOLIDF(ILAY)%X(ICOL,IROW) 
   !## nodata values cannot be combined with ibound<0
   IF(PCG(ILAY)%IB(ICOL,IROW).LT.0)THEN
    !## mask should organize boundary conditions
    IF(PCG(ILAY)%HOLD(ICOL,IROW).EQ.SOLIDF(ILAY)%NODATA)PCG(ILAY)%IB(ICOL,IROW)=1
   ENDIF
   !## apply ibound=0 for nodata values
   IF(PCG(ILAY)%IB(ICOL,IROW).EQ.0)PCG(ILAY)%HOLD(ICOL,IROW)=HNOFLOW
  END DO; END DO
 END DO
 
 SOLID_CALC_FILL_BND=.TRUE.

 END FUNCTION SOLID_CALC_FILL_BND

 !###======================================================================
 LOGICAL FUNCTION SOLID_CALC_FILL()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ICOL,IROW,ILAY,MX,ISPF,I,J,N,IPOS,NLOC,M,ISTEP,NN
 REAL :: DX,X1,X2,Y1,Y2,COND,DZZ,G,FX
 REAL,POINTER,DIMENSION(:) :: XI,ZI
 REAL,ALLOCATABLE,DIMENSION(:) :: TL,X,Z
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IC,IR
 REAL,ALLOCATABLE,DIMENSION(:,:) :: CD1,CD2,MH1,MH2
   
 SOLID_CALC_FILL=.FALSE.

 IF(.NOT.SOLID_CALC_FILL_BND())RETURN
 
 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Filling in Boundary Conditions ...')
 IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Filling in Boundary Conditions ...'
 
 !## count number of values in cell
 DO I=1,SIZE(PCG); PCG(I)%RHS=0.0; ENDDO

 !## number of layers to interpolate
 DO ILAY=1,NTBSOL
  IF(ISEL_IDF(ILAY).EQ.0)CYCLE

  !## do for number of cross-sections
  DO ISPF=1,NSPF
    
   !## skip lines that do not need to be processed
   IF(SPF(ISPF)%PROF(ILAY)%IACTIVE.EQ.0)CYCLE
   IF(SPF(ISPF)%PROF(ILAY)%NPOS.LE.0)CYCLE
    
   !## get max. length
   DX=0.0
   DO I=2,SPF(ISPF)%NXY
    DX=DX+SQRT((SPF(ISPF)%X(I)-SPF(ISPF)%X(I-1))**2.0+(SPF(ISPF)%Y(I)-SPF(ISPF)%Y(I-1))**2.0)
   ENDDO
   !## use minimal cellsize to estimate number of points to be used (times two)!
   MX=MAX(100,2*INT(DX/SOLIDF(ILAY)%DX))
   ALLOCATE(IC(MX),IR(MX),TL(0:MX))

   IF(LSPLINE)THEN
    !## create spline-points
    N=SPF(ISPF)%PROF(ILAY)%NPOS
    IF(N.GT.1)THEN
     DX=SPF(ISPF)%PROF(ILAY)%PX(N)-SPF(ISPF)%PROF(ILAY)%PX(1)
     M=CEILING(DX/SOLIDF(ILAY)%DX)+1
     ALLOCATE(XI(M),ZI(M)); XI=0.0; ZI=0.0
     X1=SPF(ISPF)%PROF(ILAY)%PX(1)-SOLIDF(ILAY)%DX; M=0
     DO
      X1=X1+SOLIDF(ILAY)%DX; M=M+1
      IF(X1.GE.SPF(ISPF)%PROF(ILAY)%PX(N))EXIT
      XI(M)=X1
     ENDDO   
     XI(M)=SPF(ISPF)%PROF(ILAY)%PX(N)
    !## spline line
!    CALL SPLINE_MAIN(SPF(ISPF)%PROF(ILAY)%PX,SPF(ISPF)%PROF(ILAY)%PZ,SPF(ISPF)%PROF(ILAY)%NPOS,XI,ZI,M)
     CALL SPLINE_AKIMA_MAIN(SPF(ISPF)%PROF(ILAY)%PX,SPF(ISPF)%PROF(ILAY)%PZ,SPF(ISPF)%PROF(ILAY)%NPOS,XI,ZI,M)  
    ENDIF
   ENDIF
       
   !## intersect/interpolate all segments of cross-section
   NLOC=0
   TL(0)=0.0
   DO I=2,SPF(ISPF)%NXY

    X1=SPF(ISPF)%X(I-1); X2=SPF(ISPF)%X(I)
    Y1=SPF(ISPF)%Y(I-1); Y2=SPF(ISPF)%Y(I)

    IF(SOLIDF(ILAY)%IEQ.EQ.0)THEN
     !## intersect line with rectangular-regular-equidistantial-grid
     N=0; CALL INTERSECT_EQUI(SOLIDF(ILAY)%XMIN,SOLIDF(ILAY)%XMAX,SOLIDF(ILAY)%YMIN,SOLIDF(ILAY)%YMAX, &
                              SOLIDF(ILAY)%DX,X1,X2,Y1,Y2,N,.FALSE.,.TRUE.)
    ELSE
     !## intersect line with rectangular-irregular-non-equidistantial-grid
     N=0; CALL INTERSECT_NONEQUI(SOLIDF(ILAY)%SX,SOLIDF(ILAY)%SY,SOLIDF(ILAY)%NROW,SOLIDF(ILAY)%NCOL, &
                                 X1,X2,Y1,Y2,N,.FALSE.,.TRUE.)
    ENDIF

    DO J=1,N
     !## count number of locations
     NLOC=NLOC+1; IC(NLOC)=INT(XA(J)); IR(NLOC)=INT(YA(J))
     IF(J.EQ.1)THEN
      TL(NLOC)=TL(NLOC-1)+0.5*LN(J)
     ELSEIF(J.EQ.N)THEN
      TL(NLOC)=TL(NLOC-1)+0.5*LN(J-1)+LN(J)
     ELSE
      TL(NLOC)=TL(NLOC-1)+0.5*LN(J-1)+0.5*LN(J)
     ENDIF
    ENDDO
      
   ENDDO
    
   !## process each pos()
   IF(LSPLINE)THEN
    PX=>XI; PZ=>ZI
   ELSE
    PX=>SPF(ISPF)%PROF(ILAY)%PX; PZ=>SPF(ISPF)%PROF(ILAY)%PZ
    N=SPF(ISPF)%PROF(ILAY)%NPOS
   ENDIF

   DO IPOS=2,SIZE(PX)  
    DX=PX(IPOS)-PX(IPOS-1); DZZ=PZ(IPOS)-PZ(IPOS-1)
    !## gradient (+) up (-) down
    G =DZZ/DX
    !## scan distance-table
    DO I=1,NLOC
     !## within current segment and within model domain
     IF(TL(I).GE.PX(IPOS-1).AND. &
        TL(I).LE.PX(IPOS).AND.   &
        IC(I).GE.1.AND.IC(I).LE.SOLIDF(ILAY)%NCOL.AND. &
        IR(I).GE.1.AND.IR(I).LE.SOLIDF(ILAY)%NROW)THEN
      !## active location with n mask-value present
      IF(PCG(ILAY)%IB(IC(I),IR(I)).EQ.1.OR.PCG(ILAY)%IB(IC(I),IR(I)).EQ.-2)THEN

       PCG(ILAY)%IB(IC(I),IR(I)) =-2
       PCG(ILAY)%RHS(IC(I),IR(I))= PCG(ILAY)%RHS(IC(I),IR(I))+1.0

       IF(PCG(ILAY)%RHS(IC(I),IR(I)).EQ.1.0)THEN
        !## overwrite HOLD with new value
        PCG(ILAY)%HOLD(IC(I),IR(I))=PZ(IPOS-1)+(TL(I)-PX(IPOS-1))*G
        PCG(ILAY)%COND(IC(I),IR(I))=TL(I)-TL(I-1)
       ELSE
        !## add new value to HOLD
        PCG(ILAY)%HOLD(IC(I),IR(I))=PCG(ILAY)%HOLD(IC(I),IR(I)) + (PZ(IPOS-1)+(TL(I)-PX(IPOS-1))*G)
        PCG(ILAY)%COND(IC(I),IR(I))=PCG(ILAY)%COND(IC(I),IR(I)) + (TL(I)-TL(I-1))
       ENDIF

      ENDIF
       
     ENDIF
    END DO
   ENDDO
   NULLIFY(PX,PZ)
   DEALLOCATE(TL,IC,IR)
  END DO !DO ISPF=1,NSPF
  CALL INTERSECT_DEALLOCATE()
 END DO !DO ILAY=1,NTBSOL 

 !## get mean values in case more values in one single cell
 DO ILAY=1,NLAY; DO IROW=1,SOLIDF(ILAY)%NROW; DO ICOL=1,SOLIDF(ILAY)%NCOL
  !## skip inactive cells
  IF(PCG(ILAY)%IB(ICOL,IROW).EQ.0)CYCLE
  IF(PCG(ILAY)%RHS(ICOL,IROW).GT.1.0)THEN
   PCG(ILAY)%HOLD(ICOL,IROW)=PCG(ILAY)%HOLD(ICOL,IROW)/PCG(ILAY)%RHS(ICOL,IROW)
  ENDIF 
 END DO; END DO; END DO
 
 IF(ITIGHT.EQ.2)THEN

  !## include more artifically spline lines
  DO ILAY=1,NLAY

   ALLOCATE(CD1(SOLIDF(ILAY)%NCOL,SOLIDF(ILAY)%NROW))
   ALLOCATE(CD2(SOLIDF(ILAY)%NCOL,SOLIDF(ILAY)%NROW))
   ALLOCATE(MH1(SOLIDF(ILAY)%NCOL,SOLIDF(ILAY)%NROW))
   ALLOCATE(MH2(SOLIDF(ILAY)%NCOL,SOLIDF(ILAY)%NROW))

   ISTEP=1; COND=SOLIDF(ILAY)%DX

   !## store conductance in row/column directory
   CD1=0.0; CD2=0.0; MH1=0.0; MH2=0.0

   !## define spline points along columns per row
   N=SOLIDF(ILAY)%NCOL; ALLOCATE(XI(N),ZI(N),X(N),Z(N)); XI=0.0; X=0.0; Z=0.0
   DO ICOL=1,N; CALL IDFGETLOC(SOLIDF(ILAY),1,ICOL,XI(ICOL),Y1); ENDDO   

   DO IROW=1,SOLIDF(ILAY)%NROW,ISTEP
    N=0; DO ICOL=1,SOLIDF(ILAY)%NCOL
     !## cross-section
     IF(PCG(ILAY)%IB(ICOL,IROW).EQ.-2)THEN
      N=N+1; CALL IDFGETLOC(SOLIDF(ILAY),IROW,ICOL,X(N),Y1); Z(N)=PCG(ILAY)%HOLD(ICOL,IROW)
     ENDIF
    ENDDO
    IF(N.GT.2)THEN
     ZI=SOLIDF(ILAY)%NODATA; NN=N
     CALL SPLINE_AKIMA_MAIN(X,Z,N,XI,ZI,SIZE(XI))  
     N=0; DO ICOL=1,SOLIDF(ILAY)%NCOL
      N=N+1
      IF(ZI(N).NE.SOLIDF(ILAY)%NODATA)THEN
       !## add to a non-cross-sectional location
       IF(PCG(ILAY)%IB(ICOL,IROW).NE.-2)THEN
        FX=SOLID_CALC_FILL_DISTANCE(XI(N),X,NN)
        MH1(ICOL,IROW)=ZI(N); CD1(ICOL,IROW)=COND*FX
       ENDIF
      ENDIF
     ENDDO
    ENDIF
   ENDDO
  
   DEALLOCATE(XI,ZI,X,Z)
  
   !## define spline points along columns per row
   N=SOLIDF(ILAY)%NROW; ALLOCATE(XI(N),ZI(N),X(N),Z(N)); XI=0.0; X=0.0; Z=0.0
   I=0; DO IROW=N,1,-1; I=I+1; CALL IDFGETLOC(SOLIDF(ILAY),IROW,1,X1,XI(I)); ENDDO   

   DO ICOL=1,SOLIDF(ILAY)%NCOL,ISTEP
    N=0; DO IROW=SOLIDF(ILAY)%NROW,1,-1
     !## cross-section
     IF(PCG(ILAY)%IB(ICOL,IROW).EQ.-2)THEN
      N=N+1; CALL IDFGETLOC(SOLIDF(ILAY),IROW,ICOL,X1,X(N)); Z(N)=PCG(ILAY)%HOLD(ICOL,IROW)
     ENDIF
    ENDDO
    IF(N.GT.2)THEN
     ZI=SOLIDF(ILAY)%NODATA; NN=N
     CALL SPLINE_AKIMA_MAIN(X,Z,N,XI,ZI,SIZE(XI))
     N=0; DO IROW=SOLIDF(ILAY)%NROW,1,-1
      N=N+1
      IF(ZI(N).NE.SOLIDF(ILAY)%NODATA)THEN
       !## add to a non-cross-sectional location
       IF(PCG(ILAY)%IB(ICOL,IROW).NE.-2)THEN
        FX=SOLID_CALC_FILL_DISTANCE(XI(N),X,NN)
        MH2(ICOL,IROW)=ZI(N); CD2(ICOL,IROW)=COND*FX
       ENDIF
      ENDIF
     ENDDO
    ENDIF
   ENDDO
  
   DEALLOCATE(XI,ZI,X,Z)

   !## get average conductions
   DO IROW=1,SOLIDF(ILAY)%NROW
    DO ICOL=1,SOLIDF(ILAY)%NCOL
     IF(PCG(ILAY)%IB(ICOL,IROW).NE.-2)THEN

      G=CD1(ICOL,IROW)+CD2(ICOL,IROW)
      IF(G.GT.0.0)THEN
       X1=CD1(ICOL,IROW)*MH1(ICOL,IROW)+ &
          CD2(ICOL,IROW)*MH2(ICOL,IROW)
       PCG(ILAY)%COND(ICOL,IROW)=0.5*G
       PCG(ILAY)%HOLD(ICOL,IROW)=X1/G
      ENDIF
      
     ENDIF
    ENDDO
   ENDDO   

   DEALLOCATE(CD1,CD2,MH1,MH2)
 
  ENDDO
   
 ENDIF

 SOLID_CALC_FILL=.TRUE.

 END FUNCTION SOLID_CALC_FILL

 !###====================================================================
 REAL FUNCTION SOLID_CALC_FILL_DISTANCE(XI,X,N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL,INTENT(IN) :: XI
 REAL,DIMENSION(N),INTENT(IN) :: X
 REAL :: TD
 INTEGER :: ID,I
 
 !## conductance is nearest to fixed point
 TD=10.0E10; ID=0
 DO I=1,N
  IF(ABS(XI-X(I)).LT.TD)THEN
   ID=I; TD=ABS(XI-X(I))
  ENDIF
 ENDDO
 
 SOLID_CALC_FILL_DISTANCE=1.0
 IF(ID.GT.0.AND.TD.GT.0.0)SOLID_CALC_FILL_DISTANCE=1.0/TD
 
 END FUNCTION SOLID_CALC_FILL_DISTANCE

 !###====================================================================
 LOGICAL FUNCTION SOLID_CALC_AL(NROW,NCOL)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,OPTIONAL,INTENT(IN) :: NROW,NCOL
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IOS
 INTEGER :: I,NC,NR
 
 SOLID_CALC_AL=.FALSE.

 CALL WINDOWOUTSTATUSBAR(4,'Allocating memory ...')

 IF(ALLOCATED(IOS))DEALLOCATE(IOS); ALLOCATE(IOS(4)) 

 CALL SOLID_CALC_DAL()
 
 ALLOCATE(PCG(NLAY))
 
 DO I=1,SIZE(PCG)
  NULLIFY(PCG(I)%P)
  NULLIFY(PCG(I)%V)
  NULLIFY(PCG(I)%SS)
  NULLIFY(PCG(I)%CD)
  NULLIFY(PCG(I)%CC)
  NULLIFY(PCG(I)%CR)
  NULLIFY(PCG(I)%HNEW)
  NULLIFY(PCG(I)%HCOF)
  NULLIFY(PCG(I)%CV)
  NULLIFY(PCG(I)%RHS)
  NULLIFY(PCG(I)%IB)
  NULLIFY(PCG(I)%HOLD) 
  NULLIFY(PCG(I)%COND) 
 ENDDO
 
 IOS=0

 DO I=1,SIZE(PCG)
  IF(PRESENT(NCOL))THEN
   NC=NCOL
  ELSE
   NC=SOLIDF(I)%NCOL
  ENDIF
  IF(PRESENT(NROW))THEN
   NR=NROW
  ELSE
   NR=SOLIDF(I)%NROW
  ENDIF
  ALLOCATE(PCG(I)%RHS(NC,NR) ,STAT=IOS(1)); PCG(I)%RHS =0.0
  ALLOCATE(PCG(I)%IB(NC,NR)  ,STAT=IOS(2)); PCG(I)%IB  =0
  ALLOCATE(PCG(I)%HOLD(NC,NR),STAT=IOS(3)); PCG(I)%HOLD=0.0
  ALLOCATE(PCG(I)%COND(NC,NR),STAT=IOS(4)); PCG(I)%COND=0.0
 ENDDO
 
 IF(SUM(IOS).NE.0)THEN
  CALL SOLID_CALC_DAL()
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD Can not allocate enough memory to solve this Problem!','Error')
  RETURN
 ENDIF

 CALL WINDOWOUTSTATUSBAR(4,'')

 IF(ALLOCATED(IOS))DEALLOCATE(IOS)

 SOLID_CALC_AL=.TRUE.

 END FUNCTION SOLID_CALC_AL

 !###====================================================================
 LOGICAL FUNCTION SOLID_CALC_AL_PCG(NR,NC)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NC,NR
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IOS

 IF(ALLOCATED(IOS))DEALLOCATE(IOS); ALLOCATE(IOS(9)) 

 ALLOCATE(PCG(1)%P(NC,NR)     ,STAT=IOS(1))
 ALLOCATE(PCG(1)%V(NC,NR)     ,STAT=IOS(2))
 ALLOCATE(PCG(1)%SS(NC,NR)    ,STAT=IOS(3))
 ALLOCATE(PCG(1)%CD(NC,NR)    ,STAT=IOS(4))
 ALLOCATE(PCG(1)%CC(NC,NR)    ,STAT=IOS(5))
 ALLOCATE(PCG(1)%CR(NC,NR)    ,STAT=IOS(6))
 ALLOCATE(PCG(1)%HNEW(NC,NR)  ,STAT=IOS(7))
 ALLOCATE(PCG(1)%HCOF(NC,NR)  ,STAT=IOS(8))
 ALLOCATE(PCG(1)%CV(1,1)      ,STAT=IOS(9))
 
 IF(SUM(IOS).NE.0)THEN
  SOLID_CALC_AL_PCG=.FALSE.
 ELSE
  SOLID_CALC_AL_PCG=.TRUE.
 ENDIF
 
 DEALLOCATE(IOS)
  
 END FUNCTION SOLID_CALC_AL_PCG
 
 !###====================================================================
 SUBROUTINE SOLID_CALC_DAL()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 IF(.NOT.ALLOCATED(PCG))RETURN

 DO I=1,SIZE(PCG)
  IF(ASSOCIATED(PCG(I)%RHS)) DEALLOCATE(PCG(I)%RHS)
  IF(ASSOCIATED(PCG(I)%IB))  DEALLOCATE(PCG(I)%IB)
  IF(ASSOCIATED(PCG(I)%HOLD))DEALLOCATE(PCG(I)%HOLD)
  IF(ASSOCIATED(PCG(I)%COND))DEALLOCATE(PCG(I)%COND)
 ENDDO
 CALL SOLID_CALC_DAL_PCG()

 DEALLOCATE(PCG)

 END SUBROUTINE SOLID_CALC_DAL

 !###====================================================================
 SUBROUTINE SOLID_CALC_DAL_PCG()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 IF(.NOT.ALLOCATED(PCG))RETURN

 DO I=1,SIZE(PCG)
  IF(ASSOCIATED(PCG(I)%P))   DEALLOCATE(PCG(I)%P)
  IF(ASSOCIATED(PCG(I)%V))   DEALLOCATE(PCG(I)%V)
  IF(ASSOCIATED(PCG(I)%SS))  DEALLOCATE(PCG(I)%SS)
  IF(ASSOCIATED(PCG(I)%CD))  DEALLOCATE(PCG(I)%CD)
  IF(ASSOCIATED(PCG(I)%CC))  DEALLOCATE(PCG(I)%CC)
  IF(ASSOCIATED(PCG(I)%CR))  DEALLOCATE(PCG(I)%CR)
  IF(ASSOCIATED(PCG(I)%CV))  DEALLOCATE(PCG(I)%CV)
  IF(ASSOCIATED(PCG(I)%HNEW))DEALLOCATE(PCG(I)%HNEW)
  IF(ASSOCIATED(PCG(I)%HCOF))DEALLOCATE(PCG(I)%HCOF)
 ENDDO

 END SUBROUTINE SOLID_CALC_DAL_PCG
 
 !###====================================================================
 SUBROUTINE SOLID_GET_POINTS(ILAY)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ILAY
 INTEGER :: I,N,IPOS,J

 DO J=1,2
  !## get number of layers to interpolate
  N=0; DO ISPF=1,NSPF; DO I=1,NTBSOL
   IF(I.NE.ILAY)CYCLE
   !## skip lines that do not need to be processed
   IF(SPF(ISPF)%PROF(I)%IACTIVE.EQ.0)CYCLE
   DO IPOS=1,SPF(ISPF)%PROF(I)%NPOS
    N=N+1
    IF(J.EQ.2)THEN
     !## process each pos() 
     CALL SOLID_GETLOCATION(SPF(ISPF)%PROF(I)%PX(IPOS),XD(N),YD(N),SPF(ISPF)%X,SPF(ISPF)%Y)  
     ZD(N)=SPF(ISPF)%PROF(I)%PZ(IPOS)
    ENDIF
   ENDDO
  ENDDO; ENDDO
  IF(J.EQ.1)THEN
   IF(ASSOCIATED(XD))DEALLOCATE(XD)
   IF(ASSOCIATED(YD))DEALLOCATE(YD)
   IF(ASSOCIATED(ZD))DEALLOCATE(ZD)
   ALLOCATE(XD(N),YD(N),ZD(N))
  ENDIF
 ENDDO
 
 END SUBROUTINE SOLID_GET_POINTS

 !###====================================================================
 SUBROUTINE SOLID_EXPORT(ILAY,IEXPORTMODE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ILAY,IEXPORTMODE
 INTEGER :: I,II,N,IPOS,J,K,NCURVE
 INTEGER,DIMENSION(:),ALLOCATABLE :: IU
 REAL :: XP,YP
 CHARACTER(LEN=256) :: LINE

 !## ipf-format
 IF(IEXPORTMODE.EQ.1)THEN
 
  ALLOCATE(IU(1))
  IU(1)=UTL_GETUNIT()
  LINE=TRIM(OUTPUTFOLDER)//'\EXPORT_INTERFACE_'//TRIM(ITOS(ILAY))//'.IPF'
  CALL OSD_OPEN(IU(1),FILE=TRIM(LINE),STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE')
  !## cannot open/write file
  IF(IU(1).LE.0)RETURN
  WRITE(IU(1),'(A)') 'X,Y,Z'
  DO I=1,SIZE(XD); WRITE(IU(1),'(3(F15.7,A1))') XD(I),',',YD(I),',',ZD(I); ENDDO
 
 !## geo-format
 ELSEIF(IEXPORTMODE.EQ.2)THEN
 
  ALLOCATE(IU(NSPF))
  DO ISPF=1,NSPF
   IU(ISPF)=UTL_GETUNIT()
   LINE=TRIM(OUTPUTFOLDER)//'\SOLIDTOOL_CROSSECTION'//TRIM(ITOS(ISPF))//'.GEO'
   CALL OSD_OPEN(IU(ISPF),FILE=TRIM(LINE),STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE')
   WRITE(IU(ISPF),'(A)') 'GEOMETRY FILE FOR THE M-SERIES'
   WRITE(IU(ISPF),'(78A1)') ('=',J=1,78)
   WRITE(IU(ISPF),'(2A)') 'COMPANY   ',' : '
   WRITE(IU(ISPF),'(3A)') 'LICENSE   ',' : ','Unknown'
   CALL IOSDATE(I,J,K)
   WRITE(IU(ISPF),'(2A,2(I2.2,A1),I4.4)') 'DATE      ',' : ',K,'-',J,'-',I
   CALL IOSTIME(I,J,K)
   WRITE(IU(ISPF),'(2A,3(I2.2,A1))') 'TIME      ',' : ',I,':',J,':',K
   LINE=TRIM(FNAME)//'\SOLIDTOOL_CROSSECTION'//TRIM(ITOS(ISPF))//'.GEO'
   WRITE(IU(ISPF),'(3A)') 'FILENAME  ',' : ',TRIM(LINE)
   WRITE(IU(ISPF),'(A)') 'CREATED BY : '//TRIM(UTL_IMODVERSION(S1='_',S2='.'))
   WRITE(IU(ISPF),'(78A1)') ('=',J=1,78)
   WRITE(IU(ISPF),'(A)') '[TITLES]'
   WRITE(IU(ISPF),'(A/)') '[END OF TITLES]'
   WRITE(IU(ISPF),'(A)') '[EXTRA TITLES]'
   WRITE(IU(ISPF),'(A/)') '[END OF EXTRA TITLES]'
   WRITE(IU(ISPF),'(A)') '[ACCURACY]'
   WRITE(IU(ISPF),'(F14.4)') 0.001
   WRITE(IU(ISPF),'(A/)') '[END OF ACCURACY]'
   WRITE(IU(ISPF),'(A)') '[POINTS]'
   !## get number of layers to interpolate
   N=0; DO I=1,NTBSOL; N=N+SPF(ISPF)%PROF(I)%NPOS; ENDDO
   WRITE(IU(ISPF),'(I7,A)') N,'  - Number of geometry points -'
  ENDDO
 
  !## write points on interface
  N=0; DO ISPF=1,NSPF
   NCURVE=0
   !## number of layers to interpolate
   DO I=1,NTBSOL
    IF(SPF(ISPF)%PROF(I)%NPOS.LE.0)CYCLE
    !## skip lines that do not need to be processed
    IF(SPF(ISPF)%PROF(I)%IACTIVE.EQ.0)CYCLE
    !## process each pos() 
    DO IPOS=1,SPF(ISPF)%PROF(I)%NPOS
     CALL SOLID_GETLOCATION(SPF(ISPF)%PROF(I)%PX(IPOS),XP,YP,SPF(ISPF)%X,SPF(ISPF)%Y)  
     N=N+1; NCURVE=NCURVE+1
     WRITE(IU(ISPF),'(I8,3(1X,F14.3))') N,SPF(ISPF)%PROF(I)%PX(IPOS),SPF(ISPF)%PROF(I)%PZ(IPOS),0.0
    END DO
    NCURVE=NCURVE-1
   END DO

  ENDDO
  
  DO ISPF=1,NSPF
   WRITE(IU(ISPF),'(A/)') '[END OF POINTS]'
   WRITE(IU(ISPF),'(A)') '[CURVES]'
   NCURVE=0
   DO I=1,NTBSOL 
    IF(SPF(ISPF)%PROF(I)%NPOS.LE.0)CYCLE
    NCURVE=NCURVE+(SPF(ISPF)%PROF(I)%NPOS-1)
   ENDDO
   WRITE(IU(ISPF),'(I4,A)') NCURVE,' - Number of curves -'
   NCURVE=0; N=0
   DO I=1,NTBSOL 
    IF(SPF(ISPF)%PROF(I)%NPOS.LE.0)CYCLE
    DO IPOS=1,SPF(ISPF)%PROF(I)%NPOS-1
     NCURVE=NCURVE+1; N=N+1
     WRITE(IU(ISPF),'(I6,A)') N,' - Curve number'
     WRITE(IU(ISPF),'(I8,A)') 2,' - Number of points on curve,  next line(s) are pointnumbers'
     WRITE(IU(ISPF),'(4X,2I6)') NCURVE,NCURVE+1
    ENDDO
    NCURVE=NCURVE+1
   ENDDO
   WRITE(IU(ISPF),'(A/)') '[END OF CURVES]'
   WRITE(IU(ISPF),'(A)') '[BOUNDARIES]'
   
   WRITE(IU(ISPF),'(I4,A)') NTBSOL,' - Number of boundaries -'

   II=0
   DO I=NTBSOL,1,-1
    II=II+1
    WRITE(IU(ISPF),'(I6,A)') II-1,' - Boundary number'
    IF(SPF(ISPF)%PROF(I)%NPOS.LE.0)THEN
     WRITE(IU(ISPF),'(I8,A)') 0,' - Number of curves on boundary, next lines(s) are curvenumbers'
     CYCLE
    ENDIF
    WRITE(IU(ISPF),'(I8,A)') SPF(ISPF)%PROF(I)%NPOS-1,' - Number of curves on boundary, next lines(s) are curvenumbers'
    !## get the right curve-numbers - reverse order
    NCURVE=0
    DO J=1,I-1
     NCURVE=NCURVE+SPF(ISPF)%PROF(J)%NPOS-1
    ENDDO
    WRITE(IU(ISPF),'(4X,99I6)') (J,J=NCURVE+1,NCURVE+SPF(ISPF)%PROF(I)%NPOS-1)
   END DO
   WRITE(IU(ISPF),'(A/)') '[END OF BOUNDARIES]'

!   WRITE(IU(ISPF),'(A)') '[STDV BOUNDARIES]'
!   WRITE(IU(ISPF),'(A/)') '[END OF STDV BOUNDARIES]'
   WRITE(IU(ISPF),'(A)') '[PIEZO LINES]'
   WRITE(IU(ISPF),'(A)') '   0 - Number of piezometric level lines -'
   WRITE(IU(ISPF),'(A/)') '[END OF PIEZO LINES]'
   WRITE(IU(ISPF),'(A)') '[PHREATIC LINE]'
   WRITE(IU(ISPF),'(A)') '   0 - Number of the piezometric level line acting as phreatic line -'
   WRITE(IU(ISPF),'(A/)') '[END OF PHREATIC LINE]'
   WRITE(IU(ISPF),'(A)') '[WORLD CO-ORDINATES]'
   WRITE(IU(ISPF),'(A)') '    0.000 - X world 1 -'
   WRITE(IU(ISPF),'(A)') '    0.000 - Y world 1 -'
   WRITE(IU(ISPF),'(A)') '    0.000 - X world 2 -'
   WRITE(IU(ISPF),'(A)') '    0.000 - Y world 2 -'
   WRITE(IU(ISPF),'(A/)') '[END OF WORLD CO-ORDINATES]'
   WRITE(IU(ISPF),'(A)') '[LAYERS]'
  
   WRITE(IU(ISPF),'(A)') '  '//TRIM(ITOS(NTBSOL-1))//' - Number of layers -'

   II=0
   DO I=NTBSOL-1,1,-1
    II=II+1
    
    LINE='   '//TRIM(ITOS(II))//' - Layer number, next line is material of layer'
    WRITE(IU(ISPF),'(A)') TRIM(LINE)
    WRITE(IU(ISPF),'(A)') '     '//TRIM(SPF(ISPF)%PROF(I)%LNAME)
    WRITE(IU(ISPF),'(A)') '     0 - Piezometric level line at top of layer'
    WRITE(IU(ISPF),'(A)') '     0 - Piezometric level line at bottom of layer'
    LINE='     '//TRIM(ITOS(I))//' - Boundarynumber at top of layer'
    WRITE(IU(ISPF),'(A)') TRIM(LINE)
    LINE='     '//TRIM(ITOS(I-1))//' - Boundarynumber at bottom of layer'
    WRITE(IU(ISPF),'(A)') TRIM(LINE)

   ENDDO

   WRITE(IU(ISPF),'(A/)') '[END OF LAYERS]'
!   WRITE(IU(ISPF),'(A)') '[LAYERLOADS]'
!   WRITE(IU(ISPF),'(A/)') ' - Layers which are loads'
!   WRITE(IU(ISPF),'(A/)') '[END OF LAYERLOADS]'
   WRITE(IU(ISPF),'(A)') 'END OF GEOMETRY FILE'
  ENDDO
 ENDIF
 
 IF(ALLOCATED(IU))THEN; DO I=1,SIZE(IU); CLOSE(IU(I)); ENDDO; DEALLOCATE(IU); ENDIF
 
 END SUBROUTINE SOLID_EXPORT
 
 !###====================================================================
 SUBROUTINE SOLID_GETLOCATION(XPOS,XP,YP,X,Y)
 !###====================================================================
 !## based upon the value of xpos, return x,y coordinates
 IMPLICIT NONE
 REAL,INTENT(IN) :: XPOS
 REAL,INTENT(OUT) :: XP,YP
 REAL,DIMENSION(:),INTENT(IN) :: X,Y
 REAL :: D,TD,RATIO
 INTEGER :: I

 TD=0.0
 DO I=2,SIZE(X)
  D =SQRT((X(I)-X(I-1))**2.0+(Y(I)-Y(I-1))**2.0)
  TD=TD+D
  !## inside current segment
  IF(TD.GE.XPOS)THEN
   RATIO=(D-(TD-XPOS))/D
   IF(RATIO.GT.0.0)THEN
    XP=X(I-1)+RATIO*(X(I)-X(I-1))
    YP=Y(I-1)+RATIO*(Y(I)-Y(I-1))
   ELSE
    XP=X(I-1)
    YP=Y(I-1)
   ENDIF
   EXIT
  ENDIF
 END DO
 
 !## still within current cross-section, take last coordinate, this is due to numerical precision
 IF(TD.LT.XPOS)THEN
  I=SIZE(X)
  XP=X(I)
  YP=Y(I)
 ENDIF
 
 END SUBROUTINE SOLID_GETLOCATION
 
 !###====================================================================
 SUBROUTINE SOLID_CHECKIBND(ILAY,NROW,NCOL)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ILAY,NROW,NCOL
 INTEGER(KIND=1),POINTER,DIMENSION(:) :: ISPEC
 INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:) :: Y
 INTEGER(KIND=2),POINTER,DIMENSION(:,:) :: THREAD,YSEL
 INTEGER :: IROW,ICOL,K,IP,IR,IC,N,I,MAXTHREAD,MAXN
 LOGICAL :: LCNST

! WRITE(*,*) 'Checking Boundary Conditions' 

 DO IROW=1,NROW; DO ICOL=1,NCOL 

  IF(PCG(ILAY)%IB(ICOL,IROW).EQ.0)PCG(ILAY)%HOLD(ICOL,IROW)=HNOFLOW
  !## adjust constant head whenever no active cell is attached to it
  IF(PCG(ILAY)%IB(ICOL,IROW).LT.0)THEN
   K=0
   IF(ICOL.GT.1)THEN; IF(PCG(ILAY)%IB(ICOL-1,IROW).NE.0)K=1; ENDIF
   IF(ICOL.LT.NCOL)THEN; IF(PCG(ILAY)%IB(ICOL+1,IROW).NE.0)K=1; ENDIF
   IF(IROW.GT.1)THEN; IF(PCG(ILAY)%IB(ICOL,IROW-1).NE.0)K=1; ENDIF
   IF(IROW.LT.NROW)THEN; IF(PCG(ILAY)%IB(ICOL,IROW+1).NE.0)K=1; ENDIF
   IF(K.EQ.0)THEN; PCG(ILAY)%IB(ICOL,IROW)=0; PCG(ILAY)%HOLD(ICOL,IROW)=HNOFLOW; ENDIF
  ENDIF

 ENDDO; ENDDO

 !## find active cells not in direct (2d/3d) relation to constant head cell!
 IF(ASSOCIATED(ISPEC))DEALLOCATE(ISPEC); IF(ASSOCIATED(THREAD))DEALLOCATE(THREAD)
 IF(ALLOCATED(Y))DEALLOCATE(Y); IF(ASSOCIATED(YSEL))DEALLOCATE(YSEL)
 ALLOCATE(Y(NCOL,NROW))
 MAXTHREAD=1000; MAXN=MAXTHREAD; ALLOCATE(ISPEC(MAXTHREAD),THREAD(2,MAXTHREAD),YSEL(2,MAXTHREAD))

 IP=0
 Y =0

 DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(Y(ICOL,IROW).EQ.0.AND.PCG(ILAY)%IB(ICOL,IROW).GT.0)THEN
   IP=1
   CALL SOLID_TRACE(IROW,ICOL,PCG(ILAY)%IB,PCG(1)%CC,PCG(1)%CR,Y,THREAD,ISPEC,YSEL,N,NROW, &
                    NCOL,IP,LCNST,MAXTHREAD,MAXN)
   !## no constant head attached, remove group of cells
   IF(.NOT.LCNST)THEN
    DO I=1,N
     IC=YSEL(1,I); IR=YSEL(2,I)
     PCG(ILAY)%IB(IC,IR)=0 
    ENDDO
    WRITE(*,*) 'Removed ',N,' of modelcells that are not attached to a constant head'
   ELSE
    WRITE(*,*) 'Nothing removed' 
   ENDIF
  ENDIF
 ENDDO; ENDDO

 DEALLOCATE(ISPEC,THREAD,Y,YSEL)

 END SUBROUTINE SOLID_CHECKIBND

 !###======================================================================
 SUBROUTINE SOLID_TRACE(IROW,ICOL,IB,CC,CR,Y,THREAD,ISPEC,YSEL,N,NROW,NCOL,IP,LCNST,MAXTHREAD,MAXN)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN),DIMENSION(NCOL,NROW) :: CC,CR
 LOGICAL,INTENT(OUT) :: LCNST
 INTEGER,INTENT(IN) :: NCOL,NROW,IROW,ICOL
 INTEGER,INTENT(INOUT) :: IP,MAXTHREAD,MAXN
 INTEGER,INTENT(OUT) :: N
 INTEGER :: NTHREAD,IR,IC,IDIR,JROW,JCOL,N1,N2,N3,M1,M2
 INTEGER,INTENT(IN),DIMENSION(NCOL,NROW) :: IB
 INTEGER(KIND=1),POINTER,DIMENSION(:) :: ISPEC,ISPEC_BU
 INTEGER(KIND=1),DIMENSION(NCOL,NROW) :: Y
 INTEGER(KIND=2),POINTER,DIMENSION(:,:) :: THREAD,YSEL,YSEL_BU,THREAD_BU
 LOGICAL :: LEX

 LCNST=.FALSE.

 !## copy current location
 JCOL             =ICOL
 JROW             =IROW

 !## define first point in thread
 NTHREAD          =1
 THREAD(1,NTHREAD)=JCOL
 THREAD(2,NTHREAD)=JROW
 ISPEC(NTHREAD)   =0
 Y(JCOL,JROW)     =IP

 N=1
 YSEL(1,N)=JCOL
 YSEL(2,N)=JROW

 DO WHILE(NTHREAD.GT.0)

  !## get row/column number current location in thread
  JCOL=THREAD(1,NTHREAD)
  JROW=THREAD(2,NTHREAD)

  !## get direction and do not use this direction again!
  IDIR          =ISPEC(NTHREAD)+1
  ISPEC(NTHREAD)=IDIR
  CALL SOLID_GETDIR(JCOL,JROW,IR,IC,IDIR)

  !## possible direction found
  IF(IDIR.LE.4)THEN

   !## existing location in grid
   IF(IR.GE.1.AND.IR.LE.NROW.AND. &
      IC.GE.1.AND.IC.LE.NCOL)THEN

    IF(Y(IC,IR).EQ.0.AND. &         !## not yet been there
       IB(IC,IR).NE.0)THEN          !## correct location

     !## check whether cc/cr/cv are not zero
     LEX=.TRUE.
     SELECT CASE (IDIR)
      CASE (1); IF(CC(IC,IR).LE.0.0)LEX=.FALSE. !## north
      CASE (2); IF(CR(IC-1,IR).LE.0.0)LEX=.FALSE.        !## east
      CASE (3); IF(CC(IC,IR-1).LE.0.0)LEX=.FALSE.        !## south
      CASE (4); IF(CR(IC,IR).LE.0.0)LEX=.FALSE. !## west
     END SELECT

     IF(LEX)THEN
      IF(IB(IC,IR).LT.0)LCNST=.TRUE.
      Y(IC,IR)      =IP
      NTHREAD       =NTHREAD+1
     
      IF(NTHREAD+1.GT.MAXTHREAD)THEN
       N1=SIZE(THREAD,1); N2=SIZE(THREAD,2); N3=SIZE(ISPEC,1)
       MAXTHREAD=MIN(NROW*NCOL,2*MAXTHREAD)
       ALLOCATE(THREAD_BU(N1,MAXTHREAD),ISPEC_BU(MAXTHREAD))
       THREAD_BU(1:N1,1:N2)=THREAD(1:N1,1:N2); ISPEC_BU(1:N3)=ISPEC(1:N3)
       DEALLOCATE(THREAD,ISPEC)
       THREAD=>THREAD_BU; ISPEC=>ISPEC_BU
      ENDIF

      THREAD(1,NTHREAD)=IC
      THREAD(2,NTHREAD)=IR
      ISPEC(NTHREAD)   =0
      !## correct places visited
      N=N+1
     
      IF(N+1.GT.MAXN)THEN
       M1=SIZE(YSEL,1); M2=SIZE(YSEL,2)
       MAXN=MIN(NROW*NCOL,2*MAXN)
       ALLOCATE(YSEL_BU(M1,MAXN)); YSEL_BU(1:M1,1:M2)=YSEL(1:M1,1:M2)
       DEALLOCATE(YSEL); YSEL=>YSEL_BU
      ENDIF

      YSEL(1,N)=IC
      YSEL(2,N)=IR

     ENDIF
    
    ENDIF
   ENDIF

  ELSE
   !## no more places to go, move one step backwards in thread
   NTHREAD=NTHREAD-1
  ENDIF
 END DO

 END SUBROUTINE SOLID_TRACE

 !###====================================================
 SUBROUTINE SOLID_GETDIR(JCOL,JROW,IR,IC,IDIR)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN)     :: JCOL,JROW
 INTEGER,INTENT(OUT)    :: IC,IR
 INTEGER,INTENT(IN OUT) :: IDIR

 !##get new direction to search in
 IC=JCOL
 IR=JROW
 IF(IDIR.EQ.1)IR=IR-1
 IF(IDIR.EQ.2)IC=IC+1
 IF(IDIR.EQ.3)IR=IR+1
 IF(IDIR.EQ.4)IC=IC-1

 END SUBROUTINE SOLID_GETDIR
  
END MODULE MOD_SOLID_PCG