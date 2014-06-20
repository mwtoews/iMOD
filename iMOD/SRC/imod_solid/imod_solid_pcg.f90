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
MODULE MOD_SOLID_PCG

USE WINTERACTER
USE RESOURCE
USE MOD_PCG, ONLY : PCG2AP,PCGSETTINGS
USE MOD_SOLID_PAR
USE MOD_UTL, ONLY : ITOS,UTL_INSIDEPOLYGON,NEWLINE,UTL_CREATEDIR,UTL_IDFSNAPTOGRID
USE MOD_IDF, ONLY : IDFREAD,IDFEQUAL,IDFREADSCALE,IDFWRITE,IDFALLOCATEX,IDFCOPY,IDFIROWICOL,IDFGETLOC,IDFFILLCOMMENT
USE MOD_POLYGON_PAR
USE MOD_INTERSECT_PAR, ONLY : XA,YA,LN
USE MOD_SOLID_UTL
USE MOD_HFB, ONLY : HFB_RP,HFB_CALC,HFB_DAL
USE MOD_KRIGING, ONLY : KRIGINGSETTINGS,KRIGING_VARIOGRAM,KRIGING_MAIN
USE MOD_INTERSECT, ONLY : INTERSECT_EQUI,INTERSECT_NONEQUI,INTERSECT_DEALLOCATE
USE MOD_POLINT, ONLY : POL1INTMAIN

REAL,ALLOCATABLE,DIMENSION(:,:,:),PRIVATE :: RHS,CC,CR,CV,P,V,SS,CD,HCOF
REAL,ALLOCATABLE,DIMENSION(:,:,:),PRIVATE :: HOLD
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:),PRIVATE :: HNEW
INTEGER,ALLOCATABLE,DIMENSION(:,:,:),PRIVATE :: IB
INTEGER,PRIVATE :: NROW,NCOL,NLAY

INTEGER,PRIVATE :: MINP=10,MAXP=50,KTYPE=2,IOFFSET=10,NOSEARCH=1,IEXPVARIOGRAM=0
REAL,PRIVATE :: RANGE=5000.0,SILL=30.0,NUGGET=0.0,SEARCHDISTANCE=7500.0
INTEGER :: IKRIGING=0
LOGICAL :: LSEMIVARIOGRAM
REAL,DIMENSION(:),ALLOCATABLE,PRIVATE :: XD,YD,ZD

CONTAINS

 !###======================================================================
 SUBROUTINE SOLID_CALC()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,ITER1,ITER2,ILAY,IROW,ICOL,ITYPE,IL,IECHO,IVERSION,NICNVG, &
     JKRIGING,ICOMP,NCOMP,NPR,NPC !,N
 INTEGER,ALLOCATABLE,DIMENSION(:) :: ICNVG
 REAL :: TOP,BOT,C,HCHG,HCHGOLD,S
 LOGICAL :: LEX
 CHARACTER(LEN=256) :: FNAME
 CHARACTER(LEN=10) :: TXT
 CHARACTER(LEN=1) :: YN
 TYPE(WIN_MESSAGE) :: MESSAGE
 TYPE(IDFOBJ),DIMENSION(2) :: IDFK
 REAL,DIMENSION(:),ALLOCATABLE :: DELR,DELC
 REAL,DIMENSION(:,:),ALLOCATABLE :: XPINT
  
 IF(IBATCH.EQ.0)IECHO=1; IF(IBATCH.EQ.1)IECHO=-2

 DO I=1,SIZE(IDFK); CALL IDFNULLIFY(IDFK(I)); ENDDO
 
 !## read all idf-files in to memory
 IF(IBATCH.EQ.0)THEN
  !## refresh memory
  CALL SOLID_DEALLOCATE()
  FNAME=GETSOLNAME()
  IF(.NOT.SOLIDOPENSOL('R',FNAME))RETURN
 ELSE
  NMASK=SLD(1)%NLAY*2
  IF(ALLOCATED(MASK))THEN; DO I=1,SIZE(MASK); CALL IDFDEALLOCATEX(MASK(I)%IDF); ENDDO; DEALLOCATE(MASK); ENDIF
  ALLOCATE(MASK(NMASK))
  DO I=1,NMASK; MASK(I)%FNAME=TRIM(OUTPUTFOLDER)//'\MASK\MASK_L'//TRIM(ITOS(I))//'.IDF'; ENDDO
 ENDIF
 
 !## get options for soluting the model
 IF(IBATCH.EQ.0)THEN
  IF(.NOT.SOLID_CALC_INIT(IVERSION))RETURN
  !## overwrite results
  IF(IVERSION.EQ.0)THEN
   OUTPUTFOLDER=FNAME(:INDEX(FNAME,'\',.TRUE.)-1)
  ELSE
   OUTPUTFOLDER=FNAME(:INDEX(FNAME,'\',.TRUE.)-1)//'\version'//TRIM(ITOS(IVERSION))
  ENDIF
 ENDIF
 
 !## read top/bottom idf files
 IF(.NOT.SOLID_READIDF())RETURN
 !## allocate memory
 IF(.NOT.SOLID_CALC_AL())RETURN
 
 ALLOCATE(ICNVG(NLAY),ISEL_IDF(NLAY),ICHECK_IDF(NLAY))
 IF(.NOT.ALLOCATED(DZ))THEN
  ALLOCATE(DZ(SLD(1)%NLAY)); DZ=0.0
 ENDIF
 
 I=0
 DO ILAY=1,NLAY
  J=2
  IF(MOD(ILAY,2).NE.0)THEN
   I=I+1; J=1
  ENDIF
  ISEL_IDF(ILAY)  =SLD(1)%ICLC(I,J)
  ICHECK_IDF(ILAY)=SLD(1)%ICHECK(I,J)
 ENDDO

 !## process all idf's (starting heads) + crosssections (fixed heads/ghb-heads)
 IF(.NOT.SOLID_CALC_FILL())RETURN
 
 IF(IKRIGING.EQ.1)THEN
  !## ibound=-1 actual borehole, ibound=-2 cross-section, ibound
  IF(ITIGHT.EQ.2)THEN
   DO ILAY=1,NLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
    IF(IB(ICOL,IROW,ILAY).EQ.-2)IB(ICOL,IROW,ILAY)=3
   ENDDO; ENDDO; ENDDO 
  ENDIF
 ENDIF
 
 !## do not compute hypothetical borders (lowest one)
 IF(NSPF.EQ.0)THEN
  ISEL_IDF(1)=0; ISEL_IDF(NLAY)=0; DO ILAY=1,NLAY-1,2; ISEL_IDF(ILAY)=0 ; ENDDO
 ENDIF
 
 !## include horizontal flow barriers
 IF(.NOT.HFB_RP(MDLIDF))THEN; ENDIF

 !## overall resistance
 C=100.0 !## day
 
 IF(IBATCH.EQ.0)CALL UTL_MESSAGEHANDLE(0)

 !## export current dataset prior to interpolation
 IF(IKRIGING.EQ.4)THEN 
  CALL UTL_CREATEDIR(TRIM(OUTPUTFOLDER)//'\EXPORT')
  CALL SOLIDEXPORT(  TRIM(OUTPUTFOLDER)//'\EXPORT',0)
  IF(.NOT.IDFALLOCATEX(SOLIDF(1)))RETURN
  DO ILAY=1,NLAY
   IF(ISEL_IDF(ILAY).EQ.1)THEN
    SOLIDF(1)%X(:,:)=REAL(IB(:,:,ILAY))
    IF(.NOT.IDFWRITE(SOLIDF(1),TRIM(OUTPUTFOLDER)//'\EXPORT\INT_L'//TRIM(ITOS(ILAY))//'_IB.IDF',1))THEN; ENDIF; CLOSE(SOLIDF(1)%IU)
    SOLIDF(1)%X(:,:)=HOLD(:,:,ILAY)
    IF(.NOT.IDFWRITE(SOLIDF(1),TRIM(OUTPUTFOLDER)//'\EXPORT\INT_L'//TRIM(ITOS(ILAY))//'_ZV.IDF',1))THEN; ENDIF; CLOSE(SOLIDF(1)%IU)
   ENDIF
   ISEL_IDF(ILAY)=0
  ENDDO
 ENDIF

 !## solve each system per modellayer
 DO ILAY=1,NLAY
  IF(ISEL_IDF(ILAY).EQ.1)THEN
  
   !## solve system
   IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(3,'Comp. Elevation: '//TRIM(ITOS(ILAY)))
   IF(IBATCH.EQ.1)WRITE(*,'(A,I10,A,F17.5)') 'Computing Elevation: ',ILAY, ' hclose=',HCLOSE

   IF(IKRIGING.EQ.1)THEN

    !## fill top as constant head whenever ibound=2
    DO IROW=1,NROW; DO ICOL=1,NCOL
     IF(IB(ICOL,IROW,ILAY).EQ.2)THEN
      IB(ICOL,IROW,ILAY)=-2; HOLD(ICOL,IROW,ILAY)=HOLD(ICOL,IROW,ILAY-1)
     ENDIF
    ENDDO; ENDDO

    IF(MAXVAL(IB(:,:,ILAY)).GT.0)THEN

     !## reset isotropic
     CR=(MDLIDF%DX*MDLIDF%DY)/C; CC=(MDLIDF%DX*MDLIDF%DY)/C
     !## include horizontal flow barriers
     CALL HFB_CALC(MDLIDF,CR(:,:,1),CC(:,:,1),ILAY)
     !## check for isolated active cell that can not be solved
     CALL SOLID_CHECKIBND(ILAY)

     NICNVG=0; HNEW(:,:,1)=HOLD(:,:,ILAY)

     DO ITER1=1,MXITER1
      IF(IBATCH.EQ.0)CALL WMESSAGEPEEK(ITYPE,MESSAGE)
      CALL SOLID_CALC_CONSTRAINS(ILAY,MDLIDF%DX*MDLIDF%DY)
      CALL PCG2AP(NROW*NCOL,NROW,NCOL,1,IB(:,:,ILAY),CR,CC,CV,HCOF,RHS(:,:,1),V,SS,P, &
                  CD,HNEW(:,:,1),MXITER1,MXITER2,ITER1,ITER2,ICNVG(ILAY),HCLOSE,RCLOSE,IECHO,NICNVG,RELAX,HCHG)
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
    
     !## copy solution into hold
     HOLD(:,:,ILAY)=HNEW(:,:,1)
    ENDIF  
    
    !## copy hold(ilay)->hold(ilay+1)
    IF(NSPF.EQ.0)THEN
     DO IROW=1,NROW; DO ICOL=1,NCOL
      IF(IB(ICOL,IROW,ILAY).GT.0)HOLD(ICOL,IROW,ILAY+1)=HOLD(ICOL,IROW,ILAY)
     ENDDO; ENDDO
    ENDIF

   !## do not take too much points along cross-sections for the computation of a semivariogram solely
   ELSEIF(IKRIGING.EQ.2.OR.IKRIGING.EQ.3)THEN
   
    CALL SOLIDEXPORT(TRIM(OUTPUTFOLDER)//'\EXPORT',ILAY)

    CALL IDFCOPY(MDLIDF,IDFK(1)); CALL IDFCOPY(MDLIDF,IDFK(2))
    IF(IDFALLOCATEX(IDFK(1)).AND.IDFALLOCATEX(IDFK(2)))THEN
     IDFK(1)%X=IDFK(1)%NODATA; IDFK(2)%X=IDFK(2)%NODATA
 
     IDFK(1)%X=IDFK(1)%NODATA; IF(IKRIGING.EQ.2)IOFFSET=1
!     DO IROW=1,NROW; DO ICOL=1,NCOL; IF(IB(ICOL,IROW,ILAY).EQ.0)IDFK(1)%X(ICOL,IROW)=HNOFLOW+1.0; ENDDO; ENDDO
     CALL KRIGING_MAIN(SIZE(XD),XD,YD,ZD,IDFK(1),IDFK(2),MINP,MAXP,RANGE,SILL,NUGGET,KTYPE,IOFFSET,SEARCHDISTANCE, &
         NOSEARCH,IEXPVARIOGRAM,0,0.0,IBATCH)   
!     DO IROW=1,NROW; DO ICOL=1,NCOL; IF(IB(ICOL,IROW,ILAY).EQ.0)IDFK(1)%X(ICOL,IROW)=HNOFLOW; ENDDO; ENDDO

     IF(IKRIGING.EQ.3)THEN
      NPR=0; DO IROW=1,NROW,IOFFSET; NPR=NPR+1; ENDDO
      NPC=0; DO ICOL=1,NCOL,IOFFSET; NPC=NPC+1; ENDDO
      DEALLOCATE(XD,YD)
      ALLOCATE(XPINT(NPC,NPR),XD(NPC),YD(NPR),DELR(0:NCOL),DELC(0:NROW))
      NPR=0; DO IROW=1,NROW,IOFFSET; NPR=NPR+1; YD(NPR)=MDLIDF%YMAX-MDLIDF%DY*REAL(IROW-0.5)
       NPC=0; DO ICOL=1,NCOL,IOFFSET; NPC=NPC+1; XD(NPC)=MDLIDF%XMIN+MDLIDF%DX*REAL(ICOL-0.5)
       XPINT(NPC,NPR)=HOLD(ICOL,IROW,ILAY)
      ENDDO; ENDDO
      DELC(0)=MDLIDF%YMAX; DO IROW=1,NROW; DELC(IROW)=DELC(IROW-1)-MDLIDF%DY; ENDDO
      DELR(0)=MDLIDF%XMIN; DO ICOL=1,NCOL; DELR(ICOL)=DELR(ICOL-1)+MDLIDF%DX; ENDDO
      CALL POL1INTMAIN(NCOL,NROW,NPC,NPR,XD,YD,XPINT,DELR,DELC,HOLD(:,:,ILAY),4,IDFK(1)%NODATA)
      DEALLOCATE(XD,YD,XPINT,DELR,DELC)
     ELSE
      !## copy solution into hold
      HOLD(:,:,ILAY)=IDFK(1)%X(:,:)
     ENDIF      
     ICNVG(ILAY)=1

    ELSE
     CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'iMOD can not allocate memory for storage of IDF-files.','Error')
     EXIT
    ENDIF
    IF(ALLOCATED(XD))DEALLOCATE(XD)
    IF(ALLOCATED(YD))DEALLOCATE(YD)
    IF(ALLOCATED(ZD))DEALLOCATE(ZD)

   !## semivariogram
   ELSEIF(IKRIGING.EQ.5)THEN
    CALL SOLIDEXPORT(TRIM(OUTPUTFOLDER)//'\EXPORT',ILAY)
    CALL KRIGING_VARIOGRAM(SIZE(XD),XD,YD,ZD,I,IDFK(1),IBATCH=0)
    ICNVG(ILAY)=1
   ENDIF
    
  ELSE
   ICNVG(ILAY)   = 1
   ISEL_IDF(ILAY)= 1
   IB(:,:,ILAY)  =-3 !## whole modellayer inactive (constant head)
  ENDIF
 ENDDO

 IF(IBATCH.EQ.0)THEN; CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,''); CALL WINDOWOUTSTATUSBAR(3,''); ENDIF

 LEX=.FALSE.
 IF(SUM(ICNVG).EQ.SUM(ISEL_IDF))THEN
  LEX=.TRUE.
 ELSE
  IF(IBATCH.EQ.0)THEN
   CALL UTL_MESSAGEHANDLE(1) 
   CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Part of Solid did not converge.'//CHAR(13)// &
     'Do you want to save the results so far in:'//CHAR(13)// &
      '['//TRIM(FNAME)//']','Question')
   IF(WINFODIALOG(4).EQ.1)LEX=.TRUE.
  ELSEIF(IBATCH.EQ.1)THEN
   WRITE(*,'(A)') 'Part of Solid did not converge. Do you want to save the results so far in:'
   WRITE(*,'(A$)') '['//TRIM(FNAME)//'] (Y/N) ? '
   READ(*,*) YN; LEX=UTL_CAP(YN,'U').EQ.'Y'
  ENDIF
 ENDIF
 DEALLOCATE(ICNVG)
 IF(IKRIGING.EQ.4)LEX=.FALSE.
 
 IF(LEX)THEN !.AND.IKRIGING.NE.4)THEN
  !## write results
  IF(IDFALLOCATEX(MDLIDF))THEN

   !## make corrections top/bottom
   DO IROW=1,NROW
    DO ICOL=1,NCOL

     !## boundary=-1 clay
     !## make sure top/bot are equal outside boundary=1 (hypothethical) or boundary=-2 (cross-section)
     IF(NSPF.EQ.0)THEN
      DO ILAY=2,NLAY-1,2
       IF(IB(ICOL,IROW,ILAY).EQ.1.OR.IB(ICOL,IROW,ILAY).EQ.-2)HOLD(ICOL,IROW,ILAY+1)=HOLD(ICOL,IROW,ILAY)
      ENDDO
     ELSE
!     !## fill top as constant head whenever ibound=2
!      DO ILAY=2,NLAY-1,2
!       IF(IB(ICOL,IROW,ILAY).EQ.-2)THEN
!        HOLD(ICOL,IROW,ILAY)=HOLD(ICOL,IROW,ILAY-1)
!       ENDIF
!      ENDDO
     ENDIF
           
     !## make sure lowest bot is lower than upper top
     IF(ICHECK_IDF(NLAY).EQ.1)THEN
      HOLD(ICOL,IROW,NLAY)=MIN(HOLD(ICOL,IROW,1),HOLD(ICOL,IROW,NLAY))
     ENDIF

     DO ILAY=2,NLAY

      !## check only whenever icheck is active
      IF(ICHECK_IDF(ILAY).EQ.0)CYCLE
      
      !## get upper boundary
      TOP=HOLD(ICOL,IROW,ILAY-1)
      !## get lower boundary in case constant head
      BOT=HOLD(ICOL,IROW,NLAY)
      !## try earlier boundary
      DO IL=ILAY+1,NLAY
       IF(IB(ICOL,IROW,IL).LT.0)THEN
        BOT=MAX(BOT,HOLD(ICOL,IROW,IL))
        EXIT
       ENDIF
      ENDDO

      HOLD(ICOL,IROW,ILAY)=MAX(HOLD(ICOL,IROW,ILAY),BOT)
      HOLD(ICOL,IROW,ILAY)=MIN(HOLD(ICOL,IROW,ILAY),TOP)

     ENDDO

    ENDDO
   ENDDO

   DO I=1,NLAY
    MDLIDF%X(:,:)=HOLD(:,:,I)
    !## get solved interpolation field
    CLOSE(MDLIDF%IU)
    ILAY=I/2;        IF(MOD(I,2).NE.0)ILAY=ILAY+1
    TXT='BOT Layer'; IF(MOD(I,2).EQ.1)TXT='TOP Layer'
    CALL IDFFILLCOMMENT(MDLIDF,'Created by SolidTool'//NEWLINE//'Units: m+MSL'//NEWLINE// &
                               TRIM(TXT)//' '//TRIM(ITOS(ILAY)))
    MDLIDF%NODATA=HNOFLOW
    FNAME=TRIM(OUTPUTFOLDER)//'\'//TRIM(SOLIDF(I)%FNAME(INDEX(SOLIDF(I)%FNAME,'\',.TRUE.)+1:))
    IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Writing '//TRIM(FNAME))
    IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Writing '//TRIM(FNAME)
    IF(.NOT.IDFWRITE(MDLIDF,FNAME,1))THEN
     IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not write IDF file:'//CHAR(13)//TRIM(FNAME),'Error')
     IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Can not write IDF file:'//CHAR(13)//TRIM(FNAME)
     EXIT
    ENDIF
   END DO

  ENDIF
  CALL WINDOWOUTSTATUSBAR(4,'')
 ENDIF

 !## free memory for pcg-computation
 CALL SOLID_CALC_DAL()
 CALL HFB_DAL()
 
 CALL IDFDEALLOCATE(SOLIDF,SIZE(SOLIDF)); DEALLOCATE(SOLIDF,ISEL_IDF,ICHECK_IDF)
 CALL IDFDEALLOCATEX(MDLIDF)
 CALL CLOSEUNITS()

 IF(IBATCH.EQ.0)THEN
  CALL UTL_MESSAGEHANDLE(1)
  IF(IKRIGING.NE.4.AND.LEX)CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Succesfully written new solid.','Information')
 ENDIF
 
 END SUBROUTINE SOLID_CALC

 !###======================================================================
 LOGICAL FUNCTION SOLID_READIDF()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J
 
 SOLID_READIDF=.FALSE.
 
 NTBSOL=SLD(1)%NLAY*2; ALLOCATE(SOLIDF(NTBSOL))
 DO I=1,NTBSOL; CALL IDFNULLIFY(SOLIDF(I)); ENDDO

 !## try to read all idf's
 NTBSOL=0
 DO I=1,SLD(1)%NLAY; DO J=1,2
  NTBSOL=NTBSOL+1
  !## read entire IDF (top/bot)
  IF(.NOT.IDFREAD(SOLIDF(NTBSOL),SLD(1)%TBNAME(I,J),0))RETURN
 ENDDO; ENDDO

 IF(IBATCH.EQ.0)THEN; CALL UTL_MESSAGEHANDLE(0); CALL WINDOWSELECT(0); ENDIF

 !## no window specified, use minimum overlapping area of set of idf files
 IF(IWINDOW.EQ.0)THEN
  MDLIDF%XMIN=MAXVAL(SOLIDF%XMIN); MDLIDF%XMAX=MINVAL(SOLIDF%XMAX)  
  MDLIDF%YMIN=MAXVAL(SOLIDF%YMIN); MDLIDF%YMAX=MINVAL(SOLIDF%YMAX)  
  MDLIDF%DX=MIN(MINVAL(SOLIDF%DX),MINVAL(SOLIDF%DY)); MDLIDF%DY=MDLIDF%DX
 ENDIF
 CALL UTL_IDFSNAPTOGRID(MDLIDF%XMIN,MDLIDF%XMAX,MDLIDF%YMIN,MDLIDF%YMAX,MDLIDF%DX,MDLIDF%NCOL,MDLIDF%NROW)
 NCOL=MDLIDF%NCOL; NROW=MDLIDF%NROW; NLAY=NTBSOL
 
 SOLID_READIDF=.TRUE.
 
 END FUNCTION SOLID_READIDF
 
 !###======================================================================
 LOGICAL FUNCTION SOLID_CALC_INIT(IVERSION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IVERSION
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I,J,N,ICOL,IROW

 SOLID_CALC_INIT=.FALSE.

 !## load solid-fit dialog
 CALL WDIALOGLOAD(ID_DSOLID_CALC,ID_DSOLID_CALC)
 CALL WDIALOGPUTIMAGE(ID_PROPERTIES_PCG,ID_ICONPROPERTIES,1)
 CALL WDIALOGPUTIMAGE(ID_PROPERTIES_KRIGING,ID_ICONPROPERTIES,1)
 CALL WDIALOGPUTREAL(IDF_REAL1,0.01,'(F10.3)')
 
 IF(WINFOGRID(IDF_GRID1,GRIDROWSMAX).LT.NTBSOL+1)THEN
  CALL WDIALOGUNLOAD()
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not store '//TRIM(ITOS(N))//' rows in grid','Error')
  RETURN
 ENDIF

 IF(ALLOCATED(ISEL_IDF))DEALLOCATE(ISEL_IDF)
 IF(ALLOCATED(ICHECK))DEALLOCATE(ICHECK)
 IF(ALLOCATED(IACT))DEALLOCATE(IACT)
 N=NTBSOL+1; CALL WGRIDROWS(IDF_GRID1,N); ALLOCATE(ISEL_IDF(N),IACT(N),ICHECK(N)) 

 IACT=1; ICHECK=1; ISEL_IDF=1
 DO I=1,NTBSOL; ISEL_IDF(I+1)=I; ENDDO
 CALL WGRIDLABELROW(IDF_GRID1,1,'All SolidLayers')
 CALL WGRIDSTATECELL(IDF_GRID1,3,1,2)
 J=1
 DO I=1,NTBSOL
  IF(MOD(I,2).EQ.0)THEN
   CALL WGRIDLABELROW(IDF_GRID1,I+1,'('//TRIM(ITOS(I))//') Bottom Layer '//TRIM(ITOS(J)))
   CALL WGRIDPUTCELLSTRING(IDF_GRID1,3,I+1,SLD(1)%TBNAME(J,2)(INDEX(SLD(1)%TBNAME(J,2),'\',.TRUE.)+1:))
   IACT(I+1)=SLD(1)%ICLC(J,2)
   ICHECK(I+1)=SLD(1)%ICHECK(J,2)
   J=J+1
  ELSE
   CALL WGRIDLABELROW(IDF_GRID1,I+1,'('//TRIM(ITOS(I))//') Top Layer '//TRIM(ITOS(J)))
   CALL WGRIDPUTCELLSTRING(IDF_GRID1,3,I+1,SLD(1)%TBNAME(J,1)(INDEX(SLD(1)%TBNAME(J,1),'\',.TRUE.)+1:))
   IACT(I+1)=SLD(1)%ICLC(J,1)
   ICHECK(I+1)=SLD(1)%ICHECK(J,1)
  ENDIF
 ENDDO
 CALL WGRIDPUTCHECKBOX(IDF_GRID1,1,IACT,N)
 CALL WGRIDPUTCHECKBOX(IDF_GRID1,2,ICHECK,N)
 CALL WGRIDSTATE(IDF_GRID1,3,2)
 CALL WDIALOGSHOW(-1,-1,0,3)
 
 IF(IKRIGING.EQ.1)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO3)
 IF(IKRIGING.EQ.2)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO4)
 IF(IKRIGING.EQ.3)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO5)
 IF(IKRIGING.EQ.4)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO6)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,I)
 IF(I.EQ.3)I=2
 CALL WDIALOGFIELDSTATE(IDF_CHECK3,ABS(I-2))
 CALL WDIALOGFIELDSTATE(ID_PROPERTIES_PCG,ABS(I-2))
 CALL WDIALOGFIELDSTATE(ID_PROPERTIES_KRIGING,I-1)
 CALL WDIALOGFIELDSTATE(IDF_BUTTON6,I-1)
 
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE(ITYPE)

   CASE(FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_RADIO1,IDF_RADIO2)
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
      CALL WDIALOGFIELDSTATE(IDF_INTEGER1,I-1)
     CASE (IDF_RADIO3,IDF_RADIO4,IDF_RADIO5,IDF_RADIO6)
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,I)
      SELECT CASE (I)
       CASE (1);   I=0; J=1
       CASE (2,3); I=1; J=0
       CASE (4);   I=0; J=0
      END SELECT
      CALL WDIALOGFIELDSTATE(IDF_CHECK3,J)
      CALL WDIALOGFIELDSTATE(ID_PROPERTIES_PCG,J)
      CALL WDIALOGFIELDSTATE(ID_PROPERTIES_KRIGING,I)
      CALL WDIALOGFIELDSTATE(IDF_BUTTON6,I)
      IF(I.EQ.4)THEN
       CALL WDIALOGPUTSTRING(IDOK,'Export ...')
      ELSE
       CALL WDIALOGPUTSTRING(IDOK,'Compute ...')
      ENDIF
     CASE (IDF_GRID1)
      CALL WDIALOGSELECT(ID_DSOLID_FIT)

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
     CASE (ID_PROPERTIES_PCG)
      CALL PCGSETTINGS(MXITER1,MXITER2,HCLOSE,RCLOSE,ITIGHT,MICNVG,RELAX,IDAMPING)
      CALL WDIALOGSELECT(ID_DSOLID_CALC)     
     CASE (ID_PROPERTIES_KRIGING)
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,IKRIGING)
      IOFFSET=1; IF(IKRIGING.EQ.3)IOFFSET=2
      CALL KRIGINGSETTINGS(MINP,MAXP,KTYPE,RANGE,SILL,NUGGET,IOFFSET,SEARCHDISTANCE,NOSEARCH)
      CALL WDIALOGSELECT(ID_DSOLID_CALC)     
     !## start computing or get semivariogram
     CASE (IDOK,IDF_BUTTON6)
      CALL WGRIDGETCHECKBOX(IDF_GRID1,1,IACT,N)
      CALL WGRIDGETCHECKBOX(IDF_GRID1,2,ICHECK,N)
      J=1
      DO I=1,NTBSOL
       IF(MOD(I,2).EQ.0)THEN
        SLD(1)%ICLC(J,2)=IACT(I+1)
        SLD(1)%ICHECK(J,2)=ICHECK(I+1)
        J=J+1
       ELSE
        SLD(1)%ICLC(J,1)=IACT(I+1)
        SLD(1)%ICHECK(J,1)=ICHECK(I+1)
       ENDIF
      ENDDO
      CALL WDIALOGGETCHECKBOX(IDF_CHECK3,IMIDELEV)
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
      IVERSION=0; IF(I.EQ.2)CALL WDIALOGGETINTEGER(IDF_INTEGER1,IVERSION)
      IF(MESSAGE%VALUE1.EQ.IDF_BUTTON6)THEN
       CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to compute Semivariogram(s) for the selected layers ? ','Question')
       IF(WINFODIALOG(4).EQ.1)THEN; SOLID_CALC_INIT=.TRUE.; EXIT; ENDIF
      ELSE
       SOLID_CALC_INIT=.TRUE.; EXIT
      ENDIF
     CASE (IDCANCEL)
      EXIT
     CASE (IDHELP)
       CALL IMODGETHELP('5.4','Solid Tool')
    END SELECT
  END SELECT

 ENDDO

 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,IKRIGING)
 !## overrule since semivariogram will be computed
 IF(MESSAGE%VALUE1.EQ.IDF_BUTTON6)IKRIGING=5

 DEALLOCATE(ISEL_IDF,IACT,ICHECK) 

 CALL WDIALOGSELECT(ID_DSOLID_FIT)
 CALL WDIALOGUNLOAD()

 END FUNCTION SOLID_CALC_INIT
 
 !###======================================================================
 SUBROUTINE SOLID_PCGINT(XD,YD,ZD,ND,IERROR,IDF,IECHO,IDUPLICATE,HNOFLOW)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ND,IECHO !## 1=WINDOWS -1=DOSBOX
 INTEGER,INTENT(IN),OPTIONAL :: IDUPLICATE
 REAL,OPTIONAL,INTENT(IN) :: HNOFLOW !## make areas inactive values in idf%x() equal to hoflow
 REAL,DIMENSION(:),POINTER :: XD,YD,ZD
 INTEGER,INTENT(OUT) :: IERROR
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 LOGICAL :: LEX
 INTEGER :: I,ITER1,ITER2,ILAY,IROW,ICOL,ICNVG,NICNVG,ITYPE
 TYPE(WIN_MESSAGE) :: MESSAGE
 REAL :: Z1,Z2,INIHEAD,C,HCHG,HCHGOLD,S

 IERROR=1
 
 !## pcg can not solve perfect flat area, so idf%x=zd
 Z1=MINVAL(ZD(1:ND)); Z2=MAXVAL(ZD(1:ND))
 IF(Z1.EQ.Z2)THEN; IDF%X(:,:)=Z1; CALL UTL_MESSAGEHANDLE(1); IERROR=0; RETURN; ENDIF
 
 NROW=IDF%NROW; NCOL=IDF%NCOL; NLAY=1
 !## allocate memory
 IF(.NOT.SOLID_CALC_AL())RETURN

 !## adjust starting head to be mean of all fixed heads
 INIHEAD=(Z1+Z2)/2.0
 HOLD=0.0; IB=1
 !## isotroop
 IF(PRESENT(HNOFLOW))THEN
  !HOLD=HNOFLOW
  DO IROW=1,NROW; DO ICOL=1,NCOL
   IF(IDF%X(ICOL,IROW).EQ.HNOFLOW)IB(ICOL,IROW,1)=0
  ENDDO; ENDDO
 ENDIF
 CR=1.0; CC=1.0; RHS=0.0; HCOF=0.0
 !## create boundary/fixed heads
 DO I=1,ND
  ICOL=INT(XD(I)); IROW=INT(YD(I)); HOLD(ICOL,IROW,1)=HOLD(ICOL,IROW,1)+ZD(I); RHS(ICOL,IROW,1)=RHS(ICOL,IROW,1)+1.0; IB(ICOL,IROW,1)=-1
 ENDDO
 !## compute mean/sum
 IF(PRESENT(IDUPLICATE))THEN
  IF(IDUPLICATE.EQ.1)RHS=1.0
 ENDIF
 DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(IB(ICOL,IROW,1).GT.0)THEN
   HOLD(ICOL,IROW,1)=INIHEAD
  ELSEIF(IB(ICOL,IROW,1).LT.0)THEN
   HOLD(ICOL,IROW,1)=HOLD(ICOL,IROW,1)/RHS(ICOL,IROW,1)
  ENDIF
 ENDDO; ENDDO 
 RHS=0.0
 !## change boundary condition
 IF(ITIGHT.EQ.2)THEN
  DO IROW=1,NROW; DO ICOL=1,NCOL
   IF(IB(ICOL,IROW,1).LT.0)IB(ICOL,IROW,1)=3.0
  ENDDO; ENDDO
 ENDIF
   
! IDF%X(:,:)=RHS(:,:,1)
! if(idfwrite(idf,"D:\RHS.idf",0))then; endif
! IDF%X(:,:)=HCOF(:,:,1)
! if(idfwrite(idf,"D:\HCOF.idf",0))then; endif
! IDF%X(:,:)=REAL(ib(:,:,1))
! if(idfwrite(idf,"D:\ib.idf",0))then; endif
 
 !## solve each system per modellayer
 NICNVG=0; HNEW=HOLD
 DO ITER1=1,MXITER1 
  !## change boundary condition
  IF(ITIGHT.EQ.2)THEN
   C=0.1
   DO IROW=1,NROW; DO ICOL=1,NCOL
    IF(IB(ICOL,IROW,1).EQ.3)THEN
     RHS(ICOL,IROW,1) =-C*HOLD(ICOL,IROW,1)
     HCOF(ICOL,IROW,1)=-C
    ENDIF
   ENDDO; ENDDO
  ENDIF
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  CALL PCG2AP(NROW*NCOL,NROW,NCOL,1,IB(:,:,1),CR,CC,CV,HCOF,RHS(:,:,1),V,SS,P, &
              CD,HNEW(:,:,1),MXITER1,MXITER2,ITER1,ITER2,ICNVG,HCLOSE,RCLOSE,IECHO,NICNVG,RELAX,HCHG) !1)
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
 HOLD=REAL(HNEW)
 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,''); CALL WINDOWOUTSTATUSBAR(3,'')

 LEX=.TRUE.
 IF(ICNVG.NE.1)THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Interpolation did not converge.'//CHAR(13)// &
    'Do you want to save the results so far in:'//CHAR(13)// &
     '['//TRIM(FNAME)//']','Question')
  IF(WINFODIALOG(4).EQ.1)LEX=.TRUE.
 ENDIF

 IF(LEX)IDF%X(:,:)=HOLD(:,:,1)

 !## clean for inactive cells
 IF(PRESENT(HNOFLOW))THEN
  DO IROW=1,NROW; DO ICOL=1,NCOL
   IF(IB(ICOL,IROW,1).EQ.0)IDF%X(ICOL,IROW)=HNOFLOW
  ENDDO; ENDDO
 ENDIF
 
 !## free memory for pcg-computation
 CALL SOLID_CALC_DAL()

 CALL UTL_MESSAGEHANDLE(1)
 IERROR=0

 END SUBROUTINE SOLID_PCGINT
 
 !###======================================================================
 SUBROUTINE SOLID_CALC_CONSTRAINS(JLAY,M2) 
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: C=0.1 !10.0 !0.01  !## day resistance
 INTEGER,INTENT(IN) :: JLAY
 REAL,INTENT(IN) :: M2
 INTEGER :: IROW,ICOL,ILAY,IL1,IL2
 REAL :: TOP,BOT,D,DH,DSYS,COND
 
 RHS=0.0; HCOF=0.0; COND=M2/C
 
 !## make sure interfaces do not cross with top- and bottom interfaces
 IF(ICHECK_IDF(JLAY).EQ.1)THEN
 
  !## fill dh with minimal thickness for aquifer

  !## fill top constrains (drain, all above available layers, filled in yet!)
  DO IROW=1,NROW; DO ICOL=1,NCOL
   IF(IB(ICOL,IROW,JLAY).GT.0)THEN

    !## initiate top as basis surfacelevel
    TOP=HOLD(ICOL,IROW,1)
    !## find top aquifer (always uneven number = bot aquitard)
    DO IL1=JLAY-1,1,-1; IF(IB(ICOL,IROW,IL1).LT.0)THEN; TOP=HOLD(ICOL,IROW,IL1); EXIT; ENDIF; ENDDO; IL1=MAX(IL1,1)
    !## initiate bot as basis
    BOT=HOLD(ICOL,IROW,NLAY)
    !## find bot aquifer (always even number = top aquifer)
    DO IL2=JLAY+1,NLAY; IF(IB(ICOL,IROW,IL2).LT.0)THEN; BOT=HOLD(ICOL,IROW,IL2); EXIT; ENDIF; ENDDO; IL2=MIN(IL2,NLAY)
    !## mean thickness available
    DSYS=(TOP-BOT)/NINT(REAL((IL2 -IL1)/2.0))
    !## can not be negative
    DSYS=MAX(0.0,DSYS)
   
    !## add drain if head above level above
    DO ILAY=JLAY-1,1,-1
     DH=0.0; IF(MOD(ILAY,2).EQ.0)DH=MIN(DZ(ILAY/2),DSYS)
     !## skip inactive cells
     IF(IB(ICOL,IROW,ILAY).EQ.0)CYCLE
     IF(HNEW(ICOL,IROW,1).GE.(HOLD(ICOL,IROW,ILAY)-DH))THEN
      !## create drain
      HCOF(ICOL,IROW,1)=HCOF(ICOL,IROW,1)-COND
      RHS(ICOL,IROW,1) =RHS(ICOL,IROW,1) -COND*(HOLD(ICOL,IROW,ILAY)-DH)
     ENDIF
    ENDDO
   
    !## fill bottom constrains (river, all below available layers with constant heads)
    !## excluding the base (nlay)
    DO ILAY=JLAY+1,NLAY-1
     !## skip inactive cells
     IF(IB(ICOL,IROW,ILAY).EQ.0)CYCLE
     !## add river if head below level below and constant head value available (clay)
     IF(IB(ICOL,IROW,ILAY).LT.0)THEN 
      DH=0.0; IF(MOD(ILAY,2).EQ.0)DH=MIN(DZ(ILAY/2),DSYS)
      IF(HNEW(ICOL,IROW,1).LE.(HOLD(ICOL,IROW,ILAY)+DH))THEN
       !## create river
       HCOF(ICOL,IROW,1)=HCOF(ICOL,IROW,1)-COND
       RHS(ICOL,IROW,1) =RHS(ICOL,IROW,1) -COND*(HOLD(ICOL,IROW,ILAY)+DH)
      ENDIF
     ENDIF
    ENDDO

   ENDIF
  ENDDO; ENDDO
 
 ENDIF
 
 !## add estimate (ghb only if ib.eq.1)
 IF(IMIDELEV.EQ.1)THEN
  DO IROW=1,NROW; DO ICOL=1,NCOL
   IF(IB(ICOL,IROW,JLAY).EQ.1)THEN !GT.0)THEN
    !## initiate top as basis surfacelevel
    TOP=HOLD(ICOL,IROW,1)
    !## find top aquifer (always uneven number)
    DO IL1=JLAY-1,1,-1; IF(IB(ICOL,IROW,IL1).LT.0)THEN; TOP=HOLD(ICOL,IROW,IL1); EXIT; ENDIF; ENDDO
    IL1=MAX(IL1,1)
    !## initiate bot as basis
    BOT=HOLD(ICOL,IROW,NLAY)
    !## find bot aquifer (always even number)
    DO IL2=JLAY+1,NLAY; IF(IB(ICOL,IROW,IL2).LT.0)THEN; BOT=HOLD(ICOL,IROW,IL2); EXIT; ENDIF; ENDDO
    IL2=MIN(IL2,NLAY)
    D=(TOP-BOT)/NINT(REAL((IL2 -IL1)/2.0))
    D=D*        NINT(REAL((JLAY-IL1)/2.0))
    !## create ghb as estimate
    HCOF(ICOL,IROW,1)=HCOF(ICOL,IROW,1)-(COND/1000.0)
    RHS(ICOL,IROW,1) =RHS(ICOL,IROW,1) -(COND/1000.0)*(HOLD(ICOL,IROW,IL1)-D)
   ENDIF
  ENDDO; ENDDO
 ENDIF

 !## cross-section in between looser
 IF(ITIGHT.EQ.2)THEN
  DO IROW=1,NROW; DO ICOL=1,NCOL
   IF(IB(ICOL,IROW,JLAY).EQ.3)THEN !GT.0)THEN
    !## create ghb as estimate
    HCOF(ICOL,IROW,1)=HCOF(ICOL,IROW,1)-(COND/1000.0)
    RHS(ICOL,IROW,1) =RHS(ICOL,IROW,1) -(COND/1000.0)*HOLD(ICOL,IROW,JLAY)
   ENDIF
  ENDDO; ENDDO
 ENDIF
 
 END SUBROUTINE SOLID_CALC_CONSTRAINS

 !###======================================================================
 LOGICAL FUNCTION SOLID_CALC_FILL()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ICOL,IROW,ILAY,MX,ISPF,I,J,N,IL,IPOS,NLOC,IR1,IR2,IC1,IC2
 REAL :: DX,DZZ,G,X1,X2,Y1,Y2
 REAL,ALLOCATABLE,DIMENSION(:) :: TL
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IC,IR

 SOLID_CALC_FILL=.FALSE.

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Creating Interpolation Matrix ...')
 IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Creating Interpolation Matrix ...'
 
 IF(IKRIGING.EQ.1)THEN
  HCOF=0.0; RHS=0.0
 ENDIF
 
 IF(SHPNO.EQ.0.AND.NMASK.EQ.0)THEN
  IB=1    !## all active
 !## include boundary, mark them as inactive ...
 ELSE
  !## all constant head --- nothing to do with!
  IB=1

  !## include shapes
  IF(SHPNO.GT.0)THEN
   !## all inactive (constant head)
   IB=-1
   N=MAXVAL(SHPNCRD)
   DO I=1,SHPNO
    X1=MINVAL(SHPXC(1:SHPNCRD(I),I)); X2=MAXVAL(SHPXC(1:SHPNCRD(I),I))
    Y1=MINVAL(SHPYC(1:SHPNCRD(I),I)); Y2=MAXVAL(SHPYC(1:SHPNCRD(I),I))
    !## get ofset from xmin/ymin in the number of cells
    CALL IDFIROWICOL(MDLIDF,IR1,IC1,X1,Y2)
    CALL IDFIROWICOL(MDLIDF,IR2,IC2,X2,Y1)
    IF(IC2.EQ.0)IC2=MDLIDF%NCOL; IF(IR2.EQ.0)IR2=MDLIDF%NROW
    DO IROW=MAX(1,IR1),MIN(IR2,MDLIDF%NROW)
     DO ICOL=MAX(1,IC1),MIN(IC2,MDLIDF%NCOL)
      CALL IDFGETLOC(MDLIDF,IROW,ICOL,X1,Y1)
      !## inside polygon --- thus editable
      IF(UTL_INSIDEPOLYGON(X1,Y1,SHPXC(:,I),SHPYC(:,I),SHPNCRD(I)).EQ.1)IB(ICOL,IROW,1)=1
     ENDDO
    ENDDO
   ENDDO
   !## copy shape to all layers
   DO ILAY=2,NLAY; IB(:,:,ILAY)=IB(:,:,1); ENDDO
  ENDIF

  !## include masks (ibound)
  IF(NMASK.GT.0)THEN
   DO I=1,NMASK
    IF(.NOT.IDFREAD(MASK(I)%IDF,MASK(I)%FNAME,1))RETURN
    IF(.NOT.IDFEQUAL(MASK(I)%IDF,MDLIDF,1))RETURN
    DO IROW=1,MDLIDF%NROW; DO ICOL=1,MDLIDF%NCOL
     !## values in between -1,0,2
     !## -2 = constant head on cross-section - to be used for tight/loose option
     !## -1 = constant precomputed value
     !##  0 = inactive
     !##  1 = active will be computed
     !##  2 = active will be computed - forced to be attached to the upper layer, so acts a -1 to the upper layer
     IB(ICOL,IROW,I)=MAX(MIN(2,INT(MASK(I)%IDF%X(ICOL,IROW))),-1)
    ENDDO; ENDDO
    CALL IDFDEALLOCATEX(MASK(I)%IDF)
   ENDDO
  ENDIF

 ENDIF

 !## process all idf's (starting heads) + crosssections (fixed heads/ghb-heads)
 DO ILAY=1,NLAY
  IF(.NOT.IDFREADSCALE(SOLIDF(ILAY)%FNAME,MDLIDF,2,1,0.0,0))RETURN  
  DO IROW=1,NROW; DO ICOL=1,NCOL
   !## starting heads --- top/bot sequence
   HOLD(ICOL,IROW,ILAY)=MDLIDF%X(ICOL,IROW) 
   !## nodata values can not be combined with ibound<0
   IF(IB(ICOL,IROW,ILAY).LT.0)THEN
    !## mask should organize boundary conditions
    IF(HOLD(ICOL,IROW,ILAY).EQ.SOLIDF(ILAY)%NODATA)IB(ICOL,IROW,ILAY)=1
   ENDIF
   !## apply ibound=0 for nodata values
   IF(IB(ICOL,IROW,ILAY).EQ.0)HOLD(ICOL,IROW,ILAY)=HNOFLOW
  END DO; END DO
 END DO
 CALL IDFDEALLOCATEX(MDLIDF)
 
 IF(IKRIGING.EQ.1)THEN
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Filling in Boundary Conditions ...')
  IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Filling in Boundary Conditions ...'
 
  !## count number of values in cell
  RHS =0.0

  DO ISPF=1,NSPF

   !## get max. length
   DX=0.0
   DO I=2,SPF(ISPF)%NXY
    DX=DX+SQRT((SPF(ISPF)%X(I)-SPF(ISPF)%X(I-1))**2.0+(SPF(ISPF)%Y(I)-SPF(ISPF)%Y(I-1))**2.0)
   ENDDO
   !## use minimal cellsize to estimate number of points to be used (times two)!
   MX=MAX(100,2*INT(DX/MDLIDF%DX))
   ALLOCATE(IC(MX),IR(MX),TL(0:MX))

   !## intersect/interpolate all segments of cross-section
   NLOC=0
   TL(0)=0.0
   DO I=2,SPF(ISPF)%NXY

    X1=SPF(ISPF)%X(I-1); X2=SPF(ISPF)%X(I)
    Y1=SPF(ISPF)%Y(I-1); Y2=SPF(ISPF)%Y(I)

    IF(MDLIDF%IEQ.EQ.0)THEN
     !## intersect line with rectangular-regular-equidistantial-grid
     N=0; CALL INTERSECT_EQUI(MDLIDF%XMIN,MDLIDF%XMAX,MDLIDF%YMIN,MDLIDF%YMAX,MDLIDF%DX,X1,X2,Y1,Y2,N)
    ELSE
     !## intersect line with rectangular-irregular-non-equidistantial-grid
     N=0; CALL INTERSECT_NONEQUI(MDLIDF%SX,MDLIDF%SY,MDLIDF%NROW,MDLIDF%NCOL,X1,X2,Y1,Y2,N)
    ENDIF

    DO J=1,N
     !## skip length too short
     IF(LN(J).GT.MDLIDF%DX*0.5)THEN
      !## count number of locations
      NLOC=NLOC+1; IC(NLOC)=INT(XA(J)); IR(NLOC)=INT(YA(J))
      IF(J.EQ.1)TL(NLOC)=TL(NLOC-1)+0.5*LN(J)
      IF(J.GT.1)TL(NLOC)=TL(NLOC-1)+0.5*LN(J-1)+0.5*LN(J)
     ENDIF
    ENDDO
      
   ENDDO

   !## number of layers to interpolate
   DO IL=1,NTBSOL 
    IF(ISEL_IDF(IL).EQ.0)CYCLE
    IF(SPF(ISPF)%PROF(IL)%NPOS.LE.0)CYCLE
    !## process each pos()
    PX=>SPF(ISPF)%PROF(IL)%PX; PZ=>SPF(ISPF)%PROF(IL)%PZ
    DO IPOS=2,SPF(ISPF)%PROF(IL)%NPOS
     DX=PX(IPOS)-PX(IPOS-1); DZZ=PZ(IPOS)-PZ(IPOS-1)
     !## gradient (+) up (-) down
     G =DZZ/DX
     !## scan distance-table
     DO I=1,NLOC
      !## within current segment and within model domain
      IF(TL(I).GE.PX(IPOS-1).AND. &
         TL(I).LE.PX(IPOS).AND. &
         IC(I).GE.1.AND.IC(I).LE.MDLIDF%NCOL.AND. &
         IR(I).GE.1.AND.IR(I).LE.MDLIDF%NROW)THEN

       !## minus two is cross-section
       IF(IB(IC(I),IR(I),IL).NE.0.AND. &
          IB(IC(I),IR(I),IL).NE.-1)IB(IC(I),IR(I),IL)=-2
       RHS(IC(I),IR(I),IL) = RHS(IC(I),IR(I),IL)+1.0

       IF(RHS(IC(I),IR(I),IL).EQ.1)THEN
        !## overwrite HOLD with new value
        HOLD(IC(I),IR(I),IL)=PZ(IPOS-1)+(TL(I)-PX(IPOS-1))*G
       ELSE
        !## add new value to HOLD
        HOLD(IC(I),IR(I),IL)=HOLD(IC(I),IR(I),IL) + (PZ(IPOS-1)+(TL(I)-PX(IPOS-1))*G)
       ENDIF

      ENDIF
     END DO
    ENDDO
    NULLIFY(PX,PZ)
   END DO
   CALL INTERSECT_DEALLOCATE()
   DEALLOCATE(TL,IC,IR)
  END DO

  !## get mean values in case more values in one single cell
  DO ILAY=1,NLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
   !## skip inactive cells
   IF(IB(ICOL,IROW,ILAY).EQ.0)CYCLE
   IF(RHS(ICOL,IROW,ILAY).GT.1.0)THEN
    HOLD(ICOL,IROW,ILAY)=HOLD(ICOL,IROW,ILAY)/RHS(ICOL,IROW,ILAY)
   ENDIF
  END DO; END DO; END DO
 
 ENDIF
 
 SOLID_CALC_FILL=.TRUE.

 END FUNCTION SOLID_CALC_FILL

 !###====================================================================
 LOGICAL FUNCTION SOLID_CALC_AL()
 !###====================================================================
 IMPLICIT NONE
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IOS

 SOLID_CALC_AL=.FALSE.

 CALL WINDOWOUTSTATUSBAR(4,'Allocating memory ...')

 IF(ALLOCATED(IOS))DEALLOCATE(IOS); ALLOCATE(IOS(12)) 

 CALL SOLID_CALC_DAL(); IOS=0
 
 IF(IKRIGING.EQ.1)THEN
  ALLOCATE(P(NCOL,NROW,1)      ,STAT=IOS(1))
  ALLOCATE(V(NCOL,NROW,1)      ,STAT=IOS(2))
  ALLOCATE(SS(NCOL,NROW,1)     ,STAT=IOS(3))
  ALLOCATE(CD(NCOL,NROW,1)     ,STAT=IOS(4))
  ALLOCATE(RHS(NCOL,NROW,NLAY) ,STAT=IOS(5))
  ALLOCATE(CC(NCOL,NROW,1)     ,STAT=IOS(7))
  ALLOCATE(CR(NCOL,NROW,1)     ,STAT=IOS(8))
  ALLOCATE(HNEW(NCOL,NROW,1)   ,STAT=IOS(9))
  ALLOCATE(HCOF(NCOL,NROW,1)   ,STAT=IOS(10))
  ALLOCATE(CV(1,1,1)           ,STAT=IOS(11))
 ENDIF
 ALLOCATE(IB(NCOL,NROW,NLAY)   ,STAT=IOS(6))
 ALLOCATE(HOLD(NCOL,NROW,NLAY) ,STAT=IOS(12))

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
 SUBROUTINE SOLID_CALC_DAL()
 !###====================================================================
 IMPLICIT NONE

 IF(ALLOCATED(P))   DEALLOCATE(P)
 IF(ALLOCATED(V))   DEALLOCATE(V)
 IF(ALLOCATED(SS))  DEALLOCATE(SS)
 IF(ALLOCATED(CD))  DEALLOCATE(CD)
 IF(ALLOCATED(RHS)) DEALLOCATE(RHS)
 IF(ALLOCATED(IB))  DEALLOCATE(IB)
 IF(ALLOCATED(CC))  DEALLOCATE(CC)
 IF(ALLOCATED(CR))  DEALLOCATE(CR)
 IF(ALLOCATED(CV))  DEALLOCATE(CV)
 IF(ALLOCATED(HNEW))DEALLOCATE(HNEW)
 IF(ALLOCATED(HCOF))DEALLOCATE(HCOF)
 IF(ALLOCATED(HOLD))DEALLOCATE(HOLD)

 END SUBROUTINE SOLID_CALC_DAL

 !###====================================================================
 SUBROUTINE SOLIDEXPORT(FNAME,ILAY)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(IN) :: ILAY
 INTEGER :: I,IROW,ICOL,N,IPOS
 INTEGER,DIMENSION(:),ALLOCATABLE :: IU
 REAL :: XP,YP

 !## write locations to a seperate file
 IF(ILAY.EQ.0)THEN
  ALLOCATE(IU(NTBSOL))
  DO I=1,NTBSOL 
   IU(I)=UTL_GETUNIT()
   CALL OSD_OPEN(IU(I),FILE=TRIM(FNAME)//'\EXPORT_INTERFACE_'//TRIM(ITOS(I))//'.IPF',STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE')
   WRITE(IU(I),'(A)') 'X,Y,Z,CROSSSECTION,INTERFACE,POSITION'
  ENDDO

 !## store into memory
 ELSE
  !## get number of layers to interpolate
  N=0; DO ISPF=1,NSPF; DO I=1,NTBSOL
   IF(I.NE.ILAY)CYCLE
   N=N+SPF(ISPF)%PROF(I)%NPOS
  ENDDO; ENDDO
  ALLOCATE(XD(N),YD(N),ZD(N))
 ENDIF
 
 !## write points on interface
 N=0; DO ISPF=1,NSPF
  !## number of layers to interpolate
  DO I=1,NTBSOL 
   IF(SPF(ISPF)%PROF(I)%NPOS.LE.0)CYCLE
   !## process each pos() 
   DO IPOS=1,SPF(ISPF)%PROF(I)%NPOS
    CALL SOLID_GETLOCATION(SPF(ISPF)%PROF(I)%PX(IPOS),XP,YP,SPF(ISPF)%X,SPF(ISPF)%Y)  
    IF(ILAY.EQ.0)THEN
     WRITE(IU(I),'(3(F15.7,A1),3(I5,A1))') XP,',',YP,',',SPF(ISPF)%PROF(I)%PZ(IPOS),',',ISPF,',',I,',',IPOS
    ELSE
     IF(ILAY.EQ.I)THEN
      N=N+1; XD(N)=XP; YD(N)=YP; ZD(N)=SPF(ISPF)%PROF(I)%PZ(IPOS)
     ENDIF
    ENDIF
   END DO
  END DO

 ENDDO

 IF(ILAY.EQ.0)THEN
  DO I=1,SIZE(IU); CLOSE(IU(I)); ENDDO; DEALLOCATE(IU)  
 ENDIF
 
 END SUBROUTINE SOLIDEXPORT

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
 SUBROUTINE SOLID_CHECKIBND(ILAY)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ILAY
 INTEGER(KIND=1),POINTER,DIMENSION(:) :: ISPEC
 INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:) :: Y
 INTEGER(KIND=2),POINTER,DIMENSION(:,:) :: THREAD,YSEL
 INTEGER :: IROW,ICOL,K,IP,IR,IC,N,I,MAXTHREAD,MAXN
 LOGICAL :: LCNST

 DO IROW=1,NROW; DO ICOL=1,NCOL 

  IF(IB(ICOL,IROW,ILAY).EQ.0)HOLD(ICOL,IROW,ILAY)=HNOFLOW
  !## adjust constant head whenever no active cell is attached to it
  IF(IB(ICOL,IROW,ILAY).LT.0)THEN
   K=0
   IF(ICOL.GT.1)THEN;    IF(IB(ICOL-1,IROW,ILAY).NE.0)K=1; ENDIF
   IF(ICOL.LT.NCOL)THEN; IF(IB(ICOL+1,IROW,ILAY).NE.0)K=1; ENDIF
   IF(IROW.GT.1)THEN;    IF(IB(ICOL,IROW-1,ILAY).NE.0)K=1; ENDIF
   IF(IROW.LT.NROW)THEN; IF(IB(ICOL,IROW+1,ILAY).NE.0)K=1; ENDIF
   IF(K.EQ.0)THEN; IB(ICOL,IROW,ILAY)=0; HOLD(ICOL,IROW,ILAY)=HNOFLOW; ENDIF
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
  IF(Y(ICOL,IROW).EQ.0.AND.IB(ICOL,IROW,ILAY).GT.0)THEN
   IP=1
   CALL SOLID_TRACE(IROW,ICOL,IB(:,:,ILAY),CC,CR,Y,THREAD,ISPEC,YSEL,N,NROW,NCOL,IP,LCNST,MAXTHREAD,MAXN)
   !## no constant head attached, remove group of cells
   IF(.NOT.LCNST)THEN
    DO I=1,N
     IC=YSEL(1,I); IR=YSEL(2,I)
     IB(IC,IR,ILAY)=0; HOLD(IC,IR,ILAY)=HNOFLOW
    ENDDO
    WRITE(*,*) 'Removed ',N,' of modelcells that are not attached to a constant head'
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
! REAL,INTENT(IN),DIMENSION(NCOL,NROWAY-1) :: CV
 LOGICAL,INTENT(OUT) :: LCNST
 INTEGER,INTENT(IN) :: NCOL,NROW,IROW,ICOL
 INTEGER,INTENT(INOUT) :: IP,MAXTHREAD,MAXN
 INTEGER,INTENT(OUT) :: N
 INTEGER :: NTHREAD,IR,IC,IDIR,JROW,JCOL,N1,N2,N3,M1,M2
 INTEGER,INTENT(IN) :: IB(NCOL,NROW)
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